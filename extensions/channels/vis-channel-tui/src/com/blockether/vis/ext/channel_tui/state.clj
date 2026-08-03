(ns com.blockether.vis.ext.channel-tui.state
  "Re-frame-like state management for the TUI.
   Single app-db atom, pure event handlers, side effects via reg-fx."
  (:require [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.internal.theme :as shared-theme]
            [com.blockether.vis.internal.header :as vh]
            [com.blockether.vis.ext.channel-tui.chat :as chat]
            [com.blockether.vis.ext.channel-tui.command-suggest :as slash]
            [com.blockether.vis.ext.channel-tui.theme :as tui-theme]
            [com.blockether.vis.ext.channel-tui.input :as input]
            [com.blockether.vis.ext.channel-tui.render :as render]
            [com.blockether.vis.ext.channel-tui.virtual :as virtual]
            [com.blockether.vis.ext.channel-tui.scroll :as scroll]
            [com.blockether.vis.internal.workspace :as workspace]
            [taoensso.telemere :as tel])
  (:import [java.util.concurrent ExecutorService Executors ScheduledExecutorService ThreadFactory
            TimeUnit]))

(set! *unchecked-math* :warn-on-boxed)

;;; ── Framework ──────────────────────────────────────────────────────────────
(defonce app-db (atom nil))

(defonce ^:private event-registry (atom {}))

(defonce ^:private fx-registry (atom {}))

;;; Render thread coordination.
;;
;; The TUI runs a dedicated render thread (see `screen/start-render-thread!`).
;; It sleeps on `render-monitor.wait` and wakes whenever a state-mutating
;; event is dispatched. `:render-version` on app-db is the dirty counter:
;; render thread compares it against the version of the last frame it drew
;; and skips work entirely if nothing changed. That's why the input thread
;; can poll at 16ms without the box-drawing CPU melting.
;;
;; Some events are pure side-projections back from the render thread
;; (`:set-layout`) and must NOT bump the version, otherwise we'd
;; livelock: render writes layout -> version bumps -> render wakes -> same
;; layout -> same version bump -> ...
(defonce ^Object render-monitor
  ^{:doc
    "Monitor object the render thread .waits on. Notify-all on every
          dispatch that changes display state."}
  (Object.))

(def ^:private no-render-bump-events
  "Events that update app-db without requesting a redraw. Right now this
   is just `:set-layout`, which is the render thread itself pushing back
   computed sizes for the input thread to read."
  #{:set-layout})

(def ^:private always-bump-events
  "Events that MUST wake the painter even though they change nothing in the
   active view's app-db slice. `:bump-render-version` is the universal escape
   hatch — notifications, prewarm completion, the cursor blink, mouse hover,
   and the F2 reopen seed all ride it to force exactly one repaint — so it has
   to bypass the `active-view-changed?` gate below."
  #{:bump-render-version})

(defn- active-view-slice
  "The portion of app-db the ACTIVE view paints. Excludes background tab state
   (`:tab-locals`), the dirty counter (`:render-version`), and the render
   thread's published layout (`:layout`). A mutation that leaves this slice
   untouched — e.g. a turn streaming into an UNFOCUSED tab, which only rewrites
   `:tab-locals[<bg-id>]` — is invisible to the user, so it must not wake the
   painter. Without this gate a background turn forces a full active-tab repaint
   per streamed token and starves the focused tab (it can't even echo typing)."
  [db]
  (dissoc db :tab-locals :render-version :layout))

(defn- active-view-changed?
  [old-db new-db]
  (not= (active-view-slice old-db) (active-view-slice new-db)))

(defn- notify-render! [] (locking render-monitor (.notifyAll render-monitor)))

(defn reg-event-db
  "Register a pure event handler: (fn [db event-vec] new-db)"
  [id handler-fn]
  (swap! event-registry assoc id {:type :db :fn handler-fn}))

(defn reg-event-fx
  "Register an effect-producing event handler:
   (fn [db event-vec] {:db new-db :fx [[:effect-id args...]]})"
  [id handler-fn]
  (swap! event-registry assoc id {:type :fx :fn handler-fn}))

(defn reg-fx
  "Register a side-effect handler: (fn [args] ...)"
  [id handler-fn]
  (swap! fx-registry assoc id handler-fn))

(defn- bump-version [db] (update db :render-version (fnil inc 0)))

(def ^:private tab-state-keys
  "EVERY db key a tab owns privately.

   This list is the ONE definition of \"per-tab\", read by both directions of a tab
   switch: `sync-active-tab`/`tab-snapshot` park these into `[:tab-locals id]`, and
   `restore-tab` merges the target tab's snapshot back over the db root. A key that
   is written per tab but MISSING here is broken twice over — the snapshot drops it,
   and because `restore-tab` only MERGES, the value left at the root by the tab you
   just LEFT silently becomes the incoming tab's value.

   That is why the whole turn identity (`active-turn-state-keys`: the gateway turn
   id, the correlation id we minted, and the cancel stamp) has to live here. Without
   it, tab A's live turn id leaked onto tab B, so B's `turn.queued` broadcasts hit
   `live-turn-mirror?` against A's turn - queued rows appeared in, and drained from,
   the wrong session - and A's `:cancelling-at-ms` was lost, so `cancel-self-heal-due?`
   could never time the stuck cancel out and `:cancelling?` wedged input forever."
  [:session :workspace :workspace/root :title :messages :utilization :scroll :layout :input
   :input-history :input-history-index :input-history-draft :slash-command-index
   :slash-command-hidden? :submitted-input :pending-sends :submissions-in-flight :queue-paused
   :pastes :paste-counter :loading? :cancel-token :cancelling? :cancelling-at-ms
   :cancel-awaiting-client-id :gateway-turn-id :live-turn-client-id :progress :turn-start-ms
   :detail-expansions :mouse-selection :session-model-pref])

(defn- empty-tab-state
  []
  {:session nil
   :workspace nil
   :workspace/root nil
   :title nil
   :messages []
   :scroll scroll/follow
   :layout nil
   :input (input/empty-input)
   :input-history []
   :input-history-index nil
   :input-history-draft nil
   :slash-command-index 0
   :slash-command-hidden? false
   :submitted-input nil
   :utilization nil
   :pending-sends []
   :submissions-in-flight []
   :queue-paused nil
   :pastes {}
   :paste-counter 0
   :loading? false
   :cancel-token nil
   :cancelling? false
   :cancelling-at-ms nil
   :cancel-awaiting-client-id nil
   :gateway-turn-id nil
   :live-turn-client-id nil
   :progress nil
   :turn-start-ms nil
   :detail-expansions {}
   :mouse-selection nil
   :session-model-pref nil})

(defn- tab-snapshot [db] (merge (empty-tab-state) (select-keys db tab-state-keys)))

(defn- current-tab-id
  [db]
  (or (:active-tab-id db) (:id (some #(when (:active? %) %) (:tabs db))) (:id (first (:tabs db)))))

(defn- active-tab-entry
  [db]
  (let [active-id (current-tab-id db)]
    (some #(when (= (:id %) active-id) %) (:tabs db))))

(defn- active-workspace
  [db]
  (or (:workspace db)
      (some-> (active-tab-entry db)
              :workspace)
      (when-let
        [root (or (:workspace/root db)
                  (some-> (active-tab-entry db)
                          :workspace/root))]
        {:workspace/root root})))

(defn- sync-active-tab
  [db]
  (if-let [id (current-tab-id db)]
    (assoc-in db [:tab-locals id] (tab-snapshot db))
    db))

(defn tab-session-snapshot
  "Ordered open-tab sessions + the active one, for per-place persistence.
   Returns {:active <session-id-str|nil>
            :sessions [{:id <sid> :root <project-root, absent when unknown>} …]}
   in left-to-right tab order. Each entry carries the tab's PROJECT root
   (`vh/tab-group-root`) so a restore — or any future project-level UI — can
   group tabs without resuming every session first. The active tab's session
   lives at the db root; every other tab's lives in `:tab-locals`."
  [db]
  (let
    [entries
     (vec (:tabs db))

     active-id
     (current-tab-id db)

     sid
     (fn [tab-id]
       (if (= tab-id active-id)
         (some-> db
                 :session
                 :id
                 str)
         (some-> (get-in db [:tab-locals tab-id :session :id])
                 str)))]

    {:active (sid active-id)
     :sessions (vec (keep (fn [entry]
                            (when-let
                              [s (or (sid (:id entry))
                                     ;; PENDING pre-allocated tab: no locals-bound
                                     ;; session yet — the id rides on the entry.
                                     (some-> (:session-id entry)
                                             str))]
                              (if-let [root (vh/tab-group-root entry)]
                                {:id s :root root}
                                {:id s})))
                          entries))}))

(defn- finalize-db
  [db]
  (cond-> db
    (map? db)
    sync-active-tab))

(defn dispatch
  "Dispatch an event vector, e.g. (dispatch [:send-message \"hello\"]).
   Wakes the render thread when the event changes the ACTIVE view. An event
   bumps `:render-version` and notifies the painter unless it is in
   `no-render-bump-events`, AND either it is in `always-bump-events` or it
   actually mutated the active-view slice (see `active-view-slice`). A turn
   streaming into a background tab only rewrites `:tab-locals`, so it no longer
   wakes the painter — the header tab spinner still animates on the render
   loop's own wall-clock tick."
  [[id :as event-vec]]
  (if-let [{:keys [type] :as handler} (get @event-registry id)]
    (let
      [allow-bump? (not (no-render-bump-events id))
       force? (contains? always-bump-events id)
       bumped? (volatile! false)
       decide-bump (fn [old-db new-db]
                     (and allow-bump? (or force? (active-view-changed? old-db new-db))))]

      (case type
        :db
        (swap! app-db (fn [db]
                        (let
                          [db' (finalize-db ((:fn handler) db event-vec))
                           eff? (decide-bump db db')]

                          (vreset! bumped? eff?)
                          (if eff? (bump-version db') db'))))

        :fx
        (let [effects (volatile! nil)]
          ;; An FX handler still performs a state transition. Use CAS just like
          ;; :db handlers: an async gateway ACK and UI keystrokes can dispatch on
          ;; different threads, and read+reset would let the ACK overwrite every
          ;; edit committed after its stale read. `swap!` retries the pure handler
          ;; against the latest db; only the winning attempt's effects run.
          (swap! app-db (fn [old-db]
                          (let [{:keys [db fx]} ((:fn handler) old-db event-vec)]
                            (vreset! effects fx)
                            (if db
                              (let
                                [db' (finalize-db db)
                                 eff? (decide-bump old-db db')]

                                (vreset! bumped? eff?)
                                (if eff? (bump-version db') db'))
                              (do
                                ;; Pure-effect handler (no :db): preserve the prior
                                ;; contract and leave the atom's value untouched.
                                (vreset! bumped? allow-bump?)
                                old-db)))))
          (doseq [[fx-id & args] @effects]
            (when-let [fx-fn (get @fx-registry fx-id)]
              (apply fx-fn args)))))
      (when @bumped? (notify-render!)))
    (throw (ex-info (str "No handler registered for event: " id) {:event event-vec}))))

;;; ── State shape ────────────────────────────────────────────────────────────
;;
;; {:config     nil              ;; provider config map or nil
;;  :session nil            ;; {:id session-id} or nil - handle to the shared sessions cache
;;  :messages   []               ;; [{:role :user|:assistant :text str :timestamp #inst}]
;;  :scroll {:mode :follow}      ;; scroll variant (see scroll.clj): :follow|:at
;;  :input      {:lines [""] :crow 0 :ccol 0}
;;  :input-history []            ;; persisted user queries for this session
;;  :input-history-index nil     ;; nil = editing live draft, 0 = newest history entry
;;  :input-history-draft nil     ;; unsent draft preserved while browsing history
;;  :submitted-input nil         ;; prompt/paste snapshot for restoring cancelled turns
;;  :loading?   false            ;; true while RLM is working
;;  :cancel-token nil            ;; channels.cancellation token for the
;;                               ;; in-flight turn (nil when idle). Holds
;;                               ;; the cooperative flag + the worker
;;                               ;; future so :cancel-turn can hit both.
;;  :cancelling? false           ;; true once Esc was pressed; cleared when the
;;                               ;; local worker reports :message-received OR
;;                               ;; when the gateway reports that the attached
;;                               ;; turn is already terminal/missing (for
;;                               ;; orphan sweeps and restart repair paths).
;;  :progress   nil              ;; live per-iteration timeline while loading:
;;                               ;;   {:iterations [{:iteration int
;;                               ;;                  :thinking  str-or-nil
;;                               ;;                  :code      [str]       ;; latest streamed forms
;;                               ;;                  :final?    bool}]}
;;  :settings  {:show-thinking true :show-iterations true}
;;  :channel-status {}           ;; extension/channel status banners keyed by id
;;  :dialog-open? false}         ;; dialog singleton guard
;;
(def ^:private settings-notification-ttl-ms 1500)

(def ^:private cancel-notification-ttl-ms 2500)

(def ^:private cancel-self-heal-timeout-ms
  "Client-side safety net for a STUCK cancel. `:cancel-turn` flips `:cancelling?`
   and waits for the daemon's terminal `turn.completed` (status cancelled) event
   to release it — that event is the ONLY release. If it never lands (an SSE
   reconnect gap right at cancel, or the daemon dying mid-unwind) the flag sticks
   true, every send parks purely local (see the enqueue race guard), and input is
   wedged until the daemon's ~6-minute stall watchdog fires — a freeze, to a
   human. This bounds that window: once `:cancelling?` has been held this long
   with no terminal event, the client self-heals. Long enough that a healthy
   terminal event always wins the race; short enough that a dropped one never
   freezes the user."
  8000)

(def ^:private turn-liveness-grace-ms
  "How long a turn must have been in flight before the liveness watchdog is armed.
   Long enough that the normal terminal event (mux SSE or the blocking worker)
   always wins on a healthy run; short enough that a DROPPED one is a blip."
  10000)

(def ^:private turn-liveness-probe-interval-ms
  "Minimum gap between liveness probes for one tab. The render loop pokes the
   watchdog every `spinner-tick-ms`; this throttles it down to one cheap
   `gateway-list-turns` round-trip per tab per interval, on the queue lane."
  5000)

(def ^:private live-progress-render-interval-ms
  "Maximum wall-clock interval between live reasoning redraws.

   Keep the live TUI heartbeat at the same cadence as the render loop: progress
   chunks coalesce to one app-db update per frame instead of per token, while
   lifecycle chunks still flush immediately so code/result/final boundaries
   appear without delay. Virtual layout then projects and paints only visible
   bubbles."
  80)

(def ^:private pending-assistant-content
  [{"id" "pending"
    "type" "notice"
    "code" "turn_pending"
    "message" "Sending request to provider..."}])

(def ^:private pending-shell-content
  "Pending-assistant placeholder for a `!`/`!&` shell-sugar turn. It runs a
   shell command LOCALLY with no provider round-trip, so it must never claim
   \"Sending request to provider…\". The engine flips it to `Vis is running:
   <cmd>` the instant it emits the shell-phase chunk."
  [{"id" "pending" "type" "notice" "code" "turn_pending" "message" "Running shell command..."}])

(def ^:private pending-slash-content
  "Pending-assistant placeholder for a registered slash command (`/draft …`,
   `/voice`, …). Slash dispatch runs LOCALLY with no provider round-trip, so the
   bubble must never claim \"Sending request to provider…\". The engine flips it to
   `Vis is running: /<name>` the instant it emits the slash-phase chunk."
  [{"id" "pending" "type" "notice" "code" "turn_pending" "message" "Running command..."}])

(defn- shell-bang-command?
  "True when TEXT is a `!`/`!&` shell-sugar command carrying a NON-blank body —
   the same rule the engine's `parse-bang` applies. A bare `!`/`!&` is ordinary
   prose (a normal LLM turn), so it is NOT a command."
  [text]
  (boolean (when (string? text)
             (let [t (str/triml text)]
               (cond (str/starts-with? t "!&") (seq (str/trim (subs t 2)))
                     (str/starts-with? t "!") (seq (str/trim (subs t 1)))
                     :else false)))))

(defn- slash-command?
  "True when TEXT is a submission for a REGISTERED slash command — it runs
   locally with NO provider call, so its settled bubble must not carry a
   model/provider footer. Mirrors the engine's `slash-text?` guard (a leading
   `/word` token with NO interior `/`, so a pasted `/var/…/shot.png` path is NOT
   a slash) AND additionally requires the root token to resolve to a registered
   slash root — an unknown `/foo` falls through to a prompt-template expansion or
   a normal LLM turn, which legitimately keeps its footer."
  [text]
  (boolean (when (string? text)
             (let [t (str/triml text)]
               (when (re-find #"^/[^\s/]+(?:\s|$)" t)
                 (let
                   [root (-> (subs t 1)
                             (str/split #"\s+")
                             first)
                    roots (into #{}
                                (comp (filter #(empty? (:slash/parent %)))
                                      (keep #(some-> (:slash/name %)
                                                     name)))
                                (vis/registered-slashes))]

                   (contains? roots root)))))))

(defn- pending-assistant-for
  "Pending-assistant slot for a submission. A shell-sugar (`!`/`!&`) turn or a
   registered slash command (`/draft …`) runs LOCALLY with no provider round-trip,
   so it gets a command-flavored placeholder and the `:slash?` command marker —
   which suppresses the model/provider footer, exactly like a resumed command turn
   (`:tag :user-shell` / `:user-slash`)."
  [text]
  (cond (shell-bang-command? text) (assoc (chat/assistant-message pending-shell-content)
                                     :pending? true
                                     :slash? true
                                     :command-phase-label "Running shell command")
        (slash-command? text) (assoc (chat/assistant-message pending-slash-content)
                                :pending? true
                                :slash? true
                                :command-phase-label "Running command")
        :else (assoc (chat/assistant-message pending-assistant-content) :pending? true)))

(defn- pending-assistant-message? [m] (and (= :assistant (:role m)) (true? (:pending? m))))

(defn- pending-assistant-index
  "Locate the optimistic assistant slot for one submit generation. Identified
   completions never fall through to a sibling generation; legacy completions
   without a client id retain the oldest-pending fallback."
  [messages client-turn-id]
  (let [messages (vec (or messages []))]
    (if client-turn-id
      (first (keep-indexed (fn [idx m]
                             (when (and (pending-assistant-message? m)
                                        (= (str client-turn-id)
                                           (some-> (:client-turn-id m)
                                                   str)))
                               idx))
                           messages))
      (first (keep-indexed (fn [idx m]
                             (when (pending-assistant-message? m) idx))
                           messages)))))

(defn- completion-response
  "Build the canonical assistant bubble shared by live, delayed and terminal
   completion paths. `trace` belongs to the completed generation, never to a
   newer turn which may already be active in the same tab."
  [answer trace wall-ms
   {:keys [model provider llm-selected llm-actual llm-fallback? llm-routing-trace iteration-count
           duration-ms tokens cost confidence session-turn-id status client-turn-id slash]}]
  (->
    (chat/assistant-message (vec (or answer [])))
    (cond->
      session-turn-id
      (assoc :session-turn-id session-turn-id)

      (seq trace)
      (assoc :traces trace)

      (or duration-ms wall-ms)
      (assoc :duration-ms (or duration-ms wall-ms))

      model
      (assoc :model model)

      provider
      (assoc :provider provider)

      llm-selected
      (assoc :llm-selected llm-selected)

      llm-actual
      (assoc :llm-actual llm-actual)

      (some? llm-fallback?)
      (assoc :llm-fallback? llm-fallback?)

      (seq llm-routing-trace)
      (assoc :llm-routing-trace llm-routing-trace)

      iteration-count
      (assoc :iteration-count iteration-count)

      tokens
      (assoc :tokens tokens)

      cost
      (assoc :cost cost)

      confidence
      (assoc :confidence confidence)

      status
      (assoc :status status)

      client-turn-id
      (assoc :client-turn-id client-turn-id)

      slash
      (assoc :slash? true))))

(defn- replace-pending-assistant
  "Replace only the pending assistant slot owned by this completion. An
   identified completion with no matching placeholder is a duplicate/stale
   callback and leaves the transcript untouched."
  [messages response]
  (let
    [messages
     (vec (or messages []))

     client-turn-id
     (:client-turn-id response)

     response
     (dissoc response :pending?)

     idx
     (pending-assistant-index messages client-turn-id)

     carry-slash
     (fn [old resp]
       (cond-> resp
         (and (not (contains? resp :slash?)) (:slash? old))
         (assoc :slash? true)))]

    (cond idx (assoc messages idx (carry-slash (get messages idx) response))
          client-turn-id messages
          (and (seq messages) (= :assistant (:role (peek messages))))
          (conj (pop messages) (carry-slash (peek messages) response))
          :else (conj messages response))))

(def ^:private throttled-streaming-phases
  "Per-token streaming phases that share the live-progress redraw budget.
   `:reasoning` chunks fire on every reasoning SSE delta; `:content`
   chunks fire on every answer-markdown SSE delta. Lifecycle phases
   (`:iteration-final`, `:form-start`, `:form-result`,
   `:provider-fallback`, …) bypass the throttle entirely so block
   boundaries appear without delay."
  #{:reasoning :content})

(defonce ^:private ^ScheduledExecutorService progress-trailing-flush-scheduler
  (Executors/newSingleThreadScheduledExecutor
    (reify
      java.util.concurrent.ThreadFactory
        (newThread [_ r]
          (doto (Thread. ^Runnable r "vis-tui-progress-trailing-flush") (.setDaemon true))))))

(def ^:private reasoning-live-tail-reveal-chars
  "Escape hatch for the live reasoning sentence-buffer. A boundary-less run
   this long past the last sentence break is revealed anyway, so a long
   thought with no punctuation still streams instead of hiding forever."
  200)

(def ^:private reasoning-sentence-boundary-chars
  "Chars that END a sentence/clause for the live reasoning buffer. We hold a
   trailing partial back until it reaches one of these, so streamed thinking
   paints WHOLE sentences instead of a 1-2 char leading stub."
  #{\. \! \? \; \newline \。 \！ \？})

(def ^:private reasoning-boundary-trailing-chars
  "Closing punctuation kept WITH a boundary char (so `.\"` / `?)` render whole)."
  #{\" \' \) \] \} \» \’ \”})

(defn- clip-reasoning-to-sentence
  "Truncate live-streaming reasoning `s` to just after its LAST sentence/clause
   boundary, holding any trailing partial sentence back until it completes —
   this is what turns the provider's `\"I\"` → 0.5s gap → full-sentence burst
   into a clean whole-sentence reveal. Growth is monotonic (source only grows,
   the boundary index only advances), so the bubble never shrinks. If the tail
   past the last boundary exceeds `max-chars`, reveal the whole string (escape
   hatch for a long boundary-less thought)."
  [s max-chars]
  (let
    [s
     (str s)

     n
     (count s)]

    (if (zero? n)
      s
      (let
        [boundary-idx
         (loop [i (dec n)]
           (cond (neg? i) -1
                 (contains? reasoning-sentence-boundary-chars (.charAt s i)) i
                 :else (recur (dec i))))

         end
         (if (neg? (long boundary-idx))
           0
           (loop [j (inc (long boundary-idx))]
             (if (and (< j n) (contains? reasoning-boundary-trailing-chars (.charAt s j)))
               (recur (inc j))
               j)))]

        (if (> (- n (long end)) (long max-chars))
          s ;; long boundary-less tail → reveal everything
          (subs s 0 end))))))

(defn- entry-streaming-reasoning?
  "True while a timeline entry is STILL accumulating live reasoning and nothing
   past it (answer prose, a form, the final) has landed yet — the only window
   where the sentence-buffer should hold a trailing partial. Once the model
   moves on, the full thinking is revealed."
  [entry]
  (and (map? entry)
       (some? (:thinking entry))
       (not (:done? entry))
       (nil? (:final entry))
       (empty? (:forms entry))
       (str/blank? (str (:assistant-prose entry)))
       (str/blank? (str (:content-stream entry)))))

(defn- clip-live-reasoning
  "Project a live progress `timeline` for painting: sentence-buffer the reasoning
   of any entry that's still actively streaming it (see
   `entry-streaming-reasoning?`). Non-map / completed entries pass through
   untouched, so restored/final traces always show the full thinking. Only a
   real timeline VECTOR (the tracker's `as-vec`) is projected; any other shape
   passes through verbatim."
  [timeline]
  (if-not (vector? timeline)
    timeline
    (mapv (fn [entry]
            (if (entry-streaming-reasoning? entry)
              (assoc entry
                :thinking (clip-reasoning-to-sentence (:thinking entry)
                                                      reasoning-live-tail-reveal-chars))
              entry))
          timeline)))

(defn- make-progress-render-updater
  ([dispatch-fn] (make-progress-render-updater dispatch-fn #(System/currentTimeMillis) nil))
  ([dispatch-fn now-ms-fn] (make-progress-render-updater dispatch-fn now-ms-fn nil))
  ([dispatch-fn now-ms-fn schedule-fn]
   ;; Rate-limit live-progress redraws WITHOUT ever painting a stale frame.
   ;;
   ;; ONE source of truth for WHAT to paint: `latest`, overwritten by every
   ;; chunk. The timeline is cumulative/monotonic (each chunk is a SUPERSET of
   ;; the last — more reasoning, a new form, a form result), so the only correct
   ;; thing to ever paint is the latest timeline. EVERY dispatch — immediate,
   ;; throttled-due, or trailing-edge flush — paints `@latest`. This is what
   ;; fixes the "I see thinking but no code" bug: the old design stashed a
   ;; per-phase timeline SNAPSHOT, and a late `:reasoning` flush re-painted that
   ;; pre-forms snapshot AFTER the code had arrived, wiping it. Painting
   ;; `@latest` makes a stale frame impossible — a late flush merely repaints
   ;; the current state (a harmless duplicate the render loop coalesces).
   ;;
   ;; WHEN to paint is throttled PER-PHASE: `:reasoning` and `:content` each
   ;; redraw at most once per `live-progress-render-interval-ms` on their OWN
   ;; clock, so a fast content stream can't starve reasoning frames (or vice
   ;; versa). Lifecycle chunks (`:form-start` / `:form-result` /
   ;; `:iteration-final` / …) bypass the throttle and paint immediately so
   ;; block boundaries never wait; they also cancel any pending flush (which
   ;; would only repaint the same @latest).
   (let
     [latest
      (atom nil)

      ;; freshest timeline — the only thing painted
      last-by-phase
      (atom {})

      ;; per-phase last-dispatch clock
      scheduled-by-phase
      (atom {})

      ;; per-phase pending trailing-flush future
      schedule!
      (or schedule-fn
          (fn default-schedule! [^Runnable f ^long delay-ms]
            (.schedule progress-trailing-flush-scheduler f delay-ms TimeUnit/MILLISECONDS)))

      dispatch!
      (fn []
        (dispatch-fn [:set-progress-iterations (clip-live-reasoning @latest)]))]

     (letfn [(cancel-pending! [phase]
               (when-let [f (get @scheduled-by-phase phase)]
                 (try (.cancel ^java.util.concurrent.Future f false) (catch Throwable _ nil)))
               (swap! scheduled-by-phase dissoc phase))
             (flush-phase! [phase]
               ;; Trailing-edge fire: bump the phase clock and paint @latest
               ;; (NOT a stashed snapshot — @latest is always current).
               (swap! scheduled-by-phase dissoc phase)
               (swap! last-by-phase assoc phase (long (or (now-ms-fn) 0)))
               (dispatch!))]
       (fn [timeline chunk]
         (reset! latest timeline) ;; the freshest state always wins
         (let
           [now
            (long (or (now-ms-fn) 0))

            phase
            (:phase chunk)

            throttled?
            (contains? throttled-streaming-phases phase)

            prev
            (when throttled? (get @last-by-phase phase))

            due?
            (or (nil? prev) (>= (- now (long prev)) (long live-progress-render-interval-ms)))]

           (cond
             ;; Lifecycle chunk → paint now; cancel pending flushes (they would
             ;; only repaint the same @latest).
             (not throttled?) (do (doseq [p (keys @scheduled-by-phase)]
                                    (cancel-pending! p))
                                  (dispatch!))
             ;; Throttled, past its window → paint now; this dispatch
             ;; supersedes any pending flush for the phase.
             due? (do (cancel-pending! phase) (swap! last-by-phase assoc phase now) (dispatch!))
             ;; Throttled, inside the window, no flush queued → queue ONE so a
             ;; stall after the last drop still reaches the screen.
             (nil? (get @scheduled-by-phase phase))
             (let
               [delay-ms
                (max 1 (- (long live-progress-render-interval-ms) (- now (long prev))))

                ^Runnable task
                (fn []
                  (try (flush-phase! phase) (catch Throwable _ nil)))

                f
                (schedule! task (long delay-ms))]

               (swap! scheduled-by-phase assoc phase f))
             ;; Throttled, inside the window, flush already queued → @latest was
             ;; updated above; nothing else to do.
             :else nil)))))))

(defn- normalize-theme-name
  [v]
  (let
    [s (cond (keyword? v) (name v)
             (string? v) (str/trim v)
             :else nil)]
    (keyword (if (str/blank? s) shared-theme/default-theme-id s))))

(defn- normalize-settings
  "Coerce the two settings keys this layer still OWNS:
     `:theme-name`           — enum, picked from registered themes
                              (dynamic; not in the toggles registry).
     `:contributors-disabled` — set of contributor ids the user wants
                              hidden.

   Every other former settings key (`:show-thinking`,
   `:show-iterations`, `:show-silent`, `:show-timestamps`,
   `:mouse-selection-copy`,
   `:reasoning-level`, `:openai-codex-verbosity`) now lives in the
   toggles registry. The `:settings` map in app-db is a cached
   projection of (registry + these two locals); a listener wired in
   `init!` keeps the projection in sync."
  [settings]
  (-> settings
      (update :theme-name normalize-theme-name)
      (update :contributors-disabled
              (fn [v]
                (cond (nil? v) #{}
                      (set? v) v
                      :else (set v))))))

(defn- migrated-toggle-projection
  "Pull the migrated boolean + enum toggles back into a flat
   `:settings`-shaped map so existing consumers
   (`(get settings :show-thinking ...)`) keep working without
   reaching into the registry directly. The registry is the source
   of truth; this projection is the cached view."
  []
  ;; thinking / full trace / silent calls / timestamps are ALWAYS shown now
  ;; (their toggles were retired — the trace IS the transcript). Kept as `true`
  ;; constants so the many `(get settings :show-thinking ...)` readers keep
  ;; working untouched.
  {:show-thinking true
   :show-iterations true
   :show-silent true
   :show-timestamps true
   :mouse-selection-copy true
   :reasoning-level (vis/toggle-value "reasoning_level")
   :openai-codex-verbosity (vis/toggle-value "openai_codex_verbosity")})

(def default-settings
  "Per-user TUI settings. Persisted to `~/.vis/state.yml` under
   `:tui-settings`.

     theme-name - reusable channel theme id. Default `:vis-light`; extension
         themes are declared through `:ext/theme` and surfaced in Settings.

     show-thinking / show-iterations / show-silent - high-signal content
         controls. Thinking is forced OFF so provider chain-of-thought is
         not shown as assistant prose. Forensics still keep reasoning in the
         DB / reproduction surfaces; chat UI normally hides successful
         :vis/silent system calls unless show-silent is enabled.

     reasoning-level - base model thinking depth for reasoning-capable
         models. Default `:balanced`; users can cycle it via
         Ctrl+K -> Settings -> Providers & Models.

     openai-codex-verbosity - Codex-only output detail knob.
         Default `:low`; users can cycle it via Ctrl+K -> Settings -> Providers & Models.

     show-timestamps - chrome control. Default OFF because timestamps
         duplicate info already on screen.

     mouse-selection-copy - app-side terminal selection. Default ON so
         drag-selecting visible text copies it on mouse release even while
         the fullscreen TUI has mouse reporting enabled.

   The previous `:show-iteration-headers` and `:show-final-answer-header`
   toggles were removed: the ITERATION N / CODE N / STDOUT / ERROR /
   FINAL ANSWER superscripts they controlled have been deleted from
   the rendering pipeline outright (the visual zones already convey
   the same boundaries without the labels)."
  {:theme-name (keyword shared-theme/default-theme-id)
   ;; Set of contributor ids the user wants hidden in the TUI
   ;; header / footer. Each extension that contributes a row /
   ;; segment / status registers under a keyword id (e.g. :goal
   ;; from vis-goal, :voice from vis-foundation-voice). Adding the id
   ;; to this set skips that contributor's rendering. Default empty
   ;; (every registered contributor shows). See
   ;; `com.blockether.vis.ext.channel-tui.contributors`.
   :contributors-disabled #{}})

(defn- load-persisted-settings
  "Read the string-keyed `tui-settings` YAML block and adapt its fixed fields."
  []
  (let
    [raw
     (try (vis/load-config-raw) (catch Throwable _ nil))

     saved
     (when (map? raw) (get raw "tui-settings"))

     runtime-saved
     (when (map? saved)
       (cond-> {}
         (contains? saved "theme-name")
         (assoc :theme-name (get saved "theme-name"))

         (contains? saved "contributors-disabled")
         (assoc :contributors-disabled (get saved "contributors-disabled"))))]

    (normalize-settings (merge default-settings runtime-saved))))

(defn- persist-settings!
  "Write `settings` back into `~/.vis/state.yml` under
   `:tui-settings`, preserving every other key in the file. Failures
   are swallowed - a config-save failure should never crash a TUI
   that's already otherwise healthy."
  [settings]
  (try (let [raw (or (vis/load-config-raw) {})]
         (vis/save-config! (assoc raw "tui-settings" settings)))
       (catch Throwable _ nil)))

(defn- apply-settings-update!
  "Merge `new-settings` over the local-owned slice (theme +
   contributors-disabled), persist that slice into
   `~/.vis/state.yml`, and rebuild the cached `:settings` view by
   overlaying the toggle projection. Migrated keys ignored here —
   they route through `vis/toggle-set-value!` / `cycle-value!` and
   the listener installed in `init!` keeps the projection coherent."
  [db new-settings]
  (render/invalidate-cache!)
  (let
    [local-merged
     (normalize-settings (merge default-settings
                                (select-keys (:settings db) (keys default-settings))
                                (select-keys new-settings (keys default-settings))))

     projected
     (merge (migrated-toggle-projection) local-merged)]

    (tui-theme/apply-theme! (:theme-name local-merged))
    ;; Re-emit OSC 11 so the emulator's window padding (the un-themed
    ;; "outer" rim around the Lanterna grid) is recolored to the NEW
    ;; theme background. `enable-terminal-escape-modes!` sets this once at
    ;; startup; a LIVE theme switch must refresh it too, otherwise the rim
    ;; keeps the previous theme's background.
    (let [^com.googlecode.lanterna.TextColor$RGB c tui-theme/terminal-bg]
      (try (input/set-default-bg! @vis/tty-out (.getRed c) (.getGreen c) (.getBlue c))
           (catch Throwable _ nil)))
    (persist-settings! local-merged)
    (assoc db :settings projected)))

(defn- model-entry
  [provider model]
  (when-let [model-name (and model (vis/model-name model))]
    (when (:id provider) {:provider-id (:id provider) :model model-name})))

(defn- entries-from-providers
  "Model-cycle entries derived from a provider fleet vec, priority order.
   Shared by the LIVE `model-cycle-entries` (the C-x m handler) and the
   CACHED footer variant so the two always derive identically."
  [providers]
  (->> providers
       (mapcat (fn [provider]
                 (keep #(model-entry provider %) (:models provider))))
       vec))

(defn- model-cycle-entries
  "Entries for the Ctrl+T model cycle, read from the LIVE provider fleet
   (`vis/configured-providers` — the SAME source the web picker uses) in
   priority order. Reading live (not a stale `:config db` snapshot) means a
   provider reorder / add / remove done after launch — or from another
   channel — is reflected immediately, and the cycle advances the PROVIDER,
   not just the model name inside an outdated set. The `_config` arg is kept
   for the existing caller but intentionally ignored now that the source is
   live."
  [_config]
  (entries-from-providers (try (vis/configured-providers) (catch Throwable _ nil))))

(defn- model-cycle-entries-cached
  "Footer-frequency variant of `model-cycle-entries`: the SAME entries,
   derived from the CACHED fleet snapshot (`vis/configured-providers-cached`)
   so the per-frame footer read never re-runs the full provider enumeration
   on the render thread — that enumeration parses four config files per call
   and costs ~200ms on machines with slow file IO, which stalled every live
   frame (issue #29). The C-x m `:cycle-model` handler keeps the LIVE
   `model-cycle-entries` (Tab-through must be exact); the fleet cache is
   invalidated on every same-process provider mutation, so the two agree
   outside a bounded cross-process staleness window."
  []
  (entries-from-providers (try (vis/configured-providers-cached) (catch Throwable _ nil))))

(defn- entry-index
  "Index (0-based) of the model-cycle ENTRY matching `provider`+`model`
   (both strings), or nil when nothing matches. Shared by the `:cycle-model`
   handler and the footer button so the two agree on the numbering."
  [entries provider model]
  (some (fn [[i e]]
          (when (and (= (:model e) model) (= (name (:provider-id e)) provider)) i))
        (map-indexed vector entries)))

(defn model-cycle-position
  "Live `[position total]` (1-based) of `provider`/`model` within the model
   cycle, or nil when the current model isn't one of the cycle entries. The
   footer button renders this as the `n/N` inside its `(cycle …)` hint. Runs
   on the render thread EVERY footer frame, so it derives from the cached
   fleet snapshot (`model-cycle-entries-cached`) — the entries are the same
   ones the C-x m handler steps through, so the count the button shows is
   exactly the count the cycle walks (issue #29)."
  [provider model]
  (let [entries (model-cycle-entries-cached)]
    (when-let [idx (entry-index entries provider model)]
      [(inc (long idx)) (count entries)])))

(defn- current-model-info
  []
  (when-let [router (try (vis/get-router) (catch Throwable _ nil))]
    (try (vis/resolve-effective-model router) (catch Throwable _ nil))))

(defn- session-model-pref
  [db]
  (or (:session-model-pref db)
      (when-let [sid (get-in db [:session :id])]
        (try (vis/gateway-session-model-cached sid) (catch Throwable _ nil)))))

(defn- current-provider-id
  "Provider selected for this session, falling back to the router default."
  [db]
  (some-> (or (:provider (session-model-pref db)) (:provider (current-model-info)))
          name
          keyword))

(def ^:private ^:const max-tabs 8)

(def untitled-session-label
  "Default workspace label for a session without a title yet.

   Aliases the channel-agnostic value in `internal/header` so the TUI
   and other channels all show the same placeholder. Kept exported
   here for callers (and tests) that already reach in via the state
   namespace."
  vh/untitled-session-label)

(def ^:private starting-session-label
  "Placeholder tab label shown while an optimistically-opened new session's
   environment is still building on a background worker (see
   `:open-building-tab`)."
  "Starting…")

(defn- history-user-texts
  [history]
  (->> (or history [])
       (keep (fn [message]
               (when (= :user (:role message)) (:text message))))
       vec))

(defn- tab-number
  [entry]
  (when-let
    [[_ n] (some->> entry
                    :id
                    name
                    (re-matches #"tab-(\d+)"))]
    (Long/parseLong n)))

(defn- next-tab-number [entries] (inc (long (reduce max 0 (keep tab-number entries)))))

(defn- insert-tab-grouped
  "Insert a freshly minted tab entry ADJACENT to its project group: right
   after the LAST existing tab sharing its `vh/tab-group-root`. No root, or
   no open tab of that project yet → append at the end (a new group starts
   on the right). Keeping same-project tabs contiguous in `:tabs` is what
   makes the header strip, the numeric jumps (C-x N) and the cycle order all
   read as project groups — no render-time reordering anywhere."
  [entries entry]
  (let
    [entries
     (vec entries)

     root
     (vh/tab-group-root entry)

     last-idx
     (when root
       (->> entries
            (keep-indexed #(when (= root (vh/tab-group-root %2)) %1))
            last))]

    (if last-idx
      (vec (concat (subvec entries 0 (inc (long last-idx)))
                   [entry]
                   (subvec entries (inc (long last-idx)))))
      (conj entries entry))))

(defn- base-tab-entry
  [db]
  {:id (or (:active-tab-id db) :main)
   :label (let [title (:title db)]
            (if (and (string? title) (not (str/blank? title))) title untitled-session-label))})

(defn- tabs-or-base
  [db]
  (let [entries (vec (:tabs db))]
    (if (seq entries) entries [(base-tab-entry db)])))

(defn- label-from-title
  [title fallback]
  (if (and (string? title) (not (str/blank? title))) title (or fallback untitled-session-label)))

(defn- ensure-tabs
  [db]
  (let
    [entries
     (tabs-or-base db)

     active-id
     (or (current-tab-id (assoc db :tabs entries)) (:id (first entries)))]

    (assoc db
      :tabs (mapv (fn [entry]
                    (cond-> (dissoc entry :active?)
                      (= (:id entry) active-id)
                      (assoc :active? true)))
                  entries)
      :active-tab-id active-id)))

(defn- restore-tab
  "Pull the per-tab locals for `workspace-id` back into the active db.

   Layout is cached per tab so revisiting a tab can paint against its own
   established geometry immediately instead of first rendering with no layout and
   visibly settling on a later frame. The cache is safe only while terminal
   dimensions match the tab being left; after a resize, discard the target layout
   so the first frame recomputes against the new geometry.

   Switching tabs is deliberately a **latest-events jump**, not a restoration of a
   prior reading position. The incoming transcript was not on screen, and its
   background turn may have grown while hidden; retaining either a parked offset
   or eased `:pos` produces stale history followed by an unwanted scroll. Resetting
   to FOLLOW makes the first frame show the current tail, including for live turns.
   The target layout remains reusable when terminal dimensions match."
  [db workspace-id]
  (let
    [entry
     (some #(when (= (:id %) workspace-id) %) (:tabs db))

     ;; Defaults FIRST: `restore-tab` only merges, so any per-tab key absent from
     ;; the stored snapshot would otherwise keep the value the tab we are LEAVING
     ;; left at the root (a live turn id, a cancel stamp, a paused queue) and hand
     ;; it to the incoming tab. Filling from `empty-tab-state` makes the restore
     ;; total for every key in `tab-state-keys`, even for a partially seeded tab.
     locals
     (merge (empty-tab-state) (get-in db [:tab-locals workspace-id]))

     current-layout
     (:layout db)

     target-layout
     (:layout locals)

     compatible-layout?
     (and (map? current-layout)
          (map? target-layout)
          (every? #(and (some? (get target-layout %))
                        (= (get current-layout %) (get target-layout %)))
                  [:cols :rows]))

     db'
     (-> (merge db locals)
         ;; A workspace switch always enters at the live tail. Per-tab scroll
         ;; snapshots still matter while a tab stays focused, but never across a
         ;; switch: a background/live tab should not reopen above its newest event.
         (assoc :scroll scroll/follow)
         (cond->
           (not compatible-layout?)
           (dissoc :layout)))]

    ;; The tab ENTRY carries the workspace root reliably (set at creation). A
    ;; stale/empty tab-locals snapshot — taken before `:set-workspace` landed —
    ;; can null the denormalized top-level `:workspace/root`; backfill it from
    ;; the entry so every reader (footer, F2 context panel, `/cd` picker,
    ;; magit) keeps the ACTIVE session's root and never the vis process cwd.
    (cond-> db'
      (and (nil? (:workspace/root db')) (:workspace/root entry))
      (assoc :workspace/root (:workspace/root entry))

      (and (nil? (:workspace db')) (:workspace entry))
      (assoc :workspace (:workspace entry)))))

(defn- activate-tab
  [db workspace-id]
  (-> db
      sync-active-tab
      (assoc :active-tab-id workspace-id)
      (update :tabs
              (fn [entries]
                (mapv (fn [entry]
                        (cond-> (dissoc entry :active?)
                          (= (:id entry) workspace-id)
                          (-> (assoc :active? true)
                              (dissoc :unread?))))
                      entries)))
      (restore-tab workspace-id)))

(defn- update-tab
  "Apply `f` to the db as seen by tab `workspace-id`, wherever that tab lives.

   The ACTIVE tab is the db root, so `f` runs on it directly. A BACKGROUND tab
   is a `[:tab-locals id]` snapshot, and `f` needs a whole db to work on: the
   globals (`:tabs`, `:settings`, `:config`, …) come from the root, the per-tab
   half MUST come from that tab alone.

   `empty-tab-state` is therefore the base of the per-tab half, exactly as in
   `restore-tab`. Without it, a snapshot that is only PARTIALLY seeded — a
   pre-allocated project tab that has never been focused and only ever got a
   `:title` written into its locals — leaves every missing per-tab key to be
   filled by `merge db`, i.e. by the tab you are LOOKING AT. A `turn.queued`
   broadcast for that tab then parked the ACTIVE tab's `:session`, `:messages`,
   `:pending-sends` and live `:gateway-turn-id` into it, so the queue row showed
   up under the wrong session (and could be swallowed outright by
   `live-turn-mirror?` matching the borrowed live turn id)."
  [db workspace-id f]
  (let [workspace-id (or workspace-id (current-tab-id db))]
    (if (and workspace-id (not= workspace-id (current-tab-id db)))
      (update-in db
                 [:tab-locals workspace-id]
                 (fn [snapshot]
                   (tab-snapshot (f (merge db (empty-tab-state) snapshot)))))
      (f db))))

(defn- tab-session-id
  [db workspace-id]
  (or (some-> (get-in db [:tab-locals workspace-id :session :id])
              str)
      ;; A PENDING pre-allocated tab has no locals-bound session until its lazy
      ;; hydration lands — its identity rides on the tab ENTRY's `:session-id`.
      (some #(when (= (:id %) workspace-id)
               (some-> (:session-id %)
                       str))
            (:tabs db))))

(defn- reasoning-effort-configurable?
  []
  (let [info (current-model-info)]
    (or (nil? info)
        (and (boolean (:reasoning? info))
             (not= false (:reasoning-effort? info))
             (not= :zai-thinking (:reasoning-style info))))))

(defn init!
  "Initialize app-db with default state. The persisted layer now
   has two halves: `~/.vis/config.edn :tui-settings` holds the
   handful of locals this channel still owns (theme +
   contributors-disabled). All migrated booleans / enums live in the
   toggles registry (`:toggles` slot, loaded by
   `vis/toggles-hydrate-from-config!` in `screen/run-chat!`). We
   merge a one-shot projection of the registry into `:settings` here
   so the first paint is coherent; ongoing changes flow through the
   listener registered there."
  []
  (let
    [local-settings
     (load-persisted-settings)

     settings
     (merge (migrated-toggle-projection) local-settings)]

    (tui-theme/apply-theme! (:theme-name settings))
    (reset! app-db
      {:config nil
       :session nil
       :title nil
       :messages []
       :scroll scroll/follow
       :input (input/empty-input)
       :input-history []
       :input-history-index nil
       :input-history-draft nil
       :slash-command-index 0
       :slash-command-hidden? false
       :submitted-input nil
       ;; Paste registry. Each multi-line / large
       ;; clipboard payload lands here keyed by an auto-
       ;; incrementing id; the input buffer carries a
       ;; placeholder token `[Pasted #N: ...]` instead of the
       ;; raw text. Send-time substitution uses this map
       ;; to materialise the full content before the
       ;; message reaches the agent. Cleared on send.
       :pastes {}
       :paste-counter 0
       :loading? false
       :cancel-token nil
       :cancelling? false
       :progress nil
       :settings settings
       :provider-limits nil
       :channel-status {}
       :detail-expansions {}
       :tabs []
       :active-tab-id nil
       :tab-locals {}
       :dialog-open? false
       ;; Render thread coordination - see render-monitor docstring.
       :render-version 0
       :shutdown? false
       ;; Populated by the render thread after each frame so the
       ;; input thread's scroll handlers know how big the
       ;; messages area is right now. nil before the first paint.
       :layout nil})))

;;; ── Pure event handlers ────────────────────────────────────────────────────
(reg-event-db :set-config
              (fn [db [_ config]]
                (vis/reload-config!)
                ;; Rebuild ONLY when a router already exists — i.e. a real
                ;; mid-session config change that must reseat cached envs
                ;; (`rebuild-router!` swaps the global singleton; cached envs keep
                ;; their snapshot, so the next turn would otherwise run against
                ;; the previous model). On the INITIAL load the router is nil:
                ;; DON'T build it here — `get-router` builds it lazily on the
                ;; first turn, so OAuth token fetches (Copilot/Codex) never run
                ;; at TUI boot and a failing provider can't stall/kill startup.
                (when (and (seq (:providers config)) (vis/router-initialized?))
                  (let [r (vis/rebuild-router! config)]
                    (vis/refresh-cached-routers! r)))
                (assoc db :config config)))

(reg-event-db :set-dialog-open
              (fn [db [_ open?]]
                (assoc db :dialog-open? (boolean open?))))

(reg-event-db :update-settings
              (fn [db [_ new-settings]]
                (apply-settings-update! db new-settings)))

(def ^:private render-neutral-toggle-ids
  "Toggle ids whose value NEVER changes a painted glyph.

   These are provider request knobs (what the next turn asks the model for),
   not view options, so busting the render caches for them is pure damage:
   `virtual/invalidate-heights!` drops every sticky height, the whole
   transcript falls back to estimates and the view visibly jumps while the
   background re-warm lands. Cycling reasoning effort with Ctrl+X r must not
   repaint history."
  #{"reasoning_level" "openai_codex_verbosity"})

(reg-event-db
  :resync-toggle-settings
  ;; Triggered by the toggles-registry listener whenever a flip
  ;; happens (settings dialog row, programmatic vis/toggle-set-value!,
  ;; provider-side cycle event). Rebuilds the cached `:settings`
  ;; projection so consumers reading `(get settings :show-thinking)`
  ;; etc. observe the new value on the very next paint.
  ;;
  ;; The optional toggle id says WHICH toggle flipped; see
  ;; `render-neutral-toggle-ids`. Omitted id = unknown flip = bust
  ;; everything, the old conservative behaviour.
  (fn [db [_ toggle-id]]
    ;; Drop BOTH render caches. A registry toggle changes what a
    ;; bubble paints, but the projected lines live in
    ;; `render/fmt-cache` (keyed on message identity, NOT toggle
    ;; value) and the row count lives in the `virtual` height
    ;; cache (its `settings-fingerprint` only tracks the keys
    ;; mirrored into `:settings` — registry-only toggles like
    ;; `:vis/show-thinking` aren't in
    ;; it). Without this bust the flip resolved live in the
    ;; registry but the painter kept handing back stale cached
    ;; lines/heights, so the new value only appeared after a
    ;; restart cleared the process caches. The local-settings
    ;; path already busts fmt-cache via `apply-settings-update!`;
    ;; the registry path needs the same on both caches.
    (let
      [render-neutral? (contains? render-neutral-toggle-ids
                                  (some-> toggle-id
                                          name))]
      ;; Drop BOTH render caches. A registry toggle changes what a
      ;; bubble paints, but the projected lines live in
      ;; `render/fmt-cache` (keyed on message identity, NOT toggle
      ;; value) and the row count lives in the `virtual` height
      ;; cache (its `settings-fingerprint` only tracks the keys
      ;; mirrored into `:settings` — registry-only toggles like
      ;; `:vis/show-thinking` aren't in
      ;; it). Without this bust the flip resolved live in the
      ;; registry but the painter kept handing back stale cached
      ;; lines/heights, so the new value only appeared after a
      ;; restart cleared the process caches. The local-settings
      ;; path already busts fmt-cache via `apply-settings-update!`;
      ;; the registry path needs the same on both caches.
      (when-not render-neutral? (render/invalidate-cache!) (virtual/invalidate-heights!))
      (let
        [settings (merge (migrated-toggle-projection)
                         (select-keys (:settings db) (keys default-settings)))]
        ;; The invalidate above dropped EVERY sticky height - the whole
        ;; transcript is back on estimates. Re-warm in the background
        ;; (same worker the startup path uses) so total-h re-settles
        ;; while the user is still idle; without this the corrections
        ;; land mid-scroll and jump the scrollbar thumb. Width comes
        ;; from the last published layout; a nil layout (no frame yet)
        ;; skips - the startup warm is still in flight then anyway.
        (when-let [cols (and (not render-neutral?) (:cols (:layout db)))]
          (virtual/rewarm! (:messages db)
                           (max 1 (- (long cols) (long render/MESSAGE_SIDE_PAD)))
                           settings
                           {:session-id (get-in db [:session :id])
                            :detail-expansions (:detail-expansions db)
                            :on-warm #(dispatch [:bump-render-version])}))
        (assoc db :settings settings)))))

(reg-event-fx :cycle-reasoning-level
              (fn [db _]
                (if-not (reasoning-effort-configurable?)
                  {:db db
                   :fx [[:notify "Reasoning effort is not configurable for this model" :warn
                         settings-notification-ttl-ms]]}
                  ;; The flip is an EFFECT, never an in-swap mutation.
                  ;; `dispatch` runs a :fx handler's FUNCTION inside `swap!
                  ;; app-db` and only its returned effects afterwards. Calling
                  ;; `vis/toggle-cycle-value!` in the function body ran the
                  ;; registry listener synchronously, which re-entrantly
                  ;; dispatched :resync-toggle-settings; that inner swap
                  ;; committed, the outer CAS failed, the handler retried and
                  ;; cycled the level AGAIN — every retry guaranteeing the next
                  ;; one. That livelock is why Ctrl+X r span through reasoning
                  ;; levels forever instead of advancing by one.
                  {:db db :fx [[:cycle-toggle "reasoning_level" "Reasoning"]]})))

(reg-event-fx :cycle-codex-verbosity
              (fn [db _]
                (if-not (= :openai-codex (current-provider-id db))
                  {:db db
                   :fx [[:notify "Codex verbosity is only available for OpenAI Codex" :warn
                         settings-notification-ttl-ms]]}
                  ;; Effect, not an in-swap mutation - see :cycle-reasoning-level.
                  {:db db :fx [[:cycle-toggle "openai_codex_verbosity" "Codex verbosity"]]})))

(reg-event-fx :cycle-model
              ;; Ctrl+T cycles the ACTIVE SESSION's model preference — the SAME unified,
              ;; persisted per-session choice the web picker sets and the engine routes
              ;; (was: reorder the GLOBAL config, which changed the default for every
              ;; session and wasn't per-session). The footer reads the same pref, so the
              ;; display follows. `db` reflects the current tab, so `(:session db)` is the
              ;; active session.
              (fn [db _]
                (let
                  [sid
                   (get-in db [:session :id])

                   config
                   (or (:config db) (vis/load-config) {:providers []})

                   entries
                   (model-cycle-entries config)]

                  (cond (nil? sid) {:fx [[:notify "Open a session first to choose its model" :warn
                                          settings-notification-ttl-ms]]}
                        (empty? entries) {:fx [[:notify "No models configured" :warn
                                                settings-notification-ttl-ms]]}
                        :else
                        ;; `current` is the explicit per-session preference when present;
                        ;; otherwise fall back to the effective router model already displayed
                        ;; in the footer. A fresh session has no stored pref, and the footer
                        ;; shows the router default; Ctrl+T must advance PAST that default, not
                        ;; "set" the same first entry and appear to do nothing.
                        (let
                          [effective
                           (current-model-info)

                           current
                           (or (vis/gateway-session-model sid)
                               (when effective
                                 {:provider (some-> (:provider effective)
                                                    name)
                                  :model (:name effective)}))

                           idx
                           (or (entry-index entries (:provider current) (:model current)) -1)

                           next-e
                           (nth entries (mod (inc (long idx)) (count entries)))

                           pid
                           (name (:provider-id next-e))

                           pref
                           {:provider pid :model (:model next-e)}]

                          {:db (assoc db :session-model-pref pref)
                           :fx [[:set-session-model sid pid (:model next-e)]
                                [:notify (str "Model: " pid "/" (:model next-e)) :info
                                 settings-notification-ttl-ms]]})))))

(reg-event-fx
  :set-model
  ;; Per-session model PICKER (C-x o / palette "Choose Model…"):
  ;; sets the ACTIVE SESSION's model to an EXPLICIT provider+model,
  ;; or — with both nil (the "★ router default" row) — CLEARS the
  ;; per-session override. Writes the SAME persisted per-session
  ;; pref the cycle (C-x m) and the web footer picker set, so the
  ;; footer display follows. `db` reflects the current tab.
  (fn [db [_ provider model]]
    (let [sid (get-in db [:session :id])]
      (cond (nil? sid) {:fx [[:notify "Open a session first to choose its model" :warn
                              settings-notification-ttl-ms]]}
            (and provider model)
            {:db (assoc db :session-model-pref {:provider provider :model model})
             :fx [[:set-session-model sid provider model]
                  [:notify (str "Model: " provider "/" model) :info settings-notification-ttl-ms]]}
            :else {:db (dissoc db :session-model-pref)
                   :fx [[:set-session-model sid nil nil]
                        [:notify "Model: router default" :info settings-notification-ttl-ms]]}))))

(reg-event-db :clear-session-model-pref
              ;; The gateway REFUSED (or never received) the pick: drop the optimistic
              ;; value so the footer falls back to the session's REAL preference instead of
              ;; advertising a model the session never got. Scoped to `sid` — a switch that
              ;; happened while the PATCH was in flight must not clobber the new session's
              ;; display.
              (fn [db [_ sid]]
                (if (or (nil? sid) (= (str sid) (str (get-in db [:session :id]))))
                  (dissoc db :session-model-pref)
                  db)))

(reg-event-db :sync-session-model
              ;; The session's model preference changed SOMEWHERE ELSE — the companion app,
              ;; another TUI process/tab, an embedded caller — and the gateway broadcast it
              ;; as `session.model_updated`. Project it onto the OWNING tab so the footer
              ;; chip follows live instead of showing this process's last local pick until a
              ;; reopen. Same one-source-of-truth the picker writes; nil provider/model means
              ;; the override was cleared, so fall back to the router default.
              (fn [db [_ workspace-id {:keys [provider model]}]]
                (update-tab db
                            workspace-id
                            (fn [w]
                              (if (and (not-empty (str provider)) (not-empty (str model)))
                                (assoc w
                                  :session-model-pref {:provider (str provider) :model (str model)})
                                (dissoc w :session-model-pref))))))

(reg-event-db
  :set-layout
  (fn [db [_ layout]]
    ;; Pushed in by the render thread; intentionally does NOT bump
    ;; render-version (see no-render-bump-events). A terminal-result
    ;; reveal survives only if this first measured layout grew; otherwise
    ;; restore FOLLOW's exact auto-bottom lock immediately.
    (let [max-s (max 0 (- (long (or (:total-h layout) 0)) (long (or (:inner-h layout) 0))))]
      (assoc db
        :layout layout
        :scroll (scroll/settle-reveal (:scroll db) max-s)))))

(defn- park-scroll-for-toggle
  "Pin the viewport before a disclosure toggle mutates message heights.

   Always snap the scroll intent to the row that is CURRENTLY painted. In
   FOLLOW mode this switches from bottom-pinning to the layout's top-message
   anchor, so an expanded body grows downward instead of pushing the clicked
   row up. For an already-parked scroll it also cancels any latent ease target:
   retaining `:offset` while `:pos` is on screen would keep racing the content
   upward immediately after the click. A settled parked scroll is unchanged in
   value. No-op before the first paint (no layout)."
  [db]
  (if-some [eff (get-in db [:layout :eff-scroll])]
    (assoc db :scroll (scroll/parked (long eff)))
    db))

(reg-event-db :toggle-detail
              (fn [db [_ session-id node-id explicit-expand?]]
                (let
                  [k
                   [(str session-id) (str node-id)]

                   db
                   (park-scroll-for-toggle db)]

                  (if (some? explicit-expand?)
                    ;; Caller knows the row's CURRENT effective state (from the click
                    ;; region's `:collapsed?`) and passes the desired new expanded state.
                    ;; Store it EXPLICITLY (true/false) — required for rows whose default
                    ;; is expanded (BLOCK header, op rows): the old absent/true-only model
                    ;; could never represent "explicitly collapsed", so collapsing a
                    ;; default-expanded row was a no-op.
                    (assoc-in db [:detail-expansions k] (boolean explicit-expand?))
                    ;; Legacy 2-arg path (default-collapsed rows): absent <-> true.
                    (update db
                            :detail-expansions
                            (fn [m]
                              (let [expanded? (true? (get m k false))]
                                (if expanded? (dissoc m k) (assoc (or m {}) k true)))))))))

(reg-event-db :collapse-all-details
              ;; C-x [ — collapse EVERY disclosure. Wipe per-node overrides and set the
              ;; bulk baseline; `render/detail-expanded?` reads `:baseline` when a node has
              ;; no explicit override, so a later click can still expand one item.
              (fn [db _]
                (-> (park-scroll-for-toggle db)
                    (assoc :detail-expansions {:vis.channel-tui/baseline :collapse}))))

(reg-event-db :expand-all-details
              ;; C-x ] — expand EVERY disclosure (per-node overrides wiped, baseline set).
              (fn [db _]
                (-> (park-scroll-for-toggle db)
                    (assoc :detail-expansions {:vis.channel-tui/baseline :expand}))))

(reg-event-db :toggle-all-details
              ;; C-x TAB / C-x S-TAB — Emacs global fold cycle (org/magit `<backtab>`): if the
              ;; bulk baseline is currently expanded, collapse EVERY disclosure; otherwise
              ;; expand them all. One keystroke flips the whole transcript (per-node overrides
              ;; wiped), mirroring org-mode's buffer-wide visibility cycle.
              (fn [db _]
                (let
                  [expanded? (= :expand (get-in db [:detail-expansions :vis.channel-tui/baseline]))]
                  (-> (park-scroll-for-toggle db)
                      (assoc :detail-expansions {:vis.channel-tui/baseline
                                                 (if expanded? :collapse :expand)})))))

(reg-event-db :set-detail-labels
              ;; C-x t — vim-style jump labels. `on?` true turns the overlay ON
              ;; (the renderer stamps a letter badge on every disclosure that was
              ;; visible WHEN the mode opened and the next keypress toggles that
              ;; one); false turns it off. `labels` is the FROZEN `[label region]`
              ;; assignment captured from the frame under the cursor at open —
              ;; the painter and the input handler both read it, so a live stream
              ;; growing underneath can't reshuffle the letters or race the
              ;; keypress mid-jump. A pure flag flip — no height changes, so no
              ;; scroll parking.
              (fn [db [_ on? labels]]
                (assoc db
                  :detail-labels-active? (boolean on?)
                  :detail-labels (when on? (vec labels)))))

(reg-event-db :select-preview-mode
              (fn [db [_ session-id node-id mode]]
                (assoc-in db [:detail-expansions [(str session-id) (str node-id)]] mode)))

(reg-event-db :bump-render-version
              (fn [db _]
                ;; No-op state mutator. The dispatcher itself bumps `:render-version` and
                ;; notifies the render monitor whenever an event lands (unless the event id is
                ;; in `no-render-bump-events`), so simply dispatching this event is enough to
                ;; wake the painter. Used by the mouse handler when a hover-state change needs
                ;; the chrome row repainted with its hover background.
                db))

(reg-event-db
  :create-tab
  (fn [db [_ opts]]
    (let
      [db
       (-> db
           ensure-tabs
           sync-active-tab)

       entries
       (vec (:tabs db))

       n
       (next-tab-number entries)

       id
       (keyword (str "tab-" n))

       workspace
       (:workspace opts)

       root
       (or (:workspace/root workspace) (:workspace/root opts))

       label
       (or (:label opts)
           (some-> workspace
                   :label
                   not-empty)
           (some-> workspace
                   :main
                   :branch
                   not-empty)
           untitled-session-label)

       entry
       (cond-> {:id id :label label :active? true}
         workspace
         (assoc :workspace workspace)

         root
         (assoc :workspace/root root))]

      (if (>= (count entries) max-tabs)
        db
        (cond->
          (-> db
              (assoc :tabs (insert-tab-grouped (mapv #(dissoc % :active?) entries) entry)
                     :active-tab-id id)
              (merge (empty-tab-state)))
          workspace
          (assoc :workspace workspace)

          root
          (assoc :workspace/root root))))))

(reg-event-db :select-tab-index
              (fn [db [_ idx]]
                (let
                  [db
                   (-> db
                       ensure-tabs
                       sync-active-tab)

                   entries
                   (vec (:tabs db))

                   idx
                   (if (#{:next :prev} idx)
                     (when (seq entries)
                       (let
                         [active-id
                          (or (:active-tab-id db)
                              (:id (some #(when (:active? %) %) entries))
                              (:id (first entries)))

                          current
                          (or (first (keep-indexed #(when (= (:id %2) active-id) %1) entries)) -1)

                          delta
                          (if (= :prev idx) -1 1)]

                         (mod (+ (long current) delta) (count entries))))
                     idx)]

                  (if-let [entry (and (integer? idx) (nth entries idx nil))]
                    (activate-tab db (:id entry))
                    db))))

(reg-event-db :select-tab-by-session
              (fn [db [_ session-id]]
                (let
                  [target-id
                   (some-> session-id
                           str)

                   db
                   (-> db
                       ensure-tabs
                       sync-active-tab)

                   entries
                   (vec (:tabs db))

                   entry
                   (when target-id
                     (some #(when (= target-id (tab-session-id db (:id %))) %) entries))]

                  (if entry (activate-tab db (:id entry)) db))))

(reg-event-fx
  :close-tab
  ;; Close one tab (default: the active tab). Removes it from `:tabs`,
  ;; drops its `:tab-locals` snapshot, and — if it was active — activates
  ;; the neighbor (same index, clamped). Refuses to close the last tab.
  ;;
  ;; RESOURCE RELEASE: when the closed tab was the LAST open view of an
  ;; IDLE session (no running turn, no queued/pending sends, and the sid
  ;; is not still open in another tab) we emit `:release-session-listener`
  ;; (drop the SSE title-listener) + `:release-session-runtime` (tell the
  ;; daemon to stop that session's background `shell` children / REPLs and drop its
  ;; live runtime). A session with a running or queued turn is LEFT alone —
  ;; it stays resumable and keeps streaming; only process exit force-stops.
  (fn [db [_ tab-id keep-project?]]
    (let
      [db
       (-> db
           ensure-tabs
           sync-active-tab)

       entries
       (vec (:tabs db))

       active-id
       (current-tab-id db)

       target-id
       (or tab-id active-id)

       idx
       (first (keep-indexed #(when (= (:id %2) target-id) %1) entries))]

      (if (or (nil? idx) (<= (count entries) 1))
        {:db db}
        (let
          [;; `sync-active-tab` above snapshotted the active tab into
           ;; `:tab-locals`, so EVERY tab's session + idle state now
           ;; lives there — read the closing tab's before we drop it.
           closing-snap
           (get-in db [:tab-locals target-id])

           closing-sid
           (some-> closing-snap
                   :session
                   :id
                   str)

           closing-idle?
           (and (not (:loading? closing-snap)) (empty? (:pending-sends closing-snap)))

           ;; AUTHORED submissions that never reached the gateway
           ;; (no :turn-id — submit failed, or the session was
           ;; busy/building when the tab closed programmatically,
           ;; e.g. a project switch). Dropping :tab-locals below
           ;; destroys their ONLY copy, so hand them to the
           ;; gateway queue of record instead (:submit-orphan-sends).
           orphan-texts
           (into []
                 (comp (remove :turn-id)
                       (keep (fn [{:keys [text]}]
                               (let
                                 [t (some-> text
                                            str)]
                                 (when-not (str/blank? t) t)))))
                 (:pending-sends closing-snap))

           remaining
           (vec (concat (subvec entries 0 idx) (subvec entries (inc (long idx)))))

           db
           (-> db
               (assoc :tabs remaining)
               (update :tab-locals dissoc target-id))

           open-elsewhere?
           (boolean (and closing-sid (some #(= closing-sid (tab-session-id db (:id %))) remaining)))

           db
           (if (= target-id active-id)
             (let
               [next-idx
                (min (long idx) (dec (count remaining)))

                next-id
                (:id (nth remaining next-idx))]

               (-> db
                   (assoc :active-tab-id next-id)
                   (update :tabs
                           (fn [es]
                             (mapv (fn [entry]
                                     (cond-> (dissoc entry :active?)
                                       (= (:id entry) next-id)
                                       (assoc :active? true)))
                                   es)))
                   (restore-tab next-id)))
             db)]

          {:db db
           :fx (cond-> []
                 ;; Tabs ARE the project's member sessions: an explicit close
                 ;; removes the session from the active project (it survives as a
                 ;; loose session, reachable via the navigator). Skipped on a
                 ;; project SWITCH (keep-project? swaps the VIEW without disowning
                 ;; the old set) or when the sid is still open in another tab.
                 (and closing-sid (not keep-project?) (not open-elsewhere?))
                 (conj [:unassign-session-project closing-sid])

                 (and closing-sid (seq orphan-texts))
                 (conj [:submit-orphan-sends closing-sid orphan-texts])

                 (and closing-sid closing-idle? (not open-elsewhere?))
                 (into [[:release-session-listener closing-sid]
                        [:release-session-runtime closing-sid]]))})))))

(reg-event-db :set-mouse-selection
              (fn [db [_ selection]]
                (assoc db :mouse-selection selection)))

(reg-event-db :clear-mouse-selection
              (fn [db _]
                (dissoc db :mouse-selection)))

(reg-event-db
  :set-provider-limits
  (fn [db [_ provider-id report]]
    (let [entry {:provider-id provider-id :report report :updated-at-ms (System/currentTimeMillis)}]
      (cond->
        (assoc db
          :provider-limits entry
          :provider-limits-force? false)
        ;; Keep the LAST report per provider, not just the active
        ;; slot: cycling the per-session model (C-x c) retargets the
        ;; poller, and without this the footer would blank to
        ;; "limits: loading…" on every switch even though the gateway
        ;; still has that provider's report warm in its own 15s
        ;; cache. The footer renders the remembered rows while the
        ;; refetch is in flight.
        provider-id
        (assoc-in [:provider-limits-cache provider-id] entry)))))

(reg-event-db :clear-provider-limits
              ;; Only the ACTIVE slot goes; the per-provider cache survives so a
              ;; switch back to a known provider paints instantly.
              (fn [db _]
                (assoc db
                  :provider-limits nil
                  :provider-limits-force? false)))

;; Ask the background limits poller to refetch on its NEXT tick instead of
;; waiting out the 60s stale window — dispatched after an auth flow so the
;; footer picks up a just-authenticated provider (or fresh quota) promptly.
(reg-event-db :force-provider-limits-refresh
              (fn [db _]
                (assoc db :provider-limits-force? true)))

(reg-event-db :shutdown
              (fn [db _]
                (assoc db :shutdown? true)))

(reg-event-db :set-workspace
              ;; Replace the session's current workspace record (trunk or draft) after a
              ;; turn that may have switched it (`/cd`, `/draft new | apply | abandon`).
              ;; Keep the denormalized root in lockstep; the footer reads it first.
              ;;
              ;; `workspace-id` = the tab whose session this workspace belongs to. A
              ;; BACKGROUND (sibling) session can complete a turn while another tab is
              ;; active; routing through `update-tab` writes the root into THAT tab (its
              ;; `:tab-locals`) instead of stomping the active tab's footer with the
              ;; wrong repo. Nil `workspace-id` (e.g. the /cd picker on the active
              ;; session) targets the active tab, as before.
              ;; A nil/blank workspace (a transient gateway miss on the picker-close
              ;; re-sync, or a caught error yielding nil) must NEVER stomp a good root:
              ;; nulling `:workspace/root` flips the footer's git chip to the vis process
              ;; cwd (the ENGINE's own repo) for a frame — the visible "flicker". Keep the
              ;; last-known-good workspace when the incoming one carries no root.
              (fn [db [_ ws workspace-id]]
                (if-not (get ws "root")
                  db
                  (update-tab db
                              workspace-id
                              (fn [d]
                                (assoc d
                                  :workspace ws
                                  :workspace/root (get ws "root")))))))

(def ^:private active-turn-state-keys
  [:loading? :cancelling? :cancelling-at-ms :progress :turn-start-ms :cancel-token :gateway-turn-id
   :live-turn-client-id :liveness-probed-at-ms])

(def ^:private cancel-gate-state-keys
  "The ONLY turn keys a locally-settled cancel keeps armed while it waits for the
   gateway ACK — the invisible send gate and the identity needed to match that
   ACK. Re-arming the full `active-turn-state-keys` instead kept `:progress`,
   `:turn-start-ms` and `:liveness-probed-at-ms` alive after the transcript rows
   were already dropped and the prompt handed back, so the composer read 'ready'
   while the rest of the frame still painted a live turn with a running clock."
  [:loading? :cancelling? :cancelling-at-ms :cancel-token :gateway-turn-id :live-turn-client-id])

(defn- session-running?
  [session]
  (or (= "running" (:status session))
      (= :running (:status session))
      (some? (:current_turn_id session))
      (some? (:current-turn-id session))))

(defn- park-live-trace
  "Stash the iterations the user already WATCHED onto the pending assistant
   placeholder, so clearing `:progress` never erases them.

   `:message-received` reads exactly this slot (`[:terminal-pending :trace]`) when a
   synthetic cancellation carries no trace of its own — the same slot the gateway
   terminal path fills. Without the park, the force-cancel (second Esc), the stuck-
   cancel self-heal and the cancel ACK all ran `clear-active-turn-state` FIRST, the
   worker's `:cancelled` result then looked like `no-work?`, and the whole visible
   run (every tool call already executed) was dropped from the transcript instead of
   being kept for the user to read."
  [db]
  (let
    [messages
     (vec (:messages db))

     idx
     (pending-assistant-index messages (:live-turn-client-id db))

     trace
     (not-empty (vec (get-in db [:progress :iterations])))]

    (if (and idx trace (empty? (get-in messages [idx :terminal-pending :trace])))
      (assoc db :messages (assoc-in messages [idx :terminal-pending :trace] trace))
      db)))

(defn- clear-active-turn-state
  "Settle this tab's in-flight turn state. ALWAYS parks the live trace first:
   `:progress` is the only home of the iterations painted live, so every path
   that stops a turn — the gateway terminal, a cancel, the not-running
   reconcile, the next turn starting — would otherwise drop everything the user
   watched the moment the turn ended (issue #61). Parking is idempotent and
   writes only onto a still-pending placeholder that has no trace yet."
  [db]
  (assoc (park-live-trace db)
    :loading? false
    :cancelling? false
    :progress nil
    :turn-start-ms nil
    :cancel-token nil
    :gateway-turn-id nil
    :live-turn-client-id nil
    :cancelling-at-ms nil
    :liveness-probed-at-ms nil))

(defn- reconcile-in-flight-state
  [next-db previous-db session]
  (if (session-running? session)
    (merge next-db (select-keys previous-db active-turn-state-keys))
    (clear-active-turn-state next-db)))

(reg-event-db :init-session
              (fn [db [_ session history workspace]]
                (let [user-history (history-user-texts history)]
                  (-> db
                      ensure-tabs
                      (assoc :session session
                             ;; The session's current workspace record (trunk or draft) — the
                             ;; single source the footer/header read to show trunk vs `<label>
                             ;; (DRAFT)`. `:root` is the cwd for trunk, the clone for a draft.
                             :workspace workspace
                             :title nil
                             ;; This tab is being REBOUND to another session, so the
                             ;; previous session's optimistic model pick must not survive:
                             ;; `session-model-pref` prefers this key over the gateway
                             ;; value, so a leftover pinned the footer chip (and the codex
                             ;; verbosity gating) to the OLD session's model until restart.
                             ;; nil = re-read the new session's real preference.
                             :session-model-pref nil
                             :messages (or history [])
                             :scroll scroll/follow
                             :input (input/empty-input)
                             :input-history user-history
                             :input-history-index nil
                             :input-history-draft nil
                             :submitted-input nil
                             :pastes {}
                             :paste-counter 0
                             :detail-expansions {})
                      (reconcile-in-flight-state db session)))))

(reg-event-fx
  :open-session-tab
  ;; Open `session` (with its `history` + pinned `workspace` record) in a TAB
  ;; WITHOUT disturbing the active tab. If a tab is already bound to this
  ;; session, focus it; otherwise mint a new tab and bind it. This is what
  ;; makes sessions run concurrently: opening/switching never resets the
  ;; running tab — its turn keeps streaming into its own `:tab-locals`.
  ;;
  ;; A PENDING pre-allocated tab (name-only, minted by
  ;; `:preallocate-project-tabs`) matches `existing` through its entry
  ;; `:session-id`: that is its FIRST real open, so the freshly loaded
  ;; session + transcript BIND into it in place (keeping its position and
  ;; title), it gains focus, and anything queued while it hydrated drains.
  (fn [db [_ session history workspace]]
    (let
      [sid
       (some-> session
               :id
               str)

       ;; Freeze the current tab (incl. any in-flight turn) into its locals
       ;; before we change focus, so its streaming worker keeps updating it.
       db
       (-> db
           ensure-tabs
           sync-active-tab)

       entries
       (vec (:tabs db))

       existing
       (when sid (some #(when (= sid (tab-session-id db (:id %))) %) entries))

       ;; W3 reopen seed: populate the F2 ctx cache immediately from the full
       ;; persisted ctx history so live + ARCHIVED tasks render the instant the
       ;; tab opens — for BOTH a freshly minted tab AND an already-open one.
       ;; Hoisted out of the `new tab` branch so a restored/restarted session
       ;; (which hits `existing` → activate-tab) no longer shows an empty F2
       ;; until its first turn end. Keyed by the raw session UUID (what screen.clj reads via
       ;; [:session :id]). One DB read; tolerate failure.
       ;; LATEST ctx only — the old merge-across-ALL-turn-snapshots seed
       ;; resurrected dropped plan steps into the TASKS section as if live.
       ;; History now has dedicated surfaces: :archived (GC'd entities,
       ;; rides the latest snapshot) and :timeline (plan generations from
       ;; the append-only task ledger, PLAN HISTORY section).
       ;; F2 panel no longer seeds tasks/facts/archived/timeline — gone.
       ctx-panel
       nil

       seed-ctx
       (fn [d]
         (cond-> d
           ctx-panel
           (assoc-in [:ctx-by-session (:id session)] ctx-panel)))]

      (cond (and existing (:pending? existing))
            (let
              [tab-id
               (:id existing)

               db'
               (-> db
                   (update :tabs
                           (fn [es]
                             (mapv (fn [e]
                                     (if (= (:id e) tab-id)
                                       (cond-> (dissoc e :pending?)
                                         workspace
                                         (assoc :workspace workspace)

                                         (:root workspace)
                                         (assoc :workspace/root (:root workspace)))
                                       e))
                                   es)))
                   (update-tab tab-id
                               (fn [w]
                                 (clear-active-turn-state (assoc w
                                                            :session session
                                                            :workspace workspace
                                                            :workspace/root (:root workspace)
                                                            :title nil
                                                            ;; Same reason as :init-session — a
                                                            ;; bound tab shows THIS session's model.
                                                            :session-model-pref nil
                                                            :messages (or history [])
                                                            :input-history (history-user-texts
                                                                             history)))))
                   (activate-tab tab-id)
                   seed-ctx)

               tab-view
               (if (= tab-id (current-tab-id db')) db' (get-in db' [:tab-locals tab-id]))]

              {:db db'
               :fx (cond-> []
                     (seq (:pending-sends tab-view))
                     (conj [:dispatch [:drain-pending tab-id]]))})
            existing
            ;; Already open — just focus that tab; its view state
            ;; (messages, scroll, in-flight turn) lives in :tab-locals.
            {:db (seed-ctx (activate-tab db (:id existing)))}
            :else (let
                    [n
                     (next-tab-number entries)

                     id
                     (keyword (str "tab-" n))

                     label
                     (or (some-> workspace
                                 :label
                                 not-empty)
                         untitled-session-label)

                     entry
                     (cond-> {:id id :label label :active? true}
                       workspace
                       (assoc :workspace workspace)

                       (:root workspace)
                       (assoc :workspace/root (:root workspace)))

                     db'
                     (-> db
                         (assoc :tabs (insert-tab-grouped (mapv #(dissoc % :active?) entries) entry)
                                :active-tab-id id)
                         ;; Make the new tab the live root state (a fresh session view);
                         ;; finalize-db snapshots this back into the tab's locals.
                         (merge (empty-tab-state))
                         (assoc :session session
                                :workspace workspace
                                :workspace/root (:root workspace)
                                :title nil
                                :messages (or history [])
                                :input-history (history-user-texts history)))]

                    {:db (seed-ctx db')})))))

(reg-event-db :open-building-tab
              ;; Optimistic new tab for a session whose cold env/runtime is still being
              ;; built on a background worker (chat/make-session-async's `:building`
              ;; branch), so the input thread NEVER blocks on the 3-4s build. Mints a fresh
              ;; active tab with NO session bound yet and `:loading? true`: any Enter while
              ;; it builds queues into `:pending-sends` (`:send-message` enqueues when
              ;; `:loading?`) instead of being lost or sent to a dead session. `build-id`
              ;; tags the tab entry so the async callback (`:bind-built-session`) can find
              ;; it again across intervening tab churn.
              (fn [db [_ build-id]]
                (let
                  [db
                   (-> db
                       ensure-tabs
                       sync-active-tab)

                   entries
                   (vec (:tabs db))

                   n
                   (next-tab-number entries)

                   id
                   (keyword (str "tab-" n))

                   entry
                   {:id id :label starting-session-label :active? true :build-id build-id}]

                  (-> db
                      (assoc :tabs (conj (mapv #(dissoc % :active?) entries) entry)
                             :active-tab-id id)
                      (merge (empty-tab-state))
                      (assoc :title nil
                             :loading? true
                             :progress {:iterations []}
                             :turn-start-ms (System/currentTimeMillis))))))

(reg-event-fx
  :bind-built-session
  ;; The background build for an optimistic `:open-building-tab` finished. Find
  ;; the tab tagged with `build-id`, bind the freshly built `session` (+ its
  ;; `history`/`workspace`) into that tab's state, clear the loading flag, and
  ;; drain anything the user queued while it built. If the tab was closed in the
  ;; meantime, close the now-orphan session instead of leaking it.
  (fn [db [_ build-id session history workspace]]
    (let
      [db
       (-> db
           ensure-tabs
           sync-active-tab)

       entries
       (vec (:tabs db))

       entry
       (some #(when (= build-id (:build-id %)) %) entries)]

      (if-not entry
        {:db db :fx [[:gateway-close-session (:id session)]]}
        (let
          [tab-id
           (:id entry)

           entries'
           (mapv (fn [e]
                   (if (= (:id e) tab-id)
                     (cond->
                       (-> e
                           (dissoc :build-id)
                           (assoc :label (or (some-> workspace
                                                     :label
                                                     not-empty)
                                             (when (not= starting-session-label (:label e))
                                               (not-empty (:label e)))
                                             untitled-session-label)))
                       workspace
                       (assoc :workspace workspace)

                       (:root workspace)
                       (assoc :workspace/root (:root workspace)))
                     e))
                 entries)

           ;; The tab was minted at the END (no workspace known
           ;; yet). Now that its project root is bound, RELOCATE
           ;; it next to its group so the strip stays grouped.
           entries'
           (let
             [entry'
              (some #(when (= (:id %) tab-id) %) entries')

              without
              (vec (remove #(= (:id %) tab-id) entries'))]

             (insert-tab-grouped without entry'))

           db
           (assoc db :tabs entries')

           db
           (update-tab db
                       tab-id
                       (fn [w]
                         (clear-active-turn-state (assoc w
                                                    :session session
                                                    :workspace workspace
                                                    :workspace/root (:root workspace)
                                                    :messages (or history [])
                                                    :input-history (history-user-texts history)
                                                    :title nil))))

           tab-view
           (if (= tab-id (current-tab-id db)) db (get-in db [:tab-locals tab-id]))

           pending?
           (seq (:pending-sends tab-view))]

          {:db db
           :fx (cond-> []
                 pending?
                 (conj [:dispatch [:drain-pending tab-id]]))})))))

(reg-event-db :preallocate-project-tabs
              ;; Pre-allocate NAME-ONLY tabs for `specs` — [{:session-id .. :label ..
              ;; :root ..} …], the launch project's member sessions in tab order —
              ;; WITHOUT loading any transcript and WITHOUT moving focus. Each minted
              ;; entry is `:pending? true` and carries its `:session-id`; the
              ;; transcript loads lazily on FIRST focus (screen.clj's
              ;; hydrate-pending-tab! resumes it and `:open-session-tab` binds it).
              ;; Sessions already open in a tab are skipped. NOT capped by
              ;; `max-tabs`: a project's member list IS the tab set that was open
              ;; last time (closing a tab unassigns the session from the project,
              ;; see `:unassign-session-project`), so capping here silently DROPPED
              ;; every tab past the 8th on relaunch — and the follow-up
              ;; `persist-tabs!` then rewrote `project_position` from the truncated
              ;; strip. `max-tabs` guards MANUAL tab creation only (`:create-tab`).
              ;; Locals seed with an empty view so an early switch paints a
              ;; blank transcript instead of ghosting the previous tab.
              (fn [db [_ specs]]
                (let
                  [db (-> db
                          ensure-tabs
                          sync-active-tab)]
                  (reduce
                    (fn [db {:keys [session-id label root]}]
                      (let
                        [sid (some-> session-id
                                     str)
                         entries (vec (:tabs db))
                         open? (when sid (some #(= sid (tab-session-id db (:id %))) entries))]

                        (if (or (nil? sid) open?)
                          db
                          (let
                            [id (keyword (str "tab-" (next-tab-number entries)))
                             entry (cond->
                                     {:id id
                                      :label (or (not-empty label) untitled-session-label)
                                      :session-id sid
                                      :pending? true}
                                     root
                                     (assoc :workspace/root root))]

                            (-> db
                                (assoc :tabs (insert-tab-grouped entries entry))
                                (assoc-in [:tab-locals id] (empty-tab-state)))))))
                    db
                    specs))))

(reg-event-db :order-project-tabs
              ;; Re-seat the tabs bound to a PROJECT so the strip reads in the
              ;; gateway's stored `project_position` order (`session-ids`). Only the
              ;; slots those tabs already occupy are rewritten, so tabs of another
              ;; project — and unbound/building ones — never move. Focus follows the
              ;; tab ENTRY, not its index, so re-seating never switches session.
              ;;
              ;; Startup mints the eagerly-resumed tab BEFORE the member list is
              ;; known, so without this the strip is "startup tab first, rest in
              ;; stored order" and the follow-up persist rewrites `project_position`
              ;; to match — rotating the tab order by one on EVERY relaunch.
              (fn [db [_ session-ids]]
                (let
                  [entries
                   (vec (:tabs db))

                   rank
                   (into {}
                         (map-indexed (fn [i sid]
                                        [(str sid) i]))
                         (distinct (map str session-ids)))

                   sid-of
                   (fn [entry]
                     (some-> (tab-session-id db (:id entry))
                             str))

                   member?
                   (fn [entry]
                     (contains? rank (sid-of entry)))

                   slots
                   (vec (keep-indexed (fn [i entry]
                                        (when (member? entry) i))
                                      entries))

                   ordered
                   (vec (sort-by (comp rank sid-of) (filterv member? entries)))]

                  (if (< (count slots) 2)
                    db
                    (assoc db
                      :tabs (reduce (fn [es [i entry]]
                                      (assoc es i entry))
                                    entries
                                    (map vector slots ordered)))))))

(reg-event-db :mark-tab-loading
              ;; Flip a tab's `:loading?` while its PENDING transcript hydrates on a
              ;; worker (screen.clj's hydrate-pending-tab!): the spinner shows and any
              ;; Enter queues into `:pending-sends` (`:send-message` enqueues when
              ;; `:loading?`) instead of hitting a nil session. Mirrors
              ;; `:open-building-tab`'s loading shape; the `:open-session-tab` pending
              ;; bind (or `:tab-hydration-failed`) clears it.
              (fn [db [_ tab-id on?]]
                (update-tab db
                            tab-id
                            (fn [w]
                              (if on?
                                (assoc w
                                  :loading? true
                                  :progress (or (:progress w) {:iterations []})
                                  :turn-start-ms (or (:turn-start-ms w) (System/currentTimeMillis)))
                                (clear-active-turn-state w))))))

(reg-event-db :tab-hydration-failed
              ;; The lazy transcript load for a PENDING tab failed (session deleted
              ;; elsewhere / gateway hiccup). Drop the pending marker + session binding
              ;; so the input loop doesn't retry forever, leaving a plain empty tab the
              ;; user can close.
              (fn [db [_ tab-id]]
                (-> db
                    (update :tabs
                            (fn [es]
                              (mapv #(if (= (:id %) tab-id) (dissoc % :pending? :session-id) %)
                                    es)))
                    (update-tab tab-id clear-active-turn-state))))

(reg-event-db :title-loading
              ;; Host auto-title generation started (true) or ended (false). Drives the
              ;; header spinner on the active tab's title. `:set-title` also clears it so
              ;; the spinner stops the instant a real title lands.
              (fn [db [_ loading?]]
                (assoc db :title-loading? (boolean loading?))))

(reg-event-db :toggle-help
              ;; Flip the Ctrl+H / F1 keyboard-shortcut overlay. Pure render flag —
              ;; `components/help-overlay!` paints it when `:help-open?` is set.
              (fn [db _]
                (-> db
                    (update :help-open? not)
                    (assoc :tasks-open? false
                           :help-scroll 0))))

(reg-event-db :toggle-tasks
              ;; F2 context panel REMOVED — this is an inert no-op kept only so the
              ;; (now-unreachable) input-dispatch sites in screen.clj don't throw on an
              ;; unregistered event. `:tasks-open?` is never set, so nothing paints.
              (fn [db _]
                db))

(reg-event-db :close-overlays
              ;; Force every render-flag overlay shut. Dispatched before opening a
              ;; modal dialog (e.g. F4 resources) so only ONE dialog is ever on
              ;; screen — the F2 context / F1 help panels can't bleed around the
              ;; modal box.
              (fn [db _]
                (assoc db
                  :help-open? false
                  :tasks-open? false
                  :help-scroll 0)))

(reg-event-db :ctx-scroll-by
              ;; Scroll the F2 context panel by `delta` rows, clamped to [0, the last
              ;; paint's :ctx-scroll-max]. Callers bump :render-version separately so the
              ;; otherwise-still overlay repaints.
              (fn [db [_ delta]]
                (let
                  [maxs
                   (long (or (:ctx-scroll-max db) 0))

                   cur
                   (long (or (:ctx-scroll db) 0))]

                  (assoc db :ctx-scroll (max 0 (min maxs (+ cur (long delta))))))))

(reg-event-db :toggle-fact-files
              ;; Fold/unfold the file list under a fact's `⛁ N files` meta row in
              ;; the F2 context panel. `:expanded-facts` is a set of fact keys
              ;; (as strings); clicking the glyph flips membership. Callers bump
              ;; :render-version separately so the otherwise-still overlay repaints.
              (fn [db [_ fact-key]]
                (let
                  [k
                   (str fact-key)

                   cur
                   (set (:expanded-facts db))]

                  (assoc db :expanded-facts (if (contains? cur k) (disj cur k) (conj cur k))))))

(reg-event-db :set-ctx-scroll-max
              ;; Record the F2 panel's max scroll offset (computed during paint) so the
              ;; scroll event can clamp. Pure assoc — does NOT bump render-version.
              (fn [db [_ maxs]]
                (assoc db :ctx-scroll-max (long (or maxs 0)))))

(reg-event-db :help-scroll-by
              ;; Scroll the F1 help overlay by `delta` rows, clamped to [0, the last
              ;; paint's :help-scroll-max]. Mirrors :ctx-scroll-by; callers bump
              ;; :render-version separately so the otherwise-still overlay repaints.
              (fn [db [_ delta]]
                (let
                  [maxs
                   (long (or (:help-scroll-max db) 0))

                   cur
                   (long (or (:help-scroll db) 0))]

                  (assoc db :help-scroll (max 0 (min maxs (+ cur (long delta))))))))

(reg-event-db :set-help-scroll-max
              ;; Record the F1 help overlay's max scroll offset (computed during paint)
              ;; so the scroll event can clamp. Pure assoc — does NOT bump render-version.
              (fn [db [_ maxs]]
                (assoc db :help-scroll-max (long (or maxs 0)))))

(reg-event-db :set-ctx-panel
              ;; F2 context panel REMOVED — inert no-op (kept so the turn-runner's
              ;; dispatch sites don't throw on an unregistered event). No cache.
              (fn [db _]
                db))

(defn tab-id-for-session
  "Resolve a session-id string to its tab id. The active tab's session lives
   at the db root; background tabs' sessions live in `:tab-locals`."
  [db session-id]
  (when-let
    [sid (some-> session-id
                 str)]
    (let [active-id (current-tab-id db)]
      (or (when (= sid
                   (some-> db
                           :session
                           :id
                           str))
            active-id)
          (some #(when (= sid (tab-session-id db (:id %))) (:id %)) (:tabs db))))))

(reg-event-db :set-title
              ;; `title` lands on a specific tab. With no `arg` we target the active tab
              ;; (legacy callers). With a session-id `arg` — what the title listener now
              ;; passes for EVERY session, focused or not — we resolve the owning tab and
              ;; relabel it directly, so a background session's title updates live without
              ;; the user opening the tab. An unresolvable arg is a no-op.
              (fn [db [_ title arg]]
                (let
                  [active-id
                   (current-tab-id db)

                   target-id
                   (if arg (tab-id-for-session db arg) active-id)]

                  (cond-> db
                    (= target-id active-id)
                    (assoc :title
                      title :title-loading?
                      false)

                    (and target-id (not= target-id active-id))
                    (assoc-in [:tab-locals target-id :title] title)

                    target-id
                    (update :tabs
                            (fn [entries]
                              (mapv (fn [entry]
                                      (cond-> entry
                                        (= (:id entry) target-id)
                                        (assoc :label (label-from-title title (:label entry)))))
                                    entries)))))))

(reg-event-db :update-input
              (fn [db [_ new-input]]
                (let [text (input/input->text new-input)]
                  (cond-> (assoc db :input new-input)
                    (not (str/starts-with? (str/triml text) "/"))
                    (assoc :slash-command-hidden? false)))))

(reg-event-db :hide-slash-command-suggestions
              (fn [db _]
                (assoc db :slash-command-hidden? true)))

(reg-event-db :move-slash-command-selection
              (fn [db [_ delta total]]
                (assoc db
                  :slash-command-index (slash/move-index (:slash-command-index db) delta total))))

(defn- text->input-state
  [text]
  (let
    [lines
     (vec (or (seq (str/split (or text "") #"\n" -1)) [""]))

     crow
     (dec (count lines))

     ccol
     (count (nth lines crow))]

    {:lines lines :crow crow :ccol ccol}))

(defn- append-input-text
  [current text]
  (let
    [current-text
     (input/input->text current)

     next-text
     (or text "")]

    (cond (str/blank? current-text) next-text
          (str/blank? next-text) current-text
          :else (str current-text "\n" next-text))))

(defn- apply-external-input
  [workspace op text]
  (let
    [current
     (:input workspace)

     next
     (case op
       :replace
       (text->input-state text)

       :append
       (text->input-state (append-input-text current text))

       :insert
       (input/paste-text current (or text ""))

       current)]

    (assoc workspace
      :input next
      :input-history-index nil
      :input-history-draft nil
      :slash-command-index 0
      :slash-command-hidden? false)))

(reg-event-db :external-input
              (fn [db [_ op text workspace-id]]
                (update-tab db workspace-id #(apply-external-input % op text))))

(reg-event-db
  :channel-status-set
  (fn [db [_ id status]]
    (assoc-in db [:channel-status id] (assoc status :updated-at-ms (System/currentTimeMillis)))))

(reg-event-db :channel-status-clear
              (fn [db [_ id]]
                (update db :channel-status dissoc id)))

(reg-event-db :channel-status-clear-if-until
              (fn [db [_ id until]]
                (if (= until (get-in db [:channel-status id :until]))
                  (update db :channel-status dissoc id)
                  db)))

(reg-event-db :human-input-open
              (fn [db [_ form]]
                (if (:human-input db)
                  (update db :human-input-queue (fnil conj []) form)
                  (assoc db :human-input form))))

(reg-event-db :human-input-form
              (fn [db [_ form]]
                (assoc db :human-input form)))

(reg-event-db :human-input-close
              (fn [db [_ request-id]]
                (if (or (nil? request-id) (= request-id (get-in db [:human-input :request :id])))
                  (let [queue (vec (:human-input-queue db))]
                    (assoc db
                      :human-input (first queue)
                      :human-input-queue (vec (rest queue))))
                  (assoc db
                    :human-input-queue (vec (remove #(= request-id (get-in % [:request :id]))
                                              (:human-input-queue db)))))))

(defn- drop-pending-turn-messages
  "Remove the transient user + assistant placeholder pair created by
   `:send-message`. Used only when a submitted prompt is cancelled and
   restored to the editor instead of becoming a transcript turn."
  [messages]
  (let
    [messages
     (vec (or messages []))

     n
     (count messages)]

    (cond (and (<= 2 n)
               (= :assistant (:role (peek messages)))
               (= :user (:role (nth messages (- n 2)))))
          (subvec messages 0 (- n 2))
          :else messages)))

(defn- restore-submitted-input
  "Drop the pending turn pair and restore the submitted prompt only when the
   editor is still pristine. Keystrokes entered while cancellation settles are
   a newer draft and must never be overwritten by the cancellation ACK."
  [db {:keys [text pastes paste-counter]}]
  (let
    [visible-text
     (input/expand-paste-placeholders text pastes)

     restore-editor?
     (input/input-empty? (:input db))]

    (cond->
      (-> db
          clear-active-turn-state
          (assoc :messages (drop-pending-turn-messages (:messages db))
                 :scroll scroll/follow)
          (update :input-history
                  (fn [xs]
                    (let [xs (vec (or xs []))]
                      (if (= visible-text (peek xs)) (pop xs) xs))))
          (dissoc :turn-start-ms :submitted-input))
      restore-editor?
      (assoc :input
        (text->input-state text) :input-history-index
        nil :input-history-draft
        nil :slash-command-index
        0 :slash-command-hidden?
        false :pastes
        (or pastes {}) :paste-counter
        (or paste-counter 0)))))

(defn- restore-editor-only
  "Restore the submitted prompt after a cancellation with visible work, unless
   the user already started a newer draft while the cancellation settled."
  [db {:keys [text pastes paste-counter]}]
  (cond-> (dissoc db :submitted-input)
    (input/input-empty? (:input db))
    (assoc :input
      (text->input-state text) :input-history-index
      nil :input-history-draft
      nil :slash-command-index
      0 :slash-command-hidden?
      false :pastes
      (or pastes {}) :paste-counter
      (or paste-counter 0))))

(defn- settle-cancelled-turn
  "Settle a cancel confirmed by the daemon ACK (or given up on by the self-heal)
   while the local worker's terminal result has not landed yet.

   Clearing the turn WITHOUT touching the editor left one cancel half-settled:
   sends flowed again, yet the composer stayed empty, the prompt sat unreachable
   in `:submitted-input`, and the transcript kept a pending assistant placeholder
   that no later event would resolve. Settle exactly like the worker's terminal —
   a stray Esc that ran nothing drops its placeholder pair and hands the prompt
   back, a cancel with visible work keeps the bubble and only refills the editor —
   so whichever half arrives first leaves the same frame."
  [db]
  (let
    [submitted
     (:submitted-input db)

     messages
     (vec (:messages db))

     idx
     (pending-assistant-index messages (:live-turn-client-id db))

     trace
     (or (not-empty (vec (get-in db [:progress :iterations])))
         (when idx (not-empty (vec (get-in messages [idx :terminal-pending :trace])))))]

    (cond (nil? submitted) (clear-active-turn-state db)
          (empty? trace) (restore-submitted-input db submitted)
          :else (-> db
                    clear-active-turn-state
                    (restore-editor-only submitted)))))

(reg-event-fx
  :history-up
  (fn [db _]
    (let
      [history
       (vec (or (:input-history db) []))

       cur-idx
       (:input-history-index db)

       draft
       (:input-history-draft db)

       input-text
       (input/input->text (:input db))

       pending
       (vec (or (:pending-sends db) []))]

      (cond
        ;; Empty box + something queued → pull the most recently
        ;; queued submission back for editing, popping it off the
        ;; queue (its paste snapshot rides along). Also drops the real
        ;; gateway queued record so it never auto-drains behind our back.
        (and (nil? cur-idx) (str/blank? input-text) (seq pending))
        (let
          [entry
           (peek pending)

           tid
           (:turn-id entry)

           sid
           (get-in db [:session :id])]

          {:db (assoc db
                 :input (text->input-state (:text entry))
                 :pending-sends (pop pending)
                 :pastes (or (:pastes entry) {})
                 :paste-counter (or (:paste-counter entry) 0)
                 :input-history-index nil
                 :input-history-draft nil
                 :slash-command-index 0
                 :slash-command-hidden? false)
           :fx (cond-> []
                 (and sid tid)
                 (conj [:gateway-delete-queued sid tid (current-tab-id db) entry]))})
        (empty? history) {:db db}
        :else (let
                [new-idx
                 (if (nil? cur-idx) (dec (count history)) (max 0 (dec (long cur-idx))))

                 draft
                 (if (nil? cur-idx) input-text draft)]

                {:db (assoc db
                       :input-history-index new-idx
                       :input-history-draft draft
                       :input (text->input-state (nth history new-idx)))})))))

(reg-event-db :history-down
              (fn [db _]
                (let
                  [history
                   (vec (or (:input-history db) []))

                   cur-idx
                   (:input-history-index db)

                   draft
                   (:input-history-draft db)]

                  (cond (nil? cur-idx) db
                        (< (long cur-idx) (dec (count history)))
                        (let [new-idx (inc (long cur-idx))]
                          (assoc db
                            :input-history-index new-idx
                            :input (text->input-state (nth history new-idx))))
                        :else (assoc db
                                :input-history-index nil
                                :input-history-draft nil
                                :input (text->input-state (or draft "")))))))

(reg-event-db :reset-input
              (fn [db _]
                (assoc db
                  :input (input/empty-input)
                  :input-history-index nil
                  :input-history-draft nil
                  :slash-command-index 0
                  :slash-command-hidden? false
                  ;; A new empty input has no placeholder tokens, so the paste
                  ;; registry is dead state. Clearing it here keeps memory
                  ;; bounded across long sessions - every send + every history
                  ;; reset drops orphans.
                  :pastes {}
                  :paste-counter 0)))

(reg-event-db :add-paste
              ;; Stashes a clipboard payload in the registry, returns the new
              ;; id (Integer) via a side-channel atom that the screen loop reads
              ;; right after dispatch - see `:paste-counter` increment below.
              (fn [db [_ content image]]
                (let [next-id (inc (long (or (:paste-counter db) 0)))]
                  (-> db
                      (assoc :paste-counter next-id)
                      (assoc-in [:pastes next-id]
                                (cond-> {:id next-id :content content}
                                  image
                                  (assoc :image image)))))))

(reg-event-db :remove-paste
              ;; Drop a single paste entry by id. Used when the user backspaces
              ;; over the closing `]` of a placeholder - the screen loop deletes
              ;; the token from the input buffer AND drops the matching content
              ;; here so memory tracks what the user can still see.
              (fn [db [_ id]]
                (update db :pastes dissoc id)))

;; -- Messages-area scroll ---------------------------------------------------
;;
;; All scroll state is ONE workspace-local tagged value, `:scroll` (see
;; `scroll.clj` for the variant + transition algebra). These events are
;; thin wrappers: each REPLACES `:scroll` with the next variant, so nothing
;; can dangle across frames. The render loop reads it back via
;; `scroll/layout-offset` (what row to paint) and drives the animation with
;; `:ease-scroll`.
;; Scroll-transition diagnostic (`:debug`, silent under the default
;; `:info` file handler — flip min-level to investigate the "jump to
;; bottom fighting" symptom). `scroll-pre!` snapshots the PRE-transition
;; `:scroll`; `log-scroll!` emits the transition and flags a `:at→:follow`
;; re-arm — the prime suspect for the bounce: a scroll-down landing in the
;; slack band re-arms FOLLOW and snaps back to bottom, then the next ease
;; pushes down again. Grep the log for `scroll-transition` / `rearm? true`.
(defn- scroll-snapshot [sc] {:mode (str (:mode sc)) :offset (long (or (:offset sc) 0))})

(defn- scroll-pre! [db] (scroll-snapshot (:scroll db)))

(defn- log-scroll!
  [label pre post extra]
  (let [rearmed? (and (= "at" (:mode pre)) (= "follow" (:mode post)))]
    (tel/log! {:level :debug
               :id ::scroll-transition
               :data (merge {:event label :pre pre :post post :rearm? rearmed?} extra)
               :msg (str "scroll-transition "
                         label
                         " pre="
                         (:mode pre)
                         "@"
                         (:offset pre)
                         " post="
                         (:mode post)
                         "@"
                         (:offset post)
                         (when rearmed? " [re-armed FOLLOW]"))})))

(reg-event-db :set-scroll
              ;; Search jump / `:scroll-to-message` resolution: snap-park at an exact
              ;; row (already clamped by the painter). No ease - the jump is the point.
              (fn [db [_ offset]]
                (let
                  [pre
                   (scroll-pre! db)

                   sc
                   (scroll/parked offset)]

                  (log-scroll! :set-scroll pre (scroll-snapshot sc) {:offset offset})
                  (assoc db :scroll sc))))

(reg-event-db :scroll-to-bottom
              ;; Emacs C-l recenter: drop back to FOLLOW (stick to the newest
              ;; content). The repaint is the caller's `:bump-render-version`.
              (fn [db _]
                (let
                  [pre
                   (scroll-pre! db)

                   sc
                   scroll/follow]

                  (log-scroll! :scroll-to-bottom pre (scroll-snapshot sc) {})
                  (assoc db :scroll sc))))

(defn- older-history-request
  "Effect vector for the lazy scroll-up loader, or nil.

   The session opened on its NEWEST turns only (`chat/resume-tail-turns`), so
   the top of `:messages` is not the top of the session. Once the viewport comes
   within one screen of that top and the cursor says older turns exist, ask for
   ONE page. `:history-loading?` (set by the caller) is the in-flight latch, so a
   fast wheel-up cannot queue a dozen identical fetches."
  [db sc ^long max-s ^long inner-h]
  (let
    [session
     (:session db)

     cursor
     (:history-cursor session)]

    (when (and (:id session)
               (:has-more cursor)
               (not (:history-loading? session))
               (<= (scroll/desired sc max-s) (max 1 inner-h)))
      [:load-older-history (:id session) (long (or (:offset cursor) 0))])))

(reg-event-fx
  :scroll-to-top
  ;; Emacs M-< (beginning-of-buffer): park at the very top. The layout
  ;; clamps the offset, so row 0 is the first LOADED message — which, in a
  ;; lazily paged session, still has older turns above it, hence the loader
  ;; kick.
  (fn [db _]
    (let
      [pre
       (scroll-pre! db)

       sc
       (scroll/parked 0)

       load
       (older-history-request db sc 0 1)

       db'
       (assoc db :scroll sc)]

      (log-scroll! :scroll-to-top pre (scroll-snapshot sc) {})
      (if load {:db (assoc-in db' [:session :history-loading?] true) :fx [load]} {:db db'}))))

(reg-event-db :reanchor-scroll
              ;; Scroll-anchoring write-back from the render thread. `anchored` is the
              ;; corrected absolute on-screen row; `delta` is how far content ABOVE the
              ;; anchor changed height as off-screen estimates resolved. Shift the
              ;; concrete fields so the anchored message stays visually put (no lurch).
              ;; FOLLOW-at-bottom feeds nil to the layout and never dispatches this.
              (fn [db [_ anchored delta]]
                (assoc db
                  :scroll (scroll/reanchor (:scroll db) (long anchored) (long (or delta 0))))))

(reg-event-db :older-history-loading
              ;; Latch for the lazy scroll-up loader: ONE in-flight fetch per tab, so
              ;; a fast wheel-up cannot queue a dozen identical page requests. Scoped
              ;; by session id — a tab switch mid-fetch must not flip the wrong tab's
              ;; flag.
              (fn [db [_ session-id loading?]]
                (if (= (str session-id) (str (get-in db [:session :id])))
                  (assoc-in db [:session :history-loading?] (boolean loading?))
                  db)))

(reg-event-db :prepend-history
              ;; An OLDER page of the transcript landed (the session opened on its
              ;; newest turns only). Splice it in ABOVE the current messages and shift
              ;; the scroll by the page's MEASURED height so the bubble the user is
              ;; reading stays exactly where it is — `virtual/layout`'s own anchoring
              ;; can't help here, it deliberately skips a frame where the message
              ;; count changed.
              ;;
              ;; Dropped when the tab has moved on to another session: the fetch is
              ;; async and its result is only ever valid for the session it was for.
              (fn [db [_ session-id page shift]]
                (if-not (= (str session-id) (str (get-in db [:session :id])))
                  db
                  (let [older (vec (:messages page))]
                    (-> db
                        (update :messages #(into older (or % [])))
                        ;; Older prompts belong at the FRONT of the up-arrow ring:
                        ;; the ring is oldest-first and `:history-up` walks back from
                        ;; the end.
                        (update :input-history #(into (vec (history-user-texts older)) (or % [])))
                        (update :session assoc
                                :history-loading? false
                                :history-cursor {:offset (:offset page)
                                                 :total (:total page)
                                                 :has-more (boolean (:has-more page))})
                        (update :scroll scroll/shift-prepended (long (or shift 0))))))))

(reg-event-db :ease-scroll
              ;; Render-loop pulse: advance the on-screen position one ease-out step
              ;; toward where the current intent WANTS it (bottom in FOLLOW, the parked
              ;; offset in AT). This single event subsumes the old tick-scroll-anim +
              ;; follow-bottom-animated + follow-bottom-if-near trio: in FOLLOW the
              ;; desired row simply IS the growing bottom, so streamed content eases in
              ;; for free, and a user parked above (mode :at) is never yanked because
              ;; their desired row is fixed.
              (fn [db [_ total-h inner-h]]
                (let
                  [max-s
                   (max 0 (- (long total-h) (long inner-h)))

                   pre
                   (scroll-pre! db)

                   cur
                   (:scroll db)

                   sc
                   (scroll/ease cur max-s)

                   post
                   (scroll-snapshot sc)]

                  ;; `:ease-scroll` fires ~per-render-frame; only log when the
                  ;; committed mode/offset actually changed (else it spams
                  ;; settled zeros every frame).
                  (when (or (not= (:mode pre) (:mode post)) (not= (:offset pre) (:offset post)))
                    (log-scroll! :ease-scroll pre post {:max-s max-s}))
                  ;; Preserve `:scroll` IDENTITY when the ease produced an EQUAL
                  ;; value. `scroll/ease` re-`assoc`s `:pos` every tick (a freshly
                  ;; boxed row), so its result is `=` but never `identical?` to the
                  ;; current scroll even when the view sits settled at the follow
                  ;; bottom. `:ease-scroll` pulses on every ~80ms streaming tick, so
                  ;; that churn silently rewrote app-db's `:scroll` to a new object
                  ;; each tick. The render loop's fast-path predicates
                  ;; (`live-progress-only-change?` / `scroll-only-change?`) diff db
                  ;; by `identical?` per key, so a churning-but-unchanged `:scroll`
                  ;; demoted EVERY progress-driven repaint to a FULL frame
                  ;; (100-280ms on a long transcript) instead of the cheap
                  ;; partial-live band. Return db untouched when nothing moved so the
                  ;; identity — and the fast path — survive.
                  (if (= sc cur) db (assoc db :scroll sc)))))

;; ── In-session search ──────────────────────────────────────────────────────
;; The render side already exists (paint-search-hits! highlights bubbles whose
;; index is in `:search :hits`, and reads `:active?`/`:query`). These events are
;; the missing CONTROLLER: compute hits over the message buffer and park scroll
;; on the current match. `:hits` is a vec of MESSAGE INDICES (the shape the
;; painter consumes); navigation cycles `:index` over it and snaps the view to
;; that message's row via the same `scroll/parked` jump as `:set-scroll`.
(defn- search-hits
  "Search `messages` for `query`. Returns {:hits [msg-indices] :total n}:
   :hits are indices of messages containing the query (the shape the painter
   and scroll consume), :total counts every OCCURRENCE across all messages
   (what the user actually sees highlighted). `case?` true = case-sensitive;
   default is case-insensitive. Blank query -> no hits."
  [messages query case?]
  (let
    [needle
     (if case? (str query) (clojure.string/lower-case (str query)))

     n-len
     (count needle)]

    (if (clojure.string/blank? needle)
      {:hits [] :total 0}
      (let
        [counts (keep-indexed (fn [i m]
                                (let
                                  [hay (cond-> (str (:text m))
                                         (not case?)
                                         clojure.string/lower-case)
                                   c (loop
                                       [from 0
                                        c 0]

                                       (let [pos (.indexOf ^String hay ^String needle (int from))]
                                         (if (neg? pos) c (recur (+ pos n-len) (inc c)))))]

                                  (when (pos? (long c)) [i c])))
                              messages)]
        {:hits (mapv first counts) :total (long (reduce + 0 (map second counts)))}))))

(defn- scroll-to-hit
  "Park scroll at the row of hit `index` (mod into `hits`). Painter clamps the
   row, so no max-scroll math is needed here. No-op when there are no hits."
  [db hits index]
  (if (seq hits)
    (let
      [msg-idx
       (nth hits (mod (long index) (count hits)))

       offsets
       (vec (:offsets (:layout db)))

       row
       (long (or (get offsets msg-idx) 0))]

      (assoc db :scroll (scroll/parked row)))
    db))

(reg-event-db :search-open
              ;; Activate the in-session find bar (empty query). One overlay at a
              ;; time — shut the F2/help panels.
              (fn [db _]
                (assoc db
                  :search {:active? true :query "" :hits [] :index 0 :case? false :total 0}
                  :help-open? false
                  :tasks-open? false)))

(reg-event-db
  :search-set-query
  ;; Incremental: recompute hits for the full new query, reset to the
  ;; first match, and snap to it.
  (fn [db [_ query]]
    (let
      [case?
       (boolean (get-in db [:search :case?]))

       {:keys [hits total]}
       (search-hits (:messages db) query case?)]

      (-> db
          (assoc :search
                 {:active? true :query (str query) :hits hits :index 0 :case? case? :total total})
          (scroll-to-hit hits 0)))))

(reg-event-db :search-next
              (fn [db _]
                (let [{:keys [hits index]} (:search db)]
                  (if (seq hits)
                    (let [i (mod (inc (long (or index 0))) (count hits))]
                      (-> db
                          (assoc-in [:search :index] i)
                          (scroll-to-hit hits i)))
                    db))))

(reg-event-db :search-prev
              (fn [db _]
                (let [{:keys [hits index]} (:search db)]
                  (if (seq hits)
                    (let [i (mod (dec (long (or index 0))) (count hits))]
                      (-> db
                          (assoc-in [:search :index] i)
                          (scroll-to-hit hits i)))
                    db))))

(reg-event-db
  :search-toggle-case
  ;; Flip case sensitivity (Alt+C / the find-bar Aa chip), recompute
  ;; hits for the current query, and snap back to the first match.
  (fn [db _]
    (let
      [{:keys [query case?]}
       (:search db)

       case?
       (not case?)

       {:keys [hits total]}
       (search-hits (:messages db) query case?)]

      (-> db
          (assoc :search
                 {:active? true :query (str query) :hits hits :index 0 :case? case? :total total})
          (scroll-to-hit hits 0)))))

(reg-event-db :search-clear
              (fn [db _]
                (assoc db
                  :search {:active? false :query "" :hits [] :index 0 :case? false :total 0})))

(reg-event-db :scroll-to-message
              ;; In-session search lands here after the user picks a hit. The painter doesn't
              ;; get told an exact :messages-scroll Y value
              ;; (which it would need to compute heights for); instead it sees
              ;; `:scroll-to-message-pending` and re-resolves the scroll target
              ;; on the next frame, then clears the pending field. One-shot.
              (fn [db [_ msg-idx]]
                (cond-> db
                  (and (integer? msg-idx) (>= (long msg-idx) 0))
                  ;; The resolution dispatches `:set-scroll`, which parks (mode :at) on
                  ;; the hit. Parking IS the scroll-up intent now, so streaming follow
                  ;; hands off automatically until the user scrolls back to the bottom.
                  (assoc :scroll-to-message-pending msg-idx))))

(reg-event-db :scroll-to-message-resolved
              ;; Painter calls this after consuming `:scroll-to-message-pending`
              ;; so the same hit doesn't re-scroll on every redraw.
              (fn [db _]
                (dissoc db :scroll-to-message-pending)))

(reg-event-fx
  :scroll-up
  ;; Wheel / arrow / PageUp: park `amount` rows above the current row and
  ;; ease there. Scrolling up is always a deliberate read-history intent
  ;; (mode :at), so the streaming follow hands off automatically — and it
  ;; is the ONE gesture that can run out of loaded history, so it also
  ;; drives the older-page fetch.
  (fn [db [_ amount total-h inner-h]]
    (let
      [max-s
       (max 0 (- (long total-h) (long inner-h)))

       pre
       (scroll-pre! db)

       sc
       (scroll/up (:scroll db) (long amount) max-s)

       load
       (older-history-request db sc max-s (long inner-h))

       db'
       (assoc db :scroll sc)]

      (log-scroll! :scroll-up pre (scroll-snapshot sc) {:amount amount :max-s max-s})
      (if load {:db (assoc-in db' [:session :history-loading?] true) :fx [load]} {:db db'}))))

(reg-event-db :scroll-down
              ;; Wheel / arrow / PageDown: ease `amount` rows down; landing within the
              ;; slack band of the bottom re-arms FOLLOW.
              (fn [db [_ amount total-h inner-h]]
                (let
                  [max-s
                   (max 0 (- (long total-h) (long inner-h)))

                   pre
                   (scroll-pre! db)

                   sc
                   (scroll/down (:scroll db) (long amount) max-s)]

                  (log-scroll! :scroll-down pre (scroll-snapshot sc) {:amount amount :max-s max-s})
                  (assoc db :scroll sc))))

(reg-event-db :scroll-to-y
              ;; Scrollbar drag / track click: map the cursor row to an offset and SNAP
              ;; (1:1, no ease - animation would lag the thumb). The very bottom
              ;; re-enters FOLLOW. Mirrors the thumb math in `scrollbar/geometry`:
              ;; `bar-top` is the top track row, `track-h` the track length, and
              ;; `total-h`/`inner-h` the layout sizes the render thread published.
              (fn [db [_ mouse-y bar-top track-h total-h inner-h]]
                (if (or (<= (long total-h) (long inner-h)) (<= (long track-h) 0))
                  db
                  (let
                    [max-s
                     (max 0 (- (long total-h) (long inner-h)))

                     denom
                     (max 1 (- (long track-h) 1))

                     fraction
                     (max 0.0 (min 1.0 (double (/ (- (long mouse-y) (long bar-top)) denom))))

                     offset
                     (long (Math/round (* fraction (double max-s))))]

                    (assoc db :scroll (scroll/to-y offset max-s))))))

(defn- turn-extra-body
  [{:keys [settings] :as db}]
  (when (= :openai-codex (current-provider-id db))
    {:text {:verbosity (name (or (:openai-codex-verbosity settings) "low"))}}))

(defonce ^:private process-submission-id (str (java.util.UUID/randomUUID)))

(defn- submission-prefix
  "Prefix every correlation id this PROCESS+TAB mints carries. Ownership of a
   mirrored queue row is therefore derivable from the id the gateway echoes back,
   with no local bookkeeping to drift out of sync."
  [workspace-id]
  (str "tui:" process-submission-id "/" (when workspace-id (name workspace-id)) ":"))

(defn- mint-client-id
  "Correlation id sent to the gateway as the `idempotency_key` for one submission.
   It encodes THIS process + tab, so `our-submission?` can decide ownership of a
   mirrored row from gateway truth alone - no local bookkeeping that could drift,
   and it still works for a backlog snapshot seeded on re-attach."
  [workspace-id]
  (str (submission-prefix workspace-id) (java.util.UUID/randomUUID)))

(defn- our-submission?
  "True when the gateway's echoed correlation id was minted by THIS tab. The ONE
   ownership rule: a cancel may pull only OUR queued text into THIS composer,
   never a sibling channel's (or another tab's) queued message."
  [workspace-id client-id]
  (boolean (and client-id
                (string? client-id)
                (str/starts-with? client-id (submission-prefix workspace-id)))))

(defn- db-for-tab
  [db workspace-id]
  (if (= workspace-id (current-tab-id db))
    db
    ;; Defaults FIRST — see `restore-tab`. Merging a partial snapshot straight
    ;; over the root would show the ACTIVE tab's turn/queue state as if it
    ;; belonged to `workspace-id`, and queued sends would drain into it.
    (merge db (empty-tab-state) (get-in db [:tab-locals workspace-id]))))

(defn- enqueue-message-result
  [db workspace-id text]
  (let
    [workspace-id
     (or workspace-id (current-tab-id db))

     source-db
     (db-for-tab db workspace-id)

     pastes
     (:pastes source-db)

     session
     (:session source-db)

     ;; DOUBLE-SUBMIT GUARD. `:pending-sends` mirrors GATEWAY truth, so it is
     ;; still EMPTY for the whole enqueue round-trip: comparing against it alone
     ;; let a second Enter inside that window (key repeat, or a terminal that
     ;; delivers CR and LF) mint a second correlation id and register the SAME
     ;; text as a SECOND queued turn — seen in the wild 7 ms apart, both drained
     ;; and both answered. A submission that is sent but not yet acked counts as
     ;; queued here; `:submission-settled` releases it when the round-trip ends.
     in-flight
     (vec (or (:submissions-in-flight source-db) []))

     dup?
     (or (= text (:text (peek (vec (or (:pending-sends source-db) [])))))
         (boolean (some #(= text (:text %)) in-flight)))]

    (cond
      ;; The user asked to STOP the current turn (`:cancelling?`). A submission in
      ;; that window is a FRESH intent, not queue fodder — never park it behind the
      ;; turn being torn down. Parking it here flashed the message into the queue
      ;; and, if the terminal event raced as a normal completion instead of a
      ;; cancel, auto-drained/sent it. The submit path keeps the text in the editor
      ;; instead, so the user re-sends it cleanly once the cancel settles.
      (:cancelling? source-db)
      {:db db :fx [[:notify "Cancelling current turn — message kept in the editor" :warn 2500]]}
      dup? {:db db}
      :else
      (let
        [preview-text
         (input/collapse-paste-placeholders text pastes)

         [agent-text workspace]
         (if session
           (let [ws (active-workspace source-db)]
             [(binding [workspace/*workspace-root* (workspace/workspace-root ws)]
                (input/expand-file-mentions (input/expand-paste-placeholders text pastes))) ws])
           [nil nil])

         client-id
         (mint-client-id workspace-id)

         entry
         {:text text
          :preview-text preview-text
          :agent-text agent-text
          :client-id client-id
          :mine? true
          :pastes pastes
          :paste-counter (:paste-counter source-db)
          :queued-at-ms (System/currentTimeMillis)}

         ;; NO OPTIMISTIC QUEUE ROW. The gateway is the ONE queue of record, so a
         ;; busy-time submission is registered THERE and the row is painted only
         ;; from gateway truth - the ack of this enqueue or the `turn.queued`
         ;; broadcast, both flowing through the single `:sync-queued-turn` writer
         ;; and keyed by the gateway turn id. An HTTP round-trip is milliseconds;
         ;; a locally invented row (no id yet) had to be reconciled against the
         ;; gateway by request TEXT, and that reconciliation is what produced the
         ;; duplicate / ghost "Queued" lines.
         gateway?
         (boolean (and session agent-text))

         gw-fx
         (when gateway?
           [[:gateway-enqueue workspace-id session entry
             (when (reasoning-effort-configurable?) (get-in db [:settings :reasoning-level]))
             (turn-extra-body source-db) {} workspace]])]

        {:db (update-tab db
                         workspace-id
                         (fn [w]
                           (cond-> w
                             ;; In flight until the gateway answers — the ONLY record of
                             ;; this submission until the ack or the `turn.queued`
                             ;; broadcast paints the real row.
                             gateway?
                             (update :submissions-in-flight
                                     (fn [q]
                                       (conj (vec (or q [])) entry)))

                             ;; The correlation id we are about to send as the gateway
                             ;; A submission with NOWHERE to go (session still being
                             ;; created, so there is no gateway queue yet) is the ONLY
                             ;; locally staged row; it drains as soon as the session binds.
                             (not gateway?)
                             (update :pending-sends
                                     (fn [q]
                                       (conj (vec (or q [])) entry)))

                             :always
                             (update :input-history
                                     (fn [xs]
                                       (let [xs (vec (or xs []))]
                                         (if (= text (last xs)) xs (conj xs text))))))))
         :fx (cond-> [[:notify "Queued — will send after current turn" :info 1500]]
               gw-fx
               (into gw-fx))}))))

(reg-event-fx
  :send-message
  ;; `text` is the input-buffer string - it may carry two shorthand surfaces:
  ;;
  ;;   1. `[Pasted #N: ...]` tokens for large clipboard payloads. These
  ;;      are expanded for BOTH the visible transcript and the agent.
  ;;   2. `@path/to/file` mentions inserted by the file picker. Those
  ;;      stay concise in the visible transcript, but expand into
  ;;      a short read-now directive for the AGENT; the model picks
  ;;      the right tool (`cat`, `z/symbols`, etc.) itself.
  (fn [db [_ text workspace-id]]
    (let
      [workspace-id
       (or workspace-id (current-tab-id db))

       source-db
       (db-for-tab db workspace-id)

       pastes
       (:pastes source-db)

       full-text
       (input/expand-paste-placeholders text pastes)

       preview-text
       (input/collapse-paste-placeholders text pastes)]

      (cond (:loading? source-db) (enqueue-message-result db workspace-id text)
            (nil? (:session source-db)) {:db db}
            :else
            (let
              [workspace
               (active-workspace source-db)

               agent-text
               (binding [workspace/*workspace-root* (workspace/workspace-root workspace)]
                 (input/expand-file-mentions full-text))

               token
               (vis/cancellation-token)

               extra-body
               (turn-extra-body source-db)

               turn-features
               {}

               reasoning-level
               (when (reasoning-effort-configurable?) (get-in db [:settings :reasoning-level]))

               client-turn-id
               (str (java.util.UUID/randomUUID))]

              {:db (update-tab
                     db
                     workspace-id
                     (fn [w]
                       (-> w
                           (update :messages
                                   conj
                                   (assoc (chat/user-message preview-text)
                                     :client-turn-id client-turn-id))
                           (update :messages
                                   conj
                                   (assoc (pending-assistant-for text)
                                     :client-turn-id client-turn-id))
                           (update :input-history
                                   (fn [xs]
                                     (let [xs (vec (or xs []))]
                                       (if (= full-text (last xs)) xs (conj xs full-text)))))
                           ;; Sending re-pins to the bottom: one atomic FOLLOW
                           ;; reset replaces the whole `:scroll` value, so no
                           ;; in-flight animation target can dangle and flash the
                           ;; view to the top of the freshly-appended message.
                           (assoc :scroll scroll/follow
                                  :loading? true
                                  :cancel-token token
                                  :cancelling? false
                                  ;; Do NOT clear `:cancel-awaiting-client-id` here. A rapid
                                  ;; cancel/resubmit must retain the old submit's identity until
                                  ;; its delayed turn.started either arrives or is proven absent.
                                  :progress {:iterations []}
                                  :turn-start-ms (System/currentTimeMillis)
                                  ;; Identity of the turn this tab launched DIRECTLY: the
                                  ;; correlation id we sent as the gateway idempotency key. A
                                  ;; queue event echoing OUR OWN submit back (the gateway was
                                  ;; still tearing down a just-cancelled turn and parked it) is
                                  ;; recognised by that id — never by request text — instead of
                                  ;; being painted as a second "Queued" row.
                                  :live-turn-client-id client-turn-id
                                  :submitted-input {:text text
                                                    :pastes (:pastes source-db)
                                                    :paste-counter (:paste-counter source-db)}
                                  :input-history-index nil
                                  :input-history-draft nil
                                  :slash-command-index 0
                                  :slash-command-hidden? false))))
               ;; `agent-text` (LLM-facing, with `@path` expanded into a
               ;; `[Attached File: ...]` directive) drives the model.
               ;; `preview-text` (un-expanded `@path` token, plus a fenced
               ;; head+tail peek of each paste) is the user's collapsed line -
               ;; flowed in as `display-text` so it lands in the persisted
               ;; `user_request` column. Without the split,
               ;; reopening a session re-rendered the verbose attachment
               ;; directive in the user bubble.
               :fx [[:session-turn workspace-id (:session source-db) agent-text token
                     reasoning-level extra-body turn-features workspace client-turn-id
                     preview-text]]})))))

(reg-event-fx :enqueue-message
              ;; Capture a user submission while a previous turn is still processing.
              ;; Queue lives on that workspace/session and drains after the
              ;; in-flight turn commits. No provider call happens from this handler.
              (fn [db [_ text workspace-id]]
                (enqueue-message-result db workspace-id text)))

(reg-event-db :submission-settled
              ;; One submission's gateway round-trip ENDED — queued, already running, or
              ;; failed. Drop it from the in-flight double-submit guard: the text is now
              ;; either a real queued row (mirrored through the one `:sync-queued-turn`
              ;; writer), a live turn, or staged locally, and the next identical text is
              ;; a NEW intent that must go through.
              (fn [db [_ workspace-id client-id]]
                (if-not client-id
                  db
                  (update-tab db
                              (or workspace-id (current-tab-id db))
                              (fn [w]
                                (assoc w
                                  :submissions-in-flight (vec (remove #(= client-id (:client-id %))
                                                                (or (:submissions-in-flight w)
                                                                    [])))))))))

(reg-event-db :stage-queued-locally
              ;; FAILURE PATH ONLY: the gateway enqueue never landed, so there is no
              ;; server record to mirror and nothing would show the text again. Keep it
              ;; as a LOCAL staged row (no `:turn-id`) so the submission is never lost.
              ;; Rows without a turn id are the only ones this channel owns outright -
              ;; every row that HAS one is written and removed by the gateway.
              (fn [db [_ workspace-id entry]]
                (let [wid (or workspace-id (current-tab-id db))]
                  (update-tab db
                              wid
                              (fn [w]
                                (update w
                                        :pending-sends
                                        (fn [q]
                                          (conj (vec (or q []))
                                                (assoc entry
                                                  :mine? true
                                                  :unsent? true)))))))))

(reg-event-fx
  :sync-turn-clock
  ;; The gateway's `turn.started` carries the canonical run clock, turn id,
  ;; AND the submitter's idempotency key (`:client-id`). Correlation matters:
  ;; after a quick Esc + resend, the old POST can start late while the new POST
  ;; waits behind it. Never bind that old ghost to the new optimistic turn.
  (fn [db [_ workspace-id {:keys [started-at-ms server-at-ms turn-id client-id]}]]
    (let
      [workspace-id
       (or workspace-id (current-tab-id db))

       ;; Convert the gateway clock into this process's wall-clock domain using
       ;; the event's gateway-sampled elapsed value.
       local-started-at-ms
       (when (nat-int? started-at-ms)
         (if (nat-int? server-at-ms)
           (- (System/currentTimeMillis) (max 0 (- (long server-at-ms) (long started-at-ms))))
           started-at-ms))]

      (if-not workspace-id
        {:db db}
        (let
          [target
           (db-for-tab db workspace-id)

           cancel-client-id
           (:cancel-awaiting-client-id target)

           live-client-id
           (:live-turn-client-id target)

           ;; Older gateways omitted the correlation id. Event order still makes
           ;; the first start after an armed cancel the old submit, so preserve the
           ;; pre-upgrade safety fallback for that one case.
           awaiting-cancel?
           (boolean
             (and turn-id cancel-client-id (or (nil? client-id) (= client-id cancel-client-id))))

           matching-live-start?
           (boolean (and (not awaiting-cancel?)
                         (or (nil? live-client-id)
                             (= client-id live-client-id)
                             (and (nil? client-id) (nil? cancel-client-id)))))

           ;; If the new submit starts first, FIFO ordering proves the cancelled
           ;; POST never reached the gateway; retire its otherwise-stale marker.
           cancel-resolved?
           (or awaiting-cancel? (and cancel-client-id live-client-id (= client-id live-client-id)))

           sid
           (get-in target [:session :id])

           db'
           (update-tab
             db
             workspace-id
             (fn [w]
               (cond-> w
                 cancel-resolved?
                 (dissoc :cancel-awaiting-client-id)

                 (and matching-live-start? (:loading? w) (nat-int? local-started-at-ms))
                 (assoc :turn-start-ms local-started-at-ms)

                 (and matching-live-start? (:loading? w) turn-id (nil? (:gateway-turn-id w)))
                 (assoc :gateway-turn-id turn-id))))]

          (cond-> {:db db'}
            (and awaiting-cancel? sid)
            (assoc :fx [[:gateway-cancel-turn sid turn-id]])))))))

(def ^:private terminal-result-grace-ms
  "Give the blocking submit/attach worker one short window to deliver the full
   result after the persistent mux observes the terminal event. If that worker
   is stranded, the independent terminal path settles its placeholder."
  500)

(defn- terminal-status
  [status]
  (keyword (or (some-> status
                       name)
               "completed")))

(defn- terminal-content
  "Content blocks for a turn the INDEPENDENT terminal path had to settle.

   A failed turn carries the gateway's OWN error blocks (`turn.failed` ships the
   settled content), and those ARE the styled provider card — rate-limited,
   auth, transport. Fabricating \"Turn failed.\" instead is exactly how a
   rate-limited turn painted a bare `ERROR: turn failed` row, or showed BOTH
   that row and the real card depending on which copy landed first."
  [{:keys [status turn-id content trace]}]
  (let [blocks (vec (filter map? content))]
    (if (seq blocks)
      blocks
      ;; A COMPLETED turn already streamed its answer: every `iteration-final`
      ;; projected `:assistant-prose` onto the trace entry, so the last non-blank
      ;; one IS the settled answer. Painting "Turn completed." over it loses the
      ;; whole reply, and the fix belongs here (zero wire bytes) rather than in a
      ;; fattened `turn.completed` payload — the terminal event stays LEAN.
      (let
        [prose (when (= :completed (terminal-status status))
                 (->> trace
                      (keep #(some-> (:assistant-prose %)
                                     str
                                     str/trim
                                     not-empty))
                      last))]
        (if prose
          [{"id" (str "terminal-" (or turn-id (java.util.UUID/randomUUID)))
            "type" "prose"
            "markdown" prose}]
          (let
            [[type code message] (case (terminal-status status)
                                   :cancelled
                                   ["notice" "turn_cancelled" "Cancelled by user."]

                                   :failed
                                   ["error" "turn_failed" "Turn failed."]

                                   ["notice" "turn_completed" "Turn completed."])]
            [{"id" (str "terminal-" (or turn-id (java.util.UUID/randomUUID)))
              "type" type
              "code" code
              "message" message}]))))))

(reg-event-fx :sync-turn-terminal
              ;; The persistent mux is independent of the blocking submit/attach worker.
              ;; Stop the matching optimistic spinner immediately, but retain its exact
              ;; placeholder for a brief grace period so the normal worker can still paint
              ;; the full answer. The delayed reconciliation owns only that generation.
              (fn [db [_ workspace-id chunk]]
                (let
                  [workspace-id
                   (or workspace-id (current-tab-id db))

                   target
                   (when workspace-id (db-for-tab db workspace-id))

                   gateway-turn-id
                   (:gateway-turn-id target)

                   live-client-id
                   (:live-turn-client-id target)

                   turn-id
                   (:turn-id chunk)

                   client-id
                   (:client-id chunk)

                   matching-turn?
                   (or (and turn-id gateway-turn-id (= (str turn-id) (str gateway-turn-id)))
                       (and client-id live-client-id (= (str client-id) (str live-client-id))))

                   idx
                   (when matching-turn? (pending-assistant-index (:messages target) client-id))

                   terminal
                   (assoc chunk
                     :status (terminal-status (:status chunk))
                     :trace (vec (or (get-in target [:progress :iterations]) [])))]

                  (if-not (and workspace-id (:loading? target) matching-turn?)
                    {:db db}
                    {:db (update-tab db
                                     workspace-id
                                     (fn [workspace]
                                       (cond-> (clear-active-turn-state workspace)
                                         idx
                                         (assoc-in [:messages idx :terminal-pending] terminal))))
                     :fx (cond-> []
                           idx
                           (conj [:settle-turn-terminal-later workspace-id terminal]))}))))

(reg-event-fx
  :settle-turn-terminal
  (fn [db [_ workspace-id terminal]]
    (let
      [workspace-id
       (or workspace-id (current-tab-id db))

       target
       (when workspace-id (db-for-tab db workspace-id))

       messages
       (vec (or (:messages target) []))

       idx
       (first (keep-indexed (fn [idx message]
                              (when (and (pending-assistant-message? message)
                                         (= terminal (:terminal-pending message)))
                                idx))
                            messages))

       options
       {:client-turn-id (:client-id terminal)
        :status (terminal-status (:status terminal))
        :terminal-sync? true
        :terminal-trace (:trace terminal)}]

      (cond (nil? idx) {:db db}
            ;; A resend started during the grace period. Settle only the old bubble;
            ;; never clear the new generation's spinner/editor/cancellation state.
            (:loading? target) {:db (update-tab db
                                                workspace-id
                                                (fn [workspace]
                                                  (assoc workspace
                                                    :messages (assoc (vec (:messages workspace))
                                                                idx (completion-response
                                                                      (terminal-content terminal)
                                                                      (:trace terminal)
                                                                      nil
                                                                      options))
                                                    :scroll scroll/follow)))}
            :else {:db db
                   :fx [[:dispatch
                         [:message-received workspace-id (terminal-content terminal) options]]]}))))

(reg-event-db :sync-queue-paused
              ;; Mirror the gateway's queue.paused / queue.resumed signal into the
              ;; tab so the Queued strip shows the provider-failure hold. The chunk
              ;; carries `:queue-paused` (a map while paused, nil once resumed).
              (fn [db [_ workspace-id chunk]]
                (let [workspace-id (or workspace-id (current-tab-id db))]
                  (if-not workspace-id
                    db
                    (update-tab db
                                workspace-id
                                (fn [w]
                                  (assoc w :queue-paused (:queue-paused chunk))))))))

(defn- same-submission?
  "True when local queue-mirror entry `e` and a gateway queued turn are the SAME
   submission. THE one identity rule for the mirror, and it is ID-ONLY:

     1. gateway `turn-id` - every mirrored row carries one, because rows are
        painted from gateway truth (enqueue ack or `turn.queued` broadcast) and
        never invented locally;
     2. `client-id` - the correlation id this tab minted and sent as the gateway
        `idempotency_key`, for the window where only one side knows the turn id.

   Request TEXT is deliberately NOT a fallback: two identical prompts are
   indistinguishable by text, so matching on it is exactly how a queued turn got
   suppressed (or duplicated) behind an unrelated one."
  [e turn-id client-id]
  (boolean (or (and turn-id (:turn-id e) (= turn-id (:turn-id e)))
               (and client-id (:client-id e) (= client-id (:client-id e))))))

(defn- live-turn-mirror?
  "True when gateway turn `turn-id` (correlation `client-id`) is THIS tab's LIVE
   turn, so it must never be mirrored as a \"Queued\" row.

   THE one rule, shared by every queue mirror writer - the live `:queue-sync`
   event (`:sync-queued-turn`) and the `:queued-turns` snapshot seeded on attach.
   Two writers with two rules is exactly how a running turn reappeared as queued
   (\"sent AND queued at the same time\"): whichever path saw a stale snapshot won.

   Same ID-ONLY order as `same-submission?`: gateway turn id, then the correlation
   id we sent as the idempotency key. No text heuristic."
  [w turn-id client-id]
  (boolean (or (and turn-id (= turn-id (:gateway-turn-id w)))
               (and client-id (= client-id (:live-turn-client-id w))))))

(defn- restore-entries-to-input
  "Append queued submissions `entries` (oldest first) to tab `w`'s editor, after
   whatever is already typed, and merge their pastes in.

   A DRAFT, never a send: the queue is gone, but the words the user wrote are
   still theirs to re-send, edit or delete. Callers decide WHICH entries come
   back; this is the one place that knows how they land in the editor."
  [w entries]
  (let [entries (vec (remove nil? entries))]
    (if (empty? entries)
      w
      (let
        [cur-text (input/input->text (:input w))
         texts (into (if (str/blank? cur-text) [] [cur-text]) (map :text entries))
         combined (str/join "\n\n" (remove str/blank? texts))
         merged-pastes (reduce merge (or (:pastes w) {}) (map :pastes entries))
         merged-counter (apply max 0 (:paste-counter w 0) (map #(:paste-counter % 0) entries))]

        (assoc w
          :input (text->input-state combined)
          :pastes merged-pastes
          :paste-counter merged-counter
          :input-history-index nil
          :input-history-draft nil)))))

(reg-event-db
  :sync-queued-turn
  ;; THE ONE WRITER of gateway-owned queue rows. Every add carries gateway truth -
  ;; either the `turn.queued` / `.updated` / `.deleted` / `.drained` broadcast
  ;; (forwarded as a :queue-sync chunk by the sync/attach subscriptions) or the ack
  ;; of this tab's own `:gateway-enqueue`, which has the same shape and the same
  ;; key. So a row ALWAYS has the gateway turn id, nothing is matched by request
  ;; text, and whichever of ack/broadcast arrives first wins (the other no-ops):
  ;;   :add    - no-op when the turn id is already mirrored, otherwise append.
  ;;   :update - rewrite the entry's text (queued-prompt edit anywhere).
  ;;   :delete - drop the entry (cleared / pulled back / drained anywhere).
  ;; A turn that is (or just became) this tab's LIVE turn collapses EVERY op to
  ;; "ensure it is not mirrored": a late, replayed or out-of-order queue event must
  ;; not resurrect a running turn as a "Queued" row.
  (fn
    [db [_ workspace-id {:keys [op turn-id client-id text preview-text reason] mine-hint? :mine?}]]
    (let [workspace-id (or workspace-id (current-tab-id db))]
      (if-not (and workspace-id turn-id)
        db
        (update-tab
          db
          workspace-id
          (fn [w]
            (let
              [;; OURS when the correlation id the gateway echoed back was
               ;; minted by THIS tab, or when a caller that already knows the
               ;; provenance says so (the delete reconcile re-writing a row it
               ;; just removed).
               mine? (boolean (or mine-hint? (our-submission? workspace-id client-id)))
               row (cond->
                     {:text text
                      :preview-text (or preview-text text)
                      :turn-id turn-id
                      :queued-at-ms (System/currentTimeMillis)}
                     client-id
                     (assoc :client-id client-id)

                     mine?
                     (assoc :mine? true))
               live? (live-turn-mirror? w turn-id client-id)
               mirrored (first (filter #(= turn-id (:turn-id %)) (:pending-sends w)))
               ;; A USER CANCEL drops the ENTIRE pre-cancel backlog server-side
               ;; and broadcasts one `turn.queued.deleted` per row carrying
               ;; `reason "cancelled"` and the row's text (gateway
               ;; `drop-cancelled-backlog!`). Stop means stop - but the words the
               ;; user already wrote are theirs, so every dropped row comes back
               ;; into THIS tab's editor as a draft.
               ;;
               ;; Authorship is deliberately NOT consulted: whoever queued the
               ;; message and whichever channel pressed stop, a mirror that just
               ;; vanishes is exactly the silent loss this exists to prevent. It
               ;; lands in the tab that owns the SESSION, not the focused one, so
               ;; a cancel while looking at another session still restores.
               ;;
               ;; A plain `:delete` (user cleared the row) and a `.drained`
               ;; (gateway started it) carry no reason and restore nothing, and a
               ;; row already pulled back locally is no longer mirrored - which is
               ;; what keeps the text from landing twice.
               restore? (and (= op :delete) (= reason "cancelled") (some? mirrored) (not live?))
               w' (update w
                          :pending-sends
                          (fn [q]
                            (let
                              [q (vec (or q []))
                               mirrored? (boolean (some #(= turn-id (:turn-id %)) q))]

                              (if live?
                                (vec (remove #(= turn-id (:turn-id %)) q))
                                (case op
                                  :add
                                  (if mirrored? q (conj q row))

                                  :update
                                  (if mirrored?
                                    (mapv (fn [e]
                                            (if (= turn-id (:turn-id e))
                                              (assoc e
                                                :text text
                                                :preview-text (or preview-text text)
                                                :agent-text text)
                                              e))
                                          q)
                                    (conj q row))

                                  :delete
                                  (vec (remove #(= turn-id (:turn-id %)) q))

                                  q)))))]

              (cond-> w'
                restore?
                (restore-entries-to-input [(cond-> mirrored
                                             (str/blank? (:text mirrored))
                                             (assoc :text text))])))))))))

(reg-event-fx :clear-pending-sends
              ;; Explicit user action - escape hatch when the queued items are no
              ;; longer wanted. Cancelling the in-flight turn must NOT auto-drop
              ;; them; that would reintroduce silent loss. Also removes the matching
              ;; gateway queued records so they never auto-drain server-side.
              (fn [db _]
                (let
                  [tab-id
                   (current-tab-id db)

                   sid
                   (get-in db [:session :id])

                   pending
                   (vec (:pending-sends (db-for-tab db tab-id)))]

                  {:db (update-tab db
                                   tab-id
                                   (fn [w]
                                     (assoc w :pending-sends [])))
                   :fx (into []
                             (keep (fn [e]
                                     (when-let [tid (:turn-id e)]
                                       [:gateway-delete-queued sid tid tab-id e])))
                             pending)})))

(reg-event-fx :drain-pending
              ;; Pop one queued submission for `workspace-id`. When it carries a
              ;; gateway turn id the gateway already (auto-)started it, so ATTACH and
              ;; render its result instead of submitting again. Without a gateway id
              ;; (submit failed) fall back to a fresh local `:send-message`.
              (fn [db [_ workspace-id]]
                (let
                  [workspace-id
                   (or workspace-id (current-tab-id db))

                   source-db
                   (db-for-tab db workspace-id)

                   q
                   (vec (or (:pending-sends source-db) []))

                   head
                   (first q)]

                  (cond (or (nil? head)
                            ;; A turn is ALREADY streaming into this tab (e.g. the
                            ;; persistent event listener attached a sibling-started turn
                            ;; first): draining now would double-attach the same turn.
                            ;; The queue pops again on the NEXT terminal.
                            (:loading? source-db))
                        {:db db}
                        (:turn-id head)
                        (let
                          [token
                           (vis/cancellation-token)

                           client-turn-id
                           (str (java.util.UUID/randomUUID))

                           preview-text
                           (or (:preview-text head) (:text head))

                           session
                           (:session source-db)]

                          {:db (update-tab db
                                           workspace-id
                                           (fn [w]
                                             (-> w
                                                 (assoc :pending-sends (vec (rest q)))
                                                 (update :messages
                                                         conj
                                                         (assoc (chat/user-message preview-text)
                                                           :client-turn-id client-turn-id))
                                                 (update :messages
                                                         conj
                                                         (assoc (pending-assistant-for preview-text)
                                                           :client-turn-id client-turn-id))
                                                 (assoc :scroll scroll/follow
                                                        :loading? true
                                                        :cancel-token token
                                                        :gateway-turn-id (:turn-id head)
                                                        :cancelling? false
                                                        :progress {:iterations []}
                                                        :turn-start-ms (System/currentTimeMillis)
                                                        :input-history-index nil
                                                        :input-history-draft nil))))
                           :fx [[:session-attach workspace-id session (:turn-id head) token
                                 client-turn-id]]})
                        :else {:db (update-tab db
                                               workspace-id
                                               (fn [w]
                                                 (assoc w
                                                   :pending-sends (vec (rest q))
                                                   :pastes (or (:pastes head) {})
                                                   :paste-counter (or (:paste-counter head) 0))))
                               :fx [[:dispatch [:send-message (:text head) workspace-id]]]}))))

(reg-event-fx :reattach-disconnected-turn
              (fn [db [_ workspace-id session tid token client-turn-id]]
                (let
                  [workspace-id
                   (or workspace-id (current-tab-id db))

                   target
                   (db-for-tab db workspace-id)]

                  (if (and workspace-id
                           tid
                           (:loading? target)
                           (identical? token (:cancel-token target))
                           (or (nil? (:gateway-turn-id target)) (= tid (:gateway-turn-id target))))
                    (let [next-token (vis/cancellation-token)]
                      {:db (update-tab db
                                       workspace-id
                                       #(assoc %
                                          :gateway-turn-id tid
                                          :cancel-token next-token))
                       :fx [[:session-attach workspace-id session tid next-token client-turn-id]]})
                    {:db db}))))

(reg-event-fx
  :attach-running-turn
  ;; Subscribe to a turn ALREADY running for `session` (started in THIS TUI,
  ;; the web, or a sibling process) the moment its tab opens/resumes, so it
  ;; STREAMS live into the tab instead of showing frozen history until it
  ;; lands in the DB. `session` carries :id, :status/:current-turn-id and the
  ;; in-flight turn's `:running-request` text + its canonical gateway
  ;; `:running-started-at` (from `chat/resume-session`). The elapsed
  ;; clock seeds from that gateway timestamp — NOT this process's
  ;; attach time — so every attached TUI shows the same elapsed.
  ;; No-op unless the session is genuinely running AND the target tab isn't
  ;; already attached (guards against double-attaching an already-live tab).
  ;; Mirrors `:drain-pending`'s busy-time attach: seed the user + pending
  ;; assistant bubbles, arm the turn state, then hand off to `:session-attach`.
  (fn [db [_ workspace-id session]]
    (let
      [workspace-id
       (or workspace-id (current-tab-id db))

       sid
       (:id session)

       tid
       (:current-turn-id session)

       ;; FIRST mirror the gateway's queued backlog (queued from
       ;; ANY channel — chat/resume-session :queued-turns) into
       ;; this tab's local queue, dedup'd by gateway turn id.
       ;; Runs even when nothing is running: a cancel leaves the
       ;; backlog queued server-side, and resume must surface it
       ;; instead of silently dropping it.
       db
       (if-let [qs (and workspace-id (seq (:queued-turns session)))]
         (update-tab
           db
           workspace-id
           (fn [w]
             (update w
                     :pending-sends
                     (fn [q]
                       (let [q (vec (or q []))]
                         (into q
                               (keep
                                 (fn [{:keys [turn-id client-id text preview-text queued-at-ms]}]
                                   (when-not (or (some #(same-submission? % turn-id client-id) q)
                                                 (live-turn-mirror? w turn-id client-id))
                                     (cond->
                                       {:text text
                                        ;; Gateway-derived row text (image paths already
                                        ;; chipped); falls back to the raw request.
                                        :preview-text (or preview-text text)
                                        :turn-id turn-id
                                        :queued-at-ms (or queued-at-ms (System/currentTimeMillis))}
                                       client-id
                                       (assoc :client-id client-id)

                                       ;; Re-attach (tab reopen / project switch) seeds the
                                       ;; backlog from the gateway snapshot: ownership comes
                                       ;; from the SAME echoed id, so a cancel still knows
                                       ;; which rows may return to this composer.
                                       (our-submission? workspace-id client-id)
                                       (assoc :mine? true)))))
                               qs))))))
         db)

       target
       (db-for-tab db workspace-id)]

      (if-not (and workspace-id
                   sid
                   tid
                   (session-running? session)
                   (not (:loading? target))
                   (not (:gateway-turn-id target)))
        ;; Not attaching a live turn here. But if the session is IDLE with a
        ;; server-side queued backlog (left queued by a cancel, or submitted
        ;; from another channel while we were away) and this tab isn't already
        ;; busy, kick the queue into motion so it starts RIGHT AWAY on
        ;; open/resume instead of sitting there. The daemon starts the head
        ;; turn and emits turn.started, which our event subscription turns into
        ;; :sibling-turn-started -> :attach-running-turn (which paints it).
        (if (and workspace-id
                 sid
                 (not (session-running? session))
                 (seq (:queued-turns session))
                 (not (:loading? target))
                 (not (:gateway-turn-id target)))
          {:db db :fx [[:drain-idle-queue sid]]}
          {:db db})
        (let
          [token
           (vis/cancellation-token)

           client-turn-id
           (str (java.util.UUID/randomUUID))

           request-text
           (or (:running-request session) "")]

          {:db (update-tab
                 db
                 workspace-id
                 (fn [w]
                   (-> w
                       (update :messages
                               conj
                               (assoc (chat/user-message request-text)
                                 :client-turn-id client-turn-id))
                       (update :messages
                               conj
                               (assoc (pending-assistant-for request-text)
                                 :client-turn-id client-turn-id))
                       ;; The turn we are ATTACHING as running must not
                       ;; ALSO linger as a "Queued" row: the backlog mirror
                       ;; above (and any in-flight :sync-queued-turn :add)
                       ;; may have seeded `tid` from a snapshot taken while it
                       ;; was still queued. Strip it so it paints once — as
                       ;; the live turn — not a second time under Queued.
                       (update :pending-sends
                               (fn [q]
                                 (vec (remove #(same-submission? % tid (:running-client-id session))
                                        (or q [])))))
                       (assoc :scroll scroll/follow
                              :loading? true
                              :cancel-token token
                              :gateway-turn-id tid
                              :cancelling? false
                              :progress {:iterations []}
                              :turn-start-ms (or (:running-started-at session)
                                                 (System/currentTimeMillis))
                              :input-history-index nil
                              :input-history-draft nil))))
           :fx [[:session-attach workspace-id session tid token client-turn-id]]})))))

(reg-event-fx :sibling-turn-started
              ;; A turn STARTED on this session from a SIBLING channel (another TUI, the
              ;; web) while this tab sat idle — delivered by the tab's persistent event
              ;; subscription (chat/subscribe-session-events!), which is the only way an
              ;; idle tab hears about it. Synthesize the running-session shape
              ;; :attach-running-turn expects and hand off; its guards plus ours (already
              ;; loading / already attached) make this a no-op for the tab that started
              ;; or drained onto the turn itself.
              (fn [db [_ workspace-id {:keys [turn-id request started-at-ms]}]]
                (let
                  [workspace-id
                   (or workspace-id (current-tab-id db))

                   target
                   (db-for-tab db workspace-id)

                   session
                   (:session target)

                   ;; Already OUR live turn — the drain/attach machinery owns it.
                   ours?
                   (boolean (and turn-id (= turn-id (:gateway-turn-id target))))

                   ;; BUSY tab: a turn is still streaming here — typically the very turn
                   ;; whose terminal the gateway used to DRAIN this one. Dropping the start
                   ;; lost the message outright: `turn.queued.drained` (delivered on the
                   ;; same subscription, always before our local completion lands) has
                   ;; already removed the mirrored queue row, so `:drain-pending` finds
                   ;; nothing and NOTHING paints the new turn. Park it instead and replay
                   ;; it from `:message-received` the moment the tab settles.
                   busy?
                   (boolean (or (:loading? target) (:gateway-turn-id target)))]

                  (cond (or (nil? workspace-id) (nil? session) (nil? turn-id) ours?) {:db db}
                        busy? {:db (update-tab db
                                               workspace-id
                                               #(assoc %
                                                  :deferred-sibling-start {:turn-id turn-id
                                                                           :request request
                                                                           :started-at-ms
                                                                           started-at-ms}))}
                        :else {:db (update-tab db workspace-id #(dissoc % :deferred-sibling-start))
                               :fx [[:dispatch
                                     [:attach-running-turn workspace-id
                                      (assoc session
                                        :status "running"
                                        :current-turn-id turn-id
                                        :running-request request
                                        :running-started-at started-at-ms)]]]}))))

(reg-event-fx :restore-pending-to-input
              ;; A user cancel with a queued backlog must NOT auto-send the next message.
              ;; Pull every queued (not-yet-started) submission back into the editor —
              ;; appended after whatever the cancelled prompt already restored — and delete
              ;; the matching gateway queued records so nothing drains server-side.
              (fn [db [_ workspace-id]]
                (let
                  [workspace-id
                   (or workspace-id (current-tab-id db))

                   source-db
                   (db-for-tab db workspace-id)

                   pending
                   (vec (or (:pending-sends source-db) []))

                   ;; OWNERSHIP: only rows THIS tab submitted (`:mine?` - stamped when the
                   ;; gateway echoed back a correlation id this tab minted, or when the
                   ;; submission never reached the gateway at all) come back to the
                   ;; editor. Rows queued by a sibling TUI / the web are the sibling's
                   ;; text, so they are never pulled into OUR composer.
                   ;;
                   ;; We no longer have to police the server records: a user cancel
                   ;; drops the whole pre-cancel backlog server-side in one swap and
                   ;; broadcasts `turn.queued.deleted` (gateway `drop-cancelled-backlog!`),
                   ;; which removes every mirror — ours and the sibling's — and settles
                   ;; any client blocked on such a turn. The per-tid deletes below are
                   ;; now only a fast local echo, reconciled from gateway truth when the
                   ;; delete does not land (see the `:gateway-delete-queued` fx).
                   mine
                   (vec (filter :mine? pending))

                   mirrors
                   (vec (remove :mine? pending))

                   sid
                   (get-in source-db [:session :id])]

                  (if (empty? mine)
                    {:db db}
                    {:db (update-tab db
                                     workspace-id
                                     (fn [w]
                                       (-> (restore-entries-to-input w mine)
                                           (assoc :pending-sends mirrors))))
                     :fx (into [[:notify "Queue restored to input — not sent" :info 2000]]
                               (keep (fn [e]
                                       (when-let [tid (:turn-id e)]
                                         [:gateway-delete-queued sid tid workspace-id e])))
                               mine)}))))

(defn- gateway-cancel-turn-or-current!
  "Cancel the gateway turn, retrying transient transport failures.

   With a known `tid`, use the id-addressed route; otherwise use `cancel-current`
   for the Esc-before-`turn.started` race. HTTP errors are semantic responses and
   are returned immediately. Discovery, connection, and other transport failures
   are retried because dropping one cancel leaves the provider running and makes
   the user's next request queue behind the supposedly cancelled turn."
  [sid tid]
  (when sid
    (loop [attempt 1]
      (let
        [outcome (try {:response (if tid
                                   (vis/gateway-cancel-turn! sid tid)
                                   (vis/gateway-cancel-current-turn! sid))}
                      (catch clojure.lang.ExceptionInfo e
                        (let [data (ex-data e)]
                          (if (:http-status data)
                            {:response {:error (keyword (or (get data "error") "gateway-error"))
                                        :http-status (:http-status data)}}
                            {:transport-error e})))
                      (catch Throwable t {:transport-error t}))]
        (cond (contains? outcome :response) (:response outcome)
              (>= (long attempt) 3) {:error :gateway-unreachable}
              :else (do (Thread/sleep (* 200 (long attempt))) (recur (inc (long attempt)))))))))

(def ^:private gateway-terminal-cancel-errors
  "Gateway cancel errors that mean the server turn is ALREADY gone/terminal —
   safe to clear local cancelling state immediately (no completion event will
   ever arrive)."
  #{:turn-not-found :not-running :no-running-turn})

(reg-event-fx
  :cancel-turn
  (fn [db _]
    (if-not (:loading? db)
      {:db db}
      (let
        [sid
         (get-in db [:session :id])

         tid
         (:gateway-turn-id db)

         cancel-client-id
         (when-not tid (:live-turn-client-id db))

         token
         (:cancel-token db)

         already-cancelling?
         (:cancelling? db)

         cancel-key
         (when-not already-cancelling? (System/currentTimeMillis))

         db'
         (if already-cancelling?
           (cond-> (clear-active-turn-state (park-live-trace db))
             cancel-client-id
             (assoc :cancel-awaiting-client-id cancel-client-id))
           (cond->
             (assoc db
               :cancelling? true
               :cancelling-at-ms cancel-key)
             cancel-client-id
             (assoc :cancel-awaiting-client-id cancel-client-id)))]

        ;; Esc must never wait for gateway discovery, HTTP, or the daemon's durable
        ;; cancel stamp. Commit the visible state first, synchronously fire the LOCAL
        ;; token next, and send the server cancel on its own daemon thread. The old
        ;; inline HTTP call could block the Lanterna input loop for its full 30s timeout,
        ;; delaying the local interrupt and making the TUI look disconnected.
        {:db db'
         :fx [[:cancel-local-turn token]
              [:notify
               (if already-cancelling? "Turn force-cancelled locally." "Cancelling current turn...")
               (if already-cancelling? :warn :info) cancel-notification-ttl-ms]
              [:gateway-cancel-active sid tid cancel-key]]}))))

(reg-event-fx :gateway-cancel-result
              ;; Do not let the LOCAL attach worker's synthetic `:cancelled` result
              ;; unlock resending before the daemon accepted the cancel. Only this
              ;; generation-keyed gateway ACK (or an already-terminal response) may
              ;; release the wait; a late result cannot clear a newer turn.
              (fn [db [_ cancel-key result]]
                (let
                  [accepted?
                   (and (map? result) (not (:error result)))

                   terminal?
                   (contains? gateway-terminal-cancel-errors (:error result))]

                  (if (and cancel-key
                           (:cancelling? db)
                           (= cancel-key (:cancelling-at-ms db))
                           (or accepted? terminal?))
                    (let [workspace-id (current-tab-id db)]
                      {:db (settle-cancelled-turn db)
                       :fx (cond->
                             [[:notify
                               (if terminal?
                                 "Turn is no longer running; cleared local cancelling state."
                                 "Cancellation accepted. You can send again.") :info
                               cancel-notification-ttl-ms]]
                             (some :mine? (:pending-sends (db-for-tab db workspace-id)))
                             (conj [:dispatch [:restore-pending-to-input workspace-id]]))})
                    {:db db}))))

(reg-event-fx :cancel-tab-turn
              ;; Best-effort SERVER-side cancel for the turn running in `tab-id`'s session —
              ;; fired by the close-busy-tab prompt's "Cancel the turn and close" choice
              ;; (screen/close-tab-with-prompt!). The tab is about to CLOSE, so no local
              ;; cancelling state is armed (there will be no tab left to clear or self-heal);
              ;; reaching the daemon is the whole point. Reads the tab's session + gateway
              ;; turn id from `:tab-locals` (`sync-active-tab` snapshots the ACTIVE tab there
              ;; too), and `gateway-cancel-turn-or-current!` falls back to the tid-less
              ;; `cancel-current` when the turn id never bound.
              (fn [db [_ tab-id]]
                (let
                  [db
                   (-> db
                       ensure-tabs
                       sync-active-tab)

                   target-id
                   (or tab-id (current-tab-id db))

                   snap
                   (get-in db [:tab-locals target-id])

                   sid
                   (some-> snap
                           :session
                           :id)

                   tid
                   (:gateway-turn-id snap)]

                  {:db db
                   :fx (cond-> []
                         sid
                         (conj [:gateway-cancel-active sid tid nil]))})))

(defn- cancel-self-heal-due?
  "True when a user cancel has been pending (`:cancelling?`) at least
   `cancel-self-heal-timeout-ms` without the daemon's terminal event arriving to
   release it — the SSE-drop / daemon-died edge that would otherwise wedge input."
  [db now-ms]
  (boolean (and (:cancelling? db)
                (:cancelling-at-ms db)
                (>= (- (long now-ms) (long (:cancelling-at-ms db)))
                    (long cancel-self-heal-timeout-ms)))))

(reg-event-fx :cancel-self-heal-tick
              ;; Render-loop heartbeat safety net for a STUCK cancel (see
              ;; `cancel-self-heal-timeout-ms`). Once the pending `:cancelling?` has outlived
              ;; the timeout with no terminal event, self-heal locally: re-fire the cancel
              ;; token (tears down any lingering local attach waiter), clear the turn state so
              ;; sends flow again, and restore the AUTHORED backlog to the editor so nothing
              ;; the user typed is lost — the same restore the terminal-event path performs.
              ;; Pure over an injected `now-ms` (tests pass it; the render loop omits it →
              ;; System/currentTimeMillis), so a dropped event self-heals deterministically.
              (fn [db [_ now-ms]]
                (let [now (or now-ms (System/currentTimeMillis))]
                  (if-not (cancel-self-heal-due? db now)
                    {:db db}
                    (let
                      [workspace-id (current-tab-id db)
                       token (:cancel-token db)
                       sid (get-in db [:session :id])
                       tid (:gateway-turn-id db)]

                      ;; Release input immediately. Server cancellation is best effort on
                      ;; the dedicated cancel lane; it must not stall this heartbeat.
                      {:db (settle-cancelled-turn db)
                       :fx (cond->
                             [[:cancel-local-turn token] [:gateway-cancel-active sid tid nil]
                              [:notify "Cancel timed out — cleared locally. You can send again."
                               :warn cancel-notification-ttl-ms]]
                             (some :mine? (:pending-sends (db-for-tab db workspace-id)))
                             (conj [:dispatch [:restore-pending-to-input workspace-id]]))})))))

(defn- loading-tab-ids
  "Ids of every tab with a turn in flight — the ACTIVE tab (db root) plus each
   loading `:tab-locals` snapshot."
  [db]
  (let [active (current-tab-id db)]
    (cond->
      (vec (keep (fn [[tab-id snap]]
                   (when (and (not= tab-id active) (:loading? snap)) tab-id))
                 (:tab-locals db)))
      (:loading? db)
      (conj active))))

(defn- turn-liveness-probe-due?
  "True when a tab's in-flight turn has outlived `turn-liveness-grace-ms` and has
   not been probed within `turn-liveness-probe-interval-ms`. A pending cancel is
   skipped — `:cancel-self-heal-tick` already owns that path."
  [tab now-ms]
  (boolean (and (:loading? tab)
                (:gateway-turn-id tab)
                (not (:cancelling? tab))
                (>= (- (long now-ms) (long (or (:turn-start-ms tab) now-ms)))
                    (long turn-liveness-grace-ms))
                (>= (- (long now-ms) (long (or (:liveness-probed-at-ms tab) 0)))
                    (long turn-liveness-probe-interval-ms)))))

(reg-event-fx :turn-liveness-tick
              ;; Render-loop heartbeat safety net for a turn the GATEWAY already settled
              ;; while the client still shows it live — the terminal `turn.completed` never
              ;; landed (mux SSE reconnect gap, a wedged blocking attach worker, a client
              ;; that started before the fix). Without it the last thinking block keeps
              ;; breathing and the spinner counts past a turn that finished minutes ago.
              ;;
              ;; Transport-INDEPENDENT by design: it asks the gateway's turn registry (the
              ;; one source of truth) instead of waiting on the event stream, and feeds the
              ;; answer through the SAME `:sync-turn-terminal` writer the real event uses,
              ;; so settling stays byte-identical (grace period, trace, prose fallback).
              ;; Pure over an injected `now-ms` (tests pass it; the render loop omits it).
              (fn [db [_ now-ms]]
                (let
                  [now
                   (or now-ms (System/currentTimeMillis))

                   due
                   (filterv (fn [tab-id]
                              (and (turn-liveness-probe-due? (db-for-tab db tab-id) now)
                                   (some? (get-in (db-for-tab db tab-id) [:session :id]))))
                     (loading-tab-ids db))]

                  (if (empty? due)
                    {:db db}
                    {:db (reduce (fn [acc tab-id]
                                   (update-tab acc tab-id #(assoc % :liveness-probed-at-ms now)))
                                 db
                                 due)
                     :fx (mapv (fn [tab-id]
                                 (let [tab (db-for-tab db tab-id)]
                                   [:probe-turn-liveness tab-id (get-in tab [:session :id])
                                    (:gateway-turn-id tab) (:live-turn-client-id tab)]))
                               due)}))))

(defn- gateway-resync-probe-due?
  "True when a tab's in-flight turn must be re-asked of the gateway RIGHT NOW,
   off the render heartbeat. Same shape as `turn-liveness-probe-due?` minus the
   start-up grace: the caller (`:sync-gateway-ready`) already holds the server's
   own verdict that the turn painted here is not the turn the daemon is running,
   so a turn that went quiet one second ago earns the probe as much as an old one.

   Throttled on its OWN stamp, never the heartbeat's. A probe issued while the
   socket was down asked over a connection that was failing, so its silence
   proves nothing and must not suppress the one probe that can finally answer.
   The stamp still collapses the N copies of a single broadcast (the mux delivers
   one ready frame per open session tab) into one round."
  [tab now-ms]
  (boolean (and (:loading? tab)
                (:gateway-turn-id tab)
                (not (:cancelling? tab))
                (let [resynced-ms (:gateway-resynced-at-ms tab)]
                  (or (nil? resynced-ms)
                      (>= (- (long now-ms) (long resynced-ms))
                          (long turn-liveness-probe-interval-ms)))))))

(reg-event-fx
  :sync-gateway-ready
  ;; Inversion of control for the reconnect gap. The server emits
  ;; `subscription.ready` for every session on every (re)subscribe, BEFORE
  ;; the replay, and it now carries the daemon's own view: `current_turn_id`
  ;; plus `is_live`. So the client is TOLD "the turn you are painting is not
  ;; the one I am running" instead of discovering it on a timer.
  ;;
  ;; A probe fires only on DISAGREEMENT — the tab paints a live turn and the
  ;; daemon reports a different one, or none. Agreement is a positive verdict
  ;; from the source of truth and costs zero round-trips, which is what lets
  ;; the render heartbeat stay a slow last resort.
  ;;
  ;; An OLD daemon omits the state (`:is-state-known` false); then the frame
  ;; degrades to the previous behaviour — reconnect probes unconditionally,
  ;; which is merely one extra read.
  ;;
  ;; Throttled on its own `:gateway-resynced-at-ms` stamp, never the
  ;; heartbeat's: a probe issued while the socket was down asked over a
  ;; failing connection, so its silence proves nothing and must not suppress
  ;; the one probe that can finally answer. The stamp still collapses the N
  ;; copies of one broadcast (the mux delivers a ready frame per open session
  ;; tab) into a single round.
  (fn [db [_ tab-id chunk now-ms]]
    (let
      [now
       (or now-ms (System/currentTimeMillis))

       tab-id
       (or tab-id (current-tab-id db))

       tab
       (db-for-tab db tab-id)

       painted
       (:gateway-turn-id tab)

       agrees?
       (boolean (and (:is-state-known chunk) painted (= painted (:gateway-turn-id chunk))))]

      (if (or agrees? (not (gateway-resync-probe-due? tab now)) (nil? (get-in tab [:session :id])))
        {:db db}
        {:db (update-tab db
                         tab-id
                         #(assoc %
                            :gateway-resynced-at-ms now
                            :liveness-probed-at-ms now))
         :fx [[:probe-turn-liveness tab-id (get-in tab [:session :id]) painted
               (:live-turn-client-id tab)]]}))))

(defn background-loading-tokens
  "Cancel tokens of every BACKGROUND tab (in `:tab-locals`, excluding the active
   tab held at the db root) whose turn is in flight. Ctrl+C quit consults these so
   a quit while other tabs are still working can warn + cancel them instead of
   orphaning their worker futures (orphans keep the JVM alive ~60s → looks frozen)."
  [db]
  (let [active (current-tab-id db)]
    (->> (:tab-locals db)
         (keep (fn [[tab-id snap]]
                 (when (and (not= tab-id active) (:loading? snap)) (:cancel-token snap))))
         vec)))

(defn any-background-loading?
  "True when a non-active tab has a turn in flight."
  [db]
  (boolean (seq (background-loading-tokens db))))

(reg-event-fx :cancel-all-turns
              ;; Cancel EVERY in-flight turn — the active tab (root :cancel-token) plus every
              ;; background tab in :tab-locals. Used by the Ctrl+C quit-confirm path so
              ;; quitting actually tears down all worker futures instead of leaving orphans
              ;; behind.
              (fn [db _]
                (doseq [tok (background-loading-tokens db)]
                  (try (vis/cancel! tok) (catch Throwable _ nil)))
                (when (:loading? db) (try (vis/cancel! (:cancel-token db)) (catch Throwable _ nil)))
                {:db (assoc db :cancelling? true)}))

(reg-event-db :set-progress-iterations
              (fn [db [_ a b]]
                (let [[workspace-id iterations] (if (keyword? a) [a b] [(current-tab-id db) a])]
                  (update-tab
                    db
                    workspace-id
                    (fn [workspace]
                      (if-not (:loading? workspace)
                        workspace
                        (assoc-in workspace [:progress :iterations] (vec (or iterations [])))))))))

(reg-event-fx
  :message-received
  (fn [db [_ a b c :as event]]
    (let
      [[event-workspace-id answer
        {:keys [status utilization client-turn-id terminal-sync? terminal-trace] :as completion}]
       (if (= 4 (count event)) [a b c] [(current-tab-id db) a b])

       workspace-id
       (or event-workspace-id (current-tab-id db))

       target
       (db-for-tab db workspace-id)

       matching-pending-index
       (when client-turn-id (pending-assistant-index (:messages target) client-turn-id))

       skip-identified-completion?
       (boolean (and client-turn-id (nil? matching-pending-index)))

       stale-generation?
       (boolean (and matching-pending-index
                     (:live-turn-client-id target)
                     (not= (str client-turn-id) (str (:live-turn-client-id target)))))

       drain?
       (volatile! false)

       restore-pending?
       (volatile! false)

       db'
       (update-tab
         db
         workspace-id
         (fn [workspace]
           (cond
             skip-identified-completion? workspace
             stale-generation? (assoc workspace
                                 :messages (replace-pending-assistant
                                             (:messages workspace)
                                             ;; A NEWER turn already owns the tab, so this
                                             ;; generation's `:progress` is gone. Settle it with
                                             ;; the trace parked on its own placeholder (issue
                                             ;; #61) — otherwise starting the next turn wipes
                                             ;; every iteration the user watched on this one.
                                             (completion-response
                                               answer
                                               (or terminal-trace
                                                   (not-empty (vec (get-in
                                                                     (vec (:messages workspace))
                                                                     [matching-pending-index
                                                                      :terminal-pending :trace]))))
                                               nil
                                               completion))
                                 :scroll scroll/follow)
             :else
             (let
               [trace
                (or terminal-trace
                    (not-empty (vec (get-in workspace [:progress :iterations])))
                    ;; The persistent mux settles this turn's active state the
                    ;; moment the terminal event lands — `:sync-turn-terminal`
                    ;; clears `:progress` up to `terminal-result-grace-ms` BEFORE
                    ;; the blocking worker delivers the full answer. The live
                    ;; trace it snapshotted survives on the placeholder; without
                    ;; this fallback every iteration the user watched LIVE is
                    ;; dropped and only the final answer stays on screen.
                    (let [ms (vec (:messages workspace))]
                      (not-empty (vec (get-in ms
                                              [(pending-assistant-index ms client-turn-id)
                                               :terminal-pending :trace])))))

                cancelled?
                (= :cancelled status)

                ;; Cancelling the local attach worker can synthesize this result before
                ;; the HTTP cancel reaches the daemon. Keep the turn gate armed until the
                ;; generation-matched :gateway-cancel-result confirms server acceptance;
                ;; otherwise an immediate resend is queued behind the still-running turn.
                awaiting-gateway-cancel?
                (boolean (and (not terminal-sync?)
                              cancelled?
                              (:cancelling? workspace)
                              (:cancelling-at-ms workspace)))

                ;; A cancellation that captured zero iterations is
                ;; usually a stray Esc - drop the placeholder pair
                ;; and restore the editor as before. A cancellation
                ;; with a non-empty trace means the agent already
                ;; did visible work (and persisted those iterations
                ;; to SQLite); KEEP the bubble so the user can read
                ;; what happened, and only repopulate the editor.
                no-work?
                (empty? trace)]

               (if (and cancelled? (:submitted-input workspace) no-work?)
                 (let [ws (restore-submitted-input workspace (:submitted-input workspace))]
                   ;; A cancel must NOT auto-send the backlog — pull it back into
                   ;; the editor instead (see :restore-pending-to-input).
                   ;; Restore it NOW, together with the prompt: the backlog is the same
                   ;; editor state, and deferring it to the ACK refilled the composer twice.
                   (when (some :mine? (:pending-sends ws)) (vreset! restore-pending? true))
                   ;; `restore-submitted-input` normally clears the completed local turn.
                   ;; A synthetic local cancellation is not completion of the SERVER turn:
                   ;; retain the exact cancellation generation until its gateway ACK arrives.
                   ;; ONLY the gate (see `cancel-gate-state-keys`) — the visible turn is over,
                   ;; so progress, the elapsed clock and the liveness probe stay cleared.
                   (if awaiting-gateway-cancel?
                     (merge ws (select-keys workspace cancel-gate-state-keys))
                     ws))
                 (let
                   [start
                    (:turn-start-ms workspace)

                    wall-ms
                    (when start (- (System/currentTimeMillis) (long start)))

                    content
                    (vec (or answer []))

                    response
                    (completion-response content trace wall-ms completion)

                    messages'
                    (replace-pending-assistant (:messages workspace) response)

                    still-pending?
                    (boolean (some pending-assistant-message? messages'))

                    workspace'
                    (cond->
                      (assoc workspace
                        ;; A final result replaces the live placeholder atomically.
                        ;; Preserve the painted row for one layout pass, then ease
                        ;; toward the newly measured tail instead of teleporting.
                        :messages messages'
                        :utilization utilization
                        :scroll (scroll/reveal
                                  (:scroll workspace)
                                  (max 0
                                       (- (long (or (get-in workspace [:layout :total-h]) 0))
                                          (long (or (get-in workspace [:layout :inner-h]) 0)))))
                        :loading? (or still-pending? awaiting-gateway-cancel?)
                        :cancelling? awaiting-gateway-cancel?
                        :cancelling-at-ms (when awaiting-gateway-cancel?
                                            (:cancelling-at-ms workspace)))
                      (and (not still-pending?) (not awaiting-gateway-cancel?))
                      clear-active-turn-state

                      ;; The cancelled bubble is already painted; only the send gate is
                      ;; still waiting on the ACK. Stop painting a live turn under it.
                      (and (not still-pending?) awaiting-gateway-cancel?)
                      (assoc :progress
                        nil :turn-start-ms
                        nil :liveness-probed-at-ms
                        nil))

                    ;; Cancelled-with-work: keep the bubble we just
                    ;; built AND refill the editor from the snapshot so
                    ;; the user can edit/resubmit the prompt that
                    ;; produced this trace without retyping.
                    ws-final
                    (if (and cancelled? (:submitted-input workspace) (not no-work?))
                      (restore-editor-only workspace' (:submitted-input workspace))
                      (cond-> workspace'
                        (not still-pending?)
                        (dissoc :submitted-input)))]

                   (when (and (or (not (:loading? ws-final)) awaiting-gateway-cancel?)
                              (seq (:pending-sends ws-final)))
                     ;; Normal completion drains the next queued turn; a cancel
                     ;; restores the AUTHORED backlog to the editor instead of
                     ;; firing it. Mirrored sibling entries are never restored
                     ;; (or deleted) here — see :restore-pending-to-input.
                     (if cancelled?
                       (when (some :mine? (:pending-sends ws-final))
                         (vreset! restore-pending? true))
                       (vreset! drain? true)))
                   ws-final))))))

       ;; A turn the GATEWAY started for this session while this tab was busy
       ;; (normally the queued message it drained on THIS turn's terminal),
       ;; parked by `:sibling-turn-started`. Its mirrored queue row is already
       ;; gone (`turn.queued.drained`), so without this replay the user's
       ;; message never appears anywhere. A drain scheduled below wins the race
       ;; and simply re-parks it for the next terminal.
       deferred
       (let [w (db-for-tab db' workspace-id)]
         (when-not (:loading? w) (:deferred-sibling-start w)))

       db'
       (cond-> db'
         deferred
         (update-tab workspace-id #(dissoc % :deferred-sibling-start)))]

      {:db (cond-> db'
             ;; Persistent unread dot: a BACKGROUND tab that just FINISHED a
             ;; turn (same gate as the bell) lights a dot that stays until the
             ;; user focuses it (cleared in `activate-tab`).
             (and (not skip-identified-completion?)
                  (not= workspace-id (current-tab-id db))
                  (not= :cancelled status))
             (update :tabs
                     (fn [entries]
                       (mapv (fn [entry]
                               (cond-> entry
                                 (= (:id entry) workspace-id)
                                 (assoc :unread? true)))
                             entries))))
       :fx (cond-> []
             @drain?
             (conj [:dispatch [:drain-pending workspace-id]])

             @restore-pending?
             (conj [:dispatch [:restore-pending-to-input workspace-id]])

             deferred
             (conj [:dispatch [:sibling-turn-started workspace-id deferred]]))})))

;;; ── Side effects ───────────────────────────────────────────────────────────

(reg-fx :dispatch
        (fn [event]
          (dispatch event)))

(reg-fx :settle-turn-terminal-later
        (fn [workspace-id terminal]
          (vis/worker-future "vis-tui-terminal-result-grace"
                             #(do (Thread/sleep (long terminal-result-grace-ms))
                                  (dispatch [:settle-turn-terminal workspace-id terminal])))))

(reg-fx :notify
        (fn [text level ttl-ms]
          (vis/notify! text :level level :ttl-ms ttl-ms)))

;; Flip a cycling registry toggle OUTSIDE the dispatch swap. `toggle-cycle-value!`
;; fires the registry listener synchronously and that listener dispatches back
;; into `app-db`; done inside the swap it livelocks the CAS retry loop (the
;; toggle advances once per retry). As an effect it runs exactly once, after the
;; state transition has committed.
(reg-fx :cycle-toggle
        (fn [toggle-id label]
          (let [next (vis/toggle-cycle-value! toggle-id)]
            ;; The listener wired in `init!` normally resyncs; dispatch it here
            ;; too so the projection is correct even without that wiring, and so
            ;; the id is carried through (render-neutral = no cache bust).
            (dispatch [:resync-toggle-settings toggle-id])
            (vis/notify! (str label ": " (name next))
                         :level :info
                         :ttl-ms settings-notification-ttl-ms))))

;; Persist the active session's model preference to the shared, channel-neutral
;; store. The engine reads it on the next turn (router-for-model) and the web
;; rail shows the same value — one source of truth across channels.
(reg-fx :set-session-model
        (fn [sid provider model]
          ;; A pick the GATEWAY refuses (a provider this gateway does not serve,
          ;; e.g. after a `/reload` dropped it) answers 400 — surface it instead
          ;; of letting the throw escape into the event loop.
          (try (vis/gateway-set-session-model! sid provider model)
               (catch Throwable t
                 ;; The optimistic `:session-model-pref` was already written by the
                 ;; dispatching event. Roll it back, or the chip keeps claiming a
                 ;; model this session will never route through.
                 (dispatch [:clear-session-model-pref sid])
                 (vis/notify! (str "Model switch failed: " (ex-message t)) :level :error)))
          ;; The background limits poller no longer resolves the active provider on
          ;; every 1s tick (issue #31); nudge it to re-resolve on its next tick so a
          ;; per-session model switch reflects in the footer's usage row promptly.
          (dispatch [:force-provider-limits-refresh])))

(reg-fx :bell
        ;; Write a raw BEL (0x07) to the terminal. BEL doesn't move the cursor, so
        ;; interleaving it with Lanterna's output is safe; the terminal turns it
        ;; into an audible/visible bell per the user's terminal settings.
        (fn []
          (try (when-let [^java.io.OutputStream out @vis/tty-out]
                 (.write out 7)
                 (.flush out))
               (catch Throwable _ nil))))

(reg-fx :apply-config
        (fn [config]
          (let
            [raw
             (or (vis/load-config-raw) {})

             persistent
             (assoc raw "providers" (vec (:providers config)))]

            (vis/save-config! persistent)
            (let
              [resolved
               (or (vis/reload-config!) config)

               router
               (vis/rebuild-router! resolved)]

              (vis/refresh-cached-routers! router)))))

(defn- gateway-disconnect-data
  "Return gateway disconnect metadata from a throwable cause chain."
  [t]
  (loop [^Throwable cause t]
    (when cause
      (let [data (ex-data cause)]
        (if (:gateway-disconnected data) data (recur (.getCause cause)))))))

(reg-fx
  :session-turn
  (fn
    [workspace-id session text token reasoning-level extra-body turn-features workspace
     client-turn-id & [display-text]]
    (let
      [fut
       (vis/worker-future
         "vis-tui-turn"
         (fn []
           (try
             (let
               [progress-update! (make-progress-render-updater (fn [[_ timeline]]
                                                                 (try (dispatch
                                                                        [:set-progress-iterations
                                                                         workspace-id timeline])
                                                                      (catch Throwable _ nil))))
                {track-chunk :on-chunk} (vis/make-progress-tracker {:on-update progress-update!})
                ;; LIVE F2 context dialog: every `:iteration-final`
                ;; chunk carries the working-memory snapshot
                ;; (`:tasks`/`:facts`) from the loop's live ctx-atom.
                ;; Push it to `:ctx-by-session` mid-turn so the panel
                ;; reflects task/fact writes as they happen — not only
                ;; after the turn ends (the turn-end DB reload still
                ;; runs in `:message-received` as the durable sync).
                ;; The `dispatch` bumps `:render-version`, so an open
                ;; overlay repaints with the fresh snapshot.
                sid (:id session)
                on-chunk
                (fn [chunk]
                  (case (:phase chunk)
                    ;; A sibling queued/edited/deleted a message
                    ;; on this session — mirror it into the local
                    ;; queue; never a progress chunk.
                    :queue-sync
                    (try (dispatch [:sync-queued-turn workspace-id chunk]) (catch Throwable _ nil))

                    ;; Queue paused/resumed after a provider failure.
                    :queue-paused
                    (try (dispatch [:sync-queue-paused workspace-id chunk]) (catch Throwable _ nil))

                    ;; The turn actually STARTED running — re-seed
                    ;; this tab's elapsed clock from the gateway's
                    ;; canonical started_at; never a progress chunk.
                    :turn-start
                    (try (dispatch [:sync-turn-clock workspace-id chunk]) (catch Throwable _ nil))

                    (do (when (and sid
                                   (= :iteration-final (:phase chunk))
                                   (or (:tasks chunk) (:facts chunk)))
                          (try (dispatch [:set-ctx-panel sid
                                          {:tasks (:tasks chunk) :facts (:facts chunk)}])
                               (catch Throwable _ nil)))
                        (track-chunk chunk))))
                result (chat/turn! session
                                   text
                                   {:on-chunk on-chunk
                                    ;; Pass the cancellation TOKEN, not the
                                    ;; bare atom: the loop registers Python /
                                    ;; provider workers with the token's
                                    ;; `on-cancel!` callback registry so
                                    ;; `vis/cancel!` hard-cancels them all
                                    ;; at once instead of waiting on each
                                    ;; one's eval-timeout.
                                    :cancel-token token
                                    ;; Correlation id: makes a resubmit idempotent AND
                                    ;; comes back on any queue event for this turn, so
                                    ;; the tab recognises its own submit by id.
                                    :idempotency-key client-turn-id
                                    :reasoning-default reasoning-level
                                    :extra-body extra-body
                                    :turn-features turn-features
                                    :workspace workspace
                                    :display-text display-text})]

               (if (get result "error")
                 (dispatch [:message-received workspace-id (chat/error-content result)
                            {:client-turn-id client-turn-id}])
                 (do (dispatch
                       [:message-received workspace-id (get result "content")
                        ;; Field-by-field pick from the canonical string-keyed
                        ;; gateway result into the TUI's internal message map —
                        ;; never a blanket re-keying of wire data.
                        {:model (get result "model")
                         :provider (get result "provider")
                         :llm-selected (get result "llm_selected")
                         :llm-actual (get result "llm_actual")
                         :llm-fallback? (get result "is_llm_fallback")
                         :llm-routing-trace (get result "llm_routing_trace")
                         :iteration-count (get result "iteration_count")
                         :duration-ms (get result "duration_ms")
                         :tokens (get result "tokens")
                         :cost (get result "cost")
                         :confidence (get result "confidence")
                         :session-turn-id (get result "session_turn_id")
                         :status (case (get result "status")
                                   "needs_input"
                                   :needs-input

                                   "cancelled"
                                   :cancelled

                                   nil)
                         :utilization (get result "utilization")
                         :slash (get result "slash")
                         :client-turn-id client-turn-id}])
                     ;; A turn may have switched the session's workspace
                     ;; (`/draft new | apply | abandon`, `/cd <path>`).
                     ;; Re-sync so header/footer reflect it. The gateway ws
                     ;; fact already carries the server-resolved :git status,
                     ;; so re-dispatch it — no client-side git walk here.
                     (try (let
                            [sid (some-> session
                                         :id)
                             ws (when sid (vis/gateway-session-workspace sid))]

                            (dispatch [:set-workspace ws workspace-id]))
                          (catch Throwable _ nil))
                     ;; W3: refresh the F2 context panel's snapshot from the
                     ;; just-completed turn's ctx (tasks + facts). One DB read
                     ;; at turn end (NOT per-paint); the overlay renders from
                     ;; this cache.
                     (try (when-let [sid (:id session)]
                            (dispatch [:set-ctx-panel sid {}]))
                          (catch Throwable _ nil)))))
             (catch Throwable t
               (if-let [disconnect (gateway-disconnect-data t)]
                 (if-let [tid (:turn-id disconnect)]
                   (dispatch [:reattach-disconnected-turn workspace-id session tid token
                              client-turn-id])
                   (let
                     [message (vis/format-error (or (ex-message t) (str t)))
                      block {"id" (str (java.util.UUID/randomUUID))
                             "type" "error"
                             "code" "turn_failed"
                             "message" message}]

                     (dispatch [:message-received workspace-id [block]
                                {:client-turn-id client-turn-id}])))
                 (let
                   [cancelled? (vis/cancellation? t)
                    message (if cancelled?
                              "Cancelled by user."
                              (vis/format-error (or (ex-message t) (str t))))
                    block {"id" (str (java.util.UUID/randomUUID))
                           "type" (if cancelled? "notice" "error")
                           "code" (if cancelled? "turn_cancelled" "turn_failed")
                           "message" message}]

                   (dispatch [:message-received workspace-id [block]
                              (cond-> {:client-turn-id client-turn-id}
                                cancelled?
                                (assoc :status :cancelled))])))))))]
      (vis/cancellation-set-future! token fut))))

(reg-fx
  :session-attach
  (fn [workspace-id session tid token client-turn-id]
    (let
      [fut
       (vis/worker-future
         "vis-tui-attach"
         (fn []
           (try
             (let
               [progress-update! (make-progress-render-updater (fn [[_ timeline]]
                                                                 (try (dispatch
                                                                        [:set-progress-iterations
                                                                         workspace-id timeline])
                                                                      (catch Throwable _ nil))))
                {track-chunk :on-chunk} (vis/make-progress-tracker {:on-update progress-update!})
                sid (:id session)
                on-chunk
                (fn [chunk]
                  (case (:phase chunk)
                    ;; A sibling queued/edited/deleted a message
                    ;; on this session — mirror it into the local
                    ;; queue; never a progress chunk.
                    :queue-sync
                    (try (dispatch [:sync-queued-turn workspace-id chunk]) (catch Throwable _ nil))

                    ;; Queue paused/resumed after a provider failure.
                    :queue-paused
                    (try (dispatch [:sync-queue-paused workspace-id chunk]) (catch Throwable _ nil))

                    ;; The turn actually STARTED running — re-seed
                    ;; this tab's elapsed clock from the gateway's
                    ;; canonical started_at; never a progress chunk.
                    :turn-start
                    (try (dispatch [:sync-turn-clock workspace-id chunk]) (catch Throwable _ nil))

                    (do (when (and sid
                                   (= :iteration-final (:phase chunk))
                                   (or (:tasks chunk) (:facts chunk)))
                          (try (dispatch [:set-ctx-panel sid
                                          {:tasks (:tasks chunk) :facts (:facts chunk)}])
                               (catch Throwable _ nil)))
                        (track-chunk chunk))))
                result (chat/attach! session tid {:on-chunk on-chunk})]

               (if (get result "error")
                 (dispatch [:message-received workspace-id (chat/error-content result)
                            {:client-turn-id client-turn-id}])
                 (do (dispatch
                       [:message-received workspace-id (get result "content")
                        ;; Same field-by-field pick as :session-turn (above).
                        {:model (get result "model")
                         :provider (get result "provider")
                         :llm-selected (get result "llm_selected")
                         :llm-actual (get result "llm_actual")
                         :llm-fallback? (get result "is_llm_fallback")
                         :llm-routing-trace (get result "llm_routing_trace")
                         :iteration-count (get result "iteration_count")
                         :duration-ms (get result "duration_ms")
                         :tokens (get result "tokens")
                         :cost (get result "cost")
                         :confidence (get result "confidence")
                         :session-turn-id (get result "session_turn_id")
                         :status (case (get result "status")
                                   "needs_input"
                                   :needs-input

                                   "cancelled"
                                   :cancelled

                                   nil)
                         :utilization (get result "utilization")
                         :slash (get result "slash")
                         :client-turn-id client-turn-id}])
                     (try (let
                            [sid (some-> session
                                         :id)
                             ws (when sid (vis/gateway-session-workspace sid))]

                            (dispatch [:set-workspace ws workspace-id]))
                          (catch Throwable _ nil))
                     (try (when-let [sid (:id session)]
                            (dispatch [:set-ctx-panel sid {}]))
                          (catch Throwable _ nil)))))
             (catch Throwable t
               (if (gateway-disconnect-data t)
                 (dispatch [:reattach-disconnected-turn workspace-id session tid token
                            client-turn-id])
                 (let
                   [cancelled? (vis/cancellation? t)
                    message (if cancelled?
                              "Cancelled by user."
                              (vis/format-error (or (ex-message t) (str t))))
                    block {"id" (str (java.util.UUID/randomUUID))
                           "type" (if cancelled? "notice" "error")
                           "code" (if cancelled? "turn_cancelled" "turn_failed")
                           "message" message}]

                   (dispatch [:message-received workspace-id [block]
                              (cond-> {:client-turn-id client-turn-id}
                                cancelled?
                                (assoc :status :cancelled))])))))))]
      (vis/cancellation-set-future! token fut))))

;; ── Gateway queue I/O ──────────────────────────────────────────────────────
;;
;; Every gateway call in this section is a BLOCKING HTTP round-trip, and
;; `dispatch` runs effects on the thread that dispatched — for a submission that
;; is the TUI's INPUT thread. Inline, one unreachable daemon froze the editor for
;; the whole `ensure-gateway!` respawn wait plus the request timeout: keys
;; ignored, nothing on screen, no way to tell a slow send from a dead one. One
;; FIFO thread fixes both halves at once — the input thread never waits on the
;; network, and queue mutations still reach the daemon in the order they were
;; typed (an add and its delete must not invert).
(def ^:private gateway-queue-executor
  (delay (java.util.concurrent.Executors/newSingleThreadExecutor
           (reify
             java.util.concurrent.ThreadFactory
               (newThread [_ r]
                 (doto (Thread. ^Runnable r "vis-tui-gateway-queue") (.setDaemon true)))))))

(defn- gateway-queue-io!
  "Run `f` on the single FIFO gateway-queue thread. Returns its Future so a
   caller (or a test) can await the round-trip; the TUI never does."
  [f]
  (.submit ^ExecutorService @gateway-queue-executor ^Runnable f))

(def ^:private gateway-cancel-executor
  ;; Cancellation must bypass the FIFO submission lane: that lane may itself be
  ;; waiting on the request we need to abort. A cached daemon pool also prevents
  ;; one unreachable gateway (30s HTTP timeout) from delaying a later Esc.
  (delay (Executors/newCachedThreadPool (reify
                                          ThreadFactory
                                            (newThread [_ r]
                                              (doto (Thread. ^Runnable r "vis-tui-gateway-cancel")
                                                (.setDaemon true)))))))

(defn- gateway-cancel-io! [f] (.submit ^ExecutorService @gateway-cancel-executor ^Runnable f))

(def ^:private gateway-queue-attempts
  "Total submit attempts before a queued submission falls back to a local row."
  3)

(def ^:private gateway-queue-retry-ms 400)

(defn- submit-queued-turn!
  "Submit `opts` for `sid`, retrying a TRANSPORT failure. Repeating is safe by
   construction, not by hope: `:idempotency-key` is the gateway's dedup key, so a
   resubmit whose first response was lost returns THE SAME turn (HTTP 200)
   instead of queueing a second one. That is what carries a submission across the
   seconds a killed daemon needs to respawn. Throws the last failure once the
   attempts are spent — the caller then stages the text locally."
  [sid opts]
  (loop [attempt 1]
    (let [res (try {:ok (vis/gateway-submit-turn! sid opts)} (catch Throwable t {:err t}))]
      (cond (contains? res :ok) (:ok res)
            (>= (long attempt) (long gateway-queue-attempts)) (throw (:err res))
            :else (do (Thread/sleep (* (long attempt) (long gateway-queue-retry-ms)))
                      (recur (inc (long attempt))))))))

(reg-fx :gateway-enqueue
        ;; Register a busy-time submission as a REAL gateway queued turn (the ONE queue
        ;; of record) and paint the row from the ACK, through the same
        ;; `:sync-queued-turn` writer the `turn.queued` broadcast feeds and keyed by the
        ;; same gateway turn id — so whichever lands first wins and the other is a
        ;; no-op. Nothing is invented locally: if the gateway STARTED the turn instead
        ;; of queueing it (the session went idle in the round-trip) there is no queued
        ;; row at all and the attach machinery renders it as the live turn. Only a
        ;; submission that never reached the queue is staged locally, so no text is lost.
        ;; Runs on the FIFO queue thread (see above): pressing Enter is never blocked by
        ;; the network.
        (fn [workspace-id session entry reasoning-level extra-body turn-features workspace]
          (let
            [sid
             (:id session)

             client-id
             (:client-id entry)

             agent-text
             (:agent-text entry)

             ;; The COLLAPSED copy: paste/image tokens still folded into their
             ;; `vis-paste`/`vis-image` fences. Sent so the queued record — and the
             ;; `turn.started` every channel replays from — carries what the USER
             ;; wrote, not the expanded agent text whose image path is a raw
             ;; `/var/folders/…/clipboard-….png`.
             display-text
             (let [p (:preview-text entry)]
               (when (and p (not= p agent-text)) p))

             ;; RESCUE: the submission never reached the queue of record. Keep the
             ;; text as a local row (marked `:unsent?`, so the strip says so rather
             ;; than implying the server has it) and nudge the drain — the turn we
             ;; queued BEHIND may well have finished while this round-trip was
             ;; failing, and its terminal (the only other thing that pops the queue)
             ;; has already passed. `:drain-pending` is a no-op while still busy.
             stage!
             (fn []
               (dispatch [:stage-queued-locally workspace-id entry])
               (try (dispatch [:drain-pending workspace-id]) (catch Throwable _ nil)))]

            (when sid
              (gateway-queue-io!
                (fn []
                  (try (let
                         [res
                          (submit-queued-turn! sid
                                               (cond->
                                                 {:request agent-text
                                                  ;; The gateway echoes this back on
                                                  ;; turn.queued and every wire view of the
                                                  ;; turn: the ONE key that tells us the row
                                                  ;; it broadcasts is our own submission —
                                                  ;; and the key that makes the retry above
                                                  ;; idempotent.
                                                  :idempotency-key client-id}
                                                 reasoning-level
                                                 (assoc :reasoning-default reasoning-level)

                                                 extra-body
                                                 (assoc :extra-body extra-body)

                                                 (seq turn-features)
                                                 (assoc :turn-features turn-features)

                                                 display-text
                                                 (assoc :display-request display-text)

                                                 (seq workspace)
                                                 (assoc :workspace workspace)))

                          turn
                          (:turn res)

                          tid
                          (get turn "turn_id")]

                         (cond (and tid (= "queued" (get turn "status")))
                               (dispatch [:sync-queued-turn workspace-id
                                          {:op :add
                                           :turn-id tid
                                           :client-id client-id
                                           :text (or (get turn "request") agent-text)
                                           :preview-text (or (get turn "request_preview")
                                                             display-text
                                                             (get turn "request")
                                                             agent-text)}])
                               ;; Accepted but already RUNNING: not a queue row.
                               tid nil
                               :else (stage!)))
                       (catch Throwable t
                         (stage!)
                         (try (vis/notify! (str "Queueing failed — kept locally: "
                                                (or (ex-message t) (str t)))
                                           :level :warn
                                           :ttl-ms 3000)
                              (catch Throwable _ nil)))
                       ;; Release the double-submit guard on the ONE path every outcome
                       ;; passes through, so an identical follow-up message is never
                       ;; swallowed once this round-trip is over.
                       (finally (dispatch [:submission-settled workspace-id client-id])))))))))

(reg-fx :gateway-delete-queued
        ;; Drop a gateway queued record. The local row is removed by the caller as a
        ;; fast echo, so this RECONCILES against gateway truth: when the delete never
        ;; landed (transport failure, or the daemon rejecting it while the record
        ;; still sits in the queue) the row is written back through the one
        ;; `:sync-queued-turn` writer — otherwise the turn would still auto-drain
        ;; server-side with nothing on screen to say so. A 404 (already gone) and a
        ;; 409 (already started — the attach machinery paints it) stay removed.
        (fn [sid tid workspace-id entry]
          (when (and sid tid)
            (gateway-queue-io!
              (fn []
                (let
                  [failed? (try (not= "deleted"
                                      (get (vis/gateway-delete-queued-turn! sid tid) "status"))
                                (catch Throwable t
                                  (not (contains? #{404 409} (:http-status (ex-data t))))))]
                  (when (and failed? workspace-id)
                    (dispatch [:sync-queued-turn workspace-id
                               {:op :add
                                :turn-id tid
                                :text (:text entry)
                                :preview-text (:preview-text entry)
                                :client-id (:client-id entry)
                                :mine? (:mine? entry)}]))))))))

(reg-fx :submit-orphan-sends
        ;; A closing tab still held AUTHORED submissions that never reached the
        ;; gateway (no :turn-id). Submit each to the gateway — the server-side queue
        ;; of record — so the text survives the tab close: it runs/queues under the
        ;; session and is visible on the next reattach. Best effort off the input
        ;; thread; a failure surfaces as a warning notification, never a throw.
        (fn [sid texts]
          (gateway-queue-io! (fn []
                               (doseq [text texts]
                                 (try (vis/gateway-submit-turn! sid {:request text})
                                      (catch Throwable t
                                        (try (vis/notify! (str "Re-queue of unsent message failed: "
                                                               (or (ex-message t) (str t)))
                                                          :level :warn
                                                          :ttl-ms 3000)
                                             (catch Throwable _ nil)))))))))

(reg-fx :cancel-local-turn
        ;; Fast and synchronous by design: flip the cooperative flag and interrupt
        ;; every registered worker before any network work starts.
        (fn [token]
          (try (vis/cancel! token) (catch Throwable _ nil))))

(reg-fx :gateway-cancel-active
        ;; Best-effort server cancel on a dedicated lane. Never run gateway discovery
        ;; or HTTP on the Lanterna input/render thread. The generation-keyed result
        ;; event may release a first cancel early, without touching a newer turn.
        (fn [sid tid cancel-key]
          (when sid
            (gateway-cancel-io! (fn []
                                  (let [result (gateway-cancel-turn-or-current! sid tid)]
                                    (when cancel-key
                                      (dispatch [:gateway-cancel-result cancel-key result]))))))))

(def ^:private terminal-turn-statuses #{"completed" "failed" "cancelled"})

(reg-fx :probe-turn-liveness
        ;; Ask the gateway registry whether the turn this tab still paints as live has
        ;; actually settled (see `:turn-liveness-tick`). One cheap listing on the queue
        ;; lane — never on the Lanterna input/render thread. A settled row is replayed
        ;; through `:sync-turn-terminal`, the exact writer the real terminal event uses,
        ;; so a late/duplicate event is a harmless no-op (that handler bails unless the
        ;; tab is still loading on a MATCHING turn). Anything else — a still-running
        ;; turn, an unreachable daemon — leaves the live view untouched.
        (fn [workspace-id sid tid client-id]
          (when (and sid tid)
            (gateway-queue-io!
              (fn []
                (when-let [turns (try (vis/gateway-list-turns sid) (catch Throwable _ nil))]
                  (when-let
                    [row (some (fn [t]
                                 (when (or (= (str tid) (str (get t "turn_id")))
                                           (and client-id
                                                (= (str client-id)
                                                   (str (get t "idempotency_key")))))
                                   t))
                               turns)]
                    (let [status (str (get row "status"))]
                      (when (contains? terminal-turn-statuses status)
                        (dispatch [:sync-turn-terminal workspace-id
                                   {:phase :turn-terminal
                                    :turn-id (or (get row "turn_id") tid)
                                    :client-id (or (get row "idempotency_key") client-id)
                                    :status status
                                    ;; The watchdog gets a FULL turn row, unlike the lean
                                    ;; terminal SSE event. Preserve a failed turn's canonical
                                    ;; error blocks so its fallback paints the same provider
                                    ;; card as live delivery and reload.
                                    :content (vec (or (get row "content") []))}]))))))))))

(reg-fx :gateway-cancel-turn
        ;; Fire-and-forget cancel of the exact RUNNING gateway turn whose correlated
        ;; turn.started completed an Esc-before-bind cancellation.
        (fn [sid tid]
          (when (and sid tid)
            (gateway-cancel-io! (fn []
                                  (try (vis/gateway-cancel-turn! sid tid)
                                       (catch Throwable _ nil)))))))

(reg-fx :drain-idle-queue
        ;; Kick a server-side queued backlog into motion for an IDLE session on
        ;; open/resume: the daemon starts the head queued turn (no-op if one is
        ;; already running) and emits turn.started, which the tab's event
        ;; subscription turns into :sibling-turn-started -> :attach-running-turn.
        ;; Best-effort; a stopped daemon or lost race simply leaves it queued.
        (fn [sid]
          (when sid
            (gateway-queue-io! (fn []
                                 (try (vis/gateway-drain-idle! sid) (catch Throwable _ nil)))))))

(reg-fx :gateway-close-session
        (fn [sid]
          (when sid (try (vis/gateway-close-session! sid) (catch Throwable _ nil)))))

(reg-fx :release-session-runtime
        ;; Stop a session's live daemon runtime + background children (`shell` op "background",
        ;; managed REPLs) WITHOUT dropping the process client lease — fired by
        ;; `:close-tab` when the LAST view of an idle session closes. Keeps the
        ;; transcript resumable; best-effort, never daemon-spawning.
        (fn [sid]
          (when sid (try (vis/gateway-release-session-runtime! sid) (catch Throwable _ nil)))))

(reg-fx :unassign-session-project
        ;; Tabs ARE the launch project's member sessions, so an explicit tab
        ;; close drops that session from the project (SET NULL — the session is
        ;; NOT deleted; it lingers loose and is reachable via the navigator).
        ;; A project SWITCH keeps membership (see :close-tab keep-project?).
        (fn [sid]
          (when sid (try (vis/gateway-assign-project! sid nil) (catch Throwable _ nil)))))
