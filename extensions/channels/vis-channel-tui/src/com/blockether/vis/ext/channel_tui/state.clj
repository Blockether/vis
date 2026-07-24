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
  (:import [java.util.concurrent Executors ScheduledExecutorService TimeUnit]))

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
  [:session :workspace :workspace/root :title :messages :utilization :scroll :input :input-history
   :input-history-index :input-history-draft :slash-command-index :slash-command-hidden?
   :submitted-input :pending-sends :pastes :paste-counter :loading? :cancel-token :cancelling?
   :progress :turn-start-ms :detail-expansions :mouse-selection :session-model-pref])

(defn- empty-tab-state
  []
  {:session nil
   :workspace nil
   :workspace/root nil
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
   :pending-sends []
   :pastes {}
   :paste-counter 0
   :loading? false
   :cancel-token nil
   :cancelling? false
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
        (let
          [old-db @app-db
           {:keys [db fx]} ((:fn handler) old-db event-vec)]

          (if db
            (let
              [db' (finalize-db db)
               eff? (decide-bump old-db db')]

              (vreset! bumped? eff?)
              (reset! app-db (if eff? (bump-version db') db')))
            ;; Pure-effect handler (no :db): preserve the prior contract —
            ;; notify whenever the event is allowed to bump at all.
            (vreset! bumped? allow-bump?))
          (doseq [[fx-id & args] fx]
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

(defn- replace-pending-assistant
  "Replace the pending assistant slot for a completed turn. Prefer the
   stable client turn id; fall back to the oldest pending placeholder for
   older events/tests. Never pop the tail: newer user messages may already
   have been appended while this turn was still live."
  [messages response]
  (let
    [messages
     (vec (or messages []))

     client-turn-id
     (:client-turn-id response)

     response
     (dissoc response :pending?)

     idx
     (or (when client-turn-id
           (first (keep-indexed (fn [idx m]
                                  (when (and (pending-assistant-message? m)
                                             (= client-turn-id (:client-turn-id m)))
                                    idx))
                                messages)))
         (first (keep-indexed (fn [idx m]
                                (when (pending-assistant-message? m) idx))
                              messages)))

     carry-slash
     (fn [old resp]
       (cond-> resp
         (and (not (contains? resp :slash?)) (:slash? old))
         (assoc :slash? true)))]

    (cond idx (assoc messages idx (carry-slash (get messages idx) response))
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
  "Pull the per-tab locals for `workspace-id` back into the active db.\n\n   Also clears two DERIVED/display fields that belong to the tab we are\n   LEAVING, not the one we are entering:\n\n   - `:layout` is a single top-level value the render thread computes for the\n     CURRENT tab's messages (`:total-h`, `:offsets`). It is not per-tab, so on a\n     switch it still describes the old tab. The first post-switch frame would\n     clamp the new tab's scroll against that stale document height (and feed the\n     old `:offsets` as `:prev-offsets`) → the viewport lurches, then the next\n     frame recomputes correctly. Dropping it forces a clean recompute for THIS\n     tab before any clamp.\n   - `:pos` on `:scroll` is the eased on-screen row, anchored to the old layout.\n     Stripping it makes a restored FOLLOW tab re-snap to its own bottom instead\n     of inheriting the previous tab's pinned bottom row, and lets a parked tab\n     re-resolve its `:offset` against the fresh layout."
  [db workspace-id]
  (let
    [entry
     (some #(when (= (:id %) workspace-id) %) (:tabs db))

     db'
     (-> (merge db (or (get-in db [:tab-locals workspace-id]) (empty-tab-state)))
         (dissoc :layout)
         (update :scroll
                 (fn [sc]
                   (if (map? sc) (dissoc sc :pos) sc))))]

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
  [db workspace-id f]
  (let [workspace-id (or workspace-id (current-tab-id db))]
    (if (and workspace-id (not= workspace-id (current-tab-id db)))
      (update-in db
                 [:tab-locals workspace-id]
                 (fn [snapshot]
                   (tab-snapshot (f (merge db (or snapshot (empty-tab-state)))))))
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

(reg-event-db :resync-toggle-settings
              ;; Triggered by the toggles-registry listener whenever a flip
              ;; happens (settings dialog row, programmatic vis/toggle-set-value!,
              ;; provider-side cycle event). Rebuilds the cached `:settings`
              ;; projection so consumers reading `(get settings :show-thinking)`
              ;; etc. observe the new value on the very next paint.
              (fn [db _]
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
                (render/invalidate-cache!)
                (virtual/invalidate-heights!)
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
                  (when-let [cols (:cols (:layout db))]
                    (virtual/rewarm! (:messages db)
                                     (max 1 (- (long cols) (long render/MESSAGE_SIDE_PAD)))
                                     settings
                                     {:session-id (get-in db [:session :id])
                                      :detail-expansions (:detail-expansions db)
                                      :on-warm #(dispatch [:bump-render-version])}))
                  (assoc db :settings settings))))

(reg-event-fx :cycle-reasoning-level
              (fn [db _]
                (if-not (reasoning-effort-configurable?)
                  {:db db
                   :fx [[:notify "Reasoning effort is not configurable for this model" :warn
                         settings-notification-ttl-ms]]}
                  (let [next (vis/toggle-cycle-value! "reasoning_level")]
                    ;; The toggle listener wired in `init!` will fire next; this
                    ;; just refreshes the cached :settings projection here too
                    ;; so the FX :db ends up consistent within the same tick.
                    {:db (apply-settings-update! db {})
                     :fx [[:notify (str "Reasoning: " (name next)) :info
                           settings-notification-ttl-ms]]}))))

(reg-event-fx :cycle-codex-verbosity
              (fn [db _]
                (if-not (= :openai-codex (current-provider-id db))
                  {:db db
                   :fx [[:notify "Codex verbosity is only available for OpenAI Codex" :warn
                         settings-notification-ttl-ms]]}
                  (let [next (vis/toggle-cycle-value! "openai_codex_verbosity")]
                    {:db (apply-settings-update! db {})
                     :fx [[:notify (str "Codex verbosity: " (name next)) :info
                           settings-notification-ttl-ms]]}))))

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

(reg-event-db :set-layout
              (fn [db [_ layout]]
                ;; Pushed in by the render thread; intentionally does NOT bump
                ;; render-version (see no-render-bump-events).
                (assoc db :layout layout)))

(defn- park-scroll-for-toggle
  "Pin the viewport before a disclosure toggle mutates message heights.

   In FOLLOW mode the painter pins the BOTTOM of the transcript, so
   expanding a fold pushes every row above it UP by the body height — the
   clicked row runs away from the cursor (worse the bigger the result
   body). Parking at the currently painted offset flips the pin to the
   TOP-of-viewport message (the layout's prev-offsets anchoring), so the
   clicked row stays put and the body grows DOWNWARD off-screen — the
   browser `<details>` behaviour. Already-parked scrolls are left alone;
   they anchor correctly as-is. No-op before the first paint (no layout)."
  [db]
  (let [eff (get-in db [:layout :eff-scroll])]
    (if (and (not (scroll/scrolled-up? (:scroll db))) (some? eff))
      (assoc db :scroll (scroll/parked (long eff)))
      db)))

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
  ;; daemon to stop that session's shell_bg children / REPLs and drop its
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

(reg-event-db :set-provider-limits
              (fn [db [_ provider-id report]]
                (assoc db
                  :provider-limits {:provider-id provider-id
                                    :report report
                                    :updated-at-ms (System/currentTimeMillis)}
                  :provider-limits-force? false)))

(reg-event-db :clear-provider-limits
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
  [:loading? :cancelling? :cancelling-at-ms :progress :turn-start-ms :cancel-token
   :gateway-turn-id])

(defn- session-running?
  [session]
  (or (= "running" (:status session))
      (= :running (:status session))
      (some? (:current_turn_id session))
      (some? (:current-turn-id session))))

(defn- clear-active-turn-state
  [db]
  (assoc db
    :loading? false
    :cancelling? false
    :progress nil
    :turn-start-ms nil
    :cancel-token nil
    :gateway-turn-id nil
    :cancelling-at-ms nil))

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
              ;; Sessions already open in a tab and anything past `max-tabs` are
              ;; skipped. Locals seed with an empty view so an early switch paints a
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

                        (if (or (nil? sid) open? (>= (count entries) max-tabs))
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
  "Drop the pending turn pair AND repopulate the editor. Used when
   a turn was cancelled before any iteration produced visible work
   - the user pressed Esc fast, no trace exists, so dropping the
   placeholder bubble keeps the transcript clean."
  [db {:keys [text pastes paste-counter]}]
  (let [visible-text (input/expand-paste-placeholders text pastes)]
    (-> db
        clear-active-turn-state
        (assoc :messages (drop-pending-turn-messages (:messages db))
               :scroll scroll/follow
               :input (text->input-state text)
               :input-history-index nil
               :input-history-draft nil
               :slash-command-index 0
               :slash-command-hidden? false
               :pastes (or pastes {})
               :paste-counter (or paste-counter 0))
        (update :input-history
                (fn [xs]
                  (let [xs (vec (or xs []))]
                    (if (= visible-text (peek xs)) (pop xs) xs))))
        (dissoc :turn-start-ms :submitted-input))))

(defn- restore-editor-only
  "Repopulate the editor without touching `:messages`. Used when
   a turn was cancelled AFTER iterations produced visible work -
   we keep the cancelled bubble (with its `:traces`) in the
   transcript so the user can see what the agent did, AND we
   refill the input box with the original prompt so they can
   tweak/resubmit without retyping. The session-turn row,
   each completed iteration, and any blocks they wrote already
   landed in SQLite via the iteration loop's per-iteration
   `db-store-iteration!` calls and `finalize-turn-result`'s
   `db-update-session-turn!`, so reopening the session
   shows the same partial trace."
  [db {:keys [text pastes paste-counter]}]
  ;; See note in restore-submitted-input: leave :input-history alone.
  (-> db
      (assoc :input (text->input-state text)
             :input-history-index nil
             :input-history-draft nil
             :slash-command-index 0
             :slash-command-hidden? false
             :pastes (or pastes {})
             :paste-counter (or paste-counter 0))
      (dissoc :submitted-input)))

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
                 (conj [:gateway-delete-queued sid tid]))})
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

(reg-event-db :scroll-to-top
              ;; Emacs M-< (beginning-of-buffer): park at the very top. The layout
              ;; clamps the offset, so row 0 is the first message.
              (fn [db _]
                (let
                  [pre
                   (scroll-pre! db)

                   sc
                   (scroll/parked 0)]

                  (log-scroll! :scroll-to-top pre (scroll-snapshot sc) {})
                  (assoc db :scroll sc))))

(reg-event-db :reanchor-scroll
              ;; Scroll-anchoring write-back from the render thread. `anchored` is the
              ;; corrected absolute on-screen row; `delta` is how far content ABOVE the
              ;; anchor changed height as off-screen estimates resolved. Shift the
              ;; concrete fields so the anchored message stays visually put (no lurch).
              ;; FOLLOW-at-bottom feeds nil to the layout and never dispatches this.
              (fn [db [_ anchored delta]]
                (assoc db
                  :scroll (scroll/reanchor (:scroll db) (long anchored) (long (or delta 0))))))

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

(reg-event-db :scroll-up
              ;; Wheel / arrow / PageUp: park `amount` rows above the current row and
              ;; ease there. Scrolling up is always a deliberate read-history intent
              ;; (mode :at), so the streaming follow hands off automatically.
              (fn [db [_ amount total-h inner-h]]
                (let
                  [max-s
                   (max 0 (- (long total-h) (long inner-h)))

                   pre
                   (scroll-pre! db)

                   sc
                   (scroll/up (:scroll db) (long amount) max-s)]

                  (log-scroll! :scroll-up pre (scroll-snapshot sc) {:amount amount :max-s max-s})
                  (assoc db :scroll sc))))

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

(defn- db-for-tab
  [db workspace-id]
  (if (= workspace-id (current-tab-id db))
    db
    (merge db (or (get-in db [:tab-locals workspace-id]) (empty-tab-state)))))

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

     dup?
     (= text (:text (peek (vec (or (:pending-sends source-db) [])))))]

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
         (str (java.util.UUID/randomUUID))

         entry
         {:text text
          :preview-text preview-text
          :agent-text agent-text
          :client-id client-id
          :pastes pastes
          :paste-counter (:paste-counter source-db)
          :queued-at-ms (System/currentTimeMillis)}

         ;; RACE GUARD: never register a SERVER-SIDE queued turn while a
         ;; cancel is in flight (`:cancelling?`). A cancel restores the whole
         ;; backlog to the editor (:restore-pending-to-input) instead of
         ;; draining it — but that restore deletes gateway records by
         ;; :turn-id, and the enqueue's turn-id is bound LATE by an async
         ;; round-trip (:set-queued-turn-id). If the restore wins that race
         ;; the orphaned gateway turn survives and auto-drains after the
         ;; cancel = the message gets SENT while ALSO landing back in the
         ;; queue/editor. Keeping it purely LOCAL here means restore pulls it
         ;; cleanly to the editor with nothing to leak server-side.
         gw-fx
         (when (and session agent-text (not (:cancelling? source-db)))
           (let
             [extra-body
              (turn-extra-body source-db)

              turn-features
              {}

              reasoning-level
              (when (reasoning-effort-configurable?) (get-in db [:settings :reasoning-level]))]

             [:gateway-enqueue workspace-id session client-id agent-text reasoning-level extra-body
              turn-features workspace]))]

        {:db (update-tab db
                         workspace-id
                         (fn [w]
                           (-> w
                               (update :pending-sends
                                       (fn [q]
                                         (conj (vec (or q [])) entry)))
                               (update :input-history
                                       (fn [xs]
                                         (let [xs (vec (or xs []))]
                                           (if (= text (last xs)) xs (conj xs text))))))))
         :fx (cond-> [[:notify "Queued — will send after current turn" :info 1500]]
               gw-fx
               (conj gw-fx))}))))

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
                                  :cancel-awaiting-turn-id? false
                                  :progress {:iterations []}
                                  :turn-start-ms (System/currentTimeMillis)
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

(reg-event-fx :set-queued-turn-id
              ;; Late-bind the gateway's queued turn id onto the local preview entry
              ;; (matched by the client-id stamped at enqueue). ArrowUp-edit and clear
              ;; use it to update/delete the real gateway record.
              ;;
              ;; RACE (the "sent AND queued at the same time" wedge): the id lands via
              ;; an async round-trip AFTER `:gateway-enqueue`. If a cancel fires in that
              ;; window it restores the backlog to the editor (:restore-pending-to-input)
              ;; and drops the local entry — but it can only delete gateway records whose
              ;; :turn-id already bound. When this bind then finds NO matching client-id
              ;; entry, the entry was already reclaimed by the cancel (or drained onto a
              ;; fresh local send): the server-side queued turn is now ORPHANED and would
              ;; auto-drain after the cancel = the message gets SENT while ALSO sitting
              ;; back in the editor, and the tab wedges on a ghost turn. Delete the
              ;; orphaned record here so nothing leaks server-side.
              (fn [db [_ workspace-id client-id tid]]
                (let
                  [wid
                   (or workspace-id (current-tab-id db))

                   source-db
                   (db-for-tab db wid)

                   matched?
                   (boolean (some #(= client-id (:client-id %)) (:pending-sends source-db)))

                   sid
                   (get-in source-db [:session :id])]

                  (if matched?
                    {:db (update-tab db
                                     wid
                                     (fn [w]
                                       (update w
                                               :pending-sends
                                               (fn [q]
                                                 (mapv (fn [e]
                                                         (if (= client-id (:client-id e))
                                                           (assoc e :turn-id tid)
                                                           e))
                                                       (vec (or q [])))))))}
                    {:db db :fx (when (and sid tid) [[:gateway-delete-queued sid tid]])}))))

(reg-event-fx :sync-turn-clock
              ;; The gateway's `turn.started` (projected as a :turn-start chunk) carries
              ;; the CANONICAL `started_at` epoch ms — the ONE clock every channel shares —
              ;; AND the gateway `:turn-id`. Three jobs here:
              ;;   1. Re-seed this tab's elapsed timer from `started_at` (only mid-turn) so
              ;;      two terminals attached to the same work show the SAME elapsed.
              ;;   2. LATE-BIND `:gateway-turn-id` (only mid-turn). A plain `:send-message`
              ;;      submit has NO turn id yet — it's minted server-side, and `turn.started`
              ;;      is the first place it reaches this tab. Without this bind the id stays
              ;;      nil and `:cancel-turn` short-circuits `(when (and sid tid) …)`: the
              ;;      cancel NEVER reaches the gateway.
              ;;   3. FINISH A CANCEL THAT RACED THE BIND. If Esc was pressed BEFORE the id
              ;;      bound (`:cancel-awaiting-turn-id?`), that cancel couldn't reach the
              ;;      daemon, so it kept running the "cancelled" turn (a ghost) and the next
              ;;      submit queued behind it — only surfacing once the ghost finished on its
              ;;      own (the visible "queue for a few seconds then it vanishes"). Now that
              ;;      the id is here, fire the gateway cancel automatically instead of forcing
              ;;      a SECOND Esc, and clear the marker so it never bites a later turn.
              (fn [db [_ workspace-id {:keys [started-at-ms server-at-ms turn-id]}]]
                (let
                  [workspace-id
                   (or workspace-id (current-tab-id db))

                   ;; Convert the gateway clock into this process's wall-clock
                   ;; domain using the event's gateway-sampled elapsed value.
                   local-started-at-ms
                   (when (nat-int? started-at-ms)
                     (if (nat-int? server-at-ms)
                       (- (System/currentTimeMillis)
                          (max 0 (- (long server-at-ms) (long started-at-ms))))
                       started-at-ms))]

                  (if-not workspace-id
                    {:db db}
                    (let
                      [target
                       (db-for-tab db workspace-id)

                       awaiting-cancel?
                       (boolean (and turn-id (:cancel-awaiting-turn-id? target)))

                       sid
                       (get-in target [:session :id])

                       db'
                       (update-tab db
                                   workspace-id
                                   (fn [w]
                                     (cond->
                                       (if (:loading? w)
                                         (cond-> w
                                           (nat-int? local-started-at-ms)
                                           (assoc :turn-start-ms local-started-at-ms)

                                           (and turn-id (nil? (:gateway-turn-id w)))
                                           (assoc :gateway-turn-id turn-id))
                                         w)
                                       (:cancel-awaiting-turn-id? w)
                                       (dissoc :cancel-awaiting-turn-id?))))]

                      (cond-> {:db db'}
                        (and awaiting-cancel? sid)
                        (assoc :fx [[:gateway-cancel-turn sid turn-id]])))))))

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

(reg-event-db :sync-queued-turn
              ;; Mirror ONE gateway queue event (turn.queued / .updated / .deleted —
              ;; forwarded through the gateway sync/attach subscriptions as a :queue-sync
              ;; chunk) into this tab's local :pending-sends. The gateway is the queue of
              ;; record, so a message queued in a sibling TUI (or the web) shows up here,
              ;; stays editable, and drains through the same attach machinery. The gateway
              ;; turn id is the sync key:
              ;;   :add    — no-op when already mirrored; otherwise bind the id onto our
              ;;             own un-bound local echo (the enqueue's :set-queued-turn-id
              ;;             may still be in flight) matched by text, else append.
              ;;   :update — rewrite the entry's text (queued-prompt edit elsewhere).
              ;;   :delete — drop the entry (cleared / pulled back to input elsewhere).
              (fn [db [_ workspace-id {:keys [op turn-id text]}]]
                (let [workspace-id (or workspace-id (current-tab-id db))]
                  (if-not (and workspace-id turn-id)
                    db
                    (update-tab
                      db
                      workspace-id
                      (fn [w]
                        (update
                          w
                          :pending-sends
                          (fn [q]
                            (let
                              [q (vec (or q []))
                               mirrored? (boolean (some #(= turn-id (:turn-id %)) q))
                               local-echo? (fn [e]
                                             (and (nil? (:turn-id e))
                                                  (or (= text (:agent-text e)) (= text (:text e)))))
                               ;; The gateway already DRAINED this turn and it is now this
                               ;; tab's LIVE turn (:gateway-turn-id). A late / out-of-order
                               ;; queue-sync add|update — a replayed backlog, or an event
                               ;; that races the local drain+attach — must NOT resurrect it
                               ;; as a "Queued" row while it is running (the "sent AND queued
                               ;; at the same time" ghost). Collapse every op to "ensure it
                               ;; is not mirrored"; the live turn paints once, on its own.
                               running? (= turn-id (:gateway-turn-id w))]

                              (if running?
                                (vec (remove #(= turn-id (:turn-id %)) q))
                                (case op
                                  :add
                                  (cond mirrored? q
                                        (some local-echo? q)
                                        (let [bound? (volatile! false)]
                                          (mapv (fn [e]
                                                  (if (and (not @bound?) (local-echo? e))
                                                    (do (vreset! bound? true)
                                                        (assoc e :turn-id turn-id))
                                                    e))
                                                q))
                                        :else (conj q
                                                    {:text text
                                                     :preview-text text
                                                     :turn-id turn-id
                                                     :queued-at-ms (System/currentTimeMillis)}))

                                  :update
                                  (if mirrored?
                                    (mapv (fn [e]
                                            (if (= turn-id (:turn-id e))
                                              (assoc e
                                                :text text
                                                :preview-text text
                                                :agent-text text)
                                              e))
                                          q)
                                    (conj q
                                          {:text text
                                           :preview-text text
                                           :turn-id turn-id
                                           :queued-at-ms (System/currentTimeMillis)}))

                                  :delete
                                  (vec (remove #(= turn-id (:turn-id %)) q))

                                  q)))))))))))

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

                   tids
                   (keep :turn-id (:pending-sends (db-for-tab db tab-id)))]

                  {:db (update-tab db
                                   tab-id
                                   (fn [w]
                                     (assoc w :pending-sends [])))
                   :fx (mapv (fn [tid]
                               [:gateway-delete-queued sid tid])
                             tids)})))

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
         (update-tab db
                     workspace-id
                     (fn [w]
                       (update w
                               :pending-sends
                               (fn [q]
                                 (let
                                   [q (vec (or q []))
                                    known (set (keep :turn-id q))]

                                   (into q
                                         (keep (fn [{:keys [turn-id text queued-at-ms]}]
                                                 (when-not (contains? known turn-id)
                                                   {:text text
                                                    :preview-text text
                                                    :turn-id turn-id
                                                    :queued-at-ms
                                                    (or queued-at-ms (System/currentTimeMillis))})))
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

          {:db
           (update-tab
             db
             workspace-id
             (fn [w]
               (->
                 w
                 (update :messages
                         conj
                         (assoc (chat/user-message request-text) :client-turn-id client-turn-id))
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
                           (vec (remove #(or (= tid (:turn-id %))
                                             (and (nil? (:turn-id %))
                                                  (= request-text (or (:agent-text %) (:text %)))))
                                  (or q [])))))
                 (assoc :scroll scroll/follow
                        :loading? true
                        :cancel-token token
                        :gateway-turn-id tid
                        :cancelling? false
                        :progress {:iterations []}
                        :turn-start-ms (or (:running-started-at session) (System/currentTimeMillis))
                        :input-history-index nil
                        :input-history-draft nil))))
           :fx [[:session-attach workspace-id session tid token client-turn-id]]})))))

(reg-event-fx
  :sibling-turn-started
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
       (:session target)]

      (if-not
        (and workspace-id session turn-id (not (:loading? target)) (not (:gateway-turn-id target)))
        {:db db}
        {:db db
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

                   ;; OWNERSHIP: only entries THIS tab authored (stamped :client-id at
                   ;; enqueue time) come back to the editor and get their gateway
                   ;; records deleted. MIRRORED entries (no :client-id — queued by a
                   ;; sibling TUI/web and mirrored here by :sync-queued-turn) are the
                   ;; sibling's property: deleting them fired `turn.queued.deleted` at
                   ;; a client still blocked on its own queued turn, which synthesized
                   ;; a spurious CANCELLED terminal there (the "session cancelled
                   ;; itself" bug). They stay queued server-side and mirrored locally.
                   mine
                   (vec (filter :client-id pending))

                   mirrors
                   (vec (remove :client-id pending))

                   sid
                   (get-in source-db [:session :id])

                   tids
                   (keep :turn-id mine)]

                  (if (empty? mine)
                    {:db db}
                    (let
                      [cur-text
                       (input/input->text (:input source-db))

                       texts
                       (into (if (str/blank? cur-text) [] [cur-text]) (map :text mine))

                       combined
                       (str/join "\n\n" (remove str/blank? texts))

                       merged-pastes
                       (reduce merge (or (:pastes source-db) {}) (map :pastes mine))

                       merged-counter
                       (apply max 0 (:paste-counter source-db 0) (map #(:paste-counter % 0) mine))]

                      {:db (update-tab db
                                       workspace-id
                                       (fn [w]
                                         (assoc w
                                           :input (text->input-state combined)
                                           :pastes merged-pastes
                                           :paste-counter merged-counter
                                           :pending-sends mirrors
                                           :input-history-index nil
                                           :input-history-draft nil)))
                       :fx (into [[:notify "Queue restored to input — not sent" :info 2000]]
                                 (mapv (fn [tid]
                                         [:gateway-delete-queued sid tid])
                                       tids))})))))

(defn- gateway-cancel-turn-or-current!
  "Best-effort gateway cancel for Esc. With a known `tid` fire the id-addressed
   cancel; WITHOUT one fall back to the tid-less `cancel-current` (kills whatever
   holds the session's `:current-turn` — the Esc-before-`turn.started` race and
   the post-self-heal ghost turn both land here, where the id-addressed route is
   useless). Gateway HTTP errors come back as `{:error <keyword>}` (e.g.
   `:turn-not-found`, `:no-running-turn`) instead of throwing — `send-json!`
   throws ex-info on 4xx, so without this translation the old inline
   `(catch Throwable _ nil)` swallowed the very errors the terminal-state check
   branched on. Transport failures return nil (cancel simply didn't reach)."
  [sid tid]
  (when sid
    (try (if tid (vis/gateway-cancel-turn! sid tid) (vis/gateway-cancel-current-turn! sid))
         (catch clojure.lang.ExceptionInfo e
           (let [data (ex-data e)]
             {:error (keyword (or (get data "error") "gateway-error"))
              :http-status (:http-status data)}))
         (catch Throwable _ nil))))

(def ^:private gateway-terminal-cancel-errors
  "Gateway cancel errors that mean the server turn is ALREADY gone/terminal —
   safe to clear local cancelling state immediately (no completion event will
   ever arrive)."
  #{:turn-not-found :not-running :no-running-turn})

(reg-event-fx
  :cancel-turn
  (fn [db _]
    (cond (not (:loading? db)) {:db db}
          ;; A SECOND cancel while one is already pending force-clears locally. The
          ;; first press armed `:cancelling?` and now waits for the daemon's terminal
          ;; `turn.completed` to release it — but if the daemon is gone (killed
          ;; gateway, SSE drop) that event never arrives, and every repeated Esc/Ctrl+C
          ;; otherwise RE-ARMS the self-heal clock (`:cancelling-at-ms` below), so
          ;; mashing Esc to escape a dead turn wedges it forever instead of letting the
          ;; 8s self-heal fire. Still make one best-effort gateway cancel (the turn id
          ;; may only just have been late-bound), then tear the turn down locally now.
          (:cancelling? db) (let
                              [sid
                               (get-in db [:session :id])

                               tid
                               (:gateway-turn-id db)

                               gateway-result
                               (gateway-cancel-turn-or-current! sid tid)

                               gateway-terminal?
                               (contains? gateway-terminal-cancel-errors (:error gateway-result))]

                              (try (vis/cancel! (:cancel-token db)) (catch Throwable _ nil))
                              {:db (clear-active-turn-state db)
                               :fx [[:notify
                                     (if gateway-terminal?
                                       "Turn is no longer running; cleared local cancelling state."
                                       "Turn force-cancelled locally.")
                                     (if gateway-terminal? :info :warn)
                                     cancel-notification-ttl-ms]]})
          :else (let
                  [sid
                   (get-in db [:session :id])

                   tid
                   (:gateway-turn-id db)

                   gateway-result
                   (gateway-cancel-turn-or-current! sid tid)

                   gateway-terminal?
                   (contains? gateway-terminal-cancel-errors (:error gateway-result))]

                  ;; Both the cooperative flag and the hard interrupt are fired through one
                  ;; channel-agnostic call. See channels.cancellation/cancel! for the
                  ;; contract.
                  (vis/cancel! (:cancel-token db))
                  ;; An attached gateway turn runs server-side: interrupting the local
                  ;; attach waiter alone would leave the engine working, so fire the
                  ;; gateway turn's own cancel token too. If the gateway says the turn is
                  ;; already gone/terminal (for example after an orphan sweep), reconcile
                  ;; the local optimistic loading state immediately: no future gateway
                  ;; completion event will arrive to clear "Cancelling...".
                  (if gateway-terminal?
                    {:db (clear-active-turn-state db)
                     :fx [[:notify "Turn is no longer running; cleared local cancelling state."
                           :info cancel-notification-ttl-ms]]}
                    {:db (cond->
                           (assoc db
                             :cancelling? true
                             :cancelling-at-ms (System/currentTimeMillis))
                           ;; Esc landed BEFORE turn.started bound the gateway id, so the
                           ;; cancel above couldn't reach the daemon. Remember it: when the
                           ;; id late-binds (`:sync-turn-clock`) the gateway cancel fires
                           ;; automatically, so a single Esc tears the turn down instead of
                           ;; leaving a ghost the next submit queues behind.
                           (nil? tid)
                           (assoc :cancel-awaiting-turn-id? true))
                     :fx [[:notify "Cancelling current turn..." :info
                           cancel-notification-ttl-ms]]})))))

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

                  (when sid (gateway-cancel-turn-or-current! sid tid))
                  {:db db})))

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
                    (let [workspace-id (current-tab-id db)]
                      (try (vis/cancel! (:cancel-token db)) (catch Throwable _ nil))
                      ;; Re-kill server-side BEFORE the local clear drops the turn id:
                      ;; after `clear-active-turn-state` the id-addressed cancel is
                      ;; unusable forever, and a still-running server ghost would keep
                      ;; `:current-turn` and silently queue every next submit behind it.
                      ;; Tid-less fallback covers the id-never-bound case too.
                      (gateway-cancel-turn-or-current! (get-in db [:session :id])
                                                       (:gateway-turn-id db))
                      {:db (clear-active-turn-state db)
                       :fx (cond->
                             [[:notify "Cancel timed out — cleared locally. You can send again."
                               :warn cancel-notification-ttl-ms]]
                             (some :client-id (:pending-sends (db-for-tab db workspace-id)))
                             (conj [:dispatch [:restore-pending-to-input workspace-id]]))})))))

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
  (fn [db [_ a b c]]
    (let
      [[workspace-id answer
        {:keys [model provider llm-selected llm-actual llm-fallback? llm-routing-trace
                iteration-count duration-ms tokens cost confidence session-turn-id status
                utilization client-turn-id slash]}]
       (if (keyword? a) [a b c] [(current-tab-id db) a b])

       drain?
       (volatile! false)

       restore-pending?
       (volatile! false)

       db'
       (update-tab
         db
         workspace-id
         (fn [workspace]
           (let
             [trace
              (get-in workspace [:progress :iterations])

              cancelled?
              (= :cancelled status)

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
                 (when (some :client-id (:pending-sends ws)) (vreset! restore-pending? true))
                 ws)
               (let
                 [start
                  (:turn-start-ms workspace)

                  wall-ms
                  (when start (- (System/currentTimeMillis) (long start)))

                  content
                  (vec (or answer []))

                  response
                  (->
                    (chat/assistant-message content)
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
                      (assoc :slash? true)))

                  messages'
                  (replace-pending-assistant (:messages workspace) response)

                  still-pending?
                  (boolean (some pending-assistant-message? messages'))

                  workspace'
                  (cond->
                    (assoc workspace
                      ;; Re-pin to the bottom by REPLACING `:scroll`
                      ;; with a fresh FOLLOW. A result can land
                      ;; atomically while an ease was in flight (e.g.
                      ;; a `/workspace list` table); replacing the
                      ;; whole value means no animation target can
                      ;; dangle, so the view snaps cleanly to the
                      ;; bottom instead of flashing to the top first.
                      :messages messages'
                      :utilization utilization
                      :scroll scroll/follow
                      :loading? still-pending?
                      :cancelling? false
                      :cancelling-at-ms nil)
                    (not still-pending?)
                    clear-active-turn-state)

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

                 (when (and (not (:loading? ws-final)) (seq (:pending-sends ws-final)))
                   ;; Normal completion drains the next queued turn; a cancel
                   ;; restores the AUTHORED backlog to the editor instead of
                   ;; firing it. Mirrored sibling entries are never restored
                   ;; (or deleted) here — see :restore-pending-to-input.
                   (if cancelled?
                     (when (some :client-id (:pending-sends ws-final))
                       (vreset! restore-pending? true))
                     (vreset! drain? true)))
                 ws-final)))))]

      {:db (cond-> db'
             ;; Persistent unread dot: a BACKGROUND tab that just FINISHED a
             ;; turn (same gate as the bell) lights a dot that stays until the
             ;; user focuses it (cleared in `activate-tab`).
             (and (not= workspace-id (current-tab-id db)) (not= :cancelled status))
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
             (conj [:dispatch [:restore-pending-to-input workspace-id]]))})))
;;; ── Side effects ───────────────────────────────────────────────────────────

(reg-fx :dispatch
        (fn [event]
          (dispatch event)))

(reg-fx :notify
        (fn [text level ttl-ms]
          (vis/notify! text :level level :ttl-ms ttl-ms)))
;; Persist the active session's model preference to the shared, channel-neutral
;; store. The engine reads it on the next turn (router-for-model) and the web
;; rail shows the same value — one source of truth across channels.
(reg-fx :set-session-model
        (fn [sid provider model]
          (vis/gateway-set-session-model! sid provider model)
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
               ;; channels.cancellation/cancellation? folds in
               ;; InterruptedException, CancellationException, and
               ;; runtime wrappers around them - keep all the
               ;; channel-shaped logic in one place. The bubble
               ;; renderer dims the result based on `:status
               ;; :cancelled`, so we attach it explicitly here.
               (let
                 [message (if (vis/cancellation? t)
                            "Cancelled by user."
                            (vis/format-error (or (ex-message t) (str t))))
                  block {"id" (str (java.util.UUID/randomUUID))
                         "type" (if (vis/cancellation? t) "notice" "error")
                         "code" (if (vis/cancellation? t) "turn_cancelled" "turn_failed")
                         "message" message}]

                 (dispatch [:message-received workspace-id [block]
                            (cond-> {:client-turn-id client-turn-id}
                              (vis/cancellation? t)
                              (assoc :status :cancelled))]))))))]
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
               (let
                 [message (if (vis/cancellation? t)
                            "Cancelled by user."
                            (vis/format-error (or (ex-message t) (str t))))
                  block {"id" (str (java.util.UUID/randomUUID))
                         "type" (if (vis/cancellation? t) "notice" "error")
                         "code" (if (vis/cancellation? t) "turn_cancelled" "turn_failed")
                         "message" message}]

                 (dispatch [:message-received workspace-id [block]
                            (cond-> {:client-turn-id client-turn-id}
                              (vis/cancellation? t)
                              (assoc :status :cancelled))]))))))]
      (vis/cancellation-set-future! token fut))))

(reg-fx
  :gateway-enqueue
  ;; Register a busy-time submission as a REAL gateway queued turn (server-side
  ;; queue of record). The returned turn id is late-bound onto the local preview
  ;; entry so ArrowUp-edit / clear can update or delete the same record.
  (fn [workspace-id session client-id agent-text reasoning-level extra-body turn-features workspace]
    (try (when-let [sid (:id session)]
           (let
             [res (vis/gateway-submit-turn! sid
                                            (cond-> {:request agent-text}
                                              reasoning-level
                                              (assoc :reasoning-default reasoning-level)

                                              extra-body
                                              (assoc :extra-body extra-body)

                                              (seq turn-features)
                                              (assoc :turn-features turn-features)

                                              (seq workspace)
                                              (assoc :workspace workspace)))
              tid (get-in res [:turn "turn_id"])]

             (when tid (dispatch [:set-queued-turn-id workspace-id client-id tid]))))
         (catch Throwable t
           (try (vis/notify! (str "Queue via gateway failed: " (or (ex-message t) (str t)))
                             :level :warn
                             :ttl-ms 3000)
                (catch Throwable _ nil))))))

(reg-fx :gateway-delete-queued
        (fn [sid tid]
          (when (and sid tid)
            (try (vis/gateway-delete-queued-turn! sid tid) (catch Throwable _ nil)))))

(reg-fx :submit-orphan-sends
        ;; A closing tab still held AUTHORED submissions that never reached the
        ;; gateway (no :turn-id). Submit each to the gateway — the server-side queue
        ;; of record — so the text survives the tab close: it runs/queues under the
        ;; session and is visible on the next reattach. Best effort off the input
        ;; thread; a failure surfaces as a warning notification, never a throw.
        (fn [sid texts]
          (vis/worker-future "tui-submit-orphan-sends"
                             (fn []
                               (doseq [text texts]
                                 (try (vis/gateway-submit-turn! sid {:request text})
                                      (catch Throwable t
                                        (try (vis/notify! (str "Re-queue of unsent message failed: "
                                                               (or (ex-message t) (str t)))
                                                          :level :warn
                                                          :ttl-ms 3000)
                                             (catch Throwable _ nil)))))))))

(reg-fx :gateway-cancel-turn
        ;; Fire-and-forget cancel of a RUNNING gateway turn. Used by `:sync-turn-clock`
        ;; to finish a cancel that was armed before the turn id late-bound (see
        ;; `:cancel-awaiting-turn-id?`); the result is irrelevant — we've already torn
        ;; the turn down locally, this just stops the daemon-side ghost.
        (fn [sid tid]
          (when (and sid tid) (try (vis/gateway-cancel-turn! sid tid) (catch Throwable _ nil)))))

(reg-fx :drain-idle-queue
        ;; Kick a server-side queued backlog into motion for an IDLE session on
        ;; open/resume: the daemon starts the head queued turn (no-op if one is
        ;; already running) and emits turn.started, which the tab's event
        ;; subscription turns into :sibling-turn-started -> :attach-running-turn.
        ;; Best-effort; a stopped daemon or lost race simply leaves it queued.
        (fn [sid]
          (when sid (try (vis/gateway-drain-idle! sid) (catch Throwable _ nil)))))

(reg-fx :gateway-close-session
        (fn [sid]
          (when sid (try (vis/gateway-close-session! sid) (catch Throwable _ nil)))))

(reg-fx :release-session-runtime
        ;; Stop a session's live daemon runtime + background children (shell_bg,
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
