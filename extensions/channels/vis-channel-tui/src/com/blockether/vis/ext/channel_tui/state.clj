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
            [com.blockether.vis.ext.channel-tui.scroll :as scroll]
            [com.blockether.vis.internal.workspace :as workspace])
  (:import [java.util.concurrent Executors ScheduledExecutorService
            ScheduledFuture TimeUnit]))

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
  ^{:doc "Monitor object the render thread .waits on. Notify-all on every
          dispatch that changes display state."}
  (Object.))

(def ^:private no-render-bump-events
  "Events that update app-db without requesting a redraw. Right now this
   is just `:set-layout`, which is the render thread itself pushing back
   computed sizes for the input thread to read."
  #{:set-layout})

(defn- notify-render! []
  (locking render-monitor (.notifyAll render-monitor)))

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

(defn- bump-version [db]
  (update db :render-version (fnil inc 0)))

(def ^:private workspace-state-keys
  [:session
   :workspace
   :workspace/root
   :title
   :messages
   :scroll
   :input
   :input-history
   :input-history-index
   :input-history-draft
   :slash-command-index
   :slash-command-hidden?
   :submitted-input
   :pending-sends
   :pastes
   :paste-counter
   :loading?
   :cancel-token
   :cancelling?
   :progress
   :turn-start-ms
   :detail-expansions
   :mouse-selection])

(defn- empty-workspace-state
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
   :mouse-selection nil})

(defn- workspace-snapshot
  [db]
  (merge (empty-workspace-state)
    (select-keys db workspace-state-keys)))

(defn- current-workspace-id
  [db]
  (or (:active-workspace-id db)
    (:id (some #(when (:active? %) %) (:workspaces db)))
    (:id (first (:workspaces db)))))

(defn- active-workspace-entry
  [db]
  (let [active-id (current-workspace-id db)]
    (some #(when (= (:id %) active-id) %) (:workspaces db))))

(defn- active-workspace
  [db]
  (or (:workspace db)
    (some-> (active-workspace-entry db) :workspace)
    (when-let [root (or (:workspace/root db)
                      (some-> (active-workspace-entry db) :workspace/root))]
      {:workspace/root root})))

(defn- sync-active-workspace
  [db]
  (if-let [id (current-workspace-id db)]
    (assoc-in db [:workspace-locals id] (workspace-snapshot db))
    db))

(defn- finalize-db
  [db]
  (cond-> db
    (map? db) sync-active-workspace))

(defn dispatch
  "Dispatch an event vector, e.g. (dispatch [:send-message \"hello\"]).
   Bumps `:render-version` and wakes the render thread for every event
   except those in `no-render-bump-events`."
  [[id :as event-vec]]
  (if-let [{:keys [type] :as handler} (get @event-registry id)]
    (let [bump? (not (no-render-bump-events id))]
      (case type
        :db (swap! app-db
              (fn [db]
                (let [db' (finalize-db ((:fn handler) db event-vec))]
                  (if bump? (bump-version db') db'))))
        :fx (let [{:keys [db fx]} ((:fn handler) @app-db event-vec)]
              (when db
                (let [db' (finalize-db db)]
                  (reset! app-db (if bump? (bump-version db') db'))))
              (doseq [[fx-id & args] fx]
                (when-let [fx-fn (get @fx-registry fx-id)]
                  (apply fx-fn args)))))
      (when bump? (notify-render!)))
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
;;  :cancelling? false           ;; true once Esc was pressed; cleared on :message-received
;;  :progress   nil              ;; live per-iteration timeline while loading:
;;                               ;;   {:iterations [{:iteration int
;;                               ;;                  :thinking  str-or-nil
;;                               ;;                  :code      [str]       ;; latest streamed forms
;;                               ;;                  :final?    bool}]}
;;                               ;; Cleared on :message-received.
;;  :settings  {:show-thinking true :show-iterations true}
;;  :channel-status {}           ;; extension/channel status banners keyed by id
;;  :dialog-open? false}         ;; dialog singleton guard
;;

(def ^:private settings-notification-ttl-ms 1500)
(def ^:private cancel-notification-ttl-ms 2500)

(def ^:private live-progress-render-interval-ms
  "Maximum wall-clock interval between live reasoning redraws.

   Keep the live TUI heartbeat at the same cadence as the render loop: progress
   chunks coalesce to one app-db update per frame instead of per token, while
   lifecycle chunks still flush immediately so code/result/final boundaries
   appear without delay. Virtual layout then projects and paints only visible
   bubbles."
  80)

(def ^:private pending-assistant-ir
  "Canonical IR placeholder shown in the assistant bubble while the
   request is in flight. Construction sites also stamp `:pending?
   true` on the message map; the predicate below relies on that flag
   exclusively — we never compare on rendered text content."
  [:ir {} [:p {} [:span {} "Sending request to provider..."]]])

(defn- pending-assistant-message? [m]
  (and (= :assistant (:role m)) (true? (:pending? m))))

(defn- replace-pending-assistant
  "Replace the pending assistant slot for a completed turn. Prefer the
   stable client turn id; fall back to the oldest pending placeholder for
   older events/tests. Never pop the tail: newer user messages may already
   have been appended while this turn was still live."
  [messages response]
  (let [messages       (vec (or messages []))
        client-turn-id (:client-turn-id response)
        response       (dissoc response :pending?)
        idx            (or (when client-turn-id
                             (first (keep-indexed
                                      (fn [idx m]
                                        (when (and (pending-assistant-message? m)
                                                (= client-turn-id (:client-turn-id m)))
                                          idx))
                                      messages)))
                         (first (keep-indexed
                                  (fn [idx m]
                                    (when (pending-assistant-message? m) idx))
                                  messages)))]
    (cond
      idx
      (assoc messages idx response)

      (and (seq messages) (= :assistant (:role (peek messages))))
      (conj (pop messages) response)

      :else
      (conj messages response))))

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
    (reify java.util.concurrent.ThreadFactory
      (newThread [_ r]
        (doto (Thread. ^Runnable r "vis-tui-progress-trailing-flush")
          (.setDaemon true))))))

(defn- make-progress-render-updater
  ([dispatch-fn]
   (make-progress-render-updater dispatch-fn #(System/currentTimeMillis) nil))
  ([dispatch-fn now-ms-fn]
   (make-progress-render-updater dispatch-fn now-ms-fn nil))
  ([dispatch-fn now-ms-fn schedule-fn]
   ;; Per-phase clocks: `:reasoning` and `:content` stream
   ;; independently and a fast content stream MUST NOT starve
   ;; reasoning frames (and vice versa). Before this split, both
   ;; phases (plus every lifecycle chunk) shared one clock; a fast
   ;; per-token content stream kept resetting the clock so the
   ;; reasoning bubble froze at the very first frame ("I" / "The").
   ;; Lifecycle chunks dispatch immediately and never touch the
   ;; throttle clocks.
   ;;
   ;; TRAILING-EDGE FLUSH. Leading-edge-only throttling drops every
   ;; chunk that lands inside the 80ms window after a dispatch. If
   ;; the stream then STALLS (model finishes reasoning fast, provider
   ;; takes 5-30s before the first content delta), the bubble freezes
   ;; on the FIRST frame ("I" / few words) for the entire stall — the
   ;; spinner ticks repaint a stale `:progress` slot. When a chunk is
   ;; dropped we now stash the latest timeline AND schedule a
   ;; delayed dispatch at `last-ms + interval`; subsequent drops just
   ;; overwrite the pending timeline so the trailing flush always
   ;; carries the most recent state. A dispatched chunk (due / not
   ;; throttled) cancels the pending timer.
   (let [last-by-phase      (atom {})
         pending-by-phase   (atom {})
         scheduled-by-phase (atom {})
         schedule!          (or schedule-fn
                              (fn default-schedule! [^Runnable f ^long delay-ms]
                                (.schedule progress-trailing-flush-scheduler
                                  f delay-ms TimeUnit/MILLISECONDS)))]
     (letfn [(cancel-pending! [phase]
               (when-let [f (get @scheduled-by-phase phase)]
                 (try (.cancel ^java.util.concurrent.Future f false)
                   (catch Throwable _ nil)))
               (swap! scheduled-by-phase dissoc phase))
             (flush-pending! [phase]
               ;; Trailing-edge fire: read whatever the latest
               ;; pending timeline is for this phase, dispatch it,
               ;; clear the slot, and bump the per-phase clock so
               ;; the next chunk respects the new window.
               (let [pending (get @pending-by-phase phase)]
                 (swap! scheduled-by-phase dissoc phase)
                 (when pending
                   (swap! pending-by-phase dissoc phase)
                   (swap! last-by-phase assoc phase (long (or (now-ms-fn) 0)))
                   (dispatch-fn [:set-progress-iterations pending]))))]
       (fn [timeline chunk]
         (let [now-ms (long (or (now-ms-fn) 0))
               phase  (:phase chunk)
               throttled? (contains? throttled-streaming-phases phase)
               last-ms (when throttled? (get @last-by-phase phase))
               due? (or (nil? last-ms)
                      (>= (- now-ms (long last-ms))
                        live-progress-render-interval-ms))]
           (cond
             (not throttled?)
             (dispatch-fn [:set-progress-iterations timeline])

             due?
             (do
               ;; Dispatching now — cancel any pending trailing flush;
               ;; we're carrying its latest timeline (and then some).
               (cancel-pending! phase)
               (swap! pending-by-phase dissoc phase)
               (swap! last-by-phase assoc phase now-ms)
               (dispatch-fn [:set-progress-iterations timeline]))

             :else
             (do
               ;; Drop now, but stash latest timeline so a trailing-edge
               ;; timer can flush it within the throttle window even if
               ;; the stream stalls afterward.
               (swap! pending-by-phase assoc phase timeline)
               (when-not (get @scheduled-by-phase phase)
                 (let [delay-ms (max 1 (- live-progress-render-interval-ms
                                         (- now-ms (long last-ms))))
                       ^Runnable task (fn [] (try (flush-pending! phase)
                                               (catch Throwable _ nil)))
                       f (schedule! task (long delay-ms))]
                   (swap! scheduled-by-phase assoc phase f)))))))))))

(defn- normalize-theme-name
  [v]
  (let [s (cond
            (keyword? v) (name v)
            (string? v)  (str/trim v)
            :else        nil)]
    (keyword (if (str/blank? s)
               shared-theme/default-theme-id
               s))))

(defn- normalize-settings
  "Coerce the two settings keys this layer still OWNS:
     `:theme-name`           — enum, picked from registered themes
                              (dynamic; not in the toggles registry).
     `:contributors-disabled` — set of contributor ids the user wants
                              hidden.

   Every other former settings key (`:show-thinking`,
   `:show-iterations`, `:show-silent`, `:show-timestamps`,
   `:mouse-selection-copy`, `:voice/respond?`, `:message-meta`,
   `:reasoning-level`, `:openai-codex-verbosity`) now lives in the
   toggles registry. The `:settings` map in app-db is a cached
   projection of (registry + these two locals); a listener wired in
   `init!` keeps the projection in sync."
  [settings]
  (-> settings
    (update :theme-name normalize-theme-name)
    (update :contributors-disabled
      (fn [v] (cond (nil? v) #{} (set? v) v :else (set v))))))

(defn- migrated-toggle-projection
  "Pull the migrated boolean + enum toggles back into a flat
   `:settings`-shaped map so existing consumers
   (`(get settings :show-thinking ...)`) keep working without
   reaching into the registry directly. The registry is the source
   of truth; this projection is the cached view."
  []
  {:show-thinking          (vis/toggle-enabled? :vis/show-thinking)
   :show-iterations        (vis/toggle-enabled? :vis/show-iterations)
   :show-silent            (vis/toggle-enabled? :vis/show-silent)
   :show-timestamps        (vis/toggle-enabled? :vis/show-timestamps)
   :mouse-selection-copy   (vis/toggle-enabled? :vis/mouse-selection-copy)
   :voice/respond?         (vis/toggle-enabled? :voice/respond?)
   :message-meta           (vis/toggle-value    :vis/message-meta)
   :reasoning-level        (vis/toggle-value    :vis/reasoning-level)
   :openai-codex-verbosity (vis/toggle-value    :openai-codex/verbosity)})

(def default-settings
  "Per-user TUI settings. Persisted to `~/.vis/config.edn` under
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
  {:theme-name            (keyword shared-theme/default-theme-id)
   ;; Set of contributor ids the user wants hidden in the TUI
   ;; header / footer. Each extension that contributes a row /
   ;; segment / status registers under a keyword id (e.g. :goal
   ;; from vis-goal, :voice from vis-foundation-voice). Adding the id
   ;; to this set skips that contributor's rendering. Default empty
   ;; (every registered contributor shows). See
   ;; `com.blockether.vis.ext.channel-tui.contributors`.
   :contributors-disabled  #{}})

(defn- load-persisted-settings
  "Read `:tui-settings` from `~/.vis/config.edn` for the keys this
   layer still owns (`:theme-name`, `:contributors-disabled`). The
   migrated boolean + enum settings are loaded by
   `vis/toggles-hydrate-from-config!` in `screen/run-chat!`."
  []
  (let [raw   (try (vis/load-config-raw) (catch Throwable _ nil))
        saved (when (map? raw) (:tui-settings raw))]
    (normalize-settings
      (merge default-settings (when (map? saved)
                                (select-keys saved (keys default-settings)))))))

(defn- persist-settings!
  "Write `settings` back into `~/.vis/config.edn` under
   `:tui-settings`, preserving every other key in the file. Failures
   are swallowed - a config-save failure should never crash a TUI
   that's already otherwise healthy."
  [settings]
  (try
    (let [raw (or (vis/load-config-raw) {})]
      (vis/save-config! (assoc raw :tui-settings settings)))
    (catch Throwable _ nil)))

(defn- apply-settings-update!
  "Merge `new-settings` over the local-owned slice (theme +
   contributors-disabled), persist that slice into
   `~/.vis/config.edn`, and rebuild the cached `:settings` view by
   overlaying the toggle projection. Migrated keys ignored here —
   they route through `vis/toggle-set-value!` / `cycle-value!` and
   the listener installed in `init!` keeps the projection coherent."
  [db new-settings]
  (render/invalidate-cache!)
  (let [local-merged (normalize-settings
                       (merge default-settings
                         (select-keys (:settings db) (keys default-settings))
                         (select-keys new-settings   (keys default-settings))))
        projected    (merge (migrated-toggle-projection) local-merged)]
    (tui-theme/apply-theme! (:theme-name local-merged))
    (persist-settings! local-merged)
    (assoc db :settings projected)))

(defn- move-to-front
  [pred coll]
  (let [items (vec (or coll []))
        idx   (first (keep-indexed (fn [idx item]
                                     (when (pred item) idx))
                       items))]
    (if (nil? idx)
      items
      (vec (concat [(nth items idx)]
             (subvec items 0 idx)
             (subvec items (inc idx)))))))

(defn- model-entry
  [provider model]
  (when-let [model-name (and model (vis/model-name model))]
    (when (:id provider)
      {:provider-id (:id provider)
       :model       model-name})))

(defn- model-cycle-entries
  [config]
  (->> (:providers config)
    (mapcat (fn [provider]
              (keep #(model-entry provider %) (:models provider))))
    vec))

(defn- active-model-entry
  [config]
  (when-let [provider (first (:providers config))]
    (model-entry provider (first (:models provider)))))

(defn- same-model-cycle?
  [a b]
  (= (frequencies a) (frequencies b)))

(defn- select-model-entry
  [config {:keys [provider-id model]}]
  (update config :providers
    (fn [providers]
      (mapv (fn [provider]
              (if (= provider-id (:id provider))
                (update provider :models
                  #(move-to-front (fn [candidate]
                                    (= model (vis/model-name candidate)))
                     %))
                provider))
        (move-to-front #(= provider-id (:id %)) providers)))))

(defn- cycle-primary-model
  ([config]
   (cycle-primary-model config nil))
  ([config cycle-order]
   (let [providers (vec (or (:providers config) []))
         entries   (model-cycle-entries config)
         order     (if (and (seq cycle-order)
                         (same-model-cycle? cycle-order entries))
                     (vec cycle-order)
                     entries)]
     (cond
       (empty? providers)
       {:config config
        :message "No providers configured"
        :level :warn}

       (< (count entries) 2)
       {:config config
        :message "No alternate models configured"
        :level :warn}

       :else
       (let [active          (active-model-entry config)
             idx             (.indexOf ^java.util.List order active)
             next-entry      (nth order (mod (inc (if (neg? idx) -1 idx))
                                          (count order)))
             provider-prefix (when (< 1 (count (distinct (map :provider-id entries))))
                               (str (name (:provider-id next-entry)) "/"))
             config'         (select-model-entry config next-entry)]
         {:config config'
          :cycle-order order
          :changed? true
          :message (str "Model: " provider-prefix (:model next-entry))
          :level :info})))))

(defn- current-model-info
  []
  (when-let [router (try (vis/get-router) (catch Throwable _ nil))]
    (try (vis/resolve-effective-model router) (catch Throwable _ nil))))

(defn- current-provider-id
  []
  (:provider (current-model-info)))

(def ^:private ^:const max-workspaces 8)

(def untitled-session-label
  "Default workspace label for a session without a title yet.

   Aliases the channel-agnostic value in `internal/header` so the TUI,
   web, Telegram, etc. all show the same placeholder. Kept exported
   here for callers (and tests) that already reach in via the state
   namespace."
  vh/untitled-session-label)

(def ^:private transcript-dump-markers
  ["▾ REASONING ["
   "▾ RESULT ["
   "▾ ERROR ["
   "RESULT [iteration"
   "REASONING [iteration"])

(defn transcript-dump-input?
  "True when text looks like a copied Vis assistant trace/transcript, not a
   fresh user prompt. These strings can enter `user_request` after accidental
   bubble copy/paste, then poison ArrowUp history on resumed sessions."
  [text]
  (let [s (str text)]
    (boolean
      (or (some #(str/includes? s %) transcript-dump-markers)
        (and (> (count s) 8000)
          (boolean (re-find #"(?m)^\s*The user (wants|is|asked|reports|says|needs)\b" s))
          (boolean (re-find #"(?m)^\s*\(def\s+" s)))))))

(defn- history-user-texts
  [history]
  (->> (or history [])
    (keep (fn [message]
            (when (= :user (:role message))
              (:text message))))
    (remove transcript-dump-input?)
    vec))

(defn- workspace-number
  [entry]
  (when-let [[_ n] (some->> entry :id name (re-matches #"tab-(\d+)"))]
    (Long/parseLong n)))

(defn- next-workspace-number
  [entries]
  (inc (reduce max 0 (keep workspace-number entries))))

(defn- base-workspace-entry
  [db]
  {:id    (or (:active-workspace-id db) :main)
   :label (let [title (:title db)]
            (if (and (string? title) (not (str/blank? title)))
              title
              untitled-session-label))})

(defn- workspaces-or-base
  [db]
  (let [entries (vec (:workspaces db))]
    (if (seq entries)
      entries
      [(base-workspace-entry db)])))

(defn- active-workspace-label
  [db fallback]
  (let [title (:title db)]
    (if (and (string? title) (not (str/blank? title)))
      title
      (or fallback untitled-session-label))))

(defn- ensure-workspaces
  [db]
  (let [entries (workspaces-or-base db)
        active-id (or (current-workspace-id (assoc db :workspaces entries))
                    (:id (first entries)))]
    (assoc db
      :workspaces (mapv (fn [entry]
                          (cond-> (dissoc entry :active?)
                            (= (:id entry) active-id) (assoc :active? true)))
                    entries)
      :active-workspace-id active-id)))

(defn- restore-workspace
  [db workspace-id]
  (merge db (or (get-in db [:workspace-locals workspace-id])
              (empty-workspace-state))))

(defn- activate-workspace
  [db workspace-id]
  (-> db
    sync-active-workspace
    (assoc :active-workspace-id workspace-id)
    (update :workspaces
      (fn [entries]
        (mapv (fn [entry]
                (cond-> (dissoc entry :active?)
                  (= (:id entry) workspace-id) (assoc :active? true)))
          entries)))
    (restore-workspace workspace-id)))

(defn- update-workspace
  [db workspace-id f]
  (let [workspace-id (or workspace-id (current-workspace-id db))]
    (if (and workspace-id (not= workspace-id (current-workspace-id db)))
      (update-in db [:workspace-locals workspace-id]
        (fn [snapshot]
          (workspace-snapshot
            (f (merge db (or snapshot (empty-workspace-state)))))))
      (f db))))

(defn- workspace-session-id
  [db workspace-id]
  (some-> (get-in db [:workspace-locals workspace-id :session :id]) str))

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
  (let [local-settings (load-persisted-settings)
        settings       (merge (migrated-toggle-projection) local-settings)]
    (tui-theme/apply-theme! (:theme-name settings))
    (reset! app-db {:config     nil
                    :session nil
                    :title      nil
                    :messages   []
                    :scroll     scroll/follow
                    :input      (input/empty-input)
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
                    :pastes     {}
                    :paste-counter 0
                    :loading?   false
                    :cancel-token nil
                    :cancelling? false
                    :progress   nil
                    :settings   settings
                    :provider-limits nil
                    :channel-status {}
                    :detail-expansions {}
                    :workspaces []
                    :active-workspace-id nil
                    :workspace-locals {}
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
    (when (seq (:providers config))
      ;; rebuild-router! only swaps the global singleton; cached envs
      ;; keep the snapshot they were created with. Reseat them too,
      ;; otherwise the next turn runs against the previous model
      ;; even though the status bar already shows the new one.
      (let [r (vis/rebuild-router! config)]
        (vis/refresh-cached-routers! r)))
    (-> db
      (assoc :config config)
      (dissoc :model-cycle-order))))

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
    (assoc db :settings (merge (migrated-toggle-projection)
                          (select-keys (:settings db) (keys default-settings))))))

(reg-event-fx :cycle-reasoning-level
  (fn [db _]
    (if-not (reasoning-effort-configurable?)
      {:db db
       :fx [[:notify "Reasoning effort is not configurable for this model" :warn settings-notification-ttl-ms]]}
      (let [next (vis/toggle-cycle-value! :vis/reasoning-level)]
        ;; The toggle listener wired in `init!` will fire next; this
        ;; just refreshes the cached :settings projection here too
        ;; so the FX :db ends up consistent within the same tick.
        {:db (apply-settings-update! db {})
         :fx [[:notify (str "Reasoning: " (name next)) :info settings-notification-ttl-ms]]}))))

(reg-event-fx :cycle-codex-verbosity
  (fn [db _]
    (if-not (= :openai-codex (current-provider-id))
      {:db db
       :fx [[:notify "Codex verbosity is only available for OpenAI Codex" :warn settings-notification-ttl-ms]]}
      (let [next (vis/toggle-cycle-value! :openai-codex/verbosity)]
        {:db (apply-settings-update! db {})
         :fx [[:notify (str "Codex verbosity: " (name next)) :info settings-notification-ttl-ms]]}))))

(reg-event-fx :cycle-model
  (fn [db _]
    (let [base-config (or (:config db) (vis/load-config) {:providers []})
          {:keys [config cycle-order changed? message level]}
          (cycle-primary-model base-config (:model-cycle-order db))]
      {:db (cond-> (assoc db :config config)
             cycle-order (assoc :model-cycle-order cycle-order))
       :fx (cond-> []
             changed? (conj [:apply-config config])
             message  (conj [:notify message level settings-notification-ttl-ms]))})))

(reg-event-db :set-layout
  (fn [db [_ layout]]
    ;; Pushed in by the render thread; intentionally does NOT bump
    ;; render-version (see no-render-bump-events).
    (assoc db :layout layout)))

(reg-event-db :toggle-detail
  (fn [db [_ session-id node-id explicit-expand?]]
    (let [k [(str session-id) (str node-id)]]
      (if (some? explicit-expand?)
        ;; Caller knows the row's CURRENT effective state (from the click
        ;; region's `:collapsed?`) and passes the desired new expanded state.
        ;; Store it EXPLICITLY (true/false) — required for rows whose default
        ;; is expanded (BLOCK header, op rows): the old absent/true-only model
        ;; could never represent "explicitly collapsed", so collapsing a
        ;; default-expanded row was a no-op.
        (assoc-in db [:detail-expansions k] (boolean explicit-expand?))
        ;; Legacy 2-arg path (default-collapsed rows): absent <-> true.
        (update db :detail-expansions
          (fn [m]
            (let [expanded? (true? (get m k false))]
              (if expanded?
                (dissoc m k)
                (assoc (or m {}) k true)))))))))

(reg-event-db :select-preview-mode
  (fn [db [_ session-id node-id mode]]
    (assoc-in db [:detail-expansions [(str session-id) (str node-id)]] mode)))

(reg-event-db :bump-render-version
  (fn [db _]
    ;; No-op state mutator. The dispatcher itself bumps
    ;; `:render-version` and notifies the render monitor whenever an
    ;; event lands (unless the event id is in `no-render-bump-events`),
    ;; so simply dispatching this event is enough to wake the painter.
    ;; Used by the mouse handler when a hover-state change needs the
    ;; chrome row repainted with its hover background.
    db))

(reg-event-db :create-workspace
  (fn [db [_ opts]]
    (let [db        (-> db ensure-workspaces sync-active-workspace)
          entries   (vec (:workspaces db))
          n         (next-workspace-number entries)
          id        (keyword (str "tab-" n))
          workspace (:workspace opts)
          root      (or (:workspace/root workspace) (:workspace/root opts))
          label     (or (:label opts)
                      (some-> workspace :label not-empty)
                      untitled-session-label)
          entry     (cond-> {:id id :label label :active? true}
                      workspace (assoc :workspace workspace)
                      root      (assoc :workspace/root root))]
      (if (>= (count entries) max-workspaces)
        db
        (cond-> (-> db
                  (assoc :workspaces (conj (mapv #(dissoc % :active?) entries) entry)
                    :active-workspace-id id)
                  (merge (empty-workspace-state)))
          workspace (assoc :workspace workspace)
          root      (assoc :workspace/root root))))))

(reg-event-db :select-workspace-index
  (fn [db [_ idx]]
    (let [db      (-> db ensure-workspaces sync-active-workspace)
          entries (vec (:workspaces db))
          idx     (if (#{:next :prev} idx)
                    (when (seq entries)
                      (let [active-id (or (:active-workspace-id db)
                                        (:id (some #(when (:active? %) %) entries))
                                        (:id (first entries)))
                            current   (or (first (keep-indexed #(when (= (:id %2) active-id) %1) entries))
                                        -1)
                            delta     (if (= :prev idx) -1 1)]
                        (mod (+ current delta) (count entries))))
                    idx)]
      (if-let [entry (and (integer? idx) (nth entries idx nil))]
        (activate-workspace db (:id entry))
        db))))

(reg-event-db :select-workspace-by-session
  (fn [db [_ session-id]]
    (let [target-id (some-> session-id str)
          db        (-> db ensure-workspaces sync-active-workspace)
          entries   (vec (:workspaces db))
          entry     (when target-id
                      (some #(when (= target-id (workspace-session-id db (:id %))) %)
                        entries))]
      (if entry
        (activate-workspace db (:id entry))
        db))))

(reg-event-db :set-mouse-selection
  (fn [db [_ selection]]
    (assoc db :mouse-selection selection)))

(reg-event-db :clear-mouse-selection
  (fn [db _]
    (dissoc db :mouse-selection)))

(reg-event-db :set-provider-limits
  (fn [db [_ provider-id report]]
    (assoc db :provider-limits {:provider-id provider-id
                                :report report
                                :updated-at-ms (System/currentTimeMillis)})))

(reg-event-db :clear-provider-limits
  (fn [db _]
    (assoc db :provider-limits nil)))

(reg-event-db :shutdown
  (fn [db _]
    (assoc db :shutdown? true)))

(reg-event-db :set-workspace
  ;; Replace the session's current workspace record (trunk or draft) after a
  ;; turn that may have switched it (`/draft new | apply | abandon`).
  (fn [db [_ ws]]
    (assoc db :workspace ws)))

(reg-event-db :init-session
  (fn [db [_ session history workspace]]
    (let [user-history (history-user-texts history)]
      (-> db
        ensure-workspaces
        (assoc :session session
          ;; The session's current workspace record (trunk or draft) — the
          ;; single source the footer/header read to show trunk vs
          ;; `<label> (DRAFT)`. `:root` is the cwd for trunk, the clone for a draft.
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
          :loading? false
          :cancel-token nil
          :cancelling? false
          :progress nil
          :detail-expansions {})))))

(reg-event-db :open-session-tab
  ;; Open `session` (with its `history` + pinned `workspace` record) in a TAB
  ;; WITHOUT disturbing the active tab. If a tab is already bound to this
  ;; session, focus it; otherwise mint a new tab and bind it. This is what
  ;; makes sessions run concurrently: opening/switching never resets the
  ;; running tab — its turn keeps streaming into its own `:workspace-locals`.
  (fn [db [_ session history workspace]]
    (let [sid      (some-> session :id str)
          ;; Freeze the current tab (incl. any in-flight turn) into its locals
          ;; before we change focus, so its streaming worker keeps updating it.
          db       (-> db ensure-workspaces sync-active-workspace)
          entries  (vec (:workspaces db))
          existing (when sid
                     (some #(when (= sid (workspace-session-id db (:id %))) %) entries))]
      (if existing
        (activate-workspace db (:id existing))
        (let [n     (next-workspace-number entries)
              id    (keyword (str "tab-" n))
              label (or (some-> workspace :label not-empty) untitled-session-label)
              entry (cond-> {:id id :label label :active? true}
                      workspace        (assoc :workspace workspace)
                      (:root workspace) (assoc :workspace/root (:root workspace)))]
          (-> db
            (assoc :workspaces (conj (mapv #(dissoc % :active?) entries) entry)
              :active-workspace-id id)
            ;; Make the new tab the live root state (a fresh session view);
            ;; finalize-db snapshots this back into the tab's locals.
            (merge (empty-workspace-state))
            (assoc :session session
              :workspace workspace
              :workspace/root (:root workspace)
              :title nil
              :messages (or history [])
              :input-history (history-user-texts history))))))))

(reg-event-db :set-title
  (fn [db [_ title]]
    (let [db' (assoc db :title title)
          active-id (current-workspace-id db')]
      (cond-> db'
        active-id
        (update :workspaces
          (fn [entries]
            (mapv (fn [entry]
                    (cond-> entry
                      (= (:id entry) active-id)
                      (assoc :label (active-workspace-label db' (:label entry)))))
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
    (assoc db :slash-command-index
      (slash/move-index (:slash-command-index db) delta total))))

(defn- text->input-state [text]
  (let [lines (vec (or (seq (str/split (or text "") #"\n" -1)) [""]))
        crow  (dec (count lines))
        ccol  (count (nth lines crow))]
    {:lines lines :crow crow :ccol ccol}))

(defn- append-input-text
  [current text]
  (let [current-text (input/input->text current)
        next-text    (or text "")]
    (cond
      (str/blank? current-text) next-text
      (str/blank? next-text)    current-text
      :else                    (str current-text "\n" next-text))))

(defn- apply-external-input
  [workspace op text]
  (let [current (:input workspace)
        next    (case op
                  :replace (text->input-state text)
                  :append  (text->input-state (append-input-text current text))
                  :insert  (input/paste-text current (or text ""))
                  current)]
    (assoc workspace :input next
      :input-history-index nil
      :input-history-draft nil
      :slash-command-index 0
      :slash-command-hidden? false)))

(reg-event-db :external-input
  (fn [db [_ op text workspace-id]]
    (update-workspace db workspace-id #(apply-external-input % op text))))

(reg-event-db :channel-status-set
  (fn [db [_ id status]]
    (assoc-in db [:channel-status id]
      (assoc status :updated-at-ms (System/currentTimeMillis)))))

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
  (let [messages (vec (or messages []))
        n        (count messages)]
    (cond
      (and (<= 2 n)
        (= :assistant (:role (peek messages)))
        (= :user (:role (nth messages (- n 2)))))
      (subvec messages 0 (- n 2))

      :else
      messages)))

(defn- restore-submitted-input
  "Drop the pending turn pair AND repopulate the editor. Used when
   a turn was cancelled before any iteration produced visible work
   - the user pressed Esc fast, no trace exists, so dropping the
   placeholder bubble keeps the transcript clean."
  [db {:keys [text pastes paste-counter]}]
  (let [visible-text (input/expand-paste-placeholders text pastes)]
    (-> db
      (assoc :messages (drop-pending-turn-messages (:messages db))
        :scroll scroll/follow
        :input (text->input-state text)
        :input-history-index nil
        :input-history-draft nil
        :slash-command-index 0
        :slash-command-hidden? false
        :pastes (or pastes {})
        :paste-counter (or paste-counter 0)
        :loading? false
        :progress nil
        :cancel-token nil
        :cancelling? false)
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

(reg-event-db :history-up
  (fn [db _]
    (let [history (vec (or (:input-history db) []))
          cur-idx (:input-history-index db)
          draft   (:input-history-draft db)
          input-text (input/input->text (:input db))]
      (if (empty? history)
        db
        (let [new-idx (if (nil? cur-idx)
                        (dec (count history))
                        (max 0 (dec cur-idx)))
              draft   (if (nil? cur-idx) input-text draft)]
          (assoc db
            :input-history-index new-idx
            :input-history-draft draft
            :input (text->input-state (nth history new-idx))))))))

(reg-event-db :history-down
  (fn [db _]
    (let [history (vec (or (:input-history db) []))
          cur-idx (:input-history-index db)
          draft   (:input-history-draft db)]
      (cond
        (nil? cur-idx) db
        (< cur-idx (dec (count history)))
        (let [new-idx (inc cur-idx)]
          (assoc db
            :input-history-index new-idx
            :input (text->input-state (nth history new-idx))))
        :else
        (assoc db
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
  (fn [db [_ content]]
    (let [next-id (inc (or (:paste-counter db) 0))]
      (-> db
        (assoc :paste-counter next-id)
        (assoc-in [:pastes next-id] {:id next-id :content content})))))

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

(reg-event-db :set-scroll
  ;; Search jump / `:scroll-to-message` resolution: snap-park at an exact
  ;; row (already clamped by the painter). No ease - the jump is the point.
  (fn [db [_ offset]]
    (assoc db :scroll (scroll/parked offset))))

(reg-event-db :reanchor-scroll
  ;; Scroll-anchoring write-back from the render thread. `anchored` is the
  ;; corrected absolute on-screen row; `delta` is how far content ABOVE the
  ;; anchor changed height as off-screen estimates resolved. Shift the
  ;; concrete fields so the anchored message stays visually put (no lurch).
  ;; FOLLOW-at-bottom feeds nil to the layout and never dispatches this.
  (fn [db [_ anchored delta]]
    (assoc db :scroll (scroll/reanchor (:scroll db) (long anchored) (long (or delta 0))))))

(reg-event-db :ease-scroll
  ;; Render-loop pulse: advance the on-screen position one ease-out step
  ;; toward where the current intent WANTS it (bottom in FOLLOW, the parked
  ;; offset in AT). This single event subsumes the old tick-scroll-anim +
  ;; follow-bottom-animated + follow-bottom-if-near trio: in FOLLOW the
  ;; desired row simply IS the growing bottom, so streamed content eases in
  ;; for free, and a user parked above (mode :at) is never yanked because
  ;; their desired row is fixed.
  (fn [db [_ total-h inner-h]]
    (let [max-s (max 0 (- (long total-h) (long inner-h)))]
      (assoc db :scroll (scroll/ease (:scroll db) max-s)))))

(reg-event-db :scroll-to-message
  ;; In-session search lands here after the user picks a hit.
  ;; The painter doesn't get told an exact :messages-scroll Y value
  ;; (which it would need to compute heights for); instead it sees
  ;; `:scroll-to-message-pending` and re-resolves the scroll target
  ;; on the next frame, then clears the pending field. One-shot.
  (fn [db [_ msg-idx]]
    (cond-> db
      (and (integer? msg-idx) (>= msg-idx 0))
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
    (let [max-s (max 0 (- (long total-h) (long inner-h)))]
      (assoc db :scroll (scroll/up (:scroll db) (long amount) max-s)))))

(reg-event-db :scroll-down
  ;; Wheel / arrow / PageDown: ease `amount` rows down; landing within the
  ;; slack band of the bottom re-arms FOLLOW.
  (fn [db [_ amount total-h inner-h]]
    (let [max-s (max 0 (- (long total-h) (long inner-h)))]
      (assoc db :scroll (scroll/down (:scroll db) (long amount) max-s)))))

(reg-event-db :scroll-to-y
  ;; Scrollbar drag / track click: map the cursor row to an offset and SNAP
  ;; (1:1, no ease - animation would lag the thumb). The very bottom
  ;; re-enters FOLLOW. Mirrors the thumb math in `scrollbar/geometry`:
  ;; `bar-top` is the top track row, `track-h` the track length, and
  ;; `total-h`/`inner-h` the layout sizes the render thread published.
  (fn [db [_ mouse-y bar-top track-h total-h inner-h]]
    (if (or (<= (long total-h) (long inner-h)) (<= (long track-h) 0))
      db
      (let [max-s    (max 0 (- (long total-h) (long inner-h)))
            denom    (max 1 (- (long track-h) 1))
            fraction (max 0.0 (min 1.0 (double (/ (- (long mouse-y) (long bar-top)) denom))))
            offset   (long (Math/round (* fraction (double max-s))))]
        (assoc db :scroll (scroll/to-y offset max-s))))))

(defn- turn-extra-body
  [{:keys [settings]}]
  (when (= :openai-codex (current-provider-id))
    {:text {:verbosity (name (or (:openai-codex-verbosity settings) :low))}}))

(defn- db-for-workspace
  [db workspace-id]
  (if (= workspace-id (current-workspace-id db))
    db
    (merge db (or (get-in db [:workspace-locals workspace-id])
                (empty-workspace-state)))))

(defn- enqueue-message-result
  [db workspace-id text]
  (let [workspace-id (or workspace-id (current-workspace-id db))
        source-db    (db-for-workspace db workspace-id)
        entry        {:text          text
                      :pastes        (:pastes source-db)
                      :paste-counter (:paste-counter source-db)
                      :queued-at-ms  (System/currentTimeMillis)}]
    {:db (update-workspace db workspace-id
           (fn [w]
             (-> w
               (update :pending-sends
                 (fn [q]
                   (let [q (vec (or q []))]
                     (if (= text (:text (peek q))) q (conj q entry)))))
               (update :input-history
                 (fn [xs]
                   (let [xs (vec (or xs []))]
                     (if (= text (last xs)) xs (conj xs text))))))))
     :fx [[:notify "Queued — will send after current turn" :info 1500]]}))

(reg-event-fx :send-message
  ;; `text` is the input-buffer string - it may carry two shorthand
  ;; surfaces:
  ;;
  ;;   1. `[Pasted #N: ...]` tokens for large clipboard payloads. These
  ;;      are expanded for BOTH the visible transcript and the agent.
  ;;   2. `@path/to/file` mentions inserted by the file picker. Those
  ;;      stay concise in the visible transcript, but expand into
  ;;      a short read-now directive for the AGENT; the model picks
  ;;      the right tool (`v/cat`, `z/symbols`, etc.) itself.
  (fn [db [_ text workspace-id]]
    (let [workspace-id (or workspace-id (current-workspace-id db))
          source-db    (db-for-workspace db workspace-id)
          visible-text (input/expand-paste-placeholders text (:pastes source-db))]
      (cond
        (transcript-dump-input? visible-text)
        {:db db
         :fx [[:notify "Input looks like copied assistant transcript; not sent" :warn 4000]]}

        (:loading? source-db)
        (enqueue-message-result db workspace-id text)

        (nil? (:session source-db))
        {:db db}

        :else
        (let [workspace    (active-workspace source-db)
              agent-text   (binding [workspace/*workspace-root* (workspace/workspace-root workspace)]
                             (input/expand-file-mentions visible-text))
              token        (vis/cancellation-token)
              extra-body   (turn-extra-body db)
              turn-features (cond-> {}
                              (get-in db [:settings :voice/respond?])
                              (assoc :voice-response? true))
              reasoning-level (when (reasoning-effort-configurable?)
                                (get-in db [:settings :reasoning-level]))
              client-turn-id (str (java.util.UUID/randomUUID))]
          {:db (update-workspace db workspace-id
                 (fn [w]
                   (-> w
                     (update :messages conj (assoc (chat/user-message visible-text)
                                              :client-turn-id client-turn-id))
                     (update :messages conj (assoc (chat/assistant-message pending-assistant-ir)
                                              :pending? true
                                              :client-turn-id client-turn-id))
                     (update :input-history (fn [xs]
                                              (let [xs (vec (or xs []))]
                                                (if (= visible-text (last xs)) xs (conj xs visible-text)))))
                     ;; Sending re-pins to the bottom: one atomic FOLLOW
                     ;; reset replaces the whole `:scroll` value, so no
                     ;; in-flight animation target can dangle and flash the
                     ;; view to the top of the freshly-appended message.
                     (assoc :scroll scroll/follow :loading? true
                       :cancel-token token
                       :cancelling? false
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
           ;; `visible-text` (un-expanded `@path` token) is the user's
           ;; original line - flowed in as `display-text` so it lands in
           ;; the persisted `user_request` column. Without the split,
           ;; reopening a session re-rendered the verbose attachment
           ;; directive in the user bubble.
           :fx [[:rlm-turn workspace-id (:session source-db) agent-text token
                 reasoning-level extra-body turn-features workspace client-turn-id
                 visible-text]]})))))

(reg-event-fx :enqueue-message
  ;; Capture a user submission while a previous turn is still processing.
  ;; Queue lives on that workspace/session and drains after the
  ;; in-flight turn commits. No provider call happens from this handler.
  (fn [db [_ text workspace-id]]
    (enqueue-message-result db workspace-id text)))

(reg-event-db :clear-pending-sends
  ;; Explicit user action - escape hatch when the queued items are no
  ;; longer wanted. Cancelling the in-flight turn must NOT auto-drop
  ;; them; that would reintroduce silent loss.
  (fn [db _]
    (update-workspace db (current-workspace-id db)
      (fn [w] (assoc w :pending-sends [])))))

(reg-event-fx :drain-pending
  ;; Pop one queued submission for `workspace-id`, restore its paste
  ;; snapshot onto that workspace, then schedule `:send-message` as an
  ;; effect after this DB update commits. Never dispatch from inside DB
  ;; mutation: swap! may retry and duplicate provider turns.
  (fn [db [_ workspace-id]]
    (let [head-atom (atom nil)
          db'       (update-workspace db workspace-id
                      (fn [w]
                        (let [q (vec (or (:pending-sends w) []))]
                          (if-let [h (first q)]
                            (do (reset! head-atom h)
                              (assoc w
                                :pending-sends (vec (rest q))
                                :pastes (or (:pastes h) {})
                                :paste-counter (or (:paste-counter h) 0)))
                            w))))]
      {:db db'
       :fx (if-let [h @head-atom]
             [[:dispatch [:send-message (:text h) workspace-id]]]
             [])})))

(reg-event-fx :cancel-turn
  (fn [db _]
    (if-not (:loading? db)
      {:db db}
      (do
        ;; Both the cooperative flag and the hard interrupt are fired
        ;; through one channel-agnostic call. See
        ;; channels.cancellation/cancel! for the contract.
        (vis/cancel! (:cancel-token db))
        {:db (assoc db :cancelling? true)
         :fx [[:notify "Cancelling current turn..." :info cancel-notification-ttl-ms]]}))))

(reg-event-db :set-progress-iterations
  (fn [db [_ a b]]
    (let [[workspace-id iterations] (if (keyword? a)
                                      [a b]
                                      [(current-workspace-id db) a])]
      (update-workspace db workspace-id
        (fn [workspace]
          (if-not (:loading? workspace)
            workspace
            (assoc-in workspace [:progress :iterations] (vec (or iterations [])))))))))

(reg-event-fx :message-received
  (fn [db [_ a b c]]
    (let [[workspace-id answer {:keys [model provider llm-selected llm-actual llm-fallback? llm-routing-trace iteration-count duration-ms tokens cost confidence session-turn-id status client-turn-id]}]
          (if (keyword? a)
            [a b c]
            [(current-workspace-id db) a b])
          drain? (volatile! false)
          db'    (update-workspace db workspace-id
                   (fn [workspace]
                     (let [trace (get-in workspace [:progress :iterations])
                           cancelled? (= :cancelled status)
                          ;; A cancellation that captured zero iterations is
                          ;; usually a stray Esc - drop the placeholder pair
                          ;; and restore the editor as before. A cancellation
                          ;; with a non-empty trace means the agent already
                          ;; did visible work (and persisted those iterations
                          ;; to SQLite); KEEP the bubble so the user can read
                          ;; what happened, and only repopulate the editor.
                           no-work? (empty? trace)]
                       (if (and cancelled? (:submitted-input workspace) no-work?)
                         (restore-submitted-input workspace (:submitted-input workspace))
                         (let [start    (:turn-start-ms workspace)
                               wall-ms  (when start (- (System/currentTimeMillis) start))
                              ;; `answer` arrives as canonical IR from `chat/turn!`
                              ;; (loop result coerced + lifted there). NULL/missing
                              ;; collapses to empty IR; we never feed strings to the
                              ;; render chokepoint.
                               answer-ir (or answer chat/empty-ir)
                               response (-> (chat/assistant-message answer-ir)
                                          (cond-> session-turn-id                (assoc :session-turn-id session-turn-id)
                                            (seq trace)
                                            (assoc :traces trace :ir answer-ir)
                                            (or duration-ms wall-ms) (assoc :duration-ms (or duration-ms wall-ms))
                                            model      (assoc :model model)
                                            provider   (assoc :provider provider)
                                            llm-selected (assoc :llm-selected llm-selected)
                                            llm-actual (assoc :llm-actual llm-actual)
                                            (some? llm-fallback?) (assoc :llm-fallback? llm-fallback?)
                                            (seq llm-routing-trace) (assoc :llm-routing-trace llm-routing-trace)
                                            iteration-count (assoc :iteration-count iteration-count)
                                            tokens     (assoc :tokens tokens)
                                            cost       (assoc :cost cost)
                                            confidence (assoc :confidence confidence)
                                            status     (assoc :status status)
                                            client-turn-id (assoc :client-turn-id client-turn-id)))
                               messages'      (replace-pending-assistant (:messages workspace) response)
                               still-pending? (boolean (some pending-assistant-message? messages'))
                               workspace'     (cond-> (assoc workspace
                                                        ;; Re-pin to the bottom by REPLACING `:scroll` with a
                                                        ;; fresh FOLLOW. A result can land atomically while an
                                                        ;; ease was in flight (e.g. a `/workspace list` table);
                                                        ;; replacing the whole value means no animation target
                                                        ;; can dangle, so the view snaps cleanly to the bottom
                                                        ;; instead of flashing to the top first.
                                                        :messages messages'
                                                        :scroll scroll/follow
                                                        :loading? still-pending?
                                                        :cancelling? false)
                                                (not still-pending?)
                                                (assoc :progress nil :cancel-token nil)

                                                (not still-pending?)
                                                (dissoc :turn-start-ms))
                              ;; Cancelled-with-work: keep the bubble we just
                              ;; built AND refill the editor from the snapshot so
                              ;; the user can edit/resubmit the prompt that
                              ;; produced this trace without retyping.
                               ws-final (if (and cancelled? (:submitted-input workspace) (not no-work?))
                                          (restore-editor-only workspace' (:submitted-input workspace))
                                          (cond-> workspace'
                                            (not still-pending?) (dissoc :submitted-input)))]
                           (when (and (not (:loading? ws-final))
                                   (seq (:pending-sends ws-final)))
                             (vreset! drain? true))
                           ws-final)))))]
      {:db db'
       :fx (cond-> []
             @drain? (conj [:dispatch [:drain-pending workspace-id]])
             ;; Ring the terminal bell when a turn finishes in a tab that
             ;; ISN'T focused — that's the one case you can't see complete on
             ;; screen, so the background session needs to announce itself.
             (and (not= workspace-id (current-workspace-id db))
               (not= :cancelled status))
             (conj [:bell]))})))

;;; ── Side effects ───────────────────────────────────────────────────────────

(defn- speak-answer-async!
  [answer]
  (try
    (when-let [speak (requiring-resolve 'com.blockether.vis.ext.foundation-voice.core/speak-answer-async!)]
      (speak answer))
    (catch Throwable t
      (vis/notify! (str "Voice response failed: " (or (ex-message t) t))
        :level :error :ttl-ms 5000))))

(reg-fx :dispatch
  (fn [event]
    (dispatch event)))

(reg-fx :notify
  (fn [text level ttl-ms]
    (vis/notify! text :level level :ttl-ms ttl-ms)))

(reg-fx :bell
  ;; Write a raw BEL (0x07) to the terminal. BEL doesn't move the cursor, so
  ;; interleaving it with Lanterna's output is safe; the terminal turns it
  ;; into an audible/visible bell per the user's terminal settings.
  (fn []
    (try
      (when-let [^java.io.OutputStream out @vis/tty-out]
        (.write out 7)
        (.flush out))
      (catch Throwable _ nil))))

(reg-fx :apply-config
  (fn [config]
    (let [raw        (or (vis/load-config-raw) {})
          persistent (assoc raw :providers (vec (:providers config)))]
      (vis/save-config! persistent)
      (let [resolved (or (vis/reload-config!) config)
            router   (vis/rebuild-router! resolved)]
        (vis/refresh-cached-routers! router)))))

(reg-fx :rlm-turn
  (fn [workspace-id session text token reasoning-level extra-body turn-features workspace client-turn-id
       & [display-text]]
    (let [fut (vis/worker-future "vis-tui-turn"
                (fn []
                  (try
                    (let [progress-update! (make-progress-render-updater
                                             (fn [[_ timeline]]
                                               (try (dispatch [:set-progress-iterations workspace-id timeline])
                                                 (catch Throwable _ nil))))
                          {:keys [on-chunk]}
                          (vis/make-progress-tracker
                            {:on-update progress-update!})
                          result (chat/turn! session text
                                   {:on-chunk          on-chunk
                                    ;; Pass the cancellation TOKEN, not the
                                    ;; bare atom: the loop registers SCI /
                                    ;; provider workers with the token's
                                    ;; `on-cancel!` callback registry so
                                    ;; `vis/cancel!` hard-cancels them all
                                    ;; at once instead of waiting on each
                                    ;; one's eval-timeout.
                                    :cancel-token      token
                                    :reasoning-default reasoning-level
                                    :extra-body        extra-body
                                    :turn-features     turn-features
                                    :workspace         workspace
                                    :display-text      display-text})]
                      (if (:error result)
                        (dispatch [:message-received workspace-id
                                   (vis/markdown->ir (vis/format-error (:error result)))
                                   {:client-turn-id client-turn-id}])
                        (do
                          (dispatch [:message-received workspace-id (:answer result)
                                     (assoc (select-keys result
                                              [:model :provider :llm-selected :llm-actual
                                               :llm-fallback? :llm-routing-trace
                                               :iteration-count :duration-ms :tokens
                                               :cost :confidence :session-turn-id :status])
                                       :client-turn-id client-turn-id)])
                          ;; A /draft new|apply|abandon turn may have switched the
                          ;; session's workspace — re-sync so header/footer reflect it.
                          (try
                            (let [d   (vis/db-info)
                                  sid (some->> (:id session) (vis/db-latest-session-state-id d))
                                  ws  (when sid (vis/workspace-for-session d sid))]
                              (dispatch [:set-workspace ws]))
                            (catch Throwable _ nil))
                          (when (:voice-response? turn-features)
                            (speak-answer-async! (:answer result))))))
                    (catch Throwable t
                    ;; channels.cancellation/cancellation? folds in
                    ;; InterruptedException, CancellationException, and
                    ;; runtime wrappers around them - keep all the
                    ;; channel-shaped logic in one place. The bubble
                    ;; renderer dims the result based on `:status
                    ;; :cancelled`, so we attach it explicitly here.
                      (if (vis/cancellation? t)
                        (dispatch [:message-received workspace-id
                                   [:ir {} [:p {} [:span {} "Cancelled by user."]]]
                                   {:status :cancelled
                                    :client-turn-id client-turn-id}])
                        (dispatch [:message-received workspace-id
                                   (vis/markdown->ir (vis/format-error (or (ex-message t) (str t))))
                                   {:client-turn-id client-turn-id}]))))))]
      (vis/cancellation-set-future! token fut))))
