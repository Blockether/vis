(ns com.blockether.vis.ext.channel-tui.state
  "Re-frame-like state management for the TUI.
   Single app-db atom, pure event handlers, side effects via reg-fx."
  (:require [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.internal.theme :as shared-theme]
            [com.blockether.vis.ext.channel-tui.chat :as chat]
            [com.blockether.vis.ext.channel-tui.command-suggest :as slash]
            [com.blockether.vis.ext.channel-tui.theme :as tui-theme]
            [com.blockether.vis.ext.channel-tui.input :as input]
            [com.blockether.vis.ext.channel-tui.render :as render]
            [com.blockether.vis.internal.workspace :as workspace]))

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
  [:conversation
   :workspace
   :workspace/root
   :title
   :messages
   :messages-scroll
   :input
   :input-history
   :input-history-index
   :input-history-draft
   :slash-command-index
   :slash-command-hidden?
   :submitted-input
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
  {:conversation nil
   :workspace nil
   :workspace/root nil
   :title nil
   :messages []
   :messages-scroll nil
   :input (input/empty-input)
   :input-history []
   :input-history-index nil
   :input-history-draft nil
   :slash-command-index 0
   :slash-command-hidden? false
   :submitted-input nil
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
    (:id (some #(when (:active? %) %) (:workspace-tabs db)))
    (:id (first (:workspace-tabs db)))))

(defn- active-workspace-tab
  [db]
  (let [active-id (current-workspace-id db)]
    (some #(when (= (:id %) active-id) %) (:workspace-tabs db))))

(defn- active-workspace
  [db]
  (or (:workspace db)
    (some-> (active-workspace-tab db) :workspace)
    (when-let [root (or (:workspace/root db)
                      (some-> (active-workspace-tab db) :workspace/root))]
      {:workspace/root root})))

(defn- sync-active-workspace
  [db]
  (if-let [id (current-workspace-id db)]
    (assoc-in db [:workspaces id] (workspace-snapshot db))
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
;;  :conversation nil            ;; {:id conversation-id} or nil - handle to the shared conversations cache
;;  :messages   []               ;; [{:role :user|:assistant :text str :timestamp #inst}]
;;  :messages-scroll nil              ;; row offset into bubbles, nil = auto-bottom
;;  :input      {:lines [""] :crow 0 :ccol 0}
;;  :input-history []            ;; persisted user queries for this conversation
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

(defn- make-progress-render-updater
  ([dispatch-fn]
   (make-progress-render-updater dispatch-fn #(System/currentTimeMillis)))
  ([dispatch-fn now-ms-fn]
   (let [last-render-ms (atom nil)]
     (fn [timeline chunk]
       (let [now-ms (long (or (now-ms-fn) 0))
             reasoning? (= :reasoning (:phase chunk))
             due? (or (nil? @last-render-ms)
                    (>= (- now-ms (long @last-render-ms))
                      live-progress-render-interval-ms))]
         (when (or (not reasoning?) due?)
           (reset! last-render-ms now-ms)
           (dispatch-fn [:set-progress-iterations timeline])))))))

(def ^:private reasoning-level-order
  [:quick :balanced :deep])

(def ^:private codex-verbosity-order
  [:low :medium :high])

(defn- normalize-reasoning-level
  "Canonical reasoning level for persisted TUI settings.

   Accepts the native UI vocabulary (`:quick/:balanced/:deep`) plus the
   low/medium/high aliases the shared turn engine already accepts.
   Unknown values fall back to `:balanced` so a hand-edited config never
   wedges the TUI into an invalid state."
  [v]
  (case (cond
          (keyword? v) v
          (string? v)  (keyword (str/lower-case (str/trim v)))
          :else        nil)
    :low      :quick
    :medium   :balanced
    :high     :deep
    :quick    :quick
    :balanced :balanced
    :deep     :deep
    :balanced))

(defn- normalize-codex-verbosity
  "Canonical OpenAI Codex verbosity level for persisted TUI settings.

   Accepts `:low/:medium/:high` plus strings; unknown values fall back
   to `:low`, matching the provider's current default."
  [v]
  (case (cond
          (keyword? v) v
          (string? v)  (keyword (str/lower-case (str/trim v)))
          :else        nil)
    :low    :low
    :medium :medium
    :high   :high
    :low))

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
  [settings]
  (-> settings
    (update :theme-name normalize-theme-name)
    (update :reasoning-level normalize-reasoning-level)
    (update :openai-codex-verbosity normalize-codex-verbosity)
    ;; Provider reasoning/thinking is forensic data, not chat UI. Keep it
    ;; hidden even if an old config persisted `:show-thinking true`; the
    ;; transcript/reproduction surfaces still retain it for debugging.
    (assoc :show-thinking false)
    (update :show-iterations boolean)
    (update :show-silent boolean)
    (update :show-timestamps boolean)
    (update :differentiate-turns boolean)
    (update :mouse-selection-copy boolean)
    (update :voice/respond? boolean)
    (update :contributors-disabled
      (fn [v] (cond (nil? v) #{} (set? v) v :else (set v))))))

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

     differentiate-turns - visual turn separator between a Vis answer and
         the next You prompt. Default ON so transcript turns read as distinct
         request/response units without drawing borders inside reasoning.

     mouse-selection-copy - app-side terminal selection. Default ON so
         drag-selecting visible text copies it on mouse release even while
         the fullscreen TUI has mouse reporting enabled.

   The previous `:show-iteration-headers` and `:show-final-answer-header`
   toggles were removed: the ITERATION N / CODE N / STDOUT / ERROR /
   FINAL ANSWER superscripts they controlled have been deleted from
   the rendering pipeline outright (the visual zones already convey
   the same boundaries without the labels)."
  {:theme-name            (keyword shared-theme/default-theme-id)
   :show-thinking          false
   :show-iterations        true
   :show-silent            false
   :reasoning-level        :balanced
   :openai-codex-verbosity :low
   :show-timestamps        false
   :differentiate-turns    true
   :mouse-selection-copy   true
   :voice/respond?         false
   ;; Set of contributor ids the user wants hidden in the TUI
   ;; header / footer. Each extension that contributes a row /
   ;; segment / status registers under a keyword id (e.g. :goal
   ;; from vis-goal, :voice from vis-voice). Adding the id to this
   ;; set skips that contributor's rendering. Default empty (every
   ;; registered contributor shows). See
   ;; `com.blockether.vis.ext.channel-tui.contributors`.
   :contributors-disabled  #{}})

(defn- load-persisted-settings
  "Read `:tui-settings` from `~/.vis/config.edn` and merge over
   `default-settings`. Missing keys fall back to defaults; unknown
   keys are dropped so a forward-compatible future we add new
   toggles into a previously-saved config doesn't blow up here."
  []
  (let [raw (try (vis/load-config-raw) (catch Throwable _ nil))
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
  [db new-settings]
  ;; Settings (show-thinking?, show-iterations?, etc.) are part of
  ;; every format-answer cache key, so toggling them already
  ;; invalidates cache entries naturally. But the OLD entries
  ;; linger until the cache fills - which on a quiet conversation
  ;; never happens. Drop them now so memory stays bounded across
  ;; many toggles.
  (render/invalidate-cache!)
  (let [merged (normalize-settings (merge default-settings (:settings db) new-settings))]
    (tui-theme/apply-theme! (:theme-name merged))
    (persist-settings! merged)
    (assoc db :settings merged)))

(defn- cycle-choice
  [choices current]
  (let [choices (vec choices)
        idx     (.indexOf ^java.util.List choices current)]
    (nth choices (mod (inc (if (neg? idx) 0 idx)) (count choices)))))

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

(def ^:private ^:const max-workspace-tabs 8)

(defn- workspace-tab-number
  [tab]
  (when-let [[_ n] (some->> tab :id name (re-matches #"tab-(\d+)"))]
    (Long/parseLong n)))

(defn- next-workspace-tab-number
  [tabs]
  (inc (reduce max 0 (keep workspace-tab-number tabs))))

(defn- base-workspace-tab
  [db]
  {:id    (or (:active-workspace-id db) :main)
   :label (let [title (:title db)]
            (if (and (string? title) (not (str/blank? title)))
              title
              "Main"))})

(defn- workspace-tabs-or-base
  [db]
  (let [tabs (vec (:workspace-tabs db))]
    (if (seq tabs)
      tabs
      [(base-workspace-tab db)])))

(defn- active-tab-label
  [db fallback]
  (let [title (:title db)]
    (if (and (string? title) (not (str/blank? title)))
      title
      fallback)))

(defn- ensure-workspace-tabs
  [db]
  (let [tabs (workspace-tabs-or-base db)
        active-id (or (current-workspace-id (assoc db :workspace-tabs tabs))
                    (:id (first tabs)))]
    (assoc db
      :workspace-tabs (mapv (fn [tab]
                              (cond-> (dissoc tab :active?)
                                (= (:id tab) active-id) (assoc :active? true)))
                        tabs)
      :active-workspace-id active-id)))

(defn- restore-workspace
  [db workspace-id]
  (merge db (or (get-in db [:workspaces workspace-id])
              (empty-workspace-state))))

(defn- activate-workspace
  [db workspace-id]
  (-> db
    sync-active-workspace
    (assoc :active-workspace-id workspace-id)
    (update :workspace-tabs
      (fn [tabs]
        (mapv (fn [tab]
                (cond-> (dissoc tab :active?)
                  (= (:id tab) workspace-id) (assoc :active? true)))
          tabs)))
    (restore-workspace workspace-id)))

(defn- update-workspace
  [db workspace-id f]
  (let [workspace-id (or workspace-id (current-workspace-id db))]
    (if (and workspace-id (not= workspace-id (current-workspace-id db)))
      (update-in db [:workspaces workspace-id]
        (fn [snapshot]
          (workspace-snapshot
            (f (merge db (or snapshot (empty-workspace-state)))))))
      (f db))))

(defn- workspace-conversation-id
  [db workspace-id]
  (some-> (get-in db [:workspaces workspace-id :conversation :id]) str))

(defn- reasoning-effort-configurable?
  []
  (let [info (current-model-info)]
    (or (nil? info)
      (and (boolean (:reasoning? info))
        (not= false (:reasoning-effort? info))
        (not= :zai-thinking (:reasoning-style info))))))

(defn init!
  "Initialize app-db with default state."
  []
  (let [settings (load-persisted-settings)]
    (tui-theme/apply-theme! (:theme-name settings))
    (reset! app-db {:config     nil
                    :conversation nil
                    :title      nil
                    :messages   []
                    :messages-scroll nil
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
                    :workspace-tabs []
                    :active-workspace-id nil
                    :workspaces {}
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

(reg-event-fx :cycle-reasoning-level
  (fn [db _]
    (if-not (reasoning-effort-configurable?)
      {:db db
       :fx [[:notify "Reasoning effort is not configurable for this model" :warn settings-notification-ttl-ms]]}
      (let [current (normalize-reasoning-level (get-in db [:settings :reasoning-level]))
            next    (cycle-choice reasoning-level-order current)]
        {:db (apply-settings-update! db {:reasoning-level next})
         :fx [[:notify (str "Reasoning: " (name next)) :info settings-notification-ttl-ms]]}))))

(reg-event-fx :cycle-codex-verbosity
  (fn [db _]
    (if-not (= :openai-codex (current-provider-id))
      {:db db
       :fx [[:notify "Codex verbosity is only available for OpenAI Codex" :warn settings-notification-ttl-ms]]}
      (let [current (normalize-codex-verbosity (get-in db [:settings :openai-codex-verbosity]))
            next    (cycle-choice codex-verbosity-order current)]
        {:db (apply-settings-update! db {:openai-codex-verbosity next})
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
  (fn [db [_ conversation-id node-id]]
    (let [k [(str conversation-id) (str node-id)]]
      (update db :detail-expansions
        (fn [m]
          (let [expanded? (true? (get m k false))]
            (if expanded?
              (dissoc m k)
              (assoc (or m {}) k true))))))))

(reg-event-db :select-preview-mode
  (fn [db [_ conversation-id node-id mode]]
    (assoc-in db [:detail-expansions [(str conversation-id) (str node-id)]] mode)))

(reg-event-db :bump-render-version
  (fn [db _]
    ;; No-op state mutator. The dispatcher itself bumps
    ;; `:render-version` and notifies the render monitor whenever an
    ;; event lands (unless the event id is in `no-render-bump-events`),
    ;; so simply dispatching this event is enough to wake the painter.
    ;; Used by the mouse handler when a hover-state change needs the
    ;; chrome row repainted with its hover background.
    db))

(reg-event-db :add-workspace-tab
  (fn [db [_ opts]]
    (let [db        (-> db ensure-workspace-tabs sync-active-workspace)
          tabs      (vec (:workspace-tabs db))
          n         (next-workspace-tab-number tabs)
          id        (keyword (str "tab-" n))
          workspace (:workspace opts)
          root      (or (:workspace/root workspace) (:workspace/root opts))
          label     (or (:label opts)
                      (some-> workspace :main :branch)
                      (str "Tab " n))
          tab       (cond-> {:id id :label label :active? true}
                      workspace (assoc :workspace workspace)
                      root      (assoc :workspace/root root))]
      (if (>= (count tabs) max-workspace-tabs)
        db
        (cond-> (-> db
                  (assoc :workspace-tabs (conj (mapv #(dissoc % :active?) tabs) tab)
                    :active-workspace-id id)
                  (merge (empty-workspace-state)))
          workspace (assoc :workspace workspace)
          root      (assoc :workspace/root root))))))

(reg-event-db :select-workspace-tab-index
  (fn [db [_ idx]]
    (let [db   (-> db ensure-workspace-tabs sync-active-workspace)
          tabs (vec (:workspace-tabs db))
          idx  (if (= :next idx)
                 (when (seq tabs)
                   (let [active-id (or (:active-workspace-id db)
                                     (:id (some #(when (:active? %) %) tabs))
                                     (:id (first tabs)))
                         current   (or (first (keep-indexed #(when (= (:id %2) active-id) %1) tabs))
                                     -1)]
                     (mod (inc current) (count tabs))))
                 idx)]
      (if-let [tab (and (integer? idx) (nth tabs idx nil))]
        (activate-workspace db (:id tab))
        db))))

(reg-event-db :select-workspace-tab-conversation-id
  (fn [db [_ conversation-id]]
    (let [target-id (some-> conversation-id str)
          db        (-> db ensure-workspace-tabs sync-active-workspace)
          tabs      (vec (:workspace-tabs db))
          tab       (when target-id
                      (some #(when (= target-id (workspace-conversation-id db (:id %))) %)
                        tabs))]
      (if tab
        (activate-workspace db (:id tab))
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

(reg-event-db :init-conversation
  (fn [db [_ conversation history]]
    (let [user-history (->> (or history [])
                         (filter #(= :user (:role %)))
                         (mapv :text))]
      (-> db
        ensure-workspace-tabs
        (assoc :conversation conversation
          :title nil
          :messages (or history [])
          :messages-scroll nil
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

(reg-event-db :set-title
  (fn [db [_ title]]
    (let [db' (assoc db :title title)
          active-id (current-workspace-id db')]
      (cond-> db'
        active-id
        (update :workspace-tabs
          (fn [tabs]
            (mapv (fn [tab]
                    (cond-> tab
                      (= (:id tab) active-id)
                      (assoc :label (active-tab-label db' (:label tab)))))
              tabs)))))))

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
  [db {:keys [text pastes paste-counter input-history]}]
  (-> db
    (assoc :messages (drop-pending-turn-messages (:messages db))
      :messages-scroll nil
      :input (text->input-state text)
      :input-history (vec (or input-history []))
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
    (dissoc :turn-start-ms :submitted-input)))

(defn- restore-editor-only
  "Repopulate the editor without touching `:messages`. Used when
   a turn was cancelled AFTER iterations produced visible work -
   we keep the cancelled bubble (with its `:traces`) in the
   transcript so the user can see what the agent did, AND we
   refill the input box with the original prompt so they can
   tweak/resubmit without retyping. The conversation-turn row,
   each completed iteration, and any blocks they wrote already
   landed in SQLite via the iteration loop's per-iteration
   `db-store-iteration!` calls and `finalize-turn-result`'s
   `db-update-conversation-turn!`, so reopening the conversation
   shows the same partial trace."
  [db {:keys [text pastes paste-counter input-history]}]
  (-> db
    (assoc :input (text->input-state text)
      :input-history (vec (or input-history []))
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

(reg-event-db :set-scroll
  (fn [db [_ scroll]]
    (assoc db :messages-scroll scroll)))

(reg-event-db :scroll-to-message
  ;; In-conversation search lands here after the user picks a hit.
  ;; The painter doesn't get told an exact :messages-scroll Y value
  ;; (which it would need to compute heights for); instead it sees
  ;; `:scroll-to-message-pending` and re-resolves the scroll target
  ;; on the next frame, then clears the pending field. One-shot.
  (fn [db [_ msg-idx]]
    (cond-> db
      (and (integer? msg-idx) (>= msg-idx 0))
      (assoc :scroll-to-message-pending msg-idx))))

(reg-event-db :scroll-to-message-resolved
  ;; Painter calls this after consuming `:scroll-to-message-pending`
  ;; so the same hit doesn't re-scroll on every redraw.
  (fn [db _]
    (dissoc db :scroll-to-message-pending)))

(reg-event-db :scroll-up
  (fn [db [_ amount total-h inner-h]]
    (let [max-s (max 0 (- total-h inner-h))
          cur   (or (:messages-scroll db) max-s)]
      (assoc db :messages-scroll (max 0 (- cur amount))))))

(reg-event-db :scroll-down
  (fn [db [_ amount total-h inner-h]]
    (let [max-s (max 0 (- total-h inner-h))
          cur   (or (:messages-scroll db) max-s)]
      (if (>= (+ cur amount) max-s)
        (assoc db :messages-scroll nil)
        (assoc db :messages-scroll (min max-s (+ cur amount)))))))

;; Scrollbar mouse drag/click. Mirrors the thumb-positioning math in
;; `render/draw-messages-area!` so a click on the bar lands the thumb
;; top at the cursor row, and a drag follows the cursor. `bar-top` is
;; the row of the topmost track cell; `track-h` is the track length
;; in rows; `total-h`/`inner-h` are the layout sizes the render thread
;; published into app-db.
(reg-event-db :scroll-to-y
  (fn [db [_ mouse-y bar-top track-h total-h inner-h]]
    (if (or (<= total-h inner-h) (<= track-h 0))
      db
      (let [thumb-h    1
            max-scroll (max 0 (- total-h inner-h))
            relative   (- mouse-y bar-top)
            denom      (max 1 (- track-h thumb-h))
            fraction   (max 0.0 (min 1.0 (double (/ relative denom))))
            new-scroll (long (Math/round (* fraction max-scroll)))]
        (assoc db :messages-scroll (max 0 (min max-scroll new-scroll)))))))

(defn- turn-extra-body
  [{:keys [settings]}]
  (when (= :openai-codex (current-provider-id))
    {:text {:verbosity (name (or (:openai-codex-verbosity settings) :low))}}))

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
  (fn [db [_ text]]
    (let [visible-text (input/expand-paste-placeholders text (:pastes db))
          workspace-id (current-workspace-id db)
          workspace    (active-workspace db)
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
      {:db (-> db
             (update :messages conj (assoc (chat/user-message visible-text)
                                      :client-turn-id client-turn-id))
             (update :messages conj (assoc (chat/assistant-message pending-assistant-ir)
                                      :pending? true
                                      :client-turn-id client-turn-id))
             (update :input-history (fn [xs]
                                      (let [xs (vec (or xs []))]
                                        (if (= visible-text (last xs)) xs (conj xs visible-text)))))
             (assoc :messages-scroll nil :loading? true
               :cancel-token token
               :cancelling? false
               :progress {:iterations []}
               :turn-start-ms (System/currentTimeMillis)
               :submitted-input {:text text
                                 :pastes (:pastes db)
                                 :paste-counter (:paste-counter db)
                                 :input-history (vec (or (:input-history db) []))}
               :input-history-index nil
               :input-history-draft nil
               :slash-command-index 0
               :slash-command-hidden? false))
       ;; `agent-text` (LLM-facing, with `@path` expanded into a
       ;; `[Attached File: ...]` directive) drives the model.
       ;; `visible-text` (un-expanded `@path` token) is the user's
       ;; original line - flowed in as `display-text` so it lands in
       ;; the persisted `user_request` column. Without the split,
       ;; reopening a conversation re-rendered the verbose attachment
       ;; directive in the user bubble.
       :fx [[:rlm-turn workspace-id (:conversation db) agent-text token
             reasoning-level extra-body turn-features workspace client-turn-id
             visible-text]]})))

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

(reg-event-db :message-received
  (fn [db [_ a b c]]
    (let [[workspace-id answer {:keys [model provider llm-selected llm-actual llm-fallback? llm-fallback-trace iteration-count duration-ms tokens cost confidence conversation-turn-id status client-turn-id]}]
          (if (keyword? a)
            [a b c]
            [(current-workspace-id db) a b])]
      (update-workspace db workspace-id
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
                               (cond-> conversation-turn-id                (assoc :conversation-turn-id conversation-turn-id)
                                 (seq trace)
                                 (assoc :traces trace :ir answer-ir)
                                 (or duration-ms wall-ms) (assoc :duration-ms (or duration-ms wall-ms))
                                 model      (assoc :model model)
                                 provider   (assoc :provider provider)
                                 llm-selected (assoc :llm-selected llm-selected)
                                 llm-actual (assoc :llm-actual llm-actual)
                                 (some? llm-fallback?) (assoc :llm-fallback? llm-fallback?)
                                 (seq llm-fallback-trace) (assoc :llm-fallback-trace llm-fallback-trace)
                                 iteration-count (assoc :iteration-count iteration-count)
                                 tokens     (assoc :tokens tokens)
                                 cost       (assoc :cost cost)
                                 confidence (assoc :confidence confidence)
                                 status     (assoc :status status)
                                 client-turn-id (assoc :client-turn-id client-turn-id)))
                    messages'      (replace-pending-assistant (:messages workspace) response)
                    still-pending? (boolean (some pending-assistant-message? messages'))
                    workspace'     (cond-> (assoc workspace
                                             :messages messages'
                                             :messages-scroll nil
                                             :loading? still-pending?
                                             :cancelling? false)
                                     (not still-pending?)
                                     (assoc :progress nil :cancel-token nil)

                                     (not still-pending?)
                                     (dissoc :turn-start-ms))]
                ;; Cancelled-with-work: keep the bubble we just
                ;; built AND refill the editor from the snapshot so
                ;; the user can edit/resubmit the prompt that
                ;; produced this trace without retyping.
                (if (and cancelled? (:submitted-input workspace) (not no-work?))
                  (restore-editor-only workspace' (:submitted-input workspace))
                  (cond-> workspace'
                    (not still-pending?) (dissoc :submitted-input)))))))))))

;;; ── Side effects ───────────────────────────────────────────────────────────

(defn- speak-answer-async!
  [answer]
  (try
    (when-let [speak (requiring-resolve 'com.blockether.vis.ext.voice.core/speak-answer-async!)]
      (speak answer))
    (catch Throwable t
      (vis/notify! (str "Voice response failed: " (or (ex-message t) t))
        :level :error :ttl-ms 5000))))

(reg-fx :notify
  (fn [text level ttl-ms]
    (vis/notify! text :level level :ttl-ms ttl-ms)))

(reg-fx :apply-config
  (fn [config]
    (let [raw        (or (vis/load-config-raw) {})
          persistent (assoc raw :providers (vec (:providers config)))]
      (vis/save-config! persistent)
      (let [resolved (or (vis/reload-config!) config)
            router   (vis/rebuild-router! resolved)]
        (vis/refresh-cached-routers! router)))))

(reg-fx :rlm-turn
  (fn [workspace-id conversation text token reasoning-level extra-body turn-features workspace client-turn-id
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
                          result (chat/turn! conversation text
                                   {:on-chunk          on-chunk
                                    :cancel-atom       (vis/cancellation-atom token)
                                    :reasoning-default reasoning-level
                                    :extra-body        extra-body
                                    :turn-features     turn-features
                                    :workspace         workspace
                                    :display-text      display-text})]
                      (if (:error result)
                        (dispatch [:message-received workspace-id
                                   (vis/text->ir (vis/format-error (:error result)))
                                   {:client-turn-id client-turn-id}])
                        (do
                          (dispatch [:message-received workspace-id (:answer result)
                                     (assoc (select-keys result
                                              [:model :iteration-count :duration-ms :tokens
                                               :cost :confidence :conversation-turn-id :status])
                                       :client-turn-id client-turn-id)])
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
                                   (vis/text->ir (vis/format-error (or (ex-message t) (str t))))
                                   {:client-turn-id client-turn-id}]))))))]
      (vis/cancellation-set-future! token fut))))
