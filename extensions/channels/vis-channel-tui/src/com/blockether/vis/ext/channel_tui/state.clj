(ns com.blockether.vis.ext.channel-tui.state
  "Re-frame-like state management for the TUI.
   Single app-db atom, pure event handlers, side effects via reg-fx."
  (:require [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.channel-tui.chat :as chat]
            [com.blockether.vis.ext.channel-tui.input :as input]
            [com.blockether.vis.ext.channel-tui.render :as render]
            [com.blockether.vis.ext.channel-tui.virtual :as virtual]))

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
;; livelock: render writes layout → version bumps → render wakes → same
;; layout → same version bump → …

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
                (let [db' ((:fn handler) db event-vec)]
                  (if bump? (bump-version db') db'))))
        :fx (let [{:keys [db fx]} ((:fn handler) @app-db event-vec)]
              (when db
                (reset! app-db (if bump? (bump-version db) db)))
              (doseq [[fx-id & args] fx]
                (when-let [fx-fn (get @fx-registry fx-id)]
                  (apply fx-fn args)))))
      (when bump? (notify-render!)))
    (throw (ex-info (str "No handler registered for event: " id) {:event event-vec}))))

;;; ── State shape ────────────────────────────────────────────────────────────
;;
;; {:config     nil              ;; provider config map or nil
;;  :conversation nil            ;; {:id conversation-id} or nil — handle to the shared conversations cache
;;  :messages   []               ;; [{:role :user|:assistant :text str :timestamp #inst}]
;;  :messages-scroll nil              ;; row offset into bubbles, nil = auto-bottom
;;  :input      {:lines [""] :crow 0 :ccol 0}
;;  :input-history []            ;; persisted user queries for this conversation
;;  :input-history-index nil     ;; nil = editing live draft, 0 = newest history entry
;;  :input-history-draft nil     ;; unsent draft preserved while browsing history
;;  :loading?   false            ;; true while RLM is working
;;  :cancel-token nil            ;; channels.cancellation token for the
;;                               ;; in-flight turn (nil when idle). Holds
;;                               ;; the cooperative flag + the worker
;;                               ;; future so :cancel-query can hit both.
;;  :cancelling? false           ;; true once Esc was pressed; cleared on :message-received
;;  :progress   nil              ;; live per-iteration timeline while loading:
;;                               ;;   {:iterations [{:iteration int
;;                               ;;                  :thinking  str-or-nil
;;                               ;;                  :code      [str]       ;; latest streamed forms
;;                               ;;                  :final?    bool}]}
;;                               ;; Cleared on :message-received.
;;  :settings  {:show-thinking true :show-iterations true}
;;  :dialog-open? false}         ;; dialog singleton guard
;;

(defn- normalize-reasoning-level
  "Canonical reasoning level for persisted TUI settings.

   Accepts the native UI vocabulary (`:quick/:balanced/:deep`) plus the
   low/medium/high aliases the shared query engine already accepts.
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

(defn- normalize-settings
  [settings]
  (-> settings
    (update :reasoning-level normalize-reasoning-level)
    (update :show-thinking boolean)
    (update :show-iterations boolean)
    (update :show-timestamps boolean)))

(def default-settings
  "Per-user TUI settings. Persisted to `~/.vis/config.edn` under
   `:tui-settings`.

     show-thinking / show-iterations  — high-signal content controls.
         Default ON because new users want to SEE the agent reasoning;
         power users turn them off when they want a clean transcript.

     reasoning-level — base model thinking depth for reasoning-capable
         models. Default `:balanced`; users can cycle it via
         Ctrl+K → Toggles.

     show-timestamps — chrome control. Default OFF because timestamps
         duplicate info already on screen.

   The previous `:show-iteration-headers` and `:show-final-answer-header`
   toggles were removed: the ITERATION N / CODE N / STDOUT / ERROR /
   FINAL ANSWER superscripts they controlled have been deleted from
   the rendering pipeline outright (the visual zones already convey
   the same boundaries without the labels)."
  {:show-thinking   true
   :show-iterations true
   :reasoning-level :balanced
   :show-timestamps false})

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
   are swallowed — a config-save failure should never crash a TUI
   that's already otherwise healthy."
  [settings]
  (try
    (let [raw (or (vis/load-config-raw) {})]
      (vis/save-config! (assoc raw :tui-settings settings)))
    (catch Throwable _ nil)))

(defn init!
  "Initialize app-db with default state."
  []
  (reset! app-db {:config     nil
                  :conversation nil
                  :title      nil
                  :messages   []
                  :messages-scroll nil
                  :input      (input/empty-input)
                  :input-history []
                  :input-history-index nil
                  :input-history-draft nil
                  ;; Pi-style paste registry. Each multi-line / large
                  ;; clipboard payload lands here keyed by an auto-
                  ;; incrementing id; the input buffer carries a
                  ;; placeholder token `[Pasted #N: …]` instead of the
                  ;; raw text. Send-time substitution uses this map
                  ;; to materialise the full content before the
                  ;; message reaches the agent. Cleared on send.
                  :pastes     {}
                  :paste-counter 0
                  :loading?   false
                  :cancel-token nil
                  :cancelling? false
                  :progress   nil
                  :settings   (load-persisted-settings)
                  :detail-expansions {}
                  :dialog-open? false
                  ;; Render thread coordination — see render-monitor docstring.
                  :render-version 0
                  :shutdown? false
                  ;; Populated by the render thread after each frame so the
                  ;; input thread's scroll handlers know how big the
                  ;; messages area is right now. nil before the first paint.
                  :layout nil}))

;;; ── Pure event handlers ────────────────────────────────────────────────────

(reg-event-db :set-config
  (fn [db [_ config]]
    (vis/reload-config!)
    (when (seq (:providers config))
      ;; rebuild-router! only swaps the global singleton; cached envs
      ;; keep the snapshot they were created with. Reseat them too,
      ;; otherwise the next query runs against the previous model
      ;; even though the status bar already shows the new one.
      (let [r (vis/rebuild-router! config)]
        (vis/refresh-cached-routers! r)))
    (assoc db :config config)))

(reg-event-db :set-dialog-open
  (fn [db [_ open?]]
    (assoc db :dialog-open? (boolean open?))))

(reg-event-db :update-settings
  (fn [db [_ new-settings]]
    ;; Settings (show-thinking?, show-iterations?, etc.) are part of
    ;; every format-answer cache key, so toggling them already
    ;; invalidates cache entries naturally. But the OLD entries
    ;; linger until the cache fills — which on a quiet conversation
    ;; never happens. Drop them now so memory stays bounded across
    ;; many toggles.
    (render/invalidate-cache!)
    (let [merged (normalize-settings (merge (:settings db) new-settings))]
      (persist-settings! merged)
      (assoc db :settings merged))))

(reg-event-db :set-layout
  (fn [db [_ layout]]
    ;; Pushed in by the render thread; intentionally does NOT bump
    ;; render-version (see no-render-bump-events).
    (assoc db :layout layout)))

(reg-event-db :toggle-detail
  (fn [db [_ conversation-id node-id]]
    (render/invalidate-cache!)
    (virtual/invalidate-heights!)
    (let [k [(str conversation-id) (str node-id)]]
      (update db :detail-expansions
        (fn [m]
          (let [expanded? (true? (get m k false))]
            (if expanded?
              (dissoc m k)
              (assoc (or m {}) k true))))))))

(reg-event-db :bump-render-version
  (fn [db _]
    ;; No-op state mutator. The dispatcher itself bumps
    ;; `:render-version` and notifies the render monitor whenever an
    ;; event lands (unless the event id is in `no-render-bump-events`),
    ;; so simply dispatching this event is enough to wake the painter.
    ;; Used by the mouse handler when a hover-state change needs the
    ;; chrome row repainted with its hover background.
    db))

(reg-event-db :shutdown
  (fn [db _]
    (assoc db :shutdown? true)))

(reg-event-db :init-conversation
  (fn [db [_ conversation history]]
    (let [user-history (->> (or history [])
                         (filter #(= :user (:role %)))
                         (mapv :text))]
      (assoc db
        :conversation conversation
        :messages (or history [])
        :input-history user-history
        :input-history-index nil
        :input-history-draft nil
        :detail-expansions {}))))

(reg-event-db :set-title
  (fn [db [_ title]]
    (assoc db :title title)))

(reg-event-db :update-input
  (fn [db [_ new-input]]
    (assoc db :input new-input)))

(defn- text->input-state [text]
  (let [lines (vec (or (seq (str/split (or text "") #"\n" -1)) [""]))
        crow  (dec (count lines))
        ccol  (count (nth lines crow))]
    {:lines lines :crow crow :ccol ccol}))

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
      ;; A new empty input has no placeholder tokens, so the paste
      ;; registry is dead state. Clearing it here keeps memory
      ;; bounded across long sessions — every send + every history
      ;; reset drops orphans.
      :pastes {}
      :paste-counter 0)))

(reg-event-db :add-paste
  ;; Stashes a clipboard payload in the registry, returns the new
  ;; id (Integer) via a side-channel atom that the screen loop reads
  ;; right after dispatch — see `:paste-counter` increment below.
  (fn [db [_ content]]
    (let [next-id (inc (or (:paste-counter db) 0))]
      (-> db
        (assoc :paste-counter next-id)
        (assoc-in [:pastes next-id] {:id next-id :content content})))))

(reg-event-db :remove-paste
  ;; Drop a single paste entry by id. Used when the user backspaces
  ;; over the closing `]` of a placeholder — the screen loop deletes
  ;; the token from the input buffer AND drops the matching content
  ;; here so memory tracks what the user can still see.
  (fn [db [_ id]]
    (update db :pastes dissoc id)))

(reg-event-db :set-scroll
  (fn [db [_ scroll]]
    (assoc db :messages-scroll scroll)))

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
      (let [thumb-h    (max 1 (int (* track-h (/ (double inner-h) total-h))))
            max-scroll (max 0 (- total-h inner-h))
            relative   (- mouse-y bar-top)
            denom      (max 1 (- track-h thumb-h))
            fraction   (max 0.0 (min 1.0 (double (/ relative denom))))
            new-scroll (long (Math/round (* fraction max-scroll)))]
        (assoc db :messages-scroll (max 0 (min max-scroll new-scroll)))))))

(reg-event-fx :send-message
  ;; `text` is the input-buffer string — it carries `[Pasted #N: …]`
  ;; placeholder tokens for any large clipboard payloads the user
  ;; pasted this turn. Substitute every token with its content from
  ;; the `:pastes` registry BEFORE the message reaches either the
  ;; chat history or the agent: the chat shows the full materialised
  ;; text (so re-reading the conversation later doesn't show a token
  ;; with no referent), and the agent sees what the user actually
  ;; meant to paste.
  (fn [db [_ text]]
    (let [expanded (input/expand-paste-placeholders text (:pastes db))
          token    (vis/cancellation-token)]
      {:db (-> db
             (update :messages conj (chat/user-message expanded))
             (update :messages conj (chat/assistant-message "Sending request to provider…"))
             (update :input-history (fn [xs]
                                      (let [xs (vec (or xs []))]
                                        (if (= expanded (last xs)) xs (conj xs expanded)))))
             (assoc :messages-scroll nil :loading? true
               :cancel-token token
               :cancelling? false
               :progress {:iterations []}
               :query-start-ms (System/currentTimeMillis)
               :input-history-index nil
               :input-history-draft nil))
       :fx [[:rlm-query (:conversation db) expanded token
             (get-in db [:settings :reasoning-level])]]})))

(reg-event-fx :cancel-query
  (fn [db _]
    (if-not (:loading? db)
      {:db db}
      (do
        ;; Both the cooperative flag and the hard interrupt are fired
        ;; through one channel-agnostic call. See
        ;; channels.cancellation/cancel! for the contract.
        (vis/cancel! (:cancel-token db))
        {:db (assoc db :cancelling? true)}))))

(reg-event-db :set-progress-iterations
  (fn [db [_ iterations]]
    (if-not (:loading? db)
      db
      (assoc-in db [:progress :iterations] (vec (or iterations []))))))

(reg-event-db :message-received
  (fn [db [_ answer & [{:keys [model iteration-count duration-ms tokens cost confidence conversation-turn-id status]}]]]
    (let [start    (:query-start-ms db)
          wall-ms  (when start (- (System/currentTimeMillis) start))
          trace    (get-in db [:progress :iterations])
          ;; Cancelled turns get a `:status :cancelled` flag on the
          ;; message so the bubble renderer dims the content (muted
          ;; italic fg) on bare terminal-bg. We KEEP the iteration
          ;; trace on cancelled messages so the user can see how far
          ;; the agent got before the abort — the renderer sews the
          ;; trace together with a plain status footer (\"Cancelled
          ;; by user.\") so partial work stays visible.
          response (-> (chat/assistant-message (or answer ""))
                     (cond-> conversation-turn-id                (assoc :conversation-turn-id conversation-turn-id)
                       (seq trace)
                       (assoc :trace trace :raw-answer (or answer ""))
                       (or duration-ms wall-ms) (assoc :duration-ms (or duration-ms wall-ms))
                       model      (assoc :model model)
                       iteration-count (assoc :iteration-count iteration-count)
                       tokens     (assoc :tokens tokens)
                       cost       (assoc :cost cost)
                       confidence (assoc :confidence confidence)
                       status     (assoc :status status)))]
      (-> db
        (update :messages pop)
        (update :messages conj response)
        (assoc :messages-scroll nil :loading? false :progress nil
          :cancel-token nil :cancelling? false)
        (dissoc :query-start-ms)))))

;;; ── Side effects ───────────────────────────────────────────────────────────

(reg-fx :rlm-query
  (fn [conversation text token reasoning-level]
    (let [fut (future
                (try
                  (let [{:keys [on-chunk]}
                        (vis/make-progress-tracker
                          {:on-update (fn [timeline _chunk]
                                        (try (dispatch [:set-progress-iterations timeline])
                                          (catch Throwable _ nil)))})
                        result (chat/query! conversation text
                                 {:on-chunk          on-chunk
                                  :cancel-atom       (vis/cancellation-atom token)
                                  :reasoning-default reasoning-level})]
                    (if (:error result)
                      (dispatch [:message-received (vis/format-error (:error result))])
                      (dispatch [:message-received (:answer result)
                                 (select-keys result
                                   [:model :iteration-count :duration-ms :tokens
                                    :cost :confidence :conversation-turn-id :status])])))
                  (catch Throwable t
                    ;; channels.cancellation/cancellation? folds in
                    ;; InterruptedException, CancellationException, and
                    ;; runtime wrappers around them — keep all the
                    ;; channel-shaped logic in one place. The bubble
                    ;; renderer dims the result based on `:status
                    ;; :cancelled`, so we attach it explicitly here.
                    (if (vis/cancellation? t)
                      (dispatch [:message-received "Cancelled by user."
                                 {:status :cancelled}])
                      (dispatch [:message-received (vis/format-error (or (ex-message t) (str t)))])))))]
      (vis/cancellation-set-future! token fut))))
