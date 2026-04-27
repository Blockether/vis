(ns com.blockether.vis.channels.tui.state
  "Re-frame-like state management for the TUI.
   Single app-db atom, pure event handlers, side effects via reg-fx."
  (:require [clojure.string :as str]
            [com.blockether.vis.channels.cancellation :as cancellation]
            [com.blockether.vis.channels.core :as channels]
            [com.blockether.vis.channels.tui.chat :as chat]
            [com.blockether.vis.channels.tui.input :as input]
            [com.blockether.vis.channels.tui.render :as render]
            [com.blockether.vis.loop.runtime.conversation.core :as conversations]
            [com.blockether.vis.loop.runtime.conversation.environment.query.core :as query-core]))

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

(defonce render-monitor
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
;;  :conv       nil              ;; {:id conv-id} or nil — handle to the shared conversations cache
;;  :messages   []               ;; [{:role :user|:assistant :text str :timestamp #inst}]
;;  :msg-scroll nil              ;; row offset into bubbles, nil = auto-bottom
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

(defn init!
  "Initialize app-db with default state."
  []
  (reset! app-db {:config     nil
                  :conv       nil
                  :title      nil
                  :messages   []
                  :msg-scroll nil
                  :input      (input/empty-input)
                  :input-history []
                  :input-history-index nil
                  :input-history-draft nil
                  :loading?   false
                  :cancel-token nil
                  :cancelling? false
                  :progress   nil
                  :settings   {:show-thinking true :show-iterations true}
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
    (channels/reload-config!)
    (when (seq (:providers config))
      ;; rebuild-router! only swaps the global singleton; cached envs
      ;; keep the snapshot they were created with. Reseat them too,
      ;; otherwise the next query runs against the previous model
      ;; even though the status bar already shows the new one.
      (let [r (query-core/rebuild-router! config)]
        (conversations/refresh-cached-routers! r)))
    (assoc db :config config)))

(reg-event-db :set-dialog-open
  (fn [db [_ open?]]
    (assoc db :dialog-open? (boolean open?))))

(reg-event-db :update-settings
  (fn [db [_ new-settings]]
    ;; Settings (show-thinking?, show-iterations?) are part of every
    ;; format-answer cache key, so toggling them already invalidates
    ;; cache entries naturally. But the OLD entries linger until the
    ;; cache fills — which on a quiet conversation never happens. Drop
    ;; them so we don't leak memory across many toggles.
    (render/invalidate-cache!)
    (assoc db :settings (merge (:settings db) new-settings))))

(reg-event-db :set-layout
  (fn [db [_ layout]]
    ;; Pushed in by the render thread; intentionally does NOT bump
    ;; render-version (see no-render-bump-events).
    (assoc db :layout layout)))

(reg-event-db :shutdown
  (fn [db _]
    (assoc db :shutdown? true)))

(reg-event-db :init-conversation
  (fn [db [_ conv history]]
    (let [user-history (->> (or history [])
                         (filter #(= :user (:role %)))
                         (mapv :text))]
      (assoc db
        :conv conv
        :messages (or history [])
        :input-history user-history
        :input-history-index nil
        :input-history-draft nil))))

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
      :input-history-draft nil)))

(reg-event-db :set-scroll
  (fn [db [_ scroll]]
    (assoc db :msg-scroll scroll)))

(reg-event-db :scroll-up
  (fn [db [_ amount total-h inner-h]]
    (let [max-s (max 0 (- total-h inner-h))
          cur   (or (:msg-scroll db) max-s)]
      (assoc db :msg-scroll (max 0 (- cur amount))))))

(reg-event-db :scroll-down
  (fn [db [_ amount total-h inner-h]]
    (let [max-s (max 0 (- total-h inner-h))
          cur   (or (:msg-scroll db) max-s)]
      (if (>= (+ cur amount) max-s)
        (assoc db :msg-scroll nil)
        (assoc db :msg-scroll (min max-s (+ cur amount)))))))

(reg-event-fx :send-message
  (fn [db [_ text]]
    (let [token (cancellation/make)]
      {:db (-> db
             (update :messages conj (chat/user-msg text))
             (update :messages conj (chat/assistant-msg "Sending request to provider…"))
             (update :input-history (fn [xs]
                                      (let [xs (vec (or xs []))]
                                        (if (= text (last xs)) xs (conj xs text)))))
             (assoc :msg-scroll nil :loading? true
               :cancel-token token
               :cancelling? false
               :progress {:iterations []}
               :query-start-ms (System/currentTimeMillis)
               :input-history-index nil
               :input-history-draft nil))
       :fx [[:rlm-query (:conv db) text token]]})))

(reg-event-fx :cancel-query
  (fn [db _]
    (if-not (:loading? db)
      {:db db}
      (do
        ;; Both the cooperative flag and the hard interrupt are fired
        ;; through one channel-agnostic call. See
        ;; channels.cancellation/cancel! for the contract.
        (cancellation/cancel! (:cancel-token db))
        {:db (assoc db :cancelling? true)}))))

(reg-event-db :set-progress-iterations
  (fn [db [_ iterations]]
    (if-not (:loading? db)
      db
      (assoc-in db [:progress :iterations] (vec (or iterations []))))))

(reg-event-db :message-received
  (fn [db [_ answer & [{:keys [model iterations duration-ms tokens cost confidence query-id status]}]]]
    (let [start    (:query-start-ms db)
          wall-ms  (when start (- (System/currentTimeMillis) start))
          trace    (get-in db [:progress :iterations])
          ;; Cancelled turns get a `:status :cancelled` flag on the
          ;; message so the bubble renderer can dim the content. We
          ;; also keep the trace off cancelled messages — a partial
          ;; iteration trace next to a \"Cancelled by user.\"
          ;; placeholder is more confusing than informative.
          cancelled? (= :cancelled status)
          response (-> (chat/assistant-msg (or answer ""))
                     (cond-> query-id                (assoc :query-id query-id)
                       (and (not cancelled?) (seq trace))
                       (assoc :trace trace :raw-answer (or answer ""))
                       (or duration-ms wall-ms) (assoc :duration-ms (or duration-ms wall-ms))
                       model      (assoc :model model)
                       iterations (assoc :iterations iterations)
                       tokens     (assoc :tokens tokens)
                       cost       (assoc :cost cost)
                       confidence (assoc :confidence confidence)
                       status     (assoc :status status)))]
      (-> db
        (update :messages pop)
        (update :messages conj response)
        (assoc :msg-scroll nil :loading? false :progress nil
          :cancel-token nil :cancelling? false)
        (dissoc :query-start-ms)))))

;;; ── Side effects ───────────────────────────────────────────────────────────

(reg-fx :rlm-query
  (fn [conv text token]
    (let [fut (future
                (try
                  (let [{:keys [on-chunk]}
                        (channels/make-progress-tracker
                          {:on-update (fn [timeline _chunk]
                                        (try (dispatch [:set-progress-iterations timeline])
                                          (catch Throwable _ nil)))})
                        result (chat/query! conv text
                                 {:on-chunk    on-chunk
                                  :cancel-atom (cancellation/cancel-atom token)})]
                    (if (:error result)
                      (dispatch [:message-received (str "Error: " (:error result))])
                      (dispatch [:message-received (:answer result)
                                 (select-keys result
                                   [:model :iterations :duration-ms :tokens
                                    :cost :confidence :query-id :status])])))
                  (catch Throwable t
                    ;; channels.cancellation/cancellation? folds in
                    ;; InterruptedException, CancellationException, and
                    ;; runtime wrappers around them — keep all the
                    ;; channel-shaped logic in one place. The bubble
                    ;; renderer dims the result based on `:status
                    ;; :cancelled`, so we attach it explicitly here.
                    (if (cancellation/cancellation? t)
                      (dispatch [:message-received "Cancelled by user."
                                 {:status :cancelled}])
                      (dispatch [:message-received (str "Error: " (or (ex-message t) (str t)))])))))]
      (cancellation/set-future! token fut))))
