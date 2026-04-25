(ns com.blockether.vis.channels.tui.state
  "Re-frame-like state management for the TUI.
   Single app-db atom, pure event handlers, side effects via reg-fx."
  (:require [com.blockether.vis.channels.core :as channels]
            [com.blockether.vis.channels.tui.chat :as chat]
            [com.blockether.vis.channels.tui.input :as input]
            [com.blockether.vis.channels.tui.render :as render]
            [com.blockether.vis.loop.runtime.conversation.core :as conversations]
            [com.blockether.vis.loop.runtime.conversation.environment.query.core :as query-core]))

;;; ── Framework ──────────────────────────────────────────────────────────────

(defonce app-db (atom nil))
(defonce ^:private event-registry (atom {}))
(defonce ^:private fx-registry (atom {}))

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

(defn dispatch
  "Dispatch an event vector, e.g. (dispatch [:send-message \"hello\"])."
  [[id :as event-vec]]
  (if-let [{:keys [type] :as handler} (get @event-registry id)]
    (case type
      :db (swap! app-db (:fn handler) event-vec)
      :fx (let [{:keys [db fx]} ((:fn handler) @app-db event-vec)]
            (when db (reset! app-db db))
            (doseq [[fx-id & args] fx]
              (when-let [fx-fn (get @fx-registry fx-id)]
                (apply fx-fn args)))))
    (throw (ex-info (str "No handler registered for event: " id) {:event event-vec}))))

;;; ── State shape ────────────────────────────────────────────────────────────
;;
;; {:config     nil              ;; provider config map or nil
;;  :conv       nil              ;; {:id conv-id} or nil — handle to the shared conversations cache
;;  :messages   []               ;; [{:role :user|:assistant :text str :timestamp #inst}]
;;  :msg-scroll nil              ;; row offset into bubbles, nil = auto-bottom
;;  :input      {:lines [""] :crow 0 :ccol 0}
;;  :loading?   false            ;; true while RLM is working
;;  :progress   nil              ;; live per-iteration timeline while loading:
;;                               ;;   {:iterations [{:iteration int
;;                               ;;                  :thinking  str-or-nil
;;                               ;;                  :code      [str]       ;; latest streamed forms
;;                               ;;                  :final?    bool}]}
;;                               ;; Cleared on :message-received.
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
                  :loading?   false
                  :progress   nil
                  :dialog-open? false}))

;;; ── Pure event handlers ────────────────────────────────────────────────────

(reg-event-db :set-config
  (fn [db [_ config]]
    (channels/reload-config!)
    (when (seq (:providers config))
      (query-core/rebuild-router! config))
    (assoc db :config config)))

(reg-event-db :set-dialog-open
  (fn [db [_ open?]]
    (assoc db :dialog-open? (boolean open?))))

(reg-event-db :init-conversation
  (fn [db [_ conv history]]
    (assoc db :conv conv :messages (or history []))))

(reg-event-db :set-title
  (fn [db [_ title]]
    (assoc db :title title)))

(reg-event-db :update-input
  (fn [db [_ new-input]]
    (assoc db :input new-input)))

(reg-event-db :reset-input
  (fn [db _]
    (assoc db :input (input/empty-input))))

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
    {:db (-> db
           (update :messages conj (chat/user-msg text))
           (update :messages conj (chat/assistant-msg "Sending request..."))
           (assoc :msg-scroll nil :loading? true
             :progress {:iterations []}
             :query-start-ms (System/currentTimeMillis)))
     :fx [[:rlm-query (:conv db) text]]}))

(reg-event-db :set-progress-iterations
  (fn [db [_ iterations]]
    (if-not (:loading? db)
      db
      (assoc-in db [:progress :iterations] (vec (or iterations []))))))

(reg-event-db :message-received
  (fn [db [_ answer & [{:keys [model iterations duration-ms tokens cost]}]]]
    (let [start    (:query-start-ms db)
          wall-ms  (when start (- (System/currentTimeMillis) start))
          trace    (get-in db [:progress :iterations])
          bubble-w (max 60 (- (or (:last-cols db) 120) 4))
          full-text (render/format-answer-with-thinking answer trace bubble-w)
          response (-> (chat/assistant-msg full-text)
                     (cond-> (or duration-ms wall-ms) (assoc :duration-ms (or duration-ms wall-ms))
                             model      (assoc :model model)
                             iterations (assoc :iterations iterations)
                             tokens     (assoc :tokens tokens)
                             cost       (assoc :cost cost)))
          ;; Auto-generate title from first user message if not yet set
          conv-id  (get-in db [:conv :id])
          first-turn? (= 2 (count (:messages db)))  ;; user msg + placeholder
          first-user-text (when first-turn?
                            (:text (first (:messages db))))]
      (when (and first-turn? first-user-text conv-id)
        (future
          (try
            (let [title (subs first-user-text 0 (min (count first-user-text) 80))]
              (conversations/set-title! conv-id title))
            (catch Throwable _ nil))))
      (-> db
        (update :messages pop)
        (update :messages conj response)
        (assoc :msg-scroll nil :loading? false :progress nil)
        (cond-> (and first-turn? first-user-text)
          (assoc :title (subs first-user-text 0 (min (count first-user-text) 60))))
        (dissoc :query-start-ms)))))

;;; ── Side effects ───────────────────────────────────────────────────────────

(reg-fx :rlm-query
  (fn [conv text]
    (future
      (let [{:keys [on-chunk get-timeline]}
            (channels/make-progress-tracker
              {:on-update (fn [timeline _chunk]
                            (try (dispatch [:set-progress-iterations timeline])
                              (catch Throwable _ nil)))})
            result (chat/query! conv text {:on-chunk on-chunk})]
        (if (:error result)
          (dispatch [:message-received (str "Error: " (:error result))])
          (dispatch [:message-received (:answer result)
                      (select-keys result [:model :iterations :duration-ms :tokens :cost])]))))))
