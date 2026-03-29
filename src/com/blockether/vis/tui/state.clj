(ns com.blockether.vis.tui.state
  "Re-frame-like state management for the TUI.
   Single app-db atom, pure event handlers, side effects via reg-fx."
  (:require [com.blockether.vis.tui.chat :as chat]
            [com.blockether.vis.tui.input :as input]))

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
;;  :conv       nil              ;; {:env ...} or nil (RLM environment)
;;  :messages   []               ;; [{:role :user|:assistant :text str :timestamp #inst}]
;;  :msg-scroll nil              ;; row offset into bubbles, nil = auto-bottom
;;  :input      {:lines [""] :crow 0 :ccol 0}
;;  :loading?   false            ;; true while RLM is working
;;  :dialog-open? false}         ;; dialog singleton guard
;;

(defn init!
  "Initialize app-db with default state."
  []
  (reset! app-db {:config     nil
                  :conv       nil
                  :messages   []
                  :msg-scroll nil
                  :input      (input/empty-input)
                  :loading?   false
                  :dialog-open? false}))

;;; ── Pure event handlers ────────────────────────────────────────────────────

(reg-event-db :set-config
              (fn [db [_ config]]
                (assoc db :config config)))

(reg-event-db :set-dialog-open
              (fn [db [_ open?]]
                (assoc db :dialog-open? (boolean open?))))

(reg-event-db :init-conversation
              (fn [db [_ conv history]]
                (assoc db :conv conv :messages (or history []))))

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
                         (update :messages conj (chat/assistant-msg "thinking..."))
                         (assoc :msg-scroll nil :loading? true
                                :query-start-ms (System/currentTimeMillis)))
                 :fx [[:rlm-query (:conv db) text]]}))

(reg-event-db :message-received
              (fn [db [_ answer]]
                (let [start    (:query-start-ms db)
                      dur-ms   (when start (- (System/currentTimeMillis) start))
                      response (-> (chat/assistant-msg answer)
                                   (cond-> dur-ms (assoc :duration-ms dur-ms)))]
                  (-> db
                      (update :messages pop)
                      (update :messages conj response)
                      (assoc :msg-scroll nil :loading? false)
                      (dissoc :query-start-ms)))))

;;; ── Side effects ───────────────────────────────────────────────────────────

(reg-fx :rlm-query
        (fn [conv text]
          (future
            (let [result (chat/query! conv text)]
              (if (:error result)
                (dispatch [:message-received (:error result)])
                (dispatch [:message-received (:answer result)]))))))
