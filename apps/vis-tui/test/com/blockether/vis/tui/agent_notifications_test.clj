(ns com.blockether.vis.tui.agent-notifications-test
  (:require [com.blockether.vis.tui.state :as state]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is]]))

(deftest only-human-answers-mark-background-tabs-unread
  (doseq [[kind role expected?] [[:user "leader" true] [:council "leader" false]
                                 [:user "subagent" false]]]
    (let [handler (:fn (get @@#'state/event-registry :message-received))
          db {:active-tab-id :main
              :tabs [{:id :main} {:id :worker}]
              :tab-locals {:worker
                           {:session {:id "worker" :agent {:role role}}
                            :loading? true
                            :voice-conversation? true
                            :messages
                            [{:role :user :text "Task" :request-kind kind :client-turn-id "turn"}
                             {:role :assistant :pending? true :client-turn-id "turn"}]
                            :progress {:iterations []}
                            :pending-sends []}}}
          result (handler db
                          [:message-received :worker "Finished"
                           {:status :success :client-turn-id "turn"}])]

      (is (= expected? (boolean (get-in result [:db :tabs 1 :unread?]))))
      (is (= expected? (boolean (some #(= :speak-reply (first %)) (:fx result))))))))

(deftest focusing-a-tab-reports-the-read-to-the-gateway
  ;; The gateway owns the unread truth: clearing only the local dot would leave
  ;; the companion badge lit for a session this reader has just read.
  (let [handler
        (:fn (get @@#'state/event-registry :select-tab-index))

        db
        {:active-tab-id :main
         :tabs [{:id :main :active? true} {:id :worker :unread? true}]
         :tab-locals {:worker {:session {:id "worker-session"}}}}

        result
        (handler db [:select-tab-index 1])]

    (is (= :worker (:active-tab-id (:db result))))
    (is (nil? (get-in result [:db :tabs 1 :unread?])))
    (is (= [[:mark-session-read "worker-session"]] (:fx result)))))

(deftest the-focused-tab-reports-its-own-answer-as-read
  ;; A tab the reader is WATCHING never lights a dot, so nothing else would ever
  ;; report the answer as read and every other surface would keep calling it new.
  (let [handler
        (:fn (get @@#'state/event-registry :message-received))

        db
        {:active-tab-id :main
         :tabs [{:id :main :active? true}]
         :session {:id "main-session" :agent {:role "leader"}}
         :loading? true
         :messages [{:role :user :text "Task" :request-kind :user :client-turn-id "turn"}
                    {:role :assistant :pending? true :client-turn-id "turn"}]
         :progress {:iterations []}
         :pending-sends []}

        result
        (handler db [:message-received :main "Finished" {:status :success :client-turn-id "turn"}])]

    (is (some #(= [:mark-session-read "main-session"] %) (:fx result)))
    (is (nil? (get-in result [:db :tabs 0 :unread?])))))
