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
