(ns com.blockether.vis.ext.channel-tui.chat-test
  (:require [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.channel-tui.chat :as chat]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe rebuild-history-test
  (it "elides persisted :vis/silent blocks when resuming a conversation"
    (with-redefs [vis/db-info (fn [] :db)
                  vis/db-list-conversation-turns
                  (fn [db conversation-id]
                    (expect (= :db db))
                    (expect (= "c1" conversation-id))
                    [{:id :turn-1
                      :user-request "set title, then inspect"
                      :answer "done"}])
                  vis/db-list-conversation-turn-iterations
                  (fn [db turn-id]
                    (expect (= :db db))
                    (expect (= :turn-1 turn-id))
                    [{:id :iter-1 :thinking "thinking"}])
                  vis/db-list-iteration-blocks
                  (fn [db iter-id]
                    (expect (= :db db))
                    (expect (= :iter-1 iter-id))
                    [{:code "(conversation-title \"Demo\")"
                      :result :vis/silent
                      :stdout ""
                      :duration-ms 1}
                     {:code "(+ 1 2)"
                      :result 3
                      :stdout ""
                      :duration-ms 2}
                     {:code "(answer \"done\")"
                      :result :vis/answer
                      :stdout ""
                      :duration-ms 3}])]
      (let [history ((var-get (resolve 'com.blockether.vis.ext.channel-tui.chat/rebuild-history)) "c1")
            assistant (second history)
            trace-entry (first (:trace assistant))]
        (expect (= ["(+ 1 2)"] (:code trace-entry)))
        (expect (= ["3"] (:results trace-entry)))
        (expect (= [true] (:successes trace-entry)))))))

(defdescribe turn-options-test
  (it "forwards reasoning-default and extra-body to vis/send!"
    (let [seen (atom nil)]
      (with-redefs [vis/send! (fn [_id _text opts]
                                (reset! seen opts)
                                {:answer "ok"})]
        (let [result (chat/turn! {:id "c1"} "hello"
                       {:reasoning-default :deep
                        :extra-body {:text {:verbosity "high"}}})]
          (expect (= "ok" (:answer result)))
          (expect (= 1 (:iteration-count result)))
          (expect (= :deep (:reasoning-default @seen)))
          (expect (= {:text {:verbosity "high"}} (:extra-body @seen))))))))
