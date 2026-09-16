(ns com.blockether.vis.contract.content-test
  (:require [com.blockether.vis.contract.content :as content]
            [com.blockether.vis.contract.document :as document]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is]]))

(deftest schema-root-validates-messages
  (let [message {"id" "m1"
                 "role" "assistant"
                 "status" "completed"
                 "created_at" 100
                 "completed_at" 110
                 "content" [{"id" "b1" "type" "prose" "markdown" "Done."}]}]
    (is (document/valid? "content" message))
    (is (content/message-valid? message))
    (is (not (document/valid? "content" (assoc message "role" "unknown"))))
    (is (not (content/message-valid? (assoc message "completed_at" 99))))
    (is (not (document/valid? "content" {"roles" ["assistant"]})))
    (is (not (document/valid? "content" (assoc message "content" [{"type" "unknown"}]))))))

(deftest payload-definitions-own-the-content-vocabulary
  (is (content/block-valid? {"id" "tool1" "type" "tool" "tool" "shell" "status" "running"}))
  (is (not (content/block-valid? {"id" "tool1" "type" "tool" "tool" "shell" "status" "unknown"})))
  (is (content/block-valid? {"id" "r1" "type" "reasoning" "text" "Working."}))
  (is (not (content/block-valid?
             {"id" "r1" "type" "reasoning" "text" "Working." "visibility" "unknown"})))
  (doseq [field ["markdown" "text"]]
    (is (content/event-valid? {"type" "content.block.delta"
                               "turn_id" "t1"
                               "block_id" "b1"
                               "field" field
                               "text" "More."})))
  (is (not (content/event-valid? {"type" "content.block.delta"
                                  "turn_id" "t1"
                                  "block_id" "b1"
                                  "field" "unknown"
                                  "text" "More."}))))
