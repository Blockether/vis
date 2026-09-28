(ns com.blockether.vis.contract.content-test
  (:require [com.blockether.vis.contract.content :as content]
            [com.blockether.vis.contract.document :as document]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe
  schema-root-validates-messages
  (it "schema root validates messages"
      (let [message {"id" "m1"
                     "role" "assistant"
                     "status" "completed"
                     "created_at" 100
                     "completed_at" 110
                     "content" [{"id" "b1" "type" "prose" "markdown" "Done."}]}]
        (expect (document/valid? "content" message))
        (expect (content/message-valid? message))
        (expect (not (document/valid? "content" (assoc message "role" "unknown"))))
        (expect (not (content/message-valid? (assoc message "completed_at" 99))))
        (expect (not (document/valid? "content" {"roles" ["assistant"]})))
        (expect (not (document/valid? "content" (assoc message "content" [{"type" "unknown"}])))))))

(defdescribe
  payload-definitions-own-the-content-vocabulary
  (it "payload definitions own the content vocabulary"
      (expect (content/block-valid? {"id" "tool1" "type" "tool" "tool" "shell" "status" "running"}))
      (expect (not (content/block-valid?
                     {"id" "tool1" "type" "tool" "tool" "shell" "status" "unknown"})))
      (expect (content/block-valid? {"id" "r1" "type" "reasoning" "text" "Working."}))
      (expect (not (content/block-valid?
                     {"id" "r1" "type" "reasoning" "text" "Working." "visibility" "unknown"})))
      (doseq [field ["markdown" "text"]]
        (expect (content/event-valid? {"type" "content.block.delta"
                                       "turn_id" "t1"
                                       "block_id" "b1"
                                       "field" field
                                       "text" "More."})))
      (expect (not (content/event-valid? {"type" "content.block.delta"
                                          "turn_id" "t1"
                                          "block_id" "b1"
                                          "field" "unknown"
                                          "text" "More."})))))
