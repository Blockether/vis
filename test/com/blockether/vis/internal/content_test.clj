(ns com.blockether.vis.internal.content-test
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.content :as content]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe
  content-construction-test
  (it "constructs the canonical role-labelled message"
      (let [message (content/message {:id "turn_1"
                                      :role "assistant"
                                      :status "completed"
                                      :created-at 100
                                      :completed-at 110
                                      :content [(content/prose "b1" "Hello **world**")]})]
        (expect (= #{"id" "role" "status" "content" "created_at" "completed_at"}
                   (set (keys message))))
        (expect (= "prose" (get-in message ["content" 0 "type"])))
        (expect (= "Hello **world**" (get-in message ["content" 0 "markdown"])))))
  (it "normalizes final Markdown into exactly one prose block"
      (let [blocks (content/answer-content {:answer "done"})]
        (expect (= 1 (count blocks)))
        (expect (= "prose" (get-in blocks [0 "type"])))
        (expect (= "done" (get-in blocks [0 "markdown"])))
        (expect (= "done" (content/text-projection blocks)))))
  (it
    "extracts a speech projection from final Markdown"
    (let
      [blocks
       (content/answer-content
         "Full **technical** answer.

```vis-speech
The work is complete and the tests pass.
```")]
      (expect (= ["prose" "speech"] (mapv #(get % "type") blocks)))
      (expect (= "Full **technical** answer." (get-in blocks [0 "markdown"])))
      (expect (= "The work is complete and the tests pass." (get-in blocks [1 "text"])))
      (expect (not (str/includes? (get-in blocks [0 "markdown"]) "vis-speech")))))
  (it "includes speech blocks in plain-text projection"
      (let [block (content/speech "s1" "A concise spoken answer.")]
        (expect (= "A concise spoken answer." (content/text-projection [block])))))
  (it "preserves canonical error blocks in wrapped final answers"
      (let [error (content/error "e1" "provider_unavailable" "Try again later." true)]
        (expect (= [error] (content/answer-content {:answer [error]})))))
  (it "canonicalizes nested tool values to string keys and string enums"
      (let [block (content/tool {:tool "run_tests"
                                 :status :completed
                                 :output {:provider :openai-codex :actual {:model "gpt-5.6"}}})]
        (expect (= {"provider" "openai-codex" "actual" {"model" "gpt-5.6"}} (get block "output")))))
  (it "keeps typed errors as data"
      (let [block (content/error "e1" "provider_unavailable" "Try again later." true)]
        (expect (= true (get block "retryable"))))))
