(ns com.blockether.vis.internal.content-test
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [com.blockether.vis.internal.content :as content]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe
  content-contract-test
  (it "accepts the canonical role-labelled message"
      (let [message (content/message {:id "turn_1"
                                      :role "assistant"
                                      :status "completed"
                                      :created-at 100
                                      :completed-at 110
                                      :content [(content/prose "b1" "Hello **world**")]})]
        (expect (s/valid? ::content/message message))
        (expect (= #{"id" "role" "status" "content" "created_at" "completed_at"}
                   (set (keys message))))
        (expect (= "prose" (get-in message ["content" 0 "type"])))
        (expect (= "Hello **world**" (get-in message ["content" 0 "markdown"])))))
  (it "rejects keyword-keyed and unknown blocks"
      (expect (not (s/valid? ::content/block {:id "b1" :type "prose" :markdown "no"})))
      (expect (not (s/valid? ::content/block {"id" "b1" "type" "html" "html" "<b>no</b>"}))))
  (it "requires ordered timestamps"
      (expect (not (s/valid? ::content/message
                             {"id" "t1"
                              "role" "assistant"
                              "status" "completed"
                              "content" []
                              "created_at" 20
                              "completed_at" 10}))))
  (it "normalizes final Markdown into exactly one prose block"
      (let [blocks (content/answer-content {:answer "done"})]
        (expect (= 1 (count blocks)))
        (expect (= "prose" (get-in blocks [0 "type"])))
        (expect (= "done" (get-in blocks [0 "markdown"])))
        (expect (= "done" (content/text-projection blocks)))))
  (it
    "extracts a voice projection from final Markdown into a speech block"
    (let
      [blocks
       (content/answer-content
         "Full **technical** answer.\n\n```vis-speech\nThe work is complete and the tests pass.\n```")]
      (expect (= ["prose" "speech"] (mapv #(get % "type") blocks)))
      (expect (= "Full **technical** answer." (get-in blocks [0 "markdown"])))
      (expect (= "The work is complete and the tests pass." (get-in blocks [1 "text"])))
      (expect (not (str/includes? (get-in blocks [0 "markdown"]) "vis-speech")))))
  (it "accepts speech blocks and includes them in plain-text projection"
      (let [block (content/speech "s1" "A concise spoken answer.")]
        (expect (s/valid? ::content/block block))
        (expect (= "A concise spoken answer." (content/text-projection [block])))))
  (it "preserves canonical error blocks in wrapped final answers"
      (let [error
            (content/error "e1" "provider_unavailable" "Try again later." true)

            blocks
            (content/answer-content {:answer [error]})]

        (expect (= [error] blocks))
        (expect (s/valid? ::content/block (first blocks)))))
  (it "validates append-only streaming events"
      (expect (s/valid?
                ::content/event
                {"type" "content.block.started" "turn_id" "t1" "block" (content/prose "b1" "")}))
      (expect (s/valid? ::content/event
                        {"type" "content.block.delta"
                         "turn_id" "t1"
                         "block_id" "b1"
                         "field" "markdown"
                         "text" "partial"}))
      (expect (not (s/valid? ::content/event
                             {"type" "content.block.delta"
                              "turn_id" "t1"
                              "block_id" "b1"
                              "field" "html"
                              "text" "<b>bad</b>"}))))
  (it "rejects keyword keys and non-JSON values at every nesting depth"
      (expect (not (s/valid? ::content/block
                             {"id" "b1"
                              "type" "tool"
                              "tool" "run_tests"
                              "status" "completed"
                              "output" {:provider :openai-codex}}))))
  (it "canonicalizes nested tool values to string keys and string enums"
      (let [block (content/tool {:tool "run_tests"
                                 :status :completed
                                 :output {:provider :openai-codex :actual {:model "gpt-5.6"}}})]
        (expect (= {"provider" "openai-codex" "actual" {"model" "gpt-5.6"}} (get block "output")))
        (expect (s/valid? ::content/block block))))
  (it "keeps typed errors as data"
      (let [block (content/error "e1" "provider_unavailable" "Try again later." true)]
        (expect (s/valid? ::content/block block))
        (expect (= true (get block "retryable"))))))
