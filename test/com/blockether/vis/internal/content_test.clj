(ns com.blockether.vis.internal.content-test
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.content :as content]
            [com.blockether.vis.contract.voice :as voice]
            [com.blockether.vis.contract.wire :as wire]
            [lazytest.core :refer [defdescribe expect it throws?]]))

(defdescribe
  handsfree-presentation-test
  ;; Regression: session a853a9c9-6296-42f9-ab32-67ee83447023 had prose but no spoken outcome.
  (it "uses the same conversation text for display and speech"
      (let [conversation
            {"type" "conversation" "phase" "answer" "text" "The diagram is ready."}

            visual
            {"type" "visual" "markdown" "```mermaid\ngraph LR; A-->B\n```"}

            raw
            (str (wire/json-str conversation) "\n" (wire/json-str visual))

            blocks
            (content/presentation-content raw)]

        (expect (= ["conversation" "visual"] (mapv #(get % "type") blocks)))
        (expect (= "The diagram is ready." (get-in blocks [0 "text"])))
        (expect (= (get visual "markdown") (get-in blocks [1 "markdown"])))
        (expect (= blocks (content/answer-content {:answer blocks})))
        (expect (= "The diagram is ready.\n\n```mermaid\ngraph LR; A-->B\n```"
                   (content/text-projection blocks)))))
  (it "commits only complete lines and accepts the last line at completion"
      (let [raw
            (wire/json-str {"type" "conversation" "phase" "answer" "text" "Ready."})

            complete
            (content/presentation-records (str raw "\n") false)]

        (expect (empty? (content/presentation-records raw false)))
        (expect (= 1 (count complete)))
        (expect (= (mapv :block complete) (content/presentation-content raw)))
        (expect (= 0 (:start (first complete))))
        (expect (= (inc (count raw)) (:end (first complete))))
        (expect (= complete (content/presentation-records (str raw "\n{\"type\":") false)))))
  (it "keeps stable identities across equivalent serialization"
      (let [a
            "{\"type\":\"conversation\",\"phase\":\"answer\",\"text\":\"Ready.\"}"

            b
            "{\"text\": \"Ready.\", \"phase\": \"answer\", \"type\": \"conversation\"}"]

        (expect (= (content/presentation-content a) (content/presentation-content b)))))
  (it "allows progress before an answer but never as the final outcome"
      (let [progress
            (wire/json-str {"type" "conversation" "phase" "progress" "text" "I will check."})

            answer
            (wire/json-str {"type" "conversation" "phase" "answer" "text" "The checks pass."})]

        (expect (= 1 (count (content/presentation-records progress true))))
        (expect (throws? clojure.lang.ExceptionInfo #(content/presentation-content progress)))
        (expect (= ["progress" "answer"]
                   (mapv #(get % "phase")
                         (content/presentation-content (str progress "\n" answer)))))))
  (it "requires explicit user-requested silence and forbids contradictory conversation"
      (let [silent
            (wire/json-str {"type" "silence" "reason" "user_requested"})

            answer
            (wire/json-str {"type" "conversation" "phase" "answer" "text" "Ready."})]

        (expect (= ["silence"] (mapv #(get % "type") (content/presentation-content silent))))
        (expect (= "" (content/text-projection (content/presentation-content silent))))
        (expect (throws? clojure.lang.ExceptionInfo
                         #(content/presentation-content (str silent "\n" answer))))))
  (it "rejects ordinary prose, markup, missing outcomes and extra record values"
      (doseq
        [raw
         ["" "Ready." "```vis-speech\nReady.\n```" "{\"type\":\"visual\",\"markdown\":\"Ready.\"}"
          "{\"type\":\"conversation\",\"phase\":\"answer\",\"text\":\"**Ready.**\"}"
          "{\"type\":\"conversation\",\"phase\":\"answer\",\"text\":\"Ready.\",\"id\":\"model_id\"}"
          "{\"type\":\"silence\",\"reason\":\"user_requested\"} {}"
          "{\"type\":\"silence\",\"reason\":\"user_requested\"} trailing"]]
        (expect (throws? clojure.lang.ExceptionInfo #(content/presentation-content raw)))))
  (it "reports sanitized validation errors without retaining provider text"
      (let [raw
            "private malformed output"

            data
            (try (content/presentation-content raw)
                 (catch clojure.lang.ExceptionInfo e (ex-data e)))]

        (expect (= {:type :content/invalid-presentation :reason :invalid-record :index 0} data))
        (expect (not (str/includes? (pr-str data) raw)))))
  (it "bounds total input and the number of records"
      (let [silent (wire/json-str {"type" "silence" "reason" "user_requested"})]
        (expect (throws? clojure.lang.ExceptionInfo
                         #(content/presentation-records
                            (apply str (repeat (inc (:max-projection-chars voice/limits)) " "))
                            false)))
        (expect (throws?
                  clojure.lang.ExceptionInfo
                  #(content/presentation-content
                     (str/join "\n" (repeat (inc (:max-speech-segments voice/limits)) silent))))))))

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
  (it
    "extracts several committed projections without leaving fences in the full answer"
    (let
      [blocks
       (content/answer-content
         "First fact.\n```vis-speech\nFirst fact.\n```\nSecond fact.\n```vis-speech\nSecond fact.\n```")]
      (expect (= ["prose" "speech" "speech"] (mapv #(get % "type") blocks)))
      (expect (= ["First fact." "Second fact."] (mapv #(get % "text") (rest blocks))))
      (expect (not (str/includes? (get-in blocks [0 "markdown"]) "vis-speech")))))
  (it "requires a complete closing line before a live speech commit"
      (let [text "```vis-speech\nStable words.\n```"]
        (expect (empty? (content/closed-speech-projections text false)))
        (expect (= ["Stable words."]
                   (mapv :text (content/closed-speech-projections (str text "\n") false))))
        (expect (= ["Stable words."] (mapv :text (content/closed-speech-projections text true))))))
  (it "ignores speech markers inside code and quoted examples"
      (doseq [text ["````markdown\n```vis-speech\nDo not speak this.\n```\n````\n"
                    "~~~markdown\n```vis-speech\nDo not speak this.\n```\n~~~\n"
                    "> ```vis-speech\n> Do not speak this.\n> ```\n"]]
        (expect (empty? (content/closed-speech-projections text false)))))
  (it "includes speech blocks in plain-text projection"
      (let [block (content/speech "s1" "A concise spoken answer.")]
        (expect (= "A concise spoken answer." (content/text-projection [block])))))
  (it "preserves canonical error blocks in wrapped final answers"
      (let [error (content/error "e1" "provider_unavailable" "Try again later." true)]
        (expect (= [error] (content/answer-content {:answer [error]})))))
  (it "canonicalizes nested tool values to string keys and string enums"
      (let [block (content/tool {:tool "shell"
                                 :status :completed
                                 :output {:provider :openai-codex :actual {:model "gpt-5.6"}}})]
        (expect (= {"provider" "openai-codex" "actual" {"model" "gpt-5.6"}} (get block "output")))))
  ;; Regression, reported session 10bb33ec-42b3-42a2-9c29-42956810aae2:
  ;; canonical error and notice codes accepted kebab-case and mixed-case identifiers.
  (it "requires every diagnostic code to be lowercase snake_case"
      (expect (throws? clojure.lang.ExceptionInfo
                       #(content/error "e1" "provider_invalid-request" "No." false)))
      (expect (throws? clojure.lang.ExceptionInfo
                       #(content/error "e1" "Provider_Invalid_Request" "No." false)))
      (expect (throws? clojure.lang.ExceptionInfo #(content/notice "n1" "rate-limit" "Wait.")))
      (expect (= "provider_invalid_request"
                 (get (content/error "e1" "provider_invalid_request" "Yes." false) "code")))
      (expect (= "rate_limit" (get (content/notice "n1" "rate_limit" "Wait.") "code")))))
