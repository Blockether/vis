(ns com.blockether.vis.internal.loop-test
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.loop :as loop]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe host-final-surface-test
  (it "recognizes only the new final-answer and title host forms"
    (let [answer-call? #'loop/answer-call-form?
          title-form?  #'loop/answer-compatible-meta-form?]
      (expect (true? (answer-call? '(turn-answer! [:ir [:p "Done"]]))))
      (expect (false? (answer-call? '(answer [:ir [:p "Done"]]))))
      (expect (true? (title-form? "(set-conversation-title \"Prompt cleanup\")")))
      (expect (false? (title-form? "(conversation-title \"Prompt cleanup\")")))))

  (it "preflight messages teach turn-answer! and not the retired answer form"
    (let [msg (loop/answer-position-error-message 0 2)]
      (expect (str/includes? msg "(turn-answer! ...)"))
      (expect (not (str/includes? msg "(answer ...)")))))

  (it "canonicalizes final answer IR and caps lazy children at the persistence boundary"
    (let [answer (loop/append-runtime-appendices
                   nil
                   [:ir [:ul (map (fn [n] [:li (str "item " n)]) (range))]]
                   nil)
          items (->> (nth answer 2) (drop 2) vec)
          rendered (loop/answer-str answer)]
      (expect (= :ir (first answer)))
      (expect (= 101 (count items)))
      (expect (str/includes? rendered "item 99"))
      (expect (not (str/includes? rendered "item 100")))
      (expect (str/includes? rendered "… many more"))
      (expect (not (str/includes? (pr-str answer) "LazySeq"))))))

(defdescribe preserved-thinking-replay-test
  (it "does not replay z.ai thinking into an Anthropic provider/model call"
    (let [append-replay #'loop/append-preserved-thinking-replay
          messages      [{:role "user" :content "continue"}]
          zai-message   {:role "assistant"
                         :content [{:type "thinking"
                                    :thinking "zai reasoning"
                                    :thinking-signature "zai reasoning"}
                                   {:type "text" :text "done"}]}
          anthropic-message {:role "assistant"
                             :content [{:type "thinking"
                                        :thinking "claude reasoning"
                                        :thinking-signature "anthropic-hmac"}
                                       {:type "text" :text "done"}]}
          journal      [[1 {:llm-provider :zai-coding
                            :llm-model "glm-5.1"
                            :assistant-message zai-message}]
                        [2 {:llm-provider :anthropic-coding-plan
                            :llm-model "claude-opus-4-7"
                            :assistant-message anthropic-message}]]]
      (expect (= [messages anthropic-message]
                [(subvec (vec (append-replay messages journal
                                {:provider :anthropic-coding-plan
                                 :model "claude-opus-4-7"})) 0 1)
                 (last (append-replay messages journal
                         {:provider :anthropic-coding-plan
                          :model "claude-opus-4-7"}))]))
      (expect (= messages
                (append-replay messages journal
                  {:provider :anthropic-coding-plan
                   :model "claude-sonnet-4-6"}))))))

(defdescribe provider-error-rendering-test
  (it "uses one stable provider error code and still includes provider body"
    (let [format-error #'loop/format-iteration-error
          rendered (format-error
                     {:message "Exceptional status code: 400"
                      :data {:status 400
                             :body "{\"error\":{\"message\":\"Invalid `signature` in `thinking` block\"}}"}})]
      (expect (str/includes? rendered "PROVIDER_ERROR"))
      (expect (str/includes? rendered "Invalid `signature` in `thinking` block"))))

  (it "builds canonical IR for user-visible provider errors"
    (let [provider-error-ir #'loop/provider-error-ir
          ir (provider-error-ir {:message "Exceptional status code: 400"
                                 :data {:status 400
                                        :body "{\"error\":{\"message\":\"Invalid `signature` in `thinking` block\"}}"}})]
      (expect (= :ir (first ir)))
      (expect (true? (get-in ir [1 :vis/provider-error])))
      (expect (str/includes? (loop/answer-str ir) "PROVIDER_ERROR")))))
