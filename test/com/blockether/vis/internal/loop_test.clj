(ns com.blockether.vis.internal.loop-test
  (:require [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.lang-clojure.core :as clj-ext]
            [com.blockether.vis.internal.loop :as loop]
            [lazytest.core :refer [defdescribe expect it]]
            [sci.core :as sci]))

(defdescribe host-final-surface-test
  (it "recognizes only the new final-answer and title host forms"
    (let [answer-call? #'loop/answer-call-form?
          title-form?  #'loop/answer-compatible-meta-form?]
      (expect (true? (answer-call? '(turn-answer! [:ir [:p "Done"]]))))
      (expect (false? (answer-call? '(answer [:ir [:p "Done"]]))))
      (expect (true? (title-form? "(set-conversation-title! \"Prompt cleanup\")")))
      (expect (false? (title-form? "(conversation-title \"Prompt cleanup\")")))))

  (it "preflight messages teach turn-answer! and not the retired answer form"
    (let [msg (loop/answer-position-error-message 0 2)]
      (expect (str/includes? msg "(turn-answer! ...)"))
      (expect (not (str/includes? msg "(answer ...)")))))

  (it "collapses answer-alone preflight to one synthetic guard block"
    (let [preflight #'loop/code-entries-preflight
          entries (:code-entries
                   (preflight 1
                     (str "(def a 1)\n\n"
                       "(set-conversation-title! \"Def a 1\")\n\n"
                       "(turn-answer! [:ir [:p \"Done\"]])")))
          error (:vis/preflight-error (first entries))]
      (expect (= 1 (count entries)))
      (expect (= "(vis/preflight-error :answer-alone)" (:expr (first entries))))
      (expect (str/includes? error "Answer-alone preflight"))
      (expect (not (str/includes? (:expr (first entries)) "(def a 1)")))))

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
      (expect (not (str/includes? (pr-str answer) "LazySeq")))))

  (it "rescues terminal \\e notation in final answer source before IR parsing"
    (let [[entries parse-error] (#'loop/split-top-level-forms
                                 (str "(set-conversation-title! \"Paste markers\")\n\n"
                                   "(turn-answer! [:ir [:c \"\\e[200~\"] [:c \"\\e[201~\"]])"))]
      (expect (nil? parse-error))
      (expect (= 2 (count entries)))
      (expect (every? :repaired? entries))
      (expect (str/includes? (:expr (second entries)) "\"\\\\e[200~\""))
      (expect (str/includes? (:expr (second entries)) "\"\\\\e[201~\""))))

  (it "composes answer escape rescue with the next parser repair"
    (let [[entries parse-error] (#'loop/split-top-level-forms
                                 "(turn-answer! [:ir [:c \"\\e[200~\"]]")]
      (expect (nil? parse-error))
      (expect (= 1 (count entries)))
      (expect (:repaired? (first entries)))
      (expect (str/includes? (:expr (first entries)) "\"\\\\e[200~\"")))))

(defdescribe routed-provider-metadata-test
  (it "uses svar routed provider/model over pre-call routing guess"
    (let [provider #'loop/actual-llm-provider
          model    #'loop/actual-llm-model
          guessed  {:provider :anthropic-coding-plan
                    :name "claude-opus-4-7"}
          result   #:routed{:provider-id :zai-coding-plan
                            :model "glm-5.1"}]
      (expect (= :zai-coding-plan (provider guessed result)))
      (expect (= "glm-5.1" (model guessed result)))
      (expect (= :anthropic-coding-plan (provider guessed {})))
      (expect (= "claude-opus-4-7" (model guessed {})))))

  (it "builds selected/actual/fallback metadata for persistence and UI"
    (let [metadata #'loop/llm-routing-metadata
          attach   #'loop/attach-llm-routing-summary
          selected {:provider :anthropic-coding-plan
                    :name "claude-opus-4-7"}
          result   {:llm-provider :zai-coding-plan
                    :llm-model "glm-5.1"
                    :llm-fallback-trace [{:provider-id :anthropic-coding-plan
                                          :model "claude-opus-4-7"
                                          :reason :transient-error
                                          :error "HTTP 529 overloaded"}]
                    :cost {:total-cost 0.01}}]
      (expect (= {:selected {:provider "anthropic-coding-plan" :model "claude-opus-4-7"}
                  :actual {:provider "zai-coding-plan" :model "glm-5.1"}
                  :fallback? true
                  :fallback-trace [{:provider-id :anthropic-coding-plan
                                    :model "claude-opus-4-7"
                                    :reason :transient-error
                                    :error "HTTP 529 overloaded"}]}
                (metadata selected result)))
      (expect (= "zai-coding-plan" (get-in (attach {:cost {:total-cost 0.01}} selected result) [:cost :provider])))
      (expect (= "glm-5.1" (get-in (attach {:cost {:total-cost 0.01}} selected result) [:cost :model]))))))

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
                   :model "claude-sonnet-4-6"})))))

  (it "does not replay poisoned z.ai-style signatures recorded as Anthropic"
    (let [append-replay #'loop/append-preserved-thinking-replay
          messages      [{:role "user" :content "continue"}]
          poisoned-message {:role "assistant"
                            :content [{:type "thinking"
                                       :thinking "raw z.ai reasoning text"
                                       :thinking-signature "raw z.ai reasoning text"}
                                      {:type "text" :text "done"}]}
          journal      [[1 {:llm-provider :anthropic-coding-plan
                            :llm-model "claude-opus-4-7"
                            :assistant-message poisoned-message}]]]
      (expect (= messages
                (append-replay messages journal
                  {:provider :anthropic-coding-plan
                   :model "claude-opus-4-7"}))))))

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
      (expect (str/includes? (loop/answer-str ir) "PROVIDER_ERROR"))))

  (it "diagnoses indexed Anthropic thinking signature errors"
    (let [provider-error-ir #'loop/provider-error-ir
          ir (provider-error-ir {:message "Exceptional status code: 400"
                                 :data {:status 400
                                        :body "{\"error\":{\"message\":\"messages.1.content.3: Invalid `signature` in `thinking` block\"}}"}})
          rendered (loop/answer-str ir)]
      (expect (str/includes? rendered "preserved-thinking replay crossed")))))

(defdescribe sci-extension-repl-test
  (it "makes doc apropos and source work for extension alias symbols"
    (let [{:keys [sci-ctx sandbox-ns initial-ns-keys]} (vis/create-sci-context nil)
          env {:sci-ctx sci-ctx
               :sandbox-ns sandbox-ns
               :initial-ns-keys initial-ns-keys
               :extensions (atom [])}
          eval* (fn [code]
                  (:val (sci/eval-string+ sci-ctx code {:ns sandbox-ns})))]
      (vis/install-extension! env clj-ext/clojure-extension)
      (expect (some #(= 'vis.ext.clj/source %)
                (eval* "(repl/apropos \"source\")")))
      (expect (str/includes? (eval* "(repl/source-fn 'z/source)")
                "(defn source"))
      (let [w (java.io.StringWriter.)]
        (sci/binding [sci/out w sci/err w]
          (eval* "(repl/doc z/source)"))
        (expect (str/includes? (str w) "vis.ext.clj/source"))
        (expect (str/includes? (str w) "Parse exact Clojure/EDN source")))
      (let [w (java.io.StringWriter.)]
        (sci/binding [sci/out w sci/err w]
          (eval* "(repl/source z/source)"))
        (expect (str/includes? (str w) "(defn source"))))))
