(ns com.blockether.vis.internal.loop-test
  (:require [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.foundation.core :as foundation]
            [com.blockether.vis.ext.lang-clojure.core :as clj-ext]
            [com.blockether.vis.internal.loop :as loop]
            [lazytest.core :refer [defdescribe expect it]]
            [sci.core :as sci]))

(defdescribe reload-extension-diff-test
  (it "diffs by manifest loader ns while removing logical extension ids"
    (let [diff (#'loop/diff-extensions
                [{:ext/namespace 'com.example.sqlite.core
                  :ext/nses      ['com.example.sqlite.registrar]}
                 {:ext/namespace 'com.example.provider.plan-a
                  :ext/nses      ['com.example.provider]}
                 {:ext/namespace 'com.example.removed.core}]
                {'sqlite  {:nses ['com.example.sqlite.registrar]}
                 'provider {:nses ['com.example.provider]}
                 'new      {:nses ['com.example.new]}})]
      (expect (= ['com.example.new] (:added diff)))
      (expect (= ['com.example.removed.core] (:removed diff)))
      (expect (= ['com.example.provider 'com.example.sqlite.registrar]
                (:reloaded diff))))))

(defdescribe host-final-surface-test
  (it "recognizes final-answer and title host forms, including guarded answers"
    (let [answer-call?      #'loop/turn-answer-call-form?
          contains-answer? #'loop/form-contains-turn-answer-call?
          direct-answer?   #'loop/direct-answer-entry?
          title-form?      #'loop/conversation-title-meta-form?]
      (expect (true? (answer-call? '(turn-answer! [:ir [:p "Done"]]))))
      (expect (false? (answer-call? '(answer [:ir [:p "Done"]]))))
      (expect (true? (contains-answer? "(when ok? (turn-answer! [:ir [:p \"Done\"]]))")))
      (expect (false? (direct-answer? "(when ok? (turn-answer! [:ir [:p \"Done\"]]))")))
      (expect (nil? (#'loop/answer-with-mutation-preflight-mismatch
                     [{:expr "(when ok? (turn-answer! [:ir [:p \"Done\"]]))"}])))
      (expect (true? (title-form? "(set-conversation-title! \"Prompt cleanup\")")))
      (expect (false? (title-form? "(conversation-title \"Prompt cleanup\")")))))

  (it "does not collapse non-mutating sibling forms beside a final answer"
    (let [preflight #'loop/code-entries-preflight
          entries (:code-entries
                   (preflight 1
                     (str "(def a 1)\n\n"
                       "(set-conversation-title! \"Def a 1\")\n\n"
                       "(turn-answer! [:ir [:p \"Done\"]])")))]
      (expect (= 3 (count entries)))
      (expect (= ["(def a 1)"
                  "(set-conversation-title! \"Def a 1\")"
                  "(turn-answer! [:ir [:p \"Done\"]])"]
                (mapv :expr entries)))))

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
      (expect (str/includes? (:expr (first entries)) "\"\\\\e[200~\""))))

  (it "caps one parser repair attempt at about one second"
    (let [repair #'loop/try-repair-with-timeout
          start  (System/nanoTime)
          fixed  (repair :slow-test
                   (fn [_src _err]
                     (Thread/sleep 5000)
                     "fixed")
                   "(broken" "parse failed")
          elapsed-ms (/ (double (- (System/nanoTime) start)) 1000000.0)]
      (expect (nil? fixed))
      (expect (< elapsed-ms 1500.0)))))

(defdescribe copilot-headers-test
  (it "builds Copilot X-Initiator headers only for Copilot providers"
    (let [headers #'loop/copilot-llm-headers]
      (expect (= {"X-Initiator" "agent"}
                (headers {:provider :github-copilot-individual} "agent")))
      (expect (= {"X-Initiator" "user"}
                (headers {:provider :github-copilot-business} "user")))
      (expect (nil? (headers {:provider :anthropic-coding-plan} "agent")))
      (expect (nil? (headers {:provider :github-copilot-individual} "other"))))))

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

(defdescribe sci-extension-symbol-introspection-test
  (it "makes v/engine-symbol-* work for extension alias symbols"
    (let [{:keys [sci-ctx sandbox-ns initial-ns-keys]} (vis/create-sci-context nil)
          env {:sci-ctx sci-ctx
               :sandbox-ns sandbox-ns
               :initial-ns-keys initial-ns-keys
               :extensions (atom [])}
          eval* (fn [code]
                  (:val (sci/eval-string+ sci-ctx code {:ns sandbox-ns})))]
      (vis/install-extension! env foundation/vis-extension)
      (vis/install-extension! env clj-ext/clojure-extension)
      (let [matches (eval* "(v/engine-symbol-apropos \"source\")")]
        (expect (some #(= 'z/source (:symbol %)) (:matches matches))))
      (let [doc (eval* "(v/engine-symbol-documentation 'z/source)")]
        (expect (= 'z/source (:symbol doc)))
        (expect (str/includes? (:doc doc) "Parse exact Clojure/EDN source")))
      (let [source (eval* "(v/engine-symbol-source-code 'z/source)")]
        (expect (str/includes? (:source source) "(defn source")))
      (let [metadata (eval* "(v/engine-symbol-metadata 'z/source)")]
        (expect (true? (get-in metadata [:metadata :has-source?])))
        (expect (seq (get-in metadata [:metadata :arglists])))))))
