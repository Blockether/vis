(ns com.blockether.vis.internal.loop-test
  (:require
   [com.blockether.svar.core :as svar]
   [com.blockether.svar.internal.llm :as svar-llm]
   [com.blockether.vis.core :as vis]
   [com.blockether.vis.ext.persistance-sqlite.core]
   [com.blockether.vis.internal.config :as config]
   [com.blockether.vis.internal.env :as env]
   [com.blockether.vis.internal.loop :as loop]
   [lazytest.core :refer [defdescribe expect it]]))

(defdescribe normalize-reasoning-level-test
  (it "accepts the UI reasoning vocabulary and OpenAI aliases"
    (expect (= :quick (loop/normalize-reasoning-level :quick)))
    (expect (= :balanced (loop/normalize-reasoning-level "medium")))
    (expect (= :deep (loop/normalize-reasoning-level "HIGH")))))

(defdescribe copilot-billing-guard-test
  (it "marks first human iteration as user and internal iterations as agent"
    (expect (= "user" (#'loop/copilot-initiator-for-iteration 0)))
    (expect (= "agent" (#'loop/copilot-initiator-for-iteration 1))))

  (it "disables reasoning for casual Copilot Claude turns"
    (expect (nil? (#'loop/copilot-claude-safe-reasoning-level
                   {:provider :github-copilot :name "claude-sonnet-4.6"}
                   "siema"
                   :deep
                   {}))))

  (it "caps Copilot Claude deep reasoning unless explicitly allowed"
    (expect (= :balanced
              (#'loop/copilot-claude-safe-reasoning-level
               {:provider :github-copilot :name "claude-sonnet-4.6"}
               "fix this failing test"
               :deep
               {})))
    (expect (= :deep
              (#'loop/copilot-claude-safe-reasoning-level
               {:provider :github-copilot :name "claude-sonnet-4.6"}
               "fix this failing test"
               :deep
               {:allow-copilot-claude-deep? true}))))

  (it "leaves non-Copilot-Claude reasoning untouched"
    (expect (= :deep
              (#'loop/copilot-claude-safe-reasoning-level
               {:provider :github-copilot :name "gpt-5.4"}
               "siema"
               :deep
               {}))))

  (it "uses dynamic log-context to override svar Copilot X-Initiator"
    (#'loop/install-copilot-header-patch!)
    (binding [svar-llm/*log-context* (assoc svar-llm/*log-context* :copilot-initiator "agent")]
      (let [headers ((deref (#'loop/svar-copilot-dynamic-headers-var)) [{:role "user" :content "synthetic journal"}])]
        (expect (= "agent" (get headers "X-Initiator")))
        (expect (= "conversation-edits" (get headers "Openai-Intent"))))))

  (it "preserves the normal user initiator when no override is bound"
    (#'loop/install-copilot-header-patch!)
    (binding [svar-llm/*log-context* (dissoc svar-llm/*log-context* :copilot-initiator)]
      (expect (= "user"
                (get ((deref (#'loop/svar-copilot-dynamic-headers-var)) [{:role "user" :content "real prompt"}])
                  "X-Initiator")))))

  (it "marks auto-title Copilot calls as agent and disables reasoning"
    (let [s (vis/db-create-connection! :memory)
          cid (vis/db-store-conversation! s {:channel :cli})
          title-atom (atom "")
          seen (atom nil)]
      (try
        (#'loop/install-copilot-header-patch!)
        (with-redefs-fn {#'svar/ask-code! (fn [_ opts]
                                            (reset! seen
                                              {:opts opts
                                               :headers ((deref (#'loop/svar-copilot-dynamic-headers-var))
                                                         [{:role "user" :content "title request"}])})
                                            {:result "Short title"
                                             :raw "Short title"
                                             :tokens {:input 1 :output 1}})}
          (fn []
            (#'loop/auto-title! {:router ::router
                                 :db-info s
                                 :conversation-id cid
                                 :conversation-title-atom title-atom
                                 :user-request "hello"})
            (expect (= "agent" (get-in @seen [:headers "X-Initiator"])))
            (expect (= "conversation-edits" (get-in @seen [:headers "Openai-Intent"])))
            (expect (= :off (get-in @seen [:opts :reasoning])))
            (expect (= "Short title" @title-atom))))
        (finally
          (vis/db-dispose-connection! s))))))

(defdescribe iteration-block-spec-test
  (it "rejects iteration blocks without required provenance"
    (let [thrown? (try
                    (#'loop/validate-iteration-blocks!
                     [{:id 0
                       :code "(+ 1 2)"
                       :result 3
                       :stdout ""
                       :stderr ""
                       :error nil
                       :execution-time-ms 1
                       :timeout? false
                       :repaired? false}])
                    false
                    (catch clojure.lang.ExceptionInfo e
                      (= :vis/invalid-iteration-block (:type (ex-data e)))))]
      (expect (true? thrown?)))))

(defdescribe iteration-exception-handling-test
  (it "marks provider HTTP failures as terminal turn errors"
    (let [result (#'loop/handle-iteration-exception!
                  (ex-info "connect failed"
                    {:type :svar.core/http-error
                     :cause-class "java.net.ConnectException"})
                  {:iteration 0 :messages [] :routing {} :reasoning-level :balanced})]
      (expect (some? (get result (keyword "com.blockether.vis.internal.loop" "iteration-error"))))
      (expect (true? (get result (keyword "com.blockether.vis.internal.loop" "fatal-iteration-error"))))))

  (it "keeps model-format failures recoverable by the RLM"
    (let [result (#'loop/handle-iteration-exception!
                  (ex-info "bad format" {:type :svar.llm/invalid-json})
                  {:iteration 0 :messages [] :routing {} :reasoning-level :balanced})]
      (expect (some? (get result (keyword "com.blockether.vis.internal.loop" "iteration-error"))))
      (expect (nil? (get result (keyword "com.blockether.vis.internal.loop" "fatal-iteration-error")))))))

(defdescribe answer-str-test
  (it "repairs glued fence boundaries before the answer leaves the loop"
    (expect (= "Here's the top-level directory structure of this repo:\n```text\n\n```\nSo: I'm a language model."
              (loop/answer-str "Here's the top-level directory structure of this repo:```text\n\n```So: I'm a language model."))))

  (it "renders explicit needs-input payloads as their ask text"
    (expect (= "Please paste the ideas you want reviewed."
              (loop/answer-str {:vis/answer-mode :needs-input
                                :answer/text "Please paste the ideas you want reviewed."})))))

(defdescribe runtime-proof-appendix-test
  (it "appends proofs to accepted normal answers in the runtime, not model boilerplate"
    (with-redefs-fn {#'loop/runtime-proof-appendix (fn [_] "<proofs>ok</proofs>")}
      (fn []
        (expect (= "Done.\n\n<proofs>ok</proofs>"
                  (loop/append-runtime-proofs {} "Done." "Done."))))))

  (it "does not duplicate an existing proofs block"
    (with-redefs-fn {#'loop/runtime-proof-appendix (fn [_] "<proofs>new</proofs>")}
      (fn []
        (expect (= "Done.\n\n<proofs>old</proofs>"
                  (loop/append-runtime-proofs {} "Done.\n\n<proofs>old</proofs>" "Done."))))))

  (it "does not append proofs to needs-input clarification answers"
    (with-redefs-fn {#'loop/runtime-proof-appendix (fn [_] "<proofs>ok</proofs>")}
      (fn []
        (expect (= "Please paste the ideas."
                  (loop/append-runtime-proofs {}
                    "Please paste the ideas."
                    {:vis/answer-mode :needs-input
                     :answer/text "Please paste the ideas."})))))))

(defdescribe intent-required-test
  (it "requires intents for pasted-content inspection and terminal demonstrations"
    (expect (true? (#'loop/intent-required? "What did I send to you right now?! # Introduction")))
    (expect (true? (#'loop/intent-required? "[Pasted #1: 129 lines, 4.6KB]")))
    (expect (true? (#'loop/intent-required? "Show me how you are calling bash and doing some ls etc."))))

  (it "keeps trivial chat free of intent ceremony"
    (expect (false? (#'loop/intent-required? "hi")))
    (expect (false? (#'loop/intent-required? "how are you today?")))))

(defdescribe final-answer-gate-error-test
  (it "accepts trivial chat when no focused intent exists"
    (let [s   (vis/db-create-connection! :memory)
          cid (vis/db-store-conversation! s {:channel :cli})
          tid (vis/db-store-conversation-turn! s {:parent-conversation-id cid
                                                  :user-request "hi"
                                                  :status :running})]
      (try
        (expect (nil? (loop/final-answer-gate-error
                        {:db-info s
                         :current-conversation-turn-id-atom (atom tid)
                         :current-user-request-atom (atom "hi")}
                        0 [])))
        (finally
          (vis/db-dispose-connection! s)))))

  (it "rejects pasted-content inspection when no focused intent exists"
    (let [s   (vis/db-create-connection! :memory)
          cid (vis/db-store-conversation! s {:channel :cli})
          tid (vis/db-store-conversation-turn! s {:parent-conversation-id cid
                                                  :user-request "What did I send to you right now?! # Introduction"
                                                  :status :running})]
      (try
        (let [error (loop/final-answer-gate-error
                      {:db-info s
                       :current-conversation-turn-id-atom (atom tid)
                       :current-user-request-atom (atom "What did I send to you right now?! # Introduction")}
                      0 [])]
          (expect (string? error))
          (expect (re-find #":missing-focused-intent" error)))
        (finally
          (vis/db-dispose-connection! s)))))

  (it "rejects evidence-bearing answers when no focused intent exists"
    (let [s   (vis/db-create-connection! :memory)
          cid (vis/db-store-conversation! s {:channel :cli})
          tid (vis/db-store-conversation-turn! s {:parent-conversation-id cid
                                                  :user-request "fix the failing test"
                                                  :status :running})]
      (try
        (let [error (loop/final-answer-gate-error
                      {:db-info s
                       :current-conversation-turn-id-atom (atom tid)
                       :current-user-request-atom (atom "fix the failing test")}
                      0 [])]
          (expect (string? error))
          (expect (re-find #":missing-focused-intent" error)))
        (finally
          (vis/db-dispose-connection! s)))))

  (it "accepts explicit needs-input answers for evidence-bearing requests with no focused intent"
    (let [s   (vis/db-create-connection! :memory)
          cid (vis/db-store-conversation! s {:channel :cli})
          tid (vis/db-store-conversation-turn! s {:parent-conversation-id cid
                                                  :user-request "Please check the ideas we currently have"
                                                  :status :running})]
      (try
        (expect (nil? (loop/final-answer-gate-error
                        {:db-info s
                         :current-conversation-turn-id-atom (atom tid)
                         :current-user-request-atom (atom "Please check the ideas we currently have")}
                        0 []
                        {:vis/answer-mode :needs-input
                         :answer/text "Please paste the ideas you currently have."})))
        (finally
          (vis/db-dispose-connection! s)))))

  (it "accepts first-iteration plain clarification answers before workspace evidence exists"
    (let [s   (vis/db-create-connection! :memory)
          cid (vis/db-store-conversation! s {:channel :cli})
          tid (vis/db-store-conversation-turn! s {:parent-conversation-id cid
                                                  :user-request "Please check the ideas we currently have"
                                                  :status :running})]
      (try
        (expect (nil? (loop/final-answer-gate-error
                        {:db-info s
                         :current-conversation-turn-id-atom (atom tid)
                         :current-user-request-atom (atom "Please check the ideas we currently have")}
                        1 [{:code "(conversation-title \"Review ideas\")"
                            :rendering-kind :vis/silent}
                           {:code "(answer \"Please paste the ideas.\")"
                            :rendering-kind :vis/answer}]
                        "Please paste the ideas.")))
        (finally
          (vis/db-dispose-connection! s)))))

  (it "rejects plain clarification answers after workspace evidence exists"
    (let [s   (vis/db-create-connection! :memory)
          cid (vis/db-store-conversation! s {:channel :cli})
          tid (vis/db-store-conversation-turn! s {:parent-conversation-id cid
                                                  :user-request "Please check the ideas we currently have"
                                                  :status :running})]
      (try
        (let [error (loop/final-answer-gate-error
                      {:db-info s
                       :current-conversation-turn-id-atom (atom tid)
                       :current-user-request-atom (atom "Please check the ideas we currently have")}
                      1 [{:code "(v/cat \"IDEAS.md\")"
                          :rendering-kind :vis/tool}
                         {:code "(answer \"Please paste the ideas.\")"
                          :rendering-kind :vis/answer}]
                      "Please paste the ideas.")]
          (expect (string? error))
          (expect (re-find #":missing-focused-intent" error)))
        (finally
          (vis/db-dispose-connection! s)))))

  (it "rejects answers while a required focused gate is still open"
    (let [s   (vis/db-create-connection! :memory)
          cid (vis/db-store-conversation! s {:channel :cli})
          tid (vis/db-store-conversation-turn! s {:parent-conversation-id cid
                                                  :user-request "fix it"
                                                  :status :running})]
      (try
        (let [intent (vis/db-store-intent! s {:conversation-turn-id tid
                                              :title "Fix it"
                                              :rationale "User asked."})
              plan   (vis/db-store-plan! s {:intent-id (:id intent)
                                            :summary "Plan"})]
          (vis/db-store-gate! s {:plan-id (:id plan)
                                 :question "Verified?"})
          (let [error (loop/final-answer-gate-error
                        {:db-info s :current-conversation-turn-id-atom (atom tid)}
                        0 [])]
            (expect (string? error))
            (expect (re-find #"Final answer rejected" error))
            (expect (re-find #":required-open-gate" error))))
        (finally
          (vis/db-dispose-connection! s)))))

  (it "accepts after the focused intent is proven and fulfilled"
    (let [s   (vis/db-create-connection! :memory)
          cid (vis/db-store-conversation! s {:channel :cli})
          tid (vis/db-store-conversation-turn! s {:parent-conversation-id cid
                                                  :user-request "fix it"
                                                  :status :running})]
      (try
        (let [intent (vis/db-store-intent! s {:conversation-turn-id tid
                                              :title "Fix it"
                                              :rationale "User asked."})
              plan   (vis/db-store-plan! s {:intent-id (:id intent)
                                            :summary "Plan"})
              gate   (vis/db-store-gate! s {:plan-id (:id plan)
                                            :question "Verified?"})
              iid    (vis/db-store-iteration! s {:conversation-turn-id tid
                                                 :blocks [{:code "(+ 1 2)"
                                                           :result 3
                                                           :execution-time-ms 1}]})
              ref    (str "turn/" (subs (str tid) 0 8) "/iteration/1/block/1")]
          (vis/db-prove-gate! s {:gate-id (:id gate)
                                 :summary "Verified."
                                 :refs [ref]})
          (vis/db-fulfill-intent! s (:id intent) {:summary "Done." :refs [ref]})
          (expect (nil? (loop/final-answer-gate-error
                          {:db-info s :current-conversation-turn-id-atom (atom tid)}
                          1 (vis/db-list-iteration-blocks s iid)))))
        (finally
          (vis/db-dispose-connection! s))))))

(defdescribe run-iteration-diagnostics-test
  (it "carries raw LLM response diagnostics from svar to the iteration result"
    (let [raw "```clojure\n(+ 1 1)\n```"
          executable-blocks [{:lang "clojure" :source "(+ 1 1)"}]
          environment {:router ::router
                       :answer-atom (atom nil)
                       :current-form-idx-atom (atom nil)}]
      (with-redefs-fn {#'svar/ask-code! (fn [router opts]
                                          (expect (= ::router router))
                                          (expect (= "clojure" (:lang opts)))
                                          {:raw raw
                                           :blocks executable-blocks
                                           :result "(+ 1 1)"
                                           :tokens {:input 1 :output 1 :cached-input 5}
                                           :duration-ms 1})
                       #'loop/execute-code (fn [_ code]
                                             (expect (= "(+ 1 1)" code))
                                             {:result 2
                                              :stdout ""
                                              :stderr ""
                                              :execution-time-ms 0})}
        (fn []
          (let [result (loop/run-iteration environment
                         [{:role "user" :content "calculate"}]
                         {:iteration 0
                          :resolved-model {:provider :test :name "model"}})]
            (expect (= raw (:llm-raw-response result)))
            (expect (= 5 (get-in result [:api-usage :prompt_tokens_details :cached_tokens])))
            (expect (= "(+ 1 1)" (:llm-executable-code result)))
            (expect (= executable-blocks (:llm-executable-blocks result)))
            (let [prov (:provenance (first (:blocks result)))]
              (expect (= :sci/eval (:op prov)))
              (expect (= :done (:status prov)))
              (expect (= 1 (:iteration prov)))
              (expect (= 1 (:form-position prov)))
              (expect (= 1 (:form-count prov)))
              (expect (= "turn/00000000/iteration/1/block/1" (:ref prov)))))))))

  (it "binds Copilot initiator for svar headers and strips internal extra-body keys"
    (let [seen (atom nil)
          environment {:router ::router
                       :answer-atom (atom nil)
                       :current-form-idx-atom (atom nil)}]
      (with-redefs-fn {#'svar/ask-code! (fn [_ opts]
                                          (reset! seen
                                            {:opts opts
                                             :headers ((deref (#'loop/svar-copilot-dynamic-headers-var))
                                                       [{:role "user" :content "synthetic journal"}])})
                                          {:raw ""
                                           :blocks []
                                           :result ""
                                           :tokens {:input 1 :output 1}
                                           :duration-ms 1})}
        (fn []
          (loop/run-iteration environment
            [{:role "user" :content "continue"}]
            {:iteration 1
             :resolved-model {:provider :github-copilot :name "claude-sonnet-4.6"}
             :extra-body {:copilot-initiator "agent"
                          :text {:verbosity "low"}}})
          (expect (= "agent" (get-in @seen [:headers "X-Initiator"])))
          (expect (= "conversation-edits" (get-in @seen [:headers "Openai-Intent"])))
          (expect (= {:text {:verbosity "low"}}
                    (get-in @seen [:opts :extra-body])))
          (expect (not (contains? (get-in @seen [:opts :extra-body]) :copilot-initiator))))))))

(defdescribe run-iteration-silent-chunk-test
  (it "does not emit form-start for known :vis/silent host forms"
    (let [chunks (atom [])
          environment {:router ::router
                       :answer-atom (atom nil)
                       :current-form-idx-atom (atom nil)}]
      (with-redefs-fn {#'svar/ask-code! (fn [_ _]
                                          {:raw "```clojure\n(conversation-title \"x\")\n```"
                                           :blocks [{:lang "clojure" :source "(conversation-title \"x\")"}]
                                           :result "(conversation-title \"x\")"
                                           :tokens {:input 1 :output 1}
                                           :duration-ms 1})
                       #'loop/execute-code (fn [_ code]
                                             (expect (= "(conversation-title \"x\")" code))
                                             {:result :vis/silent
                                              :stdout ""
                                              :stderr ""
                                              :execution-time-ms 0})}
        (fn []
          (loop/run-iteration environment
            [{:role "user" :content "set title"}]
            {:iteration 0
             :resolved-model {:provider :test :name "model"}
             :on-chunk (fn [chunk] (swap! chunks conj chunk))})
          (expect (= [:form-result]
                    (mapv :phase @chunks)))
          (expect (= :vis/silent (:result (first @chunks))))))))

  (it "does not emit form-start for v/silent! aggregate-shape forms"
    (let [chunks (atom [])
          environment {:router ::router
                       :answer-atom (atom nil)
                       :current-form-idx-atom (atom nil)}]
      (with-redefs-fn {#'svar/ask-code! (fn [_ _]
                                          {:raw "```clojure\n(v/silent! {:a 1})\n```"
                                           :blocks [{:lang "clojure" :source "(v/silent! {:a 1})"}]
                                           :result "(v/silent! {:a 1})"
                                           :tokens {:input 1 :output 1}
                                           :duration-ms 1})
                       #'loop/execute-code (fn [_ code]
                                             (expect (= "(v/silent! {:a 1})" code))
                                             {:result {:rendering-kind :vis/silent}
                                              :stdout ""
                                              :stderr ""
                                              :execution-time-ms 0})}
        (fn []
          (loop/run-iteration environment
            [{:role "user" :content "summarize"}]
            {:iteration 0
             :resolved-model {:provider :test :name "model"}
             :on-chunk (fn [chunk] (swap! chunks conj chunk))})
          (expect (= [:form-result]
                    (mapv :phase @chunks)))
          (expect (= :vis/silent (get-in (first @chunks) [:result :rendering-kind]))))))))

(defdescribe run-iteration-answer-position-test
  (it "accepts first-iteration final answers mixed with earlier top-level forms when answer is last"
    (let [environment {:router ::router
                       :answer-atom (atom nil)
                       :current-form-idx-atom (atom nil)}]
      (with-redefs-fn {#'svar/ask-code! (fn [_ _]
                                          {:raw "```clojure\n(def observed 1)\n(answer \"done\")\n```"
                                           :blocks [{:lang "clojure" :source "(def observed 1)\n(answer \"done\")"}]
                                           :result "(def observed 1)\n(answer \"done\")"
                                           :tokens {:input 1 :output 1}
                                           :duration-ms 1})
                       #'loop/execute-code (fn [env code]
                                             (if (= "(answer \"done\")" code)
                                               (do
                                                 (reset! (:answer-atom env)
                                                   {:value "done"
                                                    :form-idx @(:current-form-idx-atom env)})
                                                 {:result :vis/answer
                                                  :stdout ""
                                                  :stderr ""
                                                  :execution-time-ms 0})
                                               {:result 1
                                                :stdout ""
                                                :stderr ""
                                                :execution-time-ms 0}))}
        (fn []
          (let [result (loop/run-iteration environment
                         [{:role "user" :content "finish"}]
                         {:iteration 0
                          :resolved-model {:provider :test :name "model"}})]
            (expect (= {:final? true
                        :answer "done"
                        :answer-form-idx 1}
                      (:final-result result)))
            (expect (= 2 (count (:blocks result)))))))))

  (it "preflights post-first-iteration final answers mixed with sibling top-level forms"
    (let [executed (atom [])
          environment {:router ::router
                       :answer-atom (atom nil)
                       :current-form-idx-atom (atom nil)}]
      (with-redefs-fn {#'svar/ask-code! (fn [_ _]
                                          {:raw "```clojure\n(def observed 1)\n(answer \"done\")\n```"
                                           :blocks [{:lang "clojure" :source "(def observed 1)\n(answer \"done\")"}]
                                           :result "(def observed 1)\n(answer \"done\")"
                                           :tokens {:input 1 :output 1}
                                           :duration-ms 1})
                       #'loop/execute-code (fn [_ code]
                                             (swap! executed conj code)
                                             {:result :unexpected
                                              :stdout ""
                                              :stderr ""
                                              :execution-time-ms 0})}
        (fn []
          (let [result (loop/run-iteration environment
                         [{:role "user" :content "finish"}]
                         {:iteration 1
                          :resolved-model {:provider :test :name "model"}})]
            (expect (empty? @executed))
            (expect (nil? (:final-result result)))
            (expect (= 2 (count (:blocks result))))
            (expect (every? :error (:blocks result)))
            (expect (re-find #"Do not answer in this iteration"
                      (-> result :blocks second :error))))))))

  (it "accepts a wrapper when it is the only top-level answer form"
    (let [environment {:router ::router
                       :answer-atom (atom nil)
                       :current-form-idx-atom (atom nil)}]
      (with-redefs-fn {#'svar/ask-code! (fn [_ _]
                                          {:raw "```clojure\n(let [body \"done\"] (answer body))\n```"
                                           :blocks [{:lang "clojure" :source "(let [body \"done\"] (answer body))"}]
                                           :result "(let [body \"done\"] (answer body))"
                                           :tokens {:input 1 :output 1}
                                           :duration-ms 1})
                       #'loop/execute-code (fn [env _]
                                             (reset! (:answer-atom env)
                                               {:value "done"
                                                :form-idx @(:current-form-idx-atom env)})
                                             {:result :vis/answer
                                              :stdout ""
                                              :stderr ""
                                              :execution-time-ms 0})}
        (fn []
          (let [result (loop/run-iteration environment
                         [{:role "user" :content "finish"}]
                         {:iteration 0
                          :resolved-model {:provider :test :name "model"}})]
            (expect (= {:final? true
                        :answer "done"
                        :answer-form-idx 0}
                      (:final-result result))))))))

  (it "accepts needs-input answer payloads without creating an intent"
    (let [s   (vis/db-create-connection! :memory)
          cid (vis/db-store-conversation! s {:channel :cli})
          tid (vis/db-store-conversation-turn! s {:parent-conversation-id cid
                                                  :user-request "Please check the ideas we currently have"
                                                  :status :running})
          environment {:router ::router
                       :db-info s
                       :current-conversation-turn-id-atom (atom tid)
                       :current-user-request-atom (atom "Please check the ideas we currently have")
                       :answer-atom (atom nil)
                       :current-form-idx-atom (atom nil)}]
      (try
        (with-redefs-fn {#'svar/ask-code! (fn [_ _]
                                            {:raw "```clojure\n(answer (v/needs-input \"Please paste the ideas you currently have.\"))\n```"
                                             :blocks [{:lang "clojure" :source "(answer (v/needs-input \"Please paste the ideas you currently have.\"))"}]
                                             :result "(answer (v/needs-input \"Please paste the ideas you currently have.\"))"
                                             :tokens {:input 1 :output 1}
                                             :duration-ms 1})
                         #'loop/execute-code (fn [env _]
                                               (reset! (:answer-atom env)
                                                 {:value {:vis/answer-mode :needs-input
                                                          :answer/text "Please paste the ideas you currently have."}
                                                  :form-idx @(:current-form-idx-atom env)})
                                               {:result :vis/answer
                                                :stdout ""
                                                :stderr ""
                                                :execution-time-ms 0})}
          (fn []
            (let [result (loop/run-iteration environment
                           [{:role "user" :content "Please check the ideas we currently have"}]
                           {:iteration 0
                            :resolved-model {:provider :test :name "model"}})]
              (expect (= {:final? true
                          :answer "Please paste the ideas you currently have."
                          :answer-form-idx 0}
                        (:final-result result))))))
        (finally
          (vis/db-dispose-connection! s))))))

(defdescribe markdown-fence-guard-test
  (it "rejects multi-line fence-only fragments before SCI eval"
    (let [environment (env/create-sci-context nil)
          result (#'loop/execute-code environment (str "```" "\n\n" "```clojure"))]
      (expect (string? (:error result)))
      (expect (re-find #"Markdown fence" (:error result)))
      (expect (not (re-find #"StackOverflowError" (:error result))))
      (expect (= 0 (:execution-time-ms result)))))

  (it "aborts the whole iteration before side effects when malformed fence separators leak from ask-code"
    (let [executed (atom [])
          raw-code "(def before :should-not-run)\n``````clojure\n(def after :should-not-run)"
          environment {:router ::router
                       :answer-atom (atom nil)
                       :current-form-idx-atom (atom nil)}]
      (with-redefs-fn {#'svar/ask-code! (fn [_ _]
                                          {:raw (str "```clojure\n" raw-code "\n```")
                                           :blocks [{:lang "clojure" :source raw-code}]
                                           :result raw-code
                                           :tokens {:input 1 :output 1}
                                           :duration-ms 1})
                       #'loop/execute-code (fn [_ code]
                                             (swap! executed conj code)
                                             {:result :unexpected
                                              :stdout ""
                                              :stderr ""
                                              :execution-time-ms 0})}
        (fn []
          (let [result (loop/run-iteration environment
                         [{:role "user" :content "leak fences"}]
                         {:iteration 0
                          :resolved-model {:provider :test :name "model"}})]
            (expect (empty? @executed))
            (expect (= 1 (count (:blocks result))))
            (expect (re-find #"Raw Markdown fence leaked" (-> result :blocks first :error)))
            (expect (re-find #"Aborting the whole iteration" (-> result :blocks first :error)))))))))

(defdescribe turn-scoped-extension-env-test
  (it "lets installed extension symbols see the current turn id"
    (let [env (loop/create-environment
                {:providers [{:id :test :models [{:name "model"}]}]}
                {:db :memory :channel :cli})
          tid (vis/db-store-conversation-turn! (:db-info env)
                {:parent-conversation-id (:conversation-id env)
                 :user-request "track intent"
                 :status :running})]
      (try
        (reset! (:current-conversation-turn-id-atom env) tid)
        (let [result (#'loop/execute-code env
                                          "(v/issue-intent! {:title \"Track intent\" :rationale \"User asked.\"})")]
          (expect (nil? (:error result)))
          (expect (= "Track intent" (-> result :result :title)))
          (expect (= tid (-> result :result :created-conversation-turn-id))))
        (finally
          (loop/dispose-environment! env))))))

(defdescribe prepare-turn-context-test
  (it "preserves provider-specific extra-body opts for downstream LLM calls"
    (with-redefs [env/bump-var-index! (fn [_] nil)
                  env/sci-update-binding! (fn [& _] nil)]
      (let [ctx (#'loop/prepare-turn-context
                 {:db-info         :db
                  :conversation-id "c1"
                  :environment-id  "e1"
                  :state-atom      (atom {})
                  :sci-ctx         nil
                  :current-iteration-atom (atom 1)
                  :current-iteration-id-atom (atom nil)
                  :current-conversation-turn-id-atom (atom nil)}
                 [{:role "user" :content "hello"}]
                 {:extra-body {:text {:verbosity "high"}}})]
        (expect (= {:text {:verbosity "high"}} (:extra-body ctx)))))))

(defdescribe router-provider-resolution-test
  (it "resolves OAuth provider credentials before constructing the router"
    (let [seen-providers (atom nil)
          durable-config {:providers [{:id :openai-codex
                                       :models [{:name "gpt-5.1"}]}]}
          runtime-provider {:id :openai-codex
                            :models [{:name "gpt-5.1"}]
                            :api-key "resolved-token"
                            :llm-headers {"chatgpt-account-id" "acct_123"}}]
      (loop/reset-router!)
      (try
        (with-redefs [config/resolve-config (constantly durable-config)
                      config/->svar-provider (fn [provider]
                                               (expect (= (first (:providers durable-config)) provider))
                                               runtime-provider)
                      svar/make-router (fn [providers]
                                         (reset! seen-providers providers)
                                         ::router)]
          (expect (= ::router (loop/get-router)))
          (expect (= [runtime-provider] @seen-providers)))
        (finally
          (loop/reset-router!))))))

(lazytest.core/describe "code entry preflight"
  (lazytest.core/it "code-entries-preflight rejects raw Markdown fence leaks before parsing"
    (let [preflight (var-get (ns-resolve 'com.blockether.vis.internal.loop
                               'code-entries-preflight))
          fence (apply str (repeat 3 "`"))
          result (preflight 1 (str fence "clojure\n"
                                "(def leaked 1)\n"
                                fence "\n"))]
      (lazytest.core/expect (= 1 (count (:code-entries result))))
      (lazytest.core/expect (:raw-fence-preflight-error result))
      (lazytest.core/expect (not (:answer-preflight-error result)))
      (lazytest.core/expect (:parse-error (first (:code-entries result))))))

  (lazytest.core/it "code-entries-preflight collapses answer position violations to one failed entry"
    (let [preflight (var-get (ns-resolve 'com.blockether.vis.internal.loop
                               'code-entries-preflight))
          result (preflight 2 "(def x 1)\n(answer \"bad\")\n(def y 2)\n")]
      (lazytest.core/expect (= 1 (count (:code-entries result))))
      (lazytest.core/expect (:answer-preflight-error result))
      (lazytest.core/expect (:parse-error (first (:code-entries result)))))))
