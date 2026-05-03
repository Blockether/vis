(ns com.blockether.vis.internal.loop-test
  (:require
   [com.blockether.svar.core :as svar]
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
              (loop/answer-str "Here's the top-level directory structure of this repo:```text\n\n```So: I'm a language model.")))))

(defdescribe final-answer-gate-error-test
  (it "rejects when no focused intent exists for the current turn"
    (let [s   (vis/db-create-connection! :memory)
          cid (vis/db-store-conversation! s {:channel :cli})
          tid (vis/db-store-conversation-turn! s {:parent-conversation-id cid
                                                  :user-request "hi"
                                                  :status :running})]
      (try
        (let [error (loop/final-answer-gate-error
                      {:db-info s :current-conversation-turn-id-atom (atom tid)}
                      0 [])]
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
                                           :tokens {:input 1 :output 1}
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
            (expect (= "(+ 1 1)" (:llm-executable-code result)))
            (expect (= executable-blocks (:llm-executable-blocks result)))
            (let [prov (:provenance (first (:blocks result)))]
              (expect (= :vis/eval (:op prov)))
              (expect (= 1 (:iteration prov)))
              (expect (= 1 (:form-position prov)))
              (expect (= 1 (:form-count prov)))
              (expect (= "i1.1" (:ref prov)))
              (expect (= :vis/sci (:engine prov))))))))))

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
          (expect (= :vis/silent (:result (first @chunks)))))))))

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

  (it "rejects post-first-iteration final answers mixed with sibling top-level forms"
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
                         {:iteration 1
                          :resolved-model {:provider :test :name "model"}})]
            (expect (nil? (:final-result result)))
            (expect (= 2 (count (:blocks result))))
            (expect (re-find #"ONLY top-level form"
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
                      (:final-result result)))))))))

(defdescribe markdown-fence-guard-test
  (it "rejects multi-line fence-only fragments before SCI eval"
    (let [environment (env/create-sci-context nil)
          result (#'loop/execute-code environment (str "```" "\n\n" "```clojure"))]
      (expect (string? (:error result)))
      (expect (re-find #"Markdown fence" (:error result)))
      (expect (not (re-find #"StackOverflowError" (:error result))))
      (expect (= 0 (:execution-time-ms result))))))

(defdescribe prepare-turn-context-test
  (it "preserves provider-specific extra-body opts for downstream LLM calls"
    (with-redefs [env/bump-var-index! (fn [_] nil)
                  env/sci-update-binding! (fn [& _] nil)]
      (let [ctx (#'loop/prepare-turn-context
                 {:db-info         :db
                  :conversation-id "c1"
                  :environment-id  "e1"
                  :state-atom      (atom {})
                  :sci-ctx         nil}
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
