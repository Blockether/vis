(ns com.blockether.vis.internal.loop.copilot-metadata-test
  (:require [com.blockether.svar.core :as svar]
            [com.blockether.svar.internal.llm :as llm]
            [com.blockether.svar.internal.router :as svar-router]
            [com.blockether.vis.internal.config.core :as config]
            [com.blockether.vis.internal.config.validation :as validation]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.internal.loop.router :as router]
            [com.blockether.vis.internal.provider.error :as perr]
            [lazytest.core :refer [defdescribe expect it]]))

(defn- environment
  ([model] (environment model "test"))
  ([model credential]
   (with-redefs-fn {#'router/runtime-router-providers (constantly [{:id :github-copilot
                                                                    :api-key credential
                                                                    :base-url
                                                                    "https://gateway.example.com/v1"
                                                                    :models [model]}])}
     #(hash-map :router (router/build-router {})))))

(defn- catalog-row
  [model]
  {"id" model
   "supported_endpoints" ["/v1/messages" "/chat/completions"]
   "capabilities"
   {"supports" {"vision" false "tool_calls" true "parallel_tool_calls" false "reasoning" true}
    "limits"
    {"max_context_window_tokens" 200000 "max_prompt_tokens" 168000 "max_output_tokens" 32000}}})

(defn- with-catalog
  [rows f]
  (llm/clear-models-cache!)
  (let [calls (atom 0)]
    (with-redefs-fn {#'llm/http-get! (fn [& _]
                                       (swap! calls inc)
                                       {"data" rows})}
      #(f calls))))

;; Regression: #304. Exercise discovery, Vis normalization, routing and Svar's real budget.
(defdescribe
  copilot-first-request-metadata
  (it "hydrates Sonnet 5.5 before preflight and reuses the account-scoped catalog"
      (let [original (environment {:name "claude-sonnet-5.5"})]
        (with-catalog
          [(catalog-row "claude-sonnet-5.5")]
          (fn [calls]
            (let [hydrated (router/hydrate-request-model-metadata original {})
                  repeated (router/hydrate-request-model-metadata original {})
                  budget (svar/context-budget (:router hydrated) {})
                  model (first (get-in hydrated [:router :providers 0 :models]))]

              (expect (= 1 @calls))
              (expect (= hydrated repeated))
              (expect (identical? (get-in original [:router :state])
                                  (get-in hydrated [:router :state])))
              (expect (nil? (get-in original [:router :providers 0 :models 0 :input-limit])))
              (expect (= 168000 (:max-input-tokens budget)))
              (expect (= 32000 (:output-reserve budget)))
              (expect (= :anthropic (:api-style model)))
              (expect (not (contains? (:capabilities model) :vision)))
              (expect (false? (:parallel-tool-calls? model)))
              (expect (:ok? (svar-router/check-context-limit
                              (:name model)
                              []
                              {:input-tokens 17177
                               :input-limit (:input-limit model)
                               :context-limits {(:name model) (:context model)}
                               :output-reserve (:output-reserve budget)})))
              (expect (identical? hydrated (router/hydrate-request-model-metadata hydrated {})))
              (expect (= 1 @calls)))))))
  (it "keeps explicit output and vision settings above discovered metadata"
      (with-catalog
        [(catalog-row "claude-sonnet-future")]
        (fn [_]
          (let [env
                (environment {:name "claude-sonnet-future" :output-limit 1000 :vision? true})

                hydrated
                (router/hydrate-request-model-metadata env {})

                model
                (first (get-in hydrated [:router :providers 0 :models]))]

            (expect (= 1000 (:output-reserve (svar/context-budget (:router hydrated) {}))))
            (expect (contains? (:capabilities model) :vision))))))
  (it "does not block an unrelated selected provider on Copilot discovery"
      (with-catalog []
                    (fn [calls]
                      (let [env (update (environment {:name "claude-sonnet-5.5"})
                                        :router
                                        update
                                        :providers
                                        conj
                                        (first (:providers (svar/make-router
                                                             [{:id :openai
                                                               :api-key "test"
                                                               :models [{:name "gpt-4o"}]}]))))]
                        (expect (identical? env
                                            (router/hydrate-request-model-metadata
                                              env
                                              {:provider :openai :model "gpt-4o"})))
                        (expect (zero? @calls))))))
  (it "does not reuse learned limits after the credential identity changes"
      (with-catalog
        [(catalog-row "claude-sonnet-5.5")]
        (fn [calls]
          (let [hydrated
                (router/hydrate-request-model-metadata (environment {:name "claude-sonnet-5.5"}) {})

                switched
                (update-in hydrated
                           [:router :providers 0]
                           #(-> %
                                (assoc :api-key "other-test")
                                (@#'router/hydrate-model-metadata)))

                fresh
                (router/hydrate-request-model-metadata switched {})]

            (expect (nil? (get-in switched [:router :providers 0 :models 0 :input-limit])))
            (expect (= 168000 (:max-input-tokens (svar/context-budget (:router fresh) {}))))
            (expect (= 2 @calls)))))))

(defdescribe copilot-unavailable-metadata
             (it "shows a metadata diagnostic for absent, output-only and inconsistent catalogs"
                 (doseq [rows [[] [{"id" "claude-sonnet-5.5" "max_output_tokens" 8192}]
                               [(assoc-in (catalog-row "claude-sonnet-5.5")
                                  ["capabilities" "limits" "max_prompt_tokens"]
                                  300000)]]]
                   (with-catalog
                     rows
                     (fn [_]
                       (let [error (try (router/hydrate-request-model-metadata
                                          (environment {:name "claude-sonnet-5.5"})
                                          {})
                                        nil
                                        (catch clojure.lang.ExceptionInfo e e))]
                         (expect (= :svar.llm/model-metadata-unavailable (:type (ex-data error))))
                         (expect (= :model-metadata (perr/provider-error-kind error)))
                         (expect (= "Model limits unavailable" (perr/provider-error-title error)))
                         (expect (false? (perr/context-overflow-error? error)))
                         (expect (false? (perr/provider-error-retryable? error)))
                         (expect (re-find #"request was not sent"
                                          (perr/provider-error-explanation error)))
                         (expect (nil? (:max-input-tokens (ex-data error))))))))))

(defdescribe copilot-config-capabilities
             (it "retains explicit false capabilities through the config and schema boundaries"
                 (let [model
                       {:name "claude-sonnet-future"
                        :vision? false
                        :reasoning? false
                        :parallel-tool-calls? false
                        :tool-call? false}

                       provider
                       {"id" "github-copilot" "models" [(wire/->wire model)]}]

                   (expect (= model (config/->svar-model model)))
                   (expect (validation/valid? {"providers" [provider]}))
                   (expect (not (validation/valid? {"providers" [(assoc-in provider
                                                                   ["models" 0 "is_vision"]
                                                                   "false")]}))))))
