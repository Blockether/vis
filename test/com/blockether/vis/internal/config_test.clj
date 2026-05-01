(ns com.blockether.vis.internal.config-test
  (:require [com.blockether.vis.internal.config :as config]
            [com.blockether.vis.internal.registry :as registry]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe provider-template-test
  (it "exposes the OpenAI Codex OAuth preset"
    (let [preset (config/provider-template :openai-codex)]
      (expect (= "OpenAI Codex (ChatGPT OAuth)" (:label preset)))
      (expect (= "https://chatgpt.com/backend-api" (:base-url preset)))
      (expect (= :openai-compatible-responses (:api-style preset)))
      (expect (= "gpt-5.5" (first (:default-models preset))))
      (expect (some #{"gpt-5.1"} (:default-models preset)))))

  (it "keeps local provider base URLs while letting Ollama discover models dynamically"
    (let [ollama   (config/provider-template :ollama)
          lmstudio (config/provider-template :lmstudio)]
      (expect (= "http://localhost:11434/v1" (:base-url ollama)))
      (expect (nil? (:default-models ollama)))
      (expect (= "http://localhost:1234/v1" (:base-url lmstudio))))))

(defdescribe load-config-test
  (it "adds catalog metadata without rewriting provider-specific fields"
    (with-redefs [config/load-config-raw (fn [] {:providers [{:id :openai-codex
                                                              :models [{:name "gpt-5.5"}]
                                                              :api-key "configured-token"}]})]
      (let [provider (-> (config/load-config) :providers first)]
        (expect (= "https://chatgpt.com/backend-api" (:base-url provider)))
        (expect (= :openai-compatible-responses (:api-style provider)))
        (expect (= "configured-token" (:api-key provider)))))))

(defdescribe svar-provider-shape-test
  (it "forwards provider-specific headers from dynamic token resolvers"
    (with-redefs [registry/provider-by-id (fn [pid]
                                            (when (= pid :openai-codex)
                                              {:provider/get-token-fn (fn []
                                                                        {:token "tok"
                                                                         :api-url "https://chatgpt.com/backend-api"
                                                                         :llm-headers {"chatgpt-account-id" "acct_123"}})}))]
      (let [provider (config/->svar-provider {:id :openai-codex
                                              :models [{:name "gpt-5.5"}]})]
        (expect (= "tok" (:api-key provider)))
        (expect (= "https://chatgpt.com/backend-api" (:base-url provider)))
        (expect (= :openai-compatible-responses (:api-style provider)))
        (expect (= {"chatgpt-account-id" "acct_123"} (:llm-headers provider)))))))

(defdescribe internal-local-provider-registration-test
  (it "registers internal Ollama and LM Studio status providers"
    (let [ollama   (registry/provider-by-id :ollama)
          lmstudio (registry/provider-by-id :lmstudio)]
      (expect (= :ollama (:provider/id ollama)))
      (expect (= :lmstudio (:provider/id lmstudio)))
      (expect (ifn? (:provider/status-fn ollama)))
      (expect (ifn? (:provider/status-fn lmstudio)))))

  (it "status fns return schema-adherent local status maps"
    (with-redefs [config/local-provider-status (fn [provider-id]
                                                 {:authenticated? true
                                                  :provider-id    provider-id
                                                  :source         :local
                                                  :base-url       "http://localhost"
                                                  :status-code    200})]
      (let [status ((:provider/status-fn (registry/provider-by-id :ollama)))]
        (expect (= true (:authenticated? status)))
        (expect (= :ollama (:provider-id status)))
        (expect (= :local (:source status)))
        (expect (= 200 (:status-code status)))))))

(defdescribe model-name-test
  (it "extracts model names from strings and maps"
    (expect (= "gpt-5.1" (config/model-name "gpt-5.1")))
    (expect (= "gpt-5.1" (config/model-name {:name "gpt-5.1"})))
    (expect (nil? (config/model-name nil)))))
