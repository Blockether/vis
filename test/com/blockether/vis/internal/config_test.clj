(ns com.blockether.vis.internal.config-test
  (:require [com.blockether.vis.internal.config :as config]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe provider-template-test
  (it "exposes the OpenAI Codex OAuth preset"
    (let [preset (config/provider-template :openai-codex)]
      (expect (= "OpenAI Codex (ChatGPT OAuth)" (:label preset)))
      (expect (= "https://chatgpt.com/backend-api" (:base-url preset)))
      (expect (= :openai-codex (:api-style preset)))
      (expect (some #{"gpt-5.1"} (:default-models preset))))))

(defdescribe load-config-test
  (it "backfills OpenAI Codex runtime metadata for existing config files"
    (with-redefs [config/load-config-raw (fn [] {:providers [{:id :openai-codex
                                                              :models [{:name "gpt-5.5"}]}]})]
      (let [provider (-> (config/load-config) :providers first)]
        (expect (= "https://chatgpt.com/backend-api" (:base-url provider)))
        (expect (= :openai-codex (:api-style provider)))))))

(defdescribe model-name-test
  (it "extracts model names from strings and maps"
    (expect (= "gpt-5.1" (config/model-name "gpt-5.1")))
    (expect (= "gpt-5.1" (config/model-name {:name "gpt-5.1"})))
    (expect (nil? (config/model-name nil)))))
