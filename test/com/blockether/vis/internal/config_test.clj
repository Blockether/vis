(ns com.blockether.vis.internal.config-test
  (:require [com.blockether.vis.ext.provider-github-copilot]
            [com.blockether.vis.ext.provider-lmstudio]
            [com.blockether.vis.ext.provider-ollama]
            [com.blockether.vis.ext.provider-openai]
            [com.blockether.vis.ext.provider-openai-codex]
            [com.blockether.vis.ext.provider-zai]
            [com.blockether.vis.internal.config :as config]
            [com.blockether.vis.internal.registry :as registry]
            [lazytest.core :refer [defdescribe expect it]])
  (:import
   (java.nio.file Files)))

(defdescribe provider-template-test
  (it "exposes the OpenAI Codex OAuth preset"
    (let [preset (config/provider-template :openai-codex)]
      (expect (= "OpenAI Codex (ChatGPT OAuth)" (:label preset)))
      (expect (= "https://chatgpt.com/backend-api" (:base-url preset)))
      (expect (= :openai-compatible-responses (:api-style preset)))
      (expect (= "gpt-5.5" (first (:default-models preset))))
      (expect (= ["gpt-5.5" "gpt-5.4" "gpt-5.3-codex"] (:default-models preset)))
      (expect (config/provider-model-visible? :openai-codex "gpt-5.3-codex"))
      (expect (not (config/provider-model-visible? :openai-codex "gpt-5.2-codex")))
      (expect (not (config/provider-model-visible? :github-copilot-business "gpt-4o")))
      (expect (nil? (config/provider-template :github-copilot)))
      (expect (nil? (config/provider-template :blockether)))
      (expect (nil? (config/provider-template :openrouter)))
      (expect (nil? (config/provider-template :github-models)))))

  (it "reads standard provider presets from provider extensions"
    (let [openai (config/provider-template :openai)]
      (expect (= "OpenAI" (:label openai)))
      (expect (= "gpt-5" (first (:default-models openai))))))

  (it "keeps local provider base URLs while letting Ollama discover models dynamically"
    (let [ollama   (config/provider-template :ollama)
          lmstudio (config/provider-template :lmstudio)]
      (expect (= "http://localhost:11434/v1" (:base-url ollama)))
      (expect (nil? (:default-models ollama)))
      (expect (= "http://localhost:1234/v1" (:base-url lmstudio))))))

(defdescribe extension-env-config-test
  (it "persists extension env overrides and clears blank values"
    (let [dir (.toString (Files/createTempDirectory "vis-extension-env-test" (make-array java.nio.file.attribute.FileAttribute 0)))
          path (str dir "/config.edn")]
      (with-redefs [config/config-dir dir
                    config/config-path path]
        (expect (= :unset (:source (config/extension-env-status "EXA_API_KEY"))))
        (config/save-extension-env-var! "EXA_API_KEY" " secret ")
        (expect (= {:name "EXA_API_KEY" :source :config :value "secret"}
                  (config/extension-env-status "EXA_API_KEY")))
        (expect (= "secret" (get-in (config/load-config-raw) [:environment "EXA_API_KEY"])))
        (config/save-extension-env-var! "EXA_API_KEY" "")
        (expect (= :unset (:source (config/extension-env-status "EXA_API_KEY"))))
        (expect (nil? (:environment (config/load-config-raw))))))))

(defdescribe load-config-test
  (it "adds catalog metadata without rewriting provider-specific fields"
    (with-redefs [config/load-config-raw (fn [] {:providers [{:id :openai-codex
                                                              :models [{:name "gpt-5.5"}]
                                                              :api-key "configured-token"}]})]
      (let [provider (-> (config/load-config) :providers first)]
        (expect (= "https://chatgpt.com/backend-api" (:base-url provider)))
        (expect (= :openai-compatible-responses (:api-style provider)))
        (expect (= "configured-token" (:api-key provider)))))))

(defdescribe save-config-provider-selection-hook-test
  (it "fires the selected provider hook when the active provider changes"
    (let [dir (.toString (Files/createTempDirectory "vis-config-test" (make-array java.nio.file.attribute.FileAttribute 0)))
          path (str dir "/config.edn")
          calls (atom [])]
      (with-redefs [config/config-dir dir
                    config/config-path path
                    registry/provider-by-id (fn [pid]
                                              (when (= :new pid)
                                                {:provider/id :new
                                                 :provider/label "New"
                                                 :provider/on-selected-fn #(swap! calls conj %)}))]
        (config/save-config! {:providers [{:id :old :models [{:name "old-model"}]}]} :seed)
        (reset! calls [])
        (config/save-config! {:providers [{:id :new :models [{:name "new-model"}]}]} :test)
        (expect (= 1 (count @calls)))
        (expect (= :old (get-in @calls [0 :previous-provider :id])))
        (expect (= :new (get-in @calls [0 :provider :id])))
        (expect (= :test (get-in @calls [0 :source])))
        (config/save-config! {:providers [{:id :new :models [{:name "new-model-2"}]}]} :test)
        (expect (= 1 (count @calls)))))))

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
        (expect (= {"chatgpt-account-id" "acct_123"} (:llm-headers provider))))))

  (it "prefers OAuth token API URL over catalog Copilot base URL"
    (with-redefs [registry/provider-by-id (fn [pid]
                                            (when (= pid :github-copilot-business)
                                              {:provider/preset {:base-url "https://api.business.githubcopilot.com"}
                                               :provider/get-token-fn (fn []
                                                                        {:token "tok"
                                                                         :api-url "https://api.business.githubcopilot.com"})}))]
      (let [provider (config/->svar-provider {:id :github-copilot-business
                                              :base-url "https://api.business.githubcopilot.com"
                                              :models [{:name "claude-opus-4-6"}]})]
        (expect (= "tok" (:api-key provider)))
        (expect (= "https://api.business.githubcopilot.com" (:base-url provider))))))

  (it "preserves custom provider URLs over OAuth token API URLs"
    (with-redefs [registry/provider-by-id (fn [pid]
                                            (when (= pid :github-copilot-business)
                                              {:provider/preset {:base-url "https://api.business.githubcopilot.com"}
                                               :provider/get-token-fn (fn []
                                                                        {:token "tok"
                                                                         :api-url "https://api.business.githubcopilot.com"})}))]
      (let [provider (config/->svar-provider {:id :github-copilot-business
                                              :base-url "http://localhost:4141/v1"
                                              :models [{:name "claude-opus-4-6"}]})]
        (expect (= "tok" (:api-key provider)))
        (expect (= "http://localhost:4141/v1" (:base-url provider))))))

  (it "marks Z.ai GLM thinking models as Z.ai reasoning-capable"
    (let [provider (config/->svar-provider {:id :zai
                                            :api-key "configured-token"
                                            :models [{:name "glm-4.7"}
                                                     {:name "minimax-m2.7:cloud"}]})
          [glm minimax] (:models provider)]
      (expect (= true (:reasoning? glm)))
      (expect (= :zai-thinking (:reasoning-style glm)))
      (expect (= false (:reasoning-effort? glm)))
      (expect (nil? (:reasoning? minimax)))
      (expect (nil? (:reasoning-style minimax)))))

  (it "marks Z.ai Coding Plan GLM-5 models as Z.ai reasoning-capable"
    (let [provider (config/->svar-provider {:id :zai-coding
                                            :api-key "configured-token"
                                            :models [{:name "glm-5-turbo"}]})
          [glm] (:models provider)]
      (expect (= true (:reasoning? glm)))
      (expect (= :zai-thinking (:reasoning-style glm)))
      (expect (= false (:reasoning-effort? glm))))))

(defdescribe local-provider-extension-registration-test
  (it "registers Ollama and LM Studio through provider extensions"
    (let [ollama   (registry/provider-by-id :ollama)
          lmstudio (registry/provider-by-id :lmstudio)]
      (expect (= :ollama (:provider/id ollama)))
      (expect (= :lmstudio (:provider/id lmstudio)))
      (expect (ifn? (:provider/status-fn ollama)))
      (expect (ifn? (:provider/status-fn lmstudio)))))

  (it "status fns return local status maps"
    (let [status ((:provider/status-fn (registry/provider-by-id :ollama)))]
      (expect (= false (:authenticated? status)))
      (expect (= :ollama (:provider-id status)))
      (expect (= :local (:source status)))
      (expect (= "http://localhost:11434/v1" (:base-url status))))))

(defdescribe model-name-test
  (it "extracts model names from strings and maps"
    (expect (= "gpt-5.1" (config/model-name "gpt-5.1")))
    (expect (= "gpt-5.1" (config/model-name {:name "gpt-5.1"})))
    (expect (nil? (config/model-name nil)))))
