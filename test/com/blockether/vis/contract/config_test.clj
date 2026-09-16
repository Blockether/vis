(ns com.blockether.vis.contract.config-test
  (:require [com.blockether.vis.contract.config :as config]
            [com.blockether.vis.contract.document :as document]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is testing]]))

(deftest schema-root-validates-configuration
  (is (document/valid? "config" {"titling" {"mode" "llm"}}))
  (is (not (document/valid? "config" {"titling" {"mode" "unknown"}})))
  (is (not (document/valid? "config" {"api_style_values" ["openai"]}))))

(deftest api-style-validation-and-normalization-share-the-schema
  (is (= ["anthropic" "openai" "openai-responses" "gemini"] config/api-style-values))
  (doseq [[runtime aliases]
          {"anthropic" ["anthropic" "anthropic-messages" "anthropic_messages" "claude" "messages"]
           "openai-compatible-chat" ["openai" "openai-chat" "openai_chat" "openai-compatible"
                                     "openai_compatible" "openai-compatible-chat"
                                     "openai_compatible_chat" "chat" "chat-completions"
                                     "chat_completions"]
           "openai-compatible-responses" ["openai-responses" "openai_responses"
                                          "openai-compatible-responses"
                                          "openai_compatible_responses" "responses"]
           "gemini" ["gemini" "google" "google-gemini" "google_gemini"]}

          alias
          aliases]

    (testing alias
      (is (= runtime (get config/api-style-aliases alias)))
      (is (config/definition-valid? "apiStyle" alias))))
  (is (not (config/definition-valid? "apiStyle" "unknown"))))

(deftest selectors-derive-from-payload-constraints
  (let [schema (document/schema-document "config")]
    (is (= (set (get-in schema ["$defs" "workspaceEntry" "properties" "access" "enum"]))
           config/workspace-access-values))
    (is (= (set (get-in schema ["$defs" "workspaceEntry" "properties" "draft" "enum"]))
           config/workspace-draft-values))
    (is (= (set (get-in schema ["$defs" "workspaceOs" "enum"])) config/workspace-os-values))
    (doseq [os config/workspace-os-values]
      (is (config/definition-valid? "workspaceWhen" {"os" os}))
      (is (config/definition-valid? "workspaceWhen" {"os" [os]})))
    (is (not (config/definition-valid? "workspaceWhen" {"os" ["unknown"]})))
    (is (= (set (keys (get-in schema ["$defs" "jail" "properties"])))
           (config/definition-property-names "jail")))))
