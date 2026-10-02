(ns com.blockether.vis.contract.config-test
  (:require [com.blockether.vis.contract.config :as config]
            [com.blockether.vis.contract.document :as document]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe schema-root-validates-configuration
             (it "schema root validates configuration"
                 (expect (document/valid? "config" {"titling" {"mode" "llm"}}))
                 (expect (not (document/valid? "config" {"titling" {"mode" "unknown"}})))
                 (expect (not (document/valid? "config" {"api_style_values" ["openai"]})))))

(defdescribe schema-root-validates-the-gateway-pairing-address
             (it "schema root validates the gateway pairing address"
                 (expect (document/valid? "config" {"gateway" {"advertise" "10.0.0.5"}}))
                 (expect (document/valid? "config"
                                          {"gateway" {"advertise" "https://gateway.example.com"}}))
                 (expect (not (document/valid? "config" {"gateway" {"advertise" "   "}})))
                 (expect (not (document/valid? "config" {"gateway" {"url" "http://10.0.0.5"}})))))

(defdescribe api-style-validation-and-normalization-share-the-schema
             (it "api style validation and normalization share the schema"
                 (expect (= ["anthropic" "openai" "openai-responses" "gemini"]
                            config/api-style-values))
                 (doseq [[runtime aliases]
                         {"anthropic" ["anthropic" "anthropic-messages" "anthropic_messages"
                                       "claude" "messages"]
                          "openai-compatible-chat"
                          ["openai" "openai-chat" "openai_chat" "openai-compatible"
                           "openai_compatible" "openai-compatible-chat" "openai_compatible_chat"
                           "chat" "chat-completions" "chat_completions"]
                          "openai-compatible-responses" ["openai-responses" "openai_responses"
                                                         "openai-compatible-responses"
                                                         "openai_compatible_responses" "responses"]
                          "gemini" ["gemini" "google" "google-gemini" "google_gemini"]}

                         alias
                         aliases]

                   (expect (= runtime (get config/api-style-aliases alias)) alias)
                   (expect (config/definition-valid? "apiStyle" alias) alias))
                 (expect (not (config/definition-valid? "apiStyle" "unknown")))))

(defdescribe
  selectors-derive-from-payload-constraints
  (it "selectors derive from payload constraints"
      (let [schema (document/schema-document "config")]
        (expect (= (set (get-in schema ["$defs" "workspaceEntry" "properties" "access" "enum"]))
                   config/workspace-access-values))
        (expect (= (set (get-in schema ["$defs" "workspaceEntry" "properties" "draft" "enum"]))
                   config/workspace-draft-values))
        (expect (= (set (get-in schema ["$defs" "workspaceOs" "enum"])) config/workspace-os-values))
        (doseq [os config/workspace-os-values]
          (expect (config/definition-valid? "workspaceWhen" {"os" os}))
          (expect (config/definition-valid? "workspaceWhen" {"os" [os]})))
        (expect (not (config/definition-valid? "workspaceWhen" {"os" ["unknown"]})))
        (expect (= (set (keys (get-in schema ["$defs" "jail" "properties"])))
                   (config/definition-property-names "jail"))))))

(defdescribe access-alias-contract-test
             (it "keeps access aliases and canonical choices in the configuration schema"
                 (doseq [definition ["workspaceEntry" "networkRule"]]
                   (let [property (get-in (document/schema-document "config")
                                          ["$defs" definition "properties" "access"])
                         accepted (set (get property "enum"))
                         aliases (get property "x-vis-enum-aliases")]

                     (expect (seq aliases))
                     (expect (every? accepted (keys aliases)))
                     (expect (every? accepted (vals aliases)))
                     (expect (not-any? aliases (vals aliases)))))))
