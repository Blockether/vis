(ns com.blockether.vis.internal.native-tool-provider-contract-test
  (:require [charred.api :as json]
            [com.blockether.svar.internal.llm :as svar-llm]
            [com.blockether.vis.internal.extension :as extension]
            [com.blockether.vis.internal.loop :as lp]
            [lazytest.core :refer [defdescribe expect it]]))

(def ^:private build-anthropic @#'svar-llm/build-anthropic-request-body)
(def ^:private build-openai-chat @#'svar-llm/build-request-body)
(def ^:private build-openai-responses @#'svar-llm/build-openai-responses-request-body)
(def ^:private build-gemini @#'svar-llm/build-gemini-request-body)

(def ^:private schema-key
  {:anthropic :input_schema
   :openai-chat :parameters
   :openai-responses :parameters
   :gemini :parameters})

(def ^:private expected-tool-choice
  {:anthropic {:type "tool" :name "grep"}
   :openai-chat {:type "function" :function {:name "grep"}}
   :openai-responses {:type "function" :name "grep"}
   :gemini {:functionCallingConfig {:mode "ANY" :allowedFunctionNames ["grep"]}}})

(defn- provider-bodies
  [tools]
  (let
    [messages
     [{:role "user" :content "provider schema smoke"}]

     opts
     {:svar/tools tools :svar/tool-choice {:name "grep"}}]

    {:anthropic (build-anthropic messages "claude-smoke" opts)
     :openai-chat (build-openai-chat messages "chat-smoke" opts)
     :openai-responses (build-openai-responses messages "responses-smoke" opts)
     :gemini (build-gemini messages "gemini-smoke" opts)}))

(defn- provider-tools
  [style body]
  (case style
    :openai-chat
    (mapv :function (:tools body))

    :gemini
    (get-in body [:tools 0 :functionDeclarations])

    (:tools body)))

(defn- provider-tool-choice
  [style body]
  (if (= :gemini style) (:toolConfig body) (:tool_choice body)))

(defn- contains-nested-one-of?
  [schema]
  (boolean (some #(and (map? %) (contains? % :oneOf) (not (identical? % schema)))
                 (tree-seq coll? seq schema))))

(defdescribe native-tool-provider-contract-test
             (it
               "serializes every real native tool into every provider wire without root unions"
               (extension/discover-extensions!)
               (let
                 [tools
                  (@#'lp/native-tools (extension/registered-extensions) nil nil)

                  canonical
                  (into {} (map (juxt :name :schema)) tools)

                  bodies
                  (provider-bodies tools)]

                 (expect (>= (count tools) 20))
                 (expect (= (count tools) (count canonical)))
                 (expect (contains? canonical "grep"))
                 (expect (every? #(= "object" (get-in % [:schema :type])) tools))
                 (expect (every? #(empty? (select-keys (:schema %) [:oneOf :anyOf :allOf])) tools))
                 (expect (contains-nested-one-of? (get canonical "grep")))
                 (doseq [[style body] bodies]
                   (let
                     [wire-tools (provider-tools style body)
                      by-name (into {} (map (juxt :name identity)) wire-tools)
                      wire-schema-key (get schema-key style)]

                     (expect (= (set (keys canonical)) (set (keys by-name))))
                     (expect (= (count tools) (count wire-tools)))
                     (expect (= (get expected-tool-choice style) (provider-tool-choice style body)))
                     (expect (map? (json/read-json (json/write-json-str body) :key-fn keyword)))
                     (doseq [[tool-name schema] canonical]
                       (expect (= schema (get-in by-name [tool-name wire-schema-key])))))))))

(defn- provider-test-router
  []
  ((requiring-resolve 'com.blockether.svar.core/make-router)
    [{:id :native-tool-schema-smoke
      :api-key "test"
      :base-url "https://example.invalid/v1"
      :api-style :openai-compatible-responses
      :models [{:name "native-tool-schema-smoke"
                :tool-call? true
                :capabilities #{:chat}
                :context 100000
                :output-limit 4096}]}]))

(defdescribe
  native-tool-provider-callability-test
  (it
    "reaches the provider transport with every real native tool and receives a tool call"
    (extension/discover-extensions!)
    (let
      [tools
       (@#'lp/native-tools (extension/registered-extensions) nil nil)

       captured
       (atom nil)

       fake-response
       {:content nil
        :reasoning nil
        :provider-state {:provider :openai-responses}
        :tool-calls [{:id "call_smoke"
                      :name "grep"
                      :input {:query "native tool schema smoke" :paths ["src"]}}]
        :assistant-message {:role "assistant"
                            :content [{:type "tool_use"
                                       :id "call_smoke"
                                       :name "grep"
                                       :input {:query "native tool schema smoke" :paths ["src"]}}]}
        :api-usage {:input-tokens 3 :output-tokens 1 :total-tokens 4}
        :stream-finalization {:finish-reason "tool_calls"}}

       result
       (with-redefs
         [svar-llm/openai-responses-completion (fn [body _opts]
                                                 (reset! captured body)
                                                 fake-response)]
         (svar-llm/ask-code!* (provider-test-router)
                              {:messages [{:role "user" :content "call grep"}]
                               :tools tools
                               :tool-choice "grep"
                               :model "native-tool-schema-smoke"
                               :api-key "test"
                               :base-url "https://example.invalid/v1"
                               :api-style :openai-compatible-responses
                               :check-context? false}))]

      (expect (= :tool-calls (:stop-reason result)))
      (expect (= [{:id "call_smoke"
                   :name "grep"
                   :input {:query "native tool schema smoke" :paths ["src"]}}]
                 (:tool-calls result)))
      (expect (= (count tools) (count (:tools @captured))))
      (expect (= {:type "function" :name "grep"} (:tool_choice @captured)))
      (expect (every? #(= "object" (get-in % [:parameters :type])) (:tools @captured)))
      (expect (every? #(empty? (select-keys (:parameters %) [:oneOf :anyOf :allOf]))
                      (:tools @captured))))))
