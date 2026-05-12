(ns com.blockether.vis.internal.prompt-test
  (:require [clojure.string :as str]
            [com.blockether.svar.internal.llm :as svar-llm]
            [com.blockether.vis.internal.env :as env]
            [com.blockether.vis.internal.prompt :as prompt]
            [lazytest.core :refer [defdescribe expect it]]))

(defn- role-seq
  [messages]
  (mapv :role messages))

(defn- contains-tag?
  [message tag]
  (str/includes? (:content message) (str "<" tag)))

(defdescribe prompt-test
  (it "assembles stable prompt fragments as many system messages before the user message"
    (let [active-extensions [{:ext/namespace 'test.env
                              :ext/environment-prompt-fn (fn [_] "host facts")}
                             {:ext/namespace 'test.ext
                              :ext/prompt (fn [_] "extension rules")}]
          stable-messages (prompt/assemble-stable-prompt-messages
                            {}
                            {:system-prompt "caller addendum"
                             :active-extensions active-extensions})
          messages (prompt/assemble-initial-messages
                     {:stable-prompt-messages stable-messages
                      :initial-user-content "Do the thing."})]
      (expect (= ["system" "system" "system" "user"]
                (role-seq messages)))
      (expect (= stable-messages (subvec messages 0 (count stable-messages))))
      (expect (every? #(= "system" (:role %)) stable-messages))
      (expect (contains-tag? (nth messages 0) "system_prompt"))
      (expect (contains-tag? (nth messages 1) "environment"))
      (expect (contains-tag? (nth messages 2) "extensions"))
      (expect (not (some #(contains-tag? % "skills") stable-messages)))
      (expect (str/includes? (get-in messages [3 :content])
                "<user_turn_request_main_goal>\nDo the thing.\n</user_turn_request_main_goal>"))))

  (it "core system prompt splits static and dynamic context truthfully"
    (let [content (:content (first (prompt/assemble-stable-prompt-messages
                                     {}
                                     {:active-extensions []})))]
      (expect (str/includes? content "SCI-based recursive model runtime"))
      (expect (str/includes? content "reply := ```clojure``` fences only"))
      (expect (str/includes? content "Host evals forms, records evidence"))
      (expect (str/includes? content "TURN := USER_GOAL × WORK(ITERATIONS)* × FINAL"))
      (expect (not (str/includes? content "Host loop: assistant emits executable SCI forms")))
      (expect (str/includes? content "λSTATIC_CONTEXT."))
      (expect (str/includes? content "system-role stable prefix, built once at turn start"))
      (expect (str/includes? content "<environment>   := turn-start host/project facts from active extension callbacks."))
      (expect (str/includes? content "λDYNAMIC_CONTEXT."))
      (expect (str/includes? content "<journal>                     := token-budgeted iteration evidence; newest at bottom; may carry prior conversation iterations."))
      (expect (str/includes? content "<bindings>                    := live SCI user-var index; excludes SYSTEM vars and tool/helper bindings."))
      (expect (str/includes? content "direct turn/conversation values"))
      (expect (str/includes? content "FINAL forbids stateful mutation"))
      (expect (str/includes? content "(turn-answer! ir)"))
      (expect (str/includes? content "(set-conversation-title! s)"))
      (expect (str/includes? content "v/engine-symbol-documentation"))
      (expect (str/includes? content "v/engine-symbol-source-code"))
      (expect (str/includes? content "v/engine-symbol-metadata"))
      (expect (str/includes? content "v/engine-symbol-apropos"))
      (expect (not (str/includes? content "clojure.repl")))
      (expect (not (str/includes? content "repl/doc")))
      (expect (str/includes? content "required_evidence_observed?"))
      (expect (str/includes? content "decide from <journal> + <bindings>, not memory"))
      (expect (str/includes? content "false guard"))
      (expect (str/includes? content "(when condition (turn-answer! [:ir ...]))"))
      (expect (str/includes? content "<llm_model_prompt>"))
      (expect (not (str/includes? content "<llm_model_prompt>   := provider/model-specific prompt fragment.")))
      (expect (not (str/includes? content "v/bash")))
      (expect (not (str/includes? content "(v/cat \"src/foo.clj\" :from 0 :to 1000)")))
      (expect (not (str/includes? content "tool-events")))
      (expect (not (str/includes? content "λSYSTEM_VARS.")))
      (expect (not (str/includes? content "read_only :=")))
      (expect (not (str/includes? content "TURN_ID")))
      (expect (not (str/includes? content "CONVERSATION_PREVIOUS_ANSWER")))
      (expect (not (str/includes? content "load-skill")))
      (expect (not (str/includes? content "reload-skills")))
      (expect (not (str/includes? content "<skills>")))
      (expect (not (str/includes? content "<active_skills>")))
      (expect (not (str/includes? content "(answer")))
      (expect (not (str/includes? content "(conversation-title")))
      (expect (not (str/includes? content "cached host/project facts")))
      (expect (not (str/includes? content "NO ERRORS IN PREVIOUS ITERATION")))
      (expect (not (str/includes? content "reload-extensions")))))

  (it "current turn context renders dynamic telemetry and direct runtime values without static lifecycle policy"
    (let [turn-id #uuid "11111111-1111-1111-1111-111111111111"
          prev-id #uuid "22222222-2222-2222-2222-222222222222"
          sci-state (env/create-sci-context {})
          environment (merge sci-state
                        {:conversation-id #uuid "00000000-0000-0000-0000-000000000000"
                         :current-conversation-turn-id-atom (atom turn-id)
                         :current-turn-position-atom (atom 7)
                         :current-iteration-id-atom (atom prev-id)
                         :conversation-title-atom (atom "Prompt stack cleanup")})]
      (env/bind-and-bump! environment 'CONVERSATION_STATE_ID
        #uuid "33333333-3333-3333-3333-333333333333")
      (env/bind-and-bump! environment 'CONVERSATION_PREVIOUS_ANSWER
        "Previous final answer.")
      (let [content (prompt/build-iteration-context
                      environment
                      {:active-extensions [{:ext/namespace 'test.ext
                                            :ext/alias {:alias 't}
                                            :ext/doc "Test extension."
                                            :ext/symbols [{:ext.symbol/symbol 'demo}]}]
                       :iteration 2
                       :model "unknown-model"
                       :current-user-content "Do the thing."
                       :stable-prompt-content "stable prompt"
                       :provider-prompt-context
                       {:provider {:id :demo-provider}
                        :descriptor {:provider/prompt-fn (fn [_] "provider rules")}}})]
        (expect (str/includes? content "<current_turn_context>"))
        (expect (str/includes? content "engine_state: :turn.iteration/start"))
        (expect (str/includes? content "engine_phase: :model_think"))
        (expect (str/includes? content "engine_turn_position: 7"))
        (expect (str/includes? content "engine_iteration_position: 3"))
        (expect (str/includes? content "previous_persisted_iteration_id: 22222222-2222-2222-2222-222222222222"))
        (expect (str/includes? content "turn_id: #uuid \"11111111-1111-1111-1111-111111111111\""))
        (expect (str/includes? content "turn_position: 7"))
        (expect (str/includes? content "turn_conversation_state_id: #uuid \"33333333-3333-3333-3333-333333333333\""))
        (expect (str/includes? content "turn_system_prompt: \"stable prompt\""))
        (expect (str/includes? content "turn_active_extensions:"))
        (expect (not (str/includes? content "turn_accessible_skills:")))
        (expect (str/includes? content "turn_iteration_id: #uuid \"22222222-2222-2222-2222-222222222222\""))
        (expect (str/includes? content "turn_iteration_position: 2"))
        (expect (str/includes? content "conversation_state_id: #uuid \"33333333-3333-3333-3333-333333333333\""))
        (expect (str/includes? content "conversation_title: \"Prompt stack cleanup\""))
        (expect (str/includes? content "conversation_previous_answer: \"Previous final answer.\""))
        (expect (str/includes? content "<llm_model_prompt>\nprovider rules\n</llm_model_prompt>"))
        (expect (not (str/includes? content "TURN_ID")))
        (expect (not (str/includes? content "CONVERSATION_PREVIOUS_ANSWER")))
        (expect (not (str/includes? content "engine_state_machine")))
        (expect (not (str/includes? content "idle -> receive_user_turn"))))))

  (it "provider serializers preserve many system prompts before user content"
    (let [messages [{:role "system" :content "core rules"}
                    {:role "system" :content "extension rules"}
                    {:role "system" :content "provider rules"}
                    {:role "user" :content "Do the thing."}]
          chat-body (#'svar-llm/build-request-body messages "demo-model" nil)
          responses-body (#'svar-llm/build-openai-responses-request-body messages "demo-model" nil)
          anthropic-body (#'svar-llm/build-anthropic-request-body messages "demo-model" nil)]
      ;; OpenAI-compatible chat providers (OpenAI, local OpenAI-compatible,
      ;; GitHub Copilot, Z.ai) accept the message vector directly.
      (expect (= ["system" "system" "system" "user"]
                (role-seq (:messages chat-body))))
      (expect (= ["core rules" "extension rules" "provider rules" "Do the thing."]
                (mapv :content (:messages chat-body))))

      ;; OpenAI Responses has one instructions field, so svar joins system
      ;; messages in order and leaves only non-system turns in input.
      (expect (= "core rules\n\nextension rules\n\nprovider rules"
                (:instructions responses-body)))
      (expect (= ["user"] (role-seq (:input responses-body))))
      (expect (= "Do the thing." (get-in responses-body [:input 0 :content 0 :text])))

      ;; Anthropic Messages has top-level :system, so svar joins system
      ;; messages in order and removes system-role entries from :messages.
      (expect (= "core rules\nextension rules\nprovider rules"
                (:system anthropic-body)))
      (expect (= ["user"] (role-seq (:messages anthropic-body))))
      (expect (= "Do the thing." (get-in anthropic-body [:messages 0 :content]))))))
