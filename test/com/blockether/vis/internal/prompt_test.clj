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
  (it "sends core and extensions as separate messages before the user message"
    (let [active-extensions [{:ext/namespace 'test.env
                              :ext/prompt (fn [_] "<environment>\nhost facts\n</environment>")}
                             {:ext/namespace 'test.ext
                              :ext/prompt (fn [_] "extension rules")}]
          stable-messages (prompt/assemble-stable-prompt-messages
                            {}
                            {:system-prompt "caller addendum"
                             :active-extensions active-extensions})
          messages (prompt/assemble-initial-messages
                     {:stable-prompt-messages stable-messages
                      :initial-user-content "Do the thing."})]
      (expect (= ["system" "system" "user"]
                (role-seq messages)))
      (expect (= stable-messages (subvec messages 0 (count stable-messages))))
      (expect (every? #(= "system" (:role %)) stable-messages))
      (expect (contains-tag? (nth messages 0) "system_prompt"))
      (expect (contains-tag? (nth messages 1) "extensions"))
      (expect (str/includes? (get-in messages [1 :content]) "<extension id=\"test.env\">"))
      (expect (str/includes? (get-in messages [1 :content]) "<extension id=\"test.ext\">"))
      (expect (contains-tag? (nth messages 1) "environment"))
      (expect (str/includes? (get-in messages [1 :content]) "host facts"))
      (expect (str/includes? (get-in messages [1 :content]) "extension rules"))
      (expect (not (str/includes? (get-in messages [0 :content]) "host facts")))
      (expect (not (str/includes? (get-in messages [0 :content]) "extension rules")))
      (expect (not (some #(contains-tag? % "skills") stable-messages)))
      (expect (str/includes? (get-in messages [2 :content])
                "<current_user_message>\nDo the thing.\n</current_user_message>"))))

  (it "core system prompt is minimal: env/extension plumbing stays out, key directives stay in"
    ;; The core prompt is intentionally terse — just enough scaffolding
    ;; for the SCI loop. Environment, extensions, journal/bindings
    ;; framing, skills, system vars, and tool catalogs all live
    ;; OUTSIDE this string (as separate provider messages or
    ;; per-iteration trailer content). Only “load-bearing” directives
    ;; live here.
    (let [content (:content (first (prompt/assemble-stable-prompt-messages
                                     {}
                                     {:active-extensions []})))]
      ;; Identity + headline sections
      (expect (str/includes? content "λVis"))
      (expect (str/includes? content "ENV"))
      (expect (str/includes? content "TURN PROTOCOL"))
      (expect (str/includes? content "LOOP DISCIPLINE"))
      (expect (str/includes? content "EMIT_FINAL"))
      (expect (str/includes? content "ANSWER_IR"))

      ;; Canonical final form (the gate is hidden inside turn-answer!)
      (expect (str/includes? content "(turn-answer! <IR>)"))
      (expect (not (str/includes? content "(when (turn-converges?)")))
      (expect (not (str/includes? content "turn-converges?")))

      ;; The four structural gate criteria are surfaced to the model.
      (expect (str/includes? content "no error in the latest iteration"))
      (expect (str/includes? content "no action-tagged tool call"))
      (expect (str/includes? content "<journal> carries evidence for this turn"))
      (expect (str/includes? content "turn-answer! itself evaluated without throwing"))

      ;; Banned ops surface in the core prompt.
      (expect (str/includes? content "slurp"))
      (expect (str/includes? content "clojure.java.io"))
      (expect (str/includes? content "any filesystem access"))

      ;; ANSWER_IR shape stays callable / parseable from the prompt.
      (expect (str/includes? content "[:ir block*]"))
      (expect (str/includes? content ":h {:level 1-6}"))
      (expect (str/includes? content ":code {:lang string}"))

      ;; Plumbing kept OUT of the core string.
      (expect (not (str/includes? content "<environment>")))
      (expect (not (str/includes? content "<extensions>")))
      (expect (not (str/includes? content "<skills>")))
      (expect (not (str/includes? content "<active_skills>")))
      (expect (not (str/includes? content "load-skill")))
      (expect (not (str/includes? content "reload-skills")))
      (expect (not (str/includes? content "reload-extensions")))
      ;; Conversation-title prompting lives in the foundation nudge, not
      ;; in the always-on system prompt.
      (expect (not (str/includes? content "set-conversation-title!")))
      ;; System-vars catalogue and per-tool examples likewise stay out.
      (expect (not (str/includes? content "λSYSTEM_VARS.")))
      (expect (not (str/includes? content "TURN_ID")))
      (expect (not (str/includes? content "CONVERSATION_PREVIOUS_ANSWER")))
      (expect (not (str/includes? content "v/bash")))
      (expect (not (str/includes? content "tool-events")))
      (expect (not (str/includes? content "var-history")))
      (expect (not (str/includes? content "clojure.repl")))))

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
                       :stable-prompt-content "stable prompt"})]
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
        (expect (not (str/includes? content "provider rules")))
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
