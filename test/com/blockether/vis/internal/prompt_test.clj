(ns com.blockether.vis.internal.prompt-test
  (:require
   [clojure.string :as str]
   [com.blockether.vis.internal.prompt :as prompt]
   [com.blockether.vis.internal.skills :as skills]
   [lazytest.core :refer [defdescribe expect it]]))

(defdescribe context-budget-headroom-test
  (it "replay fraction is documented and resolves to 30k under the 200k cap"
    (expect (= 0.15 prompt/PRESERVED_THINKING_REPLAY_FRACTION))
    (expect (= 30000 (long (* prompt/PRESERVED_THINKING_REPLAY_FRACTION
                             prompt/MAX_ITERATION_CONTEXT_TOKENS)))))

  (it "fixed budget allocations + output reserve fit inside MAX_ITERATION_CONTEXT_TOKENS"
    ;; Defensive: if anyone bumps MAX_JOURNAL_TOKENS, MAX_BINDINGS_TOKENS,
    ;; or PRESERVED_THINKING_REPLAY_FRACTION without checking the others,
    ;; this test catches the overflow before production. The output reserve
    ;; matches Anthropic's recommended ~16k floor; the system + turn-overhead
    ;; estimates are upper bounds on the cached-system / per-iter trailer.
    (let [output-reserve     16000
          system-prompt-est  10000
          turn-overhead-est  15000
          replay-budget      (long (* prompt/PRESERVED_THINKING_REPLAY_FRACTION
                                     prompt/MAX_ITERATION_CONTEXT_TOKENS))
          fixed-allocations  (+ (long prompt/MAX_JOURNAL_TOKENS)
                               (long prompt/MAX_BINDINGS_TOKENS)
                               replay-budget
                               output-reserve
                               system-prompt-est
                               turn-overhead-est)
          headroom           (- (long prompt/MAX_ITERATION_CONTEXT_TOKENS)
                               fixed-allocations)]
      ;; Headroom must be positive: total fixed allocations leave room for
      ;; the model's actual reasoning + tool calls + answer payload.
      (expect (pos? headroom)
        (str "Fixed budget allocations exceed MAX_ITERATION_CONTEXT_TOKENS. "
          "journal=" prompt/MAX_JOURNAL_TOKENS
          " bindings=" prompt/MAX_BINDINGS_TOKENS
          " replay=" replay-budget
          " output_reserve=" output-reserve
          " system_prompt_est=" system-prompt-est
          " turn_overhead_est=" turn-overhead-est
          " total=" fixed-allocations
          " cap=" prompt/MAX_ITERATION_CONTEXT_TOKENS
          " → negative headroom: " headroom))
      ;; Sanity floor: at least 25k of breathing room. If this trips, the
      ;; model is being squeezed and we should re-evaluate the fractions.
      (expect (<= 25000 headroom)
        (str "Headroom (" headroom ") under 25k floor; allocations too tight.")))))

(defdescribe iteration-context-cap-test
  (it "is a uniform 200k-token ceiling across every provider"
    ;; Hard cap, NOT provider-derived. We treat 8k Haiku, 200k Sonnet,
    ;; and 1M Gemini as 200000-token windows from Vis' working-memory
    ;; perspective. Smaller-window models still get their native size
    ;; (the cap only TRIMS windows above 200k); larger-window models
    ;; get clamped down to 200k. See the docstring on
    ;; MAX_ITERATION_CONTEXT_TOKENS for rationale.
    (expect (= 200000 prompt/MAX_ITERATION_CONTEXT_TOKENS)))

  (it "effective-context-limit clamps a 1M advertised window down to 200k"
    (expect (= 200000 (#'prompt/effective-context-limit "any-model" 1000000))))

  (it "effective-context-limit leaves an 8k window at its native size"
    (expect (= 8000 (#'prompt/effective-context-limit "any-model" 8000))))

  (it "effective-context-limit accepts a context-limit of exactly 200k"
    (expect (= 200000 (#'prompt/effective-context-limit "any-model" 200000))))

  (it "effective-context-limit never returns less than 1"
    (expect (= 1 (#'prompt/effective-context-limit "any-model" 0)))
    (expect (= 1 (#'prompt/effective-context-limit "any-model" -42)))))

(defdescribe core-system-prompt-test
  (it "stays ultra-minimal and code-only"
    (let [p (prompt/build-system-prompt {})]
      (expect (str/includes? p "Reply only with ```clojure``` code fences"))
      (expect (str/includes? p "Answer IR"))
      (expect (str/includes? p "OODA:"))
      (expect (str/includes? p "TURN:"))
      (expect (str/includes? p "ITERATIONS:"))
      (expect (str/includes? p "JOURNAL:"))
      (expect (str/includes? p "IR FORMS:"))
      (expect (str/includes? p "ANSWER:"))
      (expect (str/includes? p "SURFACES:"))
      (expect (str/includes? p "CODE:"))
      (expect (not (str/includes? p "GROUND RULE")))
      (expect (not (str/includes? p "DEFS RENDER")))
      (expect (not (str/includes? p "BATCHING")))))

  (it "keeps dynamic engine values out of the cached system prompt"
    (let [p (prompt/build-system-prompt {})]
      (expect (str/includes? p "Dynamic engine state is supplied in user-role <current_turn_context>"))
      (expect (str/includes? p "Dynamic ids/positions never appear as changing values"))
      (expect (str/includes? p "engine_turn_id"))
      (expect (str/includes? p "engine_iteration_position"))
      (doseq [legacy ["<system_vars>"
                      "TURN_ITERATION_ID is the last persisted iteration UUID"
                      "{{SYSTEM_VARS_REGISTRY}}"]]
        (expect (not (str/includes? p legacy))))))

  (it "explains turns, iterations, journal, and answer timing"
    (let [p (prompt/build-system-prompt {})]
      (expect (str/includes? p "One user request = one turn"))
      (expect (str/includes? p "One model reply + engine eval = one iteration"))
      (expect (str/includes? p "current_engine_iteration_id is the logical id"))
      (expect (str/includes? p "<journal> row labels iN are 1-based"))
      (expect (str/includes? p "iN.K.M = tool event inside iN.K"))
      (expect (str/includes? p "read :hint first"))
      (expect (str/includes? p "After mutation, verify in a later iteration"))
      (expect (str/includes? p "Final answer iteration is only"))
      (expect (str/includes? p "runtime > source > docs > assumption"))))

  (it "documents core Answer IR forms and examples"
    (let [p (prompt/build-system-prompt {})]
      (expect (str/includes? p "Root: [:ir block*]"))
      (expect (str/includes? p "Blocks: :p :h{:level 1-6} :code{:lang}"))
      (expect (str/includes? p ":details{:open?} :summary"))
      (expect (str/includes? p "Inlines: :span{:preserve-ws? :nowrap?} :br"))
      (expect (str/includes? p ":img{:src :alt} :kbd :mark :sup :sub"))
      (expect (str/includes? p "Raw text tags preserving whitespace: :code, :c, :kbd"))
      (expect (str/includes? p "(answer [:ir [:p \"Done.\"]])"))
      (expect (str/includes? p "(answer [:ir [:details {:open? false}"))
      (expect (str/includes? p "Build this data directly; do not generate Markdown and convert it")))))

(defdescribe initial-messages-test
  (it "renders previous-turn context without current-objective pinning"
    (let [messages (prompt/assemble-initial-messages
                     {:system-prompt "SYS"
                      :initial-user-content "do it"
                      :previous-turn-context {:user-request "Patch TUI header spacing"
                                              :answer "I can do that."}})
          user-msg (some #(when (= "user" (:role %)) %) messages)
          content (:content user-msg)]
      (expect (str/includes? content "<previous_turn_context>"))
      (expect (str/includes? content "Patch TUI header spacing"))
      (expect (str/includes? content "<user_turn_request_main_goal>"))
      (expect (not (str/includes? content "<current_objective>"))))))

(defdescribe current-turn-context-rendering-test
  (it "embeds dynamic engine ids and positions in the per-iteration user trailer"
    (let [environment {:conversation-id "conversation-123"
                       :conversation-title-atom (atom "Template vars")
                       :current-conversation-turn-id-atom (atom "turn-12345678")
                       :current-turn-position-atom (atom 7)
                       :current-iteration-id-atom (atom "iter-previous")}
          out (prompt/build-iteration-context
                environment
                {:active-extensions []
                 :model "test-model"
                 :context-limit 4096
                 :iteration 2
                 :max-iterations 20
                 :current-user-content "check vars"})]
      (expect (str/includes? out "<current_turn_context>"))
      (expect (str/includes? out "engine_state_machine: idle -> receive_user_turn"))
      (expect (str/includes? out "engine_turn_id: turn-12345678"))
      (expect (str/includes? out "engine_turn_position: 7"))
      (expect (str/includes? out "current_engine_iteration_id: turn/turn-123/iteration/3"))
      (expect (str/includes? out "engine_iteration_position: 3"))
      (expect (str/includes? out "engine_iteration_max: 20"))
      (expect (str/includes? out "previous_persisted_iteration_id: iter-previous"))
      (expect (str/includes? out "previous_persisted_iteration_position: 2"))
      (expect (not (str/includes? out "<system_vars>")))
      (expect (not (str/includes? out "<system_var"))))))

(defdescribe hook-nudge-rendering-test
  (it "renders a current engine nudge from the canonical namespaced iteration-start phase"
    (let [out (prompt/build-iteration-context
                {:conversation-title-atom (atom "Hook phases")}
                {:active-extensions [{:ext/namespace 'test.hook-phases
                                      :ext/hooks [{:id :test/nudge
                                                   :doc "Fixture nudge."
                                                   :phase :turn.iteration/start
                                                   :fn (fn [{:keys [phase]}]
                                                         {:importance :high
                                                          :hint (str "phase=" phase)})}]}]
                 :model "test-model"
                 :context-limit 4096
                 :iteration 0
                 :current-user-content "debug this"})]
      (expect (str/includes? out "<current_engine_start_nudges>"))
      (expect (str/includes? out "<current_engine_start_nudge importance=\"high\">"))
      (expect (str/includes? out "phase=:turn.iteration/start"))))

  (it "renders turn-start and session-start nudges only at their boundaries"
    (let [mk-ext (fn [hits]
                   {:ext/namespace 'test.hook-boundaries
                    :ext/hooks [{:id :test/session
                                 :doc "Session boundary."
                                 :phase :session/start
                                 :fn (fn [_] (swap! hits conj :session/start) {:hint "session"})}
                                {:id :test/turn
                                 :doc "Turn boundary."
                                 :phase :turn/start
                                 :fn (fn [_] (swap! hits conj :turn/start) {:hint "turn"})}]})
          first-hits (atom [])
          later-hits (atom [])
          first-out (prompt/build-iteration-context
                      {:conversation-title-atom (atom "Hook boundaries")
                       :current-turn-position-atom (atom 1)}
                      {:active-extensions [(mk-ext first-hits)]
                       :model "test-model"
                       :context-limit 4096
                       :iteration 0})
          later-out (prompt/build-iteration-context
                      {:conversation-title-atom (atom "Hook boundaries")
                       :current-turn-position-atom (atom 1)}
                      {:active-extensions [(mk-ext later-hits)]
                       :model "test-model"
                       :context-limit 4096
                       :iteration 1})]
      (expect (= [:session/start :turn/start] @first-hits))
      (expect (str/includes? first-out "session"))
      (expect (str/includes? first-out "turn"))
      (expect (= [] @later-hits))
      (expect (str/includes? later-out "<current_turn_context>"))
      (expect (not (str/includes? later-out "session")))
      (expect (not (str/includes? later-out "turn</current_engine_start_nudge>"))))))

(defdescribe journal-rendering-test
  (it "renders every block in <journal> (no silent elision)"
    ;; Regression: previously blocks tagged `:rendering-kind :vis/silent`
    ;; were filtered out of the journal so the next iteration's prompt
    ;; lost track of where bindings came from. The whole silent
    ;; mechanism has been removed - every block round-trips into the
    ;; journal exactly as executed.
    (let [out (prompt/build-iteration-context
                {:conversation-title-atom (atom "Preview diagnosis")}
                {:active-extensions []
                 :model "test-model"
                 :context-limit 4096
                 :iteration 1
                 :blocks-by-iteration [[1 {:blocks [{:code "(def xd \"XDDD\")"
                                                     :result "XDDD"
                                                     :role :nudge}
                                                    {:code "(+ 1 1)"
                                                     :result 2}]}]]})]
      (expect (str/includes? out "<journal>"))
      (expect (str/includes? out "(def xd"))
      (expect (str/includes? out "XDDD"))
      (expect (str/includes? out "(+ 1 1) -> 2")))))

(defdescribe lifted-sink-failure-test
  (it "lifts a tool-envelope :success? false sink-entry into the block header (regression: convo 73f3d325)"
    ;; Reduced repro: a (z/patch ...) returned a tool-result envelope
    ;; with :success? false. The block-level :error was nil so the
    ;; previous header rendered the parent envelope as a tidy summary,
    ;; and the failure detail lived inside the journal sink-entry.
    ;; The model wrote (answer \"Now fixed\") in the same iteration
    ;; without ever observing the failure. Now the failed sink lifts
    ;; into the header `value-part` so the next-iter prompt cannot
    ;; miss it.
    (let [out (prompt/build-iteration-context
                {:conversation-title-atom (atom "Lifted-sink regression")}
                {:active-extensions   []
                 :model               "test-model"
                 :context-limit       4096
                 :iteration           1
                 :blocks-by-iteration [[1 {:blocks [{:code    "(z/patch [{:path \"x.clj\" :search \"y\" :replace \"z\"}])"
                                                     :result  nil
                                                     :error   nil
                                                     :journal [{:position  0
                                                                :form      "(z/patch [{:path \"x.clj\" :search \"y\" :replace \"z\"}])"
                                                                :success?  false
                                                                :result    nil
                                                                ;; PLAN §2.1 + §7.3.5: structured :op/error map.
                                                                :error     {:message "z/patch :search locator must match exactly once; matched 4 time(s)"}}]}]}]]})]
      (expect (str/includes? out "ERROR"))
      (expect (str/includes? out "matched 4 time(s)")))))

(defdescribe full-system-prompt-assembly-test
  (it "includes <skills> block when skills are available"
    (with-redefs [skills/list-all (fn [] [{:name "diagnose"
                                           :source :repo
                                           :description "Debug loop."}])]
      (let [out (prompt/assemble-system-prompt {}
                  {:active-extensions []
                   :system-prompt nil})]
        (expect (str/includes? out "<system_prompt>"))
        (expect (str/includes? out "</system_prompt>"))
        (expect (str/includes? out "<skills>"))
        (expect (str/includes? out "diagnose: Debug loop."))
        (expect (str/includes? out "</skills>")))))

  (it "omits <skills> when catalog is empty"
    (with-redefs [skills/list-all (fn [] [])]
      (let [out (prompt/assemble-system-prompt {}
                  {:active-extensions []
                   :system-prompt nil})]
        (expect (str/includes? out "<system_prompt>"))
        (expect (not (str/includes? out "<skills>\n"))))))

  (it "no extensions = no <extensions>/<environment-info> blocks"
    (with-redefs [skills/list-all (fn [] [])]
      (let [out (prompt/assemble-system-prompt {}
                  {:active-extensions []
                   :system-prompt nil})]
        (expect (not (str/includes? out "<extensions>\n")))
        (expect (not (str/includes? out "<environment-info>\n")))
        (expect (not (str/includes? out "<llm_model_prompt>\n")))
        (expect (not (str/includes? out "<specific_provider_model_prompt"))))))

  (it "includes <extensions> when an active extension provides :ext/prompt"
    (with-redefs [skills/list-all (fn [] [])]
      (let [fake-ext {:ext/namespace 'com.test.ext
                      :ext/prompt    (constantly "Use test/ prefix for all test fns.")}
            out (prompt/assemble-system-prompt {}
                  {:active-extensions [fake-ext]
                   :system-prompt nil})]
        (expect (str/includes? out "<extensions>"))
        (expect (str/includes? out "Use test/ prefix for all test fns."))
        (expect (str/includes? out "</extensions>")))))

  (it "includes <environment-info> when an active extension provides :ext/environment-info-fn"
    (with-redefs [skills/list-all (fn [] [])]
      (let [fake-ext {:ext/namespace 'com.test.env
                      :ext/environment-info-fn (constantly "OS: macOS 15.1\nCWD: /home/user")}
            out (prompt/assemble-system-prompt {}
                  {:active-extensions [fake-ext]
                   :system-prompt nil})]
        (expect (str/includes? out "<environment-info>"))
        (expect (str/includes? out "OS: macOS 15.1"))
        (expect (str/includes? out "</environment-info>"))))))
