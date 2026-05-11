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
  ;; Per PLAN.md §6.1: the prompt was rewritten to a ~60-line caveman
  ;; form centered on the OODA loop. These tests pin the new shape
  ;; against accidental regressions back to prose-heavy explanations.

  (it "is bounded at <= 90 lines"
    ;; PLAN §6.4 originally said 70; bumped to 90 after the
    ;; hallucination-guard + investigation-triggers section landed
    ;; (the failure-mode coverage was worth the 12 lines).
    (let [p prompt/CORE_SYSTEM_PROMPT
          n (count (str/split-lines p))]
      (expect (<= n 90)
        (str "prompt grew to " n " lines; cap is 90"))))

  (it "states the OUTPUT contract before LOOP"
    ;; Regression: conversation 185fbc4f had GLM-5.1 fabricate a
    ;; <journal> envelope and close it with a stray ``` that swallowed
    ;; the next real ```clojure opener. The output rule must come
    ;; first.
    (let [p prompt/CORE_SYSTEM_PROMPT
          fmt-idx (str/index-of p "OUTPUT:")
          loop-idx (str/index-of p "LOOP:")]
      (expect (some? fmt-idx))
      (expect (some? loop-idx))
      (expect (< (long fmt-idx) (long loop-idx)))
      (expect (str/includes? p "```clojure fences. Nothing outside"))
      (expect (str/includes? p ";; comments"))))

  (it "LOOP section names investigation triggers + hallucination guard"
    ;; Replaces the classify-first OODA strategy block (now retired).
    ;; The prompt no longer requires `(intent :X)` ceremony; it teaches
    ;; the two reply shapes (trivial chat vs work) and names the
    ;; investigation verbs that REQUIRE tool calls.
    (let [p prompt/CORE_SYSTEM_PROMPT]
      (expect (str/includes? p "trivial chat"))
      (expect (str/includes? p "Investigation triggers"))
      (expect (str/includes? p "HALLUCINATION GUARD"))
      (expect (str/includes? p "investigate"))
      (expect (str/includes? p "tool/symbol names like `z/patch-check`"))
      (expect (str/includes? p "planning-only / opinion-only / design-only"))
      (expect (str/includes? p "reproduce first"))
      ;; Old grill name must be gone (PLAN §4.5 architect rename).
      (expect (not (str/includes? p ":grill")))))

  (it "BINDINGS and SYSTEM VARS are sibling sections (not conflated)"
    (let [p prompt/CORE_SYSTEM_PROMPT
          bind-idx (str/index-of p "BINDINGS:")
          sys-idx  (str/index-of p "SYSTEM VARS:")]
      (expect (some? bind-idx))
      (expect (some? sys-idx))
      (expect (< (long bind-idx) (long sys-idx)))
      ;; BINDINGS still mentions escape hatches (PLAN §6.4 BINDINGS spec).
      (expect (str/includes? p "`*1`"))
      (expect (str/includes? p "`*e`"))
      (expect (str/includes? p "durable"))))

  (it "SYSTEM VARS section names hierarchy prefix + 11-var registry"
    (let [p prompt/CORE_SYSTEM_PROMPT]
      (expect (str/includes? p "CONVERSATION_*"))
      (expect (str/includes? p "TURN_*"))
      (expect (str/includes? p "TURN_ITERATION_*"))
      (expect (str/includes? p "hierarchy"))
      (expect (str/includes? p "11 names"))
      ;; Retired raw SOUL_ID variants must NOT appear AS VAR NAMES.
      ;; (Mentioning \"raw SOUL_IDs retired\" in the explanatory line
      ;; is fine — the model needs to know they're gone.)
      (expect (not (str/includes? p "CONVERSATION_SOUL_ID ")))
      (expect (not (str/includes? p "TURN_CONVERSATION_SOUL_ID ")))))

  (it "OPS section names the 2-value tag enum + structured error fields"
    (let [p prompt/CORE_SYSTEM_PROMPT]
      (expect (str/includes? p ":op.tag/observation"))
      (expect (str/includes? p ":op.tag/action"))
      (expect (str/includes? p "::op/tag"))
      (expect (str/includes? p "::op/success?"))
      (expect (str/includes? p "::op/error"))
      ;; Error structure named per PLAN §2.1 + §6.3.
      (expect (str/includes? p ":message"))
      (expect (str/includes? p ":hint"))
      (expect (str/includes? p ":trace"))
      (expect (str/includes? p ":block"))
      ;; PLAN §6.3: agent reads :hint first.
      (expect (str/includes? p "Read :hint first"))))

  (it "CODE section embeds the editing/aesthetics rules (abstract; tool-specifics live in extensions)"
    ;; Concrete tool choices (z/patch vs v/patch, HoneySQL vs raw
    ;; SQL) belong in the active extension `:ext/prompt` fragments,
    ;; not the core prompt.
    (let [p prompt/CORE_SYSTEM_PROMPT]
      (expect (str/includes? p "code > markdown"))
      (expect (str/includes? p "data > control_flow"))
      (expect (str/includes? p "pure > stateful"))
      (expect (str/includes? p "structural_editing > line_editing > raw_text"))
      (expect (str/includes? p "one change -> verify -> next"))
      ;; No tool-specific names in the core prompt.
      (expect (not (str/includes? p "z/patch > v/patch")))
      (expect (not (str/includes? p "HoneySQL > raw SQL")))))

  (it "closes with TRUTH precedence (runtime > source > docs > assumption)"
    (let [p prompt/CORE_SYSTEM_PROMPT]
      (expect (str/includes? p "TRUTH:"))
      (expect (str/includes? p "runtime > source > docs > assumption"))))

  (it "removed the prose-heavy GROUND RULE / DEFS RENDER sections (PLAN §6.2)"
    ;; If any of these survive, the prompt has regressed back to the
    ;; ~120-line prose form.
    (let [p prompt/CORE_SYSTEM_PROMPT]
      (expect (not (str/includes? p "GROUND RULE")))
      (expect (not (str/includes? p "DEFS RENDER")))
      (expect (not (str/includes? p "SILENT FORMS")))
      (expect (not (str/includes? p "TOP-LEVEL DEFS")))
      (expect (not (str/includes? p "BATCHING")))
      (expect (not (str/includes? p "DIAGNOSTIC OUTPUT")))
      (expect (not (str/includes? p "GENERATE one OR MORE")))
      (expect (not (str/includes? p "AUTOMATICALLY populates"))))))

(defdescribe hook-nudge-rendering-test
  (it "renders a system nudge from the canonical namespaced iteration-start phase"
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
      (expect (str/includes? out "<system_nudges>"))
      (expect (str/includes? out "<system_nudge importance=\"high\">"))
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
      (expect (nil? later-out)))))

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
        (expect (not (str/includes? out "<skills>"))))))

  (it "no extensions = no <extensions>/<environment-info> blocks"
    (with-redefs [skills/list-all (fn [] [])]
      (let [out (prompt/assemble-system-prompt {}
                  {:active-extensions []
                   :system-prompt nil})]
        (expect (not (str/includes? out "<extensions>")))
        (expect (not (str/includes? out "<environment-info>")))
        (expect (not (str/includes? out "<llm_model_prompt")))
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
