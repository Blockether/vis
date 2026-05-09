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
  (it "states the GROUND RULE: generate -> evaluate -> populate -> observe -> decide"
    (let [p prompt/CORE_SYSTEM_PROMPT]
      (expect (str/includes? p "GROUND RULE"))
      (expect (str/includes? p "GENERATE one OR MORE fenced ```clojure blocks"))
      (expect (str/includes? p "AUTOMATICALLY populates results into <journal>"))
      (expect (str/includes? p "OBSERVE"))
      (expect (str/includes? p "DECIDE"))
      (expect (str/includes? p "(answer"))))

  (it "states the OUTPUT FORMAT contract before GROUND RULE"
    ;; Regression: conversation 185fbc4f had GLM-5.1 fabricate a
    ;; <journal> envelope and close it with a stray ``` that swallowed
    ;; the next real ```clojure opener. The strict format header tells
    ;; the model: only fenced clojure blocks, prose -> ;; comments
    ;; INSIDE the fence.
    (let [p prompt/CORE_SYSTEM_PROMPT
          fmt-idx (str/index-of p "OUTPUT FORMAT")
          ground-idx (str/index-of p "GROUND RULE")]
      (expect (some? fmt-idx))
      (expect (some? ground-idx))
      (expect (< (long fmt-idx) (long ground-idx)))
      (expect (str/includes? p "ONE OR MORE fenced ```clojure blocks. NOTHING else"))
      (expect (str/includes? p ";; comments INSIDE the fence"))))

  (it "DEFS RENDER sub-rule says every top-level form (incl. defs) appears in <journal>"
    ;; Regression: the prompt previously taught "SILENT FORMS — defs
    ;; are acquisition, not observation" because lone defs were
    ;; elided from <journal>. The whole `:vis/silent` mechanism has
    ;; been removed - every top-level form including defs renders.
    (let [p prompt/CORE_SYSTEM_PROMPT]
      (expect (str/includes? p "DEFS RENDER"))
      (expect (str/includes? p "<bindings>"))
      (expect (str/includes? p "every top-level form"))
      (expect (str/includes? p "contiguous"))
      ;; The old language must be gone - if it survives, the
      ;; prompt is lying to the model about elision behavior.
      (expect (not (str/includes? p "SILENT FORMS")))
      (expect (not (str/includes? p "acquisition, not observation")))))

  (it "BATCHING sub-rule documents (do ...) for one-entry observation"
    (let [p prompt/CORE_SYSTEM_PROMPT]
      (expect (str/includes? p "BATCHING"))
      (expect (str/includes? p "(do"))
      (expect (str/includes? p "ONE journal entry"))))

  (it "BINDINGS sub-rule documents *1/*2/*3/*e as escape hatches"
    (let [p prompt/CORE_SYSTEM_PROMPT]
      (expect (str/includes? p "BINDINGS"))
      (expect (str/includes? p "`*1`"))
      (expect (str/includes? p "`*e`"))
      (expect (str/includes? p "prefer durable names")))))

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
                                                     :rendering-kind :vis/silent}
                                                    {:code "(+ 1 1)"
                                                     :result 2}]}]]})]
      (expect (str/includes? out "<journal>"))
      (expect (str/includes? out "(def xd"))
      (expect (str/includes? out "XDDD"))
      (expect (str/includes? out "(+ 1 1) -> 2")))))

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
