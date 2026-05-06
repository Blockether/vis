(ns com.blockether.vis.internal.prompt-test
  "Smoke coverage for `internal/prompt`.

   Why it exists: AGENTS.md's hard rule \u2014 every namespace must have a
   matching `_test.clj`. The richer end-to-end coverage of the system
   prompt sits in `sandbox_compose_test`; this file pins down the
   small pure helpers in `prompt.clj` so refactors that quietly break
   `<journal>`/`<var_index>` rendering or nudge plumbing get caught
   here instead of in a model trace.

   In particular, after the `repetition-warning` cull we want a
   regression that:
     1. `build-iteration-context` no longer accepts / requires a
        `:call-counts-atom` arg.
     2. The function never injects a `<system_nudge>` entry on its own
        \u2014 only `:ext/nudge-fn` results land in the output."
  (:require
   [clojure.string :as str]
   [com.blockether.vis.internal.extension :as extension]
   [com.blockether.vis.internal.prompt :as prompt]
   [lazytest.core :refer [defdescribe expect it]]))

(def ^:private NO_EXTENSIONS [])

(defdescribe safe-pr-str-test
  (it "round-trips simple values"
    (expect (= "42" (prompt/safe-pr-str 42)))
    (expect (= "\"hi\"" (prompt/safe-pr-str "hi")))
    (expect (= "[1 2 3]" (prompt/safe-pr-str [1 2 3]))))

  (it "truncates oversized strings with a chars-remaining suffix"
    (let [big (apply str (repeat 10000 \x))
          out (prompt/safe-pr-str big {:max-chars 50})]
      (expect (re-find #" \u2026<\+\d+ chars>$" out))
      (expect (< (count out) 200))))

  (it "zprints Clojure data results instead of one-line pr-str blobs"
    (let [out (prompt/safe-pr-str {:alpha (vec (range 20))
                                   :beta {:nested true
                                          :numbers (vec (range 10))}}
                {:max-chars 2000})]
      (expect (str/includes? out "\n"))
      (expect (str/includes? out ":alpha"))
      (expect (str/includes? out ":beta"))))

  (it "swallows unprintable values instead of throwing"
    (let [bomb (reify Object (toString [_] (throw (ex-info "boom" {}))))]
      (expect (string? (prompt/safe-pr-str bomb))))))

(defdescribe truncated-pr-str-test
  (it "returns [string false] for small values"
    (let [[s truncated?] (prompt/truncated-pr-str {:a 1})]
      (expect (string? s))
      (expect (false? truncated?))))

  (it "flags truncation for oversized values"
    (let [big (apply str (repeat (* 10 prompt/MAX_RESULT_DISPLAY_CHARS) \y))
          [_s truncated?] (prompt/truncated-pr-str big)]
      (expect (true? truncated?)))))

(defdescribe build-iteration-context-test
  (it "requires :active-extensions; throws otherwise"
    (let [thrown? (try
                    (prompt/build-iteration-context {} {})
                    false
                    (catch clojure.lang.ExceptionInfo e
                      (= :vis/missing-active-extensions
                        (:type (ex-data e)))))]
      (expect (true? thrown?))))

  (it "returns nil when there is nothing to render and no nudges fire"
    ;; Pre-bind a non-blank title so the always-on title nudge stays
    ;; quiet, and skip a journal seed: nothing to render -> nil.
    (expect (nil? (prompt/build-iteration-context
                    {:conversation-title-atom (atom "set")}
                    {:active-extensions NO_EXTENSIONS
                     :iteration         0}))))

  (it "renders <journal> for prior iteration blocks without LLM-only reasoning"
    (let [out (prompt/build-iteration-context
                {:conversation-title-atom (atom "set")}
                {:active-extensions   NO_EXTENSIONS
                 :blocks-by-iteration [[1 {:thinking "LLM-only private reasoning must stay out"
                                           :blocks   [{:code "(+ 1 2)"
                                                       :comment "block-authored intermediate comment is okay"
                                                       :result 3
                                                       :provenance {:ref "turn/3f2a91c0/iteration/1/block/1"
                                                                    :op :sci/eval
                                                                    :status :done}}]}]]
                 :iteration           0})]
      (expect (string? out))
      (expect (str/includes? out "<journal>"))
      (expect (str/includes? out "(+ 1 2)"))
      (expect (str/includes? out "turn/3f2a91c0/iteration/1/block/1"))
      (expect (str/includes? out ";; block-authored intermediate comment is okay"))
      (expect (not (str/includes? out "LLM-only private reasoning must stay out")))
      (expect (not (str/includes? out "iteration/1 thinking:")))))

  (it "renders every preview captured from one block into <journal>"
    (let [preview-a {:success? true :result {:a 1} :error nil
                     :provenance {:op :v/preview}}
          preview-b {:success? true :result {:b 2} :error nil
                     :provenance {:op :v/preview}}
          out (with-redefs [extension/render-tool-result
                            (fn [_surface tool-result]
                              (str "PREVIEW=" (pr-str (:result tool-result))))]
                (prompt/build-iteration-context
                  {:conversation-title-atom (atom "set")}
                  {:active-extensions   NO_EXTENSIONS
                   :blocks-by-iteration [[1 {:thinking nil
                                             :blocks [{:code "(do (v/preview a) (v/preview b) :done)"
                                                       :result :done
                                                       :previews [preview-a preview-b]
                                                       :provenance {:ref "turn/3f2a91c0/iteration/1/block/1"
                                                                    :op :sci/eval
                                                                    :status :done}}]}]]
                   :iteration 0}))]
      (expect (str/includes? out "PREVIEW={:a 1}"))
      (expect (str/includes? out "PREVIEW={:b 2}"))))

  (it "renders block-level provenance in <journal> for regular evaluated forms"
    (let [out (prompt/build-iteration-context
                {:conversation-title-atom (atom "set")}
                {:active-extensions   NO_EXTENSIONS
                 :blocks-by-iteration [[1 {:thinking nil
                                           :blocks   [{:code "(+ 1 2)"
                                                       :result 3
                                                       :provenance {:ref "turn/3f2a91c0/iteration/1/block/1"
                                                                    :op :sci/eval
                                                                    :status :done
                                                                    :duration-ms 7}}]}]]
                 :iteration           1})]
      (expect (string? out))
      (expect (str/includes? out ":provenance"))
      (expect (str/includes? out ":ref \"turn/3f2a91c0/iteration/1/block/1\""))
      (expect (str/includes? out ":op :sci/eval"))
      (expect (str/includes? out ":status :done"))))

  (it "caps whole journal rendering by token budget and keeps newest evidence"
    (let [blocks (mapv (fn [idx]
                         {:code (str "(probe " idx ")")
                          :result (apply str (repeat (* 2 prompt/MAX_RESULT_DISPLAY_CHARS) \x))
                          :provenance {:ref (str "turn/3f2a91c0/iteration/1/block/" idx)
                                       :op :sci/eval
                                       :status :done}})
                   (range 1 120))
          out (prompt/build-iteration-context
                {:conversation-title-atom (atom "set")}
                {:active-extensions   NO_EXTENSIONS
                 :blocks-by-iteration [[1 {:thinking nil :blocks blocks}]]
                 :iteration           1})]
      (expect (string? out))
      ;; nil model uses the conservative 32k context fallback; journal
      ;; budget is capped at 50% before pinned/var-index reductions,
      ;; and token count falls back to chars/4.
      (expect (< (count out) 70000))
      (expect (str/includes? out "older journal lines omitted"))
      (expect (str/includes? out "journal cap <= 50% model context"))
      (expect (str/includes? out "turn/3f2a91c0/iteration/1/block/119"))))

  ;; Helpers ------------------------------------------------------------------
  ;;
  ;; New `:blocks-by-iteration` shape is `[[pos {:thinking :blocks}]]`
  ;; (the renderer was upgraded to surface per-iter thinking + per-form
  ;; comments). Old `[[pos blocks]]` shape would be silently treated
  ;; as `:thinking nil :blocks <vec-of-pairs>` — wrap explicitly.
  ;;
  ;; The title-nudge built-in always fires on a bare `{}` env (no
  ;; `:conversation-title-atom`, so the title is treated as blank).
  ;; Tests that probe nudges from OTHER paths use a `with-title`
  ;; helper that pre-binds a non-blank title.
  (letfn [(env-with-title []
            {:conversation-title-atom (atom "already set")})
          (->iter [pos blocks]
            [pos {:thinking nil :blocks blocks}])]

    (it "never injects a built-in repetition nudge, even on identical reruns"
      ;; Regression: the built-in `repetition-warning` was removed.
      ;; Repeating the same expression must NOT produce a
      ;; `<system_nudge>` entry from the repetition path. Title-nudge
      ;; suppressed by pre-binding a non-blank title; we check for the
      ;; absence of any system nudge entry.
      (let [env    (env-with-title)
            blocks [{:code "(grep \"X\")" :result []}]
            out-1  (prompt/build-iteration-context
                     env {:active-extensions   NO_EXTENSIONS
                          :blocks-by-iteration [(->iter 0 blocks)]
                          :iteration           0})
            out-2  (prompt/build-iteration-context
                     env {:active-extensions   NO_EXTENSIONS
                          :blocks-by-iteration [(->iter 1 blocks)]
                          :iteration           1})]
        (expect (not (str/includes? (or out-1 "") "<system_nudge")))
        (expect (not (str/includes? (or out-2 "") "<system_nudge")))))

    (it "appends extension nudges when :ext/nudge-fn returns a non-blank string"
      (let [ext (identity
                  {:ext/namespace 'fake.nudger
                   :ext/nudge-fn  (fn [_ctx] "hi from fake.nudger")})
            ;; Always-on title nudge keeps the line count >= 1 already;
            ;; we just check the model's nudge gets concatenated.
            ext-only-ext {:ext/namespace 'fake.nudger
                          :ext/nudge-fn  (fn [_ctx] "hi from fake.nudger")}
            out (prompt/build-iteration-context
                  (env-with-title)
                  {:active-extensions   [ext-only-ext]
                   :blocks-by-iteration [(->iter 0 [{:code "1" :result 1}])]
                   :iteration           0})]
        (expect (str/includes? out "<system_nudges>"))
        (expect (str/includes? out "<system_nudge importance=\"normal\">\nhi from fake.nudger\n</system_nudge>"))
        ;; Silence the unused alias warning -- present for future tests.
        (when (some? ext) :ok)))

    (it "accepts extension nudge maps with explicit importance"
      (let [ext {:ext/namespace 'fake.important
                 :ext/nudge-fn  (fn [_ctx] {:importance :critical
                                            :text "Stop and ask user."})}
            out (prompt/build-iteration-context
                  (env-with-title)
                  {:active-extensions   [ext]
                   :blocks-by-iteration [(->iter 0 [{:code "1" :result 1}])]
                   :iteration           0})]
        (expect (str/includes? out "<system_nudge importance=\"critical\">\nStop and ask user.\n</system_nudge>"))))

    (it "skips invalid extension nudge returns"
      (let [ext {:ext/namespace 'fake.invalid
                 :ext/nudge-fn  (fn [_ctx] {:importance :urgent
                                            :text "bad level"})}
            out (prompt/build-iteration-context
                  (env-with-title)
                  {:active-extensions   [ext]
                   :blocks-by-iteration [(->iter 0 [{:code "1" :result 1}])]
                   :iteration           0})]
        (expect (string? out))
        (expect (not (str/includes? out "bad level")))
        (expect (not (str/includes? out "<system_nudge")))))

    (it "swallows extension nudge-fn exceptions instead of bubbling"
      (let [ext {:ext/namespace 'fake.thrower
                 :ext/nudge-fn  (fn [_ctx] (throw (ex-info "boom" {})))}
            out (prompt/build-iteration-context
                  (env-with-title)
                  {:active-extensions   [ext]
                   :blocks-by-iteration [(->iter 0 [{:code "1" :result 1}])]
                   :iteration           0})]
        (expect (string? out))
        ;; Both built-in and extension nudges suppressed: title is
        ;; non-blank, iteration not on the refresh cadence,
        ;; extension nudge threw and was swallowed.
        (expect (not (str/includes? out "<system_nudge")))))

    (it "fires the title-nudge when CONVERSATION_TITLE is blank"
      (let [env-blank-title {:conversation-title-atom (atom "")}
            out (prompt/build-iteration-context
                  env-blank-title
                  {:active-extensions   NO_EXTENSIONS
                   :blocks-by-iteration [(->iter 0 [{:code "1" :result 1}])]
                   :iteration           0})]
        (expect (str/includes? out "<system_nudges>"))
        (expect (str/includes? out "<system_nudge importance=\"low\">"))
        (expect (str/includes? out "CONVERSATION_TITLE is currently empty"))))

    (it "fires a title-refresh nudge when the turn boundary requests it"
      (let [out (prompt/build-iteration-context
                  (env-with-title)
                  {:active-extensions   NO_EXTENSIONS
                   :blocks-by-iteration [(->iter 0 [{:code "1" :result 1}])]
                   :iteration           0
                   :title-refresh?      true})]
        (expect (str/includes? out "<system_nudge importance=\"low\">"))
        (expect (str/includes? out "Current CONVERSATION_TITLE is"))))

    (it "fires the title-refresh nudge every TITLE_REFRESH_NUDGE_PERIOD iterations"
      (let [period @(requiring-resolve 'com.blockether.vis.internal.prompt/TITLE_REFRESH_NUDGE_PERIOD)
            out (prompt/build-iteration-context
                  (env-with-title)
                  {:active-extensions   NO_EXTENSIONS
                   :blocks-by-iteration [(->iter 0 [{:code "1" :result 1}])]
                   :iteration           period})]
        (expect (str/includes? out "<system_nudge importance=\"low\">"))
        (expect (str/includes? out (str "You're " period " iterations"))))))

  (it "renders loaded skill bodies under <extensions>/<active_skills>"
    (let [out (prompt/build-iteration-context
                {:conversation-title-atom (atom "already set")
                 :active-skills-atom (atom {"diagnose" {:name "diagnose"
                                                        :description "Debug loop."
                                                        :source :repo
                                                        :path "/repo/.agents/skills/diagnose/SKILL.md"
                                                        :body "# Full skill body\nDo repro first."}})}
                {:active-extensions NO_EXTENSIONS
                 :iteration 0})]
      (expect (str/includes? out "<extensions>\n<active_skills count=\"1\">"))
      (expect (str/includes? out "<skill name=\"diagnose\" source=\"repo\">\n# Full skill body\nDo repro first.\n</skill>"))
      (expect (not (str/includes? out "Debug loop.")))
      (expect (not (str/includes? out "/repo/.agents/skills/diagnose/SKILL.md")))))

  (it "budgets <var_index> by tokens and keeps newest entries"
    (let [entry (fn [n]
                  (str ";; v=" n " scope=live n=8000\n"
                    "(def v" n " \"" (apply str (repeat 8000 \x)) "\")"))
          big-index (str/join "\n" (map entry (range 1 20)))
          read-var-index-var (resolve 'com.blockether.vis.internal.prompt/read-var-index-str)]
      (with-redefs-fn {read-var-index-var (constantly big-index)}
        (fn []
          (let [out (prompt/build-iteration-context
                      {:conversation-title-atom (atom "already set")}
                      {:active-extensions NO_EXTENSIONS
                       :iteration 0
                       :context-limit 2000})]
            (expect (str/includes? out "<var_index>"))
            (expect (str/includes? out "(def v1"))
            (expect (str/includes? out "older <var_index> entries omitted"))
            (expect (not (str/includes? out "(def v19")))))))))

(defdescribe extensions-snapshot-test
  (it "includes full extension provenance plus symbols/docs"
    (let [ext (or (some #(when (= 'com.blockether.vis.core (:ext/namespace %)) %)
                    (extension/registered-extensions))
                (extension/extension {:ext/namespace 'com.blockether.vis.core
                                      :ext/doc       "vis core"}))
          [entry] (prompt/extensions-snapshot [ext])]
      (expect (= 'com.blockether.vis.core (:namespace entry)))
      (expect (contains? entry :source-paths))
      (expect (contains? entry :source-mtime-max))
      (expect (contains? entry :source-hash-sha256))
      (expect (contains? entry :symbols))
      (expect (contains? entry :docs)))))

(defdescribe core-system-prompt-test
  (it "front-loads the RLM control-flow contract"
    (let [p prompt/CORE_SYSTEM_PROMPT]
      (expect (str/includes? p "recursive language model (RLM)"))
      (expect (str/includes? p "read/eval/observe loop"))
      (expect (str/includes? p "`(answer ARG)` is terminal"))
      (expect (str/includes? p "nested Markdown fences"))
      (expect (str/includes? p "never emit raw nested Markdown fences"))
      (expect (str/includes? p "TURN_USER_REQUEST is fully satisfied"))
      (expect (str/includes? p "the host continues the same user turn"))
      (expect (str/includes? p "After iteration 1, `(answer …)` is the ONLY top-level form"))
      (expect (str/includes? p "In iteration 1 only, trivial chat may answer as the last top-level form"))
      (expect (str/includes? p "ONLY top-level form of its final iteration"))
      (expect (str/includes? p "UNDERSTAND"))
      (expect (str/includes? p "INTENT"))
      (expect (str/includes? p "EXPLORE"))
      (expect (str/includes? p "OBSERVE"))
      (expect (str/includes? p "ACT"))
      (expect (str/includes? p "VERIFY"))
      (expect (str/includes? p "ANSWER"))))

  (it "centers Vis on Nucleus plus a state/decision/observed-state matrix"
    (let [p prompt/CORE_SYSTEM_PROMPT]
      (expect (str/includes? p "λ engage(nucleus)."))
      (expect (str/includes? p "Human ⊗ Vis ⊗ Workspace ⊗ REPL"))
      (expect (str/includes? p "| OODA"))
      (expect (str/includes? p "State → decision matrix → observed new state"))
      (expect (str/includes? p "| State | Decision | Emit / do | New state in `<journal>` / `<var_index>` | Next state |"))
      (expect (str/includes? p "| STUCK |"))
      (expect (str/includes? p "Stop looping"))
      (expect (str/includes? p "ANSWER to user with impediment/clarifying ask"))
      (expect (str/includes? p "Host-enforced gates before final answer"))
      (expect (str/includes? p "Model discipline"))
      (expect (str/includes? p "run `(v/latest-provenance-refs)` / `(v/provenance-guards)` before citing provenance"))
      (expect (str/includes? p "S5 identity/rules"))
      (expect (str/includes? p "S3 plans/gates/resources"))
      (expect (str/includes? p "S2 coordinate journal+vars+tools+intent graph"))
      (expect (str/includes? p "Evidence taxonomy"))
      (expect (str/includes? p "evidence producers create observed journal facts"))
      (expect (str/includes? p "Diagnostic enrichers explain evidence"))
      (expect (str/includes? p "Resolution state consumes refs"))
      (expect (str/includes? p "Do not call this a standalone proof layer"))
      (expect (not (str/includes? p "Runtime Nucleus")))
      (expect (not (str/includes? p "Nucleus decision matrix")))
      (expect (not (str/includes? p "Nucleus palette")))
      (expect (not (str/includes? p "VSM operating stack")))
      (expect (not (str/includes? p "provenance-checks pass")))
      (expect (not (str/includes? p "Michael Whitford")))
      (expect (not (str/includes? p "github.com/michaelwhitford/nucleus")))))

  (it "requires Markdown final answers by default and prefers v/ helpers"
    (let [p prompt/CORE_SYSTEM_PROMPT]
      (expect (str/includes? p "Final answers are Markdown by default"))
      (expect (str/includes? p "prefer `v/join`"))
      (expect (str/includes? p "v/join"))
      (expect (str/includes? p "v/file-link"))
      (expect (str/includes? p "v/code-block"))))

  (it "teaches SYSTEM vars as a typed Markdown table with direct sandbox bindings"
    (let [p prompt/CORE_SYSTEM_PROMPT]
      (expect (str/includes? p "Reference them by name in Clojure forms"))
      (expect (str/includes? p "| SYSTEM VAR | Value | Type | What is it |"))
      (expect (str/includes? p "CONVERSATION_SOUL_ID"))
      (expect (str/includes? p "CONVERSATION_STATE_ID"))
      (expect (str/includes? p "| `CONVERSATION_SOUL_ID` | parent `conversation_soul.id` | `uuid` |"))
      (expect (not (str/includes? p "CONVERSATION_METADATA")))))

  (it "teaches the current conversation intent lifecycle without legacy ref patterns"
    (let [p prompt/CORE_SYSTEM_PROMPT]
      (expect (str/includes? p "intents are database-backed, conversation-scoped"))
      (expect (str/includes? p "do not keep a local proof map"))
      (expect (str/includes? p "v/issue-intent!"))
      (expect (str/includes? p "Plans are persisted Clojure DSL graphs"))
      (expect (str/includes? p "Proof slot IDs are values shaped `[intent-id :slot-name]`"))
      (expect (str/includes? p "v/proof-slot"))
      (expect (str/includes? p "v/plan"))
      (expect (str/includes? p "verification-slot"))
      (expect (str/includes? p "v/intents"))
      (expect (str/includes? p "v/fulfill-intent!"))
      (expect (str/includes? p "inside the single final wrapper when all refs are already observed"))
      (expect (str/includes? p "Every observed top-level form becomes a journal block"))
      (expect (str/includes? p "turn/<turn8>/iteration/<n>/block/<k>"))
      (expect (str/includes? p "v/await-proof!"))
      (expect (str/includes? p "Plain deref stays legal Clojure"))
      (expect (not (str/includes? p "i1.1")))
      (expect (not (str/includes? p "i4.2/tool")))
      (expect (not (str/includes? p "E1")))
      (expect (not (str/includes? p "G1")))))

  (it "teaches the correct multi-iteration finish pattern"
    (let [p prompt/CORE_SYSTEM_PROMPT]
      (expect (str/includes? p "Exploration/action/verification iterations omit `(answer …)` so the host loops"))
      (expect (str/includes? p "Correct multi-iteration finish pattern"))
      (expect (str/includes? p "iteration N: verify and surface final evidence, no answer yet"))
      (expect (str/includes? p "iteration N+1: final turn-finisher after observed evidence, exactly one top-level form"))
      (expect (str/includes? p "If intent resolution is still pending, do it inside this one wrapper with observed refs only"))
      (expect (str/includes? p "Final intent resolution may live inside that one wrapper only when it cites already-observed refs"))
      (expect (not (str/includes? p "(answer \"scanned\") ; BAD")))
      (expect (not (str/includes? p "last 2 iters")))))

  (it "pins the first-iteration no-answer discipline for code/debug/change tasks"
    (let [p prompt/CORE_SYSTEM_PROMPT]
      (expect (str/includes? p "FIRST-ITERATION ANSWER BAN"))
      (expect (str/includes? p "for code/debug/change/refactor/test/verify/run/search/explain repo-state work"))
      (expect (str/includes? p "iteration 1 probes only"))
      (expect (str/includes? p "Scratch values are still useful, but they are not proof"))
      (expect (str/includes? p "Never invent refs"))
      (expect (str/includes? p "If reader/parser errors repeat")))))

(defdescribe environment-info-prompt-test
  (it "collects dedicated environment-info fragments from active extensions"
    (let [ext-a (extension/extension
                  {:ext/namespace 'test.env-info.a
                   :ext/doc       "Environment facts A."
                   :ext/environment-info-fn (fn [env]
                                              (str "cwd: " (:cwd env)))})
          ext-b (extension/extension
                  {:ext/namespace 'test.env-info.b
                   :ext/doc       "Environment facts B."
                   :ext/environment-info-fn (constantly ["repo: service-a"
                                                         "dirty: no"])})
          out   (prompt/assemble-system-prompt
                  {:cwd "/tmp/project"}
                  {:active-extensions [ext-a ext-b]})]
      (expect (str/includes? out "<environment-info>"))
      (expect (str/includes? out "<section extension=\"test.env-info.a\">"))
      (expect (str/includes? out "cwd: /tmp/project"))
      (expect (str/includes? out "<section extension=\"test.env-info.b\">"))
      (expect (str/includes? out "repo: service-a\ndirty: no"))))

  (it "skips blank environment-info fragments"
    (let [ext (extension/extension
                {:ext/namespace 'test.env-info.blank
                 :ext/doc       "Blank environment facts."
                 :ext/environment-info-fn (constantly "")})
          out (prompt/assemble-system-prompt
                {}
                {:active-extensions [ext]})]
      (expect (not (str/includes? out "<environment-info>")))))

  (it "never char-caps string environment-info fragments"
    (let [huge (apply str (repeat 100000 \e))
          ext (extension/extension
                {:ext/namespace 'test.env-info.huge
                 :ext/doc       "Huge environment facts."
                 :ext/environment-info-fn (constantly huge)})
          out (prompt/assemble-system-prompt
                {}
                {:active-extensions [ext]})]
      (expect (str/includes? out huge))
      (expect (not (str/includes? out "…<+")))))

  (it "renders invalid non-string environment-info returns as visible errors, not truncated data"
    (let [ext (extension/extension
                {:ext/namespace 'test.env-info.invalid
                 :ext/doc       "Invalid environment facts."
                 :ext/environment-info-fn (constantly {:not :valid})})
          out (prompt/assemble-system-prompt
                {}
                {:active-extensions [ext]})]
      (expect (str/includes? out "<environment-info-error>"))
      (expect (str/includes? out "clojure.lang.PersistentArrayMap")))))

(defdescribe extension-prompt-test
  (it "wraps extension prompt fragments under <extensions>"
    (let [ext (extension/extension
                {:ext/namespace 'test.ext.prompt
                 :ext/doc "Prompt extension."
                 :ext/ns-alias '{:alias v :ns test.ext.prompt}
                 :ext/prompt (constantly "Use v/foo for foo.")})
          out (prompt/assemble-system-prompt
                {}
                {:active-extensions [ext]})]
      (expect (str/includes? out "<system_prompt>"))
      (expect (str/includes? out "</system_prompt>"))
      (expect (str/includes? out "<extensions>"))
      (expect (str/includes? out "<extension namespace=\"test.ext.prompt\" alias=\"v\" target-namespace=\"test.ext.prompt\">"))
      (expect (str/includes? out "Use v/foo for foo."))
      (expect (str/includes? out "</extensions>")))))

(defdescribe provider-prompt-test
  (it "appends active provider prompt blocks without replacing the core prompt"
    (let [seen (atom nil)
          out (prompt/assemble-system-prompt
                {:env-id :test}
                {:active-extensions []
                 :provider-prompt-context
                 {:provider {:id :codex-test :models [{:name "m"}]}
                  :model {:provider :codex-test :name "m"}
                  :environment {:env-id :test}
                  :descriptor {:provider/id :codex-test
                               :provider/label "Codex Test"
                               :provider/prompt-fn (fn [ctx]
                                                     (reset! seen ctx)
                                                     "Provider-specific addendum.")}}})]
      (expect (str/includes? out "recursive language model (RLM)"))
      (expect (str/includes? out "<specific_provider_model_prompt provider=\"codex-test\" model=\"m\">"))
      (expect (str/includes? out "Provider-specific addendum."))
      (expect (= :codex-test (get-in @seen [:provider :id])))))

  (it "skips blank provider prompt blocks"
    (let [out (prompt/assemble-system-prompt
                {}
                {:active-extensions []
                 :provider-prompt-context
                 {:provider {:id :blank-provider}
                  :descriptor {:provider/id :blank-provider
                               :provider/label "Blank"
                               :provider/prompt-fn (constantly "")}}})]
      (expect (not (str/includes? out "<specific_provider_model_prompt"))))))

(defdescribe assemble-initial-messages-test
  (it "places only system and current user content in order"
    (let [msgs (prompt/assemble-initial-messages
                 {:system-prompt        "SYS"
                  :initial-user-content "now"})]
      (expect (= [{:role "system" :content "SYS"}
                  {:role "user" :content "<user_turn_request_main_goal>\nnow\n</user_turn_request_main_goal>"}]
                msgs))))

  (it "omits the system slot when no system prompt is supplied"
    (let [msgs (prompt/assemble-initial-messages
                 {:initial-user-content "hi"})]
      (expect (= [{:role "user" :content "<user_turn_request_main_goal>\nhi\n</user_turn_request_main_goal>"}] msgs)))))
