(ns com.blockether.vis.internal.prompt-test
  (:require
   [clojure.string :as str]
   [com.blockether.vis.internal.agents :as agents]
   [com.blockether.vis.internal.prompt :as prompt]
   [com.blockether.vis.internal.toggles :as toggles]
   [lazytest.core :refer [defdescribe expect it]]))

(defdescribe prompt-assembly-test
  (it "normalizes core addendum and extension prompt text"
    (let [ext {:ext/name "test.prompt"
               :ext/engine {:ext.engine/alias 't}
               :ext/prompt (fn [_]
                             "\n\n    Extension line\n\n\n\n      Nested extension line\n")}
          env {:extensions (atom [ext])}
          messages (prompt/assemble-stable-prompt-messages env
                     {:system-prompt "\n\n    Addendum line\n\n\n\n      Nested addendum line\n"
                      :active-extensions [ext]})
          text (prompt/stable-prompt-text messages)]
      (expect (str/includes? text "Addendum line\n\n  Nested addendum line"))
      (expect (str/includes? text "Extension line\n\n  Nested extension line"))
      (expect (not (str/includes? text "\n\n\n"))))))

(defn- dag-capable-env
  []
  {:environment-atom (atom {})
   :db-info :db
   :session/state-id :state
   :ctx-atom (atom {})
   :answer-atom (atom nil)
   :answer-fn identity
   :workspace {:id :workspace :root "/repo"}})

(defdescribe dag-expression-prompt-test
  (it "injects the specialized DAG prompt only when toggle and capabilities are present"
    (with-redefs [agents/instructions (constantly {:found? false})
                  toggles/enabled? (fn [id] (= id :vis/dag-expression))]
      (let [missing-capability-text (-> (prompt/assemble-stable-prompt-messages
                                          {} {:active-extensions []})
                                      prompt/stable-prompt-text)
            dag-text (-> (prompt/assemble-stable-prompt-messages
                           (dag-capable-env) {:active-extensions []})
                       prompt/stable-prompt-text)]
        (expect (not (str/includes? missing-capability-text "## DAG expression mode")))
        (expect (str/includes? missing-capability-text "done(\"\"\"...\"\"\")"))
        (expect (str/includes? dag-text "## DAG expression mode"))
        (expect (str/includes? dag-text "`advance({...})` is the only graph-mutating and terminal form"))
        (expect (str/includes? dag-text "Plan stage means graph decomposition"))
        (expect (str/includes? dag-text "A task is an open requirement to be filled with evidence"))
        (expect (not (str/includes? dag-text "Policy-owned obligations may appear")))
        (expect (not (str/includes? dag-text "active policy extension")))
        (expect (not (str/includes? dag-text "active policy providers")))
        (expect (str/includes? dag-text "Every turn has a root goal"))
        (expect (str/includes? dag-text "tiny dialogue task"))
        (expect (str/includes? dag-text "`answer` is literal user-facing narration only"))
        (expect (str/includes? dag-text "`answer_template`"))
        (expect (str/includes? dag-text "`{{tasks.<id>.evidence | transform}}`"))
        (expect (str/includes? dag-text "`evidence_summary`"))
        (expect (str/includes? dag-text "`git_diff_summary`"))
        (expect (str/includes? dag-text "first `advance` the evidence without `done`"))
        (expect (str/includes? dag-text "One-shot terminal answers are only for deterministic `answer_template` summaries"))
        (expect (str/includes? dag-text "`done: True` means close this Vis turn"))
        (expect (str/includes? dag-text "terminal advance must include a non-blank rendered answer"))
        (expect (not (str/includes? dag-text "no_goal")))
        (expect (not (str/includes? dag-text "Bridge")))
        (expect (not (str/includes? dag-text "br_check")))
        (expect (not (str/includes? dag-text "br_next")))
        (expect (not (str/includes? dag-text "br_run_evidence")))
        (expect (not (str/includes? dag-text "`json`")))
        (expect (not (str/includes? dag-text "`truncate`")))
        (expect (not (str/includes? dag-text "done(\"\"\"...\"\"\")"))))))

  (it "mentions policy obligations only when an active extension declares the capability"
    (with-redefs [agents/instructions (constantly {:found? false})
                  toggles/enabled? (fn [id] (= id :vis/dag-expression))]
      (let [policy-ext {:ext/name "policy.test"
                        :ext/capabilities #{:policy/obligations}}
            text (-> (prompt/assemble-stable-prompt-messages
                       (assoc (dag-capable-env) :extensions (atom [policy-ext]))
                       {:active-extensions [policy-ext]})
                   prompt/stable-prompt-text)]
        (expect (str/includes? text "Policy-owned obligations may appear"))
        (expect (str/includes? text "active policy extension"))
        (expect (str/includes? text "do not mark them complete yourself"))
        (expect (str/includes? text "active policy providers"))
        (expect (not (str/includes? text "Bridge")))
        (expect (not (str/includes? text "br_check")))
        (expect (not (str/includes? text "br_next")))
        (expect (not (str/includes? text "br_run_evidence"))))))

  (it "keeps DAG content vis-native and removes foreign generic examples"
    (with-redefs [agents/instructions (constantly {:found? false})
                  toggles/enabled? (fn [id] (= id :vis/dag-expression))]
      (let [text (-> (prompt/assemble-stable-prompt-messages
                       (dag-capable-env) {:active-extensions []})
                   prompt/stable-prompt-text)]
        (expect (str/includes? text "cat"))
        (expect (str/includes? text "rg"))
        (expect (str/includes? text "patch"))
        (expect (str/includes? text "write"))
        (expect (str/includes? text "Vis `<results>` envelope"))
        (doseq [foreign ["stub.go" "stub_test.go" "go test" "shell_run" "settle({"
                         "two-step workflow" "Build the Task Tree"
                         "Goal DAG established. Implementing."]]
          (expect (not (str/includes? text foreign)))))))

  (it "keeps DAG instructions in the stable system prompt without duplicate overlays"
    (with-redefs [agents/instructions (constantly {:found? false})
                  toggles/enabled? (fn [id] (= id :vis/dag-expression))]
      (let [ext {:ext/name "test.prompt"
                 :ext/prompt (constantly "Extension line")}
            messages (prompt/assemble-stable-prompt-messages
                       (assoc (dag-capable-env) :extensions (atom [ext]))
                       {:active-extensions [ext]})
            text (prompt/stable-prompt-text messages)]
        (expect (= ["system" "system"] (mapv :role messages)))
        (expect (str/includes? (:content (first messages)) "SYSTEM-PROMPT"))
        (expect (str/includes? (:content (first messages)) "## DAG expression mode"))
        (expect (str/includes? (:content (second messages)) "TURN-SYSTEM-CONTEXT"))
        (expect (< (str/index-of text "SYSTEM-PROMPT")
                  (str/index-of text "TURN-SYSTEM-CONTEXT")))
        (expect (= 1 (count (re-seq #"## DAG expression mode" text))))
        (expect (= 1 (count (re-seq #"`advance\(\{\.\.\.\}\)` is the only" text)))))))

  (it "does not add the generic CLI done override to DAG sessions"
    (with-redefs [agents/instructions (constantly {:found? false})
                  toggles/enabled? (fn [id] (= id :vis/dag-expression))]
      (let [text (-> (prompt/assemble-stable-prompt-messages
                       (assoc (dag-capable-env) :channel :cli)
                       {:active-extensions []})
                   prompt/stable-prompt-text)]
        (expect (str/includes? text "## DAG expression mode"))
        (expect (not (str/includes? text "NON-INTERACTIVE ONE-SHOT RUN")))
        (expect (not (str/includes? text "Drive every task to completion in this single run")))))))

(defdescribe cli-autonomous-override-test
  (it "drops the candidate approval STOP for the non-interactive :cli channel only"
    (let [text-for (fn [ch]
                     (-> (prompt/assemble-stable-prompt-messages
                           {:channel ch} {:active-extensions []})
                       prompt/stable-prompt-text))
          marker "NON-INTERACTIVE ONE-SHOT RUN"]
      ;; :cli (headless one-shot — no approver) gets the override
      (expect (str/includes? (text-for :cli) marker))
      (expect (str/includes? (text-for :cli) "NEVER emit a `candidate` step"))
      ;; interactive / card-bearing channels keep the approval flow
      (expect (not (str/includes? (text-for :tui) marker)))
      (expect (not (str/includes? (text-for :web) marker)))
      (expect (not (str/includes? (text-for nil) marker))))))

(defdescribe prompt-core-test
  (it "documents engine-owned forms as bare, not extension tools"
    ;; CORE_SYSTEM_PROMPT pins: bare-symbol ENGINE FNS section.
    ;; Engine fns are emitted without namespace qualification.
    (let [text (prompt/build-system-prompt {})]
      (expect (str/includes? text "bare symbols"))
      (expect (str/includes? text "never namespace-qualify"))
      (expect (str/includes? text "Session titles are host-generated"))))

  (it "carries EPISTEMIC + IDENTITY stance so the model probes the project first"
    (let [text (prompt/build-system-prompt {})]
      (expect (str/includes? text "EPISTEMIC"))
      (expect (str/includes? text "runtime > source > docs > assumption"))
      (expect (str/includes? text "IDENTITY"))
      (expect (str/includes? text "HOST project"))
      ;; IDENTITY must be project-agnostic: it has to work in any repo.
      (expect (not (str/includes? text "the Vis PROJECT"))))))

(defdescribe project-instructions-hoist-test
  (it "injects AGENTS.md contents as a dedicated PROJECT-INSTRUCTIONS system block"
    (with-redefs [agents/instructions
                  (constantly {:found? true
                               :source :repo
                               :path "/tmp/repo/AGENTS.md"
                               :content "PROJECT-RULE-FROM-AGENTS-MD\nreproduce -> inspect -> minimal change"})]
      (let [env {:extensions (atom [])}
            messages (prompt/assemble-stable-prompt-messages env
                       {:active-extensions []})
            text (prompt/stable-prompt-text messages)]
        (expect (str/includes? text "PROJECT-INSTRUCTIONS"))
        (expect (str/includes? text "PROJECT-RULE-FROM-AGENTS-MD"))
        (expect (str/includes? text "/tmp/repo/AGENTS.md"))
        ;; Send order: SYSTEM-PROMPT first, then PROJECT-INSTRUCTIONS.
        (expect (< (str/index-of text "SYSTEM-PROMPT")
                  (str/index-of text "PROJECT-INSTRUCTIONS"))))))

  (it "falls back to CLAUDE.md when AGENTS.md is absent and labels the source"
    (with-redefs [agents/instructions
                  (constantly {:found? true
                               :source :repo:claude-md-fallback
                               :path "/tmp/repo/CLAUDE.md"
                               :content "CLAUDE-FALLBACK-RULE"})]
      (let [env {:extensions (atom [])}
            messages (prompt/assemble-stable-prompt-messages env
                       {:active-extensions []})
            text (prompt/stable-prompt-text messages)]
        (expect (str/includes? text "PROJECT-INSTRUCTIONS"))
        (expect (str/includes? text "CLAUDE-FALLBACK-RULE"))
        (expect (str/includes? text "CLAUDE.md")))))

  (it "emits no PROJECT-INSTRUCTIONS block when no guidance file is present"
    (with-redefs [agents/instructions (constantly {:found? false})]
      (let [env {:extensions (atom [])}
            messages (prompt/assemble-stable-prompt-messages env
                       {:active-extensions []})
            text (prompt/stable-prompt-text messages)]
        (expect (not (str/includes? text "PROJECT-INSTRUCTIONS")))))))

(defdescribe extension-activation-test
  (it "assembles from precomputed active extensions without activating again"
    (let [calls (atom 0)
          ext {:ext/name "test.activation"
               :ext/activation-fn (fn [_]
                                    (swap! calls inc)
                                    true)
               :ext/prompt (constantly "Active prompt")}
          env {:extensions (atom [ext])}
          active (prompt/active-extensions env)]
      (expect (= 1 @calls))
      (prompt/assemble-stable-prompt-messages env {:active-extensions active})
      (expect (= 1 @calls)))))

(defdescribe reasoning-via-comments-nudge-test
  "Reason-via-code-comments fallback for non-reasoning models."
  (it "inserts a system nudge after leading system messages, before the conversation"
    (let [msgs [{:role "system" :content "core"}
                {:role "system" :content "project"}
                {:role "user" :content "do the thing"}]
          out  (prompt/with-reasoning-comments-nudge msgs)]
      ;; one message added
      (expect (= (inc (count msgs)) (count out)))
      ;; inserted at the system/conversation boundary (index 2), still a system msg
      (expect (= "system" (:role (nth out 2))))
      (expect (str/includes? (:content (nth out 2)) ";;"))
      ;; leading systems untouched, user message still last
      (expect (= "core" (:content (first out))))
      (expect (= "user" (:role (last out))))))

  (it "prepends when there are no leading system messages"
    (let [out (prompt/with-reasoning-comments-nudge [{:role "user" :content "x"}])]
      (expect (= "system" (:role (first out))))
      (expect (= 2 (count out))))))
