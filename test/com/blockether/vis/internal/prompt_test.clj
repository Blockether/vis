(ns com.blockether.vis.internal.prompt-test
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.agents :as agents]
            [com.blockether.vis.internal.env-python :as env-python]
            [com.blockether.vis.internal.prompt :as prompt]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe prompt-assembly-test
             (it "normalizes core addendum and extension prompt text"
                 (let
                   [ext
                    {:ext/name "test.prompt"
                     :ext/engine {:ext.engine/alias 't}
                     :ext/prompt-fn
                     (fn [_]
                       "\n\n    Extension line\n\n\n\n      Nested extension line\n")}

                    env
                    {:extensions (atom [ext])}

                    messages
                    (prompt/assemble-stable-prompt-messages
                      env
                      {:system-prompt "\n\n    Addendum line\n\n\n\n      Nested addendum line\n"
                       :active-extensions [ext]})

                    text
                    (prompt/stable-prompt-text messages)]

                   (expect (str/includes? text "Addendum line\n\n  Nested addendum line"))
                   (expect (str/includes? text "Extension line\n\n  Nested extension line"))
                   (expect (not (str/includes? text "\n\n\n"))))))

(defdescribe cli-autonomous-override-test
             (it "drops the candidate approval STOP for the non-interactive :cli channel only"
                 (let
                   [text-for
                    (fn [ch]
                      (-> (prompt/assemble-stable-prompt-messages {:channel ch}
                                                                  {:active-extensions []})
                          prompt/stable-prompt-text))

                    marker
                    "NON-INTERACTIVE ONE-SHOT RUN"]

                   ;; :cli (headless one-shot — no approver) gets the override
                   (expect (str/includes? (text-for :cli) marker))
                   (expect (str/includes? (text-for :cli) "NEVER stop to wait for approval"))
                   (expect (str/includes? (text-for :cli) "MUST NOT perform destructive"))
                   (expect (not (str/includes? (text-for :cli) "big, risky")))
                   ;; interactive / card-bearing channels keep the approval flow
                   (expect (not (str/includes? (text-for :tui) marker)))
                   (expect (not (str/includes? (text-for :web) marker)))
                   (expect (not (str/includes? (text-for nil) marker))))))

(defdescribe
  prompt-core-test
  (it "keeps live native contracts authoritative"
      (let [text (prompt/build-system-prompt {})]
        (expect (str/includes? text "Native descriptions and JSON Schemas are authoritative"))
        (expect (str/includes? text "hard preconditions"))
        (expect (not (str/includes? text "Session titles are host-generated")))))
  (it
    "keeps the sectioned core contract explicit and non-contradictory"
    (let [text (var-get (ns-resolve 'com.blockether.vis.internal.prompt 'CORE_SYSTEM_PROMPT))]
      (expect (< (count text) 4440))
      (doseq
        [heading ["## 1. Identity + Epistemic stance" "## 2. Execution surfaces" "## 3. Inspect"
                  "## 4. Edit + verify" "## 5. Act autonomously" "## 6. Manage context"
                  "## 7. Style and finish"]]
        (expect (str/includes? text heading)))
      (doseq [tool ["`struct_node`" "`struct_occurrences`" "`struct_rename`"]]
        (expect (not (str/includes? text tool))))
      (doseq
        [required
         ["Work on the host project by default" "For vis tasks" "`await vis_docs()`"
          "runtime > source > docs > assumption"
          "Native descriptions and JSON Schemas are authoritative" "never guess contracts"
          "`python_execution`" "`await gather(...)` independent calls"
          "anything complicated" "stays retrievable as" "control exactly what enters context"
          "direct native tools for single operations" "Reading `session` is always live"
          "never probe merely to refresh it" "Before `repl_eval` or lifecycle changes"
          "`repl_start`" "after verification, stop only those you" "External REPLs are"
          "`find_files`" "`rg`" "`struct_index` for code structure" "batch" "independent reads"
          "Inspect dependencies before adding them" "benchmark/profile identical workloads"
          "Prefer structural editing" "`struct_index`/`struct_patch`" "reach for it over text edits"
          "structural ops cannot express" "`format` stales anchors" "re-read first"
          "spent edits/reads first" "Create no unrequested"
          "without asking permission or offering optional" "Never expose or log secrets"
          "commit, push, publish" "Before every `session_fold`" "read `session[\"turn\"]`"
          "`N < session[\"turn\"]`" "never target current/future turns"
          "Fold completed prior-turn wire steps" "`ntr[tool_id]`" "breadcrumb lists accessors"
          "`await session_state()`" "session UID" "resolve it via `await sessions()`"
          "Route vis issues upstream" "`blockether/vis`" "open one only when requested"
          "Broader/newer folds replace covered breadcrumbs" "Lead with the answer or next action"
          "≤120 words" "≤3 bullets" "numbered bounded actions" "State completed results"
          "Step N/M complete. Next: ..." "location → cause → fix"
          "3 failed attempts at the same operation" "end with one action under 2 minutes"
          "never offer a menu"]]
        (expect (str/includes? text required)))
      (expect (= 2 (count (re-seq #"ntr\[tool_id\]" text))))
      (doseq
        [surplus ["Keep managed REPLs across turns" "Native results are `ntr[tool_id]`"
                  "Raise vis bugs/issues" "After 3 failures" "Complete tasks autonomously"
                  "canonical decision table"]]
        (expect (not (str/includes? text surplus))))))
  (it "advertises concise Python guidance and every auto-imported name"
      (let [text (#'prompt/sandbox-shims-prompt-block)]
        (expect (< (count text) 1000))
        (expect (not (str/includes? text "Not supported:")))
        (expect (str/includes? text "Auto-imported by `python_execution`"))
        (expect (str/includes? text "Preinstalled shims"))
        (expect (str/includes? text "doc(name)"))
        (doseq [name env-python/AUTO_IMPORTED_PYTHON_NAMES]
          (expect (str/includes? text (str "`" name "`")))))))

(defdescribe
  project-instructions-hoist-test
  (it "injects AGENTS.md contents as a dedicated PROJECT-INSTRUCTIONS system block"
      (with-redefs
        [agents/instructions
         (constantly {:found? true
                      :source :repo
                      :path "/tmp/repo/AGENTS.md"
                      :content
                      "PROJECT-RULE-FROM-AGENTS-MD\nreproduce -> inspect -> minimal change"})]
        (let
          [env {:extensions (atom [])}
           messages (prompt/assemble-stable-prompt-messages env {:active-extensions []})
           text (prompt/stable-prompt-text messages)]

          (expect (str/includes? text "PROJECT-INSTRUCTIONS"))
          (expect (str/includes? text "PROJECT-RULE-FROM-AGENTS-MD"))
          (expect (str/includes? text "/tmp/repo/AGENTS.md"))
          (expect (str/includes? text "CORE wins"))
          (expect (not (str/includes? text "contract (CTX shape, DONE pipeline, SANDBOX)")))
          ;; Send order: SYSTEM-PROMPT first, then PROJECT-INSTRUCTIONS.
          (expect (< (str/index-of text "SYSTEM-PROMPT")
                     (str/index-of text "PROJECT-INSTRUCTIONS"))))))
  (it "falls back to CLAUDE.md when AGENTS.md is absent and labels the source"
      (with-redefs
        [agents/instructions (constantly {:found? true
                                          :source :repo:claude-md-fallback
                                          :path "/tmp/repo/CLAUDE.md"
                                          :content "CLAUDE-FALLBACK-RULE"})]
        (let
          [env {:extensions (atom [])}
           messages (prompt/assemble-stable-prompt-messages env {:active-extensions []})
           text (prompt/stable-prompt-text messages)]

          (expect (str/includes? text "PROJECT-INSTRUCTIONS"))
          (expect (str/includes? text "CLAUDE-FALLBACK-RULE"))
          (expect (str/includes? text "CLAUDE.md")))))
  (it "emits no PROJECT-INSTRUCTIONS block when no guidance file is present"
      (with-redefs [agents/instructions (constantly {:found? false})]
        (let
          [env {:extensions (atom [])}
           messages (prompt/assemble-stable-prompt-messages env {:active-extensions []})
           text (prompt/stable-prompt-text messages)]

          (expect (not (str/includes? text "PROJECT-INSTRUCTIONS")))))))

(defdescribe extension-activation-test
             (it "assembles from precomputed active extensions without activating again"
                 (let
                   [calls
                    (atom 0)

                    ext
                    {:ext/name "test.activation"
                     :ext/activation-fn (fn [_]
                                          (swap! calls inc)
                                          true)
                     :ext/prompt-fn (constantly "Active prompt")}

                    env
                    {:extensions (atom [ext])}

                    active
                    (prompt/active-extensions env)]

                   (expect (= 1 @calls))
                   (prompt/assemble-stable-prompt-messages env {:active-extensions active})
                   (expect (= 1 @calls)))))

(defdescribe
  assemble-initial-messages-images-test
  "Image attachments turn the initial user message multimodal."
  (it "keeps text-only messages as a plain content string"
      (let
        [msgs
         (prompt/assemble-initial-messages {:stable-prompt-messages [{:role "system"
                                                                      :content "sys"}]
                                            :initial-user-content "hello"})

         user
         (last msgs)]

        (expect (= "user" (:role user)))
        (expect (string? (:content user)))
        (expect (str/includes? (:content user) "CURRENT-USER-MESSAGE"))
        (expect (not (str/includes? (:content user) "ATTACHED-IMAGES")))))
  (it "rides svar image blocks ahead of the text block and lists a manifest"
      (let
        [msgs
         (prompt/assemble-initial-messages {:stable-prompt-messages []
                                            :initial-user-content "what is on /tmp/shot.png?"
                                            :user-images [{:path "/tmp/shot.png"
                                                           :media-type "image/png"
                                                           :base64 "aGVsbG8="
                                                           :size 5
                                                           :size-label "5B"}]
                                            :skipped-images
                                            [{:path "/tmp/huge.png"
                                              :reason "6.0MB exceeds the 5.0MB attachment limit"}]})

         user
         (last msgs)

         blocks
         (:content user)]

        (expect (= "user" (:role user)))
        (expect (vector? blocks))
        ;; image block first (svar/user contract), text block last
        (expect (= "image_url" (:type (first blocks))))
        (expect (str/includes? (get-in (first blocks) [:image_url :url])
                               "data:image/png;base64,aGVsbG8="))
        (let [text (:text (last blocks))]
          (expect (str/includes? text "CURRENT-USER-MESSAGE"))
          (expect (str/includes? text "ATTACHED-IMAGES"))
          (expect (str/includes? text "/tmp/shot.png (image/png, 5B)"))
          (expect (str/includes? text "NOT attached"))
          (expect (str/includes? text "/tmp/huge.png")))))
  (it "omits image blocks for a text-only model and demotes them to the manifest"
      (let
        [msgs
         (prompt/assemble-initial-messages {:stable-prompt-messages []
                                            :initial-user-content "what is on /tmp/shot.png?"
                                            :vision? false
                                            :user-images [{:path "/tmp/shot.png"
                                                           :media-type "image/png"
                                                           :base64 "aGVsbG8="
                                                           :size 5
                                                           :size-label "5B"}]})

         user
         (last msgs)]

        ;; text-only target: plain string content, NO image_url block
        (expect (= "user" (:role user)))
        (expect (string? (:content user)))
        (expect (not (str/includes? (:content user) "image_url")))
        ;; the image is not silently dropped — it is demoted with a reason
        (expect (str/includes? (:content user) "ATTACHED-IMAGES"))
        (expect (str/includes? (:content user) "/tmp/shot.png"))
        (expect (str/includes? (:content user) "NOT attached"))
        (expect (str/includes? (:content user) "no vision"))))
  (it "omits the manifest when there is no user content at all"
      (let
        [msgs (prompt/assemble-initial-messages
                {:stable-prompt-messages [{:role "system" :content "sys"}]
                 :user-images
                 [{:path "p" :media-type "image/png" :base64 "eA==" :size 1 :size-label "1B"}]})]
        ;; no user message without initial-user-content — images can't ride alone
        (expect (= 1 (count msgs)))
        (expect (= "system" (:role (first msgs)))))))
