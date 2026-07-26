(ns com.blockether.vis.internal.prompt-test
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.agents :as agents]
            [com.blockether.vis.internal.env-python :as env-python]
            [com.blockether.vis.internal.extension :as extension]
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
      (expect (< (count text) 3800))
      (let
        [steps (mapv #(str/index-of text %)
                     ["use `grep` to locate unknown code" "known supported file" "`struct_index`"
                      "`cat` only the needed body" "`struct_patch`"])]
        (expect (every? some? steps))
        (expect (apply < steps)))
      (expect (str/includes? text "`grep` FIRST"))
      (expect (str/includes? text "`~/.vis/gateway/events/<id>.ndjson`"))
      (expect (str/includes? text "never grep `.`"))
      (expect (str/includes? text "never open with it"))
      (expect (str/includes? text "the first move, before docs"))
      (expect (str/includes? text "Use `cat` on a directory for an ls-style listing"))
      (expect (< (str/index-of text "`grep` FIRST") (str/index-of text "`vis_docs()`")))
      (doseq
        [heading ["## 1. Identity + Epistemic stance" "## 2. Execution surfaces" "## 3. Inspect"
                  "## 4. Edit + verify" "## 5. Act autonomously" "## 6. Manage context"
                  "## 7. Style and finish"]]
        (expect (str/includes? text heading)))
      (doseq [tool ["`struct_node`" "`struct_occurrences`" "`struct_rename`"]]
        (expect (not (str/includes? text tool))))
      (doseq
        [required
         ["Host project default" "`vis_docs()`" "runtime > source > docs > assumption"
          "Native descriptions and JSON Schemas are authoritative" "never guess contracts"
          "hard preconditions" "`python_execution`" "`await gather(...)` only for independent calls"
          "Direct native tools: single operations" "simple edits" "small fixed call sets"
          "dependent chains" "fan-out" "output shaping" "Call advertised native tools directly"
          "never preflight a visible native tool" "read-only `session`"
          "raw structured values, not UI-rendered text" "never infer fields from presentation"
          "never use `ctx` or `context`" "reproduce before editing"
          "rerun the same check after the fix" "batch independent reads" "Create no unrequested"
          "without asking permission or offering optional" "Never expose or log secrets"
          "commit, push, publish" "retry any blocked fold" "`session_fold` owns the mechanics"
          "preserve only decisions,\n  findings, edits"
          ;; REPL lifecycle: the agent must reuse managed REPLs and STOP the ones
          ;; it started, or every session leaks a JVM/interpreter child.
          "Before `repl_eval` or any REPL lifecycle change"
          "`session[\"resources\"][\"repls\"][language][dir]`" "Reuse managed REPLs across turns"
          "stop the ones you" "External REPLs are user-owned" "detach, never kill"
          ;; Anchor staleness: without this the agent chains small writes on
          ;; anchors invalidated by its own previous edit.
          "Anchors are positional and EVERY write stales them"
          "ONE atomic\n  `patch`/`struct_patch` call" "After any write, re-read"
          "never\n  reuse a pre-write anchor"
          "Lead with the answer. Be terse; depth only when earned."
          ;; End-of-turn teardown: without this the session leaks every REPL and
          ;; background shell the agent spawned.
          "Finish clean: stop every session resource you started" "`shell` op \"stop\""
          "nothing of yours running" "Confirm destructive actions."]]
        (expect (str/includes? text required)))
      ;; Python's native-result retrieval contract belongs in the execution-surface
      ;; guidance because it controls context shaping across every native tool.
      (expect (= 1 (count (re-seq #"ntr\[tool_id\]" text))))
      (doseq
        [surplus ["Keep managed REPLs across turns" "Native results are `ntr[tool_id]`"
                  "Raise vis bugs/issues" "After 3 failures" "Complete tasks autonomously"
                  "canonical decision table" "anything complicated"
                  ;; schema-owned or removed contracts stay out of the core prompt
                  "stales anchors" "benchmark/profile" "Route vis issues upstream"
                  "Before every `session_fold`" "`await session_state" "≤120 words"
                  "never offer a menu"]]
        (expect (not (str/includes? text surplus))))))
  (it "advertises exact model-facing Python capabilities, never internal shim ids"
      (let
        [shims [{:shim/name "attach"
                 :shim/globals ["vis_attach" "vis_attach_bytes" "vis_attachments"
                                "vis_read_attachment" "vis_reinspect_attachment"]}
                {:shim/name "fonttools" :shim/imports ["brotli" "fontTools"]}
                {:shim/name "numpy" :shim/imports ["numpy"]}
                {:shim/name "pil" :shim/imports ["PIL"]}
                {:shim/name "tzdata" :shim/imports ["zoneinfo"]}]]
        (with-redefs [extension/sandbox-shims (constantly shims)]
          (let [text (#'prompt/sandbox-shims-prompt-block nil)]
            (expect (< (count text) 1200))
            (expect (not (str/includes? text "Not supported:")))
            (expect (not (str/includes? text "apropos")))
            (expect (not (str/includes? text "doc(name)")))
            (expect (str/includes? text "Auto-imported by `python_execution`"))
            (expect (str/includes? text "Preinstalled shim modules"))
            (expect (str/includes? text "import numpy as np"))
            (expect (str/includes? text "never auto-created"))
            (doseq [module ["PIL" "brotli" "fontTools" "numpy" "zoneinfo"]]
              (expect (str/includes? text (str "`" module "`"))))
            (expect (str/includes? text "Prebound shim globals"))
            (doseq
              [global ["vis_attach" "vis_attach_bytes" "vis_attachments" "vis_read_attachment"
                       "vis_reinspect_attachment"]]
              (expect (str/includes? text (str "`" global "`"))))
            (expect (not (str/includes? text "`attach`")))
            ;; No shell layer active ⇒ no shell sentence.
            (expect (not (str/includes? text "subprocess")))
            (doseq [name env-python/AUTO_IMPORTED_PYTHON_NAMES]
              (expect (str/includes? text (str "`" name "`"))))))))
  (it "names POSIX routing without duplicating the shell contract"
      ;; Invocation syntax belongs to the shell symbol docs; this supplemental
      ;; block only exposes otherwise-undiscoverable compatibility routing.
      (let [text (#'prompt/sandbox-shims-prompt-block [{:ext/name "foundation-shell"}])]
        (expect (< (count text) 1500))
        (expect (str/includes? text "active `shell` tool"))
        (expect (str/includes? text "subprocess"))
        (expect (str/includes? text "os.system"))
        (expect (str/includes? text "os.popen"))
        (expect (not (str/includes? text "shell(")))
        (expect (not (str/includes? text "\"id\""))))))

(defdescribe
  project-instructions-hoist-test
  (it "injects AGENTS.md contents as a dedicated PROJECT-INSTRUCTIONS system block"
      (with-redefs
        [agents/instructions
         (constantly {:found? true
                      :source :repo
                      :path (str (System/getProperty "user.home") "/repo/AGENTS.md")
                      :content
                      "PROJECT-RULE-FROM-AGENTS-MD\nreproduce -> inspect -> minimal change"})]
        (let
          [env {:extensions (atom [])}
           messages (prompt/assemble-stable-prompt-messages env {:active-extensions []})
           text (prompt/stable-prompt-text messages)]

          (expect (str/includes? text "PROJECT-INSTRUCTIONS"))
          (expect (str/includes? text "PROJECT-RULE-FROM-AGENTS-MD"))
          (expect (str/includes? text "~/repo/AGENTS.md"))
          (expect (not (str/includes? text
                                      (str (System/getProperty "user.home") "/repo/AGENTS.md"))))
          (expect (str/includes? text "CORE wins"))
          (expect (not (str/includes? text "contract (CTX shape, DONE pipeline, SANDBOX)")))
          ;; Send order: SYSTEM-PROMPT first, then PROJECT-INSTRUCTIONS.
          (expect (< (str/index-of text "SYSTEM-PROMPT")
                     (str/index-of text "PROJECT-INSTRUCTIONS"))))))
  (it "keeps added-folder rules path-scoped and renders every provenance path"
      (with-redefs
        [agents/instructions
         (constantly {:found? true
                      :files [{:scope :project
                               :source :agents-md
                               :path (str (System/getProperty "user.home") "/vis/AGENTS.md")
                               :content "VIS-RULE"}
                              {:scope :extra-root
                               :source :agents-md
                               :path (str (System/getProperty "user.home") "/spel/AGENTS.md")
                               :content "SPEL-RULE"}]})]
        (let
          [env {:extensions (atom [])}
           messages (prompt/assemble-stable-prompt-messages env {:active-extensions []})
           text (prompt/stable-prompt-text messages)]

          (expect (str/includes? text "AGENTS.md (workspace root) — ~/vis/AGENTS.md"))
          (expect (str/includes? text "AGENTS.md (added folder) — ~/spel/AGENTS.md"))
          (expect
            (str/includes?
              text
              "Added-folder guidance applies only to files/actions beneath its listed folder"))
          (expect (str/includes? text "cannot override primary-workspace guidance elsewhere")))))
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

(defdescribe resume-message-cache-stability-test
             (it "appends each completed turn as its own stable message"
                 (let
                   [entry
                    (fn [n]
                      {:turn n :user-request (str "q" n) :answer (str "a" n) :results []})

                    assemble
                    (fn [prior current turn]
                      (prompt/assemble-initial-messages
                        {:stable-prompt-messages [{:role "system" :content "stable"}]
                         :previous-turn-context prior
                         :turn-context (str "session[\"turn\"] = " turn)
                         :initial-user-content current}))

                    t3
                    (assemble [(entry 1) (entry 2)] "q3" 3)

                    t4
                    (assemble [(entry 1) (entry 2) (entry 3)] "q4" 4)]

                   (expect (= (vec (butlast t3)) (subvec t4 0 (dec (count t3)))))
                   (expect (str/includes? (:content (last t4)) ";; -- TURN-SYSTEM-CONTEXT --"))
                   (expect (str/includes? (:content (last t4)) "session[\"turn\"] = 4"))))
             (it "renders one checkpoint message without covered Q/A"
                 (let
                   [messages
                    (prompt/assemble-initial-messages
                      {:previous-turn-context
                       [{:checkpoint? true :turns [1 2] :gist "durable state"}]
                       :turn-context "session[\"turn\"] = 3"
                       :initial-user-content "continue"})

                    prior
                    (:content (first messages))]

                   (expect (= 2 (count messages)))
                   (expect (str/includes? prior "folded turns 1, 2"))
                   (expect (str/includes? prior "durable state"))
                   (expect (not (str/includes? prior "user asked:"))))))
