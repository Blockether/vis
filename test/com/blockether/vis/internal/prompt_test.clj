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

(defdescribe
  cli-autonomous-override-test
  (it "drops the candidate approval STOP for the non-interactive :cli channel only"
      (let
        [text-for
         (fn [ch]
           (-> (prompt/assemble-stable-prompt-messages {:channel ch} {:active-extensions []})
               prompt/stable-prompt-text))

         marker
         "NON-INTERACTIVE ONE-SHOT RUN"]

        ;; :cli (headless one-shot — no approver) gets the override
        (expect (str/includes? (text-for :cli) marker))
        (expect (str/includes? (text-for :cli) "Keep working to a finished prose answer"))
        (expect (str/includes? (text-for :cli) "Leave destructive or irreversible work"))
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
      ;; Context safety is worth a small fixed prompt cost; keep the whole core below 4.7k.
      ;; The ratchet must never squeeze out §7's teardown rule again: compressing it to a
      ;; bare "finish clean" is how sessions started leaking REPLs. The budget moved 4.5k →
      ;; 4.7k exactly once, when REPL-first reproduction and the "unverified until a test
      ;; covers it" rule landed: those rules pay for themselves, and paying for them by
      ;; shaving other rules' wording is the squeeze this lock exists to stop.
      ;; 4.7k → 4.75k exactly once more, when the merged `shell`/`fs` mega-tools split into
      ;; named verbs: §2's non-blocking rule and §3's five filesystem names are what stop the
      ;; model guessing an `op` discriminator that no longer exists.
      (expect (< (count text) 4750))
      (let
        [steps (mapv #(str/index-of text %)
                     ["`grep` locates unknown code" "`struct_index` every known file"
                      "read bodies in ONE call" "`struct_nodes`" "`cat` `ranges`"
                      "`struct_patch`"])]
        (expect (every? some? steps))
        (expect (apply < steps)))
      (expect (str/includes? text "`grep` FIRST"))
      ;; Session introspection (gateway event journals, session_state) is toggle-
      ;; gated and lives in the `foundation-introspection` extension prompt, NOT core.
      (expect (not (str/includes? text "`~/.vis/gateway/events/<id>.ndjson`")))
      (expect (str/includes? text "scoped to real paths"))
      (expect (str/includes? text "only for product questions"))
      (expect (str/includes? text "locates unknown code"))
      (expect (str/includes? text "**Filesystem work goes through native tools**"))
      ;; The routing rule NAMES the whole filesystem surface: an omitted verb silently
      ;; re-opens the `mkdir -p`/`test -f` shell reflex it exists to close.
      (doseq [verb ["`copy`" "`move`" "`delete`" "`create_directory`" "`file_exists`"]]
        (expect (str/includes? text verb)))
      (expect (str/includes? text "`shell_run` runs programs"))
      (expect (< (str/index-of text "`grep` FIRST") (str/index-of text "`vis_docs()`")))
      (doseq
        [heading ["## 1. Identity + Epistemic stance" "## 2. Execution surfaces" "## 3. Inspect"
                  "## 4. Edit + verify" "## 5. Act autonomously" "## 6. Manage context"
                  "## 7. Style and finish"]]
        (expect (str/includes? text heading)))
      (doseq [tool ["`struct_occurrences`" "`struct_rename`"]]
        (expect (not (str/includes? text tool))))
      ;; struct_nodes IS named in the core prompt: reading a definition's SOURCE is a
      ;; first-class step of the code workflow, not a specialist tool.
      (expect (str/includes? text "`struct_nodes`"))
      (doseq
        [required
         ["Host project default" "`vis_docs()`" "runtime > source > docs > assumption"
          "Native descriptions and JSON Schemas are authoritative" "follow the documented contract"
          "hard preconditions" "`python_execution`" "`await gather(...)` only for independent calls"
          "Direct native tools: single operations" "default for most Python/data work"
          ;; No tool blocks on the model's behalf: the old `shell` op `wait`/`until`
          ;; is gone, so core routes to background + a poll the model can read.
          "Nothing blocks for you" "`shell_background`" "poll `shell_logs`"
          "functions that accept or return\n  callables"
          "NEVER paste a near-identical loop or block twice" "define once and reuse"
          "second occurrence factor it out and call it" "raw data, not rendered text"
          "Use `ntr[key]" "# saved:" "`ntr.describe()`" "Inspect shape before indexing"
          "status only when absent or stale" "tests-only work starts with `run_tests`"
          "interactive work uses `repl_eval`" "Keep reproduction as a suite test"
          "rerun after the fix" "unverified until a test covers it" "BATCH every tool"
          "Write only files the task asked" "Commit, push, publish" "Treat context as a budget"
          "at most two targeted" "named unresolved decision blocks the edit"
          "no repeated search/read" "Fold obsolete settled work" "one broad `through`/range fold"
          "When edit-ready and headroom permits, patch before folding"
          "Before unavoidable folds, checkpoint"
          "paths/symbols, hypothesis, edit/test, and dirty files"
          "decisions, verification, recovery IDs" "exact paths; confirm reduction"
          "Fold only settled steps through the last completed scope"]]
        (expect (str/includes? text required)))
      ;; Regression, user report: blanket resource cleanup stopped a healthy dev server
      ;; that the user had explicitly asked the agent to open and keep available.
      (doseq
        [required ["Finish clean: stop managed REPLs you started"
                   "temporary implementation or test machinery"
                   "healthy service the user asked you to run is persistent user infrastructure"
                   "leave it running" "across turns and final answers"
                   "Confirm destructive actions."]]
        (expect (str/includes? text required)))
      (expect (not (str/includes? text "stop every session resource you started")))
      ;; Python's native-result retrieval contract belongs in the execution-surface
      ;; guidance because it controls context shaping across every native tool.
      (expect (= 1 (count (re-seq #"ntr\[key\]" text))))
      (doseq
        [surplus ["Keep managed REPLs across turns" "Native results are `ntr[tool_id]`"
                  "Raise vis bugs/issues" "After 3 failures" "Complete tasks autonomously"
                  "canonical decision table" "anything complicated"
                  ;; schema-owned or removed contracts stay out of the core prompt
                  "stales anchors" "benchmark/profile" "Route vis issues upstream"
                  "Before every `session_fold`" "`await session_state" "≤120 words"
                  "never offer a menu"
                  ;; The sleep/poll prohibition is OWNED by `python_execution`'s own
                  ;; description (pinned in loop_test). §1 already makes native
                  ;; descriptions authoritative, so a core copy is dead weight: the
                  ;; core keeps only the routing rule (background shells → `shell`
                  ;; op `wait`), never the tool-local prohibition.
                  "`time.sleep`" "`asyncio.sleep`" "poll in Python"]]
        (expect (not (str/includes? text surplus))))))
  (it
    "advertises exact model-facing Python capabilities, never internal shim ids"
    (let
      [shims [{:shim/name "attach"
               :shim/globals ["vis_attach" "vis_attachments" "vis_attachment"
                              "vis_read_attachment" "vis_reinspect_attachment"]
               :shim/description
               "Persist artifacts as durable attachments. Vis-native; no upstream library."}
              {:shim/name "fonttools" :shim/imports ["brotli" "fontTools"]}
              {:shim/name "numpy"
               :shim/imports ["numpy"]
               :shim/description "Pure-Python `numpy` subset. Not supported: eig/svd/qr."}
              {:shim/name "pil" :shim/imports ["PIL"]}
              {:shim/name "tzdata" :shim/imports ["zoneinfo"]}]]
      (with-redefs [extension/sandbox-shims (constantly shims)]
        (let [text (#'prompt/sandbox-shims-prompt-block nil)]
          (expect (< (count text) 1800))
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
            [global ["vis_attach" "vis_attachments" "vis_attachment"
                     "vis_read_attachment" "vis_reinspect_attachment"]]
            (expect (str/includes? text (str "`" global "`"))))
          (expect (not (str/includes? text "`attach`")))
          ;; NAMES ALONE ARE A TRAP. Every shim is a reimplementation, so the surface
          ;; it supports and the APIs it refuses must travel WITH the name: reading
          ;; only `numpy` in this block, the model wrote against the real numpy and
          ;; first learned about `NotImplementedError` from a failed call.
          (expect (str/includes? text "REIMPLEMENTATION"))
          (expect (str/includes?
                    text
                    "- `numpy`: Pure-Python `numpy` subset. Not supported: eig/svd/qr."))
          (expect (str/includes? text
                                 (str "- `vis_attach`, `vis_attachments`, "
                                      "`vis_attachment`, `vis_read_attachment`, "
                                      "`vis_reinspect_attachment`: Persist artifacts")))
          ;; A shim that documents nothing contributes no empty bullet.
          (expect (not (str/includes? text "- `brotli`")))
          ;; With no shell layer active the block must SAY the process surface is
          ;; gone: silence read as "maybe try `subprocess`", and every attempt then
          ;; died on an opaque spawn failure instead of being ruled out up front.
          (expect (str/includes? text "Shell commands are DISABLED"))
          (expect (str/includes? text "NOT allowed"))
          (expect (str/includes? text "No external process can run here"))
          (doseq [banned ["subprocess" "os.system" "os.popen"]]
            (expect (str/includes? text banned)))
          (expect (not (str/includes? text "route through the active")))
          (doseq [name env-python/AUTO_IMPORTED_PYTHON_NAMES]
            (expect (str/includes? text (str "`" name "`"))))))))
  (it "names POSIX routing without duplicating the shell contract"
      ;; Invocation syntax belongs to the shell symbol docs; this supplemental
      ;; block only exposes otherwise-undiscoverable compatibility routing.
      (let [text (#'prompt/sandbox-shims-prompt-block [{:ext/name "foundation-shell"}])]
        (expect (str/includes? text "active `shell_run`/`shell_background` shell tools"))
        (expect (not (str/includes? text "DISABLED")))
        (expect (str/includes? text "subprocess"))
        (expect (str/includes? text "os.system"))
        (expect (str/includes? text "os.popen"))
        (expect (not (str/includes? text "shell(")))
        (expect (not (str/includes? text "\"id\"")))))
  (it "carries every REGISTERED shim's own limits into the prompt, inside budget"
      ;; The registry itself, not a fixture: the shim the model actually gets has to
      ;; be the shim the prompt describes, or the description is dead metadata.
      (let
        [text
         (#'prompt/sandbox-shims-prompt-block [{:ext/name "foundation-shell"}])

         described
         (filter :shim/description (extension/sandbox-shims))]

        (expect (seq described))
        (doseq [shim described]
          (expect (str/includes? text (str/trim (:shim/description shim)))))
        ;; One stable block worth roughly 2.5k tokens. A description that grows
        ;; without bound is a context regression, not documentation.
        (expect (< (count text) 16000)))))

(defdescribe
  project-instructions-hoist-test
  (it
    "injects primary guidance as a dedicated PROJECT-INSTRUCTIONS system block"
    (with-redefs
      [agents/primary-instructions
       (constantly {:found? true
                    :source :repo
                    :path (str (System/getProperty "user.home") "/repo/AGENTS.md")
                    :content "PROJECT-RULE-FROM-AGENTS-MD\nreproduce -> inspect -> minimal change"})

       agents/added-root-guidance-index
       (constantly [])]

      (let
        [env
         {:extensions (atom [])}

         messages
         (prompt/assemble-stable-prompt-messages env {:active-extensions []})

         text
         (prompt/stable-prompt-text messages)]

        (expect (str/includes? text "PROJECT-INSTRUCTIONS"))
        (expect (str/includes? text "PROJECT-RULE-FROM-AGENTS-MD"))
        (expect (str/includes? text "~/repo/AGENTS.md"))
        (expect (not (str/includes? text (str (System/getProperty "user.home") "/repo/AGENTS.md"))))
        (expect (str/includes? text "CORE wins"))
        (expect (< (str/index-of text "SYSTEM-PROMPT")
                   (str/index-of text "PROJECT-INSTRUCTIONS"))))))
  (it "indexes added-root guidance without injecting its contents"
      (with-redefs
        [agents/primary-instructions
         (constantly {:found? true
                      :files [{:scope :project
                               :source :agents-md
                               :path (str (System/getProperty "user.home") "/vis/AGENTS.md")
                               :content "VIS-RULE"}]})

         agents/added-root-guidance-index
         (constantly [{:root (str (System/getProperty "user.home") "/spel")
                       :path (str (System/getProperty "user.home") "/spel/AGENTS.md")
                       :source :agents-md}])]

        (let
          [env
           {:extensions (atom [])}

           messages
           (prompt/assemble-stable-prompt-messages env {:active-extensions []})

           text
           (prompt/stable-prompt-text messages)]

          (expect (str/includes? text "VIS-RULE"))
          (expect (str/includes? text "~/spel — guidance: ~/spel/AGENTS.md"))
          (expect (str/includes? text "guidance is not loaded yet"))
          (expect (str/includes? text "read its exact guidance path with `cat`"))
          (expect (not (str/includes? text "SPEL-RULE"))))))
  (it "falls back to CLAUDE.md when primary AGENTS.md is absent"
      (with-redefs
        [agents/primary-instructions
         (constantly {:found? true
                      :source :repo:claude-md-fallback
                      :path "/tmp/repo/CLAUDE.md"
                      :content "CLAUDE-FALLBACK-RULE"})

         agents/added-root-guidance-index
         (constantly [])]

        (let
          [text (-> (prompt/assemble-stable-prompt-messages {:extensions (atom [])}
                                                            {:active-extensions []})
                    prompt/stable-prompt-text)]
          (expect (str/includes? text "CLAUDE-FALLBACK-RULE"))
          (expect (str/includes? text "CLAUDE.md")))))
  (it "emits no PROJECT-INSTRUCTIONS block when no guidance is available"
      (with-redefs
        [agents/primary-instructions
         (constantly {:found? false})

         agents/added-root-guidance-index
         (constantly [])]

        (let
          [text (-> (prompt/assemble-stable-prompt-messages {:extensions (atom [])}
                                                            {:active-extensions []})
                    prompt/stable-prompt-text)]
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

;; 1x1 red PNG — REAL pixels: the send gate decodes every image block it emits,
;; so a fake base64 payload is (correctly) refused and never reaches the wire.
(def ^:private tiny-png-b64
  "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mP8z8BQDwAEhQGAhKmMIQAAAABJRU5ErkJggg==")

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
                                                           :base64 tiny-png-b64
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
                               (str "data:image/png;base64," tiny-png-b64)))
        (let [text (:text (last blocks))]
          (expect (str/includes? text "CURRENT-USER-MESSAGE"))
          (expect (str/includes? text "ATTACHED-IMAGES"))
          (expect (str/includes? text "/tmp/shot.png (image/png,"))
          (expect (str/includes? text "NOT attached"))
          (expect (str/includes? text "/tmp/huge.png")))))
  (it "drops an image no decoder can read and NAMES it instead of sending a 400"
      ;; A perfect PNG signature + IHDR over an unreadable stream: wire-legal to
      ;; any sniff, and a `Could not process image` 400 that would replay on
      ;; every later turn of the session.
      (let
        [corrupt
         (.encodeToString (java.util.Base64/getEncoder)
                          (byte-array (concat (take 33
                                                    (.decode (java.util.Base64/getDecoder)
                                                             ^String tiny-png-b64))
                                              (repeat 24 0))))

         msgs
         (prompt/assemble-initial-messages {:stable-prompt-messages []
                                            :initial-user-content "look"
                                            :user-images [{:path "/tmp/dot.png"
                                                           :media-type "image/png"
                                                           :base64 corrupt
                                                           :size 57
                                                           :size-label "57B"}]})

         user
         (last msgs)]

        (expect (string? (:content user)))
        (expect (str/includes? (:content user) "NOT attached"))
        (expect (str/includes? (:content user) "/tmp/dot.png"))
        (expect (str/includes? (:content user) "could not be decoded"))))
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

(defdescribe
  resume-message-cache-stability-test
  (it "appends each completed turn as its own stable message"
      (let
        [entry
         (fn [n]
           {:turn n :user-request (str "q" n) :answer (str "a" n) :results []})

         assemble
         (fn [prior current turn]
           (prompt/assemble-initial-messages {:stable-prompt-messages [{:role "system"
                                                                        :content "stable"}]
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
         (prompt/assemble-initial-messages {:previous-turn-context
                                            [{:checkpoint? true :turns [1 2] :gist "durable state"}]
                                            :turn-context "session[\"turn\"] = 3"
                                            :initial-user-content "continue"})

         prior
         (:content (first messages))]

        (expect (= 2 (count messages)))
        (expect (str/includes? prior "folded turns 1, 2"))
        (expect (str/includes? prior "durable state"))
        (expect (not (str/includes? prior "user asked:")))))
  (it "renders cancelled work as settled history with a model-visible abort marker"
      (let
        [block (prompt/previous-turn-context-block [{:turn 1
                                                     :user-request "inspect and fix"
                                                     :cancelled? true
                                                     :results [{:scope "t1/i1/f1"
                                                                :src "cat(src)"}]}])]
        (expect (str/includes? block "cat(src)"))
        (expect (str/includes? block "<turn_cancelled>"))
        (expect (str/includes? block "persisted results remain valid; do not repeat settled work"))
        (expect (not (str/includes? block "INTERRUPTED before it finished"))))))
