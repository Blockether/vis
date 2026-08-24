(ns com.blockether.vis.internal.prompt-test
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.agents :as agents]
            [com.blockether.vis.internal.env-python :as env-python]
            [com.blockether.vis.internal.extension :as extension]
            [com.blockether.vis.internal.manifest :as manifest]
            [com.blockether.vis.internal.prompt :as prompt]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe prompt-assembly-test
             (it "normalizes core addendum and extension prompt text"
                 (let [ext
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
      (let [text-for
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
  ;; With one tool there is no schema to be authoritative: a capability's OWN
  ;; document is the contract, and the core prompt has to say where it lives or
  ;; the model invents a call shape instead of pulling one.
  (it "points authority at the document a capability carries"
      (let [text (prompt/build-system-prompt {})]
        (expect (str/includes? text "`doc(name)` returns"))
        (expect (str/includes? text "the authoritative contract"))
        (expect (str/includes? text "obey its stated preconditions"))
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
      ;; The ceiling has never moved UP for a rewrite: when eighteen tools became one,
      ;; stale discovery and JSON-Schema prose left §1, §2 lost native-vs-Python routing,
      ;; and what replaced them — "ONE call exists", the discovery contract and the folding
      ;; truth — had to fit UNDER the existing budget. It does, at 4 729 chars.
      ;; The win of that change is not here; it is the provider `:tools` payload, which
      ;; went from eighteen JSON Schemas to one.
      ;; 4.75k → 4.8k exactly once more, for the fifty characters that make §6 executable:
      ;; the section ordered a fold and named no callable, so `fold_session`'s NAME and call
      ;; shape now ride inline. A rule the model cannot execute costs its whole section.
      ;; 4.8k → 5k exactly once more, for the budget line: `session_utilization` reports
      ;; `saturation`/`headroom_tokens` against the HARD per-call limit, while every fold
      ;; trigger — the `hint` ladder, the breadcrumb's `% of budget` — is priced against
      ;; `auto_compress_above`. On a 1M-window model 150k of the 200k operating budget reads
      ;; as `saturation 15%, headroom 850k`, so "watch `session[\"utilization\"]`" pointed the
      ;; model at the one pair of numbers that stays calm while the budget empties.
      ;; 5k → 5.5k exactly once more, for the two verbs that give an edit a COORDINATE:
      ;; `cat` mints `line:hash` and `patch` spends it. §3 previously ordered the opposite —
      ;; "CHANGING the tree is plain Python" — and that sentence is gone, but naming both
      ;; verbs, the anchor format the model has to recognize, and the batch shape ONE
      ;; call takes does not fit in what it freed. Measured against 40 real
      ;; sessions, the instruction it replaces cost 48% of ALL block characters in blocks
      ;; that write a file, 80% of it the old text quoted back; this is the cheaper order.
      ;; 5.5k → 5.9k exactly once more, for §2's shape rule. The sandbox is a PROGRAM the
      ;; session keeps: roots bound once off `session`, results in named variables, one helper
      ;; called again instead of a near-identical block pasted twice, and — when the chore
      ;; outlives the turn — a proposed Python extension in `.vis/extensions/*.py`. Sessions
      ;; that lacked it retyped absolute paths per block, redefined the same helper each block,
      ;; and re-derived what an earlier block had already computed. Naming the `session` key,
      ;; the extension path and `doc("extending")` is what makes the rule executable rather
      ;; than a slogan; the paragraph it replaced ("A result is an ordinary Python value") is
      ;; folded into it. The budget did NOT move for the session-scope correction that
      ;; followed (a `def` lives for the whole session; `session` itself is rebuilt before
      ;; every block and cannot hold one): it was paid for by deleting the glossary
      ;; parenthetical after "higher-order helper", and lands at 5 871.
      ;; 5.9k → 6.05k for the fold KEY grammar. `fold_session` takes a key and a gist and
      ;; nothing else, but §6 named only the verb: the key shape had to be remembered, and a
      ;; guessed one folds nothing while the card still says `folded …`. Spelling the six key
      ;; forms inline is what makes the ordered fold executable; it is paid for by dropping
      ;; "Folding changes rendering, not storage" (the NOT-re-readable clause carries it), and
      ;; lands at 5 985.
      ;; 6.05k → 6.1k for the call shapes §3 was missing. The section named its code
      ;; verbs but spelled a callable form for only two (`cat`, `patch`); the ones whose
      ;; contract is a single options dict were named bare, so their shape had to be
      ;; recalled or pulled mid-edit. Measured over 179 gateway journals (1 006 sandbox
      ;; blocks, 326 of them calling a code verb): `grep` called with a bare string,
      ;; `cat` with a dict, and `patch` edits keyed `from_anchor`/`to_anchor` — a key no
      ;; release ever had.
      ;; Each is a refused call and a wasted round trip; the literal dicts cost 71 characters
      ;; and land at 6 056.
      ;; 6.1k → 6.2k for the one line that makes a helper DESCRIBE itself. §2 already ordered the
      ;; model to keep helpers and read them back, but 45 of the 146 documents `apropos` could
      ;; answer were its own `def`s carrying no text at all: an empty gist, a `doc(name)` page
      ;; that was a bare header, and nothing a described ask could match. A docstring is the whole
      ;; of that fix — first line to the listing, the rest to the page — and the rule had to name
      ;; where that line SHOWS UP, or it reads as style advice. It lands at 6 152.
      ;; 6.2k → 6.35k to distinguish questions from implementation requests before any tool rule.
      ;; Answering directly avoids turning an informational question into an unsolicited code change.
      (expect (< (count text) 6350))
      (let [steps (mapv #(str/index-of text %)
                        ["`grep` locates unknown code" "a hit IS a `patch` argument"
                         "`patch(path, edits)`"])]
        (expect (every? some? steps))
        (expect (apply < steps)))
      ;; Regression, user report: a section that ORDERS a verb has to say how it is CALLED.
      ;; Every code verb §3 names carries its literal call shape, and the options-dict ones
      ;; name the keys inside it — the shape is read, never remembered or looked up.
      (doseq [shape ["`grep({\"query\": [needles], \"paths\": [scopes]})`" "`cat(path, start, end)`"
                     "`patch(path, edits)`" "`[{\"from\": a, \"to\": b, \"replace\": text}]`"]]
        (expect (str/includes? text shape)))
      (expect
        (str/includes?
          text
          "When the user asks a question, answer the question. Do not start coding. Use tools or scripts only when you need more information for the answer."))
      (expect (< (str/index-of text "When the user asks a question")
                 (str/index-of text "`grep(...)` FIRST")))
      (expect (str/includes? text "`grep(...)` FIRST"))
      ;; A helper the model wrote is the only document it can author mid-session, so the rule that
      ;; orders one has to name what its docstring BECOMES — a gist, a page, and a way to be found.
      (expect (str/includes? text "One docstring line is its `defs()` gist"))
      ;; The verification rule must name a call the language surface accepts: a lone
      ;; string is the PAYLOAD, not a language, so `run_tests("python")` would run the
      ;; workspace's primary pack instead of the python one.
      (expect (str/includes? text "run_tests({\"language\": \"python\"})"))
      ;; Session introspection is toggle-gated in foundation-core's dynamic fragment,
      ;; never copied into the static engine prompt.
      (expect (not (str/includes? text "`~/.vis/gateway/events/<id>.ndjson`")))
      (expect (str/includes? text "scoped to real paths"))
      (expect (str/includes? text "locates unknown code"))
      (expect (str/includes? text "**Filesystem work is Python**"))
      ;; The routing rule sends every filesystem CHANGE to Python; naming the deleted
      ;; native verbs again would re-open the `mkdir -p`/`test -f` reflex it exists to close.
      (doseq [verb ["`copy`" "`move`" "`delete`" "`create_directory`" "`file_exists`"]]
        (expect (not (str/includes? text verb))))
      ;; The routing rule survived the arrival of `patch`: a filesystem CHANGE that is not
      ;; an ADDRESSED edit — create, move, delete — is still Python, never a native verb.
      (expect (str/includes? text "creating/moving/deleting is plain Python"))
      ;; The shell is a Python call now, not a native tool: the core must say WHERE it lives.
      (expect (str/includes? text "`shell(...)` runs programs"))
      (expect (str/includes? text "No shell TOOL"))
      (expect (< (str/index-of text "`grep(...)` FIRST")
                 (str/index-of text "`apropos(pattern)` filters SYMBOL names")))
      (doseq [heading ["## 1. Identity + Epistemic stance" "## 2. Execution surfaces"
                       "## 3. Inspect" "## 4. Edit + verify" "## 5. Act autonomously"
                       "## 6. Manage context" "## 7. Style and finish"]]
        (expect (str/includes? text heading)))
      (doseq [required ["Host project default" "`apropos(pattern)` filters SYMBOL names"
                        "`doc(name)` returns" "runtime > source > docs > assumption"
                        "obey its stated preconditions" "the curated index"
                        "A skill is one of those documents" "`python_execution`" "ONE call exists"
                        "there is no tool to choose" "Batch independent work in ONE block"
                        "`await gather(...)` for"
                        ;; No tool blocks on the model's behalf: the old `shell` op `wait`/`until`
                        ;; is gone, so core routes to background + a poll the model can read.
                        ;; Regression, issue #137: the handle line spelled `sh.type()` among the
                        ;; status accessors, so following it verbatim raised a TypeError —
                        ;; `type` SENDS keystrokes and its text argument is required.
                        "No shell TOOL" "`sh.logs(-50)`" "`sh.wait(s)`" "`sh.type(\"y\")`"
                        "NEVER paste a near-identical loop or block twice" "Define once and reuse"
                        "factor it out on the second occurrence" "keep results in"
                        "a value you never printed costs nothing" "gone when the block ends"
                        "Inspect shape before indexing" "nothing lists one for you"
                        "tests-only work starts with `run_tests`"
                        "interactive work uses `repl_eval`" "Keep reproduction as a suite test"
                        "rerun after the fix" "unverified until a test covers it"
                        "BATCH inside one block" "Write only files the task asked"
                        "Commit, push, publish" "Treat context as a budget" "at most two targeted"
                        ;; Regression, user report: cross-validating §6 against the runtime. The
                        ;; utilization line named no field, and the two fields a model reads first
                        ;; (`saturation`, `headroom_tokens`) are priced against the hard per-call
                        ;; limit — calm at 15% while `over-budget-hint` is already saying FOLD SOON.
                        ;; Name the ratio the fold triggers actually use.
                        "pressure is `last_request_tokens`" "`auto_compress_above`"
                        "`saturation`/`headroom_tokens` price" "`hint` only arms at 75% of it"
                        ;; The rolling cache readout is only worth its keys if the reader knows a
                        ;; collapsed rate means a MOVED endpoint, not a grown request (issue #154).
                        "`cache_hit_rate` is the share of recent input"
                        ;; `session_drop` is gone: omitting the gist IS the discard, and a model
                        ;; that does not know that writes a useless gist instead of dropping.
                        "the gist discards outright" "named unresolved decision blocks the edit"
                        "no repeated search/read"
                        ;; Regression, user report: sessions stopped folding. §6 ORDERED the fold
                        ;; but named no callable, so `fold_session` had to be remembered or
                        ;; rediscovered through `doc()` — every other verb in the core is named.
                        "`fold_session(key, gist)`" "Fold obsolete settled work"
                        ;; Regression, user report: a fold that "saved 0 tokens". §6 named the verb
                        ;; but not the KEY it takes, so the shape was guessed — a selector structure
                        ;; or a bare id that resolved to nothing. The key grammar is in the core now.
                        "the key is a STRING" "`\"-t2/i9\"` everything through it"
                        ;; Nothing stores a folded step for later: the gist is the whole survivor,
                        ;; and a prompt that hints otherwise buys a fold the model regrets.
                        "a folded step is NOT re-readable, so the gist is what survives"
                        "When edit-ready and headroom permits, edit before folding"
                        "Before unavoidable folds, checkpoint"
                        "paths/symbols, hypothesis, edit/test, and dirty files"
                        "Keep decisions, verification and" "exact paths; confirm reduction"
                        "Fold only settled steps through the last completed scope"]]
        (expect (str/includes? text required)))
      ;; Regression, user report: blanket resource cleanup stopped a healthy dev server
      ;; that the user had explicitly asked the agent to open and keep available.
      (doseq [required
              ["Finish clean: stop managed REPLs you started"
               "temporary implementation or test machinery"
               "healthy service the user asked you to run is persistent user infrastructure"
               "leave it running" "across turns and final answers" "Confirm destructive actions."]]
        (expect (str/includes? text required)))
      (expect (not (str/includes? text "stop every session resource you started")))
      ;; `ntr` is gone: with `python_execution` the only call, nothing stores a
      ;; result the model could re-read by coordinate, so the prompt must never
      ;; promise one again.
      (doseq [surplus ["Keep managed REPLs across turns" "ntr[" "# saved:" "ntr.describe()"
                       ;; Regression, issue #ctx-resources: live shells/REPLs left ctx entirely,
                       ;; so no prompt may send the model to a `session["resources"]` that is gone.
                       "session[\"resources\"]"
                       ;; The vocabulary of eighteen doors: naming any of it again re-opens
                       ;; the routing question that having ONE call exists to close.
                       "native tool" "Native tool" "JSON Schema" "advertised" "Direct native tools"
                       "Raise vis bugs/issues" "After 3 failures" "Complete tasks autonomously"
                       "canonical decision table" "anything complicated"
                       ;; schema-owned or removed contracts stay out of the core prompt
                       "stales anchors" "benchmark/profile" "Route vis issues upstream"
                       "Before every `fold_session`" "`await read_session" "≤120 words"
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
    (let [shims [{:shim/name "attachments"
                  :shim/globals ["attach" "list_attachments" "get_attachment" "read_attachment"
                                 "show_attachment"]}
                 {:shim/name "fonttools" :shim/imports ["brotli" "fontTools"]}
                 {:shim/name "numpy" :shim/imports ["numpy"]}
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
          (doseq [global ["attach" "list_attachments" "get_attachment" "read_attachment"
                          "show_attachment"]]
            (expect (str/includes? text (str "`" global "`"))))
          (expect (not (str/includes? text "`attachments`")))
          ;; NAMES ALONE WOULD BE A TRAP — every shim is a REIMPLEMENTATION — so the
          ;; block still says so, but it POINTS at the page instead of pushing one
          ;; hand-written bullet per shim (5.7 KB) into every request. `doc("numpy")`
          ;; answers from the module's own Python `__doc__` plus the members it lends.
          (expect (str/includes? text "REIMPLEMENTATION"))
          (expect (str/includes? text "doc(\"numpy\")"))
          (expect (str/includes? text "doc(\"numpy.linalg.solve\")"))
          (expect (not (str/includes? text "- `numpy`")))
          (expect (not (str/includes? text "- `brotli`")))
          ;; With no shell layer active the block must SAY the process surface is
          ;; gone: silence read as "maybe try `subprocess`", and every attempt then
          ;; died on an opaque spawn failure instead of being ruled out up front.
          ;; Verbatim from the ONE source, so the prompt cannot drift from what
          ;; `subprocess` raises and what an undriveable handle reports.
          (expect (str/includes? text (get env-python/PROCESS_SURFACE "off")))
          (expect (str/includes? text "Shell commands are DISABLED"))
          (expect (str/includes? text "nothing here can start a process"))
          (doseq [banned ["subprocess" "os.system" "os.popen"]]
            (expect (str/includes? text banned)))
          (expect (not (str/includes? text "route through the active")))
          (doseq [name env-python/AUTO_IMPORTED_PYTHON_NAMES]
            (expect (str/includes? text (str "`" name "`"))))))))
  (it "bans subprocess even with shell active, without duplicating the shell contract"
      ;; Invocation syntax belongs to the shell symbol docs; this supplemental
      ;; block only says that `subprocess` is not a second door to a process.
      (let [text (#'prompt/sandbox-shims-prompt-block
                  [{:ext/engine {:ext.engine/symbols [{:ext.symbol/symbol 'shell}]}}])]
        (expect (str/includes? text (get env-python/PROCESS_SURFACE "ban")))
        (expect (str/includes? text "never spawn"))
        (expect (str/includes? text "`shell` verb"))
        (expect (not (str/includes? text "DISABLED")))
        (expect (str/includes? text "subprocess"))
        (expect (str/includes? text "os.system"))
        (expect (str/includes? text "os.popen"))
        (expect (not (str/includes? text "shell(")))
        (expect (not (str/includes? text "\"id\"")))))
  (it "pushes every REGISTERED shim's names, and its prose nowhere"
      ;; The registry itself, not a fixture: the model gets exactly these names. The
      ;; prose that used to ride along — one hand-written bullet per shim — now lives
      ;; in each module's own Python `__doc__`, is harvested into
      ;; the manifest-listed shim apropos resource, and is PULLED by `doc(name)`. A block
      ;; that grows back into prose is a context regression, not documentation.
      (let [text
            (#'prompt/sandbox-shims-prompt-block
             [{:ext/engine {:ext.engine/symbols [{:ext.symbol/symbol 'shell}]}}])

            shims
            (extension/sandbox-shims)]

        (expect (seq shims))
        (doseq [nm (mapcat #(concat (:shim/imports %) (:shim/globals %)) shims)]
          (expect (str/includes? text (str "`" nm "`"))
                  (str nm " is installed but the prompt never names it")))
        (doseq [shim (filter :shim/docs shims)]
          (expect (not (str/includes? text (subs (:shim/docs shim) 0 40)))
                  (str (:shim/name shim) " pushes its pulled page into every request")))
        (expect (< (count text) 1500)))))

(defdescribe
  project-instructions-hoist-test
  (it "injects primary guidance as a dedicated PROJECT-INSTRUCTIONS system block"
      (with-redefs [agents/primary-instructions
                    (constantly
                      {:found? true
                       :source :repo
                       :path (str (System/getProperty "user.home") "/repo/AGENTS.md")
                       :content
                       "PROJECT-RULE-FROM-AGENTS-MD\nreproduce -> inspect -> minimal change"})

                    agents/added-root-guidance-index
                    (constantly [])]

        (let [env
              {:extensions (atom [])}

              messages
              (prompt/assemble-stable-prompt-messages env {:active-extensions []})

              text
              (prompt/stable-prompt-text messages)]

          (expect (str/includes? text "PROJECT-INSTRUCTIONS"))
          (expect (str/includes? text "PROJECT-RULE-FROM-AGENTS-MD"))
          (expect (str/includes? text "~/repo/AGENTS.md"))
          (expect (not (str/includes? text
                                      (str (System/getProperty "user.home") "/repo/AGENTS.md"))))
          (expect (str/includes? text "CORE wins"))
          (expect (< (str/index-of text "SYSTEM-PROMPT")
                     (str/index-of text "PROJECT-INSTRUCTIONS"))))))
  (it "indexes added-root guidance without injecting its contents"
      (with-redefs [agents/primary-instructions
                    (constantly {:found? true
                                 :files [{:scope :project
                                          :source :agents-md
                                          :path (str (System/getProperty "user.home")
                                                     "/vis/AGENTS.md")
                                          :content "VIS-RULE"}]})

                    agents/added-root-guidance-index
                    (constantly [{:root (str (System/getProperty "user.home") "/spel")
                                  :path (str (System/getProperty "user.home") "/spel/AGENTS.md")
                                  :source :agents-md}])]

        (let [env
              {:extensions (atom [])}

              messages
              (prompt/assemble-stable-prompt-messages env {:active-extensions []})

              text
              (prompt/stable-prompt-text messages)]

          (expect (str/includes? text "VIS-RULE"))
          (expect (str/includes? text "~/spel — guidance: ~/spel/AGENTS.md"))
          (expect (str/includes? text "guidance is not loaded yet"))
          (expect (str/includes? text "read its exact guidance path in `python_execution`"))
          (expect (not (str/includes? text "SPEL-RULE"))))))
  (it "falls back to CLAUDE.md when primary AGENTS.md is absent"
      (with-redefs [agents/primary-instructions
                    (constantly {:found? true
                                 :source :repo:claude-md-fallback
                                 :path "/tmp/repo/CLAUDE.md"
                                 :content "CLAUDE-FALLBACK-RULE"})

                    agents/added-root-guidance-index
                    (constantly [])]

        (let [text (-> (prompt/assemble-stable-prompt-messages {:extensions (atom [])}
                                                               {:active-extensions []})
                       prompt/stable-prompt-text)]
          (expect (str/includes? text "CLAUDE-FALLBACK-RULE"))
          (expect (str/includes? text "CLAUDE.md")))))
  (it "emits no PROJECT-INSTRUCTIONS block when no guidance is available"
      (with-redefs [agents/primary-instructions
                    (constantly {:found? false})

                    agents/added-root-guidance-index
                    (constantly [])]

        (let [text (-> (prompt/assemble-stable-prompt-messages {:extensions (atom [])}
                                                               {:active-extensions []})
                       prompt/stable-prompt-text)]
          (expect (not (str/includes? text "PROJECT-INSTRUCTIONS")))))))

(defdescribe prompt-names-one-tool-test
             ;; The whole surface is ONE call now. Any sentence that still names a second
             ;; door, a schema or a stored result re-opens a routing question the model can
             ;; no longer act on, so the assembled prompt is checked for the old vocabulary
             ;; rather than only the core string.
             (it "names `python_execution`, and nothing from the eighteen-door vocabulary"
                 (manifest/initialize!)
                 (let [text (prompt/stable-prompt-text
                              (prompt/assemble-stable-prompt-messages
                                {}
                                {:active-extensions (vec (extension/registered-extensions))}))]
                   (expect (str/includes? text "python_execution"))
                   (doseq [gone ["ntr[" "# saved:" "native tool" "Native tool" "native tools"
                                 "JSON Schema" "JSON schema" "advertised tool"]]
                     (expect (not (str/includes? text gone)))))))

(defdescribe extension-fragments-do-not-restate-doc-text-test
             ;; An `:ext/prompt-fn` fragment is PUSHED into every request; a symbol's own
             ;; document is PULLED once with `doc(name)`. Pasting a signature, a return
             ;; shape or a description up here is exactly how the tokens the one-tool
             ;; surface saved come straight back — and it makes a second contract that
             ;; drifts from the one that runs.
             (it "keeps every active fragment free of the text `doc(name)` already answers"
                 (manifest/initialize!)
                 (let [exts
                       (vec (extension/registered-extensions))

                       block
                       (str (#'prompt/extensions-prompt-block {} exts))

                       entries
                       (vec (for [ext
                                  exts

                                  entry
                                  (extension/ext-symbols ext)]

                              entry))

                       doc-first-lines
                       (into #{}
                             (comp (keep extension/symbol-doc-text)
                                   (map #(str/trim (first (str/split-lines %))))
                                   (remove str/blank?))
                             entries)

                       fragment-lines
                       (into [] (comp (map str/trim) (remove str/blank?)) (str/split-lines block))]

                   (expect (seq entries))
                   (expect (seq doc-first-lines))
                   (expect (not (str/blank? block)))
                   ;; a doc's own first line, copied into the prompt
                   (expect (empty? (filter doc-first-lines fragment-lines)))
                   ;; the raw-result contract belongs to `doc(name)`, never to a fragment
                   (expect (not (str/includes? block "Raw result:")))
                   ;; a declared signature: `name(args) -> shape`
                   (expect (nil? (re-find #"\w\([^)\n]*\)\s*->" block))))))

(defdescribe extension-activation-test
             (it "assembles from precomputed active extensions without activating again"
                 (let [calls
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
      (let [msgs
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
      (let [msgs
            (prompt/assemble-initial-messages
              {:stable-prompt-messages []
               :initial-user-content "what is on /tmp/shot.png?"
               :user-images [{:path "/tmp/shot.png"
                              :media-type "image/png"
                              :base64 tiny-png-b64
                              :size 5
                              :size-label "5B"}]
               :skipped-images [{:path "/tmp/huge.png"
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
      (let [corrupt
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
      (let [msgs
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
      (let [msgs (prompt/assemble-initial-messages
                   {:stable-prompt-messages [{:role "system" :content "sys"}]
                    :user-images
                    [{:path "p" :media-type "image/png" :base64 "eA==" :size 1 :size-label "1B"}]})]
        ;; no user message without initial-user-content — images can't ride alone
        (expect (= 1 (count msgs)))
        (expect (= "system" (:role (first msgs)))))))

(defdescribe
  attached-images-descriptions-test
  "When the active model cannot see, a sighted model's report stands in for the
   pixels. The manifest must carry that report AND label it second-hand: an agent
   that thinks it saw the image will testify about detail no one described."
  (let [assemble (fn [descriptions]
                   (:content (last (prompt/assemble-initial-messages
                                     {:stable-prompt-messages []
                                      :initial-user-content "what is on /tmp/shot.png?"
                                      :vision? false
                                      :user-images [{:path "/tmp/shot.png"
                                                     :media-type "image/png"
                                                     :base64 tiny-png-b64
                                                     :size 5
                                                     :size-label "5B"}]
                                      :image-descriptions descriptions}))))]
    (it "quotes the description, names the model and marks it second-hand"
        (let [content (assemble {"/tmp/shot.png" {:text "a red 1x1 pixel" :model "pricey-seer"}})]
          (expect (str/includes? content "/tmp/shot.png"))
          (expect (str/includes? content "a red 1x1 pixel"))
          (expect (str/includes? content "pricey-seer"))
          (expect (str/includes? content "second-hand"))
          ;; Nothing to open with PIL — the content is already here.
          (expect (not (str/includes? content "the ONLY way to see them here")))
          ;; Still no image blocks on a blind wire.
          (expect (string? content))))
    (it "keeps the PIL directive when nothing described the image"
        ;; Toggle off, no sighted model in the fleet, or a refused ask: unchanged.
        (let [content (assemble nil)]
          (expect (str/includes? content "NOT attached"))
          (expect (str/includes? content "PIL"))
          (expect (not (str/includes? content "second-hand")))))
    (it "ignores a description that names a different image"
        (let [content (assemble {"/tmp/other.png" {:text "not this one" :model "seer"}})]
          (expect (not (str/includes? content "not this one")))
          (expect (str/includes? content "PIL"))))
    (it "carries both directives when only some images were described"
        (let [content (:content (last (prompt/assemble-initial-messages
                                        {:stable-prompt-messages []
                                         :initial-user-content "look"
                                         :vision? false
                                         :user-images [{:path "/tmp/a.png"
                                                        :media-type "image/png"
                                                        :base64 tiny-png-b64
                                                        :size 5
                                                        :size-label "5B"}
                                                       {:path "/tmp/b.png"
                                                        :media-type "image/png"
                                                        :base64 tiny-png-b64
                                                        :size 5
                                                        :size-label "5B"}]
                                         :image-descriptions {"/tmp/a.png" {:text "a red pixel"
                                                                            :model "seer"}}})))]
          (expect (str/includes? content "a red pixel"))
          (expect (str/includes? content "NO description"))
          (expect (str/includes? content "PIL"))))
    (it "never lets a description displace pixels a SIGHTED model can read"
        (let [msgs (prompt/assemble-initial-messages
                     {:stable-prompt-messages []
                      :initial-user-content "look"
                      :user-images [{:path "/tmp/shot.png"
                                     :media-type "image/png"
                                     :base64 tiny-png-b64
                                     :size 5
                                     :size-label "5B"}]
                      :image-descriptions {"/tmp/shot.png" {:text "a red pixel" :model "seer"}}})
              blocks (:content (last msgs))]

          ;; The image rides; the stale description is not printed anywhere.
          (expect (vector? blocks))
          (expect (not (str/includes? (pr-str blocks) "a red pixel")))))))
(defdescribe
  resume-message-cache-stability-test
  (it "appends each completed turn as its own stable message"
      (let [entry
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
      (let [messages
            (prompt/assemble-initial-messages
              {:previous-turn-context [{:checkpoint? true :turns [1 2] :gist "durable state"}]
               :turn-context "session[\"turn\"] = 3"
               :initial-user-content "continue"})

            prior
            (:content (first messages))]

        (expect (= 2 (count messages)))
        (expect (str/includes? prior "folded turns 1, 2"))
        (expect (str/includes? prior "durable state"))
        (expect (not (str/includes? prior "user asked:")))))
  (it "renders cancelled work as settled history with a model-visible abort marker"
      (let [block (prompt/previous-turn-context-block [{:turn 1
                                                        :user-request "inspect and fix"
                                                        :cancelled? true
                                                        :results [{:scope "t1/i1/f1"
                                                                   :src "cat(src)"}]}])]
        (expect (str/includes? block "cat(src)"))
        (expect (str/includes? block "<turn_cancelled>"))
        (expect (str/includes? block "persisted results remain valid; do not repeat settled work"))
        (expect (not (str/includes? block "INTERRUPTED before it finished"))))))


(defdescribe core-prompt-routes-text-edits-to-patch-test
             ;; The verbs exist only if the prompt spends them. Before this, the core
             ;; prompt told the model to CHANGE the tree with `Path.write_text` — which is
             ;; how a 2 KB block that restates the old text becomes the normal way to edit.
             (it "names both anchored verbs and the address they speak"
                 (let [text (prompt/build-system-prompt {})]
                   (expect (str/includes? text "cat(path, start, end)"))
                   (expect (str/includes? text "patch(path, edits)"))
                   (expect (str/includes? text "\"replace\""))
                   (expect (str/includes? text "line:hash"))))
             (it "never tells the model to write a text EDIT in plain Python"
                 (let [text (prompt/build-system-prompt {})]
                   (expect (not (str/includes? text "CHANGING the tree is plain Python")))
                   (expect (not (str/includes? text "are edited in plain Python")))
                   (expect (str/includes? text "restate the text you replace: quote the anchor"))))
             ;; Regression: `cat` grew a negative endpoint and the prompt kept quiet, so
             ;; the tail of a file still cost a line count first and then a read.
             (it "says a negative `start` counts from the end"
                 (let [text (prompt/build-system-prompt {})]
                   (expect (str/includes? text "a negative"))
                   (expect (str/includes? text "`start` counts from the end"))))
             ;; Regression: after grep started answering ONE anchored TEXT block the
             ;; prompt still said only "hits arrive ANCHORED", never WHAT arrives, so the
             ;; model kept treating the answer as a keyed map.
             (it "says grep answers anchored TEXT, and how to spend several hits at once"
                 (let [text (prompt/build-system-prompt {})]
                   (expect (str/includes? text "answers anchored TEXT, never a map"))
                   (expect (not (str/includes? text "returns a MAP")))
                   ;; Several hits in ONE file are ONE patch call now, so there is no
                   ;; order left for the caller to compute.
                   (expect (not (str/includes? text "bottom-up")))
                   (expect (str/includes? text "every `patch` edit for a file"))
                   (expect (str/includes? text "FRESH ANCHOR"))))
             ;; Regression: `sh.logs` grew the same negative tail `cat` has, and the
             ;; prompt named the method with no arguments at all, so a watcher still
             ;; paged bytes to answer "what did it just print".
             (it "says a shell handle reads its last n LINES"
                 (let [text (prompt/build-system-prompt {})]
                   (expect (str/includes? text "`sh.logs(-50)` (last n LINES)")))))

;; Regression: §2 named the execution surfaces but never the SHAPE of the code written on
;; them, so blocks retyped absolute paths, redefined the same helper in every block, and a
;; chore repeated across turns never became anything the project keeps.
(defdescribe core-prompt-steers-python-shape-test
             (it "binds roots off `session` and derives every path from them"
                 (let [text (prompt/build-system-prompt {})]
                   (expect (str/includes? text "Write a PROGRAM, not a transcript"))
                   (expect (str/includes? text "Path(session[\"workspace\"][\"root\"])"))
                   (expect (str/includes? text "`await gather(...)`"))))
             ;; Regression, user report ("can I write function definitions into the session
             ;; object and refine them over time?"): §2 said "definitions persist between
             ;; blocks", which reads as within-turn scratch, and called `session` a "read-only
             ;; map" — a write there SUCCEEDS and is erased before the next block, so the
             ;; obvious place to keep a helper is the one place that silently loses it.
             (it "scopes a definition to the whole session and refuses `session` as storage"
                 (let [text (prompt/build-system-prompt {})]
                   (expect (str/includes? text "CALL the one an earlier block defined"))
                   (expect (str/includes? text "`defs()`"))
                   (expect (str/includes? text "`defs(name)` reads one back to refine"))
                   ;; Regression: the prompt promised a `def` only "persists for the whole
                   ;; session" — true of the interpreter, false of the PROCESS, so a restart
                   ;; silently emptied the sandbox the transcript still described.
                   (expect (str/includes? text "outlives the block, the turn, and"))
                   (expect (str/includes? text "a gateway restart"))
                   (expect (str/includes? text "REBUILT before every block"))
                   (expect (str/includes? text "never store in it"))
                   (expect (not (str/includes? text "definitions persist between blocks")))
                   (expect (not (str/includes? text "live read-only map")))))
             ;; A chore that repeats across turns is the one thing a session can turn into
             ;; project-durable tooling, and the rule is executable only if it names the file
             ;; it lives in and the document that shows how to write it.
             (it "proposes a Python extension when the chore outlives the turn"
                 (let [text (prompt/build-system-prompt {})]
                   (expect (str/includes? text ".vis/extensions/*.py"))
                   (expect (str/includes? text "doc(\"extending\")"))
                   (expect (str/includes? text "write it when asked")))))
