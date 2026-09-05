(ns com.blockether.vis.internal.python.env-form-eval-test
  "Vis-owned error mapping and host-surface checks for Python block execution.
   Parsing, per-form evaluation, async execution, persistence, protected names and
   boundary marshalling are tested exhaustively by vis-python-runtime."
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [com.blockether.vis.internal.python.env :as ep]
            [com.blockether.vis.internal.foundation.language-surface :as language-surface]
            [com.blockether.vis.test-python-context :as tpc]
            [lazytest.core :refer [defdescribe expect it]]))

(defn- py-ctx
  "This namespace's own long-lived sandbox. Parser helpers live in this SAME
   session context and take source as an argument, so they neither allocate an
   auxiliary context nor race through a scratch global."
  []
  (tpc/context ::ctx))

(defn- out
  "The block's ONE success channel: what it PRINTED, trimmed. A block that printed
   nothing has no output at all — its own value is never echoed."
  [r]
  (str/trim (str (:stdout r))))

(defn- classify
  "Parse `code` (parse-only — never evaluates the forms). On SyntaxError, run it
   through `map-python-error` and return the op-error map; otherwise `:parsed`."
  [code]
  (try (ep/count-top-level-forms (py-ctx) code)
       :parsed
       (catch Exception e (ep/map-python-error (py-ctx) (ex-message e) code))))

(defn- prose-leading? [code] (boolean (get-in (classify code) [:data :prose-leading?])))

(defdescribe
  prose-leading-guard-test
  ;; --- positives: real failing replies seen live (sessions 2e98be97 / 4c0eff03)
  (it "flags markdown heading + prose (apostrophe -> unterminated string)"
      (expect (prose-leading?
                "## Root cause found\n\nRift clones via copy-on-write.\ndone(\"x\")")))
  (it "flags a prose sentence with a unicode char (invalid character ×)"
      (expect (prose-leading?
                "Both dialogs now resolve to an identical box at 120×40.\ndone(\"\"\"ok\"\"\")")))
  (it "flags a prose sentence whose apostrophes orphan a paren (unmatched ')')"
      (expect (prose-leading? "I've spent enough (removing them didn't help).\ngit_status()")))
  ;; --- negatives: valid code or genuine code typos must NOT be flagged
  (it "does NOT flag valid code" (expect (= :parsed (classify "git_status()"))))
  (it "does NOT flag a real code typo whose first line is valid code"
      (expect (not (prose-leading? "git_status()\nx = (1 + 2"))))
  (it "does NOT flag a multiline call cut mid-construct (no prose signature)"
      (expect (not (prose-leading? "cat(\"a.clj\"\nfoo"))))
  (it "does NOT flag a comment followed by a code typo"
      (expect (not (prose-leading? "# read the file\nx = (1 + 2"))))
  ;; --- the message must name PROSE (not unicode/typo) to break the misdiagnosis loop
  (it "actionable message names prose, not the character that tripped"
      (let [msg (:message (classify "Both dialogs resolve at 120×40 now.\ndone(\"\"\"ok\"\"\")"))]
        (expect (str/includes? msg "PROSE"))))
  ;; --- a stray non-ASCII char in code position ANYWHERE (not just line 1) — the
  ;;     em-dash-at-line-71 gap the prose-leading detector (first line only) missed
  (it "flags a non-ASCII char in code position even mid-reply, with its line"
      (let [r (classify "x = 5\n# a note\ny = 3 — 1")]
        (expect (true? (get-in r [:data :non-ascii-in-code?])))
        (expect (= 3 (get-in r [:data :line])))
        (expect (str/includes? (:message r) "non-ASCII"))))
  (it "a leading-prose failure stays tagged prose-leading, not non-ascii"
      (let [r (classify "I've spent enough (removing them didn't help).\ngit_status()")]
        (expect (nil? (get-in r [:data :non-ascii-in-code?])))
        (expect (true? (get-in r [:data :prose-leading?]))))))

(defdescribe facade-verb-name-guard-test
             ;; Drift guard: the language facade verbs must NEVER regress to the bare
             ;; collision-prone names. `test`/`format` collide with the commonest variable
             ;; names AND Python builtins, so naming a facade verb that would make the
             ;; strong rebind-guard fire on natural variables is forbidden.
             (it "no facade verb uses a collision-prone bare name"
                 (let [facade
                       (set (map (comp str :ext.symbol/symbol) language-surface/symbols))

                       banned
                       #{"test" "format" "list" "type" "dict" "set" "str" "input" "id"}]

                   (expect (empty? (set/intersection facade banned)))))
             (it "pins the facade verb name set"
                 (let [facade (set (map (comp str :ext.symbol/symbol) language-surface/symbols))]
                   (expect (= #{"format_code" "lint_code" "run_tests" "repl_eval" "repl_start"
                                "repl_status" "repl_stop" "repl_connect"}
                              facade)))))

(defdescribe
  probe-path-binding-test
  "`probe-path` binds as the snake_case `probe_path` tool in the sandbox — no `is_exists`/`exists` alias."
  (it "exposes probe_path and NOT the old is_exists name"
      (tpc/with-own [ctx
                     ;; strings-only boundary: tool results are built with STRING
                     ;; keys at the source (a keyword-keyed result now throws).
                     {'probe-path (fn [path]
                                    {"path" path "exists" (= path "present.txt")})}]
                    (let [via-file
                          (ep/run-python-block
                            ctx
                            "f = await probe_path('present.txt')\nprint(f['path'], f['exists'])"
                            "t1/i1")

                          via-missing
                          (ep/run-python-block
                            ctx
                            "m = await probe_path('missing.txt')\nprint(m['path'], m['exists'])"
                            "t1/i2")

                          via-old
                          (ep/run-python-block ctx "is_exists('present.txt')" "t1/i3")]

                      (expect (nil? (:error via-file)))
                      (expect (nil? (:error via-missing)))
                      (expect (= "present.txt True" (out via-file)))
                      (expect (= "missing.txt False" (out via-missing)))
                      (expect (str/includes? (get-in via-old [:error :message])
                                             "`is_exists` is not defined")))))
  (it "removing the binding makes probe_path undefined"
      (let [ctx (tpc/context ::ctx)]
        (ep/set-python-binding! ctx
                                'probe-path
                                (fn [path]
                                  {"path" path "exists" true}))
        (expect (= "dynamic.txt True"
                   (out (ep/run-python-block
                          ctx
                          "d = await probe_path('dynamic.txt')\nprint(d['path'], d['exists'])"
                          "t1/i3"))))
        (ep/remove-python-binding! ctx 'probe-path)
        (expect (str/includes? (get-in (ep/run-python-block ctx "probe_path('dynamic.txt')" "t1/i4")
                                       [:error :message])
                               "`probe_path` is not defined")))))

(defdescribe
  no-auto-repair-test
  "Auto-repair and fabrication/glued detection were REMOVED (2026-06-21). A reply
   that fails to split is NOT salvaged — it errors as a plain SyntaxError and the
   model resends clean code. With ONE `code` argument per tool call
   the block-concat causes don't arise; :auto-repaired is always nil."
  (it "GLUED top-level forms ERROR as a SyntaxError (not repaired)"
      (let [r (ep/run-python-block (py-ctx) "len([1,2])abs(-3)")]
        (expect (not (contains? r :result)))
        (expect (some? (:error r)))
        (expect (= :python/syntax (get-in r [:error :data :phase])))
        (expect (nil? (:auto-repaired r)))))
  (it "a PARROTED transcript tail (```ctx + r[...]= echoes) ERRORS as a SyntaxError, not salvaged"
      (let [r (ep/run-python-block (py-ctx)
                                   (str "x_e2e = 1\nx_e2e + 41\n"
                                        "```ctx\n[\"env\"][\"nrepl\"] = 7888\n# tool results\n"
                                        "r[\"t4/i1/f1\"] = {\"files\": [\"a.clj\"]}"))]
        (expect (not (contains? r :result)))
        (expect (some? (:error r)))
        (expect (= :python/syntax (get-in r [:error :data :phase])))
        (expect (nil? (:auto-repaired r)))))
  (it "clean Python still runs untouched (no false repair)"
      (let [r (ep/run-python-block (py-ctx) "print(len([1,2,3]))")]
        (expect (= "3" (out r)))
        (expect (nil? (:error r)))
        (expect (nil? (:auto-repaired r))))))

(defdescribe
  sandbox-denial-hint-test
  "A sandbox capability denial (filesystem / native / process) maps to an
   ACTIONABLE hint steering to grep / cat / repl_eval — not the bare
   `PermissionError` the model kept hitting when it reached for
   importlib.exec_module / open() on a project file.

   The roots are the CONFINEMENT and the JAIL is what turns them on: a session
   with `jail.enabled` false reaches the whole machine through `shell` anyway, so
   the interpreter is not confined there and there is no denial to hint at. These
   sandboxes are jailed, rooted at the project directory, and `/etc` is what lies
   outside."
  (let [mk (fn []
             (:python-context (tpc/new-context {}
                                               (constantly [(System/getProperty "user.dir")])
                                               {:jail-enabled? true})))]
    (it "open() policy denial names the exact blocked operation and safe remedy"
        (let [m (get-in (ep/run-python-block (mk) "open('/etc/hosts').read()" "t1/i1")
                        [:error :message])]
          (expect (some? m))
          (expect (clojure.string/includes? (str m) "Sandbox policy denied file-read"))
          (expect (clojure.string/includes? (str m) "outside approved filesystem roots"))
          (expect (clojure.string/includes? (str m) "grep({\"query\": q, \"context\": 4})"))
          (expect (clojure.string/includes? (str m) "repl_eval"))
          (expect (clojure.string/includes? (str m) "workspace.filesystem"))
          (expect (clojure.string/includes? (str m) "vis.yml"))
          (expect (clojure.string/includes? (str m) "/reload"))
          (expect (not (clojure.string/includes? (str m) "/fs")))))
    (it "write denial names file-write rather than collapsing it into a read"
        (let [m (get-in (ep/run-python-block (mk) "open('/etc/vis-nope', 'w')" "t1/i1")
                        [:error :message])]
          (expect (clojure.string/includes? (str m) "Sandbox policy denied file-write"))))
    (it "importlib exec_module on a project file → the same steer"
        (let [m (get-in
                  (ep/run-python-block
                    (mk)
                    (str "import importlib.util\n"
                         "spec = importlib.util.spec_from_file_location('x', '/etc/zz_nope.py')\n"
                         "mod = importlib.util.module_from_spec(spec)\n"
                         "spec.loader.exec_module(mod)")
                    "t1/i1")
                  [:error :message])]
          (expect (clojure.string/includes? (str m) "repl_eval"))))))

(defdescribe
  precise-hint-test
  "More precise hints by what actually failed — beyond the generic parser error."
  (let [mk (fn []
             (:python-context (tpc/new-context {(quote lst) (fn []
                                                              [1 2 3])})))]
    (it "IndentationError → an indentation-specific hint"
        (let [m (get-in (ep/run-python-block (mk) "if True:\nx = 1" "t1/i1") [:error :message])]
          (expect (clojure.string/includes? (str m) "INDENTATION"))))
    (it ".get on a LIST-shaped function return answers the uniform dict probe"
        ;; A host function value whose top level is a list becomes __VisResultList__,
        ;; so the documented `res.get('op')` sweep works across return shapes instead
        ;; of failing with `'list' object has no attribute 'get'`.
        (let [r (ep/run-python-block (mk)
                                     (str "r = await lst()\n"
                                          "print([list(r), r.get('op'), r.get('x', 'dflt')])")
                                     "t1/i1")]
          (expect (nil? (:error r)))
          (expect (= "[[1, 2, 3], None, 'dflt']" (out r)))))))

(defdescribe
  source-context-test
  "Babashka-style source excerpt on an eval failure: the failing line, numbered,
   with a caret run under the exact offending span (map-python-error +
   render-source-context). The async trampoline strips guest frames from the Java
   stack, so a RUNTIME position is recovered from the Python __traceback__."
  (it "a nested runtime failure points at the failing line INSIDE the function"
      (let [r
            (ep/run-python-block
              (py-ctx)
              "def compute(x):\n    y = x + 1\n    return y / 0\n\nprint(compute(41))")

            err
            (:error r)

            msg
            (:message err)]

        (expect (= 3 (get-in err [:data :line]))) ;; the `/ 0` line, NOT the call site (5)
        (expect (= 12 (get-in err [:data :column])))
        (expect (str/includes? msg "1: def compute(x):"))
        (expect (str/includes? msg "return y / 0"))
        (expect (str/includes? msg "^"))))
  (it "an undefined name pins line+caret to the name, overriding the shallow loc"
      (let [r
            (ep/run-python-block (py-ctx) "print(undefined_zzz)")

            err
            (:error r)

            msg
            (:message err)]

        (expect (= 1 (get-in err [:data :line])))
        (expect (= 7 (get-in err [:data :column]))) ;; under `undefined_zzz`, not `print`
        (expect (str/includes? msg "1: print(undefined_zzz)"))
        (expect (str/includes? msg "^^^"))))        ;; a multi-char caret span
  (it "the DEEPEST user-code frame wins for an error raised inside a called fn"
      (let [r (ep/run-python-block
                (py-ctx)
                "def pick(xs):\n    return xs[10]\n\nrows = [1, 2, 3]\nprint(pick(rows))")]
        (expect (= 2 (get-in r [:error :data :line])))
        (expect (str/includes? (:message (:error r)) "return xs[10]"))))
  (it "a compile/syntax error keeps its precise loc-based excerpt"
      (let [r
            (ep/run-python-block (py-ctx) "def h():\n    if True 1")

            err
            (:error r)]

        (expect (= :python/syntax (get-in err [:data :phase])))
        (expect (= 2 (get-in err [:data :line])))
        (expect (str/includes? (:message err) "if True 1"))
        (expect (str/includes? (:message err) "^"))))
  (it "a tab-indented body renders an aligned caret with NO raw tab in the excerpt"
      (let [r
            (ep/run-python-block (py-ctx) "def tb(x):\n\treturn x / 0\n\nprint(tb(1))")

            msg
            (:message (:error r))]

        (expect (= 2 (get-in r [:error :data :line])))
        (expect (not (str/includes? msg "\treturn"))) ;; detabbed so 1 char == 1 column
        (expect (str/includes? msg "^"))))
  (it
    "a `raise … from …` inside an except starts the caret on `raise`, not the gutter"
    (let
      [r
       (ep/run-python-block
         (py-ctx)
         "def go():\n    try:\n        1/0\n    except Exception as e:\n        raise ValueError('oops') from e\ngo()")

       msg
       (:message (:error r))

       lines
       (str/split-lines msg)

       ci
       (first (keep-indexed (fn [i l]
                              (when (re-find #"^\s*\^+\s*$" l) i))
                            lines))

       caret
       (nth lines ci)

       src
       (nth lines (dec ci))

       col
       (count (take-while #(= \space %) caret))]

      (expect (= 5 (get-in r [:error :data :line])))
      (expect (str/includes? msg "raise ValueError('oops') from e"))
      ;; co_positions reports the handler column (4) for the re-raise; the caret
      ;; must still land on the `r` of `raise`, never in the leading-space gutter.
      (expect (= \r (nth src col)))))
  (it "a CJK glyph before the token aligns the caret by CODEPOINT columns"
      (let [r
            (ep/run-python-block (py-ctx) "名前 = missing_var + 1")

            msg
            (:message (:error r))

            lines
            (str/split-lines msg)

            caret
            (first (filter #(re-find #"^\s*\^+\s*$" %) lines))

            pad
            (count (take-while #(= \space %) caret))

            span
            (count (filter #(= \^ %) caret))]

        (expect (= 1 (get-in r [:error :data :line])))
        ;; The caret is padded by CHARACTER count, not terminal display width:
        ;; "1: " (3) + "名前 = " (5 codepoints) = 8, so it lands on `missing_var`
        ;; at the same char index the LLM reads (no double-width fudge).
        (expect (= 8 pad))
        ;; caret spans all of `missing_var` (11 chars).
        (expect (= 11 span))))
  (it "an empty / comment-only / stripped block returns a clean message, not the async internal"
      (doseq [code ["# just a comment\n# nothing here" "   \n\t\n  " "from asyncio import gather"]]
        (let [err (:error (ep/run-python-block (py-ctx) code))]
          (expect (= :python/empty-block (get-in err [:data :phase])))
          (expect (true? (get-in err [:data :empty-block?])))
          (expect (str/includes? (:message err) "nothing to execute"))
          ;; the leaked async-wrapper internal must NEVER surface.
          (expect (not (str/includes? (:message err) "AsyncFunctionDef")))
          (expect (not (str/includes? (:message err) "empty body"))))))
  (it "a clean eval carries no error and no excerpt"
      (let [r (ep/run-python-block (py-ctx) "print(1 + 2)")]
        (expect (nil? (:error r))))))
