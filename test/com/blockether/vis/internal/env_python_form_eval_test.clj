(ns com.blockether.vis.internal.env-python-form-eval-test
  "The Python form-eval contract, as executable tests. (Supersedes the old
   FORM_EVAL_CONTRACT.md spec — that was a pre-implementation agreement; the
   behavior is shipped, so these tests are the living record.)

   Two halves:
   1. `run-python-block` per-form AST eval semantics (E1–E7 / R1–R7): the reply
      splits into top-level statements via CPython `ast`; a bare expression
      echoes its value, `x = …` echoes x, a comment is not a form, the last
      form's value is the turn result, and evaluation stops at the first error.
   2. Prose-leading SyntaxError detection: when the model opens with PROSE, the
      whole reply parses as Python and fails with a CPython error whose text
      varies by which mangled token trips first (apostrophe → unterminated
      string, `×` → invalid character, apostrophe-pairs → unmatched ')').
      `map-python-error` tags these `:prose-leading? true` with an actionable
      message, while NEVER mislabeling a genuine code typo as prose."
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [com.blockether.vis.internal.env-python :as ep]
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

(defdescribe session-parser-helper-test
             (it "parses in the session context without clobbering the eval scratch global"
                 (let [ctx (py-ctx)]
                   (tpc/ev ctx "__vis_src__ = \"keep-me\"")
                   (expect (= 3 (ep/count-top-level-forms ctx "x = 1\ny = 2\nz = 3")))
                   (expect (= "keep-me" (tpc/ev ctx "__vis_src__")))))
             (it "is race-free when many threads parse different sources on one context"
                 (let [cases
                       (vec (take 90
                                  (cycle [["# comment" 0] ["x = 1" 1] ["x = 1\ny = 2\nz = 3" 3]])))

                       jobs
                       (mapv (fn [[source expected]]
                               (future (= expected (ep/count-top-level-forms (py-ctx) source))))
                             cases)]

                   (expect (every? true? (mapv deref jobs))))))

(defdescribe
  sandbox-auto-import-test
  (it "makes shlex available without an import in run_python code"
      (let [r (ep/run-python-block (py-ctx) "print(shlex.quote('a b'))")]
        (expect (nil? (:error r)))
        (expect (= "'a b'" (out r)))))
  (it "makes re available without an import in run_python code"
      (let [r (ep/run-python-block (py-ctx) "print(re.sub(r'\\d+', '#', 'a12b3'))")]
        (expect (nil? (:error r)))
        (expect (= "a#b#" (out r)))))
  (it "makes hashlib available without an import in run_python code"
      (let [r (ep/run-python-block (py-ctx) "print(hashlib.sha256(b'hello world').hexdigest())")]
        (expect (nil? (:error r)))
        (expect (= "b94d27b9934d3e08a52e52d7da7dabfac484efe37a5380ee9088f7ace2efcde9" (out r)))))
  (it "makes json available without an import in run_python code"
      (let [r (ep/run-python-block (py-ctx) "print(json.dumps({'b': 2, 'a': 1}, sort_keys=True))")]
        (expect (nil? (:error r)))
        (expect (= "{\"a\": 1, \"b\": 2}" (out r)))))
  (it "makes os available without an import in run_python code"
      (let [r (ep/run-python-block (py-ctx) "print(os.path.join('a', 'b'))")]
        (expect (nil? (:error r)))
        (expect (= "a/b" (out r)))))
  (it "makes sys available without an import in run_python code"
      (let [r (ep/run-python-block (py-ctx) "print(isinstance(sys.maxsize, int))")]
        (expect (nil? (:error r)))
        (expect (= "True" (out r)))))
  (it "makes collections available without an import in run_python code"
      (let [r (ep/run-python-block (py-ctx) "print(dict(collections.Counter('aab')))")]
        (expect (nil? (:error r)))
        (expect (= "{'a': 2, 'b': 1}" (out r)))))
  (it "makes Counter available without an import in run_python code"
      (let [r (ep/run-python-block (py-ctx) "print(dict(Counter('abb')))")]
        (expect (nil? (:error r)))
        (expect (= "{'a': 1, 'b': 2}" (out r)))))
  (it "makes pathlib and Path available without an import in run_python code"
      (let [r (ep/run-python-block
                (py-ctx)
                "print(pathlib.Path('a/b').name == 'b' and Path('a/b').name == 'b')")]
        (expect (nil? (:error r)))
        (expect (= "True" (out r)))))
  (it "makes textwrap available without an import in run_python code"
      (let [r (ep/run-python-block (py-ctx)
                                   "print(textwrap.shorten('alpha beta gamma', width=11))")]
        (expect (nil? (:error r)))
        (expect (= "alpha [...]" (out r)))))
  (it "makes base64 available without an import in run_python code"
      (let [r (ep/run-python-block (py-ctx) "print(base64.b64encode(b'hi').decode())")]
        (expect (nil? (:error r)))
        (expect (= "aGk=" (out r)))))
  (it "makes math available without an import in run_python code"
      (let [r (ep/run-python-block (py-ctx) "print(round(math.sqrt(2) + math.pi, 6))")]
        (expect (nil? (:error r)))
        (expect (= "4.555806" (out r)))))
  (it "makes glob available without an import in run_python code"
      (let [r (ep/run-python-block (py-ctx)
                                   "print(hasattr(glob, 'glob') and callable(glob.glob))")]
        (expect (nil? (:error r)))
        (expect (= "True" (out r)))))
  (it "makes builtins available without an import in run_python code"
      (let [r (ep/run-python-block
                (py-ctx)
                "print(hasattr(builtins, 'len') and builtins.len([1, 2]) == 2)")]
        (expect (nil? (:error r)))
        (expect (= "True" (out r)))))
  (it
    "does not expose auto-imported modules as apropos-listed tools/globals"
    (let
      [r
       (ep/run-python-block
         (py-ctx)
         (str "names = ['shlex', 'json', 're', 'hashlib', 'glob', 'os', 'sys', 'collections',"
              " 'Counter', 'pathlib', 'Path', 'textwrap', 'base64', 'math', 'builtins']\n"
              "listed = set().union(*({item.name for item in apropos(m)} for m in names))\n"
              "print(bool(set(names) & listed))"))]
      (expect (nil? (:error r)))
      (expect (= "False" (out r))))))

(defdescribe
  verb-arg-boundary-test
  ;; Regression: a Python dict passed to a wrapped Clojure verb crosses the
  ;; boundary via `->clj`, which KEYWORDIZES every dict key (snake verbatim).
  ;; A verb once read its opts with the STRING key "models" and so silently got
  ;; nil — the call ran on the DEFAULT model, not the proposed one. This pins the
  ;; shape so verb authors read `:models`, not "models".
  (it
    "dict args arrive with KEYWORD-snake keys; values pass through (strings, vectors)"
    (let [captured
          (atom nil)

          {:keys [python-context]}
          (tpc/new-context {'capture_args
                            (fn [prompt subctx & more]
                              (reset! captured {:prompt prompt :subctx subctx :opts (first more)})
                              "ok")})]

      ;; Tools are async-deferred — run through run-python-block so the bare
      ;; top-level call is SETTLED (executed) before we inspect the capture.
      (ep/run-python-block
        python-context
        "capture_args('go', {'tasks': {'oauth': {'status': 'doing'}}, 'focus': 'oauth'}, {'models': ['haiku', 'sonnet']})"
        "t1/i1")
      (let [{:keys [prompt subctx opts]} @captured]
        (expect (= "go" prompt))
        ;; strings-only boundary: every dict key crosses as a VERBATIM string
        (expect (= #{"tasks" "focus"} (set (keys subctx))))
        (expect (= "oauth" (get subctx "focus")))
        (expect (= "doing" (get-in subctx ["tasks" "oauth" "status"])))
        (expect (= ["haiku" "sonnet"] (get opts "models")))
        (expect (nil? (:models opts)))))))

(defdescribe verb-kwargs-boundary-test
             ;; Regression: the host tool callables are foreign ProxyExecutables (POSITIONAL
             ;; only), so Python **kwargs used to raise `__call__() got an unexpected keyword
             ;; argument`. The docstrings advertised `find("x", paths=[...])` / `rg(query="x")`,
             ;; yet those forms hard-failed. `__vis_exec_call__` now folds **kwargs into a
             ;; TRAILING DICT positional (matching the tool's `tool(query, {opts})` contract),
             ;; so kwargs work for EVERY tool at once.
             (let [captured
                   (atom nil)

                   {:keys [python-context]}
                   (tpc/new-context {'capture_args (fn [& args]
                                                     (reset! captured (vec args))
                                                     "ok")})]

               (it "a positional arg + a kwarg folds to (arg, {kw…}) with VERBATIM STRING keys"
                   (reset! captured nil)
                   (ep/run-python-block python-context
                                        "capture_args('shell', paths=['src', 'extensions'])"
                                        "t1/i1")
                   (let [[a opts] @captured]
                     (expect (= "shell" a))
                     (expect (= ["src" "extensions"] (get opts "paths")))
                     (expect (nil? (:paths opts)))))
               (it "all-kwargs collapse to a single spec map"
                   (reset! captured nil)
                   (ep/run-python-block python-context
                                        "capture_args(query='shell', is_files_only=True)"
                                        "t1/i2")
                   (let [[spec] @captured]
                     (expect (= "shell" (get spec "query")))
                     (expect (= true (get spec "is_files_only")))))
               (it "no kwargs = positional only, unchanged (no stray trailing dict)"
                   (reset! captured nil)
                   (ep/run-python-block python-context "capture_args('a', 'b')" "t1/i3")
                   (expect (= ["a" "b"] @captured)))))

(defdescribe
  protected-tool-name-test
  (let [mk (fn []
             (tpc/context-with! ::ctx
                                {'patch (fn [& _]
                                          "patched")}))]
    (it "lets a block SHADOW a bound tool name and keeps the callable usable after"
        (let [ctx (mk)
              r1 (ep/run-python-block ctx "patch = 'not callable'\nprint(patch)" "t1/i1")
              r2 (ep/run-python-block ctx "print(patch({'path': 'x'}))" "t1/i2")]

          (expect (nil? (:error r1)))
          (expect (str/includes? (str (:stdout r1)) "not callable"))
          (expect (nil? (:error r2)))
          (expect (= "patched" (out r2)))))
    ;; Output callable contract: a sandbox always exposes both spellings, and a block-local
    ;; assignment may not replace either one for the session.
    (it "keeps print and println callable after a block shadows them"
        (let [ctx (mk)
              r1 (ep/run-python-block ctx "print = 'not callable'
println = 'not callable'" "t1/i1")
              r2
              (ep/run-python-block ctx "print('still printing')
println('still println')" "t1/i2")]

          (expect (nil? (:error r1)))
          (expect (nil? (:error r2)))
          (expect (= "still printing
still println" (out r2)))))
    (it "a READ before the shadowing assignment still sees the tool"
        (let [r (ep/run-python-block
                  (mk)
                  "before = await patch({'path': 'x'})\npatch = 'shadow'\nprint(before, patch)"
                  "t1/i1")]
          (expect (nil? (:error r)))
          (expect (str/includes? (str (:stdout r)) "patched shadow"))))
    (it "allows ordinary variables while still awaiting protected tools"
        (let [r (ep/run-python-block (mk)
                                     "css = 'app.css'
print(await patch({'path': css}))"
                                     "t1/i1")]
          (expect (nil? (:error r)))
          (expect (= "patched" (out r)))))
    (it "a tool added after context creation is shadowable and survives the block"
        (let [ctx (tpc/context ::ctx)]
          (ep/set-python-binding! ctx
                                  'later_patch
                                  (fn [& _]
                                    "late"))
          (let [r1 (ep/run-python-block ctx "later_patch = 'oops'" "t1/i1")
                r2 (ep/run-python-block ctx "print(later_patch())" "t1/i2")]

            (expect (nil? (:error r1)))
            (expect (nil? (:error r2)))
            (expect (= "late" (out r2))))))
    ;; Shadowing is BLOCK-LOCAL, never durable: the facade verbs were also renamed
    ;; off the commonest variable/builtin names (`test`→`run_tests`,
    ;; `format`→`format_code`) so natural variables don't collide at all.
    (it "a shadowed run_tests is still the tool in the NEXT block"
        (let [ctx (tpc/context-with! ::ctx
                                     {'run_tests (fn [& _]
                                                   "ran")})
              r1 (ep/run-python-block ctx "run_tests = 'oops'" "t1/i1")
              r2 (ep/run-python-block ctx "print(run_tests('go'))" "t1/i2")]

          (expect (nil? (:error r1)))
          (expect (nil? (:error r2)))
          (expect (= "ran" (out r2)))))
    ;; A `for`/`with` loop TARGET is transient scratch — it stays function-local
    ;; to the wrapped block, so it neither persists nor clobbers the callable.
    ;; It must NOT trip the durable-rebind guard.
    (it "allows a `for` loop target that shadows a tool name and keeps the callable usable"
        (let [ctx (mk)
              r1 (ep/run-python-block ctx "for patch in ['a', 'b']:\n    pass" "t1/i1")
              r2 (ep/run-python-block ctx "print(patch({'path': 'x'}))" "t1/i2")]

          (expect (nil? (:error r1)))
          (expect (nil? (:error r2)))
          (expect (= "patched" (out r2)))))
    (it "lets the model bind `test` and `format` as ordinary variables (not tools)"
        (let [ctx (tpc/context-with! ::ctx
                                     {'patch (fn [& _]
                                               "patched")})
              r
              (ep/run-python-block
                ctx
                "test = 'promise_pool.test.ts'\nformat = 'csv'\nprint(await patch({'path': test}))"
                "t1/i1")]

          (expect (nil? (:error r)))
          (expect (= "patched" (out r)))))))

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
  live-interpreter-persistence-test
  "ONE persistent interpreter per session: a variable the model binds in one
   block is still live in the NEXT block on the same context — globals persist
   NATURALLY (no rebind, no pickle, no per-turn fresh sandbox)."
  (it "a bound variable is still live in a later call on the same context"
      (let [ctx (tpc/context ::ctx)]
        (ep/run-python-block ctx "kept_v = 41")
        (let [r2 (ep/run-python-block ctx "print(kept_v + 1)")]
          (expect (nil? (:error r2)))
          (expect (= "42" (clojure.string/trim (str (:stdout r2))))))))
  ;; MODULE SCOPE is not the top-level statement list: `if`/`for`/`with`/`try`
  ;; bodies execute in the SAME scope, so a name they bind is a module global in
  ;; real Python and must survive the block here too.
  (it "a variable bound INSIDE an if/for/with/try body persists into the next block"
      (let [ctx (tpc/context ::ctx)]
        (ep/run-python-block ctx
                             (str "import io\n"
                                  "if True:\n    in_if = 1\n"
                                  "for _i in range(1):\n    in_for = 2\n"
                                  "with io.StringIO('s') as fh:\n    in_with = fh.read()\n"
                                  "try:\n    in_try = 4\nexcept Exception:\n    pass\n")
                             "t1/i1")
        (let [r (ep/run-python-block ctx "print(in_if, in_for, in_with, in_try)" "t1/i2")]
          (expect (nil? (:error r)))
          (expect (= "1 2 s 4" (clojure.string/trim (str (:stdout r))))))))
  ;; The reported failure: `hk` assigned inside `async with` vanished before the
  ;; next block, which then died with a bare NameError.
  (it "a variable bound inside a `with` in an AWAIT-bearing block persists"
      (let [ctx (tpc/context-with! ::ctx
                                   {'echo (fn [x]
                                            (str "<" x ">"))})]
        (ep/run-python-block
          ctx
          (str "import io\n" "with io.StringIO('x') as fh:\n" "    hk = await echo(fh.read())\n")
          "t1/i1")
        (let [r (ep/run-python-block ctx "print(hk)" "t1/i2")]
          (expect (nil? (:error r)))
          (expect (= "<x>" (out r))))))
  ;; A `with` TARGET is still transient scratch: it must NOT go durable, so it
  ;; can never clobber a protected tool name.
  (it "a `with` target that shadows a tool name stays block-local"
      (let [ctx (tpc/context-with! ::ctx
                                   {'patch (fn [& _]
                                             "patched")})]
        (ep/run-python-block
          ctx
          (str "import io\n" "with io.StringIO('s') as patch:\n" "    got = patch.read()\n")
          "t1/i1")
        (let [r (ep/run-python-block ctx "print(patch({'path': 'x'}))" "t1/i2")]
          (expect (nil? (:error r)))
          (expect (= "patched" (out r))))))
  ;; A walrus binds in the ENCLOSING scope from wherever it appears — an `if`
  ;; test, a comprehension, a call argument — so it is a module global too.
  (it "a walrus binding in an if test / comprehension persists"
      (let [ctx (tpc/context ::ctx)]
        (ep/run-python-block ctx
                             (str "if (wal := 5) > 1:\n    pass\n"
                                  "sq = [dbl := i * 2 for i in range(3)]\n")
                             "t1/i1")
        (let [r (ep/run-python-block ctx "print(wal, dbl)" "t1/i2")]
          (expect (nil? (:error r)))
          (expect (= "5 4" (str/trim (str (:stdout r))))))))
  ;; `del x` on a module global must delete the GLOBAL. Treated as a frame local
  ;; it raised UnboundLocalError on a name that was plainly there.
  (it "`del` removes a module global instead of raising UnboundLocalError"
      (let [ctx (tpc/context ::ctx)]
        (ep/run-python-block ctx "gone = 1" "t1/i1")
        (let [r (ep/run-python-block ctx "del gone\nprint('gone' in globals())" "t1/i2")]
          (expect (nil? (:error r)))
          (expect (= "False" (str/trim (str (:stdout r))))))))
  ;; `for` / `with` / `except` / `case` targets are bindings like any other: real
  ;; module scope keeps them alive after the statement.
  (it "`for` / `with` / `match` targets persist like module bindings"
      (let [ctx (tpc/context ::ctx)]
        (ep/run-python-block ctx
                             (str "import io\n" "for line in ['a']:\n    pass\n"
                                  "with io.StringIO('s') as fh:\n    pass\n"
                                  "match [1, 2]:\n    case [lo, hi]:\n        pass\n")
                             "t1/i1")
        (let [r (ep/run-python-block ctx "print(line, fh.closed, lo, hi)" "t1/i2")]
          (expect (nil? (:error r)))
          (expect (= "a True 1 2" (str/trim (str (:stdout r))))))))
  ;; Durable targets still cannot clobber a tool: a protected name is shadowed
  ;; block-locally instead of being declared global.
  (it "a `for` target that shadows a tool name stays block-local"
      (let [ctx (tpc/context-with! ::ctx
                                   {'patch (fn [& _]
                                             "patched")})]
        (ep/run-python-block ctx "for patch in [1, 2]:\n    pass" "t1/i1")
        (let [r (ep/run-python-block ctx "print(patch({'path': 'x'}))" "t1/i2")]
          (expect (nil? (:error r)))
          (expect (= "patched" (out r))))))
  ;; `from mod import *` is a SyntaxError INSIDE a function, and every block is
  ;; wrapped in one — CPython raised that on a source-less synthesized module,
  ;; which the host could not even render (bare UnsupportedOperationException).
  (it "`from mod import *` binds module names without clobbering a tool"
      (let [ctx (tpc/context-with! ::ctx
                                   {'dumps (fn [& _]
                                             "tool-dumps")})]
        (ep/run-python-block ctx "from math import *\nfrom json import *" "t1/i1")
        (let [r (ep/run-python-block ctx "print(int(pi), dumps('x'))" "t1/i2")]
          (expect (nil? (:error r)))
          (expect (= "3 tool-dumps" (str/trim (str (:stdout r))))))))
  ;; At module level `exec('x = 1')` binds a global and `locals() is globals()`.
  (it "module-level `exec` / `locals()` act on the session globals"
      (let [ctx (tpc/context ::ctx)]
        (let [r (ep/run-python-block ctx
                                     "exec('made = 7')\nprint(made, locals() is globals())"
                                     "t1/i1")]
          (expect (nil? (:error r)))
          (expect (= "7 True" (str/trim (str (:stdout r))))))
        (let [r (ep/run-python-block ctx "print(made)" "t1/i2")]
          (expect (= "7" (str/trim (str (:stdout r))))))))
  ;; Function scope is untouched: an `exec` inside a def still writes to a
  ;; throwaway locals mapping, exactly like CPython.
  (it "`exec` inside a def keeps real function-scope semantics"
      (let [ctx
            (tpc/context ::ctx)

            r
            (ep/run-python-block ctx
                                 (str "def f():\n"
                                      "    exec('inner = 1')\n" "    try:\n"
                                      "        return inner\n" "    except NameError:\n"
                                      "        return 'unbound'\n" "print(f())\n")
                                 "t1/i1")]

        (expect (nil? (:error r)))
        (expect (= "unbound" (str/trim (str (:stdout r)))))))
  ;; A compile error on the SYNTHESIZED module has no source text; re-raised from
  ;; the preamble it renders as a normal Python error at the boundary.
  (it "a compile error on the wrapped module surfaces as a Python error"
      (let [r (ep/run-python-block (tpc/context ::ctx) "nonlocal nope" "t1/i1")]
        (expect (some? (:error r)))
        (expect (str/includes? (str (:message (:error r))) "nonlocal"))))
  ;; MODULE-SCOPE ANNOTATIONS. `x: int = 5` under the wrapper's `global x` is
  ;; "annotated name 'x' can't be global" in CPython; the AnnFix pass rewrites it
  ;; to a plain assignment plus the `__annotations__` record a module keeps.
  (it "an annotated module-level assignment binds and records its annotation"
      (let [ctx (tpc/context ::ctx)]
        (let [r (ep/run-python-block
                  ctx
                  (str "ann_x: int = 5\n"
                       "ann_y: str\n"
                       "print(ann_x, __annotations__['ann_x'] is int, 'ann_y' in globals())")
                  "t1/i1")]
          (expect (nil? (:error r)))
          (expect (= "5 True False" (str/trim (str (:stdout r))))))
        (let [r (ep/run-python-block ctx "print(ann_x + 1)" "t1/i2")]
          (expect (= "6" (str/trim (str (:stdout r))))))))
  ;; `from __future__ import ...` is only legal as the FIRST statement of a module,
  ;; so the wrapper made every future import a SyntaxError. The flags are stripped
  ;; and OR'd into compile() instead — here PEP 563 stores annotations unevaluated.
  (it "`from __future__ import annotations` compiles and applies its flag"
      (let [r (ep/run-python-block (tpc/context ::ctx)
                                   (str "from __future__ import annotations\n"
                                        "fut_v: int = 3\n"
                                        "print(fut_v, repr(__annotations__['fut_v']))")
                                   "t1/i1")]
        (expect (nil? (:error r)))
        (expect (= "3 'int'" (str/trim (str (:stdout r)))))))
  ;; A top-level `return` used to end the block silently (the wrapper is a
  ;; function), and a top-level `yield` turned it into an async generator whose
  ;; body never ran. Both are SyntaxErrors in a real module.
  (it "a top-level `return` is a SyntaxError, not a silently truncated block"
      (let [r (ep/run-python-block (tpc/context ::ctx) "print('a')\nreturn 5" "t1/i1")]
        (expect (some? (:error r)))
        (expect (str/includes? (str (:message (:error r))) "'return' outside function"))))
  ;; AUTO-SETTLE must not touch a generator: it only drives OUR deferred tool
  ;; thunks. Driving anything with `.send` exhausted the generator and bound None.
  (it "a generator binding is not driven to exhaustion by the auto-settle"
      (let [ctx (tpc/context ::ctx)]
        (ep/run-python-block ctx "gen = (i for i in range(3))" "t1/i1")
        (let [r (ep/run-python-block ctx "print(list(gen))" "t1/i2")]
          (expect (nil? (:error r)))
          (expect (= "[0, 1, 2]" (str/trim (str (:stdout r))))))))
  ;; Same trap through a catch-all `__getattr__`: bs4's `Tag.__getattr__` answers
  ;; ANY missing non-dunder attribute with `find(name)` -> None, so the INSTANCE
  ;; probe `hasattr(v, "send")` was true and auto-settle handed the soup to the
  ;; coroutine driver, where `soup.send(None)` died with "TypeError: 'NoneType'
  ;; object is not callable" on EVERY top-level `soup = BeautifulSoup(...)`. The
  ;; shape is pinned with a local class: bs4 is a pip install in the sandbox, and
  ;; a suite test never reaches the network.
  (it "an object whose __getattr__ answers everything is not driven as a coroutine"
      (let [ctx
            (tpc/context ::ctx)

            r
            (ep/run-python-block ctx
                                 (str "class Anything:\n"
                                      "    def __getattr__(self, n):\n" "        return None\n"
                                      "obj = Anything()\n" "print(type(obj).__name__, obj.send)")
                                 "t1/i1")]

        (expect (nil? (:error r)))
        (expect (= "Anything None" (str/trim (str (:stdout r)))))))
  ;; CPython compiles `await` inside a lambda into an UNCATCHABLE host fault (a
  ;; null-sourceRange NullPointerException), so `except SyntaxError` around
  ;; compile() never sees it. Reject it up front with CPython's own message.
  (it "`await` inside a lambda is a normal SyntaxError, not an engine fault"
      (let [r
            (ep/run-python-block (tpc/context ::ctx) "f = lambda: await g()" "t1/i1")

            msg
            (str (:message (:error r)))]

        (expect (some? (:error r)))
        (expect (str/includes? msg "'await' outside async function"))
        (expect (not (str/includes? msg "NullPointerException")))
        (expect (= :python/syntax (:phase (:data (:error r)))))))
  ;; Regression, issue #134: a helper factored out of working top-level code died
  ;; with `SyntaxError: 'await' outside async function` — the block already runs as
  ;; a coroutine, so only the `def` line stood between the model and its helper.
  (it "a plain `def` that awaits is promoted to `async def`, not a SyntaxError"
      (let [ctx
            (tpc/context ::ctx)

            r
            (ep/run-python-block ctx
                                 (str "async def fetch(n):\n" "    return n * 2\n"
                                      "def show(n):\n" "    v = await fetch(n)\n"
                                      "    print('got', v)\n" "await show(21)")
                                 "t1/i1")]

        (expect (nil? (:error r)))
        (expect (= "got 42" (str/trim (str (:stdout r)))))))
  ;; The promotion is per-function: an `await` that belongs to a nested helper must
  ;; not drag the enclosing plain `def` into async.
  (it "promotes only the function whose OWN body awaits"
      (let [ctx
            (tpc/context ::ctx)

            r
            (ep/run-python-block
              ctx
              (str "import inspect\n" "async def one():\n"
                   "    return 1\n" "def outer():\n"
                   "    def inner():\n" "        return await one()\n"
                   "    return inner\n" "print(inspect.iscoroutinefunction(outer),"
                   " inspect.iscoroutinefunction(outer()))\n" "print(await outer()())")
              "t1/i2")]

        (expect (nil? (:error r)))
        (expect (= "False True\n1" (str/trim (str (:stdout r)))))))
  ;; Same class of trap: a bare starred target died with a raw
  ;; `UnsupportedOperationException: StoreVisitor: Starred`.
  (it "a bare starred assignment target is a SyntaxError, not a host fault"
      (let [ctx
            (tpc/context ::ctx)

            r
            (ep/run-python-block ctx "*only = [1]" "t1/i1")

            msg
            (str (:message (:error r)))]

        (expect (some? (:error r)))
        (expect (str/includes? msg "starred assignment target must be in a list or tuple"))
        (expect (not (str/includes? msg "UnsupportedOperationException")))
        ;; the legal forms still work
        (let [ok (ep/run-python-block ctx "p, *q = [1, 2, 3]\nprint(p, q)" "t1/i2")]
          (expect (nil? (:error ok)))
          (expect (= "1 [2, 3]" (str/trim (str (:stdout ok))))))))
  ;; A compile-phase SyntaxError is re-raised from the preamble, so its POSITION
  ;; must come from the exception itself — otherwise the boundary reports a
  ;; preamble line the user never wrote.
  (it "a compile-phase SyntaxError points at the USER's line"
      (tpc/with-own [ctx {}]
                    (let [r (ep/run-python-block ctx "x = 1\ny = 2\nf(a=1, a=2)" "t1/i1")]
                      (expect (= 3 (:line (:data (:error r)))))
                      (expect (str/includes? (str (:message (:error r))) "3: f(a=1, a=2)")))))
  ;; `globals().clear()` (or deleting an engine-owned name) is legal Python and
  ;; used to KILL the session: the host then called a null `__vis_run_async__` and
  ;; every later block died with a bare NullPointerException.
  (it "a block that wipes the globals does not kill the interpreter"
      (tpc/with-own [ctx {}]
                    (ep/run-python-block ctx "globals().clear()" "t1/i1")
                    (let [r (ep/run-python-block ctx "print('alive')" "t1/i2")]
                      (expect (nil? (:error r)))
                      (expect (= "alive" (str/trim (str (:stdout r))))))
                    (let [r (ep/run-python-block ctx "zz = 5\nprint(zz + 1)" "t1/i3")]
                      (expect (= "6" (str/trim (str (:stdout r))))))))
  (it "deleting the engine's own runtime name reinstalls it instead of wedging"
      (tpc/with-own [ctx {}]
                    (ep/run-python-block ctx "del __vis_run_async__" "t1/i1")
                    (let [r (ep/run-python-block ctx "print('still here')" "t1/i2")]
                      (expect (nil? (:error r)))
                      ;; re-install must not double-wrap `print` into infinite recursion
                      (expect (= "still here" (str/trim (str (:stdout r))))))))
  ;; Engine helpers (`__vis_settle__`, `__vis_Call__` …) live only in globals, so a
  ;; legal mid-block `globals().clear()` / `del __vis_settle__` used to make the
  ;; REST OF THE SAME BLOCK die with a bogus tool-is-inactive NameError. CPython
  ;; survives it (a frame captures its builtins); pinning the helpers into builtins
  ;; gives them the same survival rule as `print`.
  (it "a mid-block globals().clear() does not break the rest of the same block"
      (tpc/with-own [ctx {}]
                    (let [r (ep/run-python-block ctx
                                                 "import sys\nglobals().clear()\nprint('recovered')"
                                                 "t1/i1")]
                      (expect (nil? (:error r)))
                      (expect (= "recovered" (str/trim (str (:stdout r))))))))
  (it "deleting one engine helper mid-block keeps the rest of the block alive"
      (tpc/with-own [ctx {}]
                    (let [r (ep/run-python-block ctx "del __vis_settle__\nprint(1 + 1)" "t1/i1")]
                      (expect (nil? (:error r)))
                      (expect (= "2" (str/trim (str (:stdout r))))))))
  (it "a print after a globals().clear() in the same block still works"
      (tpc/with-own
        [ctx {}]
        (let [r (ep/run-python-block ctx "print('a')\nglobals().clear()\nprint('b')" "t1/i1")]
          (expect (nil? (:error r)))
          (expect (= "a\nb" (str/trim (str (:stdout r)))))))))

(defdescribe
  async-runtime-test
  "Async-by-default (maki-style on CPython): tools are DEFERRED, so `await` runs
   them ANYWHERE (incl. nested), a bare top-level call auto-settles, and an
   unawaited call that leaks into output repr's a loud hint instead of silently
   misbehaving. The await path AST-wraps the program in an `async def` (CPython
   rejects top-level await) and drives it, persisting assigned vars by name."
  (let [mk (fn []
             (tpc/context-with! ::ctx
                                {'echo (fn [x]
                                         (str "<" x ">"))}))]
    (it "await runs a NESTED deferred tool call"
        (let [r (ep/run-python-block (mk) "print(await echo(\"hi\"))" "t1/i1")]
          (expect (nil? (:error r)))
          (expect (= "<hi>" (clojure.string/trim (str (:stdout r)))))))
    (it "a bare top-level call auto-settles (runs without await)"
        ;; The statement's VALUE is dropped — nothing printed, nothing echoed — so
        ;; the settle is observed where it actually happens: the tool ran.
        (let [ran (atom [])
              ctx (tpc/context-with! ::ctx
                                     {'echo (fn [x]
                                              (swap! ran conj x)
                                              (str "<" x ">"))})
              r (ep/run-python-block ctx "echo(\"bare\")" "t1/i1")]

          (expect (nil? (:error r)))
          (expect (= ["bare"] @ran))
          (expect (= "" (out r)))))
    (it "print auto-settles an UNawaited nested call (shows the value, not the hint)"
        (let [r (ep/run-python-block (mk) "print(echo(\"oops\"))" "t1/i1")]
          (expect (nil? (:error r)))
          (expect (= "<oops>" (clojure.string/trim (str (:stdout r)))))))
    (it "an awaited assignment persists in the live interpreter across calls"
        (let [ctx (mk)
              r (ep/run-python-block ctx "kept = await echo(\"x\")\nprint(kept)" "t1/i1")]

          (expect (nil? (:error r)))
          (expect (= "<x>" (clojure.string/trim (str (:stdout r)))))
          ;; one interpreter — a later call still sees `kept`
          (expect (= "<x>" (out (ep/run-python-block ctx "print(kept)"))))))
    (it "auto-settles a bare deferred assignment in an await-bearing program"
        ;; `c = await echo("a")` forces the async path; the bare `res = echo("b")`
        ;; has NO await, yet must RUN (settle) so `res` is the value, not a thunk.
        (let [r (ep/run-python-block (mk)
                                     "c = await echo(\"a\")\nres = echo(\"b\")\nprint(res)"
                                     "t1/i1")]
          (expect (nil? (:error r)))
          (expect (= "<b>" (clojure.string/trim (str (:stdout r)))))))
    (it "auto-settles a bare deferred assignment EXACTLY once (no double-run)"
        (let [calls (atom 0)
              ctx (tpc/context-with! ::ctx
                                     {'tick (fn []
                                              (str "n" (swap! calls inc)))})
              r (ep/run-python-block ctx "c = await tick()\nres = tick()\nprint(res)" "t1/i1")]

          (expect (nil? (:error r)))
          ;; the bare `res = tick()` settles inline exactly once — not twice from
          ;; a redundant wrap + post-drive pass (and `tick` from the awaited form
          ;; ran once too).
          (expect (= "n2" (clojure.string/trim (str (:stdout r)))))
          (expect (= 2 @calls))))
    (it "await on an already-settled binding is harmless and returns the value"
        ;; THE trap: `x = patch(...)` auto-settles (runs the
        ;; tool, so `x` already holds the real result), then `await x` USED to
        ;; throw `TypeError: object ForeignList can't be used in 'await'
        ;; expression`. Now the stray await just yields the value back — we don't
        ;; care that it was already resolved.
        (let [r (ep/run-python-block (mk) "x = echo(\"a\")\nprint(await x)" "t1/i1")]
          (expect (nil? (:error r)))
          (expect (= "<a>" (clojure.string/trim (str (:stdout r)))))))
    (it "await on an already-settled binding does NOT re-run the tool"
        (let [calls (atom 0)
              ctx (tpc/context-with! ::ctx
                                     {'tick (fn []
                                              (str "n" (swap! calls inc)))})
              r (ep/run-python-block ctx "x = tick()\nprint(await x)" "t1/i1")]

          (expect (nil? (:error r)))
          ;; settled ONCE at assignment; the spurious await must not run it again.
          (expect (= "n1" (clojure.string/trim (str (:stdout r)))))
          (expect (= 1 @calls))))
    (it "await on a plain non-tool value is a no-op that returns it"
        (let [r (ep/run-python-block (mk) "v = 41\nprint((await v) + 1)" "t1/i1")]
          (expect (nil? (:error r)))
          (expect (= "42" (clojure.string/trim (str (:stdout r)))))))))

(defdescribe
  tool-failure-catchable-test
  "A raising tool surfaces its MESSAGE and is CATCHABLE in-block like any other
   error (issue #42). Host tool callables raise a foreign exception that derives
   from BaseException but NOT Exception; the driver re-raises the failure at the
   coroutine's OWN await point (wrapping a foreign one so ordinary `except
   Exception` also catches it, with a clean message), while an UNCAUGHT failure
   still maps to the same host tool-failure op-error."
  (let [mk (fn []
             (:python-context (tpc/new-context {'boom (fn [& _]
                                                        (throw (ex-info "boom message"
                                                                        {:type :vis/tool-failure
                                                                         :symbol :boom})))
                                                'echo (fn [x]
                                                        (str "<" x ">"))})))]
    (it "`except Exception` catches a tool failure and sees the clean message"
        (let [r (ep/run-python-block
                  (mk)
                  "try:\n    await boom()\nexcept Exception as e:\n    print('caught: ' + str(e))"
                  "t1/i1")]
          (expect (nil? (:error r)))
          (expect (= "caught: boom message" (clojure.string/trim (str (:stdout r)))))))
    (it "`except BaseException` catches it too"
        (let [r (ep/run-python-block
                  (mk)
                  "try:\n    await boom()\nexcept BaseException as e:\n    print('base: ' + str(e))"
                  "t1/i1")]
          (expect (nil? (:error r)))
          (expect (= "base: boom message" (clojure.string/trim (str (:stdout r)))))))
    (it "catching lets the block CONTINUE and run more tools"
        (let [r (ep/run-python-block
                  (mk)
                  "try:\n    await boom()\nexcept Exception:\n    pass\nprint(await echo(\"ok\"))"
                  "t1/i1")]
          (expect (nil? (:error r)))
          (expect (= "<ok>" (clojure.string/trim (str (:stdout r)))))))
    (it "a NON-matching except still surfaces the host tool-failure op-error"
        (let [r (ep/run-python-block (mk)
                                     "try:\n    await boom()\nexcept ValueError:\n    print('nope')"
                                     "t1/i1")]
          (expect (nil? (:stdout r)))
          (expect (= "boom message" (:message (:error r))))
          (expect (= :python/host (:phase (:data (:error r)))))
          (expect (= :vis/tool-failure (:type (:data (:error r)))))))
    (it "an UNCAUGHT tool failure maps to the host tool-failure op-error (message + data)"
        (let [r (ep/run-python-block (mk) "await boom()" "t1/i1")]
          (expect (nil? (:stdout r)))
          (expect (= "boom message" (:message (:error r))))
          (expect (= :python/host (:phase (:data (:error r)))))
          (expect (= :vis/tool-failure (:type (:data (:error r)))))
          (expect (= :boom (:symbol (:data (:error r)))))))))

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

(defdescribe run-python-block-form-eval-test
             ;; (R8 in-fence r["tN/iN/fF"] memory removed: context is print-only — a later
             ;; line uses ordinary Python variables, not an r[] dict.)
             ;; ONE success channel: what the block PRINTED. A trailing bare expression is
             ;; still EVALUATED (a bare tool call runs), but its value is dropped — there is
             ;; no second, value-shaped channel for a block that printed nothing.
             (it "E1 — comment is not a form; assign + bare expr echo nothing"
                 (let [r (ep/run-python-block (py-ctx) "# read it\ne1x = 41\ne1x")]
                   (expect (nil? (:error r)))
                   (expect (not (contains? r :result)))
                   (expect (nil? (:stdout r)))))
             (it "E2 — print() is the only channel a value comes back on"
                 (let [r (ep/run-python-block (py-ctx) "print(40 + 2)")]
                   (expect (nil? (:error r)))
                   (expect (not (contains? r :result)))
                   (expect (= "42" (str/trim (str (:stdout r)))))))
             (it "E3 — a trailing expression nobody printed is gone"
                 (let [r (ep/run-python-block (py-ctx) "e3a = 1\ne3b = 2\n(e3a, e3b)")]
                   (expect (nil? (:error r)))
                   (expect (not (contains? r :result)))
                   (expect (nil? (:stdout r)))))
             (it "E6 — a call expression runs; only what it printed comes back"
                 (let [r (ep/run-python-block (py-ctx) "e6 = str(99)\nprint(e6)")]
                   (expect (not (contains? r :result)))
                   (expect (= "99" (str/trim (str (:stdout r)))))))
             (it "a def is one form; a following call evaluates"
                 (let [r (ep/run-python-block (py-ctx) "def e_f():\n    return 7\nprint(e_f())")]
                   (expect (= "7" (str/trim (str (:stdout r)))))))
             (it "E7 — evaluation stops at the first erroring form; later forms do not run"
                 (let [r (ep/run-python-block (py-ctx) "e7x = 1\ne7_boom\ne7y = 2")]
                   (expect (not (contains? r :result)))
                   (expect (= :python/runtime (get-in (:error r) [:data :phase])))))
             (it "E7b — a NameError for an undefined TOOL gets an enrichment hint"
                 (let [r
                       (ep/run-python-block (py-ctx) "definitely_not_a_tool_zzz(\"x\")")

                       err
                       (:error r)]

                   (expect (true? (get-in err [:data :name-undefined?])))
                   (expect (= "definitely_not_a_tool_zzz" (get-in err [:data :undefined-name])))
                   (expect (str/includes? (:message err) "apropos")))))

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

   The roots are the CONFINEMENT: the interpreter denies what falls outside
   them, so these sandboxes are rooted at the project directory and `/etc` is
   what lies outside."
  (let [mk (fn []
             (:python-context (tpc/new-context {} (constantly [(System/getProperty "user.dir")]))))]
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

