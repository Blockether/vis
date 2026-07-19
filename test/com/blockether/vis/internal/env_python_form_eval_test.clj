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
      `map-polyglot-error` tags these `:prose-leading? true` with an actionable
      message, while NEVER mislabeling a genuine code typo as prose."
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [com.blockether.vis.internal.env-python :as ep]
            [com.blockether.vis.internal.foundation.language-surface :as language-surface]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [org.graalvm.polyglot PolyglotException]))

(defn- classify
  "Parse `code` (parse-only — never evaluates the forms). On SyntaxError, run it
   through `map-polyglot-error` and return the op-error map; otherwise `:parsed`."
  [code]
  (try (ep/count-top-level-forms code)
       :parsed
       (catch PolyglotException e (ep/map-polyglot-error e code))))

(defn- prose-leading? [code] (boolean (get-in (classify code) [:data :prose-leading?])))

(defdescribe
  prose-leading-guard-test
  ;; --- positives: real failing replies seen live (sessions 2e98be97 / 4c0eff03)
  (it "flags markdown heading + prose (apostrophe -> unterminated string)"
      (expect (prose-leading?
                "## Root cause found\n\n`/draft` clones via rift's CoW.\ndone(\"x\")")))
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

(def ^:private py-ctx
  ;; One shared GraalPy sandbox for the eval cases; unique var names per case
  ;; avoid cross-test global contamination. No tools needed — plain Python
  ;; exercises the per-form eval semantics (R1–R7).
  (delay (:python-context (ep/create-python-context {}))))

(defdescribe
  sandbox-auto-import-test
  (it "makes shlex available without an import in run_python code"
      (let [r (ep/run-python-block @py-ctx "shlex.quote('a b')")]
        (expect (nil? (:error r)))
        (expect (= "'a b'" (:result r)))))
  (it "makes re available without an import in run_python code"
      (let [r (ep/run-python-block @py-ctx "re.sub(r'\\d+', '#', 'a12b3')")]
        (expect (nil? (:error r)))
        (expect (= "a#b#" (:result r)))))
  (it "makes hashlib available without an import in run_python code"
      (let [r (ep/run-python-block @py-ctx "hashlib.sha256(b'hello world').hexdigest()")]
        (expect (nil? (:error r)))
        (expect (= "b94d27b9934d3e08a52e52d7da7dabfac484efe37a5380ee9088f7ace2efcde9"
                   (:result r)))))
  (it "makes json available without an import in run_python code"
      (let [r (ep/run-python-block @py-ctx "json.dumps({'b': 2, 'a': 1}, sort_keys=True)")]
        (expect (nil? (:error r)))
        (expect (= "{\"a\": 1, \"b\": 2}" (:result r)))))
  (it "makes os available without an import in run_python code"
      (let [r (ep/run-python-block @py-ctx "os.path.join('a', 'b')")]
        (expect (nil? (:error r)))
        (expect (= "a/b" (:result r)))))
  (it "makes sys available without an import in run_python code"
      (let [r (ep/run-python-block @py-ctx "isinstance(sys.maxsize, int)")]
        (expect (nil? (:error r)))
        (expect (= true (:result r)))))
  (it "makes collections available without an import in run_python code"
      (let [r (ep/run-python-block @py-ctx "dict(collections.Counter('aab'))")]
        (expect (nil? (:error r)))
        ;; strings-only boundary: dict keys come back as VERBATIM strings
        (expect (= {"a" 2 "b" 1} (:result r)))))
  (it "makes pathlib and Path available without an import in run_python code"
      (let [r (ep/run-python-block @py-ctx
                                   "pathlib.Path('a/b').name == 'b' and Path('a/b').name == 'b'")]
        (expect (nil? (:error r)))
        (expect (= true (:result r)))))
  (it "makes textwrap available without an import in run_python code"
      (let [r (ep/run-python-block @py-ctx "textwrap.shorten('alpha beta gamma', width=11)")]
        (expect (nil? (:error r)))
        (expect (= "alpha [...]" (:result r)))))
  (it "makes base64 available without an import in run_python code"
      (let [r (ep/run-python-block @py-ctx "base64.b64encode(b'hi').decode()")]
        (expect (nil? (:error r)))
        (expect (= "aGk=" (:result r)))))
  (it "makes math available without an import in run_python code"
      (let [r (ep/run-python-block @py-ctx "round(math.sqrt(2) + math.pi, 6)")]
        (expect (nil? (:error r)))
        (expect (= 4.555806 (:result r)))))
  (it
    "makes glob available without an import in run_python code"
    (let [r (ep/run-python-block @py-ctx "hasattr(glob, 'glob') and callable(glob.glob)")]
      (expect (nil? (:error r)))
      (expect (= true (:result r))))
    (it "makes builtins available without an import in run_python code"
        (let [r (ep/run-python-block @py-ctx
                                     "hasattr(builtins, 'len') and builtins.len([1, 2]) == 2")]
          (expect (nil? (:error r)))
          (expect (= true (:result r)))))
    (it
      "does not expose auto-imported modules as apropos-listed tools/globals"
      (let
        [r
         (ep/run-python-block
           @py-ctx
           "bool(set(['shlex', 'json', 're', 'hashlib', 'glob', 'os', 'sys', 'collections', 'pathlib', 'Path', 'textwrap', 'base64', 'math', 'builtins']) & set().union(*(set(apropos(m)) for m in ['shlex', 'json', 're', 'hashlib', 'glob', 'os', 'sys', 'collections', 'pathlib', 'Path', 'textwrap', 'base64', 'math', 'builtins'])))")]
        (expect (nil? (:error r)))
        (expect (= false (:result r)))))))

(defdescribe
  verb-arg-boundary-test
  ;; Regression: a Python dict passed to a wrapped Clojure verb crosses the
  ;; boundary via `->clj`, which KEYWORDIZES every dict key (snake verbatim).
  ;; The `sub_loop` verb once read its opts with the STRING key "models" and so
  ;; silently got nil — the child ran on the DEFAULT model, not the proposed
  ;; one. This pins the shape so verb authors read `:models`, not "models".
  (it
    "dict args arrive with KEYWORD-snake keys; values pass through (strings, vectors)"
    (let [captured
          (atom nil)

          {:keys [python-context]}
          (ep/create-python-context
            {'capture_args (fn [prompt subctx & more]
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
                   (ep/create-python-context {'capture_args (fn [& args]
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
             (:python-context (ep/create-python-context {'patch (fn [& _]
                                                                  "patched")})))]
    (it "refuses to overwrite a bound tool name and keeps the callable usable"
        (let [ctx (mk)
              r1 (ep/run-python-block ctx "patch = 'not callable'" "t1/i1")
              r2 (ep/run-python-block ctx "patch({'path': 'x'})" "t1/i2")]

          (expect (= :python/protected-name (get-in r1 [:error :data :phase])))
          (expect (str/includes? (get-in r1 [:error :message]) "patch"))
          (expect (nil? (:error r2)))
          (expect (= "patched" (:result r2)))))
    (it "allows ordinary variables while still awaiting protected tools"
        (let [r (ep/run-python-block (mk) "css = 'app.css'
await patch({'path': css})" "t1/i1")]
          (expect (nil? (:error r)))
          (expect (= "patched" (:result r)))))
    (it "also protects tools added after context creation"
        (let [ctx (:python-context (ep/create-python-context {}))]
          (ep/set-python-binding! ctx
                                  'later_patch
                                  (fn [& _]
                                    "late"))
          (let [r1 (ep/run-python-block ctx "later_patch = 'oops'" "t1/i1")
                r2 (ep/run-python-block ctx "later_patch()" "t1/i2")]

            (expect (= :python/protected-name (get-in r1 [:error :data :phase])))
            (expect (nil? (:error r2)))
            (expect (= "late" (:result r2))))))
    ;; The guard stays STRONG (rebinding ANY bound tool is rejected). The
    ;; `test`-collision was fixed at the SOURCE: the language facade verbs were
    ;; renamed off the commonest variable/builtin names (`test`→`run_tests`,
    ;; `format`→`format_code`). So `run_tests` is still hard-protected, while
    ;; `test` — no longer a tool — is a free variable the model may bind.
    (it "still hard-protects the renamed facade verb run_tests"
        (let [ctx (:python-context (ep/create-python-context {'run_tests (fn [& _]
                                                                           "ran")}))
              r1 (ep/run-python-block ctx "run_tests = 'oops'" "t1/i1")
              r2 (ep/run-python-block ctx "run_tests('go')" "t1/i2")]

          (expect (= :python/protected-name (get-in r1 [:error :data :phase])))
          (expect (nil? (:error r2)))
          (expect (= "ran" (:result r2)))))
    ;; A `for`/`with` loop TARGET is transient scratch — it stays function-local
    ;; to the wrapped block, so it neither persists nor clobbers the callable.
    ;; It must NOT trip the durable-rebind guard.
    (it "allows a `for` loop target that shadows a tool name and keeps the callable usable"
        (let [ctx (mk)
              r1 (ep/run-python-block ctx "for patch in ['a', 'b']:\n    pass" "t1/i1")
              r2 (ep/run-python-block ctx "patch({'path': 'x'})" "t1/i2")]

          (expect (nil? (:error r1)))
          (expect (nil? (:error r2)))
          (expect (= "patched" (:result r2)))))
    (it "lets the model bind `test` and `format` as ordinary variables (not tools)"
        (let [ctx (:python-context (ep/create-python-context {'patch (fn [& _]
                                                                       "patched")}))
              r (ep/run-python-block
                  ctx
                  "test = 'promise_pool.test.ts'\nformat = 'csv'\nawait patch({'path': test})"
                  "t1/i1")]

          (expect (nil? (:error r)))
          (expect (= "patched" (:result r)))))))

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
                                "repl_stop" "repl_connect"}
                              facade)))))

(defdescribe
  live-interpreter-persistence-test
  "ONE persistent interpreter per session: a variable the model binds in one
   block is still live in the NEXT block on the same context — globals persist
   NATURALLY (no rebind, no pickle, no per-turn fresh sandbox)."
  (it "a bound variable is still live in a later call on the same context"
      (let [ctx (:python-context (ep/create-python-context {}))]
        (ep/run-python-block ctx "kept_v = 41")
        (let [r2 (ep/run-python-block ctx "print(kept_v + 1)")]
          (expect (nil? (:error r2)))
          (expect (= "42" (clojure.string/trim (str (:stdout r2)))))))))

(defdescribe
  async-runtime-test
  "Async-by-default (maki-style on GraalPy): tools are DEFERRED, so `await` runs
   them ANYWHERE (incl. nested), a bare top-level call auto-settles, and an
   unawaited call that leaks into output repr's a loud hint instead of silently
   misbehaving. The await path AST-wraps the program in an `async def` (GraalPy
   rejects top-level await) and drives it, persisting assigned vars by name."
  (let [mk (fn []
             (:python-context (ep/create-python-context {'echo (fn [x]
                                                                 (str "<" x ">"))})))]
    (it "await runs a NESTED deferred tool call"
        (let [r (ep/run-python-block (mk) "print(await echo(\"hi\"))" "t1/i1")]
          (expect (nil? (:error r)))
          (expect (= "<hi>" (clojure.string/trim (str (:stdout r)))))))
    (it "a bare top-level call auto-settles (runs without await)"
        (let [r (ep/run-python-block (mk) "echo(\"bare\")" "t1/i1")]
          (expect (nil? (:error r)))
          (expect (= "<bare>" (:result r)))))
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
          (expect (= "<x>" (:result (ep/run-python-block ctx "kept"))))))
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
              ctx (:python-context (ep/create-python-context {'tick (fn []
                                                                      (str "n"
                                                                           (swap! calls inc)))}))
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
              ctx (:python-context (ep/create-python-context {'tick (fn []
                                                                      (str "n"
                                                                           (swap! calls inc)))}))
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
  asyncio-shim-test
  "The model's habitual `asyncio.run(...)` / `asyncio.gather(...)` is ROUTED onto
   our virtual-thread driver instead of the sandbox-excluded real asyncio (which
   would trip a native `socket was excluded` crash). We meet the model where it
   is — `import asyncio` is AST-rewritten to bind the shim — so no prompt rule is
   needed and the model never fights an opaque event-loop error."
  ;; `__vis_par__` (the host virtual-thread pool backing `gather`) is supplied by
  ;; loop.clj in production; the test wires a minimal sequential stand-in so the
  ;; gather path resolves — echo is sync, so order/result are deterministic.
  (let [par
        (fn [& thunks]
          (let [ts (if (and (= 1 (count thunks)) (sequential? (first thunks)))
                     (vec (first thunks))
                     (vec thunks))]
            (mapv (fn [t]
                    (if (instance? org.graalvm.polyglot.Value t)
                      (.execute ^org.graalvm.polyglot.Value t (object-array 0))
                      (t)))
                  ts)))

        mk
        (fn []
          (:python-context (ep/create-python-context {'echo (fn [x]
                                                              (str "<" x ">"))
                                                      (symbol "__vis_par__") par})))]

    (it
      "asyncio.run(main()) drives a coroutine that awaits tools"
      (let
        [r
         (ep/run-python-block
           (mk)
           (str
             "import asyncio\n"
             "async def main():\n    a = await echo(\"x\")\n    b = await echo(\"y\")\n    return a + b\n"
             "print(asyncio.run(main()))")
           "t1/i1")]
        (expect (nil? (:error r)))
        (expect (= "<x><y>" (clojure.string/trim (str (:stdout r)))))))
    (it "asyncio.gather runs awaitables concurrently via our gather"
        (let [r (ep/run-python-block
                  (mk)
                  (str
                    "import asyncio\n"
                    "async def main():\n    return await asyncio.gather(echo(\"a\"), echo(\"b\"))\n"
                    "print(asyncio.run(main()))")
                  "t1/i1")]
          (expect (nil? (:error r)))
          (let [out (str (:stdout r))]
            (expect (clojure.string/includes? out "<a>"))
            (expect (clojure.string/includes? out "<b>")))))
    (it "NO native event-loop/socket crash leaks from asyncio use"
        (let [r
              (ep/run-python-block
                (mk)
                (str "import asyncio\nasync def main():\n    return await echo(\"z\")\n"
                     "print(asyncio.run(main()))")
                "t1/i1")

              blob
              (str (:error r) (:stdout r))]

          (expect (nil? (:error r)))
          (expect (not (clojure.string/includes? blob "socket was excluded")))
          (expect (not (clojure.string/includes? blob "PosixSupport")))))
    (it "from asyncio import run rebinds to the shim; gather stays the builtin"
        (let [r (ep/run-python-block
                  (mk)
                  (str "from asyncio import run, gather\n"
                       "async def m():\n    return await gather(echo(\"p\"), echo(\"q\"))\n"
                       "print(run(m()))")
                  "t1/i1")]
          (expect (nil? (:error r)))
          (expect (clojure.string/includes? (str (:stdout r)) "<p>"))))
    (it "import socket is a no-op; touching socket is a clean NameError, not a native crash"
        (let [r
              (ep/run-python-block (mk) "import socket\nsocket.socket()" "t1/i1")

              blob
              (str (get-in r [:error :message]) (:stdout r))]

          (expect (some? (:error r)))
          (expect (not (clojure.string/includes? blob "PosixSupport")))
          (expect (clojure.string/includes? blob "socket"))))))

(defdescribe
  guest-threads-test
  "Guest Python may CREATE threads — importlib's import machinery, `threading`,
   and libs that allocate `_thread` locks all need it; denying it surfaced an
   opaque `SecurityException: Operation is not allowed for:` mid-run. Threads
   share the GIL-like context and still can't reach IO / native / host, so the
   dangerous capabilities stay denied."
  (let [mk (fn []
             (:python-context (ep/create-python-context {})))]
    (it
      "threading.Thread runs to completion"
      (let
        [r
         (ep/run-python-block
           (mk)
           "import threading\nout=[]\nt=threading.Thread(target=lambda: out.append(7))\nt.start()\nt.join()\nprint(out[0])"
           "t1/i1")]
        (expect (nil? (:error r)))
        (expect (= "7" (clojure.string/trim (str (:stdout r)))))))
    (it "_thread.allocate_lock works (the import-machinery / lock path)"
        (let
          [r (ep/run-python-block
               (mk)
               "import _thread\nlk=_thread.allocate_lock()\nlk.acquire(); lk.release()\nprint('ok')"
               "t1/i1")]
          (expect (nil? (:error r)))
          (expect (= "ok" (clojure.string/trim (str (:stdout r)))))))
    (it "the filesystem stays DENIED — enabling threads is not a sandbox hole"
        (let [r (ep/run-python-block (mk) "print(open('/etc/hosts').read())" "t1/i1")]
          (expect (some? (:error r)))))))

(defdescribe
  file-exists-binding-test
  "`file-exists` binds as the snake_case `file_exists` tool in the sandbox — no `is_exists`/`exists` alias."
  (it "exposes file_exists and NOT the old is_exists name"
      (let [ctx
            (:python-context (ep/create-python-context
                               ;; strings-only boundary: tool results are built with STRING
                               ;; keys at the source (a keyword-keyed result now throws).
                               {'file-exists (fn [path]
                                               {"path" path "exists" (= path "present.txt")})}))

            via-file
            (ep/run-python-block ctx "await file_exists('present.txt')" "t1/i1")

            via-missing
            (ep/run-python-block ctx "await file_exists('missing.txt')" "t1/i2")

            via-old
            (ep/run-python-block ctx "is_exists('present.txt')" "t1/i3")]

        (expect (nil? (:error via-file)))
        (expect (nil? (:error via-missing)))
        (expect (= {"path" "present.txt" "exists" true} (:result via-file)))
        (expect (= {"path" "missing.txt" "exists" false} (:result via-missing)))
        (expect (str/includes? (get-in via-old [:error :message]) "`is_exists` is not defined"))))
  (it "removing the binding makes file_exists undefined"
      (let [ctx (:python-context (ep/create-python-context {}))]
        (ep/set-python-binding! ctx
                                'file-exists
                                (fn [path]
                                  {"path" path "exists" true}))
        (expect (= {"path" "dynamic.txt" "exists" true}
                   (:result (ep/run-python-block ctx "await file_exists('dynamic.txt')" "t1/i3"))))
        (ep/remove-python-binding! ctx 'file-exists)
        (expect (str/includes? (get-in
                                 (ep/run-python-block ctx "file_exists('dynamic.txt')" "t1/i4")
                                 [:error :message])
                               "`file_exists` is not defined")))))

(defdescribe
  run-python-block-form-eval-test
  ;; (R8 in-fence r["tN/iN/fF"] memory removed: context is print-only — a later
  ;; line uses ordinary Python variables, not an r[] dict.)
  (it "E1 — comment is not a form; assign + bare expr; last value is the result"
      (let [r (ep/run-python-block @py-ctx "# read it\ne1x = 41\ne1x")]
        (expect (= 41 (:result r)))
        (expect (nil? (:error r)))))
  (it "E2 — a single value-returning expression echoes its value"
      (let [r (ep/run-python-block @py-ctx "40 + 2")]
        (expect (= 42 (:result r)))))
  (it "E3 — multiple statements; the trailing tuple echoes both"
      (let [r (ep/run-python-block @py-ctx "e3a = 1\ne3b = 2\n(e3a, e3b)")]
        (expect (= [1 2] (:result r)))))
  (it "E6 — a call expression echoes its return value"
      (let [r (ep/run-python-block @py-ctx "str(99)")]
        (expect (= "99" (:result r)))))
  (it "a def is one form; a following call evaluates"
      (let [r (ep/run-python-block @py-ctx "def e_f():\n    return 7\ne_f()")]
        (expect (= 7 (:result r)))))
  (it "E7 — evaluation stops at the first erroring form; later forms do not run"
      (let [r (ep/run-python-block @py-ctx "e7x = 1\ne7_boom\ne7y = 2")]
        (expect (nil? (:result r)))
        (expect (= :python/runtime (get-in (:error r) [:data :phase])))))
  (it "E7b — a NameError for an undefined TOOL gets an enrichment hint (toggled-off extension)"
      (let [r
            (ep/run-python-block @py-ctx "shell_run(\"echo hi\")")

            err
            (:error r)]

        (expect (true? (get-in err [:data :name-undefined?])))
        (expect (= "shell_run" (get-in err [:data :undefined-name])))
        (expect (str/includes? (:message err) "apropos"))
        (expect (str/includes? (:message err) ":shell/enabled")))))

(defdescribe
  sanitize-cause-data-test
  "Tool ex-data rides into the op-error `:data` MINUS host noise: the nested
   `:tool-result` envelope (a verbatim copy of the same failure) and the Java
   `:trace` are dropped; an inner `:error` that adds nothing over the
   top-level message disappears entirely; actionable fields survive."
  (it "collapses a :vis/tool-failure to type+symbol when the inner error is just the message"
      (let [msg
            "rg spec has unknown keys: spec."

            out
            (#'ep/sanitize-cause-data
             {:type :vis/tool-failure
              :symbol :rg
              :error {:message msg :trace "clojure.lang.ExceptionInfo: ...\nframe - f.clj:1"}
              :tool-result {:result nil :success? false :error {:message msg :trace "..."}}}
             msg)]

        (expect (= {:type :vis/tool-failure :symbol :rg} out))))
  (it "keeps actionable inner-error fields, sans trace"
      (let [out (#'ep/sanitize-cause-data
                 {:type :tool/banned :error {:message "blocked" :reason :policy :trace "..."}}
                 "other message")]
        (expect (= {:type :tool/banned :error {:message "blocked" :reason :policy}} out))))
  (it "passes non-map :error through untouched"
      (expect (= {:type :x :error "boom"}
                 (#'ep/sanitize-cause-data {:type :x :error "boom" :tool-result {}} "boom")))))

(defdescribe
  no-auto-repair-test
  "Auto-repair and fabrication/glued detection were REMOVED (2026-06-21). A reply
   that fails to split is NOT salvaged — it errors as a plain SyntaxError and the
   model resends clean code. With native tool calling (one run_python code arg)
   the block-concat causes don't arise; :auto-repaired is always nil."
  (it "GLUED top-level forms ERROR as a SyntaxError (not repaired)"
      (let [r (ep/run-python-block @py-ctx "len([1,2])abs(-3)")]
        (expect (nil? (:result r)))
        (expect (some? (:error r)))
        (expect (= :python/syntax (get-in r [:error :data :phase])))
        (expect (nil? (:auto-repaired r)))))
  (it "a PARROTED transcript tail (```ctx + r[...]= echoes) ERRORS as a SyntaxError, not salvaged"
      (let [r (ep/run-python-block @py-ctx
                                   (str "x_e2e = 1\nx_e2e + 41\n"
                                        "```ctx\n[\"env\"][\"nrepl\"] = 7888\n# tool results\n"
                                        "r[\"t4/i1/f1\"] = {\"files\": [\"a.clj\"]}"))]
        (expect (nil? (:result r)))
        (expect (some? (:error r)))
        (expect (= :python/syntax (get-in r [:error :data :phase])))
        (expect (nil? (:auto-repaired r)))))
  (it "clean Python still runs untouched (no false repair)"
      (let [r (ep/run-python-block @py-ctx "len([1,2,3])")]
        (expect (= 3 (:result r)))
        (expect (nil? (:error r)))
        (expect (nil? (:auto-repaired r))))))

(defdescribe
  sandbox-denial-hint-test
  "A sandbox capability denial (filesystem / native / process) maps to an
   ACTIONABLE hint steering to cat / repl_eval — not the opaque PermissionError /
   `SecurityException: Operation is not allowed for:` the model kept hitting when
   it reached for importlib.exec_module / open() on a project file."
  (let [mk (fn []
             (:python-context (ep/create-python-context {})))]
    (it "open() denial → hint points at cat(path) + repl_eval"
        (let [m (get-in (ep/run-python-block (mk) "open('/etc/hosts').read()" "t1/i1")
                        [:error :message])]
          (expect (some? m))
          (expect (clojure.string/includes? (str m) "cat(path)"))
          (expect (clojure.string/includes? (str m) "repl_eval"))))
    (it "importlib exec_module on a project file → the same steer"
        (let [m (get-in
                  (ep/run-python-block
                    (mk)
                    (str "import importlib.util\n"
                         "spec = importlib.util.spec_from_file_location('x', '/tmp/zz_nope.py')\n"
                         "mod = importlib.util.module_from_spec(spec)\n"
                         "spec.loader.exec_module(mod)")
                    "t1/i1")
                  [:error :message])]
          (expect (clojure.string/includes? (str m) "repl_eval"))))))

(defdescribe
  precise-hint-test
  "More precise hints by what actually failed — beyond the generic parser error."
  (let [mk (fn []
             (:python-context (ep/create-python-context {(quote lst) (fn []
                                                                       [1 2 3])})))]
    (it "IndentationError → an indentation-specific hint"
        (let [m (get-in (ep/run-python-block (mk) "if True:\nx = 1" "t1/i1") [:error :message])]
          (expect (clojure.string/includes? (str m) "INDENTATION"))))
    (it ".get on a FOREIGN tool result → bracket/index hint"
        (let [m (get-in (ep/run-python-block (mk) "r = await lst()\nprint(r.get(\"x\"))" "t1/i1")
                        [:error :message])]
          (expect (clojure.string/includes? (str m) "FOREIGN"))))))
