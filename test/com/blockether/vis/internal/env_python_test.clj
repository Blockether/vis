(ns com.blockether.vis.internal.env-python-test
  "GraalPy sandbox behaviour that needs a REAL context: the proxy→dict boundary
   fix and the print-capture of tool results. Boots ONE context for the ns."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.doc-corpus :as doc-corpus]
            [com.blockether.vis.internal.env-python :as ep]
            [com.blockether.vis.internal.extension :as ext]
            [com.blockether.vis.internal.runtime-settings :as rt]
            [com.blockether.vis.test-python-context :as tpc]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [org.graalvm.polyglot Context]
           [org.graalvm.polyglot.proxy ProxyExecutable]))

(defdescribe
  canonical-python-literal-test
  (it
    "renders boundary data without a GraalPy printer context"
    (let
      [data
       (array-map "none" nil "flags" [true false] "text" "a\n\"\\\t" "nested" (array-map "x" 1))]
      (expect
        (=
          "{\"none\": None, \"flags\": [True, False], \"text\": \"a\\n\\\"\\\\\\t\", \"nested\": {\"x\": 1}}"
          (ep/ctx->python-str data)))))
  (it "keeps scalar and temporal representations Python-compatible"
      (expect (= ["nan" "inf" "-inf" "\"1970-01-01T00:00:00Z\""
                  "\"00000000-0000-0000-0000-000000000001\""]
                 (mapv ep/ctx->python-str
                       [##NaN ##Inf ##-Inf (java.util.Date. 0)
                        (java.util.UUID/fromString "00000000-0000-0000-0000-000000000001")]))))
  (it "preserves the historical multiline layout at the 100-column boundary"
      (let
        [rendered (ep/ctx->python-str (array-map "first" (apply str (repeat 100 "x"))
                                                 "second" [1 2]))]
        (expect (str/starts-with? rendered "{\n \"first\": "))
        (expect (str/includes? rendered "\n \"second\": [1, 2]\n}")))))

(defdescribe auto-imported-python-names-test
             (it "makes every advertised Python name available without an import"
                 (let
                   [ctx
                    (tpc/shared)

                    names
                    (ep/ctx->python-str ep/AUTO_IMPORTED_PYTHON_NAMES)

                    result
                    (ep/run-python-block
                      ctx
                      (str "names = " names
                           "\n" "print([name for name in names if not hasattr(builtins, name)])"))]

                   (expect (= "[]\n" (:stdout result))))))

(defdescribe cold-context-local-time-test
             ;; GraalPy leaves the time module state (`currentZoneId`) null until `time`
             ;; is imported, so the FIRST local-time call in a cold context used to die
             ;; with `NPE: Cannot read field "currentZoneId"`. The eager `time` import in
             ;; `auto-imports-python` initializes it - this guards that.
             (it "converts a local timestamp as the first call in a fresh context"
                 (let
                   [ctx
                    (tpc/shared)

                    result
                    (ep/run-python-block ctx
                                         (str "print(datetime.datetime.fromtimestamp(0).year)\n"
                                              "print(time.localtime(0).tm_year)\n"
                                              "from datetime import datetime as _dt\n"
                                              "print(_dt.now().year > 2000)"))]

                   (expect (= "1970\n1970\nTrue\n" (:stdout result))))))

(defdescribe
  block-error-fidelity-test
  "The model must ALWAYS see its own Python error. The caret/position walk
   (`__vis_error_pos__`) reads Truffle traceback frames, and on a warm
   JIT-compiled context that walk can die with an internal
   `NullPointerException: Null receiver values are not supported by libraries`
   that guest code cannot catch. It used to run INSIDE the guest `except`, so
   the internal fault REPLACED the real exception and every failing block
   surfaced as `INTERNAL engine/tool fault - a host call returned null`. The
   walk now runs on the HOST side, where it is catchable: a broken position
   walk may only cost the caret span, never the message."
  (it "reports the real Python exception for an uncaught error"
      (let
        [ctx
         (tpc/shared)

         err
         (:error (ep/run-python-block ctx "raise ValueError('probe-real')"))]

        (expect (str/includes? (:message err) "ValueError: probe-real"))
        (expect (= :python/runtime (:phase (:data err))))))
  (it "keeps the real exception when the position walk itself fails"
      (let
        [ctx
         (tpc/shared)

         _
         (ep/run-python-block ctx
                              (str "def __vis_err_pos_now__():\n"
                                   "    raise RuntimeError('simulated truffle fault')\n"
                                   "globals()['__vis_err_pos_now__'] = __vis_err_pos_now__\n"))

         err
         (:error (ep/run-python-block ctx "raise ValueError('probe-degraded')"))]

        (expect (str/includes? (:message err) "ValueError: probe-degraded"))
        (expect (not (str/includes? (:message err) "host call returned null"))))))

(defdescribe python-binding-aliases-test
             ;; A native tool is reachable in the sandbox under its canonical Python name
             ;; PLUS the intentional compatibility aliases; a missing alias is a bare
             ;; NameError to the model, which reads as "the tool is gone" and invites a spin.
             (it "exposes grep as grep/find_files/find and unaliased tools as themselves"
                 (expect (= ["grep" "find_files" "find"] (ep/python-binding-names 'grep)))
                 (expect (= ["shell"] (ep/python-binding-names 'shell)))
                 (expect (= ["_shell_logs"] (ep/python-binding-names '_shell-logs))))
             (it "routes every alias to the SAME tool in a live context"
                 (let
                   [ctx
                    (tpc/shared-with! {'grep (fn grep-stub [& args]
                                               {"op" "grep" "args" (vec args)})})

                    result
                    (ep/run-python-block ctx
                                         (str "print(grep('a')['op'], grep('a')['args'])\n"
                                              "print(find('x')['op'], find_files('x')['args'])"))]

                   (expect (= "grep ['a']\ngrep ['x']\n" (:stdout result))))))

(defdescribe
  proxy-and-capture-test
  (let
    [env
     (ep/create-python-context {})

     ctx
     (:python-context env)]

    (it "a raw tool-result proxy is NOT json-serializable; after settle it's a REAL mutable dict"
        ;; `test_proxy` is bound via ->py → a ProxyHashMap (ForeignDict). It passes
        ;; isinstance(dict) but json.dumps() raises and it is read-only — the silent
        ;; friction. Assigning it auto-settles → __vis_pyify__ → a REAL python dict:
        ;; json.dumps works, it's mutable, and nested maps are converted too.
        ;; `eof nil` reproduces cat's `:next-offset nil` — ->py stores Java null which
        ;; GraalPy surfaces as ForeignNone (`x is None` is False); pyify must normalize
        ;; it or json.dumps chokes.
        (ep/bind-and-bump! env 'test_proxy {"op" "cat" "a" {"b" 1} "eof" nil})
        (let
          [r (ep/run-python-block
               ctx
               (str "import json\n" "try:\n"
                    "    json.dumps(test_proxy); raw_json = True\n" "except Exception:\n"
                    "    raw_json = False\n" "x = test_proxy\n" ;; auto-settle → pyify → real dict
                    "x['added'] = 7\n"                          ;; mutation works on a real dict
                    "post = (isinstance(x, dict) and (json.dumps(x) is not None)\n"
                    "        and x['added'] == 7 and isinstance(x['a'], dict))\n"
                    "print(['raw_json', raw_json, 'post', post])"))]
          (expect (re-find #"\['raw_json', False, 'post', True\]" (str (:stdout r))))))
    (it
      "MEASURE: pyify cost across sizes (session-scale ~30/100 vs a large 5000-entry result)"
      (ep/bind-and-bump! env
                         'sess_proxy
                         (into {}
                               (for [i (range 30)]
                                 [(str "k" i) {"v" i "w" nil}]))) ;; session-scale
      (ep/bind-and-bump! env
                         'med_proxy
                         (into {}
                               (for [i (range 100)]
                                 [(str "k" i) {"v" i "w" nil}]))) ;; generous session
      (ep/bind-and-bump! env
                         'big_proxy
                         (into {}
                               (for [i (range 5000)]
                                 [(str "k" i) {"v" i "w" nil}])))
      (let
        [r
         (ep/run-python-block
           ctx
           ;; pyify fires at TOP-LEVEL settle only — time each top-level assignment.
           (str
             "import time\n"
             "t0 = time.perf_counter()\n" "a = sess_proxy\n" ;; settle → pyify (30 nested)
             "t1 = time.perf_counter()\n" "b = med_proxy\n"  ;; settle → pyify (100 nested)
             "t2 = time.perf_counter()\n" "c = big_proxy\n"  ;; settle → pyify (5000 nested)
             "t3 = time.perf_counter()\n"
             "print('pyify_ms', {'n30': round((t1-t0)*1000,3), 'n100': round((t2-t1)*1000,3), 'n5000': round((t3-t2)*1000,3), 'lens': [len(a), len(b), len(c)]})"))]
        (println "PERF>>>" (:stdout r))
        (expect (re-find #"n30" (str (:stdout r))))))
    (it
      "captures a REAL tool result (proxy→__VisResult__) by TYPE; a model dict with 'op' is NOT captured"
      ;; `tp` is a HOST proxy with 'op' → pyify marks it __VisResult__. A model-built
      ;; dict with 'op' is a PLAIN dict → not a __VisResult__ → correctly NOT captured.
      (ep/bind-and-bump! env 'tp {"op" "cat" "x" 1})
      (let
        [real
         (ep/run-python-block ctx "print(tp)")

         ;; proxy result → captured
         faked
         (ep/run-python-block ctx "print({'op':'cat'})")

         ;; model dict → NOT captured (robust)
         plain
         (ep/run-python-block ctx "print('just text')")

         two
         (ep/run-python-block ctx "print(tp); print(tp)")]

        (expect (= 1 (count (:printed-results real))))
        (expect (= "cat" (get (first (:printed-results real)) "op"))) ;; origin = result "op" (strings-only)
        (expect (empty? (:printed-results faked)))        ;; robustness: model 'op' dict ignored
        (expect (empty? (:printed-results plain)))
        (expect (= 2 (count (:printed-results two))))
        (expect (re-find #"'op'" (str (:stdout real)))))) ;; stdout (context) still shows it
    (it "a missing key on a tool result names the tool, the near miss and EVERY key it did return"
        ;; Result shapes are per-tool by design (shell -> stdout/stderr/exit,
        ;; run_tests -> output). A bare `KeyError: 'output'` reads as "the tool broke",
        ;; so the model guesses a second name and spins. Every host-rebuilt map -- the
        ;; result AND each nested map -- answers a miss with its own shape instead;
        ;; `.get` stays silent, and the value is still a plain dict.
        (ep/bind-and-bump! env 'tp {"op" "shell" "stdout" "hi" "exit" 0 "nested" {"a" 1}})
        (let
          [out (:stdout
                 (ep/run-python-block
                   ctx
                   (str "r = tp\n"
                        "try:\n    r['output']\nexcept KeyError as e:\n    print('MISS', e)\n"
                        "try:\n    r['stdou']\nexcept KeyError as e:\n    print('NEAR', e)\n"
                        "try:\n    r['nested']['b']\nexcept KeyError as e:\n    print('NEST', e)\n"
                        "print('GET', r.get('output'), isinstance(r, dict))")))]
          (expect (str/includes? out "'output' is not a key of 'shell' result"))
          (expect (str/includes? out "Keys: 'op', 'stdout', 'exit', 'nested'"))
          (expect (str/includes? out "Did you mean 'stdout'?"))
          (expect (str/includes? out "Keys: 'a'"))       ;; nested map knows its own shape
          (expect (str/includes? out "GET None True")))) ;; .get silent, still a real dict
    (it
      "EVERY settled tool value is dict-probeable: a list/str result answers .get without a type guard"
      ;; A capability return can be a LIST (struct_patch/write rows) or a
      ;; bare STRING, not only a dict. The TOP-LEVEL settle normalizes each so a
      ;; uniform `res.get('op')` probe never trips — while the value keeps its
      ;; native list/str behaviour (index/iterate/len/concat).
      (let
        [r (ep/run-python-block
             ctx
             (str "async def __call(fn, name):\n"
                  "    return await __vis_deferred__(fn, name)()\n"
                  "lst = await __call(lambda: [{'path': 'a', 'op': 'update'}], 'patch')\n"
                  "s = await __call(lambda: 'plain text', 'cat')\n"
                  "d = await __call(lambda: {'op': 'rg', 'hit_count': 2}, 'grep')\n"
                  "ops = [res.get('op') for res in (lst, s, d)]\n"
                  "print(['lst_get', lst.get('op'), 'lst0', lst[0]['op'], 'lst_len', len(lst)])\n"
                  "print(['str_get', s.get('op'), 'str_cat', s + '!'])\n"
                  "print(['dct_get', d.get('op')])\n"
                  "print(['sweep_ok', all(o is None or isinstance(o, str) for o in ops), "
                  "'rg_in', 'rg' in ops])"))]
        (expect (re-find #"'lst_get', None" (str (:stdout r))))
        (expect (re-find #"'lst0', 'update'" (str (:stdout r))))
        (expect (re-find #"'lst_len', 1" (str (:stdout r))))
        (expect (re-find #"'str_cat', 'plain text!'" (str (:stdout r))))
        (expect (re-find #"'dct_get', 'rg'" (str (:stdout r))))
        (expect (re-find #"'sweep_ok', True" (str (:stdout r))))
        (expect (re-find #"'rg_in', True" (str (:stdout r))))))
    (it
      "mixed print (text + result) keeps :only-printed-results? FALSE so stdout text is never dropped"
      (ep/bind-and-bump! env 'tp {"op" "cat" "x" 1})
      (let
        [pure
         (ep/run-python-block ctx "print(tp)")

         mixed
         (ep/run-python-block ctx "print('FOUND:'); print(tp)")]

        (expect (true? (:only-printed-results? pure)))  ;; pure result print → cards may replace
        (expect (not (:only-printed-results? mixed)))   ;; mixed → show full stdout
        (expect (= 1 (count (:printed-results mixed)))) ;; the result is still captured
        (expect (re-find #"FOUND:" (str (:stdout mixed))))))
    (it "a printed write/struct_patch result drops its echo-diff from stdout"
        ;; A write/struct_patch return is a LIST of `{path op changed diff}`
        ;; file summaries. Printed to stdout that diff merely re-describes the bytes
        ;; the model supplied, so it is stripped for DISPLAY exactly like the
        ;; model-wire `strip-echo-diffs`.
        (ep/bind-and-bump!
          env
          'edit
          [{"path" "a.clj" "op" "update" "changed" true "diff" "--- before\n+++ after\n-x\n+y"}])
        (let [result (ep/run-python-block ctx "print(edit)")]
          (expect (not (str/includes? (str (:stdout result)) "diff")))
          (expect (str/includes? (str (:stdout result)) "a.clj"))
          (expect (str/includes? (str (:stdout result)) "'changed': True"))))
    (it
      "a LIST-shaped tool result is dict-probeable AND captured (write/struct_patch rows)"
      ;; `patch`/`write`/`struct_patch` return a LIST of per-file rows. At the
      ;; TOP-LEVEL settle of a tool call that list must be re-typed to
      ;; `__VisResultList__`: otherwise the
      ;; documented uniform `res.get('op')` probe dies with `'list' object has no
      ;; attribute 'get'`, the print-capture cannot recognise the result, and the
      ;; block stops counting as results-only — dropping every OTHER printed card.
      (let
        [typed
         (ep/run-python-block
           ctx
           (str
             "def __rows():\n"
             "    return [{'path': 'a.clj', 'op': 'update', 'changed': True}]\n"
             "patchy = __vis_deferred__(__rows, 'patch')\n"
             "res = await patchy()\n"
             "print(['type', type(res).__name__, 'get', res.get('op'), 'row', res[0]['op'], 'len', len(res)])\n"))

         printed
         (ep/run-python-block ctx "print(res)")]

        (expect (re-find #"'type', '__VisResultList__'" (str (:stdout typed))))
        (expect (re-find #"'get', None" (str (:stdout typed)))) ;; probe answers, never throws
        (expect (re-find #"'row', 'update'" (str (:stdout typed)))) ;; rows keep their own op
        (expect (re-find #"'len', 1" (str (:stdout typed))))
        (expect (= 1 (count (:printed-results printed))))
        (expect (true? (:only-printed-results? printed)))))
    (it
      "session is a REAL dict after bind-ctx! — json.dumps(session) works (was a ForeignDict)"
      (ep/bind-ctx! ctx {"workspace" "/x" "roots" ["a" "b"] "facts" {"k" "v"}})
      (let
        [r
         (ep/run-python-block
           ctx
           "import json\nprint([isinstance(session, dict), json.dumps(session) is not None, session['workspace']])")]
        (expect (re-find #"\[True, True, '/x'\]" (str (:stdout r))))))))   ;; the text survives (the bug)

(defdescribe
  ntr-is-gone-test
  "The native-result store is RETIRED. `python_execution` prints instead of
   returning a stored `:result`, so there is nothing left to index: every name the
   old accessor exposed must be absent from the sandbox, and no docstring or
   description may promise a coordinate to re-read a result by."
  (let
    [ctx
     (:python-context (ep/create-python-context (ext/builtin-sandbox-bindings (fn []
                                                                                nil))))

     run
     (fn [code]
       (ep/run-python-block ctx code))]

    (it "every retired accessor name raises NameError"
        (doseq
          [expr ["ntr" "native_tools_results" "ntr.describe" "ntr.at" "__vis_native_result_prime__"
                 "__vis_native_result_fetch__" "__vis_native_result_ids__"
                 "__vis_native_result_index__" "__vis_native_result_scope__" "__vis_entries_at__"
                 "__vis_native_result_scan__"]]
          (let [r (run (str "print(" expr ")"))]
            (expect (some? (:error r)))
            (expect (str/includes? (str (:message (:error r))) "NameError")))))
    (it "no sandbox doc still advertises a stored-result coordinate"
        (let [out (str (:stdout (run "print(apropos(''))\nprint(doc('session_fold'))")))]
          (expect (not (str/includes? out "ntr[")))
          (expect (not (str/includes? out "ntr.describe")))
          (expect (not (str/includes? out "# saved:")))))))

(defdescribe
  doc-apropos-surface-test
  "The in-sandbox self-discovery surface must stay complete and clean: every
   bound sandbox verb answers `doc(name)` with its own contract, and `apropos('')`
   lists the real verbs while excluding Python builtins and the async-runtime
   `asyncio` shim global. Guards the docstring→doc wiring
   (`extension/sandbox-symbol-docs`) and the `apropos` non-tool filter."
  (let
    [bind
     (ext/builtin-sandbox-bindings (fn []
                                     nil))

     ctx
     (:python-context (ep/create-python-context bind))

     run
     (fn [code]
       (str (:stdout (ep/run-python-block ctx code))))

     ;; Every kernel verb wired into this sandbox — the exact set whose docs
     ;; must be seeded. Keyed by their Python name.
     native
     (for
       [e
        (ext/registered-extensions)

        s
        (ext/ext-symbols e)

        :when (contains? bind (:ext.symbol/symbol s))]

       (ep/sym->py-name (:ext.symbol/symbol s)))]

    (it "every wired sandbox verb exposes a non-empty doc"
        (expect (seq native)) ;; sanity: we actually tested some
        (let
          [out (run (str "import json\nbad=[]\n"
                         "for n in ["
                         (str/join ", " (map pr-str native))
                         "]:\n"
                         "    d = doc(n)\n" "    if ('is not a handle' in d) or (not d.strip()):\n"
                         "        bad.append(n)\n" "print('BAD='+json.dumps(bad))"))]
          (expect (re-find #"BAD=\[\]" out))))
    (it "apropos('') lists real tools but not builtins or the asyncio shim"
        (let
          [out (run (str "a=apropos('')\n" "print('asyncio='+str('asyncio' in a),"
                         "'len='+str('len' in a)," "'ls='+str('ls' in a),"
                         "'grep='+str('grep' in a)," "'struct_patch='+str('struct_patch' in a))"))]
          (expect (re-find #"asyncio=False" out))
          (expect (re-find #"len=False" out))
          (expect (re-find #"ls=True" out))
          ;; `rg`/`find_files` were replaced by `grep` (name + content search in one tool)
          (expect (re-find #"grep=True" out))
          (expect (re-find #"struct_patch=True" out))))
    ;; `apropos` is FULL TEXT now: a query that appears in no NAME at all still
    ;; answers, and rank is the "where did it match" answer — an exact name first,
    ;; a body-only hit last.
    (it "apropos searches the whole document, not just the name"
        (let
          [out (run (str "print('skeleton='+str('struct_index' in apropos('skeleton')))\n"
                         "print('names='+','.join(list(apropos('struct_patch'))[:1]))"))]
          (expect (str/includes? out "skeleton=True"))
          (expect (str/includes? out "names=struct_patch"))))
    (it "apropos ANDs its terms"
        (let
          [out (run (str "wide = len(apropos('file'))\n" "narrow = len(apropos('file skeleton'))\n"
                         "print('wide='+str(wide), 'narrow='+str(narrow),"
                         " 'shrinks='+str(narrow < wide))"))]
          (expect (str/includes? out "shrinks=True"))))
    (it "apropos and doc describe their own callable contracts"
        (let [out (run (str "print(doc('apropos'))\n" "print(doc('doc'))"))]
          (expect (str/includes? out "apropos(query='')"))
          (expect (str/includes? out "FULL-TEXT SEARCH over every document"))
          (expect (str/includes? out "doc(target) -> str"))
          (expect (str/includes? out "A skill is one of these documents and nothing more"))))
    (it "gather exposes its concurrency contract through apropos and doc"
        (let [out (run (str "print(apropos('gather')['gather'])\n" "print(doc('gather'))"))]
          (expect (str/includes? out "gather(*awaitables) -> list"))
          (expect (str/includes? out "independent deferred tool calls"))
          (expect (str/includes? out "results preserve input order"))
          (expect (str/includes? out "keep dependent calls sequential"))
          (expect (str/includes? out "every failing slot index"))))
    ;; The fold receipt promises nothing it cannot keep: with the native-result
    ;; store gone there is no coordinate to hand back, so the doc says the gist is
    ;; what survives and `session_state()` is the only door to the rest.
    (it "session_fold documents what a fold keeps and what it drops"
        (ep/set-python-binding! ctx 'session-fold identity)
        (let
          [out (run (str "print(apropos('session_fold')['session_fold'])\n"
                         "print(doc('session_fold'))"))]
          (expect (str/includes? out "session_fold(target, gist=None) -> str"))
          (expect (str/includes? out "Folding changes rendering, not storage"))
          (expect (str/includes? out "there is no destructive unfold command"))
          (expect (str/includes? out "its GIST is what survives"))
          (expect (str/includes? out "s = await session_state()"))
          (expect (str/includes? out "['iterations'][...]['blocks']"))
          (expect (str/includes? out "broader newer fold supersedes fully covered"))
          (expect (str/includes? out "Partial overlaps remain separate"))
          ;; No coordinate, no store: the words that used to promise recovery are gone.
          (expect (not (str/includes? out "`ntr`")))
          (expect (not (str/includes? out "# saved:")))))))

(defdescribe
  native-container-preservation-test
  "Auto-settle's `__vis_pyify__` must rebuild ONLY foreign host proxies
   (ProxyHashMap/ForeignDict/…) into real python dicts/lists — a value the
   model itself built (set / frozenset / tuple / defaultdict) is already native
   and MUST pass through untouched. Blindly rebuilding by an allowlist silently
   downgraded set/tuple/frozenset → list (and dict-subclasses → dict), so a
   plain `s = set(); s.add(1)` raised \"'list' object has no attribute 'add'\"."
  (let
    [ctx
     (tpc/shared)

     run
     (fn [code]
       (str (:stdout (ep/run-python-block ctx code))))]

    (it "a module-level set/tuple/frozenset/defaultdict keeps its native type"
        (let
          [out (run (str "s = set()\n" "s.add(1); s.add(1); s.add(2)\n"
                         "t = (1, 2, 3)\n" "fs = frozenset([1, 1, 2])\n"
                         "from collections import defaultdict\n"
                         "dd = defaultdict(list); dd['x'].append(9)\n"
                         "print('set='+type(s).__name__, 'add='+str(hasattr(s,'add')))\n"
                         "print('tuple='+type(t).__name__)\n"
                         "print('frozenset='+type(fs).__name__)\n"
                         "print('defaultdict='+type(dd).__name__)"))]
          (expect (re-find #"set=set add=True" out))
          (expect (re-find #"tuple=tuple" out))
          (expect (re-find #"frozenset=frozenset" out))
          (expect (re-find #"defaultdict=defaultdict" out))))
    (it "a native set persists as a set (and stays mutable) ACROSS blocks"
        (run "acc = set()\nacc.add('a')")
        (let
          [out (run (str "acc.add('b'); acc.add('a')\n"
                         "print('kind='+type(acc).__name__, 'vals='+str(sorted(acc)))"))]
          (expect (re-find #"kind=set vals=\['a', 'b'\]" out))))))

(defdescribe
  boundary-date-test
  ;; Regression (session 9c829d10): `java.util.Date` — what nippy hands back
  ;; for every persisted `#inst` (session/turn `:created-at`s) — fell through
  ;; `->py`'s `:else` branch as a raw host object. GraalPy materialising it as
  ;; a Python datetime needs the context's datetime module data, which is null
  ;; unless `import datetime` already ran in the sandbox:
  ;; `NullPointerException: Cannot read field "utc" because "moduleData" is
  ;; null`. Dates now cross as ISO-8601 strings, same as Temporals/UUIDs.
  (it "java.util.Date crosses as an ISO-8601 instant string"
      (let [d (java.util.Date. 1782986254012)]
        (expect (= (str (.toInstant d)) (get (ep/boundary-view {"created_at" d}) "created_at")))))
  (it "dates nested in the sessions() index shape survive"
      (let [v (ep/boundary-view {"sessions" [{"id" "x" "created_at" (java.util.Date. 0)}]})]
        (expect (= "1970-01-01T00:00:00Z" (get (first (get v "sessions")) "created_at"))))))

(defdescribe
  boundary-key-shape-test
  "STRINGS-ONLY boundary: every dict key is a VERBATIM string in BOTH
   directions — no keywordizing, no regex key-shape sniffing. A path, a
   line number, an option key, a git status code: all plain strings.
   A keyword or symbol ANYWHERE (key or value, any depth) is a producer bug
   and throws. Pure `boundary-view`, no context needed."
  (it "every key stays a verbatim string — paths, line numbers, option keys alike"
      (let
        [raw
         {"matches" {"extensions/channels/vis-channel-tui/src/a.clj" {"2361" "x"}
                     "src/com/foo-bar.clj" {"44" "y"}}
          "hit_count" 2
          "files" ["a-b/c.clj"]}

         v
         (ep/boundary-view raw)]

        (expect (= raw v))
        (expect (every? string? (keys v)))
        (expect (every? string? (mapcat keys (vals (get v "matches")))))))
  (it "git status codes M/A/D stay verbatim strings"
      (expect (= {"changes" {"M" 1 "A" 2 "D" 3}}
                 (ep/boundary-view {"changes" {"M" 1 "A" 2 "D" 3}}))))
  (it "boundary-view is idempotent"
      (let [v (ep/boundary-view {"matches" {"a/b-c.clj" {"1:h" "z"}} "hit_count" 1})]
        (expect (= v (ep/boundary-view v)))))
  (it "a keyword MAP KEY throws with the offending path"
      (let
        [e (try (ep/boundary-view {"outer" {:hit-count 1}})
                nil
                (catch clojure.lang.ExceptionInfo e e))]
        (expect (some? e))
        (expect (= :non-string-key (:vis/boundary-violation (ex-data e))))
        (expect (= ["outer"] (:path (ex-data e))))))
  (it "a TOP-LEVEL keyword key still names WHERE, with an empty path"
      (let [e (try (ep/boundary-view {:hit-count 1}) nil (catch clojure.lang.ExceptionInfo e e))]
        (expect (some? e))
        (expect (= :non-string-key (:vis/boundary-violation (ex-data e))))
        (expect (= [] (:path (ex-data e))))
        (expect (str/includes? (ex-message e) "TOP-LEVEL map key"))))
  (it "a top-level :result key points the producer at the envelope mistake"
      (let
        [e (try (ep/boundary-view {:result {"ok" true} :success? true})
                nil
                (catch clojure.lang.ExceptionInfo e e))]
        (expect (some? e))
        (expect (= :non-string-key (:vis/boundary-violation (ex-data e))))
        (expect (str/includes? (ex-message e) "INTERNAL result envelope"))
        (expect (str/includes? (ex-message e) "never the envelope"))))
  (it "a keyword VALUE throws at any depth"
      (let
        [e (try (ep/boundary-view {"changes" [{"status" :added}]})
                nil
                (catch clojure.lang.ExceptionInfo e e))]
        (expect (some? e))
        (expect (= :keyword-value (:vis/boundary-violation (ex-data e))))
        (expect (= :added (:value (ex-data e))))))
  (it "a symbol VALUE throws"
      (let
        [e (try (ep/boundary-view {"sym" 'git-fetch!}) nil (catch clojure.lang.ExceptionInfo e e))]
        (expect (some? e))
        (expect (= :symbol-value (:vis/boundary-violation (ex-data e)))))))

(defdescribe
  py-gc-option-guard-test
  "Layer 1 GC-option guard: a misconfigured env var must NEVER produce a value that can
   break Context construction. `clamp-gc-value` parses + clamps into the option's range."
  (let [clamp #'ep/clamp-gc-value]
    (it "an in-range value passes through unchanged"
        (expect (= "1000" (clamp "1000" 1 Integer/MAX_VALUE)))
        (expect (= "30" (clamp "30" 1 100))))
    (it "the real bug: a byte-scale threshold clamps into [1,100] instead of throwing"
        ;; the old docstring wrongly said Threshold was "bytes"; 1048576 would throw
        ;; "must be an integer in range [1, 100]" at Context build. Now it clamps to 100.
        (expect (= "100" (clamp "1048576" 1 100))))
    (it "a below-floor value clamps up to the low bound"
        (expect (= "1" (clamp "0" 1 100)))
        (expect (= "1" (clamp "-5" 1 100))))
    (it "whitespace is trimmed before parsing" (expect (= "50" (clamp "  50  " 1 100))))
    (it "a blank / nil / non-numeric input contributes nothing (nil)"
        (expect (nil? (clamp nil 1 100)))
        (expect (nil? (clamp "" 1 100)))
        (expect (nil? (clamp "   " 1 100)))
        (expect (nil? (clamp "abc" 1 100)))
        (expect (nil? (clamp "12.5" 1 100))))
    (it "resolves the 2 GB BackgroundGCTaskMinimum floor by default (box shouldn't balloon)"
        ;; No VIS_PY_GC_* env vars set in the test process, yet the minimum floor
        ;; is baked in so the background collector engages from 2 GB up.
        (let [opts (#'ep/resolve-py-gc-options)]
          (expect (= "2048" (get opts "python.BackgroundGCTaskMinimum")))
          ;; interval/threshold stay on GraalPy's own defaults (nil default)
          (expect (nil? (get opts "python.BackgroundGCTaskInterval")))
          (expect (nil? (get opts "python.BackgroundGCTaskThreshold")))))))

(defdescribe
  deferred-call-inline-settle-test
  "A deferred `__vis_Call__` (a tool call you forgot to `await`) auto-settles
   when USED INLINE via subscript / `len` / `in` — killing the classic
   `git(x)['stdout']` \"'__vis_Call__' object is not subscriptable\" papercut —
   WITHOUT weakening the loud unawaited repr, and without ever settling on the
   names internal plumbing probes with `hasattr`. The call is built INSIDE a
   function body so the top-level assignment auto-settle doesn't resolve it
   first."
  (let
    [ctx
     (tpc/shared)

     run
     (fn [code]
       (str (:stdout (ep/run-python-block ctx code))))]

    (it "subscript / len / in on an un-awaited call settle it in place"
        (let
          [out
           (run (str "def _t():\n"
                     "    c = __vis_deferred__(lambda: {'stdout': 'hi', 'exit': 0}, 'faketool')()\n"
                     "    kind = type(c).__name__\n"
                     "    return [kind, c['stdout'], len(c), 'exit' in c, 'zzz' in c]\n"
                     "print(_t())"))]
          (expect (re-find #"\['__vis_Call__', 'hi', 2, True, False\]" out))))
    (it "attribute use settles too, but the engine's own hasattr probes never run it"
        ;; Issue #97: after a host-phase failure an unresolved `__vis_Call__` escaped
        ;; into user space and every attribute touch died with a bare AttributeError
        ;; naming an object the caller never made. Attribute access is the same
        ;; single-expression use as subscript, so it settles — EXCEPT on the names
        ;; internal plumbing probes with `hasattr` (`send`/`throw`/`close`/`keys`),
        ;; which must stay absent so a probe can never silently run the tool.
        (let
          [out
           (run
             (str
               "def _t():\n" "    ran = []\n"
               "    c = __vis_deferred__(lambda: ran.append(1) or {'stdout': 'hi'}, 'faketool')()\n"
               "    probes = [hasattr(c, n) for n in ('send', 'throw', 'close', 'keys')]\n"
               "    return [probes, len(ran), c.get('stdout'), len(ran)]\n" "print(_t())"))]
          (expect (re-find #"\[\[False, False, False, False\], 0, 'hi', 1\]" out))))
    (it "an un-awaited call still repr's a LOUD hint and never silently ran"
        (let
          [out (run (str
                      "def _t():\n" "    ran = []\n"
                      "    c = __vis_deferred__(lambda: ran.append(1) or {'k': 1}, 'faketool')()\n"
                      "    r = repr(c)\n"
                      "    return [r, ran]\n" "print(_t())"))]
          (expect (re-find #"unawaited async tool call" out))
          (expect (re-find #"faketool" out))
          ;; repr must NOT have executed the tool — `ran` stays empty
          (expect (re-find #", \[\]\]" out))))
    (it "no __getattr__ auto-run: a non-slot attribute raises, it does not settle"
        (let
          [out (run (str "def _t():\n"
                         "    c = __vis_deferred__(lambda: {'stdout': 'hi'}, 'faketool')()\n"
                         "    try:\n" "        c.stdout\n"
                         "        return 'leaked'\n" "    except AttributeError:\n"
                         "        return 'safe'\n" "print(_t())"))]
          (expect (re-find #"safe" out))))))

(defdescribe collect-garbage-gil-budget-test
             ;; Regression: `collect-garbage!` runs in `loop/send!`'s `finally`, i.e. between
             ;; the engine unwinding and `gateway.state/run-turn!` appending the terminal
             ;; event. Its `.eval` first takes the Python GIL, which a SIBLING session holds
             ;; for the whole of its in-flight Python (a `shell` subprocess can hold it for
             ;; hours) and which no cancel token can unpark. Waiting for it without a budget
             ;; wedged an already-finished turn forever: no `turn.completed`/`turn.cancelled`
             ;; reached the wire, the session stayed pinned to a turn nobody was running, the
             ;; queued backlog never drained and Esc could not close the live panel.
             (it
               "returns within its budget while another thread holds the GIL"
               (let
                 [env
                  {:python-context (tpc/shared)}

                  entered
                  (promise)

                  hog
                  (doto (Thread. ^Runnable
                                 (fn []
                                   (try (deliver entered true)
                                        (ep/run-python-block (:python-context env)
                                                             "import time\ntime.sleep(30)")
                                        (catch Throwable _ nil)))
                                 "gil-hog")
                    (.setDaemon true)
                    (.start))

                  _
                  (deref entered 10000 nil)

                  ;; give the hog time to actually be inside the sleep, holding the GIL
                  _
                  (Thread/sleep 1000)

                  started
                  (System/currentTimeMillis)

                  _
                  (ep/collect-garbage! env)

                  elapsed
                  (- (System/currentTimeMillis) started)]

                 (.interrupt hog)
                 (expect (< elapsed 15000)
                         (str "collect-garbage! blocked on the GIL for " elapsed "ms")))))

(defdescribe
  sandbox-open-flush-test
  ;; GraalPy does NOT refcount, so the CPython idiom
  ;; `open(p, "w").write(text)` - a handle dropped without close() - was
  ;; never finalized at the end of the statement: the bytes stayed in the
  ;; buffer and the file on disk was EMPTY until an arbitrary later GC.
  ;; A block wrote a commit message and `git commit -F` read nothing.
  ;; The sandbox `open` tracks writable handles weakly and the runner
  ;; flushes them before every tool call and at the end of the block.
  (it
    "puts a block's unclosed write on disk, before a tool call and at block end"
    (let
      [dir
       (.toFile (java.nio.file.Files/createTempDirectory
                  "vis-open-flush"
                  (make-array java.nio.file.attribute.FileAttribute 0)))

       end-file
       (java.io.File. dir "at-end.txt")

       mid-file
       (java.io.File. dir "before-tool.txt")

       raw-file
       (java.io.File. dir "unwrapped.txt")

       ctx
       (:python-context (ep/create-python-context {'read_back
                                                   (fn [& args]
                                                     (let [f (java.io.File. (str (first args)))]
                                                       (if (.exists f) (slurp f) "")))}
                                                  (constantly [dir])))

       at-end
       (ep/run-python-block ctx
                            (str "open(" (pr-str (.getAbsolutePath end-file))
                                 ", 'w').write('flushed-bytes')\n" "print('block-done')"))

       before-tool
       (ep/run-python-block ctx
                            (str "open("
                                 (pr-str (.getAbsolutePath mid-file))
                                 ", 'w').write('mid-bytes')\n"
                                 "print(read_back("
                                 (pr-str (.getAbsolutePath mid-file))
                                 "))"))

       unwrapped
       (ep/run-python-block ctx
                            (str "__vis_real_open__(" (pr-str (.getAbsolutePath raw-file))
                                 ", 'w').write('lost')\n" "print('raw-done')"))]

      (try (expect (nil? (:error at-end)))
           (expect (= "block-done\n" (:stdout at-end)))
           (expect (= "flushed-bytes" (slurp end-file)))
           ;; A tool that reads a just-written file sees the bytes.
           (expect (nil? (:error before-tool)))
           (expect (= "mid-bytes\n" (:stdout before-tool)))
           ;; Counterfactual: the untracked handle is the old behaviour -
           ;; the write is still sitting in an unflushed buffer.
           (expect (= "raw-done\n" (:stdout unwrapped)))
           (expect (zero? (.length raw-file)))
           (finally (run! #(.delete ^java.io.File %) [end-file mid-file raw-file dir]))))))


;; A BARE sandbox verb (`_shell_logs`) is called from Python with no schema in
;; front of it — so `doc(name)` is the only place its result keys are stated.
(defdescribe
  bare-verb-docs-test
  "`doc` states the raw-result contract for the private handle transports, which
   no listing advertises."
  (let
    [bind
     (ext/builtin-sandbox-bindings (fn []
                                     nil))

     ctx
     (:python-context (ep/create-python-context bind))

     run
     (fn [code]
       (str (:stdout (ep/run-python-block ctx code))))]

    (it "the private handle transports stay out of every listing"
        (let
          [out (run (str "print('hidden='+str('_shell_logs' in apropos('')))\n"
                         "print('shell='+str('shell' in apropos('')))"))]
          (expect (str/includes? out "hidden=False"))
          (expect (str/includes? out "shell=True"))))
    (it "the handle verbs carry their raw-result contract in doc"
        (let
          [out (run (str "print('LOGS<'+doc('_shell_logs')+'>')\n"
                         "print('STOP<'+doc('_shell_stop')+'>')"))]
          ;; ONE shell result shape: `logs` fills `stdout` like a foreground run does,
          ;; and `stop` answers the same keys — never a stage-scoped subset.
          (expect (str/includes? out "The same shell result shape as every other stage"))
          (expect (str/includes? out "`stdout` is the window this read returned"))
          (expect (str/includes? out "(`stage` \"stop\"): `stopped`, `status`, `exit`."))))))

;; ONE corpus, two verbs. `apropos` SEARCHES every document the session can
;; reach — function contracts, Vis' own documentation pages, whole `SKILL.md`
;; bodies, MCP tool descriptions — and `doc` RETRIEVES one of them whole. There
;; is no third verb: `vis_docs` is gone, and a page is reachable by the same
;; `doc(name)` a function answers.
(defdescribe
  discovery-is-two-verbs-test
  "The sandbox's whole discovery surface: `apropos(query)` ranks full text,
   `doc(target)` returns one document, and bare `doc()` prints the curated index
   rather than dumping the corpus."
  (let
    [bind
     (ext/builtin-sandbox-bindings (fn []
                                     nil))

     ctx
     (:python-context (ep/create-python-context bind))

     run
     (fn [code]
       (str (:stdout (ep/run-python-block ctx code))))]

    (it "the third verb is gone and nothing carries a group any more"
        (let
          [out (run (str "print('vis_docs='+str('vis_docs' in globals()))\n"
                         "print('groups='+str('__vis_groups__' in globals()))\n"
                         "print('table='+str('__vis_apropos_table__' in globals()))"))]
          (expect (str/includes? out "vis_docs=False"))
          (expect (str/includes? out "groups=False"))
          (expect (str/includes? out "table=False"))))
    (it "a word that lives only in a page BODY finds that page"
        ;; "Truffle" is in no tool name and in no gist — only inside the pages.
        (let
          [out (run (str "hits = apropos('truffle')\n"
                         "print('n='+str(len(hits)))\n"
                         "print('graalpython='+str('graalpython' in hits))"))]
          (expect (str/includes? out "graalpython=True"))))
    (it "doc retrieves a documentation page by slug, forgiving case and `.md`"
        (let
          [out (run (str "a = doc('gateway')\n"
                         "b = doc('Gateway.MD')\n" "print('same='+str(a == b))\n"
                         "print('head='+a.splitlines()[0])\n"
                         "print('body='+str('pairing' in a.lower()))"))]
          (expect (str/includes? out "same=True"))
          (expect (str/includes? out "head=# gateway"))
          (expect (str/includes? out "body=True"))))
    ;; A skill is a document like any other: `doc` prints the whole SKILL.md and
    ;; there is no verb to call, so the entry carries no call line at all.
    (it "doc returns a skill WHOLE, with no verb to invoke and no session effect"
        (let
          [out (run (str "d = doc('spel')\n" "print('call='+str('skill(' in d))\n"
                         "print('long='+str(len(d) > 2000))\n"
                         "print('bound='+str('skill' in globals()))"))]
          (expect (str/includes? out "call=False"))
          (expect (str/includes? out "long=True"))
          (expect (str/includes? out "bound=False"))))
    (it "bare doc() is the curated index, not the corpus"
        (let
          [out (run (str "idx = doc()\n"
                         "print('rows='+str(len(idx.splitlines())))\n"
                         "print('curated='+str(idx.count(' — ') < len(apropos(''))))\n"
                         "print('grep='+str('grep — ' in idx))\n"
                         "print('points='+str('apropos(text)' in idx))"))]
          (expect (str/includes? out "curated=True"))
          (expect (str/includes? out "grep=True"))
          (expect (str/includes? out "points=True"))))
    (it "a miss answers with the closest documents instead of a dead end"
        (let [out (run "print(doc('gatewa'))")]
          (expect (str/includes? out "is not a handle"))
          (expect (str/includes? out "gateway"))
          (expect (str/includes? out "doc(name)"))))))

;; Regression: a documentation slug must never shadow a bound function — the
;; corpus is seeded with `setdefault`, so the callable contract wins its name.
(defdescribe a-page-never-shadows-a-function-test
             (it "keeps the function's own contract when a document claims its name"
                 (try (doc-corpus/register-source! ::collision
                                                   (fn []
                                                     [{:name "ls" :text "PAGE THAT MUST LOSE"}]))
                      (let
                        [ctx
                         (:python-context (ep/create-python-context (ext/builtin-sandbox-bindings
                                                                      (fn []
                                                                        nil))))

                         out
                         (str (:stdout (ep/run-python-block ctx "print(doc('ls'))")))]

                        (expect (not (str/includes? out "PAGE THAT MUST LOSE")))
                        (expect (str/includes? out "ls")))
                      (finally (doc-corpus/register-source! ::collision (constantly []))))))

(defdescribe
  block-source-introspection-test
  ;; A block's source was registered nowhere, so `inspect.getsource` on a
  ;; function the model had just defined died with "could not get source code":
  ;; the sandbox could RUN code it could not SHOW.
  (it "reads back the source of a function defined in an EARLIER block"
      (let
        [ctx
         (tpc/shared)

         _
         (ep/run-python-block ctx "def source_probe(a, b=2):\n    return a + b\n")

         _
         (ep/run-python-block ctx "source_probe_unrelated = 1")

         result
         (ep/run-python-block ctx "import inspect\nprint(inspect.getsource(source_probe))")]

        (expect (str/includes? (:stdout result) "def source_probe(a, b=2):"))
        (expect (str/includes? (:stdout result) "return a + b"))))
  (it "gives every block its own co_filename, so an older source is never overwritten"
      (let
        [ctx
         (tpc/shared)

         _
         (ep/run-python-block ctx "def source_probe_first():\n    return 1\n")

         result
         (ep/run-python-block
           ctx
           (str "import inspect\n"
                "def source_probe_second():\n    return 2\n"
                "print(source_probe_first.__code__.co_filename"
                " != source_probe_second.__code__.co_filename)\n"
                "print(inspect.getsource(source_probe_first).strip().endswith('return 1'))\n"))]

        (expect (= "True\nTrue\n" (:stdout result))))))

(defdescribe tool-introspection-test
             ;; Every bound tool was a bare `(*a, **k)` trampoline with an empty docstring,
             ;; so `help(tool)` and `inspect.signature(tool)` showed nothing of the
             ;; contract the host declares for it.
             (it "carries the host doc and the declared parameters onto the bound callable"
                 (let
                   [ctx
                    (tpc/shared-with! {'meta-probe (fn [& args]
                                                     (str "called:" (count args)))})

                    _
                    (ep/set-python-binding-doc!
                      ctx
                      'meta-probe
                      "meta_probe(language=None, **kwargs) -> str. The declared contract.")

                    _
                    (ep/set-python-binding-signature! ctx 'meta-probe "language=None, **kwargs")

                    result
                    (ep/run-python-block ctx
                                         (str "import inspect\n"
                                              "print(str(inspect.signature(meta_probe)))\n"
                                              "print(meta_probe.__doc__.split('.')[0])\n"
                                              "print(meta_probe.__name__)\n"))]

                   (expect (= (str "(language=None, **kwargs)\n"
                                   "meta_probe(language=None, **kwargs) -> str\n"
                                   "meta_probe\n")
                              (:stdout result)))))
             (it "keeps accepting the call shapes the reported signature does not name"
                 (let
                   [ctx
                    (tpc/shared-with! {'meta-probe (fn [& args]
                                                     (str "called:" (count args)))})

                    _
                    (ep/set-python-binding-signature! ctx 'meta-probe "language=None, **kwargs")

                    result
                    (ep/run-python-block ctx
                                         (str "kw = meta_probe(language=\"python\", extra=1)\n"
                                              "mapped = meta_probe({\"language\": \"python\"})\n"
                                              "print(kw, mapped)\n"))]

                   (expect (= "called:1 called:1\n" (:stdout result))))))

(defn- spinning-binding!
  "Install `vis_test_spin()` on `ctx`: a HOST callable that burns wall clock in a
   loop containing no blocking call at all — the shape of `sh.wait`'s branch that
   re-reads a chatty log — polling the guest safepoint only when `poll?`."
  [^Context ctx poll?]
  (ep/set-python-binding!
    ctx
    'vis_test_spin
    (reify
      ProxyExecutable
        (execute [_ _args]
          (let [end (+ (System/currentTimeMillis) 4000)]
            (loop []

              (when (< (System/currentTimeMillis) end) (when poll? (rt/guest-safepoint!)) (recur))))
          nil))))

(defn- park-ending
  "Park a guest thread in a HOST wait for a promise nothing ever delivers — the
   shape of a dialog nobody answers, or any wait whose end is not this thread's to
   decide — then soft-cancel the context the documented way.

   Answers `[landed? outcome ending]`: whether `Context.interrupt` landed inside
   its grace, what the eval did, and HOW the park itself ended."
  [k]
  (let
    [ctx
     (tpc/context k)

     never
     (promise)

     ending
     (atom :still-parked)

     _
     (ep/set-python-binding! ctx
                             'vis_test_park
                             (reify
                               ProxyExecutable
                                 (execute [_ _args]
                                   (reset! ending
                                     ;; The deadline is 30s only so a REGRESSION fails instead of
                                     ;; hanging CI: the cancel arrives at ~2s, so nothing but the
                                     ;; cancel can be what ends this park.
                                     (try (deref never 30000 :expired)
                                          (catch InterruptedException _ :thread-interrupt)
                                          (catch Throwable _ :polyglot-unwind)))
                                   nil)))

     done
     (promise)]

    (doto (Thread. ^Runnable
                   (fn []
                     (deliver done
                              (try (.eval ctx "python" "vis_test_park()")
                                   :returned
                                   (catch Throwable _ :unwound))))
                   "host-park-cancel-test")
      (.setDaemon true)
      (.start))
    (Thread/sleep 400)
    (let
      [landed (try (.interrupt ctx (java.time.Duration/ofMillis 1500))
                   true
                   (catch java.util.concurrent.TimeoutException _ false))]
      [landed (deref done 5000 :still-parked) @ending])))

(defn- interrupt-lands?
  "Park a guest thread inside a HOST wait `install!` binds, then soft-cancel it the
   documented way. True when `Context.interrupt` landed inside its grace, false on
   the `TimeoutException` the javadoc raises when it could not."
  [k install! code]
  (let
    [ctx
     (tpc/context k)

     _
     (install! ctx)

     done
     (promise)]

    (doto (Thread.
            ^Runnable
            (fn []
              (deliver done (try (.eval ctx "python" code) :returned (catch Throwable _ :unwound))))
            "guest-safepoint-test")
      (.setDaemon true)
      (.start))
    (Thread/sleep 400)
    (try (.interrupt ctx (java.time.Duration/ofMillis 1500))
         true
         (catch java.util.concurrent.TimeoutException _ false))))

(defdescribe
  guest-safepoint-test
  ;; Regression (session 7df808ff): a turn cancelled while its block sat in a host
  ;; wait unwound only the WAITER. Host code that polls no safepoint is exactly what
  ;; the polyglot javadoc calls "non-interruptible host code": the interrupt timed
  ;; out on it, and the guest thread was left inside GraalPy to be abandoned, where
  ;; it dies OWNING the GIL (`PythonContext.ensureGilAfterFailure` takes it
  ;; uninterruptibly, and a ReentrantLock whose owner is dead is never released).
  ;; Every later turn of that session then parked in `PythonContext.acquireGil`
  ;; forever, at `:engine-start`.
  (it "unwinds a non-blocking host loop that polls, and keeps the context USABLE"
      (expect (true? (interrupt-lands? ::polling #(spinning-binding! % true) "vis_test_spin()")))
      ;; Non-destructive by contract: no rebuilt interpreter, no lost globals.
      (expect (= 2 (ep/->clj (.eval (tpc/context ::polling) "python" "1 + 1")))))
  (it "cannot reach that same loop when it polls nothing"
      (expect (false?
                (interrupt-lands? ::not-polling #(spinning-binding! % false) "vis_test_spin()")))))

(defdescribe host-park-cancel-test
             "How a wait waits decides whether a cancel reaches it. Its LENGTH never does."
             ;; Regression (session 7df808ff): a turn cancelled while its block sat in host
             ;; code left the guest thread inside GraalPy, where an abandoned thread dies
             ;; owning the GIL and every later turn of that session parks in
             ;; `PythonContext.acquireGil` forever. This pins the other half of
             ;; `guest-safepoint-test`: a park that BLOCKS needs no poll of its own — the
             ;; polyglot interrupt reaches it through the JDK wait it is blocked in, so a
             ;; park with no deadline anywhere near it is still cancelled in milliseconds.
             (it "reaches a park nothing else could ever end, and keeps the context USABLE"
                 (expect (= [true :unwound :thread-interrupt] (park-ending ::indefinite-park)))
                 ;; Non-destructive by contract: no rebuilt interpreter, no lost globals.
                 (expect (= 2
                            (ep/->clj (.eval (tpc/context ::indefinite-park) "python" "1 + 1"))))))
