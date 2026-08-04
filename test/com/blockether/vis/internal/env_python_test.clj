(ns com.blockether.vis.internal.env-python-test
  "GraalPy sandbox behaviour that needs a REAL context: the proxy→dict boundary
   fix and the print-capture of tool results. Boots ONE context for the ns."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.env-python :as ep]
            [com.blockether.vis.internal.extension :as ext]
            [lazytest.core :refer [defdescribe expect it]]))

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
                    (:python-context (ep/create-python-context {}))

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
                    (:python-context (ep/create-python-context {}))

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
         (:python-context (ep/create-python-context {}))

         err
         (:error (ep/run-python-block ctx "raise ValueError('probe-real')"))]

        (expect (str/includes? (:message err) "ValueError: probe-real"))
        (expect (= :python/runtime (:phase (:data err))))))
  (it "keeps the real exception when the position walk itself fails"
      (let
        [ctx
         (:python-context (ep/create-python-context {}))

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
             ;; PLUS the intentional compatibility aliases. `fs` also answers to `fs_tool`
             ;; so it reads as a verb next to `shell`/`grep`; a missing alias is a bare
             ;; NameError to the model, which reads as "the tool is gone" and invites a spin.
             (it "exposes fs as both fs and fs_tool, grep as grep/find_files/find"
                 (expect (= ["fs" "fs_tool"] (ep/python-binding-names 'fs)))
                 (expect (= ["grep" "find_files" "find"] (ep/python-binding-names 'grep))))
             (it "routes every alias to the SAME tool in a live context"
                 (let
                   [ctx
                    (:python-context (ep/create-python-context {'fs (fn fs-stub [& args]
                                                                      {"op" "fs"
                                                                       "args" (vec args)})}))

                    result
                    (ep/run-python-block ctx
                                         (str "print(fs('exists')['op'], fs('exists')['args'])\n"
                                              "print(fs_tool('x')['op'], fs_tool('x')['args'])"))]

                   (expect (= "fs ['exists']\nfs ['x']\n" (:stdout result))))))

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
      "ntr store makes EVERY value dict-probeable: a list/str result answers .get without a type guard"
      ;; A stored native result can be a LIST (patch/struct_patch/write rows) or a
      ;; bare STRING, not only a dict. __vis_store__ normalizes each so a uniform
      ;; `for _id, res in ntr.items(): res.get('op')` sweep never trips — while the
      ;; value keeps its native list/str behaviour (index/iterate/len/concat).
      (let
        [r
         (ep/run-python-block
           ctx
           (str
             "lst = ntr.__vis_store__('toolu_LST', [{'path': 'a', 'op': 'update'}])\n"
             "s = ntr.__vis_store__('toolu_STR', 'plain text')\n"
             "d = ntr.__vis_store__('toolu_DCT', {'op': 'rg', 'hit_count': 2})\n"
             "ops = [res.get('op') or res.get('tool') for _id, res in ntr.items()]\n"
             "print(['lst_get', lst.get('op'), 'lst0', lst[0]['op'], 'lst_len', len(lst)])\n"
             "print(['str_get', s.get('op'), 'str_cat', s + '!'])\n"
             "print(['dct_get', d.get('op')])\n"
             "print(['sweep_ok', all(o is None or isinstance(o, str) for o in ops), 'rg_in', 'rg' in ops])"))]
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
    (it "a printed patch/write/struct_patch result drops its echo-diff from stdout"
        ;; A patch/write/struct_patch return is a LIST of `{path op changed diff}`
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
      "a LIST-shaped tool result is dict-probeable AND captured (patch/write/struct_patch rows)"
      ;; `patch`/`write`/`struct_patch` return a LIST of per-file rows. At the
      ;; TOP-LEVEL settle of a tool call that list must be re-typed to
      ;; `__VisResultList__`, exactly like a stored `ntr[...]` read: otherwise the
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
  doc-apropos-surface-test
  "The in-sandbox self-discovery surface must stay complete and clean: every
   bound NATIVE tool's `doc(name)` carries its description AND a `params:` block
   (input schema), and `apropos('')` lists the real tools while excluding Python
   builtins and the async-runtime `asyncio` shim global. Guards the schema→doc
   wiring (`extension/sandbox-symbol-docs`) and the `apropos` non-tool filter."
  (let
    [bind
     (ext/builtin-sandbox-bindings (fn []
                                     nil))

     ctx
     (:python-context (ep/create-python-context bind))

     run
     (fn [code]
       (str (:stdout (ep/run-python-block ctx code))))

     ;; The kernel native tools that are ALSO wired into this sandbox — the
     ;; exact set whose docs must be seeded. Keyed by their Python name.
     native
     (for
       [e
        (ext/registered-extensions)

        s
        (ext/ext-symbols e)

        :when (and (ext/symbol-bound? s)
                   (:ext.symbol/native-tool? s)
                   (contains? bind (:ext.symbol/symbol s)))]

       (ep/sym->py-name (:ext.symbol/symbol s)))]

    (it "every wired native tool exposes a non-empty doc WITH a params: block"
        (expect (seq native)) ;; sanity: we actually tested some
        (let
          [out (run (str "import json\nbad=[]\n"
                         "for n in ["
                         (str/join ", " (map pr-str native))
                         "]:\n"
                         "    d = doc(n)\n"
                         "    if ('<not found>' in d) or ('params:' not in d) or (not d.strip()):\n"
                         "        bad.append(n)\n" "print('BAD='+json.dumps(bad))"))]
          (expect (re-find #"BAD=\[\]" out))))
    (it "apropos('') lists real tools but not builtins or the asyncio shim"
        (let
          [out (run (str "a=apropos('')\n" "print('asyncio='+str('asyncio' in a),"
                         "'len='+str('len' in a)," "'cat='+str('cat' in a),"
                         "'grep='+str('grep' in a)," "'struct_patch='+str('struct_patch' in a))"))]
          (expect (re-find #"asyncio=False" out))
          (expect (re-find #"len=False" out))
          (expect (re-find #"cat=True" out))
          ;; `rg`/`find_files` were replaced by `grep` (name + content search in one tool)
          (expect (re-find #"grep=True" out))
          (expect (re-find #"struct_patch=True" out))))
    (it "native apropos hides advertised canonical names and compatibility aliases only"
        ;; In-Python apropos remains a complete composable index. Only the native
        ;; markdown renderer suppresses capabilities whose schema is already visible.
        (ep/set-advertised-native-tools! ctx ["cat" "grep" "find_files" "find"])
        (let
          [out (run (str "print('raw='+','.join(sorted(apropos('find').keys())))\n"
                         "print('native-find='+__vis_apropos_table__('find'))\n"
                         "print('native-struct='+__vis_apropos_table__('struct_patch'))"))]
          (expect (str/includes? out "raw=find,find_files"))
          (expect
            (str/includes? out "native-find=apropos('find'): no unadvertised capabilities match."))
          (expect (str/includes? out "native-struct=| capability | gist |"))
          (expect (str/includes? out "| `struct_patch` |"))))
    (it "apropos and doc describe their own callable contracts"
        (let [out (run (str "print(doc('apropos'))\n" "print(doc('doc'))"))]
          (expect (str/includes? out "apropos(query='')"))
          (expect (str/includes? out "doc(name)"))
          (expect (str/includes? out "result shape"))))
    (it "gather exposes its concurrency contract through apropos and doc"
        (let [out (run (str "print(apropos('gather')['gather'])\n" "print(doc('gather'))"))]
          (expect (str/includes? out "gather(*awaitables) -> list"))
          (expect (str/includes? out "independent deferred tool calls"))
          (expect (str/includes? out "results preserve input order"))
          (expect (str/includes? out "keep dependent calls sequential"))
          (expect (str/includes? out "every failing slot index"))))
    (it "session_fold documents raw recovery and fold-of-fold semantics"
        (ep/set-python-binding! ctx 'session-fold identity)
        (let
          [out (run (str "print(apropos('session_fold')['session_fold'])\n"
                         "print(doc('session_fold'))"))]
          (expect (str/includes? out "session_fold(target, gist=None) -> str"))
          (expect (str/includes? out "there is no destructive unfold command"))
          (expect (str/includes? out "`ntr` never stores a `session_fold` receipt"))
          (expect (str/includes? out "s = await session_state()"))
          (expect (str/includes? out "['iterations'][...]['blocks']"))
          (expect (str/includes? out "broader newer fold supersedes fully covered"))
          (expect (str/includes? out "Partial overlaps remain separate"))))))

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
     (:python-context (ep/create-python-context {}))

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
   `lineno:hash` anchor, an option key, a git status code: all plain strings.
   A keyword or symbol ANYWHERE (key or value, any depth) is a producer bug
   and throws. Pure `boundary-view`, no context needed."
  (it "every key stays a verbatim string — paths, anchors, option keys alike"
      (let
        [raw
         {"matches" {"extensions/channels/vis-channel-tui/src/a.clj" {"2361:abc" "x"}
                     "src/com/foo-bar.clj" {"44:f14" "y"}}
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
     (:python-context (ep/create-python-context {}))

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

(defdescribe
  ntr-browse-test
  "`ntr` must be browseable by MEANING, not by opaque id. `keys()` hands back
   24-character tool_use ids that say nothing about what they hold, and
   `items()`/`values()` thaw the ENTIRE store to find out. `describe()` sits
   between them: a bounded, newest-first window primed in ONE batched query,
   each id labelled with its `op` plus a couple of that result's own salient
   fields — so a stored result can be CHOSEN before it is fetched in full."
  (let
    [ctx
     (:python-context (ep/create-python-context {}))

     run
     (fn [code]
       (str (:stdout (ep/run-python-block ctx code))))]

    (it "describe() labels each id with its op and salient fields"
        ;; No host callbacks in a bare context: seed the in-process cache and
        ;; describe THOSE ids, which is the same path a primed read takes.
        (let
          [out (run
                 (str
                   "_c = getattr(ntr, '__vis_cache__')\n" "_c['toolu_A'] = "
                   "{'op': 'grep', 'query': 'session-fold-card', 'hit_count': 2, 'file_count': 1}\n"
                   "_c['toolu_B'] = {'op': 'cat', 'path': 'src/loop.clj'}\n"
                   "for line in ntr.describe(ids=['toolu_A', 'toolu_B', 'toolu_ZZ']):\n"
                   "    print(line)\n"))]
          (expect (str/includes? out "toolu_A · grep · query=session-fold-card · hit_count=2"))
          (expect (str/includes? out "toolu_B · cat · path=src/loop.clj"))
          ;; An unknown id is reported, never raised — browsing must not blow up.
          (expect (str/includes? out "toolu_ZZ · <missing>"))))
    (it "describe() labels from the host INDEX without fetching any payload"
        ;; The index is the whole point of the fast path: labelling a window of
        ;; ids must cost ZERO result thaws. A throwing prime proves it — it may
        ;; only ever run for an id the index cannot label.
        (let
          [own
           (:python-context (ep/create-python-context {}))

           primed
           (atom [])]

          (ep/set-python-binding!
            own
            (symbol "__vis_native_result_index__")
            (fn []
              [{"id" "toolu_A" "tool" "grep" "gist" "closed-map?, 26 hits in 1 file"}
               {"id" "toolu_B" "tool" "cat"}]))
          (ep/set-python-binding! own
                                  (symbol "__vis_native_result_prime__")
                                  (fn [ids]
                                    (swap! primed conj (vec ids))
                                    nil))
          (let
            [out (str (:stdout
                        (ep/run-python-block
                          own
                          (str "for line in ntr.describe(ids=['toolu_A', 'toolu_B', 'toolu_ZZ']):\n"
                               "    print(line)\n"))))]
            (expect (str/includes? out "toolu_A · grep · closed-map?, 26 hits in 1 file"))
            (expect (str/includes? out "toolu_B · cat"))
            ;; only the id the index does not know is ever primed
            (expect (= [["toolu_ZZ"]] @primed)))))
    (it "repr says how many results are stored and how to browse them"
        (let
          [out (run (str "_c = getattr(ntr, '__vis_cache__')\n"
                         "_c['toolu_A'] = {'op': 'grep'}\n"
                         "print(repr(ntr))\n"))]
          (expect (str/includes? out "stored native results"))
          (expect (str/includes? out "ntr.at("))
          (expect (str/includes? out "ntr.describe()"))))))

;; Regression: every tool result in the transcript is HEADED with its coordinate
;; (`# t5/i1`) and only footed with the `toolu_…` id, so the model reached for the
;; coordinate — and `ntr["t5/i1"]` always raised `no native tool result for
;; 't5/i1' — that tool_use id is unknown`, an error that named the wrong problem
;; and left no way to recover the result without re-running the tool.
(defdescribe
  ntr-coordinate-test
  "A transcript COORDINATE (`tN/iM`) is a first-class `ntr` key: it resolves to
   the native result that iteration stored, without the model having to copy a
   24-character `toolu_…` id. An iteration that ran SEVERAL native tools never
   guesses — it names them and asks for one."
  (let
    [context-with
     (fn [scope-fn]
       (let [own (:python-context (ep/create-python-context {}))]
         (ep/set-python-binding! own (symbol "__vis_native_result_scope__") scope-fn)
         own))

     run
     (fn [ctx code]
       (str (:stdout (ep/run-python-block ctx code))))]

    (it "a coordinate resolves to that iteration's one stored result"
        (let
          [fetched
           (atom [])

           own
           (context-with (fn [scope]
                           (when (= "t5/i1" scope)
                             [{"id" "toolu_A" "tool" "grep" "gist" "26 hits"}])))]

          (ep/set-python-binding! own
                                  (symbol "__vis_native_result_fetch__")
                                  (fn [id]
                                    (swap! fetched conj id)
                                    (when (= "toolu_A" id) {"op" "grep" "hit_count" 26})))
          (let [out (run own (str "r = ntr['t5/i1']\n" "print(r['op'], r['hit_count'])\n"))]
            (expect (str/includes? out "grep 26"))
            ;; the coordinate is resolved to the id, and only the ID is fetched
            (expect (= ["toolu_A"] @fetched)))))
    (it "a form scope and the verbose alias resolve the same iteration"
        (let
          [own (context-with (fn [scope]
                               ;; the host widens a `/fK` tail nobody stored back to
                               ;; the whole iteration, so a stray tail never dead-ends
                               (when (contains? #{"t5/i1" "t5/i1/f2"} scope)
                                 [{"id" "toolu_A" "tool" "cat"}])))]
          (ep/set-python-binding! own
                                  (symbol "__vis_native_result_fetch__")
                                  (fn [id]
                                    (when (= "toolu_A" id) {"op" "cat"})))
          (expect (str/includes? (run own "print(ntr['t5/i1/f2']['op'])\n") "cat"))
          (expect (str/includes? (run own "print(native_tools_results['T5/I1']['op'])\n") "cat"))))
    (it "an iteration with several native calls names them instead of guessing"
        (let
          [own
           (context-with (fn [_]
                           [{"id" "toolu_A" "tool" "grep"} {"id" "toolu_B" "tool" "cat"}]))

           out
           (run own
                (str "try:\n" "    ntr['t5/i1']\n"
                     "except KeyError as e:\n" "    print(str(e))\n"))]

          (expect (str/includes? out "t5/i1"))
          (expect (str/includes? out "toolu_A"))
          (expect (str/includes? out "grep"))
          (expect (str/includes? out "toolu_B"))
          (expect (str/includes? out "cat"))))
    ;; Regression: naming those calls by `toolu_…` id was the whole problem — the
    ;; ambiguous coordinate has to hand back keys that can be READ, and a way to
    ;; take every result the iteration stored.
    (it "several native calls in one iteration are addressed by their form key"
        (let
          [entries
           [{"id" "toolu_A" "tool" "grep" "gist" "26 hits" "form" "t5/i1/f1"}
            {"id" "toolu_B" "tool" "cat" "form" "t5/i1/f2"}]

           own
           (context-with (fn [scope]
                           ;; the host narrows a form key to ONE entry
                           (cond (= "t5/i1" scope) entries
                                 (= "t5/i1/f2" scope) [(second entries)]
                                 :else [])))]

          (ep/set-python-binding! own
                                  (symbol "__vis_native_result_fetch__")
                                  (fn [id]
                                    (get {"toolu_A" {"op" "grep"} "toolu_B" {"op" "cat"}} id)))
          ;; the ambiguous coordinate names READABLE keys, not just ids
          (let
            [out (run own
                      (str "try:\n" "    ntr['t5/i1']\n"
                           "except KeyError as e:\n" "    print(str(e))\n"))]
            (expect (str/includes? out "t5/i1/f1"))
            (expect (str/includes? out "t5/i1/f2"))
            (expect (str/includes? out "ntr.at(")))
          (expect (str/includes? (run own "print(ntr['t5/i1/f2']['op'])\n") "cat"))
          ;; …and every result of that iteration at once, in the order it ran
          (expect (str/includes? (run own "print([r['op'] for r in ntr.at('t5/i1')])\n")
                                 "['grep', 'cat']"))))
    (it "a coordinate that stored nothing says so, and points at describe()"
        (let
          [own
           (context-with (fn [_]
                           []))

           out
           (run own
                (str "try:\n" "    ntr['t9/i9']\n"
                     "except KeyError as e:\n" "    print(str(e))\n"))]

          (expect (str/includes? out "t9/i9"))
          (expect (str/includes? out "ntr.describe()"))
          ;; the id-shaped miss keeps naming the coordinate as an option
          (expect (str/includes? (run own
                                      (str "try:\n" "    ntr['toolu_NOPE']\n"
                                           "except KeyError as e:\n" "    print(str(e))\n"))
                                 "tN/iM"))))
    ;; Regression: the `# saved:` line under every result now stamps the COORDINATE, so
    ;; describe() leads with the same key instead of a 24-character id to copy.
    (it "describe() leads each entry with the coordinate the transcript stamped"
        (let [own (:python-context (ep/create-python-context {}))]
          (ep/set-python-binding!
            own
            (symbol "__vis_native_result_index__")
            (fn []
              [{"id" "toolu_A" "scope" "t7/i3" "form" "t7/i3/f2" "tool" "grep" "gist" "26 hits"}
               {"id" "toolu_OLD" "tool" "cat"}]))
          (let
            [out (run own
                      (str "for line in ntr.describe(ids=['toolu_A', 'toolu_OLD']):\n"
                           "    print(line)\n"))]
            (expect (str/includes? out "t7/i3/f2 · grep · 26 hits"))
            ;; the opaque id is never stamped in front of a key that reads back
            (expect (not (str/includes? out "toolu_A ·")))
            ;; …but a record too old to carry a coordinate still names its id
            (expect (str/includes? out "toolu_OLD · cat")))))))

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
                  {:python-context (:python-context (ep/create-python-context {}))}

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
