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
                         "'rg='+str('rg' in a)," "'struct_patch='+str('struct_patch' in a))"))]
          (expect (re-find #"asyncio=False" out))
          (expect (re-find #"len=False" out))
          (expect (re-find #"cat=True" out))
          (expect (re-find #"rg=True" out))
          (expect (re-find #"struct_patch=True" out))))
    (it "apropos and doc describe their own callable contracts"
        (let [out (run (str "print(doc('apropos'))\n" "print(doc('doc'))"))]
          (expect (str/includes? out "apropos(query='')"))
          (expect (str/includes? out "doc(name)"))
          (expect (str/includes? out "result shape"))))))

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
   WITHOUT weakening the loud unawaited repr or adding spooky `__getattr__`
   auto-run. The call is built INSIDE a function body so the top-level
   assignment auto-settle doesn't resolve it first."
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
