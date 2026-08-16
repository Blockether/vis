(ns com.blockether.vis.internal.env-python-test
  "GraalPy sandbox behaviour that needs a REAL context: the proxy→dict boundary
   fix and the print-capture of tool results. Boots ONE context for the ns."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.doc-corpus :as doc-corpus]
            [com.blockether.vis.internal.env-python :as ep]
            [com.blockether.vis.internal.extension :as ext]
            [com.blockether.vis.internal.paths :as paths]
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
        (let [out (str (:stdout (run "print(apropos(''))\nprint(doc('fold_session'))")))]
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
    ;; The CALL LINE is the one thing prose cannot supply: which parameters the
    ;; verb takes, which are required, in what order. `doc(name)` renders it from
    ;; the DECLARED signature, so a page can never drift from the contract — and
    ;; no verb may answer with the async trampoline's own `(*a, **k)` instead.
    (it "every wired sandbox verb's page opens with its own call line"
        (let
          [out (run (str "import json\nbad=[]\n"
                         "for n in [" (str/join ", " (map pr-str native))
                         "]:\n" "    if (n + '(') not in doc(n).splitlines()[2]:\n"
                         "        bad.append(n)\n" "print('NOCALL='+json.dumps(bad))"))]
          (expect (re-find #"NOCALL=\[\]" out))))
    ;; Regression, doc quality: a tool page carried prose alone — nothing named the
    ;; parameters, and an options-dict verb stated its REQUIRED keys nowhere
    ;; `doc(name)` could reach, so the contract was learned from refusals. Both
    ;; lines are rendered from the DECLARATION above the document; putting them
    ;; INSIDE the text instead cost `patch` fourteen ranks on its own ask, because
    ;; a document's first line is one of the three scored fields.
    (it "opens every tool page with its call line and names its required keys once"
        (let
          [out (run (str "import json\n"
                         "rows = apropos()\n" "bad = []\n"
                         "keyed = []\n" "for n, v in rows.items():\n"
                         "    if v['kind'] != 'tool': continue\n" "    L = doc(n).splitlines()\n"
                         "    if len(L) < 3 or (n + '(') not in L[2]: bad.append(n)\n"
                         "    if len(L) > 3 and L[3].startswith('Keys:'): keyed.append(n)\n"
                         "print('NOCALL='+json.dumps(bad))\n"
                         "print('KEYED='+str(len(keyed) > 8))\n"
                         "print('REQUIRED='+str('Keys: paths (REQUIRED)' in doc('struct_index')))\n"
                         "print('ONCE='+str(doc('patch').count('patch(path, edits)')))"))]
          (expect (re-find #"NOCALL=\[\]" out))
          (expect (str/includes? out "KEYED=True"))
          (expect (str/includes? out "REQUIRED=True"))
          (expect (str/includes? out "ONCE=1"))))
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
    ;; answers, and every row carries WHERE it matched — a bounded excerpt of the
    ;; document and the line the matched region starts on.
    (it "apropos searches the whole document, not just the name"
        (let
          [out (run (str "print('skeleton='+str('struct_index' in apropos('skeleton')))\n"
                         "print('names='+','.join(list(apropos('struct_patch'))[:1]))"))]
          (expect (str/includes? out "skeleton=True"))
          (expect (str/includes? out "names=struct_patch"))))
    ;; Regression: `apropos` ANDed its terms, so a six-word ask that several
    ;; documents partly covered answered `{}` — the query shape a model
    ;; naturally types dead-ended, and one-letter loop variables became
    ;; documents whose NAME scored an exact hit.
    (it "ranks a description instead of filtering on every term"
        (let
          [out (run (str "hits = apropos('patch from_anchor to_anchor replace edits schema')\n"
                         "print('any='+str(len(hits) > 0))\n"
                         "print('patch='+str('patch' in hits))\n"
                         "print('typo='+str('struct_patch' in apropos('strcut_patch')))\n"
                         "print('none='+str(len(apropos('zzqqxk plorbfnat'))))"))]
          (expect (str/includes? out "any=True"))
          (expect (str/includes? out "patch=True"))
          (expect (str/includes? out "typo=True"))
          (expect (str/includes? out "none=0"))))
    ;; Regression: ORing the terms made a described ask match half the corpus,
    ;; so a six-word question answered ~80 rows of mostly noise. A described
    ;; ask is capped and ranked; the EMPTY query is a listing and stays whole.
    (it "caps a described ask but never the empty listing"
        (let
          [out (run (str "hits = apropos('how do I replace lines in a file')\n"
                         "print('capped='+str(len(hits) <= 10))\n"
                         "print('first='+list(hits)[0])\n"
                         "print('all='+str(len(apropos('')) > 25))"))]
          (expect (str/includes? out "capped=True"))
          (expect (str/includes? out "first=patch"))
          (expect (str/includes? out "all=True"))))
    ;; Regression: a row was a bare gist string, so a 70 KB skill answered with
    ;; its title and nothing about the ask, and an undocumented `def` of the
    ;; model's own ranked beside real contracts with nothing to tell them apart.
    (it "answers each hit with its kind, a bounded excerpt and where it matched"
        (let
          [out (run (str
                      "def killers():\n" "    return 1\n"
                      "hits = apropos('how do I replace lines in a file')\n" "row = hits['patch']\n"
                      "print('keys='+','.join(sorted(row)))\n" "print('kind='+row['kind'])\n"
                      "print('bounded='+str(max(len(h['gist']) for h in hits.values()) <= 300))\n"
                      "print('excerpt='+str('…' in row['gist']))\n"
                      "print('hit='+str(len(row['hit']) > 0))\n"
                      "print('at='+str(isinstance(row['at'], int)))\n"
                      "print('searched='+str('killers' in hits))\n"
                      "print('listed='+apropos('')['killers']['kind'])"))]
          (expect (str/includes? out "keys=at,gist,hit,kind"))
          (expect (str/includes? out "kind=tool"))
          (expect (str/includes? out "bounded=True"))
          (expect (str/includes? out "excerpt=True"))
          (expect (str/includes? out "hit=True"))
          (expect (str/includes? out "at=True"))
          (expect (str/includes? out "searched=False"))
          (expect (str/includes? out "listed=local"))))
    ;; Regression: the bare call already answered the whole listing, but in the
    ;; SEARCH row's shape — `at: 0` and `hit: ''` repeated on every reachable
    ;; name, two keys that say nothing without a query to be relative to.
    (it "answers the bare call with the whole listing, in the listing's own shape"
        (let
          [out (run (str "bare = apropos()\n" "empty = apropos('')\n"
                         "print('same='+str(list(bare) == list(empty)))\n"
                         "print('keys='+','.join(sorted(bare['grep'])))\n"
                         "print('ordered='+str(list(bare) == sorted(bare)))\n"
                         "print('gist='+str(len(bare['grep']['gist']) > 0))"))]
          (expect (str/includes? out "same=True"))
          (expect (str/includes? out "keys=gist,kind"))
          (expect (str/includes? out "ordered=True"))
          (expect (str/includes? out "gist=True"))))
    (it "never turns a bound loop variable into a document"
        (let
          [out (run (str "x = 3\nday_set = {'a'}\n" "a = apropos('')\n"
                         "print('x='+str('x' in a), 'day_set='+str('day_set' in a),"
                         " 'grep='+str('grep' in a))"))]
          (expect (str/includes? out "x=False"))
          (expect (str/includes? out "day_set=False"))
          (expect (str/includes? out "grep=True"))))
    (it "apropos and doc describe their own callable contracts"
        (let [out (run (str "print(doc('apropos'))\n" "print(doc('doc'))"))]
          (expect (str/includes? out "apropos(query='')"))
          (expect (str/includes? out "FULL-TEXT SEARCH over every document"))
          (expect (str/includes? out "doc(target) -> str"))
          (expect (str/includes? out "A skill is one of these documents and nothing more"))))
    (it "gather exposes its concurrency contract through apropos and doc"
        (let [out (run (str "print(apropos('gather')['gather']['gist'])\n" "print(doc('gather'))"))]
          (expect (str/includes? out "gather(*awaitables) -> list"))
          (expect (str/includes? out "independent deferred tool calls"))
          (expect (str/includes? out "results preserve input order"))
          (expect (str/includes? out "keep dependent calls sequential"))
          (expect (str/includes? out "every failing slot index"))))
    ;; The fold receipt promises nothing it cannot keep: with the native-result
    ;; store gone there is no coordinate to hand back, so the doc says the gist is
    ;; what survives and `read_session()` is the only door to the rest.
    (it "fold_session documents what a fold keeps and what it drops"
        (ep/set-python-binding! ctx 'fold-session identity)
        (let
          [out (run (str "print(apropos('fold_session')['fold_session']['gist'])\n"
                         "print(doc('fold_session'))"))]
          (expect (str/includes? out "fold_session(key, gist=None) -> str"))
          (expect (str/includes? out "The key is a STRING"))
          (expect (str/includes? out "Folding changes rendering, not storage"))
          (expect (str/includes? out "there is no destructive unfold command"))
          (expect (str/includes? out "its GIST is what survives"))
          (expect (str/includes? out "s = await read_session()"))
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
  (it "dates nested in the list_sessions() index shape survive"
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
   names internal plumbing probes with `hasattr`. The thunk is held INSIDE A LIST:
   a STATEMENT now settles its own tool call at every depth, while a call in
   EXPRESSION position — a list element, an argument, a `gather` batch — defers."
  (let
    [ctx
     (tpc/shared)

     run
     (fn [code]
       (str (:stdout (ep/run-python-block ctx code))))]

    (it
      "subscript / len / in on an un-awaited call settle it in place"
      (let
        [out
         (run
           (str
             "def _t():\n"
             "    box = [__vis_deferred__(lambda: {'stdout': 'hi', 'exit': 0}, 'faketool')()]\n"
             "    kind = type(box[0]).__name__\n"
             "    return [kind, box[0]['stdout'], len(box[0]), 'exit' in box[0], 'zzz' in box[0]]\n"
             "print(_t())"))]
        (expect (re-find #"\['__vis_Call__', 'hi', 2, True, False\]" out))))
    (it
      "attribute use settles too, but the engine's own hasattr probes never run it"
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
             "    box = [__vis_deferred__(lambda: ran.append(1) or {'stdout': 'hi'}, 'faketool')()]\n"
             "    probes = [hasattr(box[0], n) for n in ('send', 'throw', 'close', 'keys')]\n"
             "    return [probes, len(ran), box[0].get('stdout'), len(ran)]\n" "print(_t())"))]
        (expect (re-find #"\[\[False, False, False, False\], 0, 'hi', 1\]" out))))
    (it "an un-awaited call still repr's a LOUD hint and never silently ran"
        (let
          [out (run
                 (str
                   "def _t():\n" "    ran = []\n"
                   "    box = [__vis_deferred__(lambda: ran.append(1) or {'k': 1}, 'faketool')()]\n"
                   "    r = repr(box[0])\n"
                   "    return [r, ran]\n" "print(_t())"))]
          (expect (re-find #"unawaited async tool call" out))
          (expect (re-find #"faketool" out))
          ;; repr must NOT have executed the tool — `ran` stays empty
          (expect (re-find #", \[\]\]" out))))
    (it "no __getattr__ auto-run: a non-slot attribute raises, it does not settle"
        (let
          [out (run (str "def _t():\n"
                         "    box = [__vis_deferred__(lambda: {'stdout': 'hi'}, 'faketool')()]\n"
                         "    try:\n" "        box[0].stdout\n"
                         "        return 'leaked'\n" "    except AttributeError:\n"
                         "        return 'safe'\n" "print(_t())"))]
          (expect (re-find #"safe" out))))))

(defdescribe
  nested-statement-settle-test
  "A deferred tool call runs WHERE IT IS WRITTEN: `__vis_run_async__` settle-wraps
   every assign/expr STATEMENT at EVERY depth, not only `tree.body`. A call in
   EXPRESSION position still defers — that is the seam `gather` batches through —
   and so does a statement inside an `async def`, where holding an awaitable is
   the idiom — except a `return`, whose value has LEFT the scope that could await
   it and settles at every helper kind. Every settle path raises the same
   catchable `__vis_ToolError__`."
  (let
    [calls
     (atom [])

     ctx
     (tpc/shared-with! {'nested_ok (fn [& args]
                                     (swap! calls conj (vec args))
                                     {"op" "nested_ok"})
                        'nested_boom (fn [& _]
                                       (throw (ex-info "nested_boom refused - nothing was written."
                                                       {})))})

     run
     (fn [code]
       (reset! calls [])
       (str (:stdout (ep/run-python-block ctx code))))]

    ;; Regression: only TOP-LEVEL statements were settle-wrapped, so a bare tool call
    ;; inside a loop built one `__vis_Call__` thunk per iteration and ran NONE of them:
    ;; `for p in paths: patch(p, edits)` reported nothing and edited nothing.
    (it "a bare call statement in a loop RUNS, once per iteration"
        (let [out (run (str "for ns_i in (1, 2, 3):\n" "    nested_ok(ns_i)\n" "print('ns done')"))]
          (expect (re-find #"ns done" out))
          (expect (= 3 (count @calls)))))
    ;; Regression: a nested assignment bound the THUNK itself, so `r` held a
    ;; `__vis_Call__` and every later use of it read as a broken tool.
    (it "a nested assignment in a loop / `try:` / a `def` body binds the RESULT"
        (let
          [out (run (str "ns_seen = []\n" "def ns_helper(i):\n"
                         "    r = nested_ok(i)\n" "    ns_seen.append(isinstance(r, dict))\n"
                         "for ns_i in (1, 2):\n" "    ns_x = nested_ok(ns_i)\n"
                         "    ns_seen.append(isinstance(ns_x, dict))\n" "try:\n"
                         "    ns_y = nested_ok(3)\n" "    ns_seen.append(isinstance(ns_y, dict))\n"
                         "except Exception:\n" "    ns_seen.append('refused')\n"
                         "ns_helper(4)\n" "print(ns_seen)"))]
          (expect (re-find #"\[True, True, True, True\]" out))
          (expect (= 4 (count @calls)))))
    ;; Regression: a host refusal reached the guest as a foreign ExceptionInfo — a
    ;; BaseException that is NOT an Exception — so `except Exception:` could not catch
    ;; it, and the refusal escaped the very handler written for it and killed the block.
    (it "a refusal inside `try:` is caught by `except Exception:`, message intact"
        (let
          [out (run (str "try:\n" "    ns_r = nested_boom(1)\n"
                         "    ns_out = 'LEAKED ' + type(ns_r).__name__\n" "except Exception as e:\n"
                         "    ns_out = 'caught ' + type(e).__name__ + ' :: ' + str(e)\n"
                         "print(ns_out)"))]
          (expect (re-find #"caught __vis_ToolError__ :: nested_boom refused" out))))
    (it "a call in EXPRESSION position still defers, so `gather` keeps its batch"
        (let
          [out (run (str "ns_box = [nested_ok(9)]\n" "ns_kind = type(ns_box[0]).__name__\n"
                         "ns_val = await ns_box[0]\n" "print([ns_kind, ns_val['op']])"))]
          (expect (re-find #"\['__vis_Call__', 'nested_ok'\]" out))))
    ;; Regression: a `return` was not settle-wrapped either, so `def edit(p, a, n):
    ;; return patch(p, edits)` handed the CALLER a thunk, and `"x" + edit(...)` died
    ;; with a TypeError naming `__vis_Call__` instead of making the edit.
    (it "a `def` that RETURNS a tool call hands back the result, not the thunk"
        (let
          [out (run (str "def ns_read(i):\n"
                         "    return nested_ok(i)\n"
                         "print([isinstance(ns_read(5), dict), len(ns_read(6))])"))]
          (expect (re-find #"\[True, 1\]" out))))
    ;; An `async def` body is the ONE place a statement keeps its thunk: holding an
    ;; awaitable (`t = asyncio.to_thread(f, x)` then `await gather(t, u)`) is the
    ;; whole idiom there, and `await` is right beside it.
    (it "inside an `async def` a statement still defers, so a coroutine can hold it"
        (let
          [out (run (str "ns_seen2 = []\n" "async def ns_coro():\n"
                         "    t = nested_ok(7)\n" "    ns_seen2.append(type(t).__name__)\n"
                         "    v = await t\n" "    ns_seen2.append(v['op'])\n"
                         "await ns_coro()\n" "print(ns_seen2)"))]
          (expect (re-find #"\['__vis_Call__', 'nested_ok'\]" out))))
    ;; Regression: `return` inside an `async def` was the ONE statement never
    ;; settle-wrapped, so `async def m(): g = grep(...); return sess, g` handed the
    ;; caller a tuple whose second slot was a raw `__vis_Call__`. It survived into the
    ;; NEXT block, where `json.dumps(g)` refused an object the model never created.
    (it "an `async def` RETURNS its results, never a thunk it never awaited"
        (let
          [out (run (str "async def ns_pair():\n"
                         "    g = nested_ok(11)\n" "    return 'k', g\n"
                         "ns_k, ns_v = await ns_pair()\n"
                         "print([ns_k, isinstance(ns_v, dict), ns_v['op']])"))]
          (expect (re-find #"\['k', True, 'nested_ok'\]" out))
          (expect (= 1 (count @calls)))))
    ;; Regression: only a BARE `return tool(...)` settled; a call one level inside the
    ;; container the helper answered with (`return a, tool(...)`, `return {'k': [...]}`)
    ;; still reached the caller as a thunk.
    (it "a returned container settles the calls inside it"
        (let
          [out (run (str "def ns_box3():\n" "    return {'hits': [nested_ok(12)]}\n"
                         "ns_b = ns_box3()\n"
                         "print([isinstance(ns_b['hits'][0], dict), ns_b['hits'][0]['op']])"))]
          (expect (re-find #"\[True, 'nested_ok'\]" out))
          (expect (= 1 (count @calls)))))))

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
          (expect (str/includes? out "(`stage` \"stop\"): `status` \"stopped\", `exit`."))))))

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
(defdescribe
  a-page-never-shadows-a-function-test
  (it "keeps the function's own contract when a document claims its name"
      (try (doc-corpus/register-source! ::collision
                                        (constantly :once)
                                        (fn []
                                          [{:name "ls" :text "PAGE THAT MUST LOSE"}]))
           (let
             [ctx
              (:python-context (ep/create-python-context (ext/builtin-sandbox-bindings (fn []
                                                                                         nil))))

              out
              (str (:stdout (ep/run-python-block ctx "print(doc('ls'))")))]

             (expect (not (str/includes? out "PAGE THAT MUST LOSE")))
             (expect (str/includes? out "ls")))
           (finally (doc-corpus/register-source! ::collision (constantly :gone) (constantly []))))))

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

(defdescribe
  live-source-eviction-test
  ;; A `def` persists for the whole session, but its block's source was evicted
  ;; oldest-first, so a helper defined a few hundred blocks back stayed CALLABLE
  ;; while `inspect.getsource` on it died with "could not get source code" — the
  ;; model could run its own helper and could not read or refine it, so it
  ;; re-pasted the definition instead of changing it.
  (it "keeps the source of an old block that still backs a live definition"
      (let
        [ctx
         (tpc/context ::source-eviction)

         _
         (ep/run-python-block ctx "__vis_blocks_kept__ = 1")

         _
         (ep/run-python-block ctx "def kept_helper(a):\n    return a + 1\n")

         _
         (ep/run-python-block ctx "dead_one = 1")

         _
         (ep/run-python-block ctx "dead_two = 2")

         result
         (ep/run-python-block ctx
                              "import inspect\nprint(inspect.getsource(kept_helper).strip())\n")]

        (expect (str/includes? (:stdout result) "def kept_helper(a):"))
        (expect (str/includes? (:stdout result) "return a + 1"))))
  (it "still evicts a block once nothing live comes from it"
      (let
        [ctx
         (tpc/context ::source-eviction-churn)

         _
         (ep/run-python-block ctx "__vis_blocks_kept__ = 1")

         _
         (ep/run-python-block ctx "def churn():\n    return 1\n")

         _
         (ep/run-python-block ctx "def churn():\n    return 2\n")

         _
         (ep/run-python-block ctx "pass")

         result
         (ep/run-python-block ctx
                              (str
                                "import inspect\n"
                                "print(len(__vis_block_names__))\n"
                                "print(inspect.getsource(churn).strip().endswith('return 2'))\n"))]

        ;; The superseded definition's block is dropped; only the live one and the
        ;; running block stay resident.
        (expect (= "2\nTrue\n" (:stdout result)))))
  (it "never drops the source of the block that is about to run"
      (let
        [ctx
         (tpc/context ::source-eviction-current)

         _
         (ep/run-python-block ctx "__vis_blocks_kept__ = 1")

         _
         (ep/run-python-block ctx "def pin_one():\n    return 1\n")

         _
         (ep/run-python-block ctx "def pin_two():\n    return 2\n")

         result
         (ep/run-python-block
           ctx
           (str "import inspect\n"
                "def defined_here():\n    return 3\n"
                "print(inspect.getsource(defined_here).strip().endswith('return 3'))\n"))]

        (expect (= "True\n" (:stdout result))))))
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


;; Helper definitions that outlive the PROCESS

(defn- restore-into-fresh-sandbox
  "Persist one sandbox's definitions and restore them into a FRESH one, exactly
   as a gateway restart does. `setup` is either code to run in the first sandbox
   or `[:file src]` to plant a snapshot directly. Answers the probe's stdout, the
   restored count and the snapshot file; always removes the session's file."
  [setup probe-code]
  (let [sid (str "vis-test-defs-" (System/nanoTime))]
    (try (let
           [planted (when (vector? setup) (second setup))
            _ (when planted
                (let [f (io/file (paths/sandbox-defs-file sid))]
                  (io/make-parents f)
                  (spit f planted)))
            first-ctx (when-not planted (:python-context (ep/create-python-context {})))
            _ (when first-ctx (ep/run-python-block first-ctx setup))
            file (when first-ctx (ep/persist-session-defs! first-ctx sid))
            fresh (:python-context (ep/create-python-context {}))
            restored (ep/restore-session-defs! fresh sid)]

           {:file file
            :snapshot (or planted (when file (slurp file)))
            :restored restored
            :stdout (:stdout (ep/run-python-block fresh probe-code))})
         (finally (.delete (io/file (paths/sandbox-defs-file sid)))))))

(defdescribe
  session-defs-across-processes-test
  ;; The sandbox died with the PROCESS: kill the gateway, resume, and every helper
  ;; the session had refined was gone while the transcript still showed it — the
  ;; next call raised NameError against code the model could read but not run.
  (it "re-creates a helper, its module alias and its constant in a fresh sandbox"
      (let
        [{:keys [restored stdout snapshot]}
         (restore-into-fresh-sandbox
           "ROOT = \"/tmp/vis-defs-probe\"\nimport json as J\ndef shout(s):\n    return s.upper()\n"
           (str "print(shout(\"ok\"))\n" "print(ROOT, J.dumps([1]))\n"
                "import inspect\n" "print(inspect.getsource(shout).splitlines()[0])\n"))]
        (expect (= 1 restored))
        (expect (str/includes? snapshot "def shout(s):"))
        (expect (str/includes? stdout "OK"))
        (expect (str/includes? stdout "/tmp/vis-defs-probe [1]"))
        ;; Restored source reads back like a local one — that is what makes a
        ;; helper refinable instead of re-pasted.
        (expect (str/includes? stdout "def shout(s):"))))
  (it "keeps every definition that still loads when one statement no longer does"
      (let
        [{:keys [restored stdout]} (restore-into-fresh-sandbox
                                     [:file
                                      (str "import totally_missing_module as tm\n"
                                           "BROKEN = undefined_name\n"
                                           "def survivor(x):\n    return x * 2\n")]
                                     "print(survivor(21))\n")]
        (expect (= 1 restored))
        (expect (str/includes? stdout "42"))))
  ;; Regression: a helper defined inside `if:`/`try:` or inside another `def` read
  ;; back INDENTED, and one indented line made the whole snapshot unparseable —
  ;; the restore then lost EVERY helper the session had, not just that one.
  (it "restores the whole toolbox when a helper is nested, or bound under another name"
      (let
        [{:keys [restored stdout snapshot]}
         (restore-into-fresh-sandbox
           (str "def outer():\n    def inner(a):\n        return a * 2\n\n    return inner\n"
                "twice = outer()\n"
                "if True:\n\n    def gated(x):\n        return x + 1\n"
                "def plain(x):\n    return x - 1\n")
           "print(twice(4), gated(1), plain(3))\n")]
        (expect (= 4 restored))
        (expect (str/includes? stdout "8 2 2"))
        ;; The closure is stored under a name its own source never binds, and the
        ;; private name that rebinding needs is dropped again afterwards.
        (expect (str/includes? snapshot "twice = inner"))
        (expect (str/includes? snapshot "del inner"))))
  ;; Regression: only functions and scalars were snapshotted, so the class and the
  ;; config dict a helper closes over never came back — the restored helper raised
  ;; NameError on its first call, callable and useless.
  (it "brings back the class, the dataclass and the config its helpers need"
      (let
        [{:keys [restored stdout]}
         (restore-into-fresh-sandbox
           (str "from dataclasses import dataclass\n"
                "CFG = {\"depth\": 2}\n"
                "@dataclass\nclass Point:\n    x: int = 0\n    y: int = 0\n\n"
                "class Node:\n    def __init__(self, v):\n        self.v = v\n\n"
                "def origin():\n    return Point(1, 2)\n"
                "def node(v):\n    return Node(v).v\n"
                "def depth(p, cfg=CFG):\n    return cfg[\"depth\"] + p\n")
           "print(origin(), node(7), depth(1))\n")]
        (expect (= 3 restored))
        (expect (str/includes? stdout "Point(x=1, y=2) 7 3"))))
  ;; Regression: the snapshot `repr`ed every global before checking its size, so a
  ;; single multi-megabyte string cost ~130ms per block to render text the cap
  ;; then threw away.
  (it "never spends the snapshot on a value too big to store"
      (let
        [{:keys [snapshot stdout]} (restore-into-fresh-sandbox
                                     (str "blob = \"x\" * 400000\n" "def small(x):\n    return x\n")
                                     "print(\"blob\" in globals(), small(1))\n")]
        (expect (> 2000 (count snapshot)))
        (expect (str/includes? stdout "False 1"))))
  ;; Regression: `functools.lru_cache` answers a wrapper with no `__code__` of its
  ;; own, so a helper vanished from `defs()` and from the snapshot the moment it
  ;; was decorated.
  (it "keeps a decorated helper listed, and restores it"
      (let
        [{:keys [restored stdout]}
         (restore-into-fresh-sandbox
           (str "import functools\n" "@functools.lru_cache\ndef squared(n):\n    return n * n\n")
           "print(squared(5))\nprint(\"squared\" in defs())\n")]
        (expect (= 1 restored))
        (expect (str/includes? stdout "25"))
        (expect (str/includes? stdout "True"))))
  ;; Regression: an unparseable snapshot raised out of the statement-by-statement
  ;; fallback, so the safety net could not run on the one file it existed for.
  (it "answers zero, and leaves the sandbox usable, for a snapshot that will not parse"
      (let
        [{:keys [restored stdout]} (restore-into-fresh-sandbox [:file "def broken(:\n    ???\n"]
                                                               "print(1 + 1)\n")]
        (expect (= 0 restored))
        (expect (str/includes? stdout "2"))))
  ;; Regression: a restored helper was exec'd from RAW source, so it MISSED the block
  ;; rewrite every locally-defined helper gets. `await` on an already-settled value
  ;; raised "object ... can't be used in 'await' expression" inside a helper that had
  ;; worked all session, and a plain `def` whose body awaits was dropped from the
  ;; toolbox outright (SyntaxError) instead of being promoted to `async def`.
  (it "restores an awaiting helper with the same rewrite a local one gets"
      (let
        [{:keys [restored stdout]}
         (restore-into-fresh-sandbox
           (str "async def unwrap(v):\n    r = await v\n    return r\n"
                "def twice_unwrapped(v):\n    return await unwrap(v) * 2\n")
           (str "print(await unwrap(41))\n" "print(await twice_unwrapped(21))\n"))]
        (expect (= 2 restored))
        (expect (str/includes? stdout "41"))
        (expect (str/includes? stdout "42"))))
  (it "writes nothing, and drops a stale file, for a session with no definitions"
      (let
        [sid
         (str "vis-test-defs-" (System/nanoTime))

         f
         (io/file (paths/sandbox-defs-file sid))

         ctx
         (:python-context (ep/create-python-context {}))]

        (try (io/make-parents f)
             (spit f "def gone():\n    return 1\n")
             (ep/run-python-block ctx "x = 1")
             (expect (nil? (ep/persist-session-defs! ctx sid)))
             (expect (not (.exists f)))
             (finally (.delete f))))))

(defdescribe
  defs-verb-test
  ;; Listing your own helpers meant writing a globals()/co_filename comprehension
  ;; by hand every time, and reading one back meant remembering `inspect`.
  (it "lists the session's own functions, and refuses a name it never defined"
      (let
        [ctx
         (:python-context (ep/create-python-context {}))

         empty-out
         (:stdout (ep/run-python-block ctx "print(defs())"))

         _
         (ep/run-python-block ctx "from json import dumps\ndef widen(a, b=2):\n    return a * b\n")

         listed
         (:stdout (ep/run-python-block ctx "print(defs())"))

         source
         (:stdout (ep/run-python-block ctx "print(defs(\"widen\"))"))

         missing
         (:stdout (ep/run-python-block ctx
                                       (str "try:\n" "    defs(\"nope\")\n"
                                            "except NameError as exc:\n"
                                            "    print(\"refused:\", exc)\n")))]

        (expect (str/includes? empty-out "no functions defined by this session yet"))
        (expect (str/includes? listed "widen(a, b=2)"))
        ;; An IMPORTED function is not this session's definition.
        (expect (not (str/includes? listed "dumps")))
        (expect (str/includes? source "def widen(a, b=2):"))
        (expect (str/includes? missing "refused:")))))

(defdescribe tool-shadow-test
             ;; Regression: a helper named after a bound tool was accepted in silence and then
             ;; quietly dropped — `def patch(...)` lived only inside its own block, was never
             ;; persisted, and the next block silently got the tool back instead.
             (it
               "refuses a top-level def or class named after a bound tool, and keeps the tool"
               (let
                 [ctx
                  (:python-context (ep/create-python-context {}))

                  _
                  (ep/set-python-binding! ctx
                                          'shadow_probe
                                          (fn [& _]
                                            "REAL-TOOL"))

                  refused
                  (ep/run-python-block ctx "def shadow_probe(a):\n    return a\n")

                  klass
                  (ep/run-python-block ctx "class defs:\n    pass\n")

                  nested
                  (:stdout (ep/run-python-block ctx
                                                (str "def outer():\n"
                                                     "    def defs(x):\n" "        return x\n"
                                                     "    return defs(7)\n" "print(outer())\n")))

                  var-shadow
                  (:stdout (ep/run-python-block ctx
                                                "shadow_probe = 'a string'\nprint(shadow_probe)"))

                  after
                  (:stdout (ep/run-python-block ctx "print(shadow_probe('x'))"))]

                 (expect (str/includes? (str (:error refused)) "`shadow_probe` is a bound tool"))
                 (expect (str/includes? (str (:error refused)) "shadow_probe_mine"))
                 (expect (str/includes? (str (:error klass)) "`defs` is a bound tool"))
                 ;; A def nested in another function is an ordinary local, and a plain
                 ;; assignment is still a block-local shadow: neither is refused.
                 (expect (str/includes? nested "7"))
                 (expect (str/includes? var-shadow "a string"))
                 (expect (str/includes? after "REAL-TOOL"))))
             ;; Regression, the same trap across processes: a snapshot written before a tool
             ;; existed re-created `def patch(...)` straight over the real one, and the tool
             ;; was gone for the whole process — while the restored count never noticed.
             (it "never restores a definition whose name is a bound tool now"
                 (let
                   [{:keys [restored stdout]}
                    (restore-into-fresh-sandbox
                      [:file
                       (str "def defs(*a, **k):\n    return \"HIJACKED\"\n\n"
                            "doc = \"clobbered\"\n"
                            "def kept(n):\n    return n * 3\n")]
                      (str "print(kept(2))\n" "print(\"HIJACKED\" in str(defs()))\n"
                           "print(callable(doc))\n" "print(__vis_restore_dropped__)\n"))]
                   (expect (= 1 restored))
                   (expect (str/includes? stdout "6"))
                   (expect (str/includes? stdout "False"))
                   (expect (str/includes? stdout "True"))
                   (expect (str/includes? stdout "['defs', 'doc']")))))
