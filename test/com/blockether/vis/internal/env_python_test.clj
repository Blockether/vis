(ns com.blockether.vis.internal.env-python-test
  "GraalPy sandbox behaviour that needs a REAL context: the proxy→dict boundary
   fix and the print-capture of tool results. Boots ONE context for the ns."
  (:require
   [com.blockether.vis.internal.env-python :as ep]
   [lazytest.core :refer [defdescribe expect it]]))

(defdescribe proxy-and-capture-test
  (let [env (ep/create-python-context {})
        ctx (:python-context env)]
    (it "a raw tool-result proxy is NOT json-serializable; after settle it's a REAL mutable dict"
      ;; `test_proxy` is bound via ->py → a ProxyHashMap (ForeignDict). It passes
      ;; isinstance(dict) but json.dumps() raises and it is read-only — the silent
      ;; friction. Assigning it auto-settles → __vis_pyify__ → a REAL python dict:
      ;; json.dumps works, it's mutable, and nested maps are converted too.
      ;; `eof nil` reproduces cat's `:next-offset nil` — ->py stores Java null which
      ;; GraalPy surfaces as ForeignNone (`x is None` is False); pyify must normalize
      ;; it or json.dumps chokes.
      (ep/bind-and-bump! env 'test_proxy {"op" "cat" "a" {"b" 1} "eof" nil})
      (let [r (ep/run-python-block ctx
                (str "import json\n"
                  "try:\n"
                  "    json.dumps(test_proxy); raw_json = True\n"
                  "except Exception:\n"
                  "    raw_json = False\n"
                  "x = test_proxy\n"               ;; auto-settle → pyify → real dict
                  "x['added'] = 7\n"               ;; mutation works on a real dict
                  "post = (isinstance(x, dict) and (json.dumps(x) is not None)\n"
                  "        and x['added'] == 7 and isinstance(x['a'], dict))\n"
                  "print(['raw_json', raw_json, 'post', post])"))]
        (expect (re-find #"\['raw_json', False, 'post', True\]" (str (:stdout r))))))
    (it "MEASURE: pyify cost across sizes (session-scale ~30/100 vs a large 5000-entry result)"
      (ep/bind-and-bump! env 'sess_proxy
        (into {} (for [i (range 30)] [(str "k" i) {"v" i "w" nil}])))   ;; session-scale
      (ep/bind-and-bump! env 'med_proxy
        (into {} (for [i (range 100)] [(str "k" i) {"v" i "w" nil}])))  ;; generous session
      (ep/bind-and-bump! env 'big_proxy
        (into {} (for [i (range 5000)] [(str "k" i) {"v" i "w" nil}])))
      (let [r (ep/run-python-block ctx
                ;; pyify fires at TOP-LEVEL settle only — time each top-level assignment.
                (str "import time\n"
                  "t0 = time.perf_counter()\n"
                  "a = sess_proxy\n"                ;; settle → pyify (30 nested)
                  "t1 = time.perf_counter()\n"
                  "b = med_proxy\n"                 ;; settle → pyify (100 nested)
                  "t2 = time.perf_counter()\n"
                  "c = big_proxy\n"                 ;; settle → pyify (5000 nested)
                  "t3 = time.perf_counter()\n"
                  "print('pyify_ms', {'n30': round((t1-t0)*1000,3), 'n100': round((t2-t1)*1000,3), 'n5000': round((t3-t2)*1000,3), 'lens': [len(a), len(b), len(c)]})"))]
        (println "PERF>>>" (:stdout r))
        (expect (re-find #"n30" (str (:stdout r))))))
    (it "captures a REAL tool result (proxy→__VisResult__) by TYPE; a model dict with 'op' is NOT captured"
      ;; `tp` is a HOST proxy with 'op' → pyify marks it __VisResult__. A model-built
      ;; dict with 'op' is a PLAIN dict → not a __VisResult__ → correctly NOT captured.
      (ep/bind-and-bump! env 'tp {"op" "cat" "x" 1})
      (let [real  (ep/run-python-block ctx "print(tp)")            ;; proxy result → captured
            faked (ep/run-python-block ctx "print({'op':'cat'})")  ;; model dict → NOT captured (robust)
            plain (ep/run-python-block ctx "print('just text')")
            two   (ep/run-python-block ctx "print(tp); print(tp)")]
        (expect (= 1 (count (:printed-results real))))
        (expect (= "cat" (:op (first (:printed-results real)))))   ;; origin = (:op result)
        (expect (empty? (:printed-results faked)))                 ;; robustness: model 'op' dict ignored
        (expect (empty? (:printed-results plain)))
        (expect (= 2 (count (:printed-results two))))
        (expect (re-find #"'op'" (str (:stdout real))))))          ;; stdout (context) still shows it
    (it "mixed print (text + result) keeps :only-printed-results? FALSE so stdout text is never dropped"
      (ep/bind-and-bump! env 'tp {"op" "cat" "x" 1})
      (let [pure  (ep/run-python-block ctx "print(tp)")
            mixed (ep/run-python-block ctx "print('FOUND:'); print(tp)")]
        (expect (true? (:only-printed-results? pure)))           ;; pure result print → cards may replace
        (expect (not (:only-printed-results? mixed)))            ;; mixed → show full stdout
        (expect (= 1 (count (:printed-results mixed))))          ;; the result is still captured
        (expect (re-find #"FOUND:" (str (:stdout mixed))))))
    (it "session is a REAL dict after bind-ctx! — json.dumps(session) works (was a ForeignDict)"
      (ep/bind-ctx! ctx {"workspace" "/x" "roots" ["a" "b"] "facts" {"k" "v"}})
      (let [r (ep/run-python-block ctx
                "import json\nprint([isinstance(session, dict), json.dumps(session) is not None, session['workspace']])")]
        (expect (re-find #"\[True, True, '/x'\]" (str (:stdout r))))))))   ;; the text survives (the bug)
