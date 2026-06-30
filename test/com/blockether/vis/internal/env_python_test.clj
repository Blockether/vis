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
    (it "printing a tool-result dict (carries 'op') captures it; plain print does NOT"
      (let [r1 (ep/run-python-block ctx "print({'op':'cat','x':1})")
            r2 (ep/run-python-block ctx "print('just text')")
            r3 (ep/run-python-block ctx "print({'op':'rg'}); print({'op':'cat'})")]
        (expect (= 1 (count (:printed-results r1))))
        ;; ->clj keywordizes keys, so origin is `(:op result)` (the render layer's key)
        (expect (= "cat" (:op (first (:printed-results r1)))))
        (expect (empty? (:printed-results r2)))
        (expect (= 2 (count (:printed-results r3))))
        ;; context (stdout) is UNCHANGED — what the model printed is still there
        (expect (re-find #"'op'" (str (:stdout r1))))))))
