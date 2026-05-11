(ns com.blockether.vis.internal.history-restore-test
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.history-restore :as hr]
            [com.blockether.vis.internal.persistance :as persistance]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe history-restore-test
  (it "detects runtime-ref sentinels"
    (expect (hr/runtime-ref? {:vis/ref :expr}))
    (expect (not (hr/runtime-ref? {:vis/ref :depth-exceeded})))
    (expect (not (hr/runtime-ref? {:x 1}))))

  (it "extracts names from simple def forms"
    (expect (= "prompt-slice"
              (hr/def-symbol-name "(def prompt-slice (subvec xs 0 2))")))
    (expect (= "foo"
              (hr/def-symbol-name "  (clojure.core/defonce foo 1)")))
    (expect (nil? (hr/def-symbol-name "(+ 1 2)"))))

  (it "restored-var-values omits runtime-ref values"
    (with-redefs [persistance/db-restore-blocks
                  (fn [_db _cid]
                    [{:name "ok" :result [1 2 3]}
                     {:name "missing" :result {:vis/ref :expr}}
                     {:result "nameless"}])]
      (expect (= {"ok" [1 2 3]}
                (hr/restored-var-values :db "cid")))))

  (it "restored-def-result renders the durable value for a def form"
    (let [rendered (hr/restored-def-result {"prompt-slice" ["alpha" "beta"]}
                     "(def prompt-slice (subvec xs 0 2))")]
      (expect (str/includes? rendered "alpha"))
      (expect (str/includes? rendered "beta")))
    (expect (nil? (hr/restored-def-result {"prompt-slice" ["alpha"]}
                    "(+ 1 2)")))))
