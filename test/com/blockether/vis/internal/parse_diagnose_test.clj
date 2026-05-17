(ns com.blockether.vis.internal.parse-diagnose-test
  (:require
   [com.blockether.vis.internal.parse-diagnose :as pd]
   [lazytest.core :refer [defdescribe expect it]]))

(defdescribe count-unescaped-quotes-test
  (it "zero on empty / nil"
    (expect (= 0 (pd/count-unescaped-quotes "")))
    (expect (= 0 (pd/count-unescaped-quotes nil))))
  (it "counts plain quotes"
    (expect (= 2 (pd/count-unescaped-quotes "\"a\"")))
    (expect (= 1 (pd/count-unescaped-quotes "\"unclosed"))))
  (it "skips escaped quotes"
    (expect (= 0 (pd/count-unescaped-quotes "\\\"")))
    (expect (= 2 (pd/count-unescaped-quotes "\"a\\\"b\""))))
  (it "double-backslash does not eat the next quote"
    (expect (= 2 (pd/count-unescaped-quotes "\"a\\\\\"")))))

(defdescribe diagnose-quote-balance-test
  (it "returns nil when balanced"
    (expect (nil? (pd/diagnose-quote-balance "\"a\" \"b\""))))
  (it "flags unbalanced and pinpoints line"
    (let [r (pd/diagnose-quote-balance "(def x \"first\nstill open\n(def y 2)")]
      (expect (= :unbalanced-quote (:reason r)))
      (expect (= 1 (:line r))))))

(defdescribe unresolved-symbol-hint-test
  (it "suggests closest sandbox bindings"
    (expect (= "Unresolved `hits`. Closest defined: `hit`."
              (pd/unresolved-symbol-hint
                "Unable to resolve symbol: hits in this context"
                ['hit 'tree 'completely-different]))))
  (it "returns nil when no candidate is close enough"
    (expect (nil? (pd/unresolved-symbol-hint
                    "Unable to resolve symbol: hits"
                    ['xxxxxxxxxxxxxxx 'yyyyyyyyy]))))
  (it "returns nil when message is not unresolved-symbol shape"
    (expect (nil? (pd/unresolved-symbol-hint "Divide by zero" ['hits])))))
