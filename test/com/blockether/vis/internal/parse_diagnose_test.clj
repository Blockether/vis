(ns com.blockether.vis.internal.parse-diagnose-test
  (:require [com.blockether.vis.internal.parse-diagnose :as pd]
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
                 (expect (nil? (pd/unresolved-symbol-hint "Unable to resolve symbol: hits"
                                                          ['xxxxxxxxxxxxxxx 'yyyyyyyyy]))))
             (it "returns nil when message is not unresolved-symbol shape"
                 (expect (nil? (pd/unresolved-symbol-hint "Divide by zero" ['hits])))))
(defdescribe diagnose-bracket-balance-test
             (it "returns nil when balanced"
                 (expect (nil? (pd/diagnose-bracket-balance "print(foo(bar))"))))
             (it "flags a wrong-type closer with open/close + 1-based line/col"
                 (let [r (pd/diagnose-bracket-balance "cat(\"x\"]")]
                   (expect (= :unbalanced-bracket (:reason r)))
                   (expect (= \( (:open r)))
                   (expect (= \] (:close r)))
                   (expect (= 1 (:line r)))
                   (expect (= 8 (:col r)))))
             (it "flags an unclosed opener with :close nil"
                 (let [r (pd/diagnose-bracket-balance "foo(bar")]
                   (expect (= :unbalanced-bracket (:reason r)))
                   (expect (= \( (:open r)))
                   (expect (nil? (:close r)))))
             (it "flags an extra closer with :open nil"
                 (let [r (pd/diagnose-bracket-balance "foo)bar")]
                   (expect (= :unbalanced-bracket (:reason r)))
                   (expect (nil? (:open r)))
                   (expect (= \) (:close r)))))
             (it "ignores brackets inside strings, comments, and triple-quoted strings"
                 (expect (nil? (pd/diagnose-bracket-balance "x = {\")\": 1}")))
                 (expect (nil? (pd/diagnose-bracket-balance "# )]}\nok()")))
                 (expect (nil? (pd/diagnose-bracket-balance "s = \"\"\")\"\"\"\ny()")))))


