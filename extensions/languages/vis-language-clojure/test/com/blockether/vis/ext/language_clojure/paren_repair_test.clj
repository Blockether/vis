(ns com.blockether.vis.ext.language-clojure.paren-repair-test
  (:require [com.blockether.vis.ext.language-clojure.paren-repair :as repair]
            [lazytest.core :refer [defdescribe describe expect it]]))

(defdescribe delimiter-error?-test
             (describe "delimiter-error?"
                       (it "false for balanced source"
                           (expect (false? (repair/delimiter-error? "(defn f [] (+ 1 2))")))
                           (expect (false? (repair/delimiter-error? "[1 2 {:a 3}]"))))
                       (it "true for a missing closer"
                           (expect (true? (repair/delimiter-error? "(defn f [] (+ 1 2)")))
                           (expect (true? (repair/delimiter-error? "(let [x 1"))))))

(defdescribe
  fix-delimiters-test
  (describe "fix-delimiters"
            (it "leaves already-balanced source UNTOUCHED"
                (let [s "(defn f [] (+ 1 2))"]
                  (expect (= s (repair/fix-delimiters s)))))
            (it "appends the missing closer (parinfer indent-mode)"
                ;; indentation says the form ends at the line — parinfer adds the `)`
                (expect (= "(defn f []\n  (+ 1 2))"
                           (repair/fix-delimiters "(defn f []\n  (+ 1 2)"))))
            (it "repairs a missing vector + paren closer together"
                (let [out (repair/fix-delimiters "(let [x 1\n  x")]
                  (expect (not (repair/delimiter-error? out)))))
            (it "a repaired result always parses clean"
                (doseq [broken ["(defn g [a]\n  (when a\n    a)" "(map inc [1 2 3]" "{:a 1 :b 2"]]
                  (let [out (repair/fix-delimiters broken)]
                    (expect (string? out))
                    (expect (not (repair/delimiter-error? out))))))))
