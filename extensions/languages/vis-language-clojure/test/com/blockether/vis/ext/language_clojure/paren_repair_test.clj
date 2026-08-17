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
                           (expect (true? (repair/delimiter-error? "(let [x 1"))))
                       (it "true for one closer too many"
                           (expect (true? (repair/delimiter-error? "(defn f [] (+ 1 2)))"))))
                       ;; Regression, session de256b01: the gate was edamame, which reports
                       ;; `:edamame/opened-delimiter` only when the innermost failure IS the
                       ;; delimiter. Source cut mid-token failed on the TOKEN, so a file two
                       ;; closers short answered "nothing to repair here" and went to the
                       ;; formatter as written.
                       (it "true when whatever lost the closers also cut a token"
                           (expect (true? (repair/delimiter-error?
                                            "(defn alt [attrs]\n  (or (:alt attrs) (:"))))
                       (it "false for balanced delimiters it cannot read"
                           ;; not this question — that one is `reads-clean?`
                           (expect (false? (repair/delimiter-error? "(def x 1/2/3)"))))))

(defdescribe reads-clean?-test
             (describe "reads-clean?"
                       (it "true for source that reads end to end"
                           (expect (true? (repair/reads-clean? "(defn f [] (+ 1 2))")))
                           (expect (true? (repair/reads-clean? "#?(:clj 1 :cljs 2)")))
                           (expect (true? (repair/reads-clean? "#:foo{:a 1}"))))
                       (it "false for balanced text whose code does not read"
                           (expect (false? (repair/reads-clean? "(or (:alt attrs) (:))")))
                           (expect (false? (repair/reads-clean? "(def x 1/2/3)"))))
                       (it "false for a string that never closes"
                           (expect (false? (repair/reads-clean? "(str \"open"))))))

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
            (it "a repaired result always reads clean"
                (doseq [broken ["(defn g [a]\n  (when a\n    a)" "(map inc [1 2 3]" "{:a 1 :b 2"]]
                  (let [out (repair/fix-delimiters broken)]
                    (expect (string? out))
                    (expect (repair/reads-clean? out)))))
            ;; Regression, session de256b01: closing `(or (:alt attrs) (:` balances the
            ;; delimiters and writes `(:)`, which is not a keyword. The check asked
            ;; edamame for a DELIMITER error, got none for that unreadable result, and
            ;; handed the text back as a repair worth writing.
            (it "refuses a repair whose result does not read"
                (expect (nil? (repair/fix-delimiters "(defn alt [attrs]\n  (or (:alt attrs) (:"))))
            (it "leaves what is not a delimiter mistake to the reader that can name it"
                ;; balanced delimiters, unreadable number — no repair to make here
                (let [s "(def x 1/2/3)"]
                  (expect (= s (repair/fix-delimiters s)))))
            (it "nil for a string that never closes"
                ;; parinfer will not rewrite a file it cannot read, so the repair is a no-op
                (expect (nil? (repair/fix-delimiters "(str \"open"))))))
