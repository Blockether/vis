(ns com.blockether.vis.internal.foundation.editing.balance-test
  "The decision that stands between a language pack's delimiter repair and the file
   on disk. Every case here is pure: the `balancer` is a stub returning the exact
   candidate, so what is under test is which candidates are ALLOWED to be written —
   not any particular pack's idea of a repair."
  (:require [com.blockether.vis.internal.foundation.editing.balance :as balance]
            [lazytest.core :refer [defdescribe expect it]]))

(defn- verdict
  "`rebalance` over a stub balancer that answers `candidate`, with every candidate
   treated as parseable unless `parses?` says otherwise."
  ([source candidate spans] (verdict source candidate spans true))
  ([source candidate spans parses?]
   (balance/rebalance {:balancer (constantly candidate)
                       :parses-clean? (constantly parses?)
                       :source source
                       :spans spans})))

(defdescribe changed-span-test
             (it "answers nil when nothing changed"
                 (expect (nil? (balance/changed-span "(a)\n(b)\n" "(a)\n(b)\n"))))
             (it "names the single line an edit rewrote, 1-based"
                 (expect (= [2 2] (balance/changed-span "(a)\n(b)\n(c)\n" "(a)\n(B)\n(c)\n"))))
             (it "spans every line a multi-line edit covers"
                 (expect (= [2 3]
                            (balance/changed-span "(a)\n(b)\n(c)\n(d)\n" "(a)\n(B)\n(C)\n(d)\n"))))
             (it "spans the inserted lines when the edit grew the file"
                 (expect (= [2 3] (balance/changed-span "(a)\n(d)\n" "(a)\n(b)\n(c)\n(d)\n"))))
             (it "answers the seam line when the edit only deleted lines"
                 (expect (= [1 1] (balance/changed-span "(a)\n(b)\n" "(a)\n")))))

(defdescribe
  rebalance-test
  (it "answers nil when the language has no repair to ask"
      (expect (nil?
                (balance/rebalance
                  {:balancer nil :parses-clean? (constantly true) :source "(a" :spans [[1 1]]}))))
  (it "keeps a repair that only closes a delimiter on the caller's own line"
      (let [r (verdict "(ns a)\n(defn f [] (inc 1)\n" "(ns a)\n(defn f [] (inc 1))\n" [[2 2]])]
        (expect (true? (:ok? r)))
        (expect (= "(ns a)\n(defn f [] (inc 1))\n" (:content r)))
        ;; the note is the whole point: the caller is TOLD which character landed where
        (expect (= ["line 2 added `)`"] (:notes r)))))
  (it "names a removed delimiter just as precisely"
      (expect (= ["line 1 removed `)`"] (:notes (verdict "(a))\n" "(a)\n" [[1 1]])))))
  ;; Regression, session 621ba390: a repair was applied to the caller's replacement FRAGMENT alone,
  ;; so a partial form silently closed itself and the write landed on a line the caller
  ;; never meant to touch. A repair may only move delimiters ON the lines the edit wrote.
  (it "refuses a repair that balances a line the edit never wrote"
      (let
        [r (verdict "(ns a)\n(defn f [] (inc 1)\n\n(defn g [] 2)\n"
                    "(ns a)\n(defn f [] (inc 1)\n\n(defn g [] 2))\n"
                    [[2 2]])]
        (expect (false? (:ok? r)))
        (expect
          (= "a delimiter repair exists but it changes line 4, outside the lines this call edited"
             (:why r)))))
  (it "refuses when the repair changes nothing"
      (expect (= "no delimiter repair was found" (:why (verdict "(a" "(a" [[1 1]])))))
  (it "refuses when the balancer answers something that is not a string"
      (expect (= "no delimiter repair was found" (:why (verdict "(a" nil [[1 1]])))))
  (it "refuses a candidate that still would not parse"
      (expect (= "a delimiter repair was found but it still would not parse"
                 (:why (verdict "(a" "(a)" [[1 1]] false)))))
  (it "refuses a candidate that rewrites code instead of delimiters"
      (expect (= "the delimiter repair would rewrite code, not delimiters"
                 (:why (verdict "(inc 1\n" "(dec 1)\n" [[1 1]])))))
  (it "refuses a candidate that changes the file's final newline"
      (expect (= "the delimiter repair would change the file's final newline"
                 (:why (verdict "(a\n" "(a)" [[1 1]])))))
  (it "refuses a candidate that adds or drops lines"
      (expect (= "the delimiter repair would add or drop lines"
                 (:why (verdict "(a\n(b)\n" "(a)(b)\n" [[1 2]])))))
  (it "allows a repair anywhere inside a multi-line edit's own span"
      (let
        [r (verdict "(ns a)\n(defn f []\n  (inc 1)\n" "(ns a)\n(defn f []\n  (inc 1))\n" [[2 3]])]
        (expect (true? (:ok? r)))
        (expect (= ["line 3 added `)`"] (:notes r))))))
