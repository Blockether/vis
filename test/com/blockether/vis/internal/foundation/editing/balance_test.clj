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
        (expect (= ["line 2 added `)` → `(defn f [] (inc 1))`"] (:notes r)))))
  ;; Regression: `(-> s str/trim)` typed with its opening paren lost reads as
  ;; `-> s str/trim)`, and dropping that surplus `)` — character for character the same
  ;; mistake as an honest `)` too many — used to be WRITTEN, quietly turning a threaded
  ;; call into loose symbols that parse.
  (it "refuses a repair that deletes a delimiter the caller wrote"
      (let
        [r (verdict "(defn f [s]\n  -> s str/trim))\n" "(defn f [s]\n  -> s str/trim)\n" [[2 2]])]
        (expect (false? (:ok? r)))
        (expect (= (str "the delimiter repair would delete `)` this edit wrote: the replacement "
                        "closes more than it opens, or dropped an opener")
                   (:why r)))))
  ;; A closer omitted in the MIDDLE of a line comes back at that line's END and regroups
  ;; the arguments between it, so the character alone tells the caller nothing — the note
  ;; carries the line the repair produced.
  (it "names the line the repair produced, not only the character"
      (let
        [r
         (verdict "(println \"a\" (count xs \"b\")\n" "(println \"a\" (count xs \"b\"))\n" [[1 1]])]
        (expect (true? (:ok? r)))
        (expect (= ["line 1 added `)` → `(println \"a\" (count xs \"b\"))`"] (:notes r)))))
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
        (expect (= ["line 3 added `)` → `(inc 1))`"] (:notes r)))))
  ;; Regression: `(foo [1 2] 3)` mistyped as `(foo (1 2] 3)` came back as `(foo (1 2 3))`
  ;; — a vector turned into a call that swallowed the argument after it. Every earlier
  ;; check passed: it parses, it is one line, the skeleton is identical, and the only
  ;; characters that moved are delimiters.
  (it "refuses a repair that retypes a delimiter the caller wrote"
      (expect (= "the delimiter repair would move or retype a delimiter you wrote"
                 (:why (verdict "(foo (1 2] 3)\n" "(foo (1 2 3))\n" [[1 1]])))))
  ;; Regression: a closer deleted from one line and re-added after the NEXT form moved
  ;; that form inside its neighbour — `(g a) (h a)` became `(g a (h a))`.
  (it "refuses a repair that moves a closer past a form the caller wrote"
      (expect (= "the delimiter repair would move or retype a delimiter you wrote"
                 (:why (verdict "(f\n  (g a)\n    (h a))\n" "(f\n  (g a\n    (h a)))\n" [[1 3]])))))
  (it "keeps a closer that moved without crossing anything the caller wrote"
      ;; the caller's own indentation is what says `:else 2` belongs to the `cond`, and
      ;; no other delimiter of theirs changed places — this is the repair's whole job
      (let [r (verdict "(cond\n  a 1)\n  :else 2\n" "(cond\n  a 1\n  :else 2)\n" [[1 3]])]
        (expect (true? (:ok? r)))
        (expect (= ["line 2 removed `)` → `a 1`" "line 3 added `)` → `:else 2)`"] (:notes r))))))
