(ns com.blockether.vis.internal.foundation.editing.balance-test
  "The decision that stands between a language pack's delimiter repair and the file
   on disk. Every case here is pure: the `balancer` is a stub returning the exact
   candidate, so what is under test is which candidates are ALLOWED to be written —
   not any particular pack's idea of a repair."
  (:require [com.blockether.vis.internal.foundation.editing.balance :as balance]
            [lazytest.core :refer [defdescribe expect it]]))

(defn- reads?
  "True when `s` reads as Clojure — the stand-in for a language pack's own parser, and the
   whole gate for a case about the candidate this namespace BUILDS: it may be accepted
   because it really parses, never because a stub said everything does."
  [^String s]
  (binding [*read-eval* false]
    (try (vector? (read-string (str "[" s "\n]"))) (catch Exception _ false))))

(defn- verdict
  "`rebalance` over a stub balancer that answers `candidate`, with that answer as the only
   text that PARSES — so a case here is decided by the rules about it, and not by the
   candidate `rebalance` builds itself once it is refused. `parses?` replaces that predicate,
   as a function or as a bare true/false for a world where everything or nothing parses."
  ([source candidate spans] (verdict source candidate spans #{candidate}))
  ([source candidate spans parses?]
   (balance/rebalance {:balancer (constantly candidate)
                       :parses-clean? (if (ifn? parses?) parses? (constantly parses?))
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
        (expect (= (str "the delimiter repair would delete `)` this edit wrote: it closes more "
                        "than it opens, or an opener was lost")
                   (:why r)))))
  ;; The same refusal serves a whole-file formatter, which has no edit to blame: `:subject`
  ;; names WHOSE delimiters these are, so neither caller has to borrow the other's wording.
  (it "names the caller's own subject in a refusal"
      (expect (= (str "the delimiter repair would delete `)` this file has: it closes more "
                      "than it opens, or an opener was lost")
                 (:why (balance/rebalance {:balancer (constantly "(defn f [s]\n  -> s str/trim)\n")
                                           :parses-clean? (constantly true)
                                           :source "(defn f [s]\n  -> s str/trim))\n"
                                           :spans [[1 2]]
                                           :subject "this file has"})))))
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
  ;; A dropped quote is not a missing bracket, and a caller told only that no repair was
  ;; found goes looking for a delimiter that is not missing.
  (it "names an unterminated string instead of a delimiter that is not missing"
      (expect (= (str "no delimiter repair is possible: line 2 opens a string that is never "
                      "closed, and a repair only puts back `()[]{}`")
                 (:why (verdict "(ns a)\n(def s \"hi)\n" nil [[2 2]])))))
  (it "is not fooled by a quote inside a comment or a character literal"
      (expect (= "no delimiter repair was found"
                 (:why
                   (verdict "(def q \\\")\n;; \" not a string\n(def s \"ok\")\n" nil [[1 1]])))))
  ;; Whitespace is the one rewrite every other rule is blind to: the skeleton drops it, the
  ;; delimiters are untouched, and the line count and final newline both hold.
  (it "refuses a repair that re-indents the line it closed"
      (let [r (verdict "(ns a)\n(defn f [] (inc 1)\n" "(ns a)\n  (defn f [] (inc 1))\n" [[2 2]])]
        (expect (false? (:ok? r)))
        (expect
          (= (str "the delimiter repair would change whitespace this edit wrote: it re-indents "
                  "or re-ends lines instead of only putting back the delimiters that were omitted")
             (:why r)))))
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
      (expect (= "the delimiter repair would move or retype a delimiter this edit wrote"
                 (:why (verdict "(foo (1 2] 3)\n" "(foo (1 2 3))\n" [[1 1]])))))
  ;; Regression: a closer deleted from one line and re-added after the NEXT form moved
  ;; that form inside its neighbour — `(g a) (h a)` became `(g a (h a))`.
  (it "refuses a repair that moves a closer past a form the caller wrote"
      (expect (= "the delimiter repair would move or retype a delimiter this edit wrote"
                 (:why (verdict "(f\n  (g a)\n    (h a))\n" "(f\n  (g a\n    (h a)))\n" [[1 3]])))))
  (it "keeps a closer that moved without crossing anything the caller wrote"
      ;; the caller's own indentation is what says `:else 2` belongs to the `cond`, and
      ;; no other delimiter of theirs changed places — this is the repair's whole job
      (let [r (verdict "(cond\n  a 1)\n  :else 2\n" "(cond\n  a 1\n  :else 2)\n" [[1 3]])]
        (expect (true? (:ok? r)))
        (expect (= ["line 2 removed `)` → `a 1`" "line 3 added `)` → `:else 2)`"] (:notes r))))))

(defn- seated
  "`rebalance` given the text the edit REPLACED, so a delimiter it dropped can go back
   where that text had it. `candidate` is the stub balancer's answer — the indentation
   guess the replaced text has to beat — and `parses?` says what may be written at all, for a
   case whose point is which candidate is REFUSED."
  ([original source candidate spans] (seated original source candidate spans (constantly true)))
  ([original source candidate spans parses?]
   (balance/rebalance {:balancer (constantly candidate)
                       :parses-clean? parses?
                       :source source
                       :original original
                       :spans spans})))

(defdescribe
  seated-repair-test
  "The text an edit REPLACED is better evidence than the caller's indentation: it says
   WHERE a dropped delimiter sat, which is the only way to put one back in the MIDDLE of
   a line, or to tell a lost opener from one closer too many. It is tried first; the
   balancer's own answer is the fallback."
  ;; Regression, session 621ba390 (the other half): a closer omitted inside a line came back at
  ;; the line's END, regrouping the arguments between — `(map? x) (str …)` was written as
  ;; `(map? x (str …))`, one cond clause turned into a call, and it parsed, so it was written.
  (it "seats a closer where the replaced text had it, not at the end of the line"
      (let
        [r (seated "(cond\n  (map? x) (str \"map\" (count x)))\n"
                   "(cond\n  (map? x (str \"map\" (count x)))\n"
                   "(cond\n  (map? x (str \"map\" (count x))))\n" [[2 2]])]
        (expect (true? (:ok? r)))
        (expect (= "(cond\n  (map? x) (str \"map\" (count x)))\n" (:content r)))
        (expect (= ["line 2 added `)` → `(map? x) (str \"map\" (count x)))`"] (:notes r)))))
  ;; Regression: `str/split-lines` drops the `\r` of a CRLF file, so a seat rebuilt by
  ;; joining its answer normalized every line ending in the file — a whole-file rewrite
  ;; from a call that asked to put ONE delimiter back.
  (it "keeps the CRLF line endings of the file it seats a delimiter into"
      (let
        [r (seated "(ns a)\r\n(defn ok [] (inc 1))\r\n"
                   "(ns a)\r\n(defn ok [] (inc 1)\r\n"
                   nil
                   [[2 2]])]
        (expect (true? (:ok? r)))
        (expect (= "(ns a)\r\n(defn ok [] (inc 1))\r\n" (:content r)))))
  ;; Regression: `(defn ok [] (inc 1))` retyped without its opening paren IS `defn ok [] (inc 1))`
  ;; — the same string as one closer too many, and refused as such when that is all there is. The
  ;; line it replaced says which of the two happened, so this one is repaired instead of refused.
  (it "restores an opener the edit lost, which the replacement alone cannot prove"
      (let
        [r (seated "(ns a)\n(defn ok [] (inc 1))\n" "(ns a)\ndefn ok [] (inc 1))\n"
                   "(ns a)\ndefn ok [] (inc 1)\n" [[2 2]])]
        (expect (true? (:ok? r)))
        (expect (= "(ns a)\n(defn ok [] (inc 1))\n" (:content r)))
        (expect (= ["line 2 added `(` → `(defn ok [] (inc 1))`"] (:notes r)))))
  (it "leaves the delimiters of code the edit deleted deleted"
      ;; the line's CODE changed, so nothing of its is seated: an opener whose form the caller
      ;; removed must not come back INSIDE what they wrote, and the surplus closer is theirs
      (let
        [r (seated "(defn ok []\n  (when x (inc 1)))\n" "(defn ok []\n  (inc 1)))\n"
                   "(defn ok []\n  (inc 1))\n" [[2 2]])]
        (expect (false? (:ok? r)))
        (expect (= (str "the delimiter repair would delete `)` this edit wrote: it closes more "
                        "than it opens, or an opener was lost")
                   (:why r)))))
  (it "falls back to the balancer for a line the edit did not replace"
      ;; an INSERTED form replaced nothing, so where its missing closer belongs is an indentation
      ;; guess and nothing else — the fallback is the whole repair here
      (let
        [r (seated "(ns a)\n(defn f [] 1)\n" "(ns a)\n(defn g [] (inc 1)\n(defn f [] 1)\n"
                   "(ns a)\n(defn g [] (inc 1))\n(defn f [] 1)\n" [[2 2]])]
        (expect (true? (:ok? r)))
        (expect (= "(ns a)\n(defn g [] (inc 1))\n(defn f [] 1)\n" (:content r)))))
  ;; Regression: one `(` too many is not an omission — parinfer closed it, the file parsed, and
  ;; `(inc x)` was written as the call of a call.
  (it "refuses to close a delimiter the edit typed, which the replaced text never had"
      (let
        [r (seated "(ns a)\n(defn f [x] (inc x))\n" "(ns a)\n(defn f [x] ((inc x))\n"
                   "(ns a)\n(defn f [x] ((inc x)))\n" [[2 2]])]
        (expect (false? (:ok? r)))
        (expect (= (str "the delimiter repair would close `(` this edit wrote on line 2, where the "
                        "text it replaced had `)`: that delimiter was retyped or added, not "
                        "omitted, and closing it regroups the line into `(defn f [x] ((inc x)))`")
                   (:why r)))))
  (it "seats an opener BEFORE the code the edit added on the same line"
      ;; the `[` belongs where the replaced text had it — in front of the bindings, including the
      ;; pair this edit appended; seating it after them would bind nothing
      (let
        [r (seated "(defn f []\n  (let [a 1]\n    a))\n"
                   "(defn f []\n  (let a 1 b 2]\n    (+ a b)))\n"
                   "(defn f []\n  (let a 1 b 2\n    (+ a b)))\n" [[2 3]])]
        (expect (true? (:ok? r)))
        (expect (= "(defn f []\n  (let [a 1 b 2]\n    (+ a b)))\n" (:content r)))
        (expect (= ["line 2 added `[` → `(let [a 1 b 2]`"] (:notes r)))))
  ;; Regression: the edit swapped two lines and dropped one closer; the balancer closed the OTHER
  ;; line — one this edit left exactly as it found it — the file parsed, and the `if` lost its
  ;; branches to the form above it.
  (it "refuses a repair that closes a line whose code the edit kept"
      ;; the balancer's answer is the only text that parses here, so the refusal is what is
      ;; under test — the case after this one hands the same edit a real parser
      (let
        [balanced
         "(defn f [xs]\n    (println \"a\"))\n  (if (seq xs\n    (println \"b\")))\n"

         r
         (seated "(defn f [xs]\n  (if (seq xs)\n    (println \"a\")\n    (println \"b\")))\n"
                 "(defn f [xs]\n    (println \"a\")\n  (if (seq xs\n    (println \"b\")))\n"
                 balanced
                 [[2 3]]
                 #{balanced})]

        (expect (false? (:ok? r)))
        (expect (= (str "a delimiter repair exists but it adds `)` to line 2, whose code this edit "
                        "did not change — the text it replaced was `(println \"a\")` and never had "
                        "that delimiter, so what this call omitted is on another line")
                   (:why r)))))
  (it "closes the line the edit left open when the balancer aimed at one it kept"
      (let
        [r (seated "(defn f [xs]\n  (if (seq xs)\n    (println \"a\")\n    (println \"b\")))\n"
                   "(defn f [xs]\n    (println \"a\")\n  (if (seq xs\n    (println \"b\")))\n"
                   "(defn f [xs]\n    (println \"a\"))\n  (if (seq xs\n    (println \"b\")))\n"
                   [[2 3]]
                   reads?)]
        (expect (true? (:ok? r)))
        (expect (= "(defn f [xs]\n    (println \"a\")\n  (if (seq xs)\n    (println \"b\")))\n"
                   (:content r)))))
  (it "appends to the last line the edit WROTE, not to the last line of the span"
      ;; the span reaches a line this edit left exactly as it found it, and the text it
      ;; replaced is the evidence that nothing of that line's is missing
      (let
        [r (seated "(defn f []\n  (when (pos? y))\n  (inc 1))\n"
                   "(defn f []\n  (if x\n  (inc 1))\n"
                   nil
                   [[2 3]]
                   reads?)]
        (expect (true? (:ok? r)))
        (expect (= "(defn f []\n  (if x)\n  (inc 1))\n" (:content r))))))

(defn- tailed
  "`rebalance` with a real parse check and no replaced text: `candidate` is what a pack's
   balancer would answer, so what is under test is the LAST candidate — the closers this edit
   omitted, appended at the end of the line it wrote."
  ([source spans] (tailed source nil spans))
  ([source candidate spans]
   (balance/rebalance
     {:balancer (constantly candidate) :parses-clean? reads? :source source :spans spans})))

(defdescribe
  tail-close-test
  "What is left when both witnesses are silent: the closers the text never wrote, at the end of
   the last line this call wrote. The file parsed before the edit, so the omission is the
   edit's own; the note hands back the line it produced."
  ;; Regression, session 2224a346: a 36-line insertion missing exactly one `)` was REFUSED —
  ;; the pack's repair put that closer on a line outside the edit, so nothing could be written,
  ;; and the same block was re-sent with one more `)` an iteration later.
  (it "closes what the edit left open at the end of the last line it wrote"
      (let [r (tailed "(ns a)\n\n(defn f []\n  (when x\n    (inc 1))\n" [[3 5]])]
        (expect (true? (:ok? r)))
        (expect (= "(ns a)\n\n(defn f []\n  (when x\n    (inc 1)))\n" (:content r)))
        (expect (= ["line 5 added `)` → `(inc 1)))`"] (:notes r)))))
  (it "repairs the omission a balancer wanted to close outside the edit"
      (let
        [r (tailed "(ns a)\n(defn f [] (inc 1)\n\n(defn g [] 2)\n"
                   "(ns a)\n(defn f [] (inc 1)\n\n(defn g [] 2))\n"
                   [[2 2]])]
        (expect (true? (:ok? r)))
        (expect (= "(ns a)\n(defn f [] (inc 1))\n\n(defn g [] 2)\n" (:content r)))))
  ;; The other half of `additions-only?`: one closer too many and a LOST OPENER are the same
  ;; string, so appending can tell them apart no better than deleting could.
  (it "refuses to guess when the text closes more than it opens"
      (let [r (tailed "(defn f [s]\n  -> s str/trim))\n" [[2 2]])]
        (expect (false? (:ok? r)))
        (expect (= "no delimiter repair was found" (:why r)))))
  (it "refuses when this call wrote more than one region"
      ;; two edits have untouched lines between them, and a closer appended after the second
      ;; takes lines nobody in this call wrote into the form it closes
      (expect (false? (:ok? (tailed "(ns a)\n(defn f [] (inc 1)\n(def x 1)\n(defn g [] 2)\n"
                                    [[2 2] [4 4]])))))
  (it "closes that same text when the one region is the caller's own"
      (expect (= "(ns a)\n(defn f [] (inc 1))\n(def x 1)\n(defn g [] 2)\n"
                 (:content (tailed "(ns a)\n(defn f [] (inc 1)\n(def x 1)\n(defn g [] 2)\n"
                                   [[2 2]])))))
  (it "keeps the CRLF line endings of the file it closes"
      (expect (= "(ns a)\r\n(defn f [] (inc 1))\r\n"
                 (:content (tailed "(ns a)\r\n(defn f [] (inc 1)\r\n" [[2 2]])))))
  (it "seats the closer after the code, not after the line's trailing whitespace"
      (expect (= "(ns a)\n(defn f [] (inc 1))   \n"
                 (:content (tailed "(ns a)\n(defn f [] (inc 1)   \n" [[2 2]])))))
  (it "passes over a blank line at the end of the span"
      (expect (= "(ns a)\n(defn f []\n  (inc 1))\n\n"
                 (:content (tailed "(ns a)\n(defn f []\n  (inc 1)\n\n" [[2 4]])))))
  (it "refuses closers that do not make the file parse"
      ;; the seat is inside a string the edit wrote, so the closer lands as text and the
      ;; deficit it was meant to answer is still there
      (expect (false? (:ok? (tailed "(ns a)\n(def s \"one\n        two\")\n(defn f [] (inc 1)\n"
                                    [[2 3]]))))))

(defn- rewrote
  "`rebalance` with the text the edit REPLACED and a real parse check — the evidence a
   language pack has nothing to say about, because a repair only puts back `()[]{}`."
  ([original source spans] (rewrote original source nil spans))
  ([original source candidate spans]
   (balance/rebalance {:balancer (constantly candidate)
                       :parses-clean? reads?
                       :source source
                       :original original
                       :spans spans})))

(defdescribe
  requote-test
  "A dropped `\"` is not a missing bracket: no balancer can put one back, every string after it
   is inside out, and the reader stops somewhere else entirely. The text the edit REPLACED is
   the only witness that a quote ended that region, and it licenses exactly one — at the end of
   the last line the edit wrote."
  ;; Regression, session cc1ca6dd: a replacement dropped a docstring's closing `"`, the refusal
  ;; named a line 56 lines below the edit as the one opening an unclosed string, and the same
  ;; replacement was re-sent twice before the quote was noticed.
  (it
    "puts back the quote the text this edit replaced ended with"
    (let
      [r
       (rewrote
         "(ns a)\n(defn f\n  \"Doc line one.\n   The rule above it stays a border.\"\n  []\n  (inc 1))\n"
         "(ns a)\n(defn f\n  \"Doc line one.\n   The rule below it is a slab over the frame.\n  []\n  (inc 1))\n"
         [[4 4]])]
      (expect (true? (:ok? r)))
      (expect
        (=
          "(ns a)\n(defn f\n  \"Doc line one.\n   The rule below it is a slab over the frame.\"\n  []\n  (inc 1))\n"
          (:content r)))
      (expect (= ["line 4 added `\"` → `The rule below it is a slab over the frame.\"`"]
                 (:notes r)))))
  (it "leaves a string open that the replaced text never closed there"
      ;; the region this edit replaced ended in `)`, so nothing here says a quote belongs at its
      ;; end — the caller is told which line opens the string instead
      (let
        [r (rewrote "(ns a)\n(def s \"one\"\n       \"two\")\n"
                    "(ns a)\n(def s \"one\"\n       two\")\n"
                    [[3 3]])]
        (expect (false? (:ok? r)))
        (expect (= (str "no delimiter repair is possible: line 3 opens a string that is never "
                        "closed, and a repair only puts back `()[]{}`")
                   (:why r)))))
  (it "refuses a quote that closes the string and leaves the file unparseable"
      ;; the same edit also opened a form it never closed, and one quote is all this candidate
      ;; is ever allowed to put back
      (let
        [r (rewrote "(ns a)\n(defn f\n  \"doc\"\n  [] 1)\n"
                    "(ns a)\n(defn f\n  (when x\n  \"doc\n  [] 1)\n"
                    [[3 4]])]
        (expect (false? (:ok? r)))
        (expect (= (str "no delimiter repair is possible: line 4 opens a string that is never "
                        "closed, and a repair only puts back `()[]{}`")
                   (:why r)))))
  (it "puts the quote on the last line the edit wrote, not the last line of the span"
      (let
        [r (rewrote "(ns a)\n(defn f\n  \"Doc one.\n   ends here.\"\n  [] 1)\n"
                    "(ns a)\n(defn f\n  \"Doc one.\n   ends there.\n  [] 1)\n"
                    [[4 5]])]
        (expect (true? (:ok? r)))
        (expect (= "(ns a)\n(defn f\n  \"Doc one.\n   ends there.\"\n  [] 1)\n" (:content r))))))
