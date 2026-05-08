(ns com.blockether.vis.internal.parse-diagnose-test
  "Coverage for `internal/parse-diagnose`, plus the canonical
   real-world reproduction case.

   The big one: conversation `cf9e29b5-b0a4-4121-9879-d9706cc2e22a`'s
   last answer was a multi-line `(answer (v/join ...))` Polish-language
   write-up about Polish typographic quotes, and the LLM dropped one
   close-quote in the middle. edamame surfaced the failure as

       Parse error: Invalid symbol: zobaczył:

   pointing at line 15 - but the actual mistake was on line 14, where
   the line carries five (odd) unescaped double-quotes. Without a
   diagnostic hint the next iteration is told 'fix the symbol on
   line 15' and walks straight past the real bug.

   The pinned snippet stays here forever as the canonical reproducer
   so future tweaks to the iteration loop's parse pipeline keep
   surfacing a useful hint for this class of LLM mistake."
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [com.blockether.vis.internal.parse-diagnose :as diag]
   [edamame.core :as edamame]
   [lazytest.core :refer [defdescribe expect it]]))

;; -----------------------------------------------------------------------------
;; Pinned reproducer - verbatim from the broken iteration block.
;; -----------------------------------------------------------------------------

(def broken-snippet-from-cf9e29b5
  ;; Verbatim copy of `(answer ...)` block that failed to parse.
  ;; DO NOT 'fix' this string - its odd-quote-count is the whole
  ;; point of the regression. Pretty-printing or auto-balancing it
  ;; would invalidate the test.
  (str
    "(answer\n"
    "  (v/join\n"
    "    (v/h2 \"Ha, ironia :D\")\n"
    "    (v/p \"To nie był problem z \" (v/code \"v/\") \" jako takim - to był problem z \"\n"
    "      (v/bold \"polskimi cudzysłowami\") \" w stringu.\")\n"
    "    (v/h3 \"Co się stało w iteracji 12.1\")\n"
    "    (v/p \"Kod wyglądał mniej więcej tak:\")\n"
    "    (v/code-block\n"
    "      (str \"(v/p \\\"Wszystkie błędy \\u201eUnable to resolve symbol: v/ls\\\", \"\n"
    "           \"\\u201ev/cat\\\" etc.)\")\n"
    "      \"clojure\")\n"
    "    (v/p \"Problem: polski cudzysłów otwierający \" (v/code \"\\u201e\") \" (U+201E, DOUBLE LOW-9 QUOTATION MARK) \"\n"
    "      \"nie jest delimiterem stringa w Clojure. Ale \" (v/bold \"zwykły\") \" \"\n"
    "      (v/code \"\\\"\") \" (U+0022) \" już tak.\")\n"
    "    (v/p \"Więc reader zobaczył:\")))"))

;; -----------------------------------------------------------------------------
;; Reproduction: edamame DOES fail on the broken snippet, with the
;; misleading row pointing past the real bug.
;; -----------------------------------------------------------------------------

(defdescribe broken-snippet-edamame-failure
  (it "edamame raises `Invalid symbol: zobaczył:` (Polish word with trailing colon)"
    (let [thrown (try
                   (edamame/parse-string-all broken-snippet-from-cf9e29b5
                     {:all true :readers (fn [_tag] (fn [v] (list 'do v)))})
                   nil
                   (catch Throwable t t))]
      (expect (some? thrown))
      (expect (str/includes? (.getMessage ^Throwable thrown) "Invalid symbol"))
      (expect (str/includes? (.getMessage ^Throwable thrown) "zobaczył"))))

  (it "edamame's reported row is the symptom (line 15 area), not the cause (line 14)"
    (let [data (try
                 (edamame/parse-string-all broken-snippet-from-cf9e29b5
                   {:all true :readers (fn [_tag] (fn [v] (list 'do v)))})
                 nil
                 (catch Throwable t (ex-data t)))]
      (expect (= 15 (:row data))))))

;; -----------------------------------------------------------------------------
;; Diagnostic helpers under test.
;; -----------------------------------------------------------------------------

(defdescribe count-unescaped-quotes-test
  (it "no quotes -> 0"
    (expect (zero? (diag/count-unescaped-quotes "no quotes here")))
    (expect (zero? (diag/count-unescaped-quotes nil)))
    (expect (zero? (diag/count-unescaped-quotes ""))))

  (it "balanced plain string -> 2"
    (expect (= 2 (diag/count-unescaped-quotes "say \"hi\""))))

  (it "escaped quote does NOT count - (v/code \"\\\"\") parses as 2 delimiters around the escape"
    (expect (= 2 (diag/count-unescaped-quotes "(v/code \"\\\"\")"))))

  (it "a backslash before a quote is treated as the escape - even when the backslash itself was a literal in the source"
    ;; Raw source bytes: `say "\"\"" twice`
    ;; (Clojure literal: "say \"\\\"\\\"\" twice")
    ;; Counter walks the BYTES, treating every backslash as the
    ;; opener of an escape pair regardless of whether it itself
    ;; arrived from a `\\` source sequence. Two `\` bytes in the
    ;; middle eat the two embedded quotes -> only the outer pair
    ;; counts.
    (expect (= 2 (diag/count-unescaped-quotes "say \"\\\"\\\"\" twice"))))

  (it "the broken snippet has an ODD count (the regression signature)"
    (expect (odd? (diag/count-unescaped-quotes broken-snippet-from-cf9e29b5)))))

(defdescribe first-odd-quote-line-test
  (it "balanced source returns nil"
    (expect (nil? (diag/first-odd-quote-line "(println \"a\" \"b\")"))))

  (it "single-line unbalanced source returns line 1"
    (expect (= 1 (diag/first-odd-quote-line "(str \"hello)"))))

  (it "multi-line unbalanced source returns the FIRST line where running count is odd"
    (expect (= 2
              (diag/first-odd-quote-line
                (str "(println \"a\" \"b\")\n"
                  "  (println \"oops)\n"
                  "(println \"c\")")))))

  (it "the broken snippet flags line 14 - the actual cause"
    (expect (= 14 (diag/first-odd-quote-line broken-snippet-from-cf9e29b5)))))

(defdescribe diagnose-quote-balance-test
  (it "returns nil when the source is balanced"
    (expect (nil? (diag/diagnose-quote-balance "(str \"hello\")"))))

  (it "returns a structured diagnostic for the broken snippet"
    (let [d (diag/diagnose-quote-balance broken-snippet-from-cf9e29b5)]
      (expect (some? d))
      (expect (= :unbalanced-quote (:reason d)))
      (expect (= 14 (:line d)))
      (expect (odd? (:total d)))
      (expect (str/includes? (:hint d) "line 14"))
      (expect (str/includes? (:hint d) "Unbalanced double-quote")))))

;; -----------------------------------------------------------------------------
;; Auto-repair - parinfer-equivalent for quotes.
;; -----------------------------------------------------------------------------

(defn- edamame-parses? [^String src]
  (try
    (edamame/parse-string-all src
      {:all true :readers (fn [_tag] (fn [v] (list 'do v)))})
    true
    (catch Throwable _ false)))

(defdescribe try-quote-rebalance-test
  (it "returns nil when the source is already balanced"
    (expect (nil? (diag/try-quote-rebalance
                    "(println \"a\" \"b\")"
                    edamame-parses?))))

  (it "removes a stray `\"` and returns a parsable variant (extra-quote case)"
    (let [broken "(str \"foo\" \"bar\" \")"
          fixed  (diag/try-quote-rebalance broken edamame-parses?)]
      (expect (some? fixed))
      (expect (edamame-parses? fixed))
      ;; The quote count of the fix is even.
      (expect (even? (diag/count-unescaped-quotes fixed)))))

  (it "appends a missing `\"` at end of line (missing-close case)"
    ;; A fence-style mistake: opener present, no closer on the line.
    (let [broken "(println \"hello)"
          fixed  (diag/try-quote-rebalance broken edamame-parses?)]
      (expect (some? fixed))
      (expect (edamame-parses? fixed))))

  (it "REPAIRS the broken cf9e29b5 snippet - the regression that motivated this whole fn"
    (let [fixed (diag/try-quote-rebalance broken-snippet-from-cf9e29b5
                  edamame-parses?)]
      (expect (some? fixed))
      (expect (edamame-parses? fixed))
      ;; The repair is local: only the offending line should change.
      ;; Lines 1-13 and 15+ stay byte-identical.
      (let [original-lines (str/split-lines broken-snippet-from-cf9e29b5)
            fixed-lines    (str/split-lines fixed)]
        (expect (= (count original-lines) (count fixed-lines)))
        (doseq [i (range (count original-lines))]
          (when (and (not= 13 i) (not= 14 i)) ;; line 14 (1-based) is at idx 13
            (expect (= (nth original-lines i) (nth fixed-lines i)))))))))

;; -----------------------------------------------------------------------------
;; Multi-line unclosed-string regression - eeaf9651-...
;;
;; Different shape of the same LLM-mistake family: the model wrote
;; `(let [content "..."])` with a multi-line markdown blob inside
;; and forgot to close the string at the END of the markdown.
;; Edamame surfaces the failure as `Invalid symbol: with:` (or any
;; other word ending with a colon) somewhere mid-prose, because the
;; unclosed string lets the reader chew through what was supposed
;; to be plain text and stumble on the first colon-suffixed token.
;;
;; The existing `try-quote-rebalance` only walks repair candidates
;; on the FIRST odd-quote line - fine for the cf9e29b5 case where
;; the missing close-quote is on the same line as the opener, but
;; broken for a multi-line unclosed string. Until the rescue is
;; extended to try inserting `\"` at later line breaks, this test
;; pins the failure shape so the gap is visible.
;;
;; Source: conversation eeaf9651-06c7-4dda-9e97-877fcef06337,
;;         turn 760d9435-..., iter 0 (status :done, no answer).
;; The verbatim 5 KB block lives at
;; `test/resources/parse-fixtures/eeaf9651-multi-line-unclosed-string.clj.txt`
;; - don't pretty-print or auto-balance it; the broken byte sequence
;; IS the regression signature.
;; -----------------------------------------------------------------------------

(def multi-line-unclosed-string
  ;; Slurped at namespace-load time; the fixture is checked in
  ;; verbatim so the test stays deterministic across machines.
  (slurp
    (io/resource
      "parse-fixtures/eeaf9651-multi-line-unclosed-string.clj.txt")))

(defdescribe multi-line-unclosed-string-failure-test
  (it "edamame surfaces the failure as `Invalid symbol: <word>:`"
    ;; The exact word edamame trips on depends on the input shape;
    ;; what matters is that it's a colon-suffixed prose token that
    ;; the reader walked into AFTER the unclosed string opener.
    ;; In the live blob it's `with:` (line of `... search with re-seq:`).
    (let [thrown (try
                   (edamame/parse-string-all multi-line-unclosed-string
                     {:all true :readers (fn [_tag] (fn [v] (list 'do v)))})
                   nil
                   (catch Throwable t t))]
      (expect (some? thrown))
      (let [msg (.getMessage ^Throwable thrown)]
        (expect (str/includes? msg "Invalid symbol"))
        ;; The message ends with `:` - the colon-suffixed word that
        ;; tripped the reader. Pin the shape, not the exact word.
        (expect (re-find #"Invalid symbol: \S+:" msg)))))

  (it "the source has an ODD count of unescaped quotes (the regression signature)"
    (expect (odd? (diag/count-unescaped-quotes multi-line-unclosed-string))))

  (it "current try-quote-rebalance does NOT repair the multi-line case"
    ;; This is the gap. `try-quote-rebalance` only walks repair
    ;; candidates ON the first odd-quote line: removing the `\"`
    ;; from the opener line turns the markdown body into bare
    ;; symbols, and appending `\"` at end of the opener line
    ;; leaves the next ~150 lines unparseable. Neither produces a
    ;; parseable variant, so the function returns nil. Pin the
    ;; gap so it's obvious when it's been closed.
    (expect (nil? (diag/try-quote-rebalance multi-line-unclosed-string
                    edamame-parses?)))))

;; -----------------------------------------------------------------------------
;; Eval-time diagnostic: bare prose symbol inside a vector of strings.
;;
;; Source: conversation ec64266c-...'s turn 2. The model wrote
;; `(answer (v/join (v/ul ["...string-a" "string-b" SzerokoscWordHere ...])))`
;; - third element of the vector is missing its opening `\"`. The
;; whole form parses (`Szerokosc...` is a legal Clojure symbol), so
;; the parse-rescue chain never fires. SCI explodes at eval-time
;; with `Unable to resolve symbol: Szerokość` and the model gets
;; no actionable signal.
;;
;; The two pinned fixtures are the verbatim broken iter-0 and iter-1
;; bodies. They live as `.clj.txt` so editors don't \"helpfully\"
;; balance the broken vector during a save.
;; -----------------------------------------------------------------------------

(def bare-prose-symbol-iter0
  (slurp
    (io/resource
      "parse-fixtures/ec64266c-bare-prose-symbol-iter0.clj.txt")))

(def bare-prose-symbol-iter1
  (slurp
    (io/resource
      "parse-fixtures/ec64266c-bare-prose-symbol-iter1.clj.txt")))

(defdescribe unresolved-symbol-hint-test
  (it "returns nil when the error message isn't an unresolve"
    (expect (nil? (diag/unresolved-symbol-hint
                    "NullPointerException: oops"
                    "(do (println :x))"))))

  (it "returns nil when args are nil"
    (expect (nil? (diag/unresolved-symbol-hint nil "(answer)")))
    (expect (nil? (diag/unresolved-symbol-hint
                    "Unable to resolve symbol: X" nil))))

  (it "returns nil for ASCII kebab-case symbols (legitimate Clojure idents)"
    (expect (nil? (diag/unresolved-symbol-hint
                    "Unable to resolve symbol: foo-bar"
                    "(v/ul [\"a\" foo-bar \"b\"])"))))

  (it "returns nil for ASCII Java-class shape (no non-ASCII letter signal)"
    (expect (nil? (diag/unresolved-symbol-hint
                    "Unable to resolve symbol: String"
                    "(.length String)"))))

  (it "returns nil when the symbol is prose-shaped but NOT in a missing-quote context"
    (expect (nil? (diag/unresolved-symbol-hint
                    "Unable to resolve symbol: Niektóre"
                    "(let [x 1] Niektóre)"))))

  (it "returns a hint for the iter-0 fixture (`Szerokość` inside a v/ul vector of strings)"
    (let [hint (diag/unresolved-symbol-hint
                 "ExceptionInfo: Unable to resolve symbol: Szerokość"
                 bare-prose-symbol-iter0)]
      (expect (some? hint))
      (expect (str/includes? hint "Szerokość"))
      (expect (str/includes? hint "unquoted string fragment"))
      (expect (str/includes? hint "opening `\"`"))))

  (it "returns a hint for the iter-1 fixture (`Niektóre` inside a v/ul vector of strings)"
    (let [hint (diag/unresolved-symbol-hint
                 "ExceptionInfo: Unable to resolve symbol: Niektóre"
                 bare-prose-symbol-iter1)]
      (expect (some? hint))
      (expect (str/includes? hint "Niektóre"))
      (expect (str/includes? hint "unquoted string fragment"))))

  (it "both fixtures have an EVEN unescaped-quote count (parse passes; this is the regression signature)"
    ;; The whole point of this diagnostic class: parse rescue can't
    ;; help because edamame is happy. Quote balance is even, the
    ;; bare prose word slips through as a legal Clojure symbol.
    (expect (even? (diag/count-unescaped-quotes bare-prose-symbol-iter0)))
    (expect (even? (diag/count-unescaped-quotes bare-prose-symbol-iter1)))))

;; -----------------------------------------------------------------------------
;; Eval-time auto-repair: `try-answer-string-restitch`.
;;
;; Pure candidate generator. Returns a vec of repair candidates; the
;; caller (`loop/execute-code`) is responsible for re-evaluating them
;; through SCI and picking the first that succeeds.
;;
;; Tests below pin: (a) the scope guards (no `(answer`, non-prose
;; symbol, no missing-quote ctx), and (b) the structural shape of
;; the candidates (parses cleanly through edamame, contains the
;; symbol now wrapped as a string).
;; -----------------------------------------------------------------------------

(defdescribe try-answer-string-restitch-test
  (it "returns nil when source has no `(answer` form (out of scope)"
    (expect (nil? (diag/try-answer-string-restitch
                    "(let [x 1] Niektóre)" "Niektóre"))))

  (it "returns nil for ASCII kebab-case symbol (might be a real binding)"
    (expect (nil? (diag/try-answer-string-restitch
                    "(answer (v/ul [\"a\" foo-bar \"b\"]))" "foo-bar"))))

  (it "returns nil when the prose symbol isn't in a missing-quote context"
    (expect (nil? (diag/try-answer-string-restitch
                    "(answer Niektóre)" "Niektóre"))))

  (it "returns nil when the symbol isn't found in source"
    (expect (nil? (diag/try-answer-string-restitch
                    "(answer (v/ul [\"a\" \"b\"]))" "Szerokość"))))

  (it "returns 2 candidates for the iter-0 fixture and at least one parses"
    (let [cands (diag/try-answer-string-restitch
                  bare-prose-symbol-iter0 "Szerokość")]
      (expect (= 2 (count cands)))
      ;; Strategy A (just prepend `\"`) won't parse because the
      ;; element body has an inner `\"` that breaks alignment.
      ;; Strategy B (prepend + escape inner `\"`s) DOES parse.
      (expect (some edamame-parses? cands))))

  (it "returns 2 candidates for the iter-1 fixture and at least one parses"
    (let [cands (diag/try-answer-string-restitch
                  bare-prose-symbol-iter1 "Niektóre")]
      (expect (= 2 (count cands)))
      (expect (some edamame-parses? cands))))

  (it "the parsable candidate wraps the bare prose symbol as part of a string literal (iter-0)"
    ;; Pin the structural shape: after restitch, edamame parses the
    ;; (v/ul [...]) form and the `Szerokość` word ends up INSIDE a
    ;; string element rather than as a bare symbol.
    (let [parsable (some #(when (edamame-parses? %) %)
                     (diag/try-answer-string-restitch
                       bare-prose-symbol-iter0 "Szerokość"))]
      (expect (some? parsable))
      (let [parsed (edamame/parse-string-all parsable
                     {:all true :readers (fn [_tag] (fn [v] (list 'do v)))})]
        ;; Find the `(v/ul [...])` form in the parse tree.
        (let [v-ul-form (some (fn walk [form]
                                (cond
                                  (and (seq? form) (= 'v/ul (first form)))
                                  form
                                  (coll? form) (some walk form)))
                          parsed)]
          (expect (some? v-ul-form))
          (let [vec-arg (second v-ul-form)
                third   (nth vec-arg 2)]
            (expect (vector? vec-arg))
            (expect (string? third))
            (expect (str/includes? third "Szerokość")))))))

  (it "only mutates source between the symbol and the enclosing `]`/`)`"
    ;; Lines outside the offending element stay byte-identical so
    ;; the repair stays surgical and the audit trail in :original-code
    ;; vs the repaired source diff is small.
    (let [parsable (some #(when (edamame-parses? %) %)
                     (diag/try-answer-string-restitch
                       bare-prose-symbol-iter0 "Szerokość"))
          orig-lines  (str/split-lines bare-prose-symbol-iter0)
          fixed-lines (str/split-lines parsable)]
      (expect (= (count orig-lines) (count fixed-lines)))
      ;; Lines 1-10 (1-based) are headers/preamble; line 11 is the
      ;; broken element. Everything except line 11 stays verbatim.
      (doseq [i (range (count orig-lines))]
        (when (not= 10 i) ;; line 11 (1-based) is at idx 10
          (expect (= (nth orig-lines i) (nth fixed-lines i))))))))
