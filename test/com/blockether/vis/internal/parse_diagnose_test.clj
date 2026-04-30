(ns com.blockether.vis.internal.parse-diagnose-test
  "Coverage for `internal/parse-diagnose`, plus the canonical
   real-world reproduction case.

   The big one: conversation `cf9e29b5-b0a4-4121-9879-d9706cc2e22a`'s
   last answer was a multi-line `(answer (md/join …))` Polish-language
   write-up about Polish typographic quotes, and the LLM dropped one
   close-quote in the middle. edamame surfaced the failure as

       Parse error: Invalid symbol: zobaczył:

   pointing at line 15 — but the actual mistake was on line 14, where
   the line carries five (odd) unescaped double-quotes. Without a
   diagnostic hint the next iteration is told 'fix the symbol on
   line 15' and walks straight past the real bug.

   The pinned snippet stays here forever as the canonical reproducer
   so future tweaks to the iteration loop's parse pipeline keep
   surfacing a useful hint for this class of LLM mistake."
  (:require
   [clojure.string :as str]
   [com.blockether.vis.internal.parse-diagnose :as diag]
   [edamame.core :as edamame]
   [lazytest.core :refer [defdescribe expect it]]))

;; -----------------------------------------------------------------------------
;; Pinned reproducer — verbatim from the broken iteration block.
;; -----------------------------------------------------------------------------

(def broken-snippet-from-cf9e29b5
  ;; Verbatim copy of `(answer …)` block that failed to parse.
  ;; DO NOT 'fix' this string — its odd-quote-count is the whole
  ;; point of the regression. Pretty-printing or auto-balancing it
  ;; would invalidate the test.
  (str
    "(answer\n"
    "  (md/join\n"
    "    (md/h2 \"Ha, ironia :D\")\n"
    "    (md/p \"To nie był problem z \" (md/code \"v/\") \" jako takim — to był problem z \"\n"
    "      (md/bold \"polskimi cudzysłowami\") \" w stringu.\")\n"
    "    (md/h3 \"Co się stało w iteracji 12.1\")\n"
    "    (md/p \"Kod wyglądał mniej więcej tak:\")\n"
    "    (md/code-block\n"
    "      (str \"(md/p \\\"Wszystkie błędy \\u201eUnable to resolve symbol: v/ls\\\", \"\n"
    "           \"\\u201ev/cat\\\" etc.)\")\n"
    "      \"clojure\")\n"
    "    (md/p \"Problem: polski cudzysłów otwierający \" (md/code \"\\u201e\") \" (U+201E, DOUBLE LOW-9 QUOTATION MARK) \"\n"
    "      \"nie jest delimiterem stringa w Clojure. Ale \" (md/bold \"zwykły\") \" \"\n"
    "      (md/code \"\\\"\") \" (U+0022) \" już tak.\")\n"
    "    (md/p \"Więc reader zobaczył:\")))"))

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

  (it "escaped quote does NOT count"
    (expect (= 2 (diag/count-unescaped-quotes "(md/code \"\\\"\")")))
    (expect (= 4 (diag/count-unescaped-quotes "say \"\\\"\\\"\" twice"))))

  (it "literal backslash followed by quote: escape eats the next char"
    ;; Source `\\\"` = one literal backslash then an escaped quote;
    ;; the escape rule consumes the second `\` and skips ahead to
    ;; the `"` after it as a string delimiter.
    (expect (= 2 (diag/count-unescaped-quotes "\\\\\"foo\""))))

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

  (it "the broken snippet flags line 14 — the actual cause"
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
