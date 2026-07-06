(ns com.blockether.vis.internal.foundation.editing.patch-test
  "Fuzzy line-matching toolkit covered here. Envelope-mode parser / hunk
   applier were retired together with `patch`'s envelope grammar; only
   the per-line fuzzy passes remain.

   Temp files under `target/editing-test/` stay inside cwd so
   `safe-path` accepts them."
  (:require [clojure.string]
            [com.blockether.vis.internal.foundation.editing.patch :as patch]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe
  seek-sequence-fuzzy-test
  (it "exact match wins" (expect (= 1 (patch/seek-sequence ["a" "b" "c"] ["b"] 0 false))))
  (it "ignores trailing whitespace" (expect (= 0 (patch/seek-sequence ["foo   "] ["foo"] 0 false))))
  (it "ignores leading+trailing whitespace"
      (expect (= 0 (patch/seek-sequence ["   foo  "] ["foo"] 0 false))))
  (it "normalizes typographic punctuation"
      (expect (= 0 (patch/seek-sequence ["it’s"] ["it's"] 0 false)))
      (expect (= 0 (patch/seek-sequence ["a—b"] ["a-b"] 0 false))))
  (it "returns nil when pattern not present"
      (expect (nil? (patch/seek-sequence ["a" "b"] ["c"] 0 false))))
  (it "returns nil when pattern longer than input (no panic)"
      (expect (nil? (patch/seek-sequence ["only"] ["too" "long"] 0 false))))
  (it "prefers EOF position when eof? is true"
      (expect (= 2 (patch/seek-sequence ["x" "y" "x"] ["x"] 0 true)))))

(defdescribe seek-sequence-with-pass-test
             ;; seek-sequence-with-pass is what powers the line-based fuzzy fallback
             ;; in patch exact-replace mode. It MUST report which fuzzy strategy
             ;; actually fired so the model can tell the difference between an
             ;; expected exact hit and a salvage — the prompt nudges humans toward
             ;; that information.
             (it "reports :exact for clean matches"
                 (expect (= {:start 1 :pass :exact}
                            (patch/seek-sequence-with-pass ["a" "b" "c"] ["b"] 0 false))))
             (it "reports :rstrip when the file has trailing whitespace but the pattern does not"
                 (let [hit (patch/seek-sequence-with-pass ["foo   "] ["foo"] 0 false)]
                   (expect (= :rstrip (:pass hit)))))
             (it "reports :trim when both sides differ in leading/trailing whitespace"
                 (let [hit (patch/seek-sequence-with-pass ["   foo  "] ["foo"] 0 false)]
                   (expect (= :trim (:pass hit)))))
             (it "reports :unicode when typographic punctuation differs"
                 (let [hit (patch/seek-sequence-with-pass ["it’s late"] ["it's late"] 0 false)]
                   (expect (= :unicode (:pass hit)))))
             (it "reports :relative-indent when only the absolute indentation differs"
                 ;; Same relative structure (2-line block, deeper inner line) but
                 ;; the file's block lives 4 spaces deeper than the SEARCH block.
                 (let [hit (patch/seek-sequence-with-pass ["    def f():" "        return 1"]
                                                          ["def f():" "    return 1"]
                                                          0
                                                          false)]
                   (expect (= :relative-indent (:pass hit)))
                   (expect (= 4 (:indent-delta hit)))))
             (it "returns nil when no pass succeeds"
                 (expect (nil? (patch/seek-sequence-with-pass ["a" "b"] ["c"] 0 false)))))

(defdescribe
  apply-indent-delta-test
  (it "is a no-op when delta is zero" (expect (= ["a" "b"] (patch/apply-indent-delta 0 ["a" "b"]))))
  (it "pads each non-blank line with N leading spaces for positive delta"
      (expect (= ["    a" "" "    b"] (patch/apply-indent-delta 4 ["a" "" "b"]))))
  (it "strips up to N leading whitespace chars for negative delta and leaves blanks alone"
      (expect (= ["a" "" "b"] (patch/apply-indent-delta -4 ["    a" "" "    b"]))))
  (it "never strips more than a line actually has"
      (expect (= ["a" "b"] (patch/apply-indent-delta -10 ["  a" " b"])))))

(defdescribe char-offset-helpers-test
             (it "char-offset-at-line returns 0 for the first line"
                 (expect (= 0 (patch/char-offset-at-line "a\nb\nc\n" 0))))
             (it "char-offset-at-line points past the newline of the previous line"
                 (expect (= 2 (patch/char-offset-at-line "a\nb\nc\n" 1)))
                 (expect (= 4 (patch/char-offset-at-line "a\nb\nc\n" 2))))
             (it "char-offset-at-line clamps to (count content) when past EOF"
                 (expect (= 6 (patch/char-offset-at-line "a\nb\nc\n" 99))))
             (it "split-content-lines drops the trailing empty element from a final newline"
                 (expect (= ["a" "b" "c"] (patch/split-content-lines "a\nb\nc\n")))
                 (expect (= ["a" "b" "c"] (patch/split-content-lines "a\nb\nc")))
                 (expect (= [""] (patch/split-content-lines "\n")))))

(defdescribe
  hashline-layer-test
  (it "line-hash is stable, hash-width hex, whitespace-insensitive"
      (expect (re-matches (re-pattern (str "[0-9a-f]{" patch/hash-width "}"))
                          (patch/line-hash "hello")))
      (expect (= (patch/line-hash "hello") (patch/line-hash "  hello  ")))
      (expect (= (patch/line-hash "x") (patch/line-hash "x"))))
  (it "line-anchor is `<lineno>:<hash>`"
      (expect (= (str 325 ":" (patch/line-hash "hello")) (patch/line-anchor 325 "hello"))))
  (it "lines->anchors maps non-blank tuples to `lineno:hash` anchors"
      (expect (= {1 (patch/line-anchor 1 "a") 2 (patch/line-anchor 2 "b")}
                 (patch/lines->anchors [[1 "a"] [2 "b"]]))))
  (it "lines->anchors omits blanks; duplicate lines stay distinct via line number"
      ;; dup 'x' (lines 1,3) now map to `1:hash`/`3:hash` — the line number is
      ;; the disambiguator, so there is NO `#N` ordinal anymore. blank line 4 is
      ;; omitted; 'y' maps to its own `lineno:hash`.
      (expect
        (= {1 (patch/line-anchor 1 "x") 2 (patch/line-anchor 2 "y") 3 (patch/line-anchor 3 "x")}
           (patch/lines->anchors [[1 "x"] [2 "y"] [3 "x"] [4 "   "]]))))
  (it "line-hash is exactly hash-width hex chars, zero-padded"
      (expect (= (long patch/hash-width) (count (patch/line-hash "anything"))))
      (expect (= (long patch/hash-width) (count (patch/line-hash "")))))
  (it "indices-matching-hash returns every 0-based content match"
      (let [lines ["x" "y" "x"]]
        (expect (= [0 2] (patch/indices-matching-hash lines (patch/line-hash "x"))))
        (expect (= [1] (patch/indices-matching-hash lines (patch/line-hash "y"))))))
  (it "resolve-anchor-edit replaces a single `lineno:hash`-anchored line"
      (let [content "alpha\nbeta\ngamma\n"]
        (expect (= {:new-content "alpha\nBETA\ngamma\n" :applied-line 2}
                   (patch/resolve-anchor-edit content (patch/line-anchor 2 "beta") nil "BETA")))))
  (it "resolve-anchor-edit replaces a from..to `lineno:hash` range"
      (let [content "a\nb\nc\nd\n"]
        (expect (= {:new-content "X\nd\n" :applied-line 1}
                   (patch/resolve-anchor-edit content
                                              (patch/line-anchor 1 "a")
                                              (patch/line-anchor 3 "c")
                                              "X")))))
  (it "empty replace DELETES the whole line(s) — no leftover blank"
      ;; Regression: `replace ""` USED to keep the trailing
      ;; newline outside the span, so a single blank-line delete was a zero-width
      ;; NO-OP and a multi-line delete left one line behind. Empty replace now
      ;; means "remove these physical lines".
      (let [content "a\nb\nc\n"]
        ;; content line: fully removed, not left as a blank
        (expect (= "a\nc\n"
                   (:new-content
                     (patch/resolve-anchor-edit content (patch/line-anchor 2 "b") nil ""))))
        ;; last line at EOF
        (expect (= "a\nb\n"
                   (:new-content
                     (patch/resolve-anchor-edit content (patch/line-anchor 3 "c") nil "")))))
      (let [blanks "x\n\n\n\ny\n"] ; lines 2,3,4 blank
        ;; single blank line: actually deleted (was a no-op)
        (expect (= "x\n\n\ny\n"
                   (:new-content
                     (patch/resolve-anchor-edit blanks (patch/line-anchor 3 "") nil ""))))
        ;; multi-blank span: ALL of them gone (was leaving one behind)
        (expect (= "x\ny\n"
                   (:new-content (patch/resolve-anchor-edit blanks
                                                            (patch/line-anchor 2 "")
                                                            (patch/line-anchor 4 "")
                                                            ""))))))
  (it "duplicate lines are addressable by line number — no ambiguity"
      ;; Duplicated content may have many same-hash neighbors; the line coordinate
      ;; is the locator and the hash verifies that exact line before any ambiguity
      ;; check. This regresses the `99:000`-style failure where the hash alone was
      ;; treated as ambiguous.
      (let [content "x\ny\nx\n"]
        (expect (= "X\ny\nx\n"
                   (:new-content
                     (patch/resolve-anchor-edit content (patch/line-anchor 1 "x") nil "X"))))
        (expect (= "x\ny\nX\n"
                   (:new-content
                     (patch/resolve-anchor-edit content (patch/line-anchor 3 "x") nil "X"))))))
  (it "exact line coordinate wins for repeated blank/brace hashes"
      (let [content
            "{\n\n}\n\n}\n\n}\n"

            blank-anchor
            (patch/line-anchor 4 "")

            brace-anchor
            (patch/line-anchor 5 "}")]

        (expect (= "{\n\n}\nBLANK\n}\n\n}\n"
                   (:new-content (patch/resolve-anchor-edit content blank-anchor nil "BLANK"))))
        (expect (= "{\n\n}\n\nCLOSE\n\n}\n"
                   (:new-content (patch/resolve-anchor-edit content brace-anchor nil "CLOSE"))))))
  (it "ambiguous/stale hash with an explicit line resolves to that LINE (line wins)"
      ;; The model named line 5 but gave a hash (blank) shared by several nearby
      ;; lines and NOT matching line 5's own content. The line locates; a dup hash
      ;; does not make an explicit `lineno:hash` anchor ambiguous.
      (let [content
            "a\n\nb\n\nTARGET\n\nc\n\nd\n"

            ; TARGET at line 5; blanks at 2,4,6,8
            anchor
            (str 5 ":" (patch/line-hash ""))]

        ; WRONG, dup hash for line 5
        (expect (= "a\n\nb\n\nREPL\n\nc\n\nd\n"
                   (:new-content (patch/resolve-anchor-edit content anchor nil "REPL"))))
        (expect (= 5 (:applied-line (patch/resolve-anchor-edit content anchor nil "REPL"))))))
  (it "WRONG-LINE GUARD: a valid hash whose content sits far from the stated line is REFUSED"
      ;; The regression that motivated `lineno:hash`: the model supplies a real,
      ;; unique hash ('target', actually on line 1) but a wrong/stale line number
      ;; far away (100). The old bare-hash scheme applied it at line 1 and
      ;; corrupted the file; now it refuses.
      (let [base
            (mapv #(str "line" %) (range 1 121))

            content
            (str (clojure.string/join "\n" (assoc base 0 "target")) "\n")

            res
            (patch/resolve-anchor-edit content (str 100 ":" (patch/line-hash "target")) nil "X")]

        (expect (= :hashline-misplaced
                   (-> res
                       :error
                       :reason)))
        (expect (= 100
                   (-> res
                       :error
                       :stated-line)))
        (expect (= [1]
                   (-> res
                       :error
                       :found-lines)))
        (expect (nil? (:new-content res)))))
  (it "small drift within tolerance still resolves"
      (let [base
            (mapv #(str "line" %) (range 1 121))

            content
            (str (clojure.string/join "\n" (assoc base 49 "target")) "\n")]

        ; target at line 50
        ;; stated line 55, real line 50 — gap 5 <= tolerance -> applies at 50
        (expect (= 50
                   (:applied-line (patch/resolve-anchor-edit content
                                                             (str 55 ":" (patch/line-hash "target"))
                                                             nil
                                                             "X"))))))
  (it "resolve-anchor-edit reports :hashline-not-found for absent content"
      (expect (= :hashline-not-found
                 (-> (patch/resolve-anchor-edit "a\nb\n" (patch/line-anchor 1 "nope") nil "Z")
                     :error
                     :reason))))
  (it "bare hash (no line number) is REFUSED - hashline requires both coordinates"
      (let [content "alpha\nbeta\ngamma\n"]
        ;; a bare hash (no `lineno:` prefix) no longer resolves by uniqueness
        (expect (= :hashline-malformed
                   (-> (patch/resolve-anchor-edit content (patch/line-hash "beta") nil "BETA")
                       :error
                       :reason)))
        ;; nothing is written: the lineno:hash form is mandatory now
        (expect (nil? (:new-content
                        (patch/resolve-anchor-edit content (patch/line-hash "beta") nil "BETA"))))
        ;; a duplicate-line bare hash is likewise refused as malformed (was :hashline-ambiguous)
        (expect (= :hashline-malformed
                   (-> (patch/resolve-anchor-edit "x\ny\nx\n" (patch/line-hash "x") nil "N")
                       :error
                       :reason)))))
  (it "resolve-anchor-edit refuses an inverted range"
      (let [content
            "a\nb\nc\n"

            res
            (patch/resolve-anchor-edit content
                                       (patch/line-anchor 3 "c")
                                       (patch/line-anchor 1 "a")
                                       "X")]

        (expect (= :hashline-range-inverted
                   (-> res
                       :error
                       :reason))))))
