(ns com.blockether.vis.ext.foundation-core.editing.patch-test
  "Fuzzy line-matching toolkit covered here. Envelope-mode parser / hunk
   applier were retired together with `v/patch`'s envelope grammar; only
   the per-line fuzzy passes remain.

   Temp files under `target/editing-test/` stay inside cwd so
   `safe-path` accepts them."
  (:require
   [com.blockether.vis.ext.foundation-core.editing.patch :as patch]
   [lazytest.core :refer [defdescribe expect it]]))

(defdescribe seek-sequence-fuzzy-test
  (it "exact match wins"
    (expect (= 1 (patch/seek-sequence ["a" "b" "c"] ["b"] 0 false))))

  (it "ignores trailing whitespace"
    (expect (= 0 (patch/seek-sequence ["foo   "] ["foo"] 0 false))))

  (it "ignores leading+trailing whitespace"
    (expect (= 0 (patch/seek-sequence ["   foo  "] ["foo"] 0 false))))

  (it "normalizes typographic punctuation"
    (expect (= 0 (patch/seek-sequence ["it\u2019s"] ["it's"] 0 false)))
    (expect (= 0 (patch/seek-sequence ["a\u2014b"] ["a-b"] 0 false))))

  (it "returns nil when pattern not present"
    (expect (nil? (patch/seek-sequence ["a" "b"] ["c"] 0 false))))

  (it "returns nil when pattern longer than input (no panic)"
    (expect (nil? (patch/seek-sequence ["only"] ["too" "long"] 0 false))))

  (it "prefers EOF position when eof? is true"
    (expect (= 2 (patch/seek-sequence ["x" "y" "x"] ["x"] 0 true)))))

(defdescribe seek-sequence-with-pass-test
  ;; seek-sequence-with-pass is what powers the line-based fuzzy fallback
  ;; in v/patch exact-replace mode. It MUST report which fuzzy strategy
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
    (let [hit (patch/seek-sequence-with-pass ["it\u2019s late"] ["it's late"] 0 false)]
      (expect (= :unicode (:pass hit)))))

  (it "reports :relative-indent when only the absolute indentation differs"
    ;; Same relative structure (2-line block, deeper inner line) but
    ;; the file's block lives 4 spaces deeper than the SEARCH block.
    (let [hit (patch/seek-sequence-with-pass
                ["    def f():" "        return 1"]
                ["def f():" "    return 1"]
                0 false)]
      (expect (= :relative-indent (:pass hit)))
      (expect (= 4 (:indent-delta hit)))))

  (it "returns nil when no pass succeeds"
    (expect (nil? (patch/seek-sequence-with-pass ["a" "b"] ["c"] 0 false)))))

(defdescribe apply-indent-delta-test
  (it "is a no-op when delta is zero"
    (expect (= ["a" "b"] (patch/apply-indent-delta 0 ["a" "b"]))))

  (it "pads each non-blank line with N leading spaces for positive delta"
    (expect (= ["    a" "" "    b"]
              (patch/apply-indent-delta 4 ["a" "" "b"]))))

  (it "strips up to N leading whitespace chars for negative delta and leaves blanks alone"
    (expect (= ["a" "" "b"]
              (patch/apply-indent-delta -4 ["    a" "" "    b"]))))

  (it "never strips more than a line actually has"
    (expect (= ["a" "b"]
              (patch/apply-indent-delta -10 ["  a" " b"])))))

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
