(ns com.blockether.vis.internal.foundation.editing.patch-test
  "Fuzzy line-matching toolkit covered here. Envelope-mode parser / hunk
   applier were retired together with `patch`'s envelope grammar; only
   the per-line fuzzy passes remain.

   Temp files under `target/editing-test/` stay inside cwd so
   `safe-path` accepts them."
  (:require
   [clojure.string]
   [com.blockether.vis.internal.foundation.editing.patch :as patch]
   [lazytest.core :refer [defdescribe expect it]]))

(defdescribe seek-sequence-fuzzy-test
  (it "exact match wins"
    (expect (= 1 (patch/seek-sequence ["a" "b" "c"] ["b"] 0 false))))

  (it "ignores trailing whitespace"
    (expect (= 0 (patch/seek-sequence ["foo   "] ["foo"] 0 false))))

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

(defdescribe hashline-layer-test
  (it "line-hash is stable, hash-width hex, whitespace-insensitive"
    (expect (re-matches (re-pattern (str "[0-9a-f]{" patch/hash-width "}"))
              (patch/line-hash "hello")))
    (expect (= (patch/line-hash "hello") (patch/line-hash "  hello  ")))
    (expect (= (patch/line-hash "x") (patch/line-hash "x"))))

  (it "line-anchor is `<lineno>:<hash>`"
    (expect (= (str 325 ":" (patch/line-hash "hello")) (patch/line-anchor 325 "hello"))))

  (it "lines->hashes maps non-blank tuples to `lineno:hash` anchors"
    (expect (= {1 (patch/line-anchor 1 "a") 2 (patch/line-anchor 2 "b")}
              (patch/lines->hashes [[1 "a"] [2 "b"]]))))

  (it "lines->hashes omits blanks; duplicate lines stay distinct via line number"
    ;; dup 'x' (lines 1,3) now map to `1:hash`/`3:hash` — the line number is
    ;; the disambiguator, so there is NO `#N` ordinal anymore. blank line 4 is
    ;; omitted; 'y' maps to its own `lineno:hash`.
    (expect (= {1 (patch/line-anchor 1 "x")
                2 (patch/line-anchor 2 "y")
                3 (patch/line-anchor 3 "x")}
              (patch/lines->hashes [[1 "x"] [2 "y"] [3 "x"] [4 "   "]]))))

  (it "render-hashline-block renders a `<lineno>:<hash>│ text` gutter"
    (let [out (patch/render-hashline-block [[7 "alpha"] [8 "beta"]])]
      (expect (= (str "7:" (patch/line-hash "alpha") "│ alpha\n"
                   "8:" (patch/line-hash "beta") "│ beta")
                out))))

  (it "render-hashline-block right-aligns line numbers, blank hash slot for blanks"
    (let [out   (patch/render-hashline-block [[9 "nine"] [10 "ten"] [11 ""]])
          lines (clojure.string/split-lines out)
          blank (apply str (repeat (long patch/hash-width) \space))]
      ;; ln 9 right-aligned to width 2 to line up with 10/11
      (expect (= (str " 9:" (patch/line-hash "nine") "│ nine") (nth lines 0)))
      (expect (= (str "10:" (patch/line-hash "ten") "│ ten") (nth lines 1)))
      ;; blank line keeps its line number, hash slot is spaces, `│` column aligned
      (expect (= (str "11:" blank "│ ") (nth lines 2)))
      (expect (apply = (map #(clojure.string/index-of % "│") lines)))))

  (it "line-hash is exactly hash-width hex chars, zero-padded"
    (expect (= (long patch/hash-width) (count (patch/line-hash "anything"))))
    (expect (= (long patch/hash-width) (count (patch/line-hash "")))))

  (it "render-hashline-range-block headers each window then the gutter"
    (let [out (patch/render-hashline-range-block
                [{:range [2 3] :lines [[2 "b"] [3 "c"]]}
                 {:range [9 9] :lines [[9 "i"]]}])]
      (expect (clojure.string/includes? out "-- range 2-3 --"))
      (expect (clojure.string/includes? out (str "2:" (patch/line-hash "b") "│ b")))
      (expect (clojure.string/includes? out "-- range 9-9 --"))
      (expect (clojure.string/includes? out (str "9:" (patch/line-hash "i") "│ i")))))

  (it "indices-matching-hash returns every 0-based content match"
    (let [lines ["x" "y" "x"]]
      (expect (= [0 2] (patch/indices-matching-hash lines (patch/line-hash "x"))))
      (expect (= [1] (patch/indices-matching-hash lines (patch/line-hash "y"))))))

  (it "resolve-hash-edit replaces a single `lineno:hash`-anchored line"
    (let [content "alpha\nbeta\ngamma\n"]
      (expect (= {:new-content "alpha\nBETA\ngamma\n" :applied-line 2}
                (patch/resolve-hash-edit content (patch/line-anchor 2 "beta") nil "BETA")))))

  (it "resolve-hash-edit replaces a from..to `lineno:hash` range"
    (let [content "a\nb\nc\nd\n"]
      (expect (= {:new-content "X\nd\n" :applied-line 1}
                (patch/resolve-hash-edit content
                  (patch/line-anchor 1 "a") (patch/line-anchor 3 "c") "X")))))

  (it "duplicate lines are addressable by line number — no ambiguity"
    ;; 'x' on lines 1 and 3; the line number picks which one (no `#N` needed).
    (let [content "x\ny\nx\n"]
      (expect (= "X\ny\nx\n"
                (:new-content (patch/resolve-hash-edit content (patch/line-anchor 1 "x") nil "X"))))
      (expect (= "x\ny\nX\n"
                (:new-content (patch/resolve-hash-edit content (patch/line-anchor 3 "x") nil "X"))))))

  (it "WRONG-LINE GUARD: a valid hash whose content sits far from the stated line is REFUSED"
    ;; The regression that motivated `lineno:hash`: the model supplies a real,
    ;; unique hash ('target', actually on line 1) but a wrong/stale line number
    ;; far away (100). The old bare-hash scheme applied it at line 1 and
    ;; corrupted the file; now it refuses.
    (let [base    (mapv #(str "line" %) (range 1 121))
          content (str (clojure.string/join "\n" (assoc base 0 "target")) "\n")
          res     (patch/resolve-hash-edit content
                    (str 100 ":" (patch/line-hash "target")) nil "X")]
      (expect (= :hash-misplaced (-> res :error :reason)))
      (expect (= 100 (-> res :error :stated-line)))
      (expect (= [1] (-> res :error :found-lines)))
      (expect (nil? (:new-content res)))))

  (it "small drift within tolerance still resolves"
    (let [base    (mapv #(str "line" %) (range 1 121))
          content (str (clojure.string/join "\n" (assoc base 49 "target")) "\n")] ; target at line 50
      ;; stated line 55, real line 50 — gap 5 <= tolerance -> applies at 50
      (expect (= 50 (:applied-line (patch/resolve-hash-edit content
                                     (str 55 ":" (patch/line-hash "target")) nil "X"))))))

  (it "resolve-hash-edit reports :hash-not-found for absent content"
    (expect (= :hash-not-found
              (-> (patch/resolve-hash-edit "a\nb\n" (patch/line-anchor 1 "nope") nil "Z")
                :error :reason))))

  (it "legacy bare hash (no line number) still resolves by uniqueness"
    (let [content "alpha\nbeta\ngamma\n"]
      (expect (= 2 (:applied-line (patch/resolve-hash-edit content (patch/line-hash "beta") nil "BETA"))))
      ;; ambiguous bare hash is still refused
      (expect (= :hash-ambiguous
                (-> (patch/resolve-hash-edit "x\ny\nx\n" (patch/line-hash "x") nil "N")
                  :error :reason)))))

  (it "resolve-hash-edit refuses an inverted range"
    (let [content "a\nb\nc\n"
          res (patch/resolve-hash-edit content
                (patch/line-anchor 3 "c") (patch/line-anchor 1 "a") "X")]
      (expect (= :hash-range-inverted (-> res :error :reason))))))
