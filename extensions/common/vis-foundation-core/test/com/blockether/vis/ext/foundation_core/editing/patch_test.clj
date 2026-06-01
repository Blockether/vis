(ns com.blockether.vis.ext.foundation-core.editing.patch-test
  "Fuzzy line-matching toolkit covered here. Envelope-mode parser / hunk
   applier were retired together with `v/patch`'s envelope grammar; only
   the per-line fuzzy passes remain.

   Temp files under `target/editing-test/` stay inside cwd so
   `safe-path` accepts them."
  (:require
   [clojure.string]
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
  (it "line-hash is stable, 6-hex, whitespace-insensitive"
    (expect (re-matches #"[0-9a-f]{6}" (patch/line-hash "hello")))
    (expect (= (patch/line-hash "hello") (patch/line-hash "  hello  ")))
    (expect (= (patch/line-hash "x") (patch/line-hash "x"))))

  (it "lines->hashes maps every tuple's line-number to its content hash"
    (expect (= {1 (patch/line-hash "a") 2 (patch/line-hash "b")}
              (patch/lines->hashes [[1 "a"] [2 "b"]]))))

  (it "render-hashline-block renders a `<hash>| text` gutter, no line numbers"
    (let [out (patch/render-hashline-block [[7 "alpha"] [8 "beta"]])]
      (expect (= (str (patch/line-hash "alpha") "│ alpha\n"
                   (patch/line-hash "beta") "│ beta")
                out))
      ;; line numbers (7/8) are NOT in the gutter
      (expect (not (re-find #"\b7\b" out)))))

  (it "render-hashline-range-block headers each window then the hash gutter"
    (let [out (patch/render-hashline-range-block
                [{:range [2 3] :lines [[2 "b"] [3 "c"]]}
                 {:range [9 9] :lines [[9 "i"]]}])]
      (expect (clojure.string/includes? out "-- range 2-3 --"))
      (expect (clojure.string/includes? out (str (patch/line-hash "b") "│ b")))
      (expect (clojure.string/includes? out "-- range 9-9 --"))
      (expect (clojure.string/includes? out (str (patch/line-hash "i") "│ i")))))

  (it "indices-matching-hash returns every 0-based match"
    (let [lines ["x" "y" "x"]]
      (expect (= [0 2] (patch/indices-matching-hash lines (patch/line-hash "x"))))
      (expect (= [1] (patch/indices-matching-hash lines (patch/line-hash "y"))))))

  (it "resolve-hash-edit replaces a single anchored line against live content"
    (let [content "alpha\nbeta\ngamma\n"
          h (patch/line-hash "beta")]
      (expect (= {:new-content "alpha\nBETA\ngamma\n" :applied-line 2}
                (patch/resolve-hash-edit content h nil "BETA")))))

  (it "resolve-hash-edit replaces a from..to hash range"
    (let [content "a\nb\nc\nd\n"]
      (expect (= {:new-content "X\nd\n" :applied-line 1}
                (patch/resolve-hash-edit content
                  (patch/line-hash "a") (patch/line-hash "c") "X")))))

  (it "resolve-hash-edit refuses a hash that hits >1 identical line"
    (let [content "x\ny\nx\n"
          res (patch/resolve-hash-edit content (patch/line-hash "x") nil "NEW")]
      (expect (= :hash-ambiguous (-> res :error :reason)))
      (expect (= [1 3] (-> res :error :lines)))))

  (it "resolve-hash-edit reports :hash-not-found for an unknown anchor"
    (expect (= :hash-not-found
              (-> (patch/resolve-hash-edit "a\nb\n" "ffffff" nil "Z") :error :reason))))

  (it "resolve-hash-edit refuses an inverted range"
    (let [content "a\nb\nc\n"
          res (patch/resolve-hash-edit content
                (patch/line-hash "c") (patch/line-hash "a") "X")]
      (expect (= :hash-range-inverted (-> res :error :reason))))))
