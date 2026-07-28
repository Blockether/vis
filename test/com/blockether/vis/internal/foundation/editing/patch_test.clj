(ns com.blockether.vis.internal.foundation.editing.patch-test
  "Pure hashline primitives used by the anchor-only patch tool."
  (:require [clojure.string]
            [com.blockether.vis.internal.foundation.editing.patch :as patch]
            [lazytest.core :refer [defdescribe expect it]]))

(defn- resolve-span-output
  "Resolve an edit span, then splice it so tests cover both offsets and content."
  [content from-anchor to-anchor replacement]
  (let [span (patch/resolve-anchor-edit-span content from-anchor to-anchor replacement)]
    (if (:error span)
      span
      {:content (str (subs content 0 (:start span)) (:replacement span) (subs content (:end span)))
       :applied-line (:applied-line span)})))

(defdescribe char-offset-helpers-test
             (it "maps line indices to character offsets"
                 (expect (= 0 (patch/char-offset-at-line "a\nb\nc\n" 0)))
                 (expect (= 2 (patch/char-offset-at-line "a\nb\nc\n" 1)))
                 (expect (= 6 (patch/char-offset-at-line "a\nb\nc\n" 99))))
             (it "splits content without a trailing newline element"
                 (expect (= ["a" "b" "c"] (patch/split-content-lines "a\nb\nc\n")))
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
  (it "anchor values require the canonical text map"
      (expect (= "x" (patch/anchor-value-text {"text" "x"})))
      (expect (= :ext.foundation.editing/invalid-anchor-value
                 (try (patch/anchor-value-text "x")
                      nil
                      (catch clojure.lang.ExceptionInfo e (:type (ex-data e)))))))
  (it "resolve-anchor-edit-span replaces a single `lineno:hash`-anchored line"
      (let [content "alpha\nbeta\ngamma\n"]
        (expect (= {:content "alpha\nBETA\ngamma\n" :applied-line 2}
                   (resolve-span-output content (patch/line-anchor 2 "beta") nil "BETA")))))
  (it
    "resolve-anchor-edit-span replaces a from..to `lineno:hash` range"
    (let [content "a\nb\nc\nd\n"]
      (expect
        (= {:content "X\nd\n" :applied-line 1}
           (resolve-span-output content (patch/line-anchor 1 "a") (patch/line-anchor 3 "c") "X")))))
  (it "empty replace DELETES the whole line(s) — no leftover blank"
      ;; Empty replacement removes the complete physical line span.
      (let [content "a\nb\nc\n"]
        ;; content line: fully removed, not left as a blank
        (expect (= "a\nc\n"
                   (:content (resolve-span-output content (patch/line-anchor 2 "b") nil ""))))
        ;; last line at EOF
        (expect (= "a\nb\n"
                   (:content (resolve-span-output content (patch/line-anchor 3 "c") nil "")))))
      (let [blanks "x\n\n\n\ny\n"] ; lines 2,3,4 blank
        ;; single blank line
        (expect (= "x\n\n\ny\n"
                   (:content (resolve-span-output blanks (patch/line-anchor 3 "") nil ""))))
        ;; multi-blank span
        (expect (= "x\ny\n"
                   (:content (resolve-span-output blanks
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
                   (:content (resolve-span-output content (patch/line-anchor 1 "x") nil "X"))))
        (expect (= "x\ny\nX\n"
                   (:content (resolve-span-output content (patch/line-anchor 3 "x") nil "X"))))))
  (it "exact line coordinate wins for repeated blank/brace hashes"
      (let
        [content
         "{\n\n}\n\n}\n\n}\n"

         blank-anchor
         (patch/line-anchor 4 "")

         brace-anchor
         (patch/line-anchor 5 "}")]

        (expect (= "{\n\n}\nBLANK\n}\n\n}\n"
                   (:content (resolve-span-output content blank-anchor nil "BLANK"))))
        (expect (= "{\n\n}\n\nCLOSE\n\n}\n"
                   (:content (resolve-span-output content brace-anchor nil "CLOSE"))))))
  (it "ambiguous/stale hash with an explicit line resolves to that LINE (line wins)"
      ;; The model named line 5 but gave a hash (blank) shared by several nearby
      ;; lines and NOT matching line 5's own content. The line locates; a dup hash
      ;; does not make an explicit `lineno:hash` anchor ambiguous.
      (let
        [content
         "a\n\nb\n\nTARGET\n\nc\n\nd\n"

         ; TARGET at line 5; blanks at 2,4,6,8
         anchor
         (str 5 ":" (patch/line-hash ""))]

        ; WRONG, dup hash for line 5
        (expect (= "a\n\nb\n\nREPL\n\nc\n\nd\n"
                   (:content (resolve-span-output content anchor nil "REPL"))))
        (expect (= 5 (:applied-line (resolve-span-output content anchor nil "REPL"))))))
  (it "WRONG-LINE GUARD: a valid hash whose content sits far from the stated line is REFUSED"
      ;; A real hash paired with a far-away line number must not relocate the edit.
      (let
        [base
         (mapv #(str "line" %) (range 1 121))

         content
         (str (clojure.string/join "\n" (assoc base 0 "target")) "\n")

         res
         (resolve-span-output content (str 100 ":" (patch/line-hash "target")) nil "X")]

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
        (expect (nil? (:content res)))))
  (it "small drift within tolerance still resolves"
      (let
        [base
         (mapv #(str "line" %) (range 1 121))

         content
         (str (clojure.string/join "\n" (assoc base 49 "target")) "\n")]

        ; target at line 50
        ;; stated line 55, real line 50 — gap 5 <= tolerance -> applies at 50
        (expect
          (= 50
             (:applied-line
               (resolve-span-output content (str 55 ":" (patch/line-hash "target")) nil "X"))))))
  (it "resolve-anchor-edit-span reports :hashline-not-found for absent content"
      (expect (= :hashline-not-found
                 (-> (resolve-span-output "a\nb\n" (patch/line-anchor 1 "nope") nil "Z")
                     :error
                     :reason))))
  (it "bare hash (no line number) is REFUSED - hashline requires both coordinates"
      (let [content "alpha\nbeta\ngamma\n"]
        ;; Both coordinates are mandatory.
        (expect (= :hashline-malformed
                   (-> (resolve-span-output content (patch/line-hash "beta") nil "BETA")
                       :error
                       :reason)))
        ;; No span is produced.
        (expect (nil? (:content (resolve-span-output content (patch/line-hash "beta") nil "BETA"))))
        ;; Duplicate content does not weaken the shape requirement.
        (expect (= :hashline-malformed
                   (-> (resolve-span-output "x\ny\nx\n" (patch/line-hash "x") nil "N")
                       :error
                       :reason)))))
  (it "resolve-anchor-edit-span refuses an inverted range"
      (let
        [content
         "a\nb\nc\n"

         res
         (resolve-span-output content (patch/line-anchor 3 "c") (patch/line-anchor 1 "a") "X")]

        (expect (= :hashline-range-inverted
                   (-> res
                       :error
                       :reason))))))
