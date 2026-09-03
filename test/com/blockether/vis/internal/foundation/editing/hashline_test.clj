(ns com.blockether.vis.internal.foundation.editing.hashline-test
  "Tests for the pure hashline layer — the anchor vocabulary `cat` mints, `grep`
   echoes and `patch` spends. No IO here: every case is a string in, a resolution
   or a refusal out."
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as string]
            [com.blockether.vis.internal.foundation.editing.hashline :as hashline]
            [lazytest.core :refer [defdescribe expect it]]))

(def ^:private content "alpha\nbeta\n\ngamma\ndelta\n")

(defn- anchor-of
  [n]
  (hashline/line-anchor n (nth (hashline/split-content-lines content) (dec (long n)))))

(defdescribe
  anchor-token-test
  (it "an anchor is a line number, a colon and exactly three hex chars"
      (doseq [n (range 1 6)]
        (expect (s/valid? :ext.editing.hashline/anchor (anchor-of n)))
        (expect (= n (hashline/anchor->line (anchor-of n))))))
  (it "a blank line hashes to 000 and is still addressable"
      (expect (= "3:000" (anchor-of 3)))
      (expect (s/valid? :ext.editing.hashline/anchor (anchor-of 3))))
  (it "the hash ignores surrounding whitespace, so re-indentation is not a new line"
      (expect (= (hashline/line-hash "  (defn f [] 1)") (hashline/line-hash "(defn f [] 1)  "))))
  (it "a parsed anchor carries both coordinates"
      (expect (s/valid? :ext.editing.hashline/parsed (hashline/parse-anchor "4439:a80")))
      (expect (:malformed (hashline/parse-anchor "a80")))
      ;; A re-quoted anchor is a common serializer mistake and must still parse.
      (expect (= 4439 (:line (hashline/parse-anchor "\"4439:a80\"")))))
  (it "anchor-string? separates an anchor endpoint from a bare line number"
      (expect (hashline/anchor-string? "120:7f2"))
      (expect (not (hashline/anchor-string? "120")))
      (expect (not (hashline/anchor-string? 120))))
  ;; Regression (session fbb1093f): `cat`, `grep` and `patch` all print
  ;; `<line>:<hash>│ <text>` and promise that line goes straight back in
  ;; as an anchor, but the parser took EVERYTHING after the colon as the
  ;; hash — gutter and line text included. No line could ever carry such
  ;; a hash, so every pasted anchor was refused, and the refusal handed
  ;; back the identical anchor it had just refused.
  (it "a whole rendered line IS the anchor it renders"
      (let [rendered (hashline/render-hashline-block
                       [[2 (nth (hashline/split-content-lines content) 1)]])]
        (expect (= (hashline/parse-anchor (anchor-of 2)) (hashline/parse-anchor rendered)))
        (expect (= 2 (hashline/anchor->line rendered)))
        (expect (hashline/anchor-string? rendered))
        (expect (= {:from-line 2 :to-line 2}
                   (hashline/resolve-anchor-range content rendered nil)))))
  (it "an indented grep row resolves, and its UPPERCASE text is not folded in"
      (let [text
            " * Persist SYNCHRONOUSLY before awaiting the plugin"

            shouty
            (str "alpha\n" text "\n")

            row
            (hashline/render-hashline-block [[2 text]] "  ")]

        (expect (= {:from-line 2 :to-line 2} (hashline/resolve-anchor-range shouty row nil)))))
  (it "bare-anchor-string? tells a naked address from a rendered line"
      (expect (hashline/bare-anchor-string? "120:7f2"))
      (expect (hashline/anchor-string? "120:7f2│ alpha"))
      (expect (not (hashline/bare-anchor-string? "120:7f2│ alpha")))))


(defdescribe
  resolve-one-anchor-write-contract-test
  ;; Regression (session 633cdc58): a line number from one line and a nearby hash
  ;; from another were silently recombined, so patch wrote to the hash's line.
  (it "an exact line and hash resolve"
      (expect (= {:from-line 2 :to-line 2}
                 (hashline/resolve-anchor-range content (anchor-of 2) nil))))
  (it "a hash found one line away is still a mismatch"
      (let [shifted
            (str "inserted\n" content)

            r
            (hashline/resolve-anchor-range shifted (anchor-of 2) nil)]

        (expect (= :anchor-mismatch (get-in r [:error :reason])))
        (expect (= (hashline/line-anchor 2 "alpha") (get-in r [:error :current-anchor])))))
  (it "both endpoints of a range must match their exact lines"
      (let [mixed-to
            (str "4:" (hashline/line-hash "delta"))

            r
            (hashline/resolve-anchor-range content (anchor-of 2) mixed-to)]

        (expect (= :anchor-mismatch (get-in r [:error :reason])))
        (expect (= :to (get-in r [:error :which])))
        (expect (= (hashline/line-anchor 4 "gamma") (get-in r [:error :current-anchor])))
        (expect (= (hashline/line-anchor 2 "beta") (get-in r [:error :current-from-anchor])))
        (expect (= (hashline/line-anchor 4 "gamma") (get-in r [:error :current-to-anchor])))))
  (it "a duplicate hash resolves only when it matches the stated line"
      (let [dupes "same\nsame\nsame\n"]
        (expect (= {:from-line 2 :to-line 2}
                   (hashline/resolve-anchor-range dupes (hashline/line-anchor 2 "same") nil)))))
  (it "changed content is a mismatch with the current anchor attached"
      (let [edited
            "alpha\nBETA\n\ngamma\ndelta\n"

            r
            (hashline/resolve-anchor-range edited (anchor-of 2) nil)]

        (expect (= :anchor-mismatch (get-in r [:error :reason])))
        (expect (= (hashline/line-anchor 2 "BETA") (get-in r [:error :current-anchor])))))
  (it "a malformed anchor and an out-of-range line are refused on their own terms"
      (expect (= :anchor-malformed
                 (get-in (hashline/resolve-anchor-range content "a80" nil) [:error :reason])))
      (expect (= :anchor-line-out-of-range
                 (get-in (hashline/resolve-anchor-range content "99:a80" nil) [:error :reason]))))
  (it "an inverted span is refused rather than silently reordered"
      (expect (= :anchor-range-inverted
                 (get-in (hashline/resolve-anchor-range content (anchor-of 4) (anchor-of 2))
                         [:error :reason])))))


(defdescribe read-tolerant-resolution-test
             ;; A READ is non-destructive, so a stale hash must not block the look the way
             ;; it (correctly) blocks a write.
             (it "a nearby moved hash follows its content for a read"
                 (let [shifted
                       (str "inserted\n" content)

                       r
                       (hashline/resolve-anchor-range-read shifted (anchor-of 2) nil)]

                   (expect (= 3 (:from-line r)))
                   (expect (not (:stale? r)))))
             (it "a stale hash falls back to its line number and says it was stale"
                 (let [edited
                       "alpha\nBETA\n\ngamma\ndelta\n"

                       r
                       (hashline/resolve-anchor-range-read edited (anchor-of 2) nil)]

                   (expect (= 2 (:from-line r)))
                   (expect (:stale? r))))
             (it "an anchor that cannot be located at all still refuses"
                 (expect (= :anchor-malformed
                            (get-in (hashline/resolve-anchor-range-read content "a80" nil)
                                    [:error :reason]))))
             (it "the window never inverts on a read"
                 (let [r (hashline/resolve-anchor-range-read content (anchor-of 4) (anchor-of 2))]
                   (expect (= 2 (:from-line r)))
                   (expect (= 4 (:to-line r))))))


(defdescribe
  edit-span-newline-semantics-test
  (it "a replacement need not end in a newline — the terminator is preserved"
      (let [span
            (hashline/resolve-anchor-edit-span content (anchor-of 2) nil "BETA")

            updated
            (str (subs content 0 (:start span)) (:replacement span) (subs content (:end span)))]

        (expect (= "alpha\nBETA\n\ngamma\ndelta\n" updated))))
  (it "an empty replacement consumes the line, leaving no blank behind"
      (let [span
            (hashline/resolve-anchor-edit-span content (anchor-of 2) nil "")

            updated
            (str (subs content 0 (:start span)) (:replacement span) (subs content (:end span)))]

        (expect (= "alpha\n\ngamma\ndelta\n" updated))))
  (it "a CRLF file keeps its CRLF on a last-line replace"
      (let [crlf
            "one\r\ntwo\r\n"

            span
            (hashline/resolve-anchor-edit-span crlf (hashline/line-anchor 2 "two") nil "TWO")

            updated
            (str (subs crlf 0 (:start span)) (:replacement span) (subs crlf (:end span)))]

        (expect (= "one\r\nTWO\r\n" updated))))
  (it "a multi-line span replace reports the lines it resolved"
      (let [span (hashline/resolve-anchor-edit-span content (anchor-of 2) (anchor-of 4) "X\nY")]
        (expect (= 2 (:from-line span)))
        (expect (= 4 (:to-line span)))))
  ;; Regression: a span whose LAST line was BLANK grew one extra blank line on every
  ;; replace — the span's own terminator sits OUTSIDE it, so the check read the
  ;; PREVIOUS line's `\n` and padded the replacement with a newline it must not carry.
  (it "a span ending on a blank line replaces it instead of growing another"
      (let [span
            (hashline/resolve-anchor-edit-span content (anchor-of 2) (anchor-of 3) "BETA")

            updated
            (str (subs content 0 (:start span)) (:replacement span) (subs content (:end span)))]

        (expect (= "alpha\nBETA\ngamma\ndelta\n" updated))))
  (it "a span ending on the file's last line keeps the file's final newline"
      (let [span
            (hashline/resolve-anchor-edit-span content (anchor-of 4) (anchor-of 5) "TAIL")

            updated
            (str (subs content 0 (:start span)) (:replacement span) (subs content (:end span)))]

        (expect (= "alpha\nbeta\n\nTAIL\n" updated)))))


(defdescribe
  render-hashline-block-test
  (it "renders one addressable row per tuple, indent included"
      (let [lines
            (hashline/split-content-lines content)

            tuples
            (map-indexed (fn [i l]
                           [(inc i) l])
                         lines)

            block
            (hashline/render-hashline-block tuples)

            indented
            (hashline/render-hashline-block tuples "  ")]

        (expect (= 5 (count (string/split-lines block))))
        (expect (every? #(re-matches #"\d+:[0-9a-f]{3}│ .*" %) (string/split-lines block)))
        (expect (every? #(string/starts-with? % "  ") (string/split-lines indented)))
        ;; The gutter never occurs in source, so splitting on it is exact.
        (expect (= "beta" (second (string/split (second (string/split-lines block)) #"│ " 2)))))))
;; Regression: a CRLF file's rendered lines kept their `\r`, so the text a model
;; sliced off `cat` output ended in a carriage return and writing it back grew a
;; SECOND one. The hash never saw it — `line-hash` trims — so the anchor is the
;; same either way; only the rendering was wrong.
(defdescribe render-drops-the-carriage-return-test
             (it "a CRLF line renders without its CR, under the same anchor"
                 (let [lines
                       (hashline/split-content-lines "alpha\r\nbeta\r\ngamma\r\n")

                       block
                       (hashline/render-hashline-block (map-indexed (fn [i l]
                                                                      [(inc i) l])
                                                                    lines))

                       rendered
                       (string/split-lines block)]

                   ;; The content the file really carries still has the CR: the char offsets
                   ;; an edit splices at count it.
                   (expect (= ["alpha\r" "beta\r" "gamma\r"] lines))
                   (expect (not-any? #(string/includes? % "\r") rendered))
                   (expect (= (hashline/line-anchor 2 "beta") (hashline/line-anchor 2 "beta\r")))
                   (expect (= (str (hashline/line-anchor 2 "beta") "│ beta") (second rendered))))))
