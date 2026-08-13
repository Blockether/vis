(ns com.blockether.vis.internal.foundation.editing.hashline
  "Pure hashline primitives: the ANCHOR vocabulary `cat` mints, `grep` echoes and
   `patch` spends.

   An anchor is `<1-based line>:<3-hex content hash>` (Can Bölük's original
   hashline shape). The LINE NUMBER locates the line; the CONTENT HASH verifies
   it. Two coordinates, so a stale or reused anchor can no longer silently land
   an edit on the wrong line: when the content sits far from the stated line the
   write is REFUSED (`:anchor-misplaced`) instead of corrupting the file.

   This namespace is pure — no IO, no tool wiring, no extension envelope. Every
   surface that addresses a line routes here so the scheme is never recomputed:

     split-content-lines / char-offset-at-line   blob <-> line/char coordinates
     line-hash / line-anchor / anchor->line      text  -> `<line>:<hash>`
     render-hashline-block                       [[ln text]…] -> gutter text
     indices-matching-hash                       content-only hash lookup
     resolve-one-anchor / resolve-anchor-range   anchor -> live line, or refusal
     resolve-anchor-range-read                   the READ-tolerant twin
     resolve-anchor-edit-span                    anchor span -> char span"
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]))

(set! *warn-on-reflection* true)

;; =============================================================================
;; The anchor token — the ONE string that crosses Clojure -> GraalPy -> the model
;; =============================================================================

(def hash-width
  "Hex chars in a line's content hash. The line number locates; this hash
   verifies within `hash-line-drift-tolerance`. Three hex chars keep an anchor
   at 4-7 characters while the line coordinate disambiguates collisions."
  3)

(def ^:const hashline-anchor-sep
  "Separator between the line number and the content hash inside an anchor
   (`<line>:<hash>`). A single char so the gutter stays narrow."
  ":")

(def ^:const hashline-gutter
  "Separator between the anchor and the line text in every rendered block.
   U+2502 plus a space: it never occurs in source, so `line.split(\"│ \", 1)` is
   exact and can never be confused with the `:` inside the anchor."
  "│ ")

(s/def :ext.editing.hashline/hash (s/and string? #(re-matches #"[0-9a-f]{3}" %)))
(s/def :ext.editing.hashline/line pos-int?)
;; `<1-based line>:<hash>` — the ONLY parseable anchor form. `cat` mints it,
;; `grep` echoes it, `patch` consumes it, and nothing else in the tree parses one.
(s/def :ext.editing.hashline/anchor (s/and string? #(re-matches #"\d+:[0-9a-f]{3}" %)))
(s/def :ext.editing.hashline/parsed
  (s/keys :req-un [:ext.editing.hashline/line :ext.editing.hashline/hash]))

;; =============================================================================
;; Blob <-> coordinates
;; =============================================================================

(defn split-content-lines
  "Split a file blob into a vec of lines. A trailing empty element (from the
   file's final newline) is dropped, so the vector's count IS the file's line
   count and index 0 is line 1."
  [^String s]
  (let
    [arr
     (.split s "\n" -1)

     v
     (vec arr)]

    (if (and (pos? (count v)) (= "" (peek v))) (pop v) v)))

(defn char-offset-at-line
  "Char offset in `content` where 0-based line `line-idx` starts. Returns
   `(count content)` when `line-idx` reaches past the last line. Public so the
   edit-span planner can map line indices back to char positions for splicing."
  ^long [^String content ^long line-idx]
  (loop
    [pos
     0

     i
     0]

    (if (= i line-idx)
      pos
      (let [nl (str/index-of content "\n" pos)]
        (if nl (recur (inc (long nl)) (inc i)) (count content))))))

;; =============================================================================
;; Minting an anchor
;; =============================================================================

(def ^:private hash-mask
  "Low `hash-width` hex digits as a bit mask: (16^hash-width) - 1."
  (long (dec (bit-shift-left 1 (* 4 (long hash-width))))))

(def ^:private hash-zero-pad
  "`hash-width` zero chars, for left-padding a short `Integer/toHexString`."
  (apply str (repeat (long hash-width) \0)))

(defn line-hash
  "Stable `hash-width`-hex-char content hash of `line` (trimmed). Folds the
   spec'd `String/hashCode` algorithm over the whitespace-trimmed line, so it is
   deterministic across JVM runs; the line coordinate disambiguates identical
   hashes, and a blank line hashes to `000`.

   Hot path: it runs once per rendered line on every `cat`, every `grep` hit and
   every patch resolve. Formats with `Integer/toHexString` plus a left-pad
   rather than java.util.Formatter (~1.5x slower), and leans on the trimmed
   `String/hashCode` because that is a JIT intrinsic."
  ^String [line]
  (let
    [h
     (int (bit-and (.hashCode (str/trim (str line))) (long hash-mask)))

     hex
     (Integer/toHexString h)

     c
     (.length hex)]

    (if (< c (long hash-width)) (str (subs hash-zero-pad c) hex) hex)))

(defn line-anchor
  "The editable anchor for a line: `<line-number>:<content-hash>` (e.g.
   `4439:a80`). The line number LOCATES the line, the hash VERIFIES its content.
   `patch` parses this back through `resolve-anchor-range` and refuses when the
   hash no longer agrees (the line changed) or when that content now lives far
   from the stated line (a stale / misattributed anchor)."
  [ln text]
  (str ln hashline-anchor-sep (line-hash text)))

(defn anchor->line
  "Parse the 1-based line number out of a `<line>:<hash>` anchor."
  ^long [anchor]
  (let
    [s
     (str anchor)

     i
     (str/index-of s hashline-anchor-sep)]

    (Long/parseLong (subs s 0 (long i)))))

(defn anchor-string?
  "True when `x` is a well-formed `<line>:<hash>` anchor STRING. `cat` uses it to
   tell an anchor endpoint from a bare line number; anything else is a number."
  [x]
  (boolean (and (string? x) (re-matches #"\s*['\"`]*\d+:[0-9a-fA-F]+['\"`]*\s*" x))))

(defn render-hashline-block
  "Render `[[line-number text]…]` tuples as the MODEL's addressable gutter —
   `<line>:<hash>│ <text>`, one line per tuple, `indent` prefixed to each. This
   is the single renderer behind `cat`, `grep`'s hit rows and `patch`'s
   re-anchored window, so all three speak one format and any of their lines can
   be split on `hashline-gutter` and fed straight back to `patch`.

   A CRLF file's lines still carry their `\\r`: `split-content-lines` splits on
   `\\n` alone because the char offsets an edit splices at must count every
   character the file really has. RENDERING drops that trailing CR — it is
   invisible on screen, `line-hash` never saw it (the hash is of the TRIMMED
   line, so the anchor is unchanged), and a model that copies a rendered line
   back as a replacement would otherwise write a SECOND carriage return in."
  (^String [tuples] (render-hashline-block tuples ""))
  (^String [tuples ^String indent]
   (->> tuples
        (map (fn [[ln s]]
               (let
                 [^String s
                  (str s)

                  ^String s
                  (if (str/ends-with? s "\r") (subs s 0 (dec (.length s))) s)]

                 (str indent (line-anchor ln s) hashline-gutter s))))
        (str/join "\n"))))

;; =============================================================================
;; Resolving an anchor against LIVE content
;; =============================================================================

(defn indices-matching-hash
  "0-based indices of `lines` whose `line-hash` equals the bare hash `h`. Pure
   content match — the line-number coordinate is applied separately by
   `resolve-one-anchor`, so this only answers \"which lines carry this content\"."
  [lines h]
  (let [h (str h)]
    (into []
          (keep-indexed (fn [i l]
                          (when (= h (line-hash l)) i)))
          lines)))

(def hash-line-drift-tolerance
  "How far (in lines) a content hash may sit from its stated line number before
   `resolve-one-anchor` calls the anchor MISPLACED and refuses. The common path
   never needs it: an anchor spent right after the `cat`/`grep` that minted it
   resolves exactly. This window only forgives small drift when anchors are
   reused across edits without re-reading; anything larger — the ~200-line gap
   of a genuinely wrong or stale anchor, the corruption this whole scheme exists
   to stop — is refused so the caller re-reads. Deliberately tight: a false
   refuse costs one re-read, a false accept corrupts the file."
  40)

(defn- unwrap-anchor
  "Tolerate an anchor wrapped in stray whitespace or quote chars — a common
   JSON/LLM mistake where the `line:hash` string is re-quoted, so `\"4439:a80\"`
   arrives WITH the literal quote characters and `parse-long` chokes on the
   leading quote. A real anchor is only digits, a colon and hex, so trimming
   surrounding whitespace and matching quotes can never corrupt a valid one."
  ^String [^String s]
  (-> s
      str/trim
      (str/replace #"^['\"`]+" "")
      (str/replace #"['\"`]+$" "")
      str/trim))

(defn parse-anchor
  "Parse a `<line>:<hash>` anchor into `{:line L :hash H}` (L a 1-based long, H
   the hex content hash). The line number is REQUIRED: an anchor with no `:`
   separator, or a non-numeric line part, parses to `{:malformed true :raw S}`
   and `resolve-one-anchor` refuses it (`:anchor-malformed`). Every anchor must
   carry BOTH coordinates so the line LOCATES and the hash VERIFIES."
  [anchor]
  (let
    [s
     (unwrap-anchor (str anchor))

     i
     (.indexOf s (int \:))

     line
     (when-not (neg? i) (parse-long (subs s 0 i)))]

    (if (and (not (neg? i)) line)
      {:line line :hash (str/lower-case (subs s (inc i)))}
      {:malformed true :raw s})))

(defn resolve-one-anchor
  "Resolve a single PARSED `{:line :hash}` anchor to a 0-based index in `lines`,
   or `{:error {:reason KW …}}`. The LINE locates; the hash VERIFIES — but a
   NON-UNIQUE hash never blocks a well-located edit:
     1. exact     - the stated line still hashes to `hash`            -> use it.
     2. drifted   - the line moved a little and `hash` sits at EXACTLY one line
                    within `hash-line-drift-tolerance`                -> follow it.
     3. line wins - `hash` is AMBIGUOUS (several lines, at least one near the
                    stated line): the hash cannot choose, but the caller named an
                    EXPLICIT line, so use it. Duplicate hashes do NOT make a
                    `line:hash` anchor ambiguous.
     4. misplaced - `hash` matches only line(s) FAR from the stated line: a
                    strong line-vs-content contradiction -> REFUSE. This is the
                    WRONG-LINE guard that stops an edit landing off target.
     5. not-found - `hash` matches no live line (the content is gone) -> REFUSE,
                    handing back the anchor that IS at the stated line so the
                    caller recovers in ONE call instead of a second read.
   Plus: a malformed anchor (no `<line>:` prefix) or a line outside the file is
   refused — those genuinely cannot be located."
  [lines which {:keys [line hash malformed raw]}]
  (if malformed
    {:error {:reason :anchor-malformed :which which :anchor raw}}
    (let
      [idx0
       (dec (long line))

       n
       (long (count lines))]

      (cond (or (neg? idx0) (>= idx0 n))
            {:error {:reason :anchor-line-out-of-range :which which :line line :lines n}}
            ;; 1. exact — content at the stated line verifies the hash
            (= hash (line-hash (nth lines idx0))) {:index idx0}
            :else (let [matches (indices-matching-hash lines hash)]
                    (if (empty? matches)
                      ;; 5. the content is gone — refuse and re-read, but carry the CURRENT
                      ;;    anchor at the stated line so the common stale-after-edit case
                      ;;    recovers in ONE step instead of a separate read.
                      {:error {:reason :anchor-not-found
                               :which which
                               :hash hash
                               :stated-line line
                               :current-anchor (line-anchor line (nth lines idx0))
                               :current-text (nth lines idx0)}}
                      (let
                        [tol (long hash-line-drift-tolerance)
                         in-win (filterv (fn [i]
                                           (<= (Math/abs (- (inc (long i)) (long line))) tol))
                                  matches)]

                        (cond
                          ;; 2. drifted — one nearby match, follow the content
                          (= 1 (long (count in-win))) {:index (first in-win)}
                          ;; 4. the hash matches only FAR from the stated line — WRONG-LINE guard.
                          ;;    Refuse (three hex chars make "unique in file" weak evidence), but
                          ;;    when it now sits at EXACTLY ONE line the correct anchor is that
                          ;;    line plus this same hash — hand it back for a one-step recovery.
                          (empty? in-win) {:error (cond->
                                                    {:reason :anchor-misplaced
                                                     :which which
                                                     :hash hash
                                                     :stated-line line
                                                     :found-lines (mapv #(inc (long %)) matches)}
                                                    (= 1 (count matches))
                                                    (assoc :current-anchor
                                                      (line-anchor (inc (long (first matches)))
                                                                   (nth lines
                                                                        (long (first matches))))))}
                          ;; 3. several nearby matches — the hash cannot disambiguate, so the
                          ;;    explicit line wins (the duplicate-line case)
                          :else {:index idx0}))))))))

(defn resolve-anchor-range
  "Resolve `from-anchor` (and `to-anchor`, defaulting to `from-anchor` for a
   single line) against LIVE `current`. Each is a `<line>:<hash>` anchor: the
   line number LOCATES it, the hash VERIFIES the content still matches AND still
   sits near the stated line (else `:anchor-misplaced` — the wrong-line guard).
   BOTH coordinates are required; a bare hash with no line number is refused
   (`:anchor-malformed`). Returns `{:from-line N :to-line N}` (1-based,
   INCLUSIVE) or `{:error {:reason KW …}}`.

   The WRITE side of the contract. `resolve-anchor-range-read` is its tolerant
   twin, so a read and a write address lines identically but only the write
   refuses."
  [^String current from-anchor to-anchor]
  (let
    [lines
     (split-content-lines current)

     from-a
     (parse-anchor from-anchor)

     to-a
     (if (or (nil? to-anchor) (= (str to-anchor) (str from-anchor)))
       from-a
       (parse-anchor to-anchor))

     fr
     (resolve-one-anchor lines :from from-a)]

    (if (:error fr)
      fr
      (let [tr (if (identical? from-a to-a) fr (resolve-one-anchor lines :to to-a))]
        (if (:error tr)
          tr
          (let
            [fi (long (:index fr))
             ti (long (:index tr))]

            (if (< ti fi)
              {:error {:reason :anchor-range-inverted :from-line (inc fi) :to-line (inc ti)}}
              {:from-line (inc fi) :to-line (inc ti)})))))))

(defn resolve-anchor-range-read
  "READ-tolerant twin of `resolve-anchor-range`, for `cat`. A read is
   NON-DESTRUCTIVE, so a stale hash must not block the look the way it
   (correctly) blocks a write. Each anchor still resolves by CONTENT first —
   following small drift exactly like the write path — but when its hash matches
   no live line the anchor's LINE NUMBER is the fallback: a read can safely show
   whatever now sits there. Returns `{:from-line N :to-line N :stale? BOOL}`
   (1-based, INCLUSIVE; the window never inverts) — or `{:error …}` ONLY for a
   genuinely unlocatable anchor (`:anchor-malformed`, no line number, or
   `:anchor-line-out-of-range`, a line outside the file)."
  [^String current from-anchor to-anchor]
  (let
    [lines
     (split-content-lines current)

     n
     (long (count lines))

     resolve-read
     (fn [which anchor]
       (let
         [a
          (parse-anchor anchor)

          r
          (resolve-one-anchor lines which a)]

         (cond (:index r) (assoc r :stale? false)
               ;; The hash is gone (the content changed) or matches only far lines,
               ;; but the anchor still names an in-range line — show that line.
               (and (:line a)
                    (contains? #{:anchor-not-found :anchor-misplaced} (get-in r [:error :reason]))
                    (<= 1 (long (:line a)) n))
               {:index (dec (long (:line a))) :stale? true}
               :else r)))

     fr
     (resolve-read :from from-anchor)]

    (if (:error fr)
      fr
      (let
        [tr (if (or (nil? to-anchor) (= (str to-anchor) (str from-anchor)))
              fr
              (resolve-read :to to-anchor))]
        (if (:error tr)
          tr
          (let
            [fi (long (:index fr))
             ti (long (:index tr))]

            {:from-line (inc (min fi ti))
             :to-line (inc (max fi ti))
             :stale? (boolean (or (:stale? fr) (:stale? tr)))}))))))

(defn- line-span->char-span
  "Convert a 0-based [line-start line-end) span to a [char-start char-end]
   substring span in `content`, keeping the region's trailing terminator
   OUTSIDE the replaced region."
  [^String content ^long line-start ^long line-end]
  (let
    [char-start
     (char-offset-at-line content line-start)

     char-end-raw
     (char-offset-at-line content line-end)

     char-end
     (let
       [e (long (if (and (< char-end-raw (count content))
                         (pos? char-end-raw)
                         (= \newline (.charAt content (dec char-end-raw))))
                  (dec char-end-raw)
                  char-end-raw))]
       ;; CRLF: the terminator is TWO chars. Dropping only the `\n` leaves the
       ;; `\r` INSIDE the replaced region, so an ordinary line replace silently
       ;; rewrites that one line's ending to bare LF and mixes endings in a CRLF
       ;; file. Keep the whole `\r\n` outside the span.
       (if (and (> e char-start) (= \return (.charAt content (dec e)))) (dec e) e))]

    [char-start char-end]))

(defn resolve-anchor-edit-span
  "Resolve an anchored line range to a CHAR SPAN against `current`, WITHOUT
   building new content: `{:start S :end E :replacement R :from-line N :to-line N}`
   or `{:error {:reason KW …}}`. `to-anchor` defaults to `from-anchor` (a single
   line). The stated line is tried first and only then small drift, so duplicate
   hashes elsewhere never make an exact `line:hash` anchor ambiguous.

   Newline semantics: a replacement need NOT end in `\\n` — the matched region's
   terminator is preserved (`\\r\\n` stays `\\r\\n` on a CRLF file) — and an EMPTY
   replacement consumes the trailing newline, so the lines actually vanish
   instead of leaving blanks behind."
  [^String current from-anchor to-anchor ^String replacement]
  (let [res (resolve-anchor-range current from-anchor to-anchor)]
    (if (:error res)
      res
      (let
        [line-start (dec (long (:from-line res)))
         line-end (long (:to-line res))]

        (if (= "" (str replacement))
          ;; DELETION: take the WHOLE physical line(s) INCLUDING the trailing
          ;; newline so the lines are actually removed. The `line-span->char-span`
          ;; rule below deliberately keeps a matched region's trailing `\n` OUTSIDE
          ;; the span (so a REPLACE never doubles the newline) — but for an empty
          ;; replacement that rule makes a single blank-line delete a zero-width
          ;; no-op and a multi-line delete leave one line behind.
          (let
            [char-start (char-offset-at-line current line-start)
             char-end (char-offset-at-line current line-end)]

            {:start char-start
             :end char-end
             :replacement ""
             :from-line (:from-line res)
             :to-line (:to-line res)})
          (let
            [[char-start char-end] (line-span->char-span current line-start line-end)
             ;; Only a NON-EMPTY span can end in a newline. An empty-line span is
             ;; zero-width (char-start == char-end); without this guard the check
             ;; reads the PREVIOUS line's `\n` and wrongly pads the replacement,
             ;; inserting instead of replacing.
             matched-ends-nl? (and (< (long char-start) (long char-end))
                                   (= \newline (.charAt current (dec (long char-end)))))
             ;; The terminator is only inside the span for the FILE'S LAST line;
             ;; re-add the SAME one (`\r\n` in a CRLF file, else `\n`) so a
             ;; last-line replace does not downgrade that line's ending.
             matched-ends-crlf? (and matched-ends-nl?
                                     (< (long char-start) (dec (long char-end)))
                                     (= \return (.charAt current (- (long char-end) 2))))
             replacement-ends-nl? (str/ends-with? replacement "\n")
             rewritten (if (and matched-ends-nl? (not replacement-ends-nl?))
                         (str replacement (if matched-ends-crlf? "\r\n" "\n"))
                         replacement)]

            {:start char-start
             :end char-end
             :replacement rewritten
             :from-line (:from-line res)
             :to-line (:to-line res)}))))))
