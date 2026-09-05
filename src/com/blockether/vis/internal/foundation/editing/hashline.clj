(ns com.blockether.vis.internal.foundation.editing.hashline
  "Pure hashline primitives: the ANCHOR vocabulary `cat` mints, `grep` echoes and
   `patch` spends.

   An anchor is `<1-based line>:<3-hex content hash>` (Can Bölük's original
   hashline shape). The LINE NUMBER locates the line; the CONTENT HASH verifies
   it. A write requires BOTH coordinates to match exactly: any contradiction is
   REFUSED (`:anchor-mismatch`) instead of relocating the edit. Only the
   non-destructive read path may follow matching content through small line drift.

   This namespace is pure — no IO, no tool wiring, no extension envelope. Every
   surface that addresses a line routes here so the scheme is never recomputed:

     split-content-lines / char-offset-at-line   blob <-> line/char coordinates
     line-hash / line-anchor / anchor->line      text  -> `<line>:<hash>`
     render-hashline-block                       [[ln text]…] -> gutter text
     anchor-token / parse-anchor                 rendered line -> bare anchor
     indices-matching-hash                       content-only hash lookup
     resolve-one-anchor / resolve-anchor-range   exact write resolution
     resolve-anchor-range-read                   tolerant read resolution
     resolve-anchor-edit-span                    anchor span -> char span"
  (:require [clojure.string :as str]))

(set! *warn-on-reflection* true)

;; The anchor token — the ONE string that crosses Clojure -> CPython -> the model

(def hash-width
  "Hex chars in a line's content hash. The line number locates; this hash verifies
   the exact line for writes. Three hex chars keep an anchor at 4-7 characters;
   the line coordinate, not a file-wide hash search, disambiguates collisions."
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

(defn hash? [x] (and (string? x) (boolean (re-matches #"[0-9a-f]{3}" x))))

(defn line? [x] (pos-int? x))

(defn anchor? [x] (and (string? x) (boolean (re-matches #"\d+:[0-9a-f]{3}" x))))

(defn parsed? [x] (and (map? x) (line? (:line x)) (hash? (:hash x))))

;; Blob <-> coordinates

(defn split-content-lines
  "Split a file blob into a vec of lines. A trailing empty element (from the
   file's final newline) is dropped, so the vector's count IS the file's line
   count and index 0 is line 1."
  [^String s]
  (let [arr
        (.split s "\n" -1)

        v
        (vec arr)]

    (if (and (pos? (count v)) (= "" (peek v))) (pop v) v)))

(defn char-offset-at-line
  "Char offset in `content` where 0-based line `line-idx` starts. Returns
   `(count content)` when `line-idx` reaches past the last line. Public so the
   edit-span planner can map line indices back to char positions for splicing."
  ^long [^String content ^long line-idx]
  (loop [pos
         0

         i
         0]

    (if (= i line-idx)
      pos
      (let [nl (str/index-of content "\n" pos)]
        (if nl (recur (inc (long nl)) (inc i)) (count content))))))

;; Minting an anchor

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
  (let [h
        (int (bit-and (.hashCode (str/trim (str line))) (long hash-mask)))

        hex
        (Integer/toHexString h)

        c
        (.length hex)]

    (if (< c (long hash-width)) (str (subs hash-zero-pad c) hex) hex)))

(defn line-anchor
  "The editable anchor for a line: `<line-number>:<content-hash>` (e.g.
   `4439:a80`). The line number LOCATES the line and the hash VERIFIES its exact
   current content. `patch` refuses any mismatch instead of following the hash
   to another line."
  [ln text]
  (str ln hashline-anchor-sep (line-hash text)))

(def ^:private bare-anchor-re
  "A whole anchor token and nothing else: `<line>:<hash>`, hex in either case."
  #"\d+:[0-9a-fA-F]+")

(defn anchor-token
  "The bare `<line>:<hash>` token inside whatever the caller actually passed.

   Every anchor a model ever sees is RENDERED — `<line>:<hash>│ <text>`, because
   `cat`, `grep` and `patch`'s re-anchored window all print through
   `render-hashline-block` — and the contract those tools advertise is that one
   of their lines goes straight back in as an anchor. So everything from
   `hashline-gutter` on is DECORATION and is cut here: the line number locates
   and the hash verifies, while the text behind the gutter only SHOWED the line.
   Without this cut the text became part of the hash, no line could carry it,
   and the refusal handed back the very anchor it had just refused.

   What is left is unwrapped from stray whitespace and quote chars — the other
   common JSON/LLM mistake, where `\"4439:a80\"` arrives WITH its literal quotes
   and `parse-long` chokes on the leading one."
  ^String [x]
  (let [s
        (str x)

        cut
        (long (or (str/index-of s hashline-gutter) (count s)))]

    (-> (subs s 0 cut)
        str/trim
        (str/replace #"^['\"`]+" "")
        (str/replace #"['\"`]+$" "")
        str/trim)))

(defn anchor->line
  "Parse the 1-based line number out of a `<line>:<hash>` anchor, rendered or
   bare."
  ^long [anchor]
  (let [s
        (anchor-token anchor)

        i
        (str/index-of s hashline-anchor-sep)]

    (Long/parseLong (subs s 0 (long i)))))

(defn anchor-string?
  "True when `x` ADDRESSES a line: a bare `<line>:<hash>` token, or a whole
   rendered `<line>:<hash>│ <text>` line, which is that same anchor with the
   gutter still attached. `cat` uses it to tell an anchor endpoint from a bare
   line number; anything else is a number."
  [x]
  (boolean (and (string? x) (re-matches bare-anchor-re (anchor-token x)))))

(defn bare-anchor-string?
  "True when `x` is ONLY an anchor — the token, with no line text behind it.
   `patch` asks this of its REPLACEMENT slot: a bare anchor there is the span
   the caller forgot to finish, while a rendered line carrying TEXT is plausibly
   the content it means to write and is written (with a note), not refused."
  [x]
  (boolean (and (string? x)
                (not (str/includes? (str x) hashline-gutter))
                (re-matches bare-anchor-re (anchor-token x)))))

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
               (let [^String s
                     (str s)

                     ^String s
                     (if (str/ends-with? s "\r") (subs s 0 (dec (.length s))) s)]

                 (str indent (line-anchor ln s) hashline-gutter s))))
        (str/join "\n"))))

;; Resolving an anchor against LIVE content

(defn indices-matching-hash
  "0-based indices of `lines` whose `line-hash` equals the bare hash `h`. This is
   only for tolerant READ resolution; writes verify the hash at the named line."
  [lines h]
  (let [h (str h)]
    (into []
          (keep-indexed (fn [i l]
                          (when (= h (line-hash l)) i)))
          lines)))

(def hash-line-drift-tolerance
  "How far `resolve-anchor-range-read` may follow content from its stated line.
   Reads are non-destructive, so following one nearby hash match is useful; writes
   never use this window and require an exact line/hash pair."
  40)

(defn parse-anchor
  "Parse a `<line>:<hash>` anchor into `{:line L :hash H}` (L a 1-based long, H
   the hex content hash). The line number is REQUIRED: an anchor with no `:`
   separator, or a non-numeric line part, parses to `{:malformed true :raw S}`
   and `resolve-one-anchor` refuses it (`:anchor-malformed`). Every anchor must
   carry BOTH coordinates so the line LOCATES and the hash VERIFIES.

   A WHOLE RENDERED LINE parses exactly like a bare token: `anchor-token` cuts
   the gutter and the text behind it first, so the hash is only ever the hex."
  [anchor]
  (let [s
        (anchor-token anchor)

        i
        (.indexOf s (int \:))

        line
        (when-not (neg? i) (parse-long (subs s 0 i)))]

    (if (and (not (neg? i)) line)
      {:line line :hash (str/lower-case (subs s (inc i)))}
      {:malformed true :raw s})))

(defn resolve-one-anchor
  "Resolve one PARSED `{:line :hash}` write anchor to a 0-based index in `lines`,
   or `{:error {:reason KW …}}`. The line LOCATES and the hash VERIFIES that exact
   line. A mismatch is never relocated, even when the same hash occurs nearby;
   its error carries the current anchor at the stated line for one-step recovery."
  [lines which {:keys [line hash malformed raw]}]
  (if malformed
    {:error {:reason :anchor-malformed :which which :anchor raw}}
    (let [idx0
          (dec (long line))

          n
          (long (count lines))]

      (cond (or (neg? idx0) (>= idx0 n))
            {:error {:reason :anchor-line-out-of-range :which which :line line :lines n}}
            (= hash (line-hash (nth lines idx0))) {:index idx0}
            :else (let [current-text (nth lines idx0)]
                    {:error {:reason :anchor-mismatch
                             :which which
                             :hash hash
                             :stated-line line
                             :current-anchor (line-anchor line current-text)
                             :current-text current-text}})))))

(defn- resolve-one-anchor-read
  "Resolve one read anchor exactly when possible, otherwise follow its hash only
   when exactly one matching line is nearby. Other in-range mismatches return the
   exact-write refusal so `resolve-anchor-range-read` can fall back to the named
   line and mark the read stale."
  [lines which {:keys [line hash] :as anchor}]
  (let [exact (resolve-one-anchor lines which anchor)]
    (if (not= :anchor-mismatch (get-in exact [:error :reason]))
      exact
      (let [tol (long hash-line-drift-tolerance)
            in-window (filterv (fn [i]
                                 (<= (Math/abs (- (inc (long i)) (long line))) tol))
                        (indices-matching-hash lines hash))]

        (if (= 1 (long (count in-window))) {:index (first in-window)} exact)))))

(defn resolve-anchor-range
  "Resolve `from-anchor` and `to-anchor` against LIVE `current`; `to` defaults to
   `from` for a single-line edit. Every endpoint is a `<line>:<hash>` pair whose
   hash must match that exact line. A mismatch is refused (`:anchor-mismatch`),
   never relocated. Returns `{:from-line N :to-line N}` (1-based, INCLUSIVE) or
   `{:error {:reason KW …}}`; a range mismatch carries the fresh anchors for both
   endpoints.

   The WRITE side of the contract. `resolve-anchor-range-read` is its tolerant,
   non-destructive twin."
  [^String current from-anchor to-anchor]
  (let [lines
        (split-content-lines current)

        from-a
        (parse-anchor from-anchor)

        single-line?
        (or (nil? to-anchor) (= (str to-anchor) (str from-anchor)))

        to-a
        (if single-line? from-a (parse-anchor to-anchor))

        fr
        (resolve-one-anchor lines :from from-a)

        tr
        (if single-line? fr (resolve-one-anchor lines :to to-a))

        current-anchor-for
        (fn [resolved]
          (or (get-in resolved [:error :current-anchor])
              (when-let [i (:index resolved)]
                (line-anchor (inc (long i)) (nth lines (long i))))))

        current-from-anchor
        (current-anchor-for fr)

        current-to-anchor
        (current-anchor-for tr)

        error
        (or (:error fr) (:error tr))]

    (if error
      {:error (cond-> error
                (and (= :anchor-mismatch (:reason error))
                     (not single-line?)
                     current-from-anchor
                     current-to-anchor)
                (assoc :current-from-anchor
                  current-from-anchor :current-to-anchor
                  current-to-anchor))}
      (let [fi
            (long (:index fr))

            ti
            (long (:index tr))]

        (if (< ti fi)
          {:error {:reason :anchor-range-inverted :from-line (inc fi) :to-line (inc ti)}}
          {:from-line (inc fi) :to-line (inc ti)})))))

(defn resolve-anchor-range-read
  "READ-tolerant twin of `resolve-anchor-range`, for `cat`. A read is
   NON-DESTRUCTIVE, so one nearby hash match may be followed; otherwise an
   in-range stale anchor falls back to its stated LINE NUMBER. Returns
   `{:from-line N :to-line N :stale? BOOL}` (1-based, INCLUSIVE; the window never
   inverts), or `{:error …}` only for a genuinely unlocatable anchor
   (`:anchor-malformed` or `:anchor-line-out-of-range`)."
  [^String current from-anchor to-anchor]
  (let [lines
        (split-content-lines current)

        n
        (long (count lines))

        resolve-read
        (fn [which anchor]
          (let [a
                (parse-anchor anchor)

                r
                (resolve-one-anchor-read lines which a)]

            (cond (:index r) (assoc r :stale? false)
                  ;; No unambiguous nearby content match: safely show the stated line.
                  (and (:line a)
                       (= :anchor-mismatch (get-in r [:error :reason]))
                       (<= 1 (long (:line a)) n))
                  {:index (dec (long (:line a))) :stale? true}
                  :else r)))

        fr
        (resolve-read :from from-anchor)]

    (if (:error fr)
      fr
      (let [tr (if (or (nil? to-anchor) (= (str to-anchor) (str from-anchor)))
                 fr
                 (resolve-read :to to-anchor))]
        (if (:error tr)
          tr
          (let [fi (long (:index fr))
                ti (long (:index tr))]

            {:from-line (inc (min fi ti))
             :to-line (inc (max fi ti))
             :stale? (boolean (or (:stale? fr) (:stale? tr)))}))))))

(defn- line-span->char-span
  "Convert a 0-based [line-start line-end) span to a [char-start char-end]
   substring span in `content`, keeping the region's trailing terminator
   OUTSIDE the replaced region."
  [^String content ^long line-start ^long line-end]
  (let [char-start
        (char-offset-at-line content line-start)

        char-end-raw
        (char-offset-at-line content line-end)

        char-end
        (let [e (long (if (and (< char-end-raw (count content))
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
  "Resolve an exact anchored line range to a CHAR SPAN against `current`, WITHOUT
   building new content: `{:start S :end E :replacement R :from-line N :to-line N}`
   or `{:error {:reason KW …}}`. `to-anchor` defaults to `from-anchor` (a single
   line); every supplied endpoint must match its exact current line.

   Newline semantics: a replacement need NOT end in `\\n` — the matched region's
   terminator is preserved (`\\r\\n` stays `\\r\\n` on a CRLF file) — and one that
   DOES is not doubled: exactly one trailing terminator is dropped, so a block
   copied with its own newline REPLACES the span instead of growing a blank line
   after it. An EMPTY replacement consumes the trailing newline, so the lines
   actually vanish instead of leaving blanks behind."
  [^String current from-anchor to-anchor ^String replacement]
  (let [res (resolve-anchor-range current from-anchor to-anchor)]
    (if (:error res)
      res
      (let [line-start (dec (long (:from-line res)))
            line-end (long (:to-line res))]

        (if (= "" (str replacement))
          ;; DELETION: take the WHOLE physical line(s) INCLUDING the trailing
          ;; newline so the lines are actually removed. The `line-span->char-span`
          ;; rule below deliberately keeps a matched region's trailing `\n` OUTSIDE
          ;; the span (so a REPLACE never doubles the newline) — but for an empty
          ;; replacement that rule makes a single blank-line delete a zero-width
          ;; no-op and a multi-line delete leave one line behind.
          (let [char-start (char-offset-at-line current line-start)
                char-end (char-offset-at-line current line-end)]

            {:start char-start
             :end char-end
             :replacement ""
             :from-line (:from-line res)
             :to-line (:to-line res)})
          (let [[char-start char-end] (line-span->char-span current line-start line-end)
                ;; The terminator is INSIDE the span only at EOF — `line-span->char-span`
                ;; keeps every other line's `\n` outside it. Reading the last char of a
                ;; span that ends anywhere else answers the PREVIOUS line's `\n` whenever
                ;; the span's last line is EMPTY, and the replacement was then padded with
                ;; a newline it must not carry: every replace over a span ending on a blank
                ;; line grew one extra blank line.
                matched-ends-nl? (and (= (long char-end) (.length current))
                                      (< (long char-start) (long char-end))
                                      (= \newline (.charAt current (dec (long char-end)))))
                ;; The terminator that IS inside the span belongs to the file's last line;
                ;; re-add the SAME one (`\r\n` in a CRLF file, else `\n`) so a last-line
                ;; replace does not downgrade that line's ending.
                matched-ends-crlf? (and matched-ends-nl?
                                        (< (long char-start) (dec (long char-end)))
                                        (= \return (.charAt current (- (long char-end) 2))))
                ;; The replacement's OWN terminator is redundant — the matched region's is
                ;; preserved — so a block copied with its trailing newline would otherwise
                ;; add a blank line nobody asked for, once per edit and silently. Exactly
                ;; ONE is dropped, so `"…\n\n"` still says "and then one blank line".
                body (cond (str/ends-with? replacement "\r\n")
                           (subs replacement 0 (- (.length replacement) 2))
                           (str/ends-with? replacement "\n")
                           (subs replacement 0 (dec (.length replacement)))
                           :else replacement)
                rewritten (if matched-ends-nl? (str body (if matched-ends-crlf? "\r\n" "\n")) body)]

            {:start char-start
             :end char-end
             :replacement rewritten
             :from-line (:from-line res)
             :to-line (:to-line res)}))))))
