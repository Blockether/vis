(ns com.blockether.vis.internal.foundation.editing.patch
  "Fuzzy line-based matching toolkit used by `patch` exact-replace.

   This namespace USED to also carry the Codex `apply_patch` envelope
   parser, but envelope mode was retired — we consolidated to a single
   per-intent mutation surface (`patch` exact-replace + `write` /
   `move` / `delete`). The fuzzy matcher stays because it is what
   lets multi-line `:search` blocks tolerate whitespace and typographic
   drift before they fall through to `:no-match`.

   Public surface (all pure):
     seek-sequence            lines pattern start eof? -> start-index or nil
     seek-sequence-with-pass  lines pattern start eof?
                              -> {:start :pass :indent-delta?} or nil
     split-content-lines      string -> vec of lines (no trailing \\n element)
     char-offset-at-line      content line-idx -> char offset
     apply-indent-delta       delta lines -> re-indented lines

   It also owns the reusable HASHLINE layer (content-addressed editing):
     line-hash / lines->anchors              text -> 6-hex anchor / {ln hash}
     render-hashline-block / -range-block   tuples -> `<hash>| text` gutter
     indices-matching-hash / resolve-anchor-edit  self-locating range replace"
  (:require [clojure.string :as str]))
;; =============================================================================
;; seek-sequence — fuzzy line matcher
;; =============================================================================
(def ^:private dash-codepoints #{0x2010 0x2011 0x2012 0x2013 0x2014 0x2015 0x2212})
(def ^:private single-quote-codepoints #{0x2018 0x2019 0x201A 0x201B})
(def ^:private double-quote-codepoints #{0x201C 0x201D 0x201E 0x201F})
(def ^:private space-codepoints
  #{0x00A0 0x2002 0x2003 0x2004 0x2005 0x2006 0x2007 0x2008 0x2009 0x200A 0x202F 0x205F 0x3000})
(defn- normalize-line
  "Fold typographic dashes/quotes/spaces to ASCII. Does NOT trim — the
   `:unicode` pass is intentionally single-purpose so it stays orthogonal
   to the `:trim` / `:rstrip` passes (and so the destructive `:trim` pass
   keeps running last)."
  [^String s]
  (let [sb (StringBuilder.)]
    (doseq [^Character ch s]
      (let [cp (int ch)]
        (cond (contains? dash-codepoints cp) (.append sb \-)
              (contains? single-quote-codepoints cp) (.append sb \')
              (contains? double-quote-codepoints cp) (.append sb \")
              (contains? space-codepoints cp) (.append sb \space)
              :else (.append sb ch))))
    (.toString sb)))
(defn- match-at?
  [lines pattern i cmp]
  (let [i (long i)
        n (long (count pattern))]
    (loop [k 0]
      (if (= k n)
        true
        (if (cmp (nth lines (+ i k)) (nth pattern k)) (recur (inc k)) false)))))
(def ^:private fuzzy-passes
  "Per-line fuzzy strategies. `seek-sequence-with-pass` orchestrates them
   in two phases so the structure-preserving `:relative-indent` pass can
   run BEFORE `:trim` (which destroys indentation structure).

   Layout convention (must stay in sync with `seek-sequence-with-pass`):
     index 0  :exact
     index 1  :rstrip
     index 2  :unicode    <- last of the tight-equality passes
     index 3  :trim       <- looser, runs only after :relative-indent

   Each entry is `[<pass-name> <cmp-fn>]`. The pass keyword is returned
   alongside the start index so failure messages can tell the model
   *why* an edit went fuzzy (e.g. `:trim` = indentation drift)."
  [[:exact =]
   [:rstrip
    (fn [^String a ^String b]
      (= (str/trimr a) (str/trimr b)))]
   [:unicode
    (fn [^String a ^String b]
      (= (normalize-line a) (normalize-line b)))]
   [:trim
    (fn [^String a ^String b]
      (= (str/trim a) (str/trim b)))]])
(defn- leading-ws-count
  "Count of leading space/tab characters on `line`."
  ^long [^String line]
  (loop [i 0]
    (if (and (< i (count line))
             (let [c (.charAt line i)]
               (or (= c \space) (= c \tab))))
      (recur (inc i))
      i)))
(defn- min-leading-indent
  "Smallest leading-whitespace count across non-blank `lines`, or nil if
   every line is blank. Used by relative-indent matcher."
  [lines]
  (let [counts (->> lines
                    (remove str/blank?)
                    (map leading-ws-count))]
    (when (seq counts) (long (apply min counts)))))
(defn- de-indent
  "Strip `n` leading chars from `line` if it has them. Blank lines are
   left alone (mirrors Python's textwrap.dedent semantics — blank lines
   don't constrain indent)."
  [^long n ^String line]
  (cond (str/blank? line) line
        (>= (count line) n) (subs line n)
        :else line))
(defn- seek-relative-indent
  "5th fuzzy pass (Aider-style): try matching `pattern` after stripping
   the common leading indent from both pattern and each candidate
   window. Lets a SEARCH block authored at one indent level still match
   the same code at a different level — common LLM whitespace quirk.

   Returns `{:start <i> :indent-delta <n>}` where `:indent-delta` is the
   number of leading WS chars the *file's* matched window carries minus
   the pattern's, or nil when no window matches. Callers use
   `:indent-delta` to re-indent the `replace` lines."
  [lines pattern start]
  (let [plen
        (long (count pattern))

        llen
        (long (count lines))]

    (when (and (pos? plen) (<= plen llen))
      (let [p-indent
            (long (or (min-leading-indent pattern) 0))

            p-deindented
            (mapv #(de-indent p-indent %) pattern)

            end
            (- llen plen)]

        (loop [i (long start)]
          (when (<= i end)
            (let [window (subvec lines i (+ i plen))
                  w-indent (long (or (min-leading-indent window) 0))
                  w-deindented (mapv #(de-indent w-indent %) window)]

              (if (= p-deindented w-deindented)
                {:start i :indent-delta (- w-indent p-indent)}
                (recur (inc i))))))))))
(defn seek-sequence-with-pass
  "Like `seek-sequence` but returns `{:start <i> :pass <kw> :indent-delta <n>?}`
   so callers can surface which fuzzy pass produced the hit. `pass` is one of
   `:exact :rstrip :unicode :relative-indent :trim`. nil when no pass matches.

   Pass priority is deliberately:
     :exact → :rstrip → :unicode → :relative-indent → :trim
   `:relative-indent` runs BEFORE `:trim` because `:trim` is destructive
   (it drops both leading and trailing whitespace from each line and so
   collapses different indentation structures to the same key). The more
   structure-preserving `:relative-indent` should win whenever it can."
  [lines pattern start eof?]
  (cond (empty? pattern) {:start start :pass :exact}
        (> (count pattern) (count lines)) nil
        :else (let [plen
                    (count pattern)

                    llen
                    (count lines)

                    end
                    (- llen plen)

                    eof-start
                    (when eof? (long (max 0 (- llen plen))))

                    search-start
                    (long (or eof-start start))

                    run-pass
                    (fn [cmp]
                      (loop [i search-start]
                        (cond (> i end) nil
                              (match-at? lines pattern i cmp) i
                              :else (recur (inc i)))))

                    [exact-pass-pairs trim-pass-pair]
                    (split-at 3 fuzzy-passes)

                    ;; First sweep: exact, rstrip, unicode. Order matters.
                    tight-hit
                    (reduce (fn [_ [pass-name cmp]]
                              (when-let [hit (run-pass cmp)]
                                (reduced {:start hit :pass pass-name})))
                            nil
                            exact-pass-pairs)]

                (or tight-hit
                    ;; Second: structure-preserving relative-indent before the
                    ;; destructive `:trim` pass.
                    (when-let [{:keys [start indent-delta]}
                               (seek-relative-indent lines pattern search-start)]
                      {:start start :pass :relative-indent :indent-delta indent-delta})
                    ;; Last resort: trim. Pass is named `:trim` here but corresponds
                    ;; to the 3rd entry in `fuzzy-passes` — see split-at above.
                    (let [[pass-name cmp] (first trim-pass-pair)]
                      (when-let [hit (run-pass cmp)]
                        {:start hit :pass pass-name}))))))
(defn seek-sequence
  "Find `pattern` (a vec of strings) inside `lines` (vec of strings)
   starting at `start`. When `eof?` is true, first try the EOF position
   then fall back to `start`.

   Match strictness, in order: exact -> rstrip -> unicode -> relative-indent
   -> trim. Returns the start index or nil. For pass attribution use
   `seek-sequence-with-pass`."
  [lines pattern start eof?]
  (:start (seek-sequence-with-pass lines pattern start eof?)))
(defn split-content-lines
  "Split a file blob into a vec of lines. Trailing empty element (from a
   final newline) is dropped, matching the convention used by the
   exact-replace path."
  [^String s]
  (let [arr
        (.split s "\n" -1)

        v
        (vec arr)]

    (if (and (pos? (count v)) (= "" (peek v))) (pop v) v)))
(defn char-offset-at-line
  "Char offset in `content` where 0-based line `line-idx` starts.
   Returns `(count content)` if `line-idx` reaches past the last line.
   Public so the exact-replace path can map line indices back to char
   positions for substring splicing."
  ^long [^String content ^long line-idx]
  (loop [pos
         0

         i
         0]

    (if (= i line-idx)
      pos
      (let [nl (str/index-of content "\n" pos)]
        (if nl (recur (inc (long nl)) (inc i)) (count content))))))
(defn apply-indent-delta
  "Re-indent `lines` by `delta` leading spaces (positive adds, negative
   strips). Blank lines untouched. Used by exact-replace when a fuzzy
   :relative-indent hit fires and the `replace` payload must follow the
   file's actual indentation rather than the SEARCH block's."
  [delta lines]
  (let [delta (long delta)]
    (cond (zero? delta) (vec lines)
          (pos? delta) (let [pad (apply str (repeat delta \space))]
                         (mapv (fn [^String l]
                                 (if (str/blank? l) l (str pad l)))
                               lines))
          :else (let [strip (- delta)]
                  (mapv (fn [^String l]
                          (if (str/blank? l) l (subs l (long (min strip (leading-ws-count l))))))
                        lines)))))
;; =============================================================================
;; ws-agnostic token matcher — re-segments across newlines
;;
;; The 5-pass `seek-sequence` matcher above is LINE-STRUCTURED: it slides a
;; window of exactly `(count search-lines)` lines and compares line-for-line.
;; That cannot match when a SEARCH block joins or splits a line relative to
;; the file (e.g. authoring `KeyType/Backspace (do …)` on one line when the
;; file keeps `KeyType/Backspace` on its own line). This token matcher folds
;; BOTH sides to a whitespace-free token stream so line breaks, indentation
;; and run-length differences are all irrelevant, then snaps the hit back to
;; whole-line boundaries so it flows through the same replace machinery.
;; =============================================================================
(defn line-index-at
  "0-based line index of char offset `off` in `content` (count of `\n`
   before it)."
  ^long [^String content ^long off]
  (let [lim (long (min off (count content)))]
    (loop [i 0
           ln 0]

      (if (>= i lim) ln (recur (inc i) (if (= \newline (.charAt content i)) (inc ln) ln))))))
(defn tokenize-with-offsets
  "Vec of `[token ^long start ^long end]` for every maximal non-whitespace
   run in `s`. `start`/`end` are raw char offsets (end exclusive)."
  [^String s]
  (let [n (long (count s))
        acc (java.util.ArrayList.)]
    (loop [i (long 0)]
      (when (< (long i) n)
        (if (Character/isWhitespace (.charAt s (long i)))
          (recur (long (inc (long i))))
          (let [j (long (loop [k (long i)]
                          (if (and (< k n) (not (Character/isWhitespace (.charAt s k))))
                            (recur (inc k))
                            k)))]
            (.add acc [(subs s (long i) j) (long i) j])
            (recur j)))))
    (vec acc)))
(defn ws-agnostic-line-span
  "Whitespace-agnostic token-subsequence match. Folds `search` and
   `content` to whitespace-free token streams and looks for `search`'s
   tokens as a contiguous subsequence of `content`'s. Because all
   whitespace (incl. newline structure) is discarded, a SEARCH block whose
   line breaks drifted from the file still locates.

   Returns `{:line-start <i> :line-end <j> :occurrences <n>}` (0-based,
   `:line-end` exclusive) snapped to whole lines around the FIRST hit, or
   nil when zero tokens match. `:occurrences` is the total contiguous-hit
   count so callers can refuse ambiguous (>1) applies."
  [^String content ^String search]
  (let [ct
        (tokenize-with-offsets content)

        st-toks
        (mapv first (tokenize-with-offsets search))

        slen
        (long (count st-toks))]

    (when (pos? slen)
      (let [ctoks
            (mapv first ct)

            n
            (long (count ctoks))

            hits
            (loop [i
                   0

                   acc
                   []]

              (if (> i (- n slen))
                acc
                (recur (inc i) (if (= st-toks (subvec ctoks i (+ i slen))) (conj acc i) acc))))]

        (when (seq hits)
          (let [i
                (long (first hits))

                [_ tstart _]
                (nth ct i)

                [_ _ tend]
                (nth ct (+ i slen -1))

                line-start
                (line-index-at content tstart)

                line-end
                (inc (line-index-at content (dec (long tend))))]

            {:line-start line-start :line-end line-end :occurrences (long (count hits))}))))))
;; =============================================================================
;; Hashline layer — the single, reusable line+content-addressed editing surface.
;;
;; An anchor is `<line-number>:<hash>` (Can Bölük's original hashline shape).
;; The line number LOCATES the line; the content hash VERIFIES it. Two
;; coordinates: a reused/stale hash can no longer silently land an edit on the
;; wrong line — if the content sits far from the stated line, patch refuses
;; (`:hashline-misplaced`) instead of corrupting. This block owns every reusable
;; piece so callers never recompute the scheme:
;;
;;   line-hash                text            -> hash-width-hex content hash
;;   line-anchor              ln text         -> "<ln>:<hash>"   (one anchor)
;;   lines->anchors            [[ln text]…]    -> {ln "ln:hash"}  (model map)
;;   render-hashline-block    [[ln text]…]    -> "<ln>:<hash>│ text…" (gutter)
;;   render-hashline-range-block ranges       -> headered gutter blocks
;;   indices-matching-hash    lines hash      -> [0-based idx …]  (content only)
;;   resolve-anchor-edit        content a a2 rep -> {:new-content}|{:error}
;;
;; The gutter the reader SEES is the edit address — `patch :from_anchor` parses
;; the same `<line-number>:<hash>` back against live content. The line number
;; is part of the address now, not just navigation.
;; =============================================================================
(def hash-width
  "Hex chars in a line's content hash. Anchors now carry the LINE NUMBER too
   (`<lineno>:<hash>` — see `line-anchor` / `lines->anchors`), so the hash no
   longer has to be globally unique: the line number LOCATES the line and the
   hash only VERIFIES the content there (drift + misattribution). That
   collapses the hash's job from whole-file disambiguation — which forced
   width 4 / 65536 buckets back when the hash was the SOLE anchor (see git
   history for the old collision-vs-token table) — to a local check inside a
   `hash-line-drift-tolerance`-sized window. 3 hex (4096 buckets) is ample
   there: the residual in-window collision chance is ~0.1%, and the line
   number disambiguates even that. Bump this and the mask/pad follow
   automatically. (This is exactly Can Bölük's original `lineno:hash` hashline
   shape — the bare-hash variant was the vis-specific detour that lost the
   line coordinate and, with it, the wrong-line guard.)"
  3)
(def ^:private hash-mask
  "Low `hash-width` hex digits as a bit mask: (16^hash-width) - 1."
  (long (dec (bit-shift-left 1 (* 4 (long hash-width))))))
(def ^:private hash-zero-pad
  "`hash-width` zero chars, for left-padding a short `Integer/toHexString`."
  (apply str (repeat (long hash-width) \0)))
(defn line-hash
  "Stable `hash-width`-hex-char content hash of `line` (trimmed). Folds
   the spec'd `String/hashCode` algorithm over the whitespace-trimmed
   line, so it is deterministic across JVM runs. Identical trimmed lines
   share a hash — a dup-line collision makes a `:from_anchor` anchor
   ambiguous and `resolve-anchor-edit` refuses it (caller falls back to
   `:search`).

   Hot path: runs once per line on every `cat` render AND every patch
   resolve. Formats with `Integer/toHexString` + a left-pad rather than
   java.util.Formatter, which benches ~1.5x slower; the trimmed
   `String/hashCode` is a JIT intrinsic so we lean on it instead of a
   hand loop."
  ^String [line]
  (let [h
        (int (bit-and (.hashCode (str/trim (str line))) (long hash-mask)))

        hex
        (Integer/toHexString h)

        c
        (.length hex)]

    (if (< c (long hash-width)) (str (subs hash-zero-pad c) hex) hex)))
(def ^:const hashline-anchor-sep
  "Separator between the line number and the content hash inside an anchor
   (`<lineno>:<hash>`). A single char so the gutter stays narrow."
  ":")
(defn line-anchor
  "The editable anchor for a line: `<line-number>:<content-hash>` (e.g.
   `325:0e3`). The line number LOCATES the line; the hash VERIFIES its
   content. `patch :from_anchor` parses this back via `resolve-anchor-range`,
   matching the line number against live content and refusing if the hash no
   longer agrees (the line changed) or that content now lives far from the
   stated line (a misattributed / stale anchor). Two coordinates, so a single
   reused hash can no longer silently land an edit on the wrong line."
  [ln text]
  (str ln hashline-anchor-sep (line-hash text)))
(defn lines->anchors
  "`{line-number anchor}` map of every non-blank line in `tuples`, where each
   anchor is `<line-number>:<content-hash>` (`line-anchor`). The canonical
   `:anchors` payload `cat` returns — the SINGLE place it is built (read-file /
   read-file-ranges / tail-file / rg all route here). Blank lines are omitted:
   the model only ever sees anchors it can actually edit by. Line numbers come
   straight from the `[ln text]` tuples, so a windowed read (range / tail /
   by-hash) carries the file's real line numbers with NO second full-file pass
   — the `#N` file-wide-ordinal scheme (and its whole-file rescan) is gone now
   that the line number, not the hash, disambiguates duplicate lines."
  [tuples]
  (into {}
        (keep (fn [[ln s]]
                (when-not (str/blank? (str s)) [ln (line-anchor ln s)])))
        tuples))

(defn anchor->line
  "Parse the line number out of a `<lineno>:<hash>` anchor."
  ^long [anchor]
  (let [s
        (str anchor)

        i
        (str/index-of s hashline-anchor-sep)]

    (Long/parseLong (subs s 0 (long i)))))

(defn lines->anchor-map
  "Ordered `{anchor {\"text\" line}}` map for `[[ln text]…]` tuples: a REAL
   `java.util.LinkedHashMap` (natively insertion-ordered), built in line order
   — each KEY is the line's `<lineno>:<hash>` `line-anchor`, each VALUE a
   `{\"text\" <line>}` map. This MIRRORS rg's hit value, so a consumer reads
   `v[\"text\"]` uniformly across `cat` and `rg` (the whole point of the shape).
   Being an ordered hashmap (not a Clojure map), it stays in file order across
   the Clojure → JSON/charred → GraalPy dict boundary at ANY size, with NO
   comparator. EVERY line is keyed (blanks included, so the read stays
   gap-free); duplicate text differs by line number, so keys are unique. THE
   single model-facing line payload `cat` returns; the key IS the
   `patch :from_anchor`."
  ^java.util.LinkedHashMap [tuples]
  (let [m (java.util.LinkedHashMap.)]
    (doseq [[ln s] tuples]
      (.put m (line-anchor ln s) {"text" s}))
    m))

(defn anchor-value-text
  "Extract the line text from an `:anchors` / `before` / `after` map VALUE.
   Every model-facing anchor value is a `{\"text\" <line>}` map (see
   `lines->anchor-map`); a bare string is tolerated for legacy / hand-built
   maps so the human renderers never choke on an older shape."
  [v]
  (if (map? v) (get v "text") v))

(defn anchor-map->tuples
  "Inverse of `lines->anchor-map`: `{anchor {\"text\" text}}` → `[[ln text]…]`
   tuples, the line number parsed from each `<lineno>:<hash>` key and the text
   pulled from the value's `\"text\"` (a bare-string value is tolerated via
   `anchor-value-text`), sorted by line. For the channel/human gutter and any
   internal consumer that still wants tuples."
  [m]
  (->> m
       (map (fn [[a t]]
              [(anchor->line a) (anchor-value-text t)]))
       (sort-by first)
       vec))
(def ^:const hashline-gutter
  "Separator between the anchor and the line text in rendered output."
  "│ ")
(defn render-lineno-block
  "Render `[line-number text]` tuples as a HUMAN line-number gutter
   `<ln>│ <text>`, line numbers right-aligned to the widest number in
   the block. Unlike `render-hashline-block` (the MODEL surface, whose
   gutter is the editable `:from_anchor` anchor), this is the channel/TUI
   display surface: humans navigate by line number, not by content hash."
  [tuples]
  (let [tuples
        (vec tuples)

        width
        (reduce (fn [^long w [ln _]]
                  (max w (long (count (str ln)))))
                1
                tuples)]

    (->> tuples
         (map (fn [[ln s]]
                (str (format (str "%" width "s") (str ln)) hashline-gutter s)))
         (str/join "\n"))))
(defn render-lineno-range-block
  "`render-lineno-block` analogue for `:ranges` windows — `-- range S-E --`
   headers followed by the human line-number gutter for each window."
  [ranges]
  (->> ranges
       (map (fn [{:keys [range lines]}]
              (let [[start end] range]
                (str "-- range " start
                     "-" end
                     " --" (when (seq lines) (str "\n" (render-lineno-block lines)))))))
       (str/join "\n\n")))
(defn tuples->ranges
  "Split flat `[[ln text]…]` tuples into contiguous `:ranges` windows\n   `[{:range [start end] :lines [[ln text]…]}…]`, breaking the run whenever\n   the line number jumps by more than 1. Produces exactly the shape\n   `render-lineno-range-block` / `render-hashline-range-block` consume, so a\n   flat tuple list (e.g. grouped grep hits) renders with the same\n   `-- range S-E --` gap headers as a native multi-range read."
  [tuples]
  (->> tuples
       (reduce (fn [groups [ln :as t]]
                 (let [ln (long ln)
                       g
                       (peek groups)]

                   (if g
                     (let [last-ln (long (first (peek g)))]
                       (if (= ln (inc last-ln))
                         (conj (pop groups) (conj g t))
                         (conj groups [t])))
                     (conj groups [t]))))
               [])
       (mapv (fn [g]
               {:range [(ffirst g) (first (peek g))] :lines g}))))
(defn- line-span->char-span
  "Convert a 0-based [line-start line-end) span to a [char-start char-end]
   substring span in `content`, keeping a trailing `\n` OUTSIDE the
   replaced region."
  [^String content ^long line-start ^long line-end]
  (let [char-start
        (char-offset-at-line content line-start)

        char-end-raw
        (char-offset-at-line content line-end)

        char-end
        (if (and (< char-end-raw (count content))
                 (pos? char-end-raw)
                 (= \newline (.charAt content (dec char-end-raw))))
          (dec char-end-raw)
          char-end-raw)]

    [char-start char-end]))
(defn indices-matching-hash
  "0-based indices of `lines` whose content `line-hash` equals the bare hash
   `h`. Pure content match — the line-number coordinate is applied separately
   by `resolve-one-anchor`, so this just answers \"which lines carry this
   content\"."
  [lines h]
  (let [h (str h)]
    (into []
          (keep-indexed (fn [i l]
                          (when (= h (line-hash l)) i)))
          lines)))
(def hash-line-drift-tolerance
  "How far (in lines) a content hash may sit from its stated line number
   before `resolve-one-anchor` calls the anchor MISPLACED and refuses. The
   common path never needs it — within one patch batch every hunk resolves
   against the file's ORIGINAL snapshot, so the stated line number is exact and
   the fast path fires. This window only forgives small drift when the model
   reuses anchors across patch calls without re-reading; anything larger (the
   ~200-line gap of a genuinely wrong/stale anchor — the corruption this whole
   scheme exists to stop) is refused so the model re-reads. Deliberately tight:
   a false refuse costs one re-read, a false accept corrupts the file."
  40)
(defn- parse-anchor
  "Parse a `<line-number>:<hash>` anchor into `{:line L :hash H}` (L a 1-based
   long, H the hex content hash). The line number is REQUIRED: an anchor with no
   `:` separator (or a non-numeric line part) parses to `{:malformed true :raw S}`
   and `resolve-one-anchor` refuses it (`:hashline-malformed`). Every
   `:from_anchor` must carry BOTH coordinates so the line LOCATES and the hash
   VERIFIES; the old bare-hash fallback that resolved by content uniqueness alone
   is gone (a hash with no line could silently land on the wrong duplicate line)."
  [anchor]
  (let [s
        (str anchor)

        i
        (.indexOf s (int \:))

        line
        (when-not (neg? i) (parse-long (subs s 0 i)))]

    (if (and (not (neg? i)) line) {:line line :hash (subs s (inc i))} {:malformed true :raw s})))
(defn- resolve-one-anchor
  "Resolve a single parsed `{:line :hash}` anchor to a 0-based index in
   `lines`, or `{:error {:reason KW ...}}`. The LINE locates; the hash VERIFIES
   but a NON-UNIQUE hash never blocks a well-located edit:
     1. exact     - the stated line still hashes to `hash`          -> use it.
     2. drifted   - the line moved a little and `hash` is at EXACTLY one line
                    within `hash-line-drift-tolerance` of it        -> follow it.
     3. line wins - `hash` is AMBIGUOUS (matches several lines, at least one
                    near the stated line): the hash can't choose, but the model
                    addressed an EXPLICIT line, so use it. Duplicate hashes do
                    NOT make a `lineno:hash` anchor ambiguous.
     4. misplaced - `hash` matches only line(s) FAR from the stated line: a
                    strong line-vs-content contradiction -> refuse (WRONG-LINE
                    guard; this is what stops an edit landing on the wrong line).
     5. not-found - `hash` matches no live line (content is gone)   -> refuse.
   Plus: a malformed anchor (no `<lineno>:` prefix) or a line outside the file
   is refused — those genuinely cannot be located."
  [lines which {:keys [line hash malformed raw]}]
  (if malformed
    {:error {:reason :hashline-malformed :which which :anchor raw}}
    (let [idx0
          (dec (long line))

          n
          (long (count lines))]

      (cond (or (neg? idx0) (>= idx0 n))
            {:error {:reason :hashline-line-out-of-range :which which :line line :lines n}}
            ;; 1. exact — content at the stated line verifies the hash
            (= hash (line-hash (nth lines idx0))) {:index idx0}
            :else (let [matches (indices-matching-hash lines hash)]
                    (if (empty? matches)
                      ;; 5. content is gone — refuse, re-read. Carry the CURRENT anchor at
                      ;;    the stated line so the caller can recover in ONE step (the
                      ;;    common stale-after-`write` case) instead of a separate `cat`.
                      {:error {:reason :hashline-not-found
                               :which which
                               :hash hash
                               :stated-line line
                               :current-anchor (line-anchor line (nth lines idx0))
                               :current-text (nth lines idx0)}}
                      (let [tol (long hash-line-drift-tolerance)
                            in-win (filterv (fn [i]
                                              (<= (Math/abs (- (inc (long i)) (long line))) tol))
                                            matches)]

                        (cond
                          ;; 2. drifted — one nearby match, follow the content
                          (= 1 (long (count in-win))) {:index (first in-win)}
                          ;; 4. hash matches only FAR from the stated line — WRONG-LINE guard
                          (empty? in-win) {:error {:reason :hashline-misplaced
                                                   :which which
                                                   :hash hash
                                                   :stated-line line
                                                   :found-lines (mapv #(inc (long %)) matches)}}
                          ;; 3. several nearby matches — hash can't disambiguate; the
                          ;;    explicit line wins (the user's `lineno:dup-hash` case)
                          :else {:index idx0}))))))))
(defn resolve-anchor-range
  "Resolve `from_anchor` (and `to_anchor`, defaulting to `from_anchor` for a single
   line) against LIVE `current`. Each is a `<line-number>:<hash>` anchor: the
   line number LOCATES it, the hash VERIFIES the content still matches AND sits
   near the stated line (else `:hashline-misplaced` — the wrong-line guard). Both
   coordinates are REQUIRED; a bare hash with no line number is refused
   (`:hashline-malformed`). Returns `{:from-line N :to-line N}` (1-based,
   INCLUSIVE) or `{:error {:reason KW …}}`.

   Shared by `resolve-anchor-edit` (WRITE — patch :from_anchor) and the cat
   `:anchor` READ path so both address lines identically."
  [^String current from_anchor to_anchor]
  (let [lines
        (split-content-lines current)

        from-a
        (parse-anchor from_anchor)

        to-a
        (if (or (nil? to_anchor) (= (str to_anchor) (str from_anchor)))
          from-a
          (parse-anchor to_anchor))

        fr
        (resolve-one-anchor lines :from from-a)]

    (if (:error fr)
      fr
      (let [tr (if (identical? from-a to-a) fr (resolve-one-anchor lines :to to-a))]
        (if (:error tr)
          tr
          (let [fi (long (:index fr))
                ti (long (:index tr))]

            (if (< ti fi)
              {:error {:reason :hashline-range-inverted :from-line (inc fi) :to-line (inc ti)}}
              {:from-line (inc fi) :to-line (inc ti)})))))))
(defn resolve-anchor-edit-span
  "Resolve a content-addressed line-range edit to a CHAR SPAN against `current`,
   WITHOUT building new content: `{:start S :end E :replacement R :applied-line N}`
   or `{:error {:reason KW …}}`. Lets a multi-edit batch resolve every anchor
   against the ORIGINAL snapshot and splice all spans together atomically, so an
   earlier edit can't drift a later edit's hash/ordinal. `to_anchor` defaults to
   `from_anchor` (single line). The stated line is tried first and only then
   small drift is considered; duplicate hashes at other lines do not make an
   exact `lineno:hash` anchor ambiguous."
  [^String current from_anchor to_anchor ^String replace]
  (let [res (resolve-anchor-range current from_anchor to_anchor)]
    (if (:error res)
      res
      (let [line-start (dec (long (:from-line res)))
            line-end (long (:to-line res))]

        (if (= "" (str replace))
          ;; DELETION (empty replace): take the WHOLE physical line(s) including
          ;; the trailing newline, so the line is actually removed. The
          ;; `line-span->char-span` rule below deliberately keeps a matched
          ;; region's trailing `\n` OUTSIDE the span (so a REPLACE never doubles
          ;; the newline) — but for an empty replace that rule makes a single
          ;; blank-line delete a ZERO-WIDTH no-op and a multi-line delete leave
          ;; one line behind. Consuming the trailing newline here makes
          ;; `replace ""` mean "delete these lines" with no leftover blank.
          (let [char-start (char-offset-at-line current line-start)
                char-end (char-offset-at-line current line-end)]

            {:start char-start :end char-end :replacement "" :applied-line (inc line-start)})
          (let [[char-start char-end] (line-span->char-span current line-start line-end)
                ;; Only a NON-EMPTY span can end in a newline. An empty-line span is
                ;; zero-width (char-start == char-end); without this guard the check
                ;; reads the PREVIOUS line's `\n` (just before char-end) and wrongly
                ;; pads the replacement with a `\n`, inserting instead of replacing.
                matched-ends-nl? (and (< (long char-start) (long char-end))
                                      (= \newline (.charAt current (dec (long char-end)))))
                replace-ends-nl? (str/ends-with? replace "\n")
                rewritten
                (if (and matched-ends-nl? (not replace-ends-nl?)) (str replace "\n") replace)]

            {:start char-start
             :end char-end
             :replacement rewritten
             :applied-line (inc line-start)}))))))
(defn resolve-anchor-edit
  "Content-addressed line-range replace returning full `{:new-content S
   :applied-line N}` (or `{:error …}`). Thin wrapper over
   `resolve-anchor-edit-span`; prefer the span variant inside a batch."
  [^String current from_anchor to_anchor ^String replace]
  (let [res (resolve-anchor-edit-span current from_anchor to_anchor replace)]
    (if (:error res)
      res
      {:new-content (str (subs current 0 (:start res)) (:replacement res) (subs current (:end res)))
       :applied-line (:applied-line res)})))
