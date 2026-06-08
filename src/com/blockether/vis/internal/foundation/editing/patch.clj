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
     line-hash / lines->hashes              text -> 6-hex anchor / {ln hash}
     render-hashline-block / -range-block   tuples -> `<hash>| text` gutter
     indices-matching-hash / resolve-hash-edit  self-locating range replace"
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
  (loop [k 0]
    (if (= k (count pattern))
      true
      (if (cmp (nth lines (+ i k)) (nth pattern k)) (recur (inc k)) false))))
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
  [[:exact =] [:rstrip (fn [^String a ^String b] (= (str/trimr a) (str/trimr b)))]
   [:unicode (fn [^String a ^String b] (= (normalize-line a) (normalize-line b)))]
   [:trim (fn [^String a ^String b] (= (str/trim a) (str/trim b)))]])
(defn- leading-ws-count
  "Count of leading space/tab characters on `line`."
  ^long [^String line]
  (loop [i 0]
    (if (and (< i (count line)) (let [c (.charAt line i)] (or (= c \space) (= c \tab))))
      (recur (inc i))
      i)))
(defn- min-leading-indent
  "Smallest leading-whitespace count across non-blank `lines`, or nil if
   every line is blank. Used by relative-indent matcher."
  [lines]
  (let [counts (->> lines
                    (remove str/blank?)
                    (map leading-ws-count))]
    (when (seq counts) (apply min counts))))
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
  (let [plen (count pattern)
        llen (count lines)]
    (when (and (pos? plen) (<= plen llen))
      (let [p-indent (or (min-leading-indent pattern) 0)
            p-deindented (mapv #(de-indent p-indent %) pattern)
            end (- llen plen)]
        (loop [i start]
          (when (<= i end)
            (let [window (subvec lines i (+ i plen))
                  w-indent (or (min-leading-indent window) 0)
                  w-deindented (mapv #(de-indent w-indent %) window)]
              (if (= p-deindented w-deindented)
                {:start i, :indent-delta (- (long w-indent) (long p-indent))}
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
  (cond (empty? pattern) {:start start, :pass :exact}
        (> (count pattern) (count lines)) nil
        :else (let [plen (count pattern)
                    llen (count lines)
                    end (- llen plen)
                    eof-start (when eof? (max 0 (- llen plen)))
                    search-start (or eof-start start)
                    run-pass (fn [cmp]
                               (loop [i search-start]
                                 (cond (> i end) nil
                                       (match-at? lines pattern i cmp) i
                                       :else (recur (inc i)))))
                    [exact-pass-pairs trim-pass-pair] (split-at 3 fuzzy-passes)
                    ;; First sweep: exact, rstrip, unicode. Order matters.
                    tight-hit (reduce (fn [_ [pass-name cmp]]
                                        (when-let [hit (run-pass cmp)]
                                          (reduced {:start hit, :pass pass-name})))
                                nil
                                exact-pass-pairs)]
                (or tight-hit
                    ;; Second: structure-preserving relative-indent before the
                    ;; destructive `:trim` pass.
                    (when-let [{:keys [start indent-delta]}
                                 (seek-relative-indent lines pattern search-start)]
                      {:start start, :pass :relative-indent, :indent-delta indent-delta})
                    ;; Last resort: trim. Pass is named `:trim` here but corresponds
                    ;; to the 3rd entry in `fuzzy-passes` — see split-at above.
                    (let [[pass-name cmp] (first trim-pass-pair)]
                      (when-let [hit (run-pass cmp)] {:start hit, :pass pass-name}))))))
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
  (let [arr (.split s "\n" -1) v (vec arr)] (if (and (pos? (count v)) (= "" (peek v))) (pop v) v)))
(defn char-offset-at-line
  "Char offset in `content` where 0-based line `line-idx` starts.
   Returns `(count content)` if `line-idx` reaches past the last line.
   Public so the exact-replace path can map line indices back to char
   positions for substring splicing."
  ^long [^String content ^long line-idx]
  (loop [pos 0
         i 0]
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
  (cond (zero? delta) (vec lines)
        (pos? delta) (let [pad (apply str (repeat delta \space))]
                       (mapv (fn [^String l] (if (str/blank? l) l (str pad l))) lines))
        :else (let [strip (- (long delta))]
                (mapv (fn [^String l]
                        (if (str/blank? l) l (subs l (min strip (leading-ws-count l)))))
                  lines))))
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
  (let [lim (min off (count content))]
    (loop [i 0
           ln 0]
      (if (>= i lim) ln (recur (inc i) (if (= \newline (.charAt content i)) (inc ln) ln))))))
(defn tokenize-with-offsets
  "Vec of `[token ^long start ^long end]` for every maximal non-whitespace
   run in `s`. `start`/`end` are raw char offsets (end exclusive)."
  [^String s]
  (let [n (count s)]
    (loop [i 0
           acc (transient [])]
      (if (>= i n)
        (persistent! acc)
        (if (Character/isWhitespace (.charAt s i))
          (recur (inc i) acc)
          (let [j (loop [k i]
                    (if (and (< k n) (not (Character/isWhitespace (.charAt s k))))
                      (recur (inc k))
                      k))]
            (recur j (conj! acc [(subs s i j) (long i) (long j)]))))))))
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
  (let [ct (tokenize-with-offsets content)
        st-toks (mapv first (tokenize-with-offsets search))
        slen (count st-toks)]
    (when (pos? slen)
      (let [ctoks (mapv first ct)
            n (count ctoks)
            hits (loop [i 0
                        acc []]
                   (if (> i (- n slen))
                     acc
                     (recur (inc i)
                            (if (= st-toks (subvec ctoks i (+ i slen))) (conj acc i) acc))))]
        (when (seq hits)
          (let [i (first hits)
                [_ tstart _] (nth ct i)
                [_ _ tend] (nth ct (+ i slen -1))
                line-start (line-index-at content tstart)
                line-end (inc (line-index-at content (dec (long tend))))]
            {:line-start line-start, :line-end line-end, :occurrences (count hits)}))))))
;; =============================================================================
;; Hashline layer — the single, reusable hash-addressed editing surface.
;;
;; vis edits by CONTENT HASH, not by line number. This block owns every
;; reusable piece so callers never recompute the scheme:
;;
;;   line-hash                text            -> hash-width-hex anchor
;;   lines->hashes            [[ln text]…]    -> {ln hash}        (model map)
;;   render-hashline-block    [[ln text]…]    -> "<hash>│ text…"   (gutter)
;;   render-hashline-range-block ranges       -> headered gutter blocks
;;   indices-matching-hash    lines hash      -> [0-based idx …]
;;   resolve-hash-edit        content h h2 rep -> {:new-content}|{:error}
;;
;; The gutter the reader SEES is the edit address — `patch :from_hash`
;; resolves the same `line-hash` against live content. Line numbers live
;; only in the CAT summary / range headers (navigation), never the gutter:
;; hashes replace them.
;; =============================================================================
(def hash-width
  "Hex chars in a line's content hash. Anchors now carry the LINE NUMBER too
   (`<lineno>:<hash>` — see `line-anchor` / `lines->hashes`), so the hash no
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
   share a hash — a dup-line collision makes a `:from_hash` anchor
   ambiguous and `resolve-hash-edit` refuses it (caller falls back to
   `:search`).

   Hot path: runs once per line on every `cat` render AND every patch
   resolve. Formats with `Integer/toHexString` + a left-pad rather than
   java.util.Formatter, which benches ~1.5x slower; the trimmed
   `String/hashCode` is a JIT intrinsic so we lean on it instead of a
   hand loop."
  ^String [line]
  (let [h (int (bit-and (.hashCode (str/trim (str line))) hash-mask))
        hex (Integer/toHexString h)
        c (.length hex)]
    (if (< c (long hash-width)) (str (subs hash-zero-pad c) hex) hex)))
(def ^:const hashline-anchor-sep
  "Separator between the line number and the content hash inside an anchor
   (`<lineno>:<hash>`). A single char so the gutter stays narrow."
  ":")
(defn line-anchor
  "The editable anchor for a line: `<line-number>:<content-hash>` (e.g.
   `325:0e3`). The line number LOCATES the line; the hash VERIFIES its
   content. `patch :from_hash` parses this back via `resolve-hash-range`,
   matching the line number against live content and refusing if the hash no
   longer agrees (the line changed) or that content now lives far from the
   stated line (a misattributed / stale anchor). Two coordinates, so a single
   reused hash can no longer silently land an edit on the wrong line."
  [ln text]
  (str ln hashline-anchor-sep (line-hash text)))
(defn lines->hashes
  "`{line-number anchor}` map of every non-blank line in `tuples`, where each
   anchor is `<line-number>:<content-hash>` (`line-anchor`). The canonical
   `:hashes` payload `cat` returns — the SINGLE place it is built (read-file /
   read-file-ranges / tail-file / rg all route here). Blank lines are omitted:
   the model only ever sees anchors it can actually edit by. Line numbers come
   straight from the `[ln text]` tuples, so a windowed read (range / tail /
   by-hash) carries the file's real line numbers with NO second full-file pass
   — the `#N` file-wide-ordinal scheme (and its whole-file rescan) is gone now
   that the line number, not the hash, disambiguates duplicate lines."
  [tuples]
  (into {}
        (keep (fn [[ln s]]
                (when-not (str/blank? (str s))
                  [ln (line-anchor ln s)])))
        tuples))
(def ^:const hashline-gutter
  "Separator between the anchor and the line text in rendered output."
  "│ ")
(defn render-hashline-block
  "Render `[line-number text]` tuples as the canonical MODEL gutter
   `<line-number>:<hash>│ <text>`. Line numbers are right-aligned within the
   block; a blank line shows its line number with a blank hash slot so the `│`
   column stays aligned. Self-contained (derives hashes from the text), the
   single source of truth for the cat body across whole-file, range and tail
   reads. The gutter IS the edit address — `patch :from_hash` parses
   `<line-number>:<hash>` back against live content (`resolve-hash-range`)."
  [tuples]
  (let [tuples  (vec tuples)
        ln-w    (reduce (fn [w [ln _]] (max w (count (str ln)))) 1 tuples)
        blank-h (apply str (repeat (long hash-width) \space))]
    (->> tuples
         (map (fn [[ln s]]
                (let [ln-str (format (str "%" ln-w "s") (str ln))
                      h      (if (str/blank? (str s)) blank-h (line-hash s))]
                  (str ln-str hashline-anchor-sep h hashline-gutter s))))
         (str/join "\n"))))
(defn render-hashline-range-block
  "Render `:ranges` windows (`[{:range [start end] :lines [[ln text]…]}…]`)
   as `-- range S-E --` headers followed by the canonical hash gutter for
   each window. Same body format as `render-hashline-block`, so multi-range
   reads carry hash anchors exactly like a whole-file read."
  [ranges]
  (->> ranges
       (map (fn [{:keys [range lines]}]
              (let [[start end] range]
                (str "-- range " start
                     "-" end
                     " --" (when (seq lines) (str "\n" (render-hashline-block lines)))))))
       (str/join "\n\n")))
(defn render-lineno-block
  "Render `[line-number text]` tuples as a HUMAN line-number gutter
   `<ln>│ <text>`, line numbers right-aligned to the widest number in
   the block. Unlike `render-hashline-block` (the MODEL surface, whose
   gutter is the editable `:from_hash` anchor), this is the channel/TUI
   display surface: humans navigate by line number, not by content hash."
  [tuples]
  (let [tuples (vec tuples)
        width (reduce (fn [w [ln _]] (max w (count (str ln)))) 1 tuples)]
    (->> tuples
         (map (fn [[ln s]] (str (format (str "%" width "s") (str ln)) hashline-gutter s)))
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
                 (let [g (peek groups)
                       last-ln (when g (first (peek g)))]
                   (if (and last-ln (= ln (inc last-ln)))
                     (conj (pop groups) (conj g t))
                     (conj groups [t]))))
         [])
       (mapv (fn [g] {:range [(ffirst g) (first (peek g))], :lines g}))))
(defn- line-span->char-span
  "Convert a 0-based [line-start line-end) span to a [char-start char-end]
   substring span in `content`, keeping a trailing `\n` OUTSIDE the
   replaced region."
  [^String content ^long line-start ^long line-end]
  (let [char-start (char-offset-at-line content line-start)
        char-end-raw (char-offset-at-line content line-end)
        char-end (if (and (< char-end-raw (count content))
                          (pos? char-end-raw)
                          (= \newline (.charAt content (dec char-end-raw))))
                   (dec char-end-raw)
                   char-end-raw)]
    [char-start char-end]))
(defn indices-matching-hash
  "0-based indices of `lines` whose anchor matches `h`. A bare hash matches\n   every line sharing that content hash (so a duplicate is still ambiguous\n   and refused). A `hash#N` ordinal anchor matches the single Nth (1-based)\n   line within the identical-line group, or nothing if N is out of range."
  [lines h]
  (let [h (str h)
        hsh (.indexOf h "#")]
    (if (neg? hsh)
      (into [] (keep-indexed (fn [i l] (when (= h (line-hash l)) i))) lines)
      (let [base (subs h 0 hsh)
            ord (parse-long (subs h (inc hsh)))
            all (into [] (keep-indexed (fn [i l] (when (= base (line-hash l)) i))) lines)]
        (if (and ord (>= (long ord) 1) (<= (long ord) (count all)))
          [(nth all (dec (long ord)))]
          [])))))
(defn resolve-hash-range
  "Resolve `from_hash` (and `to_hash`, defaulting to `from_hash` for a single
   line) against the LIVE `current` content by recomputing `line-hash` per
   line — so the anchor lands on the right line even if it drifted since the
   `cat` read. Each hash must match EXACTLY one line; a dup-line collision is
   refused (caller falls back to `:search`). Returns `{:from-line N :to-line N}`
   (1-based, INCLUSIVE) or `{:error {:reason KW …}}`.

   Shared by `resolve-hash-edit` (WRITE — patch :from_hash) and the cat
   `:hash` READ path so both address lines by content identically."
  [^String current from_hash to_hash]
  (let [lines (split-content-lines current)
        to_hash (or to_hash from_hash)
        froms (indices-matching-hash lines from_hash)
        ;; Single-line range (to == from) is the common case — reuse the one
        ;; scan instead of hashing every line a second time.
        tos (if (= to_hash from_hash) froms (indices-matching-hash lines to_hash))]
    (cond (empty? froms) {:error {:reason :hash-not-found, :which :from, :hash (str from_hash)}}
          (> (count froms) 1) {:error {:reason :hash-ambiguous,
                                       :which :from,
                                       :hash (str from_hash),
                                       :lines (mapv inc froms)}}
          (empty? tos) {:error {:reason :hash-not-found, :which :to, :hash (str to_hash)}}
          (> (count tos) 1)
            {:error
               {:reason :hash-ambiguous, :which :to, :hash (str to_hash), :lines (mapv inc tos)}}
          (< (long (first tos)) (long (first froms))) {:error {:reason :hash-range-inverted,
                                                               :from-line (inc (first froms)),
                                                               :to-line (inc (first tos))}}
          :else {:from-line (inc (long (first froms))), :to-line (inc (long (first tos)))})))
(defn resolve-hash-edit-span
  "Resolve a content-addressed line-range edit to a CHAR SPAN against `current`,
   WITHOUT building new content: `{:start S :end E :replacement R :applied-line N}`
   or `{:error {:reason KW …}}`. Lets a multi-edit batch resolve every anchor
   against the ORIGINAL snapshot and splice all spans together atomically, so an
   earlier edit can't drift a later edit's hash/ordinal. `to_hash` defaults to
   `from_hash` (single line). Each hash must match EXACTLY one line; a dup-line
   collision is refused (use `:search`)."
  [^String current from_hash to_hash ^String replace]
  (let [res (resolve-hash-range current from_hash to_hash)]
    (if (:error res)
      res
      (let [line-start (dec (long (:from-line res)))
            line-end (long (:to-line res))
            [char-start char-end] (line-span->char-span current line-start line-end)
            matched-ends-nl? (and (> (long char-end) 0)
                                  (= \newline (.charAt current (dec (long char-end)))))
            replace-ends-nl? (str/ends-with? replace "\n")
            rewritten (if (and matched-ends-nl? (not replace-ends-nl?)) (str replace "\n") replace)]
        {:start char-start :end char-end :replacement rewritten :applied-line (inc line-start)}))))
(defn resolve-hash-edit
  "Content-addressed line-range replace returning full `{:new-content S
   :applied-line N}` (or `{:error …}`). Thin wrapper over
   `resolve-hash-edit-span`; prefer the span variant inside a batch."
  [^String current from_hash to_hash ^String replace]
  (let [res (resolve-hash-edit-span current from_hash to_hash replace)]
    (if (:error res)
      res
      {:new-content (str (subs current 0 (:start res)) (:replacement res) (subs current (:end res))),
       :applied-line (:applied-line res)})))
