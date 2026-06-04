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
  (:require
   [clojure.string :as str]))

;; =============================================================================
;; seek-sequence — fuzzy line matcher
;; =============================================================================

(def ^:private dash-codepoints
  #{0x2010 0x2011 0x2012 0x2013 0x2014 0x2015 0x2212})

(def ^:private single-quote-codepoints
  #{0x2018 0x2019 0x201A 0x201B})

(def ^:private double-quote-codepoints
  #{0x201C 0x201D 0x201E 0x201F})

(def ^:private space-codepoints
  #{0x00A0 0x2002 0x2003 0x2004 0x2005 0x2006 0x2007 0x2008 0x2009 0x200A
    0x202F 0x205F 0x3000})

(defn- normalize-line
  "Fold typographic dashes/quotes/spaces to ASCII. Does NOT trim — the
   `:unicode` pass is intentionally single-purpose so it stays orthogonal
   to the `:trim` / `:rstrip` passes (and so the destructive `:trim` pass
   keeps running last)."
  [^String s]
  (let [sb (StringBuilder.)]
    (doseq [^Character ch s]
      (let [cp (int ch)]
        (cond
          (contains? dash-codepoints cp)         (.append sb \-)
          (contains? single-quote-codepoints cp) (.append sb \')
          (contains? double-quote-codepoints cp) (.append sb \")
          (contains? space-codepoints cp)        (.append sb \space)
          :else                                  (.append sb ch))))
    (.toString sb)))

(defn- match-at?
  [lines pattern i cmp]
  (loop [k 0]
    (if (= k (count pattern))
      true
      (if (cmp (nth lines (+ i k)) (nth pattern k))
        (recur (inc k))
        false))))

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
  [[:exact   =]
   [:rstrip  (fn [^String a ^String b] (= (str/trimr a) (str/trimr b)))]
   [:unicode (fn [^String a ^String b] (= (normalize-line a) (normalize-line b)))]
   [:trim    (fn [^String a ^String b] (= (str/trim a)  (str/trim b)))]])

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
    (when (seq counts)
      (apply min counts))))

(defn- de-indent
  "Strip `n` leading chars from `line` if it has them. Blank lines are
   left alone (mirrors Python's textwrap.dedent semantics — blank lines
   don't constrain indent)."
  [^long n ^String line]
  (cond
    (str/blank? line) line
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
                {:start i :indent-delta (- (long w-indent) (long p-indent))}
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
  (cond
    (empty? pattern) {:start start :pass :exact}

    (> (count pattern) (count lines)) nil

    :else
    (let [plen (count pattern)
          llen (count lines)
          end  (- llen plen)
          eof-start (when eof? (max 0 (- llen plen)))
          search-start (or eof-start start)
          run-pass (fn [cmp]
                     (loop [i search-start]
                       (cond
                         (> i end) nil
                         (match-at? lines pattern i cmp) i
                         :else (recur (inc i)))))
          [exact-pass-pairs trim-pass-pair] (split-at 3 fuzzy-passes)
          ;; First sweep: exact, rstrip, unicode. Order matters.
          tight-hit (reduce
                      (fn [_ [pass-name cmp]]
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
  (let [arr (.split s "\n" -1)
        v   (vec arr)]
    (if (and (pos? (count v)) (= "" (peek v)))
      (pop v)
      v)))

(defn char-offset-at-line
  "Char offset in `content` where 0-based line `line-idx` starts.
   Returns `(count content)` if `line-idx` reaches past the last line.
   Public so the exact-replace path can map line indices back to char
   positions for substring splicing."
  ^long [^String content ^long line-idx]
  (loop [pos 0 i 0]
    (if (= i line-idx)
      pos
      (let [nl (str/index-of content "\n" pos)]
        (if nl
          (recur (inc (long nl)) (inc i))
          (count content))))))

(defn apply-indent-delta
  "Re-indent `lines` by `delta` leading spaces (positive adds, negative
   strips). Blank lines untouched. Used by exact-replace when a fuzzy
   :relative-indent hit fires and the `replace` payload must follow the
   file's actual indentation rather than the SEARCH block's."
  [delta lines]
  (cond
    (zero? delta) (vec lines)
    (pos? delta) (let [pad (apply str (repeat delta \space))]
                   (mapv (fn [^String l]
                           (if (str/blank? l) l (str pad l)))
                     lines))
    :else (let [strip (- (long delta))]
            (mapv (fn [^String l]
                    (if (str/blank? l)
                      l
                      (subs l (min strip (leading-ws-count l)))))
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
    (loop [i 0 ln 0]
      (if (>= i lim)
        ln
        (recur (inc i) (if (= \newline (.charAt content i)) (inc ln) ln))))))

(defn tokenize-with-offsets
  "Vec of `[token ^long start ^long end]` for every maximal non-whitespace
   run in `s`. `start`/`end` are raw char offsets (end exclusive)."
  [^String s]
  (let [n (count s)]
    (loop [i 0 acc (transient [])]
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
  (let [ct       (tokenize-with-offsets content)
        st-toks  (mapv first (tokenize-with-offsets search))
        slen     (count st-toks)]
    (when (pos? slen)
      (let [ctoks (mapv first ct)
            n     (count ctoks)
            hits  (loop [i 0 acc []]
                    (if (> i (- n slen))
                      acc
                      (recur (inc i)
                        (if (= st-toks (subvec ctoks i (+ i slen)))
                          (conj acc i)
                          acc))))]
        (when (seq hits)
          (let [i           (first hits)
                [_ tstart _] (nth ct i)
                [_ _ tend]   (nth ct (+ i slen -1))
                line-start   (line-index-at content tstart)
                line-end     (inc (line-index-at content (dec (long tend))))]
            {:line-start line-start
             :line-end   line-end
             :occurrences (count hits)}))))))

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
;; The gutter the reader SEES is the edit address — `patch :from-hash`
;; resolves the same `line-hash` against live content. Line numbers live
;; only in the CAT summary / range headers (navigation), never the gutter:
;; hashes replace them.
;; =============================================================================

(def hash-width
  "Hex chars per line anchor. Measured knee of the collision-vs-token
   curve (loss = usable lines that share a truncated hash and so lose
   their `:from-hash` anchor):

     width  patch.clj(499)  core.clj(3152)  render.clj(4611)
       3        16.7%          55.4%           66.2%
       4         7.9%          22.5%           19.6%
       5         7.9%          19.5%           15.0%
       6         7.9%          19.5%           14.7%

   3 is catastrophic — most lines collide and become un-hash-editable.
   6 == 5 == 4 on normal files (<=~500 lines) and only ~3-5% better on
   3-4k-line files (range-read those, which shrinks the window and the
   collisions with it). So 4 (16-bit, 65536 buckets) is the sweet spot —
   it costs a third fewer tokens than 6 in the gutter AND the `:hashes`
   payload. Bump this and the mask/pad/gutter-width follow automatically.
   (The residual loss is mostly GENUINE duplicate lines — closing
   parens, common idioms — unaddressable by hash at any width.)"
  4)

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
   share a hash — a dup-line collision makes a `:from-hash` anchor
   ambiguous and `resolve-hash-edit` refuses it (caller falls back to
   `:search`).

   Hot path: runs once per line on every `cat` render AND every patch
   resolve. Formats with `Integer/toHexString` + a left-pad rather than
   java.util.Formatter, which benches ~1.5x slower; the trimmed
   `String/hashCode` is a JIT intrinsic so we lean on it instead of a
   hand loop."
  ^String [line]
  (let [h   (int (bit-and (.hashCode (str/trim (str line))) hash-mask))
        hex (Integer/toHexString h)
        c   (.length hex)]
    (if (< c (long hash-width)) (str (subs hash-zero-pad c) hex) hex)))

(defn- hashed-rows
  "Single hashing pass over `[line-number text]` tuples. Returns
   `[rows freqs]` where `rows` is a vec of `[line-number text hash]`
   (`line-hash` computed EXACTLY ONCE per line) and `freqs` is
   `{hash occurrence-count}`. Shared by `lines->hashes` and
   `render-hashline-block` so neither recomputes the hash 3x."
  [tuples]
  (let [rows  (mapv (fn [[ln s]] [ln s (line-hash s)]) tuples)
        freqs (persistent!
                (reduce (fn [m r]
                          (let [h (nth r 2)]
                            (assoc! m h (unchecked-inc (long (get m h 0))))))
                  (transient {}) rows))]
    [rows freqs]))

(defn- row-usable?
  "A line is a USABLE `:from-hash` anchor only when it is non-blank AND its
   hash is unique in the set — the only lines `resolve-hash-edit` accepts.
   `row` is a `[line-number text hash]` from `hashed-rows`."
  [row freqs]
  (and (not (str/blank? (nth row 1)))
    (= 1 (long (get freqs (nth row 2) 0)))))

(defn lines->hashes
  "`{line-number hash}` map of the USABLE anchors in `tuples` (non-blank
   and unique within the set). The canonical `:hashes` payload `cat`
   returns — the SINGLE place it is built (read-file / read-file-ranges /
   tail-file all route here). Ambiguous and blank lines are intentionally
   omitted: the model only ever sees hashes it can actually edit by."
  [tuples]
  (let [[rows freqs] (hashed-rows tuples)]
    (into {}
      (keep (fn [row] (when (row-usable? row freqs) [(nth row 0) (nth row 2)])))
      rows)))

(def ^:const hashline-gutter
  "Separator between the hash anchor and the line text in rendered output."
  "│ ")

(def ^:private hashline-blank-gutter
  "Aligned placeholder (`hash-width` spaces) for lines that are NOT usable
   hash anchors — keeps the `│` column aligned with hashed rows."
  (apply str (repeat (long hash-width) \space)))

(defn render-hashline-block
  "Render `[line-number text]` tuples as the canonical gutter
   `<hash>│ <text>`. A hash is shown ONLY on usable anchors (non-blank,
   unique within the block, see `usable-anchor?`); every other line gets
   an aligned blank gutter, so the reader sees a hash exactly where one
   can be edited by hash — no noise on blank/duplicate lines. Self-
   contained (derives hashes from the text), the single source of truth
   for the cat body across whole-file, range window and tail reads."
  [tuples]
  (let [[rows freqs] (hashed-rows tuples)]
    (->> rows
      (map (fn [row]
             (let [s (nth row 1)]
               (if (row-usable? row freqs)
                 (str (nth row 2) hashline-gutter s)
                 (str hashline-blank-gutter hashline-gutter s)))))
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
             (str "-- range " start "-" end " --"
               (when (seq lines)
                 (str "\n" (render-hashline-block lines)))))))
    (str/join "\n\n")))

(defn render-lineno-block
  "Render `[line-number text]` tuples as a HUMAN line-number gutter
   `<ln>│ <text>`, line numbers right-aligned to the widest number in
   the block. Unlike `render-hashline-block` (the MODEL surface, whose
   gutter is the editable `:from-hash` anchor), this is the channel/TUI
   display surface: humans navigate by line number, not by content hash."
  [tuples]
  (let [tuples (vec tuples)
        width  (reduce (fn [w [ln _]] (max w (count (str ln)))) 1 tuples)]
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
             (str "-- range " start "-" end " --"
               (when (seq lines)
                 (str "\n" (render-lineno-block lines)))))))
    (str/join "\n\n")))

(defn- line-span->char-span
  "Convert a 0-based [line-start line-end) span to a [char-start char-end]
   substring span in `content`, keeping a trailing `\n` OUTSIDE the
   replaced region."
  [^String content ^long line-start ^long line-end]
  (let [char-start   (char-offset-at-line content line-start)
        char-end-raw (char-offset-at-line content line-end)
        char-end     (if (and (< char-end-raw (count content))
                           (pos? char-end-raw)
                           (= \newline (.charAt content (dec char-end-raw))))
                       (dec char-end-raw)
                       char-end-raw)]
    [char-start char-end]))

(defn indices-matching-hash
  "0-based indices of `lines` whose `line-hash` equals `h`."
  [lines h]
  (let [h (str h)]
    (into [] (keep-indexed (fn [i l] (when (= h (line-hash l)) i))) lines)))

(defn resolve-hash-range
  "Resolve `from-hash` (and `to-hash`, defaulting to `from-hash` for a single
   line) against the LIVE `current` content by recomputing `line-hash` per
   line — so the anchor lands on the right line even if it drifted since the
   `cat` read. Each hash must match EXACTLY one line; a dup-line collision is
   refused (caller falls back to `:search`). Returns `{:from-line N :to-line N}`
   (1-based, INCLUSIVE) or `{:error {:reason KW …}}`.

   Shared by `resolve-hash-edit` (WRITE — patch :from-hash) and the cat
   `:hash` READ path so both address lines by content identically."
  [^String current from-hash to-hash]
  (let [lines   (split-content-lines current)
        to-hash (or to-hash from-hash)
        froms   (indices-matching-hash lines from-hash)
        ;; Single-line range (to == from) is the common case — reuse the one
        ;; scan instead of hashing every line a second time.
        tos     (if (= to-hash from-hash)
                  froms
                  (indices-matching-hash lines to-hash))]
    (cond
      (empty? froms)
      {:error {:reason :hash-not-found :which :from :hash (str from-hash)}}
      (> (count froms) 1)
      {:error {:reason :hash-ambiguous :which :from :hash (str from-hash)
               :lines (mapv inc froms)}}
      (empty? tos)
      {:error {:reason :hash-not-found :which :to :hash (str to-hash)}}
      (> (count tos) 1)
      {:error {:reason :hash-ambiguous :which :to :hash (str to-hash)
               :lines (mapv inc tos)}}
      (< (long (first tos)) (long (first froms)))
      {:error {:reason :hash-range-inverted
               :from-line (inc (first froms)) :to-line (inc (first tos))}}
      :else
      {:from-line (inc (long (first froms)))
       :to-line   (inc (long (first tos)))})))

(defn resolve-hash-edit
  "Content-addressed line-range replace. Resolves `from-hash` (and
   `to-hash`, defaulting to `from-hash` for a single line) against the
   LIVE `current` content by recomputing `line-hash` per line — so the
   edit lands on the right line even if it drifted since the `cat` read.
   Each hash must match EXACTLY one line; a dup-line collision is refused
   (use `:search`). Returns `{:new-content S :applied-line N}` or
   `{:error {:reason KW …}}`."
  [^String current from-hash to-hash ^String replace]
  (let [res (resolve-hash-range current from-hash to-hash)]
    (if (:error res)
      res
      (let [line-start (dec (long (:from-line res)))
            line-end   (long (:to-line res))
            [char-start char-end] (line-span->char-span current line-start line-end)
            matched-ends-nl? (and (> (long char-end) 0)
                               (= \newline (.charAt current (dec (long char-end)))))
            replace-ends-nl? (str/ends-with? replace "\n")
            rewritten (if (and matched-ends-nl? (not replace-ends-nl?))
                        (str replace "\n") replace)]
        {:new-content (str (subs current 0 char-start)
                        rewritten
                        (subs current char-end))
         :applied-line (inc line-start)}))))
