(ns com.blockether.vis.ext.foundation-core.editing.patch
  "Fuzzy line-based matching toolkit used by `v/patch` exact-replace.

   This namespace USED to also carry the Codex `apply_patch` envelope
   parser, but envelope mode was retired \u2014 we consolidated to a single
   per-intent mutation surface (`v/patch` exact-replace + `v/write` /
   `v/move` / `v/delete`). The fuzzy matcher stays because it is what
   lets multi-line `:search` blocks tolerate whitespace and typographic
   drift before they fall through to `:no-match`.

   Public surface (all pure):
     seek-sequence            lines pattern start eof? -> start-index or nil
     seek-sequence-with-pass  lines pattern start eof?
                              -> {:start :pass :indent-delta?} or nil
     split-content-lines      string -> vec of lines (no trailing \\n element)
     char-offset-at-line      content line-idx -> char offset
     apply-indent-delta       delta lines -> re-indented lines"
  (:require
   [clojure.string :as str]))

;; =============================================================================
;; seek-sequence \u2014 fuzzy line matcher
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
  "Fold typographic dashes/quotes/spaces to ASCII. Does NOT trim \u2014 the
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
   left alone (mirrors Python's textwrap.dedent semantics \u2014 blank lines
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
   the same code at a different level \u2014 common LLM whitespace quirk.

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
     :exact \u2192 :rstrip \u2192 :unicode \u2192 :relative-indent \u2192 :trim
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
        ;; to the 3rd entry in `fuzzy-passes` \u2014 see split-at above.
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
