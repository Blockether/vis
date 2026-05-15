(ns com.blockether.vis.ext.foundation.editing.patch
  "Codex `apply_patch` compatible patch envelope.

   Parses the OpenAI Codex patch language into pure data, computes line
   replacements with the same fuzzy-match policy (exact / rstrip / trim /
   Unicode-punctuation normalize), and produces planned file changes
   without performing IO. The applier in `core.clj` validates the full
   plan before any write, preserving Vis' all-or-nothing safety.

   Public surface (all pure):
     parse-patch    - string -> {:hunks [...]}
     seek-sequence  - file-lines, pattern -> start-index or nil
     compute-update - {:original :hunks} -> new-content + per-hunk records
     plan-changes   - hunks + read-fn -> per-file plan map vector"
  (:require
   [clojure.string :as str]))

;; =============================================================================
;; Markers (mirrors codex-rs/apply-patch/src/parser.rs)
;; =============================================================================

(def ^:private begin-marker     "*** Begin Patch")
(def ^:private end-marker       "*** End Patch")
(def ^:private add-marker       "*** Add File: ")
(def ^:private delete-marker    "*** Delete File: ")
(def ^:private update-marker    "*** Update File: ")
(def ^:private move-marker      "*** Move to: ")
(def ^:private eof-marker       "*** End of File")
(def ^:private ctx-marker       "@@ ")
(def ^:private empty-ctx-marker "@@")

(defn looks-like-patch?
  "Does `s` look like a Codex patch envelope? Cheap lookahead used to
   dispatch v/patch between exact-replace and patch-envelope modes."
  [s]
  (and (string? s)
    (let [t (str/triml s)]
      (str/starts-with? t begin-marker))))

;; =============================================================================
;; Parser
;; =============================================================================
;;
;; Grammar (matches codex-rs/apply-patch/src/parser.rs):
;;
;;   patch       = "*** Begin Patch" NEWLINE { hunk } "*** End Patch" NEWLINE?
;;   hunk        = add | delete | update
;;   add         = "*** Add File: " path NEWLINE { "+" line NEWLINE }
;;   delete      = "*** Delete File: " path NEWLINE
;;   update      = "*** Update File: " path NEWLINE [ "*** Move to: " path NEWLINE ]
;;                 { change }
;;   change      = (change-context | change-line)+ [ "*** End of File" NEWLINE ]
;;   change-line = (" " | "-" | "+") text NEWLINE
;;
;; The parser is lenient about surrounding whitespace and a trailing EOF
;; without final newline, matching Codex' lenient mode.

(defn- bad-patch! [msg]
  (throw (ex-info (str "invalid patch: " msg)
           {:type :ext.foundation.editing/invalid-patch
            :message msg})))

(defn- bad-hunk! [msg line-number]
  (throw (ex-info (str "invalid patch hunk at line " line-number ", " msg)
           {:type :ext.foundation.editing/invalid-hunk
            :message msg
            :line-number line-number})))

(defn- check-boundaries
  "Strip *** Begin Patch / *** End Patch and return the inner lines.
   Mirrors `check_patch_boundaries_lenient` in Codex."
  [^String patch]
  (let [trimmed (str/trim patch)
        lines   (vec (str/split-lines trimmed))]
    (when (empty? lines)
      (bad-patch! "patch text is empty"))
    (when-not (= (str/triml (first lines)) begin-marker)
      (bad-patch! (str "expected first line to be '" begin-marker "'")))
    (when-not (= (str/triml (peek lines)) end-marker)
      (bad-patch! (str "expected last line to be '" end-marker "'")))
    (subvec lines 1 (dec (count lines)))))

(defn- strip-prefix [^String s ^String prefix]
  (when (and s (str/starts-with? s prefix))
    (subs s (count prefix))))

(defn- starts-marker?
  "Does `line` start any top-level marker? Used to terminate update hunks."
  [^String line]
  (and (string? line)
    (str/starts-with? line "***")))

(defn- parse-add
  [path lines start]
  ;; consume contiguous `+` lines as the file body
  (let [n (count lines)]
    (loop [i      start
           buf    (transient [])]
      (let [line (when (< i n) (nth lines i))]
        (cond
          (and line (str/starts-with? line "+"))
          (recur (inc i) (conj! buf (subs line 1)))

          :else
          [{:op :add
            :path path
            :contents (str (str/join "\n" (persistent! buf))
                        (when (pos? (count (persistent! buf))) "\n"))}
           i])))))

(defn- parse-chunk
  "Parse one update chunk starting at `start` (line index inside `lines`).
   Returns [chunk next-index]. A chunk runs until the next `@@` context
   line, the next top-level marker, or EOF. An `*** End of File` line
   inside the chunk terminates it with `:end-of-file? true`."
  [lines start change-context patch-line-number]
  (let [n (count lines)]
    (loop [i         start
           old-lines (transient [])
           new-lines (transient [])
           eof?      false]
      (let [line (when (< i n) (nth lines i))]
        (cond
          (or (nil? line) (starts-marker? line))
          [{:change-context change-context
            :old-lines (persistent! old-lines)
            :new-lines (persistent! new-lines)
            :end-of-file? eof?}
           i]

          (= line eof-marker)
          (if (and (= 0 (count (persistent! old-lines)))
                (= 0 (count (persistent! new-lines))))
            (bad-hunk! "update hunk must contain at least one change line before *** End of File"
              (+ patch-line-number i))
            [{:change-context change-context
              :old-lines (persistent! old-lines)
              :new-lines (persistent! new-lines)
              :end-of-file? true}
             (inc i)])

          ;; new @@ context starts a new chunk for the caller
          (or (= line empty-ctx-marker)
            (str/starts-with? line ctx-marker))
          [{:change-context change-context
            :old-lines (persistent! old-lines)
            :new-lines (persistent! new-lines)
            :end-of-file? eof?}
           i]

          :else
          (let [c (when (pos? (count line)) (.charAt ^String line 0))
                body (if (zero? (count line)) "" (subs line 1))]
            (case (str c)
              " " (recur (inc i)
                    (conj! old-lines body)
                    (conj! new-lines body)
                    eof?)
              "-" (recur (inc i)
                    (conj! old-lines body)
                    new-lines
                    eof?)
              "+" (recur (inc i)
                    old-lines
                    (conj! new-lines body)
                    eof?)
              ;; treat blank line as empty context
              ""  (recur (inc i)
                    (conj! old-lines "")
                    (conj! new-lines "")
                    eof?)
              (bad-hunk! (str "unexpected line in update hunk: " (pr-str line))
                (+ patch-line-number i)))))))))

(defn- parse-update
  [path lines start patch-line-number]
  (let [n (count lines)
        ;; optional move-to
        first-line (when (< start n) (nth lines start))
        move-to    (when first-line (strip-prefix first-line move-marker))
        body-start (if move-to (inc start) start)]
    (loop [i      body-start
           chunks (transient [])]
      (let [line (when (< i n) (nth lines i))]
        (cond
          (or (nil? line) (starts-marker? line))
          [{:op :update
            :path path
            :move-to (when move-to (str/trim move-to))
            :chunks (persistent! chunks)}
           i]

          (= line empty-ctx-marker)
          (let [[chunk j] (parse-chunk lines (inc i) nil patch-line-number)]
            (recur j (conj! chunks chunk)))

          (str/starts-with? line ctx-marker)
          (let [ctx (subs line (count ctx-marker))
                [chunk j] (parse-chunk lines (inc i) ctx patch-line-number)]
            (recur j (conj! chunks chunk)))

          :else
          ;; implicit empty context — start chunk at this line
          (let [[chunk j] (parse-chunk lines i nil patch-line-number)]
            (recur j (conj! chunks chunk))))))))

(defn parse-patch
  "Parse a Codex `apply_patch` envelope string into `{:hunks [...]}`.

   Each hunk is one of:
     {:op :add    :path P :contents S}
     {:op :delete :path P}
     {:op :update :path P :move-to M-or-nil :chunks [Chunk...]}
   where Chunk = {:change-context CTX :old-lines [...] :new-lines [...]
                  :end-of-file? bool}.

   Throws ex-info on malformed input."
  [^String patch]
  (let [inner (check-boundaries patch)
        n     (count inner)]
    (loop [i      0
           hunks  (transient [])]
      (if (>= i n)
        {:hunks (persistent! hunks)}
        (let [line (nth inner i)]
          (cond
            (str/blank? line)
            (recur (inc i) hunks)

            (str/starts-with? line add-marker)
            (let [path (str/trim (subs line (count add-marker)))
                  [hunk j] (parse-add path inner (inc i))]
              (recur j (conj! hunks hunk)))

            (str/starts-with? line delete-marker)
            (let [path (str/trim (subs line (count delete-marker)))]
              (recur (inc i) (conj! hunks {:op :delete :path path})))

            (str/starts-with? line update-marker)
            (let [path (str/trim (subs line (count update-marker)))
                  [hunk j] (parse-update path inner (inc i) (+ 2 i))]
              (recur j (conj! hunks hunk)))

            :else
            (bad-hunk! (str "expected file header, got: " (pr-str line))
              (+ 2 i))))))))

;; =============================================================================
;; seek-sequence — fuzzy line matcher (parity with codex seek_sequence.rs)
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
  "Trim + fold typographic dashes/quotes/spaces to ASCII. Last-resort
   fuzzy comparison key — see Codex `seek_sequence::normalise`."
  [^String s]
  (let [trimmed (str/trim s)
        sb (StringBuilder.)]
    (doseq [^Character ch trimmed]
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

(defn seek-sequence
  "Find `pattern` (a vec of strings) inside `lines` (vec of strings)
   starting at `start`. When `eof?` is true, first try the EOF position
   then fall back to `start`.

   Match strictness, in order: exact -> rstrip -> trim -> Unicode
   normalize. Returns the start index or nil."
  [lines pattern start eof?]
  (cond
    (empty? pattern) start

    (> (count pattern) (count lines)) nil

    :else
    (let [plen (count pattern)
          llen (count lines)
          end  (- llen plen)
          eof-start (when eof? (max 0 (- llen plen)))
          search-start (or eof-start start)
          cmps [=
                (fn [^String a ^String b] (= (str/trimr a) (str/trimr b)))
                (fn [^String a ^String b] (= (str/trim a)  (str/trim b)))
                (fn [^String a ^String b] (= (normalize-line a) (normalize-line b)))]]
      (loop [pass cmps]
        (if-let [cmp (first pass)]
          (let [hit (loop [i search-start]
                      (cond
                        (> i end) nil
                        (match-at? lines pattern i cmp) i
                        :else (recur (inc i))))]
            (or hit (recur (next pass))))
          nil)))))

;; =============================================================================
;; compute-update — produce new file content from chunks (parity with
;; codex-rs/apply-patch/src/lib.rs::derive_new_contents_from_chunks)
;; =============================================================================

(defn- split-lines-preserving-empty
  "Like `(.split s \"\\n\" -1)` but returns a vec. Trailing empty
   element (from final newline) is dropped, matching Codex' behavior."
  [^String s]
  (let [arr (.split s "\n" -1)
        v   (vec arr)]
    (if (and (pos? (count v)) (= "" (peek v)))
      (pop v)
      v)))

(defn- apply-replacements
  "Apply `[[start old-len new-lines] ...]` to `lines` in descending order."
  [lines replacements]
  (let [sorted (vec (sort-by first replacements))]
    (loop [out lines
           ;; iterate in descending order
           idx (dec (count sorted))]
      (if (neg? idx)
        out
        (let [[start old-len new-segment] (nth sorted idx)
              before (subvec out 0 (min start (count out)))
              after  (subvec out (min (+ start old-len) (count out)))]
          (recur (vec (concat before new-segment after))
            (dec idx)))))))

(defn compute-update
  "Given `:original` content string and `:chunks` (from a parsed update
   hunk), compute the new file content. Throws ex-info if any chunk's
   context or old lines cannot be located.

   Returns {:original :new :replacements}, where :replacements records
   the per-chunk plan for diagnostics."
  [{:keys [original chunks path]}]
  (let [original-lines (split-lines-preserving-empty original)
        original-ends-newline? (or (str/blank? original)
                                 (str/ends-with? original "\n"))]
    (loop [chunks chunks
           line-index 0
           replacements (transient [])]
      (if-let [chunk (first chunks)]
        (let [{:keys [change-context old-lines new-lines end-of-file?]} chunk
              ;; advance past change-context if any
              ctx-idx (when change-context
                        (seek-sequence original-lines [change-context] line-index false))
              _ (when (and change-context (nil? ctx-idx))
                  (throw (ex-info (str "failed to find context '" change-context "' in " path)
                           {:type :ext.foundation.editing/patch-context-not-found
                            :path path
                            :change-context change-context})))
              line-index (if ctx-idx (inc (long ctx-idx)) line-index)]
          (cond
            (empty? old-lines)
            ;; pure addition at EOF
            (let [insertion-idx (count original-lines)]
              (recur (next chunks)
                line-index
                (conj! replacements [insertion-idx 0 (vec new-lines)])))

            :else
            (let [pat-orig (vec old-lines)
                  new-orig (vec new-lines)
                  [pat new-seg]
                  (loop [pat pat-orig new-seg new-orig]
                    ;; Codex: if pattern ends with "" sentinel and direct
                    ;; search fails, retry without trailing empties.
                    (let [hit (seek-sequence original-lines pat line-index end-of-file?)]
                      (cond
                        hit [pat new-seg]
                        (and (pos? (count pat)) (= "" (peek pat)))
                        (recur (pop pat)
                          (if (and (pos? (count new-seg)) (= "" (peek new-seg)))
                            (pop new-seg)
                            new-seg))
                        :else [pat new-seg])))
                  hit (seek-sequence original-lines pat line-index end-of-file?)]
              (when (nil? hit)
                (throw (ex-info (str "failed to find expected lines in " path)
                         {:type :ext.foundation.editing/patch-old-lines-not-found
                          :path path
                          :old-lines old-lines})))
              (recur (next chunks)
                (+ (long hit) (count pat))
                (conj! replacements [hit (count pat) new-seg])))))
        ;; done
        (let [reps (persistent! replacements)
              new-lines (apply-replacements original-lines reps)
              new-content (str (str/join "\n" new-lines)
                            (when (or (seq new-lines) original-ends-newline?) "\n"))]
          {:original original
           :new new-content
           :replacements reps})))))
