(ns com.blockether.vis.ext.foundation-core.editing.patch
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
(def ^:private env-id-marker    "*** Environment ID: ")
(def ^:private add-marker       "*** Add File: ")
(def ^:private delete-marker    "*** Delete File: ")
(def ^:private update-marker    "*** Update File: ")
(def ^:private move-marker      "*** Move to: ")
(def ^:private eof-marker       "*** End of File")
(def ^:private ctx-marker       "@@ ")
(def ^:private empty-ctx-marker "@@")

(defn looks-like-patch?
  "Does `s` look like a Codex patch envelope? Cheap lookahead used to
   dispatch v/patch between exact-replace and patch-envelope modes.
   Recognizes the heredoc-wrapped form (`<<'EOF'` ... `EOF`) as well
   so that LLM-produced commands like `apply_patch <<'EOF' ... EOF`
   still dispatch into envelope mode."
  [s]
  (boolean
    (and (string? s)
      (let [t (str/triml s)]
        (or (str/starts-with? t begin-marker)
          (some #(str/starts-with? t %)
            ["<<EOF\n" "<<'EOF'\n" "<<\"EOF\"\n"]))))))

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

(defn- strip-heredoc-wrapper
  "If `lines` is wrapped in `<<EOF` / `<<'EOF'` / `<<\"EOF\"` ... `EOF`
   markers (Codex' lenient heredoc form), strip them. Mirrors
   `check_patch_boundaries_lenient` in codex-rs/apply-patch/src/parser.rs."
  [lines]
  (if (< (count lines) 4)
    lines
    (let [first-line (str/trim (first lines))
          last-line  (str/trim (peek lines))]
      (if (and (contains? #{"<<EOF" "<<'EOF'" "<<\"EOF\""} first-line)
            (str/ends-with? last-line "EOF"))
        (subvec lines 1 (dec (count lines)))
        lines))))

(defn- check-boundaries
  "Strip optional heredoc wrapper, then *** Begin Patch / *** End Patch
   markers; return the inner lines. Mirrors
   `check_patch_boundaries_lenient` in Codex."
  [^String patch]
  (let [trimmed (str/trim patch)
        raw     (vec (str/split-lines trimmed))
        lines   (strip-heredoc-wrapper raw)]
    (when (empty? lines)
      (bad-patch! "patch text is empty"))
    (when-not (= (str/trim (first lines)) begin-marker)
      (bad-patch! "The first line of the patch must be '*** Begin Patch'"))
    (when-not (= (str/trim (peek lines)) end-marker)
      (bad-patch! "The last line of the patch must be '*** End Patch'"))
    (subvec lines 1 (dec (count lines)))))

(defn- strip-prefix [^String s ^String prefix]
  (when (and s (str/starts-with? s prefix))
    (subs s (count prefix))))

(defn- skip-environment-id
  "Codex' optional preamble: `*** Environment ID: <id>` line right after
   `*** Begin Patch`. Returns inner lines with the preamble removed (or
   unchanged if absent). Empty environment-id values are rejected to
   match `parse_environment_id_preamble`."
  [lines]
  (if-let [first-line (first lines)]
    (if-let [body (strip-prefix (str/triml first-line) env-id-marker)]
      (let [env-id (str/trim body)]
        (when (str/blank? env-id)
          (bad-patch! "apply_patch environment_id cannot be empty"))
        (subvec lines 1))
      lines)
    lines))

(defn- starts-marker?
  "Does `line` start any top-level marker? Used to terminate update
   hunks. Tolerates leading whitespace, matching Codex' `line.trim()`
   pre-check in `parse_one_hunk`."
  [^String line]
  (and (string? line)
    (str/starts-with? (str/triml line) "***")))

(defn- ctx-marker-line?
  "Is `line` a `@@` or `@@ <header>` context marker? Tolerates leading
   whitespace. Does NOT trim the contents; callers slice from the raw
   line so content lines (which legitimately start with space) are
   never mistaken for markers."
  [^String line]
  (let [t (str/triml line)]
    (or (= t empty-ctx-marker)
      (str/starts-with? t ctx-marker))))

(defn- eof-marker-line?
  [^String line]
  (= (str/trim line) eof-marker))

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
          (let [body (persistent! buf)]
            [{:op :add
              :path path
              :contents (if (pos? (count body))
                          (str (str/join "\n" body) "\n")
                          "")}
             i]))))))

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

          (eof-marker-line? line)
          (let [olds (persistent! old-lines)
                news (persistent! new-lines)]
            (if (and (zero? (count olds)) (zero? (count news)))
              (bad-hunk! "update hunk must contain at least one change line before *** End of File"
                (+ patch-line-number i))
              [{:change-context change-context
                :old-lines olds
                :new-lines news
                :end-of-file? true}
               (inc i)]))

          ;; new @@ context starts a new chunk for the caller
          (ctx-marker-line? line)
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
        ;; optional move-to (tolerant of leading whitespace)
        first-raw (when (< start n) (nth lines start))
        move-to   (when first-raw
                    (strip-prefix (str/triml first-raw) move-marker))
        body-start (if move-to (inc start) start)]
    (loop [i      body-start
           chunks (transient [])]
      (let [line (when (< i n) (nth lines i))
            trimmed (some-> line str/triml)]
        (cond
          (or (nil? line) (starts-marker? line))
          [{:op :update
            :path path
            :move-to (when move-to (str/trim move-to))
            :chunks (persistent! chunks)}
           i]

          (= trimmed empty-ctx-marker)
          (let [[chunk j] (parse-chunk lines (inc i) nil patch-line-number)]
            (recur j (conj! chunks chunk)))

          (str/starts-with? trimmed ctx-marker)
          (let [ctx (subs trimmed (count ctx-marker))
                [chunk j] (parse-chunk lines (inc i) ctx patch-line-number)]
            (recur j (conj! chunks chunk)))

          :else
          ;; implicit empty context — start chunk at this line
          (let [[chunk j] (parse-chunk lines i nil patch-line-number)]
            (recur j (conj! chunks chunk))))))))

(defn parse-patch
  "Parse a Codex `apply_patch` envelope string into
   `{:hunks [...] :environment-id ID-or-nil}`.

   Each hunk is one of:
     {:op :add    :path P :contents S}
     {:op :delete :path P}
     {:op :update :path P :move-to M-or-nil :chunks [Chunk...]}
   where Chunk = {:change-context CTX :old-lines [...] :new-lines [...]
                  :end-of-file? bool}.

   Tolerates: leading/trailing whitespace on every marker line, optional
   `*** Environment ID: <id>` preamble, and the heredoc-wrapped form
   (`<<'EOF'` ... `EOF`) used by gpt-4.1 / `local_shell`.

   Throws ex-info on malformed input."
  [^String patch]
  (let [boundary-stripped (check-boundaries patch)
        env-id (some-> (first boundary-stripped)
                 str/triml
                 (strip-prefix env-id-marker)
                 str/trim)
        inner (skip-environment-id boundary-stripped)
        n     (count inner)]
    (loop [i      0
           hunks  (transient [])]
      (if (>= i n)
        {:hunks (persistent! hunks)
         :environment-id (when-not (str/blank? env-id) env-id)}
        (let [raw-line (nth inner i)
              line     (str/triml raw-line)]
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

   Match strictness, in order: exact -> rstrip -> trim -> Unicode
   normalize -> relative-indent. Returns the start index or nil.
   For pass attribution use `seek-sequence-with-pass`."
  [lines pattern start eof?]
  (:start (seek-sequence-with-pass lines pattern start eof?)))

(defn split-content-lines
  "Split a file blob into a vec of lines, preserving empty trailing
   element behaviour consistent with `compute-update`. Public so the
   exact-replace path can share the same line view as the envelope."
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
  (let [original-lines (split-lines-preserving-empty original)]
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
        ;; done -- match Codex' behaviour: always end with a single
        ;; trailing newline regardless of `original`'s final-newline
        ;; status. (Codex pushes an empty trailing line then joins on
        ;; "\n", so the output invariably ends with newline.)
        (let [reps (persistent! replacements)
              new-lines (apply-replacements original-lines reps)
              new-content (str (str/join "\n" new-lines) "\n")]
          {:original original
           :new new-content
           :replacements reps})))))
