(ns com.blockether.vis.ext.channel-tui.render-ir
  "TUI walker over canonical answer-IR (`com.blockether.vis.internal.render/->ast`).

   Pure data: IR -> vector of lines. Each line is a vector of styled
   runs. The screen layer turns runs into ANSI / Lanterna cells.

   Why a dedicated walker (and not `ir/render :plain`):
   - We need styling metadata per character run (bold / italic / inline
     code background / link target) so the TUI can paint colours and
     attach click + selection regions.
   - We need word-wrap at an arbitrary terminal width with hanging
     indent inside lists and quotes.
   - We need stable per-node identifiers so selection / hover / click
     can map a glyph back to its IR node.

   Canonical-IR invariants we rely on (see internal.render docstring):
   - text only inside `:span` / raw bodies of `:code` / `:c` / `:kbd`;
   - no '\\n' inside `:span`;
   - hard breaks are explicit `[:br {}]`;
   - attrs map present on every vector node.

   Run shape:
     {:text   String          ; never empty, never contains '\\n'
      :style  #{:bold :italic :code :dim :link :heading :marker :quote}
      :href   String?         ; present iff :link in style
      :node   any?}           ; opaque node identity for click/select

   Public API:
     (ir->lines ir width)             ; total walker
     (ir->lines ir width opts)
   Opts:
     :heading-prefix? bool   ; render '#'-style markers (default false; bold suffices)
     :code-fence?     bool   ; render ``` lines around code blocks (default false)
     :max-lines       int    ; hard cap (default unlimited)"
  (:require
   [clojure.string :as str]
   [com.blockether.vis.ext.channel-tui.primitives :as p]
   [com.blockether.vis.internal.render :as ir]))

(set! *warn-on-reflection* true)

;; =============================================================================
;; Helpers
;; =============================================================================

(defn- node-tag      [n] (when (vector? n) (first n)))
(defn- node-attrs    [n] (if (and (vector? n) (map? (nth n 1 nil))) (nth n 1) {}))
(defn- node-children [n] (if (and (vector? n) (map? (nth n 1 nil))) (drop 2 n) (rest n)))
(defn- raw-body      [n] (or (some #(when (string? %) %) (node-children n)) ""))

(defn- empty-line []
  {:runs []})

(defn- run-width ^long [{:keys [text]}]
  (count (or text "")))

(defn- line-width ^long [line]
  (reduce + 0 (map run-width (:runs line))))

(defn- line-blank? [line]
  (every? (fn [{:keys [text]}] (str/blank? (or text ""))) (:runs line)))

;; =============================================================================
;; Inline → flat run sequence
;; =============================================================================

(declare inlines->runs)

(defn- inline->runs
  "Canonicalize-friendly inline tag dispatch. `style` is a set of
   keywords accumulated from ancestors. `href` propagates from `:a`."
  [node style href]
  (let [tag      (node-tag node)
        attrs    (node-attrs node)
        children (node-children node)]
    (case tag
      :span     [{:text  (raw-body node)
                  :style style
                  :href  href
                  :node  node}]

      :br       [{:break? true :node node}]

      :c        [{:text  (raw-body node)
                  :style (conj style :code)
                  :href  href
                  :node  node}]

      :kbd      [{:text  (raw-body node)
                  :style (conj style :code)
                  :href  href
                  :node  node}]

      :code     [{:text  (raw-body node)
                  :style (conj style :code)
                  :href  href
                  :node  node}]

      :strong   (inlines->runs children (conj style :bold)   href)
      :em       (inlines->runs children (conj style :italic) href)
      :mark     (inlines->runs children (conj style :bold)   href)
      :sup      (inlines->runs children style                href)
      :sub      (inlines->runs children style                href)

      :a        (inlines->runs children (conj style :link)   (or (:href attrs) href))

      :img      [{:text  (str "🖼 " (or (:alt attrs) (:src attrs) "image"))
                  :style (conj style :dim)
                  :href  (:src attrs)
                  :node  node}]

      ;; unknown inline: pass through children
      (inlines->runs children style href))))

(defn- inlines->runs
  "Flatten a sequence of inline children into runs."
  [children style href]
  (vec (mapcat #(inline->runs % style href) children)))

;; =============================================================================
;; Word-wrap
;; =============================================================================

(defn- atomize-run
  "Split one run's text into atoms preserving whitespace/word
   alternation. Each atom inherits the run's style/href/node."
  [{:keys [text style href node break?] :as run}]
  (cond
    break?            [run]
    (str/blank? text) (when (seq text) [run])
    :else
    ;; Manual scan replaces `(re-seq #"[^\s]+|[ \t]+" text)`.
    ;; The regex allocates a `Matcher` + lazy result seq per run;
    ;; for prose with thousands of words per streaming chunk this is
    ;; ~20k Matcher objects/chunk. A single linear scan emitting atom
    ;; maps directly into a transient gives identical output (the
    ;; regex alternation is just "non-whitespace OR space-or-tab")
    ;; with one allocation per atom, no regex machinery. Behaviour:
    ;; consecutive non-ws chars form a word atom; consecutive
    ;; \u0020 / \u0009 chars form a whitespace atom; \n/\r/etc. were
    ;; already excluded by the regex `[^\s]+` arm and we exclude them
    ;; here too (the walker hands them off via `:break?` runs).
    (let [^String s text
          n (.length s)
          atoms (transient [])]
      (loop [i 0]
        (if (>= i n)
          (persistent! atoms)
          (let [c (.charAt s i)]
            (cond
              ;; space or tab → whitespace atom
              (or (= c \space) (= c \tab))
              (let [start i
                    j (loop [j (inc i)]
                        (if (and (< j n)
                              (let [c2 (.charAt s j)]
                                (or (= c2 \space) (= c2 \tab))))
                          (recur (inc j)) j))]
                (conj! atoms {:text  (.substring s start j)
                              :style style :href href :node node})
                (recur j))

              ;; treat any other whitespace (newline, etc.) as word
              ;; boundary but skip it (matches old regex which only
              ;; matched [ \t]+ for the ws arm and skipped \n via
              ;; `[^\s]+` not consuming it; behaviour: drop the char).
              (Character/isWhitespace c)
              (recur (inc i))

              ;; non-whitespace word
              :else
              (let [start i
                    j (loop [j (inc i)]
                        (if (and (< j n)
                              (let [c2 (.charAt s j)]
                                (not (Character/isWhitespace c2))))
                          (recur (inc j)) j))]
                (conj! atoms {:text  (.substring s start j)
                              :style style :href href :node node})
                (recur j)))))))))

(defn- wrap-runs
  "Greedy word-wrap. Returns a vector of lines; each line is `{:runs [...]}`.
   Drops leading whitespace on continuation lines. `:break?` atoms force
   a line flush.

   `prefix-runs` may be a vector (used as initial AND continuation) or a
   map `{:initial [...] :cont [...]}` for hanging-indent contexts."
  [runs width prefix-runs]
  (let [width  (max 1 (long width))
        atoms  (vec (mapcat atomize-run runs))
        {init-runs :initial cont-runs :cont}
        (cond
          (map? prefix-runs)    {:initial (vec (:initial prefix-runs))
                                 :cont    (vec (:cont prefix-runs))}
          (sequential? prefix-runs) {:initial (vec prefix-runs)
                                     :cont    (vec prefix-runs)}
          :else                 {:initial [] :cont []})
        cont-w  (long (reduce + 0 (map run-width cont-runs)))
        init-w  (long (reduce + 0 (map run-width init-runs)))
        out    (volatile! [])
        line   (volatile! init-runs)
        lw     (volatile! init-w)
        prefix-w (volatile! init-w)
        flush! (fn []
                 (vswap! out conj {:runs @line})
                 (vreset! line  cont-runs)
                 (vreset! lw    cont-w)
                 (vreset! prefix-w cont-w))
        push!  (fn [a]
                 (vswap! line conj a)
                 (vswap! lw + (run-width a)))]
    (doseq [a atoms]
      (cond
        (:break? a)
        (flush!)

        (str/blank? (:text a))
        ;; whitespace atom — drop if at start, otherwise add (might overflow,
        ;; we still keep on the line; trailing ws will be collapsed by render)
        (when (> @lw @prefix-w)
          (push! a))

        :else
        (let [aw (run-width a)]
          (cond
            ;; fits on current line
            (<= (+ @lw aw) width)
            (push! a)

            ;; current line still has only the prefix → force-fit
            (= @lw @prefix-w)
            (do (push! a) (flush!))

            :else
            (do (flush!) (push! a))))))
    (when (> (line-width {:runs @line}) @prefix-w)
      (vswap! out conj {:runs @line}))
    @out))

(defn- trim-trailing-ws [line]
  (update line :runs
    (fn [runs]
      (loop [runs (vec runs)]
        (let [n (count runs)]
          (if (and (pos? n) (str/blank? (or (:text (nth runs (dec n))) "")))
            (recur (subvec runs 0 (dec n)))
            runs))))))

;; =============================================================================
;; Block walker
;; =============================================================================

(declare block->lines)

(defn- blocks->lines
  "Walk a sequence of canonical blocks, concatenating their line
   outputs.

   When `(:max-lines opts)` is set, the reduce short-circuits as soon
   as the accumulator reaches `1.5 * limit` lines. The post-walk
   pipeline in `ir->lines` may collapse blank-runs and only ever
   shrinks the result, so walking N*1.5 lines and post-truncating to
   N produces the same final output as walking the entire body. The
   slack covers worst-case blank-collapse.

   Without `:max-lines` the lazy mapcat path runs - bench shows it
   is JIT-friendlier than transients for this small-vector recursive
   shape (see autoresearch B2 discard)."
  [children width opts]
  (if-let [limit (:max-lines opts)]
    (let [cap (long (* 3 (long (long limit)) 1/2))]   ; 1.5*limit
      (reduce (fn [acc child]
                (let [acc' (into acc (block->lines child width opts))]
                  (if (>= (count acc') cap)
                    (reduced acc')
                    acc')))
              []
              children))
    (vec (mapcat #(block->lines % width opts) children))))

(defn- inline-block->lines
  "Render a block whose children are inline (`:p`, `:h`, `:th`, `:td`,
   `:quote`-paragraph). `style-prefix` is a set merged into every
   produced run."
  [children width _opts style-prefix prefix-runs]
  (let [runs (cond->> (inlines->runs children #{} nil)
               (seq style-prefix)
               (mapv (fn [r]
                       (if (:break? r)
                         r
                         (update r :style (fnil into #{}) style-prefix)))))]
    (wrap-runs runs width prefix-runs)))

(defn- code-block->lines
  "Code block: never wrap, never escape; preserve raw text. Runs carry
   `:code` style. Empty lines render as a blank line."
  [node _width {:keys [code-fence?] :as _opts}]
  (let [src   (raw-body node)
        attrs (node-attrs node)
        lang  (:lang attrs)
        body  (mapv (fn [line]
                      {:runs (if (= "" line)
                               []
                               [{:text  line
                                 :style #{:code}
                                 :node  node}])})
                (str/split-lines (or src "")))
        body  (if (str/ends-with? (or src "") "\n")
                (conj body {:runs []})
                body)]
    (if code-fence?
      (let [open  {:runs [{:text (str "```" (or lang "")) :style #{:dim :code} :node node}]}
            close {:runs [{:text "```" :style #{:dim :code} :node node}]}]
        (into [open] (conj body close)))
      body)))

(defn- list->lines [tag children width opts]
  (let [ordered? (= :ol tag)
        n        (volatile! 1)]
    (vec
      (mapcat
        (fn [li]
          (let [marker (if ordered?
                         (let [m (str @n ". ")] (vswap! n inc) m)
                         "- ")
                indent (apply str (repeat (count marker) " "))
                marker-run {:text marker :style #{:marker} :node li}
                kids (node-children li)
                ;; canonical :li children = either all blocks (post-canon
                ;; multi-paragraph) OR exactly one wrapping :p (post-canon
                ;; inline run). Both cases handled uniformly: lay out each
                ;; block, indent continuation, prefix the FIRST line of the
                ;; FIRST block with the marker.
                block-lines
                (loop [out [] first? true bs (seq kids)]
                  (if (nil? bs)
                    out
                    (let [b (first bs)
                          ;; for :p produce inline-wrapped lines with
                          ;; hanging indent equal to marker width
                          lines
                          (cond
                            (and (vector? b) (= :p (node-tag b)))
                            (let [indent-run {:text indent :style #{} :node li}
                                  inline-runs (inlines->runs (node-children b) #{} nil)
                                  prefix (if first?
                                           {:initial [marker-run]  :cont [indent-run]}
                                           {:initial [indent-run]  :cont [indent-run]})
                                  ls (wrap-runs inline-runs width prefix)]
                              (if first?
                                ls
                                (concat [{:runs []}] ls)))

                            ;; nested block: recurse and indent each line
                            :else
                            (let [inner (block->lines b (max 1 (- width (count indent))) opts)
                                  prefixed
                                  (mapv (fn [l]
                                          (update l :runs
                                            (fn [rs]
                                              (into [{:text indent :style #{} :node li}] rs))))
                                    inner)
                                  prefixed
                                  (if (and first? (seq prefixed))
                                    (let [first-line (first prefixed)
                                          new-runs   (into [marker-run]
                                                       (drop 1 (:runs first-line)))]
                                      (into [(assoc first-line :runs new-runs)] (rest prefixed)))
                                    prefixed)]
                              prefixed))]
                      (recur (into out lines) false (next bs)))))]
            block-lines))
        children))))

(defn- quote->lines [children width opts]
  (let [inner (blocks->lines children (max 1 (- width 2)) opts)
        bar   {:text "│ " :style #{:quote} :node nil}]
    (mapv (fn [l] (update l :runs #(into [bar] %))) inner)))

(defn- details->lines
  "Render a `[:details]` block:
   - one summary line tagged `:summary` carrying `:meta
     {:kind :toggle-details :node-id <stable id>}` so the bubble
     painter can register a click region;
   - body lines tagged `:details-body`.
   Whether the body is rendered or replaced with a placeholder is
   the painter's call (it knows the live `:detail-expansions` map).
   The walker emits both forms, the painter picks one."
  [node width opts]
  (let [attrs    (node-attrs node)
        open?    (boolean (:open? attrs))
        children (vec (node-children node))
        summary  (first children)
        body     (rest children)
        node-id  (or (:node-id attrs) (str "details:" (hash node)))
        ;; summary line: triangle indicator + summary text, indent body 2
        marker   {:text (if open? "▾ " "▸ ") :style #{:marker} :node summary}
        sum-runs (inlines->runs (node-children summary) #{:bold} nil)
        sum-lines (wrap-runs sum-runs width [marker])
        sum-tagged (mapv (fn [l]
                           (-> l
                             (assoc :block-tag :summary)
                             (assoc :meta {:kind     :toggle-details
                                           :node-id  node-id
                                           :open?    open?})))
                     sum-lines)
        body-lines (when (or open? (nil? (:open? attrs)))
                     (let [inner (blocks->lines body (max 1 (- width 2)) opts)
                           pad   {:text "  " :style #{} :node nil}]
                       (mapv (fn [l]
                               (-> l
                                 (update :runs #(into [pad] %))
                                 (assoc :block-tag :details-body)))
                         inner)))]
    (vec (concat sum-tagged body-lines))))

(defn- tag-lines
  "Stamp every produced line with `:block-tag` so downstream adapters
   (sentinel-string emitter, click/select region builder) can map
   each rendered line back to its semantic source.  For headings we
   also propagate `:level` so adapters can pick H1/H2/H3 markers."
  [lines block-tag & {:keys [level]}]
  (mapv (fn [l]
          (cond-> (assoc l :block-tag block-tag)
            level (assoc :block-level level)))
    lines))

(defn- block->lines
  "Render one block node into a vector of lines. Each line carries a
   `:block-tag` used by downstream adapters."
  [node width opts]
  (let [tag (node-tag node)]
    (case tag
      :p
      (let [ls (inline-block->lines (node-children node) width opts #{} nil)
            ls (if (seq ls) ls [(empty-line)])]
        (conj (vec (tag-lines ls :p)) (assoc (empty-line) :block-tag :p)))

      :h
      (let [level (or (:level (node-attrs node)) 1)
            ls (inline-block->lines (node-children node) width opts #{:bold :heading} nil)
            ls (if (seq ls) ls [(empty-line)])]
        (conj (vec (tag-lines ls :h :level level))
          (assoc (empty-line) :block-tag :h :block-level level)))

      :code
      (conj (vec (tag-lines (code-block->lines node width opts) :code))
        (assoc (empty-line) :block-tag :code))

      :ul
      (conj (vec (tag-lines (list->lines :ul (node-children node) width opts) :ul))
        (assoc (empty-line) :block-tag :ul))

      :ol
      (conj (vec (tag-lines (list->lines :ol (node-children node) width opts) :ol))
        (assoc (empty-line) :block-tag :ol))

      :quote
      (conj (vec (tag-lines (quote->lines (node-children node) width opts) :quote))
        (assoc (empty-line) :block-tag :quote))

      :details
      ;; details->lines already stamps per-line block tags + click meta;
      ;; just pad with a trailing blank for visual spacing.
      (conj (vec (details->lines node width opts))
        (assoc (empty-line) :block-tag :details-body))

      ;; tables fall back to plain projection for v1; truncate lines
      ;; to width so they don't blow past the terminal
      :table
      (let [md (ir/render [:ir node] :plain)
            cap (max 1 (long width))
            ls (mapv (fn [l]
                       (let [t (if (> (count l) cap)
                                 (str (subs l 0 (max 0 (- cap 1))) "…")
                                 l)]
                         {:runs (if (= "" t) [] [{:text t :style #{} :node node}])}))
                 (str/split-lines md))]
        (conj (vec (tag-lines ls :table)) (assoc (empty-line) :block-tag :table)))

      ;; unknown / leftover inline at block position
      [(assoc (empty-line) :block-tag :p)])))

;; =============================================================================
;; Public API
;; =============================================================================

(defn ir->lines
  "Walk canonical IR (or any input that `ir/->ast` accepts) at a given
   terminal `width` and return a vector of styled lines."
  ([input width] (ir->lines input width nil))
  ([input width opts]
   (let [ast    (ir/->ast input)
         body   (drop 2 ast)               ; canonical: [:ir {} & blocks]
         lines  (blocks->lines body width (or opts {}))
         lines  (vec lines)
         ;; collapse runs of >1 trailing blank lines into a single blank
         lines  (loop [out [] prev-blank? false ls (seq lines)]
                  (if (nil? ls)
                    out
                    (let [l (first ls)
                          blank? (line-blank? l)]
                      (recur (if (and blank? prev-blank?) out (conj out l))
                        blank?
                        (next ls)))))
         ;; drop leading + trailing blank lines
         lines  (vec (drop-while line-blank? lines))
         lines  (vec (reverse (drop-while line-blank? (reverse lines))))
         lines  (mapv trim-trailing-ws lines)]
     (if-let [n (:max-lines opts)]
       (vec (take n lines))
       lines))))

(defn- ir-text-chars
  "Sum of all text-content character counts in an IR node tree. Cheap
   recursive walk - just counts string leaves. Used by
   `ir->lines-tail` as a per-block height estimator.

   Uses `.length` on Strings (direct char-array length, no Counted
   dispatch) and `.count` on the IPersistentVector node so neither
   goes through `clojure.core/count`'s reflective fast-path table."
  ^long [node]
  (cond
    (string? node)
    (long (.length ^String node))

    (vector? node)
    (let [^clojure.lang.IPersistentVector v node
          n (long (.count v))]
      (loop [i (long 1) acc (long 0)]
        (if (>= i n)
          acc
          (recur (inc i) (+ acc (ir-text-chars (.nth v i)))))))

    :else 0))

(defn ir->lines-tail
  "Render only the last `tail-n` styled lines of the IR.

   Walks the top-level blocks BACKWARD, accumulating a cheap
   per-block line estimate (text-chars / content-width), until
   accumulated estimate ≥ `tail-n * 2` (slack covers blank-collapse
   + per-block trailing blanks). Then renders ONLY those tail blocks
   via the normal `ir->lines` and `(take-last tail-n)`.

   Per-frame cost = O(visible-tail), independent of total body
   length. The semantically-correct path for an auto-scrolled
   tail-pinned bubble (vs the broken `:max-lines` cap which would
   give the FIRST tail-n).

   `tail-n` must be positive. If the body has fewer total estimated
   lines than the budget, this falls back to a full walk."
  ([input width tail-n] (ir->lines-tail input width tail-n nil))
  ([input width tail-n opts]
   (let [tail-n     (long tail-n)
         ast        (ir/->ast input)
         blocks     (vec (drop 2 ast))
         nb         (count blocks)
         content-w  (max 1 (long width))
         ;; 2x slack covers post-walk blank collapse + estimator slop
         budget     (long (* 2 tail-n))
         picked-from
         (loop [i (dec nb) accum 0]
           (cond
             (neg? i)             0
             (>= accum budget)    (inc i)
             :else
             (let [b   (nth blocks i)
                   est (max 1 (long (Math/ceil
                                      (/ (double (max 1 (ir-text-chars b)))
                                         (double content-w)))))]
               (recur (dec i) (+ accum est)))))
         tail-blocks (subvec blocks picked-from)
         synth-ir    (into [:ir {}] tail-blocks)
         tail-lines  (ir->lines synth-ir width opts)]
     (if (<= (count tail-lines) tail-n)
       tail-lines
       (vec (take-last tail-n tail-lines))))))

(defn lines->plain
  "Concatenate the text of every run in `lines`. Useful for tests +
   clipboard fallback (preferred clipboard path: `ir/render :markdown`)."
  ^String [lines]
  (str/join "\n"
    (map (fn [l] (apply str (map (fn [r] (or (:text r) "")) (:runs l))))
      lines)))

;; =============================================================================
;; Sentinel-string adapter — bridge to the existing bubble painter
;; =============================================================================
;;
;; The pre-IR bubble painter consumes vectors of strings, where each
;; string starts with a block marker (`MARKER_MD_H1` / `MARKER_MD_BULLET`
;; / etc., see `primitives.clj`) and embeds inline span sentinels
;; (`INLINE_BOLD_ON/OFF`, `INLINE_CODE_ON/OFF`, ...). This adapter
;; emits exactly that surface from canonical IR walker output, so
;; assistant-answer rendering can switch off the markdown→parse→wrap
;; pipeline without touching the painter.
;;
;; User-typed messages still go through `render/markdown->lines` for
;; now — they're plain strings, not IR — so the painter contract
;; stays bivalent.

(def ^:private answer-marker-set
  {:h1      p/MARKER_MD_H1
   :h2      p/MARKER_MD_H2
   :h3      p/MARKER_MD_H3
   :code    p/MARKER_MD_CODE
   :bullet  p/MARKER_MD_BULLET
   :quote   p/MARKER_MD_QUOTE
   :table   p/MARKER_MD_TABLE_ROW
   :summary p/MARKER_MD_SUMMARY
   :plain   p/MARKER_ANSWER_TXT})

(def ^:private thinking-marker-set
  {:h1      p/MARKER_TH_MD_H1
   :h2      p/MARKER_TH_MD_H2
   :h3      p/MARKER_TH_MD_H3
   :code    p/MARKER_TH_MD_CODE
   :bullet  p/MARKER_TH_MD_BULLET
   :quote   p/MARKER_TH_MD_QUOTE
   :table   p/MARKER_TH_MD_TABLE_ROW
   :summary p/MARKER_TH_MD_SUMMARY
   :plain   p/MARKER_THINKING})

(defn- marker-set-for [mode]
  (case mode
    :thinking thinking-marker-set
    answer-marker-set))

(defn- block-marker-for
  "Pick the block marker for a `:block-tag` (and optional heading
   `:level`) from the `mode` marker set (`:answer` or `:thinking`)."
  ^String [marker-set block-tag level]
  (case block-tag
    :h            (case (long (or level 1))
                    1 (:h1 marker-set)
                    2 (:h2 marker-set)
                    (:h3 marker-set))
    :code         (:code marker-set)
    :ul           (:bullet marker-set)
    :ol           (:bullet marker-set)
    :li           (:bullet marker-set)
    :quote        (:quote marker-set)
    :table        (:table marker-set)
    :summary      (:summary marker-set)
    :details-body (:plain marker-set)
    ;; :p, nil, anything else → plain text marker
    (:plain marker-set)))

(defn- run->sentinel-segment
  "Wrap a run's text with the inline-span sentinels its style flags
   demand. Bold / italic / code / link each get an ON…OFF pair."
  ^String [{:keys [text style]}]
  (let [text (or text "")]
    (cond-> text
      (contains? style :code)   (->> (str p/INLINE_CODE_ON))
      (contains? style :code)   (str p/INLINE_CODE_OFF)
      (contains? style :italic) (->> (str p/INLINE_ITALIC_ON))
      (contains? style :italic) (str p/INLINE_ITALIC_OFF)
      (contains? style :bold)   (->> (str p/INLINE_BOLD_ON))
      (contains? style :bold)   (str p/INLINE_BOLD_OFF)
      (contains? style :link)   (->> (str p/INLINE_LINK_ON))
      (contains? style :link)   (str p/INLINE_LINK_OFF))))

(defn lines->sentinel-strings
  "Convert walker output (vector of `{:runs :block-tag :block-level?}`
   maps) into the painter's sentinel-prefixed string contract. Each
   line: `<block-marker><inline-sentinel-wrapped body>`. `:mode`
   selects the marker set (`:answer` (default) or `:thinking`)."
  ([lines] (lines->sentinel-strings lines nil))
  ([lines opts]
   (let [ms (marker-set-for (:mode opts))]
     (mapv (fn [{:keys [runs block-tag block-level]}]
             (let [marker (block-marker-for ms block-tag block-level)
                   body   (apply str (map run->sentinel-segment runs))]
               (str marker body)))
       lines))))

(defn ir->sentinel-strings
  "One-shot helper: canonical IR → vector of sentinel-prefixed strings
   ready for the existing bubble painter. Composes `ir->lines` with
   the sentinel adapter. `:mode` (`:answer` or `:thinking`) selects
   the marker set."
  ([ir width] (ir->sentinel-strings ir width nil))
  ([ir width opts]
   (lines->sentinel-strings (ir->lines ir width opts) opts)))

(defn ir->inline-sentinel-string
  "Flatten a canonical IR into a single sentinel-wrapped inline string
   suitable for chrome-row labels. NO block markers — just inline
   sentinels (`INLINE_BOLD_ON/OFF`, `INLINE_CODE_ON/OFF`, etc.) wrapping
   styled runs. Hard breaks `[:br]` become spaces (single line
   contract). Used by detail-summary chrome and other label
   surfaces that previously ran through `markdown->inline`."
  ^String [ir]
  (let [;; pass huge width so walker never wraps; we want one flat run
        lines (ir->lines ir Integer/MAX_VALUE)
        ;; concat all runs across all lines, joining lines with single space
        line-strs (mapv (fn [{:keys [runs]}]
                          (apply str (map run->sentinel-segment runs)))
                    lines)]
    (str/replace (str/join " " (remove str/blank? line-strs)) #"\s+" " ")))

(defn ir->entries
  "Drop-in replacement for the legacy `render/markdown->entries`.
   Returns a vector of `{:line :meta}` maps where `:line` is the
   sentinel-prefixed string the bubble painter consumes, and `:meta`
   is per-line click-region metadata (e.g. `{:kind :toggle-details
   :node-id ...}` for `:summary` lines).  `nil` `:meta` for content
   lines.

   `:mode` selects the marker set:
     `:answer`   (default) — answer-zone PUA chars (answer-bg paint)
     `:thinking` — thinking-zone PUA chars (iter-header-bg + italic)

   This is the IR-side analogue of `markdown->entries`. Every
   bubble rendering path that used to parse the rendered markdown
   back into entries should call this directly on the canonical IR
   — no markdown round-trip."
  ([ir width] (ir->entries ir width nil))
  ([ir width opts]
   (let [lines (ir->lines ir width opts)
         ms    (marker-set-for (:mode opts))]
     (mapv (fn [{:keys [runs block-tag block-level meta]}]
             {:line (let [marker (block-marker-for ms block-tag block-level)
                          body   (apply str (map run->sentinel-segment runs))]
                      (str marker body))
              :meta meta})
       lines))))
