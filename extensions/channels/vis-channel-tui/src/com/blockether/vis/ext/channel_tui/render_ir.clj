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
    (->> (re-seq #"[^\s]+|[ \t]+" text)
      (map (fn [s] {:text s :style style :href href :node node})))))

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

(defn- blocks->lines [children width opts]
  (vec (mapcat #(block->lines % width opts) children)))

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

(defn- block->lines
  "Render one block node into a vector of lines."
  [node width opts]
  (let [tag (node-tag node)]
    (case tag
      :p
      (let [ls (inline-block->lines (node-children node) width opts #{} nil)]
        (if (seq ls) (conj (vec ls) (empty-line)) [(empty-line)]))

      :h
      (let [ls (inline-block->lines (node-children node) width opts #{:bold :heading} nil)]
        (if (seq ls) (conj (vec ls) (empty-line)) [(empty-line)]))

      :code
      (conj (vec (code-block->lines node width opts)) (empty-line))

      :ul
      (conj (vec (list->lines :ul (node-children node) width opts)) (empty-line))

      :ol
      (conj (vec (list->lines :ol (node-children node) width opts)) (empty-line))

      :quote
      (conj (vec (quote->lines (node-children node) width opts)) (empty-line))

      ;; tables fall back to plain projection for v1; truncate lines
      ;; to width so they don't blow past the terminal
      :table
      (let [md (ir/render [:ir node] :plain)
            cap (max 1 (long width))]
        (conj (vec (map (fn [l]
                          (let [t (if (> (count l) cap)
                                    (str (subs l 0 (max 0 (- cap 1))) "…")
                                    l)]
                            {:runs (if (= "" t) [] [{:text t :style #{} :node node}])}))
                     (str/split-lines md)))
          (empty-line)))

      ;; unknown / leftover inline at block position
      [(empty-line)])))

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

(defn lines->plain
  "Concatenate the text of every run in `lines`. Useful for tests +
   clipboard fallback (preferred clipboard path: `ir/render :markdown`)."
  ^String [lines]
  (str/join "\n"
    (map (fn [l] (apply str (map (fn [r] (or (:text r) "")) (:runs l))))
      lines)))
