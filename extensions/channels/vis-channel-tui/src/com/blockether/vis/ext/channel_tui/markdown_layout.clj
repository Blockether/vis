(ns com.blockether.vis.ext.channel-tui.markdown-layout
  "TUI layout walker over a transient parsed Markdown tree (`com.blockether.vis.internal.render/->ast`).

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
     (ast->lines ir width)             ; total walker
     (ast->lines ir width opts)
   Opts:
     :heading-prefix? bool   ; render '#'-style markers (default false; bold suffices)
     :code-fence?     bool   ; render ``` lines around code blocks (default false)
     :max-lines       int    ; hard cap (default unlimited)"
  (:require [clojure.string :as str]
            [com.blockether.vis.ext.channel-tui.primitives :as p]
            [com.blockether.vis.ext.channel-tui.highlight :as hl]
            [com.blockether.vis.internal.render :as ir]))

;; =============================================================================
;; Helpers
;; =============================================================================

(defn- node-tag [n] (when (vector? n) (first n)))
(defn- node-attrs [n] (if (and (vector? n) (map? (nth n 1 nil))) (nth n 1) {}))
(defn- node-children [n] (if (and (vector? n) (map? (nth n 1 nil))) (drop 2 n) (rest n)))
(defn- raw-body [n] (or (some #(when (string? %) %) (node-children n)) ""))

(defn- empty-line [] {:runs []})

(defn- run-width ^long [{:keys [text]}] (p/display-width (or text "")))

(defn- line-width ^long [line] (reduce + 0 (map run-width (:runs line))))

(defn- line-blank?
  [line]
  (every? (fn [{:keys [text]}]
            (str/blank? (or text "")))
          (:runs line)))

;; =============================================================================
;; Inline → flat run sequence
;; =============================================================================

;; Mutual recursion: `inline->runs` dispatches per-node, then for
;; nested inline tags (`:strong`/`:em`/`:a` etc.) calls back into
;; `inlines->runs`, which itself maps over children invoking
;; `inline->runs`. Two-way cycle — can't be sorted away.
(declare inlines->runs)

(defn- inline->runs
  "Canonicalize-friendly inline tag dispatch. `style` is a set of
   keywords accumulated from ancestors. `href` propagates from `:a`."
  [node style href]
  (let [tag
        (node-tag node)

        attrs
        (node-attrs node)

        children
        (node-children node)]

    (case tag
      :span
      [{:text (raw-body node) :style style :href href :node node}]

      :br
      [{:break? true :node node}]

      :c
      [{:text (raw-body node) :style (conj style :code) :href href :node node}]

      :kbd
      [{:text (raw-body node) :style (conj style :code) :href href :node node}]

      :code
      [{:text (raw-body node) :style (conj style :code) :href href :node node}]

      :strong
      (inlines->runs children (conj style :bold) href)

      :em
      (inlines->runs children (conj style :italic) href)

      :mark
      (inlines->runs children (conj style :bold) href)

      :sup
      (inlines->runs children style href)

      :sub
      (inlines->runs children style href)

      :a
      (inlines->runs children (conj style :link) (or (:href attrs) href))

      :img
      [{:text (str (or (:alt attrs) (:src attrs) "image"))
        :style (conj style :dim)
        :href (:src attrs)
        :node node}]

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
  (cond break? [run]
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
        (let [^String s
              text

              n
              (.length s)]

          ;; `atoms` carried through recur per the transient contract:
          ;; `conj!` returns a possibly-different handle, so we MUST
          ;; rebind it (else `persistent!` may miss the appended values
          ;; in larger transients). Pre-Phase-7 cleanup left the return
          ;; values dropped; lint flagged it; this restructure threads
          ;; the handle correctly without changing semantics.
          (loop [i
                 (long 0)

                 atoms
                 (transient [])]

            (if (>= i n)
              (persistent! atoms)
              (let [c (.charAt s i)]
                (cond
                  ;; space or tab → whitespace atom
                  (or (= c \space) (= c \tab))
                  (let [start i
                        j (long (loop [j (long (inc i))]
                                  (if (and (< j n)
                                           (let [c2 (.charAt s j)]
                                             (or (= c2 \space) (= c2 \tab))))
                                    (recur (unchecked-inc j))
                                    j)))]

                    (recur j
                           (conj!
                             atoms
                             {:text (.substring s start j) :style style :href href :node node})))
                  ;; treat any other whitespace (newline, etc.) as word
                  ;; boundary but skip it (matches old regex which only
                  ;; matched [ \t]+ for the ws arm and skipped \n via
                  ;; `[^\s]+` not consuming it; behaviour: drop the char).
                  (Character/isWhitespace c) (recur (unchecked-inc i) atoms)
                  ;; non-whitespace word
                  :else (let [start i
                              j (long (loop [j (long (inc i))]
                                        (if (and (< j n)
                                                 (let [c2 (.charAt s j)]
                                                   (not (Character/isWhitespace c2))))
                                          (recur (unchecked-inc j))
                                          j)))]

                          (recur j
                                 (conj! atoms
                                        {:text (.substring s start j)
                                         :style style
                                         :href href
                                         :node node}))))))))))

(defn- wrap-runs
  "Greedy word-wrap. Returns a vector of lines; each line is `{:runs [...]}`.
   Drops leading whitespace on continuation lines. `:break?` atoms force
   a line flush.

   `prefix-runs` may be a vector (used as initial AND continuation) or a
   map `{:initial [...] :cont [...]}` for hanging-indent contexts."
  [runs width prefix-runs]
  (let [width
        (max 1 (long width))

        atoms
        (vec (mapcat atomize-run runs))

        {init-runs :initial cont-runs :cont}
        (cond (map? prefix-runs) {:initial (vec (:initial prefix-runs))
                                  :cont (vec (:cont prefix-runs))}
              (sequential? prefix-runs) {:initial (vec prefix-runs) :cont (vec prefix-runs)}
              :else {:initial [] :cont []})

        cont-w
        (long (reduce + 0 (map run-width cont-runs)))

        init-w
        (long (reduce + 0 (map run-width init-runs)))

        out
        (volatile! [])

        line
        (volatile! init-runs)

        lw
        (volatile! init-w)

        prefix-w
        (volatile! init-w)

        ;; false until the first flush — i.e. while we are still on the
        ;; ORIGINAL line, not a wrapped continuation segment. Leading
        ;; whitespace is meaningful indentation on the first line (source
        ;; code, the `cat` hash gutter's blank-anchor pad) and must
        ;; survive; only continuation segments drop it on reflow.
        cont?
        (volatile! false)

        ;; `soft?` marks a line flushed because the next word would
        ;; overflow `width` — a true word-wrap continuation point. Such
        ;; lines are full-justifiable downstream. A line flushed by a
        ;; `:break?` atom (hard break / paragraph end) is NOT soft, nor
        ;; is the trailing final line below — those are the natural,
        ;; ragged-right ends of a paragraph and must never be stretched.
        flush!
        (fn [soft?]
          (vswap! out
                  conj
                  (cond-> {:runs @line}
                    soft?
                    (assoc :wrap? true)))
          (vreset! line cont-runs)
          (vreset! lw cont-w)
          (vreset! prefix-w cont-w)
          (vreset! cont? true))

        push!
        (fn [a]
          (vswap! line conj a)
          (vswap! lw #(+ (long %) (long (run-width a)))))]

    (doseq [a atoms]
      (cond (:break? a) (flush! false)
            (str/blank? (:text a))
            ;; whitespace atom — on a continuation segment, drop it when at
            ;; the start (reflowed text shouldn't carry indentation). On the
            ;; FIRST line keep leading whitespace: it is the original line's
            ;; own indentation (source columns / cat blank-gutter pad), whose
            ;; loss left-shifts the line and breaks column alignment.
            ;; Otherwise add it (might overflow; trailing ws collapses in render).
            (when (or (not @cont?) (> (long @lw) (long @prefix-w))) (push! a))
            :else (let [aw (run-width a)]
                    (cond
                      ;; fits on current line
                      (<= (+ (long @lw) (long aw)) (long width)) (push! a)
                      ;; current line still has only the prefix → force-fit
                      (= @lw @prefix-w) (do (push! a) (flush! true))
                      :else (do (flush! true) (push! a))))))
    (when (> (long (line-width {:runs @line})) (long @prefix-w)) (vswap! out conj {:runs @line}))
    @out))

(defn- trim-trailing-ws
  [line]
  (update line
          :runs
          (fn [runs]
            (loop [runs (vec runs)]
              (let [n (count runs)]
                (if (and (pos? n) (str/blank? (or (:text (nth runs (dec n))) "")))
                  (recur (subvec runs 0 (dec n)))
                  runs))))))

;; =============================================================================
;; Block walker
;; =============================================================================

;; Mutual recursion: `block->lines` (final dispatch) calls
;; `blocks->lines`/`list->lines`/`quote->lines` for nested children;
;; each of those calls back into `block->lines`. The cycle is structural
;; (markdown blocks nest), can't be sorted.
(declare block->lines)

(defn- blocks->lines
  "Walk a sequence of canonical blocks, concatenating their line
   outputs.

   When `(:max-lines opts)` is set, the reduce short-circuits as soon
   as the accumulator reaches `1.5 * limit` lines. The post-walk
   pipeline in `ast->lines` may collapse blank-runs and only ever
   shrinks the result, so walking N*1.5 lines and post-truncating to
   N produces the same final output as walking the entire body. The
   slack covers worst-case blank-collapse.

   Without `:max-lines` the lazy mapcat path runs - bench shows it
   is JIT-friendlier than transients for this small-vector recursive
   shape (see autoresearch B2 discard)."
  [children width opts]
  (if-let [limit (:max-lines opts)]
    (let [cap (long (quot (* 3 (long limit)) 2))] ; 1.5*limit
      (reduce (fn [acc child]
                (let [acc' (into acc (block->lines child width opts))]
                  (if (>= (count acc') cap) (reduced acc') acc')))
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
                       (if (:break? r) r (update r :style (fnil into #{}) style-prefix)))))]
    (wrap-runs runs width prefix-runs)))

(def ^:private wrap-friendly-code-langs
  "Code-block `:lang` values whose body is plain prose / structured
   output (file dumps, search results, tool listings) — safe to soft
   wrap when a row exceeds the bubble width. Anything outside this
   set (`clojure`, `diff`, `python`, … unset) keeps verbatim layout
   so source / patch alignment survives."
  #{"text" "plain" "output" "log"})

(defn- code-block->lines
  "Code block: by default the TUI shows the body verbatim — source code
   relies on its indentation, and diff/patch output on its column
   alignment, so wrapping there would destroy the visual contract.

   When the block's `:lang` is in `wrap-friendly-code-langs` (plain
   text / tool output / log dumps) each line is greedily wrapped to
   `width` so long file contents and search hits stay inside the
   bubble instead of overflowing past the right edge. Empty lines
   render as a blank line either way.

   Every code block also gets one blank `:code` row above and below
   its content. Those rows are semantic *inside-code* padding: the
   bubble painter fills them with the code-block background, giving
   the chip the same breathing room as tool-call code/result zones."
  [node width {:keys [code-fence?] :as _opts}]
  (let [src
        (raw-body node)

        attrs
        (node-attrs node)

        lang
        (:lang attrs)

        content
        (or src "")

        budget
        (max 1 (long width))

        wrap?
        (contains? wrap-friendly-code-langs
                   (some-> lang
                           str/lower-case))

        ;; `:wrap?` on the node opts a single zone into SOFT WRAP regardless
        ;; of lang. Unlike the lang-set word-wrap above it CHAR-FOLDS (via
        ;; `p/fold-cols` → lanterna `foldColumns`): whitespace/indentation is
        ;; preserved exactly and only the over-wide rows fold, so a wide
        ;; `clj_eval` value / long tool-call arg stops overflowing while
        ;; verbatim structure survives.
        fold?
        (boolean (:wrap? attrs))

        diff?
        (= "diff"
           (some-> lang
                   str/lower-case))

        grammar
        (hl/grammar-for lang)

        colorize?
        (and grammar (not fold?) (not diff?))

        hl-lines
        (when colorize?
          (some-> (hl/highlight grammar content)
                  str/split-lines))

        ;; A `diff` fence (patch / write / format evidence) is colored by
        ;; wrapping each row in ANSI SGR: the `md-code` paint branch runs the row
        ;; through `paint-ansi-line!`, which translates the codes to Lanterna fg
        ;; (32→green add, 91→red del, 36→cyan hunk, 90→dim file header) — the SAME
        ;; mechanism that carries zprint syntax color on Clojure fences, so no
        ;; painter change is needed. Context rows stay base fg.
        ;; `ir/diff-line-kind` is the SHARED classifier the web channel also uses
        ;; (there → a CSS class), so a diff fence colours identically in both.
        kind->sgr
        {:meta "90" :hunk "36" :add "32" :del "91" :ctx nil}

        ansi-diff
        (fn [^String line]
          (if (= "" line)
            line
            (let [code (kind->sgr (ir/diff-line-kind line))]
              (if code (str "\u001b[" code "m" line "\u001b[0m") line))))

        pad
        {:runs []}

        runs-of
        (fn [line]
          [{:text (cond diff? (ansi-diff line)
                        :else line)
            :style #{:code}
            :node node}])

        verbatim-line
        (fn [line]
          {:runs (if (= "" line) [] (runs-of line))})

        wrap-line
        (fn [line]
          (if (= "" line)
            [{:runs []}]
            (let [wrapped (wrap-runs (runs-of line) budget [])]
              (if (seq wrapped)
                wrapped
                ;; Defensive: empty result from wrap means the input
                ;; was pure whitespace that got dropped; preserve the
                ;; row as blank rather than collapsing the line.
                [{:runs []}]))))

        fold-line
        (fn [line]
          (if (= "" line)
            [{:runs []}]
            (mapv (fn [seg]
                    {:runs (runs-of seg)})
                  (p/fold-cols line budget))))

        ansi-fold-line
        (fn [line]
          (if (= "" line)
            [{:runs []}]
            (mapv (fn [seg]
                    {:runs (runs-of seg)})
                  (p/ansi-fold-cols line budget))))

        body
        (vec
          (cond fold? (mapcat fold-line (str/split-lines content))
                wrap? (mapcat wrap-line (str/split-lines content))
                ;; A diff fence stays verbatim: its `+`/`-`/hunk column
                ;; alignment is a contract folding would break, and its rows
                ;; are rarely wider than the bubble.
                diff? (mapv verbatim-line (or hl-lines (str/split-lines content)))
                ;; Highlighted source (real grammar) ANSI-CHAR-FOLDS any
                ;; over-wide row to the bubble width, re-opening the SGR
                ;; active at each cut so token color survives the fold.
                ;; Rows that already fit are one segment, untouched. Without
                ;; this a pathologically wide colorized line (a `javascript:`
                ;; bookmarklet fenced ```js, a long JSON row) overflowed off
                ;; the right edge with no wrap and no horizontal scroll,
                ;; hiding its tail (the "can't see the full bookmarklet"
                ;; thread).
                colorize? (mapcat ansi-fold-line (or hl-lines (str/split-lines content)))
                ;; A plain fence (no grammar: no `:lang`, or an unknown/unset
                ;; one — e.g. a pasted URL / token blob) has no alignment
                ;; contract, so CHAR-FOLD any over-wide row too.
                :else (mapcat fold-line (str/split-lines content))))

        body
        (if (str/ends-with? content "\n") (conj body {:runs []}) body)

        body
        (vec (concat [pad] body [pad]))]

    (if code-fence?
      (let [open
            {:runs [{:text (str "```" (or lang "")) :style #{:dim :code} :node node}]}

            close
            {:runs [{:text "```" :style #{:dim :code} :node node}]}]

        (into [open] (conj body close)))
      body)))

(defn- task-list-marker
  "Return a glyph marker for GFM-style task-list text prefixes.

   CommonMark keeps `- [x] item` / `- [ ] item` as ordinary list item
   paragraph text in our canonical IR, so the TUI walker renders the task
   marker here: checked items become `☑️`, unchecked items become `⬜`, and
   the literal `[x]` / `[ ]` prefix is stripped from the first run."
  [ordered? runs]
  (when-not ordered?
    (let [first-text (or (some-> runs
                                 first
                                 :text
                                 str)
                         "")]
      (cond (re-find #"^\[[xX]\]\s+" first-text) {:marker "☑️  " :prefix #"^\[[xX]\]\s+"}
            (re-find #"^\[ \]\s+" first-text) {:marker "⬜ " :prefix #"^\[ \]\s+"}))))

(defn- strip-task-list-marker
  [runs prefix]
  (if (and prefix (seq runs))
    (update-in (vec runs) [0 :text] #(str/replace-first (or % "") prefix ""))
    runs))

(defn- list->lines
  [tag children width opts]
  (let [ordered?
        (= :ol tag)

        n
        (volatile! 1)]

    (vec
      (mapcat
        (fn [li]
          (let [kids
                (node-children li)

                first-p
                (first (filter #(and (vector? %) (= :p (node-tag %))) kids))

                first-runs
                (when first-p (inlines->runs (node-children first-p) #{} nil))

                task-marker
                (task-list-marker ordered? first-runs)

                marker
                (if ordered?
                  (let [m (str @n ". ")]
                    (vswap! n #(inc (long %)))
                    m)
                  (or (:marker task-marker) "- "))

                indent
                (apply str (repeat (p/display-width marker) " "))

                marker-run
                {:text marker :style #{:marker} :node li}

                ;; canonical :li children = either all blocks (post-canon
                ;; multi-paragraph) OR exactly one wrapping :p (post-canon
                ;; inline run). Both cases handled uniformly: lay out each
                ;; block, indent continuation, prefix the FIRST line of the
                ;; FIRST block with the marker.
                block-lines
                (loop [out
                       []

                       first?
                       true

                       bs
                       (seq kids)]

                  (if (nil? bs)
                    out
                    (let [b
                          (first bs)

                          ;; for :p produce inline-wrapped lines with
                          ;; hanging indent equal to marker width
                          lines
                          (cond (and (vector? b) (= :p (node-tag b)))
                                (let [indent-run
                                      {:text indent :style #{} :node li}

                                      raw-inline-runs
                                      (inlines->runs (node-children b) #{} nil)

                                      inline-runs
                                      (if first?
                                        (strip-task-list-marker raw-inline-runs
                                                                (:prefix task-marker))
                                        raw-inline-runs)

                                      prefix
                                      (if first?
                                        {:initial [marker-run] :cont [indent-run]}
                                        {:initial [indent-run] :cont [indent-run]})

                                      ls
                                      (wrap-runs inline-runs width prefix)]

                                  (if first? ls (concat [{:runs []}] ls)))
                                ;; nested block: recurse and indent each line
                                :else
                                (let [inner
                                      (block->lines b (max 1 (- (long width) (count indent))) opts)

                                      prefixed
                                      (mapv (fn [l]
                                              (cond-> (update
                                                        l
                                                        :runs
                                                        (fn [rs]
                                                          (into [{:text indent :style #{} :node li}]
                                                                rs)))
                                                (= :code (:block-tag l))
                                                (assoc-in [:meta :list-nested-code?] true)))
                                            inner)

                                      prefixed
                                      (if (and first? (seq prefixed))
                                        (let [first-line
                                              (first prefixed)

                                              new-runs
                                              (into [marker-run] (drop 1 (:runs first-line)))]

                                          (into [(assoc first-line :runs new-runs)]
                                                (rest prefixed)))
                                        prefixed)]

                                  prefixed))]

                      (recur (into out lines) false (next bs)))))]

            block-lines))
        children))))

(defn- quote->lines
  [children width opts]
  (let [inner
        (blocks->lines children (max 1 (- (long width) 2)) opts)

        ;; Strip per-paragraph outer-margin blanks the child block
        ;; renderers append. A blockquote should paint as ONE solid
        ;; bar block without empty `| ` rows between or below its
        ;; paragraphs; the outer-margin row appended AFTER the whole
        ;; quote block by `node->lines` handles the breathing space
        ;; from following content.
        compact
        (filterv #(not= :outer-margin (:block-tag %)) inner)

        bar
        {:text "│ " :style #{:quote} :node nil}]

    (mapv (fn [l]
            (update l :runs #(into [bar] %)))
          compact)))

(defn- inline-text
  "Visible one-line text for inline children. Used by table cells,
   where the table grid painter owns the row style and cannot safely
   consume inline sentinels inside cells. Hard breaks collapse to a
   space; links keep their visible label only."
  ^String [children]
  (->> (inlines->runs children #{} nil)
       (map (fn [{:keys [text break?]}]
              (if break? " " (or text ""))))
       (apply str)
       (#(str/replace % #"\s+" " "))
       str/trim))

(defn- table-cell-text [cell] (inline-text (node-children cell)))

(defn- pad-right-cols
  ^String [s width]
  (let [width
        (max 0 (long width))

        s
        (p/truncate-cols (or s "") width)

        w
        (p/display-width s)]

    (str s (apply str (repeat (max 0 (- width w)) \space)))))

(defn- shrink-table-widths
  "Shrink natural cell widths to fit `cap` terminal columns, preserving
   at least one column per cell when possible. If the chrome alone is
   wider than `cap`, callers still truncate the final lines as a last
   resort."
  [widths cap]
  (let [widths
        (mapv #(max 1 (long %)) widths)

        cols
        (count widths)

        budget
        (- (long cap) (+ 1 (* 3 cols)))]

    (cond (empty? widths) widths
          (>= (long budget) (long (reduce + widths))) widths
          :else (loop [ws widths]
                  (if (or (<= (long (reduce + ws)) (long budget)) (every? #(<= (long %) 1) ws))
                    ws
                    (let [max-w (apply max ws)
                          idx (first (keep-indexed (fn [i w]
                                                     (when (= w max-w) i))
                                                   ws))]

                      (recur (update ws idx dec))))))))

(defn- table-border-line
  [left join right widths]
  (p/boxed-horiz-line (map #(+ 2 (long %)) widths) (first left) (first join) (first right)))

(defn- table-data-line
  [cells widths]
  (str "│"
       (apply str
         (map-indexed (fn [i w]
                        (str " " (pad-right-cols (nth cells i "") w) " │"))
                      widths))))

(defn- wrap-cell-cols
  "Word-wrap a table cell's flat text to `width` display columns via the ONE
   shared, grapheme/EAW-aware word-wrap in the lanterna fork (`p/word-wrap`
   -> `TerminalTextUtils/wordWrap`) — the same text-flow family the screen
   paints with, so table-cell wrap points match every other wrapped surface.

   Always returns at least one line (blank input -> [\"\"]) so empty cells
   keep their grid row. A token wider than the column hard-breaks at grapheme
   boundaries; a single glyph wider than the column (emoji in a width-1
   column) lands alone on its own line, so the wrap always makes progress
   and the caller's `fit` clips the overflow."
  [s width]
  (p/word-wrap (str (or s "")) (max 1 (long width))))
(defn- table->lines
  "Render canonical `:table` IR as TUI table rows. Unlike the old plain
   projection fallback, this emits semantic header/separator/body line
   tags so the existing bubble painter can draw muted grid chrome,
   bold headers, and answer/thinking-zone backgrounds.

   Cell text WRAPS inside its column: when natural column widths exceed
   the bubble width they are shrunk (`shrink-table-widths`) and each
   over-wide cell word-wraps across as many physical rows as its tallest
   sibling in the logical row needs. Nothing is silently truncated —
   `fit` only clips the pathological case where the grid chrome alone
   is wider than the bubble."
  [node width _opts]
  (let [rows
        (vec (node-children node))

        header?
        (= :th
           (some-> rows
                   first
                   node-children
                   first
                   node-tag))

        raw-rows
        (mapv (fn [tr]
                (mapv table-cell-text (node-children tr)))
              rows)

        cols
        (apply max 0 (map count raw-rows))

        norm-rows
        (mapv (fn [row]
                (mapv #(or (nth row % nil) "") (range cols)))
              raw-rows)]

    (if (or (zero? (long cols)) (empty? norm-rows))
      []
      (let [natural-widths
            (vec (for [i (range cols)]
                   (apply max 1 (map #(p/display-width (nth % i "")) norm-rows))))

            cap
            (max 1 (long width))

            widths
            (shrink-table-widths natural-widths cap)

            fit
            (fn [s]
              (p/truncate-cols s cap))

            sep-line
            (fn [s tag]
              {:runs [{:text (fit s) :style #{:table} :node node}] :block-tag tag})

            ;; One LOGICAL row -> N physical grid rows: each cell wraps to
            ;; its column width, the row's height is its tallest cell, and
            ;; shorter cells pad with blank continuation lines.
            data-lines
            (fn [row tag]
              (let [cell-lines
                    (mapv (fn [cell w]
                            (wrap-cell-cols cell w))
                          row
                          widths)

                    height
                    (long (apply max 1 (map count cell-lines)))]

                (mapv (fn [j]
                        {:runs [{:text (fit (table-data-line (mapv #(nth % j "") cell-lines)
                                                             widths))
                                 :style #{:table}
                                 :node node}]
                         :block-tag tag})
                      (range height))))

            top
            (sep-line (table-border-line "┌" "┬" "┐" widths) :table-sep)

            mid
            (sep-line (table-border-line "├" "┼" "┤" widths) :table-sep)

            bottom
            (sep-line (table-border-line "└" "┴" "┘" widths) :table-sep)]

        (if header?
          (vec (concat [top]
                       (data-lines (first norm-rows) :table-head)
                       [mid]
                       (mapcat #(data-lines % :table-row) (rest norm-rows))
                       [bottom]))
          (vec (concat [top] (mapcat #(data-lines % :table-row) norm-rows) [bottom])))))))

(defn- tag-lines
  "Stamp every produced line with `:block-tag` so downstream adapters
   (sentinel-string emitter, click/select region builder) can map
   each rendered line back to its semantic source.  For headings we
   also propagate `:level` so adapters can pick H1/H2/H3 markers.

   Lines already tagged `:code` (a fenced code block nested inside a
   list / quote, tagged by the inner `block->lines` recursion) are left
   untouched so they keep their `MARKER_MD_CODE` row marker and still
   draw the code-block band, instead of being re-stamped to the
   enclosing `:ul`/`:ol`/`:quote` tag."
  [lines block-tag & {:keys [level]}]
  (mapv (fn [l]
          (if (= :code (:block-tag l))
            l
            (cond-> (assoc l :block-tag block-tag)
              level
              (assoc :block-level level))))
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

        (conj (vec (tag-lines ls :p)) (assoc (empty-line) :block-tag :outer-margin)))

      :h
      (let [level (or (:level (node-attrs node)) 1)
            ls (inline-block->lines (node-children node) width opts #{:bold :heading} nil)
            ls (if (seq ls) ls [(empty-line)])]

        (conj (vec (tag-lines ls :h :level level)) (assoc (empty-line) :block-tag :outer-margin)))

      :code
      ;; Code blocks carry two kinds of blank rows:
      ;;   :outer-margin   bubble-bg spacer above/below the chip
      ;;   :code           inside padding above/below content (code bg)
      ;; The outer-margin leader/trailer matches what every other block
      ;; emits, so the dedup pass in `ast->lines` collapses adjacent
      ;; outer-margin blanks regardless of which sibling block produced
      ;; them. The inner :code padding is deliberately preserved.
      (vec (concat [(assoc (empty-line) :block-tag :outer-margin)]
                   (tag-lines (code-block->lines node width opts) :code)
                   [(assoc (empty-line) :block-tag :outer-margin)]))

      :ul
      (conj (vec (tag-lines (list->lines :ul (node-children node) width opts) :ul))
            (assoc (empty-line) :block-tag :outer-margin))

      :ol
      (conj (vec (tag-lines (list->lines :ol (node-children node) width opts) :ol))
            (assoc (empty-line) :block-tag :outer-margin))

      :quote
      ;; `quote->lines` strips per-paragraph outer-margin blanks so
      ;; the bar reads as one solid block. We DO append an outer-margin
      ;; AFTER the whole block here so follow-up content breathes the
      ;; same way it does after `:p` / `:ul` / `:table` blocks.
      (conj (vec (tag-lines (quote->lines (node-children node) width opts) :quote))
            (assoc (empty-line) :block-tag :outer-margin))

      :table
      (conj (vec (table->lines node width opts)) (assoc (empty-line) :block-tag :outer-margin))

      ;; unknown / leftover inline at block position
      [(assoc (empty-line) :block-tag :outer-margin)])))

;; =============================================================================
;; Public API
;; =============================================================================

(defn ast->lines
  "Walk canonical IR (or any input that `ir/->ast` accepts) at a given
   terminal `width` and return a vector of styled lines."
  ([input width] (ast->lines input width nil))
  ([input width opts]
   (let [ast
         (ir/->ast input)

         body
         (drop 2 ast)

         ; canonical: [:ast {} & blocks]
         lines
         (blocks->lines body width (or opts {}))

         lines
         (vec lines)

         ;; Collapse runs of duplicate blank lines. Every block emits
         ;; its trailing spacer with `:block-tag :outer-margin`, so two
         ;; adjacent outer-margin blanks (regardless of which sibling
         ;; produced them — `:p`, `:h`, `:ul`, …, or the `:code`
         ;; leader/trailer) merge into one. Inside-code padding keeps
         ;; the `:code` tag and is left untouched so fenced blocks
         ;; still draw their full chrome.
         lines
         (loop [out
                []

                prev-blank-tag
                nil

                ls
                (seq lines)]

           (if (nil? ls)
             out
             (let [l
                   (first ls)

                   blank?
                   (line-blank? l)

                   blank-tag
                   (when blank? (:block-tag l))]

               (recur (if (and blank? (= prev-blank-tag blank-tag)) out (conj out l))
                      blank-tag
                      (next ls)))))

         ;; Drop leading + trailing outer-margin blanks. Preserve
         ;; `:code` blanks because they are inside-code padding; the
         ;; bookend guard below re-adds neutral outer margin when a
         ;; code block touches the answer edge.
         lines
         (vec (drop-while #(and (line-blank? %) (not= :code (:block-tag %))) lines))

         lines
         (vec (reverse (drop-while #(and (line-blank? %) (not= :code (:block-tag %)))
                                   (reverse lines))))

         ;; Code-block bookend guard: when a `:code` block sits at the
         ;; very top or bottom of an answer, the trim above will have
         ;; removed every outer-margin spacer next to it, leaving the
         ;; code chip flush against the bubble edge. Re-insert a single
         ;; `:outer-margin` blank so the chip gets one row of bubble-bg
         ;; padding on each touching edge. Mirrors the `:code` leader/
         ;; trailer emission in `block->lines`.
         lines
         (cond->> lines
           (= :code (:block-tag (first lines)))
           (into [(assoc (empty-line) :block-tag :outer-margin)]))

         lines
         (cond-> lines
           (= :code (:block-tag (peek lines)))
           (conj (assoc (empty-line) :block-tag :outer-margin)))

         lines
         (mapv trim-trailing-ws lines)]

     (if-let [n (:max-lines opts)]
       (vec (take n lines))
       lines))))

(defn- ast-text-chars
  "Sum of all text-content character counts in an IR node tree. Cheap
   recursive walk - just counts string leaves. Used by
   `ast->lines-tail` as a per-block height estimator.

   Uses `.length` on Strings (direct char-array length, no Counted
   dispatch) and `.count` on the IPersistentVector node so neither
   goes through `clojure.core/count`'s reflective fast-path table."
  ^long [node]
  (cond (string? node) (long (.length ^String node))
        (vector? node) (let [^clojure.lang.IPersistentVector v
                             node

                             n
                             (long (.count v))]

                         (loop [i
                                (long 1)

                                acc
                                (long 0)]

                           (if (>= i n) acc (recur (inc i) (+ acc (ast-text-chars (.nth v i)))))))
        :else 0))

(defn ast->lines-tail
  "Render only the last `tail-n` styled lines of the IR.

   Walks the top-level blocks BACKWARD, accumulating a cheap
   per-block line estimate (text-chars / content-width), until
   accumulated estimate ≥ `tail-n * 2` (slack covers blank-collapse
   + per-block trailing blanks). Then renders ONLY those tail blocks
   via the normal `ast->lines` and `(take-last tail-n)`.

   Per-frame cost = O(visible-tail), independent of total body
   length. The semantically-correct path for an auto-scrolled
   tail-pinned bubble (vs the broken `:max-lines` cap which would
   give the FIRST tail-n).

   `tail-n` must be positive. If the body has fewer total estimated
   lines than the budget, this falls back to a full walk."
  ([input width tail-n] (ast->lines-tail input width tail-n nil))
  ([input width tail-n opts]
   (let [tail-n
         (long tail-n)

         ast
         (ir/->ast input)

         blocks
         (vec (drop 2 ast))

         nb
         (count blocks)

         content-w
         (max 1 (long width))

         ;; 2x slack covers post-walk blank collapse + estimator slop
         budget
         (long (* 2 tail-n))

         picked-from
         (loop [i
                (dec nb)

                accum
                0]

           (cond (neg? i) 0
                 (>= accum budget) (inc i)
                 :else (let [b
                             (nth blocks i)

                             est
                             (max 1
                                  (long (Math/ceil (/ (double (max 1 (ast-text-chars b)))
                                                      (double content-w)))))]

                         (recur (dec i) (+ accum est)))))

         tail-blocks
         (subvec blocks picked-from)

         synth-ir
         (into [:ast {}] tail-blocks)

         tail-lines
         (ast->lines synth-ir width opts)]

     (if (<= (count tail-lines) tail-n) tail-lines (vec (take-last tail-n tail-lines))))))

(defn ast->lines-window
  "Render only rows `[start, start+num)` of the IR.

   Internally walks blocks forward with `:max-lines (start + num +
   slack)` so the walker short-circuits via the A1 `blocks->lines`
   path - work is O(start + num), not O(body). Output is
   bit-identical to `(subvec (ast->lines ir w opts) start (+ start
   num))` when both are in range; result is clamped if the body has
   fewer lines.

   Use cases:
     - `start = 0`: equivalent to `(:max-lines num)` head-cap.
     - `start > 0`: scrolled into a long bubble; only render the
       window. Note: skipping is still done by the walker producing
       `start` lines then discarding them - true zero-cost prefix
       skip would require per-block estimate-based skipping which
       this fn does not do.

   `num` must be positive. `start` must be non-negative."
  ([input width start num] (ast->lines-window input width start num nil))
  ([input width start num opts]
   (let [start
         (max 0 (long start))

         num
         (max 1 (long num))

         ;; 50% slack covers post-walk blank-collapse (lines vec ends
         ;; up shorter than walker output). Without slack, a window
         ;; near the body bottom could return fewer than `num` lines.
         cap
         (long (+ start (* 3 num)))

         lines
         (ast->lines input width (assoc (or opts {}) :max-lines cap))

         have
         (count lines)]

     (if (>= start have) [] (vec (subvec lines start (min have (+ start num))))))))

(defn lines->plain
  "Concatenate the text of every run in `lines`. Useful for tests +
   clipboard fallback (preferred clipboard path: `ir/render :markdown`)."
  ^String [lines]
  (str/join "\n"
            (map (fn [l]
                   (apply str
                     (map (fn [r]
                            (or (:text r) ""))
                          (:runs l))))
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
  {:h1 p/MARKER_MD_H1
   :h2 p/MARKER_MD_H2
   :h3 p/MARKER_MD_H3
   :code p/MARKER_MD_CODE
   :bullet p/MARKER_MD_BULLET
   :quote p/MARKER_MD_QUOTE
   :table p/MARKER_MD_TABLE_ROW
   :table-head p/MARKER_MD_TABLE_HEAD
   :table-sep p/MARKER_MD_TABLE_SEP
   :table-row p/MARKER_MD_TABLE_ROW
   :plain p/MARKER_ANSWER_TXT})

(def ^:private thinking-marker-set
  {:h1 p/MARKER_TH_MD_H1
   :h2 p/MARKER_TH_MD_H2
   :h3 p/MARKER_TH_MD_H3
   :code p/MARKER_TH_MD_CODE
   :bullet p/MARKER_TH_MD_BULLET
   :quote p/MARKER_TH_MD_QUOTE
   :table p/MARKER_TH_MD_TABLE_ROW
   :table-head p/MARKER_TH_MD_TABLE_HEAD
   :table-sep p/MARKER_TH_MD_TABLE_SEP
   :table-row p/MARKER_TH_MD_TABLE_ROW
   :plain p/MARKER_THINKING})

(def ^:private channel-marker-set
  ;; Channel/tool IR renders in place inside the surrounding bubble.
  ;; Structural IR (headings, code, lists, tables) keeps explicit row
  ;; styling; plain paragraphs get no hidden answer/result background.
  {:h1 p/MARKER_MD_H1
   :h2 p/MARKER_MD_H2
   :h3 p/MARKER_MD_H3
   :code p/MARKER_MD_CODE
   :bullet p/MARKER_MD_BULLET
   :quote p/MARKER_MD_QUOTE
   :table p/MARKER_MD_TABLE_ROW
   :table-head p/MARKER_MD_TABLE_HEAD
   :table-sep p/MARKER_MD_TABLE_SEP
   :table-row p/MARKER_MD_TABLE_ROW
   :plain ""})

(defn- marker-set-for
  [mode]
  (case mode
    :thinking
    thinking-marker-set

    (:channel :tool)
    channel-marker-set

    answer-marker-set))

(defn- block-marker-for
  "Pick the block marker for a `:block-tag` (and optional heading
   `:level`) from the `mode` marker set (`:answer` or `:thinking`)."
  ^String [marker-set block-tag level]
  (case block-tag
    :h
    (case (long (or level 1))
      1
      (:h1 marker-set)

      2
      (:h2 marker-set)

      (:h3 marker-set))

    :code
    (:code marker-set)

    :ul
    (:bullet marker-set)

    :ol
    (:bullet marker-set)

    :li
    (:bullet marker-set)

    :quote
    (:quote marker-set)

    :table
    (:table marker-set)

    :table-head
    (:table-head marker-set)

    :table-sep
    (:table-sep marker-set)

    :table-row
    (:table-row marker-set)

    ;; :p, nil, anything else → plain text marker
    (:plain marker-set)))

(defn- run->sentinel-segment
  "Wrap a run's text with the inline-span sentinels its style flags
   demand. Bold / italic / code / link each get an ON…OFF pair."
  ^String [{:keys [text style]}]
  (let [text (or text "")]
    (cond-> text
      (contains? style :code)
      (->> (str p/INLINE_CODE_ON))

      (contains? style :code)
      (str p/INLINE_CODE_OFF)

      (contains? style :italic)
      (->> (str p/INLINE_ITALIC_ON))

      (contains? style :italic)
      (str p/INLINE_ITALIC_OFF)

      (contains? style :bold)
      (->> (str p/INLINE_BOLD_ON))

      (contains? style :bold)
      (str p/INLINE_BOLD_OFF)

      (contains? style :link)
      (->> (str p/INLINE_LINK_ON))

      (contains? style :link)
      (str p/INLINE_LINK_OFF))))

(defn- line-link-spans
  "Clickable inline-link spans for one walker line's runs. Returns a vec of
   `{:col C :width W :url U}` — the display-COLUMN offset from the visible
   body start, the terminal-column width, and the target href — one entry
   per CONTIGUOUS stretch of runs sharing the same href (a link whose label
   carries bold/italic splits into several runs but reads as one region).
   `nil` when the line has no link runs. Column offsets are body-relative;
   the painter adds the line's absolute `x`/`row`. Block/inline markers are
   zero-width, so this offset matches the painted column."
  [runs]
  (not-empty
    (:spans
      (reduce (fn [{:keys [col spans]} {:keys [text href style]}]
                (let [w
                      (p/display-width (or text ""))

                      link?
                      (and href (contains? style :link))

                      prev
                      (peek spans)]

                  {:col (+ (long col) w)
                   :spans (cond (not link?) spans
                                ;; extend the previous span iff same href AND adjacent
                                (and prev
                                     (= (:url prev) href)
                                     (= (+ (long (:col prev)) (long (:width prev))) (long col)))
                                (conj (pop spans) (update prev :width + w))
                                :else (conj spans {:col (long col) :width w :url href}))}))
              {:col 0 :spans []}
              runs))))

(defn- line-body-sentinels
  "Inline-sentinel body string for one walker line's runs. A BLOCK code line
   (`:block-tag :code`) already paints a full-width code band via its marker,
   so its runs' inline `:code` flag is stripped — otherwise INLINE_CODE_ON/OFF
   would double-paint a narrow inline highlight (the `` look) and, since
   INLINE_CODE_ON forces code-fg/bg, clobber embedded diff/zprint ANSI colour.
   Inline `` `code` `` spans in prose keep theirs (block-tag is not `:code`)."
  ^String [block-tag runs]
  (let [runs (if (= block-tag :code)
               (mapv #(update %
                              :style
                              (fn [s]
                                (disj (or s #{}) :code)))
                     runs)
               runs)]
    (apply str (map run->sentinel-segment runs))))

(defn lines->sentinel-strings
  "Convert walker output (vector of `{:runs :block-tag :block-level?}`
   maps) into the painter's sentinel-prefixed string contract. Each
   line: `<block-marker><inline-sentinel-wrapped body>`. `:mode`
   selects the marker set (`:answer` default, `:thinking`, or `:channel`)."
  ([lines] (lines->sentinel-strings lines nil))
  ([lines opts]
   (let [ms (marker-set-for (:mode opts))]
     (mapv (fn [{:keys [runs block-tag block-level]}]
             (str (block-marker-for ms block-tag block-level) (line-body-sentinels block-tag runs)))
           lines))))

(defn ast->sentinel-strings
  "One-shot helper: canonical IR → vector of sentinel-prefixed strings
   ready for the existing bubble painter. Composes `ast->lines` with
   the sentinel adapter. `:mode` (`:answer`, `:thinking`, or `:channel`)
   selects the marker set."
  ([ir width] (ast->sentinel-strings ir width nil))
  ([ir width opts] (lines->sentinel-strings (ast->lines ir width opts) opts)))

(defn ast->inline-sentinel-string
  "Flatten a canonical IR into a single sentinel-wrapped inline string
   suitable for chrome-row labels. NO block markers — just inline
   sentinels (`INLINE_BOLD_ON/OFF`, `INLINE_CODE_ON/OFF`, etc.) wrapping
   styled runs. Hard breaks `[:br]` become spaces (single line
   contract). Used by chrome labels and other surfaces that previously
   ran through `markdown->inline`."
  ^String [ir]
  (let [;; pass huge width so walker never wraps; we want one flat run
        lines
        (ast->lines ir Integer/MAX_VALUE)

        ;; concat all runs across all lines, joining lines with single space
        line-strs
        (mapv (fn [{:keys [runs]}]
                (apply str (map run->sentinel-segment runs)))
              lines)]

    (str/replace (str/join " " (remove str/blank? line-strs)) #"\s+" " ")))

(defn ast->entries
  "Drop-in replacement for the legacy `render/markdown->entries`.
   Returns a vector of `{:line :meta}` maps where `:line` is the
   sentinel-prefixed string the bubble painter consumes, and `:meta`
   is per-line click-region metadata. `nil` `:meta` for ordinary content
   lines.

   `:mode` selects the marker set:
     `:answer`   (default) — answer-zone PUA chars (answer-bg paint)
     `:thinking` — thinking-zone PUA chars (iter-header-bg + italic)
     `:channel`  — in-place channel/tool IR; plain paragraphs use no
                   background marker, structural rows keep explicit styling

   This is the IR-side analogue of `markdown->entries`. Every
   bubble rendering path that used to parse the rendered markdown
   back into entries should call this directly on the canonical IR
   — no markdown round-trip."
  ([ir width] (ast->entries ir width nil))
  ([ir width opts]
   (let [tail-n
         (:tail-lines opts)

         lines
         (if tail-n (ast->lines-tail ir width (long tail-n) opts) (ast->lines ir width opts))

         ms
         (marker-set-for (:mode opts))]

     (mapv (fn [{:keys [runs block-tag block-level meta]}]
             (let [links (line-link-spans runs)]
               {:line (str (block-marker-for ms block-tag block-level)
                           (line-body-sentinels block-tag runs))
                :meta (if links (assoc (or meta {}) :links links) meta)}))
           lines))))
