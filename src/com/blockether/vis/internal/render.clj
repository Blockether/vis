(ns com.blockether.vis.internal.render
  "Vis answer IR — Hiccup-EDN, MDAST-equivalent, with strict canonical
   form post-`->ast`.

   Public surface:
     (->ast input)                 ; soft-normalize any input → [:ir & blocks]
     (render input flavor opts)    ; one of :html :markdown :plain
     (extract-code ast)            ; for vis run --code
     (extract-text ast)            ; for voice TTS
     (conversation->markdown db conv-ref opts?)

   Tags:
     ROOT             :ir
     BLOCKS    (13)   :p :h{:level 1-6} :code{:lang} :ul :ol{:start} :li
                      :quote :table :tr :th :td
                      :details{:open?} :summary
     INLINES   (11)   :span{:preserve-ws? :nowrap?} :br
                      :strong :em :c :a{:href}
                      :img{:src :alt} :kbd :mark :sup :sub

   `:details` is the canonical disclosure widget (LLM-emitted
   collapsible section). Children: exactly one `:summary` head
   followed by zero-or-more body blocks. `{:open? true}` opens by
   default; the TUI bubble painter toggles state via the click
   region attached to the rendered summary line.

   ─── Canonical form (invariant after `->ast`) ───────────────────────────────

   1. Every vector node has its attrs map at index 1 ({} when absent).
   2. `:ir` children are exclusively block nodes.
   3. Text lives ONLY in:
        - `:span` body (single string, no '\\n').
        - raw bodies of `:code`, `:c`, `:kbd` (single string, ws preserved).
      Anywhere else, vector children only — no bare strings in the tree.
   4. Hard line breaks are explicit `[:br {}]` nodes.
   5. Soft breaks (any '\\n' inside a non-preserve-ws string) are collapsed
      to a single space during canonicalization. This is the structural fix
      for LLM output that emits cosmetic mid-paragraph indentation
      (e.g. `\" \\n   continuation\"`).
   6. `:li` children are either all blocks OR a single `:p` wrapping the
      inline run.
   7. `:ul`/`:ol` children are exclusively `:li`.
   8. `:table` children are `:tr`; `:tr` children are `:th`/`:td`;
      `:th`/`:td` children are inline.

   ─── Coercion rules at the boundary ─────────────────────────────────────────

   `->ast` is total and pure. Accepted inputs:
     [:ir ...]                    — re-canonicalized (idempotent)
     [:tag ...] (Hiccup, non-:ir) — wrapped in [:ir <node>]
     \"text\"                       — wrapped in [:ir [:p [:span text]]]
     sequential / vector of mixed — element-by-element coercion
     anything else                — surfaced as [:code {:lang \"edn\"} pr-str]"
  (:require
   [clojure.string :as str]
   [com.blockether.vis.internal.persistance :as persistance])
  (:import
   [org.commonmark.ext.gfm.strikethrough Strikethrough StrikethroughExtension]
   [org.commonmark.ext.gfm.tables TableBlock TableCell TablesExtension]
   [org.commonmark.node BlockQuote BulletList Code Emphasis FencedCodeBlock
    HardLineBreak Heading HtmlBlock HtmlInline Image IndentedCodeBlock Link
    Node OrderedList Paragraph SoftLineBreak StrongEmphasis Text ThematicBreak]
   [org.commonmark.parser Parser]))

(set! *warn-on-reflection* true)

;; =============================================================================
;; Tag taxonomy
;; =============================================================================

(def ^:private block-tags
  #{:p :h :code :ul :ol :li :quote :table :tr :th :td :details :summary})

(def ^:private inline-tags
  #{:span :br :strong :em :c :a :img :kbd :mark :sup :sub})

(def ^:private void-inline-tags
  "Inline tags whose canonical form has no children."
  #{:br :img})

(def ^:private raw-text-tags
  "Tags whose canonical body is a single raw string (whitespace preserved)."
  #{:code :c :kbd})

(defn- block? [x] (and (vector? x) (contains? block-tags (first x))))
(defn- inline? [x] (and (vector? x) (contains? inline-tags (first x))))

;; =============================================================================
;; Canonicalization
;; =============================================================================

(defn- collapse-soft-breaks
  "Replace every run of `\\s*\\n\\s*` with a single space. Non-newline
   whitespace is preserved verbatim (so ` — ` between inline tokens
   stays intact)."
  ^String [^String s]
  (str/replace s #"\s*\n\s*" " "))

(defn- ensure-attrs
  "Insert {} attrs at index 1 when absent."
  [v]
  (cond
    (not (vector? v))   v
    (empty? v)          v
    (map? (second v))   v
    :else               (into [(first v) {}] (rest v))))

(defn- has-attrs? [v]
  (and (vector? v) (>= (count v) 2) (map? (second v))))

;; ─── canonical? predicates ──────────────────────────────────────────────
;; Pure structural checks; defined here so canon-* and `->ast` can call
;; them without forward declares. Self-recursive, not mutually recursive.

(defn- inline-canonical?
  "True when `x` is already a canonical inline node."
  [x]
  (and (vector? x)
    (>= (count x) 2)
    (map? (nth x 1))
    (let [tag (nth x 0)]
      (case tag
        :br   (= 2 (count x))
        :img  (= 2 (count x))
        :span (and (= 3 (count x))
                (string? (nth x 2))
                (not (str/includes? (nth x 2) "\n")))
        (:c :code :kbd)
        (and (= 3 (count x)) (string? (nth x 2)))
        (:strong :em :a :mark :sup :sub)
        (every? inline-canonical? (drop 2 x))
        false))))

(defn- block-canonical?
  "True when `x` is a canonical block node."
  [x]
  (and (vector? x)
    (>= (count x) 2)
    (map? (nth x 1))
    (let [tag (nth x 0)
          children (drop 2 x)]
      (case tag
        (:p :h :th :td)
        (every? inline-canonical? children)

        :code
        (and (= 3 (count x)) (string? (nth x 2)))

        (:ul :ol)
        (every? #(and (vector? %) (= :li (first %)) (block-canonical? %)) children)

        :li
        (or (every? block-canonical? children)
          (and (= 1 (count children))
            (vector? (first children))
            (= :p (first (first children)))
            (block-canonical? (first children))))

        :quote
        (every? block-canonical? children)

        :table
        (every? #(and (vector? %) (= :tr (first %)) (block-canonical? %)) children)

        :tr
        (every? #(and (vector? %) (#{:th :td} (first %)) (block-canonical? %)) children)

        :details
        (and (seq children)
          (let [head (first children)]
            (and (vector? head) (= :summary (first head)) (block-canonical? head)))
          (every? block-canonical? (rest children)))

        :summary
        (every? inline-canonical? children)

        false))))

(defn canonical?
  "Cheap structural check: `x` is already a canonical `[:ir & blocks]`
   AST. When true, `(->ast x)` is the identity (returns the same
   object), so downstream caches keyed on `System/identityHashCode`
   hit cleanly across repeated render passes."
  [x]
  (and (vector? x)
    (= :ir (first x))
    (>= (count x) 2)
    (map? (nth x 1))
    (every? block-canonical? (drop 2 x))))

;; ─── Canonicalization (canon-*) ─────────────────────────────────────────
;; Mutual recursion across blocks/inlines (canon-block <-> canon-block-rebuild
;; <-> canon-li-children <-> canon-block; canon-inline-children <-> canon-
;; inline-node). Per AGENTS.md `declare` is allowed for genuine mutual
;; recursion that no reordering can resolve.
(declare ^:private canon-block ^:private canon-block-rebuild
  ^:private canon-inline-children)

(defn- text-flatten
  "Concatenate every string anywhere in `x` (depth-first). Used to
   collapse a raw-text container's children into a single body string,
   regardless of whether the input is `[:c \"raw\"]` (LLM-flavored)
   or `[:c [:span \"raw\"]]` (re-canonicalization input)."
  ^String [x]
  (cond
    (string? x)     x
    (vector? x)     (apply str (map text-flatten (drop 2 x)))
    (sequential? x) (apply str (map text-flatten x))
    (nil? x)        ""
    :else           ""))

(defn- string->span
  "Lift a raw text string into a [:span ...] node, collapsing soft breaks
   unless `preserve-ws?` is set. Empty strings become nil (filtered out)."
  [^String s preserve-ws?]
  (let [text (if preserve-ws? s (collapse-soft-breaks s))]
    (when-not (= "" text)
      [:span (cond-> {} preserve-ws? (assoc :preserve-ws? true)) text])))

(defn- canon-inline-node
  "Canonicalize one inline vector. `preserve-ws?` propagates from
   ancestor `:c`/`:kbd`/`:span{:preserve-ws? true}`. Identity-preserving
   on already-canonical input."
  [node preserve-ws?]
  ;; Fast path: if this single inline node is already canonical at
   ;; this level, return it unchanged so parent vectors can stay
   ;; `identical?` too.
  (if (inline-canonical? node)
    node
    (let [orig     node
          node     (ensure-attrs node)
          tag      (first node)
          attrs    (second node)
          children (drop 2 node)]
      (cond
        (contains? void-inline-tags tag)
        (if (and (= 2 (count orig)) (map? (nth orig 1 nil))) orig [tag attrs])

        (contains? raw-text-tags tag)
        [tag attrs (text-flatten children)]

        (= :span tag)
        (let [pw?  (or (:preserve-ws? attrs) preserve-ws?)
              text (text-flatten children)
              text (if pw? text (collapse-soft-breaks text))]
          (if (= "" text)
            nil
            [:span attrs text]))

        :else
        (let [child-nodes (canon-inline-children children preserve-ws?)]
          (into [tag attrs] child-nodes))))))

(defn- map-keep-identity
  "Like `mapv` but returns the input vector unchanged when `f` is
   identity for every element — the building block for sub-tree
   identity preservation across canonicalization passes.

   Pairs with `clojure+.walk/walk` semantics: a transformation that
   does nothing in net should leave the data structure `identical?`
   so downstream `System/identityHashCode` caches stay hot."
  [f xs]
  (let [xs       (vec xs)
        n        (count xs)
        ys       (object-array n)
        changed? (volatile! false)]
    (dotimes [i n]
      (let [x  (nth xs i)
            x' (f x)]
        (when-not (identical? x' x) (vreset! changed? true))
        (aset ys i x')))
    (if @changed? (vec ys) xs)))

(defn- canon-inline-children
  "Walk a sibling sequence of inline content (strings + inline vectors)
   and return a vector of canonical inline nodes. Identity-preserving
   on already-canonical input."
  [children preserve-ws?]
  ;; Two-step: lift any bare strings/nils into spans (always allocates
   ;; for non-vector children), then identity-preserve over inline
   ;; vectors so unchanged sub-trees stay `identical?`.
  (let [lifted (vec
                 (keep
                   (fn [c]
                     (cond
                       (string? c) (string->span c preserve-ws?)
                       (vector? c) c
                       (nil? c)    nil
                       :else       (string->span (str c) preserve-ws?)))
                   children))]
    (map-keep-identity
      (fn [c]
        (if (vector? c)
          (canon-inline-node c preserve-ws?)
          c))
      lifted)))

(defn- canon-li-children
  "All-blocks OR all-inlines (wrapped in a single :p). Mixed input is
   bucketed in source order, with consecutive inline runs each
   wrapped in their own :p."
  [children]
  (let [classified (mapv (fn [c] (cond
                                   (and (vector? c) (block? c))  :block
                                   (and (vector? c) (inline? c)) :inline
                                   (string? c)                   :inline
                                   :else                         :inline))
                     children)]
    (cond
      (every? #(= :block %) classified)
      (map-keep-identity canon-block children)

      (every? #(= :inline %) classified)
      (let [inline-children (canon-inline-children children false)]
        (if (seq inline-children)
          [(into [:p {}] inline-children)]
          []))

      :else
      (loop [out [] buf [] cs (seq children)]
        (let [flush (fn [out buf]
                      (let [inl (canon-inline-children buf false)]
                        (if (seq inl) (conj out (into [:p {}] inl)) out)))]
          (if (nil? cs)
            (flush out buf)
            (let [c (first cs)]
              (if (and (vector? c) (block? c))
                (recur (conj (flush out buf) (canon-block c)) [] (next cs))
                (recur out (conj buf c) (next cs))))))))))

(defn- canon-block-children
  "Sequence of children of a block parent that itself accepts inline
   content (`:p`, `:h`, `:quote`, `:th`, `:td`). Strings + inline
   vectors get canonicalized into spans/inlines."
  [children]
  (canon-inline-children children false))

(defn- canon-blocks-strict
  "For parents whose children must all be blocks (`:ir`, `:quote`,
   `:ul`/`:ol` after :li-coercion is handled separately). Any loose
   inline / string is bucketed into a `:p`. Identity-preserving on
   the common case where every child is already a canonical block
   (the only path that actually allocates)."
  [children]
  (let [children (vec children)
        ;; fast path: every child already a canonical block → just
        ;; identity-preserve over canon-block.
        all-blocks? (every? #(and (vector? %) (block? %)) children)]
    (if all-blocks?
      (map-keep-identity canon-block children)
      ;; slow path: buffer loose inlines into :p.
      (loop [out [] buf [] cs (seq children)]
        (let [flush (fn [out buf]
                      (let [inl (canon-inline-children buf false)]
                        (if (seq inl) (conj out (into [:p {}] inl)) out)))]
          (if (nil? cs)
            (flush out buf)
            (let [c (first cs)]
              (cond
                (and (vector? c) (block? c))
                (recur (conj (flush out buf) (canon-block c)) [] (next cs))

                :else
                (recur out (conj buf c) (next cs))))))))))

(defn- canon-block
  "Canonicalize one block vector. Identity-preserving on already-
   canonical input."
  [node]
  (if (block-canonical? node)
    node
    (canon-block-rebuild node)))

(defn- canon-block-rebuild
  "Internal: rebuild a block via tag dispatch. Skipped via identity
   when `block-canonical?` is true at the call site."
  [node]
  (let [node     (ensure-attrs node)
        tag      (first node)
        attrs    (second node)
        children (drop 2 node)]
    (case tag
      :code  ; raw source; body = single string, ws preserved verbatim
      [:code attrs (text-flatten children)]

      :li
      (into [:li attrs] (canon-li-children children))

      (:ul :ol)
      (let [children (vec children)
            children' (map-keep-identity
                        (fn [c]
                          (let [c (ensure-attrs c)]
                            (if (and (vector? c) (= :li (first c)))
                              (canon-block c)
                              (canon-block [:li {} c]))))
                        children)]
        (if (and (= attrs (nth node 1 nil)) (identical? children' children))
          node
          (into [tag attrs] children')))

      :table
      (into [:table attrs]
        (mapv (fn [c]
                (let [c (ensure-attrs c)]
                  (if (and (vector? c) (= :tr (first c)))
                    (canon-block c)
                    (canon-block [:tr {} c]))))
          children))

      :tr
      (into [:tr attrs]
        (mapv (fn [c]
                (let [c (ensure-attrs c)]
                  (if (and (vector? c) (#{:th :td} (first c)))
                    (canon-block c)
                    (canon-block [:td {} c]))))
          children))

      (:th :td)
      (into [tag attrs] (canon-block-children children))

      (:p :h)
      (into [tag attrs] (canon-block-children children))

      :quote
      ;; quote contains blocks; loose inlines bucket into :p
      (into [:quote attrs] (canon-blocks-strict children))

      :summary
      ;; summary head: inline content only
      (into [:summary attrs] (canon-block-children children))

      :details
      ;; details: first child must be :summary; rest are body blocks.
      ;; A details with no summary gets a synthetic empty one so the
      ;; bubble painter always has a click target.
      (let [children (vec children)
            first-c  (first children)
            head     (if (and (vector? first-c) (= :summary (first first-c)))
                       (canon-block (ensure-attrs first-c))
                       (canon-block [:summary {} "Details"]))
            body     (let [tail (if (and (vector? first-c) (= :summary (first first-c)))
                                  (subvec children 1)
                                  children)]
                       (canon-blocks-strict tail))]
        (into [:details attrs head] body))

      ;; should not happen — caller dispatches by block?/inline?
      (into [tag attrs] (canon-block-children children)))))

(defn ->ast
  "Soft-normalize any answer-input value into canonical [:ir & blocks].
   Pure, total, idempotent.

   Identity-preserving: when the input already satisfies the canonical
   invariants (`canonical?`), the return value is the SAME object.
   This keeps downstream `System/identityHashCode` caches
   (`format-answer-with-thinking-data`, etc.) hot across repeated
   render passes — walker output is computed once per canonical IR
   identity, not once per equal-but-fresh allocation.

   See namespace docstring for the full canonical-form invariants."
  [v]
  (if (canonical? v)
    v   ;; identical preserved — cache-friendliness fast path
    (let [raw-children
          (cond
            (and (vector? v) (= :ir (first v)))
            (let [v (ensure-attrs v)] (drop 2 v))

            (string? v)
            [v]

            (and (vector? v) (keyword? (first v)))
            [v]

            (sequential? v)
            (seq v)

            :else
            [[:code {:lang "edn"} (pr-str v)]])

          coerced
          (mapv (fn [x]
                  (cond
                    (string? x)                              x
                    (and (vector? x) (keyword? (first x)))   x
                    (nil? x)                                  nil
                    :else                                     [:code {:lang "edn"} (pr-str x)]))
            raw-children)]
      (into [:ir {}] (canon-blocks-strict (filter some? coerced))))))

(defn ir?
  "True when x is a canonical [:ir ...] AST."
  [x]
  (and (vector? x) (= :ir (first x))))

;; =============================================================================
;; text->ir — commonmark-java markdown parser → canonical IR
;; =============================================================================
;;
;; Used at the boundaries that DON'T have IR upstream:
;;   - LLM-emitted thinking strings (`:thinking` field on iterations);
;;   - user-typed input box messages.
;; Both arrive as plain markdown strings; this function lifts them into
;; canonical IR so the entire downstream pipeline (TUI walker, Telegram
;; renderer, exporter) sees one shape.

(def ^:private ^Parser md-parser
  (-> (Parser/builder)
    (.extensions [(TablesExtension/create)
                  (StrikethroughExtension/create)])
    (.build)))

(defn- cm-children-seq
  "Iterate `Node.getNext` linked list as a Clojure seq."
  [^Node node]
  (when node
    (loop [^Node n (.getFirstChild node) acc (transient [])]
      (if (nil? n)
        (persistent! acc)
        (recur (.getNext n) (conj! acc n))))))

;; Mutual: block parser walks block-children which may contain inlines;
;; inline parser walks inline-children which may contain nested inlines.
(declare ^:private cm->blocks ^:private cm->inlines)

(defn- cm->inline-node [^Node n]
  (cond
    (instance? Text n)             [:span {} (.getLiteral ^Text n)]
    (instance? Code n)             [:c {} (.getLiteral ^Code n)]
    (instance? StrongEmphasis n)   (into [:strong {}] (cm->inlines n))
    (instance? Emphasis n)         (into [:em {}] (cm->inlines n))
    (instance? Strikethrough n)    (into [:em {}] (cm->inlines n))   ; closest IR analogue
    (instance? Link n)             (into [:a {:href (.getDestination ^Link n)}] (cm->inlines n))
    (instance? Image n)            [:img {:src (.getDestination ^Image n)
                                          :alt (.getTitle ^Image n)}]
    (instance? SoftLineBreak n)    [:span {} " "]                    ; soft → single space
    (instance? HardLineBreak n)    [:br {}]
    (instance? HtmlInline n)       [:span {} (.getLiteral ^HtmlInline n)]
    :else                          [:span {} ""]))

(defn- cm->inlines [^Node parent]
  (mapv cm->inline-node (cm-children-seq parent)))

(defn- cm-list-item->li [^Node li]
  (let [block-children (cm-children-seq li)]
    (into [:li {}]
      (mapcat (fn [^Node b]
                (cond
                  (instance? Paragraph b)   [(into [:p {}] (cm->inlines b))]
                  :else                     (cm->blocks b)))
        block-children))))

(defn- cm->table-cell [^Node cell]
  (into (if (and (instance? TableCell cell) (.isHeader ^TableCell cell))
          [:th {}] [:td {}])
    (cm->inlines cell)))

(defn- cm->table-row [^Node row]
  (into [:tr {}] (mapv cm->table-cell (cm-children-seq row))))

(defn- cm->table [^Node tbl]
  (into [:table {}]
    (mapcat (fn [^Node section] (mapv cm->table-row (cm-children-seq section)))
      (cm-children-seq tbl))))

(defn- cm->blocks
  "Convert one commonmark Node into a vector of canonical IR block(s)."
  [^Node n]
  (cond
    (instance? Heading n)
    [(into [:h {:level (.getLevel ^Heading n)}] (cm->inlines n))]

    (instance? Paragraph n)
    [(into [:p {}] (cm->inlines n))]

    (instance? FencedCodeBlock n)
    [[:code {:lang (let [info (.getInfo ^FencedCodeBlock n)] (when (seq info) info))}
      (.getLiteral ^FencedCodeBlock n)]]

    (instance? IndentedCodeBlock n)
    [[:code {} (.getLiteral ^IndentedCodeBlock n)]]

    (instance? BulletList n)
    [(into [:ul {}] (mapv cm-list-item->li (cm-children-seq n)))]

    (instance? OrderedList n)
    [(into [:ol {:start (.getMarkerStartNumber ^OrderedList n)}]
       (mapv cm-list-item->li (cm-children-seq n)))]

    (instance? BlockQuote n)
    [(into [:quote {}] (mapcat cm->blocks (cm-children-seq n)))]

    (instance? ThematicBreak n)
    [[:hr {}]]

    (instance? TableBlock n)
    [(cm->table n)]

    (instance? HtmlBlock n)
    ;; Recognise <details>/<summary> disclosure blocks; everything else
    ;; becomes a verbatim paragraph so it stays visible.
    (let [literal (.getLiteral ^HtmlBlock n)]
      [[:p {} [:span {} literal]]])

    :else
    (mapcat cm->blocks (cm-children-seq n))))

(defn text->ir
  "Parse a plain-text or markdown string into canonical answer-IR.
   Idempotent: when the input is already canonical IR, returns it
   unchanged (`identical?` preserved — cache-friendly).

   Used at the boundaries that don't have IR upstream — thinking
   text from the model, per-block `:comment` strings, user-typed
   messages from the TUI input box. Returns canonical `[:ir &
   blocks]` directly (no further `->ast` round-trip needed). Empty /
   nil input yields `[:ir {}]`.

   Implementation: commonmark-java parser + GFM tables / strikethrough
   extensions, then a faithful Node→IR walker. Soft line breaks
   collapse to a single space; hard line breaks become `[:br]`."
  [text]
  (cond
    (canonical? text)
    text   ; identity-preserving fast path

    (or (nil? text) (= "" text))
    [:ir {}]

    (string? text)
    (let [doc    (.parse md-parser ^String text)
          blocks (vec (mapcat cm->blocks (cm-children-seq doc)))]
      (->ast (into [:ir {}] blocks)))

    :else
    ;; non-string, non-canonical — best-effort coerce via ->ast
    (->ast text)))

;; =============================================================================
;; Walker helpers (canonical inputs)
;; =============================================================================

(defn- node-tag [node]      (when (vector? node) (first node)))
(defn- node-attrs [node]    (if (has-attrs? node) (second node) {}))
(defn- node-children [node] (if (has-attrs? node) (drop 2 node) (rest node)))

(defn- raw-body
  "Body string for `:span`, `:c`, `:code`, `:kbd`. Empty string when
   absent."
  ^String [node]
  (or (some #(when (string? %) %) (node-children node)) ""))

(defn- escape-html [^String s]
  (-> s
    (str/replace "&" "&amp;")
    (str/replace "<" "&lt;")
    (str/replace ">" "&gt;")
    (str/replace "\"" "&quot;")))

(defn- escape-html-attr [^String s]
  (-> s
    (str/replace "&" "&amp;")
    (str/replace "\"" "&quot;")
    (str/replace "<" "&lt;")
    (str/replace ">" "&gt;")))

(defn- escape-md
  "Escape markdown-significant characters in plain text segments. Does
   not run inside `:code`/`:c` (those preserve verbatim)."
  [^String s]
  (-> s
    (str/replace "\\" "\\\\")
    (str/replace "*" "\\*")
    (str/replace "_" "\\_")
    (str/replace "`" "\\`")
    (str/replace "[" "\\[")
    (str/replace "]" "\\]")))

;; =============================================================================
;; HTML walker — Telegram-flavored
;; =============================================================================

;; render-html <-> render-html-children mutual: html walker dispatches per
;; tag, recursing into children via the helper which itself recurses back
;; into the walker.
(declare ^:private render-html)

(defn- render-html-children [nodes opts]
  (apply str (map #(render-html % opts) nodes)))

(defn- render-html-list [tag children {:keys [start] :as opts}]
  (let [n (atom (or start 1))]
    (apply str
      (map (fn [li]
             (let [marker (if (= tag :ul)
                            "• "
                            (let [m (str @n ". ")] (swap! n inc) m))
                   inner  (render-html-children (node-children li) opts)]
               (str marker inner "\n")))
        children))))

(defn- render-html-table [node opts]
  (let [rows (node-children node)
        cell-text (fn [cell] (-> cell node-children (render-html-children opts)))
        all-rows  (mapv (fn [tr] (mapv cell-text (node-children tr))) rows)
        widths    (when (seq all-rows)
                    (let [cols (apply max 0 (map count all-rows))]
                      (vec (for [i (range cols)]
                             (apply max 1 (map #(count (or (nth % i nil) "")) all-rows))))))
        pad       (fn [s w] (str s (apply str (repeat (max 0 (- w (count s))) " "))))
        fmt-row   (fn [row] (str/join "  " (map-indexed (fn [i c] (pad (or c "") (nth widths i 0))) row)))
        sep       (when widths
                    (str/join "  " (map #(apply str (repeat % "─")) widths)))
        first-row-is-header? (and (seq rows) (= :th (some-> rows first node-children first node-tag)))
        body      (if first-row-is-header?
                    (str (fmt-row (first all-rows)) "\n" sep "\n"
                      (str/join "\n" (map fmt-row (rest all-rows))))
                    (str/join "\n" (map fmt-row all-rows)))]
    (str "<pre>" (escape-html body) "</pre>")))

(defn- render-html [node opts]
  (cond
    (string? node) (escape-html node)        ; should not occur in canonical tree
    (not (vector? node)) (escape-html (str node))
    :else
    (let [tag      (node-tag node)
          attrs    (node-attrs node)
          children (node-children node)]
      (case tag
        :ir       (render-html-children children opts)

        :p        (str (render-html-children children opts) "\n\n")
        :h        (str "<b>" (render-html-children children opts) "</b>\n\n")

        :code     (let [{:keys [lang]} attrs
                        src (raw-body node)]
                    (if (seq lang)
                      (str "<pre><code class=\"language-" (escape-html-attr lang) "\">"
                        (escape-html src) "</code></pre>\n\n")
                      (str "<pre>" (escape-html src) "</pre>\n\n")))

        :ul       (str (render-html-list :ul children opts) "\n")
        :ol       (str (render-html-list :ol children (assoc opts :start (or (:start attrs) 1))) "\n")
        :li       (render-html-children children opts)

        :details  (let [head (first children)
                        body (rest children)
                        head-html (when head (render-html-children (node-children head) opts))]
                    (str "<blockquote" (when (:open? attrs) " expandable") ">"
                      (when head-html (str "<b>" head-html "</b>\n"))
                      (render-html-children body opts)
                      "</blockquote>\n\n"))
        :summary  (render-html-children children opts)

        :quote    (let [body (render-html-children children opts)]
                    (if (= :thinking (:context opts))
                      (str "<blockquote expandable>" body "</blockquote>\n\n")
                      (str "<blockquote>" body "</blockquote>\n\n")))

        :table    (str (render-html-table node opts) "\n\n")
        :tr       (render-html-children children opts)
        :th       (render-html-children children opts)
        :td       (render-html-children children opts)

        ;; --- inline ---
        :span     (escape-html (raw-body node))
        :br       "\n"
        :strong   (str "<b>" (render-html-children children opts) "</b>")
        :em       (str "<i>" (render-html-children children opts) "</i>")
        :c        (str "<code>" (escape-html (raw-body node)) "</code>")
        :a        (str "<a href=\"" (escape-html-attr (or (:href attrs) "")) "\">"
                    (render-html-children children opts) "</a>")
        :img      (str "<i>🖼 " (escape-html (or (:alt attrs) "image")) "</i>")
        :kbd      (str "<code>" (escape-html (raw-body node)) "</code>")
        :mark     (str "<b>" (render-html-children children opts) "</b>")
        :sup      (render-html-children children opts)   ; Telegram has no <sup>
        :sub      (render-html-children children opts)   ; Telegram has no <sub>

        ;; unknown tag — pass through children
        (render-html-children children opts)))))

;; =============================================================================
;; Markdown walker
;; =============================================================================

;; render-md <-> render-md-children mutual; same pattern as html.
(declare ^:private render-md)

(defn- render-md-children [nodes opts]
  (apply str (map #(render-md % opts) nodes)))

(defn- render-md-list [tag children {:keys [start] :as opts}]
  (let [n (atom (or start 1))
        ordered? (= tag :ol)]
    (apply str
      (map (fn [li]
             (let [marker (if ordered?
                            (let [m (str @n ". ")] (swap! n inc) m)
                            "- ")
                   inner  (render-md-children (node-children li) opts)
                   inner  (str/replace inner #"\n+$" "")]
               (str marker inner "\n")))
        children))))

(defn- render-md-table [node opts]
  (let [rows (node-children node)
        cell-md (fn [cell] (-> cell node-children (render-md-children opts)
                             (str/replace "|" "\\|") str/trim))
        all-rows (mapv (fn [tr] (mapv cell-md (node-children tr))) rows)]
    (if (empty? all-rows)
      ""
      (let [first-row-is-header? (= :th (some-> rows first node-children first node-tag))
            header (if first-row-is-header? (first all-rows) (mapv (constantly "") (first all-rows)))
            body   (if first-row-is-header? (rest all-rows) all-rows)
            cols   (count header)
            sep    (str/join " | " (repeat cols "---"))
            row-md (fn [row] (str "| " (str/join " | " row) " |"))]
        (str (row-md header) "\n| " sep " |\n"
          (str/join "\n" (map row-md body)) "\n\n")))))

(defn- render-md [node opts]
  (cond
    (string? node) (escape-md node)
    (not (vector? node)) (escape-md (str node))
    :else
    (let [tag      (node-tag node)
          attrs    (node-attrs node)
          children (node-children node)]
      (case tag
        :ir       (render-md-children children opts)

        :p        (str (render-md-children children opts) "\n\n")
        :h        (let [level (max 1 (min 6 (or (:level attrs) 1)))]
                    (str (apply str (repeat level "#")) " "
                      (render-md-children children opts) "\n\n"))

        :code     (let [{:keys [lang]} attrs
                        src (raw-body node)]
                    (str "```" (or lang "") "\n" src "\n```\n\n"))

        :ul       (str (render-md-list :ul children opts) "\n")
        :ol       (str (render-md-list :ol children (assoc opts :start (or (:start attrs) 1))) "\n")
        :li       (render-md-children children opts)

        :details  (let [head      (first children)
                        body      (rest children)
                        head-md   (when head (render-md-children (node-children head) opts))
                        open?     (boolean (:open? attrs))
                        open-tag  (if open? "<details open>" "<details>")]
                    (str open-tag "\n"
                      (when head-md (str "<summary>" head-md "</summary>\n\n"))
                      (render-md-children body opts)
                      "</details>\n\n"))
        :summary  (render-md-children children opts)

        :quote    (let [body (str/trim (render-md-children children opts))
                        prefixed (str/join "\n" (map #(str "> " %) (str/split-lines body)))]
                    (str prefixed "\n\n"))

        :table    (render-md-table node opts)
        :tr       (render-md-children children opts)
        :th       (render-md-children children opts)
        :td       (render-md-children children opts)

        ;; --- inline ---
        :span     (escape-md (raw-body node))
        :br       "  \n"      ; GFM hard-break = two trailing spaces + newline
        :strong   (str "**" (render-md-children children opts) "**")
        :em       (str "*"  (render-md-children children opts) "*")
        :c        (str "`"  (raw-body node) "`")
        :a        (str "[" (render-md-children children opts) "](" (or (:href attrs) "") ")")
        :img      (str "![" (or (:alt attrs) "") "](" (or (:src attrs) "") ")")
        :kbd      (str "<kbd>" (escape-md (raw-body node)) "</kbd>")
        :mark     (str "==" (render-md-children children opts) "==")
        :sup      (str "<sup>" (render-md-children children opts) "</sup>")
        :sub      (str "<sub>" (render-md-children children opts) "</sub>")

        (render-md-children children opts)))))

;; =============================================================================
;; Plain walker
;; =============================================================================

;; render-plain <-> render-plain-children mutual; same pattern as html.
(declare ^:private render-plain)

(defn- render-plain-children [nodes opts]
  (apply str (map #(render-plain % opts) nodes)))

(defn- render-plain-list [tag children {:keys [start] :as opts}]
  (let [n (atom (or start 1))
        ordered? (= tag :ol)]
    (apply str
      (map (fn [li]
             (let [marker (if ordered?
                            (let [m (str @n ". ")] (swap! n inc) m)
                            "• ")
                   inner  (str/replace (render-plain-children (node-children li) opts)
                            #"\n+$" "")]
               (str marker inner "\n")))
        children))))

(defn- render-plain-table [node opts]
  (let [rows (node-children node)
        cell-text (fn [cell] (-> cell node-children (render-plain-children opts) str/trim))
        all-rows  (mapv (fn [tr] (mapv cell-text (node-children tr))) rows)
        widths    (when (seq all-rows)
                    (let [cols (apply max 0 (map count all-rows))]
                      (vec (for [i (range cols)]
                             (apply max 1 (map #(count (or (nth % i nil) "")) all-rows))))))
        pad       (fn [s w] (str s (apply str (repeat (max 0 (- w (count s))) " "))))
        fmt-row   (fn [row] (str/join "  " (map-indexed (fn [i c] (pad (or c "") (nth widths i 0))) row)))]
    (str (str/join "\n" (map fmt-row all-rows)) "\n\n")))

(defn- render-plain [node opts]
  (cond
    (string? node) node
    (not (vector? node)) (str node)
    :else
    (let [tag      (node-tag node)
          attrs    (node-attrs node)
          children (node-children node)]
      (case tag
        :ir       (render-plain-children children opts)

        :p        (str (render-plain-children children opts) "\n\n")
        :h        (str (render-plain-children children opts) "\n\n")

        :code     (str (raw-body node) "\n\n")

        :ul       (str (render-plain-list :ul children opts) "\n")
        :ol       (str (render-plain-list :ol children (assoc opts :start (or (:start attrs) 1))) "\n")
        :li       (render-plain-children children opts)

        :details  (let [head    (first children)
                        body    (rest children)
                        h-text  (when head (str/trim (render-plain-children (node-children head) opts)))
                        b-text  (render-plain-children body opts)]
                    (str "▸ " (or h-text "Details") "\n" b-text "\n"))
        :summary  (render-plain-children children opts)

        :quote    (let [body (str/trim (render-plain-children children opts))
                        prefixed (str/join "\n" (map #(str "│ " %) (str/split-lines body)))]
                    (str prefixed "\n\n"))

        :table    (render-plain-table node opts)
        :tr       (render-plain-children children opts)
        :th       (render-plain-children children opts)
        :td       (render-plain-children children opts)

        ;; --- inline ---
        :span     (raw-body node)
        :br       "\n"
        :strong   (render-plain-children children opts)
        :em       (render-plain-children children opts)
        :c        (raw-body node)
        :a        (let [text (render-plain-children children opts)
                        href (or (:href attrs) "")]
                    (if (= text href) text (str text " (" href ")")))
        :img      (str "🖼 " (or (:alt attrs) (:src attrs) "image"))
        :kbd      (raw-body node)
        :mark     (render-plain-children children opts)
        :sup      (render-plain-children children opts)
        :sub      (render-plain-children children opts)

        (render-plain-children children opts)))))

;; =============================================================================
;; Public render entry
;; =============================================================================

(defn render
  "Render any answer input into a flavor.

   Input:  string | Hiccup vector | [:ir ...] AST | sequential of mixed
   Flavor: :html | :markdown | :plain
   Opts:   {:context    #{:answer :thinking :status :error}
            :max-length int  - hard cap; truncate at paragraph boundary}"
  ([input flavor]      (render input flavor nil))
  ([input flavor opts]
   (let [ast    (->ast input)
         walker (case flavor
                  :html     render-html
                  :markdown render-md
                  :plain    render-plain
                  (throw (ex-info (str "Unknown render flavor: " flavor)
                           {:flavor flavor :valid #{:html :markdown :plain}})))
         output  (str/trim (walker ast opts))
         max-len (:max-length opts)]
     (if (and max-len (> (count output) max-len))
       (let [cut (or (str/last-index-of output "\n\n" (long (- max-len 2)))
                   (str/last-index-of output "\n" (long (- max-len 2)))
                   (max 0 (- max-len 2)))]
         (str (subs output 0 cut) "…"))
       output))))

;; =============================================================================
;; Code & text extraction
;; =============================================================================

(defn extract-code
  "Walk the AST and return a vector of strings, one per [:code ...] block,
   in source order. Used by `vis run --code`."
  [input]
  (let [ast (->ast input)
        out (volatile! [])]
    (letfn [(walk [n]
              (cond
                (and (vector? n) (= :code (first n)))
                (vswap! out conj (raw-body n))
                (vector? n)
                (doseq [c (node-children n)] (walk c))
                :else nil))]
      (walk ast)
      @out)))

(defn search-text
  "Universal plain-text projection for full-text search / clipboard /
   logging. Accepts canonical IR, a markdown string, or anything
   `->ast` can coerce; returns a single concatenated string suitable
   for FT5 indexing or substring matching.

   IR-side rendering: all `:p`, `:h`, `:li`, `:quote`, `:summary`,
   `:details` body content collapses to spaces; `:code`/`:c` bodies
   included verbatim (often the highest-signal text for search).
   Strings are lifted via `text->ir` so search hits the same shape
   regardless of upstream contract.

   Idempotent on canonical input via the `text->ir` shortcut."
  ^String [v]
  (when (some? v)
    (let [ir (cond
               (canonical? v) v
               (string? v)    (text->ir v)
               :else          (->ast v))
          out (volatile! [])]
      (letfn [(walk [n]
                (cond
                  (string? n)         (vswap! out conj n)
                  (not (vector? n))   nil
                  (= :br (first n))   (vswap! out conj " ")
                  (= :img (first n))  nil
                  :else               (doseq [c (drop 2 n)] (walk c))))]
        (walk ir))
      ;; collapse any whitespace runs introduced by joining spans /
      ;; block boundaries; FT search wants stable, single-spaced text.
      (-> (str/join " " @out)
        (str/replace #"\s+" " ")
        (str/trim)))))

(defn extract-text
  "Walk the AST and return concatenated plain-text content of all [:p]
   blocks (inline content stripped). Used by voice TTS."
  [input]
  (let [ast (->ast input)
        out (volatile! [])]
    (letfn [(walk [n]
              (cond
                (and (vector? n) (= :p (first n)))
                (vswap! out conj (str/trim (render-plain-children (node-children n) {})))
                (vector? n)
                (doseq [c (node-children n)] (walk c))
                :else nil))]
      (walk ast)
      (str/join "\n\n" (remove str/blank? @out)))))

;; =============================================================================
;; Conversation exporter — DB → Markdown document
;; =============================================================================

(def ^:private DEFAULT_EXPORT_OPTS
  {:include-system? false
   :include-meta?   true
   :flavor          :markdown})

(defn- render-export-header [conversation turn-count]
  (let [title    (or (:title conversation) "Conversation")
        soul-id  (or (:id conversation) (:soul-id conversation))]
    (str "# " title "\n\n"
      (when soul-id (str "_id: `" soul-id "` · " turn-count " turn"
                      (if (= 1 turn-count) "" "s") "_\n\n")))))

(defn- render-export-turn [opts turn]
  (let [user-text (or (:user turn) (:prompt turn) "")
        answer    (:answer turn)
        ast       (->ast answer)
        rendered  (render ast (:flavor opts))]
    (str "## You\n\n" user-text "\n\n## Assistant\n\n" rendered "\n")))

(defn conversation->markdown
  "Project a full conversation as a Markdown document on top of the IR
   pipeline."
  ([db-info conversation-ref]
   (conversation->markdown db-info conversation-ref nil))
  ([db-info conversation-ref opts]
   (when (and db-info conversation-ref)
     (let [opts         (merge DEFAULT_EXPORT_OPTS opts)
           conversation (persistance/db-get-conversation db-info conversation-ref)
           turns        (vec (or (persistance/db-list-conversation-turns db-info conversation-ref) []))]
       (when conversation
         (str (render-export-header conversation (count turns))
           (str/join "\n" (map (partial render-export-turn opts) turns))))))))
