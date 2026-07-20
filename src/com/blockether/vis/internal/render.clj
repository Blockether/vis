(ns com.blockether.vis.internal.render
  "Transient Markdown parsing and renderer projections.

   Canonical answers are role-labelled, string-keyed content blocks from
   `com.blockether.vis.internal.content`. Parsed Markdown trees are created only
   inside renderers and are never transported or persisted.

   `markdown->ast` parses prose for renderer-local layout. `render`,
   `extract-code`, `extract-text`, and `session->markdown` are disposable
   projections; none of their intermediate trees are canonical message data."
  (:require [clojure.string :as str]
            [clojure+.walk :as cwalk]
            [com.blockether.ruff :as ruff]
            [com.blockether.vis.internal.persistance :as persistance])
  (:import [org.commonmark.ext.gfm.strikethrough Strikethrough StrikethroughExtension]
           [org.commonmark.ext.gfm.tables TableBlock TableCell TablesExtension]
           [org.commonmark.node BlockQuote BulletList Code Emphasis FencedCodeBlock HardLineBreak
            Heading HtmlBlock HtmlInline Image IndentedCodeBlock Link Node OrderedList Paragraph
            SoftLineBreak StrongEmphasis Text ThematicBreak]
           [org.commonmark.parser Parser]))

;; =============================================================================
;; Tag taxonomy
;; =============================================================================

(def ^:private block-tags #{:p :h :code :ul :ol :li :quote :table :tr :th :td})

(def ^:private inline-tags #{:span :br :strong :em :c :a :img :kbd :mark :sup :sub})

(def ^:private void-inline-tags "Inline tags whose canonical form has no children." #{:br :img})

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
  "Insert {} attrs at index 1 when absent.

   Also accepts the common LLM heading shorthand `[:h 3 ...]` and
   normalizes it to `[:h {:level 3} ...]` so the level never leaks as
   visible heading text."
  [v]
  (cond (not (vector? v)) v
        (empty? v) v
        (map? (second v)) v
        (and (= :h (first v)) (integer? (second v)))
        (into [:h {:level (max 1 (min 6 (long (second v))))}] (nnext v))
        :else (into [(first v) {}] (rest v))))

(defn- has-attrs? [v] (and (vector? v) (>= (count v) 2) (map? (second v))))

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
           :br
           (= 2 (count x))

           :img
           (= 2 (count x))

           :span
           (and (= 3 (count x)) (string? (nth x 2)) (not (str/includes? (nth x 2) "\n")))

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
       (let
         [tag
          (nth x 0)

          children
          (drop 2 x)]

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

           false))))

(defn canonical?
  "Cheap structural check: `x` is already a canonical `[:ast & blocks]`
   AST. When true, `(->ast x)` is the identity (returns the same
   object), so downstream caches keyed on `System/identityHashCode`
   hit cleanly across repeated render passes."
  [x]
  (and (vector? x)
       (= :ast (first x))
       (>= (count x) 2)
       (map? (nth x 1))
       (every? block-canonical? (drop 2 x))))

;; ─── Canonicalization (canon-*) ─────────────────────────────────────────
;; Mutual recursion across blocks/inlines (canon-block <-> canon-block-rebuild
;; <-> canon-li-children <-> canon-block; canon-inline-children <-> canon-
;; inline-node). `declare` is required because reordering cannot remove this
;; genuine mutual recursion.
(declare ^:private canon-block ^:private canon-block-rebuild ^:private canon-inline-children)

(defn- text-flatten
  "Concatenate every string anywhere in `x` (depth-first). Used to
   collapse a raw-text container's children into a single body string,
   regardless of whether the input is `[:c \"raw\"]` (LLM-flavored)
   or `[:c [:span \"raw\"]]` (re-canonicalization input)."
  ^String [x]
  (cond (string? x) x
        (vector? x) (apply str (map text-flatten (drop 2 x)))
        (sequential? x) (apply str (map text-flatten x))
        (nil? x) ""
        :else ""))

(defn- loose-text-flatten
  "Text-flatten non-canonical or retired tag trees. Unlike `text-flatten`,
   this accepts Hiccup shorthand vectors whose attrs map is absent, so
   retired `[:details [:summary \"x\"] ...]` input stays visible while the
   unsupported structure is removed."
  ^String [x]
  (cond (string? x) x
        (vector? x)
        (let [children (if (and (keyword? (first x)) (map? (second x))) (drop 2 x) (rest x))]
          (apply str (map loose-text-flatten children)))
        (sequential? x) (apply str (map loose-text-flatten x))
        (nil? x) ""
        :else ""))

(defn- string->span
  "Lift a raw text string into a [:span ...] node, collapsing soft breaks
   unless `preserve-ws?` is set. Empty strings become nil (filtered out)."
  [^String s preserve-ws?]
  (let [text (if preserve-ws? s (collapse-soft-breaks s))]
    (when-not (= "" text)
      [:span
       (cond-> {}
         preserve-ws?
         (assoc :preserve-ws? true)) text])))

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
    (let
      [orig
       node

       node
       (ensure-attrs node)

       tag
       (first node)

       attrs
       (second node)

       children
       (drop 2 node)]

      (cond (contains? void-inline-tags tag)
            (if (and (= 2 (count orig)) (map? (nth orig 1 nil))) orig [tag attrs])
            (contains? raw-text-tags tag) [tag attrs (text-flatten children)]
            (= :span tag) (let
                            [pw?
                             (or (:preserve-ws? attrs) preserve-ws?)

                             text
                             (text-flatten children)

                             text
                             (if pw? text (collapse-soft-breaks text))]

                            (if (= "" text) nil [:span attrs text]))
            (contains? inline-tags tag)
            (let [child-nodes (canon-inline-children children preserve-ws?)]
              (into [tag attrs] child-nodes))
            :else
            ;; Unknown/retired tags are not preserved in canonical IR. This is
            ;; deliberate for removed answer affordances such as :details/:summary:
            ;; keep any human-visible text, but never keep unsupported structure.
            (let
              [text
               (loose-text-flatten children)

               text
               (if preserve-ws? text (collapse-soft-breaks text))]

              (when-not (= "" text) [:span {} text]))))))

(defn- map-keep-identity
  "Like `mapv` but returns the input vector unchanged when `f` is
   identity for every element — the building block for sub-tree
   identity preservation across canonicalization passes.

   Pairs with `clojure+.walk/walk` semantics: a transformation that
   does nothing in net should leave the data structure `identical?`
   so downstream `System/identityHashCode` caches stay hot."
  [f xs]
  (let
    [xs
     (vec xs)

     n
     (count xs)

     ys
     (object-array n)

     changed?
     (volatile! false)]

    (dotimes [i n]
      (let
        [x (nth xs i)
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
  (let
    [lifted (vec (keep (fn [c]
                         (cond (string? c) (string->span c preserve-ws?)
                               (vector? c) c
                               (nil? c) nil
                               :else (string->span (str c) preserve-ws?)))
                       children))]
    (map-keep-identity (fn [c]
                         (if (vector? c) (canon-inline-node c preserve-ws?) c))
                       lifted)))

(defn- canon-li-children
  "All-blocks OR all-inlines (wrapped in a single :p). Mixed input is
   bucketed in source order, with consecutive inline runs each
   wrapped in their own :p."
  [children]
  (let
    [classified (mapv (fn [c]
                        (cond (and (vector? c) (block? c)) :block
                              (and (vector? c) (inline? c)) :inline
                              (string? c) :inline
                              :else :inline))
                      children)]
    (cond (every? #(= :block %) classified) (map-keep-identity canon-block children)
          (every? #(= :inline %) classified)
          (let [inline-children (canon-inline-children children false)]
            (if (seq inline-children) [(into [:p {}] inline-children)] []))
          :else (loop
                  [out []
                   buf []
                   cs (seq children)]

                  (let
                    [flush (fn [out buf]
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
  (let
    [children
     (vec children)

     ;; fast path: every child already a canonical block → just
     ;; identity-preserve over canon-block.
     all-blocks?
     (every? #(and (vector? %) (block? %)) children)]

    (if all-blocks?
      (map-keep-identity canon-block children)
      ;; slow path: buffer loose inlines into :p.
      (loop
        [out
         []

         buf
         []

         cs
         (seq children)]

        (let
          [flush (fn [out buf]
                   (let [inl (canon-inline-children buf false)]
                     (if (seq inl) (conj out (into [:p {}] inl)) out)))]
          (if (nil? cs)
            (flush out buf)
            (let [c (first cs)]
              (cond (and (vector? c) (block? c))
                    (recur (conj (flush out buf) (canon-block c)) [] (next cs))
                    :else (recur out (conj buf c) (next cs))))))))))

(defn- canon-block
  "Canonicalize one block vector. Identity-preserving on already-
   canonical input."
  [node]
  (if (block-canonical? node) node (canon-block-rebuild node)))

(defn- canon-block-rebuild
  "Internal: rebuild a block via tag dispatch. Skipped via identity
   when `block-canonical?` is true at the call site."
  [node]
  (let
    [node
     (ensure-attrs node)

     tag
     (first node)

     attrs
     (second node)

     children
     (drop 2 node)]

    (case tag
      :code ; raw source; body = single string, ws preserved verbatim
      [:code attrs (text-flatten children)]

      :li
      (into [:li attrs] (canon-li-children children))

      (:ul :ol)
      (let
        [children
         (vec children)

         children'
         (map-keep-identity
           (fn [c]
             (let [c (ensure-attrs c)]
               (if (and (vector? c) (= :li (first c))) (canon-block c) (canon-block [:li {} c]))))
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

      ;; should not happen — caller dispatches by block?/inline?
      (into [tag attrs] (canon-block-children children)))))


(defn ->ast
  "Soft-normalize any answer-input value into canonical [:ast & blocks].
   Pure, total, idempotent.

   Identity-preserving: when the input already satisfies the canonical
   invariants (`canonical?`), the return value is the SAME object.
   This keeps downstream `System/identityHashCode` caches
   (`format-answer-with-thinking-data`, etc.) hot across repeated
   render passes — walker output is computed once per canonical IR
   identity, not once per equal-but-fresh allocation.

   Before canonicalization, Hiccup child positions are walked with
   `clojure+.walk` semantics and non-vector sequential values (notably
   lazy seqs from `(map ...)` inside renderer input) are safely realized to
   at most 100 items, then replaced with an explicit `… many more`
   marker when truncated. This avoids persisting Java LazySeq identity
   strings and avoids hanging on infinite seqs.

   See namespace docstring for the full canonical-form invariants."
  [v]
  (if (canonical? v)
    v ;; identical preserved — cache-friendliness fast path
    (let
      [max-seq-items
       100

       truncation-marker
       "… many more"]

      (letfn
        [(bounded-seq [xs]
           (let
             [items
              (doall (take (inc max-seq-items) xs))

              more?
              (> (count items) max-seq-items)]

             (cond-> (mapv sanitize (take max-seq-items items))
               more?
               (conj truncation-marker))))
         (expand-child [child]
           (if (and (sequential? child) (not (vector? child)) (not (string? child)))
             (bounded-seq child)
             [(sanitize child)]))
         (sanitize-vector [x]
           (let [x (vec x)]
             (if (and (seq x) (keyword? (first x)))
               (let
                 [tag (first x)
                  attrs? (map? (second x))
                  prefix (if attrs? [tag (sanitize (second x))] [tag])
                  children (if attrs? (nnext x) (next x))]

                 (into prefix (mapcat expand-child children)))
               (cwalk/walk sanitize identity x))))
         (sanitize [x]
           (cond (nil? x) nil
                 (string? x) x
                 (vector? x) (sanitize-vector x)
                 (map? x) (cwalk/walk sanitize identity x)
                 (set? x) (cwalk/walk sanitize identity x)
                 (and (sequential? x) (not (string? x))) (bounded-seq x)
                 :else x))]
        (let [v (sanitize v)]
          (if (canonical? v)
            v
            (let
              [raw-children (cond (and (vector? v) (= :ast (first v))) (let [v (ensure-attrs v)]
                                                                         (drop 2 v))
                                  (string? v) [v]
                                  (and (vector? v) (keyword? (first v))) [v]
                                  (sequential? v) (seq v)
                                  :else [[:code {:lang "edn"} (pr-str v)]])
               coerced (mapv (fn [x]
                               (cond (string? x) x
                                     (and (vector? x) (keyword? (first x))) x
                                     (nil? x) nil
                                     :else [:code {:lang "edn"} (pr-str x)]))
                             raw-children)]

              (into [:ast {}] (canon-blocks-strict (filter some? coerced))))))))))

(defn ast? "True when x is a canonical [:ast ...] AST." [x] (and (vector? x) (= :ast (first x))))

;; =============================================================================
;; markdown->ast — commonmark-java parser → transient renderer tree
;; =============================================================================
;;
;; Used at the boundaries that DON'T have IR upstream:
;;   - LLM-emitted thinking strings (`:thinking` field on iterations);
;;   - user-typed input box messages.
;; Both arrive as plain markdown strings; this function lifts them into
;; canonical IR so the entire downstream pipeline (TUI walker,
;; exporter) sees one shape.

(def ^:private ^Parser md-parser
  (-> (Parser/builder)
      (.extensions [(TablesExtension/create) (StrikethroughExtension/create)])
      (.build)))

(defn- cm-children-seq
  "Iterate `Node.getNext` linked list as a Clojure seq."
  [^Node node]
  (when node
    (loop
      [^Node n
       (.getFirstChild node)

       acc
       (transient [])]

      (if (nil? n) (persistent! acc) (recur (.getNext n) (conj! acc n))))))

;; Mutual: block parser walks block-children which may contain inlines;
;; inline parser walks inline-children which may contain nested inlines.
(declare ^:private cm->blocks ^:private cm->inlines)

(def ^:dynamic *soft-break*
  "How a CommonMark SoftLineBreak (a bare `\n` between two non-blank
   lines) is lifted into IR. CommonMark prose semantics collapse a soft
   break to a single space, which is correct for model-authored answers
   and thinking. User-typed / pasted input is line-oriented (an input
   box, not a prose document): a literal newline is intent, so we lift
   it to `[:br]` to preserve the pasted shape (code, line-numbered
   dumps, tables). Bound to `:hard` via `(markdown->ast text {:soft-break
   :hard})`. Default `:space` keeps every existing caller unchanged."
  :space)

(defn- cm->inline-node
  [^Node n]
  (cond (instance? Text n) [:span {} (.getLiteral ^Text n)]
        (instance? Code n) [:c {} (.getLiteral ^Code n)]
        (instance? StrongEmphasis n) (into [:strong {}] (cm->inlines n))
        (instance? Emphasis n) (into [:em {}] (cm->inlines n))
        (instance? Strikethrough n) (into [:em {}] (cm->inlines n)) ; closest IR analogue
        (instance? Link n) (into [:a {:href (.getDestination ^Link n)}] (cm->inlines n))
        (instance? Image n) [:img {:src (.getDestination ^Image n) :alt (.getTitle ^Image n)}]
        (instance? SoftLineBreak n) (if (= :hard *soft-break*)
                                      [:br {}]                      ; user input → preserve newline
                                      [:span {} " "]) ; prose → single space
        (instance? HardLineBreak n) [:br {}]
        (instance? HtmlInline n) [:span {} (.getLiteral ^HtmlInline n)]
        :else [:span {} ""]))

(defn- cm->inlines [^Node parent] (mapv cm->inline-node (cm-children-seq parent)))

(defn- cm-list-item->li
  [^Node li]
  (let [block-children (cm-children-seq li)]
    (into [:li {}]
          (mapcat (fn [^Node b]
                    (cond (instance? Paragraph b) [(into [:p {}] (cm->inlines b))]
                          :else (cm->blocks b)))
                  block-children))))

(defn- cm->table-cell
  [^Node cell]
  (into (if (and (instance? TableCell cell) (.isHeader ^TableCell cell)) [:th {}] [:td {}])
        (cm->inlines cell)))

(defn- cm->table-row [^Node row] (into [:tr {}] (mapv cm->table-cell (cm-children-seq row))))

(defn- cm->table
  [^Node tbl]
  (into [:table {}]
        (mapcat (fn [^Node section]
                  (mapv cm->table-row (cm-children-seq section)))
                (cm-children-seq tbl))))

(defn- html-comment-block?
  "True when a CommonMark HTML block is only an HTML comment.

  HTML comments are authoring/control markers, not answer text. CommonMark
  exposes a standalone `<!-- -->` separator as `HtmlBlock`; rendering that
  literal made the chat bubble show `<!-- -->`. Drop comment-only blocks while
  keeping other raw HTML visible-as-text (the safety behavior documented below)."
  [^String literal]
  (boolean (re-matches #"(?s)\s*<!--.*?-->\s*" (str literal))))

(def ^:private lone-code-span-block-min
  "A whole-paragraph inline code span (single backticks) at least this many
   characters wide is promoted to a `:code` block instead of staying an inline
   `:c` chip. Models routinely author a long copy-me payload — a bookmarklet, a
   shell command, a URL — as a lone `` `…` `` span; kept inline it wraps across
   rows and a select-copy yanks the wrapped (corrupted) text with no code-block
   copy affordance. Short inline chips (identifiers, paths) stay inline."
  40)

(defn- path-like-code-literal?
  "A file-path-looking code literal (`src/foo/bar.clj`, `~/x/y.edn`): no
   whitespace, at least one `/`, and no URL/bookmarklet scheme prefix. These
   stay INLINE chips however long — rg/patch/index op-cards title every
   per-file section with one, and the chip styling is what makes the path read
   as a header (the TUI paints it on the `result-path` accent, the web as an
   inline `<code>`). Promoting them to `:code` blocks silently dropped that."
  [^String lit]
  (boolean (and (not (re-find #"\s" lit))
                (str/includes? lit "/")
                (not (re-find #"^[A-Za-z][A-Za-z0-9+.-]*:" lit)))))

(defn- lone-code-span-literal
  "When paragraph `n`'s only inline content is a single CommonMark `Code` span
   whose literal is at least `lone-code-span-block-min` chars, return that
   literal (so the caller can promote it to a `:code` block); else nil.
   Path-like literals are exempt — a long file path stays an inline chip (see
   `path-like-code-literal?`)."
  [^Node n]
  (let
    [children
     (cm-children-seq n)

     c
     (first children)]

    (when (and c (instance? Code c) (nil? (next children)))
      (let [lit (.getLiteral ^Code c)]
        (when (and (>= (count lit) (long lone-code-span-block-min))
                   (not (path-like-code-literal? lit)))
          lit)))))

(defn- cm->blocks
  "Convert one commonmark Node into a vector of canonical IR block(s)."
  [^Node n]
  (cond (instance? Heading n) [(into [:h {:level (.getLevel ^Heading n)}] (cm->inlines n))]
        (instance? Paragraph n) (if-let [lit (lone-code-span-literal n)]
                                  [[:code {} lit]]
                                  [(into [:p {}] (cm->inlines n))])
        (instance? FencedCodeBlock n) [[:code
                                        {:lang (let [info (.getInfo ^FencedCodeBlock n)]
                                                 (when (seq info) info))}
                                        (.getLiteral ^FencedCodeBlock n)]]
        (instance? IndentedCodeBlock n) [[:code {} (.getLiteral ^IndentedCodeBlock n)]]
        (instance? BulletList n) [(into [:ul {}] (mapv cm-list-item->li (cm-children-seq n)))]
        (instance? OrderedList n) [(into [:ol {:start (.getMarkerStartNumber ^OrderedList n)}]
                                         (mapv cm-list-item->li (cm-children-seq n)))]
        (instance? BlockQuote n) [(into [:quote {}] (mapcat cm->blocks (cm-children-seq n)))]
        (instance? ThematicBreak n) [[:hr {}]]
        (instance? TableBlock n) [(cm->table n)]
        (instance? HtmlBlock n)
        ;; Raw HTML is not renderer structure. Keep non-comment HTML visible
        ;; as text; notably, <details>/<summary> does not become a collapsible
        ;; widget. HTML comments (`<!-- -->`) are invisible authoring/control
        ;; markers, so they should not paint in the bubble.
        (let [literal (.getLiteral ^HtmlBlock n)]
          (if (html-comment-block? literal) [] [[:p {} [:span {} literal]]]))
        :else (mapcat cm->blocks (cm-children-seq n))))

(defn- table-delimiter-line?
  "True for a GFM table delimiter row, e.g. `|---|---|` / `:--|--:`."
  [line]
  (let [t (str/trim line)]
    (and (re-matches #"[-|: ]+" t) (str/includes? t "-") (str/includes? t "|"))))

(defn- ensure-table-blank-lines
  "GFM only recognizes a pipe-table when a blank line precedes it. Models
   routinely write the table directly under a text line (ripgrep-style
   muscle memory), so commonmark folds it into the paragraph as literal
   pipes and it never renders. Inject the missing blank line so tolerant
   authoring just works — mirrors the scalar-tolerant rg spec coercion."
  [text]
  (let [lines (str/split-lines text)]
    (->> (range (count lines))
         (reduce (fn [out i]
                   (let
                     [line (nth lines i)
                      hdr (peek out)
                      pre (when (> (long i) 1) (nth lines (- (long i) 2)))]

                     (if (and (table-delimiter-line? line)
                              (string? hdr)
                              (str/includes? hdr "|")
                              (string? pre)
                              (not (str/blank? pre)))
                       (conj (pop out) "" hdr line)
                       (conj out line))))
                 [])
         (str/join "\n"))))

(defn diff-line-kind
  "Classify ONE unified-diff line for channel-neutral colouring: `:meta` (file
   headers `---`/`+++`), `:hunk` (`@@`), `:add` (`+`), `:del` (`-`), or `:ctx`
   (context / unchanged). The SINGLE classifier both channels share — the TUI maps
   the kind to an ANSI colour, the web to a CSS class — so a `diff` fence colours
   IDENTICALLY in both, from one source of truth (no per-channel copy to drift)."
  [^String line]
  (let [s (str line)]
    (cond (or (.startsWith s "+++") (.startsWith s "---")) :meta
          (.startsWith s "@@") :hunk
          (and (pos? (count s)) (= \+ (.charAt s 0))) :add
          (and (pos? (count s)) (= \- (.charAt s 0))) :del
          :else :ctx)))

(defn markdown->ast
  "Parse a Markdown string into canonical transient Markdown tree.
   Idempotent: when the input is already canonical IR, returns it
   unchanged (`identical?` preserved — cache-friendly).

   This is the SINGLE entry point for turning Markdown source into IR.
   Used by:
     - the final-answer pipeline (model's plain-prose Markdown answer)
     - thinking text from the model
     - user-typed messages from the TUI input box

   Returns canonical `[:ast & blocks]` directly (no further `->ast`
   round-trip needed). Empty / nil input yields `[:ast {}]`.

   Implementation: commonmark-java parser + GFM tables / strikethrough
   extensions, then a faithful Node→IR walker. Soft line breaks collapse
   to a single space; hard line breaks become `[:br]`.

   `opts` (2-arity) currently understands `{:soft-break :hard}`, which
   lifts every bare newline to `[:br]` instead of a space — used for
   line-oriented user/pasted input so the rendered bubble keeps the
   exact line structure the user typed. Default keeps prose semantics."
  ([text] (markdown->ast text nil))
  ([text {:keys [soft-break]}]
   (cond (canonical? text) text ; identity-preserving fast path
         (or (nil? text) (= "" text)) [:ast {}]
         (string? text) (binding [*soft-break* (or soft-break *soft-break*)]
                          (let
                            [prepared
                             (if (= :hard *soft-break*) text (ensure-table-blank-lines text))
                             doc (.parse md-parser ^String prepared)
                             blocks (vec (mapcat cm->blocks (cm-children-seq doc)))]

                            (->ast (into [:ast {}] blocks))))
         :else
         ;; non-string, non-canonical — best-effort coerce via ->ast
         (->ast text))))

(def reasoning-preview-line-limit
  "Canonical reasoning PREVIEW height shared by every channel. Up to this many
   rows/lines of a thinking trace stay visible; the remainder folds behind a
   `+N more` disclosure (web) / `\u25b8 THINKING +N more` toggle (TUI). One
   source of truth so the TUI bubble and the web card clamp reasoning to the
   SAME height."
  6)

(def reasoning-collapse-min-hidden
  "Minimum number of HIDDEN reasoning rows required before a thinking trace is
   COLLAPSED behind a disclosure at all. If the remainder beyond the preview is
   smaller than this, the whole trace renders inline — a `+1 more` toggle that
   only reveals one extra line is pure friction. One source of truth shared by
   the TUI thinking bubble and the web thinking card."
  3)

(defn normalize-reasoning
  "Canonical normalization for model reasoning / thinking text before it is
   rendered as a trace. Reasoning streams carry whitespace-padded blank rows
   (trailing spaces/tabs the model emits) and paragraph-style double newlines
   that make the compact thinking block look ragged (glm-5.2 especially). Strip
   per-line trailing whitespace, collapse every run of newlines down to ONE line
   break, then give the trace BREATHING ROOM: a line that ENDS A SENTENCE
   (`.`/`!`/`?`/`\u2026`, optionally closed by a quote/paren/bracket) and is
   followed by more text gets a blank line after it, so consecutive sentences
   read as separate paragraphs instead of a wall. Finally trim. Shared by every
   channel so the TUI bubble and the web thinking card normalize identically."
  [text]
  (-> (str text)
      (str/replace #"[ \t\r\f\013]+\r?\n" "\n")
      (str/replace #"(?:\r?\n){2,}" "\n")
      (str/replace #"([.!?\u2026][\"')\]]?)\r?\n(?=\S)" "$1\n\n")
      str/trim))

(defn reasoning->ast
  "Reasoning / thinking text -> canonical transient Markdown tree. The SINGLE shared entry
   point for rendering a model's thinking trace (TUI thinking bubble AND the web
   thinking card), so both channels paint the SAME structure. Reasoning is
   line-oriented (a trace, not flowing prose): normalize via `normalize-reasoning`
   then lift every bare newline to a HARD break (`[:br]`) via `{:soft-break
   :hard}`. Without the hard break a bold heading collapses onto its body line
   (the TUI `**heading** body` bug); with it the heading keeps its own line,
   matching the web ticker's `marked({:breaks true})`."
  [text]
  (markdown->ast (normalize-reasoning text) {:soft-break :hard}))

;; =============================================================================
;; Walker helpers (canonical inputs)
;; =============================================================================

(defn- node-tag [node] (when (vector? node) (first node)))
(defn- node-attrs [node] (if (has-attrs? node) (second node) {}))
(defn- node-children [node] (if (has-attrs? node) (drop 2 node) (rest node)))

(defn- raw-body
  "Body string for `:span`, `:c`, `:code`, `:kbd`. Empty string when
   absent."
  ^String [node]
  (or (some #(when (string? %) %) (node-children node)) ""))

(defn- escape-html
  [^String s]
  (-> s
      (str/replace "&" "&amp;")
      (str/replace "<" "&lt;")
      (str/replace ">" "&gt;")
      (str/replace "\"" "&quot;")))

(defn- escape-html-attr
  [^String s]
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
;; HTML walker
;; =============================================================================

;; render-html <-> render-html-children mutual: html walker dispatches per
;; tag, recursing into children via the helper which itself recurses back
;; into the walker.
(declare ^:private render-html)
;; HTML tables ship as a monospace <pre> grid, so their cells must be
;; PLAIN text — reuse the plain walker (defined below) rather than the
;; html one, whose <b>/<code> tags would be escaped into literal markup.
(declare ^:private render-plain-children)

(defn- render-html-children [nodes opts] (apply str (map #(render-html % opts) nodes)))

(defn- render-html-list
  [tag children {:keys [start] :as opts}]
  ;; Ordered markers are a pure function of the item index (start + i), so no
  ;; mutable counter is needed. A single StringBuilder appends the marker
  ;; (ordinal written straight in via `.append(long)` — no intermediate String),
  ;; the item body, and the per-item "\n" in one pass, avoiding the N throwaway
  ;; `(str marker inner "\n")` concatenations plus the outer `apply str`.
  (let
    [ordered?
     (= tag :ol)

     s0
     (long (or start 1))

     sb
     (StringBuilder.)]

    (loop
      [i
       0

       cs
       (seq children)]

      (if cs
        (let
          [li
           (first cs)

           ;; Loose CommonMark lists wrap each item's content in a <p>, whose
           ;; trailing "\n\n" would stack on the list's own per-item "\n" and
           ;; triple-space the bullets. Strip the item's trailing newlines.
           inner
           (str/replace (render-html-children (node-children li) opts) #"\n+$" "")]

          (if ordered?
            (-> sb
                (.append (+ s0 (long i)))
                (.append ". "))
            (.append sb "• "))
          (.append sb ^String inner)
          (.append sb "\n")
          (recur (inc i) (next cs)))
        (.toString sb)))))

(defn- render-html-table
  [node opts]
  (let
    [rows
     (node-children node)

     ;; Plain text, not html: the grid is wrapped in <pre> and the body
     ;; is escape-html'd, so any <b>/<code> from html cells would surface
     ;; as literal &lt;b&gt; markup (and throw off column widths).
     cell-text
     (fn [cell]
       (-> cell
           node-children
           (render-plain-children opts)
           str/trim))

     all-rows
     (mapv (fn [tr]
             (mapv cell-text (node-children tr)))
           rows)

     widths
     (when (seq all-rows)
       (let [cols (apply max 0 (map count all-rows))]
         (vec (for [i (range cols)]
                (apply max 1 (map #(count (or (nth % i nil) "")) all-rows))))))

     pad
     (fn [s w]
       (str s (apply str (repeat (max 0 (- (long w) (count s))) " "))))

     fmt-row
     (fn [row]
       (str/join "  "
                 (map-indexed (fn [i c]
                                (pad (or c "") (nth widths i 0)))
                              row)))

     sep
     (when widths (str/join "  " (map #(apply str (repeat % "─")) widths)))

     first-row-is-header?
     (and (seq rows)
          (= :th
             (some-> rows
                     first
                     node-children
                     first
                     node-tag)))

     body
     (if first-row-is-header?
       (str (fmt-row (first all-rows)) "\n" sep "\n" (str/join "\n" (map fmt-row (rest all-rows))))
       (str/join "\n" (map fmt-row all-rows)))]

    (str "<pre>" (escape-html body) "</pre>")))

(defn- render-html
  [node opts]
  (cond (string? node) (escape-html node) ; should not occur in canonical tree
        (not (vector? node)) (escape-html (str node))
        :else
        (let
          [tag
           (node-tag node)

           attrs
           (node-attrs node)

           children
           (node-children node)]

          (case tag
            :ast
            (render-html-children children opts)

            :p
            (str (render-html-children children opts) "\n\n")

            :h
            (str "<b>" (render-html-children children opts) "</b>\n\n")

            :code
            (let
              [{:keys [lang]}
               attrs

               src
               (raw-body node)]

              (if (seq lang)
                (str "<pre><code class=\"language-"
                     (escape-html-attr lang)
                     "\">"
                     (escape-html src)
                     "</code></pre>\n\n")
                (str "<pre>" (escape-html src) "</pre>\n\n")))

            :ul
            (str (render-html-list :ul children opts) "\n")

            :ol
            (str (render-html-list :ol children (assoc opts :start (or (:start attrs) 1))) "\n")

            :li
            (render-html-children children opts)

            :quote
            (let [body (render-html-children children opts)]
              (if (= :thinking (:context opts))
                (str "<blockquote expandable>" body "</blockquote>\n\n")
                (str "<blockquote>" body "</blockquote>\n\n")))

            :table
            (str (render-html-table node opts) "\n\n")

            :tr
            (render-html-children children opts)

            :th
            (render-html-children children opts)

            :td
            (render-html-children children opts)

            ;; --- inline ---
            :span
            (escape-html (raw-body node))

            :br
            "\n"

            :strong
            (str "<b>" (render-html-children children opts) "</b>")

            :em
            (str "<i>" (render-html-children children opts) "</i>")

            :c
            (str "<code>" (escape-html (raw-body node)) "</code>")

            :a
            (str "<a href=\""
                 (escape-html-attr (or (:href attrs) ""))
                 "\">"
                 (render-html-children children opts)
                 "</a>")

            :img
            (str "<i>" (escape-html (or (:alt attrs) "image")) "</i>")

            :kbd
            (str "<code>" (escape-html (raw-body node)) "</code>")

            :mark
            (str "<b>" (render-html-children children opts) "</b>")

            :sup
            (render-html-children children opts)

            ; no <sup> tag
            :sub
            (render-html-children children opts)

            ; no <sub> tag
            ;; unknown tag — pass through children
            (render-html-children children opts)))))

;; =============================================================================
;; Markdown walker
;; =============================================================================

;; render-md <-> render-md-children mutual; same pattern as html.
(declare ^:private render-md)

(defn- render-md-children [nodes opts] (apply str (map #(render-md % opts) nodes)))

(defn- render-md-list
  [tag children {:keys [start] :as opts}]
  ;; Single StringBuilder pass; ordinal appended via `.append(long)`, no
  ;; intermediate marker/item strings (see render-html-list).
  (let
    [ordered?
     (= tag :ol)

     s0
     (long (or start 1))

     sb
     (StringBuilder.)]

    (loop
      [i
       0

       cs
       (seq children)]

      (if cs
        (let
          [li
           (first cs)

           inner
           (str/replace (render-md-children (node-children li) opts) #"\n+$" "")]

          (if ordered?
            (-> sb
                (.append (+ s0 (long i)))
                (.append ". "))
            (.append sb "- "))
          (.append sb ^String inner)
          (.append sb "\n")
          (recur (inc i) (next cs)))
        (.toString sb)))))

(defn- render-md-table
  [node opts]
  (let
    [rows
     (node-children node)

     cell-md
     (fn [cell]
       (-> cell
           node-children
           (render-md-children opts)
           (str/replace "|" "\\|")
           str/trim))

     all-rows
     (mapv (fn [tr]
             (mapv cell-md (node-children tr)))
           rows)]

    (if (empty? all-rows)
      ""
      (let
        [first-row-is-header?
         (= :th
            (some-> rows
                    first
                    node-children
                    first
                    node-tag))

         header
         (if first-row-is-header? (first all-rows) (mapv (constantly "") (first all-rows)))

         body
         (if first-row-is-header? (rest all-rows) all-rows)

         cols
         (count header)

         sep
         (str/join " | " (repeat cols "---"))

         row-md
         (fn [row]
           (str "| " (str/join " | " row) " |"))]

        (str (row-md header) "\n| " sep " |\n" (str/join "\n" (map row-md body)) "\n\n")))))

(defn- emph-md
  "Wrap `inner` markdown in an emphasis `marker` (`**`, `*`, `==`), HOISTING any
   leading/trailing whitespace OUTSIDE the delimiters. CommonMark refuses to
   open or close emphasis flush against whitespace, so a label span that ends in
   a space — `WHAT HAPPENED: ` — serialized naively becomes `**WHAT HAPPENED: **`
   and round-trips as LITERAL asterisks. Moving the space out
   (`**WHAT HAPPENED:** `) keeps the bold intact."
  [marker inner]
  (if (str/blank? inner)
    inner
    (str (subs inner 0 (- (count inner) (count (str/triml inner))))
         marker
         (str/trim inner)
         marker
         (subs inner (count (str/trimr inner))))))


(defn- render-md
  [node opts]
  (cond (string? node) (escape-md node)
        (not (vector? node)) (escape-md (str node))
        :else
        (let
          [tag
           (node-tag node)

           attrs
           (node-attrs node)

           children
           (node-children node)]

          (case tag
            :ast
            (render-md-children children opts)

            :p
            (str (render-md-children children opts) "\n\n")

            :h
            (let [level (max 1 (min 6 (long (or (:level attrs) 1))))]
              (str (apply str (repeat level "#")) " " (render-md-children children opts) "\n\n"))

            :code
            (let
              [{:keys [lang]}
               attrs

               src
               (raw-body node)]

              (str "```" (or lang "") "\n" src "\n```\n\n"))

            :ul
            (str (render-md-list :ul children opts) "\n")

            :ol
            (str (render-md-list :ol children (assoc opts :start (or (:start attrs) 1))) "\n")

            :li
            (render-md-children children opts)

            :quote
            (let
              [body
               (str/trim (render-md-children children opts))

               prefixed
               (str/join "\n" (map #(str "> " %) (str/split-lines body)))]

              (str prefixed "\n\n"))

            :table
            (render-md-table node opts)

            :tr
            (render-md-children children opts)

            :th
            (render-md-children children opts)

            :td
            (render-md-children children opts)

            ;; --- inline ---
            :span
            (escape-md (raw-body node))

            :br
            "  \n"

            ; GFM hard-break = two trailing spaces + newline
            :strong
            (emph-md "**" (render-md-children children opts))

            :em
            (emph-md "*" (render-md-children children opts))

            :c
            (str "`" (raw-body node) "`")

            :a
            (str "[" (render-md-children children opts) "](" (or (:href attrs) "") ")")

            :img
            (str "![" (or (:alt attrs) "") "](" (or (:src attrs) "") ")")

            :kbd
            (str "<kbd>" (escape-md (raw-body node)) "</kbd>")

            :mark
            (emph-md "==" (render-md-children children opts))

            :sup
            (str "<sup>" (render-md-children children opts) "</sup>")

            :sub
            (str "<sub>" (render-md-children children opts) "</sub>")

            (render-md-children children opts)))))

;; =============================================================================
;; Plain walker
;; =============================================================================

;; render-plain <-> render-plain-children mutual; same pattern as html.
(declare ^:private render-plain)

(defn- render-plain-children [nodes opts] (apply str (map #(render-plain % opts) nodes)))

(defn- render-plain-list
  [tag children {:keys [start] :as opts}]
  ;; Single StringBuilder pass; ordinal appended via `.append(long)`, no
  ;; intermediate marker/item strings (see render-html-list).
  (let
    [ordered?
     (= tag :ol)

     s0
     (long (or start 1))

     sb
     (StringBuilder.)]

    (loop
      [i
       0

       cs
       (seq children)]

      (if cs
        (let
          [li
           (first cs)

           inner
           (str/replace (render-plain-children (node-children li) opts) #"\n+$" "")]

          (if ordered?
            (-> sb
                (.append (+ s0 (long i)))
                (.append ". "))
            (.append sb "• "))
          (.append sb ^String inner)
          (.append sb "\n")
          (recur (inc i) (next cs)))
        (.toString sb)))))

(defn- render-plain-table
  [node opts]
  (let
    [rows
     (node-children node)

     cell-text
     (fn [cell]
       (-> cell
           node-children
           (render-plain-children opts)
           str/trim))

     all-rows
     (mapv (fn [tr]
             (mapv cell-text (node-children tr)))
           rows)

     widths
     (when (seq all-rows)
       (let [cols (apply max 0 (map count all-rows))]
         (vec (for [i (range cols)]
                (apply max 1 (map #(count (or (nth % i nil) "")) all-rows))))))

     pad
     (fn [s w]
       (str s (apply str (repeat (max 0 (- (long w) (count s))) " "))))

     fmt-row
     (fn [row]
       (str/join "  "
                 (map-indexed (fn [i c]
                                (pad (or c "") (nth widths i 0)))
                              row)))]

    (str (str/join "\n" (map fmt-row all-rows)) "\n\n")))

(defn- render-plain
  [node opts]
  (cond (string? node) node
        (not (vector? node)) (str node)
        :else
        (let
          [tag
           (node-tag node)

           attrs
           (node-attrs node)

           children
           (node-children node)]

          (case tag
            :ast
            (render-plain-children children opts)

            :p
            (str (render-plain-children children opts) "\n\n")

            :h
            (str (render-plain-children children opts) "\n\n")

            :code
            (str (raw-body node) "\n\n")

            :ul
            (str (render-plain-list :ul children opts) "\n")

            :ol
            (str (render-plain-list :ol children (assoc opts :start (or (:start attrs) 1))) "\n")

            :li
            (render-plain-children children opts)

            :quote
            (let
              [body
               (str/trim (render-plain-children children opts))

               prefixed
               (str/join "\n" (map #(str "│ " %) (str/split-lines body)))]

              (str prefixed "\n\n"))

            :table
            (render-plain-table node opts)

            :tr
            (render-plain-children children opts)

            :th
            (render-plain-children children opts)

            :td
            (render-plain-children children opts)

            ;; --- inline ---
            :span
            (raw-body node)

            :br
            "\n"

            :strong
            (render-plain-children children opts)

            :em
            (render-plain-children children opts)

            :c
            (raw-body node)

            :a
            (let
              [text
               (render-plain-children children opts)

               href
               (or (:href attrs) "")]

              (if (= text href) text (str text " (" href ")")))

            :img
            (or (:alt attrs) (:src attrs) "image")

            :kbd
            (raw-body node)

            :mark
            (render-plain-children children opts)

            :sup
            (render-plain-children children opts)

            :sub
            (render-plain-children children opts)

            (render-plain-children children opts)))))

;; =============================================================================
;; Public render entry
;; =============================================================================


(defn render
  "Render any answer input into a flavor.

   Input:  string | Hiccup vector | [:ast ...] AST | sequential of mixed
   Flavor: :html | :markdown | :plain
   Opts:   {:context    #{:answer :thinking :status :error}
            :max-length int  - hard cap; truncate at paragraph boundary}"
  ([input flavor] (render input flavor nil))
  ([input flavor opts]
   (let
     [ast
      (->ast input)

      walker
      (case flavor
        :html
        render-html

        :markdown
        render-md

        :plain
        render-plain

        (throw (ex-info (str "Unknown render flavor: " flavor)
                        {:flavor flavor :valid #{:html :markdown :plain}})))

      output
      (str/trim (walker ast opts))

      max-len
      (:max-length opts)]

     (if (and max-len (> (count output) (long max-len)))
       (let
         [cut (or (str/last-index-of output "\n\n" (long (- (long max-len) 2)))
                  (str/last-index-of output "\n" (long (- (long max-len) 2)))
                  (max 0 (- (long max-len) 2)))]
         (str (subs output 0 cut) "…"))
       output))))

;; =============================================================================
;; Code & text extraction
;; =============================================================================

(defn extract-code
  "Walk the AST and return a vector of strings, one per [:code ...] block,
   in source order. Used by `vis --code`."
  [input]
  (let
    [ast
     (->ast input)

     out
     (volatile! [])]

    (letfn [(walk [n]
              (cond (and (vector? n) (= :code (first n))) (vswap! out conj (raw-body n))
                    (vector? n) (doseq [c (node-children n)]
                                  (walk c))
                    :else nil))]
      (walk ast) @out)))

(defn search-text
  "Universal plain-text projection for full-text search / clipboard /
   logging. Accepts canonical IR, a markdown string, or anything
   `->ast` can coerce; returns a single concatenated string suitable
   for FT5 indexing or substring matching.

   IR-side rendering: all prose/list/quote/table text collapses to spaces;
   `:code`/`:c` bodies are included verbatim (often the highest-signal text
   for search).
   Strings are parsed via `markdown->ast` so search sees the rendered shape
   regardless of upstream contract.

   Idempotent on parsed input via the `markdown->ast` shortcut."
  ^String [v]
  (when (some? v)
    (let
      [ast
       (cond (canonical? v) v
             (string? v) (markdown->ast v)
             :else (->ast v))

       out
       (volatile! [])]

      (letfn [(walk [n]
                (cond (string? n) (vswap! out conj n)
                      (not (vector? n)) nil
                      (= :br (first n)) (vswap! out conj " ")
                      (= :img (first n)) nil
                      :else (doseq [c (drop 2 n)]
                              (walk c))))]
        (walk ast))
      ;; collapse any whitespace runs introduced by joining spans /
      ;; block boundaries; FT search wants stable, single-spaced text.
      (-> (str/join " " @out)
          (str/replace #"\s+" " ")
          (str/trim)))))

(defn extract-text
  "Walk the AST and return concatenated plain-text content of all [:p]
   blocks (inline content stripped). Used by voice TTS."
  [input]
  (let
    [ast
     (->ast input)

     out
     (volatile! [])]

    (letfn [(walk [n]
              (cond (and (vector? n) (= :p (first n)))
                    (vswap! out conj (str/trim (render-plain-children (node-children n) {})))
                    (vector? n) (doseq [c (node-children n)]
                                  (walk c))
                    :else nil))]
      (walk ast) (str/join "\n\n" (remove str/blank? @out)))))

;; =============================================================================
;; Session exporter — DB → Markdown document
;; =============================================================================

(def ^:private DEFAULT_EXPORT_OPTS {:include-system? false :include-meta? true :flavor :markdown})

(defn- render-export-header
  [session turn-count]
  (let
    [title
     (or (:title session) "Session")

     soul-id
     (or (:id session) (:soul-id session))]

    (str "# " title
         "\n\n"
         (when soul-id
           (str "_id: `" soul-id "` · " turn-count " turn" (if (= 1 turn-count) "" "s") "_\n\n")))))

(defn- render-export-turn
  [opts turn]
  (let
    [user-text
     (or (:user-request turn) (:user turn) (:prompt turn) "")

     ;; Persistence stores the model's raw Markdown answer under
     ;; `:answer-markdown`. Source of truth; no fallback paths.
     md
     (or (:answer-markdown turn) "")

     rendered
     (case (:flavor opts)
       :markdown
       md

       (render (markdown->ast md) (:flavor opts)))]

    (str "## You\n" user-text "\n\n## Assistant\n" rendered "\n")))

(defn session->markdown
  "Project a full session as a Markdown document on top of the IR
   pipeline."
  ([db-info session-ref] (session->markdown db-info session-ref nil))
  ([db-info session-ref opts]
   (when (and db-info session-ref)
     (let
       [opts
        (merge DEFAULT_EXPORT_OPTS opts)

        session
        (persistance/db-get-session db-info session-ref)

        turns
        (vec (or (persistance/db-list-session-turns db-info session-ref) []))]

       (when session
         ;; Canonical header: the SAME grouped session-summary card the HTML/
         ;; transcript surfaces render (deferred resolve avoids the
         ;; render->transcript->core->render load cycle). Falls back to the
         ;; minimal title header only when the summary can't be built.
         (let
           [summary
            ((requiring-resolve
               'com.blockether.vis.internal.foundation.transcript/session-summary-md)
              db-info
              session-ref)

            header
            (if (str/blank? summary)
              (render-export-header session (count turns))
              (str summary "\n## Conversation\n\n"))]

           (str header (str/join "\n" (map (partial render-export-turn opts) turns)))))))))

;; ============================================================================
;; Block source rendering.
;;
;; `parse-block-display` is a pure helper that returns the block source as one
;; verbatim `:code` segment. Channels read the segment at display time.
;; ============================================================================

;; ---------------------------------------------------------------------------
;; Channel-render policy: CONTEXT vs CHANNEL split
;;
;; CONTEXT (model surface): the full transcript — the model reads whatever
;; it bound, accessors and all.
;;
;; CHANNEL (TUI): user-facing. Show TOOL CALL previews
;; (CAT / LS / RG / EDIT badges + bodies) and the final answer.
;;
;; Channels render the model's raw `:code` directly and unconditionally (the
;; canonical contract — TUI and web's `block-code` paint identical source).
;; ---------------------------------------------------------------------------

;; Display wrap width for ruff (chat bubbles are narrow; ruff's default is 88).
(def ^:private prettify-line-length 84)

;; BOUNDED LRU cache (access-order, capped) keyed by the raw source. Beautifying
;; identical code on every re-render is wasteful; an UNBOUNDED memo would grow
;; forever (a leak) across a long session — this LinkedHashMap evicts the eldest
;; past the cap, so the cache is cached-but-leak-free. ruff's own cdylib + FFM
;; buffers are already leak-free (clj-ruff frees every returned string).
(def ^:private prettify-cache
  (java.util.Collections/synchronizedMap (proxy [java.util.LinkedHashMap] [256 0.75 true]
                                           (removeEldestEntry [_entry]
                                             (> (.size ^java.util.LinkedHashMap this) 512)))))

(defn prettify-python
  "Beautify Python `src` via ruff (long calls/collections wrapped multiline,
   black-style) for DISPLAY at a CHANNEL boundary. NOT applied to the canonical
   `:code` IR (which stays VERBATIM — the executed/stored source, and the
   model's exact bytes), so channels opt in when painting. Cached (bounded LRU)
   and verbatim-safe: `format-or` returns `src` unchanged when ruff is
   unavailable or the snippet isn't valid Python (partial streams, prose), so it
   never changes meaning and never throws. ruff runs in-process via clj-ruff —
   ONE process-wide cdylib, leak-free (confined arena + `ruff_free_string` per call)."
  ^String [^String src]
  (if (str/blank? (str src))
    (str src)
    (if-let [hit (.get ^java.util.Map prettify-cache src)]
      hit
      (let [out (ruff/format-or src {:line-length prettify-line-length})]
        (.put ^java.util.Map prettify-cache src out)
        out))))

(defn parse-block-display
  "Return the model's authored source as ONE verbatim `:code` segment.

   The engine is full-Python: the source is Python and we keep it VERBATIM here —
   exactly what the model wrote, no splitting, no classification. This is the
   canonical segment (tests + the model's own context depend on it being the raw
   bytes). Channels that want a beautified view call `prettify-python` at paint
   time; the IR stays raw.

   Pure helper. Never throws. Blank / nil input returns `[]`."
  [form-source]
  (let [src (str/trimr (str (or form-source "")))]
    (if (str/blank? src) [] [{:kind :code :source src}])))

(defn block-structurally-silent?
  "True when the block source carries NO `:code` segment — it is purely
   structural engine chrome (a bare title call). The engine stamps such a
   block + its stream chunk with `:vis/silent? true` so channels drop the
   whole entry from display (web folds it on `:silent`, the TUI hides the
   form slot). Any block that DOES carry a `:code` segment flows through and
   its raw source paints unconditionally."
  [form-source]
  (let [src (str (or form-source ""))]
    (and (not (str/blank? src))
         (let [segs (parse-block-display src)]
           (not-any? #(= :code (:kind %)) segs)))))
