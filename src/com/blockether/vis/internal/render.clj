(ns com.blockether.vis.internal.render
  "Vis answer IR — Hiccup-EDN with 21-tag MDAST-equivalent shape.

   See PLAN.md and docs/specs/01-streaming-and-markdown.md.

   Public surface:
     (->ast input)                 ; soft-normalize any input → [:ir & nodes]
     (render input flavor opts)    ; one of :html :markdown :plain
     (extract-code ast)            ; for vis run --code
     (extract-text ast)            ; for voice TTS
     (conversation->markdown db conv-ref opts?)

   Tags:
     ROOT             :ir
     BLOCKS    (11)   :p :h{:level 1-6} :code{:lang} :ul :ol{:start} :li
                      :quote :table :tr :th :td
     INLINES   (9)    :strong :em :c (inline code) :a{:href}
                      :img{:src :alt} :kbd :mark :sup :sub

   Bare strings are text nodes. Attrs map is optional in source;
   normalization inserts {} where missing. :li content rule: all-blocks
   OR all-inlines; loose inlines wrapped in [:p ...]."
  (:require
   [clojure.pprint :as pp]
   [clojure.string :as str]
   [com.blockether.vis.internal.persistance :as persistance]))

(set! *warn-on-reflection* true)

;; =============================================================================
;; Tag taxonomy
;; =============================================================================

(def ^:private block-tags
  #{:ir :p :h :code :ul :ol :li :quote :table :tr :th :td})

(def ^:private inline-tags
  #{:strong :em :c :a :img :kbd :mark :sup :sub})

(def ^:private known-tags
  (into block-tags inline-tags))

(defn- block? [node]
  (and (vector? node) (contains? block-tags (first node))))

(defn- inline? [node]
  (or (string? node)
    (and (vector? node) (contains? inline-tags (first node)))))

;; =============================================================================
;; Normalization — soft-coerce any input → [:ir & nodes]
;; =============================================================================

(declare normalize-node)

(defn- has-attrs? [node]
  (and (vector? node) (>= (count node) 2) (map? (second node))))

(defn- normalize-attrs
  "Insert {} attrs where absent so renderers always see a map at index 1."
  [node]
  (cond
    (string? node) node
    (not (vector? node)) node
    (empty? node) node
    (has-attrs? node) (into [(first node) (second node)]
                        (mapv normalize-node (drop 2 node)))
    :else (into [(first node) {}]
            (mapv normalize-node (rest node)))))

(defn- normalize-li-content
  "Within :li, content must be all-blocks OR all-inlines. If mixed or
   loose inlines, wrap them in [:p ...]."
  [li-node]
  (let [[tag attrs & children] li-node
        children (or children [])]
    (if (empty? children)
      li-node
      (let [classified (mapv (fn [c] (cond (block? c) :block
                                       (inline? c) :inline
                                       :else :inline))
                         children)
            all-blocks?  (every? #(= :block %) classified)
            all-inlines? (every? #(= :inline %) classified)]
        (cond
          all-blocks?  li-node
          all-inlines? li-node
          :else (into [tag attrs]
                  (loop [out [], buf [], cs children]
                    (if (empty? cs)
                      (cond-> out (seq buf) (conj (into [:p {}] buf)))
                      (let [c (first cs)]
                        (if (block? c)
                          (recur (-> out
                                   (cond-> (seq buf) (conj (into [:p {}] buf)))
                                   (conj c))
                            []
                            (rest cs))
                          (recur out (conj buf c) (rest cs))))))))))))

(defn- normalize-node [node]
  (cond
    (string? node) node
    (and (vector? node) (= :li (first node)))
    (-> node normalize-attrs normalize-li-content)
    (vector? node) (normalize-attrs node)
    :else node))

(defn ->ast
  "Soft-normalize any answer-input value into canonical [:ir & nodes].
   Pure, total, never throws on shape.

   Coercion rules:
     [:ir ...]                    -> used as-is (after attr+:li normalization)
     [:tag ...] (Hiccup, non-:ir) -> wrapped in [:ir <node>]
     \"text\"                       -> wrapped in [:ir \"text\"]
     anything else                -> [:ir [:code {:lang \"edn\"} (pr-str x)]]"
  [v]
  (cond
    ;; already canonical
    (and (vector? v) (= :ir (first v)))
    (let [normalized (mapv normalize-node (if (has-attrs? v) (drop 2 v) (rest v)))]
      (into [:ir (if (has-attrs? v) (second v) {})] normalized))
    ;; bare string -> text node child of :ir
    (string? v)
    [:ir {} v]
    ;; single Hiccup vector with non-:ir root -> wrap
    (and (vector? v) (keyword? (first v)))
    [:ir {} (normalize-node v)]
    ;; vector of mixed children
    (vector? v)
    (into [:ir {}] (mapv (fn [x] (if (or (string? x) (and (vector? x) (keyword? (first x))))
                                   (normalize-node x)
                                   [:code {:lang "edn"} (pr-str x)]))
                     v))
    ;; sequence (variadic captured as list)
    (sequential? v)
    (into [:ir {}] (mapv (fn [x] (if (or (string? x) (and (vector? x) (keyword? (first x))))
                                   (normalize-node x)
                                   [:code {:lang "edn"} (pr-str x)]))
                     v))
    ;; anything else -> debug surface
    :else
    [:ir {} [:code {:lang "edn"} (pr-str v)]]))

(defn ir?
  "True when x is a canonical [:ir ...] AST."
  [x]
  (and (vector? x) (= :ir (first x))))

;; =============================================================================
;; Helpers shared across walkers
;; =============================================================================

(defn- node-tag [node]
  (when (vector? node) (first node)))

(defn- node-attrs [node]
  (cond
    (not (vector? node)) {}
    (has-attrs? node) (second node)
    :else {}))

(defn- node-children [node]
  (cond
    (not (vector? node)) []
    (has-attrs? node) (drop 2 node)
    :else (rest node)))

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
   not run inside :code/:c (those preserve verbatim)."
  [^String s]
  (-> s
    (str/replace "\\" "\\\\")
    (str/replace "*" "\\*")
    (str/replace "_" "\\_")
    (str/replace "`" "\\`")
    (str/replace "[" "\\[")
    (str/replace "]" "\\]")))

;; =============================================================================
;; HTML walker — Telegram-flavored (allowlist: b i u s code pre a blockquote)
;; =============================================================================

(declare ^:private render-html)

(defn- render-html-children [nodes opts]
  (apply str (map #(render-html % opts) nodes)))

(defn- render-html-list [tag children {:keys [start] :as opts}]
  (let [n (atom (or start 1))]
    (apply str
      (map (fn [li]
             (let [marker (if (= tag :ul) "• "
                            (let [m (str @n ". ")]
                              (swap! n inc)
                              m))
                   inner  (render-html-children (node-children li) opts)]
               (str marker inner "\n")))
        children))))

(defn- render-html-table [node opts]
  (let [rows (node-children node)
        cell-text (fn [cell]
                    (-> cell node-children (render-html-children opts)))
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
    (string? node) (escape-html node)

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
                        src (apply str children)]
                    (if (seq lang)
                      (str "<pre><code class=\"language-" (escape-html-attr lang) "\">"
                        (escape-html src) "</code></pre>\n\n")
                      (str "<pre>" (escape-html src) "</pre>\n\n")))

        :ul       (str (render-html-list :ul children opts) "\n")
        :ol       (str (render-html-list :ol children (assoc opts :start (or (:start attrs) 1))) "\n")
        :li       (render-html-children children opts)

        :quote    (let [body (render-html-children children opts)]
                    (if (= :thinking (:context opts))
                      (str "<blockquote expandable>" body "</blockquote>\n\n")
                      (str "<blockquote>" body "</blockquote>\n\n")))

        :table    (str (render-html-table node opts) "\n\n")
        :tr       (render-html-children children opts)
        :th       (render-html-children children opts)
        :td       (render-html-children children opts)

        :strong   (str "<b>" (render-html-children children opts) "</b>")
        :em       (str "<i>" (render-html-children children opts) "</i>")
        :c        (str "<code>" (escape-html (apply str children)) "</code>")
        :a        (str "<a href=\"" (escape-html-attr (or (:href attrs) "")) "\">"
                    (render-html-children children opts) "</a>")
        :img      (str "<i>🖼 " (escape-html (or (:alt attrs) "image")) "</i>")
        :kbd      (str "<code>" (escape-html (apply str children)) "</code>")
        :mark     (str "<b>" (render-html-children children opts) "</b>")
        :sup      (render-html-children children opts)   ;; Telegram has no <sup>
        :sub      (render-html-children children opts)   ;; Telegram has no <sub>

        ;; unknown tag -> render children, drop wrapper
        (render-html-children children opts)))))

;; =============================================================================
;; Markdown walker — GFM output for CLI / TUI / export
;; =============================================================================

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
                   ;; trim trailing blank lines from li body so the marker hugs content
                   inner  (str/replace inner #"\n+$" "")]
               (str marker inner "\n")))
        children))))

(defn- render-md-table [node opts]
  (let [rows (node-children node)
        cell-md (fn [cell]
                  (-> cell node-children (render-md-children opts)
                    (str/replace "|" "\\|")
                    str/trim))
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
                        src (apply str children)]
                    (str "```" (or lang "") "\n" src "\n```\n\n"))

        :ul       (str (render-md-list :ul children opts) "\n")
        :ol       (str (render-md-list :ol children (assoc opts :start (or (:start attrs) 1))) "\n")
        :li       (render-md-children children opts)

        :quote    (let [body (str/trim (render-md-children children opts))
                        prefixed (str/join "\n" (map #(str "> " %) (str/split-lines body)))]
                    (str prefixed "\n\n"))

        :table    (render-md-table node opts)
        :tr       (render-md-children children opts)
        :th       (render-md-children children opts)
        :td       (render-md-children children opts)

        :strong   (str "**" (render-md-children children opts) "**")
        :em       (str "*" (render-md-children children opts) "*")
        :c        (str "`" (apply str children) "`")
        :a        (str "[" (render-md-children children opts) "](" (or (:href attrs) "") ")")
        :img      (str "![" (or (:alt attrs) "") "](" (or (:src attrs) "") ")")
        :kbd      (str "<kbd>" (escape-md (apply str children)) "</kbd>")
        :mark     (str "==" (render-md-children children opts) "==")
        :sup      (str "<sup>" (render-md-children children opts) "</sup>")
        :sub      (str "<sub>" (render-md-children children opts) "</sub>")

        ;; unknown tag -> render children, drop wrapper
        (render-md-children children opts)))))

;; =============================================================================
;; Plain walker — strip formatting to readable text
;; =============================================================================

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

        :code     (str (apply str children) "\n\n")

        :ul       (str (render-plain-list :ul children opts) "\n")
        :ol       (str (render-plain-list :ol children (assoc opts :start (or (:start attrs) 1))) "\n")
        :li       (render-plain-children children opts)

        :quote    (let [body (str/trim (render-plain-children children opts))
                        prefixed (str/join "\n" (map #(str "│ " %) (str/split-lines body)))]
                    (str prefixed "\n\n"))

        :table    (render-plain-table node opts)
        :tr       (render-plain-children children opts)
        :th       (render-plain-children children opts)
        :td       (render-plain-children children opts)

        :strong   (render-plain-children children opts)
        :em       (render-plain-children children opts)
        :c        (apply str children)
        :a        (let [text (render-plain-children children opts)
                        href (or (:href attrs) "")]
                    (if (= text href) text (str text " (" href ")")))
        :img      (str "🖼 " (or (:alt attrs) (:src attrs) "image"))
        :kbd      (apply str children)
        :mark     (render-plain-children children opts)
        :sup      (render-plain-children children opts)
        :sub      (render-plain-children children opts)

        (render-plain-children children opts)))))

;; =============================================================================
;; Public render entry
;; =============================================================================

(defn render
  "Render answer input to a flavor.

   Input:  string | Hiccup vector | [:ir ...] AST | sequential of mixed
   Flavor: :html | :markdown | :plain
   Opts:   {:partial?   bool   - reserved (no-op v1; renderer never crashes
                                  on partial input by construction)
            :context    #{:answer :thinking :status :error}
            :max-length int    - hard cap; truncate at paragraph boundary}"
  ([input flavor]
   (render input flavor nil))
  ([input flavor opts]
   (let [ast (->ast input)
         walker (case flavor
                  :html     render-html
                  :markdown render-md
                  :plain    render-plain
                  (throw (ex-info (str "Unknown render flavor: " flavor)
                           {:flavor flavor :valid #{:html :markdown :plain}})))
         output (str/trim (walker ast opts))
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
                (vswap! out conj (apply str (node-children n)))
                (vector? n)
                (doseq [c (node-children n)] (walk c))
                :else nil))]
      (walk ast)
      @out)))

(defn extract-text
  "Walk the AST and return concatenated plain-text content of all
   [:p] blocks (inline content stripped to text). Used by voice TTS."
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
;;
;; Rebuilt minimal version replacing the prior 600-LOC helper soup. Walks
;; turns, renders each turn's answer-AST via the new pipeline, prepends user
;; prompt, concatenates.
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
  "Project a full conversation as a Markdown document. Greenfield rebuild
   atop the new IR pipeline."
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
