(ns com.blockether.vis.ext.bridge.languages.markdown
  "CommonMark-backed Markdown extractor for Bridge.

   Produces a PageIndex-style document tree as Bridge nodes/edges:
   file -> heading/section -> nested sections -> code blocks / links / mentions."
  (:require
   [clojure.string :as str]
   [com.blockether.vis.ext.bridge.schema :as schema])
  (:import
   (org.commonmark.ext.gfm.strikethrough StrikethroughExtension)
   (org.commonmark.ext.gfm.tables TablesExtension)
   (org.commonmark.node Code FencedCodeBlock HardLineBreak Heading HtmlInline
     Image Link Node SoftLineBreak Text)
   (org.commonmark.parser IncludeSourceSpans Parser)))

(def ^:private parser
  (-> (Parser/builder)
    (.extensions [(TablesExtension/create)
                  (StrikethroughExtension/create)])
    (.includeSourceSpans IncludeSourceSpans/BLOCKS_AND_INLINES)
    (.build)))

(def markdown-extensions #{"md" "markdown" "mdx"})

(defn supports-path?
  "True when `path` looks like a Markdown file Bridge can parse."
  [path]
  (contains? markdown-extensions
    (str/lower-case (or (second (re-find #"\.([^.]+)$" (str path))) ""))))

(defn- children-seq
  [^Node node]
  (loop [n (.getFirstChild node) acc []]
    (if (nil? n)
      acc
      (recur (.getNext n) (conj acc n)))))

(defn- first-span [^Node node]
  (first (.getSourceSpans node)))

(defn- start-line [^Node node]
  (if-let [span (first-span node)]
    (inc (.getLineIndex span))
    1))

(defn- end-line [^Node node]
  (if-let [span (last (.getSourceSpans node))]
    (inc (.getLineIndex span))
    (start-line node)))

(defn- visible-text
  [^Node node]
  (letfn [(walk [^Node n]
            (cond
              (instance? Text n)          (.getLiteral ^Text n)
              (instance? Code n)          (.getLiteral ^Code n)
              (instance? SoftLineBreak n) " "
              (instance? HardLineBreak n) "\n"
              (instance? HtmlInline n)    (.getLiteral ^HtmlInline n)
              (instance? Image n)         (or (.getTitle ^Image n) "")
              :else                       (apply str (map walk (children-seq n)))))]
    (str/trim (walk node))))

(defn- slug
  [s]
  (let [base (-> (str/lower-case (str s))
               (str/replace #"[^a-z0-9]+" "-")
               (str/replace #"(^-+|-+$)" ""))]
    (if (str/blank? base) "section" base)))

(defn- unique-slug
  [seen title]
  (let [base (slug title)
        n    (get seen base 0)]
    [(update seen base (fnil inc 0))
     (if (zero? n) base (str base "-" (inc n)))]))

(defn- heading-blocks
  [doc]
  (->> (children-seq doc)
    (filter #(instance? Heading %))
    vec))

(defn- section-end-lines
  [content headings]
  (let [total-lines (max 1 (count (str/split-lines (or content ""))))]
    (mapv (fn [idx ^Heading h]
            (let [level (.getLevel h)
                  next-peer (some (fn [^Heading h2]
                                    (when (<= (.getLevel h2) level)
                                      (start-line h2)))
                              (subvec headings (inc idx)))]
              (dec (or next-peer (inc total-lines)))))
      (range (count headings))
      headings)))

(defn- nearest-section
  [sections line]
  (->> sections
    (filter #(and (<= (:line-start %) line)
               (<= line (:line-end %))))
    (sort-by (juxt :line-start :line-end))
    last))

(defn- link-edges
  [path sections doc]
  (let [file-qname (str "doc:" path)]
    (letfn [(walk [^Node n acc]
              (let [acc' (if (instance? Link n)
                           (let [line (start-line n)
                                 section (nearest-section sections line)
                                 source (:qualified-name section file-qname)]
                             (conj acc
                               (schema/edge
                                 {:edge-kind :links-to
                                  :source source
                                  :target (.getDestination ^Link n)
                                  :path path
                                  :language "markdown"
                                  :line line
                                  :resolved? false
                                  :metadata {:text (visible-text n)}})))
                           acc)]
                (reduce (fn [a c] (walk c a)) acc' (children-seq n))))]
      (walk doc []))))

(defn- mention-edges
  [path sections doc]
  (let [file-qname (str "doc:" path)]
    (letfn [(walk [^Node n acc]
              (let [acc' (if (instance? Code n)
                           (let [line (start-line n)
                                 section (nearest-section sections line)
                                 source (:qualified-name section file-qname)]
                             (conj acc
                               (schema/edge
                                 {:edge-kind :mentions
                                  :source source
                                  :target (.getLiteral ^Code n)
                                  :path path
                                  :language "markdown"
                                  :line line
                                  :resolved? false
                                  :metadata {:syntax :inline-code}})))
                           acc)]
                (reduce (fn [a c] (walk c a)) acc' (children-seq n))))]
      (walk doc []))))

(defn- code-block-nodes-and-edges
  [path sections doc]
  (let [file-qname (str "doc:" path)]
    (loop [blocks (children-seq doc)
           idx 0
           nodes []
           edges []]
      (if-let [^Node n (first blocks)]
        (if (instance? FencedCodeBlock n)
          (let [line (start-line n)
                section (nearest-section sections line)
                parent (:qualified-name section file-qname)
                qname (str parent "/code:" idx)
                lang (some-> (.getInfo ^FencedCodeBlock n) str/trim not-empty)]
            (recur (rest blocks) (inc idx)
              (conj nodes
                (schema/node
                  {:kind :code-block
                   :language "markdown"
                   :name (str "code" idx)
                   :qualified-name qname
                   :path path
                   :line-start line
                   :line-end (end-line n)
                   :metadata {:fence-language lang
                              :text (.getLiteral ^FencedCodeBlock n)}}))
              (conj edges
                (schema/edge
                  {:edge-kind :contains
                   :source parent
                   :target qname
                   :path path
                   :language "markdown"
                   :line line
                   :resolved? true}))))
          (recur (rest blocks) idx nodes edges))
        {:nodes nodes :edges edges}))))

(defn extract-file
  "Extract normalized Bridge facts from Markdown `content` at `path`."
  [path content]
  (let [path (str path)
        content (or content "")
        doc (.parse parser content)
        headings (heading-blocks doc)
        end-lines (section-end-lines content headings)
        file-qname (str "doc:" path)
        file-node (schema/node
                    {:kind :file
                     :language "markdown"
                     :name path
                     :qualified-name file-qname
                     :path path
                     :line-start 1
                     :line-end (max 1 (count (str/split-lines content)))})
        sections+stack
        (reduce (fn [{:keys [seen stack sections edges]} [idx ^Heading h end-line]]
                  (let [title (visible-text h)
                        [seen' slug] (unique-slug seen title)
                        qname (str file-qname "#" slug)
                        level (.getLevel h)
                        stack' (vec (take-while #(< (:level %) level) stack))
                        parent (or (:qualified-name (peek stack')) file-qname)
                        section (schema/node
                                  {:kind :section
                                   :language "markdown"
                                   :name title
                                   :qualified-name qname
                                   :path path
                                   :line-start (start-line h)
                                   :line-end end-line
                                   :metadata {:level level
                                              :slug slug
                                              :index idx}})
                        contains-edge (schema/edge
                                        {:edge-kind :contains
                                         :source parent
                                         :target qname
                                         :path path
                                         :language "markdown"
                                         :line (start-line h)
                                         :resolved? true})]
                    {:seen seen'
                     :stack (conj stack' {:level level :qualified-name qname})
                     :sections (conj sections section)
                     :edges (conj edges contains-edge)}))
          {:seen {} :stack [] :sections [] :edges []}
          (map vector (range) headings end-lines))
        sections (:sections sections+stack)
        code-facts (code-block-nodes-and-edges path sections doc)
        nodes (into [file-node] (concat sections (:nodes code-facts)))
        edges (vec (concat (:edges sections+stack)
                     (:edges code-facts)
                     (link-edges path sections doc)
                     (mention-edges path sections doc)))]
    (schema/extract-result
      {:nodes nodes
       :edges edges
       :diagnostics []
       :stats {:language "markdown"
               :path path
               :node-count (count nodes)
               :edge-count (count edges)
               :heading-count (count sections)}})))
