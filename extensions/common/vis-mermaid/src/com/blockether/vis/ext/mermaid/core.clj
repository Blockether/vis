(ns com.blockether.vis.ext.mermaid.core
  "Pure-JVM Mermaid fenced-code renderer.

   No Mermaid.js, Node, browser engine, Graal JS, or external renderer. This
   namespace owns a small in-process Mermaid parser, AST, layout pass, and
   terminal renderer. The implementation is intentionally bounded for TUI use:
   it clips source/output and falls back to textual diagnostics for syntax not
   implemented yet."
  (:require
   [clojure.string :as str]
   [com.blockether.vis.core :as vis]))

(def ^:private max-source-chars 20000)
(def ^:private max-source-lines 400)
(def ^:private max-output-lines 100)
(def ^:private max-render-width 320)
(def ^:private max-flow-nodes 80)
(def ^:private max-flow-edges 140)
(def ^:private max-label-width 28)

(defn- repeat-str
  [ch n]
  (apply str (repeat (max 0 (long n)) ch)))

(defn- rstrip
  [s]
  (str/replace (str s) #"\s+$" ""))

(defn- clip-line
  [width line]
  (let [width (max 1 (long (or width 80)))
        s     (str line)]
    (cond
      (<= (count s) width) s
      (<= width 3)         (subs "..." 0 width)
      :else                (str (subs s 0 (- width 3)) "..."))))

(defn- clip-lines
  [width lines]
  (->> lines
    (take max-output-lines)
    (mapv #(clip-line width %))))

(defn- clean-label
  [s]
  (let [s (-> (str s)
            (str/replace #"(?i)<br\s*/?>" "\n")
            (str/replace #"&nbsp;" " ")
            (str/replace #"&amp;" "&")
            (str/replace #"&lt;" "<")
            (str/replace #"&gt;" ">")
            str/trim
            (str/replace #"^\"|\"$" "")
            (str/replace #"^'|'$" ""))]
    (if (str/blank? s) "?" s)))

(defn- wrap-words
  [width text]
  (let [words (str/split (str/trim (str text)) #"\s+")]
    (loop [words words line "" out []]
      (if-let [word (first words)]
        (let [candidate (if (str/blank? line) word (str line " " word))]
          (cond
            (<= (count candidate) width)
            (recur (rest words) candidate out)

            (str/blank? line)
            (recur (rest words) "" (conj out (clip-line width word)))

            :else
            (recur words "" (conj out line))))
        (cond-> out
          (not (str/blank? line)) (conj line))))))

(defn- label-lines
  [s]
  (let [lines (->> (str/split-lines (clean-label s))
                (mapcat #(wrap-words max-label-width %))
                (remove str/blank?)
                vec)]
    (if (seq lines) lines ["?"])))

(defn- single-line-label
  [s]
  (str/join " / " (label-lines s)))

(defn- bounded-label
  [s]
  (clip-line max-label-width (single-line-label s)))

(defn- strip-comment
  [line]
  (let [idx (str/index-of line "%%")]
    (if idx (subs line 0 idx) line)))

(defn- split-top-level-semis
  [line]
  (let [n (count line)]
    (loop [i 0 depth 0 quote nil start 0 out []]
      (if (>= i n)
        (cond-> out
          (< start n) (conj (subs line start n)))
        (let [ch (.charAt line i)]
          (cond
            quote
            (recur (inc i) depth (when-not (= ch quote) quote) start out)

            (or (= ch \") (= ch \'))
            (recur (inc i) depth ch start out)

            (#{\[ \( \{} ch)
            (recur (inc i) (inc depth) nil start out)

            (#{\] \) \}} ch)
            (recur (inc i) (max 0 (dec depth)) nil start out)

            (and (= ch \;) (zero? depth))
            (recur (inc i) depth nil (inc i) (conj out (subs line start i)))

            :else
            (recur (inc i) depth nil start out)))))))

(defn- content-lines
  [source]
  (->> (str/split-lines (or source ""))
    (map strip-comment)
    (mapcat split-top-level-semis)
    (map str/trim)
    (remove str/blank?)
    vec))

(defn- diagram-head
  [source]
  (first (content-lines source)))

(defn- diagram-kind
  [source]
  (some-> (diagram-head source)
    (str/split #"\s+")
    first
    str/lower-case))

(defn- diagram-direction
  [source]
  (let [[_ dir] (some-> (diagram-head source) (str/split #"\s+"))]
    (-> (or dir "TD")
      (str/replace #";+$" "")
      str/upper-case)))

(defn- shape-body
  [s open close]
  (when (and (str/starts-with? s open) (str/ends-with? s close))
    (subs s (count open) (- (count s) (count close)))))

(defn- parse-node-token
  [token]
  (let [token (str/trim (or token ""))
        [_ id tail] (or (re-matches #"^([A-Za-z0-9_.$:-]+)\s*(.*)$" token)
                      [nil token ""])
        tail (str/trim tail)
        [shape label] (cond
                        (shape-body tail "[[" "]]") [:subroutine (shape-body tail "[[" "]]")]
                        (shape-body tail "[(" ")]") [:cylinder (shape-body tail "[(" ")]")]
                        (shape-body tail "([" "])") [:stadium (shape-body tail "([" "])")]
                        (shape-body tail "((" "))") [:circle (shape-body tail "((" "))")]
                        (shape-body tail "{{" "}}") [:hexagon (shape-body tail "{{" "}}")]
                        (shape-body tail "{" "}")   [:diamond (shape-body tail "{" "}")]
                        (shape-body tail "(" ")")   [:round (shape-body tail "(" ")")]
                        (shape-body tail "[" "]")   [:rect (shape-body tail "[" "]")]
                        :else                         [:rect nil])]
    {:id (str id)
     :label (clean-label (or label id))
     :shape shape
     :explicit-label? (boolean label)}))

(defn- top-level-edge-at
  [s i]
  (let [tail (subs s i)]
    (cond
      (str/starts-with? tail "-->|")
      (when-let [end (str/index-of tail "|" 4)]
        {:from i :to (+ i end 1) :directed? true :style :normal
         :label (subs tail 4 end)})

      (str/starts-with? tail "---|")
      (when-let [end (str/index-of tail "|" 4)]
        {:from i :to (+ i end 1) :directed? false :style :normal
         :label (subs tail 4 end)})

      (str/starts-with? tail "<-->")
      {:from i :to (+ i 4) :directed? true :bidir? true :style :normal}

      (str/starts-with? tail "-.->")
      {:from i :to (+ i 4) :directed? true :style :dotted}

      (str/starts-with? tail "==>")
      {:from i :to (+ i 3) :directed? true :style :thick}

      (str/starts-with? tail "-->")
      {:from i :to (+ i 3) :directed? true :style :normal}

      (str/starts-with? tail "---")
      {:from i :to (+ i 3) :directed? false :style :normal}

      (str/starts-with? tail "--")
      (when-let [end (str/index-of tail "-->" 2)]
        {:from i :to (+ i end 3) :directed? true :style :normal
         :label (subs tail 2 end)}))))

(defn- find-top-level-edge
  [s start]
  (let [n (count s)]
    (loop [i start depth 0 quote nil]
      (when (< i n)
        (let [ch (.charAt s i)]
          (cond
            quote
            (recur (inc i) depth (when-not (= ch quote) quote))

            (or (= ch \") (= ch \'))
            (recur (inc i) depth ch)

            (#{\[ \( \{} ch)
            (recur (inc i) (inc depth) nil)

            (#{\] \) \}} ch)
            (recur (inc i) (max 0 (dec depth)) nil)

            (zero? depth)
            (or (top-level-edge-at s i)
              (recur (inc i) depth nil))

            :else
            (recur (inc i) depth nil)))))))

(defn- parse-flow-statement
  [line]
  (loop [start 0 nodes [] edges []]
    (if-let [edge (find-top-level-edge line start)]
      (let [node-fragment (subs line start (:from edge))]
        (recur (:to edge)
          (conj nodes (parse-node-token node-fragment))
          (conj edges (select-keys edge [:directed? :bidir? :style :label]))))
      (let [tail (subs line start)
            nodes (cond-> nodes
                    (not (str/blank? tail)) (conj (parse-node-token tail)))]
        (if (seq edges)
          {:kind :edges
           :nodes nodes
           :edges (mapv (fn [idx edge]
                          (assoc edge
                            :id (str "e" idx "-" (:id (nth nodes idx)) "-" (:id (nth nodes (inc idx))))
                            :source-id (:id (nth nodes idx))
                            :target-id (:id (nth nodes (inc idx)))
                            :label (some-> (:label edge) clean-label)))
                    (range) edges)}
          {:kind :node :nodes nodes :edges []})))))

(defn- flow-control-line?
  [line]
  (or (str/starts-with? line "graph ")
    (str/starts-with? line "flowchart ")
    (str/starts-with? line "classDef ")
    (str/starts-with? line "class ")
    (str/starts-with? line "style ")
    (str/starts-with? line "linkStyle ")
    (str/starts-with? line "click ")
    (str/starts-with? line "accTitle")
    (str/starts-with? line "accDescr")
    (str/starts-with? line "subgraph")
    (= line "end")))

(defn- put-node
  [nodes node]
  (update nodes (:id node)
    (fn [old]
      (cond
        (nil? old) node
        (:explicit-label? node) (merge old node)
        (:explicit-label? old) old
        :else (merge old node)))))

(defn- parse-flowchart
  [source]
  (let [dir (diagram-direction source)]
    (reduce
      (fn [ast line]
        (if (flow-control-line? line)
          (update ast :warnings conj (str "Skipped directive: " line))
          (let [{:keys [nodes edges]} (parse-flow-statement line)]
            (-> ast
              (update :nodes #(reduce put-node % nodes))
              (update :order into (remove (set (:order ast)) (map :id nodes)))
              (update :edges into edges)))))
      {:diagram/type :flowchart
       :direction dir
       :nodes {}
       :order []
       :edges []
       :warnings []}
      (rest (content-lines source)))))

(defn- participant-line
  [line]
  (when-let [[_ kind id label] (re-matches #"^(participant|actor)\s+(\S+)(?:\s+as\s+(.+))?$" line)]
    {:id id :label (clean-label (or label id)) :kind (keyword kind)}))

(defn- sequence-message
  [line]
  (when-let [[_ from arrow to text]
             (re-matches #"^\s*(\S+?)\s*(-+>>|-+>|-+x|-+\)|-->>|->>|-->|->)\s*(\S+)\s*:\s*(.*)$" line)]
    {:kind :message
     :from from
     :to to
     :arrow arrow
     :dashed? (str/starts-with? arrow "--")
     :async? (str/ends-with? arrow ")")
     :lost? (str/ends-with? arrow "x")
     :text (clean-label text)}))

(defn- sequence-note
  [line]
  (when-let [[_ pos whom text] (re-matches #"^Note\s+(right of|left of|over)\s+([^:]+)\s*:\s*(.*)$" line)]
    {:kind :note :pos pos :of (mapv str/trim (str/split whom #",")) :text (clean-label text)}))

(defn- parse-sequence
  [source]
  (let [statements (rest (content-lines source))]
    (reduce
      (fn [ast line]
        (cond
          (participant-line line)
          (let [p (participant-line line)]
            (-> ast
              (assoc-in [:participants (:id p)] p)
              (update :order #(if (some #{(:id p)} %) % (conj % (:id p))))))

          (sequence-message line)
          (let [{:keys [from to] :as msg} (sequence-message line)]
            (-> ast
              (update :events conj msg)
              (update :order #(reduce (fn [o id] (if (some #{id} o) o (conj o id))) % [from to]))
              (update :participants #(merge {from {:id from :label from :kind :participant}
                                             to   {:id to :label to :kind :participant}}
                                       %))))

          (sequence-note line)
          (update ast :events conj (sequence-note line))

          (or (str/starts-with? line "activate ")
            (str/starts-with? line "deactivate ")
            (str/starts-with? line "autonumber"))
          (update ast :warnings conj (str "Skipped directive: " line))

          :else
          (update ast :warnings conj (str "Unsupported sequence statement: " line))))
      {:diagram/type :sequence
       :participants {}
       :order []
       :events []
       :warnings []}
      statements)))

(def ^:private diagram-kind->type
  {"architecture" :architecture
   "architecture-beta" :architecture
   "block" :block
   "block-beta" :block
   "c4component" :c4
   "c4container" :c4
   "c4context" :c4
   "c4deployment" :c4
   "classdiagram" :class
   "classdiagram-v2" :class
   "erd" :er
   "erdiagram" :er
   "gantt" :gantt
   "gitgraph" :gitgraph
   "graph" :flowchart
   "flowchart" :flowchart
   "ishikawa" :ishikawa
   "journey" :journey
   "kanban" :kanban
   "kanban-beta" :kanban
   "mindmap" :mindmap
   "packet" :packet
   "packet-beta" :packet
   "pie" :pie
   "quadrantchart" :quadrant
   "radar" :radar
   "radar-beta" :radar
   "requirementdiagram" :requirement
   "sankey" :sankey
   "sankey-beta" :sankey
   "sequencediagram" :sequence
   "statediagram" :state
   "statediagram-v2" :state
   "timeline" :timeline
   "treeview" :tree
   "treemap" :treemap
   "treemap-beta" :treemap
   "userjourney" :journey
   "xychart" :xychart
   "xychart-beta" :xychart})

(defn- source-body-lines
  [source]
  (vec (rest (content-lines source))))

(defn- raw-body-lines
  [source]
  (->> (str/split-lines (or source ""))
    rest
    (map strip-comment)
    (remove str/blank?)
    vec))

(defn- title-line?
  [line]
  (str/starts-with? (str/lower-case (str/trim line)) "title"))

(defn- line-title
  [line]
  (some-> line
    (str/replace #"(?i)^title\s*:??\s*" "")
    clean-label))

(defn- parse-title
  [lines]
  (some (fn [line]
          (when (title-line? line)
            (line-title line)))
    lines))

(defn- drop-title-lines
  [lines]
  (remove title-line? lines))

(defn- structured-diagram
  [type source & {:keys [items title warnings]}]
  (let [lines (source-body-lines source)]
    {:diagram/type type
     :title (or title (parse-title lines))
     :items (vec (or items (drop-title-lines lines)))
     :warnings (vec warnings)}))

(defn- parse-class-diagram
  [source]
  (let [lines (source-body-lines source)
        classes (vec (keep (fn [line]
                             (or (some-> (re-matches #"^class\s+([A-Za-z0-9_.$:-]+).*" line) second)
                               (some-> (re-matches #"^([A-Za-z0-9_.$:-]+)\s*[:{].*" line) second)))
                       lines))
        relations (vec (filter #(re-find #"(<\|--|--\|>|\*--|o--|-->|--|\.\.)" %) lines))]
    (assoc (structured-diagram :class source)
      :classes classes
      :relations relations)))

(defn- parse-state-diagram
  [source]
  (let [lines (source-body-lines source)
        transitions (vec (keep (fn [line]
                                 (when-let [[_ a b label] (re-matches #"^(.+?)\s*-->\s*(.+?)(?:\s*:\s*(.*))?$" line)]
                                   {:from (clean-label a) :to (clean-label b) :label (some-> label clean-label)}))
                           lines))]
    (assoc (structured-diagram :state source)
      :transitions transitions)))

(defn- parse-er-diagram
  [source]
  (let [lines (source-body-lines source)
        relationships (vec (keep (fn [line]
                                   (when-let [[_ a rel b label] (re-matches #"^([A-Za-z0-9_.$:-]+)\s+([|}{o\-]+--[|}{o\-]+)\s+([A-Za-z0-9_.$:-]+)\s*:?\s*(.*)$" line)]
                                     {:from a :rel rel :to b :label (clean-label label)}))
                             lines))]
    (assoc (structured-diagram :er source)
      :relationships relationships)))

(defn- parse-number
  [s]
  (try
    (Double/parseDouble (str/trim (str s)))
    (catch Throwable _ nil)))

(defn- parse-pie-diagram
  [source]
  (let [lines (source-body-lines source)
        slices (vec (keep (fn [line]
                            (when-let [[_ label value] (re-matches #"^\s*[\"']?([^\"':]+)[\"']?\s*:\s*([0-9.]+)\s*$" line)]
                              {:label (clean-label label) :value (or (parse-number value) 0)}))
                      lines))]
    (assoc (structured-diagram :pie source)
      :slices slices)))

(defn- parse-sankey-diagram
  [source]
  (let [rows (vec (keep (fn [line]
                          (let [[a b v] (map str/trim (str/split line #","))]
                            (when (and a b v)
                              {:from (clean-label a) :to (clean-label b) :value (clean-label v)})))
                    (source-body-lines source)))]
    (assoc (structured-diagram :sankey source)
      :links rows)))

(defn- leading-space-count
  [s]
  (count (take-while #(Character/isWhitespace ^char %) (str s))))

(defn- parse-indent-diagram
  [type source]
  (let [lines (raw-body-lines source)
        title (parse-title (map str/trim lines))
        items (->> lines
                (remove #(title-line? (str/trim %)))
                (mapv (fn [line]
                        {:indent (leading-space-count line)
                         :text (clean-label (str/trim line))})))]
    {:diagram/type type
     :title title
     :items items
     :warnings []}))

(defn- parse-quadrant-diagram
  [source]
  (let [lines (source-body-lines source)
        points (vec (keep (fn [line]
                            (when-let [[_ label x y] (re-matches #"^(.+?)\s*:\s*\[\s*([0-9.]+)\s*,\s*([0-9.]+)\s*\]\s*$" line)]
                              {:label (clean-label label) :x (parse-number x) :y (parse-number y)}))
                      lines))]
    (assoc (structured-diagram :quadrant source)
      :points points)))

(defn- parse-c4-diagram
  [source]
  (let [lines (source-body-lines source)
        nodes (vec (keep (fn [line]
                           (when-let [[_ kind args] (re-matches #"^(Person|System|Container|Component|Boundary|System_Boundary|Container_Boundary)\s*\((.*)\)\s*$" line)]
                             {:kind kind :args args}))
                     lines))
        rels (vec (keep (fn [line]
                          (when-let [[_ args] (re-matches #"^Rel\s*\((.*)\)\s*$" line)]
                            {:args args}))
                    lines))]
    (assoc (structured-diagram :c4 source)
      :nodes nodes
      :relations rels)))

(defn- parse-xy-diagram
  [source]
  (let [lines (source-body-lines source)
        series (vec (filter #(re-matches #"^(line|bar)\s+.*" %) lines))]
    (assoc (structured-diagram :xychart source)
      :series series)))

(defn- parse-generic-known-diagram
  [type source]
  (if (#{:mindmap :tree :timeline :kanban :treemap :ishikawa :architecture :block :packet} type)
    (parse-indent-diagram type source)
    (structured-diagram type source)))

(defn parse-mermaid
  "Parse Mermaid source into a pure Clojure AST. Never evaluates JS."
  [source]
  (let [kind (diagram-kind source)
        type (get diagram-kind->type kind)]
    (case type
      :flowchart (parse-flowchart source)
      :sequence (parse-sequence source)
      :class (parse-class-diagram source)
      :state (parse-state-diagram source)
      :er (parse-er-diagram source)
      :pie (parse-pie-diagram source)
      :sankey (parse-sankey-diagram source)
      :mindmap (parse-indent-diagram :mindmap source)
      :tree (parse-indent-diagram :tree source)
      :quadrant (parse-quadrant-diagram source)
      :c4 (parse-c4-diagram source)
      :xychart (parse-xy-diagram source)
      nil {:diagram/type :unsupported
           :kind (or kind "unknown")
           :warnings [(str "Unsupported diagram type: " (or kind "unknown"))]}
      (parse-generic-known-diagram type source))))

(defn- node-size
  [{:keys [label shape]}]
  (let [lines (label-lines label)
        inner-w (apply max 1 (map count lines))
        w (+ inner-w (case shape
                       :diamond 6
                       :circle 6
                       4))]
    {:w (max 7 w) :h (+ 2 (count lines))}))

(defn- rank-flow-nodes
  [{:keys [nodes order edges]}]
  (let [ids (vec (or (seq order) (keys nodes)))
        outgoing (group-by :source-id edges)
        indeg (frequencies (map :target-id edges))]
    (loop [queue (seq (or (seq (remove #(pos? (get indeg % 0)) ids)) ids))
           seen #{}
           ranks (zipmap ids (repeat 0))]
      (if-let [id (first queue)]
        (let [nexts (remove seen (map :target-id (get outgoing id)))]
          (recur (concat (rest queue) nexts)
            (conj seen id)
            (reduce (fn [r target]
                      (update r target max (inc (get r id 0))))
              ranks
              (map :target-id (get outgoing id)))))
        ranks))))

(defn- grouped-ranks
  [ast]
  (let [ranks (rank-flow-nodes ast)
        order-index (zipmap (:order ast) (range))]
    (->> (:order ast)
      (group-by ranks)
      (sort-by first)
      (mapv (fn [[rank ids]]
              {:rank rank
               :ids (vec (sort-by #(get order-index % 0) ids))})))))

(defn- blank-canvas
  [w h]
  (vec (repeatedly h #(char-array (repeat w \space)))))

(defn- in-canvas?
  [canvas x y]
  (and (<= 0 y) (< y (count canvas))
    (<= 0 x) (< x (alength ^chars (first canvas)))))

(defn- put-char!
  [canvas x y ch]
  (when (in-canvas? canvas x y)
    (aset-char ^chars (nth canvas y) x ch)))

(defn- put-str!
  [canvas x y s]
  (doseq [[idx ch] (map-indexed vector (str s))]
    (put-char! canvas (+ x idx) y ch)))

(defn- draw-h!
  [canvas x1 x2 y ch]
  (doseq [x (range (min x1 x2) (inc (max x1 x2)))]
    (put-char! canvas x y ch)))

(defn- draw-v!
  [canvas x y1 y2 ch]
  (doseq [yy (range (min y1 y2) (inc (max y1 y2)))]
    (put-char! canvas x yy ch)))

(defn- canvas-lines
  [canvas]
  (->> canvas
    (map #(rstrip (String. ^chars %)))
    (drop-while str/blank?)
    reverse
    (drop-while str/blank?)
    reverse
    vec))

(defn- box-lines
  [{:keys [shape] :as node}]
  (let [{:keys [w]} (node-size node)
        lines (label-lines (:label node))
        body (mapv (fn [line]
                     (let [pad-total (max 0 (- w 2 (count line)))
                           left (quot pad-total 2)
                           right (- pad-total left)]
                       (str "│" (repeat-str \space left) line (repeat-str \space right) "│")))
               lines)]
    (case shape
      :diamond (into [(str "╱" (repeat-str \─ (- w 2)) "╲")]
                 (conj body (str "╲" (repeat-str \─ (- w 2)) "╱")))
      :circle  (into [(str "╭" (repeat-str \─ (- w 2)) "╮")]
                 (conj body (str "╰" (repeat-str \─ (- w 2)) "╯")))
      :round   (into [(str "╭" (repeat-str \─ (- w 2)) "╮")]
                 (conj body (str "╰" (repeat-str \─ (- w 2)) "╯")))
      (into [(str "┌" (repeat-str \─ (- w 2)) "┐")]
        (conj body (str "└" (repeat-str \─ (- w 2)) "┘"))))))

(defn- flow-positions-lr
  [ast groups width]
  (let [nodes (:nodes ast)
        sizes (into {} (map (fn [[id node]] [id (node-size node)]) nodes))
        col-widths (mapv (fn [{:keys [ids]}]
                           (apply max 7 (map #(:w (get sizes %)) ids)))
                     groups)
        gaps (max 1 (dec (count col-widths)))
        used (reduce + col-widths)
        h-gap (max 6 (quot (max 0 (- (long width) used 8)) gaps))
        row-step (+ 3 (apply max 3 (map :h (vals sizes))))
        col-x (reductions + 2 (map #(+ % h-gap) col-widths))]
    (into {}
      (mapcat (fn [col {:keys [ids]}]
                (map-indexed
                  (fn [row id]
                    (let [{:keys [w h]} (get sizes id)]
                      [id {:x (nth col-x col)
                           :y (+ 1 (* row row-step))
                           :w w
                           :h h}]))
                  ids))
        (range)
        groups))))

(defn- flow-positions-td
  [ast groups width]
  (let [nodes (:nodes ast)
        sizes (into {} (map (fn [[id node]] [id (node-size node)]) nodes))
        max-cols (apply max 1 (map #(count (:ids %)) groups))
        col-widths (mapv (fn [idx]
                           (apply max 7
                             (for [{:keys [ids]} groups
                                   :let [id (nth ids idx nil)]
                                   :when id]
                               (:w (get sizes id)))))
                     (range max-cols))
        gaps (max 1 (dec (count col-widths)))
        used (reduce + col-widths)
        h-gap (max 5 (quot (max 0 (- (long width) used 8)) gaps))
        row-step (+ 3 (apply max 3 (map :h (vals sizes))))
        col-x (vec (reductions + 2 (map #(+ % h-gap) col-widths)))]
    (into {}
      (mapcat (fn [rank {:keys [ids]}]
                (map-indexed
                  (fn [col id]
                    (let [{:keys [w h]} (get sizes id)]
                      [id {:x (nth col-x col)
                           :y (+ 1 (* rank row-step))
                           :w w
                           :h h}]))
                  ids))
        (range)
        groups))))

(defn- draw-node!
  [canvas {:keys [x y]} node]
  (doseq [[idx line] (map-indexed vector (box-lines node))]
    (put-str! canvas x (+ y idx) line)))

(defn- midpoint
  [a b]
  (quot (+ a b) 2))

(defn- edge-label-text
  [label]
  (when (seq label)
    (clip-line 22 (str "─ " (clean-label label) " ─"))))

(defn- draw-edge-label!
  [canvas x y label]
  (when-let [text (edge-label-text label)]
    (put-str! canvas x y text)))

(defn- draw-edge-lr!
  ([canvas positions edge]
   (draw-edge-lr! canvas positions edge false))
  ([canvas positions {:keys [source-id target-id label style directed? bidir?]} label-only?]
   (let [a (get positions source-id)
         b (get positions target-id)]
     (when (and a b)
       (let [ay (+ (:y a) (quot (:h a) 2))
             by (+ (:y b) (quot (:h b) 2))
             ax (+ (:x a) (:w a))
             bx (dec (:x b))
             ch (case style :dotted \╌ :thick \═ \─)
             arrow (if (< ax bx) \▶ \◀)
             midx (midpoint ax bx)
             label-text (edge-label-text label)
             label-w (count (or label-text ""))]
         (when-not label-only?
           (if (= ay by)
             (draw-h! canvas ax bx ay ch)
             (do
               (draw-h! canvas ax midx ay ch)
               (draw-v! canvas midx ay by \│)
               (draw-h! canvas midx bx by ch)))
           (when bidir? (put-char! canvas ax ay (if (< ax bx) \◀ \▶)))
           (when directed? (put-char! canvas bx by arrow)))
         (when (and label-only? label-text)
           (let [line-y (if (= ay by) ay (if (< ay by) (inc ay) (dec ay)))
                 gap-start (inc (min ax bx))
                 gap-end (dec (max ax bx))
                 available (max 0 (- gap-end gap-start -1))
                 label-x (if (>= available label-w)
                           (+ gap-start (quot (- available label-w) 2))
                           (min midx (max 0 (- (alength ^chars (first canvas)) label-w 1))))]
             (draw-edge-label! canvas label-x line-y label))))))))

(defn- draw-edge-td!
  ([canvas positions edge]
   (draw-edge-td! canvas positions edge false))
  ([canvas positions {:keys [source-id target-id label style directed? bidir?]} label-only?]
   (let [a (get positions source-id)
         b (get positions target-id)]
     (when (and a b)
       (let [ax (+ (:x a) (quot (:w a) 2))
             bx (+ (:x b) (quot (:w b) 2))
             ay (+ (:y a) (:h a))
             by (dec (:y b))
             ch (case style :dotted \╌ :thick \║ \│)
             arrow (if (< ay by) \▼ \▲)
             midy (midpoint ay by)]
         (when-not label-only?
           (if (= ax bx)
             (draw-v! canvas ax ay by ch)
             (do
               (draw-v! canvas ax ay midy ch)
               (draw-h! canvas ax bx midy \─)
               (draw-v! canvas bx midy by ch)))
           (when bidir? (put-char! canvas ax ay (if (< ay by) \▲ \▼)))
           (when directed? (put-char! canvas bx by arrow)))
         (when (and label-only? label)
           (draw-edge-label! canvas (+ 2 (min ax bx)) midy label)))))))

(defn- prefixed-wrap
  [width prefix continuation text]
  (let [body-width (max 10 (- (long (or width 80)) (count prefix)))
        lines (map-indexed vector (wrap-words body-width text))]
    (mapv (fn [[idx line]]
            (str (if (zero? idx) prefix continuation) line))
      lines)))

(defn- render-edge-row
  [width last? {:keys [target-id label directed?]} nodes]
  (let [target (clean-label (get-in nodes [target-id :label] target-id))
        text (str (when label (str (clean-label label) " → ")) target)
        branch (if last? "└" "├")
        arrow (if directed? "▶ " "─ ")]
    (prefixed-wrap width (str "  " branch "─" arrow) "  │   " text)))

(defn- render-flow-edge-list
  [{:keys [nodes order edges warnings]} width]
  (let [by-source (group-by :source-id edges)
        ordered-sources (->> (concat order (map :source-id edges))
                          distinct
                          (filter by-source))
        lines (mapcat (fn [source-id]
                        (let [source-label (clean-label (get-in nodes [source-id :label] source-id))
                              source-lines (prefixed-wrap width "• " "  " source-label)
                              outgoing (vec (get by-source source-id))]
                          (concat source-lines
                            (mapcat (fn [[idx edge]]
                                      (render-edge-row width (= idx (dec (count outgoing))) edge nodes))
                              (map-indexed vector outgoing)))))
                ordered-sources)]
    (cond-> (vec lines)
      (seq warnings) (conj (str "Warnings: " (count warnings))))))

(defn- flow-layout-candidate
  [ast groups direction width]
  (let [horizontal? (#{"LR" "RL"} direction)
        positions (if horizontal?
                    (flow-positions-lr ast groups width)
                    (flow-positions-td ast groups width))
        max-x (apply max 40 (map (fn [[_ {:keys [x w]}]] (+ x w 3)) positions))
        max-y (apply max 8 (map (fn [[_ {:keys [y h]}]] (+ y h 3)) positions))]
    {:layout/type :canvas
     :direction direction
     :ast ast
     :positions positions
     :w max-x
     :h max-y}))

(defn- preferred-fallback-directions
  [direction]
  (if (#{"LR" "RL"} direction)
    [direction "TD"]
    [direction "LR"]))

(defn- complex-flowchart?
  [{:keys [nodes edges]} groups]
  (let [ranks (into {}
                (mapcat (fn [{:keys [rank ids]}]
                          (map #(vector % rank) ids))
                  groups))]
    (or (> (count nodes) 6)
      (> (count edges) 10)
      (some (fn [{:keys [source-id target-id]}]
              (>= (get ranks source-id 0) (get ranks target-id 0)))
        edges))))

(defn- layout-flowchart
  ([ast]
   (layout-flowchart ast {}))
  ([{:keys [nodes edges direction] :as ast} {:keys [width]}]
   (let [width (max 20 (long (or width 80)))]
     (if (or (empty? edges)
           (> (count nodes) max-flow-nodes)
           (> (count edges) max-flow-edges))
       {:layout/type :edge-list :ast ast :width width}
       (let [groups (grouped-ranks ast)]
         (if (complex-flowchart? ast groups)
           {:layout/type :edge-list :ast ast :width width}
           (let [candidates (mapv #(flow-layout-candidate ast groups % width)
                              (preferred-fallback-directions direction))]
             (or (first (filter #(<= (:w %) width) candidates))
               (first candidates)))))))))

(defn- render-flowchart-layout
  [{:keys [layout/type ast positions w h direction width]}]
  (if (= type :edge-list)
    (render-flow-edge-list ast width)
    (let [canvas (blank-canvas w h)
          horizontal? (#{"LR" "RL"} direction)]
      (doseq [edge (:edges ast)]
        (if horizontal?
          (draw-edge-lr! canvas positions edge false)
          (draw-edge-td! canvas positions edge false)))
      (doseq [edge (:edges ast)]
        (if horizontal?
          (draw-edge-lr! canvas positions edge true)
          (draw-edge-td! canvas positions edge true)))
      (doseq [id (:order ast)]
        (draw-node! canvas (get positions id) (get-in ast [:nodes id])))
      (canvas-lines canvas))))

(defn- sequence-layout
  [{:keys [participants order events] :as ast}]
  (let [labels (mapv #(get-in participants [% :label] %) order)
        col-widths (mapv #(max 8 (+ 2 (count (bounded-label %)))) labels)
        xs (vec (reductions + 2 (map #(+ % 6) col-widths)))]
    {:layout/type :sequence
     :ast ast
     :order order
     :labels labels
     :xs xs
     :lifeline-y 4
     :events events
     :w (+ (last xs) (or (last col-widths) 8) 4)
     :h (+ 8 (* 3 (count events)))}))

(defn- render-sequence-event
  [{:keys [order labels]} event]
  (case (:kind event)
    :message
    (let [from-idx (.indexOf ^java.util.List order (:from event))
          to-idx (.indexOf ^java.util.List order (:to event))
          from-label (nth labels from-idx (:from event))
          to-label (nth labels to-idx (:to event))]
      (str from-label " " (if (:dashed? event) "⇢" "->") " " to-label ": " (:text event)))

    :note
    (str "Note " (:pos event) " " (str/join "," (:of event)) ": " (:text event))

    (str event)))

(defn- render-sequence-layout
  [{:keys [labels events] :as layout}]
  (if (seq events)
    (into [(str "Participants: " (str/join " | " labels))]
      (map #(render-sequence-event layout %) events))
    ["No renderable messages found."]))

(defn- title-lines
  [{:keys [title]}]
  (when (seq title)
    [(str "Title: " title)]))

(defn- bar-line
  [label value max-value width]
  (let [bar-width (max 1 (min 40 (- width 24)))
        n (if (pos? max-value)
            (long (Math/round (* bar-width (/ (double value) max-value))))
            0)]
    (str "• " label " " (repeat-str \█ n) " " value)))

(defn- render-indent-items
  [items]
  (mapv (fn [{:keys [indent text]}]
          (str (repeat-str \space (quot indent 2)) "• " text))
    items))

(defn- render-structured-layout
  [{:keys [ast width]}]
  (let [{:keys [diagram/type items warnings]} ast
        base (cond-> []
               (:title ast) (into (title-lines ast)))]
    (cond->
      (into base
        (case type
          :class
          (concat
            (when (seq (:classes ast))
              [(str "Classes: " (str/join ", " (distinct (:classes ast))))])
            (map #(str "• " %) (or (seq (:relations ast)) items)))

          :state
          (or (seq (map (fn [{:keys [from to label]}]
                          (str "• " from " -> " to (when label (str ": " label))))
                     (:transitions ast)))
            (map #(str "• " %) items))

          :er
          (or (seq (map (fn [{:keys [from rel to label]}]
                          (str "• " from " " rel " " to (when-not (str/blank? label) (str ": " label))))
                     (:relationships ast)))
            (map #(str "• " %) items))

          :pie
          (let [slices (:slices ast)
                max-value (apply max 0 (map :value slices))]
            (if (seq slices)
              (map #(bar-line (:label %) (:value %) max-value width) slices)
              (map #(str "• " %) items)))

          :sankey
          (or (seq (map (fn [{:keys [from to value]}]
                          (str "• " from " ── " value " ──▶ " to))
                     (:links ast)))
            (map #(str "• " %) items))

          :quadrant
          (concat
            (map #(str "• " %) (remove #(re-matches #".*:\s*\[[^]]+\]\s*" %) items))
            (map (fn [{:keys [label x y]}]
                   (str "• " label " @ [" x ", " y "]"))
              (:points ast)))

          :c4
          (concat
            (map (fn [{:keys [kind args]}] (str "• " kind "(" args ")")) (:nodes ast))
            (map (fn [{:keys [args]}] (str "• Rel(" args ")")) (:relations ast)))

          :xychart
          (or (seq (map #(str "• " %) (:series ast)))
            (map #(str "• " %) items))

          (:mindmap :tree :timeline :kanban :treemap :ishikawa :architecture :block :packet)
          (render-indent-items items)

          (map #(str "• " %) items)))
      (seq warnings) (conj (str "Warnings: " (count warnings))))))

(defn layout-diagram
  "Lay out a parsed Mermaid AST into terminal-renderable geometry."
  ([ast]
   (layout-diagram ast {}))
  ([ast opts]
   (case (:diagram/type ast)
     :flowchart (layout-flowchart ast opts)
     :sequence (sequence-layout ast)
     :unsupported {:layout/type :unsupported :ast ast}
     {:layout/type :structured
      :ast ast
      :width (:width opts)})))

(defn render-layout
  "Render Mermaid layout to terminal lines."
  [layout]
  (case (:layout/type layout)
    :canvas (render-flowchart-layout layout)
    :edge-list (render-flowchart-layout layout)
    :sequence (render-sequence-layout layout)
    :structured (render-structured-layout layout)
    ["Mermaid" (first (or (get-in layout [:ast :warnings]) ["Unsupported diagram"]))]))

(defn render-mermaid
  "Fenced renderer entry point. Pure JVM, bounded, terminal-safe.
   Returns `{:lines [...]}` for Vis' generic fenced renderer API."
  [{:keys [source width]}]
  (let [width (max 20 (min max-render-width (long (or width 80))))
        lines (str/split-lines (or source ""))]
    {:lines
     (clip-lines width
       (cond
         (> (count source) max-source-chars)
         ["Mermaid" (str "Source too large (" (count source) " chars).")]

         (> (count lines) max-source-lines)
         ["Mermaid" (str "Source too large (" (count lines) " lines).")]

         :else
         (-> source
           parse-mermaid
           (layout-diagram {:width width})
           render-layout)))}))

(def mermaid-extension
  (vis/extension
    {:ext/namespace 'com.blockether.vis.ext.mermaid.core
     :ext/doc "Pure-JVM Mermaid parser/layout renderer for terminal ASCII/text. No Node, browser, Mermaid.js, or Graal JS."
     :ext/version "0.2.0"
     :ext/author "Blockether"
     :ext/owner "vis"
     :ext/license "Apache-2.0"
     :ext/kind "renderers"
     :ext/fenced-renderers
     [{:renderer/id :mermaid/ascii
       :renderer/langs #{"mermaid"}
       :renderer/render-fn render-mermaid}]}))

(vis/register-extension! mermaid-extension)
