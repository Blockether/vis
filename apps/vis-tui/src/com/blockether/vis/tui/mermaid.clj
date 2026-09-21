(ns com.blockether.vis.tui.mermaid
  "Mermaid flowchart fences painted as box-drawing diagrams.

   `diagram` is TOTAL: it answers the rendered rows, or nil when the fence is
   not a flowchart this renderer understands — another diagram type, a
   subgraph, a graph too wide for the bubble. The caller then paints the fence
   source verbatim, so an unsupported diagram never shows a broken picture.

   Pipeline: parse -> rank -> order -> place -> draw.

   Ranking is longest-path over the acyclic edges; an edge that closes a cycle
   (a retry loop, a self loop) is routed through a lane beside the diagram
   instead of bending the ranks. Ordering runs barycentre sweeps to cut
   crossings, placement pulls each node toward the median of its neighbours
   while keeping a minimum gap. Drawing merges line cells by DIRECTION BITS,
   so a crossing or a join picks the right joint glyph instead of overwriting."
  (:require [clojure.string :as str]
            [com.blockether.vis.tui.primitives :as p]))

;; Parsing

(def ^:private header-re #"(?i)^(?:flowchart|graph)(?:\s+(TD|TB|BT|LR|RL))?$")

(def ^:private flow-of {"TD" :down "TB" :down "BT" :up "LR" :right "RL" :left})

(def ^:private ignored-re
  #"(?i)^(?:classdef|class|style|linkstyle|click|acctitle|accdescr|direction)\b")

(def ^:private unsupported-re #"(?i)^(?:subgraph|end)\b")

(def ^:private shape-forms
  "Opening / closing delimiters longest first, so `([` wins over `(`."
  [["([" "])" :stadium] ["[[" "]]" :subroutine] ["[(" ")]" :cylinder] ["((" "))" :circle]
   ["{{" "}}" :hexagon] ["[/" "/]" :rect] ["[\\" "\\]" :rect] ["[" "]" :rect] ["(" ")" :round]
   ["{" "}" :diamond] [">" "]" :flag]])

(defn- strip-comment
  "Drop a `%%` comment, which also erases a `%%{init: ...}%%` directive line."
  [line]
  (if-let [at (str/index-of line "%%")]
    (subs line 0 at)
    line))

(defn- clean-label
  [raw]
  (-> (or raw "")
      str/trim
      (str/replace #"^\"(.*)\"$" "$1")
      (str/replace #"(?i)<br\s*/?>" "\n")
      (str/replace #"(?i)&quot;|#quot;" "\"")
      (str/replace #"(?i)&amp;|#amp;" "&")
      (str/replace #"(?i)&nbsp;" " ")
      (str/replace #"<[^>]*>" "")
      str/trim))

(defn- normalize-links
  "Rewrite mid-text links into the pipe form, so one tokenizer reads them all:
   `A -- yes --> B` becomes `A -->|yes| B`, `A -. no .-> B` becomes `A -.->|no| B`."
  [statement]
  (-> statement
      (str/replace #"-\.\s+(\S.*?)\s+\.-([->ox]?)"
                   (fn [[_ text head]]
                     (str "-.-" head "|" text "|")))
      (str/replace #"(?:-{2,}|={2,})\s+(\S.*?)\s+(-{2,}|={2,})([->ox]?)(?=\s|$)"
                   (fn [[_ text link head]]
                     (str link head "|" text "|")))))

(def ^:private id-re #"^[A-Za-z0-9_](?:[A-Za-z0-9_.\-]*[A-Za-z0-9_])?")

(defn- read-label
  "Label body between `open` and `close`, answering [label rest] or nil."
  [s open close]
  (let [body (subs s (count open))]
    (if (str/starts-with? body "\"")
      (when-let [end (str/index-of body "\"" 1)]
        (let [after (str/triml (subs body (inc (long end))))]
          (when (str/starts-with? after close) [(subs body 1 end) (subs after (count close))])))
      (when-let [end (str/index-of body close)]
        [(subs body 0 end) (subs body (+ (long end) (count close)))]))))

(defn- read-node
  [s]
  (when-let [id (re-find id-re s)]
    (let [tail (subs s (count id))
          form (first (filter (fn [[open]]
                                (str/starts-with? tail open))
                              shape-forms))]

      (if form
        (let [[open close shape] form]
          (when-let [[label tail'] (read-label tail open close)]
            [{:id id :label (clean-label label) :shape shape} tail']))
        [{:id id} tail]))))

(defn- read-node-list
  "One `A & B` group of node references, answering [nodes rest] or nil."
  [s]
  (loop [s
         (str/triml s)

         acc
         []]

    (if-let [[node tail] (read-node s)]
      (let [tail (str/triml tail)
            acc (conj acc node)]

        (if (str/starts-with? tail "&") (recur (str/triml (subs tail 1)) acc) [acc tail]))
      nil)))

(def ^:private link-re #"^([-.=<>xo]{2,})(?:\|([^|]*)\|)?")

(defn- read-link
  [s]
  (when-let [[whole token label] (re-find link-re s)]
    [{:style (cond (str/includes? token ".") :dotted
                   (str/includes? token "=") :thick
                   :else :solid)
      :head? (boolean (re-find #"[>ox]$" token))
      :tail? (boolean (re-find #"^[<xo]" token))
      :label (clean-label label)} (subs s (count whole))]))

(defn- add-node
  [graph node]
  (let [id
        (:id node)

        known
        (get-in graph [:nodes id])

        merged
        (cond-> (or known {:id id :label id :shape :rect})
          (:shape node)
          (assoc :shape (:shape node))

          (seq (:label node))
          (assoc :label (:label node)))]

    (cond-> (assoc-in graph [:nodes id] merged)
      (nil? known)
      (update :order conj id))))

(defn- parse-statement
  [graph statement]
  (when-let [[nodes tail] (read-node-list statement)]
    (loop [graph (reduce add-node graph nodes)
           prev nodes
           s (str/triml tail)]

      (if (str/blank? s)
        graph
        (when-let [[link tail'] (read-link s)]
          (when-let [[nodes' tail''] (read-node-list tail')]
            (recur (-> (reduce add-node graph nodes')
                       (update :edges
                               into
                               (for [a prev
                                     b nodes']

                                 (assoc link
                                   :from (:id a)
                                   :to (:id b)))))
                   nodes'
                   (str/triml tail''))))))))

(defn- parse
  "Answer `{:flow :order :nodes :edges}` for a supported flowchart, else nil."
  [source]
  (let [statements
        (->> (str/split-lines (or source ""))
             (map strip-comment)
             (mapcat #(str/split % #";"))
             (map str/trim)
             (remove str/blank?))

        header
        (first statements)]

    (when-let [[_ dir] (and header (re-find header-re header))]
      (let [graph (reduce (fn [graph statement]
                            (cond (re-find ignored-re statement) graph
                                  (re-find unsupported-re statement) (reduced nil)
                                  :else (or (parse-statement graph (normalize-links statement))
                                            (reduced nil))))
                          {:order [] :nodes {} :edges []}
                          (rest statements))]
        (when (seq (:order graph))
          (assoc graph
            :flow (get flow-of
                       (some-> dir
                               str/upper-case)
                       :down)))))))

;; Graph shape

(defn- cycle-edge-indexes
  "Edge indexes that close a cycle, plus every self loop: the ones a lane routes."
  [order edges]
  (let [adjacency
        (reduce (fn [m [at edge]]
                  (if (= (:from edge) (:to edge))
                    m
                    (update m (:from edge) (fnil conj []) [at (:to edge)])))
                {}
                (map-indexed vector edges))

        state
        (volatile! {})

        back
        (volatile! #{})

        visit
        (fn visit [id]
          (vswap! state assoc id :open)
          (doseq [[at to] (get adjacency id)]
            (case (get @state to)
              :open
              (vswap! back conj at)

              :done
              nil

              (visit to)))
          (vswap! state assoc id :done))]

    (doseq [id order]
      (when-not (get @state id) (visit id)))
    (into @back
          (keep-indexed (fn [at edge]
                          (when (= (:from edge) (:to edge)) at))
                        edges))))

(defn- rank-nodes
  "Longest-path rank per node over the acyclic edges."
  [order edges]
  (loop [ranks
         (zipmap order (repeat 0))

         passes
         (count order)]

    (let [next-ranks
          (reduce (fn [ranks edge]
                    (let [want (inc (long (ranks (:from edge))))]
                      (if (> want (long (ranks (:to edge)))) (assoc ranks (:to edge) want) ranks)))
                  ranks
                  edges)]
      (if (or (= next-ranks ranks) (neg? passes)) next-ranks (recur next-ranks (dec passes))))))

;; Label measurement

(defn- cut-to-width
  [^String text ^long limit]
  (loop [at
         0

         used
         0]

    (if (or (>= at (.length text)) (> (+ used (p/display-width (str (.charAt text at)))) limit))
      [(subs text 0 at) (subs text at)]
      (recur (inc at) (+ used (long (p/display-width (str (.charAt text at)))))))))

(defn- wrap-words
  [text ^long limit]
  (loop [words
         (remove str/blank? (str/split (str/trim text) #"\s+"))

         line
         ""

         out
         []]

    (if-let [word (first words)]
      (cond (str/blank? line) (if (> (long (p/display-width word)) limit)
                                (let [[head tail] (cut-to-width word limit)]
                                  (recur (cons tail (rest words)) "" (conj out head)))
                                (recur (rest words) word out))
            (<= (+ (long (p/display-width line)) 1 (long (p/display-width word))) limit)
            (recur (rest words) (str line " " word) out)
            :else (recur words "" (conj out line)))
      (cond (seq line) (conj out line)
            (seq out) out
            :else [""]))))

(defn- label-lines
  [text ^long limit]
  (vec (mapcat #(wrap-words % limit) (str/split-lines (if (str/blank? text) " " text)))))

;; Canvas

(def ^:private up-bit 1)

(def ^:private down-bit 2)

(def ^:private left-bit 4)

(def ^:private right-bit 8)

(def ^:private light-glyphs
  {1 \│ 2 \│ 3 \│ 4 \─ 8 \─ 12 \─ 5 \┘ 9 \└ 6 \┐ 10 \┌ 7 \┤ 11 \├ 13 \┴ 14 \┬ 15 \┼})

(def ^:private heavy-glyphs
  {1 \┃ 2 \┃ 3 \┃ 4 \━ 8 \━ 12 \━ 5 \┛ 9 \┗ 6 \┓ 10 \┏ 7 \┫ 11 \┣ 13 \┻ 14 \┳ 15 \╋})

(def ^:private dotted-glyphs (merge light-glyphs {1 \╎ 2 \╎ 3 \╎ 4 \╌ 8 \╌ 12 \╌}))

(def ^:private style-glyphs {:solid light-glyphs :thick heavy-glyphs :dotted dotted-glyphs})

(defn- make-canvas
  [^long rows ^long cols]
  {:rows rows
   :cols cols
   :chars (let [grid (make-array Character/TYPE rows cols)]
            (dotimes [row rows]
              (java.util.Arrays/fill ^chars (aget ^"[[C" grid row) \space))
            grid)
   :bits (make-array Integer/TYPE rows cols)
   :styles (make-array Integer/TYPE rows cols)})

(defn- inside?
  [canvas row col]
  (and (>= (long row) 0)
       (>= (long col) 0)
       (< (long row) (long (:rows canvas)))
       (< (long col) (long (:cols canvas)))))

(defn- put-char!
  [canvas row col ch]
  (when (inside? canvas row col)
    (aset-char ^chars (aget ^"[[C" (:chars canvas) (long row)) (long col) ch)))

(defn- put-text!
  [canvas row col ^String text]
  (dotimes [at (.length text)]
    (put-char! canvas row (+ (long col) at) (.charAt text at))))

(def ^:private style-code {:solid 0 :dotted 1 :thick 2})

(def ^:private code-style {0 :solid 1 :dotted 2 :thick})

(defn- put-line!
  "Merge direction bits into one cell; a thicker style wins the joint."
  [canvas row col bits style]
  (when (inside? canvas row col)
    (let [^ints bit-row
          (aget ^"[[I" (:bits canvas) (long row))

          ^ints style-row
          (aget ^"[[I" (:styles canvas) (long row))

          col
          (long col)]

      (aset-int bit-row col (bit-or (aget bit-row col) (long bits)))
      (aset-int style-row col (max (aget style-row col) (long (style-code style 0)))))))

(defn- draw-vertical!
  "Line cells between two rows on one column, endpoints included."
  [canvas from-row to-row col style]
  (let [from-row
        (long from-row)

        to-row
        (long to-row)

        step
        (if (<= from-row to-row) 1 -1)]

    (loop [row from-row]
      (let [head? (= row from-row)
            tail? (= row to-row)]

        (put-line! canvas
                   row
                   col
                   (bit-or (long (if (or (not head?) (= from-row to-row))
                                   (if (pos? step) up-bit down-bit)
                                   0))
                           (long (if (not tail?) (if (pos? step) down-bit up-bit) 0)))
                   style)
        (when-not tail? (recur (+ row step)))))))

(defn- draw-horizontal!
  [canvas row from-col to-col style]
  (let [from-col
        (long from-col)

        to-col
        (long to-col)

        step
        (if (<= from-col to-col) 1 -1)]

    (loop [col from-col]
      (let [head? (= col from-col)
            tail? (= col to-col)]

        (put-line! canvas
                   row
                   col
                   (bit-or (long (if (or (not head?) (= from-col to-col))
                                   (if (pos? step) left-bit right-bit)
                                   0))
                           (long (if (not tail?) (if (pos? step) right-bit left-bit) 0)))
                   style)
        (when-not tail? (recur (+ col step)))))))

;; Boxes

(def ^:private box-chrome
  {:rect {:tl \┌ :tr \┐ :bl \└ :br \┘}
   :subroutine {:tl \┌ :tr \┐ :bl \└ :br \┘}
   :cylinder {:tl \╭ :tr \╮ :bl \╰ :br \╯}
   :round {:tl \╭ :tr \╮ :bl \╰ :br \╯}
   :stadium {:tl \╭ :tr \╮ :bl \╰ :br \╯}
   :circle {:tl \╭ :tr \╮ :bl \╰ :br \╯}
   :flag {:tl \╭ :tr \╮ :bl \╰ :br \╯}
   :hexagon {:tl \╱ :tr \╲ :bl \╲ :br \╱}
   :diamond {:tl \╱ :tr \╲ :bl \╲ :br \╱}})

(defn- draw-box!
  [canvas item]
  (let [{:keys [lines shape]}
        item

        row
        (long (:row item))

        col
        (long (:col item))

        width
        (long (:width item))

        height
        (long (:height item))

        {:keys [tl tr bl br]}
        (box-chrome shape (box-chrome :rect))

        inner
        (- width 2)]

    (put-char! canvas row col tl)
    (put-char! canvas row (+ col (dec width)) tr)
    (put-char! canvas (+ row (dec height)) col bl)
    (put-char! canvas (+ row (dec height)) (+ col (dec width)) br)
    (dotimes [at inner]
      (put-char! canvas row (+ col 1 at) \─)
      (put-char! canvas (+ row (dec height)) (+ col 1 at) \─))
    (doseq [[at text] (map-indexed vector lines)]
      (let [line-row (+ row 1 (long at))
            used (long (p/display-width text))
            pad (quot (- inner used) 2)]

        (put-char! canvas line-row col \│)
        (put-char! canvas line-row (+ col (dec width)) \│)
        (dotimes [fill inner]
          (put-char! canvas line-row (+ col 1 fill) \space))
        (put-text! canvas line-row (+ col 1 pad) text)))))

;; Layout

(def ^:private minor-gap 3)

(def ^:private row-gap 1)

(defn- pack-positions
  "Left-to-right sweep honouring `gap`, then a right-to-left compaction."
  [sizes wanted ^long gap]
  (let [forward
        (reduce (fn [acc at]
                  (let [want (long (nth wanted at))
                        edge (if (zero? (long at))
                               want
                               (max want
                                    (+ (long (peek acc)) (long (nth sizes (dec (long at)))) gap)))]

                    (conj acc edge)))
                []
                (range (count sizes)))]
    (reduce (fn [acc at]
              (let [want (long (nth wanted at))
                    limit (- (long (nth acc (inc at))) gap (long (nth sizes at)))]

                (assoc acc at (max want (min (long (nth acc at)) limit)))))
            forward
            (reverse (range (dec (count sizes)))))))

(defn- median
  [values]
  (let [sorted
        (vec (sort values))

        n
        (count sorted)]

    (when (pos? n)
      (if (odd? n)
        (double (nth sorted (quot n 2)))
        (/ (+ (double (nth sorted (dec (quot n 2)))) (double (nth sorted (quot n 2)))) 2.0)))))

(defn- order-ranks
  "Barycentre sweeps: each rank is re-sorted by the median position of its
   neighbours in the rank the sweep came from, which is what removes most
   crossings from a hand-written flowchart."
  [rank-keys segments]
  (let [preds
        (reduce (fn [m s]
                  (update m (:to s) (fnil conj []) (:from s)))
                {}
                segments)

        succs
        (reduce (fn [m s]
                  (update m (:from s) (fnil conj []) (:to s)))
                {}
                segments)]

    (reduce (fn [ranks pass]
              (let [down?
                    (even? (long pass))

                    neighbours
                    (if down? preds succs)

                    steps
                    (if down? (range 1 (count ranks)) (reverse (range 0 (dec (count ranks)))))]

                (reduce (fn [ranks at]
                          (let [reference
                                (nth ranks (if down? (dec (long at)) (inc (long at))))

                                index
                                (zipmap reference (range))

                                keys
                                (nth ranks at)

                                place
                                (zipmap keys (range))]

                            (assoc ranks
                              at (vec (sort-by (fn [key]
                                                 (let [near (keep index (get neighbours key))]
                                                   (if (seq near)
                                                     (double (median near))
                                                     (double (place key)))))
                                               keys)))))
                        ranks
                        steps)))
            rank-keys
            (range 4))))

(defn- place-minor
  "Minor-axis start of every item: one sequential pack, then median passes that
   pull a node toward its neighbours without breaking the minimum gap. Packing
   spends `sizes` (the box plus the room its edge labels need), while the pull
   aims at `boxes` — a node centres under its parent's BOX, not under the room
   reserved beside it."
  [rank-keys sizes boxes segments gap]
  (let [gap
        (long gap)

        preds
        (reduce (fn [m s]
                  (update m (:to s) (fnil conj []) (:from s)))
                {}
                segments)

        succs
        (reduce (fn [m s]
                  (update m (:from s) (fnil conj []) (:to s)))
                {}
                segments)

        initial
        (reduce (fn [pos keys]
                  (first (reduce (fn [[pos at] key]
                                   [(assoc pos key at) (+ (long at) (long (sizes key)) gap)])
                                 [pos 0]
                                 keys)))
                {}
                rank-keys)

        ;; A node's PORT column, not its geometric centre: aligning ports is what
        ;; makes a single parent / single child pair draw one straight line.
        port
        (fn [key]
          (quot (dec (long (boxes key))) 2))

        centre
        (fn [pos key]
          (double (+ (long (pos key)) (long (port key)))))]

    (reduce
      (fn [pos pass]
        (let [down?
              (even? (long pass))

              neighbours
              (if down? preds succs)

              steps
              (if down? (range 1 (count rank-keys)) (reverse (range 0 (dec (count rank-keys)))))]

          (reduce (fn [pos at]
                    (let [keys
                          (nth rank-keys at)

                          wanted
                          (mapv (fn [key]
                                  (let [near (map #(centre pos %) (get neighbours key))]
                                    (if (seq near)
                                      (- (long (Math/round (double (median near))))
                                         (long (port key)))
                                      (long (pos key)))))
                                keys)

                          packed
                          (pack-positions (mapv sizes keys) wanted gap)]

                      (reduce (fn [pos [key at]]
                                (assoc pos key at))
                              pos
                              (map vector keys packed))))
                  pos
                  steps)))
      initial
      (range 4))))

;; Layered model

(defn- layered
  "Nodes and dummy chain items keyed by rank, with one segment per rank step."
  [graph ^long label-limit]
  (let [{:keys [order nodes edges flow]}
        graph

        lane-at
        (cycle-edge-indexes order edges)

        flow-edges
        (keep-indexed (fn [at edge]
                        (when-not (lane-at at) edge))
                      edges)

        lane-edges
        (keep-indexed (fn [at edge]
                        (when (lane-at at) edge))
                      edges)

        ranks
        (rank-nodes order flow-edges)

        vertical?
        (contains? #{:down :up} flow)

        node-items
        (into {}
              (for [id order]
                (let [node (nodes id)
                      lines (label-lines (:label node) label-limit)
                      width (+ 4 (long (apply max 1 (map #(long (p/display-width %)) lines))))
                      height (+ 2 (count lines))]

                  [id
                   (assoc node
                     :key id
                     :kind :node
                     :rank (long (ranks id))
                     :lines lines
                     :width width
                     :height height)])))

        chains
        (map-indexed
          (fn [at edge]
            (let [from-rank
                  (long (ranks (:from edge)))

                  to-rank
                  (long (ranks (:to edge)))

                  inner
                  (range (inc from-rank) to-rank)

                  keys
                  (mapv (fn [rank]
                          [:dummy at rank])
                        inner)

                  path
                  (concat [(:from edge)] keys [(:to edge)])]

              {:dummies
               (mapv (fn [key rank]
                       {:key key :kind :dummy :rank rank :style (:style edge) :width 1 :height 1})
                     keys
                     inner)
               :segments (mapv (fn [[from to] at']
                                 {:from from
                                  :to to
                                  :style (:style edge)
                                  :label (when (zero? (long at')) (:label edge))
                                  :head? (and (:head? edge) (= to (:to edge)))
                                  :tail? (and (:tail? edge) (= from (:from edge)))})
                               (partition 2 1 path)
                               (range))}))
          flow-edges)

        dummy-items
        (into {}
              (for [item (mapcat :dummies chains)]
                [(:key item) item]))

        segments
        (vec (mapcat :segments chains))

        items
        (merge node-items dummy-items)

        rank-count
        (inc (long (apply max 0 (map :rank (vals items)))))

        rank-keys
        (mapv (fn [rank]
                (vec (concat (filter #(= rank (:rank (items %))) order)
                             (->> (vals dummy-items)
                                  (filter #(= rank (:rank %)))
                                  (sort-by :key)
                                  (map :key)))))
              (range rank-count))]

    {:flow flow
     :vertical? vertical?
     :items items
     :segments segments
     :lane-edges (vec lane-edges)
     :rank-keys (order-ranks rank-keys segments)}))

(defn- minor-size [item vertical?] (if vertical? (long (:width item)) (long (:height item))))

(defn- major-size [item vertical?] (if vertical? (long (:height item)) (long (:width item))))

(defn- label-allowance
  "Minor room reserved beside a source box for its outgoing edge labels."
  [key segments vertical?]
  (if vertical?
    (let [widths (for [segment segments
                       :when (and (= key (:from segment)) (seq (:label segment)))]

                   (+ 2 (long (p/display-width (:label segment)))))]
      (long (apply max 0 widths)))
    0))

(defn- geometry
  "Physical row / col of every item, plus the canvas size."
  [layout]
  (let [{:keys [items segments rank-keys vertical? flow lane-edges]}
        layout

        gap
        (if vertical? minor-gap row-gap)

        boxes
        (into {}
              (for [[key item] items]
                [key (minor-size item vertical?)]))

        sizes
        (into {}
              (for [[key item] items]
                [key (+ (minor-size item vertical?) (label-allowance key segments vertical?))]))

        placed
        (place-minor rank-keys sizes boxes segments gap)

        lowest
        (long (apply min (vals placed)))

        placed
        (into {}
              (for [[key at] placed]
                [key (- (long at) lowest)]))

        rank-major
        (mapv (fn [keys]
                (long (apply max 1 (map #(major-size (items %) vertical?) keys))))
              rank-keys)

        band-label?
        (fn [rank]
          (boolean (some (fn [segment]
                           (and (seq (:label segment))
                                (= (dec (long rank)) (long (:rank (items (:from segment)))))))
                         segments)))

        lanes?
        (boolean (seq lane-edges))

        bands
        (mapv (fn [rank]
                (cond (or (zero? (long rank)) (= (long rank) (count rank-keys))) (if lanes? 2 0)
                      vertical? (if (band-label? rank) 4 3)
                      :else (let [widest
                                  (long (apply max
                                          0
                                          (for [segment segments
                                                :when (and (seq (:label segment))
                                                           (= (dec (long rank))
                                                              (long (:rank (items (:from
                                                                                    segment))))))]

                                            (p/display-width (:label segment)))))]
                              (if (pos? widest) (+ widest 6) 5))))
              (range (inc (count rank-keys))))

        starts
        (loop [rank
               0

               at
               (long (first bands))

               acc
               []]

          (if (>= rank (count rank-keys))
            acc
            (recur (inc rank)
                   (+ at (long (nth rank-major rank)) (long (nth bands (inc rank))))
                   (conj acc at))))

        total-major
        (+ (long (reduce + bands)) (long (reduce + rank-major)))

        total-minor
        (long (apply max
                1
                (for [[key at] placed]
                  (+ (long at) (minor-size (items key) vertical?)))))

        lane-count
        (count lane-edges)

        lane-extra
        (if (pos? lane-count) (inc (* 2 lane-count)) 0)

        positioned
        (into
          {}
          (for [[rank keys]
                (map-indexed vector rank-keys)

                key
                keys]

            (let [item
                  (items key)

                  dummy?
                  (= :dummy (:kind item))

                  span
                  (long (nth rank-major rank))

                  own
                  (major-size item vertical?)

                  major
                  (+ (long (nth starts rank)) (if dummy? 0 (quot (- span own) 2)))

                  own
                  (if dummy? span own)

                  ;; BT and RL run the ranks against the axis: mirror
                  ;; the major coordinate so rank 0 sits at the bottom
                  ;; (or the right) and every arrow points that way.
                  major
                  (if (contains? #{:up :left} flow) (- total-major major own) major)

                  minor
                  (long (placed key))]

              [key
               (if vertical?
                 (assoc item
                   :row major
                   :col minor
                   :height own)
                 (assoc item
                   :row minor
                   :col major
                   :width own))])))]

    (assoc layout
      :items positioned
      :bands bands
      :starts starts
      :rows (if vertical? total-major (+ total-minor lane-extra))
      :cols (if vertical? (+ total-minor lane-extra) total-major)
      :lane-base (if vertical? total-minor total-minor))))

;; Drawing

(def ^:private arrow-glyphs {:down \u25bc :up \u25b2 :right \u25b6 :left \u25c0})

(defn- centre-col [item] (+ (long (:col item)) (quot (dec (long (:width item))) 2)))

(defn- centre-row [item] (+ (long (:row item)) (quot (dec (long (:height item))) 2)))

(defn- free?
  [canvas row col ^long length]
  (and (inside? canvas row col)
       (inside? canvas row (+ (long col) length -1))
       (every?
         (fn [at]
           (and (= \space (aget ^chars (aget ^"[[C" (:chars canvas) row) (+ (long col) (long at))))
                (zero? (aget ^ints (aget ^"[[I" (:bits canvas) row) (+ (long col) (long at))))))
         (range length))))

(defn- draw-segment!
  [canvas layout segment]
  (let [{:keys [items vertical? flow]}
        layout

        source
        (items (:from segment))

        target
        (items (:to segment))

        style
        (:style segment)

        head?
        (:head? segment)

        label
        (:label segment)]

    (if vertical?
      (let [down?
            (= :down flow)

            step
            (if down? 1 -1)

            sx
            (centre-col source)

            tx
            (centre-col target)

            sy
            (if down? (+ (long (:row source)) (long (:height source))) (dec (long (:row source))))

            ty
            (if down? (dec (long (:row target))) (+ (long (:row target)) (long (:height target))))

            channel
            (- ty step)]

        (draw-vertical! canvas sy channel sx style)
        (when (not= sx tx) (draw-horizontal! canvas channel sx tx style))
        (draw-vertical! canvas channel ty tx style)
        (when head? (put-char! canvas ty tx (arrow-glyphs (if down? :down :up))))
        (when (:tail? segment) (put-char! canvas sy sx (arrow-glyphs (if down? :up :down))))
        (when (seq label)
          ;; The label leans toward the branch it belongs to, so a two-way split
          ;; reads `yes` over the left arm and `no` over the right one.
          (let [width
                (long (p/display-width label))

                left
                (- sx 2 width)

                right
                (+ sx 2)

                sides
                (if (< tx sx) [left right] [right left])

                spots
                (for [row
                      [sy (+ sy step)]

                      side
                      sides]

                  [row side])]

            (when-let [[row at] (first (filter (fn [[row at]]
                                                 (free? canvas row at width))
                                               spots))]
              (put-text! canvas row at label)))))
      (let [right?
            (= :right flow)

            step
            (if right? 1 -1)

            sy
            (centre-row source)

            ty
            (centre-row target)

            sx
            (if right? (+ (long (:col source)) (long (:width source))) (dec (long (:col source))))

            tx
            (if right? (dec (long (:col target))) (+ (long (:col target)) (long (:width target))))

            ;; A LABELLED edge turns right after the source, so the run that
            ;; approaches the target belongs to it alone and can carry the
            ;; label. An unlabelled edge turns late instead, which keeps a
            ;; fan-out drawn as one trunk.
            channel
            (if (seq label) (+ sx step) (- tx step))]

        (draw-horizontal! canvas sy sx channel style)
        (when (not= sy ty) (draw-vertical! canvas sy ty channel style))
        (draw-horizontal! canvas ty channel tx style)
        ;; The label rides the approach run, then the head is stamped LAST so a
        ;; long label can never swallow the arrow it points at.
        (when (seq label)
          (let [text
                (str " " label " ")

                width
                (long (p/display-width text))

                low
                (inc (min channel tx))

                high
                (dec (max channel tx))

                wanted
                (if right? (+ channel 2) (- channel 2 width))

                at
                (max low (min wanted (- high width -1)))]

            (put-text! canvas ty at text)))
        (when head? (put-char! canvas ty tx (arrow-glyphs (if right? :right :left))))
        (when (:tail? segment) (put-char! canvas sy sx (arrow-glyphs (if right? :left :right))))))))

(defn- draw-lane-edge!
  [canvas layout edge lane]
  (let [{:keys [items vertical? flow]}
        layout

        source
        (items (:from edge))

        target
        (items (:to edge))

        style
        (:style edge)

        label
        (:label edge)]

    (if vertical?
      (let [down?
            (= :down flow)

            sx
            (centre-col source)

            tx
            (centre-col target)

            sy
            (if down? (+ (long (:row source)) (long (:height source))) (dec (long (:row source))))

            ty
            (if down? (dec (long (:row target))) (+ (long (:row target)) (long (:height target))))

            ;; The lane rejoins the trunk one row BEFORE the arrowhead, so the
            ;; merge reads as a junction (├) and the arrow keeps its own cell.
            join
            (if down? (dec ty) (inc ty))]

        (draw-horizontal! canvas sy sx lane style)
        (put-line! canvas sy sx (if down? up-bit down-bit) style)
        (draw-vertical! canvas sy join lane style)
        (draw-horizontal! canvas join lane tx style)
        (draw-vertical! canvas join ty tx style)
        (when (:head? edge) (put-char! canvas ty tx (arrow-glyphs (if down? :down :up))))
        (when (seq label) (put-text! canvas sy (+ (long sx) 2) (str " " label " "))))
      (let [right?
            (= :right flow)

            sy
            (centre-row source)

            ty
            (centre-row target)

            sx
            (if right? (+ (long (:col source)) (long (:width source))) (dec (long (:col source))))

            tx
            (if right? (dec (long (:col target))) (+ (long (:col target)) (long (:width target))))

            ;; Same merge rule sideways: the lane meets the approach one column
            ;; before the arrowhead.
            join
            (if right? (dec tx) (inc tx))]

        (draw-vertical! canvas sy lane sx style)
        (put-line! canvas sy sx (if right? left-bit right-bit) style)
        (draw-horizontal! canvas lane sx join style)
        (draw-vertical! canvas lane ty join style)
        (draw-horizontal! canvas ty join tx style)
        (when (:head? edge) (put-char! canvas ty tx (arrow-glyphs (if right? :right :left))))
        (when (seq label) (put-text! canvas lane (+ (min sx tx) 2) (str " " label " ")))))))

(defn- draw-dummy!
  [canvas item vertical?]
  (if vertical?
    (draw-vertical! canvas
                    (long (:row item))
                    (+ (long (:row item)) (dec (long (:height item))))
                    (long (:col item))
                    (:style item))
    (draw-horizontal! canvas
                      (long (:row item))
                      (long (:col item))
                      (+ (long (:col item)) (dec (long (:width item))))
                      (:style item))))

(defn- canvas->rows
  [canvas]
  (let [{:keys [rows cols chars bits styles]} canvas]
    (->> (range rows)
         (mapv (fn [row]
                 (let [^chars char-row (aget ^"[[C" chars row)
                       ^ints bit-row (aget ^"[[I" bits row)
                       ^ints style-row (aget ^"[[I" styles row)]

                   (str/trimr (apply str
                                (for [col (range cols)]
                                  (let [ch (aget char-row col)
                                        drawn (aget bit-row col)]

                                    (if (and (= \space ch) (pos? drawn))
                                      (get (style-glyphs (code-style (aget style-row col) :solid))
                                           drawn
                                           \space)
                                      ch))))))))
         (drop-while str/blank?)
         (reverse)
         (drop-while str/blank?)
         (reverse)
         vec)))

(defn- render
  [layout]
  (let [{:keys [items segments lane-edges vertical? rows cols lane-base]}
        layout

        canvas
        (make-canvas rows cols)]

    (doseq [item (vals items)]
      (if (= :dummy (:kind item)) (draw-dummy! canvas item vertical?) (draw-box! canvas item)))
    ;; Lanes go down BEFORE the ranked segments: a segment label then sees the
    ;; cells a loop already took and leans to its free side instead of being
    ;; painted over by it.
    (doseq [[at edge] (map-indexed vector lane-edges)]
      (draw-lane-edge! canvas layout edge (+ (long lane-base) 1 (* 2 (long at)))))
    (doseq [segment segments]
      (draw-segment! canvas layout segment))
    (canvas->rows canvas)))

(def ^:private label-limits [28 20 14])

(def ^:private node-limit 64)

(defn diagram
  "Rows of `source` painted as a box-drawing flowchart inside `width` columns,
   or nil when this renderer does not own the fence or cannot fit it."
  [source width]
  (let [width (long (or width 0))]
    (when (pos? width)
      (when-let [graph (parse source)]
        (when (and (<= (count (:order graph)) (long node-limit))
                   (<= (count (:edges graph)) (* 4 (long node-limit))))
          (first (keep (fn [limit]
                         (let [layout (geometry (layered graph limit))]
                           (when (<= (long (:cols layout)) width)
                             (let [rows (render layout)]
                               (when (and (seq rows)
                                          (every? #(<= (long (p/display-width %)) width) rows))
                                 rows)))))
                       label-limits)))))))
