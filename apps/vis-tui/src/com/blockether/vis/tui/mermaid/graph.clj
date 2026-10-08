(ns com.blockether.vis.tui.mermaid.graph
  "Node-and-link Mermaid diagrams painted as box-drawing pictures: flowcharts and
   every diagram type that another namespace turns into the same graph shape.

   `fit-graph` is TOTAL: it answers `{:rows rows}`, or `{:reason text}` when the
   graph cannot fit the bubble.

   A `subgraph` is flattened: its nodes stay, its header and `end` go. A `~~~`
   link is invisible, so it is not drawn. A `style`, `classDef`, `class` or
   `:::class` colour tints a node with the nearest theme colour.

   Graph shape: `{:flow :order :nodes {id node} :edges [edge]}`. A node has
   `:id :label :shape`, and optionally `:sections` (label rows under the title,
   split by rules), `:tone` (border colour) and `:text-tone`. An edge has
   `:from :to :style :head? :tail? :label`.

   Pipeline: rank -> order -> place -> draw.

   Ranking is longest-path over the acyclic edges; an edge that closes a cycle
   (a retry loop, a self loop) is routed through a lane beside the diagram
   instead of bending the ranks. Ordering runs barycentre sweeps to cut
   crossings, placement pulls each node toward the median of its neighbours
   while keeping a minimum gap. Drawing merges line cells by DIRECTION BITS,
   so a crossing or a join picks the right joint glyph instead of overwriting."
  (:require [clojure.string :as str]
            [com.blockether.vis.tui.mermaid.canvas :as c]
            [com.blockether.vis.tui.primitives :as p]))

;; Parsing

(def ^:private ignored-re #"(?i)^(?:linkstyle|click|acctitle|accdescr|direction|title)\b")

(def ^:private subgraph-re #"(?i)^subgraph\b(.*)$")

(def ^:private end-re #"(?i)^end$")

(def ^:private class-suffix-re #"^:::([A-Za-z0-9_\-]+)")

(def ^:private shape-forms
  "Opening / closing delimiters longest first, so `([` wins over `(`."
  [["(((" ")))" :circle] ["([" "])" :stadium] ["[[" "]]" :subroutine] ["[(" ")]" :cylinder]
   ["((" "))" :circle] ["{{" "}}" :hexagon] ["[/" "/]" :rect] ["[\\" "\\]" :rect] ["[/" "\\]" :rect]
   ["[\\" "/]" :rect] ["[" "]" :rect] ["(" ")" :round] ["{" "}" :diamond] [">" "]" :flag]])

(def ^:private named-shapes
  "Shape names of the `@{ shape: ... }` form, mapped to a drawn outline."
  {"diamond" :diamond
   "diam" :diamond
   "decision" :diamond
   "question" :diamond
   "hex" :hexagon
   "hexagon" :hexagon
   "prepare" :hexagon
   "circle" :circle
   "circ" :circle
   "sm-circ" :circle
   "small-circle" :circle
   "start" :circle
   "dbl-circ" :circle
   "double-circle" :circle
   "fr-circ" :circle
   "stop" :circle
   "f-circ" :circle
   "junction" :circle
   "filled-circle" :circle
   "cross-circ" :circle
   "summary" :circle
   "stadium" :stadium
   "pill" :stadium
   "terminal" :stadium
   "rounded" :round
   "event" :round
   "cyl" :cylinder
   "cylinder" :cylinder
   "database" :cylinder
   "db" :cylinder
   "h-cyl" :cylinder
   "das" :cylinder
   "lin-cyl" :cylinder
   "disk" :cylinder
   "subproc" :subroutine
   "subprocess" :subroutine
   "subroutine" :subroutine
   "framed-rectangle" :subroutine
   "fr-rect" :subroutine
   "flag" :flag
   "odd" :flag
   "tool" :subroutine
   "refdoc" :round
   "input" :stadium
   "output" :stadium
   "action" :round})

(defn- strip-comment
  "Drop a `%%` comment, which also erases a `%%{init: ...}%%` directive line."
  [line]
  (if-let [at (str/index-of line "%%")]
    (subs line 0 at)
    line))

(defn- split-statements
  "Split one line at `;` outside quotes and brackets."
  [^String line]
  (loop [at
         0

         depth
         0

         quote?
         false

         start
         0

         out
         []]

    (if (>= at (.length line))
      (conj out (subs line start))
      (let [ch (.charAt line at)]
        (cond (= ch \") (recur (inc at) depth (not quote?) start out)
              quote? (recur (inc at) depth quote? start out)
              (#{\[ \( \{} ch) (recur (inc at) (inc depth) quote? start out)
              (#{\] \) \}} ch) (recur (inc at) (max 0 (dec depth)) quote? start out)
              (and (= ch \;) (zero? depth))
              (recur (inc at) depth quote? (inc at) (conj out (subs line start at)))
              :else (recur (inc at) depth quote? start out))))))

(defn- join-open-quotes
  "Join a line that opens a quote with the next lines until the quote closes,
   so a multi-line label stays in one statement."
  [lines]
  (loop [lines
         (seq lines)

         out
         []]

    (if-let [line (first lines)]
      (if (odd? (count (filter #{\"} line)))
        (let [[more after] (split-with #(even? (count (filter #{\"} %))) (rest lines))
              joined (str/join "\n" (concat [line] more (take 1 after)))]

          (recur (rest after) (conj out joined)))
        (recur (rest lines) (conj out line)))
      out)))

(defn statements
  "Trimmed, non-blank statements of `lines`: comments dropped, `;` splits."
  [lines]
  (->> (join-open-quotes lines)
       (map strip-comment)
       (mapcat split-statements)
       (map str/trim)
       (remove str/blank?)))

(defn- normalize-links
  "Rewrite mid-text links into the pipe form, so one tokenizer reads them all:
   `A -- yes --> B` becomes `A -->|yes| B`, `A -. no .-> B` becomes `A -.->|no| B`.
   An edge id (`A e1@--> B`) is dropped."
  [statement]
  (-> statement
      (str/replace #"\s[A-Za-z0-9_]+@(?=[-=.~<ox])" " ")
      (str/replace #"-\.\s+(\S.*?)\s+\.-([->ox]?)"
                   (fn [[_ text head]]
                     (str "-.-" head "|" text "|")))
      (str/replace #"(?:-{2,}|={2,})\s+(\S.*?)\s+(-{2,}|={2,})([->ox]?)(?=[^-=>]|$)"
                   (fn [[_ text link head]]
                     (str link head "|" text "|")))))

(def ^:private id-re #"^[\p{L}\p{N}_](?:[\p{L}\p{N}_.\-]*[\p{L}\p{N}_])?")

(defn- read-label
  "Label body between `open` and `close`, answering [label rest] or nil."
  [s open close]
  (let [body (subs s (count open))]
    (if (str/starts-with? (str/triml body) "\"")
      (let [body (str/triml body)]
        (when-let [end (str/index-of body "\"" 1)]
          (let [after (str/triml (subs body (inc (long end))))]
            (when (str/starts-with? after close) [(subs body 1 end) (subs after (count close))]))))
      (when-let [end (str/index-of body close)]
        [(subs body 0 end) (subs body (+ (long end) (count close)))]))))

(defn- read-meta
  "`@{ key: value, ... }` metadata after a node id, answering [props rest] or nil."
  [s]
  (when (str/starts-with? s "@{")
    (when-let [end (str/index-of s "}")]
      (let [body (subs s 2 end)
            props (into {}
                        (for [[_ k v] (re-seq #"([A-Za-z]+)\s*:\s*(\"[^\"]*\"|[^,]+)" body)]
                          [(str/lower-case k) (c/clean-label v)]))]

        [props (subs s (inc (long end)))]))))

(defn- read-class-suffix
  "[class-name rest] of a `:::className` suffix, or [nil s]."
  [s]
  (if-let [[whole class-name] (re-find class-suffix-re s)]
    [class-name (subs s (count whole))]
    [nil s]))

(defn- read-node
  [s]
  (when-let [id (re-find id-re s)]
    (let [tail (subs s (count id))
          shaped (some (fn [[open close shape]]
                         (when (str/starts-with? tail open)
                           (when-let [[label tail'] (read-label tail open close)]
                             [{:id id :label (c/clean-label label) :shape shape} tail'])))
                       shape-forms)
          [node tail] (cond (str/starts-with? tail "@{")
                            (when-let [[props tail'] (read-meta tail)]
                              [(cond-> {:id id}
                                 (props "shape")
                                 (assoc :shape (get named-shapes (props "shape") :rect))

                                 (props "label")
                                 (assoc :label (props "label"))) tail'])
                            shaped shaped
                            (some #(str/starts-with? tail (first %)) shape-forms) nil
                            :else [{:id id} tail])]

      (when node
        (let [[class-name tail] (read-class-suffix tail)]
          [(cond-> node
             class-name
             (assoc :class class-name)) tail])))))

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

(def ^:private link-re #"^(~{3,}|[-.=<>xo]{2,})(?:\|([^|]*)\|)?")

(defn- read-link
  [s]
  (when-let [[whole token label] (re-find link-re s)]
    (when (re-find #"[-.=~]" token)
      [(cond-> {:style (cond (str/includes? token ".") :dotted
                             (str/includes? token "=") :thick
                             :else :solid)
                :head? (boolean (re-find #"[>ox]$" token))
                :tail? (boolean (re-find #"^[<xo]" token))
                :head-kind ({\o :circle \x :cross} (last token) :arrow)
                :tail-kind ({\o :circle \x :cross} (first token) :arrow)
                :label (c/clean-label label)}
         (str/starts-with? token "~")
         (assoc :invisible? true)) (subs s (count whole))])))

(defn add-node
  "Add `node` to `graph`, or merge its label, shape and class into a known one."
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
          (assoc :label (:label node))

          (:class node)
          (assoc :class (:class node)))]

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
            ;; An invisible `~~~` link only spaces the mermaid layout: its
            ;; nodes stay, the link itself is not drawn.
            (recur (cond-> (reduce add-node graph nodes')
                     (not (:invisible? link))
                     (update :edges
                             into
                             (for [a prev
                                   b nodes']

                               (assoc link
                                 :from (:id a)
                                 :to (:id b)))))
                   nodes'
                   (str/triml tail''))))))))

(defn- group-id
  "Id that a `subgraph` header declares: `subgraph id` or `subgraph id [Title]`.
   A quoted title alone declares no id that a link can name."
  [header-rest]
  (let [header-rest (str/trim header-rest)]
    (when-not (str/starts-with? header-rest "\"") (re-find id-re header-rest))))

(defn- flatten-groups
  "Drop the subgraph ids: a link to a group would otherwise draw a box for it."
  [{:keys [groups] :as graph}]
  (-> graph
      (dissoc :groups)
      (update :order #(filterv (complement groups) %))
      (update :nodes #(apply dissoc % groups))
      (update :edges
              #(filterv (fn [{:keys [from to]}]
                          (not (or (contains? groups from) (contains? groups to))))
                 %))))

(defn cut-reason [s] (if (> (count s) 40) (str (subs s 0 39) "…") s))

(defn- read-style
  "Record a `style`, `classDef` or `class` statement in `graph`, or nil."
  [graph statement]
  (let [[_ word a b] (re-find #"(?i)^(style|classdef|class)\s+(\S+)\s*(.*)$" statement)]
    (case (some-> word
                  str/lower-case)
      "style"
      (update graph :styles assoc a b)

      "classdef"
      (reduce #(assoc-in %1 [:class-styles %2] b) graph (str/split a #","))

      "class"
      (reduce #(assoc-in %1 [:node-classes %2] (str/trim b)) graph (str/split a #","))

      nil)))

(defn apply-styles
  "Give each node the tones of its `style`, its classes and the `default` class."
  [{:keys [styles class-styles node-classes] :as graph}]
  (-> graph
      (update :nodes
              (fn [nodes]
                (into
                  {}
                  (for [[id node] nodes]
                    (let [class-names (remove nil? ["default" (:class node) (get node-classes id)])
                          props (apply merge
                                  (concat (map #(c/style-tones (get class-styles %)) class-names)
                                          [(c/style-tones (get styles id))]))]

                      [id
                       (cond-> node
                         (:border props)
                         (assoc :tone (:border props))

                         (:text props)
                         (assoc :text-tone (:text props)))])))))
      (dissoc :styles :class-styles :node-classes)))

(defn parse-flowchart
  "`{:graph graph}` of flowchart body `lines` drawn in `flow`, else `{:reason text}`."
  [lines flow]
  (let [graph
        (reduce
          (fn [graph statement]
            (if-let [[_ header-rest] (re-find subgraph-re statement)]
              (update graph
                      :groups
                      into
                      (some-> header-rest
                              group-id
                              vector))
              (cond (re-find ignored-re statement) graph
                    (re-find end-re statement) graph
                    ;; `e1@{ animate: true }` only animates a link.
                    (re-find #"^[A-Za-z0-9_]+@\{\s*(?:animate|animation|curve)\b" statement) graph
                    :else (or (read-style graph statement)
                              (parse-statement graph (normalize-links statement))
                              (reduced {:reason (str "cannot read: " (cut-reason statement))})))))
          {:order [] :nodes {} :edges [] :groups #{}}
          (statements lines))]
    (cond (:reason graph) graph
          :else (let [graph (apply-styles (flatten-groups graph))]
                  (if (seq (:order graph))
                    {:graph (assoc graph :flow flow)}
                    {:reason "empty flowchart"})))))

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

;; Drawing

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

        (c/put-line! canvas
                     row
                     col
                     (bit-or (long (if (or (not head?) (= from-row to-row))
                                     (if (pos? step) c/up-bit c/down-bit)
                                     0))
                             (long (if (not tail?) (if (pos? step) c/down-bit c/up-bit) 0)))
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

        (c/put-line! canvas
                     row
                     col
                     (bit-or (long (if (or (not head?) (= from-col to-col))
                                     (if (pos? step) c/left-bit c/right-bit)
                                     0))
                             (long (if (not tail?) (if (pos? step) c/right-bit c/left-bit) 0)))
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
   :hexagon {:tl \/ :tr \\ :bl \\ :br \/}
   :diamond {:tl \/ :tr \\ :bl \\ :br \/}
   :double {:tl \┌ :tr \┐ :bl \└ :br \┘}})

(defn- node-lines
  "Box rows of a node: its wrapped label, then each section after a `:rule`.
   A section row is a string or `[text tone]`; only the title is centred."
  [node ^long limit]
  (let [title
        (mapv (fn [line]
                {:text line :tone (:text-tone node) :centre? true})
              (c/label-lines (:label node) limit))

        sections
        (for [section
              (:sections node)

              :when (seq section)]

          (into [:rule]
                (for [row
                      section

                      :let [[text tone]
                            (if (string? row) [row nil] row)]
                      line
                      (c/label-lines text limit)]

                  {:text line :tone tone})))]

    (into title cat sections)))

(defn- draw-box!
  [canvas item]
  (let [{:keys [lines shape tone]}
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

        [horizontal vertical]
        (if (= :double shape) [\─ \│] [\─ \│])

        inner
        (- width 2)]

    (c/put-char! canvas row col tl tone)
    (c/put-char! canvas row (+ col (dec width)) tr tone)
    (c/put-char! canvas (+ row (dec height)) col bl tone)
    (c/put-char! canvas (+ row (dec height)) (+ col (dec width)) br tone)
    (dotimes [at inner]
      (c/put-char! canvas row (+ col 1 at) horizontal tone)
      (c/put-char! canvas (+ row (dec height)) (+ col 1 at) horizontal tone))
    (doseq [[at line] (map-indexed vector lines)]
      (let [line-row (+ row 1 (long at))]
        (dotimes [fill inner]
          (c/put-char! canvas line-row (+ col 1 fill) \space))
        (if (= :rule line)
          (do (c/put-char! canvas line-row col \├ tone)
              (c/put-char! canvas line-row (+ col (dec width)) \┤ tone)
              (dotimes [fill inner]
                (c/put-char! canvas line-row (+ col 1 fill) \─ tone)))
          (let [{:keys [text centre?]} line
                text (str text)
                pad (if centre? (quot (- inner (c/width text)) 2) 1)]

            (c/put-char! canvas line-row col vertical tone)
            (c/put-char! canvas line-row (+ col (dec width)) vertical tone)
            (c/put-text! canvas line-row (+ col 1 pad) text (:tone line))))))))

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
                      lines (node-lines node label-limit)
                      width
                      (+ 4 (long (apply max 1 (map #(c/width (:text %)) (remove #{:rule} lines)))))
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
                                  :tail? (and (:tail? edge) (= from (:from edge)))
                                  :head-kind (:head-kind edge)
                                  :tail-kind (:tail-kind edge)})
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

(def ^:private end-glyphs
  "End marker glyphs by marker kind and pointing direction."
  {:arrow {:down \u25bc :up \u25b2 :right \u25b6 :left \u25c0}
   :triangle {:down \u25bd :up \u25b3 :right \u25b7 :left \u25c1}
   :diamond {:down \u25c6 :up \u25c6 :right \u25c6 :left \u25c6}
   :hollow-diamond {:down \u25c7 :up \u25c7 :right \u25c7 :left \u25c7}
   :circle {:down \u25cb :up \u25cb :right \u25cb :left \u25cb}
   :cross {:down \u2715 :up \u2715 :right \u2715 :left \u2715}})

(defn- arrow-glyph [kind direction] (get-in end-glyphs [(or kind :arrow) direction]))

(defn- centre-col [item] (+ (long (:col item)) (quot (dec (long (:width item))) 2)))

(defn- free?
  [canvas row col ^long length]
  (and (c/inside? canvas row col)
       (c/inside? canvas row (+ (long col) length -1))
       (every?
         (fn [at]
           (and (= \space (aget ^chars (aget ^"[[C" (:chars canvas) row) (+ (long col) (long at))))
                (zero? (aget ^ints (aget ^"[[I" (:bits canvas) row) (+ (long col) (long at))))))
         (range length))))

(defn- label-free?
  "True when a label fits at `col` with a free cell, or the edge, on each side."
  [canvas row col ^long length]
  (and (free? canvas row col length)
       (every? #(or (not (c/inside? canvas row %)) (free? canvas row % 1))
               [(dec (long col)) (+ (long col) length)])))

(defn- centre-row [item] (+ (long (:row item)) (quot (dec (long (:height item))) 2)))

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
        (when head?
          (c/put-char! canvas ty tx (arrow-glyph (:head-kind segment) (if down? :down :up))))
        (when (:tail? segment)
          (c/put-char! canvas sy sx (arrow-glyph (:tail-kind segment) (if down? :up :down))))
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
                                                 (label-free? canvas row at width))
                                               spots))]
              (c/put-text! canvas row at label)))))
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

            (c/put-text! canvas ty at text)))
        (when head?
          (c/put-char! canvas ty tx (arrow-glyph (:head-kind segment) (if right? :right :left))))
        (when (:tail? segment)
          (c/put-char! canvas
                       sy
                       sx
                       (arrow-glyph (:tail-kind segment) (if right? :left :right))))))))

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
        (c/put-line! canvas sy sx (if down? c/up-bit c/down-bit) style)
        (draw-vertical! canvas sy join lane style)
        (draw-horizontal! canvas join lane tx style)
        (draw-vertical! canvas join ty tx style)
        (when (:head? edge)
          (c/put-char! canvas ty tx (arrow-glyph (:head-kind edge) (if down? :down :up))))
        (when (seq label) (c/put-text! canvas sy (+ (long sx) 2) (str " " label " "))))
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
        (c/put-line! canvas sy sx (if right? c/left-bit c/right-bit) style)
        (draw-horizontal! canvas lane sx join style)
        (draw-vertical! canvas lane ty join style)
        (draw-horizontal! canvas ty join tx style)
        (when (:head? edge)
          (c/put-char! canvas ty tx (arrow-glyph (:head-kind edge) (if right? :right :left))))
        (when (seq label) (c/put-text! canvas lane (+ (min sx tx) 2) (str " " label " ")))))))

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

(defn- render
  [layout]
  (let [{:keys [items segments lane-edges vertical? rows cols lane-base]}
        layout

        canvas
        (c/make-canvas rows cols)]

    (doseq [item (vals items)]
      (if (= :dummy (:kind item)) (draw-dummy! canvas item vertical?) (draw-box! canvas item)))
    ;; Lanes go down BEFORE the ranked segments: a segment label then sees the
    ;; cells a loop already took and leans to its free side instead of being
    ;; painted over by it.
    (doseq [[at edge] (map-indexed vector lane-edges)]
      (draw-lane-edge! canvas layout edge (+ (long lane-base) 1 (* 2 (long at)))))
    (doseq [segment segments]
      (draw-segment! canvas layout segment))
    (c/canvas->rows canvas)))

(def ^:private label-limits [28 20 14 10])

(def ^:private node-limit 64)

(defn- flows-to-try
  "The chart's own flow first. A sideways chart that does not fit may still fit
   top-down, which stacks its ranks instead of laying them side by side. A
   top-down chart with wide ranks may still fit sideways."
  [flow]
  (if (contains? #{:right :left} flow) [flow :down] [flow :right]))

(defn- fit
  "`{:rows rows}` for the first flow and label limit whose drawing fits `width`,
   else `{:reason text}` with the narrowest width that any attempt needed."
  [graph ^long width]
  (loop [attempts
         (for [flow
               (flows-to-try (:flow graph))

               limit
               label-limits]

           [flow limit])

         narrowest
         Long/MAX_VALUE]

    (if-let [[flow limit] (first attempts)]
      (let [layout (geometry (layered (assoc graph :flow flow) limit))
            cols (long (:cols layout))
            rows (when (<= cols width) (render layout))]

        (if (and (seq rows) (every? #(<= (c/row-width %) width) rows))
          {:rows rows}
          (recur (rest attempts) (min narrowest cols))))
      {:reason (str "too wide: " narrowest " > " width " cols")})))

(def ^:private link-glyphs {:solid "──" :dotted "··" :thick "──"})

(defn- stacked
  "Rows of `graph` as boxes stacked in rank order, each followed by its
   outgoing links. It is the picture for a graph too wide to lay out, or nil
   when even a box does not fit `width`."
  [graph ^long width]
  (when (>= width 12)
    (let [{:keys [nodes order edges]}
          graph

          limit
          (- width 4)

          label-of
          (fn [id]
            (str/replace (str (get-in nodes [id :label] id)) #"\s*\n\s*" " "))]

      {:rows
       (vec
         (mapcat
           (fn [id]
             (let [node
                   (nodes id)

                   lines
                   (node-lines node limit)

                   inner
                   (+ 2 (long (apply max 1 (map #(c/width (:text %)) (remove #{:rule} lines)))))

                   tone
                   (or (:tone node) :chrome)

                   bar
                   (apply str (repeat inner \─))

                   {:keys [tl tr bl br]}
                   (box-chrome (:shape node) (box-chrome :rect))

                   body
                   (for [line lines]
                     (if (= :rule line)
                       [["├" tone] [bar tone] ["┤" tone]]
                       (let [text (str (:text line))
                             pad (if (:centre? line) (quot (- inner (c/width text)) 2) 1)]

                         [["│" tone] [(apply str (repeat pad \space)) nil] [text (:tone line)]
                          [(apply str (repeat (- inner pad (c/width text)) \space)) nil]
                          ["│" tone]])))

                   links
                   (for [{:keys [from to style head? tail? label]}
                         edges

                         :when (= id from)]

                     (c/seg-clip
                       [["  " nil]
                        [(str (if tail? "←" "") (link-glyphs style "──") (if head? "▶" "─") " ")
                         (:tone (nodes to))] [(label-of to) (:text-tone (nodes to))]
                        [(if (seq label) (str "  " (str/replace label #"\s*\n\s*" " ")) "")
                         :chrome]]
                       width))]

               (map c/seg-row
                    (concat [[[(str tl) tone] [bar tone] [(str tr) tone]]]
                            body
                            [[[(str bl) tone] [bar tone] [(str br) tone]]]
                            links))))
           order))})))

(defn fit-graph
  "`{:rows rows}` of `graph` drawn inside `width` columns, else `{:reason text}`."
  [graph width]
  (let [width
        (long width)

        nodes
        (count (:order graph))

        edges
        (count (:edges graph))]

    (cond (zero? nodes) {:reason "no nodes"}
          (> nodes (long node-limit)) {:reason (str "too many nodes: " nodes " > " node-limit)}
          (> edges (* 4 (long node-limit))) {:reason (str "too many links: " edges
                                                          " > " (* 4 (long node-limit)))}
          :else (let [drawing (fit graph width)]
                  (if (:reason drawing) (or (stacked graph width) drawing) drawing)))))
