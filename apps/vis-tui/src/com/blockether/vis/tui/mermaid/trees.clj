(ns com.blockether.vis.tui.mermaid.trees
  "Mermaid diagrams that are outlines: mindmaps, tree views, treemaps, Ishikawa
   (fishbone) diagrams, timelines and kanban boards. Each one is drawn as an
   indented tree or as columns; each top-level branch takes the next palette
   colour, like the coloured branches of a Mermaid mindmap."
  (:require [clojure.string :as str]
            [com.blockether.vis.tui.mermaid.canvas :as c]))

(defn- indent-of
  ^long [^String line]
  (loop [at
         0

         cols
         0]

    (if (>= at (.length line))
      cols
      (case (.charAt line at)
        \space
        (recur (inc at) (inc cols))

        \tab
        (recur (inc at) (+ cols 4))

        cols))))

(defn- build
  "[children next-index] of the items after `at` that are deeper than `indent`."
  [items ^long at ^long indent]
  (loop [i
         at

         out
         []]

    (let [item (get items i)]
      (if (and item (> (long (:indent item)) indent))
        (let [[kids j] (build items (inc i) (:indent item))]
          (recur (long j) (conj out (assoc item :children kids))))
        [out i]))))

(defn- outline
  "Root nodes `{:text :indent :children}` of indented `lines`."
  [lines]
  (let [items (vec (for [line lines
                         :when (not (str/blank? line))]

                     {:text (str/trim line) :indent (indent-of line)}))]
    (first (build items 0 -1))))

(defn- join-markdown-strings
  "Join a line that opens a \"` markdown string with its next lines."
  [lines]
  (loop [lines
         (seq lines)

         out
         []]

    (if-let [line (first lines)]
      (if (odd? (count (re-seq #"`" line)))
        (let [[more after] (split-with #(even? (count (re-seq #"`" %))) (rest lines))]
          (recur (rest after)
                 (conj out (str/join " " (cons line (map str/trim (concat more (take 1 after))))))))
        (recur (rest lines) (conj out line)))
      out)))

;; Tree painting

(defn- branch-rows
  "Segment rows of `nodes` under a parent, each wrapped inside `width`.
   `label` answers `[[text tone] ...]` of a node, `tone-of` the branch tone."
  [nodes prefix width label tone-of depth]
  (let [n (count nodes)]
    (vec
      (mapcat
        (fn [at node]
          (let [last? (= at (dec n))
                tone (tone-of node at depth)
                joint (if last? "└─ " "├─ ")
                rest-prefix (str prefix (if last? "   " "│  "))
                room (max 4 (- (long width) (c/width prefix) 3))
                segments (label node tone)
                text (apply str (map first segments))
                lines (if (<= (c/width text) room)
                        [segments]
                        (map (fn [l]
                               [[l (second (first segments))]])
                             (c/wrap-words text room)))]

            (concat
              [(into [[prefix :chrome] [joint (if (zero? (long depth)) tone :chrome)]]
                     (first lines))]
              (for [l (rest lines)]
                (into [[rest-prefix :chrome]] l))
              (branch-rows (:children node) rest-prefix width label tone-of (inc (long depth))))))
        (range)
        nodes))))

(defn- tree-drawing
  "Rows of `roots`: each root on its own row, its children drawn as branches."
  [roots width label tone-of]
  {:rows (mapv c/seg-row
               (mapcat (fn [root]
                         (concat (map (fn [line]
                                        [[line :text]])
                                      (c/wrap-words (apply str (map first (label root :text)))
                                                    width))
                                 (branch-rows (:children root) "" width label tone-of 0)))
                       roots))})

(defn- tone-by-branch
  "Map node -> tone where each node keeps the palette tone of its top branch."
  [roots]
  (into {}
        (for [root
              roots

              [at branch]
              (map-indexed vector (:children root))

              node
              (tree-seq :children :children branch)]

          [node (c/series-tone at)])))

;; Mindmaps

(def ^:private mindmap-shapes
  [[#"^\(\((.*)\)\)$"] [#"^\)\)(.*)\(\($"] [#"^\)(.*)\($"] [#"^\{\{(.*)\}\}$"] [#"^\[(.*)\]$"]
   [#"^\((.*)\)$"]])

(defn- mindmap-text
  [text]
  (let [text
        (str/replace text #":::.*$" "")

        body
        (str/replace (str/trim text) #"^[^\s(\[{)]+(?=[(\[{)])" "")]

    (c/clean-label (or (some (fn [[re]]
                               (second (re-matches re (str/trim body))))
                             mindmap-shapes)
                       text))))

(defn mindmap
  [{:keys [lines]} width]
  (let [lines
        (->> (join-markdown-strings lines)
             (remove #(re-find #"^\s*::icon\(" %)))

        roots
        (outline lines)

        roots
        (mapv (fn walk [node]
                (-> node
                    (update :text mindmap-text)
                    (update :children #(mapv walk %))))
              roots)

        tones
        (tone-by-branch roots)]

    (if (empty? roots)
      {:reason "empty mindmap"}
      (tree-drawing roots
                    width
                    (fn [node tone]
                      [[(:text node) tone]])
                    (fn [node at depth]
                      (if (zero? (long depth)) (c/series-tone at) (get tones node :text)))))))

;; Tree views

(defn tree-view
  [{:keys [lines]} width]
  (let [roots
        (outline (remove #(re-find #"^\s*(?:title|accTitle|accDescr)\b" %) lines))

        label
        (fn [node _]
          (let [[_ name note]
                (re-matches #"(.*?)(?:\s+##\s*(.*))?" (:text node))

                name
                (c/clean-label name)

                dir?
                (or (str/ends-with? name "/") (seq (:children node)))]

            (cond-> [[name (if dir? :blue :text)]]
              (seq note)
              (conj [(str "  " note) :chrome]))))]

    (if (empty? roots)
      {:reason "empty tree"}
      {:rows (mapv c/seg-row (branch-rows roots "" width label (constantly :chrome) 1))})))

;; Treemaps

(defn- treemap-node
  [text]
  (let [[_ name value] (re-matches #"\"([^\"]*)\"\s*(?::\s*([-0-9.eE]+))?.*" text)]
    {:name (or name (c/clean-label text))
     :value (some-> value
                    parse-double)}))

(defn- total [node] (or (:value node) (reduce + 0.0 (map total (:children node)))))

(defn- number-text
  [value]
  (let [v (double value)]
    (if (== v (Math/rint v)) (str (long v)) (str (/ (Math/round (* v 100.0)) 100.0)))))

(defn treemap
  [{:keys [lines]} width]
  (let [roots
        (mapv (fn walk [node]
                (merge node (treemap-node (:text node)) {:children (mapv walk (:children node))}))
              (outline (remove #(re-find #"^\s*(?:classDef|class|title|accTitle|accDescr)\b" %)
                         lines)))

        bar-w
        (max 4 (min 20 (quot (long width) 4)))

        tones
        (into {}
              (for [[at root]
                    (map-indexed vector roots)

                    node
                    (tree-seq :children :children root)]

                [node (c/series-tone at)]))

        sum
        (reduce + 0.0 (map total roots))

        label
        (fn [node _]
          (let [value
                (total node)

                share
                (if (pos? sum) (/ value sum) 0.0)

                cells
                (long (Math/round (* share (double bar-w))))]

            [[(str (str/replace (:name node) #"\s*:::.*" "") " ") :text]
             [(apply str (repeat (max 1 cells) \█)) (tones node)]
             [(str " " (number-text value)) :chrome]]))]

    (if (empty? roots)
      {:reason "empty treemap"}
      {:rows (mapv c/seg-row
                   (branch-rows roots
                                ""
                                width
                                label
                                (fn [node _ _]
                                  (tones node))
                                0))})))

;; Ishikawa diagrams

(defn ishikawa
  [{:keys [lines]} width]
  (let [[effect & causes]
        (outline lines)

        effect
        (or effect {:text "Effect"})

        causes
        (vec (concat (:children effect) causes))

        width
        (long width)

        head
        (str " " (c/clean-label (:text effect)) " ")

        spine
        (max 2 (- width (c/width head) 1))]

    {:rows (mapv c/seg-row
                 (concat [[[(apply str (repeat spine \─)) :chrome] ["▶" :chrome] [head :red]]]
                         (branch-rows causes
                                      ""
                                      width
                                      (fn [node tone]
                                        [[(c/clean-label (:text node)) tone]])
                                      (fn [_ at depth]
                                        (if (zero? (long depth)) (c/series-tone at) :text))
                                      0)))}))

;; Timelines

(defn timeline
  [{:keys [lines]} width]
  (let [width
        (long width)

        entries
        (reduce
          (fn [entries line]
            (let [t (str/trim line)]
              (cond (or (str/blank? t) (re-find #"^(?:title|accTitle|accDescr)\b" t)) entries
                    (re-find #"^section\s" t) (conj entries {:section (c/clean-label (subs t 8))})
                    (str/starts-with? t ":")
                    (update-in entries
                               [(dec (count entries)) :events]
                               into
                               (map c/clean-label
                                    (remove str/blank? (str/split (subs t 1) #"\s:\s?|^:"))))
                    :else (let [[period & events] (str/split t #"\s*:\s*")]
                            (conj entries
                                  {:period (c/clean-label period)
                                   :events (mapv c/clean-label (remove str/blank? events))})))))
          []
          lines)

        title
        (some #(second (re-find #"^\s*title\s+(.*)$" %)) lines)

        periods
        (filter :period entries)

        period-w
        (min (quot width 3) (long (apply max 1 (map #(c/width (:period %)) periods))))

        event-w
        (max 6 (- width period-w 4))

        sections
        (atom -1)]

    (if (empty? periods)
      {:reason "empty timeline"}
      {:title title
       :rows (mapv c/seg-row
                   (drop-last
                     (mapcat
                       (fn [entry]
                         (if-let [section (:section entry)]
                           [[[(c/clip section width) (c/series-tone (swap! sections inc))]]]
                           (let [tone (c/series-tone (max 0 (long @sections)))
                                 period (c/label-lines (:period entry) period-w)
                                 events (mapcat #(map-indexed (fn [i l]
                                                                (str (if (zero? i) "• " "  ") l))
                                                              (c/wrap-words % (- event-w 2)))
                                                (:events entry))
                                 height (max (count period) (count events))]

                             (concat (for [r (range height)]
                                       [[(c/pad-right (get period r "") period-w) tone]
                                        [(if (zero? r) " ● " " │ ") (if (zero? r) tone :chrome)]
                                        [(nth events r "") :text]])
                                     [[[(apply str (repeat period-w \space)) :none]
                                       [" │" :chrome]]]))))
                       entries)))})))

;; Kanban boards

(defn- kanban-item
  [text]
  (let [[_ body meta]
        (re-matches #"(.*?)(?:@\{(.*)\})?\s*" text)

        props
        (into {}
              (for [[_ k v] (re-seq #"(\w+)\s*:\s*('[^']*'|\"[^\"]*\"|[^,}]+)" (or meta ""))]
                [(str/lower-case k) (str/trim (str/replace v #"^['\"]|['\"]$" ""))]))

        [_ label]
        (re-matches #"[^\[(]*[\[(](.*)[\])]\s*" body)]

    {:label (c/clean-label (or label body)) :props props}))

(def ^:private priority-tone {"very high" :red "high" :orange "low" :blue "very low" :green})

(defn- card-rows
  [item ^long w]
  (let [{:keys [label props]}
        (kanban-item (:text item))

        inner
        (- w 4)

        meta
        (str/join " · "
                  (remove str/blank? [(props "ticket") (props "assigned") (props "priority")]))

        tone
        (get priority-tone (str/lower-case (or (props "priority") "")) :chrome)

        bar
        (apply str (repeat (- w 2) \─))]

    (vec (concat [[["╭" tone] [bar tone] ["╮" tone]]]
                 (for [line (concat (map #(vector % :text) (c/label-lines label inner))
                                    (when (seq meta)
                                      (map #(vector % :chrome) (c/label-lines meta inner))))]
                   [["│ " tone] [(c/pad-right (first line) inner) (second line)] [" │" tone]])
                 [[["╰" tone] [bar tone] ["╯" tone]]]))))

(defn kanban
  [{:keys [lines]} width]
  (let [width
        (long width)

        columns
        (outline (remove #(re-find #"^\s*(?:title|accTitle|accDescr)\b" %) lines))

        n
        (count columns)

        col-w
        (if (pos? n) (quot (- width (dec n)) n) 0)]

    (cond (zero? n) {:reason "empty board"}
          (>= col-w 16)
          (let [cols
                (map-indexed
                  (fn [at column]
                    (let [tone (c/series-tone at)]
                      (into [[[(c/pad-right (c/clip (:label (kanban-item (:text column))) col-w)
                                            col-w) tone]] [[(apply str (repeat col-w \─)) tone]]]
                            (mapcat #(card-rows % col-w) (:children column)))))
                  columns)

                height
                (apply max (map count cols))]

            {:rows (mapv (fn [r]
                           (c/seg-row (vec (butlast (mapcat (fn [col]
                                                              (conj (get col
                                                                         r
                                                                         [[(apply str
                                                                             (repeat col-w \space))
                                                                           :none]])
                                                                    [" " :none]))
                                                            cols)))))
                         (range height))})
          :else
          (let [card-w
                (min width
                     48
                     (+ 4
                        (long (apply max
                                8
                                (for [column columns
                                      item (:children column)
                                      :let [{:keys [label props]} (kanban-item (:text item))]]

                                  (max (c/width label)
                                       (c/width (str/join " · "
                                                          (vals (select-keys props
                                                                             ["ticket" "assigned"
                                                                              "priority"]))))))))))]
            {:rows (mapv c/seg-row
                         (apply concat
                           (map-indexed (fn [at column]
                                          (let [tone (c/series-tone at)]
                                            (into [[[(c/clip (:label (kanban-item (:text column)))
                                                             width) tone]]]
                                                  (mapcat #(card-rows % card-w)
                                                          (:children column)))))
                                        columns)))}))))
