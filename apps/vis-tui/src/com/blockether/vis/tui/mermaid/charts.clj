(ns com.blockether.vis.tui.mermaid.charts
  "Mermaid charts painted with terminal glyphs: bars for pie, xy, radar,
   sankey and journey charts, a time grid for gantt charts, point plots for
   quadrant and Wardley charts, a bit grid for packets, and coloured tables for
   Venn, Cynefin and event-modeling diagrams. Series take the palette tones,
   so a chart follows the TUI theme."
  (:require [clojure.string :as str]
            [com.blockether.vis.tui.mermaid.canvas :as c])
  (:import (java.time LocalDate LocalDateTime LocalTime ZoneOffset)
           (java.time.format DateTimeFormatter)))

(defn- body-lines
  "Trimmed, non-blank lines without comments and accessibility statements."
  [lines]
  (->> lines
       (map #(str/trim (str/replace % #"%%.*$" "")))
       (remove str/blank?)
       (remove #(re-find #"^(?:accTitle|accDescr)\b" %))))

(defn- title-of
  [lines]
  (some #(some-> (re-find #"^title\s+(.*)$" %)
                 second
                 c/clean-label)
        lines))

(defn- number
  [s]
  (some-> (re-find #"-?\d+(?:\.\d+)?(?:[eE]-?\d+)?" (str s))
          parse-double))

(defn- number-text
  [value]
  (let [v (double value)]
    (cond (== v (Math/rint v)) (str (long v))
          (< (Math/abs v) 1.0) (format "%.3f" v)
          :else (format "%.2f" v))))

(defn- bar
  "Bar of `value` on a scale where `top` fills `cells` columns, with a
   one-eighth-step end."
  [value top ^long cells]
  (let [eighths
        (if (pos? (double top))
          (long (Math/round (* 8.0 cells (/ (max 0.0 (double value)) (double top)))))
          0)

        full
        (quot eighths 8)

        part
        (rem eighths 8)]

    (str (apply str (repeat full \█)) (if (pos? part) (nth "▏▎▍▌▋▊▉" (dec part)) ""))))

(defn- bar-rows
  "Rows of labelled bars: `items` are `{:label :value :tone :note}`."
  [items ^long width]
  (let [label-w
        (min (quot width 3) (long (apply max 1 (map #(c/width (:label %)) items))))

        value-w
        (long (apply max 1 (map #(c/width (str (:note %))) items)))

        cells
        (max 4 (- width label-w value-w 3))

        top
        (apply max 0.0 (map #(double (:value %)) items))]

    (for [{:keys [label value tone note]} items]
      [[(c/pad-right (c/clip label label-w) label-w) :text] [" " :none] [(bar value top cells) tone]
       [(str " " note) :chrome]])))

;; Pie charts

(defn pie
  [{:keys [args lines]} width]
  (let [lines
        (body-lines lines)

        show?
        (or (str/includes? (str args) "showData") (some #(= "showData" %) lines))

        slices
        (for [line
              lines

              :let [[_ label value]
                    (re-matches #"\"([^\"]*)\"\s*:\s*(\S+)" line)]
              :when label]

          [(c/clean-label label) (or (number value) 0.0)])

        sum
        (reduce + 0.0 (map second slices))]

    (if (empty? slices)
      {:reason "empty pie chart"}
      {:title (title-of lines)
       :rows (mapv c/seg-row
                   (bar-rows (map-indexed
                               (fn [at [label value]]
                                 {:label label
                                  :value value
                                  :tone (c/series-tone at)
                                  :note (str (format "%.1f%%"
                                                     (* 100.0 (/ (double value) (max sum 1e-9))))
                                             (if show? (str " (" (number-text value) ")") ""))})
                               slices)
                             width))})))

;; Gantt charts

(defn- java-pattern
  [moment]
  (-> moment
      (str/replace "YYYY" "yyyy")
      (str/replace "YY" "yy")
      (str/replace "DD" "dd")
      (str/replace "Do" "d")))

(defn- parse-instant
  "Epoch milliseconds of `text` in Mermaid `date-format`, or nil."
  [text date-format]
  (let [text (str/trim text)]
    (try (cond (= "X" date-format) (* 1000.0 (double (parse-double text)))
               (= "x" date-format) (double (parse-double text))
               :else (let [formatter (DateTimeFormatter/ofPattern (java-pattern date-format))
                           parsed (.parse formatter text)
                           date? (.isSupported parsed java.time.temporal.ChronoField/YEAR)
                           time? (.isSupported parsed java.time.temporal.ChronoField/HOUR_OF_DAY)]

                       (double (cond (and date? time?) (.toEpochMilli (.toInstant
                                                                        (LocalDateTime/from parsed)
                                                                        ZoneOffset/UTC))
                                     date? (.toEpochMilli (.toInstant (.atStartOfDay (LocalDate/from
                                                                                       parsed))
                                                                      ZoneOffset/UTC))
                                     time? (* 1000.0 (.toSecondOfDay (LocalTime/from parsed)))
                                     :else (throw (ex-info "no date" {}))))))
         (catch Exception _
           (try (double (.toEpochMilli (.toInstant (.atStartOfDay (LocalDate/parse text))
                                                   ZoneOffset/UTC)))
                (catch Exception _ nil))))))

(def ^:private unit-ms
  {"ms" 1.0
   "s" 1000.0
   "m" 60000.0
   "h" 3600000.0
   "d" 86400000.0
   "w" 604800000.0
   "M" 2.592e9
   "y" 3.1536e10})

(defn- duration-ms
  [text]
  (when-let [[_ n unit] (re-matches #"(\d+(?:\.\d+)?)\s*(ms|s|m|h|d|w|M|y)" (str/trim text))]
    (* (double (parse-double n)) (double (unit-ms unit)))))

(def ^:private task-tags #{"done" "active" "crit" "milestone" "vert"})

(defn- gantt-tasks
  [lines date-format]
  (loop [lines
         lines

         section
         nil

         tasks
         []

         by-id
         {}

         cursor
         nil]

    (if-let [line (first lines)]
      (cond
        (re-find #"^section\s" line)
        (recur (rest lines) (c/clean-label (subs line 8)) tasks by-id cursor)
        (re-find
          #"^(?:title|dateFormat|axisFormat|excludes|includes|tickInterval|todayMarker|weekday|weekend|inclusiveEndDates|topAxis|displayMode|click)\b"
          line)
        (recur (rest lines) section tasks by-id cursor)
        :else
        (let [[_ label spec] (re-matches #"([^:]+?)\s*:\s*(.*)" line)]
          (if-not label
            (recur (rest lines) section tasks by-id cursor)
            (let [parts (map str/trim (str/split spec #","))
                  tags (set (filter task-tags parts))
                  parts (vec (remove task-tags parts))
                  timing? #(or (re-find #"^(?:after|until)\s" %)
                               (duration-ms %)
                               (parse-instant % date-format))
                  [id parts] (if (and (> (count parts) 1) (not (timing? (first parts))))
                               [(first parts) (rest parts)]
                               [nil parts])
                  [start-spec end-spec] (if (= 1 (count parts)) [nil (first parts)] parts)
                  after (fn [spec]
                          (when-let [[_ ids] (re-find #"^after\s+(.*)$" (str spec))]
                            (some->> (str/split ids #"\s+")
                                     (keep #(:end (by-id %)))
                                     seq
                                     (apply max))))
                  start (or (some-> start-spec
                                    after)
                            (some-> start-spec
                                    (parse-instant date-format))
                            cursor
                            0.0)
                  end (or (some-> end-spec
                                  duration-ms
                                  (+ start))
                          (when-let [[_ ids] (re-find #"^until\s+(.*)$" (str end-spec))]
                            (some->> (str/split ids #"\s+")
                                     (keep #(:start (by-id %)))
                                     seq
                                     (apply min)))
                          (some-> end-spec
                                  (parse-instant date-format))
                          (some-> end-spec
                                  after)
                          (+ start 86400000.0))
                  task {:label (c/clean-label label)
                        :section section
                        :tags tags
                        :start (double start)
                        :end (max (double start) (double end))}]

              (recur (rest lines)
                     section
                     (conj tasks task)
                     (cond-> by-id
                       id
                       (assoc id task))
                     (:end task))))))
      tasks)))

(defn- day-text
  [ms]
  (str (.toLocalDate
         (java.time.LocalDateTime/ofEpochSecond (long (/ (double ms) 1000.0)) 0 ZoneOffset/UTC))))

(defn gantt
  [{:keys [lines]} width]
  (let [lines
        (body-lines lines)

        date-format
        (or (some #(second (re-find #"^dateFormat\s+(.*)$" %)) lines) "YYYY-MM-DD")

        tasks
        (gantt-tasks lines date-format)

        width
        (long width)]

    (if (empty? tasks)
      {:reason "empty gantt chart"}
      (let [lo
            (apply min (map :start tasks))

            hi
            (apply max (map :end tasks))

            span
            (max 1.0 (- (double hi) (double lo)))

            label-w
            (min (quot width 3) (long (apply max 1 (map #(c/width (:label %)) tasks))))

            cells
            (max 8 (- width label-w 3))

            col
            (fn [t]
              (long (Math/floor (* (dec cells) (/ (- (double t) (double lo)) span)))))

            sections
            (vec (distinct (map :section tasks)))

            day?
            (> (double lo) 3.0e11)]

        {:title (title-of lines)
         :rows
         (mapv
           c/seg-row
           (concat
             (when day?
               [[[(apply str (repeat (+ label-w 1) \space)) :none] ["│" :chrome]
                 [(day-text lo) :chrome]
                 [(c/pad-left (day-text hi) (max 0 (- cells 1 (c/width (day-text lo))))) :chrome]]])
             (mapcat
               (fn [tasks]
                 (let [section
                       (:section (first tasks))

                       tone
                       (c/series-tone (.indexOf ^java.util.List sections section))]

                   (concat (when section [[[(c/clip section width) tone]]])
                           (for [{:keys [label tags start end]}
                                 tasks

                                 :let [a
                                       (col start)

                                       b
                                       (max (inc a) (col end))

                                       mile?
                                       (contains? tags "milestone")

                                       tone
                                       (cond (contains? tags "crit") :red
                                             (contains? tags "active") :blue
                                             (contains? tags "done") :chrome
                                             :else tone)]]

                             [[(c/pad-right (c/clip label label-w) label-w) :text] [" " :none]
                              ["│" :chrome] [(apply str (repeat a \space)) :none]
                              [(if mile?
                                 "◆"
                                 (apply str (repeat (- b a) (if (contains? tags "done") \▒ \█))))
                               (if mile? :yellow tone)]]))))
               (partition-by :section tasks))))}))))

;; XY charts

(defn- bracket-list
  [s]
  (when-let [[_ body] (re-find #"\[(.*)\]" (str s))]
    (mapv #(c/clean-label (str/trim %)) (str/split body #","))))

(defn xy-chart
  [{:keys [lines]} width]
  (let [lines
        (body-lines lines)

        x-line
        (some #(when (str/starts-with? % "x-axis") %) lines)

        categories
        (or (bracket-list x-line) [])

        series
        (for [line
              lines

              :let [[_ kind name values]
                    (re-matches #"(bar|line)\s*(?:\"([^\"]*)\")?\s*(\[.*\])" line)]
              :when kind]

          {:kind kind :name name :values (mapv #(or (number %) 0.0) (bracket-list values))})

        n
        (long (apply max (count categories) (map #(count (:values %)) series)))

        categories
        (vec (concat categories (map #(str (inc (long %))) (range (count categories) n))))]

    (if (empty? series)
      {:reason "empty xy chart"}
      (let [top
            (apply max 0.0 (mapcat :values series))

            width
            (long width)

            label-w
            (min (quot width 3) (long (apply max 1 (map c/width categories))))

            value-w
            (long (apply max 1 (map #(c/width (number-text %)) (mapcat :values series))))

            cells
            (max 4 (- width label-w value-w 3))

            many?
            (> (count series) 1)]

        {:title (title-of lines)
         :rows
         (mapv
           c/seg-row
           (concat
             (when many?
               [(vec (mapcat (fn [at {:keys [kind name]}]
                               [[(if (= "bar" kind) "█ " "● ") (c/series-tone at)]
                                [(str (or name (str kind " " (inc (long at)))) "  ") :chrome]])
                             (range)
                             series))])
             (mapcat (fn [at category]
                       (for [[s-at {:keys [kind values]}]
                             (map-indexed vector series)

                             :let [value
                                   (get values at 0.0)

                                   tone
                                   (c/series-tone s-at)

                                   label
                                   (if (zero? (long s-at))
                                     (c/pad-right (c/clip category label-w) label-w)
                                     (apply str (repeat label-w \space)))]]

                         (if (= "bar" kind)
                           [[label :text] [" " :none] [(bar value top cells) tone]
                            [(str " " (number-text value)) :chrome]]
                           (let [pos (long (Math/round (* (dec cells)
                                                          (/ (double value) (max top 1e-9)))))]
                             [[label :text] [" " :none]
                              [(apply str (repeat (max 0 pos) \·)) :chrome] ["●" tone]
                              [(str " " (number-text value)) :chrome]]))))
                     (range n)
                     categories)))}))))

;; Point plots

(defn- plot-rows
  "Rows of a point plot inside `width` columns. `points` are `{:x :y :label
   :tone :mark}` with x and y in [0, 1]; `links` are `[from-label to-label]`."
  [{:keys [points links x-labels y-label quadrants]} ^long width ^long height]
  (let [cols
        (- width 2)

        canvas
        (c/make-canvas (+ height 2) width)

        at
        (fn [{:keys [x y]}]
          [(- height 1 (long (Math/round (* (dec height) (max 0.0 (min 1.0 (double y)))))))
           (+ 2 (long (Math/round (* (- cols 3) (max 0.0 (min 1.0 (double x)))))))])

        by-label
        (into {} (map (juxt :label identity) points))]

    (dotimes [r height]
      (c/put-char! canvas r 0 \│ :chrome))
    (c/put-char! canvas height 0 \└ :chrome)
    (dotimes [i (dec width)]
      (c/put-char! canvas height (inc i) \─ :chrome))
    (when quadrants
      (let [mid-r
            (quot height 2)

            mid-c
            (+ 1 (quot cols 2))

            [q1 q2 q3 q4]
            quadrants]

        (dotimes [r height]
          (c/put-char! canvas r mid-c \┆ :chrome))
        (dotimes [i cols]
          (c/put-char! canvas mid-r (+ 1 i) \┄ :chrome))
        (doseq [[text r col tone]
                [[q2 0 2 :blue] [q1 0 (+ mid-c 2) :green] [q3 (dec height) 2 :orange]
                 [q4 (dec height) (+ mid-c 2) :purple]]

                :when (seq text)]

          (c/put-text! canvas r col (c/clip text (- (quot cols 2) 3)) tone))))
    (doseq [[from to]
            links

            :let [a
                  (by-label from)

                  b
                  (by-label to)]
            :when (and a b)]

      (let [[r1 c1]
            (at a)

            [r2 c2]
            (at b)

            steps
            (max 1
                 (Math/abs (long (- (long r2) (long r1))))
                 (Math/abs (long (- (long c2) (long c1)))))]

        (doseq [i
                (range 1 steps)

                :let [r
                      (Math/round (+ (double r1) (* (/ (double i) steps) (- (long r2) (long r1)))))

                      col
                      (Math/round (+ (double c1) (* (/ (double i) steps) (- (long c2) (long c1)))))]
                :when (= \space (c/char-at canvas r col))]

          (c/put-char! canvas r col \· :chrome))))
    (doseq [p
            points

            :let [[r col]
                  (at p)

                  text
                  (str " " (:label p))

                  room
                  (- width (long col) 1)

                  text
                  (c/clip text room)

                  text-col
                  (if (and (< (c/width text) (c/width (str " " (:label p))))
                           (> (long col) (quot width 2)))
                    (- (long col) (c/width (str (:label p) " ")))
                    (inc (long col)))

                  text
                  (if (< (long text-col) (long col)) (str (:label p) " ") text)]]

      (c/put-char! canvas r col (or (:mark p) \●) (:tone p))
      (c/put-text! canvas r (max 1 (long text-col)) text :text))
    (when (seq x-labels)
      (let [n (count x-labels)]
        (doseq [[i label] (map-indexed vector x-labels)
                :let [col (if (= 1 n)
                            2
                            (+ 2
                               (long (Math/round (* (- cols 3 (c/width label))
                                                    (/ (double i) (dec n)))))))]]

          (c/put-text! canvas (inc height) col label :chrome))))
    (when y-label (c/put-text! canvas 0 2 "" :chrome))
    (c/canvas->rows canvas)))

(defn- pair
  [s]
  (when-let [[_ a b] (re-find #"\[\s*(-?[\d.]+)\s*,\s*(-?[\d.]+)\s*\]" (str s))]
    [(parse-double a) (parse-double b)]))

(defn quadrant-chart
  [{:keys [lines]} width]
  (let [lines
        (body-lines lines)

        axis
        (fn [k]
          (some #(when-let [[_ a b]
                            (re-find (re-pattern (str "^" k "\\s+(.+?)(?:\\s*-->\\s*(.+))?$")) %)]
                   [(c/clean-label a)
                    (some-> b
                            c/clean-label)])
                lines))

        quadrant
        (fn [n]
          (some #(some-> (re-find (re-pattern (str "^quadrant-" n "\\s+(.*)$")) %)
                         second
                         c/clean-label)
                lines))

        styles
        (into {}
              (for [line
                    lines

                    :let [[_ name style]
                          (re-find #"^classDef\s+(\S+)\s+(.*)$" line)]
                    :when name]

                [name style]))

        points
        (for [line
              lines

              :let [[_ label class-name rest]
                    (re-matches #"(.+?)(?::::(\S+))?\s*:\s*(\[.*\].*)" line)

                    [x y]
                    (pair rest)

                    colour
                    (or (second (re-find #"color:\s*(#?\w+)" (str rest)))
                        (second (re-find #"color:\s*(#?\w+)" (str (styles class-name)))))]
              :when x]

          {:x x :y y :label (c/clean-label label) :tone (or (c/colour-tone colour) :cyan)})

        [x-lo x-hi]
        (axis "x-axis")

        [y-lo y-hi]
        (axis "y-axis")

        width
        (long width)]

    (if (and (empty? points) (not-any? #(re-find #"^(?:x-axis|y-axis|quadrant-\d)" %) lines))
      {:reason "empty quadrant chart"}
      {:title (title-of lines)
       :rows (vec (concat (when y-hi [(c/seg-row [[(str "▲ " y-hi) :chrome]])])
                          (plot-rows {:points points
                                      :quadrants [(quadrant 1) (quadrant 2) (quadrant 3)
                                                  (quadrant 4)]
                                      :x-labels (remove nil? [x-lo x-hi])}
                                     width
                                     (max 10 (min 20 (quot width 3))))
                          (when y-lo [(c/seg-row [[(str "▼ " y-lo) :chrome]])])))})))

(defn- wardley-name [s] (c/clean-label (str/trim (str/replace (str s) #"\s*\[.*$|\s*\(.*\)$" ""))))

(defn wardley
  [{:keys [lines]} width]
  (let [lines
        (body-lines lines)

        points
        (for [line
              lines

              :let [[_ kind name coords]
                    (re-matches #"(component|anchor)\s+(.+?)\s*(\[.*\].*)" line)

                    [visibility evolution]
                    (pair coords)]
              :when (and kind visibility)]

          {:label (wardley-name name)
           :x evolution
           :y visibility
           :tone (if (= "anchor" kind) :yellow :cyan)})

        evolved
        (for [line
              lines

              :let [[_ name to]
                    (re-matches #"evolve\s+(.+?)\s+([\d.]+)" line)

                    from
                    (some #(when (= (wardley-name name) (:label %)) %) points)]
              :when from]

          {:label (str (:label from) "'") :x (parse-double to) :y (:y from) :tone :green :mark \○})

        links
        (for [line
              lines

              :let [[_ a b]
                    (re-matches #"(.+?)\s*-+>\s*(.+?)(?:\s*;.*)?" line)]
              :when (and a (not (re-find #"^(?:evolution|evolve)\b" line)))]

          [(wardley-name a) (wardley-name b)])

        stages
        (or (some #(when-let [[_ s] (re-find #"^evolution\s+(.*)$" %)] (mapv (fn [x]
                                                                               (str/trim
                                                                                 (str/replace
                                                                                   x
                                                                                   #"@[\d.]+"
                                                                                   "")))
                                                                             (str/split s #"->")))
                  lines)
            ["Genesis" "Custom" "Product" "Commodity"])

        width
        (long width)]

    (if (and (empty? points) (empty? lines))
      {:reason "empty Wardley map"}
      {:title (title-of lines)
       :rows (vec (concat [(c/seg-row [["▲ visible" :chrome]])]
                          (plot-rows
                            {:points (concat points evolved)
                             :links (concat links
                                            (map (fn [e]
                                                   [(subs (:label e) 0 (dec (count (:label e))))
                                                    (:label e)])
                                                 evolved))
                             :x-labels (mapv #(c/clip % (max 4 (quot width (* 2 (count stages)))))
                                             stages)}
                            width
                            (max 10 (min 22 (* 2 (count points)))))
                          [(c/seg-row [["▼ invisible" :chrome] ["   evolution ▶" :chrome]])]))})))

;; Radar charts

(defn radar
  [{:keys [lines]} width]
  (let [lines
        (body-lines lines)

        axes
        (vec (for [line
                   lines

                   :when (str/starts-with? line "axis")
                   part
                   (str/split (subs line 4) #",")

                   :let [[_ id label]
                         (re-find #"^\s*([\w\-]+)(?:\[\"?([^\]\"]*)\"?\])?" part)]
                   :when id]

               {:id id :label (or label id)}))

        curves
        (for [line
              lines

              :let [[_ id label body]
                    (re-find #"^curve\s+([\w\-]+)(?:\[\"?([^\]\"]*)\"?\])?\s*\{(.*)\}" line)]
              :when id]

          {:label (or label id)
           :values (if (str/includes? body ":")
                     (into {}
                           (for [[_ k v] (re-seq #"([\w\-]+)\s*:\s*(-?[\d.]+)" body)]
                             [k (parse-double v)]))
                     (zipmap (map :id axes) (map #(or (number %) 0.0) (str/split body #","))))})

        top
        (or (some #(some-> (re-find #"^max\s+(\S+)" %)
                           second
                           number)
                  lines)
            (apply max 1.0 (mapcat (comp vals :values) curves)))

        width
        (long width)]

    (if (or (empty? axes) (empty? curves))
      {:reason "empty radar chart"}
      (let [label-w
            (min (quot width 3) (long (apply max 1 (map #(c/width (:label %)) axes))))

            cells
            (max 4 (- width label-w 8))]

        {:title (title-of lines)
         :rows (mapv c/seg-row
                     (concat [(c/seg-clip (vec (mapcat (fn [at {:keys [label]}]
                                                         [["█ " (c/series-tone at)]
                                                          [(str label "  ") :chrome]])
                                                       (range)
                                                       curves))
                                          width)]
                             (mapcat (fn [{:keys [id label]}]
                                       (for [[at curve]
                                             (map-indexed vector curves)

                                             :let [value
                                                   (get (:values curve) id 0.0)]]

                                         [[(if (zero? (long at))
                                             (c/pad-right (c/clip label label-w) label-w)
                                             (apply str (repeat label-w \space))) :text] [" " :none]
                                          [(bar value top cells) (c/series-tone at)]
                                          [(str " " (number-text value)) :chrome]]))
                                     axes)))}))))

;; Sankey diagrams

(defn- csv-fields
  [^String line]
  (mapv #(str/replace (str/trim %) #"^\"|\"$" "") (re-seq #"\"(?:[^\"]|\"\")*\"|[^,]+" line)))

(defn sankey
  [{:keys [lines]} width]
  (let [flows
        (for [line
              (body-lines lines)

              :let [[source target value]
                    (csv-fields line)]
              :when (and target (number value))]

          {:source source :target target :value (number value)})

        sources
        (vec (distinct (map :source flows)))

        top
        (apply max 0.0 (map :value flows))

        width
        (long width)]

    (if (empty? flows)
      {:reason "empty sankey diagram"}
      (let [target-w
            (min (quot width 3) (long (apply max 1 (map #(c/width (:target %)) flows))))

            cells
            (max 4 (- width target-w 16))]

        {:rows
         (mapv c/seg-row
               (mapcat
                 (fn [[at source]]
                   (let [tone
                         (c/series-tone at)

                         outs
                         (filter #(= source (:source %)) flows)]

                     (cons [[(c/clip
                               (str source " (" (number-text (reduce + 0.0 (map :value outs))) ")")
                               width) tone]]
                           (map-indexed (fn [i {:keys [target value]}]
                                          [[(if (= i (dec (count outs))) " └▶ " " ├▶ ") :chrome]
                                           [(c/pad-right (c/clip target target-w) target-w) :text]
                                           [" " :none] [(bar value top cells) tone]
                                           [(str " " (number-text value)) :chrome]])
                                        outs))))
                 (map-indexed vector sources)))}))))

;; User journeys

(def ^:private score-tone {1 :red 2 :orange 3 :yellow 4 :green 5 :green})

(defn journey
  [{:keys [lines]} width]
  (let [lines
        (body-lines lines)

        width
        (long width)

        tasks
        (for [line
              lines

              :when (not (re-find #"^(?:title|section)\b" line))
              :let [[_ task score actors]
                    (re-matches #"(.+?)\s*:\s*(\d+)\s*(?::\s*(.*))?" line)]
              :when task]

          {:task (c/clean-label task) :score (parse-long score) :actors (str/trim (or actors ""))})

        label-w
        (min (quot width 2) (long (apply max 1 (map #(c/width (:task %)) tasks))))

        sections
        (atom -1)]

    (if (empty? tasks)
      {:reason "empty journey"}
      {:title (title-of lines)
       :rows (mapv c/seg-row
                   (for [line
                         lines

                         :when (not (re-find #"^title\b" line))
                         :let [[_ section]
                               (re-find #"^section\s+(.*)$" line)

                               [_ task score actors]
                               (re-matches #"(.+?)\s*:\s*(\d+)\s*(?::\s*(.*))?" line)]
                         :when (or section task)]

                     (if section
                       [[(c/clip (c/clean-label section) width)
                         (c/series-tone (swap! sections inc))]]
                       (let [score (min 5 (max 0 (long (parse-long score))))]
                         (c/seg-clip [["  " :none]
                                      [(c/pad-right (c/clip (c/clean-label task) label-w) label-w)
                                       :text] [" " :none]
                                      [(apply str (repeat score \■)) (score-tone score :chrome)]
                                      [(apply str (repeat (- 5 score) \□)) :chrome]
                                      [(str " " score "  " (str/trim (or actors ""))) :chrome]]
                                     width)))))})))

;; Packets

(defn packet
  [{:keys [lines]} width]
  (let [lines
        (body-lines lines)

        fields
        (reduce
          (fn [fields line]
            (let [next-bit (if-let [f (peek fields)]
                             (inc (long (:end f)))
                             0)]
              (if-let [[_ a b label] (re-matches #"(\d+)(?:-(\d+))?\s*:\s*\"?([^\"]*)\"?" line)]
                (conj fields {:start (parse-long a) :end (parse-long (or b a)) :label label})
                (if-let [[_ n label] (re-matches #"\+(\d+)\s*:\s*\"?([^\"]*)\"?" line)]
                  (conj fields
                        {:start next-bit :end (+ next-bit (long (parse-long n)) -1) :label label})
                  fields))))
          []
          lines)

        per-row
        32

        width
        (long width)

        bit-w
        (max 1 (quot (- width 1) per-row))

        per-row
        (if (< (quot (- width 1) per-row) 1) (max 8 (- width 1)) per-row)

        rows
        (group-by #(quot (long (:start %)) per-row)
                  (mapcat (fn [[at f]]
                            (for [row (range (quot (long (:start f)) per-row)
                                             (inc (quot (long (:end f)) per-row)))]
                              (assoc f
                                :at at
                                :start (max (long (:start f)) (* row per-row))
                                :end (min (long (:end f)) (dec (* (inc row) per-row))))))
                          (map-indexed vector fields)))]

    (if (empty? fields)
      {:reason "empty packet"}
      {:title (title-of lines)
       :rows (mapv
               c/seg-row
               (mapcat
                 (fn [row]
                   (let [parts
                         (sort-by :start (rows row))

                         cell
                         (fn [f]
                           (* bit-w (inc (- (long (:end f)) (long (:start f))))))

                         edge
                         (fn [first-glyph joint]
                           (vec (mapcat (fn [i f]
                                          [[(if (zero? (long i)) first-glyph joint) :chrome]
                                           [(apply str (repeat (dec (cell f)) \─)) :chrome]])
                                        (range)
                                        parts)))

                         top
                         (edge "┌" "┬")

                         bottom
                         (edge "└" "┴")

                         middle
                         (vec (mapcat (fn [f]
                                        (let [w
                                              (dec (cell f))

                                              text
                                              (c/clip (:label f) w)]

                                          [["│" :chrome]
                                           [(c/pad-right
                                              (str (apply str
                                                     (repeat (quot (- w (c/width text)) 2) \space))
                                                   text)
                                              w) (c/series-tone (:at f))]]))
                                      parts))]

                     [(conj top ["┐" :chrome]) (conj middle ["│" :chrome])
                      (conj bottom ["┘" :chrome])]))
                 (sort (keys rows))))})))

;; Venn diagrams

(defn venn
  [{:keys [lines]} width]
  (let [lines
        (body-lines lines)

        width
        (long width)

        entries
        (reduce
          (fn [entries line]
            (cond (re-find #"^(?:set|union)\s" line)
                  (let [[_ kind ids label]
                        (re-find #"^(set|union)\s+([^\[\s]+)(?:\s*\[\"?([^\]\"]*)\"?\])?" line)]
                    (conj entries {:kind kind :ids (str/split ids #",") :label label :texts []}))
                  (and (re-find #"^text\s" line) (seq entries))
                  (let [[_ id label] (re-find #"^text\s+([^\[\s]+)(?:\s*\[\"?([^\]\"]*)\"?\])?"
                                              line)]
                    (update-in entries [(dec (count entries)) :texts] conj (or label id)))
                  :else entries))
          []
          lines)

        names
        (into {}
              (for [{:keys [kind ids label]}
                    entries

                    :when (= "set" kind)]

                [(first ids) (or label (first ids))]))

        tones
        (into {}
              (map-indexed (fn [at {:keys [ids]}]
                             [(first ids) (c/series-tone at)])
                           (filter #(= "set" (:kind %)) entries)))]

    (if (empty? entries)
      {:reason "empty Venn diagram"}
      {:title (title-of lines)
       :rows (mapv c/seg-row
                   (mapcat (fn [{:keys [kind ids label texts]}]
                             (let [head (if (= "set" kind)
                                          [["● " (tones (first ids))]
                                           [(names (first ids) (first ids)) :text]]
                                          (vec (concat
                                                 (mapcat (fn [id]
                                                           [["●" (tones id)]])
                                                         ids)
                                                 [[(str " " (str/join " ∩ " (map #(names % %) ids)))
                                                   :text]]
                                                 (when label [[(str "  " label) :yellow]]))))]
                               (cons (c/seg-clip head width)
                                     (for [text texts]
                                       (c/seg-clip [["    · " :chrome] [text :text]] width)))))
                           entries))})))

;; Cynefin frameworks

(def ^:private domains
  [["complex" :purple] ["complicated" :blue] ["chaotic" :red] ["clear" :green]
   ["confusion" :yellow]])

(defn- domain-box
  [title tone items ^long w]
  (let [inner
        (- w 4)

        bar
        (apply str (repeat (- w 2) \─))]

    (vec (concat [[["┌" tone] [bar tone] ["┐" tone]]
                  [["│ " tone] [(c/pad-right (c/clip (str/capitalize title) inner) inner) tone]
                   [" │" tone]]]
                 (for [item
                       items

                       line
                       (c/wrap-words (str "• " item) inner)]

                   [["│ " tone] [(c/pad-right line inner) :text] [" │" tone]])
                 [[["└" tone] [bar tone] ["┘" tone]]]))))

(defn cynefin
  [{:keys [lines]} width]
  (let [lines
        (body-lines lines)

        width
        (long width)

        items
        (loop [lines
               lines

               domain
               nil

               out
               {}]

          (if-let [line (first lines)]
            (let [word (str/lower-case (first (str/split line #"\s+")))]
              (cond (some #(= word (first %)) domains) (recur (rest lines) word out)
                    (and domain (str/starts-with? line "\""))
                    (recur (rest lines)
                           domain
                           (update out domain (fnil conj []) (c/clean-label line)))
                    :else (recur (rest lines) domain out)))
            out))

        box
        (fn [[name tone] w]
          (domain-box name tone (get items name) w))

        half
        (quot (- width 1) 2)

        side-by-side
        (fn [a b]
          (let [h
                (max (count a) (count b))

                blank
                [[(apply str (repeat half \space)) :none]]]

            (for [r (range h)]
              (vec (concat (get a r blank) [[" " :none]] (get b r blank))))))]

    {:title (title-of lines)
     :rows (mapv c/seg-row
                 (if (>= half 16)
                   (concat (side-by-side (box (nth domains 0) half) (box (nth domains 1) half))
                           (side-by-side (box (nth domains 2) half) (box (nth domains 3) half))
                           (when (seq (get items "confusion")) (box (nth domains 4) width)))
                   (mapcat #(box % width)
                           (filter #(or (seq (get items (first %))) (not= "confusion" (first %)))
                                   domains))))}))

;; Event models

(def ^:private event-kinds
  {"ui" ["UI" :text]
   "cmd" ["Command" :blue]
   "command" ["Command" :blue]
   "evt" ["Event" :orange]
   "event" ["Event" :orange]
   "rmo" ["Read model" :green]
   "readmodel" ["Read model" :green]
   "pcr" ["Processor" :purple]
   "processor" ["Processor" :purple]
   "automation" ["Processor" :purple]})

(defn event-modeling
  [{:keys [lines]} width]
  (let [lines
        (body-lines lines)

        width
        (long width)

        data
        (loop [lines
               lines

               open
               nil

               out
               {}]

          (if-let [line (first lines)]
            (cond open (if (= "}" line)
                         (recur (rest lines) nil out)
                         (recur (rest lines) open (update out open (fnil conj []) line)))
                  (re-find #"^data\s+(\S+)\s*\{" line)
                  (recur (rest lines) (second (re-find #"^data\s+(\S+)" line)) out)
                  :else (recur (rest lines) nil out))
            out))

        frames
        (for [line
              lines

              :let
              [[_ word n kind name ref]
               (re-find
                 #"^(tf|timeframe|rf|resetframe)\s+(\S+)\s+(\S+)\s+(.+?)(?:\s*\[\[(.*)\]\])?\s*$"
                 line)]

              :when word]

          {:n n
           :kind (str/lower-case kind)
           :name name
           :ref ref
           :reset? (str/starts-with? word "r")})

        kind-w
        (long
          (apply max 1 (map #(c/width (first (get event-kinds (:kind %) [(:kind %)]))) frames)))]

    (if (empty? frames)
      {:reason "empty event model"}
      {:rows (mapv c/seg-row
                   (mapcat (fn [{:keys [n kind name ref reset?]}]
                             (let [[label tone] (get event-kinds kind [kind :text])]
                               (cons (c/seg-clip [[(str n " ") :chrome] ["▌" tone]
                                                  [(str " " (c/pad-right label kind-w) "  ") tone]
                                                  [name :text] [(if reset? "  ↺" "") :chrome]]
                                                 width)
                                     (for [row (get data ref)]
                                       (c/seg-clip [[(apply str
                                                       (repeat (+ (count n) 3 kind-w) \space))
                                                     :none] ["  " :none] [row :chrome]]
                                                   width)))))
                           frames))})))
