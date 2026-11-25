(ns com.blockether.vis.tui.columns
  "Cell layout shared by live views and Ask forms.")

(def gutter
  "Columns between two cells sitting side by side — enough that a focused row's
   selection marker never touches the cell to its left."
  2)

(defn cell-width
  "How wide ONE of `n` columns plans for inside `text-w`, or nil when the plan
   is unbounded (a caller that measures before it knows its width). Never below
   four columns: narrower than that a cell cannot say anything, and the surface
   is better off stacking its children instead of splitting the row."
  [text-w n]
  (when text-w (max 4 (- (quot (+ (long text-w) 2) (max 1 (long n))) 2 (long gutter)))))

(def min-cell-width
  "Text columns a cell needs to say anything readable. A row splits only when
   each cell gets this many, and a field or a bar in a `wrap` line asks for it."
  24)

(defn row-fits?
  "Whether `n` children can share a row with at least [[min-cell-width]] text
   columns each.

   Live and Ask use this same decision at every nesting level. A row that does
   not fit stacks its children in source order; no field or control is removed.
   A nil width is an unbounded measurement plan. Empty groups never split."
  [text-w n]
  (and (pos? (long n)) (or (nil? text-w) (>= (long (cell-width text-w n)) (long min-cell-width)))))

(def ^:private cell-chrome
  "Columns for the cell inset and the space before the next cell."
  (+ 2 (long gutter)))

(defn- grid-chunks
  "`sections` in source order, cut into rows of `k`. The last row is padded with
   empty sections, so its columns keep the width and position of the rows above."
  [sections k]
  (let [k (max 1 (long k))]
    (mapv (fn [part]
            (into (vec part) (repeat (- k (count part)) [])))
          (partition-all k sections))))

(defn natural-width
  "The width one child asks for: its widest row out of `row-widths`, capped at
   `text-w` (uncapped when nil) and never below one column."
  ^long [text-w row-widths]
  (let [w (max 1 (long (reduce max 0 row-widths)))]
    (if text-w (min (max 1 (long text-w)) w) w)))

(defn zip-columns
  "Zip one row-plan per COLUMN into one plan row per LINE: cell `i` of line `n`
   is column `i`'s `n`-th row, or nothing when that column already ran out.

   The composite row is a LABEL row when its columns start with their labels, so
   scrolling still lands on the words that say which fields are being edited,
   and it is focused when any of its cells is."
  [cells]
  (let [height (long (reduce max 0 (map count cells)))]
    (mapv (fn [i]
            (let [entries (mapv #(nth % i nil) cells)]
              {:kind :columns
               :cells entries
               :is-label (boolean (some #(= :label (:kind %)) entries))
               :is-focused (boolean (some :is-focused entries))}))
          (range height))))

(defn slots
  "Where each of `n` cells starts inside `inner-w` and how wide it is:
   `[[x width] …]`, `x` relative to the row's own left edge.

   The last cell takes the remainder, so a division that does not come out even
   never leaves a column of the row unpainted."
  [inner-w n]
  (let [n
        (max 1 (long n))

        cell-w
        (max 1 (quot (long inner-w) n))]

    (mapv (fn [i]
            (let [taken (* (long i) cell-w)]
              [taken (max 1 (if (= i (dec n)) (- (long inner-w) taken) cell-w))]))
          (range n))))

(defn width-slots
  "Cells of explicit text `widths` inside `inner-w`: `[[x width] …]`, like
   [[slots]]. Each cell gets its text plus the cell inset and gap,
   and the last cell takes the remainder, like [[slots]]. A cell that starts past
   the row's right edge gets width zero, and the painter skips it."
  [inner-w widths]
  (let [inner-w
        (long inner-w)

        last-idx
        (dec (count widths))]

    (first (reduce (fn [[out ^long x] [i w]]
                     (let [room
                           (max 0 (- inner-w x))

                           cell-w
                           (if (= i last-idx) room (min (+ (long w) (long cell-chrome)) room))]

                       [(conj out [x cell-w]) (+ x cell-w)]))
                   [[] 0]
                   (map-indexed vector widths)))))

(defn gap-cells
  "Terminal cells for a layout gap."
  ^long [gap]
  (case (keyword gap) :none 0 :xs 1 :sm 2 :lg 4 3))

(defn- stack
  [blank gap parts]
  (into [] cat (interpose (repeat gap blank) (filter seq parts))))

(defn- row-line
  [width gap justify natural growers]
  (let [n (count natural)
        gap (min (long gap) (quot (max 0 (- (long width) n)) (max 1 (dec n))))
        room (max 0 (- (long width) (* gap (dec n))))
        total (long (reduce + 0 natural))
        free (max 0 (- room total))
        grow-n (count (filter true? growers))
        grow? (pos? grow-n)
        between? (and (not grow?) (= :between justify) (> n 1))
        offset (if grow? 0 (case justify :center (quot free 2) :end free 0))]
    (first
      (reduce (fn [[slots ^long x ^long before ^long grown] [i w grow]]
                (let [w (long w)
                      after (+ before w)
                      next-grow (if grow (inc grown) grown)
                      share (if (and grow? grow)
                              (- (quot (* free next-grow) grow-n) (quot (* free grown) grow-n))
                              0)
                      spacing (if (and between? (< (long i) (dec n)))
                                (- (quot (* free (inc (long i))) (dec n))
                                   (quot (* free (long i)) (dec n)))
                                0)
                      cell (if (> total room)
                             (- (quot (* room after) total) (quot (* room before) total))
                             (+ w share))]
                  [(conj slots [x cell]) (+ x cell gap spacing) after next-grow]))
              [[] offset 0 0]
              (map vector (range) natural growers)))))

(defn- pack-lines
  [width gap natural]
  (first (reduce (fn [[lines ^long used] [i w]]
                   (let [w (long w)
                         next (+ used (long gap) w)]
                     (if (and (seq lines) (<= next (long width)))
                       [(conj (pop lines) (conj (peek lines) i)) next]
                       [(conj lines [i]) w])))
                 [[] 0] (map-indexed vector natural))))

(defn- align-rows
  [parts align blank]
  (let [height (long (reduce max 0 (map count parts)))]
    (mapv (fn [rows]
            (let [free (- height (count rows))]
              (case align
                :center (into (vec (repeat (quot free 2) blank)) rows)
                :end (into (vec (repeat free blank)) rows)
                :stretch (if-let [pad (:stretch-row (meta rows))]
                           (into (into (pop rows) (repeat free pad)) [(peek rows)])
                           rows)
                rows)))
          parts)))

(defn layout
  "Plan children with the available text width. `plan` takes a child and its
   text width. `measure` reads a child's preferred text width from its plan.
   `zip` joins rows; its result carries explicit cell slots for every painter."
  [{:keys [type direction is-wrapping gap align justify min-width] maximum :columns}
   text-w children plan measure zip blank]
  (let [text-w (max 1 (long (or text-w 1000)))
        width (+ text-w 3)
        gap (gap-cells gap)
        align (or (keyword align) :stretch)
        justify (keyword justify)
        children (vec children)
        join (fn [slots nodes align]
               (let [parts (mapv (fn [node [_ w]]
                                   (if (and (map? node) (>= (long w) 4)) (plan node (- (long w) 3)) []))
                                 nodes slots)]
                 (with-meta (mapv #(assoc % :cell-slots slots) (zip (align-rows parts align blank)))
                   {:parts parts})))
        lines (cond
                (= :grid type)
                (let [cell-min (case (keyword min-width) :xs 16 :sm 24 :lg 40 32)
                      n (max 1 (min (long (or maximum Long/MAX_VALUE))
                                    (quot (+ width gap) (+ cell-min gap))))
                      room (- width (* gap (dec n)))
                      slots (mapv (fn [i]
                                    (let [i (long i)
                                          left (quot (* room i) n)]
                                      [(+ left (* gap i)) (- (quot (* room (inc i)) n) left)]))
                                  (range n))]
                  (if (= 1 n) (mapv #(plan % text-w) children)
                    (mapv #(join slots % :start) (grid-chunks children n))))

                (and (= :flex type) (= :row (keyword direction)))
                (let [wide (mapv #(plan % text-w) children)
                      natural (mapv #(min width (+ 3 (long (measure text-w %)))) wide)
                       growers (mapv (comp boolean :is-growing) children)
                       places (row-line width gap justify natural growers)
                       clipped? (some (fn [[node wanted [_ w]]]
                                        (or (< (long w) 4)
                                            (and (#{:button :menu :toggle :badge} (:type node))
                                                 (< (long w) (long wanted)))))
                                      (map vector children natural places))
                       groups (if (or is-wrapping clipped?) (pack-lines width gap natural)
                                  [(vec (range (count children)))])
                       overflow? (and (not is-wrapping) (> (count groups) 1))]
                  (mapv (fn [ids]
                          (let [nodes (mapv children ids)
                                slots (row-line width gap justify (mapv natural ids)
                                                (mapv (comp boolean :is-growing) nodes))]
                             (cond-> (join slots nodes align)
                               (and overflow? (not= ids (peek groups)))
                               (conj {:kind :row-overflow :text "…"}))))
                        (filter seq groups)))

                :else
                (mapv (fn [node]
                        (let [rows (plan node text-w)]
                          (if (and (= :flex type) (not= :stretch align))
                            (let [w (min width (+ 3 (long (measure text-w rows))))
                                  free (- width w)
                                  x (case align :center (quot free 2) :end free 0)]
                              (join [[x w]] [node] :start))
                            rows)))
                      children))]
    (with-meta (stack blank gap lines) {:parts (mapcat #(or (:parts (meta %)) [%]) lines)})))

(defn row-slots
  "The positions and widths of the row's cells at paint time."
  [inner-w {:keys [cells cell-slots cell-widths]}]
  (cond cell-slots
        (mapv (fn [[x w]] [x (min (long w) (max 0 (- (long inner-w) (long x))))]) cell-slots)
        cell-widths (width-slots inner-w cell-widths)
        :else (slots inner-w (count cells))))
