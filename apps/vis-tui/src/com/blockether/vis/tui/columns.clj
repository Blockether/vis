(ns com.blockether.vis.tui.columns
  "Side-by-side layout: the arithmetic every painter that lays out a `row`,
   `grid` or `wrap` group must agree on.

   Two surfaces paint these groups — the human-input form and the live-view
   band. Both keep their own row vocabulary, so what lives here is only what
   they must not disagree about:

   - `:row` stands ALL children beside each other when [[row-fits?]] allows it.
   - `:grid` cuts its children into rows of [[grid-span]] equal columns, and
     [[grid-chunks]] pads the last row so its columns line up with the rows above.
   - `:wrap` keeps each child at its [[natural-width]] and [[wrap-lines]] packs
     them left to right, starting a new line when the next child does not fit.

   One LINE of several columns is zipped out of one plan per column by
   [[zip-columns]]. Equal columns carry no geometry: the painter divides the row
   it was handed with [[slots]], so a group nested inside a group simply divides
   a narrower row. A `wrap` line carries its text widths as `:cell-widths`, and
   [[width-slots]] turns them into cells at paint time.")

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
  "Text columns one cell costs beyond its own text: the painter's two-column
   inset and the [[gutter]] before the next cell. [[cell-width]] and
   [[wrap-lines]] spend the same amount, so a wrap line of 24-column cells fits
   exactly where a row of them does."
  (+ 2 (long gutter)))

(defn grid-span
  "How many equal columns a `grid` of `n` children gets inside `text-w`: at most
   `max-columns` (unbounded when nil), at most `n`, and only as many as
   [[row-fits?]] allows. A result of one or less means the grid stacks."
  ^long [text-w n max-columns]
  (loop [k (long (cond-> (long n)
                   max-columns
                   (min (long max-columns))))]
    (if (or (<= k 1) (row-fits? text-w k)) (max 0 k) (recur (dec k)))))

(defn grid-chunks
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

(defn line-width
  "Text columns a line of cells `widths` wide spends, with the chrome between them."
  ^long [widths]
  (+ (long (reduce + 0 widths)) (* (long cell-chrome) (max 0 (dec (count widths))))))

(defn wrap-lines
  "Pack children of natural `widths` greedily, left to right, into lines that fit
   `text-w`: `[[0 1] [2] …]`, indexes in source order. A child that does not fit
   beside the ones before it starts the next line; a child wider than the line
   stands alone on it. A nil `text-w` is an unbounded plan, so it is one line."
  [text-w widths]
  (let [widths (vec widths)]
    (reduce (fn [lines i]
              (let [line (peek lines)]
                (if (and line
                         (or (nil? text-w)
                             (<= (line-width (conj (mapv widths line) (nth widths i)))
                                 (- (long text-w) 2))))
                  (conj (pop lines) (conj line i))
                  (conj lines [i]))))
            []
            (range (count widths)))))

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
   [[slots]]. Each cell gets its text plus the chrome [[wrap-lines]] counted,
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
