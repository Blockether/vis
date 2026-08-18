(ns com.blockether.vis.ext.channel-tui.columns
  "Side-by-side layout: the arithmetic every painter that lays out a `row` group
   must agree on.

   A layout group whose direction is `:row` stands its children BESIDE each
   other, and two surfaces paint one — the human-input form and the live-view
   band. Both keep their own row vocabulary, so what lives here is only what
   they must not disagree about: how wide one column plans for, how one LINE of
   several columns is zipped out of one plan per column, and where each cell
   starts once the row is finally painted.

   The plan carries cells, never geometry. The painter divides the row it was
   handed at paint time, so a `row` nested inside a `row` needs no new rule —
   the inner one simply divides a narrower row.")

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
