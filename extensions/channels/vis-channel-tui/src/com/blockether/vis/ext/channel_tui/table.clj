(ns com.blockether.vis.ext.channel-tui.table
  "Reusable fixed-width table primitives for TUI dialogs/pickers."
  (:require [clojure.string :as str]
            [com.blockether.vis.ext.channel-tui.primitives :as p]
            [com.blockether.vis.ext.channel-tui.theme :as t]))

(set! *unchecked-math* :warn-on-boxed)

(defn ellipsize
  "Right-truncate `s` to `max-w` columns with a trailing `…`.
   Thin delegate over the canonical `p/ellipsize` (lanterna-backed)."
  [s max-w]
  (p/ellipsize s max-w))

(defn middle-ellipsize
  "Truncate `s` in the middle when it exceeds `max-w` display columns,
   keeping the filename (after last /) fully visible. Falls back to
   right-ellipsize when there is no `/` or the column is too narrow."
  [s max-w]
  (let
    [txt
     (or s "")

     max-w
     (long max-w)]

    (cond (<= max-w 0) ""
          (<= (p/display-width txt) max-w) txt
          :else (let [slash-idx (str/last-index-of txt "/")]
                  (if-not slash-idx
                    (ellipsize txt max-w)
                    (let
                      [filename (subs txt (inc (long slash-idx)))
                       filename-w (p/display-width filename)
                       ellipsis "…"
                       ell-w (p/display-width ellipsis)
                       min-needed (+ ell-w 1 filename-w)]

                      (if (< max-w min-needed)
                        (ellipsize txt max-w)
                        (let
                          [head-budget (- max-w filename-w 1 ell-w)
                           dir (subs txt 0 slash-idx)
                           head (if (pos? head-budget)
                                  (str (p/truncate-cols dir head-budget) ellipsis)
                                  ellipsis)]

                          (str head "/" filename)))))))))

(defn fit-cell
  ([value width] (fit-cell value width :left))
  ([value width align]
   (let [text (ellipsize (or value "") width)]
     (case align
       :right
       (p/pad-left text width)

       :center
       (let
         [w (long width)
          used (p/display-width text)
          lpad (max 0 (quot (- w used) 2))]

         (p/pad-right (str (apply str (repeat lpad \space)) text) w))

       (p/pad-right text width)))))

(defn column-widths
  "Return concrete widths for `columns` inside total rendered `table-w`.
   Column spec accepts `:width` for fixed cells and `:flex` for proportional
   leftover. Total row width includes outer spaces and ` │ ` separators."
  [columns table-w]
  (let
    [columns
     (vec columns)

     n
     (count columns)

     table-w
     (max n (long table-w))

     overhead
     (if (pos? n) (dec (* 3 n)) 0)

     available
     (max n (- table-w overhead))

     fixed
     (reduce + 0 (keep :width columns))

     flex-cols
     (keep-indexed (fn [idx col]
                     (when-not (:width col) [idx (long (or (:flex col) 1))]))
                   columns)

     flex-total
     (reduce + 0 (map second flex-cols))

     leftover
     (max 0 (- (long available) (long fixed)))

     widths
     (mapv (fn [col]
             (if-let [w (:width col)]
               (max 1 (long w))
               1))
           columns)]

    (if (and (seq flex-cols) (pos? (long flex-total)))
      (loop
        [pairs
         flex-cols

         widths
         widths

         remaining
         (long leftover)

         remaining-flex
         (long flex-total)]

        (if-let [[idx flex] (first pairs)]
          (let
            [w (if (next pairs)
                 (max 1 (long (Math/floor (* remaining (/ (double flex) (double remaining-flex))))))
                 (max 1 remaining))]
            (recur (next pairs)
                   (assoc widths idx w)
                   (max 0 (- remaining w))
                   (max 0 (- remaining-flex (long flex)))))
          widths))
      widths)))

(defn row-line
  "Render `cells` to one fixed-width table row. Legacy arity accepts explicit
   widths; column arity accepts specs + row map. `opts` supports `:sep`."
  ([widths cells] (row-line widths cells (repeat :left)))
  ([widths cells aligns] (str " " (str/join " │ " (map fit-cell cells widths aligns)) " "))
  ([columns row table-w opts]
   (let
     [widths
      (column-widths columns table-w)

      cells
      (mapv (fn [{:keys [id render]}]
              (if render (render row) (get row id "")))
            columns)

      aligns
      (mapv #(or (:align %) :left) columns)

      sep
      (or (:sep opts) " │ ")]

     (str " " (str/join sep (map fit-cell cells widths aligns)) " "))))

(defn header-line
  ([columns table-w] (header-line columns table-w {}))
  ([columns table-w opts]
   (let
     [widths
      (column-widths columns table-w)

      aligns
      (mapv #(or (:align %) :left) columns)

      sep
      (or (:sep opts) " │ ")]

     (str " " (str/join sep (map fit-cell (mapv #(or (:label %) "") columns) widths aligns)) " "))))

(defn border-line
  ([widths kind]
   (let
     [junction (case kind
                 :top
                 p/BOX_T_DOWN

                 :middle
                 p/BOX_CROSS

                 :bottom
                 p/BOX_T_UP

                 p/BOX_CROSS)]
     (p/joined-horiz-line (map #(+ (long %) 2) widths) junction)))
  ([columns table-w kind] (border-line (column-widths columns table-w) kind)))

(defn boxed-row-line
  "Render fixed-width cells with outer vertical borders. Use inside dialog
   bodies when table should visually read as nested dialog chrome."
  [widths cells aligns]
  (str "│ " (str/join " │ " (map fit-cell cells widths aligns)) " │"))

(defn boxed-border-line
  "Render top/middle/bottom border for `boxed-row-line`."
  [widths kind]
  (let
    [[left junction right] (case kind
                             :top
                             [p/BOX_TL p/BOX_T_DOWN p/BOX_TR]

                             :middle
                             [p/BOX_T_R p/BOX_CROSS p/BOX_T_L]

                             :bottom
                             [p/BOX_BL p/BOX_T_UP p/BOX_BR]

                             [p/BOX_T_R p/BOX_CROSS p/BOX_T_L])]
    (p/boxed-horiz-line (map #(+ (long %) 2) widths) left junction right)))

(defn draw-line!
  [g x row width selected? line]
  (p/set-colors! g t/dialog-fg t/dialog-bg)
  (p/fill-rect! g x row width 1)
  (if selected?
    (p/styled g [p/BOLD] (p/put-str! g x row (ellipsize line width)))
    (p/put-str! g x row (ellipsize line width))))

(defn row-matches?
  [row query]
  (let
    [q (some-> query
               str
               str/trim
               str/lower-case)]
    (or (str/blank? q)
        (str/includes? (str/lower-case (str/join " "
                                                 (keep (fn [[k v]]
                                                         ;; `:target` is the action payload, not display text;
                                                         ;; booleans (e.g. row flags like `:focused?`) are
                                                         ;; never meaningful search text.
                                                         (when-not (or (= k :target) (boolean? v))
                                                           (cond (keyword? v) (name v)
                                                                 (nil? v) nil
                                                                 :else (str v))))
                                                       row)))
                       q))))

;;; ── CSV grids (the `vis-table` attachment viewer) ────────────────────────────
;;
;; A CSV/TSV attachment is DATA, not a picture: `vis_attach` emits it as a
;; ````vis-table` fence and BOTH surfaces paint it as a real grid — the TUI
;; through these primitives, the companion through `DataTable.tsx`. Parse,
;; measure, align, page, sort and render all live here as PURE functions, so
;; the table dialog's behaviour is unit-testable without a terminal.

(defn parse-csv
  "Parse RFC-4180 `text` into a vector of row vectors of strings — quoted
   fields, doubled `\"\"` escapes and embedded newlines included. Every row is
   padded to the widest one, so `(nth row i)` is total across the grid."
  [text]
  (let
    [^String s
     (str/replace (str text) "\r\n" "\n")

     n
     (long (count s))]

    (loop
      [i
       0

       ^StringBuilder field
       (StringBuilder.)

       row
       []

       rows
       []

       quoted?
       false]

      (if (>= (long i) n)
        (let
          [rows
           (if (and (zero? (.length field)) (empty? row))
             rows
             (conj rows (conj row (.toString field))))

           width
           (long (reduce max 0 (map count rows)))]

          (mapv (fn [r]
                  (into r (repeat (- width (long (count r))) "")))
                rows))
        (let
          [c
           (.charAt s (int i))

           i
           (long i)]

          (cond quoted? (if (= c \")
                          (if (and (< (inc i) n) (= \" (.charAt s (int (inc i)))))
                            (recur (+ i 2) (.append field \") row rows true)
                            (recur (inc i) field row rows false))
                          (recur (inc i) (.append field c) row rows true))
                (= c \") (recur (inc i) field row rows true)
                (= c \,) (recur (inc i) (StringBuilder.) (conj row (.toString field)) rows false)
                (= c \newline)
                (recur (inc i) (StringBuilder.) [] (conj rows (conj row (.toString field))) false)
                :else (recur (inc i) (.append field c) row rows false)))))))

(def csv-max-col-width
  "Ceiling on ONE column's natural width. A single essay-length cell must not
   push every other column off the grid; the cell ellipsizes instead."
  32)

(defn- csv-natural-widths
  "Per-column content widths, each capped at `csv-max-col-width`."
  [rows]
  (let [n (long (reduce max 0 (map count rows)))]
    (mapv (fn [i]
            (max 1
                 (min (long csv-max-col-width)
                      (long (reduce max 1 (map #(p/display-width (str (nth % i ""))) rows))))))
          (range n))))

(defn csv-natural-width
  "Total rendered width `csv-grid-lines` wants for `rows` when nothing is
   squeezed — borders and separators included. 0 for an empty grid."
  ^long [rows]
  (let
    [ws
     (csv-natural-widths rows)

     n
     (long (count ws))]

    (if (zero? n) 0 (+ (long (reduce + 0 ws)) (* 3 n) 1))))

(defn csv-widths
  "Concrete column widths for a grid drawn with `boxed-row-line` inside
   `table-w` columns: natural widths when they fit, otherwise shrunk in
   proportion to them (never below one column)."
  [rows table-w]
  (let
    [natural
     (csv-natural-widths rows)

     n
     (long (count natural))]

    (if (zero? n)
      []
      (let
        [inner
         (max n (- (long table-w) 2))

         needed
         (+ (long (reduce + 0 natural)) (dec (* 3 n)))]

        (if (<= needed inner)
          natural
          (column-widths (mapv (fn [w]
                                 {:flex w})
                               natural)
                         inner))))))

(defn csv-stretch-widths
  "Grow `widths` so the rendered grid FILLS `table-w` instead of leaving a ragged
   gap on the right — what a spreadsheet pane does. Slack is spread evenly, left
   to right; a grid already wider than `table-w` is returned untouched."
  [widths table-w]
  (let
    [n
     (long (count widths))

     extra
     (- (long table-w) (+ (long (reduce + 0 widths)) (* 3 n) 1))]

    (if (or (zero? n) (neg? extra) (zero? extra))
      (vec widths)
      (let
        [base
         (quot extra n)

         r
         (rem extra n)]

        (vec (map-indexed (fn [i w]
                            (+ (long w) base (if (< (long i) r) 1 0)))
                          widths))))))

(defn csv-number
  "Parse a cell as a number for sorting/alignment. Thousands separators, a
   leading currency mark and a trailing `%` are formatting, not text — a
   `1,234.5` column still sorts numerically. nil when the cell is not a number."
  [cell]
  (some-> cell
          str
          str/trim
          not-empty
          (str/replace #"[,_\s%$€£]" "")
          parse-double))

(defn numeric-column?
  "True when every non-blank cell of DATA rows (header excluded) under column
   `idx` reads as a number — the test that right-aligns a column."
  [rows idx]
  (let
    [vals (keep (fn [r]
                  (not-empty (str/trim (str (nth r (long idx) "")))))
                rows)]
    (and (seq vals) (every? csv-number vals))))

(defn csv-aligns
  "Per-column alignment for `rows` (first row = header): numbers right, text
   left — the convention every spreadsheet uses to make magnitudes comparable."
  [rows]
  (let [data (vec (rest rows))]
    (mapv (fn [i]
            (if (numeric-column? data i) :right :left))
          (range (long (reduce max 0 (map count rows)))))))

(defn page-count
  "How many pages of `page-size` rows `total` rows fill — never fewer than one, so
   `page 1/1` exists even for an empty sheet."
  [total page-size]
  (let
    [t
     (max 0 (long total))

     s
     (max 1 (long page-size))]

    (max 1 (quot (+ t (dec s)) s))))

(defn page-index
  "0-based page holding row `idx` when a page shows `page-size` rows."
  [idx page-size]
  (quot (max 0 (long idx)) (max 1 (long page-size))))

(defn page-start
  "First row index of the page holding `idx` — the grid's scroll offset, which is
   why the viewer moves a WHOLE page at a time instead of creeping row by row."
  [idx page-size]
  (* (long (page-index idx page-size)) (max 1 (long page-size))))

(defn sort-csv-rows
  "Sort DATA rows (header excluded) by column `idx`; `dir` is `:asc`/`:desc`. A
   column whose every non-blank cell parses as a number sorts NUMERICALLY (so 9
   comes before 10), any other column case-insensitively; blanks sort last."
  [rows idx dir]
  (let
    [i
     (long idx)

     cell
     (fn [r]
       (str (nth r i "")))

     keyfn
     (if (numeric-column? rows i)
       (fn [r]
         (or (csv-number (cell r)) Double/MAX_VALUE))
       (fn [r]
         (let [v (str/trim (cell r))]
           [(if (str/blank? v) 1 0) (str/lower-case v)])))

     sorted
     (vec (sort-by keyfn compare (vec rows)))]

    (if (= :desc dir) (vec (reverse sorted)) sorted)))

(defn csv-grid-lines
  "Render `rows` (first row = header) as boxed grid lines at most `table-w`
   columns wide: top border, header, rule, one line per data row, bottom border.
   Opts reuse a measurement or restyle the head:
     :widths  precomputed column widths (default `csv-widths`)
     :aligns  per-column alignment (default `csv-aligns`)
     :header  replacement header cells (e.g. carrying a sort arrow)"
  ([rows table-w] (csv-grid-lines rows table-w {}))
  ([rows table-w {:keys [widths aligns header]}]
   (when (seq rows)
     (let
       [widths
        (or widths (csv-widths rows table-w))

        aligns
        (or aligns (csv-aligns rows))

        head
        (or header (first rows))]

       ;; The head follows the column's own alignment: a numeric label sits over
       ;; its digits instead of drifting to the far left of a wide column.
       (-> [(boxed-border-line widths :top) (boxed-row-line widths head aligns)
            (boxed-border-line widths :middle)]
           (into (map #(boxed-row-line widths % aligns) (rest rows)))
           (conj (boxed-border-line widths :bottom)))))))
