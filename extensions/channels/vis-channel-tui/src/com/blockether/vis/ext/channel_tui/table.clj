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
  (let [txt
        (or s "")

        max-w
        (long max-w)]

    (cond (<= max-w 0) ""
          (<= (p/display-width txt) max-w) txt
          :else (let [slash-idx (str/last-index-of txt "/")]
                  (if-not slash-idx
                    (ellipsize txt max-w)
                    (let [filename (subs txt (inc (long slash-idx)))
                          filename-w (p/display-width filename)
                          ellipsis "…"
                          ell-w (p/display-width ellipsis)
                          min-needed (+ ell-w 1 filename-w)]

                      (if (< max-w min-needed)
                        (ellipsize txt max-w)
                        (let [head-budget (- max-w filename-w 1 ell-w)
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
       (let [w (long width)
             used (p/display-width text)
             lpad (max 0 (quot (- w used) 2))]

         (p/pad-right (str (apply str (repeat lpad \space)) text) w))

       (p/pad-right text width)))))

(defn column-widths
  "Return concrete widths for `columns` inside total rendered `table-w`.
   Column spec accepts `:width` for fixed cells and `:flex` for proportional
   leftover. Total row width includes outer spaces and ` │ ` separators."
  [columns table-w]
  (let [columns
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
      (loop [pairs
             flex-cols

             widths
             widths

             remaining
             (long leftover)

             remaining-flex
             (long flex-total)]

        (if-let [[idx flex] (first pairs)]
          (let [w (if (next pairs)
                    (max 1
                         (long (Math/floor (* remaining
                                              (/ (double flex) (double remaining-flex))))))
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
   (let [widths
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
   (let [widths
         (column-widths columns table-w)

         aligns
         (mapv #(or (:align %) :left) columns)

         sep
         (or (:sep opts) " │ ")]

     (str " " (str/join sep (map fit-cell (mapv #(or (:label %) "") columns) widths aligns)) " "))))

(defn border-line
  ([widths kind]
   (let [junction (case kind
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
  (let [[left junction right] (case kind
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
  (let [q (some-> query
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
