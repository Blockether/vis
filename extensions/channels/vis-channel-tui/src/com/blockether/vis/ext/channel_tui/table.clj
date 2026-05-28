(ns com.blockether.vis.ext.channel-tui.table
  "Reusable fixed-width table primitives for TUI dialogs/pickers."
  (:require [clojure.string :as str]
            [com.blockether.vis.ext.channel-tui.primitives :as p]
            [com.blockether.vis.ext.channel-tui.theme :as t]))

(defn ellipsize
  [s max-w]
  (let [txt   (or s "")
        max-w (long max-w)]
    (cond
      (<= max-w 0) ""
      (<= (p/display-width txt) max-w) txt
      (= max-w 1) "…"
      :else (str (p/truncate-cols txt (dec max-w)) "…"))))

(defn fit-cell
  ([value width]
   (fit-cell value width :left))
  ([value width align]
   (let [text (ellipsize (or value "") width)]
     (case align
       :right (p/pad-left text width)
       :center (let [w    (long width)
                     used (p/display-width text)
                     lpad (max 0 (quot (- w used) 2))]
                 (p/pad-right (str (apply str (repeat lpad \space)) text) w))
       (p/pad-right text width)))))

(defn column-widths
  "Return concrete widths for `columns` inside total rendered `table-w`.
   Column spec accepts `:width` for fixed cells and `:flex` for proportional
   leftover. Total row width includes outer spaces and ` │ ` separators."
  [columns table-w]
  (let [columns   (vec columns)
        n         (count columns)
        table-w   (max n (long table-w))
        overhead  (if (pos? n) (dec (* 3 n)) 0)
        available (max n (- table-w overhead))
        fixed     (reduce + 0 (keep :width columns))
        flex-cols (keep-indexed (fn [idx col]
                                  (when-not (:width col)
                                    [idx (long (or (:flex col) 1))]))
                    columns)
        flex-total (reduce + 0 (map second flex-cols))
        leftover  (max 0 (- available fixed))
        widths    (mapv (fn [col]
                          (if-let [w (:width col)]
                            (max 1 (long w))
                            1))
                    columns)]
    (if (and (seq flex-cols) (pos? flex-total))
      (loop [pairs flex-cols
             widths widths
             remaining leftover
             remaining-flex flex-total]
        (if-let [[idx flex] (first pairs)]
          (let [w (if (next pairs)
                    (max 1 (long (Math/floor (* remaining (/ (double flex)
                                                            (double remaining-flex))))))
                    (max 1 remaining))]
            (recur (next pairs)
              (assoc widths idx w)
              (max 0 (- remaining w))
              (max 0 (- remaining-flex flex))))
          widths))
      widths)))

(defn row-line
  "Render `cells` to one fixed-width table row. Legacy arity accepts explicit
   widths; column arity accepts specs + row map."
  ([widths cells]
   (row-line widths cells (repeat :left)))
  ([widths cells aligns]
   (str " "
     (str/join " │ " (map fit-cell cells widths aligns))
     " "))
  ([columns row table-w _opts]
   (let [widths (column-widths columns table-w)
         cells  (mapv (fn [{:keys [id render]}]
                        (if render
                          (render row)
                          (get row id "")))
                  columns)
         aligns (mapv #(or (:align %) :left) columns)]
     (row-line widths cells aligns))))

(defn header-line
  [columns table-w]
  (row-line (column-widths columns table-w)
    (mapv #(or (:label %) "") columns)
    (mapv #(or (:align %) :left) columns)))

(defn border-line
  ([widths kind]
   (let [junction (case kind
                    :top    \┬
                    :middle \┼
                    :bottom \┴
                    \┼)]
     (str/join (str junction)
       (map #(p/horiz-line (+ % 2)) widths))))
  ([columns table-w kind]
   (border-line (column-widths columns table-w) kind)))

(defn draw-line!
  [g x row width selected? line]
  (p/set-colors! g t/dialog-fg t/dialog-bg)
  (p/fill-rect! g x row width 1)
  (if selected?
    (p/styled g [p/BOLD]
      (p/put-str! g x row (ellipsize line width)))
    (p/put-str! g x row (ellipsize line width))))

(defn row-matches?
  [row query]
  (let [q (some-> query str str/trim str/lower-case)]
    (or (str/blank? q)
      (str/includes?
        (str/lower-case
          (str/join " "
            (keep (fn [[k v]]
                    (when-not (= k :target)
                      (cond
                        (keyword? v) (name v)
                        (nil? v) nil
                        :else (str v))))
              row)))
        q))))
