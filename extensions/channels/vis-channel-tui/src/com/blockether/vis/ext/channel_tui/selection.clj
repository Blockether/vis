(ns com.blockether.vis.ext.channel-tui.selection
  "Pure helpers for app-side mouse text selection in the fullscreen TUI.

   This is not native terminal selection. When mouse reporting is enabled the
   terminal sends drag/release events to Vis, so Vis tracks the visible cells,
   highlights the selected range, and copies that range on release."
  (:require [clojure.string :as str]))

(defn point
  "Construct a normalized screen point map."
  [col row]
  {:col (long col) :row (long row)})

(defn normalize
  "Return selection endpoints ordered top-left to bottom-right.

   Input shape: `{:anchor {:col c :row r} :focus {:col c :row r}}`."
  [{:keys [anchor focus]}]
  (let [a {:col (long (:col anchor 0)) :row (long (:row anchor 0))}
        f {:col (long (:col focus 0)) :row (long (:row focus 0))}]
    (if (or (< (:row a) (:row f))
          (and (= (:row a) (:row f)) (<= (:col a) (:col f))))
      {:start a :end f}
      {:start f :end a})))

(defn selected-ranges
  "Return inclusive row ranges for a normalized terminal-style selection.

   Each range is `{:row r :col c :width w}` in screen cells. `cols`/`rows`
   clip stale mouse coordinates after resizes."
  [selection cols rows]
  (let [cols (long (max 0 cols))
        rows (long (max 0 rows))]
    (when (and (pos? cols) (pos? rows) (:anchor selection) (:focus selection))
      (let [{:keys [start end]} (normalize selection)
            sr (max 0 (min (dec rows) (:row start)))
            er (max 0 (min (dec rows) (:row end)))
            sc (max 0 (min (dec cols) (:col start)))
            ec (max 0 (min (dec cols) (:col end)))]
        (vec
          (keep (fn [row]
                  (let [from (if (= row sr) sc 0)
                        to   (if (= row er) ec (dec cols))
                        w    (inc (- to from))]
                    (when (pos? w)
                      {:row row :col from :width w})))
            (range sr (inc er))))))))

(defn- row-cells
  [row]
  (cond
    (string? row) (mapv str row)
    (sequential? row) (mapv #(str (or % " ")) row)
    :else []))

(defn- trim-trailing-spaces
  [s]
  (str/replace s #" +$" ""))

(defn selected-text
  "Extract selected visible text from `screen-cells`.

   `screen-cells` is a vector of rows; each row may be a string or a vector of
   per-cell strings. Trailing screen padding spaces are stripped per copied
   line, while leading indentation inside the selected span is preserved."
  [screen-cells selection]
  (let [rows (vec (or screen-cells []))
        row-count (count rows)
        cols (long (or (some-> rows first row-cells count) 0))
        ranges (selected-ranges selection cols row-count)]
    (->> ranges
      (map (fn [{:keys [row col width]}]
             (let [cells (row-cells (nth rows row []))
                   from  (min (count cells) (long col))
                   to    (min (count cells) (+ from (long width)))]
               (trim-trailing-spaces (apply str (subvec cells from to))))))
      (str/join "\n"))))
