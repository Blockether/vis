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

(defn- base-selected-ranges
  [selection cols rows]
  (let [cols (long (max 0 cols))
        rows (long (max 0 rows))]
    (when (and (pos? cols) (pos? rows) (:anchor selection) (:focus selection))
      (let [{:keys [start end]} (normalize selection)
            start-row (long (:row start))
            end-row   (long (:row end))
            sr        (max 0 (min (dec rows) start-row))
            er        (max 0 (min (dec rows) end-row))
            sc        (max 0 (min (dec cols) (:col start)))
            ec        (max 0 (min (dec cols) (:col end)))]
        (vec
          (keep (fn [row]
                  (let [from (if (= row start-row) sc 0)
                        to   (if (= row end-row) ec (dec cols))
                        w    (inc (- to from))]
                    (when (pos? w)
                      {:row row :col from :width w})))
            (range sr (inc er))))))))

(defn- intersect-row-ranges
  [ranges selectable-ranges]
  (let [selectable-by-row (group-by :row selectable-ranges)]
    (vec
      (for [{:keys [row col width]} ranges
            selectable (get selectable-by-row row)
            :let [from (max (long col) (long (:col selectable)))
                  to   (min (+ (long col) (long width))
                         (+ (long (:col selectable)) (long (:width selectable))))]
            :when (< from to)]
        {:row row :col from :width (- to from)}))))

(defn selected-ranges
  "Return inclusive row ranges for a normalized terminal-style selection.

   Each range is `{:row r :col c :width w}` in screen cells. `cols`/`rows`
   clip stale mouse coordinates after resizes.

   Optional `selectable-ranges` restricts the result to bubble cells, so a
   drag that crosses the header, input box, footer, or gutters highlights and
   copies only transcript bubble text."
  ([selection cols rows]
   (selected-ranges selection cols rows nil))
  ([selection cols rows selectable-ranges]
   (let [ranges (base-selected-ranges selection cols rows)]
     (cond
       (nil? selectable-ranges) ranges
       (seq selectable-ranges) (intersect-row-ranges ranges selectable-ranges)
       :else []))))

(defn point-in-ranges?
  "True when `point` lies inside at least one selectable row range.

   Optional `:row-padding` expands the vertical hitbox without expanding the
   copied/highlighted cells. This gives mouse selection a forgiving start zone
   above and below bubbles while preserving bubble-only copy semantics."
  ([point ranges]
   (point-in-ranges? point ranges nil))
  ([{:keys [col row]} ranges {:keys [row-padding]}]
   (let [col     (long col)
         row     (long row)
         row-pad (long (max 0 (or row-padding 0)))]
     (boolean
       (some (fn [{r :row c :col w :width}]
               (and (>= row (- (long r) row-pad))
                 (<= row (+ (long r) row-pad))
                 (>= col (long c))
                 (< col (+ (long c) (long w)))))
         ranges)))))

(defn screen->document-point
  "Convert a visible screen point into a transcript document point.

   Document rows are relative to the top of the full message scrollback, not
   the terminal. Keeping drag anchors in this coordinate space makes selection
   stay attached to content while the viewport auto-scrolls underneath it."
  [{:keys [col row]} {:keys [viewport-top eff-scroll]}]
  (point col (+ (long (or eff-scroll 0))
               (- (long row) (long (or viewport-top 0))))))

(defn document->screen-selection
  "Project a document-space selection into the current screen viewport."
  [{:keys [anchor focus]} {:keys [viewport-top eff-scroll]}]
  (let [viewport-top (long (or viewport-top 0))
        eff-scroll   (long (or eff-scroll 0))
        ->screen     (fn [{:keys [col row]}]
                       (point col (+ viewport-top
                                    (- (long row) eff-scroll))))]
    {:anchor (->screen anchor)
     :focus  (->screen focus)}))

(defn auto-scroll-step
  "Return `{:direction :up|:down :amount n}` while a drag-selection point is in
   a viewport edge zone, otherwise nil.

   The amount ramps up toward the outermost row so selecting at the very top or
   bottom of the messages viewport scrolls faster than hovering near the middle
   of the edge zone."
  [{:keys [row]} {:keys [top bottom edge-size max-step]}]
  (let [row      (long row)
        top      (long top)
        bot      (long bottom)
        edge     (long (max 0 (or edge-size 1)))
        max-step (long (max 1 (or max-step edge 1)))]
    (when (and (< top bot) (pos? edge))
      (cond
        (< row (+ top edge))
        (let [distance (max 0 (- row top))]
          {:direction :up
           :amount    (min max-step (max 1 (- edge distance)))})

        (>= row (- bot edge))
        (let [distance (max 0 (- (dec bot) row))]
          {:direction :down
           :amount    (min max-step (max 1 (- edge distance)))})))))

(defn auto-scroll-direction
  "Return `:up`, `:down`, or nil while a drag-selection point is at a
   viewport edge.

   `top` is inclusive, `bottom` is exclusive, and `edge-size` is the number of
   rows at either edge that should trigger auto-scroll."
  [point opts]
  (:direction (auto-scroll-step point opts)))

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
   line, while leading indentation inside the selected span is preserved.

   Optional `selectable-ranges` restricts extraction to bubble cells only."
  ([screen-cells selection]
   (selected-text screen-cells selection nil))
  ([screen-cells selection selectable-ranges]
   (let [rows (vec (or screen-cells []))
         row-count (count rows)
         cols (long (or (some-> rows first row-cells count) 0))
         ranges (selected-ranges selection cols row-count selectable-ranges)]
     (->> ranges
       (map (fn [{:keys [row col width]}]
              (let [cells (row-cells (nth rows row []))
                    from  (min (count cells) (long col))
                    to    (min (count cells) (+ from (long width)))]
                (trim-trailing-spaces (apply str (subvec cells from to))))))
       (str/join "\n")))))
