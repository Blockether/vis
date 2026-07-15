(ns com.blockether.vis.ext.channel-tui.boxed-table
  "Bordered, scrollable, single-selection data table for TUI dialogs.

   Composes the lower-level `table` border/row primitives with
   `scrollbar` so callers don't repeat the same boilerplate (top
   border + header row + middle separator + N body rows + selection
   marker gutter + scrollbar) at every dialog site.

   Geometry rules
   --------------
   Given a dialog `bounds` ({:left :inner-w}) the layout reserves:

     col `left+1`            → table `│` left border (flush to dialog edge)
     col `left+3`            → selection marker, INSIDE the first column
     cols `left+1 .. R-1`    → boxed table (own `│` borders included)
     col `R`                 → scrollbar (right of right table border)

   where `R = left + inner-w`. This guarantees the scrollbar never
   overpaints the table's right `│` border.

   The selection marker lives INSIDE the first column: the first data
   column is internally widened by `p/SELECTION_WIDTH`, its cell text
   is indented by that many cols, and the marker glyph is painted over
   the reserved gutter (matching the Ctrl+G navigator). Callers size
   their columns against the reported `:table-content-w`, which already
   excludes the marker reserve — no caller-side change needed.

   Row layout (relative to caller-provided `:top`)
   -----------------------------------------------
     top+0  ┌── top border ──┐
     top+1  │ header row     │
     top+2  ├── separator  ──┤
     top+3  │ first body row │
     …
     top+2+body-h │ last body row │
     top+3+body-h └── bottom ──┘  (only when `:closed? true`)

   By default the table is open at the bottom; callers usually follow
   the body with a divider + status line and don't want a closing
   `└┘` cap. Pass `:closed? true` to draw the closing border (matches
   the navigator-style boxed picker)."
  (:require [com.blockether.vis.ext.channel-tui.primitives :as p]
            [com.blockether.vis.ext.channel-tui.scrollbar :as scrollbar]
            [com.blockether.vis.ext.channel-tui.table :as table]
            [com.blockether.vis.ext.channel-tui.theme :as t]))

(set! *unchecked-math* :warn-on-boxed)

(defn layout
  "Compute table geometry inside a dialog `bounds` ({:left :inner-w}).
   Pure: no drawing. Useful for mouse hit-testing before `draw!`."
  [{:keys [left inner-w]}]
  (let [left
        (long left)

        inner-w
        (long inner-w)

        table-x
        (+ left 1)

        table-w
        (long (max 1 (- inner-w 1)))

        ;; Full rendered row width (both `│` borders included).
        rendered-w
        table-w

        ;; First column hosts the marker inside the box, so the caller's
        ;; data columns get `SELECTION_WIDTH` fewer cells.
        table-content-w
        (long (max 1 (- rendered-w p/SELECTION_WIDTH)))]

    {:marker-col (+ table-x 2)
     :table-x table-x
     :table-w table-w
     :rendered-w rendered-w
     :table-content-w table-content-w
     :scrollbar-col (+ table-x table-w)}))

(defn rows
  "Resolve the absolute row indices for each painted line, given the
   caller-supplied `top` and `body-h`."
  [top body-h]
  (let [top
        (long top)

        body-h
        (long body-h)

        body-top
        (+ top 3)]

    {:border-top top
     :header (+ top 1)
     :separator (+ top 2)
     :body-top body-top
     :body-end (+ body-top body-h)}))

(defn hit-row
  "Map a mouse `(mx, my)` into a body row index, or nil if outside the
   table body. `geom` is `(layout bounds)`, `top` and `body-h` match the
   `draw!` call, `scroll` is the current scroll offset."
  [{:keys [table-x rendered-w table-content-w]} top body-h scroll mx my]
  (let [{:keys [body-top]}
        (rows top body-h)

        table-x
        (long table-x)

        body-top
        (long body-top)

        body-h
        (long body-h)

        scroll
        (long scroll)

        mx
        (long mx)

        my
        (long my)

        row-w
        (long (or rendered-w table-content-w))]

    (when (and (>= mx table-x) (< mx (+ table-x row-w)) (>= my body-top) (< my (+ body-top body-h)))
      (+ scroll (- my body-top)))))

(defn- empty-row-cells
  "Default empty-state cells: blank in every column except the second
   one which carries `message`. Falls back to first column when there
   is only one column."
  [widths message]
  (let [n (count widths)]
    (cond (zero? n) []
          (= 1 n) [message]
          :else (assoc (vec (repeat n "")) 1 message))))

(defn draw!
  "Render a bordered scrollable table in one call.

   Positional arg matches `scrollbar/draw!`:
     `^TextGraphics g` — the live dialog graphics surface.

   Required opts:
     :bounds       {:left :inner-w} (from `draw-dialog-chrome!`)
     :top          first row to paint (top border lands here)
     :body-h       visible body rows
     :headers      vector of header strings (count must match :widths)
     :widths       per-column widths in cells (from caller's sizing fn)
     :total        total row count in the data set
     :scroll       current scroll offset (0-based row index)
     :selected     current selection (0-based row index)
     :cell-fn      (fn [idx] -> seq of strings) for row at absolute idx

   Optional opts:
     :empty-cells   per-column cells painted when total = 0; defaults to
                    blanks with `:empty-message` in column 1
     :empty-message string shown in the default empty row
     :aligns        per-column alignment vec (default `(repeat :left)`)
     :closed?       when true, paint a closing `└──┘` border one row
                    below the last body row (default false)

   Returns the full layout map (merge of `layout` + `rows`) so callers
   can place mode lines, hint bars, hit-tests, etc."
  [^com.googlecode.lanterna.graphics.TextGraphics g
   {:keys [bounds top body-h headers widths total scroll selected cell-fn empty-cells empty-message
           aligns closed?]
    :or {empty-message "No items." aligns (repeat :left) closed? false}}]
  (let [{:keys [marker-col table-x rendered-w scrollbar-col] :as geom}
        (layout bounds)

        row-ix
        (rows top body-h)

        {:keys [border-top header separator body-top]}
        row-ix

        rendered-w
        (long rendered-w)

        table-x
        (long table-x)

        body-top
        (long body-top)

        body-h
        (long body-h)

        scroll
        (long scroll)

        total
        (long total)

        aligns
        (vec (take (count widths) aligns))

        mk
        p/SELECTION_WIDTH

        pad
        (apply str (repeat mk \space))

        ;; Widen the first column to host the in-box marker gutter and
        ;; indent that column's text so the glyph never overpaints data.
        full-widths
        (let [v (vec widths)]
          (if (seq v) (update v 0 + mk) v))

        mark-first
        (fn [cells]
          (let [v (vec cells)]
            (if (seq v) (update v 0 #(str pad %)) v)))

        empty-cells
        (mark-first (or empty-cells (empty-row-cells widths empty-message)))]

    ;; Chrome
    (p/set-colors! g t/dialog-border t/dialog-bg)
    (p/put-str! g table-x border-top (table/boxed-border-line full-widths :top))
    (p/set-colors! g t/dialog-hint-key t/dialog-bg)
    (p/put-str! g table-x header (table/boxed-row-line full-widths (mark-first headers) aligns))
    ;; Re-paint the side `│` borders in the border color: the header row
    ;; was painted in dialog-hint-key, which would otherwise leave the
    ;; vertical edges a different color than the top/middle/bottom chrome.
    (p/set-colors! g t/dialog-border t/dialog-bg)
    (p/put-str! g table-x header "│")
    (p/put-str! g (+ table-x (dec rendered-w)) header "│")
    (p/put-str! g table-x separator (table/boxed-border-line full-widths :middle))
    ;; Body
    (dotimes [i body-h]
      (let [idx (+ scroll i)
            row (+ body-top i)]

        (cond (< idx total) (do (table/draw-line! g
                                                  table-x
                                                  row
                                                  rendered-w
                                                  (= idx selected)
                                                  (table/boxed-row-line full-widths
                                                                        (mark-first (cell-fn idx))
                                                                        aligns))
                                ;; Re-paint the side `│` borders in the border color: draw-line!
                                ;; filled the whole row in dialog-fg, which would otherwise leave the
                                ;; vertical edges lighter than the top/middle/bottom chrome.
                                (p/set-colors! g t/dialog-border t/dialog-bg)
                                (p/put-str! g table-x row "│")
                                (p/put-str! g (+ table-x (dec rendered-w)) row "│")
                                (p/set-colors! g t/dialog-hint-key t/dialog-bg)
                                (p/draw-selection-marker! g marker-col row (= idx selected)))
              (and (zero? total) (zero? i))
              (do (p/set-colors! g t/dialog-hint t/dialog-bg)
                  (p/fill-rect! g table-x row rendered-w 1)
                  (p/put-str! g table-x row (table/boxed-row-line full-widths empty-cells aligns))
                  (p/set-colors! g t/dialog-border t/dialog-bg)
                  (p/put-str! g table-x row "│")
                  (p/put-str! g (+ table-x (dec rendered-w)) row "│"))
              :else (do (p/set-colors! g t/dialog-fg t/dialog-bg)
                        (p/fill-rect! g table-x row rendered-w 1)
                        (p/put-str! g
                                    table-x
                                    row
                                    (table/boxed-row-line full-widths
                                                          (vec (repeat (count full-widths) ""))
                                                          aligns))
                        (p/set-colors! g t/dialog-border t/dialog-bg)
                        (p/put-str! g table-x row "│")
                        (p/put-str! g (+ table-x (dec rendered-w)) row "│")))))
    ;; Optional closing border (matches navigator-style boxed picker)
    (when closed?
      (p/set-colors! g t/dialog-border t/dialog-bg)
      (p/put-str! g table-x (+ body-top body-h) (table/boxed-border-line full-widths :bottom)))
    ;; Scrollbar (own column outside table's right `│` border)
    (scrollbar/draw! g
                     {:col scrollbar-col
                      :top body-top
                      :track-h body-h
                      :total-h total
                      :inner-h body-h
                      :scroll scroll})
    (merge geom row-ix)))
