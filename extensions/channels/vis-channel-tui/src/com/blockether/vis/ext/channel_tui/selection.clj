(ns com.blockether.vis.ext.channel-tui.selection
  "Pure helpers for app-side mouse text selection in the fullscreen TUI.

   This is not native terminal selection. When mouse reporting is enabled the
   terminal sends drag/release events to Vis, so Vis tracks the visible cells,
   highlights the selected range, and copies that range on release."
  (:require [clojure.string :as str]))

(set! *unchecked-math* :warn-on-boxed)

(defn point "Construct a normalized screen point map." [col row] {:col (long col) :row (long row)})

(defn normalize
  "Return selection endpoints ordered top-left to bottom-right.

   Input shape: `{:anchor {:col c :row r} :focus {:col c :row r}}`."
  [{:keys [anchor focus]}]
  (let
    [a
     {:col (long (:col anchor 0)) :row (long (:row anchor 0))}

     f
     {:col (long (:col focus 0)) :row (long (:row focus 0))}]

    (if (or (< (long (:row a)) (long (:row f)))
            (and (= (:row a) (:row f)) (<= (long (:col a)) (long (:col f)))))
      {:start a :end f}
      {:start f :end a})))

(defn- base-selected-ranges
  [selection cols rows]
  (let
    [cols
     (long (max 0 (long cols)))

     rows
     (long (max 0 (long rows)))]

    (when (and (pos? cols) (pos? rows) (:anchor selection) (:focus selection))
      (let
        [{:keys [start end]}
         (normalize selection)

         start-row
         (long (:row start))

         end-row
         (long (:row end))

         sr
         (max 0 (min (dec rows) start-row))

         er
         (max 0 (min (dec rows) end-row))

         sc
         (max 0 (min (dec cols) (long (:col start))))

         ec
         (max 0 (min (dec cols) (long (:col end))))]

        (vec (keep (fn [row]
                     (let
                       [from
                        (if (= row start-row) sc 0)

                        to
                        (if (= row end-row) ec (dec cols))

                        w
                        (inc (- to from))]

                       (when (pos? w) {:row row :col from :width w})))
                   (range sr (inc er))))))))

(defn- intersect-row-ranges
  [ranges selectable-ranges]
  (let [selectable-by-row (group-by :row selectable-ranges)]
    (vec (for
           [{:keys [row col width]} ranges
            selectable (get selectable-by-row row)
            :let [from (max (long col) (long (:col selectable)))
                  to (min (+ (long col) (long width))
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
  ([selection cols rows] (selected-ranges selection cols rows nil))
  ([selection cols rows selectable-ranges]
   (let [ranges (base-selected-ranges selection cols rows)]
     (cond (nil? selectable-ranges) ranges
           (seq selectable-ranges) (intersect-row-ranges ranges selectable-ranges)
           :else []))))

(defn point-in-ranges?
  "True when `point` lies inside at least one selectable row range.

   Optional `:row-padding` expands the vertical hitbox and `:col-padding` the
   horizontal one, WITHOUT expanding the copied/highlighted cells. This gives
   mouse selection a forgiving start zone above/below AND left/right of bubbles
   (so a press in a bubble's left gutter or right padding still starts a
   selection) while preserving bubble-only copy semantics — the selection is
   still clipped back to the content band by `selected-ranges`."
  ([point ranges] (point-in-ranges? point ranges nil))
  ([{:keys [col row]} ranges {:keys [row-padding col-padding]}]
   (let
     [col
      (long col)

      row
      (long row)

      row-pad
      (long (max 0 (long (or row-padding 0))))

      col-pad
      (long (max 0 (long (or col-padding 0))))]

     (boolean (some (fn [{r :row c :col w :width}]
                      (and (>= row (- (long r) row-pad))
                           (<= row (+ (long r) row-pad))
                           (>= col (- (long c) col-pad))
                           (< col (+ (long c) (long w) col-pad))))
                    ranges)))))

(defn source-at-point
  "Return the selectable text source under `point`, or nil.

   Input rows are checked first so the input editor wins if a vertical comfort
   zone ever overlaps transcript chrome. Both sources use the same
   `point-in-ranges?` predicate, which keeps hit-testing behaviour identical for
   transcript bubbles and the input editor."
  ([point transcript-ranges input-ranges]
   (source-at-point point transcript-ranges input-ranges nil))
  ([point transcript-ranges input-ranges opts]
   (cond (point-in-ranges? point input-ranges opts) :input
         (point-in-ranges? point transcript-ranges opts) :transcript
         :else nil)))

(defn double-click?
  "True when `point` is a second click on the same source row in time.

   `last-click` is a map with `:source`, `:point`, and `:time-ms`. Columns are
   intentionally ignored: line selection should tolerate small horizontal mouse
   movement between clicks, while requiring the same row prevents a rapid click
   on a neighbouring line from selecting the wrong text."
  [last-click now-ms source point threshold-ms]
  (let [elapsed (- (long now-ms) (long (:time-ms last-click 0)))]
    (boolean (and last-click
                  (= source (:source last-click))
                  (<= 0 elapsed (long (max 0 (long threshold-ms))))
                  (= (:row point) (get-in last-click [:point :row]))))))

(defn screen->document-point
  "Convert a visible screen point into a transcript document point.

   Document rows are relative to the top of the full message scrollback, not
   the terminal. Keeping drag anchors in this coordinate space makes selection
   stay attached to content while the viewport auto-scrolls underneath it."
  [{:keys [col row]} {:keys [viewport-top eff-scroll]}]
  (point col (+ (long (or eff-scroll 0)) (- (long row) (long (or viewport-top 0))))))

(defn line-selection-at-point
  "Expand a click on a selectable screen row to the whole logical line.

   Wrapped source lines land as several vertically-adjacent selectable ranges
   that share a `:line-id`; this spans every such fragment, so a double-click on
   any visual row of a soft-wrapped line (e.g. a long error) selects the entire
   line, not just the clicked fragment. Untagged ranges expand to their single
   row. Returns a document-space `{:anchor ... :focus ...}` selection, or nil
   when the point is only in padding/chrome. The end point is inclusive to match
   `selected-ranges` semantics."
  [{:keys [col row]} selectable-ranges viewport]
  (let
    [col
     (long col)

     row
     (long row)

     hit
     (some (fn [{range-col :col range-row :row width :width :as r}]
             (when (and (= row (long range-row))
                        (>= col (long range-col))
                        (< col (+ (long range-col) (long width))))
               r))
           selectable-ranges)]

    (when hit
      (let
        [gid
         (:line-id hit)

         group
         (if (some? gid) (filterv #(= gid (:line-id %)) selectable-ranges) [hit])

         first-r
         (apply min-key :row group)

         last-r
         (apply max-key :row group)]

        {:anchor (screen->document-point (point (:col first-r) (:row first-r)) viewport)
         :focus (screen->document-point (point (dec (+ (long (:col last-r)) (long (:width last-r))))
                                               (:row last-r))
                                        viewport)}))))

(defn document->screen-selection
  "Project a document-space selection into the current screen viewport."
  [{:keys [anchor focus]} {:keys [viewport-top eff-scroll]}]
  (let
    [viewport-top
     (long (or viewport-top 0))

     eff-scroll
     (long (or eff-scroll 0))

     ->screen
     (fn [{:keys [col row]}]
       (point col (+ viewport-top (- (long row) eff-scroll))))]

    {:anchor (->screen anchor) :focus (->screen focus)}))

(defn auto-scroll-step
  "Return `{:direction :up|:down :amount n}` while a drag-selection point is in
   a viewport edge zone, otherwise nil.

   The amount ramps up toward the outermost row so selecting at the very top or
   bottom of the messages viewport scrolls faster than hovering near the middle
   of the edge zone."
  [{:keys [row]} {:keys [top bottom edge-size max-step]}]
  (let
    [row
     (long row)

     top
     (long top)

     bot
     (long bottom)

     edge
     (long (max 0 (long (or edge-size 1))))

     max-step
     (long (max 1 (long (or max-step edge 1))))]

    (when (and (< top bot) (pos? edge))
      (cond (< row (+ top edge)) (let [distance (max 0 (- row top))]
                                   {:direction :up
                                    :amount (min max-step (max 1 (- edge distance)))})
            (>= row (- bot edge)) (let [distance (max 0 (- (dec bot) row))]
                                    {:direction :down
                                     :amount (min max-step (max 1 (- edge distance)))})))))

(defn auto-scroll-direction
  "Return `:up`, `:down`, or nil while a drag-selection point is at a
   viewport edge.

   `top` is inclusive, `bottom` is exclusive, and `edge-size` is the number of
   rows at either edge that should trigger auto-scroll."
  [point opts]
  (:direction (auto-scroll-step point opts)))

(defn- row-cells
  [row]
  (cond (string? row) (mapv str row)
        (sequential? row) (mapv #(str (or % " ")) row)
        :else []))

(def ^:private box-horizontal-codepoints
  ;; U+25xx box-drawing chars whose visual is a horizontal stroke.
  #{0x2500 0x2501 0x2504 0x2505 0x2508 0x2509 0x254C 0x254D 0x2550 0x2574 0x2576 0x2578 0x257A
    0x257C 0x257E})

(def ^:private box-vertical-codepoints
  ;; U+25xx box-drawing chars whose visual is a vertical stroke.
  #{0x2502 0x2503 0x2506 0x2507 0x250A 0x250B 0x254E 0x254F 0x2551 0x2575 0x2577 0x2579 0x257B
    0x257D 0x257F})

(defn- translate-box-chars
  "Map box-drawing glyphs (┌─┬│├┼┘ ...) to ASCII so a copied table
   pastes back as readable plaintext instead of stray U+25xx bytes.
   Horizontals -> `-`, verticals -> `|`, every other corner/junction
   in the box-drawing block -> `+`."
  [^String s]
  (str/replace s
               #"[\u2500-\u257F]"
               (fn [^String m]
                 (let [cp (int (.charAt m 0))]
                   (cond (box-horizontal-codepoints cp) "-"
                         (box-vertical-codepoints cp) "|"
                         :else "+")))))

(defn- box-border-only?
  "True when a line is just box-drawing chrome + whitespace, i.e. a
   top/middle/bottom border row of a rendered :table. Those rows are
   meaningless once translated to ASCII and only inflate the paste."
  [^String s]
  (and (pos? (.length s)) (boolean (re-matches #"[\s\-\|\+]+" s)) (boolean (re-find #"[\-\+]" s))))

(defn- clean-copied-line
  [s]
  (let
    [out (-> (or s "")
             ;; Drop terminal styling/control sequences. Whole-bubble copy
             ;; can carry actual ESC bytes; pasting those through Lanterna
             ;; may turn them into the visible control-picture glyph `␛`,
             ;; so strip both forms.
             (str/replace #"(?:\u001B|\u241B)\][^\u0007\u001B\u241B]*(?:\u0007|(?:\u001B|\u241B)\\)"
                          "")
             (str/replace #"(?:\u001B|\u241B)\[[0-?]*[ -/]*[@-~]" "")
             ;; Defensive: if an external paste path dropped ESC but left a
             ;; bare SGR tail, do not put color fragments back into prompts.
             (str/replace #"\[[0-9;:]*m" "")
             (str/replace #"[\u200B-\u200D\u2060-\u206F\uFEFF\uE000-\uE02C\uE110-\uE119]" "")
             (str/replace #"[\u0000-\u0008\u000B-\u001F\u007F]" "")
             ;; Translate :table box-drawing chrome to ASCII so a copied
             ;; cell range pastes back as `| col | col |` rather than a
             ;; soup of stray U+25xx bytes.
             (translate-box-chars)
             (str/replace #" +$" ""))]
    ;; Drop pure-border rows entirely. A whole-bubble copy of a 3-row
    ;; table produces 7 rendered lines (top + th + sep + td + ... + bot);
    ;; collapsing the 3 border-only ones leaves the content rows aligned
    ;; and readable on paste.
    (if (box-border-only? out) "" out)))

(defn clean-copied-text
  "Strip render-only markers and terminal control styling from copied text.

   Used by mouse selection and whole-bubble copy. The output should be safe to
   paste back into Vis without ANSI SGR / OSC fragments reaching the prompt or
   the model."
  [text]
  (->> (str/split (str/replace (or text "") #"\r\n?" "\n") #"\n" -1)
       (map clean-copied-line)
       (str/join "\n")))

(defn selected-text
  "Extract selected visible text from `screen-cells`.

   `screen-cells` is a vector of rows; each row may be a string or a vector of
   per-cell strings. Trailing screen padding spaces are stripped per copied
   line, while leading indentation inside the selected span is preserved.

   Optional `selectable-ranges` restricts extraction to bubble cells only."
  ([screen-cells selection] (selected-text screen-cells selection nil))
  ([screen-cells selection selectable-ranges]
   (let
     [rows
      (vec (or screen-cells []))

      row-count
      (count rows)

      cols
      (long (or (some-> rows
                        first
                        row-cells
                        count)
                0))

      ranges
      (selected-ranges selection cols row-count selectable-ranges)]

     (->> ranges
          (map (fn [{:keys [row col width]}]
                 (let
                   [cells
                    (row-cells (nth rows row []))

                    from
                    (min (count cells) (long col))

                    to
                    (min (count cells) (+ from (long width)))]

                   (clean-copied-text (apply str (subvec cells from to))))))
          (str/join "\n")))))
