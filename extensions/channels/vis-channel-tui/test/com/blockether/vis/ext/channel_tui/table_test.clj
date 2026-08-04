(ns com.blockether.vis.ext.channel-tui.table-test
  "The `vis-table` viewer, end to end on the TUI side: the pure CSV primitives
   (parse, measure, align, page, sort, render), the grid a `vis-table` fence
   paints inline in the transcript, and the spreadsheet dialog a click on that
   grid opens.

   Everything except the paint is a pure function of immutable data, so the
   viewer's behaviour is pinned WITHOUT a terminal; the one paint test drives a
   real Lanterna virtual terminal and reads its back-buffer."
  (:require [clojure.string :as str]
            [lazytest.core :refer [defdescribe expect it]]
            [com.blockether.vis.ext.channel-tui.cinema :as cinema]
            [com.blockether.vis.ext.channel-tui.dialogs :as dlg]
            [com.blockether.vis.ext.channel-tui.render :as render]
            [com.blockether.vis.ext.channel-tui.table :as table]
            [com.blockether.vis.ext.channel-tui.terminals :as term]
            [com.blockether.vis.ext.channel-tui.virtual :as virtual]
            [com.blockether.vis.internal.loop :as vloop]
            [com.blockether.vis.internal.render :as ast])
  (:import [com.googlecode.lanterna.input KeyStroke KeyType]
           [com.googlecode.lanterna.screen TerminalScreen]
           [com.googlecode.lanterna.terminal.virtual DefaultVirtualTerminal]))

(def ^:private csv-text "name,qty,note\nada,9,first\nyak,10,second\nzed,120,third\n")

(def ^:private grid (table/parse-csv csv-text))

;;; ── pure CSV primitives ──────────────────────────────────────────────────────

(defdescribe
  parse-csv-test
  (it "reads a plain sheet into padded row vectors"
      (expect (= [["name" "qty" "note"] ["ada" "9" "first"] ["yak" "10" "second"]
                  ["zed" "120" "third"]]
                 grid)))
  (it "honours quoted fields, embedded separators and doubled quotes"
      (expect (= [["a" "b"] ["x, y" "say \"hi\""]]
                 (table/parse-csv "a,b\n\"x, y\",\"say \"\"hi\"\"\"\n"))))
  (it "keeps a newline INSIDE a quoted field"
      (expect (= [["a" "b"] ["one\ntwo" "3"]] (table/parse-csv "a,b\n\"one\ntwo\",3\n"))))
  (it "pads a ragged row so (nth row i) is total"
      (expect (= [["a" "b" "c"] ["1" "" ""]] (table/parse-csv "a,b,c\n1\n"))))
  (it "normalizes CRLF" (expect (= [["a" "b"] ["1" "2"]] (table/parse-csv "a,b\r\n1,2\r\n"))))
  (it "reads empty text as an empty grid"
      (expect (= [] (table/parse-csv "")))
      (expect (= [] (table/parse-csv nil)))))

(defdescribe csv-width-test
             (it "natural width covers content plus borders"
                 ;; 4 + 3 + 6 content, 3 columns of decoration, 1 closing border
                 (expect (= 23 (table/csv-natural-width grid))))
             (it "one essay-length cell cannot push the other columns off the grid"
                 (expect (= (+ (long table/csv-max-col-width) 4)
                            (table/csv-natural-width [["x"] [(apply str (repeat 100 "y"))]]))))
             (it "a roomy table keeps natural widths"
                 (expect (= [4 3 6] (table/csv-widths grid 100))))
             (it "a cramped table shrinks to fit"
                 (let [ws (table/csv-widths grid 20)]
                   (expect (<= (count (table/boxed-border-line ws :top)) 20))
                   (expect (every? pos? ws))))
             (it "stretching FILLS the pane exactly"
                 (let [ws (table/csv-stretch-widths (table/csv-widths grid 60) 60)]
                   (expect (= 60 (count (table/boxed-border-line ws :top))))))
             (it "stretching never shrinks a grid already wider than the pane"
                 (let [ws (table/csv-widths grid 100)]
                   (expect (= ws (table/csv-stretch-widths ws 5))))))

(defdescribe csv-number-test
             (it "reads formatting as formatting, not as text"
                 (expect (= 1234.5 (table/csv-number "1,234.5")))
                 (expect (= 12.0 (table/csv-number "$12")))
                 (expect (= 45.0 (table/csv-number "45%")))
                 (expect (= 1234.0 (table/csv-number "1 234"))))
             (it "is nil for anything that is not a number"
                 (expect (nil? (table/csv-number "abc")))
                 (expect (nil? (table/csv-number "")))
                 (expect (nil? (table/csv-number nil)))))

(defdescribe csv-align-test
             (it "right-aligns a column whose every data cell is a number"
                 (expect (table/numeric-column? (rest grid) 1))
                 (expect (not (table/numeric-column? (rest grid) 0)))
                 (expect (= [:left :right :left] (table/csv-aligns grid))))
             (it "ignores the header when deciding (a numeric column is titled with words)"
                 (expect (= [:right] (table/csv-aligns [["qty"] ["1"] ["2"]])))))

(defdescribe csv-page-test
             (it "counts pages, and an empty sheet still has page 1/1"
                 (expect (= 3 (table/page-count 60 25)))
                 (expect (= 2 (table/page-count 50 25)))
                 (expect (= 1 (table/page-count 0 25))))
             (it "maps a row to its page and back to that page's first row"
                 (expect (= 0 (table/page-index 24 25)))
                 (expect (= 1 (table/page-index 25 25)))
                 (expect (= 25 (table/page-start 40 25)))
                 (expect (= 0 (table/page-start 3 25))))
             (it "a page-size of zero never divides by zero"
                 (expect (= 1 (table/page-count 0 0)))
                 (expect (= 3 (table/page-start 3 0)))))

(defdescribe
  csv-sort-test
  (it "sorts a numeric column NUMERICALLY — 9 before 10 before 120"
      (expect (= ["9" "10" "120"] (mapv #(nth % 1) (table/sort-csv-rows (rest grid) 1 :asc))))
      (expect (= ["120" "10" "9"] (mapv #(nth % 1) (table/sort-csv-rows (rest grid) 1 :desc)))))
  (it "sorts text case-insensitively"
      (expect (= ["ada" "yak" "zed"] (mapv first (table/sort-csv-rows (rest grid) 0 :asc))))
      (expect (= ["a" "B" "c"] (mapv first (table/sort-csv-rows [["c"] ["a"] ["B"]] 0 :asc)))))
  (it "sorts blanks last"
      (expect (= ["a" "b" ""] (mapv first (table/sort-csv-rows [["b"] [""] ["a"]] 0 :asc))))))

(defdescribe csv-grid-lines-test
             (it "renders top border, header, rule, one line per data row and a bottom border"
                 (let [lines (table/csv-grid-lines grid 60)]
                   (expect (= 7 (count lines)))
                   (expect (str/includes? (nth lines 1) "name"))
                   (expect (str/includes? (nth lines 3) "ada"))
                   (expect (apply = (map count lines)))))
             (it "accepts a replacement header (the sort arrow the dialog paints)"
                 (let [lines (table/csv-grid-lines grid 60 {:header ["N ▲" "Q" "T"]})]
                   (expect (str/includes? (nth lines 1) "N ▲"))
                   (expect (apply = (map count lines)))))
             (it "a numeric column's label sits over its digits, not at the far left"
                 (let
                   [ws
                    (table/csv-stretch-widths (table/csv-widths grid 60) 60)

                    lines
                    (table/csv-grid-lines grid 60 {:widths ws})

                    ends
                    (fn [line s]
                      (+ (long (str/index-of line s)) (count s)))]

                   ;; lines: top, header, rule, ada, yak, zed
                   (expect (= (ends (nth lines 1) "qty") (ends (nth lines 5) "120")))
                   ;; a text column is still left-aligned, head and cells alike
                   (expect (= (str/index-of (nth lines 1) "note")
                              (str/index-of (nth lines 5) "third")))))
             (it "is nil for an empty grid" (expect (nil? (table/csv-grid-lines [] 60)))))

;;; ── the inline `vis-table` fence ─────────────────────────────────────────────

(defn- fence
  "The block `vis_attach` emits for a CSV artifact: five header lines, then the
   payload."
  [n-rows]
  (str/join "\n"
            (into [(str "[Table: fleet.csv " n-rows " rows × 3 cols, 64 B]") "fleet.csv" "text/csv"
                   (str "3x" n-rows) "64 B" "name,qty,note"]
                  (map #(str "row" % "," % ",note" %) (range n-rows)))))

(defn- markdown-render
  "The transcript path, end to end: raw Markdown text through the REAL parse the
   TUI runs on every message, then the painter."
  [md]
  (render/format-answer-markdown-data
    (ast/markdown->ast md)
    76
    {:session-id "s1" :session-turn-id "t1" :detail-expansions {} :section :user}))

(defn- table-render
  "One fence body, rendered the way the transcript renders it — wrapped in the
   four backticks `vis_attach` prints, never as a hand-built code node."
  [body]
  (markdown-render (str "````vis-table\n" body "\n````\n")))

(defn- plain
  "Human-visible text of a rendered line: ANSI colour and zero-width sentinels
   dropped."
  [line]
  (-> (str line)
      (str/replace #"\u001b\[[0-9;]*m" "")
      (str/replace #"[\u200B-\u200F\u2060-\u206F\uFEFF\uE000-\uF8FF]" "")))

(defn- rendered
  "Every painted line of the fence as plain text, joined."
  [body]
  (str/join "\n" (map plain (:lines (table-render body)))))

(defdescribe vis-table-fence-test
             (it "paints the caption and a real grid, and hangs the whole CSV off every row"
                 (let
                   [r
                    (table-render (fence 3))

                    texts
                    (mapv plain (:lines r))

                    joined
                    (str/join "\n" texts)

                    tbls
                    (keep :table (:line-meta r))]

                   (expect (str/includes? joined "[Table: fleet.csv 3 rows × 3 cols, 64 B]"))
                   (expect (str/includes? joined "name"))
                   (expect (str/includes? joined "row0"))
                   (expect (str/includes? joined "note2"))
                   ;; the grid is bordered, not a code dump
                   (expect (some #(str/includes? % "─") texts))
                   ;; every painted grid row is clickable: the dialog opens on the
                   ;; WHOLE dataset, not on the preview
                   (expect (seq tbls))
                   (expect (= "fleet.csv" (:name (first tbls))))
                   (expect (= "fleet.csv" (:title (first tbls))))
                   (expect (= 3 (:cols (first tbls))))
                   (expect (= 3 (:rows (first tbls))))
                   (expect (str/includes? (:csv (first tbls)) "row2"))))
             (it "previews the first rows and says how many are left"
                 (let [joined (rendered (fence 15))]
                   (expect (str/includes? joined "row9"))
                   (expect (not (str/includes? joined "row12")))
                   (expect (str/includes? joined "5 more rows"))))
             (it "a one-row overflow says row, not rows"
                 (expect (str/includes? (rendered (fence 11)) "1 more row —")))
             (it "the headline titles the card: same chip as the grid, and clickable"
                 (let
                   [r
                    (table-render (fence 3))

                    texts
                    (mapv plain (:lines r))

                    idx-of
                    (fn [pred]
                      (first (keep-indexed (fn [i t]
                                             (when (pred (str/trim t)) i))
                                           texts)))

                    caption
                    (idx-of #(str/includes? % "[Table:"))

                    top
                    (idx-of #(str/starts-with? % "┌"))]

                   ;; the grid starts on the very next row — one card, not a
                   ;; caption paragraph with a code block loose underneath it
                   (expect (= (inc (long caption)) (long top)))
                   ;; and the headline opens the sheet like any grid row does
                   (expect (some? (:table (nth (:line-meta r) caption))))))
             (it "the grid FILLS the card instead of leaving dead fill on the right"
                 (let
                   [texts
                    (mapv (comp str/trim plain) (:lines (table-render (fence 3))))

                    bottom
                    (first (filter #(str/starts-with? % "└") texts))]

                   ;; natural widths would draw a ~25-column grid inside a card
                   ;; three times that wide; stretched, the border is the widest
                   ;; thing painted
                   (expect (some? bottom))
                   (expect (>= (count bottom) 60))
                   (expect (= (count bottom) (apply max (map count texts)))))))

;;; ── the card reads as a SHEET, not as a code dump ────────────────────────────

(defn- card-roles
  "The part each painted line of the card plays, `nil` for the chip's own pad and
   blank rows."
  [body]
  (mapv :table-line (:line-meta (table-render body))))

(defn- paint-card
  "Paint one fence into a REAL Lanterna virtual terminal the way the transcript
   paints it, then call `f` with the plain text of every terminal row and a
   `(cell col row)` reader over the back buffer — colours included, since the
   whole point of the card's chrome is what a human SEES."
  [body f]
  (let
    [vt
     (term/virtual-screen)

     ^TerminalScreen screen
     (:screen vt)

     cols
     (.getColumns (.getTerminalSize screen))

     msg
     (virtual/project-message {:role :assistant
                               :session-id "s1"
                               :session-turn-id "t1"
                               :text (str "````vis-table\n" body "\n````\n")}
                              (- (long cols) 4)
                              {}
                              {:session-id "s1"})]

    (try (render/draw-chat-bubble! (.newTextGraphics screen) msg 1 2 (- (long cols) 4))
         (.refresh screen)
         (f (term/grid (:terminal vt))
            (fn [col row]
              (cinema/cell (.getBackCharacter screen (int col) (int row)))))
         (finally (.stopScreen screen)))))

(defn- lum
  "Relative luminance of a captured cell colour `[r g b]`, so a contrast claim can
   be made about the THEME's direction instead of one palette's literal RGB."
  [[r g b]]
  (/ (+ (* 0.2126 (double r)) (* 0.7152 (double g)) (* 0.0722 (double b))) 255.0))

(defdescribe
  vis-table-card-chrome-test
  (it "tags every line of the card with the part it plays"
      (let [roles (vec (remove nil? (card-roles (fence 12))))]
        (expect (= :title (first roles)))
        (expect (= [:border :head :border] (subvec roles 1 4)))
        ;; ten preview rows, striped in turn - the zebra IS the row separator
        (expect (= (take 10 (cycle [:row :row-alt])) (subvec roles 4 14)))
        (expect (= [:border :hint] (subvec roles 14)))))
  (it
    "paints two backgrounds down the data rows, and an INVERTED header band"
    (paint-card
      (fence 12)
      (fn [rows cell]
        (let
          [row-of
           (fn [needle]
             (first (keep-indexed (fn [i r]
                                    (when (str/includes? r needle) i))
                                  rows)))

           y0
           (row-of "row0")

           y1
           (row-of "row1")

           yh
           (row-of "name")

           text-col
           (fn [y needle]
             (str/index-of (nth rows y) needle))

           c0
           (cell (text-col y0 "row0") y0)

           c1
           (cell (text-col y1 "row1") y1)

           ch
           (cell (text-col yh "name") yh)]

          (expect (some? y0))
          (expect (some? y1))
          ;; consecutive rows do NOT share a background: that is the stripe
          (expect (not= (:bg c0) (:bg c1)))
          ;; the header is NOT one more stripe: it gets its own band, far
          ;; from the card's paper and from the zebra alike
          (expect (not= (:bg ch) (:bg c0)))
          (expect (not= (:bg ch) (:bg c1)))
          (expect (> (abs (- (lum (:bg ch)) (lum (:bg c0)))) 0.3))
          ;; and INVERTED: the body is dark ink on light paper, the header
          ;; light letters on a dark strip - on a dark theme the exact
          ;; mirror of that. Either way the contrast runs the OTHER way.
          (expect (not= (pos? (- (lum (:bg c0)) (lum (:fg c0))))
                        (pos? (- (lum (:bg ch)) (lum (:fg ch))))))
          ;; header bold, body not - the column names read as labels
          (expect (:bold ch))
          (expect (not (:bold c0)))
          ;; chrome is muted: the borders are not painted in the cell ink
          (expect (not= (:fg (cell (str/index-of (nth rows y0) "│") y0)) (:fg c0)))))))
  ;; Regression: the band used to be filled across the WHOLE row, frame columns
  ;; included. A terminal cell is tinted whole and `│` runs down the middle of
  ;; its own cell, so every other row grew a half-cell tab of colour sticking
  ;; out past the table's left and right edges.
  (it
    "keeps the band INSIDE the frame - the border columns stay on the card"
    (paint-card
      (fence 12)
      (fn [rows cell]
        (let
          [row-of
           (fn [needle]
             (first (keep-indexed (fn [i r]
                                    (when (str/includes? r needle) i))
                                  rows)))

           ;; a striped row and its neighbour, which carries the card's own bg
           y1
           (row-of "row1")

           y0
           (row-of "row0")

           line
           (nth rows y1)

           left
           (str/index-of line "│")

           right
           (str/last-index-of line "│")

           card-bg
           (:bg (cell left y0))]

          (expect (< (long left) (long right)))
          ;; the frame's own columns are painted like the rest of the card…
          (expect (= card-bg (:bg (cell left y1))))
          (expect (= card-bg (:bg (cell right y1))))
          ;; …while everything between them carries the stripe
          (expect (not= card-bg (:bg (cell (inc (long left)) y1))))
          (expect (not= card-bg (:bg (cell (dec (long right)) y1))))
          ;; and the header band is fenced in exactly the same way
          (let [yh (row-of "name")]
            (expect (= card-bg (:bg (cell left yh))))
            (expect (not= card-bg (:bg (cell (inc (long left)) yh))))))))))

;;; ── the table dialog ─────────────────────────────────────────────────────────

(defn- component [] (dlg/table-modal-component "fleet.csv" grid))

;; A sheet nobody can see at once: what paging exists for.
(def ^:private big-grid
  (table/parse-csv (apply str
                     "n,label\n"
                     (for [i (range 60)]
                       (str i ",row-" i "\n")))))

(defn- big-component [] (dlg/table-modal-component "events.csv" big-grid))

(defn- measure [component state] ((:measure component) state 80 30))

(defn- press
  "Apply `keys` to `component` the way `run-modal!` does: measure, reconcile,
   then hand the key to `:on-key`. Stops early on a `::dlg/done` result."
  [component state keys]
  (reduce (fn [st k]
            (if (contains? st ::dlg/done)
              (reduced st)
              (let [m (measure component st)]
                ((:on-key component) ((:reconcile component) st m) k m))))
          state
          keys))

(defn- special [t] (KeyStroke. t))

(defdescribe
  table-modal-component-test
  (it "opens on every row, titled with its shape"
      (let
        [c
         (component)

         m
         (measure c (:init c))]

        (expect (= 3 (:total m)))
        (expect (str/includes? (:title m) "fleet.csv"))
        (expect (str/includes? (:title m) "3 rows × 3 cols"))
        (expect (= 3 (count (:widths m))))
        (expect (= [:left :right :left] (:aligns m)))))
  (it "a sheet that fits on one page carries no page counter"
      (let [c (component)]
        (expect (not (str/includes? (:title (measure c (:init c))) "page")))
        (expect (= 1 (:pages (measure c (:init c)))))))
  (it "PgDn turns a WHOLE page: the cursor lands on its first row and the title counts"
      (let
        [c
         (big-component)

         m0
         (measure c (:init c))

         page-size
         (long (:list-h m0))

         st
         (press c (:init c) [(special KeyType/PageDown)])

         m
         (measure c st)]

        (expect (< page-size 60))
        (expect (str/includes? (:title m0) (str "page 1/" (:pages m0))))
        (expect (= page-size (:selected st)))
        (expect (= 1 (:page m)))
        (expect (= page-size (:scroll ((:reconcile c) st m))))
        (expect (str/includes? (:title m) (str "page 2/" (:pages m))))))
  (it "PgUp comes back and neither page key walks off the sheet"
      (let
        [c
         (big-component)

         back
         (press c (:init c) [(special KeyType/PageDown) (special KeyType/PageUp)])

         top
         (press c (:init c) (repeat 9 (special KeyType/PageUp)))

         end
         (press c (:init c) (repeat 9 (special KeyType/PageDown)))]

        (expect (zero? (long (:selected back))))
        (expect (zero? (long (:selected top))))
        (expect (= 59 (:selected end)))))
  (it "Home and End jump to the first and last row"
      (let
        [c
         (big-component)

         end
         (press c (:init c) [(special KeyType/End)])]

        (expect (= 59 (:selected end)))
        (expect (zero? (long (:selected (press c end [(special KeyType/Home)])))))))
  (it "↑/↓ move the row cursor and stop at the ends"
      (let
        [c
         (component)

         down
         (press c (:init c) [(special KeyType/ArrowDown)])]

        (expect (= 1 (:selected down)))
        (expect (= 2 (:selected (press c (:init c) (repeat 6 (special KeyType/ArrowDown))))))
        (expect (zero?
                  (long (:selected
                          (press c down [(special KeyType/ArrowUp) (special KeyType/ArrowUp)])))))))
  (it "←/→ move the column cursor, which the header marks"
      (let
        [c
         (component)

         st
         (press c (:init c) [(special KeyType/ArrowRight)])]

        (expect (= 1 (:col st)))
        (expect (str/includes? (nth (:head-cells (measure c st)) 1) "▸"))
        (expect (zero? (long (:col (press c (:init c) [(special KeyType/ArrowLeft)])))))))
  (it "Enter sorts by the cursor column and pressing it again flips the direction"
      (let
        [c
         (component)

         asc
         (press c (:init c) [(special KeyType/Enter)])

         desc
         (press c asc [(special KeyType/Enter)])]

        (expect (= 0 (:sort-idx asc)))
        (expect (= :asc (:sort-dir asc)))
        (expect (= ["ada" "yak" "zed"] (mapv first (:visible (measure c asc)))))
        (expect (= :desc (:sort-dir desc)))
        (expect (= ["zed" "yak" "ada"] (mapv first (:visible (measure c desc)))))
        (expect (str/includes? (nth (:head-cells (measure c desc)) 0) "▼"))))
  (it "sorting a numeric column is numeric — 9, 10, 120"
      (let
        [c
         (component)

         st
         (press c (:init c) [(special KeyType/ArrowRight) (special KeyType/Enter)])]

        (expect (= ["9" "10" "120"] (mapv #(nth % 1) (:visible (measure c st)))))))
  (it "Esc closes with nothing"
      (let
        [c
         (component)

         st
         (press c (:init c) [(special KeyType/Escape)])]

        (expect (contains? st ::dlg/done))
        (expect (nil? (::dlg/done st)))))
  (it "Tab returns the selected row"
      (let
        [c
         (component)

         st
         (press c (:init c) [(special KeyType/ArrowDown) (special KeyType/Tab)])]

        (expect (= ["yak" "10" "second"] (::dlg/done st)))))
  (it "the window snaps to the page holding the cursor, never mid-page"
      (let
        [c
         (big-component)

         page-size
         (long (:list-h (measure c (:init c))))

         st
         (press c (:init c) (repeat page-size (special KeyType/ArrowDown)))

         m
         (measure c st)]

        (expect (= page-size (:selected st)))
        (expect (= 1 (:page m)))
        (expect (= page-size (:scroll ((:reconcile c) st m)))))))

(defdescribe
  table-dialog-paint-test
  (it
    "paints the bordered grid, the page keys and the hint bar"
    (let
      [vt
       (term/virtual-screen)

       ^TerminalScreen screen
       (:screen vt)

       ^DefaultVirtualTerminal terminal
       (:terminal vt)

       c
       (component)

       m0
       (measure c (:init c))

       st
       ((:reconcile c) (:init c) m0)

       m
       (measure c st)]

      (try ((:paint c) (.newTextGraphics screen) st m)
           (.refresh screen)
           (let
             [rows
              (term/grid terminal)

              txt
              (str/join "\n" rows)]

             (expect (str/includes? txt "fleet.csv"))
             (expect (str/includes? txt "PgUp/PgDn"))
             (expect (str/includes? txt "name"))
             (expect (str/includes? txt "ada"))
             (expect (str/includes? txt "120"))
             (expect (str/includes? txt "Esc"))
             (expect (some #(str/includes? % "─") rows)))
           (finally (.stopScreen screen))))))

;;; ── cross-surface: the TUI paints what the ENGINE hands the human ────────────

(def ^:private wire-fence
  "The block `vis_attach` really prints for a CSV artifact — four backticks, five
   header lines, then the payload; `shim-attach-test` pins that shape byte for
   byte on the engine side. The note cell carries a quoted comma: the field a
   naive split tears in half."
  (str "````vis-table\n" "[Table: fleet.csv 2 rows × 3 cols, 64 B] fleet counts\n"
       "fleet.csv\n" "text/csv\n"
       "3x2\n" "64 B\n"
       "name,qty,note\n" "machine-0,7,\"note, with comma\"\n"
       "machine-1,120,plain\n" "````\n"))

(defdescribe
  vis-table-cross-surface-test
  (it "the printed fence survives the REAL Markdown parse as one vis-table node"
      (let
        [codes (filterv #(and (vector? %) (= :code (first %)))
                 (tree-seq vector? seq (ast/markdown->ast wire-fence)))]
        (expect (= 1 (count codes)))
        (expect (= "vis-table" (:lang (second (first codes)))))))
  (it "a quoted comma stays ONE cell from the fence to the painted grid"
      (let
        [r
         (markdown-render wire-fence)

         joined
         (str/join "\n" (map plain (:lines r)))

         csv
         (:csv (first (keep :table (:line-meta r))))]

        (expect (str/includes? joined "note, with comma"))
        (expect (= ["machine-0" "7" "note, with comma"] (nth (table/parse-csv csv) 1)))))
  (it "the body the engine hands the human is the fence VERBATIM, and it paints"
      (let
        [card
         (#'vloop/tool-result-display {:stdout wire-fence} "python_execution" {})

         tbls
         (keep :table (:line-meta (markdown-render (str (:body card)))))]

        (expect (= wire-fence (str (:body card))))
        (expect (= "fleet.csv" (:name (first tbls))))
        (expect (= 2 (:rows (first tbls))))))
  (it "the model-facing text keeps the headline, loses the rows, paints no grid"
      (let [wire (#'vloop/elide-table-fences wire-fence)]
        (expect (str/includes? wire "[Table: fleet.csv 2 rows × 3 cols, 64 B] fleet counts"))
        (expect (str/includes? wire "vis_read_attachment"))
        (expect (not (str/includes? wire "machine-0")))
        (expect (not (str/includes? wire "````")))
        ;; and what the model reads is prose, not a clickable grid
        (expect (empty? (keep :table (:line-meta (markdown-render wire)))))))
  (it "clicking the preview opens the dialog on the WHOLE sheet"
      (let
        [{:keys [name csv]}
         (first (keep :table (:line-meta (table-render (fence 15)))))

         c
         (dlg/table-modal-component name (table/parse-csv csv))

         m
         (measure c (:init c))]

        ;; the transcript previews 10 rows; the sheet behind it has all 15
        (expect (= "fleet.csv" name))
        (expect (= 15 (:total m)))
        (expect (str/includes? (:title m) "15 rows × 3 cols"))))
  (it "a grid keeps its click meta even when the mid-scroll window asks for one"
      ;; The windowed fast path emits no line meta at all, so a grid painted
      ;; through it would look right and do nothing on click.
      (let
        [msg
         {:role :assistant :text wire-fence}

         windowed
         (virtual/project-message msg 76 {} {:session-id "s1" :window-start 0 :window-num 12})]

        (expect (seq (keep :table (:line-meta windowed))))
        (expect (= "fleet.csv" (:name (first (keep :table (:line-meta windowed)))))))))
