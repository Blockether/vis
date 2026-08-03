(ns com.blockether.vis.ext.channel-tui.table-test
  "The `vis-table` viewer, end to end on the TUI side: the pure CSV primitives
   (parse, measure, align, filter, sort, render), the grid a `vis-table` fence
   paints inline in the transcript, and the spreadsheet dialog a click on that
   grid opens.

   Everything except the paint is a pure function of immutable data, so the
   viewer's behaviour is pinned WITHOUT a terminal; the one paint test drives a
   real Lanterna virtual terminal and reads its back-buffer."
  (:require [clojure.string :as str]
            [lazytest.core :refer [defdescribe expect it]]
            [com.blockether.vis.ext.channel-tui.dialogs :as dlg]
            [com.blockether.vis.ext.channel-tui.render :as render]
            [com.blockether.vis.ext.channel-tui.table :as table]
            [com.blockether.vis.ext.channel-tui.terminals :as term])
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

(defdescribe csv-filter-test
             (it "matches any cell, case-insensitively"
                 (expect (= [["yak" "10" "second"]] (table/filter-csv-rows (rest grid) "YAK")))
                 (expect (= [["yak" "10" "second"]] (table/filter-csv-rows (rest grid) "second"))))
             (it "a blank query keeps every row"
                 (expect (= 3 (count (table/filter-csv-rows (rest grid) ""))))
                 (expect (= 3 (count (table/filter-csv-rows (rest grid) "  ")))))
             (it "no match is an empty result, never the whole sheet"
                 (expect (= [] (table/filter-csv-rows (rest grid) "nope")))))

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

(defn- table-render
  [body]
  (render/format-answer-markdown-data
    [:ast {} [:code {:lang "vis-table"} body]]
    76
    {:session-id "s1" :session-turn-id "t1" :detail-expansions {} :section :user}))

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
                 (expect (str/includes? (rendered (fence 11)) "1 more row —"))))

;;; ── the table dialog ─────────────────────────────────────────────────────────

(defn- component [] (dlg/table-modal-component "fleet.csv" grid))

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

(defn- typing [c] (KeyStroke. (Character/valueOf c) false false false))

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
  (it "typing filters over EVERY column and retitles with the ratio"
      (let
        [c
         (component)

         st
         (press c (:init c) [(typing \y)])

         m
         (measure c st)]

        (expect (= "y" (:query st)))
        (expect (= 1 (:total m)))
        (expect (= [["yak" "10" "second"]] (:visible m)))
        (expect (str/includes? (:title m) "1/3 rows"))))
  (it "backspace restores the hidden rows"
      (let
        [c
         (component)

         st
         (press c (:init c) [(typing \y) (special KeyType/Backspace)])]

        (expect (= "" (:query st)))
        (expect (= 3 (:total (measure c st))))))
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
  (it "keeps a filtered selection inside the shrunken list"
      (let
        [c
         (component)

         st
         (press c (:init c) [(special KeyType/ArrowDown) (special KeyType/ArrowDown) (typing \y)])

         m
         (measure c st)]

        (expect (= 1 (:total m)))
        (expect (zero? (long (:selected ((:reconcile c) st m))))))))

(defdescribe
  table-dialog-paint-test
  (it
    "paints the filter field, the bordered grid and the hint bar"
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
             (expect (str/includes? txt "filter rows"))
             (expect (str/includes? txt "name"))
             (expect (str/includes? txt "ada"))
             (expect (str/includes? txt "120"))
             (expect (str/includes? txt "Esc"))
             (expect (some #(str/includes? % "─") rows)))
           (finally (.stopScreen screen))))))
