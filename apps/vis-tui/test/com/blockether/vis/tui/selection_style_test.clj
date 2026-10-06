(ns com.blockether.vis.tui.selection-style-test
  (:require [clojure.string :as str]
            [lazytest.core :refer [defdescribe expect it]]
            [com.blockether.vis.tui.boxed-table :as boxed-table]
            [com.blockether.vis.tui.capture :as cap]
            [com.blockether.vis.tui.dialogs :as dlg]
            [com.blockether.vis.tui.primitives :as p]
            [com.blockether.vis.tui.render :as render]
            [com.blockether.vis.tui.table :as table]
            [com.blockether.vis.tui.theme :as t])
  (:import [com.googlecode.lanterna TextColor TextColor$RGB]))

(defn- rgb [^TextColor color] [(.getRed color) (.getGreen color) (.getBlue color)])

(defn- color [r g b] (TextColor$RGB. (int r) (int g) (int b)))

(defn- row-text [row] (apply str (map :ch row)))

(defn- text-cells
  [frame text]
  (first (for [row
               frame

               :let [start
                     (str/index-of (row-text row) text)]
               :when start]

           (subvec row start (+ (long start) (count text))))))

(defn- capture-frame
  [paint!]
  (let [capture (cap/capture! {:cols 80 :rows 28 :paint! paint!})]
    (expect (nil? (:error capture)))
    (last (:frames capture))))

(defdescribe selectable-row-style-test
             (it "uses bold and reversed colors without a marker, on dialog and transient surfaces"
                 (doseq [background
                         [t/dialog-bg t/terminal-bg]

                         paint!
                         [(fn [g row selected?]
                            (dlg/draw-selectable-row! g 1 row 30 selected? "Option"))
                          (fn [g row selected?]
                            (dlg/draw-checkbox-item! g 1 row 30 selected? true "Option"))
                          (fn [g row selected?]
                            (#'dlg/draw-list-item! g 1 row 30 selected? "Option" "M-x"))
                          (fn [g row selected?]
                            (table/draw-line! g 2 row 30 selected? "Option"))
                          (fn [g row selected?]
                            (#'dlg/draw-session-row! g 1 row 30 selected? "Option"))]]

                   (binding [t/dialog-bg background]
                     (let [frame (capture-frame (fn [{:keys [g]}]
                                                  (paint! g 4 true)
                                                  (paint! g 5 false)
                                                  (p/put-str! g 2 6 "Next")))
                           selected (subvec (nth frame 4) 2 32)
                           unselected (subvec (nth frame 5) 2 32)]

                       (expect (= (row-text unselected) (row-text selected)))
                       (expect (str/includes? (row-text selected) "Option"))
                       (doseq [[on off] (map vector selected unselected)]
                         (expect (:bold on))
                         (expect (not (:bold off)))
                         (expect (= (:fg off) (:bg on)))
                         (expect (= (:bg off) (:fg on))))
                       (expect (every? #(not (:bold %)) (text-cells frame "Next"))))))))

(defdescribe
  boxed-selection-style-test
  (it
    "highlights the row interior and keeps the table borders and columns stable"
    (let [frame
          (capture-frame (fn [{:keys [g]}]
                           (boxed-table/draw! g
                                              {:bounds {:left 1 :inner-w 23}
                                               :top 1
                                               :body-h 2
                                               :headers ["Name" "State"]
                                               :widths [10 5]
                                               :total 2
                                               :scroll 0
                                               :selected 0
                                               :cell-fn (constantly ["alpha" "ready"])
                                               :closed? true})))

          selected
          (subvec (nth frame 4) 3 23)

          unselected
          (subvec (nth frame 5) 3 23)]

      (expect (= (row-text selected) (row-text unselected)))
      (expect (= "alpha" (row-text (subvec (nth frame 4) 4 9))))
      (expect (every? :bold selected))
      (expect (every? #(= (rgb t/dialog-fg) (:bg %)) selected))
      (expect (every? #(not (:bold %)) unselected))
      (doseq [row
              [4 5]

              col
              [2 23]]

        (let [cell (get-in frame [row col])]
          (expect (= "│" (:ch cell)))
          (expect (= (rgb t/dialog-bg) (:bg cell)))
          (expect (not (:bold cell))))))))

(defdescribe
  navigator-selection-style-test
  ;; Regression for #320: reverse video turned each span's own ink into its background,
  ;; so the selected session showed several shades instead of one block.
  (it
    "paints one muted background over the whole line, text and padding, without a cursor glyph"
    (let [entry
          {:modified "Today"
           :status "Idle"
           :title "Session"
           :group "Workbench"
           :session-group "Planning"}

          frame
          (capture-frame (fn [{:keys [g]}]
                           (p/set-colors! g t/dialog-fg t/dialog-bg)
                           (p/fill-rect! g 0 0 80 28)
                           (#'dlg/draw-navigator-session! g 2 4 60 entry true)
                           (#'dlg/draw-navigator-session! g 2 7 60 entry false)))

          selection
          (rgb (#'dlg/navigator-selection-bg))

          selected
          (subvec (nth frame 4) 2 62)

          unselected
          (subvec (nth frame 7) 2 62)]

      (expect (not= (rgb t/dialog-bg) selection))
      (expect (= (map :ch unselected) (map :ch selected)))
      (expect (every? #(= selection (:bg %)) selected))
      (expect (every? #(= (rgb t/dialog-bg) (:bg %)) unselected))
      (expect (every? :bold selected))
      (doseq [{:keys [ch fg]}
              selected

              :when (not (str/blank? ch))]

        (expect (<= (double t/legible-contrast)
                    (t/contrast-ratio (apply color fg) (apply color selection))))))))

(defdescribe suggestion-selection-style-test
             (it
               "highlights slash commands and file mentions, including chips, metadata and padding"
               (doseq [file? [false true]]
                 (let [usage (if file? "src/choice.clj" "/choice")
                       label (if file? "1 KiB · today · tracked" "Useful command")
                       suggestion {:slash/usage usage :label label :file/mention? file?}
                       frame (capture-frame (fn [{:keys [g]}]
                                              (render/draw-slash-command-suggestions!
                                                g
                                                [(assoc suggestion :slash/selected? true)
                                                 (assoc suggestion :slash/selected? false)]
                                                20
                                                80)))
                       selected (nth frame 18)
                       unselected (nth frame 19)
                       usage-cells (text-cells [selected] usage)
                       label-cells (text-cells [selected] label)]

                   (expect (= (row-text selected) (row-text unselected)))
                   (expect (not (str/includes? (row-text selected) "•")))
                   (expect (seq usage-cells))
                   (expect (every? :bold usage-cells))
                   (expect (every? #(= (rgb t/code-block-fg) (:bg %)) usage-cells))
                   (expect (every? #(= (rgb t/code-block-bg) (:fg %)) usage-cells))
                   (expect (seq label-cells))
                   (expect (every? :bold label-cells))
                   (expect (every? :italic label-cells))
                   (expect (= (rgb t/dialog-fg) (:bg (nth selected 0))))
                   (expect (:bold (nth selected 0)))
                   (expect (every? #(not (:bold %)) unselected))))))

(defdescribe
  settings-selection-style-test
  (it
    "moves the highlight with the keyboard and preserves toggle status symbols"
    (let [capture
          (with-redefs-fn {#'dlg/load-inventories! (constantly nil)
                           #'dlg/settings-rows
                           (constantly [{:type :toggle :key :first :label "First option"}
                                        {:type :toggle :key :second :label "Second option"}])}
            #(cap/capture! {:cols 80
                            :rows 28
                            :keys [:down :esc]
                            :paint!
                            (fn [{:keys [screen]}]
                              (dlg/settings-dialog! screen {:first true :second false} {}))}))

          frames
          (filter #(text-cells % "First option") (:frames capture))

          first-cells
          (map #(text-cells % "First option") frames)

          second-cells
          (map #(text-cells % "Second option") frames)]

      (expect (nil? (:error capture)))
      (expect (some #(every? :bold %) first-cells))
      (expect (some #(every? (fn [cell]
                               (= (rgb t/dialog-fg) (:bg cell)))
                             %)
                    first-cells))
      (expect (some #(every? :bold %) second-cells))
      (expect (some #(every? (fn [cell]
                               (= (rgb t/dialog-fg) (:bg cell)))
                             %)
                    second-cells))
      (expect (some #(every? (fn [cell]
                               (not (:bold cell)))
                             %)
                    first-cells))
      (expect (some #(every? (fn [cell]
                               (not (:bold cell)))
                             %)
                    second-cells))
      (expect (every? #(and (str/includes? (cap/frame-text %) p/STATUS_ON)
                            (str/includes? (cap/frame-text %) p/STATUS_OFF))
                      frames)))))
