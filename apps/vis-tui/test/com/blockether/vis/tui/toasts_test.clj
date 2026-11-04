(ns com.blockether.vis.tui.toasts-test
  (:require [clojure.string :as str]
            [com.blockether.vis.tui.primitives :as p]
            [com.blockether.vis.tui.toasts :as toasts]
            [lazytest.core :refer [defdescribe describe expect it]])
  (:import [com.googlecode.lanterna TerminalPosition TerminalSize]
           [com.googlecode.lanterna.screen TerminalScreen]
           [com.googlecode.lanterna.terminal.virtual DefaultVirtualTerminal]))

(def ^:private long-text
  (apply str (repeat 40 "The deploy of acme widgets finished with warnings. ")))

(defn- page
  [text & {:keys [index total remaining-ms] :or {index 0 total 1} :as more}]
  {:entry (merge {:id "a" :text text :level :info} (dissoc more :index :total :remaining-ms))
   :index index
   :total total
   :remaining-ms remaining-ms})

(defdescribe
  toast-layout-test
  (describe "fit"
            ;; A card wider than the transcript, or below its last row, paints over the
            ;; scrollbar, the composer or the next terminal line.
            (it "keeps the card, its heading, its close mark and its page counter inside the area"
                (doseq [cols
                        [28 30 56 80 200]

                        rows
                        [8 10 30]

                        total
                        [1 12]

                        titled?
                        [true false]]

                  (let [{:keys [col row width height lines heading pager] :as card}
                        (toasts/layout
                          (if titled?
                            (page long-text :title "Release" :source "forge" :index 10 :total total)
                            (page long-text :index 10 :total total))
                          {:left 2 :top 3 :cols cols :rows rows})]
                    (expect (some? card))
                    (expect (<= (+ col width) (+ 2 cols)))
                    (expect (<= (+ row height) (+ 3 rows)))
                    ;; "│ " text " │"; the "✕" stands in the empty row over the text.
                    (expect (every? #(<= (p/display-width %) (- width 4)) lines))
                    ;; "│ " heading " ✕ │"
                    (expect (<= (+ 6 (p/display-width (or heading ""))) width))
                    ;; "│ " pager " │"
                    (expect (<= (+ 4 (p/display-width (or pager ""))) width)))))
            (it "shows no card in an area narrower than the smallest card"
                (expect (nil? (toasts/layout (page "Copied") {:left 0 :top 0 :cols 27 :rows 20})))))
  (describe "pager"
            ;; User request: with several notices, show "1/N" in the bottom-right corner and
            ;; let the user move between them.
            (it "shows the page counter only when more notices wait"
                (expect (nil? (:pager (toasts/layout (page "Saved")
                                                     {:left 0 :top 0 :cols 80 :rows 9}))))
                (expect (= "2/3"
                           (:pager (toasts/layout (page "Saved" :index 1 :total 3)
                                                  {:left 0 :top 0 :cols 80 :rows 9}))))))
  (describe "countdown"
            ;; User request: a bar shows how long the notice stays.
            (it "gives the part of the time that is left, and none for a sticky notice"
                (expect (= 0.25
                           (:fraction (toasts/layout (page "Saved" :ttl-ms 4000 :remaining-ms 1000)
                                                     {:left 0 :top 0 :cols 80 :rows 9}))))
                (expect (nil? (:fraction (toasts/layout (page "Saved" :ttl-ms nil)
                                                        {:left 0 :top 0 :cols 80 :rows 9}))))))
  (describe "text"
            ;; A long error message must not fill the transcript. The cut must show.
            (it "wraps long text to at most four lines and marks the cut"
                (let [{:keys [lines]} (toasts/layout (page long-text)
                                                     {:left 0 :top 0 :cols 56 :rows 40})]
                  (expect (= 4 (count lines)))
                  (expect (= \… (last (last lines)))))))
  (describe "clicks"
            ;; v0.5.1 native clipboard test: the "Copied bubble" card covered the bubble
            ;; and took the next drag, so the user could not select its text.
            (it "takes clicks only on the close mark and the page counter"
                (let [card
                      (toasts/layout (page "Copied bubble" :index 1 :total 3)
                                     {:left 2 :top 3 :cols 80 :rows 20})

                      regions
                      (toasts/click-regions card)

                      kind-at
                      (fn [col row]
                        (some (fn [{r :row c :col w :width k :kind}]
                                (when (and (= row r) (<= c col) (< col (+ c w))) k))
                              regions))

                      right
                      (+ (:col card) (:width card) -1)

                      pager-row
                      (+ (:row card) (:height card) -2)

                      pager-col
                      (- right 1 (count (:pager card)))]

                  (expect (= :toast-dismiss (kind-at (- right 2) (inc (:row card)))))
                  (expect (= :toast-next (kind-at pager-col pager-row)))
                  (expect (= :toast-next (kind-at (+ pager-col 2) pager-row)))
                  (expect (nil? (kind-at (dec pager-col) pager-row)))
                  (expect (nil? (kind-at (+ 2 (:col card)) (+ 2 (:row card)))))
                  (expect (nil? (kind-at (:col card) pager-row))))))
  (describe
    "placement"
    ;; User request: a global notice stands in the top-left corner of the whole
    ;; window, and a session notice in the top-left corner of the transcript. Both
    ;; keep the same left margin, and an empty row stands between them.
    (it "puts the global card in the window corner and the session card under it, after a gap"
        (let [area
              {:screen-cols 120 :rows 40 :chat-left 0 :cols 120 :text-top 5 :messages-bottom 35}

              both
              (toasts/cards {:global (page "Rate limit") :session (page "Indexed")} area)

              {:keys [global session]}
              both]

          (expect (= [2 0] [(:col global) (:row global)]))
          (expect (= 2 (:col session)))
          (expect (= (+ (:row global) (:height global) 1) (:row session)))
          (expect (= 5 (:row (:session (toasts/cards {:session (page "Indexed")} area)))))
          ;; A pane right of the rail is not under the global card.
          (expect (= 5
                     (:row (:session (toasts/cards {:global (page "Rate limit")
                                                    :session (page "Indexed")}
                                                   (assoc area :chat-left 60)))))))))
  (describe
    "paint"
    ;; User report: the text touched the side of the card, and the side lines
    ;; of one-eighth blocks showed gaps between rows, so the border looked broken.
    (it
      "draws whole side lines with one empty column between them and the text"
      (doseq [titled?
              [true false]

              total
              [1 3]]

        (let [card
              (toasts/layout (if titled?
                               (page long-text :title "Release" :source "forge" :total total)
                               (page long-text :total total :ttl-ms 4000 :remaining-ms 2000))
                             {:left 1 :top 1 :cols 80 :rows 20})

              terminal
              (DefaultVirtualTerminal. (TerminalSize. 70 20))

              screen
              (doto (TerminalScreen. terminal) .startScreen)

              _
              (do (toasts/paint! (.newTextGraphics screen) card false) (.refresh screen))

              cell
              (fn [col row]
                (.getCharacterString (.getCharacter terminal
                                                    (TerminalPosition. (int col) (int row)))))

              {:keys [col row width height pager]}
              card

              right
              (+ col width -1)

              body-rows
              (range (inc row) (+ row height -1))]

          (try (expect (every? #(= "│" (cell col %)) body-rows))
               (expect (every? #(= "│" (cell right %)) body-rows))
               (expect (every? #(str/blank? (cell (inc col) %)) body-rows))
               (expect (every? #(str/blank? (cell (dec right) %)) body-rows))
               ;; User request: the counter sits on the row over the bottom border,
               ;; and a click on it turns the page.
               (when pager
                 (let [{r :row c :col w :width} (first (filter #(= :toast-next (:kind %))
                                                               (toasts/click-regions card)))]
                   (expect (= (+ row height -2) r))
                   (expect (= pager (apply str (map #(cell % r) (range c (+ c w))))))))
               (finally (.stopScreen screen))))))))
