(ns com.blockether.vis.tui.toasts-test
  (:require [com.blockether.vis.tui.primitives :as p]
            [com.blockether.vis.tui.toasts :as toasts]
            [lazytest.core :refer [defdescribe describe expect it]]))

(def ^:private long-text
  (apply str (repeat 40 "The deploy of acme widgets finished with warnings. ")))

(defn- entry [id text & {:as more}] (merge {:id id :text text :level :info} more))

(defdescribe
  toast-layout-test
  (describe
    "fit"
    ;; A card wider than the transcript, or below its last row, paints over the
    ;; scrollbar, the composer or the next terminal line.
    (it "keeps every card inside the area, at any width"
        (doseq [cols
                [24 30 52 80 200]

                rows
                [3 8 30]]

          (let [area
                {:left 2 :top 3 :cols cols :rows rows}

                cards
                (toasts/layout [(entry "a" long-text :title "Release" :source "forge")
                                (entry "b" "Copied") (entry "c" long-text)]
                               area)]

            (doseq [{:keys [col row width height lines heading]} cards]
              (expect (<= (+ col width) (+ 2 cols)))
              (expect (<= (+ row height) (+ 3 rows)))
              (expect (every? #(<= (p/display-width %) (- width 5))
                              (cons (or heading "") lines)))))))
    (it "shows no card in an area narrower than the smallest card"
        (expect (= [] (toasts/layout [(entry "a" "Copied")] {:left 0 :top 0 :cols 23 :rows 20})))))
  (describe "order"
            (it "puts the newest card on top, one row apart, and shows at most three"
                (let [cards (toasts/layout (mapv #(entry (str %) "Saved") (range 5))
                                           {:left 0 :top 1 :cols 80 :rows 40})]
                  (expect (= ["4" "3" "2"] (mapv :id cards)))
                  (expect (= [1 5 9] (mapv :row cards))))))
  (describe "text"
            ;; A long error message must not fill the transcript. The cut must show.
            (it "wraps long text to at most four lines and marks the cut"
                (let [[{:keys [lines]}] (toasts/layout [(entry "a" long-text)]
                                                       {:left 0 :top 0 :cols 52 :rows 40})]
                  (expect (= 4 (count lines)))
                  (expect (= \… (last (last lines))))))))
