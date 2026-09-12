(ns com.blockether.vis.tui.interactions-test
  (:require [com.blockether.vis.tui.interactions :as interactions]
            [lazytest.core :refer [defdescribe it expect]]))

(defn- toggle-region
  [row session-id node-id]
  {:bounds {:row row :col 4 :width 8}
   :kind :toggle-details
   :session-id session-id
   :node-id node-id})

(defdescribe domain-region-wiring-test
             (it "extracts the app bounds map into Lanterna's hit map"
                 (let [region {:bounds {:row 5 :col 10 :width 3} :kind :url}]
                   (.reset interactions/hit-map)
                   (.beginFrame interactions/hit-map)
                   (.register interactions/hit-map region)
                   (.commitFrame interactions/hit-map)
                   (expect (= region (.lookup interactions/hit-map 11 5))))))

(defdescribe assign-labels-test
             (it "labels only disclosures, home row first, in paint order"
                 (let [pairs (interactions/assign-labels [{:bounds {:row 1 :col 0 :width 5}
                                                           :kind :url} (toggle-region 3 "s" "n1")
                                                          (toggle-region 6 "s" "n2")])]
                   (expect (= [["a" "n1"] ["s" "n2"]]
                              (mapv (fn [[label region]]
                                      [label (:node-id region)])
                                    pairs)))))
             (it "dedupes by session and node while keeping the first painted row"
                 (let [pairs (interactions/assign-labels [(toggle-region 3 "s" "n1")
                                                          (toggle-region 9 "s" "n1")
                                                          (toggle-region 6 "s" "n2")])]
                   (expect (= ["n1" "n2"] (mapv (comp :node-id second) pairs)))
                   (expect (= 3 (:row (:bounds (second (first pairs))))))))
             (it "caps labels at the application alphabet"
                 (let [many (mapv #(toggle-region % "s" (str "n" %))
                                  (range (+ 3 (count interactions/label-alphabet))))]
                   (expect (= (count interactions/label-alphabet)
                              (count (interactions/assign-labels many)))))))

(defn- paging-region
  [row after]
  {:bounds {:row row :col 4 :width 20}
   :kind :activity-page
   :session-id "s"
   :history-id "a1"
   :after after})

(defdescribe
  activity-paging-label-test
  ;; Issue #212: the rule under an Activity band pages the record, so `C-x t`
  ;; must reach it like any other press — and one label per CURSOR, because
  ;; two rules of the same record fetch different windows.
  (it "labels each paging cursor of a record once"
      (let [pairs (interactions/assign-labels [(paging-region 3 32) (paging-region 5 0)
                                               (paging-region 9 32)])]
        (expect (= [["a" 32] ["s" 0]]
                   (mapv (fn [[label region]]
                           [label (:after region)])
                         pairs)))))
  (it "keeps paging rules and disclosures apart"
      (let [pairs (interactions/assign-labels [(toggle-region 3 "s" "n1") (paging-region 4 0)])]
        (expect (= [:toggle-details :activity-page] (mapv (comp :kind second) pairs)))))
  (it "labels the band's search rule as its own target"
      (let [pairs (interactions/assign-labels [(paging-region 3 32)
                                               {:bounds {:row 5 :col 4 :width 20}
                                                :kind :activity-search
                                                :session-id "s"
                                                :history-id "a1"}])]
        (expect (= [:activity-page :activity-search] (mapv (comp :kind second) pairs))))))
