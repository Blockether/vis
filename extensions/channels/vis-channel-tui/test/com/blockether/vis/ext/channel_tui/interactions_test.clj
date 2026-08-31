(ns com.blockether.vis.ext.channel-tui.interactions-test
  (:require [com.blockether.vis.ext.channel-tui.interactions :as interactions]
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
