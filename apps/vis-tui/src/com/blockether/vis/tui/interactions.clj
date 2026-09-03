(ns com.blockether.vis.tui.interactions
  "Application-specific values stored in Lanterna's generic hit map.

   Lanterna owns pointer geometry, frame publication, overlap and hover. Vis only
   translates its domain region's `:bounds` map and assigns vim disclosure labels."
  (:import [com.googlecode.lanterna TerminalRectangle]
           [com.googlecode.lanterna.gui2 HitRegionMap]
           [java.util.function Function]))

(set! *unchecked-math* :warn-on-boxed)

(defonce ^HitRegionMap hit-map
  (HitRegionMap.
    (reify
      Function
        (apply [_ region]
          (let [{:keys [row col width height]} (:bounds region)]
            (TerminalRectangle. (int col) (int row) (int width) (int (or height 1))))))))

(def label-alphabet
  "Single-character jump labels for the vim-style disclosure overlay, home row
   first so the common case is a no-reach keypress."
  (mapv str "asdfghjklqwertyuiopzxcvbnm"))

(defn assign-labels
  "Assign deterministic labels to visible `:toggle-details` regions, deduped by
   `[session-id node-id]` and capped by `label-alphabet`."
  [regions]
  (let [toggles (:out (reduce (fn [{:keys [seen] :as acc} region]
                                (if (= :toggle-details (:kind region))
                                  (let [key [(:session-id region) (:node-id region)]]
                                    (if (contains? seen key)
                                      acc
                                      (-> acc
                                          (update :seen conj key)
                                          (update :out conj region))))
                                  acc))
                              {:seen #{} :out []}
                              regions))]
    (mapv vector label-alphabet toggles)))
