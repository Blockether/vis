(ns com.blockether.vis.tui.interactions
  "Application-specific values stored in Lanterna's generic hit map.

   Lanterna owns pointer geometry, frame publication, overlap and hover. Vis only
   translates its domain region's `:bounds` map and assigns vim disclosure labels."
  (:import [com.googlecode.lanterna TerminalRectangle]
           [com.googlecode.lanterna.gui2 HitRegionMap]
           [java.util.function Function]))

(set! *unchecked-math* :warn-on-boxed)

(defn create-hit-map
  "Create an independent pointer surface with application region values."
  ^HitRegionMap []
  (HitRegionMap.
    (reify
      Function
        (apply [_ region]
          (let [{:keys [row col width height]} (:bounds region)]
            (TerminalRectangle. (int col) (int row) (int width) (int (or height 1))))))))

(defonce ^:dynamic ^HitRegionMap hit-map (create-hit-map))

(def label-alphabet
  "Single-character jump labels for the vim-style disclosure overlay, home row
   first so the common case is a no-reach keypress."
  (mapv str "asdfghjklqwertyuiopzxcvbnm"))

(defn label-key
  "Stable identity for keyboard-addressable transcript disclosures and live cards.
   A card's border, padding and content rows share one target. Other artifacts do not."
  [region]
  (case (:kind region)
    :toggle-details
    [:toggle-details (:session-id region) (:node-id region)]

    :artifact
    (when (:live-card? region)
      [:artifact (:session-id region) (get-in region [:artifact :iteration-id])
       (get-in region [:artifact :index])])

    nil))

(defn assign-labels
  "Label visible disclosures and live cards in paint order, once per target."
  [regions]
  (let [targets (:out (reduce (fn [{:keys [seen] :as acc} region]
                                (if-let [key (label-key region)]
                                  (if (contains? seen key)
                                    acc
                                    (-> acc
                                        (update :seen conj key)
                                        (update :out conj region)))
                                  acc))
                              {:seen #{} :out []}
                              regions))]
    (mapv vector label-alphabet targets)))
