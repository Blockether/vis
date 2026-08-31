(ns com.blockether.vis.ext.channel-tui.click-regions
  "Vis semantics over Lanterna's generic double-buffered hit map.

   Painters stage absolute screen rectangles and publish one complete frame;
   pointer readers therefore never observe a half-painted registry. Lanterna
   owns bounds, overlap, publication and hover. This namespace only keeps the
   app-specific region maps and vim-style disclosure labels."
  (:refer-clojure :exclude [reset!])
  (:import [com.googlecode.lanterna.gui2 HitRegionMap]))

(set! *unchecked-math* :warn-on-boxed)

(defonce ^:private ^HitRegionMap regions (HitRegionMap.))

(defn begin-frame!
  "Start staging a complete painted frame without changing current lookups."
  []
  (.beginFrame ^HitRegionMap regions))

(defn register!
  "Stage one app region. Bounds use absolute `:row`, `:col`, `:width` and an
   optional `:height` (default 1); later-painted overlapping regions win."
  [{:keys [bounds] :as region}]
  (assert (map? bounds))
  (assert (integer? (:row bounds)))
  (assert (integer? (:col bounds)))
  (assert (integer? (:width bounds)))
  (when (contains? bounds :height) (assert (integer? (:height bounds))))
  (.register ^HitRegionMap regions
             (int (:col bounds))
             (int (:row bounds))
             (int (:width bounds))
             (int (or (:height bounds) 1))
             region)
  nil)

(defn commit-frame!
  "Atomically publish the complete staged frame."
  []
  (.commitFrame ^HitRegionMap regions))

(defn reset! "Clear published, staged and hover state." [] (.reset ^HitRegionMap regions))

(defn current
  "Snapshot of published app regions in paint order."
  []
  (vec (.current ^HitRegionMap regions)))

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

(defn lookup
  "Return the last-painted region containing absolute `(col,row)`, else nil."
  [col row]
  (.lookup ^HitRegionMap regions (int col) (int row)))

(defn hovered
  "Return the app region currently under the pointer, else nil."
  []
  (.hovered ^HitRegionMap regions))

(defn set-hovered!
  "Set hover and return true only when its value changed."
  [region]
  (.setHovered ^HitRegionMap regions region))
