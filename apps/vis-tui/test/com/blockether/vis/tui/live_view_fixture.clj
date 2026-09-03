(ns com.blockether.vis.tui.live-view-fixture
  "Small test builders for gateway-shaped live views. The standalone client consumes
   these maps; it does not depend on the engine namespace that authors them."
  (:refer-clojure :exclude [log]))

(defn status
  ([id text] (status id text nil))
  ([id text opts]
   (assoc opts
     :id id
     :type :status
     :text text)))

(defn progress
  ([id opts]
   (assoc opts
     :id id
     :type :progress)))

(defn stat
  ([id stats] (stat id stats nil))
  ([id stats opts]
   (assoc opts
     :id id
     :type :stat
     :stats (vec stats))))

(defn log
  ([id] (log id nil))
  ([id opts] (merge {:lines [] :window-lines 12} opts {:id id :type :log})))

(defn table-column
  ([id label] {:id id :label label})
  ([id label opts]
   (assoc opts
     :id id
     :label label)))

(defn table-row
  ([id cells] {:id id :cells (vec cells)})
  ([id cells opts]
   (assoc opts
     :id id
     :cells (vec cells))))

(defn table
  ([id columns] (table id columns nil))
  ([id columns opts]
   (merge {:rows [] :selected-ids [] :max-rows 500}
          opts
          {:id id :type :table :columns (vec columns)})))

(defn row [id & nodes] {:id id :type :row :direction :row :fields (vec nodes)})

(defn view [opts & nodes] (assoc opts :nodes (vec nodes)))

(defn normalize-live-view [view] view)

(defn normalize-patch
  [view patch]
  {:view-id (:id view) :seq (inc (long (or (:seq view) 0))) :ops (vec patch)})
