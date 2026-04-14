(ns com.blockether.vis.rlm.data
  (:require
   [clojure.string :as str]
   [com.blockether.vis.rlm.db :as rlm-db]
   [com.blockether.vis.rlm.schema :as schema]
   [com.blockether.svar.internal.util :as util]
   [taoensso.trove :as trove]))

(defn- normalize-entity-type
  [raw-type]
  (let [t-name (some-> raw-type name)]
    (if (and t-name (contains? schema/ENTITY_TYPE_VALUES t-name))
      (keyword t-name)
      :concept)))

(defn- normalize-relationship-type
  [raw-type]
  (let [rt-name (some-> raw-type name)]
    (if (and rt-name (contains? schema/RELATIONSHIP_TYPE_VALUES rt-name))
      (keyword rt-name)
      :related-to)))

(defn- entity->attrs
  [db-info doc-id entity]
  (let [entity-id (util/uuid)
        entity-name (or (:name entity) (:name entity) "unknown")
        entity-type (normalize-entity-type (or (:type entity) (:type entity)))
        canonical-id (rlm-db/resolve-canonical-id db-info entity-name entity-type)]
    [entity-name
     entity-id
     (cond-> {:id entity-id
              :name entity-name
              :type entity-type
              :canonical-id canonical-id
              :description (or (:description entity) (:description entity) "")
              :document-id (str doc-id)
              :created-at (java.util.Date.)}
       (or (:section entity) (:section entity))
       (assoc :section (or (:section entity) (:section entity)))
       (or (:page entity) (:page entity))
       (assoc :page (long (or (:page entity) (:page entity)))))]))

(defn- store-entities!
  [db-info doc-id entities]
  (let [name->uuid (atom {})]
    (doseq [entity entities]
      (try
        (let [[entity-name entity-id attrs] (entity->attrs db-info doc-id entity)]
          (rlm-db/store-entity! db-info attrs)
          (swap! name->uuid assoc (str/lower-case entity-name) entity-id))
        (catch Exception e
          (trove/log! {:level :warn :data {:error (ex-message e)} :msg "Failed to store entity"}))))
    @name->uuid))

(defn- store-relationships!
  [db-info doc-id relationships name->uuid]
  (doseq [rel relationships]
    (try
      (let [src-name (or (:source-id rel) (:source rel))
            tgt-name (or (:target-id rel) (:target rel))
            src-id (get name->uuid (some-> src-name str str/lower-case))
            tgt-id (get name->uuid (some-> tgt-name str str/lower-case))]
        (when (and src-id tgt-id)
          (rlm-db/store-relationship! db-info
            {:id (util/uuid)
             :type (normalize-relationship-type (or (:type rel) (:type rel)))
             :source-id src-id
             :target-id tgt-id
             :description (or (:description rel) (:description rel) "")
             :document-id (str doc-id)})))
      (catch Exception e
        (trove/log! {:level :warn :data {:error (ex-message e)} :msg "Failed to store relationship"})))))

(defn store-extraction-results!
  "Store extracted entities + relationships into the RLM store.
   Returns {:entities-extracted N :relationships-extracted N}."
  [db-info doc-id entities relationships]
  (let [name->uuid (store-entities! db-info doc-id entities)]
    (store-relationships! db-info doc-id relationships name->uuid)
    {:entities-extracted (count entities)
     :relationships-extracted (count relationships)}))
