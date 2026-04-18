(ns com.blockether.vis.loop.storage.sqlite.concept-graph
  "Concept graph persistence: concepts, aliases, sources, and directed
   relationship edges."
  (:require
   [clojure.string :as str]
   [com.blockether.svar.internal.util :as util]
   [com.blockether.vis.loop.storage.sqlite.core :as core]
   [honey.sql :as sql]
   [next.jdbc :as jdbc]))

(defn clear-concept-graph!
  "Delete all concept graph data (concepts, aliases, sources, edges)."
  [db-info]
  (when (core/ds db-info)
    (jdbc/execute! (core/ds db-info) ["DELETE FROM concept_edge"])
    (jdbc/execute! (core/ds db-info) ["DELETE FROM concept_source"])
    (jdbc/execute! (core/ds db-info) ["DELETE FROM concept_alias"])
    (jdbc/execute! (core/ds db-info) ["DELETE FROM concept"])))

(defn store-concept!
  "Insert a concept. Returns the concept id."
  [db-info {:keys [id term definition group-name]}]
  (when (core/ds db-info)
    (let [cid (core/->id (or id (util/uuid)))]
      (jdbc/execute! (core/ds db-info)
        (sql/format {:insert-into :concept
                     :values [{:id cid
                               :term term
                               :definition definition
                               :group_name group-name
                               :created_at (core/now-ms)}]}))
      cid)))

(defn store-concept-alias!
  "Insert an alias for a concept."
  [db-info {:keys [concept-id alias reason]}]
  (when (core/ds db-info)
    (jdbc/execute! (core/ds db-info)
      (sql/format {:insert-into :concept_alias
                   :values [{:id (core/->id (util/uuid))
                             :concept_id (core/->id concept-id)
                             :alias alias
                             :reason reason}]}))))

(defn store-concept-source!
  "Insert a source reference for a concept."
  [db-info {:keys [concept-id document-id page-index excerpt]}]
  (when (core/ds db-info)
    (jdbc/execute! (core/ds db-info)
      (sql/format {:insert-into :concept_source
                   :values [{:id (core/->id (util/uuid))
                             :concept_id (core/->id concept-id)
                             :document_id (core/->id document-id)
                             :page_index page-index
                             :excerpt excerpt}]}))))

(defn store-concept-edge!
  "Insert a directed relationship between two concepts."
  [db-info {:keys [source-concept-id target-concept-id relationship-type description cardinality]}]
  (when (core/ds db-info)
    (jdbc/execute! (core/ds db-info)
      (sql/format {:insert-into :concept_edge
                   :values [{:id (core/->id (util/uuid))
                             :source_concept_id (core/->id source-concept-id)
                             :target_concept_id (core/->id target-concept-id)
                             :relationship_type relationship-type
                             :description description
                             :cardinality cardinality}]}))))

(defn list-concepts
  "List all active concepts, ordered by group then term."
  [db-info]
  (when (core/ds db-info)
    (core/query! db-info
      (sql/format {:select [:id :term :definition :group_name :status :created_at]
                   :from :concept
                   :where [:!= :status "removed"]
                   :order-by [[:group_name :asc] [:term :asc]]}))))

(defn list-concept-aliases
  "List all aliases for a concept."
  [db-info concept-id]
  (when (core/ds db-info)
    (core/query! db-info
      (sql/format {:select [:alias :reason]
                   :from :concept_alias
                   :where [:= :concept_id (core/->id concept-id)]}))))

(defn list-concept-sources
  "List all source references for a concept."
  [db-info concept-id]
  (when (core/ds db-info)
    (core/query! db-info
      (sql/format {:select [:document_id :page_index :excerpt]
                   :from :concept_source
                   :where [:= :concept_id (core/->id concept-id)]}))))

(defn list-concept-edges
  "List all edges in the concept graph."
  [db-info]
  (when (core/ds db-info)
    (core/query! db-info
      (sql/format {:select [:ce.id :ce.relationship_type :ce.description :ce.cardinality
                            [:cs.term :source_term] [:ct.term :target_term]]
                   :from [[:concept_edge :ce]]
                   :join [[:concept :cs] [:= :ce.source_concept_id :cs.id]
                          [:concept :ct] [:= :ce.target_concept_id :ct.id]]
                   :order-by [[:cs.term :asc]]}))))

(defn load-full-concept-graph
  "Load the complete concept graph: concepts with aliases, sources, and edges."
  [db-info]
  (when (core/ds db-info)
    (let [concepts (list-concepts db-info)
          edges    (list-concept-edges db-info)]
      {:concepts (mapv (fn [c]
                         (let [cid (:id c)]
                           (assoc c
                             :aliases (vec (list-concept-aliases db-info cid))
                             :sources (vec (list-concept-sources db-info cid)))))
                   concepts)
       :edges edges})))

;; =============================================================================
;; Page concepts (grounded, extracted at index time)
;; =============================================================================

(defn store-page-concept!
  "Insert a per-page concept grounded to actual page/node."
  [db-info {:keys [document-id page-id node-id term definition excerpt page-sha]}]
  (when (core/ds db-info)
    (let [cid (core/->id (util/uuid))]
      (jdbc/execute! (core/ds db-info)
        (sql/format {:insert-into :page_concept
                     :values [{:id cid
                               :document_id (core/->id document-id)
                               :page_id (core/->id page-id)
                               :node_id (core/->id node-id)
                               :term term
                               :definition definition
                               :excerpt excerpt
                               :page_sha page-sha
                               :created_at (core/now-ms)}]}))
      cid)))

(defn list-page-concepts
  "List page concepts for a document, or all if document-id is nil."
  ([db-info] (list-page-concepts db-info nil))
  ([db-info document-id]
   (when (core/ds db-info)
     (core/query! db-info
       (sql/format (cond-> {:select [:id :document_id :page_id :node_id
                                     :term :definition :excerpt :page_sha]
                            :from :page_concept
                            :order-by [[:term :asc]]}
                     document-id (assoc :where [:= :document_id (core/->id document-id)])))))))

(defn clear-page-concepts!
  "Delete page concepts for a document, or all if document-id is nil."
  ([db-info] (clear-page-concepts! db-info nil))
  ([db-info document-id]
   (when (core/ds db-info)
     (jdbc/execute! (core/ds db-info)
       (sql/format (cond-> {:delete-from :page_concept}
                     document-id (assoc :where [:= :document_id (core/->id document-id)])))))))

(defn update-page-content-sha!
  "Set the content_sha for a page."
  [db-info page-id sha]
  (when (core/ds db-info)
    (jdbc/execute! (core/ds db-info)
      (sql/format {:update :page
                   :set {:content_sha sha}
                   :where [:= :id (core/->id page-id)]}))))

;; =============================================================================
;; Concept status (user refinement)
;; =============================================================================

(defn set-concept-status!
  "Set a concept's status: 'active', 'removed', or 'user_edited'.
   Removal requires a rationale — pass it as the third arg."
  ([db-info concept-id status]
   (set-concept-status! db-info concept-id status nil))
  ([db-info concept-id status rationale]
   (when (and (= "removed" status) (or (nil? rationale) (str/blank? rationale)))
     (throw (ex-info "Removal requires a rationale. Why is this concept being removed?"
              {:type :rlm/removal-rationale-required :concept-id concept-id})))
   (when (core/ds db-info)
     (jdbc/execute! (core/ds db-info)
       (sql/format {:update :concept
                    :set (cond-> {:status status}
                           (= "removed" status) (assoc :removal_rationale rationale)
                           (= "active" status)  (assoc :removal_rationale nil))
                    :where [:= :id (core/->id concept-id)]})))))

(defn update-concept!
  "Update a concept's definition/group and mark as user_edited."
  [db-info concept-id {:keys [definition group-name]}]
  (when (core/ds db-info)
    (jdbc/execute! (core/ds db-info)
      (sql/format {:update :concept
                   :set (cond-> {:status "user_edited"}
                          definition (assoc :definition definition)
                          group-name (assoc :group_name group-name))
                   :where [:= :id (core/->id concept-id)]}))))
