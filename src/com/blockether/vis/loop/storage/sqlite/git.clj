(ns com.blockether.vis.loop.storage.sqlite.git
  "Git ingestion entities: repositories and commits (event entities +
   ticket refs, file paths, parents)."
  (:require
   [com.blockether.svar.internal.util :as util]
   [com.blockether.vis.loop.storage.sqlite.core :as core]
   [honey.sql :as sql]
   [next.jdbc :as jdbc]))

(defn db-store-repo!
  [db-info {:keys [name path head-sha head-short branch commits-ingested]}]
  (when (and (core/ds db-info) name)
    (let [entity-id (core/->id (util/uuid))]
      (jdbc/with-transaction [tx (core/ds db-info)]
        (let [tx-info {:datasource tx}]
          ;; Try to find an existing entity by repo_attrs.name
          (if-let [existing (core/query-one! tx-info
                              {:select [:entity_id] :from :repo_attrs :where [:= :name name]})]
            (let [eid (:entity_id existing)]
              (jdbc/execute! tx
                (sql/format
                  {:update :repo_attrs
                   :set (cond-> {:ingested_at (core/now-ms)}
                          path             (assoc :path (str path))
                          head-sha         (assoc :head_sha head-sha)
                          head-short       (assoc :head_short head-short)
                          branch           (assoc :branch branch)
                          commits-ingested (assoc :commits_ingested (long commits-ingested)))
                   :where [:= :entity_id eid]})))
            (do
              (core/upsert-entity-row! tx-info
                {:id entity-id :type "repo" :name name :created_at (core/now-ms) :updated_at (core/now-ms)})
              (jdbc/execute! tx
                (sql/format
                  {:insert-into :repo_attrs
                   :values [(cond-> {:entity_id entity-id :name name
                                     :path (str path) :ingested_at (core/now-ms)}
                              head-sha         (assoc :head_sha head-sha)
                              head-short       (assoc :head_short head-short)
                              branch           (assoc :branch branch)
                              commits-ingested (assoc :commits_ingested (long commits-ingested)))]})))))
        name))))

(defn- repo-row->ns [row]
  (cond-> {}
    (:name row)             (assoc :name (:name row))
    (:path row)             (assoc :path (:path row))
    (:head_sha row)         (assoc :head-sha (:head_sha row))
    (:head_short row)       (assoc :head-short (:head_short row))
    (:branch row)           (assoc :branch (:branch row))
    (some? (:commits_ingested row)) (assoc :commits-ingested (:commits_ingested row))
    (some? (:ingested_at row)) (assoc :ingested-at (core/->date (:ingested_at row)))))

(defn db-list-repos [db-info]
  (when (core/ds db-info)
    (->> (core/query! db-info
           {:select [:name :path :head_sha :head_short :branch :commits_ingested :ingested_at]
            :from :repo_attrs
            :order-by [[:name :asc]]})
      (mapv repo-row->ns))))

(defn db-get-repo-by-name [db-info name]
  (when (and (core/ds db-info) name)
    (some-> (core/query-one! db-info
              {:select [:name :path :head_sha :head_short :branch :commits_ingested :ingested_at]
               :from :repo_attrs :where [:= :name name]})
      repo-row->ns)))

(defn store-commit-entity!
  "Bulk insert a commit entity with its commit_attrs + child rows for
   ticket-refs/file-paths/parents."
  [db-info {:keys [entity-id entity-cols commit-cols ticket-refs file-paths parents]}]
  (when (core/ds db-info)
    (jdbc/with-transaction [tx (core/ds db-info)]
      (let [tx-info {:datasource tx}
            entity-id' (or entity-id
                         (:id entity-cols)
                         (core/->id (:id entity-cols)))]
        (core/upsert-entity-row! tx-info (assoc entity-cols :id entity-id'))
        (when (seq commit-cols)
          (jdbc/execute! tx
            (sql/format
              {:insert-into :commit_attrs
               :values [(assoc commit-cols :entity_id entity-id')]
               :on-conflict [:entity_id]
               :do-update-set (vec (keys commit-cols))})))
        (doseq [t (distinct ticket-refs)]
          (jdbc/execute! tx
            (sql/format
              {:insert-into :commit_ticket_ref
               :values [{:entity_id entity-id' :ticket t}]
               :on-conflict [:entity_id :ticket] :do-nothing true})))
        (doseq [p (distinct file-paths)]
          (jdbc/execute! tx
            (sql/format
              {:insert-into :commit_file_path
               :values [{:entity_id entity-id' :path p}]
               :on-conflict [:entity_id :path] :do-nothing true})))
        (doseq [s (distinct parents)]
          (jdbc/execute! tx
            (sql/format
              {:insert-into :commit_parent
               :values [{:entity_id entity-id' :parent_sha s}]
               :on-conflict [:entity_id :parent_sha] :do-nothing true})))
        entity-id'))))

(defn db-search-commits
  ([db-info] (db-search-commits db-info {}))
  ([db-info {:keys [sha category since until ticket path author-email document-id limit]
             :or {limit 50}}]
   (when (core/ds db-info)
     (let [where (cond-> [:and [:= :e.type "event"] [:is-not :c.sha nil]]
                   sha          (conj [:= :c.sha sha])
                   category     (conj [:= :c.category (core/->kw category)])
                   document-id  (conj [:= :e.document_id document-id])
                   author-email (conj [:= :c.author_email author-email])
                   since        (conj [:>= :c.date since])
                   until        (conj [:<= :c.date until]))
           base-sql {:select [[:e.id :eid]
                              [:e.name :e-name] [:e.description :e-desc]
                              [:e.document_id :document_id]
                              [:c.sha :sha] [:c.category :category]
                              [:c.date :date] [:c.prefix :prefix] [:c.scope :scope]
                              [:c.author_email :author_email]]
                     :from [[:entity :e]]
                     :join [[:commit_attrs :c] [:= :e.id :c.entity_id]]
                     :where where
                     :order-by [[:c.date :desc]]
                     :limit limit}
           base-sql (cond-> base-sql
                      ticket (update :join into [[:commit_ticket_ref :t] [:= :t.entity_id :e.id]])
                      ticket (update :where conj [:= :t.ticket ticket])
                      path (update :join into [[:commit_file_path :p] [:= :p.entity_id :e.id]])
                      path (update :where conj [:like :p.path (str "%" path "%")]))
           rows (core/query! db-info base-sql)
           ids (distinct (mapv :eid rows))
           ticket-map (if (seq ids)
                        (group-by :entity_id
                          (core/query! db-info
                            {:select [:entity_id :ticket] :from :commit_ticket_ref
                             :where [:in :entity_id ids]}))
                        {})
           file-map (if (seq ids)
                      (group-by :entity_id
                        (core/query! db-info
                          {:select [:entity_id :path] :from :commit_file_path
                           :where [:in :entity_id ids]}))
                      {})
           parent-map (if (seq ids)
                        (group-by :entity_id
                          (core/query! db-info
                            {:select [:entity_id :parent_sha] :from :commit_parent
                             :where [:in :entity_id ids]}))
                        {})]
       (mapv (fn [r]
               (let [eid (:eid r)]
                 (cond-> {:id (core/->uuid eid)
                          :name (:e-name r)
                          :description (:e-desc r)
                          :document-id (:document_id r)
                          :sha (:sha r)
                          :category (core/->kw-back (:category r))
                          :date (:date r)
                          :author-email (:author_email r)}
                   (:prefix r) (assoc :prefix (:prefix r))
                   (:scope r)  (assoc :scope (:scope r))
                   (seq (get ticket-map eid))
                   (assoc :ticket-refs (mapv :ticket (get ticket-map eid)))
                   (seq (get file-map eid))
                   (assoc :file-paths (mapv :path (get file-map eid)))
                   (seq (get parent-map eid))
                   (assoc :parents (mapv :parent_sha (get parent-map eid))))))
         rows)))))

(defn db-commit-by-sha
  [db-info sha]
  (when (and (core/ds db-info) (seq sha))
    (first (db-search-commits db-info {:sha sha}))))

(defn db-repo-stats
  "Return stats for all repos: {:total-commits N :unique-authors N :repos [{...}]}."
  [db-info]
  (when (core/ds db-info)
    (let [repos   (db-list-repos db-info)
          commits (core/query-one! db-info
                    ["SELECT COUNT(*) as cnt FROM commit_attrs"])
          authors (core/query-one! db-info
                    ["SELECT COUNT(DISTINCT author_email) as cnt FROM commit_attrs"])]
      {:repos          repos
       :total-commits  (or (:cnt commits) 0)
       :unique-authors (or (:cnt authors) 0)})))
