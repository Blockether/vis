(ns com.blockether.vis.loop.storage.sqlite.corpus
  "Corpus domain: documents, raw documents, pages, page-nodes, TOC entries,
   skills, claims, bulk PageIndex ingestion, corpus-revision meta, QA
   manifest helpers, aggregate DB stats, and the markdown results renderer."
  (:require
   [com.blockether.svar.internal.util :as util]
   [com.blockether.vis.loop.runtime.shared :as rt-shared]
   [com.blockether.vis.loop.storage.sqlite.core :as core]
   [honey.sql :as sql]
   [next.jdbc :as jdbc]
   [taoensso.trove :as trove])
  (:import
   (java.util Date)))

;; =============================================================================
;; Corpus revision (rlm_meta)
;; =============================================================================

(def ^:private CORPUS-META-ID "global")

(defn get-corpus-revision
  "Returns current corpus revision (long), defaulting to 0 if uninitialized."
  [db-info]
  (if-not (core/ds db-info)
    0
    (or (some-> (core/query-one! db-info {:select [:corpus_revision] :from :rlm_meta
                                          :where [:= :id CORPUS-META-ID]})
          :corpus_revision)
      0)))

(defn bump-corpus-revision!
  "Atomically increments corpus revision. Returns the new revision."
  [db-info]
  (if-not (core/ds db-info)
    0
    (let [new-rev (inc (long (get-corpus-revision db-info)))]
      (jdbc/execute! (core/ds db-info)
        (sql/format
          {:insert-into :rlm_meta
           :values [{:id CORPUS-META-ID
                     :corpus_revision new-rev
                     :updated_at (core/now-ms)}]
           :on-conflict [:id]
           :do-update-set [:corpus_revision :updated_at]}))
      new-rev)))

;; =============================================================================
;; Documents
;; =============================================================================

(defn document-row->ns [row]
  (cond-> {}
    (some? (:id row))                (assoc :id (:id row))
    (some? (:name row))              (assoc :name (:name row))
    (some? (:type row))              (assoc :type (core/->kw-back (:type row)))
    (some? (:title row))             (assoc :title (:title row))
    (some? (:abstract row))          (assoc :abstract (:abstract row))
    (some? (:extension row))         (assoc :extension (:extension row))
    (some? (:author row))            (assoc :author (:author row))
    (some? (:page_count row))        (assoc :page-count (:page_count row))
    (some? (:created_at row))        (assoc :created-at (core/->date (:created_at row)))
    (some? (:updated_at row))        (assoc :updated-at (core/->date (:updated_at row)))
    (some? (:certainty_alpha row))   (assoc :certainty-alpha (:certainty_alpha row))
    (some? (:certainty_beta row))    (assoc :certainty-beta (:certainty_beta row))))

(defn- doc->cols [doc]
  (let [extract (fn [m]
                  (cond-> {}
                    (:id m)          (assoc :id (:id m))
                    (:name m)        (assoc :name (:name m))
                    (:type m)        (assoc :type (core/->kw (:type m)))
                    (:title m)       (assoc :title (:title m))
                    (:abstract m)    (assoc :abstract (:abstract m))
                    (:extension m)   (assoc :extension (:extension m))
                    (:author m)      (assoc :author (:author m))
                    (:page-count m)  (assoc :page_count (:page-count m))
                    (:created-at m)  (assoc :created_at (core/->epoch-ms (:created-at m)))
                    (:updated-at m)  (assoc :updated_at (core/->epoch-ms (:updated-at m)))
                    (some? (:certainty-alpha m)) (assoc :certainty_alpha (double (:certainty-alpha m)))
                    (some? (:certainty-beta m))  (assoc :certainty_beta (double (:certainty-beta m)))))]
    (extract doc)))

(defn store-document!
  "Upsert a document row.
   Returns the stored row as a flat-key map."
  [db-info doc]
  (when (core/ds db-info)
    (let [cols (doc->cols doc)]
      (when-let [id (:id cols)]
        (jdbc/execute! (core/ds db-info)
          (sql/format
            {:insert-into :document
             :values [cols]
             :on-conflict [:id]
             :do-update-set (vec (remove #{:id} (keys cols)))}))
        (document-row->ns
          (core/query-one! db-info {:select [:*] :from :document :where [:= :id id]}))))))

(defn- get-document-toc
  "Ordered TOC summary for a doc-id. Returns vec of {:title :level :page}."
  [db-info doc-id]
  (when (core/ds db-info)
    (->> (core/query! db-info
           {:select [:title :level :target_page]
            :from :document_toc
            :where [:= :document_id doc-id]
            :order-by [[:level :asc] [:target_page :asc]]})
      (mapv (fn [r] {:title (:title r) :level (:level r) :page (:target_page r)})))))

(defn db-list-documents
  "Lists documents with optional TOC summaries."
  ([db-info] (db-list-documents db-info {}))
  ([db-info {:keys [limit include-toc?] :or {limit 100 include-toc? true}}]
   (when (core/ds db-info)
     (let [rows (core/query! db-info
                  {:select [:id :name :title :extension :abstract]
                   :from :document
                   :limit limit})]
       (mapv (fn [row]
               (cond-> {:id (:id row)
                        :name (:name row)
                        :title (:title row)
                        :extension (:extension row)}
                 (and (:abstract row) (not= "" (:abstract row)))
                 (assoc :abstract (:abstract row))
                 include-toc?
                 (assoc :toc (get-document-toc db-info (:id row)))))
         rows)))))

(defn db-get-document
  "Full document row by id."
  [db-info doc-id]
  (when (core/ds db-info)
    (some-> (core/query-one! db-info {:select [:*] :from :document :where [:= :id doc-id]})
      document-row->ns)))

;; =============================================================================
;; Raw document
;; =============================================================================

(defn store-raw-document!
  "Persist raw PageIndex EDN for replay/debugging."
  [db-info doc-id content]
  (when (core/ds db-info)
    (jdbc/execute! (core/ds db-info)
      (sql/format
        {:insert-into :raw_document
         :values [{:id doc-id :content content}]
         :on-conflict [:id]
         :do-update-set [:content]}))))

;; =============================================================================
;; Pages
;; =============================================================================

(defn- page->cols [page]
  (cond-> {}
    (:id page)                        (assoc :id (:id page))
    (:document-id page)               (assoc :document_id (:document-id page))
    (some? (:index page))             (assoc :idx (:index page))
    (:created-at page)                (assoc :created_at (core/->epoch-ms (:created-at page)))
    (:last-accessed page)             (assoc :last_accessed (core/->epoch-ms (:last-accessed page)))
    (some? (:access-count page))      (assoc :access_count (double (:access-count page)))
    (some? (:q-value page))           (assoc :q_value (double (:q-value page)))
    (some? (:q-update-count page))    (assoc :q_update_count (long (:q-update-count page)))))

(defn store-page!
  [db-info page]
  (when (core/ds db-info)
    (let [cols (page->cols page)]
      (when-let [id (:id cols)]
        (jdbc/execute! (core/ds db-info)
          (sql/format
            {:insert-into :page
             :values [cols]
             :on-conflict [:id]
             :do-update-set (vec (remove #{:id} (keys cols)))}))
        id))))

(defn get-page-q-value
  "Returns Q-value for a page (default 0.5 if not set)."
  [db-info page-id]
  (if-not (core/ds db-info)
    0.5
    (or (some-> (core/query-one! db-info
                  {:select [:q_value] :from :page :where [:= :id page-id]})
          :q_value)
      0.5)))

(defn update-page-q-value!
  "EMA update of Q-value: Q' = Q + alpha × (reward - Q).
   Alpha decreases with update count for stability."
  [db-info page-id reward]
  (when (core/ds db-info)
    (try
      (let [row (core/query-one! db-info
                  {:select [:q_value :q_update_count]
                   :from :page :where [:= :id page-id]})
            q (or (:q_value row) 0.5)
            cnt (or (:q_update_count row) 0)
            alpha (max 0.05 (/ 1.0 (+ 1.0 (/ cnt 10.0))))
            new-q (+ q (* alpha (- (double reward) q)))]
        (jdbc/execute! (core/ds db-info)
          (sql/format
            {:update :page
             :set {:q_value (min 1.0 (max 0.0 new-q))
                   :q_update_count (inc cnt)}
             :where [:= :id page-id]})))
      (catch Exception e
        (trove/log! {:level :debug :data {:page-id page-id :error (ex-message e)}
                     :msg "Q-value update failed (non-fatal)"})))))

(defn pages-accessed-since
  "Set of page ids accessed since the given Date/Instant/epoch-ms cutoff."
  [db-info cutoff]
  (when (core/ds db-info)
    (try
      (set (mapv :id
             (core/query! db-info
               {:select [:id] :from :page
                :where [:>= :last_accessed (core/->epoch-ms cutoff)]})))
      (catch Exception e
        (trove/log! {:level :debug :data {:error (ex-message e)}
                     :msg "pages-accessed-since query failed"})
        #{}))))

(defn finalize-q-updates!
  "Apply Q-reward to each page in the collection."
  [db-info accessed-page-ids reward]
  (doseq [pid (distinct accessed-page-ids)]
    (update-page-q-value! db-info pid reward)))

;; =============================================================================
;; Page nodes
;; =============================================================================

(defn node-row->ns [row]
  (cond-> {}
    (:id row)                      (assoc :id (:id row))
    (:page_id row)                 (assoc :page-id (:page_id row))
    (:document_id row)             (assoc :document-id (:document_id row))
    (:local_id row)                (assoc :local-id (:local_id row))
    (:type row)                    (assoc :type (core/->kw-back (:type row)))
    (:content row)                 (assoc :content (:content row))
    (:description row)             (assoc :description (:description row))
    (:level row)                   (assoc :level (:level row))
    (:parent_id row)               (assoc :parent-id (:parent_id row))
    (:image_data row)              (assoc :image-data (:image_data row))
    (some? (:continuation row))    (assoc :continuation? (not (zero? (long (:continuation row)))))
    (:caption row)                 (assoc :caption (:caption row))
    (:kind row)                    (assoc :kind (:kind row))
    (:bbox row)                    (assoc :bbox (:bbox row))
    (:group_id row)                (assoc :group-id (:group_id row))))

(defn- node->cols [node]
  (cond-> {}
    (:id node)           (assoc :id (:id node))
    (:page-id node)      (assoc :page_id (:page-id node))
    (:document-id node)  (assoc :document_id (:document-id node))
    (:local-id node)     (assoc :local_id (:local-id node))
    (:type node)         (assoc :type (core/->kw (:type node)))
    (:content node)      (assoc :content (:content node))
    (:description node)  (assoc :description (:description node))
    (:level node)        (assoc :level (:level node))
    (:parent-id node)    (assoc :parent_id (:parent-id node))
    (:image-data node)   (assoc :image_data (:image-data node))
    (some? (:continuation? node)) (assoc :continuation (if (:continuation? node) 1 0))
    (:caption node)      (assoc :caption (:caption node))
    (:kind node)         (assoc :kind (:kind node))
    (:bbox node)         (assoc :bbox (if (string? (:bbox node))
                                        (:bbox node)
                                        (pr-str (:bbox node))))
    (:group-id node)     (assoc :group_id (:group-id node))))

(defn store-page-node!
  [db-info node]
  (when (core/ds db-info)
    (let [cols (node->cols node)]
      (when-let [id (:id cols)]
        (jdbc/execute! (core/ds db-info)
          (sql/format
            {:insert-into :page_node
             :values [cols]
             :on-conflict [:id]
             :do-update-set (vec (remove #{:id} (keys cols)))}))
        id))))

(defn db-get-page-node [db-info node-id]
  (when (core/ds db-info)
    (some-> (core/query-one! db-info {:select [:*] :from :page_node :where [:= :id node-id]})
      node-row->ns)))

(defn db-list-page-nodes
  ([db-info] (db-list-page-nodes db-info {}))
  ([db-info {:keys [page-id document-id type limit] :or {limit 100}}]
   (when (core/ds db-info)
     (let [where (cond-> [:and [:is-not :id nil]]
                   page-id     (conj [:= :page_id page-id])
                   document-id (conj [:= :document_id document-id])
                   type        (conj [:= :type (core/->kw type)]))
           rows (core/query! db-info
                  {:select [:id :page_id :document_id :type :level :local_id :content :description]
                   :from :page_node
                   :where where
                   :limit limit})]
       (mapv (fn [r]
               (let [truncate (fn [s]
                                (when s (if (> (count s) 200) (subs s 0 200) s)))]
                 (cond-> (node-row->ns r)
                   (:content r)     (assoc :content (truncate (:content r)))
                   (:description r) (assoc :description (truncate (:description r))))))
         rows)))))

;; =============================================================================
;; TOC
;; =============================================================================

(defn toc-row->ns [row]
  (cond-> {}
    (:id row)                 (assoc :id (:id row))
    (:document_id row)        (assoc :document-id (:document_id row))
    (:type row)               (assoc :type (core/->kw-back (:type row)))
    (:title row)              (assoc :title (:title row))
    (and (:description row)
      (not= "" (:description row))) (assoc :description (:description row))
    (some? (:target_page row)) (assoc :target-page (:target_page row))
    (:target_section_id row)  (assoc :target-section-id (:target_section_id row))
    (:level row)              (assoc :level (:level row))
    (:parent_id row)          (assoc :parent-id (:parent_id row))
    (some? (:created_at row)) (assoc :created-at (core/->date (:created_at row)))))

(defn- toc->cols [e]
  (cond-> {}
    (:id e)                 (assoc :id (:id e))
    (:document-id e)        (assoc :document_id (:document-id e))
    (:type e)               (assoc :type (core/->kw (:type e)))
    (:title e)              (assoc :title (:title e))
    (:description e)        (assoc :description (:description e))
    (some? (:target-page e)) (assoc :target_page (:target-page e))
    (:target-section-id e)  (assoc :target_section_id (:target-section-id e))
    (:level e)              (assoc :level (:level e))
    (:parent-id e)          (assoc :parent_id (:parent-id e))
    (:created-at e)         (assoc :created_at (core/->epoch-ms (:created-at e)))))

(defn db-store-toc-entry!
  ([db-info entry] (db-store-toc-entry! db-info entry "standalone"))
  ([db-info entry doc-id]
   (when (core/ds db-info)
     (let [id (or (:id entry) (str (util/uuid)))
           entry-data (-> entry
                        (assoc :id id
                          :document-id doc-id
                          :created-at (Date.)))
           cols (toc->cols entry-data)]
       (jdbc/execute! (core/ds db-info)
         (sql/format
           {:insert-into :document_toc
            :values [cols]
            :on-conflict [:id]
            :do-update-set (vec (remove #{:id} (keys cols)))}))
       entry-data))))

(defn db-get-toc-entry [db-info entry-id]
  (when (core/ds db-info)
    (some-> (core/query-one! db-info {:select [:*] :from :document_toc :where [:= :id entry-id]})
      toc-row->ns)))

(defn db-list-toc-entries
  ([db-info] (db-list-toc-entries db-info {}))
  ([db-info {:keys [parent-id limit] :or {limit 100}}]
   (when (core/ds db-info)
     (let [where (cond-> [:and [:is-not :id nil]]
                   parent-id (conj [:= :parent_id parent-id]))
           rows (core/query! db-info
                  {:select [:id :title :level :description :target_page :parent_id]
                   :from :document_toc
                   :where where
                   :order-by [[:level :asc]]
                   :limit limit})]
       (mapv toc-row->ns rows)))))

;; =============================================================================
;; Skills
;; =============================================================================

(defn skill-changed?
  "True if stored content-hash differs from new-hash (or no stored skill yet)."
  [db-info skill-name new-hash]
  (let [doc-id (str "skill-" (clojure.core/name skill-name))]
    (if-not (core/ds db-info)
      true
      (let [row (core/query-one! db-info
                  {:select [:content_hash] :from :skill_attrs :where [:= :document_id doc-id]})
            stored (:content_hash row)]
        (or (nil? stored) (not= stored new-hash))))))

(defn ingest-skills!
  "Ingest skill registry into SQLite as :document.type/skill rows + skill_attrs.
   Skips skills whose content-hash hasn't changed. Returns count ingested."
  [db-info skill-registry]
  (when (and (core/ds db-info) (seq skill-registry))
    (let [changed (filterv (fn [[nm skill]]
                             (skill-changed? db-info nm (:content-hash skill)))
                    skill-registry)]
      (jdbc/with-transaction [tx (core/ds db-info)]
        (let [tx-info {:datasource tx}]
          (doseq [[nm skill] changed]
            (let [n (clojure.core/name nm)
                  doc-id (str "skill-" n)]
              (store-document! tx-info
                {:id doc-id
                 :name n
                 :type :document.type/skill
                 :title (or (:description skill) n)
                 :abstract (or (:abstract skill) "")
                 :extension "md"
                 :updated-at (Date.)
                 :certainty-alpha 2.0
                 :certainty-beta 1.0})
              (jdbc/execute! tx
                (sql/format
                  {:insert-into :skill_attrs
                   :values [(cond-> {:document_id doc-id
                                     :body (:body skill)
                                     :source_path (or (:source-path skill) "")
                                     :content_hash (or (:content-hash skill) "")}
                              (:agent skill)    (assoc :agent_config (pr-str (:agent skill)))
                              (:requires skill) (assoc :requires (pr-str (:requires skill)))
                              (:version skill)  (assoc :version (str (:version skill))))]
                   :on-conflict [:document_id]
                   :do-update-set [:body :source_path :content_hash :agent_config :requires :version]}))))))
      (trove/log! {:level :debug :id ::skills-ingested
                   :msg (str "Skills ingested: " (count changed) "/" (count skill-registry) " changed")})
      (count changed))))

(defn delete-skill-entity!
  "Remove a skill document by canonical id: skill-<name>."
  [db-info skill-name]
  (when (core/ds db-info)
    (let [doc-id (str "skill-" (clojure.core/name skill-name))]
      (jdbc/execute! (core/ds db-info)
        (sql/format {:delete-from :document :where [:= :id doc-id]})))))

;; =============================================================================
;; QA manifest corpus snapshot helpers
;; =============================================================================

(defn qa-corpus-documents [db-info]
  (->> (core/query! db-info
         {:select [:id :name :title :extension :abstract] :from :document})
    (mapv document-row->ns)
    (sort-by (juxt :id :name))
    vec))

(defn qa-corpus-toc-entries [db-info]
  (->> (core/query! db-info
         {:select [:id :document_id :title :level :target_page :target_section_id :description]
          :from :document_toc})
    (mapv toc-row->ns)
    (sort-by (juxt :document-id :target-page
               :level :title :id))
    vec))

(defn qa-corpus-page-nodes [db-info]
  (->> (core/query! db-info
         {:select [:id :document_id :page_id :type :local_id :content :description]
          :from :page_node})
    (mapv node-row->ns)
    (sort-by (juxt :document-id :page-id
               :local-id :id))
    vec))

;; =============================================================================
;; Bulk PageIndex ingestion
;; =============================================================================

(defn- build-page-entity [page doc-id]
  (let [page-id (str doc-id "-page-" (:index page))
        now (Date.)]
    {:entity {:id page-id
              :document-id doc-id
              :index (:index page)
              :created-at now
              :last-accessed now
              :access-count 1.0}
     :page-id page-id}))

(defn- build-page-node-entity [node page-id doc-id]
  (let [node-id (str page-id "-node-" (or (:id node) (util/uuid)))
        visual? (#{:image :table} (:type node))
        img-bytes (:image-data node)
        too-large? (and visual? img-bytes (> (alength ^bytes img-bytes) 5242880))
        image (when (and visual? img-bytes (not too-large?)) img-bytes)]
    (when too-large?
      (trove/log! {:level :warn :data {:node-id node-id :bytes (alength ^bytes img-bytes)}
                   :msg "Skipping page node image-data (exceeds 5MB)"}))
    (cond-> {:id node-id :page-id page-id
             :document-id doc-id :type (:type node)}
      (:id node)                   (assoc :local-id (:id node))
      (:parent-id node)            (assoc :parent-id (:parent-id node))
      (:level node)                (assoc :level (:level node))
      (and (not visual?) (:content node))
      (assoc :content (:content node))
      image                                  (assoc :image-data image)
      (:description node)          (assoc :description (:description node))
      (some? (:continuation? node)) (assoc :continuation? (:continuation? node))
      (:caption node)              (assoc :caption (:caption node))
      (:kind node)                 (assoc :kind (:kind node))
      (:bbox node)                 (assoc :bbox (pr-str (:bbox node)))
      (:group-id node)             (assoc :group-id (:group-id node)))))

(defn db-store-pageindex-document!
  "Bulk-insert a full PageIndex document: raw → document → pages + nodes → TOC.
   Returns {:document-id :pages-stored :nodes-stored :toc-entries-stored}."
  [db-info doc]
  (when (core/ds db-info)
    (let [doc-id (str (util/uuid))]
      (jdbc/with-transaction [tx (core/ds db-info)]
        (let [tx-info {:datasource tx}
              _ (store-raw-document! tx-info doc-id (pr-str doc))
              _ (store-document! tx-info
                  (cond-> {:id doc-id
                           :name (:name doc)
                           :extension (:extension doc)
                           :certainty-alpha 2.0
                           :certainty-beta 1.0}
                    (:title doc)      (assoc :title (:title doc))
                    (:abstract doc)   (assoc :abstract (:abstract doc))
                    (:author doc)     (assoc :author (:author doc))
                    (:created-at doc) (assoc :created-at (:created-at doc))
                    (:updated-at doc) (assoc :updated-at (:updated-at doc))))
              pages (:pages doc)
              page-count (count pages)
              node-count (atom 0)]
          (doseq [page pages]
            (let [{:keys [entity page-id]} (build-page-entity page doc-id)]
              (store-page! tx-info entity)
              (doseq [node (:nodes page)]
                (when-let [ne (build-page-node-entity node page-id doc-id)]
                  (store-page-node! tx-info ne)
                  (swap! node-count inc)))))
          (doseq [entry (:toc doc)]
            (let [entry-id (str doc-id "-toc-" (or (:id entry) (util/uuid)))]
              (db-store-toc-entry! tx-info (assoc entry :id entry-id) doc-id)))
          {:document-id doc-id
           :pages-stored page-count
           :nodes-stored @node-count
           :toc-entries-stored (count (:toc doc))})))))

;; =============================================================================
;; Aggregate DB stats + trajectory/refinement helpers
;; =============================================================================

(defn db-count-document-pages
  "Number of distinct pages for a document (via page_node.page_id)."
  [db-info doc-id]
  (or (some-> (core/query-one! db-info
                {:select [[[:count :%distinct.page_id] :cnt]]
                 :from :page_node
                 :where [:= :document_id doc-id]})
        :cnt)
    0))

(defn db-entity-type-counts
  "Returns {entity-type-keyword → count} for all entities."
  [db-info]
  (when (core/ds db-info)
    (let [rows (core/query! db-info
                 {:select [:type [[:count :*] :cnt]]
                  :from :entity
                  :group-by [:type]})]
      (into {} (map (fn [r] [(core/->kw-back (:type r)) (:cnt r)])) rows))))

(defn db-stored-docs-for-refinement
  "List docs with their page-node text for LLM refinement context."
  [db-info]
  (when (core/ds db-info)
    (let [docs (core/query! db-info {:select [:id :name] :from :document})]
      (mapv (fn [d]
              (let [doc-id (:id d)
                    nodes (core/query! db-info
                            {:select [:page_id :content]
                             :from :page_node
                             :where [:= :document_id doc-id]})]
                {:id (or doc-id (:name d))
                 :pages (mapv (fn [n]
                                {:page (or (:page_id n) "0")
                                 :text (or (:content n) "")})
                          nodes)}))
        docs))))

(defn db-document-page-nodes-full
  "All page-nodes for a doc-id with FULL content (no truncation), ordered by page-id.
   Used by fetch-document-content in tools.clj to build the combined document text."
  [db-info doc-id]
  (when (and (core/ds db-info) doc-id)
    (->> (core/query! db-info
           {:select [:page_id :content]
            :from :page_node
            :where [:= :document_id doc-id]
            :order-by [[:page_id :asc]]})
      (mapv (fn [r] {:page-id (:page_id r)
                     :content (:content r)})))))

(defn db-list-queries
  "List :query entities with optional status/min-iterations/limit filters.
   Newest-first. Used by trajectory export pipeline."
  ([db-info] (db-list-queries db-info {}))
  ([db-info {:keys [status limit min-iterations] :or {min-iterations 0}}]
   (when (core/ds db-info)
     (let [where (cond-> [:and [:= :e.type "query"]
                          [:>= :q.iterations (long min-iterations)]]
                   status (conj [:= :q.status (core/->kw status)]))
           sql (cond-> {:select [:e.id]
                        :from [[:entity :e]]
                        :join [[:query_attrs :q] [:= :e.id :q.entity_id]]
                        :where where
                        :order-by [[:e.created_at :desc] [:e.id :desc]]}
                 limit (assoc :limit (long limit)))
           ids (mapv :id (core/query! db-info sql))]
       (core/fetch-entities db-info ids)))))

(defn db-cited-page-ids
  "Given a set of cited source-ids (may be :id, :id, or :document-id),
   return the distinct :id set they resolve to."
  [db-info cited-source-ids]
  (when (and (core/ds db-info) (seq cited-source-ids))
    (set (mapv :page_id
           (core/query! db-info
             {:select-distinct [:page_id]
              :from :page_node
              :where [:and
                      [:or
                       [:in :id cited-source-ids]
                       [:in :page_id cited-source-ids]
                       [:in :document_id cited-source-ids]]
                      [:is-not :page_id nil]]})))))

(defn db-store-claim!
  [db-info claim]
  (when (core/ds db-info)
    (let [row (cond-> {:id (core/->id (or (:id claim) (util/uuid)))}
                (:text claim)        (assoc :text (:text claim))
                (:document-id claim) (assoc :document_id (:document-id claim))
                (some? (:page claim)) (assoc :page (:page claim))
                (:section claim)     (assoc :section (:section claim))
                (:quote claim)       (assoc :quote (:quote claim))
                (some? (:confidence claim)) (assoc :confidence (double (:confidence claim)))
                (:query-id claim)    (assoc :query_id (core/->id (:query-id claim)))
                (some? (:verified? claim))  (assoc :verified (if (:verified? claim) 1 0))
                (:verification-verdict claim) (assoc :verification_verdict (:verification-verdict claim))
                (:created-at claim)  (assoc :created_at (core/->epoch-ms (:created-at claim))))]
      (jdbc/execute! (core/ds db-info)
        (sql/format {:insert-into :claim :values [row]
                     :on-conflict [:id]
                     :do-update-set (vec (remove #{:id} (keys row)))})))))

;; =============================================================================
;; results->markdown (pure formatter)
;; =============================================================================

(defn results->markdown
  "Compact markdown rendering of search results. Accepts a vec of page-nodes or
   a {:pages :toc :entities} map."
  [results]
  (let [{:keys [pages toc entities]}
        (if (map? results) results {:pages results})
        sb (StringBuilder.)]
    (when (seq pages)
      (let [by-page (group-by :page-id pages)]
        (doseq [[pid nodes] (sort-by key by-page)]
          (.append sb (str "## " (or pid "unknown") "\n"))
          (doseq [n nodes]
            (let [t (some-> (:type n) name)
                  zone (some-> (:vitality-zone n) name)
                  preview (or (:preview n) "")]
              (.append sb (str "- **" t "**" (when zone (str " [" zone "]")) " " preview "\n"))))
          (.append sb "\n"))))
    (when (seq toc)
      (.append sb "## TOC\n")
      (doseq [e toc]
        (.append sb (str "- " (or (:level e) "") " " (or (:title e) "")
                      (when-let [p (:target-page e)] (str " (p." p ")"))
                      "\n")))
      (.append sb "\n"))
    (when (seq entities)
      (.append sb "## Entities\n")
      (doseq [e entities]
        (.append sb (str "- **" (or (:name e) "") "** ("
                      (some-> (:type e) name) ") "
                      (rt-shared/truncate (or (:description e) "") 80) "\n")))
      (.append sb "\n"))
    (str sb)))
