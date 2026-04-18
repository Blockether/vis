(ns com.blockether.vis.loop.storage.sqlite.search
  "FTS5-powered search over page nodes and TOC entries with vitality-weighted
   reranking and cooccurrence boosts."
  (:require
   [clojure.string :as str]
   [com.blockether.vis.loop.storage.sqlite.core :as core]
   [com.blockether.vis.loop.storage.sqlite.corpus :as corpus]
   [com.blockether.vis.loop.storage.sqlite.vitality :as vit]))

(defn- fts-search
  "Run an FTS5 MATCH against the unified search table, optionally scoped to
   owner_table values. Returns [{:owner_table :owner_id :rank}] ordered by rank."
  [db-info owner-tables match-query {:keys [limit] :or {limit 50}}]
  (let [stmt (if (seq owner-tables)
               (let [placeholders (str/join "," (repeat (count owner-tables) "?"))
                     sql (str "SELECT owner_table AS owner_table, owner_id AS owner_id, "
                           "bm25(search) AS rank "
                           "FROM search "
                           "WHERE search MATCH ? "
                           "AND owner_table IN (" placeholders ") "
                           "ORDER BY rank ASC "
                           "LIMIT ?")]
                 (into [sql] (concat [match-query] owner-tables [limit])))
               ["SELECT owner_table AS owner_table, owner_id AS owner_id, bm25(search) AS rank FROM search WHERE search MATCH ? ORDER BY rank ASC LIMIT ?"
                match-query
                limit])]
    (try
      (core/query! db-info stmt)
      (catch Exception _ []))))

(defn- fts-query-for-substring
  "Convert a free-form user query into an FTS5 MATCH expression.
   Uses prefix match on each token to approximate substring semantics,
   and falls back to quoted-literal for unusual chars."
  [q]
  (when (and q (not (str/blank? q)))
    (let [tokens (->> (str/split (str/lower-case q) #"\s+")
                   (remove str/blank?)
                   (map (fn [t] (str/replace t #"[^a-z0-9]" ""))) ; alnum only
                   (remove str/blank?))]
      (when (seq tokens)
        (str/join " " (map (fn [t] (str t "*")) tokens))))))

(defn- brevify-node
  "Return brief form with 150-char preview + vitality fields preserved."
  [node]
  (let [content (or (:content node) (:description node) "")
        preview (if (> (count content) 150)
                  (str (subs content 0 150) "...")
                  content)]
    (-> node
      (dissoc :content :description)
      (assoc :preview preview :content-length (count content)))))

(defn db-search-page-nodes
  "Search page nodes with vitality-weighted reranking (0.7 relevance + 0.3 vitality + cooc)."
  ([db-info query] (db-search-page-nodes db-info query {}))
  ([db-info query {:keys [top-k document-id type min-vitality]
                   :or {top-k 10 min-vitality 0.1}}]
   (if (str/blank? (str query))
     (mapv brevify-node (corpus/db-list-page-nodes db-info {:document-id document-id :type type :limit top-k}))
     (when (core/ds db-info)
       (let [match (fts-query-for-substring query)]
         (when match
           (let [hits (fts-search db-info ["page_node"] match {:limit (* 4 top-k)})
                 ids (distinct (mapv :owner_id hits))
                 nodes (when (seq ids)
                         (mapv corpus/node-row->ns
                           (core/query! db-info
                             {:select [:*] :from :page_node
                              :where [:in :id ids]})))
                 filtered (->> nodes
                            (filter #(or (nil? document-id) (= document-id (:document-id %))))
                            (filter #(or (nil? type) (= (core/->kw type) (core/->kw (:type %))))))
                 page-v-cache (atom {})
                 cached-v (fn [page-id]
                            (or (get @page-v-cache page-id)
                              (let [v (or (vit/get-page-vitality db-info page-id) {:score 1.0 :zone :active})]
                                (swap! page-v-cache assoc page-id v)
                                v)))
                 recent-pages (or (vit/recently-accessed-page-ids db-info) #{})
                 result-page-ids (distinct (keep :page-id filtered))
                 cooc-map (if (and (seq recent-pages) (seq result-page-ids))
                            (try (vit/batch-cooccurrence-boosts db-info result-page-ids recent-pages)
                              (catch Exception _ {}))
                            {})
                 total (max 1 (count filtered))
                 ranked (->> filtered
                          (map-indexed
                            (fn [idx node]
                              (let [pid (:page-id node)
                                    pv (cached-v pid)
                                    qv (corpus/get-page-q-value db-info pid)
                                    nv (vit/compute-node-vitality (:score pv) (:type node) qv)
                                    relevance (- 1.0 (/ (double idx) total))
                                    cooc-bonus (* 0.05 (get cooc-map pid 0.0))
                                    combined (+ (* 0.7 relevance) (* 0.3 (:score nv)) cooc-bonus)]
                                (assoc node
                                  ::combined combined
                                  :vitality-score (:score nv)
                                  :vitality-zone (:zone nv)))))
                          (filter #(>= (:vitality-score %) min-vitality))
                          (sort-by ::combined #(compare %2 %1))
                          (take top-k))]
             (mapv #(-> % (dissoc ::combined) brevify-node) ranked))))))))

(defn db-search-batch
  "Parallel multi-query search. Dedupes nodes by id, keeps highest vitality."
  ([db-info queries] (db-search-batch db-info queries {}))
  ([db-info queries {:keys [top-k limit document-id min-vitality]
                     :or {top-k 10 limit 30 min-vitality 0.1}}]
   (when (seq queries)
     (let [per-q (cond-> {:top-k top-k :min-vitality min-vitality}
                   document-id (assoc :document-id document-id))
           all-results (into [] (mapcat identity)
                         (pmap #(db-search-page-nodes db-info % per-q) queries))
           deduped (vals (reduce (fn [acc node]
                                   (let [id (:id node)
                                         existing (get acc id)]
                                     (if (or (nil? existing)
                                           (> (or (:vitality-score node) 0)
                                             (or (:vitality-score existing) 0)))
                                       (assoc acc id node)
                                       acc)))
                           {} all-results))]
       (->> deduped
         (sort-by #(- (or (:vitality-score %) 0)))
         (take limit)
         vec)))))

(defn db-search-toc-entries
  ([db-info query] (db-search-toc-entries db-info query {}))
  ([db-info query {:keys [top-k] :or {top-k 10}}]
   (if (str/blank? (str query))
     (corpus/db-list-toc-entries db-info {:limit top-k})
     (when (core/ds db-info)
       (when-let [match (fts-query-for-substring query)]
         (let [hits (fts-search db-info ["document_toc"] match {:limit (* 3 top-k)})
               ids (distinct (mapv :owner_id hits))]
           (when (seq ids)
             (->> (core/query! db-info {:select [:*] :from :document_toc :where [:in :id ids]})
               (take top-k)
               (mapv corpus/toc-row->ns)))))))))
