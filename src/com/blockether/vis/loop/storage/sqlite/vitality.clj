(ns com.blockether.vis.loop.storage.sqlite.vitality
  "Document certainty (Bayesian α/β), page vitality, node vitality, page
   access recording, cooccurrence edges, and activation propagation."
  (:require
   [com.blockether.vis.loop.storage.sqlite.core :as core]
   [honey.sql :as sql]
   [next.jdbc :as jdbc]
   [taoensso.trove :as trove])
  (:import
   (java.util Date)))

;; =============================================================================
;; Document certainty (Bayesian)
;; =============================================================================

(defn- days-since-ms ^double [now-ms past-ms]
  (if past-ms
    (/ (- (double now-ms) (double past-ms)) 86400000.0)
    0.0))

(defn document-certainty
  "Computes Bayesian certainty = alpha / (alpha + effective-beta)."
  [db-info doc-id]
  (when (core/ds db-info)
    (let [row (core/query-one! db-info
                {:select [:certainty_alpha :certainty_beta :updated_at :created_at]
                 :from :document :where [:= :id doc-id]})
          alpha (or (:certainty_alpha row) 2.0)
          stored-beta (or (:certainty_beta row) 1.0)
          updated-ms (or (:updated_at row) (:created_at row))
          days-since (days-since-ms (core/now-ms) updated-ms)
          effective-beta (+ stored-beta (* 0.01 days-since))]
      (when (:certainty_alpha row)
        {:certainty (/ alpha (+ alpha effective-beta))
         :alpha alpha
         :beta effective-beta}))))

(def ^:private ^:const CERTAINTY-NORM-THRESHOLD 50.0)
(def ^:private ^:const CERTAINTY-NORM-TARGET 20.0)

(defn- normalize-certainty-params! [db-info doc-id]
  (let [row (core/query-one! db-info
              {:select [:certainty_alpha :certainty_beta]
               :from :document :where [:= :id doc-id]})
        alpha (or (:certainty_alpha row) 2.0)
        beta (or (:certainty_beta row) 1.0)
        total (+ alpha beta)]
    (when (> total CERTAINTY-NORM-THRESHOLD)
      (let [scale (/ CERTAINTY-NORM-TARGET total)]
        (jdbc/execute! (core/ds db-info)
          (sql/format
            {:update :document
             :set {:certainty_alpha (* alpha scale)
                   :certainty_beta (* beta scale)}
             :where [:= :id doc-id]}))))))

(defn record-document-access!
  ([db-info doc-id] (record-document-access! db-info doc-id 1.0))
  ([db-info doc-id boost]
   (when (core/ds db-info)
     (try
       (let [row (core/query-one! db-info
                   {:select [:certainty_alpha] :from :document :where [:= :id doc-id]})
             alpha (or (:certainty_alpha row) 2.0)]
         (jdbc/execute! (core/ds db-info)
           (sql/format
             {:update :document
              :set {:certainty_alpha (+ alpha (double boost))}
              :where [:= :id doc-id]}))
         (normalize-certainty-params! db-info doc-id))
       (catch Exception e
         (trove/log! {:level :warn :data {:doc-id doc-id :error (ex-message e)}
                      :msg "Failed to record document access"}))))))

(defn decay-document-certainty!
  [db-info doc-id]
  (when (core/ds db-info)
    (try
      (let [row (core/query-one! db-info
                  {:select [:certainty_beta :updated_at :created_at]
                   :from :document :where [:= :id doc-id]})
            beta (or (:certainty_beta row) 1.0)
            updated-ms (or (:updated_at row) (:created_at row))
            days-since (days-since-ms (core/now-ms) updated-ms)
            beta-inc (* 0.01 days-since)]
        (when (> beta-inc 0.001)
          (jdbc/execute! (core/ds db-info)
            (sql/format
              {:update :document
               :set {:certainty_beta (+ beta beta-inc)}
               :where [:= :id doc-id]}))))
      (catch Exception e
        (trove/log! {:level :warn :data {:doc-id doc-id :error (ex-message e)}
                     :msg "Failed to decay document certainty"})))))

(defn reindex-certainty-jump!
  ([db-info doc-id] (reindex-certainty-jump! db-info doc-id 5.0))
  ([db-info doc-id jump]
   (when (core/ds db-info)
     (try
       (let [row (core/query-one! db-info
                   {:select [:certainty_beta] :from :document :where [:= :id doc-id]})
             beta (or (:certainty_beta row) 1.0)]
         (jdbc/execute! (core/ds db-info)
           (sql/format
             {:update :document
              :set {:certainty_beta (+ beta (double jump))}
              :where [:= :id doc-id]}))
         (normalize-certainty-params! db-info doc-id))
       (catch Exception e
         (trove/log! {:level :warn :data {:doc-id doc-id :error (ex-message e)}
                      :msg "Failed to apply reindex certainty jump"}))))))

;; =============================================================================
;; Vitality (pure computations + page-level lookup)
;; =============================================================================

(def ^:private METABOLIC-RATES
  {:section 0.3 :heading 0.3 :paragraph 1.0 :list-item 1.0 :toc-entry 0.1
   :table 0.5 :image 0.5 :header 0.8 :footer 0.8 :metadata 0.3 :entity 0.8})

(def ^:private VITALITY-ZONES
  [[0.6 :active] [0.3 :stale] [0.1 :fading] [0.0 :archived]])

(defn vitality-zone [score]
  (or (some (fn [[t z]] (when (>= score t) z)) VITALITY-ZONES) :archived))

(defn compute-page-vitality
  ([access-count created-at last-accessed children-count]
   (compute-page-vitality access-count created-at last-accessed children-count (Date.)))
  ([access-count _created-at last-accessed children-count now]
   (let [d 0.5
         now-ms (core/->epoch-ms now)
         la-ms (core/->epoch-ms last-accessed)
         recency-ms (max 1 (- (long now-ms) (long la-ms)))
         recency-days (max 0.001 (/ recency-ms 86400000.0))
         access (max 0.01 (double access-count))
         B (- (Math/log (/ access (- 1.0 d)))
             (* d (Math/log recency-days)))
         base (/ 1.0 (+ 1.0 (Math/exp (- B))))
         boost (+ 1.0 (* 0.05 (min children-count 10)))
         score (min 1.0 (* base boost))]
     {:score score :zone (vitality-zone score)})))

(defn compute-node-vitality
  ([page-score node-type] (compute-node-vitality page-score node-type 0.5))
  ([page-score node-type q-value]
   (let [base-rate (get METABOLIC-RATES (core/->kw-back (core/->kw node-type)) 1.0)
         q-bonus (* (- (double q-value) 0.5) 0.4)
         metabolic (max 0.01 (* base-rate (- 1.0 q-bonus)))
         eff (if (pos? page-score) (Math/pow page-score metabolic) 0.0)]
     {:score eff :zone (vitality-zone eff)})))

(defn get-page-vitality
  [db-info page-id]
  (when (core/ds db-info)
    (let [row (core/query-one! db-info
                {:select [:created_at :last_accessed :access_count :idx :document_id]
                 :from :page :where [:= :id page-id]})
          children-count (or (some-> (core/query-one! db-info
                                       {:select [[[:count :*] :cnt]]
                                        :from :page_node
                                        :where [:= :page_id page-id]})
                               :cnt)
                           0)]
      (when (:created_at row)
        (let [created-at (core/->date (:created_at row))
              la (core/->date (or (:last_accessed row) (:created_at row)))
              {:keys [score]} (compute-page-vitality
                                (or (:access_count row) 0.0)
                                created-at la children-count)
              doc-cert (when-let [doc-id (:document_id row)]
                         (:certainty (document-certainty db-info doc-id)))
              final-score (if doc-cert (min 1.0 (* score doc-cert)) score)]
          {:score final-score
           :zone (vitality-zone final-score)
           :access-count (or (:access_count row) 0.0)
           :last-accessed la
           :created-at created-at
           :children-count children-count})))))

;; =============================================================================
;; Cooccurrence
;; =============================================================================

(def ^:private ^:const COOC-DECAY-TAU 7.0)
(def ^:private ^:const COOC-MAX-PAIRS 20)

(defn- cooc-decay ^double [^double strength ^double days]
  (* strength (Math/exp (- (/ days COOC-DECAY-TAU)))))

(defn- cooc-pair [a b]
  (let [[x y] (if (neg? (compare a b)) [a b] [b a])]
    [x y (str x "|" y)]))

(defn record-cooccurrence!
  [db-info pa pb]
  (when (and (core/ds db-info) (not= pa pb))
    (try
      (let [[a b id] (cooc-pair pa pb)
            row (core/query-one! db-info
                  {:select [:strength :last_seen] :from :page_cooccurrence :where [:= :id id]})
            old-s (or (:strength row) 0.0)
            last-s (:last_seen row)
            days (days-since-ms (core/now-ms) last-s)
            new-s (+ (cooc-decay old-s days) 1.0)]
        (jdbc/execute! (core/ds db-info)
          (sql/format
            {:insert-into :page_cooccurrence
             :values [{:id id :page_a a :page_b b :strength new-s :last_seen (core/now-ms)}]
             :on-conflict [:id]
             :do-update-set [:page_a :page_b :strength :last_seen]})))
      (catch Exception e
        (trove/log! {:level :debug :data {:pa pa :pb pb :error (ex-message e)}
                     :msg "Cooc record failed (non-fatal)"})))))

(defn record-cooccurrences!
  [db-info page-ids]
  (let [ids (vec (take COOC-MAX-PAIRS (distinct page-ids)))]
    (when (> (count ids) 1)
      (doseq [i (range (count ids))
              j (range (inc i) (count ids))]
        (record-cooccurrence! db-info (nth ids i) (nth ids j))))))

(defn recently-accessed-page-ids
  "Set of page IDs accessed within the last hour."
  [db-info]
  (when (core/ds db-info)
    (try
      (let [cutoff (- (core/now-ms) 3600000)]
        (set (mapv :id
               (core/query! db-info
                 {:select [:id] :from :page :where [:>= :last_accessed cutoff]}))))
      (catch Exception _ #{}))))

(defn batch-cooccurrence-boosts
  "Returns {page-id -> total-decayed-boost} for result pages vs recent pages."
  [db-info result-page-ids recent-page-ids]
  (let [now (core/now-ms)
        all (set (concat result-page-ids recent-page-ids))]
    (if (empty? all)
      {}
      (let [edges (core/query! db-info
                    {:select [:page_a :page_b :strength :last_seen]
                     :from :page_cooccurrence
                     :where [:and [:in :page_a all] [:in :page_b all]]})
            recent-set (set recent-page-ids)]
        (reduce (fn [acc {:keys [page_a page_b strength last_seen]}]
                  (let [result-pid (cond
                                     (and (recent-set page_b) (not (recent-set page_a))) page_a
                                     (and (recent-set page_a) (not (recent-set page_b))) page_b
                                     :else nil)]
                    (if (and result-pid strength)
                      (update acc result-pid (fnil + 0.0)
                        (cooc-decay strength (days-since-ms now last_seen)))
                      acc)))
          {} edges)))))

(defn get-cooccurrence-boost
  [db-info page-id recent-page-ids]
  (if (or (empty? recent-page-ids) (not (core/ds db-info)))
    0.0
    (let [now (core/now-ms)]
      (double
        (reduce (fn [acc recent-pid]
                  (if (= page-id recent-pid)
                    acc
                    (let [[_ _ id] (cooc-pair page-id recent-pid)
                          row (core/query-one! db-info
                                {:select [:strength :last_seen] :from :page_cooccurrence
                                 :where [:= :id id]})]
                      (if-let [s (:strength row)]
                        (+ acc (cooc-decay s (days-since-ms now (:last_seen row))))
                        acc))))
          0.0 recent-page-ids)))))

(defn propagate-activation!
  "Spread activation from `page-id` to nearby pages via cooccurrence only."
  ([db-info page-id weight] (propagate-activation! db-info page-id weight 0.6))
  ([db-info page-id weight damping]
   (when (and (core/ds db-info) (pos? (double weight)))
     (try
       (let [cooc-neighbors (core/query! db-info
                              {:select [:page_a :page_b :strength]
                               :from :page_cooccurrence
                               :where [:or [:= :page_a page-id] [:= :page_b page-id]]})
             all-connected (->> cooc-neighbors
                             (keep (fn [{:keys [page_a page_b]}]
                                     (cond
                                       (= page_a page-id) page_b
                                       (= page_b page-id) page_a
                                       :else nil)))
                             distinct
                             (remove #(= % page-id)))]
         (doseq [pid all-connected]
           (let [boost (* weight damping)
                 row (core/query-one! db-info
                       {:select [:access_count] :from :page :where [:= :id pid]})
                 cur (or (:access_count row) 0.0)]
             (jdbc/execute! (core/ds db-info)
               (sql/format
                 {:update :page
                  :set {:last_accessed (core/now-ms) :access_count (+ cur boost)}
                  :where [:= :id pid]})))))
       (catch Exception e
         (trove/log! {:level :debug :data {:page-id page-id :error (ex-message e)}
                      :msg "Spreading activation failed (non-fatal)"}))))))

;; =============================================================================
;; Page access (propagates activation via cooccurrence; boosts doc certainty)
;; =============================================================================

(defn record-page-access!
  "Update page last-accessed/access-count, boost document certainty, propagate activation."
  [db-info page-id weight]
  (when (core/ds db-info)
    (try
      (let [row (core/query-one! db-info
                  {:select [:access_count :document_id] :from :page :where [:= :id page-id]})
            cur (or (:access_count row) 0.0)]
        (jdbc/execute! (core/ds db-info)
          (sql/format
            {:update :page
             :set {:last_accessed (core/now-ms)
                   :access_count (+ cur (double weight))}
             :where [:= :id page-id]}))
        (when (and (>= weight 1.0) (:document_id row))
          (record-document-access! db-info (:document_id row) 0.5))
        (when (pos? (double weight))
          (try
            (propagate-activation! db-info page-id (double weight))
            (catch Exception e
              (trove/log! {:level :debug
                           :data {:page-id page-id :error (ex-message e)}
                           :msg "Activation propagation failed (non-fatal)"})))))
      (catch Exception e
        (trove/log! {:level :warn :data {:page-id page-id :error (ex-message e)}
                     :msg "Failed to record page access"})))))
