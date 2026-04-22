(ns com.blockether.vis.loop.runtime.conversation.persistence
  "Conversation persistence boundary for sidecar metadata and env DB handle."
  (:require [com.blockether.vis.config :as config]
            [com.blockether.vis.loop.storage.db :as rlm-db]
            [honey.sql :as sql]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs])
  (:import [java.time Instant]))

(defonce ^:private shared-db (atom nil))

(defn db-info []
  (or @shared-db
    (swap! shared-db
      (fn [cur]
        (or cur (rlm-db/create-rlm-conn config/db-path))))))

(defn- ds [db] (:datasource db))

(def ^:private jdbc-opts {:builder-fn rs/as-unqualified-lower-maps})

(defn- q! [db sql-map]
  (jdbc/execute! (ds db) (sql/format sql-map) jdbc-opts))

(defn- q1! [db sql-map]
  (jdbc/execute-one! (ds db) (sql/format sql-map) jdbc-opts))

(defn- row->conversation [row]
  (when row
    {:id          (:conversation_id row)
     :channel     (keyword (:channel row))
     :external-id (:external_id row)
     :title       (:title row)
     :created-at  (:created_at row)}))

(defn insert-conversation!
  [db {:keys [id channel external-id title]}]
  (q1! db {:insert-into :vis_conversation
           :values [{:conversation_id id
                     :channel         (name channel)
                     :external_id     external-id
                     :title           title
                     :created_at      (.toEpochMilli (Instant/now))}]}))

(defn find-conversation-by-id [db id]
  (row->conversation
    (q1! db {:select [:*]
             :from   [:vis_conversation]
             :where  [:= :conversation_id id]})))

(defn find-conversation-by-external [db channel external-id]
  (row->conversation
    (q1! db {:select [:*]
             :from   [:vis_conversation]
             :where  [:and
                      [:= :channel (name channel)]
                      [:= :external_id (str external-id)]]})))

(defn list-conversations [db channel]
  (->> (q! db {:select   [:*]
               :from     [:vis_conversation]
               :where    [:= :channel (name channel)]
               :order-by [[:created_at :desc]]})
    (mapv row->conversation)))

(defn update-conversation-title! [db id title]
  (q1! db {:update :vis_conversation
           :set    {:title title}
           :where  [:= :conversation_id id]}))

(defn delete-conversation-row! [db id]
  (q1! db {:delete-from :vis_conversation
           :where       [:= :conversation_id id]}))

(defn dispose-shared-db! []
  (when-let [d @shared-db]
    (try (rlm-db/dispose-rlm-conn! d) (catch Exception _ nil))
    (reset! shared-db nil)))

(defn sweep-orphaned-running-queries!
  "Reconciles `query_attrs` rows left in `:running` state by a prior
   process that was killed mid-turn. Rewrites status → `:interrupted`."
  ([] (sweep-orphaned-running-queries! (db-info)))
  ([db]
   (let [orphans (try
                   (rlm-db/db-list-queries db {:status :running})
                   (catch Exception _ []))
         answer  "⚠️ Turn interrupted — the server was restarted before this answer could finalize. Re-send the message to retry."]
     (doseq [{:keys [id iterations duration-ms]} orphans]
       (try
         (rlm-db/update-query! db [:id id]
           {:answer      answer
            :iterations  (or iterations 0)
            :duration-ms (or duration-ms 0)
            :status      :interrupted})
         (catch Exception _ nil)))
     (count orphans))))
