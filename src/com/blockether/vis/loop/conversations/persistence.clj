(ns com.blockether.vis.loop.conversations.persistence
  "Conversation persistence boundary for sidecar metadata and env DB handle."
  (:require [com.blockether.vis.config :as config]
            [com.blockether.vis.loop.storage.db :as rlm-db]
            [next.jdbc.sql :as sql])
  (:import [java.time Instant]))

(defonce ^:private shared-db (atom nil))

(defn db-info
  []
  (or @shared-db
    (swap! shared-db
      (fn [cur]
        (or cur
          (rlm-db/create-rlm-conn config/db-path))))))

(defn row->conversation [row]
  (when row
    {:id          (:vis_conversation/conversation_id row)
     :channel     (keyword (:vis_conversation/channel row))
     :external-id (:vis_conversation/external_id row)
     :title       (:vis_conversation/title row)
     :created-at  (:vis_conversation/created_at row)}))

(defn insert-conversation!
  [db {:keys [id channel external-id title]}]
  (sql/insert! (:datasource db) :vis_conversation
    {:conversation_id id
     :channel         (name channel)
     :external_id     external-id
     :title           title
     :created_at      (.toEpochMilli (Instant/now))}))

(defn find-conversation-by-id
  [db id]
  (row->conversation
    (sql/get-by-id (:datasource db) :vis_conversation id :conversation_id {})))

(defn find-conversation-by-external
  [db channel external-id]
  (row->conversation
    (first (sql/find-by-keys (:datasource db) :vis_conversation
             {:channel (name channel)
              :external_id (str external-id)}))))

(defn list-conversations
  [db channel]
  (->> (sql/find-by-keys (:datasource db) :vis_conversation
         {:channel (name channel)}
         {:order-by [[:created_at :desc]]})
    (mapv row->conversation)))

(defn update-conversation-title!
  [db id title]
  (sql/update! (:datasource db) :vis_conversation
    {:title title}
    {:conversation_id id}))

(defn delete-conversation-row!
  [db id]
  (sql/delete! (:datasource db) :vis_conversation
    {:conversation_id id}))

(defn dispose-shared-db!
  []
  (when-let [d @shared-db]
    (try (rlm-db/dispose-rlm-conn! d) (catch Exception _ nil))
    (reset! shared-db nil)))

(defn sweep-orphaned-running-queries!
  "Reconciles `query_attrs` rows left in `:running` state by a prior
   process that was killed mid-turn. Safe to call at startup BEFORE any
   new query is accepted — no live turn exists yet in a fresh process,
   so every `:running` row is by definition an orphan.

   Rewrites status → `:interrupted` and stamps a user-visible answer so
   the UI renders a distinct banner instead of showing a stuck spinner
   or treating the row as `:success`. Iteration/trace rows are left
   untouched; the user can still read how far the agent got.

   Returns the number of queries that were swept."
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
