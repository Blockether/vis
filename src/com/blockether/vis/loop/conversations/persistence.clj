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
