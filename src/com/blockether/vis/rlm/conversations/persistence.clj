(ns com.blockether.vis.rlm.conversations.persistence
  "Conversation persistence boundary for sidecar metadata and env DB handle."
  (:require [com.blockether.vis.config :as config]
            [com.blockether.vis.rlm.persistence.db :as rlm-db]
            [next.jdbc :as jdbc]
            [next.jdbc.sql :as sql])
  (:import [java.time Instant]))

(def ^:private DDL
  ["CREATE TABLE IF NOT EXISTS vis_conversation (
      conversation_id TEXT PRIMARY KEY NOT NULL,
      channel         TEXT NOT NULL,
      external_id     TEXT,
      title           TEXT,
      created_at      INTEGER NOT NULL
    )"
   "CREATE INDEX IF NOT EXISTS idx_vis_conv_channel ON vis_conversation(channel, created_at DESC)"
   "CREATE UNIQUE INDEX IF NOT EXISTS uniq_vis_conv_external
      ON vis_conversation(channel, external_id)
      WHERE external_id IS NOT NULL"])

(defonce ^:private shared-db (atom nil))
(defonce ^:private schema-installed? (atom false))

(defn install-schema!
  [{:keys [datasource]}]
  (with-open [conn (jdbc/get-connection datasource)]
    (doseq [stmt DDL]
      (jdbc/execute! conn [stmt]))))

(defn db-info
  []
  (or @shared-db
    (swap! shared-db
      (fn [cur]
        (or cur
          (let [d (rlm-db/create-rlm-conn config/db-path)]
            (when-not @schema-installed?
              (install-schema! d)
              (reset! schema-installed? true))
            d))))))

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
    (reset! shared-db nil)
    (reset! schema-installed? false)))
