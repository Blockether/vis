(ns com.blockether.vis.conversations.schema
  "Sidecar table that adds vis-level channel metadata to svar conversations.

   `vis_conversation` is keyed by the rlm-generated conversation entity-id
   (a UUID string). It carries the vis channel (`'vis'` — web + TUI — or
   `'telegram'`), the optional external id (Telegram chat-id for
   `:telegram`, NULL for `:vis`), and the display title.

   `UNIQUE(channel, external_id)` lets us find-or-create a conversation by
   `(channel, external-id)` — that's how Telegram resolves a chat-id to a
   conversation without any name-prefix gymnastics."
  (:require [next.jdbc :as jdbc]))

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

(defn install!
  "Idempotently install the vis_conversation table on the shared DataSource."
  [{:keys [datasource]}]
  (with-open [conn (jdbc/get-connection datasource)]
    (doseq [stmt DDL]
      (jdbc/execute! conn [stmt]))))
