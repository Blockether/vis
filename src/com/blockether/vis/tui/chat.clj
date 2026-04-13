(ns com.blockether.vis.tui.chat
  "Conversation state and LLM integration via svar RLM.

   The TUI has one long-lived conversation named `tui:default` inside the
   shared `~/.vis/vis.mdb` SQLite DB (see `config/db-path`). Telegram
   chats and web sessions live alongside it under different names
   (`telegram:*`, `session:*`) — svar resolves them by `:conversation/name`,
   so the TUI stays isolated from the others despite the shared DB."
  (:require [com.blockether.vis.config :as config]
            [com.blockether.svar.internal.llm :as llm]
            [com.blockether.svar.internal.rlm :as rlm]
            [com.blockether.svar.internal.rlm.db :as rlm-db]
            [taoensso.telemere :as t]))

(defn user-msg
  "Create a structured user message with timestamp."
  ([text] (user-msg text (java.util.Date.)))
  ([text timestamp]
   {:role :user :text text :timestamp timestamp}))

(defn assistant-msg
  "Create a structured assistant (vis) message with timestamp."
  ([text] (assistant-msg text (java.util.Date.)))
  ([text timestamp]
   {:role :assistant :text (if (string? text) text (pr-str text)) :timestamp timestamp}))

(defn- load-history
  "Load the prior conversation from svar's SQLite DB. Uses the entity model
   (conversation → query → iteration) via db-query-history. Returns the user/
   assistant message list in chronological order, or [] on any failure."
  [env]
  (try
    (let [db-info  (:db-info env)
          conv-ref (:conversation-ref env)
          history  (when (and db-info conv-ref)
                     (rlm-db/db-query-history db-info conv-ref))]
      (vec (mapcat (fn [{:keys [text answer-preview]}]
                     (cond-> []
                       (seq text)           (conj (user-msg text))
                       (seq answer-preview) (conj (assistant-msg answer-preview))))
                   history)))
    (catch Exception e
      (t/log! :warn (str "load-history failed: " (ex-message e)))
      [])))

(defn make-conversation
  "Open the TUI's long-lived conversation (name `tui:default`) on the shared
   `~/.vis/vis.mdb` SQLite DB. First launch creates it; subsequent launches
   resolve the existing one so message history and var registry persist.
   `provider-config` is accepted for caller compat — we always route through
   the shared router from config.clj. Returns {:env env :history [msgs]}."
  [_provider-config]
  (let [router (config/get-router)
        env    (rlm/create-env router
                 {:db config/db-path
                  :conversation {:name (config/conversation-name :tui)}})
        history (load-history env)]
    {:env env :history history}))

(defn query!
  "Send query to RLM. Blocking. Returns {:answer str} or {:error str}."
  [conv text]
  (try
    (let [result (rlm/query-env! (:env conv) [(llm/user text)])
          answer (or (:answer result) "[empty response]")]
      {:answer (if (string? answer) answer (pr-str answer))})
    (catch Exception e
      (t/log! :error (str "Query failed: " (ex-message e)))
      {:error (str "error: " (ex-message e))})))

(defn dispose!
  "Release the RLM env handle. Persistent data in `~/.vis/vis.mdb` stays;
   svar leaves the shared SQLite conn open for sibling envs."
  [{:keys [env]}]
  (rlm/dispose-env! env))
