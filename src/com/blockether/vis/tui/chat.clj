(ns com.blockether.vis.tui.chat
  "Conversation state and LLM integration via svar RLM.
   Uses svar's persistent Datalevin DB at ~/.vis/rlm for conversation history."
  (:require [com.blockether.vis.config :as config]
            [com.blockether.svar.internal.llm :as llm]
            [com.blockether.svar.internal.rlm :as rlm]
            [com.blockether.svar.internal.rlm.db :as rlm-db]
            [taoensso.telemere :as t]))

(def ^:private rlm-path (str (System/getProperty "user.home") "/.vis/rlm"))

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
  "Load the prior conversation from svar's Datalevin DB. Uses the entity model
   (conversation → query → iteration) via db-query-history. Returns the user/
   assistant message list in chronological order, or [] on any failure."
  [env]
  (try
    (let [db-info  (when-let [a (:db-info-atom env)] @a)
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
  "Create an RLM environment with persistent storage at ~/.vis/rlm.
   `provider-config` is accepted for backwards compatibility with TUI callers but
   we route through the shared router from config.clj so the whole process reuses
   one circuit-breakered client. Returns {:env env, :history [structured-messages]}."
  [_provider-config]
  (let [router  (config/get-router)
        env     (rlm/create-env router {:db rlm-path :conversation :latest})
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
  "Dispose the RLM environment. Persistent data at ~/.vis/rlm is preserved."
  [{:keys [env]}]
  (rlm/dispose-env! env))
