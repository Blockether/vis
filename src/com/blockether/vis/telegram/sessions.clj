(ns com.blockether.vis.telegram.sessions
  "Per-chat svar environments for the Telegram bot.

   All chats share the single `~/.vis/vis.mdb` Datalevin DB (see
   `config/db-path`). Each chat is a distinct :conversation inside that DB,
   named `telegram:<chat-id>` via svar's `{:conversation {:name …}}` lookup
   — first message creates it, subsequent messages resume it. svar's
   conversation/query/iteration hierarchy is the storage; we don't ingest
   text messages as PageIndex documents (that would duplicate `:query/text`,
   which svar already fulltext-indexes).

   Envs are opened lazily on the first message in each chat and cached in
   the process-level `sessions` atom. The Datalevin conn itself is shared
   across envs (LMDB single-writer) — svar's dispose leaves persistent
   conns open so disposing one chat's env doesn't break the others."
  (:require [com.blockether.vis.agent :as agent]
            [com.blockether.vis.config :as config]
            [com.blockether.svar.internal.llm :as llm]
            [com.blockether.svar.internal.rlm :as rlm]))

;; chat-id (string) → {:env env :lock Object}
(defonce sessions (atom {}))

(defn- register-base-tools! [env]
  (reduce (fn [e {:keys [sym fn] :as tool-def}]
            (rlm/register-env-fn! e sym fn (dissoc tool-def :sym :fn)))
          env
          agent/base-tools))

(defn- create-env! [chat-id]
  (config/resolve-config nil)                 ;; throw early if no provider configured
  (let [router (config/get-router)
        ;; Stable name — svar lookup-or-create:
        ;;  - first time: creates :conversation with this name, fresh history
        ;;  - next time:  resolves to the existing conversation, svar rehydrates
        ;;                message history AND the persistent var registry
        name   (config/conversation-name :telegram chat-id)
        env    (rlm/create-env router {:db config/db-path
                                       :conversation {:name name}})]
    (register-base-tools! env)))

(defn- telegram-system-prompt []
  (str "You are vis over Telegram. Short replies, minimal markdown.

"
       (agent/default-system-prompt)
       (agent/environment-info)))

(defn- get-or-create-session! [chat-id]
  (or (get @sessions chat-id)
      ;; Double-checked update under swap! to avoid creating two envs for the
      ;; same chat if two messages race in.
      (-> (swap! sessions
                 (fn [m]
                   (if (contains? m chat-id)
                     m
                     (assoc m chat-id {:env  (create-env! chat-id)
                                       :lock (Object.)}))))
          (get chat-id))))

(defn handle-message!
  "Run one Telegram message through the chat's svar env and return the result
   map (`{:answer :iterations :duration-ms :tokens :cost ...}`). Concurrent
   messages from the same chat are serialized via a per-chat lock so svar
   only sees one query in flight per conversation."
  [chat-id text]
  (let [{:keys [env lock]} (get-or-create-session! (str chat-id))]
    (locking lock
      (rlm/query-env! env
                      [(llm/user text)]
                      {:system-prompt (telegram-system-prompt)}))))

(defn dispose-all!
  "Close every open env. Called on bot shutdown."
  []
  (doseq [[_ {:keys [env]}] @sessions]
    (try (rlm/dispose-env! env) (catch Exception _ nil)))
  (reset! sessions {}))
