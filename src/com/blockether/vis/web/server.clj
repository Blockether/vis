(ns com.blockether.vis.web.server
  "Web server: Jetty + session metadata projections over the conversations API.

   Conversations are owned by `com.blockether.vis.conversations` (one
   cache per process, channel-scoped). This namespace is a thin projection
   layer for the web UI — sidebar list, per-conversation message list,
   streaming status for polling, and LLM-driven title generation after the
   first user message.

   Web sessions live in the `:vis` channel, so anything the TUI creates
   shows up here too."
  (:require [com.blockether.vis.config :as config]
            [com.blockether.vis.conversations :as conv]
            [com.blockether.vis.web.routes :as routes]
            [com.blockether.vis.rlm :as rlm]
            [com.blockether.vis.rlm.db :as rlm-db]
            [ring.adapter.jetty :as jetty]
            [clojure.edn :as edn]
            [clojure.string :as str]))

;;; ── In-process state ──────────────────────────────────────────────────

;; conv-id → [{:role :text :ts :result?} …] — lazy cache for the UI.
(defonce messages-cache (atom {}))

;; conv-id → {:current "…" :iterations [entry …]} — executor streams here.
(defonce live-status (atom {}))

;;; ── Title generation ─────────────────────────────────────────────────

(defn generate-session-title
  "Ask the router for a short title (≤ 5 words) for a first user message.
   Falls back to a truncated form of the message itself on any LLM failure."
  [first-message]
  (try
    (let [result (config/ask!
                  {:messages [{:role "system" :content "Generate a short title (max 5 words) for this chat. Reply with ONLY the title."}
                              {:role "user" :content first-message}]
                   :spec {:title {:type :string :description "Short chat title, max 5 words, no quotes or markup"}}
                   :prefer :speed :capabilities #{:chat}})
          title  (str/trim (or (:title (:result result)) ""))
          raw    (if (str/blank? title) first-message title)]
      (if (> (count raw) 65)
        (str (subs raw 0 62) "…")
        raw))
    (catch Exception _
      (let [fm first-message]
        (if (> (count fm) 65) (str (subs fm 0 62) "…") fm)))))

;;; ── Git ingestion on first open ──────────────────────────────────────

(defn- maybe-ingest-git!
  "Ingest recent commits into a freshly-opened env so git SCI tools are
   available during the conversation. Best-effort — git ingestion is
   deduped by repo name, so repeated calls for the same cwd are cheap."
  [env]
  (let [cwd (System/getProperty "user.dir")]
    (try
      (rlm/ingest-git! env {:repo-path cwd :n 500})
      (println (str "[server] ingested git history from " cwd))
      (catch Exception e
        (println (str "[server] git ingestion skipped (" cwd "): " (ex-message e)))
        nil))))

;;; ── Message projection from DB ───────────────────────────────────────

(defn- safe-read-edn [s fallback]
  (if (and (string? s) (seq s))
    (try (edn/read-string s) (catch Exception _ fallback))
    fallback))

(defn- iteration-entity->exec [iter-entity]
  (let [codes   (safe-read-edn (:iteration/code iter-entity) [])
        results (safe-read-edn (:iteration/results iter-entity) [])]
    (mapv (fn [code result]
            {:code   code
             :result (safe-read-edn result result)})
          codes
          (concat results (repeat nil)))))

(defn- iteration-entity->trace-entry [idx iter-entity]
  (cond-> {:iteration  idx
           :thinking   (:iteration/thinking iter-entity)
           :executions (iteration-entity->exec iter-entity)}
    (some? (:iteration/answer iter-entity)) (assoc :final? true)))

(defn- query-entity->message-pair [db-info query-entity]
  (let [query-ref   [:id (:id query-entity)]
        iterations  (rlm-db/db-list-query-iterations db-info query-ref)
        trace       (vec (map-indexed iteration-entity->trace-entry iterations))
        final-iter  (last (filter :iteration/answer iterations))
        answer      (or (when final-iter
                          (safe-read-edn (:iteration/answer final-iter)
                            (:iteration/answer final-iter)))
                        (safe-read-edn (:query/answer query-entity)
                          (:query/answer query-entity)))
        result-map  (cond-> {:trace       trace
                             :iterations  (count iterations)
                             :duration-ms (:query/duration-ms query-entity)}
                      answer (assoc :answer answer))]
    [{:role :user :text (or (:query/text query-entity) "")}
     {:role :assistant
      :text (when (string? answer) answer)
      :result result-map}]))

(defn- load-messages-from-db [env]
  (try
    (let [db-info  (:db-info env)
          conv-ref (:conversation-ref env)
          queries  (rlm-db/db-list-conversation-queries db-info conv-ref)]
      (into [] (mapcat #(query-entity->message-pair db-info %)) queries))
    (catch Exception e
      (println (str "[server] load-messages-from-db failed: " (ex-message e)))
      [])))

(defn messages-for
  "Return the UI message list for `conv-id`, seeding the cache from the DB
   on first access (and triggering lazy git ingestion when we open the
   env)."
  [conv-id]
  (or (get @messages-cache conv-id)
      (let [env  (conv/env-for conv-id)
            _    (maybe-ingest-git! env)
            msgs (load-messages-from-db env)]
        (swap! messages-cache
               (fn [m] (if (contains? m conv-id) m (assoc m conv-id msgs))))
        (get @messages-cache conv-id))))

(defn append-message!
  "Append a single message to the cache for `conv-id`. Executor calls this
   for user-submitted and assistant-final messages."
  [conv-id msg]
  (swap! messages-cache update conv-id (fnil conj []) msg))

;;; ── Session CRUD (projected over conv API) ───────────────────────────

(defn create-session!
  "Create a new `:vis` conversation for the web UI. Returns
   `{:id :name :messages :created-at}`."
  ([] (create-session! "New Chat"))
  ([title]
   (let [{:keys [id title created-at]} (conv/create! :vis {:title title})]
     (swap! messages-cache assoc id [])
     {:id id :name title :messages [] :created-at created-at})))

(defn get-session
  "Return `{:id :name :messages :created-at}` for a conv-id, or nil."
  [id]
  (when-let [c (conv/by-id id)]
    {:id         (:id c)
     :name       (or (:title c) "New Chat")
     :messages   (messages-for id)
     :created-at (:created-at c)}))

(defn delete-session!
  "Close env, drop DB data, drop cache. Caller (routes) checks in-flight."
  [id]
  (conv/delete! id)
  (swap! messages-cache dissoc id)
  (swap! live-status dissoc id))

(defn sessions-list
  "Sidebar projection: every `:vis` conversation as `{:id :name}`, most
   recent first."
  []
  (->> (conv/by-channel :vis)
       (mapv (fn [c] {:id (:id c) :name (or (:title c) "New Chat")}))))

(defn set-session-title! [id title]
  (conv/set-title! id title))

;;; ── Jetty ──────────────────────────────────────────────────────────

(defonce server (atom nil))

(defn start! [& [{:keys [port] :or {port 3000}}]]
  (when @server (.stop @server))
  (let [exec-start! (requiring-resolve 'com.blockether.vis.web.executor/start!)]
    (exec-start!))
  (println (str "Starting vis web on http://0.0.0.0:" port))
  (println (str "Sessions: " (count (sessions-list))))
  (reset! server (jetty/run-jetty #'routes/handler
                                   {:port port :join? false :host "0.0.0.0"})))

(defn stop! []
  (let [exec-stop! (requiring-resolve 'com.blockether.vis.web.executor/stop!)]
    (exec-stop!))
  (when @server (.stop @server) (reset! server nil))
  (conv/close-all!))

(defn -main [& args]
  (let [port (if (seq args) (parse-long (first args)) 3000)]
    (.addShutdownHook (Runtime/getRuntime)
                      (Thread. ^Runnable (fn []
                                           (println "Shutting down vis web…")
                                           (stop!))))
    (start! {:port port})
    @(promise)))
