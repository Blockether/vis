(ns com.blockether.vis.web.server
  "Session management, RLM lifecycle, and Jetty server.

   All web sessions live as named :conversations inside the shared
   SQLite DB (`config/db-path`), named `session:<uuid>`.
   No per-session directories — svar resolves/creates by name, and the
   connection pool means every session env shares one DataSource."
  (:require [com.blockether.vis.agent :as agent]
            [com.blockether.vis.config :as config]
            [com.blockether.vis.web.routes :as routes]
            [com.blockether.svar.internal.rlm :as rlm]
            [com.blockether.vis.rlm.db :as rlm-db]
            [ring.adapter.jetty :as jetty]
            [clojure.edn :as edn]
            [clojure.string :as str])
  (:import [java.time Instant]
           [java.util UUID]))

;;; ── State ──────────────────────────────────────────────────────────────

(defonce sessions (atom {}))
(defonce live-status (atom {}))

;; Long-lived db-info handle for server-level queries (list sessions, delete
;; session entities). Uses the same SQLite DataSource that every session env
;; shares. Opened lazily on first access.
(defonce ^:private server-db-info (atom nil))

(defn- db-info []
  (or @server-db-info
      (reset! server-db-info (rlm-db/create-rlm-conn config/db-path))))

(def ^:private session-name-prefix "session:")

(defn- session-name [id] (str session-name-prefix id))


;;; ── Router ──��─────────────────────────────��────────────────────────────

(defn get-router [] (config/get-router))

;;; ── Session name generation ────────────────────────────────────────────

(defn generate-session-name [first-message]
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

;;; ── Session CRUD ───────────────────────────────────────────────────────

(defn- maybe-ingest-git!
  "If the process working directory is inside a git repository, ingest recent
   commits into the env so the LLM automatically gets svar's git SCI tools
   (search-commits, commit-history, file-history, blame, commit-diff, …).
   Best-effort: a non-git cwd, a shallow clone, or any JGit error just logs
   and no-ops — the session still works without git tools."
  [env]
  (let [cwd (System/getProperty "user.dir")]
    (try
      (rlm/ingest-git! env {:repo-path cwd :n 500})
      (println (str "[server] ingested git history from " cwd))
      (catch Exception e
        (println (str "[server] git ingestion skipped (" cwd "): " (ex-message e)))
        nil))))

(defn- register-base-tools! [env]
  (reduce (fn [e {:keys [sym fn] :as tool-def}]
            (rlm/register-env-fn! e sym fn (dissoc tool-def :sym :fn)))
          env
          agent/base-tools))

(defn create-session! [name]
  (let [id     (str (UUID/randomUUID))
        _cfg   (config/resolve-config nil)  ;; throw early if no provider config
        router (config/get-router)
        ;; Open a new named :conversation in the shared DB. First-time name
        ;; ⇒ svar creates it; on restart the same name resolves to the
        ;; existing conversation and rehydrates history + var registry.
        env    (rlm/create-env router
                 {:db config/db-path
                  :conversation {:name (session-name id)}})
        env    (register-base-tools! env)
        _      (maybe-ingest-git! env)
        sess   {:id id :name (or name "New Chat") :env env :messages []
                :created-at (str (Instant/now))}]
    (swap! sessions assoc id sess)
    sess))

(defn get-session [id] (get @sessions id))

(defn- retract-conversation-tree!
  "Delete a conversation and all descendant entities (queries, iterations,
   iteration-vars). Uses rlm-db/delete-entity-tree! which walks the parent_id
   chain and deletes in one shot — attr tables cascade via ON DELETE CASCADE."
  [db-info conv-ref]
  (let [conv-id (second conv-ref)]
    (rlm-db/delete-entity-tree! db-info conv-id)))

(defn delete-session!
  "Delete a session — dispose env, remove from atoms, retract DB tree.
   Caller (routes) is responsible for checking in-flight status first."
  [id]
  (when-let [sess (get-session id)]
    (let [conv-ref (some-> sess :env :conversation-ref)]
      (try (rlm/dispose-env! (:env sess)) (catch Exception _ nil))
      (swap! sessions dissoc id)
      (swap! live-status dissoc id)
      (when conv-ref
        (try
          (retract-conversation-tree! (db-info) conv-ref)
          (catch Exception e
            (println (str "[server] delete-session! retract failed for "
                          id ": " (ex-message e)))))))))

(defn sessions-list []
  (->> (vals @sessions)
       (sort-by :created-at)
       (mapv #(select-keys % [:id :name]))))

;;; ── Load existing sessions ─────────────────────────────────────────────

(defn- safe-read-edn
  "Read an EDN string, returning the fallback on any failure. Used for the
   per-iteration code/results blobs which are persisted as pr-str."
  [s fallback]
  (if (and (string? s) (seq s))
    (try (edn/read-string s)
         (catch Exception _ fallback))
    fallback))

(defn- iteration-entity->exec
  "Project a svar :iteration entity into the presenter's execution-list shape.
   Each iteration persists :iteration/code (vec of source strings) and
   :iteration/results (vec of pr-str'd results) as parallel arrays."
  [iter-entity]
  (let [codes   (safe-read-edn (:iteration/code iter-entity) [])
        results (safe-read-edn (:iteration/results iter-entity) [])]
    (mapv (fn [code result]
            {:code   code
             :result (safe-read-edn result result)})
          codes
          (concat results (repeat nil)))))

(defn- iteration-entity->trace-entry
  "Project a svar :iteration entity into the presenter's trace-entry shape.
   `idx` is the positional index — svar orders iterations by :entity/created-at
   via db-list-query-iterations, so the caller's enumeration order is correct."
  [idx iter-entity]
  (cond-> {:iteration  idx
           :thinking   (:iteration/thinking iter-entity)
           :executions (iteration-entity->exec iter-entity)}
    (some? (:iteration/answer iter-entity)) (assoc :final? true)))

(defn- query-entity->message-pair
  "Turn one svar :query entity into a [user-msg assistant-msg] pair for the
   presenter. Walks iteration entities for the trace; pulls the final answer
   off the terminal iteration (or :query/answer as a fallback)."
  [db-info query-entity]
  (let [query-ref   [:entity/id (:entity/id query-entity)]
        iterations  (rlm-db/db-list-query-iterations db-info query-ref)
        trace       (vec (map-indexed iteration-entity->trace-entry iterations))
        final-iter  (last (filter :iteration/answer iterations))
        answer      (or (when final-iter
                          (safe-read-edn (:iteration/answer final-iter)
                            (:iteration/answer final-iter)))
                        (safe-read-edn (:query/answer query-entity)
                          (:query/answer query-entity)))
        result-map  (cond-> {:trace      trace
                             :iterations (count iterations)
                             :duration-ms (:query/duration-ms query-entity)}
                      answer (assoc :answer answer))]
    [{:role :user :text (or (:query/text query-entity) "")}
     {:role :assistant
      :text (when (string? answer) answer)
      :result result-map}]))

(defn- load-messages-from-db
  "Reconstruct the presenter-format message list for a session env by walking
   the svar DB: conversation → queries → iterations. Returns [] on any failure
   so a corrupted session doesn't take the whole server down."
  [env]
  (try
    (let [db-info (:db-info env)
          conv-ref (:conversation-ref env)
          queries  (rlm-db/db-list-conversation-queries db-info conv-ref)]
      (into [] (mapcat #(query-entity->message-pair db-info %)) queries))
    (catch Exception e
      (println (str "[server] load-messages-from-db failed: " (ex-message e)))
      [])))

(defn- session-name-from-messages [messages]
  (or (some #(when (= :user (:role %))
               (let [t (:text %)]
                 (if (> (count t) 65) (str (subs t 0 62) "…") t)))
            messages)
      "New Chat"))

(defn- list-session-conversations
  "Enumerate every :conversation in the shared DB whose :conversation/name starts
   with `session:`. Returns `[{:id uuid-str :created-at inst}, …]` ordered by
   creation. We derive the session-id from the suffix of :conversation/name so
   callers can stitch back to URL-level ids without touching :entity/id."
  [db-info]
  (->> (rlm-db/db-list-conversations-by-prefix db-info session-name-prefix)
       (mapv (fn [{:keys [name created-at]}]
               {:id         (subs name (count session-name-prefix))
                :created-at created-at}))))

(defn load-sessions! []
  (let [_cfg   (config/resolve-config nil)
        router (config/get-router)
        convs  (list-session-conversations (db-info))]
    (doseq [{:keys [id created-at]} convs]
      (try
        ;; Named resolver: resolves to the already-existing conversation and
        ;; rehydrates svar's SCI var state. All envs share the SQLite
        ;; DataSource, so opening many here is cheap.
        (let [env  (rlm/create-env router
                     {:db config/db-path
                      :conversation {:name (session-name id)}})
              env  (register-base-tools! env)
              _    (maybe-ingest-git! env)
              msgs (load-messages-from-db env)
              name (session-name-from-messages msgs)]
          (swap! sessions assoc id {:id id :name name :env env :messages msgs
                                    :created-at (str created-at)}))
        (catch Exception e
          (println (str "[server] Failed to load session " id ": " (ex-message e))))))))

;;; ── Jetty ────────────────────────────────────────��─────────────────────

(defonce server (atom nil))

(defn start! [& [{:keys [port] :or {port 3000}}]]
  (when @server (.stop @server))
  (load-sessions!)
  (let [exec-start! (requiring-resolve 'com.blockether.vis.web.executor/start!)]
    (exec-start!))
  (println (str "Starting vis web on http://0.0.0.0:" port))
  (println (str "Local network: http://192.168.0.143:" port))
  (println (str "Sessions: " (count @sessions)))
  (reset! server (jetty/run-jetty #'routes/handler
                                   {:port port :join? false :host "0.0.0.0"})))

(defn stop! []
  (let [exec-stop! (requiring-resolve 'com.blockether.vis.web.executor/stop!)]
    (exec-stop!))
  (when @server (.stop @server) (reset! server nil)))

(defn -main [& args]
  (let [port (if (seq args) (parse-long (first args)) 3000)]
    (.addShutdownHook (Runtime/getRuntime)
                      (Thread. ^Runnable (fn []
                                           (println "Shutting down vis web…")
                                           (stop!))))
    (start! {:port port})
    @(promise)))
