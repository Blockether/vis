(ns com.blockether.vis.web.server
  "Session management, RLM lifecycle, and Jetty server."
  (:require [com.blockether.vis.agent :as agent]
            [com.blockether.vis.config :as config]
            [com.blockether.vis.web.routes :as routes]
            [com.blockether.svar.internal.rlm :as rlm]
            [com.blockether.svar.internal.rlm.db :as rlm-db]
            [ring.adapter.jetty :as jetty]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.time Instant]
           [java.util UUID]))

;;; ── State ───────────────────────────────────���──────────────────────────

(def sessions-dir (str (System/getProperty "user.home") "/.vis/sessions"))

(defonce sessions (atom {}))
(defonce live-status (atom {}))


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

;;; ── Session CRUD ───────���───────────────────────────────────────────────

(defn- session-path [id] (str sessions-dir "/" id))

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

(defn create-session! [name]
  (let [id     (str (UUID/randomUUID))
        _cfg   (config/resolve-config nil)  ;; throw early if no provider config
        router (config/get-router)
        path   (session-path id)
        env    (rlm/create-env router {:db path})
        env    (reduce (fn [e {:keys [sym fn] :as tool-def}]
                         (rlm/register-env-fn! e sym fn (dissoc tool-def :sym :fn)))
                       env
                       agent/base-tools)
        _      (maybe-ingest-git! env)
        sess   {:id id :name (or name "New Chat") :env env :messages [] :created-at (str (Instant/now))}]
    (swap! sessions assoc id sess)
    sess))

(defn get-session [id] (get @sessions id))

(defn delete-session! [id]
  (when-let [sess (get-session id)]
    (if ((requiring-resolve 'com.blockether.vis.web.executor/in-flight?) id)
      (println (str "[server] Session " id " has in-flight query, skipping delete"))
      (do
        (try (rlm/dispose-env! (:env sess)) (catch Exception _ nil))
        (swap! sessions dissoc id)
        (swap! live-status dissoc id)
        (let [dir (io/file (session-path id))]
          (when (.exists dir)
            (doseq [f (reverse (file-seq dir))]
              (.delete f))))))))

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
    (let [db-info @(:db-info-atom env)
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

(defn load-sessions! []
  (let [dir (io/file sessions-dir)]
    (when (.exists dir)
      (doseq [f (.listFiles dir)]
        (when (.isDirectory f)
          (let [id (.getName f)]
            (try
              (let [_cfg   (config/resolve-config nil)
                    router (config/get-router)
                    ;; :conversation :latest resumes the prior conversation on the
                    ;; persistent DB — svar rehydrates its own SCI var state.
                    env    (rlm/create-env router {:db (str f) :conversation :latest})
                    env    (reduce (fn [e {:keys [sym fn] :as tool-def}]
                                     (rlm/register-env-fn! e sym fn (dissoc tool-def :sym :fn)))
                                   env
                                   agent/base-tools)
                    ;; Re-bind git SCI tools onto the freshly created env. Commits are
                    ;; already persisted in Datalevin from the original ingest; this
                    ;; just reopens the JGit repo and wires the sandbox bindings.
                    _      (maybe-ingest-git! env)
                    msgs   (load-messages-from-db env)
                    name   (session-name-from-messages msgs)]
                (swap! sessions assoc id {:id id :name name :env env :messages msgs :created-at ""}))
              (catch Exception e
                (println (str "[server] Failed to load session " id ": " (ex-message e)))))))))))

;;; ── Jetty ────────────────────────────────────────��─────────────────────

(defonce server (atom nil))

(defn start! [& [{:keys [port] :or {port 3000}}]]
  (when @server (.stop @server))
  (load-sessions!)
  ;; Start the executor
  ((requiring-resolve 'com.blockether.vis.web.executor/start!))
  (println (str "Starting vis web on http://0.0.0.0:" port))
  (println (str "Local network: http://192.168.0.143:" port))
  (println (str "Sessions: " (count @sessions)))
  (reset! server (jetty/run-jetty #'routes/handler
                                   {:port port :join? false :host "0.0.0.0"})))

(defn stop! []
  ((requiring-resolve 'com.blockether.vis.web.executor/stop!))
  (when @server (.stop @server) (reset! server nil)))

(defn -main [& args]
  (let [port (if (seq args) (parse-long (first args)) 3000)]
    (.addShutdownHook (Runtime/getRuntime)
                      (Thread. ^Runnable (fn []
                                           (println "Shutting down vis web…")
                                           (stop!))))
    (start! {:port port})
    @(promise)))
