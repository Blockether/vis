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

(defn create-session! [name]
  (let [id    (str (UUID/randomUUID))
        cfg   (config/resolve-config nil)
        path  (session-path id)
        env   (rlm/create-env {:config cfg :path path})
        env   (reduce (fn [e {:keys [sym fn] :as tool-def}]
                        (rlm/register-env-fn! e sym fn (dissoc tool-def :sym :fn)))
                      env
                      agent/base-tools)
        sess  {:id id :name (or name "New Chat") :env env :messages [] :created-at (str (Instant/now))}]
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

(defn- load-messages-from-db [env]
  (try
    (let [db-info @(:db-info-atom env)
          raw     (rlm-db/get-recent-messages db-info 200)]
      (->> raw
           reverse
           (keep (fn [m]
                   (case (:role m)
                     :user {:role :user :text (:content m)}
                     :assistant
                     (let [result-edn (:result-edn m)
                           result (when result-edn
                                    (try (edn/read-string result-edn) (catch Exception _ nil)))]
                       (when (or result (not (clojure.string/blank? (:content m))))
                         {:role :assistant
                          :text (:content m)
                          :result (or result {:answer (:content m) :trace []})
                          :has-result? (boolean result)}))
                     nil)))
           ;; Deduplicate: keep only one assistant message per consecutive group
           ;; (svar stores one per iteration, we only want the last one = FINAL)
           (reduce (fn [acc msg]
                     (if (and (= :assistant (:role msg))
                              (seq acc)
                              (= :assistant (:role (peek acc)))
                              (not (:has-result? (peek acc))))
                       ;; Replace previous non-final assistant with this one
                       (conj (pop acc) msg)
                       (conj acc msg)))
                   [])
           (mapv #(dissoc % :has-result?))))
    (catch Exception _ [])))

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
              (let [cfg  (config/resolve-config nil)
                    env  (rlm/create-env {:config cfg :path (str f)})
                    env  (reduce (fn [e {:keys [sym fn] :as tool-def}]
                                   (rlm/register-env-fn! e sym fn (dissoc tool-def :sym :fn)))
                                 env
                                 agent/base-tools)
                    msgs (load-messages-from-db env)
                    name (session-name-from-messages msgs)]
                (swap! sessions assoc id {:id id :name name :env env :messages msgs :created-at ""}))
              (catch Exception _ nil))))))))

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
