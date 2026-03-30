(ns com.blockether.vis.web.executor
  "Async query executor — runs RLM queries on core.async thread pool.
   State lives in server.clj atoms. Only the executor mutates during query execution.
   Survives page reloads — queries run to completion regardless of client state."
  (:require [com.blockether.vis.agent :as agent]
            [com.blockether.vis.trace :as trace]
            [com.blockether.vis.web.server :as server]
            [com.blockether.svar.internal.rlm :as rlm]
            [clojure.core.async :as async]
            [clojure.string :as str])
  (:import [java.time Instant]))

;;; ── State ──────────────────────────────────────────────────────────────

(def ^:private exec-ch (atom nil))
(def ^:private running? (atom false))
(def ^:private in-flight (atom #{})) ;; set of session-ids currently executing
(def ^:private worker-chs (atom [])) ;; vector of worker go-loop channels for drain tracking
(def ^:private worker-count 4)

;;; ── Worker ─────────────────────────────────────────────────────────────

(defn- execute-query!
  "Run a single query. Updates server state atoms. Guards against deleted sessions."
  [{:keys [session-id query]}]
  (swap! in-flight conj session-id)
  (try
    (if-let [sess (server/get-session session-id)]
      (try
        (let [sys-prompt (str (:system-prompt (agent/agent {:name "web"}))
                             (agent/environment-info))
              hooks {:iteration
                     {:pre  (fn [{:keys [iteration]}]
                              (swap! server/live-status assoc-in [session-id :current]
                                     (str "Iteration " (inc iteration) "…")))
                      :post (fn [{:keys [iteration thinking executions final?] :as entry}]
                              (swap! server/live-status update-in [session-id :iterations]
                                     (fnil conj [])
                                     {:iteration iteration
                                      :thinking  thinking
                                      :final?    (boolean final?)
                                      :executions (mapv (fn [{:keys [code result error]}]
                                                          {:code   code
                                                           :result (str (trace/clean-result result))
                                                           :error  (some-> error str)})
                                                        executions)})
                              (swap! server/live-status assoc-in [session-id :current] nil))}
                     :code-exec
                     {:pre (fn [{:keys [code]}]
                             (when code
                               (let [t (str/trim code)
                                     nm (first (str/split
                                                (if (str/starts-with? t "(") (subs t 1) t)
                                                #"[\s\)\(\"']" 2))]
                                 (when nm
                                   (swap! server/live-status assoc-in [session-id :current]
                                          (str "Iteration … → " nm))))))}}
              result (rlm/query-env! (:env sess) query
                                     {:system-prompt sys-prompt :hooks hooks})]
          ;; Only write back if session still exists (not deleted mid-query)
          (when (server/get-session session-id)
            (swap! server/sessions update-in [session-id :messages] conj
                   {:role :assistant :text (:answer result) :result result
                    :ts (str (Instant/now))})
))
        (catch Exception e
          (println (str "[executor] Error in " session-id ": " (ex-message e)))
          (when (server/get-session session-id)
            (swap! server/sessions update-in [session-id :messages] conj
                   {:role :assistant :text (str "Error: " (ex-message e))
                    :result {:answer (str "Error: " (ex-message e))}
                    :ts (str (Instant/now))}))))
      ;; Session not found
      (println (str "[executor] Session " session-id " not found, skipping")))
    (finally
      ;; Always clean up — catches Error/Exception/anything
      (swap! server/live-status dissoc session-id)
      (swap! in-flight disj session-id))))

(defn- start-worker!
  "Start a single worker that takes from ch until closed.
   Returns the go-loop channel so callers can track worker lifecycle."
  [ch]
  (async/go-loop []
    (if-let [job (async/<! ch)]
      (do (async/<! (async/thread (execute-query! job)))
          (recur))
      :drained)))

;;; ── Public API ─────────────────────────────────────────────────────────

(defn in-flight?
  "True if session has a query currently executing."
  [session-id]
  (contains? @in-flight session-id))

(defn submit-query!
  "Submit a query for async execution. Returns immediately.
   Validates session exists and no query is in-flight."
  [session-id query]
  (when-let [_sess (server/get-session session-id)]
    (if (in-flight? session-id)
      (println (str "[executor] Session " session-id " already in-flight, skipping"))
      (do
        ;; Add user message immediately
        (swap! server/sessions update-in [session-id :messages] conj
               {:role :user :text query :ts (str (Instant/now))})
        ;; Generate session name from first message
        (when (= "New Chat" (:name (server/get-session session-id)))
          (async/go
            (let [title (async/<! (async/thread (server/generate-session-name query)))]
              (when (server/get-session session-id)
                (swap! server/sessions assoc-in [session-id :name] title)))))
        ;; Submit to executor
        (when-let [ch @exec-ch]
          (async/put! ch {:session-id session-id :query query}))))))

(defn- drain-workers!
  "Wait for all tracked worker go-loops to finish (with timeout)."
  [timeout-ms]
  (let [chs @worker-chs]
    (when (seq chs)
      (let [merged (async/merge chs)
            deadline (async/timeout timeout-ms)]
        (async/go-loop [remaining (count chs)]
          (when (pos? remaining)
            (let [[_ port] (async/alts! [merged deadline])]
              (when-not (= port deadline)
                (recur (dec remaining))))))))))

(defn start!
  "Start the executor. Creates fresh channel + workers. Atomic via compare-and-set!."
  []
  (when (compare-and-set! running? false true)
    ;; Drain any leftover workers from a previous cycle
    (when-let [old @exec-ch] (async/close! old))
    (drain-workers! 5000)
    (reset! in-flight #{})
    (reset! worker-chs [])
    (let [ch (async/chan 64)
          wks (vec (for [_ (range worker-count)] (start-worker! ch)))]
      (reset! exec-ch ch)
      (reset! worker-chs wks)
      (println (str "Executor started (" worker-count " workers)")))))

(defn stop!
  "Stop the executor. Closes channel, waits for workers to drain, clears state."
  []
  (when (compare-and-set! running? true false)
    (when-let [ch @exec-ch]
      (async/close! ch)
      (reset! exec-ch nil))
    (drain-workers! 10000)
    (reset! worker-chs [])
    (reset! in-flight #{})
    (println "Executor stopped")))
