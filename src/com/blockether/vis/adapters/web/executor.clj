(ns com.blockether.vis.adapters.web.executor
  "Async query executor — runs RLM queries on core.async thread pool.
   State lives in web conversations atoms. Only the executor mutates during query execution.
   Survives page reloads — queries run to completion regardless of client state."
  (:require [com.blockether.svar.internal.llm :as llm]
            [com.blockether.vis.loop.conversations.core :as conversations]
            [com.blockether.vis.loop.conversations.shared :as conv-shared]
            [com.blockether.vis.adapters.web.conversations :as web-conversations]
            [clojure.core.async :as async]
            [clojure.string :as str])
  (:import [java.time Instant]))

;;; ── State ──────────────────────────────────────────────────────────────

(def ^:private exec-ch (atom nil))
(def ^:private running? (atom false))
(def ^:private in-flight (atom #{})) ;; set of conversation-ids currently executing
(def ^:private worker-chs (atom [])) ;; vector of worker go-loop channels for drain tracking
(def ^:private worker-count 4)

(defn- web-system-prompt
  []
  ;; Persona only. The `<environment>` block (CWD / platform / relative-paths
  ;; hint) is appended automatically by `build-system-prompt` for every
  ;; adapter — don't concat it here.
  "You are vis web assistant. Keep responses clear and concise.
For simple greetings or direct prose answers, just set :answer and leave :code
empty — no def wrapping, no ceremony. For real work, batch: emit many
independent (def …) / tool calls in a single iteration's :code vector. Each
iteration is a full round-trip, so one iteration with ten blocks beats ten
with one. When reading files, fetch generously (whole file or large ranges)
rather than re-reading small slices.")

;;; ── Worker ─────────────────────────────────────────────────────────────

(defn- on-chunk-handler
  "Build the :on-chunk callback svar now uses (post-:hooks refactor). svar calls it
    during streaming with {:iteration :thinking :code :final :done?} and once with
    :done? true when an iteration finalizes. We project each call into the web
    live-status atom so the /check polling endpoint can stream UI updates.

    Merge semantics are centralized in `conv-shared/merge-stream-iteration` so
    every adapter handles sparse terminal chunks the same way. Status labels
    fall back to iteration + first tool name."
  [conversation-id]
  (let [project! (conv-shared/make-on-chunk-projector
                   {:on-update
                    (fn [timeline {:keys [iteration done?] :as chunk}]
                      (let [code-vec (conv-shared/chunk->code-vec chunk)
                            first-code (first (filter identity code-vec))
                            label (cond
                                    done?             "Finalizing…"
                                    (seq first-code)  (str "Iteration " (inc iteration) " → "
                                                        (or (conv-shared/first-symbol first-code) "…"))
                                    :else             (str "Iteration " (inc iteration) "…"))
                            iters' (mapv (fn [{:keys [iteration final? thinking code]}]
                                           {:iteration iteration
                                            :thinking  thinking
                                            :final? final?
                                            :executions (mapv (fn [c] {:code c}) code)})
                                     timeline)]
                        (swap! web-conversations/live-status
                          (fn [ls]
                            (let [conv-state (or (get ls conversation-id) {})]
                              (assoc ls conversation-id
                                (assoc conv-state
                                  :iterations iters'
                                  :current (when-not done? label))))))))})]
    (fn [chunk]
      (project! chunk))))

(defn- msgs->llm
  "Convert cached web messages into the llm message vector the RLM expects.
   Drops empty/nil text so prior assistant errors don't poison the context."
  [msgs]
  (into []
    (keep (fn [{:keys [role text]}]
            (when (and (string? text) (not (str/blank? text)))
              (case role
                :user (llm/user text)
                :assistant (llm/assistant text)
                nil))))
    msgs))

(defn- execute-query!
  "Run a single query against the conversation id `conversation-id`. The user
   message has already been appended by `submit-query!`; we append the
   assistant reply (or error) on completion. Guards against deleted
   conversations: skip writes when the conversation no longer exists."
  [{:keys [conversation-id query]}]
  (swap! in-flight conj conversation-id)
  (try
    (if-let [conv (web-conversations/get-conversation conversation-id)]
      (try
        (let [history (msgs->llm (:messages conv))
              msgs    (if (seq history) history [(llm/user query)])
              result  (conversations/send! conversation-id msgs
                         {:system-prompt (web-system-prompt)
                          :hooks {:on-chunk (on-chunk-handler conversation-id)}})]
          (when (web-conversations/get-conversation conversation-id)
            (web-conversations/append-message! conversation-id
              {:role :assistant :text (:answer result)
               :result result :ts (str (Instant/now))})))
        (catch Exception e
          (let [user-msg (conv-shared/error->user-message e)]
            (println (str "[executor] Error in " conversation-id ": " (ex-message e)))
            (when (web-conversations/get-conversation conversation-id)
              (web-conversations/append-message! conversation-id
                {:role :assistant
                 :text user-msg
                 :result {:answer user-msg}
                 :ts (str (Instant/now))})))))
      (println (str "[executor] Conversation " conversation-id " not found, skipping")))
    (finally
      (swap! web-conversations/live-status dissoc conversation-id)
      (swap! in-flight disj conversation-id))))

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
  "True if conversation has a query currently executing."
  [conversation-id]
  (contains? @in-flight conversation-id))

(defn submit-query!
  "Submit a query for async execution. Returns immediately. Appends the user
   message to the cache, kicks off LLM-driven title generation on the first
   turn, then enqueues the job for the worker pool."
  [conversation-id query]
  (when-let [conv (web-conversations/get-conversation conversation-id)]
    (if (in-flight? conversation-id)
      (println (str "[executor] Conversation " conversation-id " already in-flight, skipping"))
      (do
        (web-conversations/append-message! conversation-id
          {:role :user :text query :ts (str (Instant/now))})
        (when (= "New Chat" (:name conv))
          (async/go
            (let [title (async/<! (async/thread (web-conversations/generate-conversation-title query)))]
              (when (web-conversations/get-conversation conversation-id)
                (web-conversations/set-conversation-title! conversation-id title)))))
        (when-let [ch @exec-ch]
          (async/put! ch {:conversation-id conversation-id
                          :query query}))))))

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
