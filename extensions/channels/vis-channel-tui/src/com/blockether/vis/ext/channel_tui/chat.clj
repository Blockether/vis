(ns com.blockether.vis.ext.channel-tui.chat
  "TUI-side projections over the shared conversations API.

   On startup the TUI creates a fresh `:tui` conversation by default.
   Pass `--conversation-id ID` or `--resume` to pick up an existing one.
   Conversation data is persisted in `~/.vis/vis.mdb` so you can come
   back to it."
  (:require [clojure.string :as str]
            [com.blockether.vis.core :as sdk]
            [com.blockether.vis.core :as lp]
            [taoensso.telemere :as t])
  (:import [java.io PrintWriter StringWriter]))

(defn- exception->log-data
  "Build a log-friendly map from an exception that preserves EVERYTHING
   diagnostic: class, message, ex-data, full stack trace, and the
   cause chain. The whole point is that when an error reaches the
   chat boundary, the next person reading `~/.vis/vis.log` should not
   have to guess where it came from — the stack is right there.

   `.printStackTrace` includes nested causes (\"Caused by: …\" blocks)
   by default, so a single string captures the entire chain without
   manual recursion."
  [^Throwable e]
  (let [sw (StringWriter.)]
    (.printStackTrace e (PrintWriter. sw))
    {:class   (.getName (class e))
     :message (or (ex-message e) (.toString e))
     :ex-data (ex-data e)
     :stack   (.toString sw)}))

(defn user-message
  "Create a structured user message with timestamp."
  ([text] (user-message text (java.util.Date.)))
  ([text timestamp]
   {:role :user :text text :timestamp timestamp}))

(defn assistant-message
  "Create a structured assistant (vis) message with timestamp."
  ([text] (assistant-message text (java.util.Date.)))
  ([text timestamp]
   {:role :assistant :text (if (string? text) text (pr-str text)) :timestamp timestamp}))

(defn- rebuild-history
  "Reconstruct message history from DB for a conversation.
   Returns a vec of {:role :user|:assistant :text str :timestamp #inst ...}.
   Assistant messages include the code execution trace from all iterations."
  [conversation-id]
  (try
    (let [d       (lp/db-info)
          queries (sdk/db-list-conversation-queries d conversation-id)]
      (into []
        (mapcat (fn [q]
                  (let [user-message (user-message (or (:text q) "") (or (:created-at q) (java.util.Date.)))
                        answer    (or (:answer q) "")
                        model     (:model q)
                        tokens    (cond-> {}
                                    (:input-tokens q)  (assoc :input (:input-tokens q))
                                    (:output-tokens q) (assoc :output (:output-tokens q)))
                        iterations (:iterations q)
                        duration-ms (:duration-ms q)
                        cost      (when (:cost q) {:total-cost (:cost q)})
                        ;; Rebuild trace from iterations + expressions
                        query-iterations (sdk/db-list-query-iterations d (:id q))
                        trace (into []
                                (map (fn [it]
                                       (let [exprs (sdk/db-list-iteration-expressions d (:id it))
                                             result-strs (mapv (fn [{:keys [result error]}]
                                                                 (if error
                                                                   (sdk/format-error error)
                                                                   (pr-str result)))
                                                           exprs)
                                             stdout-strs (mapv #(or (:stdout %) "") exprs)
                                             durations (mapv #(or (:duration-ms %) 0) exprs)]
                                         {:thinking  (:thinking it)
                                          :code      (mapv :code exprs)
                                          :results   result-strs
                                          :stdouts   stdout-strs
                                          :durations durations
                                          :successes (mapv #(nil? (:error %)) exprs)})))
                                query-iterations)
                        ;; `:prior-outcome :cancelled` is how the
                        ;; persistance layer marks an aborted turn (the
                        ;; sweep + cancel paths both write that value).
                        ;; Surface it as `:status :cancelled` on the
                        ;; assistant message so the bubble renderer
                        ;; paints the gray cancelled-bg zone and the
                        ;; trace + status footer the same way live
                        ;; cancellations render.
                        cancelled? (= :cancelled (:prior-outcome q))
                        assistant-message (cond-> (assistant-message (or answer "") (or (:created-at q) (java.util.Date.)))
                                            true       (assoc :query-id (:id q))
                                            (seq trace) (assoc :trace trace :raw-answer (or answer ""))
                                            model  (assoc :model model)
                                            iterations  (assoc :iterations iterations)
                                            duration-ms (assoc :duration-ms duration-ms)
                                            cost   (assoc :cost cost)
                                            (seq tokens) (assoc :tokens tokens)
                                            cancelled? (assoc :status :cancelled))]
                    [user-message assistant-message])))
        queries))
    (catch Exception e
      (t/log! {:level :warn :id ::rebuild-history-failed
               :data  (exception->log-data e)
               :msg   (str "Failed to rebuild history: " (ex-message e))})
      [])))

(defn make-conversation
  "Create a fresh `:tui` conversation. Returns `{:id conversation-id :history []}`."
  [_provider-config]
  (let [{:keys [id]} (lp/create! :tui)]
    {:id id :history []}))

(defn- resolve-resume-id
  "Resolve a resume id. Accepts full UUID or an unambiguous prefix
   among :tui conversations. Returns full UUID string or nil."
  [conversation-id]
  (let [cid (some-> conversation-id str str/trim)]
    (when (seq cid)
      (or (some-> (lp/by-id cid) :id str)
        (let [matches (->> (lp/by-channel :tui)
                        (map :id)
                        (filter #(str/starts-with? (str %) cid))
                        vec)]
          (when (= 1 (count matches))
            (str (first matches))))))))

(defn resume-conversation
  "Resume an existing conversation by id.
   Accepts full UUID or unambiguous short UUID prefix.
   Returns `{:id conversation-id :history [...]}` with persisted messages."
  [conversation-id]
  (when-let [resolved-id (resolve-resume-id conversation-id)]
    (when-let [conversation (lp/by-id resolved-id)]
      {:id (str (:id conversation)) :history (rebuild-history (str (:id conversation)))})))

(defn query!
  "Send a user query through the shared conversations cache. Blocking.
   Returns `{:answer str}` or `{:error str}`.

   `opts` may contain:
     :on-chunk    — fn receiving `{:iteration :thinking :code :final :done?}`
                    on every streaming chunk from the RLM. The TUI uses this
                    to project a live per-iteration progress timeline into
                    the assistant placeholder bubble.
     :cancel-atom — (atom bool) honored by the iteration loop; flipping
                    it to true causes the current query to terminate at
                    the next safe point and return `{:status :cancelled}`."
  ([conversation text] (query! conversation text {}))
  ([{:keys [id]} text {:keys [on-chunk cancel-atom]}]
   (try
     (let [send-opts (cond-> {}
                       on-chunk    (assoc :hooks {:on-chunk on-chunk})
                       cancel-atom (assoc :cancel-atom cancel-atom))
           result (lp/send! id text send-opts)
           cancelled? (= :cancelled (:status result))
           ;; Plain text — the bubble renderer dims it via the
           ;; `:status :cancelled` field we propagate below, NOT via
           ;; markdown italic. Underscores would just render as
           ;; literal underscores once markdown processing is
           ;; skipped for cancelled messages.
           answer (or (:answer result)
                    (when cancelled? "Cancelled by user.")
                    "[empty response]")
           model  (or (get-in result [:cost :model]) (get result :model))
           tokens (:tokens result)
           cost   (:cost result)
           confidence (:confidence result)]
       (cond-> {:answer      (if (string? answer) answer (pr-str answer))
                :iterations  (or (:iterations result) 1)
                :duration-ms (:duration-ms result)
                :query-id    (:query-id result)}
         model      (assoc :model model)
         tokens     (assoc :tokens tokens)
         cost       (assoc :cost cost)
         confidence (assoc :confidence confidence)
         cancelled? (assoc :status :cancelled)))
     ;; future-cancel from the TUI translates to thread interruption.
     ;; The shared channels.cancellation predicate folds in
     ;; InterruptedException, CancellationException, and any runtime
     ;; wrapper that hides one in its cause chain — surface those as
     ;; a clean cancelled answer, not a generic error.
     (catch Exception e
       (if (sdk/cancellation? e)
         (do (.interrupt (Thread/currentThread))
           {:answer "Cancelled by user." :iterations 0 :status :cancelled})
         (do
           ;; Log EVERYTHING. Stripping a stack trace at the channel
           ;; boundary is how a `[SQLITE_CANTOPEN]` ends up untriagable
           ;; — the user sees a one-liner and the log has the same
           ;; one-liner. With the full trace, the next failure pinpoints
           ;; the exact JDBC call that opened the bad handle.
           (t/log! {:level :error :id ::query-failed
                    :data  (exception->log-data e)
                    :msg   (str "Query failed: " (ex-message e))})
           {:error (sdk/db-error->user-message e)}))))))

(defn dispose!
  "Release the TUI's env handle. Conversation data stays in
   `~/.vis/vis.mdb` so other consumers of the `:tui` channel
   (e.g. `vis conversations tui`, future inspectors) still see it."
  [{:keys [id]}]
  (when id (lp/close! id)))
