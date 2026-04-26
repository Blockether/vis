(ns com.blockether.vis.channels.tui.chat
  "TUI-side projections over the shared conversations API.

   On startup the TUI creates a fresh `:vis` conversation by default.
   Pass `--conversation-id ID` or `--resume` to pick up an existing one.
   Conversation data is persisted in `~/.vis/vis.mdb` so you can come
   back to it."
  (:require [clojure.string :as str]
            [com.blockether.vis.channels.cancellation :as cancellation]
            [com.blockether.vis.channels.tui.render :as render]
            [com.blockether.vis.loop.runtime.conversation.core :as conversations]
            [com.blockether.vis.persistance.core :as db]
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

(defn- rebuild-history
  "Reconstruct message history from DB for a conversation.
   Returns a vec of {:role :user|:assistant :text str :timestamp #inst ...}.
   Assistant messages include the code execution trace from all iterations."
  [conv-id]
  (try
    (let [d       (conversations/db-info)
          queries (db/db-list-conversation-queries d conv-id)]
      (into []
        (mapcat (fn [q]
                  (let [user-msg  (user-msg (or (:text q) "") (or (:created-at q) (java.util.Date.)))
                        answer    (or (:answer q) "")
                        model     (:model q)
                        tokens    (cond-> {}
                                   (:input-tokens q)  (assoc :input (:input-tokens q))
                                   (:output-tokens q) (assoc :output (:output-tokens q)))
                        iters     (:iterations q)
                        dur-ms    (:duration-ms q)
                        cost      (when (:cost q) {:total-cost (:cost q)})
                        ;; Rebuild trace from iterations + expressions
                        query-iters (db/db-list-query-iterations d (:id q))
                        trace (into []
                                (map (fn [it]
                                       (let [exprs (db/db-list-iteration-expressions d (:id it))
                                             result-strs (mapv (fn [{:keys [result error]}]
                                                                 (if error
                                                                   (str "ERROR: " error)
                                                                   (pr-str result)))
                                                           exprs)
                                             stdout-strs (mapv #(or (:stdout %) "") exprs)
                                             dur-strs    (mapv #(or (:duration-ms %) 0) exprs)]
                                         {:thinking  (:thinking it)
                                          :code      (mapv :code exprs)
                                          :results   result-strs
                                          :stdouts   stdout-strs
                                          :durations dur-strs
                                          :successes (mapv #(nil? (:error %)) exprs)})))
                                query-iters)
                        ai-msg    (cond-> (assistant-msg (or answer "") (or (:created-at q) (java.util.Date.)))
                                    true       (assoc :query-id (:id q))
                                    (seq trace) (assoc :trace trace :raw-answer (or answer ""))
                                    model  (assoc :model model)
                                    iters  (assoc :iterations iters)
                                    dur-ms (assoc :duration-ms dur-ms)
                                    cost   (assoc :cost cost)
                                    (seq tokens) (assoc :tokens tokens))]
                    [user-msg ai-msg])))
        queries))
    (catch Exception e
      (t/log! :warn (str "Failed to rebuild history: " (ex-message e)))
      [])))

(defn make-conversation
  "Create a fresh `:vis` conversation. Returns `{:id conv-id :history []}`."
  [_provider-config]
  (let [{:keys [id]} (conversations/create! :vis)]
    {:id id :history []}))

(defn- resolve-resume-id
  "Resolve a resume id. Accepts full UUID or an unambiguous prefix
   among :vis conversations. Returns full UUID string or nil."
  [conversation-id]
  (let [cid (some-> conversation-id str str/trim)]
    (when (seq cid)
      (or (some-> (conversations/by-id cid) :id str)
        (let [matches (->> (conversations/by-channel :vis)
                        (map :id)
                        (filter #(str/starts-with? (str %) cid))
                        vec)]
          (when (= 1 (count matches))
            (str (first matches))))))))

(defn resume-conversation
  "Resume an existing conversation by id.
   Accepts full UUID or unambiguous short UUID prefix.
   Returns `{:id conv-id :history [...]}` with persisted messages."
  [conversation-id]
  (when-let [resolved-id (resolve-resume-id conversation-id)]
    (when-let [conv (conversations/by-id resolved-id)]
      {:id (str (:id conv)) :history (rebuild-history (str (:id conv)))})))

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
  ([conv text] (query! conv text {}))
  ([{:keys [id]} text {:keys [on-chunk cancel-atom]}]
   (try
      (let [send-opts (cond-> {}
                        on-chunk    (assoc :hooks {:on-chunk on-chunk})
                        cancel-atom (assoc :cancel-atom cancel-atom))
           result (conversations/send! id text send-opts)
           cancelled? (= :cancelled (:status result))
           answer (or (:answer result)
                    (when cancelled? "_Cancelled by user._")
                    "[empty response]")
           model  (or (get-in result [:cost :model]) (get result :model))
           tokens (:tokens result)
           cost   (:cost result)]
       (let [confidence (:confidence result)]
         (cond-> {:answer      (if (string? answer) answer (pr-str answer))
                  :iterations  (or (:iterations result) 1)
                  :duration-ms (:duration-ms result)
                  :query-id    (:query-id result)}
           model      (assoc :model model)
           tokens     (assoc :tokens tokens)
           cost       (assoc :cost cost)
           confidence (assoc :confidence confidence))))
     ;; future-cancel from the TUI translates to thread interruption.
     ;; The shared channels.cancellation predicate folds in
     ;; InterruptedException, CancellationException, and any runtime
     ;; wrapper that hides one in its cause chain — surface those as
     ;; a clean cancelled answer, not a generic error.
     (catch Exception e
       (if (cancellation/cancellation? e)
         (do (.interrupt (Thread/currentThread))
           {:answer "_Cancelled by user._" :iterations 0 :status :cancelled})
         (do (t/log! :error (str "Query failed: " (ex-message e)))
           {:error (conversations/error->user-message e)}))))))

(defn dispose!
  "Release the TUI's env handle. Conversation data stays in
   `~/.vis/vis.mdb` so the web server (same `:vis` channel) can still see
   it."
  [{:keys [id]}]
  (when id (conversations/close! id)))
