(ns com.blockether.vis.channels.tui.chat
  "TUI-side projections over the shared conversations API.

   On startup the TUI resumes the latest `:vis` conversation if one exists,
   otherwise creates a fresh one. Conversation data is persisted in
   `~/.vis/vis.mdb` so you can come back to it."
  (:require [com.blockether.vis.channels.tui.render :as render]
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
          ref     [:id (java.util.UUID/fromString (str conv-id))]
          queries (db/db-list-conversation-queries d ref)]
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
                        query-iters (db/db-list-query-iterations d [:id (:id q)])
                        trace (into []
                                (map (fn [it]
                                       (let [exprs (db/db-list-iteration-expressions d [:id (:id it)])
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

(defn resume-conversation
  "Resume an existing conversation by id.
   Returns `{:id conv-id :history [...]}` with persisted messages."
  [conversation-id]
  (when-let [conv (conversations/by-id conversation-id)]
    {:id (str (:id conv)) :history (rebuild-history (str (:id conv)))}))

(defn query!
  "Send a user query through the shared conversations cache. Blocking.
   Returns `{:answer str}` or `{:error str}`.

   `opts` may contain:
     :on-chunk — fn receiving `{:iteration :thinking :code :final :done?}`
                 on every streaming chunk from the RLM. The TUI uses this
                 to project a live per-iteration progress timeline into
                 the assistant placeholder bubble."
  ([conv text] (query! conv text {}))
  ([{:keys [id]} text {:keys [on-chunk]}]
   (try
      (let [send-opts (cond-> {}
                        on-chunk (assoc :hooks {:on-chunk on-chunk}))
           result (conversations/send! id text send-opts)
           answer (or (:answer result) "[empty response]")
           model  (or (get-in result [:cost :model]) (get result :model))
           tokens (:tokens result)
           cost   (:cost result)]
       (cond-> {:answer      (if (string? answer) answer (pr-str answer))
                :iterations  (or (:iterations result) 1)
                :duration-ms (:duration-ms result)}
         model  (assoc :model model)
         tokens (assoc :tokens tokens)
         cost   (assoc :cost cost)))
     (catch Exception e
       (t/log! :error (str "Query failed: " (ex-message e)))
       {:error (conversations/error->user-message e)}))))

(defn dispose!
  "Release the TUI's env handle. Conversation data stays in
   `~/.vis/vis.mdb` so the web server (same `:vis` channel) can still see
   it."
  [{:keys [id]}]
  (when id (conversations/close! id)))
