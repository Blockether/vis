(ns com.blockether.vis.ext.channel-tui.chat
  "TUI-side projections over the shared conversations API.

   On startup the TUI creates a fresh `:tui` conversation by default.
   Pass `--conversation-id ID` or `--resume` to pick up an existing one.
   Conversation data is persisted in `~/.vis/vis.mdb` so you can come
   back to it."
  (:require [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.internal.extension :as extension]
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

(def ^:private encrypted-reasoning-placeholder
  "[provider returned encrypted reasoning; plaintext reasoning is unavailable]")

(defn- visible-thinking [thinking]
  (let [s (some-> thinking str)]
    (when-not (or (str/blank? (or s "")) (= encrypted-reasoning-placeholder s))
      s)))

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
    (let [d     (vis/db-info)
          turns (vis/db-list-conversation-turns d conversation-id)]
      (into []
        (mapcat (fn [q]
                  (let [user-message (user-message (or (:user-request q) "") (or (:created-at q) (java.util.Date.)))
                        answer    (or (:answer q) "")
                        model     (:model q)
                        tokens    (cond-> {}
                                    (:input-tokens q)     (assoc :input (:input-tokens q))
                                    (:output-tokens q)    (assoc :output (:output-tokens q))
                                    (:reasoning-tokens q) (assoc :reasoning (:reasoning-tokens q))
                                    (:cached-tokens q)    (assoc :cached (:cached-tokens q)))
                        iteration-count (:iteration-count q)
                        duration-ms (:duration-ms q)
                        cost      (when-let [total-cost (or (:total-cost q) (:cost q))]
                                    (cond-> {:total-cost total-cost}
                                      (:provider q) (assoc :provider (:provider q))
                                      (:model q)    (assoc :model (:model q))))
                        ;; Rebuild trace from iterations + blocks.
                        ;; The answer-bearing form (last expression of
                        ;; the answer iteration, per rule b') is
                        ;; ELIDED from the per-iteration parallel
                        ;; vectors so resumed conversations render the
                        ;; same way live ones do — just the answer
                        ;; text below the iteration trace, never the
                        ;; `(answer "…")` call as code above it.
                        turn-iterations (vis/db-list-conversation-turn-iterations d (:id q))
                        last-iteration-id (some-> (last turn-iterations) :id)
                        produced-answer? (and (some? answer)
                                           (not (str/blank? (str answer))))
                        trace (into []
                                (map (fn [it]
                                       (let [all-exprs   (vec (vis/db-list-iteration-blocks d (:id it)))
                                             answer-here? (and produced-answer?
                                                            (= (:id it) last-iteration-id)
                                                            (seq all-exprs))
                                             elide-idxs  (cond-> (into #{}
                                                                   (keep-indexed
                                                                     (fn [idx {:keys [result rendering-kind]}]
                                                                       (when (or (= :vis/silent rendering-kind)
                                                                               (= :vis/silent result)) idx)))
                                                                   all-exprs)
                                                           answer-here? (conj (dec (count all-exprs))))
                                             exprs       (into []
                                                           (keep-indexed
                                                             (fn [idx expr]
                                                               (when-not (contains? elide-idxs idx)
                                                                 expr)))
                                                           all-exprs)
                                             result-kind (fn [{:keys [result error]}]
                                                           (cond
                                                             error :error
                                                             (and (extension/tool-result? result)
                                                               (= :v/preview (get-in result [:provenance :op]))) :preview
                                                             (extension/tool-result? result) :tool
                                                             :else :value))
                                             tool-result-detail
                                             (fn [result]
                                               (when (extension/tool-result? result)
                                                 (let [prov (:provenance result)]
                                                   (cond-> (select-keys prov [:op :op-class :presentation-kind :color-role])
                                                     (= :v/preview (:op prov))
                                                     (assoc :raw (pr-str (:result result)))))))
                                             result-strs (mapv (fn [{:keys [result error] :as expr}]
                                                                 (cond
                                                                   error (vis/format-error error)
                                                                   (and (map? result) (= :expr (:vis/ref result)))
                                                                   "<runtime value; re-evaluate expression to restore>"
                                                                   (extension/tool-result? result)
                                                                   (extension/render-tool-result :tui result {:block expr})
                                                                   :else (pr-str result)))
                                                           exprs)
                                             result-details (mapv (comp tool-result-detail :result) exprs)
                                             stdout-strs (mapv #(or (:stdout %) "") exprs)
                                             durations   (mapv #(or (:duration-ms %) 0) exprs)]
                                         {:thinking  (visible-thinking (:thinking it))
                                          :code      (mapv :code exprs)
                                          :comments  (mapv :comment exprs)
                                          :results   result-strs
                                          :result-kinds (mapv result-kind exprs)
                                          :result-details result-details
                                          :stdouts   stdout-strs
                                          :durations durations
                                          :successes (mapv #(nil? (:error %)) exprs)})))
                                turn-iterations)
                        ;; `:prior-outcome :cancelled` is how the
                        ;; persistance layer marks an aborted turn (the
                        ;; sweep + cancel paths both write that value).
                        ;; Surface it as `:status :cancelled` on the
                        ;; assistant message so the bubble renderer
                        ;; emits the trace + dim italic status footer
                        ;; the same way live cancellations render —
                        ;; on bare terminal-bg, no bubble-wide fill.
                        cancelled? (= :cancelled (:prior-outcome q))
                        assistant-message (cond-> (assistant-message (or answer "") (or (:created-at q) (java.util.Date.)))
                                            true       (assoc :conversation-turn-id (:id q))
                                            (seq trace) (assoc :trace trace :raw-answer (or answer ""))
                                            model  (assoc :model model)
                                            iteration-count (assoc :iteration-count iteration-count)
                                            duration-ms (assoc :duration-ms duration-ms)
                                            cost   (assoc :cost cost)
                                            (seq tokens) (assoc :tokens tokens)
                                            cancelled? (assoc :status :cancelled))]
                    [user-message assistant-message])))
        turns))
    (catch Exception e
      (t/log! {:level :warn :id ::rebuild-history-failed
               :data  (exception->log-data e)
               :msg   (str "Failed to rebuild history: " (ex-message e))})
      [])))

(defn make-conversation
  "Create a fresh `:tui` conversation. Returns `{:id conversation-id :history []}`."
  [_provider-config]
  (let [{:keys [id]} (vis/create! :tui)]
    {:id id :history []}))

(defn- resolve-resume-id
  "Resolve a resume id. Accepts full UUID or an unambiguous prefix
   among :tui conversations. Returns full UUID string or nil."
  [conversation-id]
  (let [cid (some-> conversation-id str str/trim)]
    (when (seq cid)
      (or (some-> (vis/by-id cid) :id str)
        (let [matches (->> (vis/by-channel :tui)
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
    (when-let [conversation (vis/by-id resolved-id)]
      {:id (str (:id conversation)) :history (rebuild-history (str (:id conversation)))})))

(defn turn!
  "Send a user request through the shared conversations cache. Blocking.
   Returns `{:answer str}` or `{:error str}`.

   `opts` may contain:
     :on-chunk          — fn receiving phased chunks `{:phase :iteration ...}`. Phases:
                          `:reasoning` (LLM streaming), `:form-result` (one form done),
                          `:iteration-final` (iteration complete), `:iteration-error`
                          (iteration aborted). See `progress/make-progress-tracker`
                          for accumulating chunks into a timeline.
                          on every streaming chunk from the RLM. The TUI uses this
                          to project a live per-iteration progress timeline into
                          the assistant placeholder bubble.
     :cancel-atom       — (atom bool) honored by the iteration loop; flipping
                          it to true causes the current turn to terminate at
                          the next safe point and return `{:status :cancelled}`.
     :reasoning-default — base reasoning effort (`:quick`, `:balanced`, `:deep`)
                          forwarded to `vis/send!` for reasoning-capable models.
     :extra-body        — provider-specific request-body overrides forwarded to
                          `vis/send!` unchanged.
     :turn-features     — per-turn feature flags consumed by extension prompts."
  ([conversation text] (turn! conversation text {}))
  ([{:keys [id]} text {:keys [on-chunk cancel-atom reasoning-default extra-body turn-features]}]
   (try
     (let [send-opts (cond-> {}
                       on-chunk          (assoc :hooks {:on-chunk on-chunk})
                       cancel-atom       (assoc :cancel-atom cancel-atom)
                       reasoning-default (assoc :reasoning-default reasoning-default)
                       extra-body        (assoc :extra-body extra-body)
                       turn-features     (assoc :turn/features turn-features))
           result (vis/send! id text send-opts)
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
       (cond-> {:answer          (if (string? answer) answer (pr-str answer))
                :iteration-count (or (:iteration-count result) 1)
                :duration-ms     (:duration-ms result)
                :conversation-turn-id        (:conversation-turn-id result)}
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
       (if (vis/cancellation? e)
         (do (.interrupt (Thread/currentThread))
           {:answer "Cancelled by user." :iteration-count 0 :status :cancelled})
         (do
           ;; Log EVERYTHING. Stripping a stack trace at the channel
           ;; boundary is how a `[SQLITE_CANTOPEN]` ends up untriagable
           ;; — the user sees a one-liner and the log has the same
           ;; one-liner. With the full trace, the next failure pinpoints
           ;; the exact JDBC call that opened the bad handle.
           (t/log! {:level :error :id ::turn-failed
                    :data  (exception->log-data e)
                    :msg   (str "Query failed: " (ex-message e))})
           {:error (vis/db-error->user-message e)}))))))

(defn dispose!
  "Release the TUI's env handle. Conversation data stays in
   `~/.vis/vis.mdb` so other consumers of the `:tui` channel
   (e.g. `vis conversations tui`, future inspectors) still see it."
  [{:keys [id]}]
  (when id (vis/close! id)))
