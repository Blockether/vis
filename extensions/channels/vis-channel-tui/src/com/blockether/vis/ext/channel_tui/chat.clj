(ns com.blockether.vis.ext.channel-tui.chat
  "TUI-side projections over the shared conversations API.

   On startup the TUI creates a fresh `:tui` conversation by default.
   Pass `--conversation-id ID` or `--resume` to pick up an existing one.
   Conversation data is persisted in `~/.vis/vis.mdb` so you can come
   back to it."
  (:require [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.internal.extension :as extension]
            [com.blockether.vis.internal.history-restore :as history-restore]
            [taoensso.telemere :as t])
  (:import [java.io PrintWriter StringWriter]))

(defn- exception->log-data
  "Build a log-friendly map from an exception that preserves EVERYTHING
   diagnostic: class, message, ex-data, full stack trace, and the
   cause chain. The whole point is that when an error reaches the
   chat boundary, the next person reading `~/.vis/vis.log` should not
   have to guess where it came from - the stack is right there.

   `.printStackTrace` includes nested causes (\"Caused by: ...\" blocks)
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
  (let [s (some-> thinking str str/trim)]
    (when-not (or (str/blank? (or s "")) (= encrypted-reasoning-placeholder s))
      s)))

(defn user-message
  "Create a structured user message with timestamp.
   The user types raw markdown into the input box; we lift it to
   canonical IR via `vis/text->ir` immediately so the bubble layer
   (and every downstream consumer) sees the same shape it sees for
   assistant answers.

   The message carries ONLY `:ir` — every consumer that needs a
   string projection computes it on demand (and caches the result
   client-side, e.g. via `virtual.clj` projection) instead of us
   eagerly rendering at construction."
  ([text] (user-message text (java.util.Date.)))
  ([text timestamp]
   {:role      :user
    :ir        (vis/text->ir text)
    :timestamp timestamp}))

(def empty-ir
  "Canonical empty IR — used as the placeholder when an answer slot is
   absent (e.g. resumed turns whose answer column is NULL because the
   turn never finished). Never feed `nil` or `\"\"` to the TUI render
   chokepoints; lift to `empty-ir` instead."
  [:ir {}])

(defn- canonical-ir?
  [x]
  (and (vector? x) (= :ir (first x))))

(defn- answer->ir
  "Normalize a turn result answer into canonical IR for the TUI boundary.

   The normal `vis/send!` success path already returns `[:ir ...]`, but
   terminal paths such as cancellation / iteration-cap fallbacks can carry
   human-readable strings. The channel must still hand `assistant-message`
   canonical IR, never raw text."
  [answer fallback-text]
  (cond
    (canonical-ir? answer) answer
    (some? answer)         (vis/text->ir (if (string? answer) answer (pr-str answer)))
    (seq fallback-text)    (vis/text->ir fallback-text)
    :else                  empty-ir))

(defn render-answer
  "Render canonical answer-IR (`[:ir & nodes]`) to the markdown string
   the TUI bubble renderer expects.

   STRICT input contract: IR only. `nil` is accepted as a convenience
   for unfilled answer slots and renders as the empty string. Strings,
   Hiccup vectors, EDN values, etc. are programmer bugs — the IR
   boundary lives upstream (loop emits IR, persistence stores Nippy IR,
   resumed turns thaw to IR). Anything else throws with the offending
   type so the bug surfaces at the wrong call site, not silently in the
   bubble.

   Dispatches via the TUI channel's `:channel/messages-renderer-fn`
   (`core/render-for-tui`) when registered; otherwise falls back to
   `vis/render :markdown` directly so unit tests that load this ns in
   isolation still work. Both paths share the same strict IR contract."
  [ir]
  (cond
    (nil? ir) ""
    (not (and (vector? ir) (= :ir (first ir))))
    (throw (ex-info "chat/render-answer requires canonical [:ir ...] input; build IR upstream, do not pass raw text"
             {:got-type (some-> ir class .getName)
              :got-preview (let [s (pr-str ir)]
                             (subs s 0 (min 200 (count s))))}))
    :else
    (let [renderer (some-> (vis/channel-by-id :tui) :channel/messages-renderer-fn)]
      (if renderer (renderer ir) (vis/render ir :markdown)))))

(defn assistant-message
  "Create a structured assistant (vis) message with timestamp.
   STRICT: `ir` must be canonical `[:ir & nodes]` (or nil for empty
   placeholder). Non-IR inputs throw via `render-answer`.

   The message carries ONLY `:ir`. Walker-driven projection
   (`virtual.clj`) computes the rendered markdown string lazily and
   caches it on the projected message map; nothing here pre-renders."
  ([ir] (assistant-message ir (java.util.Date.)))
  ([ir timestamp]
   ;; Validate IR contract eagerly so non-IR garbage surfaces at the
   ;; construction site, not deep in the painter.
   (let [ir (or ir empty-ir)]
     (when-not (and (vector? ir) (= :ir (first ir)))
       (throw (ex-info "chat/assistant-message requires canonical [:ir ...] input"
                {:got-type (some-> ir class .getName)})))
     {:role      :assistant
      :ir        ir
      :timestamp timestamp})))

(defn- rebuild-history
  "Reconstruct message history from DB for a conversation.
   Returns a vec of {:role :user|:assistant :text str :timestamp #inst ...}.
   Assistant messages include the code execution trace from all iterations."
  [conversation-id]
  (try
    (let [d               (vis/db-info)
          restored-values (try
                            (history-restore/restored-var-values d conversation-id)
                            (catch Throwable e
                              (t/log! {:level :warn :id ::restore-values-for-history-failed
                                       :data  (exception->log-data e)
                                       :msg   (str "Failed to read restored var values for history: " (ex-message e))})
                              {}))
          turns           (vis/db-list-conversation-turns d conversation-id)]
      (into []
        (mapcat (fn [q]
                  (let [user-message (user-message (or (:user-request q) "") (or (:created-at q) (java.util.Date.)))
                        ;; Modern rows store Nippy-thawed canonical IR;
                        ;; legacy terminal paths may have persisted a string
                        ;; (for example iteration-cap text). Normalize both.
                        answer-ir (answer->ir (:answer q) nil)
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
                        ;; same way live ones do - just the answer
                        ;; text below the iteration trace, never the
                        ;; `(answer "...")` call as code above it.
                        turn-iterations (vis/db-list-conversation-turn-iterations d (:id q))
                        last-iteration-id (some-> (last turn-iterations) :id)
                        ;; Empty IR is `[:ir {}]` (count 2 — just root tag
                        ;; + attrs); a real answer adds at least one block.
                        produced-answer? (and (canonical-ir? (:answer q)) (> (count (:answer q)) 2))
                        trace (into []
                                (map (fn [it]
                                       (let [all-exprs   (vec (vis/db-list-iteration-blocks d (:id it)))
                                             answer-here? (and produced-answer?
                                                            (= (:id it) last-iteration-id)
                                                            (seq all-exprs))
                                             ;; Elide:
                                             ;; 1. the `(answer "...")` form on the answer iteration
                                             ;;    (rule b': always last form)
                                             ;; 2. any block tagged `:vis/preflight?` — those are
                                             ;;    synthetic gate rejections, model-facing only,
                                             ;;    never displayed to the user.
                                             preflight-idxs (into #{}
                                                              (keep-indexed
                                                                (fn [i b] (when (:vis/preflight? b) i)))
                                                              all-exprs)
                                             silent-idxs (into #{}
                                                           (keep-indexed
                                                             (fn [i b]
                                                               (when (or (:vis/silent b)
                                                                       (= :vis/silent (:result b)))
                                                                 i)))
                                                           all-exprs)
                                             answer-idx  (when answer-here?
                                                           (let [idx (or (:answer-form-idx it)
                                                                       (dec (count all-exprs)))]
                                                             (when (and (integer? idx)
                                                                     (not (neg? idx))
                                                                     (< idx (count all-exprs)))
                                                               idx)))
                                             elide-idxs  (cond-> (into preflight-idxs silent-idxs)
                                                           (some? answer-idx) (conj answer-idx))
                                             exprs       (into []
                                                           (keep-indexed
                                                             (fn [idx expr]
                                                               (when-not (contains? elide-idxs idx)
                                                                 expr)))
                                                           all-exprs)
                                             result-kind (fn [{:keys [result error]}]
                                                           (cond
                                                             error :error
                                                             (extension/tool-result? result) :tool
                                                             :else :value))
                                             tool-result-detail
                                             (fn [result]
                                               (when (extension/tool-result? result)
                                                 (let [metadata (:op/metadata result)
                                                       payload  (:op/result result)]
                                                   (cond-> (merge (select-keys result [:op/symbol :op/tag])
                                                             (select-keys metadata [:spec :paths :hit-count :truncated-by
                                                                                    :command :cwd :target]))
                                                     (:stdout payload)
                                                     (assoc :stdout (:stdout payload))
                                                     (:stderr payload)
                                                     (assoc :stderr (:stderr payload))))))
                                             result-strs (mapv (fn [{:keys [result error channel code]}]
                                                                 (let [restored (when (history-restore/runtime-ref? result)
                                                                                  (history-restore/restored-def-result restored-values code))]
                                                                   (cond
                                                                     error (vis/format-error error)
                                                                     (seq channel)
                                                                     ;; Per-form sink entries: walk each, surface
                                                                     ;; pre-rendered markdown on success, format the
                                                                     ;; error map on failure. Same shape as the live
                                                                     ;; progress path in `internal/progress.clj`. Sort
                                                                     ;; by :position so racy futures land in canonical
                                                                     ;; source order. This must win over `{:vis/ref :expr}`:
                                                                     ;; `(def x (v/cat ...))` cannot persist the live var value,
                                                                     ;; but its tool-rendered channel text is durable and should
                                                                     ;; be shown when resuming history.
                                                                     (str/join "\n\n"
                                                                       (map (fn [{:keys [success? result error]}]
                                                                              (if success?
                                                                                result
                                                                                (extension/default-channel-error-text
                                                                                  {:success? false :result nil :info {} :error error})))
                                                                         (sort-by :position channel)))
                                                                     restored restored
                                                                     (history-restore/runtime-ref? result)
                                                                     "<runtime value; re-evaluate expression to restore>"
                                                                     (extension/tool-result? result)
                                                                     (extension/channel-render-tool-result result)
                                                                     :else (pr-str result))))
                                                           exprs)
                                             result-details (mapv (fn [expr]
                                                                    (tool-result-detail (:result expr)))
                                                              exprs)
                                             stdout-strs (mapv #(or (:stdout %) "") exprs)
                                             stderr-strs (mapv #(or (:stderr %) "") exprs)
                                             durations   (mapv #(or (:duration-ms %) 0) exprs)]
                                         {:thinking  (visible-thinking (:thinking it))
                                          :code      (mapv :code exprs)
                                          :comments  (mapv :comment exprs)
                                          :results   result-strs
                                          :result-kinds (mapv result-kind exprs)
                                          :result-details result-details
                                          :stdouts   stdout-strs
                                          :stderrs   stderr-strs
                                          :durations durations
                                          :successes (mapv #(nil? (:error %)) exprs)})))
                                turn-iterations)
                        ;; `:prior-outcome :cancelled` is how the
                        ;; persistance layer marks an aborted turn (the
                        ;; sweep + cancel paths both write that value).
                        ;; Surface it as `:status :cancelled` on the
                        ;; assistant message so the bubble renderer
                        ;; emits the trace + dim italic status footer
                        ;; the same way live cancellations render -
                        ;; on bare terminal-bg, no bubble-wide fill.
                        cancelled? (= :cancelled (:prior-outcome q))
                        assistant-message (cond-> (assistant-message answer-ir (or (:created-at q) (java.util.Date.)))
                                            true       (assoc :conversation-turn-id (:id q))
                                            (seq trace) (assoc :traces trace :ir answer-ir)
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
   Returns `{:answer [:ir ...]}` or `{:error str}`.

   `opts` may contain:
     :on-chunk          - fn receiving phased chunks `{:phase :iteration ...}`. Phases:
                          `:reasoning` (LLM streaming), `:form-result` (one form done),
                          `:iteration-final` (iteration complete), `:iteration-error`
                          (iteration aborted). See `progress/make-progress-tracker`
                          for accumulating chunks into a timeline.
                          on every streaming chunk from the RLM. The TUI uses this
                          to project a live per-iteration progress timeline into
                          the assistant placeholder bubble.
     :cancel-atom       - (atom bool) honored by the iteration loop; flipping
                          it to true causes the current turn to terminate at
                          the next safe point and return `{:status :cancelled}`.
     :reasoning-default - base reasoning effort (`:quick`, `:balanced`, `:deep`)
                          forwarded to `vis/send!` for reasoning-capable models.
     :extra-body        - provider-specific request-body overrides forwarded to
                          `vis/send!` unchanged.
     :turn-features     - per-turn feature flags consumed by extension prompts."
  ([conversation text] (turn! conversation text {}))
  ([{:keys [id]} text {:keys [on-chunk cancel-atom reasoning-default extra-body turn-features workspace]}]
   (try
     (let [send-opts (cond-> {}
                       on-chunk          (assoc :hooks {:on-chunk on-chunk})
                       cancel-atom       (assoc :cancel-atom cancel-atom)
                       reasoning-default (assoc :reasoning-default reasoning-default)
                       extra-body        (assoc :extra-body extra-body)
                       turn-features     (assoc :turn/features turn-features)
                       (seq workspace)   (merge workspace))
           result (vis/send! id text send-opts)
           cancelled? (= :cancelled (:status result))
           ;; `(:answer result)` from `vis/send!` is normally canonical IR
           ;; (`[:ir & nodes]`), but cancellation/iteration-cap terminal paths
           ;; may carry raw status text. Normalize here so `render-answer` /
           ;; `assistant-message` never see raw strings.
           answer (answer->ir (:answer result)
                    (if cancelled? "Cancelled by user." "[empty response]"))
           model  (or (get-in result [:cost :model]) (get result :model))
           tokens (:tokens result)
           cost   (:cost result)
           confidence (:confidence result)]
       ;; Return canonical IR on `:answer`. The bubble layer (state
       ;; event handler -> assistant-message -> render-answer) is
       ;; the single rendering chokepoint. Pre-rendering here would
       ;; force every consumer back through string parsing.
       (cond-> {:answer          answer
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
     ;; wrapper that hides one in its cause chain - surface those as
     ;; a clean cancelled answer, not a generic error.
     (catch Exception e
       (if (vis/cancellation? e)
         (do (.interrupt (Thread/currentThread))
           {:answer [:ir {} [:p {} [:span {} "Cancelled by user."]]]
            :iteration-count 0
            :status :cancelled})
         (do
           ;; Log EVERYTHING. Stripping a stack trace at the channel
           ;; boundary is how a `[SQLITE_CANTOPEN]` ends up untriagable
           ;; - the user sees a one-liner and the log has the same
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
