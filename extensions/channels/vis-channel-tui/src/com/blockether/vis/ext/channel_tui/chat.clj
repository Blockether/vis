(ns com.blockether.vis.ext.channel-tui.chat
  "TUI-side projections over the shared sessions API.

   On startup the TUI creates a fresh `:tui` session by default.
   Pass `--session-id ID` or `--resume` to pick up an existing one.
   Session data is persisted in `~/.vis/vis.mdb` so you can come
   back to it."
  (:require [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.internal.extension :as extension]
            [com.blockether.vis.internal.format :as fmt]
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
    ;; Keep the raw text so resume paths (input-history arrow cycling in
    ;; state/:init-session, search, etc.) can recover the exact string
    ;; the user typed without re-rendering IR back to markdown.
    :text      (or text "")
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
   terminal paths such as cancellation can carry human-readable strings.
   The channel must still hand `assistant-message` canonical IR, never raw text."
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
  "Reconstruct message history from DB for a session.
   Returns a vec of {:role :user|:assistant :text str :timestamp #inst ...}.
   Assistant messages include the code execution trace from all iterations."
  [session-id]
  (try
    (let [d               (vis/db-info)
          restored-values (try
                            (history-restore/restored-var-values d session-id)
                            (catch Throwable e
                              (t/log! {:level :warn :id ::restore-values-for-history-failed
                                       :data  (exception->log-data e)
                                       :msg   (str "Failed to read restored var values for history: " (ex-message e))})
                              {}))
          turns           (vis/db-list-session-turns d session-id)]
      (into []
        (mapcat (fn [q]
                  (let [user-message (user-message (or (:user-request q) "") (or (:created-at q) (java.util.Date.)))
                        ;; Modern rows store Nippy-thawed canonical IR;
                        ;; legacy terminal paths may have persisted a string.
                        ;; Normalize both.
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
                        ;; vectors so resumed sessions render the
                        ;; same way live ones do - just the answer
                        ;; text below the iteration trace, never the
                        ;; `(done "...")` call as code above it.
                        turn-iterations (vis/db-list-session-turn-iterations d (:id q))
                        last-iteration-id (some-> (last turn-iterations) :id)
                        llm-routing (some-> (last turn-iterations) :metadata :llm)
                        ;; Empty IR is `[:ir {}]` (count 2 — just root tag
                        ;; + attrs); a real answer adds at least one block.
                        produced-answer? (and (canonical-ir? (:answer q)) (> (count (:answer q)) 2))
                        trace (into []
                                (map (fn [it]
                                       (let [;; `:render-segments` is a derived view of `:code`
                                             ;; (top-level form classification). NOT stored in
                                             ;; the DB — rederived on every render via the
                                             ;; pure `parse-block-display` parser so display
                                             ;; logic stays driven by the same classifier
                                             ;; that gates the live channel, with zero
                                             ;; serialization drift.
                                             code (or (:code it) "")
                                             segments (vis/parse-block-display code)
                                             all-exprs   [(cond-> {:position 0
                                                                   :code code}
                                                            (contains? it :result) (assoc :result (:result it))
                                                            (contains? it :error) (assoc :error (:error it))
                                                            (contains? it :channel) (assoc :channel (:channel it))
                                                            (seq segments) (assoc :render-segments segments)
                                                            (contains? it :duration-ms)
                                                            (assoc :duration-ms (:duration-ms it)))]
                                             answer-here? (and produced-answer?
                                                            (= (:id it) last-iteration-id)
                                                            (seq all-exprs))
                                             ;; Elide:
                                             ;; 1. the `(done "...")` form on the answer iteration
                                             ;;    (rule b': always last form)
                                             ;; 2. any block tagged `:vis/preflight?` — those are
                                             ;;    synthetic gate rejections, model-facing only,
                                             ;;    never displayed to the user.
                                             ;; 3. structurally-silent host bookkeeping such as
                                             ;;    `(set-session-title! ...)` and answer
                                             ;;    emission forms. They affect chrome/final answer,
                                             ;;    but should not render as normal trace code.
                                             ;; Other successful `:vis/silent` forms are retained
                                             ;; and marked in `:silents`; the TUI setting decides
                                             ;; whether to render them.
                                             visible-code-segments?
                                             (fn [b]
                                               (boolean (some #(= :code (:kind %)) (:render-segments b))))
                                             structurally-silent-block?
                                             (fn [b]
                                               (let [code (str (:code b))
                                                     trimmed (str/triml code)]
                                                 (boolean
                                                   (or (:vis/structurally-silent? b)
                                                     (str/starts-with? trimmed "(done")
                                                     (and (not (visible-code-segments? b))
                                                       (or (str/includes? code "(set-session-title!")
                                                         (str/includes? code "(done")))
                                                     (and (= :vis/silent (:result b))
                                                       (not (seq (:render-segments b)))
                                                       (or (str/includes? code "(set-session-title!")
                                                         (str/includes? code "(done")))))))
                                             preflight-idxs (into #{}
                                                              (keep-indexed
                                                                (fn [i b] (when (:vis/preflight? b) i)))
                                                              all-exprs)
                                             answer-idx  (when answer-here?
                                                           (let [idx (or (:answer-position it)
                                                                       (dec (count all-exprs)))
                                                                 block (when (and (integer? idx)
                                                                               (not (neg? idx))
                                                                               (< idx (count all-exprs)))
                                                                         (get all-exprs idx))]
                                                             (when (and block
                                                                     (not (visible-code-segments? block))
                                                                     (or (= :vis/answer (:result block))
                                                                       (str/includes? (str (:code block)) "(done")))
                                                               idx)))
                                             elide-idxs  (cond-> preflight-idxs
                                                           (some? answer-idx) (conj answer-idx))
                                             exprs       (into []
                                                           (keep-indexed
                                                             (fn [idx expr]
                                                               (when-not (contains? elide-idxs idx)
                                                                 expr)))
                                                           all-exprs)
                                             code-exprs  (if (and (seq exprs)
                                                               (every? structurally-silent-block? exprs))
                                                           []
                                                           exprs)
                                             result-kind (fn [{:keys [result error]}]
                                                           (cond
                                                             error :error
                                                             (extension/tool-result? result) :tool
                                                             :else :value))
                                             tool-result-detail
                                             (fn [result]
                                               (when (extension/tool-result? result)
                                                 (let [metadata (:metadata result)]
                                                   (merge (select-keys result [:symbol :tag])
                                                     (select-keys metadata [:spec :paths :hit-count :truncated-by
                                                                            :command :cwd :target])))))
                                             result-strs (mapv (fn [{:keys [result error channel code]}]
                                                                 (let [restored (when (history-restore/runtime-ref? result)
                                                                                  (history-restore/restored-def-result restored-values code))]
                                                                   (cond
                                                                     error (extension/default-error-ir {:success? false :error error})
                                                                     (seq channel)
                                                                     ;; Per-form sink entries: walk each, surface
                                                                     ;; pre-rendered IR on success, format the
                                                                     ;; error map on failure. Same shape as the live
                                                                     ;; progress path in `internal/progress.clj`. Sort
                                                                     ;; by :position so racy futures land in canonical
                                                                     ;; source order. This must win over `{:vis/ref :expr}`:
                                                                     ;; `(def x (v/cat ...))` cannot persist the live var value,
                                                                     ;; but its tool-rendered channel text is durable and should
                                                                     ;; be shown when resuming history.
                                                                     (extension/combine-render-values
                                                                       (map (fn [{:keys [success? result error]}]
                                                                              (if success?
                                                                                result
                                                                                (extension/default-error-ir
                                                                                  {:success? false :result nil :info {} :error error})))
                                                                         (sort-by :position channel)))
                                                                     restored restored
                                                                     (= :vis/answer result) nil
                                                                     (history-restore/runtime-ref? result)
                                                                     "<runtime value; re-evaluate expression to restore>"
                                                                     (extension/tool-result? result)
                                                                     (extension/render-tool-result result)
                                                                     :else (fmt/bounded-value-str result))))
                                                           exprs)
                                             result-details (mapv (fn [expr]
                                                                    (tool-result-detail (:result expr)))
                                                              exprs)
                                             durations   (mapv #(or (:duration-ms %) 0) exprs)
                                             silents     (mapv (fn [expr]
                                                                 (and (nil? (:error expr))
                                                                   (or (:vis/silent expr)
                                                                     (= :vis/silent (:result expr))
                                                                     (structurally-silent-block? expr))))
                                                           exprs)]
                                         {:position  (when-let [p (:position it)] (dec (long p)))
                                          :thinking  (visible-thinking (:thinking it))
                                          :provider-fallbacks (get-in it [:metadata :llm :trace])
                                          :code      (mapv :code code-exprs)
                                          :comments  (mapv :comment code-exprs)
                                          :render-segments (mapv :render-segments code-exprs)
                                          :results   result-strs
                                          :result-kinds (mapv result-kind exprs)
                                          :result-details result-details
                                          :durations durations
                                          :successes (mapv #(nil? (:error %)) exprs)
                                          :silents   silents})))
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
                                            true       (assoc :session-turn-id (:id q))
                                            (seq trace) (assoc :traces trace :ir answer-ir)
                                            model  (assoc :model model)
                                            (:selected llm-routing) (assoc :llm-selected (:selected llm-routing))
                                            (:actual llm-routing) (assoc :llm-actual (:actual llm-routing))
                                            (contains? llm-routing :fallback?) (assoc :llm-fallback? (:fallback? llm-routing))
                                            (seq (:trace llm-routing)) (assoc :llm-routing-trace (:trace llm-routing))
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

(defn make-session
  "Create a fresh `:tui` session. Returns `{:id session-id :history []}`."
  [_provider-config]
  (let [{:keys [id]} (vis/create! :tui)]
    {:id id :history []}))

(defn- resolve-resume-id
  "Resolve a resume id. Accepts full UUID or an unambiguous prefix
   among :tui sessions. Returns full UUID string or nil."
  [session-id]
  (let [cid (some-> session-id str str/trim)]
    (when (seq cid)
      (or (some-> (vis/by-id cid) :id str)
        (let [matches (->> (vis/by-channel :tui)
                        (map :id)
                        (filter #(str/starts-with? (str %) cid))
                        vec)]
          (when (= 1 (count matches))
            (str (first matches))))))))

(defn resume-session
  "Resume an existing session by id.
   Accepts full UUID or unambiguous short UUID prefix.
   Returns `{:id session-id :history [...]}` with persisted messages."
  [session-id]
  (when-let [resolved-id (resolve-resume-id session-id)]
    (when-let [session (vis/by-id resolved-id)]
      {:id (str (:id session)) :history (rebuild-history (str (:id session)))})))

(defn turn!
  "Send a user request through the shared sessions cache. Blocking.
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
  ([session text] (turn! session text {}))
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
           ;; (`[:ir & nodes]`), but cancellation terminal paths may carry raw
           ;; status text. Normalize here so `render-answer` /
           ;; `assistant-message` never see raw strings.
           answer (answer->ir (:answer result)
                    (if cancelled? "Cancelled by user." "[empty response]"))
           model  (or (get-in result [:cost :model]) (get result :model))
           provider (or (get-in result [:cost :provider]) (get result :provider))
           tokens (:tokens result)
           cost   (:cost result)
           confidence (:confidence result)
           llm-selected (:llm-selected result)
           llm-actual (:llm-actual result)
           llm-fallback? (:llm-fallback? result)
           llm-routing-trace (:llm-routing-trace result)]
       ;; Return canonical IR on `:answer`. The bubble layer (state
       ;; event handler -> assistant-message -> render-answer) is
       ;; the single rendering chokepoint. Pre-rendering here would
       ;; force every consumer back through string parsing.
       (cond-> {:answer          answer
                :iteration-count (or (:iteration-count result) 1)
                :duration-ms     (:duration-ms result)
                :session-turn-id        (:session-turn-id result)}
         model      (assoc :model model)
         provider   (assoc :provider provider)
         llm-selected (assoc :llm-selected llm-selected)
         llm-actual (assoc :llm-actual llm-actual)
         (some? llm-fallback?) (assoc :llm-fallback? llm-fallback?)
         (seq llm-routing-trace) (assoc :llm-routing-trace llm-routing-trace)
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
  "Release the TUI's env handle. Session data stays in
   `~/.vis/vis.mdb` so other consumers of the `:tui` channel
   (e.g. `vis sessions tui`, future inspectors) still see it."
  [{:keys [id]}]
  (when id (vis/close! id)))
