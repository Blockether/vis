(ns com.blockether.vis.ext.channel-tui.chat
  "TUI-side projections over the canonical in-process gateway.

   On startup the TUI creates a fresh `:tui` gateway session by default.
   Pass `--session-id ID` or `--resume` to pick up an existing one.
   Session data is persisted in `~/.vis/vis.mdb` so you can come back to it."
  (:require [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.internal.iteration :as iteration]
            [taoensso.telemere :as t])
  (:import [java.io PrintWriter StringWriter]))

(set! *unchecked-math* :warn-on-boxed)

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
    {:class (.getName (class e))
     :message (or (ex-message e) (.toString e))
     :ex-data (ex-data e)
     :stack (.toString sw)}))

(def ^:private encrypted-reasoning-placeholder
  "[provider returned encrypted reasoning; plaintext reasoning is unavailable]")

(defn- visible-thinking
  [thinking]
  (let
    [s (some-> thinking
               str
               str/trim)]
    (when-not (or (str/blank? (or s "")) (= encrypted-reasoning-placeholder s)) s)))

;; Engine chrome is detected from the RESULT sentinel (vis_silent / the
;; vis_answer done-call), not a call-head name list — mirrors progress.clj.

(defn- visible-code-segments?
  "True when an iteration block has at least one `:code` segment that
   should reach the user. Mirrors the live progress predicate."
  [b]
  (boolean (some #(= :code (:kind %)) (:render-segments b))))

(defn- structurally-silent-block?
  "True for host-bookkeeping forms that should never appear in user
   traces: structurally code-free recap blocks, and forms whose RESULT is
   the `vis_silent` (title) sentinel. (`done`'s `vis_answer` block is
   elided separately — its answer renders as the turn bubble.)"
  [b]
  (boolean (or (:vis/structurally-silent? b) (= "vis_silent" (:result b)))))

(defn- restored-tool-envelope?
  "True when a restored form's `:result` is a tool-result ENVELOPE after the
   canonical wire hop (`wire/canonical` at the gateway transcript facade):
   keyword identity values arrive as strings, so the spec-strict
   `extension/tool-result?` can no longer match — recognize the envelope
   STRUCTURALLY instead, by the `is_success` flag plus the `:symbol`/`:metadata`
   identity `envelope-of` always stamps. Also true for a raw in-process
   envelope, which carries the same keys."
  [x]
  (and (map? x) (contains? x "is_success") (or (contains? x "symbol") (contains? x "metadata"))))

(defn- form-result-kind
  "Mirror of `progress/form-result-kind` for restored sessions: a
   form is `:tool`-kind whenever it touched the tool surface, even
   indirectly via `:channel` sink entries (e.g. `(def r (ls …))`
   followed by `(select-keys r …)`). Without this the channel's
   preview pane is hidden because the FENCE's last value
   is plain data."
  [{:keys [result error channel]}]
  (cond error :error
        (restored-tool-envelope? result) :tool
        (seq channel) :tool
        :else :value))

(defn- form-result-detail
  "Project tool-result envelope to the small detail map the TUI labels
   consume. Returns nil for non-tool results.

   The transcript arrives in the canonical wire shape (`wire/canonical`
   at the gateway facade): envelope metadata keys read as snake keywords
   and keyword identities as strings. Restored forms persisted via
   `(def x (tool …))` shape have their `:result` deref'd to the inner
   value before persistence (Python binding semantics), so the envelope
   check returns false and the path below would never fire. Fall back to
   the channel slice: every sink entry carries `:symbol` / `:tag`
   (`write-sink-entries!` stamps them from the originating `sym-entry`).
   Use the FIRST successful entry's tag/symbol to label the bubble —
   same shape the live path produces, just sourced from persistance."
  [{:keys [result channel]}]
  (cond (restored-tool-envelope? result)
        (let [metadata (get result "metadata")]
          (merge (into {}
                       (keep (fn [[k wk]]
                               (when-some [v (get result wk)]
                                 [k v])))
                       [[:symbol "symbol"] [:tag "tag"]])
                 (into {}
                       (keep (fn [[k wk]]
                               (when-some [v (get metadata wk)]
                                 [k v])))
                       [[:spec "spec"] [:paths "paths"] [:hit_count "hit_count"]
                        [:truncated_by "truncated_by"] [:command "command"] [:cwd "cwd"]
                        [:target "target"]])))
        (seq channel)
        (let [first-ok (or (first (filter #(get % "is_success") channel)) (first channel))]
          (cond-> {}
            (get first-ok "symbol")
            (assoc :op (get first-ok "symbol"))

            (get first-ok "tag")
            (assoc :tag (get first-ok "tag"))))))

(defn- block->form-record
  "Materialize one DB-iteration block into a `:forms` entry. The shape
   matches the live progress tracker's per-form map so the renderer
   uses one code path for live and resumed traces.

   The display surface is projected through `vis/form->display` — the ONE
   canonical display-key projection (`internal/form.clj`) the live wire and the
   gateway also use — so a NEW display field (print-many `:cards`, the badge
   colour, …) flows onto restored bubbles automatically instead of being
   silently forgotten by a hand-listed map. On top of it we layer only the
   surfaces that AREN'T verbatim display keys: the bounded `:stdout`/`:error`,
   the derived `:duration-ms`/`:channel`, the computed op projections
   (`:result-kind`/`:result-detail`), and the restore-only status flags."
  [block]
  (merge (vis/form->display block)
         {:started-at-ms nil
          :duration-ms (or (:duration-ms block) 0)
          ;; Keep the raw sink slice so the shared `iteration/entry-ops` derives the
          ;; SAME DISPLAY-state ops the live path derives from its `:channel`.
          :channel (vec (:channel block))
          ;; The SINGLE display surface: what this form printed. Persisted per-form
          ;; envelopes carry `:stdout` (loop de-conflated value vs printed); the
          ;; renderer paints it instead of render-fn op cards / result blobs.
          :stdout (:stdout block)
          :result (:result block)
          ;; Op projections computed from the block (the persisted envelope stores the
          ;; rendered card/summary, not these derivations).
          :result-kind (form-result-kind block)
          :result-detail (form-result-detail block)
          :error (:error block)
          :success? (nil? (:error block))
          ;; A restored form is engine chrome (hidden) when structurally code-free
          ;; (answer / title recaps) OR its result is the `vis_silent` sentinel
          ;; (set_session_title) — `structurally-silent-block?`. Parity with the
          ;; live path; `done`'s vis_answer block is elided via answer-position.
          :silent? (and (nil? (:error block))
                        (or (:silent block) (structurally-silent-block? block)))}))

(defn- it->iteration-entry
  "Turn one persisted iteration row into the same shape the live
   progress tracker produces — a map carrying `:thinking`, `:forms`,
   and `:provider-fallbacks`. The renderer consumes both live and
   resumed traces through this single shape."
  [{:keys [produced-answer? last-iteration-id]} it]
  ;; The persisted shape that drives a restored bubble is the
  ;; per-form `:forms` envelope vec on the iteration row (one entry
  ;; per top-level form the model wrote). The legacy code path
  ;; pretended an iteration was ONE block and pulled non-existent
  ;; `:result` / `:error` / `:channel` keys off the row itself — so
  ;; errored forms restored as successful, tool calls lost their
  ;; pi-style preview, and a multi-form fence collapsed into a single
  ;; opaque card. Live bubbles read per-form envelopes from the
  ;; progress tracker; restored bubbles must do the same to render
  ;; identically.
  (let
    [iter-code
     (or (get it "code") "")

     envelopes
     (vec (or (get it "forms") []))

     ;; Sentinel block when the iteration ran no forms (eval timeout,
     ;; pre-eval rejection, etc.). Keep the iteration visible with
     ;; whatever code text we have so the bubble doesn't collapse to
     ;; nothing on restore.
     fallback-blocks
     [(cond-> {:position 0 :code iter-code}
        (some? (get it "render_segments"))
        (assoc :render-segments (get it "render_segments"))

        (some? (get it "duration_ms"))
        (assoc :duration-ms (get it "duration_ms"))

        (some? (get it "result"))
        (assoc :result (get it "result"))

        (some? (get it "error"))
        (assoc :error (get it "error")))]

     envelope->block
     (fn [idx env]
       (let
         [src
          (or (get env "src") (get env "source") (get env "code") "")

          segments
          (vis/parse-block-display src)]

         ;; Project the persisted envelope's WHOLE display-key set via
         ;; `vis/form->display` — the pre-rendered native-tool card
         ;; (`:result-render`/`:result-summary`), the badge identity
         ;; (`:vis/tool-name`/`:tool-color-role`), AND a print-many block's
         ;; per-result `:cards` — so a restored bubble paints the SAME op-card(s)
         ;; the live stream did. A hand-listed copy here is what silently dropped
         ;; them on resume; the canonical projection can't drift. `:code` (env
         ;; stores source under `:src`), `:scope`/`:tag`, and the computed
         ;; `:render-segments` are layered on top.
         (cond->
           (merge (vis/form<-wire env)
                  {:position idx
                   :code src
                   ;; `:scope` ("tN/iM/fK") preserves the per-form
                   ;; provenance string the engine stamped at write time.
                   :scope (get env "scope")
                   :tag (get env "tag")})
           (contains? env "result")
           (assoc :result (get env "result"))

           (contains? env "error")
           (assoc :error (get env "error"))

           (seq segments)
           (assoc :render-segments segments)

           (some? (get env "duration_ms"))
           (assoc :duration-ms (get env "duration_ms")))))

     all-blocks
     (if (seq envelopes) (vec (map-indexed envelope->block envelopes)) fallback-blocks)

     answer-here?
     (and produced-answer? (= (get it "id") last-iteration-id) (seq all-blocks))

     preflight-source?
     (fn [b]
       (let
         [c (some-> (:code b)
                    str
                    str/triml)]
         (and c (str/starts-with? c "(vis/preflight-error"))))

     preflight-idxs
     (into #{}
           (keep-indexed (fn [i b]
                           (when (or (:preflight? b) (preflight-source? b)) i)))
           all-blocks)

     answer-idx
     (when answer-here?
       (let
         [idx
          (or (get it "answer_position") (dec (count all-blocks)))

          block
          (when (and (integer? idx) (not (neg? (long idx))) (< (long idx) (count all-blocks)))
            (get all-blocks idx))]

         (when (and block
                    (not (visible-code-segments? block))
                    (or (= "vis_answer" (:result block))
                        (str/includes? (str (:code block)) "done(")))
           idx)))

     elide-idxs
     (cond-> preflight-idxs
       (some? answer-idx)
       (conj answer-idx))

     visible
     (into []
           (keep-indexed (fn [idx b]
                           (when-not (contains? elide-idxs idx) b)))
           all-blocks)

     ;; New per-form envelopes carry :duration-ms. Older DB rows do
     ;; not; recover the iteration eval duration when exactly one
     ;; non-answer form remains visible after `(done ...)` elision so
     ;; restored green footers still match live bubbles.
     duration-fallback-idxs
     (when (some? (get it "duration_ms"))
       (into []
             (keep-indexed (fn [idx b]
                             (let
                               [src (some-> (:code b)
                                            str
                                            str/triml)]
                               (when (and (nil? (:duration-ms b))
                                          (not= "vis_answer" (:result b))
                                          (not (str/starts-with? (or src "") "done(")))
                                 idx))))
             visible))

     visible
     (if (= 1 (count duration-fallback-idxs))
       (update visible (first duration-fallback-idxs) assoc :duration-ms (get it "duration_ms"))
       visible)

     ;; One restored block PER persisted form envelope - parity with the
     ;; live tracker: the loop emits one :form-start/:form-result chunk per
     ;; top-level form, so live bubbles render one card per form, each with
     ;; its OWN result. An earlier regroup collapsed every envelope into a
     ;; single merged card that kept only the LAST form's :result - resumed
     ;; or switched-to sessions showed the code but lost every intermediate
     ;; form result. Keep the per-envelope blocks instead.
     ;; `:forms` are the PERSISTED proof envelopes (model-facing, one per
     ;; top-level form). `iteration/canonicalize` derives the DISPLAY-state
     ;; `:ops` from each envelope's `:channel` sink slice — `:ops` is what
     ;; the renderer paints as block op rows; `:forms` stays proof-granular.
     forms
     (mapv block->form-record visible)]

    (iteration/canonicalize {:position (when-let [p (get it "position")]
                                         (dec (long p)))
                             :thinking (visible-thinking (get it "thinking"))
                             :provider-fallbacks (get it "llm_routing_trace")
                             :forms forms})))

(defn user-message
  "Create a canonical user message."
  ([text] (user-message text (java.util.Date.)))
  ([text timestamp]
   (let [markdown (or text "")]
     {:role :user
      :content [{"id" (str (java.util.UUID/randomUUID)) "type" "prose" "markdown" markdown}]
      :text markdown
      :timestamp timestamp})))

(defn error-content
  "Canonical blocks for a failed gateway result."
  [result]
  (let [blocks (vec (or (get result "content") []))]
    (if (seq blocks)
      blocks
      [{"id" (str (java.util.UUID/randomUUID))
        "type" "error"
        "code" "turn_failed"
        "message" (vis/format-error (get result "error"))
        "retryable" false}])))

(defn content->markdown
  "Disposable Markdown projection of canonical content blocks."
  [blocks]
  (->> blocks
       (keep (fn [block]
               (case (get block "type")
                 "prose"
                 (get block "markdown")

                 "code"
                 (str "```" (or (get block "language") "") "\n" (get block "text" "") "\n```")

                 "reasoning"
                 (get block "text")

                 ("error" "notice")
                 (get block "message")

                 "tool"
                 (some-> (get block "output")
                         str)

                 nil)))
       (str/join "\n\n")))

(defn render-answer
  "Project canonical content blocks to Markdown for clipboard/export callers."
  [blocks]
  (content->markdown blocks))

(defn assistant-message
  "Create a canonical assistant message from typed content blocks."
  ([blocks] (assistant-message blocks (java.util.Date.)))
  ([blocks timestamp]
   (let [blocks (vec (or blocks []))]
     {:role :assistant :content blocks :text (content->markdown blocks) :timestamp timestamp})))

(defn- rebuild-history
  "Reconstruct message history from DB for a session.
   Returns a vec of {:role :user|:assistant :text str :timestamp #inst ...}.
   Assistant messages include the code execution trace from all iterations."
  [session-id]
  (try
    (let
      [turns (->> (vis/gateway-transcript session-id)
                  ;; Drop the turn currently IN FLIGHT — the caller ATTACHES to it
                  ;; and streams it live (see resume-session / :attach-running-turn),
                  ;; so rebuilding its half-written `:running` row here would double
                  ;; the bubble (once frozen from the DB, once live).
                  (remove #(= "running" (str (get % "status")))))]
      (into
        []
        (mapcat
          (fn [q]
            (let
              [user-message (assoc (user-message (or (get q "user_request") "")
                                                 (or (some-> (get q "created_at")
                                                             long
                                                             ((fn [^long ms]
                                                                (java.util.Date. ms))))
                                                     (java.util.Date.)))
                              ;; Reloaded/persisted turns carry no :client-turn-id, so without this
                              ;; `turn-identity` returns nil and every `[Pasted #N]` disclosure collapses
                              ;; to the same unscoped node-id (`user:paste:dN`) — clicking one paste would
                              ;; toggle every paste in the session. Stamp the gateway turn id (same one the
                              ;; assistant message carries) so reloaded paste ids are turn-scoped, exactly
                              ;; like the live send path scopes them by :client-turn-id.
                              :session-turn-id (get q "id"))
               ;; `:prior-outcome :cancelled` is how the
               ;; persistance layer marks an aborted turn (the
               ;; sweep + cancel paths both write that value).
               cancelled? (= "cancelled" (str (get q "prior_outcome")))
               ;; Persisted history carries the same canonical content blocks as live turns.
               content-blocks (let [stored (vec (or (get q "content") []))]
                                (if (and cancelled? (empty? stored))
                                  [{"id" (str (get q "id") ":cancelled")
                                    "type" "notice"
                                    "code" "turn_cancelled"
                                    "message" "Cancelled by user."}]
                                  stored))
               model (get q "model")
               ;; Canonical string-keyed token shape, passed through to the
               ;; per-bubble render contract. "input" is TOTAL; the detail
               ;; keys are subsets. Absent keys stay absent.
               tokens (cond-> {}
                        (get q "input_tokens")
                        (assoc "input" (get q "input_tokens"))

                        (get q "input_regular_tokens")
                        (assoc "input_regular" (get q "input_regular_tokens"))

                        (get q "input_cache_read_tokens")
                        (assoc "cached" (get q "input_cache_read_tokens"))

                        (get q "input_cache_write_tokens")
                        (assoc "cache_created" (get q "input_cache_write_tokens"))

                        (get q "output_tokens")
                        (assoc "output" (get q "output_tokens"))

                        (get q "output_reasoning_tokens")
                        (assoc "reasoning" (get q "output_reasoning_tokens")))
               iteration-count (get q "iteration_count")
               duration-ms (get q "duration_ms")
               cost (when-let [total-cost (or (get q "total_cost") (get q "cost"))]
                      (cond-> {"total_cost" total-cost}
                        (get q "provider")
                        (assoc "provider" (get q "provider"))

                        (get q "model")
                        (assoc "model" (get q "model"))))
               ;; Rebuild trace from gateway transcript iterations + blocks.
               ;; The answer-bearing form (last expression of
               ;; the answer iteration, per rule b') is
               ;; ELIDED from the per-iteration parallel
               ;; vectors so resumed sessions render the
               ;; same way live ones do - just the answer
               ;; text below the iteration trace, never the
               ;; `(done "...")` call as code above it.
               turn-iterations (vec (get q "iterations"))
               last-it (last turn-iterations)
               last-iteration-id (get last-it "id")
               llm-routing (cond-> {}
                             (get last-it "llm_selected")
                             (assoc :selected (get last-it "llm_selected"))

                             (get last-it "llm_actual")
                             (assoc :actual (get last-it "llm_actual"))

                             (contains? last-it "is_llm_fallback")
                             (assoc :fallback? (get last-it "is_llm_fallback"))

                             (seq (get last-it "llm_routing_trace"))
                             (assoc :trace (get last-it "llm_routing_trace")))
               produced-answer? (seq content-blocks)
               ;; A slash turn persists ONE synthetic iteration whose
               ;; envelope is tagged `:user-slash`. Live slash turns
               ;; stream no iterations, so they render as a bare answer
               ;; bubble — rebuilding that synthetic iteration into a
               ;; trace made resumed sessions look different from live.
               ;; Suppress it so resume matches live: just the answer.
               slash-turn? (some (fn [it]
                                   (some #(#{"user-slash" "user-shell"} (str (get % "tag")))
                                         (get it "forms")))
                                 turn-iterations)
               trace (if slash-turn?
                       []
                       (into []
                             (map (partial it->iteration-entry
                                           {:produced-answer? produced-answer?
                                            :last-iteration-id last-iteration-id}))
                             turn-iterations))
               assistant-message (cond->
                                   (assistant-message content-blocks
                                                      (or (some-> (get q "created_at")
                                                                  long
                                                                  ((fn [^long ms]
                                                                     (java.util.Date. ms))))
                                                          (java.util.Date.)))
                                   true
                                   (assoc :session-turn-id (get q "id"))

                                   (seq trace)
                                   (assoc :traces trace)

                                   model
                                   (assoc :model model)

                                   (:selected llm-routing)
                                   (assoc :llm-selected (:selected llm-routing))

                                   (:actual llm-routing)
                                   (assoc :llm-actual (:actual llm-routing))

                                   (contains? llm-routing :fallback?)
                                   (assoc :llm-fallback? (:fallback? llm-routing))

                                   (seq (:trace llm-routing))
                                   (assoc :llm-routing-trace (:trace llm-routing))

                                   iteration-count
                                   (assoc :iteration-count iteration-count)

                                   duration-ms
                                   (assoc :duration-ms duration-ms)

                                   cost
                                   (assoc :cost cost)

                                   (seq tokens)
                                   (assoc :tokens tokens)

                                   cancelled?
                                   (assoc :status :cancelled)

                                   slash-turn?
                                   (assoc :slash? true))]

              [user-message assistant-message])))
        turns))
    (catch Exception e
      (t/log! {:level :warn
               :id ::rebuild-history-failed
               :data (exception->log-data e)
               :msg (str "Failed to rebuild history: " (ex-message e))})
      [])))


(defn- event-get
  "Read field `k` off a canonical string-keyed wire map — ONE deterministic
   lookup: the exact spelling `vis/wire-key` emits (snake_case, `foo?` ->
   `is_foo`), the same policy the wire encoder owns."
  [m k]
  (get m (vis/wire-key k)))

(defn- wire-keyword
  "Rehydrate a keyword-valued wire field without guessing map-key policy."
  [x]
  (cond (keyword? x) x
        (and (string? x) (not (str/blank? x))) (keyword x)
        :else nil))

(defn- wire-error-map
  "Rehydrate the concise structured error shape emitted by the gateway."
  [raw]
  (when (map? raw)
    (cond-> {}
      (event-get raw :type)
      (assoc :type (wire-keyword (event-get raw :type)))

      (event-get raw :message)
      (assoc :message (event-get raw :message))

      (some? (event-get raw :status))
      (assoc :status (event-get raw :status))

      (event-get raw :cause-class)
      (assoc :cause-class (event-get raw :cause-class)))))

(defn- provider-retry-event->chunk
  "Project a canonical `provider.retry` gateway event into the progress reset
   chunk consumed by every TUI path (fresh turn, attach, and reconnect)."
  [event]
  (let
    [raw-error
     (event-get event :error)

     raw-route
     (event-get event :event)

     attempt
     (event-get event :attempt)

     max-retries
     (event-get event :max-retries)

     delay-ms
     (event-get event :delay-ms)

     reason
     (or (some-> raw-route
                 (event-get :reason)
                 wire-keyword)
         (some-> raw-error
                 (event-get :type)
                 wire-keyword)
         :provider-retry)

     error-type
     (or (some-> raw-error
                 (event-get :type)
                 wire-keyword)
         reason)

     error-message
     (when (map? raw-error) (event-get raw-error :message))

     error
     (cond-> {:type error-type}
       error-message
       (assoc :message error-message)

       (some? attempt)
       (assoc :attempt attempt)

       (some? max-retries)
       (assoc :max-retries max-retries)

       (and (number? delay-ms) (pos? (long delay-ms)))
       (assoc :delay-ms delay-ms))

     route-event
     (cond-> {:event/type :llm.routing/provider-retry :reason reason}
       (some? attempt)
       (assoc :attempt attempt)

       (some? delay-ms)
       (assoc :delay-ms delay-ms)

       (map? raw-route)
       (merge (cond-> {}
                (event-get raw-route :provider)
                (assoc :provider (event-get raw-route :provider))

                (event-get raw-route :model)
                (assoc :model (event-get raw-route :model)))))]

    {:phase :provider-retry-reset
     :iteration (event-get event :iteration)
     :attempt attempt
     :max-retries max-retries
     :delay-ms delay-ms
     :error error
     :event route-event}))

(defn- gateway-event->chunk
  "Project canonical gateway wire events back into the progress chunk shape the
  existing TUI renderer consumes. This is intentionally a client projection: the
  gateway remains the only producer of live turn events. Preserve gateway op
  summaries/displays as synthetic channel sink entries so live TUI renders the
  same LABEL rows and expandable tool cards that restored sessions render."
  [event]
  (let
    [type
     (event-get event :type)

     iteration
     (event-get event :iteration)

     block-id
     (event-get event :block-id)

     code
     (event-get event :code)

     stdout
     (event-get event :stdout)

     _result
     (event-get event :result)

     error
     (event-get event :error)

     silent
     (event-get event :silent)

     done
     (event-get event :done)

     thinking
     (event-get event :thinking)

     text
     (event-get event :text)]

    (case type
      ;; Canonical typed-block delta projected into the TUI's transient progress shape.
      ;; `text` is the INCREMENT since the gateway's last emit; the tracker's
      ;; timeline REPLACES per-iteration text (in-process chunks are cumulative),
      ;; so prefer the bounded `cumulative` the frame also carries — falling back
      ;; to the increment for older gateways that omit it.
      "content.block.delta"
      (let
        [full (let [c (str (event-get event :cumulative))]
                (when-not (str/blank? c) c))]
        (cond (= "text" (event-get event :field))
              {:phase :reasoning :iteration iteration :thinking (or full text)}
              (str/includes? (str block-id) ":assistant-prose:")
              {:phase :assistant-prose :iteration iteration :text (or full text)}
              :else {:phase :content :iteration iteration :content (or full text)}))

      "content.block.started"
      nil

      "content.block.completed"
      nil

      "block.preview"
      (merge {:phase :tool-preview :iteration iteration :position block-id :code code}
             (vis/form<-wire event))

      "block.started"
      (merge {:phase :form-start :iteration iteration :position block-id :code code}
             ;; Read back the native-tool badge identity (`:vis/tool-name`
             ;; + colour) so the live bubble can hide the redundant
             ;; invocation code WHILE the tool runs, not just after.
             (vis/form<-wire event))

      "block.output"
      (merge {:phase :form-result
              :iteration iteration
              :position block-id
              ;; Gateway-computed / renamed fields the wire owns:
              ;; bounded stdout + error, the derived silent flag.
              :stdout stdout
              :error error
              :silent? (boolean silent)}
             (when (event-get event :duration-ms) {:duration-ms (event-get event :duration-ms)})
             ;; The WHOLE canonical display set (code, result, the
             ;; native-tool op-card card/label/colour, render-segments,
             ;; …) read back tolerantly — the mirror of the gateway's
             ;; `->display`, so a new display field flows live with no
             ;; edit here. `<-wire` re-keywords the keyword-valued fields
             ;; (`:tool-color-role`) the wire stringified.
             (vis/form<-wire event))

      "iteration.completed"
      {:phase :iteration-final
       :iteration iteration
       :thinking thinking
       :assistant-prose (event-get event :assistant-prose)
       :done? (boolean done)}

      "iteration.error"
      {:phase :iteration-error
       :iteration iteration
       :thinking thinking
       :error (or (wire-error-map (event-get event :error-data)) error)}

      "provider.retry"
      (provider-retry-event->chunk event)

      ;; Coarse live-progress ticker (provider wait, response parse, nested
      ;; shell/tool call). Project back to the phase the spinner reads so an
      ;; ATTACHED tab shows "Vis is running: …" like a locally-run turn.
      "activity"
      (let
        [activity
         (event-get event :activity)

         cmd
         (event-get event :cmd)

         op
         (event-get event :op)]

        (case (str activity)
          "provider-call"
          {:phase :provider-call :iteration iteration}

          "response-parse"
          {:phase :response-parse :iteration iteration}

          "shell-run"
          {:phase :shell-run :iteration iteration :cmd (or cmd op)}

          "shell-bg"
          {:phase :shell-bg :iteration iteration :cmd (or cmd op)}

          "tool"
          {:phase :tool-start :iteration iteration :tool-event {:op op}}

          nil))

      ;; Queue lifecycle for a DIFFERENT (queued) turn on this session — the
      ;; gateway sync/attach subscriptions forward these so every attached TUI
      ;; mirrors the queued backlog live (see state/:sync-queued-turn).
      "turn.queued"
      {:phase :queue-sync
       :op :add
       :turn-id (event-get event :turn-id)
       :text (event-get event :request)}

      "turn.queued.updated"
      {:phase :queue-sync
       :op :update
       :turn-id (event-get event :turn-id)
       :text (event-get event :request)}

      "turn.queued.deleted"
      {:phase :queue-sync :op :delete :turn-id (event-get event :turn-id)}

      ;; The queue head left the queue because the gateway auto-STARTED it.
      ;; Drop the mirrored entry — the drain/attach machinery renders the
      ;; turn itself; a replayed log nets to zero (queued … drained).
      "turn.queued.drained"
      {:phase :queue-sync :op :delete :turn-id (event-get event :turn-id)}

      ;; The turn actually STARTED running. Carries the gateway's canonical
      ;; `started_at` (epoch ms) — the ONE clock every channel shares — so the
      ;; TUI re-seeds its elapsed timer from it (see state/:sync-turn-clock):
      ;; local submit/drain/attach stamps drift from the real run start.
      "turn.started"
      {:phase :turn-start
       :turn-id (event-get event :turn-id)
       ;; The request text rides along so an IDLE tab can attach to a
       ;; sibling-started turn (state/:sibling-turn-started) with the real
       ;; user bubble, not a blank one.
       :request (event-get event :request)
       :started-at-ms (event-get event :started-at)}

      ;; A session's title changed — auto-title or a rename, possibly produced
      ;; in a SIBLING process (another TUI, the web, the serve daemon), where
      ;; THIS process's in-process titling listeners never fire. Project it so
      ;; the persistent tab subscription relabels live instead of waiting for a
      ;; tab reopen to re-read the DB title. `:session_id` is the TITLED
      ;; session's id (a foreign copy stamps it in the payload — see gateway
      ;; state/broadcast-title-event!), which may differ from the subscribed
      ;; session.
      "session.title_updated"
      {:phase :title-sync
       :session-id (some-> (event-get event :session-id)
                           str)
       :title (event-get event :title)}

      nil)))

(defn subscribe-session-events!
  "PERSISTENT live event subscription for one open session tab.

   The blocking submit/attach SSE loops exist only WHILE a turn runs in this
   process, so an idle tab is deaf: a sibling TUI/web queueing, editing or
   deleting a message — or starting a whole turn — never reached it (the
   queue-desync + frozen-idle-tab bugs). This subscribes to the session's
   live event stream (from the CURRENT cursor — no replay churn) for the
   tab's whole lifetime and forwards every projectable event as a chunk to
   `on-chunk` (`:queue-sync`, `:turn-start`, …). The caller filters phases
   and dispatches; the TUI handlers are idempotent, so overlap with an
   active attach subscription is harmless. Returns a zero-arg cleanup fn."
  [session-id on-chunk]
  (let
    [sid
     (str session-id)

     cursor
     (try (vis/gateway-current-seq sid) (catch Throwable _ 0))

     ;; ONE process-wide multiplexed SSE connection carries every open tab's
     ;; session (demuxed by :session_id), so N tabs = 1 socket + 1 server
     ;; heartbeat thread, not N. Returns this tab's cleanup fn.
     cleanup
     (vis/gateway-mux-subscribe! sid
                                 (fn [event]
                                   (try (when-let [chunk (gateway-event->chunk event)]
                                          (on-chunk chunk))
                                        (catch Throwable _ nil)))
                                 cursor)]

    (fn []
      (try (cleanup) (catch Throwable _ nil)))))

(defn- create-session*
  [_provider-config {:keys [workspace-id root]}]
  (let
    [resp (vis/gateway-create-session! (cond-> {:channel :tui}
                                         workspace-id
                                         (assoc :workspace-id workspace-id)

                                         root
                                         (assoc :root root)))]
    {:id (java.util.UUID/fromString (get resp "id")) :history []}))



(defn make-session
  "Create a fresh `:tui` session through the gateway-owned warm pool."
  ([_provider-config] (make-session _provider-config nil))
  ([provider-config opts] (create-session* provider-config opts)))

(defn make-session-async
  "Create a session off the input thread. The gateway normally answers from its
   warm pool; a pool miss still never blocks TUI input."
  ([provider-config] (make-session-async provider-config nil))
  ([provider-config opts]
   {:building (vis/worker-future "tui-session-build" #(create-session* provider-config opts))}))

(defn- resolve-resume-id
  "Resolve a resume id to a full `java.util.UUID`, or nil. Accepts a
   full UUID or an unambiguous prefix among gateway sessions (any channel)."
  [session-id]
  (let
    [cid (some-> session-id
                 str
                 str/trim)]
    (when (seq cid)
      (or (when-let [session (vis/gateway-soul cid)]
            (java.util.UUID/fromString (get session "id")))
          (let
            [matches (->> (vis/gateway-list-sessions :all)
                          (map #(get % "id"))
                          (filter #(str/starts-with? (str %) cid))
                          vec)]
            (when (= 1 (count matches)) (java.util.UUID/fromString (first matches))))))))

(defn resume-session
  "Resume an existing gateway-managed session by id — ANY channel, so a
   conversation started in the CLI or elsewhere resumes here too.
   Accepts full UUID or unambiguous short UUID prefix.
   Returns `{:id UUID :history [...]}` with persisted messages. When a turn is
   IN FLIGHT (started here, in the web, or a sibling process) the map ALSO
   carries `:status`, `:current-turn-id`, the running turn's
   `:running-request` text and its canonical gateway `:running-started-at`
   (epoch ms — the ONE clock every attaching channel shares, so two TUIs
   show the SAME elapsed time), so the caller can ATTACH and stream it live (see
   state/:attach-running-turn) instead of showing frozen history. Any queued
   backlog rides along as `:queued-turns`
   `[{:turn-id .. :text .. :queued-at-ms ..}]` (oldest first) so the tab
   mirrors the gateway's server-side queue on open."
  [session-id]
  (when-let [resolved-id (resolve-resume-id session-id)]
    (when-let [soul (vis/gateway-soul resolved-id)]
      (let
        [tid (get soul "current_turn_id")
         turns (try (vis/gateway-list-turns resolved-id) (catch Throwable _ nil))
         running-turn (when tid
                        (some (fn [t]
                                (when (and (= "running" (str (get t "status")))
                                           (= (str tid) (str (get t "turn_id"))))
                                  t))
                              turns))
         queued-turns (->> turns
                           (filter (fn [t]
                                     (= "queued" (str (get t "status")))))
                           (sort-by (fn [t]
                                      (or (get t "queued_at") 0)))
                           (mapv (fn [t]
                                   {:turn-id (get t "turn_id")
                                    :text (get t "request")
                                    :queued-at-ms (get t "queued_at")})))]

        (cond-> {:id resolved-id :history (rebuild-history resolved-id) :status (get soul "status")}
          tid
          (assoc :current-turn-id tid)

          (some? (get running-turn "request"))
          (assoc :running-request (get running-turn "request"))

          (nat-int? (get running-turn "started_at"))
          (assoc :running-started-at (get running-turn "started_at"))

          (seq queued-turns)
          (assoc :queued-turns queued-turns))))))

(defn turn!
  "Submit a user request through the canonical in-process gateway. Blocking."
  ([session text] (turn! session text {}))
  ([{:keys [id]} text
    {:keys [on-chunk cancel-token reasoning-default extra-body turn-features workspace]}]
   (try (vis/gateway-submit-turn-sync! id
                                       (cond->
                                         {:request text
                                          :on-event (fn [event]
                                                      (when-let [chunk (gateway-event->chunk event)]
                                                        (when on-chunk (on-chunk chunk))))}
                                         cancel-token
                                         (assoc :cancel-token cancel-token)

                                         reasoning-default
                                         (assoc :reasoning-default reasoning-default)

                                         extra-body
                                         (assoc :extra-body extra-body)

                                         turn-features
                                         (assoc :turn-features turn-features)

                                         (seq workspace)
                                         (assoc :workspace workspace)))
        (catch Exception e
          (if (vis/cancellation? e)
            {"content" [{"id" (str (java.util.UUID/randomUUID))
                         "type" "notice"
                         "code" "turn_cancelled"
                         "message" "Cancelled by user."}]
             "status" "cancelled"}
            {"error" (or (ex-message e) (str e))})))))

(defn attach!
  "Attach to an already submitted gateway turn and return canonical content."
  ([session tid] (attach! session tid {}))
  ([{:keys [id]} tid {:keys [on-chunk]}]
   (try (vis/gateway-attach-turn-sync! id
                                       tid
                                       {:on-event (fn [event]
                                                    (when-let [chunk (gateway-event->chunk event)]
                                                      (when on-chunk (on-chunk chunk))))})
        (catch Exception e
          (if (vis/cancellation? e)
            {"content" [{"id" (str (java.util.UUID/randomUUID))
                         "type" "notice"
                         "code" "turn_cancelled"
                         "message" "Cancelled by user."}]
             "status" "cancelled"}
            {"error" (or (ex-message e) (str e))})))))

(defn dispose!
  "Release the TUI's env handle. Session data stays in
   `~/.vis/vis.mdb` so other consumers of the `:tui` channel
   (e.g. `vis sessions tui`, future inspectors) still see it."
  [{:keys [id]}]
  (when id (vis/gateway-release-session! id)))