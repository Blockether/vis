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
  (let [s (some-> thinking
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
   STRUCTURALLY instead, by the `:success?` flag plus the `:symbol`/`:metadata`
   identity `envelope-of` always stamps. Also true for a raw in-process
   envelope, which carries the same keys."
  [x]
  (and (map? x) (contains? x :success?) (or (contains? x :symbol) (contains? x :metadata))))

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
        (let [metadata (:metadata result)]
          (merge (select-keys result [:symbol :tag])
                 (select-keys metadata
                              [:spec :paths :hit_count :truncated_by :command :cwd :target])))
        (seq channel) (let [first-ok (or (first (filter :success? channel)) (first channel))]
                        (cond-> {}
                          (:symbol first-ok)
                          (assoc :op (:symbol first-ok))

                          (:tag first-ok)
                          (assoc :tag (:tag first-ok))))))

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
  (merge (vis/form<-wire block)
         {:started-at-ms nil
          :duration-ms (or (:duration-ms block) (:duration_ms block) 0)
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
  (let [iter-code
        (or (:code it) "")

        envelopes
        (vec (or (:forms it) []))

        ;; Sentinel block when the iteration ran no forms (eval timeout,
        ;; pre-eval rejection, etc.). Keep the iteration visible with
        ;; whatever code text we have so the bubble doesn't collapse to
        ;; nothing on restore.
        fallback-blocks
        [(cond-> {:position 0 :code iter-code}
           (some? (:render_segments it))
           (assoc :render-segments (:render_segments it))

           (some? (:duration_ms it))
           (assoc :duration-ms (:duration_ms it))

           (some? (:result it))
           (assoc :result (:result it))

           (some? (:error it))
           (assoc :error (:error it)))]

        envelope->block
        (fn [idx env]
          (let [src
                (or (:src env) (:source env) (:code env) "")

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
            (cond-> (merge (vis/form<-wire env)
                           {:position idx
                            :code src
                            ;; `:scope` ("tN/iM/fK") preserves the per-form
                            ;; provenance string the engine stamped at write time.
                            :scope (:scope env)
                            :tag (:tag env)})
              (contains? env :result)
              (assoc :result (:result env))

              (contains? env :error)
              (assoc :error (:error env))

              (seq segments)
              (assoc :render-segments segments)

              (some? (:duration_ms env))
              (assoc :duration-ms (:duration_ms env)))))

        all-blocks
        (if (seq envelopes) (vec (map-indexed envelope->block envelopes)) fallback-blocks)

        answer-here?
        (and produced-answer? (= (:id it) last-iteration-id) (seq all-blocks))

        preflight-source?
        (fn [b]
          (let [c (some-> (:code b)
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
          (let [idx
                (or (:answer_position it) (dec (count all-blocks)))

                block
                (when (and (integer? idx) (not (neg? idx)) (< idx (count all-blocks)))
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
        (when (some? (:duration_ms it))
          (into []
                (keep-indexed (fn [idx b]
                                (let [src (some-> (:code b)
                                                  str
                                                  str/triml)]
                                  (when (and (nil? (:duration-ms b))
                                             (not= "vis_answer" (:result b))
                                             (not (str/starts-with? (or src "") "done(")))
                                    idx))))
                visible))

        visible
        (if (= 1 (count duration-fallback-idxs))
          (update visible (first duration-fallback-idxs) assoc :duration-ms (:duration_ms it))
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

    (iteration/canonicalize {:position (when-let [p (:position it)]
                                         (dec (long p)))
                             :thinking (visible-thinking (:thinking it))
                             :provider-fallbacks (:llm_routing_trace it)
                             :forms forms})))

(defn user-message
  "Create a structured user message with timestamp.
   The user types raw markdown into the input box; we lift it to
   canonical IR via `vis/markdown->ir` immediately so the bubble layer
   (and every downstream consumer) sees the same shape it sees for
   assistant answers.

   Unlike model answers, user input is line-oriented: every literal
   newline the user typed or pasted is intent (code, line-numbered
   dumps, stack traces). We lift with `{:soft-break :hard}` so bare
   newlines become `[:br]` and the bubble preserves the exact line
   structure instead of collapsing pasted lines into one wrapped wall
   of text (CommonMark prose soft-break semantics).

   The message carries ONLY `:ir` — every consumer that needs a
   string projection computes it on demand (and caches the result
   client-side, e.g. via `virtual.clj` projection) instead of us
   eagerly rendering at construction."
  ([text] (user-message text (java.util.Date.)))
  ([text timestamp]
   {:role :user
    :ir (vis/markdown->ir text {:soft-break :hard})
    ;; Keep the raw text so resume paths (input-history arrow cycling in
    ;; state/:init-session, search, etc.) can recover the exact string
    ;; the user typed without re-rendering IR back to markdown.
    :text (or text "")
    :timestamp timestamp}))

(def empty-ir
  "Canonical empty IR — used as the placeholder when an answer slot is
   absent (e.g. resumed turns whose answer column is NULL because the
   turn never finished). Never feed `nil` or `\"\"` to the TUI render
   chokepoints; lift to `empty-ir` instead."
  [:ir {}])
(defn error-answer-ir
  "Renderable answer-IR for a FAILED turn result. `turn!`/`attach!` fold the
   engine's provider-error IR (`:vis/provider-error` marker) onto `:answer`
   and dissoc `:answer-ir`, so a failed turn's styled card lives on `:answer` —
   NOT `:answer-ir` (that key is gone by the time a channel reads the result).
   Returns that IR when it carries real content; otherwise flattens the
   `:error` string through `format-error` so a bare/generic failure still shows
   a readable bubble instead of a blank one."
  [result]
  (let [ir (or (:answer-ir result) (:answer result))]
    (if (and (vector? ir) (= :ir (first ir)) (seq (nnext ir)))
      ir
      (vis/markdown->ir (vis/format-error (:error result))))))

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
    (throw
      (ex-info
        "chat/render-answer requires canonical [:ir ...] input; build IR upstream, do not pass raw text"
        {:got-type (some-> ir
                           class
                           .getName)
         :got-preview (let [s (pr-str ir)]
                        (subs s 0 (min 200 (count s))))}))
    :else (let [renderer (some-> (vis/channel-by-id :tui)
                                 :channel/messages-renderer-fn)]
            (if renderer (renderer ir) (vis/render ir :markdown)))))

(defn assistant-message
  "Create a structured assistant (vis) message with timestamp.

   Accepts either:
     - a Markdown source string  -> the canonical input on the Markdown
       answer pipeline; IR is derived via `vis/markdown->ir`
     - a canonical `[:ir & nodes]` vector -> IR is used as-is; raw
       Markdown is reconstructed via `vis/render ir :markdown` so the
       message still carries a `:text` field
     - nil -> empty placeholder for unfilled answer slots

   The message carries both:
     :text  raw Markdown source (used by copy/export, FTS, plain views)
     :ir    derived IR (used by the bubble layout walker)"
  ([source] (assistant-message source (java.util.Date.)))
  ([source timestamp]
   (cond (nil? source) {:role :assistant :text "" :ir empty-ir :timestamp timestamp}
         (string? source) {:role :assistant
                           :text source
                           :ir (if (str/blank? source) empty-ir (vis/markdown->ir source))
                           :timestamp timestamp}
         (and (vector? source) (= :ir (first source)))
         {:role :assistant :text (vis/render source :markdown) :ir source :timestamp timestamp}
         :else (throw
                 (ex-info
                   "chat/assistant-message requires Markdown string or canonical [:ir ...] input"
                   {:got-type (some-> source
                                      class
                                      .getName)})))))

(defn- rebuild-history
  "Reconstruct message history from DB for a session.
   Returns a vec of {:role :user|:assistant :text str :timestamp #inst ...}.
   Assistant messages include the code execution trace from all iterations."
  [session-id]
  (try
    (let [turns (->> (vis/gateway-transcript session-id)
                     ;; Drop the turn currently IN FLIGHT — the caller ATTACHES to it
                     ;; and streams it live (see resume-session / :attach-running-turn),
                     ;; so rebuilding its half-written `:running` row here would double
                     ;; the bubble (once frozen from the DB, once live).
                     (remove #(contains? #{:running "running"} (:status %))))]
      (into
        []
        (mapcat
          (fn [q]
            (let [user-message (assoc (user-message (or (:user_request q) "")
                                                    (or (some-> (:created_at q)
                                                                long
                                                                (java.util.Date.))
                                                        (java.util.Date.)))
                                 ;; Reloaded/persisted turns carry no :client-turn-id, so without this
                                 ;; `turn-identity` returns nil and every `[Pasted #N]` disclosure collapses
                                 ;; to the same unscoped node-id (`user:paste:dN`) — clicking one paste would
                                 ;; toggle every paste in the session. Stamp the gateway turn id (same one the
                                 ;; assistant message carries) so reloaded paste ids are turn-scoped, exactly
                                 ;; like the live send path scopes them by :client-turn-id.
                                 :session-turn-id (:id q))
                  ;; `:prior-outcome :cancelled` is how the
                  ;; persistance layer marks an aborted turn (the
                  ;; sweep + cancel paths both write that value).
                  cancelled? (= "cancelled" (str (:prior_outcome q)))
                  ;; Persistence stores the raw Markdown source
                  ;; the model wrote in `(done {:answer ...})`.
                  ;; Channels derive IR via `vis/markdown->ir` at
                  ;; render time; keep the source so copy/export
                  ;; round-trip byte-for-byte. Cancelled turns can
                  ;; persist with no answer when the user aborts
                  ;; before the first iteration; show an explicit
                  ;; status line instead of a blank gray Vis bubble.
                  answer-md (let [stored (or (:answer_markdown q) "")]
                              (if (and cancelled? (str/blank? stored)) "Cancelled by user." stored))
                  answer-ir (if (str/blank? answer-md) empty-ir (vis/markdown->ir answer-md))
                  model (:model q)
                  ;; Phase B: canonical token shape mapped to the per-bubble
                  ;; render contract. `:input` is TOTAL, `:input-regular` /
                  ;; `:cached` (cache-read) / `:cache-created` (cache-write)
                  ;; are subsets. Render layer formats whichever it needs;
                  ;; absent keys stay absent.
                  tokens (cond-> {}
                           (:input_tokens q)
                           (assoc :input (:input_tokens q))

                           (:input_regular_tokens q)
                           (assoc :input-regular (:input_regular_tokens q))

                           (:input_cache_read_tokens q)
                           (assoc :cached (:input_cache_read_tokens q))

                           (:input_cache_write_tokens q)
                           (assoc :cache-created (:input_cache_write_tokens q))

                           (:output_tokens q)
                           (assoc :output (:output_tokens q))

                           (:output_reasoning_tokens q)
                           (assoc :reasoning (:output_reasoning_tokens q)))
                  iteration-count (:iteration_count q)
                  duration-ms (:duration_ms q)
                  cost (when-let [total-cost (or (:total_cost q) (:cost q))]
                         (cond-> {:total-cost total-cost}
                           (:provider q)
                           (assoc :provider (:provider q))

                           (:model q)
                           (assoc :model (:model q))))
                  ;; Rebuild trace from gateway transcript iterations + blocks.
                  ;; The answer-bearing form (last expression of
                  ;; the answer iteration, per rule b') is
                  ;; ELIDED from the per-iteration parallel
                  ;; vectors so resumed sessions render the
                  ;; same way live ones do - just the answer
                  ;; text below the iteration trace, never the
                  ;; `(done "...")` call as code above it.
                  turn-iterations (vec (:iterations q))
                  last-it (last turn-iterations)
                  last-iteration-id (:id last-it)
                  llm-routing (cond-> {}
                                (:llm_selected last-it)
                                (assoc :selected (:llm_selected last-it))

                                (:llm_actual last-it)
                                (assoc :actual (:llm_actual last-it))

                                (contains? last-it :llm_fallback?)
                                (assoc :fallback? (:llm_fallback? last-it))

                                (seq (:llm_routing_trace last-it))
                                (assoc :trace (:llm_routing_trace last-it)))
                  ;; A non-blank Markdown source signals a real
                  ;; final answer; empty / nil-only turns keep
                  ;; their assistant message but elide the
                  ;; answer-bearing form differently.
                  produced-answer? (not (str/blank? answer-md))
                  ;; A slash turn persists ONE synthetic iteration whose
                  ;; envelope is tagged `:user-slash`. Live slash turns
                  ;; stream no iterations, so they render as a bare answer
                  ;; bubble — rebuilding that synthetic iteration into a
                  ;; trace made resumed sessions look different from live.
                  ;; Suppress it so resume matches live: just the answer.
                  slash-turn? (some (fn [it]
                                      (some #(#{"user-slash" "user-shell"} (str (:tag %)))
                                            (:forms it)))
                                    turn-iterations)
                  trace (if slash-turn?
                          []
                          (into []
                                (map (partial it->iteration-entry
                                              {:produced-answer? produced-answer?
                                               :last-iteration-id last-iteration-id}))
                                turn-iterations))
                  assistant-message (cond-> (assistant-message answer-ir
                                                               (or (some-> (:created_at q)
                                                                           long
                                                                           (java.util.Date.))
                                                                   (java.util.Date.)))
                                      true
                                      (assoc :session-turn-id (:id q))

                                      (seq trace)
                                      (assoc :traces
                                        trace :ir
                                        answer-ir)

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

(defonce ^:private prewarmed-sessions (atom []))

(defonce ^:private prewarm-in-flight (atom 0))

(defonce ^:private prewarm-futures (atom #{}))

(def ^:private prewarm-pool-depth
  "How many empty `:tui` sessions to keep warm ahead of demand. Deep enough
   that a couple of quick consecutive new-tabs still land on a warm session
   while the pool refills; shallow enough that the shutdown hook only has a
   couple of unused sessions to reap."
  2)

(defn- event-get
  [m k]
  (or (get m k)
      (get m (keyword (str/replace (name k) "-" "_")))
      (get m (keyword (str/replace (name k) "_" "-")))
      (get m (name k))
      (get m (str/replace (name k) "-" "_"))
      (get m (str/replace (name k) "_" "-"))))

(defn- gateway-event->chunk
  "Project canonical gateway wire events back into the progress chunk shape the
  existing TUI renderer consumes. This is intentionally a client projection: the
  gateway remains the only producer of live turn events. Preserve gateway op
  summaries/displays as synthetic channel sink entries so live TUI renders the
  same LABEL rows and expandable tool cards that restored sessions render."
  [event]
  (let [type
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
      ;; Older gateways may still send these partial model-text frames; accept
      ;; them for backward compatibility. Current gateway builds hold reasoning
      ;; and prose until iteration.completed.
      "reasoning.delta"
      {:phase :reasoning :iteration iteration :thinking text}

      "content.delta"
      (if (event-get event :prose-final)
        {:phase :assistant-prose :iteration iteration :text text}
        {:phase :content :iteration iteration :content text})

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
      {:phase :iteration-error :iteration iteration :thinking thinking :error error}

      ;; Coarse live-progress ticker (provider wait, response parse, nested
      ;; shell/tool call). Project back to the phase the spinner reads so an
      ;; ATTACHED tab shows "Vis is running: …" like a locally-run turn.
      "activity"
      (let [activity
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
  (let [sid
        (str session-id)

        sub-id
        (str "tui-events-" (java.util.UUID/randomUUID))

        cursor
        (try (vis/gateway-current-seq sid) (catch Throwable _ 0))]

    (vis/gateway-subscribe! sid
                            sub-id
                            (fn [event]
                              (try (when-let [chunk (gateway-event->chunk event)]
                                     (on-chunk chunk))
                                   (catch Throwable _ nil)))
                            cursor)
    (fn []
      (try (vis/gateway-unsubscribe! sid sub-id) (catch Throwable _ nil)))))

(defn- create-session*
  [_provider-config {:keys [workspace-id root prewarm?]}]
  (let [{:keys [id]} (vis/gateway-create-session! (cond-> {:channel :tui}
                                                    workspace-id
                                                    (assoc :workspace-id workspace-id)

                                                    root
                                                    (assoc :root root)

                                                    ;; Warm-pool builds are UNCLAIMED: they must
                                                    ;; not surface in the cross-channel session
                                                    ;; list until a tab actually uses one (the
                                                    ;; first turn claims the soul). Real new-tab
                                                    ;; builds (pool miss) omit this and are claimed.
                                                    prewarm?
                                                    (assoc :prewarm true)))]
    {:id (java.util.UUID/fromString id) :history []}))

(defn- pop-prewarmed!
  "Atomically take one warm session from the pool (FIFO), or nil when empty."
  []
  (let [[old _] (swap-vals! prewarmed-sessions
                            (fn [q]
                              (if (seq q) (subvec q 1) q)))]
    (first old)))

(defn- reserve-prewarm-slot!
  "Atomically claim an in-flight warmup slot iff the pool (already-warm +
   building) is below `prewarm-pool-depth`. Returns true when a slot was
   claimed, so the caller must fire exactly one warmup."
  []
  (let [ready
        (count @prewarmed-sessions)

        [old new]
        (swap-vals! prewarm-in-flight
                    (fn [n]
                      (if (< (+ ready n) prewarm-pool-depth) (inc n) n)))]

    (not= old new)))

(defn- kick-prewarm!
  "Fire ONE background warmup on a worker. Tracks the future so shutdown can
   cancel it, joins the built session to the pool on success, and always
   decrements the in-flight count when it settles. Assumes the caller already
   reserved a slot via `reserve-prewarm-slot!`."
  [provider-config]
  (let [self
        (promise)

        fut
        (vis/worker-future
          "tui-session-prewarm"
          (fn []
            (try (let [session (create-session* provider-config {:prewarm? true})]
                   (swap! prewarmed-sessions conj session))
                 (catch Throwable e
                   (t/log! {:level :warn
                            :id ::prewarm-session-failed
                            :data (exception->log-data e)
                            :msg (str "Failed to prewarm TUI session: " (ex-message e))}))
                 (finally (swap! prewarm-in-flight dec) (swap! prewarm-futures disj @self)))))]

    (swap! prewarm-futures conj fut)
    (deliver self fut)
    fut))

(defn prewarm-session!
  "Keep a small pool of empty `:tui` sessions warm on background workers.

  The gateway facade intentionally couples the cheap session row with the
  expensive environment/runtime construction, so the practical prewarm unit is
  a real empty `:tui` session. `make-session` / `make-session-async` consume
  them atomically; shutdown deletes any left over.

  Idempotent and safe to call from any thread: tops the pool up toward
  `prewarm-pool-depth`, counting both already-warm sessions and in-flight
  warmups, so racing callers never over-build."
  [provider-config]
  (loop []

    (when (reserve-prewarm-slot!) (kick-prewarm! provider-config) (recur))))

(defn discard-prewarmed-session!
  "Delete every warmed-but-unused TUI session and stop any in-flight warmups."
  []
  (doseq [fut (first (reset-vals! prewarm-futures #{}))]
    (try (future-cancel fut) (catch Throwable _)))
  (reset! prewarm-in-flight 0)
  (doseq [{:keys [id]} (first (reset-vals! prewarmed-sessions []))]
    (try (vis/gateway-close-session! id)
         (catch Throwable e
           (t/log! {:level :warn
                    :id ::discard-prewarmed-session-failed
                    :data (exception->log-data e)
                    :msg (str "Failed to delete prewarmed TUI session: " (ex-message e))})))))

(defn make-session
  "Create a fresh `:tui` session. BLOCKING on a pool miss (builds inline).

  Prefer `make-session-async` on the input thread; use this only where the
  caller genuinely needs the session id synchronously.

   Optional opts map (second arity):
     :workspace-id  pre-spawned workspace to pin the new session to.
                    Omit and a trunk workspace is auto-minted by
                    `create-environment`.

   Returns `{:id session-id :history []}`."
  ([_provider-config] (make-session _provider-config nil))
  ([provider-config opts]
   (if-let [session (when (nil? (:workspace-id opts)) (pop-prewarmed!))]
     (do (prewarm-session! provider-config) session)
     (let [session (create-session* provider-config opts)]
       (when (nil? (:workspace-id opts)) (prewarm-session! provider-config))
       session))))

(defn make-session-async
  "Non-blocking `make-session` for the input thread. Never runs the cold
  env/runtime build on the caller's thread.

  Returns one of:
    {:session {:id … :history []}}  a warm session was ready — open it now.
    {:building fut}                 nothing warm — `fut` (a worker Future)
                                    resolves to `{:id … :history []}` once the
                                    cold build finishes. The caller opens an
                                    optimistic placeholder tab now and binds the
                                    real session when `fut` lands.

  `:workspace-id` opts always build off-thread (never pooled), so they take the
  `:building` branch too."
  ([provider-config] (make-session-async provider-config nil))
  ([provider-config opts]
   (if-let [session (when (nil? (:workspace-id opts)) (pop-prewarmed!))]
     (do (prewarm-session! provider-config) {:session session})
     {:building (vis/worker-future "tui-session-build"
                                   (fn []
                                     (let [session (create-session* provider-config opts)]
                                       (when (nil? (:workspace-id opts))
                                         (prewarm-session! provider-config))
                                       session)))})))

(defn- resolve-resume-id
  "Resolve a resume id to a full `java.util.UUID`, or nil. Accepts a
   full UUID or an unambiguous prefix among gateway sessions (any channel)."
  [session-id]
  (let [cid (some-> session-id
                    str
                    str/trim)]
    (when (seq cid)
      (or (when-let [session (vis/gateway-soul cid)]
            (java.util.UUID/fromString (:id session)))
          (let [matches (->> (vis/gateway-list-sessions :all)
                             (map :id)
                             (filter #(str/starts-with? (str %) cid))
                             vec)]
            (when (= 1 (count matches)) (java.util.UUID/fromString (first matches))))))))

(defn resume-session
  "Resume an existing gateway-managed session by id — ANY channel, so a
   conversation started in the web (or CLI) resumes here too.
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
      (let [tid (:current_turn_id soul)
            turns (try (vis/gateway-list-turns resolved-id) (catch Throwable _ nil))
            running-turn (when tid
                           (some (fn [t]
                                   (when (and (= "running" (str (:status t)))
                                              (= (str tid) (str (:turn_id t))))
                                     t))
                                 turns))
            queued-turns
            (->> turns
                 (filter (fn [t]
                           (= "queued" (str (:status t)))))
                 (sort-by (fn [t]
                            (or (:queued_at t) 0)))
                 (mapv (fn [t]
                         {:turn-id (:turn_id t) :text (:request t) :queued-at-ms (:queued_at t)})))]

        (cond-> {:id resolved-id :history (rebuild-history resolved-id) :status (:status soul)}
          tid
          (assoc :current-turn-id tid)

          (some? (:request running-turn))
          (assoc :running-request (:request running-turn))

          (nat-int? (:started_at running-turn))
          (assoc :running-started-at (:started_at running-turn))

          (seq queued-turns)
          (assoc :queued-turns queued-turns))))))

(defn turn!
  "Submit a user request through the canonical in-process gateway. Blocking.
  Returns `{:answer [:ir ...]}` or `{:error str}`."
  ([session text] (turn! session text {}))
  ([{:keys [id]} text
    {:keys [on-chunk cancel-token reasoning-default extra-body turn-features workspace]}]
   (try (let [result
              (vis/gateway-submit-turn-sync!
                id
                (cond-> {:request text
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

              answer-ir
              (or (:answer-ir result)
                  (some-> (:answer result)
                          vis/markdown->ir)
                  empty-ir)]

          (cond-> (assoc result :answer answer-ir)
            (:answer-ir result)
            (dissoc :answer-ir)))
        (catch Exception e
          (if (vis/cancellation? e)
            {:answer [:ir {} [:p {} [:span {} "Cancelled by user."]]] :status :cancelled}
            {:error (or (ex-message e) (str e))})))))
(defn attach!
  "Attach to a gateway turn `tid` already queued/running for `session`, blocking
   until it completes. Same result shape as `turn!` — drives TUI rendering for a
   busy-time submission the gateway queued (see gateway/state `attach-turn-sync!`)."
  ([session tid] (attach! session tid {}))
  ([{:keys [id]} tid {:keys [on-chunk]}]
   (try (let [result
              (vis/gateway-attach-turn-sync! id
                                             tid
                                             {:on-event (fn [event]
                                                          (when-let [chunk (gateway-event->chunk
                                                                             event)]
                                                            (when on-chunk (on-chunk chunk))))})

              answer-ir
              (or (:answer-ir result)
                  (some-> (:answer result)
                          vis/markdown->ir)
                  empty-ir)]

          (cond-> (assoc result :answer answer-ir)
            (:answer-ir result)
            (dissoc :answer-ir)))
        (catch Exception e
          (if (vis/cancellation? e)
            {:answer [:ir {} [:p {} [:span {} "Cancelled by user."]]] :status :cancelled}
            {:error (or (ex-message e) (str e))})))))

(defn dispose!
  "Release the TUI's env handle. Session data stays in
   `~/.vis/vis.mdb` so other consumers of the `:tui` channel
   (e.g. `vis sessions tui`, future inspectors) still see it."
  [{:keys [id]}]
  (when id (vis/gateway-release-session! id)))
