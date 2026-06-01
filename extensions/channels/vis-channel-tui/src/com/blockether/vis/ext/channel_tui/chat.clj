(ns com.blockether.vis.ext.channel-tui.chat
  "TUI-side projections over the shared sessions API.

   On startup the TUI creates a fresh `:tui` session by default.
   Pass `--session-id ID` or `--resume` to pick up an existing one.
   Session data is persisted in `~/.vis/vis.mdb` so you can come
   back to it."
  (:require [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.internal.ctx-engine :as ctx-engine]
            [com.blockether.vis.internal.extension :as extension]
            [com.blockether.vis.internal.format :as fmt]
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

;; Engine-form detection delegates to `ctx-engine/engine-form-src?` so
;; we share one edamame-parsed head-symbol predicate with progress.clj.
;; No string-prefix list to keep in sync.

(defn- visible-code-segments?
  "True when an iteration block has at least one `:code` segment that
   should reach the user. Mirrors the live progress predicate."
  [b]
  (boolean (some #(= :code (:kind %)) (:render-segments b))))

(defn- structurally-silent-block?
  "True for host-bookkeeping forms that should never appear in user
   traces: title updates, answer emission, ctx mutators (`task-set!`,
   `spec-set!`, `fact-set!`), and engine-internal probes
   (`introspect-*`)."
  [b]
  (let [code (str (:code b))]
    (boolean
      (or (:vis/structurally-silent? b)
        (and (not (visible-code-segments? b))
          (ctx-engine/engine-form-src? code))
        (and (= :vis/silent (:result b))
          (not (seq (:render-segments b)))
          (ctx-engine/engine-form-src? code))))))

(defn- form-result-kind
  "Mirror of `progress/form-result-kind` for restored sessions: a
   form is `:tool`-kind whenever it touched the tool surface, even
   indirectly via `:channel` sink entries (e.g. `(def r (v/ls …))`
   followed by `(select-keys r …)`). Without this the channel's
   preview pane is hidden because the FENCE's last value
   is plain data."
  [{:keys [result error channel]}]
  (cond
    error                            :error
    (extension/tool-result? result)  :tool
    (seq channel)                    :tool
    :else                            :value))

(defn- form-result-detail
  "Project tool-result envelope to the small detail map the TUI labels
   consume. Returns nil for non-tool results.

   Restored forms persisted via `(def x (tool …))` shape have their
   `:result` deref'd to the inner value before persistence (SCI def
   semantics), so `tool-result?` returns false and the envelope path
   below would never fire. Fall back to the channel slice: every
   sink entry now carries `:symbol` / `:tag` (`write-sink-entries!`
   stamps them from the originating `sym-entry`). Use the FIRST
   successful entry's tag/symbol to label the bubble — same shape
   the live path produces, just sourced from persistance."
  [{:keys [result channel]}]
  (cond
    (extension/tool-result? result)
    (let [metadata (:metadata result)]
      (merge (select-keys result [:symbol :tag])
        (select-keys metadata [:spec :paths :hit-count :truncated-by
                               :command :cwd :target])))

    (seq channel)
    (let [first-ok (or (first (filter :success? channel)) (first channel))]
      (cond-> {}
        (:symbol first-ok) (assoc :op (:symbol first-ok))
        (:tag first-ok)    (assoc :tag (:tag first-ok))))))

(defn- runtime-ref?
  "True when `v` is a legacy persistence sentinel for a runtime-only value.
   Cross-turn `(def ...)` rehydration is gone, but historical session
   rows may still carry `{:vis/ref :expr}` payloads; render them as a
   small static label instead of a misleading value."
  [v]
  (and (map? v) (= :expr (:vis/ref v))))

(defn- form-result-render
  "Render the form's result for the trace bubble. Mirrors the live
   progress `format-form-result` chokepoint. Legacy runtime-ref
   sentinels render as a small label since cross-turn restore is
   no longer supported."
  [{:keys [result error channel]}]
  (cond
    error nil
    (seq channel)
    ;; Per-form sink entries: each carries the `{:summary :display}`
    ;; render contract. Flatten to summary-led IR (badge first, body
    ;; after) on success; on failure flatten the default error contract.
    ;; Sort by `:position` so racy futures land in canonical source order.
    (extension/combine-render-values
      (map (fn [{:keys [success? result error]}]
             (if success?
               (extension/render-fn-result->ir result)
               (extension/render-fn-result->ir
                 (extension/default-error-result
                   {:success? false :result nil :info {} :error error}))))
        (sort-by :position channel)))
    (= :vis/answer result)           nil
    (runtime-ref? result)            "<legacy runtime value; not restorable>"
    (extension/tool-result? result)  (extension/render-fn-result->ir
                                       (extension/render-tool-result result))
    :else                            (fmt/bounded-value-str result)))

(defn- block->form-record
  "Materialize one DB-iteration block into a `:forms` entry. The shape
   matches the live progress tracker's per-form map so the renderer
   uses one code path for live and resumed traces."
  [block]
  {:code            (:code block)
   :comment         (:comment block)
   :render-segments (:render-segments block)
   :scope           (:scope block)
   :tag             (:tag block)
   :started-at-ms   nil
   :duration-ms     (or (:duration-ms block) 0)
   ;; Keep the raw sink slice so the shared `iteration/entry-ops` derives
   ;; the SAME DISPLAY-state ops the live path derives from its `:channel`.
   :channel         (vec (:channel block))
   :result-render   (form-result-render block)
   :result-kind     (form-result-kind block)
   :result-detail   (form-result-detail block)
   :error           (:error block)
   :success?        (nil? (:error block))
   :silent?         (and (nil? (:error block))
                      (or (:vis/silent block)
                        (= :vis/silent (:result block))
                        (structurally-silent-block? block)))})

(defn- it->iteration-entry
  "Turn one persisted iteration row into the same shape the live
   progress tracker produces — a map carrying `:thinking`, `:forms`,
   `:recaps`, and `:provider-fallbacks`. The renderer consumes both
   live and resumed traces through this single shape."
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
  (let [iter-code   (or (:code it) "")
        envelopes   (vec (or (:forms it) []))
        ;; Sentinel block when the iteration ran no forms (eval timeout,
        ;; pre-eval rejection, etc.). Keep the iteration visible with
        ;; whatever code text we have so the bubble doesn't collapse to
        ;; nothing on restore.
        fallback-blocks
        [(cond-> {:position 0 :code iter-code}
           (some? (:render-segments it)) (assoc :render-segments (:render-segments it))
           (some? (:duration-ms it))     (assoc :duration-ms (:duration-ms it))
           (some? (:result it))          (assoc :result (:result it))
           (some? (:error it))           (assoc :error (:error it)))]
        envelope->block
        (fn [idx env]
          (let [src      (or (:src env) (:source env) (:code env) "")
                segments (vis/parse-block-display src)]
            (cond-> {:position idx
                     :code     src
                     ;; `:scope` ("tN/iM/fK") preserves the per-form
                     ;; provenance string the engine stamped at write
                     ;; time. Live bubbles never had it but the
                     ;; renderer ignores unknown keys safely.
                     :scope    (:scope env)
                     :tag      (:tag env)}
              (contains? env :result)      (assoc :result (:result env))
              (contains? env :error)       (assoc :error  (:error env))
              ;; `:channel` is the pre-rendered IR sink the loop
              ;; serialises onto every tool-call envelope (see
              ;; `ctx-engine/block->envelope`). Carrying it onto the
              ;; restored block lets `block->form-record` reuse the
              ;; LIVE chokepoint (`form-result-render`) without any
              ;; restore-only branch — same renderer everywhere.
              (seq (:channel env))         (assoc :channel (vec (:channel env)))
              (seq segments)               (assoc :render-segments segments)
              (some? (:duration-ms env))   (assoc :duration-ms (:duration-ms env)))))
        all-blocks  (if (seq envelopes)
                      (vec (map-indexed envelope->block envelopes))
                      fallback-blocks)
        answer-here? (and produced-answer?
                       (= (:id it) last-iteration-id)
                       (seq all-blocks))
        preflight-source? (fn [b]
                            (let [c (some-> (:code b) str str/triml)]
                              (and c (str/starts-with? c "(vis/preflight-error"))))
        preflight-idxs (into #{}
                         (keep-indexed (fn [i b]
                                         (when (or (:vis/preflight? b)
                                                 (preflight-source? b))
                                           i)))
                         all-blocks)
        answer-idx  (when answer-here?
                      (let [idx   (or (:answer-position it)
                                    (dec (count all-blocks)))
                            block (when (and (integer? idx) (not (neg? idx))
                                          (< idx (count all-blocks)))
                                    (get all-blocks idx))]
                        (when (and block
                                (not (visible-code-segments? block))
                                (or (= :vis/answer (:result block))
                                  (str/includes? (str (:code block)) "(done")))
                          idx)))
        elide-idxs  (cond-> preflight-idxs
                      (some? answer-idx) (conj answer-idx))
        visible     (into [] (keep-indexed (fn [idx b]
                                             (when-not (contains? elide-idxs idx) b)))
                      all-blocks)
        ;; New per-form envelopes carry :duration-ms. Older DB rows do
        ;; not; recover the iteration eval duration when exactly one
        ;; non-answer form remains visible after `(done ...)` elision so
        ;; restored green footers still match live bubbles.
        duration-fallback-idxs
        (when (some? (:duration-ms it))
          (into []
            (keep-indexed
              (fn [idx b]
                (let [src (some-> (:code b) str str/triml)]
                  (when (and (nil? (:duration-ms b))
                          (not= :vis/answer (:result b))
                          (not (str/starts-with? (or src "") "(done")))
                    idx))))
            visible))
        visible     (if (= 1 (count duration-fallback-idxs))
                      (update visible (first duration-fallback-idxs)
                        assoc :duration-ms (:duration-ms it))
                      visible)
        ;; Persisted `:forms` are model-facing proof envelopes (one per
        ;; top-level form, scopes tN/iM/fK). Live TUI progress, however,
        ;; renders one card per emitted fence/code-entry. A single fence
        ;; containing `(git/status)`, `(git/add ...)`, `(git/commit! ...)`,
        ;; `(git/push!)` therefore appeared as ONE block live but split into
        ;; four cards after resume. Re-group restored envelopes back to the
        ;; fence row so resume matches live presentation; keep channel slices
        ;; and first error so tool previews / failures still render.
        visible     (if (and (seq envelopes) (seq visible))
                      (let [first-visible (first visible)
                            channels      (vec (mapcat :channel visible))
                            first-error   (some :error visible)
                            grouped-code  (if (seq elide-idxs)
                                            (str/join "\n" (keep :code visible))
                                            iter-code)
                            grouped       (cond-> {:position 0
                                                   :code     grouped-code
                                                   :scope    (:scope first-visible)
                                                   :tag      (:tag first-visible)}
                                            (seq channels) (assoc :channel channels)
                                            (some? first-error) (assoc :error first-error)
                                            (nil? first-error) (assoc :result (:result (last visible)))
                                            (seq (vis/parse-block-display grouped-code))
                                            (assoc :render-segments (vis/parse-block-display grouped-code))
                                            (some? (:duration-ms it))
                                            (assoc :duration-ms (:duration-ms it)))]
                        [grouped])
                      visible)
        ;; `:forms` are the PERSISTED proof envelopes (model-facing, one per
        ;; top-level form). `iteration/canonicalize` derives the DISPLAY-state
        ;; `:ops` from each envelope's `:channel` sink slice — `:ops` is what
        ;; the renderer paints as block op rows; `:forms` stays proof-granular.
        forms       (mapv block->form-record visible)]
    (iteration/canonicalize
      {:position           (when-let [p (:position it)] (dec (long p)))
       :thinking           (visible-thinking (:thinking it))
       :provider-fallbacks (:llm-routing-trace it)
       :forms              forms
     ;; Iteration-level recaps DROPPED: model bookkeeping (task-set! /
     ;; spec-set! / fact-set! / set-session-title!) is silent in the
     ;; chat trace. State is visible via :session/{tasks,specs,facts}
     ;; in the engine ctx (next-iter render) and via the task / spec /
     ;; fact panes in the chrome — the chat history shouldn't echo every
     ;; mutation. Provider-error / fallback / consult recaps still flow
     ;; via render.clj's own paths.
       :recaps             []})))

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
   {:role      :user
    :ir        (vis/markdown->ir text {:soft-break :hard})
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

(defn- answer->markdown
  "Extract the Markdown string from a final-answer value.

   The Markdown-answer pipeline produces exactly two shapes:
     - `{:answer string}`                                    -- canonical `(done {:answer ...})`
     - `{:vis/answer-mode :needs-input :answer/text string}` -- needs-input gate

   Anything else is an upstream bug; we surface it as a pr-str fallback
   so the user sees something instead of an empty bubble. `fallback-text`
   covers the empty-answer case (cancellation slots, placeholder bubbles)."
  [answer fallback-text]
  (cond
    (and (map? answer) (string? (:answer answer)))      (:answer answer)
    (and (map? answer) (string? (:answer/text answer))) (:answer/text answer)
    (seq fallback-text)                                 fallback-text
    :else                                               ""))

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
   (cond
     (nil? source)
     {:role :assistant :text "" :ir empty-ir :timestamp timestamp}

     (string? source)
     {:role      :assistant
      :text      source
      :ir        (if (str/blank? source) empty-ir (vis/markdown->ir source))
      :timestamp timestamp}

     (and (vector? source) (= :ir (first source)))
     {:role      :assistant
      :text      (vis/render source :markdown)
      :ir        source
      :timestamp timestamp}

     :else
     (throw (ex-info "chat/assistant-message requires Markdown string or canonical [:ir ...] input"
              {:got-type (some-> source class .getName)})))))

(defn- rebuild-history
  "Reconstruct message history from DB for a session.
   Returns a vec of {:role :user|:assistant :text str :timestamp #inst ...}.
   Assistant messages include the code execution trace from all iterations."
  [session-id]
  (try
    (let [d               (vis/db-info)
          turns           (vis/db-list-session-turns d session-id)]
      (into []
        (mapcat (fn [q]
                  (let [user-message (user-message (or (:user-request q) "") (or (:created-at q) (java.util.Date.)))
                        ;; `:prior-outcome :cancelled` is how the
                        ;; persistance layer marks an aborted turn (the
                        ;; sweep + cancel paths both write that value).
                        cancelled? (= :cancelled (:prior-outcome q))
                        ;; Persistence stores the raw Markdown source
                        ;; the model wrote in `(done {:answer ...})`.
                        ;; Channels derive IR via `vis/markdown->ir` at
                        ;; render time; keep the source so copy/export
                        ;; round-trip byte-for-byte. Cancelled turns can
                        ;; persist with no answer when the user aborts
                        ;; before the first iteration; show an explicit
                        ;; status line instead of a blank gray Vis bubble.
                        answer-md (let [stored (or (:answer-markdown q) "")]
                                    (if (and cancelled? (str/blank? stored))
                                      "Cancelled by user."
                                      stored))
                        answer-ir (if (str/blank? answer-md)
                                    empty-ir
                                    (vis/markdown->ir answer-md))
                        model     (:model q)
                        ;; Phase B: canonical token shape mapped to the per-bubble
                        ;; render contract. `:input` is TOTAL, `:input-regular` /
                        ;; `:cached` (cache-read) / `:cache-created` (cache-write)
                        ;; are subsets. Render layer formats whichever it needs;
                        ;; absent keys stay absent.
                        tokens    (cond-> {}
                                    (:input-tokens q)             (assoc :input (:input-tokens q))
                                    (:input-regular-tokens q)     (assoc :input-regular (:input-regular-tokens q))
                                    (:input-cache-read-tokens q)  (assoc :cached (:input-cache-read-tokens q))
                                    (:input-cache-write-tokens q) (assoc :cache-created (:input-cache-write-tokens q))
                                    (:output-tokens q)            (assoc :output (:output-tokens q))
                                    (:output-reasoning-tokens q)  (assoc :reasoning (:output-reasoning-tokens q)))
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
                        last-it (last turn-iterations)
                        last-iteration-id (:id last-it)
                        llm-routing (cond-> {}
                                      (:llm-selected last-it) (assoc :selected (:llm-selected last-it))
                                      (:llm-actual last-it) (assoc :actual (:llm-actual last-it))
                                      (contains? last-it :llm-fallback?) (assoc :fallback? (:llm-fallback? last-it))
                                      (seq (:llm-routing-trace last-it)) (assoc :trace (:llm-routing-trace last-it)))
                        ;; A non-blank Markdown source signals a real
                        ;; final answer; empty / nil-only turns keep
                        ;; their assistant message but elide the
                        ;; answer-bearing form differently.
                        produced-answer? (not (str/blank? answer-md))
                        trace (into [] (map (partial it->iteration-entry
                                              {:produced-answer? produced-answer?
                                               :last-iteration-id last-iteration-id}))
                                turn-iterations)
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
  "Create a fresh `:tui` session.

   Optional opts map (second arity):
     :workspace-id  pre-spawned workspace to pin the new session to.
                    Omit and a trunk workspace is auto-minted by
                    `create-environment`.

   Returns `{:id session-id :history []}`."
  ([_provider-config] (make-session _provider-config nil))
  ([_provider-config {:keys [workspace-id]}]
   (let [{:keys [id]} (vis/create! :tui (when workspace-id
                                          {:workspace-id workspace-id}))]
     {:id id :history []})))

(defn- resolve-resume-id
  "Resolve a resume id to a full `java.util.UUID`, or nil. Accepts a
   full UUID or an unambiguous prefix among :tui sessions."
  [session-id]
  (let [cid (some-> session-id str str/trim)]
    (when (seq cid)
      (or (some-> (vis/by-id cid) :id)        ; UUID
        (let [matches (->> (vis/by-channel :tui)
                        (map :id)
                        (filter #(str/starts-with? (str %) cid))
                        vec)]
          (when (= 1 (count matches))
            (first matches)))))))

(defn resume-session
  "Resume an existing session by id.
   Accepts full UUID or unambiguous short UUID prefix.
   Returns `{:id UUID :history [...]}` with persisted messages."
  [session-id]
  (when-let [resolved-id (resolve-resume-id session-id)]
    (when-let [session (vis/by-id resolved-id)]
      {:id (:id session) :history (rebuild-history (:id session))})))

(defn turn!
  "Send a user request through the shared sessions cache. Blocking.
   Returns `{:answer [:ir ...]}` or `{:error str}`.

   `opts` map:
     :on-chunk          fn receiving phased chunks; phases include
                        `:reasoning`, `:form-start`, `:form-result`,
                        `:consult-resolved`, `:iteration-final`,
                        `:iteration-error`. The TUI feeds these into
                        `progress/make-progress-tracker` to paint a
                        live per-iteration timeline in the placeholder
                        bubble.
     :cancel-token      cancellation handle from `vis/cancellation-token`.
                        `(vis/cancel! token)` flips the cooperative flag
                        AND fires every registered `on-cancel!` callback
                        (SCI worker future, provider HTTP client, voice
                        recorder) so unresponsive SCI hangs die straight
                        from the input thread without waiting for the
                        eval timeout.
     :reasoning-default base reasoning effort (`:quick` | `:balanced`
                        | `:deep`) for reasoning-capable models. The
                        engine may bump within the turn; this is the
                        starting level. Per-user setting in Telegram.
     :extra-body        provider-specific HTTP request-body overrides.
                        Used by the engine for `:max_tokens` bump-on-
                        retry; callers can override custom provider
                        params (anthropic headers, openai temperature,
                        etc).
     :turn-features     per-turn feature flag map
                        (e.g. `{:voice-response? true}`) consumed by
                        extension `:ext/prompt` callbacks. Tells the
                        prompt assembler about transient user intents
                        (voice-mode reply length, etc).
     :workspace         workspace pin overrides for unusual per-turn
                        cases."
  ([session text] (turn! session text {}))
  ([{:keys [id]} text {:keys [on-chunk cancel-token reasoning-default extra-body turn-features workspace]}]
   (try
     (let [send-opts (cond-> {}
                       on-chunk          (assoc :hooks {:on-chunk on-chunk})
                       cancel-token      (assoc :cancel-token cancel-token)
                       reasoning-default (assoc :reasoning-default reasoning-default)
                       extra-body        (assoc :extra-body extra-body)
                       turn-features     (assoc :turn/features turn-features)
                       (seq workspace)   (merge workspace))
           result (vis/send! id text send-opts)
           cancelled? (= :cancelled (:status result))
           ;; `(:answer result)` is a `{:answer markdown}` map from
           ;; `(done {:answer ...})`, a needs-input map, a plain
           ;; string (cancel/error fallbacks), or nil. Normalize to a
           ;; Markdown string here; the bubble layer derives IR from
           ;; it via `vis/markdown->ir`.
           answer-markdown (answer->markdown (:answer result)
                             (if cancelled? "Cancelled by user." ""))
           answer-ir       (if (str/blank? answer-markdown)
                             empty-ir
                             (vis/markdown->ir answer-markdown))
           answer answer-ir
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
           {:answer (vis/markdown->ir "Cancelled by user.")
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
