(ns com.blockether.vis.internal.gateway.state
  "Gateway session manager.

   One process-global registry over the live session fleet: per-session
   ordered event log (monotonic `:seq`, ring-buffered), SSE subscriber
   fan-out, async turn submission with idempotency keys, cancellation,
   and turn/cost metrics.

   The engine is reached ONLY through the same internal surfaces the
   TUI/Telegram channels use: `loop/create!`-`send!`-`close!` for the
   lifecycle, `:hooks {:on-chunk ...}` phased chunks for the live
   stream, `ctx-loop/session-snapshot` for the context. No engine state
   lives here - this namespace owns wire bookkeeping (events, turn
   records, subscribers), nothing else."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.attachment-storage :as attachment-storage]
            [com.blockether.vis.internal.cancellation :as cancellation]
            [com.blockether.vis.internal.form :as form]
            [com.blockether.vis.internal.session-model :as smodel]
            [com.blockether.vis.internal.ctx-loop :as ctx-loop]
            [com.blockether.vis.internal.gateway.bus :as bus]
            [com.blockether.vis.internal.gateway.wire :as wire]
            [com.blockether.vis.internal.loop :as lp]
            [com.blockether.vis.internal.titling :as titling]
            [com.blockether.vis.internal.persistance :as persistance]
            [com.blockether.vis.internal.provider-error :as provider-error]
            [com.blockether.vis.internal.resources :as resources]
            [com.blockether.vis.internal.render :as ir]
            [com.blockether.vis.internal.workspace :as workspace]
            [taoensso.telemere :as tel]))

(def ^:private EVENT_RING_MAX
  "Per-session event-log ring size. Older events stay durable in the
   session transcript; the ring only backs SSE cursor replay."
  10000)

(def ^:private RESULT_PR_LIMIT 4000)
(def ^:private ERROR_PR_LIMIT 2000)

;; Live reasoning/content deltas arrive per provider token, and every wire
;; event carries the FULL cumulative text (self-contained frames — consumers
;; REPLACE, never append). Serializing + fanning out + journaling that growing
;; text once per token is O(n²) over a long stream; coalesce the transient
;; delta phases to at most one wire event per window. Lossless: a skipped
;; frame is subsumed by the next cumulative one, and `:done?` frames always
;; pass so the final state lands immediately.
(def ^:private DELTA_COALESCE_MS 100)

(def ^:private DELTA_COALESCE_MAX_MS
  "Ceiling for the ADAPTIVE coalescing window: even a huge cumulative
   reasoning stream still ticks at least once per second."
  1000)

(def ^:private DELTA_CHARS_PER_MS
  "Adaptive scale for the coalescing window: +1ms of window per this many
   chars of cumulative delta text. Up to ~25KB stays at the 100ms floor
   (~10 frames/s); the window then grows linearly and caps at 1s around
   ~256KB — bounding the wire / journal / client re-render cost of the
   ever-growing replace-style frames instead of shipping megabytes per
   second to every subscriber."
  256)

(def ^:private transient-delta-phases
  "Per-token streaming phases whose wire events are cumulative and therefore
   safely coalescible (see `DELTA_COALESCE_MS`). `:assistant-prose` is a
   one-shot boundary frame, never a token stream — it always passes."
  #{:reasoning :content})

(defn- delta-window-ms
  "The coalescing window for ONE transient delta chunk. Every frame carries
   the FULL cumulative text, so the per-frame cost (serialize + SSE fan-out +
   cross-process journal append + each client's re-render) GROWS with the
   text; scale the window with that size so long streams degrade smoothly to
   ~1 frame/s instead of hammering every client at 10Hz with a growing wall."
  ^long [{:keys [text thinking content]}]
  (let [len (count (str (or text thinking content)))]
    (-> (quot len DELTA_CHARS_PER_MS)
        (max DELTA_COALESCE_MS)
        (min DELTA_COALESCE_MAX_MS))))

(defn- coalesce-delta?
  "True when this transient reasoning/content delta should be SKIPPED on the
   wire: a fresher cumulative frame will follow within the (size-adaptive)
   window. `:done?` frames and every non-delta phase always pass."
  [last-delta-ms {:keys [phase done?] :as chunk} now]
  (and (contains? transient-delta-phases phase)
       (not done?)
       (< (- (long now) (long (get last-delta-ms phase 0))) (delta-window-ms chunk))))

;; sid (UUID) -> {:next-seq long
;;                :events [event ...]          ; ring, ascending :seq
;;                :subscribers {sub-id fn}     ; SSE sinks
;;                :turns {tid turn-record}     ; :cancel-token stripped on wire
;;                :turn-order [tid ...]
;;                :current-turn tid|nil
;;                :idempotency {key tid}
;;                :last-active epoch-ms}
(defonce ^:private registry (atom {}))

(defonce ^:private metrics
  (atom {:turns-total 0
         :turns-failed 0
         :tokens-input 0
         :tokens-output 0
         :cost-total 0.0
         :duration-ms-total 0
         :per-session {}}))

;; =============================================================================
;; Event log + fan-out
;; =============================================================================

(defn- trim-ring
  [events]
  (let [n (count events)]
    (if (> n EVENT_RING_MAX) (subvec events (- n EVENT_RING_MAX)) events)))

(defn- fan-out!
  "Deliver `event` to every local SSE sink for `sid`. A sink that throws is
   dropped - one dead connection must never poison the appender or siblings."
  [sid event]
  (doseq [[sub-id sink] (get-in @registry [sid :subscribers])]
    (try (sink event)
         (catch Throwable t
           (swap! registry update-in [sid :subscribers] dissoc sub-id)
           (tel/log! :debug ["gateway: dropped dead subscriber" sub-id (ex-message t)])))))

(defn append-event!
  "Append one event for `sid`, fan it out to LOCAL subscribers, and publish
   it on the cross-process bus so watchers in OTHER processes stream it too.

   Assigns the next monotonic `:seq` atomically. `:store? false` events
   are fanned out live but kept OUT of the replay ring, so neither a
   cursor replay nor a `/poll` pull (both read the ring) re-delivers
   them - reserve it for genuinely ephemeral fan-out where SSE/poll
   divergence is acceptable (no current caller; `:store?` defaults true).
   A subscriber sink that throws is dropped - one dead SSE connection
   must never poison the appender or sibling subscribers."
  ([sid type payload] (append-event! sid type payload {:store? true}))
  ([sid type payload {:keys [store?]}]
   (let [captured (volatile! nil)]
     (swap! registry update
       sid
       (fn [entry]
         (let [entry (or entry {:next-seq 0})
               n (inc (:next-seq entry 0))
               event
               (merge
                 {:schema 1 :seq n :ts (System/currentTimeMillis) :session_id (str sid) :type type}
                 payload)]

           (vreset! captured event)
           (cond-> (assoc entry
                     :next-seq n
                     :last-active (System/currentTimeMillis))
             store?
             (update :events #(trim-ring (conj (or % []) event)))))))
     (let [event @captured]
       (fan-out! sid event)
       ;; Mirror to sibling processes. `turn.started` truncates the journal so
       ;; a file only ever holds the current turn's live deltas.
       (bus/publish! sid event {:store? store? :truncate? (= type "turn.started")})
       event))))

(defn ingest-mirrored-event!
  "Deliver a FOREIGN gateway event (produced in another process, arriving via
   the cross-process bus) into THIS process's registry so a web / Telegram /
   TUI watcher streams a turn running elsewhere in real time.

   The foreign event is RE-SEQUENCED onto this process's OWN monotonic `:seq`,
   never the producer's. Each process runs an independent seq counter, but the
   SSE wire treats `:seq` as a single strictly-increasing per-connection cursor;
   adopting the producer's raw counter would let a watcher whose local seq is
   already past that value (e.g. it ran an earlier turn on this session) silently
   drop the entire foreign turn. Re-sequencing keeps THIS process's stream
   monotonic for its own subscribers regardless of the producer's counter — and
   is safe because only the producer persists the turn; the mirror is live-only.

   Stored in the ring when `store?`; `:current-turn` mirrored so the session
   list lights up while the turn runs elsewhere. A running TURN ROW is
   materialized in `:turns`/`:turn-order` on `turn.started` (and marked terminal
   on `turn.completed`/`turn.failed`) so `list-turns` frames the mirrored turn
   exactly like a locally-started one — user bubble, running chip, correct live
   placement — instead of leaking bare deltas under the previous answer.

   Ignores sessions this process has never touched (no local registry entry), so
   no state accrues for conversations nobody here is watching."
  [sid store? event]
  (when (contains? @registry sid)
    (let [type
          (:type event)

          tid
          (:turn_id event)

          terminal?
          (contains? #{"turn.completed" "turn.failed"} type)

          term-patch
          (-> event
              (dissoc :type :seq :turn_id)
              (assoc :status (or (:status event) (if (= type "turn.failed") "failed" "completed"))))

          captured
          (volatile! nil)]

      (swap! registry update
        sid
        (fn [entry]
          (if entry
            (let [n
                  (inc (:next-seq entry 0))

                  ev
                  (assoc event :seq n)]

              (vreset! captured ev)
              (cond-> (assoc entry
                        :next-seq n
                        :last-active (System/currentTimeMillis))
                store?
                (update :events #(trim-ring (conj (or % []) ev)))

                (= type "turn.started")
                (-> (assoc :current-turn tid)
                    (assoc-in [:turns tid]
                              {:turn_id tid
                               :session_id (str sid)
                               :status "running"
                               :request (:request event)
                               ;; Adopt the PRODUCER's canonical run-start
                               ;; clock — stamping mirror-local time here made
                               ;; a watcher in another process show a
                               ;; different elapsed than the producer.
                               :started_at (or (:started_at event) (System/currentTimeMillis))})
                    (update :turn-order
                            (fn [order]
                              (if (some #{tid} order) order ((fnil conj []) order tid)))))

                (and terminal? tid (get-in entry [:turns tid]))
                (update-in [:turns tid] merge term-patch)

                (and terminal? (= tid (:current-turn entry)))
                (assoc :current-turn nil)))
            entry)))
      (when-let [ev @captured]
        (fan-out! sid ev))))
  nil)

(defn subscribe!
  "Register an SSE sink and return the replay vector (events with
   `:seq` > `cursor`) ATOMICALLY with the registration, so no event can
   fall between replay and live fan-out. The caller serializes replay
   writes against live sink calls (see server.clj).

   Before capturing replay, HYDRATE any turn currently running in a sibling
   process from the cross-process journal (`bus/hydrate!`) — but only when this
   process isn't already tracking a live turn (`:current-turn` unset), so an
   already-mirrored turn isn't re-delivered to existing subscribers. This
   materializes the running turn's row + ring HERE, so a watcher joining a turn
   in flight elsewhere replays it from `turn.started` (user bubble + running
   frame) instead of catching only the bare deltas after connect."
  [sid sub-id sink cursor]
  ;; ensure an entry exists so `ingest-mirrored-event!` (called by hydrate)
  ;; doesn't no-op, then hydrate the in-flight foreign turn INTO the ring
  ;; before we snapshot replay from it.
  (swap! registry update
    sid
    (fn [entry]
      (or entry {:next-seq 0})))
  (when-not (:current-turn (get @registry sid)) (bus/hydrate! sid))
  (let [replay (volatile! [])]
    (swap! registry update
      sid
      (fn [entry]
        (let [entry (or entry {:next-seq 0})]
          (vreset! replay (filterv #(> (:seq %) (or cursor 0)) (:events entry)))
          (assoc-in entry [:subscribers sub-id] sink))))
    @replay))

(defn unsubscribe! [sid sub-id] (swap! registry update-in [sid :subscribers] dissoc sub-id) nil)

(defn current-seq
  "Highest event `:seq` assigned for `sid` so far. Subscribing with this
   as the cursor yields a live-only stream (empty replay)."
  [sid]
  (get-in @registry [sid :next-seq] 0))

(defn events-since
  "Read-only peek at the replay ring: stored events with `:seq` > cursor,
   oldest first. Lets a page renderer locate the running turn's
   `turn.started` seq so its SSE reconnect can replay the WHOLE in-flight
   turn instead of only what happens after connect."
  [sid cursor]
  (filterv #(> (:seq %) (or cursor 0)) (get-in @registry [sid :events] [])))

(defn running-turn-count
  "Number of live turns currently owned by this gateway process. Used by the
   daemon lifecycle gate: the server may only self-stop when this is zero AND
   the client refcount is zero."
  []
  (->> (vals @registry)
       (keep :current-turn)
       count))

;; =============================================================================
;; Per-session model preference
;; =============================================================================

(defn set-session-model!
  "Set (or clear, with blank model) the per-session PROVIDER + MODEL
   preference. Every turn submitted for `sid` routes through it (the engine
   reads it at turn start; `router-for-model` hoists the model, an unknown
   name degrades to the default order). Channel-agnostic: web + TUI + embedded
   callers all set it here, persisted in the DB and shared across channels."
  [sid provider model]
  (swap! registry update sid #(assoc (or % {:next-seq 0}) :last-active (System/currentTimeMillis)))
  (smodel/set-model! (lp/db-info) sid provider model))

(defn session-model
  "The session's persisted model preference as `{:provider :model}`
   (DB-backed shared store), or nil for the router default."
  [sid]
  (smodel/model-of (lp/db-info) sid))

(defn session-model-cached
  "Cached variant of `session-model` for hot render paths. Still part of the
  gateway facade: callers do not reach into the session-model store directly."
  [sid]
  (smodel/model-of-cached (lp/db-info) sid))

(defn session-workspace-info
  "Workspace state for a channel surface (the web footer AND the TUI
   directory picker): `{:id :draft? :root :repo-root :label :fork-ms
   :filesystem-roots}` for the session pinned to `sid` (soul id), or nil.
   `:id` is the workspace-id every filesystem-root mutation
   (`add/remove-filesystem-root!`) needs — WITHOUT it the TUI picker treats the
   session as read-only and C-a silently no-ops. `:filesystem-roots` is the
   normalized `[{:trunk :clone :fork-ms}]`. Lets the footer announce that the
   session — and its extra roots — are isolated drafts. Resolves soul → latest
   state → workspace; never throws."
  [sid]
  (try (when-let [db (lp/db-info)]
         (when-let [state-id (persistance/db-latest-session-state-id db (str sid))]
           (when-let [ws (workspace/for-session db state-id)]
             {:id (:id ws)
              :draft? (workspace/draft? ws)
              :root (:root ws)
              :repo-root (:repo-root ws)
              :label (:label ws)
              :fork-ms (:fork-ms ws)
              :filesystem-roots (workspace/filesystem-roots ws)})))
       (catch Throwable _ nil)))

(defn- session-state-id
  "Latest persisted state id for soul `sid`, or nil."
  [db sid]
  (persistance/db-latest-session-state-id db (str sid)))

(defn add-filesystem-root!
  "Add `path` as an extra filesystem root for the session pinned to `sid`, then
   return the refreshed `session-workspace-info`. Runs SERVER-SIDE in the daemon
   so the draft backend-fork and DB write land where the session actually lives;
   every channel (web footer, TUI picker/footer) then reads the same roots back
   over the gateway. Channel-agnostic twin of `set-session-model!`."
  [sid path]
  (when-let [db (lp/db-info)]
    (when-let [state-id (session-state-id db sid)]
      (when-let [ws (workspace/for-session db state-id)]
        (workspace/add-filesystem-root! db (:id ws) path))))
  (session-workspace-info sid))

(defn remove-filesystem-root!
  "Remove `path` from the session's extra filesystem roots and return the
   refreshed `session-workspace-info`. Server-side twin of `add-filesystem-root!`."
  [sid path]
  (when-let [db (lp/db-info)]
    (when-let [state-id (session-state-id db sid)]
      (when-let [ws (workspace/for-session db state-id)]
        (workspace/remove-filesystem-root! db (:id ws) path))))
  (session-workspace-info sid))

(defn change-root!
  "Repoint the session pinned to `sid` at `path` as its PRIMARY root, then return
   the refreshed `session-workspace-info` (whose `:id` is the newly pinned
   workspace). Server-side so the change lands in the daemon that runs the turns."
  [sid path]
  (when-let [db (lp/db-info)]
    (when-let [state-id (session-state-id db sid)]
      (workspace/change-root! db state-id path)))
  (session-workspace-info sid))

;; =============================================================================
;; Chunk -> event translation (§8)
;; =============================================================================

(defn- error->wire-text
  "LEAN client-facing text for a form error: the message (+ python line/col
   when present, + the recovery hint when it isn't already in the message).
   Never the pr-str'd error map — the raw map nests trace/host chains no
   user or client can act on."
  [error]
  (if-not (map? error)
    (str error)
    (let [msg
          (or (:message error)
              (some-> (:type error)
                      str)
              "error")

          hint
          (:hint error)

          {:keys [line column]}
          (:data error)]

      (cond-> msg
        (and line column)
        (str " (line " line ", col " column ")")

        (and hint (not (str/includes? msg (str hint))))
        (str "\nhint: " hint)))))

(defn- normalize-thinking-text
  "Canonical thinking text for every gateway surface. Reasoning streams can
  arrive with paragraph-style blank-line runs and whitespace-padded blank rows;
  normalize that once at the gateway boundary so SSE, poll/replay, and session
  consumers all see the same compact trace."
  [text]
  (when-let [s (some-> text
                       str)]
    (not-empty (-> s
                   (str/replace #"[ \t\r\f\v]+\r?\n" "\n")
                   (str/replace #"(?:\r?\n){2,}" "\n")
                   str/trim))))

(defn- chunk->event
  "Translate one phased iteration chunk (progress.clj contract) into a
   `[type store? payload]` wire event triple."
  [{:keys [phase position code result error silent? done? iteration text thinking content]
    :as chunk}]
  ;; Every streaming chunk carries its iteration POSITION under `:iteration`.
  ;; It MUST ride the wire event, or `make-progress-tracker` silently DROPS the
  ;; chunk (it skips chunks with no iteration) — which is how `block.started` /
  ;; `block.output` once lost their forms and the live bubble showed reasoning
  ;; but no code.
  (let [payload
        (case phase
          :reasoning
          {:text (normalize-thinking-text (or text thinking content))}

          ;; Model PROSE streaming live alongside the tool call (`:content` = the
          ;; growing tail; `:assistant-prose` = the final commentary on iteration
          ;; close). Both ride as `content.delta` so the bubble paints the markdown.
          ;; PROSE is markdown, NOT reasoning: only trim it — never run it through
          ;; `normalize-thinking-text`, which collapses the blank-line runs that
          ;; markdown needs for paragraph / list separation.
          :content
          {:text (some-> (or content text)
                         str
                         str/trim
                         not-empty)}

          ;; `:prose-final` distinguishes the COMPLETE end-of-iteration commentary
          ;; from the growing `:content` tail (both ride `content.delta`). The web
          ;; pins this one as a permanent thread block; the partials stay transient.
          :assistant-prose
          {:text (some-> (or text content)
                         str
                         str/trim
                         not-empty)
           :prose-final true}

          :form-start
          (merge
            ;; Carry the native-tool badge identity so a client can
            ;; hide the redundant invocation code WHILE the tool runs.
            (form/->display chunk)
            {:block_id position :code code})

          :form-result
          (merge
            ;; The native-tool op-card fields (pre-rendered card +
            ;; badge label + colour) — projected from ONE canonical
            ;; list (`form/tool-display-keys`) so the gateway can't
            ;; silently drop one the way it used to. A new op-card
            ;; field flows here automatically.
            (form/->display chunk)
            {:block_id position
             :code code
             :result result
             ;; The SINGLE display surface: what the block PRINTED
             ;; (joined per-form stdout, computed loop-side — the
             ;; same text the model reads back). Clients paint this
             ;; instead of render-fn op cards / result blobs.
             :stdout (when-let [s (:stdout chunk)]
                       (wire/bounded-str s RESULT_PR_LIMIT))
             ;; Lean error text — always surfaced now (no op card
             ;; to dedupe against; stdout + error are the only
             ;; result surfaces).
             :error (when (some? error) (wire/bounded-str (error->wire-text error) ERROR_PR_LIMIT))
             ;; a `vis_silent` result suppresses the row.
             :silent (boolean (or silent? (and (nil? error) (contains? #{"vis_silent"} result))))
             :duration_ms (let [{:keys [started-at-ms finished-at-ms]} (:envelope chunk)]
                            (when (and (nat-int? started-at-ms) (nat-int? finished-at-ms))
                              (max 0 (- (long finished-at-ms) (long started-at-ms)))))})

          ;; the iteration's full reasoning rides the boundary event so
          ;; the web thread can pin it as a permanent thinking block
          ;; (the live #thinking ticker only ever shows the moving tail)
          :iteration-final
          {:done (boolean done?) :thinking (normalize-thinking-text thinking)}

          :iteration-error
          ;; Carry the SAME canonical provider-error map the final settled turn
          ;; bubble paints the styled CARD from (`provider-error-info` →
          ;; `:vis/provider-error-data`), so the LIVE `iteration.error` fragment
          ;; renders identically instead of dumping the flattened error string.
          ;; An iteration-error is always a provider-call failure (user-code
          ;; errors surface as `block.output`), so — exactly like the loop's final
          ;; `provider-error-ir` fallback — every one carries the structured data
          ;; (`:kind` may be `:generic`; the card renders it just the same).
          (cond-> {:error (when (some? error)
                            (wire/bounded-str (error->wire-text error) ERROR_PR_LIMIT))
                   :thinking (normalize-thinking-text thinking)}
            (some? error)
            (assoc :provider-error-data (provider-error/provider-error-info error)))

          {:detail (wire/bounded-pr (dissoc chunk :phase) ERROR_PR_LIMIT)})]
    [(case phase
       :reasoning
       "reasoning.delta"

       :content
       "content.delta"

       :assistant-prose
       "content.delta"

       :form-start
       "block.started"

       :form-result
       "block.output"

       :iteration-final
       "iteration.completed"

       :iteration-error
       "iteration.error"

       :provider-retry-reset
       "provider.retry"

       (str "chunk." (name phase)))
     ;; reasoning + live content deltas are transient (not persisted to the wire log)
     (not (#{:reasoning :content :assistant-prose} phase))
     (cond-> payload
       (some? iteration)
       (assoc :iteration iteration))]))

;; =============================================================================
;; Context
;; =============================================================================

(defn context-snapshot
  "The read-only ctx mirror the model sees as its bound `session`
   (`ctx-loop/session-snapshot`), for an existing session, ENRICHED for
   the USER with `:session/archived` (the GC'd/summarized entities that are
   no longer in the model's live ctx). Resolving the env
   through `lp/env-for` rehydrates an evicted session on demand.
   nil when the session does not exist."
  [sid]
  (when (lp/by-id sid)
    (when-let [env (lp/env-for sid)]
      (ctx-loop/session-snapshot env))))

(defn- emit-context-updated!
  [sid]
  (let [snapshot (try (context-snapshot sid) (catch Throwable _ nil))]
    ;; `snapshot` is the STRING-KEYED session-view (`eng/session-view`).
    (when-let [utilization (get snapshot "session_utilization")]
      (append-event! sid "context.updated" {:utilization utilization}))))

;; =============================================================================
;; Turn records
;; =============================================================================

(defn- ir-ast-answer?
  "True when the engine handed back a canonical IR AST (`[:ir {…} …]`) as
   the turn answer rather than markdown - loop.clj does this for the
   provider-error and fatal-iteration fallbacks. Those answers must ride
   the wire as `:answer_ir` and render through the IR walker; pr-str'ing
   the vector into `:answer_md` printed the raw AST into the chat bubble."
  [answer]
  (and (vector? answer) (= :ir (first answer))))

(defn- ir-ast->text
  "Depth-first flatten of a canonical IR AST to READABLE plain text for
   `:answer_md` (the DB column + clients that only read markdown). Block-level
   children (`:h`/`:p`/`:ul`/`:ol`) are separated by blank lines and list items
   by newlines, so a provider-error IR — title / WHAT HAPPENED / NEXT STEP /
   a facts list — flattens to scannable multi-line text instead of one run-on
   line (the form a refreshed page renders, since `:answer_ir`/its marker is not
   persisted). Skips the `[tag attrs]` prefix of each canonical node. Never a
   pr-str'd vector."
  [ast]
  (letfn
    [(inline [x]
       ;; Inline content: strings concatenated; every [tag attrs & kids]
       ;; node (span/code/strong/c/…) is unwrapped to its kids.
       (cond (string? x) x
             (vector? x) (str/join "" (map inline (drop 2 x)))
             (seq? x) (str/join "" (map inline x))
             :else ""))
     (block-text [node]
       (let [tag
             (first node)

             kids
             (drop 2 node)]

         (case tag
           ;; List items render one per line so a facts <ul> stays scannable.
           (:ul :ol)
           (str/join "\n" (map #(str "- " (inline (drop 2 %))) kids))

           ;; Headings / paragraphs / pre / unknown: inline text only.
           (str/join "" (map inline kids)))))
     (walk [x]
       (cond (string? x) x
             (and (vector? x) (= :ir (first x))) (str/join "\n\n" (map block-text (drop 2 x)))
             (vector? x) (block-text x)
             :else ""))]
    (not-empty (str/trim (walk ast)))))

(defn- answer-md
  "Normalize a `send!` `:answer` value to a markdown string. Accepts the
   canonical `{:answer md}` (a plain-text reply finalizes the turn), the wrapped `{:result {:answer
   md}}`, a needs-input map, a plain string (cancel/error surfaces), or an
   IR-AST answer (provider-error fallback) - which flattens to plain text
   so the bubble NEVER shows a pr-str'd vector. The rich IR rides
   `:answer_ir` alongside (see `run-turn!`)."
  [answer]
  (cond (string? answer) answer
        (and (map? answer) (string? (:answer answer))) (:answer answer)
        (and (map? answer) (string? (get-in answer [:result :answer]))) (get-in answer
                                                                                [:result :answer])
        (ir-ast-answer? answer) (ir-ast->text answer)
        (nil? answer) nil
        :else (wire/bounded-pr answer RESULT_PR_LIMIT)))

(defn- wire-turn
  [turn]
  ;; :engine_turn_id stays ON the wire view: the web page needs the ENGINE's
  ;; persisted row id to restore a finished turn's machinery after refresh
  ;; (the gateway tid is a different uuid and finds no DB iterations).
  (when turn (dissoc turn :cancel-token)))

(def ^:private terminal-turn-statuses #{"completed" "failed" "cancelled" "suspended" "error"})

(defn- date->ms [d] (when (instance? java.util.Date d) (.getTime ^java.util.Date d)))

(defn- persisted-duplicate-of-live?
  "True when persisted engine row `row` is the durable copy of gateway live row
  `live`. Prefer the persisted row on hydration: it owns the DB iteration trace,
  while the completed gateway row is only the transient SSE record. The primary
  key is :engine_turn_id; the fallback covers terminal turns that finished before
  the gateway learned/cached that engine id."
  [live row]
  (let [engine-id
        (some-> (:engine_turn_id live)
                str)

        row-id
        (some-> (:id row)
                str)

        status
        (str (:status live))]

    (or (and (seq engine-id) (= engine-id row-id))
        (and (contains? terminal-turn-statuses status)
             (str/blank? (str engine-id))
             (= (str (:request live)) (str (:user-request row)))
             ;; Answer text matches, OR the live row never captured an answer.
             ;; A FAILED/cancelled turn's failure text lands only on the durable
             ;; row (`answer-markdown`); the transient live row's `answer_md` is
             ;; blank. The old strict equality therefore never matched an error
             ;; turn, so BOTH rows rendered — the same "Could not reach provider"
             ;; twice. Request + terminal-status + created-after-start still
             ;; identifies the one turn; drop the answer requirement when the
             ;; live side has nothing to compare.
             (or (= (str (:answer_md live)) (str (:answer-markdown row)))
                 (str/blank? (str (:answer_md live))))
             (if-let [created (date->ms (:created-at row))]
               (>= created (long (or (:started_at live) 0)))
               true)))))

(defn get-turn
  "Wire view of one turn record, or nil."
  [sid tid]
  (wire-turn (get-in @registry [sid :turns tid])))

(defn- persisted-turn->wire
  "Map one persisted turn row (`db-list-session-turns` / `row->turn`
   shape) onto the wire turn record, so a reopened session shows its
   full history after a daemon restart — the ENGINE persisted every
   turn; only the gateway's overlay is in-memory.

   Status: nil/blank legacy rows coerce to \"completed\"; a `:running`
   row stays \"running\" — painting an unfinished turn as completed put
   a phantom '(completed)' bubble in the thread (orphaned rows are
   re-stamped :interrupted at startup, so 'running' never sticks)."
  [sid row]
  {:turn_id (str (:id row))
   :session_id (str sid)
   :status (let [s (some-> (:status row)
                           name)]
             (if (contains? #{nil ""} s) "completed" s))
   :request (:user-request row)
   :answer_md (:answer-markdown row)
   :iteration_count (:iteration-count row)
   :duration_ms (:duration-ms row)
   :tokens {:input (:input-tokens row) :output (:output-tokens row)}
   :cost (cond-> {:total-cost (:total-cost row)}
           (:model row)
           (assoc :model (:model row))

           (:provider row)
           (assoc :provider (:provider row)))
   :started_at (when-let [d (:created-at row)]
                 (when (instance? java.util.Date d) (.getTime ^java.util.Date d)))})

(defn list-turns
  "Wire views of every turn for `sid`, newest first: persisted history hydrated
  from the engine DB (survives daemon restarts), plus only genuinely live gateway
  overlay rows (running/queued or terminal rows not yet visible in persistence).

  DEDUP: the gateway's `tid` is NOT the engine's persisted row id - the engine
  mints its own id inside `send!`. Once the durable row is visible, prefer it:
  it owns the iteration trace. Keeping the completed gateway row alongside the
  persisted row rendered the last request/response twice after refresh, with the
  transient duplicate missing the iterations disclosure."
  [sid]
  (let [{:keys [turns turn-order]}
        (get @registry sid)

        live0
        (->> (or turn-order [])
             (keep #(some-> (get turns %)
                            wire-turn
                            (dissoc :answer_ir)))
             vec)

        run-start
        (some #(when (= "running" (:status %)) (long (or (:started_at %) 0))) live0)

        in-flight?
        (fn [row]
          (boolean (and run-start
                        (or (= :running (:status row))
                            (when-let [d (:created-at row)]
                              (and (instance? java.util.Date d)
                                   (>= (.getTime ^java.util.Date d) run-start)))))))

        persisted-rows
        (try (->> (persistance/db-list-session-turns (lp/db-info) sid)
                  (remove in-flight?)
                  vec)
             (catch Throwable t
               (tel/log! :warn ["gateway: turn-history hydration failed" (ex-message t)])
               []))

        live
        (->> live0
             (remove (fn [t]
                       (some #(persisted-duplicate-of-live? t %) persisted-rows)))
             vec)

        live-ids
        (into (set (map :turn_id live)) (keep :engine_turn_id live))

        att-by-soul
        (try (persistance/db-list-turns-attachments (lp/db-info) (map :id persisted-rows))
             (catch Throwable _ {}))

        persisted
        (->> persisted-rows
             (map (fn [row]
                    (let [wire (persisted-turn->wire sid row)]
                      (if-let [atts (seq (get att-by-soul (:turn_id wire)))]
                        (assoc wire :attachments atts)
                        wire))))
             (remove #(contains? live-ids (:turn_id %)))
             vec)]

    ;; persisted rows arrive oldest-first; the wire contract is
    ;; newest-first (the page reverses for display).
    (vec (concat (reverse live) (reverse persisted)))))

(defn transcript
  "Rich persisted transcript rows for `sid` in THE canonical wire shape
  (`wire/canonical`): turns oldest-first, each carrying its persisted iteration
  rows under `:iterations`. Canonicalizing AT THE SOURCE makes the HTTP hop an
  identity — an in-process reader and a remote gateway client (TUI / web /
  mobile) see the SAME maps, so there is exactly ONE transcript shape and a
  channel can never again be written against a shape only one transport sees."
  [sid]
  (try (let [db
             (lp/db-info)

             turns
             (persistance/db-list-session-turns db sid)

             att-by-soul
             (try (persistance/db-list-turns-attachments db (map :id turns))
                  (catch Throwable _ {}))]

         (wire/canonical
           (mapv (fn [turn]
                   (cond-> (assoc turn
                             :iterations
                             (try (->> (persistance/db-list-session-turn-iterations db (:id turn))
                                       (mapv #(update % :thinking normalize-thinking-text)))
                                  (catch Throwable t
                                    (tel/log! :warn
                                              ["gateway: turn-iteration hydration failed" (:id turn)
                                               (ex-message t)])
                                    [])))
                     (seq (get att-by-soul (str (:id turn))))
                     (assoc :attachments (get att-by-soul (str (:id turn))))))
                 turns)))
       (catch Throwable t
         (tel/log! :warn ["gateway: transcript hydration failed" (ex-message t)])
         [])))

(defn turn-trace
  "THE canonical wire trace of ONE persisted turn: its iteration rows (each
  with hydrated `:attachments` re-read from the attachment store) through
  `wire/canonical`, same as [[transcript]] — canonicalizing AT THE SOURCE
  keeps the HTTP hop an identity, so the in-process web channel and a remote
  client (TUI / mobile) render from the SAME maps. Returns a (possibly empty)
  vector for a valid turn id, nil for an unparsable id or a read failure —
  callers use nil to fall back / retry."
  [tid]
  (try (when-let [turn-id (some-> tid
                                  str
                                  parse-uuid)]
         (let [db (lp/db-info)
               iters (->> (persistance/db-list-session-turn-iterations db turn-id)
                          (mapv #(update % :thinking normalize-thinking-text)))
               atts-by-iter
               (when (seq iters)
                 (try (into {}
                            (map (fn [[iter-id rows]]
                                   [(str iter-id) (attachment-storage/hydrate-all rows)]))
                            (persistance/db-list-iterations-attachments db (keep :id iters)))
                      (catch Throwable _ {})))]

           (wire/canonical (mapv (fn [it]
                                   (if-let [atts (seq (get atts-by-iter (str (:id it))))]
                                     (assoc it :attachments (vec atts))
                                     it))
                                 iters))))
       (catch Throwable t
         (tel/log! :warn ["gateway: turn-trace hydration failed" tid (ex-message t)])
         nil)))

(defn reconcile-running-turns!
  "Gateway facade for startup/client resume reconciliation of orphaned running turns."
  []
  (try (lp/db-sweep-orphaned-running-turns!) (catch Throwable _ nil)))

(defn- finish-turn!
  [sid tid patch]
  (swap! registry update
    sid
    (fn [entry]
      (cond-> (update-in entry [:turns tid] merge patch)
        (= tid (:current-turn entry))
        (assoc :current-turn nil)))))

(defn- record-metrics!
  [sid {:keys [tokens cost duration-ms status]}]
  (let [input
        (long (or (:input tokens) 0))

        output
        (long (or (:output tokens) 0))

        cost-total
        (double (or (:total-cost cost) 0.0))

        duration
        (long (or duration-ms 0))

        failed?
        (contains? #{:error :cancelled} status)]

    (swap! metrics (fn [m]
                     (-> m
                         (update :turns-total inc)
                         (update :turns-failed (if failed? inc identity))
                         (update :tokens-input + input)
                         (update :tokens-output + output)
                         (update :cost-total + cost-total)
                         (update :duration-ms-total + duration)
                         (update-in
                           [:per-session (str sid)]
                           (fnil (fn [s]
                                   (-> s
                                       (update :turns inc)
                                       (update :tokens-input + input)
                                       (update :tokens-output + output)
                                       (update :cost-total + cost-total)))
                                 {:turns 0 :tokens-input 0 :tokens-output 0 :cost-total 0.0})))))))

;; =============================================================================
;; Turn execution
;; =============================================================================

(declare drain-next-queued!)

(defn- run-turn!
  "Worker body for one submitted turn. Streams phased chunks into the
  event log, runs the blocking `lp/send!`, then lands the terminal turn
  record + events. Never throws - a worker failure becomes a `failed`
  turn record and a `turn.failed` event."
  [sid tid request
   {:keys [messages model reasoning-default cancel-token extra-body turn-features workspace
           engine-opts attachments]}]
  (let [caller-on-chunk
        (get-in engine-opts [:hooks :on-chunk])

        ;; phase -> epoch-ms of the last transient delta emitted on the wire
        last-delta-ms
        (volatile! {})

        on-chunk
        (fn [chunk]
          (try (when caller-on-chunk
                 (try (caller-on-chunk chunk)
                      (catch Throwable t
                        (tel/log! :warn ["gateway: caller chunk hook failed" (ex-message t)]))))
               (let [phase
                     (:phase chunk)

                     now
                     (System/currentTimeMillis)]

                 (when-not (coalesce-delta? @last-delta-ms chunk now)
                   (when (contains? transient-delta-phases phase)
                     (vswap! last-delta-ms assoc phase now))
                   (let [[type store? payload] (chunk->event chunk)]
                     (append-event! sid type (assoc payload :turn_id tid) {:store? store?}))))
               (catch Throwable t
                 (tel/log! :warn ["gateway: chunk translation failed" (ex-message t)]))))]

    (try
      (let [opts
            (cond-> (assoc (or engine-opts {})
                      :hooks {:on-chunk on-chunk}
                      :cancel-token cancel-token)
              model
              (assoc :model model)

              reasoning-default
              (assoc :reasoning-default reasoning-default)

              extra-body
              (assoc :extra-body extra-body)

              turn-features
              (assoc :turn/features turn-features)

              (seq workspace)
              (merge workspace)

              (seq attachments)
              (assoc :user/attachments attachments))

            result
            (lp/send! sid (or messages request) opts)

            answer
            (:answer result)

            needs-input?
            (= :needs-input (:vis/answer-mode answer))

            ;; The engine returns a canonical IR AST (not markdown) for the
            ;; provider-error / fatal-iteration fallbacks. Carry it verbatim
            ;; as :answer_ir so the channel walks it through ir->hiccup; the
            ;; flattened plain text rides :answer_md as the lean fallback.
            ir-answer?
            (ir-ast-answer? answer)

            md
            (answer-md answer)

            answer-ir
            (if ir-answer? answer (when md (try (ir/markdown->ir md) (catch Throwable _ nil))))

            status
            (cond (= :cancelled (:status result)) "cancelled"
                  (= :error (:status result)) "failed"
                  needs-input? "suspended"
                  :else "completed")

            patch
            {:status status
             :answer_md md
             :answer_ir answer-ir
             :needs_input needs-input?
             ;; the ENGINE's persisted row id - list-turns dedups the
             ;; DB hydration against it (the gateway tid differs).
             :engine_turn_id (some-> (:session-turn-id result)
                                     str)
             :model (or (get-in result [:cost :model]) (:model result))
             :provider (or (get-in result [:cost :provider]) (:provider result))
             :llm_selected (:llm-selected result)
             :llm_actual (:llm-actual result)
             :llm_fallback (:llm-fallback? result)
             :llm_routing_trace (:llm-routing-trace result)
             :tokens (:tokens result)
             :cost (:cost result)
             :confidence (:confidence result)
             :iteration_count (:iteration-count result)
             :duration_ms (:duration-ms result)
             :utilization (:utilization result)
             :finished_at (System/currentTimeMillis)}]

        (finish-turn! sid tid patch)
        (record-metrics! sid result)
        (append-event! sid
                       (if (= status "failed") "turn.failed" "turn.completed")
                       ;; :answer_ir is normally dropped from the live event (markdown
                       ;; answers re-render client-side via `marked` off :answer_md). An
                       ;; engine IR-AST answer has no markdown twin, so it MUST ride the
                       ;; event for the channel to render anything but the lean fallback.
                       (-> patch
                           (cond->
                             (not ir-answer?)
                             (dissoc :answer_ir))
                           (assoc :turn_id tid)))
        (emit-context-updated! sid)
        ;; A user cancel means "stop", not "advance": do NOT auto-start the next
        ;; queued turn. Leaving the backlog queued lets the channel pull it back
        ;; into the editor (TUI) or keep it visible/editable (web) instead of
        ;; firing an uninterruptible follow-up the instant the cancel lands.
        (when-not (cancellation/cancelled? cancel-token) (drain-next-queued! sid)))
      (catch Throwable t
        (tel/log! :error ["gateway: turn worker failed" tid (ex-message t)])
        (finish-turn!
          sid
          tid
          {:status "failed" :error (ex-message t) :finished_at (System/currentTimeMillis)})
        (append-event! sid "turn.failed" {:turn_id tid :status "failed" :error (ex-message t)})
        (when-not (cancellation/cancelled? cancel-token) (drain-next-queued! sid))))))

(defn- launch-turn-worker!
  [sid tid request
   {:keys [messages model reasoning-default cancel-token queued? extra-body turn-features workspace
           engine-opts attachments]}]
  (append-event! sid
                 "turn.started"
                 ;; Carry the CANONICAL run-start clock (stamped into the registry
                 ;; row by submit/drain) so every attached channel seeds its
                 ;; elapsed timer from the ONE shared timestamp — a TUI's local
                 ;; submit/drain/attach stamp drifts from the actual run start.
                 (cond-> {:turn_id tid
                          :request request
                          :started_at (or (get-in @registry [sid :turns tid :started_at])
                                          (System/currentTimeMillis))}
                   queued?
                   (assoc :queued? true)))
  (cancellation/cancellation-set-future! cancel-token
                                         (cancellation/worker-future
                                           (str "gateway-turn-" tid)
                                           #(run-turn! sid
                                                       tid
                                                       request
                                                       {:messages messages
                                                        :model model
                                                        :reasoning-default reasoning-default
                                                        :cancel-token cancel-token
                                                        :extra-body extra-body
                                                        :turn-features turn-features
                                                        :workspace workspace
                                                        :engine-opts engine-opts
                                                        :attachments attachments}))))

(defn- first-queued-turn
  [entry]
  (some (fn [tid]
          (let [turn (get-in entry [:turns tid])]
            (when (= "queued" (:status turn)) [tid turn])))
        (:turn-order entry)))

(defn- replace-last-user-message-content
  "Return `messages` with the last user message content replaced by `text`.

  Queued web/API turns may carry both the display `:request` and provider
  `:messages`. Editing a queued prompt must update both; otherwise the queue
  drains with the old provider payload and appears to answer the previous ask."
  [messages text]
  (if (vector? messages)
    (if-let [idx (->> (map-indexed vector messages)
                      reverse
                      (some (fn [[i m]]
                              (when (contains? #{"user" :user} (:role m)) i))))]
      (assoc-in messages [idx :content] text)
      messages)
    messages))

(defn- drain-next-queued!
  "Start the oldest queued turn for `sid`, if one exists. Returns the started turn."
  [sid]
  (let [decision (volatile! nil)]
    (swap! registry update
      sid
      (fn [entry]
        (if (or (nil? entry) (:current-turn entry))
          entry
          (if-let [[tid
                    {:keys [request messages model reasoning-default cancel-token extra-body
                            turn-features workspace engine-opts attachments]}]
                   (first-queued-turn entry)]
            (let [token (or cancel-token (cancellation/cancellation-token))
                  started-at (System/currentTimeMillis)]

              (vreset! decision
                       {:tid tid
                        :request request
                        :messages messages
                        :model model
                        :reasoning-default reasoning-default
                        :cancel-token token
                        :extra-body extra-body
                        :turn-features turn-features
                        :workspace workspace
                        :engine-opts engine-opts
                        :attachments attachments})
              (-> entry
                  (assoc :current-turn tid
                         :last-active started-at)
                  (update-in [:turns tid]
                             merge
                             {:status "running" :cancel-token token :started_at started-at})))
            entry))))
    (when-let [{:keys [tid request messages model reasoning-default cancel-token extra-body
                       turn-features workspace engine-opts attachments]}
               @decision]
      ;; Queue-mirror signal: the queue head is no longer QUEUED. Every
      ;; attached channel drops its mirrored entry on this, and a replayed
      ;; event log nets to zero (turn.queued … turn.queued.drained). The
      ;; turn.started that follows carries :queued? true for attach flows.
      (append-event! sid "turn.queued.drained" {:turn_id tid})
      (launch-turn-worker! sid
                           tid
                           request
                           {:messages messages
                            :model model
                            :reasoning-default reasoning-default
                            :cancel-token cancel-token
                            :queued? true
                            :extra-body extra-body
                            :turn-features turn-features
                            :workspace workspace
                            :engine-opts engine-opts
                            :attachments attachments})
      (get-turn sid tid))))

(defn submit-turn!
  "Submit one turn for `sid`. Async: starts immediately when idle, otherwise queues.

   Returns `{:turn record}` (plus `:idempotent? true` on an idempotency
   replay) or `{:error :session-not-found | :invalid-request, ...}`. One engine
   turn still runs per session; busy submissions become visible queued records."
  [sid
   {:keys [request messages idempotency-key model reasoning-default cancel-token extra-body
           turn-features workspace engine-opts attachments]}]
  (cond
    (or (not (string? request)) (str/blank? request))
    {:error :invalid-request :message "request must be a non-blank string"}
    (nil? (lp/by-id sid)) {:error :session-not-found}
    :else
    (let [tid
          (str (java.util.UUID/randomUUID))

          ;; session pref is {:provider :model}; the engine routes by model name
          model
          (or model (:model (session-model sid)))

          decision
          (volatile! nil)]

      (swap! registry update
        sid
        (fn [entry]
          (let [entry (or entry {:next-seq 0})]
            (cond (and idempotency-key (get-in entry [:idempotency idempotency-key]))
                  (do (vreset! decision [:idempotent (get-in entry [:idempotency idempotency-key])])
                      entry)
                  (:current-turn entry) (do
                                          (vreset! decision [:queued tid])
                                          (let [queued-at (System/currentTimeMillis)]
                                            (-> entry
                                                (assoc :last-active queued-at)
                                                (assoc-in [:turns tid]
                                                          (cond-> {:turn_id tid
                                                                   :session_id (str sid)
                                                                   :status "queued"
                                                                   :request request
                                                                   :queued_at queued-at}
                                                            messages
                                                            (assoc :messages messages)

                                                            cancel-token
                                                            (assoc :cancel-token cancel-token)

                                                            extra-body
                                                            (assoc :extra-body extra-body)

                                                            turn-features
                                                            (assoc :turn-features turn-features)

                                                            (seq workspace)
                                                            (assoc :workspace workspace)

                                                            engine-opts
                                                            (assoc :engine-opts engine-opts)

                                                            model
                                                            (assoc :model model)

                                                            reasoning-default
                                                            (assoc :reasoning-default
                                                              reasoning-default)

                                                            (seq attachments)
                                                            (assoc :attachments attachments)))
                                                (update :turn-order (fnil conj []) tid)
                                                (cond->
                                                  idempotency-key
                                                  (assoc-in [:idempotency idempotency-key] tid)))))
                  :else (do (vreset! decision [:accepted tid])
                            (let [token (or cancel-token (cancellation/cancellation-token))
                                  started-at (System/currentTimeMillis)]

                              (-> entry
                                  (assoc :current-turn tid
                                         :last-active started-at)
                                  (assoc-in [:turns tid]
                                            (cond-> {:turn_id tid
                                                     :session_id (str sid)
                                                     :status "running"
                                                     :request request
                                                     :cancel-token token
                                                     :started_at started-at}
                                              model
                                              (assoc :model model)

                                              reasoning-default
                                              (assoc :reasoning-default reasoning-default)

                                              (seq attachments)
                                              (assoc :attachments attachments)))
                                  (update :turn-order (fnil conj []) tid)
                                  (cond->
                                    idempotency-key
                                    (assoc-in [:idempotency idempotency-key] tid)))))))))
      (let [[kind v] @decision]
        (case kind
          :idempotent
          {:turn (get-turn sid v) :idempotent? true}

          :queued
          (do (append-event! sid "turn.queued" {:turn_id tid :request request})
              {:turn (get-turn sid tid)})

          :accepted
          (let [turn (get-turn sid tid)]
            (launch-turn-worker! sid
                                 tid
                                 request
                                 {:messages messages
                                  :model model
                                  :reasoning-default reasoning-default
                                  :cancel-token (:cancel-token (get-in @registry [sid :turns tid]))
                                  :extra-body extra-body
                                  :turn-features turn-features
                                  :workspace workspace
                                  :engine-opts engine-opts
                                  :attachments attachments})
            {:turn turn}))))))

(defn- terminal-event->result
  "Build the engine-shaped blocking result shared by `submit-turn-sync!` and
   `attach-turn-sync!`, from a terminal `turn.completed`/`turn.failed` event OR an
   equivalent stored turn record. `fallback-turn-id` seeds `:session-turn-id`."
  [event fallback-turn-id]
  (let [failed?
        (or (= "turn.failed" (:type event)) (= "failed" (:status event)))

        cancelled?
        (= "cancelled" (:status event))

        needs-input?
        (or (true? (:needs_input event)) (= "suspended" (:status event)))

        answer
        (or (:answer event) (:answer_md event))]

    (cond-> {:answer answer
             :answer-ir (:answer_ir event)
             :iteration-count (or (:iteration_count event) 1)
             :duration-ms (:duration_ms event)
             :session-turn-id (or (:engine_turn_id event) fallback-turn-id)
             :utilization (:utilization event)}
      (:model event)
      (assoc :model (:model event))

      (:provider event)
      (assoc :provider (:provider event))

      (:llm_selected event)
      (assoc :llm-selected (:llm_selected event))

      (:llm_actual event)
      (assoc :llm-actual (:llm_actual event))

      (some? (:llm_fallback event))
      (assoc :llm-fallback? (:llm_fallback event))

      (seq (:llm_routing_trace event))
      (assoc :llm-routing-trace (:llm_routing_trace event))

      (:tokens event)
      (assoc :tokens (:tokens event))

      (:cost event)
      (assoc :cost (:cost event))

      (:confidence event)
      (assoc :confidence (:confidence event))

      needs-input?
      (assoc :status :needs-input)

      cancelled?
      (assoc :status :cancelled)

      failed?
      (assoc :error (or (:error event) (:answer_md event) "turn failed")))))

(def ^:private queue-mirror-event-types
  "Queue lifecycle events forwarded to a turn-scoped subscriber even though
   they belong to a DIFFERENT (queued) turn of the same session: every
   attached channel mirrors the gateway's queued backlog live from these,
   so a message queued in one TUI shows up in every sibling attached to
   the session (see the TUI's :sync-queued-turn). Canonical set:
   `wire/queue-mirror-event-types` — the SSE client (`gateway.client`)
   forwards the SAME set, so both transports stay in lockstep."
  wire/queue-mirror-event-types)

(defn submit-turn-sync!
  "Submit one turn through the gateway and block until that turn reaches a terminal event.

  Accepts the same request keys as `submit-turn!`; optional `:on-event` is called
  for every replay/live event for the submitted turn. Returns an engine-shaped
  result map for in-process clients (CLI/TUI/Telegram) that need a blocking
  call without bypassing the canonical gateway machinery."
  [sid {:keys [on-event] :as opts}]
  (let [sub-id
        (str "gateway-sync-" (java.util.UUID/randomUUID))

        started-cursor
        (current-seq sid)

        terminal
        (promise)

        submitted-turn-id
        (atom nil)

        handle-event!
        (fn [{:keys [type turn_id] :as event}]
          (cond (or (nil? @submitted-turn-id) (= turn_id @submitted-turn-id))
                (do (when on-event (on-event event))
                    (when (contains? #{"turn.completed" "turn.failed"} type)
                      (deliver terminal event))
                    ;; Our own queued record deleted before it ever ran
                    ;; (pulled back into a sibling's editor): synthesize a
                    ;; cancelled terminal so the blocking submit never hangs.
                    (when (and (= "turn.queued.deleted" type)
                               (some? @submitted-turn-id)
                               (= turn_id @submitted-turn-id))
                      (deliver terminal
                               {:type "turn.completed" :turn_id turn_id :status "cancelled"})))
                ;; ANOTHER turn's queue event: forward so the channel can
                ;; mirror the session's queued backlog; never terminal here.
                (contains? queue-mirror-event-types type) (when on-event (on-event event))))]

    (try (let [replay
               (subscribe! sid sub-id handle-event! started-cursor)

               submit-result
               (submit-turn! sid (dissoc opts :on-event))

               turn
               (:turn submit-result)

               turn-id
               (:turn_id turn)]

           (when-let [e (:error submit-result)]
             (throw (ex-info (or (:message submit-result) (str e)) submit-result)))
           (reset! submitted-turn-id turn-id)
           (doseq [event replay]
             (handle-event! event))
           (terminal-event->result (deref terminal) turn-id))
         (finally (unsubscribe! sid sub-id)))))
(defn attach-turn-sync!
  "Attach to an ALREADY-submitted turn `tid` on `sid` and block until it reaches a
   terminal event, returning the same engine-shaped result as `submit-turn-sync!`.

   Creates NO new turn: it drives in-process (TUI) rendering for a turn the gateway
   queued and then auto-drains, so a busy-time submission becomes a real gateway
   queued record instead of a client-side shadow queue. Optional `:on-event` fires
   for every replay/live event of `tid`."
  [sid tid {:keys [on-event]}]
  (let [sub-id
        (str "gateway-attach-" (java.util.UUID/randomUUID))

        started-cursor
        (current-seq sid)

        terminal
        (promise)

        handle-event!
        (fn [{:keys [type turn_id] :as event}]
          (cond (= turn_id tid)
                (do (when on-event (on-event event))
                    (when (contains? #{"turn.completed" "turn.failed"} type)
                      (deliver terminal event))
                    ;; The queued record was deleted before it ever ran
                    ;; (pulled back into a sibling's editor): synthesize a
                    ;; cancelled terminal so the attach never hangs.
                    (when (= "turn.queued.deleted" type)
                      (deliver terminal {:type "turn.completed" :turn_id tid :status "cancelled"})))
                ;; ANOTHER turn's queue event: forward so the channel can
                ;; mirror the session's queued backlog; never terminal here.
                (contains? queue-mirror-event-types type) (when on-event (on-event event))))]

    (try (let [replay (subscribe! sid sub-id handle-event! started-cursor)]
           (doseq [event replay]
             (handle-event! event))
           ;; A terminal that landed at/just-before our cursor (the gateway auto-drained
           ;; AND finished the turn before we attached) will not arrive as a live event.
           ;; Recover it from the stored record so we never block forever.
           (when-not (realized? terminal)
             (let [turn (get-turn sid tid)]
               (when (contains? terminal-turn-statuses (:status turn))
                 (deliver terminal
                          (assoc turn
                            :type
                            (if (= "failed" (:status turn)) "turn.failed" "turn.completed"))))))
           (terminal-event->result (deref terminal) tid))
         (finally (unsubscribe! sid sub-id)))))

(defn update-queued-turn!
  "Replace the prompt text for a queued turn. Returns the updated turn or an error."
  [sid tid request]
  (cond (or (not (string? request)) (str/blank? request))
        {:error :invalid-request :message "request must be a non-blank string"}
        :else (let [decision (volatile! nil)]
                (swap! registry update
                  sid
                  (fn [entry]
                    (let [turn (get-in entry [:turns tid])]
                      (cond (nil? turn) (do (vreset! decision [:missing]) entry)
                            (not= "queued" (:status turn))
                            (do (vreset! decision [:not-queued (:status turn)]) entry)
                            :else (do (vreset! decision [:updated])
                                      (-> entry
                                          (assoc-in [:turns tid :request] request)
                                          (update-in [:turns tid :messages]
                                                     replace-last-user-message-content
                                                     request)))))))
                (let [[kind status] @decision]
                  (case kind
                    :updated
                    (do (append-event! sid "turn.queued.updated" {:turn_id tid :request request})
                        {:turn (get-turn sid tid)})

                    :missing
                    {:error :turn-not-found}

                    :not-queued
                    {:error :not-queued :status status})))))

(defn delete-queued-turn!
  "Remove a queued turn before it starts. Returns deleted status or an error."
  [sid tid]
  (let [decision (volatile! nil)]
    (swap! registry update
      sid
      (fn [entry]
        (let [turn (get-in entry [:turns tid])]
          (cond (nil? turn) (do (vreset! decision [:missing]) entry)
                (not= "queued" (:status turn)) (do (vreset! decision [:not-queued (:status turn)])
                                                   entry)
                :else (do (vreset! decision [:deleted])
                          (-> entry
                              (update :turns dissoc tid)
                              (update :turn-order
                                      (fn [order]
                                        (vec (remove #{tid} order))))
                              (update :idempotency
                                      (fn [m]
                                        (into {} (remove (comp #{tid} val) m))))))))))
    (let [[kind status] @decision]
      (case kind
        :deleted
        (do (append-event! sid "turn.queued.deleted" {:turn_id tid}) {:status "deleted"})

        :missing
        {:error :turn-not-found}

        :not-queued
        {:error :not-queued :status status}))))

(defn cancel-turn!
  "Fire the cancellation token of a running turn. Returns
   `{:status \"cancelling\"}` or `{:error ...}`."
  [sid tid]
  (let [turn (get-in @registry [sid :turns tid])]
    (cond (nil? turn) {:error :turn-not-found}
          (not= "running" (:status turn)) {:error :not-running :status (:status turn)}
          :else (do (some-> (:cancel-token turn)
                            cancellation/cancel!)
                    {:status "cancelling"}))))

;; =============================================================================
;; Session lifecycle + souls
;; =============================================================================

(defn create-session!
  "Create a fresh gateway-managed session. Defaults to `:api`, but in-process
  clients such as the TUI can pass `:channel :tui` and still use the same
  gateway turn/event machinery without pretending to be an HTTP client."
  [{:keys [channel title external-id workspace-id root prewarm?]}]
  (let [channel
        (or channel :api)

        workspace-id
        (or workspace-id (when root (:id (workspace/create-trunk-at! (lp/db-info) root))))

        created
        (lp/create! channel
                    (cond-> {}
                      title
                      (assoc :title title)

                      external-id
                      (assoc :external-id external-id)

                      workspace-id
                      (assoc :workspace-id workspace-id)

                      prewarm?
                      (assoc :prewarm? true)))]

    (swap! registry assoc (:id created) {:next-seq 0 :last-active (System/currentTimeMillis)})
    {:id (str (:id created))
     :channel (name channel)
     :title (:title created)
     :external_id (:external-id created)
     :workspace_id (:workspace-id created)}))

(defn soul
  "Wire soul for one session: persisted record + live gateway status."
  [sid]
  (when-let [session (lp/by-id sid)]
    (let [entry (get @registry sid)
          last-turn (some->> (:turn-order entry)
                             peek
                             (get (:turns entry)))]

      {:id (str (:id session))
       :channel (some-> (:channel session)
                        name)
       :title (:title session)
       :model (:model session)
       :external_id (:external-id session)
       :created_at (:created-at session)
       :status (cond (:current-turn entry) "running"
                     (= "suspended" (:status last-turn)) "suspended"
                     :else "idle")
       :current_turn_id (:current-turn entry)
       :last_active_at (:last-active entry)})))

(defn list-sessions
  "Wire souls for every persisted session.

   CROSS-CHANNEL by default (`channel` = `:all`): a conversation started
   in the web is visible in the TUI and vice-versa. Pass a specific
   channel keyword only when a caller genuinely needs a single-channel
   slice (e.g. Telegram resolving a chat by external-id)."
  ([] (list-sessions :all))
  ([channel]
   (->> (lp/by-channel channel)
        (keep (comp soul :id))
        vec)))

(defn release-session!
  "Release the live runtime for a session while keeping persisted data resumable.

   This is the gateway facade for local clients that are merely closing a view
   (for example a TUI tab or process exit). Use `close-session!` for DELETE.

   Background resources (shell_bg processes, managed REPLs) are STOPPED here:
   closing the view is the user walking away, and a bg child must not outlive
   that — the transcript stays resumable, the processes do not."
  [sid]
  (try (resources/stop-all! sid) (catch Throwable _ nil))
  (try (lp/close! sid) (catch Throwable _ nil))
  nil)

(defn close-session!
  "DELETE a session: dispose the live environment, trash the session's draft
   clones (primary + auto-cloned filesystem roots — only DRAFTS have clones; a
   trunk workspace's roots are the user's real dirs and are never touched),
   then delete the session tree. Idempotent. NOTE: this is the DELETE path —
   merely quitting/closing a session (navigating away, no server call) keeps
   the draft intact so it can be resumed."
  [sid]
  (try (resources/stop-all! sid) (catch Throwable _ nil))
  (try (lp/close! sid) (catch Throwable _ nil))
  ;; trash on-disk clones BEFORE the DB tree (delete) so the workspace row is
  ;; still resolvable; draft-only, so this can never delete a real directory.
  (try (workspace/discard-session-clones! (lp/db-info) sid) (catch Throwable _ nil))
  (try (lp/delete! sid) (catch Throwable _ nil))
  (swap! registry dissoc sid)
  (bus/forget! sid)
  nil)

(defn set-title! [sid title] (when (lp/by-id sid) (lp/set-title! sid title) (soul sid)))

(defn- broadcast-title-event!
  "Append a `session.title_updated` event for `sid` (stored, so a cursor
   replay re-delivers it) and STORE a copy on every OTHER registered
   session - a client watching session B sees session A's auto-generated
   title land without re-opening A.

   STORED, not live-only, and to every registered session rather than only
   ones with a live SSE subscriber: both are required so the transport is
   transparent. `/poll` reads the replay ring (`events-since`), and a poll
   client never registers as a subscriber - a live-only copy gated on
   `:subscribers` was invisible to it, so a client on the poll fallback (an
   edge proxy buffering the SSE stream) silently missed sibling-title
   updates the SSE client received. Now SSE and poll deliver the identical
   frame.

   The foreign copy keeps the TITLED session's id in `:session_id` (the
   payload wins over the default stamp) while riding each session's own
   monotonic seq, so the per-connection last-seq guard stays sound.
   Title generation is once-per-session, so the extra ring writes are
   negligible and stay bounded by the ring trim; idle sessions never replay
   it (a page renders at the current seq, so a pre-render foreign event sits
   below the cursor)."
  [sid title]
  (append-event! sid "session.title_updated" {:title (str title)})
  (doseq [other
          (keys @registry)

          :when (not= other sid)]

    (append-event! other "session.title_updated" {:session_id (str sid) :title (str title)})))

(defonce bus-wiring
  ;; Wire the cross-process bus ONCE at namespace load: foreign events tailed
  ;; from sibling processes flow into `ingest-mirrored-event!`, and the
  ;; background tailer starts. Every process that touches the gateway (web,
  ;; TUI, Telegram, the `serve` daemon) both publishes and consumes.
  (do
    ;; pass the VAR so a dev-time ns reload is picked up without re-wiring.
    (bus/set-deliver-fn! #'ingest-mirrored-event!)
    ;; Skip the tailer thread during native-image BUILD: graal InitClojureClasses
    ;; runs this ns-load at build time, and a started thread cannot be baked into
    ;; the image heap. On a normal JVM this guard is false so the tailer starts at
    ;; load exactly as before; the native RUNTIME starts it lazily on first
    ;; bus/publish! (see bus/publish!).
    (when-not (= "buildtime" (System/getProperty "org.graalvm.nativeimage.imagecode")) (bus/start!))
    true))

(defonce title-listener
  ;; Registered ONCE at namespace load: loop.clj's single title mutation
  ;; point (`set-title-with-broadcast!`) fires this for host renames
  ;; and auto-title generation alike, so
  ;; every title change becomes a `session.title_updated` SSE event.
  (titling/add-global-title-listener! #'broadcast-title-event!))

(defn metrics-snapshot
  "Global + per-session counters for /metrics."
  []
  (let [reg @registry]
    (assoc @metrics
      :sessions-tracked (count reg)
      :turns-running (count (keep :current-turn (vals reg))))))

(defn warm-db!
  "Force the persistence backend + shared connection on the CALLER's
   thread. The gateway runs this on its single-threaded boot path so
   the heavyweight backend namespace never lazy-loads under request
   concurrency (see require-backend-ns! in internal/persistance.clj)."
  []
  (try (lp/db-info)
       true
       (catch Throwable t (tel/log! :warn ["gateway: db warmup failed" (ex-message t)]) false)))
