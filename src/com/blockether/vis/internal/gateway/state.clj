(ns com.blockether.vis.internal.gateway.state
  "Gateway session manager.

   One process-global registry over the live session fleet: per-session
   ordered event log (monotonic `:seq`, ring-buffered), SSE subscriber
   fan-out, async turn submission with idempotency keys, cancellation,
   and turn/cost metrics.

   The engine is reached ONLY through the same internal surfaces the
   TUI channel uses: `loop/create!`-`send!`-`close!` for the
   lifecycle, `:hooks {:on-chunk ...}` phased chunks for the live
   stream, `ctx-loop/session-snapshot` for the context. No engine state
   lives here - this namespace owns wire bookkeeping (events, turn
   records, subscribers), nothing else."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.attachment-storage :as attachment-storage]
            [com.blockether.vis.internal.cancellation :as cancellation]
            [com.blockether.vis.internal.content :as content]
            [com.blockether.vis.internal.form :as form]
            [com.blockether.vis.internal.git :as git]
            [com.blockether.vis.internal.session-model :as smodel]
            [com.blockether.vis.internal.ctx-loop :as ctx-loop]
            [com.blockether.vis.internal.gateway.bus :as bus]
            [com.blockether.vis.internal.gateway.wire :as wire]
            [com.blockether.vis.internal.loop :as lp]
            [com.blockether.vis.internal.titling :as titling]
            [com.blockether.vis.internal.persistance :as persistance]
            [com.blockether.vis.internal.provider-error :as provider-error]
            [com.blockether.vis.internal.resources :as resources]
            [com.blockether.vis.internal.workspace :as workspace]
            [taoensso.telemere :as tel]))

(def ^:private EVENT_RING_MAX
  "Per-session event-log ring size. Older events stay durable in the
   session transcript; the ring only backs SSE cursor replay."
  10000)

(def ^:private RESULT_PR_LIMIT 4000)
(def ^:private ERROR_PR_LIMIT 2000)
(def ^:private STREAM_CUMULATIVE_LIMIT
  "Bound for the cumulative text a live reasoning/content/prose frame carries
   alongside its increment (`:cumulative`) — plenty for any live band; the
   boundary `iteration.completed` still ships the complete text."
  16000)

;; Live reasoning/content/prose deltas arrive per provider token. Each emitted
;; wire frame carries the INCREMENT since the last emit under `:text` (append
;; consumers, e.g. the web prose stream) PLUS the bounded FULL cumulative text
;; under `:cumulative` (replace consumers — the web thinking ticker, the TUI
;; live bands — repaint from it, so a skipped frame is subsumed by the next
;; one). Streaming one frame per token is O(n²) over a long stream AND reads as
;; jittery token-vomit; instead COALESCE to SENTENCE granularity: emit when a
;; sentence just closed, with a time cap so even a long sentence still ticks.
;; `:done?` frames always pass so the final state lands.
(def ^:private DELTA_TIME_CAP_MS
  "Backstop for the sentence-coalesced live stream: even mid-sentence, a long
   reasoning/prose run still ticks at least this often so the user sees motion."
  2000)

(def ^:private streaming-text-phases
  "Model-generated text (reasoning/content/prose) streamed LIVE over the gateway,
   coalesced to sentence granularity (see `coalesce-delta?`). Thinking rides
   `reasoning.delta`; provider content + end-of-iteration prose ride
   `content.delta` (prose flagged `:prose-final`) — DISTINCT wire events so a
   client paints live thinking and live prose as SEPARATE blocks."
  #{:reasoning :content :assistant-prose})

(defn- delta-text
  "The cumulative text a streaming reasoning/content/prose chunk carries — the
   per-phase key the loop fills (`:thinking` / `:content` / `:text`)."
  [{:keys [phase thinking content text]}]
  (case phase
    :reasoning
    thinking

    :content
    content

    :assistant-prose
    text

    nil))

(defn- sentence-closed-in-suffix?
  "True when the newly-arrived tail (chars past `prev-len` of the cumulative
   text) CLOSES a sentence — `. ! ? …` (plus any trailing quotes/brackets) at a
   whitespace/end, or a newline. This flushes the live stream one COMPLETE
   sentence at a time instead of per token."
  [text ^long prev-len]
  (let
    [s
     (str text)

     n
     (count s)

     tail
     (if (and (pos? prev-len) (<= prev-len n)) (subs s prev-len) s)]

    (boolean (re-find #"[.!?…][\"')\]]*(?:\s|$)|\n" tail))))

(defn- coalesce-delta?
  "True when this transient reasoning/content/prose delta should be SKIPPED on
   the wire: still mid-sentence AND within `DELTA_TIME_CAP_MS` of the last emit,
   so a fresher frame follows. A just-closed sentence, the time cap,
   `:done?` frames, and every non-streaming phase all pass.
   `last-emit` is [phase iteration] -> {:ms emit-epoch :len emitted-text-length}
   — keyed PER ITERATION so a fresh iteration's stream never inherits the
   previous iteration's emitted length (which swallowed its first frames)."
  [last-emit {:keys [phase done? iteration] :as chunk} now]
  (and (contains? streaming-text-phases phase)
       (not done?)
       (let [{:keys [ms len] :or {len 0}} (get last-emit [phase (long (or iteration 0))])]
         ;; No prior record => FIRST frame of this phase+iteration: always emit.
         (and (some? ms)
              (< (- (long now) (long ms)) (long DELTA_TIME_CAP_MS))
              (not (sentence-closed-in-suffix? (delta-text chunk) len))))))

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
    (if (> n (long EVENT_RING_MAX)) (subvec events (- n (long EVENT_RING_MAX))) events)))

(defn- fan-out!
  "Deliver `event` to every local SSE sink for `sid`. Runs on the APPENDING
   (turn) thread, so sinks must be NON-BLOCKING — server.clj registers a
   bounded-queue enqueue (`sse-sink`), never a raw socket write. A sink that
   throws is dropped - one dead connection must never poison the appender or
   siblings."
  [sid event]
  (doseq [[sub-id sink] (get-in @registry [sid :subscribers])]
    (try (sink event)
         (catch Throwable t
           (swap! registry update-in [sid :subscribers] dissoc sub-id)
           (tel/log! :debug ["gateway: dropped dead subscriber" sub-id (ex-message t)])))))

(defn- fresh-entry
  "A brand-new registry entry for `sid`, seeding `:next-seq` from the journal's
   high-water so a RESTARTED daemon keeps numbering ABOVE a client's stale
   monotonic replay cursor instead of resetting to 0 (which the client's
   `seq > cursor` filter would silently drop — dropping a whole reconnect turn,
   the orphan-reap terminal included)."
  [sid]
  {:next-seq (bus/journal-high-water-seq sid)})

(defn append-event!
  "Append one event for `sid`, fan it out to LOCAL subscribers, and publish
   it on the cross-process bus so watchers in OTHER processes stream it too.

   The event is normalized to THE canonical wire shape (`wire/canonical`:
   snake_case STRING keys) BEFORE it is stored/fanned/published, so every
   consumer — in-process sink, replay ring, `/poll`, SSE, journal tail —
   reads the IDENTICAL string-keyed map.

   Assigns the next monotonic `\"seq\"` atomically. `:store? false` events
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
         (let
           [entry (or entry (fresh-entry sid))
            n (inc (long (:next-seq entry 0)))
            event
            (wire/canonical
              (merge
                {:schema 1 :seq n :ts (System/currentTimeMillis) :session_id (str sid) :type type}
                payload))]

           (vreset! captured event)
           (cond->
             (assoc entry
               :next-seq n
               :last-active (System/currentTimeMillis))
             (and (= type "turn.started") (get-in entry [:turns (:turn_id payload)]))
             (assoc-in [:turns (:turn_id payload) :event_start_seq] n)

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
   the cross-process bus, already in the canonical string-keyed wire shape)
   into THIS process's registry so a TUI watcher streams a
   turn running elsewhere in real time.

   The foreign event is RE-SEQUENCED onto this process's OWN monotonic `\"seq\"`,
   never the producer's. Each process runs an independent seq counter, but the
   SSE wire treats `\"seq\"` as a single strictly-increasing per-connection cursor;
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
    (let
      [type
       (get event "type")

       tid
       (get event "turn_id")

       terminal?
       (contains? #{"turn.completed" "turn.failed"} type)

       ;; The registry's internal turn records are keyword-keyed engine
       ;; state, so the string-keyed wire event is re-keyed at THIS ingress
       ;; (the one place foreign wire data meets internal records).
       term-patch
       (-> (into {}
                 (map (fn [[k v]]
                        [(keyword k) v]))
                 (dissoc event "type" "seq" "turn_id"))
           (assoc :status (or (get event "status")
                              (if (= type "turn.failed") "failed" "completed"))))

       captured
       (volatile! nil)]

      (swap! registry update
        sid
        (fn [entry]
          (if entry
            (let
              [n
               (inc (long (:next-seq entry 0)))

               ev
               (assoc event "seq" n)]

              (vreset! captured ev)
              (cond->
                (assoc entry
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
                               :request (get event "request")
                               :event_start_seq n
                               ;; Adopt the PRODUCER's canonical run-start
                               ;; clock — stamping mirror-local time here made
                               ;; a watcher in another process show a
                               ;; different elapsed than the producer.
                               :started_at (or (get event "started_at")
                                               (System/currentTimeMillis))})
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
  "Register an SSE sink and return the replay vector (canonical string-keyed
   events with `\"seq\"` > `cursor`) ATOMICALLY with the registration, so no
   event can fall between replay and live fan-out. The sink must be
   NON-BLOCKING (fan-out runs on the appending turn thread); the caller
   dedups via a seq guard, since a live event may land in both the replay
   and the sink (see server.clj).

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
      (or entry (fresh-entry sid))))
  (when-not (:current-turn (get @registry sid)) (bus/hydrate! sid))
  (let [replay (volatile! [])]
    (swap! registry update
      sid
      (fn [entry]
        (let [entry (or entry (fresh-entry sid))]
          (vreset! replay (filterv #(> (long (get % "seq")) (long (or cursor 0))) (:events entry)))
          (assoc-in entry [:subscribers sub-id] sink))))
    @replay))

(defn unsubscribe! [sid sub-id] (swap! registry update-in [sid :subscribers] dissoc sub-id) nil)

(defn current-seq
  "Highest event `:seq` assigned for `sid` so far. Subscribing with this
   as the cursor yields a live-only stream (empty replay)."
  [sid]
  (get-in @registry [sid :next-seq] 0))

(defn events-since
  "Read-only peek at the replay ring: stored canonical (string-keyed) events
   with `\"seq\"` > cursor, oldest first. Lets a page renderer locate the
   running turn's `turn.started` seq so its SSE reconnect can replay the WHOLE
   in-flight turn instead of only what happens after connect."
  [sid cursor]
  (filterv #(> (long (get % "seq")) (long (or cursor 0))) (get-in @registry [sid :events] [])))

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

(defn- session-state-id
  "Latest persisted state id for soul `sid`, or nil."
  [db sid]
  (persistance/db-latest-session-state-id db (str sid)))

(defn- live-env
  "The session's LIVE env, or nil. Guarded by `lp/by-id` so a bogus sid never
   spawns a session (this is consulted from hot render paths)."
  [sid]
  (try (when (lp/by-id sid) (lp/env-for sid)) (catch Throwable _ nil)))

(defn- resolve-state-id
  "Latest session_state id for soul `sid`, falling back to the id stamped on the
   LIVE env for a freshly-created session whose row the DB re-query can't see
   yet (the create-environment race loop.clj guards the same way). nil when the
   session is unknown."
  [db sid]
  (or (session-state-id db sid) (:session/state-id (live-env sid))))

(defn- resolve-workspace
  "Workspace record pinned to soul `sid`: latest session_state -> workspace,
   falling back to the LIVE env's pinned workspace (which create-environment
   always mints) when the session_state re-query hasn't settled. nil when the
   session is unknown. This is why adding a filesystem root works even before
   the session's first turn."
  [db sid]
  (or (some->> (session-state-id db sid)
               (workspace/for-session db))
      (some->> (:workspace/id (live-env sid))
               (persistance/db-workspace-get db))))

(defn session-workspace-info
  "Workspace state for a channel surface (the web footer AND the TUI
   directory picker), in THE canonical string-keyed wire shape:
   `{\"id\" \"draft?\" \"root\" \"repo_root\" \"label\" \"fork_ms\"
   \"filesystem_roots\" \"git\"}` for the session pinned to `sid` (soul id), or
   nil. `\"id\"` is the workspace-id every filesystem-root mutation
   (`add/remove-filesystem-root!`) needs — WITHOUT it the TUI picker treats the
   session as read-only and C-a silently no-ops. `\"filesystem_roots\"` is the
   the PUBLIC element shape `[{\"dir\" \"isolated\" \"draft_dir\"}]`. Lets the footer announce
   that the session — and its extra roots — are isolated drafts. Resolves
   soul → latest state → workspace; never throws."
  [sid]
  (try (when-let [db (lp/db-info)]
         (when-let [ws (resolve-workspace db sid)]
           (wire/canonical {:id (:id ws)
                            :draft? (workspace/draft? ws)
                            :root (:root ws)
                            :repo-root (:repo-root ws)
                            :label (:label ws)
                            :fork-ms (:fork-ms ws)
                            :filesystem-roots (mapv #(workspace/public-filesystem-root % true)
                                                    (workspace/filesystem-roots ws))
                            ;; Git working-tree status resolved HERE, in the gateway/daemon
                            ;; that owns the repo on disk — streamed to channels as a cached
                            ;; session fact instead of each client re-walking git locally (a
                            ;; remote TUI has no access to the repo's filesystem, and even
                            ;; colocated it stops every tab switch from recomputing). Cached
                            ;; per repo root, so repeated fetches never re-walk a warm root.
                            :git (git/workspace-status (:root ws))})))
       (catch Throwable _ nil)))

(defn add-filesystem-root!
  "Add `path` as an extra filesystem root for the session pinned to `sid`, then
   return the refreshed `session-workspace-info`. Runs SERVER-SIDE in the daemon
   so the draft backend-fork and DB write land where the session actually lives;
   every channel (web footer, TUI picker/footer) then reads the same roots back
   over the gateway. Channel-agnostic twin of `set-session-model!`."
  [sid path]
  (when-let [db (lp/db-info)]
    (when-let [ws (resolve-workspace db sid)]
      (workspace/add-filesystem-root! db (:id ws) path)))
  (session-workspace-info sid))

(defn remove-filesystem-root!
  "Remove `path` from the session's extra filesystem roots and return the
   refreshed `session-workspace-info`. Server-side twin of `add-filesystem-root!`."
  [sid path]
  (when-let [db (lp/db-info)]
    (when-let [ws (resolve-workspace db sid)]
      (workspace/remove-filesystem-root! db (:id ws) path)))
  (session-workspace-info sid))

(defn change-root!
  "Repoint the session pinned to `sid` at `path` as its PRIMARY root, then return
   the refreshed `session-workspace-info` (whose `:id` is the newly pinned
   workspace). Server-side so the change lands in the daemon that runs the turns."
  [sid path]
  (when-let [db (lp/db-info)]
    (when-let [state-id (resolve-state-id db sid)]
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
    (let
      [msg
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
  (when-let
    [s (some-> text
               str)]
    (not-empty (-> s
                   (str/replace #"[ \t\r\f\v]+\r?\n" "\n")
                   (str/replace #"(?:\r?\n){2,}" "\n")
                   str/trim))))

(defn iteration-attachments
  "Ordered OUTBOUND artifacts (matplotlib figures / produced images) a tool call
   persisted under iteration `iid`, in the `db-list-iteration-attachments` shape
   (each hydrated with inline `:base64` or an external `:storage-uri`), or `[]`.
   THE canonical, ordered list the attachment byte endpoint indexes AND the live
   `iteration.completed` descriptors mirror — so index N always names the same
   artifact live, on re-fetch, and across a restart. nil/unparsable id -> `[]`."
  [iid]
  (try (if-let
         [iid (some-> iid
                      str
                      parse-uuid)]
         (vec (persistance/db-list-iteration-attachments (lp/db-info) iid))
         [])
       (catch Throwable t
         (tel/log! :warn ["gateway: iteration-attachments read failed" (str iid) (ex-message t)])
         [])))

(defn attachment-bytes
  "Raw bytes for ONE attachment map (an [[iteration-attachments]] element): its
   inline `:base64` decoded, else its external `:storage-uri` fetched through the
   storage rail. nil when neither resolves."
  ^bytes [{:keys [base64 storage-uri]}]
  (cond (some? base64) (.decode (java.util.Base64/getDecoder) ^String base64)
        (some? storage-uri) (attachment-storage/resolve-bytes storage-uri)
        :else nil))

(defn- live-attachment-descriptors
  "Lean wire descriptors — metadata ONLY, NEVER base64 — for the artifacts
   iteration `iteration-id` persisted, so a native client (iOS/RN) learns 'image
   N produced' on the LIVE `iteration.completed` frame and lazy-fetches the bytes
   from `GET /v1/sessions/:sid/iterations/:iid/attachments/:idx` rather than
   bloating every SSE frame with 100s of KB. `:index` is the position in
   [[iteration-attachments]] — the EXACT list (and order) that byte endpoint
   serves — so a persist-skipped artifact is absent from both and index N always
   agrees. `[]` on any read failure."
  [iteration-id]
  (into []
        (map-indexed (fn [idx {:keys [tool-call-id kind media-type filename size]}]
                       {:index idx
                        :iteration_id (str iteration-id)
                        :tool_call_id tool-call-id
                        :kind (or kind "image")
                        :media_type (str (or media-type "application/octet-stream"))
                        :filename filename
                        :size (long (or size 0))}))
        (iteration-attachments iteration-id)))


(def ^:private activity-phases
  "Coarse 'Vis is doing X' phases surfaced to the LIVE ticker but never pinned
   into the durable trace: a provider wait, response parsing, and shell/tool
   calls (incl. a nested `shell_run` inside python_execution) that would
   otherwise leave the bubble frozen for the whole call."
  #{:provider-call :response-parse :shell-run :shell-bg :tool-start})

(defn- activity-chunk->event
  "Ephemeral `activity` wire event `[type store? payload]` for a coarse-progress
   phase, or nil. store? is false: channels paint a spinner label; nothing
   persists. `:response-parse :done` clears (emits nil) — the parse finished."
  [{:keys [phase cmd iteration] :as chunk}]
  (when (and (activity-phases phase)
             (not (and (= phase :response-parse) (= :done (:status chunk)))))
    (let
      [op
       (some-> (:op (:tool-event chunk))
               name)

       activity
       (if (= phase :tool-start) "tool" (name phase))]

      ["activity" false
       (cond-> {:activity activity}
         (some? iteration)
         (assoc :iteration iteration)

         (some? cmd)
         (assoc :cmd (str cmd))

         (some? op)
         (assoc :op op))])))

(defn- chunk->event
  "Translate one phased iteration chunk (progress.clj contract) into a
   `[type store? payload]` wire event triple, or nil. Model text phases
   (reasoning/content/prose) stream LIVE \u2014 the caller coalesces them to sentence
   granularity \u2014 as TRANSIENT (`store? false`) `reasoning.delta` / `content.delta`
   frames; the iteration boundary still ships the complete text on
   `iteration.completed`, which is what persists."
  [{:keys [phase position code result error silent? done? iteration thinking assistant-prose
           iteration-id attachment-count stream-delta stream-block-id]
    :as chunk}]
  ;; Every streaming chunk carries its iteration POSITION under `:iteration`.
  ;; It MUST ride the wire event, or `make-progress-tracker` silently DROPS the
  ;; chunk (it skips chunks with no iteration) — which is how `block.started` /
  ;; `block.output` once lost their forms.
  (or
    (activity-chunk->event chunk)
    (let
      [payload
       (case phase
         :form-start
         (merge
           ;; Carry the native-tool badge identity so a client can hide the
           ;; redundant invocation code WHILE the tool runs.
           (form/->display chunk)
           {:block_id position :code code})

         :form-result
         (merge
           ;; The native-tool op-card fields (pre-rendered card + badge label
           ;; + colour) — projected from ONE canonical list.
           (form/->display chunk)
           {:block_id position
            :code code
            :result result
            :stdout (when-let [s (:stdout chunk)]
                      (wire/bounded-str s RESULT_PR_LIMIT))
            :error (when (some? error) (wire/bounded-str (error->wire-text error) ERROR_PR_LIMIT))
            :silent (boolean (or silent? (and (nil? error) (contains? #{"vis_silent"} result))))
            :duration_ms (let [{:keys [started-at-ms finished-at-ms]} (:envelope chunk)]
                           (when (and (nat-int? started-at-ms) (nat-int? finished-at-ms))
                             (max 0 (- (long finished-at-ms) (long started-at-ms)))))})

         ;; Live thinking, on its OWN wire event so a client paints it as the
         ;; thinking trace — distinct from prose. `:text` is the INCREMENT
         ;; since the last emit; `:cumulative` is the bounded full text for
         ;; replace-style consumers (web ticker, TUI live bands).
         :reasoning
         {:block_id stream-block-id
          :field "text"
          :text (or stream-delta "")
          :cumulative (wire/bounded-str (str (delta-text chunk)) STREAM_CUMULATIVE_LIMIT)}

         ;; Live provider Markdown appends to the canonical prose block.
         :content
         {:block_id stream-block-id
          :field "markdown"
          :text (or stream-delta "")
          :cumulative (wire/bounded-str (str (delta-text chunk)) STREAM_CUMULATIVE_LIMIT)}

         :assistant-prose
         {:block_id stream-block-id
          :field "markdown"
          :text (or stream-delta "")
          :cumulative (wire/bounded-str (str (delta-text chunk)) STREAM_CUMULATIVE_LIMIT)}

         ;; The iteration's complete reasoning + complete assistant prose ride
         ;; the boundary event too — the canonical, PERSISTED final text.
         :iteration-final
         (cond-> {:done (boolean done?) :thinking (normalize-thinking-text thinking)}
           (some-> assistant-prose
                   str
                   str/trim
                   not-empty)
           (assoc :assistant-prose (str/trim (str assistant-prose)))

           (and iteration-id (pos? (long (or attachment-count 0))))
           (assoc :attachments (live-attachment-descriptors iteration-id)))

         :iteration-error
         ;; Carry the SAME canonical provider-error map the final settled turn
         ;; bubble paints the styled CARD from (`provider-error-info` →
         ;; `:vis/provider-error-data`).
         (cond->
           {:error (when (some? error) (wire/bounded-str (error->wire-text error) ERROR_PR_LIMIT))
            :thinking (normalize-thinking-text thinking)}
           (some? error)
           (assoc :provider-error-data (provider-error/provider-error-info error)))

         {:detail (wire/bounded-pr (dissoc chunk :phase) ERROR_PR_LIMIT)})]
      [(case phase
         :form-start
         "block.started"

         :form-result
         "block.output"

         (:reasoning :content :assistant-prose)
         "content.block.delta"

         :iteration-final
         "iteration.completed"

         :iteration-error
         "iteration.error"

         :provider-retry-reset
         "provider.retry"

         (str "chunk." (name phase)))
       ;; Block deltas are replayable: reconnect applies the same ordered event
       ;; sequence instead of reconstructing text from renderer state.
       true
       (cond-> payload
         (some? iteration)
         (assoc :iteration iteration))])))

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

(defn- answer-content
  "Normalize the engine's final answer into canonical typed content blocks."
  [answer]
  (content/answer-content answer))



(defn- wire-turn
  [turn]
  (when turn
    (let [started-at (or (:created_at turn) (:started_at turn) (:queued_at turn))]
      (-> turn
          (dissoc :cancel-token)
          (assoc :id (or (:id turn) (:turn_id turn))
                 :role (or (:role turn) "assistant")
                 :content (vec (or (:content turn) []))
                 :created_at started-at)
          (update :status #(if (= "running" %) "streaming" %))))))

(def ^:private terminal-turn-statuses #{"completed" "failed" "cancelled" "suspended" "error"})

(defn- date->ms [d] (when (instance? java.util.Date d) (.getTime ^java.util.Date d)))

(defn- persisted-duplicate-of-live?
  "True when persisted engine row `row` is the durable copy of gateway live row
  `live`. Prefer the persisted row on hydration: it owns the DB iteration trace,
  while the completed gateway row is only the transient SSE record. The primary
  key is :engine_turn_id; the fallback covers terminal turns that finished before
  the gateway learned/cached that engine id."
  [live row]
  (let
    [engine-id
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
             (or (= (:content live) (:content row)) (empty? (:content live)))
             (if-let [created (date->ms (:created-at row))]
               (>= (long created) (long (or (:started_at live) 0)))
               true)))))

(defn get-turn
  "Canonical (string-keyed) wire view of one turn record, or nil."
  [sid tid]
  (some-> (wire-turn (get-in @registry [sid :turns tid]))
          wire/canonical))

(defn- persisted-turn->wire
  "Project one durable engine turn into the canonical role/content message shape."
  [sid row]
  (let
    [id
     (str (:id row))

     status
     (case
       (some-> (:status row)
               name)
       (nil "" "success" "done")
       "completed"

       "interrupted"
       "cancelled"

       "error"
       "failed"

       "running"
       "streaming"

       (some-> (:status row)
               name))

     created-at
     (some-> (:created-at row)
             date->ms)]

    {:id id
     :turn_id id
     :session_id (str sid)
     :role "assistant"
     :status status
     :request (:user-request row)
     :content (vec (or (:content row) []))
     :iteration_count (:iteration-count row)
     :duration_ms (:duration-ms row)
     :tokens {"input" (:input-tokens row)
              "input_regular" (:input-regular-tokens row)
              "cache_created" (:input-cache-write-tokens row)
              "cached" (:input-cache-read-tokens row)
              "output" (:output-tokens row)
              "reasoning" (:output-reasoning-tokens row)}
     :cost (cond-> {"total_cost" (:total-cost row)}
             (:model row)
             (assoc "model" (:model row))

             (:provider row)
             (assoc "provider" (:provider row)))
     :created_at created-at
     :completed_at (when (and created-at (:duration-ms row))
                     (+ created-at (long (:duration-ms row))))}))

(defn list-turns
  "Canonical (string-keyed) wire views of every turn for `sid`, OLDEST-first (chronological, chat order):
  persisted history hydrated from the engine DB (survives daemon restarts), plus
  only genuinely live gateway overlay rows (running/queued or terminal rows not
  yet visible in persistence). Ordering matches `transcript` so every consumer
  renders top-to-bottom directly — no channel re-reverses for display.

  DEDUP: the gateway's `tid` is NOT the engine's persisted row id - the engine
  mints its own id inside `send!`. Once the durable row is visible, prefer it:
  it owns the iteration trace. Keeping the completed gateway row alongside the
  persisted row rendered the last request/response twice after refresh, with the
  transient duplicate missing the iterations disclosure."
  [sid]
  (let
    [{:keys [turns turn-order]}
     (get @registry sid)

     live0
     (->> (or turn-order [])
          (keep #(some-> (get turns %)
                         wire-turn))
          vec)

     run-start
     (some #(when (= "streaming" (:status %)) (long (or (:started_at %) 0))) live0)

     in-flight?
     (fn [row]
       (boolean (and run-start
                     (or (= :running (:status row))
                         (when-let [d (:created-at row)]
                           (and (instance? java.util.Date d)
                                (>= (.getTime ^java.util.Date d) (long run-start))))))))

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

    ;; persisted rows arrive oldest-first; live overlay rows (running/queued,
    ;; newer) chronologically follow. The wire contract is oldest-first, so a
    ;; chat thread renders top-to-bottom with no reverse.
    (wire/canonical (vec (concat persisted live)))))

(defn transcript
  "Rich persisted transcript rows for `sid` in THE canonical wire shape
  (`wire/canonical`): turns oldest-first, each carrying its persisted iteration
  rows under `:iterations`. Canonicalizing AT THE SOURCE makes the HTTP hop an
  identity — an in-process reader and a remote gateway client (TUI / web /
  mobile) see the SAME maps, so there is exactly ONE transcript shape and a
  channel can never again be written against a shape only one transport sees."
  [sid]
  (try (let
         [db
          (lp/db-info)

          turns
          (persistance/db-list-session-turns db sid)

          att-by-soul
          (try (persistance/db-list-turns-attachments db (map :id turns)) (catch Throwable _ {}))]

         (wire/canonical
           (mapv (fn [turn]
                   (cond->
                     (assoc turn
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
   keeps the HTTP hop an identity, so an in-process client and a remote
  client (TUI / mobile) render from the SAME maps. Returns a (possibly empty)
  vector for a valid turn id, nil for an unparsable id or a read failure —
  callers use nil to fall back / retry."
  [tid]
  (try (when-let
         [turn-id (some-> tid
                          str
                          parse-uuid)]
         (let
           [db (lp/db-info)
            iters (->> (persistance/db-list-session-turn-iterations db turn-id)
                       (mapv #(update % :thinking normalize-thinking-text)))
            atts-by-iter (when (seq iters)
                           (try (into {}
                                      (map (fn [[iter-id rows]]
                                             [(str iter-id) (attachment-storage/hydrate-all rows)]))
                                      (persistance/db-list-iterations-attachments db
                                                                                  (keep :id iters)))
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
  (let
    [input
     (long (or (get tokens "input") 0))

     output
     (long (or (get tokens "output") 0))

     cost-total
     (double (or (get cost "total_cost") 0.0))

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

(declare drain-next-queued! queued-after-cancel?)

(def ^:private TURN_STALL_TIMEOUT_MS
  "Daemon backstop: force-cancel a turn wedged with NO chunk activity for this
   long. Set ABOVE svar's 4-minute semantic stream timeout so it fires ONLY when
   svar's own idle/semantic stream watchdogs miss a stalled connection — the
   'calling the provider… 8m, nothing moving' hang that freezes the session's
   turn queue. Gated on [[stall-exempt-phases]] (NOT just `:provider-call`) so a
   legitimately long tool / Python-eval phase is never force-cancelled, while a
   wedge in ANY engine/provider-internal phase — including the between-iteration
   `:iteration-final` gap where the next provider call is built / auth headers
   refreshed — is still caught. Without this, such a wedge emits no terminal
   event, so the turn never finishes and the queued backlog never drains."
  (* 6 60 1000))

(def ^:private stall-exempt-phases
  "Phases where a running turn may legitimately produce NO chunk for a long time
   — a slow shell-run / Python-eval / native tool. The stall watchdog NEVER
   force-cancels while the live phase is one of these. EVERY other phase
   (provider-call, reasoning/content streaming, response-parse, and the
   between-iteration `:iteration-final` gap) is engine/provider-internal and must
   never sit idle for `TURN_STALL_TIMEOUT_MS`."
  #{:form-start :form-result :tool-start :shell-run :shell-bg})

(defn- start-turn-stall-watchdog!
  "Daemon thread guarding ONE running turn against a stalled provider stream.
   While `tid` remains the session's current turn, it polls the shared `stall`
   atom (updated by `run-turn!`'s on-chunk with the live phase + last-chunk
   wall-clock). If the live phase is `:provider-call` and no chunk has landed
   for `TURN_STALL_TIMEOUT_MS`, it flags the turn stalled and `cancel!`s the
   token — which closes the in-flight provider stream (svar's cancel-watchdog),
   so the blocked worker unwinds into a `turn.failed` and the queue drains.
   Self-terminating: exits as soon as `tid` is no longer the current turn."
  [sid tid cancel-token stall]
  (let
    [check-ms (-> (long TURN_STALL_TIMEOUT_MS)
                  (quot 8)
                  (max 1000)
                  (min 20000))]
    (doto (Thread. ^Runnable
                   (fn []
                     (try (loop []

                            (Thread/sleep check-ms)
                            (when (= tid (:current-turn (get @registry sid)))
                              (let
                                [{:keys [phase last-ms]} @stall
                                 idle-ms (- (System/currentTimeMillis) (long (or last-ms 0)))]

                                (if (and (not (contains? stall-exempt-phases phase))
                                         (>= idle-ms (long TURN_STALL_TIMEOUT_MS)))
                                  (do (tel/log!
                                        :warn
                                        ["gateway: provider stream stalled — force-cancelling turn"
                                         tid (str idle-ms "ms idle in phase " phase)])
                                      (swap! stall assoc :stalled? true)
                                      (cancellation/cancel! cancel-token))
                                  (recur)))))
                          (catch InterruptedException _ nil)
                          (catch Throwable _ nil)))
                   (str "gateway-turn-stall-watchdog-" tid))
      (.setDaemon true)
      (.start))
    nil))

(defn- run-turn!
  "Worker body for one submitted turn. Streams phased chunks into the
  event log, runs the blocking `lp/send!`, then lands the terminal turn
  record + events. Never throws - a worker failure becomes a `failed`
  turn record and a `turn.failed` event."
  [sid tid request
   {:keys [messages model reasoning-default cancel-token extra-body turn-features workspace
           engine-opts attachments stall]}]
  (let
    [caller-on-chunk
     (get-in engine-opts [:hooks :on-chunk])

     ;; phase -> last emitted cumulative length and timestamp
     last-delta-ms
     (volatile! {})

     started-blocks
     (volatile! #{})

     on-chunk
     (fn [chunk]
       ;; Feed the stall-watchdog BEFORE any coalescing drop: record the live
       ;; phase + wall-clock of the latest provider chunk so a wedged
       ;; `:provider-call` phase (no chunks arriving at all) is detectable.
       (when stall (swap! stall assoc :phase (:phase chunk) :last-ms (System/currentTimeMillis)))
       (try
         (when caller-on-chunk
           (try (caller-on-chunk chunk)
                (catch Throwable t
                  (tel/log! :warn ["gateway: caller chunk hook failed" (ex-message t)]))))
         (let
           [phase
            (:phase chunk)

            now
            (System/currentTimeMillis)]

           (when-not (coalesce-delta? @last-delta-ms chunk now)
             (let
               [streaming?
                (contains? streaming-text-phases phase)

                cumulative
                (str (delta-text chunk))

                stream-key
                (when streaming? [phase (long (or (:iteration chunk) 0))])

                previous-len
                (long (get-in @last-delta-ms [stream-key :len] 0))

                block-id
                (when streaming? (str tid ":" (name phase) ":" (long (or (:iteration chunk) 0))))

                delta
                (when streaming? (subs cumulative (min previous-len (count cumulative))))

                chunk
                (cond-> chunk
                  streaming?
                  (assoc :stream-block-id
                    block-id :stream-delta
                    delta))]

               (when streaming?
                 (when-not (contains? @started-blocks block-id)
                   (vswap! started-blocks conj block-id)
                   (append-event! sid
                                  "content.block.started"
                                  {:turn_id tid
                                   :block (if (= phase :reasoning)
                                            (content/reasoning block-id "" "private")
                                            (content/prose block-id ""))}))
                 (vswap! last-delta-ms assoc stream-key {:ms now :len (count cumulative)}))
               (when-let [[type store? payload] (chunk->event chunk)]
                 (append-event! sid type (assoc payload :turn_id tid) {:store? store?})))))
         (catch Throwable t
           (tel/log! :warn ["gateway: chunk translation failed" (ex-message t)]))))]

    (try
      (let
        [opts
         (cond->
           (assoc (or engine-opts {})
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

         content-blocks
         (answer-content answer)

         stalled?
         (boolean (and stall (:stalled? @stall)))

         status
         (cond stalled? "failed"
               (= :cancelled (:status result)) "cancelled"
               (= :error (:status result)) "failed"
               needs-input? "suspended"
               :else "completed")

         patch
         {:status status
          :role "assistant"
          :content (cond-> content-blocks
                     stalled?
                     (conj (content/error "provider_stream_stalled"
                                          (str "Provider stream stalled: no output for "
                                               TURN_STALL_TIMEOUT_MS
                                               "ms")
                                          true)))
          :is_needs_input needs-input?
          ;; the ENGINE's persisted row id - list-turns dedups the
          ;; DB hydration against it (the gateway tid differs).
          :engine_turn_id (some-> (:session-turn-id result)
                                  str)
          :model (or (get-in result [:cost "model"]) (:model result))
          :provider (or (get-in result [:cost "provider"]) (:provider result))
          :llm_selected (:llm-selected result)
          :llm_actual (:llm-actual result)
          :is_llm_fallback (:llm-fallback? result)
          :llm_routing_trace (:llm-routing-trace result)
          :tokens (:tokens result)
          :cost (:cost result)
          :confidence (:confidence result)
          :eval (:eval result)
          :iteration_count (:iteration-count result)
          :duration_ms (:duration-ms result)
          :utilization (:utilization result)
          :error (when stalled?
                   (str "provider stream stalled: no output for " TURN_STALL_TIMEOUT_MS "ms"))
          :completed_at (System/currentTimeMillis)}]

        (finish-turn! sid tid patch)
        (record-metrics! sid result)
        (doseq [block-id @started-blocks]
          (append-event! sid "content.block.completed" {:turn_id tid :block_id block-id}))
        (append-event! sid
                       (case status
                         "failed"
                         "turn.failed"

                         "cancelled"
                         "turn.cancelled"

                         "turn.completed")
                       {:turn_id tid :status status})
        (emit-context-updated! sid)
        ;; A user cancel means "stop", not "advance": do NOT auto-start a turn
        ;; that was already queued BEFORE the cancel. Leaving that backlog
        ;; queued lets the channel pull it back into the editor (TUI) or keep
        ;; it visible/editable (web) instead of firing an uninterruptible
        ;; follow-up the instant the cancel lands. A message submitted AFTER
        ;; the cancel fired is the OPPOSITE intent — "stop that, run THIS" —
        ;; so it drains the moment this worker unwinds (queued-after-cancel?).
        ;; A STALL force-cancel, though, is a FAILURE not a user stop — the token
        ;; is cancelled either way, so distinguish on the stall flag and drain.
        (when (or stalled?
                  (not (cancellation/cancelled? cancel-token))
                  (queued-after-cancel? sid tid))
          (drain-next-queued! sid)))
      (catch Throwable t
        (let
          [stalled?
           (boolean (and stall (:stalled? @stall)))

           data
           (ex-data t)

           eval
           (when (= :vis/unsupported-reasoning-effort (:type data))
             {:valid? false
              :invalid-reasons [{:type :unsupported-reasoning-effort
                                 :requested (:requested data)
                                 :provider (some-> (:provider data)
                                                   name)
                                 :model (:model data)
                                 :supported (vec (:supported data))}]
              :reasoning-effort {:requested (:requested data) :iterations []}})

           err
           (if stalled?
             (str "provider stream stalled: no output for "
                  TURN_STALL_TIMEOUT_MS
                  "ms (force-cancelled)")
             (ex-message t))]

          (tel/log! :error ["gateway: turn worker failed" tid err])
          (finish-turn! sid
                        tid
                        (cond->
                          {:status "failed"
                           :role "assistant"
                           :content [(content/error "turn_failed" (or err "Turn failed") false)]
                           :error err
                           :completed_at (System/currentTimeMillis)}
                          eval
                          (assoc :eval eval)))
          (append-event! sid "turn.failed" {:turn_id tid :status "failed"})
          (when (or stalled?
                    (not (cancellation/cancelled? cancel-token))
                    (queued-after-cancel? sid tid))
            (drain-next-queued! sid)))))))


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
                 (cond->
                   {:turn_id tid
                    :request request
                    :started_at (or (get-in @registry [sid :turns tid :started_at])
                                    (System/currentTimeMillis))}
                   queued?
                   (assoc :queued? true)))
  (let [stall (atom {:phase nil :last-ms (System/currentTimeMillis)})]
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
                                                          :attachments attachments
                                                          :stall stall})))
    (start-turn-stall-watchdog! sid tid cancel-token stall)))


(defn- first-queued-turn
  [entry]
  (some (fn [tid]
          (let [turn (get-in entry [:turns tid])]
            (when (= "queued" (:status turn)) [tid turn])))
        (:turn-order entry)))

(defn- queued-after-cancel?
  "True when the oldest queued turn for `sid` was submitted AFTER turn `tid`'s
   cancel fired (its `:cancelling_at` stamp, set by `cancel-turn!`). A user
   cancel normally leaves the backlog queued — stop means stop — but a message
   typed AFTER Esc is the opposite intent (\"stop that, run THIS\"): while the
   cancelled provider call unwinds, the dying turn still holds `:current-turn`,
   so that fresh submit lands in the queue; without this check it would sit
   there forever because the user-cancel path skips `drain-next-queued!`."
  [sid tid]
  (let
    [entry
     (get @registry sid)

     cancelling-at
     (get-in entry [:turns tid :cancelling_at])

     [_ head]
     (first-queued-turn entry)]

    (boolean (and cancelling-at head (>= (long (or (:queued_at head) 0)) (long cancelling-at))))))

(defn- replace-last-user-message-content
  "Return `messages` with the last user message content replaced by `text`.

  Queued web/API turns may carry both the display `:request` and provider
  `:messages`. Editing a queued prompt must update both; otherwise the queue
  drains with the old provider payload and appears to answer the previous ask."
  [messages text]
  (if (vector? messages)
    (if-let
      [idx (->> (map-indexed vector messages)
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
          (if-let
            [[tid
              {:keys [request messages model reasoning-default cancel-token extra-body turn-features
                      workspace engine-opts attachments]}]
             (first-queued-turn entry)]
            (let
              [token (or cancel-token (cancellation/cancellation-token))
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
    (when-let
      [{:keys [tid request messages model reasoning-default cancel-token extra-body turn-features
               workspace engine-opts attachments]}
       @decision]
      ;; Queue-mirror signal: the queue head is no longer QUEUED. Every
      ;; attached channel drops its mirrored entry on this, and a replayed
      ;; event log nets to zero (turn.queued … turn.queued.drained). The
      ;; turn.started that follows carries :queued? true for attach flows.
      (append-event! sid "turn.queued.drained" {:turn_id tid} {:store? false})
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

(defn drain-idle!
  "Start the oldest queued turn for `sid` IF the session is idle (no turn in
   flight) AND the backlog was not deliberately left queued by a user cancel.
   No-op returning nil otherwise. Lets an attaching channel kick an orphaned
   backlog — submitted from another channel while this one was away — into
   motion the moment a client opens/resumes, instead of letting it sit forever.

   PROVENANCE GATE: a head turn queued BEFORE the session's most recent cancel
   fired was left queued by Esc on purpose — stop means stop (the same policy
   `queued-after-cancel?` encodes for the turn-terminal path). Auto-draining it
   from a background attach (tab open, project switch) would resurrect work the
   user explicitly stopped. A head queued AFTER the last cancel — or with no
   cancel in the session at all — drains normally.

   Safe to call redundantly: `drain-next-queued!` guards on `:current-turn`."
  [sid]
  (let
    [entry
     (get @registry sid)

     [_ head]
     (first-queued-turn entry)

     last-cancel
     (reduce max 0 (map long (keep :cancelling_at (vals (:turns entry)))))]

    (when (and head
               (or (zero? (long last-cancel))
                   (>= (long (or (:queued_at head) 0)) (long last-cancel))))
      (drain-next-queued! sid))))

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
    (let
      [tid
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
                                                          (cond->
                                                            {:turn_id tid
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
                            (let
                              [token (or cancel-token (cancellation/cancellation-token))
                               started-at (System/currentTimeMillis)]

                              (-> entry
                                  (assoc :current-turn tid
                                         :last-active started-at)
                                  (assoc-in [:turns tid]
                                            (cond->
                                              {:turn_id tid
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
          (do (append-event! sid "turn.queued" {:turn_id tid :request request} {:store? false})
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

(defn reconcile-orphaned-turns!
  "Mark turns left running by a dead process as interrupted.

   Queued work is deliberately memory-only. Startup never reconstructs or
   resubmits messages from persisted user requests. Returns the persistence
   sweep result."
  []
  (try (lp/db-sweep-orphaned-running-turns!) (catch Throwable _ nil)))

(defn- terminal-event->result
  "Resolve a terminal event to the canonical settled message. Terminal events
   intentionally carry no duplicate answer payload; the registry owns content."
  [event fallback-turn-id]
  (let
    [failed?
     (or (= "turn.failed" (get event "type")) (= "failed" (get event "status")))

     cancelled?
     (= "cancelled" (get event "status"))

     needs-input?
     (= "suspended" (get event "status"))

     sid-string
     (get event "session_id")

     sid
     (some #(when (= (str %) sid-string) %) (keys @registry))

     turn-id
     (or (get event "turn_id") fallback-turn-id)

     message
     (when sid (get-turn sid turn-id))

     blocks
     (or (get message "content") [])]

    ;; The terminal event is deliberately LEAN ({:turn_id :status}); the
    ;; registry row (`message`, patched by finish-turn!) owns the settled
    ;; meta — tokens/cost/model/provider/duration/…. Read the meta from the
    ;; ROW first, letting any event-carried value win, otherwise the sync
    ;; submit/attach result drops usage and live bubbles render no
    ;; tokens/cost meta at all.
    (cond->
      (-> (merge (select-keys message wire/turn-meta-keys)
                 (into {} (filter (comp some? val)) (select-keys event wire/turn-meta-keys)))
          (assoc "content" blocks
                 "iteration_count" (or (get message "iteration_count") 1)
                 "session_turn_id" (or (get message "engine_turn_id") turn-id)))
      needs-input?
      (assoc "status" "needs_input")

      cancelled?
      (assoc "status" "cancelled")

      failed?
      (assoc "error"
        (or (some #(when (= "error" (get % "type")) (get % "message")) blocks)
            (get event "error")
            "turn failed")))))

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
  for every replay/live event (canonical string-keyed) for the submitted turn.
  Returns an engine-shaped result map for in-process clients (CLI/TUI)
  that need a blocking call without bypassing the canonical gateway machinery."
  [sid {:keys [on-event] :as opts}]
  (let
    [sub-id
     (str "gateway-sync-" (java.util.UUID/randomUUID))

     started-cursor
     (current-seq sid)

     terminal
     (promise)

     submitted-turn-id
     (atom nil)

     handle-event!
     (fn [event]
       (let
         [type
          (get event "type")

          turn_id
          (get event "turn_id")]

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
                              {"type" "turn.completed" "turn_id" turn_id "status" "cancelled"})))
               ;; ANOTHER turn's queue event: forward so the channel can
               ;; mirror the session's queued backlog; never terminal here.
               (contains? queue-mirror-event-types type) (when on-event (on-event event)))))]

    (try (let
           [replay
            (subscribe! sid sub-id handle-event! started-cursor)

            submit-result
            (submit-turn! sid (dissoc opts :on-event))

            turn
            (:turn submit-result)

            turn-id
            (get turn "turn_id")]

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
   for every replay/live event (canonical string-keyed) of `tid`."
  [sid tid {:keys [on-event]}]
  (let
    [sub-id
     (str "gateway-attach-" (java.util.UUID/randomUUID))

     ;; Replay from the turn's OWN start, not from "now": a channel
     ;; reattaching mid-turn must repaint everything the turn already
     ;; produced (the iterations it missed while detached), then continue
     ;; live. `event_start_seq` is stamped on the turn record when its
     ;; `turn.started` is appended (local AND mirrored ingress). The HTTP
     ;; client path already replays (cursor 0 — client.clj/attach-turn-sync!);
     ;; this aligns the in-process path. A turn with no recorded start seq
     ;; (foreign, not yet hydrated — `subscribe!` hydrates below) falls back
     ;; to live-only: hydration appends the whole foreign turn ABOVE the
     ;; current cursor, so the replay still carries it.
     started-cursor
     (let [start-seq (get-in @registry [sid :turns tid :event_start_seq])]
       (if (pos-int? start-seq) (dec (long start-seq)) (current-seq sid)))

     terminal
     (promise)

     handle-event!
     (fn [event]
       (let
         [type
          (get event "type")

          turn_id
          (get event "turn_id")]

         (cond (= turn_id tid)
               (do
                 (when on-event (on-event event))
                 (when (contains? #{"turn.completed" "turn.failed"} type) (deliver terminal event))
                 ;; The queued record was deleted before it ever ran
                 ;; (pulled back into a sibling's editor): synthesize a
                 ;; cancelled terminal so the attach never hangs.
                 (when (= "turn.queued.deleted" type)
                   (deliver terminal {"type" "turn.completed" "turn_id" tid "status" "cancelled"})))
               ;; ANOTHER turn's queue event: forward so the channel can
               ;; mirror the session's queued backlog; never terminal here.
               (contains? queue-mirror-event-types type) (when on-event (on-event event)))))]

    (try (let [replay (subscribe! sid sub-id handle-event! started-cursor)]
           (doseq [event replay]
             (handle-event! event))
           ;; A terminal that landed at/just-before our cursor (the gateway auto-drained
           ;; AND finished the turn before we attached) will not arrive as a live event.
           ;; Recover it from the stored record so we never block forever.
           (when-not (realized? terminal)
             (let [turn (get-turn sid tid)]
               (when (contains? terminal-turn-statuses (get turn "status"))
                 (deliver terminal
                          (assoc turn
                            "type" (if (= "failed" (get turn "status"))
                                     "turn.failed"
                                     "turn.completed"))))))
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
                    (do (append-event! sid
                                       "turn.queued.updated"
                                       {:turn_id tid :request request}
                                       {:store? false})
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
        (do (append-event! sid "turn.queued.deleted" {:turn_id tid} {:store? false})
            {:status "deleted"})

        :missing
        {:error :turn-not-found}

        :not-queued
        {:error :not-queued :status status}))))

(defn- persist-cancel-stamp!
  "Durably mark `sid`'s in-flight ENGINE turn row `:cancelled` the moment a
   user cancel fires. The engine's own unwind persists the full cancel record
   when it survives — but the documented quit-during-cancel path (second
   Ctrl+C fires the token and exits immediately) can kill the JVM first,
   leaving the row `:running`; startup reconciliation would otherwise leave a
   phantom running turn. Stamping BEFORE
   the token fires makes the cancel durable even when nothing else ever gets
   to run. The engine's later terminal write (same row) simply overwrites
   this with the full record. Best-effort: cancellation must never block on
   persistence, so every failure is logged and swallowed."
  [sid]
  (try (let [db (lp/db-info)]
         (doseq
           [{:keys [id status iteration-count duration-ms]} (persistance/db-list-session-turns db
                                                                                               sid)
            :when (= :running status)]

           (persistance/db-update-session-turn! db
                                                id
                                                {:status :cancelled
                                                 :prior-outcome :cancelled
                                                 :iteration-count iteration-count
                                                 :duration-ms duration-ms})))
       (catch Throwable t
         (tel/log! :warn ["gateway: cancel stamp persist failed" (str sid) (ex-message t)]))))

(defn cancel-turn!
  "Fire the cancellation token of a running turn. Returns
   `{:status \"cancelling\"}` or `{:error ...}`."
  [sid tid]
  (let [turn (get-in @registry [sid :turns tid])]
    (cond (nil? turn) {:error :turn-not-found}
          (not= "running" (:status turn)) {:error :not-running :status (:status turn)}
          :else
          (do ;; Stamp the cancel wall-clock BEFORE firing the token so the
              ;; unwinding worker can tell post-cancel submissions (drain
              ;; them: "stop that, run THIS") from the pre-cancel backlog
              ;; (leave queued) — see `queued-after-cancel?`.
            (swap! registry assoc-in [sid :turns tid :cancelling_at] (System/currentTimeMillis))
            ;; Durable twin of the stamp above: a JVM death mid-unwind must
            ;; not leave the engine row `:running` after the process exits).
            (persist-cancel-stamp! sid)
            (some-> (:cancel-token turn)
                    cancellation/cancel!)
            {:status "cancelling"}))))

(defn cancel-current-turn!
  "Tid-less twin of `cancel-turn!`: fire the cancellation token of WHATEVER
   turn currently holds `sid`'s `:current-turn` slot. For clients that lost
   (or never learned) the turn id — an Esc that raced the `turn.started`
   late-bind, or a client-side cancel self-heal that dropped its
   `:gateway-turn-id` while the server turn kept running. Without this, that
   ghost turn keeps `:current-turn` and every next submit silently queues
   behind it. Returns `{:status \"cancelling\" :turn_id tid}` or
   `{:error :no-running-turn}`."
  [sid]
  (if-let [tid (get-in @registry [sid :current-turn])]
    (let [res (cancel-turn! sid tid)]
      (if (:error res) res (assoc res :turn_id tid)))
    {:error :no-running-turn}))

(defn cancel-all-running!
  "Fire the cancellation token of EVERY running turn across all sessions.
   Called on gateway shutdown to break in-flight provider loops BEFORE the
   shared HTTP executor is torn down — a looping turn would otherwise
   redispatch its next iteration into the dying pool and die with a
   RejectedExecutionException surfaced as a bogus \"Provider unavailable\".
   Best-effort; returns the number of turns signalled."
  []
  (reduce (fn [n sess]
            (reduce (fn [n turn]
                      (if (and (= "running" (:status turn)) (:cancel-token turn))
                        (do (try (cancellation/cancel! (:cancel-token turn))
                                 (catch Throwable _ nil))
                            (inc (long n)))
                        n))
                    n
                    (vals (:turns sess))))
          0
          (vals @registry)))

;; =============================================================================
;; Session lifecycle + souls
;; =============================================================================

(def ^:private PREWARM_POOL_DEPTH
  "Empty, fully-built sessions retained per channel. Two absorbs rapid consecutive
   creates while a background worker replenishes the first slot."
  2)

(defonce ^:private prewarm-pool (atom {:ready {} :in-flight {} :accepting? false}))

(defonce ^:private prewarm-futures (atom #{}))


(defn- session->wire
  [{:keys [id channel title external-id workspace-id]}]
  (wire/canonical {:id (str id)
                   :channel (name channel)
                   :title title
                   :external_id external-id
                   :workspace_id workspace-id}))

(defn- create-session-cold!
  [{:keys [channel title external-id workspace-id root prewarm?]}]
  (let
    [channel
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
    created))

(defn- pop-prewarmed!
  [channel]
  (let
    [[old _] (swap-vals! prewarm-pool
                         (fn [pool]
                           (update-in pool
                                      [:ready channel]
                                      (fn [ready]
                                        (let [ready (vec ready)]
                                          (if (seq ready) (subvec ready 1) ready))))))]
    (first (get-in old [:ready channel]))))

(defn- reserve-prewarm-slot!
  [channel]
  (let
    [[old new] (swap-vals! prewarm-pool
                           (fn [pool]
                             (let
                               [ready (count (get-in pool [:ready channel]))
                                building (long (get-in pool [:in-flight channel] 0))]

                               (if (and (:accepting? pool)
                                        (< (+ ready building) (long PREWARM_POOL_DEPTH)))
                                 (assoc-in pool [:in-flight channel] (inc building))
                                 pool))))]
    (< (long (get-in old [:in-flight channel] 0)) (long (get-in new [:in-flight channel] 0)))))

(defn- finish-prewarm-slot!
  [channel]
  (swap! prewarm-pool update-in [:in-flight channel] #(max 0 (dec (long (or % 0))))))

(defn- add-prewarmed!
  [channel session]
  (let
    [[old _] (swap-vals! prewarm-pool
                         (fn [pool]
                           (if (:accepting? pool)
                             (update-in pool [:ready channel] (fnil conj []) session)
                             pool)))]
    (when-not (:accepting? old)
      (swap! registry dissoc (:id session))
      (try (lp/delete! (:id session)) (catch Throwable _ nil)))))

(defn- kick-prewarm!
  [channel]
  (let
    [self
     (promise)

     fut
     (cancellation/worker-future
       (str "gateway-session-prewarm-" (name channel))
       (fn []
         (try (add-prewarmed! channel (create-session-cold! {:channel channel :prewarm? true}))
              (catch Throwable e
                (tel/log! :warn ["gateway: session prewarm failed" (name channel) (ex-message e)]))
              (finally (finish-prewarm-slot! channel) (swap! prewarm-futures disj @self)))))]

    (swap! prewarm-futures conj fut)
    (deliver self fut)
    fut))

(defn ensure-prewarmed!
  "Asynchronously top up the gateway-owned warm-session pool for `channel`.
   Idempotent and race-safe: ready plus in-flight sessions never exceed the pool
   depth. Does nothing after gateway shutdown has stopped pool acceptance."
  [channel]
  (let [channel (or channel :api)]
    (loop []

      (when (reserve-prewarm-slot! channel) (kick-prewarm! channel) (recur))))
  nil)

(defn start-prewarming!
  "Start gateway-owned warm pools for every supplied channel.
   This is the sole lifecycle entry point; channels never manage pools directly."
  [channels]
  (swap! prewarm-pool assoc :accepting? true)
  (doseq [channel channels]
    (ensure-prewarmed! channel))
  nil)

(defn- request-prewarm!
  [channel]
  (try (ensure-prewarmed! channel)
       (catch Throwable e
         (tel/log! :warn
                   ["gateway: failed to schedule session prewarm" (name channel) (ex-message e)])))
  nil)

(defn- claim-prewarmed!
  [session title]
  (let [id (:id session)]
    (persistance/db-claim-session! (lp/db-info) id)
    (when title (lp/set-title! id title))
    (assoc session :title title)))

(defn create-session!
  "Create or adopt a gateway-managed session.

   Ordinary default-workspace creates consume the gateway-owned warm pool and
   replenish it in the background. `:workspace-id`, `:root`, and `:external-id`
   require a purpose-built environment and bypass the pool."
  [{:keys [channel external-id workspace-id root] :as opts}]
  (let
    [channel
     (or channel :api)

     opts
     (assoc opts :channel channel)

     pool-eligible?
     (and (nil? external-id) (nil? workspace-id) (nil? root))

     pooled
     (when pool-eligible? (pop-prewarmed! channel))]

    (try (let
           [created (if pooled (claim-prewarmed! pooled (:title opts)) (create-session-cold! opts))]
           (when pool-eligible? (request-prewarm! channel))
           (session->wire created))
         (catch Throwable e
           (if pooled
             (do (swap! registry dissoc (:id pooled))
                 (try (lp/delete! (:id pooled)) (catch Throwable _ nil))
                 (let [created (create-session-cold! opts)]
                   (when pool-eligible? (request-prewarm! channel))
                   (session->wire created)))
             (throw e))))))

(defn discard-prewarmed!
  "Cancel warmups and delete every unused pooled session. Gateway shutdown owns
   this cleanup; channel shutdowns must not discard a pool shared by other clients."
  []
  (doseq [fut (first (reset-vals! prewarm-futures #{}))]
    (try (future-cancel fut) (catch Throwable _ nil)))
  (let
    [stopped
     {:ready {} :in-flight {} :accepting? false}

     ready
     (mapcat val (:ready (first (reset-vals! prewarm-pool stopped))))]

    (doseq [{:keys [id]} ready]
      (swap! registry dissoc id)
      (try (lp/delete! id)
           (catch Throwable e
             (tel/log! :warn
                       ["gateway: failed to discard prewarmed session" (str id) (ex-message e)])))))
  nil)

(defn soul
  "Canonical (string-keyed) wire soul for one session: persisted record + live
   gateway status."
  [sid]
  (when-let [session (lp/by-id sid)]
    (let
      [entry (get @registry sid)
       last-turn (some->> (:turn-order entry)
                          peek
                          (get (:turns entry)))]

      (wire/canonical
        {:id (str (:id session))
         :channel (some-> (:channel session)
                          name)
         :title (:title session)
         :model (:model session)
         :external_id (:external-id session)
         :created_at (:created-at session)
         :owner_id (:owner-id session)
         :project_id (some-> (:project-id session)
                             str)
         :project_name (:project-name session)
         :project_position (:project-position session)
         :status (cond (:current-turn entry) "running"
                       (= "suspended" (:status last-turn)) "suspended"
                       :else "idle")
         :current_turn_id (:current-turn entry)
         :last_active_at (:last-active entry)}))))

(defn- session-summary-extras
  "Bulk summary decorations for `list-sessions`: per-session `turn_count` +
   `modified_at` (ONE grouped `db-session-turn-stats` query for the whole
   store) and a lean `workspace` map ({root repo_root label fork_ms}) — the
   facts the TUI session picker previously fetched with TWO HTTP round-trips
   PER session (109 sequential calls / ~7.5s at 54 sessions). Deliberately NO
   git status here: that stays in the per-session `session-workspace-info`."
  [souls]
  (let
    [db
     (try (lp/db-info) (catch Throwable _ nil))

     stats
     (if db (try (persistance/db-session-turn-stats db) (catch Throwable _ {})) {})]

    (mapv (fn [s]
            (let
              [st
               (get stats (str (get s "id")))

               ws
               (when db
                 (try (when-let [w (resolve-workspace db (get s "id"))]
                        (wire/canonical {:root (:root w)
                                         :repo-root (:repo-root w)
                                         :label (:label w)
                                         :fork-ms (:fork-ms w)}))
                      (catch Throwable _ nil)))]

              (cond-> (assoc s "turn_count" (long (or (:turn-count st) 0)))
                (:latest-turn-at st)
                (assoc "modified_at" (:latest-turn-at st))

                ws
                (assoc "workspace" ws))))
          souls)))

(defn list-sessions
  "Wire souls for every persisted session, each decorated with the bulk
   summary facts (`turn_count`, `modified_at`, lean `workspace`) so ONE
   `GET /v1/sessions` is enough to paint a session picker.

   CROSS-CHANNEL by default (`channel` = `:all`): a conversation started
   in one channel is visible in the others and vice-versa. Pass a specific
   channel keyword only when a caller genuinely needs a single-channel
   slice (e.g. resolving a chat by external-id)."
  ([] (list-sessions :all))
  ([channel]
   (->> (lp/by-channel channel)
        (keep (comp soul :id))
        vec
        session-summary-extras)))

;; --- Projects (cross-channel) + movable project sessions + ownership (V6/V7) ---

(defn- project-wire
  "Canonical (string-keyed) JSON-friendly projection of a persisted project."
  [p]
  (when p
    (wire/canonical {:id (str (:id p))
                     :owner_id (:owner-id p)
                     :name (:name p)
                     :color (:color p)
                     :position (:position p)
                     :session_count (:session-count p)
                     :workspace_root (:workspace-root p)
                     :created_at (:created-at p)
                     :archived_at (:archived-at p)})))

(defn list-projects
  "Wire projects for one owner view (see loop/projects) — projects are
   cross-channel. `opts` keys: :owner-id, :include-archived?."
  ([] (list-projects {}))
  ([opts] (mapv project-wire (lp/projects opts))))

(defn get-project [pid] (project-wire (lp/get-project pid)))

(defn create-project! [opts] (project-wire (lp/create-project! opts)))

(defn get-project-by-root
  "Wire project bound to canonical workspace `root` for `owner-id` (default
   \"local\"), or nil."
  ([root] (get-project-by-root "local" root))
  ([owner-id root] (project-wire (lp/get-project-by-root owner-id root))))

(defn ensure-project-for-root!
  "Get-or-create the wire project bound to canonical workspace `root`."
  ([root] (ensure-project-for-root! "local" root nil))
  ([owner-id root name] (project-wire (lp/ensure-project-for-root! owner-id root name))))

(defn update-project! [pid opts] (project-wire (lp/update-project! pid opts)))

(defn delete-project!
  "Delete a project; its member sessions scatter back to project-less (never deleted)."
  [pid]
  (lp/delete-project! pid)
  true)

(defn assign-project!
  "Assign a session to `pid` (nil clears / removes from project). Returns the refreshed soul."
  [sid pid]
  (lp/assign-project! sid pid)
  (soul sid))

(defn reorder-project-sessions!
  "Persist the manual order of sessions inside `pid`. Returns the count applied."
  [pid session-ids]
  (lp/reorder-project-sessions! pid session-ids))

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
  (doseq
    [other
     (keys @registry)

     :when (not= other sid)]

    (append-event! other "session.title_updated" {:session_id (str sid) :title (str title)})))

(defonce bus-wiring
  ;; Wire the cross-process bus ONCE at namespace load: foreign events tailed
  ;; from sibling processes flow into `ingest-mirrored-event!`, and the
  ;; background tailer starts. Every process that touches the gateway (the
  ;; TUI, the `serve` daemon) both publishes and consumes.
  (do
    ;; pass the VAR so a dev-time ns reload is picked up without re-wiring.
    (bus/set-deliver-fn! #'ingest-mirrored-event!)
    ;; Tell the tailer which journals are worth draining: only sessions THIS
    ;; process tracks. `ingest-mirrored-event!` already no-ops on an unknown sid,
    ;; so draining the rest just burns CPU stat'ing every sibling's journal.
    (bus/set-relevant-sid-fn! (fn [sid]
                                (contains? @registry sid)))
    ;; And the SET of those sids, so the tailer drains only their journals
    ;; directly instead of listing/stat'ing every sibling's file each poll.
    (bus/set-relevant-sids-fn! (fn []
                                 (keys @registry)))
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
      :turns-running (count (keep :current-turn (vals reg)))
      :auth-refresh (lp/auth-refresh-metrics))))

(defn warm-db!
  "Force the persistence backend + shared connection on the CALLER's
   thread. The gateway runs this on its single-threaded boot path so
   the heavyweight backend namespace never lazy-loads under request
   concurrency (see require-backend-ns! in internal/persistance.clj)."
  []
  (try (lp/db-info)
       true
       (catch Throwable t (tel/log! :warn ["gateway: db warmup failed" (ex-message t)]) false)))
