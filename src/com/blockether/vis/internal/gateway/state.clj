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
            [com.blockether.vis.internal.attachments :as attachments]
            [com.blockether.vis.internal.cancellation :as cancellation]
            [com.blockether.vis.internal.content :as content]
            [com.blockether.vis.internal.form :as form]
            [com.blockether.vis.internal.format :as fmt]
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
  "Per-session event-log ring size. Older events stay durable in the session
   transcript; the ring only backs short SSE cursor reconnects. Override with
   `VIS_GATEWAY_EVENT_RING_MAX`; values <= 0 use the default of 2000.

   A `delay`, never an eager read: `native-image` initializes this namespace at
   BUILD time, so a top-level `getenv` would ship the BUILDER's answer."
  (delay (let
           [configured (some-> (System/getenv "VIS_GATEWAY_EVENT_RING_MAX")
                               str/trim
                               parse-long)]
           (if (pos? (long (or configured 0))) (long configured) 2000))))

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
  "Model-generated text and native-call code streamed LIVE over the gateway.
   Reasoning/content/prose become typed content deltas; tool previews keep their
   dedicated `block.preview` event but share the same bounded coalescing policy."
  #{:reasoning :content :assistant-prose :tool-preview})

(defn- delta-text
  "The cumulative text carried by a streaming chunk."
  [{:keys [phase thinking content text code]}]
  (case phase
    :reasoning
    thinking

    :content
    content

    :assistant-prose
    text

    :tool-preview
    code

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

;; sid (string, see `sid-key`) -> {:next-seq long
;;                :events [event ...]          ; ring, ascending :seq
;;                :subscribers {sub-id fn}     ; SSE sinks
;;                :turns {tid turn-record}     ; :cancel-token stripped on wire
;;                :turn-order [tid ...]
;;                :current-turn tid|nil
;;                :idempotency {key tid}
;;                :last-active epoch-ms}
(defonce ^:private registry (atom {}))

(defn- sid-key
  "THE key one session has in the registry, whatever spelling the caller holds.

   Every HTTP route parses its sid into a `java.util.UUID` (`path-sid`,
   `parse-multi-sids`) while a producer holds the id as a STRING (an event
   payload's `session_id`, a human-input request). Two spellings of one session
   used to be two ENTRIES: the second one had
   used to open a SECOND entry under that spelling: its own `:next-seq` counter,
   its own replay ring, and NO subscribers. Those events reached the
   cross-process journal and never the SSE fan-out, which is exactly how a
   `human_input.request` sat in the journal while the TUI never drew the form.

   Normalizing lives HERE and nowhere else: nothing below touches `registry`
   directly, every read and write goes through one of the accessors under this
   one, so a new call site cannot forget the conversion. The canonical spelling
   is the STRING one, because that is the spelling everything OUTSIDE this atom
   already uses: the `session_id` on the wire, the `<sid>.ndjson` journal the bus
   tails, and the keys handed to `bus/set-relevant-sids-fn!`."
  [sid]
  (str sid))

(defn- session-entry
  "`sid`'s registry record, or nil when this process never touched the session."
  [sid]
  (get @registry (sid-key sid)))

(defn- session-known?
  "True when this process holds a registry entry for `sid`."
  [sid]
  (contains? @registry (sid-key sid)))

(defn- known-sid
  "`sid` as the key it actually has HERE, or nil when this process is not
   tracking that session."
  [sid]
  (let [k (sid-key sid)]
    (when (contains? @registry k) k)))

(defn- other-session-ids
  "Every session this process tracks EXCEPT `sid`, in registry-key form — a
   broadcast must not hand a session its own event back because the caller
   happened to hold the id in the other spelling."
  [sid]
  (let [k (sid-key sid)]
    (remove #(= % k) (keys @registry))))

(defn- turn-record
  "One turn's internal, keyword-keyed record."
  [sid tid]
  (get-in @registry [(sid-key sid) :turns tid]))

(defn- update-session!
  "THE write path: apply `f` to `sid`'s entry (nil when it has none yet), so a
   caller that means to CREATE the entry says so with `(or entry ...)`. `f` runs
   inside the `swap!` and is retried on contention, so it must be pure."
  [sid f]
  (swap! registry update (sid-key sid) f)
  nil)

(defn- update-existing-session!
  "Apply `f` to `sid`'s entry only when it HAS one. `update-session!` would
   otherwise leave a nil entry under a live key, and a key is all it takes for
   the journal tailer to start draining a stranger's session."
  [sid f]
  (swap! registry (fn [reg]
                    (let [k (sid-key sid)]
                      (if (contains? reg k) (update reg k f) reg))))
  nil)

(defn- update-turn!
  "Apply `f` to one turn record in place."
  [sid tid f]
  (swap! registry update-in [(sid-key sid) :turns tid] f)
  nil)

(defn- put-session!
  "Install `entry` as `sid`'s whole registry record."
  [sid entry]
  (swap! registry assoc (sid-key sid) entry)
  nil)

(defn- drop-session!
  "Forget `sid`'s registry record entirely."
  [sid]
  (swap! registry dissoc (sid-key sid))
  nil)

(defn- drop-subscriber!
  "Unregister one SSE sink."
  [sid sub-id]
  (swap! registry update-in [(sid-key sid) :subscribers] dissoc sub-id)
  nil)

(defonce ^:private metrics
  (atom {:turns-total 0
         :turns-failed 0
         :tokens-input 0
         :tokens-output 0
         :cost-total 0.0
         :duration-ms-total 0
         :per-session {}}))

(def ^:private MAX_CONCURRENT_TURNS
  "Process-wide cap for simultaneously executing gateway turns. Each turn can
   own a GraalPy context and substantial transient heap, so per-session
   serialization alone is insufficient. Override with
   `VIS_GATEWAY_MAX_CONCURRENT_TURNS`; values <= 0 use the default of 2.

   A `delay`, never an eager read: `native-image` initializes this namespace at
   BUILD time, so a top-level `getenv` would ship the BUILDER's answer."
  (delay (let
           [configured (some-> (System/getenv "VIS_GATEWAY_MAX_CONCURRENT_TURNS")
                               str/trim
                               parse-long)]
           (if (pos? (long (or configured 0))) (long configured) 50))))

;; A `delay`: the permit count is read from the environment, so the semaphore
;; itself must be built by the RUNNING process, never baked into the image heap.
(defonce ^:private turn-permits
  (delay (java.util.concurrent.Semaphore. (int @MAX_CONCURRENT_TURNS) true)))

(defonce ^:private turns-executing (atom 0))

(defonce ^:private turns-waiting (atom 0))

(defn- acquire-turn-permit!
  "Wait for a process-wide execution slot. Returns false when cancellation wins
   while queued; no Python environment is opened before a slot is acquired."
  [cancel-token]
  (swap! turns-waiting inc)
  (try (loop []

         (cond (cancellation/cancelled? cancel-token) false
               (try (.tryAcquire ^java.util.concurrent.Semaphore @turn-permits
                                 100
                                 java.util.concurrent.TimeUnit/MILLISECONDS)
                    (catch InterruptedException _ false))
               true
               :else (recur)))
       (finally (swap! turns-waiting dec))))

(defn- release-turn-permit!
  []
  (swap! turns-executing dec)
  (.release ^java.util.concurrent.Semaphore @turn-permits))

;; =============================================================================
;; Event log + fan-out
;; =============================================================================

(defn- trim-ring
  "Keep the newest replay events in a persistent queue. `subvec` is deliberately
   avoided: its view retains the entire historical backing vector and turns a
   nominally bounded replay ring into an unbounded heap retention path."
  [events]
  (loop
    [ring (if (instance? clojure.lang.PersistentQueue events)
            events
            (into clojure.lang.PersistentQueue/EMPTY events))]
    (if (> (count ring) (long @EVENT_RING_MAX)) (recur (pop ring)) ring)))

(defn- fan-out!
  "Deliver `event` to every local SSE sink for `sid`. Runs on the APPENDING
   (turn) thread, so sinks must be NON-BLOCKING — server.clj registers a
   bounded-queue enqueue (`sse-sink`), never a raw socket write. A sink that
   throws is dropped - one dead connection must never poison the appender or
   siblings."
  [sid event]
  (doseq [[sub-id sink] (:subscribers (session-entry sid))]
    (try (sink event)
         (catch Throwable t
           (drop-subscriber! sid sub-id)
           (tel/log! :debug ["gateway: dropped dead subscriber" sub-id (ex-message t)])))))

(defn- fresh-entry
  "A brand-new registry entry for `sid`, seeding `:next-seq` from the journal's
   high-water so a RESTARTED daemon keeps numbering ABOVE a client's stale
   monotonic replay cursor instead of resetting to 0 (which the client's
   `seq > cursor` filter would silently drop — dropping a whole reconnect turn,
   the orphan-reap terminal included)."
  [sid]
  {:next-seq (bus/journal-high-water-seq sid)})

(defonce ^:private event-taps
  ;; key -> (fn [sid event]) run AFTER an event is stored, fanned out and
  ;; published. Side-channel observers (push notifications) attach here so
  ;; they see every locally-produced event without this ns depending on them.
  (atom {}))

(defn add-event-tap!
  "Register `f` (`[sid event]`, canonical string-keyed event) under `k`, replacing
   any previous tap with that key. A tap that throws is swallowed — an observer
   must never break the appender."
  [k f]
  (swap! event-taps assoc k f)
  k)

(defn remove-event-tap! [k] (swap! event-taps dissoc k) k)

(defn- run-event-taps!
  [sid event]
  (doseq [[k f] @event-taps]
    (try (f sid event)
         (catch Throwable t
           (tel/log!
             {:level :debug :id ::event-tap-failed :data {:tap k :error (ex-message t)}})))))

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
   (let
     [captured
      (volatile! nil)

      ;; Canonicalize the payload ONCE, OUTSIDE the swap!. `wire/canonical` is a
      ;; FULL recursive walk of the payload, and tool results reach megabytes —
      ;; while a `swap!` body is re-run from scratch on EVERY CAS retry. There is
      ;; ONE `registry` atom for every session in the process, so concurrent
      ;; appends (SSE fan-out, several live turns, the cross-process bus mirror)
      ;; collide constantly: measured 15155 walks to append 2400 events — 84%
      ;; of the work thrown away, 8x the wall time. Nothing in the stamp depends
      ;; on `entry`, so only `seq` has to be decided inside the loop.
      canonical-payload
      (wire/canonical payload)

      ;; Stamped once as well: a CAS retry must not drift the event's timestamp.
      stamp-ts
      (System/currentTimeMillis)

      stamp-sid
      (str sid)

      stamp-type
      (wire/->wire type)]

     (update-session!
       sid
       (fn [entry]
         (let
           [entry
            (or entry (fresh-entry sid))

            n
            (inc (long (:next-seq entry 0)))

            event
            ;; The identity stamp is applied LAST and a payload can NEVER override
            ;; it: `:session_id`, `:seq`, `:ts`, `:type` and `:schema` are facts
            ;; of the RING this event is being appended to, not free payload
            ;; fields. Both dedup guards on the wire key off that pair — the
            ;; multiplexed SSE body's per-session `last-seqs` and every client's
            ;; per-session cursor — so an event stamped with ANOTHER session's
            ;; id while carrying THIS ring's seq taught both sides that the other
            ;; session had already reached this (much higher) seq. Every later
            ;; event of that session then looked "already seen" and was dropped:
            ;; a live turn stopped streaming mid-flight and never delivered its
            ;; terminal, leaving the channel spinning forever. Cross-session
            ;; payloads carry their subject under their OWN key (see
            ;; `broadcast-title-event!`'s `:titled_session_id`).
            ;;
            ;; The stamp keys are ALREADY in canonical spelling, so assoc-ing
            ;; them onto the canonicalized payload is identical to canonicalizing
            ;; the stamped payload — and it enforces the rule above outright: a
            ;; payload key that merely CANONICALIZES onto a stamp key
            ;; (`:session-id` -> `session_id`) can no longer race it.
            (assoc canonical-payload
              "schema" 1
              "seq" n
              "ts" stamp-ts
              "session_id" stamp-sid
              "type" stamp-type)]

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
       ;; A turn id is single-use. Its only `turn.started` begins a fresh journal;
       ;; Vis never re-queues or relaunches the failed request under that id.
       (bus/publish! sid event {:store? store? :truncate? (= type "turn.started")})
       (run-event-taps! sid event)
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
  (when (session-known? sid)
    (let
      [type
       (get event "type")

       tid
       (get event "turn_id")

       terminal?
       (contains? #{"turn.completed" "turn.failed" "turn.cancelled"} type)

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

      (update-session!
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

(defn- ensure-session-entry!
  "Make sure `sid` HAS a registry entry, so a mirror path that no-ops on an
   unknown session (`ingest-mirrored-event!`) can deliver into it."
  [sid]
  (update-session! sid
                   (fn [entry]
                     (or entry (fresh-entry sid)))))

(defn- claim-hydrate!
  "Win the right to hydrate `sid`'s foreign turn — or don't, and move on.

   ONE hydrate at a time per session: concurrent SSE subscribers otherwise both
   read `:current-turn` unset and both mirror the sibling's turn, so every event
   lands in the ring twice (re-sequenced, so no dedup catches it). The claim is
   a flag on the session's OWN entry, taken in the SAME `swap!` that reads
   `:current-turn`, so that decision is one atomic step. The `locking` this
   replaces parked a request thread for the whole length of a journal read, and
   needed a side map of per-sid monitor objects that had to be allocated, looked
   up and disposed along with the session.

   The subscriber that loses the claim never waits and never duplicates:
   `subscribe!` snapshots its replay and registers its sink in a SINGLE swap, so
   whatever the winner hydrates after that reaches it live rather than twice."
  [sid]
  (let [claimed (volatile! false)]
    (update-session! sid
                     (fn [entry]
                       (let [entry (or entry (fresh-entry sid))]
                         (if (or (:current-turn entry) (:is-hydrating entry))
                           (do (vreset! claimed false) entry)
                           (do (vreset! claimed true) (assoc entry :is-hydrating true))))))
    @claimed))

(defn- hydrate-foreign-turn!
  "Mirror a turn running in a SIBLING process into this registry: at most one
   hydrate in flight per session, and none at all while this process is already
   tracking a live turn."
  [sid]
  (when (claim-hydrate! sid)
    (try (bus/hydrate! sid)
         (finally (update-existing-session! sid
                                            (fn [entry]
                                              (dissoc entry :is-hydrating)))))))
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
  (ensure-session-entry! sid)
  (hydrate-foreign-turn! sid)
  (let [replay (volatile! [])]
    (update-session!
      sid
      (fn [entry]
        (let [entry (or entry (fresh-entry sid))]
          (vreset! replay (filterv #(> (long (get % "seq")) (long (or cursor 0))) (:events entry)))
          (assoc-in entry [:subscribers sub-id] sink))))
    @replay))

(defn unsubscribe! [sid sub-id] (drop-subscriber! sid sub-id) nil)

(defn current-seq
  "Highest event `:seq` assigned for `sid` so far. Subscribing with this
   as the cursor yields a live-only stream (empty replay)."
  [sid]
  (:next-seq (session-entry sid) 0))

(defn running-turn-start-cursor
  "For a live-only subscriber joining a session mid-turn: the cursor (one below
   the currently-running turn's `turn.started` seq) that replays the WHOLE
   in-flight turn — user bubble, thinking, forms, activity — instead of only the
   deltas that happen after connect. This is what lets a companion/web client
   that OPENS a session already driven from the TUI paint the same live 'Vis is
   running: …' bubble the originating channel shows. nil when no turn is running
   locally or its start seq wasn't recorded (a foreign turn is handled instead by
   `subscribe!`'s hydrate, which appends it above the live-only cursor)."
  [sid]
  (let
    [entry
     (session-entry sid)

     tid
     (:current-turn entry)

     start-seq
     (get-in entry [:turns tid :event_start_seq])]

    (when (pos-int? start-seq) (dec (long start-seq)))))

(defn current-turn-id
  "Turn id this process is running for `sid` right now, or nil when the session
   is idle. The registry's `:current-turn` mirror: set on `turn.started`, cleared
   by that turn's terminal event, and maintained for FOREIGN turns too (a sibling
   process's turn is hydrated into this registry on subscribe).

   This is the one fact a reconnecting client needs and cannot infer from its own
   stream: whether the turn it is still painting is the turn the daemon is still
   running. `sse-ready!` ships it with every subscription so the answer costs no
   round trip."
  [sid]
  (:current-turn (session-entry sid)))

(defn events-since
  "Read-only peek at the replay ring: stored canonical (string-keyed) events
   with `\"seq\"` > cursor, oldest first. Lets a page renderer locate the
   running turn's `turn.started` seq so its SSE reconnect can replay the WHOLE
   in-flight turn instead of only what happens after connect."
  [sid cursor]
  (filterv #(> (long (get % "seq")) (long (or cursor 0))) (:events (session-entry sid) [])))

(defn running-turn-count
  "Number of live turns currently owned by this gateway process. Used by the
   daemon lifecycle gate: the server may only self-stop when this is zero AND
   the client refcount is zero."
  []
  (->> (vals @registry)
       (keep :current-turn)
       count))

(defn session-busy?
  "True when `sid` still has work the daemon owns: a live `:current-turn`, or a
   turn parked in the queue. THE guard for view-close teardown — a session is
   shared, so \"my last view closed\" never proves \"nobody is working here\":
   another channel (companion app, web, a second TUI) may be attached to and
   streaming that very turn."
  [sid]
  (let [entry (session-entry sid)]
    (boolean (or (:current-turn entry)
                 (some (fn [turn]
                         (contains? #{"running" "queued"}
                                    (some-> (:status turn)
                                            name)))
                       (vals (:turns entry)))))))

;; =============================================================================
;; Per-session model preference
;; =============================================================================

(defn set-session-model!
  "Set (or clear, with blank model) the per-session PROVIDER + MODEL
   preference. Every turn submitted for `sid` routes through it (the engine
   reads it at turn start; `router-for-model` hoists the model, an unknown
   name degrades to the default order). Channel-agnostic: web + TUI + embedded
   callers all set it here, persisted in the DB and shared across channels.

   A changed manual preference also receives a small durable audit sidecar for
   the `usage` section of `session_state()`; the live `session.model_updated`
   event remains non-replayable so old cursor events cannot overwrite a newer preference."
  [sid provider model]
  ;; `fresh-entry`, never a bare zero: an entry seeded at 0 restarts the seq
  ;; counter below a live client's cursor and silently kills its stream.
  (update-session! sid #(assoc (or % (fresh-entry sid)) :last-active (System/currentTimeMillis)))
  (let
    [label
     #(cond (nil? %) nil
            (keyword? %) (name %)
            :else (not-empty (str %)))

     db
     (lp/db-info)

     before
     (smodel/model-of db sid)

     result
     (smodel/set-model! db sid provider model)]

    (smodel/record-switch! db sid before result :gateway)
    ;; BROADCAST the pick. The store is shared, but every attached channel keeps
    ;; its own display copy (the TUI footer chip, the web rail), and without an
    ;; event a change made in one channel stayed invisible in the others until
    ;; they happened to re-read - the "I switched the model and nothing moved"
    ;; desync. Live-only: the pref is persisted, so a cursor replay must not
    ;; re-apply an old pick over a newer one.
    (append-event! sid
                   "session.model_updated"
                   {:provider (label provider) :model (label model)}
                   {:store? false})
    result))

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
   \"git\"}` for the session pinned to `sid`, or nil. Resolves soul → latest\n   state → workspace; never throws."
  [sid]
  (try (when-let [db (lp/db-info)]
         (when-let [ws (resolve-workspace db sid)]
           (wire/canonical {:id (:id ws)
                            :draft? (workspace/draft? ws)
                            :root (:root ws)
                            :repo-root (:repo-root ws)
                            :label (:label ws)
                            :fork-ms (:fork-ms ws)
                            ;; Git working-tree status resolved HERE, in the gateway/daemon
                            ;; that owns the repo on disk — streamed to channels as a cached
                            ;; session fact instead of each client re-walking git locally (a
                            ;; remote TUI has no access to the repo's filesystem, and even
                            ;; colocated it stops every tab switch from recomputing). Cached
                            ;; per repo root, so repeated fetches never re-walk a warm root.
                            :git (git/workspace-status (:root ws))})))
       (catch Throwable _ nil)))

(defn session-usage-info
  "Whole-session USAGE rollup for `sid` in THE canonical string-keyed wire shape
   `{\"turn_count\" \"iteration_count\" \"tool_call_count\" \"fold_count\"
   \"top_tools\" \"error_count\" \"top_errors\" \"input_tokens\"
   \"input_regular_tokens\"
   \"input_cache_write_tokens\" \"input_cache_read_tokens\" \"output_tokens\"
   \"output_reasoning_tokens\" \"cache_hit_rate\" \"cost_usd\" \"duration_ms\"
   \"first_turn_at\" \"last_turn_at\" \"provider\" \"model\"}`, or nil when the
   session has no turns yet.

   `cache_hit_rate` is derived HERE, once, so every channel reads the same
   number instead of three clients each dividing differently: cached input over
   TOTAL input (`input_tokens` is the total; the three detail columns are its
   subsets). Never throws."
  [sid]
  (try (when-let [db (lp/db-info)]
         (when-let [u (persistance/db-session-usage-stats db sid)]
           (let
             [input (long (or (:input-tokens u) 0))
              cached (long (or (:input-cache-read-tokens u) 0))]

             (wire/canonical (cond-> u
                               (pos? input)
                               (assoc :cache-hit-rate (double (/ cached input))))))))
       (catch Throwable _ nil)))

(defn change-root!
  "Repoint the session pinned to `sid` at `path` as its PRIMARY root, then return
   the refreshed `session-workspace-info` (whose `:id` is the newly pinned
   workspace). Server-side so the change lands in the daemon that runs the turns."
  [sid path]
  (when-let [db (lp/db-info)]
    (when-let [state-id (resolve-state-id db sid)]
      (workspace/change-root! db state-id path)))
  (session-workspace-info sid))

(defn list-drafts
  "Active/stashed DRAFTS for the repo the session pinned to `sid` lives in, in
   THE canonical string-keyed wire shape
   `[{\"workspace_id\" \"label\" \"root\" \"repo_root\" \"fork_ms\" \"is_current\"}]`,
   newest first — the parked drafts any channel can `resume-draft!`. The session's
   own current draft (when it is in one) rides `\"is_current\" true`. Server-side
   twin of the `/draft list` slash: ONE gateway fact every channel (web picker,
   TUI drafts view) reads instead of typing the slash. Never throws; returns []
   when the session or its repo is unknown."
  [sid]
  (or (try (when-let [db (lp/db-info)]
             (when-let [ws (resolve-workspace db sid)]
               (let [current-id (when (workspace/draft? ws) (:id ws))]
                 (mapv (fn [d]
                         (wire/canonical {:workspace-id (:id d)
                                          :label (workspace/display-label db d nil)
                                          :root (:root d)
                                          :repo-root (:repo-root d)
                                          :fork-ms (:fork-ms d)
                                          :current? (= current-id (:id d))}))
                       (workspace/list-drafts db (:repo-id ws))))))
           (catch Throwable _ nil))
      []))

(defn stash-draft!
  "Park the session's current draft — leave the draft row `:active` and its clone
   on disk, repoint the session back to trunk — then return the refreshed
   `session-workspace-info`. The non-destructive twin of abandoning; a no-op that
   returns trunk info when the session is already on trunk. Runs SERVER-SIDE in
   the daemon that owns the DB. Channel-agnostic twin of the `/draft stash` slash."
  [sid]
  (when-let [db (lp/db-info)]
    (when-let [state-id (resolve-state-id db sid)]
      (workspace/stash! db state-id)))
  (session-workspace-info sid))

(defn resume-draft!
  "Switch the session pinned to `sid` INTO the stashed draft `workspace-id`, then
   return the refreshed `session-workspace-info`. The target is validated against
   the session's current repo BEFORE any current draft is stashed. When the session
   is currently in another draft it is then stashed non-destructively, so this is a
   true draft switch, not just an enter-from-trunk. Runs SERVER-SIDE in the daemon.
   Throws `ex-info` with a `:type` when `workspace-id` is not resumable (see
   `workspace/resume!`). Channel-agnostic twin of the `/draft resume` slash."
  [sid workspace-id]
  (when-let [db (lp/db-info)]
    (when-let [state-id (resolve-state-id db sid)]
      (let
        [current (resolve-workspace db sid)
         target (workspace/get db workspace-id)]

        (when (and current target (not= (:repo-id current) (:repo-id target)))
          (throw (ex-info "Draft belongs to a different repository"
                          {:type :workspace/draft-repo-mismatch
                           :workspace-id workspace-id
                           :repo-id (:repo-id current)
                           :draft-repo-id (:repo-id target)})))
        (when (workspace/draft? current) (workspace/stash! db state-id))
        (workspace/resume! db {:session-state-id state-id :workspace-id workspace-id}))))
  (session-workspace-info sid))

(defn create-draft!
  "Create and enter a named draft for `sid` in the daemon. If the session is
   already in a draft, park that draft first; creating from the picker is thus
   non-destructive and always forks the real repo trunk. `clean?` seeds from the
   COMMITTED HEAD, leaving the user's uncommitted work behind in their repo.
   Returns refreshed canonical workspace info."
  [sid label clean?]
  (let
    [label (some-> label
                   str
                   str/trim)]
    (when (str/blank? label)
      (throw (ex-info "Draft name cannot be blank" {:type :workspace/blank-draft-label})))
    (when-let [db (lp/db-info)]
      (when-let [state-id (resolve-state-id db sid)]
        (let
          [current (resolve-workspace db sid)
           repo-root (or (:repo-root current) (:root current) (workspace/trunk-root))]

          (when-not (workspace/isolated-workspaces-supported? repo-root)
            (throw (ex-info "No workspace backend can create an isolated draft here"
                            {:type :workspace/isolation-unavailable
                             :root repo-root
                             :hint (workspace/isolation-unavailable-hint repo-root)})))
          (when (workspace/draft? current) (workspace/stash! db state-id))
          (let [trunk (resolve-workspace db sid)]
            (workspace/create!
              db
              {:session-state-id state-id :label label :from trunk :clean? (boolean clean?)})))))
    (session-workspace-info sid)))

(defn abandon-draft!
  "Permanently discard one active draft owned by `sid`'s current repo. A parked
   draft may be removed directly. If it is the caller's current draft, first
   repoint the session to that draft's real trunk. Drafts from another repo or
   pinned to another session are rejected. Returns refreshed canonical workspace
   info."
  [sid workspace-id reason]
  (when-let [db (lp/db-info)]
    (when-let [state-id (resolve-state-id db sid)]
      (let
        [target (workspace/get db workspace-id)
         current (resolve-workspace db sid)]

        (when-not (workspace/draft? target)
          (throw (ex-info "Not an active draft"
                          {:type :workspace/not-a-draft :workspace-id workspace-id})))
        (when (not= :active (:state target))
          (throw (ex-info "Draft is no longer active"
                          {:type :workspace/draft-inactive :workspace-id workspace-id})))
        (when (and current (not= (:repo-id current) (:repo-id target)))
          (throw (ex-info "Draft belongs to a different repository"
                          {:type :workspace/draft-repo-mismatch
                           :workspace-id workspace-id
                           :repo-id (:repo-id current)
                           :draft-repo-id (:repo-id target)})))
        (let
          [pinned-elsewhere (remove #(= (str state-id) (str (:id %)))
                              (persistance/db-session-state-list-for-workspace db workspace-id))]
          (when (seq pinned-elsewhere)
            (throw (ex-info "Draft is in use by another session"
                            {:type :workspace/draft-in-use :workspace-id workspace-id}))))
        (when (= (str (:id current)) (str (:id target)))
          (workspace/exit-to-trunk! db state-id (:repo-root target)))
        (workspace/abandon! db {:workspace-id workspace-id :reason reason}))))
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
   persisted under iteration `iid` as METADATA ONLY — the
   `db-list-iteration-attachments-meta` shape, never a byte of payload — or `[]`.
   THE canonical, ordered, UNFILTERED list. Everything a client sees is derived
   from it by [[user-iteration-attachments]], which is what both the descriptors
   and the byte endpoint go through; the ONE artifact the endpoint then serves
   fetches its own bytes in [[attachment-bytes]].

   Listing the bytes here made LISTING cost the whole iteration: every
   `iteration.completed` frame read (and base64-encoded) every figure it was
   only going to describe, and serving image N of a gallery re-read all N — a
   9-image gallery paid 81 image reads for 9. nil/unparsable id -> `[]`."
  [iid]
  (try (if-let
         [iid (some-> iid
                      str
                      parse-uuid)]
         (vec (persistance/db-list-iteration-attachments-meta (lp/db-info) iid))
         [])
       (catch Throwable t
         (tel/log! :warn ["gateway: iteration-attachments read failed" (str iid) (ex-message t)])
         [])))

(defn user-iteration-attachments
  "[[iteration-attachments]] minus the rows a human is never shown (audience
   `model`) — THE list index N addresses.

   One list, filtered ONCE, is the whole contract: the descriptors number this
   seq and `GET /v1/sessions/:sid/iterations/:iid/attachments/:idx` serves from
   it. Filtering on the descriptor side alone re-numbered what survived while
   the byte endpoint still indexed the raw rows, so an iteration whose first
   artifact was model-only handed every later index the wrong bytes — and handed
   the human the artifact that was hidden from it."
  [iid]
  (into [] (remove attachments/hidden-from-user?) (iteration-attachments iid)))

(defn- decode-base64 ^bytes [^String s] (.decode (java.util.Base64/getDecoder) s))

(defn attachment-bytes
  "Raw bytes for ONE attachment map (an [[iteration-attachments]] element),
   fetched LAZILY — listing describes, only this reads. An inline `:base64` the
   caller already holds is decoded; an external `:storage-uri` goes through the
   storage rail; otherwise the row's own `:id` is re-read from the store, which
   is why the metadata listers can stay byte-free.

   TOTAL: a corrupt payload, a vanished row or an unreachable storage backend is
   `nil` — the endpoint answers a clean 404 instead of throwing a 500 out of a
   `Base64` decoder."
  ^bytes [{:keys [base64 storage-uri id has-bytes]}]
  (try (cond (some? base64) (decode-base64 base64)
             (some? storage-uri) (attachment-storage/resolve-bytes storage-uri)
             (and (some? id) (not (false? has-bytes)))
             (some-> (persistance/db-read-attachment (lp/db-info) id)
                     :base64
                     decode-base64)
             :else nil)
       (catch Throwable t
         (tel/log! :warn ["gateway: attachment-bytes read failed" (str id) (ex-message t)])
         nil)))

(defn- attachment-descriptors
  "Lean wire descriptors — metadata ONLY, NEVER base64 — for ONE iteration's
   ordered attachment `rows` (the [[iteration-attachments]] shape). `:index` is
   the position in that list — the EXACT list (and order) that
   `GET /v1/sessions/:sid/iterations/:iid/attachments/:idx` serves — so a
   persist-skipped artifact is absent from both and index N always names the SAME
   artifact live, in history, and at the byte endpoint. THE one descriptor shape:
   the live `iteration.completed` frame and the persisted transcript ship it
   verbatim, so a remote client (iOS/Android/web) renders a produced image with
   one code path instead of two.

   `rows` is ALREADY the human's own list ([[user-iteration-attachments]]): a
   model-only artifact is dropped there, by the very call the byte endpoint
   indexes, so the gallery a person scrolls stays the one the agent meant them
   to review AND index N names the same artifact on both sides."
  [iteration-id rows]
  (into []
        (map-indexed (fn [idx {:keys [tool-call-id kind media-type filename size audience version]}]
                       {:index idx
                        :iteration_id (str iteration-id)
                        :tool_call_id tool-call-id
                        :kind (or kind "image")
                        :media_type (str (or media-type "application/octet-stream"))
                        :filename filename
                        ;; VERSION: re-attaching a filename is the next cut of THAT
                        ;; artifact, so a client groups the gallery by name and
                        ;; shows the newest version with its history behind it.
                        :version (long (or version 1))
                        :audience (attachments/normalize-audience audience)
                        :size (long (or size 0))}))
        rows))

(defn- live-attachment-descriptors
  "[[attachment-descriptors]] for the artifacts iteration `iteration-id`
   persisted, so a native client (iOS/RN) learns 'image N produced' on the LIVE
   `iteration.completed` frame and lazy-fetches the bytes from
   `GET /v1/sessions/:sid/iterations/:iid/attachments/:idx` rather than bloating
   every SSE frame with 100s of KB. `[]` on any read failure."
  [iteration-id]
  (attachment-descriptors iteration-id (user-iteration-attachments iteration-id)))

(defn append-iteration-attachment!
  "Store a HUMAN's revision of an artifact the model produced, into the very
   iteration that produced it, and hand back its wire descriptor.

   The version rule is the engine's own and lives in the writer: re-using the
   filename is the next CUT of that artifact, so a note the human annotated in
   the companion becomes `v2` of that note rather than a second file with the
   same name. Everything a client already knows how to do with an artifact -
   list it, thread it by name, fetch its bytes by index - then works on the
   revision unchanged.

   `att` is `{:filename :media-type :base64}`. nil when the iteration is unknown
   or the payload could not be stored."
  [iid att]
  (try (when-let
         [iid (some-> iid
                      str
                      parse-uuid)]
         (when-let [stored (persistance/db-append-iteration-attachment! (lp/db-info) iid att)]
           (let
             [rows (user-iteration-attachments iid)
              idx (first (keep-indexed (fn [i row]
                                         (when (= (str (:id row)) (str (:id stored))) i))
                                       rows))]

             (when idx (nth (attachment-descriptors iid rows) idx nil)))))
       (catch Throwable t
         (tel/log! :warn ["gateway: append-iteration-attachment! failed" (str iid) (ex-message t)])
         nil)))

(def ^:private activity-phases
  "Coarse 'Vis is doing X' phases surfaced to the LIVE ticker but never pinned
   into the durable trace: a provider wait, response parsing, and shell/tool
   calls (incl. a nested `shell` call inside python_execution) that would
   otherwise leave the bubble frozen for the whole call."
  #{:provider-call :response-parse :shell-run :shell-bg :tool-start})

(defn- activity-chunk->event
  "Ephemeral `activity` wire event `[type store? payload]` for a coarse-progress
   phase, or nil. store? is false: channels paint a spinner label; nothing
   persists. `:response-parse :done` clears (emits nil) — the parse finished."
  [{:keys [phase cmd iteration reason] :as chunk}]
  (when (and (activity-phases phase)
             (not (and (= phase :response-parse) (= :done (:status chunk)))))
    (let
      [op
       (some-> (:op (:tool-event chunk))
               name)

       label
       (:label (:tool-event chunk))

       activity
       (if (= phase :tool-start) "tool" (name phase))]

      ["activity" false
       (cond-> {:activity activity}
         (some? iteration)
         (assoc :iteration iteration)

         ;; WHY the provider request exists (`user-submit` / `tool-result`).
         (some? reason)
         (assoc :reason (name reason))

         (some? cmd)
         (assoc :cmd (str cmd))

         (some? op)
         (assoc :op op)

         (some? label)
         (assoc :label (str label)))])))

(defn- chunk->event
  "Translate one phased iteration chunk (progress.clj contract) into a
   `[type store? payload]` wire event triple. Model text phases
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
         (:tool-preview :form-start)
         (merge
           ;; Carry the native-tool badge identity so a client can hide the
           ;; redundant invocation code WHILE the tool runs.
           (form/->display (form/with-display-code chunk))
           (cond-> {:block_id position :code code}
             (:vis/tool-name chunk)
             (assoc :tool_name (:vis/tool-name chunk))

             (:svar/tool-call-id chunk)
             (assoc :tool_call_id (:svar/tool-call-id chunk))))

         :form-result
         (merge
           ;; The native-tool op-card fields (pre-rendered card + badge label
           ;; + colour) — projected from ONE canonical list.
           (form/->display (form/with-display-code chunk))
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
           (map? error)
           (assoc :error-data (select-keys error [:type :message :status :cause-class]))

           (some? error)
           (assoc :provider-error-data (provider-error/provider-error-info error)))

         (if (= phase :provider-retry-reset)
           (cond->
             {:attempt (:attempt chunk)
              :max-retries (:max-retries chunk)
              :delay-ms (:delay-ms chunk)}
             (map? (:error chunk))
             (assoc :error (select-keys (:error chunk) [:type :message :status :cause-class]))

             (map? (:event chunk))
             (assoc :event
               (select-keys (:event chunk)
                            [:event/type :reason :provider :model :from-provider :from-model
                             :attempt :delay-ms :status :error])))
           {:detail (wire/bounded-pr (dissoc chunk :phase) ERROR_PR_LIMIT)}))]
      [(case phase
         :tool-preview
         "block.preview"

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

(defn- inline-attachment-preview
  "Byte-free chip payload for ONE inline (already base64-encoded) upload."
  [a]
  (let
    [pick
     (fn [& ks]
       (some (fn [k]
               (let [v (or (get a k) (get a (keyword k)))]
                 (when-not (str/blank? (str v)) (str v))))
             ks))

     b64
     (str (or (get a "base64") (get a :base64) ""))

     size
     (long (* 3 (quot (count b64) 4)))]

    (when-let [filename (pick "filename" "name")]
      {:filename filename
       :media_type (or (pick "media_type" "media-type") "image")
       :size size
       :size_label (attachments/size-label size)})))

(defn- attachment-previews
  "What a channel needs to PAINT one user message's images - filename, media
   type, human size - with no pixel bytes at all.

   Two authoring styles feed the same list: inline uploads (companion/web/API
   post base64) and image PATHS inside the request text (the TUI's drag-drop or
   clipboard paste). Both resolve HERE, once, at submit time, so a queued row
   renders identically in every channel instead of each one re-deriving it (or,
   as before, showing a raw `/var/folders/.../clipboard-....png`).
   De-duped by filename; never throws."
  [request inline workspace]
  (let
    [root
     (or (:root workspace) (get workspace "root"))

     from-inline
     (keep inline-attachment-preview (or inline []))

     from-text
     (try (map (fn [d]
                 {:filename (:filename d)
                  :media_type (:media-type d)
                  :size (:size d)
                  :size_label (:size-label d)
                  :path (:path d)})
               (attachments/scan-image-descriptors request {:workspace-root root}))
          (catch Throwable _ nil))]

    (->> (concat from-inline from-text)
         (reduce (fn [acc p]
                   (if (some #(= (:filename %) (:filename p)) acc) acc (conj acc p)))
                 [])
         vec)))

(defn- request-preview-text
  "The prose a QUEUE ROW should show for `request`.

   Image paths collapse to `name.png` chips (see
   `attachments/text->chip-preview`); nil means the message was nothing but
   images and the channel should paint its attachment chips alone. The
   untouched `:request` still travels beside it, so pulling a queued message
   back into a composer restores the exact text — paths included — that
   re-attaches on re-send."
  [request _previews]
  (try (attachments/text->chip-preview request)
       (catch Throwable _ (not-empty (str/trim (str (or request "")))))))

(def ^:private session-opening-line-max
  "Character cap for the `first_request` decoration. Long enough for a real
   opening sentence, short enough that a session LIST never ships a transcript."
  240)

(defn- session-opening-line
  "One bounded, path-free, single-line rendering of a session's FIRST user
   request - what the session opened with. Session pickers show it beside the
   generated title, so a row says what was actually asked instead of only how
   the titler named it. nil when there is nothing to show."
  [request]
  (some-> (request-preview-text request nil)
          str
          (str/replace #"\s+" " ")
          str/trim
          not-empty
          (as-> s (if (> (count s) (long session-opening-line-max))
                    (str (subs s 0 (long session-opening-line-max)) "\u2026")
                    s))))

(defn- wire-turn
  [turn]
  (when turn
    (let [started-at (or (:created_at turn) (:started_at turn) (:queued_at turn))]
      (-> turn
          (dissoc :cancel-token)
          ;; Inline uploads carry base64 pixels: a turn LIST must never ship
          ;; them. `:attachment_previews` is the byte-free chip payload.
          (dissoc :attachments)
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
  (some-> (wire-turn (turn-record sid tid))
          wire/canonical))

(defn- request-text-attachments
  "The images a turn's own request TEXT points at, loaded WITH their pixel bytes.

   Not every image arrives as an upload. The TUI's drag-drop and clipboard paste
   author a message that merely NAMES the file - an absolute path inside a
   `vis-image` fence - and only the ENGINE turns those paths into stored bytes,
   under its own session-turn id, once the turn lands. So this authoring style
   had no byte source at all while the turn was in flight, and the app's live
   user bubble painted a filename chip with no picture in it until the session
   was reloaded. `attachment-previews` already resolves BOTH authoring styles for
   the byte-free chips; this is that same resolution with the pixels, in the
   shape a transcript row carries. Never throws."
  [turn]
  (try (let
         [workspace
          (:workspace turn)

          root
          (or (:root workspace) (get workspace "root"))]

         (into []
               (map-indexed (fn [position {:keys [path media-type base64 size]}]
                              {:filename (.getName (java.io.File. (str path)))
                               :media_type media-type
                               :base64 base64
                               :size size
                               :kind "image"
                               :source "user"
                               :audience "both"
                               :position position}))
               (:attached (attachments/collect-user-images (:request turn)
                                                           {:workspace-root root}))))
       (catch Throwable _ nil)))

(defn- dedupe-attachments-by-filename
  "First row wins per filename: an inline upload and the same file named in the
   request text are ONE attachment, never two."
  [rows]
  (->> rows
       (reduce (fn [acc row]
                 (let [filename (get row "filename")]
                   (if (some #(= filename (get % "filename")) acc) acc (conj acc row))))
               [])
       vec))

(defn turn-attachments
  "The FULL attachments (filename / media_type / base64) of ONE turn.

   The live rail and the queue mirror ship byte-free `:attachment_previews`, and
   a turn's persisted row only exists once it LANDS — so between submit and
   landing the SENDER's own in-memory copy was the only thing that could paint
   the user bubble's images. Restart the app (or open the session on a second
   device) mid-turn and they were gone for good.

   The gateway can reach those bytes the whole time, from all THREE sources a
   user's image has: `:attachments` on the registry entry of a running/queued
   turn (inline uploads), the paths the turn's own request text names (the TUI's
   drag-drop and clipboard paste, which upload nothing), and the attachment store
   once the turn lands. This serves them, in the SAME shape a transcript row
   carries, so a channel can lazily fetch what it does not have. nil when the
   turn is unknown or carried no images."
  [sid tid]
  (when (and sid tid)
    (let [turn (turn-record sid tid)]
      (or (seq (dedupe-attachments-by-filename
                 (wire/canonical (into (vec (:attachments turn)) (request-text-attachments turn)))))
          (try (seq (wire/canonical (vec (get (persistance/db-list-turns-attachments (lp/db-info)
                                                                                     [tid])
                                              (str tid)))))
               (catch Throwable _ nil))))))

(def ^:private persisted-status->wire
  "Durable engine turn status -> wire status. A map lookup rather than `case`:
   the constants collide on hash, so `case` degrades to a linear scan anyway."
  {nil "completed"
   "" "completed"
   "success" "completed"
   "done" "completed"
   "interrupted" "cancelled"
   "error" "failed"
   "running" "streaming"})

(defn- persisted-turn->wire
  "Project one durable engine turn into the canonical role/content message shape."
  [sid row]
  (let
    [id
     (str (:id row))

     status
     (let
       [raw (some-> (:status row)
                    name)]
       (get persisted-status->wire raw raw))

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
                     (+ (long created-at) (long (:duration-ms row))))}))

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
     (session-entry sid)

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

     persisted-ids
     ;; The PRIMARY dedup key is an exact id match, so it belongs in a SET. The
     ;; old `some` rescanned every persisted row for every live row, and the
     ;; fallback arm compares whole `:content` vectors, so hydrating a session
     ;; with many turns paid O(live x persisted) DEEP comparisons.
     (into #{}
           (keep #(some-> (:id %)
                          str
                          not-empty))
           persisted-rows)

     live
     (->> live0
          (remove (fn [t]
                    (let
                      [engine-id (some-> (:engine_turn_id t)
                                         str)]
                      (if (seq engine-id)
                        ;; A non-blank engine id decides it outright: the fallback
                        ;; arm of `persisted-duplicate-of-live?` REQUIRES a blank
                        ;; one, so it can never fire here.
                        (contains? persisted-ids engine-id)
                        (some #(persisted-duplicate-of-live? t %) persisted-rows)))))
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

(defn list-queued-turns
  "ONLY the still-queued rows for `sid`, oldest-first — the exact slice a tray
   polls for, and nothing else.

   A queued turn lives solely in the in-memory registry overlay: persistence
   never holds one (a row reaches the DB after it RUNS). So this reads the
   overlay and skips [[list-turns]]'s whole-history DB hydration. That matters
   on the wire, not just in the server: a companion polling the backlog every
   5s was pulling the session's ENTIRE turn history — 600KB of completed
   `:content` for a long session — just to learn the queue is empty."
  [sid]
  (let [{:keys [turns turn-order]} (session-entry sid)]
    (->> (or turn-order [])
         (keep #(some-> (get turns %)
                        wire-turn))
         (filterv #(= "queued" (:status %)))
         wire/canonical)))

(defn- with-display-iteration
  "Normalize reasoning and attach the same cached ruff-formatted Python that the
   local TUI paints. The wire therefore remains identical after reconnects.

   `:llm-assistant-message` is dropped: persistence hands it back as a
   `<-json-lazy` DELAY (the raw provider envelope, forced only by a replay), and
   a Delay JSON-encodes to the useless string `\"clojure.lang.Delay@1f2e3d\"` —
   3031 of a real 247-turn session's 3098 iterations shipped exactly that. No
   consumer needs the field; forcing it would instead paste every tool envelope onto the wire.
   `transcript/transcript` already dissoc's it for the same reason."
  [iteration]
  (cond->
    (-> iteration
        (dissoc :llm-assistant-message)
        (update :thinking normalize-thinking-text))
    (seq (:forms iteration))
    (update :forms #(mapv form/with-display-code %))))

(def ^:private in-flight-turn-statuses
  "Persisted turn statuses that mean the turn has NOT finished. The engine writes
   the row at submit and patches it at the terminal frame, so everything else —
   `:success`, `:error`, `:cancelled`, … — is a settled turn."
  #{"running" "streaming" "queued" "pending"})

(defn- transcript-turn
  "Hydrate one persisted turn and attach the canonical TUI/CLI bubble-footer
   strings. Remote channels consume these verbatim; older clients can still use
   the underlying usage/routing fields."
  [db att-by-soul turn]
  (let
    [iteration-rows
     (try (->> (persistance/db-list-session-turn-iterations db (:id turn))
               (mapv with-display-iteration))
          (catch Throwable t
            (tel/log! :warn ["gateway: turn-iteration hydration failed" (:id turn) (ex-message t)])
            []))

     ;; Produced artifacts (matplotlib figures, `attach`ed images) as the
     ;; SAME lean descriptors the live `iteration.completed` frame carries —
     ;; byte-free, so history costs nothing on the wire, and a remote client
     ;; lazy-fetches the bytes from the attachment endpoint. Without this a
     ;; produced image existed only for the seconds the live frame was on
     ;; screen and vanished on the next transcript read.
     atts-by-iter
     (if (seq iteration-rows)
       (try (into {}
                  (map (fn [[iter-id rows]]
                         ;; The HUMAN's own list, filtered once — the very door
                         ;; the live frame and the byte endpoint go through — so
                         ;; a model-only artifact is absent from history too and
                         ;; index N names the same picture on every surface.
                         [(str iter-id) (into [] (remove attachments/hidden-from-user?) rows)]))
                  (persistance/db-list-iterations-attachments-meta db (keep :id iteration-rows)))
            (catch Throwable _ {}))
       {})

     iterations
     (mapv (fn [it]
             (if-let [rows (seq (get atts-by-iter (str (:id it))))]
               (assoc it :attachments (attachment-descriptors (:id it) rows))
               it))
           iteration-rows)

     last-it
     (last iterations)

     tokens
     (cond-> {}
       (:input-tokens turn)
       (assoc "input" (:input-tokens turn))

       (:input-regular-tokens turn)
       (assoc "input_regular" (:input-regular-tokens turn))

       (:input-cache-write-tokens turn)
       (assoc "cache_created" (:input-cache-write-tokens turn))

       (:input-cache-read-tokens turn)
       (assoc "cached" (:input-cache-read-tokens turn))

       (:output-tokens turn)
       (assoc "output" (:output-tokens turn))

       (:output-reasoning-tokens turn)
       (assoc "reasoning" (:output-reasoning-tokens turn)))

     cost
     (when-let [total-cost (or (:total-cost turn) (:cost turn))]
       (cond-> {"total_cost" total-cost}
         (:provider turn)
         (assoc "provider" (:provider turn))

         (:model turn)
         (assoc "model" (:model turn))))

     meta-source
     (cond-> {:duration-ms (:duration-ms turn)}
       (seq tokens)
       (assoc :tokens tokens)

       cost
       (assoc :cost cost)

       (:provider turn)
       (assoc :provider (:provider turn))

       (:model turn)
       (assoc :model (:model turn))

       (:llm-selected last-it)
       (assoc :llm-selected (:llm-selected last-it))

       (:llm-actual last-it)
       (assoc :llm-actual (:llm-actual last-it))

       (contains? last-it :llm-fallback?)
       (assoc :llm-fallback? (:llm-fallback? last-it))

       (seq (:llm-routing-trace last-it))
       (assoc :llm-routing-trace (:llm-routing-trace last-it)))

     ;; The footer summarises a FINISHED turn. A row that is still RUNNING
     ;; already carries the last completed iteration's `:llm-actual`, so
     ;; hydrating one mid-turn shipped a bare "provider/model" line — no
     ;; tokens, no cost, no duration — and every channel painted a
     ;; settled-looking footer under work that is still in flight.
     ;; The engine's terminal status is an OPEN set (`:success`, `:error`, …),
     ;; so ask the negative question: only a row the engine still has in flight
     ;; is withheld.
     in-flight?
     (contains? in-flight-turn-statuses
                (some-> (:status turn)
                        name))

     meta-summary
     (when-not in-flight? (fmt/meta-summary-line meta-source))

     fallback-note
     (when-not in-flight? (fmt/meta-fallback-note meta-source))]

    (cond-> (assoc turn :iterations iterations)
      (seq (get att-by-soul (str (:id turn))))
      (assoc :attachments (get att-by-soul (str (:id turn))))

      meta-summary
      (assoc :meta-summary meta-summary)

      fallback-note
      (assoc :meta-fallback-note fallback-note))))

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

         (wire/canonical (mapv (partial transcript-turn db att-by-soul) turns)))
       (catch Throwable t
         (tel/log! :warn ["gateway: transcript hydration failed" (ex-message t)])
         [])))

(def ^:private TRANSCRIPT_PAGE_MAX_BYTES
  "Byte ceiling for ONE WINDOWED transcript page — the fact that makes a page a
   page, because turn COUNT does not bound BYTES. Real sessions carry single
   turns of 5 MB (one grep whose result held a 2.9 MB file), so the newest 24
   turns of a 38-turn session encode to 9.5 MB: the exact cost paging exists to
   avoid, silently back. A windowed request therefore hydrates NEWEST-FIRST and
   stops BEFORE the encoded rows exceed this many bytes — the page shrinks in ROWS
   instead of growing without bound, and the rows it drops raise `:offset`,
   which is precisely where the client's next `load earlier` resumes. Never
   applies to an UNWINDOWED request (the TUI's whole-transcript GET). At least
   one row always comes back, so an oversized turn still arrives and paging
   always advances. Override with `VIS_GATEWAY_TRANSCRIPT_PAGE_MAX_BYTES`;
   values <= 0 use the default of 2 MiB.

   A `delay`, never an eager read: `native-image` initializes this namespace at
   BUILD time, so a top-level `getenv` would ship the BUILDER's answer."
  (delay (let
           [configured (some-> (System/getenv "VIS_GATEWAY_TRANSCRIPT_PAGE_MAX_BYTES")
                               str/trim
                               parse-long)]
           (if (pos? (long (or configured 0))) (long configured) (* 2 1024 1024)))))

(defn- budgeted-page-turns
  "Hydrate `window` (oldest-first rows) from its NEWEST row BACKWARDS, stopping
   AT the row that first exceeds `TRANSCRIPT_PAGE_MAX_BYTES` — that row is still
   INCLUDED, so the page overshoots by at most one turn and a page is never
   empty. Hydration is where a page's cost lives, so a stopped page never pays
   for the rows it does not send. Returns `[rows dropped]` — `rows` still
   oldest-first, `dropped` the number of OLDEST window rows left out, which the
   caller adds to `:offset`."
  [db att-by-soul window]
  (loop
    [i
     (dec (count window))

     rows
     '()

     bytes
     0]

    (if (neg? i)
      [(vec rows) 0]
      (let
        [row
         (wire/canonical (transcript-turn db att-by-soul (nth window i)))

         bytes'
         (+ (long bytes)
            (long (alength (.getBytes ^String (wire/json-str row)
                                      java.nio.charset.StandardCharsets/UTF_8))))]

        ;; The busting row is KEPT, not deferred. Deferring it silently ate the
        ;; newest turn that carried a user image: one 3 MB inline upload two
        ;; turns from the head cut a 24-turn page down to the one turn above it,
        ;; so re-entering the session showed no image at all until the user
        ;; happened to scroll back. Overshoot is bounded by that single row, and
        ;; the page still always advances (`dropped` counts the rows below it).
        (if (> bytes' (long @TRANSCRIPT_PAGE_MAX_BYTES))
          [(vec (conj rows row)) i]
          (recur (dec i) (conj rows row) bytes'))))))

(defn transcript-page
  "A WINDOW of `sid`'s transcript, hydrated LAZILY — the whole point is that only
  the rows in the window pay for iteration/attachment hydration, which is where
  a big session's cost lives (a 247-turn session: ~26 ms to list, ~750 ms to
  hydrate all of it, ~50 ms to hydrate the newest 30).

  The cursor is an INDEX into the oldest-first list, NOT `:position` — positions
  are neither unique nor monotonic in practice (one real 247-turn session has
  172 distinct positions), so they cannot page anything. New turns only ever
  append, so an offset counted from the OLDEST row is stable while paging
  backwards.

  A windowed request is ALSO capped in bytes (`TRANSCRIPT_PAGE_MAX_BYTES`): the
  newest rows are hydrated first and the oldest ones fall out of the page, so
  `:offset` can come back HIGHER than the one asked for. Clients must page from
  the RETURNED `:offset`, never from their own arithmetic.

  `opts`: `:limit` window size (nil = every row, unbudgeted — the TUI's
  whole-transcript read), `:offset` 0-based start in the oldest-first list
  (nil = the NEWEST `:limit` rows).

  Returns `{:turns <oldest-first window> :total <turn count> :offset <window
  start> :has-more <older rows exist>}`."
  [sid {:keys [limit offset]}]
  (try
    (let
      [db
       (lp/db-info)

       all
       (vec (persistance/db-list-session-turns db sid))

       total
       (long (count all))

       lim
       (long (if limit (max 0 (min (long limit) total)) total))

       start
       (long (if offset (max 0 (min (long offset) total)) (max 0 (- total lim))))

       end
       (long (min total (+ start lim)))

       window
       (subvec all start end)

       att-by-soul
       (try (persistance/db-list-turns-attachments db (map :id window)) (catch Throwable _ {}))

       [rows dropped]
       (if limit
         (budgeted-page-turns db att-by-soul window)
         [(wire/canonical (mapv (partial transcript-turn db att-by-soul) window)) 0])

       from
       (long (+ start (long dropped)))]

      {:turns rows :total total :offset from :has-more (pos? from)})
    (catch Throwable t
      (tel/log! :warn ["gateway: transcript page hydration failed" (ex-message t)])
      {:turns [] :total 0 :offset 0 :has-more false})))

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
                       (mapv with-display-iteration))
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
  (update-session! sid
                   (fn [entry]
                     (cond-> (update-in entry [:turns tid] merge patch)
                       (= tid (:current-turn entry))
                       (assoc :current-turn nil)))))

(defn- turn-terminal-payload
  "Payload for a TERMINAL turn event (`turn.completed` / `turn.failed` /
   `turn.cancelled`), carrying the SAME `idempotency_key` correlation id that
   `turn.started` stamps.

   Channels reconcile their optimistic bubble against the terminal event
   INDEPENDENTLY of the blocking submit/attach worker (the TUI's
   `:sync-turn-terminal`), and a tab whose submit ack is still in flight knows
   only the correlation id it minted - never the gateway turn id. Emitting the
   terminal without that id made the whole independent path unreachable in
   exactly the case it exists for: the spinner kept running and the answer
   painted late, when the stranded worker finally returned."
  [sid tid status]
  (let
    [turn
     (turn-record sid tid)

     key
     (:idempotency_key turn)

     ;; A failed turn ships its settled content and reason too, so the terminal
     ;; event alone describes the failure. A channel that reconciles the terminal
     ;; independently of the blocking worker has nothing else to paint, and the
     ;; persisted row remains the durable source for later readers.
     content
     (when (= "failed" status) (not-empty (vec (:content turn))))

     error
     (when (= "failed" status)
       (not-empty (some-> (:error turn)
                          str)))]

    (cond-> {:turn_id tid :status status}
      key
      (assoc :idempotency_key key)

      content
      (assoc :content content)

      error
      (assoc :error error))))

(defn turn-answer-text
  "Plain-text projection of ONE finished turn's answer content, or nil.

   Read straight from the live registry (`finish-turn!` has already merged the
   content patch by the time a terminal event is appended), so this costs one
   map lookup and never touches the DB. Exists for the push alert: a
   notification that only says \"turn finished\" makes you open the app to learn
   anything at all."
  [sid tid]
  (try (some-> (:content (turn-record sid tid))
               content/text-projection
               str/trim
               not-empty)
       (catch Throwable _ nil)))

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

(declare drain-next-queued!
         drop-cancelled-backlog!
         next-drainable-turn
         pause-queue!
         resume-queue!
         after-turn-terminal!)
(def ^:private TURN_STALL_TIMEOUT_MS
  "Daemon backstop: force-cancel a turn wedged with NO meaningful progress for
   this long. Set ABOVE svar's 5-minute semantic stream timeout so it fires ONLY
   when svar's own idle/semantic stream watchdogs miss a stalled connection — the
   'calling the provider… 8m, nothing moving' hang that freezes the session's
   turn queue. Gated on [[stall-exempt-phases]] (NOT just `:provider-call`) so a
   legitimately long tool / Python-eval phase is never force-cancelled, while a
   wedge in ANY engine/provider-internal phase — including the between-iteration
   `:iteration-final` gap where the next provider call is built / auth headers
   refreshed — is still caught. Without this, such a wedge emits no terminal
   event, so the turn never finishes and the queued backlog never drains."
  (* 6 60 1000))

(def ^:private TURN_FIRST_OUTPUT_TIMEOUT_MS
  "Tight ceiling for a STARTED turn that has produced nothing at all: no token,
   no reasoning, no tool call — only the `:provider-call` marker saying the
   request left the building.

   [[TURN_STALL_TIMEOUT_MS]] is sized for a stream that died MID-answer and sits
   deliberately above svar's 5-minute semantic watchdog. Applying that same budget
   to silence from the very first byte is what let the reported turn sit 3m47s
   with zero iterations, freezing the session queue behind it while the user
   watched a spinner. A streaming provider that has not emitted one byte in two
   minutes is wedged, not slow — and the resulting failure is TRANSIENT, so the
   queue re-runs the message on backoff and an over-eager trip costs one retry."
  (* 2 60 1000))

(def ^:private first-output-exempt-phases
  "Phases where a started turn may legitimately still owe its first output:
   queueing for the process-wide execution permit behind another session's turn is
   waiting, not wedging. Such a turn is still held to the full
   [[TURN_STALL_TIMEOUT_MS]]."
  #{:awaiting-permit})

(defn- stall-detail-text
  "What the watchdog actually observed, recorded by it on the shared `stall` atom.
   The failure the user reads must name the real idle time and phase — quoting
   [[TURN_STALL_TIMEOUT_MS]] told a turn killed at the first-output ceiling that it
   had waited six minutes."
  [stall]
  (or (:stall-detail (some-> stall
                             deref))
      (str "no output for " TURN_STALL_TIMEOUT_MS "ms")))

(defn- stall-attribution
  "WHO went quiet: the provider and model named by the live `:provider-call`
   marker, as `\"<provider> / <model>\"`. nil when the turn died before reaching a
   provider at all — attribution is stamped BY that marker, so there is nothing
   truthful to say before it."
  [stall]
  (let
    [{:keys [provider model]}
     (some-> stall
             deref)

     p
     (some-> provider
             name
             str/trim
             not-empty)

     m
     (some-> model
             str
             str/trim
             not-empty)]

    (cond (and p m) (str p " / " m)
          :else (or p m))))

(defn- stall-reached-provider?
  "Whether this turn ever got as far as the provider. The `:provider-call` marker
   is what stamps attribution, and streamed output can only have come from a
   provider — so before either of those, the turn stalled with NO provider
   involved at all: queueing for the execution permit, or building/entering its
   own session engine."
  [stall]
  (let
    [{:keys [provider produced?]} (some-> stall
                                          deref)]
    (boolean (or provider produced?))))

(defn- stall-failure-text
  "The stall failure a human reads. `Provider stream stalled: no output for
   362142ms in phase :provider-call` named the symptom and nothing else, so the
   card could not say that it was github-copilot-enterprise / claude-opus-5 whose
   connection died — the log knew, the turn did not.

   A turn that never reached a provider must not blame one either: a turn parked
   on its own session's wedged engine died with `Provider stream stalled: no
   output for 360047ms in phase :awaiting-permit`, sending the reader after a
   network that was never touched."
  [stall]
  (let [who (stall-attribution stall)]
    (if (stall-reached-provider? stall)
      (str "Provider stream stalled" (when who (str " (" who ")")) ": " (stall-detail-text stall))
      (str "Turn stalled before reaching the provider: " (stall-detail-text stall)))))

(def ^:private stall-exempt-phases
  "Phases where a running turn may legitimately produce NO chunk for a long time
   — a slow shell-run / Python-eval / native tool. The stall watchdog NEVER
   force-cancels while the live phase is one of these. EVERY other phase
   (provider-call, reasoning/content streaming, response-parse, and the
   between-iteration `:iteration-final` gap) is engine/provider-internal and must
   never sit idle for `TURN_STALL_TIMEOUT_MS`."
  #{:form-start :form-result :tool-start :shell-run :shell-bg})

(def ^:private stall-lifecycle-phases
  "Phases whose chunks are engine LIFECYCLE markers, not model output. `loop`
   emits `{:phase :provider-call}` the moment it STARTS the call, so counting it
   as output made a turn that never received one token look like a producing turn:
   it kept the full cancel grace and the full [[TURN_STALL_TIMEOUT_MS]] ceiling.
   The marker still moves the idle deadline — the wait legitimately begins there —
   it just no longer claims the model said anything."
  #{:provider-call})

(defn- advance-turn-stall-state
  "Records the live phase, but moves the deadline only for real progress.
   Streaming callbacks carry cumulative text plus `:delta`; an empty delta is
   an SSE heartbeat/no-op and must not keep a wedged turn alive.

   `:produced?` is stricter than the deadline: only actual model output sets it,
   never a [[stall-lifecycle-phases]] marker. It is the flag that separates a turn
   the provider is answering slowly from one it never answered at all."
  [state chunk now]
  (let
    [meaningful?
     (or (not (contains? chunk :delta)) (seq (:delta chunk)) (:done? chunk))

     output?
     (and meaningful? (not (contains? stall-lifecycle-phases (:phase chunk))))]

    (cond-> (assoc state :phase (:phase chunk))
      ;; Attribution is stamped by the `:provider-call` marker and kept: the
      ;; streaming chunks that follow carry none of their own, and the failure
      ;; card must still be able to name the connection that went silent.
      (:provider chunk)
      (assoc :provider (:provider chunk))

      (:model chunk)
      (assoc :model (:model chunk))

      meaningful?
      (assoc :last-ms now)

      output?
      (assoc :produced? true))))


(defonce ^:private turn-terminal-claims
  ;; `[sid tid]` -> the claim key of the run that owns the turn's one terminal
  ;; landing. The token identity prevents racing worker/watchdog/cancel paths
  ;; from publishing more than one terminal event.
  (java.util.concurrent.ConcurrentHashMap.))

(defn- turn-terminal-claim-key
  "Identity of ONE run of a turn. Every launch mints a fresh cancellation token
   (`drain-next-queued!`, `submit-turn!`), so the token's flag atom identifies
   the run; a token-less turn falls back to a shared sentinel."
  [cancel-token]
  (or (cancellation/cancellation-atom cancel-token) ::untokened-turn))

(defn- current-turn-run?
  "True when `cancel-token` belongs to the turn's live run. A turn removed from
   the registry cannot pin a session and is treated as live."
  [sid tid cancel-token]
  (if-let [turn (turn-record sid tid)]
    (= (turn-terminal-claim-key cancel-token) (turn-terminal-claim-key (:cancel-token turn)))
    true))

(defn- claim-turn-terminal!
  "Claim the RIGHT to land `tid`'s terminal record + event. True exactly once
   per RUN of the turn.

   Every path that finishes a turn (the worker's success and failure arms, the
   permit-denied cancel, the post-cancel backstop) goes through here, so a turn
   can never emit two terminals and — more importantly — a worker WEDGED between
   the engine unwinding and its terminal append can be overtaken: the backstop
   lands the terminal, and if the worker ever thaws its landing is a no-op.

   A turn id is single-use. Only the run whose token is current in the registry
   may claim its terminal."
  [sid tid cancel-token]
  (if-not (current-turn-run? sid tid cancel-token)
    false
    (let
      [^java.util.concurrent.ConcurrentHashMap claims
       turn-terminal-claims

       k
       [sid tid]

       claim
       (turn-terminal-claim-key cancel-token)

       prev
       (.putIfAbsent claims k claim)]

      (cond (nil? prev) true
            (= prev claim) false
            :else (.replace claims k prev claim)))))

#_{:clj-kondo/ignore [:unused-private-var]}
(defn- release-turn-terminal-claim!
  "Forget `tid`'s terminal claim. Used only to isolate tests; production turns are
   single-use and are never re-queued under the same id."
  [sid tid]
  (.remove ^java.util.concurrent.ConcurrentHashMap turn-terminal-claims [sid tid])
  nil)

(def ^:private CANCEL_TERMINAL_GRACE_MS
  "How long a cancelled turn's worker gets to land its OWN terminal before the
   backstop lands one for it.

   A worker unwinding on a fired token normally finishes in milliseconds. When it
   does not, it is stuck somewhere `cancel!` cannot reach (the observed case: the
   between-turns Python GC parked on a GIL held by another session's shell), and
   the terminal event sits BEHIND that park — so the session stays pinned to a
   turn nobody is running and its queued backlog never drains."
  30000)

(def ^:private SILENT_CANCEL_TERMINAL_GRACE_MS
  "Grace for a cancelled turn whose engine never produced ONE chunk.

   A user stop lands empty content either way, so a turn that never streamed has
   nothing for its worker to flush and no open block to close. All the full grace
   buys such a turn is half a minute of \"Vis is cancelling\" on screen after the
   user already pressed stop — the observed wedge: a turn that sat 3m47s without
   a single iteration, then took the whole 30s backstop to settle. The terminal
   claim still guarantees exactly one landing if the worker thaws mid-grace."
  2000)

(defn- cancel-terminal-grace-ms
  "How long THIS cancelled turn's worker may keep its own terminal: the full
   [[CANCEL_TERMINAL_GRACE_MS]] once it has produced output, the much shorter
   [[SILENT_CANCEL_TERMINAL_GRACE_MS]] while it has produced none."
  ^long [stall]
  (if (:produced? (some-> stall
                          deref))
    (long CANCEL_TERMINAL_GRACE_MS)
    (long SILENT_CANCEL_TERMINAL_GRACE_MS)))

(defn- start-cancel-terminal-backstop!
  "Daemon backstop for ONE cancelled turn: if `tid` is still the session's current
   turn and nobody has landed its terminal `grace-ms` after the cancel, land
   `turn.cancelled` here and drain the queue.

   This is the last line of defence for a wedged worker: the stall watchdog only
   fires `cancel!`, and a thread parked in uninterruptible native code (or any
   post-engine cleanup) ignores it forever."
  [sid tid cancel-token grace-ms land!]
  (doto (Thread. ^Runnable
                 (fn []
                   (try (Thread/sleep (long grace-ms))
                        (when (= tid (:current-turn (session-entry sid)))
                          (tel/log!
                            :warn
                            ["gateway: cancelled turn never landed a terminal — backstopping" tid
                             (str grace-ms "ms after cancel")])
                          (land! sid tid cancel-token)
                          ;; The terminal we just synthesized is only half the
                          ;; truth: the worker never came back, so its thread may
                          ;; still own this session's ENGINE lock. Landing the
                          ;; event alone reports the session idle while every
                          ;; later turn parks on that lock forever — started, no
                          ;; events, deaf to its own cancel. Condemn the engine
                          ;; so the next turn abandons the wedged context and
                          ;; runs on a fresh one instead of queueing behind a
                          ;; thread that is never coming back.
                          (try (lp/condemn-env! sid) (catch Throwable _ nil)))
                        (catch InterruptedException _ nil)
                        (catch Throwable _ nil)))
                 (str "gateway-turn-cancel-backstop-" tid))
    (.setDaemon true)
    (.start))
  nil)

(def ^:private TURN_LAUNCH_TIMEOUT_MS
  "How long a LAUNCHED turn may report NO worker activity at all — not a phase,
   not even the stamp the worker writes before it queues for an execution permit
   — before it is declared orphaned.

   `turn.started` is on the wire and `:current-turn` points at the turn the
   moment `launch-turn-worker!` runs, but EVERYTHING that could finish it lives
   in the worker body. When that body never begins — a throw between the started
   event and the worker future, a cancel racing the launch that no hook observed,
   a drain path whose exception a daemon thread swallowed — nothing ever lands a
   terminal: the session sits on a spinner that cannot be answered, retried or
   cancelled. Entering the body takes microseconds, so a minute is pure slack."
  60000)

(defn- turn-watchdog-live?
  "True while THIS run of `tid` still owes the session a terminal.

   The session pin is the primary signal, but a turn whose record still says
   `running` is unfinished even when something else took the `:current-turn`
   slot — and that clash is exactly how a turn ends up with `turn.started` and no
   terminal. Guarding on the pin ALONE let the watchdog exit silently and leave
   the orphan behind."
  [sid tid cancel-token]
  (let
    [entry
     (session-entry sid)

     turn
     (get-in entry [:turns tid])]

    (and (current-turn-run? sid tid cancel-token)
         (or (= tid (:current-turn entry)) (= "running" (:status turn))))))

(defn- fail-orphaned-turn!
  "Land `turn.failed` for a turn nobody else finished. Claim-guarded, so it is a
   no-op the instant a real terminal exists.

   Reported as a STALL-class failure (`:stalled? true`), never a user stop. The
   failed request remains terminal and any distinct queued backlog is held for an
   explicit resume."
  [sid tid cancel-token reason]
  (when (claim-turn-terminal! sid tid cancel-token)
    (tel/log! :error ["gateway: orphaned turn — landing turn.failed" tid reason])
    (finish-turn! sid
                  tid
                  {:status "failed"
                   :role "assistant"
                   :content [(content/error "turn_failed" reason false)]
                   :error reason
                   :completed_at (System/currentTimeMillis)})
    (append-event! sid "turn.failed" (turn-terminal-payload sid tid "failed"))
    (emit-context-updated! sid)
    (after-turn-terminal! sid tid {:failed? true :cancel-token cancel-token :stalled? true})
    true))

(defn- start-turn-stall-watchdog!
  "Daemon thread guarding ONE turn from `turn.started` — not from its first chunk
   — until a terminal exists for it.

   While the turn still owes a terminal ([[turn-watchdog-live?]]) it polls the
   shared `stall` atom (`run-turn!` records the live phase, the last
   meaningful-progress wall-clock, and whether the model has produced anything at
   all; the worker stamps `:started?` the moment its body begins). Three ceilings:

     - a turn whose worker body NEVER began trips after
       [[TURN_LAUNCH_TIMEOUT_MS]] — the orphaned launch;
     - a started turn that has produced NO output yet trips after
       [[TURN_FIRST_OUTPUT_TIMEOUT_MS]] — the provider that never answered;
     - a started turn that streamed and then went quiet in a non-exempt phase
       trips after [[TURN_STALL_TIMEOUT_MS]] — the stalled provider stream.

   Tripping cancels the token, closing the in-flight stream so the blocked worker
   unwinds and the queue drains. A wedged worker can ignore `cancel!` forever
   (uninterruptible native code) — and a launch that never produced a worker has
   nothing to unwind at all — so the watchdog then GUARANTEES the terminal: if
   nobody landed one within this turn's [[cancel-terminal-grace-ms]] it lands
   `turn.failed` itself. Every `turn.started` therefore ends in a terminal event.

   Self-terminating: exits as soon as the turn no longer owes a terminal."
  [sid tid cancel-token stall]
  (let
    [check-ms (-> (min (long TURN_STALL_TIMEOUT_MS)
                       (long TURN_FIRST_OUTPUT_TIMEOUT_MS)
                       (long TURN_LAUNCH_TIMEOUT_MS))
                  (quot 8)
                  (max 25)
                  (min 20000))]
    (doto
      (Thread.
        ^Runnable
        (fn []
          (try
            (loop []

              (Thread/sleep check-ms)
              (when (turn-watchdog-live? sid tid cancel-token)
                (let
                  [{:keys [phase last-ms started? produced?]} @stall
                   idle-ms (- (System/currentTimeMillis) (long (or last-ms 0)))
                   ;; Nothing from the model yet — and not merely queueing
                   ;; for the execution permit.
                   silent?
                   (and started? (not produced?) (not (contains? first-output-exempt-phases phase)))
                   ceiling (long (cond (not started?) TURN_LAUNCH_TIMEOUT_MS
                                       silent? TURN_FIRST_OUTPUT_TIMEOUT_MS
                                       :else TURN_STALL_TIMEOUT_MS))
                   tripped? (if started?
                              (and (not (contains? stall-exempt-phases phase)) (>= idle-ms ceiling))
                              (>= idle-ms ceiling))]

                  (if tripped?
                    (let
                      [detail (cond (not started?) (str "no worker activity for " idle-ms "ms")
                                    silent? (str "no output at all for " idle-ms
                                                 "ms since the turn started, in phase " phase)
                                    :else (str "no output for " idle-ms "ms in phase " phase))
                       _ (swap! stall assoc :stalled? true :stall-detail detail)
                       reason (if started?
                                (stall-failure-text stall)
                                (str "turn never started running: " detail))]

                      (tel/log! :warn
                                ["gateway: turn made no progress — force-cancelling" tid reason])
                      (cancellation/cancel! cancel-token
                                            (if started? :stall-watchdog :launch-watchdog))
                      ;; Last line of defence: the cancel normally makes the
                      ;; worker (or the cancel hook) land the terminal. When
                      ;; neither can, land it here — a `turn.started` with no
                      ;; terminal is what wedges a session forever. A turn
                      ;; that produced nothing has nothing to flush, so it
                      ;; gets the short grace.
                      (Thread/sleep (cancel-terminal-grace-ms stall))
                      (when (turn-watchdog-live? sid tid cancel-token)
                        (fail-orphaned-turn! sid tid cancel-token reason)))
                    (recur)))))
            (catch InterruptedException _ nil)
            (catch Throwable t
              (tel/log! :error ["gateway: turn watchdog failed" tid (ex-message t)]))))
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
           engine-opts attachments display-request stall]}]
  (let
    [caller-on-chunk
     (get-in engine-opts [:hooks :on-chunk])

     ;; phase -> last emitted cumulative length and timestamp
     last-delta-ms
     (volatile! {})

     started-blocks
     (volatile! #{})

     ;; Blocks opened and NOT yet closed. An iteration's reasoning/prose block
     ;; used to stay open until the TERMINAL flush, so a 72-iteration turn
     ;; streamed 146 half-open blocks for ten minutes and closed them all in one
     ;; 3ms burst at the end: every consumer that renders an open reasoning
     ;; block as live kept the LAST thinking on screen, as if the work never
     ;; finished. Close them at the iteration boundary that actually ended them.
     open-blocks
     (volatile! #{})

     close-blocks!
     (fn [block-ids]
       (doseq [block-id block-ids]
         (vswap! open-blocks disj block-id)
         (append-event! sid "content.block.completed" {:turn_id tid :block_id block-id})))

     close-iteration-blocks!
     (fn [iteration]
       (let [suffix (str ":" (long (or iteration 0)))]
         (close-blocks! (filter #(str/ends-with? (str %) suffix) @open-blocks))))

     on-chunk
     (fn [chunk]
       ;; Empty cumulative stream callbacks are transport heartbeats, not model
       ;; progress. Keep their live phase for diagnostics without moving the
       ;; stall deadline.
       (when stall (swap! stall advance-turn-stall-state chunk (System/currentTimeMillis)))
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
                 (when (and (not= phase :tool-preview) (not (contains? @started-blocks block-id)))
                   (vswap! started-blocks conj block-id)
                   (vswap! open-blocks conj block-id)
                   (append-event! sid
                                  "content.block.started"
                                  {:turn_id tid
                                   :block (if (= phase :reasoning)
                                            (content/reasoning block-id "" "private")
                                            (content/prose block-id ""))}))
                 (vswap! last-delta-ms assoc stream-key {:ms now :len (count cumulative)}))
               (let [[type store? payload] (chunk->event chunk)]
                 (append-event! sid type (assoc payload :turn_id tid) {:store? store?}))
               ;; The iteration is over — its live blocks are settled text now.
               (when (= phase :iteration-final) (close-iteration-blocks! (:iteration chunk))))))
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

           display-request
           (assoc :display-text display-request)

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
         (try (answer-content answer)
              (catch Throwable _
                ;; answer-content threw — the loop's terminal fallback didn't
                ;; pass content validation. Build readable content from the
                ;; result error so the user sees the real provider failure
                ;; (429, quota exhausted) instead of the misleading
                ;; "Final answer must be canonical content or Markdown prose".
                (let [err (or (:error result) (some :error (reverse (:trace result))))]
                  (or (when (some? err) (seq (provider-error/provider-error-content err)))
                      [(content/error "turn_failed" "Turn failed" false)]))))

         stalled?
         (boolean (and stall (:stalled? @stall)))

         status
         (cond stalled? "failed"
               (= :cancelled (:status result)) "cancelled"
               (= :error (:status result)) "failed"
               needs-input? "suspended"
               :else "completed")

         ;; A "failed" turn must NEVER ship empty. Channels reconcile the
         ;; terminal event independently of the blocking worker; with no content
         ;; and no :error all they can paint is a fabricated bare "Turn failed."
         ;; row - the unstyled line reported next to a lost answer.
         failure-code
         (cond stalled?
               (if (stall-reached-provider? stall) "provider_stream_stalled" "turn_stalled")
               (and (= "failed" status) (empty? content-blocks)) "turn_failed")

         failure-text
         (cond stalled? (stall-failure-text stall)
               failure-code (or (some-> (:error result)
                                        str
                                        str/trim
                                        not-empty)
                                (some-> (:message result)
                                        str
                                        str/trim
                                        not-empty)
                                "The turn failed before producing any output."))

         patch
         {:status status
          :role "assistant"
          :content (cond-> content-blocks
                     failure-code
                     (conj (content/error failure-code failure-text true)))
          :is_needs_input needs-input?
          ;; the ENGINE's persisted row id - list-turns dedups the
          ;; DB hydration against it (the gateway tid differs).
          :engine_turn_id (some-> (:session-turn-id result)
                                  str)
          :model (or (get-in result [:cost "model"])
                     (:model result)
                     (some-> stall
                             deref
                             :model))
          :provider (or (get-in result [:cost "provider"])
                        (:provider result)
                        (some-> stall
                                deref
                                :provider
                                name))
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
          :error (when failure-code failure-text)
          :completed_at (System/currentTimeMillis)}]

        (when (claim-turn-terminal! sid tid cancel-token)
          (finish-turn! sid tid patch)
          (record-metrics! sid result)
          (close-blocks! @open-blocks)
          (append-event! sid
                         (case status
                           "failed"
                           "turn.failed"

                           "cancelled"
                           "turn.cancelled"

                           "turn.completed")
                         (turn-terminal-payload sid tid status))
          (emit-context-updated! sid)
          ;; A user cancel means "stop", not "advance": the backlog queued BEFORE
          ;; the cancel is DROPPED with it (`drop-cancelled-backlog!`) so nothing
          ;; can resurrect it later; the channel keeps its own copy and pulls it
          ;; back into the editor. A message submitted AFTER the cancel fired is
          ;; the OPPOSITE intent — "stop that, run THIS" — so it survives the drop
          ;; and drains the moment this worker unwinds.
          ;; A STALL force-cancel, though, is a FAILURE not a user stop — the token
          ;; is cancelled either way, so distinguish on the stall flag and drain.
          (after-turn-terminal!
            sid
            tid
            {:failed? (= "failed" status) :cancel-token cancel-token :stalled? stalled?})))
      (catch Throwable t
        (let
          [stalled?
           (boolean (and stall (:stalled? @stall)))

           ;; A USER CANCEL unwinds this worker by THROWING (the engine aborts on
           ;; the fired token), and this path used to call that "failed": it wrote
           ;; `turn.failed`, and `after-turn-terminal!` then saw a failure with a
           ;; backlog and PAUSED the queue with `provider_error` — a TERMINAL pause
           ;; with no auto-resume. One Esc therefore wedged the whole session: the
           ;; next message sat held forever and every channel showed a dead turn
           ;; whose live panel never closed. Esc is not a provider outage — it is
           ;; exactly a cancelled token that did not stall and carries the
           ;; `:cancelling_at` stamp `cancel-turn!` writes.
           user-cancel?
           (boolean (and (not stalled?)
                         (cancellation/cancelled? cancel-token)
                         (some? (:cancelling_at (turn-record sid tid)))))

           status
           (if user-cancel? "cancelled" "failed")

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
           (cond user-cancel? nil
                 stalled? (str (stall-failure-text stall) " (force-cancelled)")
                 :else (ex-message t))]

          (if user-cancel?
            (tel/log! :info ["gateway: turn cancelled by user" tid])
            (tel/log! :error ["gateway: turn worker failed" tid err]))
          (when (claim-turn-terminal! sid tid cancel-token)
            (finish-turn!
              sid
              tid
              (cond->
                {:status status
                 :role "assistant"
                 :content (cond user-cancel? []
                                ;; A provider failure that unwound the worker (rate
                                ;; limit, auth rejection, dead connection) is the SAME
                                ;; failure the in-loop path renders as a styled card.
                                ;; Emitting the bare `ex-message` here is why an error
                                ;; Vis formats perfectly elsewhere sometimes landed in
                                ;; the TUI (and the app) as raw unformatted text.
                                (and (not stalled?) (provider-error/provider-failure? t))
                                (provider-error/provider-error-content t)
                                :else [(content/error "turn_failed" (or err "Turn failed") false)])
                 :completed_at (System/currentTimeMillis)}
                err
                (assoc :error err)

                eval
                (assoc :eval eval)))
            ;; Close every block still OPEN. Without it a cancelled/failed turn
            ;; leaves half-open live panels in every channel — the blank screen the
            ;; TUI showed after Esc.
            (close-blocks! @open-blocks)
            (append-event! sid
                           (if user-cancel? "turn.cancelled" "turn.failed")
                           (turn-terminal-payload sid tid status))
            (emit-context-updated! sid)
            (after-turn-terminal!
              sid
              tid
              {:failed? (not user-cancel?) :cancel-token cancel-token :stalled? stalled?})))))))

(def ^:private WATCHDOG_CANCEL_REASONS
  "Cancel reasons that mean \"nobody ever ran this turn\", never \"the user stopped
   it\": both are [[start-turn-stall-watchdog!]] force-cancelling a turn that made
   no progress."
  #{:stall-watchdog :launch-watchdog})

(defn- cancel-waiting-turn!
  "Land a turn cancelled while waiting for the global execution permit without
   constructing a Python environment. Also the backstop's landing path.

   Claims the turn's ONE terminal, so it is a no-op when the worker already
   landed (or is about to land) its own.

   WHO fired the token decides WHICH terminal this is, because a force-cancel has
   TWO landing paths racing on the same grace: the watchdog's own
   [[fail-orphaned-turn!]] and this one, reached through the backstop or the
   launch's cancel hook. Landing a watchdog cancel as `turn.cancelled` told
   [[after-turn-terminal!]] the turn had ended cancelled but neither stalled nor
   stopped by the user, which drains nothing, re-queues nothing and pauses
   nothing: the request was silently dropped and the whole backlog stayed `queued`
   behind a session with no current turn, so re-sending only added another row
   that could never run. A stall is a failure — delegate it."
  [sid tid cancel-token]
  (if-let [why (WATCHDOG_CANCEL_REASONS (cancellation/cancel-reason cancel-token))]
    (fail-orphaned-turn! sid
                         tid
                         cancel-token
                         (if (= :launch-watchdog why)
                           "turn never started running: force-cancelled with no worker activity"
                           "provider stream stalled: force-cancelled with no output"))
    (when (claim-turn-terminal! sid tid cancel-token)
      (finish-turn! sid
                    tid
                    {:status "cancelled"
                     :role "assistant"
                     :content []
                     :completed_at (System/currentTimeMillis)})
      (append-event! sid "turn.cancelled" (turn-terminal-payload sid tid "cancelled"))
      (emit-context-updated! sid)
      (after-turn-terminal! sid tid {:failed? false :cancel-token cancel-token :stalled? false}))))

(defn- launch-turn-worker!
  [sid tid request
   {:keys [messages model reasoning-default cancel-token queued? extra-body turn-features workspace
           engine-opts attachments display-request]}]
  ;; `turn.started` is the point of no return: from there on the turn is public
  ;; and `:current-turn` points at `tid`, while everything that could finish it is
  ;; still being wired up below. The announcement itself used to sit outside this
  ;; guard, so a throw in its fan-out — like any throw before the watchdog was
  ;; armed — left a turn nobody runs and nobody ends. One try covers the whole
  ;; launch, and the watchdog is armed before the turn is announced.
  (try
    (let
      [stall
       (atom {:phase nil :last-ms (System/currentTimeMillis)})

       ;; Armed BEFORE the turn is announced, so it also covers the window in
       ;; which the announcement — or the worker — fails to come into existence.
       _
       (start-turn-stall-watchdog! sid tid cancel-token stall)

       _
       (append-event! sid
                      "turn.started"
                      (cond->
                        {:turn_id tid
                         :request (or display-request request)
                         :display_request display-request
                         :started_at (or (:started_at (turn-record sid tid))
                                         (System/currentTimeMillis))}
                        queued?
                        (assoc :queued? true)

                        (:idempotency_key (turn-record sid tid))
                        (assoc :idempotency_key (:idempotency_key (turn-record sid tid)))

                        (seq attachments)
                        (assoc :attachments attachments)))

       ;; Single-claim ticket for this turn's ONE terminal landing. `turn.started`
       ;; is already on the wire and `:current-turn` already points at `tid`, so
       ;; EXACTLY one of {the worker body, the cancel hook} must finish the turn.
       ;; Zero would pin the session to a turn nobody is running: the UI shows an
       ;; empty assistant row forever and every later message piles up `queued`
       ;; with nothing to drain it.
       claimed
       (java.util.concurrent.atomic.AtomicBoolean. false)

       worker
       (fn []
         (when (.compareAndSet claimed false true)
           ;; Proof of life for the watchdog: the body BEGAN. Waiting for the
           ;; process-wide permit is legitimate work, so it moves the deadline
           ;; off the launch ceiling and onto the stall ceiling.
           (swap! stall assoc
             :phase :awaiting-permit
             :started? true
             :last-ms (System/currentTimeMillis))
           (if (acquire-turn-permit! cancel-token)
             (do (swap! turns-executing inc)
                 ;; The permit is IN HAND: everything from here on — building or
                 ;; entering this session's engine, then the first provider
                 ;; request — is execution, not queueing. Leaving the
                 ;; `:awaiting-permit` stamp standing made a turn parked on its
                 ;; own session's wedged engine report a phase it had already
                 ;; left, kept it exempt from the first-output ceiling for six
                 ;; minutes, and blamed a provider it never reached.
                 (swap! stall assoc :phase :engine-start :last-ms (System/currentTimeMillis))
                 (try (run-turn! sid
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
                                  :display-request display-request
                                  :stall stall})
                      (finally (release-turn-permit!))))
             (cancel-waiting-turn! sid tid cancel-token))))

       fut
       ;; Turn work can block in embedded Python, native SDKs, or subprocesses.
       ;; Keep it off the virtual-thread scheduler so a pinned cancelled worker
       ;; cannot prevent the next turn from beginning.
       (cancellation/worker-future (str "gateway-turn-" tid) worker {:platform? true})]

      ;; Deliberately NOT `cancellation-set-future!`. That registers a bare
      ;; `.cancel(true)`, and a `FutureTask` cancelled BEFORE its thread enters
      ;; `run` never invokes the body at all — so a cancel racing the launch (or a
      ;; token already cancelled when the queue head drains: app backgrounded,
      ;; view closed, stop pressed) killed the worker after `turn.started` and
      ;; before anything could emit a terminal. THAT is the wedged-session bug.
      ;; Interrupt the worker, then land the cancellation ourselves iff we win the
      ;; claim — winning means the body has not started and never will.
      (cancellation/on-cancel!
        cancel-token
        (fn []
          (try (.cancel ^java.util.concurrent.Future fut true) (catch Throwable _ nil))
          (if (.compareAndSet claimed false true)
            ;; Off the cancelling thread: this lands a terminal and drains the
            ;; queue, which must never run inside an HTTP/UI cancel handler.
            ;; It must also remain runnable while a cancelled turn pins native code.
            (cancellation/worker-future (str "gateway-turn-cancel-" tid)
                                        (fn []
                                          (cancel-waiting-turn! sid tid cancel-token))
                                        {:platform? true})
            ;; The body IS running, so it owns the terminal — unless it never
            ;; gets there. `cancel!` only fires a token; a worker parked in
            ;; uninterruptible code (native GIL, stuck cleanup) ignores it and the
            ;; session stays pinned to a turn nobody runs, with its backlog held.
            (start-cancel-terminal-backstop! sid
                                             tid
                                             cancel-token
                                             (cancel-terminal-grace-ms stall)
                                             cancel-waiting-turn!))))
      fut)
    (catch Throwable t
      (tel/log! :error ["gateway: turn launch failed" tid (ex-message t)])
      (fail-orphaned-turn! sid tid cancel-token (str "turn launch failed: " (ex-message t)))
      nil)))

(defn- left-queued-by-cancel?
  "True when queued turn `head` was submitted BEFORE the session's cancel floor
   — the wall-clock of the last USER cancel, stamped on the entry as
   `:cancel-floor` by [[drop-cancelled-backlog!]]. Such a turn was deliberately
   stopped (the user pressed Esc while it sat in the backlog) so it must NEVER
   auto-start again, no matter which path reaches the queue: a later terminal or
   an attach/resume kick. This is the ONE provenance gate;
   [[drain-next-queued!]] enforces it for every caller.

   The floor is read from ONE entry-level key rather than scanning per-turn
   `:cancelling_at` stamps, because a STALL force-cancel stamps `:cancelling_at`
   too — that is a failure, not a user stop, and its backlog must still run.

   A head queued AFTER the floor (\"stop that, run THIS\") — or a session with no
   user cancel at all — drains normally."
  [entry head]
  (let
    [floor
     (long (or (:cancel-floor entry) 0))

     queued-at
     (long (or (:queued_at head) 0))]

    (and (pos? floor) (< queued-at floor))))

(defn- next-drainable-turn
  "The oldest queued turn for `entry` that may auto-start: the first `queued`
   entry in `:turn-order` that is not [[left-queued-by-cancel?]] (every turn when
   `force?`, the explicit user resume). Returns `[tid turn]` or nil.

   Gating at SELECTION, not after picking the head, is what keeps the queue from
   wedging: a pre-cancel straggler that survived [[drop-cancelled-backlog!]]
   (a submit that raced the cancel sweep, a re-queue) is skipped over rather than
   parked at the head blocking the message queued AFTER the cancel — the \"stop
   that, run THIS\" intent."
  [entry force?]
  (some (fn [tid]
          (let [turn (get-in entry [:turns tid])]
            (when (and (= "queued" (:status turn))
                       (or force? (not (left-queued-by-cancel? entry turn))))
              [tid turn])))
        (:turn-order entry)))

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
  "Start the oldest DRAINABLE queued turn for `sid`, if one exists. Returns the
   started turn.

   THE single place a queued turn becomes running — so it is also the single
   place the cancel provenance gate lives ([[next-drainable-turn]] /
   [[left-queued-by-cancel?]]): a turn queued BEFORE the session's last user
   cancel NEVER auto-starts. Without that gate a later terminal or attach kick
   could resurrect the stopped backlog and fire it as an uninterruptible follow-up — the
   queue-storm bug. `:force?` is the explicit user resume that deliberately
   overrides the gate."
  ([sid] (drain-next-queued! sid nil))
  ([sid {:keys [force?]}]
   (let
     [decision
      (volatile! nil)

      ;; Read the session pin HERE, as the turn starts, to stamp the observable
      ;; turn record. The worker still receives only the raw caller override;
      ;; the engine resolves this persisted model together with its provider.
      pinned-model
      (:model (session-model sid))]

     (update-session!
       sid
       (fn [entry]
         (if (or (nil? entry) (:current-turn entry))
           entry
           (if-let
             [[tid
               {:keys [request messages model reasoning-default cancel-token extra-body
                       turn-features workspace engine-opts attachments display_request]}]
              (next-drainable-turn entry force?)]
             (let
               [token (or cancel-token (cancellation/cancellation-token))
                started-at (System/currentTimeMillis)]

               (vreset! decision
                        {:tid tid
                         :request request
                         :display-request display_request
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
                              (cond-> {:status "running" :cancel-token token :started_at started-at}
                                (or model pinned-model)
                                (assoc :model (or model pinned-model))))))
             entry))))
     (when-let
       [{:keys [tid request display-request messages model reasoning-default cancel-token extra-body
                turn-features workspace engine-opts attachments]}
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
                             :attachments attachments
                             :display-request display-request})
       (get-turn sid tid)))))

(defn drain-idle!
  "Start the oldest queued turn for `sid` IF the session is idle (no turn in
   flight). No-op returning nil otherwise. Lets an attaching channel kick an
   orphaned backlog — submitted from another channel while this one was away —
   into motion the moment a client opens/resumes, instead of letting it sit
   forever.

   The cancel provenance gate is NOT re-implemented here: `drain-next-queued!`
   owns it for every caller, so a backlog the user stopped with Esc can never be
   resurrected by a background attach (tab open, project switch) either.

   Safe to call redundantly: `drain-next-queued!` guards on `:current-turn`."
  [sid]
  (drain-next-queued! sid))

(defn- count-queued [entry] (count (filter #(= "queued" (:status %)) (vals (:turns entry)))))
(defn- pause-queue!
  "Hold the distinct queued backlog after a failed turn. The failed turn remains
   terminal and is never re-queued; only an explicit resume may start the next
   user request."
  [sid {:keys [reason]}]
  (let [captured (volatile! nil)]
    (update-session!
      sid
      (fn [entry]
        (when entry
          (let [held (count-queued entry)]
            (if (pos? (long held))
              (let
                [gen (inc (long (get-in entry [:queue-paused :gen] 0)))
                 paused {:reason reason :held held :gen gen :at (System/currentTimeMillis)}]

                (vreset! captured paused)
                (assoc entry :queue-paused paused))
              entry)))))
    (when-let [{:keys [held]} @captured]
      (append-event! sid "queue.paused" {:reason reason :held held}))))

(defn resume-queue!
  "Clear a paused backlog and start its head. A failed turn is never replayed;
   resume advances only to a distinct queued request. No-op when not paused."
  [sid {:keys [auto?]}]
  (let [was (volatile! false)]
    (update-session! sid
                     (fn [entry]
                       (when entry
                         (when (:queue-paused entry) (vreset! was true))
                         (cond-> (dissoc entry :queue-paused)
                           (not auto?)
                           (dissoc :cancel-floor)))))
    (when @was
      (append-event! sid "queue.resumed" {:is_auto (boolean auto?)})
      (drain-next-queued! sid {:force? (not auto?)}))))

(defn queue-paused-info
  "The live `:queue-paused` marker for `sid` (`{:reason :held :gen …}`), or nil
   when the queue is running."
  [sid]
  (:queue-paused (session-entry sid)))

(defn- drop-cancelled-backlog!
  "Terminally drop every turn still QUEUED for `sid` that was submitted BEFORE
   turn `tid`'s user cancel fired (its `:cancelling_at` stamp). Stop means stop:
   the pre-cancel backlog dies WITH the cancel, in ONE atomic registry swap,
   instead of lingering as a record no path may auto-start.

   This is what makes the queue race-free rather than merely race-guarded. The
   old policy left those rows queued and relied on each CLIENT to delete them by
   turn id — a delete that raced the late-bound id, skipped rows mirrored from a
   sibling, and silently lost on a transport error. Every survivor then either
   resurrected on a later turn's terminal (the same prompt re-running minutes
   later) or sat forever as a ghost \"Queued\" row that also BLOCKED a genuinely
   post-cancel message behind it. Deleting server-side makes client cleanup
   optional, settles any client blocked on such a turn (`turn.queued.deleted` is
   terminal for a waiter), and leaves a message queued AFTER the cancel — the
   \"stop that, run THIS\" intent — untouched at the head.

   The events are appended AFTER the `turn.cancelled` terminal, so a channel
   restores its own queued text into the composer first and only then drops the
   mirrors. Returns the dropped turn records."
  [sid tid]
  (let [dropped (volatile! [])]
    (update-session! sid
                     (fn [entry]
                       (if-let [cancelling-at (get-in entry [:turns tid :cancelling_at])]
                         (let
                           [stale (filterv (fn [[_ turn]]
                                             (and (= "queued" (:status turn))
                                                  (< (long (or (:queued_at turn) 0))
                                                     (long cancelling-at))))
                                    (:turns entry))]
                           (vreset! dropped (mapv second stale))
                           (reduce (fn [e [stale-tid _]]
                                     (-> e
                                         (update :turns dissoc stale-tid)
                                         (update :turn-order
                                                 (fn [order]
                                                   (vec (remove #{stale-tid} order))))
                                         (update :idempotency
                                                 (fn [m]
                                                   (into {} (remove (comp #{stale-tid} val) m))))))
                                   ;; The cancel FLOOR: one entry-level stamp marking "everything
                                   ;; submitted before this instant was stopped". Set only here,
                                   ;; on a real user cancel, so [[left-queued-by-cancel?]] can
                                   ;; refuse a straggler (a re-queue, a racing submit that landed
                                   ;; mid-cancel) long after this sweep.
                                   (assoc entry :cancel-floor (long cancelling-at))
                                   stale))
                         entry)))
    (doseq [turn @dropped]
      (append-event! sid
                     "turn.queued.deleted"
                     {:turn_id (:turn_id turn) :request (:request turn) :reason "cancelled"}
                     {:store? false}))
    @dropped))

(defn- after-turn-terminal!
  "Choose what happens after one terminal turn. Success advances the backlog.
   Failure remains terminal and holds any distinct queued requests for explicit
   resume; Vis never retries or replays a provider request after svar returns.

   A user cancel drops work queued before the cancel and may drain only requests
   submitted afterward. Shutdown cancellation drains nothing."
  [sid tid {:keys [failed? cancel-token stalled?]}]
  (let
    [cancelled?
     (cancellation/cancelled? cancel-token)

     user-cancel?
     (and cancelled? (not stalled?) (some? (:cancelling_at (turn-record sid tid))))

     failed?
     (and failed? (not user-cancel?))

     _
     (when user-cancel? (drop-cancelled-backlog! sid tid))

     drain?
     (cond (not cancelled?) true
           stalled? true
           user-cancel? (boolean (next-drainable-turn (session-entry sid) false))
           :else false)]

    (cond (not drain?) nil
          (not failed?) (let [was-paused (volatile! false)]
                          (update-session! sid
                                           (fn [entry]
                                             (when entry
                                               (when (:queue-paused entry)
                                                 (vreset! was-paused true))
                                               (dissoc entry :queue-paused))))
                          (when @was-paused (append-event! sid "queue.resumed" {:is_auto true}))
                          (drain-next-queued! sid))
          (next-drainable-turn (session-entry sid) false) (pause-queue! sid {:reason "turn_failed"})
          :else nil)))

(defn submit-turn!
  "Submit one turn for `sid`. Async: starts immediately when idle, otherwise queues.

   Returns `{:turn record}` (plus `:idempotent? true` on an idempotency
   replay) or `{:error :session-not-found | :invalid-request, ...}`. One engine
   turn still runs per session; busy submissions become visible queued records."
  [sid
   {:keys [request messages idempotency-key model reasoning-default cancel-token extra-body
           turn-features workspace engine-opts attachments display-request]}]
  (cond
    (or (not (string? request)) (str/blank? request))
    {:error :invalid-request :message "request must be a non-blank string"}
    (nil? (lp/by-id sid)) {:error :session-not-found}
    :else
    (let
      [tid
       (str (java.util.UUID/randomUUID))

       ;; Byte-free image chips, resolved ONCE at submit time so every channel's
       ;; queue row (TUI strip, companion tray) paints the same attachments —
       ;; whether they arrived as inline uploads or as paths inside the text.
       ;; Sized from the attached bytes themselves, which vis stores verbatim.
       previews
       (attachment-previews request attachments workspace)

       request-preview
       (request-preview-text request previews)

       ;; The pin is resolved when a turn actually STARTS, never frozen here.
       ;; A turn that waits in the queue must honour a model picked WHILE it
       ;; waited (companion picker, TUI, another channel) — baking the pin into
       ;; the queued record made every already-queued message run on the model
       ;; that happened to be live at submit, so changing the model mid-session
       ;; appeared to do nothing. Only an EXPLICIT caller model is carried on
       ;; the queued record; `drain-next-queued!` re-reads the pin at drain.
       resolved-model
       (or model (:model (session-model sid)))

       decision
       (volatile! nil)]

      (update-session!
        sid
        (fn [entry]
          ;; Seed via `fresh-entry` (journal high-water), never `{:next-seq 0}`:
          ;; a submit is often the FIRST touch of a session after a daemon
          ;; restart, and zero would renumber under every attached client's
          ;; cursor — their streams would go silent for the whole new turn.
          (let [entry (or entry (fresh-entry sid))]
            (cond (and idempotency-key (get-in entry [:idempotency idempotency-key]))
                  (do (vreset! decision [:idempotent (get-in entry [:idempotency idempotency-key])])
                      entry)
                  (:current-turn entry)
                  (do
                    (vreset! decision [:queued tid])
                    (let [queued-at (System/currentTimeMillis)]
                      (-> entry
                          (assoc :last-active queued-at)
                          (assoc-in
                            [:turns tid]
                            (cond->
                              {:turn_id tid
                               :session_id (str sid)
                               :status "queued"
                               :request request
                               :queued_at queued-at}
                              ;; The submitter's OWN correlation id, echoed back on
                              ;; every wire view of this turn and on turn.queued. A
                              ;; channel paints no queue row of its own, so this is
                              ;; how it recognises which gateway rows are ITS
                              ;; submissions - by ID, never by request TEXT (two
                              ;; identical prompts are indistinguishable by text).
                              idempotency-key
                              (assoc :idempotency_key idempotency-key)

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
                              (assoc :reasoning-default reasoning-default)

                              (seq attachments)
                              (assoc :attachments attachments)

                              (seq previews)
                              (assoc :attachment_previews previews)

                              request-preview
                              (assoc :request_preview request-preview)

                              (not (str/blank? (str display-request)))
                              (assoc :display_request display-request)))
                          (update :turn-order (fnil conj []) tid)
                          (cond->
                            idempotency-key
                            (assoc-in [:idempotency idempotency-key] tid)))))
                  :else (do
                          (vreset! decision [:accepted tid])
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
                                            idempotency-key
                                            (assoc :idempotency_key idempotency-key)

                                            resolved-model
                                            (assoc :model resolved-model)

                                            reasoning-default
                                            (assoc :reasoning-default reasoning-default)

                                            (seq attachments)
                                            (assoc :attachments attachments)

                                            (seq previews)
                                            (assoc :attachment_previews previews)

                                            request-preview
                                            (assoc :request_preview request-preview)

                                            (not (str/blank? (str display-request)))
                                            (assoc :display_request display-request)))
                                (update :turn-order (fnil conj []) tid)
                                (cond->
                                  idempotency-key
                                  (assoc-in [:idempotency idempotency-key] tid)))))))))
      (let [[kind v] @decision]
        (case kind
          :idempotent
          {:turn (get-turn sid v) :idempotent? true}

          :queued
          (do (append-event! sid
                             "turn.queued"
                             (cond-> {:turn_id tid :request request}
                               idempotency-key
                               (assoc :idempotency_key idempotency-key)

                               request-preview
                               (assoc :request_preview request-preview)

                               (seq previews)
                               (assoc :attachment_previews previews)

                               (not (str/blank? (str display-request)))
                               (assoc :display_request display-request))
                             {:store? false})
              {:turn (get-turn sid tid)})

          :accepted
          (let [turn (get-turn sid tid)]
            (launch-turn-worker! sid
                                 tid
                                 request
                                 {:messages messages
                                  :model model
                                  :reasoning-default reasoning-default
                                  :cancel-token (:cancel-token (turn-record sid tid))
                                  :extra-body extra-body
                                  :turn-features turn-features
                                  :workspace workspace
                                  :engine-opts engine-opts
                                  :attachments attachments
                                  :display-request display-request})
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
     (known-sid sid-string)

     turn-id
     (or (get event "turn_id") fallback-turn-id)

     message
     (when sid (get-turn sid turn-id))

     ;; The persisted row is the primary settled source. The terminal event also
     ;; carries the failure content so a listener can render it without racing a
     ;; registry lookup.
     blocks
     (or (not-empty (get message "content")) (not-empty (get event "content")) [])]

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

     ;; We subscribe BEFORE the turn id exists (the subscription must not miss
     ;; our own first events). Until `submit-turn!` returns that id, an arriving
     ;; event cannot be classified, so it is BUFFERED — never assumed to be
     ;; ours. Assuming it handed a SIBLING turn's terminal to this caller.
     inbox
     (atom {:turn-id nil :pending []})

     dispatch!
     (fn [event tid]
       (let
         [type
          (get event "type")

          turn_id
          (get event "turn_id")]

         (cond (= turn_id tid)
               (do (when on-event (on-event event))
                   (when (contains? wire/turn-terminal-event-types type) (deliver terminal event))
                   ;; Our own queued record deleted before it ever ran
                   ;; (pulled back into a sibling's editor): synthesize a
                   ;; cancelled terminal so the blocking submit never hangs.
                   (when (= "turn.queued.deleted" type)
                     (deliver terminal
                              {"type" "turn.completed" "turn_id" turn_id "status" "cancelled"})))
               ;; ANOTHER turn's queue event: forward so the channel can
               ;; mirror the session's queued backlog; never terminal here.
               (contains? queue-mirror-event-types type) (when on-event (on-event event)))))

     handle-event!
     (fn [event]
       (let
         [tid (:turn-id (swap! inbox (fn [s]
                                       (cond-> s
                                         (nil? (:turn-id s))
                                         (update :pending conj event)))))]
         (when tid (dispatch! event tid))))

     ;; Atomically publish the id and take the buffered events, so an event
     ;; racing this hand-off is dispatched exactly once — by us or by its own
     ;; caller, never both and never neither.
     adopt-turn!
     (fn [tid]
       (let [[old _] (swap-vals! inbox assoc :turn-id tid :pending [])]
         (doseq [event (:pending old)]
           (dispatch! event tid))))]

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
           (adopt-turn! turn-id)
           (doseq [event replay]
             (handle-event! event))
           ;; A terminal that landed at/just-before our cursor (an idempotent
           ;; replay of an already-settled turn, or a turn the gateway drained
           ;; and finished before we adopted it) never arrives as a live event.
           ;; Recover it from the stored record so we never block forever.
           (when-not (realized? terminal)
             (let [turn (get-turn sid turn-id)]
               (when (contains? terminal-turn-statuses (get turn "status"))
                 (deliver terminal
                          (assoc turn
                            "type" (if (= "failed" (get turn "status"))
                                     "turn.failed"
                                     "turn.completed"))))))
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
     (let [start-seq (:event_start_seq (turn-record sid tid))]
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
               (do (when on-event (on-event event))
                   (when (contains? wire/turn-terminal-event-types type) (deliver terminal event))
                   ;; The queued record was deleted before it ever ran
                   ;; (pulled back into a sibling's editor): synthesize a
                   ;; cancelled terminal so the attach never hangs.
                   (when (= "turn.queued.deleted" type)
                     (deliver terminal
                              {"type" "turn.completed" "turn_id" tid "status" "cancelled"})))
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
  "Replace the prompt text for a queued turn. Returns the updated turn or an error.

   The row's presentation is re-derived from the NEW text: image chips are
   re-resolved and the stale `:display_request` (which described the text the
   submitter authored BEFORE this edit) is dropped, so an edited row never
   keeps painting the old prompt."
  [sid tid request]
  (cond (or (not (string? request)) (str/blank? request))
        {:error :invalid-request :message "request must be a non-blank string"}
        :else (let
                [decision
                 (volatile! nil)

                 existing
                 (turn-record sid tid)

                 previews
                 (attachment-previews request (:attachments existing) (:workspace existing))

                 request-preview
                 (request-preview-text request previews)]

                (update-session!
                  sid
                  (fn [entry]
                    (let [turn (get-in entry [:turns tid])]
                      (cond (nil? turn) (do (vreset! decision [:missing]) entry)
                            (not= "queued" (:status turn))
                            (do (vreset! decision [:not-queued (:status turn)]) entry)
                            :else (do (vreset! decision [:updated])
                                      (-> entry
                                          (assoc-in [:turns tid :request] request)
                                          (update-in [:turns tid] dissoc :display_request)
                                          (update-in
                                            [:turns tid]
                                            (fn [t]
                                              (cond->
                                                (dissoc t :attachment_previews :request_preview)
                                                (seq previews)
                                                (assoc :attachment_previews previews)

                                                request-preview
                                                (assoc :request_preview request-preview))))
                                          (update-in [:turns tid :messages]
                                                     replace-last-user-message-content
                                                     request)))))))
                (let [[kind status] @decision]
                  (case kind
                    :updated
                    (do (append-event! sid
                                       "turn.queued.updated"
                                       (cond-> {:turn_id tid :request request}
                                         request-preview
                                         (assoc :request_preview request-preview)

                                         (seq previews)
                                         (assoc :attachment_previews previews))
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
    (update-session! sid
                     (fn [entry]
                       (let [turn (get-in entry [:turns tid])]
                         (cond (nil? turn) (do (vreset! decision [:missing]) entry)
                               (not= "queued" (:status turn))
                               (do (vreset! decision [:not-queued (:status turn)]) entry)
                               :else (do (vreset! decision [:deleted])
                                         (-> entry
                                             (update :turns dissoc tid)
                                             (update :turn-order
                                                     (fn [order]
                                                       (vec (remove #{tid} order))))
                                             (update :idempotency
                                                     (fn [m]
                                                       (into {}
                                                             (remove (comp #{tid} val) m))))))))))
    (let [[kind status] @decision]
      (case kind
        :deleted
        (do (append-event! sid "turn.queued.deleted" {:turn_id tid} {:store? false})
            {:status "deleted"})

        :missing
        {:error :turn-not-found}

        :not-queued
        {:error :not-queued :status status}))))

(defn cancel-turn!
  "Fire the cancellation token of a running turn. Returns
   `{:status \"cancelling\"}` or `{:error ...}`.

   `source` is stamped on the token and logged. Every cancel reads downstream
   as one interrupt, so an unattributed cancel leaves a post mortem unable to
   tell a user stop from the daemon stopping its own turn — this line is the
   only durable record of WHO stopped it."
  ([sid tid] (cancel-turn! sid tid :client-cancel-turn))
  ([sid tid source]
   (let [turn (turn-record sid tid)]
     (cond (nil? turn) {:error :turn-not-found}
           (not= "running" (:status turn)) {:error :not-running :status (:status turn)}
           :else (do (tel/log! :info ["gateway: cancelling turn" tid (str "source=" (name source))])
                     ;; Stamp the cancel wall-clock BEFORE firing the token so the
                     ;; unwinding worker can tell post-cancel submissions (drain
                     ;; them: "stop that, run THIS") from the pre-cancel backlog
                     ;; (dropped) — see `drop-cancelled-backlog!`.
                     (update-turn! sid tid #(assoc % :cancelling_at (System/currentTimeMillis)))
                     (some-> (:cancel-token turn)
                             (cancellation/cancel! source))
                     ;; A stop must ALWAYS resolve. Firing the token only unwinds a
                     ;; worker that EXISTS and registered its hook — a turn whose launch
                     ;; never got that far, or whose worker is parked in uninterruptible
                     ;; code, ignored the cancel completely and kept `:current-turn`
                     ;; forever, so Esc did nothing at all and every later message piled
                     ;; up behind a turn nobody was running. Claim-guarded: a no-op the
                     ;; instant a real terminal lands.
                     (start-cancel-terminal-backstop! sid
                                                      tid
                                                      (:cancel-token turn)
                                                      CANCEL_TERMINAL_GRACE_MS
                                                      cancel-waiting-turn!)
                     {:status "cancelling"})))))

(defn cancel-current-turn!
  "Tid-less twin of `cancel-turn!`: fire the cancellation token of the turn
   currently holding `sid`'s `:current-turn` slot, but ONLY when `owner-key` is
   the `idempotency_key` that turn was submitted with. For clients that lost (or
   never learned) the turn id — an Esc that raced the `turn.started` late-bind,
   or a client-side cancel self-heal that dropped its `:gateway-turn-id`. Without
   this, that ghost turn keeps `:current-turn` and every next submit silently
   queues behind it.

   A session is SHARED, so \"whatever is running here\" never proves \"the turn I
   submitted\": an unaddressed cancel used to kill the turn another channel (the
   companion app, the web, a second TUI) was running the moment a client opened
   the session. The correlation id the submitter already sent is the proof, and
   a turn submitted without one is reachable only by `cancel-turn!`'s id-addressed
   route — which every client learns from `turn.started`.

   Returns `{:status \"cancelling\" :turn_id tid}`, `{:error :not-owner :turn_id
   tid}` for someone else's turn, or `{:error :no-running-turn}`."
  [sid owner-key]
  (if-let [tid (:current-turn (session-entry sid))]
    (let [turn-key (:idempotency_key (turn-record sid tid))]
      (if-not (and owner-key turn-key (= (str owner-key) (str turn-key)))
        (do (tel/log! :info
                      ["gateway: refusing tid-less cancel of turn" tid
                       (str "owner=" (pr-str turn-key)) (str "caller=" (pr-str owner-key))])
            {:error :not-owner :turn_id tid})
        (let [res (cancel-turn! sid tid :client-cancel-current)]
          (if (:error res) res (assoc res :turn_id tid)))))
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
                        (do (try (cancellation/cancel! (:cancel-token turn) :gateway-shutdown)
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
  "Empty, fully-built sessions retained per channel. One removes cold-start
   latency without pinning a second unused GraalPy context per channel."
  1)

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

    (put-session! (:id created) {:next-seq 0 :last-active (System/currentTimeMillis)})
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
      (drop-session! (:id session))
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

   Default-workspace creates consume the gateway-owned warm pool and replenish
   it in the background. An explicit `:root` can share that pool only when it
   matches the gateway launch root; other roots, `:workspace-id`, and
   `:external-id` require a purpose-built environment."
  [{:keys [channel external-id workspace-id root] :as opts}]
  (let
    [channel
     (or channel :api)

     opts
     (assoc opts :channel channel)

     pool-eligible?
     (and (nil? external-id)
          (nil? workspace-id)
          (or (nil? root) (= (workspace/normalize-root root) (workspace/trunk-root))))

     pooled
     (when pool-eligible? (pop-prewarmed! channel))]

    (try (let
           [created (if pooled (claim-prewarmed! pooled (:title opts)) (create-session-cold! opts))]
           (when pool-eligible? (request-prewarm! channel))
           (session->wire created))
         (catch Throwable e
           (if pooled
             (do (drop-session! (:id pooled))
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
      (drop-session! id)
      (try (lp/delete! id)
           (catch Throwable e
             (tel/log! :warn
                       ["gateway: failed to discard prewarmed session" (str id) (ex-message e)])))))
  nil)

(defn soul
  "Canonical (string-keyed) wire soul for one session: persisted record + live
   gateway status. Running sessions include their request, start timestamp, and
   the gateway clock sampled in the same response so remote channels can derive
   one elapsed baseline without trusting their device wall clock.

   Carries the SAME `turn_count` / `modified_at` freshness pair the list rows
   get from `session-summary-extras` (one session-scoped query here, not a
   whole-store scan). Without them a client holding only a detail row cannot
   tell that a session moved: its transcript stamp is constant, so a cached
   transcript never revalidates and an unread mark can only count the page it
   happens to hold."
  [sid]
  (when-let [session (lp/by-id sid)]
    (let
      [entry (session-entry sid)
       ;; Liveness is a fact about the MACHINE, not about this process. A turn
       ;; running in a SIBLING vis process is mirrored into this registry only
       ;; once somebody subscribes to that session, so a registry-only answer lit
       ;; up exactly the sessions the asking client had already watched — the
       ;; desktop app and the phone reading the same gateway and being told two
       ;; different fleets. `bus/live-turn-id` is the index every producer writes.
       current-turn-id (or (:current-turn entry) (bus/live-turn-id sid))
       current-turn (get-in entry [:turns current-turn-id])
       last-turn (some->> (:turn-order entry)
                          peek
                          (get (:turns entry)))
       server-time-ms (System/currentTimeMillis)
       stats (try (some-> (lp/db-info)
                          (persistance/db-session-turn-stats sid))
                  (catch Throwable _ nil))
       ;; The session's MODEL PIN, from the same `session_soul` row `by-id` just
       ;; read — no extra query, so every list row carries it and a client never
       ;; has to follow up with `GET /v1/sessions/:sid/model` to name the model it
       ;; will run on. An unflushed pick (600ms debounce) still wins over the row.
       model-pref (let [[pending? v] (smodel/pending-pref sid)]
                    (if pending? v (:model-pref session)))]

      (wire/canonical
        (cond->
          {:id (str (:id session))
           :channel (some-> (:channel session)
                            name)
           :title (:title session)
           :model (:model session) ; the state's ROOT model, NOT the pin below
           :external_id (:external-id session)
           :created_at (:created-at session)
           :owner_id (:owner-id session)
           :project_id (some-> (:project-id session)
                               str)
           :project_name (:project-name session)
           :project_position (:project-position session)
           :status (cond current-turn-id "running"
                         (= "suspended" (:status last-turn)) "suspended"
                         :else "idle")
           ;; Explicit wire-level liveness keeps clients from reverse-engineering
           ;; status/current_turn_id, and answers for the whole machine, so every
           ;; client of this gateway sees the SAME session running.
           :live (boolean current-turn-id)
           :current_turn_id current-turn-id
           :last_active_at (:last-active entry)
           :turn_count (long (or (:turn-count stats) 0))
           :server_time_ms server-time-ms}
          model-pref
          (assoc :model_pref model-pref)

          (:latest-turn-at stats)
          (assoc :modified_at (:latest-turn-at stats))

          (session-opening-line (:first-request stats))
          (assoc :first_request (session-opening-line (:first-request stats)))

          (and current-turn-id (:request current-turn))
          (assoc :running_request (:request current-turn))

          (and current-turn-id (nat-int? (:started_at current-turn)))
          (assoc :running_started_at (:started_at current-turn)))))))

(defn- session-summary-extras
  "Bulk summary decorations for `list-sessions`: per-session `turn_count` +
   `modified_at` (from the ONE grouped `db-session-turn-stats` query `stats`
   already holds for the whole store) and a lean `workspace` map
   ({root repo_root label fork_ms is_draft}) — facts the TUI session picker
   previously fetched with TWO HTTP round-trips PER session (109 sequential
   calls / ~7.5s at 54 sessions). Deliberately NO git status here: that stays in
   the per-session `session-workspace-info`.

   `db` and `stats` are passed IN because the caller has already paid for both:
   this runs over a PAGE, and re-querying per page would put the whole-store
   scan back on every window."
  [souls db stats]
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
                                       :fork-ms (:fork-ms w)
                                       ;; A DRAFT is a per-session clone under
                                       ;; ~/.vis/drafts/<repo>/<label>; without this flag a
                                       ;; client cannot tell its `root` from a real project
                                       ;; root and groups every draft as its own project.
                                       ;; Clients group by `repo_root`, badge on `is_draft`.
                                       :is-draft (boolean (workspace/draft? w))}))
                    (catch Throwable _ nil)))]

            (cond-> (assoc s "turn_count" (long (or (:turn-count st) 0)))
              (:latest-turn-at st)
              (assoc "modified_at" (:latest-turn-at st))

              (session-opening-line (:first-request st))
              (assoc "first_request" (session-opening-line (:first-request st)))

              ws
              (assoc "workspace" ws))))
        souls))

;; Times reach this namespace as ms longs, `Instant`s or legacy `java.util.Date`s
;; depending on which store they came from; ordering must not care which.
(defn- ->epoch-ms
  ^long [value]
  (cond (number? value) (long value)
        (instance? java.time.Instant value) (.toEpochMilli ^java.time.Instant value)
        (instance? java.util.Date value) (.getTime ^java.util.Date value)
        :else 0))

(defn- session-recency-ms
  [session]
  (->epoch-ms
    (or (get session "modified_at") (get session "last_active_at") (get session "created_at"))))

(defn- record-recency-ms
  "`session-recency-ms` for an UNDECORATED session: the same three sources
   (`latest-turn-at` from the grouped stats, the registry's `last-active`, the
   record's `created-at`) that `soul` copies into `modified_at`/`last_active_at`/
   `created_at`, read straight from the cheap facts."
  ^long [record entry st]
  (->epoch-ms (or (:latest-turn-at st) (:last-active entry) (:created-at record))))

(defn- session-order
  "Session ids for `channel` in navigator order, computed from CHEAP facts only:
   the persisted record, the live registry and the one grouped turn-stats query.

   It deliberately does NOT touch `soul` or workspace resolution. That
   per-session decoration is where a listing's time actually goes (measured on a
   448-session store: 120ms of `soul` + 40ms of workspace lookups inside a 257ms
   build), so ordering FIRST is the whole reason a page can pay for its own rows
   instead of the fleet's. Same key as `order-session-summaries`, from the same
   sources, so the cut is the one a fully decorated sort would have made."
  [channel stats]
  (let
    [;; ONE scan for the whole ordering: `sort-by` calls its key fn O(n log n)
     ;; times, and a liveness key that could flip mid-sort is a comparator that
     ;; contradicts itself.
     live (bus/live-turns)]
    (->> (lp/by-channel channel)
         (sort-by (fn [record]
                    (let
                      [id (:id record)
                       entry (session-entry id)]

                      [(if (or (:current-turn entry) (get live (str id))) 0 1)
                       (unchecked-negate (record-recency-ms record entry (get stats (str id))))
                       (str id)])))
         (mapv :id))))

(defn- order-session-summaries
  "Gateway-owned navigator order: live sessions first, then most recently
   active. The id tie-breaker keeps repeated polls deterministic."
  [sessions]
  (->> sessions
       (sort-by (fn [session]
                  [(if (true? (get session "live")) 0 1)
                   (unchecked-negate (long (session-recency-ms session)))
                   (str (get session "id"))]))
       vec))

(defn- session-project-root
  "The PROJECT a session belongs to: its workspace `repo-root` when there is one,
   else its `root`. The same key clients group the list by (a draft is a clone
   under ~/.vis/drafts and reports the repo it was forked from), so a `root=`
   window is exactly one project's column of the navigator."
  [db id]
  (try (when-let [w (resolve-workspace db id)]
         (or (:repo-root w) (:root w)))
       (catch Throwable _ nil)))

(defn list-sessions-page
  "A WINDOW of `list-sessions`, in the same gateway-owned navigator order:
   `{:sessions rows :total n :offset o :limit l :order-digest s :has-more bool}`.

   `nil` limit means \"the rest\", so `(list-sessions-page channel nil)` is the
   whole fleet and older callers keep their full list.

   `:root` narrows the whole listing to ONE project before the window is cut, so a
   client paging a project asks the gateway for that project's page instead of
   downloading the fleet and slicing it locally. `total`/`has-more` then describe
   that project, which is what a pager prints.

   The window is cut BEFORE decoration: `session-order` ranks every session from
   cheap facts, and only the ids that survive the cut pay for `soul` + workspace
   resolution. A 100-row page of a 448-session store therefore costs about a
   fifth of the full build (~257ms) and a fifth of its ~300KB, which is what
   makes a polled session list affordable."
  ([opts] (list-sessions-page :all opts))
  ([channel {:keys [limit offset root]}]
   (let
     [db
      (try (lp/db-info) (catch Throwable _ nil))

      ;; ONE grouped query serves BOTH the ordering and the page's decorations.
      stats
      (if db (try (persistance/db-session-turn-stats db) (catch Throwable _ {})) {})

      ordered
      (cond->> (session-order channel stats)
        (and db (seq root))
        (filterv (fn [id]
                   (= root (session-project-root db id)))))

      total
      (count ordered)

      ;; Digest of the ORDERING this window was cut from. Offsets index a list that
      ;; is RECOMPUTED per request — a session starting a turn jumps into the live
      ;; bucket at the top and shifts every window below it — so a client paging
      ;; across such a change merges one row twice (duplicate id) and skips another
      ;; entirely (a session silently missing from the list), with the merged count
      ;; still equal to `total` so nothing looks wrong. Stamping the ordering makes
      ;; that DETECTABLE with no server-side cursor state: two windows carrying
      ;; different digests were cut from different fleets, and the client re-walks.
      order-digest
      (Integer/toUnsignedString (int (hash ordered)) 16)

      from
      (min total (max 0 (long (or offset 0))))

      window
      (if (some? limit)
        (subvec ordered from (min total (+ from (max 0 (long limit)))))
        (subvec ordered from))

      rows
      (-> (into [] (keep soul) window)
          (session-summary-extras db stats)
          order-session-summaries)]

     {:sessions rows
      :total total
      :offset from
      :limit (some-> limit
                     long)
      :order-digest order-digest
      :has-more (< (+ from (count window)) total)})))

(defn list-sessions
  "Wire souls for every persisted session, each decorated with the bulk
   summary facts (`turn_count`, `modified_at`, lean `workspace`) so ONE
   `GET /v1/sessions` is enough to paint a session picker. Unwindowed —
   `list-sessions-page` serves clients that page.

   The gateway owns navigator ordering: live sessions first, followed by idle
   sessions in most-recently-active order. Clients must preserve this order.

   CROSS-CHANNEL by default (`channel` = `:all`): a conversation started
   in one channel is visible in the others and vice-versa. Pass a specific
   channel keyword only when a caller genuinely needs a single-channel
   slice (e.g. resolving a chat by external-id)."
  ([] (list-sessions :all))
  ([channel] (:sessions (list-sessions-page channel nil))))

(defn search-session-ids
  "Soul-id STRINGS whose TRANSCRIPT (user request + assistant iteration text)
   matches `query`. The SERVER-side half of transcript search: clients match
   title/project locally over the already-loaded list and union these ids for
   the deep matches, so the 105MB of assistant text never crosses the wire.
   Blank query → []."
  ([query] (search-session-ids :all query))
  ([channel query]
   (let [db (try (lp/db-info) (catch Throwable _ nil))]
     (if db (mapv str (persistance/db-search-session-ids db channel query)) []))))

(defn search-session-matches
  "Soul-id STRINGS whose TRANSCRIPT matches `query`, each TAGGED with WHERE it hit
   and carrying up to a handful of MATCH SNIPPETS:
   `[{:session_id str :is_in_request bool :is_in_reply bool
      :request_snippet str :reply_snippet str
      :hits [{:side \"request\"|\"reply\" :snippet str :at ms}]}]`
   (wire-shaped: snake_case string-ish keys, `is_<foo>` flags). Same SERVER-side
   deep search as `search-session-ids` — the assistant text never crosses the wire,
   only these snippet windows. `:is_in_request` = the user's own request matched;
   `:is_in_reply` = assistant reply text matched. Blank query → []."
  ([query] (search-session-matches :all query))
  ([channel query]
   (let [db (try (lp/db-info) (catch Throwable _ nil))]
     (if db
       (mapv (fn [{:keys [id in-request? in-reply? request-snippet reply-snippet hits]}]
               {:session_id (str id)
                :is_in_request (boolean in-request?)
                :is_in_reply (boolean in-reply?)
                :request_snippet request-snippet
                :reply_snippet reply-snippet
                :hits (mapv (fn [h]
                              {:side (name (:side h))
                               :snippet (:snippet h)
                               :at (some-> (:at h)
                                           inst-ms)})
                            (or hits []))})
             (persistance/db-search-session-matches db channel query))
       []))))

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

(declare close-session!)

(defn delete-project!
  "Delete a project. By DEFAULT its member sessions scatter back to project-less
   (conversations are never deleted).

   With `{:is-recursive true}` every member session is DELETED first via
   `close-session!` (draft clones trashed, session tree dropped, runtime torn
   down off-thread) and only then is the project row removed — sessions first, so
   an interrupted teardown leaves a project holding survivors rather than
   orphaned sessions. Real workspace directories are never touched.

   Membership comes from the DB (`lp/project-session-ids`), not from any client's
   filtered list. Returns `{:project_id … :deleted_session_ids […]
   :session_count n}`; the ids let a caller prune local state without racing a
   re-read."
  ([pid] (delete-project! pid nil))
  ([pid {:keys [is-recursive]}]
   (let [sids (if is-recursive (lp/project-session-ids pid) [])]
     (run! close-session! sids)
     (lp/delete-project! pid)
     {:project_id (str pid) :deleted_session_ids (mapv str sids) :session_count (count sids)})))

(defn assign-project!
  "Assign a session to `pid` (nil clears / removes from project). Returns the refreshed soul."
  [sid pid]
  (lp/assign-project! sid pid)
  (soul sid))

(defn reorder-project-sessions!
  "Atomically adopt loose named sessions into `pid`, then persist their manual
   order. Guests owned by another project are never stolen. Returns the member
   count applied."
  [pid session-ids]
  (lp/reorder-project-sessions! pid session-ids))

(defn release-session!
  "Release the live runtime for a session while keeping persisted data resumable.

   This is the gateway facade for local clients that are merely closing a view
   (for example a TUI tab or process exit). Use `close-session!` for DELETE.

   Background resources (background `shell` processes, managed REPLs) are STOPPED here:
   closing the view is the user walking away, and a bg child must not outlive
   that — the transcript stays resumable, the processes do not.

   A BUSY SESSION IS NEVER TORN DOWN. Sessions are shared across channels, so a
   closing view only proves THAT view is gone — the companion app, web, or another
   TUI may be attached to and streaming the very turn this would kill (`lp/close!`
   drops the runtime mid-turn, which the other client sees as its work being
   cancelled). A client that wants to STOP work cancels the turn explicitly; this
   endpoint is a view-lifecycle hint and is a no-op while work is in flight. Real
   process exit is covered by the daemon's own gate (client refcount +
   `running-turn-count`)."
  [sid]
  (when-not (session-busy? sid)
    (try (resources/stop-all! sid) (catch Throwable _ nil))
    (try (lp/close! sid) (catch Throwable _ nil)))
  nil)

(defonce ^:private teardown-executor
  ;; Single daemon thread: runs live-session teardown OFF the request thread.
  ;; Stopping background shells and managed REPLs waits on real processes, and
  ;; `lp/close!` waits up to 5s on the turn lock before disposing the polyglot
  ;; Context — seconds of work that a DELETE must never charge to the caller.
  (delay (java.util.concurrent.Executors/newSingleThreadExecutor
           (reify
             java.util.concurrent.ThreadFactory
               (newThread [_ r]
                 (doto (Thread. ^Runnable r "vis-session-teardown") (.setDaemon true)))))))

(defn- teardown-session-async!
  "Stop `sid`'s background resources and dispose its live environment off the
   calling thread. Returns a Future callers/tests can await; the session is
   already gone from the DB and the registry by the time this is submitted, so
   nothing a client can observe waits on it. Best-effort: a failed teardown is
   swallowed rather than resurrecting the deleted session."
  [sid]
  (.submit ^java.util.concurrent.ExecutorService @teardown-executor
           ^Callable
           (fn []
             (try (resources/stop-all! sid) (catch Throwable _ nil))
             (try (lp/close! sid) (catch Throwable _ nil))
             nil)))

(defn close-session!
  "DELETE a session: trash the session's draft clones (primary + auto-cloned
   filesystem roots — only DRAFTS have clones; a trunk workspace's roots are the
   user's real dirs and are never touched), delete the session tree, and drop it
   from this process. Idempotent. Returns the teardown Future.

   THE RESPONSE COSTS THE DB REMOVAL, NOTHING MORE. Disposing the live runtime
   (background shells, managed REPLs, the polyglot Context) used to run right
   here on the request thread, so deleting a session the user had just worked in
   held DELETE open for seconds — long enough for the companion's confirm modal
   to read as a frozen screen. Teardown now runs on `teardown-session-async!`,
   after the session has already stopped existing for every client.

   NOTE: this is the DELETE path — merely quitting/closing a session (navigating
   away, no server call) keeps the draft intact so it can be resumed."
  [sid]
  ;; trash on-disk clones BEFORE the DB tree (delete) so the workspace row is
  ;; still resolvable; draft-only, so this can never delete a real directory.
  (try (workspace/discard-session-clones! (lp/db-info) sid) (catch Throwable _ nil))
  (try (persistance/db-delete-session-tree! (lp/db-info) sid) (catch Throwable _ nil))
  (drop-session! sid)
  (bus/forget! sid)
  (teardown-session-async! sid))

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

   The foreign copy names the TITLED session under `:titled_session_id` and
   NEVER re-stamps `:session_id`: that key is the id of the ring the event was
   appended to (see `append-event!`), and it keys the per-session dedup on both
   ends of the multiplexed SSE stream. Re-stamping it advanced the OTHER
   session's cursor to THIS ring's seq and silently killed that session's live
   stream. Title generation is once-per-session, so the extra ring writes are
   negligible and stay bounded by the ring trim; idle sessions never replay
   it (a page renders at the current seq, so a pre-render foreign event sits
   below the cursor)."
  [sid title]
  (append-event! sid "session.title_updated" {:title (str title)})
  (doseq [other (other-session-ids sid)]
    (append-event! other
                   "session.title_updated"
                   {:titled_session_id (str sid) :title (str title)})))

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
                                (session-known? sid)))
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
  "Global, per-session, concurrency, replay-buffer, and JVM gauges for /metrics."
  []
  (let
    [reg
     @registry

     entries
     (vals reg)]

    (merge @metrics
           (lp/gateway-runtime-metrics)
           {:sessions-tracked (count reg)
            :turns-running (count (keep :current-turn entries))
            :turns-executing @turns-executing
            :turns-waiting @turns-waiting
            :turn-concurrency-limit @MAX_CONCURRENT_TURNS
            :turns-queued (reduce + 0 (map count-queued entries))
            :replay-events-retained (reduce + 0 (map #(count (:events %)) entries))
            :auth-refresh (lp/auth-refresh-metrics)})))

(defn warm-db!
  "Force the persistence backend + shared connection on the CALLER's
   thread. The gateway runs this on its single-threaded boot path so
   the heavyweight backend namespace never lazy-loads under request
   concurrency (see require-backend-ns! in internal/persistance.clj)."
  []
  (try (lp/db-info)
       true
       (catch Throwable t (tel/log! :warn ["gateway: db warmup failed" (ex-message t)]) false)))
