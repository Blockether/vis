(ns com.blockether.vis.internal.gateway.wire
  "Wire encoding for the HTTP gateway.

   One dumb, deterministic boundary: engine EDN -> JSON. Keyword/symbol
   keys become snake_case strings (namespace dropped), keyword VALUES keep
   their full `ns/name` (a badge role like `:tool-color/search` must survive
   the hop — dropping the namespace made the remote TUI see `:search` while
   the hop), non-JSON leaves fall back to `str`. The walker makes zero semantic
   or rendering decisions. Canonical message content is already a string-keyed
   vector of typed block maps before it reaches this boundary.

   `canonical` is the SAME shape on the Clojure side: by definition what
   `parse-json` ∘ `json-str` yields — snake_case STRING keys — serve it from
   a facade and in-process readers see exactly what a remote client sees."
  (:require [charred.api :as json]
            [clojure.string :as str]))

(defn wire-key
  "Keyword/symbol map key -> snake_case string. A boolean-style `foo?` key
   becomes `is_foo` (already-`is-` prefixed keys just drop the `?`). String
   keys (fact keys, scope strings, file paths) pass VERBATIM - rewriting
   them could corrupt user data that legitimately contains hyphens. ANY
   other key (the number/boolean/nil keys a decoded JSON or Python value can
   carry into a tool result) is rendered to its JSON key spelling: JSON has
   no non-string keys, and leaving one unrendered makes the whole event
   unencodable - which kills the transport, not just the field."
  [k]
  (cond (or (keyword? k) (symbol? k))
        (let
          [n
           (name k)

           n
           (if (str/ends-with? n "?")
             (let [base (subs n 0 (dec (count n)))]
               (if (str/starts-with? base "is-") base (str "is-" base)))
             n)]

          (str/replace n "-" "_"))
        (string? k) k
        (nil? k) "null"
        :else (str k)))

(defn ->wire
  "Recursively convert an engine value into JSON-encodable data."
  [x]
  (cond (map? x) (persistent! (reduce-kv (fn [m k v]
                                           (assoc! m (wire-key k) (->wire v)))
                                         (transient {})
                                         x))
        (coll? x) (mapv ->wire x)
        (keyword? x) (if-let [kns (namespace x)]
                       (str kns "/" (name x))
                       (name x))
        (symbol? x) (str x)
        (uuid? x) (str x)
        (ratio? x) (double x)
        ;; JSON has no NaN/Infinity: a non-finite double makes the encoder
        ;; THROW, and the throw happens per-frame at the transport, so one
        ;; poisoned value takes down SSE and /poll for the whole session (and
        ;; again on every replay, because the event is already in the ring).
        ;; `null` is what JSON.stringify emits for the same value.
        (and (float? x) (not (Double/isFinite (double x)))) nil
        ;; A BigDecimal parses back as a double, so passing it through would
        ;; break the `canonical` = parse-json ∘ json-str invariant.
        (decimal? x) (double x)
        (or (string? x) (number? x) (boolean? x) (nil? x)) x
        (instance? java.time.Instant x) (.toEpochMilli ^java.time.Instant x)
        (instance? java.util.Date x) (.getTime ^java.util.Date x)
        :else (str x)))

(defn canonical
  "THE canonical gateway value shape — snake_case STRING map keys, exactly
   what a remote client holds after `parse-json` ∘ `json-str`. In-process and
   remote consumers therefore read the same role-labelled messages and typed
   content blocks.

   Invariant: `(canonical x)` equals `(parse-json (json-str x))`."
  [x]
  (->wire x))

(defn json-str
  "Encode any engine value as a JSON string via [[->wire]]."
  ^String [x]
  (json/write-json-str (->wire x)))

(defn json-str-pretty
  "Pretty-printed (2-space indent) JSON via [[->wire]] — for
   HUMAN-facing surfaces (the web ctx rail's trailer view), never the
   wire itself."
  ^String [x]
  (json/write-json-str (->wire x) :indent-str "  "))

(defn parse-json
  "Parse a JSON string into the canonical wire shape: snake_case STRING map
   keys, identical to [[canonical]]. Returns nil on blank or malformed input
   (callers map that to 400)."
  [^String s]
  (when-not (str/blank? s) (try (json/read-json s) (catch Throwable _ nil))))

(defn- clamp
  "Cut `s` to at most `limit` chars and mark it truncated, WITHOUT splitting a
   surrogate pair — a lone surrogate is not valid text and corrupts every
   UTF-8 consumer downstream (JSON escape, SQLite, the mobile client)."
  [^String s ^long limit]
  (if (<= (count s) limit)
    s
    (let
      [cut (if (and (pos? limit) (Character/isHighSurrogate (.charAt s (dec limit))))
             (dec limit)
             limit)]
      (str (subs s 0 (max 0 cut)) " …[truncated]"))))

(defn bounded-pr
  "Bounded `pr-str` for tool results / errors riding events. Protects the
   event log and SSE frames from multi-megabyte values."
  [x ^long limit]
  (let [s (try (pr-str x) (catch Throwable t (str "#render-error " (ex-message t))))]
    (clamp s limit)))

(defn bounded-str
  "Bounded plain-string clamp for an ALREADY-rendered value (e.g. the
   model-facing `render-form-value` string) — same megabyte protection as
   `bounded-pr` but WITHOUT re-`pr-str`'ing, so the string rides the wire
   verbatim instead of quoted/escaped."
  [s ^long limit]
  (clamp (str s) limit))

(def queue-mirror-event-types
  "Queue lifecycle event types every attached channel mirrors LIVE even when
   they belong to a DIFFERENT (queued) turn of the same session — the ONE set
   both transports forward (the in-process `gateway.state` subscriptions AND
   the SSE loop in `gateway.client`), so a message queued/edited/deleted in
   one channel shows up in every sibling. `turn.queued.drained` marks the
   queue head leaving the queue because the gateway auto-STARTED it, so
   mirrors drop the entry and a replayed history nets to zero
   (`turn.queued` … `turn.queued.drained`). `queue.paused`/`queue.resumed` carry
   the held count so every sibling shows the same paused banner and unpauses
   together."
  #{"turn.queued" "turn.queued.updated" "turn.queued.deleted" "turn.queued.drained" "queue.paused"
    "queue.resumed"})

(def turn-terminal-event-types
  "Every event type that ENDS a turn — the ONE set both blocking readers use
   (`gateway.state`'s in-process submit/attach subscriptions AND the SSE loop in
   `gateway.client`). `turn.cancelled` belongs here: a user stop (or a stall
   force-cancel) lands a turn exactly like a completion, and a reader that only
   watched for `turn.completed`/`turn.failed` parked on that turn FOREVER —
   its SSE connection stayed open, its channel kept a live spinner, and a
   queued turn draining behind it streamed into a tab whose previous stream
   had never closed."
  #{"turn.completed" "turn.failed" "turn.cancelled"})

(def turn-meta-keys
  "Wire keys of a settled turn's META (usage/routing/timing) — the fields
   `terminal-event->result` (both the in-process `gateway.state` impl and the
   SSE `gateway.client` twin) resolves for the sync submit/attach result.
   Terminal events are deliberately LEAN (`{:turn_id :status}`), so these are
   read primarily from the registry's turn row (merged by `finish-turn!`),
   with any event-carried value winning. ONE list so the two impls can't
   drift."
  ["model" "provider" "llm_selected" "llm_actual" "is_llm_fallback" "llm_routing_trace" "tokens"
   "cost" "confidence" "eval" "duration_ms" "utilization"])

(defn sse-frame
  "Render one canonical (string-keyed) event map as an SSE frame. The event's
   `\"seq\"` doubles as the SSE `id:` so `Last-Event-ID` reconnects resume
   losslessly."
  ^String [event]
  (str "id: "
       (get event "seq")
       "\n"
       "event: "
       (get event "type")
       "\n"
       "data: "
       (json-str event)
       "\n\n"))

(def voice-job-event
  "SSE `event:` name of EVERY frame on a transcription job's stream — the ONE
   discriminator that keeps a job's progress from being read as a session event.

   The gateway speaks SSE on two unrelated resources and a consumer must never
   mistake one for the other. `GET /v1/sessions/:sid/events` is the session's
   ordered event LOG: every frame carries an `id:` cursor, its `event:` is the
   engine event TYPE, it replays from `Last-Event-ID`, and it stays open for the
   life of the session. `GET /v1/sessions/:sid/voice/jobs/:job-id/events` is ONE
   transcription's state: no cursor, no replay, exactly this event name on every
   frame, and the stream ENDS on the terminal one. `/v1/capabilities` publishes
   this string as `features.voice.progress_event` and the companion mirrors it as
   `VOICE_JOB_EVENT` (apps/vis-companion/src/lib/gateway.ts), so a client filters
   on a name it was told rather than guessing from the payload's shape."
  "voice.job")

(defn voice-job-sse-frame
  "Render one transcription job as its own SSE frame.

   Deliberately no `id:`: a job stream carries a RESOURCE's current state, not a
   replayable log, so there is no cursor to resume from — a reconnect is answered
   with the job as it is now (see [[voice-job-event]])."
  ^String [job]
  (str "event: " voice-job-event "\ndata: " (json-str job) "\n\n"))
