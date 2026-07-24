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
   them could corrupt user data that legitimately contains hyphens."
  [k]
  (if (or (keyword? k) (symbol? k))
    (let
      [n
       (name k)

       n
       (if (str/ends-with? n "?")
         (let [base (subs n 0 (dec (count n)))]
           (if (str/starts-with? base "is-") base (str "is-" base)))
         n)]

      (str/replace n "-" "_"))
    k))

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


(defn bounded-pr
  "Bounded `pr-str` for tool results / errors riding events. Protects the
   event log and SSE frames from multi-megabyte values."
  [x ^long limit]
  (let [s (try (pr-str x) (catch Throwable t (str "#render-error " (ex-message t))))]
    (if (> (count s) limit) (str (subs s 0 limit) " …[truncated]") s)))

(defn bounded-str
  "Bounded plain-string clamp for an ALREADY-rendered value (e.g. the
   model-facing `render-form-value` string) — same megabyte protection as
   `bounded-pr` but WITHOUT re-`pr-str`'ing, so the string rides the wire
   verbatim instead of quoted/escaped."
  [s ^long limit]
  (let [s (str s)]
    (if (> (count s) limit) (str (subs s 0 limit) " …[truncated]") s)))

(def queue-mirror-event-types
  "Queue lifecycle event types every attached channel mirrors LIVE even when
   they belong to a DIFFERENT (queued) turn of the same session — the ONE set
   both transports forward (the in-process `gateway.state` subscriptions AND
   the SSE loop in `gateway.client`), so a message queued/edited/deleted in
   one channel shows up in every sibling. `turn.queued.drained` marks the
   queue head leaving the queue because the gateway auto-STARTED it, so
   mirrors drop the entry and a replayed history nets to zero
   (`turn.queued` … `turn.queued.drained`). `queue.paused`/`queue.resumed` carry
   the provider-health lane state (held count, breaker) so every sibling shows
   the same paused banner and unpauses together."
  #{"turn.queued" "turn.queued.updated" "turn.queued.deleted" "turn.queued.drained" "queue.paused"
    "queue.resumed"})

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
