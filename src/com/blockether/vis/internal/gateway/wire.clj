(ns com.blockether.vis.internal.gateway.wire
  "Wire encoding for the HTTP gateway.

   One dumb, deterministic boundary: engine EDN -> JSON. Keyword/symbol
   keys become snake_case strings (namespace dropped), keyword VALUES keep
   their full `ns/name` (a badge role like `:tool-color/search` must survive
   the hop — dropping the namespace made the remote TUI see `:search` while
   the in-process web saw the full keyword), non-JSON leaves fall back to
   `str`. The walker makes ZERO semantic decisions - no flattening, no
   rendering. Canonical IR vectors pass through structurally
   (`[:ir {...} ...]` -> `[\"ir\", {...}, ...]`), which is exactly the
   ALWAYS-IR contract: the client walks IR; the gateway never renders.

   `canonical` is the SAME shape on the Clojure side: by definition what
   `parse-json` ∘ `json-str` yields — serve it from a facade and in-process
   readers see exactly what a remote client sees."
  (:require [charred.api :as json]
            [clojure.string :as str]))

(defn- wire-key
  "Keyword/symbol map key -> snake_case string. String keys (fact keys,
   scope strings, file paths) pass VERBATIM - rewriting them could
   corrupt user data that legitimately contains hyphens."
  [k]
  (if (or (keyword? k) (symbol? k))
    (-> k
        name
        (str/replace "-" "_"))
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
  "THE canonical gateway value shape — BY CONSTRUCTION identical to what a
   remote client holds after `parse-json` ∘ `json-str`: snake_case KEYWORD
   map keys (namespaces dropped), keyword/symbol values stringified, dates
   as epoch millis. Serve `(canonical x)` from a facade and the HTTP hop
   becomes an IDENTITY — in-process and remote consumers read the SAME
   shape, so a channel written against one transport can never break on
   the other. Invariant (guarded by the wire round-trip test):
   `(canonical x)` == `(parse-json (json-str x))` for every engine value."
  [x]
  (let [keywordize (fn keywordize [v]
                     (cond (map? v) (persistent! (reduce-kv
                                                   (fn [m k v']
                                                     (assoc! m (keyword k) (keywordize v')))
                                                   (transient {})
                                                   v))
                           (coll? v) (mapv keywordize v)
                           :else v))]
    (keywordize (->wire x))))

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
  "Parse a JSON request body into keyword-keyed Clojure data. Returns
   nil on blank or malformed input (callers map that to 400)."
  [^String s]
  (when-not (str/blank? s) (try (json/read-json s :key-fn keyword) (catch Throwable _ nil))))

(defn- unwire-key
  "Inverse of [[wire-key]] for ONE map key: a snake_case keyword (what
   `parse-json` yields for a JSON object key) -> the engine's kebab keyword.
   Preserves any namespace; non-keyword keys pass through."
  [k]
  (if (keyword? k) (keyword (namespace k) (str/replace (name k) "_" "-")) k))

(defn kebab-keys
  "Recursively rewrite every MAP KEY snake_case -> kebab-case — the structural
   INVERSE of the `-`->`_` munge [[wire-key]]/[[->wire]] apply on the way out.
   Values pass through UNTOUCHED (paths, labels, ids never mutate).

   Deliberately OPT-IN, applied only at a TYPED boundary whose keys are known to
   be engine keywords (e.g. the gateway workspace record) — NOT folded into
   [[parse-json]]: once JSON has flattened every key to a string the encoder's
   keyword-vs-string distinction is gone, so a legit STRING key that genuinely
   carries an underscore (a fact key, a scope like `turn_5`, a file path) is
   indistinguishable from a munged keyword and a blanket rewrite would corrupt it.
   Where the payload has no such string keys, this restores the engine's kebab
   shape LOSSLESSLY and — unlike a hand-maintained rename map — can never silently
   miss a newly-added key. Idempotent on already-kebab input."
  [x]
  (cond (map? x) (persistent! (reduce-kv (fn [m k v]
                                           (assoc! m (unwire-key k) (kebab-keys v)))
                                         (transient {})
                                         x))
        (coll? x) (mapv kebab-keys x)
        :else x))


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
   (`turn.queued` … `turn.queued.drained`)."
  #{"turn.queued" "turn.queued.updated" "turn.queued.deleted" "turn.queued.drained"})

(defn sse-frame
  "Render one event map as an SSE frame. The event's `:seq` doubles as
   the SSE `id:` so `Last-Event-ID` reconnects resume losslessly."
  ^String [{:keys [seq type] :as event}]
  (str "id: " seq "\n" "event: " type "\n" "data: " (json-str event) "\n\n"))
