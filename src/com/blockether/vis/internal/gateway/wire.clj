(ns com.blockether.vis.internal.gateway.wire
  "Wire encoding for the HTTP gateway.

   One dumb, deterministic boundary: engine EDN -> JSON. Keyword/symbol
   keys become snake_case strings (namespace dropped), keyword values
   become their name, non-JSON leaves fall back to `str`. The walker
   makes ZERO semantic decisions - no flattening, no rendering. Canonical
   IR vectors pass through structurally (`[:ir {...} ...]` ->
   `[\"ir\", {...}, ...]`), which is exactly the ALWAYS-IR contract:
   the client walks IR; the gateway never renders."
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
        (keyword? x) (name x)
        (symbol? x) (str x)
        (uuid? x) (str x)
        (ratio? x) (double x)
        (or (string? x) (number? x) (boolean? x) (nil? x)) x
        (instance? java.time.Instant x) (.toEpochMilli ^java.time.Instant x)
        (instance? java.util.Date x) (.getTime ^java.util.Date x)
        :else (str x)))

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

(defn bounded-pr
  "Bounded `pr-str` for tool results / errors riding events. Protects the
   event log and SSE frames from multi-megabyte values."
  [x limit]
  (let [s (try (pr-str x) (catch Throwable t (str "#render-error " (ex-message t))))]
    (if (> (count s) limit) (str (subs s 0 limit) " …[truncated]") s)))

(defn bounded-str
  "Bounded plain-string clamp for an ALREADY-rendered value (e.g. the
   model-facing `render-form-value` string) — same megabyte protection as
   `bounded-pr` but WITHOUT re-`pr-str`'ing, so the string rides the wire
   verbatim instead of quoted/escaped."
  [s limit]
  (let [s (str s)]
    (if (> (count s) limit) (str (subs s 0 limit) " …[truncated]") s)))

(defn sse-frame
  "Render one event map as an SSE frame. The event's `:seq` doubles as
   the SSE `id:` so `Last-Event-ID` reconnects resume losslessly."
  ^String [{:keys [seq type] :as event}]
  (str "id: " seq "\n" "event: " type "\n" "data: " (json-str event) "\n\n"))
