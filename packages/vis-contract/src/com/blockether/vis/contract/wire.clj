(ns com.blockether.vis.contract.wire
  "Canonical value and JSON codecs shared by Core, the gateway and every SDK.

   This namespace is a pure deterministic boundary: it performs no transport,
   filesystem, process or lifecycle work. Engine EDN becomes total JSON data;
   in-process readers receive exactly the shape a remote JSON reader receives."
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
        (let [n
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

(defn engine-key
  "Wire key -> engine keyword: THE inverse of [[wire-key]], and the only one.
   `is_foo` -> `:is-foo`, `foo_bar` -> `:foo-bar`.

   It is total only because the engine spells a boolean `:is-foo` and never
   `:foo?`: [[wire-key]] collapses BOTH spellings onto `is_foo`, so while a
   `:foo?` key exists no rule can tell which one a wire key came from. That
   ambiguity is why every inbound seam used to grow its own hand-written list of
   exceptions, and why forgetting one entry was silent. A FOREIGN contract that
   insists on `?` (svar's `:tool-call?`) maps through its own named table at its
   own seam — never by convention here."
  [k]
  (keyword (str/replace (if (keyword? k) (name k) (str k)) "_" "-")))

(defn ->engine
  "Recursively convert decoded wire data into the engine's keyword-keyed shape —
   the mirror of [[->wire]]. Only KEYS are converted (via [[engine-key]]); values
   are data and are never re-typed."
  [x]
  (cond (map? x) (into {}
                       (map (fn [[k v]]
                              [(engine-key k) (->engine v)]))
                       x)
        (sequential? x) (mapv ->engine x)
        :else x))

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

(defn parse-json
  "Parse a JSON string into the canonical wire shape: snake_case STRING map
   keys, identical to [[canonical]]. Returns nil on blank or malformed input
   (callers map that to 400)."
  [^String s]
  (when-not (str/blank? s) (try (json/read-json s) (catch Throwable _ nil))))
