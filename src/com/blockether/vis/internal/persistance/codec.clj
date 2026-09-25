(ns com.blockether.vis.internal.persistance.codec
  "Column codecs every persistence backend shares: entity ids and references,
   keyword, timestamp and JSON TEXT columns, and turn status values. A leaf
   namespace, so the facade, the SQLite backend and the public API require it
   without a load cycle."
  (:require [charred.api :as json])
  (:import (java.time Instant)
           (java.util Date UUID)))

;; Storage base helpers

(defn ds [db-info] (:datasource db-info))

(defn ->id
  [v]
  (cond (nil? v) nil
        (uuid? v) (str v)
        (string? v) v
        :else (str v)))

(defn ->uuid
  ^UUID [v]
  (cond (nil? v) nil
        (uuid? v) v
        (string? v) (try (UUID/fromString v) (catch IllegalArgumentException _ nil))
        :else nil))

(defn ->ref
  "Normalize an entity reference to a string ID for SQL.
   Accepts: UUID, string, or nil. Returns string or nil.

   The ONLY way to extract a SQL-ready string from an entity
   reference -- pass the plain UUID or string directly."
  [v]
  (cond (nil? v) nil
        (uuid? v) (str v)
        (string? v) v
        :else (str v)))

(defn ->kw
  "Keyword/string -> TEXT, stripping the leading colon. Nil -> nil."
  [v]
  (cond (nil? v) nil
        (keyword? v) (subs (str v) 1)
        :else (str v)))

(defn ->kw-back [v] (when (and v (not= "" v)) (keyword v)))

(defn ->epoch-ms
  [v]
  (cond (nil? v) nil
        (instance? Date v) (.getTime ^Date v)
        (instance? Instant v) (.toEpochMilli ^Instant v)
        (number? v) (long v)
        :else nil))

(defn ->date ^Date [v] (when v (Date. (long v))))

(defn new-uuid ^UUID [] (UUID/randomUUID))

(defn new-id [] (->id (new-uuid)))

;; Column codecs (shared by EVERY backend)

(defn- json-key
  "A map key charred can actually write. Keyword/symbol/string keys keep their
   EXACT current spelling (persisted columns must not shift under an existing
   database); anything else — the int keys a Python `Counter` or a decoded JSON
   value carries out of the sandbox — is RENDERED rather than left to blow up
   the whole write."
  [k]
  (cond (string? k) k
        (keyword? k) (subs (str k) 1)
        (symbol? k) (str k)
        (nil? k) "null"
        :else (str k)))

(defn- json-encodable
  "Rewrite ONLY what charred REFUSES to encode: non-string map keys and
   non-finite doubles (a pandas `NaN`, a `x/0.0`). Everything else passes
   through untouched, so no persisted spelling changes. Without this the
   encoder throws mid-write and the caller loses the whole column — the final
   answer content of a settled turn — over one exotic value inside it."
  [x]
  (cond (map? x) (persistent! (reduce-kv (fn [m k v]
                                           (assoc! m (json-key k) (json-encodable v)))
                                         (transient {})
                                         x))
        (coll? x) (mapv json-encodable x)
        (and (float? x) (not (Double/isFinite (double x)))) nil
        :else x))

(defn ->json
  "Serialize a value to a JSON TEXT column. Nil in, nil out."
  [m]
  (when m (json/write-json-str (json-encodable m))))

(defn <-json
  "Parse a JSON TEXT column. STRINGS-ONLY: keys come back as VERBATIM STRINGS -
   no `:key-fn keyword` re-keywordizing. Whatever needs an internal keyword
   shape converts at ONE named adapter, never here."
  [s]
  (when s (json/read-json s)))

(defn normalize-status
  "Map runtime status keywords to the schema CHECK constraint values.
   Allowed: running, done, error, interrupted."
  [status]
  (case status
    (:success :done)
    "done"

    :error
    "error"

    (:cancelled :interrupted)
    "interrupted"

    :running
    "running"

    (->kw (or status :done))))
