(ns com.blockether.vis.persistance.base
  "Shared persistence base helpers used by SQLite storage modules."
  (:import (java.time Instant)
           (java.util Date UUID)))

(defn ds [db-info] (:datasource db-info))

(defn now-ms ^long [] (System/currentTimeMillis))

(defn ->id
  "UUID/string → canonical TEXT id. Nil → nil."
  [v]
  (cond
    (nil? v) nil
    (uuid? v) (str v)
    (string? v) v
    :else (str v)))

(defn ->uuid
  "TEXT id → UUID. Nil → nil."
  ^UUID [v]
  (cond
    (nil? v) nil
    (uuid? v) v
    (string? v) (try (UUID/fromString v) (catch IllegalArgumentException _ nil))
    :else nil))

(defn ->ref
  "Normalize any entity reference to a string ID for SQL.
   Accepts: UUID, string, [:id UUID], [:id string], or nil.
   Returns string or nil.
   
   This is the ONLY way to extract a SQL-ready string from a ref.
   Legacy code passed [:id uuid] tagged pairs; new code passes plain UUIDs.
   This function accepts both so callers don't need to know which form they have."
  [v]
  (cond
    (nil? v) nil
    (uuid? v) (str v)
    (string? v) v
    (and (vector? v) (>= (count v) 2)) (->id (second v))
    :else (str v)))

(defn ->kw
  "Keyword/string → TEXT (no leading colon). Nil → nil."
  [v]
  (cond
    (nil? v) nil
    (keyword? v) (subs (str v) 1)
    :else (str v)))

(defn ->kw-back
  "TEXT → keyword. Nil → nil."
  [v]
  (when (and v (not= "" v))
    (keyword v)))

(defn ->epoch-ms
  "java.util.Date / Instant → epoch-ms long. Nil → nil."
  [v]
  (cond
    (nil? v) nil
    (instance? Date v) (.getTime ^Date v)
    (instance? Instant v) (.toEpochMilli ^Instant v)
    (number? v) (long v)
    :else nil))

(defn ->date
  "epoch-ms long → java.util.Date. Nil → nil."
  ^Date [v]
  (when v (Date. (long v))))

