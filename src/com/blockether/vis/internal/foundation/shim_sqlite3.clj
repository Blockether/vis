(ns com.blockether.vis.internal.foundation.shim-sqlite3
  "Built-in sandbox SHIM: a DB-API 2.0 `sqlite3` module for the model's Python
   sandbox, backed by the JVM's xerial `sqlite-jdbc` driver (already on the
   classpath via the persistence extension, so no new dependency and native-image
   reachability is already configured). CPython's `_sqlite3` native extension is
   absent in GraalPy, so `import sqlite3` otherwise fails with ModuleNotFoundError.

   Connections live HOST-side as `java.sql.Connection`s in a per-JVM registry keyed
   by an integer handle; the Python `Connection`/`Cursor` are thin handle wrappers.
   SQL + params cross the strings-only boundary; result rows come back as vectors
   (BLOBs base64-tagged). `:memory:` databases are fully supported; a file path is
   opened host-side via `jdbc:sqlite:<path>`. Autocommit is on, so `commit()` is a
   no-op flush and data persists without it (the forgiving DB-API path)."
  (:require [clojure.string :as str]
            [com.blockether.vis.core :as vis])
  (:import [java.sql DriverManager Connection PreparedStatement ResultSet]
           [java.util ArrayList Base64]))

;; ---------------------------------------------------------------------------
;; Host-side connection registry: handle (long) -> java.sql.Connection.
;; The Python Connection/Cursor are just handles; the DB lives on the JVM.
;; ---------------------------------------------------------------------------

(defonce ^:private db-registry (atom {}))

(defonce ^:private db-counter (atom 0))

(defn- reg-conn!
  "Register `c` and return its new integer handle."
  [^Connection c]
  (let [h (swap! db-counter inc)]
    (swap! db-registry assoc h c)
    h))

(defn- conn-of
  ^Connection [h]
  (or (get @db-registry (long h)) (throw (ex-info "Cannot operate on a closed database." {}))))

(def ^:private blob-tag "__vis_blob__")

;; ---------------------------------------------------------------------------
;; Parameter binding + value marshaling across the strings-only boundary.
;; ---------------------------------------------------------------------------

(defn- rewrite-named
  "Rewrite :name / @name / $name placeholders to positional `?` (outside string
   literals), returning [sql ordered-names]. Plain `?` placeholders are untouched."
  [^String sql]
  (let
    [sb
     (StringBuilder.)

     names
     (ArrayList.)

     n
     (.length sql)]

    (loop
      [i
       0

       q
       nil]

      (if (>= i n)
        [(.toString sb) (vec names)]
        (let [ch (.charAt sql i)]
          (cond q (do (.append sb ch) (recur (inc i) (if (= ch (char q)) nil q)))
                (or (= ch \') (= ch \")) (do (.append sb ch) (recur (inc i) ch))
                (and (#{\: \@ \$} ch)
                     (< (inc i) n)
                     (let [c2 (.charAt sql (inc i))]
                       (or (Character/isLetter c2) (= c2 \_))))
                (let
                  [j (long (loop [k (inc i)]
                             (if (and (< k n)
                                      (let [c (.charAt sql k)]
                                        (or (Character/isLetterOrDigit c) (= c \_))))
                               (recur (inc k))
                               k)))]
                  (.add names (subs sql (inc i) j))
                  (.append sb \?)
                  (recur j nil))
                :else (do (.append sb ch) (recur (inc i) q))))))))

(defn- bind-val!
  [^PreparedStatement ps ^long idx v]
  (cond (nil? v) (.setObject ps idx nil)
        (instance? Boolean v) (.setInt ps idx (if v 1 0))
        (instance? Long v) (.setLong ps idx (long v))
        (instance? Integer v) (.setLong ps idx (long v))
        (instance? Double v) (.setDouble ps idx (double v))
        (instance? Float v) (.setDouble ps idx (double v))
        (and (vector? v) (= (first v) blob-tag))
        (.setBytes ps idx (.decode (Base64/getDecoder) ^String (second v)))
        :else (.setString ps idx (str v))))

(defn- bind-params!
  [^PreparedStatement ps params names]
  (cond (nil? params) nil
        (map? params) (dotimes [i (count names)]
                        (bind-val! ps (inc i) (get params (nth names i))))
        (sequential? params) (dotimes [i (count params)]
                               (bind-val! ps (inc i) (nth params i)))
        :else (bind-val! ps 1 params)))

(defn- ->cell
  [v]
  (cond (nil? v) nil
        (instance? (Class/forName "[B") v) [blob-tag (.encodeToString (Base64/getEncoder) ^bytes v)]
        (instance? Integer v) (long v)
        (instance? Long v) (long v)
        (instance? java.math.BigDecimal v) (double v)
        (instance? Double v) (double v)
        (instance? Float v) (double v)
        (instance? Boolean v) (if v 1 0)
        :else v))

(defn- collect-rs
  [^ResultSet rs]
  (let
    [md
     (.getMetaData rs)

     nc
     (.getColumnCount md)

     cols
     (mapv #(.getColumnLabel md (inc (long %))) (range nc))

     rows
     (ArrayList.)]

    (while (.next rs)
      (.add rows
            (mapv (fn [^long i]
                    (->cell (.getObject rs (int (inc i)))))
                  (range nc))))
    {"description" cols "rows" (vec rows)}))

(defn- select-sql?
  [^String sql]
  (let [s (str/lower-case (str/triml sql))]
    (or (str/starts-with? s "select")
        (str/starts-with? s "pragma")
        (str/starts-with? s "with")
        (str/starts-with? s "explain"))))

;; ---------------------------------------------------------------------------
;; DB-API operations.
;; ---------------------------------------------------------------------------

(defn- op-connect
  [database]
  (let
    [db
     (if (or (nil? database) (= database "") (= database ":memory:")) ":memory:" (str database))

     url
     (if (= db ":memory:") "jdbc:sqlite::memory:" (str "jdbc:sqlite:" db))

     c
     (DriverManager/getConnection url)]

    (.setAutoCommit c true)
    (reg-conn! c)))

(defn- op-execute
  [conn-h ^String sql params]
  (let
    [c
     (conn-of conn-h)

     [sql2 names]
     (rewrite-named sql)

     ^PreparedStatement ps
     (.prepareStatement c sql2)]

    (try (bind-params! ps params names)
         (if (select-sql? sql)
           (let [m (collect-rs (.executeQuery ps))]
             (assoc m
               "rowcount" (count (get m "rows"))
               "lastrowid" nil))
           (let
             [uc
              (.executeUpdate ps)

              lid
              (with-open
                [st
                 (.createStatement c)

                 rs
                 (.executeQuery st "select last_insert_rowid()")]

                (when (.next rs) (.getLong rs 1)))]

             {"description" nil "rows" [] "rowcount" uc "lastrowid" lid}))
         (finally (.close ps)))))

(defn- op-executemany
  [conn-h ^String sql seq-params]
  (let
    [c
     (conn-of conn-h)

     [sql2 names]
     (rewrite-named sql)

     ^PreparedStatement ps
     (.prepareStatement c sql2)]

    (try (doseq [p seq-params]
           (bind-params! ps p names)
           (.addBatch ps))
         ;; CPython reports the rows the whole batch changed; JDBC hands back one
         ;; update count per statement, and -2 (SUCCESS_NO_INFO) for "unknown".
         (let
           [^ints counts
            (.executeBatch ps)

            n
            (areduce counts
                     i
                     acc
                     0
                     (let [u (aget counts i)]
                       (if (neg? u) acc (+ acc u))))]

           {"description" nil "rows" [] "rowcount" n "lastrowid" nil})
         (finally (.close ps)))))

(defn- op-executescript
  [conn-h ^String sql]
  (let [c (conn-of conn-h)]
    (with-open [st (.createStatement c)]
      (doseq
        [chunk (str/split sql #";")
         :let [s (str/trim chunk)]
         :when (seq s)]

        (.execute st s)))
    {"description" nil "rows" [] "rowcount" -1 "lastrowid" nil}))

(defn- op-commit
  [conn-h]
  (let [c (conn-of conn-h)]
    (when-not (.getAutoCommit c) (.commit c)))
  nil)

(defn- op-rollback
  [conn-h]
  (let [c (conn-of conn-h)]
    (when-not (.getAutoCommit c) (.rollback c)))
  nil)

(defn- op-close
  [conn-h]
  (when-let [^Connection c (get @db-registry (long conn-h))]
    (.close c)
    (swap! db-registry dissoc (long conn-h)))
  nil)

(defn- op-total-changes
  [conn-h]
  (with-open
    [st
     (.createStatement (conn-of conn-h))

     rs
     (.executeQuery st "select total_changes()")]

    (if (.next rs) (.getLong rs 1) 0)))

(defn- sqlite-envelope
  [f]
  (try [true (f)] (catch Throwable t [false (str (or (.getMessage t) t))])))

(defn- sqlite-bridge-bindings
  "Host callables (xerial sqlite-jdbc) the sqlite3 shim delegates to. The Python
   side only holds integer connection handles + SQL/param/row strings."
  []
  {"__vis_sqlite_connect__" (fn [database]
                              (sqlite-envelope #(op-connect database)))
   "__vis_sqlite_execute__" (fn [h sql params]
                              (sqlite-envelope #(op-execute h sql params)))
   "__vis_sqlite_executemany__" (fn [h sql ps]
                                  (sqlite-envelope #(op-executemany h sql ps)))
   "__vis_sqlite_executescript__" (fn [h sql]
                                    (sqlite-envelope #(op-executescript h sql)))
   "__vis_sqlite_commit__" (fn [h]
                             (sqlite-envelope #(op-commit h)))
   "__vis_sqlite_rollback__" (fn [h]
                               (sqlite-envelope #(op-rollback h)))
   "__vis_sqlite_close__" (fn [h]
                            (sqlite-envelope #(op-close h)))
   "__vis_sqlite_total_changes__" (fn [h]
                                    (sqlite-envelope #(op-total-changes h)))})


(def vis-extension
  (vis/extension
    {:ext/name "foundation-shim-sqlite3"
     :ext/description
     "Sandbox DB-API 2.0 `sqlite3` over JVM xerial sqlite-jdbc: connections/cursors, execute variants, fetch, transactions/context manager, Row, named/qmark params, blobs, errors, and `total_changes`. Replaces unavailable GraalPy `_sqlite3`; no pip/new dependency. Bind values: int/float/str/None only."
     :ext/version "0.1.0"
     :ext/author "Blockether"
     :ext/owner "vis"
     :ext/license "Apache-2.0"
     :ext/kind "foundation"
     :ext/sandbox-shims
     [{:shim/name "sqlite3"
       :shim/imports ["sqlite3"]
       :shim/description
       "JVM xerial sqlite-jdbc `sqlite3` DB-API 2.0; connections use integer handles. Bindings support int/float/str/None only, else `InterfaceError`."
       :shim/bindings sqlite-bridge-bindings
       :shim/source "vis-shims/sqlite3.py"}]}))

(vis/register-extension! vis-extension)
