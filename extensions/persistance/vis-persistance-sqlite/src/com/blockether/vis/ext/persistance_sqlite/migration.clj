(ns com.blockether.vis.ext.persistance-sqlite.migration
  "Flyway-backed schema migration runner.

   Lives in the SQLite extension because:

     1. The dialect-specific Flyway driver (`flyway-database-nc-sqlite`)
        is required to recognize `jdbc:sqlite:` URLs - already
        declared in this extension's deps.edn.
     2. Flyway is the only backend-side concern using
        `flyway-core`; making it a per-backend dep keeps the root
        package free of the migration toolchain.
     3. The previous arrangement shipped a generic `migrate!` from
        `com.blockether.vis.sdk`, but it had exactly one caller
        - this extension. Other backends will ship their own
        migration entry point in their own jar.

   Public API:

     `(migrate! datasource locations)` - apply every Flyway
     migration found at the given classpath `locations` to the
     supplied `DataSource`. Returns the datasource for thread-style
     chaining.

   `:baseline-on-migrate true` so existing databases without a
   `flyway_schema_history` table get one on first run. `:mixed true`
   so SQL files with mixed transactional + DDL statements work under
   SQLite.

   GraalVM native-image note: Flyway discovers migrations by LISTING the
   classpath location directory, which native-image does not support (it can
   `getResource` a specific file but not enumerate a dir). So `build.clj` writes
   an `_index.edn` of filenames next to each migration dir, and here we feed
   Flyway an explicit `ResourceProvider` built from those exact paths. On the
   JVM (no index) we fall back to Flyway's normal location scanning."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [javax.sql DataSource]
           [java.nio.charset StandardCharsets]
           [org.flywaydb.core.api Location ResourceProvider]
           [org.flywaydb.core.internal.resource.classpath ClassPathResource]))

(defn- strip-classpath
  [^String loc]
  (if (str/starts-with? loc "classpath:") (subs loc (count "classpath:")) loc))

(defn- index-resource-provider
  "A `ResourceProvider` serving the migrations named in each location's
   build-generated `_index.edn`. Returns nil when no index exists (JVM/dev),
   so callers fall back to Flyway's directory scanning."
  ^ResourceProvider [locations]
  (let
    [cl
     (or (.getContextClassLoader (Thread/currentThread)) (.getClassLoader ResourceProvider))

     res
     (vec (for
            [loc
             locations

             :let [base
                   (strip-classpath loc)

                   idx
                   (io/resource (str base "/_index.edn"))]
             :when idx
             fname
             (edn/read-string (slurp idx))]

            (ClassPathResource. (Location. loc) (str base "/" fname) cl StandardCharsets/UTF_8)))]

    (when (seq res)
      (reify
        ResourceProvider
          (getResource [_ name]
            (some (fn [^ClassPathResource r]
                    (when (or (= name (.getFilename r)) (= name (.getRelativePath r))) r))
                  res))
          (getResources [_ prefix suffixes]
            (filterv (fn [^ClassPathResource r]
                       (let [fname (.getFilename r)]
                         (and (str/starts-with? fname (or prefix ""))
                              (boolean (some #(str/ends-with? fname %) suffixes)))))
              res))))))


(defn- repairable-validation-error?
  "True when Flyway rejected the applied migration metadata during validation.
   The canonical V1 is intentionally edited in place, and historical V2+ rows
   can remain after those migrations are folded back into V1. `repair` is the
   safe Flyway operation for both cases: it realigns checksums and marks removed
   migrations as deleted without touching application tables or their rows."
  [^Throwable e]
  (boolean (some (fn [^Throwable t]
                   (let [^String message (or (ex-message t) "")]
                     (or (str/includes? message "Migration checksum mismatch")
                         (str/includes? message "Migrations have failed validation")
                         (str/includes? message
                                        "Detected applied migration not resolved locally"))))
                 (take 16
                       (take-while some?
                                   (iterate (fn [^Throwable t]
                                              (.getCause t))
                                            e))))))

(defn- migration-filenames
  "Names of the `.sql` files at one classpath `base`. Prefers the build-generated
   `_index.edn` (the only thing that works in the native image) and falls back to
   listing the directory on the JVM."
  [^String base]
  (if-let [idx (io/resource (str base "/_index.edn"))]
    (vec (edn/read-string (slurp idx)))
    (when-let [url (io/resource base)]
      (when (= "file" (.getProtocol url))
        (->> (.listFiles (io/file (.toURI url)))
             (map (fn [^java.io.File f]
                    (.getName f)))
             (filter (fn [^String n]
                       (str/ends-with? n ".sql")))
             (sort)
             (vec))))))

(defn- migration-sql-texts
  "The SQL bodies of every migration shipped at `locations`, in file order."
  [locations]
  (vec (for
         [loc
          locations

          :let [base
                (strip-classpath loc)]
          fname
          (or (migration-filenames base) [])

          :let [res
                (io/resource (str base "/" fname))]
          :when res]

         (slurp res))))

(defn- strip-sql-comments [^String sql] (str/replace sql #"--[^\n]*" ""))

(defn- closing-paren
  "Index of the `)` closing the `(` at index `open`, or nil."
  [^String s ^long open]
  (loop
    [i
     (inc open)

     depth
     1

     q
     ;; NUL = "not inside a quoted literal"; a real char keeps the loop primitive.
     \u0000]

    (when (< i (.length s))
      (let [c (.charAt s i)]
        (cond (not= q \u0000) (recur (inc i) depth (if (= c q) \u0000 q))
              (or (= c \') (= c \")) (recur (inc i) depth c)
              (= c \() (recur (inc i) (inc depth) \u0000)
              (= c \)) (if (= depth 1) i (recur (inc i) (dec depth) \u0000))
              :else (recur (inc i) depth \u0000))))))

(defn- split-top-level
  "Split a `CREATE TABLE` body at the commas that separate its definitions,
   ignoring commas nested in parens (CHECK, composite keys) or quotes."
  [^String body]
  (loop
    [i
     0

     depth
     0

     q
     \u0000

     start
     0

     acc
     []]

    (if (>= i (.length body))
      (conj acc (subs body start))
      (let [c (.charAt body i)]
        (cond (not= q \u0000) (recur (inc i) depth (if (= c q) \u0000 q) start acc)
              (or (= c \') (= c \")) (recur (inc i) depth c start acc)
              (= c \() (recur (inc i) (inc depth) \u0000 start acc)
              (= c \)) (recur (inc i) (dec depth) \u0000 start acc)
              (and (= c \,) (zero? depth))
              (recur (inc i) depth \u0000 (inc i) (conj acc (subs body start i)))
              :else (recur (inc i) depth \u0000 start acc))))))

(def ^:private table-constraint-heads
  "Leading words of a TABLE-level constraint, i.e. not a column definition."
  #{"constraint" "primary" "unique" "check" "foreign" "exclude"})

(defn- column-definition
  "`{:name ... :sql ...}` for one member of a `CREATE TABLE` body, or nil when
   that member is a table-level constraint rather than a column."
  [^String part]
  (let
    [text
     (str/trim (str/replace part #"\s+" " "))

     head
     (first (str/split text #"[\s(]" 2))]

    (when (and (seq text)
               (not (contains? table-constraint-heads (str/lower-case (or head ""))))
               (re-matches #"[A-Za-z_][A-Za-z0-9_]*" (or head "")))
      {:name head :sql text})))

(defn- canonical-columns
  "Table name -> ordered column definitions, parsed out of the canonical SQL."
  [^String sql]
  (let
    [s
     (strip-sql-comments sql)

     m
     (re-matcher
       #"(?i)create\s+table\s+(?:if\s+not\s+exists\s+)?[\"`\[]?([A-Za-z0-9_]+)[\"`\]]?\s*\("
       s)]

    (loop [acc {}]
      (if-not (.find m)
        acc
        (let
          [table (.group m 1)
           open (dec (.end m))
           close (closing-paren s open)]

          (recur (if close
                   (assoc acc
                     table (vec (keep column-definition
                                      (split-top-level (subs s (inc open) close)))))
                   acc)))))))

(defn- addable-column?
  "True when SQLite accepts this column definition in `ALTER TABLE ... ADD COLUMN`:
   no PRIMARY KEY / UNIQUE, no foreign key, and NOT NULL only with a DEFAULT."
  [{:keys [^String sql]}]
  (let [u (str/upper-case sql)]
    (and (not (re-find #"\bPRIMARY\s+KEY\b" u))
         (not (re-find #"\bUNIQUE\b" u))
         (not (re-find #"\bREFERENCES\b" u))
         (or (not (re-find #"\bNOT\s+NULL\b" u)) (boolean (re-find #"\bDEFAULT\b" u))))))




(defn- existing-columns
  "Lower-cased column names of `table`, or an empty set when it does not exist."
  [^java.sql.Connection conn ^String table]
  (with-open
    [st
     (.createStatement conn)

     rs
     (.executeQuery st (str "PRAGMA table_info(" table ")"))]

    (loop [acc #{}]
      (if (.next rs) (recur (conj acc (str/lower-case (.getString rs "name")))) acc))))

(defn- reconcile-canonical-columns!
  "Additively realign a database that was created by an EARLIER copy of the
   canonical V1 with the V1 shipped now.

   V1 is intentionally edited in place, so Flyway `repair` fixes the recorded
   checksum but never re-runs the file: an older store keeps its old columns and
   the next insert dies with `no such column`. Rather than adding a V2 file, we
   diff each `CREATE TABLE` in the shipped SQL against `PRAGMA table_info` and
   `ADD COLUMN` whatever is missing, using the column's own DDL text from the SQL
   file - no schema DDL is written in Clojure. Fresh databases match already, so
   this is a no-op for them; columns SQLite cannot add after the fact (keys,
   foreign keys, NOT NULL without DEFAULT) are left to a new schema generation."
  [^DataSource ds locations]
  (let [tables (reduce merge {} (map canonical-columns (migration-sql-texts locations)))]
    (when (seq tables)
      (with-open [conn (.getConnection ds)]
        (doseq
          [[table cols] tables
           :let [have (existing-columns conn table)]
           :when (seq have)
           col cols
           :when (and (not (contains? have (str/lower-case ^String (:name col))))
                      (addable-column? col))]

          ;; Best effort: a racing process may have added it already, and a
          ;; definition SQLite still refuses must surface at its real use site
          ;; rather than making the whole store unopenable.
          (try (with-open [st (.createStatement conn)]
                 (.executeUpdate st (str "ALTER TABLE " table " ADD COLUMN " (:sql col))))
               (catch Throwable _ nil)))))))

(defn migrate!
  "Install the single canonical V1 schema.

   Applied checksum drift and history left by migrations later consolidated
   into V1 self-heal through Flyway `repair`, then migration is retried. Repair
   changes only `flyway_schema_history`; persisted Vis rows and schema objects
   are preserved. Databases created by an older V1 are then topped up additively
   by `reconcile-canonical-columns!`, so one canonical migration keeps serving
   stores that already exist."
  [^DataSource ds locations]
  (let
    [locs
     (cond (string? locations) [locations]
           (sequential? locations) (vec locations)
           :else (throw (ex-info "locations must be a string or coll of strings"
                                 {:type :persistance/invalid-migration-locations
                                  :got (type locations)})))

     rp
     (index-resource-provider locs)

     ^org.flywaydb.core.api.configuration.FluentConfiguration cfg
     (cond->
       (-> (org.flywaydb.core.Flyway/configure)
           (.dataSource ds)
           (.locations ^"[Ljava.lang.String;" (into-array String locs))
           (.baselineOnMigrate true)
           (.baselineVersion "0")
           (.mixed true))
       rp
       (.resourceProvider rp))]

    (let [^org.flywaydb.core.Flyway flyway (.load cfg)]
      (try
        (.migrate flyway)
        (catch Throwable e
          (if (repairable-validation-error? e) (do (.repair flyway) (.migrate flyway)) (throw e)))))
    (reconcile-canonical-columns! ds locs)
    ds))
