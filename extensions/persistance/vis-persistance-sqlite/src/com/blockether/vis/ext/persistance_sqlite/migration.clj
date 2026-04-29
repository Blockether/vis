(ns com.blockether.vis.ext.persistance-sqlite.migration
  "Flyway-backed schema migration runner.

   Lives in the SQLite extension because:

     1. The dialect-specific Flyway driver (`flyway-database-nc-sqlite`)
        is required to recognize `jdbc:sqlite:` URLs — already
        declared in this extension's deps.edn.
     2. Flyway is the only backend-side concern using
        `flyway-core`; making it a per-backend dep keeps the root
        package free of the migration toolchain.
     3. The previous arrangement shipped a generic `migrate!` from
        `com.blockether.vis.sdk`, but it had exactly one caller
        — this extension. Other backends will ship their own
        migration entry point in their own jar.

   Public API:

     `(migrate! datasource locations)` — apply every Flyway
     migration found at the given classpath `locations` to the
     supplied `DataSource`. Returns the datasource for thread-style
     chaining.

   `:baseline-on-migrate true` so existing databases without a
   `flyway_schema_history` table get one on first run. `:mixed true`
   so SQL files with mixed transactional + DDL statements work under
   SQLite."
  (:import
   [javax.sql DataSource]))

(defn migrate!
  "Apply every Flyway migration found at the given classpath
   `locations` to `ds`. Accepts a single string or a coll of strings.
   Returns `ds`."
  [^DataSource ds locations]
  (let [locs (cond
               (string? locations)     [locations]
               (sequential? locations) (vec locations)
               :else
               (throw (ex-info "locations must be a string or coll of strings"
                        {:type :persistance/invalid-migration-locations
                         :got  (type locations)})))
        ^org.flywaydb.core.api.configuration.FluentConfiguration cfg
        (-> (org.flywaydb.core.Flyway/configure)
          (.dataSource ds)
          (.locations ^"[Ljava.lang.String;" (into-array String locs))
          (.baselineOnMigrate true)
          (.baselineVersion "0")
          (.mixed true))
        ^org.flywaydb.core.Flyway flyway (.load cfg)]
    (.migrate flyway)
    ds))
