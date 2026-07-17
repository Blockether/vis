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
  (let [cl
        (or (.getContextClassLoader (Thread/currentThread)) (.getClassLoader ResourceProvider))

        res
        (vec
          (for [loc
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


(defn migrate!
  "Install the single canonical V1 schema. Existing databases whose applied V1
   checksum differs are rejected: this flag-day schema has no compatibility or
   repair path."
  [^DataSource ds locations]
  (let [locs (cond (string? locations) [locations]
                   (sequential? locations) (vec locations)
                   :else (throw (ex-info "locations must be a string or coll of strings"
                                         {:type :persistance/invalid-migration-locations
                                          :got (type locations)})))
        rp (index-resource-provider locs)
        ^org.flywaydb.core.api.configuration.FluentConfiguration cfg
        (cond-> (-> (org.flywaydb.core.Flyway/configure)
                    (.dataSource ds)
                    (.locations ^"[Ljava.lang.String;" (into-array String locs))
                    (.baselineOnMigrate true)
                    (.baselineVersion "0")
                    (.mixed true))
          rp (.resourceProvider rp))]
    (.migrate (.load cfg))
    ds))
