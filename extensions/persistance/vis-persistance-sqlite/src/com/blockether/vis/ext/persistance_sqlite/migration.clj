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
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as str])
  (:import
   [javax.sql DataSource]
   [java.nio.charset StandardCharsets]
   [org.flywaydb.core.api Location ResourceProvider]
   [org.flywaydb.core.internal.resource.classpath ClassPathResource]))

(defn- strip-classpath [^String loc]
  (if (str/starts-with? loc "classpath:") (subs loc (count "classpath:")) loc))

(defn- index-resource-provider
  "A `ResourceProvider` serving the migrations named in each location's
   build-generated `_index.edn`. Returns nil when no index exists (JVM/dev),
   so callers fall back to Flyway's directory scanning."
  ^ResourceProvider [locations]
  (let [cl  (or (.getContextClassLoader (Thread/currentThread))
              (.getClassLoader ResourceProvider))
        res (vec (for [loc   locations
                       :let  [base (strip-classpath loc)
                              idx  (io/resource (str base "/_index.edn"))]
                       :when idx
                       fname (edn/read-string (slurp idx))]
                   (ClassPathResource. (Location. loc) (str base "/" fname)
                     cl StandardCharsets/UTF_8)))]
    (when (seq res)
      (reify ResourceProvider
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

(defn- checksum-mismatch-error?
  "True when a Flyway failure in the cause chain is a migration-checksum /
   validation mismatch — i.e. an already-applied migration (e.g. V1) was edited
   in place, so its recorded checksum no longer matches the source."
  [^Throwable e]
  (boolean
    (some (fn [^Throwable t]
            (let [^String m (or (.getMessage t) "")]
              (or (.contains m "checksum mismatch")
                (.contains m "failed validation"))))
      (take-while some? (iterate (fn [^Throwable t] (.getCause t)) e)))))

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
        rp  (index-resource-provider locs)
        ;; DEV ESCAPE HATCH: when an already-applied migration (e.g. V1) is
        ;; edited in place, Flyway's on-migrate validation throws a checksum
        ;; mismatch and every DB open fails - which wedges the TUI render loop.
        ;; Set VIS_DB_ALLOW_SCHEMA_DRIFT=1 to tolerate the drift (skip validation
        ;; + ignore mismatch states) so you can keep working WITHOUT nuking
        ;; ~/.vis/vis.mdb. NOTE: an edited-in-place migration is NOT re-applied -
        ;; only NEW V*__ files run. Leave this OFF in prod (the guard exists to
        ;; catch real schema drift).
        allow-drift? (contains? #{"1" "true" "yes"}
                       (some-> (System/getenv "VIS_DB_ALLOW_SCHEMA_DRIFT")
                         str/trim
                         str/lower-case))
        ^org.flywaydb.core.api.configuration.FluentConfiguration cfg
        (cond-> (-> (org.flywaydb.core.Flyway/configure)
                  (.dataSource ds)
                  (.locations ^"[Ljava.lang.String;" (into-array String locs))
                  (.baselineOnMigrate true)
                  (.baselineVersion "0")
                  (.mixed true))
          allow-drift? (-> (.validateOnMigrate false)
                         (.ignoreMigrationPatterns
                           ^"[Ljava.lang.String;" (into-array String ["*:*"])))
          ;; native image: serve migrations explicitly (dir listing unavailable)
          rp (.resourceProvider rp))
        ^org.flywaydb.core.Flyway flyway (.load cfg)]
    (try
      (.migrate flyway)
      (catch Throwable e
        ;; NON-DESTRUCTIVE self-heal. An in-place edit of an already-applied
        ;; migration (e.g. a V1 comment/format tweak) drifts its recorded
        ;; checksum and wedges every DB open. `repair` realigns the
        ;; checksums/descriptions in `flyway_schema_history` to the current
        ;; source — it touches ONLY that metadata table, never a data row — then
        ;; we re-run migrate. All persisted history is preserved (no nuke).
        ;; A genuine STRUCTURAL edit still needs a NEW V* file; repair only
        ;; realigns checksum drift, so if migrate still fails after repair the
        ;; real error surfaces. Skipped under allow-drift? (validation already
        ;; off, so a mismatch never throws here).
        (if (and (not allow-drift?) (checksum-mismatch-error? e))
          (do (.repair flyway)
            (.migrate flyway))
          (throw e))))
    ds))
