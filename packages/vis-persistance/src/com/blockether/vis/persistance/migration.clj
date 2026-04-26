(ns com.blockether.vis.persistance.migration
  "Backend-agnostic migration runner.

   Wraps Flyway's fluent configure/migrate calls behind a single fn
   so every backend installs schema the same way. Dialect support
   (e.g. `flyway-database-nc-sqlite`) lives in the matching backend
   package \u2014 this namespace only depends on `flyway-core`, which is
   shipped by `vis-persistance` itself.

   Typical backend usage:

     (ns my.backend
       (:require [com.blockether.vis.persistance.migration :as migration]))

     (defn open-store [spec]
       (let [ds (build-datasource spec)]
         (migration/migrate! ds [\\\"classpath:db/<dialect>/migration\\\"])
         {:datasource ds \u2026}))

   The classpath location convention is
   `db/<dialect>/migration/V*__schema.sql`, all canonical SQL ships
   in `vis-persistance/resources/db/<dialect>/migration/` so backends
   never need to ship migration files of their own."
  (:import (javax.sql DataSource)))

(defn migrate!
  "Apply every Flyway migration found at the given classpath
   `locations` to `ds`. `locations` defaults to a single dialect
   directory if a string is passed, or accepts a coll of strings.

   `:baseline-on-migrate true` so existing databases without a
   `flyway_schema_history` table get one on the first run.
   `:mixed true` so SQL files with mixed transactional + DDL
   statements work under SQLite (and other dialects that need it).

   Returns `ds`."
  [^DataSource ds locations]
  (let [locs   (cond
                 (string? locations)     [locations]
                 (sequential? locations) (vec locations)
                 :else
                 (throw (ex-info "locations must be a string or coll of strings"
                          {:type :persistance/invalid-migration-locations
                           :got  (type locations)})))
        flyway (-> (org.flywaydb.core.Flyway/configure)
                 (.dataSource ds)
                 (.locations (into-array String locs))
                 (.baselineOnMigrate true)
                 (.baselineVersion "0")
                 (.mixed true)
                 (.load))]
    (.migrate flyway)
    ds))
