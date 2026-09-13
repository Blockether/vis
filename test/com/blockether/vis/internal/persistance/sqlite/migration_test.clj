(ns com.blockether.vis.internal.persistance.sqlite.migration-test
  "Indexed native migrations must not trigger Flyway's classpath scanner."
  (:require [clojure.java.io :as io]
            [babashka.fs :as fs]
            [com.blockether.vis.internal.persistance.sqlite.migration :as migration]
            [lazytest.core :refer [defdescribe expect it]]
            [next.jdbc :as jdbc])
  (:import [java.net URL URLClassLoader]
           [org.sqlite SQLiteDataSource]))

(defdescribe
  migration-resource-discovery-test
  (it
    "uses indexed SQL without scanning, while preserving JVM discovery and repair"
    (doseq [indexed? [false true]]
      (let [dir (fs/create-temp-dir {:prefix "vis-migration-discovery-"})
            base "db/sqlite/migration"
            index (io/file (str dir) base "_index.edn")
            scans (atom [])
            thread (Thread/currentThread)
            original-loader (.getContextClassLoader thread)
            ds (doto (SQLiteDataSource.)
                 (.setUrl (str "jdbc:sqlite:" (io/file (str dir) "vis.db"))))]

        (try (when indexed? (io/make-parents index) (spit index (pr-str ["V1__schema.sql"])))
             (with-open [loader (proxy [URLClassLoader] [(into-array URL
                                                                     [(-> dir
                                                                          .toUri
                                                                          .toURL)])
                                                         (clojure.lang.RT/baseLoader)]
                                  (getResources [name]
                                    (when (= base name) (swap! scans conj name))
                                    (proxy-super getResources name)))]
               (.setContextClassLoader thread loader)
               (with-bindings {clojure.lang.Compiler/LOADER loader}
                 (expect (identical? ds (migration/migrate! ds (str "classpath:" base))))
                 (expect
                   (= {:flyway_schema_history/version "1" :flyway_schema_history/success 1}
                      (jdbc/execute-one!
                        ds
                        ["SELECT version, success FROM flyway_schema_history WHERE type = 'SQL'"])))
                 (jdbc/execute! ds ["CREATE TABLE migration_probe (id INTEGER)"])
                 (jdbc/execute! ds ["INSERT INTO migration_probe VALUES (42)"])
                 (jdbc/execute!
                   ds
                   ["UPDATE flyway_schema_history SET checksum = -999 WHERE version = '1'"])
                 (expect (identical? ds (migration/migrate! ds [base])))
                 (expect (= {:migration_probe/id 42}
                            (jdbc/execute-one! ds ["SELECT id FROM migration_probe"])))
                 (expect (if indexed? (empty? @scans) (seq @scans))
                         (str "indexed?=" indexed? ", directory scans=" (pr-str @scans)))))
             (finally (.setContextClassLoader thread original-loader) (fs/delete-tree dir)))))))
