(ns com.blockether.vis.ext.persistance-sqlite.migration-snake-test
  "Single-migration + snake-schema guard: exactly ONE Flyway migration (V1)
   ships, and its DDL is fully canonical — snake_case identifiers only, no
   kebab, no `?`, and the cross-channel `project` table carries NO channel
   column."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [lazytest.core :refer [defdescribe expect it]]))

(def ^:private migration-dir "db/sqlite/migration")

(defn- migration-files
  []
  (let [url (io/resource migration-dir)]
    (->> (file-seq (io/file url))
         (filter #(.isFile ^java.io.File %))
         (map #(.getName ^java.io.File %))
         (filter #(str/ends-with? % ".sql"))
         sort
         vec)))

(defn- v1-sql [] (slurp (io/resource (str migration-dir "/V1__schema.sql"))))

(defdescribe single-unified-migration-test
             (it "exactly one migration ships — V1, no V2+ leftovers"
                 (expect (= ["V1__schema.sql"] (migration-files)))))

(defdescribe snake-schema-test
             (it "every identifier is snake_case — no kebab-case, no `?` in the DDL"
                 (let [sql (v1-sql)]
                   (expect (not (str/includes? sql "?")))
                   (expect (nil? (re-find #"(?m)^\s*[a-z0-9_]+-[a-z0-9-]+\s" sql)))))
             (it "projects are cross-channel: the project table has NO channel column"
                 (let [sql
                       (v1-sql)

                       project-ddl
                       (re-find #"(?s)CREATE TABLE project\s*\((.*?)\);" sql)]

                   (expect (some? project-ddl))
                   (expect (not (str/includes? (str/lower-case (second project-ddl)) "channel"))))))
