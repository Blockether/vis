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

(defdescribe shipped-migrations-test
             (it "exactly one canonical migration ships — V1, with no V2+ leftovers"
                 (expect (= ["V1__schema.sql"] (migration-files)))))

(defdescribe transcript-fts-schema-test
             (it "V1 creates both external-content FTS5 indexes and keeps them trigger-synced"
                 (let [sql (v1-sql)]
                   (expect (str/includes? sql "CREATE VIRTUAL TABLE transcript_request_fts"))
                   (expect (str/includes? sql "CREATE VIRTUAL TABLE transcript_reply_fts"))
                   ;; External content = index only, no second copy of the text.
                   (expect (str/includes? sql "content='session_turn_soul'"))
                   (expect (str/includes? sql "content='session_turn_iteration'"))
                   ;; Insert/update/delete triggers on BOTH source tables.
                   (expect (= 6 (count (re-seq #"CREATE TRIGGER trg_transcript_" sql))))
                   ;; And a one-time backfill of everything already stored.
                   (expect (= 2 (count (re-seq #"VALUES \('rebuild'\)" sql)))))))

(defdescribe snake-schema-test
             (it "every identifier is snake_case — no kebab-case, no `?` in the DDL"
                 (let [sql (v1-sql)]
                   (expect (not (str/includes? sql "?")))
                   (expect (nil? (re-find #"(?m)^\s*[a-z0-9_]+-[a-z0-9-]+\s" sql)))))
             (it "projects are cross-channel: the project table has NO channel column"
                 (let
                   [sql
                    (v1-sql)

                    project-ddl
                    (re-find #"(?s)CREATE TABLE project\s*\((.*?)\);" sql)]

                   (expect (some? project-ddl))
                   (expect (not (str/includes? (str/lower-case (second project-ddl)) "channel"))))))
