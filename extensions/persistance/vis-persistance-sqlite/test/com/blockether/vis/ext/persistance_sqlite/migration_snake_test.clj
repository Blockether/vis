(ns com.blockether.vis.ext.persistance-sqlite.migration-snake-test
  "Single-migration + snake-schema guard: exactly ONE Flyway migration (V1)
   ships, and its DDL is fully canonical — snake_case identifiers only, no
   kebab, no `?`, and the cross-channel `project` table carries NO channel
   column. Plus the RETIRED-column pass, which strips what an older V1 left
   behind in a store that already exists."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.ext.persistance-sqlite.migration :as migration]
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

(defn- temp-ds
  "A file-backed store nobody else shares, with the file to delete afterwards."
  []
  (let [f (java.io.File/createTempFile "vis-retired-column-" ".db")]
    (.delete f)
    [f (doto (org.sqlite.SQLiteDataSource.) (.setUrl (str "jdbc:sqlite:" (.getAbsolutePath f))))]))

(defn- exec!
  [^javax.sql.DataSource ds ^String sql]
  (with-open
    [conn
     (.getConnection ds)

     st
     (.createStatement conn)]

    (.executeUpdate st sql)))

(defn- columns-of
  [^javax.sql.DataSource ds ^String table]
  (with-open
    [conn
     (.getConnection ds)

     st
     (.createStatement conn)

     rs
     (.executeQuery st (str "PRAGMA table_info(" table ")"))]

    (loop [acc #{}]
      (if (.next rs) (recur (conj acc (str/lower-case (.getString rs "name")))) acc))))

(defn- row-count
  [^javax.sql.DataSource ds ^String table]
  (with-open
    [conn
     (.getConnection ds)

     st
     (.createStatement conn)

     rs
     (.executeQuery st (str "SELECT count(*) FROM " table))]

    (when (.next rs) (.getLong rs 1))))

;; Regression: `llm_assistant_message` shipped in V1 and was then retired, but a
;; store CREATED by the older V1 kept the column — and 471 MB of raw provider
;; envelopes in it — because one canonical migration cannot alter a database
;; that already exists.
(defdescribe retired-column-test
             (it "drops a retired column an existing store still carries, keeping its rows"
                 (let
                   [[^java.io.File file ds]
                    (temp-ds)

                    [table column]
                    (first @#'migration/retired-columns)]

                   (try (migration/migrate! ds [migration-dir])
                        ;; the shape an older V1 left behind
                        (exec! ds (str "ALTER TABLE " table " ADD COLUMN " column " TEXT"))
                        (exec! ds
                               (str
                                 "INSERT INTO "
                                 table
                                 " (id, session_turn_state_id, position, status, code, created_at, "
                                 column
                                 ") VALUES ('i1', 't1', 1, 'done', 'print(1)', 1, 'envelope')"))
                        (expect (contains? (columns-of ds table) (str/lower-case column)))
                        (migration/migrate! ds [migration-dir])
                        (expect (not (contains? (columns-of ds table) (str/lower-case column))))
                        ;; the table REWRITE keeps every row and every other column
                        (expect (= 1 (row-count ds table)))
                        (expect (contains? (columns-of ds table) "llm_thinking"))
                        (finally (.delete file))))))
