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

(defn- master-sql
  "`name -> DDL` for every object in the store's schema."
  [^javax.sql.DataSource ds]
  (with-open
    [conn
     (.getConnection ds)

     st
     (.createStatement conn)

     rs
     (.executeQuery st "SELECT name, sql FROM sqlite_master WHERE sql IS NOT NULL")]

    (loop [acc {}]
      (if (.next rs) (recur (assoc acc (.getString rs "name") (.getString rs "sql"))) acc))))

(defn- fts-count
  [^javax.sql.DataSource ds ^String term]
  (with-open
    [conn
     (.getConnection ds)

     st
     (.createStatement conn)

     rs
     (.executeQuery st
                    (str "SELECT count(*) FROM transcript_reply_fts"
                         " WHERE transcript_reply_fts MATCH '"
                         term
                         "'"))]

    (when (.next rs) (.getLong rs 1))))

(defn- legacy-reply-fts
  "The reply index as the RETIRED V1 shipped it: `column` is a third indexed
   field and all three sync triggers read it. This is what makes SQLite refuse
   `DROP COLUMN` on a store that already exists — the reason the retired column
   used to survive every open."
  [^String column]
  ["DROP TRIGGER trg_transcript_reply_fts_ai" "DROP TRIGGER trg_transcript_reply_fts_au"
   "DROP TRIGGER trg_transcript_reply_fts_ad" "DROP TABLE transcript_reply_fts"
   (str "CREATE VIRTUAL TABLE transcript_reply_fts USING fts5("
        "llm_assistant_prose, llm_thinking, " column
        ", " "content='session_turn_iteration', content_rowid='rowid', tokenize='unicode61')")
   (str "CREATE TRIGGER trg_transcript_reply_fts_ai AFTER INSERT ON session_turn_iteration BEGIN"
        " INSERT INTO transcript_reply_fts(rowid, llm_assistant_prose, llm_thinking, "
        column
        ")"
        " VALUES (new.rowid, new.llm_assistant_prose, new.llm_thinking, new."
        column
        "); END")
   (str "CREATE TRIGGER trg_transcript_reply_fts_ad AFTER DELETE ON session_turn_iteration BEGIN"
        " INSERT INTO transcript_reply_fts(transcript_reply_fts, rowid, llm_assistant_prose,"
        " llm_thinking, "
        column
        ")"
        " VALUES ('delete', old.rowid, old.llm_assistant_prose, old.llm_thinking, old."
        column
        "); END")
   (str "CREATE TRIGGER trg_transcript_reply_fts_au AFTER UPDATE ON session_turn_iteration BEGIN"
        " INSERT INTO transcript_reply_fts(transcript_reply_fts, rowid, llm_assistant_prose,"
        " llm_thinking, "
        column
        ")"
        " VALUES ('delete', old.rowid, old.llm_assistant_prose, old.llm_thinking, old."
        column
        ");"
        " INSERT INTO transcript_reply_fts(rowid, llm_assistant_prose, llm_thinking, "
        column
        ")"
        " VALUES (new.rowid, new.llm_assistant_prose, new.llm_thinking, new."
        column
        "); END")])

;; Regression: `llm_assistant_message` shipped in V1 and was then retired, but a
;; store CREATED by the older V1 kept the column — and 471 MB of raw provider
;; envelopes in it — because one canonical migration cannot alter a database
;; that already exists. The first retirement pass then dropped nothing at all on
;; a real store: SQLite refuses DROP COLUMN while the old FTS triggers read it,
;; and the best-effort catch swallowed the refusal.
(defdescribe
  retired-column-test
  (it
    "drops a retired column an existing store still carries, keeping its rows"
    (let
      [[^java.io.File file ds]
       (temp-ds)

       [table column]
       (first @#'migration/retired-columns)]

      (try (migration/migrate! ds [migration-dir])
           ;; the shape an older V1 left behind
           (exec! ds (str "ALTER TABLE " table " ADD COLUMN " column " TEXT"))
           (doseq [stmt (legacy-reply-fts column)]
             (exec! ds stmt))
           (exec! ds
                  (str "INSERT INTO "
                       table
                       " (id, session_turn_state_id, position, status, code, created_at,"
                       " llm_assistant_prose, " column
                       ") VALUES ('i1', 't1', 1, 'done', 'print(1)', 1, 'canary prose',"
                       " 'envelope')"))
           (expect (contains? (columns-of ds table) (str/lower-case column)))
           (migration/migrate! ds [migration-dir])
           (expect (not (contains? (columns-of ds table) (str/lower-case column))))
           ;; the table REWRITE keeps every row and every other column
           (expect (= 1 (row-count ds table)))
           (expect (contains? (columns-of ds table) "llm_thinking"))
           ;; the index and its triggers come back CANONICAL: recreated from the
           ;; shipped SQL, so nothing in the schema still names the retired column
           (let [ddl (master-sql ds)]
             (expect (every? ddl
                             ["transcript_reply_fts" "trg_transcript_reply_fts_ai"
                              "trg_transcript_reply_fts_ad" "trg_transcript_reply_fts_au"]))
             (expect (not-any? #(str/includes? % column) (vals ddl))))
           ;; rebuilt, so a row stored before the retirement is still findable,
           ;; and the recreated triggers keep feeding the index
           (expect (= 1 (fts-count ds "canary")))
           (exec! ds (str "UPDATE " table " SET llm_assistant_prose = 'kestrel' WHERE id = 'i1'"))
           (expect (= 1 (fts-count ds "kestrel")))
           (finally (.delete file))))))
