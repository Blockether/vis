(ns com.blockether.vis.internal.persistance.sqlite.migration-test
  "Indexed native migrations must not trigger Flyway's classpath scanner."
  (:require [clojure.java.io :as io]
            [babashka.fs :as fs]
            [clojure.string :as str]
            [com.blockether.vis.internal.persistance.sqlite.migration :as migration]
            [lazytest.core :refer [defdescribe expect it]]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs])
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

;; Regression, this Vis session (paraphrased: "give the group listing an index"):
;; an index ADDED to a table that already existed never reached a store created
;; by an older V1 - `install-missing-canonical-tables!` only brings the indexes
;; of a table it installs itself - so the query kept paying a full scan.
(defdescribe
  canonical-index-top-up-test
  (it "installs an index the canonical schema gained after the store was created"
      (let [dir
            (fs/create-temp-dir {:prefix "vis-migration-index-"})

            locations
            ["db/sqlite/migration"]

            ds
            (doto (SQLiteDataSource.) (.setUrl (str "jdbc:sqlite:" (io/file (str dir) "vis.db"))))

            indexes
            (fn []
              (into #{}
                    (map :name)
                    (jdbc/execute! ds
                                   ["SELECT name FROM sqlite_master WHERE type = 'index'"]
                                   {:builder-fn rs/as-unqualified-lower-maps})))]

        (try (migration/migrate! ds locations)
             (expect (contains? (indexes) "idx_session_soul_group"))
             ;; A store created by an older V1 looks exactly like this one.
             (jdbc/execute! ds ["DROP INDEX idx_session_soul_group"])
             (expect (not (contains? (indexes) "idx_session_soul_group")))
             (expect (identical? ds (migration/migrate! ds locations)))
             (expect (contains? (indexes) "idx_session_soul_group"))
             (finally (fs/delete-tree dir))))))

(defdescribe
  derived-session-turn-stats-migration-test
  (it
    "fills a shared old store once and repairs missing or changed triggers"
    (let [dir
          (fs/create-temp-dir {:prefix "vis-migration-turn-stats-"})

          ds
          (doto (SQLiteDataSource.) (.setUrl (str "jdbc:sqlite:" (io/file (str dir) "vis.db"))))

          locations
          ["db/sqlite/migration"]

          migrate!
          #(migration/migrate! ds locations {:shared? true})

          exec!
          #(jdbc/execute! ds [%] {:builder-fn rs/as-unqualified-lower-maps})

          table-rows
          #(exec!
             "SELECT turn_count, answer_count, last_status, latest_turn_at FROM session_turn_stats")

          trigger
          "trg_session_turn_stats_turn_state_au"]

      (try
        (migrate!)
        ;; Remove the new schema objects to reproduce an older version of V1.
        (doseq [{:keys [name]} (exec! (str "SELECT name FROM sqlite_master WHERE type = 'trigger' "
                                           "AND name GLOB 'trg_session_turn_stats_*'"))]
          (exec! (str "DROP TRIGGER " name)))
        (exec! "DROP TABLE session_turn_stats")
        (exec! "DROP INDEX idx_session_agent_leader_status")
        (exec! "INSERT INTO session_soul (id, created_at, claimed_at) VALUES ('soul', 0, 0)")
        (exec!
          "INSERT INTO workspace (id, repo_id, root, created_at) VALUES ('ws', 'repo', '/', 0)")
        (exec!
          (str "INSERT INTO session_state (id, session_soul_id, workspace_id, version, created_at) "
               "VALUES ('state', 'soul', 'ws', 0, 0)"))
        (exec! (str "INSERT INTO session_turn_soul (id, session_state_id, position, created_at) "
                    "VALUES ('turn', 'state', 1, 0)"))
        (exec!
          (str
            "INSERT INTO session_turn_state (id, session_turn_soul_id, version, status, created_at) "
            "VALUES ('reply', 'turn', 0, 'done', 0)"))
        (exec!
          (str
            "INSERT INTO session_turn_iteration (id, session_turn_state_id, position, status, created_at, code) "
            "VALUES ('round', 'reply', 1, 'done', 3, '')"))
        (migrate!)
        (expect (= [{:turn_count 1 :answer_count 1 :last_status "done" :latest_turn_at 3}]
                   (table-rows)))
        (with-open [observer (jdbc/get-connection ds)]
          (let [version #(-> (jdbc/execute-one! observer
                                                ["PRAGMA data_version"]
                                                {:builder-fn rs/as-unqualified-lower-maps})
                             :data_version)
                before (version)]

            (migrate!)
            (expect (= before (version)))))
        (exec! (str "DROP TRIGGER " trigger))
        (exec! "UPDATE session_turn_state SET status = 'running' WHERE id = 'reply'")
        (migrate!)
        (expect (= [{:turn_count 1 :answer_count 0 :last_status "running" :latest_turn_at nil}]
                   (table-rows)))
        (let [sql (:sql (first (exec! (str "SELECT sql FROM sqlite_master WHERE name = '"
                                           trigger
                                           "'"))))]
          (exec! (str "DROP TRIGGER " trigger))
          (exec! (str/replace sql "WHEN " "WHEN 0 AND ")))
        (exec! "UPDATE session_turn_state SET status = 'error' WHERE id = 'reply'")
        (migrate!)
        (expect (= [{:turn_count 1 :answer_count 1 :last_status "error" :latest_turn_at 3}]
                   (table-rows)))
        (exec! "UPDATE session_turn_state SET status = 'interrupted' WHERE id = 'reply'")
        (expect (= "interrupted" (:last_status (first (table-rows)))))
        (finally (fs/delete-tree dir))))))

(defdescribe
  council-request-search-migration-test
  (it "reindexes an existing Council turn with the text people see"
      (let [dir (fs/create-temp-dir {:prefix "vis-migration-request-search-"})
            ds (doto (SQLiteDataSource.) (.setUrl (str "jdbc:sqlite:" (io/file (str dir) "vis.db"))))
            migrate! #(migration/migrate! ds ["db/sqlite/migration"] {:shared? true})
            exec! #(jdbc/execute! ds [%] {:builder-fn rs/as-unqualified-lower-maps})
            snippets (fn [text]
                       (mapv :snippet
                             (jdbc/execute!
                               ds
                               [(str "SELECT snippet(transcript_request_fts, 0, '', '', '…', 20) AS snippet "
                                     "FROM transcript_request_fts WHERE transcript_request_fts MATCH ?")
                                text]
                               {:builder-fn rs/as-unqualified-lower-maps})))]
        (try (migrate!)
             (doseq [{:keys [name]} (exec! (str "SELECT name FROM sqlite_master WHERE type = 'trigger' "
                                                "AND name GLOB 'trg_transcript_request_fts_*'"))]
               (exec! (str "DROP TRIGGER " name)))
             (exec! "DROP TABLE transcript_request_fts")
             (exec! "DROP VIEW IF EXISTS transcript_request_text")
             (exec! (str "CREATE VIRTUAL TABLE transcript_request_fts USING fts5("
                         "user_request, content='session_turn_soul', content_rowid='rowid', "
                         "tokenize='unicode61', prefix='2 3')"))
             (exec! "INSERT INTO session_soul (id, created_at, claimed_at) VALUES ('soul', 0, 0)")
             (exec! "INSERT INTO workspace (id, repo_id, root, created_at) VALUES ('ws', 'repo', '/', 0)")
             (exec! (str "INSERT INTO session_state (id, session_soul_id, workspace_id, version, created_at) "
                         "VALUES ('state', 'soul', 'ws', 0, 0)"))
             (exec! (str "INSERT INTO council_entry (id, author_sid, activation_id, title, content, created_at, idempotency_key, fingerprint) "
                         "VALUES (1, 'child', 'active', 'Review', 'Zebracanvas needs review', 0, 'one', 'one')"))
             (exec! (str "INSERT INTO session_turn_soul (id, session_state_id, position, created_at, request_kind, council_entry_id, user_request) "
                         "VALUES ('turn', 'state', 1, 0, 'council', 1, 'Quartzflag is model-only')"))
             (exec! "INSERT INTO transcript_request_fts(transcript_request_fts) VALUES ('rebuild')")
             (expect (= ["Quartzflag is model-only"] (snippets "Quartzflag")))
             (migrate!)
             (expect (= ["Zebracanvas needs review"] (snippets "Zebracanvas")))
             (expect (empty? (snippets "Quartzflag")))
             (with-open [observer (jdbc/get-connection ds)]
               (let [version #(:data_version (jdbc/execute-one! observer ["PRAGMA data_version"]
                                                               {:builder-fn rs/as-unqualified-lower-maps}))
                     before (version)]
                 (migrate!)
                 (expect (= before (version)))))
             (exec! "DELETE FROM session_turn_soul WHERE id = 'turn'")
             (expect (empty? (snippets "Zebracanvas")))
             (finally (fs/delete-tree dir)))))
  (it "drops a retired request index trigger and indexes every entry of a turn"
      (let [dir (fs/create-temp-dir {:prefix "vis-migration-request-batch-"})
            ds (doto (SQLiteDataSource.) (.setUrl (str "jdbc:sqlite:" (io/file (str dir) "vis.db"))))
            migrate! #(migration/migrate! ds ["db/sqlite/migration"] {:shared? true})
            exec! #(jdbc/execute! ds [%] {:builder-fn rs/as-unqualified-lower-maps})
            triggers #(set (map :name (exec! (str "SELECT name FROM sqlite_master WHERE type = 'trigger' "
                                                  "AND name GLOB 'trg_transcript_request_fts_*'"))))
            matches #(count (exec! (str "SELECT rowid FROM transcript_request_fts "
                                        "WHERE transcript_request_fts MATCH '" % "'")))]
        (try (migrate!)
             (let [current (triggers)]
               (exec! (str "CREATE TRIGGER trg_transcript_request_fts_ad AFTER DELETE ON session_turn_soul "
                           "BEGIN INSERT INTO transcript_request_fts(transcript_request_fts, rowid, user_request) "
                           "VALUES ('delete', old.rowid, old.user_request); END"))
               (migrate!)
               (expect (= current (triggers))))
             (exec! "INSERT INTO session_soul (id, created_at, claimed_at) VALUES ('soul', 0, 0)")
             (exec! "INSERT INTO workspace (id, repo_id, root, created_at) VALUES ('ws', 'repo', '/', 0)")
             (exec! (str "INSERT INTO session_state (id, session_soul_id, workspace_id, version, created_at) "
                         "VALUES ('state', 'soul', 'ws', 0, 0)"))
             (exec! (str "INSERT INTO council_entry (id, author_sid, activation_id, title, content, created_at, idempotency_key, fingerprint) "
                         "VALUES (1, 'child', 'active', 'One', 'Zebracanvas started', 0, 'one', 'one'), "
                         "(2, 'child', 'active', 'Two', 'Antaresquill passed', 0, 'two', 'two')"))
             (jdbc/with-transaction [tx ds]
               (jdbc/execute! tx [(str "INSERT INTO session_turn_council_entry (session_turn_soul_id, position, council_entry_id) "
                                       "VALUES ('turn', 2, 2)")])
               (jdbc/execute! tx [(str "INSERT INTO session_turn_soul (id, session_state_id, position, created_at, request_kind, council_entry_id, user_request) "
                                       "VALUES ('turn', 'state', 1, 0, 'council', 1, 'Quartzflag is model-only')")]))
             (expect (= [1 1 0] (mapv matches ["Zebracanvas" "Antaresquill" "Quartzflag"])))
             (exec! "DELETE FROM session_turn_soul WHERE id = 'turn'")
             (expect (= [0 0] (mapv matches ["Zebracanvas" "Antaresquill"])))
             (finally (fs/delete-tree dir))))))
