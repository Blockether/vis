(ns com.blockether.vis.internal.persistance-test
  "Tests for the persistence facade: adapter-owned error translation and
   process-wide shared connection lifecycle."
  (:require
   [babashka.fs :as fs]
   [clojure.string :as str]
   [com.blockether.vis.ext.persistance-sqlite.core]
   [com.blockether.vis.internal.manifest :as manifest]
   [com.blockether.vis.internal.persistance :as persistance]
   [lazytest.core :refer [defdescribe expect it]])
  (:import
   (java.nio.file Files)
   (java.nio.file.attribute FileAttribute)))

(defdescribe db-error-translation-test
  (it "delegates SQLite CANTOPEN translation to the SQLite adapter"
    (let [message (persistance/db-error->user-message
                    (ex-info "[SQLITE_CANTOPEN] unable to open database file" {}))]
      (expect (str/includes? message "Vis database is unavailable"))
      (expect (str/includes? message "/.vis/vis.mdb/vis.db"))))

  (it "falls back to the exception message when no adapter recognizes it"
    (expect (= "plain failure"
              (persistance/db-error->user-message (ex-info "plain failure" {}))))))

(defdescribe shared-connection-refresh-test
  (it "reopens the shared persistent SQLite store when the db file is replaced"
    (let [dir (str (Files/createTempDirectory "vis-persist-test" (make-array FileAttribute 0)))]
      (try
        (try (persistance/db-dispose-shared-connection!) (catch Throwable _ nil))
        (let [first-store (with-redefs [manifest/scan-extensions! (fn [] nil)]
                            (persistance/db-shared-connection! {:backend :sqlite :path dir}))
              db-file     (:db-file first-store)
              old-ds      (:datasource first-store)
              old-key     (:file-key-snapshot first-store)]
          (fs/delete db-file)
          (spit db-file "")
          (let [second-store (with-redefs [manifest/scan-extensions! (fn [] nil)]
                               (persistance/db-shared-connection! {:backend :sqlite :path dir}))]
            (expect (not (identical? old-ds (:datasource second-store))))
            (expect (not= old-key (:file-key-snapshot second-store)))))
        (finally
          (try (persistance/db-dispose-shared-connection!) (catch Throwable _ nil))
          (fs/delete-tree dir))))))
