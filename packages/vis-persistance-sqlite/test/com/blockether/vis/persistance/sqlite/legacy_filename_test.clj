(ns com.blockether.vis.persistance.sqlite.legacy-filename-test
  "Regression: the canonical SQLite file used to be `rlm.db`. New installs
   write `vis.db`. Upgraded installs MUST keep their conversations: opening
   a directory that still has `rlm.db` (and its `-wal` / `-shm` sidecars)
   transparently renames it to `vis.db` on first open.

   The check writes a marker row through the original (legacy-named)
   handle, closes it, then reopens the SAME directory through the
   public `create-rlm-conn` API and confirms (a) the legacy file is
   gone, (b) the canonical file exists, (c) the marker row survived."
  (:require
   [clojure.java.io :as io]
   [com.blockether.vis.persistance.core :as db]
   [honey.sql :as sql]
   [lazytest.core :refer [describe it expect]]
   [next.jdbc :as jdbc]
   [next.jdbc.result-set :as rs]))

(def ^:private jdbc-opts {:builder-fn rs/as-unqualified-lower-maps})

(defn- temp-dir ^java.io.File []
  (let [f (java.io.File/createTempFile "vis-legacy-rename" "")]
    (.delete f)
    (.mkdirs f)
    f))

(defn- delete-recursively! [^java.io.File dir]
  (when (.exists dir)
    (doseq [^java.io.File c (reverse (file-seq dir))]
      (.delete c))))

(describe "Legacy DB filename auto-migration"
  (it "renames rlm.db -> vis.db on first open and preserves data"
    (let [dir   (temp-dir)
          path  (.getAbsolutePath dir)]
      (try
        ;; 1. Seed a `rlm.db` by opening the legacy filename directly,
        ;;    creating the schema via the same backend, and inserting a
        ;;    marker row. Going through `create-rlm-conn` would already
        ;;    pick the new name, so we shortcut to JDBC for setup.
        (let [legacy-jdbc (str "jdbc:sqlite:" path "/rlm.db")]
          (with-open [conn (jdbc/get-connection {:jdbcUrl legacy-jdbc})]
            (jdbc/execute! conn ["CREATE TABLE marker (id TEXT PRIMARY KEY, note TEXT)"])
            (jdbc/execute! conn ["INSERT INTO marker (id, note) VALUES ('seed','hello')"])))

        (expect (.exists (io/file dir "rlm.db")))
        (expect (not (.exists (io/file dir "vis.db"))))

        ;; 2. Open the directory through the public API. The backend
        ;;    must rename rlm.db -> vis.db before installing schema.
        (let [store (db/create-rlm-conn {:backend :sqlite :path path})]
          (try
            (expect (not (.exists (io/file dir "rlm.db"))))
            (expect (.exists (io/file dir "vis.db")))

            ;; 3. The marker row survived the rename.
            (let [rows (jdbc/execute! (:datasource store)
                         (sql/format {:select [:*] :from [:marker]})
                         jdbc-opts)]
              (expect (= 1 (count rows)))
              (expect (= "hello" (:note (first rows)))))
            (finally
              (db/dispose-rlm-conn! store))))
        (finally
          (delete-recursively! dir))))))
