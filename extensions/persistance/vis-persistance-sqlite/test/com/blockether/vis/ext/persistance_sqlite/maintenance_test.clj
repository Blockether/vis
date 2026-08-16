(ns com.blockether.vis.ext.persistance-sqlite.maintenance-test
  "The fortnightly space reclaim: a store is rewritten only when the freelist is
   both OLD enough and BIG enough, and the `vis.db.vacuum` marker is what makes
   the window a window."
  (:require [com.blockether.vis.ext.persistance-sqlite.maintenance :as maintenance]
            [lazytest.core :refer [defdescribe expect it]])
  (:import (java.io File)))

(def ^:private day-ms 86400000)

(defn- temp-store
  "A file-backed store nobody else shares: `[path datasource]`."
  []
  (let
    [f
     (File/createTempFile "vis-vacuum-" ".db")

     path
     (.getAbsolutePath f)]

    (.delete f)
    [path (doto (org.sqlite.SQLiteDataSource.) (.setUrl (str "jdbc:sqlite:" path)))]))

(defn- exec!
  [^javax.sql.DataSource ds ^String sql]
  (with-open
    [conn
     (.getConnection ds)

     st
     (.createStatement conn)]

    (.executeUpdate st sql)))

(defn- fill!
  "Write `n` rows of ~4 KB so the store grows real pages."
  [^javax.sql.DataSource ds ^long n]
  (exec! ds "CREATE TABLE IF NOT EXISTS bulk (id INTEGER PRIMARY KEY, payload BLOB)")
  (with-open
    [conn
     (.getConnection ds)

     ps
     (.prepareStatement conn "INSERT INTO bulk (payload) VALUES (?)")]

    (.setAutoCommit conn false)
    (let [payload (byte-array 4096 (byte 7))]
      (dotimes [_ n]
        (.setBytes ps 1 payload)
        (.addBatch ps))
      (.executeBatch ps)
      (.commit conn))))

(defn- cleanup!
  [^String path]
  (doseq [suffix ["" "-wal" "-shm" ".vacuum"]]
    (try (.delete (File. (str path suffix))) (catch Throwable _ nil))))

(defdescribe
  free-space-test
  (it "reports the freelist SQLite is holding, not the file size"
      (let [[path ds] (temp-store)]
        (try (fill! ds 2000)
             (expect (zero? (long (:freelist-count (maintenance/free-space ds)))))
             (exec! ds "DELETE FROM bulk")
             (let [{:keys [free-bytes file-bytes freelist-count]} (maintenance/free-space ds)]
               (expect (pos? (long freelist-count)))
               (expect (pos? (long free-bytes)))
               (expect (> (double free-bytes) (* 0.5 (double (long file-bytes))))))
             (finally (cleanup! path))))))

(defdescribe compact-store-is-left-alone-test
             (it "a store with nothing to reclaim is never rewritten and leaves no marker"
                 (let [[path ds] (temp-store)]
                   (try (fill! ds 200)
                        (let [report (maintenance/maybe-vacuum! ds path {:min-free-bytes 1024})]
                          (expect (false? (:is-vacuumed report)))
                          (expect (= :compact (:reason report)))
                          (expect (not (.isFile (File. (str path ".vacuum"))))))
                        (finally (cleanup! path))))))

(defdescribe
  vacuum-window-test
  (it
    "reclaims a big freelist once, then holds the fortnight before doing it again"
    (let
      [[path ds]
       (temp-store)

       opts
       {:min-free-bytes 1024}]

      (try (fill! ds 2000)
           (exec! ds "DELETE FROM bulk")
           (let
             [before
              (.length (File. path))

              report
              (maintenance/maybe-vacuum! ds path opts)]

             (expect (true? (:is-vacuumed report)))
             (expect (= :vacuumed (:reason report)))
             ;; The FILE gave the space back, not just the freelist.
             (expect (< (.length (File. path)) before))
             (expect (zero? (long (:freelist-count (maintenance/free-space ds)))))
             (expect (.isFile (File. (str path ".vacuum")))))
           ;; Inside the window a fresh freelist waits its turn.
           (fill! ds 2000)
           (exec! ds "DELETE FROM bulk")
           (let [report (maintenance/maybe-vacuum! ds path opts)]
             (expect (false? (:is-vacuumed report)))
             (expect (= :recent (:reason report))))
           ;; A fortnight later it is due again.
           (let
             [later
              (+ (System/currentTimeMillis)
                 (* (long maintenance/vacuum-interval-days) (long day-ms))
                 1000)

              report
              (maintenance/maybe-vacuum! ds path (assoc opts :now-ms later))]

             (expect (true? (:is-vacuumed report)))
             (expect (zero? (long (:freelist-count (maintenance/free-space ds))))))
           (finally (cleanup! path))))))
