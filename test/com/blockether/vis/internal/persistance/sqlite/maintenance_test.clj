(ns com.blockether.vis.internal.persistance.sqlite.maintenance-test
  "The fortnightly space reclaim: a store is rewritten only when the freelist is
   both OLD enough and BIG enough, and the `vis.db.vacuum` marker is what makes
   the window a window."
  (:require [com.blockether.vis.internal.janitor :as janitor]
            [com.blockether.vis.internal.persistance.sqlite.maintenance :as maintenance]
            [lazytest.core :refer [defdescribe expect it]]
            [taoensso.telemere :as tel])
  (:import (java.io File)))

(def ^:private day-ms 86400000)

(defn- temp-store
  "A file-backed store nobody else shares: `[path datasource]`."
  []
  (let [f
        (File/createTempFile "vis-vacuum-" ".db")

        path
        (.getAbsolutePath f)]

    (.delete f)
    [path (doto (org.sqlite.SQLiteDataSource.) (.setUrl (str "jdbc:sqlite:" path)))]))

(defn- exec!
  [^javax.sql.DataSource ds ^String sql]
  (with-open [conn
              (.getConnection ds)

              st
              (.createStatement conn)]

    (.executeUpdate st sql)))

(defn- wal-store
  []
  (let [[path _]
        (temp-store)

        config
        (doto (org.sqlite.SQLiteConfig.)
          (.setJournalMode org.sqlite.SQLiteConfig$JournalMode/WAL)
          (.setBusyTimeout 50))]

    [path (doto (org.sqlite.SQLiteDataSource. config) (.setUrl (str "jdbc:sqlite:" path)))]))

(defn- fill!
  "Write `n` rows of ~4 KB so the store grows real pages."
  [^javax.sql.DataSource ds ^long n]
  (exec! ds "CREATE TABLE IF NOT EXISTS bulk (id INTEGER PRIMARY KEY, payload BLOB)")
  (with-open [conn
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

(defdescribe free-space-test
             (it "reports the freelist SQLite is holding, not the file size"
                 (let [[path ds] (temp-store)]
                   (try (fill! ds 2000)
                        (expect (zero? (long (:freelist-count (maintenance/free-space ds)))))
                        (exec! ds "DELETE FROM bulk")
                        (let [{:keys [free-bytes file-bytes freelist-count]} (maintenance/free-space
                                                                               ds)]
                          (expect (pos? (long freelist-count)))
                          (expect (pos? (long free-bytes)))
                          (expect (> (double free-bytes) (* 0.5 (double (long file-bytes))))))
                        (finally (cleanup! path))))))

(defdescribe new-store-test
             (it "gives a new store incremental vacuum and never rewrites a store with tables"
                 (let [[path ds]
                       (wal-store)

                       [old-path old-ds]
                       (wal-store)]

                   (try (maintenance/use-incremental-vacuum! ds)
                        (expect (= 2 (:auto-vacuum (maintenance/free-space ds))))
                        (fill! old-ds 10)
                        (maintenance/use-incremental-vacuum! old-ds)
                        (expect (zero? (long (:auto-vacuum (maintenance/free-space old-ds)))))
                        (finally (cleanup! path) (cleanup! old-path))))))

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
    (let [[path ds]
          (temp-store)

          opts
          {:min-free-bytes 1024}]

      (try (fill! ds 2000)
           (exec! ds "DELETE FROM bulk")
           (let [before
                 (.length (File. path))

                 report
                 (maintenance/maybe-vacuum! ds path opts)]

             (expect (true? (:is-vacuumed report)))
             (expect (= :vacuumed (:reason report)))
             ;; The FILE gave the space back, not just the freelist.
             (expect (< (.length (File. path)) before))
             (expect (zero? (long (:freelist-count (maintenance/free-space ds)))))
             ;; The rewrite converts the store, so it never needs a full VACUUM again.
             (expect (= 2 (:auto-vacuum (maintenance/free-space ds))))
             (expect (.isFile (File. (str path ".vacuum")))))
           ;; Inside the window a fresh freelist waits its turn.
           (fill! ds 2000)
           (exec! ds "DELETE FROM bulk")
           (let [report (maintenance/maybe-vacuum! ds path opts)]
             (expect (false? (:is-vacuumed report)))
             (expect (= :recent (:reason report))))
           ;; A fortnight later it is due again.
           (let [later
                 (+ (System/currentTimeMillis)
                    (* (long maintenance/vacuum-interval-days) (long day-ms))
                    1000)

                 report
                 (maintenance/maybe-vacuum! ds path (assoc opts :now-ms later))]

             (expect (true? (:is-vacuumed report)))
             (expect (zero? (long (:freelist-count (maintenance/free-space ds))))))
           (finally (cleanup! path))))))

(defdescribe
  vacuum-disk-space-test
  (it "leaves a reclaimable store due when there is too little disk space"
      (let [[path ds] (temp-store)]
        (try (fill! ds 200)
             (exec! ds "DELETE FROM bulk")
             (let [{:keys [file-bytes freelist-count]} (maintenance/free-space ds)
                   report (maintenance/maybe-vacuum! ds
                                                     path
                                                     {:min-free-bytes 1024
                                                      :usable-space (constantly file-bytes)})]

               (expect (= :low-disk (:reason report)))
               (expect (= file-bytes (:usable-bytes report)))
               (expect (= (* 2 file-bytes) (:required-bytes report)))
               (expect (= freelist-count (:freelist-count (maintenance/free-space ds))))
               (expect (not (.exists (File. (str path ".vacuum"))))))
             (finally (cleanup! path)))))
  (it "does not lock a store that is too large to rewrite inside the busy timeout"
      (let [[path ds] (temp-store)]
        (try (fill! ds 200)
             (exec! ds "DELETE FROM bulk")
             (let [{:keys [file-bytes freelist-count]} (maintenance/free-space ds)
                   report (maintenance/maybe-vacuum! ds
                                                     path
                                                     {:min-free-bytes 1024
                                                      :max-file-bytes (dec (long file-bytes))})]

               (expect (= :too-large (:reason report)))
               (expect (= freelist-count (:freelist-count (maintenance/free-space ds))))
               (expect (not (.exists (File. (str path ".vacuum"))))))
             (finally (cleanup! path)))))
  (it "reclaims an incremental store of any size in chunks, without a full rewrite"
      (let [[path ds] (temp-store)]
        (try (maintenance/use-incremental-vacuum! ds)
             (fill! ds 3000)
             (exec! ds "DELETE FROM bulk")
             (let [before (.length (File. path))
                   report
                   (maintenance/maybe-vacuum! ds path {:min-free-bytes 1024 :max-file-bytes 1})]

               (expect (= :vacuumed (:reason report)))
               (expect (zero? (long (:freelist-count (maintenance/free-space ds)))))
               (expect (< (.length (File. path)) before)))
             (finally (cleanup! path)))))
  (it "truncates the WAL after changing a store with an open connection"
      (let [[path ^javax.sql.DataSource ds]
            (wal-store)

            wal
            (File. (str path "-wal"))]

        (try (with-open [keeper (.getConnection ds)]
               (with-open [st (.createStatement keeper)]
                 (.executeUpdate st "CREATE TABLE bulk (id INTEGER PRIMARY KEY, payload BLOB)"))
               (fill! ds 200)
               (exec! ds "DELETE FROM bulk WHERE id > 10")
               (expect (pos? (.length wal)))
               (expect (= :vacuumed
                          (:reason (maintenance/maybe-vacuum! ds path {:min-free-bytes 1024}))))
               (expect (zero? (.length wal))))
             (finally (cleanup! path)))))
  (it "truncates the WAL even when a vacuum fails"
      (let [[path ^javax.sql.DataSource ds]
            (wal-store)

            wal
            (File. (str path "-wal"))]

        (try (with-open [keeper (.getConnection ds)]
               (with-open [st (.createStatement keeper)]
                 (.executeUpdate st "CREATE TABLE bulk (id INTEGER PRIMARY KEY, payload BLOB)"))
               (fill! ds 200)
               (exec! ds "DELETE FROM bulk WHERE id > 10")
               (expect (pos? (.length wal)))
               (with-redefs-fn {#'maintenance/vacuum! (fn [_]
                                                        (throw (ex-info "Vacuum failed" {})))}
                 #(expect (= :failed
                             (:reason (maintenance/maybe-vacuum! ds path {:min-free-bytes 1024})))))
               (expect (zero? (.length wal))))
             (finally (cleanup! path))))))

;; The vacuum of a store ran on its own thread and dropped each result. On the
;; janitor each result is a line, so a store with no work must give none.

(defn- janitor-lines
  "Call `queue!`, wait until the janitor is past the tasks that it gave, and answer
   the signals of the janitor for the store at `path`."
  [^String path queue!]
  (let [signals
        (atom [])

        done
        (promise)]

    (tel/add-handler! ::janitor-lines (fn ([signal] (swap! signals conj signal)) ([])) {:async nil})
    (try (queue!)
         (janitor/submit! {:id ::done
                           :run (fn []
                                  (deliver done true)
                                  nil)})
         (deref done 30000 nil)
         (filterv #(= [:sqlite-vacuum path] (:task (:data %))) @signals)
         (finally (tel/remove-handler! ::janitor-lines)))))

(defdescribe
  checkpoint-retry-test
  (it "truncates the WAL after a reader releases its snapshot without repeating the vacuum"
      (let [[path ^javax.sql.DataSource ds]
            (wal-store)

            wal
            (File. (str path "-wal"))

            retry
            (atom nil)]

        (try (with-open [keeper (.getConnection ds)]
               (with-open [st (.createStatement keeper)]
                 (.executeUpdate st "CREATE TABLE bulk (id INTEGER PRIMARY KEY, payload BLOB)"))
               (fill! ds 200)
               (with-open [st (.createStatement keeper)
                           rs (.executeQuery st "SELECT id FROM bulk")]

                 (expect (.next rs))
                 (exec! ds "DELETE FROM bulk WHERE id > 10")
                 (with-redefs [janitor/submit! #(reset! retry %)]
                   (expect (= :vacuumed
                              (:reason
                                (maintenance/maybe-vacuum! ds path {:min-free-bytes 1024})))))
                 (expect (pos? (.length wal))))
               (expect (= [:sqlite-checkpoint path] (:id @retry)))
               (janitor-lines path #(janitor/submit! (assoc @retry :delay-ms 0)))
               (expect (zero? (.length wal)))
               (expect (= :recent (:reason (maintenance/maybe-vacuum! ds path)))))
             (finally (cleanup! path))))))

(defdescribe
  queued-vacuum-test
  (it "reclaims the space on the janitor, after the rewrite of the store"
      (let [[path ds]
            (temp-store)

            freelist-at-rewrite
            (atom nil)]

        (try (fill! ds 2000)
             (exec! ds "DELETE FROM bulk")
             (let [before
                   (.length (File. path))

                   lines
                   (janitor-lines path
                                  #(maintenance/vacuum-async! ds
                                                              path
                                                              {:delay-ms 0
                                                               :min-free-bytes 1024
                                                               :before (fn []
                                                                         (reset! freelist-at-rewrite
                                                                           (:freelist-count
                                                                             (maintenance/free-space
                                                                               ds))))}))]

               (expect (pos? (long @freelist-at-rewrite)))
               (expect (< (.length (File. path)) before))
               (expect (= [[::janitor/ran :vacuumed]]
                          (mapv (juxt :id (comp :reason :result :data)) lines))))
             (finally (cleanup! path)))))
  (it "truncates the WAL after a rewrite even when the store does not need a vacuum"
      (let [[path ^javax.sql.DataSource ds]
            (wal-store)

            wal
            (File. (str path "-wal"))]

        (try (with-open [keeper (.getConnection ds)]
               (with-open [st (.createStatement keeper)]
                 (.executeUpdate st "CREATE TABLE bulk (id INTEGER PRIMARY KEY, payload BLOB)"))
               (fill! ds 200)
               (expect (pos? (.length wal)))
               (expect (empty? (janitor-lines path
                                              #(maintenance/vacuum-async! ds
                                                                          path
                                                                          {:delay-ms 0
                                                                           :before (fn []
                                                                                     (fill! ds 1)
                                                                                     true)}))))
               (expect (zero? (.length wal))))
             (finally (cleanup! path)))))
  ;; Each test store and each short command closes its store before the delay ends.
  (it "leaves a store that closed in the delay alone, without a line"
      (let [[path ds]
            (temp-store)

            rewrites
            (atom 0)]

        (try (fill! ds 2000)
             (exec! ds "DELETE FROM bulk")
             (let [before
                   (.length (File. path))

                   lines
                   (janitor-lines path
                                  #(maintenance/vacuum-async! ds
                                                              path
                                                              {:delay-ms 0
                                                               :min-free-bytes 1024
                                                               :open? (constantly false)
                                                               :before (fn []
                                                                         (swap! rewrites inc))}))]

               (expect (zero? (long @rewrites)))
               (expect (= before (.length (File. path))))
               (expect (empty? lines)))
             (finally (cleanup! path)))))
  (it "writes no line for a store with nothing to reclaim, and a warning for a failed vacuum"
      (let [[path ds] (temp-store)]
        (try (fill! ds 200)
             (expect (empty? (janitor-lines path
                                            #(maintenance/vacuum-async! ds path {:delay-ms 0}))))
             (let [broken (doto (org.sqlite.SQLiteDataSource.)
                            (.setUrl (str "jdbc:sqlite:" path File/separator "no-such-store")))]
               (expect (= [[::janitor/failed :warn]]
                          (mapv (juxt :id :level)
                                (janitor-lines
                                  path
                                  #(maintenance/vacuum-async! broken path {:delay-ms 0}))))))
             (finally (cleanup! path))))))
