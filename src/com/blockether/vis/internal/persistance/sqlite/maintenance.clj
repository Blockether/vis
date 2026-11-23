(ns com.blockether.vis.internal.persistance.sqlite.maintenance
  "Space reclamation for a file-backed SQLite store.

   SQLite does not give the file back by itself. Pages that a delete or a `DROP
   COLUMN` frees go on the FREELIST, and later writes use them again. A one-off
   bulk reclaim (retiring a column across a whole transcript) can leave hundreds
   of megabytes in the file.

   `maybe-vacuum!` gives that space back, on the same fortnight window as
   `foundation.housekeeping` retention: at most once per `vacuum-interval-days`
   per store, and only when the freelist is at least `vacuum-min-free-bytes` AND
   `vacuum-min-free-fraction` of the file. A compact store is never touched, so
   the usual answer is four PRAGMAs and no I/O.

   Writers never wait for the whole file. A store with `auto_vacuum=INCREMENTAL`
   frees its freelist with `PRAGMA incremental_vacuum(N)` in chunks of
   `vacuum-chunk-pages`. Each chunk is its own short write transaction, and a
   pause after each chunk lets waiting writers take the lock. Mozilla storage
   does the same (`mozStorageConnection.cpp` AsyncVacuum, application-services
   `maintenance.rs`). `use-incremental-vacuum!` gives each new store this mode.

   A store with `auto_vacuum=NONE` (made by an older Vis) has no page map for
   this. One full VACUUM with `auto_vacuum=INCREMENTAL` converts it, and it is
   its last full VACUUM. That rewrite holds the write lock for the whole file
   (measured: 15 s for 2.4 GB), so it runs only when the store is at most
   `vacuum-max-file-bytes`, inside the 30 s busy timeout of `core`. A larger old
   store keeps its freelist, and SQLite uses those pages again: on a 17 GB store
   the rewrite held the lock for many minutes and each turn write failed or
   waited.

   The work runs off the open path: `vacuum-async!` gives it to
   `internal.janitor`, the one maintenance worker of a process, after a settling
   delay. Each chunk and each rewrite is a transaction, so a process that stops
   first leaves a consistent store, and the rest stays due.

   The `vis.db.vacuum` marker beside the store is BOTH the clock (its mtime is
   the last successful vacuum) and the cross-process mutex (an exclusive
   `FileLock` held for the work). It is created only when a vacuum is
   actually attempted and deleted again when one fails, so a store that never
   needed reclaiming carries no marker and stays due."
  (:require [com.blockether.vis.internal.janitor :as janitor]
            [com.blockether.vis.internal.util :as util]
            [taoensso.telemere :as tel])
  (:import (java.io File RandomAccessFile)
           (java.nio.channels FileLock OverlappingFileLockException)
           (java.sql Statement)
           (javax.sql DataSource)))

(def ^:private ^:const day-ms 86400000)

(def vacuum-interval-days
  "Days between two reclaims of one store. The fortnight
   `foundation.housekeeping/default-retention-days` already uses for every other
   self-bounding artifact: long enough that no ordinary week ever pays the
   rewrite, short enough that space freed by a migration comes back while the
   operator still remembers the upgrade."
  14)

(def ^:private ^:const vacuum-min-free-bytes
  "Freelist floor, in bytes (64 MiB). Below it the rewrite costs more than the
   disk it returns."
  (* 64 1024 1024))

(def ^:private ^:const vacuum-min-free-fraction
  "Share of the file the freelist must reach on top of the floor. A big store
   with 2% waste is healthy churn, not something to rewrite."
  0.10)

;; Measured 15 s for 2.4 GB, so 2 GiB stays in the 30 s busy timeout of `core`.
(def ^:private ^:const vacuum-max-file-bytes
  "The largest `auto_vacuum=NONE` store, in bytes (2 GiB), that the one full VACUUM
   converts while writers wait."
  (* 2 1024 1024 1024))

(def ^:private ^:const vacuum-chunk-pages
  "Pages that one `incremental_vacuum` transaction frees (4 MiB at 4 KiB pages).
   A writer waits for one chunk at most."
  1024)

(def ^:private ^:const vacuum-chunk-pause-ms
  "Pause after each chunk. The SQLite busy handler of a waiting writer sleeps at
   most 100 ms between tries, so it gets the lock in this pause."
  100)

(def ^:private ^:const vacuum-start-delay-ms
  "Settling time before the sweep. Startup is the busiest moment of a store's
   day and the one VACUUM would block; a minute later it is quiet."
  60000)

(defn- marker-file
  "The `vis.db.vacuum` clock/mutex beside the store at `db-file`."
  ^File [^String db-file]
  (File. (str db-file ".vacuum")))

(defn free-space
  "`{:auto-vacuum :page-size :page-count :freelist-count :file-bytes :free-bytes}`
   as SQLite itself reports it — `:auto-vacuum` is 0 (NONE) or 2 (INCREMENTAL),
   `:file-bytes` is the live file, `:free-bytes` what a vacuum would give back."
  [^DataSource ds]
  (with-open [conn
              (.getConnection ds)

              st
              (.createStatement conn)]

    (let [pragma
          (fn [^String name]
            (with-open [rs (.executeQuery st (str "PRAGMA " name))]
              (if (.next rs) (.getLong rs 1) 0)))

          page-size
          (long (pragma "page_size"))

          page-count
          (long (pragma "page_count"))

          freelist
          (long (pragma "freelist_count"))]

      {:auto-vacuum (long (pragma "auto_vacuum"))
       :page-size page-size
       :page-count page-count
       :freelist-count freelist
       :file-bytes (* page-size page-count)
       :free-bytes (* page-size freelist)})))

(defn- usable-space
  "Bytes that this process can use on the store volume."
  [^String db-file]
  (.getUsableSpace (.getParentFile (.getCanonicalFile (File. db-file)))))

(def ^:private checkpoint-retry-delays-ms [30000 60000 120000])

(defn- checkpoint!
  "Give the WAL space back. Return false if SQLite cannot truncate it."
  [^DataSource ds db-file]
  (try (with-open [conn
                   (.getConnection ds)

                   st
                   (.createStatement conn)

                   rs
                   (.executeQuery st "PRAGMA wal_checkpoint(TRUNCATE)")]

         (if (and (.next rs) (zero? (.getLong rs 1)))
           true
           (do (tel/log! {:level :warn :id ::checkpoint-busy :data {:db-file db-file}}) false)))
       (catch Exception e
         (tel/log! {:level :warn :id ::checkpoint-failed :data {:db-file db-file} :error e})
         false)))

(defn- checkpoint-or-retry!
  "Retry a checkpoint without repeating the VACUUM or data rewrite."
  [ds db-file open? [delay-ms & later]]
  (when (and (not (checkpoint! ds db-file)) delay-ms)
    (janitor/submit! {:id [:sqlite-checkpoint db-file]
                      :delay-ms delay-ms
                      :run (fn []
                             (when (or (nil? open?) (open?))
                               (checkpoint-or-retry! ds db-file open? later)))}))
  nil)

(defn- time-due?
  "True when `marker` has never recorded a vacuum, or the last one is older than
   `interval-days`."
  [^File marker ^long now-ms ^long interval-days]
  (or (not (.isFile marker))
      (>= (- now-ms (.lastModified marker)) (* interval-days (long day-ms)))))

(defn- size-due?
  "True when the freelist clears BOTH the byte floor and the share of the file."
  [{:keys [file-bytes free-bytes]} ^long min-bytes ^double min-fraction]
  (let [free (long free-bytes)]
    (and (>= free min-bytes) (>= (double free) (* min-fraction (double (long file-bytes)))))))

(defn- with-autocommit!
  "Call `f` with a statement of one connection in autocommit mode, and restore the
   mode after. SQLite refuses VACUUM inside a transaction."
  [^DataSource ds f]
  (with-open [conn (.getConnection ds)]
    (let [auto (.getAutoCommit conn)]
      (try (.setAutoCommit conn true)
           (with-open [st (.createStatement conn)]
             (f st))
           (finally (try (.setAutoCommit conn auto) (catch Throwable _ nil)))))))

(defn- rewrite-incremental!
  "Rewrite the store compactly with `auto_vacuum=INCREMENTAL`. In WAL mode only
   a VACUUM changes this mode."
  [^Statement st]
  (.execute st "PRAGMA auto_vacuum = INCREMENTAL")
  (.execute st "VACUUM"))

(defn- vacuum!
  "Rewrite the whole store and convert it to `auto_vacuum=INCREMENTAL`. Writers
   wait for the whole rewrite."
  [^DataSource ds]
  (with-autocommit! ds rewrite-incremental!))

(defn- incremental-vacuum!
  "Free the freelist of an `auto_vacuum=INCREMENTAL` store in chunks of
   `vacuum-chunk-pages`, each in its own write transaction, with a pause after
   each chunk. Stop when the freelist is empty, a chunk frees nothing, or the
   store closes."
  [^DataSource ds open?]
  (loop [free (long (:freelist-count (free-space ds)))]
    (when (and (pos? free) (or (nil? open?) (open?)))
      (with-autocommit!
        ds
        (fn [^Statement st]
          ;; `execute` steps this pragma once and frees one page.
          (.executeUpdate st (str "PRAGMA incremental_vacuum(" vacuum-chunk-pages ")"))))
      (let [left (long (:freelist-count (free-space ds)))]
        (when (< 0 left free) (Thread/sleep (long vacuum-chunk-pause-ms)) (recur left))))))

(defn use-incremental-vacuum!
  "Give a new store `auto_vacuum=INCREMENTAL` before its first table, so that
   `maybe-vacuum!` never needs a full VACUUM for it. A new connection already
   writes the file header for WAL mode, so the mode needs a VACUUM, which is
   instant on an empty store. A store with tables stays as it is."
  [^DataSource ds]
  (with-autocommit! ds
                    (fn [^Statement st]
                      (let [value (fn [^String sql]
                                    (with-open [rs (.executeQuery st sql)]
                                      (if (.next rs) (.getLong rs 1) 0)))]
                        (when (and (zero? (long (value "SELECT count(*) FROM sqlite_schema")))
                                   (not= 2 (long (value "PRAGMA auto_vacuum"))))
                          (rewrite-incremental! st))))))

(defn- with-vacuum-lock!
  "Run `f` holding the exclusive cross-process lock on `marker`, which this call
   CREATES. Answers nil when another process (or another thread of this one) is
   already reclaiming the same store."
  [^File marker f]
  (let [raf
        (RandomAccessFile. marker "rw")

        channel
        (.getChannel raf)

        ^FileLock lock
        (try (.tryLock channel) (catch OverlappingFileLockException _ nil) (catch Throwable _ nil))]

    (try (when lock (f))
         (finally (try (when lock (.release lock)) (catch Throwable _ nil))
                  (try (.close channel) (catch Throwable _ nil))
                  (try (.close raf) (catch Throwable _ nil))))))

(defn- run-vacuum!
  "One reclaim `work` under the held lock, stamping `marker` on success. Reports
   rather than throws: a busy store is a later retry, not a failed open."
  [^DataSource ds ^String db-file ^File marker space open? work]
  (let [before
        (.length (File. db-file))

        started
        (util/now-ms)]

    (try (try (work) (finally (checkpoint-or-retry! ds db-file open? checkpoint-retry-delays-ms)))
         (.setLastModified marker (util/now-ms))
         (merge space
                {:is-vacuumed true
                 :reason :vacuumed
                 :before-bytes before
                 :after-bytes (.length (File. db-file))
                 :duration-ms (- (util/now-ms) started)})
         (catch Throwable t
           (merge space {:is-vacuumed false :reason :failed :error (ex-message t)})))))

(defn maybe-vacuum!
  "Reclaim the freelist of the store at `db-file` when the fortnight has passed
   AND the freelist is worth a rewrite. Returns a report — `:is-vacuumed` with a
   `:reason` of `:vacuumed`, `:recent` (marker inside the window), `:compact`
   (nothing worth reclaiming), `:too-large` (an `auto_vacuum=NONE` store whose
   rewrite would block writers too long), `:low-disk`, `:locked` (another process is doing
   it) or `:failed`. Does not throw. A low-disk skip leaves the marker alone:
   only the next store open checks again, after the janitor delay. A busy
   checkpoint retries on the janitor without a new vacuum. The optional `:open?`
   predicate stops these retries when the store closes.

   Test options: `:now-ms`, `:interval-days`, `:min-free-bytes`,
   `:min-free-fraction`, `:max-file-bytes`, and `:usable-space` (takes the store path)."
  ([^DataSource ds ^String db-file] (maybe-vacuum! ds db-file nil))
  ([^DataSource ds ^String db-file
    {:keys [now-ms interval-days min-free-bytes min-free-fraction max-file-bytes open?]
     disk-space :usable-space}]
   (try
     (let [marker
           (marker-file db-file)

           now
           (long (or now-ms (util/now-ms)))

           window
           (long (or interval-days vacuum-interval-days))]

       (if-not (time-due? marker now window)
         {:is-vacuumed false :reason :recent}
         (let [space
               (free-space ds)

               file-bytes
               (max (long (:file-bytes space)) (.length (File. db-file)))

               reclaim!
               (fn [work]
                 (let [report (with-vacuum-lock! marker
                                                 #(run-vacuum! ds db-file marker space open? work))]
                   (when (= :failed (:reason report))
                     (try (.delete marker) (catch Throwable _ nil)))
                   (or report (merge space {:is-vacuumed false :reason :locked}))))]

           (cond (not (size-due? space
                                 (long (or min-free-bytes vacuum-min-free-bytes))
                                 (double (or min-free-fraction vacuum-min-free-fraction))))
                 (merge space {:is-vacuumed false :reason :compact})
                 (= 2 (long (:auto-vacuum space))) (reclaim! #(incremental-vacuum! ds open?))
                 (< (long (or max-file-bytes vacuum-max-file-bytes)) file-bytes)
                 (merge space {:is-vacuumed false :reason :too-large})
                 :else (let [usable-bytes
                             (long ((or disk-space usable-space) db-file))

                             ;; VACUUM writes a temp database and a WAL. Both can be on this volume.
                             ;; https://www.sqlite.org/lang_vacuum.html#how_vacuum_works
                             required-bytes
                             (* 2 file-bytes)]

                         (if (< usable-bytes required-bytes)
                           (assoc space
                             :is-vacuumed false
                             :reason :low-disk
                             :usable-bytes usable-bytes
                             :required-bytes required-bytes)
                           (reclaim! #(vacuum! ds))))))))
     (catch Throwable t {:is-vacuumed false :reason :failed :error (ex-message t)}))))

(defn vacuum-async!
  "Give `maybe-vacuum!` for the store at `db-file` to the janitor, which runs it
   after `:delay-ms`, so the rewrite is not what a starting process waits on. A
   `:before` function runs first in the same task, so a data rewrite frees its
   pages before the reclaim checks them. If it returns true, truncate the WAL.
   `:open?` answers whether the store is
   still open: a store that closed in the delay is due at its next open. Called
   once per store per process, from the pool that just opened it. A second call
   for one store replaces a task that still waits.

   The janitor writes the report of a vacuum that ran or that another process
   holds. A store with nothing to reclaim leaves no line, and a failed vacuum is a
   warning."
  ([ds db-file] (vacuum-async! ds db-file nil))
  ([ds db-file {:keys [delay-ms before open?] :as opts}]
   (janitor/submit!
     {:id [:sqlite-vacuum db-file]
      :delay-ms (long (or delay-ms vacuum-start-delay-ms))
      :run (fn []
             (when (or (nil? open?) (open?))
               (when (and before
                          (try (true? (before))
                               (catch Exception e
                                 (tel/log! {:level :warn
                                            :id ::store-rewrite-failed
                                            :data {:db-file db-file}
                                            :error e})
                                 ;; An earlier rewrite can commit before a later one fails.
                                 true)))
                 (checkpoint-or-retry! ds db-file open? checkpoint-retry-delays-ms))
               (let [{:keys [reason] :as report} (maybe-vacuum! ds db-file opts)]
                 (case reason
                   (:recent :compact)
                   nil

                   :failed
                   (throw (ex-info "The SQLite vacuum failed" report))

                   report))))})))
