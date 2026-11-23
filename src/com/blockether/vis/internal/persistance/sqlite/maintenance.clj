(ns com.blockether.vis.internal.persistance.sqlite.maintenance
  "Space reclamation for a file-backed SQLite store.

   SQLite never hands the file back on its own: `auto_vacuum` is off, so pages a
   delete or a `DROP COLUMN` frees go on the FREELIST and are only reused by
   later writes. That is the right default — reuse costs nothing — but a one-off
   bulk reclaim (retiring a column across a whole transcript) leaves hundreds of
   megabytes the file keeps until someone runs `VACUUM`.

   `maybe-vacuum!` is that someone, on the same fortnight window as
   `foundation.housekeeping` retention: at most once per `vacuum-interval-days`
   per store, and only when the freelist is worth the rewrite — at least
   `vacuum-min-free-bytes` AND `vacuum-min-free-fraction` of the file. A compact
   store is never rewritten, so the usual answer is three PRAGMAs and no I/O.

   VACUUM takes SQLite's exclusive lock and rewrites the whole file (measured:
   15 s and 2.4 GB -> 2.0 GB on a real store), so it runs OFF the open path —
   `vacuum-async!` gives it to `internal.janitor`, the one maintenance worker of
   a process, after a settling delay, never inside a transaction. Readers and
   writers in this process or another one block for its duration and then
   continue, which the 30 s busy timeout and the write-retry ladder in `core`
   absorb. A process that exits first, or a `db-close!` that aborts the lease
   mid-rewrite, simply leaves the store due at the next start: the rewrite is
   transactional, so an interrupted VACUUM rolls back rather than damaging
   anything.

   The `vis.db.vacuum` marker beside the store is BOTH the clock (its mtime is
   the last successful vacuum) and the cross-process mutex (an exclusive
   `FileLock` held for the rewrite). It is created only when a vacuum is
   actually attempted and deleted again when one fails, so a store that never
   needed reclaiming carries no marker and stays due."
  (:require [com.blockether.vis.internal.janitor :as janitor]
            [com.blockether.vis.internal.util :as util]
            [taoensso.telemere :as tel])
  (:import (java.io File RandomAccessFile)
           (java.nio.channels FileLock OverlappingFileLockException)
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

(def ^:private ^:const vacuum-start-delay-ms
  "Settling time before the sweep. Startup is the busiest moment of a store's
   day and the one VACUUM would block; a minute later it is quiet."
  60000)

(defn- marker-file
  "The `vis.db.vacuum` clock/mutex beside the store at `db-file`."
  ^File [^String db-file]
  (File. (str db-file ".vacuum")))

(defn free-space
  "`{:page-size :page-count :freelist-count :file-bytes :free-bytes}` as SQLite
   itself reports it — `:file-bytes` is the live file, `:free-bytes` what a
   VACUUM would give back."
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

      {:page-size page-size
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

(defn- vacuum!
  "Rewrite the store compactly. Outside any transaction — SQLite refuses VACUUM
   inside one — so autocommit is forced on for the statement and restored after."
  [^DataSource ds]
  (with-open [conn (.getConnection ds)]
    (let [auto (.getAutoCommit conn)]
      (try (.setAutoCommit conn true)
           (with-open [st (.createStatement conn)]
             (.execute st "VACUUM"))
           (finally (try (.setAutoCommit conn auto) (catch Throwable _ nil)))))))

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
  "One rewrite under the held lock, stamping `marker` on success. Reports rather
   than throws: a busy store is a later retry, not a failed open."
  [^DataSource ds ^String db-file ^File marker space open?]
  (let [before
        (.length (File. db-file))

        started
        (util/now-ms)]

    (try (try (vacuum! ds)
              (finally (checkpoint-or-retry! ds db-file open? checkpoint-retry-delays-ms)))
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
   (nothing worth reclaiming), `:low-disk`, `:locked` (another process is doing
   it) or `:failed`. Does not throw. A low-disk skip leaves the marker alone:
   only the next store open checks again, after the janitor delay. A busy
   checkpoint retries on the janitor without a new vacuum. The optional `:open?`
   predicate stops these retries when the store closes.

   Test options: `:now-ms`, `:interval-days`, `:min-free-bytes`,
   `:min-free-fraction`, and `:usable-space` (takes the store path)."
  ([^DataSource ds ^String db-file] (maybe-vacuum! ds db-file nil))
  ([^DataSource ds ^String db-file
    {:keys [now-ms interval-days min-free-bytes min-free-fraction open?] disk-space :usable-space}]
   (try
     (let [marker
           (marker-file db-file)

           now
           (long (or now-ms (util/now-ms)))

           window
           (long (or interval-days vacuum-interval-days))]

       (if-not (time-due? marker now window)
         {:is-vacuumed false :reason :recent}
         (let [space (free-space ds)]
           (if-not (size-due? space
                              (long (or min-free-bytes vacuum-min-free-bytes))
                              (double (or min-free-fraction vacuum-min-free-fraction)))
             (merge space {:is-vacuumed false :reason :compact})
             (let [usable-bytes (long ((or disk-space usable-space) db-file))
                   ;; VACUUM writes a temp database and a WAL. Both can be on this volume.
                   ;; https://www.sqlite.org/lang_vacuum.html#how_vacuum_works
                   required-bytes (* 2 (max (long (:file-bytes space)) (.length (File. db-file))))]

               (if (< usable-bytes required-bytes)
                 (assoc space
                   :is-vacuumed false
                   :reason :low-disk
                   :usable-bytes usable-bytes
                   :required-bytes required-bytes)
                 (let [report (with-vacuum-lock! marker #(run-vacuum! ds db-file marker space open?))]
                   (when (= :failed (:reason report))
                     (try (.delete marker) (catch Throwable _ nil)))
                   (or report (merge space {:is-vacuumed false :reason :locked})))))))))
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
