(ns com.blockether.vis.internal.persistance.sqlite.maintenance
  "Space reclamation for a file-backed SQLite store.

   SQLite does not give the file back by itself. Pages that a delete or a `DROP
   COLUMN` frees go on the FREELIST, and later writes use them again. A one-off
   bulk reclaim (retiring a column across a whole transcript) can leave hundreds
   of megabytes in the file.

   `maybe-vacuum!` gives that space back when the freelist is at least
   `vacuum-min-free-bytes`. A compact store is never touched, so the usual
   answer is four PRAGMAs and no I/O.

   Writers never wait for the whole file. A store with `auto_vacuum=INCREMENTAL`
   frees its freelist with `PRAGMA incremental_vacuum(N)` in chunks of
   `vacuum-chunk-pages`. Each chunk is its own short write transaction, and a
   pause after each chunk lets waiting writers take the lock. Mozilla storage
   does the same (`mozStorageConnection.cpp` AsyncVacuum, application-services
   `maintenance.rs`). `use-incremental-vacuum!` gives each new store this mode.
   Two processes that reclaim one store at the same time only take turns.

   A store with `auto_vacuum=NONE` (made by an older Vis) has no page map for
   this. One full VACUUM with `auto_vacuum=INCREMENTAL` converts it, and it is
   its last full VACUUM. That rewrite holds the write lock for the whole file
   (measured: 15 s for 2.4 GB), so it runs only when the store is at most
   `vacuum-max-file-bytes`, inside the 30 s busy timeout of `core`. A larger old
   store keeps its freelist, and SQLite uses those pages again: on a 17 GB store
   the rewrite held the lock for many minutes and each turn write failed or
   waited. The `vis.db.vacuum` file beside the store is the cross-process lock
   of this conversion, an exclusive `FileLock`. The next reclaim of the
   converted store deletes it.

   The work runs off the open path: `vacuum-async!` gives it to
   `internal.janitor`, the one maintenance worker of a process. One pass frees
   pages for at most `vacuum-slice-ms` and then queues the next pass, so a long
   freelist never holds the other chores for more than one slice. Each chunk and
   each rewrite is a transaction, so a process that stops first leaves a
   consistent store, and the rest stays due."
  (:require [com.blockether.vis.internal.janitor :as janitor]
            [com.blockether.vis.internal.util :as util]
            [taoensso.telemere :as tel])
  (:import (java.io File RandomAccessFile)
           (java.nio.channels FileLock OverlappingFileLockException)
           (java.sql Statement)
           (javax.sql DataSource)))

(def ^:private ^:const vacuum-min-free-bytes
  "Freelist floor, in bytes (64 MiB). Below it SQLite uses the pages again soon."
  (* 64 1024 1024))

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

(def ^:private ^:const vacuum-slice-ms
  "The longest time that one pass frees pages before it gives the janitor back."
  2000)

(def ^:private ^:const vacuum-start-delay-ms
  "Settling time after the open, so that the reclaim does not compete with the
   start of the process."
  5000)

(defn- marker-file
  "The `vis.db.vacuum` conversion lock beside the store at `db-file`."
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
   store closes. Answer `{:freelist-count :is-more}`: `:is-more` is true when the
   slice ended at `deadline` and pages are left."
  [^DataSource ds open? ^long deadline]
  (loop [free (long (:freelist-count (free-space ds)))]
    (if-not (and (pos? free) (or (nil? open?) (open?)))
      {:freelist-count free :is-more false}
      (do (with-autocommit!
            ds
            (fn [^Statement st]
              ;; `execute` steps this pragma once and frees one page.
              (.executeUpdate st (str "PRAGMA incremental_vacuum(" vacuum-chunk-pages ")"))))
          (let [left (long (:freelist-count (free-space ds)))]
            (cond (not (< 0 left free)) {:freelist-count left :is-more false}
                  (<= deadline (long (util/now-ms))) {:freelist-count left :is-more true}
                  :else (do (Thread/sleep (long vacuum-chunk-pause-ms)) (recur left))))))))

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
   already converting the same store."
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

(defn- measured!
  "Run the reclaim `work` and truncate the WAL after it, even when it fails. Add
   the file size before and after and the duration to the report of `work`."
  [^DataSource ds ^String db-file space open? work]
  (let [before
        (.length (File. db-file))

        started
        (util/now-ms)

        result
        (try (work) (finally (checkpoint-or-retry! ds db-file open? checkpoint-retry-delays-ms)))]

    (merge space
           {:is-vacuumed true
            :reason :vacuumed
            :before-bytes before
            :after-bytes (.length (File. db-file))
            :duration-ms (- (util/now-ms) started)}
           result)))

(defn- convert!
  "Convert an `auto_vacuum=NONE` store with its last full VACUUM, under the
   cross-process lock. Answer `:locked` when another process converts it."
  [^DataSource ds ^String db-file space open?]
  (or (with-vacuum-lock! (marker-file db-file)
                         #(measured! ds
                                     db-file
                                     space
                                     open?
                                     (fn []
                                       (vacuum! ds)
                                       nil)))
      (merge space {:is-vacuumed false :reason :locked})))

(defn maybe-vacuum!
  "Give back the freelist of the store at `db-file` when it is at least
   `vacuum-min-free-bytes`. Returns a report — `:is-vacuumed` with a `:reason` of
   `:vacuumed`, `:compact` (nothing worth reclaiming), `:too-large` (an
   `auto_vacuum=NONE` store whose rewrite would block writers too long),
   `:low-disk`, `:locked` (another process converts it) or `:failed`. Does not
   throw. An incremental store frees pages for at most `vacuum-slice-ms`, and
   `:is-more` is true when pages are left for the next pass. A busy checkpoint
   retries on the janitor without a new reclaim. The optional `:open?` predicate
   stops the work and these retries when the store closes.

   Test options: `:min-free-bytes`, `:max-file-bytes`, `:slice-ms`, and
   `:usable-space` (takes the store path)."
  ([^DataSource ds ^String db-file] (maybe-vacuum! ds db-file nil))
  ([^DataSource ds ^String db-file
    {:keys [min-free-bytes max-file-bytes slice-ms open?] disk-space :usable-space}]
   (try
     (let [space
           (free-space ds)

           file-bytes
           (max (long (:file-bytes space)) (.length (File. db-file)))]

       (cond (< (long (:free-bytes space)) (long (or min-free-bytes vacuum-min-free-bytes)))
             (merge space {:is-vacuumed false :reason :compact})
             (= 2 (long (:auto-vacuum space)))
             (do (.delete (marker-file db-file))
                 (measured! ds
                            db-file
                            space
                            open?
                            #(incremental-vacuum! ds
                                                  open?
                                                  (+ (long (util/now-ms))
                                                     (long (or slice-ms vacuum-slice-ms))))))
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
                       (convert! ds db-file space open?)))))
     (catch Throwable t {:is-vacuumed false :reason :failed :error (ex-message t)}))))

(defn- vacuum-pass!
  "One janitor pass of `maybe-vacuum!`. While pages are left, queue the next
   pass at once under the same id, so other chores run between two slices."
  [ds db-file {:keys [open?] :as opts}]
  (when (or (nil? open?) (open?))
    (let [{:keys [reason is-more] :as report} (maybe-vacuum! ds db-file opts)]
      (when is-more
        (janitor/submit! {:id [:sqlite-vacuum db-file] :run #(vacuum-pass! ds db-file opts)}))
      (case reason
        :compact
        nil

        :failed
        (throw (ex-info "The SQLite vacuum failed" report))

        report))))

(defn vacuum-async!
  "Give `maybe-vacuum!` for the store at `db-file` to the janitor, which runs it
   after `:delay-ms`, so the reclaim is not what a starting process waits on. A
   `:before` function runs first in the same task, so a data rewrite frees its
   pages before the reclaim checks them. If it returns true, truncate the WAL.
   `:open?` answers whether the store is still open: a store that closed in the
   delay is due at its next open. Called once per store per process, from the
   pool that just opened it. A second call for one store replaces a task that
   still waits.

   The janitor writes the report of each pass that did work or that another
   process holds. A store with nothing to reclaim leaves no line, and a failed
   pass is a warning."
  ([ds db-file] (vacuum-async! ds db-file nil))
  ([ds db-file {:keys [delay-ms before open?] :as opts}]
   (janitor/submit! {:id [:sqlite-vacuum db-file]
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
                              (vacuum-pass! ds db-file opts)))})))
