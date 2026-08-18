(ns ^{:clj-kondo/config '{:linters {:unused-public-var {:level :off}}}}
    com.blockether.vis.ext.persistance-sqlite.core
  "SQLite store - V1 schema implementation.

   Every public defn in this file is dispatched dynamically by
   `vis-sdk.core/defdelegate` via `ns-resolve`; clj-kondo never sees
   the call sites. The ns-level config above silences
   `:unused-public-var` for the whole file. The actual call surface
   is verified through the storage facade tests.

   Tables (V1__schema.sql):
     session_soul, session_state,
     session_turn_soul, session_turn_state,
     session_turn_iteration, llm_routing_event,
     extension_aggregate,
     log

   Connection lifecycle:
     (db-open! db-spec)   -> {:datasource ds :path ...}
     (db-close! store)    -> idempotent dispose"
  (:require [charred.api :as json]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [com.blockether.vis.ext.persistance-sqlite.maintenance :as maintenance]
            [com.blockether.vis.ext.persistance-sqlite.migration :as migration]
            [com.blockether.vis.internal.attachments :as attachments]
            [com.blockether.vis.internal.paths :as paths]
            [com.blockether.vis.core :as vis]
            [honey.sql :as sql]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]
            [taoensso.nippy :as nippy])
  (:import (com.zaxxer.hikari HikariConfig HikariDataSource HikariPoolMXBean)
           (java.io File RandomAccessFile)
           (java.nio.channels FileLock)
           (java.nio.file Files LinkOption Paths)
           (java.sql SQLException)
           (java.util.concurrent.atomic AtomicLong)
           (javax.sql DataSource)
           (org.sqlite SQLiteConfig
                       SQLiteConfig$JournalMode
                       SQLiteConfig$SynchronousMode
                       SQLiteConfig$TransactionMode
                       SQLiteDataSource)))

;; Helpers

(def ds vis/ds)

(def now-ms vis/now-ms)

(def ->id vis/->id)

(def ->uuid vis/->uuid)

(def ->ref vis/->ref)

(def ->kw vis/->kw)

(def ->kw-back vis/->kw-back)

(def ->date vis/->date)

(def new-uuid random-uuid)

(defn new-id [] (->id (new-uuid)))

(defn query!
  "Run a HoneySQL map and return rows with unqualified lower-case keys."
  [db-info q]
  (jdbc/execute! (ds db-info) (sql/format q) {:builder-fn rs/as-unqualified-lower-maps}))

(defn query-one! [db-info q] (first (query! db-info q)))

(defn execute! [db-info q] (jdbc/execute! (ds db-info) (sql/format q)))

(def normalize-status vis/normalize-status)

(def ->json vis/->json)

(def <-json vis/<-json)

(def ^:private blob-freeze-opts
  "Nippy options for every BLOB column. Zstd — `io.airlift/aircompressor`, pure
   Java, already on Nippy's own classpath, no JNI — instead of Nippy's default
   (LZ4 above 2 KiB, nothing below): on 250 real iterations' tool-call forms it
   stores 315 KB where LZ4 stores 426 KB and the default stores 630 KB, for tens
   of milliseconds more per 250 rows. Only `->blob` takes options. Nippy stamps
   the compressor id into the header, so `thaw` with NO options reads every blob
   written before this one existed; passing `:compressor` to `thaw` is exactly
   what would break them."
  {:compressor nippy/zstd-compressor})

(defn- ->blob
  "Serialize a Clojure value to a Nippy byte array for BLOB columns."
  ^bytes [v]
  (nippy/freeze v blob-freeze-opts))

(defn- <-blob
  "Deserialize a Nippy byte array from a BLOB column back to a Clojure value."
  [^bytes bs]
  (when bs (nippy/thaw bs)))

;; Error translation + bootstrap error normalization

(defn- causal-chain
  "Walk `(.getCause e)` until a fixed point or cycle is hit. Returns the
   chain in causal order (innermost first), bounded so a self-referential
   cause graph can't loop forever."
  [^Throwable e]
  (loop
    [acc
     []

     cur
     e

     seen
     #{}]

    (cond (nil? cur) (reverse acc)
          (contains? seen cur) (reverse acc)
          (>= (count acc) 16) (reverse acc)
          :else (recur (conj acc cur) (.getCause cur) (conj seen cur)))))

(def ^:private migration-checksum-mismatch-user-message
  (str "Database schema mismatch: the canonical V1 history could not be auto-repaired. "
       "Flyway repair normally realigns checksum and consolidated-migration metadata while "
       "preserving every application row; no database content was deleted."))

(defn- migration-checksum-mismatch?
  "True when any throwable in the causal chain looks like Flyway's
   migration checksum validation failure."
  [^Throwable e]
  (boolean (some (fn [^Throwable t]
                   (let [^String m (or (ex-message t) "")]
                     (or (.contains m "Migration checksum mismatch")
                         (.contains m "Migrations have failed validation"))))
                 (causal-chain e))))

(defn- maybe-wrap-db-open-error
  "Normalize known SQLite bootstrap failures into caller-facing user
   errors. Unknown failures pass through unchanged so fatal startup
   behavior remains unchanged."
  [^Throwable e]
  (if (migration-checksum-mismatch? e)
    (ex-info migration-checksum-mismatch-user-message
             {:vis/user-error true :type :vis/db-migration-checksum-mismatch}
             e)
    e))

(defn- sqlite-cantopen-message?
  "True when any link in the cause chain looks like a SQLite open failure."
  [^Throwable e]
  (boolean (some (fn [^Throwable t]
                   (let [^String m (or (ex-message t) "")]
                     (or (.contains m "[SQLITE_CANTOPEN]")
                         (.contains m "unable to open database file")
                         (.contains m "Unable to open the database file"))))
                 (causal-chain e))))

(defn db-error->user-message
  "Translate SQLite persistence exceptions into actionable user text.
   Return nil for non-SQLite errors so the facade can try other adapters
   or fall back to `(ex-message e)`."
  [^Throwable e]
  (when (sqlite-cantopen-message? e)
    (let
      [home
       (System/getProperty "user.home")

       dbpath
       (str home "/.vis/vis.mdb/vis.db")

       dbdir
       (str home "/.vis/vis.mdb")

       dirf
       (File. dbdir)

       filef
       (File. dbpath)]

      (str
        "Vis database is unavailable. "
        "Expected file: " dbpath
        ". "
        (cond
          (not (.exists filef))
          "The file is missing - likely deleted while Vis was running. Restart Vis to recreate it."
          (not (.canWrite dirf)) (str "The directory " dbdir " is not writable by this process.")
          :else "The handle was lost mid-session. Restart Vis to reconnect.")))))

;; Schema install
;;
;; The Flyway runner and canonical V*__schema.sql files live in this
;; package under
;; `resources/db/sqlite/migration/`. We only point Flyway at that
;; classpath location. No schema DDL or repair DDL lives in Clojure.
;; The runner repairs drifted checksums and history left by V2+ files that were
;; consolidated back into the single canonical V1, and additively tops up columns
;; a store created by an older canonical V1 is missing (the DDL text still comes
;; from V1__schema.sql); it never recreates the DB.

(def ^:private MIGRATIONS "classpath:db/sqlite/migration")

(defn- install-schema! [^DataSource ds] (migration/migrate! ds MIGRATIONS))

;; Connection management
;;
;; xerial's `SQLiteConnectionPoolDataSource` is a JNDI-shaped façade
;; that opens a NEW physical SQLite connection on every
;; `getConnection()` - it does NOT pool. We replace it with HikariCP:
;; an underlying `SQLiteDataSource` configured via `SQLiteConfig`
;; (WAL with a bounded `-wal` sidecar, NORMAL sync, FK enforcement,
;; 30s busy timeout) is wrapped
;; in a `HikariDataSource` with a small fixed pool. SQLite WAL allows
;; N readers + 1 writer, so 5 connections give read concurrency
;; without amplifying writer contention. The pool keeps physical
;; connections alive for the process lifetime (no idle eviction,
;; no maxLifetime recycling - unlike a network DB, SQLite handles
;; have no equivalent of a connection drop). `db-close!` actually
;; closes the pool now, so db-dispose-connection! releases handles
;; deterministically.

(def ^:private wal-size-limit-bytes
  "Cap on the `-wal` sidecar file, in bytes (16 MiB).

   SQLite's default `journal_size_limit` is -1: a checkpoint hands the WAL pages
   back to the database but NEVER gives the FILE back, so one oversized
   transaction leaves the sidecar at its high-water mark for the life of the
   store. Measured on a developer machine: a 112.5 MB `vis.db-wal` holding 156
   live frames (624 KB) — 99% of the file was space nothing could reclaim.

   With a limit set, SQLite truncates the sidecar back to it on the first commit
   after a checkpoint resets the WAL. 16 MiB is 4x the 1000-page (~4 MB)
   autocheckpoint threshold, so steady-state writes never reach it and only a
   burst — a large attachment row — pays a single truncate."
  (* 16 1024 1024))

(defn- raw-sqlite-datasource
  "Build a configured xerial `SQLiteDataSource` (the plain non-pooled
   one). All pragmas (`journal_mode=WAL`, `synchronous=NORMAL`,
   `transaction_mode=IMMEDIATE`, `foreign_keys=ON`, `busy_timeout=30000`,
   `journal_size_limit=`[[wal-size-limit-bytes]]) are set on the
   `SQLiteConfig`, so every Hikari-handed connection inherits them.

   The returned object is what we hand to Hikari as its underlying
   DataSource; callers should NOT call `getConnection` on this directly."
  ^DataSource [^String url]
  (let
    [cfg
     (doto (SQLiteConfig.)
       (.setJournalMode SQLiteConfig$JournalMode/WAL)
       (.setSynchronous SQLiteConfig$SynchronousMode/NORMAL)
       (.setTransactionMode SQLiteConfig$TransactionMode/IMMEDIATE)
       (.enforceForeignKeys true)
       (.setBusyTimeout 30000)
       (.setJournalSizeLimit (int wal-size-limit-bytes)))

     ds
     (SQLiteDataSource. cfg)]

    (.setUrl ds url)
    ds))

(def ^:private pool-max-size
  "One pool now serves the WHOLE process per database file (see
   `acquire-disk-pool!`), where every environment used to get its own pool of
   5. Size it for the gateway's request threads instead: SQLite WAL runs N
   concurrent readers against 1 writer, and an idle SQLite handle costs a file
   descriptor and a few KiB."
  (max 8 (min 16 (* 2 (.availableProcessors (Runtime/getRuntime))))))

(defn- pooled-datasource
  "Wrap `raw-ds` in a HikariCP pool. `pool-name` is the JMX name and
   shows up in thread names (`HikariPool-vis-rlm-disk-1`), which
   makes debugging far easier when there are multiple envs alive.

   Pool sizing for SQLite WAL:
     maximumPoolSize = `pool-max-size` - concurrent readers for the whole
                            process; writes serialize on `sqlite-write-lock`
                            and the 30s busy timeout regardless.
     minimumIdle     = 1  - keep one connection warm; cold-start cost
                            on SQLite is small, but eliminating it
                            removes one source of `[SQLITE_CANTOPEN]`
                            jitter on the very first request.
     idleTimeout     = 0  - SQLite handles are cheap to keep; the
                            pool keeps them alive and the WAL state
                            stays warm.
     maxLifetime     = 0  - no network drop concern; recycling adds
                            zero value and creates spurious opens.
     leakDetectionThreshold = 60s - surface checked-out-but-never-
                                    returned connections in the log."
  ^HikariDataSource [^DataSource raw-ds ^String pool-name]
  (let
    [cfg (doto (HikariConfig.)
           (.setPoolName pool-name)
           (.setDataSource raw-ds)
           (.setMaximumPoolSize pool-max-size)
           (.setMinimumIdle 1)
           (.setConnectionTimeout 30000)
           (.setIdleTimeout 0)
           (.setMaxLifetime 0)
           (.setLeakDetectionThreshold 60000))]
    (HikariDataSource. cfg)))

(def ^:private ^String DB_FILENAME "vis.db")

(def ^:private ^AtomicLong pool-counter
  ;; Monotonic suffix so multiple envs alive in the same JVM get
  ;; distinct pool names (and thread names) instead of colliding.
  (AtomicLong.))

(def ^:private ^String DB_MIGRATION_LOCK_FILENAME "vis.db.migrate.lock")

(defn- close-lock-resources!
  [{:keys [^FileLock lock ^java.nio.channels.FileChannel channel ^RandomAccessFile raf]}]
  (doseq
    [close! [#(when lock (.release lock)) #(when channel (.close channel))
             #(when raf (.close raf))]]
    (try (close!) (catch Throwable _ nil))))

(def ^:private ^java.util.concurrent.ConcurrentHashMap migration-monitors
  "Per-canonical-dir IN-PROCESS mutex. A `FileLock` is JVM-WIDE: two threads of
   the SAME process locking the same region throw `OverlappingFileLockException`
   instead of blocking (the lock coordinates ACROSS processes, not within one).
   Gateway warm-pool workers can race a foreground purpose-built session; both
   open the disk env and reach `with-migration-lock!` on the same path at once
   (for example, rapid consecutive session creation). Serialize same-JVM callers
   BEFORE the cross-process file lock so they queue instead of colliding."
  (java.util.concurrent.ConcurrentHashMap.))

(defn- migration-monitor
  ^java.util.concurrent.locks.ReentrantLock [^String canonical-dir]
  (or (.get migration-monitors canonical-dir)
      (let
        [m
         (java.util.concurrent.locks.ReentrantLock.)

         prev
         (.putIfAbsent migration-monitors canonical-dir m)]

        (or prev m))))

(defn- with-migration-lock!
  "Serialize Flyway across JVMs (the cross-process file lock) AND within this JVM
   (the in-process monitor). The file lock alone is not enough: it is JVM-WIDE,
   so two threads of THIS process racing it throw `OverlappingFileLockException`
   rather than blocking — the in-process monitor makes same-JVM callers queue.
   Allows the database itself to stay multiprocess-open for normal SQLite WAL
   writes."
  [^String canonical-dir f]
  (let [^java.util.concurrent.locks.ReentrantLock monitor (migration-monitor canonical-dir)]
    (.lock monitor)
    (try
      (let
        [lock-file (File. canonical-dir DB_MIGRATION_LOCK_FILENAME)
         raf (RandomAccessFile. lock-file "rw")
         channel (.getChannel raf)
         lock (try
                (.lock channel)
                (catch Throwable t (close-lock-resources! {:channel channel :raf raf}) (throw t)))]

        (try (f) (finally (close-lock-resources! {:lock lock :channel channel :raf raf}))))
      (finally (.unlock monitor)))))

(def ^:private POOL_DRAIN_TIMEOUT_MS 5000)

(defn- close-pool!
  "Close a Hikari pool WITHOUT yanking connections out from under their owners.

   `HikariDataSource.close` ends in `abortActiveConnections`, which tears down
   the physical connection of every LEASED entry. Measured on this stack
   (sqlite-jdbc 3.53.2.1) that abort cannot corrupt a query already inside
   `sqlite3_step` - `NativeDB.step` and `NativeDB._close` are both
   `synchronized` on the same instance, so a close waits for the step - but the
   lease owner still ends up with a dead handle for its next statement, which
   is how dispose used to fail live gateway requests.

   So: stop handing out connections, wait (bounded) for the in-flight leases to
   come back, and only then close. The SIGBUS this namespace guards against
   comes from the other end of the lifecycle - see `acquire-disk-pool!`."
  [^HikariDataSource pool]
  (try (when-not (.isClosed pool)
         (when-let [^HikariPoolMXBean mx (.getHikariPoolMXBean pool)]
           (.softEvictConnections mx)
           (let [deadline (+ (System/currentTimeMillis) (long POOL_DRAIN_TIMEOUT_MS))]
             (while (and (pos? (.getActiveConnections mx)) (< (System/currentTimeMillis) deadline))
               (Thread/sleep 20)))))
       (catch Throwable _ nil))
  (try (.close pool) (catch Throwable _ nil))
  nil)

(defn- fs-file-key
  "`(dev, ino)` identity of `file`, or nil when it does not exist yet or the
   attribute cannot be read. Two different values mean the pathname now
   resolves to a DIFFERENT file."
  [^String file]
  (try (get (Files/readAttributes ^java.nio.file.Path (Paths/get file (make-array String 0))
                                  "basic:fileKey"
                                  ^"[Ljava.nio.file.LinkOption;" (make-array LinkOption 0))
            "fileKey")
       (catch Throwable _ nil)))

(defonce ^:private disk-pools
  ;; canonical `vis.db` path -> {:pool HikariDataSource :refs long}
  (atom {}))

(defonce ^:private disk-pools-monitor (Object.))

(defn- open-disk-pool!
  "Fresh pool over `file`, schema installed under the cross-process migration
   lock, then the fortnightly space reclaim handed to its own thread
   (`maintenance/vacuum-async!` — off the open path, since VACUUM rewrites the
   whole file). Closes the pool if the migration fails so a failed open leaks
   nothing."
  ^HikariDataSource [^String path ^String file]
  (let
    [raw
     (raw-sqlite-datasource (str "jdbc:sqlite:" file))

     pool
     (pooled-datasource raw (str "vis-rlm-disk-" (.incrementAndGet pool-counter)))]

    (try (with-migration-lock! path #(install-schema! pool))
         (maintenance/vacuum-async! pool file)
         pool
         (catch Throwable t (try (.close pool) (catch Throwable _ nil)) (throw t)))))

(defn- acquire-disk-pool!
  "ONE Hikari pool per SQLite file per process, reference counted.

   Why it matters, from the gateway crash (`hs_err_pid61432`): a jetty `qtp`
   thread died in `_platform_memmove` five frames under `NativeDB.step`,
   copying `WALINDEX_PGSZ - WALINDEX_HDR_SIZE` (32768 - 136) bytes into the
   wal-index at `shm_base + 0x88` and faulting at the first 16 KiB page edge.
   `mmap_size` is 0 on this sqlite build, so the wal-index `-shm` is the only
   mapping sqlite holds, and that mapping had lost its backing: the file was
   shorter than the region being written.

   The wal-index node is PER PROCESS, not per connection (`lsof` on a live
   gateway: 17 descriptors on `vis.db`, ONE on `vis.db-shm`). When a process'
   connection count for the file drops to ZERO, sqlite purges that node and
   drops its DMS lock; the next opener takes the DMS lock exclusively and
   TRUNCATES `-shm` to zero before rebuilding it. Anything still holding the
   old mapping is one write away from SIGBUS. Per-environment pools plus the
   process-wide shared connection made those zero-connection transitions
   routine - the crashed gateway had reached pool generation
   `vis-rlm-disk-19` in 75 minutes (351 in an earlier 3h21m run).

   One shared pool with `minimumIdle 1`, no idle timeout and no max lifetime
   keeps at least one connection - and therefore the wal-index and its DMS
   lock - alive for as long as ANY store is open, and makes the ordinary
   open/dispose cycle stop closing connections other callers still use.

   The pool IS retired and reopened when `file` resolves to a different inode,
   the one case where the pooled handles are worthless (see
   `db-store-stale?`)."
  ^HikariDataSource [^String path ^String file]
  (let
    [key-now
     (fs-file-key file)

     [pool doomed]
     (locking disk-pools-monitor
       (let
         [entry
          (get @disk-pools file)

          ^HikariDataSource pool
          (:pool entry)

          file-key
          (:file-key entry)

          reusable?
          (and pool
               (not (.isClosed pool))
               ;; A MISSING file is a replacement in progress, not a
               ;; reason to reuse: reusing would bind the caller to
               ;; the unlinked inode and silently lose its writes.
               (or (nil? file-key) (= key-now file-key)))]

         (if reusable?
           (do (swap! disk-pools assoc
                 file
                 {:pool pool :refs (inc (long (:refs entry 0))) :file-key (or file-key key-now)})
               [pool nil])
           (let [fresh (open-disk-pool! path file)]
             (swap! disk-pools assoc file {:pool fresh :refs 1 :file-key (fs-file-key file)})
             [fresh (when (and pool (not (.isClosed pool))) pool)]))))]

    ;; A retired generation drains OUTSIDE the monitor; its remaining holders
    ;; find a foreign entry on release and close nothing twice.
    (when doomed (close-pool! doomed))
    pool))

(defn- release-disk-pool!
  "Drop one reference; close only the last one. The decision is taken under the
   monitor, the (draining, therefore slow) close happens outside it."
  [^String file ^HikariDataSource pool]
  (let
    [doomed (locking disk-pools-monitor
              (let [entry (get @disk-pools file)]
                (cond (or (nil? entry) (not (identical? pool (:pool entry)))) pool ;; an older generation nobody tracks any more
                      (<= (long (:refs entry 1)) 1) (do (swap! disk-pools dissoc file) pool)
                      :else (do (swap! disk-pools update-in [file :refs] dec) nil))))]
    (when doomed (close-pool! doomed))
    nil))

(defn- open-sqlite-at-dir
  [^String dir]
  ;; Forward slashes for the JDBC URL, the `File` ops and the migration lock
  ;; alike, whatever separators the canonical path came back with.
  (let [path (paths/unixify (.getCanonicalPath (File. dir)))]
    (.mkdirs (File. path))
    (let
      [file (str path "/" DB_FILENAME)
       pool (acquire-disk-pool! path file)]

      {:datasource pool
       :conn pool
       :path path
       :db-file file
       :backend :sqlite
       ;; One-shot latch so a store released twice drops ONE reference.
       :dispose-once (java.util.concurrent.atomic.AtomicBoolean. false)})))

(def ^:private ^AtomicLong mem-counter (AtomicLong.))

(defn- open-sqlite-mem
  []
  ;; Use a named shared-cache in-memory DB so every pooled connection
  ;; sees the same tables. Each call gets a unique name to isolate
  ;; tests; the pool's `minimumIdle 1` keeps the shared-cache DB
  ;; alive.
  (let
    [db-name
     (str "vis_mem_" (.incrementAndGet mem-counter))

     raw
     (raw-sqlite-datasource (str "jdbc:sqlite:file:" db-name "?mode=memory&cache=shared"))

     pool
     (pooled-datasource raw (str "vis-rlm-mem-" (.incrementAndGet pool-counter)))]

    (install-schema! pool)
    {:datasource pool
     :conn pool
     :path nil
     :db-file nil
     :backend :sqlite
     :owned? true
     :mode :memory
     :dispose-once (java.util.concurrent.atomic.AtomicBoolean. false)}))

(defn- stable-db-file-key
  "Filesystem IDENTITY of the SQLite file behind a store - the `(dev, ino)`
   file key, and deliberately NOT its size or mtime.

   Used to detect when the pathname stayed the same but the file was
   replaced underneath a long-lived JVM (common in dev when `~/.vis/vis.mdb`
   gets recreated while nREPL survives).

   Size and mtime used to be part of this snapshot, and they made the check
   LIE: SQLite rewrites `vis.db` in place on every WAL checkpoint, so
   ORDINARY write traffic moved both while the inode never changed.
   `db-store-stale?` then answered true for the rest of the process, and the
   facade responds to a stale store by disposing it and opening a new one -
   which is what drove the pool-generation churn (`vis-rlm-disk-351` in
   3h21m) behind the wal-index SIGBUS described in `acquire-disk-pool!`.
   Identity moves exactly when a reopen is the right answer."
  [store]
  (when-let [db-file (:db-file store)]
    (if-let [k (fs-file-key db-file)]
      {:db-file db-file :file-key k}
      {:db-file db-file :missing? true})))

(defn- with-file-key-snapshot
  [store]
  (cond-> store
    (= :persistent (:mode store))
    (assoc :file-key-snapshot (stable-db-file-key store))))

(defn db-open!
  [db-spec]
  (try
    (cond (nil? db-spec) nil
          (= :memory db-spec) (open-sqlite-mem)
          ;; `:owned? true` for both memory and persistent: we built the
          ;; Hikari pool ourselves, so we own its lifecycle. An un-closed
          ;; pool leaks daemon threads and connection handles.
          (string? db-spec) (with-file-key-snapshot (assoc (open-sqlite-at-dir db-spec)
                                                      :owned? true
                                                      :mode :persistent))
          (map? db-spec)
          (cond (or (:datasource db-spec) (:conn db-spec))
                (let [ds (or (:datasource db-spec) (:conn db-spec))]
                  (install-schema! ds)
                  {:datasource ds
                   :conn ds
                   :path nil
                   :db-file nil
                   :backend :external
                   :owned? false
                   :mode :external})
                (:path db-spec) (with-file-key-snapshot (assoc (open-sqlite-at-dir (:path db-spec))
                                                          :owned? true
                                                          :mode :persistent))
                :else (throw (ex-info "Invalid db-spec map"
                                      {:type :vis/invalid-db-spec :db-spec db-spec})))
          :else (throw (ex-info "Invalid db-spec" {:type :vis/invalid-db-spec :db-spec db-spec})))
    (catch Throwable e
      ;; A checksum drift from an in-place migration edit is self-healed
      ;; NON-DESTRUCTIVELY inside `migration/migrate!` (Flyway `repair` realigns
      ;; the recorded checksums, preserving all rows), so it should not reach
      ;; here. If some other bootstrap failure surfaces, normalize it into a
      ;; clean caller-facing error — never delete the store.
      (throw (maybe-wrap-db-open-error e)))))

(defn db-close!
  "Idempotent dispose. Releases our reference on the per-file Hikari pool when
   we own it - the pool itself is closed (after draining in-flight leases, see
   `close-pool!`) once the last holder lets go. For `:external` mode
   (caller-supplied DataSource) it's a no-op - the caller still owns the handle
   they passed in.

   Idempotent means idempotent PER STORE, enforced by the store's own
   `:dispose-once` latch: the reference is dropped exactly once however often
   the same store is disposed. `dispose-environment!` is reachable from several
   teardown paths (eviction, `delete!`, `close-all!`, the `main` finally), and
   a second release would close a pool other environments are still using."
  [store]
  (when (and store
             (:owned? store)
             (let [^java.util.concurrent.atomic.AtomicBoolean once (:dispose-once store)]
               (or (nil? once) (.compareAndSet once false true))))
    (let
      [^Object ds
       (:datasource store)

       file
       (:db-file store)]

      (cond (and file (instance? HikariDataSource ds)) (release-disk-pool! file ds)
            (instance? HikariDataSource ds) (close-pool! ds)
            (instance? java.io.Closeable ds) (try (.close ^java.io.Closeable ds)
                                                  (catch Throwable _ nil))
            :else nil)))
  nil)

(defn- canonical-path
  "Canonicalize for IDENTITY comparison — the store's :path went through
   open and is canonical, while a caller's spec may carry the raw form
   (`/tmp/x` vs `/private/tmp/x` through the macOS symlink, relative vs
   absolute). Comparing raw strings made every db-info call see a STALE
   store -> dispose+reopen storm; under request concurrency the
   disposals killed sibling in-flight queries (\"HikariDataSource ...
   has been closed\")."
  ^String [p]
  (try (.getCanonicalPath (java.io.File. (str p))) (catch Throwable _ (str p))))

(defn db-store-stale?
  "True when a persistent SQLite store no longer matches the requested
   db-spec or the file at the same path was REPLACED under this JVM. When
   true, the facade closes the old shared pool and opens a new one, so this
   predicate must never fire on a store that is merely being written to -
   the close lands on connections other threads are querying.

   Paths compare CANONICALIZED on both sides; the file compares by
   filesystem identity only (see `stable-db-file-key`)."
  [store db-spec]
  (when (= :persistent (:mode store))
    (or (and (string? db-spec) (not= (canonical-path db-spec) (canonical-path (:path store))))
        (and (map? db-spec)
             (:path db-spec)
             (not= (canonical-path (:path db-spec)) (canonical-path (:path store))))
        (and (:file-key-snapshot store)
             (not= (stable-db-file-key store) (:file-key-snapshot store))))))

;; SQLite write policy

(def ^:private sqlite-write-retry-delays-ms [5 10 20 40 80 160])

(defonce ^:private sqlite-write-lock (Object.))

(defn- sqlite-busy-cause?
  [^Throwable t]
  (let
    [message
     (some-> (.getMessage t)
             str/lower-case)

     code
     (when (instance? SQLException t) (.getErrorCode ^SQLException t))]

    (or (contains? #{5 6 517} code)
        (and message
             (or (str/includes? message "sqlite_busy")
                 (str/includes? message "busy_snapshot")
                 (str/includes? message "database is locked")
                 (str/includes? message "database table is locked"))))))

(defn- sqlite-busy?
  [^Throwable t]
  (loop [cause t]
    (cond (nil? cause) false
          (sqlite-busy-cause? cause) true
          :else (recur (.getCause cause)))))

(defn- sqlite-write-attempt!
  "Run one immediate write transaction attempt. The xerial connection config
   sets `transaction_mode=IMMEDIATE`, so next.jdbc's transaction start acquires
   the SQLite writer reservation before any read can become a stale WAL
   snapshot. Nested calls reuse the caller's tx-info."
  [db-info f]
  (if (:sqlite-write-tx? db-info)
    (f db-info)
    (jdbc/with-transaction [tx (ds db-info)]
                           (f (assoc db-info
                                :datasource tx
                                :conn tx
                                :sqlite-write-tx? true)))))

(defn- sqlite-write-tx!
  "Central SQLite write boundary. Serializes Vis writes inside this JVM,
   starts an immediate SQLite transaction, and retries the whole operation on
   stale WAL snapshots / busy lock failures. Retry must happen outside the
   transaction because SQLITE_BUSY_SNAPSHOT poisons the current transaction's
   snapshot."
  [db-info f]
  (if (:sqlite-write-tx? db-info)
    (f db-info)
    (locking sqlite-write-lock
      (loop
        [attempt
         0

         delays
         sqlite-write-retry-delays-ms]

        (let
          [result (try {:success? true :value (sqlite-write-attempt! db-info f)}
                       (catch Throwable t {:success? false :throwable t}))]
          (if (:success? result)
            (:value result)
            (let [t (:throwable result)]
              (if (and (sqlite-busy? t) (seq delays))
                (do (Thread/sleep (long (first delays))) (recur (inc attempt) (rest delays)))
                (throw t)))))))))

;; Logging - log table

(defn db-log!
  [db-info entry]
  (when (ds db-info)
    (sqlite-write-tx!
      db-info
      (fn [tx-info]
        (execute! tx-info
                  {:insert-into :log
                   :values [(cond->
                              {:id (new-id)
                               :level (->kw (:level entry))
                               :event (str (:event entry))
                               :created_at (now-ms)}
                              (:data entry)
                              (assoc :data (:data entry))

                              (:session-soul-id entry)
                              (assoc :session_soul_id (->id (:session-soul-id entry)))

                              (:session-state-id entry)
                              (assoc :session_state_id (->id (:session-state-id entry)))

                              (:session-turn-soul-id entry)
                              (assoc :session_turn_soul_id (->id (:session-turn-soul-id entry)))

                              (:session-turn-state-id entry)
                              (assoc :session_turn_state_id (->id (:session-turn-state-id entry)))

                              (:iteration-id entry)
                              (assoc :session_turn_iteration_id
                                (->id (:iteration-id entry))))]})))))

;; Workspace - trunk-native work units


(defn- row->workspace
  "Project a `workspace` row from SQLite into the canonical Clojure shape
   used by the workspace facade. Keys mirror `db-get-session` style
   (plain, not namespaced); the workspace ns adds `:workspace/*` aliases
   when publishing into an environment."
  [row]
  (when row
    (cond->
      {:id (->uuid (:id row))
       :type :workspace
       :repo-id (:repo_id row)
       :repo-root (:repo_root row)
       :root (:root row)
       :state (->kw-back (:state row))
       :created-at (->date (:created_at row))}
      (:fork_ms row)
      (assoc :fork-ms (:fork_ms row))

      (:apply_fork_ms row)
      (assoc :apply-fork-ms (:apply_fork_ms row))

      (:workspace_kind row)
      (assoc :workspace-kind (->kw-back (:workspace_kind row)))

      (:workspace_backend row)
      (assoc :workspace-backend (->kw-back (:workspace_backend row)))

      (:workspace_mechanism row)
      (assoc :workspace-mechanism (->kw-back (:workspace_mechanism row)))

      (:parent_workspace_id row)
      (assoc :parent-workspace-id (->uuid (:parent_workspace_id row)))

      (:discarded_at row)
      (assoc :discarded-at (->date (:discarded_at row)))

      ;; Surfaced for the workspace facade's label/focus helpers.
      ;; NULL columns are skipped so callers can
      ;; treat absence as "fall back to default" (label → session.title;
      ;; last-focused → created_at).
      (:label row)
      (assoc :label (:label row))

      (:last_focused_at_ms row)
      (assoc :last-focused-at-ms (:last_focused_at_ms row))

      ;; Extra filesystem roots bound to this workspace: the per-root draft
      ;; clones minted for it. JSON array of
      ;; `{trunk, clone, fork_ms, backend, policy}` maps.
      (:filesystem_roots row)
      (assoc :filesystem-roots (or (<-json (:filesystem_roots row)) [])))))


(defn db-workspace-insert!
  "Insert a workspace row. Returns the inserted record (canonical shape).

   Required: :repo-id :repo-root :root
   Optional: :id (defaults to a new UUID), :label, :fork-ms, :apply-fork-ms,
             :workspace-kind, :workspace-backend, :workspace-mechanism (how the
             backend made the clone), :parent-workspace-id, :state
             (defaults to :active), :filesystem-roots (extra per-root draft
             clones, persisted as a JSON array)"
  [db-info
   {:keys [id repo-id repo-root root label fork-ms apply-fork-ms workspace-kind workspace-backend
           workspace-mechanism parent-workspace-id state filesystem-roots]}]
  (when (ds db-info)
    (let
      [ws-id
       (->id (or id (new-uuid)))

       now
       (now-ms)]

      (sqlite-write-tx!
        db-info
        (fn [tx-info]
          (execute! tx-info
                    {:insert-into :workspace
                     :values [{:id ws-id
                               :repo_id repo-id
                               :repo_root repo-root
                               :root root
                               :label label
                               :fork_ms fork-ms
                               :apply_fork_ms apply-fork-ms
                               :workspace_kind (->kw (or workspace-kind (if fork-ms :draft :trunk)))
                               :workspace_backend (->kw (or workspace-backend :live))
                               :workspace_mechanism (some-> workspace-mechanism
                                                            ->kw)
                               :parent_workspace_id (some-> parent-workspace-id
                                                            ->ref)
                               :state (->kw (or state :active))
                               :filesystem_roots (when (seq filesystem-roots)
                                                   (->json (vec filesystem-roots)))
                               :created_at now}]})
          (row->workspace (query-one! tx-info
                                      {:select [:*] :from :workspace :where [:= :id ws-id]})))))))

(defn db-workspace-update-state!
  "Transition `workspace-id` to `new-state` (`:active` | `:discarded`).
   Stamps `discarded_at` on :discarded. Returns the updated record."
  [db-info workspace-id new-state]
  (when (and (ds db-info) workspace-id)
    (let
      [id
       (->ref workspace-id)

       now
       (now-ms)

       to
       (->kw new-state)

       set
       (cond-> {:state to}
         (= "discarded" to)
         (assoc :discarded_at now))]

      (sqlite-write-tx! db-info
                        (fn [tx-info]
                          (execute! tx-info {:update :workspace :set set :where [:= :id id]})
                          (row->workspace
                            (query-one! tx-info
                                        {:select [:*] :from :workspace :where [:= :id id]})))))))

(defn db-workspace-update-label!
  "Set the human-friendly `:label` override. Pass nil to clear the
   label and fall back to the heuristic. Returns the updated record."
  [db-info workspace-id label]
  (when (and (ds db-info) workspace-id)
    (let [id (->ref workspace-id)]
      (sqlite-write-tx!
        db-info
        (fn [tx-info]
          (execute! tx-info {:update :workspace :set {:label label} :where [:= :id id]})
          (row->workspace (query-one! tx-info
                                      {:select [:*] :from :workspace :where [:= :id id]})))))))


(defn db-workspace-touch-focus!
  "Stamp `last_focused_at_ms` to now-ms on the workspace row. Called by
   `workspace/focus!`. Returns the updated record."
  [db-info workspace-id]
  (when (and (ds db-info) workspace-id)
    (let
      [id
       (->ref workspace-id)

       now
       (now-ms)]

      (sqlite-write-tx!
        db-info
        (fn [tx-info]
          (execute! tx-info {:update :workspace :set {:last_focused_at_ms now} :where [:= :id id]})
          (row->workspace (query-one! tx-info
                                      {:select [:*] :from :workspace :where [:= :id id]})))))))

(defn db-repo-focus-get
  "Return the `workspace_id` currently pinned as the focus pointer for
   `repo-id`. Nil when no entry exists yet."
  [db-info repo-id]
  (when (and (ds db-info) repo-id)
    (some-> (query-one! db-info
                        {:select [:workspace_id :updated_at_ms]
                         :from :repo_focus
                         :where [:= :repo_id repo-id]})
            (as-> r {:workspace-id (->uuid (:workspace_id r)) :updated-at-ms (:updated_at_ms r)}))))

(defn db-repo-focus-set!
  "Upsert the per-repo focus pointer to `workspace-id`. Updates
   `updated_at_ms` to now. Returns the new pointer map."
  [db-info repo-id workspace-id]
  (when (and (ds db-info) repo-id workspace-id)
    (let
      [ws-id
       (->ref workspace-id)

       now
       (now-ms)]

      (sqlite-write-tx! db-info
                        (fn [tx-info]
                          ;; SQLite UPSERT: INSERT ... ON CONFLICT REPLACE the row.
                          (execute! tx-info
                                    {:insert-into :repo_focus
                                     :values
                                     [{:repo_id repo-id :workspace_id ws-id :updated_at_ms now}]
                                     :on-conflict :repo_id
                                     :do-update-set {:workspace_id ws-id :updated_at_ms now}})
                          {:workspace-id (->uuid ws-id) :updated-at-ms now})))))

(defn db-workspace-get
  [db-info workspace-id]
  (when (and (ds db-info) workspace-id)
    (row->workspace
      (query-one! db-info {:select [:*] :from :workspace :where [:= :id (->ref workspace-id)]}))))

(defn db-workspace-list-by-repo
  "List workspaces in `repo-id`, optionally filtered to a `state-set` of
   keywords (e.g. #{:active :merging}). Newest first."
  ([db-info repo-id] (db-workspace-list-by-repo db-info repo-id nil))
  ([db-info repo-id state-set]
   (when (ds db-info)
     (let
       [where (cond-> [:and [:= :repo_id repo-id]]
                (seq state-set)
                (conj [:in :state (mapv ->kw state-set)]))]
       (mapv row->workspace
             (query!
               db-info
               {:select [:*] :from :workspace :where where :order-by [[:created_at :desc]]}))))))

(defn db-workspace-list-drafts
  "Every draft workspace across all repos, newest first. Housekeeping reads
   this to decide which draft clones on disk have gone stale; it is
   deliberately repo-agnostic because the drafts store is a single
   `~/.vis/drafts` tree shared by every trunk."
  [db-info]
  (when (ds db-info)
    (mapv row->workspace
          (query! db-info
                  {:select [:*]
                   :from :workspace
                   :where [:= :workspace_kind (->kw :draft)]
                   :order-by [[:created_at :desc]]}))))

(defn db-workspace-for-session
  "Return the workspace pinned to `session-state-id`, or nil. Always
   non-nil after step-4 wiring (1:1 invariant); nil only during the
   transitional window where session_state may not yet have a workspace."
  [db-info session-state-id]
  (when (and (ds db-info) session-state-id)
    (let [sid (->ref session-state-id)]
      (row->workspace (query-one! db-info
                                  {:select [:w.*]
                                   :from [[:session_state :s]]
                                   :join [[:workspace :w] [:= :w.id :s.workspace_id]]
                                   :where [:= :s.id sid]})))))

(defn db-session-state-list-for-workspace
  "List session_state rows whose `workspace_id` = `workspace-id`,
   newest first. The 1:1 invariant guarantees at most one ACTIVE row
   per workspace, but historical version rows (from forks) may exist
   so the projection is a vec, not a single row. Returns canonical
   `{:id :session-soul-id :title :version :created-at …}` shape."
  [db-info workspace-id]
  (when (and (ds db-info) workspace-id)
    (let [ws-id (->ref workspace-id)]
      (mapv (fn [r]
              {:id (->uuid (:id r))
               :session-soul-id (->uuid (:session_soul_id r))
               :title (:title r)
               :version (:version r)
               :workspace-id (->uuid (:workspace_id r))
               :created-at (->date (:created_at r))})
            (query! db-info
                    {:select [:*]
                     :from :session_state
                     :where [:= :workspace_id ws-id]
                     :order-by [[:version :desc]]})))))

(defn db-session-state-set-workspace!
  "Pin `session-state-id` to `workspace-id`. Caller guarantees the
   target session_state row exists and has no other workspace pinned
   (UNIQUE on session_state.workspace_id enforces 1:1)."
  [db-info session-state-id workspace-id]
  (when (and (ds db-info) session-state-id workspace-id)
    (sqlite-write-tx! db-info
                      (fn [tx-info]
                        (execute! tx-info
                                  {:update :session_state
                                   :set {:workspace_id (->ref workspace-id)}
                                   :where [:= :id (->ref session-state-id)]})
                        {:session-state-id (->uuid session-state-id)
                         :workspace-id (->uuid workspace-id)}))))

;; Session - session_soul + session_state

(defn db-store-session!
  "Create session_soul + initial session_state (version 0).
   Returns the session-soul UUID.

   Required opt: `:workspace-id` — session_state.workspace_id is NOT NULL
   after V1, enforcing the 1:1 invariant. The caller
   (loop/create-environment) calls `workspace/ensure-trunk!` to mint a
   trunk workspace before invoking this fn.

   Session identity and LLM root defaults are first-class columns:
     session_soul.channel / external_id
     session_state.system_prompt / llm_root_provider / llm_root_model
     session_state.title."
  [db-info
   {:keys [channel external-id title system-prompt provider model workspace-id parent-state-id
           claimed? owner-id]
    :or {claimed? true}}]
  (when-not workspace-id
    (throw (ex-info "db-store-session! requires :workspace-id (1:1 invariant)"
                    {:type :persistance/missing-workspace-id})))
  (when (ds db-info)
    (sqlite-write-tx!
      db-info
      (fn [tx-info]
        (let
          [soul-id
           (new-uuid)

           state-id
           (new-uuid)

           now
           (now-ms)]

          (execute! tx-info
                    {:insert-into :session_soul
                     :values [(cond->
                                {:id (str soul-id)
                                 :channel (name (->kw (or channel :tui)))
                                 :external_id (some-> external-id
                                                      str)
                                 :created_at now
                                 :owner_id (or owner-id "local")
                                 ;; adoption marker (V5). Unclaimed (NULL)
                                 ;; = warm-pool scaffolding, hidden from
                                 ;; db-list-sessions; claimed = a real
                                 ;; conversation. Prewarm passes :claimed?
                                 ;; false; everyone else defaults true.
                                 :claimed_at (when claimed? now)}
                                ;; child session → cross-soul link to the parent state;
                                ;; keeps the child OUT of the top-level list (queryable
                                ;; sub-tree + cascade-delete with the parent).
                                parent-state-id
                                (assoc :parent_state_id (->ref parent-state-id)))]})
          (execute! tx-info
                    {:insert-into :session_state
                     :values [(cond->
                                {:id (str state-id)
                                 :session_soul_id (str soul-id)
                                 :workspace_id (->ref workspace-id)
                                 :title title
                                 :version 0
                                 :system_prompt (or system-prompt "")
                                 :llm_root_model (or model "")
                                 :created_at now}
                                provider
                                (assoc :llm_root_provider (name (->kw provider))))]})
          soul-id)))))

(defn- latest-state-for
  [db-info soul-id-s]
  (query-one! db-info
              {:select [:*]
               :from :session_state
               :where [:and [:= :session_soul_id soul-id-s]
                       [:= :version
                        {:select [[[:max :version]]]
                         :from :session_state
                         :where [:= :session_soul_id soul-id-s]}]]}))

(defn db-get-session
  [db-info session-id]
  (when (and (ds db-info) session-id)
    (let [id (->ref session-id)]
      (when-let [soul (query-one! db-info {:select [:*] :from :session_soul :where [:= :id id]})]
        (let
          [state (latest-state-for db-info id)
           project (when (:project_id soul)
                     (query-one!
                       db-info
                       {:select [:name] :from :project :where [:= :id (:project_id soul)]}))]

          (cond->
            {:id (->uuid (:id soul))
             :type :session
             :channel (->kw-back (:channel soul))
             :external-id (:external_id soul)
             :title (:title state)
             :system-prompt (:system_prompt state)
             :model (:llm_root_model state)
             :version (or (:version state) 0)
             :created-at (->date (:created_at soul))
             :owner-id (:owner_id soul)
             :project-id (->uuid (:project_id soul))
             :project-position (:project_position soul)
             ;; The human's STAR, off the `:*` soul row already read: every list
             ;; row carries it without a second query per session (nil =
             ;; unstarred). Backend-owned, so two clients cannot disagree.
             :favorite-rank (:favorite_rank soul)}
            (:llm_root_provider state)
            (assoc :provider (->kw-back (:llm_root_provider state)))

            ;; The per-session model PIN lives on this same `session_soul` row
            ;; (`db-get-session-model-pref` reads exactly these two columns), and
            ;; the row is already selected with `:*`. Carrying it here is free and
            ;; lets a session list answer "which model does this run on?" without
            ;; an extra per-session round-trip.
            (not-empty (:llm_pref_model soul))
            (assoc :model-pref {:provider (:llm_pref_provider soul) :model (:llm_pref_model soul)})

            project
            (assoc :project-name (:name project))))))))

(defn db-resolve-session-id
  [db-info selector]
  (cond (nil? selector) nil
        (= :latest selector)
        (when (ds db-info)
          (when-let
            [row (query-one!
                   db-info
                   {:select [:id] :from :session_soul :order-by [[:created_at :desc]] :limit 1})]
            (->uuid (:id row))))
        (and (vector? selector) (= :id (first selector))) (->uuid (second selector))
        (uuid? selector) selector
        (string? selector) (->uuid selector)
        :else nil))

(defn db-list-sessions
  "Top-level sessions, newest-first.

   `channel` filters by the `session_soul.channel` column; pass `:all`
   (or nil) for the CROSS-CHANNEL view — every channel's sessions in one
   list. This is the shared view web + TUI use so a conversation started
   in one surface is visible and resumable from the other."
  [db-info channel]
  (when (ds db-info)
    (let
      [ch
       (some-> channel
               ->kw
               name)

       all?
       (or (nil? ch) (= "all" ch))]

      (mapv
        (fn [row]
          {:id (->uuid (:id row))
           :channel (->kw-back (:channel row))
           :external-id (:external_id row)
           :title (:state_title row)
           :version (:version row)
           :fork-count (or (:fork_count row) 0)
           :created-at (->date (:created_at row))
           :owner-id (:owner_id row)
           :project-id (->uuid (:project_id row))
           :project-position (:project_position row)
           :project-name (:project_name row)
           :favorite-rank (:favorite_rank row)})
        (query! db-info
                {:select [:cs.id :cs.channel :cs.external_id :cs.created_at :cs.owner_id
                          :cs.project_id :cs.project_position :cs.favorite_rank
                          [:p.name :project_name] [:s.title :state_title] :s.version
                          [{:select [[[:count :*]]]
                            :from [[:session_state :child]]
                            :where [:and [:= :child.session_soul_id :cs.id]
                                    [:not= :child.parent_state_id nil]]} :fork_count]]
                 :from [[:session_soul :cs]]
                 :join [[:session_state :s] [:= :s.session_soul_id :cs.id]]
                 :left-join [[:project :p] [:= :p.id :cs.project_id]]
                 :where (into [:and
                               ;; TOP-LEVEL only — child souls (parent_state_id set)
                               ;; hang off their parent's sub-tree, never the session list.
                               [:= :cs.parent_state_id nil]
                               ;; ADOPTION filter: hide unclaimed warm-pool
                               ;; scaffolding (NULL claimed_at) - only real
                               ;; conversations reach the cross-channel list.
                               [:not= :cs.claimed_at nil]
                               [:= :s.version
                                {:select [[[:max :s2.version]]]
                                 :from [[:session_state :s2]]
                                 :where [:= :s2.session_soul_id :cs.id]}]]
                              (when-not all? [[:= :cs.channel ch]]))
                 :order-by [[:cs.project_position :asc] [:cs.created_at :desc]]})))))


(def ^:private transcript-hits-per-session
  "Snippets kept per matching session. The old shape collapsed a session to ONE
   `MAX()`-picked snippet per side, so a session with 40 hits still showed one
   arbitrary (lexicographically largest!) line. Now the newest few real hits
   travel."
  6)

(def ^:private transcript-hit-scan-limit
  "Newest matching transcript ROWS RANKED per side before the per-session cap.
   Ranking is id-only (no `snippet()`), so even a stop-word query that matches
   tens of thousands of rows costs a few tens of milliseconds."
  20000)

(def ^:private transcript-hit-sessions-limit
  "Sessions (newest hit first) that get snippets rendered. Bounds the second,
   snippet-producing pass to `transcript-hits-per-session` × this many rows."
  200)

(defn- raw-query!
  "Run a raw `[sql & params]` vector — for the SQL HoneySQL cannot express
   (FTS5 `MATCH`, `snippet()`). Rows come back with unqualified lower-case keys."
  [db-info sql-vec]
  (jdbc/execute! (ds db-info) sql-vec {:builder-fn rs/as-unqualified-lower-maps}))

(defn- fts-match-expr
  "FTS5 MATCH expression for a human `query`: every alphanumeric token QUOTED
   (so punctuation can never be read as FTS syntax) and AND-ed, with the LAST
   token turned into a PREFIX term so search stays useful mid-typing — `dia`
   finds `dialogs`. A ONE-character token is left exact: `d*` would expand to a
   doclist of nearly every document and cost more than the scan it replaced.
   nil when the query tokenizes to nothing (`???`)."
  [query]
  (let
    [tokens
     (->> (str/split (str query) #"[^\p{Alnum}_]+")
          (remove str/blank?)
          vec)

     last-i
     (dec (count tokens))]

    (when (seq tokens)
      (str/join " "
                (map-indexed (fn [i t]
                               (str "\"" t "\"" (when (and (= i last-i) (< 1 (count t))) "*")))
                             tokens)))))

(def ^:private snippet-hit-marker
  "Sentinel FTS5 wraps around a matched term inside `snippet()`. Its PRESENCE is
   how we tell a column that really matched from one whose snippet is just its
   leading text — `snippet()` happily returns the head of an unmatched column."
  "\u0001")

(defn- hit-snippet
  "The snippet to SHOW for a hit row, WITH the band that snippet came from:
   `{:side :request|:reply|:thinking :snippet str-or-nil}`.

   The reply side asks SQLite for one snippet per indexed column — assistant
   prose (0) then thinking (1) — and the FIRST column that really matched wins,
   so a term present in both renders the answer rather than the reasoning aside,
   and a term found ONLY in the reasoning is tagged `:thinking` and ranks below
   every answer. Marker stripped, blank → nil."
  [side row]
  (let
    [request?
     (= :request side)

     bands
     (if request? [:request] [:reply :thinking])

     cols
     (if request? [(:snip row)] [(:s0 row) (:s1 row)])

     matched
     (fn [pred]
       (first (keep-indexed (fn [i c]
                              (when (pred c) i))
                            cols)))

     idx
     (or (matched #(and % (str/includes? % snippet-hit-marker))) (matched some?) 0)]

    {:side (nth bands idx side)
     :snippet (some-> (nth cols idx nil)
                      (str/replace snippet-hit-marker "")
                      str/trim
                      not-empty)}))

(defn- transcript-hit-sql
  "FTS query for ONE side (`:request` from the user's own text, `:reply` from
   assistant prose/thinking): session soul id, FTS rowid, the row's `created_at`
   and the `snippet()` window(s) that say WHY it matched — newest first, in ONE
   statement.

   Newest-first is `rowid DESC` — insertion order — deliberately NOT `created_at`:
   ordering on a non-indexed column would force a sort of every match.

   The DESC walk, the scan cap AND the snippets live in a SUBQUERY over the FTS
   table ALONE, before any join. Both halves of that are measured:

   FTS5 can only serve `ORDER BY rowid DESC` from its own index when nothing else
   is in the query; ordering the joined result instead made SQLite spool EVERY
   match into a temp B-tree and sort it (`USE TEMP B-TREE FOR ORDER BY`), so a
   stop word paid for all 75k of its hits to hand back 20k — 240ms of a 300ms
   search.

   And `snippet()` used to be a SECOND pass that repeated the MATCH and
   intersected it with the ranked rowids (`… MATCH ? AND rowid IN (…)`). SQLite
   re-runs that MATCH once PER rowid, and a PREFIX term longer than the
   `prefix='2 3'` indexes — `star*`, which is what a human actually types — has
   to be re-expanded across the term index on every one of those seeks: 0.75ms a
   row, ~800ms of snippets for one query against a 2.2GB store, against 5ms for
   the same 400 rows under an exact term. That single pass was most of the wait
   the app reported as a search that hangs with nothing to show. Snippeting the
   ordered scan instead expands the prefix ONCE for the whole query (~150ms for
   `star*`, ~270ms for a stop word) and needs no second statement at all.

   The cap therefore counts matches BEFORE the claimed/unforked filter rather
   than after: beyond `transcript-hit-scan-limit` newest hits the tail is a
   depth heuristic either way, and the per-session cap decides what is shown."
  [side chan-sql]
  (let
    [fts
     (case side
       :request
       "transcript_request_fts"

       :reply
       "transcript_reply_fts")

     ;; The reply side asks for one window per indexed column — assistant prose
     ;; (0) then thinking (1) — so `hit-snippet` can tell an answer from the
     ;; reasoning aside. `char(1)` is the marker that proves a column really
     ;; matched; the request side has one column and needs none.
     snips
     (case side
       :request
       (str "snippet(" fts ", 0, '', '', '…', 20) AS snip")

       :reply
       (str "snippet("
            fts
            ", 0, char(1), '', '…', 20) AS s0, "
            "snippet("
            fts
            ", 1, char(1), '', '…', 20) AS s1"))

     cols
     (case side
       :request
       "f.snip AS snip"

       :reply
       "f.s0 AS s0, f.s1 AS s1")

     joins
     (case side
       :request
       (str "JOIN session_turn_soul ts ON ts.rowid = f.rid "
            "JOIN session_state s ON s.id = ts.session_state_id ")

       :reply
       (str "JOIN session_turn_iteration it ON it.rowid = f.rid "
            "JOIN session_turn_state tst ON tst.id = it.session_turn_state_id "
            "JOIN session_turn_soul ts ON ts.id = tst.session_turn_soul_id "
            "JOIN session_state s ON s.id = ts.session_state_id "))

     at
     (case side
       :request
       "ts.created_at"

       :reply
       "it.created_at")]

    (str "SELECT cs.id AS sid, f.rid AS rid, "
         cols
         ", "
         at
         " AS at "
         "FROM (SELECT rowid AS rid, "
         snips
         " FROM "
         fts
         " WHERE "
         fts
         " MATCH ? "
         "ORDER BY rowid DESC LIMIT "
         transcript-hit-scan-limit
         ") f "
         joins
         "JOIN session_soul cs ON cs.id = s.session_soul_id "
         "WHERE cs.parent_state_id IS NULL AND cs.claimed_at IS NOT NULL"
         chan-sql
         " ORDER BY f.rid DESC")))

(defn- transcript-hit-rows
  "FTS hits on ONE side, FAIRLY capped: up to `transcript-hits-per-session`
   snippets for EACH of the newest `transcript-hit-sessions-limit` matching
   sessions — not the newest N rows overall, which is what starved older
   sessions down to a single snippet.

   ONE statement does it (`transcript-hit-sql`): the ranked walk and the
   `snippet()` windows arrive together, and the snippet is also what SPLITS the
   reply side into `:reply` (the answer) and `:thinking` (the reasoning aside) —
   see `hit-snippet`. The caps are applied here, on rows already in hand."
  [db-info side ch match]
  (let
    [chan-sql
     (if ch " AND cs.channel = ?" "")

     params
     (cond-> [match]
       ch
       (conj ch))

     ranked
     (raw-query! db-info (into [(transcript-hit-sql side chan-sql)] params))

     ;; `ranked` is newest-first, so first appearance order = sessions ordered
     ;; by their newest hit.
     sids
     (into #{} (take transcript-hit-sessions-limit) (distinct (map :sid ranked)))

     kept
     (->> ranked
          (filter #(contains? sids (:sid %)))
          (reduce (fn [acc r]
                    (let
                      [sid
                       (:sid r)

                       n
                       (long (get-in acc [:counts sid] 0))]

                      (if (>= n (long transcript-hits-per-session))
                        acc
                        (-> acc
                            (assoc-in [:counts sid] (inc n))
                            (update :rows conj r)))))
                  {:counts {} :rows []})
          :rows)]

    (mapv (fn [row]
            (let [hit (hit-snippet side row)]
              {:sid (str (:sid row)) :side (:side hit side) :at (:at row) :snippet (:snippet hit)}))
          kept)))

(defn- title-hit-rows
  "Sessions whose own TITLE contains `needle`, shaped exactly like
   `transcript-hit-rows` so a name and a transcript line rank in ONE table.

   SUBSTRING, case-insensitive (`instr`), not the FTS prefix match the transcript
   uses: a title is one short line a human wrote, and someone hunting for it types
   the middle of a word as readily as its start. `instr` also needs no `LIKE`
   escaping, so a query holding `%` or `_` means itself.

   The title travels as the hit's snippet only to carry the match; the caller
   keeps title hits out of the snippet list, which is about transcript text."
  [db-info ch needle]
  (let
    [sql
     (str "SELECT cs.id AS sid, s.title AS title, cs.created_at AS at " "FROM session_soul cs "
          "JOIN session_state s ON s.session_soul_id = cs.id "
          "WHERE cs.parent_state_id IS NULL AND cs.claimed_at IS NOT NULL "
          "AND s.title IS NOT NULL AND instr(lower(s.title), ?) > 0 "
          "AND s.version = (SELECT MAX(s2.version) FROM session_state s2 "
          "WHERE s2.session_soul_id = cs.id)" (if ch " AND cs.channel = ?" "")
          " ORDER BY cs.created_at DESC LIMIT " transcript-hit-sessions-limit)

     params
     (cond-> [(str/lower-case (str needle))]
       ch
       (conj ch))]

    (mapv (fn [row]
            {:sid (str (:sid row)) :side :title :at (:at row) :snippet (:title row)})
          (raw-query! db-info (into [sql] params)))))

(def ^:private transcript-hit-side-rank
  "Ordering band of ONE search hit, best first — the ONE table every surface is
   ranked by, computed here and shipped to clients as `:rank`.

   A search is someone looking for a SESSION, and its TITLE is the one line a
   human wrote to name it, so a name hit outranks anything found inside the chat.
   Inside the chat what the USER wrote outranks the assistant's ANSWER, and the
   answer outranks its THINKING: the request is the thing they remember typing,
   the answer is what it caused, and the reasoning aside is not what either of
   them said."
  {:title 0 :request 1 :reply 2 :thinking 3})

(defn- session-activity-at
  "When each of `sids` LAST MOVED, epoch ms, keyed by soul-id string.

   The same instant a list read reports as `modified_at` (`db-session-turn-stats`
   `:latest-turn-at`): the newest turn across every state of the soul, falling
   back to the soul's own creation for a session nothing has ever run in. Search
   ORDERS by this (`search-rows->sessions`), which is why it must come from the
   same place the row's own date does — an order computed from anything else
   paints a list whose dates do not descend.

   ONE grouped statement over the matched ids only: ~6ms for 292 sessions on a
   2.2 GB store, against the ~40ms whole-store stats scan a listing pays."
  [db-info sids]
  (if (empty? sids)
    {}
    (let
      [sql (str "SELECT cs.id AS sid, MAX(COALESCE(ts.created_at, cs.created_at)) AS at "
                "FROM session_soul cs "
                "LEFT JOIN session_state ss ON ss.session_soul_id = cs.id "
                "LEFT JOIN session_turn_soul ts ON ts.session_state_id = ss.id "
                "WHERE cs.id IN (" (str/join "," (repeat (count sids) "?"))
                ") " "GROUP BY cs.id")]
      (into {}
            (map (fn [row]
                   [(str (:sid row)) (long (or (:at row) 0))]))
            (raw-query! db-info (into [sql] sids))))))

(defn- search-rows->sessions
  "Fold per-row hits into one entry per session soul, FRESHEST SESSION FIRST.

   Ordering happens HERE, on the server, and travels as the vector's own order so
   no client invents one: sessions are sorted by when each LAST MOVED
   (`activity`, soul-id string -> epoch ms), newest first. A search is someone
   looking for a conversation they had, and the newest is nearly always the one
   they mean.

   `transcript-hit-side-rank` still decides `:rank` — title, then the user's
   request, then the assistant's reply, then its thinking — and it breaks a tie
   between two sessions that last moved at the same instant. Inside a session the
   same bands order the hits, so the per-session cap
   (`transcript-hits-per-session`) keeps the user's words before the assistant's
   and the reasoning aside last.

   `:hits` carries TRANSCRIPT snippets only — a title hit is reported by
   `:in-title?` and by the rank, not as a fake chat line. `:request-snippet` /
   `:reply-snippet` stay as the FIRST hit of each side so single-snippet callers
   keep working, the reply snippet falling back to a thinking hit when that is
   all there was."
  [rows activity]
  (->> rows
       (filter :snippet)
       (group-by :sid)
       (mapv
         (fn [[sid hits]]
           (let
             [at
              (fn [h]
                (long (or (:at h) 0)))

              band
              (fn [h]
                (long (get transcript-hit-side-rank (:side h) 2)))

              ordered
              (vec (sort-by (juxt band (comp - at)) hits))

              kept
              (vec (take transcript-hits-per-session (remove #(= :title (:side %)) ordered)))

              side?
              (fn [& sides]
                (boolean (some (comp (set sides) :side) ordered)))

              side-snip
              (fn [& sides]
                (some #(when ((set sides) (:side %)) (:snippet %)) ordered))]

             {:id (->uuid sid)
              :activity (long (get activity (str sid) 0))
              :rank (reduce min 9 (map band ordered))
              :in-title? (side? :title)
              :in-request? (side? :request)
              :in-reply? (side? :reply)
              :in-thinking? (side? :thinking)
              :request-snippet (side-snip :request)
              :reply-snippet (side-snip :reply :thinking)
              :hits (mapv (fn [h]
                            {:side (:side h) :snippet (:snippet h) :at (->date (:at h))})
                          kept)})))
       ;; Freshest first; the band and then the id keep the order TOTAL, so the
       ;; same store answers the same query the same way every time.
       (sort-by (juxt (comp - long :activity) (comp long :rank) (comp str :id)))
       (mapv #(dissoc % :activity))))

(defn db-search-session-matches
  "Sessions whose TITLE or TRANSCRIPT text matches `query`, RANKED, each carrying
   WHERE it hit and up to `transcript-hits-per-session` MATCH SNIPPETS:

   `{:id uuid :rank 0-3 :in-title? bool :in-request? bool :in-reply? bool
     :in-thinking? bool :request-snippet str-or-nil :reply-snippet str-or-nil
     :hits [{:side :request|:reply|:thinking :snippet str :at Date}]}`

   `:in-title?` = the session's own name matched (`title-hit-rows`, substring);
   `:in-request?` = the user's request (`session_turn_soul.user_request`) matched;
   `:in-reply?` = the assistant's ANSWER (`llm_assistant_prose`) matched;
   `:in-thinking?` = its reasoning aside (`llm_thinking`) matched — the weakest
   band, since it is what neither of them said. Any of the four can be true
   together.

   The RESULT ORDER IS THE ANSWER, and the answer is FRESHNESS: sessions come
   back newest-first by the instant each one last moved (`session-activity-at`
   — the same fact a list read prints as `modified_at`), so a result list reads
   like the session list it filters and the dates only ever fall as you scan
   down. Someone hunting a conversation looks for the one they were in, not for
   the strongest lexical band: a title match from a year ago must not sit above
   this morning's session. `:rank` still travels — it breaks a tie between two
   sessions that last moved at the same instant, orders the hits INSIDE a
   session, and tells a surface WHERE the query hit without it re-deciding
   anything. See `transcript-hit-side-rank`.

   Transcript matching is served by the V1 FTS5 indexes (`transcript_request_fts` /
   `transcript_reply_fts`) — single-digit milliseconds instead of the ~400ms full
   `LIKE '%q%'` scan of the whole corpus, which is what made TUI/app session search
   feel frozen. It is token PREFIX matching (`dia` finds `dialogs`); a query with
   NO alphanumeric token at all (`???`) still searches TITLES, which are matched
   as a plain substring.

   This is the SERVER-side half of session search: the assistant text never
   crosses the wire, only these snippet windows. `channel` filters like
   `db-list-sessions` (`:all`/nil = cross-channel). Blank query returns `[]`."
  [db-info channel query]
  (let
    [q (some-> query
               str
               str/trim)]
    (if (or (not (ds db-info)) (nil? q) (= "" q))
      []
      (let
        [ch (some-> channel
                    ->kw
                    name)
         ch (when-not (or (nil? ch) (= "all" ch)) ch)
         match (fts-match-expr q)
         rows (cond-> (title-hit-rows db-info ch q)
                match
                (into (transcript-hit-rows db-info :request ch match))

                match
                (into (transcript-hit-rows db-info :reply ch match)))
         sids (into #{} (map :sid) rows)]

        (search-rows->sessions rows (session-activity-at db-info sids))))))

(defn db-search-session-ids
  "Soul ids whose TRANSCRIPT text matches `query`.

   Thin id-only projection of `db-search-session-matches` (see it for the
   where-it-hit tags and snippets). `channel` filters like `db-list-sessions`
   (`:all`/nil = cross-channel). Blank query returns `[]`."
  [db-info channel query]
  (mapv :id (db-search-session-matches db-info channel query)))

(defn db-session-turn-stats
  "Per-session turn aggregates: `{:turn-count n :latest-turn-at Date
   :first-request str-or-absent}`.

   1-arity: the WHOLE store in ONE grouped query, keyed by soul-id string —
   `{soul-id-str stats}`. Powers the session picker summaries (`turn_count` +
   `modified_at` folded into list-sessions) without an N+1 per-session
   `db-list-session-turns` hydration.

   2-arity: the SAME aggregate for ONE session, returned unwrapped (nil when
   the session has no state rows). `GET /v1/sessions/:id` needs these two facts
   as much as the list does — without them a client cannot tell that a session
   moved — and a detail poll must not scan every session to learn them.

   `:first-request` is the RAW `user_request` of the session's EARLIEST turn —
   what the session opened with, so a picker row can show the actual ask beside
   a generated title. It rides a second grouped query that leans on SQLite's
   bare-column/`min()` pairing (exactly ONE aggregate per row, so the pairing is
   well defined). Callers bound/clean it for display; it is stored text, not a
   preview.

   Counts every turn soul across ALL of a session's states (forks included) — an
   upper bound of the chain view, but exact for `has any turns?` and for
   latest-activity ordering."
  ([db-info]
   (if (ds db-info)
     (let
       [firsts (into {}
                     (map (fn [row]
                            [(str (:sid row))
                             (some-> (:req row)
                                     str
                                     not-empty)]))
                     (query! db-info
                             {:select [[:ss.session_soul_id :sid] [:ts.user_request :req]
                                       [[:min :ts.created_at] :first_at]]
                              :from [[:session_turn_soul :ts]]
                              :join [[:session_state :ss] [:= :ss.id :ts.session_state_id]]
                              :group-by [:ss.session_soul_id]}))]
       (into {}
             (map (fn [row]
                    (let [sid (str (:sid row))]
                      [sid
                       (cond->
                         {:turn-count (long (or (:n row) 0)) :latest-turn-at (->date (:latest row))}
                         (get firsts sid)
                         (assoc :first-request (get firsts sid)))])))
             (query! db-info
                     {:select [[:ss.session_soul_id :sid] [[:count :ts.id] :n]
                               [[:max :ts.created_at] :latest]]
                      :from [[:session_turn_soul :ts]]
                      :join [[:session_state :ss] [:= :ss.id :ts.session_state_id]]
                      :group-by [:ss.session_soul_id]})))
     {}))
  ([db-info session-id]
   (when (and (ds db-info) session-id)
     (let [soul-id-s (->ref session-id)]
       (when-let
         [row (query-one! db-info
                          {:select [[[:count :ts.id] :n] [[:max :ts.created_at] :latest]]
                           :from [[:session_turn_soul :ts]]
                           :join [[:session_state :ss] [:= :ss.id :ts.session_state_id]]
                           :where [:= :ss.session_soul_id soul-id-s]})]
         (let
           [first-request
            (some-> (query-one! db-info
                                {:select [[:ts.user_request :req] [[:min :ts.created_at] :first_at]]
                                 :from [[:session_turn_soul :ts]]
                                 :join [[:session_state :ss] [:= :ss.id :ts.session_state_id]]
                                 :where [:= :ss.session_soul_id soul-id-s]})
                    :req
                    str
                    not-empty)]
           (cond-> {:turn-count (long (or (:n row) 0)) :latest-turn-at (->date (:latest row))}
             first-request
             (assoc :first-request first-request))))))))

(defn- session-usage-scope
  "Honeysql `[from join where]` fragments selecting the CURRENT chain of the
   session soul `soul-id-s`: every turn soul across the session's states, each
   pinned to its HIGHEST `session_turn_state.version`. Retried/superseded turn
   versions are excluded, so a usage rollup counts what the transcript shows
   rather than every attempt ever written."
  [soul-id-s]
  [[[:session_turn_state :sts]]
   [[:session_turn_soul :ts] [:= :ts.id :sts.session_turn_soul_id] [:session_state :ss]
    [:= :ss.id :ts.session_state_id]]
   [:and [:= :ss.session_soul_id soul-id-s]
    [:= :sts.version
     {:select [[[:max :s2.version]]]
      :from [[:session_turn_state :s2]]
      :where [:= :s2.session_turn_soul_id :sts.session_turn_soul_id]}]]])

;; ── whole-session usage tally ────────────────────────────────────────────────
;;
;; `session_turn_iteration.tool_calls` is written ONCE at INSERT and never
;; UPDATEd, so ONE row's per-tool tally is a constant and can be cached for the
;; process lifetime, keyed by row id.
;;
;; That matters because the usage rollup is fetched per session ROW EXPAND in
;; the companion (`GET /v1/sessions/:sid/usage`), and it used to re-thaw the
;; session's ENTIRE blob history every single time — measured at ~200 ms for a
;; real 2 907-call session, paid again on every open, per client. With the
;; cache a blob is thawed at most ONCE, a LIVE session pays only for the
;; iterations added since the last read, and an unchanged session answers from
;; the merged memo without touching a blob at all.

(def ^:private ^java.util.concurrent.ConcurrentHashMap usage-tally-cache
  "Iteration row id → that row's `{tool n}` call tally. Counts only — no
   forms, no results — so a row costs a few tens of bytes."
  (java.util.concurrent.ConcurrentHashMap.))

(def ^:private usage-tally-cache-max
  "Row ceiling. On overflow the whole cache is dropped: a rebuild costs one
   decode pass, never correctness."
  20000)

(def ^:private ^java.util.concurrent.ConcurrentHashMap usage-rollup-cache
  "Session ref → `{:fingerprint [row-count id-digest] :tally {…}}`, the
   MERGED tally, so re-reading an unchanged session skips even the per-row
   merge. The fingerprint moves whenever an iteration row is added (new id) or
   removed (new count), which is exactly when the rollup can differ."
  (java.util.concurrent.ConcurrentHashMap.))

(def ^:private usage-rollup-cache-max
  "Session ceiling; dropped wholesale on overflow like the row cache."
  512)

(def ^:private empty-tally {})

(defn- forms-tally
  "ONE iteration's `{tool n}` call tally over its decoded tool-call `forms`,
   keyed by `:vis/tool-name`."
  [forms]
  (reduce (fn [acc form]
            (let [n (str (:vis/tool-name form))]
              (if (str/blank? n) acc (update acc n (fnil inc 0)))))
          empty-tally
          forms))

(defn- merge-tally "Sum two `{tool n}` tallies tool by tool." [a b] (merge-with + a b))

(defn- usage-tally
  "Merged `{tool n}` call tally over the iteration rows `row-ids` of session
   `sref`, computed at most ONCE per row per process.

   Three tiers, cheapest first: an unchanged session returns its memoised merge;
   a session whose rows are all tallied re-merges cached per-row counts without
   touching SQLite; only genuinely new rows are fetched — in bounded chunks, ids
   first — and Nippy-thawed."
  [db-info sref row-ids]
  (let
    [;; Row ids are opaque STRINGS, so the fingerprint is row count plus an
     ;; order-independent digest of the ids: it moves the moment a row is
     ;; added or removed, and never depends on SQLite's row order.
     fingerprint
     [(count row-ids)
      (reduce (fn [acc id]
                (unchecked-add (long acc) (long (hash id))))
              0
              row-ids)]

     memo
     (.get usage-rollup-cache sref)]

    (if (= fingerprint (:fingerprint memo))
      (:tally memo)
      (let [missing (into [] (remove #(.containsKey usage-tally-cache %)) row-ids)]
        (when (seq missing)
          (when (> (.size usage-tally-cache) (long usage-tally-cache-max))
            (.clear usage-tally-cache))
          (doseq [chunk (partition-all 200 missing)]
            (doseq
              [row (query! db-info
                           {:select [:qti.id :qti.tool_calls]
                            :from [[:session_turn_iteration :qti]]
                            :where [:and [:in :qti.id (vec chunk)] [:<> :qti.tool_calls nil]]})]
              (.put usage-tally-cache (:id row) (forms-tally (<-blob (:tool_calls row)))))
            ;; A row that vanished between the id query and this one tallies
            ;; empty — never re-queried, never a miss loop.
            (doseq [rid chunk]
              (when-not (.containsKey usage-tally-cache rid)
                (.put usage-tally-cache rid empty-tally)))))
        (let
          [tally (reduce (fn [acc rid]
                           (merge-tally acc (or (.get usage-tally-cache rid) empty-tally)))
                         empty-tally
                         row-ids)]
          (when (> (.size usage-rollup-cache) (long usage-rollup-cache-max))
            (.clear usage-rollup-cache))
          (.put usage-rollup-cache sref {:fingerprint fingerprint :tally tally})
          tally)))))

(defn- session-fold-count
  "How many folds a session CARRIES: the length of the fold ledger
   (`ctx` -> `\"session_summaries\"`) on its newest turn state that has a ctx.

   Never the tool tally. A fold is `fold_session(...)` called INSIDE a
   `python_execution` block, so no tool form is ever named `fold_session` and a
   tally over `:vis/tool-name` reports 0 for every session that ever folded. The
   ledger is also the number a reader means: superseded breadcrumbs are dropped
   when a broader fold covers them, so it counts folds that still SHAPE the wire
   rather than fold calls ever issued."
  [db-info from join where]
  (let
    [row
     (query-one! db-info
                 {:select [:sts.ctx]
                  :from from
                  :join join
                  :where (conj where [:<> :sts.ctx nil])
                  :order-by [[:ts.created_at :desc] [:sts.version :desc]]
                  :limit 1})]

    (long (count (get (some-> (:ctx row) <-blob) "session_summaries")))))

(defn db-session-usage-stats
  "ONE session's whole-life USAGE rollup, or nil when the session has no turns:

   `{:turn-count :iteration-count :duration-ms :input-tokens
     :input-regular-tokens :input-cache-write-tokens :input-cache-read-tokens
     :output-tokens :output-reasoning-tokens :cost-usd :first-turn-at
     :last-turn-at :provider :model :tool-call-count :fold-count}`

   Token/cost/iteration facts are SQL aggregates over the turn-state rollups
   (`session_turn_state`), which the turn writer already maintains — no
   iteration scan. The TOOL count has no column: it lives inside each
   iteration's nippy `tool_calls` BLOB, so it is counted by `:vis/tool-name`
   through `usage-tally`, which thaws any one row's blob at most ONCE per
   process and memoises the merged result per session — a re-read costs two
   skinny id-only queries, a repeat read of an unchanged session costs nothing
   beyond them, and a live session pays only for the iterations added since the
   last read. It is still an ON-DEMAND single-session read and never part of
   `list-sessions`.

   The FOLD count is not in that blob at all — a fold is called INSIDE a
   `python_execution` block, never as a tool of its own — so it is read from the
   newest turn's fold ledger by `session-fold-count`.

   Only the TOTALS survive the blob pass: per-tool and per-error rankings had no
   reader left on any channel, so they are not computed."
  [db-info session-id]
  (when (and (ds db-info) session-id)
    (let
      [soul-id-s
       (->ref session-id)

       [from join where]
       (session-usage-scope soul-id-s)

       agg
       (query-one! db-info
                   {:select [[[:count :sts.id] :turns] [[:sum :sts.iteration_count] :iterations]
                             [[:sum :sts.duration_ms] :duration_ms]
                             [[:sum :sts.input_tokens] :input_tokens]
                             [[:sum :sts.input_regular_tokens] :input_regular_tokens]
                             [[:sum :sts.input_cache_write_tokens] :input_cache_write_tokens]
                             [[:sum :sts.input_cache_read_tokens] :input_cache_read_tokens]
                             [[:sum :sts.output_tokens] :output_tokens]
                             [[:sum :sts.output_reasoning_tokens] :output_reasoning_tokens]
                             [[:sum :sts.total_cost_usd] :cost_usd]
                             [[:min :ts.created_at] :first_at] [[:max :ts.created_at] :last_at]]
                    :from from
                    :join join
                    :where where})]

      (when (pos? (long (or (:turns agg) 0)))
        (let
          [latest
           ;; The newest turn that HAS a model. A turn's row is inserted when
           ;; the turn STARTS and only stamped with provider/model when it
           ;; finishes, so a plain "newest turn" pick reads NULL for as long as
           ;; a session is live — exactly when a client is looking at it — and
           ;; the rollup then reported no model at all. Skip unstamped rows.
           (query-one! db-info
                       {:select [:sts.llm_root_provider :sts.llm_root_model]
                        :from from
                        :join join
                        :where (conj where [:<> :sts.llm_root_model nil])
                        :order-by [[:ts.created_at :desc]]
                        :limit 1})

           ;; Ids first, blobs in chunks: an oversized session never pulls its
           ;; whole tool-call history across the JDBC boundary at once.
           iter-ids
           (mapv :id
                 (query! db-info
                         {:select [:qti.id]
                          :from [[:session_turn_iteration :qti]]
                          :join (into [[:session_turn_state :sts]
                                       [:= :sts.id :qti.session_turn_state_id]]
                                      join)
                          :where (conj where [:<> :qti.tool_calls nil])}))

           ;; ONE Nippy thaw per iteration row, ONCE per process (see
           ;; `usage-tally`).
           counts
           (usage-tally db-info soul-id-s iter-ids)

           folds
           (session-fold-count db-info from join where)]

          (cond->
            {:turn-count (long (or (:turns agg) 0))
             :iteration-count (long (or (:iterations agg) 0))
             :duration-ms (long (or (:duration_ms agg) 0))
             :input-tokens (long (or (:input_tokens agg) 0))
             :input-regular-tokens (long (or (:input_regular_tokens agg) 0))
             :input-cache-write-tokens (long (or (:input_cache_write_tokens agg) 0))
             :input-cache-read-tokens (long (or (:input_cache_read_tokens agg) 0))
             :output-tokens (long (or (:output_tokens agg) 0))
             :output-reasoning-tokens (long (or (:output_reasoning_tokens agg) 0))
             :cost-usd (double (or (:cost_usd agg) 0))
             :tool-call-count (long (reduce + 0 (vals counts)))
             :fold-count folds}
            (:first_at agg)
            (assoc :first-turn-at (long (:first_at agg)))

            (:last_at agg)
            (assoc :last-turn-at (long (:last_at agg)))

            (:llm_root_provider latest)
            (assoc :provider (str (:llm_root_provider latest)))

            (:llm_root_model latest)
            (assoc :model (str (:llm_root_model latest)))))))))

(defn db-find-session-by-external
  [db-info channel external-id]
  (when (and (ds db-info) external-id)
    (let
      [ch
       (name (->kw channel))

       ext
       (str external-id)]

      (when-let
        [row (query-one! db-info
                         {:select [:id]
                          :from :session_soul
                          :where [:and [:= :channel ch] [:= :external_id ext]]})]
        (->uuid (:id row))))))

(defn db-latest-session-state-id
  "Return the latest `session_state.id` UUID for the soul behind
   `session-id` (which is the soul id). Used by the iteration
   loop to bind `TURN_SESSION_STATE_ID` so the agent can reference
   the exact `session_state` row this turn was attached to.
   Returns nil when the session is unknown or the env has no
   datasource."
  [db-info session-id]
  (when (and (ds db-info) session-id)
    (let [soul-id-s (->ref session-id)]
      (some-> (latest-state-for db-info soul-id-s)
              :id
              ->uuid))))

(defn db-update-session-title!
  [db-info session-id title]
  (when (and (ds db-info) session-id)
    (sqlite-write-tx! db-info
                      (fn [tx-info]
                        (let [soul-id-s (->ref session-id)]
                          (when-let [state (latest-state-for tx-info soul-id-s)]
                            (execute! tx-info
                                      {:update :session_state
                                       :set {:title title}
                                       :where [:= :id (:id state)]})))))))

(defn db-claim-session!
  "Mark an unclaimed warm-pool soul as a real conversation. Idempotent."
  [db-info session-id]
  (when (and (ds db-info) session-id)
    (sqlite-write-tx! db-info
                      (fn [tx-info]
                        (execute! tx-info
                                  {:update :session_soul
                                   :set {:claimed_at (now-ms)}
                                   :where [:and [:= :id (->ref session-id)]
                                           [:= :claimed_at nil]]})))))

(defn db-delete-session-tree!
  [db-info session-soul-id]
  (when (and (ds db-info) session-soul-id)
    (sqlite-write-tx!
      db-info
      (fn [tx-info]
        (execute! tx-info {:delete-from :session_soul :where [:= :id (->id session-soul-id)]})))))

;; Projects (cross-channel) + movable project sessions + ownership  (V6/V7)

(defn- row->project
  "Project a `project` row (with an optional `session_count` aggregate) into the
   canonical Clojure shape."
  [row]
  {:id (->uuid (:id row))
   :owner-id (:owner_id row)
   :name (:name row)
   :color (:color row)
   :position (:position row)
   :created-at (->date (:created_at row))
   :archived-at (->date (:archived_at row))
   :workspace-root (:workspace_root row)
   :session-count (or (:session_count row) 0)})

(def ^:private project-select-cols
  [:p.id :p.owner_id :p.name :p.color :p.position :p.workspace_root :p.created_at :p.archived_at
   [{:select [[[:count :*]]] :from [[:session_soul :ss]] :where [:= :ss.project_id :p.id]}
    :session_count]])

(defn db-get-project
  "Return one `project` (canonical shape, with live `:session-count`) or nil."
  [db-info project-id]
  (when (and (ds db-info) project-id)
    (some-> (query-one! db-info
                        {:select project-select-cols
                         :from [[:project :p]]
                         :where [:= :p.id (->id project-id)]})
            row->project)))

(defn db-list-projects
  "List `project`s for `owner-id` (default \"local\"), each with a live
   `:session-count`, ordered by (position, created_at). Projects are
   CROSS-CHANNEL by construction — every channel sees the same set.
   Archived projects are hidden unless `:include-archived?` is truthy."
  [db-info {:keys [owner-id include-archived?]}]
  (when (ds db-info)
    (let [owner (or owner-id "local")]
      (mapv row->project
            (query! db-info
                    {:select project-select-cols
                     :from [[:project :p]]
                     :where (into [:and [:= :p.owner_id owner]]
                                  (when-not include-archived? [[:= :p.archived_at nil]]))
                     :order-by [[:p.position :asc] [:p.created_at :asc]]})))))

(defn db-get-project-by-root
  "Return the `project` bound to canonical workspace `root` for `owner-id`
   (default \"local\"), or nil. Backs the TUI's launch-dir -> project (tab set)
   resolution. `root` matches the stored `project.workspace_root` exactly
   (callers pass a canonical path)."
  [db-info owner-id root]
  (when (and (ds db-info) (not (str/blank? (str root))))
    (some-> (query-one! db-info
                        {:select project-select-cols
                         :from [[:project :p]]
                         :where [:and [:= :p.owner_id (or owner-id "local")]
                                 [:= :p.workspace_root (str root)]]})
            row->project)))

(defn db-create-project!
  "Create a `project` (always cross-channel). `:name` is required (non-blank).
   `:owner-id` defaults to \"local\". `:position`, when omitted, appends after
   the owner's current projects. Returns the created project (canonical shape)."
  [db-info {:keys [name color owner-id position workspace-root]}]
  (when (str/blank? (str name))
    (throw (ex-info "db-create-project! requires a non-blank :name"
                    {:type :persistance/invalid-project-name})))
  (when (ds db-info)
    (let
      [project-id
       (sqlite-write-tx!
         db-info
         (fn [tx-info]
           (let
             [pid (new-uuid)
              now (now-ms)
              owner (or owner-id "local")
              pos (or position
                      (inc (long (or (:maxpos (query-one! tx-info
                                                          {:select [[[:max :position] :maxpos]]
                                                           :from :project
                                                           :where [:= :owner_id owner]}))
                                     -1))))]

             (execute!
               tx-info
               {:insert-into :project
                :values
                [(cond->
                   {:id (str pid) :owner_id owner :name (str name) :position pos :created_at now}
                   color
                   (assoc :color color)

                   (not (str/blank? (str workspace-root)))
                   (assoc :workspace_root (str workspace-root)))]})
             pid)))]
      (db-get-project db-info project-id))))

(defn db-update-project!
  "Patch a `project`: any of `:name` (non-blank), `:color`, `:position`,
   `:archived?` (true stamps `archived_at`=now, false clears it). Returns the
   updated project (canonical shape) or nil when nothing to change."
  [db-info project-id {:keys [name color position archived? workspace-root] :as opts}]
  (when (and (ds db-info) project-id (seq opts))
    (when (and (contains? opts :name) (str/blank? (str name)))
      (throw (ex-info "db-update-project! :name must be non-blank"
                      {:type :persistance/invalid-project-name})))
    (let
      [set-map (cond-> {}
                 (contains? opts :name)
                 (assoc :name (str name))

                 (contains? opts :color)
                 (assoc :color color)

                 (contains? opts :position)
                 (assoc :position position)

                 (contains? opts :archived?)
                 (assoc :archived_at (when archived? (now-ms)))

                 (contains? opts :workspace-root)
                 (assoc :workspace_root workspace-root))]
      (when (seq set-map)
        (sqlite-write-tx!
          db-info
          (fn [tx-info]
            (execute! tx-info {:update :project :set set-map :where [:= :id (->id project-id)]})))
        (db-get-project db-info project-id)))))

(defn db-delete-project!
  "Delete a `project`. Member souls are scattered back to project-less by the
   `ON DELETE SET NULL` FK - conversations are NEVER deleted."
  [db-info project-id]
  (when (and (ds db-info) project-id)
    (sqlite-write-tx! db-info
                      (fn [tx-info]
                        (execute! tx-info
                                  {:delete-from :project :where [:= :id (->id project-id)]})))))

(defn db-set-session-project!
  "Assign the soul behind `session-id` to `project-id`; a nil `project-id` clears
   membership (removes it from its project AND resets its now-meaningless
   `project_position` to 0, so no stale ordinal is left behind). Moving INTO a
   project the soul does NOT already belong to APPENDS it (its `project_position`
   becomes max+1 within that project) so project sessions stay MOVABLE;
   re-assigning a soul ALREADY in the project is idempotent and keeps its current
   position. Returns the soul id."
  [db-info session-id project-id]
  (when (and (ds db-info) session-id)
    (sqlite-write-tx!
      db-info
      (fn [tx-info]
        (let
          [pid
           (when project-id (->id project-id))

           cur
           (:project_id (query-one! tx-info
                                    {:select [:project_id]
                                     :from :session_soul
                                     :where [:= :id (->id session-id)]}))

           member?
           (and pid (= (str cur) (str pid)))

           set-map
           (cond
             ;; already a member -> idempotent, keep its order
             member? {:project_id (->ref project-id)}
             ;; joining a project -> append after its last member
             pid {:project_id (->ref project-id)
                  :project_position
                  (inc (long (or (:maxpos (query-one! tx-info
                                                      {:select [[[:max :project_position] :maxpos]]
                                                       :from :session_soul
                                                       :where [:= :project_id pid]}))
                                 -1)))}
             ;; leaving all projects -> clear pointer + stale ordinal
             :else {:project_id (->ref nil) :project_position 0})]

          (execute! tx-info
                    {:update :session_soul :set set-map :where [:= :id (->id session-id)]}))))
    session-id))

(defn- reorder-members-in-tx!
  "Within an open write transaction, renumber every soul in `pid` to a gap-free
   0-based `project_position`, leading with the members named in `session-ids`.
   Unnamed members retain their relative order after those names. Two parking
   phases avoid transient collisions with the unique project-position index."
  [tx-info pid session-ids]
  (let
    [members
     (mapv (comp str :id)
           (query! tx-info
                   {:select [:id]
                    :from :session_soul
                    :where [:= :project_id pid]
                    :order-by [[:project_position :asc] [:created_at :desc]]}))

     member?
     (set members)

     wanted
     (distinct (filter member? (map str session-ids)))

     wanted-set
     (set wanted)

     ordered
     (into (vec wanted) (remove wanted-set members))]

    (doseq [[pos sid] (map-indexed vector ordered)]
      (execute! tx-info
                {:update :session_soul
                 :set {:project_position (- (inc (long pos)))}
                 :where [:and [:= :id (->id sid)] [:= :project_id pid]]}))
    (doseq [[pos sid] (map-indexed vector ordered)]
      (execute! tx-info
                {:update :session_soul
                 :set {:project_position pos}
                 :where [:and [:= :id (->id sid)] [:= :project_id pid]]}))
    (count ordered)))

(defn db-reorder-project-sessions!
  "Persist a manual order for the sessions inside `project-id`. `session-ids` is
   the desired leading order; any members not named are kept and appended in their
   current order. Every member receives a gap-free, 0-based `project_position`.
   Only souls already belonging to `project-id` are touched. Returns the member
   count."
  [db-info project-id session-ids]
  (when (and (ds db-info) project-id)
    (sqlite-write-tx! db-info
                      (fn [tx-info]
                        (reorder-members-in-tx! tx-info (->id project-id) session-ids)))))

(defn db-adopt-and-reorder-project-sessions!
  "Persist a manual tab order for `project-id` in one atomic transaction. Named
   sessions with no project are adopted first; sessions owned by another project
   remain guests and are never stolen. Every target-project member is then
   renumbered exactly as in `db-reorder-project-sessions!`. Returns the member
   count."
  [db-info project-id session-ids]
  (when (and (ds db-info) project-id)
    (sqlite-write-tx!
      db-info
      (fn [tx-info]
        (let
          [pid
           (->id project-id)

           wanted
           (vec (distinct (map str session-ids)))

           loose?
           (if (seq wanted)
             (set (map (comp str :id)
                       (query! tx-info
                               {:select [:id]
                                :from :session_soul
                                :where [:and [:in :id (mapv ->id wanted)] [:= :project_id nil]]})))
             #{})

           loose
           (filterv loose? wanted)]

          (when (seq loose)
            (let
              [maxpos (long (or (:maxpos (query-one! tx-info
                                                     {:select [[[:max :project_position] :maxpos]]
                                                      :from :session_soul
                                                      :where [:= :project_id pid]}))
                                -1))]
              (doseq [[offset sid] (map-indexed vector loose)]
                (execute! tx-info
                          {:update :session_soul
                           :set {:project_id (->ref project-id)
                                 :project_position (+ maxpos 1 (long offset))}
                           :where [:and [:= :id (->id sid)] [:= :project_id nil]]}))))
          (reorder-members-in-tx! tx-info pid session-ids))))))

;; Fork - branch a session at a point

(defn db-list-session-states
  "List every `session_state` row for the soul behind `session-id`,
   oldest version first. Each row maps to
   `{:state-id :version :parent-state-id :title :system-prompt :provider :model
     :created-at :turn-count}` - the raw fork tree of one session soul.

   The trunk is `:version 0` with `:parent-state-id nil`. A fork is any row
   whose `:parent-state-id` points at another `:state-id` in the same vector;
   group-by `:parent-state-id` to walk the tree.

   `:turn-count` is the number of `session_turn_soul` rows hanging off that specific
   state - cheap to compute, useful when triaging which branch is active.

   Returns `[]` (never nil) when the session is unknown or the env has no
   datasource."
  [db-info session-id]
  (if (and (ds db-info) session-id)
    (let
      [soul-id-s
       (->ref session-id)

       rows
       (query! db-info
               {:select [:cs.id :cs.version :cs.parent_state_id :cs.title :cs.system_prompt
                         :cs.llm_root_provider :cs.llm_root_model :cs.created_at
                         [{:select [[[:count :*]]]
                           :from :session_turn_soul
                           :where [:= :session_turn_soul.session_state_id :cs.id]} :turn_count]]
                :from [[:session_state :cs]]
                :where [:= :cs.session_soul_id soul-id-s]
                :order-by [[:cs.version :asc]]})]

      (mapv (fn [row]
              (cond->
                {:state-id (->uuid (:id row))
                 :version (:version row)
                 :parent-state-id (some-> (:parent_state_id row)
                                          ->uuid)
                 :title (:title row)
                 :created-at (->date (:created_at row))
                 :turn-count (or (:turn_count row) 0)}
                (:system_prompt row)
                (assoc :system-prompt (:system_prompt row))

                (:llm_root_provider row)
                (assoc :provider (->kw-back (:llm_root_provider row)))

                (:llm_root_model row)
                (assoc :model (:llm_root_model row))))
            rows))
    []))

(defn db-list-session-turn-states
  "List every `session_turn_state` row (i.e. every retry version) for the soul behind
   `session-turn-id`, oldest version first. Each row maps to
   `{:state-id :version :forked-from-session-turn-state-id :status :prior-outcome
     :provider :model :created-at :iteration-count}`.

   Version 0 with `:forked-from-session-turn-state-id nil` is the original run; any
   higher version is a retry, with `:forked-from-session-turn-state-id` pointing at
   the previous `:state-id`. `:iteration-count` is the number of `iteration`
   rows attached to that specific state - retries get their own iteration
   trace.

   Returns `[]` (never nil) when the turn is unknown or the env has no
   datasource."
  [db-info session-turn-id]
  (if (and (ds db-info) session-turn-id)
    (let
      [soul-id-s
       (->ref session-turn-id)

       rows
       (query! db-info
               {:select [:qst.id :qst.version :qst.forked_from_session_turn_state_id :qst.status
                         :qst.prior_outcome :qst.llm_root_provider :qst.llm_root_model
                         :qst.created_at
                         [{:select [[[:count :*]]]
                           :from :session_turn_iteration
                           :where [:= :session_turn_iteration.session_turn_state_id :qst.id]}
                          :session_turn_iteration_count]]
                :from [[:session_turn_state :qst]]
                :where [:= :qst.session_turn_soul_id soul-id-s]
                :order-by [[:qst.version :asc]]})]

      (mapv (fn [row]
              (cond->
                {:state-id (->uuid (:id row))
                 :version (:version row)
                 :forked-from-session-turn-state-id (some-> (:forked_from_session_turn_state_id row)
                                                            ->uuid)
                 :status (->kw-back (:status row))
                 :created-at (->date (:created_at row))
                 :iteration-count (or (:session_turn_iteration_count row) 0)}
                (:prior_outcome row)
                (assoc :prior-outcome (->kw-back (:prior_outcome row)))

                (:llm_root_provider row)
                (assoc :provider (->kw-back (:llm_root_provider row)))

                (:llm_root_model row)
                (assoc :model (:llm_root_model row))))
            rows))
    []))

(defn db-fork-session!
  "Fork a session. Creates a new session_state with
   parent_state_id pointing to the current latest state.
   The forked state gets a '(fork)' suffix when no title is supplied.
   The parent state gets a '[forked]' suffix to mark the divergence point.
   Returns the new state UUID.

   Required opt: `:workspace-id` — every session_state must be pinned to
   exactly one workspace. Callers (screen handlers, cli) call
   `workspace/spawn-branch!` or `workspace/ensure-trunk!` first and pass
   the returned id here."
  [db-info session-id {:keys [system-prompt provider model title workspace-id]}]
  (when-not workspace-id
    (throw (ex-info "db-fork-session! requires :workspace-id (1:1 invariant)"
                    {:type :persistance/missing-workspace-id})))
  (when (ds db-info)
    (sqlite-write-tx!
      db-info
      (fn [tx-info]
        (let [soul-id-s (->ref session-id)]
          (when-let [current (latest-state-for tx-info soul-id-s)]
            (let
              [new-id (new-uuid)
               now (now-ms)
               parent-title (:title current)
               fork-title (or title (str parent-title " (fork)"))
               new-version (inc (long (:version current)))]

              (execute! tx-info
                        {:insert-into :session_state
                         :values [(cond->
                                    {:id (str new-id)
                                     :session_soul_id soul-id-s
                                     :parent_state_id (:id current)
                                     :workspace_id (->ref workspace-id)
                                     :title fork-title
                                     :version new-version
                                     :system_prompt (or system-prompt (:system_prompt current))
                                     :llm_root_model (or model (:llm_root_model current))
                                     :created_at now}
                                    (or provider (:llm_root_provider current))
                                    (assoc :llm_root_provider
                                      (name (->kw (or provider (:llm_root_provider current))))))]})
              ;; Mark parent state as forked (idempotent)
              (when (and parent-title (not (str/includes? parent-title "[forked]")))
                (execute! tx-info
                          {:update :session_state
                           :set {:title (str parent-title " [forked]")}
                           :where [:= :id (:id current)]}))
              new-id)))))))

;; State resolution

(defn- latest-state-id
  [db-info session-id]
  (when (ds db-info)
    (let [soul-id-s (->ref session-id)]
      (:id (latest-state-for db-info soul-id-s)))))

(defn- session-state-chain
  "Return active branch state ids from root to latest leaf."
  [db-info session-id]
  (when (and (ds db-info) session-id)
    (when-let [leaf-id (latest-state-id db-info session-id)]
      (loop
        [state-id leaf-id
         acc []]

        (if state-id
          (if-let
            [row (query-one!
                   db-info
                   {:select [:id :parent_state_id] :from :session_state :where [:= :id state-id]})]
            (recur (:parent_state_id row) (conj acc (:id row)))
            (vec (reverse acc)))
          (vec (reverse acc)))))))

;; Runtime-value serialization helpers
;;
;; These live above any code that calls them (db-update-session-turn! snapshots
;; CTX through `freeze-safe`; the iteration writer does the same for per-form
;; results). Keep this block here — do not push it back down past consumers, or
;; you'll be tempted to `(declare freeze-safe)` again.

(defn- runtime-object?
  "True when `v` is a runtime-only object (function, var, or other
   runtime-internal object) that cannot be meaningfully serialized as
   data. These get a :vis/ref marker
   so the system knows to re-eval from the `expression` column to reconstruct them."
  [v]
  (or (fn? v)
      (instance? clojure.lang.Var v)
      (instance? java.util.concurrent.Future v)
      (some-> v
              class
              .getName
              (str/starts-with? "org.graalvm.polyglot."))))

(defn- freeze-safe
  "Prepare `v` for nippy serialization.

   Rules:
   - Realized collections (vectors, sets, maps, lists) -> walk recursively, freeze.
   - Lazy seqs -> `{:vis/ref :expr}`. A lazy seq IS a computation. Its durable
     form is the source code that produces it, not a materialized snapshot.
     Re-eval from :expr to reconstruct.
   - Functions, vars -> `{:vis/ref :expr}`. Same reason.
   - Plain scalars (strings, numbers, keywords, etc.) -> pass through
     at ANY depth. The depth limit is a safety against runaway recursion
     into self-referential collections; clipping scalars makes legitimate
     data (the canonical IR a tool render produces, deeply nested under
     forms-vec → channel → :result) lose its leaf text.

   Default depth raised to 32 — the per-form channel sink writes a
   canonical IR whose natural depth (forms → channel → :result → :ir →
   block → inline → inline-child → leaf) already eats 7 levels; the old
   depth-8 cap turned tool badges (`[:strong {} [:span {} \"LS\"]]`) into
   `[:strong {} [:vis/ref :depth-exceeded ...]]` after persistance, and
   restored bubbles painted no badge text."
  ([v] (freeze-safe v 32))
  ([v ^long depth]
   (cond (nil? v) nil
         (runtime-object? v) {:vis/ref :expr}
         (instance? clojure.lang.LazySeq v) {:vis/ref :expr}
         (map? v) (if (zero? depth)
                    {:vis/ref :depth-exceeded}
                    (persistent! (reduce-kv (fn [m k val]
                                              (assoc! m k (freeze-safe val (dec depth))))
                                            (transient {})
                                            v)))
         (vector? v)
         (if (zero? depth) {:vis/ref :depth-exceeded} (mapv #(freeze-safe % (dec depth)) v))
         (set? v) (if (zero? depth)
                    {:vis/ref :depth-exceeded}
                    (into #{} (map #(freeze-safe % (dec depth))) v))
         (list? v)
         (if (zero? depth) {:vis/ref :depth-exceeded} (doall (map #(freeze-safe % (dec depth)) v)))
         ;; A NON-lazy seq (ArraySeq from `sort`, Cons, ChunkedCons) is already
         ;; realized DATA — walk it like a list. Only LazySeq (caught above) is
         ;; a computation whose durable form is the source in :expr.
         (seq? v)
         (if (zero? depth) {:vis/ref :depth-exceeded} (doall (map #(freeze-safe % (dec depth)) v)))
         :else v)))

;; Per-session model preference (session_soul.llm_pref_provider + llm_pref_model)

(defn db-get-session-model-pref
  "The persisted model preference for a session (soul id) as
   `{:provider p :model m}`, or nil for the router default. Channel-neutral:
   web + TUI route through the same value."
  [db-info session-id]
  (when (and (ds db-info) session-id)
    (let
      [row
       (query-one! db-info
                   {:select [:llm_pref_provider :llm_pref_model]
                    :from :session_soul
                    :where [:= :id (->ref session-id)]})

       m
       (:llm_pref_model row)]

      (when m {:provider (:llm_pref_provider row) :model m}))))

(defn db-set-session-model-pref!
  "Persist (or clear, with both nil/blank) the PROVIDER + MODEL preference for
   a session (soul id). Lives on the stable soul, so it survives turn
   forks/retries."
  [db-info session-id provider model]
  (when (and (ds db-info) session-id)
    (let
      [m
       (some-> model
               str
               str/trim
               not-empty)

       p
       (some-> provider
               str
               str/trim
               not-empty)]

      (execute! db-info
                {:update :session_soul
                 :set {:llm_pref_provider (when m p) :llm_pref_model m}
                 :where [:= :id (->ref session-id)]}))
    nil))

;; The human's star (session_soul.favorite_rank)

(defn db-set-session-favorite!
  "Star (`true`) or unstar (`false`) the soul behind `session-id`. Returns the
   rank the session now holds - a long while starred, nil once it is not.

   The star is a RANK, not a boolean, because the clients pin starred sessions
   to the top of the list and that band needs a TOTAL order: two stars sharing a
   wall-clock millisecond would tie, and a tie is exactly how a deterministic
   order dies. The rank is allocated MAX+1 INSIDE the write transaction, so two
   devices starring at the same moment still get two different ranks. Ranks are
   only ever compared, so the gaps unstarring leaves behind cost nothing.

   Starring an ALREADY starred soul keeps its rank: the star is a state the
   human sets, not an event, so a retried request or a second tap racing the
   first must not reshuffle a band nobody touched."
  [db-info session-id is-favorite]
  (when (and (ds db-info) session-id)
    (sqlite-write-tx!
      db-info
      (fn [tx-info]
        (let
          [id
           (->ref session-id)

           cur
           (:favorite_rank
             (query-one! tx-info {:select [:favorite_rank] :from :session_soul :where [:= :id id]}))

           rank
           (cond (not is-favorite) nil
                 (some? cur) (long cur)
                 :else (inc (long (or (:maxrank (query-one! tx-info
                                                            {:select [[[:max :favorite_rank]
                                                                       :maxrank]]
                                                             :from :session_soul}))
                                      0))))]

          (execute! tx-info {:update :session_soul :set {:favorite_rank rank} :where [:= :id id]})
          rank)))))
;; Turn - session_turn_soul + session_turn_state

(defn- attachment-payload-cols
  "Payload columns for ONE attachment row: EXTERNAL (`:storage_uri` set,
   `:bytes` nil) when the map carries a `:storage-uri` (the storage-offload rail
   already wrote the bytes to a backend), else INLINE (`:bytes` = decoded
   base64, `:storage_uri` nil). Returns nil for an inline entry whose base64
   fails to decode, and for one whose decoded payload is past
   `attachments/max-stored-attachment-bytes` - the caller skips it (never aborts
   the enclosing insert), because a pathological artifact must cost its own row
   and nothing more: SQLite refuses a bound value over `SQLITE_MAX_LENGTH` with
   `[SQLITE_TOOBIG]`, which would take the whole iteration down with it.
   Satisfies the table's exactly-one(bytes, storage_uri) CHECK either way."
  [att]
  (if-let [uri (:storage-uri att)]
    {:storage_uri uri :bytes nil :size_bytes (long (or (:size att) 0))}
    (when-let
      [^bytes data (try (.decode (java.util.Base64/getDecoder) (str (:base64 att)))
                        (catch Throwable _ nil))]
      (when (<= (alength data) (long attachments/max-stored-attachment-bytes))
        {:bytes data :storage_uri nil :size_bytes (long (or (:size att) (alength data)))}))))

(defn- turn-session-soul-id
  "The SESSION soul a turn belongs to, resolved `session_turn_soul -> session_state
   -> session_soul_id`. Versioning is a SESSION-wide question - a re-attached name
   is the next cut of that artifact no matter which turn, iteration or branch
   emitted it - and the turn row only knows its state, so the writer takes this
   one hop before allocating. nil when the turn is unknown (no scope, version 1)."
  [db-info soul-id-s]
  (:session_soul_id (first (query! db-info
                                   {:select [:ss.session_soul_id]
                                    :from [[:session_turn_soul :ts]]
                                    :join [[:session_state :ss] [:= :ts.session_state_id :ss.id]]
                                    :where [:= :ts.id soul-id-s]
                                    :limit 1}))))

(defn- max-attachment-version
  "Highest `version` already stored for `filename` anywhere in the session owning
   `session-soul-id`, or 0 when that name is new. One indexed probe against
   `idx_attachment_version`, narrowed by the same soul join the session-wide
   lister uses."
  [db-info session-soul-id filename]
  (or (some-> (first (query! db-info
                             {:select [[[:max :a.version] :v]]
                              :from [[:session_attachment :a]]
                              :join [[:session_turn_soul :ts] [:= :a.session_turn_soul_id :ts.id]
                                     [:session_state :ss] [:= :ts.session_state_id :ss.id]]
                              :where [:and [:= :ss.session_soul_id session-soul-id]
                                      [:= :a.filename filename]]}))
              :v
              long)
      0))

(defn- version-allocator
  "A stateful `filename -> version` allocator for ONE insert batch.

   THE rule the whole feature rests on: attaching the SAME name again is the next
   VERSION of that artifact, never a new artifact. The first cut of a name is 1;
   every later cut is `1 + max` over the session. The batch remembers what it
   just handed out, so two same-named artifacts inside a single tool call become
   version N and N+1 rather than colliding on the stale database maximum.

   An anonymous attachment (no filename) has no identity to chain to and is
   always version 1."
  [db-info soul-id-s]
  (let
    [session-soul-id
     (turn-session-soul-id db-info soul-id-s)

     handed-out
     (volatile! {})]

    (fn [filename]
      (if (or (nil? session-soul-id) (nil? filename) (= "" filename))
        1
        (let
          [prev
           (or (get @handed-out filename) (max-attachment-version db-info session-soul-id filename))

           v
           (inc (long prev))]

          (vswap! handed-out assoc filename v)
          v)))))

(defn- store-turn-attachments!
  "Insert one `session_attachment` row per validated INBOUND user image attached
   to the turn - INLINE web/API uploads AND terminal-drop disk images alike.
   Each attachment is `{:media-type :base64 :size? :filename? :kind?}` (INLINE),
   or `{:media-type :storage-uri :size? :filename? :kind?}` when the offload rail
   parked its bytes in a storage backend. User rows leave
   `session_turn_iteration_id`/`tool_call_id` NULL - that NULL iteration IS what
   marks their `source` as `user`. Skips any inline entry whose base64 fails to
   decode - never aborts the enclosing turn insert."
  [tx-info soul-id-s attachments now]
  (let [next-version (version-allocator tx-info soul-id-s)]
    (doseq [[position att] (map-indexed vector (or attachments []))]
      (when-let [payload (attachment-payload-cols att)]
        (execute! tx-info
                  {:insert-into :session_attachment
                   :values [(merge {:id (str (new-uuid))
                                    :session_turn_soul_id soul-id-s
                                    :session_turn_iteration_id nil
                                    :tool_call_id nil
                                    :position position
                                    :kind (or (some-> (:kind att)
                                                      name)
                                              "image")
                                    :media_type (str (:media-type att))
                                    :filename (:filename att)
                                    :version (next-version (:filename att))
                                    :audience (attachments/normalize-audience (:audience att))
                                    :created_at now}
                                   payload)]})))))

(defn db-store-session-turn!
  "Create session_turn_soul + initial session_turn_state (version 0).

   `:attachments` - EVERY validated image attached to the turn `[{:media-type
   :base64 :size :filename}]`, both INLINE (web/API) uploads and terminal-drop
   disk images - persist as `session_attachment` BLOB rows (user rail: NULL
   iteration) so resume + history re-render survive a restart even after the source file moves or is
   deleted.

   Returns the session-turn-soul UUID."
  [db-info {:keys [parent-session-id user-request status attachments]}]
  (when (ds db-info)
    (sqlite-write-tx!
      db-info
      (fn [tx-info]
        (let
          [soul-id
           (new-uuid)

           state-id
           (new-uuid)

           now
           (now-ms)

           state-id-s
           (latest-state-id tx-info parent-session-id)

           turn-position
           (or (:next_position (query-one! tx-info
                                           {:select [[[:coalesce [:+ [:max :position] 1] 1]
                                                      :next_position]]
                                            :from :session_turn_soul
                                            :where [:= :session_state_id state-id-s]}))
               1)

           user-request-s
           (or user-request "")]

          (execute! tx-info
                    {:insert-into :session_turn_soul
                     :values [{:id (str soul-id)
                               :session_state_id state-id-s
                               :position turn-position
                               :user_request user-request-s
                               :created_at now}]})
          (execute! tx-info
                    {:insert-into :session_turn_state
                     :values [{:id (str state-id)
                               :session_turn_soul_id (str soul-id)
                               :version 0
                               :status (normalize-status (or status :running))
                               :created_at now}]})
          (store-turn-attachments! tx-info (str soul-id) attachments now)
          ;; A turn is intent: stamp the parent soul ADOPTED so a first-turn
          ;; session (e.g. a warm-pool tab the user just typed into) leaves the
          ;; unclaimed scaffolding state and becomes list-visible. Idempotent -
          ;; only flips a still-NULL claimed_at, a no-op on every later turn.
          (execute! tx-info
                    {:update :session_soul
                     :set {:claimed_at now}
                     :where [:and [:= :id (->ref parent-session-id)] [:= :claimed_at nil]]})
          soul-id)))))





(defn- row->attachment
  "Project ONE `session_attachment` row into the returned envelope. `:source` is
   DERIVED - a row carrying an iteration is a `tool` artifact, a NULL iteration
   is a `user` image (no enum column to drift). `:id` is the bare row uuid;
   read-back knocks the single table directly by it, no prefix, no dispatch."
  [row]
  (let [^bytes bs (:bytes row)]
    {:id (:id row)
     :source (if (:session_turn_iteration_id row) :tool :user)
     :tool-call-id (:tool_call_id row)
     :position (:position row)
     :kind (:kind row)
     :media-type (:media_type row)
     :filename (:filename row)
     ;; VERSION: same `:filename` inside a session = ONE artifact, iterated. 1 is
     ;; the first cut and the floor for anonymous rows, so a caller never has to
     ;; branch on nil to compare two versions of the same name.
     :version (long (or (:version row) 1))
     ;; AUDIENCE: who the artifact was attached FOR. "user" is stored + rendered
     ;; but never a wire image block; "model" is sent and never shown. Normalized
     ;; on read so the send-time gate never sees a legacy or NULL value.
     :audience (attachments/normalize-audience (:audience row))
     :storage-uri (:storage_uri row)
     :size (long (or (:size_bytes row) (when bs (alength bs)) 0))
     :base64 (when bs (.encodeToString (java.util.Base64/getEncoder) bs))}))


(defn db-list-turn-attachments
  "Ordered INBOUND user images persisted for one `session_turn_soul` (the `user`
   rail only - `session_turn_iteration_id IS NULL`). Returns `[{:id :source
   :position :kind :media-type :filename :size :base64}]` - `:id` is the bare row
   uuid for read-back via `db-read-attachment`, base64-encoded bytes match the
   shape the prompt assembler + web history re-render consume - or `[]` when
   none / no datasource. For user+tool combined see [[db-list-turn-all-attachments]]."
  [db-info session-turn-soul-id]
  (if-not (and (ds db-info) session-turn-soul-id)
    []
    (let [soul-id-s (->ref session-turn-soul-id)]
      (mapv row->attachment
            (query! db-info
                    {:select [:*]
                     :from :session_attachment
                     :where [:and [:= :session_turn_soul_id soul-id-s]
                             [:= :session_turn_iteration_id nil]]
                     :order-by [[:position :asc]]})))))

(defn db-list-turns-attachments
  "Batch variant of [[db-list-turn-attachments]]: INBOUND user images for MANY
   `session_turn_soul` ids in ONE query (the `user` rail only -
   `session_turn_iteration_id IS NULL`), grouped as `{soul-id-string [{:id
   :source :position :kind :media-type :filename :size :base64} …]}` (each vector
   ordered by `:position`). Missing ids are simply absent from the map. Safe: no
   ids / no datasource -> `{}`. Lets the gateway hydrate a whole session's user
   images without an N+1 per-turn query."
  [db-info session-turn-soul-ids]
  (let
    [ids (->> session-turn-soul-ids
              (keep #(some-> %
                             ->ref))
              distinct
              vec)]
    (if-not (and (ds db-info) (seq ids))
      {}
      (reduce (fn [m row]
                (update m (:session_turn_soul_id row) (fnil conj []) (row->attachment row)))
              {}
              (query! db-info
                      {:select [:*]
                       :from :session_attachment
                       :where [:and [:in :session_turn_soul_id ids]
                               [:= :session_turn_iteration_id nil]]
                       :order-by [[:session_turn_soul_id :asc] [:position :asc]]})))))

(defn db-list-iteration-attachments
  "Ordered OUTBOUND tool artifacts persisted for ONE `session_turn_iteration`
   (its id, not the soul - the `tool` rail only). Returns `[{:id :source
   :tool-call-id :position :kind :media-type :filename :size :base64}]` -
   base64-encoded inline bytes matching the shape the prompt assembler + web
   history re-render consume - ordered by `(tool_call_id, position)`, or `[]`
   when none / no datasource."
  [db-info iteration-id]
  (if-not (and (ds db-info) iteration-id)
    []
    (let [iter-id-s (->ref iteration-id)]
      (mapv row->attachment
            (query! db-info
                    {:select [:*]
                     :from :session_attachment
                     :where [:= :session_turn_iteration_id iter-id-s]
                     :order-by [[:tool_call_id :asc] [:position :asc]]})))))

(defn db-list-iterations-attachments
  "Batch variant of [[db-list-iteration-attachments]]: OUTBOUND tool artifacts
   for MANY `session_turn_iteration` ids in ONE query, grouped as
   `{iteration-id-string [{:id :source :tool-call-id :position :kind :media-type
   :filename :size :base64} …]}` (each vector ordered by `(tool_call_id,
   position)`). Missing ids are simply absent. Safe: no ids / no datasource ->
   `{}`. Lets a history replay hydrate a whole conversation's generated images
   without an N+1 per-iteration query."
  [db-info iteration-ids]
  (let
    [ids (->> iteration-ids
              (keep #(some-> %
                             ->ref))
              distinct
              vec)]
    (if-not (and (ds db-info) (seq ids))
      {}
      (reduce (fn [m row]
                (update m (:session_turn_iteration_id row) (fnil conj []) (row->attachment row)))
              {}
              (query! db-info
                      {:select [:*]
                       :from :session_attachment
                       :where [:in :session_turn_iteration_id ids]
                       :order-by [[:session_turn_iteration_id :asc] [:tool_call_id :asc]
                                  [:position :asc]]})))))

(def ^:private attachment-meta-select
  "Every `session_attachment` column EXCEPT the `bytes` BLOB, plus a computed
   `has_bytes` flag. A metadata read - the wire descriptors, a transcript
   projection, the byte endpoint's own index lookup - must never pay for the
   payload: `SELECT *` over a 20-figure iteration reads megabytes off disk and
   then base64-ENCODES every one of them into a String the caller throws away."
  [:id :session_turn_soul_id :session_turn_iteration_id :tool_call_id :position :kind :media_type
   :filename :version :audience :storage_uri :size_bytes
   [[:case [:= :bytes nil] 0 :else 1] :has_bytes]])

(defn- row->attachment-meta
  "[[row->attachment]] for a bytes-free row: the same envelope minus `:base64`,
   with `:has-bytes` in its place so a caller can still tell an inline blob from
   an external `:storage-uri` without reading either. `:size` stays exact -
   `size_bytes` is written on insert, never derived from the blob at read time."
  [row]
  (let [flag (:has_bytes row)]
    (-> (row->attachment row)
        (dissoc :base64)
        (assoc :has-bytes (if (number? flag) (pos? (long flag)) (true? flag))))))

(defn db-list-iteration-attachments-meta
  "[[db-list-iteration-attachments]] WITHOUT the bytes - the same rows in the
   same `(tool_call_id, position)` order, `:base64` replaced by `:has-bytes`.
   THE lister for everything that only NUMBERS or DESCRIBES an iteration's
   artifacts; the one blob a caller actually serves is then read by id with
   [[db-read-attachment]], so serving image N costs one blob and not all N."
  [db-info iteration-id]
  (if-not (and (ds db-info) iteration-id)
    []
    (let [iter-id-s (->ref iteration-id)]
      (mapv row->attachment-meta
            (query! db-info
                    {:select attachment-meta-select
                     :from :session_attachment
                     :where [:= :session_turn_iteration_id iter-id-s]
                     :order-by [[:tool_call_id :asc] [:position :asc]]})))))

(defn db-list-iterations-attachments-meta
  "Batch variant of [[db-list-iteration-attachments-meta]]: bytes-free rows for
   MANY `session_turn_iteration` ids in ONE query, grouped as
   `{iteration-id-string [{:id :source :tool-call-id :position :kind :media-type
   :filename :audience :size :has-bytes :storage-uri} …]}`. Missing ids are
   simply absent. Safe: no ids / no datasource -> `{}`. A transcript page reads
   every iteration's artifact LIST this way and stays byte-free."
  [db-info iteration-ids]
  (let
    [ids (->> iteration-ids
              (keep #(some-> %
                             ->ref))
              distinct
              vec)]
    (if-not (and (ds db-info) (seq ids))
      {}
      (reduce (fn [m row]
                (update m
                        (:session_turn_iteration_id row)
                        (fnil conj [])
                        (row->attachment-meta row)))
              {}
              (query! db-info
                      {:select attachment-meta-select
                       :from :session_attachment
                       :where [:in :session_turn_iteration_id ids]
                       :order-by [[:session_turn_iteration_id :asc] [:tool_call_id :asc]
                                  [:position :asc]]})))))

(defn db-list-turn-all-attachments
  "EVERY attachment hanging off one TURN - user images AND tool artifacts
   together - in ONE indexed filter (`session_turn_soul_id = ?`), the roll-up
   that single-table unification makes trivial. Tool rows denormalize the soul,
   so both rails answer the same `WHERE`. Returns `[{:id :source :tool-call-id
   :position :kind :media-type :filename :size :base64}]` ordered by `(source,
   position)` so user images lead, then tool artifacts - or `[]` when none / no
   datasource. Callers split by `:source` (`:user` / `:tool`) as needed."
  [db-info session-turn-soul-id]
  (if-not (and (ds db-info) session-turn-soul-id)
    []
    (let [soul-id-s (->ref session-turn-soul-id)]
      (->> (query! db-info
                   {:select [:*]
                    :from :session_attachment
                    :where [:= :session_turn_soul_id soul-id-s]
                    :order-by [[:position :asc]]})
           (mapv row->attachment)
           ;; user (no iteration) first, then tool, stable within each by position
           (sort-by (juxt #(if (= :user (:source %)) 0 1) :position))
           vec))))

(defn db-list-session-attachments
  "EVERY attachment across a whole SESSION - user images AND tool artifacts -
   scoped to the active branch's state chain (root->leaf). ONE join through
   `session_turn_soul` scopes `session_attachment` by the session's states;
   both rails answer because tool rows denormalize the soul. Returns [{:id
   :source :turn-soul-id :iteration-id :tool-call-id :position :kind :media-type
   :filename :size :storage-uri :base64}] ordered by (turn position, source
   [user first], position) so callers split by `:source` (`:user` / `:tool`) or
   slice per `:turn-soul-id` - or `[]` when none / no datasource. The
   whole-session roll-up sibling of [[db-list-turn-all-attachments]] (one turn)
   and [[db-list-iteration-attachments]] (one iteration); replaces the
   introspection reader's hand-rolled per-turn N+1 walk."
  [db-info session-id]
  (let [state-ids (session-state-chain db-info session-id)]
    (if-not (and (ds db-info) (seq state-ids))
      []
      (let [rank (zipmap state-ids (range))]
        (->> (query! db-info
                     {:select [:a.* [:ts.position :turn_position]
                               [:ts.session_state_id :turn_state_id]]
                      :from [[:session_attachment :a]]
                      :join [[:session_turn_soul :ts] [:= :a.session_turn_soul_id :ts.id]]
                      :where [:in :ts.session_state_id state-ids]})
             (sort-by (fn [r]
                        [(get rank (:turn_state_id r) Long/MAX_VALUE) (or (:turn_position r) 0)
                         (if (:session_turn_iteration_id r) 1 0) (or (:position r) 0)]))
             (mapv (fn [row]
                     (assoc (row->attachment row)
                       :turn-soul-id (:session_turn_soul_id row)
                       :iteration-id (:session_turn_iteration_id row))))
             vec)))))

(def ^:private session-attachment-meta-select
  "[[attachment-meta-select]] qualified to the `a` alias, plus the turn columns
   the whole-session roll-up orders by. Same rule: never the `bytes` BLOB — a
   session-wide INDEX must not read a session's worth of payload off disk."
  (into [[:a.id :id] [:a.session_turn_soul_id :session_turn_soul_id]
         [:a.session_turn_iteration_id :session_turn_iteration_id] [:a.tool_call_id :tool_call_id]
         [:a.position :position] [:a.kind :kind] [:a.media_type :media_type] [:a.filename :filename]
         [:a.version :version] [:a.audience :audience] [:a.storage_uri :storage_uri]
         [:a.size_bytes :size_bytes] [[:case [:= :a.bytes nil] 0 :else 1] :has_bytes]]
        [[:ts.position :turn_position] [:ts.session_state_id :turn_state_id]]))

(defn db-list-session-attachments-meta
  "[[db-list-session-attachments]] WITHOUT the bytes — the same rows in the same
   order, `:base64` replaced by `:has-bytes`. THE lister for a whole-session
   artifact INDEX: a client that wants to know everything a session produced
   asks once and pays metadata, then fetches the one blob it renders by id."
  [db-info session-id]
  (let [state-ids (session-state-chain db-info session-id)]
    (if-not (and (ds db-info) (seq state-ids))
      []
      (let [rank (zipmap state-ids (range))]
        (->> (query! db-info
                     {:select session-attachment-meta-select
                      :from [[:session_attachment :a]]
                      :join [[:session_turn_soul :ts] [:= :a.session_turn_soul_id :ts.id]]
                      :where [:in :ts.session_state_id state-ids]})
             (sort-by (fn [r]
                        [(get rank (:turn_state_id r) Long/MAX_VALUE) (or (:turn_position r) 0)
                         (if (:session_turn_iteration_id r) 1 0) (or (:position r) 0)]))
             (mapv (fn [row]
                     (assoc (row->attachment-meta row)
                       :turn-soul-id (:session_turn_soul_id row)
                       :iteration-id (:session_turn_iteration_id row))))
             vec)))))


(defn db-read-attachment
  "Read ONE persisted attachment by its bare row id (as returned by the
   `db-list-*-attachments` listers). There is exactly ONE `session_attachment`
   table, so this is a single indexed lookup - no source prefix, no dispatch, no
   cross-table fallback. `:source` is derived from the row (`:tool` when it
   carries an iteration, else `:user`). Returns `{:id :source :tool-call-id
   :position :kind :media-type :filename :size :storage-uri :base64}` (
   `:tool-call-id` nil for user images) or nil when the id is absent / no
   datasource. The read-back twin of the listers: a tool re-fetches an artifact
   it (or an earlier turn, or the user) produced."
  [db-info attachment-id]
  (when-let [id-s (and (ds db-info) (->ref attachment-id))]
    (when-let
      [row (query-one! db-info {:select [:*] :from :session_attachment :where [:= :id id-s]})]
      (row->attachment row))))

(defn- latest-session-turn-state
  [db-info session-turn-soul-id-s]
  (query-one! db-info
              {:select [:*]
               :from :session_turn_state
               :where [:and [:= :session_turn_soul_id session-turn-soul-id-s]
                       [:= :version
                        {:select [[[:max :version]]]
                         :from :session_turn_state
                         :where [:= :session_turn_soul_id session-turn-soul-id-s]}]]}))

(defn db-retry-session-turn!
  "Create a new session_turn_state (version N+1) for an existing session_turn_soul.
   Used when re-running a turn with a different provider/model or settings.
   Returns the new session-turn-state UUID."
  [db-info session-turn-soul-id {:keys [status provider model]}]
  (when (ds db-info)
    (sqlite-write-tx! db-info
                      (fn [tx-info]
                        (let
                          [soul-id-s
                           (->ref session-turn-soul-id)

                           current
                           (latest-session-turn-state tx-info soul-id-s)

                           new-id
                           (new-uuid)

                           now
                           (now-ms)]

                          (when current
                            (execute! tx-info
                                      {:insert-into :session_turn_state
                                       :values [(cond->
                                                  {:id (str new-id)
                                                   :session_turn_soul_id soul-id-s
                                                   :forked_from_session_turn_state_id (:id current)
                                                   :version (inc (long (:version current)))
                                                   :status (normalize-status (or status :running))
                                                   :llm_root_model model
                                                   :created_at now}
                                                  provider
                                                  (assoc :llm_root_provider
                                                    (name (->kw provider))))]})
                            new-id))))))

(defn db-update-session-turn!
  "Update the latest session_turn_state with final outcome.

   When `:prior-outcome` is provided (one of `:complete`, `:cancelled`,
   `:error`), it lands in the dedicated `prior_outcome` column so the next turn's handover digest can read
   it without scanning every iteration. The column is bounded by a
   CHECK constraint at the schema level."
  [db-info session-turn-id
   {:keys [content iteration-count duration-ms status tokens cost prior-outcome ctx error]}]
  (when (and (ds db-info) session-turn-id)
    (sqlite-write-tx!
      db-info
      (fn [tx-info]
        (let
          [soul-id-s
           (->ref session-turn-id)

           state
           (latest-session-turn-state tx-info soul-id-s)]

          (when state
            (execute!
              tx-info
              {:update :session_turn_state
               :set (cond->
                      {:status (normalize-status (or status :done))
                       :iteration_count (long (or iteration-count 0))
                       :duration_ms (long (or duration-ms 0))
                       ;; Phase B canonical columns. :input is TOTAL
                       ;; (Anthropic-additive raw values summed at the
                       ;; canonical-normalizer boundary in svar 0.6+).
                       ;; The :input-regular subset is derived from the
                       ;; invariant when the upstream map omits it
                       ;; (older accumulators don't track it explicitly):
                       ;;   regular = input - cache-write - cache-read
                       :input_tokens (long (or (get tokens "input") 0))
                       :input_regular_tokens
                       (long (or (get tokens "input_regular")
                                 (max 0
                                      (- (long (or (get tokens "input") 0))
                                         (long (or (get tokens "cache_created") 0))
                                         (long (or (get tokens "cached") 0))))))
                       :input_cache_write_tokens (long (or (get tokens "cache_created") 0))
                       :input_cache_read_tokens (long (or (get tokens "cached") 0))
                       :output_tokens (long (or (get tokens "output") 0))
                       :output_reasoning_tokens (long (or (get tokens "reasoning") 0))
                       :total_cost_usd (double (or (get cost "total_cost") 0.0))}
                      (get cost "model")
                      (assoc :llm_root_model (str (get cost "model")))

                      (get cost "provider")
                      (assoc :llm_root_provider (name (->kw (get cost "provider"))))

                      (some? content)
                      ;; The shared column codec, like every other JSON column
                      ;; here: raw `write-json-str` throws on a value charred
                      ;; refuses (int map keys, NaN) and takes the whole final
                      ;; outcome row — the settled answer — down with it.
                      (assoc :content_json (->json content))

                      prior-outcome
                      (assoc :prior_outcome (name prior-outcome))

                      ;; Nippy-encode the CTX snapshot as of end-of-turn.
                      ;; Live CTX = this row's ctx on the latest turn-state
                      ;; for the latest turn-soul; history = walking the
                      ;; soul chain.
                      (some? ctx)
                      (assoc :ctx (->blob (freeze-safe ctx)))

                      ;; First-class STRUCTURED terminal error (queryable),
                      ;; nippy-encoded like ctx — an error is not an answer.
                      (some? error)
                      (assoc :error (->blob (freeze-safe error))))
               :where [:= :id (:id state)]})))))))

;; Extra workflow persistence removed.

;; Iteration - session_turn_iteration table

(defn- require-iteration-code!
  [opts]
  (when-not (contains? opts :code)
    (throw (ex-info "db-store-iteration! requires flat :code"
                    {:type :vis.persistence/iteration-code-required :keys (keys opts)})))
  (:code opts))

(defn- prepare-iteration-columns
  "Prepare the session_turn_iteration payload. The canonical per-form vec
   lives Nippy-encoded in `:forms`; callers MUST pass it (or omit when the
   iter executed nothing). There is no legacy fallback — flat `:result` /
   `:error` are no longer accepted.

   `:duration-ms` is the Python sandbox eval wall time for this iteration's
   block; persisted into the named `eval_duration_ms` column. The LLM
   call wall time arrives as `:llm-full-duration-ms` and lands on
   `llm_full_duration_ms`.

   Preflight-only iterations (every form a synthetic
   `(vis/preflight-error ...)` source) blank the `:code` column so
   resumed bubbles don't render the synthetic source as success. The
   model-facing `:forms` envelopes keep the rejection text so context
   carry still teaches the next iter."
  [{:keys [forms duration-ms] :as opts}]
  (let
    [code
     (require-iteration-code! opts)

     preflight-only?
     (and (seq forms)
          (every? (fn [f]
                    (let
                      [s (some-> (:src f)
                                 str
                                 clojure.string/triml)]
                      (and s (clojure.string/starts-with? s "(vis/preflight-error"))))
                  forms))

     code-out
     (if preflight-only? "" (str code))]

    (cond-> {:code code-out}
      (seq forms)
      (assoc :tool_calls (->blob (freeze-safe (vec forms))))

      (some? duration-ms)
      (assoc :eval_duration_ms (long duration-ms)))))

(defn- routing-summary-columns
  [routing]
  (let
    [selected
     (:selected routing)

     actual
     (:actual routing)]

    (cond-> {}
      (:provider selected)
      (assoc :llm_selected_provider (name (:provider selected)))

      (:model selected)
      (assoc :llm_selected_model (:model selected))

      (:provider actual)
      (assoc :llm_actual_provider (name (:provider actual)))

      (:model actual)
      (assoc :llm_actual_model (:model actual))

      (contains? routing :fallback?)
      (assoc :is_llm_fallback (if (:fallback? routing) 1 0)))))

(defn- routing-event-row
  [iteration-id-s now position event]
  (let [event-type (:event/type event)]
    (cond->
      {:id (or (:event/id event) (new-id))
       :session_turn_iteration_id iteration-id-s
       :position position
       :event_type (str event-type)
       :event_json (->json event)
       :created_at now}
      (:provider event)
      (assoc :provider (name (:provider event)))

      (:model event)
      (assoc :model (:model event))

      (:from-provider event)
      (assoc :from_provider (name (:from-provider event)))

      (:from-model event)
      (assoc :from_model (:from-model event))

      (:to-provider event)
      (assoc :to_provider (name (:to-provider event)))

      (:to-model event)
      (assoc :to_model (:to-model event))

      (:status event)
      (assoc :status (long (:status event)))

      (:reason event)
      (assoc :reason (name (:reason event)))

      (:error event)
      (assoc :error (str (:error event)))

      (:attempt event)
      (assoc :attempt (long (:attempt event)))

      (:delay-ms event)
      (assoc :delay_ms (long (:delay-ms event)))

      (:elapsed-ms event)
      (assoc :elapsed_ms (long (:elapsed-ms event)))

      (:at-ms event)
      (assoc :at_ms (long (:at-ms event))))))

(defn- store-iteration-attachments!
  "Insert one `session_attachment` row per OUTBOUND artifact a tool call produced
   inside this iteration (the canonical case: a `matplotlib` figure emitted by
   `plt.show()`/`plt.savefig()`, or an `attach` payload, from
   `python_execution`). Each attachment is `{:tool-call-id? :media-type :base64
   :filename? :size? :kind?}` (INLINE), or `{... :storage-uri ...}` when the
   offload rail parked its bytes in a storage backend.

   Every row carries BOTH the owning iteration AND its turn soul (denormalized so
   a per-turn roll-up is one indexed filter); the set iteration IS what marks
   their `source` as `tool`. Grain is `(iteration, tool_call_id, position)` - so
   `position` is a 0-based ordinal RESET per `tool_call_id` group (one call may
   emit several figures, even same-named), matching the table's `UNIQUE`
   constraint. A `nil` tool-call-id (a whole-iteration artifact) forms its own
   group. Skips any inline entry whose base64 fails to decode - never aborts the
   enclosing iteration insert.

   IDENTITY COMES WITH THE ARTIFACT: an entry that already carries an `:id` is
   inserted under it, because the producer was handed that id the moment the
   bytes were captured (`mpl-capture/record-attachment!`) and may already have
   addressed the artifact by it, and the same holds for a stamped `:version`.
   Only an entry with neither gets a fresh uuid and the allocator's next cut."
  [tx-info soul-id-s iteration-id-s attachments now]
  (let [next-version (version-allocator tx-info soul-id-s)]
    (doseq [[_call-id group] (group-by :tool-call-id (or attachments []))]
      (doseq [[position att] (map-indexed vector group)]
        (when-let [payload (attachment-payload-cols att)]
          (execute! tx-info
                    {:insert-into :session_attachment
                     :values [(merge {:id (or (not-empty (some-> (:id att)
                                                                 str))
                                              (str (new-uuid)))
                                      :session_turn_soul_id soul-id-s
                                      :session_turn_iteration_id iteration-id-s
                                      :tool_call_id (some-> (:tool-call-id att)
                                                            str)
                                      :position position
                                      :kind (or (some-> (:kind att)
                                                        name)
                                                "image")
                                      :media_type (str (:media-type att))
                                      :filename (:filename att)
                                      ;; Same rule, computed at the sink when the
                                      ;; producer was told which cut this is.
                                      :version (or (:version att) (next-version (:filename att)))
                                      :audience (attachments/normalize-audience (:audience att))
                                      :created_at now}
                                     payload)]}))))))

(defn db-append-iteration-attachment!
  "Append ONE artifact to an EXISTING iteration - a human's own revision of a
   document the model produced (a markdown note annotated in the companion).

   Same table, same grain and the SAME version rule as
   [[store-iteration-attachments!]]: re-using a `:filename` is the NEXT CUT of
   that artifact, never a stranger beside it, so an annotated note lands as `v2`
   of the file it came from and the gallery threads the two. The row joins the
   iteration's `NULL` tool-call group (the one group no tool call owns) at the
   end of it, which keeps the table's `(iteration, tool_call_id, position)`
   UNIQUE constraint.

   The owning turn soul is read off the iteration's existing attachments: an
   artifact is only ever revised through the one it came from, so there is
   always a sibling row, and no assumption about the iteration table's own
   columns is baked in here.

   `att` is `{:media-type :base64 :filename? :kind? :audience?}`. Returns
   `{:id :version :position}` for the stored row, or nil when the iteration is
   unknown, has no artifacts, or the payload does not decode."
  [db-info iteration-id att]
  (when (and (ds db-info) iteration-id)
    (sqlite-write-tx!
      db-info
      (fn [tx-info]
        (let
          [iter-id-s
           (->ref iteration-id)

           soul-id-s
           (:session_turn_soul_id (first (query! tx-info
                                                 {:select [:session_turn_soul_id]
                                                  :from :session_attachment
                                                  :where [:= :session_turn_iteration_id iter-id-s]
                                                  :limit 1})))

           payload
           (attachment-payload-cols att)]

          (when (and soul-id-s payload)
            (let
              [version
               ((version-allocator tx-info soul-id-s) (:filename att))

               position
               (inc (long (or (:p (first (query! tx-info
                                                 {:select [[[:max :position] :p]]
                                                  :from :session_attachment
                                                  :where [:and
                                                          [:= :session_turn_iteration_id iter-id-s]
                                                          [:= :tool_call_id nil]]})))
                              -1)))

               id
               (str (new-uuid))]

              (execute! tx-info
                        {:insert-into :session_attachment
                         :values [(merge {:id id
                                          :session_turn_soul_id soul-id-s
                                          :session_turn_iteration_id iter-id-s
                                          :tool_call_id nil
                                          :position position
                                          :kind (or (some-> (:kind att)
                                                            name)
                                                    "doc")
                                          :media_type (str (:media-type att))
                                          :filename (:filename att)
                                          :version version
                                          :audience (attachments/normalize-audience
                                                      (or (:audience att) "user"))
                                          :created_at (now-ms)}
                                         payload)]})
              {:id id :version version :position position})))))))

#_{:clojure-lsp/ignore [:clojure-lsp/unused-public-var]}

(defn db-store-iteration!
  "Store one iteration row in a single SQLite transaction.

   The executed tool-call records travel on the iteration row's
   `tool_calls` BLOB; there is no sandbox-var sidecar persistence.
   Cross-turn references go through `:session/facts` and introspect-iter /
   introspect-turn (DB reads against `session_turn_iteration.tool_calls`).

   Returns the iteration UUID."
  ;; `:duration-ms` is consumed by `prepare-iteration-columns` and lands in
  ;; `eval_duration_ms`; kept off the outer :keys to stop clj-kondo's
  ;; unused-binding lint while staying documented here.
  [db-info
   {:keys [session-turn-id thinking assistant-prose answer llm-full-duration-ms error llm-routing
           cache-created-tokens llm-provider llm-model llm-returned-empty-code? tokens cost-usd
           attachments]
    :as opts}]
  (when (ds db-info)
    (sqlite-write-tx!
      db-info
      (fn [tx-info]
        (let
          [iteration-id
           (new-uuid)

           iteration-id-s
           (str iteration-id)

           now
           (now-ms)

           session-turn-soul-id-s
           (when session-turn-id (->ref session-turn-id))

           ;; Need session_turn_state_id (iteration FK points to session_turn_state)
           session-turn-state
           (when session-turn-soul-id-s (latest-session-turn-state tx-info session-turn-soul-id-s))

           session-turn-state-id-s
           (:id session-turn-state)

           ;; Compute position (1-indexed within this session_turn_state).
           ;; Next position is `MAX(position)+1` (monotonic and survives
           ;; row deletions), aliased as `:next_position` so the SQL
           ;; column name and the Clojure key line up. The
           ;; `UNIQUE (session_turn_state_id, position)` constraint
           ;; rejects any duplicate.
           position
           (or (:next_position (query-one!
                                 tx-info
                                 {:select [[[:coalesce [:+ [:max :position] 1] 1] :next_position]]
                                  :from :session_turn_iteration
                                  :where [:= :session_turn_state_id session-turn-state-id-s]}))
               1)

           ;; When the caller hands us only legacy :llm-provider /
           ;; :llm-model (no routing summary), synthesise an `actual`
           ;; routing record so the typed `llm_actual_*` columns stay
           ;; populated. This is the canonical landing spot for
           ;; "what provider/model answered".
           routing
           (or llm-routing
               (when (or llm-provider llm-model)
                 (cond-> {}
                   llm-provider
                   (assoc-in [:actual :provider] (->kw llm-provider))

                   llm-model
                   (assoc-in [:actual :model] (str llm-model)))))

           ;; 1. Iteration row - includes the single-form code payload inline.
           ;;    Hard cut: callers pass :code + :forms (Nippy vec of per-form envelopes).
           iteration-cols
           (prepare-iteration-columns opts)]

          (execute!
            tx-info
            {:insert-into :session_turn_iteration
             :values [(cond->
                        (merge {:id iteration-id-s
                                :session_turn_state_id session-turn-state-id-s
                                :position position
                                :status (normalize-status (cond answer :done
                                                                error :error
                                                                (:error iteration-cols) :error
                                                                :else :done))
                                :llm_thinking (str/trim (or thinking ""))
                                :llm_assistant_prose (str/trim (or assistant-prose ""))
                                :llm_full_duration_ms (long (or llm-full-duration-ms 0))
                                :is_llm_returned_empty_code (if llm-returned-empty-code? 1 0)
                                :created_at now
                                :finished_at now}
                               iteration-cols)
                        ;; Token / cost columns - omitted when nil so the
                        ;; row keeps NULL (the schema marks them nullable
                        ;; for exactly this reason: an LLM call that
                        ;; failed before returning usage produces no
                        ;; tokens, no cost, no fake zeros).
                        ;; Phase B canonical columns. :input is TOTAL,
                        ;; details (regular / cache-write / cache-read)
                        ;; are subsets obeying the canonical invariant.
                        (some? (get tokens "input"))
                        (assoc :input_tokens (long (get tokens "input")))

                        ;; :input-regular derived from invariant when absent
                        ;; on per-iter rows. The invariant is enforced at the
                        ;; canonical boundary in svar 0.6+, so this is a safe
                        ;; compute even when the accumulator did not surface it.
                        true
                        (assoc :input_regular_tokens
                          (long (or (get tokens "input_regular")
                                    (max 0
                                         (- (long (or (get tokens "input") 0))
                                            (long (or cache-created-tokens 0))
                                            (long (or (get tokens "cached") 0)))))))

                        (some? cache-created-tokens)
                        (assoc :input_cache_write_tokens (long cache-created-tokens))

                        (some? (get tokens "cached"))
                        (assoc :input_cache_read_tokens (long (get tokens "cached")))

                        (some? (get tokens "output"))
                        (assoc :output_tokens (long (get tokens "output")))

                        (some? (get tokens "reasoning"))
                        (assoc :output_reasoning_tokens (long (get tokens "reasoning")))

                        (some? cost-usd)
                        (assoc :cost_usd (double cost-usd))

                        (seq routing)
                        (merge (routing-summary-columns routing)))]})
          (store-iteration-attachments! tx-info
                                        session-turn-soul-id-s
                                        iteration-id-s
                                        attachments
                                        now)
          (doseq [[idx event] (map-indexed vector (:trace routing))]
            (execute! tx-info
                      {:insert-into :llm_routing_event
                       :values [(routing-event-row iteration-id-s now idx event)]}))
          iteration-id)))))

;; Read helpers

(defn- row->turn
  [row]
  (cond->
    {:id (->uuid (:soul_id row))
     :type :turn
     :session-state-id (->uuid (:session_state_id row))
     :position (:position row)
     :user-request (:user_request row)
     :status (->kw-back (:status row))
     :created-at (->date (:soul_created_at row))
     :iteration-count (long (or (:iteration_count row) 0))
     :duration-ms (long (or (:duration_ms row) 0))
     ;; Phase B canonical token shape. `:input-tokens` is TOTAL;
     ;; the detail keys are subsets obeying the invariant.
     :input-tokens (long (or (:input_tokens row) 0))
     :input-regular-tokens (long (or (:input_regular_tokens row) 0))
     :input-cache-write-tokens (long (or (:input_cache_write_tokens row) 0))
     :input-cache-read-tokens (long (or (:input_cache_read_tokens row) 0))
     :output-tokens (long (or (:output_tokens row) 0))
     :output-reasoning-tokens (long (or (:output_reasoning_tokens row) 0))
     :total-cost (double (or (:total_cost_usd row) 0.0))}
    ;; Turn rows carry no `title` column; `:name` is not populated
    ;; here. UI/display layers should use `:user-request` for a
    ;; turn label or read the session-level title via `:title`
    ;; on the session map.
    ;; (intentionally no `(:title row)` branch)
    (:content_json row)
    (assoc :content (json/read-json (:content_json row)))

    ;; First-class structured terminal error (nippy BLOB) for a failed turn —
    ;; channels render the error CARD from this instead of re-deriving it.
    (:error row)
    (assoc :error (<-blob (:error row)))

    (:llm_root_provider row)
    (assoc :provider (->kw-back (:llm_root_provider row)))

    (:llm_root_model row)
    (assoc :model (:llm_root_model row))))

(defn- session-turn-soul+state-query
  "HoneySQL fragment joining session_turn_soul + latest session_turn_state."
  [where-clause]
  ;; `:qs.title` intentionally absent: `session_turn_soul` has
  ;; no `title` column. Display layers fall back to `:user_request`
  ;; or the session-level title.
  {:select [:qs.id :qs.session_state_id :qs.position :qs.user_request
            [:qs.created_at :soul_created_at] [:qs.id :soul_id] :qst.status :qst.content_json
            :qst.iteration_count :qst.duration_ms
            ;; Phase B canonical columns on session_turn_state.
            :qst.input_tokens :qst.input_regular_tokens :qst.input_cache_write_tokens
            :qst.input_cache_read_tokens :qst.output_tokens :qst.output_reasoning_tokens
            :qst.total_cost_usd :qst.llm_root_provider :qst.llm_root_model :qst.error]
   :from [[:session_turn_soul :qs]]
   :join [[:session_turn_state :qst] [:= :qst.session_turn_soul_id :qs.id]]
   :where [:and where-clause
           [:= :qst.version
            {:select [[[:max :version]]]
             :from [[:session_turn_state :qst2]]
             :where [:= :qst2.session_turn_soul_id :qs.id]}]]})

(defn db-list-session-turns-by-status
  [db-info status]
  (if (ds db-info)
    (mapv row->turn
          (query! db-info
                  (session-turn-soul+state-query [:= :qst.status (normalize-status status)])))
    []))


(defn- attach-prior-outcome
  [row->turn-map]
  ;; Surface :prior-outcome on the turn map when the column has a value.
  ;; NULL columns surface as an absent key on the returned map.
  (fn [row]
    (cond-> (row->turn-map row)
      (:prior_outcome row)
      (assoc :prior-outcome (keyword (:prior_outcome row))))))

(defn db-list-session-turns
  [db-info session-id]
  (if (and (ds db-info) session-id)
    (let
      [state-ids
       (session-state-chain db-info session-id)

       state-rank
       (zipmap state-ids (range))]

      (if (seq state-ids)
        (mapv (attach-prior-outcome row->turn)
              (sort-by (fn [r]
                         [(get state-rank (:session_state_id r) Long/MAX_VALUE) (or (:position r) 0)
                          (or (:soul_created_at r) 0)])
                       (query! db-info
                               (-> (session-turn-soul+state-query [:in :qs.session_state_id
                                                                   state-ids])
                                   (update :select conj :qst.prior_outcome)))))
        []))
    []))

(defn db-fork-session-at-turn!
  "Fork a session UP TO AND INCLUDING the turn whose `session_turn_soul` id is
   `:through-turn-id`, into a brand-new INDEPENDENT session.

   Unlike [[db-fork-session!]] (which adds a new `session_state` UNDER the same
   soul, so the soul's latest-leaf chain keeps ALL prior turns), this mints a
   fresh `session_soul` + root `session_state` (version 0, `parent_state_id`
   NULL) and DEEP-COPIES every transcript turn from the start through
   `:through-turn-id` into it — each turn's `session_turn_soul`, its LATEST
   `session_turn_state`, that state's `session_turn_iteration` rows, and the
   turn's `session_attachment` rows (ids remapped). The SOURCE session is left
   completely untouched; the fork is a separate tab/session whose history is
   exactly those turns and which continues fresh from there.

   Copies rows generically (`SELECT *` → re-insert with remapped id/FK columns),
   so it stays correct as columns evolve. Required opt: `:workspace-id`. Returns
   the new SESSION SOUL UUID (for `resume-session`, which resolves sessions by
   `session_soul.id`), or nil when `:through-turn-id`
   is not a turn of the source session (or the env has no datasource)."
  [db-info session-id {:keys [title workspace-id through-turn-id]}]
  (when-not workspace-id
    (throw (ex-info "db-fork-session-at-turn! requires :workspace-id (1:1 invariant)"
                    {:type :persistance/missing-workspace-id})))
  (when-not through-turn-id
    (throw (ex-info "db-fork-session-at-turn! requires :through-turn-id"
                    {:type :persistance/missing-through-turn-id})))
  (when (ds db-info)
    (sqlite-write-tx!
      db-info
      (fn [tx-info]
        (let
          [soul-id-s
           (->ref session-id)

           src-soul
           (query-one! tx-info
                       {:select [:channel :owner_id] :from :session_soul :where [:= :id soul-id-s]})

           current
           (latest-state-for tx-info soul-id-s)

           turns
           (db-list-session-turns tx-info session-id)

           through-s
           (str through-turn-id)

           ;; Transcript-ordered turns from the start THROUGH the picked one.
           cut
           (reduce (fn [acc t]
                     (let [acc' (conj acc t)]
                       (if (= (str (:id t)) through-s) (reduced acc') acc')))
                   []
                   turns)]

          ;; Only fork when the picked turn actually belongs to the session
          ;; (`reduce` without a hit returns ALL turns — reject that case).
          (when (and src-soul current (seq cut) (= (str (:id (peek cut))) through-s))
            (let
              [new-soul-id
               (str (new-uuid))

               new-state-id
               (str (new-uuid))

               now
               (now-ms)

               fork-title
               (or title (str (:title current) " (fork)"))]

              ;; Fresh INDEPENDENT session soul (claimed = a real conversation).
              (execute! tx-info
                        {:insert-into :session_soul
                         :values [{:id new-soul-id
                                   :channel (:channel src-soul)
                                   :created_at now
                                   :owner_id (or (:owner_id src-soul) "local")
                                   :claimed_at now}]})
              ;; Root state: version 0, no parent — the fork's own history.
              (execute! tx-info
                        {:insert-into :session_state
                         :values [(cond->
                                    {:id new-state-id
                                     :session_soul_id new-soul-id
                                     :parent_state_id nil
                                     :workspace_id (->ref workspace-id)
                                     :title fork-title
                                     :version 0
                                     :system_prompt (or (:system_prompt current) "")
                                     :llm_root_model (or (:llm_root_model current) "")
                                     :created_at now}
                                    (:llm_root_provider current)
                                    (assoc :llm_root_provider
                                      (name (->kw (:llm_root_provider current)))))]})
              ;; Deep-copy each source turn into the fork's root state.
              (doseq [[idx turn] (map-indexed vector cut)]
                (let
                  [old-soul-s (->ref (:id turn))
                   new-turn-soul-id (str (new-uuid))
                   soul-row (query-one!
                              tx-info
                              {:select [:*] :from :session_turn_soul :where [:= :id old-soul-s]})
                   ts-row (query-one! tx-info
                                      {:select [:*]
                                       :from :session_turn_state
                                       :where [:and [:= :session_turn_soul_id old-soul-s]
                                               [:= :version
                                                {:select [[[:max :version]]]
                                                 :from :session_turn_state
                                                 :where [:= :session_turn_soul_id old-soul-s]}]]})
                   new-ts-id (str (new-uuid))
                   iters (when ts-row
                           (query! tx-info
                                   {:select [:*]
                                    :from :session_turn_iteration
                                    :where [:= :session_turn_state_id (:id ts-row)]
                                    :order-by [[:position :asc]]}))
                   iter-id-map (into {}
                                     (map (fn [it]
                                            [(:id it) (str (new-uuid))])
                                          iters))
                   atts (query! tx-info
                                {:select [:*]
                                 :from :session_attachment
                                 :where [:= :session_turn_soul_id old-soul-s]})]

                  (when soul-row
                    (execute! tx-info
                              {:insert-into :session_turn_soul
                               :values [(assoc soul-row
                                          :id new-turn-soul-id
                                          :session_state_id new-state-id
                                          :position (inc (long idx)))]}))
                  (when ts-row
                    (execute! tx-info
                              {:insert-into :session_turn_state
                               :values [(assoc ts-row
                                          :id new-ts-id
                                          :session_turn_soul_id new-turn-soul-id
                                          :version 0
                                          :forked_from_session_turn_state_id nil)]}))
                  (doseq [it iters]
                    (execute! tx-info
                              {:insert-into :session_turn_iteration
                               :values [(assoc it
                                          :id (get iter-id-map (:id it))
                                          :session_turn_state_id new-ts-id)]}))
                  ;; Attachments: user-rail rows (nil iteration) always copy; tool-rail
                  ;; rows copy only when their iteration was copied (latest state), with
                  ;; the iteration FK remapped. Skip tool rows from older versions.
                  (doseq
                    [a atts
                     :let [it-id (:session_turn_iteration_id a)]
                     :when (or (nil? it-id) (contains? iter-id-map it-id))]

                    (execute! tx-info
                              {:insert-into :session_attachment
                               :values [(assoc a
                                          :id (str (new-uuid))
                                          :session_turn_soul_id new-turn-soul-id
                                          :session_turn_iteration_id (some-> it-id
                                                                             iter-id-map))]}))))
              ;; Return the fork's SOUL id: resume-session/db-get-session key
              ;; sessions by session_soul.id, so a state id can never be reopened.
              new-soul-id)))))))

(defn- normalize-routing-event
  "THE one adapter between `event_json` (parsed with STRING keys — `<-json`
   never keywordizes) and the INTERNAL svar routing-event shape (keyword keys,
   keyword `:event/type`/`:reason`) that live traces use. DB-loaded events must
   render exactly like live ones; this is svar-telemetry internal shape, it
   never crosses the Python boundary."
  [event]
  (if-not (map? event)
    event
    (let
      [kv (into {}
                (map (fn [[k v]]
                       [(if (string? k) (keyword k) k) v]))
                event)]
      (cond-> kv
        (string? (:event/type kv))
        (assoc :event/type (keyword (:event/type kv)))

        (string? (:reason kv))
        (assoc :reason (keyword (:reason kv)))))))

(defn- row-routing-summary
  [row trace]
  (let
    [selected
     (cond-> {}
       (:llm_selected_provider row)
       (assoc :provider (->kw-back (:llm_selected_provider row)))

       (:llm_selected_model row)
       (assoc :model (:llm_selected_model row)))

     actual
     (cond-> {}
       (:llm_actual_provider row)
       (assoc :provider (->kw-back (:llm_actual_provider row)))

       (:llm_actual_model row)
       (assoc :model (:llm_actual_model row)))]

    (cond-> {}
      (seq selected)
      (assoc :selected selected)

      (seq actual)
      (assoc :actual actual)

      (some? (:is_llm_fallback row))
      (assoc :fallback? (= 1 (long (:is_llm_fallback row))))

      (seq trace)
      (assoc :trace trace))))

(defn- routing-events-for-iteration
  [db-info iteration-id]
  (try
    (mapv
      (fn [row]
        (normalize-routing-event
          (or (<-json (:event_json row))
              (cond->
                {:event/type (some-> (:event_type row)
                                     keyword)}
                (:provider row)
                (assoc :provider (->kw-back (:provider row)))

                (:model row)
                (assoc :model (:model row))

                (:from_provider row)
                (assoc :from-provider (->kw-back (:from_provider row)))

                (:from_model row)
                (assoc :from-model (:from_model row))

                (:to_provider row)
                (assoc :to-provider (->kw-back (:to_provider row)))

                (:to_model row)
                (assoc :to-model (:to_model row))

                (:status row)
                (assoc :status (:status row))

                (:reason row)
                (assoc :reason (->kw-back (:reason row)))

                (:error row)
                (assoc :error (:error row))

                (:attempt row)
                (assoc :attempt (:attempt row))

                (:delay_ms row)
                (assoc :delay-ms (:delay_ms row))

                (:elapsed_ms row)
                (assoc :elapsed-ms (:elapsed_ms row))

                (:at_ms row)
                (assoc :at-ms (:at_ms row))))))
      (query! db-info
              {:select [:*]
               :from :llm_routing_event
               :where [:= :session_turn_iteration_id (->ref iteration-id)]
               :order-by [[:position :asc]]}))
    (catch SQLException _ [])))

(defn- attach-routing
  [iteration routing]
  (if (seq routing)
    (cond-> iteration
      (:selected routing)
      (assoc :llm-selected (:selected routing))

      (:actual routing)
      (assoc :llm-actual (:actual routing))

      (contains? routing :fallback?)
      (assoc :llm-fallback? (:fallback? routing))

      (seq (:trace routing))
      (assoc :llm-routing-trace (vec (:trace routing))))
    iteration))

(defn- row->iteration
  [row]
  ;; DB column is `tool_calls`; the in-memory iteration key stays `:forms`
  ;; (the executed tool-call records the resume/replay path reads).
  (let [forms-vec (<-blob (:tool_calls row))]
    (cond->
      {:id (->uuid (:id row))
       :type :iteration
       :position (:position row)
       :status (->kw-back (:status row))
       :created-at (->date (:created_at row))}
      (some? (:code row))
      (assoc :code (:code row))

      (some? forms-vec)
      (assoc :forms forms-vec)

      (some? (:eval_duration_ms row))
      (assoc :duration-ms (:eval_duration_ms row))

      (some? (:llm_thinking row))
      (assoc :thinking (:llm_thinking row))

      (not (str/blank? (str (:llm_assistant_prose row))))
      (assoc :assistant-prose (:llm_assistant_prose row))

      (some? (:finished_at row))
      (assoc :finished-at (->date (:finished_at row)))

      ;; `:provider` / `:model` on the iteration map mirror what actually
      ;; answered (post-fallback). They are aliases of the typed
      ;; `llm_actual_*` columns; consumers that want the router's
      ;; pre-call selection should read `:llm-selected-provider/model`.
      (some? (:llm_actual_provider row))
      (assoc :provider (->kw-back (:llm_actual_provider row)))

      (some? (:llm_actual_model row))
      (assoc :model (:llm_actual_model row))

      (some? (:llm_selected_provider row))
      (assoc :llm-selected-provider (->kw-back (:llm_selected_provider row)))

      (some? (:llm_selected_model row))
      (assoc :llm-selected-model (:llm_selected_model row))

      (some? (:llm_actual_provider row))
      (assoc :llm-actual-provider (->kw-back (:llm_actual_provider row)))

      (some? (:llm_actual_model row))
      (assoc :llm-actual-model (:llm_actual_model row))

      (some? (:is_llm_fallback row))
      (assoc :llm-fallback? (= 1 (long (:is_llm_fallback row))))

      (some? (:is_llm_returned_empty_code row))
      (assoc :returned-empty-code? (= 1 (long (:is_llm_returned_empty_code row))))

      ;; Token / cost columns - ALWAYS present on the read side, with
      ;; sane numeric defaults (0 tokens, $0.00 cost) when the column
      ;; is NULL. Callers can assume `(:input-tokens it)` is a long
      ;; and `(:cost-usd it)` is a double without `or`-padding at
      ;; every use site. The schema CHECK still rejects negative
      ;; values; absent = zero by convention.
      ;; Phase B canonical token shape. `:input-tokens` is TOTAL,
      ;; details (`:input-regular`, `:input-cache-write`,
      ;; `:input-cache-read`) are subsets obeying the invariant
      ;; (regular + write + read = total). `:output-reasoning` is a
      ;; subset of `:output-tokens`.
      true
      (assoc :input-tokens (long (or (:input_tokens row) 0)))

      true
      (assoc :input-regular-tokens (long (or (:input_regular_tokens row) 0)))

      true
      (assoc :input-cache-write-tokens (long (or (:input_cache_write_tokens row) 0)))

      true
      (assoc :input-cache-read-tokens (long (or (:input_cache_read_tokens row) 0)))

      true
      (assoc :output-tokens (long (or (:output_tokens row) 0)))

      true
      (assoc :output-reasoning-tokens (long (or (:output_reasoning_tokens row) 0)))

      true
      (assoc :cost-usd (double (or (:cost_usd row) 0.0))))))

(defn- iterations-for-state-id
  "Iteration views for one concrete `session_turn_state.id`, position-ordered."
  [db-info state-id-s]
  (mapv (fn [row]
          (let
            [trace
             (routing-events-for-iteration db-info (:id row))

             routing
             (row-routing-summary row trace)]

            (attach-routing (row->iteration row) routing)))
        (query! db-info
                {:select [:*]
                 :from :session_turn_iteration
                 :where [:= :session_turn_state_id state-id-s]
                 :order-by [[:position :asc]]})))

(defn db-list-session-turn-iterations
  [db-info session-turn-id]
  ;; `session-turn-id` arrives as EITHER a `session_turn_soul` id (the
  ;; canonical turn id `row->turn`/`db-turn-history` expose, the one history
  ;; views carry) OR a concrete `session_turn_state` id (the engine's
  ;; `:session-turn-id` run result, surfaced to the web as `:engine_turn_id`
  ;; on freshly-finished live turns). Resolve soul -> latest state first; if
  ;; no soul matches (or it has no iterations), treat the id AS a state id.
  ;; The two id spaces are independent random UUIDs, so this never crosses
  ;; wires — it just makes machinery restore work for both callers.
  (if (and (ds db-info) session-turn-id)
    (let
      [id-s
       (->ref session-turn-id)

       state
       (latest-session-turn-state db-info id-s)

       via-soul
       (when state (iterations-for-state-id db-info (:id state)))]

      (if (seq via-soul) via-soul (iterations-for-state-id db-info id-s)))
    []))

;; `db-list-iteration-vars`, `db-latest-var-registry`, `db-var-history*`,
;; `db-store-dependency!`, `db-list-dependencies`, `db-restore-blocks`,
;; and `latest-visible-definition-rows` were retired together with the
;; `definition_*` sidecar tables.
;; Executed tool-call records live on `session_turn_iteration.tool_calls`;
;; cross-turn references flow through `:session/facts` and `introspect-iter` /
;; `introspect-turn` (DB reads against that BLOB).

(defn db-turn-history
  "Per-turn history rows with canonical typed `:content`."
  [db-info session-id]
  (let [turns (db-list-session-turns db-info session-id)]
    (mapv (fn [idx turn]
            (let
              [turn-ref (:id turn)
               iteration-count (count (db-list-session-turn-iterations db-info turn-ref))]

              (cond->
                {:turn-pos idx
                 :session-turn-id (:id turn)
                 :created-at (:created-at turn)
                 :user-request (:user-request turn)
                 :status (:status turn)
                 :iteration-count iteration-count}
                (seq (:content turn))
                (assoc :content (:content turn)))))
          (range)
          turns)))

;; Extension aggregate sidecars

(defn- ->edn-text [v] (pr-str v))

(defn- <-edn-text [s] (when s (try (edn/read-string s) (catch Throwable _ s))))

(defn- extension-aggregate-sql-row
  [opts id now]
  (let
    [row {:id id
          :extension_id (str (:extension-id opts))
          :aggregate_key (->edn-text (:aggregate-key opts))
          :kind (->edn-text (:kind opts))
          :index_data (->json (:index-data opts))
          :content (->blob (:content opts))
          :session_soul_id (some-> (:session-soul-id opts)
                                   ->ref)
          :session_state_id (some-> (:session-state-id opts)
                                    ->ref)
          :session_turn_state_id (some-> (:session-turn-state-id opts)
                                         ->ref)
          :session_turn_iteration_id (some-> (:iteration-id opts)
                                             ->ref)
          :session_turn_iteration_block_index (:iteration-form-index opts)
          :session_turn_iteration_block_id (some-> (:iteration-block-id opts)
                                                   str)
          :created_at now
          :updated_at now}]
    (when (str/blank? (:extension_id row))
      (throw (ex-info "extension aggregate requires extension-id"
                      {:type :extension-aggregate/missing-required :key :extension-id})))
    (when (nil? (:aggregate-key opts))
      (throw (ex-info "extension aggregate requires aggregate-key"
                      {:type :extension-aggregate/missing-required :key :aggregate-key})))
    (when (nil? (:kind opts))
      (throw (ex-info "extension aggregate requires kind"
                      {:type :extension-aggregate/missing-required :key :kind})))
    (when (and (or (:session_turn_iteration_block_index row) (:session_turn_iteration_block_id row))
               (nil? (:session_turn_iteration_id row)))
      (throw (ex-info "extension aggregate block scope requires iteration-id"
                      {:type :extension-aggregate/block-without-iteration})))
    row))

(defn- row->extension-aggregate
  [row]
  (when row
    (let
      [scope
       (cond-> {}
         (:session_soul_id row)
         (assoc :session-soul-id (:session_soul_id row))

         (:session_state_id row)
         (assoc :session-state-id (:session_state_id row))

         (:session_turn_state_id row)
         (assoc :session-turn-state-id (:session_turn_state_id row))

         (:session_turn_iteration_id row)
         (assoc :iteration-id (:session_turn_iteration_id row))

         (:session_turn_iteration_block_index row)
         (assoc :iteration-form-index (:session_turn_iteration_block_index row))

         (:session_turn_iteration_block_id row)
         (assoc :iteration-block-id (:session_turn_iteration_block_id row)))

       aggregate-key
       (<-edn-text (:aggregate_key row))]

      {:id (:id row)
       :extension-id (:extension_id row)
       :aggregate-key aggregate-key
       :key aggregate-key
       :kind (<-edn-text (:kind row))
       :scope scope
       :index-data (<-json (:index_data row))
       :content (<-blob (:content row))
       :created-at (->date (:created_at row))
       :updated-at (->date (:updated_at row))})))

(defn- extension-aggregate-clauses
  [opts]
  (cond-> []
    (:id opts)
    (conj [:= :id (->ref (:id opts))])

    (:extension-id opts)
    (conj [:= :extension_id (str (:extension-id opts))])

    (contains? opts :aggregate-key)
    (conj [:= :aggregate_key (->edn-text (:aggregate-key opts))])

    (contains? opts :kind)
    (conj [:= :kind (->edn-text (:kind opts))])

    (:session-soul-id opts)
    (conj [:= :session_soul_id (->ref (:session-soul-id opts))])

    (:session-state-id opts)
    (conj [:= :session_state_id (->ref (:session-state-id opts))])

    (:session-turn-state-id opts)
    (conj [:= :session_turn_state_id (->ref (:session-turn-state-id opts))])

    (:iteration-id opts)
    (conj [:= :session_turn_iteration_id (->ref (:iteration-id opts))])

    (contains? opts :iteration-form-index)
    (conj [:= :session_turn_iteration_block_index (:iteration-form-index opts)])

    (:iteration-block-id opts)
    (conj [:= :session_turn_iteration_block_id (str (:iteration-block-id opts))])

    ;; index_data JSON field filtering.
    ;; :index-data in a query is a map of {field-key expected-value} pairs.
    ;; Each pair becomes a json_extract WHERE clause. This lets extensions
    ;; run structured queries over their index sidecar without custom tables.
    ;; Example: (extension-list-aggregates env {:kind :bridge/edge :index-data {:source "core/run"}})
    (and (map? (:index-data opts)) (seq (:index-data opts)))
    (into (for [[k v] (:index-data opts)]
            [:= [:json_extract :index_data (str "$.\"" (name k) "\"")] (str v)]))))

(defn- extension-aggregate-select
  [opts]
  (let [clauses (extension-aggregate-clauses opts)]
    (cond->
      {:select [:*] :from :extension_aggregate :order-by [[:updated_at :desc] [:created_at :desc]]}
      (seq clauses)
      (assoc :where (into [:and] clauses))

      (pos-int? (:limit opts))
      (assoc :limit (:limit opts))

      (nat-int? (:offset opts))
      (assoc :offset (:offset opts)))))

(defn db-create-extension-aggregate!
  [db-info opts]
  (when (ds db-info)
    (sqlite-write-tx!
      db-info
      (fn [tx-info]
        (let
          [id
           (new-id)

           now
           (now-ms)]

          (execute! tx-info
                    {:insert-into :extension_aggregate
                     :values [(extension-aggregate-sql-row opts id now)]})
          (row->extension-aggregate
            (query-one! tx-info {:select [:*] :from :extension_aggregate :where [:= :id id]})))))))

(defn db-put-extension-aggregate!
  [db-info opts]
  (when (ds db-info)
    (sqlite-write-tx!
      db-info
      (fn [tx-info]
        (let
          [id
           (new-id)

           now
           (now-ms)

           row
           (extension-aggregate-sql-row opts id now)]

          (execute! tx-info
                    {:insert-into :extension_aggregate
                     :values [row]
                     :on-conflict [:extension_id :aggregate_key :kind :scope_key]
                     :do-update-set
                     {:index_data (:index_data row)
                      :content (:content row)
                      :session_soul_id (:session_soul_id row)
                      :session_state_id (:session_state_id row)
                      :session_turn_state_id (:session_turn_state_id row)
                      :session_turn_iteration_id (:session_turn_iteration_id row)
                      :session_turn_iteration_block_index (:session_turn_iteration_block_index row)
                      :session_turn_iteration_block_id (:session_turn_iteration_block_id row)
                      :updated_at now}})
          (row->extension-aggregate
            (query-one! tx-info
                        {:select [:*]
                         :from :extension_aggregate
                         :where [:and [:= :extension_id (:extension_id row)]
                                 [:= :aggregate_key (:aggregate_key row)] [:= :kind (:kind row)]
                                 (if (:session_soul_id row)
                                   [:= :session_soul_id (:session_soul_id row)]
                                   [:is :session_soul_id nil])
                                 (if (:session_state_id row)
                                   [:= :session_state_id (:session_state_id row)]
                                   [:is :session_state_id nil])
                                 (if (:session_turn_state_id row)
                                   [:= :session_turn_state_id (:session_turn_state_id row)]
                                   [:is :session_turn_state_id nil])
                                 (if (:session_turn_iteration_id row)
                                   [:= :session_turn_iteration_id (:session_turn_iteration_id row)]
                                   [:is :session_turn_iteration_id nil])
                                 (if (:session_turn_iteration_block_index row)
                                   [:= :session_turn_iteration_block_index
                                    (:session_turn_iteration_block_index row)]
                                   [:is :session_turn_iteration_block_index nil])
                                 (if (:session_turn_iteration_block_id row)
                                   [:= :session_turn_iteration_block_id
                                    (:session_turn_iteration_block_id row)]
                                   [:is :session_turn_iteration_block_id nil])]})))))))

(defn db-get-extension-aggregate
  [db-info opts]
  (when (ds db-info)
    (row->extension-aggregate (query-one! db-info
                                          (assoc (extension-aggregate-select opts) :limit 1)))))

(defn db-list-extension-aggregates
  [db-info opts]
  (if (ds db-info)
    (mapv row->extension-aggregate
          (query! db-info
                  (extension-aggregate-select (cond-> opts
                                                (not (:limit opts))
                                                (assoc :limit 100)))))
    []))

(defn db-delete-extension-aggregates!
  [db-info opts]
  (when (ds db-info)
    (sqlite-write-tx! db-info
                      (fn [tx-info]
                        (let
                          [clauses
                           (extension-aggregate-clauses opts)

                           result
                           (jdbc/execute! (ds tx-info)
                                          (sql/format (cond-> {:delete-from :extension_aggregate}
                                                        (seq clauses)
                                                        (assoc :where (into [:and] clauses)))))]

                          (or (:next.jdbc/update-count (first result)) 0))))))

(defn db-swap-extension-aggregate!
  [db-info opts f args]
  (when (ds db-info)
    (sqlite-write-tx! db-info
                      (fn [tx-info]
                        (let
                          [current
                           (db-get-extension-aggregate tx-info opts)

                           next-content
                           (apply f (:content current) args)]

                          (db-put-extension-aggregate! tx-info
                                                       (assoc opts
                                                         :index-data (or (:index-data opts)
                                                                         (:index-data current))
                                                         :content next-content)))))))

;; CTX snapshots (per-turn :session/* state)

(defn db-load-latest-ctx
  "Load the CTX snapshot (Nippy BLOB) from the latest session_turn_state
   that has a non-NULL ctx column, scoped to this session_state. Returns
   the decoded CTX map or nil when the session has no persisted CTX yet.
   STRING-KEYED: ctx blobs are frozen string-keyed (strings-only end to end).

   This is the resume path: on a new turn, the loop reads this back into
   the ctx-atom so the model picks up where (done …) left off. The cursor
   is intentionally NOT restored — it's iter-local and gets stamped fresh
   by the renderer."
  [db-info session-id]
  (when (and (ds db-info) session-id)
    (let [state-ids (session-state-chain db-info session-id)]
      (when (seq state-ids)
        (when-let
          [row (first (query! db-info
                              {:select [:qts.ctx]
                               :from [[:session_turn_state :qts]]
                               :join [[:session_turn_soul :qs]
                                      [:= :qs.id :qts.session_turn_soul_id]]
                               :where [:and [:in :qs.session_state_id state-ids] [:<> :qts.ctx nil]]
                               :order-by [[:qs.position :desc] [:qts.version :desc]]
                               :limit 1}))]
          (<-blob (:ctx row)))))))

;; Backend registration
;;
;; The extension is registered by the lightweight
;; `com.blockether.vis.ext.persistance-sqlite.registrar` namespace, which
;; manifest discovery loads on every Vis startup. That registrar declares
;; `:persistance/ns` as THIS ns; the persistance facade calls
;; `requiring-resolve` on backend fns, so this heavyweight ns is loaded
;; on demand on the first real DB op (and not at all when the user runs
;; commands that never touch the DB - see `registrar.clj` for the
;; rationale and the load-cost numbers).
;;
;; Tests that need the SQLite backend registered should require the
;; `registrar` ns; tests that only need the fns can require this ns
;; directly without registering the extension.

(defn db-load-ctx-history
  "Return a sorted-by-turn vec of `[turn-n ctx-map]` pairs for the session.
   Each ctx-map is the Nippy-decoded `:session/turn_state.ctx` for the
   latest version of that turn. Used by introspect-* verbs to reach
   archived entries and replay any past turn snapshot through the engine's
   pure-fn introspect-spec / -task / -fact / -archived / -ctx-at helpers.

   The session_state chain is honoured (forks see ancestor snapshots
   ordered by parent_state -> child_state and then by turn position)."
  [db-info session-id]
  (when (and (ds db-info) session-id)
    (let [state-ids (session-state-chain db-info session-id)]
      (when (seq state-ids)
        (vec (for
               [row (query! db-info
                            {:select [:qs.position [:qts.ctx :ctx]]
                             :from [[:session_turn_state :qts]]
                             :join [[:session_turn_soul :qs] [:= :qs.id :qts.session_turn_soul_id]]
                             :where [:and [:in :qs.session_state_id state-ids] [:<> :qts.ctx nil]
                                     [:= :qts.version
                                      {:select [[[:max :version]]]
                                       :from [[:session_turn_state :qts2]]
                                       :where [:= :qts2.session_turn_soul_id
                                               :qts.session_turn_soul_id]}]]
                             :order-by [[:qs.position :asc]]})
                :let [decoded (<-blob (:ctx row))]
                :when (some? decoded)]

               [(long (:position row)) decoded]))))))
(defn- fts5-quote [value] (str "\"" (str/replace (str value) "\"" "\"\"") "\""))

(declare render-search-query)

(defn- render-search-join
  [operator nodes]
  (when-not (seq nodes)
    (throw (ex-info "search boolean node needs at least one child" {:operator operator})))
  (str "(" (str/join (str " " operator " ") (map render-search-query nodes)) ")"))

(defn- render-search-query
  "Render the backend-neutral search DSL as a safely quoted FTS5 expression."
  [query]
  (cond (string? query) (let [terms (remove str/blank? (str/split (str/trim query) #"\s+"))]
                          (when-not (seq terms)
                            (throw (ex-info "search query must contain text" {:query query})))
                          (if (= 1 (count terms))
                            (fts5-quote (first terms))
                            (str "(" (str/join " AND " (map fts5-quote terms)) ")")))
        (map? query)
        (let [{:keys [term phrase prefix all any near]} query]
          (cond (contains? query :term) (fts5-quote term)
                (contains? query :phrase) (fts5-quote phrase)
                (contains? query :prefix) (str (fts5-quote prefix) "*")
                (contains? query :near)
                (let [{:keys [terms within]} near]
                  (when-not (seq terms) (throw (ex-info "search :near needs terms" {:query query})))
                  (str "NEAR(" (str/join " " (map fts5-quote terms)) ", " (long within) ")"))
                (contains? query :any) (render-search-join "OR" any)
                (contains? query :all) (let
                                         [negative? #(and (map? %) (contains? % :not))
                                          positive (remove negative? all)
                                          negative (map :not (filter negative? all))]

                                         (when-not (seq positive)
                                           (throw (ex-info "search :all needs a positive child"
                                                           {:query query})))
                                         (reduce #(str %1 " NOT " (render-search-query %2))
                                                 (render-search-join "AND" positive)
                                                 negative))
                (contains? query :not) (throw (ex-info "search :not must be a child of :all"
                                                       {:query query}))
                :else (throw (ex-info "unrecognized search-query node" {:query query}))))
        :else (throw (ex-info "search query must be a string or a DSL map" {:query query}))))

(def ^:private searchable-transcript-fields
  [{:owner-table "session_turn_soul"
    :field "user_request"
    :table "transcript_request_fts"
    :column "user_request"
    :owner-alias "ts"
    :column-index 0}
   {:owner-table "session_turn_iteration"
    :field "answer_text"
    :table "transcript_reply_fts"
    :column "llm_assistant_prose"
    :owner-alias "it"
    :column-index 0}
   {:owner-table "session_turn_iteration"
    :field "thinking_text"
    :table "transcript_reply_fts"
    :column "llm_thinking"
    :owner-alias "it"
    :column-index 1}])

(defn- search-field-sql
  [{:keys [owner-table field table owner-alias column-index]}]
  (str "SELECT '"
       owner-table
       "' AS owner_table, "
       owner-alias
       ".id AS owner_id, '"
       field
       "' AS field, snippet("
       table
       ", "
       column-index
       ", '[', ']', '…', 32) AS snippet, bm25("
       table
       ") AS rank FROM "
       table
       " JOIN "
       owner-table
       " "
       owner-alias
       " ON "
       owner-alias
       ".rowid = "
       table
       ".rowid WHERE "
       table
       " MATCH ? ORDER BY rank ASC LIMIT ?"))

(defn db-search
  "Search persisted prompts, answers, and thinking through SQLite FTS5."
  ([db-info query] (db-search db-info query nil))
  ([db-info query {:keys [limit owner-table field]}]
   (if (or (not (ds db-info))
           (and (string? query) (str/blank? query))
           (and (map? query) (empty? query)))
     []
     (let
       [match-query
        (render-search-query query)

        result-limit
        (max 1 (long (or limit 50)))

        sources
        (filter #(and (or (nil? owner-table) (= owner-table (:owner-table %)))
                      (or (nil? field) (= field (:field %))))
                searchable-transcript-fields)]

       (->> sources
            (mapcat (fn [{:keys [column] :as source}]
                      (raw-query! db-info
                                  [(search-field-sql source) (str column " : (" match-query ")")
                                   result-limit])))
            (map (fn [row]
                   {:owner-table (:owner_table row)
                    :owner-id (:owner_id row)
                    :field (:field row)
                    :snippet (:snippet row)
                    :rank (:rank row)}))
            (sort-by :rank)
            (take result-limit)
            vec)))))