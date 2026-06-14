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
  (:require
   [charred.api :as json]
   [clojure.edn :as edn]
   [clojure.string :as str]
   [com.blockether.vis.ext.persistance-sqlite.migration :as migration]
   [com.blockether.vis.core :as vis]
   [honey.sql :as sql]
   [next.jdbc :as jdbc]
   [next.jdbc.result-set :as rs]
   [taoensso.nippy :as nippy]
   [taoensso.telemere :as tel])
  (:import
   (com.zaxxer.hikari HikariConfig HikariDataSource)
   (java.io File RandomAccessFile)
   (java.nio.channels FileLock)
   (java.nio.file Files LinkOption Paths)
   (java.security MessageDigest)
   (java.sql SQLException)
   (java.util.concurrent.atomic AtomicLong)
   (javax.sql DataSource)
   (org.sqlite SQLiteConfig SQLiteConfig$JournalMode SQLiteConfig$SynchronousMode
     SQLiteConfig$TransactionMode SQLiteDataSource)))

;; =============================================================================
;; Helpers
;; =============================================================================

(def ds      vis/ds)
(def now-ms  vis/now-ms)
(def ->id    vis/->id)
(def ->uuid  vis/->uuid)
(def ->ref   vis/->ref)
(def ->kw    vis/->kw)
(def ->kw-back vis/->kw-back)
(def ->date  vis/->date)

(defn- new-uuid []
  (->uuid (str (java.util.UUID/randomUUID))))

(defn- new-id []
  (->id (new-uuid)))

(defn query!
  "Run a HoneySQL map and return rows with unqualified lower-case keys."
  [db-info q]
  (jdbc/execute! (ds db-info) (sql/format q) {:builder-fn rs/as-unqualified-lower-maps}))

(defn query-one! [db-info q] (first (query! db-info q)))

(defn execute! [db-info q]
  (jdbc/execute! (ds db-info) (sql/format q)))

(defn- normalize-status
  "Map runtime status keywords to V1 schema CHECK constraint values.
   Schema allows: running, done, error, interrupted."
  [status]
  (case status
    (:success :done)                       "done"
    :error                                 "error"
    (:cancelled :interrupted)              "interrupted"
    :running                               "running"
    ;; fallback
    (->kw (or status :done))))

(defn- ->json [m] (when m (json/write-json-str m)))
(defn- <-json [s] (when s (json/read-json s :key-fn keyword)))

(defn- ->blob
  "Serialize a Clojure value to a Nippy byte array for BLOB columns."
  ^bytes [v]
  (nippy/freeze v))

(defn- reindex-search!
  "Replace every `search` row for `(owner-table, owner-id, field)` with
   `(vis/search-text v)`. Skips when projection is blank — FT5
   indexing empty rows just inflates the index.

   Used as the write-side hook for the rich text fields the bubble
   layer carries as Markdown (answer) or markdown-shaped strings
   (thinking, per-code-block comments). `vis/search-text` lifts the
   string via `markdown->ir` and projects it to plain text before
   indexing. Search hits land back on `(owner_table, owner_id)`
   which the caller translates to the right row."
  [tx-info owner-table owner-id field v]
  (let [text (some-> v vis/search-text)]
    (execute! tx-info
      {:delete-from :search
       :where [:and
               [:= :owner_table owner-table]
               [:= :owner_id (str owner-id)]
               [:= :field field]]})
    (when-not (or (nil? text) (= "" text))
      (execute! tx-info
        {:insert-into :search
         :values [{:owner_table owner-table
                   :owner_id    (str owner-id)
                   :field       field
                   :text        text}]}))))

(defn- <-blob
  "Deserialize a Nippy byte array from a BLOB column back to a Clojure value."
  [^bytes bs]
  (when bs (nippy/thaw bs)))

(def ^:private raw-response-preview-chars 4000)

(defn- sha256-hex [^String s]
  (let [digest (.digest (MessageDigest/getInstance "SHA-256")
                 (.getBytes s "UTF-8"))]
    (apply str (map #(format "%02x" (bit-and 0xff %)) digest))))

(defn- raw-response-preview [^String s]
  (subs s 0 (min raw-response-preview-chars (count s))))

(defn- utf8-byte-count
  ^long [s]
  (long (alength (.getBytes (str s) "UTF-8"))))

;; =============================================================================
;; Error translation + bootstrap error normalization
;; =============================================================================

(defn- causal-chain
  "Walk `(.getCause e)` until a fixed point or cycle is hit. Returns the
   chain in causal order (innermost first), bounded so a self-referential
   cause graph can't loop forever."
  [^Throwable e]
  (loop [acc [] cur e seen #{}]
    (cond
      (nil? cur)           (reverse acc)
      (contains? seen cur) (reverse acc)
      (>= (count acc) 16)  (reverse acc)
      :else (recur (conj acc cur) (.getCause cur) (conj seen cur)))))

(def ^:private migration-checksum-mismatch-user-message
  (str "Database schema mismatch: local migrations changed since this database was created. "
    "Close all Vis processes, remove ~/.vis/vis.mdb, then restart Vis to recreate it from packaged migration resources."))

;; Phase B: no legacy fallback. When the packaged V1__schema.sql checksum no
;; longer matches the recorded one on disk (i.e. we changed schema in
;; source without bumping the version), nuke the whole `~/.vis/vis.mdb`
;; directory and let the next open-attempt rebuild from V1 fresh. This
;; eliminates the "close vis, rm -rf, restart" manual loop. Only fires
;; for persistent stores under the canonical user-home path; refuses to
;; touch arbitrary paths or in-memory stores.
(def ^:private canonical-vis-home-suffix ".vis/vis.mdb")

(defn- canonical-vis-db-path?
  "True when `path` is the canonical `~/.vis/vis.mdb` directory. The
   path may be a file inside the dir or the dir itself; both resolve
   to a parent ending in `.vis/vis.mdb`. Conservative: refuses to nuke
   arbitrary user-supplied paths."
  [^String path]
  (when (string? path)
    (let [abs (try (.getCanonicalPath (java.io.File. path)) (catch Throwable _ path))]
      (or (.endsWith ^String abs canonical-vis-home-suffix)
        (.contains ^String abs (str canonical-vis-home-suffix "/"))
        (.contains ^String abs (str canonical-vis-home-suffix java.io.File/separator))))))

(defn- rm-rf!
  "Recursive delete — deletes the file or directory tree at `path`.
   Java NIO `Files/walk` returns descendants depth-first so deletion
   order is safe. Errors are logged but not thrown; the caller still
   raises the original checksum-mismatch on continuation, so a partial
   nuke surfaces visibly. Returns the count of paths deleted."
  [^String path]
  (let [root (java.nio.file.Paths/get path (into-array String []))]
    (when (java.nio.file.Files/exists root
            (into-array java.nio.file.LinkOption []))
      (with-open [stream (java.nio.file.Files/walk root
                           (into-array java.nio.file.FileVisitOption []))]
        (let [paths (-> stream
                      .sorted
                      java.util.stream.Stream/.toArray
                      vec
                      reverse)]
          (doseq [^java.nio.file.Path p paths]
            (try (java.nio.file.Files/deleteIfExists p)
              (catch Throwable _)))
          (count paths))))))

(defn- migration-checksum-mismatch?
  "True when any throwable in the causal chain looks like Flyway's
   migration checksum validation failure."
  [^Throwable e]
  (boolean
    (some (fn [^Throwable t]
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
      {:vis/user-error true
       :type           :vis/db-migration-checksum-mismatch}
      e)
    e))

(defn- sqlite-cantopen-message?
  "True when any link in the cause chain looks like a SQLite open failure."
  [^Throwable e]
  (boolean
    (some (fn [^Throwable t]
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
    (let [home   (System/getProperty "user.home")
          dbpath (str home "/.vis/vis.mdb/vis.db")
          dbdir  (str home "/.vis/vis.mdb")
          dirf   (File. dbdir)
          filef  (File. dbpath)]
      (str "Vis database is unavailable. "
        "Expected file: " dbpath ". "
        (cond
          (not (.exists filef))
          "The file is missing - likely deleted while Vis was running. Restart Vis to recreate it."

          (not (.canWrite dirf))
          (str "The directory " dbdir " is not writable by this process.")

          :else
          "The handle was lost mid-session. Restart Vis to reconnect.")))))

;; =============================================================================
;; Schema install
;;
;; The Flyway runner and canonical V*__schema.sql files live in this
;; package under
;; `resources/db/sqlite/migration/`. We only point Flyway at that
;; classpath location. No schema DDL or repair DDL lives in Clojure.
;; If an existing local database no longer matches the packaged V1 SQL,
;; Flyway fails and the user removes/recreates the local DB intentionally.
;; =============================================================================

(def ^:private MIGRATIONS "classpath:db/sqlite/migration")

(defn- install-schema! [^DataSource ds]
  (migration/migrate! ds MIGRATIONS))

;; =============================================================================
;; Connection management
;;
;; xerial's `SQLiteConnectionPoolDataSource` is a JNDI-shaped façade
;; that opens a NEW physical SQLite connection on every
;; `getConnection()` - it does NOT pool. We replace it with HikariCP:
;; an underlying `SQLiteDataSource` configured via `SQLiteConfig`
;; (WAL, NORMAL sync, FK enforcement, 30s busy timeout) is wrapped
;; in a `HikariDataSource` with a small fixed pool. SQLite WAL allows
;; N readers + 1 writer, so 5 connections give read concurrency
;; without amplifying writer contention. The pool keeps physical
;; connections alive for the process lifetime (no idle eviction,
;; no maxLifetime recycling - unlike a network DB, SQLite handles
;; have no equivalent of a connection drop). `db-close!` actually
;; closes the pool now, so db-dispose-connection! releases handles
;; deterministically.
;; =============================================================================

(defn- raw-sqlite-datasource
  "Build a configured xerial `SQLiteDataSource` (the plain non-pooled
   one). All pragmas (`journal_mode=WAL`, `synchronous=NORMAL`,
   `transaction_mode=IMMEDIATE`, `foreign_keys=ON`,
   `busy_timeout=30000`) are set on the `SQLiteConfig` so every
   Hikari-handed connection inherits them.

   The returned object is what we hand to Hikari as its underlying
   DataSource; callers should NOT call `getConnection` on this directly."
  ^DataSource [^String url]
  (let [cfg (doto (SQLiteConfig.)
              (.setJournalMode SQLiteConfig$JournalMode/WAL)
              (.setSynchronous SQLiteConfig$SynchronousMode/NORMAL)
              (.setTransactionMode SQLiteConfig$TransactionMode/IMMEDIATE)
              (.enforceForeignKeys true)
              (.setBusyTimeout 30000))
        ds  (SQLiteDataSource. cfg)]
    (.setUrl ds url)
    ds))

(defn- pooled-datasource
  "Wrap `raw-ds` in a HikariCP pool. `pool-name` is the JMX name and
   shows up in thread names (`HikariPool-vis-rlm-disk-1`), which
   makes debugging far easier when there are multiple envs alive.

   Pool sizing for SQLite WAL:
     maximumPoolSize = 5  - 1 writer + up to 4 concurrent readers,
                            mirrors the SQLite WAL concurrency model.
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
  (let [cfg (doto (HikariConfig.)
              (.setPoolName             pool-name)
              (.setDataSource           raw-ds)
              (.setMaximumPoolSize      5)
              (.setMinimumIdle          1)
              (.setConnectionTimeout    30000)
              (.setIdleTimeout          0)
              (.setMaxLifetime          0)
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
  (doseq [close! [#(when lock (.release lock))
                  #(when channel (.close channel))
                  #(when raf (.close raf))]]
    (try (close!) (catch Throwable _ nil))))

(defn- with-migration-lock!
  "Serialize Flyway across JVMs while allowing the
   database itself to stay multiprocess-open for normal SQLite WAL writes."
  [^String canonical-dir f]
  (let [lock-file (File. canonical-dir DB_MIGRATION_LOCK_FILENAME)
        raf       (RandomAccessFile. lock-file "rw")
        channel   (.getChannel raf)
        lock      (try
                    (.lock channel)
                    (catch Throwable t
                      (close-lock-resources! {:channel channel :raf raf})
                      (throw t)))]
    (try
      (f)
      (finally
        (close-lock-resources! {:lock lock :channel channel :raf raf})))))

(defn- open-sqlite-at-dir [^String dir]
  (let [path (.getCanonicalPath (File. dir))]
    (.mkdirs (File. path))
    (let [file (str path "/" DB_FILENAME)
          raw  (raw-sqlite-datasource (str "jdbc:sqlite:" file))
          pool (pooled-datasource raw
                 (str "vis-rlm-disk-" (.incrementAndGet pool-counter)))]
      (try
        (with-migration-lock! path #(install-schema! pool))
        {:datasource pool :conn pool :path path :db-file file :backend :sqlite}
        (catch Throwable t
          (try (.close ^HikariDataSource pool) (catch Throwable _ nil))
          (throw t))))))

(def ^:private ^AtomicLong mem-counter
  (AtomicLong.))

(defn- open-sqlite-mem []
  ;; Use a named shared-cache in-memory DB so every pooled connection
  ;; sees the same tables. Each call gets a unique name to isolate
  ;; tests; the pool's `minimumIdle 1` keeps the shared-cache DB
  ;; alive without the manual `getConnection` keep-alive the old
  ;; non-pooled implementation needed.
  (let [db-name (str "vis_mem_" (.incrementAndGet mem-counter))
        raw     (raw-sqlite-datasource
                  (str "jdbc:sqlite:file:" db-name "?mode=memory&cache=shared"))
        pool    (pooled-datasource raw
                  (str "vis-rlm-mem-" (.incrementAndGet pool-counter)))]
    (install-schema! pool)
    {:datasource pool :conn pool :path nil :db-file nil
     :backend :sqlite :owned? true :mode :memory}))

(defn- stable-db-file-key
  "Best-effort stable identity for the current SQLite file behind a store.
   Used to detect when the pathname stayed the same but the file was
   replaced underneath a long-lived JVM (common in dev when `~/.vis/vis.mdb`
   gets recreated while nREPL survives)."
  [store]
  (when-let [db-file (:db-file store)]
    (try
      (let [attrs (Files/readAttributes (Paths/get db-file (make-array String 0))
                    "basic:fileKey,lastModifiedTime,size"
                    (make-array LinkOption 0))]
        {:db-file       db-file
         :file-key      (get attrs "fileKey")
         :last-modified (some-> (get attrs "lastModifiedTime") str)
         :size          (get attrs "size")})
      (catch Throwable _
        {:db-file db-file :missing? true}))))

(defn- with-file-key-snapshot [store]
  (cond-> store
    (= :persistent (:mode store))
    (assoc :file-key-snapshot (stable-db-file-key store))))

(defn db-open! [db-spec]
  (try
    (cond
      (nil? db-spec) nil
      (= :memory db-spec) (open-sqlite-mem)
      ;; `:owned? true` for both memory and persistent: we built the
      ;; Hikari pool ourselves, so we own its lifecycle. The old code
      ;; stamped persistent stores as `:owned? false` because the
      ;; non-pooled DataSource didn't *need* closing - it had no
      ;; resources to release. With Hikari that's no longer true; an
      ;; un-closed pool leaks daemon threads and connection handles.
      (string? db-spec) (with-file-key-snapshot
                          (assoc (open-sqlite-at-dir db-spec) :owned? true :mode :persistent))
      (map? db-spec)
      (cond
        (or (:datasource db-spec) (:conn db-spec))
        (let [ds (or (:datasource db-spec) (:conn db-spec))]
          (install-schema! ds)
          {:datasource ds :conn ds :path nil :db-file nil
           :backend :external :owned? false :mode :external})
        (:path db-spec)
        (with-file-key-snapshot
          (assoc (open-sqlite-at-dir (:path db-spec)) :owned? true :mode :persistent))
        :else
        (throw (ex-info "Invalid db-spec map" {:type :vis/invalid-db-spec :db-spec db-spec})))
      :else
      (throw (ex-info "Invalid db-spec" {:type :vis/invalid-db-spec :db-spec db-spec})))
    (catch Throwable e
      ;; Phase B: on checksum mismatch for the canonical ~/.vis/vis.mdb
      ;; path, nuke and retry once. The DB is a forensics/replay store,
      ;; not the source of truth; schema-rewrites drop legacy state and
      ;; the rebuild is cheap (Flyway re-runs V1 from packaged resources).
      (if (and (migration-checksum-mismatch? e)
            (string? db-spec)
            (canonical-vis-db-path? db-spec))
        (let [deleted (try (rm-rf! db-spec) (catch Throwable _ 0))]
          (tel/log! {:level :warn :id ::db-nuke-on-checksum-mismatch
                     :data  {:path db-spec :deleted-paths deleted}}
            (str "Phase B: nuked ~/.vis/vis.mdb on schema-checksum mismatch ("
              deleted " paths); retrying open with fresh schema"))
          (try
            (with-file-key-snapshot
              (assoc (open-sqlite-at-dir db-spec) :owned? true :mode :persistent))
            (catch Throwable e2
              (throw (maybe-wrap-db-open-error e2)))))
        (throw (maybe-wrap-db-open-error e))))))

(defn db-close!
  "Idempotent dispose. Closes the Hikari pool when we own it; for
   `:external` mode (caller-supplied DataSource) it's a no-op - the
   caller still owns the handle they passed in."
  [store]
  (when (and store (:owned? store))
    (let [^Object ds (:datasource store)]
      (when (instance? java.io.Closeable ds)
        (try (.close ^java.io.Closeable ds) (catch Throwable _ nil)))))
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
  (try
    (.getCanonicalPath (java.io.File. (str p)))
    (catch Throwable _ (str p))))

(defn db-store-stale?
  "True when a persistent SQLite store no longer matches the requested
   db-spec or the file at the same path was replaced under this JVM. When
   true, the facade closes the old shared pool and opens a new one.
   Paths compare CANONICALIZED on both sides."
  [store db-spec]
  (when (= :persistent (:mode store))
    (or (and (string? db-spec)
          (not= (canonical-path db-spec) (canonical-path (:path store))))
      (and (map? db-spec)
        (:path db-spec)
        (not= (canonical-path (:path db-spec)) (canonical-path (:path store))))
      (and (:file-key-snapshot store)
        (not= (stable-db-file-key store) (:file-key-snapshot store))))))

;; =============================================================================
;; SQLite write policy
;; =============================================================================

(def ^:private sqlite-write-retry-delays-ms
  [5 10 20 40 80 160])

(defonce ^:private sqlite-write-lock
  (Object.))

(defn- sqlite-busy-cause?
  [^Throwable t]
  (let [message (some-> (.getMessage t) str/lower-case)
        code    (when (instance? SQLException t)
                  (.getErrorCode ^SQLException t))]
    (or (contains? #{5 6 517} code)
      (and message
        (or (str/includes? message "sqlite_busy")
          (str/includes? message "busy_snapshot")
          (str/includes? message "database is locked")
          (str/includes? message "database table is locked"))))))

(defn- sqlite-busy?
  [^Throwable t]
  (loop [cause t]
    (cond
      (nil? cause) false
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
      (loop [attempt 0 delays sqlite-write-retry-delays-ms]
        (let [result (try
                       {:success? true :value (sqlite-write-attempt! db-info f)}
                       (catch Throwable t
                         {:success? false :throwable t}))]
          (if (:success? result)
            (:value result)
            (let [t (:throwable result)]
              (if (and (sqlite-busy? t) (seq delays))
                (do
                  (Thread/sleep (long (first delays)))
                  (recur (inc attempt) (rest delays)))
                (throw t)))))))))

;; =============================================================================
;; Logging - log table
;; =============================================================================

(defn db-log! [db-info entry]
  (when (ds db-info)
    (sqlite-write-tx! db-info
      (fn [tx-info]
        (execute! tx-info
          {:insert-into :log
           :values [(cond-> {:id         (new-id)
                             :level      (->kw (:level entry))
                             :event      (str (:event entry))
                             :created_at (now-ms)}
                      (:data entry)                  (assoc :data (:data entry))
                      (:session-soul-id entry)  (assoc :session_soul_id (->id (:session-soul-id entry)))
                      (:session-state-id entry) (assoc :session_state_id (->id (:session-state-id entry)))
                      (:session-turn-soul-id entry)         (assoc :session_turn_soul_id (->id (:session-turn-soul-id entry)))
                      (:session-turn-state-id entry)        (assoc :session_turn_state_id (->id (:session-turn-state-id entry)))
                      (:iteration-id entry)          (assoc :session_turn_iteration_id (->id (:iteration-id entry))))]})))))

;; =============================================================================
;; Workspace - trunk-native work units
;; =============================================================================

(defn- row->workspace
  "Project a `workspace` row from SQLite into the canonical Clojure shape
   used by the workspace facade. Keys mirror `db-get-session` style
   (plain, not namespaced); the workspace ns adds `:workspace/*` aliases
   when publishing into an environment."
  [row]
  (when row
    (cond-> {:id         (->uuid (:id row))
             :type       :workspace
             :repo-id    (:repo_id row)
             :repo-root  (:repo_root row)
             :root       (:root row)
             :state      (->kw-back (:state row))
             :created-at (->date (:created_at row))}
      (:fork_ms row)             (assoc :fork-ms (:fork_ms row))
      (:apply_fork_ms row)       (assoc :apply-fork-ms (:apply_fork_ms row))
      (:workspace_kind row)      (assoc :workspace-kind (->kw-back (:workspace_kind row)))
      (:workspace_backend row)   (assoc :workspace-backend (->kw-back (:workspace_backend row)))
      (:parent_workspace_id row) (assoc :parent-workspace-id (->uuid (:parent_workspace_id row)))
      (:discarded_at row)        (assoc :discarded-at (->date (:discarded_at row)))
      ;; Surfaced for the workspace facade's label/focus helpers.
      ;; NULL columns are skipped so callers can
      ;; treat absence as "fall back to default" (label → session.title;
      ;; last-focused → created_at).
      (:label row)               (assoc :label (:label row))
      (:last_focused_at_ms row)  (assoc :last-focused-at-ms (:last_focused_at_ms row))
      (:context_roots row)       (assoc :context-roots (or (<-json (:context_roots row)) [])))))

(defn db-workspace-insert!
  "Insert a workspace row. Returns the inserted record (canonical shape).

   Required: :repo-id :repo-root :root
   Optional: :id (defaults to a new UUID), :label, :fork-ms,
             :apply-fork-ms, :workspace-kind, :workspace-backend,
             :parent-workspace-id, :state
             (defaults to :active)"
  [db-info {:keys [id repo-id repo-root root label fork-ms apply-fork-ms
                   workspace-kind workspace-backend parent-workspace-id state]}]
  (when (ds db-info)
    (let [ws-id (->id (or id (new-uuid)))
          now   (now-ms)]
      (sqlite-write-tx! db-info
        (fn [tx-info]
          (execute! tx-info
            {:insert-into :workspace
             :values [{:id          ws-id
                       :repo_id     repo-id
                       :repo_root   repo-root
                       :root        root
                       :label       label
                       :fork_ms     fork-ms
                       :apply_fork_ms apply-fork-ms
                       :workspace_kind (->kw (or workspace-kind
                                               (if fork-ms :draft :trunk)))
                       :workspace_backend (->kw (or workspace-backend :live))
                       :parent_workspace_id (some-> parent-workspace-id ->ref)
                       :state       (->kw (or state :active))
                       :created_at  now}]})
          (row->workspace
            (query-one! tx-info
              {:select [:*] :from :workspace
               :where  [:= :id ws-id]})))))))

(defn db-workspace-update-state!
  "Transition `workspace-id` to `new-state` (`:active` | `:discarded`).
   Stamps `discarded_at` on :discarded. Returns the updated record."
  [db-info workspace-id new-state]
  (when (and (ds db-info) workspace-id)
    (let [id  (->ref workspace-id)
          now (now-ms)
          to  (->kw new-state)
          set (cond-> {:state to}
                (= "discarded" to) (assoc :discarded_at now))]
      (sqlite-write-tx! db-info
        (fn [tx-info]
          (execute! tx-info
            {:update :workspace
             :set    set
             :where  [:= :id id]})
          (row->workspace
            (query-one! tx-info
              {:select [:*] :from :workspace
               :where  [:= :id id]})))))))

(defn db-workspace-update-label!
  "Set the human-friendly `:label` override. Pass nil to clear the
   label and fall back to the heuristic. Returns the updated record."
  [db-info workspace-id label]
  (when (and (ds db-info) workspace-id)
    (let [id (->ref workspace-id)]
      (sqlite-write-tx! db-info
        (fn [tx-info]
          (execute! tx-info
            {:update :workspace
             :set    {:label label}
             :where  [:= :id id]})
          (row->workspace
            (query-one! tx-info
              {:select [:*] :from :workspace
               :where  [:= :id id]})))))))

(defn db-workspace-set-context-roots!
  "Persist the workspace's extra context roots as a JSON array of canonical
   path strings. `roots` is a coll of strings (deduped/canonicalized by the
   caller). Empty/nil stores NULL (no extra roots). Returns the updated record."
  [db-info workspace-id roots]
  (when (and (ds db-info) workspace-id)
    (let [id    (->ref workspace-id)
          rs    (vec (distinct (filter some? roots)))
          stored (when (seq rs) (->json rs))]
      (sqlite-write-tx! db-info
        (fn [tx-info]
          (execute! tx-info
            {:update :workspace
             :set    {:context_roots stored}
             :where  [:= :id id]})
          (row->workspace
            (query-one! tx-info
              {:select [:*] :from :workspace
               :where  [:= :id id]})))))))

(defn db-workspace-touch-focus!
  "Stamp `last_focused_at_ms` to now-ms on the workspace row. Called by
   `workspace/focus!`. Returns the updated record."
  [db-info workspace-id]
  (when (and (ds db-info) workspace-id)
    (let [id  (->ref workspace-id)
          now (now-ms)]
      (sqlite-write-tx! db-info
        (fn [tx-info]
          (execute! tx-info
            {:update :workspace
             :set    {:last_focused_at_ms now}
             :where  [:= :id id]})
          (row->workspace
            (query-one! tx-info
              {:select [:*] :from :workspace
               :where  [:= :id id]})))))))

(defn db-repo-focus-get
  "Return the `workspace_id` currently pinned as the focus pointer for
   `repo-id`. Nil when no entry exists yet."
  [db-info repo-id]
  (when (and (ds db-info) repo-id)
    (some-> (query-one! db-info
              {:select [:workspace_id :updated_at_ms]
               :from   :repo_focus
               :where  [:= :repo_id repo-id]})
      (as-> r {:workspace-id   (->uuid (:workspace_id r))
               :updated-at-ms  (:updated_at_ms r)}))))

(defn db-repo-focus-set!
  "Upsert the per-repo focus pointer to `workspace-id`. Updates
   `updated_at_ms` to now. Returns the new pointer map."
  [db-info repo-id workspace-id]
  (when (and (ds db-info) repo-id workspace-id)
    (let [ws-id (->ref workspace-id)
          now   (now-ms)]
      (sqlite-write-tx! db-info
        (fn [tx-info]
          ;; SQLite UPSERT: INSERT ... ON CONFLICT REPLACE the row.
          (execute! tx-info
            {:insert-into :repo_focus
             :values [{:repo_id       repo-id
                       :workspace_id  ws-id
                       :updated_at_ms now}]
             :on-conflict :repo_id
             :do-update-set {:workspace_id  ws-id
                             :updated_at_ms now}})
          {:workspace-id   (->uuid ws-id)
           :updated-at-ms  now})))))

(defn db-workspace-get [db-info workspace-id]
  (when (and (ds db-info) workspace-id)
    (row->workspace
      (query-one! db-info
        {:select [:*] :from :workspace
         :where  [:= :id (->ref workspace-id)]}))))

(defn db-workspace-list-by-repo
  "List workspaces in `repo-id`, optionally filtered to a `state-set` of
   keywords (e.g. #{:active :merging}). Newest first."
  ([db-info repo-id] (db-workspace-list-by-repo db-info repo-id nil))
  ([db-info repo-id state-set]
   (when (ds db-info)
     (let [where (cond-> [:and [:= :repo_id repo-id]]
                   (seq state-set)
                   (conj [:in :state (mapv ->kw state-set)]))]
       (mapv row->workspace
         (query! db-info
           {:select   [:*]
            :from     :workspace
            :where    where
            :order-by [[:created_at :desc]]}))))))

(defn db-workspace-for-session
  "Return the workspace pinned to `session-state-id`, or nil. Always
   non-nil after step-4 wiring (1:1 invariant); nil only during the
   transitional window where session_state may not yet have a workspace."
  [db-info session-state-id]
  (when (and (ds db-info) session-state-id)
    (let [sid (->ref session-state-id)]
      (row->workspace
        (query-one! db-info
          {:select [:w.*]
           :from   [[:session_state :s]]
           :join   [[:workspace :w] [:= :w.id :s.workspace_id]]
           :where  [:= :s.id sid]})))))

(defn db-session-state-list-for-workspace
  "List session_state rows whose `workspace_id` = `workspace-id`,
   newest first. The 1:1 invariant guarantees at most one ACTIVE row
   per workspace, but historical version rows (from forks) may exist
   so the projection is a vec, not a single row. Returns canonical
   `{:id :session-soul-id :title :version :created-at …}` shape."
  [db-info workspace-id]
  (when (and (ds db-info) workspace-id)
    (let [ws-id (->ref workspace-id)]
      (mapv
        (fn [r]
          {:id               (->uuid (:id r))
           :session-soul-id  (->uuid (:session_soul_id r))
           :title            (:title r)
           :version          (:version r)
           :workspace-id     (->uuid (:workspace_id r))
           :created-at       (->date (:created_at r))})
        (query! db-info
          {:select   [:*]
           :from     :session_state
           :where    [:= :workspace_id ws-id]
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
           :set    {:workspace_id (->ref workspace-id)}
           :where  [:= :id (->ref session-state-id)]})
        {:session-state-id (->uuid session-state-id)
         :workspace-id     (->uuid workspace-id)}))))

(defn db-session-state-cas-workspace!
  "Atomically repin `session-state-id` from `expected-workspace-id` to
   `workspace-id`. Returns true only when the expected workspace was still the
   current value at commit time."
  [db-info session-state-id expected-workspace-id workspace-id]
  (when (and (ds db-info) session-state-id expected-workspace-id workspace-id)
    (sqlite-write-tx! db-info
      (fn [tx-info]
        (let [result (execute! tx-info
                       {:update :session_state
                        :set    {:workspace_id (->ref workspace-id)}
                        :where  [:and
                                 [:= :id (->ref session-state-id)]
                                 [:= :workspace_id (->ref expected-workspace-id)]]})]
          (= 1 (or (:next.jdbc/update-count (first result)) 0)))))))

;; =============================================================================
;; Session - session_soul + session_state
;; =============================================================================

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
  [db-info {:keys [channel external-id title system-prompt provider model
                   workspace-id parent-state-id]}]
  (when-not workspace-id
    (throw (ex-info "db-store-session! requires :workspace-id (1:1 invariant)"
             {:type :persistance/missing-workspace-id})))
  (when (ds db-info)
    (sqlite-write-tx! db-info
      (fn [tx-info]
        (let [soul-id  (new-uuid)
              state-id (new-uuid)
              now      (now-ms)]
          (execute! tx-info
            {:insert-into :session_soul
             :values [(cond-> {:id          (str soul-id)
                               :channel     (name (->kw (or channel :tui)))
                               :external_id (some-> external-id str)
                               :created_at  now}
                        ;; sub_loop child → cross-soul link to the parent state;
                        ;; keeps the child OUT of the top-level list (queryable
                        ;; sub-tree + cascade-delete with the parent).
                        parent-state-id (assoc :parent_state_id (->ref parent-state-id)))]})
          (execute! tx-info
            {:insert-into :session_state
             :values [(cond-> {:id                   (str state-id)
                               :session_soul_id      (str soul-id)
                               :workspace_id         (->ref workspace-id)
                               :title                title
                               :version              0
                               :system_prompt        (or system-prompt "")
                               :llm_root_model       (or model "")
                               :created_at           now}
                        provider (assoc :llm_root_provider (name (->kw provider))))]})
          soul-id)))))

(defn- latest-state-for [db-info soul-id-s]
  (query-one! db-info
    {:select [:*]
     :from   :session_state
     :where  [:and
              [:= :session_soul_id soul-id-s]
              [:= :version
               {:select [[[:max :version]]]
                :from   :session_state
                :where  [:= :session_soul_id soul-id-s]}]]}))

(defn db-get-session [db-info session-id]
  (when (and (ds db-info) session-id)
    (let [id (->ref session-id)]
      (when-let [soul (query-one! db-info {:select [:*] :from :session_soul :where [:= :id id]})]
        (let [state (latest-state-for db-info id)]
          (cond-> {:id            (->uuid (:id soul))
                   :type          :session
                   :channel       (->kw-back (:channel soul))
                   :external-id   (:external_id soul)
                   :title         (:title state)
                   :system-prompt (:system_prompt state)
                   :model         (:llm_root_model state)
                   :version       (or (:version state) 0)
                   :created-at    (->date (:created_at soul))}
            (:llm_root_provider state) (assoc :provider (->kw-back (:llm_root_provider state)))))))))

(defn db-resolve-session-id [db-info selector]
  (cond
    (nil? selector) nil
    (= :latest selector)
    (when (ds db-info)
      (when-let [row (query-one! db-info
                       {:select [:id] :from :session_soul
                        :order-by [[:created_at :desc]] :limit 1})]
        (->uuid (:id row))))
    (and (vector? selector) (= :id (first selector))) (->uuid (second selector))
    (uuid? selector) selector
    (string? selector) (->uuid selector)
    :else nil))

(defn db-list-sessions [db-info channel]
  (when (ds db-info)
    (let [ch (name (->kw channel))]
      (mapv (fn [row]
              {:id          (->uuid (:id row))
               :channel     (->kw-back (:channel row))
               :external-id (:external_id row)
               :title       (:state_title row)
               :version     (:version row)
               :fork-count  (or (:fork_count row) 0)
               :created-at  (->date (:created_at row))})
        (query! db-info
          {:select [:cs.id :cs.channel :cs.external_id :cs.created_at
                    [:s.title :state_title]
                    :s.version
                    [{:select [[[:count :*]]]
                      :from   [[:session_state :child]]
                      :where  [:and
                               [:= :child.session_soul_id :cs.id]
                               [:not= :child.parent_state_id nil]]}
                     :fork_count]]
           :from   [[:session_soul :cs]]
           :join   [[:session_state :s]
                    [:= :s.session_soul_id :cs.id]]
           :where  [:and
                    [:= :cs.channel ch]
                    ;; TOP-LEVEL only — sub_loop child souls (parent_state_id set)
                    ;; hang off their parent's sub-tree, never the session list.
                    [:= :cs.parent_state_id nil]
                    [:= :s.version
                     {:select [[[:max :s2.version]]]
                      :from   [[:session_state :s2]]
                      :where  [:= :s2.session_soul_id :cs.id]}]]
           :order-by [[:cs.created_at :desc]]})))))

(defn db-find-session-by-external [db-info channel external-id]
  (when (and (ds db-info) external-id)
    (let [ch  (name (->kw channel))
          ext (str external-id)]
      (when-let [row (query-one! db-info
                       {:select [:id]
                        :from   :session_soul
                        :where  [:and
                                 [:= :channel ch]
                                 [:= :external_id ext]]})]
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
      (some-> (latest-state-for db-info soul-id-s) :id ->uuid))))

(defn db-update-session-title! [db-info session-id title]
  (when (and (ds db-info) session-id)
    (sqlite-write-tx! db-info
      (fn [tx-info]
        (let [soul-id-s (->ref session-id)]
          (when-let [state (latest-state-for tx-info soul-id-s)]
            (execute! tx-info
              {:update :session_state
               :set    {:title title}
               :where  [:= :id (:id state)]})))))))

(defn db-delete-session-tree! [db-info session-soul-id]
  (when (and (ds db-info) session-soul-id)
    (sqlite-write-tx! db-info
      (fn [tx-info]
        (execute! tx-info
          {:delete-from :session_soul
           :where [:= :id (->id session-soul-id)]})))))

;; =============================================================================
;; Fork - branch a session at a point
;; =============================================================================

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
    (let [soul-id-s (->ref session-id)
          rows (query! db-info
                 {:select [:cs.id :cs.version :cs.parent_state_id :cs.title
                           :cs.system_prompt :cs.llm_root_provider :cs.llm_root_model
                           :cs.created_at
                           [{:select [[[:count :*]]]
                             :from   :session_turn_soul
                             :where  [:= :session_turn_soul.session_state_id :cs.id]}
                            :turn_count]]
                  :from   [[:session_state :cs]]
                  :where  [:= :cs.session_soul_id soul-id-s]
                  :order-by [[:cs.version :asc]]})]
      (mapv (fn [row]
              (cond-> {:state-id        (->uuid (:id row))
                       :version         (:version row)
                       :parent-state-id (some-> (:parent_state_id row) ->uuid)
                       :title           (:title row)
                       :created-at      (->date (:created_at row))
                       :turn-count      (or (:turn_count row) 0)}
                (:system_prompt row)     (assoc :system-prompt (:system_prompt row))
                (:llm_root_provider row) (assoc :provider (->kw-back (:llm_root_provider row)))
                (:llm_root_model row)    (assoc :model (:llm_root_model row))))
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
    (let [soul-id-s (->ref session-turn-id)
          rows (query! db-info
                 {:select [:qst.id :qst.version :qst.forked_from_session_turn_state_id
                           :qst.status :qst.prior_outcome
                           :qst.llm_root_provider :qst.llm_root_model
                           :qst.created_at
                           [{:select [[[:count :*]]]
                             :from   :session_turn_iteration
                             :where  [:= :session_turn_iteration.session_turn_state_id :qst.id]}
                            :session_turn_iteration_count]]
                  :from   [[:session_turn_state :qst]]
                  :where  [:= :qst.session_turn_soul_id soul-id-s]
                  :order-by [[:qst.version :asc]]})]
      (mapv (fn [row]
              (cond-> {:state-id                    (->uuid (:id row))
                       :version                     (:version row)
                       :forked-from-session-turn-state-id  (some-> (:forked_from_session_turn_state_id row) ->uuid)
                       :status                      (->kw-back (:status row))
                       :created-at                  (->date (:created_at row))
                       :iteration-count             (or (:session_turn_iteration_count row) 0)}
                (:prior_outcome row)     (assoc :prior-outcome (->kw-back (:prior_outcome row)))
                (:llm_root_provider row) (assoc :provider (->kw-back (:llm_root_provider row)))
                (:llm_root_model row)    (assoc :model (:llm_root_model row))))
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
    (sqlite-write-tx! db-info
      (fn [tx-info]
        (let [soul-id-s (->ref session-id)]
          (when-let [current (latest-state-for tx-info soul-id-s)]
            (let [new-id       (new-uuid)
                  now          (now-ms)
                  parent-title (:title current)
                  fork-title   (or title (str parent-title " (fork)"))
                  new-version  (inc (:version current))]
              (execute! tx-info
                {:insert-into :session_state
                 :values [(cond-> {:id                   (str new-id)
                                   :session_soul_id      soul-id-s
                                   :parent_state_id      (:id current)
                                   :workspace_id         (->ref workspace-id)
                                   :title                fork-title
                                   :version              new-version
                                   :system_prompt        (or system-prompt (:system_prompt current))
                                   :llm_root_model       (or model (:llm_root_model current))
                                   :created_at           now}
                            (or provider (:llm_root_provider current))
                            (assoc :llm_root_provider (name (->kw (or provider (:llm_root_provider current))))))]})
              ;; Mark parent state as forked (idempotent)
              (when (and parent-title
                      (not (str/includes? parent-title "[forked]")))
                (execute! tx-info
                  {:update :session_state
                   :set    {:title (str parent-title " [forked]")}
                   :where  [:= :id (:id current)]}))
              new-id)))))))

;; =============================================================================
;; State resolution
;; =============================================================================

(defn- latest-state-id [db-info session-id]
  (when (ds db-info)
    (let [soul-id-s (->ref session-id)]
      (:id (latest-state-for db-info soul-id-s)))))

(defn- session-state-chain
  "Return active branch state ids from root to latest leaf."
  [db-info session-id]
  (when (and (ds db-info) session-id)
    (when-let [leaf-id (latest-state-id db-info session-id)]
      (loop [state-id leaf-id
             acc      []]
        (if state-id
          (if-let [row (query-one! db-info
                         {:select [:id :parent_state_id]
                          :from   :session_state
                          :where  [:= :id state-id]})]
            (recur (:parent_state_id row) (conj acc (:id row)))
            (vec (reverse acc)))
          (vec (reverse acc)))))))

;; =============================================================================
;; Runtime-value serialization helpers
;;
;; These live above any code that calls them (db-update-session-turn! snapshots
;; CTX through `freeze-safe`; the iteration writer does the same for per-form
;; results). Keep this block here — do not push it back down past consumers, or
;; you'll be tempted to `(declare freeze-safe)` again.
;; =============================================================================

(defn- runtime-object?
  "True when `v` is a runtime-only object (function, var, or other
   runtime-internal object) that cannot be meaningfully serialized as
   data. These get a :vis/ref marker
   so the system knows to re-eval from the `expression` column to reconstruct them."
  [v]
  (or (fn? v)
    (instance? clojure.lang.Var v)
    (instance? java.util.concurrent.Future v)
    (some-> v class .getName (str/starts-with? "org.graalvm.polyglot."))))

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
  ([v depth]
   (cond
     (nil? v)                         nil
     (runtime-object? v)              {:vis/ref :expr}
     (instance? clojure.lang.LazySeq v) {:vis/ref :expr}
     (map? v)                         (if (zero? depth)
                                        {:vis/ref :depth-exceeded}
                                        (persistent!
                                          (reduce-kv
                                            (fn [m k val]
                                              (assoc! m k (freeze-safe val (dec depth))))
                                            (transient {})
                                            v)))
     (vector? v)                      (if (zero? depth)
                                        {:vis/ref :depth-exceeded}
                                        (mapv #(freeze-safe % (dec depth)) v))
     (set? v)                         (if (zero? depth)
                                        {:vis/ref :depth-exceeded}
                                        (into #{} (map #(freeze-safe % (dec depth))) v))
     (list? v)                        (if (zero? depth)
                                        {:vis/ref :depth-exceeded}
                                        (doall (map #(freeze-safe % (dec depth)) v)))
     (seq? v)                         {:vis/ref :expr}
     :else                            v)))

;; =============================================================================
;; Per-session model preference (session_soul.llm_pref_provider + llm_pref_model)
;; =============================================================================

(defn db-get-session-model-pref
  "The persisted model preference for a session (soul id) as
   `{:provider p :model m}`, or nil for the router default. Channel-neutral:
   web + TUI route through the same value."
  [db-info session-id]
  (when (and (ds db-info) session-id)
    (let [row (query-one! db-info
                {:select [:llm_pref_provider :llm_pref_model]
                 :from   :session_soul
                 :where  [:= :id (->ref session-id)]})
          m   (:llm_pref_model row)]
      (when m {:provider (:llm_pref_provider row) :model m}))))

(defn db-set-session-model-pref!
  "Persist (or clear, with both nil/blank) the PROVIDER + MODEL preference for
   a session (soul id). Lives on the stable soul, so it survives turn
   forks/retries."
  [db-info session-id provider model]
  (when (and (ds db-info) session-id)
    (let [m (some-> model str str/trim not-empty)
          p (some-> provider str str/trim not-empty)]
      (execute! db-info
        {:update :session_soul
         :set    {:llm_pref_provider (when m p) :llm_pref_model m}
         :where  [:= :id (->ref session-id)]}))
    nil))

;; =============================================================================
;; Turn - session_turn_soul + session_turn_state
;; =============================================================================

(defn db-store-session-turn!
  "Create session_turn_soul + initial session_turn_state (version 0).
   Returns the session-turn-soul UUID."
  [db-info {:keys [parent-session-id user-request status]}]
  (when (ds db-info)
    (sqlite-write-tx! db-info
      (fn [tx-info]
        (let [soul-id        (new-uuid)
              state-id       (new-uuid)
              now            (now-ms)
              state-id-s     (latest-state-id tx-info parent-session-id)
              turn-position  (or (:next_position
                                  (query-one! tx-info
                                    {:select [[[:coalesce [:+ [:max :position] 1] 1]
                                               :next_position]]
                                     :from   :session_turn_soul
                                     :where  [:= :session_state_id state-id-s]}))
                               1)
              user-request-s (or user-request "")]
          ;; `session_turn_soul` has no `title` column by design.
          ;; The canonical title lives on `session_state.title`
          ;; only (set via `(set-session-title! ...)`). A per-turn
          ;; title column previously existed but was auto-populated
          ;; with `(subs user_request 0 100)` — useless for trivial
          ;; greetings and never written by any model-facing primitive.
          (execute! tx-info
            {:insert-into :session_turn_soul
             :values [{:id                    (str soul-id)
                       :session_state_id state-id-s
                       :position              turn-position
                       :user_request          user-request-s
                       :created_at            now}]})
          (execute! tx-info
            {:insert-into :session_turn_state
             :values [{:id            (str state-id)
                       :session_turn_soul_id (str soul-id)
                       :version       0
                       :status        (normalize-status (or status :running))
                       :created_at    now}]})
          soul-id)))))

(defn- latest-session-turn-state [db-info session-turn-soul-id-s]
  (query-one! db-info
    {:select [:*]
     :from   :session_turn_state
     :where  [:and
              [:= :session_turn_soul_id session-turn-soul-id-s]
              [:= :version
               {:select [[[:max :version]]]
                :from   :session_turn_state
                :where  [:= :session_turn_soul_id session-turn-soul-id-s]}]]}))

(defn db-retry-session-turn!
  "Create a new session_turn_state (version N+1) for an existing session_turn_soul.
   Used when re-running a turn with a different provider/model or settings.
   Returns the new session-turn-state UUID."
  [db-info session-turn-soul-id {:keys [status provider model]}]
  (when (ds db-info)
    (sqlite-write-tx! db-info
      (fn [tx-info]
        (let [soul-id-s (->ref session-turn-soul-id)
              current   (latest-session-turn-state tx-info soul-id-s)
              new-id    (new-uuid)
              now       (now-ms)]
          (when current
            (execute! tx-info
              {:insert-into :session_turn_state
               :values [(cond-> {:id                         (str new-id)
                                 :session_turn_soul_id              soul-id-s
                                 :forked_from_session_turn_state_id (:id current)
                                 :version                    (inc (:version current))
                                 :status                     (normalize-status (or status :running))
                                 :llm_root_model             model
                                 :created_at                 now}
                          provider (assoc :llm_root_provider (name (->kw provider))))]})
            new-id))))))

;; =============================================================================
;; Dedicated CTX stores — task / fact / archive (write-through, slice 2).
;;
;; Alongside the per-turn ctx Nippy blob we UPSERT the engine's tasks/facts/
;; archived into queryable rows keyed by the OWNING session_state. task + fact
;; MIRROR the live ctx (clear-then-insert per turn = snapshot semantics, same as
;; the blob); archive ACCUMULATES (upsert by kind+key). Reads still come from the
;; blob until a later slice flips them — this just keeps the rows in sync so the
;; sub-session visibility view + cross-tree queries have real data.
;; =============================================================================
(defn- ->name-str
  "keyword|string|symbol -> string (for name columns); nil passes through."
  [x] (when (some? x) (if (keyword? x) (name x) (str x))))

(defn- scoped-id
  "Globally-unique PRIMARY KEY for the per-session ctx stores (task/fact/archive):
   the entity's logical id prefixed with the owning `session_state` ref. The
   real identity is `(session_state_id, key)` — the bare logical id repeats
   across sessions (a fact auto-keyed `turn_1`, a task `t3/auth`), and since a
   `sub_loop` child is a DISTINCT session_state sharing the parent's DB
   connection, an unscoped `id` collides parent↔child on the PK. `session_state`
   ids are UUIDs (no `/`), so the join is unambiguous; reads still filter by the
   `session_state_id` column, never by parsing this id."
  [ss logical-id]
  (str ss "/" logical-id))

(defn- task->row [ss now k t]
  (let [ks (->name-str k)]
    {:id         (scoped-id ss (str (or (:id t) ks)))
     :session_state_id ss
     :key        ks
     :parent_key (->name-str (:parent t))
     :status     (or (->name-str (:status t)) "todo")
     :kind       (->name-str (:kind t))
     :composite  (->name-str (:composite t))
     :position   (:order t)
     :plan       (if (:plan? t) 1 0)
     :title      (:title t)
     :acceptance (:acceptance t)
     :evidence   (:evidence t)
     :reason     (:reason t)
     :born       (:born t)
     :done_born  (:done-born t)
     :live       1
     :plan_gen   (:plan-gen t)
     :entity     (->blob (freeze-safe t))
     :updated_at now}))

(defn- fact->row [ss now k f]
  (let [ks (->name-str k)]
    {:id        (scoped-id ss (str (or (:id f) ks)))
     :session_state_id ss
     :key       ks
     :status    (or (->name-str (:status f)) "active")
     :source    (->name-str (:source f))
     :content   (some-> (:content f) str)
     :born      (:born f)
     :done_born (:done-born f)
     :entity    (->blob (freeze-safe f))
     :updated_at now}))

(defn- archive->row [ss now id v]
  {:id          (scoped-id ss (str id))
   :session_state_id ss
   :kind        (->name-str (:vis/kind v))
   :key         (->name-str (:vis/key v))
   :snippet     (some-> (or (:content v) (:title v) (:summary v)) str)
   :entity      (->blob (freeze-safe v))
   :born        (:born v)
   :archived_at now})

(defn- write-through-ctx-stores!
  "UPSERT tasks/facts/archived from `ctx` into the dedicated tables for the
   owning `session-state-id`. The task table is an append-only LEDGER: live
   tasks are upserted by scoped `:id` with `live 1`, and rows whose id is no
   longer present in live ctx are flipped `live 0` (never deleted) - so
   replaced/GC'd plan steps stay queryable via `db-list-task-history`. fact
   stays clear-then-insert (mirror of live ctx); archive upserts by
   (session_state, kind, key) and accumulates. Runs inside the caller's write
   tx so it commits atomically with the ctx blob."
  [tx-info session-state-id ctx]
  (when (and session-state-id ctx)
    (let [now       (now-ms)
          ss        (->ref session-state-id)
          task-rows (mapv (fn [[k t]] (task->row ss now k t)) (:session/tasks ctx))]
      (doseq [row task-rows]
        (execute! tx-info
          {:insert-into   :task
           :values        [row]
           :on-conflict   [:id]
           :do-update-set (dissoc row :id :session_state_id)}))
      (execute! tx-info
        {:update :task
         :set    {:live 0 :updated_at now}
         :where  (into [:and [:= :session_state_id ss] [:= :live 1]]
                   (when (seq task-rows)
                     [[:not-in :id (mapv :id task-rows)]]))})
      (execute! tx-info {:delete-from :fact :where [:= :session_state_id ss]})
      (doseq [[k f] (:session/facts ctx)]
        (execute! tx-info {:insert-into :fact :values [(fact->row ss now k f)]}))
      (doseq [[id v] (:session/archived ctx)]
        (let [row (archive->row ss now id v)]
          (execute! tx-info
            {:insert-into   :archive
             :values        [row]
             :on-conflict   [:session_state_id :kind :key]
             :do-update-set (dissoc row :id :session_state_id :kind :key)}))))))

(defn- graph-revision-row
  [row]
  (when row
    {:workspace-id        (->uuid (:workspace_id row))
     :session-state-id    (->uuid (:session_state_id row))
     :parent-workspace-id (some-> (:parent_workspace_id row) ->uuid)
     :ctx                 (<-blob (:ctx row))
     :advance             (<-blob (:advance row))
     :receipt             (<-blob (:receipt row))
     :created-at-ms       (:created_at row)
     :updated-at-ms       (:updated_at row)}))

(defn- graph-revision-exists?
  [db-info workspace-id]
  (some? (query-one! db-info
           {:select [:workspace_id]
            :from   :workspace_graph_revision
            :where  [:= :workspace_id (->ref workspace-id)]})))

(defn- insert-graph-revision-if-absent!
  [tx-info {:keys [workspace-id session-state-id parent-workspace-id ctx
                   advance receipt]}]
  (when (and ctx (not (graph-revision-exists? tx-info workspace-id)))
    (let [now (now-ms)]
      (execute! tx-info
        {:insert-into :workspace_graph_revision
         :values [{:workspace_id        (->ref workspace-id)
                   :session_state_id    (->ref session-state-id)
                   :parent_workspace_id (some-> parent-workspace-id ->ref)
                   :ctx                 (->blob (freeze-safe ctx))
                   :advance             (some-> advance freeze-safe ->blob)
                   :receipt             (some-> receipt freeze-safe ->blob)
                   :created_at          now
                   :updated_at          now}]}))))

(defn- put-graph-revision!
  [tx-info {:keys [workspace-id session-state-id parent-workspace-id ctx
                   advance receipt]}]
  (when ctx
    (if (graph-revision-exists? tx-info workspace-id)
      (execute! tx-info
        {:update :workspace_graph_revision
         :set    {:session_state_id    (->ref session-state-id)
                  :parent_workspace_id (some-> parent-workspace-id ->ref)
                  :ctx                 (->blob (freeze-safe ctx))
                  :advance             (some-> advance freeze-safe ->blob)
                  :receipt             (some-> receipt freeze-safe ->blob)
                  :updated_at          (now-ms)}
         :where  [:= :workspace_id (->ref workspace-id)]})
      (insert-graph-revision-if-absent! tx-info
        {:workspace-id workspace-id
         :session-state-id session-state-id
         :parent-workspace-id parent-workspace-id
         :ctx ctx
         :advance advance
         :receipt receipt}))))

(defn- refresh-current-graph-revision!
  "Refresh a tracked workspace's CTX at normal turn completion. Metadata stays
   attached to the advance that created the workspace revision."
  [tx-info session-state-id ctx]
  (when-let [workspace-id (:workspace_id
                           (query-one! tx-info
                             {:select [:workspace_id]
                              :from   :session_state
                              :where  [:= :id (->ref session-state-id)]}))]
    (execute! tx-info
      {:update :workspace_graph_revision
       :set    {:ctx        (->blob (freeze-safe ctx))
                :updated_at (now-ms)}
       :where  [:= :workspace_id workspace-id]})))

(defn db-workspace-checkpoint-accept!
  "Atomically repin a session from checkpoint parent to child. When CTX is
   supplied, persist parent and child graph revisions and refresh the dedicated
   task/fact stores in the same transaction. Returns true only on a successful
   compare-and-set."
  [db-info {:keys [session-state-id parent-workspace-id checkpoint-id
                   parent-ctx ctx advance receipt]}]
  (when (and (ds db-info) session-state-id parent-workspace-id checkpoint-id)
    (sqlite-write-tx! db-info
      (fn [tx-info]
        (let [result (execute! tx-info
                       {:update :session_state
                        :set    {:workspace_id (->ref checkpoint-id)}
                        :where  [:and
                                 [:= :id (->ref session-state-id)]
                                 [:= :workspace_id (->ref parent-workspace-id)]]})
              moved? (= 1 (or (:next.jdbc/update-count (first result)) 0))]
          (when (and moved? ctx)
            (insert-graph-revision-if-absent! tx-info
              {:workspace-id parent-workspace-id
               :session-state-id session-state-id
               :ctx parent-ctx})
            (put-graph-revision! tx-info
              {:workspace-id checkpoint-id
               :session-state-id session-state-id
               :parent-workspace-id parent-workspace-id
               :ctx ctx
               :advance advance
               :receipt receipt})
            (write-through-ctx-stores! tx-info session-state-id ctx))
          moved?)))))

(defn db-workspace-checkpoint-move!
  "Atomically repin a session to an existing checkpoint revision and restore
   its CTX stores. Pointer-only legacy checkpoints remain movable and return a
   nil `:ctx`; graph-tracked checkpoints restore both halves of the state."
  [db-info {:keys [session-state-id expected-workspace-id workspace-id]}]
  (when (and (ds db-info) session-state-id expected-workspace-id workspace-id)
    (sqlite-write-tx! db-info
      (fn [tx-info]
        (let [revision (graph-revision-row
                         (query-one! tx-info
                           {:select [:*]
                            :from   :workspace_graph_revision
                            :where  [:= :workspace_id (->ref workspace-id)]}))
              result   (execute! tx-info
                         {:update :session_state
                          :set    {:workspace_id (->ref workspace-id)}
                          :where  [:and
                                   [:= :id (->ref session-state-id)]
                                   [:= :workspace_id (->ref expected-workspace-id)]]})
              moved?   (= 1 (or (:next.jdbc/update-count (first result)) 0))]
          (when (and moved? (:ctx revision))
            (write-through-ctx-stores! tx-info session-state-id (:ctx revision)))
          {:moved? moved?
           :ctx (:ctx revision)
           :graph? (boolean revision)})))))

(defn db-workspace-graph-revision
  "Return the graph revision attached to `workspace-id`, including decoded CTX
   and advance receipt, or nil when the workspace is not graph-tracked."
  [db-info workspace-id]
  (when (and (ds db-info) workspace-id)
    (graph-revision-row
      (query-one! db-info
        {:select [:*]
         :from   :workspace_graph_revision
         :where  [:= :workspace_id (->ref workspace-id)]}))))

(defn db-list-tasks
  "Live tasks for a `session_state`, as an ordered `{key entity-map}` (entity =
   the full thawed task). Only `live = 1` rows - the ledger keeps dropped rows
   too; see `db-list-task-history` for the full append-only view."
  [db-info session-state-id]
  (when (and (ds db-info) session-state-id)
    (into {}
      (map (fn [r] [(:key r) (<-blob (:entity r))]))
      (query! db-info
        {:select   [:key :entity :position]
         :from     :task
         :where    [:and
                    [:= :session_state_id (->ref session-state-id)]
                    [:= :live 1]]
         :order-by [:position]}))))

(defn db-list-task-history
  "EVERY task row ever written for a `session_state` - the append-only ledger
   (live AND dropped/replaced rows). One query, no blob replay. Returns a vec
   of `{:key :title :status :live? :plan-gen :position :entity}` ordered by
   `(plan_gen, position)` so past plan generations group together; `:entity`
   is the full thawed task map."
  [db-info session-state-id]
  (when (and (ds db-info) session-state-id)
    (mapv (fn [r]
            {:key      (:key r)
             :title    (:title r)
             :status   (:status r)
             :live?    (pos? (long (or (:live r) 0)))
             :plan-gen (:plan_gen r)
             :position (:position r)
             :entity   (<-blob (:entity r))})
      (query! db-info
        {:select   [:key :title :status :entity :position :live :plan_gen]
         :from     :task
         :where    [:= :session_state_id (->ref session-state-id)]
         :order-by [[:plan_gen :asc] [:position :asc]]}))))

(defn db-list-facts
  "Live facts for a `session_state`, as `{key entity-map}` (thawed)."
  [db-info session-state-id]
  (when (and (ds db-info) session-state-id)
    (into {}
      (map (fn [r] [(:key r) (<-blob (:entity r))]))
      (query! db-info
        {:select [:key :entity]
         :from   :fact
         :where  [:= :session_state_id (->ref session-state-id)]}))))

(defn db-list-archive
  "Archived entities for a `session_state` as `{id entity-map}` (thawed), keyed
   by the entity's LOGICAL id (`:id` in the blob — the `:session/archived` ctx
   key), not the session-scoped row PK. `stash!` only archives entities that
   HAVE an `:id`, so the blob always carries it — no legacy fallback."
  [db-info session-state-id]
  (when (and (ds db-info) session-state-id)
    (into {}
      (map (fn [r] (let [e (<-blob (:entity r))] [(:id e) e])))
      (query! db-info
        {:select [:entity]
         :from   :archive
         :where  [:= :session_state_id (->ref session-state-id)]}))))

(defn db-update-session-turn!
  "Update the latest session_turn_state with final outcome.

   When `:prior-outcome` is provided (one of `:complete`, `:cancelled`,
   `:error`), it lands in the dedicated `prior_outcome` column so the next turn's handover digest can read
   it without scanning every iteration. The column is bounded by a
   CHECK constraint at the schema level."
  [db-info session-turn-id {:keys [answer-markdown iteration-count duration-ms
                                   status tokens cost prior-outcome ctx]}]
  (when (and (ds db-info) session-turn-id)
    (sqlite-write-tx! db-info
      (fn [tx-info]
        (let [soul-id-s (->ref session-turn-id)
              state     (latest-session-turn-state tx-info soul-id-s)]
          (when state
            (execute! tx-info
              {:update :session_turn_state
               :set    (cond-> {:status                   (normalize-status (or status :done))
                                :iteration_count          (long (or iteration-count 0))
                                :duration_ms              (long (or duration-ms 0))
                                ;; Phase B canonical columns. :input is TOTAL
                                ;; (Anthropic-additive raw values summed at the
                                ;; canonical-normalizer boundary in svar 0.6+).
                                ;; The :input-regular subset is derived from the
                                ;; invariant when the upstream map omits it
                                ;; (older accumulators don't track it explicitly):
                                ;;   regular = input - cache-write - cache-read
                                :input_tokens             (long (or (:input tokens) 0))
                                :input_regular_tokens     (long (or (:input-regular tokens)
                                                                  (max 0 (- (long (or (:input tokens) 0))
                                                                           (long (or (:cache-created tokens) 0))
                                                                           (long (or (:cached tokens) 0))))))
                                :input_cache_write_tokens (long (or (:cache-created tokens) 0))
                                :input_cache_read_tokens  (long (or (:cached tokens) 0))
                                :output_tokens            (long (or (:output tokens) 0))
                                :output_reasoning_tokens  (long (or (:reasoning tokens) 0))
                                :total_cost_usd           (double (or (:total-cost cost) 0.0))}
                         (:model cost)    (assoc :llm_root_model (str (:model cost)))
                         (:provider cost) (assoc :llm_root_provider (name (->kw (:provider cost))))
                         (some? answer-markdown) (assoc :answer_markdown answer-markdown)
                         prior-outcome    (assoc :prior_outcome (name prior-outcome))
                         ;; Nippy-encode the CTX snapshot as of end-of-turn.
                         ;; Live CTX = this row's ctx on the latest turn-state
                         ;; for the latest turn-soul; history = walking the
                         ;; soul chain.
                         (some? ctx)      (assoc :ctx (->blob (freeze-safe ctx))))
               :where  [:= :id (:id state)]})
            ;; Index the raw Markdown answer for FT5 search. `vis/search-text`
            ;; lifts the string through `markdown->ir` and walks the AST so
            ;; fence/link/heading syntax does not pollute the index.
            (when (some? answer-markdown)
              (reindex-search! tx-info "session_turn_state" (:id state) "answer_text" answer-markdown))
            ;; Write-through the dedicated task/fact/archive stores (slice 2),
            ;; atomically with the blob. Resolve the OWNING session_state via the
            ;; turn soul; a sub_loop child resolves to its own child session_state.
            (when (some? ctx)
              (when-let [ss-id (:session_state_id
                                (query-one! tx-info
                                  {:select [:session_state_id]
                                   :from   :session_turn_soul
                                   :where  [:= :id (:session_turn_soul_id state)]}))]
                (write-through-ctx-stores! tx-info ss-id ctx)
                (refresh-current-graph-revision! tx-info ss-id ctx)))))))))

;; Extra workflow persistence removed.

;; =============================================================================
;; Iteration - session_turn_iteration table
;; =============================================================================

(defn- require-iteration-code!
  [opts]
  (when-not (contains? opts :code)
    (throw (ex-info "db-store-iteration! requires flat :code"
             {:type :vis.persistence/iteration-code-required
              :keys (keys opts)})))
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
  (let [code         (require-iteration-code! opts)
        preflight-only? (and (seq forms)
                          (every? (fn [f]
                                    (let [s (some-> (:src f) str clojure.string/triml)]
                                      (and s (clojure.string/starts-with? s "(vis/preflight-error"))))
                            forms))
        code-out (if preflight-only? "" (str code))]
    (cond-> {:code code-out}
      (seq forms)              (assoc :forms (->blob (freeze-safe (vec forms))))
      (some? duration-ms)      (assoc :eval_duration_ms (long duration-ms)))))

(defn- routing-summary-columns
  [routing]
  (let [selected (:selected routing)
        actual   (:actual routing)]
    (cond-> {}
      (:provider selected) (assoc :llm_selected_provider (name (:provider selected)))
      (:model selected)    (assoc :llm_selected_model (:model selected))
      (:provider actual)   (assoc :llm_actual_provider (name (:provider actual)))
      (:model actual)      (assoc :llm_actual_model (:model actual))
      (contains? routing :fallback?) (assoc :llm_fallback (if (:fallback? routing) 1 0)))))

(defn- routing-event-row
  [iteration-id-s now position event]
  (let [event-type (:event/type event)]
    (cond-> {:id                             (or (:event/id event) (new-id))
             :session_turn_iteration_id iteration-id-s
             :position                       position
             :event_type                     (str event-type)
             :event_json                     (->json event)
             :created_at                     now}
      (:provider event)      (assoc :provider (name (:provider event)))
      (:model event)         (assoc :model (:model event))
      (:from-provider event) (assoc :from_provider (name (:from-provider event)))
      (:from-model event)    (assoc :from_model (:from-model event))
      (:to-provider event)   (assoc :to_provider (name (:to-provider event)))
      (:to-model event)      (assoc :to_model (:to-model event))
      (:status event)        (assoc :status (long (:status event)))
      (:reason event)        (assoc :reason (name (:reason event)))
      (:error event)         (assoc :error (str (:error event)))
      (:attempt event)       (assoc :attempt (long (:attempt event)))
      (:delay-ms event)      (assoc :delay_ms (long (:delay-ms event)))
      (:elapsed-ms event)    (assoc :elapsed_ms (long (:elapsed-ms event)))
      (:at-ms event)         (assoc :at_ms (long (:at-ms event))))))

#_{:clojure-lsp/ignore [:clojure-lsp/unused-public-var]}
(defn- provider-request-zone-row
  [iteration-id-s now idx zone-entry]
  (let [content (str (or (:content zone-entry) ""))
        zone    (name (->kw (or (:zone zone-entry) :unknown)))
        cache-class (name (->kw (or (:cache-class zone-entry) :unknown)))]
    (cond-> {:id                              (str (new-uuid))
             :session_turn_iteration_id       iteration-id-s
             :message_index                   (long (or (:message-index zone-entry) 0))
             :zone_index                      (long (or (:zone-index zone-entry) idx))
             :zone                            zone
             :zone_id                         (str (or (:zone-id zone-entry)
                                                     (str zone ":" idx)))
             :cache_class                     cache-class
             :role                            (str (or (:role zone-entry) "unknown"))
             :content_sha256                  (sha256-hex content)
             :char_count                      (long (count content))
             :byte_count                      (utf8-byte-count content)
             :content                         content
             :created_at                      now}
      (:scope zone-entry) (assoc :scope (str (:scope zone-entry)))
      (:source zone-entry) (assoc :source (name (->kw (:source zone-entry))))
      (some? (:estimated-tokens zone-entry))
      (assoc :estimated_tokens (long (:estimated-tokens zone-entry)))
      (some? (:provider-input-tokens zone-entry))
      (assoc :provider_input_tokens (long (:provider-input-tokens zone-entry)))
      (some? (:provider-cache-write-tokens zone-entry))
      (assoc :provider_cache_write_tokens (long (:provider-cache-write-tokens zone-entry)))
      (some? (:provider-cache-read-tokens zone-entry))
      (assoc :provider_cache_read_tokens (long (:provider-cache-read-tokens zone-entry)))
      (some? (:cost-usd zone-entry))
      (assoc :cost_usd (double (:cost-usd zone-entry))))))

(defn- insert-provider-request-zones!
  [tx-info iteration-id-s now zones]
  (doseq [[idx zone-entry] (map-indexed vector (or zones []))]
    (execute! tx-info
      {:insert-into :provider_request_zone
       :values [(provider-request-zone-row iteration-id-s now idx zone-entry)]})))

(defn db-store-iteration!
  "Store one iteration row in a single SQLite transaction.

   Per-form payload travels on the iteration row's `forms` BLOB; there
   is no per-`(def ...)` sidecar persistence. Cross-turn references go
   through `:session/facts` and introspect-form / introspect-iter /
   introspect-turn (DB reads against `session_turn_iteration.forms`).

   Returns the iteration UUID."
  ;; `:duration-ms` is consumed by `prepare-iteration-columns` and lands in
  ;; `eval_duration_ms`; kept off the outer :keys to stop clj-kondo's
  ;; unused-binding lint while staying documented here.
  [db-info {:keys [session-turn-id thinking answer llm-full-duration-ms
                   error
                   llm-routing cache-created-tokens
                   llm-messages llm-provider llm-model llm-raw-response
                   llm-executable-blocks llm-assistant-message llm-returned-empty-code?
                   llm-request-zones tokens cost-usd]
            :as opts}]
  (when (ds db-info)
    (sqlite-write-tx! db-info
      (fn [tx-info]
        (let [iteration-id   (new-uuid)
              iteration-id-s (str iteration-id)
              now            (now-ms)
              session-turn-soul-id-s (when session-turn-id (->ref session-turn-id))
              ;; Need session_turn_state_id (iteration FK points to session_turn_state)
              session-turn-state (when session-turn-soul-id-s
                                   (latest-session-turn-state tx-info session-turn-soul-id-s))
              session-turn-state-id-s (:id session-turn-state)
              ;; Compute position (1-indexed within this session_turn_state)
              ;; Next position is `MAX(position)+1` (monotonic and survives
              ;; row deletions), aliased as `:next_position` so the SQL
              ;; column name and the Clojure key line up. HoneySQL renders
              ;; `:row-count` as the SQL identifier `row_count`, and
              ;; `as-unqualified-lower-maps` returns `:row_count` in the
              ;; row map; reading via `:row-count` (with hyphen) was always
              ;; `nil` and pinned every iteration to the first position, which
              ;; collided with the `UNIQUE (session_turn_state_id, position)`
              ;; constraint on the second iteration of every turn.
              position  (or (:next_position
                             (query-one! tx-info
                               {:select [[[:coalesce [:+ [:max :position] 1] 1]
                                          :next_position]]
                                :from   :session_turn_iteration
                                :where  [:= :session_turn_state_id session-turn-state-id-s]}))
                          1)
              ;; When the caller hands us only legacy :llm-provider /
              ;; :llm-model (no routing summary), synthesise an `actual`
              ;; routing record so the typed `llm_actual_*` columns stay
              ;; populated. This is the canonical landing spot for
              ;; "what provider/model answered".
              routing (or llm-routing
                        (when (or llm-provider llm-model)
                          (cond-> {}
                            llm-provider (assoc-in [:actual :provider] (->kw llm-provider))
                            llm-model    (assoc-in [:actual :model] (str llm-model)))))
              raw-response-s (some-> llm-raw-response str)]
          ;; 1. Iteration row - includes the single-form code payload inline.
          ;;    Hard cut: callers pass :code + :forms (Nippy vec of per-form envelopes).
          (let [iteration-cols (prepare-iteration-columns opts)]
            (execute! tx-info
              {:insert-into :session_turn_iteration
               :values [(cond-> (merge {:id                   iteration-id-s
                                        :session_turn_state_id session-turn-state-id-s
                                        :position             position
                                        :status               (normalize-status (cond answer :done error :error (:error iteration-cols) :error :else :done))
                                        :llm_system_prompt    (when (seq llm-messages)
                                                                (:content (first (filter #(= "system" (:role %)) llm-messages))))
                                        :llm_user_prompt      (when (seq llm-messages) (->json llm-messages))
                                        :llm_thinking         (str/trim (or thinking ""))
                                        :llm_full_duration_ms (long (or llm-full-duration-ms 0))
                                        :llm_returned_empty_code (if llm-returned-empty-code? 1 0)
                                        :llm_executable_code_blocks (when (some? llm-executable-blocks)
                                                                      (->json (vec llm-executable-blocks)))
                                        :llm_assistant_message (when (some? llm-assistant-message)
                                                                 (->json llm-assistant-message))
                                        :created_at           now
                                        :finished_at          now}
                                  iteration-cols)
                          raw-response-s
                          (assoc :llm_raw_response         raw-response-s
                            :llm_raw_response_preview (raw-response-preview raw-response-s)
                            :llm_raw_response_length  (count raw-response-s)
                            :llm_raw_response_sha256  (sha256-hex raw-response-s))
                          ;; Token / cost columns - omitted when nil so the
                          ;; row keeps NULL (the schema marks them nullable
                          ;; for exactly this reason: an LLM call that
                          ;; failed before returning usage produces no
                          ;; tokens, no cost, no fake zeros).
                          ;; Phase B canonical columns. :input is TOTAL,
                          ;; details (regular / cache-write / cache-read)
                          ;; are subsets obeying the canonical invariant.
                          (some? (:input tokens))          (assoc :input_tokens             (long (:input tokens)))
                          ;; :input-regular derived from invariant when absent
                          ;; on per-iter rows. The invariant is enforced at the
                          ;; canonical boundary in svar 0.6+, so this is a safe
                          ;; compute even when the accumulator did not surface it.
                          true                             (assoc :input_regular_tokens
                                                             (long (or (:input-regular tokens)
                                                                     (max 0 (- (long (or (:input tokens) 0))
                                                                              (long (or cache-created-tokens 0))
                                                                              (long (or (:cached tokens) 0)))))))
                          (some? cache-created-tokens)     (assoc :input_cache_write_tokens (long cache-created-tokens))
                          (some? (:cached tokens))         (assoc :input_cache_read_tokens  (long (:cached tokens)))
                          (some? (:output tokens))         (assoc :output_tokens            (long (:output tokens)))
                          (some? (:reasoning tokens))      (assoc :output_reasoning_tokens  (long (:reasoning tokens)))
                          (some? cost-usd)                 (assoc :cost_usd                 (double cost-usd))
                          (seq routing)               (merge (routing-summary-columns routing)))]})
            (insert-provider-request-zones! tx-info iteration-id-s now llm-request-zones)
            (doseq [[idx event] (map-indexed vector (:trace routing))]
              (execute! tx-info
                {:insert-into :llm_routing_event
                 :values [(routing-event-row iteration-id-s now idx event)]}))
            ;; Index thinking manually; code itself is indexed by schema triggers.
            (let [thinking-s (str/trim (or thinking ""))]
              (when-not (= "" thinking-s)
                (reindex-search! tx-info "session_turn_iteration"
                  iteration-id-s "thinking_text" thinking-s)))
            ;; Index per-form ERRORS. Failures lived ONLY inside the Nippy
            ;; `:forms` blob — invisible to FTS — so recall search could
            ;; never answer "what failed earlier?" (session f5aba6d4 hunted
            ;; a prior clj_edit failure and found nothing). One search row
            ;; per iteration, field "errors": the failing form's head line
            ;; + the op-error :message/:hint text.
            (let [errors-s (->> (:forms opts)
                             (keep (fn [f]
                                     (when-let [e (:error f)]
                                       (let [head (some-> (:src f) str str/split-lines first)
                                             msg  (cond
                                                    (string? e) e
                                                    (map? e)    (str/join " — "
                                                                  (keep #(some-> (get e %) str not-empty)
                                                                    [:message :hint]))
                                                    :else       (str e))]
                                         (str/trim (str (or head "") "\n" (or msg "")))))))
                             (remove str/blank?)
                             (str/join "\n\n"))]
              (when-not (str/blank? errors-s)
                (reindex-search! tx-info "session_turn_iteration"
                  iteration-id-s "errors" errors-s))))
          iteration-id)))))

;; =============================================================================
;; Read helpers
;; =============================================================================

(defn- row->turn [row]
  (cond-> {:id                    (->uuid (:soul_id row))
           :type                  :turn
           :session-state-id      (->uuid (:session_state_id row))
           :position              (:position row)
           :user-request          (:user_request row)
           :status                (->kw-back (:status row))
           :created-at            (->date (:soul_created_at row))
           :iteration-count       (long (or (:iteration_count row) 0))
           :duration-ms           (long (or (:duration_ms row) 0))
           ;; Phase B canonical token shape. `:input-tokens` is TOTAL;
           ;; the detail keys are subsets obeying the invariant.
           :input-tokens             (long (or (:input_tokens row) 0))
           :input-regular-tokens     (long (or (:input_regular_tokens row) 0))
           :input-cache-write-tokens (long (or (:input_cache_write_tokens row) 0))
           :input-cache-read-tokens  (long (or (:input_cache_read_tokens row) 0))
           :output-tokens            (long (or (:output_tokens row) 0))
           :output-reasoning-tokens  (long (or (:output_reasoning_tokens row) 0))
           :total-cost               (double (or (:total_cost_usd row) 0.0))}
    ;; Turn rows carry no `title` column; `:name` is not populated
    ;; here. UI/display layers should use `:user-request` for a
    ;; turn label or read the session-level title via `:title`
    ;; on the session map.
    ;; (intentionally no `(:title row)` branch)
    (:answer_markdown row)   (assoc :answer-markdown (:answer_markdown row))
    (:llm_root_provider row) (assoc :provider (->kw-back (:llm_root_provider row)))
    (:llm_root_model row)    (assoc :model (:llm_root_model row))))

(defn- session-turn-soul+state-query
  "HoneySQL fragment joining session_turn_soul + latest session_turn_state."
  [where-clause]
  ;; `:qs.title` intentionally absent: `session_turn_soul` has
  ;; no `title` column. Display layers fall back to `:user_request`
  ;; or the session-level title.
  {:select [:qs.id :qs.session_state_id :qs.position :qs.user_request
            [:qs.created_at :soul_created_at] [:qs.id :soul_id]
            :qst.status
            :qst.answer_markdown
            :qst.iteration_count :qst.duration_ms
            ;; Phase B canonical columns on session_turn_state.
            :qst.input_tokens :qst.input_regular_tokens
            :qst.input_cache_write_tokens :qst.input_cache_read_tokens
            :qst.output_tokens :qst.output_reasoning_tokens
            :qst.total_cost_usd
            :qst.llm_root_provider :qst.llm_root_model]
   :from   [[:session_turn_soul :qs]]
   :join   [[:session_turn_state :qst] [:= :qst.session_turn_soul_id :qs.id]]
   :where  [:and
            where-clause
            [:= :qst.version
             {:select [[[:max :version]]]
              :from   [[:session_turn_state :qst2]]
              :where  [:= :qst2.session_turn_soul_id :qs.id]}]]})

(defn db-list-session-turns-by-status [db-info status]
  (if (ds db-info)
    (mapv row->turn
      (query! db-info (session-turn-soul+state-query [:= :qst.status (normalize-status status)])))
    []))

(defn- attach-prior-outcome [row->turn-map]
  ;; Surface :prior-outcome on the turn map when the column has a value.
  ;; NULL columns surface as an absent key on the returned map.
  (fn [row]
    (cond-> (row->turn-map row)
      (:prior_outcome row) (assoc :prior-outcome (keyword (:prior_outcome row))))))

(defn db-list-session-turns [db-info session-id]
  (if (and (ds db-info) session-id)
    (let [state-ids  (session-state-chain db-info session-id)
          state-rank (zipmap state-ids (range))]
      (if (seq state-ids)
        (mapv (attach-prior-outcome row->turn)
          (sort-by (fn [r]
                     [(get state-rank (:session_state_id r) Long/MAX_VALUE)
                      (or (:position r) 0)
                      (or (:soul_created_at r) 0)])
            (query! db-info
              (-> (session-turn-soul+state-query [:in :qs.session_state_id state-ids])
                (update :select conj :qst.prior_outcome)))))
        []))
    []))

(defn- normalize-routing-event
  [event]
  (cond-> event
    (string? (:event/type event)) (assoc :event/type (keyword (:event/type event)))
    (string? (:reason event))     (assoc :reason (keyword (:reason event)))))

(defn- row-routing-summary
  [row trace]
  (let [selected (cond-> {}
                   (:llm_selected_provider row) (assoc :provider (->kw-back (:llm_selected_provider row)))
                   (:llm_selected_model row)    (assoc :model (:llm_selected_model row)))
        actual   (cond-> {}
                   (:llm_actual_provider row) (assoc :provider (->kw-back (:llm_actual_provider row)))
                   (:llm_actual_model row)    (assoc :model (:llm_actual_model row)))]
    (cond-> {}
      (seq selected) (assoc :selected selected)
      (seq actual)   (assoc :actual actual)
      (some? (:llm_fallback row)) (assoc :fallback? (= 1 (long (:llm_fallback row))))
      (seq trace) (assoc :trace trace))))

(defn- routing-events-for-iteration
  [db-info iteration-id]
  (try
    (mapv (fn [row]
            (normalize-routing-event
              (or (<-json (:event_json row))
                (cond-> {:event/type (some-> (:event_type row) keyword)}
                  (:provider row)      (assoc :provider (->kw-back (:provider row)))
                  (:model row)         (assoc :model (:model row))
                  (:from_provider row) (assoc :from-provider (->kw-back (:from_provider row)))
                  (:from_model row)    (assoc :from-model (:from_model row))
                  (:to_provider row)   (assoc :to-provider (->kw-back (:to_provider row)))
                  (:to_model row)      (assoc :to-model (:to_model row))
                  (:status row)        (assoc :status (:status row))
                  (:reason row)        (assoc :reason (->kw-back (:reason row)))
                  (:error row)         (assoc :error (:error row))
                  (:attempt row)       (assoc :attempt (:attempt row))
                  (:delay_ms row)      (assoc :delay-ms (:delay_ms row))
                  (:elapsed_ms row)    (assoc :elapsed-ms (:elapsed_ms row))
                  (:at_ms row)         (assoc :at-ms (:at_ms row))))))
      (query! db-info
        {:select [:*]
         :from   :llm_routing_event
         :where  [:= :session_turn_iteration_id (->ref iteration-id)]
         :order-by [[:position :asc]]}))
    (catch SQLException _ [])))

(defn- attach-routing
  [iteration routing]
  (if (seq routing)
    (cond-> iteration
      (:selected routing) (assoc :llm-selected (:selected routing))
      (:actual routing)   (assoc :llm-actual (:actual routing))
      (contains? routing :fallback?) (assoc :llm-fallback? (:fallback? routing))
      (seq (:trace routing)) (assoc :llm-routing-trace (vec (:trace routing))))
    iteration))

(defn- row->iteration [row]
  (let [forms-vec (<-blob (:forms row))]
    (cond-> {:id          (->uuid (:id row))
             :type        :iteration
             :position    (:position row)
             :status      (->kw-back (:status row))
             :created-at  (->date (:created_at row))}
      (some? (:code row))                (assoc :code (:code row))
      (some? forms-vec)                  (assoc :forms forms-vec)
      (some? (:eval_duration_ms row))    (assoc :duration-ms (:eval_duration_ms row))
      (some? (:llm_thinking row))         (assoc :thinking (:llm_thinking row))
      (some? (:finished_at row))          (assoc :finished-at (->date (:finished_at row)))
      ;; `:provider` / `:model` on the iteration map mirror what actually
      ;; answered (post-fallback). They are aliases of the typed
      ;; `llm_actual_*` columns; consumers that want the router's
      ;; pre-call selection should read `:llm-selected-provider/model`.
      (some? (:llm_actual_provider row))   (assoc :provider (->kw-back (:llm_actual_provider row)))
      (some? (:llm_actual_model row))      (assoc :model (:llm_actual_model row))
      (some? (:llm_selected_provider row)) (assoc :llm-selected-provider (->kw-back (:llm_selected_provider row)))
      (some? (:llm_selected_model row))    (assoc :llm-selected-model (:llm_selected_model row))
      (some? (:llm_actual_provider row))   (assoc :llm-actual-provider (->kw-back (:llm_actual_provider row)))
      (some? (:llm_actual_model row))      (assoc :llm-actual-model (:llm_actual_model row))
      (some? (:llm_fallback row))          (assoc :llm-fallback? (= 1 (long (:llm_fallback row))))
      ;; Forensic fields - the full transcript surface needs these on
      ;; the data shape even when callers don't render them by default.
      (some? (:llm_system_prompt row))    (assoc :llm-system-prompt (:llm_system_prompt row))
      (some? (:llm_user_prompt row))      (assoc :llm-user-prompt   (<-json (:llm_user_prompt row)))
      (some? (:llm_raw_response row))
      (assoc :llm-raw-response (:llm_raw_response row))
      (some? (:llm_raw_response_preview row))
      (assoc :llm-raw-response-preview (:llm_raw_response_preview row))
      (some? (:llm_raw_response_length row))
      (assoc :llm-raw-response-length (:llm_raw_response_length row))
      (some? (:llm_raw_response_sha256 row))
      (assoc :llm-raw-response-sha256 (:llm_raw_response_sha256 row))
      (some? (:llm_executable_code_blocks row))
      (assoc :llm-executable-blocks (<-json (:llm_executable_code_blocks row)))
      ;; Canonical assistant message svar emitted on this iteration; rehydrated
      ;; on resume so preserved-thinking replay survives a vis restart.
      (some? (:llm_assistant_message row))
      (assoc :llm-assistant-message (<-json (:llm_assistant_message row)))
      (some? (:llm_returned_empty_code row))
      (assoc :returned-empty-code? (= 1 (long (:llm_returned_empty_code row))))
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
      true (assoc :input-tokens             (long   (or (:input_tokens row) 0)))
      true (assoc :input-regular-tokens     (long   (or (:input_regular_tokens row) 0)))
      true (assoc :input-cache-write-tokens (long   (or (:input_cache_write_tokens row) 0)))
      true (assoc :input-cache-read-tokens  (long   (or (:input_cache_read_tokens row) 0)))
      true (assoc :output-tokens            (long   (or (:output_tokens row) 0)))
      true (assoc :output-reasoning-tokens  (long   (or (:output_reasoning_tokens row) 0)))
      true (assoc :cost-usd                 (double (or (:cost_usd row) 0.0))))))

(defn- row->provider-request-zone
  [row]
  (cond-> {:id                 (->uuid (:id row))
           :message-index      (long (or (:message_index row) 0))
           :zone-index         (long (or (:zone_index row) 0))
           :zone               (->kw-back (:zone row))
           :zone-id            (:zone_id row)
           :cache-class        (->kw-back (:cache_class row))
           :role               (:role row)
           :content-sha256     (:content_sha256 row)
           :char-count         (long (or (:char_count row) 0))
           :byte-count         (long (or (:byte_count row) 0))
           :content            (:content row)
           :created-at         (->date (:created_at row))}
    (:scope row) (assoc :scope (:scope row))
    (:source row) (assoc :source (->kw-back (:source row)))
    (some? (:estimated_tokens row))
    (assoc :estimated-tokens (long (:estimated_tokens row)))
    (some? (:provider_input_tokens row))
    (assoc :provider-input-tokens (long (:provider_input_tokens row)))
    (some? (:provider_cache_write_tokens row))
    (assoc :provider-cache-write-tokens (long (:provider_cache_write_tokens row)))
    (some? (:provider_cache_read_tokens row))
    (assoc :provider-cache-read-tokens (long (:provider_cache_read_tokens row)))
    (some? (:cost_usd row))
    (assoc :cost-usd (double (:cost_usd row)))))

(defn- provider-request-zones-for-iteration
  [db-info iteration-id]
  (mapv row->provider-request-zone
    (query! db-info
      {:select [:*]
       :from   :provider_request_zone
       :where  [:= :session_turn_iteration_id (->ref iteration-id)]
       :order-by [[:message_index :asc] [:zone_index :asc]]})))

(defn- iterations-for-state-id
  "Iteration views for one concrete `session_turn_state.id`, position-ordered."
  [db-info state-id-s]
  (mapv (fn [row]
          (let [trace     (routing-events-for-iteration db-info (:id row))
                routing   (row-routing-summary row trace)
                iteration (attach-routing (row->iteration row) routing)
                zones     (provider-request-zones-for-iteration db-info (:id iteration))]
            (cond-> iteration
              (seq zones) (assoc :provider-request-zones zones))))
    (query! db-info
      {:select   [:*] :from :session_turn_iteration
       :where    [:= :session_turn_state_id state-id-s]
       :order-by [[:position :asc]]})))

(defn db-list-session-turn-iterations [db-info session-turn-id]
  ;; `session-turn-id` arrives as EITHER a `session_turn_soul` id (the
  ;; canonical turn id `row->turn`/`db-turn-history` expose, the one history
  ;; views carry) OR a concrete `session_turn_state` id (the engine's
  ;; `:session-turn-id` run result, surfaced to the web as `:engine_turn_id`
  ;; on freshly-finished live turns). Resolve soul -> latest state first; if
  ;; no soul matches (or it has no iterations), treat the id AS a state id.
  ;; The two id spaces are independent random UUIDs, so this never crosses
  ;; wires — it just makes machinery restore work for both callers.
  (if (and (ds db-info) session-turn-id)
    (let [id-s     (->ref session-turn-id)
          state    (latest-session-turn-state db-info id-s)
          via-soul (when state (iterations-for-state-id db-info (:id state)))]
      (if (seq via-soul)
        via-soul
        (iterations-for-state-id db-info id-s)))
    []))

;; -----------------------------------------------------------------------------
;; `db-list-iteration-vars`, `db-latest-var-registry`, `db-var-history*`,
;; `db-store-dependency!`, `db-list-dependencies`, `db-restore-blocks`,
;; and `latest-visible-definition-rows` were retired together with the
;; `definition_*` sidecar tables.
;; Per-form payload lives on `session_turn_iteration.forms`; cross-turn
;; references flow through `:session/facts` and `introspect-form` /
;; `introspect-iter` / `introspect-turn` (DB reads against that BLOB).
;; -----------------------------------------------------------------------------

(defn db-turn-history
  "Per-turn history rows for a session. `:answer-markdown` is the raw
   Markdown source the model wrote in `(done {:answer ...})` (or nil).
   Channels derive IR via `vis/markdown->ir` at render time; persistence
   stays flavor-free."
  [db-info session-id]
  (let [turns (db-list-session-turns db-info session-id)]
    (mapv (fn [idx turn]
            (let [turn-ref        (:id turn)
                  iteration-count (count (db-list-session-turn-iterations db-info turn-ref))]
              (cond-> {:turn-pos             idx
                       :session-turn-id (:id turn)
                       :created-at           (:created-at turn)
                       :user-request         (:user-request turn)
                       :status               (:status turn)
                       :iteration-count      iteration-count}
                (:answer-markdown turn) (assoc :answer-markdown (:answer-markdown turn)))))
      (range)
      turns)))

;; =============================================================================
;; Extension aggregate sidecars
;; =============================================================================

(defn- ->edn-text [v]
  (pr-str v))

(defn- <-edn-text [s]
  (when s
    (try
      (edn/read-string s)
      (catch Throwable _ s))))

(defn- extension-aggregate-sql-row
  [opts id now]
  (let [row {:id                          id
             :extension_id                (str (:extension-id opts))
             :aggregate_key               (->edn-text (:aggregate-key opts))
             :kind                        (->edn-text (:kind opts))
             :index_data                  (->json (:index-data opts))
             :content                     (->blob (:content opts))
             :session_soul_id        (some-> (:session-soul-id opts) ->ref)
             :session_state_id       (some-> (:session-state-id opts) ->ref)
             :session_turn_state_id  (some-> (:session-turn-state-id opts) ->ref)
             :session_turn_iteration_id                (some-> (:iteration-id opts) ->ref)
             :session_turn_iteration_block_index       (:iteration-form-index opts)
             :session_turn_iteration_block_id          (some-> (:iteration-block-id opts) str)
             :created_at                  now
             :updated_at                  now}]
    (when (str/blank? (:extension_id row))
      (throw (ex-info "extension aggregate requires extension-id"
               {:type :extension-aggregate/missing-required
                :key :extension-id})))
    (when (nil? (:aggregate-key opts))
      (throw (ex-info "extension aggregate requires aggregate-key"
               {:type :extension-aggregate/missing-required
                :key :aggregate-key})))
    (when (nil? (:kind opts))
      (throw (ex-info "extension aggregate requires kind"
               {:type :extension-aggregate/missing-required
                :key :kind})))
    (when (and (or (:session_turn_iteration_block_index row) (:session_turn_iteration_block_id row))
            (nil? (:session_turn_iteration_id row)))
      (throw (ex-info "extension aggregate block scope requires iteration-id"
               {:type :extension-aggregate/block-without-iteration})))
    row))

(defn- row->extension-aggregate
  [row]
  (when row
    (let [scope (cond-> {}
                  (:session_soul_id row)       (assoc :session-soul-id (:session_soul_id row))
                  (:session_state_id row)      (assoc :session-state-id (:session_state_id row))
                  (:session_turn_state_id row) (assoc :session-turn-state-id (:session_turn_state_id row))
                  (:session_turn_iteration_id row)               (assoc :iteration-id (:session_turn_iteration_id row))
                  (:session_turn_iteration_block_index row)      (assoc :iteration-form-index (:session_turn_iteration_block_index row))
                  (:session_turn_iteration_block_id row)         (assoc :iteration-block-id (:session_turn_iteration_block_id row)))
          aggregate-key (<-edn-text (:aggregate_key row))]
      {:id            (:id row)
       :extension-id  (:extension_id row)
       :aggregate-key aggregate-key
       :key           aggregate-key
       :kind          (<-edn-text (:kind row))
       :scope         scope
       :index-data    (<-json (:index_data row))
       :content       (<-blob (:content row))
       :created-at    (->date (:created_at row))
       :updated-at    (->date (:updated_at row))})))

(defn- extension-aggregate-clauses
  [opts]
  (cond-> []
    (:id opts)                         (conj [:= :id (->ref (:id opts))])
    (:extension-id opts)               (conj [:= :extension_id (str (:extension-id opts))])
    (contains? opts :aggregate-key)    (conj [:= :aggregate_key (->edn-text (:aggregate-key opts))])
    (contains? opts :kind)             (conj [:= :kind (->edn-text (:kind opts))])
    (:session-soul-id opts)       (conj [:= :session_soul_id (->ref (:session-soul-id opts))])
    (:session-state-id opts)      (conj [:= :session_state_id (->ref (:session-state-id opts))])
    (:session-turn-state-id opts) (conj [:= :session_turn_state_id (->ref (:session-turn-state-id opts))])
    (:iteration-id opts)               (conj [:= :session_turn_iteration_id (->ref (:iteration-id opts))])
    (contains? opts :iteration-form-index)
    (conj [:= :session_turn_iteration_block_index (:iteration-form-index opts)])
    (:iteration-block-id opts)         (conj [:= :session_turn_iteration_block_id (str (:iteration-block-id opts))])
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
    (cond-> {:select [:*]
             :from   :extension_aggregate
             :order-by [[:updated_at :desc] [:created_at :desc]]}
      (seq clauses) (assoc :where (into [:and] clauses))
      (pos-int? (:limit opts)) (assoc :limit (:limit opts))
      (nat-int? (:offset opts)) (assoc :offset (:offset opts)))))

(defn db-create-extension-aggregate!
  [db-info opts]
  (when (ds db-info)
    (sqlite-write-tx! db-info
      (fn [tx-info]
        (let [id  (new-id)
              now (now-ms)]
          (execute! tx-info
            {:insert-into :extension_aggregate
             :values [(extension-aggregate-sql-row opts id now)]})
          (row->extension-aggregate
            (query-one! tx-info
              {:select [:*]
               :from   :extension_aggregate
               :where  [:= :id id]})))))))

(defn db-put-extension-aggregate!
  [db-info opts]
  (when (ds db-info)
    (sqlite-write-tx! db-info
      (fn [tx-info]
        (let [id  (new-id)
              now (now-ms)
              row (extension-aggregate-sql-row opts id now)]
          (execute! tx-info
            {:insert-into :extension_aggregate
             :values [row]
             :on-conflict [:extension_id :aggregate_key :kind :scope_key]
             :do-update-set {:index_data                  (:index_data row)
                             :content                     (:content row)
                             :session_soul_id        (:session_soul_id row)
                             :session_state_id       (:session_state_id row)
                             :session_turn_state_id  (:session_turn_state_id row)
                             :session_turn_iteration_id                (:session_turn_iteration_id row)
                             :session_turn_iteration_block_index       (:session_turn_iteration_block_index row)
                             :session_turn_iteration_block_id          (:session_turn_iteration_block_id row)
                             :updated_at                  now}})
          (row->extension-aggregate
            (query-one! tx-info
              {:select [:*]
               :from   :extension_aggregate
               :where  [:and
                        [:= :extension_id (:extension_id row)]
                        [:= :aggregate_key (:aggregate_key row)]
                        [:= :kind (:kind row)]
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
                          [:= :session_turn_iteration_block_index (:session_turn_iteration_block_index row)]
                          [:is :session_turn_iteration_block_index nil])
                        (if (:session_turn_iteration_block_id row)
                          [:= :session_turn_iteration_block_id (:session_turn_iteration_block_id row)]
                          [:is :session_turn_iteration_block_id nil])]})))))))

(defn db-get-extension-aggregate
  [db-info opts]
  (when (ds db-info)
    (row->extension-aggregate
      (query-one! db-info (assoc (extension-aggregate-select opts) :limit 1)))))

(defn- fts5-quote
  "Escape one token/phrase as a quoted FTS5 string literal — the ONE place a
   user value crosses into the MATCH expression, so operators/punctuation in
   code text are inert and can never break the query."
  [t]
  (str "\"" (str/replace (str t) "\"" "\"\"") "\""))

(declare render-query)

(defn- render-join
  [op nodes]
  (str "(" (str/join (str " " op " ") (map render-query nodes)) ")"))

(defn- render-query
  "Render the BACKEND-NEUTRAL search-query DSL (see
   `persistance/search-query-dsl-doc`) into an FTS5 MATCH expression. Every
   leaf term is `fts5-quote`d, so the result is ALWAYS a valid MATCH string for
   well-formed DSL — there is no raw operator passthrough and nothing to parse.

   A bare string is a convenience: whitespace-split into an implicit-AND of
   escaped terms (so `\"patch auth\"` finds both words, safely). The map forms:
     {:term \"w\"}                  one term
     {:phrase \"a b\"}              adjacent phrase
     {:prefix \"wor\"}              prefix  wor*
     {:all  [q …]}                AND      (a {:not b} child negates: a NOT b)
     {:any  [q …]}                OR
     {:near {:terms [a b] :within k}}  NEAR(a b, k)

   This is the SQLite (FTS5) renderer; a Postgres adapter renders the SAME DSL
   into tsquery. Throws ex-info on malformed DSL (e.g. a bare/all-negative
   :not) — the caller maps that to a structured search error."
  [q]
  (cond
    (string? q)
    (let [terms (remove str/blank? (str/split (str/trim (str q)) #"\s+"))]
      (if (<= (count terms) 1)
        (fts5-quote (or (first terms) q))
        (str "(" (str/join " AND " (map fts5-quote terms)) ")")))

    (map? q)
    (let [{:keys [term phrase prefix all any near]} q
          neg (get q :not)]
      (cond
        term   (fts5-quote term)
        phrase (fts5-quote phrase)
        prefix (str (fts5-quote prefix) "*")
        near   (str "NEAR(" (str/join " " (map fts5-quote (:terms near)))
                 ", " (long (:within near)) ")")
        any    (render-join "OR" any)
        all    (let [neg? (fn [n] (and (map? n) (contains? n :not)))
                     pos  (remove neg? all)
                     negs (map #(get % :not) (filter neg? all))]
                 (when-not (seq pos)
                   (throw (ex-info "search :all needs a positive term (a lone :not can't match)"
                            {:query q})))
                 (reduce (fn [acc n] (str acc " NOT " (render-query n)))
                   (render-join "AND" pos) negs))
        neg    (throw (ex-info "search :not must be a child of :all (it needs a positive sibling)"
                        {:query q}))
        :else  (throw (ex-info "unrecognized search-query node" {:query q}))))

    :else (throw (ex-info "search query must be a string or a DSL map" {:query (str q)}))))

(defn db-search
  "Full-text search over the FTS5 `search` index. Returns a vector of
   hits sorted by FTS5 rank (highest relevance first), each entry
   shaped:

     {:owner-table  String   ; e.g. \"session_turn_state\" / \"session_turn_iteration\"
      :owner-id     String   ; UUID into that table
      :field        String   ; \"answer_text\" | \"thinking_text\" | \"comments_text\" | \"user_request\" | \"expression\"
      :snippet      String   ; FTS5 snippet with `[match]` markers around hit terms
      :rank         double}  ; FTS5 rank (lower = better match)

   `query` is the BACKEND-NEUTRAL search-query DSL — a plain string (implicit
   AND of its words) or a DSL map (`{:all […]}`, `{:any […]}`, `{:phrase …}`,
   `{:prefix …}`, `{:near {:terms […] :within k}}`, `{:not …}` as an `:all`
   child). This adapter renders it to FTS5 via `render-query`; see
   `persistance/search-query-dsl-doc` for the spec.

   `opts` may include:
     :limit        max hits returned (default 50)
     :owner-table  filter to one table (string)
     :field        filter to one field (string) or several (collection
                   of strings, rendered as `field IN (...)`)"
  ([db-info query] (db-search db-info query nil))
  ([db-info query {:keys [limit owner-table field]}]
   (when (and (ds db-info)
           (or (and (string? query) (not (str/blank? query)))
             (and (map? query) (seq query))))
     ;; SQLite FTS5 spells the match operator as `<table> MATCH ?`,
     ;; which HoneySQL's vocabulary doesn't model directly. Raw SQL
     ;; with positional params is the simplest faithful expression.
     (let [base        "SELECT owner_table, owner_id, field, snippet(search, 3, '[', ']', '…', 32) AS snippet, rank FROM search WHERE search MATCH ?"
           match-query (render-query query)
           fields      (cond
                         (and (coll? field) (seq field)) (vec field)
                         (string? field)                 [field]
                         :else                           nil)
           [filt-sql filt-params] (cond-> ["" []]
                                    owner-table (-> (update 0 str " AND owner_table = ?")
                                                  (update 1 conj owner-table))
                                    fields      (-> (update 0 str
                                                      (str " AND field IN ("
                                                        (str/join ", " (repeat (count fields) "?"))
                                                        ")"))
                                                  (update 1 into fields)))
           sql-vec (into [(str base filt-sql " ORDER BY rank ASC LIMIT ?") match-query]
                     (conj filt-params (max 1 (long (or limit 50)))))]
       (mapv
         (fn [row]
           {:owner-table (:owner_table row)
            :owner-id    (:owner_id row)
            :field       (:field row)
            :snippet     (:snippet row)
            :rank        (:rank row)})
         (jdbc/execute! (ds db-info) sql-vec
           {:builder-fn rs/as-unqualified-lower-maps}))))))

(defn db-list-extension-aggregates
  [db-info opts]
  (if (ds db-info)
    (mapv row->extension-aggregate
      (query! db-info (extension-aggregate-select (cond-> opts
                                                    (not (:limit opts)) (assoc :limit 100)))))
    []))

(defn db-delete-extension-aggregates!
  [db-info opts]
  (when (ds db-info)
    (sqlite-write-tx! db-info
      (fn [tx-info]
        (let [clauses (extension-aggregate-clauses opts)
              result  (jdbc/execute! (ds tx-info)
                        (sql/format
                          (cond-> {:delete-from :extension_aggregate}
                            (seq clauses) (assoc :where (into [:and] clauses)))))]
          (or (:next.jdbc/update-count (first result)) 0))))))

(defn db-swap-extension-aggregate!
  [db-info opts f args]
  (when (ds db-info)
    (sqlite-write-tx! db-info
      (fn [tx-info]
        (let [current     (db-get-extension-aggregate tx-info opts)
              next-content (apply f (:content current) args)]
          (db-put-extension-aggregate! tx-info
            (assoc opts
              :index-data (or (:index-data opts) (:index-data current))
              :content next-content)))))))

;; =============================================================================
;; CTX snapshots (per-turn :session/* state)
;; =============================================================================

(defn db-load-latest-ctx
  "Load the CTX snapshot for the session's current workspace revision, falling
   back to the latest session_turn_state snapshot for legacy/untracked tips.
   Returns the decoded CTX map or nil when the session has no persisted CTX.

   This is the resume path: on a new turn, the loop reads this back into
   the ctx-atom so the model picks up where (done …) left off. The cursor
   is intentionally NOT restored — it's iter-local and gets stamped fresh
   by the renderer."
  [db-info session-id]
  (when (and (ds db-info) session-id)
    (let [state-ids (session-state-chain db-info session-id)]
      (when (seq state-ids)
        (or (some-> (query-one! db-info
                      {:select [:r.ctx]
                       :from   [[:session_state :s]]
                       :join   [[:workspace_graph_revision :r]
                                [:= :r.workspace_id :s.workspace_id]]
                       :where  [:= :s.id (last state-ids)]})
              :ctx
              <-blob)
          (when-let [row (first (query! db-info
                                  {:select [:qts.ctx]
                                   :from   [[:session_turn_state :qts]]
                                   :join   [[:session_turn_soul :qs]
                                            [:= :qs.id :qts.session_turn_soul_id]]
                                   :where  [:and
                                            [:in :qs.session_state_id state-ids]
                                            [:<> :qts.ctx nil]]
                                   :order-by [[:qs.position :desc]
                                              [:qts.version :desc]]
                                   :limit  1}))]
            (<-blob (:ctx row))))))))

;; =============================================================================
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
;; =============================================================================

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
        (vec
          (for [row (query! db-info
                      {:select [:qs.position [:qts.ctx :ctx]]
                       :from   [[:session_turn_state :qts]]
                       :join   [[:session_turn_soul :qs]
                                [:= :qs.id :qts.session_turn_soul_id]]
                       :where  [:and
                                [:in :qs.session_state_id state-ids]
                                [:<> :qts.ctx nil]
                                [:= :qts.version
                                 {:select [[[:max :version]]]
                                  :from   [[:session_turn_state :qts2]]
                                  :where  [:= :qts2.session_turn_soul_id :qts.session_turn_soul_id]}]]
                       :order-by [[:qs.position :asc]]})
                :let [decoded (<-blob (:ctx row))]
                :when (some? decoded)]
            [(long (:position row)) decoded]))))))
