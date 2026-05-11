(ns ^{:clj-kondo/config '{:linters {:unused-public-var {:level :off}}}}
  com.blockether.vis.ext.persistance-sqlite.core
  "SQLite store - V1 schema implementation.

   Every public defn in this file is dispatched dynamically by
   `vis-sdk.core/defdelegate` via `ns-resolve`; clj-kondo never sees
   the call sites. The ns-level config above silences
   `:unused-public-var` for the whole file. The actual call surface
   is verified through the storage facade tests.

   Tables (V1__schema.sql):
     conversation_soul, conversation_state,
     conversation_turn_soul, conversation_turn_state,
     iteration,
     expression_soul, expression_state, expression_dependency,
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
   [taoensso.nippy :as nippy])
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
(defn- json-extract [column path] [:json_extract column path])

(defn- ->blob
  "Serialize a Clojure value to a Nippy byte array for BLOB columns."
  ^bytes [v]
  (nippy/freeze v))

(defn- reindex-search!
  "Replace every `search` row for `(owner-table, owner-id, field)` with
   `(vis/search-text v)`. Skips when projection is blank — FT5
   indexing empty rows just inflates the index.

   Used as the write-side hook for the rich text fields the bubble
   layer carries as IR (or markdown strings lifted via `text->ir`):
   answer, thinking, per-block comments. Search hits land back on
   `(owner_table, owner_id)` which the caller translates to the
   right BLOB row."
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
      (throw (maybe-wrap-db-open-error e)))))

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

(defn db-store-stale?
  "True when a persistent SQLite store no longer matches the requested
   db-spec or the file at the same path was replaced under this JVM. When
   true, the facade closes the old shared pool and opens a new one."
  [store db-spec]
  (when (= :persistent (:mode store))
    (or (and (string? db-spec)
          (not= db-spec (:path store)))
      (and (map? db-spec)
        (:path db-spec)
        (not= (:path db-spec) (:path store)))
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
                      (:conversation-soul-id entry)  (assoc :conversation_soul_id (->id (:conversation-soul-id entry)))
                      (:conversation-state-id entry) (assoc :conversation_state_id (->id (:conversation-state-id entry)))
                      (:conversation-turn-soul-id entry)         (assoc :conversation_turn_soul_id (->id (:conversation-turn-soul-id entry)))
                      (:conversation-turn-state-id entry)        (assoc :conversation_turn_state_id (->id (:conversation-turn-state-id entry)))
                      (:iteration-id entry)          (assoc :conversation_turn_iteration_id (->id (:iteration-id entry)))
                      (:expression-soul-id entry)    (assoc :expression_soul_id (->id (:expression-soul-id entry)))
                      (:expression-state-id entry)   (assoc :expression_state_id (->id (:expression-state-id entry))))]})))))

;; =============================================================================
;; Conversation - conversation_soul + conversation_state
;; =============================================================================

(defn db-store-conversation!
  "Create conversation_soul + initial conversation_state (version 0).
   Returns the conversation-soul UUID.

   Metadata layout:
     conversation_soul.metadata  -> {:channel :tui, :external-id \"...\"}
     conversation_state.metadata -> {:system-prompt \"...\", :provider :openai,
                                    :model \"gpt-4o\"}
     conversation_state.title    -> title column

   `:provider` is stored as a keyword (e.g. `:openai`, `:github-copilot`)
   so the snapshot is a faithful round-trip of the provider id; the
   reader (`db-get-conversation`) re-keywordizes it on the way back."
  [db-info {:keys [channel external-id title system-prompt provider model]}]
  (when (ds db-info)
    (sqlite-write-tx! db-info
      (fn [tx-info]
        (let [soul-id  (new-uuid)
              state-id (new-uuid)
              now      (now-ms)]
          (execute! tx-info
            {:insert-into :conversation_soul
             :values [{:id         (str soul-id)
                       :metadata   (->json {:channel     (->kw (or channel :tui))
                                            :external-id external-id})
                       :created_at now}]})
          (execute! tx-info
            {:insert-into :conversation_state
             :values [{:id                   (str state-id)
                       :conversation_soul_id (str soul-id)
                       :title                title
                       :version              0
                       :metadata             (->json (cond-> {:system-prompt (or system-prompt "")
                                                              :model         (or model "")}
                                                       provider (assoc :provider (->kw provider))))
                       :created_at           now}]})
          soul-id)))))

(defn- latest-state-for [db-info soul-id-s]
  (query-one! db-info
    {:select [:*]
     :from   :conversation_state
     :where  [:and
              [:= :conversation_soul_id soul-id-s]
              [:= :version
               {:select [[[:max :version]]]
                :from   :conversation_state
                :where  [:= :conversation_soul_id soul-id-s]}]]}))

(defn db-get-conversation [db-info conversation-id]
  (when (and (ds db-info) conversation-id)
    (let [id (->ref conversation-id)]
      (when-let [soul (query-one! db-info {:select [:*] :from :conversation_soul :where [:= :id id]})]
        (let [state    (latest-state-for db-info id)
              soul-meta (<-json (:metadata soul))
              state-meta (when state (<-json (:metadata state)))]
          (cond-> {:id            (->uuid (:id soul))
                   :type          :conversation
                   :channel       (->kw-back (:channel soul-meta))
                   :external-id   (:external-id soul-meta)
                   :title         (or (:title state) (:title soul-meta))
                   :system-prompt (:system-prompt state-meta)
                   :model         (:model state-meta)
                   :version       (or (:version state) 0)
                   :created-at    (->date (:created_at soul))}
            (:provider state-meta) (assoc :provider (->kw-back (:provider state-meta)))))))))

(defn db-resolve-conversation-id [db-info selector]
  (cond
    (nil? selector) nil
    (= :latest selector)
    (when (ds db-info)
      (when-let [row (query-one! db-info
                       {:select [:id] :from :conversation_soul
                        :order-by [[:created_at :desc]] :limit 1})]
        (->uuid (:id row))))
    (and (vector? selector) (= :id (first selector))) (->uuid (second selector))
    (uuid? selector) selector
    (string? selector) (->uuid selector)
    :else nil))

(defn db-list-conversations [db-info channel]
  (when (ds db-info)
    (let [ch (->kw channel)]
      ;; conversation_soul.metadata contains channel - filter through SQLite JSON1,
      ;; expressed with HoneySQL so predicates/subqueries stay structured.
      (mapv (fn [row]
              (let [soul-meta (<-json (:soul_metadata row))]
                {:id          (->uuid (:id row))
                 :channel     (->kw-back (:channel soul-meta))
                 :external-id (:external-id soul-meta)
                 :title       (or (:state_title row) (:title soul-meta))
                 :version     (:version row)
                 :fork-count  (or (:fork_count row) 0)
                 :created-at  (->date (:created_at row))}))
        (query! db-info
          {:select [:cs.id
                    [:cs.metadata :soul_metadata]
                    :cs.created_at
                    [:s.title :state_title]
                    :s.version
                    [{:select [[[:count :*]]]
                      :from   [[:conversation_state :child]]
                      :where  [:and
                               [:= :child.conversation_soul_id :cs.id]
                               [:not= :child.parent_state_id nil]]}
                     :fork_count]]
           :from   [[:conversation_soul :cs]]
           :join   [[:conversation_state :s]
                    [:= :s.conversation_soul_id :cs.id]]
           :where  [:and
                    [:= (json-extract :cs.metadata "$.channel") ch]
                    [:= :s.version
                     {:select [[[:max :s2.version]]]
                      :from   [[:conversation_state :s2]]
                      :where  [:= :s2.conversation_soul_id :cs.id]}]]
           :order-by [[:cs.created_at :desc]]})))))

(defn db-find-conversation-by-external [db-info channel external-id]
  (when (and (ds db-info) external-id)
    (let [ch  (->kw channel)
          ext (str external-id)]
      (when-let [row (query-one! db-info
                       {:select [:id]
                        :from   :conversation_soul
                        :where  [:and
                                 [:= (json-extract :metadata "$.channel") ch]
                                 [:= (json-extract :metadata "$.\"external-id\"") ext]]})]
        (->uuid (:id row))))))

(defn db-latest-conversation-state-id
  "Return the latest `conversation_state.id` UUID for the soul behind
   `conversation-id` (which is the soul id). Used by the iteration
   loop to bind `TURN_CONVERSATION_STATE_ID` so the agent can reference
   the exact `conversation_state` row this turn was attached to.
   Returns nil when the conversation is unknown or the env has no
   datasource."
  [db-info conversation-id]
  (when (and (ds db-info) conversation-id)
    (let [soul-id-s (->ref conversation-id)]
      (some-> (latest-state-for db-info soul-id-s) :id ->uuid))))

(defn db-update-conversation-title! [db-info conversation-id title]
  (when (and (ds db-info) conversation-id)
    (sqlite-write-tx! db-info
      (fn [tx-info]
        (let [soul-id-s (->ref conversation-id)]
          (when-let [state (latest-state-for tx-info soul-id-s)]
            (execute! tx-info
              {:update :conversation_state
               :set    {:title title}
               :where  [:= :id (:id state)]})))))))

(defn db-delete-conversation-tree! [db-info conversation-soul-id]
  (when (and (ds db-info) conversation-soul-id)
    (sqlite-write-tx! db-info
      (fn [tx-info]
        (execute! tx-info
          {:delete-from :conversation_soul
           :where [:= :id (->id conversation-soul-id)]})))))

;; =============================================================================
;; Fork - branch a conversation at a point
;; =============================================================================

(defn db-list-conversation-states
  "List every `conversation_state` row for the soul behind `conversation-id`,
   oldest version first. Each row maps to
   `{:state-id :version :parent-state-id :title :system-prompt :provider :model
     :created-at :turn-count}` - the raw fork tree of one conversation soul.

   The trunk is `:version 0` with `:parent-state-id nil`. A fork is any row
   whose `:parent-state-id` points at another `:state-id` in the same vector;
   group-by `:parent-state-id` to walk the tree.

   `:turn-count` is the number of `conversation_turn_soul` rows hanging off that specific
   state - cheap to compute, useful when triaging which branch is active.

   Returns `[]` (never nil) when the conversation is unknown or the env has no
   datasource."
  [db-info conversation-id]
  (if (and (ds db-info) conversation-id)
    (let [soul-id-s (->ref conversation-id)
          rows (query! db-info
                 {:select [:cs.id :cs.version :cs.parent_state_id :cs.title
                           :cs.metadata :cs.created_at
                           [{:select [[[:count :*]]]
                             :from   :conversation_turn_soul
                             :where  [:= :conversation_turn_soul.conversation_state_id :cs.id]}
                            :turn_count]]
                  :from   [[:conversation_state :cs]]
                  :where  [:= :cs.conversation_soul_id soul-id-s]
                  :order-by [[:cs.version :asc]]})]
      (mapv (fn [row]
              (let [state-meta (<-json (:metadata row))]
                (cond-> {:state-id        (->uuid (:id row))
                         :version         (:version row)
                         :parent-state-id (some-> (:parent_state_id row) ->uuid)
                         :title           (:title row)
                         :created-at      (->date (:created_at row))
                         :turn-count      (or (:turn_count row) 0)}
                  (:system-prompt state-meta) (assoc :system-prompt (:system-prompt state-meta))
                  (:provider state-meta)      (assoc :provider (->kw-back (:provider state-meta)))
                  (:model state-meta)         (assoc :model (:model state-meta)))))
        rows))
    []))

(defn db-list-conversation-turn-states
  "List every `conversation_turn_state` row (i.e. every retry version) for the soul behind
   `conversation-turn-id`, oldest version first. Each row maps to
   `{:state-id :version :forked-from-conversation-turn-state-id :status :prior-outcome
     :provider :model :created-at :iteration-count}`.

   Version 0 with `:forked-from-conversation-turn-state-id nil` is the original run; any
   higher version is a retry, with `:forked-from-conversation-turn-state-id` pointing at
   the previous `:state-id`. `:iteration-count` is the number of `iteration`
   rows attached to that specific state - retries get their own iteration
   trace.

   Returns `[]` (never nil) when the turn is unknown or the env has no
   datasource."
  [db-info conversation-turn-id]
  (if (and (ds db-info) conversation-turn-id)
    (let [soul-id-s (->ref conversation-turn-id)
          rows (query! db-info
                 {:select [:qst.id :qst.version :qst.forked_from_conversation_turn_state_id
                           :qst.status :qst.prior_outcome
                           :qst.llm_root_provider :qst.llm_root_model
                           :qst.created_at
                           [{:select [[[:count :*]]]
                             :from   :conversation_turn_iteration
                             :where  [:= :conversation_turn_iteration.conversation_turn_state_id :qst.id]}
                            :conversation_turn_iteration_count]]
                  :from   [[:conversation_turn_state :qst]]
                  :where  [:= :qst.conversation_turn_soul_id soul-id-s]
                  :order-by [[:qst.version :asc]]})]
      (mapv (fn [row]
              (cond-> {:state-id                    (->uuid (:id row))
                       :version                     (:version row)
                       :forked-from-conversation-turn-state-id  (some-> (:forked_from_conversation_turn_state_id row) ->uuid)
                       :status                      (->kw-back (:status row))
                       :created-at                  (->date (:created_at row))
                       :iteration-count             (or (:conversation_turn_iteration_count row) 0)}
                (:prior_outcome row)     (assoc :prior-outcome (->kw-back (:prior_outcome row)))
                (:llm_root_provider row) (assoc :provider (->kw-back (:llm_root_provider row)))
                (:llm_root_model row)    (assoc :model (:llm_root_model row))))
        rows))
    []))

(defn db-fork-conversation!
  "Fork a conversation. Creates a new conversation_state with
   parent_state_id pointing to the current latest state.
   The forked state gets a '(fork)' suffix when no title is supplied.
   The parent state gets a '[forked]' suffix to mark the divergence point.
   Returns the new state UUID."
  [db-info conversation-id {:keys [system-prompt provider model title]}]
  (when (ds db-info)
    (sqlite-write-tx! db-info
      (fn [tx-info]
        (let [soul-id-s (->ref conversation-id)]
          (when-let [current (latest-state-for tx-info soul-id-s)]
            (let [new-id       (new-uuid)
                  now          (now-ms)
                  cur-meta     (<-json (:metadata current))
                  parent-title (:title current)
                  fork-title   (or title (str parent-title " (fork)"))
                  new-version  (inc (:version current))]
              (execute! tx-info
                {:insert-into :conversation_state
                 :values [{:id                   (str new-id)
                           :conversation_soul_id soul-id-s
                           :parent_state_id      (:id current)
                           :title                fork-title
                           :version              new-version
                           :metadata             (->json
                                                   (cond-> (or cur-meta {})
                                                     system-prompt (assoc :system-prompt system-prompt)
                                                     provider      (assoc :provider (->kw provider))
                                                     model         (assoc :model model)))
                           :created_at           now}]})
              ;; Mark parent state as forked (idempotent)
              (when (and parent-title
                      (not (str/includes? parent-title "[forked]")))
                (execute! tx-info
                  {:update :conversation_state
                   :set    {:title (str parent-title " [forked]")}
                   :where  [:= :id (:id current)]}))
              new-id)))))))

;; =============================================================================
;; State resolution
;; =============================================================================

(defn- latest-state-id [db-info conversation-id]
  (when (ds db-info)
    (let [soul-id-s (->ref conversation-id)]
      (:id (latest-state-for db-info soul-id-s)))))

;; =============================================================================
;; Turn - conversation_turn_soul + conversation_turn_state
;; =============================================================================

(defn db-store-conversation-turn!
  "Create conversation_turn_soul + initial conversation_turn_state (version 0).
   Returns the conversation-turn-soul UUID."
  [db-info {:keys [parent-conversation-id user-request messages status]}]
  (when (ds db-info)
    (sqlite-write-tx! db-info
      (fn [tx-info]
        (let [soul-id        (new-uuid)
              state-id       (new-uuid)
              now            (now-ms)
              state-id-s     (latest-state-id tx-info parent-conversation-id)
              turn-position  (or (:next_position
                                  (query-one! tx-info
                                    {:select [[[:coalesce [:+ [:max :position] 1] 1]
                                               :next_position]]
                                     :from   :conversation_turn_soul
                                     :where  [:= :conversation_state_id state-id-s]}))
                               1)
              user-request-s (or user-request "")]
          (execute! tx-info
            {:insert-into :conversation_turn_soul
             :values [{:id                    (str soul-id)
                       :conversation_state_id state-id-s
                       :position              turn-position
                       :title                 (subs user-request-s 0 (min (count user-request-s) 100))
                       :user_request          user-request-s
                       :created_at            now}]})
          (execute! tx-info
            {:insert-into :conversation_turn_state
             :values [{:id            (str state-id)
                       :conversation_turn_soul_id (str soul-id)
                       :version       0
                       :status        (normalize-status (or status :running))
                       :metadata      (->json (when messages {:messages messages}))
                       :created_at    now}]})
          soul-id)))))

(defn- latest-conversation-turn-state [db-info conversation-turn-soul-id-s]
  (query-one! db-info
    {:select [:*]
     :from   :conversation_turn_state
     :where  [:and
              [:= :conversation_turn_soul_id conversation-turn-soul-id-s]
              [:= :version
               {:select [[[:max :version]]]
                :from   :conversation_turn_state
                :where  [:= :conversation_turn_soul_id conversation-turn-soul-id-s]}]]}))

(defn db-retry-conversation-turn!
  "Create a new conversation_turn_state (version N+1) for an existing conversation_turn_soul.
   Used when re-running a turn with a different provider/model or settings.
   Returns the new conversation-turn-state UUID."
  [db-info conversation-turn-soul-id {:keys [status provider model]}]
  (when (ds db-info)
    (sqlite-write-tx! db-info
      (fn [tx-info]
        (let [soul-id-s (->ref conversation-turn-soul-id)
              current   (latest-conversation-turn-state tx-info soul-id-s)
              new-id    (new-uuid)
              now       (now-ms)]
          (when current
            (execute! tx-info
              {:insert-into :conversation_turn_state
               :values [(cond-> {:id                         (str new-id)
                                 :conversation_turn_soul_id              soul-id-s
                                 :forked_from_conversation_turn_state_id (:id current)
                                 :version                    (inc (:version current))
                                 :status                     (normalize-status (or status :running))
                                 :llm_root_model             model
                                 :created_at                 now}
                          provider (assoc :llm_root_provider (name (->kw provider))))]})
            new-id))))))

(defn db-update-conversation-turn!
  "Update the latest conversation_turn_state with final outcome.

   When `:prior-outcome` is provided (one of `:complete`, `:cancelled`,
   `:error`), it lands in the dedicated `prior_outcome` column so the next turn's handover digest can read
   it without scanning every iteration. The column is bounded by a
   CHECK constraint at the schema level."
  [db-info conversation-turn-id {:keys [answer iteration-count duration-ms
                                        status tokens cost prior-outcome]}]
  (when (and (ds db-info) conversation-turn-id)
    (sqlite-write-tx! db-info
      (fn [tx-info]
        (let [soul-id-s (->ref conversation-turn-id)
              state     (latest-conversation-turn-state tx-info soul-id-s)]
          (when state
            (execute! tx-info
              {:update :conversation_turn_state
               :set    (cond-> {:status (normalize-status (or status :done))
                                :metadata (->json
                                            (merge (<-json (:metadata state))
                                              (cond-> {:iteration-count (or iteration-count 0)
                                                       :duration-ms     (or duration-ms 0)}
                                                (:input tokens)     (assoc :input-tokens     (long (:input tokens)))
                                                (:output tokens)    (assoc :output-tokens    (long (:output tokens)))
                                                (:reasoning tokens) (assoc :reasoning-tokens (long (:reasoning tokens)))
                                                (:cached tokens)    (assoc :cached-tokens    (long (:cached tokens)))
                                                (:total-cost cost)  (assoc :total-cost       (double (:total-cost cost)))
                                                (:provider cost)    (assoc :provider         (->kw (:provider cost)))
                                                (:model cost)       (assoc :model            (str (:model cost))))))}
                         (:model cost)    (assoc :llm_root_model (str (:model cost)))
                         (:provider cost) (assoc :llm_root_provider (name (->kw (:provider cost))))
                         (some? answer)   (assoc :answer (->blob answer))
                         prior-outcome    (assoc :prior_outcome (name prior-outcome)))
               :where  [:= :id (:id state)]})
            ;; Index answer plain-text projection for FT5 search. The
            ;; canonical IR stays in `:answer` BLOB; FTS hits map back
            ;; via `(owner_table, owner_id) = ("conversation_turn_state",
            ;; state-id)` and the caller thaws the BLOB.
            (when (some? answer)
              (reindex-search! tx-info "conversation_turn_state" (:id state) "answer_text" answer))))))))

;; Extra workflow persistence removed.

;; =============================================================================
;; SCI var serialization helpers
;; =============================================================================

(defn- runtime-object?
  "True when `v` is a runtime-only object (function, SCI var, SCI internal)
   that cannot be meaningfully serialized as data. These get a :vis/ref marker
   so the system knows to re-eval from the `expr` column to reconstruct them."
  [v]
  (or (fn? v)
    (instance? clojure.lang.Var v)
    (instance? java.util.concurrent.Future v)
    (and (some? v) (str/starts-with? (.getName (class v)) "sci."))))

(defn- freeze-safe
  "Prepare `v` for nippy serialization.

   Rules:
   - Realized collections (vectors, sets, maps, lists) -> walk recursively, freeze.
   - Lazy seqs -> `{:vis/ref :expr}`. A lazy seq IS a computation. Its durable
     form is the source code that produces it, not a materialized snapshot.
     Re-eval from :expr to reconstruct.
   - Functions, SCI vars -> `{:vis/ref :expr}`. Same reason.
   - Plain scalars (strings, numbers, keywords, etc.) -> pass through."
  ([v] (freeze-safe v 8))
  ([v depth]
   (cond
     (nil? v)                         nil
     (zero? depth)                    {:vis/ref :depth-exceeded}
     (runtime-object? v)              {:vis/ref :expr}
     (instance? clojure.lang.LazySeq v) {:vis/ref :expr}
     (map? v)                         (persistent!
                                        (reduce-kv
                                          (fn [m k val]
                                            (assoc! m k (freeze-safe val (dec depth))))
                                          (transient {})
                                          v))
     (vector? v)                      (mapv #(freeze-safe % (dec depth)) v)
     (set? v)                         (into #{} (map #(freeze-safe % (dec depth))) v)
     (list? v)                        (doall (map #(freeze-safe % (dec depth)) v))
     (seq? v)                         {:vis/ref :expr}
     :else                            v)))

;; =============================================================================
;; Iteration - iteration table
;; =============================================================================

(defn- normalize-role
  "Per PLAN §1.3: 4-value block role enum.
   Maps the block's `:role` (or legacy `:rendering-kind`) to one of
   `#{:answer :nudge :tool :thinking}`. Errors are derived from
   `:op/success?` on the envelope, NOT a separate `:vis/error` role.

   Legacy value mapping (for blobs persisted before the cutover):
     :vis/answer                  -> :answer
     :vis/tool, :vis/sci          -> :tool
     :vis/system, :vis/diagnostic -> :nudge
     :vis/error                   -> derived from :error -> default :tool"
  [exec]
  (let [v (or (:role exec) (:rendering-kind exec))]
    (case v
      (:answer :tool :nudge :thinking) v
      :vis/answer                      :answer
      (:vis/tool :vis/sci)             :tool
      (:vis/system :vis/diagnostic)    :nudge
      :tool)))

(defn- prepare-blocks-blob
  "Encode the per-iteration code-block log as one Nippy-frozen vec."
  [_turn-id-s _iteration-position blocks]
  (let [blank? (fn [s] (or (nil? s) (and (string? s) (str/blank? s))))]
    (->> (or blocks [])
      (map-indexed
        (fn [pos exec]
          (cond-> {:idx pos
                   :code (:code exec)
                   :role (normalize-role exec)}
            (some? (:comment exec))            (assoc :comment (:comment exec))
            (some? (:result exec))             (assoc :result (freeze-safe (:result exec)))
            (seq (:journal exec))              (assoc :journal (freeze-safe (:journal exec)))
            (seq (:channel exec))              (assoc :channel (freeze-safe (:channel exec)))
;; Per PLAN §2.1 + §7.3.5: :error is the structured :op/error map
            ;; (Nippy serialises maps natively). Was previously stringified via
            ;; (str ...) which produced unreadable Clojure-map literal text.
            (some? (:error exec))              (assoc :error (freeze-safe (:error exec)))
            (not (blank? (:stdout exec)))      (assoc :stdout (:stdout exec))
            (not (blank? (:stderr exec)))      (assoc :stderr (:stderr exec))
            (some? (:execution-time-ms exec))  (assoc :duration-ms (:execution-time-ms exec))
            (:timeout? exec)                   (assoc :timeout? true)
            (:repaired? exec)                  (assoc :repaired? true))))
      vec)))

(defn db-store-iteration!
  "Store one iteration row + per-`(def ...)` expression_soul/expression_state
   rows. The iteration's full code-block log is written inline as a
   Nippy blob in `iteration.blocks` (no per-call rows; see V1 schema
   migration banner). Returns the iteration UUID."
  [db-info {:keys [conversation-turn-id blocks thinking answer answer-form-idx duration-ms vars error metadata
                   llm-messages llm-provider llm-model llm-raw-response llm-executable-code
                   llm-executable-blocks llm-assistant-message tokens cost-usd]}]
  (when (ds db-info)
    (sqlite-write-tx! db-info
      (fn [tx-info]
        (let [iteration-id   (new-uuid)
              iteration-id-s (str iteration-id)
              now            (now-ms)
              conversation-turn-soul-id-s (when conversation-turn-id (->ref conversation-turn-id))
              ;; Need conversation_turn_state_id (iteration FK points to conversation_turn_state)
              conversation-turn-state (when conversation-turn-soul-id-s
                                        (latest-conversation-turn-state tx-info conversation-turn-soul-id-s))
              conversation-turn-state-id-s (:id conversation-turn-state)
              ;; Need conversation_state_id for expression_soul
              conversation-state-id (when conversation-turn-state
                                      (:conversation_state_id
                                       (query-one! tx-info
                                         {:select [:conversation_state_id]
                                          :from   :conversation_turn_soul
                                          :where  [:= :id conversation-turn-soul-id-s]})))
              ;; Compute position (1-indexed within this conversation_turn_state)
              ;; Next position is `MAX(position)+1` (monotonic and survives
              ;; row deletions), aliased as `:next_position` so the SQL
              ;; column name and the Clojure key line up. HoneySQL renders
              ;; `:row-count` as the SQL identifier `row_count`, and
              ;; `as-unqualified-lower-maps` returns `:row_count` in the
              ;; row map; reading via `:row-count` (with hyphen) was always
              ;; `nil` and pinned every iteration to the first position, which
              ;; collided with the `UNIQUE (conversation_turn_state_id, position)`
              ;; constraint on the second iteration of every turn.
              position  (or (:next_position
                             (query-one! tx-info
                               {:select [[[:coalesce [:+ [:max :position] 1] 1]
                                          :next_position]]
                                :from   :conversation_turn_iteration
                                :where  [:= :conversation_turn_state_id conversation-turn-state-id-s]}))
                          1)
              raw-response-s (some-> llm-raw-response str)]
          ;; 1. Iteration row - includes the full block log inline as
          ;;    `iteration.blocks BLOB` (Nippy-encoded vec). Per-form
          ;;    observations live here; expression_soul/expression_state
          ;;    are only for named vars.
          (let [blocks-vec (prepare-blocks-blob conversation-turn-soul-id-s position blocks)]
            (execute! tx-info
              {:insert-into :conversation_turn_iteration
               :values [(cond-> {:id                   iteration-id-s
                                 :conversation_turn_state_id       conversation-turn-state-id-s
                                 :position             position
                                 :status               (normalize-status (cond answer :done error :error :else :done))
                                 :llm_system_prompt    (when (seq llm-messages)
                                                         (:content (first (filter #(= "system" (:role %)) llm-messages))))
                                 :llm_user_prompt      (when (seq llm-messages) (->json llm-messages))
                                 :llm_provider         (when llm-provider (name (->kw llm-provider)))
                                 :llm_model            llm-model
                                 :llm_thinking         (str/trim (or thinking ""))
                                 :llm_full_duration_ms (or duration-ms 0)
                                 :llm_error            (when error (->json (if (map? error) error {:message (str error)})))
                                 :llm_returned_empty_blocks (if (empty? blocks) 1 0)
                                 :metadata             (when metadata (->json metadata))
                                 :blocks               (->blob blocks-vec)
                                 :llm_executable_code (some-> llm-executable-code str)
                                 :llm_executable_blocks (when (some? llm-executable-blocks)
                                                          (->json (vec llm-executable-blocks)))
                                 :llm_assistant_message (when (some? llm-assistant-message)
                                                          (->json llm-assistant-message))
                                 :created_at           now
                                 :finished_at          now}
                          (some? answer-form-idx)
                          (assoc :answer_form_idx answer-form-idx)
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
                          (some? (:input tokens))     (assoc :llm_input_tokens     (long (:input tokens)))
                          (some? (:output tokens))    (assoc :llm_output_tokens    (long (:output tokens)))
                          (some? (:reasoning tokens)) (assoc :llm_reasoning_tokens (long (:reasoning tokens)))
                          (some? (:cached tokens))    (assoc :llm_cached_tokens    (long (:cached tokens)))
                          (some? cost-usd)            (assoc :llm_cost_usd         (double cost-usd)))]})
            ;; Index thinking + per-block comment plain-text projections
            ;; for FTS5 search. Both are markdown strings; `vis/search-text`
            ;; lifts them via `text->ir` then concatenates.
            (let [thinking-s (str/trim (or thinking ""))]
              (when-not (= "" thinking-s)
                (reindex-search! tx-info "conversation_turn_iteration"
                  iteration-id-s "thinking_text" thinking-s)))
            (let [comments (->> blocks-vec (keep :comment) (remove str/blank?))]
              (when (seq comments)
                (reindex-search! tx-info "conversation_turn_iteration"
                  iteration-id-s "comments_text"
                  (clojure.string/join "\n\n" comments)))))
          ;; 3. Vars -> expression_soul (kind=var, stateful) + expression_state (versioned)
          (when conversation-state-id
            (doseq [{:keys [name value code time-ms metadata]} (or vars [])]
              (when name
                (let [name-s (str name)
                      ;; Find-or-create: partial unique index can't use ON CONFLICT
                      existing (:id (query-one! tx-info
                                      {:select [:id]
                                       :from   :expression_soul
                                       :where  [:and
                                                [:= :conversation_state_id conversation-state-id]
                                                [:= :name name-s]]}))
                      soul-id (or existing
                                (let [new-id (new-id)]
                                  (execute! tx-info
                                    {:insert-into :expression_soul
                                     :values [{:id                    new-id
                                               :conversation_state_id conversation-state-id
                                               :kind                  "var"
                                               :state_mode            "stateful"
                                               :name                  name-s
                                               :created_at            now}]})
                                  new-id))
                      max-ver (or (:v (query-one! tx-info
                                        {:select [[[:max :version] :v]]
                                         :from   :expression_state
                                         :where  [:= :expression_soul_id soul-id]}))
                                -1)]
                  (execute! tx-info
                    {:insert-into :expression_state
                     :values [{:id                 (new-id)
                               :expression_soul_id soul-id
                               :conversation_turn_iteration_id       iteration-id-s
                               :version            (inc max-ver)
                               :success            1
                               :expr               code
                               :result             (->blob (freeze-safe value))
                               :metadata           (->json (cond-> {}
                                                             time-ms  (assoc :time-ms time-ms)
                                                             metadata (assoc :metadata metadata)))
                               :created_at         now}]})))))
          iteration-id)))))

;; =============================================================================
;; Read helpers
;; =============================================================================

(defn- row->turn [row]
  (let [state-meta (<-json (:state_metadata row))
        ;; `:provider` may live in either the JSON metadata or the
        ;; dedicated `llm_root_provider` column. The column is the
        ;; canonical source (typed, indexable); the JSON copy is a
        ;; convenience for older readers. Prefer the column, fall
        ;; back to the JSON, and always re-keywordize.
        provider   (or (:llm_root_provider row) (:provider state-meta))]
    (cond-> {:id                    (->uuid (:soul_id row))
             :type                  :turn
             :conversation-state-id (->uuid (:conversation_state_id row))
             :position              (:position row)
             :user-request          (:user_request row)
             :status                (->kw-back (:status row))
             :created-at            (->date (:soul_created_at row))}
      (:title row)              (assoc :name (:title row))
      (:answer row)             (assoc :answer (<-blob (:answer row)))
      (:iteration-count state-meta)  (assoc :iteration-count (:iteration-count state-meta))
      (:duration-ms state-meta)      (assoc :duration-ms (:duration-ms state-meta))
      provider                  (assoc :provider (->kw-back provider))
      (:model state-meta)       (assoc :model (:model state-meta))
      (:input-tokens state-meta)     (assoc :input-tokens (:input-tokens state-meta))
      (:output-tokens state-meta)    (assoc :output-tokens (:output-tokens state-meta))
      (:reasoning-tokens state-meta) (assoc :reasoning-tokens (:reasoning-tokens state-meta))
      (:cached-tokens state-meta)    (assoc :cached-tokens (:cached-tokens state-meta))
      (:total-cost state-meta)       (assoc :total-cost (:total-cost state-meta)))))

(defn- conversation-turn-soul+state-query
  "HoneySQL fragment joining conversation_turn_soul + latest conversation_turn_state."
  [where-clause]
  {:select [:qs.id :qs.conversation_state_id :qs.position :qs.title :qs.user_request
            [:qs.created_at :soul_created_at] [:qs.id :soul_id]
            :qst.status :qst.metadata [:qst.metadata :state_metadata]
            :qst.answer
            :qst.llm_root_provider :qst.llm_root_model]
   :from   [[:conversation_turn_soul :qs]]
   :join   [[:conversation_turn_state :qst] [:= :qst.conversation_turn_soul_id :qs.id]]
   :where  [:and
            where-clause
            [:= :qst.version
             {:select [[[:max :version]]]
              :from   [[:conversation_turn_state :qst2]]
              :where  [:= :qst2.conversation_turn_soul_id :qs.id]}]]})

(defn db-list-conversation-turns-by-status [db-info status]
  (if (ds db-info)
    (mapv row->turn
      (query! db-info (conversation-turn-soul+state-query [:= :qst.status (normalize-status status)])))
    []))

(defn- attach-prior-outcome [row->turn-map]
  ;; Surface :prior-outcome on the turn map when the column has a value.
  ;; NULL columns surface as an absent key on the returned map.
  (fn [row]
    (cond-> (row->turn-map row)
      (:prior_outcome row) (assoc :prior-outcome (keyword (:prior_outcome row))))))

(defn db-list-conversation-turns [db-info conversation-id]
  (if (and (ds db-info) conversation-id)
    (let [state-id-s (latest-state-id db-info conversation-id)]
      (when state-id-s
        (mapv (attach-prior-outcome row->turn)
          (query! db-info
            (-> (conversation-turn-soul+state-query [:= :qs.conversation_state_id state-id-s])
              (update :select conj :qst.prior_outcome)
              (assoc :order-by [[:qs.position :asc]]))))))
    []))

(defn- row->iteration [row]
  (cond-> {:id          (->uuid (:id row))
           :type        :iteration
           :position    (:position row)
           :status      (->kw-back (:status row))
           :created-at  (->date (:created_at row))}
    (some? (:llm_thinking row))         (assoc :thinking (:llm_thinking row))
    (some? (:llm_error row))            (assoc :error (:llm_error row))
    (some? (:llm_full_duration_ms row)) (assoc :duration-ms (:llm_full_duration_ms row))
    (some? (:finished_at row))          (assoc :finished-at (->date (:finished_at row)))
    (some? (:llm_provider row))         (assoc :provider (->kw-back (:llm_provider row)))
    (some? (:llm_model row))            (assoc :model (:llm_model row))
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
    (some? (:llm_executable_code row))
    (assoc :llm-executable-code (:llm_executable_code row))
    (some? (:llm_executable_blocks row))
    (assoc :llm-executable-blocks (<-json (:llm_executable_blocks row)))
    ;; Canonical assistant message svar emitted on this iteration; rehydrated
    ;; on resume so preserved-thinking replay survives a vis restart.
    (some? (:llm_assistant_message row))
    (assoc :llm-assistant-message (<-json (:llm_assistant_message row)))
    (some? (:answer_form_idx row))      (assoc :answer-form-idx   (:answer_form_idx row))
    (some? (:llm_returned_empty_blocks row))
    (assoc :returned-empty-blocks? (= 1 (long (:llm_returned_empty_blocks row))))
    ;; Token / cost columns - ALWAYS present on the read side, with
    ;; sane numeric defaults (0 tokens, $0.00 cost) when the column
    ;; is NULL. Callers can assume `(:input-tokens it)` is a long
    ;; and `(:cost-usd it)` is a double without `or`-padding at
    ;; every use site. The schema CHECK still rejects negative
    ;; values; absent = zero by convention.
    true (assoc :input-tokens     (long   (or (:llm_input_tokens row) 0)))
    true (assoc :output-tokens    (long   (or (:llm_output_tokens row) 0)))
    true (assoc :reasoning-tokens (long   (or (:llm_reasoning_tokens row) 0)))
    true (assoc :cached-tokens    (long   (or (:llm_cached_tokens row) 0)))
    true (assoc :cost-usd         (double (or (:llm_cost_usd row) 0.0)))
    ;; Iteration metadata (JSON) carries the per-iteration metrics:
    ;; :var-history-recall-count plus per-iteration extension info.
    (some? (:metadata row))             (assoc :metadata (<-json (:metadata row)))))

(defn db-list-conversation-turn-iterations [db-info conversation-turn-id]
  (if (and (ds db-info) conversation-turn-id)
    (let [soul-id-s (->ref conversation-turn-id)
          state     (latest-conversation-turn-state db-info soul-id-s)]
      (when state
        (mapv row->iteration
          (query! db-info
            {:select [:*] :from :conversation_turn_iteration
             :where [:= :conversation_turn_state_id (:id state)]
             :order-by [[:position :asc]]}))))
    []))

(defn db-list-iteration-vars [db-info iteration-id]
  (if (and (ds db-info) iteration-id)
    (let [iteration-id-s (->ref iteration-id)]
      (mapv (fn [r]
              {:name    (:name r)
               :value   (<-blob (:result r))
               :code    (:expr r)
               :version (:version r)})
        (query! db-info
          {:select [:es.name [:est.result :result] [:est.expr :expr] :est.version]
           :from   [[:expression_state :est]]
           :join   [[:expression_soul :es] [:= :est.expression_soul_id :es.id]]
           :where  [:and
                    [:= :est.conversation_turn_iteration_id iteration-id-s]
                    [:= :es.kind "var"]]
           :order-by [[:est.created_at :asc]]})))
    []))

(defn db-list-iteration-blocks
  "Return code blocks for an iteration, ordered by 0-based `:idx`.
   Each entry carries :idx + :code (and optionally :comment :result
   :error :stdout :stderr :duration-ms :timeout? :repaired?).

   Source: the Nippy-encoded `iteration.blocks` BLOB. Per-form calls are
   read directly from that blob; expression_soul/expression_state are
   reserved for named vars."
  [db-info iteration-id]
  (if (and (ds db-info) iteration-id)
    (let [iteration-id-s (->ref iteration-id)
          row            (query-one! db-info
                           {:select [:blocks]
                            :from   :conversation_turn_iteration
                            :where  [:= :id iteration-id-s]})
          decoded        (<-blob (:blocks row))]
      (vec (or decoded [])))
    []))

(defn db-latest-var-registry
  ([db-info conversation-id] (db-latest-var-registry db-info conversation-id {}))
  ([db-info conversation-id _opts]
   (if (and (ds db-info) conversation-id)
     (let [state-id-s (latest-state-id db-info conversation-id)]
       (when state-id-s
         (into {}
           (map (fn [r]
                  [(symbol (:name r))
                   {:value      (<-blob (:result r))
                    :code       (:expr r)
                    :version    (:version r)
                    :conversation-turn-id   (->uuid (:conversation_turn_soul_id r))
                    :created-at (->date (:created_at r))}]))
           (query! db-info
             {:select [:es.name :est.result :est.expr :est.version :est.created_at
                       :qst.conversation_turn_soul_id]
              :from   [[:expression_soul :es]]
              :join   [[:expression_state :est] [:= :est.expression_soul_id :es.id]
                       [:conversation_turn_iteration :it]         [:= :it.id :est.conversation_turn_iteration_id]
                       [:conversation_turn_state :qst]      [:= :qst.id :it.conversation_turn_state_id]]
              :where  [:and
                       [:= :es.conversation_state_id state-id-s]
                       [:= :es.kind "var"]
                       [:= :est.version
                        {:select [[[:max :version]]]
                         :from   [[:expression_state :est2]]
                         :where  [:= :est2.expression_soul_id :es.id]}]]}))))
     {})))

(defn- var-result-ref?
  [v]
  (and (map? v) (= :expr (:vis/ref v))))

(defn- safe-defn-source?
  [expr]
  (boolean
    (when (string? expr)
      (re-find #"^\s*\((?:clojure.core/)?defn(?:-|\s)" expr))))

(defn- var-kind
  [value expr]
  (cond
    (safe-defn-source? expr) :fn
    (var-result-ref? value)  :unavailable
    :else                   :data))

(defn- var-restorable?
  [value expr]
  (or (not (var-result-ref? value))
    (safe-defn-source? expr)))

(defn- row->var-info
  [r]
  (cond-> {:conversation-state-id (:conversation_state_id r)}
    (:conversation_turn_soul_id r) (assoc :conversation-turn-id (->uuid (:conversation_turn_soul_id r)))
    (:conversation_turn_iteration_id r)             (assoc :iteration-id (->uuid (:conversation_turn_iteration_id r)))
    (:iteration_position r)       (assoc :iteration-position (:iteration_position r))))

(defn- row->bindings-entry
  [r]
  (let [value (<-blob (:result r))]
    (cond-> {:name        (symbol (:name r))
             :version     (:version r)
             :kind        (var-kind value (:expr r))
             :restorable? (var-restorable? value (:expr r))
             :created-at  (->date (:created_at r))
             :info  (row->var-info r)}
      (not (var-restorable? value (:expr r)))
      (assoc :status :unavailable
        :reason :unsafe-restore
        :guidance "Recreate intentionally to persist a new version."))))

(defn db-var-history-index
  "Compact, value-free latest-version symbol index for a conversation branch.
   Default limit is 200, newest definitions first. Values are intentionally
   omitted; callers that know a symbol can request that symbol's history."
  ([db-info conversation-id] (db-var-history-index db-info conversation-id {}))
  ([db-info conversation-id {:keys [limit] :or {limit 200}}]
   (if (and (ds db-info) conversation-id)
     (let [state-id-s (latest-state-id db-info conversation-id)]
       (if state-id-s
         (mapv row->bindings-entry
           (query! db-info
             (cond-> {:select [:es.name :es.conversation_state_id
                               :est.version :est.result :est.expr :est.created_at
                               [:est.conversation_turn_iteration_id :conversation_turn_iteration_id]
                               [:it.position :iteration_position]
                               :qst.conversation_turn_soul_id]
                      :from   [[:expression_soul :es]]
                      :join   [[:expression_state :est] [:= :est.expression_soul_id :es.id]
                               [:conversation_turn_iteration :it] [:= :it.id :est.conversation_turn_iteration_id]
                               [:conversation_turn_state :qst] [:= :qst.id :it.conversation_turn_state_id]]
                      :where  [:and
                               [:= :es.conversation_state_id state-id-s]
                               [:= :es.kind "var"]
                               [:= :est.version
                                {:select [[[:max :version]]]
                                 :from   [[:expression_state :est2]]
                                 :where  [:= :est2.expression_soul_id :es.id]}]]
                      :order-by [[:est.created_at :desc] [:es.name :asc]]}
               (pos-int? limit) (assoc :limit limit))))
         []))
     [])))

(defn db-var-history [db-info conversation-id var-sym]
  (if (and (ds db-info) conversation-id)
    (let [state-id-s (latest-state-id db-info conversation-id)]
      (if state-id-s
        (mapv (fn [r]
                (let [value (<-blob (:result r))]
                  {:version    (:version r)
                   :value      value
                   :code       (:expr r)
                   :kind       (var-kind value (:expr r))
                   :restorable? (var-restorable? value (:expr r))
                   :created-at (->date (:created_at r))
                   :info (row->var-info r)}))
          (query! db-info
            {:select [:est.version :est.result :est.expr :est.created_at
                      [:est.conversation_turn_iteration_id :conversation_turn_iteration_id]
                      [:it.position :iteration_position]
                      :es.conversation_state_id
                      :qst.conversation_turn_soul_id]
             :from   [[:expression_state :est]]
             :join   [[:expression_soul :es] [:= :est.expression_soul_id :es.id]
                      [:conversation_turn_iteration :it] [:= :it.id :est.conversation_turn_iteration_id]
                      [:conversation_turn_state :qst] [:= :qst.id :it.conversation_turn_state_id]]
             :where  [:and
                      [:= :es.conversation_state_id state-id-s]
                      [:= :es.kind "var"]
                      [:= :es.name (str var-sym)]]
             :order-by [[:est.version :asc]]}))
        []))
    []))

(defn db-var-history-timeline
  "Newest-first, value-free symbol-memory timeline. Definition/redefinition
   events are persisted. Runtime archive/restore events are intentionally not
   persisted yet; durable tombstones are future work."
  ([db-info conversation-id] (db-var-history-timeline db-info conversation-id {}))
  ([db-info conversation-id {:keys [limit order symbol]
                             :or {limit 100 order :newest-first}}]
   (if (and (ds db-info) conversation-id)
     (let [state-id-s (latest-state-id db-info conversation-id)]
       (if state-id-s
         (let [direction (if (= :oldest-first order) :asc :desc)]
           (mapv (fn [r]
                   (let [value (<-blob (:result r))]
                     {:event       (if (zero? (:version r)) :defined :redefined)
                      :durability  :persisted
                      :symbol      (clojure.core/symbol (:name r))
                      :version     (:version r)
                      :kind        (var-kind value (:expr r))
                      :restorable? (var-restorable? value (:expr r))
                      :at          (->date (:created_at r))
                      :info  (row->var-info r)}))
             (query! db-info
               (cond-> {:select [:es.name :es.conversation_state_id
                                 :est.version :est.result :est.expr :est.created_at
                                 [:est.conversation_turn_iteration_id :conversation_turn_iteration_id]
                                 [:it.position :iteration_position]
                                 :qst.conversation_turn_soul_id]
                        :from   [[:expression_state :est]]
                        :join   [[:expression_soul :es] [:= :est.expression_soul_id :es.id]
                                 [:conversation_turn_iteration :it] [:= :it.id :est.conversation_turn_iteration_id]
                                 [:conversation_turn_state :qst] [:= :qst.id :it.conversation_turn_state_id]]
                        :where  (cond-> [:and
                                         [:= :es.conversation_state_id state-id-s]
                                         [:= :es.kind "var"]]
                                  symbol (conj [:= :es.name (str symbol)]))
                        :order-by [[:est.created_at direction] [:es.name :asc] [:est.version direction]]}
                 (pos-int? limit) (assoc :limit limit)))))
         []))
     [])))

(defn db-turn-history
  "Per-turn history rows for a conversation. `:answer` is canonical
   `[:ir & nodes]` IR (or nil). Channels render via their registered
   `:channel/messages-renderer-fn` - persistence stays flavor-free."
  [db-info conversation-id]
  (let [turns (db-list-conversation-turns db-info conversation-id)]
    (mapv (fn [idx turn]
            (let [turn-ref        (:id turn)
                  iteration-count (count (db-list-conversation-turn-iterations db-info turn-ref))]
              (cond-> {:turn-pos             idx
                       :conversation-turn-id (:id turn)
                       :created-at           (:created-at turn)
                       :user-request         (:user-request turn)
                       :status               (:status turn)
                       :iteration-count      iteration-count}
                (:answer turn) (assoc :answer (:answer turn)))))
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
             :metadata                    (->json (:metadata opts))
             :content                     (->blob (:content opts))
             :conversation_soul_id        (some-> (:conversation-soul-id opts) ->ref)
             :conversation_state_id       (some-> (:conversation-state-id opts) ->ref)
             :conversation_turn_state_id  (some-> (:conversation-turn-state-id opts) ->ref)
             :conversation_turn_iteration_id                (some-> (:iteration-id opts) ->ref)
             :conversation_turn_iteration_block_index       (:iteration-block-index opts)
             :conversation_turn_iteration_block_id          (some-> (:iteration-block-id opts) str)
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
    (when (and (or (:conversation_turn_iteration_block_index row) (:conversation_turn_iteration_block_id row))
            (nil? (:conversation_turn_iteration_id row)))
      (throw (ex-info "extension aggregate block scope requires iteration-id"
               {:type :extension-aggregate/block-without-iteration})))
    row))

(defn- row->extension-aggregate
  [row]
  (when row
    (let [scope (cond-> {}
                  (:conversation_soul_id row)       (assoc :conversation-soul-id (:conversation_soul_id row))
                  (:conversation_state_id row)      (assoc :conversation-state-id (:conversation_state_id row))
                  (:conversation_turn_state_id row) (assoc :conversation-turn-state-id (:conversation_turn_state_id row))
                  (:conversation_turn_iteration_id row)               (assoc :iteration-id (:conversation_turn_iteration_id row))
                  (:conversation_turn_iteration_block_index row)      (assoc :iteration-block-index (:conversation_turn_iteration_block_index row))
                  (:conversation_turn_iteration_block_id row)         (assoc :iteration-block-id (:conversation_turn_iteration_block_id row)))
          aggregate-key (<-edn-text (:aggregate_key row))]
      {:id            (:id row)
       :extension-id  (:extension_id row)
       :aggregate-key aggregate-key
       :key           aggregate-key
       :kind          (<-edn-text (:kind row))
       :scope         scope
       :metadata      (<-json (:metadata row))
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
    (:conversation-soul-id opts)       (conj [:= :conversation_soul_id (->ref (:conversation-soul-id opts))])
    (:conversation-state-id opts)      (conj [:= :conversation_state_id (->ref (:conversation-state-id opts))])
    (:conversation-turn-state-id opts) (conj [:= :conversation_turn_state_id (->ref (:conversation-turn-state-id opts))])
    (:iteration-id opts)               (conj [:= :conversation_turn_iteration_id (->ref (:iteration-id opts))])
    (contains? opts :iteration-block-index)
    (conj [:= :conversation_turn_iteration_block_index (:iteration-block-index opts)])
    (:iteration-block-id opts)         (conj [:= :conversation_turn_iteration_block_id (str (:iteration-block-id opts))])))

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
             :do-update-set {:metadata                    (:metadata row)
                             :content                     (:content row)
                             :conversation_soul_id        (:conversation_soul_id row)
                             :conversation_state_id       (:conversation_state_id row)
                             :conversation_turn_state_id  (:conversation_turn_state_id row)
                             :conversation_turn_iteration_id                (:conversation_turn_iteration_id row)
                             :conversation_turn_iteration_block_index       (:conversation_turn_iteration_block_index row)
                             :conversation_turn_iteration_block_id          (:conversation_turn_iteration_block_id row)
                             :updated_at                  now}})
          (row->extension-aggregate
            (query-one! tx-info
              {:select [:*]
               :from   :extension_aggregate
               :where  [:and
                        [:= :extension_id (:extension_id row)]
                        [:= :aggregate_key (:aggregate_key row)]
                        [:= :kind (:kind row)]
                        (if (:conversation_soul_id row)
                          [:= :conversation_soul_id (:conversation_soul_id row)]
                          [:is :conversation_soul_id nil])
                        (if (:conversation_state_id row)
                          [:= :conversation_state_id (:conversation_state_id row)]
                          [:is :conversation_state_id nil])
                        (if (:conversation_turn_state_id row)
                          [:= :conversation_turn_state_id (:conversation_turn_state_id row)]
                          [:is :conversation_turn_state_id nil])
                        (if (:conversation_turn_iteration_id row)
                          [:= :conversation_turn_iteration_id (:conversation_turn_iteration_id row)]
                          [:is :conversation_turn_iteration_id nil])
                        (if (:conversation_turn_iteration_block_index row)
                          [:= :conversation_turn_iteration_block_index (:conversation_turn_iteration_block_index row)]
                          [:is :conversation_turn_iteration_block_index nil])
                        (if (:conversation_turn_iteration_block_id row)
                          [:= :conversation_turn_iteration_block_id (:conversation_turn_iteration_block_id row)]
                          [:is :conversation_turn_iteration_block_id nil])]})))))))

(defn db-get-extension-aggregate
  [db-info opts]
  (when (ds db-info)
    (row->extension-aggregate
      (query-one! db-info (assoc (extension-aggregate-select opts) :limit 1)))))

(defn db-search
  "Full-text search over the FTS5 `search` index. Returns a vector of
   hits sorted by FTS5 rank (highest relevance first), each entry
   shaped:

     {:owner-table  String   ; e.g. \"conversation_turn_state\" / \"conversation_turn_iteration\"
      :owner-id     String   ; UUID into that table
      :field        String   ; \"answer_text\" | \"thinking_text\" | \"comments_text\" | \"user_request\" | \"expr\"
      :snippet      String   ; FTS5 snippet with `[match]` markers around hit terms
      :rank         double}  ; FTS5 rank (lower = better match)

   `query` is a literal FT5 MATCH expression (`\"foo bar\"` for AND,
   `\"foo OR bar\"`, prefix `foo*`, etc.); see SQLite FTS5 docs.

   `opts` may include:
     :limit        max hits returned (default 50)
     :owner-table  filter to one table (string)
     :field        filter to one field (string)"
  ([db-info query] (db-search db-info query nil))
  ([db-info query {:keys [limit owner-table field]}]
   (when (and (ds db-info) (string? query) (not (str/blank? query)))
     ;; SQLite FTS5 spells the match operator as `<table> MATCH ?`,
     ;; which HoneySQL's vocabulary doesn't model directly. Raw SQL
     ;; with positional params is the simplest faithful expression.
     (let [base "SELECT owner_table, owner_id, field, snippet(search, 3, '[', ']', '…', 32) AS snippet, rank FROM search WHERE search MATCH ?"
           [filt-sql filt-params] (cond-> ["" []]
                                    owner-table (-> (update 0 str " AND owner_table = ?")
                                                  (update 1 conj owner-table))
                                    field       (-> (update 0 str " AND field = ?")
                                                  (update 1 conj field)))
           sql-vec (into [(str base filt-sql " ORDER BY rank ASC LIMIT ?") query]
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
              :metadata (or (:metadata opts) (:metadata current))
              :content next-content)))))))

;; =============================================================================
;; Expression dependencies
;; =============================================================================

(defn db-store-dependency!
  "Store an edge: downstream depends on upstream.
   Both must be expression_souls in the same conversation_state."
  [db-info {:keys [conversation-state-id downstream-soul-id upstream-soul-id]}]
  (when (ds db-info)
    (sqlite-write-tx! db-info
      (fn [tx-info]
        (let [id (new-id)]
          (execute! tx-info
            {:insert-into :expression_dependency
             :values [{:id                            id
                       :conversation_state_id         (->id conversation-state-id)
                       :downstream_expression_soul_id (->id downstream-soul-id)
                       :upstream_expression_soul_id   (->id upstream-soul-id)
                       :created_at                    (now-ms)}]})
          id)))))

(defn db-list-dependencies
  "List all dependency edges for a conversation_state."
  [db-info conversation-state-id]
  (when (ds db-info)
    (mapv (fn [r]
            {:id         (:id r)
             :downstream (:downstream_expression_soul_id r)
             :upstream   (:upstream_expression_soul_id r)})
      (query! db-info
        {:select [:id :downstream_expression_soul_id :upstream_expression_soul_id]
         :from   :expression_dependency
         :where  [:= :conversation_state_id (->id conversation-state-id)]}))))

;; =============================================================================
;; Restore - read all vars in topological order for sandbox reconstruction
;; =============================================================================

(defn db-restore-blocks
  "Returns all stateful var expression_souls with their latest expression_state,
   ordered topologically (dependencies first, then by created_at).

   Each entry:
     {:soul-id     uuid-str
      :name        string
      :kind        'var'
      :state-mode  'stateful'
      :version     int
      :expr        string (source code)
      :result      <thawed value or {:vis/ref :expr}>
      :depends-on  [upstream-soul-id ...]
      :depended-by [downstream-soul-id ...]}

   The caller evals entries in order: for each entry, if result is
   {:vis/ref :expr}, eval the :expr to reconstruct the value.
   Dependencies are guaranteed to appear before dependents."
  [db-info conversation-id]
  (when (ds db-info)
    (let [state-id-s (latest-state-id db-info conversation-id)]
      (when state-id-s
        ;; 1. All var souls with latest state
        (let [rows (query! db-info
                     {:select [:es.id :es.name :es.kind :es.state_mode
                               :est.version :est.expr :est.result :est.success
                               :est.created_at]
                      :from   [[:expression_soul :es]]
                      :join   [[:expression_state :est] [:= :est.expression_soul_id :es.id]]
                      :where  [:and
                               [:= :es.conversation_state_id state-id-s]
                               [:= :es.kind "var"]
                               [:= :es.state_mode "stateful"]
                               [:= :est.version
                                {:select [[[:max :version]]]
                                 :from   [[:expression_state :est2]]
                                 :where  [:= :est2.expression_soul_id :es.id]}]]})
              by-id (into {} (map (fn [r] [(:id r) r])) rows)
              ;; 2. Dependencies
              deps  (db-list-dependencies db-info state-id-s)
              ;; Build adjacency: soul-id -> {:depends-on #{} :depended-by #{}}
              adj   (reduce (fn [m {:keys [downstream upstream]}]
                              (-> m
                                (update-in [downstream :depends-on] (fnil conj #{}) upstream)
                                (update-in [upstream :depended-by] (fnil conj #{}) downstream)))
                      {} deps)
              ;; 3. Topological sort (Kahn's algorithm)
              soul-ids  (set (keys by-id))
              in-degree (reduce (fn [m id]
                                  (assoc m id (count (filter soul-ids
                                                       (get-in adj [id :depends-on])))))
                          {} soul-ids)
              sorted    (loop [queue  (into clojure.lang.PersistentQueue/EMPTY
                                        (sort-by (fn [id] (:created_at (get by-id id)))
                                          (filter #(zero? (get in-degree % 0)) soul-ids)))
                               result []
                               deg    in-degree]
                          (if (empty? queue)
                            result
                            (let [id   (peek queue)
                                  queue (pop queue)
                                  result (conj result id)
                                  downstream (filter soul-ids
                                               (get-in adj [id :depended-by]))
                                  deg (reduce (fn [d did]
                                                (update d did dec))
                                        deg downstream)
                                  newly-ready (filter #(zero? (get deg %)) downstream)
                                  queue (reduce conj queue
                                          (sort-by (fn [id] (:created_at (get by-id id)))
                                            newly-ready))]
                              (recur queue result deg))))]
          ;; 4. Build result
          (mapv (fn [soul-id]
                  (let [r (get by-id soul-id)]
                    {:soul-id    soul-id
                     :name       (:name r)
                     :kind       (:kind r)
                     :state-mode (:state_mode r)
                     :version    (:version r)
                     :expr       (:expr r)
                     :result     (<-blob (:result r))
                     :success    (= 1 (:success r))
                     :depends-on (vec (filter soul-ids (get-in adj [soul-id :depends-on])))
                     :depended-by (vec (filter soul-ids (get-in adj [soul-id :depended-by])))}))
            sorted))))))

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
