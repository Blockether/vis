(ns ^{:clj-kondo/config '{:linters {:unused-public-var {:level :off}}}}
  com.blockether.vis.ext.persistance-sqlite.core
  "SQLite store — V1 schema implementation.

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
     conversation_intent, conversation_intent_ref,
     conversation_intent_relation, conversation_intent_plan,
     conversation_intent_gate, conversation_intent_gate_ref,
     conversation_intent_focus, log

   Connection lifecycle:
     (db-open! db-spec)   → {:datasource ds :path ...}
     (db-close! store)    → idempotent dispose"
  (:require
   [charred.api :as json]
   [clojure.edn :as edn]
   [clojure.string :as str]
   [com.blockether.vis.ext.persistance-sqlite.migration :as migration]
   [com.blockether.vis.core :as vis]
   [com.blockether.vis.internal.proof :as proof]
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
          "The file is missing — likely deleted while Vis was running. Restart Vis to recreate it."

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
;; `getConnection()` — it does NOT pool. We replace it with HikariCP:
;; an underlying `SQLiteDataSource` configured via `SQLiteConfig`
;; (WAL, NORMAL sync, FK enforcement, 30s busy timeout) is wrapped
;; in a `HikariDataSource` with a small fixed pool. SQLite WAL allows
;; N readers + 1 writer, so 5 connections give read concurrency
;; without amplifying writer contention. The pool keeps physical
;; connections alive for the process lifetime (no idle eviction,
;; no maxLifetime recycling — unlike a network DB, SQLite handles
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
     maximumPoolSize = 5  — 1 writer + up to 4 concurrent readers,
                            mirrors the SQLite WAL concurrency model.
     minimumIdle     = 1  — keep one connection warm; cold-start cost
                            on SQLite is small, but eliminating it
                            removes one source of `[SQLITE_CANTOPEN]`
                            jitter on the very first request.
     idleTimeout     = 0  — SQLite handles are cheap to keep; the
                            pool keeps them alive and the WAL state
                            stays warm.
     maxLifetime     = 0  — no network drop concern; recycling adds
                            zero value and creates spurious opens.
     leakDetectionThreshold = 60s — surface checked-out-but-never-
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
      ;; non-pooled DataSource didn't *need* closing — it had no
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
   `:external` mode (caller-supplied DataSource) it's a no-op — the
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
;; Logging — log table
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
                      (:iteration-id entry)          (assoc :iteration_id (->id (:iteration-id entry)))
                      (:expression-soul-id entry)    (assoc :expression_soul_id (->id (:expression-soul-id entry)))
                      (:expression-state-id entry)   (assoc :expression_state_id (->id (:expression-state-id entry))))]})))))

;; =============================================================================
;; Conversation — conversation_soul + conversation_state
;; =============================================================================

(defn db-store-conversation!
  "Create conversation_soul + initial conversation_state (version 0).
   Returns the conversation-soul UUID.

   Metadata layout:
     conversation_soul.metadata  → {:channel :tui, :external-id \"...\"}
     conversation_state.metadata → {:system-prompt \"...\", :provider :openai,
                                    :model \"gpt-4o\"}
     conversation_state.title    → title column

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
      ;; conversation_soul.metadata contains channel — filter through SQLite JSON1,
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
;; Fork — branch a conversation at a point
;; =============================================================================

(defn db-list-conversation-states
  "List every `conversation_state` row for the soul behind `conversation-id`,
   oldest version first. Each row maps to
   `{:state-id :version :parent-state-id :title :system-prompt :provider :model
     :created-at :turn-count}` — the raw fork tree of one conversation soul.

   The trunk is `:version 0` with `:parent-state-id nil`. A fork is any row
   whose `:parent-state-id` points at another `:state-id` in the same vector;
   group-by `:parent-state-id` to walk the tree.

   `:turn-count` is the number of `conversation_turn_soul` rows hanging off that specific
   state — cheap to compute, useful when triaging which branch is active.

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
   rows attached to that specific state — retries get their own iteration
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
                             :from   :iteration
                             :where  [:= :iteration.conversation_turn_state_id :qst.id]}
                            :iteration_count]]
                  :from   [[:conversation_turn_state :qst]]
                  :where  [:= :qst.conversation_turn_soul_id soul-id-s]
                  :order-by [[:qst.version :asc]]})]
      (mapv (fn [row]
              (cond-> {:state-id                    (->uuid (:id row))
                       :version                     (:version row)
                       :forked-from-conversation-turn-state-id  (some-> (:forked_from_conversation_turn_state_id row) ->uuid)
                       :status                      (->kw-back (:status row))
                       :created-at                  (->date (:created_at row))
                       :iteration-count             (or (:iteration_count row) 0)}
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
;; Turn — conversation_turn_soul + conversation_turn_state
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

   When `:prior-outcome` is provided (one of `:complete`,
   `:abandoned`, `:cancelled`, `:error`), it lands in the dedicated
   `prior_outcome` column so the next turn's handover digest can read
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
                                              (cond-> {:answer          (or answer "")
                                                       :iteration-count (or iteration-count 0)
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
                         prior-outcome    (assoc :prior_outcome (name prior-outcome)))
               :where  [:= :id (:id state)]})))))))

 ;; =============================================================================
;; Conversation-scoped intent -> plan -> blocking gate
;; =============================================================================

(defn- require-latest-turn-state
  [db-info conversation-turn-id]
  (let [soul-id-s (->ref conversation-turn-id)
        state     (when soul-id-s (latest-conversation-turn-state db-info soul-id-s))]
    (when-not state
      (throw (ex-info "conversation turn state not found" {:conversation-turn-id conversation-turn-id})))
    state))

(defn- require-row
  [db-info table id message]
  (let [row (query-one! db-info {:select [:*] :from table :where [:= :id (->ref id)]})]
    (when-not row
      (throw (ex-info message {:table table :id id})))
    row))

(defn- turn-state-context
  [db-info conversation-turn-id]
  (let [turn-state (require-latest-turn-state db-info conversation-turn-id)
        row        (query-one! db-info
                     {:select [[:cts.id :conversation_turn_state_id]
                               [:cts.conversation_turn_soul_id :conversation_turn_soul_id]
                               [:cs.conversation_soul_id :conversation_soul_id]
                               [:cs.id :conversation_state_id]]
                      :from   [[:conversation_turn_state :cts]]
                      :join   [[:conversation_turn_soul :ct] [:= :ct.id :cts.conversation_turn_soul_id]
                               [:conversation_state :cs] [:= :cs.id :ct.conversation_state_id]]
                      :where  [:= :cts.id (:id turn-state)]})]
    (merge turn-state row)))

(defn- conversation-soul-id-for-conversation
  [db-info conversation-id]
  (:id (require-row db-info :conversation_soul conversation-id "conversation_soul not found")))

(defn- intent-conversation-soul-id
  [db-info intent-id]
  (:conversation_soul_id (require-row db-info :conversation_intent intent-id "conversation_intent not found")))

(defn- conversation-soul-id-for-opts
  [db-info {:keys [conversation-id conversation-turn-id intent-id plan-id gate-id]}]
  (cond
    conversation-id (conversation-soul-id-for-conversation db-info conversation-id)
    conversation-turn-id (:conversation_soul_id (turn-state-context db-info conversation-turn-id))
    intent-id (intent-conversation-soul-id db-info intent-id)
    plan-id (let [plan (require-row db-info :conversation_intent_plan plan-id "conversation_intent_plan not found")]
              (intent-conversation-soul-id db-info (:intent_id plan)))
    gate-id (let [gate (require-row db-info :conversation_intent_gate gate-id "conversation_intent_gate not found")
                  plan (require-row db-info :conversation_intent_plan (:plan_id gate) "conversation_intent_plan not found")]
              (intent-conversation-soul-id db-info (:intent_id plan)))
    :else (throw (ex-info "conversation scope required" {:opts (keys (or nil {}))}))))

(defn- id-handle [prefix id]
  (str prefix (subs (str id) 0 8)))

(defn- intent-refs
  [db-info intent-id]
  (->> (query! db-info
         {:select [:ref :role :metadata :created_at]
          :from   :conversation_intent_ref
          :where  [:= :intent_id (->ref intent-id)]
          :order-by [[:created_at :asc]]})
    (mapv (fn [row]
            (cond-> {:ref (:ref row)
                     :role (->kw-back (:role row))
                     :created-at (->date (:created_at row))}
              (:metadata row) (assoc :metadata (<-json (:metadata row))))))))

(defn- gate-refs
  [db-info gate-id]
  (->> (query! db-info
         {:select [:ref :role :intent_id :slot :metadata :created_at]
          :from   :conversation_intent_gate_ref
          :where  [:= :gate_id (->ref gate-id)]
          :order-by [[:created_at :asc]]})
    (mapv (fn [row]
            (cond-> {:ref (:ref row)
                     :role (->kw-back (:role row))
                     :created-at (->date (:created_at row))}
              (:intent_id row) (assoc :slot [(->uuid (:intent_id row)) (keyword (:slot row))])
              (:metadata row) (assoc :metadata (<-blob (:metadata row))))))))

(defn- row->intent
  ([row] (row->intent row [] [] []))
  ([row refs plans relations]
   (cond-> {:id                   (->uuid (:id row))
            :handle               (id-handle "I" (:id row))
            :conversation-soul-id (->uuid (:conversation_soul_id row))
            :title                (:title row)
            :rationale            (:rationale row)
            :status               (->kw-back (:status row))
            :source               (or (->kw-back (:source row)) :user)
            :refs                 refs
            :plans                plans
            :relations            relations
            :created-at           (->date (:created_at row))}
     (:owner_extension_id row)  (assoc :owner-extension-id (:owner_extension_id row))
     (:parent_intent_id row)    (assoc :parent-intent-id (->uuid (:parent_intent_id row)))
     (:fulfillment_summary row) (assoc :fulfillment-summary (:fulfillment_summary row))
     (:abandonment_reason row)  (assoc :abandonment-reason (:abandonment_reason row))
     (:abandonment_scope row)   (assoc :abandonment-scope
                                  (keyword "abandon" (:abandonment_scope row)))
     (:accepted_by_kind row)    (assoc :accepted-by-kind (->kw-back (:accepted_by_kind row)))
     (:accepted_by_id row)      (assoc :accepted-by-id (:accepted_by_id row))
     (:accepted_at row)         (assoc :accepted-at (->date (:accepted_at row)))
     (:defer_trigger_kind row)  (assoc :defer-trigger-kind
                                  (keyword "defer" (:defer_trigger_kind row)))
     (:defer_trigger_payload row) (assoc :defer-trigger-payload (<-blob (:defer_trigger_payload row)))
     (:defer_sibling_policy row) (assoc :defer-sibling-policy
                                   (keyword "defer" (:defer_sibling_policy row)))
     (:resumable_at row)        (assoc :resumable-at (->date (:resumable_at row)))
     (:resumed_by_kind row)     (assoc :resumed-by-kind (->kw-back (:resumed_by_kind row)))
     (:resumed_by_id row)       (assoc :resumed-by-id (:resumed_by_id row))
     (:resumed_at row)          (assoc :resumed-at (->date (:resumed_at row)))
     (:created_conversation_turn_id row) (assoc :created-conversation-turn-id (->uuid (:created_conversation_turn_id row)))
     (:resolved_conversation_turn_id row) (assoc :resolved-conversation-turn-id (->uuid (:resolved_conversation_turn_id row)))
     (:created_ref row)         (assoc :created-ref (:created_ref row))
     (:resolved_ref row)        (assoc :resolved-ref (:resolved_ref row))
     (:metadata row)            (assoc :metadata (<-json (:metadata row)))
     (:resolved_at row)         (assoc :resolved-at (->date (:resolved_at row))))))

(defn- row->plan
  ([row] (row->plan row []))
  ([row gates]
   (cond-> {:id         (->uuid (:id row))
            :handle     (id-handle "P" (:id row))
            :intent-id  (->uuid (:intent_id row))
            :status     (->kw-back (:status row))
            :summary    (:summary row)
            :gates      gates
            :created-at (->date (:created_at row))}
     (:supersedes_plan_id row) (assoc :supersedes-plan-id (->uuid (:supersedes_plan_id row)))
     (:plan_dsl row)           (assoc :plan (<-blob (:plan_dsl row)))
     (:steps row)              (assoc :steps (<-blob (:steps row)))
     (:created_conversation_turn_id row) (assoc :created-conversation-turn-id (->uuid (:created_conversation_turn_id row)))
     (:created_ref row)        (assoc :created-ref (:created_ref row))
     (:metadata row)           (assoc :metadata (<-blob (:metadata row))))))

(defn- row->gate
  ([row] (row->gate row []))
  ([row refs]
   (cond-> {:id             (->uuid (:id row))
            :handle         (id-handle "G" (:id row))
            :plan-id        (->uuid (:plan_id row))
            :status         (->kw-back (:status row))
            :required?      (= 1 (long (:required row)))
            :proposition    (:proposition row)
            :expected-proof (<-blob (:expected_proof row))
            :refs           refs
            :created-at     (->date (:created_at row))}
     (:candidate_proof row) (assoc :candidate-proof (<-blob (:candidate_proof row)))
     (:proof row)           (assoc :proof (<-blob (:proof row)))
     (:impediment row)      (assoc :impediment (<-blob (:impediment row)))
     (:metadata row)        (assoc :metadata (<-blob (:metadata row)))
     (:created_ref row)     (assoc :created-ref (:created_ref row))
     (:resolved_ref row)    (assoc :resolved-ref (:resolved_ref row))
     (:resolved_at row)     (assoc :resolved-at (->date (:resolved_at row))))))

(defn- kw-sql
  [x]
  (cond
    (keyword? x) (if (namespace x) (str (namespace x) "/" (name x)) (name x))
    (symbol? x) (if (namespace x) (str (namespace x) "/" (name x)) (name x))
    (some? x) (str x)
    :else nil))

(defn- input-keyword
  [x]
  (cond
    (keyword? x) x
    (symbol? x) (keyword (namespace x) (name x))
    (string? x) (keyword x)
    :else x))

(defn- proof-kind-sql
  [kind]
  (let [kind (input-keyword kind)]
    (when-not (contains? proof/event-kinds kind)
      (throw (ex-info "provenance event kind is not supported" {:kind kind})))
    (name kind)))

(defn- proof-status-sql
  [status]
  (let [status (input-keyword status)]
    (when-not (contains? proof/lifecycle-statuses status)
      (throw (ex-info "provenance event status is not supported" {:status status})))
    (name status)))

(defn- payload-digest
  [payload]
  (sha256-hex (pr-str payload)))

(defn- emit-proof-event!
  [event payload]
  (try
    (vis/emit-proof-event! (assoc payload :proof/event event))
    (catch Throwable _ nil)))

(defn- row->provenance-event
  [row]
  (cond-> {:id (->uuid (:id row))
           :conversation-id (->uuid (:conversation_soul_id row))
           :ref (:ref row)
           :kind (->kw-back (:kind row))
           :op (:op row)
           :status (->kw-back (:status row))
           :created-at (->date (:created_at row))}
    (:conversation_turn_soul_id row) (assoc :conversation-turn-id (->uuid (:conversation_turn_soul_id row)))
    (:conversation_turn_state_id row) (assoc :conversation-turn-state-id (->uuid (:conversation_turn_state_id row)))
    (:iteration_id row) (assoc :iteration-id (->uuid (:iteration_id row)))
    (:parent_ref row) (assoc :parent-ref (:parent_ref row))
    (:rendering_kind row) (assoc :rendering-kind (->kw-back (:rendering_kind row)))
    (:payload row) (assoc :payload (<-blob (:payload row)))
    (:payload_sha256 row) (assoc :payload-sha256 (:payload_sha256 row))
    (:summary row) (assoc :summary (:summary row))
    (:metadata row) (assoc :metadata (<-blob (:metadata row)))))

(defn db-store-provenance-event!
  "Append one immutable provenance_event row.

   This is the first proof-grade ledger slice. It records runtime observations;
   evidence bundles and attestations consume these rows later."
  [db-info {:keys [conversation-id conversation-turn-id conversation-turn-state-id iteration-id
                   ref parent-ref kind op status rendering-kind payload payload-sha256 summary metadata]}]
  (when (ds db-info)
    (let [ref (str ref)]
      (when-not (proof/canonical-ref? ref)
        (throw (ex-info "provenance ref must be canonical"
                 {:ref ref
                  :accepted "turn/<turn8>/iteration/<n>/block/<k>[/tool/<tool-id>|/error]"})))
      (when (and parent-ref (not (proof/canonical-ref? parent-ref)))
        (throw (ex-info "provenance parent ref must be canonical" {:parent-ref parent-ref})))
      (sqlite-write-tx! db-info
        (fn [tx-info]
          (let [id (new-id)
                row {:id id
                     :conversation_soul_id (->ref conversation-id)
                     :conversation_turn_soul_id (some-> conversation-turn-id ->ref)
                     :conversation_turn_state_id (some-> conversation-turn-state-id ->ref)
                     :iteration_id (some-> iteration-id ->ref)
                     :ref ref
                     :parent_ref parent-ref
                     :kind (proof-kind-sql kind)
                     :op (or (kw-sql op) "event")
                     :status (proof-status-sql status)
                     :rendering_kind (kw-sql rendering-kind)
                     :payload (when (some? payload) (->blob payload))
                     :payload_sha256 (or payload-sha256 (when (some? payload) (payload-digest payload)))
                     :summary summary
                     :metadata (when (some? metadata) (->blob metadata))
                     :created_at (now-ms)}]
            (execute! tx-info {:insert-into :provenance_event
                               :values [(into {} (remove (comp nil? val) row))]})
            (let [event-row (row->provenance-event
                              (require-row tx-info :provenance_event id "provenance_event not found after insert"))]
              (emit-proof-event! :proof/event-appended {:event event-row})
              event-row)))))))

(defn db-get-provenance-event
  [db-info conversation-id ref]
  (when (ds db-info)
    (some-> (query-one! db-info
              {:select [:*]
               :from :provenance_event
               :where [:and
                       [:= :conversation_soul_id (->ref conversation-id)]
                       [:= :ref (str ref)]]})
      row->provenance-event)))

(defn db-list-provenance-events
  [db-info {:keys [conversation-id iteration-id status kind limit] :or {limit 100}}]
  (if-not (ds db-info)
    []
    (mapv row->provenance-event
      (query! db-info
        (cond-> {:select [:*]
                 :from :provenance_event
                 :order-by [[:created_at :asc] [:id :asc]]
                 :limit limit}
          conversation-id (assoc :where [:= :conversation_soul_id (->ref conversation-id)])
          iteration-id (update :where (fn [where]
                                        (if where
                                          [:and where [:= :iteration_id (->ref iteration-id)]]
                                          [:= :iteration_id (->ref iteration-id)])))
          status (update :where (fn [where]
                                  (if where
                                    [:and where [:= :status (proof-status-sql status)]]
                                    [:= :status (proof-status-sql status)])))
          kind (update :where (fn [where]
                                (if where
                                  [:and where [:= :kind (proof-kind-sql kind)]]
                                  [:= :kind (proof-kind-sql kind)]))))))))

(defn- bundle-kind-sql
  [kind]
  (let [kind (input-keyword kind)]
    (when-not (contains? proof/bundle-kinds kind)
      (throw (ex-info "evidence bundle kind is not supported" {:kind kind})))
    (name kind)))

(defn- subject-kind-sql
  [kind]
  (let [kind (input-keyword kind)]
    (when-not (contains? proof/subject-kinds kind)
      (throw (ex-info "evidence bundle subject kind is not supported" {:subject-kind kind})))
    (name kind)))

(defn- bundle-source-sql
  [source]
  (let [source (input-keyword (or source :derived))]
    (when-not (contains? proof/bundle-sources source)
      (throw (ex-info "evidence bundle source is not supported" {:source source})))
    (name source)))

(defn- member-role-sql
  [role]
  (let [role (input-keyword (or role :observation))]
    (when-not (contains? proof/member-roles role)
      (throw (ex-info "evidence bundle member role is not supported" {:role role})))
    (name role)))

(defn- row->evidence-member
  [row]
  (cond-> {:id (->uuid (:id row))
           :bundle-id (->uuid (:bundle_id row))
           :slot (<-blob (:slot row))
           :from-ref (:event_ref row)
           :extract (<-blob (:extract_path row))
           :guard-ok (= 1 (long (:guard_ok row)))
           :member-role (->kw-back (:member_role row))}
    (:derived_value row) (assoc :value (<-blob (:derived_value row)))
    (:guard row) (assoc :guard (<-blob (:guard row)))
    (:error_code row) (assoc :error-code (->kw-back (:error_code row)))
    (:error row) (assoc :error (:error row))))

(defn- row->evidence-bundle
  [db-info row]
  (let [members (mapv row->evidence-member
                  (query! db-info {:select [:*]
                                   :from :evidence_bundle_member
                                   :where [:= :bundle_id (:id row)]
                                   ;; Position is the persisted insertion order;
                                   ;; without it tests that read `(last members)`
                                   ;; flake on UUID alpha collation in one tx.
                                   :order-by [[:position :asc] [:created_at :asc] [:id :asc]]}))]
    (cond-> {:id (->uuid (:id row))
             :conversation-id (->uuid (:conversation_soul_id row))
             :kind (->kw-back (:kind row))
             :subject-kind (->kw-back (:subject_kind row))
             :subject-id (->uuid (:subject_id row))
             :source (->kw-back (:source row))
             :status (->kw-back (:status row))
             :accepted? (= "accepted" (:status row))
             :members members
             :created-at (->date (:created_at row))}
      (:summary row) (assoc :summary (:summary row))
      (:metadata row) (assoc :metadata (<-blob (:metadata row))))))

(defn- event-row->proof-event
  [row]
  {:event/ref (:ref row)
   :event/status (->kw-back (:status row))
   :event/kind (->kw-back (:kind row))
   :event/op (:op row)
   :event/rendering-kind (some-> (:rendering_kind row) ->kw-back)
   :event/payload (<-blob (:payload row))})

(defn- bundle-event-lookup
  "Resolve a canonical ref to a `provenance_event` row scoped to the bundle's
   conversation. Returns one of:
     {:status :found  :event <row>}
     {:status :non-canonical}           ; ref is not canonical-shaped at all
     {:status :missing}                 ; canonical ref but no event anywhere
     {:status :cross-conversation}      ; canonical ref, event in another conversation
   The caller maps each status onto a precise binding-error code so audit can
   distinguish \"compact display alias\" from \"never observed\" from
   \"observed under another conversation\" instead of collapsing them."
  [tx-info conversation-id ref]
  (cond
    (not (proof/canonical-ref? ref))
    {:status :non-canonical}

    :else
    (if-let [scoped (query-one! tx-info {:select [:*]
                                         :from :provenance_event
                                         :where [:and
                                                 [:= :conversation_soul_id (->ref conversation-id)]
                                                 [:= :ref ref]]})]
      {:status :found :event scoped}
      (if (query-one! tx-info {:select [:id]
                               :from :provenance_event
                               :where [:= :ref ref]})
        {:status :cross-conversation}
        {:status :missing}))))

(defn- bundle-precise-binding-error
  [requirement lookup-status]
  (case lookup-status
    :missing
    {:evidence/slot (:evidence/slot requirement)
     :evidence/from-ref (:evidence/from-ref requirement)
     :evidence/extract (:evidence/extract requirement)
     :evidence/error "No runtime event exists for evidence requirement ref"
     :evidence/error-code :missing-event
     :evidence/guard-ok false}
    :cross-conversation
    {:evidence/slot (:evidence/slot requirement)
     :evidence/from-ref (:evidence/from-ref requirement)
     :evidence/extract (:evidence/extract requirement)
     :evidence/error "Evidence ref points at an event in another conversation"
     :evidence/error-code :cross-conversation-ref
     :evidence/guard-ok false}))

(defn db-create-evidence-bundle!
  "Derive and persist an evidence bundle from provenance_event rows. Caller-supplied slot values are ignored.

   Bundles with no requirements, no resolvable events, or events from
   another conversation are stored as `:rejected` so audit can see the
   attempt, but they cannot back any attestation."
  [db-info {:keys [conversation-id kind subject-kind subject-id source summary metadata requirements]}]
  (when (ds db-info)
    (sqlite-write-tx! db-info
      (fn [tx-info]
        (let [lookups       (mapv (fn [r] (bundle-event-lookup tx-info conversation-id (:evidence/from-ref r)))
                              requirements)
              ;; Build a binding for each requirement:
              ;;   :non-canonical    — hand to `derive-binding` so it emits
              ;;                       the precise `:non-canonical-ref` code
              ;;                       and other shape diagnostics.
              ;;   :found            — derive against the runtime row.
              ;;   :missing | :cross-conversation — emit precise persistence
              ;;                                    binding-error codes.
              precise-bindings
              (mapv (fn [requirement {:keys [status event]}]
                      (case status
                        :non-canonical    (proof/derive-binding requirement
                                            {:event/ref (:evidence/from-ref requirement)})
                        :found            (proof/derive-binding requirement (event-row->proof-event event))
                        (:missing :cross-conversation)
                        (bundle-precise-binding-error requirement status)))
                requirements lookups)
              ;; If `requirements` is empty, evaluate-gate returns the
              ;; `:empty-requirements` rejection on its own.
              gate-result   (if (empty? requirements)
                              (proof/evaluate-gate [] [])
                              {:gate/proven? (every? #(true? (:evidence/guard-ok %)) precise-bindings)
                               :attestation/status (if (every? #(true? (:evidence/guard-ok %)) precise-bindings)
                                                     :accepted :rejected)
                               :attestation/decision (if (every? #(true? (:evidence/guard-ok %)) precise-bindings)
                                                       :proven :impeded)
                               :bundle/bindings precise-bindings
                               :gate/errors (filterv :evidence/error precise-bindings)})
              accepted? (:gate/proven? gate-result)
              bundle-id (new-id)
              now (now-ms)]
          (execute! tx-info {:insert-into :evidence_bundle
                             :values [{:id bundle-id
                                       :conversation_soul_id (->ref conversation-id)
                                       :kind (bundle-kind-sql (or kind :proof))
                                       :subject_kind (subject-kind-sql subject-kind)
                                       :subject_id (->ref subject-id)
                                       :source (bundle-source-sql source)
                                       :status (if accepted? "accepted" "rejected")
                                       :summary summary
                                       :metadata (when (some? metadata) (->blob metadata))
                                       :created_at now}]})
          (doseq [[idx binding] (map-indexed vector (:bundle/bindings gate-result))]
            ;; Skip pseudo-bindings (e.g. the `:empty-requirements` marker)
            ;; that have no slot/from-ref/extract — they describe a top-level
            ;; proof-shape failure, not a real evidence requirement.
            (when (and (:evidence/slot binding) (:evidence/from-ref binding))
              (execute! tx-info {:insert-into :evidence_bundle_member
                                 :values [(into {}
                                            (remove (comp nil? val)
                                              {:id (new-id)
                                               :bundle_id bundle-id
                                               :position idx
                                               :slot (->blob (:evidence/slot binding))
                                               :event_ref (:evidence/from-ref binding)
                                               :extract_path (->blob (:evidence/extract binding))
                                               :derived_value (when (contains? binding :evidence/value)
                                                                (->blob (:evidence/value binding)))
                                               :guard (when (contains? binding :evidence/guard)
                                                        (->blob (:evidence/guard binding)))
                                               :guard_ok (if (:evidence/guard-ok binding) 1 0)
                                               :error_code (some-> (:evidence/error-code binding) name)
                                               :error (:evidence/error binding)
                                               :member_role (member-role-sql (:evidence/member-role binding))
                                               :created_at now}))]})))
          (let [bundle (row->evidence-bundle tx-info
                         (require-row tx-info :evidence_bundle bundle-id "evidence_bundle not found after insert"))]
            (emit-proof-event! :proof/evidence-bundle-created {:evidence-bundle bundle})
            bundle))))))

(defn db-get-evidence-bundle
  [db-info bundle-id]
  (when (ds db-info)
    (some->> (query-one! db-info {:select [:*]
                                  :from :evidence_bundle
                                  :where [:= :id (->ref bundle-id)]})
      (row->evidence-bundle db-info))))

(defn- attestation-kind-sql
  [kind]
  (let [kind (input-keyword kind)
        stored (kw-sql kind)]
    (when-not (contains? proof/attestation-kinds kind)
      (throw (ex-info "attestation kind is not supported" {:kind kind})))
    stored))

(defn- attestation-decision-sql
  [decision]
  (let [decision (input-keyword decision)]
    (when-not (contains? proof/attestation-decisions decision)
      (throw (ex-info "attestation decision is not supported" {:decision decision})))
    (name decision)))

(defn- attestation-status-sql
  [status]
  (let [status (input-keyword (or status :accepted))]
    (when-not (contains? proof/attestation-statuses status)
      (throw (ex-info "attestation status is not supported" {:status status})))
    (name status)))

(defn- attester-kind-sql
  [kind]
  (when kind
    (let [kind (input-keyword kind)]
      (when-not (contains? proof/attester-kinds kind)
        (throw (ex-info "attester kind is not supported" {:attester-kind kind})))
      (name kind))))

(defn- row->attestation
  [row]
  (cond-> {:id (->uuid (:id row))
           :conversation-id (->uuid (:conversation_soul_id row))
           :kind (->kw-back (:kind row))
           :subject-kind (->kw-back (:subject_kind row))
           :subject-id (->uuid (:subject_id row))
           :evidence-bundle-id (->uuid (:evidence_bundle_id row))
           :decision (->kw-back (:decision row))
           :status (->kw-back (:status row))
           :created-at (->date (:created_at row))}
    (:reason row) (assoc :reason (:reason row))
    (:policy_version row) (assoc :policy-version (:policy_version row))
    (:attester_kind row) (assoc :attester-kind (->kw-back (:attester_kind row)))
    (:attester_id row) (assoc :attester-id (:attester_id row))
    (:schema_version row) (assoc :schema-version (:schema_version row))
    (:payload row) (assoc :payload (<-blob (:payload row)))
    (:payload_sha256 row) (assoc :payload-sha256 (:payload_sha256 row))))

(defn- accepted-bundle-row!
  [db-info bundle-id]
  (let [bundle-row (require-row db-info :evidence_bundle bundle-id "evidence_bundle not found")]
    (when-not (= "accepted" (:status bundle-row))
      (throw (ex-info "attestation requires an accepted evidence bundle"
               {:evidence-bundle-id (->ref bundle-id)
                :bundle-status (:status bundle-row)})))
    bundle-row))

(defn- update-plan-resolution-after-gate!
  [db-info gate-id]
  (let [gate-row (require-row db-info :conversation_intent_gate gate-id "conversation_intent_gate not found")
        plan-id (:plan_id gate-row)
        required-gates (query! db-info {:select [:status]
                                        :from :conversation_intent_gate
                                        :where [:and
                                                [:= :plan_id plan-id]
                                                [:= :required 1]]})
        statuses (set (map :status required-gates))]
    (cond
      (and (seq required-gates) (= #{"proven"} statuses))
      (execute! db-info {:update :conversation_intent_plan
                         :set {:status "completed"}
                         :where [:and
                                 [:= :id plan-id]
                                 [:= :status "active"]]})

      (contains? statuses "impeded")
      (execute! db-info {:update :conversation_intent_plan
                         :set {:status "abandoned"}
                         :where [:and
                                 [:= :id plan-id]
                                 [:= :status "active"]]})

      :else nil)))

(defn- completed-plan-for-intent
  [db-info intent-id]
  (query-one! db-info {:select [:*]
                       :from :conversation_intent_plan
                       :where [:and
                               [:= :intent_id (->ref intent-id)]
                               [:= :status "completed"]]
                       :order-by [[:created_at :desc]]}))

(defn- abandoned-plan-for-intent
  [db-info intent-id]
  (query-one! db-info {:select [:*]
                       :from :conversation_intent_plan
                       :where [:and
                               [:= :intent_id (->ref intent-id)]
                               [:= :status "abandoned"]]
                       :order-by [[:created_at :desc]]}))

(defn- impeded-required-gate-for-intent
  [db-info intent-id]
  (some (fn [plan]
          (query-one! db-info {:select [:*]
                               :from :conversation_intent_gate
                               :where [:and
                                       [:= :plan_id (:id plan)]
                                       [:= :required 1]
                                       [:= :status "impeded"]]}))
    (query! db-info {:select [:*]
                     :from :conversation_intent_plan
                     :where [:= :intent_id (->ref intent-id)]})))

(defn- require-intent-closure-preconditions!
  [db-info kind intent-id]
  (case kind
    :intent/fulfilled
    (when-not (completed-plan-for-intent db-info intent-id)
      (throw (ex-info "fulfilled intent attestation requires a completed plan"
               {:intent-id (->uuid (->ref intent-id))})))

    :intent/abandoned
    (when-not (or (abandoned-plan-for-intent db-info intent-id)
                (impeded-required-gate-for-intent db-info intent-id))
      (throw (ex-info "abandoned intent attestation requires an abandoned plan or impeded required gate"
               {:intent-id (->uuid (->ref intent-id))})))

    nil))

(defn db-create-attestation!
  "Persist an explicit decision over an accepted evidence bundle. Gate and intent
   attestations update entity status as attestation-derived projections; the
   legacy proof/impediment/ref blobs are not the authority."
  [db-info {:keys [evidence-bundle-id kind subject-kind subject-id decision status reason
                   policy-version attester-kind attester-id schema-version payload payload-sha256]}]
  (when (ds db-info)
    (sqlite-write-tx! db-info
      (fn [tx-info]
        (let [bundle-row (accepted-bundle-row! tx-info evidence-bundle-id)
              subject-kind (input-keyword subject-kind)
              subject-id (->ref subject-id)]
          (when-not (= (subject-kind-sql subject-kind) (:subject_kind bundle-row))
            (throw (ex-info "attestation subject kind must match evidence bundle"
                     {:attestation-subject-kind subject-kind
                      :bundle-subject-kind (:subject_kind bundle-row)})))
          (when-not (= subject-id (:subject_id bundle-row))
            (throw (ex-info "attestation subject id must match evidence bundle"
                     {:attestation-subject-id subject-id
                      :bundle-subject-id (:subject_id bundle-row)})))
          (let [kind-kw (input-keyword kind)
                id (new-id)
                status-s (attestation-status-sql status)
                row (into {}
                      (remove (comp nil? val)
                        {:id id
                         :conversation_soul_id (:conversation_soul_id bundle-row)
                         :kind (attestation-kind-sql kind-kw)
                         :subject_kind (subject-kind-sql subject-kind)
                         :subject_id subject-id
                         :evidence_bundle_id (->ref evidence-bundle-id)
                         :decision (attestation-decision-sql decision)
                         :status status-s
                         :reason reason
                         :policy_version policy-version
                         :attester_kind (attester-kind-sql attester-kind)
                         :attester_id attester-id
                         :schema_version schema-version
                         :payload (when (some? payload) (->blob payload))
                         :payload_sha256 (or payload-sha256 (when (some? payload) (payload-digest payload)))
                         :created_at (now-ms)}))]
            (when (and (= :intent subject-kind) (= "accepted" status-s))
              (require-intent-closure-preconditions! tx-info kind-kw subject-id))
            (execute! tx-info {:insert-into :attestation
                               :values [row]})
            (when (and (= :gate subject-kind) (= "accepted" status-s))
              (case kind-kw
                :gate/proven
                (execute! tx-info {:update :conversation_intent_gate
                                   :set {:status "proven"
                                         :proof (->blob {:authority :attestation
                                                         :attestation-id id
                                                         :evidence-bundle-id (->ref evidence-bundle-id)
                                                         :reason reason})
                                         :impediment nil
                                         :resolved_ref nil
                                         :resolved_at (now-ms)}
                                   :where [:= :id subject-id]})
                :gate/impeded
                (execute! tx-info {:update :conversation_intent_gate
                                   :set {:status "impeded"
                                         :impediment (->blob {:authority :attestation
                                                              :attestation-id id
                                                              :evidence-bundle-id (->ref evidence-bundle-id)
                                                              :reason reason})
                                         :proof nil
                                         :resolved_ref nil
                                         :resolved_at (now-ms)}
                                   :where [:= :id subject-id]})
                nil)
              (update-plan-resolution-after-gate! tx-info subject-id))
            (when (and (= :intent subject-kind) (= "accepted" status-s))
              (case kind-kw
                :intent/fulfilled
                (execute! tx-info {:update :conversation_intent
                                   :set {:status "fulfilled"
                                         :fulfillment_summary (or (:summary payload) reason)
                                         :abandonment_reason nil
                                         :resolved_ref nil
                                         :resolved_at (now-ms)}
                                   :where [:= :id subject-id]})
                :intent/abandoned
                (execute! tx-info {:update :conversation_intent
                                   :set {:status "abandoned"
                                         :fulfillment_summary nil
                                         :abandonment_reason reason
                                         :resolved_ref nil
                                         :resolved_at (now-ms)}
                                   :where [:= :id subject-id]})
                nil))
            (let [attestation (row->attestation
                                (require-row tx-info :attestation id "attestation not found after insert"))]
              (when (= :accepted (:status attestation))
                (emit-proof-event! :proof/attestation-accepted {:attestation attestation}))
              attestation)))))))

(defn db-attest-gate!
  [db-info {:keys [gate-id evidence-bundle-id kind] :as opts}]
  (let [kind (input-keyword (or kind :gate/proven))]
    (db-create-attestation! db-info
      (merge opts
        {:subject-kind :gate
         :subject-id gate-id
         :evidence-bundle-id evidence-bundle-id
         :kind kind
         :decision (case kind
                     :gate/impeded :impeded
                     :gate/proven :proven)}))))

(defn db-attest-intent!
  [db-info {:keys [intent-id evidence-bundle-id kind summary reason] :as opts}]
  (let [kind (input-keyword (or kind :intent/fulfilled))]
    (db-create-attestation! db-info
      (merge opts
        {:subject-kind :intent
         :subject-id intent-id
         :evidence-bundle-id evidence-bundle-id
         :kind kind
         :decision (case kind
                     :intent/abandoned :abandoned
                     :intent/fulfilled :fulfilled)
         :reason (or reason summary)
         :payload (cond-> (:payload opts)
                    summary (assoc :summary summary))}))))

(defn db-get-attestation
  [db-info attestation-id]
  (when (ds db-info)
    (some-> (query-one! db-info {:select [:*]
                                 :from :attestation
                                 :where [:= :id (->ref attestation-id)]})
      row->attestation)))

(defn- accepted-attestation-row
  [db-info subject-kind subject-id kind]
  (query-one! db-info {:select [:*]
                       :from :attestation
                       :where [:and
                               [:= :subject_kind (subject-kind-sql subject-kind)]
                               [:= :subject_id (->ref subject-id)]
                               [:= :kind (kw-sql kind)]
                               [:= :status "accepted"]]
                       :order-by [[:created_at :desc]]}))

(defn- audit-violation
  [type attrs]
  (assoc attrs :type type :blocking? true))

(defn db-audit-proof
  "Audit persisted proof state from ledger-derived attestations to entity state.
   This is a read-only validation surface: it reports state transitions that
   bypassed accepted attestations or no longer match their required aggregate
   state. It intentionally does not mutate old rows."
  [db-info {:keys [conversation-id]}]
  (if-not (ds db-info)
    {:success? true :violations [] :counts {}}
    (let [intent-rows (query! db-info
                        (cond-> {:select [:*]
                                 :from :conversation_intent
                                 :order-by [[:created_at :asc]]}
                          conversation-id (assoc :where [:= :conversation_soul_id (->ref conversation-id)])))
          intent-ids (set (map :id intent-rows))
          plan-rows (if (seq intent-ids)
                      (query! db-info {:select [:*]
                                       :from :conversation_intent_plan
                                       :where [:in :intent_id intent-ids]})
                      [])
          plan-ids (set (map :id plan-rows))
          gate-rows (if (seq plan-ids)
                      (query! db-info {:select [:*]
                                       :from :conversation_intent_gate
                                       :where [:in :plan_id plan-ids]})
                      [])
          plans-by-intent (group-by :intent_id plan-rows)
          gates-by-plan (group-by :plan_id gate-rows)
          gate-violations (mapcat
                            (fn [{:keys [id status plan_id]}]
                              (case status
                                "proven"
                                (when-not (accepted-attestation-row db-info :gate id :gate/proven)
                                  [(audit-violation :missing-gate-proof-attestation
                                     {:gate-id (->uuid id)
                                      :plan-id (->uuid plan_id)})])
                                "impeded"
                                (when-not (accepted-attestation-row db-info :gate id :gate/impeded)
                                  [(audit-violation :missing-gate-impediment-attestation
                                     {:gate-id (->uuid id)
                                      :plan-id (->uuid plan_id)})])
                                nil))
                            gate-rows)
          plan-violations (mapcat
                            (fn [{:keys [id intent_id status]}]
                              (let [required-gates (filter #(= 1 (long (:required %))) (get gates-by-plan id []))]
                                (case status
                                  "completed"
                                  (when-let [unproven (seq (remove #(= "proven" (:status %)) required-gates))]
                                    [(audit-violation :completed-plan-has-unproven-required-gates
                                       {:plan-id (->uuid id)
                                        :intent-id (->uuid intent_id)
                                        :gate-ids (mapv #(->uuid (:id %)) unproven)})])
                                  "abandoned"
                                  (when-not (some #(= "impeded" (:status %)) required-gates)
                                    [(audit-violation :abandoned-plan-has-no-impeded-required-gate
                                       {:plan-id (->uuid id)
                                        :intent-id (->uuid intent_id)})])
                                  nil)))
                            plan-rows)
          intent-violations (mapcat
                              (fn [{:keys [id status]}]
                                (let [plans (get plans-by-intent id [])]
                                  (case status
                                    "fulfilled"
                                    (concat
                                      (when-not (accepted-attestation-row db-info :intent id :intent/fulfilled)
                                        [(audit-violation :missing-intent-closure-attestation
                                           {:intent-id (->uuid id)})])
                                      (when-not (some #(= "completed" (:status %)) plans)
                                        [(audit-violation :fulfilled-intent-has-no-completed-plan
                                           {:intent-id (->uuid id)})]))
                                    "abandoned"
                                    (concat
                                      (when-not (accepted-attestation-row db-info :intent id :intent/abandoned)
                                        [(audit-violation :missing-intent-abandonment-attestation
                                           {:intent-id (->uuid id)})])
                                      (when-not (some #(= "abandoned" (:status %)) plans)
                                        [(audit-violation :abandoned-intent-has-no-abandoned-plan
                                           {:intent-id (->uuid id)})]))
                                    nil)))
                              intent-rows)
          violations (vec (concat gate-violations plan-violations intent-violations))
          audit {:success? (empty? violations)
                 :violations violations
                 :counts {:intents (count intent-rows)
                          :plans (count plan-rows)
                          :gates (count gate-rows)
                          :attestations (:c (query-one! db-info
                                              (cond-> {:select [[:%count.* :c]]
                                                       :from :attestation}
                                                conversation-id (assoc :where [:= :conversation_soul_id (->ref conversation-id)]))))}}]
      (doseq [violation violations]
        (emit-proof-event! :proof/audit-violation {:audit audit :violation violation}))
      audit)))

(defn- canonical-ref-or-throw! [ref]
  (when-not (proof/canonical-ref? ref)
    (throw (ex-info "provenance ref must be canonical"
             {:ref ref
              :accepted "turn/<turn8>/iteration/<n>/block/<k>[/tool/<tool-id>|/error]"})))
  ref)

(defn- observed-event-at-canonical-ref
  [db-info conversation-soul-id canonical-ref]
  (when-let [{:keys [conversation-prefix turn-prefix iteration block child]} (proof/parse-ref canonical-ref)]
    (let [turn-row (query-one! db-info
                     (cond-> {:select [[:ct.id :turn_id]
                                       [:cs.conversation_soul_id :conversation_soul_id]]
                              :from [[:conversation_turn_soul :ct]]
                              :join [[:conversation_state :cs] [:= :cs.id :ct.conversation_state_id]]
                              :where [:and
                                      [:= :cs.conversation_soul_id (->ref conversation-soul-id)]
                                      [:like :ct.id (str turn-prefix "%")]]}
                       conversation-prefix
                       (assoc :where [:and
                                      [:= :cs.conversation_soul_id (->ref conversation-soul-id)]
                                      [:like :cs.conversation_soul_id (str conversation-prefix "%")]
                                      [:like :ct.id (str turn-prefix "%")]])))]
      (when turn-row
        (let [it-row (query-one! db-info
                       {:select [:it.id :it.blocks :it.status]
                        :from [[:iteration :it]]
                        :join [[:conversation_turn_state :cts] [:= :cts.id :it.conversation_turn_state_id]]
                        :where [:and
                                [:= :cts.conversation_turn_soul_id (:turn_id turn-row)]
                                [:= :it.position iteration]]})
              blocks (<-blob (:blocks it-row))
              block-map (nth (vec (or blocks [])) (dec block) nil)]
          (when block-map
            (cond
              (= :error (:kind child))
              (when (and (:error block-map)
                      (= canonical-ref (str (get-in block-map [:provenance :ref]) "/error")))
                {:rendering-kind :vis/error
                 :provenance {:ref canonical-ref
                              :op :sci/error
                              :status :error}
                 :error (:error block-map)})

              (= :tool (:kind child))
              (some #(when (= canonical-ref (get-in % [:provenance :ref])) %)
                (:events block-map))

              (= canonical-ref (get-in block-map [:provenance :ref]))
              block-map)))))))

(defn- event-summary
  [iteration event]
  (let [provenance (:provenance event)]
    (cond-> {:ref       (:ref provenance)
             :status    (:status provenance)
             :op        (:op provenance)
             :iteration iteration}
      (:rendering-kind event) (assoc :rendering-kind (:rendering-kind event))
      (:parent-ref provenance) (assoc :parent-ref (:parent-ref provenance)))))

(defn- observed-event-summaries
  [db-info conversation-soul-id]
  (->> (query! db-info
         {:select [[:ct.id :turn_id]
                   [:it.position :iteration]
                   [:it.blocks :blocks]]
          :from [[:iteration :it]]
          :join [[:conversation_turn_state :cts] [:= :cts.id :it.conversation_turn_state_id]
                 [:conversation_turn_soul :ct] [:= :ct.id :cts.conversation_turn_soul_id]
                 [:conversation_state :cs] [:= :cs.id :ct.conversation_state_id]]
          :where [:= :cs.conversation_soul_id (->ref conversation-soul-id)]
          :order-by [[:ct.created_at :asc] [:it.position :asc]]})
    (mapcat (fn [{:keys [blocks iteration]}]
              (mapcat (fn [block]
                        (let [base-event (event-summary iteration block)
                              child-events (map #(event-summary iteration %) (:events block))]
                          (cond-> [base-event]
                            (seq child-events) (into child-events))))
                (or (<-blob blocks) []))))
    (filter :ref)
    vec))

(defn- nearest-observed-refs
  [db-info conversation-soul-id canonical-ref]
  (let [parsed      (proof/parse-ref canonical-ref)
        turn-prefix (:turn-prefix parsed)
        events      (observed-event-summaries db-info conversation-soul-id)
        same-turn   (when turn-prefix
                      (filterv #(str/starts-with? (:ref %) (str "turn/" turn-prefix "/")) events))
        candidates  (if (seq same-turn) same-turn events)]
    (->> candidates reverse (take 5) vec)))

(defn- not-observed-reason
  [nearest canonical-ref]
  (let [parsed (proof/parse-ref canonical-ref)]
    (cond
      (empty? nearest) :no-observed-refs
      (and (:iteration parsed)
        (let [latest-observed-iteration (reduce max 0 (keep :iteration nearest))]
          (> (:iteration parsed) latest-observed-iteration))) :not-observed-yet
      :else :not-observed)))

(defn- not-observed-message
  [canonical nearest]
  (str "Provenance ref is syntactically valid but is not observed yet: " canonical "\n"
    "Only refs from previous persisted journal entries are valid evidence.\n"
    "Current iteration refs are not valid until the next iteration.\n"
    "Do not construct refs from the current iteration number.\n"
    "Run `(v/latest-provenance-refs)` or `(v/provenance-timeline)` and cite one of its refs."
    (when (seq nearest)
      (str "\nNearest observed refs:\n"
        (str/join "\n"
          (map (fn [{:keys [ref status op]}]
                 (str "- " ref " " (pr-str status) " " (pr-str op)))
            nearest))))))

(defn- validate-provenance-refs!
  [db-info conversation-soul-id refs role]
  (doseq [ref refs]
    (let [ref-value (if (map? ref) (:ref ref) ref)
          canonical (canonical-ref-or-throw! (str ref-value))
          event     (observed-event-at-canonical-ref db-info conversation-soul-id canonical)]
      (when-not event
        (let [nearest (nearest-observed-refs db-info conversation-soul-id canonical)]
          (throw (ex-info (not-observed-message canonical nearest)
                   {:ref canonical
                    :conversation-soul-id conversation-soul-id
                    :reason (not-observed-reason nearest canonical)
                    :hint "Run `(v/latest-provenance-refs)` or `(v/provenance-timeline)` and cite an observed ref."
                    :nearest-observed nearest}))))
      (when (and (= :proof role) (not (proof/proof-compatible? event)))
        (throw (ex-info "proof provenance must cite a completed successful lifecycle event"
                 {:ref canonical
                  :status (get-in event [:provenance :status])})))
      (when (and (= :blocker role) (not (proof/blocker-compatible? event)))
        (throw (ex-info "blocker provenance cannot cite a running lifecycle event"
                 {:ref canonical
                  :status (get-in event [:provenance :status])}))))))

(defn- current-focused-intent-ids
  [db-info turn-state-id]
  (mapv :intent_id
    (query! db-info
      {:select [:intent_id]
       :from :conversation_intent_focus
       :where [:= :conversation_turn_state_id (->ref turn-state-id)]
       :order-by [[:created_at :asc]]})))

(defn- unresolved-intent? [db-info intent-id]
  (= "active" (:status (require-row db-info :conversation_intent intent-id "conversation_intent not found"))))

(defn- related-intents? [db-info from-id to-id]
  (or (= (->ref from-id) (->ref to-id))
    (boolean
      (query-one! db-info
        {:select [:id]
         :from :conversation_intent_relation
         :where [:and
                 [:or
                  [:and [:= :from_intent_id (->ref from-id)] [:= :to_intent_id (->ref to-id)]]
                  [:and [:= :from_intent_id (->ref to-id)] [:= :to_intent_id (->ref from-id)]]]
                 [:in :relation ["subintent" "supports" "related"]]]}))))

(defn- enforce-focus-switch!
  [db-info turn-state-id target-intent-id]
  (let [focused (filter #(unresolved-intent? db-info %) (current-focused-intent-ids db-info turn-state-id))]
    (when-let [blocking (some #(when-not (related-intents? db-info % target-intent-id) %) focused)]
      (throw (ex-info "This conversation is still focused on an unresolved intent. Finish or abandon that intent first; fork for unrelated parallel work."
               {:focused-intent-id blocking
                :target-intent-id target-intent-id
                :type :vis.intent/unrelated-focus-switch}))))
  true)

(defn db-focus-intent!
  [db-info intent-id {:keys [conversation-turn-id conversation-turn-state-id rationale source metadata]}]
  (when (ds db-info)
    (when-not (seq (str rationale))
      (throw (ex-info "focus-intent requires :rationale" {:intent-id intent-id})))
    (sqlite-write-tx! db-info
      (fn [tx-info]
        (let [turn-state-id (or (some-> conversation-turn-state-id ->ref)
                              (:id (require-latest-turn-state tx-info conversation-turn-id)))
              now           (now-ms)]
          (enforce-focus-switch! tx-info turn-state-id intent-id)
          (execute! tx-info
            {:insert-into :conversation_intent_focus
             :values [{:id (new-id)
                       :conversation_turn_state_id turn-state-id
                       :intent_id (->ref intent-id)
                       :source (->kw (or source :touched))
                       :metadata (->json (assoc (or metadata {}) :rationale rationale))
                       :created_at now}]
             :on-conflict [:conversation_turn_state_id :intent_id]
             :do-update-set {:source (->kw (or source :touched))
                             :metadata (->json (assoc (or metadata {}) :rationale rationale))
                             :created_at now}})
          (row->intent (require-row tx-info :conversation_intent intent-id "conversation_intent not found")))))))

(defn- previous-unresolved-focused-intent-ids
  [db-info conversation-turn-state-id]
  (let [ctx (turn-state-context db-info
              (:conversation_turn_soul_id
               (require-row db-info :conversation_turn_state conversation-turn-state-id
                 "conversation_turn_state not found")))]
    (mapv :intent_id
      (query! db-info
        {:select-distinct [:f.intent_id]
         :from [[:conversation_intent_focus :f]]
         :join [[:conversation_turn_state :cts] [:= :cts.id :f.conversation_turn_state_id]
                [:conversation_turn_soul :ct] [:= :ct.id :cts.conversation_turn_soul_id]
                [:conversation_state :cs] [:= :cs.id :ct.conversation_state_id]
                [:conversation_intent :i] [:= :i.id :f.intent_id]]
         :where [:and
                 [:= :cs.conversation_soul_id (:conversation_soul_id ctx)]
                 [:<> :f.conversation_turn_state_id (->ref conversation-turn-state-id)]
                 [:= :i.status "active"]]
         :order-by [[:cts.created_at :desc] [:f.created_at :asc]]}))))

(defn db-infer-focus!
  "Conservatively carry unresolved focus from the previous branch/run into the
   new turn-state. This makes focus switching enforcement real: unrelated new
   issue/focus calls cannot bypass unresolved focused work merely by starting a
   new turn."
  [db-info conversation-turn-id {:keys [rationale source metadata]
                                 :or {rationale "Inherited unresolved focus from previous turn."
                                      source :inferred}}]
  (when (and (ds db-info) conversation-turn-id)
    (sqlite-write-tx! db-info
      (fn [tx-info]
        (let [ctx (turn-state-context tx-info conversation-turn-id)
              turn-state-id (:conversation_turn_state_id ctx)
              now (now-ms)
              ids (previous-unresolved-focused-intent-ids tx-info turn-state-id)]
          (doseq [intent-id ids]
            (execute! tx-info
              {:insert-into :conversation_intent_focus
               :values [{:id (new-id)
                         :conversation_turn_state_id turn-state-id
                         :intent_id intent-id
                         :source (->kw source)
                         :metadata (->json (assoc (or metadata {}) :rationale rationale))
                         :created_at now}]
               :on-conflict [:conversation_turn_state_id :intent_id]
               :do-nothing true}))
          {:conversation-turn-state-id (->uuid turn-state-id)
           :focused-intent-ids (mapv ->uuid ids)})))))

(defn- intent-status-sql
  [status]
  (let [status (input-keyword (or status :active))]
    (when-not (contains? proof/intent-statuses status)
      (throw (ex-info "intent status is not supported"
               {:status status :allowed proof/intent-statuses})))
    (name status)))

(defn- intent-source-sql
  [source]
  (let [source (input-keyword (or source :user))]
    (when-not (contains? proof/intent-sources source)
      (throw (ex-info "intent source is not supported"
               {:source source :allowed proof/intent-sources})))
    (name source)))

(defn- defer-trigger-kind-sql
  [k]
  (when k
    (let [k (input-keyword k)]
      (when-not (contains? proof/defer-trigger-kinds k)
        (throw (ex-info "defer trigger kind is not supported" {:trigger-kind k})))
      (name k))))

(defn- defer-sibling-policy-sql
  [k]
  (when k
    (let [k (input-keyword k)]
      (when-not (contains? proof/defer-sibling-policies k)
        (throw (ex-info "defer sibling policy is not supported" {:policy k})))
      (name k))))

(defn- abandonment-scope-sql
  [k]
  (when k
    (let [k (input-keyword k)]
      (when-not (contains? proof/abandonment-scopes k)
        (throw (ex-info "abandonment scope is not supported" {:scope k})))
      (name k))))

(defn- intent-actor-kind-sql
  [k]
  (when k
    (let [k (input-keyword k)]
      (when-not (contains? proof/intent-acceptance-actor-kinds k)
        (throw (ex-info "intent actor kind is not supported (extensions cannot self-accept/resume)"
                 {:actor-kind k})))
      (name k))))

(defn db-store-intent!
  "Persist a new intent.

   PROOF.md Tasks 28–29: in addition to the legacy fields, this writer accepts:
     :status              — one of #{:suggested :active :deferred}.
                            Defaults to :active for back-compat.
     :source              — :user (default), :system, :extension.
     :owner-extension-id  — required when source = :extension; forbidden
                            otherwise (DB CHECK enforces).
     :parent-intent-id    — real intent-tree parent (subintent edge).
   The schema rejects illegal combinations (e.g. extension source without
   owner). Acceptance/defer/resume bookkeeping is set by db-accept-intent!,
   db-defer-intent!, etc., not by this writer."
  [db-info {:keys [conversation-id conversation-turn-id title rationale
                   created-ref metadata
                   status source owner-extension-id parent-intent-id]}]
  (when (ds db-info)
    (when-not (seq (str title))
      (throw (ex-info "issue-intent requires :title" {})))
    (when-not (seq (str rationale))
      (throw (ex-info "issue-intent requires :rationale" {})))
    (when created-ref (canonical-ref-or-throw! created-ref))
    (let [status-kw (input-keyword (or status :active))]
      (when-not (contains? #{:suggested :active :deferred} status-kw)
        (throw (ex-info "db-store-intent! only creates :suggested, :active, or :deferred intents"
                 {:status status-kw})))
      (let [src-kw (input-keyword (or source :user))]
        (when (and (= :extension src-kw) (not (seq (str owner-extension-id))))
          (throw (ex-info "extension-sourced intent requires :owner-extension-id" {})))
        (when (and (not= :extension src-kw) (seq (str owner-extension-id)))
          (throw (ex-info ":owner-extension-id only allowed when :source = :extension" {})))))
    (sqlite-write-tx! db-info
      (fn [tx-info]
        (let [ctx (when conversation-turn-id (turn-state-context tx-info conversation-turn-id))
              conversation-soul-id (or (:conversation_soul_id ctx)
                                     (conversation-soul-id-for-conversation tx-info conversation-id))
              id  (new-id)
              now (now-ms)
              status-kw (input-keyword (or status :active))
              status-name (intent-status-sql status-kw)
              source-name (intent-source-sql source)]
          (execute! tx-info
            {:insert-into :conversation_intent
             :values [(cond-> {:id id
                               :conversation_soul_id conversation-soul-id
                               :title title
                               :rationale rationale
                               :status status-name
                               :source source-name
                               :metadata (->json metadata)
                               :created_at now}
                        owner-extension-id (assoc :owner_extension_id owner-extension-id)
                        parent-intent-id (assoc :parent_intent_id (->ref parent-intent-id))
                        conversation-turn-id (assoc :created_conversation_turn_id (->ref conversation-turn-id))
                        created-ref (assoc :created_ref created-ref))]})
          ;; Suggested/deferred intents do NOT touch the legacy focus table
          ;; (focus implies active commitment); only :active intents do.
          (when (and (= :active status-kw) (:conversation_turn_state_id ctx))
            (db-focus-intent! tx-info id {:conversation-turn-state-id (:conversation_turn_state_id ctx)
                                          :rationale rationale
                                          :source :created}))
          (row->intent (require-row tx-info :conversation_intent id "conversation_intent not found")))))))

(defn db-store-intent-ref!
  [db-info intent-id {:keys [ref role metadata]}]
  (when (ds db-info)
    (canonical-ref-or-throw! ref)
    (sqlite-write-tx! db-info
      (fn [tx-info]
        (let [conversation-soul-id (intent-conversation-soul-id tx-info intent-id)]
          (validate-provenance-refs! tx-info conversation-soul-id [ref] :context))
        (execute! tx-info
          {:insert-into :conversation_intent_ref
           :values [{:id (new-id)
                     :intent_id (->ref intent-id)
                     :ref ref
                     :role (->kw (or role :context))
                     :metadata (->json metadata)
                     :created_at (now-ms)}]})
        {:intent-id (->uuid (->ref intent-id)) :ref ref :role (or role :context)}))))

(defn db-relate-intents!
  [db-info {:keys [from-intent-id to-intent-id relation rationale metadata]}]
  (when (ds db-info)
    (when (= (->ref from-intent-id) (->ref to-intent-id))
      (throw (ex-info "intent relation cannot target itself" {:intent-id from-intent-id})))
    (sqlite-write-tx! db-info
      (fn [tx-info]
        (let [id (new-id)]
          (execute! tx-info
            {:insert-into :conversation_intent_relation
             :values [{:id id
                       :from_intent_id (->ref from-intent-id)
                       :to_intent_id (->ref to-intent-id)
                       :relation (->kw relation)
                       :rationale rationale
                       :metadata (->json metadata)
                       :created_at (now-ms)}]})
          {:id (->uuid id)
           :from-intent-id (->uuid (->ref from-intent-id))
           :to-intent-id (->uuid (->ref to-intent-id))
           :relation relation
           :rationale rationale})))))

(defn- default-plan-dsl
  [intent-id steps]
  {:entry [:intent (->uuid (->ref intent-id))]
   :nodes {[:intent (->uuid (->ref intent-id))] {:kind :intent}}
   :edges []
   :steps (vec (or steps []))})

(defn db-store-plan!
  [db-info {:keys [intent-id summary plan steps created-ref metadata conversation-turn-id]}]
  (when (ds db-info)
    (when-not (seq (str summary))
      (throw (ex-info "issue-plan requires :summary" {:intent-id intent-id})))
    (when created-ref (canonical-ref-or-throw! created-ref))
    (sqlite-write-tx! db-info
      (fn [tx-info]
        (let [_intent (require-row tx-info :conversation_intent intent-id "conversation_intent not found")
              previous (:id (query-one! tx-info {:select [:id]
                                                 :from :conversation_intent_plan
                                                 :where [:and [:= :intent_id (->ref intent-id)] [:= :status "active"]]}))
              id (new-id)
              plan-dsl (or plan (default-plan-dsl intent-id steps))
              now (now-ms)]
          (when previous
            (execute! tx-info {:update :conversation_intent_plan
                               :set {:status "superseded"}
                               :where [:= :id previous]}))
          (execute! tx-info
            {:insert-into :conversation_intent_plan
             :values [(cond-> {:id id
                               :intent_id (->ref intent-id)
                               :status "active"
                               :summary summary
                               :plan_dsl (->blob plan-dsl)
                               :steps (->blob (when steps (vec steps)))
                               :supersedes_plan_id previous
                               :metadata (->blob metadata)
                               :created_at now}
                        conversation-turn-id (assoc :created_conversation_turn_id (->ref conversation-turn-id))
                        created-ref (assoc :created_ref created-ref))]})
          (row->plan (require-row tx-info :conversation_intent_plan id "conversation_intent_plan not found")))))))

(defn- normalize-proof-slot-id
  [slot]
  (when-not (and (vector? slot)
              (= 2 (count slot))
              (keyword? (second slot)))
    (throw (ex-info "proof slot must be [intent-id :slot-name]"
             {:slot slot})))
  [(->uuid (first slot)) (second slot)])

(defn- normalize-proof-slots
  [slots]
  (into {}
    (map (fn [[slot value]]
           [(normalize-proof-slot-id slot) value]))
    (or slots {})))

(defn- proof-guard-slot-ids
  [guard]
  (let [slots (atom [])]
    (letfn [(walk [x]
              (cond
                (and (vector? x) (= :slot (first x)))
                (do (swap! slots conj (second x))
                  (doseq [child (drop 2 x)] (walk child)))

                (map? x)
                (doseq [[k v] x] (walk k) (walk v))

                (coll? x)
                (doseq [child x] (walk child))))]
      (walk guard)
      @slots)))

(defn- normalize-expected-proof
  [expected-proof]
  (let [expected-proof (or expected-proof {:slots {}})
        slots          (normalize-proof-slots (:slots expected-proof))]
    (doseq [slot (proof-guard-slot-ids (:guard expected-proof))]
      (normalize-proof-slot-id slot))
    (assoc expected-proof :slots slots)))

(defn- normalize-proof-data
  [proof-data]
  (-> (or proof-data {})
    (update :slots normalize-proof-slots)
    (update :refs #(vec (or % [])))))

(defn- selector-value
  [proof-data selector]
  (cond
    (= selector [:refs]) (:refs proof-data)
    (and (vector? selector) (= :slot (first selector)))
    (get-in (:slots proof-data) (into [(normalize-proof-slot-id (second selector))] (drop 2 selector)))
    :else selector))

(defn- guard-value
  [proof-data x]
  (if (and (vector? x) (#{:slot :refs} (first x)))
    (selector-value proof-data x)
    x))

(defn- guard-passes?
  [proof-data guard]
  (letfn [(eval-guard [expr]
            (if-not (vector? expr)
              (boolean expr)
              (let [op (first expr)
                    args (rest expr)]
                (case op
                  :and (every? eval-guard args)
                  :or  (boolean (some eval-guard args))
                  :not (not (eval-guard (first args)))
                  :=   (= (guard-value proof-data (first args)) (guard-value proof-data (second args)))
                  :!=  (not= (guard-value proof-data (first args)) (guard-value proof-data (second args)))
                  :<   (< (guard-value proof-data (first args)) (guard-value proof-data (second args)))
                  :<=  (<= (guard-value proof-data (first args)) (guard-value proof-data (second args)))
                  :>   (> (guard-value proof-data (first args)) (guard-value proof-data (second args)))
                  :>=  (>= (guard-value proof-data (first args)) (guard-value proof-data (second args)))
                  :exists (some? (guard-value proof-data (first args)))
                  :contains (let [haystack (guard-value proof-data (first args))
                                  needle   (guard-value proof-data (second args))]
                              (cond
                                (string? haystack) (str/includes? haystack (str needle))
                                (coll? haystack)   (contains? (set haystack) needle)
                                :else false))
                  :matches (boolean (re-find (re-pattern (str (guard-value proof-data (second args))))
                                      (str (guard-value proof-data (first args)))))
                  (boolean (guard-value proof-data expr))))))]
    (if guard (eval-guard guard) true)))

(defn- required-proof-slots-present?
  [expected-proof proof-data]
  (every? (fn [[slot {:keys [required?]}]]
            (or (false? required?) (contains? (:slots proof-data) slot)))
    (:slots expected-proof)))

(defn- proof-ref-entries
  [proof-data role]
  (let [explicit (map #(if (map? %) (assoc % :role role) {:ref % :role role}) (:refs proof-data))
        slotted  (keep (fn [[slot value]]
                         (when-let [ref (:ref value)]
                           {:ref ref :role role :slot slot}))
                   (:slots proof-data))]
    (->> (concat explicit slotted)
      (reduce (fn [acc entry]
                (update acc [(:ref entry) (:role entry)] merge entry))
        {})
      vals
      vec)))

(defn- refs-covered?
  [existing requested]
  (let [existing-set (set (or existing []))]
    (every? existing-set (or requested []))))

(defn- proof-slots-covered?
  [existing-slots requested-slots]
  (every? (fn [[slot value]]
            (= value (get existing-slots slot)))
    (or requested-slots {})))

(defn- proven-gate-request-covered?
  [gate-row refs slots]
  (let [proof           (normalize-proof-data (<-blob (:proof gate-row)))
        requested-slots (normalize-proof-slots slots)]
    (and (refs-covered? (:refs proof) refs)
      (proof-slots-covered? (:slots proof) requested-slots))))

(defn db-store-gate!
  [db-info {:keys [plan-id proposition expected-proof candidate-proof required? created-ref metadata]
            legacy-question :question}]
  (when (ds db-info)
    (let [proposition (or proposition legacy-question)]
      (when-not (seq (str proposition))
        (throw (ex-info "issue-gate requires :proposition" {:plan-id plan-id})))
      (when created-ref (canonical-ref-or-throw! created-ref))
      (sqlite-write-tx! db-info
        (fn [tx-info]
          (let [_plan (require-row tx-info :conversation_intent_plan plan-id "conversation_intent_plan not found")
                id (new-id)
                expected-proof (normalize-expected-proof expected-proof)
                candidate-proof (normalize-proof-data candidate-proof)]
            (execute! tx-info
              {:insert-into :conversation_intent_gate
               :values [(cond-> {:id id
                                 :plan_id (->ref plan-id)
                                 :status "open"
                                 :required (if (false? required?) 0 1)
                                 :proposition proposition
                                 :expected_proof (->blob expected-proof)
                                 :candidate_proof (->blob candidate-proof)
                                 :metadata (->blob metadata)
                                 :created_at (now-ms)}
                          created-ref (assoc :created_ref created-ref))]})
            (row->gate (require-row tx-info :conversation_intent_gate id "conversation_intent_gate not found"))))))))

(defn- normalize-ref-entry [role ref]
  (let [{ref-value :ref ref-role :role ref-metadata :metadata slot :slot} (if (map? ref) ref {:ref ref})]
    (cond-> {:ref (str ref-value) :role (or ref-role role) :metadata ref-metadata}
      slot (assoc :slot (normalize-proof-slot-id slot)))))

(defn- store-gate-refs!
  [db-info gate-id refs role now]
  (doseq [{:keys [ref role metadata slot]} (map #(normalize-ref-entry role %) refs)]
    (execute! db-info
      {:insert-into :conversation_intent_gate_ref
       :values [(cond-> {:id         (new-id)
                         :gate_id    (->ref gate-id)
                         :ref        ref
                         :role       (->kw role)
                         :metadata   (->blob metadata)
                         :created_at now}
                  slot (assoc :intent_id (->ref (first slot))
                         :slot (name (second slot))))]})))

(defn db-offer-proof!
  [db-info {:keys [gate-id slots refs metadata]}]
  (when (ds db-info)
    (let [candidate-update (normalize-proof-data {:slots slots :refs refs})]
      (sqlite-write-tx! db-info
        (fn [tx-info]
          (let [gate    (require-row tx-info :conversation_intent_gate gate-id "conversation_intent_gate not found")
                previous (normalize-proof-data (<-blob (:candidate_proof gate)))
                candidate {:slots (merge (:slots previous) (:slots candidate-update))
                           :refs  (vec (distinct (concat (:refs previous) (:refs candidate-update))))}]
            (execute! tx-info
              {:update :conversation_intent_gate
               :set    (cond-> {:candidate_proof (->blob candidate)}
                         metadata (assoc :metadata (->blob metadata)))
               :where  [:= :id (->ref gate-id)]})
            (row->gate (require-row tx-info :conversation_intent_gate gate-id "conversation_intent_gate not found")
              (gate-refs tx-info gate-id))))))))

(defn db-prove-gate!
  [db-info {:keys [gate-id summary refs slots resolved-ref metadata]}]
  (when (ds db-info)
    (let [refs-v (vec refs)]
      (when-not (seq refs-v)
        (throw (ex-info "proven gate requires at least one proof ref" {:gate-id gate-id})))
      (when-not (seq (str summary))
        (throw (ex-info "proven gate requires :summary" {:gate-id gate-id})))
      (sqlite-write-tx! db-info
        (fn [tx-info]
          (let [gate-row (require-row tx-info :conversation_intent_gate gate-id "conversation_intent_gate not found")]
            (if (not= "open" (:status gate-row))
              (if (and (= "proven" (:status gate-row))
                    (proven-gate-request-covered? gate-row refs-v slots))
                (row->gate gate-row (gate-refs tx-info gate-id))
                (throw (ex-info "gate is already resolved"
                         {:type :vis/gate-already-resolved
                          :gate-id (->uuid (->ref gate-id))
                          :status (->kw-back (:status gate-row))})))
              (let [plan            (require-row tx-info :conversation_intent_plan (:plan_id gate-row) "conversation_intent_plan not found")
                    soul-id         (intent-conversation-soul-id tx-info (:intent_id plan))
                    now             (now-ms)
                    expected-proof  (normalize-expected-proof (<-blob (:expected_proof gate-row)))
                    candidate-proof (normalize-proof-data (<-blob (:candidate_proof gate-row)))
                    proof-data      (normalize-proof-data
                                      {:summary summary
                                       :refs refs-v
                                       :slots (merge (:slots candidate-proof) (normalize-proof-slots slots))
                                       :guard (:guard expected-proof)})]
                (when-not (required-proof-slots-present? expected-proof proof-data)
                  (throw (ex-info "proven gate proof is missing required slots"
                           {:gate-id gate-id
                            :required-slots (keys (:slots expected-proof))
                            :provided-slots (keys (:slots proof-data))})))
                (when-not (guard-passes? proof-data (:guard expected-proof))
                  (throw (ex-info "proven gate proof did not satisfy expected proof guard"
                           {:gate-id gate-id
                            :guard (:guard expected-proof)})))
                (validate-provenance-refs! tx-info soul-id refs-v :proof)
                (store-gate-refs! tx-info gate-id (proof-ref-entries proof-data :proof) :proof now)
                (execute! tx-info
                  {:update :conversation_intent_gate
                   :set    (cond-> {:status          "proven"
                                    :candidate_proof nil
                                    :proof           (->blob proof-data)
                                    :impediment      nil
                                    :resolved_at     now}
                             metadata     (assoc :metadata (->blob metadata))
                             resolved-ref (assoc :resolved_ref resolved-ref))
                   :where  [:= :id (->ref gate-id)]})
                (row->gate (require-row tx-info :conversation_intent_gate gate-id "conversation_intent_gate not found")
                  (gate-refs tx-info gate-id))))))))))

(defn db-impede-gate!
  [db-info {:keys [gate-id reason refs slots resolved-ref metadata]}]
  (when (ds db-info)
    (let [refs-v (vec refs)]
      (when-not (seq refs-v)
        (throw (ex-info "impeded gate requires at least one impediment ref" {:gate-id gate-id})))
      (when-not (seq (str reason))
        (throw (ex-info "impeded gate requires :reason" {:gate-id gate-id})))
      (sqlite-write-tx! db-info
        (fn [tx-info]
          (let [gate-row        (require-row tx-info :conversation_intent_gate gate-id "conversation_intent_gate not found")
                plan            (require-row tx-info :conversation_intent_plan (:plan_id gate-row) "conversation_intent_plan not found")
                soul-id         (intent-conversation-soul-id tx-info (:intent_id plan))
                now             (now-ms)
                candidate-proof (normalize-proof-data (<-blob (:candidate_proof gate-row)))
                impediment-data (normalize-proof-data
                                  {:reason reason
                                   :refs refs-v
                                   :slots (merge (:slots candidate-proof) (normalize-proof-slots slots))})]
            (validate-provenance-refs! tx-info soul-id refs-v :blocker)
            (store-gate-refs! tx-info gate-id (proof-ref-entries impediment-data :impediment) :impediment now)
            (execute! tx-info
              {:update :conversation_intent_gate
               :set    (cond-> {:status          "impeded"
                                :candidate_proof nil
                                :proof           nil
                                :impediment      (->blob impediment-data)
                                :resolved_at     now}
                         metadata     (assoc :metadata (->blob metadata))
                         resolved-ref (assoc :resolved_ref resolved-ref))
               :where  [:= :id (->ref gate-id)]})
            (row->gate (require-row tx-info :conversation_intent_gate gate-id "conversation_intent_gate not found")
              (gate-refs tx-info gate-id))))))))

(defn db-block-gate!
  [db-info opts]
  (db-impede-gate! db-info opts))

(defn- active-plan-for-intent [db-info intent-id]
  (query-one! db-info {:select [:*]
                       :from :conversation_intent_plan
                       :where [:and [:= :intent_id (->ref intent-id)] [:= :status "active"]]}))

(defn- required-active-gates [db-info plan-id]
  (query! db-info {:select [:*]
                   :from :conversation_intent_gate
                   :where [:and [:= :plan_id (->ref plan-id)] [:= :required 1]]}))

(defn- impeded-required-gate-refs
  "Return blocker refs already proven by required impeded gates on the
   intent's active plan. Used as abandonment evidence when callers omit refs:
   the gate graph already carries the blocking observation, so the model should
   not have to rediscover and copy those refs by hand."
  [db-info intent-id]
  (if-let [plan (active-plan-for-intent db-info intent-id)]
    (->> (required-active-gates db-info (:id plan))
      (filter #(= "impeded" (:status %)))
      (mapcat #(gate-refs db-info (:id %)))
      (filter #(= :impediment (:role %)))
      (map :ref)
      (remove str/blank?)
      distinct
      vec)
    []))

(defn- store-intent-resolution-refs!
  [db-info intent-id refs role now]
  (doseq [{:keys [ref role metadata]} (map #(normalize-ref-entry role %) refs)]
    (execute! db-info
      {:insert-into :conversation_intent_ref
       :values [{:id (new-id)
                 :intent_id (->ref intent-id)
                 :ref ref
                 :role (->kw role)
                 :metadata (->json metadata)
                 :created_at now}]})))

(defn- missing-intent-resolution-refs
  [db-info intent-id refs role-kw]
  (let [existing (->> (intent-refs db-info intent-id)
                   (keep (fn [{:keys [role ref]}]
                           (when (= role role-kw) ref)))
                   set)]
    (->> refs
      (map #(normalize-ref-entry role-kw %))
      (remove #(contains? existing (:ref %))))))

(defn db-fulfill-intent!
  [db-info intent-id {:keys [summary refs resolved-ref metadata conversation-turn-id]}]
  (when (ds db-info)
    (let [refs-v (vec refs)]
      (when-not (seq refs-v)
        (throw (ex-info "fulfilled intent requires at least one evidence ref" {:intent-id intent-id})))
      (when-not (seq (str summary))
        (throw (ex-info "fulfilled intent requires :summary" {:intent-id intent-id})))
      (sqlite-write-tx! db-info
        (fn [tx-info]
          (let [intent-row (require-row tx-info :conversation_intent intent-id "conversation_intent not found")]
            (if (not= "active" (:status intent-row))
              (if (= "fulfilled" (:status intent-row))
                (let [soul-id (intent-conversation-soul-id tx-info intent-id)
                      now     (now-ms)
                      missing (vec (missing-intent-resolution-refs tx-info intent-id refs-v :fulfillment-evidence))]
                  (when (seq missing)
                    (validate-provenance-refs! tx-info soul-id (mapv :ref missing) :proof)
                    (store-intent-resolution-refs! tx-info intent-id missing :fulfillment-evidence now))
                  (row->intent (require-row tx-info :conversation_intent intent-id "conversation_intent not found")
                    (intent-refs tx-info intent-id)
                    []
                    []))
                (throw (ex-info "intent is already resolved"
                         {:type :vis/intent-already-resolved
                          :intent-id (->uuid (->ref intent-id))
                          :status (->kw-back (:status intent-row))})))
              (let [soul-id (intent-conversation-soul-id tx-info intent-id)
                    plan    (active-plan-for-intent tx-info intent-id)
                    gates   (when plan (required-active-gates tx-info (:id plan)))
                    now     (now-ms)]
                (when-not plan
                  (throw (ex-info "fulfilled intent requires an active plan" {:intent-id intent-id})))
                (when (seq (remove #(= "proven" (:status %)) gates))
                  (throw (ex-info "fulfilled intent requires every required gate on the active plan to be proven"
                           {:intent-id intent-id
                            :gate-ids (mapv :id (remove #(= "proven" (:status %)) gates))})))
                (validate-provenance-refs! tx-info soul-id refs-v :proof)
                (store-intent-resolution-refs! tx-info intent-id refs-v :fulfillment-evidence now)
                (execute! tx-info
                  {:update :conversation_intent
                   :set (cond-> {:status "fulfilled"
                                 :fulfillment_summary summary
                                 :abandonment_reason nil
                                 :resolved_at now}
                          conversation-turn-id (assoc :resolved_conversation_turn_id (->ref conversation-turn-id))
                          resolved-ref (assoc :resolved_ref resolved-ref)
                          metadata (assoc :metadata (->json metadata)))
                   :where [:= :id (->ref intent-id)]})
                (row->intent (require-row tx-info :conversation_intent intent-id "conversation_intent not found")
                  (intent-refs tx-info intent-id)
                  []
                  [])))))))))

(defn db-abandon-intent!
  [db-info intent-id {:keys [reason refs resolved-ref metadata conversation-turn-id]}]
  (when (ds db-info)
    (when-not (seq (str reason))
      (throw (ex-info "abandoned intent requires :reason" {:intent-id intent-id})))
    (sqlite-write-tx! db-info
      (fn [tx-info]
        (let [explicit-refs (vec refs)
              refs-v (if (seq explicit-refs)
                       explicit-refs
                       (impeded-required-gate-refs tx-info intent-id))
              soul-id (intent-conversation-soul-id tx-info intent-id)
              now     (now-ms)]
          (when-not (seq refs-v)
            (throw (ex-info "abandoned intent requires at least one evidence ref" {:intent-id intent-id})))
          (validate-provenance-refs! tx-info soul-id refs-v :blocker)
          (store-intent-resolution-refs! tx-info intent-id refs-v :abandonment-evidence now)
          (execute! tx-info
            {:update :conversation_intent
             :set (cond-> {:status "abandoned"
                           :fulfillment_summary nil
                           :abandonment_reason reason
                           :resolved_at now}
                    conversation-turn-id (assoc :resolved_conversation_turn_id (->ref conversation-turn-id))
                    resolved-ref (assoc :resolved_ref resolved-ref)
                    metadata (assoc :metadata (->json metadata)))
             :where [:= :id (->ref intent-id)]})
          (row->intent (require-row tx-info :conversation_intent intent-id "conversation_intent not found")
            (intent-refs tx-info intent-id)
            []
            []))))))

;; ============================================================================
;; PROOF.md Tasks 30–34 — intent query surface, defer/resume, cursor, audit
;; ============================================================================

(defn- require-intent-row!
  [tx-info intent-id]
  (require-row tx-info :conversation_intent intent-id "conversation_intent not found"))

(defn- check-intent-transition!
  [from-row to-status]
  (let [from-status (->kw-back (:status from-row))]
    (when-not (proof/legal-intent-transition? from-status to-status)
      (throw (ex-info "illegal intent lifecycle transition"
               {:intent-id (->uuid (:id from-row))
                :from from-status
                :to to-status
                :legal-from-current
                (filterv #(proof/legal-intent-transition? from-status %)
                  proof/intent-statuses)})))))

(defn- legacy-intent-resolution-event-row!
  "Append a `:lifecycle` provenance_event so the new intent transition has a
   canonical ref to bundle/attest against. Used for the new defer/resume
   suite where callers do not always have a runtime block to cite. Returns
   the canonical ref string."
  [tx-info conversation-soul-id transition payload]
  (let [ref (str "turn/" (subs (str (java.util.UUID/randomUUID)) 0 8)
              "/iteration/1/block/1/tool/intent." (name transition))]
    (execute! tx-info
      {:insert-into :provenance_event
       :values [{:id (new-id)
                 :conversation_soul_id (->ref conversation-soul-id)
                 :ref ref
                 :status "done"
                 :kind "lifecycle"
                 :op (str "intent." (name transition))
                 :rendering_kind "vis/system"
                 :payload (->blob payload)
                 :payload_sha256 (payload-digest payload)
                 :created_at (now-ms)}]})
    ref))

;; ----------------------------------------------------------------------------
;; Task 30 — Intent query surface
;; ----------------------------------------------------------------------------

(defn db-list-intents
  "Durable intent query for extensions and host UIs. Replaces prompt scraping
   and event-bus subscription with a SQL-backed read.

   Filters (all optional, all AND-combined):
     :conversation-id        — scope to one conversation soul.
     :status                 — keyword or set of intent-statuses.
     :source                 — keyword or set of intent-sources.
     :owner-extension-id     — string; finds extension-owned suggestions.
     :parent-intent-id       — direct subintents only.
     :resumable?             — true: only deferred intents whose trigger has
                                been observed (resumable_at IS NOT NULL).
                                false: deferred intents NOT yet resumable.
     :limit / :offset        — pagination.

   Returns a vector of intent maps in created_at ASC order. Refs / plans /
   relations are NOT eagerly hydrated; use `db-intents` for the aggregate
   shape callers used to consume."
  [db-info {:keys [conversation-id status source owner-extension-id
                   parent-intent-id resumable? limit offset]}]
  (when (ds db-info)
    (let [status-set (cond
                       (nil? status) nil
                       (set? status) (set (map (comp name input-keyword) status))
                       :else #{(name (input-keyword status))})
          source-set (cond
                       (nil? source) nil
                       (set? source) (set (map (comp name input-keyword) source))
                       :else #{(name (input-keyword source))})
          where (cond-> [:and]
                  conversation-id (conj [:= :conversation_soul_id (->ref conversation-id)])
                  status-set     (conj [:in :status status-set])
                  source-set     (conj [:in :source source-set])
                  owner-extension-id (conj [:= :owner_extension_id owner-extension-id])
                  parent-intent-id (conj [:= :parent_intent_id (->ref parent-intent-id)])
                  (true? resumable?)  (conj [:and
                                             [:= :status "deferred"]
                                             [:is-not :resumable_at nil]])
                  (false? resumable?) (conj [:and
                                             [:= :status "deferred"]
                                             [:is :resumable_at nil]]))
          query (cond-> {:select [:*]
                         :from :conversation_intent
                         :where (if (= 1 (count where)) [:= 1 1] where)
                         :order-by [[:created_at :asc]]}
                  limit  (assoc :limit limit)
                  offset (assoc :offset offset))]
      (mapv row->intent (query! db-info query)))))

(defn db-get-intent
  "Fetch one intent by id. Returns the lightweight `row->intent` shape. Use
   `db-intents` for the full aggregate (plans/gates/relations) when needed."
  [db-info intent-id]
  (when (ds db-info)
    (when-let [row (query-one! db-info {:select [:*]
                                        :from :conversation_intent
                                        :where [:= :id (->ref intent-id)]})]
      (row->intent row))))

(defn db-intent-tree
  "Return the depth-first subintent tree rooted at `root-intent-id` as a
   vector of `{:intent <intent-map> :depth <n>}` rows. PROOF.md Task 32:
   `:active` intents in this tree are the cursor's execution candidates."
  [db-info root-intent-id]
  (when (ds db-info)
    (let [root (db-get-intent db-info root-intent-id)]
      (when root
        (loop [stack [{:intent root :depth 0}] acc []]
          (if (empty? stack)
            acc
            (let [{:keys [intent depth] :as node} (peek stack)
                  rest (pop stack)
                  children (mapv (fn [r] (row->intent r))
                             (query! db-info {:select [:*]
                                              :from :conversation_intent
                                              :where [:= :parent_intent_id (->ref (:id intent))]
                                              :order-by [[:created_at :asc]]}))
                  child-nodes (mapv (fn [c] {:intent c :depth (inc depth)}) children)]
              (recur (into rest (reverse child-nodes))
                (conj acc node)))))))))

;; ----------------------------------------------------------------------------
;; Task 31 — defer/resume APIs (and acceptance for suggested intents)
;;
;; All transitions go through the attestation ledger so audit can replay every
;; commitment hop. Each transition:
;;   1) appends a `:lifecycle` provenance_event with op = "intent.<verb>";
;;   2) creates an evidence_bundle citing that event;
;;   3) writes an attestation row;
;;   4) updates conversation_intent fields atomically in one tx.
;; ----------------------------------------------------------------------------

(defn- attest-intent-transition!
  [tx-info intent-id from-row {:keys [transition decision actor-kind actor-id
                                      reason payload conversation-soul-id]}]
  ;; Build a small intra-tx evidence bundle so audit gets a real attestation
  ;; chain. Reusing db-create-evidence-bundle! would re-open a tx; we inline
  ;; the writes for atomicity.
  (let [ref (legacy-intent-resolution-event-row! tx-info conversation-soul-id transition
              (merge {:intent-id (str (->uuid (:id from-row)))
                      :transition transition} payload))
        slot [(str (->uuid (:id from-row))) :lifecycle]
        bundle-id (new-id)
        member-id (new-id)
        att-id (new-id)
        now (now-ms)]
    (execute! tx-info {:insert-into :evidence_bundle
                       :values [{:id bundle-id
                                 :conversation_soul_id (->ref conversation-soul-id)
                                 :kind "closure"
                                 :subject_kind "intent"
                                 :subject_id (->ref intent-id)
                                 :source "derived"
                                 :status "accepted"
                                 :summary (str "intent " (name transition))
                                 :created_at now}]})
    (execute! tx-info {:insert-into :evidence_bundle_member
                       :values [{:id member-id
                                 :bundle_id bundle-id
                                 :position 0
                                 :slot (->blob slot)
                                 :event_ref ref
                                 :extract_path (->blob [:transition])
                                 :derived_value (->blob (name transition))
                                 :guard_ok 1
                                 :member_role "observation"
                                 :created_at now}]})
    (execute! tx-info {:insert-into :attestation
                       :values [{:id att-id
                                 :conversation_soul_id (->ref conversation-soul-id)
                                 :kind (str "intent/" (name transition))
                                 :subject_kind "intent"
                                 :subject_id (->ref intent-id)
                                 :evidence_bundle_id bundle-id
                                 :decision (name decision)
                                 :status "accepted"
                                 :reason reason
                                 :attester_kind (when actor-kind (name (input-keyword actor-kind)))
                                 :attester_id  actor-id
                                 :created_at now}]})
    {:ref ref :bundle-id bundle-id :attestation-id att-id}))

(defn db-suggest-intent!
  "Convenience writer: store an intent with `:status :suggested`. Extensions
   should use this rather than `db-store-intent!` so audit can read the
   `:suggested` lifecycle without inferring from `:source`."
  [db-info opts]
  (db-store-intent! db-info (assoc opts :status :suggested)))

(defn db-accept-intent!
  "Move a `:suggested` intent into a commitment. `actor-kind` must be
   `:user` or `:system-policy`; extensions cannot self-accept.

   Options:
     :actor-kind   — :user | :system-policy   (REQUIRED)
     :actor-id     — string id of the actor    (optional)
     :defer        — nil to accept directly into :active.
                     Map `{:trigger-kind ... :trigger-payload ... :sibling-policy ...}`
                     to accept directly into :deferred (commits the intent
                     and immediately defers it on the same trigger).
     :reason       — short rationale for audit."
  [db-info intent-id {:keys [actor-kind actor-id defer reason]}]
  (when (ds db-info)
    (when-not (intent-actor-kind-sql actor-kind)
      (throw (ex-info "db-accept-intent! requires :actor-kind :user or :system-policy"
               {:actor-kind actor-kind})))
    (sqlite-write-tx! db-info
      (fn [tx-info]
        (let [from-row (require-intent-row! tx-info intent-id)
              soul-id  (->uuid (:conversation_soul_id from-row))
              to       (if defer :deferred :active)]
          (check-intent-transition! from-row to)
          (when (= to :deferred)
            (when-not (defer-trigger-kind-sql (:trigger-kind defer))
              (throw (ex-info ":defer requires :trigger-kind" {:defer defer}))))
          (let [now (now-ms)
                update-set
                (cond-> {:status (intent-status-sql to)
                         :accepted_by_kind (intent-actor-kind-sql actor-kind)
                         :accepted_at now}
                  actor-id (assoc :accepted_by_id actor-id)
                  (= to :deferred) (assoc :defer_trigger_kind
                                     (defer-trigger-kind-sql (:trigger-kind defer))
                                     :defer_trigger_payload
                                     (some-> (:trigger-payload defer) ->blob)
                                     :defer_sibling_policy
                                     (defer-sibling-policy-sql
                                       (or (:sibling-policy defer) :defer/continue-siblings))))]
            (execute! tx-info
              {:update :conversation_intent
               :set update-set
               :where [:= :id (->ref intent-id)]})
            (let [audit (attest-intent-transition! tx-info intent-id from-row
                          {:transition (if (= to :deferred) :deferred :accepted)
                           :decision (if (= to :deferred) :deferred :accepted)
                           :actor-kind actor-kind
                           :actor-id actor-id
                           :reason (or reason
                                     (if (= to :deferred)
                                       "intent accepted into deferred state"
                                       "intent accepted into active state"))
                           :payload (cond-> {:to (name to)}
                                      defer (assoc :defer-trigger-kind
                                              (name (:trigger-kind defer))))
                           :conversation-soul-id soul-id})]
              (assoc (row->intent (require-intent-row! tx-info intent-id))
                :transition-attestation (:attestation-id audit)
                :transition-event-ref (:ref audit)))))))))

(defn db-defer-intent!
  "Move an `:active` (or `:suggested`) intent to `:deferred`. The trigger and
   sibling policy are recorded so audit can explain WHY the cursor stalled."
  [db-info intent-id {:keys [actor-kind actor-id trigger-kind trigger-payload
                             sibling-policy reason]}]
  (when (ds db-info)
    (when-not (intent-actor-kind-sql actor-kind)
      (throw (ex-info "db-defer-intent! requires :actor-kind :user or :system-policy"
               {:actor-kind actor-kind})))
    (when-not (defer-trigger-kind-sql trigger-kind)
      (throw (ex-info "db-defer-intent! requires :trigger-kind"
               {:trigger-kind trigger-kind})))
    (sqlite-write-tx! db-info
      (fn [tx-info]
        (let [from-row (require-intent-row! tx-info intent-id)
              soul-id  (->uuid (:conversation_soul_id from-row))]
          (check-intent-transition! from-row :deferred)
          (let [_now (now-ms)]
            (execute! tx-info
              {:update :conversation_intent
               :set {:status "deferred"
                     :defer_trigger_kind (defer-trigger-kind-sql trigger-kind)
                     :defer_trigger_payload (some-> trigger-payload ->blob)
                     :defer_sibling_policy (defer-sibling-policy-sql
                                             (or sibling-policy :defer/continue-siblings))
                     ;; Re-deferring clears any prior resumable mark.
                     :resumable_at nil}
               :where [:= :id (->ref intent-id)]})
            (let [audit (attest-intent-transition! tx-info intent-id from-row
                          {:transition :deferred
                           :decision :deferred
                           :actor-kind actor-kind
                           :actor-id actor-id
                           :reason (or reason "intent deferred")
                           :payload {:trigger-kind (name trigger-kind)
                                     :sibling-policy (name (or sibling-policy
                                                             :defer/continue-siblings))}
                           :conversation-soul-id soul-id})]
              (assoc (row->intent (require-intent-row! tx-info intent-id))
                :transition-attestation (:attestation-id audit)
                :transition-event-ref (:ref audit)))))))))

(defn db-mark-intent-resumable!
  "Mark a `:deferred` intent as resumable (its Defer Trigger has been
   observed). This does NOT move the cursor; a separate Resume Decision
   does. Extensions may call this when they observe their trigger."
  [db-info intent-id {:keys [observed-at observation-payload]}]
  (when (ds db-info)
    (sqlite-write-tx! db-info
      (fn [tx-info]
        (let [from-row (require-intent-row! tx-info intent-id)]
          (when-not (= "deferred" (:status from-row))
            (throw (ex-info "only :deferred intents can be marked resumable"
                     {:intent-id intent-id
                      :status (->kw-back (:status from-row))})))
          (execute! tx-info
            {:update :conversation_intent
             :set {:resumable_at (or observed-at (now-ms))}
             :where [:= :id (->ref intent-id)]})
          ;; This is an observation, not a Resume Decision. We append a
          ;; provenance_event so the resume audit chain has data, but we do
          ;; NOT write an attestation (no decision yet).
          (legacy-intent-resolution-event-row! tx-info
            (->uuid (:conversation_soul_id from-row))
            :resumable
            (cond-> {:intent-id (str (->uuid (:id from-row)))}
              observation-payload (assoc :observation observation-payload)))
          (row->intent (require-intent-row! tx-info intent-id)))))))

(defn db-resume-intent!
  "Move a resumable `:deferred` intent back to `:active`. Requires a Resume
   Decision actor; extensions may not move the cursor themselves."
  [db-info intent-id {:keys [actor-kind actor-id reason]}]
  (when (ds db-info)
    (when-not (intent-actor-kind-sql actor-kind)
      (throw (ex-info "db-resume-intent! requires :actor-kind :user or :system-policy"
               {:actor-kind actor-kind})))
    (sqlite-write-tx! db-info
      (fn [tx-info]
        (let [from-row (require-intent-row! tx-info intent-id)
              soul-id  (->uuid (:conversation_soul_id from-row))]
          (when-not (= "deferred" (:status from-row))
            (throw (ex-info "only :deferred intents can be resumed"
                     {:intent-id intent-id})))
          (when-not (:resumable_at from-row)
            (throw (ex-info "intent is not resumable; mark it resumable first"
                     {:intent-id intent-id})))
          (check-intent-transition! from-row :active)
          (let [now (now-ms)]
            (execute! tx-info
              {:update :conversation_intent
               :set {:status "active"
                     :resumed_by_kind (intent-actor-kind-sql actor-kind)
                     :resumed_by_id actor-id
                     :resumed_at now}
               :where [:= :id (->ref intent-id)]})
            (let [audit (attest-intent-transition! tx-info intent-id from-row
                          {:transition :resumed
                           :decision :resumed
                           :actor-kind actor-kind
                           :actor-id actor-id
                           :reason (or reason "deferred intent resumed")
                           :payload {}
                           :conversation-soul-id soul-id})]
              (assoc (row->intent (require-intent-row! tx-info intent-id))
                :transition-attestation (:attestation-id audit)
                :transition-event-ref (:ref audit)))))))))

;; ----------------------------------------------------------------------------
;; Task 32 — Intent cursor enforcement
;; ----------------------------------------------------------------------------

(defn db-get-intent-cursor
  "Return `{:conversation-id ... :intent-id ... :updated-at ...}` for the
   conversation cursor, or nil when no cursor row exists."
  [db-info conversation-id]
  (when (ds db-info)
    (when-let [row (query-one! db-info {:select [:*]
                                        :from :conversation_intent_cursor
                                        :where [:= :conversation_soul_id (->ref conversation-id)]})]
      {:conversation-id (->uuid (:conversation_soul_id row))
       :intent-id (some-> (:intent_id row) ->uuid)
       :updated-at (->date (:updated_at row))})))

(defn db-set-intent-cursor!
  "Set the running-intent cursor for `conversation-id` to `intent-id` (or nil
   to clear). The intent must be `:active` and live in the same conversation;
   the cursor row enforces single-running cardinality structurally."
  [db-info conversation-id intent-id]
  (when (ds db-info)
    (sqlite-write-tx! db-info
      (fn [tx-info]
        (when intent-id
          (let [row (require-intent-row! tx-info intent-id)]
            (when-not (= (:conversation_soul_id row) (->ref conversation-id))
              (throw (ex-info "intent does not belong to that conversation"
                       {:intent-id intent-id :conversation-id conversation-id})))
            (when-not (= "active" (:status row))
              (throw (ex-info "only :active intents may be the cursor"
                       {:intent-id intent-id
                        :status (->kw-back (:status row))})))))
        (let [now (now-ms)]
          (execute! tx-info
            {:insert-into :conversation_intent_cursor
             :values [{:conversation_soul_id (->ref conversation-id)
                       :intent_id (some-> intent-id ->ref)
                       :updated_at now}]
             :on-conflict :conversation_soul_id
             :do-update-set {:intent_id (some-> intent-id ->ref)
                             :updated_at now}})
          (db-get-intent-cursor tx-info conversation-id))))))

;; ----------------------------------------------------------------------------
;; Task 33 — Abandonment gate / scope
;; ----------------------------------------------------------------------------

(defn- collect-branch-descendants
  [tx-info root-id]
  (loop [stack [(->ref root-id)] acc #{}]
    (if (empty? stack)
      acc
      (let [head (peek stack)
            children (mapv :id (query! tx-info
                                 {:select [:id]
                                  :from :conversation_intent
                                  :where [:and
                                          [:= :parent_intent_id head]
                                          [:not-in :status ["fulfilled" "abandoned"]]]}))]
        (recur (into (pop stack) children) (conj acc head))))))

(defn db-abandon-intent-with-scope!
  "Abandonment with explicit scope. PROOF.md Task 33 says direction changes
   need an Abandonment Gate that names which intents are affected:
     :abandon/current-intent  (default) — only this intent.
     :abandon/current-branch            — this intent + descendants.
     :abandon/all-running               — every active/deferred intent in
                                          the conversation.
   This writes per-intent abandonment_scope so audit can read the decision
   later. It does NOT replace `db-abandon-intent!`'s evidence-ref contract
   for the primary intent: a non-empty `:refs` list is still required."
  [db-info intent-id {:keys [scope] :as opts}]
  (when (ds db-info)
    (let [scope-kw (or scope :abandon/current-intent)]
      (when-not (proof/abandonment-scope? scope-kw)
        (throw (ex-info "unknown abandonment scope"
                 {:scope scope-kw :allowed proof/abandonment-scopes})))
      (sqlite-write-tx! db-info
        (fn [tx-info]
          (let [from-row (require-intent-row! tx-info intent-id)
                soul-id  (->uuid (:conversation_soul_id from-row))
                scope-name (abandonment-scope-sql scope-kw)
                now (now-ms)
                ;; Other intents to abandon under this scope decision.
                ;; Collected BEFORE the primary abandonment so descendant
                ;; queries see their pre-abandonment status.
                others (case scope-kw
                         :abandon/current-intent #{}
                         :abandon/current-branch (disj (collect-branch-descendants
                                                         tx-info intent-id)
                                                   (->ref intent-id))
                         :abandon/all-running
                         (set (mapv :id (query! tx-info
                                          {:select [:id]
                                           :from :conversation_intent
                                           :where [:and
                                                   [:= :conversation_soul_id (->ref soul-id)]
                                                   [:in :status ["active" "deferred"]]
                                                   [:not= :id (->ref intent-id)]]}))))]
            ;; Primary intent first — reuse legacy abandon writer for the
            ;; evidence-ref contract; then stamp scope on top.
            (db-abandon-intent! tx-info intent-id (select-keys opts [:reason :refs :metadata
                                                                     :resolved-ref :conversation-turn-id]))
            (execute! tx-info
              {:update :conversation_intent
               :set {:abandonment_scope scope-name}
               :where [:= :id (->ref intent-id)]})
            ;; Cascade scope to others without re-running the evidence-ref
            ;; contract (these are scope-cascaded, not separately attested).
            (doseq [other-id others]
              (execute! tx-info
                {:update :conversation_intent
                 :set {:status "abandoned"
                       :abandonment_reason (str "cascade-from "
                                             (->uuid (:id from-row))
                                             " scope=" (name scope-kw))
                       :abandonment_scope scope-name
                       :resolved_at now}
                 :where [:and
                         [:= :id other-id]
                         [:not-in :status ["fulfilled" "abandoned"]]]}))
            ;; If the cursor pointed at any abandoned intent, clear it.
            (let [abandoned-set (conj others (->ref intent-id))]
              (execute! tx-info
                {:update :conversation_intent_cursor
                 :set {:intent_id nil :updated_at now}
                 :where [:and
                         [:= :conversation_soul_id (->ref soul-id)]
                         [:in :intent_id abandoned-set]]}))
            {:intent (row->intent (require-intent-row! tx-info intent-id))
             :scope scope-kw
             :cascaded-intent-ids (mapv ->uuid others)}))))))

;; ----------------------------------------------------------------------------
;; Task 34 — Deferred-intent audit / reporting
;; ----------------------------------------------------------------------------

(defn db-deferred-intent-report
  "Categorized snapshot of intent lifecycle state for one conversation.
   PROOF.md Task 34 says audit must distinguish:
     :active            — currently blocking the cursor.
     :suggested         — not yet committed; awaiting Intent Acceptance.
     :deferred-waiting  — deferred, trigger NOT yet observed.
     :deferred-resumable— deferred, trigger observed, awaiting Resume Decision.
     :sibling-blocking  — deferred subintents whose `:defer/block-parent`
                          policy stalls a sibling branch.
     :extension-owned   — suggested or deferred work owned by some extension."
  [db-info {:keys [conversation-id]}]
  (when (ds db-info)
    (let [intents (db-list-intents db-info {:conversation-id conversation-id})
          by-status (group-by :status intents)
          deferred  (get by-status :deferred [])]
      {:conversation-id (some-> conversation-id ->uuid)
       :active            (vec (get by-status :active []))
       :suggested         (vec (get by-status :suggested []))
       :deferred-waiting  (vec (filter #(nil? (:resumable-at %)) deferred))
       :deferred-resumable (vec (filter #(some? (:resumable-at %)) deferred))
       :sibling-blocking  (vec (filter #(= :defer/block-parent
                                          (:defer-sibling-policy %)) deferred))
       :extension-owned   (vec (filter #(= :extension (:source %))
                                 (concat (get by-status :suggested [])
                                   deferred)))
       :resolved
       {:fulfilled (vec (get by-status :fulfilled []))
        :abandoned (vec (get by-status :abandoned []))}})))

(defn- rows-by [k rows]
  (group-by k rows))

(defn- intent-relations [db-info intent-id]
  (mapv (fn [r]
          {:relation (->kw-back (:relation r))
           :intent-id (->uuid (:other_id r))
           :handle (id-handle "I" (:other_id r))
           :rationale (:rationale r)})
    (query! db-info
      {:select [[[:case [:= :from_intent_id (->ref intent-id)] :to_intent_id :else :from_intent_id] :other_id]
                :relation :rationale]
       :from :conversation_intent_relation
       :where [:or [:= :from_intent_id (->ref intent-id)] [:= :to_intent_id (->ref intent-id)]]})))

(defn- aggregate-intents [db-info conversation-soul-id]
  (let [intent-rows (query! db-info {:select [:*]
                                     :from :conversation_intent
                                     :where [:= :conversation_soul_id (->ref conversation-soul-id)]
                                     :order-by [[:created_at :asc]]})
        intent-ids  (set (map :id intent-rows))
        plan-rows   (if (seq intent-ids)
                      (query! db-info {:select [:*]
                                       :from :conversation_intent_plan
                                       :where [:in :intent_id intent-ids]
                                       :order-by [[:created_at :asc]]})
                      [])
        plan-ids    (set (map :id plan-rows))
        gate-rows   (if (seq plan-ids)
                      (query! db-info {:select [:*]
                                       :from :conversation_intent_gate
                                       :where [:in :plan_id plan-ids]
                                       :order-by [[:created_at :asc]]})
                      [])
        gates-by-plan (rows-by :plan_id gate-rows)
        plans-by-intent (rows-by :intent_id plan-rows)]
    (mapv (fn [intent-row]
            (let [plans (mapv (fn [plan-row]
                                (row->plan plan-row
                                  (mapv #(row->gate % (gate-refs db-info (:id %)))
                                    (get gates-by-plan (:id plan-row) []))))
                          (get plans-by-intent (:id intent-row) []))]
              (row->intent intent-row
                (intent-refs db-info (:id intent-row))
                plans
                (intent-relations db-info (:id intent-row)))))
      intent-rows)))

(defn- active-plans [intent]
  (filter #(= :active (:status %)) (:plans intent)))

(defn- intent-checks-and-violations [focused-ids intent]
  (let [focused? (contains? focused-ids (:id intent))]
    (if-not focused?
      [[] []]
      (cond
        (= :fulfilled (:status intent))
        [[{:check :intent-resolved :success? true :intent-id (:id intent)}] []]

        (= :abandoned (:status intent))
        [[{:check :intent-resolved :success? true :intent-id (:id intent)}] []]

        :else
        (let [plans (vec (active-plans intent))
              gates (mapcat :gates plans)
              required (filter :required? gates)
              violations (vec
                           (concat
                             [{:type :focused-intent-unresolved
                               :blocking? true
                               :intent-id (:id intent)
                               :message (str "Focused intent is still active: " (:title intent))}]
                             (when (empty? plans)
                               [{:type :missing-active-plan
                                 :blocking? true
                                 :intent-id (:id intent)
                                 :message "Focused active intent has no active plan."}])
                             (when (> (count plans) 1)
                               [{:type :multiple-active-plans
                                 :blocking? true
                                 :intent-id (:id intent)
                                 :message "Focused active intent has more than one active plan."}])
                             (when (and (= 1 (count plans)) (empty? gates))
                               [{:type :active-plan-without-gates
                                 :blocking? true
                                 :intent-id (:id intent)
                                 :plan-id (:id (first plans))
                                 :message "Active plan has no gates."}])
                             (map (fn [gate]
                                    {:type :required-open-gate
                                     :blocking? true
                                     :intent-id (:id intent)
                                     :gate-id (:id gate)
                                     :message (str "Required gate is open: " (:proposition gate))})
                               (filter #(= :open (:status %)) required))
                             (map (fn [gate]
                                    {:type :required-impeded-gate
                                     :blocking? true
                                     :intent-id (:id intent)
                                     :gate-id (:id gate)
                                     :message (str "Required gate is impeded; re-plan or abandon: " (:proposition gate))})
                               (filter #(= :impeded (:status %)) required))))]
          [[{:check :intent-resolved :success? false :intent-id (:id intent)}] violations])))))

(defn- code-ish [x]
  (str "`" x "`"))

(defn- refs-text [refs]
  (when (seq refs)
    (str/join ", " (map code-ish (distinct (map :ref refs))))))

(defn- proof-slots-text [slots]
  (when (seq slots)
    (str/join ", "
      (map (fn [[slot value]]
             (str (code-ish (pr-str slot)) " → " (code-ish (pr-str value))))
        slots))))

(defn- gate-report-markdown [gate]
  (str "    - Gate " (code-ish (:handle gate)) " " (name (:status gate))
    " — " (:proposition gate) "\n"
    "      - required: " (:required? gate) "\n"
    (when-let [slots (proof-slots-text (get-in gate [:expected-proof :slots]))]
      (str "      - expected slots: " slots "\n"))
    (when-let [guard (get-in gate [:expected-proof :guard])]
      (str "      - guard: " (code-ish (pr-str guard)) "\n"))
    (when-let [slots (proof-slots-text (get-in gate [:candidate-proof :slots]))]
      (str "      - candidate-proof slots: " slots "\n"))
    (when-let [proof (:proof gate)]
      (str "      - proof: " (:summary proof) "\n"
        (when-let [slots (proof-slots-text (:slots proof))]
          (str "      - proof slots: " slots "\n"))))
    (when-let [impediment (:impediment gate)]
      (str "      - impediment: " (:reason impediment) "\n"
        (when-let [slots (proof-slots-text (:slots impediment))]
          (str "      - impediment slots: " slots "\n"))))
    (when-let [refs (refs-text (:refs gate))]
      (str "      - refs: " refs "\n"))))

(defn- plan-report-markdown [plan]
  (str "  - Plan " (code-ish (:handle plan)) " " (name (:status plan))
    " — " (:summary plan) "\n"
    (when-let [plan-dsl (:plan plan)]
      (str "    - DSL: " (code-ish (pr-str plan-dsl)) "\n"))
    (when (seq (:steps plan))
      (str "    - steps: " (code-ish (pr-str (:steps plan))) "\n"))
    (if (seq (:gates plan))
      (str/join "" (map gate-report-markdown (:gates plan)))
      "    - gates: none\n")))

(defn- relation-report-markdown [relation]
  (str "  - " (name (:relation relation)) " " (code-ish (:handle relation))
    (when-let [rationale (:rationale relation)]
      (str " — " rationale))
    "\n"))

(defn- intent-report-markdown [intent]
  (str "- Intent " (code-ish (:handle intent)) " " (name (:status intent))
    " — " (:title intent) "\n"
    "  - id: " (code-ish (:id intent)) "\n"
    (when-let [refs (refs-text (:refs intent))]
      (str "  - refs: " refs "\n"))
    (when (seq (:relations intent))
      (str "  - relations:\n" (str/join "" (map relation-report-markdown (:relations intent)))))
    (if (seq (:plans intent))
      (str "  - plans:\n" (str/join "" (map plan-report-markdown (:plans intent))))
      "  - plans: none\n")))

(defn- intents-report-markdown [ok? focused-ids unfocused-active-ids intents violations]
  (str "## Intents\n\n"
    "- Scope: conversation\n"
    "- Status: " (if ok? "ok" (str "needs resolution (" (count violations) " violation(s))")) "\n"
    "- Focused: " (if (seq focused-ids) (str/join ", " focused-ids) "none") "\n"
    (when (seq unfocused-active-ids)
      (str "- Unfocused active intents: " (str/join ", " unfocused-active-ids) "\n"))
    "\n"
    (if (seq intents)
      (str/join "\n" (map intent-report-markdown intents))
      "_No intents._")
    (when (seq violations)
      (str "\n\n### Violations\n"
        (str/join "\n" (map #(str "- " (:message %)) violations))))
    "\n"))

(defn db-intents
  [db-info opts-or-conversation-turn-id]
  (let [opts (if (map? opts-or-conversation-turn-id)
               opts-or-conversation-turn-id
               {:conversation-turn-id opts-or-conversation-turn-id})]
    (if (ds db-info)
      (let [conversation-turn-id (:conversation-turn-id opts)
            ctx (when conversation-turn-id (turn-state-context db-info conversation-turn-id))
            conversation-soul-id (or (:conversation_soul_id ctx)
                                   (conversation-soul-id-for-opts db-info opts))
            turn-state-id (:conversation_turn_state_id ctx)
            intents (aggregate-intents db-info conversation-soul-id)
            focused-ids (set (mapv #(->uuid %) (if turn-state-id
                                                 (current-focused-intent-ids db-info turn-state-id)
                                                 [])))
            focused-ids-v (vec focused-ids)
            unfocused-active-ids (->> intents
                                   (filter #(and (= :active (:status %))
                                              (not (contains? focused-ids (:id %)))))
                                   (mapv :id))
            [checks violations] (reduce (fn [[cs vs] intent]
                                          (let [[ics ivs] (intent-checks-and-violations focused-ids intent)]
                                            [(into cs ics) (into vs ivs)]))
                                  [[] []]
                                  intents)
            violations (cond-> violations
                         (empty? focused-ids)
                         (conj {:type :missing-focused-intent
                                :blocking? true
                                :message "No focused conversation intent exists."}))
            ok? (empty? violations)]
        {:success? ok?
         :scope :conversation
         :conversation-id (->uuid conversation-soul-id)
         :turn-state-id (some-> turn-state-id ->uuid)
         :focused-intent-ids focused-ids-v
         :unfocused-active-intent-ids unfocused-active-ids
         :intents intents
         :checks checks
         :violations violations
         :report (intents-report-markdown ok? focused-ids-v unfocused-active-ids intents violations)})
      {:success? false
       :scope :conversation
       :conversation-id nil
       :turn-state-id nil
       :focused-intent-ids []
       :unfocused-active-intent-ids []
       :intents []
       :checks []
       :violations [{:type :missing-db :blocking? true :message "No persistence store is configured."}]
       :report "## Intents\n\n_No persistence store is configured._\n"})))

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
   - Realized collections (vectors, sets, maps, lists) → walk recursively, freeze.
   - Lazy seqs → `{:vis/ref :expr}`. A lazy seq IS a computation. Its durable
     form is the source code that produces it, not a materialized snapshot.
     Re-eval from :expr to reconstruct.
   - Functions, SCI vars → `{:vis/ref :expr}`. Same reason.
   - Plain scalars (strings, numbers, keywords, etc.) → pass through."
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
;; Iteration — iteration table
;; =============================================================================

(defn- normalize-rendering-kind
  [exec]
  (cond
    (:error exec) :vis/error
    (= :vis/silent (:rendering-kind exec)) :vis/silent
    (= true (:vis/silent exec)) :vis/silent
    (= :vis/system (:rendering-kind exec)) :vis/system
    (= :vis/tool (:rendering-kind exec)) :vis/tool
    (= :vis/answer (:rendering-kind exec)) :vis/answer
    (= :vis/diagnostic (:rendering-kind exec)) :vis/diagnostic
    (keyword? (:rendering-kind exec)) (:rendering-kind exec)
    :else :vis/sci))

(defn- normalized-provenance
  [turn-id-s iteration-position block-pos exec]
  (let [incoming-provenance (:provenance exec)
        rendering-kind (normalize-rendering-kind exec)
        status (cond
                 (:timeout? exec) :timeout
                 (:error exec) :error
                 :else :done)
        op (or (:op incoming-provenance)
             (case rendering-kind
               :vis/tool :v/tool
               :vis/system :vis/system
               :vis/answer :vis/answer
               :vis/error :sci/eval
               :sci/eval))]
    (:provenance
     (proof/event
       {:ref (proof/block-ref {:turn-prefix (subs turn-id-s 0 8)
                               :iteration iteration-position
                               :block (inc block-pos)})
        :op op
        :status status
        :rendering-kind rendering-kind
        :duration-ms (or (:duration-ms incoming-provenance)
                       (:execution-time-ms exec))
        :started-at-ms (:started-at-ms incoming-provenance)
        :finished-at-ms (:finished-at-ms incoming-provenance)}))))

(defn- structural-tool-result?
  [v]
  (and (map? v)
    (contains? v :success?)
    (contains? v :provenance)))

(defn- lifecycle-child-events
  [parent-provenance exec]
  (let [parent-ref (:ref parent-provenance)
        result (:result exec)
        running-tool-events (when-not (structural-tool-result? result)
                              (seq (:tool-events exec)))]
    (cond-> []
      (structural-tool-result? result)
      (conj
        (let [tool-prov (:provenance result)
              status (or (:status tool-prov)
                       (cond
                         (:error result) :error
                         (false? (:success? result)) :error
                         :else :done))
              op (or (:op tool-prov) :v/tool)
              ref (proof/child-ref parent-ref {:op op})]
          (proof/event
            {:ref ref
             :parent-ref parent-ref
             :op op
             :status status
             :rendering-kind :vis/tool
             :duration-ms (:duration-ms tool-prov)
             :started-at-ms (:started-at-ms tool-prov)
             :finished-at-ms (:finished-at-ms tool-prov)
             :metadata (dissoc tool-prov :op :duration-ms :started-at-ms :finished-at-ms)})))

      running-tool-events
      (into
        (map (fn [{:keys [op id started-at-ms tool] :as event}]
               (proof/start-event
                 {:ref (proof/child-ref parent-ref {:op op :id id})
                  :parent-ref parent-ref
                  :op (or op :v/tool)
                  :rendering-kind :vis/tool
                  :started-at-ms started-at-ms
                  :metadata (cond-> {:state :running
                                     :proof-note "This event proves only that a tool was started, not that it completed."}
                              tool (assoc :tool tool)
                              (seq event) (assoc :event (dissoc event :op :id :started-at-ms :tool)))}))
          running-tool-events))

      (instance? java.util.concurrent.Future result)
      (conj
        (proof/start-event
          {:ref (proof/child-ref parent-ref {:id :future})
           :parent-ref parent-ref
           :op :future/deferred
           :rendering-kind :vis/tool
           :metadata {:state :running
                      :proof-note "This event proves only that deferred work was started, not that it completed."}})))))

(defn- prepare-blocks-blob
  "Encode the per-iteration code-block log as one Nippy-frozen vec.
   Every persisted block receives canonical provenance and a namespaced
   `:rendering-kind`; obsolete presentation keys are stripped from provenance."
  [turn-id-s iteration-position blocks]
  (let [blank? (fn [s] (or (nil? s) (and (string? s) (str/blank? s))))]
    (->> (or blocks [])
      (map-indexed
        (fn [pos exec]
          (let [provenance (normalized-provenance turn-id-s iteration-position pos exec)
                child-events (lifecycle-child-events provenance exec)]
            (cond-> {:idx pos
                     :code (:code exec)
                     :provenance provenance
                     :rendering-kind (normalize-rendering-kind exec)}
              (some? (:comment exec))            (assoc :comment (:comment exec))
              (some? (:result exec))             (assoc :result (freeze-safe (:result exec)))
              (some? (:error exec))              (assoc :error  (str (:error exec)))
              (not (blank? (:stdout exec)))      (assoc :stdout (:stdout exec))
              (not (blank? (:stderr exec)))      (assoc :stderr (:stderr exec))
              (some? (:execution-time-ms exec))  (assoc :duration-ms (:execution-time-ms exec))
              (seq child-events)                 (assoc :events child-events)
              (:timeout? exec)                   (assoc :timeout? true)
              (:repaired? exec)                  (assoc :repaired? true)))))
      vec)))

(defn db-store-iteration!
  "Store one iteration row + per-`(def …)` expression_soul/expression_state
   rows. The iteration's full code-block log is written inline as a
   Nippy blob in `iteration.blocks` (no per-call rows; see V1 schema
   migration banner). Returns the iteration UUID."
  [db-info {:keys [conversation-turn-id blocks thinking answer answer-form-idx duration-ms vars error metadata
                   llm-messages llm-provider llm-model llm-raw-response llm-executable-code
                   llm-executable-blocks tokens cost-usd]}]
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
                                :from   :iteration
                                :where  [:= :conversation_turn_state_id conversation-turn-state-id-s]}))
                          1)
              raw-response-s (some-> llm-raw-response str)]
          ;; 1. Iteration row — includes the full block log inline as
          ;;    `iteration.blocks BLOB` (Nippy-encoded vec). Per-form
          ;;    observations live here; expression_soul/expression_state
          ;;    are only for named vars.
          (let [blocks-vec (prepare-blocks-blob conversation-turn-soul-id-s position blocks)]
            (execute! tx-info
              {:insert-into :iteration
               :values [(cond-> {:id                   iteration-id-s
                                 :conversation_turn_state_id       conversation-turn-state-id-s
                                 :position             position
                                 :status               (normalize-status (cond answer :done error :error :else :done))
                                 :llm_system_prompt    (when (seq llm-messages)
                                                         (:content (first (filter #(= "system" (:role %)) llm-messages))))
                                 :llm_user_prompt      (when (seq llm-messages) (->json llm-messages))
                                 :llm_provider         (when llm-provider (name (->kw llm-provider)))
                                 :llm_model            llm-model
                                 :llm_thinking         (or thinking "")
                                 :llm_full_duration_ms (or duration-ms 0)
                                 :llm_error            (when error (->json (if (map? error) error {:message (str error)})))
                                 :llm_returned_empty_blocks (if (empty? blocks) 1 0)
                                 :metadata             (when metadata (->json metadata))
                                 :blocks               (->blob blocks-vec)
                                 :llm_executable_code (some-> llm-executable-code str)
                                 :llm_executable_blocks (when (some? llm-executable-blocks)
                                                          (->json (vec llm-executable-blocks)))
                                 :created_at           now
                                 :finished_at          now}
                          (some? answer-form-idx)
                          (assoc :answer_form_idx answer-form-idx)
                          raw-response-s
                          (assoc :llm_raw_response         raw-response-s
                            :llm_raw_response_preview (raw-response-preview raw-response-s)
                            :llm_raw_response_length  (count raw-response-s)
                            :llm_raw_response_sha256  (sha256-hex raw-response-s))
                          ;; Token / cost columns — omitted when nil so the
                          ;; row keeps NULL (the schema marks them nullable
                          ;; for exactly this reason: an LLM call that
                          ;; failed before returning usage produces no
                          ;; tokens, no cost, no fake zeros).
                          (some? (:input tokens))     (assoc :llm_input_tokens     (long (:input tokens)))
                          (some? (:output tokens))    (assoc :llm_output_tokens    (long (:output tokens)))
                          (some? (:reasoning tokens)) (assoc :llm_reasoning_tokens (long (:reasoning tokens)))
                          (some? (:cached tokens))    (assoc :llm_cached_tokens    (long (:cached tokens)))
                          (some? cost-usd)            (assoc :llm_cost_usd         (double cost-usd)))]}))
          ;; 3. Vars → expression_soul (kind=var, stateful) + expression_state (versioned)
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
                               :iteration_id       iteration-id-s
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
      (:answer state-meta)           (assoc :answer (:answer state-meta))
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
    ;; Forensic fields — the full transcript surface needs these on
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
    (some? (:answer_form_idx row))      (assoc :answer-form-idx   (:answer_form_idx row))
    (some? (:llm_returned_empty_blocks row))
    (assoc :returned-empty-blocks? (= 1 (long (:llm_returned_empty_blocks row))))
    ;; Token / cost columns — ALWAYS present on the read side, with
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
            {:select [:*] :from :iteration
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
                    [:= :est.iteration_id iteration-id-s]
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
                            :from   :iteration
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
                       [:iteration :it]         [:= :it.id :est.iteration_id]
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

(defn- row->var-provenance
  [r]
  (cond-> {:conversation-state-id (:conversation_state_id r)}
    (:conversation_turn_soul_id r) (assoc :conversation-turn-id (->uuid (:conversation_turn_soul_id r)))
    (:iteration_id r)             (assoc :iteration-id (->uuid (:iteration_id r)))
    (:iteration_position r)       (assoc :iteration-position (:iteration_position r))))

(defn- row->var-index-entry
  [r]
  (let [value (<-blob (:result r))]
    (cond-> {:name        (symbol (:name r))
             :version     (:version r)
             :kind        (var-kind value (:expr r))
             :restorable? (var-restorable? value (:expr r))
             :created-at  (->date (:created_at r))
             :provenance  (row->var-provenance r)}
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
         (mapv row->var-index-entry
           (query! db-info
             (cond-> {:select [:es.name :es.conversation_state_id
                               :est.version :est.result :est.expr :est.created_at
                               [:est.iteration_id :iteration_id]
                               [:it.position :iteration_position]
                               :qst.conversation_turn_soul_id]
                      :from   [[:expression_soul :es]]
                      :join   [[:expression_state :est] [:= :est.expression_soul_id :es.id]
                               [:iteration :it] [:= :it.id :est.iteration_id]
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
                   :provenance (row->var-provenance r)}))
          (query! db-info
            {:select [:est.version :est.result :est.expr :est.created_at
                      [:est.iteration_id :iteration_id]
                      [:it.position :iteration_position]
                      :es.conversation_state_id
                      :qst.conversation_turn_soul_id]
             :from   [[:expression_state :est]]
             :join   [[:expression_soul :es] [:= :est.expression_soul_id :es.id]
                      [:iteration :it] [:= :it.id :est.iteration_id]
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
                      :provenance  (row->var-provenance r)}))
             (query! db-info
               (cond-> {:select [:es.name :es.conversation_state_id
                                 :est.version :est.result :est.expr :est.created_at
                                 [:est.iteration_id :iteration_id]
                                 [:it.position :iteration_position]
                                 :qst.conversation_turn_soul_id]
                        :from   [[:expression_state :est]]
                        :join   [[:expression_soul :es] [:= :est.expression_soul_id :es.id]
                                 [:iteration :it] [:= :it.id :est.iteration_id]
                                 [:conversation_turn_state :qst] [:= :qst.id :it.conversation_turn_state_id]]
                        :where  (cond-> [:and
                                         [:= :es.conversation_state_id state-id-s]
                                         [:= :es.kind "var"]]
                                  symbol (conj [:= :es.name (str symbol)]))
                        :order-by [[:est.created_at direction] [:es.name :asc] [:est.version direction]]}
                 (pos-int? limit) (assoc :limit limit)))))
         []))
     [])))

(defn db-turn-history [db-info conversation-id]
  (let [turns (db-list-conversation-turns db-info conversation-id)]
    (mapv (fn [idx turn]
            (let [turn-ref        (:id turn)
                  iteration-count (count (db-list-conversation-turn-iterations db-info turn-ref))
                  answer-raw      (or (:answer turn) "")
                  answer-preview  (subs answer-raw 0 (min (count answer-raw) 160))]
              {:turn-pos             idx
               :conversation-turn-id (:id turn)
               :created-at           (:created-at turn)
               :user-request         (:user-request turn)
               :status               (:status turn)
               :iteration-count      iteration-count
               :answer-preview       answer-preview}))
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

(defn- extension-scope-key
  [{:keys [scope-key conversation-soul-id conversation-state-id
           conversation-turn-state-id iteration-id iteration-block-index
           iteration-block-id]}]
  (cond
    (seq (str scope-key)) scope-key
    iteration-block-id (str "block-id:" iteration-block-id)
    (and iteration-id (some? iteration-block-index)) (str "block:" iteration-id ":" iteration-block-index)
    iteration-id (str "iteration:" iteration-id)
    conversation-turn-state-id (str "turn-state:" conversation-turn-state-id)
    conversation-state-id (str "conversation-state:" conversation-state-id)
    conversation-soul-id (str "conversation-soul:" conversation-soul-id)
    :else "global"))

(defn- extension-aggregate-sql-row
  [opts id now]
  (let [row {:id                          id
             :extension_id                (str (:extension-id opts))
             :aggregate_key               (->edn-text (:aggregate-key opts))
             :kind                        (->edn-text (:kind opts))
             :metadata                    (->json (:metadata opts))
             :content                     (->blob (:content opts))
             :scope_key                   (extension-scope-key opts)
             :conversation_soul_id        (some-> (:conversation-soul-id opts) ->ref)
             :conversation_state_id       (some-> (:conversation-state-id opts) ->ref)
             :conversation_turn_state_id  (some-> (:conversation-turn-state-id opts) ->ref)
             :iteration_id                (some-> (:iteration-id opts) ->ref)
             :iteration_block_index       (:iteration-block-index opts)
             :iteration_block_id          (some-> (:iteration-block-id opts) str)
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
    (when (and (or (:iteration_block_index row) (:iteration_block_id row))
            (nil? (:iteration_id row)))
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
                  (:iteration_id row)               (assoc :iteration-id (:iteration_id row))
                  (:iteration_block_index row)      (assoc :iteration-block-index (:iteration_block_index row))
                  (:iteration_block_id row)         (assoc :iteration-block-id (:iteration_block_id row)))
          aggregate-key (<-edn-text (:aggregate_key row))]
      {:id            (:id row)
       :extension-id  (:extension_id row)
       :aggregate-key aggregate-key
       :key           aggregate-key
       :kind          (<-edn-text (:kind row))
       :scope-key     (:scope_key row)
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
    (:scope-key opts)                  (conj [:= :scope_key (:scope-key opts)])
    (:conversation-soul-id opts)       (conj [:= :conversation_soul_id (->ref (:conversation-soul-id opts))])
    (:conversation-state-id opts)      (conj [:= :conversation_state_id (->ref (:conversation-state-id opts))])
    (:conversation-turn-state-id opts) (conj [:= :conversation_turn_state_id (->ref (:conversation-turn-state-id opts))])
    (:iteration-id opts)               (conj [:= :iteration_id (->ref (:iteration-id opts))])
    (contains? opts :iteration-block-index)
    (conj [:= :iteration_block_index (:iteration-block-index opts)])
    (:iteration-block-id opts)         (conj [:= :iteration_block_id (str (:iteration-block-id opts))])))

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
                             :iteration_id                (:iteration_id row)
                             :iteration_block_index       (:iteration_block_index row)
                             :iteration_block_id          (:iteration_block_id row)
                             :updated_at                  now}})
          (row->extension-aggregate
            (query-one! tx-info
              {:select [:*]
               :from   :extension_aggregate
               :where  [:and
                        [:= :extension_id (:extension_id row)]
                        [:= :aggregate_key (:aggregate_key row)]
                        [:= :kind (:kind row)]
                        [:= :scope_key (:scope_key row)]]})))))))

(defn db-get-extension-aggregate
  [db-info opts]
  (when (ds db-info)
    (row->extension-aggregate
      (query-one! db-info (assoc (extension-aggregate-select opts) :limit 1)))))

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
;; Restore — read all vars in topological order for sandbox reconstruction
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
;; Loading this namespace plugs the SQLite backend into the persistence
;; facade. `vis-persistance` itself never references this ns directly —
;; it resolves vars by name at call time, so as long as this file is
;; on the classpath and has been required somewhere, every facade fn
;; routes here.
;; =============================================================================

(vis/register-extension!
  (vis/extension
    {:ext/namespace 'com.blockether.vis.ext.persistance-sqlite.core
     :ext/doc       "SQLite + Flyway persistence backend."
     :ext/version   "0.3.0"
     :ext/author    "Blockether"
     :ext/owner     "vis"
     :ext/license   "Apache-2.0"
     :ext/persistance [{:persistance/id :sqlite
                        :persistance/ns 'com.blockether.vis.ext.persistance-sqlite.core}]}))
