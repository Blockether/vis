(ns com.blockether.vis.loop.storage.sqlite.core
  "SQLite store — connection lifecycle + shared low-level helpers.

   Connection lifecycle:
     (open-store db-spec)   → {:datasource ds :path ... :owned? bool :mode ...}
     (close-store store)    → idempotent dispose

   db-spec forms:
     nil              — no DB (returns nil)
     :memory          — SQLite in-memory (ephemeral, for tests)
     \"path/to.db\"   — persistent SQLite file
     {:path \"...\"}  — persistent SQLite file
     {:datasource ds} — caller-owned DataSource (NOT closed on dispose)

   PRAGMAs applied per connection:
     journal_mode=WAL, synchronous=NORMAL, foreign_keys=ON, busy_timeout=30000."
  (:require
   [clojure.edn :as edn]
   [com.blockether.svar.internal.util :as util]
   [honey.sql :as sql]
   [next.jdbc :as jdbc]
   [next.jdbc.result-set :as rs]
   [taoensso.trove :as trove])
  (:import
   (java.time Instant)
   (java.util Date UUID)
   (javax.sql DataSource)
   (org.sqlite SQLiteConfig SQLiteConfig$JournalMode SQLiteConfig$SynchronousMode)
   (org.sqlite.javax SQLiteConnectionPoolDataSource)))

;; =============================================================================
;; Type coercion helpers
;; =============================================================================

(defn ->id
  "UUID/string → canonical TEXT id. Nil → nil."
  [v]
  (cond
    (nil? v) nil
    (uuid? v) (str v)
    (string? v) v
    :else (str v)))

(defn ->uuid
  "TEXT id → UUID. Nil → nil."
  ^UUID [v]
  (cond
    (nil? v) nil
    (uuid? v) v
    (string? v) (try (UUID/fromString v) (catch IllegalArgumentException _ nil))
    :else nil))

(defn ->kw
  "Keyword/string → TEXT (no leading colon). Nil → nil."
  [v]
  (cond
    (nil? v) nil
    (keyword? v) (subs (str v) 1)
    :else (str v)))

(defn ->kw-back
  "TEXT → keyword. Nil → nil."
  [v]
  (when (and v (not= "" v))
    (keyword v)))

(defn ->epoch-ms
  "java.util.Date / Instant → epoch-ms long. Nil → nil."
  [v]
  (cond
    (nil? v) nil
    (instance? Date v) (.getTime ^Date v)
    (instance? Instant v) (.toEpochMilli ^Instant v)
    (number? v) (long v)
    :else nil))

(defn ->date
  "epoch-ms long → java.util.Date. Nil → nil."
  ^Date [v]
  (when v (Date. (long v))))

;; =============================================================================
;; Schema install — Flyway
;; =============================================================================

(def ^:private MIGRATIONS ["classpath:db/migration"])

(defn- install-schema!
  "Runs Flyway migrations against `ds`."
  [^DataSource ds]
  (let [flyway (-> (org.flywaydb.core.Flyway/configure)
                 (.dataSource ds)
                 (.locations (into-array String MIGRATIONS))
                 (.baselineOnMigrate true)
                 (.baselineVersion "0")
                 (.mixed true)
                 (.load))]
    (.migrate flyway))
  ds)

;; =============================================================================
;; Connection management — SQLite
;; =============================================================================

(defn- sqlite-datasource
  "Builds an SQLiteConnectionPoolDataSource with standard PRAGMAs."
  ^DataSource [^String url]
  (let [cfg (doto (SQLiteConfig.)
              (.setJournalMode SQLiteConfig$JournalMode/WAL)
              (.setSynchronous SQLiteConfig$SynchronousMode/NORMAL)
              (.enforceForeignKeys true)
              (.setBusyTimeout 30000))
        ds  (SQLiteConnectionPoolDataSource. cfg)]
    (.setUrl ds url)
    ds))

(def ^:private DB_FILENAME "rlm.db")

(defn- open-sqlite-at-dir
  "Open a pooled SQLite store rooted at directory `dir`."
  [^String dir]
  (.mkdirs (java.io.File. dir))
  (let [file (str dir "/" DB_FILENAME)
        ds   (sqlite-datasource (str "jdbc:sqlite:" file))]
    (install-schema! ds)
    {:datasource ds :conn ds :path dir :db-file file :backend :sqlite}))

;; =============================================================================
;; Connection management — SQLite in-memory (tests)
;; =============================================================================

(defn- open-sqlite-mem []
  (let [ds (sqlite-datasource "jdbc:sqlite::memory:")]
    (install-schema! ds)
    {:datasource ds :conn ds :path nil :db-file nil
     :backend :sqlite :owned? true :mode :memory}))

;; =============================================================================
;; Public API
;; =============================================================================

(defn open-store
  "Opens or wraps a SQL store. See ns docstring for db-spec forms.

   Returns nil when db-spec is nil. Otherwise returns:
     {:datasource ds :conn ds :path str|nil :db-file str|nil
      :backend :sqlite :owned? bool :mode kw}"
  [db-spec]
  (cond
    (nil? db-spec)
    nil

    (= :memory db-spec)
    (open-sqlite-mem)

    (string? db-spec)
    (assoc (open-sqlite-at-dir db-spec) :owned? false :mode :persistent)

    (map? db-spec)
    (cond
      (or (:datasource db-spec) (:conn db-spec))
      (let [ds (or (:datasource db-spec) (:conn db-spec))]
        (install-schema! ds)
        {:datasource ds :conn ds :path nil :db-file nil
         :backend :external :owned? false :mode :external})

      (:path db-spec)
      (assoc (open-sqlite-at-dir (:path db-spec)) :owned? false :mode :persistent)

      :else
      (throw (ex-info "Invalid db-spec map — expected :datasource or :path"
               {:type :rlm/invalid-db-spec :db-spec db-spec})))

    :else
    (throw (ex-info "Invalid db-spec — expected nil, :memory, path string, or map"
             {:type :rlm/invalid-db-spec :db-spec db-spec}))))

(defn close-store
  "Releases resources for a store. Idempotent."
  [_store]
  ;; SQLite pooled datasources are GC'd; no explicit close needed.
  nil)

;; =============================================================================
;; Low-level query helpers
;; =============================================================================

(defn ds [db-info] (:datasource db-info))

(defn now-ms ^long [] (System/currentTimeMillis))

(defn read-edn-safe
  "Tolerant EDN reader. Empty/nil → fallback. Parse error → fallback (logged)."
  [s fallback]
  (if (or (nil? s) (= "" s))
    fallback
    (try
      (edn/read-string s)
      (catch Exception e
        (trove/log! {:level :debug :id ::read-edn-safe-fallback
                     :data {:error (ex-message e)}
                     :msg "EDN parse failed, returning fallback"})
        fallback))))

(defn query!
  "Run a HoneySQL map and return rows with unqualified lower-case keys."
  [db-info q]
  (let [stmt (if (map? q) (sql/format q) q)]
    (jdbc/execute! (ds db-info) stmt {:builder-fn rs/as-unqualified-lower-maps})))

(defn query-one!
  "Run a HoneySQL map and return the first row (or nil)."
  [db-info q]
  (first (query! db-info q)))

(defn execute!
  "Run a HoneySQL map for side-effects (INSERT/UPDATE/DELETE)."
  [db-info q]
  (jdbc/execute! (ds db-info) (sql/format q)))

;; =============================================================================
;; Logging
;; =============================================================================

(defn log!
  "Insert a structured log row into the `log` table.

   `entry` keys:
     :level            — :trace :debug :info :warn :error :fatal (required)
     :ns               — source namespace string
     :message          — human-readable message
     :data             — arbitrary EDN data (will be pr-str'd)
     :conversation-soul-id  — scope to conversation
     :state-id              — scope to conversation_state
     :query-id              — scope to query
     :iteration-id          — scope to iteration
     :execution-id          — scope to execution"
  [db-info entry]
  (when (ds db-info)
    (let [now (now-ms)]
      (execute! db-info
        {:insert-into :log
         :values [(cond-> {:id         (str (java.util.UUID/randomUUID))
                           :level      (->kw (:level entry))
                           :created_at now}
                    (:ns entry)              (assoc :ns (str (:ns entry)))
                    (:message entry)         (assoc :message (:message entry))
                    (:data entry)            (assoc :data (pr-str (:data entry)))
                    (:conversation-soul-id entry) (assoc :conversation_soul_id (->id (:conversation-soul-id entry)))
                    (:state-id entry)        (assoc :state_id (->id (:state-id entry)))
                    (:query-id entry)        (assoc :query_id (->id (:query-id entry)))
                    (:iteration-id entry)    (assoc :iteration_id (->id (:iteration-id entry)))
                    (:execution-id entry)    (assoc :execution_id (->id (:execution-id entry))))]}))))

(defn query-logs
  "Query logs with optional filters.

   `opts` keys (all optional):
     :level            — keyword, filter by exact level
     :conversation-id  — filter by conversation
     :query-id         — filter by query
     :iteration-id     — filter by iteration
     :since            — epoch-ms, logs after this time
     :limit            — max rows (default 100)"
  [db-info opts]
  (when (ds db-info)
    (let [clauses (cond-> []
                    (:level opts)           (conj [:= :level (->kw (:level opts))])
                    (:conversation-soul-id opts) (conj [:= :conversation_soul_id (->id (:conversation-soul-id opts))])
                    (:query-id opts)        (conj [:= :query_id (->id (:query-id opts))])
                    (:iteration-id opts)    (conj [:= :iteration_id (->id (:iteration-id opts))])
                    (:since opts)           (conj [:>= :created_at (:since opts)]))
          where   (when (seq clauses) (into [:and] clauses))]
      (mapv (fn [r]
              (cond-> {:id         (->uuid (:id r))
                       :level      (->kw-back (:level r))
                       :created-at (->date (:created_at r))}
                (:ns r)              (assoc :ns (:ns r))
                (:message r)         (assoc :message (:message r))
                (:data r)            (assoc :data (read-edn-safe (:data r) nil))
                (:conversation_soul_id r) (assoc :conversation-soul-id (->uuid (:conversation_soul_id r)))
                (:state_id r)        (assoc :state-id (->uuid (:state_id r)))
                (:query_id r)        (assoc :query-id (->uuid (:query_id r)))
                (:iteration_id r)    (assoc :iteration-id (->uuid (:iteration_id r)))
                (:execution_id r)    (assoc :execution-id (->uuid (:execution_id r)))))
        (query! db-info
          (cond-> {:select   [:*]
                   :from     :log
                   :order-by [[:created_at :desc]]
                   :limit    (or (:limit opts) 100)}
            where (assoc :where where)))))))

;; =============================================================================
;; Delete tree — cascading delete via FK ON DELETE CASCADE
;; =============================================================================

(defn delete-conversation-tree!
  "Delete a conversation soul and all children.
   FK ON DELETE CASCADE handles the chain."
  [db-info conversation-soul-id]
  (when (and (ds db-info) conversation-soul-id)
    (execute! db-info {:delete-from :conversation_soul
                       :where [:= :id (->id conversation-soul-id)]})))
