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
     query_soul, query_state,
     iteration,
     expression_soul, expression_state, expression_dependency,
     log

   Connection lifecycle:
     (db-open! db-spec)   → {:datasource ds :path ...}
     (db-close! store)    → idempotent dispose"
  (:require
   [charred.api :as json]
   [clojure.string :as str]
   [com.blockether.vis.ext.persistance-sqlite.migration :as migration]
   [com.blockether.vis.core :as sdk]
   [honey.sql :as sql]
   [next.jdbc :as jdbc]
   [next.jdbc.result-set :as rs]
   [taoensso.nippy :as nippy])
  (:import
   (com.zaxxer.hikari HikariConfig HikariDataSource)
   (java.util UUID)
   (java.util.concurrent.atomic AtomicLong)
   (javax.sql DataSource)
   (org.sqlite SQLiteConfig SQLiteConfig$JournalMode SQLiteConfig$SynchronousMode SQLiteDataSource)))

;; =============================================================================
;; Helpers
;; =============================================================================

(def ds      sdk/ds)
(def now-ms  sdk/now-ms)
(def ->id    sdk/->id)
(def ->uuid  sdk/->uuid)
(def ->ref   sdk/->ref)
(def ->kw    sdk/->kw)
(def ->kw-back sdk/->kw-back)
(def ->date  sdk/->date)

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

(defn- <-blob
  "Deserialize a Nippy byte array from a BLOB column back to a Clojure value."
  [^bytes bs]
  (when bs (nippy/thaw bs)))

;; =============================================================================
;; Schema install
;;
;; The dialect-agnostic Flyway runner lives in `vis-persistance`; the
;; canonical V*__schema.sql files live there too, under
;; `resources/db/sqlite/migration/`. We just point the runner at the
;; classpath location and let Flyway handle ordering, idempotency,
;; and the `flyway_schema_history` baseline. The dialect-specific
;; Flyway driver (flyway-database-nc-sqlite) is pulled in through
;; this package's deps.edn so Flyway recognizes `jdbc:sqlite:` URLs.
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
   `foreign_keys=ON`, `busy_timeout=30000`) are set on the
   `SQLiteConfig` so every Hikari-handed connection inherits them.

   The returned object is what we hand to Hikari as its underlying
   DataSource; callers should NOT call `getConnection` on this directly."
  ^DataSource [^String url]
  (let [cfg (doto (SQLiteConfig.)
              (.setJournalMode SQLiteConfig$JournalMode/WAL)
              (.setSynchronous SQLiteConfig$SynchronousMode/NORMAL)
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
     idleTimeout     = 0  — SQLite handles are cheap to keep; don't
                            evict, don't churn WAL state.
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

(defn- open-sqlite-at-dir [^String dir]
  (.mkdirs (java.io.File. dir))
  (let [file   (str dir "/" DB_FILENAME)
        raw    (raw-sqlite-datasource (str "jdbc:sqlite:" file))
        pool   (pooled-datasource raw
                 (str "vis-rlm-disk-" (.incrementAndGet pool-counter)))]
    (install-schema! pool)
    {:datasource pool :conn pool :path dir :db-file file :backend :sqlite}))

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

(defn db-open! [db-spec]
  (cond
    (nil? db-spec)    nil
    (= :memory db-spec) (open-sqlite-mem)
    ;; `:owned? true` for both memory and persistent: we built the
    ;; Hikari pool ourselves, so we own its lifecycle. The old code
    ;; stamped persistent stores as `:owned? false` because the
    ;; non-pooled DataSource didn't *need* closing — it had no
    ;; resources to release. With Hikari that's no longer true; an
    ;; un-closed pool leaks daemon threads and connection handles.
    (string? db-spec) (assoc (open-sqlite-at-dir db-spec) :owned? true :mode :persistent)
    (map? db-spec)
    (cond
      (or (:datasource db-spec) (:conn db-spec))
      (let [ds (or (:datasource db-spec) (:conn db-spec))]
        (install-schema! ds)
        {:datasource ds :conn ds :path nil :db-file nil
         :backend :external :owned? false :mode :external})
      (:path db-spec)
      (assoc (open-sqlite-at-dir (:path db-spec)) :owned? true :mode :persistent)
      :else
      (throw (ex-info "Invalid db-spec map" {:type :vis/invalid-db-spec :db-spec db-spec})))
    :else
    (throw (ex-info "Invalid db-spec" {:type :vis/invalid-db-spec :db-spec db-spec}))))

(defn db-close!
  "Idempotent dispose. Closes the Hikari pool when we own it; for
   `:external` mode (caller-supplied DataSource) it's a no-op so we
   don't yank the rug out from under whoever passed us the handle."
  [store]
  (when (and store (:owned? store))
    (let [^Object ds (:datasource store)]
      (when (instance? java.io.Closeable ds)
        (try (.close ^java.io.Closeable ds) (catch Throwable _ nil)))))
  nil)

;; =============================================================================
;; Logging — log table
;; =============================================================================

(defn db-log! [db-info entry]
  (when (ds db-info)
    (execute! db-info
      {:insert-into :log
       :values [(cond-> {:id         (str (UUID/randomUUID))
                         :level      (->kw (:level entry))
                         :event      (str (:event entry))
                         :created_at (now-ms)}
                  (:data entry)                  (assoc :data (:data entry))
                  (:conversation-soul-id entry)  (assoc :conversation_soul_id (->id (:conversation-soul-id entry)))
                  (:conversation-state-id entry) (assoc :conversation_state_id (->id (:conversation-state-id entry)))
                  (:query-soul-id entry)         (assoc :query_soul_id (->id (:query-soul-id entry)))
                  (:query-state-id entry)        (assoc :query_state_id (->id (:query-state-id entry)))
                  (:iteration-id entry)          (assoc :iteration_id (->id (:iteration-id entry)))
                  (:expression-soul-id entry)    (assoc :expression_soul_id (->id (:expression-soul-id entry)))
                  (:expression-state-id entry)   (assoc :expression_state_id (->id (:expression-state-id entry))))]})))

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
    (let [soul-id  (UUID/randomUUID)
          state-id (UUID/randomUUID)
          now      (now-ms)]
      (execute! db-info
        {:insert-into :conversation_soul
         :values [{:id         (str soul-id)
                   :metadata   (->json {:channel     (->kw (or channel :tui))
                                        :external-id external-id})
                   :created_at now}]})
      (execute! db-info
        {:insert-into :conversation_state
         :values [{:id                   (str state-id)
                   :conversation_soul_id (str soul-id)
                   :title                title
                   :version              0
                   :metadata             (->json (cond-> {:system-prompt (or system-prompt "")
                                                          :model         (or model "")}
                                                   provider (assoc :provider (->kw provider))))
                   :created_at           now}]})
      soul-id)))

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
      ;; conversation_soul.metadata contains channel — we need to filter by it.
      ;; SQLite json_extract works on the metadata column.
      (mapv (fn [row]
              (let [soul-meta (<-json (:soul_metadata row))]
                {:id          (->uuid (:id row))
                 :channel     (->kw-back (:channel soul-meta))
                 :external-id (:external-id soul-meta)
                 :title       (or (:state_title row) (:title soul-meta))
                 :version     (:version row)
                 :created-at  (->date (:created_at row))}))
        (jdbc/execute! (ds db-info)
          [(str "SELECT cs.id, cs.metadata AS soul_metadata, cs.created_at,"
             " s.title AS state_title, s.version, s.metadata AS state_metadata"
             " FROM conversation_soul cs"
             " JOIN conversation_state s ON s.conversation_soul_id = cs.id"
             " WHERE json_extract(cs.metadata, '$.channel') = ?"
             " AND s.version = (SELECT MAX(s2.version) FROM conversation_state s2"
             "   WHERE s2.conversation_soul_id = cs.id)"
             " ORDER BY cs.created_at DESC")
           ch]
          {:builder-fn rs/as-unqualified-lower-maps})))))

(defn db-find-conversation-by-external [db-info channel external-id]
  (when (and (ds db-info) external-id)
    (let [ch (->kw channel)
          ext (str external-id)]
      (when-let [row (first
                       (jdbc/execute! (ds db-info)
                         [(str "SELECT id FROM conversation_soul"
                            " WHERE json_extract(metadata, '$.channel') = ?"
                            " AND json_extract(metadata, '$.\"external-id\"') = ?")
                          ch ext]
                         {:builder-fn rs/as-unqualified-lower-maps}))]
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
    (let [soul-id-s (->ref conversation-id)]
      (when-let [state (latest-state-for db-info soul-id-s)]
        (execute! db-info
          {:update :conversation_state
           :set    {:title title}
           :where  [:= :id (:id state)]})))))

(defn db-delete-conversation-tree! [db-info conversation-soul-id]
  (when (and (ds db-info) conversation-soul-id)
    (execute! db-info
      {:delete-from :conversation_soul
       :where [:= :id (->id conversation-soul-id)]})))

;; =============================================================================
;; Fork — branch a conversation at a point
;; =============================================================================

(defn db-list-conversation-states
  "List every `conversation_state` row for the soul behind `conversation-id`,
   oldest version first. Each row maps to
   `{:state-id :version :parent-state-id :title :system-prompt :provider :model
     :created-at :query-count}` — the raw fork tree of one conversation soul.

   The trunk is `:version 0` with `:parent-state-id nil`. A fork is any row
   whose `:parent-state-id` points at another `:state-id` in the same vector;
   group-by `:parent-state-id` to walk the tree.

   `:query-count` is the number of `query_soul` rows hanging off that specific
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
                             :from   :query_soul
                             :where  [:= :query_soul.conversation_state_id :cs.id]}
                            :query_count]]
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
                         :query-count     (or (:query_count row) 0)}
                  (:system-prompt state-meta) (assoc :system-prompt (:system-prompt state-meta))
                  (:provider state-meta)      (assoc :provider (->kw-back (:provider state-meta)))
                  (:model state-meta)         (assoc :model (:model state-meta)))))
        rows))
    []))

(defn db-list-query-states
  "List every `query_state` row (i.e. every retry version) for the soul behind
   `query-id`, oldest version first. Each row maps to
   `{:state-id :version :forked-from-query-state-id :status :prior-outcome
     :provider :model :created-at :iteration-count}`.

   Version 0 with `:forked-from-query-state-id nil` is the original run; any
   higher version is a retry, with `:forked-from-query-state-id` pointing at
   the previous `:state-id`. `:iteration-count` is the number of `iteration`
   rows attached to that specific state — retries get their own iteration
   trace.

   Returns `[]` (never nil) when the query is unknown or the env has no
   datasource."
  [db-info query-id]
  (if (and (ds db-info) query-id)
    (let [soul-id-s (->ref query-id)
          rows (query! db-info
                 {:select [:qst.id :qst.version :qst.forked_from_query_state_id
                           :qst.status :qst.prior_outcome
                           :qst.llm_root_provider :qst.llm_root_model
                           :qst.created_at
                           [{:select [[[:count :*]]]
                             :from   :iteration
                             :where  [:= :iteration.query_state_id :qst.id]}
                            :iteration_count]]
                  :from   [[:query_state :qst]]
                  :where  [:= :qst.query_soul_id soul-id-s]
                  :order-by [[:qst.version :asc]]})]
      (mapv (fn [row]
              (cond-> {:state-id                    (->uuid (:id row))
                       :version                     (:version row)
                       :forked-from-query-state-id  (some-> (:forked_from_query_state_id row) ->uuid)
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
   Returns the new state UUID."
  [db-info conversation-id {:keys [system-prompt provider model title]}]
  (when (ds db-info)
    (let [soul-id-s (->ref conversation-id)
          current   (latest-state-for db-info soul-id-s)
          new-id    (UUID/randomUUID)
          now       (now-ms)
          cur-meta  (when current (<-json (:metadata current)))]
      (when current
        (execute! db-info
          {:insert-into :conversation_state
           :values [{:id                   (str new-id)
                     :conversation_soul_id soul-id-s
                     :parent_state_id      (:id current)
                     :title                (or title (:title current))
                     :version              (inc (:version current))
                     :metadata             (->json
                                             (cond-> (or cur-meta {})
                                               system-prompt (assoc :system-prompt system-prompt)
                                               provider      (assoc :provider (->kw provider))
                                               model         (assoc :model model)))
                     :created_at           now}]})
        new-id))))

;; =============================================================================
;; State resolution
;; =============================================================================

(defn- latest-state-id [db-info conversation-id]
  (when (ds db-info)
    (let [soul-id-s (->ref conversation-id)]
      (:id (latest-state-for db-info soul-id-s)))))

;; =============================================================================
;; Query — query_soul + query_state
;; =============================================================================

(defn db-store-query!
  "Create query_soul + initial query_state (version 0).
   Returns the query-soul UUID."
  [db-info {:keys [parent-conversation-id query messages status]}]
  (when (ds db-info)
    (let [soul-id  (UUID/randomUUID)
          state-id (UUID/randomUUID)
          now      (now-ms)
          state-id-s (latest-state-id db-info parent-conversation-id)
          q        (or query "")]
      (execute! db-info
        {:insert-into :query_soul
         :values [{:id                    (str soul-id)
                   :conversation_state_id state-id-s
                   :title                 (subs q 0 (min (count q) 100))
                   :query                 q
                   :created_at            now}]})
      (execute! db-info
        {:insert-into :query_state
         :values [{:id            (str state-id)
                   :query_soul_id (str soul-id)
                   :version       0
                   :status        (normalize-status (or status :running))
                   :metadata      (->json (when messages {:messages messages}))
                   :created_at    now}]})
      soul-id)))

(defn- latest-query-state [db-info query-soul-id-s]
  (query-one! db-info
    {:select [:*]
     :from   :query_state
     :where  [:and
              [:= :query_soul_id query-soul-id-s]
              [:= :version
               {:select [[[:max :version]]]
                :from   :query_state
                :where  [:= :query_soul_id query-soul-id-s]}]]}))

(defn db-retry-query!
  "Create a new query_state (version N+1) for an existing query_soul.
   Used when re-running a query with a different provider/model or settings.
   Returns the new query-state UUID."
  [db-info query-soul-id {:keys [status provider model]}]
  (when (ds db-info)
    (let [soul-id-s (->ref query-soul-id)
          current   (latest-query-state db-info soul-id-s)
          new-id    (UUID/randomUUID)
          now       (now-ms)]
      (when current
        (execute! db-info
          {:insert-into :query_state
           :values [(cond-> {:id                         (str new-id)
                             :query_soul_id              soul-id-s
                             :forked_from_query_state_id (:id current)
                             :version                    (inc (:version current))
                             :status                     (normalize-status (or status :running))
                             :llm_root_model             model
                             :created_at                 now}
                      provider (assoc :llm_root_provider (name (->kw provider))))]})
        new-id))))

(defn db-update-query!
  "Update the latest query_state with final outcome.

   When `:prior-outcome` is provided (one of `:complete`,
   `:abandoned`, `:cancelled`, `:error`), it lands in the dedicated
   `prior_outcome` column so the next turn's handover digest can read
   it without scanning every iteration. The column is bounded by a
   CHECK constraint at the schema level."
  [db-info query-id {:keys [answer iteration-count duration-ms
                            status tokens cost prior-outcome]}]
  (when (and (ds db-info) query-id)
    (let [soul-id-s (->ref query-id)
          state     (latest-query-state db-info soul-id-s)]
      (when state
        (execute! db-info
          {:update :query_state
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
           :where  [:= :id (:id state)]})))))

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
     (nil? v)               nil
     (zero? depth)          {:vis/ref :depth-exceeded}
     (runtime-object? v)    {:vis/ref :expr}
     ;; Lazy seq — not yet realized. Don't realize it, just ref.
     (instance? clojure.lang.LazySeq v) {:vis/ref :expr}
     ;; Realized collections — walk recursively
     (map? v)    (persistent! (reduce-kv (fn [m k val] (assoc! m k (freeze-safe val (dec depth)))) (transient {}) v))
     (vector? v) (mapv #(freeze-safe % (dec depth)) v)
     (set? v)    (into #{} (map #(freeze-safe % (dec depth))) v)
     (list? v)   (doall (map #(freeze-safe % (dec depth)) v))
     ;; Other seqs (cons, range, iterate, etc.) — also lazy by nature
     (seq? v)    {:vis/ref :expr}
     :else       v)))

;; =============================================================================
;; Iteration — iteration table
;; =============================================================================

(defn- prepare-blocks-blob
  "Encode the per-iteration code-block log as one Nippy-frozen vec
   suitable for `iteration.blocks BLOB`. Each map carries the same
   shape every read path expects from `db-list-iteration-blocks`,
   so the legacy per-call-row contract round-trips through the blob:

     {:idx           0-based block index
      :code          \"(some-code)\"
      :comment       leading `;; … / #_(...)` block, absent when blank
      :result        deep-frozen value, absent on error
      :error         string error message, absent on success
      :stdout/:stderr  non-blank captured streams, absent otherwise
      :duration-ms   N
      :timeout?      true | absent
      :repaired?     true | absent}"
  [blocks]
  (let [blank? (fn [s] (or (nil? s) (and (string? s) (str/blank? s))))]
    (->> (or blocks [])
      (map-indexed
        (fn [pos exec]
          (cond-> {:idx pos
                   :code (:code exec)}
            (some? (:comment exec))            (assoc :comment (:comment exec))
            (some? (:result exec))             (assoc :result (freeze-safe (:result exec)))
            (some? (:error exec))              (assoc :error  (str (:error exec)))
            (not (blank? (:stdout exec)))      (assoc :stdout (:stdout exec))
            (not (blank? (:stderr exec)))      (assoc :stderr (:stderr exec))
            (some? (:execution-time-ms exec))  (assoc :duration-ms (:execution-time-ms exec))
            (:timeout? exec)                   (assoc :timeout? true)
            (:repaired? exec)                  (assoc :repaired? true))))
      vec)))

(defn db-store-iteration!
  "Store one iteration row + per-`(def …)` expression_soul/expression_state
   rows. The iteration's full code-block log is written inline as a
   Nippy blob in `iteration.blocks` (no per-call rows; see V1 schema
   migration banner). Returns the iteration UUID."
  [db-info {:keys [query-id blocks thinking answer answer-form-idx duration-ms vars error metadata
                   llm-messages llm-provider llm-model]}]
  (when (ds db-info)
    (let [iteration-id   (UUID/randomUUID)
          iteration-id-s (str iteration-id)
          now       (now-ms)
          query-soul-id-s (when query-id (->ref query-id))
          ;; Need query_state_id (iteration FK points to query_state)
          query-state (when query-soul-id-s
                        (latest-query-state db-info query-soul-id-s))
          query-state-id-s (:id query-state)
          ;; Need conversation_state_id for expression_soul
          conversation-state-id (when query-state
                                  (:conversation_state_id
                                   (query-one! db-info
                                     {:select [:conversation_state_id]
                                      :from   :query_soul
                                      :where  [:= :id query-soul-id-s]})))
          ;; Compute position (0-indexed within this query_state)
          ;; Next position is `MAX(position)+1` (monotonic and survives
          ;; row deletions), aliased as `:next_position` so the SQL
          ;; column name and the Clojure key line up. HoneySQL renders
          ;; `:row-count` as the SQL identifier `row_count`, and
          ;; `as-unqualified-lower-maps` returns `:row_count` in the
          ;; row map; reading via `:row-count` (with hyphen) was always
          ;; `nil` and pinned every iteration to position 0, which
          ;; collided with the `UNIQUE (query_state_id, position)`
          ;; constraint on the second iteration of every query.
          position  (or (:next_position
                         (query-one! db-info
                           {:select [[[:coalesce [:+ [:max :position] 1] 0]
                                      :next_position]]
                            :from   :iteration
                            :where  [:= :query_state_id query-state-id-s]}))
                      0)]
      ;; 1. Iteration row — includes the full block log inline as
      ;;    `iteration.blocks BLOB` (Nippy-encoded vec). Replaces
      ;;    the legacy expression_soul kind='call' + expression_state
      ;;    fanout: every reader iterated per-iteration anyway.
      (let [blocks-vec (prepare-blocks-blob blocks)]
        (execute! db-info
          {:insert-into :iteration
           :values [(cond-> {:id                   iteration-id-s
                             :query_state_id       query-state-id-s
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
                             :created_at           now
                             :finished_at          now}
                      (some? answer-form-idx)
                      (assoc :answer_form_idx answer-form-idx))]}))
      ;; 3. Vars → expression_soul (kind=var, stateful) + expression_state (versioned)
      (when conversation-state-id
        (doseq [{:keys [name value code time-ms metadata]} (or vars [])]
          (when name
            (let [name-s (str name)
                  ;; Find-or-create: partial unique index can't use ON CONFLICT
                  existing (:id (query-one! db-info
                                  {:select [:id]
                                   :from   :expression_soul
                                   :where  [:and
                                            [:= :conversation_state_id conversation-state-id]
                                            [:= :name name-s]]}))
                  soul-id (or existing
                            (let [new-id (str (UUID/randomUUID))]
                              (execute! db-info
                                {:insert-into :expression_soul
                                 :values [{:id                    new-id
                                           :conversation_state_id conversation-state-id
                                           :kind                  "var"
                                           :state_mode            "stateful"
                                           :name                  name-s
                                           :created_at            now}]})
                              new-id))
                  max-ver (or (:v (query-one! db-info
                                    {:select [[[:max :version] :v]]
                                     :from   :expression_state
                                     :where  [:= :expression_soul_id soul-id]}))
                            -1)]
              (execute! db-info
                {:insert-into :expression_state
                 :values [{:id                 (str (UUID/randomUUID))
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
      iteration-id)))

;; =============================================================================
;; Read helpers
;; =============================================================================

(defn- row->query [row]
  (let [state-meta (<-json (:state_metadata row))
        ;; `:provider` may live in either the JSON metadata or the
        ;; dedicated `llm_root_provider` column. The column is the
        ;; canonical source (typed, indexable); the JSON copy is a
        ;; convenience for older readers. Prefer the column, fall
        ;; back to the JSON, and always re-keywordize.
        provider   (or (:llm_root_provider row) (:provider state-meta))]
    (cond-> {:id                    (->uuid (:soul_id row))
             :type                  :query
             :conversation-state-id (->uuid (:conversation_state_id row))
             :text                  (:query row)
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

(defn- query-soul+state-query
  "HoneySQL fragment joining query_soul + latest query_state."
  [where-clause]
  {:select [:qs.id :qs.conversation_state_id :qs.title :qs.query
            [:qs.created_at :soul_created_at] [:qs.id :soul_id]
            :qst.status :qst.metadata [:qst.metadata :state_metadata]
            :qst.llm_root_provider :qst.llm_root_model]
   :from   [[:query_soul :qs]]
   :join   [[:query_state :qst] [:= :qst.query_soul_id :qs.id]]
   :where  [:and
            where-clause
            [:= :qst.version
             {:select [[[:max :version]]]
              :from   [[:query_state :qst2]]
              :where  [:= :qst2.query_soul_id :qs.id]}]]})

(defn db-list-queries-by-status [db-info status]
  (if (ds db-info)
    (mapv row->query
      (query! db-info (query-soul+state-query [:= :qst.status (normalize-status status)])))
    []))

(defn- attach-prior-outcome [row->qmap]
  ;; Surface :prior-outcome on the query map when the column has a value.
  ;; NULL columns surface as an absent key on the returned map.
  (fn [row]
    (cond-> (row->qmap row)
      (:prior_outcome row) (assoc :prior-outcome (keyword (:prior_outcome row))))))

(defn db-list-conversation-queries [db-info conversation-id]
  (if (and (ds db-info) conversation-id)
    (let [state-id-s (latest-state-id db-info conversation-id)]
      (when state-id-s
        (mapv (attach-prior-outcome row->query)
          (query! db-info
            (-> (query-soul+state-query [:= :qs.conversation_state_id state-id-s])
              (update :select conj :qst.prior_outcome)
              (assoc :order-by [[:qs.created_at :asc]]))))))
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
    ;; Iteration metadata (JSON) carries the per-iteration metrics:
    ;; :var-history-recall-count, :expression-redundancy-fraction,
    ;; :dedup-saves, plus per-iteration extension info.
    (some? (:metadata row))             (assoc :metadata (<-json (:metadata row)))))

(defn db-list-query-iterations [db-info query-id]
  (if (and (ds db-info) query-id)
    (let [soul-id-s (->ref query-id)
          state     (latest-query-state db-info soul-id-s)]
      (when state
        (mapv row->iteration
          (query! db-info
            {:select [:*] :from :iteration
             :where [:= :query_state_id (:id state)]
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

   Source: the Nippy-encoded `iteration.blocks` BLOB. Replaces the
   legacy expression_soul kind='call' + expression_state row join.
   Same shape every legacy reader expects, so call sites round-trip
   through the blob without changes."
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
                    :query-id   (->uuid (:query_soul_id r))
                    :created-at (->date (:created_at r))}]))
           (query! db-info
             {:select [:es.name :est.result :est.expr :est.version :est.created_at
                       :qst.query_soul_id]
              :from   [[:expression_soul :es]]
              :join   [[:expression_state :est] [:= :est.expression_soul_id :es.id]
                       [:iteration :it]         [:= :it.id :est.iteration_id]
                       [:query_state :qst]      [:= :qst.id :it.query_state_id]]
              :where  [:and
                       [:= :es.conversation_state_id state-id-s]
                       [:= :es.kind "var"]
                       [:= :est.version
                        {:select [[[:max :version]]]
                         :from   [[:expression_state :est2]]
                         :where  [:= :est2.expression_soul_id :es.id]}]]}))))
     {})))

(defn db-var-history [db-info conversation-id var-sym]
  (if (and (ds db-info) conversation-id)
    (let [state-id-s (latest-state-id db-info conversation-id)]
      (when state-id-s
        (mapv (fn [r]
                {:version    (:version r)
                 :value      (<-blob (:result r))
                 :code       (:expr r)
                 :created-at (->date (:created_at r))})
          (query! db-info
            {:select [:est.version :est.result :est.expr :est.created_at]
             :from   [[:expression_state :est]]
             :join   [[:expression_soul :es] [:= :est.expression_soul_id :es.id]]
             :where  [:and
                      [:= :es.conversation_state_id state-id-s]
                      [:= :es.kind "var"]
                      [:= :es.name (str var-sym)]]
             :order-by [[:est.version :asc]]}))))
    []))

(defn db-query-history [db-info conversation-id]
  (let [queries (db-list-conversation-queries db-info conversation-id)]
    (mapv (fn [idx query]
            (let [qref       (:id query)
                  iteration-count (count (db-list-query-iterations db-info qref))
                  answer-raw (or (:answer query) "")
                  answer-preview (subs answer-raw 0 (min (count answer-raw) 160))]
              {:query-pos       idx
               :query-id        (:id query)
               :created-at      (:created-at query)
               :query           (:text query)
               :status          (:status query)
               :iteration-count iteration-count
               :answer-preview  answer-preview}))
      (range)
      queries)))

;; =============================================================================
;; Expression dependencies
;; =============================================================================

(defn db-store-dependency!
  "Store an edge: downstream depends on upstream.
   Both must be expression_souls in the same conversation_state."
  [db-info {:keys [conversation-state-id downstream-soul-id upstream-soul-id]}]
  (when (ds db-info)
    (let [id (str (UUID/randomUUID))]
      (execute! db-info
        {:insert-into :expression_dependency
         :values [{:id                            id
                   :conversation_state_id         (->id conversation-state-id)
                   :downstream_expression_soul_id (->id downstream-soul-id)
                   :upstream_expression_soul_id   (->id upstream-soul-id)
                   :created_at                    (now-ms)}]})
      id)))

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

(sdk/register-extension!
  (sdk/extension
    {:ext/namespace 'com.blockether.vis.ext.persistance-sqlite.core
     :ext/doc       "SQLite + Flyway persistence backend."
     :ext/version   "0.3.0"
     :ext/author    "Blockether"
     :ext/license   "Apache-2.0"
     :ext/persistance [{:persistance/id :sqlite
                        :persistance/ns 'com.blockether.vis.ext.persistance-sqlite.core}]}))
