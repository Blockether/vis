(ns com.blockether.vis.loop.storage.sqlite.core
  "SQLite store — infrastructure + generic entity plumbing.

   Holds type coercion helpers, Flyway-backed schema install, connection
   lifecycle, shared low-level query helpers, the per-type ext-table
   projection, and the generic store/update entrypoints that the domain
   namespaces (conversations, corpus, git, vitality, concept-graph,
   search) build on top of.

   Connection lifecycle:
     (open-store db-spec)   → {:datasource ds :path ... :owned? bool :mode ...}
     (close-store store)    → idempotent dispose

   db-spec forms:
     nil              — no DB (returns nil)
     :temp            — ephemeral file under java.io.tmpdir, deleted on close
     \"path/to.db\"   — persistent SQLite file
     {:path \"...\"}  — persistent SQLite file
     {:datasource ds} — caller-owned DataSource (NOT closed on dispose)

   PRAGMAs applied per-connection:
     journal_mode=WAL, synchronous=NORMAL, foreign_keys=ON, busy_timeout=30000."
  (:require
   [babashka.fs :as fs]
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
;; Schema install — delegated to Flyway (see resources/db/migration/V*.sql)
;; =============================================================================

(defn install-schema!
  "Runs Flyway migrations under `resources/db/migration/` against `ds`.

   Every migration (V1__*.sql, V2__*.sql, ...) is applied exactly once;
   Flyway tracks state in `flyway_schema_history` inside the same DB.
   Returns the datasource so callers can keep their existing chain."
  [^DataSource ds]
  (let [flyway (-> (org.flywaydb.core.Flyway/configure)
                 (.dataSource ds)
                 (.locations (into-array String ["classpath:db/migration"]))
                 (.baselineOnMigrate true)
                 (.baselineVersion "0")
                 ;; SQLite FTS5 virtual tables + triggers mix transactional
                 ;; and non-transactional statements in the same script;
                 ;; flip :mixed on so Flyway runs them all.
                 (.mixed true)
                 (.load))]
    (.migrate flyway))
  ds)

;; =============================================================================
;; Connection management
;; =============================================================================

(defn- pooled-datasource
  "Builds an SQLiteConnectionPoolDataSource pre-configured with the standard
   PRAGMAs (WAL, synchronous=NORMAL, foreign_keys=ON, busy_timeout=30000)."
  ^DataSource [^String url]
  (let [cfg (doto (SQLiteConfig.)
              (.setJournalMode SQLiteConfig$JournalMode/WAL)
              (.setSynchronous SQLiteConfig$SynchronousMode/NORMAL)
              (.enforceForeignKeys true)
              (.setBusyTimeout 30000))
        ds  (SQLiteConnectionPoolDataSource. cfg)]
    (.setUrl ds url)
    ds))

(defn- url-for-path [^String path] (str "jdbc:sqlite:" path))

(def ^:private DB_FILENAME "rlm.db")

(defn- open-at-dir
  "Open a pooled SQLite store rooted at directory `dir`. The SQLite file
   lives at <dir>/rlm.db; `:path` stays the directory so siblings
   (qa-manifest.edn, trajectory EDN, etc.) have a stable home next to it."
  [^String dir]
  (.mkdirs (java.io.File. dir))
  (let [file (str dir "/" DB_FILENAME)
        ds   (pooled-datasource (url-for-path file))]
    (install-schema! ds)
    {:datasource ds :conn ds :path dir :db-file file}))

(defn open-store
  "Opens or wraps a SQLite store for RLM. See ns docstring for db-spec forms.

   `:path` in the returned map is the *directory* that contains the SQLite
   file so callers can drop qa-manifest.edn and trajectory EDN next to it.

   Returns nil when db-spec is nil. Otherwise returns:
     {:datasource ds :conn ds :path \"/dir\" :db-file \"/dir/rlm.db\"
      :owned? bool :mode :temp|:persistent|:external}"
  [db-spec]
  (cond
    (nil? db-spec)
    nil

    (= :temp db-spec)
    (let [dir (str (System/getProperty "java.io.tmpdir") "/rlm-sqlite-" (util/uuid))]
      (assoc (open-at-dir dir) :owned? true :mode :temp))

    (string? db-spec)
    (assoc (open-at-dir db-spec) :owned? false :mode :persistent)

    (map? db-spec)
    (cond
      (or (:datasource db-spec) (:conn db-spec))
      (let [ds (or (:datasource db-spec) (:conn db-spec))]
        (install-schema! ds)
        {:datasource ds :conn ds :path nil :db-file nil
         :owned? false :mode :external})

      (:path db-spec)
      (assoc (open-at-dir (:path db-spec)) :owned? false :mode :persistent)

      :else
      (throw (ex-info "Invalid db-spec map — expected :datasource or :path"
               {:type :rlm/invalid-db-spec :db-spec db-spec})))

    :else
    (throw (ex-info "Invalid db-spec — expected nil, :temp, path string, or {:datasource ...}/{:path ...}"
             {:type :rlm/invalid-db-spec :db-spec db-spec}))))

(defn close-store
  "Releases resources for an RLM store. Idempotent.

   For :temp mode the whole store directory is wiped (DB file + WAL/SHM
   sidecars + any adjacent manifests). External datasources are NOT closed.
   Pooled SQLite datasources are GC'd; explicit close not required."
  [store]
  (when store
    (let [{:keys [path mode owned?]} store]
      (when (and (= :temp mode) owned? path (fs/exists? path))
        (try
          (fs/delete-tree path)
          (catch Exception e
            (trove/log! {:level :warn :data {:error (ex-message e)}
                         :msg "Failed to delete temp SQLite store"})))))))

;; =============================================================================
;; Low-level query helpers (shared across domain namespaces)
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

(defn entity-ref->id
  "Lookup ref [:id uuid] → string TEXT id. Tolerant of bare UUID/string."
  [ref]
  (cond
    (nil? ref) nil
    (and (vector? ref) (= :id (first ref))) (->id (second ref))
    (uuid? ref) (->id ref)
    (string? ref) ref
    :else nil))

(defn id->entity-ref
  "string TEXT id → [:id uuid] lookup ref."
  [id]
  (when id [:id (->uuid id)]))

;; =============================================================================
;; Type-keyword ↔ extension-table mapping
;; =============================================================================

(def ^:private TYPE->EXT-TABLE
  "Maps :type values (keyword) to their extension attrs table.
   Types not in this map have no extension table — only entity row exists."
  {:conversation   :conversation_attrs
   :query          :query_attrs
   :iteration      :iteration_attrs
   :iteration-var  :iteration_var_attrs
   :event          :commit_attrs       ;; git commit
   :repo           :repo_attrs
   :person         :person_attrs})

;; Per-type column lists (used for projection on read; SELECT *)

(def ^:private CONVERSATION-COLS [:system_prompt :model])
(def ^:private QUERY-COLS        [:messages :text :answer :iterations :duration_ms :status :eval_score
                                  :model :input_tokens :output_tokens :reasoning_tokens :cached_tokens :total_cost])
(def ^:private ITERATION-COLS    [:code :results :vars :answer :thinking :error :duration_ms])
(def ^:private ITER-VAR-COLS     [:name :value :code])
(def ^:private REPO-COLS         [:name :path :head_sha :head_short :branch :commits_ingested :ingested_at])
(def ^:private COMMIT-COLS       [:category :sha :date :prefix :scope :author_email])
(def ^:private PERSON-COLS       [:email])

(defn- ext-cols-for [type-kw]
  (case type-kw
    :conversation  CONVERSATION-COLS
    :query         QUERY-COLS
    :iteration     ITERATION-COLS
    :iteration-var ITER-VAR-COLS
    :repo          REPO-COLS
    :event         COMMIT-COLS
    :person        PERSON-COLS
    nil))

;; =============================================================================
;; Row → entity map projection
;; =============================================================================

(defn entity-base
  "Project the core entity columns from a flat row into namespaced keys."
  [row]
  (let [type-kw (->kw-back (:type row))]
    (cond-> {:id          (->uuid (:id row))
             :type        type-kw}
      (some? (:name row))         (assoc :name (:name row))
      (some? (:description row))  (assoc :description (:description row))
      (some? (:parent_id row))    (assoc :parent-id (->uuid (:parent_id row)))
      (some? (:document_id row))  (assoc :document-id (:document_id row))
      (some? (:page row))         (assoc :page (:page row))
      (some? (:section row))      (assoc :section (:section row))
      (some? (:created_at row))   (assoc :created-at (->date (:created_at row)))
      (some? (:updated_at row))   (assoc :updated-at (->date (:updated_at row))))))

(defn- conversation-attrs->ns
  [row]
  (cond-> {}
    (some? (:system_prompt row)) (assoc :system-prompt (:system_prompt row))
    (some? (:model row))         (assoc :model (:model row))))

(defn- query-attrs->ns
  [row]
  (cond-> {}
    (some? (:messages row))         (assoc :messages (:messages row))
    (some? (:text row))             (assoc :text (:text row))
    (some? (:answer row))           (assoc :answer (:answer row))
    (some? (:iterations row))       (assoc :iterations (:iterations row))
    (some? (:duration_ms row))      (assoc :duration-ms (:duration_ms row))
    (some? (:status row))           (assoc :status (->kw-back (:status row)))
    (some? (:eval_score row))       (assoc :eval-score (float (:eval_score row)))
    (some? (:model row))            (assoc :model (:model row))
    (some? (:input_tokens row))     (assoc :input-tokens (:input_tokens row))
    (some? (:output_tokens row))    (assoc :output-tokens (:output_tokens row))
    (some? (:reasoning_tokens row)) (assoc :reasoning-tokens (:reasoning_tokens row))
    (some? (:cached_tokens row))    (assoc :cached-tokens (:cached_tokens row))
    (some? (:total_cost row))       (assoc :total-cost (:total_cost row))))

(defn- iteration-attrs->ns
  [row]
  (cond-> {}
    (some? (:code row))        (assoc :code (:code row))
    (some? (:results row))     (assoc :results (:results row))
    (some? (:vars row))        (assoc :vars (:vars row))
    (some? (:answer row))      (assoc :answer (:answer row))
    (some? (:thinking row))    (assoc :thinking (:thinking row))
    (some? (:error row))       (assoc :error (:error row))
    (some? (:duration_ms row)) (assoc :duration-ms (:duration_ms row))))

(defn- iteration-var-attrs->ns
  [row]
  (cond-> {}
    (some? (:name row))  (assoc :name (:name row))
    (some? (:value row)) (assoc :value (:value row))
    (some? (:code row))  (assoc :code (:code row))))

(defn- ext-attrs->ns [type-kw row]
  (case type-kw
    :conversation  (conversation-attrs->ns row)
    :query         (query-attrs->ns row)
    :iteration     (iteration-attrs->ns row)
    :iteration-var (iteration-var-attrs->ns row)
    {}))

(defn fetch-entity
  "Pull a single entity by string TEXT id, joined with its per-type ext attrs.
   Returns the legacy namespaced map or nil. Two queries (entity, then ext)
   to avoid SELECT * ambiguity across joined tables."
  [db-info entity-id]
  (when entity-id
    (when-let [base (query-one! db-info
                      {:select [:*] :from :entity
                       :where [:= :id entity-id]})]
      (let [type-kw (->kw-back (:type base))
            ext-tbl (TYPE->EXT-TABLE type-kw)
            ext-row (when ext-tbl
                      (query-one! db-info
                        {:select (ext-cols-for type-kw) :from ext-tbl
                         :where [:= :entity_id entity-id]}))]
        (merge (entity-base base)
          (when ext-row (ext-attrs->ns type-kw ext-row)))))))

(defn fetch-entities
  "Pull multiple entities by string TEXT ids, joined with per-type ext attrs.
   Single query per type — used to materialize lists efficiently.

   Preserves the order of `entity-ids` in the returned vector. Callers
   (e.g. `db-list-conversation-queries`) pre-sort ids by `created_at` and
   rely on this function to not scramble that order. SQLite's
   `WHERE id IN (...)` returns rows in undefined order (usually rowid,
   which is NOT insertion order for TEXT-id tables after VACUUM / multi-
   threaded inserts), which used to manifest as conversations replaying
   with turn 2 before turn 1 after a web restart — i.e. the previously
   correct message history got reshuffled, then re-persisted into the
   next turn's `query_attrs.messages` permanently."
  [db-info entity-ids]
  (when (seq entity-ids)
    (let [bases (query! db-info
                  {:select [:*] :from :entity
                   :where [:in :id entity-ids]})
          by-type (group-by #(->kw-back (:type %)) bases)
          ;; Materialize each row with its merged ext-attrs, keyed by id.
          id->materialized
          (into {}
            (mapcat (fn [[type-kw rows]]
                      (let [ids (mapv :id rows)
                            ext-tbl (TYPE->EXT-TABLE type-kw)
                            ext-rows (when (and ext-tbl (seq ids))
                                       (query! db-info
                                         {:select (cons :entity_id (ext-cols-for type-kw))
                                          :from ext-tbl
                                          :where [:in :entity_id ids]}))
                            ext-by-id (into {} (map (fn [r] [(:entity_id r) r])) ext-rows)]
                        (map (fn [base]
                               [(:id base)
                                (merge (entity-base base)
                                  (when-let [ext (get ext-by-id (:id base))]
                                    (ext-attrs->ns type-kw ext)))])
                          rows))))
            by-type)]
      ;; Walk `entity-ids` in the caller-supplied order so the output
      ;; matches the ORDER BY from the calling query.
      (into [] (keep id->materialized) entity-ids))))

;; =============================================================================
;; Entity store / update — generic
;; =============================================================================

(defn- split-entity-attrs
  "Splits attrs map
   into {:entity-cols {col v} :ext-cols {col v}}.
   Drops keys that don't match any known column."
  [attrs]
  (let [type-kw (some-> attrs :type)
        entity-cols (cond-> {}
                      (:id attrs)           (assoc :id (->id (:id attrs)))
                      (:type attrs)         (assoc :type (->kw (:type attrs)))
                      (:name attrs)         (assoc :name (:name attrs))
                      (:description attrs)  (assoc :description (:description attrs))
                      (:parent-id attrs)    (assoc :parent_id (->id (:parent-id attrs)))
                      (:document-id attrs)  (assoc :document_id (:document-id attrs))
                      (some? (:page attrs)) (assoc :page (:page attrs))
                      (:section attrs)      (assoc :section (:section attrs))
                      (:created-at attrs)   (assoc :created_at (->epoch-ms (:created-at attrs)))
                      (:updated-at attrs)   (assoc :updated_at (->epoch-ms (:updated-at attrs))))
        ext-cols (case type-kw
                   :conversation (cond-> {}
                                   (:system-prompt attrs) (assoc :system_prompt (:system-prompt attrs))
                                   (:model attrs)         (assoc :model (:model attrs)))
                   :query (cond-> {}
                            (:messages attrs)    (assoc :messages (:messages attrs))
                            (:text attrs)        (assoc :text (:text attrs))
                            (:answer attrs)      (assoc :answer (:answer attrs))
                            (:iterations attrs)  (assoc :iterations (:iterations attrs))
                            (:duration-ms attrs) (assoc :duration_ms (:duration-ms attrs))
                            (:status attrs)      (assoc :status (->kw (:status attrs)))
                            (:eval-score attrs)  (assoc :eval_score (double (:eval-score attrs))))
                   :iteration (cond-> {}
                                (:code attrs)        (assoc :code (:code attrs))
                                (:results attrs)     (assoc :results (:results attrs))
                                (:vars attrs)        (assoc :vars (:vars attrs))
                                (:answer attrs)      (assoc :answer (:answer attrs))
                                (:thinking attrs)    (assoc :thinking (:thinking attrs))
                                (:duration-ms attrs) (assoc :duration_ms (:duration-ms attrs)))
                   :iteration-var (cond-> {}
                                    (:name attrs)  (assoc :name (:name attrs))
                                    (:value attrs) (assoc :value (:value attrs))
                                    (:code attrs)  (assoc :code (:code attrs)))
                   :person (cond-> {}
                             (:email attrs) (assoc :email (:email attrs)))
                   {})]
    {:entity-cols entity-cols
     :ext-cols ext-cols
     :type type-kw}))

(defn upsert-entity-row!
  "Insert-or-replace an entity row. Returns the string id."
  [db-info entity-cols]
  (let [{:keys [id]} entity-cols]
    (jdbc/execute! (ds db-info)
      (sql/format
        {:insert-into :entity
         :values [entity-cols]
         :on-conflict [:id]
         :do-update-set (vec (remove #{:id} (keys entity-cols)))}))
    id))

(defn- upsert-ext-row!
  "Insert-or-replace a per-type ext row. No-op when ext-cols is empty."
  [db-info type-kw entity-id ext-cols]
  (when-let [tbl (and (seq ext-cols) (TYPE->EXT-TABLE type-kw))]
    (let [pk-col :entity_id
          row (assoc ext-cols pk-col entity-id)]
      (jdbc/execute! (ds db-info)
        (sql/format
          {:insert-into tbl
           :values [row]
           :on-conflict [pk-col]
           :do-update-set (vec (remove #{pk-col} (keys row)))})))))

(defn store-entity!
  "Stores an entity. Mirrors db.clj/store-entity!.

   Generates an :id UUID if absent. Stamps :created-at and
   :updated-at when missing. Returns the lookup ref [:id uuid].

   When db-info has no :datasource (legacy nil-db case), returns nil and
   performs no work — matches the old behavior."
  [db-info attrs]
  (when (ds db-info)
    (let [id (or (:id attrs) (UUID/randomUUID))
          now (Date.)
          attrs+ (cond-> (assoc attrs
                           :id id
                           :type (or (:type attrs)
                                   (throw (ex-info "store-entity! requires :type"
                                            {:type :rlm/missing-entity-type :attrs attrs}))))
                   (not (:created-at attrs)) (assoc :created-at now)
                   (not (:updated-at attrs)) (assoc :updated-at now))
          {:keys [entity-cols ext-cols type]} (split-entity-attrs attrs+)]
      (jdbc/with-transaction [tx (ds db-info)]
        (let [tx-info {:datasource tx}]
          (upsert-entity-row! tx-info entity-cols)
          (upsert-ext-row!    tx-info type (->id id) ext-cols)))
      [:id id])))

(defn update-entity!
  "Merges attrs onto an existing entity by lookup ref. Stamps :updated-at."
  [db-info entity-ref attrs]
  (when (ds db-info)
    (let [id (entity-ref->id entity-ref)
          base-row (when id (query-one! db-info {:select [:type] :from :entity :where [:= :id id]}))
          existing-type (->kw-back (:type base-row))
          merged (cond-> attrs
                   (and existing-type (not (:type attrs))) (assoc :type existing-type)
                   (not (:updated-at attrs))               (assoc :updated-at (Date.))
                   true                                           (assoc :id (->uuid id)))
          {:keys [entity-cols ext-cols type]} (split-entity-attrs merged)]
      (when id
        (jdbc/with-transaction [tx (ds db-info)]
          (let [tx-info {:datasource tx}]
            (when (seq entity-cols)
              (upsert-entity-row! tx-info entity-cols))
            (upsert-ext-row! tx-info (or type existing-type) id ext-cols)))))))

(defn delete-entity-tree!
  "Delete an entity and all descendants linked via parent_id.
   Walks the parent_id chain (conversation → queries → iterations → vars)
   and deletes all in one shot. Attr tables cascade via ON DELETE CASCADE."
  [db-info entity-id]
  (when (and (ds db-info) entity-id)
    (let [id (str entity-id)
          ;; Collect all descendant IDs via iterative BFS
          all-ids (loop [queue [id] acc []]
                    (if (empty? queue)
                      acc
                      (let [children (->> (query! db-info
                                            {:select [:id]
                                             :from :entity
                                             :where [:in :parent_id queue]})
                                       (mapv :id))]
                        (recur children (into acc queue)))))]
      (when (seq all-ids)
        ;; Delete in batches — children's attr tables cascade automatically
        (doseq [batch (partition-all 100 all-ids)]
          (jdbc/execute! (ds db-info)
            (sql/format {:delete-from :entity
                         :where [:in :id batch]})))))))
