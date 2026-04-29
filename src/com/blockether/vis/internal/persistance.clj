(ns com.blockether.vis.internal.persistance
  "Persistence facade: backend registry, connection lifecycle, every
   delegated `store-*`/`db-*` fn, error translation, the process-wide
   shared connection, and the orphan-sweep that reconciles `:running`
   queries on process restart.

   Backends register themselves via `register-backend!` at namespace
   load. The facade dispatches each delegated call by resolving the
   matching var on the chosen backend's namespace
   (`(ns-resolve ns-sym 'db-store-iteration!)` etc.) and applying it to
   the original args. This keeps the facade dialect-agnostic — every
   migration runner / driver-specific oddity stays inside the
   backend.

   `db-error->user-message` lives here too. Frontends (TUI, CLI,
   Telegram) all surface persistence exceptions in chat bubbles; the
   raw JDBC text is meaningless without context, so the persistence
   layer owns the translation — not any one frontend, not the
   conversation runtime above it."
  (:require
   [com.blockether.vis.internal.manifest :as manifest])
  (:import
   (java.time Instant)
   (java.util Date UUID)))

;; =============================================================================
;; Storage base helpers
;; =============================================================================

(defn ds [db-info] (:datasource db-info))

(defn now-ms ^long [] (System/currentTimeMillis))

(defn ->id [v]
  (cond
    (nil? v) nil
    (uuid? v) (str v)
    (string? v) v
    :else (str v)))

(defn ->uuid ^UUID [v]
  (cond
    (nil? v) nil
    (uuid? v) v
    (string? v) (try (UUID/fromString v) (catch IllegalArgumentException _ nil))
    :else nil))

(defn ->ref
  "Normalize an entity reference to a string ID for SQL.
   Accepts: UUID, string, or nil. Returns string or nil.

   The ONLY way to extract a SQL-ready string from an entity
   reference -- pass the plain UUID or string directly."
  [v]
  (cond
    (nil? v)    nil
    (uuid? v)   (str v)
    (string? v) v
    :else       (str v)))

(defn ->kw
  "Keyword/string → TEXT, stripping the leading colon. Nil → nil."
  [v]
  (cond
    (nil? v) nil
    (keyword? v) (subs (str v) 1)
    :else (str v)))

(defn ->kw-back [v]
  (when (and v (not= "" v))
    (keyword v)))

(defn ->epoch-ms [v]
  (cond
    (nil? v) nil
    (instance? Date v) (.getTime ^Date v)
    (instance? Instant v) (.toEpochMilli ^Instant v)
    (number? v) (long v)
    :else nil))

(defn ->date ^Date [v]
  (when v (Date. (long v))))

;; =============================================================================
;; Backend registry
;; =============================================================================

(defonce ^:private backends
  ;; {:sqlite {:ns 'com.blockether.vis.ext.persistance-sqlite.core}}
  (atom {}))

(defn register-backend!
  "Register a persistence backend implementation.

   `id`     — keyword identity, e.g. `:sqlite`.
   `ns-sym` — fully qualified namespace symbol that defines the backend
              functions (`db-open!`, `db-close!`, `db-log!`, every
              `store-*`/`db-*` fn used by this facade). Vars are
              resolved lazily via `ns-resolve` so REPL redefinition
              just works.

   Idempotent on `id`. Returns `id`."
  [id ns-sym]
  (when-not (keyword? id)
    (throw (ex-info "Backend id must be a keyword" {:id id})))
  (when-not (symbol? ns-sym)
    (throw (ex-info "Backend ns-sym must be a symbol" {:ns-sym ns-sym})))
  (swap! backends assoc id {:ns ns-sym})
  id)

(defn deregister-backend! [id]
  (swap! backends dissoc id)
  nil)

(defn registered-backends
  "Map of registered backends keyed by id."
  []
  @backends)

;; ----------------------------------------------------------------------------
;; Auto-discovery
;;
;; There is no backend-specific scanner. The single source of truth
;; is `manifest/scan-extensions!`, which scans every
;; `META-INF/vis-extension/vis.edn` on the classpath and `require`s the
;; namespaces listed inside. Any namespace that calls
;; `(register-backend! ...)` lands in this registry as a side
;; effect. The facade triggers a scan on the first
;; `db-create-connection!` so callers don't have to wire it up
;; themselves.
;; ----------------------------------------------------------------------------

(defn- pick-backend-id
  "Decide which backend handles this call. Honors an explicit
   `:backend` key on the spec/store; otherwise falls back to the
   single registered backend; otherwise throws."
  [db-spec-or-store]
  (or (when (map? db-spec-or-store) (:backend db-spec-or-store))
    (when (= 1 (count @backends)) (first (keys @backends)))
    (throw
      (ex-info
        (str "No persistence backend selected. "
          (if (empty? @backends)
            "No backends registered. Did you forget to require "
            "Multiple backends registered, pass {:backend …} in db-spec. ")
          (when (empty? @backends)
            "`com.blockether.vis.ext.persistance-sqlite.core`?"))
        {:registered (vec (keys @backends))}))))

(defn- resolve-impl
  "Resolve the var implementing `fn-name` on the chosen backend.
   Throws a useful error when the backend is missing the fn."
  [db-spec-or-store fn-name]
  (let [bid     (pick-backend-id db-spec-or-store)
        ns-sym  (get-in @backends [bid :ns])
        _       (when-not ns-sym
                  (throw (ex-info (str "Backend " bid " not registered")
                           {:backend bid :registered (vec (keys @backends))})))
        v       (ns-resolve ns-sym fn-name)]
    (when-not v
      (throw (ex-info (str "Backend " bid " (" ns-sym ") does not implement '" fn-name "'")
               {:backend bid :ns ns-sym :fn fn-name})))
    v))

(defn- normalize-spec
  "Reshape explicit-sqlite nested forms into the canonical shape
   backends accept. `:memory` is the canonical shorthand for the
   ephemeral in-process DB."
  [db-spec]
  (cond
    (and (map? db-spec)
      (= :sqlite (:backend db-spec)))
    (cond
      (:datasource db-spec) {:datasource (:datasource db-spec) :backend :sqlite}
      (:conn db-spec)       {:conn (:conn db-spec) :backend :sqlite}
      (:path db-spec)       (assoc {:backend :sqlite} :path (:path db-spec))
      :else                 db-spec)
    :else db-spec))

;; =============================================================================
;; Connection lifecycle
;; =============================================================================

(defn db-create-connection!
  "Open a persistence connection from `db-spec`.

   Common spec forms:
     nil              — no DB (returns nil)
     :memory          — in-memory ephemeral store (backend-defined)
     \"path/to.db\"   — file-backed store (backend-defined)
     {:backend :sqlite :path …}     — explicit backend selection
     {:backend :sqlite :datasource ds} — caller-owned DataSource

   With a single registered backend, omitting `:backend` works and the
   facade tags the returned store map with the chosen backend so all
   subsequent facade calls dispatch correctly.

   Triggers `manifest/scan-extensions!` on first call so a freshly
   started JVM with no extensions yet required has a chance to load
   any backend-providing namespaces from the classpath."
  [db-spec]
  (manifest/scan-extensions!)
  (let [normalized (normalize-spec db-spec)
        bid        (pick-backend-id (if (map? normalized)
                                      normalized
                                      {:backend (pick-backend-id {})}))
        f          @(resolve-impl {:backend bid} 'db-open!)
        store      (f normalized)]
    (cond
      (nil? store) nil
      (map? store) (assoc store :backend bid)
      :else        store)))

(defn db-dispose-connection! [store]
  (when store
    (let [f @(resolve-impl store 'db-close!)]
      (f store))))

;; =============================================================================
;; Delegated API — every fn delegates to the selected backend's var of
;; the same name. Each defn is intentionally one-line to keep the
;; surface obvious; add a new entry here only after the matching fn
;; lands in at least one backend.
;; =============================================================================

(defmacro ^:private defdelegate
  "Define a facade fn whose body resolves the matching backend var
   (using the first arg as the dispatch value) and applies it to
   the original args."
  [sym arglist]
  (let [bsym (gensym "backend-fn")]
    `(defn ~sym ~arglist
       (let [~bsym @(resolve-impl ~(first arglist) (quote ~sym))]
         (~bsym ~@arglist)))))

;; --- Logging ---
(defdelegate db-log! [db-info opts])

;; --- Conversation lifecycle ---
(defdelegate db-store-conversation!              [db-info opts])
(defdelegate db-get-conversation              [db-info ref])
(defdelegate db-resolve-conversation-id       [db-info sel])
(defdelegate db-list-conversations            [db-info channel])
(defdelegate db-find-conversation-by-external [db-info channel ext-id])
(defdelegate db-update-conversation-title!    [db-info ref title])
(defdelegate db-delete-conversation-tree!        [db-info id])
(defdelegate db-fork-conversation!               [db-info conversation-id opts])
(defdelegate db-list-conversation-states         [db-info conversation-id])
(defdelegate db-latest-conversation-state-id     [db-info conversation-id])

;; --- Query lifecycle ---
(defdelegate db-store-query!                  [db-info opts])
(defdelegate db-update-query!                 [db-info query-id opts])
(defdelegate db-list-queries-by-status     [db-info status])
(defdelegate db-list-conversation-queries  [db-info conversation-ref])
(defdelegate db-retry-query!                  [db-info query-soul-id opts])
(defdelegate db-list-query-states             [db-info query-id])

;; --- Iteration lifecycle ---
(defn db-store-iteration!
  "Same delegating shape as the macro-defined fns, but with input
   validation kept here so every backend gets the same precondition
   guarantees for free."
  [db-info opts]
  (when-not (map? opts)
    (throw (ex-info "db-store-iteration! opts must be a map" {:got (type opts)})))
  (when-not (:query-id opts)
    (throw (ex-info "db-store-iteration! requires :query-id" {:opts (keys opts)})))
  ((deref (resolve-impl db-info 'db-store-iteration!)) db-info opts))

(defdelegate db-list-query-iterations     [db-info query-ref])
(defdelegate db-list-iteration-vars       [db-info iteration-ref])
(defdelegate db-list-iteration-blocks [db-info iteration-ref])

;; --- Var registry & history ---
(defn db-latest-var-registry
  ([db-info conversation-ref]      ((deref (resolve-impl db-info 'db-latest-var-registry)) db-info conversation-ref))
  ([db-info conversation-ref opts] ((deref (resolve-impl db-info 'db-latest-var-registry)) db-info conversation-ref opts)))

(defdelegate db-var-history    [db-info conversation-ref sym])
(defdelegate db-query-history  [db-info conversation-ref])

;; --- Dependencies ---
(defdelegate db-store-dependency!     [db-info opts])
(defdelegate db-list-dependencies  [db-info conversation-state-id])

;; --- Restore ---
(defdelegate db-restore-blocks [db-info conversation-id])

;; =============================================================================
;; Error translation
;;
;; Frontends (TUI, CLI, Telegram) all surface persistence exceptions in
;; chat bubbles. The raw JDBC text (e.g. `[SQLITE_CANTOPEN] unable to
;; open the database file`) is meaningless without context, so the
;; persistence layer owns the translation — not any one frontend, not
;; the conversation runtime above it. Detection is text-based on
;; purpose so this jar keeps zero compile-time dep on driver classes.
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
  "Translate an exception from the persistence layer into something a
   human reading a chat bubble can act on.

   For most exceptions we surface `(ex-message e)` verbatim — provider
   errors, validation issues, etc. are often self-explanatory. The one
   case we rewrite is `SQLITE_CANTOPEN`, because the raw message is
   meaningless without context: the underlying file at
   `~/.vis/vis.mdb/vis.db` was either deleted out from under the
   running JVM, or moved, or the process lost write permissions to the
   directory. Anyone hitting this on the chat surface needs to know
   what to inspect, not the JDBC error code."
  [^Throwable e]
  (cond
    (sqlite-cantopen-message? e)
    (let [home   (System/getProperty "user.home")
          dbpath (str home "/.vis/vis.mdb/vis.db")
          dbdir  (str home "/.vis/vis.mdb")
          dirf   (java.io.File. dbdir)
          filef  (java.io.File. dbpath)]
      (str "Vis database is unavailable. "
        "Expected file: " dbpath ". "
        (cond
          (not (.exists filef))
          "The file is missing — likely deleted while Vis was running. Restart Vis to recreate it."

          (not (.canWrite dirf))
          (str "The directory " dbdir " is not writable by this process.")

          :else
          "The handle was lost mid-session. Restart Vis to reconnect.")))

    :else
    (or (ex-message e) "Internal error")))

;; =============================================================================
;; Process-wide shared connection (singleton helper)
;;
;; vis runs every channel (TUI, CLI, Telegram) against ONE SQLite DB
;; per process. Owning the singleton here — instead of in any
;; particular frontend or in conversation/core — keeps the DB
;; lifecycle inside the persistence layer where it belongs and lets
;; multiple frontends share the handle without each maintaining its
;; own atom.
;; =============================================================================

(defonce ^:private shared-conn (atom nil))

(defn db-shared-connection!
  "Return the process-wide shared persistence connection for `db-spec`,
   opening it on first call and caching the handle for the lifetime of
   the JVM. Subsequent calls return the cached handle regardless of
   the `db-spec` argument — the singleton intentionally pins to the
   first spec it saw.

   Pair with `db-dispose-shared-connection!` on process shutdown."
  [db-spec]
  (or @shared-conn
    (swap! shared-conn (fn [cur] (or cur (db-create-connection! db-spec))))))

(defn db-dispose-shared-connection!
  "Close the shared connection if one is open. Idempotent."
  []
  (when-let [c @shared-conn]
    (try (db-dispose-connection! c) (catch Exception _ nil))
    (reset! shared-conn nil)))

;; =============================================================================
;; Orphan sweep (process-restart cleanup)
;; =============================================================================

(def ^:private ORPHAN_INTERRUPTED_ANSWER
  "Warning: Turn interrupted — the server was restarted before this answer could finalize. Re-send the message to retry.")

(defn db-sweep-orphaned-running-queries!
  "Mark every `:running` query as `:interrupted`. Run at process start
   to clean up queries that crashed or were killed mid-write so the
   next turn's handover digest renders the right outcome instead of
   guessing. Returns the number of queries swept."
  [db-info]
  (let [orphans (try (db-list-queries-by-status db-info :running)
                  (catch Exception _ []))]
    (doseq [{:keys [id iteration-count duration-ms]} orphans]
      (try
        (db-update-query! db-info id
          {:answer          ORPHAN_INTERRUPTED_ANSWER
           :iteration-count (or iteration-count 0)
           :duration-ms     (or duration-ms 0)
           :status          :interrupted
           :prior-outcome   :cancelled})
        (catch Exception _ nil)))
    (count orphans)))
