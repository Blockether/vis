(ns com.blockether.vis.internal.persistance
  "Persistence facade: backend registry, connection lifecycle, and every
   delegated `store-*`/`db-*` fn.

   Extensions register backend adapters through `:ext/persistance`. The
   facade dispatches each delegated call by resolving the matching var on
   the chosen backend namespace (`(ns-resolve ns-sym 'db-store-iteration!)`
   etc.) and applying it to the original args. This keeps the facade
   dialect-agnostic - every migration runner / driver-specific oddity stays
   inside the backend adapter.

   Frontends still call `db-error->user-message` here, but the actual
   translation is offered by backend adapters. Same for store-staleness
   checks used by the process-wide shared connection."
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
  "Keyword/string -> TEXT, stripping the leading colon. Nil -> nil."
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

   `id`     - keyword identity, e.g. `:sqlite`.
   `ns-sym` - fully qualified namespace symbol that defines the backend
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
;; namespaces listed inside. Any loaded extension with `:ext/persistance`
;; entries lands in this registry as a side effect. The facade triggers a
;; scan on the first `db-create-connection!` so callers reach the registered backend
;; without wiring discovery themselves.
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
            "Multiple backends registered, pass {:backend ...} in db-spec. ")
          (when (empty? @backends)
            "`com.blockether.vis.ext.persistance-sqlite.core`?"))
        {:registered (vec (keys @backends))}))))

(defn- require-backend-ns!
  "Ensure the backend's heavyweight namespace has been loaded. Backends
   are typically registered through a lightweight `registrar` ns whose
   `:persistance/ns` points at the heavy ns; this fn does the deferred
   `(require ...)` so callers don't pay the load cost until they actually
   dispatch a backend call. `requiring-resolve`-style: silent no-op when
   the ns is already loaded."
  [bid ns-sym]
  (try
    (require ns-sym)
    (catch Throwable t
      (throw (ex-info (str "Backend " bid " (" ns-sym ") failed to load: "
                        (or (ex-message t) (str t)))
               {:backend bid :ns ns-sym}
               t)))))

(defn- resolve-impl
  "Resolve the var implementing `fn-name` on the chosen backend.
   Auto-loads the backend ns on first call so backends can ship as a
   tiny registrar + heavy implementation pair. Throws a useful error
   when the backend is missing the fn."
  [db-spec-or-store fn-name]
  (let [bid     (pick-backend-id db-spec-or-store)
        ns-sym  (get-in @backends [bid :ns])
        _       (when-not ns-sym
                  (throw (ex-info (str "Backend " bid " not registered")
                           {:backend bid :registered (vec (keys @backends))})))
        _       (require-backend-ns! bid ns-sym)
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

(defn- resolve-optional-impl
  "Resolve an optional backend var. Missing vars return nil; bad backend
   selection still throws because the store/spec itself is invalid.
   Auto-loads the backend ns the same way `resolve-impl` does."
  [db-spec-or-store fn-name]
  (let [bid    (pick-backend-id db-spec-or-store)
        ns-sym (get-in @backends [bid :ns])]
    (when-not ns-sym
      (throw (ex-info (str "Backend " bid " not registered")
               {:backend bid :registered (vec (keys @backends))})))
    (require-backend-ns! bid ns-sym)
    (some-> (ns-resolve ns-sym fn-name) deref)))

;; =============================================================================
;; Connection lifecycle
;; =============================================================================

(defn db-create-connection!
  "Open a persistence connection from `db-spec`.

   Common spec forms:
     nil              - no DB (returns nil)
     :memory          - in-memory ephemeral store (backend-defined)
     \"path/to.db\"   - file-backed store (backend-defined)
     {:backend :sqlite :path ...}     - explicit backend selection
     {:backend :sqlite :datasource ds} - caller-owned DataSource

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
;; Delegated API
;;
;; Every fn delegates to the selected backend adapter's var of the same
;; name. Keep this as a compact, readable index; add a new entry here only
;; after the matching fn lands in at least one backend.
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

;; --- Iteration lifecycle ---
(defn db-store-iteration!
  "Same delegating shape as the macro-defined fns, but with input
   validation kept here so every backend gets the same precondition
   guarantees for free."
  [db-info opts]
  (when-not (map? opts)
    (throw (ex-info "db-store-iteration! opts must be a map" {:got (type opts)})))
  (when-not (:conversation-turn-id opts)
    (throw (ex-info "db-store-iteration! requires :conversation-turn-id" {:opts (keys opts)})))
  ((deref (resolve-impl db-info 'db-store-iteration!)) db-info opts))

;; --- Logging ---
(defdelegate db-log! [db-info opts])

;; --- Conversation lifecycle ---
(defdelegate db-store-conversation! [db-info opts])
(defdelegate db-get-conversation [db-info ref])
(defdelegate db-resolve-conversation-id [db-info sel])
(defdelegate db-list-conversations [db-info channel])
(defdelegate db-find-conversation-by-external [db-info channel ext-id])
(defdelegate db-update-conversation-title! [db-info ref title])
(defdelegate db-delete-conversation-tree! [db-info id])
(defdelegate db-fork-conversation! [db-info conversation-id opts])
(defdelegate db-list-conversation-states [db-info conversation-id])
(defdelegate db-latest-conversation-state-id [db-info conversation-id])

;; --- Turn lifecycle ---
(defdelegate db-store-conversation-turn! [db-info opts])
(defdelegate db-update-conversation-turn! [db-info conversation-turn-id opts])
(defdelegate db-list-conversation-turns-by-status [db-info status])
(defdelegate db-list-conversation-turns [db-info conversation-ref])
(defdelegate db-retry-conversation-turn! [db-info conversation-turn-soul-id opts])
(defdelegate db-list-conversation-turn-states [db-info conversation-turn-id])
(defdelegate db-list-conversation-turn-iterations [db-info conversation-turn-ref])
(defdelegate db-list-iteration-vars [db-info iteration-ref])
(defdelegate db-list-iteration-blocks [db-info iteration-ref])

;; --- Full-text search ---
(defdelegate db-search [db-info query opts])

;; --- Var registry & history ---
(defn db-latest-var-registry
  ([db-info conversation-ref]      ((deref (resolve-impl db-info 'db-latest-var-registry)) db-info conversation-ref))
  ([db-info conversation-ref opts] ((deref (resolve-impl db-info 'db-latest-var-registry)) db-info conversation-ref opts)))

(defdelegate db-var-history-index [db-info conversation-ref opts])
(defdelegate db-var-history [db-info conversation-ref sym])
(defdelegate db-var-history-timeline [db-info conversation-ref opts])
(defdelegate db-turn-history [db-info conversation-ref])

;; --- Dependencies ---
(defdelegate db-store-dependency! [db-info opts])
(defdelegate db-list-dependencies [db-info conversation-state-id])

;; --- Restore ---
(defdelegate db-restore-blocks [db-info conversation-id])

;; --- Extension aggregate sidecars ---
(defdelegate db-create-extension-aggregate! [db-info opts])
(defdelegate db-put-extension-aggregate! [db-info opts])
(defdelegate db-get-extension-aggregate [db-info opts])
(defdelegate db-list-extension-aggregates [db-info opts])
(defdelegate db-delete-extension-aggregates! [db-info opts])
(defdelegate db-swap-extension-aggregate! [db-info opts f args])

;; =============================================================================
;; Error translation
;;
;; Frontends (TUI, CLI, Telegram) all surface persistence exceptions in
;; chat bubbles. The facade stays backend-agnostic: adapters may expose
;; `db-error->user-message`; the first non-empty translation wins.
;; =============================================================================

(defn- backend-error-translators
  []
  (try (manifest/scan-extensions!) (catch Throwable _ nil))
  (keep (fn [[_ {:keys [ns]}]]
          (some-> (ns-resolve ns 'db-error->user-message) deref))
    @backends))

(defn db-error->user-message
  "Translate a persistence exception into something a human can act on.
   Backend adapters own backend-specific recognition; unknown errors fall
   back to `(ex-message e)`."
  [^Throwable e]
  (or (some (fn [f]
              (try
                (when-let [message (f e)]
                  (when (seq (str message))
                    (str message)))
                (catch Throwable _ nil)))
        (backend-error-translators))
    (ex-message e)
    "Internal error"))

;; =============================================================================
;; Process-wide shared connection (singleton helper)
;;
;; vis runs every channel (TUI, CLI, Telegram) against one persistence
;; store per process. Owning the singleton here - instead of in any
;; particular frontend - keeps the DB lifecycle behind the persistence
;; facade. Backend adapters may expose `db-store-stale?` for
;; adapter-specific file/handle replacement detection.
;; =============================================================================

(defonce ^:private shared-conn (atom nil))

(defn- store-stale?
  [store db-spec]
  (boolean
    (when store
      (when-let [f (resolve-optional-impl store 'db-store-stale?)]
        (f store (normalize-spec db-spec))))))

(defn db-shared-connection!
  "Return the process-wide shared persistence connection for `db-spec`,
   opening it on first call and caching the handle for the lifetime of
   the JVM. Subsequent calls return the cached handle regardless of
   the `db-spec` argument - the singleton intentionally pins to the
   first spec it saw.

  Pair with `db-dispose-shared-connection!` on process shutdown."
  [db-spec]
  (or (when-let [cur @shared-conn]
        (when-not (store-stale? cur db-spec)
          cur))
    (swap! shared-conn
      (fn [cur]
        (if (and cur (not (store-stale? cur db-spec)))
          cur
          (do
            (when cur
              (try (db-dispose-connection! cur) (catch Exception _ nil)))
            (db-create-connection! db-spec)))))))

(defn db-dispose-shared-connection!
  "Close the shared connection if one is open. Idempotent."
  []
  (when-let [c @shared-conn]
    (try (db-dispose-connection! c) (catch Exception _ nil))
    (reset! shared-conn nil)))
