(ns com.blockether.vis-persistance.core
  "Persistence facade.

   This namespace is **backend-agnostic**. It defines the public API
   (every fn callers use) and dispatches to a registered backend via
   `register-backend!`. Backends live in their own jars
   (`vis-persistance-sqlite`, future `vis-persistance-postgres`, …)
   and call `register-backend!` at namespace load time.

   ── Calling convention ─────────────────────────────────────────────

       (require 'com.blockether.vis.ext.persistance-sqlite.core)  ;; loads + registers :sqlite
       (def store (persistance/create-store-connection \"vis.db\"))   ;; backend chosen automatically
       (persistance/log! store {:level :info :event \"hello\"})

   When several backends are registered, `db-spec` may carry an
   explicit `:backend` keyword to select one. With a single backend
   registered the facade picks it implicitly.

   ── Backend implementor contract ───────────────────────────────────

   A backend `register-backend!`s a namespace symbol whose vars match
   the names used by this facade (`open-store`, `close-store`,
   `log!`, `store-conversation!`, …). The facade calls them with
   `ns-resolve`, so a hot-reload of the backend ns is picked up
   automatically. Every facade fn that needs a connection gets the
   `store` map back as its first argument; the `:backend` key on
   `store` controls dispatch.

   ── Auto-discovery ───────────────────────────────────

   Backends are loaded the same way channels and other extensions are:
   ship the unified `META-INF/vis-extension/vis.edn` in your jar's resources/
   listing the namespaces that call `register-backend!`. The first
   `create-store-connection` call triggers a lazy discovery pass, memoized for
   the process lifetime."
  ;; Avoid a direct require cycle: `com.blockether.vis-extension.extension` needs
  ;; this registry for the `:ext/persistance` slot, while this facade
  ;; asks the extension loader to discover backend namespaces lazily.
  )

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
              functions (`open-store`, `close-store`, `log!`, every
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

(defn deregister-backend!
  [id]
  (swap! backends dissoc id)
  nil)

(defn registered-backends
  "Map of registered backends keyed by id."
  []
  @backends)

;; ----------------------------------------------------------------------------
;; Auto-discovery
;;
;; There is no backend-specific scanner. The single source of truth is
;; `com.blockether.vis-extension.extension/discover-extensions!`, which scans every
;; `META-INF/vis-extension/vis.edn` on the classpath and `require`s the namespaces
;; listed inside. Any of those namespaces that calls
;; `(register-backend! ...)` lands in this backend registry as a side
;; effect.
;;
;; This namespace resolves the loader through `requiring-resolve` to
;; avoid a compile-time cycle with `com.blockether.vis-extension.extension`, which
;; dispatches `:ext/persistance` entries into this registry. If the
;; loader cannot be resolved, discovery becomes a no-op and the caller
;; is expected to `(require ...)` their backend explicitly.
;; ----------------------------------------------------------------------------

(defonce ^:private discovery-once
  ;; Lazy: triggered on first `create-store-connection`. Wrapped in `defonce`
  ;; so subsequent calls are O(1) reads of the realized delay. Resolves
  ;; the unified loader at use time to avoid a compile-time cycle.
  (delay
    (try
      (when-let [v (requiring-resolve 'com.blockether.vis-extension.extension/discover-extensions!)]
        (v))
      (catch Throwable _ 0))))

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

(defn create-store-connection
  "Open a persistence connection from `db-spec`.

   Common spec forms:
     nil              — no DB (returns nil)
     :memory          — in-memory ephemeral store (backend-defined)
     \"path/to.db\"   — file-backed store (backend-defined)
     {:backend :sqlite :path …}     — explicit backend selection
     {:backend :sqlite :datasource ds} — caller-owned DataSource

   With a single registered backend, omitting `:backend` works and the
   facade tags the returned store map with the chosen backend so all
   subsequent facade calls dispatch correctly."
  [db-spec]
  @discovery-once
  (let [normalized (normalize-spec db-spec)
        bid        (pick-backend-id (if (map? normalized)
                                      normalized
                                      {:backend (pick-backend-id {})}))
        f          @(resolve-impl {:backend bid} 'open-store)
        store      (f normalized)]
    (cond
      (nil? store) nil
      (map? store) (assoc store :backend bid)
      :else        store)))

(defn dispose-store-connection!
  "Close a persistence connection."
  [store]
  (when store
    (let [f @(resolve-impl store 'close-store)]
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
(defdelegate log! [db-info opts])

;; --- Conversation lifecycle ---
(defdelegate store-conversation!              [db-info opts])
(defdelegate db-get-conversation              [db-info ref])
(defdelegate db-resolve-conversation-id       [db-info sel])
(defdelegate db-list-conversations            [db-info channel])
(defdelegate db-find-conversation-by-external [db-info channel ext-id])
(defdelegate db-update-conversation-title!    [db-info ref title])
(defdelegate delete-conversation-tree!        [db-info id])
(defdelegate fork-conversation!               [db-info conv-id opts])

;; --- Query lifecycle ---
(defdelegate store-query!                  [db-info opts])
(defdelegate update-query!                 [db-info query-id opts])
(defdelegate db-list-queries-by-status     [db-info status])
(defdelegate db-list-conversation-queries  [db-info conv-ref])
(defdelegate retry-query!                  [db-info query-soul-id opts])

;; --- Iteration lifecycle ---
(defn store-iteration!
  "Same delegating shape as the macro-defined fns, but with input
   validation kept here so every backend gets the same precondition
   guarantees for free."
  [db-info opts]
  (when-not (map? opts)
    (throw (ex-info "store-iteration! opts must be a map" {:got (type opts)})))
  (when-not (:query-id opts)
    (throw (ex-info "store-iteration! requires :query-id" {:opts (keys opts)})))
  ((deref (resolve-impl db-info 'store-iteration!)) db-info opts))

(defdelegate db-list-query-iterations     [db-info query-ref])
(defdelegate db-list-iteration-vars       [db-info iter-ref])
(defdelegate db-list-iteration-expressions [db-info iter-ref])

;; --- Var registry & history ---
(defn db-latest-var-registry
  ([db-info conv-ref]      ((deref (resolve-impl db-info 'db-latest-var-registry)) db-info conv-ref))
  ([db-info conv-ref opts] ((deref (resolve-impl db-info 'db-latest-var-registry)) db-info conv-ref opts)))

(defdelegate db-var-history    [db-info conv-ref sym])
(defdelegate db-query-history  [db-info conv-ref])

;; --- Dependencies ---
(defdelegate store-dependency!     [db-info opts])
(defdelegate db-list-dependencies  [db-info conv-state-id])

;; --- Restore ---
(defdelegate db-restore-expressions [db-info conv-id])

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

(defn error->user-message
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

(defn shared-conn!
  "Return the process-wide shared persistence connection for `db-spec`,
   opening it on first call and caching the handle for the lifetime of
   the JVM. Subsequent calls return the cached handle regardless of
   the `db-spec` argument — the singleton intentionally pins to the
   first spec it saw.

   Pair with `dispose-shared-conn!` on process shutdown."
  [db-spec]
  (or @shared-conn
    (swap! shared-conn (fn [cur] (or cur (create-store-connection db-spec))))))

(defn dispose-shared-conn!
  "Close the shared connection if one is open. Idempotent."
  []
  (when-let [c @shared-conn]
    (try (dispose-store-connection! c) (catch Exception _ nil))
    (reset! shared-conn nil)))

;; =============================================================================
;; Orphan sweep (process-restart cleanup)
;; =============================================================================

(def ^:private ORPHAN_INTERRUPTED_ANSWER
  "Warning: Turn interrupted — the server was restarted before this answer could finalize. Re-send the message to retry.")

(defn sweep-orphaned-running-queries!
  "Mark every `:running` query as `:interrupted`. Run at process start
   to clean up queries that crashed or were killed mid-write so the
   next turn's handover digest renders the right outcome instead of
   guessing. Returns the number of queries swept."
  [db-info]
  (let [orphans (try (db-list-queries-by-status db-info :running)
                  (catch Exception _ []))]
    (doseq [{:keys [id iterations duration-ms]} orphans]
      (try
        (update-query! db-info id
          {:answer        ORPHAN_INTERRUPTED_ANSWER
           :iterations    (or iterations 0)
           :duration-ms   (or duration-ms 0)
           :status        :interrupted
           :prior-outcome :cancelled})
        (catch Exception _ nil)))
    (count orphans)))
