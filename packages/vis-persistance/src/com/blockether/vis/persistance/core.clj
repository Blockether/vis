(ns com.blockether.vis.persistance.core
  "Persistence facade.

   This namespace is **backend-agnostic**. It defines the public API
   (every fn callers use) and dispatches to a registered backend via
   `register-backend!`. Backends live in their own jars
   (`vis-persistance-sqlite`, future `vis-persistance-postgres`, …)
   and call `register-backend!` at namespace load time.

   ── Calling convention ─────────────────────────────────────────────

       (require 'com.blockether.vis.persistance.sqlite.core)  ;; loads + registers :sqlite
       (def store (persistance/create-rlm-conn \"vis.db\"))   ;; backend chosen automatically
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

   ── Auto-discovery ────────────────────────────────────────────────

   Backends are loaded the same way channels and extensions are: ship
   `META-INF/vis/persistance-backends.edn` in your jar's resources/
   listing the namespaces that call `register-backend!`. The first
   `create-rlm-conn` call triggers `discover-backends!`, which is
   memoized for the process lifetime."
  (:require [clojure.edn :as edn]
            [clojure.string :as str]))

;; =============================================================================
;; Backend registry
;; =============================================================================

(defonce ^:private backends
  ;; {:sqlite {:ns 'com.blockether.vis.persistance.sqlite.core}}
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
;; ----------------------------------------------------------------------------

(def ^:private BACKENDS_RESOURCE "META-INF/vis/persistance-backends.edn")

(defn discover-backends!
  "Scan the classpath for every `META-INF/vis/persistance-backends.edn`
   resource. Each file is an EDN vector of namespace symbols whose
   load triggers `register-backend!`. Returns the count of backends
   added by this call. Idempotent through the underlying `require`
   cache — calling it twice does no extra work."
  []
  (let [urls   (try
                 (enumeration-seq
                   (.getResources
                     (.getContextClassLoader (Thread/currentThread))
                     BACKENDS_RESOURCE))
                 (catch Exception _ nil))
        before (set (keys @backends))]
    (doseq [^java.net.URL url urls]
      (try
        (let [ns-syms (edn/read-string (slurp url))]
          (when (sequential? ns-syms)
            (doseq [ns-sym ns-syms]
              (when (symbol? ns-sym)
                (try (require ns-sym)
                  (catch Throwable _ nil))))))
        (catch Throwable _ nil)))
    (- (count @backends) (count before))))

(defonce ^:private discovery-once
  ;; Lazy: triggered on first `create-rlm-conn`. Wrapped in `defonce`
  ;; so subsequent calls are O(1) reads of the realized delay.
  (delay (discover-backends!)))

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
            "`com.blockether.vis.persistance.sqlite.core`?"))
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
  "Strip the legacy `:temp` alias and reshape the legacy explicit-sqlite
   nested forms into the canonical shape backends accept."
  [db-spec]
  (cond
    (= :temp db-spec)               :memory
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

(defn create-rlm-conn
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

(defn dispose-rlm-conn!
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
