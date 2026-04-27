(ns com.blockether.vis.ext.self-debug.core
  "Self-debug extension — programmatic introspection of the agent's own
   state from inside `:code`. Exposes `(self/plan)`, `(self/breadcrumbs)`,
   `(self/attempts)`, `(self/turn-history)`, `(self/iteration-budget)`,
   `(self/var-history 'sym)`, `(self/errors)`.

   Every function is a pure read off the same DB tables the projection
   layer reads from. Failures return nil/[], never throw, so a
   misbehaving introspection call cannot break iteration execution.

   Extension contract:
   - `:ext/symbols` are bound under the `self` alias in the SCI sandbox.
   - Each fn takes `env` as its first arg; the loop's
     `wrap-extension` closes over the environment at registration time
     and the shared `:before-fn` `inject-environment` prepends it to
     args, so the model still calls e.g. `(self/plan)` with zero args.

   Opt-in: not auto-loaded by default. Add this jar to the classpath
   (or list it in `:dev`/`:vis` deps.edn aliases) to enable."
  (:require
   [clojure.string :as str]
   [com.blockether.vis.extension :as ext]
   [com.blockether.vis.persistance.core :as db]))

;; ---------------------------------------------------------------------------
;; Helpers \u2014 derive the current query-id and resolve env atoms.
;; ---------------------------------------------------------------------------

(defn- safe-deref [a]
  (when a (try (deref a) (catch Throwable _ nil))))

(defn- current-query-id
  "Best-effort resolution of the in-flight query-id for this conversation.
   Walks down: latest query for this conversation \u2192 its id. Returns
   nil when the env is mid-construction or the DB is unreachable."
  [{:keys [db-info conversation-id]}]
  (when (and db-info conversation-id)
    (try
      (some-> (db/db-list-conversation-queries db-info conversation-id)
        last
        :id)
      (catch Throwable _ nil))))

;; ---------------------------------------------------------------------------
;; Self-debug fns \u2014 pure reads, env-aware via the shared :before-fn below.
;; Every fn returns nil / [] on missing context so callers can chain
;; without `(when ...)` guards.
;; ---------------------------------------------------------------------------

(defn- self-plan
  "Current sticky plan-state for this query, or nil. Same map shape the
   projection's `<plan>` block renders."
  [env]
  (when-let [query-id (current-query-id env)]
    (try
      (some :plan-state
        (reverse (db/db-list-query-iterations (:db-info env) query-id)))
      (catch Throwable _ nil))))

(defn- self-breadcrumbs
  "Vector of `{:position int :breadcrumb str}` oldest-first for this
   query. Optional `n` clips to the last N entries (default: all)."
  ([env] (self-breadcrumbs env nil))
  ([env n]
   (when-let [query-id (current-query-id env)]
     (try
       (let [iterations (db/db-list-query-iterations (:db-info env) query-id)
             chain (->> iterations
                     (keep-indexed (fn [index iteration]
                                     (when (and (:breadcrumb iteration)
                                             (not (str/blank? (:breadcrumb iteration))))
                                       {:position index
                                        :breadcrumb (:breadcrumb iteration)}))))]
         (vec (if (and n (pos? (long n))) (take-last n chain) chain)))
       (catch Throwable _ [])))))

(defn- self-attempts
  "Vector of `{:iteration-id :iteration :code :result :error :stdout
   :stderr :duration-ms}` for every code-block executed in this query
   so far. Oldest-first. Optional `n` clips to the last N entries.

   This is the programmatic equivalent of what `<attempts>` will
   eventually render in the projection (Phase 2). Available now because
   the data is already persisted on `expression_state`."
  ([env] (self-attempts env nil))
  ([env n]
   (when-let [query-id (current-query-id env)]
     (try
       (let [iterations (db/db-list-query-iterations (:db-info env) query-id)
             attempts (->> iterations
                        (mapcat (fn [iteration]
                                  (let [iteration-id (:id iteration)
                                        position (:position iteration)
                                        rows (db/db-list-iteration-expressions
                                               (:db-info env) iteration-id)]
                                    (mapv (fn [row]
                                            {:iteration-id iteration-id
                                             :iteration position
                                             :code         (:code row)
                                             :result       (:result row)
                                             :error        (:error row)
                                             :stdout       (:stdout row)
                                             :duration-ms  (:duration-ms row)})
                                      rows)))))]
         (vec (if (and n (pos? (long n))) (take-last n attempts) attempts)))
       (catch Throwable _ [])))))

(defn- self-errors
  "Subset of `(self-attempts)` filtered to entries with non-nil :error.
   Useful when the model wants to review what failed without scrolling
   the whole attempts ledger."
  ([env]   (filterv :error (self-attempts env)))
  ([env n] (vec (take-last (or n 50) (filterv :error (self-attempts env))))))

(defn- self-turn-history
  "Vector of `{:turn-id :goal :outcome :answer :iterations}` for every
   query in this conversation, oldest-first. Optional `n` clips to the
   last N entries. Useful for cross-turn introspection on what the
   agent has been doing in this conversation."
  ([env] (self-turn-history env nil))
  ([env n]
   (when (and (:db-info env) (:conversation-id env))
     (try
       (let [queries (db/db-list-conversation-queries (:db-info env)
                       (:conversation-id env))
             history (mapv (fn [query]
                             (cond-> {:turn-id (:id query)
                                      :outcome (or (:prior-outcome query)
                                                 (:status query))}
                               (:answer query)     (assoc :answer (:answer query))
                               (:iterations query) (assoc :iterations (:iterations query))
                               (:text query)       (assoc :goal (:text query))))
                       queries)]
         (vec (if (and n (pos? (long n))) (take-last n history) history)))
       (catch Throwable _ [])))))

(defn- self-iteration-budget
  "Map `{:current N :budget M :remaining K}` describing the loop's
   current pointer. Mirrors `<system_state>.ITERATION` so the model can
   query budget mid-iteration without scraping the projection."
  [env]
  (let [current-zero-based (or (safe-deref (:current-iteration-atom env)) 0)
        current (inc (long current-zero-based))
        budget (or (safe-deref (:max-iterations-atom env)) 0)
        remaining (max 0 (- (long budget) current))]
    {:current current :budget budget :remaining remaining}))

(defn- self-var-history
  "Full version timeline for `sym`. Each entry is `{:value :code
   :version}`; oldest-first. Returns [] when the symbol has never been
   defined or when the DB is unreachable."
  [env sym]
  (when (and (:db-info env) (:conversation-id env) sym)
    (try
      (let [sym-resolved (cond
                           (symbol? sym) sym
                           (string? sym) (symbol sym)
                           :else (symbol (str sym)))]
        (vec (db/db-var-history (:db-info env) (:conversation-id env) sym-resolved)))
      (catch Throwable _ []))))

;; ---------------------------------------------------------------------------
;; Env injection \u2014 shared :before-fn for every symbol below.
;; Prepends the environment map to args so impl fns can be written as
;; ordinary `(defn- self-foo [env & rest])` without each one having to
;; replicate the env-fetch boilerplate.
;; ---------------------------------------------------------------------------

(defn- inject-environment
  "`:before-fn` for every self-debug symbol. The extension framework
   passes the environment map as `env`; we prepend it to the call's
   positional args so impl fns receive `(impl env & user-args)`. The
   model still invokes `(self/foo a b)` with two args; the impl sees
   `(env a b)`."
  [env _f args]
  {:args (vec (cons env args))})

;; ---------------------------------------------------------------------------
;; Symbol entries \u2014 each maps a sandbox-visible name to its impl fn,
;; documented for the LLM (the `:doc` + `:examples` fields render into
;; the auto-generated extension prompt).
;; ---------------------------------------------------------------------------

(def plan-symbol
  (ext/symbol 'plan self-plan
    {:doc "Current sticky plan-state for this query (or nil). Same shape rendered as `<plan>` in the projection."
     :arglists '([])
     :examples ["(self/plan)"]
     :before-fn inject-environment}))

(def breadcrumbs-symbol
  (ext/symbol 'breadcrumbs self-breadcrumbs
    {:doc "Vector of `{:position int :breadcrumb str}` oldest-first for this query. Optional N clips to last N."
     :arglists '([] [n])
     :examples ["(self/breadcrumbs)"
                "(self/breadcrumbs 5)"]
     :before-fn inject-environment}))

(def attempts-symbol
  (ext/symbol 'attempts self-attempts
    {:doc "Every code-block executed in this query: `[{:iteration-id :iteration :code :result :error :stdout :stderr :duration-ms} \u2026]`. Oldest-first. Optional N clips to last N."
     :arglists '([] [n])
     :examples ["(self/attempts)"
                "(self/attempts 20)"]
     :before-fn inject-environment}))

(def errors-symbol
  (ext/symbol 'errors self-errors
    {:doc "`(self-attempts)` filtered to entries with :error set. Optional N caps the result (default 50)."
     :arglists '([] [n])
     :examples ["(self/errors)"
                "(self/errors 10)"]
     :before-fn inject-environment}))

(def turn-history-symbol
  (ext/symbol 'turn-history self-turn-history
    {:doc "Every query in this conversation as `{:turn-id :goal :outcome :answer :iterations}`. Oldest-first. Optional N clips to last N."
     :arglists '([] [n])
     :examples ["(self/turn-history)"
                "(self/turn-history 3)"]
     :before-fn inject-environment}))

(def iteration-budget-symbol
  (ext/symbol 'iteration-budget self-iteration-budget
    {:doc "Map `{:current N :budget M :remaining K}` mirroring `<system_state>.ITERATION`."
     :arglists '([])
     :examples ["(self/iteration-budget)"]
     :before-fn inject-environment}))

(def var-history-symbol
  (ext/symbol 'var-history self-var-history
    {:doc "Full version timeline for `sym`: `[{:value :code :version} \u2026]` oldest-first. Returns [] when undefined."
     :arglists '([sym])
     :examples ["(self/var-history 'callers)"
                "(self/var-history \"foo-fn\")"]
     :before-fn inject-environment}))

(def all-symbols
  [plan-symbol
   breadcrumbs-symbol
   attempts-symbol
   errors-symbol
   turn-history-symbol
   iteration-budget-symbol
   var-history-symbol])

;; ---------------------------------------------------------------------------
;; Extension definition + global registration.
;; ---------------------------------------------------------------------------

(def extension
  (ext/extension
    {:ext/namespace 'com.blockether.vis.ext.self-debug.core
     :ext/doc       "Self-debug introspection: read your own plan, breadcrumbs, attempts, errors, turn history, iteration budget, and var history from inside :code."
     :ext/version   "0.1.0"
     :ext/author    "Blockether"
     :ext/license   "Apache-2.0"
     :ext/ns-alias  {:ns 'vis.ext.self :alias 'self}
     :ext/group     "self-debug"
     :ext/prompt    (str "Self-debug functions are READ-ONLY pure introspection. They never\n"
                      "throw \u2014 a misconfigured DB returns nil/[] instead. Reach for them\n"
                      "when the projection (`<plan>`, `<breadcrumbs>`, `<system_state>`)\n"
                      "doesn't carry what you need; otherwise stick to reading the\n"
                      "projection because it is bounded by construction.")
     :ext/symbols   all-symbols}))

(ext/register-global! extension)
