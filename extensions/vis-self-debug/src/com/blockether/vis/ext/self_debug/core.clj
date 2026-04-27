(ns com.blockether.vis.ext.self-debug.core
  "Self-debug extension — programmatic introspection of the agent's own
   state from inside `:code`. Five functions, all returning maps or
   vectors so the agent can manipulate the data structurally:

   - `(self/turn)`                   → current turn snapshot (one map)
   - `(self/conversation [id])`      → current or specific conversation
   - `(self/conversations [channel])`→ list every conversation, optionally filtered
   - `(self/var-history sym [id])`   → version timeline for a var
   - `(self/find-attempts pattern [id])` → regex search over executed code

   Every function is a pure read off the same DB tables the projection
   layer reads from. Failures return nil/[], never throw, so a
   misbehaving introspection call cannot break iteration execution.

   The agent gets the data once, manipulates it via plain Clojure
   (`(filter …)`, `(get-in turn [:plan :items])`, etc.) instead of
   making 7 separate function calls.

   Opt-in: not auto-loaded by default. Add this jar to the classpath
   to enable."
  (:require
   [clojure.string :as str]
   [com.blockether.vis.extension :as ext]
   [com.blockether.vis.persistance.core :as db]))

;; ---------------------------------------------------------------------------
;; Channels we know how to enumerate. Every channel-aware extension
;; registers itself, so this constant is the agent-visible canonical
;; list — new channels only need to be added here when they want
;; `(self/conversations)` (no-arg form) to surface them.
;; ---------------------------------------------------------------------------

(def ^:private KNOWN_CHANNELS
  [:vis :tui :telegram :cli])

;; ---------------------------------------------------------------------------
;; Helpers — derive ids, deref atoms, normalize sym args.
;; ---------------------------------------------------------------------------

(defn- safe-deref [a]
  (when a (try (deref a) (catch Throwable _ nil))))

(defn- as-sym [x]
  (cond
    (nil? x)     nil
    (symbol? x)  x
    (string? x)  (symbol x)
    :else        (symbol (str x))))

(defn- current-conversation-id [env]
  (:conversation-id env))

(defn- current-query-id
  "Most-recent query for the current conversation. Returns nil when
   the env is mid-construction or the DB is unreachable."
  [{:keys [db-info conversation-id]}]
  (when (and db-info conversation-id)
    (try
      (some-> (db/db-list-conversation-queries db-info conversation-id)
        last
        :id)
      (catch Throwable _ nil))))

(defn- iteration-rows
  "Fetch the iteration rows for `query-id`; returns [] on any failure."
  [db-info query-id]
  (try
    (db/db-list-query-iterations db-info query-id)
    (catch Throwable _ [])))

;; ---------------------------------------------------------------------------
;; Builders — assemble each top-level snapshot map.
;; ---------------------------------------------------------------------------

(defn- iteration-pointer
  "Snapshot of the live iteration counter. Reads the env atoms set by
   the iteration-loop's prepared context."
  [env]
  (let [current-zero-based (or (safe-deref (:current-iteration-atom env)) 0)
        current (inc (long current-zero-based))
        budget  (or (safe-deref (:max-iterations-atom env)) 0)]
    {:current   current
     :budget    budget
     :remaining (max 0 (- (long budget) current))}))

(defn- attempts-from-iterations
  "Walk `iterations` (in DB order) and collect every executed
   expression. Used by `(self/turn)` and by `find-attempts`."
  [db-info iterations]
  (vec
    (mapcat (fn [iteration]
              (let [iteration-id (:id iteration)
                    position     (:position iteration)]
                (try
                  (mapv (fn [row]
                          {:iteration-id iteration-id
                           :iteration    position
                           :code         (:code row)
                           :result       (:result row)
                           :error        (:error row)
                           :stdout       (:stdout row)
                           :duration-ms  (:duration-ms row)})
                    (db/db-list-iteration-expressions db-info iteration-id))
                  (catch Throwable _ []))))
      iterations)))

(defn- breadcrumbs-from-iterations [iterations]
  (vec
    (keep-indexed (fn [index iteration]
                    (when (and (:breadcrumb iteration)
                            (not (str/blank? (:breadcrumb iteration))))
                      {:position   index
                       :breadcrumb (:breadcrumb iteration)}))
      iterations)))

(defn- sticky-plan [iterations]
  (some :plan-state (reverse iterations)))

(defn- query-cost-summary
  "Pull the token / cost map persisted on `query_state.metadata`. Returns
   a map with the :input-tokens / :output-tokens / :total-cost keys
   when present, or an empty map. Never throws."
  [query]
  (cond-> {}
    (:input-tokens query)     (assoc :input-tokens     (:input-tokens query))
    (:output-tokens query)    (assoc :output-tokens    (:output-tokens query))
    (:reasoning-tokens query) (assoc :reasoning-tokens (:reasoning-tokens query))
    (:cached-tokens query)    (assoc :cached-tokens    (:cached-tokens query))
    (:total-cost query)       (assoc :total-cost       (:total-cost query))
    (:model query)            (assoc :model            (:model query))))

(defn- elapsed-ms
  "Wall-clock duration for the query in milliseconds. Read from
   `:duration-ms` when persisted; otherwise computed from the query's
   `:created-at` so the model can self-pace mid-turn."
  [query]
  (or (:duration-ms query)
    (when-let [created (:created-at query)]
      (try
        (- (System/currentTimeMillis)
          (cond
            (inst? created)    (inst-ms created)
            (integer? created) (long created)
            :else              0))
        (catch Throwable _ nil)))))

(defn- turn-snapshot
  "The single-call rich snapshot returned by `(self/turn)`. Aggregates
   the data the projection ALSO surfaces (plan, breadcrumbs,
   iteration pointer) plus data the projection does NOT carry
   (attempts, cost, elapsed-ms). The agent picks what it needs by map
   key."
  [env]
  (let [{:keys [db-info conversation-id]} env]
    (when (and db-info conversation-id)
      (when-let [queries (try (db/db-list-conversation-queries db-info conversation-id)
                           (catch Throwable _ nil))]
        (when-let [query (last queries)]
          (let [iterations (iteration-rows db-info (:id query))
                attempts   (attempts-from-iterations db-info iterations)]
            (cond-> {:id          (:id query)
                     :goal        (:text query)
                     :status      (:status query)
                     :plan        (sticky-plan iterations)
                     :breadcrumbs (breadcrumbs-from-iterations iterations)
                     :attempts    attempts
                     :errors      (filterv :error attempts)
                     :iteration   (iteration-pointer env)
                     :cost        (query-cost-summary query)}
              (elapsed-ms query) (assoc :elapsed-ms (elapsed-ms query)))))))))

(defn- conversation-snapshot
  "Map for a single conversation: identity + every turn rolled up to a
   compact `{:id :goal :outcome :answer :iterations :status}` shape.
   Used by `(self/conversation [id])`."
  [db-info conversation-id]
  (when (and db-info conversation-id)
    (try
      (when-let [conversation (db/db-get-conversation db-info conversation-id)]
        (let [queries (db/db-list-conversation-queries db-info conversation-id)
              turns (mapv (fn [query]
                            (cond-> {:id (:id query)
                                     :outcome (or (:prior-outcome query)
                                                (:status query))}
                              (:text query)       (assoc :goal       (:text query))
                              (:answer query)     (assoc :answer     (:answer query))
                              (:iterations query) (assoc :iterations (:iterations query))
                              (:total-cost query) (assoc :total-cost (:total-cost query))))
                      queries)]
          {:id          conversation-id
           :channel     (:channel conversation)
           :title       (:title conversation)
           :model       (:model conversation)
           :created-at  (:created-at conversation)
           :turns       turns
           :turn-count  (count turns)}))
      (catch Throwable _ nil))))

;; ---------------------------------------------------------------------------
;; Self-debug fns — each takes `env` as first arg via the shared
;; `:before-fn` injector below. The agent never sees `env`; it calls
;; e.g. `(self/turn)` with zero args.
;; ---------------------------------------------------------------------------

(defn- self-turn [env]
  (turn-snapshot env))

(defn- self-conversation
  ([env]
   (self-conversation env (current-conversation-id env)))
  ([env conversation-id]
   (conversation-snapshot (:db-info env) conversation-id)))

(defn- self-conversations
  "List every conversation the DB knows about, newest-first. With no
   arg, scans every channel in `KNOWN_CHANNELS`. With a channel kw,
   filters to that channel."
  ([env]
   (when (:db-info env)
     (vec
       (sort-by (comp #(if-let [c (:created-at %)]
                         (cond (inst? c)    (- (inst-ms c))
                           (integer? c) (- (long c))
                           :else        0)
                         0)
                  identity)
         (mapcat #(self-conversations env %) KNOWN_CHANNELS)))))
  ([env channel]
   (when (:db-info env)
     (try
       (mapv (fn [conversation]
               (let [conversation-id (:id conversation)
                     queries (try (db/db-list-conversation-queries (:db-info env) conversation-id)
                               (catch Throwable _ []))]
                 (cond-> {:id          conversation-id
                          :channel     (:channel conversation)
                          :title       (:title conversation)
                          :created-at  (:created-at conversation)
                          :turn-count  (count queries)}
                   (:external-id conversation) (assoc :external-id (:external-id conversation)))))
         (db/db-list-conversations (:db-info env) channel))
       (catch Throwable _ [])))))

(defn- self-var-history
  "Full version timeline for `sym`: `[{:value :code :version} …]`
   oldest-first. Defaults to the current conversation; pass a
   `conversation-id` to query a different one."
  ([env sym]
   (self-var-history env sym (current-conversation-id env)))
  ([env sym conversation-id]
   (when (and (:db-info env) conversation-id sym)
     (try
       (vec (db/db-var-history (:db-info env) conversation-id (as-sym sym)))
       (catch Throwable _ [])))))

(defn- ->pattern
  "Coerce a regex argument into a `java.util.regex.Pattern`. Strings
   become `(re-pattern s)`; anything else falls back to its string
   representation. Centralizes the coercion so `re-find` never sees a
   non-Pattern (it would throw `ClassCastException`)."
  [x]
  (cond
    (instance? java.util.regex.Pattern x) x
    (string? x) (re-pattern x)
    :else       (re-pattern (str x))))

(defn- code-matches? [pattern attempt]
  ;; `re-find` is `(pattern, string)` — don't thread it.
  (let [code (:code attempt)]
    (and (string? code) (re-find pattern code))))

(defn- self-find-attempts
  "Regex search over executed `:code` strings. With one arg, searches
   the CURRENT TURN's attempts. With two args, searches every turn of
   the given conversation. Returns
   `[{:turn-id :iteration-id :iteration :code :result :error} …]`."
  ([env pattern]
   (let [pattern (->pattern pattern)]
     (when-let [turn (turn-snapshot env)]
       (->> (:attempts turn)
         (filterv #(code-matches? pattern %))
         (mapv #(assoc % :turn-id (:id turn)))))))
  ([env pattern conversation-id]
   (when (and (:db-info env) conversation-id)
     (let [pattern (->pattern pattern)
           queries (try (db/db-list-conversation-queries (:db-info env) conversation-id)
                     (catch Throwable _ []))]
       (vec
         (mapcat (fn [query]
                   (let [iterations (iteration-rows (:db-info env) (:id query))]
                     (->> (attempts-from-iterations (:db-info env) iterations)
                       (filter #(code-matches? pattern %))
                       (mapv #(assoc % :turn-id (:id query))))))
           queries))))))

;; ---------------------------------------------------------------------------
;; Env injection — shared :before-fn for every symbol below.
;; Prepends the environment map to args so impl fns can be written as
;; ordinary `(defn- self-foo [env & rest])` without each one having to
;; replicate the env-fetch boilerplate.
;; ---------------------------------------------------------------------------

(defn- inject-environment
  "`:before-fn` for every self-debug symbol. The framework passes the
   environment map as `env`; we prepend it to the call's positional
   args so impl fns receive `(impl env & user-args)`. The model still
   invokes `(self/foo a b)` with two args; the impl sees `(env a b)`."
  [env _f args]
  {:args (vec (cons env args))})

;; ---------------------------------------------------------------------------
;; Symbol entries — each maps a sandbox-visible name to its impl fn,
;; documented for the LLM (the `:doc` + `:examples` fields render into
;; the auto-generated extension prompt).
;; ---------------------------------------------------------------------------

(def turn-symbol
  (ext/symbol 'turn self-turn
    {:doc       (str "Snapshot of the current turn as a single map: "
                  "{:id :goal :status :plan :breadcrumbs :attempts :errors "
                  ":iteration :cost :elapsed-ms}. The agent reads keys "
                  "directly: (:plan (self/turn)), (filter :error (:attempts (self/turn))), etc.")
     :arglists  '([])
     :examples  ["(self/turn)"
                 "(:plan (self/turn))"
                 "(count (:attempts (self/turn)))"
                 "(filter #(re-find #\"grep\" (:code %)) (:attempts (self/turn)))"]
     :before-fn inject-environment}))

(def conversation-symbol
  (ext/symbol 'conversation self-conversation
    {:doc       (str "Conversation snapshot: {:id :channel :title :model "
                  ":created-at :turns :turn-count}. Default = current "
                  "conversation; pass an id to inspect any other.")
     :arglists  '([] [conversation-id])
     :examples  ["(self/conversation)"
                 "(self/conversation \"3a7b2c…\")"
                 "(map :answer (:turns (self/conversation)))"]
     :before-fn inject-environment}))

(def conversations-symbol
  (ext/symbol 'conversations self-conversations
    {:doc       (str "Vector of every known conversation, newest-first: "
                  "[{:id :channel :title :created-at :turn-count} …]. "
                  "No arg = all channels; pass :tui / :telegram / :cli / "
                  ":vis to filter to one.")
     :arglists  '([] [channel])
     :examples  ["(self/conversations)"
                 "(self/conversations :telegram)"
                 "(filter #(= :telegram (:channel %)) (self/conversations))"]
     :before-fn inject-environment}))

(def var-history-symbol
  (ext/symbol 'var-history self-var-history
    {:doc       (str "Full version timeline for a var: "
                  "[{:value :code :version} …] oldest-first. Defaults to "
                  "the current conversation; pass an id to read from another.")
     :arglists  '([sym] [sym conversation-id])
     :examples  ["(self/var-history 'callers)"
                 "(self/var-history \"foo-fn\")"
                 "(self/var-history 'foo \"3a7b2c…\")"]
     :before-fn inject-environment}))

(def find-attempts-symbol
  (ext/symbol 'find-attempts self-find-attempts
    {:doc       (str "Regex search over executed :code strings. One-arg "
                  "form scans the current turn; two-arg form scans every "
                  "turn of the given conversation. Returns "
                  "[{:turn-id :iteration-id :iteration :code :result :error} …].")
     :arglists  '([pattern] [pattern conversation-id])
     :examples  ["(self/find-attempts \"grep\")"
                 "(self/find-attempts #\"\\bdef\\b\" \"3a7b2c…\")"
                 "(map :code (self/find-attempts \"foo-fn\"))"]
     :before-fn inject-environment}))

(def all-symbols
  [turn-symbol
   conversation-symbol
   conversations-symbol
   var-history-symbol
   find-attempts-symbol])

;; ---------------------------------------------------------------------------
;; Extension definition + global registration.
;; ---------------------------------------------------------------------------

(def extension
  (ext/extension
    {:ext/namespace 'com.blockether.vis.ext.self-debug.core
     :ext/doc       "Self-debug introspection: read your own turn, conversation, var history, and search attempts from inside :code. Returns plain maps and vectors for structural manipulation."
     :ext/version   "0.2.0"
     :ext/author    "Blockether"
     :ext/license   "Apache-2.0"
     :ext/ns-alias  {:ns 'vis.ext.self :alias 'self}
     :ext/group     "self-debug"
     :ext/prompt    (str "Self-debug functions are READ-ONLY pure introspection that\n"
                      "returns Clojure maps/vectors. They never throw — DB / context\n"
                      "errors return nil/[]. Reach for them when you need to\n"
                      "PROGRAMMATICALLY MANIPULATE state (filter attempts, count\n"
                      "turns, search by regex). For just reading the plan or\n"
                      "breadcrumbs, the projection (`<plan>`, `<breadcrumbs>`,\n"
                      "`<system_state>`) is already in your prompt — use that.")
     :ext/symbols   all-symbols}))

(ext/register-global! extension)
