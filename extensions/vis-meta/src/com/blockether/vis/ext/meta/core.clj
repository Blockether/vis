(ns com.blockether.vis.ext.meta.core
  "Meta extension — programmatic introspection of the agent's own
   state from inside `:code`. Seven functions, all returning maps or
   vectors so the agent can manipulate the data structurally:

   - `(meta/turn)`                   → current turn snapshot (one map)
   - `(meta/conversation [id])`      → current or specific conversation
   - `(meta/conversations [channel])`→ list every conversation, optionally filtered
   - `(meta/var-history sym [id])`   → version timeline for a var
   - `(meta/find-attempts pattern [id])` → regex search over executed code
   - `(meta/failures [id])`          → provider + code failures as data
   - `(meta/diagnose [id])`          → counts and next actions for stalled turns

   Every function is a pure read off the same DB tables the projection
   layer reads from. Failures return nil/[], never throw, so a
   misbehaving introspection call cannot break iteration execution.

   The agent gets the data once, manipulates it via plain Clojure
   (`(filter …)`, `(get-in turn [:plan :items])`, etc.) instead of
   making 7 separate function calls.

   Opt-in: not auto-loaded by default. Add this jar to the classpath
   to enable."
  (:require
   [charred.api :as json]
   [clojure.string :as str]
   [com.blockether.vis.extension :as ext]
   [com.blockether.vis.persistance.core :as db]))

;; ---------------------------------------------------------------------------
;; Channels we know how to enumerate. Every channel-aware extension
;; registers itself, so this constant is the agent-visible canonical
;; list — new channels only need to be added here when they want
;; `(meta/conversations)` (no-arg form) to surface them.
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
  "Snapshot of the live iteration counter. Reads the env atom set by
   the iteration-loop's prepared context. The loop runs until the
   model emits `:answer` -- there is no model-visible budget, so the
   pointer carries only `:current`."
  [env]
  (let [current-zero-based (or (safe-deref (:current-iteration-atom env)) 0)]
    {:current (inc (long current-zero-based))}))

(defn- attempts-from-iterations
  "Walk `iterations` (in DB order) and collect every executed
   expression. Used by `(meta/turn)` and by `find-attempts`."
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

(defn- redundancy-summary
  "Aggregate `:expression-redundancy-fraction` + `:dedup-saves` across
   `iterations`. Returns a map the agent can use to check whether it
   has been issuing duplicate calls in this turn. Total / total
   ratio is computed from the actual expression count, not from
   per-iteration fractions averaged (which would over-weight short iterations)."
  [iterations attempt-count]
  (let [duplicate-total (reduce + 0
                          (keep #(get-in % [:metadata :dedup-saves]) iterations))]
    {:duplicate-count duplicate-total
     :total-count     attempt-count
     :fraction        (if (pos? attempt-count)
                        (double (/ duplicate-total attempt-count))
                        0.0)}))

(defn- parse-json-map
  "Best-effort JSON object parser for persisted provider errors. The
   persistence layer stores `iteration.llm_error` as JSON text; meta
   keeps parsing local so callers never need to run sqlite3 by hand."
  [text]
  (when (and (string? text) (not (str/blank? text)))
    (try
      (let [parsed (json/read-json text :key-fn keyword)]
        (when (map? parsed) parsed))
      (catch Throwable _ nil))))

(defn- preview
  ([text] (preview text 220))
  ([text limit]
   (when (some? text)
     (let [string-value (str text)]
       (if (> (count string-value) limit)
         (str (subs string-value 0 limit) "…")
         string-value)))))

(defn- error-text [error]
  (cond
    (nil? error) nil
    (string? error) error
    (map? error) (or (:message error) (pr-str error))
    :else (str error)))

(defn- keywordish [value]
  (cond
    (keyword? value) value
    (string? value) (keyword (str/replace value #"^:" ""))
    :else value))

(defn- schema-rejected-type? [value]
  (= :svar.spec/schema-rejected (keywordish value)))

(defn- provider-failure [iteration]
  (when-let [error (:error iteration)]
    (let [error-map (or (parse-json-map error) {:message (str error)})
          data      (:data error-map)
          type      (keywordish (:type data))
          reason    (keywordish (:reason data))
          raw-data  (:raw-data data)]
      (cond-> {:source       :provider
               :iteration-id (:id iteration)
               :iteration    (:position iteration)
               :status       (:status iteration)
               :message      (or (:message error-map) (str error))
               :classification (if (schema-rejected-type? type)
                                 :provider-schema-rejected
                                 :provider-error)}
        type                    (assoc :type type)
        reason                  (assoc :reason reason)
        (:received-type data)   (assoc :received-type (:received-type data))
        (some? raw-data)        (assoc :raw-preview (preview raw-data))
        (:raw-data-preview data) (assoc :raw-data-preview (:raw-data-preview data))))))

(defn- tool-name-from-code [code]
  (when (string? code)
    (second (re-find #"^\s*\(?([^\s\)]+)" code))))

(defn- classify-expression-failure [code error]
  (let [message (or (error-text error) "")
        lower-message (str/lower-case message)
        tool-name (or (tool-name-from-code code) "")]
    (cond
      (and (str/includes? tool-name "vis/rg")
        (str/includes? lower-message "unsupported escape character"))
      :regex-unsupported-escape

      (and (str/includes? tool-name "vis/rg")
        (str/includes? lower-message "unable to resolve symbol"))
      :regex-unescaped-quote

      (and (str/includes? tool-name "vis/patch")
        (str/includes? lower-message "unmatched delimiter"))
      :patch-unbalanced-string

      (and (str/includes? tool-name "vis/patch")
        (str/includes? lower-message "search block")
        (str/includes? lower-message "not found"))
      :patch-no-match

      (str/includes? lower-message "unable to resolve symbol")
      :unresolved-symbol

      :else
      :code-execution-error)))

(defn- advice-for-classification [classification]
  (case classification
    :provider-schema-rejected
    "Provider returned prose/string instead of the iteration map. Do not inspect SQLite; the raw preview is already here. Continue after the built-in schema retry, or switch model if it repeats."

    :regex-unsupported-escape
    "Clojure strings do not support \\|. Use bare | for regex alternation inside a string, or use a #\"…\" regex literal for complex patterns."

    :regex-unescaped-quote
    "The regex string likely contains an unescaped inner quote. Escape it as \\\" or use a regex literal / simpler pattern."

    :patch-unbalanced-string
    "The vis/patch EDN payload likely lost the closing quote of a :search or :replace string. Prefer the marker-delimited SEARCH/REPLACE patch form for bracket-heavy or quote-heavy edits."

    :patch-no-match
    "The SEARCH text did not match the file exactly. Use the near-match data when present, or re-read the smallest file slice and emit an exact-byte SEARCH block."

    :unresolved-symbol
    "A reader/string boundary probably split the form and exposed a bare symbol. Check quoting before retrying."

    "Read :message, :code, and :iteration; fix the smallest failing form before issuing new searches."))

(defn- expression-failures-for-iteration [db-info iteration]
  (try
    (->> (db/db-list-iteration-expressions db-info (:id iteration))
      (keep (fn [row]
              (when-let [error (:error row)]
                (let [classification (classify-expression-failure (:code row) error)]
                  {:source        :code
                   :iteration-id  (:id iteration)
                   :iteration     (:position iteration)
                   :tool          (tool-name-from-code (:code row))
                   :classification classification
                   :code          (:code row)
                   :message       (error-text error)
                   :advice        (advice-for-classification classification)}))))
      vec)
    (catch Throwable _ [])))

(defn- failures-from-iterations [db-info iterations]
  (vec
    (mapcat (fn [iteration]
              (let [provider (when-let [failure (provider-failure iteration)]
                               [(assoc failure :advice
                                  (advice-for-classification (:classification failure)))])]
                (concat provider (expression-failures-for-iteration db-info iteration))))
      iterations)))

(defn- latest-query [db-info conversation-id]
  (when (and db-info conversation-id)
    (last (try (db/db-list-conversation-queries db-info conversation-id)
            (catch Throwable _ [])))))

(defn- turn-snapshot
  "The single-call rich snapshot returned by `(meta/turn)`. Aggregates
   the data the projection ALSO surfaces (plan, breadcrumbs,
   iteration pointer) plus data the projection does NOT carry
   (attempts, provider/code failures, cost, elapsed-ms, redundancy).
   The agent picks what it needs by map key instead of querying
   SQLite manually."
  [env]
  (let [{:keys [db-info conversation-id]} env]
    (when-let [query (latest-query db-info conversation-id)]
      (let [iterations (iteration-rows db-info (:id query))
            attempts   (attempts-from-iterations db-info iterations)]
        (cond-> {:id          (:id query)
                 :goal        (:text query)
                 :status      (:status query)
                 :plan        (sticky-plan iterations)
                 :breadcrumbs (breadcrumbs-from-iterations iterations)
                 :attempts    attempts
                 :errors      (filterv :error attempts)
                 :failures    (failures-from-iterations db-info iterations)
                 :iteration   (iteration-pointer env)
                 :cost        (query-cost-summary query)
                 :redundancy  (redundancy-summary iterations (count attempts))}
          (elapsed-ms query) (assoc :elapsed-ms (elapsed-ms query)))))))

(defn- conversation-snapshot
  "Map for a single conversation: identity + every turn rolled up to a
   compact `{:id :goal :outcome :answer :iterations :status}` shape.
   Used by `(meta/conversation [id])`."
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
;; Meta fns — each takes `env` as first arg via the shared
;; `:before-fn` injector below. The agent never sees `env`; it calls
;; e.g. `(meta/turn)` with zero args.
;; ---------------------------------------------------------------------------

(defn- meta-turn [env]
  (turn-snapshot env))

(defn- meta-conversation
  ([env]
   (meta-conversation env (current-conversation-id env)))
  ([env conversation-id]
   (conversation-snapshot (:db-info env) conversation-id)))

(defn- meta-conversations
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
         (mapcat #(meta-conversations env %) KNOWN_CHANNELS)))))
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

(defn- meta-var-history
  "Full version timeline for `sym`: `[{:value :code :version} …]`
   oldest-first. Defaults to the current conversation; pass a
   `conversation-id` to query a different one."
  ([env sym]
   (meta-var-history env sym (current-conversation-id env)))
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

(defn- meta-find-attempts
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

(defn- meta-failures
  "Provider/schema and code/tool failures, normalized into one
   chronological vector. No arg = current turn. Pass a conversation id
   to scan every turn in that conversation. This is the replacement
   for ad-hoc sqlite3 triage from inside the agent."
  ([env]
   (when-let [turn (turn-snapshot env)]
     (:failures turn)))
  ([env conversation-id]
   (when (and (:db-info env) conversation-id)
     (try
       (vec
         (mapcat (fn [query]
                   (let [iterations (iteration-rows (:db-info env) (:id query))]
                     (mapv #(assoc % :turn-id (:id query)
                              :goal (:text query))
                       (failures-from-iterations (:db-info env) iterations))))
           (db/db-list-conversation-queries (:db-info env) conversation-id)))
       (catch Throwable _ [])))))

(defn- classification-counts [failures]
  (into {}
    (map (fn [[classification total]] [classification total]))
    (frequencies (map :classification failures))))

(defn- next-actions [failures]
  (let [classes (set (map :classification failures))]
    (vec
      (cond-> []
        (contains? classes :provider-schema-rejected)
        (conj "Treat schema rejection as provider noise, not a reason to inspect SQLite. Use :raw-preview from meta/failures and retry/switch model only if it repeats.")

        (contains? classes :regex-unsupported-escape)
        (conj "For vis/rg, replace \\| with bare | or use a #\"…\" regex literal before retrying.")

        (contains? classes :regex-unescaped-quote)
        (conj "Fix the quoted regex string; an inner quote escaped poorly and exposed a bare symbol.")

        (contains? classes :patch-unbalanced-string)
        (conj "Switch vis/patch to marker-delimited SEARCH/REPLACE form when search text contains brackets, quotes, or newlines.")

        (contains? classes :patch-no-match)
        (conj "Use any :near-match hint, then re-read the smallest file slice and emit an exact SEARCH block.")))))

(defn- meta-diagnose
  "Compact current-turn diagnosis built from meta/failures. Returns a
   map with counts and next actions so the agent can stop burning
   iterations on DB spelunking. Pass a conversation id to diagnose all
   turns in that conversation."
  ([env]
   (let [turn     (turn-snapshot env)
         failures (vec (:failures turn))]
     {:turn-id (:id turn)
      :goal (:goal turn)
      :status (:status turn)
      :failure-count (count failures)
      :by-classification (classification-counts failures)
      :failures failures
      :next-actions (next-actions failures)}))
  ([env conversation-id]
   (let [failures (vec (meta-failures env conversation-id))]
     {:conversation-id conversation-id
      :failure-count (count failures)
      :by-classification (classification-counts failures)
      :failures failures
      :next-actions (next-actions failures)})))

;; ---------------------------------------------------------------------------
;; Env injection — shared :before-fn for every symbol below.
;; Prepends the environment map to args so impl fns can be written as
;; ordinary `(defn- meta-foo [env & rest])` without each one having to
;; replicate the env-fetch boilerplate.
;; ---------------------------------------------------------------------------

(defn- inject-environment
  "`:before-fn` for every meta symbol. The framework passes the
   environment map as `env`; we prepend it to the call's positional
   args so impl fns receive `(impl env & user-args)`. The model still
   invokes `(meta/foo a b)` with two args; the impl sees `(env a b)`."
  [env _f args]
  {:args (vec (cons env args))})

;; ---------------------------------------------------------------------------
;; Symbol entries — each maps a sandbox-visible name to its impl fn,
;; documented for the LLM (the `:doc` + `:examples` fields render into
;; the auto-generated extension prompt).
;; ---------------------------------------------------------------------------

(def turn-symbol
  (ext/symbol 'turn meta-turn
    {:doc       (str "Snapshot of the current turn as a single map: "
                  "{:id :goal :status :plan :breadcrumbs :attempts :errors "
                  ":iteration :cost :elapsed-ms}. The agent reads keys "
                  "directly: (:plan (meta/turn)), (filter :error (:attempts (meta/turn))), etc.")
     :arglists  '([])
     :examples  ["(meta/turn)"
                 "(:plan (meta/turn))"
                 "(count (:attempts (meta/turn)))"
                 "(filter #(re-find #\"grep\" (:code %)) (:attempts (meta/turn)))"]
     :before-fn inject-environment}))

(def conversation-symbol
  (ext/symbol 'conversation meta-conversation
    {:doc       (str "Conversation snapshot: {:id :channel :title :model "
                  ":created-at :turns :turn-count}. Default = current "
                  "conversation; pass an id to inspect any other.")
     :arglists  '([] [conversation-id])
     :examples  ["(meta/conversation)"
                 "(meta/conversation \"3a7b2c…\")"
                 "(map :answer (:turns (meta/conversation)))"]
     :before-fn inject-environment}))

(def conversations-symbol
  (ext/symbol 'conversations meta-conversations
    {:doc       (str "Vector of every known conversation, newest-first: "
                  "[{:id :channel :title :created-at :turn-count} …]. "
                  "No arg = all channels; pass :tui / :telegram / :cli / "
                  ":vis to filter to one.")
     :arglists  '([] [channel])
     :examples  ["(meta/conversations)"
                 "(meta/conversations :telegram)"
                 "(filter #(= :telegram (:channel %)) (meta/conversations))"]
     :before-fn inject-environment}))

(def var-history-symbol
  (ext/symbol 'var-history meta-var-history
    {:doc       (str "Full version timeline for a var: "
                  "[{:value :code :version} …] oldest-first. Defaults to "
                  "the current conversation; pass an id to read from another.")
     :arglists  '([sym] [sym conversation-id])
     :examples  ["(meta/var-history 'callers)"
                 "(meta/var-history \"foo-fn\")"
                 "(meta/var-history 'foo \"3a7b2c…\")"]
     :before-fn inject-environment}))

(def find-attempts-symbol
  (ext/symbol 'find-attempts meta-find-attempts
    {:doc       (str "Regex search over executed :code strings. One-arg "
                  "form scans the current turn; two-arg form scans every "
                  "turn of the given conversation. Returns "
                  "[{:turn-id :iteration-id :iteration :code :result :error} …].")
     :arglists  '([pattern] [pattern conversation-id])
     :examples  ["(meta/find-attempts \"grep\")"
                 "(meta/find-attempts #\"\\bdef\\b\" \"3a7b2c…\")"
                 "(map :code (meta/find-attempts \"foo-fn\"))"]
     :before-fn inject-environment}))

(def failures-symbol
  (ext/symbol 'failures meta-failures
    {:doc       (str "Current-turn provider/schema and code/tool failures as "
                  "data. Includes classifications for schema rejections, "
                  "vis/rg escaping mistakes, malformed vis/patch payloads, "
                  "and SEARCH block misses. Pass a conversation id to scan "
                  "every turn. Use this instead of running sqlite3 from :code.")
     :arglists  '([] [conversation-id])
     :examples  ["(meta/failures)"
                 "(filter #(= :patch-no-match (:classification %)) (meta/failures))"
                 "(meta/failures \"3a7b2c…\")"]
     :before-fn inject-environment}))

(def diagnose-symbol
  (ext/symbol 'diagnose meta-diagnose
    {:doc       (str "Summarize current-turn failures into counts and next "
                  "actions: {:failure-count :by-classification :failures "
                  ":next-actions}. Pass a conversation id to diagnose all "
                  "turns. This is the first stop for stalled-agent triage, "
                  "not raw SQLite checks.")
     :arglists  '([] [conversation-id])
     :examples  ["(meta/diagnose)"
                 "(:next-actions (meta/diagnose))"
                 "(meta/diagnose \"3a7b2c…\")"]
     :before-fn inject-environment}))

(def all-symbols
  [turn-symbol
   conversation-symbol
   conversations-symbol
   var-history-symbol
   find-attempts-symbol
   failures-symbol
   diagnose-symbol])

;; ---------------------------------------------------------------------------
;; Extension definition + global registration.
;; ---------------------------------------------------------------------------

(def extension
  (ext/extension
    {:ext/namespace 'com.blockether.vis.ext.meta.core
     :ext/doc       "Meta introspection: read your own turn, conversation, var history, attempts, and failure diagnostics from inside :code. Returns plain maps and vectors for structural manipulation."
     :ext/version   "0.3.0"
     :ext/author    "Blockether"
     :ext/license   "Apache-2.0"
     :ext/ns-alias  {:ns 'vis.ext.meta :alias 'meta}
     :ext/group     "meta"
     :ext/prompt    (str "Meta functions are READ-ONLY introspection that return\n"
                      "Clojure maps/vectors. They never throw — DB / context\n"
                      "errors return nil/[]. Use meta/diagnose or meta/failures\n"
                      "when a turn stalls, a provider returns malformed schema\n"
                      "content, vis/rg parsing fails, or vis/patch reports a\n"
                      "SEARCH miss. Do NOT run sqlite3 from :code for current-turn\n"
                      "triage; these functions expose the same useful signal as\n"
                      "data. For just reading the plan or breadcrumbs, the\n"
                      "projection (`<plan>`, `<breadcrumbs>`, `<system_state>`)\n"
                      "is already in your prompt — use that.")
     :ext/symbols   all-symbols}))

(ext/register-global! extension)
