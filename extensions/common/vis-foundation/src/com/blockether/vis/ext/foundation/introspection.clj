(ns com.blockether.vis.ext.foundation.introspection
  "Meta extension — programmatic introspection of the agent's own
   state from inside `:code`. Ten functions, all returning maps or
   vectors so the agent can manipulate the data structurally:

   - `(foundation/turn)`                       → current turn snapshot (one map)
   - `(foundation/conversation [id])`          → current or specific conversation
   - `(foundation/conversations [channel])`    → list every conversation, optionally filtered
   - `(foundation/var-history sym [id])`       → version timeline for a var
   - `(foundation/find-attempts pattern [id])` → regex search over executed code
   - `(foundation/failures [id])`              → provider + code failures as data
   - `(foundation/diagnose [id])`              → counts and next actions for stalled turns
   - `(foundation/extensions)`                 → catalog of every loaded extension
   - `(foundation/extension-docs [ref])`       → docs declared by an extension with descriptions
   - `(foundation/extension-doc ref name)`     → full Markdown body of a declared doc
   - `(foundation/extension-readme ref)`       → convenience for (extension-doc ref README.md)

   Every function is a pure read off the same DB tables the projection
   layer reads from (or a classpath read for the doc accessors).
   Failures return nil/[], never throw, so a misbehaving introspection
   call cannot break iteration execution.

   The agent gets the data once, manipulates it via plain Clojure
   (`(filter …)`, `(map :code (:attempts turn))`, etc.) instead of
   making 7 separate function calls.

   Opt-in: not auto-loaded by default. Add this jar to the classpath
   to enable."
  (:require
   [charred.api :as json]
   [clojure.string :as str]
   [com.blockether.vis.core :as sdk]))

;; ---------------------------------------------------------------------------
;; Channels we know how to enumerate. Derived from the global channel
;; registry (`sdk/registered-channels`) so any third-party channel jar
;; on the classpath surfaces in `(foundation/conversations)` automatically —
;; no edits to this file when a new front-end ships.
;;
;; `:cli` is added unconditionally because the CLI agent uses `:cli` as
;; its conversations-channel namespace WITHOUT registering a channel
;; descriptor (the `vis` dispatcher itself is the surface; there is no
;; `vis channels cli` sub-command, so it has no `:channel/cmd`). Every
;; other channel id comes from the registry.
;; ---------------------------------------------------------------------------

(defn- known-channels
  "Vec of conversations-channel keywords known to this process. Derived
   from the global channel registry plus the implicit `:cli` namespace."
  []
  (->> (sdk/registered-channels)
    (map :channel/id)
    (cons :cli)
    distinct
    vec))

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
  "UUID of the in-flight turn, or nil when no turn is running yet.

   Internally the turn is persisted as a `query` row — hence the legacy
   binding name `CURRENT_QUERY_ID` and this helper's `query-id` suffix.
   The product concept is *turn*; everywhere this id is surfaced to
   the model (e.g. `(foundation/turn)` results, the `:in-flight-turn-id`
   slot) it's labelled `turn`. Read from `:current-query-id-atom` set
   by
   `iteration-loop`. Mirrors the SCI-visible `CURRENT_QUERY_ID` system
   var so meta-fns can filter it without round-tripping through SCI."
  [env]
  (some-> (:current-query-id-atom env) deref))

(defn- same-uuid?
  "True when two values denote the same UUID. Accepts UUID instances
   or any object whose `str` is the canonical UUID form. Used to
   match `CURRENT_QUERY_ID` against a turn's `:id` regardless of
   whether the persistence layer returned a UUID or a string."
  [a b]
  (and a b (= (str a) (str b))))

(defn- iteration-rows
  "Fetch the iteration rows for `query-id`; returns [] on any failure."
  [db-info query-id]
  (try
    (sdk/db-list-query-iterations db-info query-id)
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
   expression. Used by `(foundation/turn)` and by `find-attempts`."
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
                    (sdk/db-list-iteration-blocks db-info iteration-id))
                  (catch Throwable _ []))))
      iterations)))

(defn- format-provider-model
  "Render `\"provider/model\"` when both are known, otherwise just the
   model (or just the provider) so callers always get a non-empty
   string when at least one component exists. Returns nil when both
   are nil/blank — callers `cond->` on the result."
  [provider model]
  (let [provider-str (some-> provider name str/trim not-empty)
        model-str    (some-> model str str/trim not-empty)]
    (cond
      (and provider-str model-str) (str provider-str "/" model-str)
      model-str                    model-str
      provider-str                 provider-str
      :else                        nil)))

(defn- query-cost-summary
  "Pull the token / cost / provider / model map persisted on
   `query_state.metadata` (with `llm_root_provider` /
   `llm_root_model` as the canonical typed columns). Returns a map
   with the :input-tokens / :output-tokens / :total-cost / :provider
   / :model / :provider-model keys when present, or an empty map.
   Never throws.

   `:provider-model` is a derived `\"provider/model\"` display string
   (e.g. `\"openai/gpt-4o\"`) so callers don't have to format it
   themselves — the canonical data still lives in `:provider` and
   `:model` separately."
  [query]
  (let [provider-model (format-provider-model (:provider query) (:model query))]
    (cond-> {}
      (:input-tokens query)     (assoc :input-tokens     (:input-tokens query))
      (:output-tokens query)    (assoc :output-tokens    (:output-tokens query))
      (:reasoning-tokens query) (assoc :reasoning-tokens (:reasoning-tokens query))
      (:cached-tokens query)    (assoc :cached-tokens    (:cached-tokens query))
      (:total-cost query)       (assoc :total-cost       (:total-cost query))
      (:provider query)         (assoc :provider         (:provider query))
      (:model query)            (assoc :model            (:model query))
      provider-model            (assoc :provider-model   provider-model))))

(defn- elapsed-ms
  "Wall-clock duration for a turn in milliseconds. Read from
   `:duration-ms` when persisted; otherwise computed from the
   underlying query row's `:created-at` so the model can self-pace
   mid-turn."
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
   keeps parsing local so callers get failures as Clojure data."
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
    "The vis/patch EDN payload likely lost the closing quote of a :search or :replace string. Re-emit as the canonical vector shape (vis/patch [{:path :search :replace} ...]) and compose multi-line content with (str \"line1\\n\" \"line2\\n\") so each line stays on its own physical line and the closing quote stays visible."

    :patch-no-match
    "The SEARCH text did not match the file exactly. Use the near-match data when present, or re-read the smallest file slice and emit an exact-byte SEARCH block."

    :unresolved-symbol
    "A reader/string boundary probably split the form and exposed a bare symbol. Check quoting before retrying."

    "Read :message, :code, and :iteration; fix the smallest failing form before issuing new searches."))

(defn- expression-failures-for-iteration [db-info iteration]
  (try
    (->> (sdk/db-list-iteration-blocks db-info (:id iteration))
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
    (last (try (sdk/db-list-conversation-queries db-info conversation-id)
            (catch Throwable _ [])))))

(defn- turn-snapshot
  "The single-call rich snapshot returned by `(foundation/turn)`. Aggregates
   the per-iteration data the prompt projection does NOT carry
   (attempts, provider/code failures, cost, elapsed-ms, redundancy)
   plus the iteration pointer. The agent picks what it needs by map
   key instead of querying SQLite manually."
  [env]
  (let [{:keys [db-info conversation-id]} env]
    (when-let [query (latest-query db-info conversation-id)]
      (let [iterations (iteration-rows db-info (:id query))
            attempts   (attempts-from-iterations db-info iterations)]
        (cond-> {:id          (:id query)
                 :goal        (:text query)
                 :status      (:status query)
                 :attempts    attempts
                 :errors      (filterv :error attempts)
                 :failures    (failures-from-iterations db-info iterations)
                 :iteration   (iteration-pointer env)
                 :cost        (query-cost-summary query)
                 :redundancy  (redundancy-summary iterations (count attempts))}
          (elapsed-ms query) (assoc :elapsed-ms (elapsed-ms query)))))))

(defn- conversation-snapshot
  "Map for a single conversation: identity + every turn rolled up to a
   compact `{:id :goal :outcome :answer :iteration-count :status}`
   shape. Used by `(foundation/conversation [id])`.

   `:iteration-count` is the integer number of LLM rounds the turn
   consumed. Spelled out so it never gets confused with the vector
   shape that the runtime trace uses for per-iteration entries."
  [db-info conversation-id]
  (when (and db-info conversation-id)
    (try
      (when-let [conversation (sdk/db-get-conversation db-info conversation-id)]
        (let [queries (sdk/db-list-conversation-queries db-info conversation-id)
              turns (mapv (fn [query]
                            (cond-> {:id (:id query)
                                     :outcome (or (:prior-outcome query)
                                                (:status query))}
                              (:text query)            (assoc :goal            (:text query))
                              (:answer query)          (assoc :answer          (:answer query))
                              (:iteration-count query) (assoc :iteration-count (:iteration-count query))
                              (:total-cost query)      (assoc :total-cost      (:total-cost query))))
                      queries)]
          (cond-> {:id          conversation-id
                   :channel     (:channel conversation)
                   :title       (:title conversation)
                   :model       (:model conversation)
                   :created-at  (:created-at conversation)
                   :turns       turns
                   :turn-count  (count turns)}
            (:provider conversation)
            (assoc :provider (:provider conversation))
            (format-provider-model (:provider conversation) (:model conversation))
            (assoc :provider-model
              (format-provider-model (:provider conversation) (:model conversation))))))
      (catch Throwable _ nil))))

;; ---------------------------------------------------------------------------
;; Meta fns — each takes `env` as first arg via the shared
;; `:before-fn` injector below. The agent never sees `env`; it calls
;; e.g. `(foundation/turn)` with zero args.
;; ---------------------------------------------------------------------------

(defn- foundation-turn [env]
  (turn-snapshot env))

(defn- foundation-conversation
  "Snapshot for a conversation. The in-flight turn (= current `CURRENT_QUERY_ID`)
   is automatically excluded from `:turns` because its `:iteration-count` /
   `:total-cost` haven't been finalized yet — listing it would render
   as `null | $null` and confuse downstream summaries. The excluded id
   is surfaced as `:in-flight-turn-id` so callers can opt into seeing
   it. Filtering happens only when the in-flight turn belongs to this
   conversation; foreign conversations are returned verbatim."
  ([env]
   (foundation-conversation env (current-conversation-id env)))
  ([env conversation-id]
   (when-let [snapshot (conversation-snapshot (:db-info env) conversation-id)]
     (let [in-flight-id (current-query-id env)
           same-conv?   (and in-flight-id
                          (same-uuid? conversation-id (current-conversation-id env)))]
       (if-not same-conv?
         snapshot
         (let [filtered (filterv #(not (same-uuid? in-flight-id (:id %)))
                          (:turns snapshot))]
           (-> snapshot
             (assoc :turns filtered)
             (assoc :turn-count (count filtered))
             (assoc :in-flight-turn-id in-flight-id))))))))

(defn- foundation-conversations
  "List every conversation the DB knows about, newest-first. With no
   arg, scans every channel surfaced by `known-channels`. With a
   channel kw, filters to that channel. Returns `[]` (never nil) when
   the env is missing a `:db-info` handle so callers can chain seq
   operations safely."
  ([env]
   (if (:db-info env)
     (vec
       (sort-by (comp #(if-let [c (:created-at %)]
                         (cond (inst? c)    (- (inst-ms c))
                           (integer? c) (- (long c))
                           :else        0)
                         0)
                  identity)
         (mapcat #(foundation-conversations env %) (known-channels))))
     []))
  ([env channel]
   (if (:db-info env)
     (try
       (mapv (fn [conversation]
               (let [conversation-id (:id conversation)
                     queries (try (sdk/db-list-conversation-queries (:db-info env) conversation-id)
                               (catch Throwable _ []))]
                 (cond-> {:id          conversation-id
                          :channel     (:channel conversation)
                          :title       (:title conversation)
                          :created-at  (:created-at conversation)
                          :turn-count  (count queries)}
                   (:external-id conversation) (assoc :external-id (:external-id conversation)))))
         (sdk/db-list-conversations (:db-info env) channel))
       (catch Throwable _ []))
     [])))

(defn- foundation-var-history
  "Full version timeline for `sym`: `[{:value :code :version} …]`
   oldest-first. Defaults to the current conversation; pass a
   `conversation-id` to inspect a different one. Returns `[]` (never
   nil) when the env is missing required handles."
  ([env sym]
   (foundation-var-history env sym (current-conversation-id env)))
  ([env sym conversation-id]
   (if (and (:db-info env) conversation-id sym)
     (try
       (vec (sdk/db-var-history (:db-info env) conversation-id (as-sym sym)))
       (catch Throwable _ []))
     [])))

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

(defn- foundation-find-attempts
  "Regex search over executed `:code` strings. With one arg, searches
   the CURRENT TURN's attempts only. With two args, searches every turn
   of the given conversation. To scan EVERY conversation in the DB use
   `foundation/find-attempts-everywhere` instead. Returns
   `[{:turn-id :iteration-id :iteration :code :result :error} …]` —
   `[]` (never nil) when nothing matches or the env is missing handles."
  ([env pattern]
   (let [pattern (->pattern pattern)
         turn    (turn-snapshot env)]
     (if turn
       (->> (:attempts turn)
         (filterv #(code-matches? pattern %))
         (mapv #(assoc % :turn-id (:id turn))))
       [])))
  ([env pattern conversation-id]
   (if (and (:db-info env) conversation-id)
     (let [pattern (->pattern pattern)
           queries (try (sdk/db-list-conversation-queries (:db-info env) conversation-id)
                     (catch Throwable _ []))]
       (vec
         (mapcat (fn [query]
                   (let [iterations (iteration-rows (:db-info env) (:id query))]
                     (->> (attempts-from-iterations (:db-info env) iterations)
                       (filter #(code-matches? pattern %))
                       (mapv #(assoc % :turn-id (:id query))))))
           queries)))
     [])))

(defn- foundation-conversation-forks
  "List every `conversation_state` row for the conversation soul behind
   `conversation-id`, oldest version first. Each row maps to
   `{:state-id :version :parent-state-id :title :system-prompt :provider
     :model :created-at :query-count}`. The trunk is `:version 0` with
   `:parent-state-id nil`; any later row with non-nil `:parent-state-id`
   is a fork off the referenced state. Returns `[]` (never nil) when
   the conversation has no rows OR the env is missing handles — lets
   callers chain `(group-by :parent-state-id ...)` without nil-guards.

   No-arg form uses the current-conversation-id from the env."
  ([env]
   (foundation-conversation-forks env (current-conversation-id env)))
  ([env conversation-id]
   (if (and (:db-info env) conversation-id)
     (try
       (vec (sdk/db-list-conversation-states (:db-info env) conversation-id))
       (catch Throwable _ []))
     [])))

(defn- meta-query-retries
  "List every `query_state` row (= every retry version) for the query
   soul behind `query-id`, oldest version first. Each row maps to
   `{:state-id :version :forked-from-query-state-id :status :prior-outcome
     :provider :model :created-at :iteration-count}`. Version 0 with
   `:forked-from-query-state-id nil` is the original run; any higher
   version is a retry. `query-id` is a `query_soul` UUID — the same id
   surfaced as `:turn-id` by `foundation/find-attempts`, `:id` by `foundation/turn`,
   or `:turns[].id` by `foundation/conversation`. Returns `[]` (never nil)
   when the query is unknown or the env is missing handles."
  [env query-id]
  (if (and (:db-info env) query-id)
    (try
      (vec (sdk/db-list-query-states (:db-info env) query-id))
      (catch Throwable _ []))
    []))

(defn- foundation-find-attempts-everywhere
  "Regex search over executed `:code` strings across EVERY conversation
   the DB knows about. Same hit shape as `foundation-find-attempts`, plus a
   `:conversation-id` key on each entry so callers can group / drill
   in. Heavy: walks every persisted turn + iteration on disk. Returns
   `[]` (never nil) when nothing matches or the env has no
   `:db-info`."
  [env pattern]
  (if (:db-info env)
    (vec
      (mapcat (fn [{:keys [id]}]
                (mapv #(assoc % :conversation-id id)
                  (foundation-find-attempts env pattern id)))
        (foundation-conversations env)))
    []))

(defn- foundation-failures
  "Provider/schema and code/tool failures, normalized into one
   chronological vector. No arg = current turn. Pass a conversation id
   to scan every turn in that conversation. To scan EVERY conversation
   in the DB use `foundation/failures-everywhere` instead. Returns `[]`
   (never nil) when there is nothing to report or the env is missing
   handles."
  ([env]
   (or (:failures (turn-snapshot env)) []))
  ([env conversation-id]
   (if (and (:db-info env) conversation-id)
     (try
       (vec
         (mapcat (fn [query]
                   (let [iterations (iteration-rows (:db-info env) (:id query))]
                     (mapv #(assoc % :turn-id (:id query)
                              :goal (:text query))
                       (failures-from-iterations (:db-info env) iterations))))
           (sdk/db-list-conversation-queries (:db-info env) conversation-id)))
       (catch Throwable _ []))
     [])))

(defn- foundation-failures-everywhere
  "Provider/schema and code/tool failures across EVERY conversation the
   DB knows about. Same shape as `foundation-failures` (each entry already
   carries `:turn-id` and `:goal`), with `:conversation-id` added so
   callers can group / drill in. Heavy: walks every persisted turn +
   iteration on disk. Returns `[]` (never nil) when there is nothing
   to report or the env has no `:db-info`."
  [env]
  (if (:db-info env)
    (vec
      (mapcat (fn [{:keys [id]}]
                (mapv #(assoc % :conversation-id id)
                  (foundation-failures env id)))
        (foundation-conversations env)))
    []))

(defn- classification-counts [failures]
  (into {}
    (map (fn [[classification total]] [classification total]))
    (frequencies (map :classification failures))))

(def ^:private REPETITION_THRESHOLD
  "Minimum number of failures sharing the same normalized signature
   before the turn is flagged as locked in a same-error loop. Empirical
   floor: agents that miss a path 2-3× and pivot stay below; agents
   that emit 5+ identical-root-cause errors are stuck and not learning.
   Anchored to the worst-case in the self-analyze report for
   conversation 89ea9c98-21d4-4483-a962-f8ccb1d8232d (148 'src/tui not
   found' failures in one turn — the failure mode this catches)."
  5)

(defn- repetition-signature
  "Project a failure to a lossy 'same root cause' signature so 148
   varying filename attempts under the same missing directory hash to
   the same bucket. Strategy: keep `:source` + `:classification` and
   collapse the message to the leading phrase before the first `:`
   (e.g. `Path not found: /…/foo.clj` and `File not found: /…/bar.clj`
   become `\"Path not found\"` and `\"File not found\"`). Drops the
   varying tail that would otherwise scatter identical-cause errors
   across distinct buckets."
  [failure]
  (let [message (or (:message failure) "")
        head    (or (some-> (re-find #"^([^:\n]{1,80}):" message) second str/trim)
                  (let [trimmed (str/trim message)]
                    (subs trimmed 0 (min 60 (count trimmed)))))]
    [(:source failure) (:classification failure) head]))

(defn- repetition-clusters
  "Buckets of failures sharing a `repetition-signature`. Returns a vec
   of `{:signature .. :count .. :sample failure}` for clusters whose
   size meets `REPETITION_THRESHOLD`, sorted largest-first. Empty vec
   when nothing is repeating — caller treats `(seq …)` as the
   `:repetition-loop?` flag. Surfacing this gives the agent a single
   number to read instead of having to derive it from `:failures`."
  [failures]
  (->> (group-by repetition-signature failures)
    (keep (fn [[signature group]]
            (when (>= (count group) REPETITION_THRESHOLD)
              {:signature signature
               :count     (count group)
               :sample    (first group)})))
    (sort-by :count >)
    vec))

(defn- next-actions [failures clusters]
  (let [classes (set (map :classification failures))]
    (vec
      (cond-> []
        ;; Repetition loop is the loudest signal and goes first so
        ;; the agent reads it before any classification-specific tip.
        ;; Sample message is truncated; full failure stays in :failures.
        (seq clusters)
        (into
          (mapv (fn [{:keys [count sample]}]
                  (str "Same error repeated " count "× this turn (e.g. "
                    (preview (:message sample) 80)
                    "). STOP varying inputs to the failing call. "
                    "Switch strategy: list a parent directory, broaden "
                    "the search, or pivot — repeating the same shape "
                    "will not converge."))
            clusters))

        (contains? classes :provider-schema-rejected)
        (conj "Treat schema rejection as provider noise, not a reason to inspect SQLite. Use :raw-preview from foundation/failures and retry/switch model only if it repeats.")

        (contains? classes :regex-unsupported-escape)
        (conj (str "vis/rg now takes a non-empty vector of LITERAL substrings, not a regex string. "
                "Replace \"foo\\\\|bar\" with [\"foo\" \"bar\"]; PCRE metacharacters are auto-escaped. "
                "For genuine regex needs drop to (re-seq #\"…\" (slurp f))."))

        (contains? classes :regex-unescaped-quote)
        (conj "Fix the quoted regex string; an inner quote escaped poorly and exposed a bare symbol.")

        (contains? classes :patch-unbalanced-string)
        (conj "Re-emit vis/patch as a vector of {:path :search :replace} maps; compose multi-line :search/:replace with (str \"line\\n\" \"line\\n\") so each line stays on its own physical line and the closing quote stays visible.")

        (contains? classes :patch-no-match)
        (conj "Use any :near-match hint, then re-read the smallest file slice and emit an exact SEARCH block.")))))

(defn- foundation-diagnose
  "Compact current-turn diagnosis built from foundation/failures. Returns a
   map with counts, repetition-loop detection, and next actions so the
   agent can stop burning iterations on DB spelunking. Pass a
   conversation id to diagnose all turns in that conversation.

   `:repetition-loop?` is `true` when any error signature repeats at
   least `REPETITION_THRESHOLD` times in the failure list — the
   single-glance flag for the 'agent retried the same broken call N
   times' pathology. `:repetition-clusters` carries the supporting
   data (signature, count, sample failure)."
  ([env]
   (let [turn     (turn-snapshot env)
         failures (vec (:failures turn))
         clusters (repetition-clusters failures)]
     {:turn-id             (:id turn)
      :goal                (:goal turn)
      :status              (:status turn)
      :failure-count       (count failures)
      :by-classification   (classification-counts failures)
      :repetition-loop?    (boolean (seq clusters))
      :repetition-clusters clusters
      :failures            failures
      :next-actions        (next-actions failures clusters)}))
  ([env conversation-id]
   (let [failures (vec (foundation-failures env conversation-id))
         clusters (repetition-clusters failures)]
     {:conversation-id     conversation-id
      :failure-count       (count failures)
      :by-classification   (classification-counts failures)
      :repetition-loop?    (boolean (seq clusters))
      :repetition-clusters clusters
      :failures            failures
      :next-actions        (next-actions failures clusters)})))

;; ---------------------------------------------------------------------------
;; Extension catalog + README / doc access
;;
;; Every extension declares itself in a single classpath resource at
;; `META-INF/vis-extension/vis.edn`, an EDN map keyed by id
;; (`{<id-symbol> {:nses [...] :docs {<name> <body>}}}`). The id is the
;; same token the LLM uses as the SCI sandbox alias (`'foundation`, `'vis`,
;; etc.). Each doc descriptor is a map with `:description` (one-paragraph
;; summary) + `:content` (full Markdown body) as plain EDN strings.
;; `(foundation/extension-docs ...)` returns the descriptions (no `:content`)
;; so the LLM can scan the index before pulling a full body via
;; `(foundation/extension-doc ...)`.
;; See AGENTS.md ▸ "Every extension ships a single canonical README
;; in vis.edn".
;; ---------------------------------------------------------------------------

(defn- registered-extensions []
  (try (sdk/registered-extensions) (catch Throwable _ [])))

(defn- reference-as-symbol
  "Coerce an extension reference to the canonical id symbol used by
   the docs registry. Accepts the id symbol itself (`'foundation`), a
   keyword (`:foundation`), a string (`\"meta\"`), the alias-ns symbol
   (`'vis.ext.foundation`), or the full extension namespace (
   `'com.blockether.vis.ext.foundation.introspection`). Multi-segment symbols are
   resolved through the global extension registry."
  [reference]
  (cond
    (nil? reference) nil
    (keyword? reference) (clojure.core/symbol (name reference))
    (string? reference) (clojure.core/symbol reference)
    (symbol? reference)
    (if (namespace reference)
      (clojure.core/symbol (str reference))
      reference)
    :else nil))

(defn- extension-matches? [target extension]
  (let [ns-sym    (some-> (:ext/namespace extension) str)
        alias-sym (some-> (get-in extension [:ext/ns-alias :alias]) str)
        alias-ns  (some-> (get-in extension [:ext/ns-alias :ns]) str)]
    (boolean (some #(= target %) (remove nil? [ns-sym alias-sym alias-ns])))))

(defn- resolve-extension-id
  "Resolve `reference` to a registered extension id (symbol). Returns
   `nil` when no extension matches. Resolution order:
     1. The reference matches an id directly known to the docs
        registry (cheapest path).
     2. Otherwise, look up the extension by alias / alias-ns / full
        namespace through the global registry, then map its namespace
        back to the id via `extension-id-of-ns`."
  [reference]
  (when-let [target-sym (reference-as-symbol reference)]
    (let [target-str (str target-sym)]
      (or (when (contains? (set (sdk/registered-extension-ids)) target-sym)
            target-sym)
        (some (fn [extension]
                (when (extension-matches? target-str extension)
                  (or (get-in extension [:ext/ns-alias :alias])
                    (sdk/extension-id-of-ns (:ext/namespace extension)))))
          (registered-extensions))))))

(defn- extension-summary [extension]
  (let [ext-ns   (:ext/namespace extension)
        alias    (get-in extension [:ext/ns-alias :alias])
        id       (or alias (sdk/extension-id-of-ns ext-ns))
        doc-list (when id (sdk/extension-docs id))]
    (cond-> {:namespace ext-ns
             :symbols   (mapv :ext.symbol/sym (:ext/symbols extension))
             :docs      (or doc-list [])}
      alias                    (assoc :alias alias)
      (:ext/group extension)   (assoc :group   (:ext/group extension))
      (:ext/version extension) (assoc :version (:ext/version extension))
      (:ext/doc extension)     (assoc :doc     (:ext/doc extension)))))

(defn- foundation-extensions
  "Catalog every loaded extension as data. Returns a vector of
   `{:namespace :alias :group :version :doc :symbols :docs}` maps.
   `:docs` is a vector of `{:name :description}` descriptors for every
   doc the extension declares."
  [_env]
  (mapv extension-summary (registered-extensions)))

(defn- foundation-extension-docs
  "With one arg, return the doc catalog for one extension as a vector
   of `{:name :description}` descriptors. With no arg, return the full
   registry as `{<id-symbol> [<descriptor> ...]}`."
  ([_env]
   (try (sdk/extension-docs) (catch Throwable _ {})))
  ([_env reference]
   (when-let [id (resolve-extension-id reference)]
     (sdk/extension-docs id))))

(defn- foundation-extension-doc
  "Return the full descriptor map for one declared doc, by extension
   reference (id, alias, or full namespace) and doc name (e.g.
   \"README.md\"). The descriptor carries
   {:name :created-at :description :content :links :reflinks}; the
   Markdown body is at `:content`. Returns `nil` when the extension
   is not registered or declares no doc by that name."
  [_env reference doc-name]
  (when (string? doc-name)
    (when-let [id (resolve-extension-id reference)]
      (sdk/extension-doc id doc-name))))

(defn- foundation-extension-readme
  "Convenience: full Markdown body of an extension's canonical
   README. Equivalent to `(:content (foundation/extension-doc ref
   \"README.md\"))`. Every extension is required to declare a README
   in its `vis.edn`, so this returns text for any registered
   extension that follows the convention."
  [env reference]
  (:content (foundation-extension-doc env reference "README.md")))

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
  (sdk/symbol 'turn foundation-turn
    {:doc       (str "Snapshot of the current turn as a single map: "
                  "{:id :goal :status :attempts :errors :failures "
                  ":iteration :cost :redundancy :elapsed-ms}. The agent "
                  "reads keys directly: (filter :error (:attempts (foundation/turn))), etc.")
     :arglists  '([])
     :examples  ["(foundation/turn)"
                 "(count (:attempts (foundation/turn)))"
                 "(filter #(re-find #\"grep\" (:code %)) (:attempts (foundation/turn)))"]
     :before-fn inject-environment}))

(def conversation-symbol
  (sdk/symbol 'conversation foundation-conversation
    {:doc       (str "Conversation snapshot: {:id :channel :title :provider "
                  ":model :provider-model :created-at :turns :turn-count}. "
                  "`:provider-model` is a `\"provider/model\"` display string "
                  "(e.g. `\"openai/gpt-4o\"`); the raw `:provider` and `:model` "
                  "are kept as separate canonical keys. Default = current "
                  "conversation; pass an id to inspect any other. "
                  "The in-flight turn (= `CURRENT_QUERY_ID`) is auto-excluded "
                  "from `:turns` because its `:iteration-count` / `:total-cost` are "
                  "not finalized yet; the excluded id is returned as "
                  "`:in-flight-turn-id` so callers can opt into seeing it.")
     :arglists  '([] [conversation-id])
     :examples  ["(foundation/conversation)"
                 "(foundation/conversation \"3a7b2c…\")"
                 "(map :answer (:turns (foundation/conversation)))"]
     :before-fn inject-environment}))

(def conversations-symbol
  (sdk/symbol 'conversations foundation-conversations
    {:doc       (str "Vector of every known conversation, newest-first: "
                  "[{:id :channel :title :created-at :turn-count} …]. "
                  "No arg = all channels; pass :tui / :telegram / :cli / "
                  ":vis to filter to one.")
     :arglists  '([] [channel])
     :examples  ["(foundation/conversations)"
                 "(foundation/conversations :telegram)"
                 "(filter #(= :telegram (:channel %)) (foundation/conversations))"]
     :before-fn inject-environment}))

(def var-history-symbol
  (sdk/symbol 'var-history foundation-var-history
    {:doc       (str "Full version timeline for a var: "
                  "[{:value :code :version} …] oldest-first. Defaults to "
                  "the current conversation; pass an id to read from another.")
     :arglists  '([sym] [sym conversation-id])
     :examples  ["(foundation/var-history 'callers)"
                 "(foundation/var-history \"foo-fn\")"
                 "(foundation/var-history 'foo \"3a7b2c…\")"]
     :before-fn inject-environment}))

(def find-attempts-symbol
  (sdk/symbol 'find-attempts foundation-find-attempts
    {:doc       (str "Regex search over executed :code strings. One-arg "
                  "form scans the CURRENT TURN only; two-arg form scans "
                  "every turn of the given conversation. To scan every "
                  "conversation in the DB use `foundation/find-attempts-everywhere`. "
                  "Returns [{:turn-id :iteration-id :iteration :code :result :error} …] "
                  "— [] (never nil) when nothing matches.")
     :arglists  '([pattern] [pattern conversation-id])
     :examples  ["(foundation/find-attempts \"grep\")"
                 "(foundation/find-attempts #\"\\bdef\\b\" \"3a7b2c…\")"
                 "(map :code (foundation/find-attempts \"foo-fn\"))"]
     :before-fn inject-environment}))

(def conversation-forks-symbol
  (sdk/symbol 'conversation-forks foundation-conversation-forks
    {:doc       (str "Vector of every conversation_state row for the soul "
                  "behind `conversation-id`, oldest version first: "
                  "[{:state-id :version :parent-state-id :title :system-prompt "
                  ":provider :model :created-at :query-count} …]. "
                  "Trunk = :version 0 with :parent-state-id nil; forks point "
                  "at another :state-id in the same vector via :parent-state-id. "
                  "No-arg form = current conversation. Returns [] (never nil) "
                  "when the conversation has no rows or no fork tree.")
     :arglists  '([] [conversation-id])
     :examples  ["(foundation/conversation-forks)"
                 "(foundation/conversation-forks \"3a7b2c…\")"
                 "(group-by :parent-state-id (foundation/conversation-forks))"
                 "(filter :parent-state-id (foundation/conversation-forks))"]
     :before-fn inject-environment}))

(def query-retries-symbol
  (sdk/symbol 'query-retries meta-query-retries
    {:doc       (str "Vector of every query_state row (one per retry version) "
                  "for the soul behind `query-id`, oldest first: "
                  "[{:state-id :version :forked-from-query-state-id :status "
                  ":prior-outcome :provider :model :created-at :iteration-count} …]. "
                  "Version 0 = the original run; higher versions are retries "
                  "with :forked-from-query-state-id pointing at the previous :state-id. "
                  "`query-id` = the query_soul UUID surfaced as :turn-id by "
                  "find-attempts / :id by (foundation/turn) / :turns[].id by (foundation/conversation). "
                  "Returns [] (never nil) when the query has no rows.")
     :arglists  '([query-id])
     :examples  ["(foundation/query-retries (:id (foundation/turn)))"
                 "(foundation/query-retries \"3a7b2c…\")"
                 "(filter :forked-from-query-state-id (foundation/query-retries qid))"]
     :before-fn inject-environment}))

(def find-attempts-everywhere-symbol
  (sdk/symbol 'find-attempts-everywhere foundation-find-attempts-everywhere
    {:doc       (str "Regex search over executed :code strings across EVERY "
                  "conversation in the DB. Same hit shape as `find-attempts` "
                  "plus `:conversation-id`. Heavy: walks every persisted turn "
                  "+ iteration on disk — prefer `find-attempts` with a "
                  "specific id when you already know the target. Returns "
                  "[] (never nil) when nothing matches.")
     :arglists  '([pattern])
     :examples  ["(foundation/find-attempts-everywhere #\"Unmatched delimiter\")"
                 "(map :conversation-id (foundation/find-attempts-everywhere \"vis/rg\"))"]
     :before-fn inject-environment}))

(def failures-symbol
  (sdk/symbol 'failures foundation-failures
    {:doc       (str "Current-turn provider/schema and code/tool failures as "
                  "data. Includes classifications for schema rejections, "
                  "vis/rg escaping mistakes, malformed vis/patch payloads, "
                  "and SEARCH block misses. Pass a conversation id to scan "
                  "every turn of one conversation; use `foundation/failures-everywhere` "
                  "to scan every conversation in the DB. Returns [] (never nil) "
                  "when there is nothing to report.")
     :arglists  '([] [conversation-id])
     :examples  ["(foundation/failures)"
                 "(filter #(= :patch-no-match (:classification %)) (foundation/failures))"
                 "(foundation/failures \"3a7b2c…\")"]
     :before-fn inject-environment}))

(def failures-everywhere-symbol
  (sdk/symbol 'failures-everywhere foundation-failures-everywhere
    {:doc       (str "Provider/schema and code/tool failures across EVERY "
                  "conversation in the DB. Same shape as `failures` plus "
                  "`:conversation-id`. Heavy: walks every persisted turn + "
                  "iteration on disk — prefer `failures` with a specific id "
                  "when you already know the target. Returns [] (never nil) "
                  "when there is nothing to report.")
     :arglists  '([])
     :examples  ["(foundation/failures-everywhere)"
                 "(frequencies (map :classification (foundation/failures-everywhere)))"]
     :before-fn inject-environment}))

(def diagnose-symbol
  (sdk/symbol 'diagnose foundation-diagnose
    {:doc       (str "Summarize current-turn failures into counts, "
                  "repetition-loop detection, and next actions: "
                  "{:failure-count :by-classification :repetition-loop? "
                  ":repetition-clusters :failures :next-actions}. "
                  ":repetition-loop? = true when the same root-cause "
                  "error has fired "
                  REPETITION_THRESHOLD
                  "+ times this turn — read it before retrying. "
                  "Pass a conversation id to diagnose all turns. "
                  "First stop for stalled-agent triage, not raw "
                  "SQLite checks.")
     :arglists  '([] [conversation-id])
     :examples  ["(foundation/diagnose)"
                 "(:repetition-loop? (foundation/diagnose))"
                 "(:repetition-clusters (foundation/diagnose))"
                 "(:next-actions (foundation/diagnose))"
                 "(foundation/diagnose \"3a7b2c…\")"]
     :before-fn inject-environment}))

(def extensions-symbol
  (sdk/symbol 'extensions foundation-extensions
    {:doc       (str "Catalog of every loaded extension: "
                  "[{:namespace :alias :group :version :doc :symbols "
                  ":docs} …]. `:docs` is a vector of {:name :description} "
                  "descriptors for every doc the extension declares. "
                  "Use this to discover what surfaces are available "
                  "before reaching for a specific tool.")
     :arglists  '([])
     :examples  ["(foundation/extensions)"
                 "(map :namespace (foundation/extensions))"
                 "(map (juxt :alias :docs) (foundation/extensions))"]
     :before-fn inject-environment}))

(def extension-docs-symbol
  (sdk/symbol 'extension-docs foundation-extension-docs
    {:doc       (str "Doc index for an extension as a vector of "
                  "summaries: {:name :created-at :description :links "
                  ":reflinks} (no :content -- pull that with "
                  "foundation/extension-doc when needed). Scan descriptions "
                  "first to decide which full body is worth a fetch. "
                  "With no arg, returns the full registry keyed by "
                  "extension id symbol.")
     :arglists  '([] [extension-ref])
     :examples  ["(foundation/extension-docs)"
                 "(foundation/extension-docs 'foundation)"
                 "(map :description (foundation/extension-docs 'foundation))"
                 "(:links (first (foundation/extension-docs 'foundation)))"]
     :before-fn inject-environment}))

(def extension-doc-symbol
  (sdk/symbol 'extension-doc foundation-extension-doc
    {:doc       (str "Full descriptor map for one declared extension "
                  "doc: {:name :created-at :description :content :links "
                  ":reflinks}. The first arg is the extension "
                  "reference (id symbol, alias symbol/keyword, or full "
                  "extension namespace); the second arg is the doc "
                  "filename (e.g. \"README.md\"). The Markdown body is "
                  "at :content; :links are author-declared outgoing "
                  "references; :reflinks are auto-derived inbound "
                  "references. Returns nil when the extension is not "
                  "registered or declares no doc by that name.")
     :arglists  '([extension-ref doc-name])
     :examples  ["(foundation/extension-doc 'foundation \"README.md\")"
                 "(:content (foundation/extension-doc :vis \"README.md\"))"
                 "(:links       (foundation/extension-doc 'foundation \"README.md\"))"
                 "(:reflinks    (foundation/extension-doc 'foundation \"README.md\"))"]
     :before-fn inject-environment}))

(def extension-readme-symbol
  (sdk/symbol 'extension-readme foundation-extension-readme
    {:doc       (str "Convenience: full Markdown text of an extension's "
                  "canonical README. Equivalent to "
                  "`(foundation/extension-doc ref \"README.md\")`. The arg may "
                  "be the full namespace symbol, the alias symbol or "
                  "keyword, or the alias-ns symbol. Returns nil when the "
                  "extension is not registered or ships no README.")
     :arglists  '([extension-ref])
     :examples  ["(foundation/extension-readme 'foundation)"
                 "(foundation/extension-readme 'com.blockether.vis.ext.foundation.introspection)"
                 "(foundation/extension-readme :vis)"
                 "(println (foundation/extension-readme 'foundation))"]
     :before-fn inject-environment}))

(def all-symbols
  [turn-symbol
   conversation-symbol
   conversations-symbol
   conversation-forks-symbol
   var-history-symbol
   find-attempts-symbol
   find-attempts-everywhere-symbol
   failures-symbol
   failures-everywhere-symbol
   query-retries-symbol
   diagnose-symbol
   extensions-symbol
   extension-docs-symbol
   extension-doc-symbol
   extension-readme-symbol])

;; ---------------------------------------------------------------------------
;; Extension definition + global registration.
;; ---------------------------------------------------------------------------

(def extension
  (sdk/extension
    {:ext/namespace 'com.blockether.vis.ext.foundation.introspection
     :ext/doc       "Meta introspection: read your own turn, conversation, var history, attempts, failure diagnostics, and the canonical README + declared docs (with descriptions) of every loaded extension from inside :code. Returns plain maps and vectors for structural manipulation."
     :ext/version   "0.6.0"
     :ext/author    "Blockether"
     :ext/license   "Apache-2.0"
     :ext/ns-alias  {:ns 'vis.ext.foundation :alias 'foundation}
     :ext/group     "meta"
     :ext/prompt
     (str "`meta/` = READ-ONLY introspection. Returns maps/vecs. Never throws (errors -> nil/[]).\n"
       "  (foundation/turn)                       in-flight turn snapshot {:id :goal :status :attempts :errors :failures :iteration :cost :redundancy :elapsed-ms}\n"
       "  (foundation/conversation cid?)          conversation tree (turns/iterations/cost). AUTO-EXCLUDES in-flight turn (= CURRENT_QUERY_ID).\n"
       "  (foundation/conversations channel?)     list conversations\n"
       "  (foundation/conversation-forks cid?)    list every conversation_state row (trunk + forks)\n"
       "  (foundation/query-retries qid)          list every query_state row (original + retries)\n"
       "  (foundation/var-history 'sym)           prior versions of a SCI def\n"
       "  (foundation/find-attempts pat cid?)     grep prior :code attempts (current turn or one conversation)\n"
       "  (foundation/find-attempts-everywhere pat) grep :code across EVERY conversation in DB (heavy)\n"
       "  (foundation/failures cid?)              classify failed iterations (current turn or one conversation)\n"
       "  (foundation/failures-everywhere)        classify failed iterations across EVERY conversation (heavy)\n"
       "  (foundation/diagnose cid?)              {:repetition-loop? :repetition-clusters :by-classification :next-actions ...}\n"
       "  (foundation/extensions)                 loaded ext catalog\n"
       "  (foundation/extension-docs ext-ref)     declared doc summaries (no content)\n"
       "  (foundation/extension-doc ext-ref name) full doc descriptor incl. :content\n"
       "  (foundation/extension-readme ext-ref)   README :content shortcut\n"
       "\n"
       "Use when: stalled, malformed provider schema, vis/rg parse fail, vis/edit SEARCH miss, repeat fail, before guessing symbol names.")
     :ext/symbols   all-symbols}))

(sdk/register-extension! extension)
