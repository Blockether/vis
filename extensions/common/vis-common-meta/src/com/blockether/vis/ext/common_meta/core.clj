(ns com.blockether.vis.ext.common-meta.core
  "Meta extension — programmatic introspection of the agent's own
   state from inside `:code`. Ten functions, all returning maps or
   vectors so the agent can manipulate the data structurally:

   - `(meta/turn)`                       → current turn snapshot (one map)
   - `(meta/conversation [id])`          → current or specific conversation
   - `(meta/conversations [channel])`    → list every conversation, optionally filtered
   - `(meta/var-history sym [id])`       → version timeline for a var
   - `(meta/find-attempts pattern [id])` → regex search over executed code
   - `(meta/failures [id])`              → provider + code failures as data
   - `(meta/diagnose [id])`              → counts and next actions for stalled turns
   - `(meta/extensions)`                 → catalog of every loaded extension
   - `(meta/extension-docs [ref])`       → docs declared by an extension with descriptions
   - `(meta/extension-doc ref name)`     → full Markdown body of a declared doc
   - `(meta/extension-readme ref)`       → convenience for (extension-doc ref README.md)

   Every function is a pure read off the same DB tables the projection
   layer reads from (or a classpath read for the doc accessors).
   Failures return nil/[], never throw, so a misbehaving introspection
   call cannot break iteration execution.

   The agent gets the data once, manipulates it via plain Clojure
   (`(filter …)`, `(get-in turn [:plan :items])`, etc.) instead of
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
;; on the classpath surfaces in `(meta/conversations)` automatically —
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
  "UUID of the in-flight turn (= current query) or nil when no turn
   is running yet. Read from `:current-query-id-atom` set by
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
                    (sdk/db-list-iteration-expressions db-info iteration-id))
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
    (->> (sdk/db-list-iteration-expressions db-info (:id iteration))
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
      (when-let [conversation (sdk/db-get-conversation db-info conversation-id)]
        (let [queries (sdk/db-list-conversation-queries db-info conversation-id)
              turns (mapv (fn [query]
                            (cond-> {:id (:id query)
                                     :outcome (or (:prior-outcome query)
                                                (:status query))}
                              (:text query)       (assoc :goal       (:text query))
                              (:answer query)     (assoc :answer     (:answer query))
                              (:iterations query) (assoc :iterations (:iterations query))
                              (:total-cost query) (assoc :total-cost (:total-cost query))))
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
;; e.g. `(meta/turn)` with zero args.
;; ---------------------------------------------------------------------------

(defn- meta-turn [env]
  (turn-snapshot env))

(defn- meta-conversation
  "Snapshot for a conversation. The in-flight turn (= current `CURRENT_QUERY_ID`)
   is automatically excluded from `:turns` because its `:iterations` /
   `:total-cost` haven't been finalized yet — listing it would render
   as `null | $null` and confuse downstream summaries. The excluded id
   is surfaced as `:in-flight-turn-id` so callers can opt into seeing
   it. Filtering happens only when the in-flight query belongs to this
   conversation; foreign conversations are returned verbatim."
  ([env]
   (meta-conversation env (current-conversation-id env)))
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

(defn- meta-conversations
  "List every conversation the DB knows about, newest-first. With no
   arg, scans every channel surfaced by `known-channels`. With a
   channel kw, filters to that channel."
  ([env]
   (when (:db-info env)
     (vec
       (sort-by (comp #(if-let [c (:created-at %)]
                         (cond (inst? c)    (- (inst-ms c))
                           (integer? c) (- (long c))
                           :else        0)
                         0)
                  identity)
         (mapcat #(meta-conversations env %) (known-channels))))))
  ([env channel]
   (when (:db-info env)
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
       (vec (sdk/db-var-history (:db-info env) conversation-id (as-sym sym)))
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
           queries (try (sdk/db-list-conversation-queries (:db-info env) conversation-id)
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
   to scan every turn in that conversation."
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
           (sdk/db-list-conversation-queries (:db-info env) conversation-id)))
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
        (conj (str "vis/rg now takes a non-empty vector of LITERAL substrings, not a regex string. "
                "Replace \"foo\\\\|bar\" with [\"foo\" \"bar\"]; PCRE metacharacters are auto-escaped. "
                "For genuine regex needs drop to (re-seq #\"…\" (slurp f))."))

        (contains? classes :regex-unescaped-quote)
        (conj "Fix the quoted regex string; an inner quote escaped poorly and exposed a bare symbol.")

        (contains? classes :patch-unbalanced-string)
        (conj "Re-emit vis/patch as a vector of {:path :search :replace} maps; compose multi-line :search/:replace with (str \"line\\n\" \"line\\n\") so each line stays on its own physical line and the closing quote stays visible.")

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
;; Extension catalog + README / doc access
;;
;; Every extension declares itself in a single classpath resource at
;; `META-INF/vis-extension/vis.edn`, an EDN map keyed by id
;; (`{<id-symbol> {:nses [...] :docs {<name> <body>}}}`). The id is the
;; same token the LLM uses as the SCI sandbox alias (`'meta`, `'vis`,
;; etc.). Each doc descriptor is a map with `:description` (one-paragraph
;; summary) + `:content` (full Markdown body) as plain EDN strings.
;; `(meta/extension-docs ...)` returns the descriptions (no `:content`)
;; so the LLM can scan the index before pulling a full body via
;; `(meta/extension-doc ...)`.
;; See AGENTS.md ▸ "Every extension ships a single canonical README
;; in vis.edn".
;; ---------------------------------------------------------------------------

(defn- registered-extensions []
  (try (sdk/registered-extensions) (catch Throwable _ [])))

(defn- reference-as-symbol
  "Coerce an extension reference to the canonical id symbol used by
   the docs registry. Accepts the id symbol itself (`'meta`), a
   keyword (`:meta`), a string (`\"meta\"`), the alias-ns symbol
   (`'vis.ext.meta`), or the full extension namespace (
   `'com.blockether.vis.ext.common-meta.core`). Multi-segment symbols are
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

(defn- meta-extensions
  "Catalog every loaded extension as data. Returns a vector of
   `{:namespace :alias :group :version :doc :symbols :docs}` maps.
   `:docs` is a vector of `{:name :description}` descriptors for every
   doc the extension declares."
  [_env]
  (mapv extension-summary (registered-extensions)))

(defn- meta-extension-docs
  "With one arg, return the doc catalog for one extension as a vector
   of `{:name :description}` descriptors. With no arg, return the full
   registry as `{<id-symbol> [<descriptor> ...]}`."
  ([_env]
   (try (sdk/extension-docs) (catch Throwable _ {})))
  ([_env reference]
   (when-let [id (resolve-extension-id reference)]
     (sdk/extension-docs id))))

(defn- meta-extension-doc
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

(defn- meta-extension-readme
  "Convenience: full Markdown body of an extension's canonical
   README. Equivalent to `(:content (meta/extension-doc ref
   \"README.md\"))`. Every extension is required to declare a README
   in its `vis.edn`, so this returns text for any registered
   extension that follows the convention."
  [env reference]
  (:content (meta-extension-doc env reference "README.md")))

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
  (sdk/symbol 'turn meta-turn
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
  (sdk/symbol 'conversation meta-conversation
    {:doc       (str "Conversation snapshot: {:id :channel :title :provider "
                  ":model :provider-model :created-at :turns :turn-count}. "
                  "`:provider-model` is a `\"provider/model\"` display string "
                  "(e.g. `\"openai/gpt-4o\"`); the raw `:provider` and `:model` "
                  "are kept as separate canonical keys. Default = current "
                  "conversation; pass an id to inspect any other. "
                  "The in-flight turn (= `CURRENT_QUERY_ID`) is auto-excluded "
                  "from `:turns` because its `:iterations` / `:total-cost` are "
                  "not finalized yet; the excluded id is returned as "
                  "`:in-flight-turn-id` so callers can opt into seeing it.")
     :arglists  '([] [conversation-id])
     :examples  ["(meta/conversation)"
                 "(meta/conversation \"3a7b2c…\")"
                 "(map :answer (:turns (meta/conversation)))"]
     :before-fn inject-environment}))

(def conversations-symbol
  (sdk/symbol 'conversations meta-conversations
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
  (sdk/symbol 'var-history meta-var-history
    {:doc       (str "Full version timeline for a var: "
                  "[{:value :code :version} …] oldest-first. Defaults to "
                  "the current conversation; pass an id to read from another.")
     :arglists  '([sym] [sym conversation-id])
     :examples  ["(meta/var-history 'callers)"
                 "(meta/var-history \"foo-fn\")"
                 "(meta/var-history 'foo \"3a7b2c…\")"]
     :before-fn inject-environment}))

(def find-attempts-symbol
  (sdk/symbol 'find-attempts meta-find-attempts
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
  (sdk/symbol 'failures meta-failures
    {:doc       (str "Current-turn provider/schema and code/tool failures as "
                  "data. Includes classifications for schema rejections, "
                  "vis/rg escaping mistakes, malformed vis/patch payloads, "
                  "and SEARCH block misses. Pass a conversation id to scan "
                  "every turn.")
     :arglists  '([] [conversation-id])
     :examples  ["(meta/failures)"
                 "(filter #(= :patch-no-match (:classification %)) (meta/failures))"
                 "(meta/failures \"3a7b2c…\")"]
     :before-fn inject-environment}))

(def diagnose-symbol
  (sdk/symbol 'diagnose meta-diagnose
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

(def extensions-symbol
  (sdk/symbol 'extensions meta-extensions
    {:doc       (str "Catalog of every loaded extension: "
                  "[{:namespace :alias :group :version :doc :symbols "
                  ":docs} …]. `:docs` is a vector of {:name :description} "
                  "descriptors for every doc the extension declares. "
                  "Use this to discover what surfaces are available "
                  "before reaching for a specific tool.")
     :arglists  '([])
     :examples  ["(meta/extensions)"
                 "(map :namespace (meta/extensions))"
                 "(map (juxt :alias :docs) (meta/extensions))"]
     :before-fn inject-environment}))

(def extension-docs-symbol
  (sdk/symbol 'extension-docs meta-extension-docs
    {:doc       (str "Doc index for an extension as a vector of "
                  "summaries: {:name :created-at :description :links "
                  ":reflinks} (no :content -- pull that with "
                  "meta/extension-doc when needed). Scan descriptions "
                  "first to decide which full body is worth a fetch. "
                  "With no arg, returns the full registry keyed by "
                  "extension id symbol.")
     :arglists  '([] [extension-ref])
     :examples  ["(meta/extension-docs)"
                 "(meta/extension-docs 'meta)"
                 "(map :description (meta/extension-docs 'meta))"
                 "(:links (first (meta/extension-docs 'meta)))"]
     :before-fn inject-environment}))

(def extension-doc-symbol
  (sdk/symbol 'extension-doc meta-extension-doc
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
     :examples  ["(meta/extension-doc 'meta \"README.md\")"
                 "(:content (meta/extension-doc :vis \"README.md\"))"
                 "(:links       (meta/extension-doc 'meta \"README.md\"))"
                 "(:reflinks    (meta/extension-doc 'meta \"README.md\"))"]
     :before-fn inject-environment}))

(def extension-readme-symbol
  (sdk/symbol 'extension-readme meta-extension-readme
    {:doc       (str "Convenience: full Markdown text of an extension's "
                  "canonical README. Equivalent to "
                  "`(meta/extension-doc ref \"README.md\")`. The arg may "
                  "be the full namespace symbol, the alias symbol or "
                  "keyword, or the alias-ns symbol. Returns nil when the "
                  "extension is not registered or ships no README.")
     :arglists  '([extension-ref])
     :examples  ["(meta/extension-readme 'meta)"
                 "(meta/extension-readme 'com.blockether.vis.ext.common-meta.core)"
                 "(meta/extension-readme :vis)"
                 "(println (meta/extension-readme 'meta))"]
     :before-fn inject-environment}))

(def all-symbols
  [turn-symbol
   conversation-symbol
   conversations-symbol
   var-history-symbol
   find-attempts-symbol
   failures-symbol
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
    {:ext/namespace 'com.blockether.vis.ext.common-meta.core
     :ext/doc       "Meta introspection: read your own turn, conversation, var history, attempts, failure diagnostics, and the canonical README + declared docs (with descriptions) of every loaded extension from inside :code. Returns plain maps and vectors for structural manipulation."
     :ext/version   "0.5.0"
     :ext/author    "Blockether"
     :ext/license   "Apache-2.0"
     :ext/ns-alias  {:ns 'vis.ext.meta :alias 'meta}
     :ext/group     "meta"
     :ext/prompt
     (str "`meta/` = READ-ONLY introspection. Returns maps/vecs. Never throws (errors -> nil/[]).\n"
       "  (meta/turn)                       in-flight turn snapshot {:id :goal :plan :breadcrumbs :attempts :errors :iteration :cost :elapsed-ms}\n"
       "  (meta/conversation cid?)          conversation tree (turns/iterations/cost). AUTO-EXCLUDES in-flight turn (= CURRENT_QUERY_ID).\n"
       "  (meta/conversations channel?)     list conversations\n"
       "  (meta/var-history 'sym)           prior versions of a SCI def\n"
       "  (meta/find-attempts pat cid?)     grep prior :code attempts\n"
       "  (meta/failures cid?)              classify failed iterations\n"
       "  (meta/diagnose cid?)              {:stall? :loops? ...} diagnostic summary\n"
       "  (meta/extensions)                 loaded ext catalog\n"
       "  (meta/extension-docs ext-ref)     declared doc summaries (no content)\n"
       "  (meta/extension-doc ext-ref name) full doc descriptor incl. :content\n"
       "  (meta/extension-readme ext-ref)   README :content shortcut\n"
       "\n"
       "Use when: stalled, malformed provider schema, vis/rg parse fail, vis/edit SEARCH miss, repeat fail, before guessing symbol names. Plan/breadcrumbs already in <plan>/<breadcrumbs>/<system_state> — use those, don't fetch.")
     :ext/symbols   all-symbols}))

(sdk/register-extension! extension)
