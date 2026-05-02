(ns com.blockether.vis.ext.foundation.introspection
  "Programmatic introspection of the agent's own state from inside
   `:code`. The public state surface is deliberately small:

   - `(v/inspect [conversation-id])` → canonical data map, including raw LLM diagnostics
   - `(v/report [conversation-id])`  → Markdown rendered from that data
   - `(v/provenance-timeline [conversation-id])` → ordered eval/tool provenance events
   - `(v/provenance-stats [conversation-id])`    → ops/status/failure/slow summaries
   - `(v/provenance-guards [conversation-id])`   → provenance integrity checks
   - `(v/provenance-report [conversation-id])`   → compact Markdown audit trail
   - `(v/intent! opts)` / `(v/plan! opts)` / `(v/gate! opts)`
     persist turn-scoped work-state versions
   - `(v/attest! gate opts)` / `(v/block-gate! gate opts)`
     persist exactly one attestation for exactly one gate version
   - `(v/work-state)` / `(v/gates)` / `(v/attestations)` /
     `(v/gate-checks)` / `(v/gate-report)` inspect/enforce gates

   Everything else in this namespace is implementation behind that
   deeper interface. The agent gets the data once, manipulates it via
   plain Clojure (`get-in`, `filter`, `map`, etc.), and renders the
   same data to Markdown only when presentation is needed.

   Extension catalog/doc access remains public because it is discovery,
   not conversation-state introspection:

   - `(v/extensions)`                 → catalog of every loaded extension
   - `(v/extension-docs [ref])`       → docs declared by an extension with descriptions
   - `(v/extension-doc ref name)`     → full Markdown body of a declared doc
   - `(v/extension-readme ref)`       → convenience for (extension-doc ref README.md)

   Every function is a pure read off the same DB tables the projection
   layer reads from (or a classpath read for the doc accessors).
   Failures return nil/[], never throw, so a misbehaving introspection
   call cannot break iteration execution.

   Opt-in: not auto-loaded by default. Add this jar to the classpath
   to enable."
  (:require
   [charred.api :as json]
   [clojure.string :as str]
   [com.blockether.vis.core :as vis]
   [com.blockether.vis.ext.foundation.transcript :as transcript]))

;; ---------------------------------------------------------------------------
;; Channels we know how to enumerate. Derived from the global channel
;; registry (`vis/registered-channels`) so any third-party channel jar
;; on the classpath surfaces in the inspect conversation index automatically —
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
  (->> (vis/registered-channels)
    (map :channel/id)
    (cons :cli)
    distinct
    vec))

;; ---------------------------------------------------------------------------
;; Helpers — derive ids, deref atoms, normalize sym args.
;; ---------------------------------------------------------------------------

(defn- safe-deref [a]
  (when a (try (deref a) (catch Throwable _ nil))))

(defn- current-conversation-id [env]
  (:conversation-id env))

(defn- current-conversation-turn-id
  "UUID of the in-flight turn, or nil when no turn is running yet.

   Internally this is the `conversation_turn_soul` id — hence the
   `:current-conversation-turn-id-atom` env key and this helper's `conversation-turn-id`
   suffix. The product concept is *turn*; everywhere this id is
   surfaced to the model (e.g. inspect current-turn results, the
   `:in-flight-turn-id` slot) it's labelled `turn`. Mirrors the
   SCI-visible `TURN_CONVERSATION_TURN_ID` SYSTEM var so meta-fns can filter
   it without round-tripping through SCI."
  [env]
  (some-> (:current-conversation-turn-id-atom env) deref))

(defn- same-uuid?
  "True when two values denote the same UUID. Accepts UUID instances
   or any object whose `str` is the canonical UUID form. Used to
   match `TURN_CONVERSATION_TURN_ID` against a turn's `:id` regardless of
   whether the persistence layer returned a UUID or a string."
  [a b]
  (and a b (= (str a) (str b))))

(defn- iteration-rows
  "Fetch the iteration rows for `conversation-turn-id`; returns [] on any failure."
  [db-info conversation-turn-id]
  (try
    (vis/db-list-conversation-turn-iterations db-info conversation-turn-id)
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
   expression. Used by the current-turn snapshot and by attempt search."
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
                    (vis/db-list-iteration-blocks db-info iteration-id))
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

(defn- turn-cost-summary
  "Pull the token / cost / provider / model map persisted on
   `conversation_turn_state.metadata` (with `llm_root_provider` /
   `llm_root_model` as the canonical typed columns). Returns a map
   with the :input-tokens / :output-tokens / :total-cost / :provider
   / :model / :provider-model keys when present, or an empty map.
   Never throws.

   `:provider-model` is a derived `\"provider/model\"` display string
   (e.g. `\"openai/gpt-4o\"`) so callers render it directly — the
   canonical data still lives in `:provider` and `:model` separately."
  [turn]
  (let [provider-model (format-provider-model (:provider turn) (:model turn))]
    (cond-> {}
      (:input-tokens turn)     (assoc :input-tokens     (:input-tokens turn))
      (:output-tokens turn)    (assoc :output-tokens    (:output-tokens turn))
      (:reasoning-tokens turn) (assoc :reasoning-tokens (:reasoning-tokens turn))
      (:cached-tokens turn)    (assoc :cached-tokens    (:cached-tokens turn))
      (:total-cost turn)       (assoc :total-cost       (:total-cost turn))
      (:provider turn)         (assoc :provider         (:provider turn))
      (:model turn)            (assoc :model            (:model turn))
      provider-model           (assoc :provider-model   provider-model))))

(defn- elapsed-ms
  "Wall-clock duration for a turn in milliseconds. Read from
   `:duration-ms` when persisted; otherwise computed from the
   underlying turn row's `:created-at` so the model can self-pace
   mid-turn."
  [turn]
  (or (:duration-ms turn)
    (when-let [created (:created-at turn)]
      (try
        (- (System/currentTimeMillis)
          (cond
            (inst? created)    (inst-ms created)
            (integer? created) (long created)
            :else              0))
        (catch Throwable _ nil)))))

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
      (and (str/includes? tool-name "v/rg")
        (str/includes? lower-message "unsupported escape character"))
      :regex-unsupported-escape

      (and (str/includes? tool-name "v/rg")
        (str/includes? lower-message "unable to resolve symbol"))
      :regex-unescaped-quote

      (and (str/includes? tool-name "v/patch")
        (str/includes? lower-message "unmatched delimiter"))
      :patch-unbalanced-string

      (and (str/includes? tool-name "v/patch")
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
    "Provider returned prose/string instead of the iteration map. Skip the SQLite trip — the raw preview is already here. Continue after the built-in schema retry, or switch model when this repeats."

    :regex-unsupported-escape
    "Clojure strings reject \\| as an escape. Use bare | for regex alternation inside a string, or switch to a #\"…\" regex literal for complex patterns."

    :regex-unescaped-quote
    "The regex string likely contains an unescaped inner quote. Escape it as \\\" or use a regex literal / simpler pattern."

    :patch-unbalanced-string
    "The v/patch EDN payload likely lost the closing quote of a :search or :replace string. Re-emit as the canonical vector shape (v/patch [{:path :search :replace} ...]) and compose multi-line content with (str \"line1\\n\" \"line2\\n\") so each line stays on its own physical line and the closing quote stays visible."

    :patch-no-match
    "The SEARCH text did not match the file exactly. Use the near-match data when present, or re-read the smallest file slice and emit an exact-byte SEARCH block."

    :unresolved-symbol
    "A reader/string boundary probably split the form and exposed a bare symbol. Check quoting before retrying."

    "Read :message, :code, and :iteration; fix the smallest failing form before issuing new searches."))

(defn- expression-failures-for-iteration [db-info iteration]
  (try
    (->> (vis/db-list-iteration-blocks db-info (:id iteration))
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

(defn- latest-turn [db-info conversation-id]
  (when (and db-info conversation-id)
    (last (try (vis/db-list-conversation-turns db-info conversation-id)
            (catch Throwable _ [])))))

(defn- turn-snapshot
  "The single-call rich current-turn snapshot. Aggregates
   the per-iteration data the prompt projection does NOT carry
   (attempts, provider/code failures, cost, elapsed-ms) plus the
   iteration pointer. The agent picks what it needs by map key
   instead of querying SQLite manually."
  [env]
  (let [{:keys [db-info conversation-id]} env]
    (when-let [turn (latest-turn db-info conversation-id)]
      (let [iterations (iteration-rows db-info (:id turn))
            attempts   (attempts-from-iterations db-info iterations)]
        (cond-> {:id           (:id turn)
                 :user-request (:user-request turn)
                 :status       (:status turn)
                 :attempts     attempts
                 :errors       (filterv :error attempts)
                 :failures     (failures-from-iterations db-info iterations)
                 :iteration    (iteration-pointer env)
                 :cost         (turn-cost-summary turn)}
          (elapsed-ms turn) (assoc :elapsed-ms (elapsed-ms turn)))))))

(defn- conversation-snapshot
  "Map for a single conversation: identity + every turn rolled up to a
   compact `{:id :user-request :outcome :answer :iteration-count :status}`
   shape. Used by the conversation-summary portion of inspect.

   `:iteration-count` is the integer number of LLM rounds the turn
   consumed. Spelled out so it never gets confused with the vector
   shape that the runtime trace uses for per-iteration entries."
  [db-info conversation-id]
  (when (and db-info conversation-id)
    (try
      (when-let [conversation (vis/db-get-conversation db-info conversation-id)]
        (let [turn-rows (vis/db-list-conversation-turns db-info conversation-id)
              turns (mapv (fn [turn]
                            (cond-> {:id (:id turn)
                                     :outcome (or (:prior-outcome turn)
                                                (:status turn))}
                              (:user-request turn)     (assoc :user-request    (:user-request turn))
                              (:answer turn)           (assoc :answer          (:answer turn))
                              (:iteration-count turn)  (assoc :iteration-count (:iteration-count turn))
                              (:total-cost turn)       (assoc :total-cost      (:total-cost turn))))
                      turn-rows)]
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
;; e.g. current-turn snapshot with zero args.
;; ---------------------------------------------------------------------------

(defn- foundation-turn [env]
  (turn-snapshot env))

(defn- foundation-conversation
  "Snapshot for a conversation. The in-flight turn (= current `TURN_CONVERSATION_TURN_ID`)
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
     (let [in-flight-id (current-conversation-turn-id env)
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
                     turns (try (vis/db-list-conversation-turns (:db-info env) conversation-id)
                             (catch Throwable _ []))]
                 (cond-> {:id          conversation-id
                          :channel     (:channel conversation)
                          :title       (:title conversation)
                          :created-at  (:created-at conversation)
                          :turn-count  (count turns)}
                   (:external-id conversation) (assoc :external-id (:external-id conversation)))))
         (vis/db-list-conversations (:db-info env) channel))
       (catch Throwable _ []))
     [])))

(defn- foundation-conversation-forks
  "List every `conversation_state` row for the conversation soul behind
   `conversation-id`, oldest version first. Each row maps to
   `{:state-id :version :parent-state-id :title :system-prompt :provider
     :model :created-at :turn-count}`. The trunk is `:version 0` with
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
       (vec (vis/db-list-conversation-states (:db-info env) conversation-id))
       (catch Throwable _ []))
     [])))

(defn- meta-turn-retries
  "List every `conversation_turn_state` row (= every retry version) for the turn
   soul behind `conversation-turn-id`, oldest version first. Each row maps to
   `{:state-id :version :forked-from-conversation-turn-state-id :status :prior-outcome
     :provider :model :created-at :iteration-count}`. Version 0 with
   `:forked-from-conversation-turn-state-id nil` is the original run; any higher
   version is a retry. `conversation-turn-id` is a `conversation_turn_soul` UUID — the same id
   surfaced as `:turn-id` by attempt search, `:id` by the current-turn
   snapshot, or `:turns[].id` by the conversation summary. Returns `[]` (never nil)
   when the turn is unknown or the env is missing handles."
  [env conversation-turn-id]
  (if (and (:db-info env) conversation-turn-id)
    (try
      (vec (vis/db-list-conversation-turn-states (:db-info env) conversation-turn-id))
      (catch Throwable _ []))
    []))

(defn- foundation-failures
  "Provider/schema and code/tool failures, normalized into one
   chronological vector. No arg = current turn. Pass a conversation id
   to scan every turn in that conversation. To scan EVERY conversation
   in the DB use the DB-wide helper instead. Returns `[]`
   (never nil) when there is nothing to report or the env is missing
   handles."
  ([env]
   (or (:failures (turn-snapshot env)) []))
  ([env conversation-id]
   (if (and (:db-info env) conversation-id)
     (try
       (vec
         (mapcat (fn [turn]
                   (let [iterations (iteration-rows (:db-info env) (:id turn))]
                     (mapv #(assoc % :turn-id (:id turn)
                              :user-request (:user-request turn))
                       (failures-from-iterations (:db-info env) iterations))))
           (vis/db-list-conversation-turns (:db-info env) conversation-id)))
       (catch Throwable _ []))
     [])))

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
        (conj "Treat schema rejection as provider noise, not a reason to inspect SQLite. Use :raw-preview from (:failures (v/inspect)) and retry/switch model only if it repeats.")

        (contains? classes :regex-unsupported-escape)
        (conj (str "v/rg now takes a non-empty vector of LITERAL substrings, not a regex string. "
                "Replace \"foo\\\\|bar\" with [\"foo\" \"bar\"]; PCRE metacharacters are auto-escaped. "
                "For genuine regex needs drop to (re-seq #\"…\" (slurp f))."))

        (contains? classes :regex-unescaped-quote)
        (conj "Fix the quoted regex string; an inner quote escaped poorly and exposed a bare symbol.")

        (contains? classes :patch-unbalanced-string)
        (conj "Re-emit v/patch as a vector of {:path :search :replace} maps; compose multi-line :search/:replace with (str \"line\\n\" \"line\\n\") so each line stays on its own physical line and the closing quote stays visible.")

        (contains? classes :patch-no-match)
        (conj "Use any :near-match hint, then re-read the smallest file slice and emit an exact SEARCH block.")))))

(defn- foundation-diagnose
  "Compact current-turn diagnosis built from failure data. Returns a
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
      :user-request        (:user-request turn)
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

(defn- safe-call
  [f default]
  (try
    (let [v (f)]
      (if (nil? v) default v))
    (catch Throwable _ default)))

(defn- retries-by-turn
  [env turns]
  (into {}
    (keep (fn [{:keys [id]}]
            (when id
              [id (meta-turn-retries env id)])))
    turns))

(defn- raw-response-map
  [iteration]
  (let [blocks (when (seq (:llm-executable-blocks iteration))
                 (vec (:llm-executable-blocks iteration)))]
    (cond-> {}
      (some? (:llm-raw-response-preview iteration))
      (assoc :preview (:llm-raw-response-preview iteration))
      (some? (:llm-raw-response-length iteration))
      (assoc :length (:llm-raw-response-length iteration))
      (some? (:llm-raw-response-sha256 iteration))
      (assoc :sha256 (:llm-raw-response-sha256 iteration))
      (some? (:llm-executable-code iteration))
      (assoc :executable-code (:llm-executable-code iteration))
      blocks
      (assoc :executable-blocks blocks
        :block-count (count blocks)
        :block-langs (mapv :lang blocks)))))

(defn- llm-diagnostic-row
  [turn iteration]
  (let [raw-response (raw-response-map iteration)]
    (when (seq raw-response)
      (cond-> {:turn-id      (:id turn)
               :user-request (:user-request turn)
               :iteration-id (:id iteration)
               :iteration    (:position iteration)
               :status       (:status iteration)
               :raw-response raw-response}
        (:provider iteration) (assoc :provider (:provider iteration))
        (:model iteration)    (assoc :model (:model iteration))))))

(defn- llm-diagnostics
  "Flatten the full transcript into the raw LLM diagnostics view exposed
   by `v/inspect`. This is a convenience index over the canonical
   transcript payload, not another storage read."
  [transcript-data]
  (vec
    (mapcat (fn [turn]
              (keep #(llm-diagnostic-row turn %) (:iterations turn)))
      (:turns transcript-data))))

(defn- foundation-inspect
  "Canonical conversation-state data surface. One read returns the
   navigation summary, live current turn, classified failures,
   diagnosis, fork/retry metadata, raw LLM diagnostics, and the full
   transcript payload. Default target is the current conversation;
   pass a conversation id or unambiguous prefix to inspect another
   conversation."
  ([env]
   (foundation-inspect env (:conversation-id env)))
  ([env conversation-id]
   (let [target-id            (or conversation-id (:conversation-id env))
         transcript-data      (safe-call #(transcript/transcript (:db-info env) target-id) nil)
         resolved-id          (or (get-in transcript-data [:conversation :id]) target-id)
         conversation-summary (safe-call #(foundation-conversation env resolved-id) nil)
         failures             (safe-call #(foundation-failures env resolved-id) [])
         diagnosis            (safe-call #(foundation-diagnose env resolved-id) {})
         forks                (safe-call #(foundation-conversation-forks env resolved-id) [])
         turn-retries         (safe-call #(retries-by-turn env (:turns transcript-data)) {})]
     {:schema-version      1
      :scope               :conversation
      :conversation-id     resolved-id
      :conversation-index  (safe-call #(foundation-conversations env) [])
      :conversation        conversation-summary
      :current-turn        (safe-call #(foundation-turn env) nil)
      :failures            failures
      :diagnosis           diagnosis
      :conversation-forks  forks
      :turn-retries        turn-retries
      :llm-diagnostics    (safe-call #(llm-diagnostics transcript-data) [])
      :transcript          transcript-data})))

(defn- foundation-report
  "Render the same canonical data returned by `foundation-inspect` as
   Markdown. Returns a string and never throws."
  ([env]
   (foundation-report env (:conversation-id env)))
  ([env conversation-id]
   (let [data (foundation-inspect env conversation-id)]
     (if-let [transcript-data (:transcript data)]
       (transcript/transcript->md transcript-data)
       (str "Conversation not found: " (:conversation-id data) "\n")))))

;; ---------------------------------------------------------------------------
;; Provenance timeline + guards — model-facing audit helpers.
;; ---------------------------------------------------------------------------

(defn- tool-result-envelope? [value]
  (and (map? value)
    (contains? value :ok?)
    (contains? value :provenance)
    (contains? value :markdown)))

(defn- event-status [error ok?]
  (cond
    error :error
    (false? ok?) :error
    :else :ok))

(defn- block-index [block]
  (or (:idx block) (:id block) 0))

(defn- block-ref [iteration-position block]
  (or (get-in block [:provenance :ref])
    (str "i" iteration-position "." (inc (long (block-index block))))))

(defn- eval-event [conversation-id turn iteration block]
  (let [provenance (:provenance block)
        iteration-position (:position iteration)
        ref (block-ref iteration-position block)
        status (event-status (:error block) true)]
    (cond-> {:kind            :eval
             :ref             ref
             :conversation-id conversation-id
             :turn-id         (:id turn)
             :iteration-id    (:id iteration)
             :iteration       iteration-position
             :form-position   (or (:form-position provenance)
                                (inc (long (block-index block))))
             :form-count      (:form-count provenance)
             :code            (:code block)
             :op              (:op provenance)
             :engine          (:engine provenance)
             :status          status
             :duration-ms     (or (:duration-ms provenance)
                                (:execution-time-ms block)
                                (:duration-ms block)
                                0)
             :links           {:conversation conversation-id
                               :turn         (:id turn)
                               :iteration    (:id iteration)
                               :form         ref}}
      (:error block)     (assoc :error (:error block))
      (:timeout? block)  (assoc :timeout? true)
      (:repaired? block) (assoc :repaired? true)
      provenance         (assoc :provenance provenance))))

(defn- tool-event [conversation-id turn iteration block]
  (let [value (:result block)]
    (when (tool-result-envelope? value)
      (let [parent-ref (block-ref (:position iteration) block)
            provenance (:provenance value)
            status (event-status (:error value) (:ok? value))]
        (cond-> {:kind            :tool
                 :ref             (str parent-ref "/tool")
                 :parent-ref      parent-ref
                 :conversation-id conversation-id
                 :turn-id         (:id turn)
                 :iteration-id    (:id iteration)
                 :iteration       (:position iteration)
                 :form-position   (get-in block [:provenance :form-position])
                 :op              (:op provenance)
                 :status          status
                 :duration-ms     (or (:duration-ms provenance) 0)
                 :markdown        (:markdown value)
                 :links           {:conversation conversation-id
                                   :turn         (:id turn)
                                   :iteration    (:id iteration)
                                   :form         parent-ref
                                   :parent       parent-ref}}
          (:tool provenance)   (assoc :tool (:tool provenance))
          (:target provenance) (assoc :target (:target provenance))
          (:error value)       (assoc :error (:error value))
          provenance           (assoc :provenance provenance))))))

(defn- transcript-for-provenance [env conversation-id]
  (safe-call #(transcript/transcript (:db-info env) (or conversation-id (:conversation-id env))) nil))

(defn- foundation-provenance-timeline
  "Ordered provenance ledger for one conversation. Emits one `:eval`
   event for every executed top-level form, plus one child `:tool`
   event whenever that form returned a tool-result envelope."
  ([env]
   (foundation-provenance-timeline env (:conversation-id env)))
  ([env conversation-id]
   (let [data (transcript-for-provenance env conversation-id)
         conversation-id (get-in data [:conversation :id])]
     (if-not data
       []
       (vec
         (mapcat (fn [turn]
                   (mapcat (fn [iteration]
                             (mapcat (fn [block]
                                       (let [tool (tool-event conversation-id turn iteration block)]
                                         (cond-> [(eval-event conversation-id turn iteration block)]
                                           tool (conj tool))))
                               (:blocks iteration)))
                     (:iterations turn)))
           (:turns data)))))))

(defn- foundation-provenance-stats
  "Roll up a provenance timeline into counts and audit-focused slices."
  ([env]
   (foundation-provenance-stats env (:conversation-id env)))
  ([env conversation-id]
   (let [timeline (foundation-provenance-timeline env conversation-id)]
     {:event-count (count timeline)
      :by-kind     (frequencies (map :kind timeline))
      :by-op       (frequencies (map :op timeline))
      :by-engine   (frequencies (keep :engine timeline))
      :by-status   (frequencies (map :status timeline))
      :failures    (filterv #(= :error (:status %)) timeline)
      :slowest     (vec (take 5 (sort-by (comp - long #(or % 0) :duration-ms) timeline)))
      :tools       (filterv #(= :tool (:kind %)) timeline)})))

(defn- provenance-guard-violations [timeline]
  (let [refs       (keep :ref timeline)
        ref-counts (frequencies refs)
        ref-set    (set refs)]
    (vec
      (concat
        (mapcat (fn [event]
                  (let [expected-ref (when (= :eval (:kind event))
                                       (str "i" (:iteration event) "." (:form-position event)))]
                    (cond-> []
                      (nil? (:ref event))
                      (conj {:type :missing-ref :event event})

                      (nil? (:op event))
                      (conj {:type :missing-op :ref (:ref event)})

                      (nil? (:status event))
                      (conj {:type :missing-status :ref (:ref event)})

                      (and (= :error (:status event)) (nil? (:error event)))
                      (conj {:type :failed-event-without-error :ref (:ref event)})

                      (and expected-ref (not= expected-ref (:ref event)))
                      (conj {:type :eval-ref-mismatch
                             :ref (:ref event)
                             :expected expected-ref})

                      (and (= :tool (:kind event))
                        (not (contains? ref-set (:parent-ref event))))
                      (conj {:type :tool-parent-missing
                             :ref (:ref event)
                             :parent-ref (:parent-ref event)})

                      (and (= :eval (:kind event))
                        (nil? (:provenance event)))
                      (conj {:type :eval-missing-provenance :ref (:ref event)}))))
          timeline)
        (keep (fn [[ref n]]
                (when (> n 1)
                  {:type :duplicate-ref :ref ref :count n}))
          ref-counts)))))

(defn- foundation-provenance-guards
  "Integrity checks over the provenance ledger. Returns data, never
   throws, so the model can gate answers on `:ok?` before citing refs."
  ([env]
   (foundation-provenance-guards env (:conversation-id env)))
  ([env conversation-id]
   (let [timeline   (foundation-provenance-timeline env conversation-id)
         violations (provenance-guard-violations timeline)]
     {:ok?        (empty? violations)
      :checked    {:events (count timeline)
                   :refs   (count (set (keep :ref timeline)))}
      :violations violations})))

(defn- foundation-provenance-report
  "Compact Markdown audit trail suitable for inclusion in an answer."
  ([env]
   (foundation-provenance-report env (:conversation-id env)))
  ([env conversation-id]
   (let [timeline (foundation-provenance-timeline env conversation-id)
         stats    (foundation-provenance-stats env conversation-id)
         guards   (foundation-provenance-guards env conversation-id)]
     (str "## Provenance\n\n"
       "- Events: " (:event-count stats) "\n"
       "- Status: " (pr-str (:by-status stats)) "\n"
       "- Guards: " (if (:ok? guards) "ok" (str "failed " (count (:violations guards)) " check(s)")) "\n\n"
       (if (seq timeline)
         (str/join "\n"
           (map (fn [{:keys [ref parent-ref kind op engine status duration-ms error]}]
                  (str "- `" ref "` " (name kind) " " (pr-str op)
                    (when parent-ref (str " parent `" parent-ref "`"))
                    (when engine (str " via " (pr-str engine)))
                    " → " (name status) ", " (or duration-ms 0) "ms"
                    (when error (str " — " (preview (error-text error) 180)))))
             timeline))
         "_No provenance events._")
       "\n"))))

;; ---------------------------------------------------------------------------
;; Completion contract — turn-scoped intent -> plan -> gate -> attestation.
;; `work-state` remains the low-level projection name; model-facing docs
;; should prefer "contract" for the domain concept.
;; ---------------------------------------------------------------------------

(defn- current-turn-work-state
  [env]
  (safe-call #(vis/db-work-state (:db-info env) (current-conversation-turn-id env))
    {:intents [] :plans [] :gates [] :attestations []}))

(defn- latest-by-soul
  [xs]
  (->> xs
    (group-by :soul-id)
    vals
    (mapv #(last (sort-by :version %)))))

(defn- current-plan-states
  [work-state]
  (let [active-intent-ids (->> (:intents work-state)
                            latest-by-soul
                            (filter #(= :active (:status %)))
                            (map :id)
                            set)]
    (->> (:plans work-state)
      latest-by-soul
      (filter #(and (= :active (:status %))
                 (contains? active-intent-ids (:intent-state-id %))))
      vec)))

(defn- current-gate-states
  [work-state]
  (let [active-plan-ids (set (map :id (current-plan-states work-state)))]
    (->> (:gates work-state)
      latest-by-soul
      (filter #(contains? active-plan-ids (:plan-state-id %)))
      vec)))

(defn- attestations-by-gate
  [work-state]
  (group-by :gate-state-id (:attestations work-state)))

(defn- current-turn-provenance-timeline
  [env]
  (let [turn-id (current-conversation-turn-id env)]
    (->> (foundation-provenance-timeline env (:conversation-id env))
      (filter #(same-uuid? (:turn-id %) turn-id))
      vec)))

(defn- resolve-current-gate-state-id
  [work-state gate]
  (cond
    (uuid? gate) gate
    (string? gate) (or (some #(when (= (str (:id %)) gate) (:id %)) (:gates work-state))
                     (some #(when (= (:key %) (keyword gate)) (:id %)) (current-gate-states work-state)))
    (keyword? gate) (some #(when (= (:key %) gate) (:id %)) (current-gate-states work-state))
    (map? gate) (or (:gate-state-id gate) (:id gate))
    :else gate))

(defn- foundation-work-state
  ([env]
   (current-turn-work-state env))
  ([env conversation-turn-id]
   (safe-call #(vis/db-work-state (:db-info env) conversation-turn-id)
     {:conversation-turn-id conversation-turn-id
      :intents [] :plans [] :gates [] :attestations []})))

(defn- foundation-contract
  "Model-facing alias for the current completion contract projection.
   This is intentionally a read projection, not a separate domain
   object: the domain objects are Intent, Plan, Gate, Attestation, and
   provenance refs."
  ([env]
   (foundation-work-state env))
  ([env conversation-turn-id]
   (foundation-work-state env conversation-turn-id)))

(defn- foundation-intent!
  [env opts]
  (vis/db-store-intent! (:db-info env)
    (assoc opts :conversation-turn-id (or (:conversation-turn-id opts)
                                        (current-conversation-turn-id env)))))

(defn- foundation-plan!
  [env opts]
  (vis/db-store-plan! (:db-info env) opts))

(defn- foundation-gate!
  [env opts]
  (vis/db-store-gate! (:db-info env) opts))

(defn- foundation-attest!
  ([env opts]
   (let [work-state (current-turn-work-state env)
         gate-id    (or (:gate-state-id opts)
                      (resolve-current-gate-state-id work-state (:gate opts))
                      (resolve-current-gate-state-id work-state (:gate-key opts)))]
     (vis/db-store-attestation! (:db-info env)
       (-> opts
         (dissoc :gate :gate-key)
         (assoc :gate-state-id gate-id)))))
  ([env gate opts]
   (foundation-attest! env (assoc opts :gate gate))))

(defn- foundation-block-gate!
  ([env opts]
   (foundation-attest! env (assoc opts :status :blocked)))
  ([env gate opts]
   (foundation-attest! env gate (assoc opts :status :blocked))))

(defn- foundation-gates
  [env]
  (current-gate-states (current-turn-work-state env)))

(defn- foundation-attestations
  [env]
  (:attestations (current-turn-work-state env)))

(defn- gate-check-violations
  [work-state timeline]
  (let [gates      (current-gate-states work-state)
        by-gate    (attestations-by-gate work-state)
        ref-set    (set (keep :ref timeline))
        active-intents (filter #(= :active (:status %)) (latest-by-soul (:intents work-state)))
        active-plans   (current-plan-states work-state)]
    (vec
      (concat
        (when (empty? active-intents)
          [{:type :missing-active-intent}])
        (when (and (seq active-intents) (empty? active-plans))
          [{:type :missing-active-plan}])
        (mapcat
          (fn [gate]
            (let [atts (get by-gate (:id gate))
                  att  (first atts)
                  refs (mapv :ref (:refs att))]
              (cond-> []
                (and (:required? gate) (= :open (:status gate)))
                (conj {:type :required-gate-open :gate gate})

                (and (= :closed (:status gate)) (not= 1 (count atts)))
                (conj {:type :closed-gate-attestation-count :gate gate :count (count atts)})

                (and (= :closed (:status gate)) (= 1 (count atts)) (not= :proven (:status att)))
                (conj {:type :closed-gate-attestation-not-proven :gate gate :attestation att})

                (and (= :blocked (:status gate)) (not= 1 (count atts)))
                (conj {:type :blocked-gate-attestation-count :gate gate :count (count atts)})

                (and (= :blocked (:status gate)) (= 1 (count atts)) (not= :blocked (:status att)))
                (conj {:type :blocked-gate-attestation-not-blocked :gate gate :attestation att})

                (and (= :blocked (:status gate)) (= 1 (count atts)) (str/blank? (:reason att)))
                (conj {:type :blocked-gate-missing-reason :gate gate :attestation att})

                (and (contains? #{:closed :blocked} (:status gate)) (empty? refs))
                (conj {:type :attestation-missing-refs :gate gate :attestation att})

                (and (seq refs) (seq (remove ref-set refs)))
                (conj {:type :attestation-ref-missing-from-current-turn-provenance
                       :gate gate
                       :attestation att
                       :missing-refs (vec (remove ref-set refs))}))))
          gates)))))

(defn- foundation-gate-checks
  [env]
  (let [work-state (current-turn-work-state env)
        timeline   (current-turn-provenance-timeline env)
        violations (gate-check-violations work-state timeline)
        gates      (current-gate-states work-state)]
    {:scope :conversation-turn
     :conversation-turn-id (current-conversation-turn-id env)
     :conversation-turn-state-id (:conversation-turn-state-id work-state)
     :ok? (empty? violations)
     :checked {:active-intents (count (filter #(= :active (:status %)) (latest-by-soul (:intents work-state))))
               :active-plans   (count (current-plan-states work-state))
               :gates          (count gates)
               :required-gates (count (filter :required? gates))
               :attestations   (count (:attestations work-state))
               :current-turn-provenance-events (count timeline)}
     :violations violations}))

(defn- gate-report-for-work-state
  [work-state checks]
  (let [gates   (current-gate-states work-state)
        by-gate (attestations-by-gate work-state)]
    (str "## Gates\n\n"
      "- Scope: current conversation turn\n"
      "- Status: " (if (:ok? checks) "ok" (str "failed " (count (:violations checks)) " check(s)")) "\n\n"
      (if (seq gates)
        (str/join "\n"
          (map (fn [gate]
                 (let [att (first (get by-gate (:id gate)))]
                   (str "- `" (name (:key gate)) "` " (name (:status gate))
                     " — " (:question gate)
                     (when att
                       (str "\n  - attestation: " (name (:status att))
                         (when-let [summary (:summary att)] (str " — " summary))
                         (when-let [reason (:reason att)] (str " — " reason))
                         "\n  - refs: " (str/join ", " (map :ref (:refs att))))))))
            gates))
        "_No current gates._")
      (when (seq (:violations checks))
        (str "\n\n### Violations\n"
          (str/join "\n" (map #(str "- " (pr-str (dissoc % :gate :attestation))) (:violations checks)))))
      "\n")))

(defn- foundation-gate-report
  [env]
  (let [work-state (current-turn-work-state env)
        checks     (foundation-gate-checks env)]
    (gate-report-for-work-state work-state checks)))

(defn- foundation-contract-report
  "Markdown report for the current completion contract. Optional arg
   accepts a conversation-turn id and renders that turn's latest run."
  ([env]
   (str "# Completion Contract\n\n"
     (foundation-gate-report env)
     "\n"
     (foundation-provenance-report env (:conversation-id env))))
  ([env conversation-turn-id]
   (let [work-state (foundation-work-state env conversation-turn-id)
         timeline   (->> (foundation-provenance-timeline env (:conversation-id env))
                      (filter #(same-uuid? (:turn-id %) conversation-turn-id))
                      vec)
         checks     (let [violations (gate-check-violations work-state timeline)]
                      {:ok? (empty? violations)
                       :violations violations
                       :checked {:gates (count (current-gate-states work-state))
                                 :attestations (count (:attestations work-state))
                                 :current-turn-provenance-events (count timeline)}})]
     (str "# Completion Contract\n\n"
       "- Turn: `" conversation-turn-id "`\n"
       "- Turn-state: `" (:conversation-turn-state-id work-state) "`\n\n"
       (gate-report-for-work-state work-state checks)))))

(defn- foundation-audit-report
  "Conversation-level Markdown audit report: every turn's contract
   summary plus the conversation provenance ledger. Useful for
   diagnosing historical conversations like a failed gating run."
  ([env]
   (foundation-audit-report env (:conversation-id env)))
  ([env conversation-id]
   (let [turns (safe-call #(vis/db-list-conversation-turns (:db-info env) conversation-id) [])]
     (str "# Audit Report\n\n"
       "- Conversation: `" conversation-id "`\n"
       "- Turns: " (count turns) "\n\n"
       (if (seq turns)
         (str/join "\n"
           (map (fn [turn]
                  (let [work-state (foundation-work-state env (:id turn))
                        gates      (current-gate-states work-state)]
                    (str "## Turn `" (:id turn) "`\n\n"
                      "- Status: " (name (or (:status turn) :unknown)) "\n"
                      "- Request: " (preview (:user-request turn) 180) "\n"
                      "- Contract: " (if (seq gates)
                                       (str (count gates) " gate(s), " (count (:attestations work-state)) " attestation(s)")
                                       "none") "\n")))
             turns))
         "_No turns found._\n")
       "\n"
       (foundation-provenance-report env conversation-id)))))

;; ---------------------------------------------------------------------------
;; Extension catalog + README / doc access
;;
;; Every extension declares itself in a single classpath resource at
;; `META-INF/vis-extension/vis.edn`, an EDN map keyed by id
;; (`{<id-symbol> {:nses [...] :docs {<name> <body>}}}`). The id is the
;; same token the LLM uses as the SCI sandbox alias (`'vis`, `'vis`,
;; etc.). Each doc descriptor is a map with `:description` (one-paragraph
;; summary) + `:content` (full Markdown body) as plain EDN strings.
;; `(v/extension-docs ...)` returns the descriptions (no `:content`)
;; so the LLM can scan the index before pulling a full body via
;; `(v/extension-doc ...)`.
;; See AGENTS.md ▸ "Every extension ships a single canonical README
;; in vis.edn".
;; ---------------------------------------------------------------------------

(defn- registered-extensions []
  (try (vis/registered-extensions) (catch Throwable _ [])))

(defn- reference-as-symbol
  "Coerce an extension reference to the canonical id symbol used by
   the docs registry. Accepts the id symbol itself (`'vis`), a
   keyword (`:vis`), a string (`\"meta\"`), the alias-ns symbol
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
      (or (when (contains? (set (vis/registered-extension-ids)) target-sym)
            target-sym)
        (some (fn [extension]
                (when (extension-matches? target-str extension)
                  (or (get-in extension [:ext/ns-alias :alias])
                    (vis/extension-id-of-ns (:ext/namespace extension)))))
          (registered-extensions))))))

(defn- extension-summary [extension]
  (let [prov     (vis/extension-provenance extension)
        id       (:registry-id prov)
        doc-list (when id (vis/extension-docs id))]
    (assoc prov
      :symbols (mapv :ext.symbol/sym (:ext/symbols extension))
      :docs    (or doc-list []))))

(defn- foundation-extensions
  "Catalog every loaded extension as data. Returns a vector of
   `{:namespace :alias :kind :version :author :owner :license
     :registry-id :source-paths :source-mtime-max :source-hash-sha256
     :doc :symbols :docs}` maps.
   `:docs` is a vector of `{:name :description}` descriptors for every
   doc the extension declares."
  [_env]
  (mapv extension-summary (registered-extensions)))

(defn- foundation-extension-docs
  "With one arg, return the doc catalog for one extension as a vector
   of `{:name :description}` descriptors. With no arg, return the full
   registry as `{<id-symbol> [<descriptor> ...]}`."
  ([_env]
   (try (vis/extension-docs) (catch Throwable _ {})))
  ([_env reference]
   (when-let [id (resolve-extension-id reference)]
     (vis/extension-docs id))))

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
      (vis/extension-doc id doc-name))))

(defn- foundation-extension-readme
  "Convenience: full Markdown body of an extension's canonical
   README. Equivalent to `(:content (v/extension-doc ref
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

(def inspect-symbol
  (vis/symbol 'inspect foundation-inspect
    {:doc       "Conversation introspection map. Default current conversation; pass id/prefix for another."
     :arglists  '([] [conversation-id])
     :examples  ["(v/inspect)"
                 "(get-in (v/inspect) [:diagnosis :next-actions])"
                 "(-> (v/inspect) :llm-diagnostics last :raw-response :preview)"
                 "(get-in (v/inspect) [:transcript :totals :cost-usd])"
                 "(map :user-request (get-in (v/inspect) [:transcript :turns]))"]
     :before-fn inject-environment}))

(def report-symbol
  (vis/symbol 'report foundation-report
    {:doc       "Markdown report from `(v/inspect)`. Default current conversation; pass id/prefix for another."
     :arglists  '([] [conversation-id])
     :examples  ["(v/report)"
                 "(v/report \"3a7b2c…\")"]
     :before-fn inject-environment}))

(def provenance-timeline-symbol
  (vis/symbol 'provenance-timeline foundation-provenance-timeline
    {:doc       "Ordered provenance ledger: eval events plus child tool events with stable iN.K refs."
     :arglists  '([] [conversation-id])
     :examples  ["(v/provenance-timeline)"
                 "(filter #(= :error (:status %)) (v/provenance-timeline))"
                 "(map (juxt :ref :kind :op :status) (v/provenance-timeline))"]
     :before-fn inject-environment}))

(def provenance-stats-symbol
  (vis/symbol 'provenance-stats foundation-provenance-stats
    {:doc       "Provenance rollup: counts by kind/op/status/engine, failures, slowest events, tools."
     :arglists  '([] [conversation-id])
     :examples  ["(v/provenance-stats)"
                 "(:failures (v/provenance-stats))"
                 "(:by-op (v/provenance-stats))"]
     :before-fn inject-environment}))

(def provenance-guards-symbol
  (vis/symbol 'provenance-guards foundation-provenance-guards
    {:doc       "Run provenance integrity guards. Returns {:ok? :checked :violations}; gate answer citations on :ok?."
     :arglists  '([] [conversation-id])
     :examples  ["(v/provenance-guards)"
                 "(:violations (v/provenance-guards))"]
     :before-fn inject-environment}))

(def provenance-report-symbol
  (vis/symbol 'provenance-report foundation-provenance-report
    {:doc       "Compact Markdown provenance audit trail for answer proof sections."
     :arglists  '([] [conversation-id])
     :examples  ["(v/provenance-report)"
                 "(answer (v/provenance-report))"]
     :before-fn inject-environment}))

(def contract-symbol
  (vis/symbol 'contract foundation-contract
    {:doc       "Current turn completion contract projection: intents, plans, gates, attestations. Prefer this over the legacy name `work-state`."
     :arglists  '([] [conversation-turn-id])
     :examples  ["(v/contract)"
                 "(:gates (v/contract))"]
     :before-fn inject-environment}))

(def work-state-symbol
  (vis/symbol 'work-state foundation-work-state
    {:doc       "Legacy alias/read projection for the current completion contract. Prefer `(v/contract)`."
     :arglists  '([] [conversation-turn-id])
     :examples  ["(v/work-state)"
                 "(:gates (v/contract))"]
     :before-fn inject-environment}))

(def intent!-symbol
  (vis/symbol 'intent! foundation-intent!
    {:doc       "Persist a new versioned intent for the current conversation turn."
     :arglists  '([{:keys [key text status created-ref metadata]}])
     :examples  ["(def intent (v/intent! {:key :main :text TURN_USER_REQUEST :created-ref \"i1.1\"}))"]
     :before-fn inject-environment}))

(def plan!-symbol
  (vis/symbol 'plan! foundation-plan!
    {:doc       "Persist a new versioned plan for an intent_state."
     :arglists  '([{:keys [intent-state-id key summary steps status created-ref metadata]}])
     :examples  ["(def plan (v/plan! {:intent-state-id (:id intent) :key :main :summary \"Inspect, change, verify.\" :steps [{:do :inspect}]}))"]
     :before-fn inject-environment}))

(def gate!-symbol
  (vis/symbol 'gate! foundation-gate!
    {:doc       "Persist a new versioned gate for a plan_state. Gates are open until attested or blocked."
     :arglists  '([{:keys [plan-state-id key question required? status created-ref metadata]}])
     :examples  ["(def verify-gate (v/gate! {:plan-state-id (:id plan) :key :verify :question \"Did verification pass?\"}))"]
     :before-fn inject-environment}))

(def attest!-symbol
  (vis/symbol 'attest! foundation-attest!
    {:doc       "Persist the exactly-one attestation for a gate version. Status :proven closes the gate. Refs must cite current-turn provenance refs."
     :arglists  '([opts] [gate opts])
     :examples  ["(v/attest! (:id verify-gate) {:status :proven :summary \"Tests passed.\" :refs [\"i4.1/tool\"]})"
                 "(v/attest! :verify {:status :proven :summary \"Quick verification passed.\" :refs [\"i4.1/tool\"]})"]
     :before-fn inject-environment}))

(def block-gate!-symbol
  (vis/symbol 'block-gate! foundation-block-gate!
    {:doc       "Persist a blocked attestation for a gate version. Requires :reason and provenance refs."
     :arglists  '([opts] [gate opts])
     :examples  ["(v/block-gate! :verify {:reason \"verify.sh unavailable\" :refs [\"i3.1/tool\"]})"]
     :before-fn inject-environment}))

(def gates-symbol
  (vis/symbol 'gates foundation-gates
    {:doc       "Current active-plan gate states for this conversation turn."
     :arglists  '([])
     :examples  ["(v/gates)"
                 "(filter :required? (v/gates))"]
     :before-fn inject-environment}))

(def attestations-symbol
  (vis/symbol 'attestations foundation-attestations
    {:doc       "Attestations for the current conversation turn."
     :arglists  '([])
     :examples  ["(v/attestations)"
                 "(mapcat :refs (v/attestations))"]
     :before-fn inject-environment}))

(def gate-checks-symbol
  (vis/symbol 'gate-checks foundation-gate-checks
    {:doc       "Enforce the current-turn gate contract before final answer: required gates terminal, exactly one attestation, refs resolve."
     :arglists  '([])
     :examples  ["(v/gate-checks)"
                 "(:violations (v/gate-checks))"]
     :before-fn inject-environment}))

(def gate-report-symbol
  (vis/symbol 'gate-report foundation-gate-report
    {:doc       "Markdown gate/attestation report for final answer proof sections."
     :arglists  '([])
     :examples  ["(v/gate-report)"
                 "(answer (v/join (v/gate-report) (v/provenance-report)))"]
     :before-fn inject-environment}))

(def contract-report-symbol
  (vis/symbol 'contract-report foundation-contract-report
    {:doc       "Markdown completion-contract report with gates, attestations, and provenance. Optional arg renders a specific turn."
     :arglists  '([] [conversation-turn-id])
     :examples  ["(v/contract-report)"
                 "(v/contract-report TURN_CONVERSATION_TURN_ID)"]
     :before-fn inject-environment}))

(def audit-report-symbol
  (vis/symbol 'audit-report foundation-audit-report
    {:doc       "Conversation-level Markdown audit: per-turn contract summary plus full provenance timeline."
     :arglists  '([] [conversation-id])
     :examples  ["(v/audit-report)"
                 "(v/audit-report \"cff21022-c11f-4818-a2de-b4d5c5c4630a\")"]
     :before-fn inject-environment}))

;; Legacy per-view sandbox symbols were removed from the public surface.
;; The private implementation functions above remain as building blocks for
;; `v/inspect` so tests and internal composition can target the real seams
;; without re-exposing shallow API peers.

(def extensions-symbol
  (vis/symbol 'extensions foundation-extensions
    {:doc       "Loaded extension catalog: {:namespace :alias :kind :version :doc :symbols :docs}."
     :arglists  '([])
     :examples  ["(v/extensions)"
                 "(map :namespace (v/extensions))"
                 "(map (juxt :alias :docs) (v/extensions))"]
     :before-fn inject-environment}))

(def extension-docs-symbol
  (vis/symbol 'extension-docs foundation-extension-docs
    {:doc       "Extension doc index. No arg returns registry; one arg returns summaries without `:content`."
     :arglists  '([] [extension-ref])
     :examples  ["(v/extension-docs)"
                 "(v/extension-docs 'vis)"
                 "(map :description (v/extension-docs 'vis))"
                 "(:links (first (v/extension-docs 'vis)))"]
     :before-fn inject-environment}))

(def extension-doc-symbol
  (vis/symbol 'extension-doc foundation-extension-doc
    {:doc       "Full extension doc descriptor: {:name :description :content :links :reflinks}. Nil when missing."
     :arglists  '([extension-ref doc-name])
     :examples  ["(v/extension-doc 'vis \"README.md\")"
                 "(:content (v/extension-doc :vis \"README.md\"))"
                 "(:links       (v/extension-doc 'vis \"README.md\"))"
                 "(:reflinks    (v/extension-doc 'vis \"README.md\"))"]
     :before-fn inject-environment}))

(def extension-readme-symbol
  (vis/symbol 'extension-readme foundation-extension-readme
    {:doc       "Extension README Markdown text. Nil when missing."
     :arglists  '([extension-ref])
     :examples  ["(v/extension-readme 'vis)"
                 "(v/extension-readme 'com.blockether.vis.ext.foundation.introspection)"
                 "(v/extension-readme :vis)"
                 "(println (v/extension-readme 'vis))"]
     :before-fn inject-environment}))

(def all-symbols
  [inspect-symbol
   report-symbol
   provenance-timeline-symbol
   provenance-stats-symbol
   provenance-guards-symbol
   provenance-report-symbol
   contract-symbol
   work-state-symbol
   intent!-symbol
   plan!-symbol
   gate!-symbol
   attest!-symbol
   block-gate!-symbol
   gates-symbol
   attestations-symbol
   gate-checks-symbol
   gate-report-symbol
   contract-report-symbol
   audit-report-symbol
   extensions-symbol
   extension-docs-symbol
   extension-doc-symbol
   extension-readme-symbol])

(def introspection-prompt
  (str "`v/` introspection:\n"
    "  (v/inspect cid?)              conversation data incl. :llm-diagnostics\n"
    "  (v/report cid?)               Markdown report incl. raw LLM diagnostics\n"
    "  (v/provenance-timeline cid?)  ordered eval/tool ledger with iN.K refs\n"
    "  (v/provenance-stats cid?)     counts, failures, slowest events, tools\n"
    "  (v/provenance-guards cid?)    integrity checks before citing provenance\n"
    "  (v/provenance-report cid?)    compact Markdown audit trail\n"
    "  (v/contract turn-id?)         current-turn completion contract projection\n"
    "  (v/contract-report turn-id?)  Markdown contract + proof report\n"
    "  (v/audit-report cid?)         conversation audit: contracts + provenance\n"
    "  (v/intent! opts)              create current-turn intent_state version\n"
    "  (v/plan! opts)                create plan_state for an intent_state\n"
    "  (v/gate! opts)                create gate_state for a plan_state\n"
    "  (v/attest! gate opts)         prove exactly one gate version with refs\n"
    "  (v/block-gate! gate opts)     block exactly one gate version with refs + reason\n"
    "  (v/work-state)                legacy alias for (v/contract)\n"
    "  (v/gates)                     current active-plan gate states\n"
    "  (v/attestations)              current-turn attestations\n"
    "  (v/gate-checks)               current-turn gate contract checks\n"
    "  (v/gate-report)               Markdown gate/attestation report\n"
    "  (v/extensions)                loaded extension catalog\n"
    "  (v/extension-docs ext-ref)    doc summaries\n"
    "  (v/extension-doc ext-ref name) doc incl. :content\n"
    "  (v/extension-readme ext-ref)  README text\n"))

;; The extension that owns all `v/`-aliased symbols is built
;; and registered by `com.blockether.vis.ext.foundation.core`,
;; not here — this namespace only exposes the symbol vec + prompt
;; fragment for the aggregator to assemble.
