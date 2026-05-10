(ns com.blockether.vis.ext.foundation.introspection
  "Programmatic introspection of the agent's own state from inside
   `:code`. The public state surface is deliberately small:

   - `(v/conversation-state [conversation-id])` -> data map, including raw LLM diagnostics
   - `(v/conversation-report [conversation-id])` -> Markdown rendered from that data

   Everything else in this namespace is implementation detail. The agent
   gets the data once, manipulates it via plain Clojure (`get-in`,
   `filter`, `map`, etc.), and renders the same data to Markdown only
   when presentation is needed.

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
;; on the classpath surfaces in the inspect conversation index automatically -
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
;; Helpers - derive ids, deref atoms, normalize sym args.
;; ---------------------------------------------------------------------------

(defn- safe-deref [a]
  (when a (try (deref a) (catch Throwable _ nil))))

(defn- current-conversation-id [env]
  (:conversation-id env))

(defn- current-conversation-turn-id
  "UUID of the in-flight turn, or nil when no turn is running yet.

   Internally this is the `conversation_turn_soul` id - hence the
   `:current-conversation-turn-id-atom` env key and this helper's `conversation-turn-id`
   suffix. The product concept is *turn*; everywhere this id is
   surfaced to the model (e.g. inspect current-turn results, the
   `:in-flight-turn-id` slot) it's labelled `turn`. Mirrors the
   SCI-visible `TURN_ID` SYSTEM var so meta-fns can filter
   it without round-tripping through SCI."
  [env]
  (some-> (:current-conversation-turn-id-atom env) deref))

(defn- same-uuid?
  "True when two values denote the same UUID. Accepts UUID instances
   or any object whose `str` is the canonical UUID form. Used to
   match `TURN_ID` against a turn's `:id` regardless of
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
;; Builders - assemble each top-level snapshot map.
;; ---------------------------------------------------------------------------

(defn- iteration-pointer
  "Snapshot of the live iteration counter. Reads the env atom set by
   the iteration-loop's prepared context. The loop runs until the
   model emits `:answer` -- there is no model-visible budget, so the
   pointer carries only `:current`."
  [env]
  (let [current-position (or (safe-deref (:current-iteration-atom env)) 1)]
    {:current (long current-position)}))

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
   are nil/blank - callers `cond->` on the result."
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
   (e.g. `\"openai/gpt-4o\"`) so callers render it directly - the
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
         (str (subs string-value 0 limit) "...")
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
    "Provider returned prose/string instead of the iteration map. Skip the SQLite trip - the raw preview is already here. Continue after the built-in schema retry, or switch model when this repeats."

    :regex-unsupported-escape
    "Clojure strings reject \\| as an escape. Use bare | for regex alternation inside a string, or switch to a #\"...\" regex literal for complex patterns."

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
;; Meta fns - each takes `env` as first arg via the shared
;; `:before-fn` injector below. The agent never sees `env`; it calls
;; e.g. current-turn snapshot with zero args.
;; ---------------------------------------------------------------------------

(defn- foundation-turn [env]
  (turn-snapshot env))

(defn- foundation-conversation
  "Snapshot for a conversation. The in-flight turn (= current `TURN_ID`)
   is automatically excluded from `:turns` because its `:iteration-count` /
   `:total-cost` haven't been finalized yet - listing it would render
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
   the conversation has no rows OR the env is missing handles - lets
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
   version is a retry. `conversation-turn-id` is a `conversation_turn_soul` UUID - the same id
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
   floor: agents that miss a path 2-3x and pivot stay below; agents
   that emit 5+ identical-root-cause errors are stuck and not learning.
   Anchored to the worst-case in the self-analyze report for
   conversation 89ea9c98-21d4-4483-a962-f8ccb1d8232d (148 'src/tui not
   found' failures in one turn - the failure mode this catches)."
  5)

(defn- repetition-signature
  "Project a failure to a lossy 'same root cause' signature so 148
   varying filename attempts under the same missing directory hash to
   the same bucket. Strategy: keep `:source` + `:classification` and
   collapse the message to the leading phrase before the first `:`
   (e.g. `Path not found: /.../foo.clj` and `File not found: /.../bar.clj`
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
   when nothing is repeating - caller treats `(seq ...)` as the
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
                  (str "Same error repeated " count "x this turn (e.g. "
                    (preview (:message sample) 80)
                    "). STOP varying inputs to the failing call. "
                    "Switch strategy: list a parent directory, broaden "
                    "the search, or pivot - repeating the same shape "
                    "will not converge."))
            clusters))

        (contains? classes :provider-schema-rejected)
        (conj "Treat schema rejection as provider noise, not a reason to inspect SQLite. Use :raw-preview from (:failures (v/conversation-state)) and retry/switch model only if it repeats.")

        (contains? classes :regex-unsupported-escape)
        (conj (str "v/rg takes one spec map with literal vectors, not regex strings or positional args. "
                "Use {:all [\"foo|bar\"]} for literal pipe text, or {:any [\"foo\" \"bar\"]} for OR. "
                "Add :paths and :include in the same map."))

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
   least `REPETITION_THRESHOLD` times in the failure list - the
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
   by `v/conversation-state`. This is a convenience index over the canonical
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

;; Removed extra workflow surfaces.

(defn- inject-environment
  [env f args]
  {:env env :fn f :args (into [env] args)})

;; -- public, doc-bearing aliases -----------------------------------------------
;; The two underlying defs (`foundation-inspect`, `foundation-report`) are
;; private and named for clarity inside this ns. Re-export them under their
;; SCI-visible names with `:doc` and `:arglists` baked into the var meta so
;; `vis/symbol` can read both straight off the var.
(def ^{:doc "Full conversation state: conversation index, current turn snapshot, classified failures, diagnosis, fork/retry metadata, raw LLM diagnostics, and complete transcript. Default target = current conversation; pass a conversation-id or unambiguous prefix to inspect another."
       :arglists '([] [conversation-id])} conversation-state foundation-inspect)
(def ^{:doc "Complete Markdown report for a conversation: every turn, iteration, code block, result, answer, and LLM diagnostic rendered as a single Markdown artifact. Same underlying data as `v/conversation-state`. Default target = current conversation."
       :arglists '([] [conversation-id])} conversation-report foundation-report)

(def conversation-state-symbol
  (vis/symbol #'conversation-state
    {:before-fn inject-environment
     :journal-render-fn vis/render-pr-str-journal
     :channel-render-fn vis/render-pr-str-channel}))

(def conversation-report-symbol
  (vis/symbol #'conversation-report
    {:before-fn inject-environment
     :journal-render-fn vis/render-string-journal
     :channel-render-fn vis/render-string-channel}))

(def all-symbols
  [conversation-state-symbol
   conversation-report-symbol])

(def introspection-prompt
  (str "`v/` conversation state: (v/conversation-state cid?) -> full conversation data map (turns, iterations, failures, diagnosis, transcript, LLM diagnostics).\n"
    "`v/` conversation report: (v/conversation-report cid?) -> complete Markdown report of the same data.\n"))

;; The extension that owns all `v/`-aliased symbols is built
;; and registered by `com.blockether.vis.ext.foundation.core`,
;; not here - this namespace only exposes the symbol vec + prompt
;; fragment for the aggregator to assemble.
