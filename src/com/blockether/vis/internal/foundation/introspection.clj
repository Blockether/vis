(ns com.blockether.vis.internal.foundation.introspection
  "Programmatic introspection of the agent's own state from inside
   `:code`. The public state surface is deliberately small:

   - `(session-state [session-id])` -> data map, including raw LLM diagnostics
   - `(session-report-html [session-id])` -> HTML report rendered from that data

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
  (:require [charred.api :as json]
            [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.internal.foundation.transcript :as transcript]
            [com.blockether.vis.internal.extension :as extension]))

;; ---------------------------------------------------------------------------
;; Channels we know how to enumerate. Derived from the global channel
;; registry (`vis/registered-channels`) so any third-party channel jar
;; on the classpath surfaces in the inspect session index automatically -
;; no edits to this file when a new front-end ships.
;;
;; `:cli` is added unconditionally because the CLI agent uses `:cli` as
;; its sessions-channel namespace WITHOUT registering a channel
;; descriptor (the `vis` dispatcher itself is the surface; there is no
;; `vis channels cli` sub-command, so it has no `:channel/cmd`). Every
;; other channel id comes from the registry.
;; ---------------------------------------------------------------------------

(defn- known-channels
  "Vec of sessions-channel keywords known to this process. Derived
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

(defn- current-session-id [env] (:session-id env))

(defn- current-session-turn-id
  "UUID of the in-flight turn, or nil when no turn is running yet.

   Internally this is the `session_turn_soul` id — stored as the
   `:session-turn-id` field on the single `:turn-state-atom` (see
   `ctx-loop/make-turn-state-atom`). The product concept is *turn*;
   everywhere this id is surfaced to the model (e.g. inspect
   current-turn results, the `:in-flight-turn-id` slot) it's labelled
   `turn`. Mirrors the sandbox-visible `TURN_ID` SYSTEM var so meta-fns
   can filter it without round-tripping through the sandbox."
  [env]
  (some-> (:turn-state-atom env)
          deref
          :session-turn-id))

(defn- same-uuid?
  "True when two values denote the same UUID. Accepts UUID instances
   or any object whose `str` is the canonical UUID form. Used to
   match `TURN_ID` against a turn's `:id` regardless of
   whether the persistence layer returned a UUID or a string."
  [a b]
  (and a b (= (str a) (str b))))

(defn- iteration-rows
  "Fetch the iteration rows for `session-turn-id`; returns [] on any failure."
  [db-info session-turn-id]
  (try (vis/db-list-session-turn-iterations db-info session-turn-id) (catch Throwable _ [])))

;; ---------------------------------------------------------------------------
;; Builders - assemble each top-level snapshot map.
;; ---------------------------------------------------------------------------

(defn- iteration-pointer
  "Snapshot of the live iteration counter. Reads the env atom set by
   the iteration-loop's prepared context. The loop runs until the
   model emits `:answer` -- there is no model-visible budget, so the
   pointer carries only `:current`."
  [env]
  (let
    [iter-raw
     (some-> (:turn-state-atom env)
             deref
             :iteration)

     current-position
     (cond (map? iter-raw) (or (:position iter-raw) 1)
           (number? iter-raw) iter-raw
           :else 1)]

    {:current (long current-position)}))

(defn- attempts-from-iterations
  "Walk `iterations` (in DB order) and collect every executed
   expression. Used by the current-turn snapshot and by attempt search."
  [_db-info iterations]
  (mapv (fn [iteration]
          {:iteration-id (:id iteration)
           :iteration (:position iteration)
           :code (:code iteration)
           :result (:result iteration)
           :error (:error iteration)
           :duration-ms (:duration-ms iteration)})
        iterations))

(defn- format-provider-model
  "Render `\"provider/model\"` when both are known, otherwise just the
   model (or just the provider) so callers always get a non-empty
   string when at least one component exists. Returns nil when both
   are nil/blank - callers `cond->` on the result."
  [provider model]
  (let
    [provider-str
     (some-> provider
             name
             str/trim
             not-empty)

     model-str
     (some-> model
             str
             str/trim
             not-empty)]

    (cond (and provider-str model-str) (str provider-str "/" model-str)
          model-str model-str
          provider-str provider-str
          :else nil)))

(defn- turn-cost-summary
  "Pull the token / cost / provider / model summary persisted on
   `session_turn_state` canonical columns (Phase B: `input_tokens`,
   `input_regular_tokens`, `input_cache_write_tokens`,
   `input_cache_read_tokens`, `output_tokens`, `output_reasoning_tokens`,
   `total_cost_usd`, `llm_root_provider`, `llm_root_model`). Returns a
   map with the canonical token keys + cost + provider/model when
   present, or an empty map. Never throws.

   `:provider-model` is a derived `\"provider/model\"` display string
   (e.g. `\"openai/gpt-4o\"`) so callers render it directly - the
   canonical data still lives in `:provider` and `:model` separately."
  [turn]
  (let [provider-model (format-provider-model (:provider turn) (:model turn))]
    (cond-> {}
      (:input-tokens turn)
      (assoc :input-tokens (:input-tokens turn))

      (:input-regular-tokens turn)
      (assoc :input-regular-tokens (:input-regular-tokens turn))

      (:input-cache-write-tokens turn)
      (assoc :input-cache-write-tokens (:input-cache-write-tokens turn))

      (:input-cache-read-tokens turn)
      (assoc :input-cache-read-tokens (:input-cache-read-tokens turn))

      (:output-tokens turn)
      (assoc :output-tokens (:output-tokens turn))

      (:output-reasoning-tokens turn)
      (assoc :output-reasoning-tokens (:output-reasoning-tokens turn))

      (:total-cost turn)
      (assoc :total-cost (:total-cost turn))

      (:provider turn)
      (assoc :provider (:provider turn))

      (:model turn)
      (assoc :model (:model turn))

      provider-model
      (assoc :provider-model provider-model))))

(defn- elapsed-ms
  "Wall-clock duration for a turn in milliseconds. Read from
   `:duration-ms` when persisted; otherwise computed from the
   underlying turn row's `:created-at` so the model can self-pace
   mid-turn."
  [turn]
  (or (:duration-ms turn)
      (when-let [created (:created-at turn)]
        (try (- (System/currentTimeMillis)
                (long (cond (inst? created) (inst-ms created)
                            (integer? created) (long created)
                            :else 0)))
             (catch Throwable _ nil)))))

(defn- parse-json-map
  "Best-effort JSON object parser for persisted provider errors. The
   persistence layer stores `iteration.llm_error` as JSON text; meta
   keeps parsing local so callers get failures as Clojure data.

   `:key-fn keyword` stays: the parsed map is INTERNAL to
   `provider-failure` (its keys are read locally via `(:type data)`
   etc. and never emitted), and its string VALUES are what flow into
   the boundary-crossing failure map. Keeping idiomatic keyword access
   here does not put a keyword on the wire."
  [text]
  (when (and (string? text) (not (str/blank? text)))
    (try (let [parsed (json/read-json text :key-fn keyword)]
           (when (map? parsed) parsed))
         (catch Throwable _ nil))))

(defn- preview
  ([text] (preview text 220))
  ([text ^long limit]
   (when (some? text)
     (let [string-value (str text)]
       (if (> (count string-value) limit) (str (subs string-value 0 limit) "...") string-value)))))

(defn- error-text
  [error]
  (cond (nil? error) nil
        (string? error) error
        (map? error) (or (:message error) (pr-str error))
        :else (str error)))

(def ^:private schema-rejected-type-strings
  "String forms svar's schema-rejection error type serializes to. The
   persisted `iteration.llm_error` JSON carries the type as a string
   (charred writes the `:svar.spec/schema-rejected` keyword as the bare
   `\"svar.spec/schema-rejected\"`); the colon-prefixed variant is kept
   in case a different serializer preserved the leading colon. Matched
   by string equality - no keyword minting off boundary-supplied data."
  #{"svar.spec/schema-rejected" ":svar.spec/schema-rejected"})

(defn- schema-rejected-type? [value] (contains? schema-rejected-type-strings value))

(defn- provider-failure
  [iteration]
  (when-let [error (:error iteration)]
    (let
      [error-map (or (parse-json-map error) {:message (str error)})
       data (:data error-map)
       ;; `:type` / `:reason` are JSON string values carried straight
       ;; into the failure map, which crosses the strings-only
       ;; boundary - keep them as strings, never mint keywords.
       type (:type data)
       reason (:reason data)
       raw-data (:raw-data data)]

      (cond->
        {:source :provider
         :iteration-id (:id iteration)
         :iteration (:position iteration)
         :status (:status iteration)
         :message (or (:message error-map) (str error))
         :classification
         (if (schema-rejected-type? type) :provider-schema-rejected :provider-error)}
        type
        (assoc :type type)

        reason
        (assoc :reason reason)

        (:received-type data)
        (assoc :received-type (:received-type data))

        (some? raw-data)
        (assoc :raw-preview (preview raw-data))

        (:raw-data-preview data)
        (assoc :raw-data-preview (:raw-data-preview data))))))

(defn- tool-name-from-code
  [code]
  (when (string? code) (second (re-find #"^\s*\(?([^\s\)]+)" code))))

(defn- classify-expression-failure
  [code error]
  (let
    [message
     (or (error-text error) "")

     lower-message
     (str/lower-case message)

     tool-name
     (or (tool-name-from-code code) "")]

    (cond (and (str/includes? tool-name "rg")
               (str/includes? lower-message "unsupported escape character"))
          :regex-unsupported-escape
          (and (str/includes? tool-name "rg")
               (str/includes? lower-message "unable to resolve symbol"))
          :regex-unescaped-quote
          (and (str/includes? tool-name "patch")
               (str/includes? lower-message "unmatched delimiter"))
          :patch-invalid-program
          (and (str/includes? tool-name "patch")
               (or (str/includes? lower-message "anchor") (str/includes? lower-message "hashline"))
               (or (str/includes? lower-message "not found")
                   (str/includes? lower-message "no longer match")
                   (str/includes? lower-message "stale")))
          :patch-stale-anchor
          (str/includes? lower-message "unable to resolve symbol") :unresolved-symbol
          :else :code-execution-error)))

(defn- advice-for-classification
  [classification]
  (case classification
    :provider-schema-rejected
    "Provider returned prose/string instead of the iteration map. Skip the SQLite trip - the raw preview is already here. Continue after the built-in schema retry, or switch model when this repeats."

    :regex-unsupported-escape
    "rg matches literal substrings by default — no regex escaping needed. Pass one options dict: rg({\"any\": [\"foo\", \"bar\"]}) for OR, or rg({\"all\": [\"foo|bar\"]}) when you need the literal pipe text. Set \"is_regex\": True only for a real regex."

    :regex-unescaped-quote
    "The regex string likely contains an unescaped inner quote. Escape it as \\\" or use a regex literal / simpler pattern."

    :patch-invalid-program
    "The patch payload likely lost the closing quote of its replacement. Re-emit patch([{\"path\": …, \"from_anchor\": …, \"replace\": …}]) and use a Python triple-quoted string for multi-line content."

    :patch-stale-anchor
    "The anchor no longer matches. Re-read the smallest file slice and retry once with its fresh lineno:hash anchor."

    :unresolved-symbol
    "A reader/string boundary probably split the form and exposed a bare symbol. Check quoting before retrying."

    "Read :message, :code, and :iteration; fix the smallest failing form before issuing new searches."))

(defn- expression-failures-for-iteration
  [_db-info iteration]
  (if-let [error (:error iteration)]
    (let [classification (classify-expression-failure (:code iteration) error)]
      [{:source :code
        :iteration-id (:id iteration)
        :iteration (:position iteration)
        :tool (tool-name-from-code (:code iteration))
        :classification classification
        :code (:code iteration)
        :message (error-text error)
        :advice (advice-for-classification classification)}])
    []))

(defn- failures-from-iterations
  [db-info iterations]
  (vec (mapcat (fn [iteration]
                 (let
                   [provider (when-let [failure (provider-failure iteration)]
                               [(assoc failure
                                  :advice (advice-for-classification (:classification failure)))])]
                   (concat provider (expression-failures-for-iteration db-info iteration))))
               iterations)))

(defn- latest-turn
  [db-info session-id]
  (when (and db-info session-id)
    (last (try (vis/db-list-session-turns db-info session-id) (catch Throwable _ [])))))

(defn- turn-snapshot
  "The single-call rich current-turn snapshot. Aggregates
   the per-iteration data the prompt projection does NOT carry
   (attempts, provider/code failures, cost, elapsed-ms) plus the
   iteration pointer. The agent picks what it needs by map key
   instead of querying SQLite manually."
  [env]
  (let [{:keys [db-info session-id]} env]
    (when-let [turn (latest-turn db-info session-id)]
      (let
        [iterations (iteration-rows db-info (:id turn))
         attempts (attempts-from-iterations db-info iterations)]

        ;; No `:errors` key: a `(filterv :error attempts)` would be a
        ;; verbatim DUPLICATE of every errored attempt (full code+result
        ;; again) that nothing consumes. `:failures` already carries the
        ;; curated failure diagnostics, and the model can derive raw errored
        ;; attempts itself: [a for a in r["attempts"] if a.get("error")].
        (cond->
          {:id (:id turn)
           :user-request (:user-request turn)
           :status (:status turn)
           :attempts attempts
           :failures (failures-from-iterations db-info iterations)
           :iteration (iteration-pointer env)
           :cost (turn-cost-summary turn)}
          (elapsed-ms turn)
          (assoc :elapsed-ms (elapsed-ms turn)))))))

(defn- session-snapshot
  "Map for a single session: identity + every turn rolled up to a
   compact `{:id :user-request :outcome :answer :iteration-count :status}`
   shape. Used by the session-summary portion of inspect.

   `:iteration-count` is the integer number of LLM rounds the turn
   consumed. Spelled out so it never gets confused with the vector
   shape that the runtime trace uses for per-iteration entries."
  [db-info session-id]
  (when (and db-info session-id)
    (try
      (when-let [session (vis/db-get-session db-info session-id)]
        (let
          [turn-rows (vis/db-list-session-turns db-info session-id)
           turns (mapv (fn [turn]
                         (cond-> {:id (:id turn) :outcome (or (:prior-outcome turn) (:status turn))}
                           (:user-request turn)
                           (assoc :user-request (:user-request turn))

                           (:answer turn)
                           (assoc :answer (:answer turn))

                           (:iteration-count turn)
                           (assoc :iteration-count (:iteration-count turn))

                           (:total-cost turn)
                           (assoc :total-cost (:total-cost turn))))
                       turn-rows)]

          (cond->
            {:id session-id
             :channel (:channel session)
             :title (:title session)
             :model (:model session)
             :created-at (:created-at session)
             :turns turns
             :turn-count (count turns)}
            (:provider session)
            (assoc :provider (:provider session))

            (format-provider-model (:provider session) (:model session))
            (assoc :provider-model (format-provider-model (:provider session) (:model session))))))
      (catch Throwable _ nil))))

;; ---------------------------------------------------------------------------
;; Meta fns - each takes `env` as first arg via the shared
;; `:before-fn` injector below. The agent never sees `env`; it calls
;; e.g. current-turn snapshot with zero args.
;; ---------------------------------------------------------------------------

(defn- foundation-turn [env] (turn-snapshot env))

(defn- foundation-session
  "Snapshot for a session. The in-flight turn (= current `TURN_ID`)
   is automatically excluded from `:turns` because its `:iteration-count` /
   `:total-cost` haven't been finalized yet - listing it would render
   as `null | $null` and confuse downstream summaries. The excluded id
   is surfaced as `:in-flight-turn-id` so callers can opt into seeing
   it. Filtering happens only when the in-flight turn belongs to this
   session; foreign sessions are returned verbatim."
  ([env] (foundation-session env (current-session-id env)))
  ([env session-id]
   (when-let [snapshot (session-snapshot (:db-info env) session-id)]
     (let
       [in-flight-id (current-session-turn-id env)
        same-session? (and in-flight-id (same-uuid? session-id (current-session-id env)))]

       (if-not same-session?
         snapshot
         (let [filtered (filterv #(not (same-uuid? in-flight-id (:id %))) (:turns snapshot))]
           (-> snapshot
               (assoc :turns filtered)
               (assoc :turn-count (count filtered))
               (assoc :in-flight-turn-id in-flight-id))))))))

(defn- foundation-sessions-data
  "List every session the DB knows about, newest-first. With no
   arg, scans every channel surfaced by `known-channels`. With a
   channel kw, filters to that channel. Returns `[]` (never nil) when
   the env is missing a `:db-info` handle so callers can chain seq
   operations safely."
  ([env]
   (if (:db-info env)
     (vec (sort-by (comp #(if-let [c (:created-at %)]
                            (cond (inst? c) (- (long (inst-ms c)))
                                  (integer? c) (- (long c))
                                  :else 0) 0)
                         identity)
                   (mapcat #(foundation-sessions-data env %) (known-channels))))
     []))
  ([env channel]
   (if (:db-info env)
     (try (mapv (fn [session]
                  (let
                    [session-id
                     (:id session)

                     turns
                     (try (vis/db-list-session-turns (:db-info env) session-id)
                          (catch Throwable _ []))

                     modified-at
                     (or (->> turns
                              (keep :created-at)
                              (sort-by #(if (inst? %) (inst-ms %) 0))
                              last)
                         (:created-at session))]

                    (cond->
                      {:id session-id
                       :channel (:channel session)
                       :title (:title session)
                       :created-at (:created-at session)
                       :modified-at modified-at
                       :turn-count (count turns)}
                      (:external-id session)
                      (assoc :external-id (:external-id session)))))
                (vis/db-list-sessions (:db-info env) channel))
          (catch Throwable _ []))
     [])))

(defn- foundation-session-forks
  "List every `session_state` row for the session soul behind
   `session-id`, oldest version first. Each row maps to
   `{:state-id :version :parent-state-id :title :system-prompt :provider
     :model :created-at :turn-count}`. The trunk is `:version 0` with
   `:parent-state-id nil`; any later row with non-nil `:parent-state-id`
   is a fork off the referenced state. Returns `[]` (never nil) when
   the session has no rows OR the env is missing handles - lets
   callers chain `(group-by :parent-state-id ...)` without nil-guards.

   No-arg form uses the current-session-id from the env."
  ([env] (foundation-session-forks env (current-session-id env)))
  ([env session-id]
   (if (and (:db-info env) session-id)
     (try (vec (vis/db-list-session-states (:db-info env) session-id)) (catch Throwable _ []))
     [])))

(defn- meta-turn-retries
  "List every `session_turn_state` row (= every retry version) for the turn
   soul behind `session-turn-id`, oldest version first. Each row maps to
   `{:state-id :version :forked-from-session-turn-state-id :status :prior-outcome
     :provider :model :created-at :iteration-count}`. Version 0 with
   `:forked-from-session-turn-state-id nil` is the original run; any higher
   version is a retry. `session-turn-id` is a `session_turn_soul` UUID - the same id
   surfaced as `:turn-id` by attempt search, `:id` by the current-turn
   snapshot, or `:turns[].id` by the session summary. Returns `[]` (never nil)
   when the turn is unknown or the env is missing handles."
  [env session-turn-id]
  (if (and (:db-info env) session-turn-id)
    (try (vec (vis/db-list-session-turn-states (:db-info env) session-turn-id))
         (catch Throwable _ []))
    []))

(defn- foundation-failures
  "Provider/schema and code/tool failures, normalized into one
   chronological vector. No arg = current turn. Pass a session id
   to scan every turn in that session. To scan EVERY session
   in the DB use the DB-wide helper instead. Returns `[]`
   (never nil) when there is nothing to report or the env is missing
   handles."
  ([env] (or (:failures (turn-snapshot env)) []))
  ([env session-id]
   (if (and (:db-info env) session-id)
     (try (vec (mapcat (fn [turn]
                         (let [iterations (iteration-rows (:db-info env) (:id turn))]
                           (mapv #(assoc %
                                    :turn-id (:id turn)
                                    :user-request (:user-request turn))
                                 (failures-from-iterations (:db-info env) iterations))))
                       (vis/db-list-session-turns (:db-info env) session-id)))
          (catch Throwable _ []))
     [])))

(defn- classification-counts
  [failures]
  (into {}
        (map (fn [[classification total]]
               [classification total]))
        (frequencies (map :classification failures))))

(def ^:private ^:const REPETITION_THRESHOLD
  "Minimum number of failures sharing the same normalized signature
   before the turn is flagged as locked in a same-error loop. Empirical
   floor: agents that miss a path 2-3x and pivot stay below; agents
   that emit 5+ identical-root-cause errors are stuck and not learning.
   Anchored to a worst-case self-analyze report (148 'src/tui not
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
  (let
    [message
     (or (:message failure) "")

     head
     (or (some-> (re-find #"^([^:\n]{1,80}):" message)
                 second
                 str/trim)
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
                 {:signature signature :count (count group) :sample (first group)})))
       (sort-by :count >)
       vec))

(defn- next-actions
  [failures clusters]
  (let [classes (set (map :classification failures))]
    (vec
      (cond-> []
        ;; Repetition loop is the loudest signal and goes first so
        ;; the agent reads it before any classification-specific tip.
        ;; Sample message is truncated; full failure stays in :failures.
        (seq clusters)
        (into (mapv (fn [{:keys [count sample]}]
                      (str "Same error repeated " count
                           "x this turn (e.g. " (preview (:message sample) 80)
                           "). STOP varying inputs to the failing call. "
                           "Switch strategy: list a parent directory, broaden "
                           "the search, or pivot - repeating the same shape " "will not converge."))
                    clusters))

        (contains? classes :provider-schema-rejected)
        (conj
          "Treat schema rejection as provider noise, not a reason to inspect SQLite. Use raw_preview from session_state()[\"failures\"] and retry/switch model only if it repeats.")

        (contains? classes :regex-unsupported-escape)
        (conj
          (str
            "rg takes one options dict with list values, not regex strings or positional args. "
            "Use rg({\"all\": [\"foo|bar\"]}) for literal pipe text, or rg({\"any\": [\"foo\", \"bar\"]}) for OR. "
            "Add \"paths\" and \"include\" in the same dict."))

        (contains? classes :regex-unescaped-quote)
        (conj
          "Fix the quoted regex string; an inner quote escaped poorly and exposed a bare symbol.")

        (contains? classes :patch-invalid-program)
        (conj
          "Re-emit patch as a vector of {:path :from_anchor :replace} maps; use a triple-quoted Python string for multi-line replacement text.")

        (contains? classes :patch-stale-anchor)
        (conj
          "Re-read the smallest file slice and retry once with its fresh lineno:hash anchor.")))))

(defn- foundation-diagnose
  "Compact current-turn diagnosis built from failure data. Returns a
   map with counts, repetition-loop detection, and next actions so the
   agent can stop burning iterations on DB spelunking. Pass a
   session id to diagnose all turns in that session.

   `:repetition-loop?` is `true` when any error signature repeats at
   least `REPETITION_THRESHOLD` times in the failure list - the
   single-glance flag for the 'agent retried the same broken call N
   times' pathology. `:repetition-clusters` carries the supporting
   data (signature, count, sample failure)."
  ([env]
   (let
     [turn
      (turn-snapshot env)

      failures
      (vec (:failures turn))

      clusters
      (repetition-clusters failures)]

     {:turn-id (:id turn)
      :user-request (:user-request turn)
      :status (:status turn)
      :failure-count (count failures)
      :by-classification (classification-counts failures)
      :repetition-loop? (boolean (seq clusters))
      :repetition-clusters clusters
      :failures failures
      :next-actions (next-actions failures clusters)}))
  ([env session-id]
   (let
     [failures
      (vec (foundation-failures env session-id))

      clusters
      (repetition-clusters failures)]

     {:session-id session-id
      :failure-count (count failures)
      :by-classification (classification-counts failures)
      :repetition-loop? (boolean (seq clusters))
      :repetition-clusters clusters
      :failures failures
      :next-actions (next-actions failures clusters)})))

(defn- safe-call
  [f default]
  (try (let [v (f)]
         (if (nil? v) default v))
       (catch Throwable _ default)))

(defn- retries-by-turn
  [env turns]
  (into {}
        (keep (fn [{:keys [id]}]
                (when id [id (meta-turn-retries env id)])))
        turns))

(defn- foundation-inspect-data
  "Canonical session-state data surface. One read returns the
   navigation summary, live current turn, classified failures,
   diagnosis, fork/retry metadata, and the full transcript payload.
   Default target is the current session; pass a session id or
   unambiguous prefix to inspect another session."
  [env session-id]
  (let
    [target-id
     (or session-id (:session-id env))

     transcript-data
     (safe-call #(transcript/transcript (:db-info env) target-id) nil)

     resolved-id
     (or (get-in transcript-data [:session :id]) target-id)

     session-summary
     (safe-call #(foundation-session env resolved-id) nil)

     failures
     (safe-call #(foundation-failures env resolved-id) [])

     diagnosis
     (safe-call #(foundation-diagnose env resolved-id) {})

     forks
     (safe-call #(foundation-session-forks env resolved-id) [])

     turn-retries
     (safe-call #(retries-by-turn env (:turns transcript-data)) {})]

    {:schema-version 1
     :scope :session
     :session-id resolved-id
     :session-index (safe-call #(foundation-sessions-data env) [])
     :session session-summary
     :current-turn (safe-call #(foundation-turn env) nil)
     :failures failures
     :diagnosis diagnosis
     :session-forks forks
     :turn-retries turn-retries
     :transcript transcript-data}))

;; ---------------------------------------------------------------------------
;; Strings-only boundary egress. session_state / sessions / session_report_html
;; are MODEL-FACING verbs: the `:result` they return crosses the Clojure->Python
;; boundary, which throws on any keyword/symbol key or value. The internal
;; builders + the transcript projection stay idiomatic keyword Clojure (the
;; Markdown renderer reads them back by keyword); we stringify ONCE at the verb
;; exit, mirroring `env-python/kw->snake` (kebab->snake, trailing `?`/`!`
;; stripped, namespace folded with `_`) so the model reads the SAME snake_case
;; keys/enum values it saw when the boundary itself did the conversion.
;; ---------------------------------------------------------------------------

(defn- kw->snake
  ^String [k]
  (-> (if (namespace k) (str (namespace k) "_" (name k)) (name k))
      (str/replace "-" "_")
      (str/replace #"[?!]$" "")))

(defn- boundary-key
  [k]
  (cond (string? k) k
        (keyword? k) (kw->snake k)
        (symbol? k) (kw->snake (keyword k))
        :else (str k)))

(defn- deep-stringify
  "Rebuild a value into the strings-only boundary shape: map KEYS and
   keyword/symbol VALUES become snake_case strings, collections recurse.
   UUID/Date/Temporal leaves are left for the real boundary (`->py`) to
   ISO-stringify."
  [x]
  (cond (map? x) (reduce-kv (fn [m k v]
                              (assoc m (boundary-key k) (deep-stringify v)))
                            {}
                            x)
        (or (vector? x) (seq? x) (set? x)) (mapv deep-stringify x)
        (keyword? x) (kw->snake x)
        (symbol? x) (kw->snake (keyword x))
        :else x))

(defn- session-envelope [op result] (extension/success {:op op :result (deep-stringify result)}))

(defn- foundation-inspect
  "Canonical session-state data surface. Returns a Vis tool envelope;
   sandbox callers receive the unwrapped data map."
  ([env] (foundation-inspect env (:session-id env)))
  ([env session-id] (session-envelope :session-state (foundation-inspect-data env session-id))))

(defn- foundation-report-html
  "Render the same canonical data returned by `foundation-inspect` as a
   standalone, vis-light-styled HTML document. Returns a Vis tool
   envelope; sandbox callers receive the unwrapped string."
  ([env] (foundation-report-html env (:session-id env)))
  ([env session-id]
   (let
     [data
      (foundation-inspect-data env session-id)

      report
      (if-let [transcript-data (:transcript data)]
        (transcript/transcript->html transcript-data)
        (str "Session not found: " (:session-id data) "\n"))]

     (session-envelope :session-report-html report))))

(defn- foundation-sessions
  "Envelope-wrapped session index (see `foundation-sessions-data`).
   Returns a Vis tool envelope; sandbox callers receive the unwrapped
   vector. The raw-data fn stays separate because `foundation-inspect-data`
   EMBEDS the index inside its own envelope — the guard
   (`assert-symbol-envelope!`) rejects a bare vector, which is exactly how
   `sessions()` was broken for every caller (session 9c829d10)."
  ([env] (session-envelope :sessions (foundation-sessions-data env)))
  ([env channel] (session-envelope :sessions (foundation-sessions-data env channel))))

;; Removed extra workflow surfaces.

;; ---------------------------------------------------------------------------
;; Session-state IR render helpers
;;
;; Symbol introspection is not a foundation `v/` tool: the engine owns it
;; as the bare `doc` / `apropos` system calls wired into the Python
;; sandbox, since those describe the sandbox itself.
;; ---------------------------------------------------------------------------

(defn- inject-environment [env f args] {:env env :fn f :args (into [env] args)})

;; -- public, doc-bearing aliases -----------------------------------------------
;; The underlying defs (`foundation-inspect`, `foundation-report-html`) are
;; private and named for clarity inside this ns. Re-export them under their
;; sandbox-visible names with `:doc` and `:arglists` baked into the var meta so
;; `vis/symbol` can read both straight off the var.
(def
  ^{:doc
    "await session_state(session_id)  # investigate ANOTHER conversation
Returns {\"session\" (identity + per-turn rollup), \"current_turn\", \"failures\", \"diagnosis\", \"session_forks\", \"turn_retries\", \"transcript\", ...}. The rich one is `transcript`: `[\"totals\"]` (turns/iterations/tokens/cost) and `[\"turns\"]` = [{id, user_request, answer, status, iteration_count, tokens, cost_usd, iterations:[{position, status, blocks:[code/result]}]}] — iterate it in python_execution to gather answers, grep code, or diff cost; slice, don't dump.
Pick keys; the whole dict stays bound. No-arg defaults to the current session, but for
THIS conversation the live `session` bag already has turn, scope, utilization,
workspace, and tool state. It is also the recovery path for a folded current-session
turn: select `['transcript']['turns']` by numeric `position`. For OTHER sessions,
use sessions() for the index, then pass that session id."
    :arglists '([] [session-id])}
  session-state
  foundation-inspect)
(def
  ^{:doc
    "await session_report_html(session_id)  # standalone HTML report for ANOTHER conversation
Returns a self-contained HTML document (vis-light themed): every turn, iteration, code,
result, answer - the same transcript data, styled to match the web TUI. Write it
to a file to open in a browser. Most useful for OTHER sessions."
    :arglists '([] [session-id])}
  session-report-html
  foundation-report-html)
(def
  ^{:doc
    "await sessions()  # index of EVERY past conversation, newest-first
Returns [{\"id\", \"channel\", \"title\", \"turn_count\", \"created_at\", \"modified_at\"} ...],
newest-first; `modified_at` is the latest turn's timestamp (falls back to `created_at` for
empty sessions). Pass a channel keyword to filter. Take an id from here into
session_state(id) to investigate that conversation."
    :arglists '([] [channel])}
  sessions
  foundation-sessions)

(def session-state-symbol
  (vis/symbol #'session-state {:before-fn inject-environment :tag :observation}))

(def session-report-html-symbol
  (vis/symbol #'session-report-html {:before-fn inject-environment :tag :observation}))

(def sessions-symbol (vis/symbol #'sessions {:before-fn inject-environment :tag :observation}))

(def all-symbols [session-state-symbol session-report-html-symbol sessions-symbol])

;; The extension that owns all `v/`-aliased symbols is built
;; and registered by `com.blockether.vis.internal.foundation.core`,
;; not here - this namespace only exposes doc-bearing symbols.
