(ns com.blockether.vis.internal.foundation.introspection
  "Programmatic introspection of the agent's own state from inside
   `:code`. The public state surface is deliberately small:

   - `(session-state [session-id])` -> canonical data map, including usage and raw LLM diagnostics
   - `(sessions [channel])` -> metadata-only session index

   Everything else in this namespace is implementation detail. The agent
   gets the data once and manipulates it with ordinary Python collection
   operations when filtering or presentation is needed.

   Every function is a pure read off the same DB tables the projection
   layer reads from (or a classpath read for the doc accessors).
   Failures return nil/[], never throw, so a misbehaving introspection
   call cannot break iteration execution.

   Gated: the extension registered at the bottom of this namespace binds its
   symbols and prompt only while the `introspection` toggle is ON (default OFF)."
  (:require [charred.api :as json]
            [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.internal.foundation.transcript :as transcript]
            [com.blockether.vis.internal.extension :as extension]
            [com.blockether.vis.internal.persistance :as persistance]))

;; ---------------------------------------------------------------------------
;; Channels we know how to enumerate. Derived from the global channel
;; registry (`vis/registered-channels`) so any third-party channel jar
;; on the classpath surfaces in the inspect session index automatically -
;; no edits to this file when a new front-end ships.
;;
;; `:cli` is added unconditionally because the CLI agent uses `:cli` as
;; its sessions-channel namespace WITHOUT registering a channel
;; descriptor (the `vis` dispatcher itself is the surface; there is no
;; `vis-agent channels cli` sub-command, so it has no `:channel/cmd`). Every
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

(defn- iteration-forms
  "The executed forms of one iteration row.

   Results and errors live ONLY inside the persisted `:forms` envelope:
   the row projection carries no top-level `:result`/`:error` column, so
   `(:error iteration)` is nil even for an iteration that blew up."
  [iteration]
  (let [forms (:forms iteration)]
    (if (sequential? forms) (vec forms) [])))

(defn- attempts-from-iterations
  "Walk `iterations` (in DB order) and collect every executed
   expression. Used by the current-turn snapshot and by attempt search.

   One entry per FORM, not per iteration: an iteration is an envelope
   around the forms the model emitted, and the code/result/error of a
   round hang off those forms."
  [_db-info iterations]
  (into []
        (mapcat
          (fn [iteration]
            (let
              [base
               {:iteration-id (:id iteration)
                :iteration (:position iteration)
                :duration-ms (:duration-ms iteration)}

               forms
               (iteration-forms iteration)]

              (if (seq forms)
                (mapv (fn [form]
                        (cond->
                          (assoc base
                            :code (or (:src form) (:code iteration))
                            :result (:result form)
                            :error (:error form))
                          (:vis/tool-name form)
                          (assoc :tool (:vis/tool-name form))

                          (:duration-ms form)
                          (assoc :duration-ms (:duration-ms form))))
                      forms)
                [(assoc base
                   :code (:code iteration)
                   :result nil
                   :error nil)]))))
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

(defn- cancellation-failure?
  "True when an error is fallout from interrupting a cancelled turn, rather than
   a defect in the form that happened to be on the stack."
  [lower-message]
  (or (str/includes? lower-message "interruptedexception")
      (str/includes? lower-message "cancellationexception")
      (str/includes? lower-message "closedbyinterruptexception")
      (str/includes? lower-message "interrupted while")
      (str/includes? lower-message "sleep interrupted")))

(defn- classify-expression-failure
  [code error]
  (let
    [message
     (or (error-text error) "")

     lower-message
     (str/lower-case message)

     tool-name
     (or (tool-name-from-code code) "")]

    (cond (cancellation-failure? lower-message) :turn-cancelled
          (and (str/includes? tool-name "rg")
               (str/includes? lower-message "unsupported escape character"))
          :regex-unsupported-escape
          (and (str/includes? tool-name "rg")
               (str/includes? lower-message "unable to resolve symbol"))
          :regex-unescaped-quote
          (and (str/includes? tool-name "patch")
               (str/includes? lower-message "unmatched delimiter"))
          :struct-patch-invalid-code
          (str/includes? lower-message "unable to resolve symbol") :unresolved-symbol
          :else :code-execution-error)))

(defn- advice-for-classification
  [classification]
  (case classification
    :provider-schema-rejected
    "Provider returned prose/string instead of the iteration map. Skip the SQLite trip - the raw preview is already here. Continue after the built-in schema retry, or switch model when this repeats."

    :regex-unsupported-escape
    "grep matches literal substrings by default — no regex escaping needed. Pass a list of terms for OR: grep([\"foo\", \"bar\"]). Regex is not supported — filter the matches in Python."

    :regex-unescaped-quote
    "The regex string likely contains an unescaped inner quote. Escape it as \\\" or use a regex literal / simpler pattern."

    :struct-patch-invalid-code
    "The struct_patch `code` likely lost the closing quote or a delimiter. Re-emit it with a Python triple-quoted string for multi-line content."

    :turn-cancelled
    "Not a code defect: the turn was cancelled and the interrupt surfaced on the frame that was running. Re-run the interrupted step if you still need it."

    :unresolved-symbol
    "A reader/string boundary probably split the form and exposed a bare symbol. Check quoting before retrying."

    "Read :message, :code, and :iteration; fix the smallest failing form before issuing new searches."))

(defn- expression-failures-for-iteration
  "Curated failure entries for one iteration - one per errored FORM.
   The error is a property of the form, never of the iteration row."
  [_db-info iteration]
  (into []
        (keep (fn [form]
                (when-let [error (:error form)]
                  (let
                    [code (or (:src form) (:code iteration))
                     classification (classify-expression-failure code error)]

                    {:source :code
                     :iteration-id (:id iteration)
                     :iteration (:position iteration)
                     :tool (or (:vis/tool-name form) (tool-name-from-code code))
                     :classification classification
                     :code code
                     :message (error-text error)
                     :advice (advice-for-classification classification)}))))
        (iteration-forms iteration)))

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
   instead of querying SQLite manually.

   Takes the session to snapshot explicitly: inspecting another
   session must report THAT session's latest turn, not the caller's
   own live one. The iteration pointer is env-local runtime state,
   so it only rides along when the inspected session IS this one."
  ([env] (turn-snapshot env (current-session-id env)))
  ([env session-id]
   (let [{:keys [db-info]} env]
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
            :cost (turn-cost-summary turn)}
           (same-uuid? session-id (current-session-id env))
           (assoc :iteration (iteration-pointer env))

           (elapsed-ms turn)
           (assoc :elapsed-ms (elapsed-ms turn))))))))

(defn- session-snapshot
  "Map for a single session: identity + every persisted turn rolled up to a
   compact `{:id :user-request :outcome :answer :iteration-count}` shape.

   Iteration rows are authoritative while a turn is live: the denormalized
   count on `session_turn_soul` is finalized only when the turn settles. Reading
   the rows here keeps this summary consistent with `transcript.turns` for both
   current and foreign live sessions."
  [db-info session-id]
  (when (and db-info session-id)
    (try (when-let [session (vis/db-get-session db-info session-id)]
           (let
             [turn-rows (vis/db-list-session-turns db-info session-id)
              turns (mapv (fn [turn]
                            (let [iteration-count (count (iteration-rows db-info (:id turn)))]
                              (cond->
                                {:id (:id turn)
                                 :outcome (or (:prior-outcome turn) (:status turn))
                                 :iteration-count iteration-count}
                                (:user-request turn)
                                (assoc :user-request (:user-request turn))

                                (:answer turn)
                                (assoc :answer (:answer turn))

                                (:total-cost turn)
                                (assoc :total-cost (:total-cost turn)))))
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
               (assoc :provider-model
                 (format-provider-model (:provider session) (:model session))))))
         (catch Throwable _ nil))))

;; ---------------------------------------------------------------------------
;; Meta fns take `env` as their first argument via declarative `:inject-env?`.
;; The agent never sees `env`; it calls e.g. the current-turn snapshot with zero args.
;; ---------------------------------------------------------------------------

(defn- foundation-turn
  ([env] (turn-snapshot env))
  ([env session-id] (turn-snapshot env session-id)))

(defn- foundation-session
  "Snapshot for a session, including its live turn and every iteration row that
   has already completed. The richer `:current-turn` projection separately
   carries the env-local pointer for the provider round still in flight."
  ([env] (foundation-session env (current-session-id env)))
  ([env session-id] (session-snapshot (:db-info env) session-id)))

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
               (when (>= (count group) (long REPETITION_THRESHOLD))
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
        (conj (str "grep takes a term or a list of terms (OR), not regex strings. "
                   "Use grep([\"foo\", \"bar\"]) for OR; filter complex matches in Python. "
                   "Add \"paths\" and \"include\" in the same dict."))

        (contains? classes :regex-unescaped-quote)
        (conj
          "Fix the quoted regex string; an inner quote escaped poorly and exposed a bare symbol.")

        (contains? classes :struct-patch-invalid-code)
        (conj
          "Re-emit struct_patch with balanced `code`; use a triple-quoted Python string for multi-line replacement text.")))))

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

(declare safe-call session-envelope)

(defn- usage-tokens
  [iteration]
  (let
    [input
     (long (or (:input-tokens iteration) 0))

     cached
     (long (or (:input-cache-read-tokens iteration) 0))]

    {:input input
     :cached cached
     :uncached (max 0 (- input cached))
     :cache-created (long (or (:input-cache-write-tokens iteration) 0))
     :output (long (or (:output-tokens iteration) 0))
     :reasoning (long (or (:output-reasoning-tokens iteration) 0))}))

(defn- empty-usage
  []
  {:iterations 0
   :tokens {:input 0 :cached 0 :uncached 0 :cache-created 0 :output 0 :reasoning 0}
   :cost-usd 0.0})

(defn- add-iteration-usage
  [total usage]
  (-> total
      (update :iterations inc)
      (update-in [:tokens :input] + (get-in usage [:tokens :input]))
      (update-in [:tokens :cached] + (get-in usage [:tokens :cached]))
      (update-in [:tokens :uncached] + (get-in usage [:tokens :uncached]))
      (update-in [:tokens :cache-created] + (get-in usage [:tokens :cache-created]))
      (update-in [:tokens :output] + (get-in usage [:tokens :output]))
      (update-in [:tokens :reasoning] + (get-in usage [:tokens :reasoning]))
      (update :cost-usd + (:cost-usd usage))))

(defn- tool-call-status
  [form]
  (cond (:timeout? form) :timeout
        (:error form) :error
        (false? (:success? form)) :error
        :else :done))

(defn- native-tool-calls
  [turn iteration]
  (->> (:forms iteration)
       (keep-indexed (fn [index form]
                       (when-let [tool (:vis/tool-name form)]
                         {:tool (str tool)
                          :turn (:position turn)
                          :iteration (:position iteration)
                          :form (unchecked-inc (long index))
                          :status (tool-call-status form)
                          :error (:error form)})))
       vec))

(defn- tool-outcomes
  [calls]
  (reduce (fn [outcomes {:keys [status]}]
            (update outcomes status (fnil inc 0)))
          {:done 0 :error 0 :timeout 0}
          calls))

(defn- tool-failure? [call] (not= :done (:status call)))

(defn- usage-tool-error
  [{:keys [tool turn iteration form status error]}]
  {:tool tool
   :turn turn
   :iteration iteration
   :form form
   :status status
   :message (preview (or (error-text error)
                         (when (= :timeout status) "Tool execution timed out")
                         "Tool reported an unsuccessful result")
                     300)})

(defn- usage-tool-errors
  [iterations]
  (let
    [errors
     (->> iterations
          (mapcat :tool-call-statuses)
          (filter tool-failure?)
          vec)

     limit
     20]

    {:tool-errors (mapv usage-tool-error (take limit errors))
     :tool-errors-truncated? (> (count errors) limit)}))

(defn- add-tool-usage
  [total usage]
  (-> total
      (update :tool-calls + (:tool-call-count usage))
      (update :tool-errors + (:tool-error-count usage))
      (update :tool-outcomes #(merge-with + % (:tool-outcomes usage)))))

(defn- empty-tool-usage [] {:tool-calls 0 :tool-errors 0 :tool-outcomes (tool-outcomes [])})

(defn- usage-total
  [iterations]
  (merge (reduce add-iteration-usage (empty-usage) iterations)
         (reduce add-tool-usage (empty-tool-usage) iterations)))

(defn- public-usage-iteration [usage] (dissoc usage :tool-call-statuses :routing-trace))

(def ^:private routing-event-limit 20)

(defn- route
  [{:keys [provider model]}]
  (let
    [provider
     (cond (keyword? provider) (name provider)
           (some? provider) (str provider))

     model
     (some-> model
             str
             not-empty)]

    (when model
      {:provider (some-> provider
                         not-empty)
       :model model})))

(defn- iteration-route
  "Return a normalized route from either the hydrated routing map or the
   flattened typed columns used by persisted iteration rows."
  [iteration route-key]
  (or (route (get iteration route-key))
      (let [prefix (name route-key)]
        (route {:provider (get iteration (keyword (str prefix "-provider")))
                :model (get iteration (keyword (str prefix "-model")))}))))

(defn- routing-event?
  [event type]
  (let
    [actual
     (or (:event/type event) (:type event))

     persisted-type
     (if-let [ns (namespace type)]
       (str ns "_" (name type))
       (name type))]

    (or (= type actual)
        ;; Wire serialization flattens a namespaced keyword's slash into an
        ;; underscore while retaining the event name's hyphens.
        (= persisted-type
           (some-> actual
                   name)))))

(defn- usage-routing-events
  [usage]
  (mapv (fn [event]
          (cond-> {:turn (:turn usage) :iteration (:iteration usage) :type (:event/type event)}
            (:status event)
            (assoc :status (:status event))

            (:reason event)
            (assoc :reason (:reason event))

            (:attempt event)
            (assoc :attempt (:attempt event))

            (:retry event)
            (assoc :retry (:retry event))

            (:delay-ms event)
            (assoc :delay-ms (:delay-ms event))

            (:backoff-ms event)
            (assoc :backoff-ms (:backoff-ms event))

            (:error event)
            (assoc :message
              (preview (or (error-text (:error event)) "Provider routing failed") 300))))
        (:routing-trace usage)))

(defn- usage-route-rows
  [iterations k]
  (->> iterations
       (keep (fn [usage]
               (when-let [r (route (get usage k))]
                 [r usage])))
       (reduce (fn [rows [r usage]]
                 (update rows r #(add-iteration-usage (or % (merge (empty-usage) r)) usage)))
               {})
       vals
       (sort-by (juxt :provider :model))
       vec))

(defn- usage-routing-transitions
  [iterations]
  (->> iterations
       (keep (fn [usage]
               (let
                 [from
                  (route (:llm-selected usage))

                  to
                  (route (:llm-actual usage))]

                 (when (and from to (not= from to)) [from to]))))
       frequencies
       (map (fn [[[from to] count]]
              {:from from :to to :count count}))
       (sort-by #(pr-str [(:from %) (:to %)]))
       vec))

(defn- usage-routing
  [iterations]
  (let
    [events
     (vec (mapcat usage-routing-events iterations))

     retries
     (count (filter #(routing-event? % :llm.routing/provider-retry) events))]

    {:selected (usage-route-rows iterations :llm-selected)
     :actual (usage-route-rows iterations :llm-actual)
     :fallbacks (count (filter :llm-fallback? iterations))
     :retries retries
     :transitions (usage-routing-transitions iterations)
     :events (vec (take routing-event-limit events))
     :events-truncated? (> (long (count events)) (long routing-event-limit))}))

(defn- manual-model-switches
  [db-info sid]
  (let
    [switches (->> (safe-call
                     #(persistance/db-list-extension-aggregates
                        db-info
                        {:extension-id "vis" :kind :session-model-switch :session-soul-id sid})
                     [])
                   (keep (fn [{:keys [content created-at]}]
                           (when (map? content)
                             (assoc (select-keys content [:from :to :source])
                               :at-ms (some-> ^java.util.Date created-at
                                              .getTime)))))
                   vec)]
    {:manual-switches (vec (take routing-event-limit switches))
     :manual-switches-truncated? (> (long (count switches)) (long routing-event-limit))}))

(defn- usage-iteration
  [turn iteration]
  (let
    [calls
     (native-tool-calls turn iteration)

     outcomes
     (tool-outcomes calls)

     routing-trace
     (vec (or (:llm-routing-trace iteration) []))]

    {:turn (:position turn)
     :iteration (:position iteration)
     :status (:status iteration)
     :tokens (usage-tokens iteration)
     :cost-usd (double (or (:cost-usd iteration) 0.0))
     :tools (vec (distinct (map :tool calls)))
     :tool-calls (mapv :tool calls)
     :tool-call-count (count calls)
     :tool-error-count (count (filter tool-failure? calls))
     :tool-outcomes outcomes
     :llm-selected (iteration-route iteration :llm-selected)
     :llm-actual (iteration-route iteration :llm-actual)
     :llm-fallback? (boolean (:llm-fallback? iteration))
     :provider-retries (count (filter #(routing-event? % :llm.routing/provider-retry)
                                      routing-trace))
     ;; These stay private while aggregate rows and bounded samples are derived.
     :tool-call-statuses calls
     :routing-trace routing-trace}))

(defn- add-tool-row
  [summary usage tool calls]
  (let
    [summary (or summary
                 (assoc (empty-usage)
                   :tool tool
                   :calls 0
                   :errors 0
                   :outcomes (tool-outcomes [])))]
    (-> (add-iteration-usage summary usage)
        (update :calls + (count calls))
        (update :errors + (count (filter tool-failure? calls)))
        (update :outcomes #(merge-with + % (tool-outcomes calls))))))

(defn- usage-tools
  "Group model iterations by every native tool they invoked. Usage intentionally
   overlaps across tools, so grouped rows must not be summed."
  [iterations]
  (let
    [by-tool (reduce (fn [by-tool usage]
                       (reduce-kv (fn [by-tool tool calls]
                                    (update by-tool tool #(add-tool-row % usage tool calls)))
                                  by-tool
                                  (group-by :tool (:tool-call-statuses usage))))
                     {}
                     iterations)]
    (->> by-tool
         vals
         (sort-by :tool)
         vec)))

(defn- foundation-usage-data
  "Compact token/cost, tool-outcome, and provider-routing ledger. It deliberately
   omits messages, code, results, and raw provider payloads."
  [env session-id]
  (let
    [target-id
     (or session-id (:session-id env))

     data
     (safe-call #(transcript/transcript (:db-info env) target-id) nil)

     empty-routing
     (merge (usage-routing []) (manual-model-switches (:db-info env) target-id))]

    (if-not data
      {:schema-version 1
       :scope :session-usage
       :session-id target-id
       :session nil
       :totals (assoc (merge (empty-usage) (empty-tool-usage)) :turns 0)
       :turns []
       :tools []
       :tool-errors []
       :tool-errors-truncated? false
       :routing empty-routing}
      (let
        [turns
         (mapv (fn [turn]
                 (let
                   [iterations
                    (mapv (partial usage-iteration turn) (:iterations turn))

                    totals
                    (usage-total iterations)]

                   {:position (:position turn)
                    :status (:status turn)
                    :iteration-count (count iterations)
                    :tokens (:tokens totals)
                    :cost-usd (:cost-usd totals)
                    :tool-calls (:tool-calls totals)
                    :tool-errors (:tool-errors totals)
                    :tool-outcomes (:tool-outcomes totals)
                    :iterations iterations}))
               (:turns data))

         iterations
         (vec (mapcat :iterations turns))

         {:keys [tool-errors tool-errors-truncated?]}
         (usage-tool-errors iterations)

         routing
         (merge (usage-routing iterations) (manual-model-switches (:db-info env) target-id))]

        {:schema-version 1
         :scope :session-usage
         :session-id (get-in data [:session :id])
         :session (select-keys (:session data) [:id :title :channel :provider :model])
         :totals (assoc (usage-total iterations) :turns (count turns))
         :turns (mapv #(update % :iterations (partial mapv public-usage-iteration)) turns)
         :tools (usage-tools iterations)
         :tool-errors tool-errors
         :tool-errors-truncated? tool-errors-truncated?
         :routing routing}))))


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
   diagnosis, fork/retry metadata, compact usage ledger, and the full
   transcript payload. Default target is the current session; pass a
   session id or unambiguous prefix to inspect another session."
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
     (safe-call #(retries-by-turn env (:turns transcript-data)) {})

     usage
     (safe-call #(foundation-usage-data env resolved-id) {})]

    {:schema-version 1
     :scope :session
     :session-id resolved-id
     :session-index (safe-call #(foundation-sessions-data env) [])
     :session session-summary
     :current-turn (safe-call #(foundation-turn env resolved-id) nil)
     :failures failures
     :diagnosis diagnosis
     :session-forks forks
     :turn-retries turn-retries
     :usage usage
     :transcript transcript-data}))

;; ---------------------------------------------------------------------------
;; Strings-only boundary egress. session_state / sessions
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
        ;; Force any lazy leaf (e.g. a `<-json-lazy` DELAY) so no unrealized
        ;; deref crosses the boundary as a ForeignObject and breaks json.dumps.
        ;; Degrade a bad blob to nil rather than aborting the whole verb.
        (delay? x) (deep-stringify (try (force x) (catch Throwable _ nil)))
        (keyword? x) (kw->snake x)
        (symbol? x) (kw->snake (keyword x))
        :else x))

(defn- session-envelope [op result] (extension/success {:op op :result (deep-stringify result)}))

(defn- foundation-inspect
  "Canonical session-state data surface. Returns a Vis tool envelope;
   sandbox callers receive the unwrapped data map."
  ([env] (foundation-inspect env (:session-id env)))
  ([env session-id] (session-envelope :session-state (foundation-inspect-data env session-id))))


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

;; -- public, doc-bearing aliases -----------------------------------------------
;; The underlying defs (`foundation-inspect`, `foundation-report-html`) are
;; private and named for clarity inside this ns. Re-export them under their
;; sandbox-visible names with `:doc` and `:arglists` baked into the var meta so
;; `vis/symbol` can read both straight off the var.
(def
  ^{:doc
    "await session_state(session_id=None)  # current session by default; pass id for another
String-keyed fields: `session`, `current_turn`, `failures`, `diagnosis`, `session_forks`, `turn_retries`, \"usage\", `transcript`. \"usage\" is compact token/cost/outcome/error/routing; tool rows overlap, so never sum them. Filter `transcript/turns/iterations/blocks` (`code`/`result`) in python_execution; don't dump. Use live `session` for current state. This is the recovery path for raw folded current-session content; it does not undo fold intents or restore them. Find ids via `sessions()`."
    :arglists '([] [session-id])}
  session-state
  foundation-inspect)



(def
  ^{:doc
    "await sessions(channel=None)  # newest-first conversation index
String-keyed rows: `{id,channel,title,turn_count,created_at,modified_at}`; optional `channel` filter. Content: `session_state(id)`. Filter in python_execution; don't stringify or slice blindly."
    :arglists '([] [channel])}
  sessions
  foundation-sessions)

(def session-state-symbol (vis/symbol #'session-state {:inject-env? true :tag :observation}))


(def sessions-symbol (vis/symbol #'sessions {:inject-env? true :tag :observation}))


(def all-symbols [session-state-symbol sessions-symbol])

;; ---------------------------------------------------------------------------
;; The introspection extension. Self-inspection (`session_state` / `sessions`,
;; plus the gateway event journals) is NOT core agent
;; policy: most projects never want the agent reading its own transcripts, so
;; the whole surface — symbols AND prompt guidance — hangs off the
;; `introspection` toggle, which is OFF by default. Turn it on in `vis.yml`
;; (`toggles: { introspection: true }`) or from the settings dialog.
;; ---------------------------------------------------------------------------

(vis/register-toggle!
  {:id "introspection"
   :label "Session introspection"
   :description (str "Let the agent inspect its OWN history through `session_state` / `sessions`, "
                     "plus the gateway event journals under `~/.vis/gateway/events`. OFF by "
                     "default — enable it for debugging Vis itself, not for ordinary project work.")
   :default false
   :owner :vis
   :persist? true
   :group :sandbox})

(def ^:private INTROSPECTION_PROMPT
  (str
    "## Session introspection\n"
    "- Raw wire history: `~/.vis/gateway/events/<id>.ndjson`; never grep `.`.\n"
    "- Call `await session_state()` once. `usage` summarizes per-turn/iteration/tool/provider routing; tool rows overlap, never sum them.\n"
    "- Current transcript and folded-content recovery: `transcript/turns/iterations/blocks` (`code`/`result`).\n"
    "- Other conversation: `await sessions()`, then `await session_state(id)`.\n"
    "- Filter in `python_execution`; never dump whole structures.\n"))

(defn- introspection-prompt [_env] INTROSPECTION_PROMPT)

(def vis-extension
  (vis/extension
    {:ext/name "foundation-introspection"
     :ext/description
     "`session_state`: transcript + compact usage/tool/routing ledger; `sessions`: newest-first metadata. Raw journal: `~/.vis/gateway/events/<id>.ndjson`. Requires the default-off `introspection` toggle."
     :ext/version "0.1.0"
     :ext/author "Blockether"
     :ext/owner "vis"
     :ext/license "Apache-2.0"
     :ext/kind "foundation"
     :ext/activation-fn (fn [_env]
                          (vis/toggle-enabled? "introspection"))
     :ext/engine {:ext.engine/builtin? true :ext.engine/symbols all-symbols}
     :ext/prompt-fn introspection-prompt}))

(vis/register-extension! vis-extension)
