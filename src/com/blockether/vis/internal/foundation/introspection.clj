(ns com.blockether.vis.internal.foundation.introspection
  "Read a session view, a single session descriptor, or a page of sessions.
   These reads are available to the sandbox only when introspection is on."
  (:require [charred.api :as json]
            [clojure.java.io :as io]
            [com.blockether.vis.internal.paths :as paths]
            [com.blockether.vis.contract.gateway :as gateway-contract]
            [com.blockether.vis.internal.activity.presenter :as presenter]
            [clojure.string :as str]
            [com.blockether.vis.internal.foundation.transcript :as transcript]
            [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.internal.config.runtime-settings :as rt]
            [com.blockether.vis.internal.gateway.diagnostics :as gateway-diagnostics]
            [com.blockether.vis.internal.persistance.core :as persistance]
            [com.blockether.vis.internal.channel.header :as header]
            [com.blockether.vis.internal.config.toggles :as toggles]
            [com.blockether.vis.internal.content :as content]
            [com.blockether.vis.internal.provider.error :as perr]
            [com.blockether.vis.internal.session.goals :as goals]
            [com.blockether.vis.internal.session.model :as session-model]
            [com.blockether.vis.internal.util :as util]))

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
  (try (persistance/db-list-session-turn-iterations db-info session-turn-id)
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
  (let [iter-raw
        (some-> (:turn-state-atom env)
                deref
                :iteration)

        current-position
        (cond (map? iter-raw) (or (:position iter-raw) 1)
              (number? iter-raw) iter-raw
              :else 1)]

    {:current (long current-position)}))

(defn- iteration-forms
  "The executed forms of one iteration row. Stdout and errors live only inside
   the persisted `:forms` envelope; the row has no top-level output columns."
  [iteration]
  (let [forms (:forms iteration)]
    (if (sequential? forms) (vec forms) [])))

(defn- format-provider-model
  "Render `\"provider/model\"` when both are known, otherwise just the
   model (or just the provider) so callers always get a non-empty
   string when at least one component exists. Returns nil when both
   are nil/blank - callers `cond->` on the result."
  [provider model]
  (let [provider-str
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
        (try (- (util/now-ms)
                (long (cond (inst? created) (inst-ms created)
                            (integer? created) (long created)
                            :else 0)))
             (catch Throwable _ nil)))))

(defn- parse-json-map
  "Best-effort JSON object parser for persisted provider errors. The
   persistence layer stores `iteration.llm_error` as JSON text; meta
   keeps parsing local so callers get failures as Clojure data.

   `:key-fn keyword` stays: the parsed map is INTERNAL to
   `iteration-failure` (its keys are read locally via `(:type data)`
   etc. and never emitted), and its string VALUES are what flow into
   the boundary-crossing failure map. Keeping idiomatic keyword access
   here does not put a keyword on the wire."
  [text]
  (when (util/non-blank-string? text)
    (try (let [parsed (json/read-json text :key-fn keyword)]
           (when (map? parsed) parsed))
         (catch Throwable _ nil))))

(defn- preview
  ([text] (preview text 220))
  ([text ^long limit]
   (when (some? text)
     (let [string-value (str text)]
       (if (> (count string-value) limit)
         (str (util/truncate string-value limit) "...")
         string-value)))))

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

(defn- retired-python-error?
  [error]
  (contains? #{":com.blockether.vis.internal.python.env/context-retired"
               "com.blockether.vis.internal.python.env/context-retired"}
             (str (or (:type error) (get-in error [:data :type])))))

(defn- persisted-provider-error
  "A provider error read back from `iteration.llm_error` JSON, with the few
   keyword values `provider.error` dispatches on restored from their strings."
  [error-map]
  (let [->kw #(if (string? %) (keyword (str/replace % #"^:" "")) %)]
    (cond-> error-map
      (get-in error-map [:data :type])
      (update-in [:data :type] ->kw)

      (get-in error-map [:data :stream-recovery :declined])
      (update-in [:data :stream-recovery :declined] ->kw)

      (seq (get-in error-map [:data :routed/trace]))
      (update-in [:data :routed/trace] (partial mapv #(update % :event/type ->kw))))))

(defn- llm-label
  [provider model]
  (when (or provider model)
    (str (some-> provider
                 name)
         (when model (str "/" model)))))

(defn- iteration-failure
  [iteration]
  (when-let [error (:error iteration)]
    (let [error-map (if (map? error) error (or (parse-json-map error) {:message (str error)}))
          runtime? (retired-python-error? error-map)
          data (:data error-map)
          ;; `:type` / `:reason` are JSON string values carried straight
          ;; into the failure map, which crosses the strings-only
          ;; boundary - keep them as strings, never mint keywords.
          type (:type data)
          reason (:reason data)
          raw-data (:raw-data data)
          provider-message (perr/provider-body-message (some-> (:body data)
                                                               str))
          actual (llm-label (:llm-actual-provider iteration) (:llm-actual-model iteration))
          selected (llm-label (:llm-selected-provider iteration) (:llm-selected-model iteration))]

      (cond-> {:source (if runtime? :runtime :provider)
               :iteration-id (:id iteration)
               :iteration (:position iteration)
               :status (:status iteration)
               :message (or (:message error-map) (str error))
               :classification (cond runtime? :python-environment-retired
                                     (schema-rejected-type? type) :provider-schema-rejected
                                     :else :provider-error)}
        type
        (assoc :type type)

        reason
        (assoc :reason reason)

        ;; #401-#403: the cause that was only in the SQLite `log` table.
        (some? (:status data))
        (assoc :http-status (:status data))

        (:request-id data)
        (assoc :request-id (:request-id data))

        (:log-id data)
        (assoc :log-id (str (:log-id data)))

        (seq provider-message)
        (assoc :provider-message provider-message)

        actual
        (assoc :model actual)

        (and selected (not= selected actual))
        (assoc :selected-model selected)

        (some? (:llm-fallback? iteration))
        (assoc :fallback? (:llm-fallback? iteration))

        (not runtime?)
        (assoc :explanation (perr/provider-error-explanation (persisted-provider-error error-map)))

        (:received-type data)
        (assoc :received-type (:received-type data))

        (some? raw-data)
        (assoc :raw-preview (preview raw-data))

        (:raw-data-preview data)
        (assoc :raw-data-preview (:raw-data-preview data))))))

(defn- cancellation-failure?
  "True when an error is fallout from interrupting a cancelled turn, rather than
   a defect in the form that happened to be on the stack."
  [lower-message]
  (or (str/includes? lower-message "interruptedexception")
      (str/includes? lower-message "cancellationexception")
      (str/includes? lower-message "closedbyinterruptexception")
      (str/includes? lower-message "interrupted while")
      (str/includes? lower-message "sleep interrupted")))

(def ^:private patch-refusal-causes
  "Cause phrases `patch` prints under its refusal head, in match order, paired with the
   classification whose advice actually fixes that cause. The wording is the editor's own
   (`foundation.editing.core`): an overlap, changed lines, an unusable version, then the
   edit-shape checks."
  [[:patch-overlapping-edits ["overlap"]]
   [:patch-lines-changed ["changed since that read" "changed too much since"]]
   [:patch-lines-not-shown ["result showed lines"]]
   [:patch-version-invalid ["is not known here" "version must be the `@` value"]]
   [:patch-invalid-edit
    ["must be a line number" "out of range" "is after to line" "must be a map"
     "non-empty list of maps" "missing `from`" "missing `new`" "allowed: from, to, new"]]])

(defn- patch-refusal-classification
  "The classification a `patch` refusal earns from the cause line it printed, or nil when
   the message is not a patch refusal at all. `patch` refuses ATOMICALLY under ONE shared
   `patch refused` head for causes whose recoveries are opposites, so the head only proves
   that nothing was written — the cause line under it is what picks the fix. That head also
   identifies the refusal on its own, whatever token the failing block happened to open with."
  [lower-message]
  (when (str/includes? lower-message "patch refused")
    (some (fn [[classification phrases]]
            (when (some (fn [phrase]
                          (str/includes? lower-message phrase))
                        phrases)
              classification))
          patch-refusal-causes)))

(defn- classify-expression-failure
  [error]
  (let [lower-message (str/lower-case (or (error-text error) ""))]
    (cond (retired-python-error? error) :python-environment-retired
          (cancellation-failure? lower-message) :turn-cancelled
          ;; Changed lines are their own loop: the refusal names the current version and
          ;; a window to read. But an overlap and an unusable edit shape refuse under the
          ;; SAME head, and re-read advice sends those back with the very text that was
          ;; refused, so the cause line decides — never the head alone.
          :else (or (patch-refusal-classification lower-message) :code-execution-error))))

(defn- advice-for-classification
  [classification]
  (case classification
    :python-environment-retired
    "The local Python environment was retired, not the model provider. Check operations already started before continuing in a new turn with a fresh Python environment; do not blindly replay side effects."

    :provider-schema-rejected
    "Provider returned prose/string instead of the iteration map. Skip the SQLite trip - the raw preview is already here. Continue after the built-in schema retry, or switch model when this repeats."

    :patch-overlapping-edits
    "Two edits in the batch cover the same line, so the whole batch was refused. Merge them into ONE edit spanning both ranges instead of sending the same pair again."

    :patch-invalid-edit
    "The edit shape is unusable as written: each edit is {\"old\": exact_text, \"new\": text} or {\"from\": 12, \"to\": 14, \"new\": text}, and a line edit uses line numbers of the version that the cat/grep header showed, inside the file and `from` before `to`. The cause line names the failing edit — fix that one and re-send."

    :patch-lines-changed
    "The lines of this edit changed after the read that showed them, so the whole batch was refused. Read only the `cat` window the refusal names, then send the edit again with the current version it gives. Edits elsewhere in the file move on their own."

    :patch-lines-not-shown
    "No cat, grep or patch result of this version showed these lines, so their numbers can come from another read. If the refusal shows the lines, check them and send the edit again with the version it gives. Else send the edit with the version of the read that showed the lines, or read the `cat` window that the refusal names."

    :patch-version-invalid
    "The `version` argument is not a version this file showed. Pass the `@` value from the header of the last cat, grep or patch result for this file, or read the lines again to get the current version."

    :turn-cancelled
    "Not a code defect: the turn was cancelled and the interrupt surfaced on the frame that was running. Re-run the interrupted step if you still need it."

    "Read :message, :code, and :iteration; fix the smallest failing form before issuing new searches."))

(defn- expression-failures-for-iteration
  "Curated failure entries for one iteration - one per errored FORM.
   The error is a property of the form, never of the iteration row."
  [iteration]
  (into []
        (keep (fn [form]
                (when-let [error (:error form)]
                  (let [code (or (:src form) (:code iteration))
                        classification (classify-expression-failure error)]

                    {:source (if (= :python-environment-retired classification) :runtime :code)
                     :iteration-id (:id iteration)
                     :iteration (:position iteration)
                     :tool (:vis/tool-name form)
                     :classification classification
                     :code code
                     :message (error-text error)
                     :advice (advice-for-classification classification)}))))
        (iteration-forms iteration)))

(defn- failures-from-iterations
  [iterations]
  (vec (mapcat (fn [iteration]
                 (let [failure (when-let [failure (iteration-failure iteration)]
                                 [(assoc failure
                                    :advice (advice-for-classification (:classification
                                                                         failure)))])]
                   (concat failure (expression-failures-for-iteration iteration))))
               iterations)))

(defn- latest-turn
  [db-info session-id]
  (when (and db-info session-id)
    (last (try (persistance/db-list-session-turns db-info session-id) (catch Throwable _ [])))))

(defn- turn-snapshot
  "Read the latest turn and its classified failures for diagnosis."
  [{:keys [db-info session-id]}]
  (when-let [turn (latest-turn db-info session-id)]
    (assoc (transcript/request-fields turn (:user-request turn))
      :id (:id turn)
      :status (:status turn)
      :failures (failures-from-iterations (iteration-rows db-info (:id turn))))))

(defn- turn-outcome
  "How `turn` ended. A turn that a fork settled is `:forked`, not the `:cancelled` that
   its row stores: nobody cancelled it."
  [turn]
  (if (some content/fork-notice? (:content turn))
    :forked
    (or (:prior-outcome turn) (:status turn))))

(defn- fork-origin
  "The `origin` of a fork with the current `:outcome` of its source turn. That turn can
   still run in the source or end after the fork. Without the source turn, no `:outcome`."
  [db-info {:keys [session-id turn-id] :as origin}]
  (let [turn (try (persistance/db-read-session-turn db-info session-id turn-id)
                  (catch Throwable _ nil))]
    (cond-> origin
      turn
      (assoc :outcome (turn-outcome turn)))))

(defonce ^:private default-route (atom nil))

(defn install-default-route!
  "Install the function of a session id that gives the default `{:provider :model}` of
   that session: the pair that its turns use when the session has no model pick. It
   reads the project configuration through `gateway.state`, a higher layer, so the
   process wiring installs it. Without it, [[session-route]] falls back to the root pair."
  [f]
  (reset! default-route f)
  nil)

(defn- session-route
  "The `{:provider :model}` strings that the turns of `session` run on now: its model
   pick, also before the debounced write saves it, else the default pair of the session.
   The root pair of the session state is only the router default at the creation of the
   session, and a fork copies it. Thus it is the last answer."
  [db-info session]
  (let [sid
        (:id session)

        route
        (or (session-model/model-of db-info sid)
            (when-let [default @default-route]
              (default sid)))]

    (if (:model route)
      {:provider (some-> (:provider route)
                         name)
       :model (:model route)}
      {:provider (some-> (:provider session)
                         name)
       :model (:model session)})))

(defn- present-request-fields
  [turn user-request]
  (let [fields (transcript/request-fields turn user-request)]
    (if (nil? user-request) (dissoc fields :user-request) fields)))

(defn- session-summary
  [db-info session turn-count]
  (let [{:keys [provider model]} (session-route db-info session)]
    (cond-> {:id (:id session)
             :channel (:channel session)
             :title (preview (:title session) 160)
             :goal-status (goals/visible-status (:goal session))
             :created-at (:created-at session)
             :turn-count turn-count
             :model model}
      provider (assoc :provider provider)
      (:forked-from session) (assoc :forked-from (fork-origin db-info (:forked-from session))))))

(defn- session-turn-rows
  [db-info session-id]
  (try (persistance/db-list-session-turns db-info session-id) (catch Throwable _ [])))

(defn- session-index-row
  "ONE index row for `session`: identity, turn count and last activity - never
   transcript text. Shared by the index, its ranked `search` projection and the
   single-session descriptor, so a row means the same thing whichever verb
   produced it."
  ([db-info session] (session-index-row db-info session (session-turn-rows db-info (:id session))))
  ([_db-info session turns]
   (let [modified-at (or (->> turns
                              (keep :created-at)
                              (sort-by #(if (inst? %) (inst-ms %) 0))
                              last)
                         (:created-at session))]
     (cond-> {:id (:id session)
              :channel (:channel session)
              :title (:title session)
              :goal-status (goals/visible-status (:goal session))
              :created-at (:created-at session)
              :modified-at modified-at
              :turn-count (count turns)}
       (:external-id session)
       (assoc :external-id (:external-id session))

       ;; The human's STAR, exactly as the gateway keeps it: a rank, absent when
       ;; the session carries no star, so a row never claims one it does not have.
       (:favorite-rank session)
       (assoc :favorite-rank (:favorite-rank session))))))

(defn- recency-key
  "Newest-first sort key over `:created-at` (inst or epoch millis)."
  [row]
  (if-let [c (:created-at row)]
    (cond (inst? c) (- (long (inst-ms c)))
          (integer? c) (- (long c))
          :else 0)
    0))

(defn- search-tags
  "The where-it-hit half of a search row, in the same vocabulary the gateway
   wire uses (`is_<foo>`): the band it earned plus which side matched, with the
   server's snippet windows when it carries them."
  [match]
  (cond-> {:rank (:rank match)
           :is-in-title (boolean (:in-title? match))
           :is-in-request (boolean (:in-request? match))
           :is-in-reply (boolean (:in-reply? match))
           :is-in-thinking (boolean (:in-thinking? match))}
    (:request-snippet match)
    (assoc :request-snippet (:request-snippet match))

    (:reply-snippet match)
    (assoc :reply-snippet (:reply-snippet match))))

(defn- search-session-rows
  "The SAME session search the TUI and the companion app run
   (`db-search-session-matches`): the server answers FRESHEST FIRST — the order
   the session list is already in, so a search reads as a FILTER of that list.
   This projection hydrates each match into an index row and KEEPS that order,
   so the agent paints one relevance order instead of inventing a fourth.
   `:rank` travels as WHERE the query hit, never as the order."
  [env query]
  (if-let [db (:db-info env)]
    (try (into []
               (keep (fn [match]
                       (when-let [session (persistance/db-get-session db (:id match))]
                         (merge (session-index-row db session) (search-tags match)))))
               (persistance/db-search-session-matches db :all query))
         (catch Throwable _ []))
    []))

(defn- foundation-sessions-data
  "List every session the DB knows about, newest-first. With a non-blank
   `search` the SERVER's ranked answer replaces that order (see
   `search-session-rows`); a blank search is the plain index. Returns `[]`
   (never nil) when the env is missing a `:db-info` handle so callers can
   chain seq operations safely."
  ([env]
   (if-let [db (:db-info env)]
     (try (->> (persistance/db-list-sessions db :all)
               (map #(session-index-row db %))
               (sort-by recency-key)
               vec)
          (catch Throwable _ []))
     []))
  ([env search]
   (if-let [q (some-> search
                      str
                      str/trim
                      not-empty)]
     (search-session-rows env q)
     (foundation-sessions-data env))))

(defn- foundation-session-descriptor
  "ONE session's descriptor: its index row plus provider/model, whether it is
   THIS session, and the last turn's id, outcome and input.
   The cheap middle read between the whole index and the whole transcript, so
   \"what is that session\" never costs a transcript. `nil` when nothing
   matches `target` (an id or an unambiguous prefix)."
  ([env] (foundation-session-descriptor env (current-session-id env)))
  ([env target]
   (let [db
         (:db-info env)

         wanted
         (or target (current-session-id env))

         session-id
         (or (try (persistance/db-resolve-session-id db wanted) (catch Throwable _ nil)) wanted)]

     (when (and db session-id)
       (try (when-let [session (persistance/db-get-session db session-id)]
              (let [turns (session-turn-rows db (:id session))
                    last-turn (last turns)
                    {:keys [provider model]} (session-route db session)]

                (cond-> (assoc (session-index-row db session turns)
                          :is-current (boolean (same-uuid? (:id session) (current-session-id env))))
                  provider
                  (assoc :provider provider)

                  model
                  (assoc :model model)

                  (format-provider-model provider model)
                  (assoc :provider-model (format-provider-model provider model))

                  last-turn
                  (assoc :last-turn
                    (assoc (present-request-fields last-turn (preview (:user-request last-turn) 200))
                      :id (:id last-turn)
                      :outcome (turn-outcome last-turn))))))
            (catch Throwable _ nil))))))

(defn- foundation-session-forks
  "List every `session_state` row for the session soul behind
   `session-id`, oldest version first. Each row maps to
   `{:state-id :version :parent-state-id :title :provider :model
     :created-at :turn-count}`. The trunk is `:version 0` with
   `:parent-state-id nil`; any later row with non-nil `:parent-state-id`
   is a fork off the referenced state. Returns `[]` (never nil) when
   the session has no rows OR the env is missing handles - lets
   callers chain `(group-by :parent-state-id ...)` without nil-guards.

   No-arg form uses the current-session-id from the env."
  ([env] (foundation-session-forks env (current-session-id env)))
  ([env session-id]
   (if (and (:db-info env) session-id)
     (try (vec (persistance/db-list-session-states (:db-info env) session-id))
          (catch Throwable _ []))
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
    (try (vec (persistance/db-list-session-turn-states (:db-info env) session-turn-id))
         (catch Throwable _ []))
    []))

(defn- failures-for-turns
  [env turns]
  (vec (mapcat (fn [turn]
                 (let [iterations (iteration-rows (:db-info env) (:id turn))]
                   (mapv #(into (assoc % :turn-id (:id turn))
                                (transcript/request-fields turn (:user-request turn)))
                         (failures-from-iterations iterations))))
               turns)))

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
  (let [message
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
          "Treat schema rejection as provider noise, not a reason to inspect SQLite. Use raw_preview from read_session(view=\"diagnosis\")[\"diagnosis\"][\"failures\"] and retry/switch model only if it repeats.")

        (contains? classes :patch-lines-changed)
        (conj
          "Read only the window the patch refusal names and use the current version it gives; put every edit for one file in ONE patch(path, edits) call.")))))

(def ^:private STALE_RUNNING_TURN_MS
  "A running turn with no new iteration for this long is a suspected hang."
  (* 5 60 1000))

(defonce ^:private block-clock (atom nil))

(defn install-block-clock!
  "Install the function of a session id and a turn id that gives the epoch
   milliseconds when the turn's running Python block started, or nil. It reads
   the gateway journal in `gateway.journal`, a higher layer, so the process wiring
   installs it. Without it, [[hung-turns]] counts from the newest iteration."
  [f]
  (reset! block-clock f)
  nil)

(def ^:private HUNG_TURN_ACTION
  "A turn hung in the gateway. It is not a failure of your code. Read hung_turns[].reports[].path for the thread stacks and phase for the step that waited. Resubmit the request if you still need it.")

(defn- epoch-ms
  [value]
  (cond (inst? value) (inst-ms value)
        (integer? value) (long value)
        :else nil))

(defn- last-progress-ms
  "Wall-clock time of the newest iteration of `turn`, else its start."
  [db-info turn]
  (let [iterations (try (iteration-rows db-info (:id turn)) (catch Throwable _ []))]
    (or (some->> (keep (comp epoch-ms :created-at) iterations)
                 seq
                 (apply max))
        (epoch-ms (:created-at turn)))))

(defn- turn-error-message
  [turn]
  (let [error (:error turn)]
    (when (map? error)
      (some-> (:message error)
              (preview 220)))))

(defn- hung-turns
  "Turns that stopped making progress in the gateway. A turn qualifies when a
   watchdog saved a hang report for it, or when it still runs without a new
   iteration for [[STALE_RUNNING_TURN_MS]]. A Python block that still runs is
   work, such as a 30-minute shell wait: its turn qualifies only after the eval
   watchdog ceiling plus that time. The caller's own live turn is skipped."
  [env session-id turns position]
  (let [reports
        (group-by :turn-id (gateway-diagnostics/session-reports session-id))

        own-turn
        (when (same-uuid? session-id (current-session-id env)) (:id (last turns)))

        now
        (util/now-ms)]

    (into []
          (keep
            (fn [turn]
              (let [turn-reports
                    (mapv #(dissoc % :turn-id) (get reports (str (:id turn))))

                    watched?
                    (and (= :running (:status turn)) (not= own-turn (:id turn)))

                    block-since
                    (when-let [clock (when watched? @block-clock)]
                      (clock session-id (:id turn)))

                    idle-ms
                    (when watched?
                      (some->> (or block-since (last-progress-ms (:db-info env) turn))
                               (- now)))

                    stale-ms
                    (cond-> (long STALE_RUNNING_TURN_MS)
                      block-since
                      (+ (long rt/MAX_EVAL_TIMEOUT_MS)))

                    stale?
                    (boolean (and idle-ms (>= (long idle-ms) stale-ms)))]

                (when (or (seq turn-reports) stale?)
                  (cond-> (assoc (present-request-fields turn (preview (:user-request turn) 120))
                            :turn-id (:id turn)
                            :status (:status turn)
                            :signal (if (seq turn-reports) :hang-report :no-progress)
                            :iteration-count (:iteration-count turn))

                    stale?
                    (assoc :idle-ms idle-ms)

                    (seq turn-reports)
                    (assoc :reports turn-reports)

                    (turn-error-message turn)
                    (assoc :error (turn-error-message turn)))))))
          (if position (filter #(= position (:position %)) turns) turns))))

(defn- foundation-diagnose
  "Compact current-turn diagnosis built from failure data. Returns a
   map with counts, repetition-loop detection, and next actions so the
   agent can stop burning iterations on DB spelunking. Pass a
   session id to diagnose all turns in that session.

   `:repetition-loop?` is `true` when any error signature repeats at
   least `REPETITION_THRESHOLD` times in the failure list - the
   single-glance flag for the 'agent retried the same broken call N
   times' pathology. `:repetition-clusters` carries the supporting
    data (signature, count, sample failure).

    The session form also returns `:hung-turns`: turns that a gateway watchdog
    saved a hang report for, and running turns without a new iteration for
    five minutes. Their failures never reach the iteration rows."
  ([env]
   (let [turn
         (turn-snapshot env)

         failures
         (vec (:failures turn))

         clusters
         (repetition-clusters failures)]

     (assoc (transcript/request-fields turn (:user-request turn))
       :turn-id (:id turn)
       :status (:status turn)
       :failure-count (count failures)
       :by-classification (classification-counts failures)
       :repetition-loop? (boolean (seq clusters))
       :repetition-clusters clusters
       :failures failures
       :next-actions (next-actions failures clusters))))
  ([env session-id]
   (let [turns (when (and (:db-info env) session-id)
                 (session-turn-rows (:db-info env) session-id))]
     (foundation-diagnose env session-id turns nil)))
  ([env session-id turns position]
   (let [selected (if position (filterv #(= position (:position %)) turns) turns)
         failures (failures-for-turns env selected)
         clusters (repetition-clusters failures)
         hung (if (and (:db-info env) session-id)
                (hung-turns env session-id turns position)
                [])]

     {:session-id session-id
      :failure-count (count failures)
      :by-classification (classification-counts failures)
      :repetition-loop? (boolean (seq clusters))
      :repetition-clusters clusters
      :failures failures
      :hung-turns hung
      :next-actions (cond-> (next-actions failures clusters)
                      (seq hung)
                      (conj HUNG_TURN_ACTION))})))

(declare safe-call session-envelope)

(defn- usage-tokens
  [iteration]
  (let [input
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

(defn- python-execution-calls
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
  (let [errors
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
  (let [provider
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
  (let [actual
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
               (let [from
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
  (let [events
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
  (let [switches (->> (safe-call
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
  (let [calls
        (python-execution-calls turn iteration)

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
  (let [summary (or summary
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
  "Group model iterations by the py3 operations they invoked. Usage
   intentionally overlaps across operations, so grouped rows must not be summed."
  [iterations]
  (let [by-tool (reduce (fn [by-tool usage]
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
  ([env session-id]
   (let [target-id (or session-id (:session-id env))]
     (foundation-usage-data env
                            target-id
                            (safe-call #(transcript/transcript (:db-info env) target-id) nil))))
  ([env target-id data] (foundation-usage-data env target-id data nil))
  ([env target-id data position]
   (let [switches (when-not position (manual-model-switches (:db-info env) target-id))
         empty-routing (merge (usage-routing []) switches)]
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
       (let [turns (mapv (fn [turn]
                           (let [iterations (mapv (partial usage-iteration turn) (:iterations turn))
                                 totals (usage-total iterations)]

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
             iterations (vec (mapcat :iterations turns))
             {:keys [tool-errors tool-errors-truncated?]} (usage-tool-errors iterations)
             routing (merge (usage-routing iterations) switches)]

         {:schema-version 1
          :scope :session-usage
          :session-id (get-in data [:session :id])
          :session (select-keys (:session data) [:id :title :channel :provider :model])
          :totals (assoc (usage-total iterations) :turns (count turns))
          :turns (mapv #(update % :iterations (partial mapv public-usage-iteration)) turns)
          :tools (usage-tools iterations)
          :tool-errors tool-errors
          :tool-errors-truncated? tool-errors-truncated?
          :routing routing})))))

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

(defn- bounded-number
  [value default minimum maximum]
  (cond (not (integer? value)) default
        (neg? (compare value minimum)) (long minimum)
        (pos? (compare value maximum)) (long maximum)
        :else (long value)))

(defn- page-limit [opts default maximum] (bounded-number (get opts "limit") default 1 maximum))

(defn- page-offset [opts] (bounded-number (get opts "offset") 0 0 Integer/MAX_VALUE))

(defn- text-slice
  [^String text ^long offset ^long limit]
  (let [size (long (.codePointCount text 0 (.length text)))
        start (min offset size)
        end (min size (+ start limit))]
    {:text (subs text (.offsetByCodePoints text 0 (int start))
                 (.offsetByCodePoints text 0 (int end)))
     :offset start :chars size :next-offset (when (< end size) end)}))

(defn- stdout-slice
  [block offset limit]
  (if-let [stdout (:stdout block)]
    (let [{:keys [text offset chars next-offset]} (text-slice stdout offset limit)]
      (assoc block :stdout text :stdout-next-offset next-offset
                   :stdout-offset offset :stdout-chars chars))
    block))

(defn- model-iteration
  "Keep the executed code once, with its timing and call data."
  [opts iteration]
  (let [forms (vec (:forms iteration))
        exact-block? (and (= "blocks" (get opts "view"))
                          (every? #(get opts %) ["turn" "iteration" "block"]))
        offset (page-offset opts)
        limit (page-limit opts 4096 (if exact-block? Integer/MAX_VALUE 65536))
        blocks (into []
                     (comp (map-indexed
                             (fn [idx block]
                               (merge (dissoc (get forms idx) :src :op :activity)
                                      (dissoc block :op :activity))))
                           (filter #(or (nil? (get opts "block"))
                                        (= (get opts "block") (inc (long (:position %))))))
                           (map #(stdout-slice % offset limit)))
                     (:blocks iteration))]
    (cond-> (dissoc iteration :forms :blocks :request-health)
      (seq (:blocks iteration)) (dissoc :code)
      (seq (:llm-actual iteration)) (dissoc :provider :model)
      (contains? iteration :blocks) (assoc :blocks blocks))))

(defn- at-position?
  [position row]
  (or (nil? position) (= position (:position row))))

(defn- model-transcript
  [opts {:keys [turns]}]
  {:turns (mapv (fn [turn]
                  (cond-> (update turn :iterations
                                   #(into [] (comp (filter (partial at-position? (get opts "iteration")))
                                                   (map (partial model-iteration opts))) %))
                    (get opts "iteration") (dissoc :tokens :cost-usd :failure-count
                                                    :provider :model)))
                turns)})

(defn- saved-answer [turn]
  (let [blocks (map (fn [block]
                      (if (= "text" (get block "type"))
                        {"type" "prose" "markdown" (get block "text")}
                        block))
                    (:content turn))
        answer (content/text-projection blocks)]
    (when-not (str/blank? answer) answer)))

(defn- index-turn
  [metadata turn]
  (let [answer (saved-answer turn)
        council? (or (= :council (:request-kind turn)) (:council turn))]
    (cond-> {:id (:id turn)
             :position (:position turn)
             :status (:status turn)
             :outcome (turn-outcome turn)
             :iteration-count (count (get metadata (str (:id turn))))}
      council? (assoc :request-kind :council)
      (not council?) (assoc :request-preview (preview (:user-request turn) 160))
      ;; A Council turn shows its attributed input, never the Council instruction.
      (and council? (get-in turn [:council :content]))
      (assoc :request-preview (preview (get-in turn [:council :content]) 160))
      answer (assoc :answer-preview (preview answer 160)))))

(defn- turn-detail
  [db opts turn]
  (let [tid (:id turn)
        rows (get (persistance/db-list-session-turns-iterations db [tid] {:request-health? false})
                  (str tid))
        answer (saved-answer turn)
        slice (when answer (text-slice answer (page-offset opts) (page-limit opts 4096 65536)))]
    (merge (select-keys turn [:id :position :status])
           (present-request-fields turn (:user-request turn))
           {:outcome (turn-outcome turn)
            :answer (:text slice)
            :answer-chars (or (:chars slice) 0)
            :answer-next-offset (:next-offset slice)
            :iterations (mapv (fn [iteration]
                                (let [error (or (error-text (:error iteration))
                                                (some (comp error-text :error)
                                                      (:forms iteration)))
                                      actual (llm-label (:llm-actual-provider iteration)
                                                        (:llm-actual-model iteration))
                                      selected (llm-label (:llm-selected-provider iteration)
                                                          (:llm-selected-model iteration))]
                                  (cond-> (assoc (select-keys iteration [:position :status])
                                            :block-count (count (:forms iteration)))
                                    actual (assoc :model actual)
                                    (and selected (not= selected actual)) (assoc :selected-model selected)
                                    error (assoc :error (preview error 160)))))
                              rows)})))

(defn- session-read-index
  [env target-id opts turns]
  (if-let [position (get opts "turn")]
    (if-let [turn (first (filter (partial at-position? position) turns))]
      {:turn (turn-detail (:db-info env) opts turn)}
      {:error (str "Turn " position " does not exist.")})
    (let [offset (page-offset opts)
          limit (page-limit opts 6 50)
          page (into [] (comp (drop offset) (take limit)) (rseq (vec turns)))
          metadata (safe-call #(persistance/db-list-session-turns-iterations-meta
                                 (:db-info env) (mapv :id page)) {})
          latest (peek (vec turns))]
      {:turns (mapv (partial index-turn metadata) page)
       :current-turn (when latest
                       (cond-> (assoc (select-keys latest [:id :position :status])
                                 :cost (turn-cost-summary latest))
                         (same-uuid? target-id (current-session-id env))
                         (assoc :iteration (iteration-pointer env))))
       :next-offset (when (< (+ offset limit) (count turns)) (+ offset limit))})))

(defn- session-children [db target-id]
  (when target-id
    (mapv (fn [child]
            (cond-> (select-keys child [:id :session_id :role :title :status :outcome])
              (:result child) (assoc :result-headline
                                     (util/truncate (first (str/split-lines (:result child))) 120))))
          (safe-call #(persistance/db-agent-list db target-id) []))))

(defn- lean-current-turn
  [env target-id turns]
  (when-let [turn (peek (vec turns))]
    (cond-> (merge (select-keys turn [:id :position :status])
                   (present-request-fields turn (:user-request turn))
                   {:cost (turn-cost-summary turn)})
      (same-uuid? target-id (current-session-id env)) (assoc :iteration (iteration-pointer env))
      (elapsed-ms turn) (assoc :elapsed-ms (elapsed-ms turn)))))

(def ^:private event-payload-fields
  (merge (zipmap ["task.started" "task.updated" "task.ended"]
                 (repeat ["id" "name" "kind" "state" "cancel_reason" "detached" "started_at" "ended_at"]))
         (zipmap gateway-contract/turn-terminal-event-types
                 (repeat ["status" "outcome" "reason" "cancel_reason" "request_kind"]))
         {"turn.started" ["request_kind" "started_at" "resumed"]
          "turn.progress" ["progress" "phase" "reason" "model" "silent_ms" "connection"]
          "provider.retry" ["attempt" "max_retries" "delay_ms"]
          "block.output" ["form_index" "duration_ms" "silent" "error" "stdout"]
          "voice.input_overflow" ["operation" "active_operation" "active_duration_ms"
                                  "queue_depth" "buffered_ms" "audio_frames"]}))

(defn- event-row
  [event]
  (when-let [fields (get event-payload-fields (get event "type"))]
    (let [header (select-keys event ["type" "seq" "ts" "session_id" "turn_id" "scope" "iteration"])]
      (if-let [task (get event "task")]
        (merge header (select-keys task fields)
               (select-keys (get task "origin") ["turn" "iteration"]))
        (cond-> (merge header (select-keys event fields))
          (string? (get event "stdout")) (update "stdout" preview 512)
          (string? (get event "error")) (update "error" preview 512))))))

(defn- scan-session-events
  [lines target-id tid turn offset limit]
  (reduce (fn [{:keys [last-seq matching] :as scan} line]
            (let [event (safe-call #(json/read-json line) nil)
                  n (get event "seq")]
              (if-not (and (integer? n) (same-uuid? target-id (get event "session_id")))
                (assoc scan :is-complete false)
                (let [row (event-row event)
                      event-turn-id (get row "turn_id")
                      row (when (or (nil? tid)
                                    (if event-turn-id
                                      (same-uuid? tid event-turn-id)
                                      (= turn (get row "turn"))))
                            row)]
                  (cond-> (assoc scan
                            :first-seq (or (:first-seq scan) n)
                            :last-seq n
                            :is-complete (and (:is-complete scan) (= n (inc (or last-seq 0)))))
                    row (update :matching inc)
                    (and row (<= offset matching) (< matching (+ offset limit)))
                    (update :events conj row))))))
          {:first-seq nil :last-seq nil :is-complete true :matching 0 :events []}
          lines))

(defn- session-read-events
  [target-id opts turns]
  (let [turn (get opts "turn")
        tid (when turn (:id (some #(when (= turn (:position %)) %) turns)))
        offset (page-offset opts)
        limit (page-limit opts 50 200)
        scan (when (and (uuid? target-id) (or (nil? turn) tid))
               (safe-call #(with-open [reader (io/reader (io/file (paths/vis-home) "gateway" "events"
                                                                  (str target-id ".ndjson"))
                                                          :encoding "UTF-8")]
                             (scan-session-events (line-seq reader) target-id tid turn offset limit)) nil))
        {:keys [first-seq last-seq is-complete matching events]} scan]
    {:events (or events [])
     :coverage {:first-retained-seq first-seq
                :last-retained-seq last-seq
                :is-complete (boolean (and first-seq is-complete))}
     :next-offset (when (< (+ offset limit) (or matching 0)) (+ offset limit))}))

(defn- attributed-entries [batch]
  (or (seq (:entries batch)) (when batch [batch])))

(defn- council-refs
  [db turns opts]
  (let [selected (filterv (partial at-position? (get opts "turn")) turns)
        iterations (safe-call #(persistance/db-list-session-turns-iterations
                                 db (mapv :id selected) {:request-health? false}) {})]
    (into []
          (comp (mapcat (fn [turn]
                          (let [rows (filter (partial at-position? (get opts "iteration"))
                                             (get iterations (str (:id turn))))]
                            (concat (when-not (get opts "iteration")
                                      (map #(vector (:position turn) nil %) (attributed-entries (:council turn))))
                                    (for [row rows
                                          entry (get-in row [:council-input :entries])]
                                      [(:position turn) (:position row) entry])))))
                (filter (fn [[_ _ entry]] (or (:entry-id entry) (:entry_id entry))))
                (drop (page-offset opts))
                (take (inc (page-limit opts 50 100))))
          selected)))

(defn- council-read-entry
  [target-id rows [turn iteration input]]
  (let [id (or (:entry-id input) (:entry_id input))
        saved (get rows id)
        reply (some #(when (same-uuid? target-id (:session_id %)) %) (:replies saved))]
    (cond-> {:entry-id id
             :thread-id (or (:thread_id saved) (:thread_id input) (:thread-id input))
             :author-session-id (or (:author_session_id saved) (:author_session_id input)
                                    (:author-session-id input))
             :turn turn
             :state (or (:state reply) :recorded)
             :content (preview (or (:content input) (:content saved)) 500)}
      iteration (assoc :iteration iteration)
      (:source_ref saved) (assoc :source-ref (:source_ref saved))
      (:reply_entry_id reply) (assoc :result-ref {:entry-id (:reply_entry_id reply)}))))

(defn- session-read-council
  [db target-id opts turns]
  (let [refs (when target-id (council-refs db turns opts))
        limit (page-limit opts 50 100)
        page (take limit refs)
        ids (into #{} (map (fn [[_ _ row]] (or (:entry-id row) (:entry_id row)))) page)
        rows (into {} (map #(vector % (safe-call (fn [] (persistance/db-council-get db %)) nil))) ids)]
    {:council {:entries (mapv (partial council-read-entry target-id rows) page)}
     :next-offset (when (> (count refs) limit) (+ (page-offset opts) limit))}))

(def ^:private view-selectors
  {"index" ["turn" "offset" "limit"]
   "blocks" ["turn" "iteration" "block" "offset" "limit"]
   "diagnosis" ["turn"]
   "usage" ["turn"]
   "council" ["turn" "iteration" "offset" "limit"]
   "events" ["turn" "offset" "limit"]
   "full" ["turn" "offset" "limit"]})

(defn- selector-error [view opts]
  (if-let [allowed (get view-selectors view)]
    (let [provided (into #{} (keep (fn [[k v]] (when (some? v) k)))
                         (dissoc opts "view" "target" "session_id" "id"))
          bad-keys (remove (set allowed) provided)
          bad-position (some (fn [key]
                               (let [value (get opts key)]
                                 (when (and (some? value) (not (and (integer? value) (pos? value))))
                                   key)))
                             ["turn" "iteration" "block" "limit"])
          offset (get opts "offset")]
      (cond (seq bad-keys) (str "View " view " does not accept " (str/join ", " (sort bad-keys))
                                ". Use " (str/join ", " allowed) ".")
            bad-position (str bad-position " must be a positive integer.")
            (and (some? offset) (not (and (integer? offset) (not (neg? offset)))))
            "offset must be a non-negative integer."))
    "Unknown session view. See doc(read_session)."))

(defn- foundation-inspect-data
  "Build only the requested model-facing view. Human exports are independent."
  ([env session-id] (foundation-inspect-data env session-id {}))
  ([env session-id opts]
   (let [db (:db-info env)
         wanted (or session-id (current-session-id env))
         target-id (when (and db wanted) (safe-call #(transcript/resolve-session-ref db wanted) nil))
         session (when target-id (safe-call #(persistance/db-get-session db target-id) nil))
         view (or (get opts "view") "index")
         position (get opts "turn")
         error (selector-error view opts)
         turns (when (and target-id (not error)
                          (or position (contains? #{"index" "blocks" "full" "council"} view)))
                 (session-turn-rows db target-id))
         selected (when position (filterv (partial at-position? position) turns))
         turn-count (if turns (count turns)
                        (or (:turn-count (when session (safe-call #(persistance/db-session-turn-stats db target-id) nil))) 0))
         result {:schema-version 3
                 :scope :session
                 :view view
                 :session-id (or target-id wanted)
                 :session (when session (session-summary db session turn-count))}
         children (when (and (not error) (or (= "council" view) (= "index" view)))
                    (session-children db target-id))]
     (merge result
            (when (or (seq children) (= "council" view)) {:children (or children [])})
            (cond
              error {:error error}
              (and target-id position (empty? selected)) {:error (str "Turn " position " does not exist.")}
              :else
              (let [opts (assoc opts "view" view)
                    history (fn [] (if position
                                     {:session (:session result)
                                      :turns (mapv (partial transcript/build-model-turn db nil) selected)}
                                     (safe-call #(transcript/transcript db target-id) nil)))
                    diagnose (fn [] (safe-call #(if position
                                                   (foundation-diagnose env target-id turns position)
                                                   (foundation-diagnose env target-id)) {}))
                    usage (fn [data] (safe-call #(foundation-usage-data env (or target-id wanted) data position) {}))]
                (case view
                  "index" (session-read-index env target-id opts turns)
                  "blocks" {:transcript (model-transcript opts
                                                          {:turns (mapv (partial transcript/build-model-turn db (get opts "iteration"))
                                                                        (or selected turns))})}
                  "diagnosis" {:diagnosis (diagnose)}
                  "usage" {:usage (usage (when target-id (history)))}
                  "events" (session-read-events target-id opts turns)
                  "council" (session-read-council db target-id opts turns)
                  "full" (let [data (history)]
                           (cond-> {:diagnosis (diagnose)
                                    :turn-retries (safe-call #(retries-by-turn env (or selected turns)) {})
                                    :usage (usage data)
                                    :transcript (model-transcript opts data)}
                             (not position) (assoc :current-turn (lean-current-turn env target-id turns)
                                                   :session-forks (safe-call #(foundation-session-forks env target-id) [])))))))))))

;; ---------------------------------------------------------------------------
;; Strings-only boundary egress. read_session / get_session / list_sessions
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
        ;; Force any lazy leaf so no unrealized deref crosses the boundary
        ;; as a ForeignObject and breaks json.dumps.
        ;; Degrade a bad blob to nil rather than aborting the whole verb.
        (delay? x) (deep-stringify (try (force x) (catch Throwable _ nil)))
        (keyword? x) (kw->snake x)
        (symbol? x) (kw->snake (keyword x))
        :else x))

(defn- session-envelope [op result] (extension/success {:op op :result (deep-stringify result)}))

(defn- target-arg
  "Unwrap the ONE argument these reads take. A Python KEYWORD call
   (`read_session(target=…)`) crosses as a trailing dict whose keys are
   VERBATIM STRINGS, so accept that shape as well as the bare positional value
   and never make the caller choose between `target=` and a positional id.

   An id COPIED from the TUI header chip or the companion app arrives marked
   (`vis_session_id#<uuid>` — the marker that tells a reader the id names a Vis
   session), so it is stripped here, at the one door both reads share, and the
   caller can paste what the clipboard gave it."
  [value]
  (header/unmark-session-id
    (if (map? value) (or (get value "target") (get value "session_id") (get value "id")) value)))

(defn- search-arg
  "`list_sessions(search=…)`'s argument, positional or keyword (see
   `target-arg`)."
  [value]
  (if (map? value) (or (get value "search") (get value "query")) value))

(defn- foundation-inspect
  "Return a model-facing view, wrapped for the host."
  ([env] (foundation-inspect env nil))
  ([env target] (foundation-inspect env (target-arg target) (if (map? target) target {})))
  ([env target opts]
   (session-envelope :read-session (foundation-inspect-data env (target-arg target) opts))))

(defn- foundation-get-session
  "Envelope-wrapped single-session descriptor (see
   `foundation-session-descriptor`)."
  ([env] (session-envelope :get-session (foundation-session-descriptor env)))
  ([env target]
   (session-envelope :get-session (foundation-session-descriptor env (target-arg target)))))

(defn- foundation-sessions
  "Return one page of the session index. The last row ID is the next cursor."
  ([env] (foundation-sessions env nil))
  ([env search] (foundation-sessions env (search-arg search) (if (map? search) search {})))
  ([env search opts]
   (let [rows (foundation-sessions-data env search)
         cursor (get opts "cursor")
         at-cursor (when cursor (seq (drop-while #(not (same-uuid? (:id %) cursor)) rows)))]
     (when (and cursor (not at-cursor))
       (throw (ex-info "Unknown session cursor. Use the last row ID with the same search." {})))
     (session-envelope :list-sessions
                       (into [] (take (page-limit opts 50 200)) (if cursor (rest at-cursor) rows))))))

;; Removed extra workflow surfaces.

;; ---------------------------------------------------------------------------
;; Session-read IR render helpers
;;
;; Symbol introspection is not a foundation `v/` tool: the engine owns it
;; as the bare `doc` / `apropos` system calls wired into the Python
;; sandbox, since those describe the sandbox itself.
;; ---------------------------------------------------------------------------

;; -- public, doc-bearing aliases -----------------------------------------------
;; The underlying defs (`foundation-inspect`, `foundation-get-session`,
;; `foundation-sessions`) are private and named for clarity inside this ns.
;; Re-export them under their sandbox-visible names with `:doc` and `:arglists`
;; baked into the var meta so `extension/symbol` can read both straight off the var.
;;
;; The names are the attachment surface's rule applied here: `verb_noun`, plural
;; noun = the index and singular = ONE target, every read taking exactly one
;; target so name and id are never a question the caller answers, and variants
;; (`search`) staying ARGUMENTS instead of becoming more globals. `session_state`
;; in particular was a storage noun on the agent surface - it is a DB TABLE
;; (`session_state`, the fork rows `session_forks` reads) - so the tool that
;; reads a conversation is `read_session`.
(def
  ^{:doc
    "Developer alias for `foundation-inspect` — the model-facing contract is the
          symbol's `:description`/`:result` below."
    :arglists '([] [target])}
  read-session
  foundation-inspect)

(def
  ^{:doc
    "Developer alias for `foundation-get-session` — the model-facing contract is the
          symbol's `:description`/`:result` below."
    :arglists '([] [target])}
  get-session
  foundation-get-session)

(def
  ^{:doc
    "Developer alias for `foundation-sessions` — the model-facing contract is the
          symbol's `:description`/`:result` below."
    :arglists '([] [search])}
  list-sessions
  foundation-sessions)

(def read-session-symbol
  (extension/symbol
    #'read-session
    {:activity (presenter/for-tool :read_session)
     :inject-env? true
     :tag :observation
     :call {:opt-pos ["target"] :rest :auto}
     :params [{:name "target"} {:name "view" :note "index, blocks, diagnosis, usage, council, events or full"}
              {:name "turn"} {:name "iteration"} {:name "block"} {:name "offset"} {:name "limit"}]
     :description
     (str "Read one session. With `introspection` off, pass your parent session ID from "
           "`session[\"agent\"][\"parent_id\"]`. With it on, pass an ID, unique prefix, or copied "
           "`vis_session_id#<uuid>` marker, or omit the target to read the current session. "
           "Positions start at 1.")
     :result
     (str "A map with `session`, `session_id`, `view`, and the selected data. "
          "`index` lists six turn previews by default, newest first. Use `turn=N` for one `turn` with its request, "
          "`answer`, `answer_chars`, `answer_next_offset`, and iteration summaries. "
          "`blocks` reads `transcript.turns[].iterations[].blocks[]`. Select one block with "
          "`turn=N, iteration=K, block=B`. Use `limit=stdout_chars` to read all stdout into a "
          "variable, then filter it or write it to a file. Do not print large output. "
          "Answer and stdout slices use `offset` and `limit` in characters (4096 by default). "
          "`answer_next_offset` and `stdout_next_offset` are None on the last slice. "
          "`council` gives entries and child results. The index also shows child results. "
          "`events` gives typed journal rows and retained sequence coverage. "
          "Use `offset` and `limit` for index, Council, and event pages. `next_offset` is None "
          "on the last page. All views accept `turn`. Only `blocks` and `council` accept `iteration`. "
          "Only `blocks` accepts `block`. `diagnosis` gives failures. `usage` tool rows overlap. "
          "`full` gives history, diagnosis, usage, forks, and retries. It uses stdout slice limits. "
          "A session read does not restore folded steps.")}))

(def get-session-symbol
  (extension/symbol
    #'get-session
    {:activity (presenter/for-tool :get_session)
     :inject-env? true
     :tag :observation
     :call {:opt-pos ["target"]}
     :description
     (str
       "Get the descriptor of one session. `get_session()` is the current session, and "
       "`get_session(target)` is another session. `target` takes an id, an unambiguous prefix or the "
       "copied `vis_session_id#<uuid>` marker. It has no transcript: read the content with "
       "`read_session(id)` and the whole index with `list_sessions()`.")
     :result
     (str
       "String-keyed row `{id, channel, title, goal_status, turn_count, created_at, modified_at, "
       "is_current}`, plus `provider`/`model`/`provider_model` and `last_turn` "
       "(`{id, outcome, user_request}` or `{id, outcome, request_kind, council}`) when known. "
       "The model is the one that the session uses "
       "now: its model pick, else its default. `goal_status` is the status of a goal that "
       "still directs work (`active`, `paused`, `blocked` or `budget_limited`), else None. "
       "A starred session adds `favorite_rank` — the human's star order, lowest first. "
       "None when nothing matches.")}))

(def list-sessions-symbol
  (extension/symbol
    #'list-sessions
    {:activity (presenter/for-tool :list_sessions)
     :inject-env? true
     :tag :observation
     :call {:opt-pos ["search"] :rest :auto}
     :params [{:name "search"} {:name "limit"} {:name "cursor" :note "the last row's id"}]
     :description
     (str "Read one page of sessions, newest first, or use `search` to keep the TUI and app search order. "
          "`limit` defaults to 50 (maximum 200). Pass the last row's `id` as `cursor` to continue "
          "with the same search. A page with fewer than `limit` rows is the last page. "
          "An unknown cursor returns an error.")
     :result
     (str "String-keyed rows with `id`, `channel`, `title`, `goal_status`, `turn_count`, "
          "`created_at` and `modified_at`. A starred session has `favorite_rank`. "
          "A search match has `rank`, `is_in_*` flags and optional `request_snippet` and `reply_snippet`.")}))

(defn introspection-enabled? [_env] (toggles/enabled? "introspection"))

(defn- parent-session-id [{:keys [db-info session-id]}]
  (when (and db-info session-id)
    (:parent_id (persistance/db-agent-info db-info session-id))))

(defn- read-session-active? [env]
  (or (introspection-enabled? env) (some? (parent-session-id env))))

(defn- sandbox-read-session [env & args]
  (when-not (introspection-enabled? env)
    (when-let [parent-id (parent-session-id env)]
      (when-not (same-uuid? parent-id (target-arg (first args)))
        (throw (ex-info "With introspection off, read_session requires your parent session ID." {})))))
  (apply read-session env args))

(def all-symbols
  [(assoc read-session-symbol :ext.symbol/active-fn read-session-active?
                              :ext.symbol/fn sandbox-read-session)
   (assoc get-session-symbol :ext.symbol/active-fn introspection-enabled?)
   (assoc list-sessions-symbol :ext.symbol/active-fn introspection-enabled?)])

(toggles/register-toggle! {:id "introspection"
                           :label "Session introspection"
                           ;; One line for the row; the "debugging Vis itself, not ordinary project
                           ;; work" rationale is the section comment above.
                           :description
                           "Let the agent read its own session history and gateway event journals."
                           :default false
                           :scopes ["global" "project" "group" "session"]
                           :owner :vis
                           :persist? true
                           :group :sandbox})

(def ^:private INTROSPECTION_PROMPT
  (str
    "## Session introspection\n"
    "- Read session history only when the answer changes with what a past session says: session diagnostics, evidence from a referenced conversation, or context another session holds. A pointing word (\"that drug\", \"the file we fixed\") is a reference, not a reason. If the answer is the same either way, answer from your own knowledge. A session or turn start, and `/reload`, keep the conversation intact: continue from it.\n"
    "- For a missing fact, start with `print(await read_session(id, turn=N))`. It shows the request and its Council entries, the final answer, the steps, and the selected and actual model. A text in quotes is exact and complete. Read a field by key only when the print omits it. Use `view=\"events\"` only for delivery, retry, Task or provider-wait evidence. Gateway logs are not in a session read.\n"
    "- This session's conversation is already in front of you. Use `view=\"usage\"` or `view=\"diagnosis\"` for data the conversation cannot hold. Usage tool rows overlap: do not sum them.\n"
    "- A read does not restore a fold. To recover one fact that a gist omits, first name the fact and its `tN/iK` scope. Then use `view=\"blocks\"` with `turn=N, iteration=K` for code, stdout or errors. Reuse the result unless you need newer evidence.\n"
    "- Find a session with `await list_sessions(search=\"…\")`. Read turn previews with `read_session(id)`. For one block, add `view=\"blocks\", turn=N, iteration=K, block=B`. Use one search round.\n"
    "- The snippets `request_snippet` and `reply_snippet` are optional. Read them with `row.get(\"request_snippet\")` and `row.get(\"reply_snippet\")`; these return None for an absent field.\n"
    "- A session id copied from the TUI or the companion app has the form `vis_session_id#<uuid>`, which marks a Vis session. Pass it unchanged, or the bare id, to `read_session` or `get_session`.\n"
    "- Filter in `py3`; print the fields the question needs.\n"))

(defn prompt [env] (when (introspection-enabled? env) INTROSPECTION_PROMPT))
