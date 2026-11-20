(ns com.blockether.vis.internal.loop.trajectory
  "ATIF trajectory of one `vis-agent` run.

   `vis-agent --trajectory PATH` writes the run as one ATIF-v1.8 document. Harbor
   and its viewer read this format:

     user step      the prompt
     agent step     one for each iteration: thinking, prose, model and effort
     tool call      one `python_execution` call for each code block, with its `code`
     observation    the stdout and the error of that block
     final_metrics  the token and cost totals of the run
     subagents      one embedded trajectory for each Council child

   The final agent step carries the final answer. Vis facts without an ATIF
   field go to `extra`: iteration, block status, Activity, repairs, timeouts
   and the run profile. The Activity of a block has one row for each host
   function call, with its operation, arguments, result, state and duration.
   Each agent step has the tokens and the cost of its provider call in
   `metrics`. The `extra` of the document has the run status and evaluation,
   and for a `--persist` run the session usage and the saved goal.

   A Council child comes from its persisted turns, so its steps have the token
   counts and the cost of each iteration. The observation of the step that
   started the child refers to the child trajectory. The total cost of the run
   includes the cost of the children.

   The CLI result has two shapes: engine keywords, or the snake_case string keys
   of a `--persist` run through the gateway. [[build]] reads the canonical wire
   shape of both.

   Before the write, each known credential value becomes `[REDACTED]`: the
   provider keys and tokens in `~/.vis/*-auth.json`, in the provider
   configuration and from `api_key_command`, the gateway token, the values of
   secret settings, keychain and command values of
   `environment:`, and values of the environment, `.env` and `environment:` whose
   names contain KEY, TOKEN, SECRET, PASSWORD or CREDENTIAL. Each value also
   matches in its JSON-escaped and URL-encoded forms. Credentials that their
   form shows also become `[REDACTED]`: private PEM keys, passwords in URLs,
   credential headers and well-known API key and token formats. Base64 text that
   holds a known value or one of these forms becomes `[REDACTED]` too. Vis does not
   find other secrets in the text. Only the owner can read the file."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.internal.config.core :as config]
            [com.blockether.vis.internal.config.toggles :as toggles]
            [com.blockether.vis.internal.gateway.discovery :as discovery]
            [com.blockether.vis.internal.provider.credential-command :as cred]
            [com.blockether.vis.internal.content :as content]
            [com.blockether.vis.internal.error :as error]
            [com.blockether.vis.internal.gateway.runtime :as runtime]
            [com.blockether.vis.internal.loop.transcript :as transcript]
            [com.blockether.vis.internal.paths :as paths]
            [com.blockether.vis.internal.util :as util])
  (:import (java.io File)
           (java.nio.file CopyOption Files OpenOption Path StandardCopyOption)
           (java.nio.file.attribute FileAttribute PosixFilePermissions)
           (java.net URLEncoder)
           (java.nio.charset StandardCharsets)
           (java.time Instant)
           (java.util Base64 Locale)
           (java.util.regex Matcher)))

(def schema-version "The ATIF version of each trajectory that Vis writes." "ATIF-v1.8")

(def ^:private redacted "[REDACTED]")

(def ^:private min-secret-length
  "Shorter values are flags, counts and common words, not credentials."
  8)

(def ^:private secret-env-name
  "An environment variable name that holds a credential."
  #"(?i)KEY|TOKEN|SECRET|PASSWORD|CREDENTIAL")

(defn- compact "`m` without its nil values." [m] (into {} (remove (comp nil? val)) m))

(defn- iso-time [ms] (when (number? ms) (str (Instant/ofEpochMilli (long ms)))))

(defn- model-ref
  "The model and the provider that answered one iteration. Each value can be nil."
  [entry]
  (let [effort
        (get entry "reasoning_effort")

        selected
        (get effort "selected")]

    {:model (or (get selected "model")
                (get effort "model")
                (some #(get % "llm_model") (get entry "blocks")))
     :provider (or (get selected "provider") (get effort "provider"))}))

(defn- error-text
  "One trace error as the model saw it."
  [err]
  (some-> err
          wire/->engine
          transcript/error->display))

(defn- observation-text
  "The stdout and the error of one code block."
  [block]
  (or (get block "atif_output")
      (->> [(get block "stdout") (error-text (get block "error"))]
           (remove str/blank?)
           (str/join "\n"))))

(defn- call-ids
  "One id for each block of one iteration: the tool call id of the provider, or a
   new id when the block has none or shares it with another block."
  [iteration blocks]
  (let [ids
        (map #(get % "tool_call_id") blocks)

        counts
        (frequencies ids)]

    (map-indexed (fn [i id]
                   (if (and (util/non-blank-string? id) (= 1 (counts id)))
                     id
                     (str "vis-" iteration "-" (inc i))))
                 ids)))

(defn- block-activity
  "The Activity of one code block: its state, counts and rows. Each row is one
   host function call with its operation, arguments, result, state and
   duration. The history cursor stays out."
  [block]
  (not-empty (select-keys (get block "activity") ["state" "counts" "rows"])))

(defn- code-points
  "The number of Unicode code points in `s`. Python and the JSON text count
   characters this way, not as UTF-16 units."
  [^String s]
  (.codePointCount s 0 (.length s)))

(defn- tool-call
  "The ATIF tool call and observation result of one code block. A persisted form
   has its code in `src`. `ref` names the block when its envelope has no ref."
  [call-id block ref]
  (let [envelope (get block "envelope")]
    {:call {:tool-call-id call-id
            :function-name (or (get block "tool_name") "python_execution")
            :arguments {"code" (str (or (get block "code") (get block "src")))}
            :extra (compact {:status (get envelope "status")
                             :ref (or (get envelope "ref") ref)
                             :started-at-ms (get envelope "started_at_ms")
                             :finished-at-ms (get envelope "finished_at_ms")
                             :is-repaired (boolean (or (get envelope "is_repaired")
                                                       (get block "is_repaired")))
                             :is-timeout (boolean (or (get envelope "is_timeout")
                                                      (get block "is_timeout")))
                             :activity (block-activity block)
                             :stdout-chars (or (get block "atif_stdout_chars")
                                               (some-> (get block "stdout")
                                                       code-points))
                             :error (get block "error")
                             :repaired-source (get block "repaired_source")})}
     :result {:source-call-id call-id :content (observation-text block)}}))

(defn- usage-metrics
  "ATIF metrics of the provider call of one trace entry. The input count of Vis
   includes cached tokens, as ATIF prompt tokens do."
  [usage]
  (let [tokens (get usage "tokens")]
    (compact {:prompt-tokens (get tokens "input")
              :completion-tokens (get tokens "output")
              :cached-tokens (get tokens "cached")
              :cost-usd (some-> (get usage "cost_usd")
                                double)
              :extra (not-empty (compact {:cache-creation-tokens (get tokens "cache_created")
                                          :reasoning-tokens (get tokens "reasoning")}))})))

(defn- agent-step
  "The ATIF agent step of one trace entry. The final entry carries `answer`."
  [step-id entry answer]
  (let [iteration
        (get entry "iteration")

        blocks
        (get entry "blocks")

        final?
        (boolean (get entry "is_final"))

        calls
        (mapv tool-call (call-ids iteration blocks) blocks (repeat nil))

        started
        (get-in (first blocks) ["envelope" "started_at_ms"])

        {:keys [model provider]}
        (model-ref entry)

        effort
        (get-in entry ["reasoning_effort" "effective"])

        metrics
        (usage-metrics (get entry "usage"))

        thinking
        (get entry "thinking")]

    (cond-> {:step-id step-id
             :source "agent"
             :message (or (when final? answer) (get entry "assistant_prose") "")
             :extra (compact {:iteration iteration
                              :is-final final?
                              :provider provider
                              :error (error-text (get entry "error"))})}
      started
      (assoc :timestamp (iso-time started))

      model
      (assoc :model-name model)

      effort
      (assoc :reasoning-effort effort)

      (seq metrics)
      (assoc :metrics metrics)

      (util/non-blank-string? thinking)
      (assoc :reasoning-content thinking)

      (seq calls)
      (assoc :tool-calls
        (mapv :call calls) :observation
        {:results (mapv :result calls)}))))

(defn- epoch-ms
  "Epoch milliseconds of one wire time: a number or an ISO-8601 instant."
  [value]
  (cond (number? value) (long value)
        (string? value) (try (.toEpochMilli (Instant/parse value)) (catch Exception _ nil))))

(defn- text-value [value] (when (util/non-blank-string? value) value))

(defn- request-text
  "The text of one turn request."
  [request]
  (cond (string? request) request
        (nil? request) ""
        :else (wire/json-str request)))

(defn- form-refs
  "The ref `t<turn>/i<iteration>/f<form>` of each persisted form, as the envelope
   of a live form names it. Without a turn position, each ref is nil."
  [turn-position iteration forms]
  (map-indexed (fn [i _]
                 (when (and turn-position iteration)
                   (str "t" turn-position "/i" iteration "/f" (inc (long i)))))
               forms))

(defn- model-iteration-step
  "The ATIF agent step of one persisted iteration: of a Council child, or of a
   persisted run. The final iteration of a turn carries `answer`. `turn-position`
   is the position of its turn."
  [step-id turn-position iteration answer]
  (let [position
        (get iteration "position")

        forms
        (get iteration "forms")

        calls
        (mapv tool-call (call-ids position forms) forms (form-refs turn-position position forms))

        created
        (epoch-ms (get iteration "created_at"))

        model
        (text-value (or (get iteration "llm_actual_model") (get iteration "model")))

        metrics
        (compact {:prompt-tokens (get iteration "input_tokens")
                  :completion-tokens (get iteration "output_tokens")
                  :cached-tokens (get iteration "input_cache_read_tokens")
                  :cost-usd (some-> (get iteration "cost_usd")
                                    double)
                  :extra (not-empty (compact {:cache-creation-tokens
                                              (get iteration "input_cache_write_tokens")
                                              :reasoning-tokens (get iteration
                                                                     "output_reasoning_tokens")}))})

        thinking
        (get iteration "thinking")]

    (cond-> {:step-id step-id
             :source "agent"
             :message (or answer (get iteration "assistant_prose") "")
             :extra (compact {:iteration position
                              :is-final (if (get iteration "atif_imported_step")
                                          (get iteration "atif_final")
                                          (some? answer))
                              :provider (text-value (or (get iteration "llm_actual_provider")
                                                        (get iteration "provider")))
                              :finished-at-ms (or (epoch-ms (get iteration "finished_at")) created)
                              :error (or (get iteration "atif_step_error")
                                         (error-text (get iteration "error")))
                              :attachments (not-empty (get iteration "attachments"))})}
      created
      (assoc :timestamp (iso-time created))

      model
      (assoc :model-name model)

      (seq metrics)
      (assoc :metrics metrics)

      (util/non-blank-string? thinking)
      (assoc :reasoning-content thinking)

      (seq calls)
      (assoc :tool-calls
        (mapv :call calls) :observation
        {:results (mapv :result calls)}))))

(defn- unattributed-cost-step
  [step-id cost]
  {:step-id step-id
   :source "agent"
   :message ""
   :metrics {:cost-usd cost}
   :extra {:is-unattributed-cost true}})

(defn- iteration-step
  [step-id turn-position iteration answer]
  (if (get iteration "atif_unattributed_cost")
    (unattributed-cost-step step-id (get iteration "cost_usd"))
    (model-iteration-step step-id turn-position iteration answer)))

(defn- cache-warm-step
  [step-id warm]
  (let [tokens (get warm "tokens")]
    (compact {:step-id step-id
              :source "agent"
              :message ""
              :timestamp (some-> (epoch-ms (get warm "created_at"))
                                 iso-time)
              :model-name (get warm "model")
              :metrics (compact {:prompt-tokens (get tokens "input")
                                 :completion-tokens (get tokens "output")
                                 :cached-tokens (get tokens "cached")
                                 :cost-usd (get warm "cost_usd")
                                 :extra (not-empty (compact {:cache-creation-tokens
                                                             (get tokens "cache_created")}))})
              :extra (compact {:is-cache-warm true :provider (get warm "provider")})})))

(defn- with-unattributed-cost
  "Keep saved costs that have no call record, including warms in older sessions."
  [steps first-id turn project-step]
  (let [saved
        (double (or (get-in turn ["cost" "total_cost"]) 0))

        exported
        (reduce + 0.0 (keep #(get-in % [:metrics :cost-usd]) steps))

        difference
        (- saved (double exported))]

    (if (> difference 1.0e-10)
      (conj steps (project-step (unattributed-cost-step (+ first-id (count steps)) difference)))
      steps)))

(defn- turn-steps
  "The ATIF steps of one turn of a Council child, from step `first-id`: the
   request, then one agent step for each persisted iteration."
  [first-id turn project-step]
  (let [answer
        (or (not-empty (content/text-projection (vec (get turn "content"))))
            (text-value (get turn "answer")))

        iterations
        (or (seq (get turn "trace")) (when answer [{}]))

        attachments
        (into (vec (get turn "attachments")) (get (first iterations) "atif_user_attachments"))

        created
        (epoch-ms (get turn "created_at"))]

    (loop [remaining
           iterations

           steps
           [(project-step (cond-> {:step-id first-id
                                   :source "user"
                                   :message (request-text (get turn "request"))}
                            created
                            (assoc :timestamp (iso-time created))

                            (seq attachments)
                            (assoc-in [:extra :attachments] attachments)))]]

      (if (seq remaining)
        (let [more
              (next remaining)

              iteration
              (first remaining)

              steps
              (reduce (fn [out warm]
                        (conj out (project-step (cache-warm-step (+ first-id (count out)) warm))))
                      steps
                      (get iteration "cache_warms"))]

          (recur more
                 (cond-> steps
                   (not (get iteration "atif_omit_step"))
                   (conj (project-step (iteration-step (+ first-id (count steps))
                                                       (get turn "position")
                                                       iteration
                                                       (when (and (not more)
                                                                  (not (get iteration
                                                                            "atif_imported_step")))
                                                         answer)))))))
        (with-unattributed-cost steps first-id turn project-step)))))

(defn- child-steps
  "The ATIF steps of each turn of one Council child. A child without a turn has
   only its task."
  [record project-step]
  (or (not-empty (reduce (fn [steps turn]
                           (into steps (turn-steps (inc (count steps)) turn project-step)))
                         []
                         (get record "turns")))
      [(project-step {:step-id 1 :source "user" :message (request-text (get record "task"))})]))

(defn- step-end-ms
  "The time when one agent step ended: its own end, else the end of its last call."
  [step]
  (or (get-in step [:extra :finished-at-ms])
      (some->> (:tool-calls step)
               (keep #(get-in % [:extra :finished-at-ms]))
               seq
               (apply max))))

(defn- spawn-index
  "The index of the step that started a child at `created-ms`: the first agent
   step that ends at or after that time. Without a time, the last agent step."
  [steps created-ms]
  (let [indexes (keep-indexed (fn [i step]
                                (when (and (= "agent" (:source step))
                                           (not (get-in step [:extra :is-cache-warm]))
                                           (not (get-in step [:extra :is-unattributed-cost])))
                                  i))
                              steps)]
    (or (when created-ms
          (some (fn [i]
                  (when-let [end (step-end-ms (nth steps i))]
                    (when (>= (long end) (long created-ms)) i)))
                indexes))
        (last indexes))))

(defn- with-ref
  "`step` with `ref` on the observation result of the call that started the
   child: the first call that ends at or after `created-ms`, else the last call.
   A step without calls gets an observation result with only the reference."
  [step ref created-ms]
  (let [results
        (get-in step [:observation :results])

        index
        (when (seq results)
          (or (when created-ms
                (first (keep-indexed (fn [i call]
                                       (when-let [end (get-in call [:extra :finished-at-ms])]
                                         (when (>= (long end) (long created-ms)) i)))
                                     (:tool-calls step))))
              (dec (count results))))]

    (if index
      (update-in step [:observation :results index :subagent-trajectory-ref] (fnil conj []) ref)
      (update-in step [:observation :results] (fnil conj []) {:subagent-trajectory-ref [ref]}))))

(defn- subagent-ref
  "The ATIF reference to one embedded trajectory."
  [trajectory]
  (compact {:trajectory-id (:trajectory-id trajectory)
            :session-id (:session-id trajectory)
            :extra (not-empty (compact {:status (get-in trajectory [:extra :status])}))}))

(defn- subagent-cost
  "The total cost of the embedded trajectories, or nil when none has a cost."
  [trajectory]
  (some->> (:subagent-trajectories trajectory)
           (keep #(get-in % [:final-metrics :total-cost-usd]))
           seq
           (reduce +)))

(defn- with-subagents
  "`trajectory` with one embedded trajectory for each child record, and a
   reference on the step that started the child. `embed` builds one embedded
   trajectory from one record."
  [trajectory records embed]
  (if-let [records (seq records)]
    (let [embedded (mapv embed records)]
      (-> trajectory
          (assoc :subagent-trajectories embedded)
          (update :steps
                  (fn [steps]
                    (reduce (fn [steps [record sub]]
                              (let [created (epoch-ms (get record "created_at"))]
                                (if-let [i (spawn-index steps created)]
                                  (update steps i with-ref (subagent-ref sub) created)
                                  steps)))
                            (vec steps)
                            (map vector records embedded))))))
    trajectory))

(defn- child-metrics
  "The totals of one child trajectory: the totals of its turns, else the sums of
   its step metrics. The cost includes the children of the child."
  [record trajectory]
  (let [sum
        (fn [values]
          (some->> values
                   (remove nil?)
                   seq
                   (reduce +)))

        turns
        (get record "turns")

        metrics
        (keep :metrics (:steps trajectory))

        total
        (fn [path step-key]
          (or (sum (map #(get-in % path) turns)) (sum (map step-key metrics))))

        costs
        (remove nil? [(sum (map :cost-usd metrics)) (subagent-cost trajectory)])]

    (compact {:total-prompt-tokens (total ["tokens" "input"] :prompt-tokens)
              :total-completion-tokens (total ["tokens" "output"] :completion-tokens)
              :total-cached-tokens (total ["tokens" "cached"] :cached-tokens)
              :total-cost-usd (when (seq costs) (double (reduce + costs)))
              :total-steps (count (:steps trajectory))})))

(defn- saved-extra
  "The session metadata and the end time of its last dated step."
  [record steps]
  (not-empty (compact {:status (text-value (get record "status"))
                       :title (text-value (get record "title"))
                       :cwd (text-value (get record "cwd"))
                       :started-at (some-> (or (epoch-ms (get record "started_at"))
                                               (epoch-ms (get record "created_at")))
                                           iso-time)
                       :last-activity-at (some->> (rseq steps)
                                                  (some #(or (step-end-ms %)
                                                             (epoch-ms (:timestamp %))))
                                                  iso-time)})))

(defn- persisted-trajectory
  "The ATIF trajectory of one saved session. `children` maps a
   parent session id to its child records, so a child also embeds its own
   children. `seen` holds the session ids above the child, so a bad parent link
   cannot loop."
  [{:keys [children read-turns project-step]} seen record]
  (let [sid
        (str (get record "session_id"))

        seen
        (conj seen sid)

        record
        (cond-> record
          read-turns
          (assoc "turns" (read-turns sid)))

        steps
        (child-steps record (or project-step identity))

        provider
        (or (text-value (get record "provider")) (some #(get-in % [:extra :provider]) steps))

        trajectory
        (with-subagents {:schema-version schema-version
                         :session-id sid
                         :trajectory-id sid
                         :agent (compact {:name "vis"
                                          :version (runtime/release-version)
                                          :model-name (or (text-value (get record "atif_model"))
                                                          (text-value (get record "model"))
                                                          (some :model-name steps))
                                          :extra (not-empty (compact
                                                              {:provider provider
                                                               :parent-session-id
                                                               (some-> (get record "parent_id")
                                                                       str)
                                                               :depth (get record "depth")}))})
                         :steps steps
                         :extra (saved-extra record steps)}
                        (remove #(contains? seen (str (get % "session_id"))) (get children sid))
                        #(persisted-trajectory
                           {:children children :read-turns read-turns :project-step project-step}
                           seen
                           %))]

    (assoc trajectory :final-metrics (child-metrics record trajectory))))

(defn- children-by-parent
  "Child records by parent session id, each session once and the oldest first. A
   child without a known parent belongs to `root`."
  [root records]
  (let [unique
        (->> records
             (map wire/canonical)
             (remove #(contains? #{"" root} (str (get % "session_id"))))
             (reduce (fn [[ids out] record]
                       (let [sid (str (get record "session_id"))]
                         (if (contains? ids sid) [ids out] [(conj ids sid) (conj out record)])))
                     [#{} []])
             second
             (sort-by #(or (epoch-ms (get % "created_at")) 0)))

        known
        (into #{root} (map #(str (get % "session_id"))) unique)]

    (group-by (fn [record]
                (let [parent (some-> (get record "parent_id")
                                     str)]
                  (if (contains? known parent) parent root)))
              unique)))

(defn- with-subagent-cost
  "Run totals with the cost of the embedded trajectories. The ATIF total cost
   includes the subagents."
  [metrics cost]
  (if cost
    (-> metrics
        (assoc :total-cost-usd (+ (double (or (:total-cost-usd metrics) 0.0)) (double cost)))
        (assoc-in [:extra :subagent-cost-usd] (double cost)))
    metrics))

(defn- final-metrics
  "Run totals. The input count of Vis includes cached tokens, as ATIF prompt tokens do."
  [run step-count]
  (let [tokens (get run "tokens")]
    (compact {:total-prompt-tokens (get tokens "input")
              :total-completion-tokens (get tokens "output")
              :total-cached-tokens (get tokens "cached")
              :total-cost-usd (some-> (get-in run ["cost" "total_cost"])
                                      double)
              :total-steps step-count
              :extra (not-empty (compact {:cache-creation-tokens (get tokens "cache_created")
                                          :reasoning-tokens (get tokens "reasoning")
                                          :total-tokens (get tokens "total")
                                          :iteration-count (get run "iteration_count")
                                          :duration-ms (get run "duration_ms")}))})))

(defn- tool-definitions
  "The tools that the provider sees, in the function calling schema of OpenAI."
  []
  (mapv (fn [tool]
          {:type "function"
           :function
           {:name (:name tool) :description (:description tool) :parameters (:schema tool)}})
        (transcript/model-facing-tools)))

(defn build
  "ATIF document of one finished run. `result` is the CLI result map. Options:

     :prompt         the user prompt
     :started-at-ms  the time when the run started
     :profile        the run options that shape the agent
     :iterations     the persisted iterations of the turn of the run. A persisted
                     run returns no `trace`, so they give its agent steps.
     :turn-position  the position of that turn. It gives the refs of its forms.
     :subagents      the Council children of the run: agent rows with their
                     `turns`, and each turn with its persisted iterations in
                     `trace`
     :session-usage  the usage rollup of a persisted session
     :goal           the saved goal of a persisted session"
  [result
   {:keys [prompt started-at-ms profile iterations turn-position subagents session-usage goal]}]
  (let [run
        (wire/canonical (select-keys result
                                     [:session-id :content :trace :iteration-count :duration-ms
                                      :tokens :cost :status :eval]))

        answer
        (not-empty (content/text-projection (vec (get run "content"))))

        persisted
        (when (empty? (get run "trace")) (not-empty (vec iterations)))

        steps
        (into [(cond-> {:step-id 1 :source "user" :message (str prompt)}
                 started-at-ms
                 (assoc :timestamp (iso-time started-at-ms)))]
              (if persisted
                (map-indexed (fn [i iteration]
                               (iteration-step (+ 2 i)
                                               turn-position
                                               iteration
                                               (when (= i (dec (count persisted))) answer))))
                (map-indexed (fn [i entry]
                               (agent-step (+ 2 i) entry answer))))
              (or persisted (get run "trace")))

        root
        (some-> (get run "session_id")
                str)

        children
        (children-by-parent (or root "") subagents)

        document
        (with-subagents
          (cond-> {:schema-version schema-version
                   :trajectory-id (or root (str (random-uuid)))
                   :agent (compact
                            {:name "vis"
                             :version (runtime/release-version)
                             :model-name (or (get-in run ["cost" "model"]) (some :model-name steps))
                             :tool-definitions (tool-definitions)
                             :extra (compact {:build (runtime/build-id)
                                              :provider (or (get-in run ["cost" "provider"])
                                                            (some #(get-in % [:extra :provider])
                                                                  steps))
                                              :profile profile})})
                   :steps steps
                   :final-metrics (final-metrics run (count steps))
                   :extra (compact {:status (get run "status")
                                    :error (some-> (:error result)
                                                   error/format-error)
                                    :eval (not-empty (get run "eval"))
                                    :session-usage (not-empty session-usage)
                                    :goal goal})}
            root
            (assoc :session-id root))
          (get children (or root ""))
          #(persisted-trajectory {:children children} #{(or root "")} %))]

    (update document :final-metrics with-subagent-cost (subagent-cost document))))

(defn- secret-leaves
  "Each string of `data` under a credential key."
  ([data] (secret-leaves data false))
  ([data secret?]
   (cond (map? data) (mapcat (fn [[k v]]
                               (secret-leaves v (or secret? (util/secret-key? k))))
                             data)
         (sequential? data) (mapcat #(secret-leaves % secret?) data)
         (and secret? (string? data)) [data]
         :else nil)))

(defn- read-json-file [^File file] (try (wire/parse-json (slurp file)) (catch Exception _ nil)))

(defn known-secrets
  "Credential values that a trajectory must not carry, longest first:

     :env        environment values whose names contain KEY, TOKEN, SECRET,
                 PASSWORD or CREDENTIAL
     :files      JSON credential files: each string under a credential key
     :providers  the provider configuration: each string under a credential key
     :settings   the values of secret settings
     :environment  the values that Vis gives to child processes: an
                 `environment:` value from the keychain or a command, and a
                 `.env` or `environment:` value under a credential name, as
                 `[[name source value]]`

   Values with fewer than 8 characters do not count."
  [{:keys [env files providers settings environment]}]
  (->> (concat (keep (fn [[k v]]
                       (when (re-find secret-env-name (str k)) v))
                     env)
               (mapcat (comp secret-leaves read-json-file) files)
               (secret-leaves providers)
               settings
               (keep (fn [[k source v]]
                       (when (or (#{:keychain :command} source) (re-find secret-env-name (str k)))
                         v))
                     environment))
       (filter #(and (string? %) (<= (long min-secret-length) (count %))))
       distinct
       (sort-by count >)
       vec))

(defn- credential-files
  "The provider credential files `~/.vis/<vendor>-auth.json`."
  []
  (filter (fn [^File f]
            (and (.isFile f) (str/ends-with? (.getName f) "-auth.json")))
          (.listFiles (io/file (paths/vis-home)))))

(defn- environment
  "`[[name source value]]` of the variables that Vis gives to child processes in the
   bound workspace."
  []
  (try (into []
             cat
             [(for [name
                    (config/declared-environment-names)

                    :let [{:keys [source value]}
                          (config/extension-env-status name)]
                    :when (and value (not= :literal source))]

                [name source value])
              (for [[name value] (config/workspace-environment-values)]
                [name :dotenv value])])
       (catch Exception _ nil)))

(defn- json-escaped
  "`s` as it shows inside a JSON string."
  [s]
  (let [encoded (wire/json-str s)]
    (subs encoded 1 (dec (count encoded)))))

(defn- url-encoded
  "`s` as `URLEncoder` and `encodeURIComponent` encode it, with upper and lower hex."
  [^String s]
  (let [form
        (URLEncoder/encode s StandardCharsets/UTF_8)

        spaces
        (str/replace form "+" "%20")]

    (distinct (mapcat (fn [x]
                        [x (str/replace x #"%[0-9A-F]{2}" str/lower-case)])
                      [form spaces]))))

(defn- with-escaped
  "`secrets` and their JSON-escaped and URL-encoded forms, because text can hold JSON and
   URLs."
  [secrets]
  (vec (distinct (mapcat #(list* % (json-escaped %) (url-encoded %)) secrets))))

(defn secret-forms
  "The credentials in `values` as an export shows them: each value of 8 characters or
   more, with its JSON-escaped and URL-encoded forms."
  [values]
  (with-escaped (known-secrets {:settings values})))

(defn- gateway-token
  "The bearer token in the default token file of the local gateway, or nil."
  []
  (let [file (discovery/default-token-file)]
    (when (.isFile file) (try (str/trim (slurp file)) (catch Exception _ nil)))))

(defn run-secrets
  "The known credentials of this process and of the bound workspace, with their forms:
   also the gateway token and the provider keys that `api_key_command` gave."
  []
  (with-escaped (known-secrets
                  {:env (System/getenv)
                   :files (credential-files)
                   :providers (try (:providers (config/current-config)) (catch Exception _ nil))
                   :settings (concat (toggles/secret-values) (cred/cached-tokens) [(gateway-token)])
                   :environment (environment)})))

(defn setting-secrets
  "The values of the secret settings in `settings`, `{setting-id value}`, with their forms."
  [settings]
  (secret-forms (for [[id value]
                      settings

                      :when (toggles/secret-setting? id)
                      leaf
                      (secret-leaves value true)]

                  leaf)))

(defn- replace-spans
  "Replace spans of `s` in one scan. `start` is the first index where a span can start,
   or -1 when `s` has no span.
   `(next-start s from)` gives the next index from `from` where a span can start, or -1.
   `(span s i)` gives `[end text]` for the span from `i` to `end` and its new text,
   `[end nil]` to keep the text to `end`, or nil when no span starts at `i`. The result
   is `s` itself when no span has a new text."
  [^String s start next-start span]
  (loop [i
         (long start)

         from
         0

         out
         nil]

    (if (neg? i)
      (if out (str (.append ^StringBuilder out s (int from) (.length s))) s)
      (let [[end text] (span s i)]
        (cond (nil? end) (recur (long (next-start s (inc i))) from out)
              (nil? text) (recur (long (next-start s end)) from out)
              :else (recur (long (next-start s end))
                           (long end)
                           (doto ^StringBuilder (or out (StringBuilder. (.length s)))
                             (.append s (int from) (int i))
                             (.append ^String text))))))))

(defmacro ^:private pair-code
  "A code of the characters `a` and `b`, from 0 to 65535. With a `fold` of 0x20, an ASCII
   letter has the same code in each case; else `fold` is 0. A macro, because a function
   call for each character of a text is slow."
  [fold a b]
  `(bit-or (bit-shift-left (bit-and (bit-or (int ~a) ~fold) 0xFF) 8)
           (bit-and (bit-or (int ~b) ~fold) 0xFF)))

(deftype LiteralIndex [^long width ^long fold ^bytes shifts ^objects groups])

(defn- literal-index
  "An index of `literals` for a search that does not read each character of a text. A
   window with the `width` of the shortest literal moves on the text. By the
   [[pair-code]] of the last two characters of the window, `shifts` gives how far the
   window moves before a literal can start in it. By the [[pair-code]] of their first
   two characters, `groups` gives the literals, the longest first. A literal of one
   character has no entry. When `fold-case?` is true, the literals are in lower case and
   each letter case matches."
  (^LiteralIndex [literals] (literal-index literals false))
  (^LiteralIndex [literals fold-case?]
   (let [literals
         (->> (distinct literals)
              (filter #(<= 2 (count %)))
              (sort-by count >))

         ;; A shift is a byte.
         width
         (long (apply min 128 (map count literals)))

         fold
         (if fold-case? 0x20 0)

         shifts
         (byte-array 65536 (byte (dec width)))

         groups
         (object-array 65536)]

     ;; Only the first `width` characters of a literal are in the window.
     (doseq [^String literal
             literals

             end
             (range 1 width)]

       (let [k (pair-code fold (.charAt literal (dec (long end))) (.charAt literal (long end)))]
         (aset shifts k (byte (min (aget shifts k) (- width 1 (long end)))))))
     (doseq [[k group] (group-by (fn [^String literal]
                                   (pair-code fold (.charAt literal 0) (.charAt literal 1)))
                                 literals)]
       (aset groups (int k) (into-array String group)))
     (LiteralIndex. width fold shifts groups))))

(defmacro ^:private literals-at
  "The literals of `groups` with the first two characters of `s` at `i`, or nil. `i` is
   not the last index of `s`. A macro for the same reason as [[pair-code]]."
  [groups fold s i]
  `(aget ~groups (pair-code ~fold (.charAt ~s ~i) (.charAt ~s (inc ~i)))))

(defn- literal-at
  "The longest literal of `index` that starts at `i` in `s`, or nil."
  [^LiteralIndex index ^String s ^long i]
  (let [fold
        (.-fold index)

        ^objects groups
        (.-groups index)]

    (when (< (inc i) (.length s))
      (when-let [^objects literals (literals-at groups fold s i)]
        ;; One loop for each comparison, because a choice at each literal is slow.
        (if (zero? fold)
          (loop [j 0]
            (when (< j (alength literals))
              (let [^String literal (aget literals j)]
                (if (.startsWith s literal (int i)) literal (recur (inc j))))))
          (loop [j 0]
            (when (< j (alength literals))
              (let [^String literal (aget literals j)]
                (if (.regionMatches s true (int i) literal 0 (.length literal))
                  literal
                  (recur (inc j)))))))))))

(defn- literal-from
  "The first index from `from` and before `stop` in `s` where a literal of `index` starts,
   or -1. `stop` is at most the last index of `s`. This search tests each index."
  ^long [^LiteralIndex index ^String s ^long from ^long stop]
  (let [fold
        (.-fold index)

        ^objects groups
        (.-groups index)]

    (loop [i from]
      (cond (>= i stop) -1
            (and (literals-at groups fold s i) (literal-at index s i)) i
            :else (recur (inc i))))))

(defn- next-literal
  "The first index from `from` in `s` where a literal of `index` starts, or -1. A regex of
   all literals tries each literal at each index. This search reads the end of the
   window and moves it by the shift of those characters."
  ^long [^LiteralIndex index ^String s ^long from]
  (let [n
        (.length s)

        width
        (.-width index)

        fold
        (.-fold index)

        ^bytes shifts
        (.-shifts index)]

    ;; A move waits for the read before it, so a move costs more than a test of one
    ;; index. Thus a short window, which moves by few characters, tests each index.
    (if (< width 6)
      (literal-from index s from (dec n))
      (loop [end (+ from (dec width))]
        (if (>= end n)
          -1
          (let [shift (long (aget shifts (pair-code fold (.charAt s (dec end)) (.charAt s end))))]
            (if (pos? shift)
              (recur (+ end shift))
              ;; A literal can start here. When it does not, the next shifts are often
              ;; short too, so the search tests each index of one more window.
              (let [start (- end (dec width))
                    stop (min (dec n) (+ start width))
                    i (literal-from index s start stop)]

                (if (neg? i) (recur (+ stop (dec width))) i)))))))))

(defn- replace-secrets
  "Replace in `s` each secret of `index`, from the left, the longest first."
  [index ^String s]
  (replace-spans s
                 (next-literal index s 0)
                 #(next-literal index %1 (long %2))
                 (fn [s i]
                   [(+ (long i) (count (literal-at index s (long i)))) redacted])))

(def ^:private token-format
  "Credentials that their form shows: provider API keys and access tokens of
   OpenAI, Anthropic, Stripe, GitHub, GitLab, Slack, AWS, Google, npm and
   Hugging Face, and JSON Web Tokens, also encrypted ones."
  (re-pattern
    (str "(?<![A-Za-z0-9_-])(?:"
         (str/join
           "|"
           ["sk-(?=[A-Za-z0-9_-]*[0-9])[A-Za-z0-9_-]{30,}"
            "(?:sk|rk)_(?:live|test)_[0-9A-Za-z]{20,}" "gh[pousr]_[A-Za-z0-9]{36,}"
            "github_pat_[A-Za-z0-9_]{50,}" "gl(?:pat|dt|ptt|rt)-[A-Za-z0-9_-]{20,}"
            "xox[abposr]-[A-Za-z0-9-]{10,}" "(?:AKIA|ASIA)[A-Z0-9]{16}" "AIza[0-9A-Za-z_-]{35}"
            "npm_[A-Za-z0-9]{36}" "hf_[A-Za-z0-9]{30,}"
            "eyJ[A-Za-z0-9_-]{10,}\\.[A-Za-z0-9_-]*\\.[A-Za-z0-9_-]*(?:\\.[A-Za-z0-9_-]+){0,2}"])
         ")(?![A-Za-z0-9_-])")))

(def ^:private token-markers
  "Each match of [[token-format]] starts with one of these."
  ["sk-" "sk_" "rk_" "ghp_" "gho_" "ghu_" "ghs_" "ghr_" "github_pat_" "glpat-" "gldt-" "glptt-"
   "glrt-" "xox" "AKIA" "ASIA" "AIza" "npm_" "hf_" "eyJ"])

(def ^:private private-key-start "-----BEGIN (?:[A-Z0-9]+ )*PRIVATE KEY-----")

(def ^:private private-key-end #"-----END (?:[A-Z0-9]+ )*PRIVATE KEY-----")

(def ^:private putty-key-end #"Private-MAC:[ \t]*[0-9a-fA-F]+")

(def ^:private url-credential
  "The user and password in the authority of a URL."
  #"(?i)(?<![\w+.-])([a-z][a-z0-9+.-]*+://)(?-i)[^\s/?#@:]++:[^\s/?#@]++@")

(defn- base64-text
  "The UTF-8 text that the base64 or base64url `run` encodes, or nil."
  [^String run]
  (let [body
        (cond-> run
          (or (str/includes? run "=") (str/includes? run "\n"))
          (str/replace #"=+$|\r|\n" ""))

        url?
        (or (str/includes? body "_") (str/includes? body "-"))]

    ;; A file path has the signs of the two alphabets. It is not base64, and the
    ;; exception of the decoder is slow.
    (when-not (or (= 1 (mod (count body) 4))
                  (and url? (or (str/includes? body "/") (str/includes? body "+"))))
      (try (String. (.decode (if url? (Base64/getUrlDecoder) (Base64/getDecoder)) ^String body)
                    StandardCharsets/UTF_8)
           (catch IllegalArgumentException _ nil)))))

(defn- printable?
  "True for text without control characters and without bytes that are not UTF-8."
  [^String s]
  (let [n (.length s)]
    ;; A loop, because a regex search for each short text is slow.
    (loop [i 0]
      (or (== i n)
          (let [c (int (.charAt s i))]
            (and (not (or (< c 9) (and (< 13 c) (< c 32)) (== c 127) (== c 0xFFFD)))
                 (recur (inc i))))))))

(defn- basic-credential?
  "True when `token` is the base64 of a `user:password` pair."
  [^String token]
  (boolean (when-let [text (base64-text token)]
             (and (printable? text) (str/includes? text ":")))))

(def ^:private min-base64-length
  "The length of the base64 of the shortest credential."
  (long (Math/ceil (/ (* 4 (long min-secret-length)) 3.0))))

(def ^:private wrapped-base64
  "Base64 in lines of 64 or 76 characters. Group 1 is its last line. The group is absent
   when the text after the lines is not the end of base64."
  #"(?:(?:[A-Za-z0-9+/]{76}|[A-Za-z0-9+/]{64})\r?\n)++([A-Za-z0-9+/]*+={0,2}(?![A-Za-z0-9+/_=-]))?")

(defn- char-table
  "A table that is true at the code of each of the ASCII `chars`, for [[in-table?]]."
  ^booleans [chars]
  (let [table (boolean-array 128)]
    (doseq [c chars]
      (aset table (int c) true))
    table))

(def ^:private alphanumeric
  "The ASCII letters and digits, each as one character. The regex engine tests the single
   characters of a class with one table, and each range of a class with one more call."
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789")

(def ^:private ^"[Z" base64-chars
  "The base64 and base64url characters."
  (char-table (str alphanumeric "+/_-")))

(defmacro ^:private in-table?
  "True when the character at `i` in `s` is in `table`, which is of [[char-table]]. A
   macro, because a function call for each character of a text is slow."
  [table s i]
  `(let [c# (int (.charAt ~s ~i))]
     (and (< c# 128) (aget ~table c#))))

(defn- base64-end
  "The index after the base64 characters that start at `i` in `s`."
  ^long [^String s ^long i]
  (let [n
        (.length s)

        ^booleans table
        base64-chars]

    (loop [j i]
      (if (and (< j n) (in-table? table s j)) (recur (inc j)) j))))

(defn- padded-end
  "The index after the `=` signs of the base64 run whose characters stop at `end` in `s`.
   It is -1 when base64 text follows there, because a part of a longer text is not a run."
  ^long [^String s ^long end]
  (let [n
        (.length s)

        e
        (long (loop [e end]
                (if (and (< e n) (< (- e end) 2) (= \= (.charAt s e))) (recur (inc e)) e)))]

    (if (and (< e n) (or (= \= (.charAt s e)) (< e (base64-end s e)))) -1 e)))

(defn- base64-start
  "The first index from `from` in `s` where a base64 run starts that is long enough for a
   credential, or -1. A run that starts before `from` must not go on at `from`. Such a
   run has the character that is `min-base64-length` after the last character out of a
   run. Thus the search reads only that character and the run around it."
  [^String s from]
  (let [n
        (.length s)

        minimum
        (long min-base64-length)

        ^booleans table
        base64-chars]

    ;; No run that is long enough starts before `low`, and no run goes on at `low`.
    (loop [low (long from)]
      (let [probe (+ low (dec minimum))]
        (if (>= probe n)
          -1
          (let [start (long (loop [i probe]
                              (if (and (>= i low) (in-table? table s i)) (recur (dec i)) (inc i))))
                end (long (loop [j (max start (inc probe))]
                            (if (and (< j n) (< (- j start) minimum) (in-table? table s j))
                              (recur (inc j))
                              j)))]

            (if (< (- end start) minimum) (recur (inc end)) start)))))))

(defn- replace-base64
  "Replace in `s` each base64 run for which `hidden?` is true."
  [^String s hidden?]
  (let [start (long (base64-start s 0))]
    (if (neg? start)
      s
      (let [n (.length s)
            wrapped (delay (re-matcher wrapped-base64 s))
            mask (fn [run]
                   (when (hidden? run) redacted))]

        (replace-spans s
                       start
                       base64-start
                       (fn [_ i]
                         (let [i (long i)
                               end (base64-end s i)]

                           ;; Only a line of 64 or 76 characters can start wrapped base64.
                           (if-let [^Matcher lines
                                    (when (and (or (== 64 (- end i)) (== 76 (- end i)))
                                               (< end n)
                                               (#{\return \newline} (.charAt s end)))
                                      (let [^Matcher m @wrapped]
                                        (when (.lookingAt (.region m (int i) (int n))) m)))]
                             (let [block (subs s i (.end lines))]
                               [(.end lines)
                                (if (neg? (.start lines 1))
                                  ;; The lines do not end as base64, so each line is a run. All lines go
                                  ;; in this one span, because a new try at each line reads them all again.
                                  (str/replace block #"[^\r\n]+" #(or (mask %) %))
                                  (mask block))])
                             (let [e (padded-end s end)]
                               (if (neg? e) [end nil] [e (mask (subs s i e))]))))))))))

(defn- base64-candidate?
  "True for a run with a digit or with both letter cases."
  [^String run]
  (loop [i
         0

         lower
         false

         upper
         false]

    (if (< i (.length run))
      (let [c (.charAt run i)]
        (cond (Character/isDigit c) true
              (Character/isLowerCase c) (if upper true (recur (inc i) true upper))
              (Character/isUpperCase c) (if lower true (recur (inc i) lower true))
              :else (recur (inc i) lower upper)))
      false)))

(defn- authorization-value
  "A pattern for an `Authorization` value, also in JSON or code. Group 1 is the text to
   the end of `scheme`, and the last group is `value`."
  [scheme value]
  (re-pattern (str "(?i)(\\b(?:proxy-)?authorization[\"']?[ \\t]*[:=][ \\t]*[\"']?"
                   scheme
                   "[ \\t]+)(?-i)("
                   value
                   ")")))

(def ^:private authorization-credential
  "The credential after the scheme of an `Authorization` value."
  (authorization-value "(bearer|basic|token|bot)" (str "[" alphanumeric "._~+/=-]{4,}+")))

(def ^:private digest-credential
  "The parameters of a `Digest` value that prove the credential."
  (authorization-value "digest" "[^\\r\\n]*"))

(defn- credential-like?
  "True for a token with a digit, a base64 sign, or an upper case letter after a lower
   case letter, so that a word of prose stays."
  [^String token]
  (let [n (.length token)]
    ;; A loop, because a regex search for each token is slow.
    (loop [i 0
           lower? false]

      (and (< i n)
           (let [c (int (.charAt token i))]
             (or (and (<= 48 c) (<= c 57))
                 (== c 43)
                 (== c 47)
                 (== c 61)
                 (and lower? (<= 65 c) (<= c 90))
                 (recur (inc i) (or lower? (and (<= 97 c) (<= c 122))))))))))

(def ^:private cookie-value
  "A `Cookie` or `Set-Cookie` value up to the end of the line or the quote."
  #"(?i)(\b(?:set-)?cookie[\"']?[ \t]*:[ \t]*[\"']?)(?-i)([^\r\n\"']+)")

(defn- jwt?
  "True when the first part of `token` is a JSON header with an `alg` field."
  [^String token]
  (boolean (some->> (first (str/split token #"\." 2))
                    base64-text
                    (re-find #"\"alg\""))))

(defn- index-of
  "A function that gives the next index of `marker` in a string from an index, or -1."
  [^String marker]
  (fn [^String s from]
    (.indexOf s marker (int from))))

(defn- scheme-char?
  "True when the character with code `c` can be in the scheme of a URL."
  [^long c]
  ;; A comparison of three numbers is a function call, and it is slow in a scan.
  (or (and (<= 97 c) (<= c 122))
      (and (<= 65 c) (<= c 90))
      (and (<= 48 c) (<= c 57))
      (== c 43)
      (== c 45)
      (== c 46)))

(defn- user-part?
  "True when an `@` is in the authority of a URL that starts at `i` in `s`. Only then can
   a user and a password be there."
  [^String s ^long i]
  (let [n (.length s)]
    (loop [j i]
      (when (< j n)
        (let [c (int (.charAt s j))]
          ;; The authority stops at `/`, `?`, `#` or white space.
          (cond (== c 64) true
                (or (== c 47) (== c 63) (== c 35) (== c 32) (and (<= 9 c) (<= c 13))) false
                :else (recur (inc j))))))))

(defn- scheme-start
  "The first index from `from` in `s` where the scheme of a URL with a user part starts,
   or -1."
  [^String s from]
  (loop [p (.indexOf s "://" (int from))]
    (if (neg? p)
      -1
      (let [q (long
                (loop [q p]
                  (if (and (pos? q) (scheme-char? (int (.charAt s (dec q))))) (recur (dec q)) q)))]
        (if (and (< q p) (<= (long from) q) (user-part? s (+ p 3)))
          q
          (recur (.indexOf s "://" (+ p 3))))))))

(defn- word-start
  "A function that gives the first index from an index where `word` starts in a string, or
   where `prefix` starts when it is before the word; else -1. Each letter case matches.
   `word` and `prefix` are in lower case."
  [^String word ^String prefix]
  (let [index
        (literal-index [word] true)

        before
        (.length prefix)]

    (fn [^String s from]
      (let [from
            (long from)

            p
            (next-literal index s from)

            q
            (- p before)]

        (if (and (<= from q) (.regionMatches s true (int q) prefix 0 before)) q p)))))

(def ^:private token-index (literal-index token-markers))

(defn- last-match
  "A function that gives the index where the last match of `pattern` starts in a string,
   or -1. Each match starts with `marker`, and the search goes back from the end."
  [^String marker pattern]
  (fn [^String s]
    (let [m (re-matcher pattern s)]
      (loop [i (.lastIndexOf s marker)]
        (cond (neg? i) -1
              (.lookingAt (.region m i (.length s))) i
              :else (recur (.lastIndexOf s marker (dec i))))))))

(def ^:private handle-form
  "A handle of a Vis secret."
  {:pattern #"vis-secret:[A-Za-z0-9._-]+"
   :start (index-of "vis-secret:")
   :replacement (constantly "[SECRET HANDLE]")})

(def ^:private private-key-form
  "A PEM block of a private key, to its end line."
  {:pattern (re-pattern (str private-key-start "[\\s\\S]*?" private-key-end))
   :start (index-of "-----BEGIN ")
   :end (last-match "-----END " private-key-end)
   :replacement (constantly redacted)})

(def ^:private cut-key-form
  "The start line of a private key with its base64 lines, when the text was cut before
   the end line."
  {:pattern (re-pattern (str private-key-start "(?:\\r?\\n[A-Za-z0-9+/=]{16,})+"))
   :start (index-of "-----BEGIN ")
   :replacement (constantly redacted)})

(def ^:private putty-key-form
  "A PuTTY key file, to the value of its last line."
  {:pattern (re-pattern (str "(?s)PuTTY-User-Key-File-[0-9]+:.*?" putty-key-end))
   :start (index-of "PuTTY-User-Key-File-")
   :end (last-match "Private-MAC:" putty-key-end)
   :replacement (constantly redacted)})

(def ^:private url-form
  {:pattern url-credential
   :start scheme-start
   :replacement (fn [^Matcher m]
                  (str (.group m 1) redacted "@"))})

(def ^:private authorization-form
  {:pattern authorization-credential
   :start (word-start "authorization" "proxy-")
   :replacement (fn [^Matcher m]
                  (let [token (.group m 3)]
                    (when (or (and (<= 8 (count token)) (credential-like? token))
                              (and (.equalsIgnoreCase "basic" (.group m 2))
                                   (basic-credential? token)))
                      (str (.group m 1) redacted))))})

(def ^:private digest-form
  {:pattern digest-credential
   :start (word-start "authorization" "proxy-")
   :replacement (fn [^Matcher m]
                  (str (.group m 1)
                       (str/replace (.group m 2)
                                    #"(?i)\b(response|cnonce|nonce|opaque)=(\"?)[^\",\s]*+"
                                    (str "$1=$2" (Matcher/quoteReplacement redacted)))))})

(def ^:private ^"[Z" cookie-name-chars
  "The characters of the name of a cookie."
  (char-table (str alphanumeric "_.-")))

(def ^:private ^"[Z" cookie-value-ends
  "The characters after the value of a cookie: `;` and white space."
  (char-table "; \t\n\u000B\f\r"))

(defn- redact-cookie-values
  "Replace in the cookie text `value` each value of 8 or more characters. A `name=value`
   pair starts at the start of the text or after a `;` and blanks. This is the regex
   `(^|;[ \\t]*)([\\w.-]+)=[^;\\s]{8,}` as a loop, because a regex search in each header
   is slow."
  [^String value]
  (let [n
        (.length value)

        ^booleans names
        cookie-name-chars

        ^booleans ends
        cookie-value-ends]

    (loop [;; The index where a name can start.
           start
           0

           from
           0

           out
           nil]

      (let [name-end
            (long (loop [i start]
                    (if (and (< i n) (in-table? names value i)) (recur (inc i)) i)))

            value-end
            (long (if (and (< start name-end) (< name-end n) (= \= (.charAt value name-end)))
                    (loop [i (inc name-end)]
                      (if (and (< i n) (not (in-table? ends value i))) (recur (inc i)) i))
                    -1))

            ;; The regex counts code points, and a pair of surrogates is one code point.
            secret?
            (and (<= 8 (- value-end (inc name-end)))
                 (<= 8 (.codePointCount value (int (inc name-end)) (int value-end))))

            next-pair
            (.indexOf value (int \;) (int (if secret? value-end start)))

            out
            (if secret?
              (doto ^StringBuilder (or out (StringBuilder. n))
                (.append value (int from) (int (inc name-end)))
                (.append ^String redacted))
              out)

            from
            (long (if secret? value-end from))]

        (if (neg? next-pair)
          (if out (str (.append ^StringBuilder out value (int from) n)) value)
          (recur (long (loop [i (inc next-pair)]
                         (if (and (< i n) (#{\space \tab} (.charAt value i))) (recur (inc i)) i)))
                 from
                 out))))))

(def ^:private cookie-form
  {:pattern cookie-value
   :start (word-start "cookie" "set-")
   :replacement (fn [^Matcher m]
                  (str (.group m 1) (redact-cookie-values (.group m 2))))})

(def ^:private token-form
  {:pattern token-format
   :start #(next-literal token-index %1 (long %2))
   :replacement (fn [^Matcher m]
                  (let [token (.group m)]
                    (when (or (not (str/starts-with? token "eyJ")) (jwt? token)) redacted)))})

(defn- form-scan
  "The `[next-start span]` pair of `form` on `s` for [[replace-spans]]. It replaces each
   match of the regex `:pattern` with `(replacement matcher)`, or keeps the match when
   that is nil. The regex runs only at some indexes, because a regex scan of each index
   is slow.
   - `(start s from)` gives the first index from `from` where a match can start, or -1.
     It must not miss a start.
   - `(end s)` gives the index where the last end of a match starts. Thus no match
     starts after it, and a try there reads the text to its end."
  [^String s {:keys [pattern start end replacement]}]
  (let [n
        (.length s)

        matcher
        (delay
          (doto (re-matcher pattern s) (.useTransparentBounds true) (.useAnchoringBounds false)))

        limit
        (delay (end s))]

    [(if end
       (fn [s from]
         (let [i (long (start s from))]
           (if (and (<= 0 i) (< i (long @limit))) i -1)))
       start)
     (fn [_ i]
       (let [^Matcher m @matcher]
         (when (.lookingAt (.region m (int i) n)) [(.end m) (replacement m)])))]))

(defn- replace-form
  "Replace in `s` each credential of `form`. See [[form-scan]]."
  [s form]
  ;; Most strings have no start, and the scan of a form costs more than this search.
  (let [start (long ((:start form) s 0))]
    (if (neg? start)
      s
      (let [[next-start span] (form-scan s form)]
        (replace-spans s (next-start s start) next-start span)))))

(defn- replace-first-form
  "Replace in `s` each credential of `forms` in one scan, as one regex of their patterns
   does: the match that starts first, and at one index the first of `forms`."
  [^String s forms]
  (let [scans
        (map #(form-scan s %) forms)

        starts
        (mapv first scans)

        spans
        (mapv second scans)

        ;; The next start of each form, or -1. A form without a later start does not
        ;; read the text again.
        nexts
        (long-array (map #(% s 0) starts))

        next-start
        (fn [s from]
          (let [from (long from)]
            (loop [k 0
                   least -1]

              (if (< k (alength nexts))
                (let [known (aget nexts k)
                      ;; A start before `from` is old.
                      i (long (if (and (<= 0 known) (< known from))
                                (aset nexts k (long ((starts k) s from)))
                                known))]

                  (recur (inc k) (if (and (<= 0 i) (or (neg? least) (< i least))) i least)))
                least))))]

    (replace-spans s
                   (next-start s 0)
                   next-start
                   (fn [s i]
                     (loop [k 0]
                       (when (< k (alength nexts))
                         (or (when (== (long i) (aget nexts k)) ((spans k) s i))
                             (recur (inc k)))))))))

(defn- redact-forms
  "Replace in `s` each credential that its form shows."
  [^String s]
  ;; The words of the last forms are rare, and a search in lower case text is fast.
  (let [lower (.toLowerCase s Locale/ROOT)]
    (cond-> (-> s
                (replace-form handle-form)
                (cond->
                  (or (str/includes? s "PRIVATE KEY-----") (str/includes? s "PuTTY-User-Key-File-"))
                  (replace-first-form [private-key-form cut-key-form putty-key-form]))
                (replace-form url-form))
      (str/includes? lower "authorization")
      (-> (replace-form authorization-form)
          (cond->
            (str/includes? lower "digest")
            (replace-form digest-form)))

      (str/includes? lower "cookie")
      (replace-form cookie-form)

      true
      (replace-form token-form))))

(defn redactor
  "Build the function that replaces, in each string of a value, each of `secrets` and
   each credential that its form shows with `[REDACTED]`. Build it once for an export."
  [secrets]
  (let [known
        (some-> (seq secrets)
                literal-index)

        ;; A base64 run hides a credential when its text holds a known value or a
        ;; credential form. The whole run goes, as the parts of base64 do not split.
        hidden?
        (fn [^String run]
          (when-let [text (when (base64-candidate? run) (base64-text run))]
            (or (and known (<= 0 (next-literal known text 0)))
                (and (printable? text) (not= text (redact-forms text))))))

        text
        (fn [s]
          (redact-forms (replace-base64 (cond->> s
                                          known
                                          (replace-secrets known))
                                        hidden?)))]

    (fn walk [x]
      (cond (string? x) (text x)
            (map? x) (into {}
                           (map (fn [[k v]]
                                  [(walk k) (walk v)]))
                           x)
            (sequential? x) (mapv walk x)
            :else x))))

(defn redact
  "Replace each `secrets` value in each string of `data` with `[REDACTED]`."
  [data secrets]
  ((redactor secrets) data))

(defn- clip-text
  "Keep the first 20,000 Unicode code points and give the number left out."
  [^String text]
  (if (> (.length text) 20000)
    (let [n (long (code-points text))]
      (if (> n 20000)
        (let [end (.offsetByCodePoints text 0 20000)]
          (if (and (<= n 20046)
                   (re-matches #"\n… \[[1-9][0-9]* more characters cut\]" (.substring text end)))
            text
            (str (.substring text 0 end) "\n… [" (- n 20000) " more characters cut]")))
        text))
    text))

(defn- clip-step
  "Clip text in the shared step and its tools before retaining the next step."
  [step]
  (letfn [(clip [value]
            (cond (string? value) (clip-text value)
                  (map? value) (update-vals value clip)
                  (vector? value) (mapv clip value)
                  :else value))]
    (clip step)))

(defn- unique-call-ids
  "Give the calls and their results one sequence of IDs across the whole export."
  [trajectory]
  (let [counter (volatile! 0)]
    (letfn [(step-ids [step]
              (reduce-kv (fn [step i _]
                           (let [id (str "vis-" (vswap! counter #(inc (long %))))]
                             (-> step
                                 (assoc-in [:tool-calls i :tool-call-id] id)
                                 (assoc-in [:observation :results i :source-call-id] id))))
                         step
                         (:tool-calls step)))
            (trajectory-ids [trajectory]
              (cond-> (update trajectory :steps #(mapv step-ids %))
                (:subagent-trajectories trajectory)
                (update :subagent-trajectories #(mapv trajectory-ids %))))]
      (trajectory-ids trajectory))))

(defn saved-json
  "Encode one saved session and its children as a redacted ATIF document.
   `read-turns` gives the turns of one session on demand. Each of `secrets`, by default
   [[run-secrets]], becomes `[REDACTED]`."
  ([root records read-turns] (saved-json root records read-turns (run-secrets)))
  ([root records read-turns secrets]
   (let [root
         (wire/canonical root)

         sid
         (get root "session_id")

         redact-value
         (redactor secrets)

         children
         (children-by-parent sid records)]

     (->> (persisted-trajectory
            {:children children :read-turns read-turns :project-step #(clip-step (redact-value %))}
            #{}
            root)
          redact-value
          unique-call-ids
          wire/json-str))))

(def ^:private owner-only (PosixFilePermissions/fromString "rw-------"))

(defn- private-temp-file
  "A new empty file in `dir` that only its owner can read and write."
  ^Path [^Path dir]
  (try (Files/createTempFile dir
                             ".trajectory-"
                             ".json"
                             (into-array FileAttribute
                                         [(PosixFilePermissions/asFileAttribute owner-only)]))
       (catch UnsupportedOperationException _
         (Files/createTempFile dir ".trajectory-" ".json" (make-array FileAttribute 0)))))

(defn write!
  "Write `document` to `path` as JSON and return `path`. Vis writes a private
   temporary file next to `path`, then moves it there, so only the owner can read
   the trajectory."
  [path document]
  (let [target
        (.toAbsolutePath (.toPath (io/file (paths/expand-home (str path)))))

        dir
        (.getParent target)

        _
        (Files/createDirectories dir (make-array FileAttribute 0))

        tmp
        (private-temp-file dir)]

    (try (Files/writeString tmp (wire/json-str document) (make-array OpenOption 0))
         (Files/move tmp
                     target
                     (into-array CopyOption
                                 [StandardCopyOption/REPLACE_EXISTING
                                  StandardCopyOption/ATOMIC_MOVE]))
         path
         (finally (Files/deleteIfExists tmp)))))

(defn export!
  "Build the trajectory of one run, redact the known credentials and write it to
   `path`. `opts` are the options of [[build]]."
  [path result opts]
  (write! path (redact (build result opts) (run-secrets))))
