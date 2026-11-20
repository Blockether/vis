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
           (java.util Base64)
           (java.util.regex Pattern)))

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
  "`secrets` and their JSON-escaped and URL-encoded forms, longest first, because text
   can hold JSON and URLs."
  [secrets]
  (->> secrets
       (mapcat #(list* % (json-escaped %) (url-encoded %)))
       distinct
       (sort-by count >)
       vec))

(defn- string-leaves
  [data]
  (cond (string? data) [data]
        (map? data) (mapcat string-leaves (vals data))
        (sequential? data) (mapcat string-leaves data)
        :else nil))

(defn- gateway-tokens
  "The bearer token in the default token file of the local gateway."
  []
  (let [file (discovery/default-token-file)]
    (keep not-empty [(when (.isFile file) (try (str/trim (slurp file)) (catch Exception _ nil)))])))

(defn run-secrets
  "The known credentials of this process and of the bound workspace, also JSON-escaped:
   also the gateway token and the provider keys that `api_key_command` gave."
  []
  (with-escaped (known-secrets
                  {:env (System/getenv)
                   :files (credential-files)
                   :providers (try (:providers (config/current-config)) (catch Exception _ nil))
                   :settings (concat (toggles/secret-values) (cred/cached-tokens) (gateway-tokens))
                   :environment (environment)})))

(defn setting-secrets
  "The values of the secret settings in `settings`, `{setting-id value}`, also JSON-escaped."
  [settings]
  (with-escaped (known-secrets {:settings (for [[id value]
                                                settings

                                                :when (toggles/secret-setting? id)
                                                leaf
                                                (string-leaves value)]

                                            leaf)})))

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
  "A string without each of these has no match of [[token-format]]."
  ["sk-" "sk_" "rk_" "ghp_" "gho_" "ghu_" "ghs_" "ghr_" "github_pat_" "glpat-" "gldt-" "glptt-"
   "glrt-" "xox" "AKIA" "ASIA" "AIza" "npm_" "hf_" "eyJ"])

(def ^:private private-key-format
  "A private key: a PEM block with its end line, or with base64 lines after its start
   line when the text was cut, and a PuTTY key file."
  (re-pattern
    (str "-----BEGIN (?:[A-Z0-9]+ )*PRIVATE KEY-----"
         "(?:[\\s\\S]*?-----END (?:[A-Z0-9]+ )*PRIVATE KEY-----|(?:\\r?\\n[A-Za-z0-9+/=]{16,})+)"
         "|(?s)PuTTY-User-Key-File-[0-9]+:.*?Private-MAC:[ \\t]*[0-9a-fA-F]+")))

(def ^:private url-credential
  "The user and password in the authority of a URL."
  #"(?i)(?<![\w+.-])([a-z][a-z0-9+.-]*+://)[^\s/?#@:]++:[^\s/?#@]++@")

(defn- base64-text
  "The UTF-8 text that the base64 or base64url `run` encodes, or nil."
  [^String run]
  (let [body (cond-> run
               (or (str/includes? run "=") (str/includes? run "\n"))
               (str/replace #"=+$|\r|\n" ""))]
    (when-not (= 1 (mod (count body) 4))
      (try (String. (.decode (if (or (str/includes? body "_") (str/includes? body "-"))
                               (Base64/getUrlDecoder)
                               (Base64/getDecoder))
                             ^String body)
                    StandardCharsets/UTF_8)
           (catch IllegalArgumentException _ nil)))))

(defn- printable?
  "True for text without control characters and without bytes that are not UTF-8."
  [^String s]
  (not (re-find #"[\x00-\x08\x0E-\x1F\x7F\uFFFD]" s)))

(defn- basic-credential?
  "True when `token` is the base64 of a `user:password` pair."
  [^String token]
  (boolean (when-let [text (base64-text token)]
             (and (printable? text) (str/includes? text ":")))))

(def ^:private base64-run
  "A run of base64 or base64url text, also wrapped in lines of 64 or 76 characters."
  #"(?<![A-Za-z0-9+/_-])(?:(?:(?:[A-Za-z0-9+/]{76}|[A-Za-z0-9+/]{64})\r?\n)++[A-Za-z0-9+/]*+|[A-Za-z0-9+/_-]{11,}+)={0,2}(?![A-Za-z0-9+/_=-])")

(def ^:private ^"[Z" base64-chars
  "True at the code of each base64 and base64url character."
  (let [table (boolean-array 128)]
    (doseq [c "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/_-"]
      (aset table (int c) true))
    table))

(defn- base64-end
  "The index after the base64 characters that start at `i` in `s`."
  ^long [^String s ^long i]
  (let [n
        (.length s)

        ^booleans table
        base64-chars]

    (loop [j i]
      (if (< j n)
        (let [c (int (.charAt s j))]
          (if (and (< c 128) (aget table c)) (recur (inc j)) j))
        j))))

(defn- replace-base64
  "Replace in `s` each [[base64-run]] for which `hidden?` is true. A loop over the
   characters finds the runs, because a regex scan of each position is slow."
  [^String s hidden?]
  (let [n
        (.length s)

        m
        (doto (re-matcher base64-run s) (.useTransparentBounds true) (.useAnchoringBounds false))]

    (loop [i
           0

           from
           0

           out
           nil]

      (if (>= i n)
        (if out (str (.append ^StringBuilder out s (int from) (int n))) s)
        (let [end
              (base64-end s i)

              len
              (- end i)]

          (if (< len 11)
            (recur (max end (inc i)) from out)
            ;; Only a line of 64 or 76 characters can start wrapped base64.
            (let [e (if (and (or (= len 64) (= len 76))
                             (< end n)
                             (#{\return \newline} (.charAt s end)))
                      (when (.lookingAt (.region m (int i) (int n))) (.end m))
                      (let [e (loop [e end]
                                (if (and (< e n) (< (- e end) 2) (= \= (.charAt s e)))
                                  (recur (inc e))
                                  e))]
                        (when-not (and (< e n) (or (= \= (.charAt s e)) (< e (base64-end s e))))
                          e)))]
              (cond (nil? e) (recur end from out)
                    (hidden? (subs s i e)) (recur (long e)
                                                  (long e)
                                                  (doto ^StringBuilder (or out (StringBuilder. n))
                                                    (.append s (int from) (int i))
                                                    (.append ^String redacted)))
                    :else (recur (long e) from out)))))))))

(defn- base64-candidate?
  "True for a run long enough to hold a credential of 8 bytes, with a digit or with both
   letter cases."
  [^String run]
  (and (<= 11 (.length run))
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
           false))))

(def ^:private authorization-credential
  "The credential after the scheme of an `Authorization` value, also in JSON or code."
  #"(?i)(\b(?:proxy-)?authorization[\"']?[ \t]*[:=][ \t]*[\"']?(bearer|basic|token|bot)[ \t]+)([A-Za-z0-9._~+/=-]{4,}+)")

(def ^:private digest-credential
  "The parameters of a `Digest` value that prove the credential."
  #"(?i)(\b(?:proxy-)?authorization[\"']?[ \t]*[:=][ \t]*[\"']?digest[ \t]+)([^\r\n]*)")

(defn- credential-like?
  "True for a token with a digit, a base64 sign, or an upper case letter after a lower
   case letter, so that a word of prose stays."
  [^String token]
  (or (boolean (re-find #"[0-9+/=]" token))
      (let [m (re-matcher #"[a-z]" token)]
        (and (.find m) (boolean (re-find #"[A-Z]" (subs token (.start m))))))))

(def ^:private cookie-value
  "A `Cookie` or `Set-Cookie` value up to the end of the line or the quote."
  #"(?i)(\b(?:set-)?cookie[\"']?[ \t]*:[ \t]*[\"']?)([^\r\n\"']+)")

(defn- jwt?
  "True when the first part of `token` is a JSON header with an `alg` field."
  [^String token]
  (boolean (some->> (first (str/split token #"\." 2))
                    base64-text
                    (re-find #"\"alg\""))))

(defn- redact-forms
  "Replace in `s` each credential that its form shows. Each pattern runs only when its
   marker occurs, because a scan for a marker is much faster."
  [^String s]
  (let [lower (str/lower-case s)]
    (cond-> s
      (str/includes? s "vis-secret:")
      (str/replace #"vis-secret:[A-Za-z0-9._-]+" "[SECRET HANDLE]")

      (or (str/includes? s "PRIVATE KEY-----") (str/includes? s "PuTTY-User-Key-File-"))
      (str/replace private-key-format redacted)

      (str/includes? s "://")
      (str/replace url-credential
                   (fn [[_ scheme]]
                     (str scheme redacted "@")))

      (str/includes? lower "authorization")
      (-> (str/replace authorization-credential
                       (fn [[match prefix scheme token]]
                         (if (or (and (<= 8 (count token)) (credential-like? token))
                                 (and (= "basic" (str/lower-case scheme))
                                      (basic-credential? token)))
                           (str prefix redacted)
                           match)))
          (str/replace digest-credential
                       (fn [[_ prefix params]]
                         (str prefix
                              (str/replace params
                                           #"(?i)\b(response|cnonce|nonce|opaque)=(\"?)[^\",\s]*+"
                                           (fn [[_ k q]]
                                             (str k "=" q redacted)))))))

      (str/includes? lower "cookie")
      (str/replace cookie-value
                   (fn [[_ prefix value]]
                     (str prefix
                          (str/replace value
                                       #"(^|;[ \t]*)([\w.-]+)=([^;\s]{8,})"
                                       (fn [[_ start name]]
                                         (str start name "=" redacted))))))

      (some #(str/includes? s %) token-markers)
      (str/replace token-format #(if (or (not (str/starts-with? % "eyJ")) (jwt? %)) redacted %)))))

(defn- pair-code
  "A code of the two characters at `i` in `s`, from 0 to 65535."
  ^long [^String s ^long i]
  (bit-or (bit-shift-left (bit-and (int (.charAt s i)) 0xFF) 8)
          (bit-and (int (.charAt s (inc i))) 0xFF)))

(defn- secret-index
  "`secrets` in a table by the [[pair-code]] of their start, longest first. Nil when a
   secret has less than two characters."
  [secrets]
  (when (every? #(<= 2 (count %)) secrets)
    (let [table (object-array 65536)]
      (doseq [^String secret (sort-by count > (distinct secrets))]
        (let [k (pair-code secret 0)]
          (aset table k (conj (or (aget table k) []) secret))))
      table)))

(defn- secret-at
  "The longest secret of `table` that starts at `i` in `s`, or nil."
  [^objects table ^String s ^long i]
  (when (< (inc i) (.length s))
    (when-let [secrets (aget table (pair-code s i))]
      (some (fn [^String secret]
              (when (.startsWith s secret (int i)) secret))
            secrets))))

(defn- replace-secrets
  "Replace in `s` each secret of `table`, from the left, the longest first. This is one
   scan of `s`, while a regex of all secrets tries each secret at each position."
  [^objects table ^String s]
  (let [n
        (.length s)

        last
        (dec n)]

    (loop [i
           0

           from
           0

           out
           nil]

      (if (>= i last)
        (if out (str (.append ^StringBuilder out s (int from) (int n))) s)
        (if-let [^String secret (when (aget table
                                            (bit-or
                                              (bit-shift-left (bit-and (int (.charAt s i)) 0xFF) 8)
                                              (bit-and (int (.charAt s (inc i))) 0xFF)))
                                  (secret-at table s i))]
          (let [e (+ i (.length secret))]
            (recur e
                   e
                   (doto ^StringBuilder (or out (StringBuilder. n))
                     (.append s (int from) (int i))
                     (.append ^String redacted))))
          (recur (inc i) from out))))))

(defn- has-secret?
  "True when `s` holds a secret of `table`."
  [^objects table ^String s]
  (loop [i 0]
    (cond (>= i (.length s)) false
          (secret-at table s i) true
          :else (recur (inc i)))))

(defn redactor
  "Compile the credential pattern once for all values in an export. Each of `secrets`,
   and each credential that its form shows, becomes `[REDACTED]`."
  [secrets]
  (let [index
        (when (seq secrets) (secret-index secrets))

        known
        (when (and (seq secrets) (not index))
          (re-pattern (str/join "|" (map #(Pattern/quote %) (sort-by count > secrets)))))

        replace-known
        (cond index #(replace-secrets index %)
              known #(str/replace % known redacted)
              :else identity)

        known?
        (cond index #(has-secret? index %)
              known #(boolean (re-find known %))
              :else (constantly false))

        ;; A base64 run hides a credential when its text holds a known value or a
        ;; credential form. The whole run goes, as the parts of base64 do not split.
        hidden?
        (fn [^String run]
          (when-let [text (when (base64-candidate? run) (base64-text run))]
            (or (known? text) (and (printable? text) (not= text (redact-forms text))))))

        text
        (fn [s]
          (redact-forms (replace-base64 (replace-known s) hidden?)))]

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
