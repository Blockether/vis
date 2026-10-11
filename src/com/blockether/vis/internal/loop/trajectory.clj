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
   field go to `extra`: iteration, block status, Activity counts, repairs,
   timeouts and the run profile. The run result has no token counts for each
   iteration, so the steps of the run have no `metrics`.

   A Council child comes from its persisted turns, so its steps have the token
   counts and the cost of each iteration. The observation of the step that
   started the child refers to the child trajectory. The total cost of the run
   includes the cost of the children.

   The CLI result has two shapes: engine keywords, or the snake_case string keys
   of a `--persist` run through the gateway. [[build]] reads the canonical wire
   shape of both.

   Before the write, each known credential value becomes `[REDACTED]`: the
   provider keys and tokens in `~/.vis/*-auth.json` and in the provider
   configuration, and environment values whose names contain KEY, TOKEN,
   SECRET, PASSWORD or CREDENTIAL. Vis replaces only these known values. It
   does not find other secrets in the text. Only the owner can read the file."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.internal.config.core :as config]
            [com.blockether.vis.internal.content :as content]
            [com.blockether.vis.internal.error :as error]
            [com.blockether.vis.internal.gateway.runtime :as runtime]
            [com.blockether.vis.internal.loop.transcript :as transcript]
            [com.blockether.vis.internal.paths :as paths]
            [com.blockether.vis.internal.util :as util])
  (:import (java.io File)
           (java.nio.file CopyOption Files OpenOption Path StandardCopyOption)
           (java.nio.file.attribute FileAttribute PosixFilePermissions)
           (java.time Instant)
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
  (->> [(get block "stdout") (error-text (get block "error"))]
       (remove str/blank?)
       (str/join "\n")))

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

(defn- tool-call
  "The ATIF tool call and observation result of one code block. A persisted form
   of a Council child has its code in `src`."
  [call-id block]
  (let [envelope (get block "envelope")]
    {:call {:tool-call-id call-id
            :function-name (or (get block "tool_name") "python_execution")
            :arguments {"code" (str (or (get block "code") (get block "src")))}
            :extra (compact {:status (get envelope "status")
                             :ref (get envelope "ref")
                             :started-at-ms (get envelope "started_at_ms")
                             :finished-at-ms (get envelope "finished_at_ms")
                             :is-repaired (boolean (or (get envelope "is_repaired")
                                                       (get block "is_repaired")))
                             :is-timeout (boolean (or (get envelope "is_timeout")
                                                      (get block "is_timeout")))
                             :activity (get-in block ["activity" "counts"])
                             :repaired-source (get block "repaired_source")})}
     :result {:source-call-id call-id :content (observation-text block)}}))

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
        (mapv tool-call (call-ids iteration blocks) blocks)

        started
        (get-in (first blocks) ["envelope" "started_at_ms"])

        {:keys [model provider]}
        (model-ref entry)

        effort
        (get-in entry ["reasoning_effort" "effective"])

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

(defn- iteration-step
  "The ATIF agent step of one persisted iteration of a Council child. The final
   iteration of a turn carries `answer`."
  [step-id iteration answer]
  (let [position
        (get iteration "position")

        forms
        (get iteration "forms")

        calls
        (mapv tool-call (call-ids position forms) forms)

        created
        (epoch-ms (get iteration "created_at"))

        model
        (text-value (or (get iteration "llm_actual_model") (get iteration "model")))

        metrics
        (compact {:prompt-tokens (get iteration "input_tokens")
                  :completion-tokens (get iteration "output_tokens")
                  :cached-tokens (get iteration "input_cache_read_tokens")
                  :cost-usd (some-> (get iteration "cost_usd")
                                    double)})

        thinking
        (get iteration "thinking")]

    (cond-> {:step-id step-id
             :source "agent"
             :message (or answer (get iteration "assistant_prose") "")
             :extra (compact {:iteration position
                              :provider (text-value (or (get iteration "llm_actual_provider")
                                                        (get iteration "provider")))
                              :finished-at-ms (or (epoch-ms (get iteration "finished_at")) created)
                              :error (error-text (get iteration "error"))})}
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

(defn- turn-steps
  "The ATIF steps of one turn of a Council child, from step `first-id`: the
   request, then one agent step for each persisted iteration."
  [first-id turn]
  (let [answer
        (or (not-empty (content/text-projection (vec (get turn "content"))))
            (text-value (get turn "answer")))

        iterations
        (vec (or (seq (get turn "trace")) (when answer [{}])))

        created
        (epoch-ms (get turn "created_at"))]

    (into [(cond-> {:step-id first-id :source "user" :message (request-text (get turn "request"))}
             created
             (assoc :timestamp (iso-time created)))]
          (map-indexed (fn [i iteration]
                         (iteration-step (+ first-id 1 i)
                                         iteration
                                         (when (= i (dec (count iterations))) answer))))
          iterations)))

(defn- child-steps
  "The ATIF steps of each turn of one Council child. A child without a turn has
   only its task."
  [record]
  (or (not-empty (reduce (fn [steps turn]
                           (into steps (turn-steps (inc (count steps)) turn)))
                         []
                         (get record "turns")))
      [{:step-id 1 :source "user" :message (request-text (get record "task"))}]))

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
                                (when (= "agent" (:source step)) i))
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
        (remove nil? [(total ["cost" "total_cost"] :cost-usd) (subagent-cost trajectory)])]

    (compact {:total-prompt-tokens (total ["tokens" "input"] :prompt-tokens)
              :total-completion-tokens (total ["tokens" "output"] :completion-tokens)
              :total-cached-tokens (total ["tokens" "cached"] :cached-tokens)
              :total-cost-usd (when (seq costs) (double (reduce + costs)))
              :total-steps (count (:steps trajectory))})))

(defn- subagent-trajectory
  "The embedded ATIF trajectory of one Council child record. `children` maps a
   parent session id to its child records, so a child also embeds its own
   children. `seen` holds the session ids above the child, so a bad parent link
   cannot loop."
  [children seen record]
  (let [sid
        (str (get record "session_id"))

        seen
        (conj seen sid)

        steps
        (child-steps record)

        provider
        (or (text-value (get record "provider")) (some #(get-in % [:extra :provider]) steps))

        trajectory
        (with-subagents
          {:schema-version schema-version
           :session-id sid
           :trajectory-id sid
           :agent (compact
                    {:name "vis"
                     :version (runtime/release-version)
                     :model-name (or (text-value (get record "model")) (some :model-name steps))
                     :extra (not-empty (compact {:provider provider
                                                 :parent-session-id (some-> (get record "parent_id")
                                                                            str)
                                                 :depth (get record "depth")}))})
           :steps steps
           :extra (not-empty (compact {:status (text-value (get record "status"))}))}
          (remove #(contains? seen (str (get % "session_id"))) (get children sid))
          #(subagent-trajectory children seen %))]

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
        (transcript/model-facing-tools nil)))

(defn build
  "ATIF document of one finished run. `result` is the CLI result map. Options:

     :prompt         the user prompt
     :started-at-ms  the time when the run started
     :profile        the run options that shape the agent
     :subagents      the Council children of the run: agent rows with their
                     `turns`, and each turn with its persisted iterations in
                     `trace`"
  [result {:keys [prompt started-at-ms profile subagents]}]
  (let [run
        (wire/canonical (select-keys result
                                     [:session-id :content :trace :iteration-count :duration-ms
                                      :tokens :cost :status :eval]))

        answer
        (not-empty (content/text-projection (vec (get run "content"))))

        steps
        (into [(cond-> {:step-id 1 :source "user" :message (str prompt)}
                 started-at-ms
                 (assoc :timestamp (iso-time started-at-ms)))]
              (map-indexed (fn [i entry]
                             (agent-step (+ 2 i) entry answer)))
              (get run "trace"))

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
                                    :eval (not-empty (select-keys (get run "eval")
                                                                  ["is_valid"
                                                                   "invalid_reasons"]))})}
            root
            (assoc :session-id root))
          (get children (or root ""))
          #(subagent-trajectory children #{(or root "")} %))]

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

   Values with fewer than 8 characters do not count."
  [{:keys [env files providers]}]
  (->> (concat (keep (fn [[k v]]
                       (when (re-find secret-env-name (str k)) v))
                     env)
               (mapcat (comp secret-leaves read-json-file) files)
               (secret-leaves providers))
       (filter #(and (string? %) (<= (long min-secret-length) (count %))))
       distinct
       (sort-by count >)
       vec))

(defn- credential-files
  "The provider credential files `~/.vis/<vendor>-auth.json`."
  []
  (filter (fn [^File f]
            (and (.isFile f) (str/ends-with? (.getName f) "-auth.json")))
          (.listFiles (io/file (paths/expand-home "~/.vis")))))

(defn- run-secrets
  "The known credentials of this process."
  []
  (known-secrets {:env (System/getenv)
                  :files (credential-files)
                  :providers (try (:providers (config/current-config)) (catch Exception _ nil))}))

(defn redact
  "Replace each `secrets` value in each string of `data` with `[REDACTED]`."
  [data secrets]
  (if (empty? secrets)
    data
    (let [pattern
          (re-pattern (str/join "|" (map #(Pattern/quote %) (sort-by count > secrets))))

          walk
          (fn walk [x]
            (cond (string? x) (str/replace x pattern redacted)
                  (map? x) (into {}
                                 (map (fn [[k v]]
                                        [(walk k) (walk v)]))
                                 x)
                  (sequential? x) (mapv walk x)
                  :else x))]

      (walk data))))

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
