(ns com.blockether.vis.internal.session.atif-import
  "Read ATIF as saved conversation records. The records contain no runtime state."
  (:require [charred.api :as json]
            [clojure.string :as str]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.internal.content :as content]
            [com.blockether.vis.internal.util :as util]
            [taoensso.nippy :as nippy])
  (:import (java.nio.charset StandardCharsets)
           (java.time Instant)
           (java.util.function Supplier)))

(defn- invalid! [reason]
  (throw (ex-info (str "Invalid ATIF: " reason) {:type :session/invalid-atif})))

(defn read-document
  "Read an ATIF JSON body, or answer nil for a binary snapshot."
  [^bytes data]
  (when (some #(and (pos? (alength data)) (= (long %) (aget data 0))) [9 10 13 32 123 91])
    (let [text (str/trim (String. data StandardCharsets/UTF_8))
          document (try (with-open [^java.lang.AutoCloseable reader
                                   (json/read-json-supplier text {:eof-error? false :eof-value ::eof})]
                          (let [value (.get ^Supplier reader)]
                            (when (= ::eof (.get ^Supplier reader)) value)))
                        (catch Exception _ nil))]
      (when-not document (invalid! "the body is not valid JSON."))
      (if (and (map? document) (string? (get document "schema_version"))
               (str/starts-with? (get document "schema_version") "ATIF-"))
        document
        (invalid! "the body must be an object with an ATIF schema_version.")))))

(defn- time-ms [value]
  (when (string? value)
    (try (.toEpochMilli (Instant/parse value)) (catch Exception _ nil))))

(defn- text-content [value]
  (cond (string? value) value
        (nil? value) ""
        (vector? value) (str/join "\n" (keep #(when (map? %) (get % "text")) value))
        :else (wire/json-str value)))

(defn- objects [array field]
  (when-not (or (nil? array) (and (vector? array) (every? map? array)))
    (invalid! (str field " must be an array of objects.")))
  array)

(defn- numeric [metrics path integer-value?]
  (when-let [v (get-in metrics path)]
    (if (and (if integer-value? (int? v) (number? v))
             (<= 0.0 (double v)) (Double/isFinite (double v)))
      (if integer-value? (long v) (double v))
      (invalid! (str (peek path) " must be a non-negative " (if integer-value? "integer." "number."))))))

(defn- token [metrics k] (numeric metrics [k] true))

(defn- warm-record [step now]
  (let [metrics (get step "metrics")]
    {:created-at (or (time-ms (get step "timestamp")) now)
     :model (get step "model_name")
     :provider (get-in step ["extra" "provider"])
     :tokens {"input" (token metrics "prompt_tokens")
              "output" (token metrics "completion_tokens")
              "cached" (token metrics "cached_tokens")
              "cache_created" (numeric metrics ["extra" "cache_creation_tokens"] true)}
     :cost-usd (numeric metrics ["cost_usd"] false)}))

(defn- tool-form [results call]
  (let [id (get call "tool_call_id")
        function-name (get call "function_name")
        arguments (get call "arguments")
        call-extra (get call "extra")
        observations (get results id)
        output (str/join "\n" (keep #(some-> (get % "content") text-content) observations))
        error (or (get call-extra "error") (some #(get-in % ["extra" "error"]) observations))
        timeout? (true? (get call-extra "is_timeout"))
        failure (or error (when timeout? {"message" "The tool call timed out."}))
        code (get arguments "code")]
    (when-not (and (util/non-blank-string? id) (util/non-blank-string? function-name) (map? arguments))
      (invalid! "a tool call needs an id, a function name, and an arguments object."))
    (cond-> {:src (if (string? code) code (str function-name " " (wire/json-str arguments)))
             :tool-name function-name
             :tool-call-id id
             :stdout output
             :atif-output output
             :timeout? timeout?
             :repaired? (true? (get call-extra "is_repaired"))}
      (some? (get call-extra "stdout_chars"))
      (assoc :atif-stdout-chars (numeric call-extra ["stdout_chars"] true))

      (string? (get call-extra "repaired_source"))
      (assoc :repaired-source (get call-extra "repaired_source"))
      failure
      (assoc :error (if (map? failure)
                      {:message (text-content (get failure "message")) :data (get failure "data")}
                      {:message (text-content failure)})))))

(defn- step-forms [step]
  (let [results (group-by #(get % "source_call_id")
                          (objects (get-in step ["observation" "results"]) "results"))]
    (mapv #(tool-form results %) (objects (get step "tool_calls") "tool_calls"))))

(defn- metric-columns [metrics]
  (let [prompt (token metrics "prompt_tokens")
        cached (token metrics "cached_tokens")
        written (numeric metrics ["extra" "cache_creation_tokens"] true)]
    {:input_tokens prompt
     :input_regular_tokens (when prompt (max 0 (- (long prompt) (long (or cached 0)) (long (or written 0)))))
     :input_cache_read_tokens cached
     :input_cache_write_tokens written
     :output_tokens (token metrics "completion_tokens")
     :output_reasoning_tokens (numeric metrics ["extra" "reasoning_tokens"] true)
     :cost_usd (numeric metrics ["cost_usd"] false)}))

(defn- attachment-descriptors [step]
  (mapv #(select-keys % ["filename" "media_type" "size_bytes"])
        (objects (get-in step ["extra" "attachments"]) "attachments")))

(defn- iteration-row [position now {:keys [step warms user-attachments]}]
  (let [extra (get step "extra")
        forms (step-forms step)
        created (or (time-ms (get step "timestamp")) now)]
    (merge (metric-columns (get step "metrics"))
           {:id (str (random-uuid))
            :position position
            :status (if (or (get extra "error") (some :error forms)) "error" "done")
            :llm_actual_provider (when (string? (get extra "provider")) (get extra "provider"))
            :llm_actual_model (when (string? (get step "model_name")) (get step "model_name"))
            :llm_thinking (text-content (get step "reasoning_content"))
            :llm_assistant_prose (text-content (get step "message"))
            :request_health (nippy/freeze {:cache-warms warms :atif-omit-step (nil? step)
                                         :atif-step-error (get extra "error")
                                          :atif-imported-step (some? step)
                                          :atif-final (get extra "is_final")
                                          :atif-attachments (attachment-descriptors step)
                                          :atif-user-attachments user-attachments
                                         :atif-unattributed-cost (true? (get extra "is_unattributed_cost"))})
            :code (str/join "\n" (map :src forms))
            :tool_calls (when (seq forms) (nippy/freeze forms))
            :created_at created
            :finished_at (or (numeric extra ["finished_at_ms"] true) created)})))

(defn- step-groups [steps now]
  (let [{:keys [iterations warms]}
        (reduce (fn [acc step]
                  (if (true? (get-in step ["extra" "is_cache_warm"]))
                    (update acc :warms conj (warm-record step now))
                    (-> acc
                        (update :iterations conj {:step step :warms (:warms acc)})
                        (assoc :warms []))))
                {:iterations [] :warms []}
                steps)]
    (cond-> iterations (seq warms) (conj {:warms warms}))))

(defn- turn-row [{:keys [user steps]} now]
  (let [created (or (time-ms (get user "timestamp")) now)
        attachments (attachment-descriptors user)
        groups (step-groups steps created)
        groups (if (seq attachments) (assoc-in groups [0 :user-attachments] attachments) groups)
        iterations (mapv #(iteration-row (inc (long %1)) created %2) (range) groups)
        answer (get (or (last (filter #(true? (get-in % ["extra" "is_final"])) steps))
                        (last (keep :step groups))) "message")
        tokens (fn [k] (reduce + 0 (keep k iterations)))]
    {:soul {:id (str (random-uuid)) :user_request (text-content (get user "message"))
            :created_at created}
     :state {:status "done" :prior_outcome "complete" :iteration_count (count iterations)
             :input_tokens (tokens :input_tokens) :input_regular_tokens (tokens :input_regular_tokens)
             :input_cache_read_tokens (tokens :input_cache_read_tokens)
             :input_cache_write_tokens (tokens :input_cache_write_tokens)
             :output_tokens (tokens :output_tokens) :output_reasoning_tokens (tokens :output_reasoning_tokens)
             :total_cost_usd (+ (tokens :cost_usd) (reduce + 0 (keep :cost-usd (mapcat :warms groups))))
             :content_json (wire/json-str (if (str/blank? (text-content answer))
                                          [] [(content/prose (text-content answer))]))
             :created_at created}
     :iterations iterations}))

(defn- turns [document now]
  (let [steps (objects (get document "steps") "steps")]
    (when-not (seq steps) (invalid! "steps must not be empty."))
    (->> steps
         (remove #(true? (get % "is_copied_context")))
         (reduce (fn [out step]
                   (case (get step "source")
                     "user" (conj out {:user step :steps []})
                     "system" out
                     "agent" (if (seq out)
                               (update-in out [(dec (count out)) :steps] conj step)
                               [{:steps [step]}])
                     (invalid! "the step source must be user, agent, or system.")))
                 [])
         (mapv #(turn-row % now)))))

(defn- keep-vis-cost
  "Keep the billed cost in an older Vis export that lacks per-call warm costs."
  [document rows children]
  (if-let [total (and (= "vis" (get-in document ["agent" "name"]))
                      (numeric (get document "final_metrics") ["total_cost_usd"] false))]
    (let [calls (reduce + 0.0 (map #(get-in % [:state :total_cost_usd]) rows))
          child-cost (reduce + 0.0 (keep #(numeric (get % "final_metrics") ["total_cost_usd"] false)
                                        children))
          missing (- (double total) (double calls) (double child-cost))]
      (if (and (seq rows) (> missing 1.0e-10))
        (update-in rows [(dec (count rows)) :state :total_cost_usd] + missing)
        rows))
    rows))

(def ^:private child-statuses
  #{"completed" "failed" "cancelled" "budget_limited" "blocked"})

(defn- session-rows [document parent-id leader-id depth now]
  (when (> (long depth) 2) (invalid! "Vis supports up to two levels of subagents."))
  (let [id (str (random-uuid))
        extra (get document "extra")
        agent (get document "agent")
        created (or (time-ms (get extra "started_at"))
                    (time-ms (get-in document ["steps" 0 "timestamp"])) now)
        title (or (when (util/non-blank-string? (get extra "title")) (get extra "title"))
                  (str "ATIF: " (get agent "name")))
        children (objects (get document "subagent_trajectories") "subagent_trajectories")
        rows (keep-vis-cost document (turns document created) children)
        status (get extra "status")]
    (when-not (and (map? agent) (util/non-blank-string? (get agent "name")))
      (invalid! "the agent must have a name."))
    (into [(cond-> {:soul {:id id :channel "tui" :created_at created}
                    :state {:title title}
                     :atif-imported-model (when (string? (get agent "model_name"))
                                            (get agent "model_name"))
                    :parent-id parent-id
                    :turns rows}
             parent-id
             (assoc :agent {:parent_id parent-id :leader_id leader-id :team_id (or (:id (:soul (first rows))) id)
                            :task (or (not-empty (get-in rows [0 :soul :user_request])) title)
                            :title title
                            :status (if (contains? child-statuses status) status "completed")
                            :depth depth :inherited_turns 0 :spawn_key id :spawn_fingerprint id
                            :checkpoint (nippy/freeze []) :created_at created}))]
          (mapcat #(session-rows % id (or leader-id id) (inc depth) created))
          children)))

(defn session-tree
  "Convert an ATIF document to rows for the session tree restorer."
  [document]
  {:sessions (session-rows document nil nil 0 (util/now-ms))})
