(ns com.blockether.vis.loop.runtime.conversation.environment.query.iteration.core
  "Single LLM iteration.

   `run-iteration` calls the LLM via ask!, parses the structured response,
   executes code blocks, validates final answers. Returns a map describing
   everything that happened in one iteration.

   Also contains error-normalization helpers for infrastructure vs recoverable
   failures."
  (:require
   [clojure.string :as str]
   [com.blockether.vis.loop.runtime.conversation.environment.query.iteration.shared :as helpers]
   [com.blockether.vis.loop.runtime.conversation.environment.query.iteration.validate :as validate]
   [com.blockether.vis.loop.runtime.conversation.environment.query.iteration.execute :as execute]

   [com.blockether.vis.loop.runtime.shared :as rt-shared :refer [truncate realize-value format-exception format-exception-short]]
   [com.blockether.vis.loop.storage.schema :as schema
    :refer [ITERATION_SPEC_NON_REASONING ITERATION_SPEC_REASONING *rlm-ctx*]]
   [com.blockether.vis.loop.mustache :as mustache]
   [com.blockether.svar.internal.llm :as llm]
   [edamame.core :as edamame]
   [taoensso.trove :as trove]))

;; ---------------------------------------------------------------------------
;; Error normalization
;; ---------------------------------------------------------------------------

(def ^:private INFRASTRUCTURE_ERROR_TYPES
  #{:svar.llm/all-providers-exhausted
    :svar.llm/circuit-open
    :svar.llm/provider-exhausted})

(defn- infrastructure-error? [ex-data-map]
  (contains? INFRASTRUCTURE_ERROR_TYPES (:type ex-data-map)))

(def ^:private LAST_USER_PREVIEW_CHARS 500)

(defn- last-user-message-preview [messages]
  (when-let [c (some (fn [m] (when (= (:role m) "user") (:content m)))
                 (reverse messages))]
    (let [s (str c)]
      (if (> (count s) LAST_USER_PREVIEW_CHARS)
        (str (subs s 0 LAST_USER_PREVIEW_CHARS)
          " …<+" (- (count s) LAST_USER_PREVIEW_CHARS) " chars>")
        s))))

(defn exception->iter-err
  "Normalize an exception into the iter-err map stored on the query row.
   Delegates to the unified `format-exception` and adds iteration context."
  [^Throwable e ctx]
  (format-exception e
    {:context {:iteration         (:iteration ctx)
               :messages-count    (count (:messages ctx))
               :routing           (:routing ctx)
               :reasoning-level   (:reasoning-level ctx)
               :last-user-preview (last-user-message-preview (:messages ctx))}}))

(defn handle-iteration-exception!
  "Error path for the main-loop try/catch around `run-iteration`.
   Infrastructure failures re-throw; others return `{::iteration-error …}`."
  [^Throwable e ctx]
  (let [ex-data-map (ex-data e)
        iteration (:iteration ctx)]
    (if (infrastructure-error? ex-data-map)
      (do (trove/log! {:level :error
                       :data (assoc (format-exception-short e) :iteration iteration)
                       :msg "Provider infrastructure error — aborting iteration loop"})
        (throw e))
      (let [iter-err (exception->iter-err e ctx)]
        (trove/log! {:level :warn
                     :data (assoc (format-exception-short e) :iteration iteration)
                     :msg "RLM iteration failed, feeding error to LLM"})
        {::iteration-error iter-err}))))

;; ---------------------------------------------------------------------------
;; get-locals (read sandbox vars)
;; ---------------------------------------------------------------------------

(defn get-locals
  "Returns {sym → val} of user-defined vars in SCI sandbox."
  [{:keys [sci-ctx initial-ns-keys]}]
  (try
    (let [sandbox (get-in @(:env sci-ctx) [:namespaces 'sandbox])]
      (persistent!
        (reduce-kv (fn [acc k v]
                     (if (or (contains? initial-ns-keys k) (keyword? k))
                       acc
                       (assoc! acc k (if (instance? clojure.lang.IDeref v) @v v))))
          (transient {}) sandbox)))
    (catch Exception e
      (trove/log! {:level :warn :id ::get-locals-fallback
                   :data {:error (ex-message e)}
                   :msg "Failed to read sandbox locals, returning empty map"})
      {})))

;; ---------------------------------------------------------------------------
;; run-iteration
;; ---------------------------------------------------------------------------

(defn run-iteration
  "Runs a single RLM iteration: ask! → check final → execute code.
   Returns map with :thinking :executions :final-result :api-usage etc."
  [rlm-env messages & [{:keys [iteration-spec routing iteration reasoning-level]
                        :or {iteration-spec ITERATION_SPEC_NON_REASONING}}]]
  (binding [*rlm-ctx* (merge *rlm-ctx* {:rlm-phase :run-iteration})]
    (let [effective-reasoning (when (some? reasoning-level)
                                (or (helpers/normalize-reasoning-level reasoning-level)
                                  (throw (ex-info "Invalid :reasoning-level."
                                           {:type :rlm/invalid-reasoning-level
                                            :got reasoning-level}))))
          ask-result (binding [llm/*log-context* {:query-id (:env-id rlm-env) :iteration iteration}]
                       (llm/ask! (:router rlm-env)
                         (cond-> {:spec iteration-spec
                                  :messages messages
                                  :routing (or routing {})
                                  :check-context? false}
                           effective-reasoning (assoc :reasoning effective-reasoning))))
          parsed (:result ask-result)
          model-reasoning (:reasoning ask-result)
          thinking (or model-reasoning (:thinking parsed))
          _ (helpers/rlm-stage! :llm-response iteration
              {:has-reasoning (some? model-reasoning)
               :has-final (some? (:final parsed))
               :code-count (count (:code parsed))
               :duration-ms (:duration-ms ask-result)
               :tokens (:tokens ask-result)
               :thinking thinking})
          next-hint (:next parsed)
          next-model (when-let [m (:model next-hint)] (keyword m))
          next-reasoning (helpers/normalize-reasoning-level (:reasoning next-hint))
          api-usage {:prompt_tokens (get-in ask-result [:tokens :input] 0)
                     :completion_tokens (get-in ask-result [:tokens :output] 0)
                     :completion_tokens_details {:reasoning_tokens (get-in ask-result [:tokens :reasoning] 0)}
                     :prompt_tokens_details {:cached_tokens (get-in ask-result [:tokens :cached] 0)}}]
      (if-let [raw-final-answer (:answer parsed)]
        ;; FINAL path
        (let [answer-type (some-> (:answer-type parsed) keyword)
              raw-code (or (:code parsed) [])
              code-entries (vec (keep (fn [block]
                                        (when (map? block)
                                          (let [expr (str (:expr block ""))
                                                time-ms (:time-ms block)]
                                            (when-not (str/blank? expr)
                                              {:expr expr :time-ms (or time-ms (throw (ex-info "Code block missing :time-ms" {:expr expr})))}))))
                                  raw-code))
              code-blocks (mapv :expr code-entries)
              exec-results (when (seq code-blocks)
                             (mapv (fn [{:keys [expr time-ms]}]
                                      (if-let [err (validate/literal-code-block-error expr)]
                                        {:result nil :error err
                                         :stdout "" :stderr "" :execution-time-ms 0}
                                        (execute/execute-code rlm-env expr :timeout-ms time-ms)))
                                code-entries))
              exec-errors (when exec-results
                            (seq (clojure.core/filter :error exec-results)))
              raw-answer (str raw-final-answer)
              locals (try (get-locals rlm-env) (catch Throwable _ {}))
              single-token? (and (re-matches #"\S+" raw-answer)
                              (try (symbol? (read-string raw-answer)) (catch Throwable _ false)))
              resolved-var-value (when single-token?
                                   (let [sym (symbol raw-answer)
                                         resolved (get locals sym)]
                                     (when (some? resolved)
                                       (let [v (if (instance? clojure.lang.IDeref resolved)
                                                 @resolved resolved)]
                                         (cond
                                           (string? v) v
                                           :else (pr-str v))))))
              mustache-detected? (and (not resolved-var-value)
                                   (some? answer-type))
              mustache-result (when mustache-detected?
                                (try
                                  (let [result (mustache/render raw-answer locals)]
                                    {:answer (when (not= result raw-answer) result)})
                                  (catch Exception e
                                    {:error (str "Mustache error: " (.getMessage e)
                                              ". Define all referenced vars in :code first.")})))
              template-answer (:answer mustache-result)
              mustache-missing (:error mustache-result)
              raw-answer (or resolved-var-value template-answer raw-answer)
              _ (when resolved-var-value
                  (helpers/rlm-debug! {:token (str raw-final-answer)
                                       :resolved-chars (count resolved-var-value)}
                    "Single-word :answer resolved to var value"))
              _ (when template-answer
                  (helpers/rlm-debug! {:template (str raw-final-answer)
                                       :resolved-chars (count template-answer)}
                    "Mustache template rendered"))
              final-answer raw-answer
              confidence (or (:confidence parsed) :high)
              validation-error (or (when-not answer-type
                                     ":answer-type is required with :answer. Set mustache-text or mustache-markdown.")
                                 (when exec-errors
                                   (str "Code errors before final: " (:error (first exec-errors))))

                                 mustache-missing)
              executions (when exec-results
                           (mapv (fn [idx code result]
                                   {:id idx :code code
                                    :result (:result result) :stdout (:stdout result)
                                    :stderr (:stderr result) :error (:error result)
                                    :execution-time-ms (:execution-time-ms result)
                                    :repaired? (:repaired? result)})
                             (range) code-blocks exec-results))]
          (if validation-error
            (do (helpers/rlm-debug! {:final-answer (truncate final-answer 200)
                                     :validation-error validation-error} "FINAL rejected")
              {:thinking thinking
               :next-model next-model :next-reasoning next-reasoning
               :executions (or executions
                             [{:id 0 :code final-answer :result nil :stdout "" :stderr ""
                               :error validation-error}])
               :final-result nil :api-usage api-usage
               :duration-ms (or (:duration-ms ask-result) 0)})
            (let [sources (vec (or (:sources parsed) []))
                  final-result (cond-> {:final? true
                                        :answer final-answer
                                        :confidence confidence}
                                 (seq sources) (assoc :sources sources)
                                 (:reasoning parsed) (assoc :reasoning (:reasoning parsed)))]
              {:thinking thinking
               :next-model next-model :next-reasoning next-reasoning
               :executions (or executions []) :final-result final-result :api-usage api-usage
               :duration-ms (or (:duration-ms ask-result) 0)})))
        ;; Normal path: execute code blocks
        (let [normalized (vec (:code parsed))
              ;; Coalesce fragments: join unbalanced blocks with the next
              coalesced (loop [remaining normalized
                               result []]
                          (if (empty? remaining)
                            result
                            (let [{:keys [expr time-ms]} (first remaining)]
                              ;; Without form-repair, pass blocks through as-is
                              (recur (rest remaining) (conj result (first remaining))))))
              total-blocks (count coalesced)
              executed (mapv (fn [idx {:keys [expr time-ms]}]
                              (helpers/rlm-stage! :code-exec iteration
                                {:idx (inc idx) :total total-blocks :code expr :time-ms time-ms})
                              (let [result (if-let [err (validate/literal-code-block-error expr)]
                                             {:result nil :error err :stdout "" :stderr "" :execution-time-ms 0}
                                             (let [r (execute/execute-code rlm-env expr :timeout-ms time-ms)]
                                               (helpers/rlm-stage! :code-result iteration
                                                 {:idx (inc idx) :total total-blocks
                                                  :execution-time-ms (:execution-time-ms r)
                                                  :error (:error r) :timeout? (:timeout? r) :result (:result r)})
                                               r))]
                                {:block expr :result result}))
                        (range) coalesced)
              code-blocks (mapv :block executed)
              execution-results (mapv :result executed)
              executions (mapv (fn [idx code result]
                                 {:id idx
                                  :code code
                                  :result (:result result)
                                  :stdout (:stdout result)
                                  :stderr (:stderr result)
                                  :error (:error result)
                                  :execution-time-ms (:execution-time-ms result)
                                  :timeout? (:timeout? result)
                                  :repaired? (:repaired? result)})
                           (range) code-blocks execution-results)]
           {:thinking thinking
            :next-model next-model :next-reasoning next-reasoning
            :executions executions :final-result nil :api-usage api-usage
            :duration-ms (or (:duration-ms ask-result) 0)})))))
