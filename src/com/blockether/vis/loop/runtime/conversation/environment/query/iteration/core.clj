(ns com.blockether.vis.loop.runtime.conversation.environment.query.iteration.core
  "Single LLM iteration.

   `run-iteration` calls the LLM via ask!, parses the structured response,
   executes code blocks, validates final answers. Returns a map describing
   everything that happened in one iteration.

   Also contains error-normalization helpers for infrastructure vs recoverable
   failures."
  (:require
   [clojure.string :as str]
   [com.blockether.vis.loop.runtime.conversation.environment.core :as sci-env]

   [com.blockether.vis.persistance.core :as db]
   [com.blockether.vis.persistance.spec :as rlm-spec
    :refer [ITERATION_SPEC_NON_REASONING ITERATION_SPEC_REASONING
            *eval-timeout-ms* *rlm-ctx* clamp-eval-timeout-ms]]
   [com.blockether.vis.loop.mustache :as mustache]
   [com.blockether.svar.internal.llm :as llm]
   [com.blockether.svar.internal.router :as router]
   [edamame.core :as edamame]
   [sci.core :as sci]
   [taoensso.telemere :as tel]))

;; ---------------------------------------------------------------------------
;; Core helpers
;; ---------------------------------------------------------------------------

(def ^:const MAX_RESULT_DISPLAY_CHARS 30000)

(defn- truncate [s n]
  (let [s (str s)] (if (> (count s) n) (subs s 0 n) s)))

(defn- strip-sandbox-ns [s]
  (-> (str s) (str/replace "sandbox/" "")))

(defn- realize-value [v]
  (cond
    (instance? clojure.lang.IDeref v) @v
    (map? v) (into {} (map (fn [[k vv]] [k (realize-value vv)])) v)
    (vector? v) (mapv realize-value v)
    (set? v) (set (map realize-value v))
    (sequential? v) (doall (map realize-value v))
    :else v))

(defn- format-exception-short [^Throwable t]
  {:class (.getName (class t))
   :message (or (ex-message t) (str t))})

(defn- format-exception [^Throwable t & [{:keys [context]}]]
  (merge (format-exception-short t)
    {:data (ex-data t) :context context}))

;; ---------------------------------------------------------------------------

(defn log-stage!
  [stage iteration data]
  (tel/log! {:level :info :data (merge {:stage stage :iteration iteration} data)}))

(defn normalize-reasoning-level [v]
  (router/normalize-reasoning-level v))

(defn reasoning-level-for-errors [base consecutive-errors]
  (cond
    (<= (long consecutive-errors) 0) base
    (= 1 (long consecutive-errors)) (if (= base :quick) :balanced :deep)
    :else :deep))

(defn answer-str [answer]
  (let [v (:result answer answer)]
    (if (string? v) v (str v))))

(def edamame-opts
  {:all true
   :readers (fn [_tag] (fn [val] (list 'do val)))})

(defn- check-syntax [code]
  (edamame/parse-string-all code edamame-opts))

(defn- check-bare-list [forms]
  (let [first-form (first forms)]
    (when (and (= 1 (count forms))
            (list? first-form) (seq first-form)
            (let [head (first first-form)]
              (not (or (symbol? head) (keyword? head)
                     (list? head) (set? head) (map? head) (vector? head)))))
      (str "Bare list literal: " (pr-str first-form)
        ". Quote it: '(" (str/join " " first-form) ")"))))

(defn- parse-clojure-syntax [code]
  (try
    (let [forms (check-syntax code)]
      (or (check-bare-list forms)
        nil))
    (catch Throwable e
      (ex-message e))))

(def ^:private BARE_STRING_RE #"^\s*\"[^\"]*\"\s*$")

(defn- bare-string-code-block? [expr]
  (boolean (re-matches BARE_STRING_RE (str expr))))

(defn- comment-only-block? [^String expr]
  (try
    (zero? (count (edamame/parse-string-all (str/trim expr) edamame-opts)))
    (catch Throwable _ false)))

(defn- literal-code-block-error [expr]
  (cond
    (bare-string-code-block? expr)
    "Bare string literal in :code. Prose belongs in :answer with answer-type text, not in :code."

    (comment-only-block? expr)
    "Code block contains only comments / discards (`;;` or `#_`) and no executable form. Add an expression to evaluate, or drop the block entirely."))

(defn- detect-common-mistakes [code]
  (let [s (str/trim code)]
    (cond
      (re-find #"#\([^)]*#\(" s)
      "Nested #() is illegal in Clojure. Rewrite inner #() as (fn [...] ...)"
      :else nil)))

(defn- run-sci-code [sci-ctx code & {:keys [sandbox-ns]}]
  (let [stdout-writer (java.io.StringWriter.)
        stderr-writer (java.io.StringWriter.)
        err-pw       (java.io.PrintWriter. stderr-writer true)
        exec-future (future
                      (try
                        (let [result (sci/binding [sci/out stdout-writer
                                                   sci/err err-pw]
                                       (let [ns (or (sci/find-ns sci-ctx 'sandbox) sandbox-ns)]
                                         (:val (sci/eval-string+ sci-ctx code
                                                 (when ns {:ns ns})))))]
                          {:result result :stdout (str stdout-writer) :stderr (str stderr-writer) :error nil})
                        (catch Throwable e
                          {:result nil :stdout (str stdout-writer) :stderr (str stderr-writer)
                           :error (str (.getSimpleName (class e)) ": " (or (ex-message e) (str e)))})))
        timeout-ms (long *eval-timeout-ms*)
        execution-result (try
                           (deref exec-future timeout-ms nil)
                           (catch Throwable e
                             {:result nil :stdout "" :stderr "" :error (str (.getSimpleName (class e)) ": " (ex-message e))}))]
    (.close stdout-writer)
    (.close stderr-writer)
    (if (nil? execution-result)
      (do (future-cancel exec-future)
        {:result nil :stdout "" :stderr "" :error (str "Timeout (" (/ timeout-ms 1000) "s)") :timeout? true})
      execution-result)))

(defn- execute-code [{:keys [sci-ctx sandbox-ns]} code & {:keys [timeout-ms]}]
  (binding [*rlm-ctx* (merge *rlm-ctx* {:rlm-phase :execute-code})]
    (let [start-time (System/currentTimeMillis)
          lint-error (detect-common-mistakes code)]
      (if lint-error
        {:result nil :stdout "" :stderr "" :error lint-error
         :execution-time-ms 0 :timeout? false}
        (if-let [parse-error (parse-clojure-syntax code)]
          {:result nil :stdout "" :stderr "" :error parse-error
           :execution-time-ms 0 :timeout? false}
          (let [execution-result (if timeout-ms
                                   (binding [*eval-timeout-ms* (clamp-eval-timeout-ms timeout-ms)]
                                     (run-sci-code sci-ctx code :sandbox-ns sandbox-ns))
                                   (run-sci-code sci-ctx code :sandbox-ns sandbox-ns))
                execution-time (- (System/currentTimeMillis) start-time)]
            (if (:timeout? execution-result)
              (assoc execution-result :execution-time-ms execution-time :timeout? true)
              (assoc execution-result :execution-time-ms execution-time :timeout? false))))))))

(def ^:const SLOW_EXECUTION_MS 5000)
(def ^:const EXECUTION_SAFETY_CAP_CHARS MAX_RESULT_DISPLAY_CHARS)
(def ^:const EXECUTION_STDERR_CHARS 2000)
(def ^:const HANDOVER_KEEP_LAST 2)
(def ^:const PRIOR_THINKING_MAX_CHARS 4000)

(def PRIOR_THINKING_BREADCRUMB
  "[older reasonings] call `(var-history '*reasoning*)` from :code (oldest first; `take-last N` for a window).")

(defn- truncated-pr-str [v]
  (let [s (strip-sandbox-ns (pr-str v))]
    (if (> (count s) EXECUTION_SAFETY_CAP_CHARS)
      [(truncate s EXECUTION_SAFETY_CAP_CHARS) true]
      [s false])))

(defn- format-expression-results [expressions _iteration]
  (when (seq expressions)
    (str "<journal>\n"
      (str/join "\n"
        (map-indexed
          (fn [idx {:keys [code error result stdout stderr execution-time-ms]}]
            (let [code-str      (str/trim (or code ""))
                  stdout-suffix (when-not (str/blank? stdout)
                                  (str " :stdout " (pr-str stdout)))
                  stderr-suffix (when-not (str/blank? stderr)
                                  (str " :stderr " (pr-str (truncate stderr EXECUTION_STDERR_CHARS))))
                  time-ms       (or execution-time-ms 0)
                  slow-suffix   (when (> time-ms SLOW_EXECUTION_MS)
                                  (str " (" time-ms "ms SLOW)"))
                  value-part (if error
                               (str "ERROR: " (truncate error 400))
                               (let [v (realize-value result)
                                     [value-str truncated?] (truncated-pr-str v)]
                                 (str value-str
                                   (when truncated? " :truncated? true"))))]
              (str "  [" (inc idx) "] " code-str " → " value-part
                (or slow-suffix "")
                (or stdout-suffix "")
                (or stderr-suffix ""))))
          expressions))
      "\n</journal>")))

(defn- format-prior-thinking-chain [iterations]
  (let [entries (->> iterations
                  (keep (fn [{:keys [iteration thinking]}]
                          (when (and (string? thinking)
                                  (not (str/blank? thinking)))
                            (str "[iter " iteration "] " (str/trim thinking))))))]
    (when (seq entries)
      (str/join "\n\n" entries))))

(defn- format-prior-turn-handover [{:keys [iterations final-answer]}]
  (let [kept (take-last HANDOVER_KEEP_LAST iterations)]
    (when (seq kept)
      (let [thinking-lines (->> kept
                             (keep (fn [{:keys [iteration thinking]}]
                                     (when (and (string? thinking)
                                             (not (str/blank? thinking)))
                                       (str "  [iter " iteration "] "
                                         (str/trim thinking))))))
            final-line (when (and (string? final-answer)
                               (not (str/blank? final-answer)))
                         (str "  [final answer] " (str/trim final-answer)))]
        (str/join "\n"
          (concat ["[prior turn]"]
            thinking-lines
            (when final-line [final-line])
            ["[new query]"]))))))

(defn assemble-initial-messages [{:keys [system-prompt initial-user-content history-messages]}]
  (into [{:role "system" :content system-prompt}
         {:role "user"   :content initial-user-content}]
    (or history-messages [])))

(defn trim-to-initial-history [messages initial-count]
  (vec (take initial-count messages)))

(defn- read-var-index-str [rlm-env]
  (let [var-index-atom (or (:var-index-atom rlm-env)
                         (atom {:index nil :revision -1 :current-revision 0}))
        {:keys [index revision current-revision]} @var-index-atom]
    (if (= revision current-revision)
      index
      (let [sandbox-map (get-in @(:env (:sci-ctx rlm-env))
                          [:namespaces 'sandbox])
            idx         (sci-env/build-var-index
                          (:sci-ctx rlm-env) (:initial-ns-keys rlm-env)
                          sandbox-map
                          (:db-info rlm-env) (:conversation-id rlm-env)
                          nil)
            live-rev    (:current-revision @var-index-atom)]
        (swap! var-index-atom assoc :index idx :revision live-rev)
        idx))))

(defn- load-prior-thinking-chain [db-info query-id]
  (try
    (if query-id
      (let [iters (db/db-list-query-iterations db-info query-id)]
        (vec (keep-indexed
               (fn [idx it]
                 (when-let [t (:thinking it)]
                   {:iteration idx :thinking t}))
               iters)))
      [])
    (catch Throwable t
      (tel/log! {:level :warn
                 :data {:error (ex-message t) :query-id query-id}
                 :msg "load-prior-thinking-chain failed"})
      [])))

(defn build-prior-thinking [_rlm-env db-info query-id]
  (let [chain (load-prior-thinking-chain db-info query-id)]
    (when (seq chain)
      (let [tail (vec (take-last 1 chain))
            body (format-prior-thinking-chain tail)]
        (if body
          (str body "\n" PRIOR_THINKING_BREADCRUMB)
          PRIOR_THINKING_BREADCRUMB)))))

(defn build-cross-query-handover [db-info conversation-id current-query-id parent-iteration-id]
  (when (and db-info conversation-id (nil? parent-iteration-id))
    (try
      (let [all-queries (sort-by :created-at
                          (db/db-list-conversation-queries db-info conversation-id))
            current-id  (second current-query-id)
            prior       (last (remove #(= (:id %) current-id) all-queries))]
        (when prior
          (let [prior-id    [:id (:id prior)]
                iters        (db/db-list-query-iterations db-info prior-id)
                tagged-iters (vec (keep-indexed
                                    (fn [idx it]
                                      (when-let [t (:thinking it)]
                                        {:iteration idx :thinking t}))
                                    iters))
                final-answer (:answer prior)]
            (format-prior-turn-handover
              {:iterations tagged-iters
               :final-answer (when (and (string? final-answer)
                                     (not (str/blank? final-answer)))
                               final-answer)}))))
      (catch Throwable t
        (tel/log! {:level :warn
                   :data {:error (ex-message t) :conversation-id conversation-id}
                   :msg "build-cross-query-handover failed"})
        nil))))

;; ---------------------------------------------------------------------------
;; Nudges — per-iteration system hints injected into the iteration context
;; ---------------------------------------------------------------------------

(def ^:private BUDGET_WARNING_WINDOW 2)
(def ^:private REPETITION_THRESHOLD 3)

(defn- budget-warning
  [{:keys [iteration current-max-iterations]}]
  (let [iter (long (or iteration 0))
        max-iters (long (or current-max-iterations 0))
        remaining (- max-iters (inc iter))]
    (when (<= remaining BUDGET_WARNING_WINDOW)
      (str "[system_nudge] Iteration budget nearly exhausted (remaining="
        (max 0 remaining) "). If you can finalize safely, do it now."))))

(defn- repetition-warning
  [call-counts-atom prev-expressions]
  (when (and call-counts-atom (seq prev-expressions))
    (let [keys* (mapv (fn [{:keys [code error result]}]
                        (if error
                          [:error-only (str/trim (str error))]
                          [(str/trim (str code)) (pr-str result)]))
                  prev-expressions)
          max-count (swap! call-counts-atom
                      (fn [m]
                        (reduce (fn [acc k] (update acc k (fnil inc 0)))
                          (or m {}) keys*)))
          seen (apply max 0 (map #(get max-count % 0) keys*))]
      (when (>= seen REPETITION_THRESHOLD)
        "[system_nudge] You are repeating the same expression pattern. Change strategy."))))

(defn- collect-extension-nudges
  [extensions ctx]
  (when (seq extensions)
    (into []
      (keep (fn [ext]
              (when-let [nudge-fn (:ext/nudge-fn ext)]
                (try
                  (when ((:ext/activation-fn ext) (:environment ctx))
                    (let [result (nudge-fn ctx)]
                      (when (and (string? result) (not (str/blank? result)))
                        result)))
                  (catch Throwable t
                    (tel/log! {:level :warn :data {:ext (:ext/namespace ext) :error (ex-message t)}})
                    nil)))))
      extensions)))

;; ---------------------------------------------------------------------------
;; Iteration context builder
;; ---------------------------------------------------------------------------

(defn build-iteration-context
  [rlm-env {:keys [iteration current-max-iterations
                   prior-thinking
                   prev-expressions prev-iteration
                   call-counts-atom]}]
  (let [clamp (fn [s n] (if (and (string? s) (> (count s) n)) (subs s 0 n) s))
        iter-header (when (and iteration current-max-iterations)
                      (str "[iter " (inc (long iteration)) "/"
                        (long current-max-iterations) "]"))
        prior-block (when (and (string? prior-thinking)
                            (not (str/blank? prior-thinking)))
                      (str "<prior_thinking>\n"
                        (clamp prior-thinking PRIOR_THINKING_MAX_CHARS)
                        "\n</prior_thinking>"))
        var-index-str (read-var-index-str rlm-env)
        var-block (when (and (string? var-index-str)
                          (not (str/blank? var-index-str)))
                    (str "<var_index>\n" var-index-str "\n</var_index>"))
        expr-results (format-expression-results prev-expressions prev-iteration)
        nudge-ctx {:environment          rlm-env
                   :iteration            iteration
                   :current-max-iterations current-max-iterations
                   :prev-expressions      prev-expressions
                   :prev-iteration       prev-iteration
                   :user-var-count       0}
        built-in-nudges (keep identity
                          [(when (and iteration current-max-iterations)
                             (budget-warning
                               {:iteration              iteration
                                :current-max-iterations current-max-iterations}))
                           (repetition-warning call-counts-atom prev-expressions)])
        ext-nudges (collect-extension-nudges
                     (some-> (:extensions rlm-env) deref) nudge-ctx)
        nudges-block (str/join "\n" (concat built-in-nudges ext-nudges))
        parts (keep (fn [p] (when (and (string? p) (not (str/blank? p))) p))
                [iter-header prior-block expr-results var-block nudges-block])]
    (when (seq parts)
      (str/join "\n" parts))))



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

(defn- exception->iter-err
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
      (do (tel/log! {:level :error
                     :data  (assoc (format-exception-short e) :iteration iteration)}
            "Provider infrastructure error — aborting iteration loop")
        (throw e))
      (let [iter-err (exception->iter-err e ctx)]
        (tel/log! {:level :warn
                   :data (assoc (format-exception-short e) :iteration iteration)}
          "RLM iteration failed, feeding error to LLM")
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
      (tel/log! {:level :warn :id ::get-locals-fallback
                 :data {:error (ex-message e)}
                 :msg "Failed to read sandbox locals, returning empty map"})
      {})))

;; ---------------------------------------------------------------------------
;; Noop expression filter
;; ---------------------------------------------------------------------------

(def ^:private noop-exprs
  "Expressions the LLM emits only to satisfy the 'must return code' constraint.
   These carry no information — filter them before storage and display."
  #{":ok" ":ok\n" "nil" ":noop"})

(defn- noop-expr?
  "True when an expression is a structural noop (e.g. `:ok`)."
  [expr]
  (contains? noop-exprs (str/trim (str expr))))

(defn- strip-noop-expressions
  "Remove noop expressions from a vec. Returns nil-safe vec."
  [expressions]
  (vec (remove #(noop-expr? (:code %)) (or expressions []))))

;; ---------------------------------------------------------------------------
;; run-iteration
;; ---------------------------------------------------------------------------

(defn run-iteration
  "Runs a single RLM iteration: ask! → check final → execute code.
   Returns map with :thinking :expressions :final-result :api-usage etc."
  [rlm-env messages & [{:keys [iteration-spec routing iteration reasoning-level resolved-model on-chunk]
                        :or {iteration-spec ITERATION_SPEC_NON_REASONING}}]]
  (binding [*rlm-ctx* (merge *rlm-ctx* {:rlm-phase :run-iteration})]
    (let [effective-reasoning (when (some? reasoning-level)
                                (or (normalize-reasoning-level reasoning-level)
                                  (throw (ex-info "Invalid :reasoning-level."
                                           {:type :vis/invalid-reasoning-level
                                            :got reasoning-level}))))
          ;; Stream reasoning chunks to the TUI while the LLM is thinking
          streaming-fn (when on-chunk
                         (fn [{:keys [reasoning done?] :as chunk}]
                           (when (or (some? reasoning) done?)
                             (on-chunk {:iteration iteration
                                        :thinking  (some-> reasoning str)
                                        :code      nil
                                        :done?     (boolean done?)}))))
          ask-result (binding [llm/*log-context* {:query-id (:env-id rlm-env) :iteration iteration}]
                       (llm/ask! (:router rlm-env)
                         (cond-> {:spec iteration-spec
                                  :messages messages
                                  :routing (or routing {})
                                  :check-context? false}
                           effective-reasoning (assoc :reasoning effective-reasoning)
                           streaming-fn       (assoc :on-chunk streaming-fn))))
          parsed (:result ask-result)
          model-reasoning (:reasoning ask-result)
          thinking (or model-reasoning (:thinking parsed))
          _ (log-stage! :llm-response iteration
              {:has-reasoning (some? model-reasoning)
               :has-final (some? (:final parsed))
               :code-count (count (:code parsed))
               :duration-ms (:duration-ms ask-result)
               :tokens (:tokens ask-result)
               :thinking thinking})
          next-hint (:next parsed)
          next-model (when-let [m (:model next-hint)] (keyword m))
          next-reasoning (normalize-reasoning-level (:reasoning next-hint))
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
              expr-results (when (seq code-blocks)
                             (mapv (fn [{:keys [expr time-ms]}]
                                     (if-let [err (literal-code-block-error expr)]
                                       {:result nil :error err
                                        :stdout "" :stderr "" :execution-time-ms 0}
                                       (execute-code rlm-env expr :timeout-ms time-ms)))
                               code-entries))
              expr-errors (when expr-results
                            (seq (clojure.core/filter :error expr-results)))
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
              final-answer raw-answer
              confidence (or (:confidence parsed) :high)
              validation-error (or (when-not answer-type
                                     ":answer-type is required with :answer. Set mustache-text or mustache-markdown.")
                                 (when expr-errors
                                   (str "Code errors before final: " (:error (first expr-errors))))

                                 mustache-missing)
              expressions (when expr-results
                            (mapv (fn [idx code result]
                                    {:id idx :code code
                                     :result (:result result) :stdout (:stdout result)
                                     :stderr (:stderr result) :error (:error result)
                                     :execution-time-ms (:execution-time-ms result)
                                     :repaired? (:repaired? result)})
                              (range) code-blocks expr-results))]
          (if validation-error
            {:thinking thinking
             :next-model next-model :next-reasoning next-reasoning
             :expressions (or expressions
                            [{:id 0 :code final-answer :result nil :stdout "" :stderr ""
                              :error validation-error}])
             :final-result nil :api-usage api-usage
             :duration-ms (or (:duration-ms ask-result) 0)
             :llm-messages messages :llm-model (str resolved-model)}
            (let [final-result (cond-> {:final? true
                                        :answer final-answer
                                        :confidence confidence}
                                 (:reasoning parsed) (assoc :reasoning (:reasoning parsed)))]
              {:thinking thinking
               :next-model next-model :next-reasoning next-reasoning
               :expressions (strip-noop-expressions expressions) :final-result final-result :api-usage api-usage
               :duration-ms (or (:duration-ms ask-result) 0)
               :llm-messages messages :llm-model (str resolved-model)})))
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
                               (log-stage! :code-exec iteration
                                 {:idx (inc idx) :total total-blocks :code expr :time-ms time-ms})
                               (let [result (if-let [err (literal-code-block-error expr)]
                                              {:result nil :error err :stdout "" :stderr "" :execution-time-ms 0}
                                              (let [r (execute-code rlm-env expr :timeout-ms time-ms)]
                                                (log-stage! :code-result iteration
                                                  {:idx (inc idx) :total total-blocks
                                                   :execution-time-ms (:execution-time-ms r)
                                                   :error (:error r) :timeout? (:timeout? r) :result (:result r)})
                                                r))]
                                 {:block expr :result result}))
                         (range) coalesced)
              code-blocks (mapv :block executed)
              expression-results (mapv :result executed)
              expressions (mapv (fn [idx code result]
                                  {:id idx
                                   :code code
                                   :result (:result result)
                                   :stdout (:stdout result)
                                   :stderr (:stderr result)
                                   :error (:error result)
                                   :execution-time-ms (:execution-time-ms result)
                                   :timeout? (:timeout? result)
                                   :repaired? (:repaired? result)})
                            (range) code-blocks expression-results)]
          {:thinking thinking
           :next-model next-model :next-reasoning next-reasoning
           :expressions (strip-noop-expressions expressions) :final-result nil :api-usage api-usage
           :duration-ms (or (:duration-ms ask-result) 0)
           :llm-messages messages :llm-model (str resolved-model)})))))
