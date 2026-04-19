(ns com.blockether.vis.loop.core
  (:require
   [clojure.string :as str]
   [com.blockether.anomaly.core :as anomaly]
   [com.blockether.svar.internal.llm :as llm]
   [com.blockether.svar.internal.router :as router]
   [com.blockether.svar.internal.util :as util]

   [com.blockether.vis.loop.storage.db :as rlm-db
    :refer [create-rlm-conn dispose-rlm-conn!
            db-list-documents
            db-store-pageindex-document!]]
   [com.blockether.vis.loop.runtime.query.routing
    :refer [make-routed-sub-rlm-query-fn resolve-root-model provider-has-reasoning?]]
   [com.blockether.vis.loop.storage.schema :as schema
    :refer [ITERATION_SPEC_NON_REASONING ITERATION_SPEC_REASONING
            *eval-timeout-ms*
            validate-final bytes->base64 *rlm-ctx*]]
   [com.blockether.vis.loop.knowledge.skills :as rlm-skills]
   [com.blockether.vis.loop.knowledge.entity :as rlm-entity]
   [com.blockether.vis.loop.knowledge.ontology :as ontology]
   [com.blockether.vis.loop.runtime.prompt :as prompt]
   [com.blockether.vis.loop.runtime.core :as rlm-tools
    :refer [create-sci-context build-var-index]]
   [com.blockether.vis.loop.runtime.shared :as rt-shared :refer [realize-value truncate]]
   [com.blockether.vis.loop.tool :as sci-tool]
   [com.blockether.vis.loop.mustache :as mustache]
   [com.blockether.vis.loop.runtime.form-repair :as form-repair]
   [edamame.core :as edamame]
   [sci.core :as sci]
   [taoensso.trove :as trove]))

(defn rlm-debug!
  "Logs at :info level only when :rlm-debug? is true in *rlm-ctx*.
   Includes :rlm-phase from context automatically in data."
  [data msg]
  (when (:rlm-debug? *rlm-ctx*)
    (trove/log! {:level :info :data (assoc data :rlm-phase (:rlm-phase *rlm-ctx*)) :msg msg})))

(defn rlm-stage!
  "Always-on structured stage log for RLM pipeline visibility.
   Produces a single compact line per event with visual hierarchy."
  [stage iteration data]
  (let [fmt (fn [& parts] (str/join "  " (remove nil? parts)))
        msg (case stage
              :query-start
              (fmt "── RLM START"
                (str "model=" (:model data))
                (str "max-iter=" (:max-iterations data))
                (when (:reasoning? data) "reasoning=true")
                (str "query=\"" (str/replace (str (:query data)) #"\s+" " ") "\""))

              :iter-start
              (fmt (str "┌─ ITER " iteration)
                (str "msgs=" (:msg-count data))
                (when-let [r (:reasoning data)] (str "reasoning=" r)))

              :llm-response
              (let [thinking (str (:thinking data))]
                (fmt "│  ⇐ LLM"
                  (str "code=" (:code-count data))
                  (when (:has-final data) "FINAL=true")
                  (when (seq thinking)
                    (str "\n│     reasoning: " (str/replace thinking #"\s+" " ")))))

              :code-exec
              (let [code-1line (str/replace (str (:code data)) #"\s+" " ")]
                (fmt (str "│  ▶ EXEC [" (:idx data) "/" (:total data) "]")
                  code-1line
                  (str "budget=" (:time-ms data) "ms")))

              :code-result
              (fmt (str "│  ◀ EXEC [" (:idx data) "/" (:total data) "]")
                (str (:execution-time-ms data) "ms")
                (when (:error data) (str "ERROR: " (truncate (str/replace (str (:error data)) #"\s+" " ") 80)))
                (when (:timeout? data) "TIMEOUT")
                (when (and (not (:error data)) (not (:timeout? data)))
                  (let [r (:result data)]
                    (cond
                      (nil? r) "result=✓"
                      (fn? r) "result=fn"
                      :else (str "result=" (truncate (str/replace (pr-str r) #"\s+" " ") 80))))))

              :iter-end
              (fmt (str "└─ ITER " iteration)
                (str "blocks=" (:blocks data))
                (str "errors=" (:errors data))
                (str "total=" (reduce + 0 (map #(or % 0) (or (:times data) []))) "ms"))

              :final
              (fmt "│  ══ FINAL"
                (str "answer=\"" (str/replace (str (:answer data)) #"\s+" " ") "\"")
                (str "confidence=" (:confidence data))
                (str "iters=" (:iterations data)))

              :query-end
              (fmt "── RLM END"
                (str (:duration-ms data) "ms")
                (str "iters=" (:iterations data))
                (when (:cost data) (str "cost=$" (:cost data))))

              :error
              (fmt "│  ✘ ERR"
                (str "reason=" (:reason data))
                (when (:max data) (str "max=" (:max data))))

              :empty
              (fmt "│  ⚠ EMPTY" "no code blocks")

              ;; fallback
              (fmt (str "│  " (name stage)) (pr-str data)))
        env-id (:rlm-env-id *rlm-ctx*)
        prefix (when env-id (str "[q=" env-id " i=" iteration "] "))]
    (trove/log! {:level :info :id ::rlm-stage :msg (str prefix msg)})))

(defn- status->id
  [status]
  (when status
    (keyword "rlm.status" (name status))))

(def ^:private reasoning-quick :quick)
(def ^:private reasoning-balanced :balanced)
(def ^:private reasoning-deep :deep)

(defn- normalize-reasoning-level
  "Returns canonical :quick | :balanced | :deep, or nil for unknown input.
   Accepts the abstract vocabulary, or OpenAI-style :low/:medium/:high aliases
   (for backward-compat with `:reasoning-default` configs). Delegates to svar."
  [v]
  (router/normalize-reasoning-level v))

(defn- reasoning-level-for-errors
  "Maps consecutive error count to an effective reasoning level.
   `base` is the no-error level for iteration start/recovery."
  [base consecutive-errors]
  (cond
    (<= (long consecutive-errors) 0) base
    (= 1 (long consecutive-errors)) (if (= base reasoning-quick) reasoning-balanced reasoning-deep)
    :else reasoning-deep))

;; =============================================================================
;; RLM Environment
;; =============================================================================

(defn create-rlm-env
  "Creates an RLM execution environment (internal use only).

   Params:
   `depth-atom` - Atom tracking recursion depth.
   `router` - Router from llm/make-router. Required.
   `opts` - Map, optional:
     - :db - DB spec: nil (no DB), :temp, path string, {:conn c}, {:path p}
     - :documents - Vector of PageIndex documents to preload (stored exactly as-is).

   Returns:
   Map with :sci-ctx, :sub-rlm-query-fn, :db-info, :router."
  ([depth-atom router]
   (create-rlm-env depth-atom router {}))
  ([depth-atom router {:keys [db documents]}]
   (when-not router
     (throw (ex-info "Router is required for RLM environment" {:type :rlm/missing-router})))
   (let [db-info (create-rlm-conn db)
         _ (when (and db-info (seq documents))
             (doseq [doc documents]
               (db-store-pageindex-document! db-info doc)))
         sub-rlm-query-fn (make-routed-sub-rlm-query-fn {} depth-atom router nil nil)
         {:keys [sci-ctx sandbox-ns initial-ns-keys]} (create-sci-context sub-rlm-query-fn db-info nil nil)
         tool-registry-atom (atom {})
         state-atom (atom {:custom-bindings {} :custom-docs []})
         env {:sci-ctx sci-ctx :sandbox-ns sandbox-ns :initial-ns-keys initial-ns-keys
              :sub-rlm-query-fn sub-rlm-query-fn
              :tool-registry-atom tool-registry-atom
              :state-atom state-atom
              :db-info db-info
              :router router}]
     (or (rlm-tools/register-builtin-tools! env) env))))

(defn dispose-rlm-env! [{:keys [db-info]}]
  (when db-info (dispose-rlm-conn! db-info)))

(defn get-locals
  "Returns {sym → val} of user-defined vars in SCI sandbox (excludes built-ins).
   Direct atom read from SCI env — zero eval overhead."
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

;; =============================================================================
;; Public env API
;; =============================================================================

(defn db-info
  "Current db-info map for env. Nil-safe."
  [env]
  (:db-info env))

(defn custom-bindings
  "Current custom SCI bindings {sym -> value}."
  [env]
  (some-> (:state-atom env) deref :custom-bindings))

(defn custom-docs
  "Current custom doc entries (vec of tool-defs)."
  [env]
  (some-> (:state-atom env) deref :custom-docs))
;; =============================================================================
;; Code Execution
;; =============================================================================

(defn- run-sci-code
  "Evaluate `code` in `sci-ctx` with captured stdout/stderr.
   Uses eval-string+ with :ns to ensure code runs in sandbox namespace.
   Returns {:result :stdout :stderr :error} with writers already closed."
  [sci-ctx code & {:keys [sandbox-ns]}]
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

(defn- detect-common-mistakes
  "Pre-exec lint: catches common Clojure mistakes BEFORE SCI eval.
   Returns nil if clean, or an error string with actionable fix."
  [code]
  (let [s (str/trim code)]
    (cond
      ;; Nested #() - illegal in Clojure, cryptic SCI error
      (re-find #"#\([^)]*#\(" s)
      "Nested #() is illegal in Clojure. Rewrite inner #() as (fn [...] ...)"
      ;; Nothing wrong
      :else nil)))

(def ^:private edamame-opts
  "Edamame parser options matching Clojure/SCI syntax.
   :all enables fn literals, deref, var, regex, quote, etc."
  {:all true
   :readers (fn [tag] (fn [val] (list 'do val)))})

(defn- check-syntax
  "Parses code with edamame. Returns parsed forms or throws."
  [code]
  (edamame/parse-string-all code edamame-opts))

(defn- check-bare-list
  "Detects unquoted list literals like (6 7 8) that parse fine but fail at eval.
   Returns error string or nil."
  [forms]
  (let [first-form (first forms)]
    (when (and (= 1 (count forms))
            (list? first-form) (seq first-form)
            (let [head (first first-form)]
              (not (or (symbol? head) (keyword? head)
                     (list? head) (set? head) (map? head) (vector? head)))))
      (str "Bare list literal: " (pr-str first-form)
        ". Quote it: '(" (str/join " " first-form) ")"))))

(defn- parse-clojure-syntax
  "Validates Clojure syntax using edamame (same parser as SCI).
   Runs all checks: syntax parse, bare list detection.
   Returns nil if valid, or an error string if broken."
  [code]
  (try
    (let [forms (check-syntax code)]
      (or (check-bare-list forms)
        nil))
    (catch Throwable e
      (ex-message e))))

(defn execute-code [{:keys [sci-ctx sandbox-ns]} code & {:keys [timeout-ms]}]
  (binding [*rlm-ctx* (merge *rlm-ctx* {:rlm-phase :execute-code})]
    (let [bal (form-repair/form-balance code)
          start-time (System/currentTimeMillis)
          lint-error (detect-common-mistakes code)]
      (if lint-error
        ;; Pre-exec lint caught a known mistake - return clear error without eval
        (do (rlm-debug! {:lint-error lint-error} "Pre-exec lint caught mistake")
          {:result nil :stdout "" :stderr "" :error lint-error
           :execution-time-ms 0 :timeout? false})
        (if-let [parse-error (parse-clojure-syntax code)]
          ;; Pre-parse failed — attempt paren repair before giving up
          (let [repaired (try (form-repair/repair-code code) (catch Throwable _ code))]
            (if (and (not= repaired code) (nil? (parse-clojure-syntax repaired)))
              (do (rlm-debug! {:parse-error parse-error :repaired-len (count repaired)} "Edamame pre-parse failed, paren repair recovered")
                (let [result (run-sci-code sci-ctx repaired :sandbox-ns sandbox-ns)
                      t (- (System/currentTimeMillis) start-time)]
                  (assoc result :execution-time-ms t :timeout? false :repaired? true)))
              (do (rlm-debug! {:parse-error parse-error} "Edamame pre-parse failed, repair did not help")
                {:result nil :stdout "" :stderr "" :error parse-error
                 :execution-time-ms 0 :timeout? false})))
        ;; Normal execution path
          (let [execution-result (if timeout-ms
                                   (binding [*eval-timeout-ms* (schema/clamp-eval-timeout-ms timeout-ms)]
                                     (run-sci-code sci-ctx code :sandbox-ns sandbox-ns))
                                   (run-sci-code sci-ctx code :sandbox-ns sandbox-ns))
                execution-time (- (System/currentTimeMillis) start-time)]
            (if (:timeout? execution-result)
              (do
                (assoc execution-result :execution-time-ms execution-time :timeout? true))
              (let [{:keys [error]} execution-result
                    final-result (if (and error (form-repair/parse-error? error))
                                   (try
                                     (let [repaired (form-repair/repair-code code)]
                                       (if (= repaired code)
                                         (do (trove/log! {:level :debug :id ::repair-noop
                                                          :data {:code-len (count code) :error error}
                                                          :msg "Paren repair: no change needed"})
                                           execution-result)
                                         (let [retry (run-sci-code sci-ctx repaired :sandbox-ns sandbox-ns)]
                                           (if (:error retry)
                                             (do (trove/log! {:level :warn :id ::repair-retry-failed
                                                              :data {:original-error error
                                                                     :retry-error (:error retry)
                                                                     :code-len (count code)
                                                                     :repaired-len (count repaired)
                                                                     :added-chars (- (count repaired) (count code))
                                                                     :repaired-tail (subs repaired (max 0 (- (count repaired) 80)))}
                                                              :msg "Paren repair changed code but retry still failed"})
                                               execution-result)
                                             (do
                                               (trove/log! {:level :info :id ::repair-applied
                                                            :data {:original code :repaired repaired :sci-error error}
                                                            :msg "Paren repair applied successfully"})
                                               (assoc retry :repaired? true))))))
                                     (catch Throwable _
                                       execution-result))
                                   execution-result)
                    {:keys [result stdout stderr error]} final-result]
                (assoc final-result :execution-time-ms execution-time :timeout? false)))))))))

(defn answer-str
  "Extracts a string representation from an RLM answer.
   Answer is {:result value :type type} — returns the :result as a string."
  [answer]
  (let [v (:result answer answer)]
    (if (string? v) v (pr-str v))))

(defn- cleanup-claim-without-forget?
  "True when the final answer claims vars/index cleanup, but the iteration did
   not actually emit :forget. Prevents user-facing mutation claims that were
   never performed by the runtime."
  [final-answer forget-names]
  (let [s (str/lower-case (str final-answer))]
    (and (empty? forget-names)
      (re-find #"(posprz|sprzat|cleaned up|cleanup|wyrzucon|usun|wywal|removed|forgot|forgotten)" s)
      (re-find #"(var|vars|zmienn|var_index|<var_index>|index|indeks)" s))))

(def ^:private BARE_STRING_RE
  "Matches a code block that is just a quoted string literal (double-quoted).
   Allows optional whitespace around it."
  #"^\s*\"[^\"]*\"\s*$")

(defn bare-string-code-block?
  "True when a code expression is a bare string literal — prose that the LLM
   should have put in :answer instead of :code."
  [expr]
  (boolean (re-matches BARE_STRING_RE (str expr))))

(def ^:private PLACEHOLDER_WORDS
  "Single-word placeholders the LLM sometimes emits as :answer instead of
   the real content. Checked AFTER var-resolve — if the word matches a def, it
   is resolved and this check never fires."
  #{"listing" "result" "results" "done" "ok" "output" "ready" "complete"
    "finished" "listed" "wynik" "gotowe" "zrobione" "wypisane"})

(defn placeholder-final-answer?
  "True when :answer is a known placeholder word that was NOT resolved
   to a var (var resolution happens upstream). Returns nil when answer is fine."
  [raw-answer resolved?]
  (and (not resolved?)
    (contains? PLACEHOLDER_WORDS (str/lower-case (str/trim (str raw-answer))))))

;; =============================================================================
;; System Prompt — delegates to loop.runtime.prompt
;; =============================================================================

(def format-custom-docs prompt/format-custom-docs)

(def build-document-summary prompt/build-document-summary)



(def build-system-prompt prompt/build-system-prompt)

;; =============================================================================
;; Iteration Loop
;; =============================================================================

(defn run-iteration
  "Runs a single RLM iteration: ask! → check final → execute code.

   Uses ask! with iteration spec for provider-enforced JSON structured output.
   No regex fallback, no code-level FINAL detection.

   Params:
   `rlm-env` - RLM environment map.
   `messages` - Vector of message maps for the LLM.
   `opts` - Map, optional:
      - :iteration-spec - Spec for ask! (default: ITERATION_SPEC_NON_REASONING).
                         For reasoning providers, pass ITERATION_SPEC_REASONING.
      - :on-chunk - Streaming callback function."
  [rlm-env messages & [{:keys [iteration-spec on-chunk routing iteration reasoning-level]
                        :or {iteration-spec ITERATION_SPEC_NON_REASONING}}]]
  (binding [*rlm-ctx* (merge *rlm-ctx* {:rlm-phase :run-iteration})]
    (let [model-name (resolve-root-model (:router rlm-env))
          effective-reasoning (when (some? reasoning-level)
                                (or (normalize-reasoning-level reasoning-level)
                                  (throw (ex-info "Invalid :reasoning-level. Expected one of quick|balanced|deep."
                                           {:type :rlm/invalid-reasoning-level
                                            :got reasoning-level}))))
          ;; ask! auto-translates :reasoning to the right provider extra-body
          ;; using the selected model's :reasoning? flag + provider :api-style.
          ask-result (binding [llm/*log-context* {:query-id (:env-id rlm-env) :iteration iteration}]
                       (llm/ask! (:router rlm-env)
                         (cond-> {:spec iteration-spec
                                  :messages messages
                                  :routing (or routing {})
                                  :check-context? false}
                           on-chunk          (assoc :on-chunk on-chunk)
                           effective-reasoning (assoc :reasoning effective-reasoning))))
          parsed (:result ask-result)
          model-reasoning (:reasoning ask-result)
          ;; Native reasoning takes priority over spec-parsed thinking
          thinking (or model-reasoning (:thinking parsed))
          _ (rlm-stage! :llm-response iteration
              {:has-reasoning (some? model-reasoning)
               :has-final (some? (:final parsed))
               :code-count (count (:code parsed))
               :duration-ms (:duration-ms ask-result)
               :tokens (:tokens ask-result)
               :thinking thinking})
          ;; LLM's steering hints for the next iteration (both sub-keys optional).
          next-hint (:next parsed)
          next-model (when-let [m (:model next-hint)]
                       (keyword m))
          next-reasoning (normalize-reasoning-level (:reasoning next-hint))
          ;; LLM's request to drop named vars from <var_index>.
          forget-names (vec (keep (fn [n] (when (and n (seq (str n))) (str n)))
                              (:forget parsed)))
          ;; Token usage from ask! result
          api-usage {:prompt_tokens (get-in ask-result [:tokens :input] 0)
                     :completion_tokens (get-in ask-result [:tokens :output] 0)
                     :completion_tokens_details {:reasoning_tokens (get-in ask-result [:tokens :reasoning] 0)}
                     :prompt_tokens_details {:cached_tokens (get-in ask-result [:tokens :cached] 0)}}]
      ;; Check for final answer in flat spec response
      (if-let [raw-final-answer (:answer parsed)]
        (let [answer-type (some-> (:answer-type parsed) keyword)
              ;; Extract code blocks — must be maps with :expr and :time-ms
              raw-code (or (:code parsed) [])
              code-entries (vec (keep (fn [block]
                                        (when (map? block)
                                          (let [expr (str (:expr block ""))
                                                time-ms (:time-ms block)]
                                            (when-not (str/blank? expr)
                                              {:expr expr :time-ms (or time-ms (throw (ex-info "Code block missing :time-ms" {:expr expr})))}))))
                                  raw-code))
              code-blocks (mapv :expr code-entries)
               ;; Execute code blocks in SCI — ALWAYS when present, regardless of answer-type.
               ;; MUST run BEFORE single-token :answer resolve so freshly-def'd vars are visible.
              exec-results (when (seq code-blocks)
                             (mapv (fn [{:keys [expr time-ms]}]
                                     (if (bare-string-code-block? expr)
                                       {:result nil :error "Bare string literal in :code. Prose belongs in :answer, not :code."
                                        :stdout "" :stderr "" :execution-time-ms 0}
                                       (execute-code rlm-env expr :timeout-ms time-ms)))
                               code-entries))
              exec-errors (when exec-results
                            (seq (filter :error exec-results)))
               ;; NOW read sandbox — code has executed, (def reply ...) is visible.
               ;; Var-resolve mechanic (two modes):
               ;; 1. Single-word: :answer is one token matching a sandbox var → substitute entirely.
               ;;    e.g. {:answer "reply"} → var value of `reply`.
               ;; 2. Mustache template: :answer contains {{var}} / {{#list}}...{{/list}} etc.
               ;;    Full Mustache via jmustache. Sandbox vars are the data context.
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
              ;; Mustache template rendering via jmustache.
              ;; Clojure maps work natively — no conversion needed.
              ;; Missing vars → jmustache throws → validation error fed back to LLM.
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
                  (rlm-debug! {:token (str raw-final-answer)
                               :resolved-chars (count resolved-var-value)}
                    "Single-word :answer resolved to var value"))
              _ (when template-answer
                  (rlm-debug! {:template (str raw-final-answer)
                               :resolved-chars (count template-answer)}
                    "Mustache template rendered"))
              final-answer raw-answer
              confidence (or (:confidence parsed) :high)
              validation-error (or (when-not answer-type
                                     ":answer-type is required with :answer. Set mustache-text or mustache-markdown.")
                                 (when exec-errors
                                   (str "Code errors before final: " (:error (first exec-errors))))
                                 (when (cleanup-claim-without-forget? final-answer forget-names)
                                   "You claimed vars/index cleanup in the final answer, but this iteration did not emit :forget. Emit :forget with the concrete var names first, then finalize.")
                                 (when (placeholder-final-answer? raw-final-answer (or (some? resolved-var-value) (some? template-answer)))
                                   (str "Placeholder word '" (str/trim (str raw-final-answer)) "' is not a real answer. Put the actual content in :answer, or def the result and use its var name."))
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
            (do (rlm-debug! {:final-answer (truncate final-answer 200)
                             :validation-error validation-error} "FINAL rejected")
              {:thinking thinking
               :next-model next-model :next-reasoning next-reasoning
               :forget forget-names
               :executions (or executions
                             [{:id 0 :code final-answer :result nil :stdout "" :stderr ""
                               :error validation-error}])
               :final-result nil :api-usage api-usage
               :duration-ms (or (:duration-ms ask-result) 0)})
            (let [sources (vec (or (:sources parsed) []))
                  final-result (cond-> {:final? true
                                        :answer {:result final-answer :type String}
                                        :confidence confidence}
                                 (seq sources) (assoc :sources sources)
                                 (:reasoning parsed) (assoc :reasoning (:reasoning parsed)))]
              {:thinking thinking
               :next-model next-model :next-reasoning next-reasoning
               :forget forget-names
               :executions (or executions []) :final-result final-result :api-usage api-usage
               :duration-ms (or (:duration-ms ask-result) 0)})))
        ;; Normal path: execute code blocks — must be maps with :expr and :time-ms
        (let [raw-parsed (or (:code parsed) [])
              ;; Extract: every block must be a map with :expr and :time-ms
              normalized (vec (keep (fn [block]
                                      (when (map? block)
                                        (let [expr (str (:expr block ""))
                                              time-ms (:time-ms block)]
                                          (when-not (str/blank? expr)
                                            {:expr expr :time-ms (or time-ms (throw (ex-info "Code block missing :time-ms" {:expr expr})))}))))
                                raw-parsed))
              raw-exprs (mapv :expr normalized)
              ;; Coalesce fragments: when model splits one expression across multiple
              ;; array entries (one line per string), join unbalanced blocks with the
              ;; next until parens balance. Prevents form-repair from "fixing" each
              ;; line individually into a broken zero-body form.
              coalesced (loop [remaining normalized
                               result []]
                          (if (empty? remaining)
                            result
                            (let [{:keys [expr time-ms]} (first remaining)
                                  bal (form-repair/form-balance expr)]
                              (if (and (pos? bal) (next remaining))
                                ;; Unbalanced opener - join with subsequent blocks, sum timeouts
                                (let [[joined-expr joined-time rest-blocks]
                                      (loop [acc expr
                                             t-acc (or time-ms 0)
                                             rem (rest remaining)]
                                        (if (or (<= (form-repair/form-balance acc) 0) (empty? rem))
                                          [acc t-acc rem]
                                          (let [nxt (first rem)]
                                            (recur (str acc "\n" (:expr nxt))
                                              (+ t-acc (or (:time-ms nxt) 0))
                                              (rest rem)))))]
                                  (recur rest-blocks
                                    (conj result {:expr joined-expr
                                                  :time-ms joined-time})))
                                (recur (rest remaining) (conj result (first remaining)))))))
              code-blocks (mapv :expr coalesced)
              time-limits (mapv :time-ms coalesced)
              total-blocks (count code-blocks)
              execution-results (mapv (fn [idx code timeout]
                                        (rlm-stage! :code-exec iteration
                                          {:idx (inc idx) :total total-blocks
                                           :code code :time-ms timeout})
                                        (if (bare-string-code-block? code)
                                          ;; Runtime rejection: prose in :code → error, not execution
                                          (let [err "Bare string literal in :code. Prose belongs in :answer with answer-type text, not in :code."]
                                            (rlm-stage! :code-result iteration
                                              {:idx (inc idx) :total total-blocks :error err})
                                            {:result nil :error err :stdout "" :stderr "" :execution-time-ms 0})
                                          (let [r (execute-code rlm-env code :timeout-ms timeout)]
                                            (rlm-stage! :code-result iteration
                                              {:idx (inc idx) :total total-blocks
                                               :execution-time-ms (:execution-time-ms r)
                                               :error (:error r)
                                               :timeout? (:timeout? r)
                                               :result (:result r)})
                                            r)))
                                  (range) code-blocks time-limits)
              ;; Combine code blocks with their execution results
              executions (mapv (fn [idx code result]
                                 {:id idx
                                  :code code
                                  :result (:result result)
                                  :stdout (:stdout result)
                                  :stderr (:stderr result)
                                  :error (:error result)
                                  :execution-time-ms (:execution-time-ms result)
                                  :repaired? (:repaired? result)})
                           (range) code-blocks execution-results)]
          {:thinking thinking
           :next-model next-model :next-reasoning next-reasoning
           :executions executions :final-result nil :api-usage api-usage
           :duration-ms (or (:duration-ms ask-result) 0)})))))

(defn- error-hint
  "Returns a specialized hint for a known error, or nil. Extracts context
   from the error message for a targeted fix suggestion."
  [error-msg]
  (when error-msg
    (let [e (str error-msg)]
      (cond
        ;; Unable to resolve symbol: X -> tell them exactly which symbol
        (re-find #"Unable to resolve symbol: (\S+)" e)
        (let [[_ sym] (re-find #"Unable to resolve symbol: (\S+)" e)]
          (str "'" sym "' is not defined. (def " sym " ...) or check spelling."))

        ;; Wrong number of args (N) passed to: X
        (re-find #"Wrong number of args \((\d+)\) passed to: (\S+)" e)
        (let [[_ n target] (re-find #"Wrong number of args \((\d+)\) passed to: (\S+)" e)]
          (cond
            (str/includes? target "PersistentVector")
            (str "Vectors take 1 arg (index). Use (nth v idx) or (subvec v start end), not (v " (str/join " " (repeat (parse-long n) "x")) ").")
            :else
            (str "Function expects different arity than " n ". Check with (doc fn-name).")))

        ;; Long cannot be cast to IFn
        (str/includes? e "cannot be cast to clojure.lang.IFn")
        "You're calling a non-function. Bare (1 2 3) calls 1 as fn. Use '(1 2 3) for list literals."

        ;; Nested fn literals
        (str/includes? e "Nested fn")
        "Nested #() is illegal. Rewrite inner #() as (fn [x] ...)."

        ;; Unbound fn
        (re-find #"unbound fn: #'sandbox/(\S+)" e)
        (let [[_ sym] (re-find #"unbound fn: #'sandbox/(\S+)" e)]
          (str "'" sym "' was declared but its defn failed. Fix the defn above first."))

        ;; LazySeq cast
        (str/includes? e "LazySeq")
        "conj/peek/pop need a vector, not a lazy seq. Wrap with (vec ...) first."

        ;; NullPointerException
        (str/includes? e "NullPointerException")
        "Something is nil unexpectedly. Debug with (prn suspect-value) to find which value is nil."

        ;; recur tail position
        (re-find #"recur.*tail" e)
        "recur must be the last expression in a loop/fn body."

        ;; Unmatched delimiter inside map with char literals (\} \{)
        (and (str/includes? e "Unmatched delimiter")
          (str/includes? e "}"))
        "Map literals with \\} or \\{ as keys/values confuse the reader. Use (hash-map \\) \\( \\] \\[ \\} \\{) instead of {\\} \\{ ...}."

        ;; EOF in map/fn context — might be char literal issue
        (and (str/includes? e "EOF while reading")
          (str/includes? e "match {"))
        "If your map contains bracket char literals like \\} \\{, use (hash-map ...) instead of a map literal."

        :else nil))))

(def ^:private SLOW_EXECUTION_MS
  "Threshold in ms above which execution time is flagged as slow."
  5000)

(defn- formatted-str-of
  "Return the canonical formatted string for a tool-produced value.

   Lookup order:
   1. `:rlm/formatted` from value metadata — precomputed by the tool wrapper.
   2. `(:rlm/format meta) called on value` — meta was preserved but the cached
      string wasn't (e.g. value was transformed but `vary-meta` carried the fn).
   3. nil — caller falls back to its own rendering (pr-str of realized value).

   Returns nil for non-IObj values (primitives cannot carry meta). The caller
   is responsible for the default-string fallback so this fn stays pure."
  [v]
  (when (instance? clojure.lang.IObj v)
    (let [m (meta v)]
      (or (:rlm/formatted m)
          (when-let [f (:rlm/format m)]
            (try (f v) (catch Throwable _ nil)))))))

(defn format-executions
  "Formats executions for LLM feedback as EDN.
   All results shown inline — context budget handles size naturally.
   Error hints injected only when an error matches a known pattern.
   Includes execution time for every block; warns when slow."
  [executions]
  (str/join "\n"
    (map (fn [{:keys [code error result stdout repaired? execution-time-ms]}]
           (let [code-str (str/trim (or code ""))
                 hint (when error (error-hint error))
                 val-part (cond
                            error
                            (str ":error " (pr-str error)
                              (when hint (str " :hint " (pr-str hint))))

                             (fn? result)
                             (str ":error \"" code-str " is a function object. Call it: (" code-str ")\"")

                             :else
                             (str ":ok " (or (formatted-str-of result)
                                             (pr-str (realize-value result)))))

                 stdout-part (when-not (str/blank? stdout)
                               (str " :stdout " (pr-str stdout)))
                 warning-part (when repaired?
                                " :warning \"auto-repaired delimiters\"")
                 time-ms (or execution-time-ms 0)
                 slow? (> time-ms SLOW_EXECUTION_MS)
                 time-part (str " :time-ms " time-ms
                             (when slow?
                               " :perf-warning \"SLOW — optimize algorithm or reduce input size\""))]
             (str "{" code-str " → " val-part (or stdout-part "") (or warning-part "") time-part "}")))
      executions)))

(def ^:private EXECUTION_SAFETY_CAP_CHARS
  "Safety cap for one value rendering in an execution receipt. Receipts show
   the FULL value — lazy seqs are already bounded by realize-value's 100-item
   cap. This guard only protects against pathological non-lazy dumps (e.g.
   a 50MB string). In normal use no truncation happens."
  200000)

(def ^:private EXECUTION_STDERR_CHARS
  "Budget for captured stderr — diagnostics only, not a full data dump."
  2000)

(defn- type-label-of
  "Short human-readable type label for result serialization."
  [v]
  (cond
    (nil? v) "nil"
    (map? v) "map"
    (vector? v) "vector"
    (set? v) "set"
    (sequential? v) "seq"
    (string? v) "string"
    (integer? v) "int"
    (float? v) "float"
    (boolean? v) "bool"
    (keyword? v) "keyword"
    :else (.getSimpleName (class v))))

(defn- size-suffix
  "Optional ' :size N-units' tail for collections / strings, blank otherwise."
  [v]
  (cond
    (nil? v) ""
    (string? v) (str " :size " (count v) "-chars")
    (coll? v) (str " :size " (count v) "-items")
    :else ""))

(defn- format-execution-results
  "Formats the previous iteration's executions as an XML receipt tagged
   `<journal>`. Each block shows the FULL evaluated value — no summarisation,
   no preview. Lazy sequences are already bounded by realize-value's 100-item
   cap; everything else is pr-str'd in full, protected only by a
   pathological-size safety guard. The LLM grounds :final on what it sees
   here plus anything it explicitly pulls via history/var tools.

   (def sym expr) is rendered as :result-kind :var :var-name \"sym\" so the
   LLM can reference the symbol on the next iteration.

   NOTE: the XML tag is `<journal>` (NOT `<execution_results>`) — the name
   reflects that this is the only rolling ledger in the prompt. Anything
   earlier than the previous iteration is accessed on-demand via
   conversation-history / var-history / var-diff."
  [executions iteration]
  (when (seq executions)
    (str "<journal iteration=\"" iteration "\">\n"
      (str/join "\n"
        (map-indexed
          (fn [idx {:keys [code error result stdout stderr execution-time-ms]}]
            (let [code-str (str/trim (or code ""))
                  stdout-part (when-not (str/blank? stdout)
                                (str " :stdout " (pr-str stdout)))
                  stderr-part (when-not (str/blank? stderr)
                                (str " :stderr " (pr-str (truncate stderr EXECUTION_STDERR_CHARS))))
                  time-ms (or execution-time-ms 0)
                  slow? (> time-ms SLOW_EXECUTION_MS)
                  time-part (str " :time-ms " time-ms
                              (when slow?
                                " :perf-warning \"SLOW — optimize algorithm, avoid brute-force, reduce input size\""))
                  result-info (cond
                                error
                                (str "{:success? false :error " (pr-str (truncate error 400)) time-part "}")

                                (fn? result)
                                (str "{:success? false :error \"Result is a function, not a value\"" time-part "}")

                                (instance? clojure.lang.Var result)
                                (let [^clojure.lang.Var var-obj result
                                      var-name (name (.sym var-obj))
                                      raw-bound (.getRawRoot var-obj)
                                      ;; Check meta on the RAW bound value before
                                      ;; realize-value strips it (realize-value
                                      ;; rebuilds maps/vecs/sets, losing meta).
                                      pre-formatted (formatted-str-of raw-bound)
                                      bound (realize-value raw-bound)
                                      value-str (or pre-formatted
                                                    (truncate (pr-str bound) EXECUTION_SAFETY_CAP_CHARS))]
                                  (str "{:success? true :result-kind :var"
                                    " :var-name " (pr-str var-name)
                                    " :value-type " (type-label-of bound)
                                    (str/replace (size-suffix bound) ":size" ":value-size")
                                    " :value " value-str
                                    stdout-part stderr-part
                                    time-part
                                    "}"))

                                :else
                                (let [pre-formatted (formatted-str-of result)
                                      v (realize-value result)
                                      value-str (or pre-formatted
                                                    (truncate (pr-str v) EXECUTION_SAFETY_CAP_CHARS))]
                                  (str "{:success? true :result-type " (type-label-of v)
                                    (size-suffix v)
                                    " :value " value-str
                                    stdout-part stderr-part
                                    time-part
                                    "}")))]
              (str "  [" (inc idx) "] " code-str " → " result-info)))
          executions))
      "\n</journal>")))

;; NOTE (2025-04): the accumulating `execution_journal` block was removed.
;; It grew monotonically per iteration (~300 chars squashed + 8K recent per
;; entry × iterations_so_far) and bloated the prompt on long turns with no
;; real benefit once the LLM has:
;;   - `*query*`     — current query, bound into the sandbox on query start.
;;   - `*reasoning*` — current iteration's thinking, auto-def'd per iteration.
;;   - `conversation-history` / `conversation-code` / `conversation-results`
;;     — on-demand access to prior turns across queries.
;;   - `var-history` / `var-diff` — on-demand access to var evolution.
;; The `<journal>` block (was `<execution_results>`) carries only the PREVIOUS
;; iteration's executions. Pulling anything further back is the LLM's job via
;; the tools above. This makes the per-iteration prompt O(1) instead of O(n).

(defn- extract-def-names
  "Extracts var names from code blocks via EDN parsing of def-like forms."
  [executions]
  (->> executions
    (mapcat (fn [{:keys [code error]}]
              (when-not error
                (try
                  (->> (edamame/parse-string-all (or code "") {:all true})
                    (keep (fn [form]
                            (when (seq? form)
                              (let [[op name & _] form]
                                (when (and (contains? '#{def defn defn- defonce defmulti defmacro} op)
                                        (symbol? name))
                                  name)))))
                    distinct)
                  (catch Exception e
                    (trove/log! {:level :debug :id ::extract-def-names-fallback
                                 :data {:error (ex-message e)}
                                 :msg "Failed to parse code forms for def names, returning empty"})
                    [])))))
    (map str)
    vec))

(defn- restorable-var-snapshots
  "Returns serializable snapshots of user vars introduced by this iteration."
  [rlm-env executions]
  (let [execution->defs (mapv (fn [{:keys [error] :as execution}]
                                [execution (when-not error (set (map symbol (extract-def-names [execution]))))])
                          executions)
        defined (into #{} (mapcat second) execution->defs)
        sym->code (reduce (fn [acc [{:keys [code]} defs]]
                            (if (and code (seq defs))
                              (reduce #(assoc %1 %2 code) acc defs)
                              acc))
                    {}
                    execution->defs)
        locals (get-locals rlm-env)]
    (->> locals
      (keep (fn [[sym value]]
              (when (contains? defined sym)
                (let [realized (realize-value value)]
                  (when (or (nil? realized)
                          (string? realized)
                          (number? realized)
                          (keyword? realized)
                          (boolean? realized)
                          (symbol? realized)
                          (map? realized)
                          (vector? realized)
                          (set? realized)
                          (list? realized)
                          (sequential? realized))
                    {:name (str sym)
                     :value realized
                     :code (get sym->code sym)})))))
      vec)))

(defn- earmuffed-sym?
  "True for names matching *foo* — used to identify SYSTEM vars. Also
   rejects the degenerate single-char `*` to avoid false positives."
  [sym]
  (let [n (name sym)]
    (and (> (count n) 2) (str/starts-with? n "*") (str/ends-with? n "*"))))

(defn- forget-vars!
  "Unmap `names` from the SCI sandbox namespace. Mirrors what the LLM asked
   for via the `:forget` iteration-spec field — removes the bindings so they
   stop showing up in <var_index>. The persisted :iteration-var rows in the
   DB are not touched; `(restore-var 'sym)` can bring one back later.

   HARD GUARD: earmuffed SYSTEM vars (*query*, *reasoning*, *answer*, …)
   can NEVER be forgotten — silently filtered out of `names`. These are
   contract surfaces the iteration loop re-binds every turn; dropping them
   would leave the sandbox in a torn state where *query* disappears
   mid-turn. If the LLM asks to forget one, we log it and move on."
  [sci-ctx names]
  (let [raw-syms (keep (fn [n]
                         (cond (symbol? n) n
                           (string? n) (try (symbol n) (catch Throwable _ nil))
                           :else       nil))
                   names)
        {system-syms true user-syms false} (group-by (comp boolean earmuffed-sym?) raw-syms)]
    (when (seq system-syms)
      (trove/log! {:level :info :id ::forget-system-var-refused
                   :data {:requested (mapv str system-syms)}
                   :msg "Refusing to forget SYSTEM vars (*foo*) — ignoring those names"}))
    (when (seq user-syms)
      (try
        (swap! (:env sci-ctx) update-in [:namespaces 'sandbox]
          (fn [ns-map] (apply dissoc ns-map user-syms)))
        (catch Throwable e
          (trove/log! {:level :debug :id ::forget-vars-failed
                       :data {:error (ex-message e) :syms (mapv str user-syms)}
                       :msg "forget-vars! failed — skipping"}))))))

;; =============================================================================
;; Deterministic Auto-Forget
;; =============================================================================

(def ^:const AUTO_FORGET_STALE_QUERIES
  "Number of recent queries a var must have been defined/redefined in to survive
   auto-forget. Vars without a docstring that were last touched more than this
   many queries ago are automatically removed from the live sandbox at the start
   of each new query. DB rows are untouched — (restore-var 'sym) can bring them back."
  3)

(defn auto-forget-candidates
  "Pure function. Returns the set of sandbox var symbols that should be
   auto-forgotten at the start of a new query.

   A var is a candidate for auto-forget when ALL of:
   1. It is a user var (not in `initial-ns-keys`)
   2. It has NO docstring (runtime SCI meta :doc is nil/blank)
   3. It was last defined/redefined in a query that is NOT among the
      `recent-query-ids` (last N queries of this conversation)

   Params:
   - `sandbox-map`      — SCI sandbox namespace map {symbol → value-or-var}
   - `initial-ns-keys`  — set of symbols that are built-in tools/helpers
   - `var-registry`     — result of `db-latest-var-registry`:
                           {symbol → {:query-id ... :value ... :code ...}}
   - `recent-query-ids` — set of query UUIDs for the last N queries

   Returns: set of symbols to forget."
  [sandbox-map initial-ns-keys var-registry recent-query-ids]
  (let [recent-ids (set recent-query-ids)]
    (into #{}
      (filter
        (fn [sym]
          (let [v (get sandbox-map sym)
                doc (:doc (meta v))
                has-doc? (and doc (not (str/blank? doc)))
                reg-entry (get var-registry sym)
                defining-query-id (:query-id reg-entry)]
            (and
              ;; Not a built-in
              (not (contains? initial-ns-keys sym))
              ;; Not an earmuffed SYSTEM var (*query*, *reasoning*, *answer*, …)
              (not (earmuffed-sym? sym))
              ;; No docstring — vars with docs are intentionally persisted
              (not has-doc?)
              ;; Has a registry entry (was persisted at some point)
              (some? reg-entry)
              ;; Was last defined in a query that's NOT recent
              (not (contains? recent-ids defining-query-id))))))
      (keys sandbox-map))))
(defn iteration-loop [rlm-env query
                      {:keys [output-spec max-context-tokens custom-docs system-prompt
                              pre-fetched-context query-ref user-messages
                              max-iterations max-consecutive-errors max-restarts
                              hooks cancel-atom current-iteration-atom
                              reasoning-default routing]}]
  (let [max-iterations (or max-iterations schema/MAX_ITERATIONS)
        max-consecutive-errors (or max-consecutive-errors 5)
        max-restarts (or max-restarts 3)
        ;; Adaptive budget: if rlm-env has a max-iterations-atom (set by query-env!),
        ;; read from it so the LLM can extend its own budget via (request-more-iterations n).
        ;; Otherwise use the static max-iterations parameter.
        max-iter-atom (:max-iterations-atom rlm-env)
        effective-max-iterations (fn [] (if max-iter-atom @max-iter-atom max-iterations))
        ;; Resolve effective model name for token counting
        effective-model (resolve-root-model (:router rlm-env))
        _ (assert effective-model "Router must resolve a root model — check provider config")
        ;; Default max-context-tokens to 60% of model's context window.
        ;; Prevents unbounded history accumulation (quadratic token growth over iterations).
        max-context-tokens (or max-context-tokens
                             (long (* 0.6 (router/context-limit effective-model))))
        ;; Check if root provider has native reasoning (thinking tokens)
        has-reasoning? (boolean (provider-has-reasoning? (:router rlm-env)))
        base-reasoning-level (or (normalize-reasoning-level reasoning-default)
                               reasoning-quick)
        has-docs? (when-let [db (:db-info rlm-env)]
                    (pos? (count (db-list-documents db {:limit 1 :include-toc? false}))))
        doc-summary (when (and has-docs? (:db-info rlm-env))
                      (build-document-summary (:db-info rlm-env)))
        ;; Git repos are read from SQLite on each iteration (NOT from an
        ;; atom) so persistent conversations resume with attached repos
        ;; intact. Empty list elides the GIT REPO block entirely from the
        ;; system prompt.
        git-repos (when-let [db (:db-info rlm-env)]
                    (rlm-db/db-list-repos db))
        concept-prompt (when-let [db (:db-info rlm-env)]
                         (ontology/concept-graph-for-prompt db))
        ;; Activation flags — mirror the per-tool activation-fns in
        ;; register-builtin-tools!. Must stay in sync with them: if a tool
        ;; group's activation-fn flips false, the corresponding prompt
        ;; section below should also flip off so the LLM doesn't see tool
        ;; references it can't actually call.
        has-conv?     (boolean (:conversation-ref rlm-env))
        has-concepts? (when-let [db (:db-info rlm-env)]
                        (try (boolean (seq (rlm-db/list-concepts db)))
                          (catch Throwable _ false)))
        system-prompt (build-system-prompt {:output-spec output-spec
                                            :custom-docs custom-docs
                                            :has-documents? has-docs?
                                            :document-summary doc-summary
                                            :has-conversation? has-conv?
                                            :has-concepts? has-concepts?
                                            :has-reasoning? has-reasoning?
                                            :system-prompt system-prompt
                                            :git-repos git-repos
                                            :concept-graph-prompt concept-prompt
                                            :max-context-tokens max-context-tokens
                                            :skill-registry (when-let [a (:skill-registry-atom rlm-env)] @a)})
        initial-user-content (str "{:requirement " (pr-str query)
                               (when pre-fetched-context
                                 (str "\n :plan " (pr-str pre-fetched-context)))
                               "}")
        ;; Build initial messages: system + structured context/requirement + original user messages (multimodal)
        initial-messages (into [{:role "system" :content system-prompt}
                                {:role "user" :content initial-user-content}]
                           (when (and user-messages
                                   (some #(sequential? (:content %)) user-messages))
                              ;; Include original multimodal messages (images etc.) as additional context
                             user-messages))
        ;; Store initial messages if history tracking is enabled
        db-info (:db-info rlm-env)
        ;; ── Auto-forget stale vars ──────────────────────────────────────
        ;; Deterministic cleanup at query boundary: remove sandbox vars that
        ;; (a) have no docstring, and (b) were last defined/redefined more
        ;; than AUTO_FORGET_STALE_QUERIES queries ago. This replaces the
        ;; unreliable "ask LLM to emit :forget" pattern for scratch vars.
        ;; DB rows are untouched — (restore-var 'sym) can bring them back.
        _ (when (and db-info (:conversation-ref rlm-env) (:sci-ctx rlm-env))
            (try
              (let [all-queries (sort-by :created-at
                                  (rlm-db/db-list-conversation-queries db-info (:conversation-ref rlm-env)))
                    recent-ids (into #{}
                                 (map :id)
                                 (take-last AUTO_FORGET_STALE_QUERIES all-queries))
                    var-registry (rlm-db/db-latest-var-registry db-info (:conversation-ref rlm-env))
                    sandbox-map (get-in @(:env (:sci-ctx rlm-env)) [:namespaces 'sandbox])
                    candidates (auto-forget-candidates sandbox-map (:initial-ns-keys rlm-env)
                                 var-registry recent-ids)]
                (when (seq candidates)
                  (trove/log! {:level :info :id ::auto-forget
                               :data {:forgotten (mapv str candidates) :count (count candidates)}
                               :msg (str "Auto-forget: evicting " (count candidates) " stale vars without docstrings")})
                  (forget-vars! (:sci-ctx rlm-env) candidates)
                  (when-let [via (:var-index-atom rlm-env)]
                    (swap! via update :current-revision inc))))
              (catch Exception e
                (trove/log! {:level :warn :id ::auto-forget-failed
                             :data {:error (ex-message e)}
                             :msg "Auto-forget failed — skipping"}))))
        ;; Cost tracking: accumulate token usage across all iterations
        usage-atom (atom {:input-tokens 0 :output-tokens 0 :reasoning-tokens 0 :cached-tokens 0})
        accumulate-usage! (fn [api-usage]
                            (when api-usage
                              (swap! usage-atom
                                (fn [acc]
                                  (-> acc
                                    (update :input-tokens + (or (:prompt_tokens api-usage) 0))
                                    (update :output-tokens + (or (:completion_tokens api-usage) 0))
                                    (update :reasoning-tokens + (or (get-in api-usage [:completion_tokens_details :reasoning_tokens]) 0))
                                    (update :cached-tokens + (or (get-in api-usage [:prompt_tokens_details :cached_tokens]) 0)))))))
        ;; Repetition detection: track individual call→result pairs across iterations
        call-counts-atom (atom {})  ;; {[code result-str] count}
        detect-repetition (fn [executions]
                            (let [pairs (mapv (fn [e] [(:code e) (truncate (str (:result e)) 200)]) executions)
                                  counts (swap! call-counts-atom
                                           (fn [m] (reduce (fn [acc p] (update acc p (fnil inc 0))) m pairs)))
                                  repeated (->> pairs
                                             (filter #(>= (get counts % 0) 3))
                                             (map first))]
                              (when (seq repeated)
                                (str "\n\n⚠ REPETITION DETECTED: These calls have been executed 3+ times with the SAME results:\n"
                                  (str/join "\n" (map #(str "  - " (truncate (str %) 80)) (distinct repeated)))
                                  "\nRepeating the same action will NOT produce different results. "
                                  "You MUST try a DIFFERENT approach, or call \final\": {\"answer\": \"your answer\", \"confidence\": \"high\"} with what you have."))))
        finalize-cost (fn []
                        (let [{:keys [input-tokens output-tokens reasoning-tokens cached-tokens]} @usage-atom
                              total-tokens (+ input-tokens output-tokens)
                              cost (router/estimate-cost effective-model input-tokens output-tokens)]
                          {:tokens {:input input-tokens :output output-tokens
                                    :reasoning reasoning-tokens :cached cached-tokens
                                    :total total-tokens}
                           :cost cost}))
        ;; Cache var-index by env-level execution revision so it survives across queries.
        ;; SCI may keep stable sandbox map identity across (def ...) updates.
        ;; Use grouped :var-index-atom when available (new layout), fall back to local atom.
        ;; Layout: {:index built-idx :revision last-cache-build-rev :current-revision live-rev}
        ;;   :current-revision — bumped on every SCI mutation (was var-index-revision-atom)
        ;;   :revision         — revision at which :index was last built (was :revision in cache)
        ;; Cache hit: (:revision vi) == (:current-revision vi)
        var-index-atom (or (:var-index-atom rlm-env) (atom {:index nil :revision -1 :current-revision 0}))
        get-var-index (fn []
                        (let [{:keys [index revision current-revision]} @var-index-atom]
                          (if (= revision current-revision)
                            index
                            (let [sandbox-map (get-in @(:env (:sci-ctx rlm-env)) [:namespaces 'sandbox])
                                  idx (build-var-index (:sci-ctx rlm-env) (:initial-ns-keys rlm-env) sandbox-map
                                        (:db-info rlm-env) (:conversation-ref rlm-env))
                                  live-rev (:current-revision @var-index-atom)]
                              (swap! var-index-atom assoc :index idx :revision live-rev)
                              idx))))
        on-chunk (:on-chunk hooks)
        on-iteration (:on-iteration hooks)
        on-cancel (:on-cancel hooks)
        emit-hook! (fn [hook-fn payload log-msg]
                     (when hook-fn
                       (try
                         (hook-fn payload)
                         (catch Exception e
                           (trove/log! {:level :warn :data {:error (ex-message e)}
                                        :msg log-msg})))))]
    ;; query-start is logged in query.clj — don't duplicate
    ;; Auto-bind *query* into the SCI sandbox so the LLM (and var-history)
    ;; can always see the current user query. Updated once per query.
    (rlm-tools/sci-update-binding! (:sci-ctx rlm-env) '*query* query)
    (binding [*rlm-ctx* (merge *rlm-ctx* {:rlm-phase :iteration-loop})]
      (loop [iteration 0 messages initial-messages trace [] consecutive-errors 0 restarts 0
             prev-executions nil prev-iteration -1
             prev-next-model nil prev-next-reasoning nil]
        (when current-iteration-atom
          (reset! current-iteration-atom iteration))
        (cond
          ;; Cooperative cancellation — caller-owned :cancel-atom from query-env!
          ;; (or an internal per-query atom when not supplied).
          (when cancel-atom @cancel-atom)
          (do (rlm-stage! :error iteration {:reason :cancelled})
            (emit-hook! on-cancel {:iteration iteration
                                   :status :cancelled
                                   :status-id (status->id :cancelled)}
              "on-cancel hook threw — swallowing")
            (merge {:answer nil
                    :status :cancelled
                    :status-id (status->id :cancelled)
                    :trace trace
                    :iterations iteration}
              (finalize-cost)))

          (>= iteration (effective-max-iterations))
          (let [debug? (:rlm-debug? *rlm-ctx*)
                locals (when debug? (get-locals rlm-env))]
            (rlm-stage! :error iteration {:reason :max-iterations :max (effective-max-iterations)})
            (merge {:answer nil
                    :status :max-iterations
                    :status-id (status->id :max-iterations)
                    :trace trace
                    :iterations iteration}
              (when debug? {:locals locals})
              (finalize-cost)))

          :else
          (if (>= consecutive-errors max-consecutive-errors)
            ;; Strategy restart: instead of terminating, reset with anti-knowledge
            (if (< restarts max-restarts)
              (let [failed-summary (->> trace
                                     (filter :error)
                                     (take 3)
                                     (map #(str "- " (get-in % [:error :message] (str (:error %)))))
                                     (str/join "\n"))
                    restart-hint (str "{:strategy-restart true\n"
                                   " :errors " (pr-str failed-summary) "\n"
                                   " :instruction \"Start fresh with a DIFFERENT strategy. Do NOT repeat the same approach. Consider: different search terms, different tools, different data access pattern.\"\n"
                                   " :requirement " (pr-str query) "}")
                    restart-messages [{:role "system" :content system-prompt}
                                      {:role "user" :content restart-hint}]]
                (trove/log! {:level :info :data {:iteration iteration :restarts (inc restarts)
                                                 :errors consecutive-errors}
                             :msg "Strategy restart — resetting with anti-knowledge"})
                (rlm-debug! {:failed-summary failed-summary} "Strategy restart triggered")
                (recur (inc iteration) restart-messages trace 0 (inc restarts)
                  nil -1 nil nil))
              (do (trove/log! {:level :warn :data {:iteration iteration :consecutive-errors consecutive-errors
                                                   :restarts restarts}
                               :msg "Error budget exhausted — too many consecutive errors across restarts. Simplify your code or break the task into smaller steps."})
                (merge {:answer nil :status :error-budget-exhausted :trace trace :iterations iteration}
                  {:status-id (status->id :error-budget-exhausted)}
                  (finalize-cost))))
            (let [;; Prefer the LLM's own next-iteration preference, fall back to
                  ;; error-driven escalation when it hasn't asked for one.
                  reasoning-level (when has-reasoning?
                                    (or prev-next-reasoning
                                      (reasoning-level-for-errors base-reasoning-level consecutive-errors)))
                  _ (rlm-stage! :iter-start iteration {:msg-count (count messages) :reasoning reasoning-level})
                  ;; Build single-shot prompt: system + user-query + <journal> (prev iter
                  ;; execution results) + <var_index>. No accumulating multi-iteration
                  ;; journal — the LLM pulls older turns via history tools on demand.
                  var-index-str (get-var-index)
                  exec-results-str (format-execution-results prev-executions prev-iteration)
                  iteration-context (str
                                      (when exec-results-str (str "\n" exec-results-str))
                                      (when var-index-str
                                        (str "\n<var_index>\n" var-index-str "\n</var_index>")))
                  base-messages (vec (take 2 messages)) ;; [system-prompt, user-query]
                  effective-messages (cond-> base-messages
                                       (not (str/blank? iteration-context))
                                       (conj {:role "user" :content iteration-context}))
                   ;; Carry the latest non-nil thinking/code across chunks so
                   ;; the terminal `:done? true` chunk doesn't blank them out.
                   ;; svar streams partial results, then fires one final done=true
                   ;; with tokens/cost but no payload — downstream consumers
                   ;; (e.g. web live-status) overwrote the streamed thinking
                   ;; with nil and the UI lost it. Hold onto the last seen
                   ;; values in a closed-over atom and re-emit them on done.
                   chunk-state (atom {:thinking nil :code nil})
                   iter-on-chunk (when on-chunk
                                   (fn [{:keys [result reasoning tokens cost done?]}]
                                     (let [streamed-thinking (or reasoning (:thinking result))
                                           streamed-code (when-let [c (:code result)]
                                                           (when (sequential? c) (vec c)))
                                           state (swap! chunk-state
                                                   (fn [s]
                                                     (cond-> s
                                                       (some? streamed-thinking) (assoc :thinking streamed-thinking)
                                                       (some? streamed-code)     (assoc :code streamed-code))))]
                                       (if done?
                                         (on-chunk {:iteration iteration
                                                    :thinking (:thinking state)
                                                    :code (:code state)
                                                    :final nil
                                                    :tokens tokens
                                                    :cost cost
                                                    :done? true})
                                         (on-chunk {:iteration iteration
                                                    :thinking (:thinking state)
                                                    :code (:code state)
                                                    :final nil
                                                    :tokens nil
                                                    :cost nil
                                                    :done? false})))))
                   ;; Effective routing = caller's base `:routing` + LLM's
                   ;; `:next.model` override on `:optimize`. Caller's other
                   ;; routing keys (`:provider`, `:model`, etc.) survive.
                   effective-routing (cond-> (or routing {})
                                       prev-next-model (assoc :optimize prev-next-model))
                   iteration-result (try
                                      (run-iteration rlm-env effective-messages
                                        (cond-> {:iteration-spec (if has-reasoning?
                                                                   ITERATION_SPEC_REASONING
                                                                   ITERATION_SPEC_NON_REASONING)
                                                 :iteration iteration
                                                 :reasoning-level reasoning-level
                                                 :routing effective-routing}
                                          iter-on-chunk (assoc :on-chunk iter-on-chunk)))
                                     (catch Exception e
                                       (let [ex-data-map (ex-data e)
                                             err-msg (ex-message e)
                                             err-type (:type ex-data-map)
                                             ;; Infrastructure errors (provider down, auth failures) are NOT
                                             ;; recoverable by the LLM — re-throw immediately instead of
                                             ;; burning iterations feeding the error back to a dead provider.
                                             infra-error? (contains? #{:svar.llm/all-providers-exhausted
                                                                       :svar.llm/circuit-open
                                                                       :svar.llm/provider-exhausted}
                                                            err-type)]
                                         (when infra-error?
                                           (trove/log! {:level :error
                                                        :data {:iteration iteration :error err-msg :type err-type}
                                                        :msg "Provider infrastructure error — aborting iteration loop"})
                                           (throw e))
                                         (let [cause (.getCause ^Throwable e)
                                               stack (mapv str (take 12 (.getStackTrace ^Throwable e)))
                                               iter-err {:message err-msg
                                                         :type err-type
                                                         :class (.getName (class e))
                                                         :data (when (seq ex-data-map)
                                                                 (dissoc ex-data-map :type))
                                                         :cause (when cause
                                                                  {:message (.getMessage ^Throwable cause)
                                                                   :class (.getName (class cause))})
                                                         :stack stack}]
                                           (trove/log! {:level :warn
                                                        :data {:iteration iteration :error err-msg :type err-type}
                                                        :msg "RLM iteration failed, feeding error to LLM"})
                                           ;; Return ::iteration-error sentinel — loop will feed error to LLM
                                           {::iteration-error iter-err}))))]
              (if-let [iter-err (::iteration-error iteration-result)]
                ;; Error path: feed error back to LLM as user message, let it recover
                (let [error-feedback (str "[Iteration " (inc iteration) "/" (effective-max-iterations) "]\n"
                                       "<error>LLM call failed: " (:message iter-err) "</error>\n"
                                       "The previous attempt failed. Adjust your approach or call \final\": {\"answer\": \"your answer\", \"confidence\": \"high\"} with what you have.")
                      trace-entry {:iteration iteration :error iter-err :final? false}]
                  ;; Store error iteration snapshot — full ex-data/type/stack so the UI
                  ;; (and fine-tuning corpus) can inspect why the iteration blew up.
                  (rlm-db/store-iteration! db-info
                    {:query-ref query-ref
                     :vars []
                     :executions nil :thinking nil :duration-ms 0
                     :error iter-err})
                  ;; Global observer hook after store-iteration! (error path)
                  (emit-hook! on-iteration
                    {:iteration iteration
                     :status :error
                     :status-id (status->id :error)
                     :thinking nil
                     :executions nil
                     :final-result nil
                     :error iter-err
                     :duration-ms 0}
                    "on-iteration hook threw (error branch) — swallowing")
                  (recur (inc iteration)
                    (conj messages {:role "user" :content error-feedback})
                    (conj trace trace-entry)
                    (inc consecutive-errors)
                    restarts
                    nil -1 nil nil))
                ;; Normal path — accumulate token usage
                (let [_ (accumulate-usage! (:api-usage iteration-result))
                      {:keys [thinking executions final-result next-model next-reasoning forget]} iteration-result
                      _ (when (seq forget)
                          (forget-vars! (:sci-ctx rlm-env) forget)
                          (swap! var-index-atom update :current-revision inc))
                      ;; Auto-bind *reasoning* into the SCI sandbox after every iteration.
                      ;; The LLM (and var-history) can inspect prior reasoning via (var-history '*reasoning*).
                      _ (when (seq thinking)
                          (rlm-tools/sci-update-binding! (:sci-ctx rlm-env) '*reasoning* thinking))
                      ;; Auto-bind *answer* when the turn finalizes. Survives into the
                      ;; next turn in this conversation (vars persist across queries).
                      ;; (var-history '*answer*) → every prior turn's answer; most
                      ;; recent lives under the bare *answer* var.
                      final-answer (when final-result (:answer final-result))
                      _ (when final-result
                          (rlm-tools/sci-update-binding! (:sci-ctx rlm-env) '*answer* final-answer))
                      vars-snapshot (restorable-var-snapshots rlm-env executions)
                      ;; Inject auto-vars (*query*, *reasoning*, *answer*) into the
                      ;; persisted snapshot so var-history can track them across
                      ;; queries and iterations.
                      ;; SYSTEM vars behave like any other SCI var at read time
                      ;; — executable, callable, printable. What makes them
                      ;; SYSTEM is that the iteration loop BINDS them every
                      ;; turn and forget-vars! refuses to drop them. There's
                      ;; no (def …) source for the binding itself, so :code
                      ;; is a marker string, not evaluable code.
                      vars-snapshot (cond-> vars-snapshot
                                      ;; Persist *query* on the FIRST iteration only (once per query)
                                      (zero? iteration)
                                      (conj {:name "*query*" :value query :code ";; SYSTEM var — bound by agent loop, never forgotten"})
                                      ;; Persist *reasoning* whenever thinking is present
                                      (seq thinking)
                                      (conj {:name "*reasoning*" :value thinking :code ";; SYSTEM var — bound by agent loop, never forgotten"})
                                      ;; Persist *answer* on turn finalize
                                      final-result
                                      (conj {:name "*answer*" :value final-answer :code ";; SYSTEM var — bound by agent loop, never forgotten"}))
                      ;; Store iteration snapshot — exact input/output for fine-tuning
                      _traj-iter (rlm-db/store-iteration! db-info
                                   {:query-ref query-ref
                                    :executions executions
                                    :vars vars-snapshot
                                    :thinking thinking
                                    :answer (when final-result (answer-str (:answer final-result)))
                                    :duration-ms (or (:duration-ms iteration-result) 0)})
                      ;; Global observer hook after store-iteration! (success/empty/final)
                      _ (emit-hook! on-iteration
                          {:iteration iteration
                           :status (cond
                                     final-result :final
                                     (empty? executions) :empty
                                     :else :success)
                           :status-id (status->id (cond
                                                    final-result :final
                                                    (empty? executions) :empty
                                                    :else :success))
                           :thinking thinking
                           :executions executions
                           :final-result final-result
                           :error nil
                           :duration-ms (or (:duration-ms iteration-result) 0)}
                          "on-iteration hook threw (success branch) — swallowing")
                      trace-entry {:iteration iteration
                                   :thinking thinking
                                   :executions executions
                                   :final? (boolean final-result)}]
                  (if final-result
                    (do (rlm-stage! :final iteration
                          {:answer (truncate (answer-str (:answer final-result)) 200)
                           :confidence (:confidence final-result)
                           :iterations (inc iteration)})
                      (rlm-stage! :iter-end iteration
                        {:blocks (count executions)
                         :errors (count (filter :error executions))
                         :times (mapv :execution-time-ms executions)})
                        ;; Fire final streaming callback
                      (when on-chunk
                        (on-chunk {:iteration iteration
                                   :thinking thinking
                                   :code (mapv :code executions)
                                   :final {:answer (:answer final-result)
                                           :confidence (:confidence final-result)
                                           :summary (:summary final-result)
                                           :iterations (inc iteration)
                                           :status :success}
                                   :done? true}))
                        ;; Final result persisted via store-iteration! with :answer
                      (merge (cond-> {:answer (:answer final-result)
                                      :trace (conj trace trace-entry)
                                      :iterations (inc iteration)
                                      :confidence (:confidence final-result)}
                               (:sources final-result)   (assoc :sources (:sources final-result))
                               (:reasoning final-result) (assoc :reasoning (:reasoning final-result)))
                        (finalize-cost)))
                     (if (empty? executions)
                       ;; Empty iteration: the iteration counter still
                       ;; advances (see `(recur (inc iteration) …)` below)
                       ;; so the budget IS being consumed. Include the same
                       ;; last-iteration nudge here — otherwise a stubborn
                       ;; model can spend its final slot on another empty
                       ;; response and the loop terminates silently.
                       (let [_ (rlm-stage! :empty iteration {})
                             current-max (effective-max-iterations)
                             remaining-iters (- current-max (inc iteration))
                             last-iter? (zero? remaining-iters)
                             nudge (str "[Iteration " (inc iteration) "/" current-max "]\n"
                                     "{:requirement " (pr-str (truncate query 200)) "}\n"
                                     "⚠ EMPTY — no code executed. You MUST include code. "
                                     (if has-reasoning?
                                       "Respond with code or set final to finish."
                                       "Respond with thinking + code, or set final to finish.")
                                     (when last-iter?
                                       (str "\n[SYSTEM_NUDGE] ‼ THIS IS YOUR LAST ITERATION ‼ "
                                         "Emit :final NOW or call (request-more-iterations N) — "
                                         "the loop terminates after this turn.")))]
                        ;; Store empty iteration snapshot
                        (rlm-db/store-iteration! db-info
                          {:query-ref query-ref
                           :vars []
                           :executions nil :thinking thinking :duration-ms (or (:duration-ms iteration-result) 0)})
                        (recur (inc iteration) ;; still increment to prevent infinite loop
                          (conj messages
                            {:role "assistant" :content (or thinking "[empty]")}
                            {:role "user" :content nudge})
                          trace ;; DON'T add empty trace entry
                          (inc consecutive-errors)
                          restarts
                          nil -1 next-model next-reasoning))
                       ;; Normal iteration with executions
                       (let [exec-feedback (format-executions executions)
                             current-max (effective-max-iterations)
                             iteration-header (str "[Iteration " (inc iteration) "/" current-max "]\n"
                                                "{:requirement " (pr-str (truncate query 200)) "}")
                             repetition-warning (detect-repetition executions)
                             remaining-iters (- current-max (inc iteration))
                             ;; Budget-warning scales with the current budget (max/3, min 1).
                             ;; With max=10 → nudge from iter 7 onward. With max=50 → from iter 34.
                             ;; Keeps the nudge proportional for short budgets AND big ones.
                             warn-threshold (max 1 (long (/ current-max 3)))
                             budget-warning (when (and (<= remaining-iters warn-threshold)
                                                    (pos? remaining-iters))
                                              (str "\n[SYSTEM_NUDGE] Only " remaining-iters
                                                " iteration" (when (not= 1 remaining-iters) "s")
                                                " left! Set final NOW with what you have. "
                                                "DO NOT start new explorations. "
                                                "If you genuinely need more, call "
                                                "(request-more-iterations N) this iteration."))
                             ;; Last-iteration nudge — unambiguous, no mistaking it for a soft warning.
                             ;; Fires when this is the FINAL iteration the loop will run (remaining = 0).
                             last-iteration-nudge (when (zero? remaining-iters)
                                                    (str "\n[SYSTEM_NUDGE] ‼ THIS IS YOUR LAST ITERATION ‼ "
                                                      "You MUST set :final on this turn — the loop will terminate "
                                                      "after this response with no further chances. "
                                                      "If the answer is genuinely incomplete and more work is required, "
                                                      "call (request-more-iterations N) NOW to extend the budget; "
                                                      "otherwise emit :final with your best current answer."))
                             user-feedback (str iteration-header "\n" exec-feedback
                                             repetition-warning budget-warning last-iteration-nudge)]
                        (rlm-stage! :iter-end iteration
                          {:blocks (count executions)
                           :errors (count (filter :error executions))
                           :times (mapv :execution-time-ms executions)})
                        (let [had-successful-execution? (some #(nil? (:error %)) executions)
                              next-errors (if had-successful-execution? 0 (inc consecutive-errors))
                              _ (when had-successful-execution?
                                  (swap! var-index-atom update :current-revision inc))]
                          (recur (inc iteration)
                            messages
                            (conj trace trace-entry)
                            next-errors
                            restarts
                            executions iteration
                            next-model next-reasoning))))))))))))))

;; =============================================================================
;; Public API — environment lifecycle, tool registration
;; =============================================================================
;; Placed after iteration-loop so all internal fns are resolved without declare.

(defn create-env
  "Creates an RLM environment (component) for document ingestion and querying.

   The environment holds:
   - In-memory store for documents and conversation history
   - LLM configuration for queries
   - SCI sandbox context with custom bindings

   Params:
   router - Required. Router from llm/make-router, pre-built.
   opts - Map with :db and optional :conversation.
   :db accepted forms:
     nil               - no DB (SCI-only execution)
     :temp             - ephemeral SQLite DB
     path string       - persistent SQLite DB at path
     {:path p}         - persistent SQLite DB at path
     {:datasource ds}  - caller-owned DataSource (not closed on dispose)

   Returns:
   RLM environment map (component). Pass to register-env-fn!, register-env-def!, ingest-to-env!, query-env!, dispose-env!."
  [router {:keys [db conversation]}]
  (when-not router
    (anomaly/incorrect! "Missing router" {:type :rlm/missing-router}))
  (let [depth-atom (atom 0)
        tool-registry-atom (atom {})
        db-info (create-rlm-conn db)
        var-index-atom (atom {:index nil :revision -1 :current-revision 0})
        qa-corpus-atom (atom {:snapshot-cache nil
                              :stats {:hits 0 :misses 0
                                      :last-digest-ms nil
                                      :last-revision 0}})
        skill-registry-map (rlm-skills/load-skills {})
        _ (when db-info
            (rlm-skills/ingest-skills! db-info skill-registry-map))
        state-atom (atom {:custom-bindings {}
                          :custom-docs []
                          :skill-registry skill-registry-map
                          :rlm-env nil
                          :conversation-ref nil})
        rlm-env-atom (atom nil)
        skill-registry-atom (atom skill-registry-map)
        ;; Single sub-rlm tool — LLM decides routing per-call via `:routing` opt.
        ;; Previously we forked a `cheap-sub-rlm-query-fn` with `{:optimize :cost}`
        ;; baked in and shoved that into the SCI sandbox; that stole agency from
        ;; the model. Now the model gets the neutral fn and picks cost/speed/
        ;; intelligence itself when the task calls for it.
        sub-rlm-query-fn (make-routed-sub-rlm-query-fn
                           {} depth-atom router skill-registry-atom rlm-env-atom
                           iteration-loop)
        env-id (str (util/uuid))
        root-model (or (resolve-root-model router) "unknown")
        has-reasoning? (boolean (provider-has-reasoning? router))
        system-prompt (build-system-prompt {:has-reasoning? has-reasoning?
                                            :skill-registry skill-registry-map})
        resolved-conversation-ref (rlm-db/db-resolve-conversation-ref db-info conversation)
        conversation-ref (or resolved-conversation-ref
                           (rlm-db/store-conversation! db-info
                             {:model root-model
                              :system-prompt system-prompt}))
        {:keys [sci-ctx sandbox-ns initial-ns-keys]}
        (create-sci-context sub-rlm-query-fn db-info
          conversation-ref
          (:custom-bindings @state-atom))
        env {:env-id env-id
             :conversation-ref conversation-ref
             :depth-atom depth-atom
             :tool-registry-atom tool-registry-atom
             :db-info db-info
             :var-index-atom var-index-atom
             :qa-corpus-atom qa-corpus-atom
             :state-atom state-atom
             :skill-registry-atom skill-registry-atom
             :sci-ctx sci-ctx
             :sandbox-ns sandbox-ns
             :initial-ns-keys initial-ns-keys
             :router router
             :sub-rlm-query-fn sub-rlm-query-fn}]
    (reset! rlm-env-atom env)
    (swap! state-atom assoc :rlm-env env :conversation-ref conversation-ref)
    ;; Register all built-in tools with proper activation-fns
    (let [env (or (rlm-tools/register-builtin-tools! env) env)]
      (reset! rlm-env-atom env)
      env)))

(defn dispose-env!
  "Disposes an RLM environment and releases resources.

   For persistent DBs (created with :path), data is preserved.
   For disposable DBs, all data is deleted."
  [env]
  (when-let [db-info (:db-info env)]
    (dispose-rlm-conn! db-info)))

(defn register-env-fn!
  "Registers a function in the RLM environment's SCI sandbox."
  [env sym f tool-def]
  (when-not (:state-atom env)
    (anomaly/incorrect! "Invalid RLM environment" {:type :rlm/invalid-env}))
  (when-not (symbol? sym)
    (anomaly/incorrect! "sym must be a symbol" {:type :rlm/invalid-sym :sym sym}))
  (when-not (fn? f)
    (anomaly/incorrect! "f must be a function" {:type :rlm/invalid-fn}))
  (when-not (map? tool-def)
    (anomaly/incorrect! "tool-def must be a map" {:type :rlm/invalid-tool-def}))
  (let [canonical-tool-def (sci-tool/make-tool-def sym f tool-def)]
    (rlm-tools/register-tool-def! (:tool-registry-atom env) sym canonical-tool-def)
    (when-let [sci-ctx (:sci-ctx env)]
      (rlm-tools/sci-update-binding! sci-ctx sym
        (rlm-tools/wrap-tool-for-sci env sym f (:tool-registry-atom env))))
    (swap! (:state-atom env) update :custom-docs conj (dissoc canonical-tool-def :fn)))
  env)

(defn register-env-def!
  "Registers a constant/value in the RLM environment's SCI sandbox."
  [env sym value tool-def]
  (when-not (:state-atom env)
    (anomaly/incorrect! "Invalid RLM environment" {:type :rlm/invalid-env}))
  (when-not (symbol? sym)
    (anomaly/incorrect! "sym must be a symbol" {:type :rlm/invalid-sym :sym sym}))
  (when-not (map? tool-def)
    (anomaly/incorrect! "tool-def must be a map" {:type :rlm/invalid-tool-def}))
  (swap! (:state-atom env) update :custom-bindings assoc sym value)
  (when-let [sci-ctx (:sci-ctx env)]
    (rlm-tools/sci-update-binding! sci-ctx sym value))
  (swap! (:state-atom env) update :custom-docs conj (assoc tool-def :type :def :sym sym))
  env)

(defn register-hook!
  "Attach a hook to an existing tool's chain."
  [env sym {:keys [stage id fn]}]
  (rlm-tools/register-tool-def! (:tool-registry-atom env) sym
    {stage [{:id id :fn fn}]})
  env)

(defn unregister-hook!
  "Remove a per-tool hook entry by :id."
  [env sym stage id]
  (rlm-tools/unregister-hook! (:tool-registry-atom env) sym stage id))

(defn list-tool-hooks
  "Return hook chains registered for `sym`, or nil when missing."
  [env sym]
  (rlm-tools/list-tool-hooks (:tool-registry-atom env) sym))

(defn list-registered-tools
  "Return a vec of {:sym :hook-counts} maps for registered tools."
  [env]
  (rlm-tools/list-registered-tools (:tool-registry-atom env)))

;; =============================================================================
;; Entity Extraction — delegates to loop.knowledge.entity
;; =============================================================================

(def extract-entities-from-page! rlm-entity/extract-entities-from-page!)
(def extract-entities-from-visual-node! rlm-entity/extract-entities-from-visual-node!)
(def extract-entities-from-document! rlm-entity/extract-entities-from-document!)

