(ns com.blockether.vis.loop.core
  (:require
   [clojure.string :as str]
   [com.blockether.anomaly.core :as anomaly]
   [com.blockether.svar.internal.llm :as llm]
   [com.blockether.svar.internal.router :as router]
   [com.blockether.svar.internal.util :as util]

   [com.blockether.vis.persistance.core :as db
    :refer [create-rlm-conn dispose-rlm-conn!]]
   [com.blockether.vis.persistance.spec :as rlm-spec
    :refer [ITERATION_SPEC_NON_REASONING ITERATION_SPEC_REASONING
            *eval-timeout-ms*
            *rlm-ctx*]]


   [com.blockether.vis.loop.runtime.conversation.environment.core :as sci-env]
   [com.blockether.vis.loop.runtime.conversation.environment.extension :as ext]
   [com.blockether.vis.loop.runtime.shared :as rt-shared :refer [realize-value truncate]]
   [com.blockether.vis.loop.mustache :as mustache]

   [edamame.core :as edamame]
   [sci.core :as sci]
   [taoensso.telemere :as tel]))

;; `bump-and-detect-repetition` moved to iteration/core.clj nudges
;; where every per-iteration SYSTEM_NUDGE composer lives (built-in and
;; extension-contributed).

(defn- log-stage!
  [stage iteration data]
  (tel/log! {:level :info :data (merge {:stage stage :iteration iteration} data)}))

(defn- status->id
  [status]
  (when status
    (keyword "rlm.status" (name status))))

(def ^:private quick-reasoning :quick)
(def ^:private balanced-reasoning :balanced)
(def ^:private deep-reasoning :deep)

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
    (= 1 (long consecutive-errors)) (if (= base quick-reasoning) balanced-reasoning deep-reasoning)
    :else deep-reasoning))

(defn- resolve-effective-model
  "Best-effort root model descriptor from router config."
  [router]
  (first (mapcat :models (:providers router))))

(defn- provider-has-reasoning?
  "True when the configured root model exposes reasoning support."
  [router]
  (boolean (:reasoning? (resolve-effective-model router))))


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
      (tel/log! {:level :warn :id ::get-locals-fallback
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
;; =============================================================================
;; Code Evaluation
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
   :readers (fn [_tag] (fn [val] (list 'do val)))})

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
    (let [_bal 0
          start-time (System/currentTimeMillis)
          lint-error (detect-common-mistakes code)]
      (if lint-error
        {:result nil :stdout "" :stderr "" :error lint-error
         :execution-time-ms 0 :timeout? false}
        (if-let [parse-error (parse-clojure-syntax code)]
          (let [repaired (try code (catch Throwable _ code))]
            (if (and (not= repaired code) (nil? (parse-clojure-syntax repaired)))
              (let [result (run-sci-code sci-ctx repaired :sandbox-ns sandbox-ns)
                    t (- (System/currentTimeMillis) start-time)]
                (assoc result :execution-time-ms t :timeout? false :repaired? true))
              {:result nil :stdout "" :stderr "" :error parse-error
               :execution-time-ms 0 :timeout? false}))
        ;; Normal execution path
          (let [execution-result (if timeout-ms
                                   (binding [*eval-timeout-ms* (rlm-spec/clamp-eval-timeout-ms timeout-ms)]
                                     (run-sci-code sci-ctx code :sandbox-ns sandbox-ns))
                                   (run-sci-code sci-ctx code :sandbox-ns sandbox-ns))
                execution-time (- (System/currentTimeMillis) start-time)]
            (if (:timeout? execution-result)
              (assoc execution-result :execution-time-ms execution-time :timeout? true)
              (let [{:keys [error]} execution-result
                    final-result (if (and error false)
                                   (try
                                     (let [repaired code]
                                       (if (= repaired code)
                                         (do (tel/log! {:level :debug :id ::repair-noop
                                                          :data {:code-len (count code) :error error}
                                                          :msg "Paren repair: no change needed"})
                                           execution-result)
                                         (let [retry (run-sci-code sci-ctx repaired :sandbox-ns sandbox-ns)]
                                           (if (:error retry)
                                             (do (tel/log! {:level :warn :id ::repair-retry-failed
                                                              :data {:original-error error
                                                                     :retry-error (:error retry)
                                                                     :code-len (count code)
                                                                     :repaired-len (count repaired)
                                                                     :added-chars (- (count repaired) (count code))
                                                                     :repaired-tail (subs repaired (max 0 (- (count repaired) 80)))}
                                                              :msg "Paren repair changed code but retry still failed"})
                                               execution-result)
                                             (do
                                               (tel/log! {:level :info :id ::repair-applied
                                                            :data {:original code :repaired repaired :sci-error error}
                                                            :msg "Paren repair applied successfully"})
                                               (assoc retry :repaired? true))))))
                                     (catch Throwable _
                                       execution-result))
                                   execution-result)]
                (assoc final-result :execution-time-ms execution-time :timeout? false)))))))))

(defn answer-str
  "Extracts a string representation from an RLM answer.
   Answer is always a string now."
  [answer]
  (if (string? answer) answer (pr-str answer)))

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

(defn- data-literal-form?
  "True when `form` is a literal data payload rather than executable code."
  [form]
  (or (map? form)
    (vector? form)
    (set? form)
    (keyword? form)
    (number? form)
    (boolean? form)
    (nil? form)
    (char? form)
    (instance? java.util.regex.Pattern form)
    (and (seq? form)
      (= 'quote (first form))
      (= 2 (count form)))))

(defn- bare-data-literal-shape?
  "Cheap fallback for malformed literal dumps that fail full parsing.
   This intentionally catches obvious payload shapes the model should never
   emit directly in :code: maps, vectors, sets, keywords, quoted data, and
   scalar literals."
  [expr]
  (let [s (str/trim (str expr))]
    (or (str/starts-with? s "{")
      (str/starts-with? s "[")
      (str/starts-with? s "#{")
      (str/starts-with? s ":")
      (str/starts-with? s "'{")
      (str/starts-with? s "'[")
      (str/starts-with? s "'#{")
      (str/starts-with? s "':")
      (re-matches #"[-+]?(?:\d+(?:\.\d+)?|\.\d+)(?:[eE][-+]?\d+)?" s)
      (= s "true")
      (= s "false")
      (= s "nil"))))

(defn bare-data-literal-code-block?
  "True when a code expression is just a single literal data form.
   Raw data belongs in :answer, or behind `(def result ...)` when the model
   wants to reuse it later in the same turn."
  [expr]
  (or (try
        (let [forms (check-syntax (str expr))]
          (and (= 1 (count forms))
            (data-literal-form? (first forms))))
        (catch Throwable _ false))
    (bare-data-literal-shape? expr)))

(defn- comment-only-block?
  "True when the expr parses to zero forms — i.e. it contains only `;;`
   line comments and/or `#_` discards. SCI evaluates such a block to nil,
   which is indistinguishable from a legitimately-nil result. Better to
   reject it explicitly so the LLM gets actionable feedback."
  [^String expr]
  (try
    (zero? (count (edamame/parse-string-all (str/trim expr) edamame-opts)))
    (catch Throwable _ false)))

(defn literal-code-block-error
  "Returns a validation error when `expr` is literal payload incorrectly
   emitted in :code instead of actual executable code, or when it contains
   no executable form at all.

   Self-evaluating data literals (`{:a 1}`, `[1 2 3]`, `:kw`, `42`, `nil`,
   quoted forms, etc.) are ALLOWED. They're valid Clojure code and agents
   legitimately use them as steps — most importantly to push a payload into
   the next iteration's `<journal>` without binding a var. Execution
   receipts persist the evaluated value, so a literal form is a cheap,
   visible way to stash state between turns.

   Comments (`;; …`) and discard reader macros (`#_ form`) ARE supported
   alongside an executable form — `;; intent\\n(read-file \"x\")` evaluates
   normally, the comment carries narrative for post-mortem readability.
   Only the case where the WHOLE expr is just comments/discards is rejected,
   because such a block evaluates to nil and consumes an iteration slot
   without doing anything useful.

   Bare string prose is also blocked, because that's the concrete
   failure mode where the LLM tries to answer *through* `:code` instead of
   `:answer`."
  [expr]
  (cond
    (bare-string-code-block? expr)
    "Bare string literal in :code. Prose belongs in :answer with answer-type text, not in :code."

    (comment-only-block? expr)
    "Code block contains only comments / discards (`;;` or `#_`) and no executable form. Add an expression to evaluate, or drop the block entirely."))

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
;; System Prompt + Nudges
;; =============================================================================

(defn build-system-prompt [{:keys [system-prompt]}] (or system-prompt ""))

(defn- budget-warning [{:keys [iteration current-max-iterations]}]
  (let [remaining (- (long (or current-max-iterations 0)) (inc (long (or iteration 0))))]
    (when (<= remaining 2)
      (str "[system_nudge] Iteration budget nearly exhausted (remaining="
        (max 0 remaining) "). If you can finalize safely, do it now."))))

(defn- var-index-overflow [user-var-count]
  (when (> (long (or user-var-count 0)) 150)
    (str "[system_nudge] <var_index> is large (" user-var-count " vars). Prefer :forget for scratch vars.")))

(defn- repetition-warning [call-counts-atom prev-expressions]
  (when (and call-counts-atom (seq prev-expressions))
    (let [keys* (mapv (fn [{:keys [code error result]}]
                        (if error [:error-only (str/trim (str error))]
                          [(str/trim (str code)) (pr-str result)]))
                  prev-expressions)
          max-count (swap! call-counts-atom
                      (fn [m] (reduce (fn [acc k] (update acc k (fnil inc 0))) (or m {}) keys*)))
          seen (apply max 0 (map #(get max-count % 0) keys*))]
      (when (>= seen 3)
        "[system_nudge] You are repeating the same expression pattern. Change strategy."))))

(defn- collect-extension-nudges [extensions ctx]
  (when (seq extensions)
    (into []
      (keep (fn [ext]
              (when-let [nudge-fn (:ext/nudge-fn ext)]
                (try
                  (when ((:ext/activation-fn ext) (:environment ctx))
                    (let [result (nudge-fn ctx)]
                      (when (and (string? result) (not (str/blank? result))) result)))
                  (catch Throwable _ nil)))))
      extensions)))

;; =============================================================================
;; Iteration message / context assembly (pure helpers — fully testable)
;; =============================================================================
;;
;; Extracted from the iteration-loop body so three known context-loss bugs
;; can be covered by unit tests that don't spin up an LLM or DB:
;;
;;   Fix #1 — `assemble-initial-messages` drops the old multimodal-only gate
;;            so plain-text user/assistant history from web/Telegram actually
;;            reaches the model.
;;   Fix #2 — `trim-to-initial-history` keeps the entire initial prompt
;;            (system + requirement + prior transcript) across iterations;
;;            previously hard-coded `(take 2 messages)` truncated it.
;;   Fix #3 — `build-iteration-context` promotes `*reasoning*` to its own
;;            <prior_thinking> block with the full text (up to
;;            `PRIOR_THINKING_MAX_CHARS`), instead of a 150-char preview
;;            buried in <var_index>.

(def ^:const PRIOR_THINKING_MAX_CHARS
  "Ceiling on how many characters of the previous iteration's `:thinking`
   we re-inject into the next iteration's prompt via <prior_thinking>.
   4000 ≈ ~800–1000 tokens — enough to keep the model's reasoning chain
   visible without letting unbounded thinking blow the context window."
  4000)

(defn assemble-initial-messages
  "Build the LLM message vector for iteration 0 of a query.

   In order:
   1. {:role \"system\"  :content system-prompt}
   2. {:role \"user\"    :content initial-user-content} — the
      the user's query text for the CURRENT turn.
   3. Every entry of `:history-messages` verbatim. Plain-text transcripts
      and multimodal entries alike — the caller owns this slice and
      guarantees it contains NO current-turn message (the current turn
      is already carried by `initial-user-content`).

   Previously the loop gated history behind `(some sequential? …)`, which
   silently dropped every plain-text transcript and caused the observable
   'agent forgets prior turns' symptom. Fix #1."
  [{:keys [system-prompt initial-user-content history-messages]}]
  (into [{:role "system" :content system-prompt}
         {:role "user"   :content initial-user-content}]
    (or history-messages [])))

(defn trim-to-initial-history
  "Strip error-feedback / empty-iteration nudges that the iteration loop
   appended via `recur`, keeping exactly the first `initial-count`
   messages.

   Rationale: the loop accumulates one-shot feedback messages across
   iterations for its error-recovery / empty-response paths. When a
   NEW iteration is assembled, only the ORIGINAL set (system +
   requirement + prior conversation history) should flow back into the
   prompt — intra-query nudges are not meant to recur.

   Previously hard-coded as `(take 2 messages)`, which dropped the
   cross-turn history restored by fix #1. See fix #2."
  [messages initial-count]
  (vec (take initial-count messages)))

;; Forward-declared so `build-iteration-context` can call them — the
;; concrete bodies live further down in this file next to the other
;; rendering helpers (they depend on vars defined later).
(declare read-var-index-str read-user-var-count format-expression-results register-extension!)

(defn build-iteration-context
  "Assemble the trailing user message appended to the base prompt on
   every iteration beyond the first.

   Single source of truth for per-iteration rendering: pulls
   <var_index> from the env-scoped cache, counts USER vars for the
   overflow nudge, formats the previous iteration's <journal>, calls
   nudge composers. The iteration-loop hands in the minimal state
   (iteration counters + prev-expressions) and gets back the assembled
   string (or nil when there's nothing to say).

   State (all optional):
     :iteration              — 0-indexed iteration number.
     :current-max-iterations — LIVE cap (includes extensions granted
                               via `request-more-iterations`). Pairs
                               with `:iteration` for the always-on
                               `[iter N/M]` header and for the budget
                               nudges.
     :prior-thinking         — `<prior_thinking>` body string (sliced
                               to latest + requested reasonings).
     :prev-expressions        — `[{:code :result :error …}]` from the
                               previous iteration; feeds the
                               <journal> render AND the repetition
                               nudge (fed through `call-counts-atom`).
     :prev-iteration         — iteration number that produced
                               `prev-expressions`.
     :call-counts-atom       — mutable repetition-counter owned by
                               the iteration-loop. Swapped by
                               `repetition-warning`.

   Returns the joined block or nil when every component is blank."
  [rlm-env {:keys [iteration current-max-iterations
                   prior-thinking
                   prev-expressions prev-iteration
                   call-counts-atom]}]
  (let [clamp (fn [s n] (if (and (string? s) (> (count s) n)) (subs s 0 n) s))
        ;; Always-on iteration marker so the LLM knows where it is in
        ;; the budget without reading the nudges. Cheap — ~20 chars.
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
        user-var-count (read-user-var-count rlm-env)
        ;; SYSTEM_NUDGE composers — built-in + extension-contributed.
        ;; Each returns a string or nil; `keep identity` drops nils,
        ;; `str/join` handles 0/1/many cleanly.
        nudge-ctx {:environment          rlm-env
                   :iteration            iteration
                   :current-max-iterations current-max-iterations
                   :prev-expressions      prev-expressions
                   :prev-iteration       prev-iteration
                   :user-var-count       user-var-count}
        built-in-nudges (keep identity
                          [(when (and iteration current-max-iterations)
                             (budget-warning
                               {:iteration              iteration
                                :current-max-iterations current-max-iterations}))
                           (var-index-overflow user-var-count)
                           (repetition-warning call-counts-atom prev-expressions)])
        ext-nudges (collect-extension-nudges
                     (some-> (:extensions rlm-env) deref) nudge-ctx)
        nudges-block (str/join "\n" (concat built-in-nudges ext-nudges))
        parts (keep (fn [p] (when (and (string? p) (not (str/blank? p))) p))
                [iter-header prior-block expr-results var-block nudges-block])]
    (when (seq parts)
      (str/join "\n" parts))))

;; =============================================================================
;; Iteration Loop
;; =============================================================================

(def ^:private FRESH_ITER_CARRY
  "Keys reset between iterations when the loop lost continuity —
   restart, error recovery. Merged into `loop-state` wherever the
   next turn can't trust what the previous one produced. Defining
   this once at namespace level keeps the recur sites short and the
   reset semantics in a single place."
  {:prev-expressions      nil
   :prev-iteration       -1
   :prev-next-model      nil
   :prev-next-reasoning  nil})

;; =============================================================================
;; Iteration error normalization
;; =============================================================================
;;
;; Any exception thrown inside `run-iteration` is caught by the main loop,
;; which either (a) aborts the turn if the error is infrastructure-level or
;; (b) feeds a compact description back to the LLM so it can recover.
;; The three helpers below own that normalization end-to-end so the catch
;; site stays trivial — classify, normalize, log, decide.

(def ^:private INFRASTRUCTURE_ERROR_TYPES
  "Error types we will NEVER feed back to the LLM — they indicate the
   provider itself is down (all providers tried, circuit breaker tripped,
   retry budget exhausted). Re-throw aborts the turn so we don't burn
   iterations repeatedly re-calling a dead upstream."
  #{:svar.llm/all-providers-exhausted
    :svar.llm/circuit-open
    :svar.llm/provider-exhausted})

(defn- infrastructure-error?
  "True when the given ex-data map belongs to an infrastructure class —
   see INFRASTRUCTURE_ERROR_TYPES for the exhaustive list."
  [ex-data-map]
  (contains? INFRASTRUCTURE_ERROR_TYPES (:type ex-data-map)))

(def ^:private LAST_USER_PREVIEW_CHARS 500)

(defn- last-user-message-preview
  "Scan `messages` right-to-left for the most recent `user` role message
   and return its content, capped at LAST_USER_PREVIEW_CHARS with an
   overflow suffix. Returns nil when no user message exists or its
   content is empty. Used to pin a failure to a specific prompt without
   digging through the DB."
  [messages]
  (when-let [c (some (fn [m] (when (= (:role m) "user") (:content m)))
                 (reverse messages))]
    (let [s (str c)]
      (if (> (count s) LAST_USER_PREVIEW_CHARS)
        (str (subs s 0 LAST_USER_PREVIEW_CHARS)
          " …<+" (- (count s) LAST_USER_PREVIEW_CHARS) " chars>")
        s))))

(defn- exception->iter-err
  "Normalize an exception thrown by `run-iteration` into the iter-err map
   stored on the query row and fed back to the LLM. Pure function — no
   logging, no mutation.

   svar's ex-data (0.3.4+) carries what the PROVIDER saw —
   `:chat-url`, `:api-style`, `:http-response {:parsed :raw-body …}`,
   `:duration-ms`. We preserve that under `:data` and additionally attach
   a `:vis-context` describing what the AGENT LOOP saw: iteration number,
   effective routing, and the last user-message preview. Together they
   are enough to reproduce the failure offline.

   `ctx` keys: :iteration :messages :routing :reasoning-level."
  [^Throwable e ctx]
  (let [ex-data-map (ex-data e)
        cause       (.getCause e)]
    {:message     (ex-message e)
     :type        (:type ex-data-map)
     :class       (.getName (class e))
     :data        (when (seq ex-data-map) (dissoc ex-data-map :type))
     :vis-context {:iteration         (:iteration ctx)
                   :messages-count    (count (:messages ctx))
                   :routing           (:routing ctx)
                   :reasoning-level   (:reasoning-level ctx)
                   :last-user-preview (last-user-message-preview (:messages ctx))}
     :cause       (when cause
                    {:message (.getMessage ^Throwable cause)
                     :class   (.getName (class e))})
     :stack       (mapv str (take 12 (.getStackTrace e)))}))

(defn- handle-iteration-exception!
  "Error path for the main-loop try/catch around `run-iteration`.

   Infrastructure failures abort the turn (re-throw); everything else is
   normalized via `exception->iter-err`, logged, and returned wrapped in
   `{::iteration-error …}` so the loop can feed the error back to the
   LLM for recovery. `ctx` is the same map accepted by
   `exception->iter-err` plus `:iteration` (duplicated for log clarity)."
  [^Throwable e ctx]
  (let [ex-data-map (ex-data e)
        iteration (:iteration ctx)]
    (if (infrastructure-error? ex-data-map)
      (do (tel/log! {:level :error
                       :data {:iteration iteration
                              :error (ex-message e)
                              :type (:type ex-data-map)}
                       :msg "Provider infrastructure error — aborting iteration loop"})
        (throw e))
      (let [iter-err (exception->iter-err e ctx)]
        (tel/log! {:level :warn
                     :data (assoc iter-err :iteration iteration)
                    }
  "RLM iteration failed, feeding error to LLM")
        {::iteration-error iter-err}))))

(defn run-iteration
  "Runs a single RLM iteration: ask! → check final → execute code.

   Uses ask! with iteration spec for provider-enforced JSON structured output.
   No regex fallback, no code-level FINAL detection.

   Params:
   `rlm-env` - RLM environment map.
   `messages` - Vector of message maps for the LLM.
   `opts` - Map, optional:
      - :iteration-spec - Spec for ask! (default: ITERATION_SPEC_NON_REASONING).
                         For reasoning providers, pass ITERATION_SPEC_REASONING."
  [rlm-env messages & [{:keys [iteration-spec routing iteration reasoning-level]
                        :or {iteration-spec ITERATION_SPEC_NON_REASONING}}]]
  (binding [*rlm-ctx* (merge *rlm-ctx* {:rlm-phase :run-iteration})]
    (let [_model-name (:name (resolve-effective-model (:router rlm-env)))
          effective-reasoning (when (some? reasoning-level)
                                (or (normalize-reasoning-level reasoning-level)
                                  (throw (ex-info "Invalid :reasoning-level. Expected one of quick|balanced|deep."
                                           {:type :vis/invalid-reasoning-level
                                            :got reasoning-level}))))
          ;; ask! auto-translates :reasoning to the right provider extra-body
          ;; using the selected model's :reasoning? flag + provider :api-style.
          ask-result (binding [llm/*log-context* {:query-id (:env-id rlm-env) :iteration iteration}]
                       (llm/ask! (:router rlm-env)
                         (cond-> {:spec iteration-spec
                                  :messages messages
                                  :routing (or routing {})
                                  :check-context? false}
                           effective-reasoning (assoc :reasoning effective-reasoning))))
          parsed (:result ask-result)
          model-reasoning (:reasoning ask-result)
          ;; Native reasoning takes priority over spec-parsed thinking
          thinking (or model-reasoning (:thinking parsed))
          _ (log-stage! :llm-response iteration
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
              expr-results (when (seq code-blocks)
                             (mapv (fn [{:keys [expr time-ms]}]
                                      (if-let [err (literal-code-block-error expr)]
                                        {:result nil :error err
                                         :stdout "" :stderr "" :execution-time-ms 0}
                                        (execute-code rlm-env expr :timeout-ms time-ms)))
                                code-entries))
              expr-errors (when expr-results
                            (seq (filter :error expr-results)))
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
              final-answer raw-answer
              confidence (or (:confidence parsed) :high)
              validation-error (or (when-not answer-type
                                     ":answer-type is required with :answer. Set mustache-text or mustache-markdown.")
                                 (when expr-errors
                                   (str "Code errors before final: " (:error (first expr-errors))))
                                 (when (cleanup-claim-without-forget? final-answer forget-names)
                                   "You claimed vars/index cleanup in the final answer, but this iteration did not emit :forget. Emit :forget with the concrete var names first, then finalize.")
                                 (when (placeholder-final-answer? raw-final-answer (or (some? resolved-var-value) (some? template-answer)))
                                   (str "Placeholder word '" (str/trim (str raw-final-answer)) "' is not a real answer. Put the actual content in :answer, or def the result and use its var name."))
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
               :forget forget-names
               :expressions (or expressions
                             [{:id 0 :code final-answer :result nil :stdout "" :stderr ""
                               :error validation-error}])
               :final-result nil :api-usage api-usage
               :duration-ms (or (:duration-ms ask-result) 0)}
            (let [sources (vec (or (:sources parsed) []))
                  final-result (cond-> {:final? true
                                        ;; `:text` for plain strings (was `String` class — mixing
                                        ;; Java Class with `:markdown` keyword across the
                                        ;; success vs fallback branches made consumers eat the
                                        ;; ambiguity). Fallback branches use `:markdown` to flag
                                        ;; rendered markdown (⚠️ warnings).
                                        :answer final-answer
                                        :confidence confidence}
                                 (seq sources) (assoc :sources sources)
                                 (:reasoning parsed) (assoc :reasoning (:reasoning parsed)))]
              {:thinking thinking
               :next-model next-model :next-reasoning next-reasoning
               :forget forget-names
               :expressions (or expressions []) :final-result final-result :api-usage api-usage
               :duration-ms (or (:duration-ms ask-result) 0)})))
        ;; Normal path: execute code blocks.
        ;;
        ;; Each block is already `{:expr string :time-ms int}` — svar's
        ;; CODE_BLOCK_SPEC marks BOTH fields required, so the response
        ;; parser guarantees the shape. No `(when (map? block))`, no
        ;; `(str/blank?)`, no `(throw "missing :time-ms")` — that
        ;; defensive branch is dead code and just obscures control flow.
        ;; See AGENTS.md § "Trust svar spec guarantees".
        (let [normalized (vec (:code parsed))
              _raw-exprs (mapv :expr normalized)
              ;; Coalesce fragments: when model splits one expression across multiple
              ;; array entries (one line per string), join unbalanced blocks with the
              ;; next until parens balance. Prevents repair from "fixing" each
              ;; line individually into a broken zero-body form.
              coalesced (loop [remaining normalized
                               result []]
                          (if (empty? remaining)
                            result
                            (let [{:keys [expr time-ms]} (first remaining)
                                  bal 0]
                              (if (and (pos? bal) (next remaining))
                                ;; Unbalanced opener - join with subsequent blocks, sum timeouts
                                (let [[joined-expr joined-time rest-blocks]
                                      (loop [acc expr
                                             t-acc (or time-ms 0)
                                             rem (rest remaining)]
                                        (if (or (<= 0 0) (empty? rem))
                                          [acc t-acc rem]
                                          (let [nxt (first rem)]
                                            (recur (str acc "\n" (:expr nxt))
                                              (+ t-acc (or (:time-ms nxt) 0))
                                              (rest rem)))))]
                                  (recur rest-blocks
                                    (conj result {:expr joined-expr
                                                  :time-ms joined-time})))
                                (recur (rest remaining) (conj result (first remaining)))))))
              total-blocks (count coalesced)
              code-blocks (mapv :expr coalesced)
              expression-results
              (map-indexed
                (fn [idx {:keys [expr time-ms]}]
                  (log-stage! :code-exec iteration
                    {:idx (inc idx) :total total-blocks
                     :code expr :time-ms time-ms})
                  (if-let [err (literal-code-block-error expr)]
                    (do (log-stage! :code-result iteration
                          {:idx (inc idx) :total total-blocks :error err})
                      {:result nil :error err :stdout "" :stderr "" :execution-time-ms 0})
                    (let [r (execute-code rlm-env expr :timeout-ms time-ms)]
                      (log-stage! :code-result iteration
                        {:idx (inc idx) :total total-blocks
                         :execution-time-ms (:execution-time-ms r)
                         :error (:error r)
                         :timeout? (:timeout? r)
                         :result (:result r)})
                      r)))
                coalesced)
              ;; Combine code blocks with their execution results
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
            :expressions expressions :final-result nil :api-usage api-usage
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

(defn format-expressions
  "Formats expressions as compact per-block receipts for LLM feedback in the
   error-recovery / iteration-accumulation path. One line per block:

     <code> → <value>                          ;; success
     <code> → ERROR: <msg> :hint \"...\"          ;; error + pattern hint
     <code> → <value> (1250ms SLOW)            ;; slow execution
     <code> → <value> :stdout \"...\"            ;; stdout if present

   Type labels, size suffixes, `:success?` flags, `:time-ms 0` spam, and
   per-block wrapping braces were all removed — they duplicated info the
   LLM derives from Clojure syntax. Only `:hint` (pattern-based error
   guidance) and `:warning \"auto-repaired delimiters\"` carry real
   signal not in the value itself."
  [expressions]
  (str/join "\n"
    (map (fn [{:keys [code error result stdout repaired? execution-time-ms]}]
           (let [code-str (str/trim (or code ""))
                  hint (when error (error-hint error))
                  val-part (cond
                             error
                             (str "ERROR: " error
                               (when hint (str " :hint " (pr-str hint))))

                             (fn? result)
                             (str "ERROR: " code-str " is a function object. Call it: (" code-str ")")

                             (instance? clojure.lang.Var result)
                             (let [^clojure.lang.Var var-obj result
                                   var-name (name (.sym var-obj))
                                   raw-bound (.getRawRoot var-obj)]
                               (str "*" var-name "* = "
                                 (rt-shared/result->display raw-bound :full)))

                             :else
                             (rt-shared/result->display result :full))
                 stdout-part (when-not (str/blank? stdout)
                               (str " :stdout " (pr-str stdout)))
                 warning-part (when repaired?
                                " :warning \"auto-repaired delimiters\"")
                 time-ms (or execution-time-ms 0)
                 slow-part (when (> time-ms SLOW_EXECUTION_MS)
                             (str " (" time-ms "ms SLOW)"))]
             (str code-str " → " val-part
               (or slow-part "")
               (or stdout-part "")
               (or warning-part ""))))
      expressions)))

(def ^:private EXECUTION_SAFETY_CAP_CHARS
  "Cap for one value rendering in a journal entry. Aligned with the UI cap
   (`runtime.shared/MAX_RESULT_DISPLAY_CHARS`, 30000) so the LLM and the
   web/TUI see the same horizon. The DB still stores uncapped values via
   `store-iteration!` so the debugger and corpus exporter retain ground
   truth — only the prompt-facing render is bounded.

   The `:truncated? true` flag (rather than an inline `…[truncated]…`
   marker) is preserved so the LLM can still pattern-match on it the way
   it always has."
  rt-shared/MAX_RESULT_DISPLAY_CHARS)

(def ^:private EXECUTION_STDERR_CHARS
  "Budget for captured stderr — diagnostics only, not a full data dump."
  2000)

(defn- truncated-pr-str
  "pr-str v + truncate-at-cap flag. Single pass, used by journal rendering.
   Strips the `sandbox/` ns from var literals so the LLM sees `#'foo`
   instead of `#'sandbox/foo` — same cosmetic the UI applies."
  [v]
  (let [s (rt-shared/strip-sandbox-ns (pr-str v))]
    (if (> (count s) EXECUTION_SAFETY_CAP_CHARS)
      [(truncate s EXECUTION_SAFETY_CAP_CHARS) true]
      [s false])))

(defn format-prior-thinking-chain
  "Compose the `<prior_thinking>` block showing EVERY iteration's
   reasoning inside the current query, oldest first.

   Input: vector of `{:iteration N :thinking s}` maps (any extra keys
   ignored). Blanks are skipped entirely — an iteration that had no
   thinking doesn't earn a tag in the chain.

   Returns the joined block string, or nil when nothing non-blank
   was supplied. Callers splice the result under a `<prior_thinking>`
   wrapper via `build-iteration-context`.

   The chain is the cross-iteration version of what `*reasoning*` used
   to approximate — `*reasoning*` only held the LAST step, so iter N+2
   had no view of iter N's reasoning. With the full chain the agent
   can reference any of its own earlier steps by iteration number
   without re-deriving them.

   Cross-QUERY visibility is out of scope here; older turns' reasoning
   is reachable on demand via `(var-history '*reasoning*)`."
  [iterations]
  (let [entries (->> iterations
                  (keep (fn [{:keys [iteration thinking]}]
                          (when (and (string? thinking)
                                  (not (str/blank? thinking)))
                            (str "[iter " iteration "] " (str/trim thinking))))))]
    (when (seq entries)
      (str/join "\n\n" entries))))

;; Context-compaction (PRIOR_THINKING_BUDGET_CHARS + should-compact +
;; plan-thinking-compaction + format-compacted-thinking-chain + a
;; `*compacted-reasoning*` SCI var) was removed. The same lesson applied
;; later to multi-iteration <prior_thinking>: the spec used to expose a
;; `:request-prior-reasonings` knob and the loop spliced 3+ historical
;; reasonings into every prompt by default. Both are gone. The agent now
;; sees at most the SINGLE latest `[iter N] …` and reaches anything older
;; via `(var-history '*reasoning*)` on demand. Eager auto-context burns
;; tokens on summaries nobody asked for; deliberate pulls don't.

(def ^:const HANDOVER_KEEP_LAST
  "How many of the previous turn's most-recent iterations carry over
   verbatim into the new turn's handover block. Everything older in the
   prior turn is implicit in the final answer; dragging it forward just
   wastes context on the new task."
  2)

(defn format-prior-turn-handover
  "Compose the cross-query handover shown at the TOP of a new turn's
   prompt.

   A new query in an ongoing conversation is still informed by what
   came before — but not by every iteration of every previous turn,
   which would balloon context. The handover compacts the previous
   turn to the minimum the agent needs to thread continuity:

     [prior turn]
       [iter N-1] <thinking>
       [iter N]   <thinking>
       [final answer] <…answer text…>
     [new query]

   Input:
     :iterations   — full iteration list of the previous turn, oldest
                     first. Only the last `HANDOVER_KEEP_LAST` are kept.
     :final-answer — string or nil; the previous turn's committed
                     answer. Omitted from the block when blank/nil.

   Returns nil when there are no previous iterations at all (fresh
   conversation — nothing to hand over)."
  [{:keys [iterations final-answer]}]
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

(defn- format-expression-results
  "Formats the previous iteration's expressions as the `<journal>` block
   shown at the top of the next iteration's user message.

   Minimal one-line-per-block format (as of 2026-04):

     [1] (+ 1 2) → 3
     [2] (def x 42) → *x* = 42
     [3] (bad-fn) → ERROR: Unable to resolve symbol: bad-fn
     [4] (slow-scan db) → [...data...] (2100ms SLOW)
     [5] (println \"hi\") → nil :stdout \"hi\\n\"
     [6] (huge-expr) → \"...\" :truncated? true

   What was removed and why (compared to the earlier EDN-map format):

   - `:success?` — `ERROR:` prefix already marks failure.
   - `:result-type` / `:value-type` — Clojure `pr-str` syntax is its own
     type ticker. `3` is obviously an int, `[1 2 3]` obviously a vector.
   - `:value-size` — the LLM can count `[1 2 3]` too.
   - `:time-ms 0` on every block — noise. Only `SLOW` exceedances matter,
     and the ARCH section already calibrates the agent's time budget.
   - `:perf-warning \"SLOW — optimize …\"` full sentence — replaced by
     the `(Xms SLOW)` suffix; the advice itself lives once in ARCH.
   - `:result-kind :var` + `:var-name` — a `(def sym …)` block now
     renders as `*sym* = <value>` inline, which is both obvious to the
     LLM and the same earmuffed convention used elsewhere in vis.
   - Outer `{…}` wrap on every entry — the line structure already
     delimits entries.
   - `iteration=\"N\"` on `<journal>` — the loop already tells the LLM
     which iteration it's on.

   Cross-reference by `[N]` is preserved so the model can say \"the error
   in [3]\". Stdout/stderr stay as `:stdout \"…\"` / `:stderr \"…\"`
   suffixes (non-empty only). Over-cap values get `:truncated? true`.

   Semantic equivalence: every signal the earlier format carried is still
   reachable from the new format; redundancy was stripped, not content."
  [expressions _iteration]
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
                  value-part
                  (cond
                    error
                    (str "ERROR: " (truncate error 400))

                    (fn? result)
                    "ERROR: Result is a function, not a value"

                    (instance? clojure.lang.Var result)
                    (let [^clojure.lang.Var var-obj result
                          var-name (name (.sym var-obj))
                          raw-bound (.getRawRoot var-obj)
                          bound (realize-value raw-bound)
                          [value-str truncated?] (truncated-pr-str bound)]
                      (str "*" var-name "* = " value-str
                        (when truncated? " :truncated? true")))

                    :else
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

;; NOTE (2025-04): the accumulating `execution_journal` block was removed.
;; It grew monotonically per iteration (~300 chars squashed + 8K recent per
;; entry × iterations_so_far) and bloated the prompt on long turns with no
;; real benefit once the LLM has:
;;   - `*query*`     — current query, bound into the sandbox on query start.
;;   - `*reasoning*` — current iteration's thinking, auto-def'd per iteration.
;;   - `conversation-history` / `conversation-code` / `conversation-results`
;;     — on-demand access to prior turns across queries.
;;   - `var-history` — on-demand access to var evolution.
;; The `<journal>` block (was `<expression_results>`) carries only the PREVIOUS
;; iteration's expressions. Pulling anything further back is the LLM's job via
;; the tools above. This makes the per-iteration prompt O(1) instead of O(n).

(defn- extract-def-names
  "Extracts var names from code blocks via EDN parsing of def-like forms."
  [expressions]
  (->> expressions
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
                    (tel/log! {:level :debug :id ::extract-def-names-fallback
                                 :data {:error (ex-message e)}
                                 :msg "Failed to parse code forms for def names, returning empty"})
                    [])))))
    (map str)
    vec))

(defn- load-prior-thinking-chain
  "Pull every iteration's `:thinking` for the current query from the
   DB, tagged with its 0-based iteration index, oldest first. Each
   `store-iteration!` persists thinking as it lands, so iterations
   0..N-1 are visible when building the prompt for iteration N.

   Returns a vector of `{:iteration int :thinking string}` maps, or
   `[]` when there is no query-id or the DB read throws."
  [db-info query-id]
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
                   :data {:error (ex-message t)
                          :ex-data (ex-data t)
                          :class (.getName (class t))
                          :stack (mapv str (take 8 (.getStackTrace t)))
                          :query-id query-id}
                   :msg "load-prior-thinking-chain failed — <prior_thinking> will render empty for this iteration"})
      [])))

(def PRIOR_THINKING_BREADCRUMB
  "Static hint appended to <prior_thinking>. Tells the agent how to
   reach older reasonings without auto-shipping them in every prompt."
  "[older reasonings] call `(var-history '*reasoning*)` from :code (oldest first; `take-last N` for a window).")

(defn- build-prior-thinking
  "Render the <prior_thinking> body: ONLY the most recent iteration's
   reasoning plus a one-line breadcrumb pointing at `var-history`.

   The loop deliberately does NOT pack older reasonings into the
   prompt. Eager re-injection burned tokens on context the agent
   rarely re-read; the agent now pulls older thinking on demand via
   `(var-history '*reasoning*)` (or `(take-last N (var-history
   '*reasoning*))`). Returns the string or nil when no prior
   iteration has produced thinking yet."
  [_rlm-env db-info query-id]
  (let [chain (load-prior-thinking-chain db-info query-id)]
    (when (seq chain)
      (let [tail (vec (take-last 1 chain))
            body (format-prior-thinking-chain tail)]
        (if body
          (str body "\n" PRIOR_THINKING_BREADCRUMB)
          PRIOR_THINKING_BREADCRUMB)))))

(defn- read-user-var-count
  "How many USER vars live in the current SCI sandbox — i.e. the rows
   `<var_index>` will render. Filters out tool/helper bindings via
   `:initial-ns-keys`. Defensive: catches any SCI internals weirdness
   and returns 0 so the overflow nudge never crashes the turn. Logs
   the failure so an agent silently missing var-overflow nudges is
   diagnosable, not invisible."
  [rlm-env]
  (try
    (let [sandbox-map (get-in @(:env (:sci-ctx rlm-env)) [:namespaces 'sandbox])
          initials    (or (:initial-ns-keys rlm-env) #{})]
      (count (filter (fn [[s _]]
                       (and (symbol? s) (not (contains? initials s))))
               sandbox-map)))
    (catch Throwable t
      (tel/log! {:level :warn
                   :data {:error (ex-message t)
                          :ex-data (ex-data t)
                          :class (.getName (class t))
                          :stack (mapv str (take 8 (.getStackTrace t)))}
                   :msg "read-user-var-count failed — var-overflow nudge disabled this turn"})
      0)))

(defn- read-var-index-str
  "Render (or serve the cached render of) `<var_index>` for the given
   env. Uses the env-scoped `:var-index-atom` as a revision-keyed cache
   so a turn that touched nothing doesn't re-walk the sandbox map.

   Persisted-but-evicted vars appear as placeholders so the LLM
   knows they exist and can call `(var-history 'sym)` to inspect."
  [rlm-env]
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

(defn- build-cross-query-handover
  "Compose the `[prior turn]` block shown at iteration 0 of a new
   query in an ongoing conversation.

   Fetches the previous `:query` entity from DB, its iterations, and
   its final answer; threads them through `format-prior-turn-handover`.
   Returns nil for a fresh conversation (no previous query) or a sub-
   RLM query (we don't want the parent's whole-turn history leaking
   into sub-RLM context — sub-RLM already gets a targeted
   `[parent handoff]` via `build-handoff`)."
  [db-info conversation-id current-query-id parent-iteration-id]
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
                     :data {:error (ex-message t)
                            :ex-data (ex-data t)
                            :class (.getName (class t))
                            :stack (mapv str (take 8 (.getStackTrace t)))
                            :conversation-id conversation-id}
                     :msg "build-cross-query-handover failed — new query missing [prior turn] context"})
        nil))))

(defn- restorable-var-snapshots
  "Returns serializable snapshots of user vars introduced by this iteration."
  [rlm-env expressions]
  (let [execution->defs (mapv (fn [{:keys [error] :as execution}]
                                [execution (when-not error
                                             (set (map symbol (extract-def-names [execution]))))])
                          expressions)
        defined (into #{} (mapcat second) execution->defs)
        sym->exec (reduce (fn [acc [{:keys [code execution-time-ms]} defs]]
                            (if (and code (seq defs))
                              (reduce #(assoc %1 %2 {:expr code :time-ms execution-time-ms})
                                acc defs)
                              acc))
                    {}
                    execution->defs)
        locals (get-locals rlm-env)]
    (->> locals
      (keep (fn [[sym value]]
              (when (contains? defined sym)
                (let [realized (realize-value value)
                      exec-info (get sym->exec sym)]
                  ;; Accept ALL values — freeze-safe in the persistence layer
                  ;; handles non-serializable types (fns → {:vis/ref :expr}).
                  (cond-> {:name (str sym)
                             :value realized
                             :code (:expr exec-info)}
                      (:time-ms exec-info)
                      (assoc :time-ms (:time-ms exec-info)))))))
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
   DB rows are not touched; `(var-history 'sym)` can inspect old values.

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
      (tel/log! {:level :info :id ::forget-system-var-idused
                   :data {:requested (mapv str system-syms)}
                   :msg "Refusing to forget SYSTEM vars (*foo*) — ignoring those names"}))
    (when (seq user-syms)
      (try
        (swap! (:env sci-ctx) update-in [:namespaces 'sandbox]
          (fn [ns-map] (apply dissoc ns-map user-syms)))
        (catch Throwable e
          (tel/log! {:level :debug :id ::forget-vars-failed
                       :data {:error (ex-message e) :syms (mapv str user-syms)}
                       :msg "forget-vars! failed — skipping"}))))))

;; =============================================================================
;; Deterministic Auto-Forget
;; =============================================================================

(def ^:const AUTO_FORGET_STALE_QUERIES
  "Number of recent queries a var must have been defined/redefined in to survive
   auto-forget. Vars without a docstring that were last touched more than this
   many queries ago are automatically removed from the live sandbox at the start
   of each new query. DB rows are untouched — (var-history 'sym) can inspect old values."
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

(defn- auto-forget-stale-vars!
  "Deterministic cleanup at query boundary: remove sandbox vars that
  (a) have no docstring, and (b) were last defined/redefined more
  than AUTO_FORGET_STALE_QUERIES queries ago. This replaces the
  unreliable \"ask LLM to emit :forget\" pattern for scratch vars.
  DB rows are untouched — (var-history 'sym) can inspect old values."
  [{:keys [db-info conversation-id sci-ctx initial-ns-keys var-index-atom]}]
  (when (and db-info conversation-id sci-ctx)
    (try
      (let [all-queries    (sort-by :created-at
                             (db/db-list-conversation-queries db-info conversation-id))
            recent-ids     (into #{} (map :id) (take-last AUTO_FORGET_STALE_QUERIES all-queries))
            var-registry   (db/db-latest-var-registry db-info conversation-id)
            sandbox-map    (get-in @(:env sci-ctx) [:namespaces 'sandbox])
            candidates     (auto-forget-candidates sandbox-map initial-ns-keys
                             var-registry recent-ids)]
        (when (seq candidates)
          (tel/log! {:level :info :id ::auto-forget
                     :data {:forgotten (mapv str candidates) :count (count candidates)}
                     :msg (str "Auto-forget: evicting " (count candidates) " stale vars without docstrings")})
          (forget-vars! sci-ctx candidates)
          (when var-index-atom
            (swap! var-index-atom update :current-revision inc))))
      (catch Exception e
        (tel/log! {:level :warn :id ::auto-forget-failed
                   :data {:error (ex-message e)}
                   :msg "Auto-forget failed — skipping"})))))

(defn iteration-loop [rlm-env query
                      {:keys [max-context-tokens system-prompt
                              query-id history-messages
                              max-iterations max-consecutive-errors max-restarts
                              hooks cancel-atom current-iteration-atom
                              reasoning-default routing]}]
  (let [max-iterations (or max-iterations rlm-spec/MAX_ITERATIONS)
        max-consecutive-errors (or max-consecutive-errors 5)
        max-restarts (or max-restarts 3)
        ;; Adaptive budget: if rlm-env has a max-iterations-atom (set by vis!),
        ;; read from it so the LLM can extend its own budget via (request-more-iterations n).
        ;; Otherwise use the static max-iterations parameter.
        max-iter-atom (:max-iterations-atom rlm-env)
        effective-max-iterations (fn [] (if max-iter-atom @max-iter-atom max-iterations))
        ;; Resolve effective model name for token counting
        effective-model (:name (resolve-effective-model (:router rlm-env)))
        _ (assert effective-model "Router must resolve a root model — check provider config")
        ;; Default max-context-tokens to 60% of model's context window.
        ;; Prevents unbounded history accumulation (quadratic token growth over iterations).
        max-context-tokens (or max-context-tokens
                             (long (* 0.6 (router/context-limit effective-model))))
        ;; Check if root provider has native reasoning (thinking tokens)
        has-reasoning? (provider-has-reasoning? (:router rlm-env))
        base-reasoning-level (or (normalize-reasoning-level reasoning-default)
                               balanced-reasoning)
        ;; `:has-documents?` is the only activation flag that still matters
        ;; at this level — it drives the RESPONSE FORMAT's :sources field in
        system-prompt (build-system-prompt {:has-reasoning? has-reasoning?
                                            :system-prompt system-prompt
                                            :max-context-tokens max-context-tokens
                                            :env rlm-env})
        initial-user-content query
        initial-messages (assemble-initial-messages
                           {:system-prompt system-prompt
                            :initial-user-content initial-user-content
                            :history-messages history-messages})
        db-info (:db-info rlm-env)
        _ (auto-forget-stale-vars! rlm-env)
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
         ;; Repetition detection — `repetition-warning`
         ;; reads+swaps this atom from build-iteration-context each
         ;; turn. Keyed by both `[code result]` AND
         ;; `[:error-only msg]` so varying-input-same-error loops
         ;; get caught (see bump-and-detect-repetition docstring).
         call-counts-atom (atom {})
        finalize-cost (fn []
                        (let [{:keys [input-tokens output-tokens reasoning-tokens cached-tokens]} @usage-atom
                              total-tokens (+ input-tokens output-tokens)
                              cost (router/estimate-cost effective-model input-tokens output-tokens)]
                          {:tokens {:input input-tokens :output output-tokens
                                    :reasoning reasoning-tokens :cached cached-tokens
                                    :total total-tokens}
                           :cost cost}))
        ;; Var-index cache lives on the env (`:var-index-atom`); the
        ;; rendering fn `read-var-index-str` + the count fn
        ;; `read-user-var-count` both take rlm-env directly — no local
        ;; closure needed. Kept a default atom below only so envs built
        ;; without a shared var-index-atom still cache within this loop.
        var-index-atom (or (:var-index-atom rlm-env)
                         (atom {:index nil :revision -1 :current-revision 0}))
        on-iteration (:on-iteration hooks)
        on-chunk (:on-chunk hooks)
        on-cancel (:on-cancel hooks)
        emit-hook! (fn [hook-fn payload log-msg]
                     (when hook-fn
                       (try
                         (hook-fn payload)
                         (catch Exception e
                           (tel/log! {:level :warn :data {:error (ex-message e)}
                                        :msg log-msg})))))
        active-extension-names (fn []
                                 (when-let [exts (some-> (:extensions rlm-env) deref seq)]
                                   (mapv (comp str :ext/namespace) exts)))
        iter-metadata (fn []
                        (let [ext-names (active-extension-names)]
                          (when (seq ext-names)
                            {:extensions ext-names})))]
    ;; query-start is logged in query.clj — don't duplicate
    ;; Auto-bind *query* into the SCI sandbox so the LLM (and var-history)
    ;; can always see the current user query. `bind-and-bump!` atomically
    ;; updates SCI AND invalidates the var-index cache — skipping the bump
    ;; causes turns with no `:code` (greetings) to serve a stale <var_index>
    ;; with a PRIOR turn's `*query*` value to the LLM.
    (sci-env/bind-and-bump! rlm-env '*query* query)
    ;; Fresh query, fresh parent-iteration-id. The env-scoped atom
    ;; may still hold the LAST iteration of the PREVIOUS query; sub-
    ;; RLMs spawned during iter 0 would otherwise inherit that stale
    ;; ref as their :parent. nil = fall back to conversation-level
    ;; parenting until store-iteration! runs.
    (when-let [a (:current-iteration-id-atom rlm-env)]
      (reset! a nil))
    (binding [*rlm-ctx* (merge *rlm-ctx* {:rlm-phase :iteration-loop})]
      ;; `loop-state` is a single map threaded through every recur.
      ;; Positional `(recur x y z nil -1 nil nil nil)` turned the
      ;; carry-or-reset semantics into a column of anonymous values;
      ;; the map makes each name self-describing, and "reset the
      ;; inter-iteration carry" is a single `merge` with
      ;; `FRESH_ITER_CARRY` (top-level def above).
      ;;
      ;; Keys:
      ;;   :iteration            — 0-indexed iteration count.
      ;;   :messages             — accumulating user/assistant history.
      ;;   :trace                — per-iteration trace entries.
      ;;   :consecutive-errors   — error counter for strategy-restart.
      ;;   :restarts             — completed restart count.
      ;;   :prev-expressions      — previous iter's expressions, for
      ;;                           <journal>. Nilled on error/restart
      ;;                           so the next turn doesn't cite a
      ;;                           dead result as its baseline.
      ;;   :prev-iteration       — iteration index that produced the
      ;;                           prev-expressions journal entry.
      ;;   :prev-next-model      — the LLM's :next.model override for
      ;;                           the NEXT iteration's routing.
      ;;   :prev-next-reasoning  — the LLM's :next.reasoning override.
      (loop [loop-state (merge {:iteration          0
                                :messages           initial-messages
                                :trace              []
                                :consecutive-errors 0
                                :restarts           0}
                          FRESH_ITER_CARRY)]
        (let [{:keys [iteration messages trace consecutive-errors restarts
                      prev-expressions prev-iteration
                      prev-next-model prev-next-reasoning]} loop-state]
        (when current-iteration-atom
          (reset! current-iteration-atom iteration))
        (cond
          ;; Cooperative cancellation — caller-owned :cancel-atom from query!
          ;; (or an internal per-query atom when not supplied).
          (when cancel-atom @cancel-atom)
          (do (log-stage! :error iteration {:reason :cancelled})
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
                locals (when debug? (get-locals rlm-env))
                max-iter (effective-max-iterations)
                last-thinking (some->> trace reverse
                                (map :thinking)
                                (filter #(and (string? %) (not (str/blank? %))))
                                first)
                fallback-answer (str "⚠️ Iteration limit reached (" iteration "/" max-iter
                                  ") without finalizing.\n\n"
                                  (when last-thinking
                                    (str "**Last reasoning step:**\n\n"
                                      (truncate last-thinking 800) "\n\n"))
                                  "**What to try:** Check the reasoning trace below for partial progress, "
                                  "or rephrase the question more narrowly so the agent can commit to an answer faster.")]
            (log-stage! :error iteration {:reason :max-iterations :max max-iter})
            (merge {:answer fallback-answer
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
                    restart-hint (str "Previous attempts failed with these errors:\n" failed-summary
                                   "\n\nStart fresh with a DIFFERENT strategy. Do NOT repeat the same approach."
                                   "\n\nOriginal request: " query)
                    restart-messages [{:role "system" :content system-prompt}
                                      {:role "user" :content restart-hint}]]
                (tel/log! {:level :info :data {:iteration iteration :restarts (inc restarts)
                                                 :errors consecutive-errors}
                             :msg "Strategy restart — resetting with anti-knowledge"})
                (recur (merge loop-state
                         {:iteration          (inc iteration)
                          :messages           restart-messages
                          :trace              trace
                          :consecutive-errors 0
                          :restarts           (inc restarts)}
                         FRESH_ITER_CARRY)))
              (do (tel/log! {:level :warn :data {:iteration iteration :consecutive-errors consecutive-errors
                                                   :restarts restarts}
                               :msg "Error budget exhausted — too many consecutive errors across restarts. Simplify your code or break the task into smaller steps."})
                (let [recent-errors (->> trace reverse
                                      (keep :error)
                                      (take 3)
                                      (map (fn [e] (str "- " (or (:message e) (str e)))))
                                      (str/join "\n"))
                      fallback-answer (str "⚠️ Too many consecutive errors (" consecutive-errors
                                        ") across " (inc restarts) " restart attempt"
                                        (when (not= restarts 0) "s")
                                        ". Giving up.\n\n"
                                        (when-not (str/blank? recent-errors)
                                          (str "**Recent errors:**\n\n" recent-errors "\n\n"))
                                        "**What to try:** Simplify the request, break the task into smaller steps, "
                                        "or rephrase so the agent can use a different approach.")]
                  (merge {:answer fallback-answer
                          :status :error-budget-exhausted
                          :trace trace
                          :iterations iteration}
                    {:status-id (status->id :error-budget-exhausted)}
                    (finalize-cost)))))
            (let [;; Prefer the LLM's own next-iteration preference, fall back to
                  ;; error-driven escalation when it hasn't asked for one.
                  reasoning-level (when has-reasoning?
                                    (or prev-next-reasoning
                                      (reasoning-level-for-errors base-reasoning-level consecutive-errors)))
                   _ (log-stage! :iter-start iteration {:msg-count (count messages) :reasoning reasoning-level})
                   ;; `<prior_thinking>` shows ONLY the most recent
                   ;; iteration's reasoning + a breadcrumb pointing at
                   ;; `(var-history '*reasoning*)` for anything older.
                   ;; The agent pulls deeper context deliberately from
                   ;; :code, never automatically.
                   prior-thinking-body
                   (build-prior-thinking rlm-env db-info query-id)
                   ;; Iteration 0 of a NEW query (in an ongoing
                   ;; conversation) prepends a `[prior turn]` handover
                   ;; with last 2 reasonings + final from the previous
                   ;; query. Sub-RLMs skip this (they already get a
                   ;; targeted `[parent handoff]` from
                   ;; build-handoff).
                   cross-query-handover (when (zero? iteration)
                                          (build-cross-query-handover
                                            db-info
                                            (:conversation-id rlm-env)
                                            query-id
                                            (:parent-iteration-id rlm-env)))
                   prior-thinking (cond
                                    (and cross-query-handover prior-thinking-body)
                                    (str cross-query-handover "\n\n" prior-thinking-body)
                                    cross-query-handover cross-query-handover
                                    :else prior-thinking-body)
                   iteration-context (build-iteration-context rlm-env
                                       {:iteration              iteration
                                        :current-max-iterations (effective-max-iterations)
                                        :prior-thinking         prior-thinking
                                        :prev-expressions        prev-expressions
                                        :prev-iteration         prev-iteration
                                        :call-counts-atom       call-counts-atom})
                  ;; Preserve the FULL initial prompt (system + requirement +
                  ;; caller's prior transcript) across iterations. Was
                  ;; `(take 2 messages)`, which dropped cross-turn history
                  ;; restored by fix #1. Fix #2.
                  base-messages (trim-to-initial-history messages (count initial-messages))
                  effective-messages (cond-> base-messages
                                       (not (str/blank? iteration-context))
                                       (conj {:role "user" :content iteration-context}))
                    ;; Effective routing = caller's base `:routing` + LLM's
                    ;; `:next.model` override on `:optimize`. Caller's other
                    ;; routing keys (`:provider`, `:model`, etc.) survive.
                    effective-routing (cond-> (or routing {})
                                        prev-next-model (assoc :optimize prev-next-model))
                     iteration-result (try
                                        (run-iteration rlm-env effective-messages
                                          {:iteration-spec (if has-reasoning?
                                                             ITERATION_SPEC_REASONING
                                                             ITERATION_SPEC_NON_REASONING)
                                           :iteration iteration
                                           :reasoning-level reasoning-level
                                           :routing effective-routing})
                                       (catch Exception e
                                         (handle-iteration-exception! e
                                           {:iteration       iteration
                                            :messages        effective-messages
                                            :routing         effective-routing
                                            :reasoning-level reasoning-level})))]
              (if-let [iter-err (::iteration-error iteration-result)]
                ;; Error path: feed error back to LLM as user message, let it recover
                (let [error-feedback (str "[Iteration " (inc iteration) "/" (effective-max-iterations) "]\n"
                                       "<error>LLM call failed: " (:message iter-err) "</error>\n"
                                       "The previous attempt failed. Adjust your approach or call \final\": {\"answer\": \"your answer\", \"confidence\": \"high\"} with what you have.")
                      trace-entry {:iteration iteration :error iter-err :final? false}]
                     ;; Store error iteration snapshot — full ex-data/type/stack so the UI
                     ;; (and fine-tuning corpus) can inspect why the iteration blew up.
                     ;; When svar signals :svar.llm/empty-content (provider returned
                     ;; reasoning with no content), preserve that reasoning as the
                     ;; iteration's :thinking so DB + UI keep the model's thoughts
                     ;; for triage instead of a bare NULL row. The typed error
                     ;; survives in (:type iter-err) and its ex-data lives in
                     ;; (:data iter-err) — see the catch block above.
                     (let [empty-content-reasoning (when (= :svar.llm/empty-content (:type iter-err))
                                                     (:reasoning (:data iter-err)))
                           err-iter-id (db/store-iteration! db-info
                                          {:query-id query-id
                                           :vars []
                                           :expressions nil
                                           :thinking empty-content-reasoning
                                           :duration-ms 0
                                           :error iter-err
                                           :metadata (iter-metadata)})]
                       (when-let [a (:current-iteration-id-atom rlm-env)]
                         (reset! a err-iter-id)))
                   ;; Global observer hook after store-iteration! (error path)
                   (emit-hook! on-iteration
                     {:iteration iteration
                      :status :error
                      :status-id (status->id :error)
                      :thinking (when (= :svar.llm/empty-content (:type iter-err))
                                  (:reasoning (:data iter-err)))
                      :expressions nil
                      :final-result nil
                      :error iter-err
                      :duration-ms 0}
                     "on-iteration hook threw (error branch) — swallowing")
                  (recur (merge loop-state
                           {:iteration          (inc iteration)
                            :messages           (conj messages {:role "user" :content error-feedback})
                            :trace              (conj trace trace-entry)
                            :consecutive-errors (inc consecutive-errors)
                            :restarts           restarts}
                           FRESH_ITER_CARRY)))
                ;; Normal path — accumulate token usage
                (let [_ (accumulate-usage! (:api-usage iteration-result))
                      {:keys [thinking expressions final-result next-model next-reasoning forget]} iteration-result
                      _ (when (seq forget)
                          (forget-vars! (:sci-ctx rlm-env) forget)
                          (swap! var-index-atom update :current-revision inc))
                      ;; Auto-bind *reasoning* into the SCI sandbox after every iteration.
                      ;; The LLM (and var-history) can inspect prior reasoning via (var-history '*reasoning*).
                      ;; Bump `:current-revision` on every system-var mutation
                      ;; so the next `get-var-index` call rebuilds the cache.
                      ;; Otherwise a next turn that runs no `:code` would keep
                      ;; serving this turn's stale `<var_index>` to the LLM.
                       _ (when (seq thinking)
                           (sci-env/bind-and-bump! rlm-env '*reasoning* thinking))

                      ;; Auto-bind *answer* when the turn finalizes. Survives into the
                      ;; next turn in this conversation (vars persist across queries).
                      ;; (var-history '*answer*) → every prior turn's answer; most
                      ;; recent lives under the bare *answer* var.
                      final-answer (when final-result (:answer final-result))
                      _ (when final-result
                          (sci-env/bind-and-bump! rlm-env '*answer* final-answer))
                      vars-snapshot (restorable-var-snapshots rlm-env expressions)
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
                       iter-id (db/store-iteration! db-info
                                  {:query-id query-id
                                   :expressions expressions
                                   :vars vars-snapshot
                                   :thinking thinking
                                   :answer (when final-result (answer-str (:answer final-result)))
                                   :duration-ms (or (:duration-ms iteration-result) 0)
                                   :metadata (iter-metadata)})
                       ;; Publish the fresh iteration ref so any SCI
                       ;; code that subsequently calls query
                       ;; can parent its sub-query under this iter
                       ;; (see `:current-iteration-id-atom` on env).
                       _ (when-let [a (:current-iteration-id-atom rlm-env)]
                           (reset! a iter-id))
                      ;; Global observer hook after store-iteration! (success/empty/final)
                      _ (emit-hook! on-iteration
                          {:iteration iteration
                           :status (cond
                                     final-result :final
                                     (empty? expressions) :empty
                                     :else :success)
                           :status-id (status->id (cond
                                                    final-result :final
                                                    (empty? expressions) :empty
                                                    :else :success))
                           :thinking thinking
                           :expressions expressions
                           :final-result final-result
                           :error nil
                           :duration-ms (or (:duration-ms iteration-result) 0)}
                          "on-iteration hook threw (success branch) — swallowing")
                      trace-entry {:iteration iteration
                                   :thinking thinking
                                   :expressions expressions
                                   :final? (boolean final-result)}]
                  (if final-result
                    (do (log-stage! :final iteration
                          {:answer (truncate (answer-str (:answer final-result)) 200)
                           :confidence (:confidence final-result)
                           :iterations (inc iteration)})
                      (log-stage! :iter-end iteration
                        {:blocks (count expressions)
                         :errors (count (filter :error expressions))
                         :times (mapv :execution-time-ms expressions)})
                        ;; Fire final streaming callback
                      (when on-chunk
                        (on-chunk {:iteration iteration
                                   :thinking thinking
                                   :code (mapv :code expressions)
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
                       (if (empty? expressions)
                        ;; Empty iteration — spec-level `:code` is required so
                        ;; this branch should almost never fire; svar rejects
                        ;; responses missing the field via
                        ;; `:svar.spec/required-field-missing` or
                        ;; `:svar.spec/schema-rejected` (strict mode landed in
                        ;; svar 0.3.6+). It survives only as defence against a
                        ;; provider that silently skips enforcement. We do NOT
                        ;; re-persist (the iteration was already stored above
                        ;; with :expressions []) and we do NOT scold — the old
                        ;; soft nudge was dead weight duplicating what the
                        ;; spec description already says. All we do is
                        ;; propagate state so the agent's last real journal
                        ;; survives an empty "thinking only" blip (regression
                        ;; guarded by journal-preservation-test).
                         (do
                           (log-stage! :empty iteration {})
                          ;; Close the telemetry frame even for empty
                          ;; turns so `:iter-start :empty :iter-end`
                          ;; triples show in logs without a stray
                          ;; open-start. `blocks=0 errors=0` mirrors
                          ;; the no-exec state.
                          (log-stage! :iter-end iteration
                            {:blocks 0 :errors 0 :times []})
                          ;; Empty turn bumps `:iteration`, threads
                          ;; NEW model overrides, AND appends the
                          ;; `trace-entry` built above so the
                          ;; returned `:trace` reflects what the
                          ;; model actually did this turn. Before
                          ;; this was added, empty iters were
                          ;; persisted to SQLite but elided from the
                          ;; in-memory trace — `conversations/send!`
                          ;; returned a pruned trace, the web
                          ;; message cache persisted the prune, and
                          ;; the user saw `ITERATION 1` / `ITERATION
                          ;; 8` with six invisible thinking-only
                          ;; iterations between them (conversation
                          ;; 2e3bf18c). Regression guarded by
                          ;; trace-empty-iter-test. Every other
                          ;; value in `loop-state` carries through
                          ;; verbatim — including `prev-expressions`
                          ;; so the last real journal survives a
                          ;; "thinking only" blip.
                          (recur (merge loop-state
                                  {:iteration           (inc iteration)
                                   :trace               (conj trace trace-entry)
                                   :prev-next-model     next-model
                                   :prev-next-reasoning next-reasoning})))
                        ;; Normal iteration with expressions. No
                        ;; user-message composition here — every nudge
                        ;; that used to live inline (`repetition`,
                        ;; `budget`, `last-iteration`) now flows
                        ;; through `build-iteration-context` + nudges
                        ;; composers at the TOP of the next turn
                        ;; instead of being appended and immediately
                        ;; trimmed away by trim-to-initial-history.
                        (do
                        (log-stage! :iter-end iteration
                          {:blocks (count expressions)
                           :errors (count (filter :error expressions))
                           :times (mapv :execution-time-ms expressions)})
                        ;; Fire streaming callback so the web UI (and any
                        ;; other adapter) gets live updates per iteration,
                        ;; not just at finalize.
                        (when on-chunk
                          (on-chunk {:iteration iteration
                                     :thinking thinking
                                     :code (mapv :code expressions)
                                     :done? false}))
                        (let [had-successful-execution? (some #(nil? (:error %)) expressions)
                              next-errors (if had-successful-execution? 0 (inc consecutive-errors))
                              _ (when had-successful-execution?
                                  (swap! var-index-atom update :current-revision inc))]
                          ;; Normal success: next loop-state absorbs
                          ;; this iter's expressions as the journal for
                          ;; the next turn and threads model overrides.
                          (recur (merge loop-state
                                   {:iteration            (inc iteration)
                                    :messages             messages
                                    :trace                (conj trace trace-entry)
                                    :consecutive-errors   next-errors
                                    :prev-expressions      expressions
                                    :prev-iteration       iteration
                                    :prev-next-model      next-model
                                    :prev-next-reasoning  next-reasoning}))))))))))))))))

;; =============================================================================
;; Public API — environment lifecycle
;; =============================================================================
;; Placed after iteration-loop so all internal fns are resolved without declare.

(defn create-environment
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
   RLM environment map (component)."
  [router {:keys [db conversation channel external-id title]}]
  (when-not router
    (anomaly/incorrect! "Missing router" {:type :vis/missing-router}))
  (let [depth-atom (atom 0)
        db-info (create-rlm-conn db)
        var-index-atom (atom {:index nil :revision -1 :current-revision 0})
        qa-corpus-atom (atom {:snapshot-cache nil
                              :stats {:hits 0 :misses 0
                                      :last-digest-ms nil
                                      :last-revision 0}})
        state-atom (atom {:custom-bindings {}
                          :rlm-env nil
                          :conversation-id nil})
        rlm-env-atom (atom nil)
                env-id (str (util/uuid))
        root-model (or (:name (resolve-effective-model router)) "unknown")
        has-reasoning? (provider-has-reasoning? router)
        system-prompt (build-system-prompt {:has-reasoning? has-reasoning?
                                            })
        resolved-conversation-id (db/db-resolve-conversation-id db-info conversation)
        conversation-id (or resolved-conversation-id
                           (db/store-conversation! db-info
                             {:channel       (or channel :vis)
                              :external-id   external-id
                              :model         root-model
                              :title         title
                              :system-prompt system-prompt}))
        {:keys [sci-ctx sandbox-ns initial-ns-keys]}
        (sci-env/create-sci-context (:custom-bindings @state-atom))
        env {:env-id env-id
             :conversation-id conversation-id
             :depth-atom depth-atom
             :db-info db-info
             :var-index-atom var-index-atom
             :qa-corpus-atom qa-corpus-atom
             :state-atom state-atom
             :sci-ctx sci-ctx
             :sandbox-ns sandbox-ns
             :initial-ns-keys initial-ns-keys
             :router router
             :extensions (atom [])
}]
    (reset! rlm-env-atom env)
    (swap! state-atom assoc :rlm-env env :conversation-id conversation-id)
    ;; Install all globally registered extensions in dependency order.
    (ext/register-extensions! env register-extension!)
    env))

(defn dispose-environment!
  "Disposes an RLM environment and releases resources.

   For persistent DBs (created with :path), data is preserved.
   For disposable DBs, all data is deleted."
  [environment]
  (when-let [db-info (:db-info environment)]
    (dispose-rlm-conn! db-info)))

(defn register-extension!
  "Register a validated extension into the environment.

   Checks `:ext/requires` — if the extension declares dependencies,
   all listed extension namespaces must already be registered.
   Throws on missing dependencies.

   If an extension with the same `:ext/namespace` is already registered,
   it is replaced (not duplicated). This enables hot-swap via
   `reload-extension!`.

   Returns `environment` for chaining."
  [environment ext]
  (when-not (:extensions environment)
    (anomaly/incorrect! "Invalid RLM environment — missing :extensions atom"
      {:type :vis/invalid-env}))
  (when-let [requires (seq (:ext/requires ext))]
    (let [registered (into #{} (map :ext/namespace) @(:extensions environment))
          missing    (vec (remove registered requires))]
      (when (seq missing)
        (anomaly/incorrect!
          (str "Extension '" (:ext/namespace ext)
            "' requires " missing " but they are not registered. "
            "Register dependencies first.")
          {:type         :extension/missing-dependencies
           :extension    (:ext/namespace ext)
           :requires     (vec requires)
           :missing      missing
           :registered   (vec registered)}))))
  (swap! (:extensions environment)
    (fn [exts]
      (let [ns-sym  (:ext/namespace ext)
            without (vec (remove #(= (:ext/namespace %) ns-sym) exts))]
        (conj without ext))))
  environment)

;; =============================================================================
;; Entity Extraction — delegates to loop.knowledge.entity
;; =============================================================================

