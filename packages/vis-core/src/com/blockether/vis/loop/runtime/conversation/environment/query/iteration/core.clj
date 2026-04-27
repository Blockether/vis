(ns com.blockether.vis.loop.runtime.conversation.environment.query.iteration.core
  "Single LLM iteration.

   `run-iteration` calls the LLM via ask!, parses the structured response,
   executes code blocks, validates final answers. Returns a map describing
   everything that happened in one iteration.

   Also contains error-normalization helpers for infrastructure vs recoverable
   failures."
  (:require
   [clojure.set]
   [clojure.string :as str]
   [com.blockether.vis.loop.runtime.conversation.environment.core :as sci-env]

   [com.blockether.vis.persistance.core :as db]
   [com.blockether.vis.persistance.spec :as rlm-spec
    :refer [ITERATION_SPEC_NON_REASONING
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
(def ^:const RECENT_THOUGHT_MAX_CHARS 4000)
(def ^:const BREADCRUMBS_KEEP_LAST 20)
(def ^:const RECENT_KEEP_ITERS 1)
(def ^:const PLAN_MAX_ITEMS 20)

(defn- truncated-pr-str [v]
  (let [s (strip-sandbox-ns (pr-str v))]
    (if (> (count s) EXECUTION_SAFETY_CAP_CHARS)
      [(truncate s EXECUTION_SAFETY_CAP_CHARS) true]
      [s false])))

(defn- format-recent-block
  "Render the last `RECENT_KEEP_ITERS` iterations of expression results
   with iN.K addressable ids.

   `expressions-by-iteration` is a seq of `[iteration-position [exprs]]` pairs, ordered
   oldest-first.. Only the most recent iteration is shipped
   to keep the projection bounded; the breadcrumb chain (one line per iteration)
   plus the addressable `iN.K` ids in <attempts> cover everything
   else the model needs to reference."
  [expressions-by-iteration]
  (let [kept (take-last RECENT_KEEP_ITERS (or expressions-by-iteration []))]
    (when (seq kept)
      (let [lines (for [[iteration-position exprs] kept
                        [k expr]         (map-indexed vector exprs)
                        :let [{:keys [code error result stdout stderr execution-time-ms]} expr]]
                    (let [code-str      (str/trim (or code ""))
                          stdout-suffix (when-not (str/blank? stdout)
                                          (str " :stdout " (pr-str stdout)))
                          stderr-suffix (when-not (str/blank? stderr)
                                          (str " :stderr " (pr-str (truncate stderr EXECUTION_STDERR_CHARS))))
                          time-ms       (or execution-time-ms 0)
                          slow-suffix   (when (> time-ms SLOW_EXECUTION_MS)
                                          (str " (" time-ms "ms SLOW)"))
                          value-part    (if error
                                          (str "ERROR: " (truncate error 400))
                                          (let [v (realize-value result)
                                                [value-str truncated?] (truncated-pr-str v)]
                                            (str value-str
                                              (when truncated? " :truncated? true"))))]
                      (str "  i" iteration-position "." (inc k) "  " code-str " → " value-part
                        (or slow-suffix "")
                        (or stdout-suffix "")
                        (or stderr-suffix ""))))]
        (when (seq lines)
          (str "<recent>\n" (str/join "\n" lines) "\n</recent>"))))))

;; -- Plan-as-first-class-slot helpers --------------------------
;;
;; Two complementary projections of the agent's reasoning state:
;;
;;   <plan>          sticky structured TODO list, carried verbatim from
;;                   the most-recent iteration row that emitted a :plan. The
;;                   model only re-emits it when reality forces a real
;;                   change; otherwise the loop carries it forward.
;;
;;   <breadcrumbs>   bounded one-liner chain (last K=20). Built from
;;                   the :breadcrumb column of recent iteration rows, ordered
;;                   oldest-first, prefixed by "iN ".
;;
;; The plan never gets re-summarized through tactical iterations, and
;; the breadcrumbs preserve the strategic frame at one line per iteration.

(def ^:private PLAN_STATUS_DISPLAY
  {:in_progress "in_progress"
   :in-progress "in_progress"  ;; tolerate kebab-case input
   :pending     "pending"
   :done        "done"
   :blocked     "blocked"})

(defn- normalize-plan-status [s]
  (cond (keyword? s) (or (PLAN_STATUS_DISPLAY s)
                       (PLAN_STATUS_DISPLAY (-> s name (str/replace "_" "-") keyword))
                       (name s))
    (string? s)  (or (PLAN_STATUS_DISPLAY (keyword s)) s)
    :else        "pending"))

(defn- plan-item-line [{:keys [id content status evidence]}]
  (let [status-s (normalize-plan-status status)
        ev-s    (when (and evidence (not (str/blank? (str evidence))))
                  (str "  (" (str/trim (str evidence)) ")"))]
    (str "  [" id "] " status-s " — " (str/trim (str content)) (or ev-s ""))))

(defn- format-plan-block
  "Render structured plan as the model-facing `<plan>` block. Returns nil
   when no plan ever emitted (model has not decomposed yet) so the model
   sees a clean iteration-0 prompt instead of an empty wrapper."
  [plan-state]
  (when (and (map? plan-state) (or (:goal plan-state) (seq (:items plan-state))))
    (let [{:keys [goal items open decided]} plan-state
          item-lines (map plan-item-line (or items []))
          open-lines (when (seq open) (map #(str "  - " (str/trim (str %))) open))
          decided-lines (when (seq decided) (map #(str "  - " (str/trim (str %))) decided))]
      (str "<plan>\n"
        (when goal (str "  goal: " (str/trim (str goal)) "\n"))
        (when (seq item-lines) (str (str/join "\n" item-lines) "\n"))
        (when (seq open-lines)
          (str "  open:\n" (str/join "\n" (map #(str "  " %) open-lines)) "\n"))
        (when (seq decided-lines)
          (str "  decided:\n" (str/join "\n" (map #(str "  " %) decided-lines)) "\n"))
        "</plan>"))))

(defn- format-breadcrumbs-block
  "Render the cumulative breadcrumb chain as `<breadcrumbs>`. Each entry
   is a one-line breadcrumb authored by the model in :breadcrumb. Bounded
   at last K=20. Ordered oldest-first (matches reading order)."
  [breadcrumb-rows]
  (let [entries (->> breadcrumb-rows
                  (keep (fn [{:keys [position breadcrumb]}]
                          (when (and breadcrumb (not (str/blank? breadcrumb)))
                            (str "  i" position "  " (str/trim breadcrumb)))))
                  (take-last BREADCRUMBS_KEEP_LAST))]
    (when (seq entries)
      (str "<breadcrumbs>\n" (str/join "\n" entries) "\n</breadcrumbs>"))))

(defn- format-recent-thought-block
  "Render the most-recent iteration's :thinking text under <recent_thought>.
   Hard-capped at RECENT_THOUGHT_MAX_CHARS to keep the projection bounded.

   This replaces the old <prior_thinking> wrapper. The semantic shift:
   <recent_thought> is explicitly the *current* tactical step's free-form
   text; the strategic frame lives in <plan> + <breadcrumbs> instead."
  [thinking]
  (when (and (string? thinking) (not (str/blank? thinking)))
    (str "<recent_thought>\n"
      (truncate thinking RECENT_THOUGHT_MAX_CHARS)
      "\n</recent_thought>")))

(defn- format-iteration-info
  "Render the per-loop ITERATION pointer as a single inline map. Lives
   inside <system_state> next to QUERY/ANSWER/REASONING because it is
   the same flavor of data: 'what the loop knows about your current
   pointer.' Replaces the standalone `[iteration N/M]` header line.

   Returns nil when iteration data is missing (test fixtures with no
   loop bookkeeping); the parent block renders without ITERATION in
   that case."
  [{:keys [iteration current-max-iterations]}]
  (when (and (some? iteration) (some? current-max-iterations))
    (let [current   (long iteration)
          one-based (inc current)
          budget    (long current-max-iterations)
          remaining (max 0 (- budget one-based))]
      (str "  ITERATION  {:current " one-based
        " :budget " budget
        " :remaining " remaining "}"))))

(defn- format-system-state-block
  "Render the always-present <system_state> block: loop-managed pointers
   in one place — SYSTEM vars (QUERY / ANSWER / REASONING) plus the
   current ITERATION pointer plus the bounded PRIOR_TURN digest.

   SYSTEM-var names follow `sci-env/SYSTEM_VAR_NAMES` (UPPERCASE, no
   earmuffs). PRIOR_TURN and ITERATION are digest / pointer
   pseudo-names — NOT real sandbox bindings, just rendered keys grouped
   here for the model's convenience.

   Bounded by construction:
   - SYSTEM-var values truncated to 500 chars each.
   - PRIOR_TURN ships only `:goal :counts :outcome :abandon-reason`,
     never the full plan body or transcript.
   - ITERATION is a 3-key map; size is rounding error."
  [{:keys [system-vars prior-turn iteration current-max-iterations]}]
  (let [vars-lines (->> [["QUERY"     (:QUERY system-vars)]
                         ["ANSWER"    (:ANSWER system-vars)]
                         ["REASONING" (:REASONING system-vars)]]
                     (keep (fn [[label v]]
                             (when (some? v)
                               (let [s (pr-str v)]
                                 (str "  " label "  " (truncate s 500)))))))
        iteration-line (format-iteration-info
                         {:iteration              iteration
                          :current-max-iterations current-max-iterations})
        prior-lines (when (seq prior-turn)
                      [(str "  PRIOR_TURN {:goal "
                         (pr-str (or (:goal prior-turn) ""))
                         " :outcome " (or (:outcome prior-turn) :unknown)
                         (when (:abandon-reason prior-turn)
                           (str " :abandon-reason " (pr-str (:abandon-reason prior-turn))))
                         (when (:counts prior-turn)
                           (str " :counts " (pr-str (:counts prior-turn))))
                         "}")])
        all-lines (concat vars-lines
                    (when iteration-line [iteration-line])
                    prior-lines)]
    (when (seq all-lines)
      (str "<system_state>\n" (str/join "\n" all-lines) "\n</system_state>"))))

;; -- Cross-field plan validation -----------------------------------------
;;
;; svar's spec engine validates structural shape; the cross-field
;; rules (≤20 items, exactly-one :in_progress, monotonic ids) are
;; checked here in Clojure and surfaced as a structured error map
;; that the iteration loop renders into a `[system_nudge]` line via
;; `format-loop-nudge`.

(declare item-status-key)

(defn validate-plan-state
  "Returns nil when the plan is valid, or an error map shaped as
   `{:type :message :data}`. Side-effect-free; the caller hands the
   result to `format-loop-nudge` to render the user-facing string."
  [plan-state]
  (when (map? plan-state)
    (let [items (or (:items plan-state) [])
          in-progress-items (filter #(= :in_progress (item-status-key %)) items)
          ids (mapv :id items)]
      (cond
        (> (count items) PLAN_MAX_ITEMS)
        {:type :vis/plan-too-large
         :message (str "<plan>.items has " (count items)
                    " entries; max is " PLAN_MAX_ITEMS
                    ". Merge or drop items.")
         :data {:item-count (count items) :max PLAN_MAX_ITEMS}}

        (> (count in-progress-items) 1)
        {:type :vis/plan-multiple-in-progress
         :message (str "<plan> has " (count in-progress-items)
                    " :in_progress items. Set exactly one :in_progress at a time.")
         :data {:in-progress-ids (mapv :id in-progress-items)}}

        (and (seq ids) (not= ids (sort ids)))
        {:type :vis/plan-non-monotonic-ids
         :message "<plan>.items :id values must be monotonic and unique. Don't reuse ids."
         :data {:ids ids}}))))

(defn item-status-key
  "Normalize a plan item's :status to a canonical keyword
   (`:pending`, `:in_progress`, `:done`, `:blocked`). Tolerates both
   keyword and string inputs."
  [item]
  (let [s (:status item)
        k (cond (keyword? s) s
            (string? s) (keyword s)
            :else :pending)]
    (case k
      :in-progress :in_progress
      k)))

;; ---------------------------------------------------------------------------
;; Loop nudge formatter — the user-facing string the model sees on the
;; NEXT iteration when the loop rejects the prior iteration's output
;; (PEV gate, plan validation, etc.). Mirrors `format-iteration-error`
;; for trace `:error` entries: takes a structured violation map and
;; produces the `[system_nudge]`-prefixed directive line.
;; ---------------------------------------------------------------------------

(defn format-loop-nudge
  "Render a loop-level violation map into the `[system_nudge]`-prefixed
   directive line. Returns nil for nil input so callers can `(keep
   format-loop-nudge …)` over a sparse vec.

   Recognized `:type` values supply a canonical message body when
   `:message` is absent; arbitrary `:type` falls through to a generic
   `Loop violation: <type>` line."
  [{:keys [type message data] :as violation}]
  (when violation
    (let [body (or message
                 (case type
                   :vis/incomplete-plan-on-answer
                   (let [open-ids (:open-item-ids data)]
                     (str "Cannot finalize: plan items "
                       (str/join ", " (map #(str "[" % "]") open-ids))
                       " are :in_progress / :pending. Either complete them with cited "
                       "evidence (set :status :done with :evidence iN.K), or set "
                       ":abandon-reason describing what blocks completion."))
                   (str "Loop violation: " (or type :unknown))))]
      (str "[system_nudge] " body))))

;; ---------------------------------------------------------------------------
;; Plan diff — powers the plan-edit-distance metric and the
;; <breadcrumbs> annotation when a plan changes between iterations.
;; ---------------------------------------------------------------------------

(defn compute-plan-diff
  "Diff two plan-state maps. Returns nil when identical, or a map
   `{:added [ids] :removed [ids] :status-changed [{id :from :to}] :goal-changed? bool}`.
   Callers compute `(count added)+(count removed)+(count status-changed)`
   for the `plan-edit-distance` metric."
  [previous-plan new-plan]
  (when (or (some? previous-plan) (some? new-plan))
    (let [previous-items (into {} (map (juxt :id identity) (or (:items previous-plan) [])))
          new-items  (into {} (map (juxt :id identity) (or (:items new-plan) [])))
          previous-ids   (set (keys previous-items))
          new-ids    (set (keys new-items))
          added      (sort (clojure.set/difference new-ids previous-ids))
          removed    (sort (clojure.set/difference previous-ids new-ids))
          shared     (sort (clojure.set/intersection previous-ids new-ids))
          status-changes (vec
                           (keep (fn [id]
                                   (let [from (item-status-key (get previous-items id))
                                         to   (item-status-key (get new-items id))]
                                     (when (not= from to)
                                       {:id id :from from :to to})))
                             shared))
          goal-changed? (not= (:goal previous-plan) (:goal new-plan))
          changed? (or (seq added) (seq removed) (seq status-changes) goal-changed?)]
      (when changed?
        {:added (vec added)
         :removed (vec removed)
         :status-changed status-changes
         :goal-changed? goal-changed?}))))

(defn plan-edit-distance
  "Sum of additions, removals, and status changes. 0 → plan unchanged."
  [diff]
  (if (nil? diff)
    0
    (+ (count (:added diff))
      (count (:removed diff))
      (count (:status-changed diff)))))

(defn assemble-initial-messages [{:keys [system-prompt initial-user-content history-messages]}]
  (into [{:role "system" :content system-prompt}
         {:role "user"   :content initial-user-content}]
    (or history-messages [])))

(defn trim-to-initial-history [messages initial-count]
  (vec (take initial-count messages)))

(defn- read-var-index-str
  "Lazily build (and cache) the `<var_index>` body for the active env.
   Returns nil when the env has no SCI context (test fixtures that exercise
   only the projection layer)."
  [environment]
  (when-let [sci-ctx (:sci-ctx environment)]
    (let [var-index-atom (or (:var-index-atom environment)
                           (atom {:index nil :revision -1 :current-revision 0}))
          {:keys [index revision current-revision]} @var-index-atom]
      (if (= revision current-revision)
        index
        (let [sandbox-map (get-in @(:env sci-ctx)
                            [:namespaces 'sandbox])
              idx         (sci-env/build-var-index
                            sci-ctx (:initial-ns-keys environment)
                            sandbox-map
                            (:db-info environment) (:conversation-id environment)
                            nil)
              live-rev    (:current-revision @var-index-atom)]
          (swap! var-index-atom assoc :index idx :revision live-rev)
          idx)))))

;; -- Sticky-plan loader ---------------------------------------
;;
;; The plan slot is sticky: the model writes it once and the loop carries
;; the most-recent persisted plan forward across iterations until the model
;; re-emits one. This loader returns the latest non-nil plan_state from the
;; current query's iteration rows; nil when no plan has ever been emitted.

(defn load-effective-plan
  "Most-recent persisted :plan-state for this query, or nil. Reads the
   iteration rows in DB-time order; takes the last row whose plan-state
   is non-nil. This is the implementation of sticky-carry semantics:
   when iteration K omits :plan, we still know what the model's plan was."
  [db-info query-id]
  (when (and db-info query-id)
    (try
      (let [iters (db/db-list-query-iterations db-info query-id)]
        (some :plan-state (reverse iters)))
      (catch Throwable t
        (tel/log! {:level :warn
                   :data {:error (ex-message t) :query-id query-id}
                   :msg "load-effective-plan failed"})
        nil))))

(defn load-breadcrumb-chain
  "Vector of `{:position int :breadcrumb str}` for this query, oldest-first,
   capped at last `BREADCRUMBS_KEEP_LAST` entries. Skips iters with no
   breadcrumb so the chain is dense."
  [db-info query-id]
  (when (and db-info query-id)
    (try
      (let [iters (db/db-list-query-iterations db-info query-id)]
        (->> iters
          (keep-indexed (fn [idx it]
                          (when (and (:breadcrumb it)
                                  (not (str/blank? (:breadcrumb it))))
                            {:position idx :breadcrumb (:breadcrumb it)})))
          (take-last BREADCRUMBS_KEEP_LAST)
          vec))
      (catch Throwable t
        (tel/log! {:level :warn
                   :data {:error (ex-message t) :query-id query-id}
                   :msg "load-breadcrumb-chain failed"})
        []))))

(defn load-prior-turn-digest
  "Bounded handover digest for the previous query in the same conversation.
   Returns nil when there is no prior turn (first turn of conversation).
   Replaces the old `build-cross-query-handover` which shipped raw
   thinking strings; PLAN.md §5.5 caps this at goal/counts/outcome only."
  [db-info conversation-id current-query-id]
  (when (and db-info conversation-id)
    (try
      (let [all-queries (sort-by :created-at
                          (db/db-list-conversation-queries db-info conversation-id))
            prior       (last (remove #(= (:id %) current-query-id) all-queries))]
        (when prior
          (let [final-plan   (load-effective-plan db-info (:id prior))
                items        (or (:items final-plan) [])
                bucket       (frequencies (map item-status-key items))
                outcome      (or (:prior-outcome prior)
                               ;; Fallback when :prior-outcome is missing — derive
                               ;; from query status.
                               (case (:status prior)
                                 :done :complete
                                 :error :error
                                 :interrupted :cancelled
                                 :unknown))
                abandon-reason (when (and (= outcome :abandoned)
                                       (not (str/blank? (str (:answer prior)))))
                                 (str (:answer prior)))]
            {:goal           (:goal final-plan)
             :counts         (-> {}
                               (assoc :done (or (:done bucket) 0))
                               (assoc :pending (or (:pending bucket) 0))
                               (assoc :in_progress (or (:in_progress bucket) 0))
                               (assoc :blocked (or (:blocked bucket) 0)))
             :outcome        outcome
             :abandon-reason abandon-reason})))
      (catch Throwable t
        (tel/log! {:level :warn
                   :data {:error (ex-message t) :conversation-id conversation-id}
                   :msg "load-prior-turn-digest failed"})
        nil))))

;; ---------------------------------------------------------------------------
;; Nudges — per-iteration system hints injected into the iteration context
;; ---------------------------------------------------------------------------

(def ^:private BUDGET_WARNING_WINDOW 2)
;; A single repeat is enough signal: with <attempts> in the projection,
;; a single repeat is enough signal that the model should change strategy.
(def ^:private REPETITION_THRESHOLD 1)

(defn- budget-warning
  [{:keys [iteration current-max-iterations]}]
  (let [iteration (long (or iteration 0))
        max-iters (long (or current-max-iterations 0))
        remaining (- max-iters (inc iteration))]
    (when (<= remaining BUDGET_WARNING_WINDOW)
      (str "[system_nudge] " (max 0 remaining) " iteration(s) left. "
        "Either finalize with :answer NOW, or call (request-more-iterations N) in :code alongside your other operations to extend."))))

(defn- repetition-warning
  [call-counts-atom previous-expressions]
  (when (and call-counts-atom (seq previous-expressions))
    (let [keys* (mapv (fn [{:keys [code error result]}]
                        (if error
                          [:error-only (str/trim (str error))]
                          [(str/trim (str code)) (pr-str result)]))
                  previous-expressions)
          max-count (swap! call-counts-atom
                      (fn [m]
                        (reduce (fn [acc k] (update acc k (fnil inc 0)))
                          (or m {}) keys*)))
          seen (apply max 0 (map #(get max-count % 0) keys*))]
      (when (>= seen REPETITION_THRESHOLD)
        "[system_nudge] You are repeating the same expression pattern. Change strategy."))))

(defn- collect-extension-nudges
  "Invoke `:ext/nudge-fn` on every PRECOMPUTED active extension and
   collect the resulting strings. Activation-fn is NOT evaluated here —
   `extensions` is expected to be the seq returned by
   `loop-core/active-extensions` once at query start. This keeps
   activation single-fire-per-query even though nudges fire per-iteration."
  [extensions ctx]
  (when (seq extensions)
    (into []
      (keep (fn [ext]
              (when-let [nudge-fn (:ext/nudge-fn ext)]
                (try
                  (let [result (nudge-fn ctx)]
                    (when (and (string? result) (not (str/blank? result)))
                      result))
                  (catch Throwable t
                    (tel/log! {:level :warn :data {:ext (:ext/namespace ext) :error (ex-message t)}})
                    nil)))))
      extensions)))

;; ---------------------------------------------------------------------------
;; Iteration context builder
;; ---------------------------------------------------------------------------

(defn build-iteration-context
  "Assemble the per-iteration trailing user message.

   Layered shape:
     <plan>          — sticky structured TODO list (PLAN_STATE_SPEC)
     <breadcrumbs>   — last K=20 one-liners, oldest-first
     <recent>        — last iteration's expressions with iN.K addressable ids
     <recent_thought> — last iteration's :thinking text (≤4000c)
     <system_state>  — QUERY/ANSWER/REASONING + ITERATION pointer + PRIOR_TURN digest
     <var_index>     — user-defined vars
     [system_nudge]  — loop / budget / repetition / extension nudges

   Note: the per-iteration pointer lives in
   `<system_state>.ITERATION` so all loop-managed pointers live in
   one coherent block instead of fragmented across the projection.

   Required opts:
     `:active-extensions` — vec returned by `(loop-core/active-extensions env)`.
        Caller MUST compute this exactly ONCE per query and thread it through
        every iteration. This fn does NOT re-evaluate `:ext/activation-fn`.

   Optional:
     `:iteration`, `:current-max-iterations`, `:plan-state`,
     `:breadcrumbs`, `:expressions-by-iteration`, `:recent-thought`,
     `:system-vars`, `:prior-turn`, `:call-counts-atom`,
     `:loop-nudges` (vec of strings; gate-violation / plan-validation
        directives that the loop wants to surface to the model on the
        NEXT iteration —). Each entry is
        rendered as a `[system_nudge] …` line, prefix added if
        missing.

   Returns the joined block or nil when every component is blank."
  [environment {:keys [iteration current-max-iterations
                       plan-state breadcrumbs
                       expressions-by-iteration recent-thought
                       system-vars prior-turn
                       call-counts-atom
                       loop-nudges
                       active-extensions] :as opts}]
  (when-not (contains? opts :active-extensions)
    (throw (ex-info "build-iteration-context requires :active-extensions — compute once per query via (loop-core/active-extensions env)"
             {:type :vis/missing-active-extensions})))
  (let [plan-block (format-plan-block plan-state)
        breadcrumbs-block (format-breadcrumbs-block breadcrumbs)
        recent-block (format-recent-block expressions-by-iteration)
        ;; Repetition warning still consumes flat (last-iteration) expressions —
        ;; pluck them out of the canonical pairs once.
        last-iteration-expressions (some-> expressions-by-iteration last second)
        recent-thought-block (format-recent-thought-block recent-thought)
        system-state-block (format-system-state-block
                             {:system-vars            system-vars
                              :prior-turn             prior-turn
                              :iteration              iteration
                              :current-max-iterations current-max-iterations})
        var-index-str (read-var-index-str environment)
        var-block (when (and (string? var-index-str)
                          (not (str/blank? var-index-str)))
                    (str "<var_index>\n" var-index-str "\n</var_index>"))
        nudge-ctx {:environment            environment
                   :iteration              iteration
                   :current-max-iterations current-max-iterations
                   :previous-expressions   last-iteration-expressions
                   :plan-state             plan-state
                   :user-var-count         0}
        ;; Loop nudges (gate-violation, plan-validation, etc.) are
        ;; injected by the iteration loop based on the PRIOR iteration's
        ;; outcome; rendered as `[system_nudge]` lines so the model
        ;; sees them with the same UX as built-in/extension nudges.
        loop-nudge-lines (->> (or loop-nudges [])
                           (keep (fn [n]
                                   (when (and (string? n) (not (str/blank? n)))
                                     (if (str/starts-with? n "[system_nudge]")
                                       n
                                       (str "[system_nudge] " n))))))
        built-in-nudges (keep identity
                          [(when (and iteration current-max-iterations)
                             (budget-warning
                               {:iteration              iteration
                                :current-max-iterations current-max-iterations}))
                           (repetition-warning call-counts-atom last-iteration-expressions)])
        ext-nudges (collect-extension-nudges active-extensions nudge-ctx)
        all-nudges (concat loop-nudge-lines built-in-nudges ext-nudges)
        nudges-block (when (seq all-nudges)
                       (str/join "\n" all-nudges))
        parts (keep (fn [p] (when (and (string? p) (not (str/blank? p))) p))
                [plan-block
                 breadcrumbs-block
                 recent-block
                 recent-thought-block
                 system-state-block
                 var-block
                 nudges-block])]
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

(defn- exception->iteration-error-data
  "Normalize an exception into the iteration-error-data map stored on the query row.
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
      (let [iteration-error-data (exception->iteration-error-data e ctx)]
        (tel/log! {:level :warn
                   :data (assoc (format-exception-short e) :iteration iteration)}
          "RLM iteration failed, feeding error to LLM")
        {::iteration-error iteration-error-data}))))

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
  [environment messages & [{:keys [iteration-spec routing iteration reasoning-level resolved-model on-chunk]
                            :or {iteration-spec ITERATION_SPEC_NON_REASONING}}]]
  (binding [*rlm-ctx* (merge *rlm-ctx* {:rlm-phase :run-iteration})]
    (let [effective-reasoning (when (some? reasoning-level)
                                (or (normalize-reasoning-level reasoning-level)
                                  (throw (ex-info "Invalid :reasoning-level."
                                           {:type :vis/invalid-reasoning-level
                                            :got reasoning-level}))))
          ;; Stream reasoning chunks to the TUI while the LLM is thinking
          streaming-fn (when on-chunk
                         (fn [{:keys [reasoning done?]}]
                           (when (or (some? reasoning) done?)
                             (on-chunk {:iteration iteration
                                        :thinking  (some-> reasoning str)
                                        :code      nil
                                        :done?     (boolean done?)}))))
          ask-result (binding [llm/*log-context* {:query-id (:environment-id environment) :iteration iteration}]
                       (llm/ask! (:router environment)
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
                     :prompt_tokens_details {:cached_tokens (get-in ask-result [:tokens :cached] 0)}}
          ;; �� surface plan slot fields from svar parse so
          ;; downstream `store-iteration!` can persist them. Keep keys
          ;; absent when blank/missing.
          plan-state-raw  (:plan parsed)
          breadcrumb-raw  (:breadcrumb parsed)
          abandon-reason  (some-> (:abandon-reason parsed) str str/trim not-empty)]
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
              expression-results (when (seq code-blocks)
                                   (mapv (fn [{:keys [expr time-ms]}]
                                           (if-let [err (literal-code-block-error expr)]
                                             {:result nil :error err
                                              :stdout "" :stderr "" :execution-time-ms 0}
                                             (execute-code environment expr :timeout-ms time-ms)))
                                     code-entries))
              expression-errors (when expression-results
                                  (seq (clojure.core/filter :error expression-results)))
              raw-answer (str raw-final-answer)
              locals (try (get-locals environment) (catch Throwable _ {}))
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
                                 (when expression-errors
                                   (str "Code errors before final: " (:error (first expression-errors))))

                                 mustache-missing)
              expressions (when expression-results
                            (mapv (fn [idx code result]
                                    {:id idx :code code
                                     :result (:result result) :stdout (:stdout result)
                                     :stderr (:stderr result) :error (:error result)
                                     :execution-time-ms (:execution-time-ms result)
                                     :repaired? (:repaired? result)})
                              (range) code-blocks expression-results))]
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
                                 (:reasoning parsed)  (assoc :reasoning (:reasoning parsed))
                                 abandon-reason       (assoc :abandon-reason abandon-reason))]
              (cond-> {:thinking thinking
                       :next-model next-model :next-reasoning next-reasoning
                       :expressions (strip-noop-expressions expressions)
                       :final-result final-result :api-usage api-usage
                       :duration-ms (or (:duration-ms ask-result) 0)
                       :llm-messages messages :llm-model (str resolved-model)}
                plan-state-raw (assoc :plan-state plan-state-raw)
                breadcrumb-raw (assoc :breadcrumb breadcrumb-raw)))))
        ;; Normal path: execute code blocks
        (let [normalized (vec (:code parsed))
              ;; Coalesce fragments: join unbalanced blocks with the next
              coalesced (loop [remaining normalized
                               result []]
                          ;; Without form-repair, pass blocks through as-is
                          (if (empty? remaining)
                            result
                            (recur (rest remaining) (conj result (first remaining)))))
              total-blocks (count coalesced)
              executed (mapv (fn [idx {:keys [expr time-ms]}]
                               (log-stage! :code-exec iteration
                                 {:idx (inc idx) :total total-blocks :code expr :time-ms time-ms})
                               (let [result (if-let [err (literal-code-block-error expr)]
                                              {:result nil :error err :stdout "" :stderr "" :execution-time-ms 0}
                                              (let [r (execute-code environment expr :timeout-ms time-ms)]
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
          (cond-> {:thinking thinking
                   :next-model next-model :next-reasoning next-reasoning
                   :expressions (strip-noop-expressions expressions) :final-result nil :api-usage api-usage
                   :duration-ms (or (:duration-ms ask-result) 0)
                   :llm-messages messages :llm-model (str resolved-model)}
            plan-state-raw (assoc :plan-state plan-state-raw)
            breadcrumb-raw (assoc :breadcrumb breadcrumb-raw)))))))
