(ns com.blockether.vis.loop.runtime.conversation.environment.query.core
  "vis! orchestration: context prep, iteration loop, finalization."
  (:require
   [clojure.string :as str]
   [com.blockether.anomaly.core :as anomaly]
   [com.blockether.svar.internal.llm :as llm]
   [com.blockether.svar.internal.router :as router]
   [com.blockether.vis.config :as config]
   [com.blockether.vis.error :as vis-error]
   [com.blockether.vis.loop.core :as loop-core]
   [com.blockether.vis.persistance.core :as db]

   [com.blockether.vis.loop.runtime.conversation.environment.query.iteration.core :as iterate]
   [com.blockether.vis.loop.runtime.conversation.environment.query.iteration.spec
    :refer [ITERATION_SPEC_NON_REASONING ITERATION_SPEC_REASONING]]
   [com.blockether.vis.loop.runtime.conversation.environment.query.runtime :as query-runtime
    :refer [*rlm-context*]]
   [com.blockether.vis.loop.runtime.conversation.environment.core :as sci-env]
   [com.blockether.svar.internal.util :as util]
   [edamame.core :as edamame]
   [taoensso.telemere :as tel])
  (:import
   [java.util.concurrent ConcurrentHashMap Semaphore]))

;; -----------------------------------------------------------------------------
;; Core helpers
;; -----------------------------------------------------------------------------

(defn- truncate [s n]
  (let [s (str s)] (if (> (count s) n) (subs s 0 n) s)))

(defn- format-iteration-error
  "Render one trace `:error` map as a Markdown bullet for the user.
   Always includes the wrapper message; appends the raw provider
   response when the spec layer captured one (`svar.spec/schema-rejected`
   stashes the literal model output under `:data :raw-data`). Without
   this, errors like \"Your organization does not have access to Claude\"
   were stored in the DB but the user only saw the schema-rejection
   wrapper text. The raw response is the actually-useful part."
  [err]
  (let [msg  (or (:message err) (str err))
        data (:data err)
        raw  (some-> (:raw-data data) str)
        recv (:received-type data)
        body (when (and raw (not (str/blank? raw)))
               (truncate raw 400))]
    (cond-> (str "- " msg)
      body (str "\n  provider returned"
             (when recv (str " (" recv ")"))
             ": " body))))

(defn- recent-errors-block
  "Render the last `n` trace `:error` entries as a Markdown block
   (\"**Recent provider errors:**\\n\\n- …\"). Returns nil when the
   trace carries no errors so callers can `(when …)` without churn."
  [trace n]
  (let [errs (->> trace reverse (keep :error) (take n))]
    (when (seq errs)
      (str "**Recent provider errors:**\n\n"
        (str/join "\n" (map format-iteration-error errs))
        "\n\n"))))

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

;; -----------------------------------------------------------------------------
;; Router lifecycle + model helpers (query single-file API)
;; -----------------------------------------------------------------------------

(defonce ^:private router-atom (atom nil))

(defn get-router
  "Get or create the shared LLM router."
  []
  (or @router-atom
    (let [cfg (config/resolve-config)
          r   (llm/make-router (:providers cfg))]
      (reset! router-atom r)
      r)))

(defn reset-router!
  []
  (reset! router-atom nil))

(defn rebuild-router!
  "Rebuild the router from the given config. Used when provider settings change."
  [config]
  (let [r (llm/make-router (:providers config))]
    (reset! router-atom r)
    r))

(defn ask!
  [opts]
  (llm/ask! (get-router) opts))

(defn resolve-effective-model
  "Best-effort root model descriptor from router config."
  ([router]
   (first (mapcat :models (:providers router))))
  ([router _routing-overrides]
   (resolve-effective-model router)))

(defn provider-has-reasoning?
  [router]
  (boolean (:reasoning? (resolve-effective-model router))))

;; -----------------------------------------------------------------------------
;; Concurrency primitives (reentrant semaphore, deadline helpers)
;; -----------------------------------------------------------------------------

(defn make-reentrant-semaphore
  "Creates a reentrant semaphore with `permits` slots.
   Thread-id keyed: same thread can re-acquire without blocking.
   Different threads contend for permits fairly (FIFO).

   Returns a map with :acquire! and :release! fns."
  [permits]
  (let [sem (Semaphore. (int permits) true)
        thread-depths (ConcurrentHashMap.)]
    {:acquire!
     (fn acquire! []
       (let [tid (.getId (Thread/currentThread))
             depth (.getOrDefault thread-depths tid (int 0))]
         (if (pos? depth)
           (.put thread-depths tid (int (inc depth)))
           (do (.acquire sem)
             (.put thread-depths tid (int 1))))))

     :release!
     (fn release! []
       (let [tid (.getId (Thread/currentThread))
             depth (.getOrDefault thread-depths tid (int 0))]
         (when (pos? depth)
           (if (= depth 1)
             (do (.remove thread-depths tid)
               (.release sem))
             (.put thread-depths tid (int (dec depth)))))))

     :permits (fn [] (.availablePermits sem))
     :queued  (fn [] (.getQueueLength sem))}))

;; -----------------------------------------------------------------------------
;; Var snapshot + system var helpers (inlined from former query/shared)
;; -----------------------------------------------------------------------------

(defn extract-def-names
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
                  (catch Exception _ [])))))
    (map str)
    vec))

(defn restorable-var-snapshots
  "Returns serializable snapshots of user vars introduced by this iteration.

   Includes both explicit `(def ...)` vars and runtime autobind vars
   emitted by extension symbols via `:autobind-fn`."
  [environment expressions]
  (let [execution->defs (mapv (fn [{:keys [error] :as execution}]
                                [execution (when-not error
                                             (set (map symbol (extract-def-names [execution]))))])
                          expressions)
        defined (into #{} (mapcat second) execution->defs)
        symbol->execution (reduce (fn [acc [{:keys [code execution-time-ms]} defs]]
                                    (if (and code (seq defs))
                                      (reduce #(assoc %1 %2 {:expr code :time-ms execution-time-ms}) acc defs)
                                      acc))
                            {}
                            execution->defs)
        locals (iterate/get-locals environment)
        def-snapshots
        (->> locals
          (keep (fn [[symbol-name value]]
                  (when (contains? defined symbol-name)
                    (let [realized-value (realize-value value)
                          execution-information (get symbol->execution symbol-name)]
                      ;; Accept ALL values — freeze-safe in the persistence layer
                      ;; handles non-serializable types (fns → {:vis/ref :expr}).
                      (cond-> {:name (str symbol-name)
                               :value realized-value
                               :code (:expr execution-information)}
                        (:time-ms execution-information)
                        (assoc :time-ms (:time-ms execution-information)))))))
          vec)
        autobind-snapshots
        (->> expressions
          (mapcat (fn [expression]
                    (or (:autobind-events expression) [])))
          (keep (fn [{:keys [status symbol value kind id]}]
                  (when (and (= status :bound) symbol)
                    {:name (str symbol)
                     :value value
                     :code ";; AUTOBIND"
                     :metadata {:autobind true
                                :kind kind
                                :id id}})))
          ;; Last writer wins per symbol within this iteration.
          (reduce (fn [acc snapshot]
                    (assoc acc (:name snapshot) snapshot))
            {})
          vals
          vec)
        snapshots-by-name
        (reduce (fn [acc snapshot]
                  (assoc acc (:name snapshot) snapshot))
          {}
          (concat autobind-snapshots def-snapshots))]
    (vec (vals snapshots-by-name))))

(defn update-system-vars!
  "Rebind REASONING and ANSWER in the SCI sandbox after an iteration.
   See `sci-env/SYSTEM_VAR_NAMES` for the full SYSTEM-var registry."
  [environment {:keys [thinking final-result final-answer]}]
  (when (seq thinking)
    (sci-env/bind-and-bump! environment 'REASONING thinking))
  (when final-result
    (sci-env/bind-and-bump! environment 'ANSWER final-answer)))

(defn inject-system-var-snapshots
  "Append SYSTEM-var entries to a vars-snapshot vec for persistence.
   Names match `sci-env/SYSTEM_VAR_NAMES`."
  [vars-snapshot {:keys [iteration query thinking final-result final-answer]}]
  (cond-> vars-snapshot
    (zero? iteration) (conj {:name "QUERY"     :value query        :code ";; SYSTEM var"})
    (seq thinking)    (conj {:name "REASONING" :value thinking     :code ";; SYSTEM var"})
    final-result      (conj {:name "ANSWER"    :value final-answer :code ";; SYSTEM var"})))

;; -----------------------------------------------------------------------------
;; Iteration loop + run-query! (inlined from former query/base)
;; -----------------------------------------------------------------------------

(def ^:private FRESH_ITER_CARRY
  {:previous-expressions nil :previous-iteration -1
   :previous-next-model nil :previous-next-reasoning nil
   ;; nudges the loop wants to surface to the
   ;; model on the NEXT iteration. Populated when the prior iter hit
   ;; a gate violation, a plan-validation error, etc. Cleared after
   ;; one delivery so the same nudge isn't repeated.
   :pending-loop-nudges []})

;; Soft retry budget for plan-validation. A model that emits a 21-item
;; plan once gets one nudge; if it does it again we accept the prior
;; plan and move on (a buggy validator must not soft-lock the loop).
(def ^:private MAX_PLAN_VALIDATION_RETRIES 1)

;; Plan churn nudges fire when the cumulative `:plan-edit-distance`
;; across the current turn's iterations exceeds this threshold. The
;; intent: catch thrashing before budget exhaustion. A single nudge
;; per turn (the threshold-stickiness flag in the loop state ensures
;; we don't re-nudge every iteration once the threshold is breached).
(def ^:private CHURN_THRESHOLD 5)

;; Consecutive-error nudge fires the iteration BEFORE the strategy
;; restart kicks in, giving the model one warning to re-emit :plan
;; with a different approach. Tuned to MAX_CONSECUTIVE_ERRORS - 1.
(def ^:private CONSECUTIVE_ERROR_NUDGE_AT 2)

;; Same idea for the open-plan gate: one nudge, then accept.
(def ^:private MAX_GATE_RETRIES 1)

(def ^:private balanced-reasoning :balanced)

(defn- status->id [status]
  (when status (keyword "rlm.status" (name status))))

(defn iteration-loop
  "The core iteration loop. Runs assemble → ask LLM → execute → persist
   until the model emits `:answer`, the user cancels, or the
   consecutive-error budget is exhausted. There is NO iteration cap.
   If a buggy model never finalizes, the user cancels."
  [environment query
   {:keys [system-prompt
           query-id history-messages
           max-consecutive-errors max-restarts
           hooks cancel-atom current-iteration-atom
           reasoning-default routing]}]
  (let [;; Tightened from 5 to 3. Three consecutive failures is enough
        ;; signal that the current approach is wrong; the nudge fires
        ;; at CONSECUTIVE_ERROR_NUDGE_AT (= 2) so the model gets a
        ;; warning before the strategy-restart kicks in.
        max-consecutive-errors (or max-consecutive-errors 3)
        max-restarts (or max-restarts 3)
        effective-model (:name (resolve-effective-model (:router environment)))
        _ (assert effective-model "Router must resolve a root model")
        has-reasoning? (provider-has-reasoning? (:router environment))
        base-reasoning-level (or (iterate/normalize-reasoning-level reasoning-default) balanced-reasoning)
        ;; Activate extensions ONCE per query. Threaded through both the
        ;; system-prompt assembler (cacheable prefix) and the per-iteration
        ;; nudge collector — activation-fn never re-fires inside the loop.
        active-exts   (loop-core/active-extensions environment)
        system-prompt (loop-core/assemble-system-prompt environment
                        {:system-prompt      system-prompt
                         :active-extensions  active-exts})
        initial-user-content query
        initial-messages (iterate/assemble-initial-messages
                           {:system-prompt system-prompt
                            :initial-user-content initial-user-content
                            :history-messages history-messages})
        db-info (:db-info environment)
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
        call-counts-atom (atom {})
        ;; Phase 2-m measurement: per-query set of canonical hashes
        ;; for SUCCESSFUL expressions. The iteration handler counts how
        ;; many of THIS iter's blocks were already in the set
        ;; (`:expression-redundancy-fraction` + `:dedup-saves` metadata)
        ;; before adding the new successful ones.
        seen-expression-hashes-atom (atom #{})
        ;; Per-query map of canonical hash -> cached execution result.
        ;; When the model re-issues an identical canonical form,
        ;; `execute-code` returns the cached result with `:cached?
        ;; true` and `:cached-from "iN.K"` instead of re-running SCI.
        ;; Saves SCI eval cost + downstream side effects AND tells the
        ;; model the call was already made so it stops re-issuing.
        dedup-cache-atom (atom {})
        finalize-cost (fn []
                        (let [{:keys [input-tokens output-tokens reasoning-tokens cached-tokens]} @usage-atom
                              total-tokens (+ input-tokens output-tokens)
                              cost (router/estimate-cost effective-model input-tokens output-tokens)]
                          {:tokens {:input input-tokens :output output-tokens
                                    :reasoning reasoning-tokens :cached cached-tokens
                                    :total total-tokens}
                           :cost cost}))
        var-index-atom (or (:var-index-atom environment)
                         (atom {:index nil :revision -1 :current-revision 0}))
        on-iteration (:on-iteration hooks)
        on-chunk (:on-chunk hooks)
        on-cancel (:on-cancel hooks)
        emit-hook! (fn [hook-fn payload log-msg]
                     (when hook-fn
                       (try (hook-fn payload)
                         (catch Exception e
                           (tel/log! {:level :warn :data (format-exception-short e)} log-msg)))))
        ;; Metadata persisted on each iteration row — reuses the
        ;; precomputed `active-exts` (no second activation pass).
        iteration-metadata (fn []
                             (when (seq active-exts)
                               {:extensions (mapv (fn [ext]
                                                    (cond-> {:namespace (str (:ext/namespace ext))}
                                                      (:ext/version ext) (assoc :version (:ext/version ext))))
                                              active-exts)}))]
    (sci-env/bind-and-bump! environment 'QUERY query)
    (when-let [autobind-events-atom (:autobind-events-atom environment)]
      (reset! autobind-events-atom []))
    (when-let [a (:current-iteration-id-atom environment)] (reset! a nil))
    (loop-core/auto-forget-stale-vars! environment)
    (binding [*rlm-context* (merge *rlm-context* {:rlm-phase :iteration-loop})]
      (loop [loop-state (merge {:iteration 0 :messages initial-messages
                                :trace [] :consecutive-errors 0 :restarts 0}
                          FRESH_ITER_CARRY)]
        (let [{:keys [iteration messages trace consecutive-errors restarts
                      previous-expressions previous-iteration
                      previous-next-model previous-next-reasoning
                      pending-loop-nudges
                      gate-retries plan-validation-retries]} loop-state
              gate-retries (or gate-retries 0)
              plan-validation-retries (or plan-validation-retries 0)]
          (when current-iteration-atom (reset! current-iteration-atom iteration))
          (cond
            (when cancel-atom @cancel-atom)
            (do (iterate/log-stage! :error iteration {:reason :cancelled})
              (emit-hook! on-cancel {:iteration iteration :status :cancelled
                                     :status-id (status->id :cancelled)} "on-cancel hook threw")
              (merge {:answer nil :status :cancelled :status-id (status->id :cancelled)
                      :trace trace :iterations iteration} (finalize-cost)))

            :else
            (if (>= consecutive-errors max-consecutive-errors)
              (if (< restarts max-restarts)
                (let [failed (->> trace (filter :error) (take 3)
                               (map #(str "- " (get-in % [:error :message] (str (:error %)))))
                               (str/join "\n"))
                      hint (str "Previous attempts failed with these errors:\n" failed
                             "\n\nStart fresh with a DIFFERENT strategy.\n\nOriginal request: " query)
                      msgs [{:role "system" :content system-prompt} {:role "user" :content hint}]]
                  (recur (assoc loop-state
                           :iteration (inc iteration) :messages msgs
                           :trace trace :consecutive-errors 0 :restarts (inc restarts))))
                (let [errors-block (recent-errors-block trace 3)
                      fallback     (str "Warning: Too many errors (" consecutive-errors ") across "
                                     (inc restarts) " restart(s).\n\n"
                                     ;; Includes the raw provider payload
                                     ;; (e.g. an HTTP plain-text auth rejection) so
                                     ;; the user can act on it instead of guessing.
                                     errors-block)]
                  (merge {:answer fallback
                          :status :error :status-id (status->id :error)
                          :trace trace :iterations iteration} (finalize-cost))))

              (let [reasoning-level (when has-reasoning?
                                      (or previous-next-reasoning
                                        (iterate/reasoning-level-for-errors base-reasoning-level consecutive-errors)))
                    _ (iterate/log-stage! :iteration-start iteration {:message-count (count messages) :reasoning reasoning-level})
                    ;; �� sticky plan + breadcrumb chain + last
                    ;; iteration's :thinking. All come from DB so the
                    ;; projection is always rebuilt from persisted
                    ;; state, never accumulated in messages.
                    sticky-plan      (iterate/load-effective-plan db-info query-id)
                    breadcrumb-chain (iterate/load-breadcrumb-chain db-info query-id)
                    recent-thought   (when (pos? iteration)
                                       (some-> (last (db/db-list-query-iterations db-info query-id))
                                         :thinking))
                    system-vars      {:QUERY     query
                                      :ANSWER    nil
                                      :REASONING recent-thought}
                    prior-turn       (when (zero? iteration)
                                       (iterate/load-prior-turn-digest
                                         db-info (:conversation-id environment) query-id))
                    expressions-by-iteration (when (seq previous-expressions)
                                               [[(or previous-iteration 0) previous-expressions]])
                    ;; Attempts ledger: deduped log of every executed
                    ;; call across this turn so far.
                    ;; build-attempts-from-iterations walks the
                    ;; persisted iteration rows + their
                    ;; expression_state rows; format-attempts-block
                    ;; (in build-iteration-context) dedups by
                    ;; canonical hash and caps at ATTEMPTS_KEEP_LAST.
                    attempts-ledger (try
                                      (iterate/build-attempts-from-iterations
                                        db-info
                                        (db/db-list-query-iterations db-info query-id))
                                      (catch Throwable _ []))
                    iteration-context (iterate/build-iteration-context environment
                                        {:iteration              iteration
                                         :plan-state             sticky-plan
                                         :breadcrumbs            breadcrumb-chain
                                         :attempts               attempts-ledger
                                         :recent-thought         recent-thought
                                         :system-vars            system-vars
                                         :prior-turn             prior-turn
                                         :expressions-by-iteration expressions-by-iteration
                                         :call-counts-atom       call-counts-atom
                                         ;; surface
                                         ;; gate-violation / plan-validation
                                         ;; nudges from the prior iter exactly
                                         ;; once.
                                         :loop-nudges            (or pending-loop-nudges [])
                                         :active-extensions      active-exts})
                    base-messages (iterate/trim-to-initial-history messages (count initial-messages))
                    effective-messages (cond-> base-messages
                                         (not (str/blank? iteration-context))
                                         (conj {:role "user" :content iteration-context}))
                    effective-overrides (cond-> {}
                                          previous-next-model (assoc :optimize previous-next-model)
                                          previous-next-reasoning (assoc :reasoning previous-next-reasoning))
                    resolved-model (resolve-effective-model (:router environment) effective-overrides)
                    effective-routing (merge (or routing {}) effective-overrides)
                    iteration-result (try
                                       (iterate/run-iteration environment effective-messages
                                         {:iteration-spec (if has-reasoning? ITERATION_SPEC_REASONING ITERATION_SPEC_NON_REASONING)
                                          :iteration iteration :reasoning-level reasoning-level
                                          :routing effective-routing
                                          :resolved-model resolved-model
                                          :on-chunk on-chunk
                                          :dedup-cache-atom dedup-cache-atom})
                                       (catch Exception e
                                         (iterate/handle-iteration-exception! e
                                           {:iteration iteration :messages effective-messages
                                            :routing effective-routing :reasoning-level reasoning-level})))]
                (if-let [iteration-error-data (::iterate/iteration-error iteration-result)]
                  ;; Cancellation short-circuit. When the user pressed Esc
                  ;; mid-call, `cancel!` flipped the flag BEFORE
                  ;; future-cancel, so by the time we land here the flag is
                  ;; already true. Treat the resulting interrupt-shaped
                  ;; \"iteration-error-data\" as cancellation, not a real failure: skip
                  ;; the trace entry, skip the DB write, skip the on-chunk
                  ;; error chunk (otherwise the bubble paints a phantom
                  ;; ITERATION N ERROR block right next to FINAL ANSWER:
                  ;; \"_Cancelled by user._\"). Bail straight to the cancel
                  ;; result that the top-of-loop branch would have produced.
                  (if (and cancel-atom @cancel-atom)
                    (do (iterate/log-stage! :error iteration {:reason :cancelled})
                      (emit-hook! on-cancel {:iteration iteration :status :cancelled
                                             :status-id (status->id :cancelled)}
                        "on-cancel hook threw")
                      (merge {:answer nil :status :cancelled
                              :status-id (status->id :cancelled)
                              :trace trace :iterations iteration}
                        (finalize-cost)))
                    (let [error-feedback (str "[Iteration " (inc iteration) "]\n"
                                           "<error>LLM call failed: " (:message iteration-error-data) "</error>\n"
                                           "Adjust your approach or emit :final with what you have.")
                          trace-entry {:iteration iteration :error iteration-error-data :final? false}
                          empty-reasoning (when (= :svar.llm/empty-content (:type iteration-error-data))
                                            (:reasoning (:data iteration-error-data)))
                          err-iteration-id (db/store-iteration! (:db-info environment)
                                             {:query-id query-id :vars [] :expressions nil
                                              :thinking empty-reasoning :duration-ms 0 :error iteration-error-data
                                              :llm-messages effective-messages
                                              :llm-model (str (:name resolved-model))
                                              :metadata (iteration-metadata)})]
                      (when-let [a (:current-iteration-id-atom environment)] (reset! a err-iteration-id))
                      ;; Live error chunk — lets the TUI / web bubble show
                      ;; \"iteration N failed: <msg>\" the moment it happens, instead
                      ;; of waiting for the whole loop to give up. The chunk
                      ;; carries the same shape on-iteration sees so any UI
                      ;; that already reads :error gets it for free.
                      (emit-hook! on-chunk
                        {:iteration iteration :thinking empty-reasoning :code nil
                         :error iteration-error-data :done? true}
                        "on-chunk (iteration error)")
                      (emit-hook! on-iteration
                        {:iteration iteration :status :error :status-id (status->id :error)
                         :thinking empty-reasoning :expressions nil :final-result nil
                         :error iteration-error-data :duration-ms 0} "on-iteration (error)")
                      (recur (assoc loop-state
                               :iteration (inc iteration)
                               :messages (conj messages {:role "user" :content error-feedback})
                               :trace (conj trace trace-entry)
                               :consecutive-errors (inc consecutive-errors) :restarts restarts))))

                  (let [_ (accumulate-usage! (:api-usage iteration-result))
                        {:keys [thinking expressions final-result next-model next-reasoning]} iteration-result
                        ;; �� plan/breadcrumb fields from svar parse
                        new-plan-state    (:plan-state iteration-result)
                        breadcrumb-text   (:breadcrumb iteration-result)
                        ;; �� plan-edit-distance + change marker.
                        ;; Compute against the sticky plan (loaded above)
                        ;; so we record the diff for THIS iteration.
                        plan-validation   (when new-plan-state
                                            (iterate/validate-plan-state new-plan-state))
                        effective-plan    (cond
                                            plan-validation sticky-plan ;; reject — keep prior
                                            new-plan-state  new-plan-state
                                            :else           sticky-plan)
                        plan-diff         (when new-plan-state
                                            (iterate/compute-plan-diff sticky-plan new-plan-state))
                        plan-edit-dist    (iterate/plan-edit-distance plan-diff)
                        final-answer (when final-result (:answer final-result))
                        _ (update-system-vars! environment
                            {:thinking thinking :final-result final-result :final-answer final-answer})
                        vars-snapshot (restorable-var-snapshots environment expressions)
                        vars-snapshot (inject-system-var-snapshots vars-snapshot
                                        {:iteration iteration :query query :thinking thinking
                                         :final-result final-result :final-answer final-answer})
                        ;; Phase 2-m: count duplicates against the
                        ;; per-query seen-hashes set; record both the
                        ;; raw count (would-be savings if Phase 2's
                        ;; auto-dedup were live) and the per-iter
                        ;; fraction. The atom is mutated so the next
                        ;; iteration's count includes hashes added
                        ;; here.
                        [redundant-count expression-count]
                        (iterate/count-duplicates seen-expression-hashes-atom
                          (or expressions []))
                        redundancy-fraction
                        (if (pos? expression-count)
                          (double (/ redundant-count expression-count))
                          0.0)
                        ;; Augment iteration metadata with the iteration-level counters.
                        iteration-metadata-with-metrics
                        (merge (or (iteration-metadata) {})
                          {:plan-edit-distance plan-edit-dist
                           :plan-changed?      (boolean (and plan-diff (pos? plan-edit-dist)))
                           :var-history-recall-count
                           (count (filter #(and (string? (:code %))
                                             (re-find #"\(var-history\b" (:code %)))
                                    (or expressions [])))
                           :expression-redundancy-fraction redundancy-fraction
                           :dedup-saves                    redundant-count
                           :plan-validation-error          (some-> plan-validation :type name)
                           :confidence-gate-rejected?
                           (boolean (and final-result
                                      (= :low (:confidence final-result))
                                      (str/blank? (str (:abandon-reason final-result)))
                                      (< gate-retries MAX_GATE_RETRIES)))})
                        ;; The PEV gate actively rejects :answer when
                        ;; the plan still has open items and no
                        ;; :abandon-reason. Up to MAX_GATE_RETRIES; after
                        ;; that the answer goes through (a buggy gate
                        ;; must not soft-lock the loop).
                        gate-violation
                        (when (and final-result
                                effective-plan
                                (some #(contains? #{:in-progress :pending}
                                         (iterate/item-status-key %))
                                  (:items effective-plan))
                                (str/blank? (str (:abandon-reason final-result)))
                                (< gate-retries MAX_GATE_RETRIES))
                          {:type :vis/incomplete-plan-on-answer
                           :data {:open-item-ids
                                  (mapv :id
                                    (filter #(contains? #{:in-progress :pending}
                                               (iterate/item-status-key %))
                                      (:items effective-plan)))}})
                        ;; Confidence gate: :answer with :confidence
                        ;; :low must carry :abandon-reason. Reuses the
                        ;; same retry budget as the open-plan gate.
                        confidence-gate-violation
                        (when (and final-result
                                (= :low (:confidence final-result))
                                (str/blank? (str (:abandon-reason final-result)))
                                (< gate-retries MAX_GATE_RETRIES))
                          {:type :vis/low-confidence-on-answer
                           :data {:confidence (:confidence final-result)}})
                        ;; Plan-validation surfaces the validator's
                        ;; structured error on the next iteration, up
                        ;; to MAX_PLAN_VALIDATION_RETRIES.
                        plan-validation-violation
                        (when (and plan-validation
                                (< plan-validation-retries MAX_PLAN_VALIDATION_RETRIES))
                          plan-validation)
                        ;; Plan churn: cumulative `:plan-edit-distance`
                        ;; across this turn's iterations. Sum from
                        ;; prior iter rows + this iter. Latches a
                        ;; one-shot nudge once the cumulative crosses
                        ;; CHURN_THRESHOLD.
                        prior-edit-distance-sum
                        (reduce + 0 (keep #(get-in % [:metadata :plan-edit-distance])
                                      (db/db-list-query-iterations db-info query-id)))
                        cumulative-edit-distance
                        (+ prior-edit-distance-sum (or plan-edit-dist 0))
                        churn-violation
                        (when (and (> cumulative-edit-distance CHURN_THRESHOLD)
                                (not (:churn-nudged? loop-state)))
                          {:type :vis/plan-churn
                           :data {:edit-distance cumulative-edit-distance
                                  :iteration-count (inc iteration)}})
                        ;; Pre-restart consecutive-error nudge: fires
                        ;; ONCE when the count reaches
                        ;; CONSECUTIVE_ERROR_NUDGE_AT (= 2). Past that
                        ;; the strategy-restart logic kicks in, so a
                        ;; queued nudge would never be seen anyway.
                        consecutive-error-violation
                        (when (and (= consecutive-errors CONSECUTIVE_ERROR_NUDGE_AT)
                                (not (:consecutive-error-nudged? loop-state)))
                          {:type :vis/consecutive-errors
                           :data {:count consecutive-errors}})
                        ;; Aggregate loop-nudges to surface on the next
                        ;; iteration. `format-loop-nudge` gates nil input
                        ;; and returns the [system_nudge]-prefixed line.
                        next-pending-loop-nudges
                        (vec (keep iterate/format-loop-nudge
                               [gate-violation
                                confidence-gate-violation
                                plan-validation-violation
                                churn-violation
                                consecutive-error-violation]))
                        ;; Latching flags: once a churn / consecutive-
                        ;; error nudge has fired, don't re-fire it on
                        ;; every subsequent iteration. Stays sticky
                        ;; through all recur paths via merge into
                        ;; loop-state.
                        next-churn-nudged?
                        (or (:churn-nudged? loop-state) (boolean churn-violation))
                        next-consecutive-error-nudged?
                        (or (:consecutive-error-nudged? loop-state)
                          (boolean consecutive-error-violation))
                        iteration-id (db/store-iteration! (:db-info environment)
                                       {:query-id query-id :expressions expressions :vars vars-snapshot
                                        :thinking thinking
                                        :answer (when final-result (iterate/answer-str (:answer final-result)))
                                        :duration-ms (or (:duration-ms iteration-result) 0)
                                        :llm-messages (:llm-messages iteration-result)
                                        :llm-model (:llm-model iteration-result)
                                   ;; Plan slot persistence.
                                        :plan-state (when (and new-plan-state (nil? plan-validation))
                                                      new-plan-state)
                                        :breadcrumb breadcrumb-text
                                        :plan-diff  plan-diff
                                        :metadata iteration-metadata-with-metrics})
                        _ (when-let [a (:current-iteration-id-atom environment)] (reset! a iteration-id))
                        _ (emit-hook! on-iteration
                            {:iteration iteration
                             :status (cond final-result :final (empty? expressions) :empty :else :success)
                             :status-id (status->id (cond final-result :final (empty? expressions) :empty :else :success))
                             :thinking thinking :expressions expressions :final-result final-result
                             :error nil :duration-ms (or (:duration-ms iteration-result) 0)}
                            "on-iteration (success)")
                        trace-entry {:iteration iteration :thinking thinking
                                     :expressions expressions :final? (boolean final-result)}]
                    (cond
                      ;; §15.1 PEV gate active: a final-result with open
                      ;; plan items and no :abandon-reason is REJECTED.
                      ;; Drop final-result, queue nudge for next iter,
                      ;; bump gate-retries, recur.
                      (or gate-violation confidence-gate-violation)
                      (do (iterate/log-stage! :gate-rejected iteration
                            {:open-items (-> gate-violation :data :open-item-ids)
                             :low-confidence? (boolean confidence-gate-violation)
                             :retry      (inc gate-retries)})
                        (recur (merge loop-state
                                 {:iteration (inc iteration)
                                  :trace (conj trace (assoc trace-entry :gate-rejected? true))
                                  :previous-next-model next-model
                                  :previous-next-reasoning next-reasoning
                                  :pending-loop-nudges next-pending-loop-nudges
                                  :gate-retries (inc gate-retries)
                                  :churn-nudged? next-churn-nudged?
                                  :consecutive-error-nudged? next-consecutive-error-nudged?})))

                      final-result
                      (do (iterate/log-stage! :final iteration
                            {:answer (truncate (iterate/answer-str (:answer final-result)) 200)
                             :confidence (:confidence final-result) :iterations (inc iteration)})
                        (iterate/log-stage! :iteration-end iteration
                          {:blocks (count expressions) :errors (count (filter :error expressions))
                           :times (mapv :execution-time-ms expressions)})
                        (when on-chunk
                          (on-chunk {:iteration iteration :thinking thinking
                                     :code (mapv :code expressions)
                                     :results (mapv #(if (:error %) (vis-error/format-error (:error %)) (iterate/safe-pr-str (:result %))) expressions)
                                     :stdouts (mapv #(or (:stdout %) "") expressions)
                                     :durations (mapv #(or (:execution-time-ms %) 0) expressions)
                                     :successes (mapv #(nil? (:error %)) expressions)
                                     :final {:answer (:answer final-result) :confidence (:confidence final-result)
                                             :iterations (inc iteration) :status :success}
                                     :done? true}))
                        (merge (cond-> {:answer (:answer final-result) :trace (conj trace trace-entry)
                                        :iterations (inc iteration) :confidence (:confidence final-result)}
                                 (:reasoning final-result) (assoc :reasoning (:reasoning final-result)))
                          (finalize-cost)))

                      :else
                      (if (empty? expressions)
                        (do (iterate/log-stage! :empty iteration {})
                          (iterate/log-stage! :iteration-end iteration {:blocks 0 :errors 0 :times []})
                          (recur (merge loop-state
                                   {:iteration (inc iteration) :trace (conj trace trace-entry)
                                    :previous-next-model next-model :previous-next-reasoning next-reasoning
                                    :pending-loop-nudges next-pending-loop-nudges
                                    :plan-validation-retries (if plan-validation
                                                               (inc plan-validation-retries)
                                                               plan-validation-retries)
                                    :churn-nudged? next-churn-nudged?
                                    :consecutive-error-nudged? next-consecutive-error-nudged?})))

                        (do (iterate/log-stage! :iteration-end iteration
                              {:blocks (count expressions) :errors (count (filter :error expressions))
                               :times (mapv :execution-time-ms expressions)})
                          (when on-chunk
                            (on-chunk {:iteration iteration :thinking thinking
                                       :code (mapv :code expressions)
                                       :results (mapv #(if (:error %) (vis-error/format-error (:error %)) (iterate/safe-pr-str (:result %))) expressions)
                                       :stdouts (mapv #(or (:stdout %) "") expressions)
                                       :durations (mapv #(or (:execution-time-ms %) 0) expressions)
                                       :successes (mapv #(nil? (:error %)) expressions)
                                       :done? false}))
                          (let [had-success? (some #(nil? (:error %)) expressions)
                                next-errors (if had-success? 0 (inc consecutive-errors))
                                _ (when had-success? (swap! var-index-atom update :current-revision inc))]
                            (recur (merge loop-state
                                     {:iteration (inc iteration) :messages messages
                                      :pending-loop-nudges next-pending-loop-nudges
                                      :plan-validation-retries (if plan-validation
                                                                 (inc plan-validation-retries)
                                                                 plan-validation-retries)
                                      :trace (conj trace trace-entry) :consecutive-errors next-errors
                                      :previous-expressions expressions :previous-iteration iteration
                                      :previous-next-model next-model
                                      :previous-next-reasoning next-reasoning
                                      :churn-nudged? next-churn-nudged?
                                      :consecutive-error-nudged? next-consecutive-error-nudged?}))))))))))))))))

(defn- ->prior-outcome
  "Map a query-loop result to one of the four canonical outcomes
   stored on `query_state.prior_outcome`. Used by the next turn's
   handover digest (see `iterate/load-prior-turn-digest`)."
  [result]
  (let [status   (:status result)
        abandon? (some-> result :abandon-reason str str/blank? not)]
    (cond
      abandon?                                          :abandoned
      (= status :cancelled)                             :cancelled
      (= status :error)                                  :error
      :else                                              :complete)))

(defn run-query!
  "Store query → iteration-loop → update query → return result.

   Derives `:prior-outcome` (one of `:complete`,
   `:abandoned`, `:cancelled`, `:error`) from the loop result and
   persists it on the `query_state` row. The next turn's
   `<system_state>` digest reads it."
  [env query loop-opts]
  (when-not (map? env)
    (throw (ex-info "run-query! requires an env map" {:got (type env)})))
  (when (clojure.string/blank? query)
    (throw (ex-info "run-query! requires a non-blank query string" {:got query})))
  (let [query-id (db/store-query! (:db-info env)
                   {:parent-conversation-id (:conversation-id env)
                    :query query
                    :messages nil
                    :status :running})
        result (iteration-loop env query (assoc loop-opts :query-id query-id))
        prior-outcome (->prior-outcome result)
        _ (db/update-query! (:db-info env) query-id
            {:answer        (:answer result)
             :iterations    (:iterations result)
             :duration-ms   (:duration-ms result)
             :status        (or (:status result) :success)
             :tokens        (:tokens result)
             :cost          (:cost result)
             :prior-outcome prior-outcome})]
    (assoc result :query-id query-id :prior-outcome prior-outcome)))

;; -----------------------------------------------------------------------------
;; Prepare query context
;; -----------------------------------------------------------------------------

(defn- prepare-query-context
  "Validates inputs, resolves SCI bindings, sets up atoms.
   Returns a map of all computed context needed for subsequent phases."
  [env messages opts]
  (let [{:keys [spec model
                max-context-tokens
                system-prompt debug? hooks cancel-atom eval-timeout-ms
                reasoning-default routing]
         :or   {debug? false}} opts]
    (when-not (:db-info env)
      (anomaly/incorrect! "Invalid RLM environment" {:type :vis/invalid-env}))
    (when-not (and (vector? messages) (seq messages))
      (anomaly/incorrect! "messages must be a non-empty vector of message maps, e.g. [(llm/user \"...\")]"
        {:type :vis/invalid-messages :got (type messages)}))
    (when (and (some? eval-timeout-ms) (not (integer? eval-timeout-ms)))
      (anomaly/incorrect! ":eval-timeout-ms must be an integer (milliseconds)"
        {:type     :vis/invalid-eval-timeout
         :got      eval-timeout-ms
         :got-type (type eval-timeout-ms)}))
    (let [cancel-atom            (or cancel-atom (atom false))
          ;; `query-str` = ONLY the current turn's user message.
          ;;
          ;; Prior behavior joined every message's :content (including
          ;; previous turns' user msgs + assistant answers + system!) into
          ;; one growing blob. That corrupted three things at once:
          ;;   1. `query_attrs.text` / `.name` stored the entire transcript
          ;;      for every turn — the sidebar showed "Siema\nSiema!\n…".
          ;;   2. `*query*` SYSTEM var (bound from this same string) grew
          ;;      with each turn instead of reflecting the current ask.
          ;;   3. The synthetic `{:requirement …}` frame the LLM sees
          ;;      restated the whole conversation as the "requirement".
          ;;
          ;; Conversation history still reaches the model via the `messages`
          ;; vector itself (passed through to the LLM call unmodified).
          ;; `query-str` is ONLY the current turn — one ask, one value.
          extract-text           (fn [c]
                                   (cond
                                     (string? c)     c
                                     (sequential? c) (str/join " "
                                                       (keep #(when (= "text" (:type %)) (:text %)) c))
                                     :else           nil))
          ;; Locate the LAST user message once. It's both the source of
          ;; `query-str` (the `{:requirement ...}` payload for iteration 0)
          ;; AND the boundary between history and the current turn:
          ;; everything BEFORE its index is prior conversation history,
          ;; which `iteration-loop` replays as `:history-messages`.
          last-user-idx          (->> (map-indexed vector messages)
                                   reverse
                                   (some (fn [[i m]]
                                           (when (contains? #{"user" :user} (:role m))
                                             i))))
          last-user-msg          (when last-user-idx (nth messages last-user-idx))
          query-str              (or (some-> last-user-msg :content extract-text)
                                   ;; Fallback: no :user role found (malformed caller) —
                                   ;; use the last message's text. Better than an empty query.
                                   (some-> messages last :content extract-text)
                                   "")
          history-messages       (if last-user-idx
                                   (vec (take last-user-idx messages))
                                   (vec messages))
          env-router             (:router env)
          root-model             (or (when env-router (:name (resolve-effective-model env-router))) model)
          db-info                (:db-info env)
          custom-bindings        (loop-core/custom-bindings env)
          current-iteration-atom (atom 0)
          sci-ctx                (:sci-ctx env)
          _                      (sci-env/bump-var-index! env)
          _                      (doseq [[sym val] (or custom-bindings {})]
                                   (when val
                                     (sci-env/sci-update-binding! sci-ctx sym val)))
          _                      (sci-env/bump-var-index! env)
          current-iteration-id-atom (atom nil)
          environment            (assoc env
                                   :current-iteration-atom current-iteration-atom
                                   :current-iteration-id-atom current-iteration-id-atom)
          environment-id         (:environment-id env)]
      {:cancel-atom            cancel-atom
       :query-str              query-str
       :router                 env-router
       :root-model             root-model
       :db-info                db-info
       :current-iteration-atom current-iteration-atom
       :environment            environment
       :environment-id         environment-id
       :spec                   spec
       :max-context-tokens     max-context-tokens
       :system-prompt          system-prompt
       :debug?                 debug?
       :hooks                  hooks
       :eval-timeout-ms        eval-timeout-ms
       :reasoning-default      reasoning-default
       :routing                routing
       :messages               messages
       :history-messages       history-messages})))

;; -----------------------------------------------------------------------------
;; Run iteration phase
;; -----------------------------------------------------------------------------

(defn- run-iteration-phase
  "Runs the main iteration loop via run-query!.
   Returns iteration-result, query-id, cost atoms, and merge-cost! fn."
  [{:keys [environment query-str history-messages spec
           max-context-tokens system-prompt
           current-iteration-atom hooks cancel-atom
           reasoning-default routing]}]
  (let [iteration-result (run-query! environment query-str
                           (cond-> {:output-spec            spec
                                    :max-context-tokens     max-context-tokens
                                    :system-prompt          system-prompt
                                    :reasoning-default      reasoning-default
                                    :history-messages       history-messages
                                    :current-iteration-atom current-iteration-atom
                                    :hooks                  hooks
                                    :cancel-atom            cancel-atom}
                             routing (assoc :routing routing)))
        query-id         (:query-id iteration-result)
        {iteration-tokens :tokens
         iteration-cost   :cost} iteration-result
        total-tokens-atom (atom (or iteration-tokens {}))
        total-cost-atom   (atom (or iteration-cost {}))
        merge-cost!       (fn [extra-tokens extra-cost]
                            (when extra-tokens
                              (swap! total-tokens-atom
                                (fn [acc]
                                  (merge-with + acc
                                    (select-keys extra-tokens [:input :output :reasoning :cached :total])))))
                            (when extra-cost
                              (swap! total-cost-atom
                                (fn [acc]
                                  (merge-with + (select-keys acc [:input-cost :output-cost :total-cost])
                                    (select-keys extra-cost [:input-cost :output-cost :total-cost]))))))]
    {:iteration-result  iteration-result
     :query-id         query-id
     :total-tokens-atom total-tokens-atom
     :total-cost-atom   total-cost-atom
     :merge-cost!       merge-cost!}))

;; -----------------------------------------------------------------------------
;; Finalize query result
;; -----------------------------------------------------------------------------

(defn- finalize-query-result
  "Updates DB query record, builds result map.

   `:model` is attached to the persisted cost map so the web footer can
   render `model · N iteration · duration · tokens · $total` after a restart."
  [{:keys [db-info root-model]}
   {:keys [query-id start-time iterations status status-id trace locals
           answer confidence reasoning total-tokens-atom total-cost-atom]}]
  (let [duration-ms (util/elapsed-since start-time)
        cost-with-model (cond-> @total-cost-atom
                          (and root-model (not (:model @total-cost-atom)))
                          (assoc :model (str root-model)))]
    (if status
      ;; failure path — surface the fallback answer (built by the loop for
      ;; :error) to the caller. Leaving
      ;; :answer nil here meant the web bubble rendered blank even though
      ;; we had diagnostic text ready.
      (do
        (iterate/log-stage! :query-end 0
          {:duration-ms duration-ms :iterations iterations :status status})
        (let [fallback-answer (:result answer answer)]
          (try
            (db/update-query! db-info query-id
              {:answer      fallback-answer
               :iterations  iterations
               :duration-ms duration-ms
               :status      status
               :tokens      @total-tokens-atom
               :cost        cost-with-model})
            (catch Exception e
              (tel/log! {:level :warn :data (format-exception-short e)
                         :msg   "Failed to update query (max iterations)"})))
          (cond-> {:answer      fallback-answer
                   :status      status
                   :status-id   status-id
                   :trace       trace
                   :iterations  iterations
                   :duration-ms duration-ms
                   :tokens      @total-tokens-atom
                   :cost        cost-with-model}
            (some? locals) (assoc :locals locals))))
      ;; success path
      (do
        (iterate/log-stage! :query-end 0
          {:duration-ms duration-ms :iterations iterations
           :cost (str (:total-cost cost-with-model))})
        (try
          (db/update-query! db-info query-id
            {:answer      answer
             :iterations  iterations
             :duration-ms duration-ms
             :status      :success
             :tokens      @total-tokens-atom
             :cost        cost-with-model})
          (catch Exception e
            (tel/log! {:level :warn :data (format-exception-short e)
                       :msg   "Failed to update query (success)"})))
        (cond-> {:answer      answer
                 :trace       trace
                 :iterations  iterations
                 :duration-ms duration-ms
                 :tokens      @total-tokens-atom
                 :cost        cost-with-model}
          (some? confidence) (assoc :confidence confidence)
          (some? reasoning)  (assoc :reasoning reasoning))))))

;; -----------------------------------------------------------------------------
;; Public entry point
;; -----------------------------------------------------------------------------

(defn query!
  "Runs a query on an RLM environment using iterative LLM code evaluation.

    Params:
    `environment` - RLM environment from create-environment.
    `messages` - Vector of message maps. Always a vector, e.g.:
                 [(llm/user <prompt-text>)]
                 [(llm/user <prompt-text> (llm/image <b64> <mime-type>))]
   `opts` - Map, optional:
     - :spec - Output spec for structured answers.
     - :model - Override config's default model.
      - :max-context-tokens - Token budget for context.
      - :debug? - Enable verbose debug logging (default: false). Logs iteration details,
        code evaluation, LLM responses at :info level with :rlm-phase context.
      - :reasoning-default - Optional base reasoning effort for reasoning-capable models.
        Accepts :low/:medium/:high or low/medium/high strings. Adaptive escalation still applies.

    Returns:
   Map with:
      - :trace - Vector of iteration trace entries, each containing:
          {:iteration N
           :response <llm-response-text>
           :expressions [{:id 0 :code <code-str> :result <value> :stdout <str> :error nil :execution-time-ms 5}
                       ...]}
     - :iterations - Number of iterations used.
     - :duration-ms - Query duration in milliseconds.
     - :tokens - Token usage map {:input N :output N :total N}.
     - :cost - Cost map {:input-cost N :output-cost N :total-cost N}.
     - :confidence - Confidence level (:high/:medium/:low) from final iteration.
      - :reasoning - String summary of how the answer was derived (from LLM's FINAL call).
      - :status - Only present on failure (`:error` or `:cancelled`)."
  ([environment messages]
   (query! environment messages {}))
  ([environment messages opts]
   (let [ctx (prepare-query-context environment messages opts)
         {:keys [eval-timeout-ms concurrency
                 debug? query-str root-model
                 db-info
                 environment-id]} ctx
         merged-concurrency (merge query-runtime/DEFAULT_CONCURRENCY concurrency)]
     (binding [query-runtime/*rlm-context*       {:rlm-environment-id environment-id :rlm-type :main
                                                  :rlm-debug? debug? :rlm-phase :query
                                                  :db-info db-info
                                                  :conversation-soul-id (:conversation-id environment)}
               query-runtime/*eval-timeout-ms*  (query-runtime/clamp-eval-timeout-ms
                                                  (or eval-timeout-ms query-runtime/*eval-timeout-ms*))
               query-runtime/*concurrency*      merged-concurrency]
       (tel/with-ctx+ {:db-info db-info
                       :conversation-soul-id (:conversation-id environment)}
         (iterate/log-stage! :query-start 0
           {:model root-model
            :reasoning? (boolean (:reasoning? (first (mapcat :models (:providers (:router environment))))))
            :query query-str})
         (let [start-time   (System/nanoTime)
               phase2       (run-iteration-phase ctx)
               {:keys [iteration-result query-id
                       total-tokens-atom total-cost-atom]} phase2
               {iteration-answer :answer
                trace       :trace
                iterations  :iterations
                status      :status
                status-id   :status-id
                locals      :locals
                confidence  :confidence
                reasoning   :reasoning} iteration-result]
           (if status
             (finalize-query-result
               ctx
               {:query-id         query-id
                :start-time        start-time
                :iterations        iterations
                :status            status
                :status-id         status-id
                :trace             trace
                :locals            locals
                :answer            iteration-answer
                :total-tokens-atom total-tokens-atom
                :total-cost-atom   total-cost-atom})
             (finalize-query-result
               ctx
               {:query-id         query-id
                :start-time        start-time
                :iterations        iterations
                :trace             trace
                :answer            iteration-answer
                :confidence        confidence
                :reasoning         reasoning
                :total-tokens-atom total-tokens-atom
                :total-cost-atom   total-cost-atom}))))))))
