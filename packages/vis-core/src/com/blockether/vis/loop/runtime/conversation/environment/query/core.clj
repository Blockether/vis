(ns com.blockether.vis.loop.runtime.conversation.environment.query.core
  "vis! orchestration: context prep, iteration loop, finalization."
  (:require
   [clojure.string :as str]
   [com.blockether.anomaly.core :as anomaly]
   [com.blockether.svar.internal.llm :as llm]
   [com.blockether.svar.internal.router :as router]
   [com.blockether.vis.config :as config]
   [com.blockether.vis.loop.core :as loop-core]
   [com.blockether.vis.persistance.core :as db]

   [com.blockether.vis.loop.runtime.conversation.environment.query.iteration.core :as iterate]
   [com.blockether.vis.persistance.spec :as rlm-spec
    :refer [ITERATION_SPEC_NON_REASONING ITERATION_SPEC_REASONING *rlm-ctx*]]
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
  "Returns serializable snapshots of user vars introduced by this iteration."
  [environment expressions]
  (let [execution->defs (mapv (fn [{:keys [error] :as execution}]
                                [execution (when-not error
                                             (set (map symbol (extract-def-names [execution]))))])
                          expressions)
        defined (into #{} (mapcat second) execution->defs)
        sym->exec (reduce (fn [acc [{:keys [code execution-time-ms]} defs]]
                            (if (and code (seq defs))
                              (reduce #(assoc %1 %2 {:expr code :time-ms execution-time-ms}) acc defs)
                              acc))
                    {}
                    execution->defs)
        locals (iterate/get-locals environment)]
    (->> locals
      (keep (fn [[sym value]]
              (when (contains? defined sym)
                (let [realized (realize-value value)
                      exec-info (get sym->exec sym)]
                  ;; Accept ALL values — freeze-safe in the persistence layer
                  ;; handles non-serializable types (fns → {:vis/ref :expr}).
                  (cond-> {:name (str sym) :value realized :code (:expr exec-info)}
                    (:time-ms exec-info) (assoc :time-ms (:time-ms exec-info)))))))
      vec)))

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
   Names match `sci-env/SYSTEM_VAR_NAMES` (UPPERCASE, no earmuffs)."
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
   :previous-next-model nil :previous-next-reasoning nil})

(def ^:private balanced-reasoning :balanced)

(defn- status->id [status]
  (when status (keyword "rlm.status" (name status))))

(defn iteration-loop
  "The core iteration loop. Runs N iterations of: assemble → ask LLM → execute → persist."
  [environment query
   {:keys [output-spec max-context-tokens system-prompt
           query-id history-messages
           max-iterations max-consecutive-errors max-restarts
           hooks cancel-atom current-iteration-atom
           reasoning-default routing]}]
  (let [max-iterations (or max-iterations rlm-spec/MAX_ITERATIONS)
        max-consecutive-errors (or max-consecutive-errors 5)
        max-restarts (or max-restarts 3)
        max-iterations-atom-binding (:max-iterations-atom environment)
        effective-max-iterations (fn [] (if max-iterations-atom-binding @max-iterations-atom-binding max-iterations))
        effective-model (:name (resolve-effective-model (:router environment)))
        _ (assert effective-model "Router must resolve a root model")
        max-context-tokens (or max-context-tokens
                             (long (* 0.6 (router/context-limit effective-model))))
        has-reasoning? (boolean (provider-has-reasoning? (:router environment)))
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
    (when-let [a (:current-iteration-id-atom environment)] (reset! a nil))
    (loop-core/auto-forget-stale-vars! environment)
    (binding [*rlm-ctx* (merge *rlm-ctx* {:rlm-phase :iteration-loop})]
      (loop [loop-state (merge {:iteration 0 :messages initial-messages
                                :trace [] :consecutive-errors 0 :restarts 0}
                          FRESH_ITER_CARRY)]
        (let [{:keys [iteration messages trace consecutive-errors restarts
                      previous-expressions previous-iteration
                      previous-next-model previous-next-reasoning]} loop-state]
          (when current-iteration-atom (reset! current-iteration-atom iteration))
          (cond
            (when cancel-atom @cancel-atom)
            (do (iterate/log-stage! :error iteration {:reason :cancelled})
              (emit-hook! on-cancel {:iteration iteration :status :cancelled
                                     :status-id (status->id :cancelled)} "on-cancel hook threw")
              (merge {:answer nil :status :cancelled :status-id (status->id :cancelled)
                      :trace trace :iterations iteration} (finalize-cost)))

            (>= iteration (effective-max-iterations))
            (let [max-iteration (effective-max-iterations)
                  last-thinking (some->> trace reverse (map :thinking)
                                  (filter #(and (string? %) (not (str/blank? %)))) first)
                  errors-block  (recent-errors-block trace 3)
                  fallback (str "Warning: Iteration limit (" iteration "/" max-iteration ") reached.\n\n"
                             ;; Surface the actual provider failures FIRST when present —
                             ;; they're the load-bearing diagnostic. Without this the
                             ;; user only saw \"max-iteration reached, rephrase narrowly\"
                             ;; while the DB held the real cause (e.g. a Claude auth
                             ;; rejection that hit because routing slipped models).
                             errors-block
                             (when last-thinking (str "**Last reasoning:**\n\n" (truncate last-thinking 800) "\n\n"))
                             (if errors-block
                               "**What to try:** Fix the provider error above (config / model availability), then retry."
                               "**What to try:** Rephrase more narrowly."))]
              (iterate/log-stage! :error iteration {:reason :max-iterations :max max-iteration})
              (merge {:answer fallback
                      :status :max-iterations :status-id (status->id :max-iterations)
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
                                     ;; Same shared formatter as the max-iterations
                                     ;; branch — includes the raw provider payload
                                     ;; (e.g. an HTTP plain-text auth rejection) so
                                     ;; the user can act on it instead of guessing.
                                     errors-block)]
                  (merge {:answer fallback
                          :status :error-budget-exhausted :status-id (status->id :error-budget-exhausted)
                          :trace trace :iterations iteration} (finalize-cost))))

              (let [reasoning-level (when has-reasoning?
                                      (or previous-next-reasoning
                                        (iterate/reasoning-level-for-errors base-reasoning-level consecutive-errors)))
                    _ (iterate/log-stage! :iteration-start iteration {:message-count (count messages) :reasoning reasoning-level})
                    ;; Phase 1 — sticky plan + breadcrumb chain + last
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
                    iteration-context (iterate/build-iteration-context environment
                                        {:iteration              iteration
                                         :current-max-iterations (effective-max-iterations)
                                         :plan-state             sticky-plan
                                         :breadcrumbs            breadcrumb-chain
                                         :recent-thought         recent-thought
                                         :system-vars            system-vars
                                         :prior-turn             prior-turn
                                         :expressions-by-iteration    expressions-by-iteration
                                         :call-counts-atom       call-counts-atom
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
                                          :on-chunk on-chunk})
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
                    (let [error-feedback (str "[Iteration " (inc iteration) "/" (effective-max-iterations) "]\n"
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
                        ;; Phase 1 — plan/breadcrumb fields from svar parse
                        new-plan-state    (:plan-state iteration-result)
                        breadcrumb-text   (:breadcrumb iteration-result)
                        ;; Phase 0b — plan-edit-distance + change marker.
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
                        ;; Augment iteration metadata with Phase 0/0b counters.
                        iteration-metadata-with-metrics
                        (merge (or (iteration-metadata) {})
                          {:plan-edit-distance plan-edit-dist
                           :plan-changed?      (boolean (and plan-diff (pos? plan-edit-dist)))
                           :var-history-recall-count
                           (count (filter #(and (string? (:code %))
                                             (re-find #"\(var-history\b" (:code %)))
                             (or expressions [])))
                           :expression-redundancy-fraction 0.0   ;; populated in Phase 2
                           :dedup-saves                    0     ;; populated in Phase 2
                           :plan-validation-error          (some-> plan-validation :type name)})
                        ;; PEV gate (PLAN.md §10.1): if the model emitted
                        ;; :answer with open plan items and no
                        ;; :abandon-reason, surface a structured nudge
                        ;; that the next iteration will see. We don't reject
                        ;; the answer here yet (Phase 6 makes it strict);
                        ;; for now we annotate so the model gets a hint.
                        gate-violation
                        (when (and final-result
                                effective-plan
                                (some #(contains? #{:in_progress :pending}
                                         (iterate/item-status-key %))
                                  (:items effective-plan))
                                (str/blank? (str (:abandon-reason final-result))))
                          {:type :vis/incomplete-plan-on-answer
                           :open-item-ids (mapv :id
                                            (filter #(contains? #{:in_progress :pending}
                                                       (iterate/item-status-key %))
                                              (:items effective-plan)))})
                        iteration-id (db/store-iteration! (:db-info environment)
                                  {:query-id query-id :expressions expressions :vars vars-snapshot
                                   :thinking thinking
                                   :answer (when final-result (iterate/answer-str (:answer final-result)))
                                   :duration-ms (or (:duration-ms iteration-result) 0)
                                   :llm-messages (:llm-messages iteration-result)
                                   :llm-model (:llm-model iteration-result)
                                   ;; Phase 1 plan slot persistence.
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
                    (if final-result
                      (do (iterate/log-stage! :final iteration
                            {:answer (truncate (iterate/answer-str (:answer final-result)) 200)
                             :confidence (:confidence final-result) :iterations (inc iteration)})
                        (iterate/log-stage! :iteration-end iteration
                          {:blocks (count expressions) :errors (count (filter :error expressions))
                           :times (mapv :execution-time-ms expressions)})
                        (when on-chunk
                          (on-chunk {:iteration iteration :thinking thinking
                                     :code (mapv :code expressions)
                                     :results (mapv #(if (:error %) (str "ERROR: " (:error %)) (pr-str (:result %))) expressions)
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

                      (if (empty? expressions)
                        (do (iterate/log-stage! :empty iteration {})
                          (iterate/log-stage! :iteration-end iteration {:blocks 0 :errors 0 :times []})
                          (recur (merge loop-state
                                   {:iteration (inc iteration) :trace (conj trace trace-entry)
                                    :previous-next-model next-model :previous-next-reasoning next-reasoning})))

                        (do (iterate/log-stage! :iteration-end iteration
                              {:blocks (count expressions) :errors (count (filter :error expressions))
                               :times (mapv :execution-time-ms expressions)})
                          (when on-chunk
                            (on-chunk {:iteration iteration :thinking thinking
                                       :code (mapv :code expressions)
                                       :results (mapv #(if (:error %) (str "ERROR: " (:error %)) (pr-str (:result %))) expressions)
                                       :stdouts (mapv #(or (:stdout %) "") expressions)
                                       :durations (mapv #(or (:execution-time-ms %) 0) expressions)
                                       :successes (mapv #(nil? (:error %)) expressions)
                                       :done? false}))
                          (let [had-success? (some #(nil? (:error %)) expressions)
                                next-errors (if had-success? 0 (inc consecutive-errors))
                                _ (when had-success? (swap! var-index-atom update :current-revision inc))]
                            (recur (merge loop-state
                                     {:iteration (inc iteration) :messages messages
                                      :trace (conj trace trace-entry) :consecutive-errors next-errors
                                      :previous-expressions expressions :previous-iteration iteration
                                      :previous-next-model next-model
                                      :previous-next-reasoning next-reasoning}))))))))))))))))

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
      (contains? #{:max-iterations
                   :error-budget-exhausted
                   :error}
        status)                                          :error
      :else                                              :complete)))

(defn run-query!
  "Store query → iteration-loop → update query → return result.

   Phase 1: derives `:prior-outcome` (one of `:complete`,
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
;; Phase 1 - Prepare query context
;; -----------------------------------------------------------------------------

(defn- prepare-query-context
  "Validates inputs, resolves SCI bindings, sets up atoms.
   Returns a map of all computed context needed for subsequent phases."
  [env messages opts]
  (let [{:keys [spec model max-iterations
                max-context-tokens concurrency
                system-prompt debug? hooks cancel-atom eval-timeout-ms
                reasoning-default routing]
         :or   {max-iterations      rlm-spec/MAX_ITERATIONS
                debug?              false}} opts]
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
          max-iterations-atom    (atom max-iterations)
          budget-bindings        {'request-more-iterations
                                  (fn [n]
                                    (let [requested (max 1 (long n))
                                          current   @max-iterations-atom
                                          new-budget (+ current requested)]
                                      (reset! max-iterations-atom new-budget)
                                      (tel/log! {:level :info :id ::iteration-budget
                                                 :data {:requested requested :granted requested
                                                        :new-budget new-budget}
                                                 :msg  "LLM requested more iterations"})
                                      {:granted requested :new-budget new-budget}))}
          sci-ctx                (:sci-ctx env)
          _                      (sci-env/bump-var-index! env)
          _                      (let [per-query (merge budget-bindings
                                                   (or custom-bindings {}))]

                                   (doseq [[sym val] per-query]
                                     (when val
                                       (sci-env/sci-update-binding! sci-ctx sym val))))
          _                      (sci-env/bump-var-index! env)
          current-iteration-id-atom (atom nil)
          environment            (assoc env
                                   :max-iterations-atom max-iterations-atom
                                   :current-iteration-id-atom current-iteration-id-atom)
          environment-id         (:environment-id env)]
      {:cancel-atom            cancel-atom
       :query-str              query-str
       :router                 env-router
       :root-model             root-model
       :db-info                db-info
       :current-iteration-atom current-iteration-atom
       :max-iterations-atom    max-iterations-atom
       :environment             environment
       :environment-id         environment-id
       :spec                   spec
       :max-iterations         max-iterations
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
;; Phase 2 - Run iteration phase
;; -----------------------------------------------------------------------------

(defn- run-iteration-phase
  "Runs the main iteration loop via run-query!.
   Returns iteration-result, query-id, cost atoms, and merge-cost! fn."
  [{:keys [environment query-str messages history-messages spec max-iterations
           max-context-tokens system-prompt
           current-iteration-atom hooks cancel-atom db-info
           reasoning-default routing]}]
  (let [iteration-result (run-query! environment query-str
                           (cond-> {:max-iterations         max-iterations
                                    :output-spec            spec
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
;; Phase 5 - Finalize query result
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
      ;; :max-iterations / :error-budget-exhausted) to the caller. Leaving
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
     - :max-iterations - Initial iteration budget (default: 4). No cap —
       system nudges tell the LLM how to extend when needed.
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
      - :status - Only present on failure, e.g. :max-iterations."
  ([environment messages]
   (query! environment messages {}))
  ([environment messages opts]
   (let [ctx (prepare-query-context environment messages opts)
         {:keys [eval-timeout-ms concurrency
                 debug? query-str root-model max-iterations
                 db-info max-iterations-atom
                 environment-id]} ctx
         merged-concurrency (merge rlm-spec/DEFAULT_CONCURRENCY concurrency)]
     (binding [rlm-spec/*rlm-ctx*               {:rlm-environment-id environment-id :rlm-type :main
                                                 :rlm-debug? debug? :rlm-phase :query
                                                 :db-info db-info
                                                 :conversation-soul-id (:conversation-id environment)}
               rlm-spec/*eval-timeout-ms*       (rlm-spec/clamp-eval-timeout-ms
                                                  (or eval-timeout-ms rlm-spec/*eval-timeout-ms*))
               rlm-spec/*concurrency*           merged-concurrency]
       (tel/with-ctx+ {:db-info db-info
                       :conversation-soul-id (:conversation-id environment)}
         (iterate/log-stage! :query-start 0
           {:model root-model
            :max-iterations max-iterations
            :reasoning? (boolean (:reasoning? (first (mapcat :models (:providers (:router environment))))))
            :query query-str})
         (let [start-time   (System/nanoTime)
               phase2       (run-iteration-phase ctx)
               {:keys [iteration-result query-id
                       total-tokens-atom total-cost-atom merge-cost!]} phase2
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
