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
   [com.blockether.vis.loop.runtime.shared :as rt-shared :refer [realize-value truncate]]
   [com.blockether.vis.loop.runtime.conversation.environment.query.iteration.core :as iterate]
   [com.blockether.vis.persistance.spec :as rlm-spec
    :refer [ITERATION_SPEC_NON_REASONING ITERATION_SPEC_REASONING *rlm-ctx*]]
   [com.blockether.vis.loop.runtime.conversation.environment.core :as sci-env]
   [com.blockether.vis.loop.runtime.prompt :as prompt]
   [com.blockether.svar.internal.util :as util]
   [edamame.core :as edamame]
   [taoensso.telemere :as tel])
  (:import
   [java.util.concurrent ConcurrentHashMap Semaphore]))

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
  [rlm-env expressions]
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
        locals (iterate/get-locals rlm-env)]
    (->> locals
      (keep (fn [[sym value]]
              (when (contains? defined sym)
                (let [realized (realize-value value)
                      exec-info (get sym->exec sym)]
                  ;; Accept ALL values — freeze-safe in the persistence layer
                  ;; handles non-serializable types (fns → {:rlm/ref :expr}).
                  (cond-> {:name (str sym) :value realized :code (:expr exec-info)}
                    (:time-ms exec-info) (assoc :time-ms (:time-ms exec-info)))))))
      vec)))

(defn update-system-vars!
  "Bind *reasoning* and *answer* into the SCI sandbox after an iteration."
  [rlm-env {:keys [thinking final-result final-answer]}]
  (when (seq thinking)
    (sci-env/bind-and-bump! rlm-env '*reasoning* thinking))
  (when final-result
    (sci-env/bind-and-bump! rlm-env '*answer* final-answer)))

(defn inject-system-var-snapshots
  "Append SYSTEM var entries to a vars-snapshot vec for persistence."
  [vars-snapshot {:keys [iteration query thinking final-result final-answer]}]
  (cond-> vars-snapshot
    (zero? iteration) (conj {:name "*query*" :value query :code ";; SYSTEM var"})
    (seq thinking) (conj {:name "*reasoning*" :value thinking :code ";; SYSTEM var"})
    final-result (conj {:name "*answer*" :value final-answer :code ";; SYSTEM var"})))

;; -----------------------------------------------------------------------------
;; Iteration loop + run-query! (inlined from former query/base)
;; -----------------------------------------------------------------------------

(def ^:private FRESH_ITER_CARRY
  {:prev-expressions nil :prev-iteration -1
   :prev-next-model nil :prev-next-reasoning nil})

(def ^:private balanced-reasoning :balanced)

(defn- status->id [status]
  (when status (keyword "rlm.status" (name status))))

(defn iteration-loop
  "The core iteration loop. Runs N iterations of: assemble → ask LLM → execute → persist."
  [rlm-env query
   {:keys [output-spec max-context-tokens system-prompt
           query-id history-messages
           max-iterations max-consecutive-errors max-restarts
           hooks cancel-atom current-iteration-atom
           reasoning-default routing]}]
  (let [max-iterations (or max-iterations rlm-spec/MAX_ITERATIONS)
        max-consecutive-errors (or max-consecutive-errors 5)
        max-restarts (or max-restarts 3)
        max-iter-atom (:max-iterations-atom rlm-env)
        effective-max-iterations (fn [] (if max-iter-atom @max-iter-atom max-iterations))
        effective-model (:name (resolve-effective-model (:router rlm-env)))
        _ (assert effective-model "Router must resolve a root model")
        max-context-tokens (or max-context-tokens
                             (long (* 0.6 (router/context-limit effective-model))))
        has-reasoning? (boolean (provider-has-reasoning? (:router rlm-env)))
        base-reasoning-level (or (iterate/normalize-reasoning-level reasoning-default) balanced-reasoning)
        system-prompt (prompt/build-system-prompt
                        {:system-prompt system-prompt})
        initial-user-content query
        initial-messages (iterate/assemble-initial-messages
                           {:system-prompt system-prompt
                            :initial-user-content initial-user-content
                            :history-messages history-messages})
        db-info (:db-info rlm-env)
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
        var-index-atom (or (:var-index-atom rlm-env)
                         (atom {:index nil :revision -1 :current-revision 0}))
        on-iteration (:on-iteration hooks)
        on-chunk (:on-chunk hooks)
        on-cancel (:on-cancel hooks)
        emit-hook! (fn [hook-fn payload log-msg]
                     (when hook-fn
                       (try (hook-fn payload)
                         (catch Exception e
                           (tel/log! {:level :warn :data (rt-shared/format-exception-short e)} log-msg)))))
        active-extension-names (fn []
                                 (when-let [exts (some-> (:extensions rlm-env) deref seq)]
                                   (mapv (comp str :ext/namespace) exts)))
        iter-metadata (fn []
                        (let [ext-names (active-extension-names)]
                          (when (seq ext-names)
                            {:extensions ext-names})))]
    (sci-env/bind-and-bump! rlm-env '*query* query)
    (when-let [a (:current-iteration-id-atom rlm-env)] (reset! a nil))
    (binding [*rlm-ctx* (merge *rlm-ctx* {:rlm-phase :iteration-loop})]
      (loop [loop-state (merge {:iteration 0 :messages initial-messages
                                :trace [] :consecutive-errors 0 :restarts 0}
                          FRESH_ITER_CARRY)]
        (let [{:keys [iteration messages trace consecutive-errors restarts
                      prev-expressions prev-iteration
                      prev-next-model prev-next-reasoning]} loop-state]
          (when current-iteration-atom (reset! current-iteration-atom iteration))
          (cond
            (when cancel-atom @cancel-atom)
            (do (iterate/log-stage! :error iteration {:reason :cancelled})
              (emit-hook! on-cancel {:iteration iteration :status :cancelled
                                     :status-id (status->id :cancelled)} "on-cancel hook threw")
              (merge {:answer nil :status :cancelled :status-id (status->id :cancelled)
                      :trace trace :iterations iteration} (finalize-cost)))

            (>= iteration (effective-max-iterations))
            (let [max-iter (effective-max-iterations)
                  last-thinking (some->> trace reverse (map :thinking)
                                  (filter #(and (string? %) (not (str/blank? %)))) first)
                  fallback (str "⚠️ Iteration limit (" iteration "/" max-iter ") reached.\n\n"
                             (when last-thinking (str "**Last reasoning:**\n\n" (truncate last-thinking 800) "\n\n"))
                             "**What to try:** Rephrase more narrowly.")]
              (iterate/log-stage! :error iteration {:reason :max-iterations :max max-iter})
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
                  (recur (merge loop-state {:iteration (inc iteration) :messages msgs
                                            :trace trace :consecutive-errors 0 :restarts (inc restarts)}
                           FRESH_ITER_CARRY)))
                (let [errs (->> trace reverse (keep :error) (take 3)
                             (map #(str "- " (or (:message %) (str %)))) (str/join "\n"))
                      fallback (str "⚠️ Too many errors (" consecutive-errors ") across "
                                 (inc restarts) " restart(s).\n\n"
                                 (when-not (str/blank? errs) (str "**Recent errors:**\n\n" errs "\n\n")))]
                  (merge {:answer fallback
                          :status :error-budget-exhausted :status-id (status->id :error-budget-exhausted)
                          :trace trace :iterations iteration} (finalize-cost))))

              (let [reasoning-level (when has-reasoning?
                                      (or prev-next-reasoning
                                        (iterate/reasoning-level-for-errors base-reasoning-level consecutive-errors)))
                    _ (iterate/log-stage! :iter-start iteration {:msg-count (count messages) :reasoning reasoning-level})
                    prior-thinking-body (iterate/build-prior-thinking rlm-env db-info query-id)
                    cross-query-handover (when (zero? iteration)
                                           (iterate/build-cross-query-handover
                                             db-info (:conversation-id rlm-env) query-id
                                             (:parent-iteration-id rlm-env)))
                    prior-thinking (cond
                                     (and cross-query-handover prior-thinking-body)
                                     (str cross-query-handover "\n\n" prior-thinking-body)
                                     cross-query-handover cross-query-handover
                                     :else prior-thinking-body)
                    iteration-context (iterate/build-iteration-context rlm-env
                                        {:iteration iteration
                                         :current-max-iterations (effective-max-iterations)
                                         :prior-thinking prior-thinking
                                         :prev-expressions prev-expressions
                                         :prev-iteration prev-iteration
                                         :call-counts-atom call-counts-atom})
                    base-messages (iterate/trim-to-initial-history messages (count initial-messages))
                    effective-messages (cond-> base-messages
                                         (not (str/blank? iteration-context))
                                         (conj {:role "user" :content iteration-context}))
                    effective-overrides (cond-> {}
                                          prev-next-model (assoc :optimize prev-next-model)
                                          prev-next-reasoning (assoc :reasoning prev-next-reasoning))
                    resolved-model (resolve-effective-model (:router rlm-env) effective-overrides)
                    effective-routing (merge (or routing {}) effective-overrides)
                    iteration-result (try
                                       (iterate/run-iteration rlm-env effective-messages
                                         {:iteration-spec (if has-reasoning? ITERATION_SPEC_REASONING ITERATION_SPEC_NON_REASONING)
                                          :iteration iteration :reasoning-level reasoning-level
                                          :routing effective-routing
                                          :resolved-model resolved-model
                                          :on-chunk on-chunk})
                                       (catch Exception e
                                         (iterate/handle-iteration-exception! e
                                           {:iteration iteration :messages effective-messages
                                            :routing effective-routing :reasoning-level reasoning-level})))]
                (if-let [iter-err (::iterate/iteration-error iteration-result)]
                  (let [error-feedback (str "[Iteration " (inc iteration) "/" (effective-max-iterations) "]\n"
                                         "<error>LLM call failed: " (:message iter-err) "</error>\n"
                                         "Adjust your approach or emit :final with what you have.")
                        trace-entry {:iteration iteration :error iter-err :final? false}
                        empty-reasoning (when (= :svar.llm/empty-content (:type iter-err))
                                          (:reasoning (:data iter-err)))
                        err-iter-id (iterate/store-iteration! rlm-env
                                      {:query-id query-id :vars [] :expressions nil
                                       :thinking empty-reasoning :duration-ms 0 :error iter-err
                                       :llm-messages effective-messages
                                       :llm-model (str (:name resolved-model))
                                       :metadata (iter-metadata)})]
                    (when-let [a (:current-iteration-id-atom rlm-env)] (reset! a err-iter-id))
                    (emit-hook! on-iteration
                      {:iteration iteration :status :error :status-id (status->id :error)
                       :thinking empty-reasoning :expressions nil :final-result nil
                       :error iter-err :duration-ms 0} "on-iteration (error)")
                    (recur (merge loop-state
                             {:iteration (inc iteration)
                              :messages (conj messages {:role "user" :content error-feedback})
                              :trace (conj trace trace-entry)
                              :consecutive-errors (inc consecutive-errors) :restarts restarts}
                             FRESH_ITER_CARRY)))

                  (let [_ (accumulate-usage! (:api-usage iteration-result))
                        {:keys [thinking expressions final-result next-model next-reasoning]} iteration-result
                        final-answer (when final-result (:answer final-result))
                        _ (update-system-vars! rlm-env
                            {:thinking thinking :final-result final-result :final-answer final-answer})
                        vars-snapshot (restorable-var-snapshots rlm-env expressions)
                        vars-snapshot (inject-system-var-snapshots vars-snapshot
                                        {:iteration iteration :query query :thinking thinking
                                         :final-result final-result :final-answer final-answer})
                        iter-id (iterate/store-iteration! rlm-env
                                  {:query-id query-id :expressions expressions :vars vars-snapshot
                                   :thinking thinking
                                   :answer (when final-result (iterate/answer-str (:answer final-result)))
                                   :duration-ms (or (:duration-ms iteration-result) 0)
                                   :llm-messages (:llm-messages iteration-result)
                                   :llm-model (:llm-model iteration-result)
                                   :metadata (iter-metadata)})
                        _ (when-let [a (:current-iteration-id-atom rlm-env)] (reset! a iter-id))
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
                        (iterate/log-stage! :iter-end iteration
                          {:blocks (count expressions) :errors (count (filter :error expressions))
                           :times (mapv :execution-time-ms expressions)})
                        (when on-chunk
                          (on-chunk {:iteration iteration :thinking thinking
                                     :code (mapv :code expressions)
                                     :final {:answer (:answer final-result) :confidence (:confidence final-result)
                                             :iterations (inc iteration) :status :success}
                                     :done? true}))
                        (merge (cond-> {:answer (:answer final-result) :trace (conj trace trace-entry)
                                        :iterations (inc iteration) :confidence (:confidence final-result)}
                                 (:reasoning final-result) (assoc :reasoning (:reasoning final-result)))
                          (finalize-cost)))

                      (if (empty? expressions)
                        (do (iterate/log-stage! :empty iteration {})
                          (iterate/log-stage! :iter-end iteration {:blocks 0 :errors 0 :times []})
                          (recur (merge loop-state
                                   {:iteration (inc iteration) :trace (conj trace trace-entry)
                                    :prev-next-model next-model :prev-next-reasoning next-reasoning})))

                        (do (iterate/log-stage! :iter-end iteration
                              {:blocks (count expressions) :errors (count (filter :error expressions))
                               :times (mapv :execution-time-ms expressions)})
                          (when on-chunk
                            (on-chunk {:iteration iteration :thinking thinking
                                       :code (mapv :code expressions) :done? false}))
                          (let [had-success? (some #(nil? (:error %)) expressions)
                                next-errors (if had-success? 0 (inc consecutive-errors))
                                _ (when had-success? (swap! var-index-atom update :current-revision inc))]
                            (recur (merge loop-state
                                     {:iteration (inc iteration) :messages messages
                                      :trace (conj trace trace-entry) :consecutive-errors next-errors
                                      :prev-expressions expressions :prev-iteration iteration
                                      :prev-next-model next-model
                                      :prev-next-reasoning next-reasoning}))))))))))))))))

(defn run-query!
  "Store query → iteration-loop → update query → return result."
  [env query loop-opts]
  (rt-shared/validate! ::rt-shared/env env)
  (rt-shared/validate! ::rt-shared/non-blank-string query)
  (let [query-id (db/store-query! (:db-info env)
                   {:parent-conversation-id (:conversation-id env)
                    :query query
                    :messages nil
                    :status :running})
        result (iteration-loop env query (assoc loop-opts :query-id query-id))
        _ (db/update-query! (:db-info env) query-id
            {:answer      (:answer result)
             :iterations  (:iterations result)
             :duration-ms (:duration-ms result)
             :status      (or (:status result) :success)
             :tokens      (:tokens result)
             :cost        (:cost result)})]
    (assoc result :query-id query-id)))

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
      (anomaly/incorrect! "Invalid RLM environment" {:type :rlm/invalid-env}))
    (when-not (and (vector? messages) (seq messages))
      (anomaly/incorrect! "messages must be a non-empty vector of message maps, e.g. [(llm/user \"...\")]"
        {:type :rlm/invalid-messages :got (type messages)}))
    (when (and (some? eval-timeout-ms) (not (integer? eval-timeout-ms)))
      (anomaly/incorrect! ":eval-timeout-ms must be an integer (milliseconds)"
        {:type     :rlm/invalid-eval-timeout
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
          rlm-router             (:router env)
          root-model             (or (when rlm-router (:name (resolve-effective-model rlm-router))) model)
          db-info                (:db-info env)
          custom-bindings        (loop-core/custom-bindings env)
          current-iteration-atom (atom 0)
          max-iterations-atom    (atom max-iterations)
          budget-bindings        {'request-more-iterations
                                  (fn [n]
                                    (let [requested  (max 1 (min (long n) (long rlm-spec/MAX_EXTENSION_PER_REQUEST)))
                                          current    @max-iterations-atom
                                          new-budget (min (+ current requested) (long rlm-spec/MAX_ITERATION_CAP))]
                                      (reset! max-iterations-atom new-budget)
                                      (let [granted (- new-budget current)]
                                        (tel/log! {:level :info :id ::iteration-budget
                                                   :data {:requested  (long n) :granted granted
                                                          :new-budget new-budget :cap rlm-spec/MAX_ITERATION_CAP}
                                                   :msg  "LLM requested more iterations"})
                                        {:granted granted :new-budget new-budget :cap rlm-spec/MAX_ITERATION_CAP})))}
          sci-ctx                (:sci-ctx env)
          _                      (sci-env/bump-var-index! env)
          _                      (let [per-query (merge budget-bindings
                                                   (or custom-bindings {}))]

                                   (doseq [[sym val] per-query]
                                     (when val
                                       (sci-env/sci-update-binding! sci-ctx sym val))))
          _                      (sci-env/bump-var-index! env)
          rlm-env                (assoc env :max-iterations-atom max-iterations-atom)
          env-id                 (:env-id env)]
      {:cancel-atom            cancel-atom
       :query-str              query-str
       :rlm-router             rlm-router
       :root-model             root-model
       :db-info                db-info
       :current-iteration-atom current-iteration-atom
       :max-iterations-atom    max-iterations-atom
       :rlm-env                rlm-env
       :env-id                 env-id
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
  [{:keys [rlm-env query-str messages history-messages spec max-iterations
           max-context-tokens system-prompt
           current-iteration-atom hooks cancel-atom db-info
           reasoning-default routing]}]
  (let [iteration-result (run-query! rlm-env query-str
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
        {iter-tokens :tokens
         iter-cost   :cost} iteration-result
        total-tokens-atom (atom (or iter-tokens {}))
        total-cost-atom   (atom (or iter-cost {}))
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
   render `model · N iter · duration · tokens · $total` after a restart."
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
              (tel/log! {:level :warn :data (rt-shared/format-exception-short e)
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
            (tel/log! {:level :warn :data (rt-shared/format-exception-short e)
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
     - :max-iterations - Max code iterations (default: 50).
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
                 env-id]} ctx
         merged-concurrency (merge rlm-spec/DEFAULT_CONCURRENCY concurrency)]
     (binding [rlm-spec/*rlm-ctx*               {:rlm-env-id env-id :rlm-type :main
                                                 :rlm-debug? debug? :rlm-phase :query
                                                 :db-info db-info
                                                 :conversation-soul-id (some-> environment :conversation-id second)}
               rlm-spec/*eval-timeout-ms*       (rlm-spec/clamp-eval-timeout-ms
                                                  (or eval-timeout-ms rlm-spec/*eval-timeout-ms*))
               rlm-spec/*concurrency*           merged-concurrency]
       (tel/with-ctx+ {:db-info db-info
                       :conversation-soul-id (some-> environment :conversation-id second)}
         (iterate/log-stage! :query-start 0
           {:model root-model
            :max-iterations max-iterations
            :reasoning? (boolean (:reasoning? (first (mapcat :models (:providers (:router environment))))))
            :query query-str})
         (let [start-time   (System/nanoTime)
               phase2       (run-iteration-phase ctx)
               {:keys [iteration-result query-id
                       total-tokens-atom total-cost-atom merge-cost!]} phase2
               {iter-answer :answer
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
                :answer            iter-answer
                :total-tokens-atom total-tokens-atom
                :total-cost-atom   total-cost-atom})
             (finalize-query-result
               ctx
               {:query-id         query-id
                :start-time        start-time
                :iterations        iterations
                :trace             trace
                :answer            iter-answer
                :confidence        confidence
                :reasoning         reasoning
                :total-tokens-atom total-tokens-atom
                :total-cost-atom   total-cost-atom}))))))))
