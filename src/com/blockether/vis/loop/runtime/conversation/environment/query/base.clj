(ns com.blockether.vis.loop.runtime.conversation.environment.query.base
  "Core engine: iteration-loop + run-query!

   This is the heart of the agent. Everything flows through here:
     conversation → query → iteration-loop → run-iteration → SCI execute

   Uses:
   - query/shared for router, var snapshots, system vars
   - query/persistence for DB operations
   - iteration/* for per-iteration stages"
  (:require
   [clojure.string :as str]
   [com.blockether.svar.internal.router :as router]
   [com.blockether.vis.loop.runtime.conversation.environment.query.shared :as shared
    :refer [            restorable-var-snapshots update-system-vars! inject-system-var-snapshots]]
   [com.blockether.vis.loop.runtime.conversation.environment.query.persistence :as query-db]
   [com.blockether.vis.loop.runtime.conversation.environment.query.iteration.persistence :as iter-db]
   [com.blockether.vis.loop.runtime.conversation.environment.query.iteration.shared :as iter-shared
    :refer [rlm-debug! rlm-stage! answer-str normalize-reasoning-level reasoning-level-for-errors]]
   [com.blockether.vis.loop.runtime.conversation.environment.query.iteration.assemble :as assemble]
   [com.blockether.vis.loop.runtime.conversation.environment.query.iteration.core :as iterate]
   [com.blockether.vis.loop.runtime.conversation.environment.core :as rlm-tools]
   [com.blockether.vis.loop.runtime.prompt :as prompt]
   [com.blockether.vis.loop.runtime.shared :as rt-shared :refer [truncate]]
   [com.blockether.vis.loop.storage.db :as rlm-db]
   [com.blockether.vis.loop.storage.schema :as schema
    :refer [ITERATION_SPEC_NON_REASONING ITERATION_SPEC_REASONING *rlm-ctx*]]
   [taoensso.trove :as trove]))

;; Re-export from shared for callers that require base
(def get-router              shared/get-router)
(def reset-router!           shared/reset-router!)
(def ask!                    shared/ask!)

;; =============================================================================
;; iteration-loop — the main recur machine
;; =============================================================================

(def ^:private FRESH_ITER_CARRY
  {:prev-executions nil :prev-iteration -1
   :prev-next-model nil :prev-next-reasoning nil})

(def ^:private reasoning-quick :quick)

(defn- status->id [status]
  (when status (keyword "rlm.status" (name status))))

(defn iteration-loop
  "The core iteration loop. Runs N iterations of: assemble → ask LLM → execute → persist.
   Returns {:answer :trace :iterations :tokens :cost :status ...}."
  [rlm-env query
   {:keys [output-spec max-context-tokens custom-docs system-prompt
           pre-fetched-context query-id history-messages
           max-iterations max-consecutive-errors max-restarts
           hooks cancel-atom current-iteration-atom
           reasoning-default routing]}]
  (let [max-iterations (or max-iterations schema/MAX_ITERATIONS)
        max-consecutive-errors (or max-consecutive-errors 5)
        max-restarts (or max-restarts 3)
        max-iter-atom (:max-iterations-atom rlm-env)
        effective-max-iterations (fn [] (if max-iter-atom @max-iter-atom max-iterations))
        effective-model (shared/resolve-root-model (:router rlm-env))
        _ (assert effective-model "Router must resolve a root model")
        max-context-tokens (or max-context-tokens
                             (long (* 0.6 (router/context-limit effective-model))))
        has-reasoning? (boolean (shared/provider-has-reasoning? (:router rlm-env)))
        base-reasoning-level (or (normalize-reasoning-level reasoning-default) reasoning-quick)
        has-docs? (when-let [db (:db-info rlm-env)]
                    (pos? (count (rlm-db/db-list-documents db {:limit 1 :include-toc? false}))))
        system-prompt (prompt/build-system-prompt
                        {:output-spec output-spec :custom-docs custom-docs
                         :has-documents? has-docs? :has-reasoning? has-reasoning?
                         :system-prompt system-prompt :max-context-tokens max-context-tokens
                         :env rlm-env
                         :tool-defs (when-let [a (:tool-registry-atom rlm-env)] (vals @a))
                         :skill-registry (when-let [a (:skill-registry-atom rlm-env)] @a)})
        initial-user-content (str "{:requirement " (pr-str query)
                               (when pre-fetched-context (str "\n :plan " (pr-str pre-fetched-context)))
                               "}")
        initial-messages (assemble/assemble-initial-messages
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
                           (trove/log! {:level :warn :data (rt-shared/format-exception-short e) :msg log-msg})))))]
    (rlm-tools/bind-and-bump! rlm-env '*query* query)
    (when-let [a (:current-iteration-id-atom rlm-env)] (reset! a nil))
    (binding [*rlm-ctx* (merge *rlm-ctx* {:rlm-phase :iteration-loop})]
      (loop [loop-state (merge {:iteration 0 :messages initial-messages
                                :trace [] :consecutive-errors 0 :restarts 0}
                          FRESH_ITER_CARRY)]
        (let [{:keys [iteration messages trace consecutive-errors restarts
                      prev-executions prev-iteration
                      prev-next-model prev-next-reasoning]} loop-state]
          (when current-iteration-atom (reset! current-iteration-atom iteration))
          (cond
            ;; Cancelled
            (when cancel-atom @cancel-atom)
            (do (rlm-stage! :error iteration {:reason :cancelled})
              (emit-hook! on-cancel {:iteration iteration :status :cancelled
                                     :status-id (status->id :cancelled)} "on-cancel hook threw")
              (merge {:answer nil :status :cancelled :status-id (status->id :cancelled)
                      :trace trace :iterations iteration} (finalize-cost)))

            ;; Max iterations
            (>= iteration (effective-max-iterations))
            (let [max-iter (effective-max-iterations)
                  last-thinking (some->> trace reverse (map :thinking)
                                  (filter #(and (string? %) (not (str/blank? %)))) first)
                  fallback (str "⚠️ Iteration limit (" iteration "/" max-iter ") reached.\n\n"
                              (when last-thinking (str "**Last reasoning:**\n\n" (truncate last-thinking 800) "\n\n"))
                              "**What to try:** Rephrase more narrowly.")]
              (rlm-stage! :error iteration {:reason :max-iterations :max max-iter})
              (merge {:answer fallback
                      :status :max-iterations :status-id (status->id :max-iterations)
                      :trace trace :iterations iteration} (finalize-cost)))

            :else
            (if (>= consecutive-errors max-consecutive-errors)
              ;; Strategy restart or give up
              (if (< restarts max-restarts)
                (let [failed (->> trace (filter :error) (take 3)
                               (map #(str "- " (get-in % [:error :message] (str (:error %)))))
                               (str/join "\n"))
                      hint (str "{:strategy-restart true\n :errors " (pr-str failed)
                              "\n :instruction \"Start fresh with a DIFFERENT strategy.\"\n :requirement "
                              (pr-str query) "}")
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

              ;; ── Normal iteration ──
              (let [reasoning-level (when has-reasoning?
                                      (or prev-next-reasoning
                                        (reasoning-level-for-errors base-reasoning-level consecutive-errors)))
                    _ (rlm-stage! :iter-start iteration {:msg-count (count messages) :reasoning reasoning-level})
                    prior-thinking-body (assemble/build-prior-thinking rlm-env db-info query-id)
                    cross-query-handover (when (zero? iteration)
                                           (assemble/build-cross-query-handover
                                             db-info (:conversation-id rlm-env) query-id
                                             (:parent-iteration-id rlm-env)))
                    prior-thinking (cond
                                     (and cross-query-handover prior-thinking-body)
                                     (str cross-query-handover "\n\n" prior-thinking-body)
                                     cross-query-handover cross-query-handover
                                     :else prior-thinking-body)
                    iteration-context (assemble/build-iteration-context rlm-env
                                        {:iteration iteration
                                         :current-max-iterations (effective-max-iterations)
                                         :prior-thinking prior-thinking
                                         :prev-executions prev-executions
                                         :prev-iteration prev-iteration
                                         :call-counts-atom call-counts-atom})
                    base-messages (assemble/trim-to-initial-history messages (count initial-messages))
                    effective-messages (cond-> base-messages
                                         (not (str/blank? iteration-context))
                                         (conj {:role "user" :content iteration-context}))
                    effective-routing (cond-> (or routing {})
                                        prev-next-model (assoc :optimize prev-next-model))
                    iteration-result (try
                                       (iterate/run-iteration rlm-env effective-messages
                                         {:iteration-spec (if has-reasoning? ITERATION_SPEC_REASONING ITERATION_SPEC_NON_REASONING)
                                          :iteration iteration :reasoning-level reasoning-level
                                          :routing effective-routing})
                                       (catch Exception e
                                         (iterate/handle-iteration-exception! e
                                           {:iteration iteration :messages effective-messages
                                            :routing effective-routing :reasoning-level reasoning-level})))]
                (if-let [iter-err (::iterate/iteration-error iteration-result)]
                  ;; ── Error path ──
                  (let [error-feedback (str "[Iteration " (inc iteration) "/" (effective-max-iterations) "]\n"
                                         "<error>LLM call failed: " (:message iter-err) "</error>\n"
                                         "Adjust your approach or emit :final with what you have.")
                        trace-entry {:iteration iteration :error iter-err :final? false}
                        empty-reasoning (when (= :svar.llm/empty-content (:type iter-err))
                                          (:reasoning (:data iter-err)))
                        err-iter-id (iter-db/store! rlm-env
                                       {:query-id query-id :vars [] :executions nil
                                        :thinking empty-reasoning :duration-ms 0 :error iter-err})]
                    (when-let [a (:current-iteration-id-atom rlm-env)] (reset! a err-iter-id))
                    (emit-hook! on-iteration
                      {:iteration iteration :status :error :status-id (status->id :error)
                       :thinking empty-reasoning :executions nil :final-result nil
                       :error iter-err :duration-ms 0} "on-iteration (error)")
                    (recur (merge loop-state
                             {:iteration (inc iteration)
                              :messages (conj messages {:role "user" :content error-feedback})
                              :trace (conj trace trace-entry)
                              :consecutive-errors (inc consecutive-errors) :restarts restarts}
                             FRESH_ITER_CARRY)))

                  ;; ── Success path ──
                  (let [_ (accumulate-usage! (:api-usage iteration-result))
                        {:keys [thinking executions final-result next-model next-reasoning]} iteration-result
                        final-answer (when final-result (:answer final-result))
                        _ (update-system-vars! rlm-env
                            {:thinking thinking :final-result final-result :final-answer final-answer})
                        vars-snapshot (restorable-var-snapshots rlm-env executions)
                        vars-snapshot (inject-system-var-snapshots vars-snapshot
                                        {:iteration iteration :query query :thinking thinking
                                         :final-result final-result :final-answer final-answer})
                        iter-id (iter-db/store! rlm-env
                                   {:query-id query-id :executions executions :vars vars-snapshot
                                    :thinking thinking
                                    :answer (when final-result (answer-str (:answer final-result)))
                                    :duration-ms (or (:duration-ms iteration-result) 0)})
                        _ (when-let [a (:current-iteration-id-atom rlm-env)] (reset! a iter-id))
                        _ (emit-hook! on-iteration
                            {:iteration iteration
                             :status (cond final-result :final (empty? executions) :empty :else :success)
                             :status-id (status->id (cond final-result :final (empty? executions) :empty :else :success))
                             :thinking thinking :executions executions :final-result final-result
                             :error nil :duration-ms (or (:duration-ms iteration-result) 0)}
                            "on-iteration (success)")
                        trace-entry {:iteration iteration :thinking thinking
                                     :executions executions :final? (boolean final-result)}]
                    (if final-result
                      ;; ── Final answer ──
                      (do (rlm-stage! :final iteration
                            {:answer (truncate (answer-str (:answer final-result)) 200)
                             :confidence (:confidence final-result) :iterations (inc iteration)})
                        (rlm-stage! :iter-end iteration
                          {:blocks (count executions) :errors (count (filter :error executions))
                           :times (mapv :execution-time-ms executions)})
                        (when on-chunk
                          (on-chunk {:iteration iteration :thinking thinking
                                     :code (mapv :code executions)
                                     :final {:answer (:answer final-result) :confidence (:confidence final-result)
                                             :iterations (inc iteration) :status :success}
                                     :done? true}))
                        (merge (cond-> {:answer (:answer final-result) :trace (conj trace trace-entry)
                                        :iterations (inc iteration) :confidence (:confidence final-result)}
                                 (:sources final-result)   (assoc :sources (:sources final-result))
                                 (:reasoning final-result) (assoc :reasoning (:reasoning final-result)))
                          (finalize-cost)))

                      (if (empty? executions)
                        ;; ── Empty iteration ──
                        (do (rlm-stage! :empty iteration {})
                          (rlm-stage! :iter-end iteration {:blocks 0 :errors 0 :times []})
                          (recur (merge loop-state
                                   {:iteration (inc iteration) :trace (conj trace trace-entry)
                                    :prev-next-model next-model :prev-next-reasoning next-reasoning})))

                        ;; ── Normal iteration with executions ──
                        (do (rlm-stage! :iter-end iteration
                              {:blocks (count executions) :errors (count (filter :error executions))
                               :times (mapv :execution-time-ms executions)})
                          (when on-chunk
                            (on-chunk {:iteration iteration :thinking thinking
                                       :code (mapv :code executions) :done? false}))
                          (let [had-success? (some #(nil? (:error %)) executions)
                                next-errors (if had-success? 0 (inc consecutive-errors))
                                _ (when had-success? (swap! var-index-atom update :current-revision inc))]
                            (recur (merge loop-state
                                     {:iteration (inc iteration) :messages messages
                                      :trace (conj trace trace-entry) :consecutive-errors next-errors
                                      :prev-executions executions :prev-iteration iteration
                                      :prev-next-model next-model
                                      :prev-next-reasoning next-reasoning}))))))))))))))))

;; =============================================================================
;; run-query! — store → iteration-loop → persist
;; =============================================================================

(defn run-query!
  "Store query → iteration-loop → update query → return result.

   `env`       — RLM environment (validated against ::rt-shared/env).
   `query`     — the user's query text (non-blank string).
   `loop-opts` — map passed to iteration-loop (:max-iterations, :system-prompt,
                 :cancel-atom, :hooks, etc). :query-id is injected."
  [env query loop-opts]
  (rt-shared/validate! ::rt-shared/env env)
  (rt-shared/validate! ::rt-shared/non-blank-string query)
  (let [query-id (query-db/store! env query)
        result    (iteration-loop env query (assoc loop-opts :query-id query-id))
        _         (query-db/update! env query-id result)]
    (assoc result :query-id query-id)))

;; =============================================================================
