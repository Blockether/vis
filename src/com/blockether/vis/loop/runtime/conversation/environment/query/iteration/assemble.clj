(ns com.blockether.vis.loop.runtime.conversation.environment.query.iteration.assemble
  "Message and context assembly.

   Builds the LLM message vector for each iteration: initial messages,
   iteration context (journal + var-index + nudges + prior thinking),
   and cross-query handover."
  (:require
   [clojure.string :as str]
   [com.blockether.vis.loop.runtime.conversation.environment.query.iteration.shared :as helpers]
   [com.blockether.vis.loop.runtime.conversation.environment.query.iteration.format :as fmt]
   [com.blockether.vis.loop.nudges :as nudges]
   [com.blockether.vis.loop.runtime.conversation.environment.base :as rlm-base
    :refer [build-var-index]]
   [com.blockether.vis.loop.storage.db :as rlm-db]
   [taoensso.trove :as trove]))

;; ---------------------------------------------------------------------------
;; Constants
;; ---------------------------------------------------------------------------

(def ^:const PRIOR_THINKING_MAX_CHARS 4000)

(def PRIOR_THINKING_BREADCRUMB
  "[older reasonings] call `(var-history '*reasoning*)` from :code (oldest first; `take-last N` for a window).")

;; ---------------------------------------------------------------------------
;; Initial message assembly
;; ---------------------------------------------------------------------------

(defn assemble-initial-messages
  "Build the LLM message vector for iteration 0 of a query."
  [{:keys [system-prompt initial-user-content history-messages]}]
  (into [{:role "system" :content system-prompt}
         {:role "user"   :content initial-user-content}]
    (or history-messages [])))

(defn trim-to-initial-history
  "Strip error-feedback / empty-iteration nudges, keeping exactly the first
   `initial-count` messages."
  [messages initial-count]
  (vec (take initial-count messages)))

;; ---------------------------------------------------------------------------
;; Var-index and var-count readers (env-scoped)
;; ---------------------------------------------------------------------------

(defn read-var-index-str
  "Render (or serve cached render of) `<var_index>` for the given env."
  [rlm-env]
  (let [var-index-atom (or (:var-index-atom rlm-env)
                         (atom {:index nil :revision -1 :current-revision 0}))
        {:keys [index revision current-revision]} @var-index-atom]
    (if (= revision current-revision)
      index
      (let [sandbox-map (get-in @(:env (:sci-ctx rlm-env))
                          [:namespaces 'sandbox])

            tool-reg    (some-> rlm-env :tool-registry-atom deref)
            idx         (build-var-index
                          (:sci-ctx rlm-env) (:initial-ns-keys rlm-env)
                          sandbox-map
                          (:db-info rlm-env) (:conversation-id rlm-env)
                          {:include-persisted? true
                           :tool-registry      tool-reg})
            live-rev    (:current-revision @var-index-atom)]
        (swap! var-index-atom assoc :index idx :revision live-rev)
        idx))))

;; ---------------------------------------------------------------------------
;; Prior thinking
;; ---------------------------------------------------------------------------

(defn- load-prior-thinking-chain
  [db-info query-id]
  (try
    (if query-id
      (let [iters (rlm-db/db-list-query-iterations db-info query-id)]
        (vec (keep-indexed
               (fn [idx it]
                 (when-let [t (:thinking it)]
                   {:iteration idx :thinking t}))
               iters)))
      [])
    (catch Throwable t
      (trove/log! {:level :warn
                   :data {:error (ex-message t) :query-id query-id}
                   :msg "load-prior-thinking-chain failed"})
      [])))

(defn build-prior-thinking
  "Render <prior_thinking> body: ONLY the most recent iteration's reasoning
   plus a breadcrumb pointing at `var-history`."
  [_rlm-env db-info query-id]
  (let [chain (load-prior-thinking-chain db-info query-id)]
    (when (seq chain)
      (let [tail (vec (take-last 1 chain))
            body (fmt/format-prior-thinking-chain tail)]
        (if body
          (str body "\n" PRIOR_THINKING_BREADCRUMB)
          PRIOR_THINKING_BREADCRUMB)))))

;; ---------------------------------------------------------------------------
;; Cross-query handover
;; ---------------------------------------------------------------------------

(defn build-cross-query-handover
  "Compose the `[prior turn]` block for iteration 0 of a new query."
  [db-info conversation-id current-query-id parent-iteration-id]
  (when (and db-info conversation-id (nil? parent-iteration-id))
    (try
      (let [all-queries (sort-by :created-at
                          (rlm-db/db-list-conversation-queries db-info conversation-id))
            current-id  (second current-query-id)
            prior       (last (remove #(= (:id %) current-id) all-queries))]
        (when prior
          (let [prior-id    [:id (:id prior)]
                iters        (rlm-db/db-list-query-iterations db-info prior-id)
                tagged-iters (vec (keep-indexed
                                    (fn [idx it]
                                      (when-let [t (:thinking it)]
                                        {:iteration idx :thinking t}))
                                    iters))
                final-answer (:answer prior)]
            (fmt/format-prior-turn-handover
              {:iterations tagged-iters
               :final-answer (when (and (string? final-answer)
                                     (not (str/blank? final-answer)))
                               final-answer)}))))
      (catch Throwable t
        (trove/log! {:level :warn
                     :data {:error (ex-message t) :conversation-id conversation-id}
                     :msg "build-cross-query-handover failed"})
        nil))))

;; ---------------------------------------------------------------------------
;; build-iteration-context (the per-iteration user message)
;; ---------------------------------------------------------------------------

(defn build-iteration-context
  "Assemble the trailing user message appended to the base prompt on
   every iteration beyond the first."
  [rlm-env {:keys [iteration current-max-iterations
                   prior-thinking
                   prev-executions prev-iteration
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
        exec-results (fmt/format-execution-results prev-executions prev-iteration)
        nudges-block (str/join "\n"
                       (keep identity
                         [(when (and iteration current-max-iterations)
                            (nudges/budget-warning
                              {:iteration              iteration
                               :current-max-iterations current-max-iterations}))
                          (nudges/repetition-warning call-counts-atom prev-executions)]))
        parts (keep (fn [p] (when (and (string? p) (not (str/blank? p))) p))
                [iter-header prior-block exec-results var-block nudges-block])]
    (when (seq parts)
      (str/join "\n" parts))))
