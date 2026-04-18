(ns com.blockether.vis.loop.storage.sqlite.conversations
  "Conversation / query / iteration / iteration-var entity tree.

   One :conversation entity owns many :query children (one per user turn);
   each :query owns many :iteration children; each :iteration owns many
   :iteration-var children."
  (:require
   [com.blockether.vis.loop.storage.sqlite.core :as core]
   [taoensso.trove :as trove]))

;; =============================================================================
;; Conversation
;; =============================================================================

(defn store-conversation!
  "Create a new :conversation entity and return its lookup ref. Callers that
   want to resume an existing conversation should pass `[:id uuid]` to
   `db-resolve-conversation-ref` directly instead of calling this."
  [db-info {:keys [system-prompt model]}]
  (when (core/ds db-info)
    (core/store-entity! db-info
      {:type          :conversation
       :system-prompt (or system-prompt "")
       :model         (or model "")})))

(defn db-get-conversation
  "Returns a conversation entity by lookup ref or nil."
  [db-info conversation-ref]
  (when (and (core/ds db-info) (vector? conversation-ref))
    (core/fetch-entity db-info (core/entity-ref->id conversation-ref))))

(defn db-find-latest-conversation-ref
  "Returns lookup ref for the most recently created conversation, or nil."
  [db-info]
  (when (core/ds db-info)
    (when-let [row (core/query-one! db-info
                     {:select [:id]
                      :from :entity
                      :where [:= :type "conversation"]
                      :order-by [[:created_at :desc] [:id :desc]]
                      :limit 1})]
      (core/id->entity-ref (:id row)))))

(defn db-resolve-conversation-ref
  "Resolve a conversation selector to a lookup ref. Accepts:
     nil              → nil (caller should then create a new conversation)
     :latest          → the most recent :conversation entity
     [:id uuid]       → returned unchanged
     uuid             → wrapped as [:id uuid]"
  [db-info selector]
  (cond
    (nil? selector) nil
    (= :latest selector) (db-find-latest-conversation-ref db-info)
    (and (vector? selector) (= :id (first selector))) selector
    (uuid? selector) [:id selector]
    :else nil))

;; =============================================================================
;; Query
;; =============================================================================

(defn store-query!
  "Stores a query entity linked to a conversation via parent-id."
  [db-info {:keys [conversation-ref text messages answer iterations duration-ms status eval-score]}]
  (let [parent-id (when conversation-ref (second conversation-ref))]
    (core/store-entity! db-info
      (cond-> {:type :query
               :name (let [t (or text "")]
                       (subs t 0 (min (count t) 100)))
               :parent-id parent-id
               :text (or text "")
               :answer (or (when answer (pr-str answer)) "")
               :iterations (or iterations 0)
               :duration-ms (or duration-ms 0)
               :status (or status :unknown)}
        messages (assoc :messages (pr-str messages))
        eval-score (assoc :eval-score (float eval-score))))))

(defn update-query!
  "Updates a query entity with final outcome, including optional cost/token
   metadata so the UI can reconstruct meta lines after a restart."
  [db-info query-ref {:keys [answer iterations duration-ms status eval-score tokens cost]}]
  (core/update-entity! db-info query-ref
    (cond-> {:answer (or (when answer (pr-str answer)) "")
             :iterations (or iterations 0)
             :duration-ms (or duration-ms 0)
             :status (or status :unknown)}
      eval-score             (assoc :eval-score (float eval-score))
      (:input tokens)        (assoc :input-tokens     (long (:input tokens)))
      (:output tokens)       (assoc :output-tokens    (long (:output tokens)))
      (:reasoning tokens)    (assoc :reasoning-tokens (long (:reasoning tokens)))
      (:cached tokens)       (assoc :cached-tokens    (long (:cached tokens)))
      (:total-cost cost)     (assoc :total-cost       (double (:total-cost cost)))
      (:model cost)          (assoc :model            (str (:model cost))))))

;; =============================================================================
;; Iteration + iteration-vars
;; =============================================================================

(defn store-iteration!
  "Stores an iteration entity linked to a query via parent-id, plus child
   iteration-var entities for any restorable vars."
  [db-info {:keys [query-ref executions thinking answer duration-ms vars error]}]
  (let [parent-id (when query-ref (second query-ref))
        executions (or executions [])
        code-strs (mapv :code executions)
        result-strs (mapv #(try (pr-str (:result %))
                             (catch Exception e
                               (trove/log! {:level :warn :data {:error (ex-message e)}
                                            :msg "Failed to serialize execution result"})
                               "???"))
                      executions)
        iter-ref (core/store-entity! db-info
                   (cond-> {:type :iteration
                            :parent-id parent-id
                            :code (pr-str code-strs)
                            :results (pr-str result-strs)
                            :thinking (or thinking "")
                            :duration-ms (or duration-ms 0)}
                     answer (assoc :answer answer)
                     error  (assoc :error (pr-str error))))]
    (doseq [{:keys [name value code]} (or vars [])]
      (when name
        (core/store-entity! db-info
          {:type :iteration-var
           :name (str name)
           :parent-id (second iter-ref)
           :value (pr-str value)
           :code (or code "")})))
    iter-ref))

(defn db-list-iteration-vars
  "Lists persisted restorable vars for an iteration. Returns plain {:name :value :code} maps,
   matching the db.clj contract."
  [db-info iteration-ref]
  (if (and (core/ds db-info) iteration-ref)
    (let [iter-id (core/entity-ref->id iteration-ref)
          rows (core/query! db-info
                 {:select [:e.created_at :v.name :v.value :v.code]
                  :from [[:entity :e]]
                  :join [[:iteration_var_attrs :v] [:= :e.id :v.entity_id]]
                  :where [:and [:= :e.type "iteration-var"]
                          [:= :e.parent_id iter-id]]
                  :order-by [[:e.created_at :asc] [:e.id :asc]]})]
      (mapv (fn [r] {:name (:name r)
                     :value (core/read-edn-safe (:value r) nil)
                     :code  (:code r)}) rows))
    []))

(defn db-list-conversation-queries
  "Lists query entities for a conversation ordered by created-at."
  [db-info conversation-ref]
  (if (and (core/ds db-info) conversation-ref)
    (let [conv-id (core/entity-ref->id conversation-ref)
          ids (mapv :id (core/query! db-info
                          {:select [:id]
                           :from :entity
                           :where [:and [:= :type "query"]
                                   [:= :parent_id conv-id]]
                           :order-by [[:created_at :asc] [:id :asc]]}))]
      (core/fetch-entities db-info ids))
    []))

(defn db-list-query-iterations
  "Lists iteration entities for a query ordered by created-at."
  [db-info query-ref]
  (if (and (core/ds db-info) query-ref)
    (let [q-id (core/entity-ref->id query-ref)
          ids (mapv :id (core/query! db-info
                          {:select [:id]
                           :from :entity
                           :where [:and [:= :type "iteration"]
                                   [:= :parent_id q-id]]
                           :order-by [[:created_at :asc] [:id :asc]]}))]
      (core/fetch-entities db-info ids))
    []))
