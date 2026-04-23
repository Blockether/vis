(ns com.blockether.vis.loop.storage.sqlite.conversations
  "Conversation persistence — soul/state pattern.

   conversation       (soul)  → stable identity (channel, external_id)
   conversation_state (state) → mutable props (system_prompt, model, title)
                                + fork chain via parent_state_id

   query → conversation_state (not the soul)
   iteration → query
   execution → iteration
   iteration_var (soul) → conversation
   iteration_var_state  → iteration_var + iteration"
  (:require
   [clojure.string :as str]
   [com.blockether.vis.loop.storage.sqlite.core :as core]
   [honey.sql :as sql]
   [next.jdbc :as jdbc]
   [taoensso.trove :as trove])
  (:import
   (java.util UUID)))

;; =============================================================================
;; Conversation soul
;; =============================================================================

(defn store-conversation!
  "Create a new conversation soul + initial state (version 1).
   Returns [:id conversation-uuid].

   Required: `:channel` (:vis, :telegram, :cli).
   Optional: `:external-id`, `:system-prompt`, `:model`, `:title`."
  [db-info {:keys [channel external-id title system-prompt model]}]
  (when (core/ds db-info)
    (let [conv-id  (UUID/randomUUID)
          state-id (UUID/randomUUID)
          now      (core/now-ms)]
      ;; Soul
      (core/execute! db-info
        {:insert-into :conversation
         :values [{:id          (str conv-id)
                   :channel     (core/->kw (or channel :vis))
                   :external_id external-id
                   :title       title
                   :created_at  now}]})
      ;; Initial state (version 1)
      (core/execute! db-info
        {:insert-into :conversation_state
         :values [{:id              (str state-id)
                   :conversation_id (str conv-id)
                   :system_prompt   (or system-prompt "")
                   :model           (or model "")
                   :title           title
                   :version         1
                   :created_at      now}]})
      [:id conv-id])))

(defn db-get-conversation
  "Returns the conversation soul + latest state merged, or nil."
  [db-info conversation-id]
  (when (and (core/ds db-info) (vector? conversation-id))
    (let [id (core/->id (second conversation-id))]
      (when-let [row (core/query-one! db-info
                       {:select [:c.id :c.channel :c.external_id :c.title :c.created_at
                                 [:s.id :state_id] :s.system_prompt :s.model [:s.title :state_title]
                                 :s.version :s.parent_state_id]
                        :from [[:conversation :c]]
                        :join [[:conversation_state :s] [:= :s.conversation_id :c.id]]
                        :where [:and
                                [:= :c.id id]
                                [:= :s.version
                                 {:select [[[:max :version]]]
                                  :from :conversation_state
                                  :where [:= :conversation_id :c.id]}]]})]
        {:id            (core/->uuid (:id row))
         :type          :conversation
         :channel       (core/->kw-back (:channel row))
         :external-id   (:external_id row)
         :title         (or (:state_title row) (:title row))
         :system-prompt (:system_prompt row)
         :model         (:model row)
         :version       (:version row)
         :created-at    (core/->date (:created_at row))}))))

(defn db-find-latest-conversation-id
  "Returns lookup ref for the most recently created conversation, or nil."
  [db-info]
  (when (core/ds db-info)
    (when-let [row (core/query-one! db-info
                     {:select [:id]
                      :from :conversation
                      :order-by [[:created_at :desc] [:id :desc]]
                      :limit 1})]
      [:id (core/->uuid (:id row))])))

(defn db-resolve-conversation-id
  "Resolve a conversation selector to a lookup ref."
  [db-info selector]
  (cond
    (nil? selector) nil
    (= :latest selector) (db-find-latest-conversation-id db-info)
    (and (vector? selector) (= :id (first selector))) selector
    (uuid? selector) [:id selector]
    :else nil))

(defn db-list-conversations
  "Lists conversations for a channel (soul + latest state), most recent first."
  [db-info channel]
  (when (core/ds db-info)
    (mapv (fn [row]
            {:id          (core/->uuid (:id row))
             :channel     (core/->kw-back (:channel row))
             :external-id (:external_id row)
             :title       (or (:state_title row) (:title row))
             :version     (:version row)
             :created-at  (core/->date (:created_at row))})
      (core/query! db-info
        {:select [:c.id :c.channel :c.external_id :c.title :c.created_at [:s.title :state_title] :s.version]
         :from [[:conversation :c]]
         :join [[:conversation_state :s] [:= :s.conversation_id :c.id]]
         :where [:and
                 [:= :c.channel (core/->kw channel)]
                 [:= :s.version
                  {:select [[[:max :version]]]
                   :from [[:conversation_state :s2]]
                   :where [:= :s2.conversation_id :c.id]}]]
         :order-by [[:c.created_at :desc]]}))))

(defn db-find-conversation-by-external
  "Find a conversation by channel + external-id."
  [db-info channel external-id]
  (when (and (core/ds db-info) external-id)
    (when-let [row (core/query-one! db-info
                     {:select [:id]
                      :from :conversation
                      :where [:and
                              [:= :channel (core/->kw channel)]
                              [:= :external_id (str external-id)]]})]
      [:id (core/->uuid (:id row))])))

(defn db-update-conversation-title!
  "Update the title on the conversation soul."
  [db-info conversation-id title]
  (when (and (core/ds db-info) conversation-id)
    (core/execute! db-info
      {:update :conversation
       :set {:title title}
       :where [:= :id (core/->id (second conversation-id))]}))))

;; =============================================================================
;; Fork
;; =============================================================================

(defn fork-conversation!
  "Fork a conversation at a specific query. Creates a new conversation_state
   with parent_state_id pointing to the current state and fork_after_query_id
   marking the branch point.

   Returns [:state-id uuid] for the new state. New queries go under this state.
   Reading the full timeline = parent queries up to fork point + own queries."
  [db-info conversation-id {:keys [fork-after-query-id system-prompt model title]}]
  (when (core/ds db-info)
    (let [conv-id (core/->id (second conversation-id))
          ;; Find current (latest) state
          current (core/query-one! db-info
                    {:select [:id :version :system_prompt :model :title]
                     :from :conversation_state
                     :where [:and
                             [:= :conversation_id conv-id]
                             [:= :version
                              {:select [[[:max :version]]]
                               :from :conversation_state
                               :where [:= :conversation_id conv-id]}]]})
          new-id  (UUID/randomUUID)
          now     (core/now-ms)]
      (when current
        (core/execute! db-info
          {:insert-into :conversation_state
           :values [{:id                  (str new-id)
                     :conversation_id     conv-id
                     :parent_state_id     (:id current)
                     :fork_after_query_id (when fork-after-query-id
                                            (core/->id fork-after-query-id))
                     :system_prompt       (or system-prompt (:system_prompt current))
                     :model               (or model (:model current))
                     :title               (or title (:title current))
                     :version             (inc (:version current))
                     :created_at          now}]})
        [:state-id new-id]))))

;; =============================================================================
;; State resolution helpers
;; =============================================================================

(defn db-get-latest-state-id
  "Returns the latest conversation_state id (as string) for a conversation."
  [db-info conversation-id]
  (when (core/ds db-info)
    (let [conv-id (core/->id (second conversation-id))]
      (:id (core/query-one! db-info
             {:select [:id]
              :from :conversation_state
              :where [:and
                      [:= :conversation_id conv-id]
                      [:= :version
                       {:select [[[:max :version]]]
                        :from :conversation_state
                        :where [:= :conversation_id conv-id]}]]})))))

;; =============================================================================
;; Query
;; =============================================================================

(defn store-query!
  "Stores a query row linked to the latest conversation_state. Returns [:id uuid]."
  [db-info {:keys [parent-conversation-id
                   query messages answer iterations duration-ms status eval-score]}]
  (when (core/ds db-info)
    (let [id       (UUID/randomUUID)
          now      (core/now-ms)
          state-id (db-get-latest-state-id db-info parent-conversation-id)
          q        (or query "")]
      (core/execute! db-info
        {:insert-into :query
         :values [(cond-> {:id                    (str id)
                           :conversation_state_id state-id
                           :name                  (subs q 0 (min (count q) 100))
                           :text                  q
                           :answer                (or (when answer (pr-str answer)) "")
                           :iterations            (or iterations 0)
                           :duration_ms           (or duration-ms 0)
                           :status                (core/->kw (or status :unknown))
                           :created_at            now
                           :updated_at            now}
                    messages   (assoc :messages (pr-str messages))
                    eval-score (assoc :eval_score (float eval-score)))]})
      [:id id])))

(defn update-query!
  "Updates a query row with final outcome."
  [db-info query-id {:keys [answer iterations duration-ms status eval-score tokens cost]}]
  (when (and (core/ds db-info) query-id)
    (let [id  (core/->id (second query-id))
          now (core/now-ms)]
      (core/execute! db-info
        {:update :query
         :set (cond-> {:answer      (or (when answer (pr-str answer)) "")
                       :iterations  (or iterations 0)
                       :duration_ms (or duration-ms 0)
                       :status      (core/->kw (or status :unknown))
                       :updated_at  now}
                eval-score          (assoc :eval_score (float eval-score))
                (:input tokens)     (assoc :input_tokens     (long (:input tokens)))
                (:output tokens)    (assoc :output_tokens    (long (:output tokens)))
                (:reasoning tokens) (assoc :reasoning_tokens (long (:reasoning tokens)))
                (:cached tokens)    (assoc :cached_tokens    (long (:cached tokens)))
                (:total-cost cost)  (assoc :total_cost       (double (:total-cost cost)))
                (:model cost)       (assoc :model            (str (:model cost))))
         :where [:= :id id]}))))

;; =============================================================================
;; SCI var serialization helpers
;; =============================================================================

(defn- sci-var?
  [v]
  (and (some? v)
    (= "sci.lang.Var" (.getName (class v)))))

(declare edn-safe)

(defn- sci-var->surrogate [v depth]
  (let [s (str v)
        bare-name (-> s (subs 2) (str/replace #"^[^/]+/" ""))
        bound (try (.getRawRoot v) (catch Throwable _ nil))]
    {:rlm/var-id bare-name
     :rlm/var-value (edn-safe bound (dec depth))}))

(defn- edn-safe
  ([v] (edn-safe v 6))
  ([v depth]
   (cond
     (zero? depth) v
     (sci-var? v) (sci-var->surrogate v depth)
     (map? v)    (persistent! (reduce-kv (fn [m k val] (assoc! m k (edn-safe val (dec depth)))) (transient {}) v))
     (vector? v) (mapv #(edn-safe % (dec depth)) v)
     (set? v)    (into #{} (map #(edn-safe % (dec depth))) v)
     (seq? v)    (doall (map #(edn-safe % (dec depth)) v))
     :else v)))

;; =============================================================================
;; Iteration + Execution + Vars
;; =============================================================================

(defn store-iteration!
  "Stores iteration + execution rows + var soul upserts + var state appends.
   Returns [:id iteration-uuid]."
  [db-info {:keys [query-id executions thinking answer duration-ms vars error]}]
  (when (core/ds db-info)
    (let [iter-id   (UUID/randomUUID)
          iter-id-s (str iter-id)
          now       (core/now-ms)
          query-id-s (when query-id (core/->id (second query-id)))
          ;; Resolve conversation_id via query → state → conversation
          conv-id   (when query-id-s
                      (:conversation_id
                        (core/query-one! db-info
                          {:select [:cs.conversation_id]
                           :from [[:query :q]]
                           :join [[:conversation_state :cs] [:= :q.conversation_state_id :cs.id]]
                           :where [:= :q.id query-id-s]})))]
      ;; 1. Iteration
      (core/execute! db-info
        {:insert-into :iteration
         :values [(cond-> {:id          iter-id-s
                           :query_id    query-id-s
                           :thinking    (or thinking "")
                           :duration_ms (or duration-ms 0)
                           :created_at  now
                           :updated_at  now}
                    answer (assoc :answer answer)
                    error  (assoc :error (pr-str error)))]})
      ;; 2. Executions
      (let [blank? (fn [s] (or (nil? s) (and (string? s) (str/blank? s))))]
        (doseq [[pos exec] (map-indexed vector (or executions []))]
          (core/execute! db-info
            {:insert-into :execution
             :values [(cond-> {:id           (str (UUID/randomUUID))
                               :iteration_id iter-id-s
                               :position     pos
                               :code         (:code exec)
                               :created_at   now}
                        (some? (:result exec))            (assoc :result (pr-str (edn-safe (:result exec))))
                        (some? (:error exec))             (assoc :error (str (:error exec)))
                        (not (blank? (:stdout exec)))     (assoc :stdout (:stdout exec))
                        (not (blank? (:stderr exec)))     (assoc :stderr (:stderr exec))
                        (some? (:execution-time-ms exec)) (assoc :duration_ms (:execution-time-ms exec))
                        (true? (:timeout? exec))          (assoc :timeout 1)
                        (true? (:repaired? exec))         (assoc :repaired 1))]})))
      ;; 3. Var souls + states
      (when conv-id
        (doseq [{:keys [name value code time-ms metadata]} (or vars [])]
          (when name
            (let [name-s    (str name)
                  soul-id-s (str (UUID/randomUUID))
                  _         (jdbc/execute! (core/ds db-info)
                              (sql/format
                                {:insert-into :iteration_var
                                 :values [{:id              soul-id-s
                                           :conversation_id conv-id
                                           :name            name-s
                                           :created_at      now}]
                                 :on-conflict [:conversation_id :name]
                                 :do-nothing true}))
                  actual-soul (:id (core/query-one! db-info
                                     {:select [:id]
                                      :from :iteration_var
                                      :where [:and
                                              [:= :conversation_id conv-id]
                                              [:= :name name-s]]}))
                  max-ver   (or (:v (core/query-one! db-info
                                      {:select [[[:max :version] :v]]
                                       :from :iteration_var_state
                                       :where [:= :iteration_var_id actual-soul]}))
                              0)
                  rich-code (pr-str (cond-> {}
                                      code     (assoc :expr code)
                                      time-ms  (assoc :time-ms time-ms)
                                      metadata (assoc :metadata metadata)))]
              (core/execute! db-info
                {:insert-into :iteration_var_state
                 :values [{:id               (str (UUID/randomUUID))
                           :iteration_var_id actual-soul
                           :iteration_id     iter-id-s
                           :version          (inc max-ver)
                           :value            (pr-str (edn-safe value))
                           :code             rich-code
                           :created_at       now}]})))))
      [:id iter-id])))

;; =============================================================================
;; Read helpers
;; =============================================================================

(defn db-list-executions
  "Lists execution rows for an iteration, ordered by position."
  [db-info iteration-id]
  (if (and (core/ds db-info) iteration-id)
    (let [iter-id (core/->id (second iteration-id))]
      (mapv (fn [r]
              (cond-> {:id          (core/->uuid (:id r))
                       :position    (:position r)
                       :code        (:code r)
                       :created-at  (core/->date (:created_at r))}
                (some? (:result r))      (assoc :result (core/read-edn-safe (:result r) nil))
                (some? (:error r))       (assoc :error (:error r))
                (some? (:stdout r))      (assoc :stdout (:stdout r))
                (some? (:stderr r))      (assoc :stderr (:stderr r))
                (some? (:duration_ms r)) (assoc :duration-ms (:duration_ms r))
                (= 1 (:timeout r))       (assoc :timeout? true)
                (= 1 (:repaired r))      (assoc :repaired? true)))
        (core/query! db-info
          {:select [:*]
           :from :execution
           :where [:= :iteration_id iter-id]
           :order-by [[:position :asc]]})))
    []))

(defn db-list-iteration-vars
  "Lists var states produced by a specific iteration."
  [db-info iteration-id]
  (if (and (core/ds db-info) iteration-id)
    (let [iter-id (core/->id (second iteration-id))]
      (mapv (fn [r] {:name    (:name r)
                     :value   (core/read-edn-safe (:value r) nil)
                     :code    (:code r)
                     :version (:version r)})
        (core/query! db-info
          {:select [:v.name :s.value :s.code :s.version]
           :from [[:iteration_var_state :s]]
           :join [[:iteration_var :v] [:= :s.iteration_var_id :v.id]]
           :where [:= :s.iteration_id iter-id]
           :order-by [[:s.created_at :asc]]})))
    []))

(defn db-list-var-states
  "Full version history for a named var in a conversation."
  [db-info conversation-id var-name]
  (if (and (core/ds db-info) conversation-id)
    (let [conv-id (core/->id (second conversation-id))]
      (mapv (fn [r]
              {:version      (:version r)
               :value        (core/read-edn-safe (:value r) nil)
               :code         (:code r)
               :iteration-id (core/->uuid (:iteration_id r))
               :created-at   (core/->date (:created_at r))})
        (core/query! db-info
          {:select [:s.version :s.value :s.code :s.iteration_id :s.created_at]
           :from [[:iteration_var_state :s]]
           :join [[:iteration_var :v] [:= :s.iteration_var_id :v.id]]
           :where [:and
                   [:= :v.conversation_id conv-id]
                   [:= :v.name (str var-name)]]
           :order-by [[:s.version :asc]]})))
    []))

(defn db-latest-var-registry
  "Latest var registry for a conversation (max version per soul)."
  ([db-info conversation-id] (db-latest-var-registry db-info conversation-id {}))
  ([db-info conversation-id _opts]
   (if (and (core/ds db-info) conversation-id)
     (let [conv-id (core/->id (second conversation-id))]
       (into {}
         (map (fn [r]
                [(symbol (:name r))
                 {:value        (core/read-edn-safe (:value r) nil)
                  :code         (:code r)
                  :version      (:version r)
                  :iteration-id (core/->uuid (:iteration_id r))
                  :created-at   (core/->date (:created_at r))}]))
         (core/query! db-info
           {:select [:v.name :s.value :s.code :s.version :s.iteration_id :s.created_at]
            :from [[:iteration_var :v]]
            :join [[:iteration_var_state :s] [:= :s.iteration_var_id :v.id]]
            :where [:and
                    [:= :v.conversation_id conv-id]
                    [:= :s.version
                     {:select [[[:max :version]]]
                      :from :iteration_var_state
                      :where [:= :iteration_var_id :v.id]}]]})))
     {})))

;; =============================================================================
;; List children — queries and iterations
;; =============================================================================

(defn- row->query [row]
  (cond-> {:id                    (core/->uuid (:id row))
           :type                  :query
           :conversation-state-id (core/->uuid (:conversation_state_id row))
           :name                  (:name row)
           :text                  (:text row)
           :answer                (:answer row)
           :iterations            (:iterations row)
           :duration-ms           (:duration_ms row)
           :status                (core/->kw-back (:status row))
           :created-at            (core/->date (:created_at row))
           :updated-at            (core/->date (:updated_at row))}
    (some? (:messages row))         (assoc :messages (:messages row))
    (some? (:eval_score row))       (assoc :eval-score (float (:eval_score row)))
    (some? (:model row))            (assoc :model (:model row))
    (some? (:input_tokens row))     (assoc :input-tokens (:input_tokens row))
    (some? (:output_tokens row))    (assoc :output-tokens (:output_tokens row))
    (some? (:reasoning_tokens row)) (assoc :reasoning-tokens (:reasoning_tokens row))
    (some? (:cached_tokens row))    (assoc :cached-tokens (:cached_tokens row))
    (some? (:total_cost row))       (assoc :total-cost (:total_cost row))))

(defn- row->iteration [row]
  (cond-> {:id          (core/->uuid (:id row))
           :type        :iteration
           :query-id    (core/->uuid (:query_id row))
           :created-at  (core/->date (:created_at row))
           :updated-at  (core/->date (:updated_at row))}
    (some? (:answer row))      (assoc :answer (:answer row))
    (some? (:thinking row))    (assoc :thinking (:thinking row))
    (some? (:error row))       (assoc :error (:error row))
    (some? (:duration_ms row)) (assoc :duration-ms (:duration_ms row))))

(defn db-list-queries-by-status
  "Lists all query rows with a given status across all conversations.
   Used by orphan-sweep on startup."
  [db-info status]
  (if (core/ds db-info)
    (mapv row->query
      (core/query! db-info
        {:select [:*]
         :from :query
         :where [:= :status (core/->kw status)]}))
    []))

(defn db-list-conversation-queries
  "Lists queries for a conversation's latest state, ordered by created_at.
   Does NOT walk the fork chain — returns only queries directly on the
   latest state. Use `db-list-conversation-queries-full` for fork-aware timeline."
  [db-info conversation-id]
  (if (and (core/ds db-info) conversation-id)
    (let [state-id (db-get-latest-state-id db-info conversation-id)]
      (when state-id
        (mapv row->query
          (core/query! db-info
            {:select [:*]
             :from :query
             :where [:= :conversation_state_id state-id]
             :order-by [[:created_at :asc] [:id :asc]]}))))
    []))

(defn db-list-query-iterations
  "Lists iteration rows for a query ordered by created_at."
  [db-info query-id]
  (if (and (core/ds db-info) query-id)
    (let [q-id (core/->id (second query-id))]
      (mapv row->iteration
        (core/query! db-info
          {:select [:*]
           :from :iteration
           :where [:= :query_id q-id]
           :order-by [[:created_at :asc] [:id :asc]]})))
    []))
