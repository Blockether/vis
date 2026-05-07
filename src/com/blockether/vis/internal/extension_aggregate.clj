(ns com.blockether.vis.internal.extension-aggregate
  "Extension-owned durable sidecar API.

   Public ext-* helpers are for code running inside an extension callback.
   They never accept :extension-id from callers; the currently executing
   extension identity is supplied by com.blockether.vis.internal.extension.

   The db-* persistence facade remains the privileged/admin surface for
   inspecting rows across extensions."
  (:require
   [com.blockether.vis.internal.extension :as extension]
   [com.blockether.vis.internal.persistance :as persistance]))

(defn- safe-deref [a]
  (when a
    (try
      @a
      (catch Throwable _ nil))))

(defn- db-info! [env]
  (or (:db-info env)
    (throw (ex-info "Extension aggregate API requires :db-info in env"
             {:type :extension-aggregate/no-db-info}))))

(defn- current-extension-id! []
  (or (extension/current-extension-id)
    (throw (ex-info "Extension aggregate API requires extension callback context"
             {:type :extension-aggregate/no-extension-context}))))

(defn- reject-extension-id! [m]
  (when (or (contains? m :extension-id)
          (contains? m :extension_id))
    (throw (ex-info "extension-id is runtime-owned; extension callers must not supply it"
             {:type :extension-aggregate/extension-id-forbidden
              :keys (vec (filter #(contains? m %) [:extension-id :extension_id]))}))))

(defn- require-non-blank [kind v]
  (when (or (nil? v) (and (string? v) (empty? (.trim ^String v))))
    (throw (ex-info (str "Extension aggregate requires " (name kind))
             {:type :extension-aggregate/missing-required
              :key kind})))
  v)

(defn- latest-turn-state-id [env]
  (let [db-info (db-info! env)
        turn-id (safe-deref (:current-conversation-turn-id-atom env))]
    (when turn-id
      (some->> (persistance/db-list-conversation-turn-states db-info turn-id)
        (sort-by :version)
        last
        :id))))

(defn- current-conversation-state-id [env]
  (let [db-info (db-info! env)]
    (or (:conversation-state-id env)
      (safe-deref (:conversation-state-id-atom env))
      (when-let [conversation-id (:conversation-id env)]
        (persistance/db-latest-conversation-state-id db-info conversation-id)))))

(defn- require-scope-id [scope k v]
  (or v
    (throw (ex-info (str "Cannot resolve extension aggregate " (name scope) " scope")
             {:type :extension-aggregate/scope-unavailable
              :scope scope
              :missing k}))))

(defn- explicit-scope [scope]
  (cond-> {}
    (:conversation-soul-id scope)
    (assoc :conversation-soul-id (:conversation-soul-id scope))

    (:conversation-id scope)
    (assoc :conversation-soul-id (:conversation-id scope))

    (:conversation-state-id scope)
    (assoc :conversation-state-id (:conversation-state-id scope))

    (:conversation-turn-state-id scope)
    (assoc :conversation-turn-state-id (:conversation-turn-state-id scope))

    (:turn-state-id scope)
    (assoc :conversation-turn-state-id (:turn-state-id scope))

    (:iteration-id scope)
    (assoc :iteration-id (:iteration-id scope))

    (:iteration-block-index scope)
    (assoc :iteration-block-index (:iteration-block-index scope))

    (:block-index scope)
    (assoc :iteration-block-index (:block-index scope))

    (:iteration-block-id scope)
    (assoc :iteration-block-id (:iteration-block-id scope))

    (:block-id scope)
    (assoc :iteration-block-id (:block-id scope))))

(defn- resolve-scope [env scope]
  (cond
    (nil? scope)
    {}

    (= :global scope)
    {:scope-key "global"}

    (map? scope)
    (explicit-scope scope)

    (= :conversation scope)
    {:conversation-soul-id (require-scope-id scope :conversation-id (:conversation-id env))}

    (= :conversation-state scope)
    {:conversation-soul-id  (:conversation-id env)
     :conversation-state-id (require-scope-id scope :conversation-state-id
                              (current-conversation-state-id env))}

    (= :turn-state scope)
    {:conversation-soul-id       (:conversation-id env)
     :conversation-state-id      (current-conversation-state-id env)
     :conversation-turn-state-id (require-scope-id scope :conversation-turn-state-id
                                   (latest-turn-state-id env))}

    (= :iteration scope)
    {:conversation-soul-id       (:conversation-id env)
     :conversation-state-id      (current-conversation-state-id env)
     :conversation-turn-state-id (latest-turn-state-id env)
     :iteration-id               (require-scope-id scope :iteration-id
                                   (safe-deref (:current-iteration-id-atom env)))}

    (= :block scope)
    (let [iteration-id (require-scope-id scope :iteration-id
                         (safe-deref (:current-iteration-id-atom env)))
          block-index  (safe-deref (:current-form-idx-atom env))]
      {:conversation-soul-id       (:conversation-id env)
       :conversation-state-id      (current-conversation-state-id env)
       :conversation-turn-state-id (latest-turn-state-id env)
       :iteration-id               iteration-id
       :iteration-block-index      (require-scope-id scope :iteration-block-index block-index)})

    :else
    (throw (ex-info "Unknown extension aggregate scope"
             {:type :extension-aggregate/unknown-scope
              :scope scope}))))

(defn- normalize-row [env row]
  (when-not (map? row)
    (throw (ex-info "Extension aggregate row must be a map"
             {:type :extension-aggregate/invalid-row
              :got (type row)})))
  (reject-extension-id! row)
  (merge (dissoc row :key :scope)
    {:extension-id  (current-extension-id!)
     :aggregate-key (require-non-blank :key (or (:aggregate-key row) (:key row)))
     :kind          (require-non-blank :kind (:kind row))}
    (resolve-scope env (or (:scope row) :global))))

(defn- normalize-query [env query]
  (when-not (map? query)
    (throw (ex-info "Extension aggregate query must be a map"
             {:type :extension-aggregate/invalid-query
              :got (type query)})))
  (reject-extension-id! query)
  (cond-> (merge (dissoc query :key :scope)
            {:extension-id (current-extension-id!)}
            (resolve-scope env (:scope query)))
    (or (:aggregate-key query) (:key query))
    (assoc :aggregate-key (or (:aggregate-key query) (:key query)))))

(defn ext-create!
  "Append one extension-owned aggregate row. Returns the decoded row.
   Extension id is always filled from current extension callback context."
  [env row]
  (persistance/db-create-extension-aggregate! (db-info! env) (normalize-row env row)))

(defn ext-put!
  "Upsert one singleton extension-owned aggregate row for key/kind/scope.
   Returns the decoded row. Requires schema-level scope_key support."
  [env row]
  (persistance/db-put-extension-aggregate! (db-info! env) (normalize-row env row)))

(defn ext-get
  "Return one extension-owned aggregate row by query, or nil. Defaults to the
   latest row when the query is not unique."
  [env query]
  (persistance/db-get-extension-aggregate (db-info! env) (normalize-query env query)))

(defn ext-list
  "List extension-owned aggregate rows. The current extension id is always
   applied; normal extension code cannot list another extension's rows."
  [env query]
  (persistance/db-list-extension-aggregates (db-info! env) (normalize-query env query)))

(defn ext-delete!
  "Delete extension-owned aggregate rows matching query. Cross-extension delete
   is impossible through this API because extension id is runtime-filled."
  [env query]
  (persistance/db-delete-extension-aggregates! (db-info! env) (normalize-query env query)))

(defn ext-swap!
  "Atomic singleton update. Reads the current content for query, applies f, and
   writes the returned value as :content. Query must include :key and :kind."
  [env query f & args]
  (let [row (normalize-query env (cond-> query (not (contains? query :scope)) (assoc :scope :global)))]
    (when-not (:aggregate-key row)
      (throw (ex-info "ext-swap! requires :key"
               {:type :extension-aggregate/missing-required
                :key :key})))
    (when-not (:kind row)
      (throw (ex-info "ext-swap! requires :kind"
               {:type :extension-aggregate/missing-required
                :key :kind})))
    (persistance/db-swap-extension-aggregate! (db-info! env) row f args)))
