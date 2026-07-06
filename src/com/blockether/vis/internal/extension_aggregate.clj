(ns com.blockether.vis.internal.extension-aggregate
  "Extension-owned durable sidecar API.

   Public ext-* helpers are for code running inside an extension callback.
   They never accept :extension-id from callers; the currently executing
   extension identity is supplied by com.blockether.vis.internal.extension.

   The db-* persistence facade remains the privileged/admin surface for
   inspecting rows across extensions."
  (:require [com.blockether.vis.internal.extension :as extension]
            [com.blockether.vis.internal.persistance :as persistance]))

(defn- safe-deref [a] (when a (try @a (catch Throwable _ nil))))

(defn- db-info!
  [env]
  (or (:db-info env)
      (throw (ex-info "Extension aggregate API requires :db-info in env"
                      {:type :extension-aggregate/no-db-info}))))

(defn- current-extension-id!
  []
  (or (extension/current-extension-id)
      (throw (ex-info "Extension aggregate API requires extension callback context"
                      {:type :extension-aggregate/no-extension-context}))))

(defn- reject-extension-id!
  [m]
  (when (or (contains? m :extension-id) (contains? m :extension_id))
    (throw (ex-info "extension-id is runtime-owned; extension callers must not supply it"
                    {:type :extension-aggregate/extension-id-forbidden
                     :keys (vec (filter #(contains? m %) [:extension-id :extension_id]))}))))

(defn- require-non-blank
  [kind v]
  (when (or (nil? v) (and (string? v) (empty? (.trim ^String v))))
    (throw (ex-info (str "Extension aggregate requires " (name kind))
                    {:type :extension-aggregate/missing-required :key kind})))
  v)

(defn- read-turn-state-field
  "Helper: pull a field from the single :turn-state-atom on env."
  [env k]
  (some-> (:turn-state-atom env)
          deref
          (get k)))

(defn- latest-turn-state-id
  [env]
  (let [db-info
        (db-info! env)

        turn-id
        (read-turn-state-field env :session-turn-id)]

    (when turn-id
      (some->> (persistance/db-list-session-turn-states db-info turn-id)
               (sort-by :version)
               last
               :id))))

(defn- current-session-state-id
  [env]
  (let [db-info (db-info! env)]
    (or (:session-state-id env)
        (safe-deref (:session-state-id-atom env))
        (when-let [session-id (:session-id env)]
          (persistance/db-latest-session-state-id db-info session-id)))))

(defn- require-scope-id
  [scope k v]
  (or v
      (throw (ex-info (str "Cannot resolve extension aggregate " (name scope) " scope")
                      {:type :extension-aggregate/scope-unavailable :scope scope :missing k}))))

(defn- explicit-scope
  [scope]
  (cond-> {}
    (:session-soul-id scope)
    (assoc :session-soul-id (:session-soul-id scope))

    (:session-id scope)
    (assoc :session-soul-id (:session-id scope))

    (:session-state-id scope)
    (assoc :session-state-id (:session-state-id scope))

    (:session-turn-state-id scope)
    (assoc :session-turn-state-id (:session-turn-state-id scope))

    (:turn-state-id scope)
    (assoc :session-turn-state-id (:turn-state-id scope))

    (:iteration-id scope)
    (assoc :iteration-id (:iteration-id scope))

    (:iteration-form-index scope)
    (assoc :iteration-form-index (:iteration-form-index scope))

    (:form-index scope)
    (assoc :iteration-form-index (:form-index scope))

    (:iteration-block-id scope)
    (assoc :iteration-block-id (:iteration-block-id scope))

    (:block-id scope)
    (assoc :iteration-block-id (:block-id scope))))

(defn- resolve-scope
  [env scope]
  (cond (nil? scope) {}
        (= :global scope) {}
        (map? scope) (explicit-scope scope)
        (= :session scope) {:session-soul-id (require-scope-id scope :session-id (:session-id env))}
        (= :session-state scope)
        {:session-soul-id (:session-id env)
         :session-state-id
         (require-scope-id scope :session-state-id (current-session-state-id env))}
        (= :turn-state scope) {:session-soul-id (:session-id env)
                               :session-state-id (current-session-state-id env)
                               :session-turn-state-id (require-scope-id scope
                                                                        :session-turn-state-id
                                                                        (latest-turn-state-id env))}
        (= :iteration scope)
        {:session-soul-id (:session-id env)
         :session-state-id (current-session-state-id env)
         :session-turn-state-id (latest-turn-state-id env)
         :iteration-id
         (require-scope-id scope :iteration-id (read-turn-state-field env :iteration-id))}
        (= :block scope)
        (let [iteration-id
              (require-scope-id scope :iteration-id (read-turn-state-field env :iteration-id))

              form-index
              (read-turn-state-field env :form-idx)]

          {:session-soul-id (:session-id env)
           :session-state-id (current-session-state-id env)
           :session-turn-state-id (latest-turn-state-id env)
           :iteration-id iteration-id
           :iteration-form-index (require-scope-id scope :iteration-form-index form-index)})
        :else (throw (ex-info "Unknown extension aggregate scope"
                              {:type :extension-aggregate/unknown-scope :scope scope}))))

(defn- normalize-row
  [env row]
  (when-not (map? row)
    (throw (ex-info "Extension aggregate row must be a map"
                    {:type :extension-aggregate/invalid-row :got (type row)})))
  (reject-extension-id! row)
  (merge (dissoc row :key :scope)
         {:extension-id (current-extension-id!)
          :aggregate-key (require-non-blank :key (or (:aggregate-key row) (:key row)))
          :kind (require-non-blank :kind (:kind row))}
         (resolve-scope env (or (:scope row) :global))))

(defn- normalize-query
  [env query]
  (when-not (map? query)
    (throw (ex-info "Extension aggregate query must be a map"
                    {:type :extension-aggregate/invalid-query :got (type query)})))
  (reject-extension-id! query)
  (cond-> (merge (dissoc query :key :scope)
                 {:extension-id (current-extension-id!)}
                 (resolve-scope env (:scope query)))
    (or (:aggregate-key query) (:key query))
    (assoc :aggregate-key (or (:aggregate-key query) (:key query)))))

(defn extension-aggregate-create!
  "Append one extension-owned aggregate row. Returns the decoded row.
   Extension id is always filled from current extension callback context."
  [env row]
  (persistance/db-create-extension-aggregate! (db-info! env) (normalize-row env row)))

(defn extension-aggregate-put!
  "Upsert one singleton extension-owned aggregate row for key/kind/scope.
   Returns the decoded row."
  [env row]
  (persistance/db-put-extension-aggregate! (db-info! env) (normalize-row env row)))

(defn extension-aggregate-get
  "Return one extension-owned aggregate row by query, or nil. Defaults to the
   latest row when the query is not unique."
  [env query]
  (persistance/db-get-extension-aggregate (db-info! env) (normalize-query env query)))

(defn extension-list-aggregates
  "List extension-owned aggregate rows. The current extension id is always
   applied; normal extension code cannot list another extension's rows."
  [env query]
  (persistance/db-list-extension-aggregates (db-info! env) (normalize-query env query)))

(defn extension-delete-aggregate!
  "Delete extension-owned aggregate rows matching query. Cross-extension delete
   is impossible through this API because extension id is runtime-filled."
  [env query]
  (persistance/db-delete-extension-aggregates! (db-info! env) (normalize-query env query)))

(defn extension-update-aggregate!
  "Atomic singleton update. Reads the current content for query, applies f, and
   writes the returned value as :content. Query must include :key and :kind."
  [env query f & args]
  (let [row (normalize-query env
                             (cond-> query
                               (not (contains? query :scope))
                               (assoc :scope :global)))]
    (when-not (:aggregate-key row)
      (throw (ex-info "extension-update-aggregate! requires :key"
                      {:type :extension-aggregate/missing-required :key :key})))
    (when-not (:kind row)
      (throw (ex-info "extension-update-aggregate! requires :kind"
                      {:type :extension-aggregate/missing-required :key :kind})))
    (persistance/db-swap-extension-aggregate! (db-info! env) row f args)))
