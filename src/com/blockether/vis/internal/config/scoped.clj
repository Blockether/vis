(ns com.blockether.vis.internal.config.scoped
  "Sparse settings overlays shared by every gateway client.

   Resolution is per key: global, canonical project, organizational group, session.
   Missing is inheritance, false is a value. Project writes use the existing local
   YAML overlay; group/session writes belong to their durable entities and cascade
   on deletion. Moving a session changes ancestors, not its overrides. New sessions
   and forks start without overrides. Provider administration and client preferences
   are not session settings. Resource availability is checked at invocation time;
   an already running external call is not cancelled by a settings edit."
  (:require [clojure.string :as str]
            [com.blockether.vis.contract.toggle :as contract]
            [com.blockether.vis.internal.config.core :as config]
            [com.blockether.vis.internal.config.toggles :as toggles]
            [com.blockether.vis.internal.persistance.core :as store]
            [com.blockether.vis.internal.util :as util]
            [com.blockether.vis.internal.workspace.core :as workspace]
            [taoensso.telemere :as tel]))

(defonce ^:private listeners (atom #{}))

(defn add-listener!
  "Listen for a successful scoped write. Returns a detach function."
  [f]
  (swap! listeners conj f)
  #(swap! listeners disj f))

(defn- notify-listeners!
  [event]
  (doseq [f @listeners]
    (try (f event)
         (catch Exception _
           (tel/log! {:level :warn :id ::refresh-failed}
                     "Settings saved; a cached environment could not refresh")))))

(defn target
  "Validate a read/write target; omitted scope means global, never current session."
  [db scope target-id]
  (let [scope
        (or scope "global")

        id
        (some-> target-id
                str
                str/trim
                not-empty)

        entity
        (case scope
          "global"
          nil

          "project"
          (when id
            (or (store/db-get-project db id)
                (store/db-get-project-by-root db nil (workspace/normalize-root id))))

          "group"
          (when id (store/db-get-session-group db id))

          "session"
          (when id (store/db-get-session db id))

          (throw (ex-info "Unknown settings scope" {:status 400 :type :invalid-scope})))

        _
        (when (and (= scope "global") id)
          (throw (ex-info "Global settings do not take a target id" {:status 400})))

        _
        (when (and (not= scope "global") (nil? entity))
          (throw (ex-info "Settings target not found" {:status (if id 404 400)})))

        pinned-workspace
        (when (and (= scope "session") (nil? (:project-id entity)))
          (some->> (store/db-latest-session-state-id db id)
                   (store/db-workspace-for-session db)))

        pinned-root
        (some-> (or (:repo-root pinned-workspace) (:root pinned-workspace))
                workspace/normalize-root)

        project
        (cond (= scope "project") entity
              (:project-id entity) (store/db-get-project db (:project-id entity))
              pinned-root (store/db-get-project-by-root db nil pinned-root))

        group-id
        (case scope
          "session"
          (:group-id entity)

          "group"
          id

          nil)]

    {:scope scope
     :target-id (if (= scope "project")
                  (some-> (:id project)
                          str)
                  id)
     :project-id (some-> (:id project)
                         str)
     :group-id (some-> group-id
                       str)
     :root (or (:workspace-root project) pinned-root)
     :label (or (:title entity) (:name entity) "Global")}))

(defn resolve-layers
  "Resolve each declared key independently and retain its last explicit source.
   Layers are ordered least-specific first. Illegal scope declarations in YAML
   do not gain power by bypassing an HTTP writer."
  [specs layers scope overrides]
  (mapv (fn [{:keys [id default scopes] :as spec}]
          (let [allowed
                (set scopes)

                resolved
                (reduce (fn [result layer]
                          (if (and (allowed (:scope layer)) (contains? (:values layer) id))
                            {:value (get (:values layer) id) :source (:scope layer)}
                            result))
                        {:value default :source "default"}
                        layers)]

            (merge spec resolved {:scope scope :is-override (contains? overrides id)})))
        specs))

(defn- raw-toggles
  [raw]
  (into {}
        (keep (fn [[id value]]
                (when-let [chosen (toggles/wire-value id value)]
                  [id (:value chosen)])))
        (get raw "toggles")))

(defn layers
  "Read ancestors afresh, including memberships and project configuration."
  [db {:keys [scope target-id root group-id]}]
  (let [global
        (merge (raw-toggles (config/load-global-yaml-config-raw))
               (raw-toggles (config/load-global-config-raw)))

        project
        (when root
          (binding [workspace/*workspace-root* root]
            (raw-toggles (config/load-project-tiers-raw))))]

    (cond-> [{:scope "global" :values global}]
      root
      (conj {:scope "project" :values project})

      group-id
      (conj {:scope "group" :values (store/db-scoped-settings db "group" group-id)})

      (= scope "session")
      (conj {:scope "session" :values (store/db-scoped-settings db "session" target-id)}))))

(defn- own-values
  [db {:keys [scope target-id root]}]
  (case scope
    "global"
    (raw-toggles (config/load-global-config-raw))

    "project"
    (binding [workspace/*workspace-root* root]
      (raw-toggles (config/load-project-config-raw)))

    (store/db-scoped-settings db scope target-id)))

(defn- definition-prefix [section] (str "config:" (str/join "/" section) ":"))

(defn own-definitions
  "Read only definitions written at this target, not inherited entries."
  [db {:keys [scope target-id root]} section]
  (case scope
    "global"
    (get-in (config/load-global-config-raw) section {})

    "project"
    (binding [workspace/*workspace-root* root]
      (get-in (config/load-project-config-raw) section {}))

    (let [prefix (definition-prefix section)]
      (into {}
            (keep (fn [[k v]]
                    (when (str/starts-with? k prefix) [(subs k (count prefix)) v])))
            (store/db-scoped-settings db scope target-id)))))

(defn definitions
  "Resolve named definitions atomically per name, never merging credentials across scopes."
  [db {:keys [scope root group-id] :as target} section]
  (let [global
        (merge (get-in (config/load-global-yaml-config-raw) section)
               (get-in (config/load-global-config-raw) section))

        project
        (when root
          (binding [workspace/*workspace-root* root]
            (get-in (config/load-project-tiers-raw) section)))

        layers
        (cond-> [["global" global]]
          root
          (conj ["project" project])

          group-id
          (conj ["group" (own-definitions db {:scope "group" :target-id group-id} section)])

          (= scope "session")
          (conj ["session" (own-definitions db target section)]))

        own
        (own-definitions db target section)]

    (->> layers
         (reduce (fn [result [source entries]]
                   (reduce-kv (fn [result name value]
                                (assoc result
                                  name {:name name
                                        :value value
                                        :source source
                                        :is-override (contains? own name)}))
                              result
                              (or entries {})))
                 {})
         vals
         (sort-by :name)
         vec)))

(defn set-definition!
  "Persist one validated named definition; nil removes only this target's override."
  [db {:keys [scope target-id root] :as target} section name value]
  (let [edit (fn [raw]
               (if (nil? value)
                 (update-in raw section dissoc name)
                 (assoc-in raw (conj section name) value)))]
    (case scope
      "global"
      (config/update-machine-config! edit)

      "project"
      (do (when-not root (throw (ex-info "Project has no workspace root" {:status 409})))
          (binding [workspace/*workspace-root* root]
            (config/update-project-config! edit)))

      (store/db-set-scoped-setting! db
                                    scope
                                    target-id
                                    (str (definition-prefix section) name)
                                    value))
    (notify-listeners! (assoc target
                         :section section
                         :name name))
    nil))

(defn inherited-layers
  "Resolve ancestors without this target's writable overlay; authored YAML remains."
  [db {:keys [scope root] :as target}]
  (case scope
    "global"
    [{:scope "global" :values (raw-toggles (config/load-global-yaml-config-raw))}]

    "project"
    (conj (vec (filter #(= "global" (:scope %)) (layers db target)))
          {:scope "project"
           :values (when root
                     (binding [workspace/*workspace-root* root]
                       (raw-toggles (config/load-project-root-config-raw))))})

    (vec (remove #(= scope (:scope %)) (layers db target)))))

(defn inherited-definitions
  "Named ancestor values after removing only the selected writable overlay."
  [db {:keys [scope root] :as target} section]
  (let [ancestors (case scope
                    "global"
                    [{:scope "global"
                      :values (get-in (config/load-global-yaml-config-raw) section)}]

                    "project"
                    [{:scope "global"
                      :values (merge (get-in (config/load-global-yaml-config-raw) section)
                                     (get-in (config/load-global-config-raw) section))}
                     {:scope "project"
                      :values (when root
                                (binding [workspace/*workspace-root* root]
                                  (get-in (config/load-project-root-config-raw) section)))}]

                    (map (fn [{:keys [name value source]}]
                           {:scope source :values {name value}})
                         (definitions db
                                      (if (= scope "session")
                                        (assoc target
                                          :scope (if (:group-id target) "group" "project")
                                          :target-id (:group-id target))
                                        (assoc target
                                          :scope "project"
                                          :group-id nil))
                                      section)))]
    (reduce (fn [result {:keys [scope values]}]
              (reduce-kv #(assoc %1 %2 {:value %3 :source scope}) result (or values {})))
            {}
            ancestors)))

(defn settings
  "Effective, own and inherited values, with provenance and eligible scopes."
  [db target]
  (let [specs
        (toggles/registered-toggles)

        own
        (own-values db target)

        inherited
        (into {}
              (map (juxt :id identity))
              (resolve-layers specs (inherited-layers db target) (:scope target) {}))]

    (mapv (fn [{:keys [id] :as row}]
            (assoc row
              :own-value (get own id)
              :inherited-value (get-in inherited [id :value])
              :inherited-source (get-in inherited [id :source])))
          (resolve-layers specs (layers db target) (:scope target) own))))

(defn- specificity
  "Position in resolution order: later scopes win; `default` precedes them all."
  ^long [scope]
  (.indexOf ^java.util.List contract/scopes scope))

(defn mark-overridden
  "Mark each row read at `owner` that a more specific scope decides for session
   `session-id`, naming that scope and the value the session uses. A blank or
   unknown session, or one outside the owner, such as another project's, marks nothing."
  [db owner rows session-id]
  (let [context
        (when-let [id (some-> session-id
                              str
                              str/trim
                              not-empty)]
          (try (target db "session" id) (catch clojure.lang.ExceptionInfo _ nil)))

        related?
        (when context
          (case (:scope owner)
            "global"
            true

            "project"
            (= (:project-id owner) (:project-id context))

            "group"
            (= (:target-id owner) (:group-id context))

            false))

        winners
        (when related? (into {} (map (juxt :id identity)) (settings db context)))

        rank
        (specificity (:scope owner))]

    (mapv (fn [{:keys [id] :as row}]
            (let [{:keys [source value]} (get winners id)]
              (cond-> row
                (and source (< rank (specificity source)))
                (assoc :overridden-by {:scope source :value value}))))
          rows)))

(defn values
  "The session snapshot used by callbacks; does not mutate the global registry."
  [db session-id]
  (into {} (map (juxt :id :value)) (settings db (target db "session" session-id))))

(defn resource-id
  "Stable collision-resistant toggle id for a named skill, MCP server or extension."
  [kind resource-name]
  (str (name kind) "_" (util/sha256-hex resource-name)))

(defn register-resource!
  "Expose availability in the same scoped catalog as ordinary settings."
  [kind resource-name]
  (let [id (resource-id kind resource-name)]
    (when-not (toggles/toggle-spec id)
      (toggles/register-toggle! {:id id
                                 :label resource-name
                                 :default true
                                 :group kind
                                 :scopes contract/scopes
                                 :persist? true}))
    id))

(defn engine-setting!
  "Register Auto/On/Off only for optional tool extensions, never infrastructure."
  [ext]
  (when (and (seq (get-in ext [:ext/engine :ext.engine/symbols]))
             (not (get-in ext [:ext/engine :ext.engine/builtin?]))
             (empty? (:ext/providers ext))
             (empty? (:ext/channels ext)))
    (let [id (resource-id :engines (:ext/name ext))]
      (when-not (toggles/toggle-spec id)
        (toggles/register-toggle! {:id id
                                   :label (:ext/name ext)
                                   :type :enum
                                   :choices ["auto" "on" "off"]
                                   :default "auto"
                                   :description
                                   "Auto detects applicability; On stays active; Off denies tools."
                                   :group :engines
                                   :scopes contract/scopes
                                   :persist? true
                                   :owner (:ext/name ext)}))
      id)))

(defn live-values
  "The session values [[engine-mode]] and [[resource-enabled?]] read for `env`,
   resolved once so a caller checking many extensions, skills or servers pays
   for one resolution. Pass it as a `delay`: only a check that needs a session
   value forces it. Nil outside a session, where both read the process-wide
   settings."
  [env]
  (when (and (:db-info env) (:session-id env)) (values (:db-info env) (:session-id env))))

(defn- live-value
  "`id`'s live value from `live`, a map or a delay of one, resolved afresh when
   `live` predates the id's registration."
  [env live id]
  (let [live (force live)]
    (if (contains? live id)
      (get live id)
      (if-let [fresh (live-values env)]
        (get fresh id)
        (toggles/value-of id)))))

(defn engine-mode
  "Live Auto/On/Off mode, independent of the response snapshot. Pass a delay of
   [[live-values]] when checking several extensions."
  ([env ext] (engine-mode env ext nil))
  ([env ext live]
   (if-let [id (engine-setting! ext)]
     (or (live-value env live id) "auto")
     "auto")))

(defn resource-enabled?
  "Live gate: saved names and cached handles cannot bypass a scoped disable.
   Pass a delay of [[live-values]] when checking several resources."
  ([env kind resource-name] (resource-enabled? env kind resource-name nil))
  ([env kind resource-name live]
   (not (false? (live-value env live (register-resource! kind resource-name))))))

(defn set-setting!
  "Write one eligible key, or inherit by deleting it. Validate before any write."
  [db target id action value]
  (let [_
        (when-not (contract/toggle-id? id)
          (throw (ex-info "Setting id must be lower-case snake_case" {:status 400 :id id})))

        spec
        (or (toggles/toggle-spec id) (throw (ex-info "Unknown setting" {:status 404 :id id})))

        scope
        (:scope target)

        _
        (when-not (some #{scope} (:scopes spec))
          (throw (ex-info "This setting is not available in that scope"
                          {:status 400 :type :invalid-setting-scope :id id :scope scope})))

        current
        (:value (first (filter #(= id (:id %)) (settings db target))))

        selected
        (case action
          "inherit"
          nil

          "toggle"
          (when (= :boolean (:type spec)) {:value (not current)})

          "cycle"
          (when (= :enum (:type spec))
            (let [choices
                  (:choices spec)

                  index
                  (.indexOf ^java.util.List choices current)]

              {:value (nth choices (mod (inc index) (count choices)))}))

          "value"
          (toggles/wire-value id value)

          nil)

        _
        (when (and (not= action "inherit") (nil? selected))
          (throw (ex-info "Invalid setting action or value" {:status 400 :id id})))

        v
        (:value selected)

        edit
        (fn [raw]
          (if (= action "inherit")
            (update raw "toggles" dissoc id)
            (assoc-in raw ["toggles" id] v)))]

    (case scope
      "global"
      (do (config/update-machine-config! edit)
          (binding [toggles/*overrides*
                    nil

                    toggles/*persist-writes*
                    false]

            (toggles/set-value! id (:value (first (filter #(= id (:id %)) (settings db target)))))))

      "project"
      (do (when-not (:root target) (throw (ex-info "Project has no workspace root" {:status 409})))
          (binding [workspace/*workspace-root* (:root target)]
            (config/update-project-config! edit)))

      (store/db-set-scoped-setting! db scope (:target-id target) id v))
    (notify-listeners! (assoc target :id id))
    (first (filter #(= id (:id %)) (settings db target)))))

(defn edit-settings!
  "Edit one owner atomically. `edit` validates inside the storage lock/transaction.
   It receives the current store and returns section/name/value edits; nil inherits."
  [db {:keys [scope target-id root] :as target} edit]
  (let [apply-raw
        (fn [current-db raw]
          (reduce (fn [result {:keys [section name value]}]
                    (if (nil? value)
                      (if (seq section) (update-in result section dissoc name) (dissoc result name))
                      (assoc-in result (conj section name) value)))
                  raw
                  (edit current-db)))

        changed
        (case scope
          "global"
          (config/update-machine-config! #(apply-raw db %))

          "project"
          (do (when-not root (throw (ex-info "Project has no workspace root" {:status 409})))
              (binding [workspace/*workspace-root* root]
                (config/update-project-config! #(apply-raw db %))))

          (store/db-edit-scoped-settings!
            db
            scope
            target-id
            (fn [tx raw]
              (reduce (fn [result {:keys [section name value]}]
                        (let [id (if (= section ["toggles"])
                                   name
                                   (str (definition-prefix section) name))]
                          (if (nil? value) (dissoc result id) (assoc result id value))))
                      raw
                      (edit tx)))))]

    (when (= scope "global")
      (binding [toggles/*overrides*
                nil

                toggles/*persist-writes*
                false]

        (doseq [{:keys [id value]} (settings db target)]
          (toggles/set-value! id value))))
    (notify-listeners! target)
    changed))
