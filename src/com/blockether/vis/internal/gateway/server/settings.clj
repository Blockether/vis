(ns com.blockether.vis.internal.gateway.server.settings
  "Settings routes, including the Improve register and its settings."
  (:require [clojure.string :as str]
            [com.blockether.vis.contract.toggle :as toggle-contract]
            [com.blockether.vis.internal.config.scoped :as scoped]
            [com.blockether.vis.internal.sandbox.scoped-policy :as scoped-policy]
            [com.blockether.vis.internal.foundation.harness.discovery :as harness]
            [com.blockether.vis.internal.workspace.core :as workspace]
            [com.blockether.vis.internal.loop :as lp]
            [com.blockether.vis.internal.config.core :as config]
            [com.blockether.vis.internal.config.toggles :as toggles]
            [com.blockether.vis.internal.gateway.server.http :as http]
            [com.blockether.vis.internal.gateway.state :as state]))

(defn- toggle-json
  [{:keys [id label description type choices value experimental? scopes source scope is-override]}]
  (cond-> {:id id
           :label label
           :type (name type)
           :scopes scopes
           :scope scope
           :source source
           :is-override is-override
           :is-experimental (boolean experimental?)}
    description
    (assoc :description description)

    (= type :boolean)
    (assoc :enabled (boolean value))

    (not= type :boolean)
    (assoc :value value)

    choices
    (assoc :choices choices)))

(defn- resource-inventory
  [target]
  (binding [workspace/*workspace-root* (:root target)]
    (let [skills (cond->> (harness/all-skills)
                   (= "global" (:scope target))
                   (remove :project-root))
          servers (scoped/definitions (lp/db-info) target ["mcp" "servers"])]

      (set (concat (map #(scoped/register-resource! :skills (:name %)) skills)
                   (map #(scoped/register-resource! :mcp (:name %)) servers))))))

(defn- request-target
  [request body]
  (let [params (merge (:query-params request) body)]
    (scoped/target (lp/db-info) (get params "scope") (get params "target_id"))))

(defn- settings-response
  [f]
  (try (f)
       (catch clojure.lang.ExceptionInfo e
         (if-let [status (:status (ex-data e))]
           (http/error-response status (or (:type (ex-data e)) :invalid-setting) (ex-message e))
           (throw e)))))

(defn- agent-name-setting
  []
  {:id "agent_name"
   :label "Agent name"
   :description "Shared by all clients of this gateway. Overrides project names."
   :type "string"
   :value (config/agent-name)
   :max-length 80})

(defn- set-agent-name-setting
  [action given]
  (if (not= action "value")
    (http/error-response 400 :invalid-setting-action "Agent name takes the value action.")
    (try (state/set-agent-name! (:raw given))
         (http/json-response (agent-name-setting))
         (catch clojure.lang.ExceptionInfo e
           (if (= :config/invalid-agent-name (:type (ex-data e)))
             (http/error-response 400 :invalid-setting-value (ex-message e) :id "agent_name")
             (throw e))))))

(defn- list-settings-handler
  "GET /v1/settings?scope=...&target_id=...; all clients share this catalog."
  [request]
  (settings-response
    (fn []
      (let [target
            (request-target request nil)

            local?
            (not= "global" (:scope target))

            resources
            (resource-inventory target)

            channel
            (some-> (get-in request [:query-params "channel"])
                    keyword)

            rows
            (filter #(and (some #{(:scope target)} (:scopes %))
                          (or (not (#{:skills :mcp} (:group %))) (resources (:id %)))
                          (or local?
                              (and (not (false? (:settings? %))) (toggles/toggle-visible? %)))
                          (or (nil? channel)
                              (#{:all :*} channel)
                              (toggles/toggle-for-channel? channel %)))
                    (scoped/settings (lp/db-info) target))

            grouped
            (sort-by (comp str key) (group-by #(or (:group %) :other) rows))]

        (http/json-response
          {:scope (:scope target)
           :target-id (:target-id target)
           :label (:label target)
           :groups (into (cond-> [{:id "access"
                                   :title "Paths and access"
                                   :toggles (scoped-policy/settings (lp/db-info) target)}]
                           (not local?)
                           (conj {:id "agent"
                                  :title "Agent"
                                  :toggles [(assoc (agent-name-setting) :scopes ["global"])]}))
                         (map (fn [[group specs]]
                                {:id (name group)
                                 :title (if (and local? (= group :provider))
                                          "Response"
                                          (str/capitalize (str/replace (name group) #"[-_]+" " ")))
                                 :toggles (mapv toggle-json specs)}))
                         grouped)})))))

(defn- get-setting-handler
  "Read one setting, including response controls hidden in the global dialog."
  [request]
  (settings-response
    (fn []
      (let [target
            (request-target request nil)

            id
            (get-in request [:path-params :id])

            resources
            (resource-inventory target)

            spec
            (first (filter #(= id (:id %)) (scoped/settings (lp/db-info) target)))]

        (cond
          (not (toggle-contract/toggle-id? id))
          (http/error-response 400 :invalid-setting-id "Setting id must be lower-case snake_case")
          (scoped-policy/setting? id)
          (http/json-response (first (filter #(= id (:id %))
                                             (scoped-policy/settings (lp/db-info) target))))
          (and (= id "agent_name") (= "global" (:scope target))) (http/json-response
                                                                   (agent-name-setting))
          (and spec
               (some #{(:scope target)} (:scopes spec))
               (or (not (#{:skills :mcp} (:group spec))) (resources id)))
          (http/json-response (toggle-json spec))
          :else (http/error-response 404 :unknown-setting "No setting in this scope" :id id))))))

(defn- set-setting-handler
  "Set one key or remove its override with action=inherit. false is explicit."
  [request]
  (settings-response
    (fn []
      (let [body
            (merge (:query-params request) (http/body-json request))

            target
            (request-target request body)

            resources
            (resource-inventory target)

            id
            (get body "id")

            action
            (get body "action" "toggle")]

        (cond (and (#{:skills :mcp} (:group (toggles/toggle-spec id))) (not (resources id)))
              (http/error-response 404 :unknown-setting "Resource is not available in this target")
              (scoped-policy/setting? id)
              (http/json-response
                (scoped-policy/set-setting! (lp/db-info) target id action (get body "value")))
              (= id "agent_name")
              (if (= "global" (:scope target))
                (set-agent-name-setting action {:raw (get body "value")})
                (http/error-response 400 :invalid-setting-scope "Agent name is global"))
              :else
              (http/json-response
                (toggle-json
                  (scoped/set-setting! (lp/db-info) target id action (get body "value")))))))))

(defn- improve-handler
  [operation]
  (fn [request]
    (try (let [raw
               (if (contains? #{:create :update :save-settings} operation)
                 (try (http/body-json request) (catch Exception _ nil))
                 (or (:query-params request) {}))

               _
               (when-not (map? raw)
                 (throw (ex-info "Expected an Improve JSON object"
                                 {:status 400 :type :improve/invalid})))

               opts
               (into {}
                     (map (fn [[k v]]
                            [(keyword k)
                             (cond (and (= operation :list) (#{"after" "limit"} k) (string? v))
                                   (Long/parseLong v)
                                   (and (= k "project_id") (= v "")) nil
                                   :else v)]))
                     raw)

               opts
               (cond-> opts
                 (#{:get :update} operation)
                 (assoc :id (Long/parseLong (get-in request [:path-params :id]))))]

           (http/json-response (state/improve-operation! operation opts)))
         (catch NumberFormatException _
           (http/error-response 400 :improve/invalid "Improve ids and cursors must be integers"))
         (catch clojure.lang.ExceptionInfo e
           (if-let [status (:status (ex-data e))]
             (http/error-response status (:type (ex-data e)) (ex-message e))
             (throw e))))))

(def handlers
  "Handlers for this namespace's routes, keyed by the gateway contract's `[method path]`."
  {[:get "/v1/settings"] list-settings-handler
   [:post "/v1/settings"] set-setting-handler
   [:get "/v1/improve"] (improve-handler :list)
   [:post "/v1/improve"] (improve-handler :create)
   [:get "/v1/improve/settings"] (improve-handler :settings)
   [:patch "/v1/improve/settings"] (improve-handler :save-settings)
   [:post "/v1/improve/review"] (improve-handler :review)
   [:get "/v1/improve/:id"] (improve-handler :get)
   [:patch "/v1/improve/:id"] (improve-handler :update)
   [:get "/v1/settings/:id"] get-setting-handler})
