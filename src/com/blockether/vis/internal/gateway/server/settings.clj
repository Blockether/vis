(ns com.blockether.vis.internal.gateway.server.settings
  "Settings routes, including the Improve register and its settings."
  (:require [clojure.string :as str]
            [com.blockether.vis.contract.toggle :as toggle-contract]
            [com.blockether.vis.internal.config.core :as config]
            [com.blockether.vis.internal.config.toggles :as toggles]
            [com.blockether.vis.internal.gateway.server.http :as http]
            [com.blockether.vis.internal.gateway.state :as state]))

(defn- toggle-json
  "One settings row as JSON — the wire twin of the server-side
   `toggle-row` hiccup: boolean rows carry `enabled`, enum rows carry
   `value` + `choices`."
  [{:keys [id label description type experimental?]}]
  (let [choices
        (try (toggles/choices-of id) (catch Throwable _ nil))

        value
        (try (toggles/value-of id) (catch Throwable _ nil))

        pretty
        (fn [v]
          (if (keyword? v) (name v) (str v)))

        base
        {:id id
         :label (str (or label id))
         :is-experimental (boolean experimental?)
         :type (name (or type (if (seq choices) :enum :boolean)))}]

    (cond-> base
      description
      (assoc :description (str description))

      (seq choices)
      (assoc :value
        (pretty value) :choices
        (mapv pretty choices))

      (empty? choices)
      (assoc :enabled (boolean (try (toggles/enabled? id) (catch Throwable _ false)))))))

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
  "GET /v1/settings[?channel=web|all] — the gateway identity and feature toggles
   rendered by every channel (web dialog, TUI pane, mobile app) as grouped JSON.
   `channel` scopes rows exactly like `toggles-for-channel`; `all` (or
   `*`, or omitting the param) ships every visible toggle regardless of
   channel — the cross-channel view a remote companion wants."
  [request]
  (let [raw
        (get-in request [:query-params "channel"])

        channel
        (when (and raw (not (contains? #{"all" "*"} (str/lower-case raw)))) (keyword raw))

        specs
        (if channel (toggles/toggles-for-channel channel) (toggles/visible-toggles))

        grouped
        (sort-by (comp str key) (group-by #(or (:group %) :other) specs))]

    (http/json-response
      {:groups (into [{:id "agent" :title "Agent" :toggles [(agent-name-setting)]}]
                     (map (fn [[group group-specs]]
                            {:id (name group)
                             :title (str/capitalize (str/replace (name group) #"[-_]+" " "))
                             :toggles (mapv toggle-json group-specs)}))
                     grouped)})))

(defn- get-setting-handler
  "GET /v1/settings/:id — the agent name or ONE registered toggle row, INCLUDING the ids
   `list-settings-handler` hides. `reasoning_level` is registered
   `:settings? false` because every channel drives it from its own dedicated
   control (TUI Ctrl+R, the companion's model dialog) rather than the Settings
   list, so a remote channel still needs a way to READ its current value.
   Same row shape as the list endpoint."
  [request]
  (let [id-str
        (get-in request [:path-params :id])

        id
        (when (string? id-str) (str/trim id-str))

        spec
        (when (seq id) (toggles/toggle-spec id))]

    (cond (not (toggle-contract/toggle-id? id))
          (http/error-response 400 :bad-setting-id "settings id must be a snake_case string")
          (= id "agent_name") (http/json-response (agent-name-setting))
          (nil? spec) (http/error-response 404 :unknown-setting "no such setting" :id (str id-str))
          :else (http/json-response (toggle-json spec)))))

(defn- set-setting-handler
  "POST /v1/settings {id, action} — flip (`toggle`, the default), `cycle` an
   enum, or set an exact value (`value` action with `{value}`) on one registered
   toggle or the agent name; answers with the refreshed row. JSON body or query params both work.

   A `value` the setting's own type cannot name is a 400, never a silent 200:
   booleans take true/false (on/off, yes/no, 1/0), enums take a choice name, and
   agent_name takes a nonblank string of at most 80 characters without control characters."
  [request]
  (let [body
        (try (http/body-json request) (catch Throwable _ nil))

        id-str
        (or (get body "id") (get-in request [:query-params "id"]))

        action
        (str (or (get body "action") (get-in request [:query-params "action"]) "toggle"))

        ;; A JSON `false` is a LEGAL value, so PRESENCE decides. An `or` here read
        ;; `{"value": false}` as "no value given" and answered 200 to a request
        ;; that changed nothing — the client believed the setting was off.
        given
        (cond (contains? body "value") {:raw (get body "value")}
              (contains? (:query-params request) "value") {:raw (get-in request
                                                                        [:query-params "value"])})

        id
        (when (string? id-str) (str/trim id-str))

        spec
        (when (seq id) (toggles/toggle-spec id))]

    (cond
      (not (toggle-contract/toggle-id? id))
      (http/error-response 400 :bad-setting-id "settings id must be a snake_case string")
      (= id "agent_name") (set-agent-name-setting action given)
      (nil? spec) (http/error-response 404 :unknown-setting "no such setting" :id (str id-str))
      (= action "value")
      (if-let [chosen (when given (toggles/wire-value id (:raw given)))]
        (do (toggles/set-value! id (:value chosen))
            (http/json-response (toggle-json (toggles/toggle-spec id))))
        (http/error-response
          400
          :invalid-setting-value
          "value must match the setting's type: true/false for a boolean, one of its choices for an enum"
          :id id))
      (and (= action "cycle") (not= :enum (toggles/type-of id)))
      (http/error-response 400
                           :invalid-setting-action
                           "cycle advances an enum; a boolean setting takes toggle or value"
                           :id id)
      :else (do (if (= action "cycle")
                  (toggles/cycle-value! id)
                  (toggles/set-enabled! id (not (toggles/enabled? id))))
                (http/json-response (toggle-json (toggles/toggle-spec id)))))))

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
