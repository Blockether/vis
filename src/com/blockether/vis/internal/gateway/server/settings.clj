(ns com.blockether.vis.internal.gateway.server.settings
  "Settings routes, including the Improve register and its settings."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.contract.toggle :as toggle-contract]
            [com.blockether.vis.contract.document :as document]
            [com.blockether.vis.internal.gateway.server.settings-edit :as settings-edit]
            [com.blockether.vis.internal.config.scoped :as scoped]
            [com.blockether.vis.internal.sandbox.scoped-policy :as scoped-policy]
            [com.blockether.vis.internal.foundation.harness.discovery :as harness]
            [com.blockether.vis.internal.workspace.core :as workspace]
            [com.blockether.vis.internal.loop :as lp]
            [com.blockether.vis.internal.config.core :as config]
            [com.blockether.vis.internal.config.toggles :as toggles]
            [com.blockether.vis.internal.council.rooms :as rooms]
            [com.blockether.vis.internal.gateway.server.http :as http]
            [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.internal.paths :as paths]
            [com.blockether.vis.internal.python.extensions :as python-extensions]
            [com.blockether.vis.internal.gateway.state :as state]))

(defn- toggle-json
  [{:keys [id label description type choices value experimental? scopes source scope is-override
           overridden-by inherited-value inherited-source own-value group parent inheritance]}]
  (cond-> {:id id
           :label label
           :type (name type)
           :scopes scopes
           :scope scope
           :source source
           :is-override is-override
           :is-experimental (boolean experimental?)
           :editor (if (= type :boolean) "switch" "select")
           :own-value own-value
           :inherited-value inherited-value
           :inherited-source inherited-source
           :applies
           (if (or (#{:skills :mcp :engines} group) (rooms/setting? id)) "next_call" "next_turn")}
    parent
    (assoc :parent parent)

    (= "restrict" inheritance)
    (assoc :inheritance inheritance)

    (= "council_room" id)
    (assoc :choice-labels
      (into {"local" "Local Council"} (map (juxt :room_id :name)) (rooms/known-rooms)))

    description
    (assoc :description description)

    (= type :boolean)
    (assoc :enabled (boolean value))

    (not= type :boolean)
    (assoc :value value)

    choices
    (assoc :choices choices)

    overridden-by
    (assoc :overridden-by
      (if (= type :boolean)
        {:scope (:scope overridden-by) :enabled (boolean (:value overridden-by))}
        {:scope (:scope overridden-by) :value (:value overridden-by)}))))

(defn- nest-rows
  "Put each row in `:children` of the row that its `:parent` names, at any depth.
   A row stays at the top when its parent is not in `rows` or when parents form a cycle.
   So every row shows once. Rows keep their order."
  [rows]
  (let [ids
        (set (map :id rows))

        by-parent
        (group-by #(when (ids (:parent %)) (:parent %)) rows)

        build
        (fn build [seen row]
          (let [kids
                (remove #(seen (:id %)) (get by-parent (:id row)))

                [children seen]
                (reduce (fn [[acc seen] kid]
                          (let [[tree seen] (build seen kid)]
                            [(conj acc tree) seen]))
                        [[] (into seen (map :id) kids)]
                        kids)]

            [(cond-> row
               (seq children)
               (assoc :children children)) seen]))]

    (loop [pending
           (concat (get by-parent nil) rows)

           seen
           #{}

           out
           []]

      (if-let [[row & more] (seq pending)]
        (if (seen (:id row))
          (recur more seen out)
          (let [[tree seen] (build (conj seen (:id row)) row)]
            (recur more seen (conj out tree))))
        out))))

(defn- resource-inventory
  [target]
  (binding [workspace/*workspace-root* (:root target)]
    (let [skills (cond->> (filter harness/own-setting? (harness/all-skills))
                   (= "global" (:scope target))
                   (remove :project-root))
          servers (scoped/definitions (lp/db-info) target ["mcp" "servers"])]

      (set (concat (map #(scoped/register-resource! :skills (:name %)) skills)
                   (map #(scoped/register-resource! :mcp (:name %)) servers))))))

(def ^:private sections
  "The sections of settings, in order: id, title and the toggle groups that each gathers.
   Owners register toggles into groups. Clients add their own controls, such as provider
   accounts or MCP servers, to the section with the same id."
  [[:general "General" [:council :experimental]] [:providers "Providers" [:provider]]
   [:voice "Voice" [:voice]] [:permissions "Permissions" [:sandbox]] [:tools "Tools" [:skills]]])

(defn- section-title
  "The heading clients show for a section. Outside global settings the providers section
   holds only response options, because providers are global."
  [id title local?]
  (if (and local? (= :providers id)) "Response" title))

(defn- group-title [group] (str/capitalize (str/replace (name group) #"[-_]+" " ")))

(defn- request-target
  [request body]
  (let [params (merge (:query-params request) body)]
    (scoped/target (lp/db-info) (get params "scope") (get params "target_id"))))

(defn- mark-overridden
  "Mark rows the optional `context_session_id` session takes from a more specific scope."
  [request target rows]
  (scoped/mark-overridden (lp/db-info)
                          target
                          rows
                          (get-in request [:query-params "context_session_id"])))

(defn- settings-response
  [f]
  (try (f)
       (catch clojure.lang.ExceptionInfo e
         (if-let [status (:status (ex-data e))]
           (http/error-response status
                                (or (:type (ex-data e)) :invalid-setting)
                                (ex-message e)
                                :id (:id (ex-data e))
                                :field-errors (:field-errors (ex-data e)))
           (throw e)))))

(defn- agent-name-setting
  []
  {:id "agent_name"
   :label "Agent name"
   :description "Shared by all clients of this gateway. Overrides project names."
   :type "string"
   :value (config/agent-name)
   :max-length 80
   :editor "text"
   :own-value (get (config/load-global-config-raw) "agent_name")
   :is-override (contains? (config/load-global-config-raw) "agent_name")
   :inherited-value (or (get (config/load-global-yaml-config-raw) "agent_name") "Vis")
   :inherited-source "default"
   :applies "next_turn"})

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

(defn- machine-name-setting
  "The name that Council Rooms shows for this machine. One gateway is one machine."
  []
  (let [value
        (rooms/machine-name (lp/db-info))

        default
        (rooms/default-machine-name)]

    {:id rooms/machine-name-id
     :label "Machine name"
     :description "Other machines in your rooms see this name."
     :type "string"
     :value value
     :max-length rooms/machine-name-length
     :editor "text"
     :own-value value
     :is-override (not= value default)
     :inherited-value default
     :inherited-source "default"
     :applies "immediate"
     :scopes ["global"]
     :parent "council"}))

(defn- set-machine-name-setting
  "Save the machine name. The inherit action gives the host name again."
  [action value]
  (if-not (#{"value" "inherit"} action)
    (http/error-response 400
                         :invalid-setting-action
                         "Machine name takes the value or inherit action.")
    (do (rooms/set-machine-name! (lp/db-info)
                                 (if (= "inherit" action) (rooms/default-machine-name) value))
        (http/json-response (machine-name-setting)))))

(defn- extension-rows
  "Row id -> `[extension position]` for each extension loaded where `target` runs.
   An extension's own section holds its engine choice, its settings and each packaged skill
   that has its own switch. A foundation extension is part of Vis and always registered,
   so it gets no section and no engine row (#337)."
  [target]
  (into {}
        (mapcat (fn [{ext-name :ext/name :as ext}]
                  (map-indexed (fn [position id]
                                 [id [ext-name position]])
                               (concat [(scoped/resource-id :engines ext-name)]
                                       (map :id (:ext/toggles ext))
                                       (map #(scoped/resource-id :skills (:name %))
                                            (filter harness/own-setting? (:ext/skills ext)))))))
        (remove #(= "foundation" (:ext/kind %)) (extension/registered-extensions (:root target)))))

(defn- extension-path
  "Name an extension file for its reader: inside the project, or under `~`."
  [root path]
  (let [prefix (str root "/")]
    (if (and root (str/starts-with? (str path) prefix))
      (subs (str path) (count prefix))
      (paths/abbreviate-home (str path)))))

(defn- extension-states
  "Origin, file and load state of each Python extension where `target` runs, by section
   name. A file that failed before it ever loaded is named by its file."
  [target]
  (let [root
        (:root target)

        origin
        #(if % "project" "global")

        loaded
        (into {}
              (map (fn [[path {:keys [ext-name project-root]}]]
                     [ext-name
                      {:origin (origin project-root)
                       :path (extension-path root path)
                       :status "loaded"}]))
              (python-extensions/loaded-python-extensions root))]

    (reduce (fn [states {:keys [file extension error stale? project-root]}]
              (update states
                      (or extension (.getName (io/file (str file))))
                      merge
                      {:origin (origin project-root)
                       :path (extension-path root file)
                       :status (if stale? "stale" "failed")
                       :error (str error)}))
            loaded
            (python-extensions/load-failures root))))

(defn- one-thinking-control
  "Keep one thinking control: the three levels while simplified thinking modes are on,
   else the exact provider levels. Both are session settings, so both showed (#334)."
  [rows]
  (let [simplified?
        (not (false? (:value (first (filter #(= "simplified_thinking_modes" (:id %)) rows)))))

        other
        (if simplified? "reasoning_effort" "reasoning_level")]

    (remove #(= other (:id %)) rows)))

(defn- settings-catalog
  [request target]
  (let [local?
        (not= "global" (:scope target))

        resources
        (resource-inventory target)

        owners
        (extension-rows target)

        channel
        (some-> (get-in request [:query-params "channel"])
                keyword)

        ;; MCP rows stay out: every scope's MCP servers section owns each server's switch.
        ;; An engine row shows only where its extension is loaded.
        rows
        (filter #(and
                   (some #{(:scope target)} (:scopes %))
                   (case (:group %)
                     :skills
                     (resources (:id %))

                     :mcp
                     false

                     :engines
                     (owners (:id %))

                     true)
                   (or local? (and (not (false? (:settings? %))) (toggles/toggle-visible? %)))
                   (or (nil? channel) (#{:all :*} channel) (toggles/toggle-for-channel? channel %)))
                (one-thinking-control (scoped/settings (lp/db-info) target)))

        ;; Everything an extension contributes is configured in that extension's section.
        section
        (fn [row]
          (if-let [[ext-name] (owners (:id row))]
            [:extension ext-name]
            [:vis (or (:group row) :other)]))

        states
        (extension-states target)

        ;; A failed extension keeps its section, so its error shows where its controls were.
        grouped
        (sort-by (fn [[[kind group] _]]
                   [(if (= :vis kind) 0 1) (str group)])
                 (merge (into {}
                              (keep (fn [[ext-name {:keys [status]}]]
                                      (when (not= "loaded" status) [[:extension ext-name] []])))
                              states)
                        (group-by section (mark-overridden request target rows))))]

    {:scope (:scope target)
     :target-id (:target-id target)
     :label (:label target)
     :revision (settings-edit/revision (lp/db-info) target)
     :lineage (cond-> [{:scope "global" :label "Machine"}]
                (:project-id target)
                (conj {:scope "project" :target-id (:project-id target)})

                (:group-id target)
                (conj {:scope "group" :target-id (:group-id target)})

                (= "session" (:scope target))
                (conj {:scope "session" :target-id (:target-id target) :label (:label target)}))
     :groups
     (let [vis
           (into {}
                 (keep (fn [[[kind group] specs]]
                         (when (= :vis kind) [group specs])))
                 grouped)

           rows
           #(mapv toggle-json (get vis %))

           section-rows
           (fn [id groups]
             (case id
               ;; The agent name stands first. The machine name follows the Council
               ;; switches, above the room actions. Experimental switches stand last.
               :general
               (concat (when-not local? [(assoc (agent-name-setting) :scopes ["global"])])
                       (rows :council)
                       (when-not local? [(machine-name-setting)])
                       (rows :experimental))

               :permissions
               (concat (scoped-policy/settings (lp/db-info) target) (rows :sandbox))

               (mapcat rows groups)))

           sectioned
           (set (mapcat last sections))]

       (vec
         (concat
           (keep (fn [[id title groups]]
                   (when-let [toggles (seq (section-rows id groups))]
                     {:id (name id)
                      :title (section-title id title local?)
                      :toggles (nest-rows toggles)}))
                 sections)
           (keep (fn [[[kind group] specs]]
                   (cond (= :extension kind)
                         {:id (str "extension:" group)
                          :title (str group)
                          :extension (merge {:name (str group) :origin "built_in" :status "loaded"}
                                            (get states group))
                          :toggles (nest-rows (mapv toggle-json
                                                    (sort-by #(second (owners (:id %))) specs)))}
                         (not (sectioned group)) {:id (name group)
                                                  :title (group-title group)
                                                  :toggles (nest-rows (mapv toggle-json specs))}))
                 grouped))))}))

(defn- list-settings-handler
  "GET /v1/settings; typed catalog, provenance and a concurrency revision."
  [request]
  (settings-response #(http/json-response (settings-catalog request (request-target request nil)))))

(defn- reload-extensions-handler
  "POST /v1/extensions/reload; load the extension files where a settings target runs again.
   This runs their code; reading the catalog never does. A machine target reloads global
   extensions alone."
  [request]
  (settings-response
    (fn []
      (let [body
            (http/body-json request)

            _
            (when-not (document/valid-json? "gateway" "settings_target" body)
              (throw (ex-info "Supply a valid settings target" {:status 400})))

            root
            (:root (request-target request body))]

        (http/json-response (let [{:keys [loaded failed scopes]}
                                  (binding [workspace/*workspace-root* root]
                                    (python-extensions/reload-python-extensions!
                                      (when-not root {:global-only? true})))]
                              {"loaded" loaded
                               "failed" failed
                               "scopes" (mapv (fn [{:keys [scope dirs loaded failed extensions]}]
                                                {"scope" (name scope)
                                                 "dirs" dirs
                                                 "loaded" loaded
                                                 "failed" failed
                                                 "extensions" extensions})
                                              scopes)}))))))

(defn- apply-settings-handler
  "PATCH /v1/settings; apply one versioned owner batch, or write nothing."
  [request]
  (settings-response
    (fn []
      (let [body
            (http/body-json request)

            _
            (when-not (document/valid-json? "gateway" "settings_batch" body)
              (throw (ex-info "Supply a valid settings batch" {:status 400})))

            request
            (update request :query-params merge (select-keys body ["channel" "context_session_id"]))

            _
            (when-let [sid (get-in request [:query-params "context_session_id"])]
              (scoped/target (lp/db-info) "session" sid))

            target
            (request-target request body)

            resources
            (resource-inventory target)]

        (doseq [{:strs [id]} (get body "changes")]
          (when (and (#{:skills :mcp} (:group (toggles/toggle-spec id))) (not (resources id)))
            (throw (ex-info "Resource is not available in this target" {:status 400 :id id}))))
        (settings-edit/apply! (lp/db-info) target (get body "revision") (get body "changes"))
        (http/json-response (settings-catalog request target))))))

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
            (first (mark-overridden request
                                    target
                                    (filter #(= id (:id %))
                                            (scoped/settings (lp/db-info) target))))]

        (cond
          (not (toggle-contract/toggle-id? id))
          (http/error-response 400 :invalid-setting-id "Setting id must be lower-case snake_case")
          (scoped-policy/setting? id)
          (http/json-response (first (filter #(= id (:id %))
                                             (scoped-policy/settings (lp/db-info) target))))
          (and (= id "agent_name") (= "global" (:scope target))) (http/json-response
                                                                   (agent-name-setting))
          (and (= id rooms/machine-name-id) (= "global" (:scope target))) (http/json-response
                                                                            (machine-name-setting))
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
              (= id rooms/machine-name-id)
              (if (= "global" (:scope target))
                (set-machine-name-setting action (get body "value"))
                (http/error-response 400 :invalid-setting-scope "Machine name is global"))
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
   [:patch "/v1/settings"] apply-settings-handler
   [:post "/v1/extensions/reload"] reload-extensions-handler
   [:get "/v1/improve"] (improve-handler :list)
   [:post "/v1/improve"] (improve-handler :create)
   [:get "/v1/improve/settings"] (improve-handler :settings)
   [:patch "/v1/improve/settings"] (improve-handler :save-settings)
   [:post "/v1/improve/review"] (improve-handler :review)
   [:get "/v1/improve/:id"] (improve-handler :get)
   [:patch "/v1/improve/:id"] (improve-handler :update)
   [:get "/v1/settings/:id"] get-setting-handler})
