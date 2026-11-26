(ns com.blockether.vis.internal.gateway.response-schema-test
  "Real gateway answers match the response schema that their route declares (#487)."
  (:require [lazytest.core :refer [defdescribe expect it throws?]]
            [com.blockether.vis.contract.document :as document]
            [com.blockether.vis.contract.gateway :as gateway-contract]
            [com.blockether.vis.contract.openapi :as openapi]
            [com.blockether.vis.contract.wire :as wire]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.gateway.server :as server]
            [com.blockether.vis.internal.gateway.state :as state]
            [com.blockether.vis.internal.gateway.view :as gateway-view]
            [com.blockether.vis.internal.gateway.wiring :as wiring]
            [com.blockether.vis.internal.loop :as lp]
            [com.blockether.vis.internal.task :as task]
            [com.blockether.vis.internal.persistance.core :as persistance]
            [com.blockether.vis.internal.persistance.sqlite.core :as sqlite]
            [com.blockether.vis.internal.persistance.sqlite.test-helpers :as h]
            [com.blockether.vis.internal.gateway.response-check :as response-check]
            [reitit.ring :as rr]
            [ring.middleware.params :as ring-params]
            [ring.core.protocols :as ring-protocols]))

(wiring/install!)

(defn- app
  []
  (rr/ring-handler ((ns-resolve 'com.blockether.vis.internal.gateway.server 'router) "token" [])))

(defn- call
  "Send one request to `handler` and answer its status and parsed JSON body."
  [handler method uri & [body]]
  (let [[path query]
        (str/split uri #"\?" 2)

        response
        (handler (cond-> (ring-params/assoc-query-params
                           {:request-method method :uri path :query-string query}
                           "UTF-8")
                   body
                   (assoc :body
                     (java.io.ByteArrayInputStream. (.getBytes (wire/json-str body) "UTF-8")))))

        out
        (java.io.ByteArrayOutputStream.)]

    (ring-protocols/write-body-to-stream (:body response) response out)
    {:status (:status response)
     :headers (:headers response)
     :body (let [text (.toString out "UTF-8")]
             (when (seq text) (wire/parse-json text)))}))

(defn- declared
  "The response schema name of `method` on the contract `path`."
  [method path]
  (some-> (gateway-contract/operation method path)
          :response-schema
          name))

(defdescribe task-polling-response
  (it "snapshots running Tasks, then replays their settlement once"
      (let [store (sqlite/db-open! :memory)
            sid (str (h/store-session! store {:channel :api}))
            uri (str "/v1/sessions/" sid "/events-since?tasks=true&cursor=")
            handler (app)]
        (try
          (with-redefs [lp/db-info (constantly store)]
            (let [{:keys [row complete!]} (task/start-manual! sid {:name "watch"} (fn [_]))
                  initial (call handler :get (str uri -1))
                  cursor (get-in initial [:body "cursor"])]
              (expect (= 200 (:status initial)))
              (expect (document/valid-json? "gateway" (declared :get "/v1/sessions/:sid/events-since") (:body initial)))
              (expect (= [(:id row)] (mapv #(get % "id") (get-in initial [:body "events" 0 "tasks"]))))
              (complete! (constantly {:state :done :value "ready"}))
              (let [after (call handler :get (str uri cursor))
                    events (get-in after [:body "events"])]
                (expect (= 200 (:status after)))
                (expect (= [["task.ended" (:id row) "done"]]
                           (mapv (fn [event] [(get event "type") (get-in event ["task" "id"])
                                              (get-in event ["task" "state"])]) events)))
                (expect (= [] (get-in (call handler :get (str uri (get-in after [:body "cursor"])))
                                      [:body "events"]))))))
          (finally (task/sweep! sid :session-close) (sqlite/db-close! store))))))

(defdescribe every-json-route-declares-a-response-schema
             (it "names a gateway schema for each JSON response"
                 (doseq [{:keys [path operations]}
                         gateway-contract/route-table

                         [method {:keys [response response-schema]}]
                         operations

                         :when (= :json response)]

                   (expect (some? response-schema) (pr-str [method path]))
                   (expect (some? (get-in (document/schema-document "gateway")
                                          ["$defs" (name response-schema)]))
                           (pr-str [method path response-schema])))))

(def ^:private open-answers
  "The places in the JSON answers whose schema does not list fields, each with its
   reason. A place is a schema name and a path: `.field`, `[]` for array items and
   `|n` for a `oneOf`, `anyOf` or `allOf` branch. This map can only get smaller: the
   test fails when an entry has fields or when a new open place comes."
  {"activity_page" "The page shape comes from the activity history store. It is not mapped yet."
   "app_door_answer|0" "Ordinary app door answers vary with the live operation in the envelope."
   "app_list.views[]" "An extension declares the view fields."
   "app_view|0.view" "An extension declares the view fields."
   "capabilities.features" "Each feature has its own shape, and extensions add features."
   "client_calls.calls[].kwargs"
   "The arguments of a client extension call. The client extension declares them."
   "decision_answer.answers" "The keys are the question names that the caller sends."
   "decision_answer.usage" "The decision model runtime reports this usage. It is not mapped yet."
   "device_registered.device" "The device record of the push relay. It is not mapped yet."
   "device_registered.push" "The push transport status. It is not mapped yet."
   "device_test.results[]" "The push relay sends one result for each device. It is not mapped yet."
   "devices.devices[]" "The device record of the push relay. It is not mapped yet."
   "devices.push"
   "The push transport status has one entry for each transport. It is not mapped yet."
   "mcp_app_rpc_answer|0.result" "The raw MCP result. The MCP method of the request defines its fields."
   "models_catalog.models[]" "Each provider adds its own model fields."
   "openapi_document.info" "The OpenAPI Info object. The OpenAPI 3.1 schema defines it."
   "openapi_document.paths" "The OpenAPI Paths object. The OpenAPI 3.1 schema defines it."
   "provider_presets.presets[]" "Each provider preset adds its own fields."
   "router_fleet.providers[]" "Each provider adds its own fields to its router entry."
   "session.match" "The search match comes from the search index. It is not mapped yet."
   "session_artifacts.artifacts[]" "Each tool writes its own artifact fields."
   "session_context" "The session mirror that the model sees. Extensions add open keys."
   "session_fork_turns.turns[].council|0" "The Council data of a turn. It is not mapped yet."
   "session_resources.resources[]" "Each extension declares its own resource fields."
   "session_suggestions[]" "Each extension declares its own suggestion fields."
   "session_workspace.workspace.git"
   "The git status has different fields for a repository and for a folder. It is not mapped yet."
   "setting.editor_options" "Each setting editor declares its own options."
   "setting.own_value" "A setting value has the type of its setting."
   "setting_value" "A setting value has the type of its setting."
   "settings.lineage[]" "Each configuration layer adds its own fields."
   "speech_voice.voice" "Each speech engine declares its own voice fields."
   "transcript.turns[]" "The transcript turn row comes from the store. It is not mapped yet."
   "turn.content[]"
   "The content blocks have one shape for each block type. They are not mapped yet."
   "turn.council|0" "The Council data of a turn. It is not mapped yet."
   "turn_attachments.attachments[]" "Each attachment kind adds its own fields."
   "turn_trace.iterations[]" "The trace row comes from the store. It is not mapped yet."
   "view_log" "The log page shape comes from the view sink. It is not mapped yet."
   "view_requests.requests[]" "An extension declares the request fields."})

(def ^:private shape-keys
  "Keys that give a schema a shape other than an object with listed fields."
  ["$ref" "oneOf" "anyOf" "allOf" "enum" "const" "items" "prefixItems"])

(defn- open-schema?
  "True when `schema` accepts any object: `{}`, `true`, or an object type without
   listed fields. An empty `properties` without `additionalProperties: false` is open."
  [schema]
  (or (true? schema)
      (= {} schema)
      (and (map? schema)
           (not-any? #(contains? schema %) shape-keys)
           (let [types
                 (get schema "type")

                 types
                 (if (sequential? types) (set types) #{types})

                 extra
                 (get schema "additionalProperties")]

             (and (or (contains? types nil) (contains? types "object"))
                  (empty? (get schema "properties"))
                  (not (false? extra))
                  (not (and (map? extra) (seq extra))))))))

(defn- all-optional?
  "True when `schema` lists fields but requires none and allows other fields."
  [schema]
  (and (map? schema)
       (seq (get schema "properties"))
       (empty? (get schema "required"))
       (not (false? (get schema "additionalProperties")))))

(defn- constraint-only?
  "True when `schema` only adds a `required` or `not` rule to its parent shape."
  [schema]
  (and (map? schema)
       (every? #{"required" "not" "description" "title"} (keys schema))
       (some #(contains? schema %) ["required" "not"])))

(defn- loose-places
  "The `[kind place]` pairs of the open and all-optional schemas under `schema`.
   Each local `$ref` is followed once, and named by its schema name."
  [defs schema place seen]
  (cond (true? schema) [[:open place]]
        (or (not (map? schema)) (constraint-only? schema)) []
        (str/starts-with? (str (get schema "$ref")) "#/$defs/")
        (let [ref-name (subs (get schema "$ref") (count "#/$defs/"))]
          (if (contains? @seen ref-name)
            []
            (do (swap! seen conj ref-name) (loose-places defs (get defs ref-name) ref-name seen))))
        :else (concat
                (cond (open-schema? schema) [[:open place]]
                      (all-optional? schema) [[:optional place]])
                (mapcat (fn [[k v]]
                          (loose-places defs v (str place "." k) seen))
                        (get schema "properties"))
                (when (or (map? (get schema "items")) (true? (get schema "items")))
                  (loose-places defs (get schema "items") (str place "[]") seen))
                (when (map? (get schema "additionalProperties"))
                  (loose-places defs (get schema "additionalProperties") (str place ".*") seen))
                (mapcat (fn [k]
                          (apply concat
                            (map-indexed (fn [i v]
                                           (loose-places defs v (str place "|" i) seen))
                                         (get schema k))))
                        ["oneOf" "anyOf" "allOf"]))))

(defn- answer-loose-places
  [defs]
  (let [names
        (set (for [{:keys [operations]}
                   gateway-contract/route-table

                   [_ {:keys [response-schema]}]
                   operations

                   :when response-schema]

               (name response-schema)))

        seen
        (atom names)]

    (set (mapcat #(loose-places defs (get defs %) % seen) (sort names)))))

(defdescribe
  every-response-schema-lists-its-fields
  (it "lists fields for each JSON answer and its nested objects, except the shrinking allowlist"
      (let [defs
            (get (document/schema-document "gateway") "$defs")

            places
            (answer-loose-places defs)]

        (expect (= (set (keys open-answers)) (set (map second places)))
                (pr-str (sort (map second places))))
        (expect (empty? (filter (comp #{:optional} first) places))
                (pr-str (filter (comp #{:optional} first) places)))))
  (it "treats an empty field list and an all-optional object as loose"
      (let [defs
            {"empty" {"type" "object" "properties" {}}
             "closed" {"type" "object" "properties" {} "additionalProperties" false}
             "optional" {"type" "object" "properties" {"a" {"type" "string"}} "required" []}
             "nested" {"type" "object"
                       "properties" {"inner" {"type" ["object" "null"]}}
                       "required" ["inner"]}}

            places
            #(set (loose-places defs (get defs %) % (atom #{})))]

        (expect (= #{[:open "empty"]} (places "empty")))
        (expect (empty? (places "closed")))
        (expect (= #{[:optional "optional"]} (places "optional")))
        (expect (= #{[:open "nested.inner"]} (places "nested"))))))

(defdescribe
  real-answers-match-their-response-schema
  (it
    "validates the answers of the session, turn, project and group routes"
    (let [store (sqlite/db-open! :memory)]
      (try
        (let [sid (str (h/store-session! store {:channel :api}))
              tid (str (persistance/db-store-session-turn! store
                                                           {:parent-session-id sid
                                                            :user-request "ship it"}))
              _ (persistance/db-update-session-turn!
                  store
                  tid
                  {:status :success :content [{"type" "prose" "markdown" "shipped"}]})
              handler (app)
              root (str (System/getProperty "java.io.tmpdir"))
              checked (atom [])
              check! (fn [method path uri & [body]]
                       (let [{:keys [status headers body]} (call handler method uri body)
                             schema (declared method path)
                             template (str/replace path #":([^/]+)" "{$1}")
                             operation (get-in (openapi/document) ["paths" template (name method)])
                             response (get-in operation ["responses" (str status)])
                             queries (into #{} (keep #(when (= "query" (get % "in"))
                                                        (get % "name")))
                                           (get operation "parameters"))]

                         (expect (<= 200 status 299) (pr-str [method uri status body]))
                         (expect (some? response) (pr-str [method uri status]))
                         (expect (contains? (get response "content")
                                            (some-> (get (update-keys headers str/lower-case) "content-type")
                                                    (str/split #";")
                                                    first))
                                 (pr-str [method uri headers]))
                         (when-let [q (second (str/split uri #"\?" 2))]
                           (doseq [key (map #(first (str/split % #"=" 2)) (str/split q #"&"))]
                             (expect (contains? queries key) (pr-str [method uri key]))))
                         (when (<= 200 status 299)
                           (swap! checked conj [method path])
                           (expect (document/valid-json? "gateway" schema body)
                                   (pr-str [method path (document/explain "gateway" schema body)])))
                         body))]

          (with-redefs [lp/db-info (constantly store)]
            (check! :post "/v1/sessions" "/v1/sessions")
            (doseq [[path suffix]
                    [["/v1/sessions/:sid/turns" "/turns"]
                     ["/v1/sessions/:sid/turns/:tid" (str "/turns/" tid)]
                     ["/v1/sessions/:sid/turns/:tid/attachments" (str "/turns/" tid "/attachments")]
                     ["/v1/sessions/:sid/turns/:tid/trace" (str "/turns/" tid "/trace")]
                     ["/v1/sessions/:sid/transcript" "/transcript"]
                     ["/v1/sessions/:sid/forks" "/forks"] ["/v1/sessions/:sid/usage" "/usage"]
                     ["/v1/sessions/:sid/seq" "/seq"]
                     ["/v1/sessions/:sid/events-since" "/events-since"]
                     ["/v1/sessions/:sid/alert" "/alert"]
                     ["/v1/sessions/:sid/artifacts" "/artifacts"]
                     ["/v1/sessions/:sid/model" "/model"] ["/v1/sessions/:sid/slashes" "/slashes"]
                     ["/v1/sessions/:sid/workspace" "/workspace"]
                     ["/v1/sessions/:sid/council" "/council"]
                     ["/v1/sessions/:sid/council/entries" "/council/entries"]
                     ["/v1/sessions/:sid/council/threads" "/council/threads"]
                     ["/v1/sessions/:sid/council/members" "/council/members"]
                     ["/v1/sessions/:sid/views/input" "/views/input"]
                     ["/v1/sessions/:sid/views/live" "/views/live"]]]
              (check! :get path (str "/v1/sessions/" sid suffix)))
            ;; A session with no turns has no usage rollup yet: `usage` is null.
            (let [empty-sid (str (h/store-session! store {:channel :api}))]
              (check! :get "/v1/sessions/:sid/usage" (str "/v1/sessions/" empty-sid "/usage")))
            (doseq [path ["/v1/capabilities" "/v1/projects" "/v1/projects/overview" "/v1/apps"
                          "/v1/models" "/v1/router" "/v1/sessions/actions/search"]]
              (check! :get path path))
            (check! :post
                    "/v1/sessions/:sid/queue/send-now"
                    (str "/v1/sessions/" sid "/queue/send-now")
                    {})
            (check! :put "/v1/sessions/:sid/read" (str "/v1/sessions/" sid "/read") {})
            (check! :post
                    "/v1/sessions/:sid/council/changed"
                    (str "/v1/sessions/" sid "/council/changed")
                    {})
            (let [project (check! :post "/v1/projects"
                                  "/v1/projects" {:name "Probe" :workspace_root root})
                  pid (get project "id")
                  group (check! :post "/v1/session-groups"
                                "/v1/session-groups" {:project_id pid :name "G"})]

              (check! :get "/v1/projects/:pid" (str "/v1/projects/" pid))
              (check! :get "/v1/session-groups" (str "/v1/session-groups?project=" pid))
              (check! :patch
                      "/v1/session-groups/:gid"
                      (str "/v1/session-groups/" (get group "id"))
                      {:name "G2"})
              (check! :put
                      "/v1/sessions/:sid/group"
                      (str "/v1/sessions/" sid "/group")
                      {:group_id (get group "id")})
              (let [ungrouped (check! :put "/v1/sessions/:sid/group"
                                      (str "/v1/sessions/" sid "/group"))]
                (expect (nil? (get ungrouped "group_id"))))
              (check! :delete
                      "/v1/session-groups/:gid"
                      (str "/v1/session-groups/" (get group "id")))))
          (expect (<= 35 (count @checked)) (pr-str @checked)))
        (finally (sqlite/db-close! store))))))

(defdescribe
  declared-turn-submit-variants
  (it "describes the real accepted and idempotent reply statuses"
      (let [sid (str (random-uuid))
            uri (str "/v1/sessions/" sid "/turns")
            handler (app)
            responses (get-in (openapi/document)
                              ["paths" "/v1/sessions/{sid}/turns" "post" "responses"])
            turn (get-in (wire/parse-json (slurp (io/resource
                                                  "vis-contract/fixtures/gateway-answers.json")))
                         ["answers" "GET /v1/sessions/:sid/turns/:tid"])
            idempotent? (atom false)]
        (with-redefs [state/session-archived? (constantly false)
                      state/submit-turn! (fn [_ _] {:turn turn :idempotent? @idempotent?})]
          (doseq [retry? [false true]]
            (reset! idempotent? retry?)
            (let [{:keys [status body]} (call handler :post uri {:request "Probe"})]
              (expect (= turn body))
              (expect (= (if retry? 200 202) status))
              (expect (contains? (get-in responses [(str status) "content"]) "application/json"))))))))

(defn- capabilities-handler
  "A router that answers GET /v1/capabilities with `body`, checked by the hook."
  [body]
  (response-check/checked #(rr/ring-handler
                             (rr/router [["/v1/capabilities"
                                          {:get (fn [_]
                                                  {:status 200
                                                   :headers {"Content-Type" "application/json"}
                                                   :body (wire/json-str body)})}]]))))

(defdescribe
  response-check-refuses-a-changed-answer
  (it "passes a matching answer and refuses one without a required field"
      (let [good (get-in (wire/parse-json (slurp (io/resource
                                                   "vis-contract/fixtures/gateway-answers.json")))
                         ["answers" "GET /v1/capabilities"])]
        (expect (= 200 (:status (call (capabilities-handler good) :get "/v1/capabilities"))))
        (expect (throws?
                  clojure.lang.ExceptionInfo
                  #(call (capabilities-handler (dissoc good "version")) :get "/v1/capabilities")))
        (expect (throws? clojure.lang.ExceptionInfo
                         #(call (capabilities-handler (-> good
                                                          (dissoc "version")
                                                          (assoc "release" "1.0.0")))
                                :get
                                "/v1/capabilities"))))))


(defdescribe negotiated-and-domain-responses
  (it "requires authentication for readiness and brand writes but not brand reads"
      (let [wrap-auth (ns-resolve 'com.blockether.vis.internal.gateway.server 'wrap-auth)
            handler (wrap-auth (constantly {:status 204}) "token" [])]
        (with-redefs [server/auth-required? (constantly true)]
          (expect (= 401 (:status (call handler :get "/readyz"))))
          (expect (= 204 (:status (call handler :get "/v1/web/brand"))))
          (expect (= 401 (:status (call handler :put "/v1/web/brand")))))))
  (it "describes the version refusal sent before the protected route runs"
      (let [wrap-protocol (ns-resolve 'com.blockether.vis.internal.gateway.server 'wrap-protocol)
            handler (wrap-protocol (app) [])
            {:keys [status body]} (call handler :get "/v1/models")]
        (expect (= 426 status))
        (expect (get-in (openapi/document) ["paths" "/v1/models" "get" "responses" "426"]))
        (expect (document/valid-json? "gateway" "incompatible_protocol" body)
                (pr-str body))))
  (it "returns a stale View action as a domain result"
      (let [sid (str (random-uuid))
            vid (str (random-uuid))
            handler (app)]
        (with-redefs [gateway-view/view-of (constantly {:id vid})
                      gateway-view/action! (constantly {:is-accepted false
                                                       :reason "stale_revision"
                                                       :revision "new"})]
          (let [{:keys [status body]}
                (call handler :post (str "/v1/sessions/" sid "/views/" vid "/actions")
                      {"action" "submit" "revision" "old"})]
            (expect (= 200 status))
            (expect (= "stale_revision" (get body "reason")))
            (expect (document/valid-json? "gateway" "view_outcome" body))
            (doseq [invalid [(assoc body "reason" "free-form text")
                             (assoc body "is_accepted" true)
                             (dissoc body "reason")]]
              (expect (not (document/valid-json? "gateway" "view_outcome" invalid)))))))))
