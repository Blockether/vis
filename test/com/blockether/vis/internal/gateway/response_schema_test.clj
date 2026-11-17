(ns com.blockether.vis.internal.gateway.response-schema-test
  "Real gateway answers match the response schema that their route declares (#487)."
  (:require [lazytest.core :refer [defdescribe expect it throws?]]
            [com.blockether.vis.contract.document :as document]
            [com.blockether.vis.contract.gateway :as gateway-contract]
            [com.blockether.vis.contract.wire :as wire]
            [clojure.string :as str]
            [com.blockether.vis.internal.gateway.server]
            [com.blockether.vis.internal.gateway.wiring :as wiring]
            [com.blockether.vis.internal.loop :as lp]
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
     :body (let [text (.toString out "UTF-8")]
             (when (seq text) (wire/parse-json text)))}))

(defn- declared
  "The response schema name of `method` on the contract `path`."
  [method path]
  (some-> (gateway-contract/operation method path)
          :response-schema
          name))

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
  "The JSON answers whose schema does not yet list fields. This set can only get
   smaller: the test fails when an entry has fields or when a new open answer comes."
  {"activity_page" "The page shape comes from the activity history store. It is not mapped yet."
   "app_door_answer" "The live seam answer changes with the live operation in the envelope."
   "session_context" "The session mirror that the model sees. Extensions add open keys."
   "view_log" "The log page shape comes from the view sink. It is not mapped yet."})

(defn- open-schema?
  "True when `schema` constrains no field: `{}`, `true`, or only `type: object`."
  [schema]
  (or (true? schema)
      (and (map? schema)
           (not-any? #(contains? schema %)
                     ["properties" "required" "$ref" "oneOf" "anyOf" "allOf" "items" "enum"
                      "const"])
           (contains? #{nil "object"} (get schema "type")))))

(defdescribe every-response-schema-lists-its-fields
             (it "lists fields for each JSON answer, except the shrinking allowlist"
                 (let [defs
                       (get (document/schema-document "gateway") "$defs")

                       open
                       (set (for [{:keys [operations]}
                                  gateway-contract/route-table

                                  [_ {:keys [response-schema]}]
                                  operations

                                  :when response-schema
                                  :let [schema-name
                                        (name response-schema)]
                                  :when (open-schema? (get defs schema-name))]

                              schema-name))]

                   (expect (= (set (keys open-answers)) open) (pr-str open)))))

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
                       (let [{:keys [status body]} (call handler method uri body)
                             schema (declared method path)]

                         (expect (<= 200 status 299) (pr-str [method uri status body]))
                         (when (<= 200 status 299)
                           (swap! checked conj [method path])
                           (expect (document/valid-json? "gateway" schema body)
                                   (pr-str [method path (document/explain "gateway" schema body)])))
                         body))]

          (with-redefs [lp/db-info (constantly store)]
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
            (doseq [path ["/v1/capabilities" "/v1/projects" "/v1/projects/overview" "/v1/apps"
                          "/v1/models" "/v1/router" "/v1/sessions/actions/search"]]
              (check! :get path path))
            (check! :post
                    "/v1/sessions/:sid/queue/send-now"
                    (str "/v1/sessions/" sid "/queue/send-now")
                    {})
            (check! :put "/v1/sessions/:sid/read" (str "/v1/sessions/" sid "/read") {})
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
              (check! :delete
                      "/v1/session-groups/:gid"
                      (str "/v1/session-groups/" (get group "id")))))
          (expect (<= 35 (count @checked)) (pr-str @checked)))
        (finally (sqlite/db-close! store))))))

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
      (let [good {"version" 1 "protocol" {"current" 1} "features" {}}]
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
