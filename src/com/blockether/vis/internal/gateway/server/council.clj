(ns com.blockether.vis.internal.gateway.server.council
  "Council and agent-team routes for one session."
  (:require [com.blockether.vis.internal.gateway.server.http :as http]
            [com.blockether.vis.internal.gateway.state :as state]))

(defn- agents-handler
  [operation]
  (fn [request]
    (try (let [opts (case operation
                      :list
                      {}

                      :timeline
                      {:session_id (get-in request [:path-params :child-id])}

                      (http/body-json request))]
           (when-not (map? opts)
             (throw (ex-info "Expected an agents JSON object" {:error :invalid-request})))
           (http/json-response
             (state/agents-operation! (get-in request [:path-params :sid]) operation opts)))
         (catch clojure.lang.ExceptionInfo e
           (http/error-response (case (:error (ex-data e))
                                  :invalid-request
                                  400

                                  :session-not-found
                                  404

                                  409)
                                (:error (ex-data e))
                                (ex-message e))))))

(defn- council-handler
  [operation]
  (fn [request]
    (try (let [raw
               (if (contains? #{:publish :wake} operation)
                 (let [body (try (http/body-json request) (catch Exception _ nil))]
                   (when-not (map? body)
                     (throw (ex-info "Expected a Council JSON object" {:error :invalid-request})))
                   body)
                 (:query-params request))

               opts
               (into {}
                     (map (fn [[k v]]
                            [(keyword k)
                             (if (and (string? v) (contains? #{"thread_id" "after" "limit"} k))
                               (Long/parseLong v)
                               v)]))
                     raw)

               opts
               (cond-> opts
                 (= operation :get)
                 (assoc :entry_id (Long/parseLong (get-in request [:path-params :entry-id]))))]

           (http/json-response
             (state/council-operation! (get-in request [:path-params :sid]) operation opts)))
         (catch NumberFormatException _
           (http/error-response 400
                                :invalid-request
                                "Council identifiers and cursors must be integers"))
         (catch clojure.lang.ExceptionInfo e
           (let [kind
                 (:error (ex-data e))

                 status
                 (case kind
                   (:group-not-found :entry-not-found)
                   404

                   (:disabled :inactive-session
                              :invalid-recipient :idempotency-conflict
                              :budget-exceeded :not-owner)
                   409

                   (:invalid-request :invalid-thread :invalid-reply)
                   400

                   (throw e))]

             (http/error-response status kind (ex-message e)))))))

(defn- task-handler [cancel?]
  (fn [request]
    (let [{:keys [sid task-id]} (:path-params request)]
      (try (http/json-response (state/task-operation! sid task-id cancel?))
           (catch clojure.lang.ExceptionInfo e
             (if (= :task-not-found (:error (ex-data e)))
               (http/error-response 404 :task-not-found (ex-message e))
               (throw e)))))))

(def handlers
  "Handlers for this namespace's routes, keyed by the gateway contract's `[method path]`."
  {[:post "/v1/sessions/:sid/agents"] (agents-handler :spawn)
   [:post "/v1/sessions/:sid/agents/answer"] (agents-handler :answer)
   [:post "/v1/sessions/:sid/agents/deny"] (agents-handler :deny)
   [:post "/v1/sessions/:sid/agents/extend"] (agents-handler :extend)
   [:post "/v1/sessions/:sid/agents/grant"] (agents-handler :grant)
   [:post "/v1/sessions/:sid/agents/route"] (agents-handler :route)
   [:post "/v1/sessions/:sid/agents/run"] (agents-handler :run)
   [:get "/v1/sessions/:sid/tasks/:task-id"] (task-handler false)
   [:post "/v1/sessions/:sid/tasks/:task-id/cancel"] (task-handler true)
   [:get "/v1/sessions/:sid/agents/:child-id/timeline"] (agents-handler :timeline)
   [:get "/v1/sessions/:sid/council"] (council-handler :binding)
   [:post "/v1/sessions/:sid/council/wake"] (council-handler :wake)
   [:get "/v1/sessions/:sid/council/members"] (council-handler :members)
   [:get "/v1/sessions/:sid/council/threads"] (council-handler :threads)
   [:get "/v1/sessions/:sid/council/entries"] (council-handler :read)
   [:post "/v1/sessions/:sid/council/entries"] (council-handler :publish)
   [:get "/v1/sessions/:sid/council/entries/:entry-id"] (council-handler :get)})
