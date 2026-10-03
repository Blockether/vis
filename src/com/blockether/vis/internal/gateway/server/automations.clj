(ns com.blockether.vis.internal.gateway.server.automations
  "Automation routes. `/v1/hooks/:automation-id` is the only route without the
   gateway token: the webhook secret of the automation authenticates it."
  (:require [com.blockether.vis.internal.automation.core :as automation]
            [com.blockether.vis.internal.automation.runner :as runner]
            [com.blockether.vis.internal.gateway.server.http :as http]
            [com.blockether.vis.internal.loop :as lp])
  (:import (java.io InputStream)))

(defn- now [] (System/currentTimeMillis))

(defn- json-body
  [request]
  (let [body (http/body-json request)]
    (if (map? body)
      body
      (throw (ex-info "The request body must be a JSON object"
                      {:status 400 :code :invalid-automation})))))

(defn- respond
  [f]
  (fn [request]
    (try (http/json-response (f request (lp/db-info) (:path-params request)))
         (catch clojure.lang.ExceptionInfo e
           (let [{:keys [status code]} (ex-data e)]
             (if status
               (http/error-response status (or code :automation-error) (ex-message e))
               (throw e)))))))

(defn- runs
  [request db _]
  {"runs" (automation/runs db
                           {:automation-id (http/query-str request "automation_id")
                            :statuses (some-> (http/query-str request "status")
                                              vector)
                            :session-id (http/query-str request "session_id")
                            :limit (min 200 (max 1 (or (http/query-long request "limit") 50)))})})

(defn- read-limited
  "The body bytes, or nil when the body is larger than `limit`."
  [request limit]
  (let [^InputStream in
        (:body request)

        data
        (if in (.readNBytes in (int (inc (long limit)))) (byte-array 0))]

    (when (<= (alength ^bytes data) (long limit)) data)))

(defn- hook
  [request]
  (let [body (read-limited request (automation/limit :webhook_body_bytes))]
    (if-not body
      (http/error-response 413 :payload-too-large "The webhook body is too large")
      (let [{:keys [status error] :as result} (runner/accept-webhook!
                                                (lp/db-info)
                                                (get-in request [:path-params :automation-id])
                                                {:headers (:headers request) :body body})]
        (if error
          (http/error-response status (first error) (second error))
          (http/json-response status (:body result)))))))

(def handlers
  {[:get "/v1/automations"] (respond (fn [_ db _]
                                       {"automations" (automation/list-all db (now))
                                        "is_enabled" (runner/globally-enabled? db)}))
   [:post "/v1/automations"] (respond (fn [request db _]
                                        (automation/create! db (json-body request) (now))))
   [:get "/v1/automations/runs"] (respond runs)
   [:get "/v1/automations/runs/:run-id"] (respond (fn [_ db params]
                                                    (automation/run db (:run-id params))))
   [:get "/v1/automations/:automation-id"]
   (respond (fn [_ db params]
              (automation/describe db (:automation-id params) (now))))
   [:patch "/v1/automations/:automation-id"]
   (respond (fn [request db params]
              (automation/update! db (:automation-id params) (json-body request) (now))))
   [:delete "/v1/automations/:automation-id"] (respond
                                                (fn [_ db params]
                                                  (automation/delete! db (:automation-id params))))
   [:post "/v1/automations/:automation-id/run"] (respond
                                                  (fn [_ db params]
                                                    (runner/run-now! db (:automation-id params))))
   [:post "/v1/automations/:automation-id/secrets"] (respond (fn [request db params]
                                                               (automation/rotate-secret!
                                                                 db
                                                                 (:automation-id params)
                                                                 (get (json-body request) "kind")
                                                                 (now))))
   [:post "/v1/hooks/:automation-id"] hook})
