(ns com.blockether.vis.internal.gateway.server.automations-test
  (:require [com.blockether.vis.contract.document :as document]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.internal.automation.core :as automation]
            [com.blockether.vis.internal.automation.runner :as runner]
            [com.blockether.vis.internal.gateway.server.automations :as api]
            [com.blockether.vis.internal.loop :as lp]
            [com.blockether.vis.internal.persistance.core :as ps]
            [lazytest.core :refer [defdescribe expect it]])
  (:import (java.io ByteArrayInputStream)
           (java.nio.charset StandardCharsets)))

(def ^:private input
  {"name" "Morning report"
   "triggers" [{"kind" "every" "seconds" 3600}]
   "prompt" "Summarize the open pull requests."
   "target" {"mode" "temporary"}})

(defn- stream
  [body]
  (ByteArrayInputStream.
    (if (bytes? body) body (.getBytes ^String (wire/json-str body) StandardCharsets/UTF_8))))

(defn- call
  "Answer one request through the route table and parse the JSON body."
  [method path & {:keys [params query body headers]}]
  (let [response ((get api/handlers [method path])
                   {:path-params params
                    :query-params query
                    :headers (or headers {})
                    :body (some-> body
                                  stream)})]
    (assoc response :json (wire/parse-json (:body response)))))

(defn- with-db
  "Run `f` with an in-memory store as the gateway database."
  [f]
  (let [db (ps/db-create-connection! :memory)]
    (try (with-redefs [lp/db-info (constantly db)]
           (f db))
         (finally (ps/db-dispose-connection! db)))))

(defn- valid? [definition body] (document/valid-json? "gateway" definition body))

(defdescribe
  automation-routes-test
  (it
    "creates, reads, changes and deletes an automation"
    (with-db
      (fn [_]
        (let [created
              (call :post "/v1/automations" :body input)

              id
              (get-in created [:json "id"])

              params
              {:automation-id id}]

          (expect (= 200 (:status created)))
          (expect (valid? "automations_automation" (:json created)))
          (expect (= "Morning report" (get-in created [:json "name"])))
          (let [{:keys [status json]} (call :get "/v1/automations")]
            (expect (= 200 status))
            (expect (valid? "automations_list" json))
            (expect (not (contains? json "is_enabled")))
            (expect (= [id] (mapv #(get % "id") (get json "automations")))))
          (expect (= id
                     (get-in (call :get "/v1/automations/:automation-id" :params params)
                             [:json "id"])))
          (let [{:keys [status json]} (call :patch "/v1/automations/:automation-id"
                                            :params params
                                            :body {"name" "Evening report"})]
            (expect (= 200 status))
            (expect (= "Evening report" (get json "name"))))
          (let [{:keys [status json]} (call :post "/v1/automations/:automation-id/secrets"
                                            :params params
                                            :body {"kind" "callback"})]
            (expect (= 200 status))
            (expect (valid? "automations_secret_created" json))
            (expect (= "callback" (get json "kind"))))
          (let [{:keys [status json]} (call :delete "/v1/automations/:automation-id"
                                            :params params)]
            (expect (= 200 status))
            (expect (valid? "automations_deleted" json))
            (expect (= {"id" id "is_deleted" true} json)))
          (let [{:keys [status json]} (call :get "/v1/automations/:automation-id" :params params)]
            (expect (= 404 status))
            (expect (valid? "error_response" json))
            (expect (= "not-found" (get-in json ["error" "type"]))))))))
  (it "answers a request that is not valid with HTTP 400 and names the field"
      (with-db (fn [_]
                 (let [{:keys [status json]}
                       (call :post "/v1/automations"
                             :body (assoc input "triggers" [{"kind" "every" "seconds" 6}]))]
                   (expect (= 400 status))
                   (expect (valid? "error_response" json))
                   (expect (= "invalid-automation" (get-in json ["error" "type"])))
                   (expect (re-find #"/triggers/0/seconds" (get-in json ["error" "message"]))))
                 (let [{:keys [status json]} (call :post "/v1/automations"
                                                   :body ["not" "an" "object"])]
                   (expect (= 400 status))
                   (expect (= "The request body must be a JSON object"
                              (get-in json ["error" "message"])))))))
  (it "answers HTTP 404 when a run starts for an unknown automation"
      (with-db (fn [_]
                 (let [{:keys [status json]} (call :post "/v1/automations/:automation-id/run"
                                                   :params {:automation-id "missing"})]
                   (expect (= 404 status))
                   (expect (= "not-found" (get-in json ["error" "type"])))))))
  (it "sends the query filters to the run list and limits its size"
      (with-db (fn [_]
                 (let [seen (atom nil)]
                   (with-redefs [automation/runs (fn [_ opts]
                                                   (reset! seen opts)
                                                   [])]
                     (let [{:keys [status json]}
                           (call :get "/v1/automations/runs"
                                 :query {"automation_id" "a1" "status" "failed" "limit" "500"})]
                       (expect (= 200 status))
                       (expect (valid? "automations_run_list" json))))
                   (expect (= {:automation-id "a1" :statuses ["failed"] :session-id nil :limit 200}
                              @seen)))))))

(defdescribe
  webhook-route-test
  (it "gives the headers and the body bytes to the webhook check"
      (with-db
        (fn [_]
          (let [seen (atom nil)]
            (with-redefs [runner/accept-webhook!
                          (fn [_ id request]
                            (reset! seen [id (:headers request) (String. ^bytes (:body request))])
                            {:status 202
                             :body {"status" "accepted"
                                    "run_id" "0b5c7c1e-8f8a-4f39-9d55-2f0f3d1b9a10"
                                    "reason" nil}})]
              (let [{:keys [status json]} (call :post "/v1/hooks/:automation-id"
                                                :params {:automation-id "a1"}
                                                :headers {"webhook-id" "msg_1"}
                                                :body (.getBytes "{\"ok\":true}"
                                                                 StandardCharsets/UTF_8))]
                (expect (= 202 status))
                (expect (valid? "automations_webhook_result" json))
                (expect (= ["a1" {"webhook-id" "msg_1"} "{\"ok\":true}"] @seen))))))))
  (it "maps a refused delivery to the error envelope"
      (with-db (fn [_]
                 (with-redefs [runner/accept-webhook!
                               (constantly {:status 401
                                            :error [:unauthorized "The signature is not valid"]})]
                   (let [{:keys [status json]} (call :post "/v1/hooks/:automation-id"
                                                     :params {:automation-id "a1"}
                                                     :body {})]
                     (expect (= 401 status))
                     (expect (valid? "error_response" json))
                     (expect (= "unauthorized" (get-in json ["error" "type"]))))))))
  (it "refuses a body over the limit before the webhook check"
      (with-db (fn [_]
                 (let [calls (atom 0)]
                   (with-redefs [runner/accept-webhook! (fn [& _]
                                                          (swap! calls inc)
                                                          nil)]
                     (let [{:keys [status json]}
                           (call :post "/v1/hooks/:automation-id"
                                 :params {:automation-id "a1"}
                                 :body (byte-array (inc (long (automation/limit
                                                                :webhook_body_bytes)))))]
                       (expect (= 413 status))
                       (expect (= "payload-too-large" (get-in json ["error" "type"])))))
                   (expect (zero? @calls))))))
  (it "answers HTTP 404 for an unknown automation"
      (with-db (fn [_]
                 (let [{:keys [status json]} (call :post "/v1/hooks/:automation-id"
                                                   :params {:automation-id "missing"}
                                                   :body {})]
                   (expect (= 404 status))
                   (expect (valid? "error_response" json)))))))
