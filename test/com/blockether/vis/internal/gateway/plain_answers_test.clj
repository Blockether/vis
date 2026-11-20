;; The answers of these routes need no model, engine turn or outside service.
;; The `response-check/answers` hook checks each answer against its schema, and the
;; coverage check counts these operations (#487).
(ns com.blockether.vis.internal.gateway.plain-answers-test
  (:require [lazytest.core :refer [defdescribe expect it]]
            [com.blockether.vis.contract.gateway :as gateway-contract]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.internal.gateway.server :as server]
            [com.blockether.vis.internal.gateway.server.instance :as instance]
            [com.blockether.vis.internal.gateway.state :as state]
            [com.blockether.vis.internal.util :as util])
  (:import [java.io ByteArrayInputStream]
           [java.util UUID]))

(defn- request
  [method uri body]
  {:request-method method
   :uri uri
   :headers {"content-type" "application/json"
             "x-vis-client-id" "owner"
             "x-vis-protocol" (str gateway-contract/protocol-version)
             "x-vis-min-gateway-protocol" (str gateway-contract/minimum-gateway-protocol)}
   :body (ByteArrayInputStream. (.getBytes ^String (wire/json-str body) "UTF-8"))})

(defn- answer
  [response]
  [(:status response)
   (let [body (:body response)]
     (wire/parse-json (if (string? body) body (slurp body))))])

(defdescribe
  status-routes-answer-their-schema
  (it "health, readiness, client release and drain answer their schema"
      (let [before
            @instance/server-state

            now
            (util/now-ms)

            handler
            (#'server/app "" [])]

        (reset! instance/server-state {:require-token? false
                                       :managed? false
                                       :clients {"owner" {:connected-at now :last-seen-at now}}})
        (try (expect (= 200 (first (answer (handler (request :get "/healthz" {}))))))
             (expect (= 200 (first (answer (handler (request :get "/readyz" {}))))))
             (let [[status body] (answer (handler (request :delete "/v1/clients/owner" {})))]
               (expect (= 200 status))
               (expect (true? (get body "released"))))
             (let [[status body] (answer (handler (request :post
                                                           "/v1/admin/drain"
                                                           {"timeout_ms" 0 "grace_ms" 0})))]
               (expect (= 200 status))
               (expect (= {"parked" 0 "forced" 0 "running" 0} body)))
             (finally (state/end-drain!) (reset! instance/server-state before))))))

(defdescribe
  client-extension-detach-answers-an-empty-object
  (it "client extension detach answers an empty object"
      (let [before
            @instance/server-state

            now
            (util/now-ms)

            sid
            (str (UUID/randomUUID))

            handler
            (#'server/app "" [])]

        (reset! instance/server-state {:require-token? false
                                       :managed? false
                                       :clients {"owner" {:connected-at now :last-seen-at now}}})
        (try (with-redefs [state/soul (fn [id]
                                        (when (= sid (str id)) {"id" sid}))]
               (expect (= [200 {}]
                          (answer (handler (request :delete
                                                    (str "/v1/sessions/" sid "/client-extensions")
                                                    {}))))))
             (finally (reset! instance/server-state before))))))
