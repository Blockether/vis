(ns com.blockether.vis.internal.gateway.agents-test
  (:require [charred.api :as json]
            [com.blockether.vis.contract.gateway :as contract]
            [com.blockether.vis.internal.gateway.server :as server]
            [com.blockether.vis.internal.gateway.wiring :as wiring]
            [com.blockether.vis.internal.loop :as lp]
            [com.blockether.vis.internal.loop.router :as loop-router]
            [com.blockether.vis.internal.session.model :as smodel]
            [com.blockether.vis.internal.persistance.core :as ps]
            [com.blockether.vis.internal.persistance.sqlite.test-helpers :as h]
            [lazytest.core :refer [defdescribe expect it]]
            [reitit.core :as r])
  (:import [java.io ByteArrayInputStream]))

(wiring/install!)

(h/use-mem-store! {"subagents" true})

(defn- request
  [method path body]
  (let [match
        (r/match-by-path (#'server/router "test-token" []) path)

        handler
        (get-in match [:data method :handler])]

    (expect (some? handler))
    (handler {:request-method method
              :uri path
              :path-params (:path-params match)
              :body (when body (ByteArrayInputStream. (.getBytes ^String body "UTF-8")))})))

(defdescribe canonical-agent-routes-and-validation-test
             (it "canonical agent routes and validation"
                 (doseq [[path methods]
                         [["/v1/sessions/:sid/agents" [:get :post]]
                          ["/v1/sessions/:sid/agents/cancel" [:post]]
                          ["/v1/sessions/:sid/agents/route" [:post]]]

                         method
                         methods]

                   (expect (some? (contract/operation method path))))
                 (let [db
                       (h/store)

                       leader
                       (str (h/store-session! db {:channel :api}))

                       path
                       (str "/v1/sessions/" leader "/agents")]

                   (with-redefs [lp/db-info
                                 (constantly db)

                                 lp/env-for
                                 (fn [_]
                                   (expect false "Listing a team must not bootstrap a session"))]

                     (expect (= [] (json/read-json (:body (request :get path nil)))))
                     (doseq [body ["[]" "not json" "{}" (json/write-json-str {:task " "})
                                   "{\"task\":\"Work\",\"iteration_budget\":0}"]]
                       (expect (= 400 (:status (request :post path body))) body))
                     (expect (= 404 (:status (request :get "/v1/sessions/unknown/agents" nil))))))))

(defdescribe
  routing-is-local-and-respects-human-locks-through-http-test
  (it
    "routing is local and respects human locks through http"
    (let [db
          (h/store)

          leader
          (str (h/store-session! db {:channel :api}))

          other
          (str (h/store-session! db {:channel :api}))

          path
          (str "/v1/sessions/" leader "/agents/route")

          router
          {:providers [{:id :p :models [{:name "small"} {:name "large"}] :root "large"}]}]

      (with-redefs [lp/db-info
                    (constantly db)

                    loop-router/get-router
                    (constantly router)

                    lp/env-for
                    (fn [_]
                      (expect false "Routing a team must not bootstrap a session"))]

        (expect (= 200 (:status (request :post path "{\"provider\":\"p\",\"model\":\"small\"}"))))
        (expect (= {:provider "p" :model "small"} (smodel/model-of db leader)))
        (expect (nil? (smodel/model-of db other)))
        (expect (= 409
                   (:status (request :post
                                     path
                                     (json/write-json-str
                                       {:provider "p" :model "large" :session_id other})))))
        (smodel/set-model! db leader "p" "small")
        (expect (ps/db-routing-locked? db leader) "An idempotent human pick locks agent routing")
        (expect (= 409
                   (:status (request :post path "{\"provider\":\"p\",\"model\":\"large\"}"))))))))
