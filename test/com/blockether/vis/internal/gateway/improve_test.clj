(ns com.blockether.vis.internal.gateway.improve-test
  (:require [charred.api :as json]
            [com.blockether.vis.contract.gateway :as contract]
            [com.blockether.vis.internal.config.improve :as settings]
            [com.blockether.vis.internal.gateway.server :as server]
            [com.blockether.vis.internal.improve.core :as improve]
            [com.blockether.vis.internal.improve.review :as review]
            [com.blockether.vis.internal.loop :as lp]
            [com.blockether.vis.internal.persistance.sqlite.test-helpers :as h]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is]]
            [reitit.core :as r])
  (:import (java.io ByteArrayInputStream)))

(h/use-mem-store! {"improve" true})

(defn- request
  [method path body query]
  (let [match
        (r/match-by-path (#'server/router "test-token" []) path)

        handler
        (get-in match [:data method :handler])]

    (is (some? handler) (str method " " path))
    (handler {:request-method method
              :uri path
              :path-params (:path-params match)
              :query-params query
              :body (when body (ByteArrayInputStream. (.getBytes ^String body "UTF-8")))})))

(defn- body [response] (json/read-json (:body response)))

(deftest canonical-machine-routes
  (doseq [[path methods]
          [["/v1/improve" [:get :post]] ["/v1/improve/settings" [:get :patch]]
           ["/v1/improve/review" [:post]] ["/v1/improve/:id" [:get :patch]]]

          method
          methods]

    (is (some? (contract/operation method path)))))

(deftest record-crud-and-pagination-through-route-handlers
  (let [db (h/store)]
    (with-redefs [lp/db-info (constantly db)]
      (let [a (body (request :post "/v1/improve" "{\"title\":\"First\",\"content\":\"Human\"}" {}))
            b (body (request :post "/v1/improve" "{\"title\":\"Second\"}" {}))
            page (body (request :get "/v1/improve" nil {"project_id" "" "limit" "1"}))
            path (str "/v1/improve/" (get a "id"))]

        (is (= [(get a "id")] (mapv #(get % "id") (get page "records"))))
        (is (true? (get page "has_more")))
        (is (= [(get b "id")]
               (mapv #(get % "id")
                     (get (body (request :get "/v1/improve" nil {"after" (str (get page "after"))}))
                          "records"))))
        (is (= "First" (get (body (request :get path nil {})) "title")))
        (is (= 200
               (:status (request :patch path
                                 "{\"content\":\"Edited\",\"expected_version\":1}" {}))))
        (is (= 409
               (:status (request :patch path "{\"content\":\"Stale\",\"expected_version\":1}" {}))))
        (is (= "Edited" (:content (improve/get-record db (get a "id")))))
        (is (= 400 (:status (request :get "/v1/improve" nil {"limit" "bad"}))))
        (is (= 400 (:status (request :get "/v1/improve" nil {"limit" "201"}))))
        (is (= 400 (:status (request :post "/v1/improve" "[]" {}))))
        (is (= 400 (:status (request :post "/v1/improve" "not json" {}))))
        (is (= 404 (:status (request :get "/v1/improve/999999" nil {}))))))))

(deftest settings-and-manual-review-route-boundaries
  (with-redefs [lp/db-info
                (constantly (h/store))

                settings/settings
                (constantly {:mode "human" :provider nil :model nil :interval_minutes 60})

                settings/snapshot
                (constantly {:settings {:mode "human"}})]

    (is (= "human" (get (body (request :get "/v1/improve/settings" nil {})) "mode")))
    (is (= 409 (:status (request :post "/v1/improve/review" nil {}))))
    (let [called (atom nil)]
      (with-redefs [settings/update-settings! (fn [attrs]
                                                (reset! called attrs)
                                                attrs)]
        (is (= 200
               (:status (request :patch "/v1/improve/settings"
                                 "{\"mode\":\"automatic\",\"provider\":\"p\",\"model\":\"m\"}"
                                 {}))))
        (is (= {:mode "automatic" :provider "p" :model "m"} @called))))
    (with-redefs [review/run! (fn [_]
                                {:projects [] :reproduction "not_attempted"})]
      (is (= "not_attempted"
             (get (body (request :post "/v1/improve/review" nil {})) "reproduction"))))))
