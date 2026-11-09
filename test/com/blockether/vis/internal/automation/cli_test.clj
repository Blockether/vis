(ns com.blockether.vis.internal.automation.cli-test
  "`vis-agent automations` against the real gateway router and an in-memory store."
  (:require [clojure.string :as str]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.internal.automation.cli :as cli]
            [com.blockether.vis.internal.automation.core :as automation]
            [com.blockether.vis.internal.gateway.client :as gateway-client]
            [com.blockether.vis.internal.loop :as lp]
            [com.blockether.vis.internal.persistance.sqlite.core :as ps]
            [com.blockether.vis.internal.util :as util]
            [lazytest.core :refer [defdescribe expect it]]
            [reitit.ring :as reitit])
  (:import (java.io ByteArrayInputStream)
           (java.nio.charset StandardCharsets)))

(def ^:private definition
  {"name" "Morning report"
   "triggers" [{"kind" "every" "seconds" 3600}]
   "prompt" "Summarize the open pull requests."
   "target" {"mode" "temporary"}})

(defn- with-gateway
  "Call `f` with `gateway-client/request!` answered by the real router over an
   in-memory store."
  [f]
  (let [store
        (ps/db-open! :memory)

        server
        #(requiring-resolve (symbol "com.blockether.vis.internal.gateway.server" (name %)))

        app
        ((server 'wrap-scoped-params) (reitit/ring-handler ((server 'router) nil [])) [])]

    (try (with-redefs [lp/db-info
                       (constantly store)

                       gateway-client/request!
                       (fn [method path {:keys [body]}]
                         (app {:request-method method
                               :uri path
                               :headers {"content-type" "application/json"}
                               :body (when (some? body)
                                       (ByteArrayInputStream. (.getBytes
                                                                (wire/json-str body)
                                                                StandardCharsets/UTF_8)))}))]

           (f store))
         (finally (ps/db-close! store)))))

(defn- stored [store] (automation/list-all store (util/now-ms)))

(defdescribe
  automations-command-line-test
  (it "creates one automation when a setup script runs create twice"
      (with-gateway (fn [store]
                      (let [[first-outcome created]
                            (cli/upsert! definition)

                            [second-outcome updated]
                            (cli/upsert! (assoc definition "prompt" "List the open issues."))]

                        (expect (= :created first-outcome))
                        (expect (= :updated second-outcome))
                        (expect (= (get created "id") (get updated "id")))
                        (expect (= ["List the open issues."]
                                   (mapv #(get % "prompt") (stored store))))))))
  (it "leaves an automation of an extension alone and creates the person's own"
      (with-gateway (fn [store]
                      (automation/sync-extensions! store
                                                   nil
                                                   [{:ext/name "reports"
                                                     :ext/automations
                                                     [{:key "morning"
                                                       :name "Morning report"
                                                       :doc "Report from the extension."
                                                       :triggers [{"kind" "every" "seconds" 3600}]
                                                       :session "temporary"
                                                       :push false
                                                       :enabled true}]}]
                                                   (util/now-ms))
                      (expect (= :created (first (cli/upsert! definition))))
                      (expect (= #{"Report from the extension." "Summarize the open pull requests."}
                                 (set (map #(get % "prompt") (stored store))))))))
  (it "refuses a create when two automations already have the name"
      (with-gateway
        (fn [store]
          (automation/create! store definition (util/now-ms))
          (automation/create! store definition (util/now-ms))
          (let [e (try (cli/upsert! definition) nil (catch clojure.lang.ExceptionInfo e e))]
            (expect (:vis/user-error (ex-data e)))
            (expect (str/starts-with? (ex-message e) "2 automations have the name"))
            (expect (= 2 (count (stored store))))))))
  (it "shows the gateway message when the gateway refuses a definition"
      (with-gateway (fn [_]
                      (let [e (try (cli/upsert! (dissoc definition "prompt"))
                                   nil
                                   (catch clojure.lang.ExceptionInfo e e))]
                        (expect (:vis/user-error (ex-data e)))
                        (expect (not (str/starts-with? (ex-message e) "The gateway answered HTTP"))
                                (ex-message e)))))))
