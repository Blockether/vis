(ns com.blockether.vis.internal.foundation.mcp.core-test
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.config :as config]
            [com.blockether.vis.internal.foundation.mcp.client :as client]
            [com.blockether.vis.internal.foundation.mcp.core :as mcp]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe mcp-native-contract-test
             (it "keeps native/Python alias routing in each compact description"
                 (let [symbols (get-in mcp/vis-extension [:ext/engine :ext.engine/symbols])]
                   (doseq [s symbols]
                     (let [description (:ext.symbol/description s)]
                       (expect (str/includes? description "In `python_execution`"))
                       (expect (< (count description) 350))))))
             (it "closes the dispatcher schemas while leaving MCP tool args open"
                 (let
                   [symbols
                    (get-in mcp/vis-extension [:ext/engine :ext.engine/symbols])

                    call
                    (first (filter #(= "mcp__call" (:ext.symbol/name %)) symbols))]

                   (doseq [s symbols]
                     (expect (false? (get-in s [:ext.symbol/schema :additionalProperties]))))
                   (expect (= "object"
                              (get-in call [:ext.symbol/schema :properties "args" :type]))))))

(defdescribe
  gateway-mcp-management-test
  (it "persists gateway server values while returning only a sanitized inventory row"
      (let
        [saved
         (atom nil)

         reconnect!
         (ns-resolve 'com.blockether.vis.internal.foundation.mcp.core 'reconcile-async!)]

        (with-redefs-fn {#'config/load-global-config-raw (constantly {})
                         #'config/save-config! (fn [value source]
                                                 (reset! saved [value source]))
                         reconnect! (constantly nil)}
          (fn []
            (let
              [row (mcp/save-gateway-server! "filesystem"
                                             {"transport" "stdio"
                                              "command" "npx"
                                              "env" {"API_TOKEN" "not-for-the-client"}})]
              (expect (= "filesystem" (:name row)))
              (expect (= "stdio" (:transport row)))
              (expect (nil? (:env row)))
              (expect (nil? (:headers row)))
              (expect (= :gateway-mcp (second @saved)))
              (expect (= "not-for-the-client"
                         (get-in (first @saved)
                                 ["mcp" "servers" "filesystem" "env" "API_TOKEN"]))))))))
  (it "tests a candidate without persisting it and always closes its connection"
      (let
        [seen
         (atom nil)

         closed
         (atom nil)]

        (with-redefs
          [client/connect
           (fn [name spec]
             (reset! seen [name spec])
             ::connection)

           client/list-tools
           (constantly [{"name" "list_files" "description" "Enumerate files"}])

           client/close
           (fn [conn]
             (reset! closed conn))]

          (let
            [result (mcp/test-gateway-server! "remote"
                                              {"transport" "streamable_http"
                                               "url" "https://mcp.example.test/mcp"
                                               "headers" {"Authorization" "Bearer private"}})]
            (expect (= "remote" (:name result)))
            (expect (= [{:name "list_files" :description "Enumerate files"}] (:tools result)))
            (expect (nil? (:headers result)))
            (expect (= ::connection @closed))
            (expect (= :streamable-http (get-in @seen [1 :transport])))
            (expect (= "Bearer private" (get-in @seen [1 :headers "Authorization"]))))))))
