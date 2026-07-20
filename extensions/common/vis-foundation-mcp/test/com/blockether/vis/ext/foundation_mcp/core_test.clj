(ns com.blockether.vis.ext.foundation-mcp.core-test
  (:require [clojure.string :as str]
            [com.blockether.vis.ext.foundation-mcp.core :as mcp]
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
