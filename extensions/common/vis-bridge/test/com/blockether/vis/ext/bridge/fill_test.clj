(ns com.blockether.vis.ext.bridge.fill-test
  (:require
   [com.blockether.vis.ext.bridge.fill :as fill]
   [com.blockether.vis.ext.bridge.schema :as schema]
   [lazytest.core :refer [defdescribe expect it]]))

(def sample-result
  (schema/extract-result
    {:nodes [(schema/node
               {:kind :function
                :language "clojure"
                :name "run"
                :qualified-name "demo.core/run"
                :path "src/demo/core.clj"})]
     :edges [(schema/edge
               {:edge-kind :calls
                :source "demo.core/run"
                :target "demo.core/parse"
                :path "src/demo/core.clj"
                :language "clojure"})]
     :diagnostics []
     :stats {:language "clojure" :path "src/demo/core.clj"}}))

(defdescribe bridge-fill-test
  (it "maps normalized facts to aggregate rows consistently"
    (let [rows (fill/rows-for-result sample-result)
          by-kind (group-by :kind rows)]
      (expect (= #{:bridge/node :bridge/edge :bridge/index} (set (keys by-kind))))
      (expect (= "node:demo.core/run" (:key (first (:bridge/node by-kind)))))
      (expect (= "edge:demo.core/run::calls::demo.core/parse" (:key (first (:bridge/edge by-kind)))))
      (expect (= {:path "src/demo/core.clj"
                  :language "clojure"
                  :kind "function"
                  :name "run"
                  :visibility "unknown"}
                (:metadata (first (:bridge/node by-kind)))))))

  (it "can override index path explicitly"
    (let [idx (first (filter #(= :bridge/index (:kind %))
                       (fill/rows-for-result sample-result {:path "override.clj"})))]
      (expect (= "idx:override.clj" (:key idx)))
      (expect (= "override.clj" (get-in idx [:metadata :path]))))))
