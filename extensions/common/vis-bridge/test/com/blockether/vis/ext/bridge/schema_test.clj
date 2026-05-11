(ns com.blockether.vis.ext.bridge.schema-test
  (:require
   [com.blockether.vis.ext.bridge.schema :as schema]
   [lazytest.core :refer [defdescribe expect it]]))

(defdescribe bridge-schema-test
  (it "validates normalized extraction results"
    (let [result (schema/extract-result
                   {:nodes [(schema/node
                              {:kind :file
                               :language "markdown"
                               :name "README.md"
                               :qualified-name "doc:README.md"
                               :path "README.md"})]
                    :edges [(schema/edge
                              {:edge-kind :contains
                               :source "repo"
                               :target "doc:README.md"
                               :path "README.md"})]
                    :diagnostics []
                    :stats {:language "markdown"}})]
      (expect (schema/valid-extract-result? result))
      (expect (= 1 (count (:nodes result)))))))
