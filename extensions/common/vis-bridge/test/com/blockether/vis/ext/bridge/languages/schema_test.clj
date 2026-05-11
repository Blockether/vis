(ns com.blockether.vis.ext.bridge.languages.schema-test
  (:require
   [com.blockether.vis.ext.bridge.languages.schema :as schema]
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
      (expect (= 1 (count (:nodes result))))))

  (it "validates aggregate rows in the same core schema namespace"
    (expect (schema/valid-aggregate-rows?
              [{:key "node:doc:README.md"
                :kind :bridge/node
                :scope :global
                :metadata {:path "README.md"}
                :content {:kind :file}}]))
    (expect (not (schema/valid-aggregate-rows?
                   [{:key "bad"
                     :kind :not-bridge
                     :scope :global
                     :metadata {}
                     :content {}}])))))
