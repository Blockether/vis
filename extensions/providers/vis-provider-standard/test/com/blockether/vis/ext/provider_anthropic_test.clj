(ns com.blockether.vis.ext.provider-anthropic-test
  (:require [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.provider-anthropic]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe provider-anthropic-test
  (it "registers one Anthropic provider extension"
    (let [provider (vis/provider-by-id :anthropic)]
      (expect (= :anthropic (:provider/id provider)))
      (expect (= "Anthropic" (:provider/label provider)))
      (expect (= "claude-opus-4-6" (first (get-in provider [:provider/preset :default-models])))))))
