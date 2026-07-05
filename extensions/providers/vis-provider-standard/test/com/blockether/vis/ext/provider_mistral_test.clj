(ns com.blockether.vis.ext.provider-mistral-test
  (:require [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.provider-mistral]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe provider-mistral-test
  (it "registers one Mistral provider extension"
    (let [provider (vis/provider-by-id :mistral)]
      (expect (= :mistral (:provider/id provider)))
      (expect (= "Mistral" (:provider/label provider)))
      (expect (= "mistral-large-latest" (first (get-in provider [:provider/preset :default-models])))))))
