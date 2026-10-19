(ns com.blockether.vis.internal.provider.vendor.mistral-test
  (:require [com.blockether.vis.internal.extension.registry :as registry]
            [com.blockether.vis.internal.provider.vendor.mistral]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe provider-mistral-test
             (it "registers one Mistral provider extension"
                 (let [provider (registry/provider-by-id :mistral)]
                   (expect (= :mistral (:provider/id provider)))
                   (expect (= "Mistral" (:provider/label provider)))
                   (expect (= "mistral-large-latest"
                              (first (get-in provider [:provider/preset :default-models])))))))
