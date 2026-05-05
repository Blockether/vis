(ns com.blockether.vis.ext.provider-lmstudio-test
  (:require [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.provider-lmstudio]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe provider-lmstudio-test
  (it "registers one LM Studio provider extension"
    (let [provider (vis/provider-by-id :lmstudio)
          status ((:provider/status-fn provider))]
      (expect (= :lmstudio (:provider/id provider)))
      (expect (= "LM Studio" (:provider/label provider)))
      (expect (= "http://localhost:1234/v1" (:base-url status))))))
