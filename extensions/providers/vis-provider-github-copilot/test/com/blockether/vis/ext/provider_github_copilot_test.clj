(ns com.blockether.vis.ext.provider-github-copilot-test
  (:require [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.provider-github-copilot]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe provider-registration-test
  (it "registers GitHub Copilot through the provider extension contract"
    (let [provider (vis/provider-by-id :github-copilot)]
      (expect (= :github-copilot (:provider/id provider)))
      (expect (ifn? (:provider/status-fn provider)))
      (expect (ifn? (:provider/logout-fn provider)))
      (expect (ifn? (:provider/detect-fn provider)))
      (expect (ifn? (:provider/auth-fn provider)))
      (expect (ifn? (:provider/get-token-fn provider))))))
