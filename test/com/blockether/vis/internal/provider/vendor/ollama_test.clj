(ns com.blockether.vis.internal.provider.vendor.ollama-test
  (:require [com.blockether.vis.internal.extension.registry :as registry]
            [com.blockether.vis.internal.provider.vendor.ollama]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe provider-ollama-test
             (it "registers one Ollama provider extension"
                 (let [provider
                       (registry/provider-by-id :ollama)

                       status
                       ((:provider/status-fn provider))]

                   (expect (= :ollama (:provider/id provider)))
                   (expect (false? (:is-authenticated status)))
                   (expect (nil? (:authenticated? status)))
                   (expect (= "http://localhost:11434/v1" (:base-url status)))))
             (it "retains svar's ordinary network defaults"
                 (expect (nil? (get-in (registry/provider-by-id :ollama)
                                       [:provider/preset :network])))))
