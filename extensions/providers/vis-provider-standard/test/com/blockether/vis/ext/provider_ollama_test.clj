(ns com.blockether.vis.ext.provider-ollama-test
  (:require [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.provider-ollama]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe provider-ollama-test
             (it "registers one Ollama provider extension"
                 (let
                   [provider
                    (vis/provider-by-id :ollama)

                    status
                    ((:provider/status-fn provider))]

                   (expect (= :ollama (:provider/id provider)))
                   (expect (= "Ollama" (:provider/label provider)))
                   (expect (= "http://localhost:11434/v1" (:base-url status))))))
