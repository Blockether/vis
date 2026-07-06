(ns com.blockether.vis.ext.provider-openai-test
  (:require [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.provider-openai]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe provider-openai-test
             (it "registers one OpenAI provider extension"
                 (let [provider (vis/provider-by-id :openai)]
                   (expect (= :openai (:provider/id provider)))
                   (expect (= "OpenAI" (:provider/label provider)))
                   (expect (= "gpt-5"
                              (first (get-in provider [:provider/preset :default-models])))))))
