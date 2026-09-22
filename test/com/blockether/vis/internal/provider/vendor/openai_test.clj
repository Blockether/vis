(ns com.blockether.vis.internal.provider.vendor.openai-test
  (:require [com.blockether.svar.core :as svar]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.internal.provider.vendor.openai :as sut]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe openai-sol-luna-catalog-test
             (it "exposes Sol and Luna with published Svar metadata through the Vis preset"
                 (let [provider
                       (do (sut/register!) (vis/provider-by-id :openai))

                       defaults
                       (get-in provider [:provider/preset :default-models])

                       router
                       (svar/make-router [(assoc (:provider/preset provider)
                                            :id (:provider/id provider)
                                            :api-key "test"
                                            :models [{:name "gpt-6-sol"} {:name "gpt-6-luna"}])])]

                   (doseq [model (:models (first (:providers router)))]
                     (expect (some #{(:name model)} defaults))
                     (expect (= 922000 (:context model) (:input-limit model)))
                     (expect (= 128000 (:output-limit model)))
                     (expect (= :openai-compatible-responses (:api-style model)))
                     (expect (= :openai-effort (:reasoning-style model)))
                     (expect (= [{:type "effort"
                                  :values ["none" "low" "medium" "high" "xhigh" "max"]}]
                                (:reasoning-options model)))
                     (expect (= #{:chat :vision} (:capabilities model)))
                     (expect (= (if (= "gpt-6-sol" (:name model))
                                  {:input 2.0 :cached-input 0.2 :output 10.0}
                                  {:input 0.1 :cached-input 0.01 :output 0.5})
                                (select-keys (:pricing model) [:input :cached-input :output])))))))

(defdescribe provider-openai-test
             (it "registers one OpenAI provider extension"
                 (let [provider (vis/provider-by-id :openai)]
                   (expect (= :openai (:provider/id provider)))
                   (expect (= "OpenAI" (:provider/label provider)))
                   (expect (= "gpt-6-astra"
                              (first (get-in provider [:provider/preset :default-models])))))))
