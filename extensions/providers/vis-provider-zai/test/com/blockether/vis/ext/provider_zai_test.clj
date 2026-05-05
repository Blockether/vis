(ns com.blockether.vis.ext.provider-zai-test
  (:require [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.provider-zai :as zai]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe provider-registration-test
  (it "registers both Z.ai plans as separate provider extension entries"
    (require 'com.blockether.vis.ext.provider-zai :reload)
    (let [coding (vis/provider-by-id :zai-coding)
          pass   (vis/provider-by-id :zai)
          ext-nses (set (map :ext/namespace (vis/registered-extensions)))]
      (expect (= :zai-coding (:provider/id coding)))
      (expect (= :zai (:provider/id pass)))
      (expect (contains? ext-nses 'com.blockether.vis.ext.provider-zai.coding))
      (expect (contains? ext-nses 'com.blockether.vis.ext.provider-zai.pass))
      (expect (= "https://api.z.ai/api/coding/paas/v4" (get-in coding [:provider/preset :base-url])))
      (expect (= "https://api.z.ai/api/paas/v4" (get-in pass [:provider/preset :base-url])))
      (expect (ifn? (:provider/limits-fn coding)))
      (expect (ifn? (:provider/limits-fn pass)))
      (expect (ifn? (:provider/auth-prompt-fn coding)))
      (expect (ifn? (:provider/auth-prompt-fn pass))))))

(defdescribe auth-prompt-test
  (it "exposes static API-key guidance for the coding plan"
    (require 'com.blockether.vis.ext.provider-zai :reload)
    (let [lines ((:provider/auth-prompt-fn (vis/provider-by-id :zai-coding)))]
      (expect (some #(= "  Z.ai (Coding Plan) requires a static API key." %) lines))
      (expect (some #(= "         export ZAI_CODING_API_KEY=<your-zai-api-key>" %) lines))
      (expect (some #(= "  Endpoint: https://api.z.ai/api/coding/paas/v4" %) lines)))))

(defdescribe limits-test
  (it "reports :ok when the coding-plan key is available"
    (require 'com.blockether.vis.ext.provider-zai :reload)
    (with-redefs-fn {#'zai/detect-key (fn [plan-tag]
                                        (when (= :coding plan-tag)
                                          {:api-key "k" :source :auth-file}))}
      (fn []
        (let [report ((:provider/limits-fn (vis/provider-by-id :zai-coding)))]
          (expect (= :zai-coding (:provider-id report)))
          (expect (= :ok (:status report)))
          (expect (= [] (get-in report [:dynamic :limits])))))))

  (it "reports :unauthenticated when the coding-plan key is absent"
    (require 'com.blockether.vis.ext.provider-zai :reload)
    (with-redefs-fn {#'zai/detect-key (constantly nil)}
      (fn []
        (let [report ((:provider/limits-fn (vis/provider-by-id :zai-coding)))]
          (expect (= :zai-coding (:provider-id report)))
          (expect (= :unauthenticated (:status report)))
          (expect (= [] (get-in report [:dynamic :limits]))))))))
