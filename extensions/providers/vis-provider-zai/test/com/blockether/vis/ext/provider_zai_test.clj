(ns com.blockether.vis.ext.provider-zai-test
  (:require [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.provider-zai :as zai]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe provider-registration-test
  (it "registers both Z.ai providers with limits and prompt functions"
    (let [coding (vis/provider-by-id :zai-coding)
          pass   (vis/provider-by-id :zai)]
      (expect (= :zai-coding (:provider/id coding)))
      (expect (= :zai (:provider/id pass)))
      (expect (ifn? (:provider/limits-fn coding)))
      (expect (ifn? (:provider/limits-fn pass)))
      (expect (ifn? (:provider/auth-prompt-fn coding)))
      (expect (ifn? (:provider/auth-prompt-fn pass))))))

(defdescribe auth-prompt-test
  (it "exposes static API-key guidance for the coding plan"
    (let [lines ((:provider/auth-prompt-fn (vis/provider-by-id :zai-coding)))]
      (expect (some #(= "  Z.ai (Coding Plan) requires a static API key." %) lines))
      (expect (some #(= "         export ZAI_CODING_API_KEY=<your-zai-api-key>" %) lines))
      (expect (some #(= "  Endpoint: https://api.z.ai/api/coding/paas/v4" %) lines)))))

(defdescribe limits-test
  (it "reports :ok when the coding-plan key is available"
    (with-redefs-fn {#'zai/detect-key (fn [plan-tag]
                                        (when (= :coding plan-tag)
                                          {:api-key "k" :source :auth-file}))}
      (fn []
        (let [report ((:provider/limits-fn (vis/provider-by-id :zai-coding)))]
          (expect (= :zai-coding (:provider-id report)))
          (expect (= :ok (:status report)))
          (expect (= [] (get-in report [:dynamic :limits])))))))

  (it "reports :unauthenticated when the coding-plan key is absent"
    (with-redefs-fn {#'zai/detect-key (constantly nil)}
      (fn []
        (let [report ((:provider/limits-fn (vis/provider-by-id :zai-coding)))]
          (expect (= :zai-coding (:provider-id report)))
          (expect (= :unauthenticated (:status report)))
          (expect (= [] (get-in report [:dynamic :limits]))))))))
