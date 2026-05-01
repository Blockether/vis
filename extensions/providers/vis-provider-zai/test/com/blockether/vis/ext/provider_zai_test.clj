(ns com.blockether.vis.ext.provider-zai-test
  (:require [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.provider-zai :as zai]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe provider-registration-test
  (it "registers both Z.ai providers with limits functions"
    (let [coding (vis/provider-by-id :zai-coding)
          pass   (vis/provider-by-id :zai)]
      (expect (= :zai-coding (:provider/id coding)))
      (expect (= :zai (:provider/id pass)))
      (expect (ifn? (:provider/limits-fn coding)))
      (expect (ifn? (:provider/limits-fn pass))))))

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
