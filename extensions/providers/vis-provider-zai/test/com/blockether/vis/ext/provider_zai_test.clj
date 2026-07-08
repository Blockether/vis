(ns com.blockether.vis.ext.provider-zai-test
  (:require [com.blockether.svar.core :as svar]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.provider-zai :as zai]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe provider-registration-test
             (it "registers both Z.ai plans as separate provider extension entries"
                 (require 'com.blockether.vis.ext.provider-zai :reload)
                 (let [coding
                       (vis/provider-by-id :zai-coding-plan)

                       pass
                       (vis/provider-by-id :zai)

                       ext-nses
                       (set (map :ext/name (vis/registered-extensions)))]

                   (expect (= :zai-coding-plan (:provider/id coding)))
                   (expect (= :zai (:provider/id pass)))
                   (expect (contains? ext-nses "provider-zai"))
                   (expect (= (svar/provider-base-url :zai-coding-plan)
                              (get-in coding [:provider/preset :base-url])))
                   (expect (= (svar/provider-base-url :zai)
                              (get-in pass [:provider/preset :base-url])))
                   (expect (ifn? (:provider/limits-fn coding)))
                   (expect (ifn? (:provider/limits-fn pass)))
                   (expect (ifn? (:provider/auth-prompt-fn coding)))
                   (expect (ifn? (:provider/auth-prompt-fn pass))))))

(defdescribe auth-prompt-test
             (it "exposes static API-key guidance for the coding plan"
                 (require 'com.blockether.vis.ext.provider-zai :reload)
                 (let [lines ((:provider/auth-prompt-fn (vis/provider-by-id :zai-coding-plan)))]
                   (expect (some #(= "  Z.ai (Coding Plan) requires a static API key." %) lines))
                   (expect (some #(= "         export ZAI_CODING_API_KEY=<your-zai-api-key>" %)
                                 lines))
                   (expect (some #(= (str "  Endpoint: " (svar/provider-base-url :zai-coding-plan)) %) lines)))))

(defdescribe
  auth-detection-test
  (it "detects the TUI/config API key used by runtime model calls"
      (require 'com.blockether.vis.ext.provider-zai :reload)
      (with-redefs-fn {#'zai/load-auth-file (constantly nil)
                       #'zai/env-key-for-plan (constantly nil)
                       #'vis/current-config (constantly {:providers [{:id :zai-coding-plan
                                                                      :api-key "config-key"}]})}
        (fn []
          (expect (= {:api-key "config-key" :source :config} (#'zai/detect-key :coding)))))))

(defdescribe
  limits-test
  (it "reports live 5h and 7d coding-plan quota when the coding-plan key is available"
      (require 'com.blockether.vis.ext.provider-zai :reload)
      (with-redefs-fn {#'zai/detect-key (fn [plan-tag]
                                          (when (= :coding plan-tag)
                                            {:api-key "k" :source :auth-file}))
                       #'zai/fetch-quota! (fn [api-key]
                                            (expect (= "k" api-key))
                                            {:data {:level "pro"
                                                    :limits [{:type "TOKENS_LIMIT"
                                                              :unit 3
                                                              :number 5
                                                              :usage 800000000
                                                              :currentValue 200000000
                                                              :remaining 600000000
                                                              :percentage 25
                                                              :nextResetTime 1770648402389}
                                                             {:type "TOKENS_LIMIT"
                                                              :unit 6
                                                              :number 7
                                                              :usage 2000000000
                                                              :currentValue 1000000000
                                                              :remaining 1000000000
                                                              :percentage 50
                                                              :nextResetTime 1770848402389}]}})}
        (fn []
          (let [report (vis/provider-limits :zai-coding-plan)]
            (expect (= :zai-coding-plan (:provider-id report)))
            (expect (= :ok (:status report)))
            (expect (= [:zai-coding-plan-5h :zai-coding-plan-7d]
                       (mapv :id (get-in report [:dynamic :limits]))))
            (expect (= 25.0 (get-in report [:dynamic :limits 0 :used])))
            (expect (= 100.0 (get-in report [:dynamic :limits 0 :limit])))
            (expect (= 75.0 (get-in report [:dynamic :limits 0 :remaining])))
            (expect (= {:kind :rolling :unit :hour :size 5 :resets-at-ms 1770648402389}
                       (get-in report [:dynamic :limits 0 :window])))
            (expect (= {:kind :rolling :unit :day :size 7 :resets-at-ms 1770848402389}
                       (get-in report [:dynamic :limits 1 :window])))))))
  (it "reports :unauthenticated when the coding-plan key is absent"
      (require 'com.blockether.vis.ext.provider-zai :reload)
      (with-redefs-fn {#'zai/detect-key (constantly nil)}
        (fn []
          (let [report ((:provider/limits-fn (vis/provider-by-id :zai-coding-plan)))]
            (expect (= :zai-coding-plan (:provider-id report)))
            (expect (= :unauthenticated (:status report)))
            (expect (= [] (get-in report [:dynamic :limits]))))))))
