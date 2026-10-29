(ns com.blockether.vis.internal.provider.vendor.zai-test
  (:require [babashka.http-client :as http]
            [com.blockether.vis.internal.extension.registry :as registry]
            [com.blockether.vis.internal.provider.limits :as provider-limits]
            [com.blockether.vis.internal.config.core :as config]
            [com.blockether.vis.internal.provider.key-store :as key-store]
            [com.blockether.vis.internal.provider.vendor.zai :as zai]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe
  auth-detection-test
  (it "detects the TUI/config API key used by runtime model calls"
      (require 'com.blockether.vis.internal.provider.vendor.zai :reload)
      (with-redefs-fn {#'config/current-config (constantly {:providers [{:id :zai-coding-plan
                                                                         :api-key "config-key"}]})}
        (fn []
          (expect (= {:api-key "config-key" :source :config}
                     ((:provider/detect-fn (registry/provider-by-id :zai-coding-plan)))))))))

(defdescribe
  limits-test
  (it "reports live 5h and 7d coding-plan quota when the coding-plan key is available"
      (require 'com.blockether.vis.internal.provider.vendor.zai :reload)
      (with-redefs-fn {#'key-store/detect-key (fn [_book plan-tag]
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
          (let [report (provider-limits/provider-limits :zai-coding-plan)]
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
      (require 'com.blockether.vis.internal.provider.vendor.zai :reload)
      (with-redefs-fn {#'key-store/detect-key (constantly nil)}
        (fn []
          (let [report ((:provider/limits-fn (registry/provider-by-id :zai-coding-plan)))]
            (expect (= :zai-coding-plan (:provider-id report)))
            (expect (= :unauthenticated (:status report)))
            (expect (= [] (get-in report [:dynamic :limits])))))))
  (it "treats an application-level 401 quota response as rejected credentials"
      (require 'com.blockether.vis.internal.provider.vendor.zai :reload)
      (with-redefs-fn
        {#'key-store/detect-key (fn [_book plan-tag]
                                  (when (= :coding plan-tag)
                                    {:api-key "rejected-key" :source :config}))
         #'http/get (fn [_url _request]
                      {:status 200
                       :body
                       "{\"success\":false,\"code\":401,\"msg\":\"token expired or incorrect\"}"})}
        (fn []
          (let [report ((:provider/limits-fn (registry/provider-by-id :zai-coding-plan)))]
            (expect (= :unauthenticated (:status report)))
            (expect (= [] (get-in report [:dynamic :limits])))
            (expect (=
                      "Z.ai (Coding Plan) rejected the current API key: token expired or incorrect."
                      (get-in report [:dynamic :note]))))))))

(defdescribe pass-limits-test
             (it "keeps a saved Pass key unverified when no live quota endpoint exists"
                 (require 'com.blockether.vis.internal.provider.vendor.zai :reload)
                 (with-redefs-fn {#'key-store/detect-key (fn [_book plan-tag]
                                                           (when (= :pass plan-tag)
                                                             {:api-key "saved-key"
                                                              :source :config}))}
                   (fn []
                     (let [report ((:provider/limits-fn (registry/provider-by-id :zai)))]
                       (expect (= :unsupported (:status report)))
                       (expect (= [] (get-in report [:dynamic :limits]))))))))
