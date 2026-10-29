(ns com.blockether.vis.internal.provider.vendor.alibaba-test
  (:require [com.blockether.vis.internal.extension.registry :as registry]
            [com.blockether.vis.internal.provider.limits :as provider-limits]
            [com.blockether.vis.internal.config.core :as config]
            [com.blockether.vis.internal.provider.key-store :as key-store]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe
  auth-detection-test
  ;; The lookup ORDER, the plan isolation and the token envelope belong to the
  ;; shared key store (`provider-key-store-test`); what this pack owns is that
  ;; its two plans are wired to their OWN provider ids.
  (it "detects the TUI/config API key used by runtime model calls"
      (require 'com.blockether.vis.internal.provider.vendor.alibaba :reload)
      (with-redefs-fn {#'config/current-config (constantly {:providers [{:id :alibaba-token-plan
                                                                         :api-key "config-key"}]})}
        (fn []
          (expect (= {:api-key "config-key" :source :config}
                     ((:provider/detect-fn (registry/provider-by-id :alibaba-token-plan)))))
          ;; Plan-scoped: the Token Plan key must never authenticate the Coding
          ;; Plan, which the live endpoints reject with 401.
          (expect (not= "config-key"
                        (:api-key ((:provider/detect-fn (registry/provider-by-id
                                                          :alibaba-coding-plan))))))))))

(defdescribe
  limits-test
  (it "reports :unsupported with a console note - no endpoint verifies the key"
      (require 'com.blockether.vis.internal.provider.vendor.alibaba :reload)
      (with-redefs-fn {#'key-store/detect-key (constantly {:api-key "k" :source :auth-file})}
        (fn []
          (let [report (provider-limits/provider-limits :alibaba-token-plan)]
            (expect (= :alibaba-token-plan (:provider-id report)))
            (expect (= :unsupported (:status report)))
            (expect (= [] (get-in report [:dynamic :limits])))))))
  (it "reports :unauthenticated when the plan key is absent"
      (require 'com.blockether.vis.internal.provider.vendor.alibaba :reload)
      (with-redefs-fn {#'key-store/detect-key (constantly nil)}
        (fn []
          (let [report ((:provider/limits-fn (registry/provider-by-id :alibaba-coding-plan)))]
            (expect (= :alibaba-coding-plan (:provider-id report)))
            (expect (= :unauthenticated (:status report)))
            (expect (= [] (get-in report [:dynamic :limits]))))))))
