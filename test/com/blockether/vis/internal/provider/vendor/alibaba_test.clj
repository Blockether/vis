(ns com.blockether.vis.internal.provider.vendor.alibaba-test
  (:require [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.internal.extension.registry :as registry]
            [com.blockether.vis.internal.provider.limits :as provider-limits]
            [com.blockether.vis.internal.config.core :as config]
            [com.blockether.vis.internal.provider.key-store :as key-store]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe
  provider-registration-test
  (it "registers both Alibaba plans as separate provider extension entries"
      (require 'com.blockether.vis.internal.provider.vendor.alibaba :reload)
      (let [coding
            (registry/provider-by-id :alibaba-coding-plan)

            token
            (registry/provider-by-id :alibaba-token-plan)

            ext-names
            (set (map :ext/name (extension/registered-extensions)))]

        (expect (= :alibaba-coding-plan (:provider/id coding)))
        (expect (= :alibaba-token-plan (:provider/id token)))
        (expect (contains? ext-names "provider-alibaba"))
        ;; The ids ARE the models.dev slugs and the base URLs are the ones the
        ;; catalog publishes for them - that pairing is what resolves pricing
        ;; and context windows for the preset models.
        (expect (= "https://coding-intl.dashscope.aliyuncs.com/v1"
                   (get-in coding [:provider/preset :base-url])))
        (expect (= "https://token-plan.ap-southeast-1.maas.aliyuncs.com/compatible-mode/v1"
                   (get-in token [:provider/preset :base-url])))))
  (it
    "presets the models each endpoint actually serves, with a window for the uncatalogued one"
    (require 'com.blockether.vis.internal.provider.vendor.alibaba :reload)
    (let [coding-models
          (get-in (registry/provider-by-id :alibaba-coding-plan) [:provider/preset :default-models])

          token-models
          (get-in (registry/provider-by-id :alibaba-token-plan) [:provider/preset :default-models])]

      (expect (= "qwen3.7-max" (first coding-models)))
      (expect (contains? (set coding-models) "glm-5"))
      ;; `qwen3.8-max` is served live but ships as `qwen3.8-max-preview` in
      ;; the models.dev catalog, so its window is declared inline.
      (expect (= {:name "qwen3.8-max" :context 1000000 :output-limit 131072 :tool-call? true}
                 (first token-models)))
      (expect (contains? (set (rest token-models)) "deepseek-v4-pro")))))

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
