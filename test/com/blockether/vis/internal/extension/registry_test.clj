(ns com.blockether.vis.internal.extension.registry-test
  "Provider descriptors are checked before they enter the registry, including the
   routing policy a provider owns."
  (:require [com.blockether.vis.internal.extension.registry :as registry]
            [com.blockether.vis.test-provider-policies :as policies]
            [lazytest.core :refer [defdescribe expect it throws?]]))

(defn- descriptor
  [policy]
  {:provider/id :policy-test :provider/label "Policy Test" :provider/policy policy})

(defdescribe
  provider-policy-validation-test
  (it "accepts every policy the first-party providers declare"
      (doseq [[id policy] policies/first-party]
        (expect (registry/provider? (descriptor policy)) (str id))))
  (it "accepts a descriptor without a policy, and an empty one"
      (expect (registry/provider? {:provider/id :policy-test :provider/label "Policy Test"}))
      (expect (registry/provider? (descriptor {}))))
  (it "rejects a malformed policy before registration"
      (doseq [policy [[:preset-rank 1] {:preset-rank "1"} {:title-rank 1.5}
                      {:prompt-cache {:strategy :unknown}} {:prompt-cache {:ttl :24h}}
                      {:initiator-header ""} {:adaptive-reasoning-models "(unclosed"}
                      {:refusal-fallback {:models "(?i)claude" :fallbacks []}}
                      {:refusal-fallback {:models "(?i)claude" :fallbacks '("claude-opus-4-8")}}
                      {:fast-mode
                       {:turn-feature "fast" :service-tier "priority" :cost-multiplier 0}}
                      {:fast-mode {:turn-feature "fast" :cost-multiplier 2.0}}]]
        (expect (not (registry/provider? (descriptor policy))) (pr-str policy))
        (expect (throws? clojure.lang.ExceptionInfo #(registry/provider (descriptor policy)))))))
