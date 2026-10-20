(ns com.blockether.vis.contract.provider-test
  (:require [com.blockether.vis.contract.document :as document]
            [com.blockether.vis.contract.provider :as provider]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe
  schema-root-validates-provider-reports
  (it "schema root validates provider reports"
      (let [report {"provider_id" "example"
                    "status" "ok"
                    "fetched_at_ms" 0
                    "static" {}
                    "dynamic" {"limits" []}}]
        (expect (document/valid? "provider" report))
        (expect (provider/report-valid? report))
        (expect (not (document/valid? "provider" (assoc report "status" "invented"))))
        (expect (not (document/valid? "provider" (assoc-in report ["dynamic" "limits"] [{}]))))
        (expect (not (document/valid? "provider" {"version" 1 "limits" {}})))
        (expect (not (contains? (get (document/schema-document "provider") "$defs") "limits"))))))

(defdescribe limit-row-vocabulary-is-enforced-by-the-payload-schema
             (it "limit row vocabulary is enforced by the payload schema"
                 (let [row {"id" "tokens"
                            "label" "Tokens"
                            "scope" "account"
                            "kind" "tokens"
                            "precision" "exact"
                            "source" "provider-api"
                            "is_unlimited" false
                            "window" {"kind" "rolling" "unit" "hour" "size" 1}}]
                   (expect (provider/limit-row-valid? row))
                   (doseq [field ["scope" "kind" "precision" "source"]]
                     (expect (not (provider/limit-row-valid? (assoc row field "invented")))))
                   (expect (not (provider/limit-row-valid?
                                  (assoc-in row ["window" "unit"] "invented")))))))

(defdescribe
  catalog-model-carries-price-billing-and-limits
  (it "catalog model carries price, billing and the limits report of its provider"
      (let [model {"provider" "example"
                   "name" "example-large"
                   "label" "Example Large"
                   "pricing"
                   {"input" 3.0 "output" 15.0 "tiers" [{"input" 6.0 "above_tokens" 200000}]}
                   "pricing_source" "catalog"
                   "billing" "metered"
                   "context" 400000
                   "is_reasoning" true
                   "reasoning_options" ["low" "high"]
                   "is_tool_call" true
                   "is_image_input" false
                   "modalities" {"input" ["text"] "output" ["text"]}
                   "is_available" true
                   "is_default" false
                   "limits" {"provider_id" "example"
                             "status" "ok"
                             "fetched_at_ms" 0
                             "static" {}
                             "dynamic" {"limits" []}}}]
        (expect (provider/model-valid? model))
        (expect (provider/model-valid? (assoc model "pricing" nil)))
        (expect (not (provider/model-valid? (assoc model "billing" "invented"))))
        (expect (not (provider/model-valid? (assoc-in model ["pricing" "cache_read"] nil))))
        (expect (not (provider/model-valid? (assoc-in model ["pricing" "tiers" 0] {"input" 6.0}))))
        (expect (not (provider/model-valid? (dissoc model "limits"))))
        (expect (some? (provider/explain-model (assoc-in model ["limits" "status"] "invented")))))))
