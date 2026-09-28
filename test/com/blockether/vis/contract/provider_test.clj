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
