(ns com.blockether.vis.contract.provider-test
  (:require [com.blockether.vis.contract.document :as document]
            [com.blockether.vis.contract.provider :as provider]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is]]))

(deftest schema-root-validates-provider-reports
  (let [report {"provider_id" "example"
                "status" "ok"
                "fetched_at_ms" 0
                "static" {}
                "dynamic" {"limits" []}}]
    (is (document/valid? "provider" report))
    (is (provider/report-valid? report))
    (is (not (document/valid? "provider" (assoc report "status" "invented"))))
    (is (not (document/valid? "provider" (assoc-in report ["dynamic" "limits"] [{}]))))
    (is (not (document/valid? "provider" {"version" 1 "limits" {}})))
    (is (not (contains? (get (document/schema-document "provider") "$defs") "limits")))))

(deftest limit-row-vocabulary-is-enforced-by-the-payload-schema
  (let [row {"id" "tokens"
             "label" "Tokens"
             "scope" "account"
             "kind" "tokens"
             "precision" "exact"
             "source" "provider-api"
             "is_unlimited" false
             "window" {"kind" "rolling" "unit" "hour" "size" 1}}]
    (is (provider/limit-row-valid? row))
    (doseq [field ["scope" "kind" "precision" "source"]]
      (is (not (provider/limit-row-valid? (assoc row field "invented")))))
    (is (not (provider/limit-row-valid? (assoc-in row ["window" "unit"] "invented"))))))
