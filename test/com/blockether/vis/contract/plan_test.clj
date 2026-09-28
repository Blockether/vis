(ns com.blockether.vis.contract.plan-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.contract.document :as document]
            [com.blockether.vis.contract.plan :as plan]
            [com.blockether.vis.contract.wire :as wire]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe
  canonical-planning-schema
  (it "canonical planning schema"
      (expect (nil? (io/resource "vis-contract/plans.json")))
      (expect (document/valid? "plans" {"kind" "plan" "feature" "search" "status" "ready"}))
      (doseq [invalid [{"kind" "plan" "feature" "search" "status" "unknown"}
                       {"kind" "other" "feature" "search" "status" "ready"}
                       {"kind" "plan" "feature" "Search" "status" "ready"}
                       {"kind" "plan" "feature" "search"}]]
        (expect (not (document/valid? "plans" invalid))))
      (doseq [declaration (get-in (document/schema-document "plans") ["$defs" "action" "oneOf"])]
        (let [action (get declaration "const")
              request {"filename" "PLAN-search.md" "version" 3 "action" action}]

          (expect (document/valid-json? "plans" "action_request" request))
          (expect (str/ends-with? (plan/action-request "PLAN-search.md" 3 action)
                                  (get declaration "x-vis-request")))
          (doseq [invalid [(assoc request "version" 0) (assoc request "action" "unknown")
                           (assoc request "filename" "search.md")]]
            (expect (not (document/valid-json? "plans" "action_request" invalid))))))))

(defdescribe shared-planning-fixtures
             (it "shared planning fixtures"
                 (let [fixture (wire/parse-json (slurp (io/resource
                                                         "vis-contract/fixtures/plans.json")))]
                   (doseq [{:strs [filename text expected actions]} (get fixture "documents")]
                     (let [info (plan/document-info filename text)]
                       (expect (= expected (wire/->wire info)))
                       (expect (= actions (mapv name (plan/available-actions info false)))))))))
