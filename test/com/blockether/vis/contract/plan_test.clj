(ns com.blockether.vis.contract.plan-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.contract.document :as document]
            [com.blockether.vis.contract.plan :as plan]
            [com.blockether.vis.contract.wire :as wire]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is]]))

(deftest canonical-planning-schema
  (is (nil? (io/resource "vis-contract/plans.json")))
  (is (document/valid? "plans" {"kind" "plan" "feature" "search" "status" "ready"}))
  (doseq [invalid [{"kind" "plan" "feature" "search" "status" "unknown"}
                   {"kind" "other" "feature" "search" "status" "ready"}
                   {"kind" "plan" "feature" "Search" "status" "ready"}
                   {"kind" "plan" "feature" "search"}]]
    (is (not (document/valid? "plans" invalid))))
  (doseq [declaration (get-in (document/schema-document "plans") ["$defs" "action" "oneOf"])]
    (let [action (get declaration "const")
          request {"filename" "PLAN-search.md" "version" 3 "action" action}]

      (is (document/valid-json? "plans" "action_request" request))
      (is (str/ends-with? (plan/action-request "PLAN-search.md" 3 action)
                          (get declaration "x-vis-request")))
      (doseq [invalid [(assoc request "version" 0) (assoc request "action" "unknown")
                       (assoc request "filename" "search.md")]]
        (is (not (document/valid-json? "plans" "action_request" invalid)))))))

(deftest shared-planning-fixtures
  (let [fixture (wire/parse-json (slurp (io/resource "vis-contract/fixtures/plans.json")))]
    (doseq [{:strs [filename text expected actions]} (get fixture "documents")]
      (let [info (plan/document-info filename text)]
        (is (= expected (wire/->wire info)))
        (is (= actions (mapv name (plan/available-actions info false))))))))
