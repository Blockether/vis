(ns com.blockether.vis.contract.activity-test
  "The same Activity receipt is consumed by Python, Companion and TUI."
  (:require [clojure.data.json :as json]
            [com.blockether.vis.contract.document :as document]
            [clojure.java.io :as io]
            [com.blockether.vis.contract.activity :as activity]
            [com.blockether.vis.contract.wire :as wire]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is]]))

(deftest portable-activity-contract-test
  (let [vocabulary
        (document/load! "activity")

        fixture
        (json/read-str (slurp (io/resource "vis-contract/fixtures/activity.json")))]

    (is (= "block.activity" (get vocabulary "event")))
    (is (document/valid-json? "activity" "projection" fixture))
    (doseq [old-key ["anchor" "schema_version" "view_id"]]
      (is (not (document/valid-json? "activity" "projection" (assoc fixture old-key 1)))))
    (is (document/valid-json? "activity"
                              "declaration"
                              {"presenter" "tests" "label" "checking components"}))
    (is (not (document/valid-json? "activity"
                                   "declaration"
                                   {"presenter" "tests" "state" "succeeded"})))))

(deftest shared-activity-admission-test
  (doseq [{:strs [name valid projection]}
          (json/read-str (slurp (io/resource "vis-contract/fixtures/activity-cases.json")))]
    (is (= valid (boolean (activity/from-wire projection))) name)
    (when valid (is (= projection (wire/->wire (activity/from-wire projection))) name))))
