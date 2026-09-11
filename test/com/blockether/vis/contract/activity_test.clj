(ns com.blockether.vis.contract.activity-test
  "The same Activity receipt is consumed by Python, Companion and TUI."
  (:require [clojure.data.json :as json]
            [com.blockether.vis.contract.document :as document]
            [clojure.java.io :as io]
            [com.blockether.vis.contract.activity :as activity]
            [com.blockether.vis.contract.wire :as wire]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is]]))

(deftest shared-activity-copy-test
  (doseq [{:strs [name projection text]}
          (json/read-str (slurp (io/resource "vis-contract/fixtures/activity-copy.json")))]
    (let [receipt (activity/from-wire projection)]
      (is (some? receipt) name)
      (is (= text (activity/copy-text receipt)) name))))

(deftest portable-activity-contract-test
  (let [vocabulary
        (document/load! "activity")

        fixture
        (json/read-str (slurp (io/resource "vis-contract/fixtures/activity.json")))]

    (is (= "block.activity" (get vocabulary "event")))
    (is (document/valid-json? "activity" "projection" fixture))
    (doseq [old-key ["anchor" "schema_version" "view_id"]]
      (is (not (document/valid-json? "activity" "projection" (assoc fixture old-key 1)))))
    (doseq [show-start [true false]]
      (is (document/valid-json?
            "activity"
            "declaration"
            {"presenter" "tests" "label" "Check components" "show_start" show-start})))
    (doseq [show-start [nil 0 1 "false"]]
      (is (not (document/valid-json? "activity"
                                     "declaration"
                                     {"presenter" "tests" "show_start" show-start}))))
    (is (not (document/valid-json? "activity"
                                   "declaration"
                                   {"presenter" "tests" "state" "succeeded"})))))

(deftest shared-activity-admission-test
  (doseq [{:strs [name valid projection]}
          (json/read-str (slurp (io/resource "vis-contract/fixtures/activity-cases.json")))]
    (is (= valid (boolean (activity/from-wire projection))) name)
    (when valid (is (= projection (wire/->wire (activity/from-wire projection))) name))))

(deftest shared-operation-groups-test
  ;; Regression #201: extension labels must agree with Companion without changing receipts.
  (doseq [{:strs [name projection groups]}
          (json/read-str (slurp (io/resource "vis-contract/fixtures/activity-groups.json")))]
    (let [receipt (activity/from-wire projection)
          group-rows (ns-resolve 'com.blockether.vis.contract.activity 'operation-groups)]

      (is (some? receipt) name)
      (is (= projection (wire/->wire receipt)) name)
      (is (some? group-rows) "The contract owns chronological grouping")
      (when group-rows
        (is (= groups
               (mapv (fn [{:keys [id label rows]}]
                       {"id" id "label" label "rows" (mapv :id rows)})
                     (group-rows (:rows receipt))))
            name)))))

(deftest shared-argument-groups-test
  (doseq [{:strs [name projection groups]}
          (json/read-str (slurp (io/resource "vis-contract/fixtures/activity-arguments.json")))]
    (let [receipt (activity/from-wire projection)
          group-rows (ns-resolve 'com.blockether.vis.contract.activity 'argument-groups)]

      (is (some? receipt) name)
      (is (= projection (wire/->wire receipt)) name)
      (is (some? group-rows))
      (when group-rows
        (is (= groups
               (mapv (fn [{:keys [id rows]}]
                       {"id" id "rows" (mapv :id rows)})
                     (group-rows (:rows receipt))))
            name)))))
