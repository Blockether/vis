(ns com.blockether.vis.contract.view-test
  "View wire lifecycle is the same input/live contract in SDK, gateway and UI."
  (:require [charred.api :as json]
            [clojure.java.io :as io]
            [com.blockether.vis.contract.document :as document]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.internal.view.core :as view]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is]]))

(deftest shared-view-lifecycle-test
  (let [samples (json/read-json (slurp (io/resource "vis-contract/fixtures/view.json")))]
    (doseq [[sample definition] [["input" "input_view"] ["live" "live_view"] ["patch" "live_patch"]
                                 ["answer" "answer"] ["input_result" "input_result"]
                                 ["result" "live_result"]]]
      (is (document/valid-json? "view" definition (get samples sample)))
      (is (not
            (document/valid-json? "view" definition (assoc (get samples sample) "legacy" true)))))
    (doseq [action [{"action" "submit" "values" {"name" "Ada"}} {"action" "cancel"}
                    {"action" "select" "node_id" "table" "item_ids" ["one"]}
                    {"action" "interrupt" "note" "Stop"}]]
      (is (document/valid-json? "view" "operator_action" action))
      (is (not (document/valid-json? "view" "operator_action" (assoc action "legacy" true)))))))

(deftest input-view-fixture-is-the-engine-projection
  ;; The real SDK flow exposed a false fixture: request includes channel routing,
  ;; whereas request->view removes it and carries the engine's created_at stamp.
  (let [sample
        (get (json/read-json (slurp (io/resource "vis-contract/fixtures/view.json"))) "input")

        projected
        (-> {:id "input-one"
             :title "Name"
             :timeout-ms 30000
             :fields [{:type "plaintext" :id "name" :label "Name" :is-required true}]}
            view/normalize-request
            (assoc :created-at 1)
            view/request->view
            wire/->wire)]

    (is (= sample projected))
    (is (document/valid-json? "view" "input_view" projected))
    (is (not (document/valid-json? "view" "input_view" (assoc projected "channel_ids" ["app"]))))))

(deftest every-input-field-crosses-the-real-projection
  (doseq [type ["checkbox" "multiline" "multiselect" "otp" "password" "plaintext" "range" "select"]]
    (let [field (cond-> {:type type :id "field" :label "Value"}
                  (#{"select" "multiselect"} type)
                  (assoc :options ["one" "two"]))
          projected (-> {:title "Fields" :fields [{:type "group" :fields [field]}]}
                        view/normalize-request
                        (assoc :created-at 1)
                        view/request->view
                        wire/->wire)]

      (is (document/valid-json? "view" "input_view" projected))
      (doseq [[key value] [["is_secret" false] ["validate" ["callback"]]]]
        (is (not (document/valid-json? "view"
                                       "input_view"
                                       (assoc-in projected ["fields" 0 "fields" 0 key] value))))))))
