(ns com.blockether.vis.contract.view-test
  "View wire lifecycle is the same input/live contract in SDK, gateway and UI."
  (:require [charred.api :as json]
            [clojure.java.io :as io]
            [com.blockether.vis.contract.document :as document]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.contract.view :as contract]
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

(deftest view-schema-is-the-source-of-runtime-vocabulary
  (let [schema
        (document/schema-document "view")

        fields
        (get-in schema ["$defs" "field" "oneOf"])

        live-nodes
        (get-in schema ["$defs" "live_node" "oneOf"])

        names
        #(set (map (fn [shape]
                     (get-in shape ["properties" "type" "const"]))
                   %))]

    (is (nil? (io/resource "vis-contract/view.json")))
    (is (= (names fields) (set (keys contract/field-types))))
    (is (= (disj (names live-nodes) "group") (set (keys contract/live-node-types))))
    (is (= #{:otp :password} contract/secret-types))
    (is (= #{:multiline :password :plaintext} contract/text-types))
    (is (= #{:multiselect :select} contract/choice-types))
    (is (= {:min 0 :max 100 :step 1} contract/range-defaults))
    (is (= {:length 6 :ceiling 12} contract/otp-defaults))
    (is (= #{:id :seq :created-at :owner} contract/live-view-stamp-keys))
    (is (= #{:created-at} contract/request-stamp-keys))
    (is (= #{:is-secret} contract/derived-keys))
    (doseq [[variant frames] contract/spinner-frames]
      (is (document/valid-json? "view" "spinner_variant" (name variant)))
      (is (= 10 (count frames))))
    (doseq [sample ["input" "live"]]
      (is (document/valid?
            "view"
            (get (json/read-json (slurp (io/resource "vis-contract/fixtures/view.json"))) sample))))
    (is (not (document/valid? "view" {"field_types" ["plaintext"]})))))

(deftest schema-derived-key-sets-cover-each-real-operator
  (let [schema (document/schema-document "view")]
    (doseq [[definition discriminator actual] [["operator_action" "action"
                                                contract/view-action-key-sets]
                                               ["live_op" "op" contract/live-op-key-sets]]
            shape (get-in schema ["$defs" definition "oneOf"])]

      (is (= (set (keys (wire/->engine (get shape "properties"))))
             (get actual (keyword (get-in shape ["properties" discriminator "const"]))))))))
