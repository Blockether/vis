(ns com.blockether.vis.contract.toggle-test
  (:require [com.blockether.vis.contract.document :as document]
            [com.blockether.vis.contract.toggle :as toggle]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is]]))

(deftest schema-root-validates-contributions
  (is (document/valid? "toggle" {"id" "show_details" "label" "Show details" "default" true}))
  (is (not (document/valid? "toggle" {"id" "show-details" "label" "Show details" "default" true})))
  (is (not (document/valid? "toggle" {"types" ["boolean"]})))
  (is (toggle/contribution-valid? {:id "show_details" :label "Show details" :default true}))
  (is (not (toggle/contribution-valid? {:id "show_details" :label "Show details" :default "yes"}))))

(deftest toggle-metadata-comes-from-contribution-constraints
  (let [properties (get-in (document/schema-document "toggle")
                           ["$defs" "contribution" "properties"])]
    (is (= (get-in properties ["id" "pattern"]) toggle/id-pattern))
    (is (= (set (map keyword (get-in properties ["type" "enum"]))) toggle/types))
    (is (= (keyword (get-in properties ["type" "default"])) toggle/default-type))
    (is (= (get-in properties ["description" "maxLength"]) toggle/max-description-length))))

(deftest boolean-wire-is-a-real-token-schema
  (is (= #{"1" "on" "true" "yes"} toggle/boolean-true-tokens))
  (is (= #{"0" "false" "no" "off"} toggle/boolean-false-tokens))
  (doseq [token (concat toggle/boolean-true-tokens toggle/boolean-false-tokens)]
    (is (document/valid-json? "toggle" "boolean_wire" token)))
  (doseq [value [true false 0 1 "unknown" {"true" ["yes"]}]]
    (is (not (document/valid-json? "toggle" "boolean_wire" value)))))
