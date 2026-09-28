(ns com.blockether.vis.contract.toggle-test
  (:require [com.blockether.vis.contract.document :as document]
            [com.blockether.vis.contract.toggle :as toggle]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe
  schema-root-validates-contributions
  (it "schema root validates contributions"
      (expect (document/valid? "toggle"
                               {"id" "show_details" "label" "Show details" "default" true}))
      (expect (not (document/valid? "toggle"
                                    {"id" "show-details" "label" "Show details" "default" true})))
      (expect (not (document/valid? "toggle" {"types" ["boolean"]})))
      (expect (toggle/contribution-valid? {:id "show_details" :label "Show details" :default true}))
      (expect (not (toggle/contribution-valid?
                     {:id "show_details" :label "Show details" :default "yes"})))))

(defdescribe toggle-metadata-comes-from-contribution-constraints
             (it "toggle metadata comes from contribution constraints"
                 (let [properties (get-in (document/schema-document "toggle")
                                          ["$defs" "contribution" "properties"])]
                   (expect (= (get-in properties ["id" "pattern"]) toggle/id-pattern))
                   (expect (= (set (map keyword (get-in properties ["type" "enum"]))) toggle/types))
                   (expect (= (keyword (get-in properties ["type" "default"])) toggle/default-type))
                   (expect (= (get-in properties ["description" "maxLength"])
                              toggle/max-description-length)))))

(defdescribe boolean-wire-is-a-real-token-schema
             (it "boolean wire is a real token schema"
                 (expect (= #{"1" "on" "true" "yes"} toggle/boolean-true-tokens))
                 (expect (= #{"0" "false" "no" "off"} toggle/boolean-false-tokens))
                 (doseq [token (concat toggle/boolean-true-tokens toggle/boolean-false-tokens)]
                   (expect (document/valid-json? "toggle" "boolean_wire" token)))
                 (doseq [value [true false 0 1 "unknown" {"true" ["yes"]}]]
                   (expect (not (document/valid-json? "toggle" "boolean_wire" value))))))
