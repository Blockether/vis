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

(defdescribe contribution-problems-name-field-rule-and-limit
             ;; #322 and #323: a description of up to 300 characters is valid, and each
             ;; failure names the field, the rule and the limit.
             (let [mode {:id "steering_mode"
                         :label "Steering mode"
                         :type :enum
                         :choices ["vibe" "control"]
                         :default "vibe"}]
               (it "accepts 300 characters and explains 301"
                   (expect (= 300 toggle/max-description-length))
                   (expect (= []
                              (toggle/contribution-problems
                                (assoc mode :description (apply str (repeat 300 "x"))))))
                   (expect (= (str "Invalid setting steering_mode: description is 301 characters; "
                                   "maximum is 300 (maxLength). Shorten the description.")
                              (toggle/contribution-message
                                "Invalid setting"
                                (assoc mode :description (apply str (repeat 301 "x")))))))
               (it "keeps the empty and multi-line rules, with readable lines"
                   (expect (= ["description is 0 characters; minimum is 1 (minLength)."
                               "description must be one line without line breaks (pattern)."]
                              (toggle/contribution-problems (assoc mode :description ""))))
                   (expect (= ["description must be one line without line breaks (pattern)."]
                              (toggle/contribution-problems (assoc mode :description "one\ntwo")))))
               (it "names missing fields, closed values and semantic failures"
                   (expect (= ["label is missing (required)."]
                              (toggle/contribution-problems (dissoc mode :label))))
                   (expect (= ["type must be one of: boolean, enum, string (enum)."]
                              (toggle/contribution-problems (assoc mode :type :list))))
                   (expect (= ["default must be one of the choices."]
                              (toggle/contribution-problems (assoc mode :default "other")))))))

(defdescribe boolean-wire-is-a-real-token-schema
             (it "boolean wire is a real token schema"
                 (expect (= #{"1" "on" "true" "yes"} toggle/boolean-true-tokens))
                 (expect (= #{"0" "false" "no" "off"} toggle/boolean-false-tokens))
                 (doseq [token (concat toggle/boolean-true-tokens toggle/boolean-false-tokens)]
                   (expect (document/valid-json? "toggle" "boolean_wire" token)))
                 (doseq [value [true false 0 1 "unknown" {"true" ["yes"]}]]
                   (expect (not (document/valid-json? "toggle" "boolean_wire" value))))))

(defdescribe string-settings-follow-the-contract
             (let [url {:id "kb_url" :label "Knowledge base URL" :type :string :default ""}]
               (it "accepts a text setting and a global secret with an empty default"
                   (expect (toggle/contribution-valid? url))
                   (expect (toggle/contribution-valid? (assoc url
                                                         :default "https://kb.example.com")))
                   (expect (toggle/contribution-valid? (assoc url
                                                         :secret? true
                                                         :scopes ["global"]))))
               (it "refuses a secret outside the global scope, with a default, or of another type"
                   (expect (not (toggle/contribution-valid? (assoc url
                                                              :secret? true
                                                              :scopes ["global" "project"]))))
                   (expect (not (toggle/contribution-valid? (assoc url
                                                              :secret? true
                                                              :default "x"))))
                   (expect (not (toggle/contribution-valid?
                                  {:id "flag" :label "Flag" :default false :secret? true}))))
               (it "refuses choices, a non-string default and a multi-line default"
                   (expect (not (toggle/contribution-valid? (assoc url :choices ["a" "b"]))))
                   (expect (not (toggle/contribution-valid? (assoc url :default 1))))
                   (expect (not (toggle/contribution-valid? (assoc url :default "a\nb")))))
               (it "bounds a value to one line within the limit"
                   (expect (= 4096 toggle/max-string-length))
                   (expect (toggle/string-value? ""))
                   (expect (toggle/string-value? (apply str (repeat 4096 "x"))))
                   (expect (not (toggle/string-value? (apply str (repeat 4097 "x")))))
                   (expect (not (toggle/string-value? "a\tb")))
                   (expect (not (toggle/string-value? nil))))))
