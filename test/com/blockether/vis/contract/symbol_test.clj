(ns com.blockether.vis.contract.symbol-test
  (:require [com.blockether.vis.contract.document :as document]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is]]))

(deftest schema-root-validates-symbol-declarations
  (let [callable {"version" 1
                  "name" "example"
                  "tag" "observation"
                  "description" "Read an example value."
                  "signature" "()"
                  "parameters" []
                  "returns" {"kind" "scalar" "name" "str"}}]
    (is (document/valid? "symbol" callable))
    (is (document/valid? "symbol" {"version" 1 "name" "examples" "members" [callable]}))
    (is (not (document/valid? "symbol" (assoc-in callable ["returns" "kind"] "invented"))))
    (is (not (document/valid? "symbol" {"version" 1 "parameter_kinds" [] "type_kinds" []})))
    ;; Issue #256: only record contracts may declare a public sequence field.
    (let [record {"kind" "record"
                  "name" "Pages"
                  "sequence_field" "results"
                  "fields" [{"name" "results"
                             "required" true
                             "has_default" false
                             "default_is_none" false
                             "type" {"kind" "generic"
                                     "name" "list"
                                     "arguments" [{"kind" "scalar" "name" "str"}]}}]}
          typed (assoc callable "returns" record)]

      (is (document/valid? "symbol" typed))
      (doseq [bad [(assoc record "kind" "scalar") (assoc record "sequence_field" "_private")
                   (assoc record "sequence_field" "") (assoc record "sequence_field" nil)
                   (dissoc record "fields")]]
        (is (not (document/valid? "symbol" (assoc callable "returns" bad))))))))
