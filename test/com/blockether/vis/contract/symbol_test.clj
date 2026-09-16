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
    (is (not (document/valid? "symbol" {"version" 1 "parameter_kinds" [] "type_kinds" []})))))
