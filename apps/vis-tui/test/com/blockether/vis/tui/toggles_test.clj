(ns com.blockether.vis.tui.toggles-test
  (:require [clojure.string :as str]
            [com.blockether.vis.contract.toggle :as contract]
            [com.blockether.vis.tui.toggles :as toggles]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is]]))

(deftest config-boolean-tokens-use-the-wire-schema
  (let [id "schema_boolean_token_test"]
    (toggles/register-toggle! {:id id :label "Schema token test" :default false})
    (doseq [token contract/boolean-true-tokens]
      (is (true? (toggles/coerce-config-value id token)))
      (is (true? (toggles/coerce-config-value id (str "  " (str/upper-case token) "  ")))))
    (doseq [token (conj contract/boolean-false-tokens "unknown")]
      (is (false? (toggles/coerce-config-value id token))))
    (is (true? (toggles/coerce-config-value id true)))
    (is (false? (toggles/coerce-config-value id false)))))
