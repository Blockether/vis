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

(deftest drafts-are-an-experimental-opt-in-that-defaults-to-off
  (let [spec (toggles/toggle-spec "draft_backend")]
    (is (= "off" (:default spec)))
    (is (= :enum (:type spec)))
    (is (= ["auto" "worktree" "rift" "off"] (:choices spec)))
    (is (true? (:experimental? spec)))
    (is (= :experimental (:group spec)))
    (is (some #(= "draft_backend" (:id %)) (toggles/toggles-for-channel :tui)))))
