;; Fixture: packages/vis-contract/resources/vis-contract/fixtures/setting-model.json
(ns com.blockether.vis.tui.model-setting-test
  (:require [clojure.java.io :as io]
            [lazytest.core :refer [defdescribe describe expect it]]
            [com.blockether.vis.contract.document :as document]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.tui.capture :as cap]
            [com.blockether.vis.tui.client :as vis]
            [com.blockether.vis.tui.dialogs :as dlg]))

(def ^:private fixture
  (wire/parse-json (slurp (io/resource "vis-contract/fixtures/setting-model.json"))))

(def ^:private catalog-row (get fixture "row"))

(def ^:private empty-row
  (assoc catalog-row
    "value" {}
    "own_value" {}))

(def ^:private fleet
  [{:id :anthropic :models [{:name "claude-sonnet-4-5"} {:name "claude-haiku-4-5"}]}])

(defn- written-values
  "Open `row` in the settings list on a virtual terminal with `keys`. Returns each written
   `[id value]`."
  [row keys]
  (let [writes
        (atom [])

        result
        (cap/capture!
          {:cols 100
           :rows 34
           :keys keys
           :paint! (fn [{:keys [screen g]}]
                     (with-redefs [vis/picker-fleet
                                   (constantly fleet)

                                   vis/set-setting-value!
                                   (fn [id value _]
                                     (swap! writes conj [id value])
                                     (assoc row "value" value))

                                   dlg/load-settings-inventory!
                                   (constantly nil)]

                       (#'dlg/activate-unlocked-row!
                        screen
                        g
                        {:left 1 :inner-w 96 :hint-row 32 :text-w 92 :min-row 2}
                        {}
                        {}
                        (#'dlg/catalog-setting-row row))))})]

    (when-let [error (:error result)]
      (throw error))
    (doseq [[_ value] @writes]
      (expect (nil? (document/explain "gateway" "model_value" value))))
    @writes))

(defdescribe
  model-setting-test
  (describe
    "the settings list"
    (it "shows the provider and the model"
        (expect (= "anthropic/claude-sonnet-4-5"
                   (#'dlg/settings-option-value (#'dlg/catalog-setting-row catalog-row) {}))))
    (it "shows a short label when no model is set"
        (expect (= "Not set"
                   (#'dlg/settings-option-value (#'dlg/catalog-setting-row empty-row) {})))))
  (describe "the editor"
            (it "writes the chosen model of the picker"
                (let [{:strs [id value]} (get fixture "write")]
                  (expect (= [[id value]]
                             (written-values empty-row [\s \o \n \n \e \t :enter :esc])))))
            (it "writes an empty value from the first row"
                (let [{:strs [id value]} (get fixture "clear_write")]
                  (expect (= [[id value]] (written-values catalog-row [:home :enter :esc])))))))
