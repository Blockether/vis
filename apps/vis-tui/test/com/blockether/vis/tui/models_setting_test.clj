;; Fixture: packages/vis-contract/resources/vis-contract/fixtures/setting-models.json
(ns com.blockether.vis.tui.models-setting-test
  (:require [clojure.java.io :as io]
            [lazytest.core :refer [defdescribe describe expect it]]
            [com.blockether.vis.contract.document :as document]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.tui.capture :as cap]
            [com.blockether.vis.tui.client :as vis]
            [com.blockether.vis.tui.dialogs :as dlg]
            [com.blockether.vis.tui.provider :as provider]))

(def ^:private fixture
  (wire/parse-json (slurp (io/resource "vis-contract/fixtures/setting-models.json"))))

(def ^:private catalog-row (get fixture "row"))

(def ^:private empty-row
  (assoc catalog-row
    "value" []
    "own_value" []))

(def ^:private fleet
  [{:id :anthropic :models [{:name "claude-sonnet-4-5"} {:name "claude-opus-4-1"}]}
   {:id :openai :models [{:name "gpt-5"}]}])

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
                        {:choose-model #(provider/choose-model-transient! screen %1 %2 %3)}
                        (#'dlg/catalog-setting-row row))))})]

    (when-let [error (:error result)]
      (throw error))
    (doseq [[_ value] @writes]
      (expect (nil? (document/explain "gateway" "models_value" value))))
    @writes))

(defdescribe
  models-setting-test
  (describe
    "the settings list"
    (it "shows the models in order"
        (expect (= "anthropic/claude-sonnet-4-5, openai/gpt-5"
                   (#'dlg/settings-option-value (#'dlg/catalog-setting-row catalog-row) {}))))
    (it "shows a short label when no model is set"
        (expect (= "Not set"
                   (#'dlg/settings-option-value (#'dlg/catalog-setting-row empty-row) {})))))
  (describe "the editor"
            (it "adds the model chosen in the provider and model bands at the end"
                (let [{:strs [id value]} (get fixture "write")]
                  (expect (= [[id value]] (written-values catalog-row [\a \a \b :esc])))))
            (it "makes a later model the default"
                (let [[first-model second-model] (get catalog-row "value")]
                  (expect (= [["subagent_models" [second-model first-model]]]
                             (written-values catalog-row [\2 \d :esc])))))
            (it "writes an empty list when each model is removed"
                (let [{:strs [id value]}
                      (get fixture "clear_write")

                      [_ second-model]
                      (get catalog-row "value")]

                  (expect (= [[id [second-model]] [id value]]
                             (written-values catalog-row [\1 \x \1 \x :esc])))))
            (it "writes nothing on Esc"
                (expect (= [] (written-values catalog-row [\a \a :esc :esc :esc]))))))
