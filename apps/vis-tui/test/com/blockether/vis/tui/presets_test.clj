(ns com.blockether.vis.tui.presets-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [lazytest.core :refer [defdescribe describe expect it]]
            [com.blockether.vis.contract.document :as document]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.tui.capture :as cap]
            [com.blockether.vis.tui.client :as vis]
            [com.blockether.vis.tui.dialogs :as dlg]
            [com.blockether.vis.tui.presets :as presets]))

(def ^:private fixture
  (wire/parse-json (slurp (io/resource "vis-contract/fixtures/setting-presets.json"))))

(def ^:private catalog-row (get fixture "row"))

(defn- drive
  "Run the presets editor of the fixture row on a virtual terminal with `keys`. Returns the
   values it wrote, the previews it asked for and the text of every frame."
  [keys & {:keys [write]}]
  (let [writes
        (atom [])

        previews
        (atom [])

        result
        (cap/capture!
          {:cols 100
           :rows 34
           :keys keys
           :paint! (fn [{:keys [screen g]}]
                     (with-redefs [vis/set-setting-value!
                                   (or write
                                       (fn [id value _]
                                         (swap! writes conj [id value])
                                         (assoc catalog-row "value" value)))

                                   dlg/load-settings-inventory!
                                   (constantly nil)

                                   presets/preview!
                                   (fn [id body]
                                     (swap! previews conj [id body])
                                     {:played? true})]

                       (#'dlg/settings-presets-editor!
                        screen
                        g
                        {:left 1 :inner-w 96 :hint-row 32 :text-w 92 :min-row 2}
                        (#'dlg/catalog-setting-row catalog-row))))})]

    (when-let [error (:error result)]
      (throw error))
    {:writes @writes
     :previews @previews
     :frames (mapv #(cap/frame-text (assoc result :frames [%])) (:frames result))}))

(defn- written-value
  "The one value that a drive wrote, after a check that the gateway schema accepts it."
  [{:keys [writes]}]
  (expect (= 1 (count writes)))
  (let [[id value] (first writes)]
    (expect (= "sound_presets" id))
    (expect (nil? (document/explain "gateway" "presets_value" value)))
    value))

(defdescribe
  presets-editor-test
  (describe
    "the settings list"
    (it "names the active preset, not a count of fields"
        (expect (= "My mix"
                   (#'dlg/settings-option-value (#'dlg/catalog-setting-row catalog-row) {}))))
    (it "locks the row while its parent switch is off"
        (let [groups
              [{"title" "Sounds"
                "toggles" [{"id" "sounds"
                            "type" "boolean"
                            "label" "Sounds"
                            "enabled" false
                            "children" [catalog-row]}]}]

              rows
              (#'dlg/lock-overridden-rows groups (#'dlg/catalog-toggle-rows groups))]

          (expect (= "Turn on Sounds to change this."
                     (:locked (first (filter #(= "sound_presets" (:toggle-id %)) rows))))))))
  (describe
    "the editor"
    (it "shows built-in presets first, then custom presets and the slots of the active one"
        (let [frame
              (first (:frames (drive [:esc])))

              at
              #(str/index-of frame %)]

          (expect (< (at "Classic") (at "Bright") (at "My mix") (at "Finish")))
          (expect (str/includes? frame "Glass (built in)"))
          (expect (str/includes? frame "~/Sounds/done.wav"))
          (expect (str/includes? frame "No sound"))))
    (it "writes the whole value with the chosen preset active"
        (expect (= {"active" "bright" "custom" (get-in catalog-row ["value" "custom"])}
                   (written-value (drive [:up :enter :esc])))))
    (it "previews the slot value of the active preset"
        (let [{:keys [previews writes]} (drive [:down :down :enter :esc])]
          (expect (empty? writes))
          (expect (= [["sound_presets" (get fixture "preview_request")]] previews)))
        (expect (= {"slot" "finish" "value" "glass"}
                   (presets/preview-body (get catalog-row "editor_options")
                                         (get catalog-row "value")
                                         "glass"
                                         "finish"))))
    (it "adds a new preset filled from the active one and makes it active"
        (let [value (written-value (drive [:down :down :down :down :enter :enter \L \o \u \d :enter
                                           :down :down :down :down :enter :esc]))]
          (expect (= "Loud" (get value "active")))
          (expect (= {"name" "Loud"
                      "slots" {"finish" "~/Sounds/done.wav" "input" "glass" "error" ""}}
                     (last (get value "custom"))))))
    (it "renames a custom preset and keeps it active"
        (expect (= {"active" "Mine"
                    "custom" [{"name" "Mine"
                               "slots" {"finish" "~/Sounds/done.wav" "input" "glass" "error" ""}}]}
                   (written-value (drive (concat [:down :down :down :down :down :enter :enter]
                                                 (repeat 6 :backspace)
                                                 [\M \i \n \e :enter :down :down :down :down :enter
                                                  :esc]))))))
    (it "deletes the active preset and makes the first built-in preset active"
        (expect (= {"active" "classic" "custom" []}
                   (written-value (drive [:down :down :down :down :down :down :enter \y :esc])))))
    (it "shows the message of a refused write"
        (let [{:keys [frames]} (drive [:up :enter \q :esc]
                                      :write
                                      (fn [& _]
                                        (throw (ex-info "Preset A: no sound file at ~/nope.wav."
                                                        {:http-status 400}))))]
          (expect (some #(str/includes? % "Preset A: no sound file at ~/nope.wav.") frames))))))
