(ns com.blockether.vis.internal.sounds-test
  "Sound presets: which file a cue plays, and which preset values a person can save."
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [com.blockether.vis.contract.document :as document]
            [com.blockether.vis.test-temp :as test-temp]
            [lazytest.core :refer [defdescribe expect it]]
            [com.blockether.vis.internal.sounds :as sounds]))

(defn- bundled-bytes
  [preset slot]
  (seq (.readAllBytes (io/input-stream (io/resource (str "vis-sounds/" preset "/" slot ".wav"))))))

(defn- file-bytes [^java.io.File file] (seq (java.nio.file.Files/readAllBytes (.toPath file))))

(defn- thrown-type [f] (try (f) nil (catch clojure.lang.ExceptionInfo e (:type (ex-data e)))))

(defdescribe
  cue-file-test
  (it "plays each slot of a built-in preset from its own bundled file"
      (doseq [{:strs [id]}
              sounds/builtin

              {slot "id"}
              sounds/slots]

        (expect (= (bundled-bytes id slot)
                   (file-bytes (sounds/cue-file {"active" id "custom" []} (keyword slot)))))))
  (it "plays a custom slot as no sound, a built-in sound or a file"
      (let [own
            (doto (java.io.File/createTempFile "own-sound-" ".wav") (test-temp/delete-at-exit!))

            presets
            {"active" "Mine"
             "custom" [{"name" "Mine"
                        "slots"
                        {"finish" "" "input" "soft" "error" (str " " (.getPath own) " ")}}]}]

        (expect (nil? (sounds/cue-file presets :finish)))
        (expect (= (bundled-bytes "soft" "input") (file-bytes (sounds/cue-file presets :input))))
        (expect (= own (sounds/cue-file presets :error)))))
  (it "plays the default preset when no preset has the active name"
      (expect (= (bundled-bytes "classic" "finish")
                 (file-bytes (sounds/cue-file {"active" "Gone" "custom" []} :finish)))))
  (it "throws for a custom path that is not a file"
      (expect (= :vis.sounds/missing-file
                 (thrown-type #(sounds/cue-file
                                 {"active" "Mine"
                                  "custom" [{"name" "Mine" "slots" {"error" "/no/such/sound.wav"}}]}
                                 :error))))))

(defdescribe
  problem-test
  (it "accepts the default value and a custom preset that is active"
      (expect (nil? (sounds/problem sounds/default-value)))
      (expect (nil? (sounds/problem {"active" "Mine"
                                     "custom" [{"name" "Mine"
                                                "slots" {"finish" "glass" "input" ""}}]}))))
  (it "refuses names that collide, unknown slots and an unknown active preset"
      (expect (some? (sounds/problem {"active" "classic" "custom" [{"name" "Glass" "slots" {}}]})))
      (expect (some? (sounds/problem {"active" "classic"
                                      "custom" [{"name" "A" "slots" {}}
                                                {"name" " a " "slots" {}}]})))
      (expect (some? (sounds/problem {"active" "classic"
                                      "custom" [{"name" "A" "slots" {"beep" ""}}]})))
      (expect (some? (sounds/problem {"active" "Nope" "custom" []})))
      (expect (some? (sounds/problem {"active" "classic"
                                      "custom" (vec (for [i (range (inc sounds/MAX_CUSTOM))]
                                                      {"name" (str "P" i) "slots" {}}))}))))
  (it "names a path that is not a file only in the file check"
      (let [value {"active" "A" "custom" [{"name" "A" "slots" {"finish" "/no/such/sound.wav"}}]}]
        (expect (nil? (sounds/problem value)))
        (expect (= "Preset A: no sound file at /no/such/sound.wav." (sounds/file-problem value))))))

(defdescribe preview-test
             (it "plays a slot value before a save and refuses an unknown slot"
                 (let [files (atom [])]
                   (with-redefs [sounds/play-file! (fn [file _]
                                                     (swap! files conj file)
                                                     (some? file))]
                     (expect (true? (sounds/preview! "finish" "bright")))
                     (expect (false? (sounds/preview! "input" "")))
                     (expect (= :vis.sounds/unknown-slot
                                (thrown-type #(sounds/preview! "beep" "classic")))))
                   (expect (= (bundled-bytes "bright" "finish") (file-bytes (first @files))))))
             (it "stops the sound that plays when the person previews another one"
                 (let [started (atom [])]
                   (with-redefs [sounds/player-command (fn [_]
                                                         ["sleep" "30"])]
                     (try (expect (true? (sounds/preview! "finish" "bright")))
                          (swap! started conj @@#'sounds/playing)
                          (expect (true? (sounds/preview! "error" "bright")))
                          (swap! started conj @@#'sounds/playing)
                          (let [[^Process first-player ^Process second-player] @started]
                            (expect (.waitFor first-player 5 java.util.concurrent.TimeUnit/SECONDS))
                            (expect (.isAlive second-player)))
                          (finally (doseq [^Process process @started]
                                     (.destroyForcibly process))))))))

(defdescribe
  presets-fixture-test
  ;; The TUI, the Companion and the SDK build the presets editor from this fixture.
  (it "matches the editor data of the gateway and the gateway schema"
      (let [fixture (json/read-str (slurp (io/resource
                                            "vis-contract/fixtures/setting-presets.json")))]
        (expect (= sounds/editor-options (get-in fixture ["row" "editor_options"])))
        (expect (document/valid-json? "gateway" "setting" (get fixture "row")))
        (expect (document/valid-json? "gateway" "setting_write" (get fixture "write")))
        (expect (document/valid-json? "gateway" "setting_preview" (get fixture "preview_request")))
        (expect
          (document/valid-json? "gateway" "setting_preview_result" (get fixture "preview_result")))
        (expect (nil? (sounds/problem (get-in fixture ["write" "value"]))))))
  (it "refuses each invalid value of the fixture with a message"
      (let [fixture (json/read-str (slurp (io/resource
                                            "vis-contract/fixtures/setting-presets.json")))]
        (doseq [{:strs [value reason]} (get fixture "invalid_values")]
          (expect (string? (or (sounds/problem value) (sounds/file-problem value))) reason)))))
