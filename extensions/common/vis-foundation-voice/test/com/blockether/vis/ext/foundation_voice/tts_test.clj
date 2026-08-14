(ns com.blockether.vis.ext.foundation-voice.tts-test
  (:require [com.blockether.vis.ext.foundation-voice.assets :as assets]
            [com.blockether.vis.ext.foundation-voice.sherpa :as sherpa]
            [com.blockether.vis.ext.foundation-voice.tts :as tts]
            [lazytest.core :refer [defdescribe it expect]]))

(defn- ex-data-of [f] (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))

(defdescribe catalogue-test
             (it "publishes the manifest's Piper voices, the default first"
                 (expect (= ["kristin" "cori" "john"] (mapv #(name (:id %)) (tts/piper-voices))))
                 (expect (every? :label (tts/piper-voices)))
                 (expect (every? :language (tts/piper-voices)))
                 (expect (= "kristin" (name (:id (:voice (first (tts/piper-assets))))))))
             (it "keeps which file backs a pocket voice to itself"
                 (expect (= ["bria" "loona" "hibiki"] (mapv #(name (:id %)) (tts/pocket-voices))))
                 (expect (not-any? :clip (tts/pocket-voices)))
                 (expect (every? :clip (:voices (tts/pocket-asset)))))
             (it "answers an unknown voice with the ones that exist"
                 (let [data (ex-data-of #(#'tts/piper-asset-for "nope"))]
                   (expect (= :voice-tts/unknown-voice (:type data)))
                   (expect (= :piper (:family data)))
                   (expect (= ["kristin" "cori" "john"] (:known data)))))
             (it "asks for the shared phoneme tables before the voice that reads them"
                 (expect (= ["espeak-ng-data" "piper-en_US-kristin-medium"]
                            (mapv :id (#'tts/required-assets :piper nil))))
                 (expect (= ["espeak-ng-data" "piper-en_GB-cori-medium"]
                            (mapv :id (#'tts/required-assets :piper "cori"))))
                 (expect (= ["pocket-tts-int8"]
                            (mapv :id (#'tts/required-assets :pocket-tts nil))))))

(defdescribe
  readiness-test
  (it "is as ready as its worst part, and its progress never jumps backwards"
      ;; A two-part install: espeak finishing must not reset the number to 0.
      (expect (= {:state :ready} (#'tts/combined-state [{:state :ready} {:state :ready}])))
      (expect (= :absent (:state (#'tts/combined-state [{:state :ready} {:state :absent}]))))
      (let
        [state (#'tts/combined-state
                [{:state :ready} {:state :downloading :phase :downloading :progress 40}])]
        (expect (= :downloading (:phase state)))
        (expect (= :downloading (:state state)))
        (expect (= 70 (:progress state))))
      (expect (= :failed
                 (:state (#'tts/combined-state
                          [{:state :failed :error "refused"}
                           {:state :downloading :progress 10}])))))
  (it "refuses an opt-in family BY NAME instead of reporting absent forever"
      ;; `:absent` with nothing downloading is the one answer a user cannot act
      ;; on, so the refusal carries the command that installs it.
      (with-redefs [assets/installed? (constantly false)]
        (let [state (tts/start-download! :pocket-tts)]
          (expect (= :failed (:state state)))
          (expect (re-find #"pocket-tts-int8" (:error state)))
          (expect (re-find #"--pocket-tts" (:error state))))))
  (it "starts the download of a family Vis does fetch by itself"
      (let [started (atom [])]
        (with-redefs
          [assets/installed? (constantly false)
           assets/install! (fn [entry & _]
                             (swap! started conj (:id entry))
                             (:install-dir entry))]

          (let [state (tts/start-download! :piper)]
            (expect (contains? #{:downloading :ready} (:state state))))))))

(defdescribe install-model-test
             (it "installs an opt-in model when the CLI asks for it by name"
                 ;; `assets/ensure!` refuses pocket-tts on the user's behalf; typing the
                 ;; flag IS the ask, so this path must accept it.
                 (let [installed (atom [])]
                   (with-redefs
                     [sherpa/ensure-native! (constantly nil)
                      assets/installed? (constantly false)
                      assets/install! (fn [entry & _]
                                        (swap! installed conj (:id entry))
                                        (:install-dir entry))]

                     (expect (= ["sherpa-onnx-pocket-tts-int8-2026-01-26"]
                                (tts/install-model! :pocket-tts)))
                     (expect (= ["pocket-tts-int8"] @installed)))))
             (it "installs nothing when everything the voice needs is already there"
                 (with-redefs
                   [sherpa/ensure-native!
                    (constantly nil)

                    assets/installed?
                    (constantly true)

                    assets/install!
                    (fn [& _]
                      (throw (ex-info "must not download" {})))]

                   (expect (= [] (tts/install-model! :piper))))))

(defdescribe synthesize-test
             (it "refuses blank text before it downloads anything"
                 (with-redefs
                   [assets/ensure!
                    (fn [& _]
                      (throw (ex-info "must not download" {})))

                    assets/install!
                    (fn [& _]
                      (throw (ex-info "must not download" {})))]

                   (let [data (ex-data-of #(tts/synthesize! :piper {:text "   "}))]
                     (expect (= :voice-tts/blank-text (:type data)))
                     (expect (= :piper (:family data)))))))
