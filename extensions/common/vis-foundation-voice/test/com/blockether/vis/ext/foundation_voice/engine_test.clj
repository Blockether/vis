(ns com.blockether.vis.ext.foundation-voice.engine-test
  "The two ways the local Parakeet engine used to need a RESTART instead of a retry:
   a download that failed once, and a native library the JVM could not link."
  (:require [clojure.string :as str]
            [com.blockether.vis.ext.foundation-voice.asr :as asr]
            [com.blockether.vis.ext.foundation-voice.engine :as engine]
            [com.blockether.vis.ext.foundation-voice.sherpa :as sherpa]
            [lazytest.core :refer [defdescribe expect it]]))

(defn- answers
  "A `model-state` that walks a script and then stays on its last answer."
  [script]
  (let [remaining (atom script)]
    (fn []
      (let [[head & more] @remaining]
        (when (seq more) (reset! remaining more))
        head))))

;; Regression, user report: with the model already installed, every recording still failed
;; with the same message and the only cure anyone found was restarting Vis. `model-state`
;; holds a FAILED transfer until something starts a new one, and the wait loop threw the
;; moment it saw one - so a single dropped connection was a verdict on the whole process.
(defdescribe
  a-failed-download-is-not-a-verdict-test
  (it "buys one more attempt, and the recording goes through"
      (let
        [downloads
         (atom 0)

         phases
         (atom [])]

        (with-redefs
          [asr/model-state
           (answers [{:state :failed :error "connection reset"} {:state :ready :progress 100}])

           asr/start-download!
           (fn []
             (swap! downloads inc)
             nil)

           asr/model-dir
           (constantly "model-dir")

           asr/transcribe-file!
           (fn [_dir _audio _opts]
             "it speaks again")

           sherpa/ensure-native!
           (constantly true)]

          (expect (= "it speaks again"
                     (engine/transcribe {:audio-path "clip.wav"
                                         :on-progress (fn [m]
                                                        (swap! phases conj (:phase m)))})))
          (expect (= 1 @downloads) "the retry is a real new download, not a re-read of the verdict")
          (expect (= [:preparing] (distinct @phases)) "and the human is told it is preparing"))))
  (it "reports the second failure in a row, with the reason the transfer gave"
      (with-redefs
        [asr/model-state
         (constantly {:state :failed :error "connection reset"})

         asr/start-download!
         (constantly nil)

         asr/model-dir
         (constantly "model-dir")

         asr/transcribe-file!
         (fn [_dir _audio _opts]
           "never reached")

         sherpa/ensure-native!
         (constantly true)]

        (let [thrown (try (engine/transcribe {:audio-path "clip.wav"}) nil (catch Throwable t t))]
          (expect (some? thrown))
          (expect (= :voice-asr/model-download-failed (:type (ex-data thrown))))
          (expect (str/includes? (ex-message thrown) "connection reset"))))))

;; Regression, user report: voice worked again only after restarting Vis. A class whose static
;; initializer already met a missing library can never load again in the same JVM, so the
;; refusal has to SAY restart - repeating the linker error taught nobody anything.
(defdescribe
  an-unlinkable-runtime-asks-for-the-restart-it-needs-test
  (it "translates the linker failure into advice, and keeps the failure as the cause"
      (with-redefs
        [asr/model-state
         (constantly {:state :ready :progress 100})

         asr/model-dir
         (constantly "model-dir")

         asr/transcribe-file!
         (fn [_dir _audio _opts]
           (throw (NoClassDefFoundError. "com/k2fsa/sherpa/onnx/OfflineRecognizer")))

         sherpa/ensure-native!
         (constantly true)]

        (let [thrown (try (engine/transcribe {:audio-path "clip.wav"}) nil (catch Throwable t t))]
          (expect (some? thrown))
          (expect (= :voice/native-unavailable (:type (ex-data thrown))))
          (expect (true? (:is-restart-required (ex-data thrown))))
          (expect (str/includes? (ex-message thrown) "restart Vis"))
          (expect (instance? NoClassDefFoundError (ex-cause thrown)))))))
