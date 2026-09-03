(ns com.blockether.vis.internal.speech.engine-test
  "Parakeet transcript cleanup and native-runtime failures."
  (:require [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.internal.speech.asr :as asr]
            [com.blockether.vis.internal.speech.engine :as engine]
            [com.blockether.vis.internal.speech.sherpa :as sherpa]
            [lazytest.core :refer [around-each defdescribe expect it set-ns-context!]]))

;; `call-native` RECORDS a terminal linker failure in the host's capability registry, and that
;; verdict is process-wide BY DESIGN: a class whose static initializer failed can never load again
;; in this JVM. The last test here drives exactly that path, so without this every later voice test
;; sharing the JVM met the verdict it left behind instead of provisioning its own runtime.
(set-ns-context! [(around-each [f] (try (f) (finally (vis/capability-forget-verdicts!))))])

(defn- wav-path
  "A REAL (empty) 16-bit PCM WAV on disk, deleted when the JVM exits.

   The engine now converts anything that is not RIFF/WAVE before it reaches the
   model (`transcode/with-wav`), so a made-up `\"clip.wav\"` no longer reaches the
   stub below — it reaches ffmpeg, which correctly refuses a file that does not
   exist. What these tests are about is the recorder's OWN output, which is a wav."
  ^String []
  (let [file
        (java.io.File/createTempFile "vis-engine-test" ".wav")

        header
        (byte-array 46)]

    (.deleteOnExit file)
    (System/arraycopy (.getBytes "RIFF" "US-ASCII") 0 header 0 4)
    (System/arraycopy (.getBytes "WAVE" "US-ASCII") 0 header 8 4)
    (with-open [out (java.io.FileOutputStream. file)]
      (.write out header))
    (.getPath file)))


;; Regression, user report: voice worked again only after restarting Vis. A class whose static
;; initializer already met a missing library can never load again in the same JVM, so the
;; refusal has to SAY restart - repeating the linker error taught nobody anything.
(defdescribe
  an-unlinkable-runtime-asks-for-the-restart-it-needs-test
  (it "translates the linker failure into advice, and keeps the failure as the cause"
      (with-redefs [asr/model-dir
                    (constantly "model-dir")

                    asr/transcribe-file!
                    (fn [_dir _audio _opts]
                      (throw (NoClassDefFoundError. "com/k2fsa/sherpa/onnx/OfflineRecognizer")))

                    sherpa/ensure-native!
                    (constantly true)]

        (let [thrown (try (engine/transcribe {:audio-path (wav-path)}) nil (catch Throwable t t))]
          (expect (some? thrown))
          (expect (= :speech/native-unavailable (:type (ex-data thrown))))
          (expect (true? (:is-restart-required (ex-data thrown))))
          (expect (str/includes? (ex-message thrown) "restart Vis"))
          (expect (instance? NoClassDefFoundError (ex-cause thrown)))))))

(defdescribe transcript-cleanup-test
             (it "removes filler sounds and adjacent stutters in the gateway engine"
                 (expect (= "I want to add this to the transcript"
                            (engine/clean-transcript
                              "uh I I want to you know add add this to to the transcript")))
                 (expect (= "I want to fix it"
                            (engine/clean-transcript "I, I, I, I want to fix it")))
                 (expect (= "don't worry" (engine/clean-transcript "don't don't worry")))))
