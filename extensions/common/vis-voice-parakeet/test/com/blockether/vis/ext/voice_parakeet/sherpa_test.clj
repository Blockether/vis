(ns com.blockether.vis.ext.voice-parakeet.sherpa-test
  (:require [clojure.java.io :as io]
            [com.blockether.vis.ext.voice-parakeet.sherpa :as sherpa]
            [lazytest.core :refer [defdescribe it expect]])
  (:import [java.io ByteArrayInputStream]
           [javax.sound.sampled AudioFileFormat$Type AudioFormat AudioInputStream AudioSystem]))

(defn- write-silence-wav!
  [file seconds]
  (let [format      (AudioFormat. 16000.0 16 1 true false)
        frame-count (long (* 16000 seconds))
        audio-bytes (byte-array (* frame-count 2))
        stream      (AudioInputStream. (ByteArrayInputStream. audio-bytes) format frame-count)]
    (AudioSystem/write stream AudioFileFormat$Type/WAVE file)
    file))

(defdescribe sherpa-test
  (it "uses the Parakeet int8 model file convention"
    (expect (= {:encoder "/m/encoder.int8.onnx"
                :decoder "/m/decoder.int8.onnx"
                :joiner  "/m/joiner.int8.onnx"
                :tokens  "/m/tokens.txt"}
              (sherpa/model-files "/m"))))

  (it "detects installed model files"
    (let [dir (.toFile (java.nio.file.Files/createTempDirectory "vis-parakeet-test" (make-array java.nio.file.attribute.FileAttribute 0)))]
      (doseq [name ["encoder.int8.onnx" "decoder.int8.onnx" "joiner.int8.onnx" "tokens.txt"]]
        (spit (io/file dir name) "x"))
      (expect (true? (sherpa/model-installed? (str dir))))))

  (it "reports missing model files before inference without downloading in this test"
    (with-redefs [sherpa/ensure-model! identity]
      (try
        (sherpa/transcribe-file! "/definitely/missing/model" "/tmp/no.wav")
        (expect false)
        (catch clojure.lang.ExceptionInfo e
          (expect (= :voice-parakeet/missing-model-file (-> e ex-data :type)))))))

  (it "rejects empty or too-short recordings before ONNX inference"
    (let [dir (.toFile (java.nio.file.Files/createTempDirectory "vis-parakeet-model-test" (make-array java.nio.file.attribute.FileAttribute 0)))
          wav (java.io.File/createTempFile "vis-parakeet-too-short" ".wav")]
      (doseq [name ["encoder.int8.onnx" "decoder.int8.onnx" "joiner.int8.onnx" "tokens.txt"]]
        (spit (io/file dir name) "x"))
      (write-silence-wav! wav 0.0)
      (with-redefs [sherpa/ensure-model! identity]
        (try
          (sherpa/transcribe-file! (str dir) (str wav))
          (expect false)
          (catch clojure.lang.ExceptionInfo e
            (expect (= "Voice recording too short — try again" (ex-message e)))
            (expect (= :voice-parakeet/audio-too-short (-> e ex-data :type)))
            (expect (= 0 (-> e ex-data :samples)))
            (expect (= sherpa/min-audio-seconds (-> e ex-data :min-duration-seconds)))))))))
