(ns com.blockether.vis.ext.voice-parakeet.sherpa-test
  (:require [clojure.java.io :as io]
            [com.blockether.vis.ext.voice-parakeet.sherpa :as sherpa]
            [lazytest.core :refer [defdescribe it expect]]))

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
          (expect (= :voice-parakeet/missing-model-file (-> e ex-data :type))))))))
