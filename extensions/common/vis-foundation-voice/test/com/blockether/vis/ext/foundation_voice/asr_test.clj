(ns com.blockether.vis.ext.foundation-voice.asr-test
  (:require [clojure.java.io :as io]
            [com.blockether.vis.ext.foundation-voice.asr :as asr]
            [lazytest.core :refer [defdescribe it expect]])
  (:import [java.io ByteArrayInputStream File FileOutputStream]
           [javax.sound.sampled AudioFileFormat$Type AudioFormat AudioInputStream AudioSystem]
           [org.apache.commons.compress.archivers.tar TarArchiveEntry TarArchiveOutputStream]
           [org.apache.commons.compress.compressors.bzip2 BZip2CompressorOutputStream]))

(defn- write-silence-wav!
  [^java.io.File file seconds]
  (let
    [format
     (AudioFormat. 16000.0 16 1 true false)

     frame-count
     (long (* 16000 seconds))

     audio-bytes
     (byte-array (* frame-count 2))

     stream
     (AudioInputStream. (ByteArrayInputStream. audio-bytes) format frame-count)]

    (AudioSystem/write stream AudioFileFormat$Type/WAVE file)
    file))

(defdescribe
  asr-test
  (it "uses the Parakeet int8 model file convention"
      (expect (= {:encoder "/m/encoder.int8.onnx"
                  :decoder "/m/decoder.int8.onnx"
                  :joiner "/m/joiner.int8.onnx"
                  :tokens "/m/tokens.txt"}
                 (asr/model-files "/m"))))
  (it "detects installed model files"
      (let
        [dir (.toFile (java.nio.file.Files/createTempDirectory
                        "vis-voice-asr-test"
                        (make-array java.nio.file.attribute.FileAttribute 0)))]
        (doseq [name ["encoder.int8.onnx" "decoder.int8.onnx" "joiner.int8.onnx" "tokens.txt"]]
          (spit (io/file dir name) "x"))
        (expect (true? (asr/model-installed? (str dir))))))
  (it "reports missing model files before inference without downloading in this test"
      (with-redefs [asr/ensure-model! identity]
        (try (asr/transcribe-file! "/definitely/missing/model" "/tmp/no.wav")
             (expect false)
             (catch clojure.lang.ExceptionInfo e
               (expect (= :voice-asr/missing-model-file
                          (-> e
                              ex-data
                              :type)))))))
  (it "accepts a well-formed PCM16 WAV"
      (let [wav (java.io.File/createTempFile "vis-voice-asr-valid" ".wav")]
        (write-silence-wav! wav 1.0)
        (expect (= (str wav) (asr/validate-wav-file! (str wav))))))
  (it "rejects a truncated WAV whose header declares more data than the file holds"
      ;; the exact shape a cut-off upload / interrupted write lands on disk as -
      ;; handing this to the native WaveReader SIGSEGVs the whole JVM
      (let [wav (java.io.File/createTempFile "vis-voice-asr-truncated" ".wav")]
        (write-silence-wav! wav 2.0)
        (let
          [bytes (java.nio.file.Files/readAllBytes (.toPath wav))
           cut (java.util.Arrays/copyOf bytes (int (/ (alength bytes) 4)))]

          (java.nio.file.Files/write (.toPath wav)
                                     cut
                                     ^"[Ljava.nio.file.OpenOption;"
                                     (make-array java.nio.file.OpenOption 0)))
        (try (asr/validate-wav-file! (str wav))
             (expect false)
             (catch clojure.lang.ExceptionInfo e
               (expect (= :voice-asr/invalid-wav
                          (-> e
                              ex-data
                              :type)))))))
  (it "rejects a body with RIFF/WAVE magic but a garbage chunk table"
      (let [wav (java.io.File/createTempFile "vis-voice-asr-garbage" ".wav")]
        (with-open [out (io/output-stream wav)]
          (.write out (.getBytes "RIFFxxxxWAVE" "US-ASCII"))
          (.write out (byte-array (repeat 64 (byte 0x7f)))))
        (try (asr/validate-wav-file! (str wav))
             (expect false)
             (catch clojure.lang.ExceptionInfo e
               (expect (= :voice-asr/invalid-wav
                          (-> e
                              ex-data
                              :type)))))))
  (it "rejects a non-WAV body outright"
      (let [wav (java.io.File/createTempFile "vis-voice-asr-notwav" ".wav")]
        (spit wav (apply str (repeat 100 "not a wave file ")))
        (try (asr/validate-wav-file! (str wav))
             (expect false)
             (catch clojure.lang.ExceptionInfo e
               (expect (= :voice-asr/invalid-wav
                          (-> e
                              ex-data
                              :type)))))))
  (it "surfaces invalid WAVs from transcribe-file! before any native code runs"
      (let
        [dir
         (.toFile (java.nio.file.Files/createTempDirectory
                    "vis-voice-asr-model-test"
                    (make-array java.nio.file.attribute.FileAttribute 0)))

         wav
         (java.io.File/createTempFile "vis-voice-asr-truncated2" ".wav")]

        (doseq [name ["encoder.int8.onnx" "decoder.int8.onnx" "joiner.int8.onnx" "tokens.txt"]]
          (spit (io/file dir name) "x"))
        (write-silence-wav! wav 2.0)
        (let
          [bytes
           (java.nio.file.Files/readAllBytes (.toPath wav))

           cut
           (java.util.Arrays/copyOf bytes (int (/ (alength bytes) 4)))]

          (java.nio.file.Files/write (.toPath wav)
                                     cut
                                     ^"[Ljava.nio.file.OpenOption;"
                                     (make-array java.nio.file.OpenOption 0)))
        (with-redefs [asr/ensure-model! identity]
          (try (asr/transcribe-file! (str dir) (str wav))
               (expect false)
               (catch clojure.lang.ExceptionInfo e
                 (expect (= :voice-asr/invalid-wav
                            (-> e
                                ex-data
                                :type))))))))
  (it
    "rejects empty or too-short recordings before ONNX inference"
    (let
      [dir
       (.toFile (java.nio.file.Files/createTempDirectory
                  "vis-voice-asr-model-test"
                  (make-array java.nio.file.attribute.FileAttribute 0)))

       wav
       (java.io.File/createTempFile "vis-voice-asr-too-short" ".wav")]

      (doseq [name ["encoder.int8.onnx" "decoder.int8.onnx" "joiner.int8.onnx" "tokens.txt"]]
        (spit (io/file dir name) "x"))
      (write-silence-wav! wav 0.0)
      (with-redefs [asr/ensure-model! identity]
        (try (asr/transcribe-file! (str dir) (str wav))
             (expect false)
             (catch clojure.lang.ExceptionInfo e
               (expect (= "Voice recording too short - try again" (ex-message e)))
               (expect (= :voice-asr/audio-too-short
                          (-> e
                              ex-data
                              :type)))
               (expect (= 0
                          (-> e
                              ex-data
                              :samples)))
               (expect (= asr/min-audio-seconds
                          (-> e
                              ex-data
                              :min-duration-seconds))))
             (catch java.lang.LinkageError _
               ;; No sherpa-onnx native here (CI lacks libonnxruntime): the length
               ;; check needs the native WaveReader to read the audio, so it can't
               ;; run at all. Skip rather than fail on the missing shared object.
               (expect true)))))))

(defn- fake-model-archive!
  "A tiny tar.bz2 shaped like the release archive — a top-level directory holding
   the four model files — so the install path can be driven without the network."
  ^File []
  (let
    [archive
     (File/createTempFile "vis-voice-model-test-" ".tar.bz2")

     payload
     (byte-array (* 512 1024))]

    (with-open
      [out
       (FileOutputStream. archive)

       bz
       (BZip2CompressorOutputStream. out)

       tar
       (TarArchiveOutputStream. bz)]

      (doseq [file-name ["encoder.int8.onnx" "decoder.int8.onnx" "joiner.int8.onnx" "tokens.txt"]]
        (let [entry (TarArchiveEntry. (str "sherpa-onnx-model/" file-name))]
          (.setSize entry (alength payload))
          (.putArchiveEntry tar entry)
          (.write tar payload)
          (.closeArchiveEntry tar))))
    archive))

(defdescribe
  model-install-progress-test
  (it "reports progress through the UNPACK too, not only the transfer"
      ;; Regression: the download reported 0..99 and extraction reported nothing,
      ;; so a ~465MB bzip2 archive sat on "99%" for minutes and looked hung.
      (let
        [archive
         (fake-model-archive!)

         target
         (str (io/file (System/getProperty "java.io.tmpdir")
                       (str "vis-voice-model-dir-" (System/nanoTime))))

         seen
         (atom [])]

        (try (with-redefs [asr/model-url (str (.toURI archive))]
               (#'asr/install-model!
                target
                (fn [update]
                  (swap! seen conj update))))
             (let
               [updates
                @seen

                downloading
                (filter #(= :downloading (:phase %)) updates)

                extracting
                (filter #(= :extracting (:phase %)) updates)]

               (expect (seq downloading))
               (expect (seq extracting))
               (expect (every? #(<= 0 (:progress %) 89) downloading))
               (expect (every? #(<= 90 (:progress %) 99) extracting))
               (expect (true? (boolean (#'asr/model-installed? target)))))
             (finally (.delete archive) (#'asr/delete-dir! (io/file target)))))))

(defdescribe
  onnxruntime-native-test
  (it "materialises the VERSIONED onnxruntime dylib sherpa's JNI links against"
      ;; Regression: sherpa's libsherpa-onnx-jni links @rpath/libonnxruntime.<ver>
      ;; from ~/lib/<platform>. The onnxruntime jar only carries the UNversioned
      ;; name, so without this copy the first sherpa class touched dies with
      ;; UnsatisfiedLinkError "Library not loaded: @rpath/libonnxruntime.1.17.1.dylib".
      (let
        [home
         (io/file (System/getProperty "java.io.tmpdir") (str "vis-voice-home-" (System/nanoTime)))

         previous
         (System/getProperty "user.home")]

        (try (.mkdirs home)
             (System/setProperty "user.home" (str home))
             (let
               [platform
                (#'asr/native-platform)

                dir
                (io/file home "lib" platform)]

               (#'asr/ensure-onnxruntime-native!)
               (doseq [name (#'asr/onnxruntime-target-names platform)]
                 (expect (.isFile (io/file dir name)))))
             (finally (System/setProperty "user.home" previous) (#'asr/delete-dir! home))))))
