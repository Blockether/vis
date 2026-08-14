(ns com.blockether.vis.ext.foundation-voice.asr-test
  (:require [clojure.java.io :as io]
            [com.blockether.vis.ext.foundation-voice.asr :as asr]
            [com.blockether.vis.ext.foundation-voice.files :as files]
            [com.blockether.vis.ext.foundation-voice.sherpa :as sherpa]
            [lazytest.core :refer [defdescribe it expect]])
  (:import [com.k2fsa.sherpa.onnx VersionInfo WaveReader]
           [java.io ByteArrayInputStream File FileOutputStream]
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
        ;; Every platform's native rides in its own jar now, so the WaveReader
        ;; this check needs always loads: a LinkageError here is a real failure
        ;; and is no longer swallowed as a skip.
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
                              :min-duration-seconds)))))))))

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

        (try (with-redefs
               [asr/model-asset
                (constantly {:id "parakeet-test"
                             :requires ["encoder.int8.onnx" "decoder.int8.onnx" "joiner.int8.onnx"
                                        "tokens.txt"]
                             :sources [{:host :test :kind :archive :url (str (.toURI archive))}]})]
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
             (finally (.delete archive) (files/delete-dir! (io/file target)))))))

;; Regression, issue #143: the repackaged 1.12.7 fork shipped only sherpa's JNI,
;; so the first sherpa class touched died with UnsatisfiedLinkError "Library not
;; loaded: @rpath/libonnxruntime.1.17.1.dylib" unless a hand-written copy step had
;; first materialised a versioned dylib in ~/lib/<platform> from a separately
;; pinned ONNX Runtime — and on Linux one minor off that pin killed every
;; transcription with "version `VERS_1.17.1' not found".
(defdescribe sherpa-native-test
             (it "runs the version the deps.edn pins, with the ONNX Runtime that jar carries"
                 ;; The classpath no longer carries any native library: `ensure-native!`
                 ;; is what puts THIS platform's pair where sherpa's loader finds it.
                 (sherpa/ensure-native!)
                 ;; Both are NATIVE methods: an answer at all means the JNI loaded, and the
                 ;; runtime it reports is the one shipped beside it — not a coordinate we
                 ;; pin, and no longer ours to keep in step.
                 (expect (= "1.13.5" (VersionInfo/getVersion)))
                 (expect (re-matches #"\d+\.\d+\.\d+" (VersionInfo/getOnnxruntimeVersion))))
             (it "reads a WAV through the native stack with nothing but the upstream jars"
                 ;; WaveReader's constructor is what triggers sherpa's LibraryLoader, and it
                 ;; is the first native call every transcription makes.
                 (sherpa/ensure-native!)
                 (let [wav (write-silence-wav! (File/createTempFile "vis-voice-native" ".wav") 1)]
                   (try (let [reader (WaveReader. (str wav))]
                          (expect (= 16000 (.getSampleRate reader)))
                          (expect (= 16000 (alength ^floats (.getSamples reader)))))
                        (finally (.delete wav))))))

(defdescribe chunk-plan-test
             ;; Progress used to be impossible to report at all: the whole recording went
             ;; into ONE offline `decode` call, so a two-minute clip was a black box.
             (it "a clip shorter than one chunk is decoded whole"
                 (expect (= [[0 16000]] (asr/chunk-plan 16000 16000 20.0)))
                 (expect (= [[0 320000]] (asr/chunk-plan 320000 16000 20.0))))
             (it "a long recording is cut on chunk boundaries, in order, covering every sample"
                 (let [plan (asr/chunk-plan 500000 16000 20.0)]
                   (expect (= [[0 320000] [320000 500000]] plan))
                   (expect (= 0 (ffirst plan)))
                   (expect (= 500000 (last (peek plan))))
                   ;; no gaps: each range starts where the previous ended
                   (expect (every? (fn [[[_ end] [start _]]]
                                     (= end start))
                                   (partition 2 1 plan)))))
             (it "a sliver of a tail is merged into the piece before it, never decoded alone"
                 ;; a 0.1s tail decodes blank or trips an ONNX shape error, and would still
                 ;; be reported as a whole step of progress
                 (let [plan (asr/chunk-plan 321600 16000 20.0)]
                   (expect (= [[0 321600]] plan)))
                 (let [plan (asr/chunk-plan 641600 16000 20.0)]
                   (expect (= [[0 320000] [320000 641600]] plan))))
             (it "degenerate input is answered, not thrown at"
                 (expect (= [] (asr/chunk-plan 0 16000 20.0)))
                 (expect (= [] (asr/chunk-plan -5 16000 20.0)))
                 (expect (= [[0 1000]] (asr/chunk-plan 1000 0 20.0)))
                 (expect (= [[0 1000]] (asr/chunk-plan 1000 16000 0.0)))))
