(ns com.blockether.vis.ext.voice.asr
  "Direct Java sherpa-onnx integration for Parakeet TDT ASR."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.core :as vis])
  (:import [com.k2fsa.sherpa.onnx
            OfflineModelConfig OfflineRecognizer OfflineRecognizerConfig
            OfflineStream OfflineTransducerModelConfig WaveReader]
           [java.io File FileInputStream FileOutputStream]
           [java.net URL]
           [org.apache.commons.compress.archivers.tar TarArchiveInputStream]
           [org.apache.commons.compress.compressors.bzip2 BZip2CompressorInputStream]))

(def model-dir-env "VIS_PARAKEET_MODEL_DIR")

(def model-url
  "https://github.com/k2-fsa/sherpa-onnx/releases/download/asr-models/sherpa-onnx-nemo-parakeet-tdt-0.6b-v3-int8.tar.bz2")

(def default-model-dir
  "~/.vis model path used when no env/config override is set."
  (str (System/getProperty "user.home")
    "/.vis/models/sherpa-onnx-nemo-parakeet-tdt-0.6b-v3-int8"))

(defn model-dir
  []
  (or (some-> (vis/extension-env-value model-dir-env) str str/trim not-empty)
    (some-> (System/getenv model-dir-env) str str/trim not-empty)
    default-model-dir))

(defn model-files
  ([] (model-files (model-dir)))
  ([dir]
   {:encoder (str (io/file dir "encoder.int8.onnx"))
    :decoder (str (io/file dir "decoder.int8.onnx"))
    :joiner  (str (io/file dir "joiner.int8.onnx"))
    :tokens  (str (io/file dir "tokens.txt"))}))

(defn model-installed?
  ([] (model-installed? (model-dir)))
  ([dir]
   (every? #(.isFile (io/file %)) (vals (model-files dir)))))

(def ^:private onnxruntime-version "1.17.1")

(defn- native-platform
  []
  (let [os   (str/lower-case (System/getProperty "os.name" "generic"))
        arch (str/lower-case (System/getProperty "os.arch" "generic"))
        os'  (cond
               (or (str/includes? os "mac") (str/includes? os "darwin")) "osx"
               (str/includes? os "win") "win"
               (str/includes? os "nux") "linux"
               :else (throw (ex-info "Unsupported OS for sherpa-onnx"
                              {:type :voice-asr/unsupported-native-platform
                               :os os
                               :arch arch})))
        arch' (cond
                (or (str/starts-with? arch "amd64")
                  (str/starts-with? arch "x86_64")) "x64"
                (str/starts-with? arch "aarch64") "aarch64"
                :else (throw (ex-info "Unsupported architecture for sherpa-onnx"
                               {:type :voice-asr/unsupported-native-platform
                                :os os
                                :arch arch})))]
    (str os' "-" arch')))

(defn- onnxruntime-resource-name
  [platform]
  (str "ai/onnxruntime/native/" platform "/"
    (cond
      (str/starts-with? platform "win-") "onnxruntime.dll"
      (str/starts-with? platform "osx-") "libonnxruntime.dylib"
      :else "libonnxruntime.so")))

(defn- onnxruntime-target-names
  [platform]
  (cond
    (str/starts-with? platform "win-") ["onnxruntime.dll"]
    (str/starts-with? platform "osx-") [(str "libonnxruntime." onnxruntime-version ".dylib")
                                        "libonnxruntime.dylib"]
    :else [(str "libonnxruntime.so." onnxruntime-version)
           "libonnxruntime.so"]))

(defn- native-lib-dir
  [platform]
  (io/file (System/getProperty "user.home") "lib" platform))

(defn- resource-stream
  [path]
  (or (some-> (Thread/currentThread)
        .getContextClassLoader
        (.getResourceAsStream path))
    (some-> (ClassLoader/getSystemClassLoader)
      (.getResourceAsStream path))))

(defn- ensure-onnxruntime-native!
  "sherpa-onnx's JNI dylib links against libonnxruntime by filename in the same
   ~/lib/<platform> directory. The Microsoft ONNX Runtime jar carries that native
   library as a resource, but it does not place the versioned filename sherpa
   expects. Copy it there before sherpa's LibraryLoader calls System/load."
  []
  (let [platform (native-platform)
        dir      (native-lib-dir platform)
        resource (onnxruntime-resource-name platform)
        targets  (mapv #(io/file dir %) (onnxruntime-target-names platform))]
    (.mkdirs dir)
    (doseq [target targets]
      (when-not (.isFile target)
        (with-open [in (or (resource-stream resource)
                         (throw (ex-info "ONNX Runtime native library resource not found"
                                  {:type :voice-asr/missing-onnxruntime-native
                                   :resource resource
                                   :platform platform})))
                    out (FileOutputStream. target)]
          (io/copy in out))))
    (first targets)))

(defn- download!
  [url path]
  (.mkdirs (.getParentFile (io/file path)))
  (with-open [in  (.openStream (URL. url))
              out (FileOutputStream. (io/file path))]
    (io/copy in out))
  path)

(defn- safe-entry-name
  [entry-name]
  (let [parts (->> (str/split entry-name #"/")
                (remove str/blank?)
                ;; Release archives contain a top-level directory. Strip it so
                ;; custom VIS_PARAKEET_MODEL_DIR targets get the files directly.
                rest)]
    (when (and (seq parts)
            (not-any? #(or (= % "..") (str/includes? % "\\")) parts))
      (str/join File/separator parts))))

(defn- extract-tar-bz2!
  [archive-path target-dir]
  (.mkdirs (io/file target-dir))
  (with-open [fis (FileInputStream. (io/file archive-path))
              bz  (BZip2CompressorInputStream. fis)
              tar (TarArchiveInputStream. bz)]
    (loop []
      (when-let [entry (.getNextTarEntry tar)]
        (when-let [relative (safe-entry-name (.getName entry))]
          (let [out-file (io/file target-dir relative)]
            (if (.isDirectory entry)
              (.mkdirs out-file)
              (do
                (.mkdirs (.getParentFile out-file))
                (with-open [out (FileOutputStream. out-file)]
                  (io/copy tar out))))))
        (recur))))
  target-dir)

(defn ensure-model!
  "Download and extract Parakeet int8 model files if missing. Pure JVM path:
   URL download + commons-compress tar.bz2 extraction. Returns model dir."
  ([] (ensure-model! (model-dir)))
  ([dir]
   (if (model-installed? dir)
     dir
     (let [archive (File/createTempFile "vis-voice-asr-model-" ".tar.bz2")]
       (try
         (vis/notify! "Downloading Parakeet ASR model (~465MB)..." :level :info :ttl-ms 5000)
         (download! model-url archive)
         (vis/notify! "Extracting Parakeet ASR model..." :level :info :ttl-ms 5000)
         (extract-tar-bz2! archive dir)
         (when-not (model-installed? dir)
           (throw (ex-info "Parakeet model download did not produce expected files"
                    {:type :voice-asr/download-incomplete
                     :model-dir dir
                     :expected (model-files dir)})))
         dir
         (finally
           (try (.delete archive) (catch Throwable _))))))))

(defn- assert-files!
  [files]
  (doseq [[k path] files]
    (when-not (.isFile (io/file path))
      (throw (ex-info (str "Missing Parakeet model file: " path)
               {:type :voice-asr/missing-model-file
                :key k
                :path path
                :model-dir (model-dir)
                :remediation (str "Download sherpa-onnx-nemo-parakeet-tdt-0.6b-v3-int8 into "
                               (model-dir)
                               " or set " model-dir-env ".")}))))
  files)

(defn- recognizer
  [{:keys [encoder decoder joiner tokens]}]
  (ensure-onnxruntime-native!)
  (let [transducer (.. (OfflineTransducerModelConfig/builder)
                     (setEncoder encoder)
                     (setDecoder decoder)
                     (setJoiner joiner)
                     build)
        model      (.. (OfflineModelConfig/builder)
                     (setTransducer transducer)
                     (setTokens tokens)
                     (setNumThreads (max 1 (.availableProcessors (Runtime/getRuntime))))
                     (setDebug false)
                     (setModelType "nemo_transducer")
                     build)
        config     (.. (OfflineRecognizerConfig/builder)
                     (setOfflineModelConfig model)
                     (setDecodingMethod "greedy_search")
                     build)]
    (OfflineRecognizer. config)))

(def min-audio-seconds
  "Minimum microphone audio length sent to Parakeet ASR.
   Very short clips either transcribe blank or trigger opaque ONNX Conv_quant
   shape errors, so reject them before inference."
  1.0)

(defn- audio-stats
  [^WaveReader reader]
  (let [samples     (alength ^floats (.getSamples reader))
        sample-rate (.getSampleRate reader)
        duration    (if (pos? sample-rate)
                      (/ samples (double sample-rate))
                      0.0)]
    {:samples samples
     :sample-rate sample-rate
     :duration-seconds duration}))

(defn- assert-audio-long-enough!
  [audio-path ^WaveReader reader]
  (let [{:keys [duration-seconds] :as stats} (audio-stats reader)]
    (when (< duration-seconds min-audio-seconds)
      (throw (ex-info "Voice recording too short - try again"
               (assoc stats
                 :type :voice-asr/audio-too-short
                 :path (str audio-path)
                 :min-duration-seconds min-audio-seconds))))))

(defn transcribe-file!
  "Transcribe `audio-path` with local Parakeet TDT int8 through the sherpa-onnx
   Java API. Auto-downloads the model on first use. Returns plain text."
  ([audio-path] (transcribe-file! (model-dir) audio-path))
  ([dir audio-path]
   (let [dir   (ensure-model! dir)
         files (assert-files! (model-files dir))
         ^File audio-file (io/file audio-path)]
     (when-not (.isFile audio-file)
       (throw (ex-info (str "Missing audio file: " audio-path)
                {:type :voice-asr/missing-audio-file
                 :path (str audio-path)})))
     (let [reader (WaveReader. (str audio-file))
           _      (assert-audio-long-enough! audio-path reader)
           r      (recognizer files)
           stream (.createStream r)]
       (try
         (.acceptWaveform ^OfflineStream stream (.getSamples reader) (.getSampleRate reader))
         (.decode r stream)
         (str/trim (.. r (getResult stream) getText))
         (finally
           (try (.release stream) (catch Throwable _))
           (try (.release r) (catch Throwable _))))))))
