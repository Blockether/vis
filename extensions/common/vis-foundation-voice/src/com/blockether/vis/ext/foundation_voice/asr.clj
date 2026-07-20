(ns com.blockether.vis.ext.foundation-voice.asr
  "Direct Java sherpa-onnx integration for Parakeet TDT ASR."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.internal.paths :as paths])
  (:import [com.k2fsa.sherpa.onnx OfflineModelConfig OfflineRecognizer OfflineRecognizerConfig
            OfflineStream OfflineTransducerModelConfig WaveReader]
           [java.io File FileInputStream FileOutputStream]
           [java.net URL]
           [org.apache.commons.compress.archivers.tar TarArchiveInputStream]
           [org.apache.commons.compress.compressors.bzip2 BZip2CompressorInputStream]))

;; Reflective interop is FATAL in the native image (needs metadata per call
;; site) — keep this ns reflection-free at compile time. The bundled-model
;; installer's untyped `(or (resource-stream …))` with-open already shipped
;; one such failure ("Cannot reflectively invoke ByteArrayInputStream.close").
(set! *warn-on-reflection* true)

(def model-dir-env "VIS_PARAKEET_MODEL_DIR")

(def model-url
  "https://github.com/k2-fsa/sherpa-onnx/releases/download/asr-models/sherpa-onnx-nemo-parakeet-tdt-0.6b-v3-int8.tar.bz2")

(def default-model-dir
  "~/.vis model path used when no env/config override is set."
  (str (System/getProperty "user.home") "/.vis/models/sherpa-onnx-nemo-parakeet-tdt-0.6b-v3-int8"))

(defn model-dir
  []
  (or (some-> (vis/extension-env-value model-dir-env)
              str
              str/trim
              not-empty)
      (some-> (System/getenv model-dir-env)
              str
              str/trim
              not-empty)
      default-model-dir))

(defn model-files
  ([] (model-files (model-dir)))
  ([dir]
   ;; `/`-separated on every OS — Windows native loaders accept `/`, and the
   ;; model address stays identical across platforms.
   (let
     [p (fn [name]
          (paths/unixify (io/file dir name)))]
     {:encoder (p "encoder.int8.onnx")
      :decoder (p "decoder.int8.onnx")
      :joiner (p "joiner.int8.onnx")
      :tokens (p "tokens.txt")})))

(defn model-installed?
  ([] (model-installed? (model-dir)))
  ([dir] (every? #(.isFile (io/file %)) (vals (model-files dir)))))

(def ^:private onnxruntime-version "1.17.1")

(defn- native-platform
  []
  (let
    [os
     (str/lower-case (System/getProperty "os.name" "generic"))

     arch
     (str/lower-case (System/getProperty "os.arch" "generic"))

     os'
     (cond (or (str/includes? os "mac") (str/includes? os "darwin")) "osx"
           (str/includes? os "win") "win"
           (str/includes? os "nux") "linux"
           :else (throw (ex-info "Unsupported OS for sherpa-onnx"
                                 {:type :voice-asr/unsupported-native-platform :os os :arch arch})))

     arch'
     (cond (or (str/starts-with? arch "amd64") (str/starts-with? arch "x86_64")) "x64"
           (str/starts-with? arch "aarch64") "aarch64"
           :else (throw (ex-info
                          "Unsupported architecture for sherpa-onnx"
                          {:type :voice-asr/unsupported-native-platform :os os :arch arch})))]

    (str os' "-" arch')))

(defn- onnxruntime-resource-name
  [platform]
  (str "ai/onnxruntime/native/" platform
       "/" (cond (str/starts-with? platform "win-") "onnxruntime.dll"
                 (str/starts-with? platform "osx-") "libonnxruntime.dylib"
                 :else "libonnxruntime.so")))

(defn- onnxruntime-target-names
  [platform]
  (cond (str/starts-with? platform "win-") ["onnxruntime.dll"]
        (str/starts-with? platform "osx-") [(str "libonnxruntime." onnxruntime-version ".dylib")
                                            "libonnxruntime.dylib"]
        :else [(str "libonnxruntime.so." onnxruntime-version) "libonnxruntime.so"]))

(defn- native-lib-dir [platform] (io/file (System/getProperty "user.home") "lib" platform))

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
  (let
    [platform
     (native-platform)

     ^File dir
     (native-lib-dir platform)

     resource
     (onnxruntime-resource-name platform)

     targets
     (mapv #(io/file dir %) (onnxruntime-target-names platform))]

    (.mkdirs dir)
    (doseq [^File target targets]
      (when-not (.isFile target)
        ;; ^InputStream: `(or …)` erases the type and with-open's `.close`
        ;; goes REFLECTIVE — works on the JVM, but in a native image it
        ;; needs reflection metadata and fails without it.
        (with-open
          [^java.io.InputStream in (or (resource-stream resource)
                                       (throw (ex-info
                                                "ONNX Runtime native library resource not found"
                                                {:type :voice-asr/missing-onnxruntime-native
                                                 :resource resource
                                                 :platform platform})))
           out (FileOutputStream. target)]

          (io/copy in out))))
    (first targets)))

(defn- safe-entry-name
  [entry-name]
  (let
    [parts (->> (str/split entry-name #"/")
                (remove str/blank?)
                ;; Release archives contain a top-level directory. Strip it so
                ;; custom VIS_PARAKEET_MODEL_DIR targets get the files directly.
                rest)]
    (when (and (seq parts) (not-any? #(or (= % "..") (str/includes? % "\\")) parts))
      (str/join File/separator parts))))

(defn- extract-tar-bz2!
  [archive-path target-dir]
  (.mkdirs (io/file target-dir))
  (with-open
    [fis
     (FileInputStream. (io/file archive-path))

     bz
     (BZip2CompressorInputStream. fis)

     tar
     (TarArchiveInputStream. bz)]

    (loop []

      (when-let [entry (.getNextTarEntry tar)]
        (when-let [relative (safe-entry-name (.getName entry))]
          (let [out-file (io/file target-dir relative)]
            (if (.isDirectory entry)
              (.mkdirs out-file)
              (do (.mkdirs (.getParentFile out-file))
                  (with-open [out (FileOutputStream. out-file)]
                    (io/copy tar out))))))
        (recur))))
  target-dir)

(defn- download-with-progress!
  "Stream `url` to `path`, calling `(on-progress pct)` (0..99) as bytes land
   when the server reports a content length. nil `on-progress` is fine."
  [url path on-progress]
  (.mkdirs (.getParentFile (io/file path)))
  (let
    [^java.net.URLConnection conn
     (.openConnection (URL. url))

     total
     (.getContentLengthLong conn)]

    (with-open
      [in
       (.getInputStream conn)

       out
       (FileOutputStream. (io/file path))]

      (let [buf (byte-array 1048576)]
        (loop [done 0]
          (let [n (.read in buf)]
            (when-not (neg? n)
              (.write out buf 0 n)
              (let [done' (+ (long done) (long n))]
                (when (and on-progress (pos? total))
                  (on-progress (min 99 (long (* 100 (/ (double done') (double total)))))))
                (recur done'))))))))
  path)

(defn- delete-dir!
  [^File f]
  (when (.isDirectory f)
    (doseq [c (.listFiles f)]
      (delete-dir! c)))
  (.delete f))

(def ^:private bundled-resource-dir "voice-assets/parakeet")
(def ^:private bundled-file-names
  ["encoder.int8.onnx" "decoder.int8.onnx" "joiner.int8.onnx" "tokens.txt"])

(defn bundled-model-available?
  "True when the ASR model is embedded as classpath resources — i.e. this is a
   `--with-assets` native build — so it can be installed WITHOUT a network
   download by copying the resources straight out of the binary."
  []
  (every? #(some? (resource-stream (str bundled-resource-dir "/" %))) bundled-file-names))

(defn- install-bundled-model!
  "Copy the embedded model resources into a STAGING dir, verify, then ATOMICALLY
   move into `dir` (same no-partial-files guarantee as the download path)."
  [dir]
  (let [staging (io/file (str dir ".staging-" (System/nanoTime)))]
    (try (.mkdirs staging)
         (doseq [name bundled-file-names]
           ;; ^InputStream: same reflective-`.close` trap as
           ;; ensure-onnxruntime-native! — fatal in a native image.
           (with-open
             [^java.io.InputStream in (or (resource-stream (str bundled-resource-dir "/" name))
                                          (throw (ex-info "bundled model resource missing"
                                                          {:type :voice-asr/bundled-missing
                                                           :resource name})))
              out (FileOutputStream. (io/file staging name))]

             (io/copy in out)))
         (when-not (model-installed? (str staging))
           (throw (ex-info "bundled model extraction incomplete"
                           {:type :voice-asr/bundled-incomplete :model-dir dir})))
         (let [final (io/file dir)]
           (when (.exists final) (delete-dir! final))
           (.mkdirs (.getParentFile final))
           (when-not (.renameTo staging final)
             (throw (ex-info "could not move bundled model into place"
                             {:type :voice-asr/install-failed :model-dir dir}))))
         dir
         (finally (try (when (.exists staging) (delete-dir! staging)) (catch Throwable _))))))

(defn- install-model!
  "Download + extract into a STAGING dir, verify all files, then ATOMICALLY
   move it into place. The final `dir` never holds partial files — an
   interrupted or corrupt download can't leave a truncated `.onnx` that
   native-aborts the JVM on the next load; it just stays absent. Returns dir."
  [dir on-progress]
  (let
    [archive
     (File/createTempFile "vis-voice-asr-model-" ".tar.bz2")

     staging
     (io/file (str dir ".staging-" (System/nanoTime)))]

    (try (download-with-progress! model-url (str archive) on-progress)
         (extract-tar-bz2! (str archive) (str staging))
         (when-not (model-installed? (str staging))
           (throw (ex-info "Parakeet model download did not produce expected files"
                           {:type :voice-asr/download-incomplete :model-dir dir})))
         (let [final (io/file dir)]
           (when (.exists final) (delete-dir! final))
           (.mkdirs (.getParentFile final))
           (when-not (.renameTo staging final)
             (throw (ex-info "Could not move the downloaded model into place"
                             {:type :voice-asr/install-failed :model-dir dir}))))
         dir
         (finally (try (.delete archive) (catch Throwable _))
                  (try (when (.exists staging) (delete-dir! staging)) (catch Throwable _))))))

(defn ensure-model!
  "Download + atomically install the Parakeet int8 model if missing (blocking).
   Returns model dir. Used by the TUI's synchronous voice path; the web drives
   a non-blocking download via `start-download!` + `model-state`."
  ([] (ensure-model! (model-dir)))
  ([dir]
   (cond (model-installed? dir) dir
         ;; --with-assets binary: extract the embedded model (no network)
         (bundled-model-available?)
         (do (vis/notify! "Installing bundled Parakeet ASR model..." :level :info :ttl-ms 5000)
             (install-bundled-model! dir)
             (vis/notify! "Parakeet ASR model ready." :level :info :ttl-ms 3000)
             dir)
         :else
         (do (vis/notify! "Downloading Parakeet ASR model (~465MB)..." :level :info :ttl-ms 5000)
             (install-model! dir nil)
             (vis/notify! "Parakeet ASR model ready." :level :info :ttl-ms 3000)
             dir))))

;; ── Async / UI-driven model lifecycle ──────────────────────────────────────
;; `:ready` is DERIVED from `model-installed?`, never stored, so it can't go
;; stale. The atom only tracks an in-flight or failed download.
(defonce ^:private download-state (atom nil))

(defn model-state
  "Current voice-model state for a UI to POLL:
     {:state :ready}                        model files installed
     {:state :downloading :progress 0..100} a background download is running
     {:state :failed :error \"…\"}          the last download failed
     {:state :absent}                       not installed, idle (no download)"
  []
  (cond (model-installed?) {:state :ready}
        (#{:downloading :failed} (:state @download-state)) @download-state
        :else {:state :absent}))

(defn start-download!
  "Idempotent, NON-blocking: if the model is absent and no download is already
   running, start one on a background thread (progress tracked in the atom).
   Returns the current `model-state` immediately."
  []
  (locking download-state
    (when (and (not (model-installed?)) (not= :downloading (:state @download-state)))
      (reset! download-state {:state :downloading :progress 0})
      (future (try (if (bundled-model-available?)
                     ;; --with-assets binary: copy the embedded model out (no network)
                     (install-bundled-model! (model-dir))
                     (install-model! (model-dir)
                                     (fn [pct]
                                       (swap! download-state assoc :progress pct))))
                   (reset! download-state nil) ; model-state now derives :ready
                   (catch Throwable t
                     (reset! download-state {:state :failed
                                             :error (or (ex-message t) "download failed")}))))))
  (model-state))

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
                                         " or set "
                                         model-dir-env
                                         ".")}))))
  files)

(defn- recognizer
  [{:keys [encoder decoder joiner tokens]}]
  (ensure-onnxruntime-native!)
  (let
    [transducer
     (.. (OfflineTransducerModelConfig/builder)
         (setEncoder encoder)
         (setDecoder decoder)
         (setJoiner joiner)
         build)

     model
     (.. (OfflineModelConfig/builder)
         (setTransducer transducer)
         (setTokens tokens)
         (setNumThreads (max 1 (.availableProcessors (Runtime/getRuntime))))
         (setDebug false)
         (setModelType "nemo_transducer")
         build)

     config
     (.. (OfflineRecognizerConfig/builder)
         (setOfflineModelConfig model)
         (setDecodingMethod "greedy_search")
         build)]

    (OfflineRecognizer. config)))

(defn- u16le
  ^long [^bytes b ^long off]
  (bit-or (bit-and (aget b off) 0xff) (bit-shift-left (bit-and (aget b (inc off)) 0xff) 8)))

(defn- u32le
  ^long [^bytes b ^long off]
  (bit-or (u16le b off) (bit-shift-left (u16le b (+ off 2)) 16)))

(defn validate-wav-file!
  "Structural RIFF/WAVE check in PURE JVM code before a file reaches
   sherpa-onnx's native WaveReader, which SIGSEGVs AND ABORTS THE WHOLE JVM
   on malformed input — including a well-formed header whose declared chunk
   sizes overrun the bytes actually present (a truncated upload or partial
   write; observed live). No catchable native exception is on offer, so
   every chunk in the table must fit inside the file, and a 16-bit PCM
   `fmt ` chunk plus a `data` chunk must both be present (the only shape
   either producer emits — ui.js's encoder and the TUI recorder — and the
   only one WaveReader reads). Throws ex-info :voice-asr/invalid-wav.
   Returns audio-path."
  [audio-path]
  (let
    [f
     (io/file audio-path)

     len
     (.length f)

     fail!
     (fn [reason data]
       (throw (ex-info
                (str "Voice audio is not a readable WAV file - " reason)
                (merge
                  {:type :voice-asr/invalid-wav :path (str audio-path) :reason reason :length len}
                  data))))]

    (when (< len 44) (fail! "shorter than a WAV header" {}))
    (with-open [in (java.io.DataInputStream. (java.io.BufferedInputStream. (io/input-stream f)))]
      (let [head (byte-array 12)]
        (.readFully in head)
        (when-not (and (= "RIFF" (String. head 0 4 "US-ASCII"))
                       (= "WAVE" (String. head 8 4 "US-ASCII")))
          (fail! "missing RIFF/WAVE magic" {})))
      (loop
        [pos 12
         pcm16? false
         data? false]

        (if (>= pos len)
          (do (when-not pcm16? (fail! "no 16-bit PCM fmt chunk" {}))
              (when-not data? (fail! "no data chunk" {})))
          (do (when (> (+ pos 8) len) (fail! "dangling bytes after the last chunk" {:at pos}))
              (let
                [hdr (byte-array 8)
                 _ (.readFully in hdr)
                 id (String. hdr 0 4 "US-ASCII")
                 size (u32le hdr 4)
                 end (+ pos 8 size)]

                (when (> end len)
                  (fail! "chunk declares more bytes than the file holds (truncated?)"
                         {:chunk id :declared-size size :at pos}))
                (let
                  [fmt-read? (and (= id "fmt ") (>= size 16))
                   pcm16? (or pcm16?
                              (and fmt-read?
                                   (let [fb (byte-array 16)]
                                     (.readFully in fb)
                                     (and (= 1 (u16le fb 0)) ; PCM
                                          (= 16 (u16le fb 14)))))) ; 16-bit
                   ;; chunks are word-aligned, but a final odd-sized
                   ;; chunk may legally arrive unpadded
                   next-pos (long (min len (+ end (rem size 2))))]

                  (loop [n (- next-pos (+ pos 8 (long (if fmt-read? 16 0))))]
                    (when (pos? n)
                      (let [s (.skipBytes in (int n))]
                        (when-not (pos? s) (fail! "unexpected EOF inside a chunk" {:chunk id}))
                        (recur (- n s)))))
                  (recur next-pos pcm16? (or data? (= id "data"))))))))))
  audio-path)

(def ^:const min-audio-seconds
  "Minimum microphone audio length sent to Parakeet ASR.
   Very short clips either transcribe blank or trigger opaque ONNX Conv_quant
   shape errors, so reject them before inference."
  1.0)

(defn- audio-stats
  [^WaveReader reader]
  (let
    [samples
     (alength ^floats (.getSamples reader))

     sample-rate
     (.getSampleRate reader)

     duration
     (if (pos? sample-rate) (/ samples (double sample-rate)) 0.0)]

    {:samples samples :sample-rate sample-rate :duration-seconds duration}))

(defn- assert-audio-long-enough!
  [audio-path ^WaveReader reader]
  (let [{:keys [duration-seconds] :as stats} (audio-stats reader)]
    (when (< (double duration-seconds) min-audio-seconds)
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
   (let
     [dir
      (ensure-model! dir)

      files
      (assert-files! (model-files dir))

      ^File audio-file
      (io/file audio-path)]

     (when-not (.isFile audio-file)
       (throw (ex-info (str "Missing audio file: " audio-path)
                       {:type :voice-asr/missing-audio-file :path (str audio-path)})))
     (validate-wav-file! audio-path)
     ;; every interop call below is TYPE-HINTED: reflective calls in a native
     ;; image only work when reflection metadata happens to cover them — the
     ;; hot path must not depend on that.
     (let
       [reader
        (WaveReader. (str audio-file))

        _
        (assert-audio-long-enough! audio-path reader)

        ^OfflineRecognizer r
        (recognizer files)

        ^OfflineStream stream
        (.createStream r)]

       (try (.acceptWaveform stream (.getSamples reader) (.getSampleRate reader))
            (.decode r stream)
            (str/trim (.getText (.getResult r stream)))
            (finally (try (.release stream) (catch Throwable _))
                     (try (.release r) (catch Throwable _))))))))
