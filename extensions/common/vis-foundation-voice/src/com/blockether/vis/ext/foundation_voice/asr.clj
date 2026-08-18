(ns com.blockether.vis.ext.foundation-voice.asr
  "Direct Java sherpa-onnx integration for Parakeet TDT ASR."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.foundation-voice.assets :as assets]
            [com.blockether.vis.ext.foundation-voice.sherpa :as sherpa]
            [com.blockether.vis.internal.paths :as paths])
  (:import [com.k2fsa.sherpa.onnx OfflineModelConfig OfflineRecognizer OfflineRecognizerConfig
            OfflineStream OfflineTransducerModelConfig WaveReader]
           [java.io File]))

;; Reflective interop is FATAL in the native image (needs metadata per call
;; site) — keep this ns reflection-free at compile time. An untyped
;; `(or …)` binding inside `with-open` already shipped one such failure
;; ("Cannot reflectively invoke ByteArrayInputStream.close").
(set! *warn-on-reflection* true)

(def model-dir-env "VIS_PARAKEET_MODEL_DIR")

(def ^:const asset-id "parakeet-tdt-0.6b-v3-int8")

(defn model-asset
  "This model's manifest entry — where it may be fetched from, what verifies it
   and what it is licensed under. The URL used to be a literal here; it now lives
   in one file with an SPDX id beside it."
  []
  (assets/entry asset-id))

(defn default-model-dir
  "~/.vis model path used when no env/config override is set. A function, never a
   top-level `def`: `native-image` initializes this namespace at BUILD time, so a
   captured `user.home` would point every installed binary at the BUILDER's home."
  []
  (assets/install-dir (model-asset)))

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
      (default-model-dir)))

(defn model-files
  ([] (model-files (model-dir)))
  ([dir]
   ;; `/`-separated on every OS, so the model address stays identical across
   ;; platforms.
   (let [p (fn [name]
             (paths/unixify (io/file dir name)))]
     {:encoder (p "encoder.int8.onnx")
      :decoder (p "decoder.int8.onnx")
      :joiner (p "joiner.int8.onnx")
      :tokens (p "tokens.txt")})))

(defn model-installed?
  ([] (model-installed? (model-dir)))
  ([dir] (every? #(.isFile (io/file %)) (vals (model-files dir)))))

(defn- install-model!
  "Download + verify + ATOMICALLY install, through the manifest that knows where
   this model may come from. `on-progress` (optional) is called with
   {:phase :downloading|:extracting :progress 0..99}."
  [dir on-progress]
  (sherpa/ensure-native!)
  (assets/install! (model-asset) dir on-progress))

(defn ensure-model!
  "Download + atomically install the Parakeet int8 model if missing (blocking).
   Returns model dir. Used by the TUI's synchronous voice path; the web drives
   a non-blocking download via `start-download!` + `model-state`."
  ([] (ensure-model! (model-dir)))
  ([dir]
   (if (model-installed? dir)
     dir
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
      (reset! download-state {:state :downloading :phase :downloading :progress 0})
      (future (try (install-model! (model-dir)
                                   (fn [m]
                                     (swap! download-state #(when (= :downloading (:state %))
                                                              (merge % m)))))
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
  (let [transducer
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
  (let [f
        (io/file audio-path)

        len
        (.length f)

        fail!
        (fn [reason data]
          (throw (ex-info (str "Voice audio is not a readable WAV file - " reason)
                          (merge {:type :voice-asr/invalid-wav
                                  :path (str audio-path)
                                  :reason reason
                                  :length len}
                                 data))))]

    (when (< len 44) (fail! "shorter than a WAV header" {}))
    (with-open [in (java.io.DataInputStream. (java.io.BufferedInputStream. (io/input-stream f)))]
      (let [head (byte-array 12)]
        (.readFully in head)
        (when-not (and (= "RIFF" (String. head 0 4 "US-ASCII"))
                       (= "WAVE" (String. head 8 4 "US-ASCII")))
          (fail! "missing RIFF/WAVE magic" {})))
      (loop [pos 12
             pcm16? false
             data? false]

        (if (>= pos len)
          (do (when-not pcm16? (fail! "no 16-bit PCM fmt chunk" {}))
              (when-not data? (fail! "no data chunk" {})))
          (do (when (> (+ pos 8) len) (fail! "dangling bytes after the last chunk" {:at pos}))
              (let [hdr (byte-array 8)
                    _ (.readFully in hdr)
                    id (String. hdr 0 4 "US-ASCII")
                    size (u32le hdr 4)
                    end (+ pos 8 size)]

                (when (> end len)
                  (fail! "chunk declares more bytes than the file holds (truncated?)"
                         {:chunk id :declared-size size :at pos}))
                (let [fmt-read? (and (= id "fmt ") (>= size 16))
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
  (let [samples
        (alength ^floats (.getSamples reader))

        sample-rate
        (.getSampleRate reader)

        duration
        (if (pos? sample-rate) (/ samples (double sample-rate)) 0.0)]

    {:samples samples :sample-rate sample-rate :duration-seconds duration}))

(defn- assert-audio-long-enough!
  [audio-path ^WaveReader reader]
  (let [{:keys [duration-seconds] :as stats} (audio-stats reader)]
    (when (< (double duration-seconds) (double min-audio-seconds))
      (throw (ex-info "Voice recording too short - try again"
                      (assoc stats
                        :type :voice-asr/audio-too-short
                        :path (str audio-path)
                        :min-duration-seconds min-audio-seconds))))))

(def ^:const default-chunk-seconds
  "Audio longer than this is decoded in pieces so PROGRESS is a measurement and
   not an animation: each finished piece is a real fraction of the recording.
   Parakeet is an offline (non-streaming) model, so a single `decode` call is a
   black box of unknown length — the only honest progress it can report is how
   much AUDIO has been consumed."
  20.0)

(defn chunk-plan
  "Pure: the `[start end]` SAMPLE ranges to decode, in order.

   A recording shorter than `chunk-seconds` is ONE range (chunking cannot improve
   a clip that decodes in a moment, and every extra boundary risks cutting a
   word). A trailing piece shorter than [[min-audio-seconds]] is merged into the
   one before it: a sliver of audio decodes blank or trips ONNX shape errors, and
   it would be reported as a whole chunk of progress for nothing."
  [^long total-samples ^long sample-rate ^double chunk-seconds]
  (cond (not (pos? total-samples)) []
        (or (not (pos? sample-rate)) (not (pos? chunk-seconds))) [[0 total-samples]]
        :else (let [size (max 1 (long (* sample-rate chunk-seconds)))]
                (if (<= total-samples size)
                  [[0 total-samples]]
                  (let [starts (range 0 total-samples size)
                        ranges (mapv (fn [s]
                                       [(long s) (long (min total-samples (+ (long s) size)))])
                                     starts)
                        tail (peek ranges)
                        min-samples (long (* (double sample-rate) (double min-audio-seconds)))]

                    (if (and (> (count ranges) 1)
                             (< (- (long (tail 1)) (long (tail 0))) min-samples))
                      (let [prev (nth ranges (- (count ranges) 2))]
                        (conj (subvec ranges 0 (- (count ranges) 2)) [(prev 0) (tail 1)]))
                      ranges))))))

(defn- decode-chunk!
  "One `[start end]` range through its own stream. A stream is single-use, so a
   fresh one per chunk is the API's own contract, not a precaution."
  ;; NO primitive hints on the numbers: five arguments is one past the four a
  ;; primitive-taking fn may have, and the ints are cast at the call below anyway.
  ^String [^OfflineRecognizer r ^floats samples sample-rate start end]
  (let [^OfflineStream stream (.createStream r)]
    (try (.acceptWaveform stream
                          (java.util.Arrays/copyOfRange samples (int start) (int end))
                          (int sample-rate))
         (.decode r stream)
         (str/trim (.getText (.getResult r stream)))
         (finally (try (.release stream) (catch Throwable _))))))

(defn transcribe-file!
  "Transcribe `audio-path` with local Parakeet TDT int8 through the sherpa-onnx
   Java API. Auto-downloads the model on first use. Returns plain text.

   `opts` may carry `:on-progress`, called with `{:phase :progress}` (`:preparing`
   while the model and the native runtime are being made ready, then
   `:transcribing` with 0..100 of the AUDIO consumed) and `:chunk-seconds`. A
   throwing or slow callback can never fail a transcription: it is guarded here."
  ([audio-path] (transcribe-file! (model-dir) audio-path nil))
  ([dir audio-path] (transcribe-file! dir audio-path nil))
  ([dir audio-path {:keys [on-progress chunk-seconds]}]
   (let [report
         (fn [m]
           (when on-progress (try (on-progress m) (catch Throwable _ nil))))

         _
         (report {:phase :preparing :progress 0})

         _
         (sherpa/ensure-native!)

         dir
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
     (let [reader
           (WaveReader. (str audio-file))

           _
           (assert-audio-long-enough! audio-path reader)

           ^floats samples
           (.getSamples reader)

           sample-rate
           (long (.getSampleRate reader))

           total
           (long (alength samples))

           plan
           (chunk-plan total sample-rate (double (or chunk-seconds default-chunk-seconds)))

           _
           (report {:phase :transcribing :progress 0})

           ^OfflineRecognizer r
           (recognizer files)]

       (try (->> plan
                 (map (fn [[start end]]
                        (let [text (decode-chunk! r samples sample-rate start end)]
                          (report {:phase :transcribing
                                   :progress (min 100 (/ (* 100.0 (long end)) (max 1 total)))})
                          text)))
                 (remove str/blank?)
                 (str/join " ")
                 str/trim)
            (finally (try (.release r) (catch Throwable _))))))))
