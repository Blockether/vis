(ns com.blockether.vis.ext.foundation-voice.tts
  "Local speech synthesis through sherpa-onnx.

   Two families, one shape: make the assets present, build one `OfflineTts`,
   generate. They differ only in what a VOICE is.

     :piper       a voice IS a model — one 63 MB VITS network per speaker, all of
                  them sharing one installed copy of espeak-ng's phoneme tables.
                  Public-domain voices, so this is the family Vis ships.
     :pocket-tts  a voice is a reference CLIP the model clones, so the catalogue
                  is a directory of WAVs. Opt-in until Vis exports the original
                  CC BY 4.0 weights itself — see the manifest entry for why.

   Which assets exist, where they come from and what they are licensed under is
   `assets.clj`'s question; this namespace only asks for them by id."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.ext.foundation-voice.assets :as assets]
            [com.blockether.vis.ext.foundation-voice.sherpa :as sherpa]
            [com.blockether.vis.internal.paths :as paths])
  (:import [com.k2fsa.sherpa.onnx GeneratedAudio GenerationConfig OfflineTts OfflineTtsCallback
            OfflineTtsConfig OfflineTtsModelConfig OfflineTtsPocketModelConfig
            OfflineTtsVitsModelConfig WaveReader]
           [java.io File]
           [java.util.concurrent.atomic AtomicLong]))

;; Reflective interop is FATAL in the native image (needs metadata per call
;; site) — keep this ns reflection-free at compile time.
(set! *warn-on-reflection* true)

(def ^:const espeak-asset-id "espeak-ng-data")
(def ^:const pocket-asset-id "pocket-tts-int8")

;; =============================================================================
;; Catalogue
;; =============================================================================

(defn piper-assets
  "Every Piper voice in the manifest, in manifest order. The FIRST is the default
   voice: the one a caller who names none gets, and the only one downloaded
   before somebody asks for another."
  []
  (assets/for-engine :piper))

(defn pocket-asset [] (assets/entry pocket-asset-id))

(defn piper-voices [] (mapv :voice (piper-assets)))

(defn pocket-voices
  "The reference clips the bundle carries, without the path: which file backs a
   voice is nobody else's business."
  []
  (mapv #(dissoc % :clip) (:voices (pocket-asset))))

(defn- named-voice
  [voices voice-id]
  (let
    [id (some-> voice-id
                name
                not-empty)]
    (cond (nil? id) (first voices)
          :else (first (filter #(= id (name (:id %))) voices)))))

(defn- unknown-voice!
  [family voice-id voices]
  (throw (ex-info (str "No " (name family) " voice named " (pr-str voice-id))
                  {:type :voice-tts/unknown-voice
                   :family family
                   :voice-id voice-id
                   :known (mapv #(name (:id %)) voices)})))

(defn- piper-asset-for
  "The manifest entry whose voice is `voice-id`, or the default when none is
   named."
  [voice-id]
  (let
    [entries
     (piper-assets)

     wanted
     (named-voice (mapv :voice entries) voice-id)]

    (or (first (filter #(= (:voice %) wanted) entries))
        (unknown-voice! :piper voice-id (mapv :voice entries)))))

;; =============================================================================
;; Readiness - one entry per ASSET, so two engines sharing espeak share its state
;; =============================================================================

;; `:ready` is DERIVED from what is on disk, never stored, so it can't go stale.
;; The atom only tracks an in-flight or failed download.
(defonce ^:private downloads* (atom {}))

(defn- asset-state
  [asset]
  (let [tracked (get @downloads* (:id asset))]
    (cond (assets/installed? asset) {:state :ready}
          (#{:downloading :failed} (:state tracked)) tracked
          :else {:state :absent})))

(defn- combined-state
  "One readiness for a GROUP of assets: a voice whose phoneme tables are still
   arriving cannot speak, so the group is as ready as its worst member and its
   progress is the group's average — a two-part install must not jump back to 0%
   when the first part finishes."
  [states]
  (cond (some #(= :failed (:state %)) states) (first (filter #(= :failed (:state %)) states))
        (every? #(= :ready (:state %)) states) {:state :ready}
        (some #(= :downloading (:state %)) states)
        {:state :downloading
         :phase (:phase (first (filter #(= :downloading (:state %)) states)))
         :progress
         (long (/ (long (reduce +
                                (map #(if (= :ready (:state %)) 100 (long (or (:progress %) 0)))
                                     states)))
                  (long (max 1 (count states)))))}
        :else {:state :absent}))

(defn- start-asset-download!
  "Idempotent, NON-blocking: if `asset` is absent and no download is already
   running, start one on a background thread."
  [asset]
  (locking downloads*
    (when (and (not (assets/installed? asset))
               (not= :downloading (:state (get @downloads* (:id asset)))))
      (swap! downloads* assoc (:id asset) {:state :downloading :phase :downloading :progress 0})
      (future (try (assets/install! asset
                                    (fn [m]
                                      (swap! downloads* update
                                        (:id asset)
                                        #(when (= :downloading (:state %)) (merge % m)))))
                   (swap! downloads* dissoc (:id asset)) ; state now derives :ready
                   (catch Throwable t
                     (swap! downloads* assoc
                       (:id asset)
                       {:state :failed :error (or (ex-message t) "download failed")}))))))
  (asset-state asset))

(defn- required-assets
  "What has to be installed before `family` can speak in `voice-id`."
  [family voice-id]
  (case family
    :piper
    [(assets/entry espeak-asset-id) (piper-asset-for voice-id)]

    :pocket-tts
    [(pocket-asset)]))

(defn model-state
  ([family] (model-state family nil))
  ([family voice-id] (combined-state (mapv asset-state (required-assets family voice-id)))))

(defn start-download!
  "Begin (or report) the download every surface polls. An OPT-IN model is never
   started here: the refusal names the command that installs it, because a
   silent `:absent` forever is the one answer a user cannot act on."
  ([family] (start-download! family nil))
  ([family voice-id]
   (let [needed (required-assets family voice-id)]
     (if-let [opt-in (first (filter #(and (:is-opt-in %) (not (assets/installed? %))) needed))]
       {:state :failed
        :error (str (:id opt-in)
                    " is not downloaded automatically. Install it with "
                    "`vis-agent extension voice models download --"
                    (name family)
                    "`.")}
       (combined-state (mapv start-asset-download! needed))))))

(defn install-model!
  "Blocking install of everything `family` needs before it can speak, for the
   CLI. Unlike [[start-download!]] it accepts an OPT-IN model: a user naming one
   IS the explicit ask [[assets/ensure!]] refuses to make on their behalf.
   Returns the install dir of every asset actually fetched, in order."
  ([family] (install-model! family nil nil))
  ([family voice-id on-progress]
   (sherpa/ensure-native!)
   (mapv #(assets/install! % on-progress)
         (remove assets/installed? (required-assets family voice-id)))))
;; =============================================================================
;; Synthesis
;; =============================================================================

(defn- num-threads ^long [] (max 1 (long (.availableProcessors (Runtime/getRuntime)))))

(defn- model-path
  "`/`-separated on every OS, so a model address is identical across platforms."
  ^String [dir name]
  (paths/unixify (io/file dir name)))

(defn- piper-config
  ^OfflineTtsConfig [dir espeak-dir model-file]
  (let
    [vits
     (.. (OfflineTtsVitsModelConfig/builder)
         (setModel (model-path dir model-file))
         (setTokens (model-path dir "tokens.txt"))
         ;; Piper phonemizes through espeak-ng, whose tables are installed ONCE
         ;; and shared: a voice directory holds only the network.
         (setDataDir (paths/unixify (io/file espeak-dir)))
         build)

     model
     (.. (OfflineTtsModelConfig/builder)
         (setVits vits)
         (setNumThreads (num-threads))
         (setDebug false)
         build)]

    (.. (OfflineTtsConfig/builder) (setModel model) build)))

(defn- pocket-config
  ^OfflineTtsConfig [dir]
  (let
    [pocket
     (.. (OfflineTtsPocketModelConfig/builder)
         (setLmFlow (model-path dir "lm_flow.int8.onnx"))
         (setLmMain (model-path dir "lm_main.int8.onnx"))
         (setEncoder (model-path dir "encoder.onnx"))
         (setDecoder (model-path dir "decoder.int8.onnx"))
         (setTextConditioner (model-path dir "text_conditioner.onnx"))
         (setVocabJson (model-path dir "vocab.json"))
         (setTokenScoresJson (model-path dir "token_scores.json"))
         build)

     model
     (.. (OfflineTtsModelConfig/builder)
         (setPocket pocket)
         (setNumThreads (num-threads))
         (setDebug false)
         build)]

    (.. (OfflineTtsConfig/builder) (setModel model) build)))

;; ONE loaded model at a time. Loading is seconds and hundreds of megabytes, so
;; speaking the next line in the same voice must not pay it again; and a session
;; that switches voice must not hold every model it ever used. The instance we
;; drop is released by sherpa's own finalizer once nothing refers to it.
(defonce ^:private loaded* (atom nil))

(defn- loaded-tts
  ^OfflineTts [cache-key build-config]
  (locking loaded*
    (let [current @loaded*]
      (if (= cache-key (:key current))
        (:tts current)
        (let [tts (OfflineTts. ^OfflineTtsConfig (build-config))]
          (reset! loaded* {:key cache-key :tts tts})
          tts)))))

(def ^:const chars-per-second
  "Speech is roughly this many characters a second, and that is the only estimate
   available before generation starts: an offline TTS model reports the samples
   it has produced, never the ones it still owes. Progress is therefore honest
   about being an estimate — it never goes backwards and never reaches 100 until
   the audio is written."
  15.0)

(defn- generation-callback
  ^OfflineTtsCallback [^AtomicLong produced ^long sample-rate ^double expected-seconds report]
  (reify
    OfflineTtsCallback
      (invoke [_ samples]
        (let [done (.addAndGet produced (alength ^floats samples))]
          (report {:phase :synthesizing
                   :progress (min 99
                                  (long (* 100.0
                                           (/ (/ (double done) (double (max 1 sample-rate)))
                                              (max 0.1 expected-seconds)))))}))
        (Integer/valueOf 1))))

(defn- generate!
  [^OfflineTts tts ^GenerationConfig gen ^String text report]
  (let
    [sample-rate
     (long (.getSampleRate tts))

     produced
     (AtomicLong. 0)

     callback
     (generation-callback produced sample-rate (/ (count text) chars-per-second) report)

     ^GeneratedAudio audio
     (.generateWithConfigAndCallback tts text gen callback)

     out
     (File/createTempFile "vis-speech-" ".wav")

     samples
     (alength ^floats (.getSamples audio))]

    (when-not (.save audio (str out))
      (.delete out)
      (throw (ex-info "sherpa-onnx could not write the generated audio"
                      {:type :voice-tts/save-failed :path (str out)})))
    {:audio-path (str out)
     :media-type "audio/wav"
     :sample-rate (.getSampleRate audio)
     :duration-ms (long (* 1000.0 (/ samples (double (max 1 (.getSampleRate audio))))))}))

(defn- piper-generation-config
  ^GenerationConfig [^OfflineTtsConfig config]
  (let [gen (GenerationConfig.)]
    ;; Every voice Vis ships is single-speaker, so the speaker id is 0 and the
    ;; catalogue is one MODEL per voice rather than one id into a table.
    (.setSid gen (int 0))
    (.setSpeed gen (float 1.0))
    (.setSilenceScale gen (.getSilenceScale config))
    gen))

(defn- pocket-generation-config
  ^GenerationConfig [clip-path]
  (let
    [reader
     (WaveReader. ^String clip-path)

     gen
     (GenerationConfig.)]

    (.setReferenceAudio gen (.getSamples reader))
    (.setReferenceSampleRate gen (.getSampleRate reader))
    (.setNumSteps gen (int 5))
    (.setExtra gen {"temperature" "0.7" "chunk_size" "15"})
    gen))

(defn- ensure-assets!
  [family voice-id report]
  (report {:phase :preparing :progress 0})
  (sherpa/ensure-native!)
  (mapv #(do (assets/ensure! %
                             (fn [m]
                               (report (assoc m :phase :preparing))))
             %)
        (required-assets family voice-id)))

(defn synthesize!
  "Speak `text` and return the WAV that was written, plus the facts a player
   needs. `family` is `:piper` or `:pocket-tts`; `voice-id` names a voice from
   that family's catalogue and defaults to the first one.

   The assets are installed if they are missing, reported as `:preparing`,
   because the download is a real part of \"how long until I hear something\"."
  [family {:keys [text voice-id on-progress]}]
  (let
    [report
     (fn [m]
       (when on-progress (try (on-progress m) (catch Throwable _ nil))))

     spoken
     (str/trim (str text))]

    (when (str/blank? spoken)
      (throw (ex-info "Nothing to speak" {:type :voice-tts/blank-text :family family})))
    (let
      [installed
       (ensure-assets! family voice-id report)

       dir
       (assets/install-dir (last installed))]

      (report {:phase :synthesizing :progress 0})
      (case family
        :piper
        (let
          [espeak-dir
           (assets/install-dir (first installed))

           model-file
           (first (filter #(str/ends-with? % ".onnx") (:requires (last installed))))

           config
           (piper-config dir espeak-dir model-file)

           tts
           (loaded-tts [:piper dir] (constantly config))]

          (generate! tts (piper-generation-config config) spoken report))

        :pocket-tts
        (let
          [voice
           (or (named-voice (:voices (pocket-asset)) voice-id)
               (unknown-voice! :pocket-tts voice-id (:voices (pocket-asset))))

           clip
           (io/file dir (:clip voice))]

          (when-not (.isFile clip)
            (throw (ex-info (str "Reference clip is missing: " (:clip voice))
                            {:type :voice-tts/missing-voice-clip
                             :family family
                             :voice-id (name (:id voice))
                             :path (str clip)})))
          (generate! (loaded-tts [:pocket-tts dir] #(pocket-config dir))
                     (pocket-generation-config (str clip))
                     spoken
                     report))))))
