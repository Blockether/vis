(ns com.blockether.vis.ext.foundation-voice.tts
  "Local speech synthesis through sherpa-onnx.

   Two families, one shape: make the assets present, build one `OfflineTts`,
   generate. They differ only in what a VOICE is.

     :piper       a voice IS a model — one 63 MB VITS network per speaker,
                  phonemized by the SYSTEM's espeak-ng. Vis mirrors no voice
                  and no phoneme tables: each comes from its publisher, and
                  a machine without espeak-ng is told so instead of failing.
     :pocket-tts  a voice is a reference CLIP the model clones, so the catalogue
                  is a WAV per voice: the clips the bundle ships and the ones
                  somebody imported through `voices.clj`. Opt-in for its size,
                  not its licence - Vis exports those weights itself.

   Which assets exist, where they come from and what they are licensed under is
   `assets.clj`'s question; this namespace only asks for them by id."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.ext.foundation-voice.assets :as assets]
            [com.blockether.vis.ext.foundation-voice.sherpa :as sherpa]
            [com.blockether.vis.ext.foundation-voice.voices :as voices]
            [com.blockether.vis.internal.paths :as paths])
  (:import [com.k2fsa.sherpa.onnx GeneratedAudio GenerationConfig OfflineTts OfflineTtsCallback
            OfflineTtsConfig OfflineTtsModelConfig OfflineTtsPocketModelConfig
            OfflineTtsVitsModelConfig WaveReader]
           [java.io File]
           [java.util LinkedHashMap]
           [java.util.concurrent.atomic AtomicLong]))

;; Reflective interop is FATAL in the native image (needs metadata per call
;; site) — keep this ns reflection-free at compile time.
(set! *warn-on-reflection* true)

(def ^:const pocket-asset-id "pocket-tts-int8")

;; espeak-ng, which is the SYSTEM's to install and not Vis' to ship

;; Piper phonemizes through espeak-ng, whose tables Vis does not ship:
;; GPL-3.0-or-later, one package away, read from their own directory at run time
;; and shared by everything else on the machine. Missing tables are a REFUSAL that
;; names the install command for this platform, never a silent failure or a
;; download.
(def ^:const espeak-data-env "VIS_ESPEAK_NG_DATA")

(def espeak-data-files
  "What makes a directory espeak-ng's data directory rather than any directory."
  ["phontab" "phondata" "phonindex"])

(def espeak-data-candidates
  "Where each package manager puts the tables, in the order they are tried."
  ["/opt/homebrew/share/espeak-ng-data"       ; Homebrew, Apple silicon
   "/usr/local/share/espeak-ng-data"          ; Homebrew on Intel, /usr/local
   "/opt/local/share/espeak-ng-data"          ; MacPorts
   "/usr/share/espeak-ng-data"                ; Debian, Ubuntu, Fedora, Arch
   "/usr/lib/x86_64-linux-gnu/espeak-ng-data" ; Debian multiarch
   "/usr/lib/aarch64-linux-gnu/espeak-ng-data" "/usr/local/lib/espeak-ng-data"])          ; built from source

(defn espeak-data-dir?
  "True when `dir` holds espeak-ng's tables and not merely the right name."
  [dir]
  (boolean (and (not (str/blank? (str dir)))
                (every? #(.isFile (io/file (str dir) ^String %)) espeak-data-files))))

(defn espeak-data-dir
  "The system's espeak-ng phoneme tables, or nil. `VIS_ESPEAK_NG_DATA` wins so a
   machine with them somewhere unusual - or a test - can say where."
  []
  (or (let [named (assets/env-value espeak-data-env)]
        (when (espeak-data-dir? named) named))
      (first (filter espeak-data-dir? espeak-data-candidates))))

(defn espeak-install-hint
  "How to get the tables on THIS platform, in the words that machine uses."
  []
  (let [os (str/lower-case (str (System/getProperty "os.name")))]
    (cond
      (str/includes? os "mac") "Install espeak-ng: `brew install espeak-ng`."
      (str/includes? os "win")
      "Install espeak-ng from https://github.com/espeak-ng/espeak-ng/releases and set VIS_ESPEAK_NG_DATA to its espeak-ng-data directory."
      :else
      "Install espeak-ng: `apt install espeak-ng`, `dnf install espeak-ng` or `pacman -S espeak-ng`.")))

(defn espeak-missing-message
  []
  (str "espeak-ng's phoneme tables are not on this system, and Piper cannot "
       "speak without them. "
       (espeak-install-hint)))

(defn- espeak-data-dir!
  []
  (or (espeak-data-dir)
      (throw (ex-info (espeak-missing-message)
                      {:type :voice-tts/espeak-ng-missing
                       :family :piper
                       :env espeak-data-env
                       :searched espeak-data-candidates
                       :remediation (espeak-install-hint)}))))

;; Catalogue

(defn piper-assets
  "Every Piper voice in the manifest, in manifest order. The FIRST is the default
   voice: the one a caller who names none gets, and the only one downloaded
   before somebody asks for another."
  []
  (assets/for-engine :piper))

(defn pocket-asset [] (assets/entry pocket-asset-id))

(defn piper-voices [] (mapv :voice (piper-assets)))

(defn pocket-voice-catalogue
  "The pocket catalogue with each clip resolved to an absolute path: the clips
   the bundle ships first, then the imported ones. An import with the same id
   WINS - somebody who names their own recording after a shipped voice meant
   their own recording."
  []
  (let
    [entry
     (pocket-asset)

     dir
     (assets/install-dir entry)

     bundled
     (mapv #(assoc % :clip (str (io/file dir (str (:clip %))))) (:voices entry))

     mine
     (voices/imported)

     shadowed
     (set (map :id mine))]

    (into (filterv #(not (shadowed (:id %))) bundled) mine)))

(defn pocket-voices
  "The voices pocket-tts can speak in - the clips the bundle carries and the
   ones somebody imported - without the path: which file backs a voice is
   nobody else's business."
  []
  (mapv #(dissoc % :clip) (pocket-voice-catalogue)))

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

(defn- no-reference-clips!
  "A pocket voice IS a reference clip, so an empty catalogue is not an unknown
   voice - it is a bundle with no voice in it and nothing imported either."
  [entry]
  (throw (ex-info (str "The " (:id entry)
                       " bundle carries no reference clip to speak with."
                       " Import one: vis-agent extension voice import <clip.wav> --name <name>")
                  {:type :voice-tts/no-reference-clips
                   :family :pocket-tts
                   :asset-id (:id entry)
                   :notice (:notice entry)})))
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

;; Readiness - one entry per ASSET, so two engines sharing espeak share its state

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
    [(piper-asset-for voice-id)]

    :pocket-tts
    [(pocket-asset)]))

(defn model-state
  ([family] (model-state family nil))
  ([family voice-id]
   ;; A voice on disk with no phoneme tables under it is not ready, and saying
   ;; `:ready` there would only move the failure to the first spoken word.
   (if (and (= :piper family) (not (espeak-data-dir)))
     {:state :failed :error (espeak-missing-message)}
     (combined-state (mapv asset-state (required-assets family voice-id))))))

(defn start-download!
  "Begin (or report) the download every surface polls. An OPT-IN model is never
   started here, and neither is a Piper voice on a machine without espeak-ng:
   the refusal names the command that fixes it, because a silent `:absent`
   forever is the one answer a user cannot act on."
  ([family] (start-download! family nil))
  ([family voice-id]
   (if (and (= :piper family) (not (espeak-data-dir)))
     {:state :failed :error (espeak-missing-message)}
     (let [needed (required-assets family voice-id)]
       (if-let [opt-in (first (filter #(and (:is-opt-in %) (not (assets/installed? %))) needed))]
         {:state :failed
          :error (str (:id opt-in)
                      " is not downloaded automatically. Install it with "
                      "`vis-agent extension voice models download --"
                      (name family)
                      (when-let [voice (:voice opt-in)]
                        (str " --voice " (name (:id voice))))
                      "`.")}
         (combined-state (mapv start-asset-download! needed)))))))

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
;; Synthesis

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

;; ONE loaded model at a time: loading costs seconds and hundreds of megabytes, so
;; the same voice must not pay it twice and a switch of voice must not retain the
;; old model. sherpa frees the dropped instance in its own finalizer.
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

(deftype
  ;; NAMED on purpose: sherpa calls this back FROM C++ with GetMethodID "invoke"
  ;; "([F)Ljava/lang/Integer;", and a `reify`'s class name is invented anew by every
  ;; build, so no metadata can register it. `sherpa-test` fails if
  ;; reachability-metadata.json stops carrying this class.
  GenerationCallback
  [^AtomicLong produced ^long sample-rate ^double expected-seconds report]
  OfflineTtsCallback
    (invoke [_ samples]
      (let [done (.addAndGet produced (alength ^floats samples))]
        (report {:phase :synthesizing
                 :progress (min 99
                                (long (* 100.0
                                         (/ (/ (double done) (double (max 1 sample-rate)))
                                            (max 0.1 expected-seconds)))))}))
      (Integer/valueOf 1)))

(defn- generate!
  [^OfflineTts tts ^GenerationConfig gen ^String text report]
  (let
    [sample-rate
     (long (.getSampleRate tts))

     produced
     (AtomicLong. 0)

     ^OfflineTtsCallback callback
     (GenerationCallback. produced
                          sample-rate
                          (/ (double (count text)) (double chars-per-second))
                          report)

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
  ^GenerationConfig [clip-path clip-text]
  (let
    [reader
     (WaveReader. ^String clip-path)

     gen
     (GenerationConfig.)]

    (.setReferenceAudio gen (.getSamples reader))
    (.setReferenceSampleRate gen (.getSampleRate reader))
    ;; pocket-tts is given the reference audio AND its transcript: a clone that is not
    ;; guessing at the words tracks the voice far more closely.
    (.setReferenceText gen (str clip-text))
    (.setNumSteps gen (int 5))
    ;; sherpa walks this map FROM C++ by whatever CONCRETE class it finds
    ;; (`entrySet`, `iterator`, `next`, `getKey`). A Clojure map changes class with
    ;; its size, so no metadata can register that walk and the native image died in
    ;; the JNI trampoline. One JDK class is the whole fix.
    (.setExtra gen (doto (LinkedHashMap.) (.put "temperature" "0.7") (.put "chunk_size" "15")))
    gen))

(defn- ensure-assets!
  [family voice-id report]
  ;; Refuse BEFORE the download: a 63 MB voice that cannot be phonemized is a
  ;; wasted download and a worse error message.
  (when (= :piper family) (espeak-data-dir!))
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
           (espeak-data-dir!)

           model-file
           (first (filter #(str/ends-with? % ".onnx") (:requires (last installed))))

           config
           (piper-config dir espeak-dir model-file)

           tts
           (loaded-tts [:piper dir] (constantly config))]

          (generate! tts (piper-generation-config config) spoken report))

        :pocket-tts
        (let
          [catalogue
           (pocket-voice-catalogue)

           _
           (when (empty? catalogue) (no-reference-clips! (pocket-asset)))

           voice
           (or (named-voice catalogue voice-id) (unknown-voice! :pocket-tts voice-id catalogue))

           clip
           (io/file (str (:clip voice)))]

          (when-not (.isFile clip)
            (throw (ex-info (str "Reference clip is missing: " (:clip voice))
                            {:type :voice-tts/missing-voice-clip
                             :family family
                             :voice-id (name (:id voice))
                             :path (str clip)})))
          (generate! (loaded-tts [:pocket-tts dir] #(pocket-config dir))
                     (pocket-generation-config (str clip) (:clip-text voice))
                     spoken
                     report))))))
