(ns com.blockether.vis.ext.foundation-voice.assets
  "Where a voice asset comes from, what it is licensed under, and how it lands on
   disk.

   `resources/vis-models/manifest.edn` is the only answer to \"may we ship
   this\": every model and data directory Vis installs has an entry there with an
   SPDX id, an attribution line and `:is-commercial-ok true`. An artifact that is
   not in the manifest cannot be downloaded, because nothing else carries a URL.

   Each entry lists its sources in preference order and every source delivers the
   SAME bytes, so one `:sha256` verifies them all:

     :hf        Hugging Face. SKIPPED unless a token is configured, tried FIRST
                when one is. A token is never required — it only changes WHERE
                the same bytes come from.
     :pack      the Vis VOICE_ASSETS_PACK release, the default, so a first run
                needs no account anywhere.
     :upstream  the project that published the artifact, last, as a fallback.

   Sources are tried in that order and the first that installs cleanly wins, so
   an asset host being down is a slower install rather than a dead feature."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.foundation-voice.files :as files])
  (:import [java.io File]))

;; Reflective interop is FATAL in the native image (needs metadata per call
;; site) — keep this ns reflection-free at compile time.
(set! *warn-on-reflection* true)

(def ^:const manifest-resource "vis-models/manifest.edn")

(def models-dir-env "VIS_VOICE_MODELS_DIR")

(def hf-token-env-vars
  "Both names the Hugging Face tooling itself reads, so a machine already set up
   for `huggingface-cli` needs no extra configuration."
  ["HF_TOKEN" "HUGGING_FACE_HUB_TOKEN"])

(defonce ^:private manifest*
  (delay (let [resource (io/resource manifest-resource)]
           (when-not resource
             (throw (ex-info "Voice asset manifest is missing from the classpath"
                             {:type :voice-assets/manifest-missing :resource manifest-resource})))
           (vec (edn/read-string (slurp resource))))))

(defn manifest "Every asset entry, in manifest order." [] @manifest*)

(defn entry
  "The manifest entry with `id`. Throws rather than returning nil: an unknown id
   is a bug in the caller, not a missing download."
  [id]
  (or (first (filter #(= id (:id %)) (manifest)))
      (throw (ex-info (str "No voice asset named " id " in the manifest")
                      {:type :voice-assets/unknown-asset :id id :known (mapv :id (manifest))}))))

(defn env-value
  "An environment value as Vis sees it: the extension environment first, then
   the process environment, blank treated as absent."
  [name]
  (or (some-> (vis/extension-env-value name)
              str
              str/trim
              not-empty)
      (some-> (System/getenv name)
              str
              str/trim
              not-empty)))

(defn hf-token
  "The configured Hugging Face token, or nil. OPTIONAL by design: without one
   every entry still resolves, from the Vis asset pack. Returned only to the
   downloader that needs it — never logged, never put in an ex-info."
  []
  (some env-value hf-token-env-vars))

(defn models-root
  "Where installed assets live. A function, never a top-level `def`:
   `native-image` initializes this namespace at BUILD time, so a captured
   `user.home` would point every installed binary at the BUILDER's home."
  []
  (or (env-value models-dir-env) (str (System/getProperty "user.home") "/.vis/models")))

(defn install-dir
  "Absolute directory this entry installs into."
  [entry]
  (str (io/file (models-root) (:install-dir entry))))

(defn sources
  "The sources of `entry` worth trying, in order. Sources that need a token are
   dropped when there is none and moved to the FRONT when there is one, which is
   the whole of the policy: a token changes where the bytes come from and never
   whether they can be had."
  ([entry] (sources entry (some? (hf-token))))
  ([entry is-token-available]
   (let [gated
         (filter :is-token-required (:sources entry))

         open
         (remove :is-token-required (:sources entry))]

     (vec (if is-token-available (concat gated open) open)))))

(defn missing-files
  "The `:requires` paths of `entry` that are not present under `dir`."
  ([entry] (missing-files entry (install-dir entry)))
  ([entry dir] (vec (remove #(.isFile (io/file dir %)) (:requires entry)))))

(defn installed?
  ([entry] (installed? entry (install-dir entry)))
  ([entry dir] (empty? (missing-files entry dir))))

(defn- source-headers
  [source]
  (when (:is-token-required source)
    (when-let [token (hf-token)]
      {"authorization" (str "Bearer " token)})))

(defn- fetch-archive!
  [source staging report]
  (let [archive (File/createTempFile "vis-voice-asset-" ".tar.bz2")]
    (try (files/download! (:url source)
                          (str archive)
                          {:sha256 (:sha256 source)
                           :headers (source-headers source)
                           :on-progress (fn [pct]
                                          (report :downloading (long (* 0.9 (long pct)))))})
         (files/extract-tar-bz2! (str archive)
                                 (str staging)
                                 (fn [pct]
                                   (report :extracting (+ 90 (long (* 0.09 (long pct)))))))
         (finally (try (.delete archive) (catch Throwable _))))))

(defn- fetch-files!
  "A source that serves the files loose instead of as an archive. Each file is
   verified on the way in, so there is no unpack step and progress is shared out
   by declared size."
  [source staging report]
  (let [files
        (:files source)

        total
        (max 1 (long (reduce + 0 (map #(long (or (:bytes %) 0)) files))))]

    (loop [remaining
           files

           done
           0]

      (when-let [f (first remaining)]
        (files/download! (:url f)
                         (str (io/file staging (:path f)))
                         {:sha256 (:sha256 f)
                          :headers (source-headers source)
                          :on-progress (fn [pct]
                                         (report :downloading
                                                 (long (* 0.99
                                                          (/ (* 100.0
                                                                (+ done
                                                                   (* (long (or (:bytes f) 0))
                                                                      (/ (long pct) 100.0))))
                                                             total)))))})
        (recur (rest remaining) (+ done (long (or (:bytes f) 0))))))))

(defn- install-from-source!
  [entry source staging report]
  (files/delete-dir! (io/file staging))
  (case (:kind source)
    :archive
    (fetch-archive! source staging report)

    :files
    (fetch-files! source staging report)

    (throw (ex-info
             (str "Unknown asset source kind: " (pr-str (:kind source)))
             {:type :voice-assets/unknown-source-kind :id (:id entry) :kind (:kind source)})))
  (let [missing (missing-files entry (str staging))]
    (when (seq missing)
      (throw (ex-info (str "Download of " (:id entry) " did not produce " (str/join ", " missing))
                      {:type :voice-assets/install-incomplete
                       :id (:id entry)
                       :host (:host source)
                       :missing missing})))))

(defn install!
  "Download + verify + ATOMICALLY install `entry`, trying each source in turn.
   The final directory never holds partial files: an interrupted or corrupt
   download can't leave a truncated `.onnx` that native-aborts the JVM on the
   next load, it just stays absent. Returns the install dir.

   `on-progress` (optional) is called with {:phase :downloading|:extracting
   :progress 0..99}. Transfer owns 0..89 and unpacking owns 90..98, so the number
   keeps MOVING through a multi-minute bzip2 extraction instead of parking on 99%
   and looking hung."
  ([entry] (install! entry (install-dir entry) nil))
  ([entry on-progress] (install! entry (install-dir entry) on-progress))
  ([entry dir on-progress]
   (let [staging
         (io/file (str dir ".staging-" (System/nanoTime)))

         report
         (fn [phase pct]
           (when on-progress (on-progress {:phase phase :progress pct})))

         candidates
         (sources entry)]

     (when (empty? candidates)
       (throw (ex-info (str "No source for " (:id entry) " is usable")
                       {:type :voice-assets/no-source :id (:id entry)})))
     (try
       (loop [remaining
              candidates

              failures
              []]

         (if-let [source (first remaining)]
           (let [failure (try (install-from-source! entry source staging report)
                              nil
                              (catch Throwable t
                                {:host (:host source) :error (or (ex-message t) (str t))}))]
             (if failure
               (recur (rest remaining) (conj failures failure))
               (let [final (io/file dir)]
                 (report :extracting 99)
                 (when (.exists final) (files/delete-dir! final))
                 (.mkdirs (.getParentFile final))
                 (when-not (.renameTo staging final)
                   (throw (ex-info "Could not move the downloaded asset into place"
                                   {:type :voice-assets/install-failed :id (:id entry) :dir dir})))
                 dir)))
           (throw (ex-info (str "Could not install " (:id entry) " from any source")
                           {:type :voice-assets/install-failed
                            :id (:id entry)
                            :tried (mapv :host candidates)
                            :failures failures}))))
       (finally (try (when (.exists staging) (files/delete-dir! staging)) (catch Throwable _)))))))

(defn for-engine
  "Every asset an engine owns, in manifest order."
  [engine]
  (filterv #(= engine (:engine %)) (manifest)))

(defn ensure!
  "Install `asset` if it is not already there (blocking). Returns the install
   dir.

   This is the AUTOMATIC path, so it refuses an `:is-opt-in` asset: one whose
   terms we will not accept on a user's behalf arrives only through an explicit
   `install!`, never because something wanted to speak."
  ([asset] (ensure! asset nil))
  ([asset on-progress]
   (when (and (:is-opt-in asset) (not (installed? asset)))
     (throw (ex-info (str (:id asset) " is not installed and is not fetched automatically")
                     {:type :voice-assets/opt-in-required
                      :id (:id asset)
                      :license (:license asset)
                      :notice (:notice asset)
                      :source-url (:source-url asset)})))
   (if (installed? asset) (install-dir asset) (install! asset (install-dir asset) on-progress))))
