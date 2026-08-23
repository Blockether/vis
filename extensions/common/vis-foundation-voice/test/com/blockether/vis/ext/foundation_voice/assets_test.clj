(ns com.blockether.vis.ext.foundation-voice.assets-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.ext.foundation-voice.assets :as assets]
            [com.blockether.vis.ext.foundation-voice.attribution :as attribution]
            [com.blockether.vis.ext.foundation-voice.files :as files]
            [lazytest.core :refer [defdescribe it expect]])
  (:import [java.io File FileOutputStream]
           [org.apache.commons.compress.archivers.tar TarArchiveEntry TarArchiveOutputStream]
           [org.apache.commons.compress.compressors.bzip2 BZip2CompressorOutputStream]))

(def ^:private sha256-pattern #"[0-9a-f]{64}")

(defn- archive-of!
  "A tar.bz2 shaped like a release asset - one top-level directory, which the
   extractor strips, holding `names` - so the install path runs offline."
  ^File [names]
  (let [archive (File/createTempFile "vis-voice-asset-test-" ".tar.bz2")]
    (with-open [out (FileOutputStream. archive)
                bz (BZip2CompressorOutputStream. out)
                tar (TarArchiveOutputStream. bz)]

      (doseq [file-name names]
        (let [payload (.getBytes (str "payload of " file-name) "UTF-8")
              entry (TarArchiveEntry. (str "release-top-level/" file-name))]

          (.setSize entry (alength payload))
          (.putArchiveEntry tar entry)
          (.write tar payload)
          (.closeArchiveEntry tar))))
    archive))

(defn- temp-dir-path
  ^String [prefix]
  (str (io/file (System/getProperty "java.io.tmpdir") (str prefix (System/nanoTime)))))

(defn- siblings-of
  "Every entry beside `path` whose name starts with its own - a leftover
   `.staging-…` directory shows up here."
  [^String path]
  (let [target
        (io/file path)

        parent
        (.getParentFile target)]

    (->> (or (.listFiles parent) (make-array File 0))
         (map #(.getName ^File %))
         (filter #(str/starts-with? ^String % (.getName target)))
         vec)))

(defn- ex-data-of [f] (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))

(defdescribe
  manifest-test
  (it "says of every asset what it is, who to credit and where it may come from"
      ;; The manifest is the only place a URL exists, so an entry missing its
      ;; licence or its checksum is an artifact Vis would install blind.
      (doseq [entry (assets/manifest)]
        (expect (string? (:id entry)))
        (expect (seq (:install-dir entry)))
        (expect (seq (:requires entry)))
        (expect (seq (:license entry)))
        (expect (seq (:attribution entry)))
        (expect (seq (:source-url entry)))
        (expect (seq (:sources entry)))
        (doseq [source (:sources entry)]
          (expect (keyword? (:host source)))
          (expect (contains? #{:archive :files} (:kind source)))
          (if (= :archive (:kind source))
            (do (expect (str/starts-with? (:url source) "https://"))
                (expect (re-matches sha256-pattern (:sha256 source)))
                (expect (pos? (long (:bytes source)))))
            (do (expect (= (set (:requires entry)) (set (map :path (:files source)))))
                (doseq [file (:files source)]
                  (expect (str/starts-with? (:url file) "https://"))
                  (expect (re-matches sha256-pattern (:sha256 file)))
                  (expect (pos? (long (:bytes file))))))))))
  (it "mirrors only what it is allowed to mirror"
      ;; The pack is Vis' own release: hosting an artifact there is a claim that
      ;; we may host it. Anything short of that is opt-in and says why.
      (doseq [entry (assets/manifest)]
        (let [is-mirrored (boolean (some #(= :pack (:host %)) (:sources entry)))]
          (expect (= is-mirrored (boolean (:is-redistributed entry))))
          (when is-mirrored (expect (true? (:is-commercial-ok entry))))
          (when-not (:is-commercial-ok entry)
            (expect (true? (:is-opt-in entry)))
            (expect (seq (:notice entry))))
          ;; Opt-in is a LICENCE gate and nothing else: size never earns one, or
          ;; `download` would leave a model unfetched and an engine silent.
          (when (:is-opt-in entry)
            (expect (false? (boolean (:is-commercial-ok entry))) (:id entry))))))
  (it "leaves every asset reachable WITHOUT a Hugging Face token"
      ;; A token changes where the bytes come from, never whether they can be
      ;; had: a gated source is dropped when there is none and moves to the
      ;; front when there is one.
      (doseq [entry (assets/manifest)]
        (let [open (assets/sources entry false)
              with-token (assets/sources entry true)]

          (expect (seq open))
          (expect (not-any? :is-token-required open))
          (expect (= (count (:sources entry)) (count with-token)))
          (when (some :is-token-required (:sources entry))
            (expect (true? (:is-token-required (first with-token))))))))
  (it "resolves the ids the engines ask for and refuses one it does not know"
      (expect (= "parakeet-tdt-0.6b-v3-int8" (:id (assets/entry "parakeet-tdt-0.6b-v3-int8"))))
      (expect (= "pocket-tts-int8" (:id (assets/entry "pocket-tts-int8"))))
      (expect (= ["piper-en_US-kristin-medium" "piper-en_GB-cori-high" "piper-en_US-john-medium"
                  "piper-en_US-ljspeech-high" "piper-en_US-ryan-high"]
                 (mapv :id (assets/for-engine :piper))))
      (expect (= ["pocket-tts-int8"] (mapv :id (assets/for-engine :pocket-tts))))
      (let [data (ex-data-of #(assets/entry "no-such-model"))]
        (expect (= :voice-assets/unknown-asset (:type data)))
        (expect (contains? (set (:known data)) "parakeet-tdt-0.6b-v3-int8"))))
  (it "lists Ryan and refuses to host him"
      ;; The voice a user asked for by name: CC BY-NC-SA 4.0, so he is in the
      ;; catalogue with his terms attached and no source of him is ours.
      (let [entry (assets/entry "piper-en_US-ryan-high")]
        (expect (= "ryan" (name (get-in entry [:voice :id]))))
        (expect (true? (get-in entry [:voice :is-opt-in])))
        (expect (= "CC-BY-NC-SA-4.0" (:license entry)))
        (expect (false? (:is-commercial-ok entry)))
        (expect (not-any? #(= :pack (:host %)) (:sources entry)))
        (expect (str/includes? (:notice entry) "non-commercial"))))
  (it "spells the same voice and the same quality level in every part of an entry"
      ;; An entry names <lang>-<speaker>-<level> five times over: its id, its
      ;; install directory, the file it requires, its source URL and every
      ;; download URL. Lifting a voice from medium to high by editing four of
      ;; the five installs the wrong weights under the right name, and the
      ;; sha256 that would catch it is itself one of the things being edited.
      (doseq [entry (assets/for-engine :piper)]
        (let [voice (str/replace (:id entry) #"^piper-" "")
              [lang speaker level] (str/split voice #"-")]

          (expect (= (str "vits-piper-" voice) (:install-dir entry)))
          (expect (= [(str voice ".onnx") "tokens.txt"] (take 2 (:requires entry))))
          (expect (= #{"espeak-ng-data/phontab" "espeak-ng-data/phondata"
                       "espeak-ng-data/phonindex"}
                     (set (drop 2 (:requires entry)))))
          (expect (str/ends-with? (:source-url entry)
                                  (str/join "/" [(subs lang 0 2) lang speaker level])))
          (doseq [source (:sources entry)
                  url (if (= :files (:kind source)) (mapv :url (:files source)) [(:url source)])]

            (expect (str/includes? url voice)))))))

(defdescribe install-test
             (it "installs from a source and lands the files the entry requires"
                 (let [archive
                       (archive-of! ["model.onnx" "tokens.txt"])

                       dir
                       (temp-dir-path "vis-voice-install-")

                       entry
                       {:id "test-asset"
                        :requires ["model.onnx" "tokens.txt"]
                        :sources [{:host :test :kind :archive :url (str (.toURI archive))}]}

                       seen
                       (atom [])]

                   (try (expect (= dir (assets/install! entry dir #(swap! seen conj %))))
                        (expect (true? (assets/installed? entry dir)))
                        (expect (seq (filter #(= :downloading (:phase %)) @seen)))
                        (expect (seq (filter #(= :extracting (:phase %)) @seen)))
                        (expect (every? #(<= 0 (long (:progress %)) 99) @seen))
                        (finally (.delete archive) (files/delete-dir! (io/file dir))))))
             (it "leaves NOTHING behind when every source fails"
                 ;; A half-written model directory is worse than an absent one: the file is
                 ;; present, so a caller that only asks `.isFile` loads a truncated .onnx
                 ;; and the native runtime aborts the JVM.
                 (let [dir
                       (temp-dir-path "vis-voice-failed-install-")

                       gone
                       (str (.toURI (io/file (temp-dir-path "vis-voice-absent-archive-"))))

                       entry
                       {:id "test-asset"
                        :requires ["model.onnx"]
                        :sources [{:host :first :kind :archive :url gone}
                                  {:host :second :kind :archive :url gone}]}

                       data
                       (ex-data-of #(assets/install! entry dir nil))]

                   (expect (= :voice-assets/install-failed (:type data)))
                   (expect (= [:first :second] (:tried data)))
                   (expect (= [:first :second] (mapv :host (:failures data))))
                   (expect (false? (.exists (io/file dir))))
                   (expect (= [] (siblings-of dir)))))
             (it "refuses a source kind it does not understand instead of installing part of one"
                 (let [dir
                       (temp-dir-path "vis-voice-bad-kind-")

                       entry
                       {:id "test-asset"
                        :requires ["model.onnx"]
                        :sources
                        [{:host :first :kind :carrier-pigeon :url "https://example.com/x.tar.bz2"}]}

                       data
                       (ex-data-of #(assets/install! entry dir nil))]

                   (expect (= :voice-assets/install-failed (:type data)))
                   (expect (str/includes? (:error (first (:failures data))) "carrier-pigeon"))
                   (expect (false? (.exists (io/file dir)))))))

(defdescribe ensure-test
             (it "refuses to accept an opt-in asset's terms on the user's behalf"
                 (with-redefs [assets/installed? (constantly false)]
                   (let [data (ex-data-of #(assets/ensure! (assets/entry "piper-en_US-ryan-high")))]
                     (expect (= :voice-assets/opt-in-required (:type data)))
                     (expect (= "piper-en_US-ryan-high" (:id data)))
                     (expect (seq (:notice data)))
                     (expect (seq (:source-url data))))))
             (it "never asks twice for a model Vis may host itself"
                 ;; pocket-tts is our own CC BY 4.0 export, so the AUTOMATIC path installs
                 ;; it like any other asset instead of demanding that it be named.
                 (with-redefs [assets/installed? (constantly true)]
                   (expect (str/ends-with? (assets/ensure! (assets/entry "pocket-tts-int8"))
                                           "sherpa-onnx-pocket-tts-int8"))))
             (it "mirrors nothing it may not host, and hosts nothing it did not make"
                 ;; The pack is a release of ASSETS WE OWN: our own exports and the
                 ;; models we may mirror outright. Each Piper voice and its GPL phoneme
                 ;; tables come together from that voice's publisher, never from our pack.
                 (doseq [entry (assets/manifest)]
                   (let [pack (filter #(= :pack (:host %)) (:sources entry))]
                     (when (seq pack)
                       (expect (true? (:is-redistributed entry)) (:id entry))
                       (expect (true? (:is-commercial-ok entry)) (:id entry))
                       (expect (nil? (:voice entry)) (:id entry))
                       (doseq [source pack]
                         (expect (str/starts-with?
                                   (:url source)
                                   "https://github.com/Blockether/vis/releases/download/")
                                 (:id entry))))
                     (when-not (:is-redistributed entry) (expect (empty? pack) (:id entry)))))
                 (expect (empty? (filter #(= "espeak-ng-data" (:id %)) (assets/manifest)))))
             (it "requires every reference clip it says it ships"
                 ;; A bundle that promises a voice it does not carry is worse than one
                 ;; with no voice at all: the install reports itself ready and the
                 ;; first synthesis is what discovers the clip was never there.
                 (doseq [entry
                         (assets/manifest)

                         voice
                         (:voices entry)]

                   (expect (some? (:clip voice)) (:id entry))
                   (expect (str/starts-with? (:clip voice) "voices/") (:id entry))
                   (expect (contains? (set (:requires entry)) (:clip voice)) (:id entry))
                   ;; the transcript rides along, because the clone is given the words
                   (expect (seq (:clip-text voice)) (:id entry)))))


(defn- repo-document
  "The generated document at the repository root, found by walking up from the
   working directory so the test does not care where the runner starts."
  ^File []
  (loop [dir (.getAbsoluteFile (io/file "."))]
    (when dir
      (let [candidate (io/file dir attribution/document-name)]
        (if (.isFile candidate) candidate (recur (.getParentFile dir)))))))

(defdescribe
  third-party-models-test
  (it "keeps THIRD_PARTY_MODELS.md exactly what the manifest renders to"
      ;; The document a reader checks the credits in and the manifest
      ;; the installer obeys are ONE artifact. Editing either alone is
      ;; how a shipped model ends up credited to the wrong project.
      (let [document (repo-document)]
        (expect (some? document) (str attribution/document-name " is missing from the repository"))
        (expect (= (attribution/markdown) (slurp document))
                (str attribution/document-name
                     " no longer matches the manifest - regenerate it: "
                     attribution/regenerate-command))))
  (it "credits every entry it renders, and hides no source"
      ;; Equality with the file only proves the two agree; this proves
      ;; the render still carries what attribution IS - who made it,
      ;; under what terms, and every place the bytes come from.
      (let [rendered (attribution/markdown)]
        (doseq [entry (assets/manifest)]
          (expect (str/includes? rendered (:id entry)) (:id entry))
          (expect (str/includes? rendered (:attribution entry)) (:id entry))
          (expect (str/includes? rendered (:license entry)) (:id entry))
          (expect (str/includes? rendered (:source-url entry)) (:id entry))
          (when (:notice entry) (expect (str/includes? rendered (:notice entry)) (:id entry)))
          (when (:is-opt-in entry) (expect (str/includes? rendered "Opt-in") (:id entry)))
          (doseq [source (:sources entry)
                  :let [url (:url source)]
                  :when url]

            (expect (str/includes? rendered url) (:id entry)))))))
