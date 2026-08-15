(ns com.blockether.vis.ext.foundation-voice.attribution
  "`THIRD_PARTY_MODELS.md` is nobody's prose: it is
   `resources/vis-models/manifest.edn` rendered, so the credits a reader checks
   and the sources the installer obeys can never say different things.

   `markdown` is the whole file. `assets-test` fails when the copy in the tree
   drifts from it, and `vis-agent extension voice models licenses --markdown`
   reprints it."
  (:require [clojure.string :as str]
            [com.blockether.vis.ext.foundation-voice.assets :as assets]))

;; Reflective interop is FATAL in the native image - keep this ns reflection-free.
(set! *warn-on-reflection* true)

(def ^:const document-name "The generated file, at the repository root." "THIRD_PARTY_MODELS.md")

(def ^:const manifest-path
  "The one file this document is a render of."
  "extensions/common/vis-foundation-voice/resources/vis-models/manifest.edn")

(def ^:const regenerate-command
  "How a human puts the render back in the tree after editing the manifest."
  "vis-agent extension voice models licenses --markdown > THIRD_PARTY_MODELS.md")

(def ^:private host-labels
  {:pack "the Vis voice assets release" :hf "Hugging Face" :upstream "the publisher"})

(defn- megabytes
  "Rounded megabytes, because an exact byte count is the manifest's job."
  [byte-count]
  (str (Math/round (/ (double byte-count) 1000000.0)) " MB"))

(defn- source-bytes
  "How big this source is: one archive, or every file it downloads."
  ^long [source]
  (long (or (:bytes source) (reduce + 0 (keep :bytes (:files source))))))

(defn- source-line
  "One source, in the order `assets/sources` tries them."
  [source]
  (let
    [files
     (:files source)

     host
     (get host-labels (:host source) (name (:host source)))

     size
     (source-bytes source)

     ;; A file source downloads several files out of one repository: name the
     ;; repository, not the first file that happens to be listed.
     url
     (or (:url source)
         (some-> (some :url files)
                 (str/replace #"/resolve/.*$" "")))]

    (str "  - "
         host
         (when (pos? size) (str ", " (megabytes size)))
         (when (seq files) (str ", " (count files) " files"))
         (when (:is-token-required source) ", when a Hugging Face token is configured")
         (when url (str " - <" url ">")))))

(defn- voice-names
  "Every voice an entry speaks in - one for a Piper model, a clip set for a
   cloning model."
  [entry]
  (->> (concat (when-let [voice (:voice entry)]
                 [voice])
               (:voices entry))
       (map #(or (:label %) (:id %)))
       (remove nil?)))

(defn- entry-section
  "One manifest entry as a section: what it is, what it costs a redistributor,
   and where the bytes come from."
  [entry]
  (let [voices (voice-names entry)]
    (str/join
      "\n"
      (remove nil?
        [(str "## `" (:id entry) "`") ""
         (str "Engine `" (:engine entry)
              "` - " (:license entry)
              " - "
              (if (:is-commercial-ok entry) "commercial use permitted" "NOT for commercial use")
              " - " (if (:is-redistributed entry)
                      "hosted by Vis"
                      "downloaded from its publisher, never mirrored by Vis")) ""
         (:attribution entry) "" (when (:notice entry) (str "> " (:notice entry) "\n"))
         (str "- Upstream: <" (:source-url entry) ">")
         (when (seq voices) (str "- Voices: " (str/join ", " voices)))
         (str "- Installs into: `~/.vis/models/" (:install-dir entry) "`")
         (when (:needs-espeak-ng entry)
           "- Cannot speak until espeak-ng's phoneme tables are on the system.")
         (when (:is-opt-in entry)
           "- Opt-in: Vis never fetches this on its own; it is installed only when asked for by name.")
         "- Downloaded from, in order:" (str/join "\n" (map source-line (:sources entry)))]))))

(defn- summary-row
  [entry]
  (str "| `"
       (:id entry)
       "` | "
       (:license entry)
       " | "
       (if (:is-commercial-ok entry) "yes" "no")
       " | "
       (if (:is-redistributed entry) "the Vis assets release" "its publisher")
       " | "
       (if (:is-opt-in entry) "only when asked for by name" "automatically")
       " |"))

(def ^:private espeak-section
  (str/join
    "\n"
    ["## espeak-ng" ""
     "Not a model and not in the manifest. The Piper voices above phonemize through espeak-ng's"
     "tables, which are GPL-3.0-or-later DATA, so Vis ships neither them nor a native library with"
     "espeak-ng compiled in. The system installs them once (`brew install espeak-ng`,"
     "`apt install espeak-ng`) and every voice on the machine shares that copy."]))

(defn markdown
  "The entire `THIRD_PARTY_MODELS.md`, rendered from the manifest."
  []
  (let [entries (assets/manifest)]
    (str
      (str/join
        "\n"
        (concat
          ["# Third-party models" ""
           "Vis speaks and listens with models it did not train. Every model it can install is here,"
           "with its licence, who to credit, and whether Vis hosts a copy or sends you to the publisher."
           ""
           (str "This file is generated: it is `"
                manifest-path
                "` rendered. Edit the manifest, then run")
           (str "`" regenerate-command "`. A test fails when the two disagree.") ""
           "| model | licence | commercial use | comes from | installed |"
           "| --- | --- | --- | --- | --- |"]
          (map summary-row entries)
          [""]
          (interpose "" (map entry-section entries))
          (when (some :needs-espeak-ng entries) ["" espeak-section])))
      "\n")))
