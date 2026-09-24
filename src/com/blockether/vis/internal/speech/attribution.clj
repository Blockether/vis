(ns com.blockether.vis.internal.speech.attribution
  "`THIRD_PARTY_MODELS.md` renders the speech and decision asset manifests, so the
   credits a reader checks and the pinned downloads stay in step.

   `markdown` is the whole file. `assets-test` fails when the copy in the tree
   drifts from it, and `vis-agent speech models licenses --markdown`
   reprints it."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.decisions.assets :as decisions]
            [com.blockether.vis.internal.speech.assets :as assets]))

;; Reflective interop is FATAL in the native image - keep this ns reflection-free.
(set! *warn-on-reflection* true)

(def ^:const document-name "The generated file, at the repository root." "THIRD_PARTY_MODELS.md")

(def ^:const manifest-path
  "The speech manifest; decisions live in the adjacent decisions.json."
  "resources/vis-models/manifest.edn")

(def ^:const regenerate-command
  "How a human puts the render back in the tree after editing the manifest."
  "vis-agent speech models licenses --markdown > THIRD_PARTY_MODELS.md")

(def ^:private host-labels
  {:pack "the Vis assets release" :hf "Hugging Face" :upstream "the publisher"})

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
  (let [files
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
         ;; Not every entry is an engine's model: the samples pack is what the
         ;; catalogue SOUNDS like, and calling it an engine would credit it wrong.
         (str (if-let [engine (:engine entry)]
                (str "Engine `" engine "`")
                "Voice samples")
              " - " (:license entry)
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
           "- Includes eSpeak NG phoneme tables from the publisher archive (GPL-3.0-or-later).")
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
    ["## eSpeak NG phoneme data" ""
     "Piper uses eSpeak NG phoneme data under GPL-3.0-or-later. Each publisher archive includes"
     "the data required by its voice. Vis downloads it to the model store without administrator"
     "access and does not redistribute that data in Vis releases."]))

(defn- decision-section
  [entry]
  (let [artifacts (concat [[:inference (get-in entry [:artifacts :inference])]
                           [:training (get-in entry [:artifacts :training])]]
                          (for [platform ["macos-arm64" "linux-x86_64"]]
                            [(str "wheels " platform)
                             (get-in entry [:artifacts :wheels (keyword platform)])]))]
    (str/join
      "\n"
      (concat
        [(str "## `" (:id entry) "`") ""
         (str "Decision baseline model - "
              (:license entry)
              " - commercial use permitted - hosted by Vis.") "" (:attribution entry) ""
         (str "- Upstream: <" (:source-url entry) ">")
         (str "- Pinned revision: `" (:revision entry) "`")
         (str "- Installs into: `~/.vis/models/decisions/" (:id entry) "/" (:revision entry) "/`")
         "- Downloaded from the shared Vis assets-pack release (verified by SHA-256):"]
        (for [[kind artifact] artifacts]
          (str "  - "
               (if (keyword? kind) (name kind) kind)
               ": <"
               (:url artifact)
               "> ("
               (megabytes (:bytes artifact))
               ", SHA-256 `"
               (:sha256 artifact)
               "`)"))
        ["- The two optional CPython 3.12 wheelhouses contain pinned dependency wheels,"
         "  their SHA-256, upstream URLs and licenses in `PROVENANCE.json`; license texts"
         "  are in each wheel or in `licenses/`. Linux uses CPU-only PyTorch."
         "- Training assets and wheels are installed only when explicitly requested."
         "- A base model is not approved to execute autonomous actions without a"
         "  separate evaluation of both decision heads for the intended use case."]))))

(defn markdown
  "The entire `THIRD_PARTY_MODELS.md`, rendered from both manifests."
  []
  (let [entries
        (assets/manifest)

        decision-entries
        (decisions/manifest)]

    (str (str/join
           "\n"
           (concat
             ["# Third-party models" ""
              "Vis uses third-party models for speech and decisions. This table lists"
              "their licenses, authors and download sources." ""
              (str "This file is generated from `" manifest-path "` and")
              "`resources/vis-models/decisions.json`. Edit the manifests, then run"
              (str "`" regenerate-command "`. Tests check that the document matches the manifest.")
              "" "| Model | License | Commercial use | Source | Installation |"
              "| --- | --- | --- | --- | --- |"]
             (concat (map summary-row entries)
                     (for [entry decision-entries]
                       (str "| `"
                            (:id entry)
                            "` | "
                            (:license entry)
                            " | yes | the Vis assets release | only when asked for by name |")))
             [""]
             (interpose "" (map entry-section entries))
             (when (some :needs-espeak-ng entries) ["" espeak-section])
             ["" "## Decision models" ""]
             (interpose "" (map decision-section decision-entries))))
         "\n")))
