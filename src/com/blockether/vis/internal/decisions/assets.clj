(ns com.blockether.vis.internal.decisions.assets
  "Pinned, verified decision bundles. A request never installs assets implicitly."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.internal.config.core :as config]
            [com.blockether.vis.internal.speech.files :as files]
            [com.blockether.vis.internal.util :as util])
  (:import [java.io File FileInputStream FileOutputStream]
           [java.nio.file Path]
           [java.security MessageDigest]
           [java.util.zip ZipInputStream]))

(set! *warn-on-reflection* true)

(def ^:const manifest-resource "vis-models/decisions.json")

(def ^:const models-dir-env "VIS_DECISION_MODELS_DIR")

(def ^:const max-inference-upload-bytes 1600000000)

(def ^:private max-expanded-bytes 3000000000)

(def ^:private inference-required
  ["model.onnx" "rl_agent_config.json" "tokenizer/tokenizer.json" "tokenizer/tokenizer_config.json"
   "PROVENANCE.json" "LICENSE.txt"])

(defonce ^:private manifest*
  (delay (if-let [resource (io/resource manifest-resource)]
           (mapv walk/keywordize-keys (wire/parse-json (slurp resource)))
           (throw (ex-info "Decision asset manifest is missing" {:resource manifest-resource})))))

(defn manifest "Pinned model entries in manifest order." [] @manifest*)

(defn entry
  "Find an exact model name; an unknown name never falls back to the baseline."
  [id]
  (or (some #(when (= id (:id %)) %) (manifest))
      (throw (ex-info (str "Unknown decision model: " id)
                      {:type :decisions/unknown-model :model id}))))

(defn platform
  "The platform whose CPython 3.12 wheelhouse can be installed locally."
  []
  (let [os
        (System/getProperty "os.name")

        arch
        (System/getProperty "os.arch")]

    (cond (and (str/includes? os "Mac") (contains? #{"aarch64" "arm64"} arch)) "macos-arm64"
          (and (str/includes? os "Linux") (contains? #{"amd64" "x86_64"} arch)) "linux-x86_64"
          :else (throw (ex-info (str "No decision training wheelhouse for " os " / " arch)
                                {:type :decisions/unsupported-platform :os os :arch arch})))))

(defn models-root
  "The persistent store, resolved at runtime rather than native-image build time."
  []
  (or (config/extension-env-value models-dir-env)
      (str (System/getProperty "user.home") "/.vis/models/decisions")))

(defn artifact
  "An inference, training or platform-specific wheelhouse from one manifest entry."
  ([model kind] (artifact model kind (when (= kind :wheels) (platform))))
  ([model kind target-platform]
   (or (if (= kind :wheels)
         (get-in model [:artifacts :wheels (keyword target-platform)])
         (get-in model [:artifacts kind]))
       (throw (ex-info (str "No " (name kind) " asset for " (:id model))
                       {:type :decisions/unknown-artifact
                        :model (:id model)
                        :kind kind
                        :platform target-platform})))))

(defn install-dir
  "Stable location of an immutable revision and artifact kind."
  ([model kind] (install-dir model kind (when (= kind :wheels) (platform))))
  ([model kind target-platform]
   (str (io/file (models-root)
                 (:id model)
                 (:revision model)
                 (if (= kind :wheels) (str "wheels-" target-platform) (name kind))))))

(defn- safe-file
  ^File [^File dir name]
  (when (or (not (string? name))
            (str/blank? name)
            (str/includes? name "\\")
            (str/includes? name (str (char 0)))
            (some #{"" "." ".."} (str/split name #"/" -1)))
    (throw (ex-info "Unsafe decision archive path" {:type :decisions/unsafe-path})))
  (let [^Path root
        (.toPath (.getAbsoluteFile dir))

        ^Path relative
        (.toPath (io/file name))

        ^Path target
        (.normalize (.resolve root relative))]

    (when (or (.isAbsolute relative) (not (.startsWith target root)) (= target root))
      (throw (ex-info "Unsafe decision archive path" {:type :decisions/unsafe-path})))
    (.toFile target)))

(defn- verified-files!
  [^File dir artifact]
  (let [provenance
        (wire/parse-json (slurp (io/file dir "PROVENANCE.json")))

        listed
        (if (= "training-dependencies" (get provenance "kind"))
          (into {}
                (map (fn [wheel]
                       [(str "wheels/" (get wheel "wheel")) wheel]))
                (get provenance "wheels"))
          (get provenance "files"))]

    (when-not (seq listed)
      (throw (ex-info "Decision bundle has no file inventory" {:type :decisions/no-inventory})))
    (doseq [[name details] listed]
      (let [^File f (safe-file dir name)]
        (when (or (not (.isFile f)) (not= (.length f) (long (get details "bytes"))))
          (throw (ex-info (str "Decision bundle is missing or truncated: " name)
                          {:type :decisions/incomplete :file name})))
        (with-open [stream (FileInputStream. f)]
          (let [^MessageDigest digest (util/sha256-digest)
                buffer (byte-array 1048576)]

            (loop []

              (let [n (.read stream buffer)]
                (when (pos? n) (.update digest buffer 0 n) (recur))))
            (when-not (= (get details "sha256") (util/bytes->hex (.digest digest)))
              (throw (ex-info (str "Decision bundle file checksum failed: " name)
                              {:type :decisions/file-checksum :file name})))))))
    (doseq [name (:requires artifact)]
      (when-not (.isFile (safe-file dir name))
        (throw (ex-info (str "Decision bundle missing: " name)
                        {:type :decisions/incomplete :file name}))))
    provenance))

(defn- extract!
  [^File archive ^File dir]
  (.mkdirs dir)
  (with-open [zip (ZipInputStream. (FileInputStream. archive))]
    (let [buffer (byte-array 262144)]
      (loop [seen #{}
             expanded 0]

        (if-let [item (.getNextEntry zip)]
          (let [name (.getName item)
                ^File target (safe-file dir name)]

            (when (or (contains? seen name) (>= (count seen) 1024))
              (throw (ex-info "Duplicate or oversized decision archive"
                              {:type :decisions/invalid-archive})))
            (let [^long written (if (.isDirectory item)
                                  (do (.mkdirs target) 0)
                                  (do (.mkdirs (.getParentFile target))
                                      (with-open [out (FileOutputStream. target)]
                                        (loop [written 0]
                                          (let [n (.read zip buffer)]
                                            (if (neg? n)
                                              written
                                              (let [total (+ expanded written n)]
                                                (when (> (long total) (long max-expanded-bytes))
                                                  (throw (ex-info
                                                           "Decision archive exceeds size limit"
                                                           {:type :decisions/invalid-archive})))
                                                (.write out buffer 0 n)
                                                (recur (+ written n)))))))))]
              (.closeEntry zip)
              (recur (conj seen name) (long (+ expanded written)))))
          seen)))))

(defn installed?
  "An archive verified at installation, with all required files still present."
  [artifact dir]
  (let [^File root (io/file dir)]
    (and (= (:sha256 artifact)
            (when (.isFile (io/file root ".vis-verified"))
              (str/trim (slurp (io/file root ".vis-verified")))))
         (every? #(.isFile (safe-file root %)) (:requires artifact)))))

(defn install!
  "Download and verify an archive before atomically replacing an incomplete install.
   The optional destination and URL make the exact same path testable without network."
  ([model kind] (install! model kind (install-dir model kind) nil))
  ([model kind dir url]
   (let [asset
         (artifact model kind)

         ^File target
         (io/file dir)

         ^File staging
         (io/file (str dir ".staging-" (System/nanoTime)))

         ^File backup
         (io/file (str dir ".backup-" (System/nanoTime)))

         ^File archive
         (File/createTempFile "vis-decision-" ".zip")]

     (if (installed? asset dir)
       (do (.delete archive) dir)
       (try (files/download! (or url (:url asset)) (str archive) {:sha256 (:sha256 asset)})
            (when-not (= (.length archive) (long (:bytes asset)))
              (throw (ex-info "Decision archive size does not match the manifest"
                              {:type :decisions/archive-size})))
            (extract! archive staging)
            (let [provenance (verified-files! staging asset)]
              (when (or (not= (:revision model) (get provenance "revision"))
                        (not= (:id model) (get provenance "model")))
                (when-not (= kind :wheels)
                  (throw (ex-info "Decision archive model identity does not match"
                                  {:type :decisions/archive-identity})))))
            (spit (io/file staging ".vis-verified") (str (:sha256 asset) "\n"))
            (.mkdirs (.getParentFile target))
            (when (and (.exists target) (not (.renameTo target backup)))
              (throw (ex-info "Cannot preserve previous decision bundle"
                              {:type :decisions/install-failed})))
            (when-not (.renameTo staging target)
              (when (.exists backup) (.renameTo backup target))
              (throw (ex-info "Cannot activate verified decision bundle"
                              {:type :decisions/install-failed})))
            (when (.exists backup) (files/delete-dir! backup))
            dir
            (finally (.delete archive) (when (.exists staging) (files/delete-dir! staging))))))))

(defn download-model!
  "Install FP32 only, or FP32 plus the complete checkpoint and local wheelhouse."
  [id training?]
  (let [model
        (entry id)

        kinds
        (if training? [:inference :training :wheels] [:inference])]

    (into {}
          (map (fn [kind]
                 [kind (install! model kind)])
               kinds))))

(defn unpack-upload!
  "Verify and extract a bounded inference-only archive into an empty private directory.
   The caller validates the runtime before making that directory visible."
  [^File archive ^File dir expected-sha]
  (when-not (and (string? expected-sha)
                 (re-matches #"[0-9a-f]{64}" expected-sha)
                 (<= 1 (.length archive) max-inference-upload-bytes))
    (throw (ex-info "Decision upload exceeds the archive limit"
                    {:type :decisions/invalid-archive})))
  (let [digest
        (util/sha256-digest)

        buffer
        (byte-array 1048576)]

    (with-open [in (FileInputStream. archive)]
      (loop []

        (let [n (.read in buffer)]
          (when (pos? n) (.update ^MessageDigest digest buffer 0 n) (recur)))))
    (when-not (= expected-sha (util/bytes->hex (.digest ^MessageDigest digest)))
      (throw (ex-info "Decision upload checksum failed" {:type :decisions/archive-checksum}))))
  (let [names
        (extract! archive dir)

        provenance-file
        (io/file dir "PROVENANCE.json")]

    (when (or (not (.isFile provenance-file)) (> (.length provenance-file) 1048576))
      (throw (ex-info "Decision provenance is missing or oversized"
                      {:type :decisions/invalid-archive})))
    (let [provenance
          (try (verified-files! dir {:requires inference-required})
               (catch Exception e
                 (throw (ex-info "Decision upload has an invalid file inventory"
                                 {:type :decisions/invalid-archive}
                                 e))))

          listed
          (set (keys (get provenance "files")))

          files
          (disj names "PROVENANCE.json" "LICENSE.txt")]

      (when-not (and (= "inference" (get provenance "kind"))
                     (= "onnx" (get provenance "format"))
                     (= "fp32" (get provenance "precision"))
                     (= "laya-typed-decisions" (get provenance "model"))
                     (= "Apache-2.0" (get provenance "license"))
                     (re-matches #"(?:[0-9a-f]{40}|[0-9a-f]{64})" (str (get provenance "revision")))
                     (= listed files)
                     (every? #(or (= % "model.onnx")
                                  (= % "model.onnx.data")
                                  (= % "rl_agent_config.json")
                                  (re-matches #"tokenizer/[A-Za-z0-9_.-]+\.(?:json|txt)" %))
                             listed))
        (throw (ex-info "Only a complete Laya FP32 inference bundle may be uploaded"
                        {:type :decisions/invalid-archive})))
      (spit (io/file dir ".vis-verified") (str expected-sha "\n"))
      provenance)))
