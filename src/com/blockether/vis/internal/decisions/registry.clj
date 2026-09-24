(ns com.blockether.vis.internal.decisions.registry
  "Immutable local imports and compare-and-swap aliases for verified decision bundles."
  (:require [clojure.java.io :as io]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.internal.decisions.assets :as assets]
            [com.blockether.vis.internal.speech.files :as files])
  (:import [java.io File]
           [java.nio.file Files StandardCopyOption]))

(set! *warn-on-reflection* true)

(def ^:private lock (Object.))

(defn- imported-root ^File [] (io/file (assets/models-root) "registered"))

(defn- version-ref? [ref] (and (string? ref) (boolean (re-matches #"sha256-[0-9a-f]{64}" ref))))

(defn- alias?
  [name]
  (and (string? name)
       (boolean (re-matches #"[a-z][a-z0-9-]{0,63}" name))
       (not (contains? (conj (set (keys assets/gliner-architectures)) "laya-typed-decisions")
                       name))))

(defn- record
  [ref]
  (when (version-ref? ref)
    (let [dir
          (io/file (imported-root) ref)

          metadata
          (io/file dir "manifest.json")]

      (when (.isFile metadata)
        (let [value
              (wire/parse-json (slurp metadata))

              model-id
              (get value "model" "laya-typed-decisions")]

          (when (and (= ref (get value "model_ref"))
                     (= (subs ref 7) (get value "sha256"))
                     (re-matches #"(?:[0-9a-f]{40}|[0-9a-f]{64})" (str (get value "revision")))
                     (try (assets/inference-required model-id)
                          (catch clojure.lang.ExceptionInfo _ nil)))
            (assoc value "model" model-id)))))))

(defn- installed-record
  [ref]
  (when-let [value (record ref)]
    (let [model-id (get value "model")
          dir (io/file (imported-root) ref "inference")
          artifact {:sha256 (get value "sha256") :requires (assets/inference-required model-id)}]

      (when (assets/installed? artifact (.getPath dir))
        {:model-ref ref
         :model {:id model-id :revision (get value "revision")}
         :artifact artifact
         :dir dir}))))

(defn versions
  "Immutable imported versions, read from persistent local storage."
  []
  (let [root (imported-root)]
    (if (.isDirectory root)
      (->> (.listFiles root)
           (filter #(.isDirectory ^File %))
           (keep (fn [^File dir]
                   (when-let [value (installed-record (.getName dir))]
                     {"model_ref" (:model-ref value)
                      "revision" (get-in value [:model :revision])
                      "installed" true})))
           (sort-by #(get % "model_ref"))
           vec)
      [])))

(defn- aliases
  []
  (let [source (io/file (assets/models-root) "aliases.json")]
    (if (.isFile source)
      (let [data (wire/parse-json (slurp source))]
        (when-not (and (map? data)
                       (every? (fn [[name ref]]
                                 (and (alias? name) (version-ref? ref)))
                               data))
          (throw (ex-info "Decision alias registry is invalid"
                          {:type :decisions/invalid-registry})))
        data)
      {})))

(defn get-alias
  "Read the selected immutable version, if this alias exists."
  [name]
  (when (alias? name)
    (when-let [ref (get (aliases) name)]
      {"alias" name "model_ref" ref})))

(defn resolve-model
  "Look up a registered ref or alias. The caller handles pinned baseline refs."
  [name]
  (let [ref (if (version-ref? name) name (get (aliases) name))]
    (installed-record ref)))

(defn register!
  "Verify a local archive, validate its FP32 runtime, then publish atomically.
   `validate!` receives the immutable model metadata and private inference directory."
  [^File archive sha validate!]
  (when-not (and (string? sha) (re-matches #"[0-9a-f]{64}" sha))
    (throw (ex-info "Decision upload digest must be SHA-256" {:type :decisions/invalid-archive})))
  (let [ref
        (str "sha256-" sha)

        target
        (io/file (imported-root) ref)

        existing
        (installed-record ref)]

    (if existing
      {"model_ref" ref "revision" (get-in existing [:model :revision]) "installed" true}
      (do
        (.mkdirs (imported-root))
        (let [stage
              (io/file (imported-root) (str ".pending-" (random-uuid)))

              inference
              (io/file stage "inference")]

          (try (let [provenance
                     (assets/unpack-upload! archive inference sha)

                     revision
                     (get provenance "revision")

                     metadata
                     {"model_ref" ref
                      "revision" revision
                      "sha256" sha
                      "model" (get provenance "model")}

                     model
                     {:id (get provenance "model") :revision revision}]

                 (validate! model inference)
                 (spit (io/file stage "manifest.json") (str (wire/json-str metadata) "\n"))
                 (locking lock
                   (if-let [saved (installed-record ref)]
                     {"model_ref" ref "revision" (get-in saved [:model :revision]) "installed" true}
                     (do (when (.exists target)
                           (throw (ex-info "Existing decision version is incomplete"
                                           {:type :decisions/invalid-registry})))
                         (Files/move (.toPath stage)
                                     (.toPath target)
                                     (into-array StandardCopyOption
                                                 [StandardCopyOption/ATOMIC_MOVE]))
                         {"model_ref" ref "revision" revision "installed" true}))))
               (finally (when (.exists stage) (files/delete-dir! stage)))))))))

(defn activate!
  "Explicit CAS. Nil expected_current creates an alias; updating needs its exact old ref."
  [name new-ref expected-current]
  (when-not (and (alias? name)
                 (version-ref? new-ref)
                 (or (nil? expected-current) (version-ref? expected-current)))
    (throw (ex-info "Invalid decision alias or immutable reference"
                    {:type :decisions/invalid-alias})))
  (locking lock
    (when-not (installed-record new-ref)
      (throw (ex-info "Decision version is not registered" {:type :decisions/unknown-model})))
    (let [current (aliases)]
      (when-not (= expected-current (get current name))
        (throw (ex-info "Decision alias changed; read it before retrying"
                        {:type :decisions/alias-conflict})))
      (let [file (io/file (assets/models-root) "aliases.json")]
        (.mkdirs (.getParentFile file))
        (let [temporary (File/createTempFile ".decision-alias-" ".json" (.getParentFile file))]
          (try (spit temporary (str (wire/json-str (assoc current name new-ref)) "\n"))
               (Files/move (.toPath temporary)
                           (.toPath file)
                           (into-array StandardCopyOption
                                       [StandardCopyOption/ATOMIC_MOVE
                                        StandardCopyOption/REPLACE_EXISTING]))
               (finally (.delete temporary)))))
      {"alias" name "model_ref" new-ref})))
