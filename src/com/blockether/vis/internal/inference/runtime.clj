(ns com.blockether.vis.internal.inference.runtime
  "One ONNX Runtime native library shared by Sherpa speech and Java decisions."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.config.core :as config])
  (:import [java.io File InputStream]))

(set! *warn-on-reflection* true)

(def ort-version "1.30.0")

(def sherpa-version "1.13.8")

(def native-dir-env "VIS_SHERPA_NATIVE_DIR")

(def ort-native-path-property "onnxruntime.native.path")

(def sherpa-native-path-property "sherpa_onnx.native.path")

(def shared-platforms
  "Platforms built and tested with both ONNX Runtime Java and Sherpa JNI."
  #{"osx-aarch64" "linux-x64" "linux-aarch64"})

(defn platform-token
  "The native directory token shared by the two Java loaders for this OS and CPU."
  ([]
   (platform-token (System/getProperty "os.name" "generic")
                   (System/getProperty "os.arch" "generic")))
  ([os-name os-arch]
   (let [os
         (str/lower-case (str os-name))

         arch
         (str/lower-case (str os-arch))

         o
         (cond (or (str/includes? os "mac") (str/includes? os "darwin")) "osx"
               (str/includes? os "win") "win"
               (str/includes? os "nux") "linux"
               :else (throw (ex-info "sherpa-onnx has no native library for this operating system"
                                     {:type :speech/unsupported-platform :os os-name})))

         a
         (cond (or (str/starts-with? arch "amd64") (str/starts-with? arch "x86_64")) "x64"
               (str/starts-with? arch "x86") "x86"
               (or (str/starts-with? arch "aarch64") (str/starts-with? arch "arm64"))
               (if (= "win" o) "arm64" "aarch64")
               (str/starts-with? arch "arm") "arm"
               :else (throw (ex-info "sherpa-onnx has no native library for this CPU architecture"
                                     {:type :speech/unsupported-platform :arch os-arch})))]

     (str o "-" a))))

(defn native-cache-name
  "The compatible runtime and JNI cache name for this release platform."
  [token]
  (str "onnxruntime-"
       ort-version
       "-sherpa-"
       sherpa-version
       (if (contains? #{"linux-x64" "linux-aarch64"} token) "-abi2" "")))

(defn default-native-dir
  "Versioned path so upgrading either JNI package or Linux ABI cannot reuse an old library."
  [token]
  (str (io/file (System/getProperty "user.home") ".vis" "native" (native-cache-name token) token)))

(defn native-dir
  "The one path both JNI loaders use on a supported native release platform."
  ([] (native-dir (platform-token)))
  ([token] (or (config/extension-env-value native-dir-env) (default-native-dir token))))

(defn library-names
  "The runtime, Java JNI shim and Sherpa JNI shim for this platform."
  []
  [(System/mapLibraryName "onnxruntime") (System/mapLibraryName "onnxruntime4j_jni")
   (System/mapLibraryName "sherpa-onnx-jni")])

(defn installed?
  "A staged library must be complete before a loader sees it."
  [dir name]
  (let [^File file (io/file (str dir) name)]
    (and (not (str/blank? (str dir))) (.isFile file) (pos? (.length file)))))

(defn install-stream!
  "Copy a bundled JNI library atomically into the shared native directory."
  [dir name ^InputStream stream]
  (with-open [in stream]
    (let [^File parent (io/file (str dir))
          ^File target (io/file parent name)]

      (when-not (installed? dir name)
        (when-not (or (.isDirectory parent) (.mkdirs parent))
          (throw (ex-info "Could not create the ONNX Runtime library directory"
                          {:type :inference/native-install-failed :dir (str dir)})))
        (let [^File staging (File/createTempFile "vis-onnx-" ".tmp" parent)]
          (try (with-open [out (io/output-stream staging)]
                 (io/copy in out))
               (when (zero? (.length staging))
                 (throw (ex-info "An ONNX Runtime native library is empty"
                                 {:type :inference/native-incomplete :name name})))
               (when-not (or (.renameTo staging target) (installed? dir name))
                 (throw (ex-info
                          "Could not install an ONNX Runtime native library"
                          {:type :inference/native-install-failed :name name :dir (str dir)})))
               (finally (when (.exists staging) (.delete staging))))))
      target)))

(defn install-resource!
  "Stage a classpath native resource, refusing a missing platform library."
  [dir name resource]
  (when-not (installed? dir name)
    (let [url (or (io/resource resource)
                  (throw (ex-info "The ONNX Runtime native resource is missing"
                                  {:type :inference/native-incomplete :resource resource})))]
      (install-stream! dir name (io/input-stream url)))))

(defn ensure-ort!
  "Stage ORT 1.30 and the Java JNI before OrtEnvironment initializes.
   An explicit Java native path is respected when complete. Other platforms
   retain their independent upstream loaders."
  []
  (let [token (platform-token)]
    (if-not (contains? shared-platforms token)
      {:source :upstream :platform token}
      (locking ort-native-path-property
        (let [[runtime jni _] (library-names)
              given (System/getProperty ort-native-path-property)]

          (if (and given (installed? given runtime) (installed? given jni))
            {:source :property :platform token :dir given}
            (let [dir (.getAbsolutePath (io/file (native-dir token)))]
              (doseq [name [runtime jni]]
                (install-resource! dir name (str "ai/onnxruntime/native/" token "/" name)))
              (System/setProperty ort-native-path-property dir)
              {:source :embedded :platform token :dir dir})))))))
