(ns com.blockether.vis.ext.foundation-voice.sherpa-test
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]
            [com.blockether.vis.ext.foundation-voice.sherpa :as sherpa]
            [com.blockether.vis.ext.foundation-voice.tts]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [com.blockether.vis.ext.foundation_voice.tts GenerationCallback]
           [com.k2fsa.sherpa.onnx VersionInfo]
           [java.util.jar JarEntry JarFile]))

(defn- refused
  "The ex-info a lookup throws, as data — a platform sherpa has no library for
   must be REFUSED by name, never guessed into a download that 404s."
  [f]
  (try (f) :not-thrown (catch clojure.lang.ExceptionInfo e (:type (ex-data e)))))

;; Issue #143: `deps.edn` used to depend on all five of sherpa's native jars, so
;; every machine downloaded 51 MB of libraries to load the 10 MB it can run, and
;; four foreign platforms' libraries sat on every classpath and in every uberjar.
(defdescribe platform-token-test
             (it "answers the directory name sherpa's own loader looks under"
                 (expect (= "osx-aarch64" (sherpa/platform-token "Mac OS X" "aarch64")))
                 (expect (= "osx-aarch64" (sherpa/platform-token "Darwin" "arm64")))
                 (expect (= "osx-x64" (sherpa/platform-token "Mac OS X" "x86_64")))
                 (expect (= "linux-x64" (sherpa/platform-token "Linux" "amd64")))
                 (expect (= "linux-aarch64" (sherpa/platform-token "Linux" "aarch64")))
                 (expect (= "win-x64" (sherpa/platform-token "Windows 11" "amd64"))))
             (it "keeps the ORDER of LibraryUtils.getOsArch(), which two arches depend on"
                 ;; x86_64 must match the x64 branch before the x86 one, and arm64 is
                 ;; win-arm64 on Windows but linux-aarch64 everywhere else.
                 (expect (= "linux-x86" (sherpa/platform-token "Linux" "x86")))
                 (expect (= "win-arm64" (sherpa/platform-token "Windows 11" "aarch64")))
                 (expect (= "linux-arm" (sherpa/platform-token "Linux" "armv7l"))))
             (it "refuses a platform it has no name for instead of guessing one"
                 (expect (= :voice/unsupported-platform
                            (refused #(sherpa/platform-token "Solaris" "sparc"))))
                 (expect (= :voice/unsupported-platform
                            (refused #(sherpa/platform-token "Linux" "riscv64"))))))

(defdescribe library-names-test
             (it "names both libraries the way sherpa maps them for this OS"
                 (let [[runtime jni] (sherpa/library-names)]
                   ;; the ONNX Runtime is FIRST because sherpa loads it before its own
                   ;; JNI, so a system copy cannot win
                   (expect (= (System/mapLibraryName "onnxruntime") runtime))
                   (expect (= (System/mapLibraryName "sherpa-onnx-jni") jni)))))

(defdescribe ensure-native-test
             (it "provisions THIS platform's pair and the JNI answers the pinned version"
                 (let [{:keys [source platform dir]} (sherpa/ensure-native!)]
                   (expect (contains? #{:property :embedded :downloaded} source))
                   (expect (= (sherpa/platform-token) platform))
                   (when dir (expect (sherpa/installed? dir)))
                   ;; a NATIVE method: an answer at all means both libraries loaded, and
                   ;; the version is the integrity check no digest can be pinned for
                   (expect (= sherpa/version (VersionInfo/getVersion)))))
             (it "is idempotent — the pair is provisioned once per JVM"
                 (expect (= (sherpa/ensure-native!) (sherpa/ensure-native!))))
             (it "carries NO other platform's libraries"
                 ;; the classpath is where the 51 MB used to sit: every foreign platform
                 ;; must be absent from it, downloaded or not.
                 (doseq [other (disj sherpa/published-platforms (sherpa/platform-token))]
                   (expect (not (sherpa/embedded? other))))))

(defdescribe
  jar-url-test
  (it "fetches the native jar from its publisher and never from a Vis host"
      ;; espeak-ng is GPL-3 and compiled INTO libsherpa-onnx-jni - 10 `espeak_*`
      ;; symbols and its data paths are in the shipped library - so this jar is
      ;; the one artifact the voice pack must NOT mirror: the user gets it from
      ;; the project that published it.
      (let [url (sherpa/jar-url "osx-aarch64")]
        (expect (str/starts-with? url "https://jitpack.io/com/github/k2-fsa/sherpa-onnx/"))
        (expect (str/ends-with? url
                                (str "sherpa-onnx-native-lib-osx-aarch64-v" sherpa/version ".jar")))
        (expect (not (str/includes? url "Blockether"))))))


;; Regression, user report: voice failed with a linker error after the model was installed and
;; kept failing until Vis was restarted. The JVM caches a failed static initializer, so that
;; state is real - what was missing is a refusal that says so instead of a stack trace.
(defdescribe
  a-linker-failure-is-advice-test
  (it "recognises the JVM refusing to link at any depth in the cause chain"
      (expect (sherpa/native-error? (UnsatisfiedLinkError.
                                      "no sherpa-onnx-jni in java.library.path")))
      (expect (sherpa/native-error? (ex-info "transcribe failed"
                                             {}
                                             (ExceptionInInitializerError. (UnsatisfiedLinkError.
                                                                             "no onnxruntime")))))
      (expect (not (sherpa/native-error? (ex-info "the disk is full" {})))))
  (it "asks for a restart only when the process really cannot recover"
      (let
        [linker
         (sherpa/native-failure (NoClassDefFoundError. "com/k2fsa/sherpa/onnx/OfflineTts"))

         other
         (sherpa/native-failure (java.io.IOException. "connection reset"))]

        (expect (true? (:is-restart-required (ex-data linker))))
        (expect (str/includes? (ex-message linker) "restart Vis"))
        (expect (str/includes? (:remediation (ex-data linker)) "Restart Vis"))
        (expect (false? (:is-restart-required (ex-data other))))
        (expect (str/includes? (:remediation (ex-data other)) sherpa/native-dir-env))))
  (it "passes a working call straight through and only translates what it must"
      (with-redefs [sherpa/ensure-native! (constantly true)]
        (expect (= :spoken (sherpa/call-native (constantly :spoken))))
        (let
          [ordinary (try (sherpa/call-native #(throw (ex-info "no such voice"
                                                              {:type :voice-tts/unknown-voice})))
                         nil
                         (catch Throwable t t))]
          (expect (= :voice-tts/unknown-voice (:type (ex-data ordinary)))
                  "an engine's own refusal is not dressed up as a native failure")))))


;; ── the JNI's view of the API jar ────────────────────────────────────────────

(def ^:private metadata-resource
  "Where the native image build reads this extension's reachability metadata."
  "META-INF/native-image/com.blockether/vis-foundation-voice/reachability-metadata.json")

(defn- registered
  "Every type the metadata hands to the JNI, and the whole entry by type."
  []
  (let [url (io/resource metadata-resource)]
    (when-not url
      (throw (ex-info "The extension ships no reachability metadata."
                      {:resource metadata-resource})))
    (into {}
          (map (juxt :type identity))
          (:reflection (json/read-str (slurp url) :key-fn keyword)))))

(defn- sherpa-type?
  "Types of sherpa's own API, as opposed to the one class of OURS the JNI calls
   back into."
  [^String type-name]
  (str/starts-with? type-name "com.k2fsa.sherpa.onnx."))

(defn- api-types
  "Every class of sherpa's API jar, read from the jar `VersionInfo` came from.
   The JNI may ask for any of them BY NAME, so the metadata has to name them all."
  []
  (let
    [^Class api
     VersionInfo

     location
     (.. api getProtectionDomain getCodeSource getLocation)

     jar
     (io/file (.toURI location))]

    (if (.isDirectory jar)
      (throw (ex-info "sherpa is on the classpath as a directory, not its jar." {:path (str jar)}))
      (with-open [zip (JarFile. jar)]
        (into (sorted-set)
              (comp (map #(.getName ^JarEntry %))
                    (filter #(and (str/starts-with? % "com/k2fsa/sherpa/onnx/")
                                  (str/ends-with? % ".class")))
                    (map #(-> ^String %
                              (subs 0 (- (count %) 6))
                              (str/replace "/" "."))))
              (enumeration-seq (.entries zip)))))))

;; Regression, user report: speaking and listening failed in the SHIPPED BINARY
;; with the models already installed, and restarting Vis was the only cure
;; anyone found. sherpa's JNI reads its own Java API by name - GetFieldID
;; "model", FindClass "com/k2fsa/sherpa/onnx/WaveData" - and native-image keeps
;; no name it cannot see used, so the image linked and then could not speak.
(defdescribe
  jni-metadata-test
  (it "hands the JNI every type sherpa's API jar carries"
      (let
        [known
         (registered)

         api
         (api-types)]

        (expect (seq api) "no sherpa classes were found - the API jar is not on this classpath")
        (expect (= api (into (sorted-set) (filter sherpa-type?) (keys known)))
                (str "reachability-metadata.json no longer matches sherpa-onnx-jvm."
                     "\n  missing: " (pr-str (set/difference api (set (keys known))))
                     "\n  gone from the jar: "
                     (pr-str (set/difference (into #{} (filter sherpa-type?) (keys known)) api))))))
  (it "registers the class sherpa calls BACK into, under the name it really has"
      ;; Regression, same report: with every sherpa type registered the image
      ;; got as far as printing its config and then died inside the first
      ;; progress callback. sherpa asks for that method with GetMethodID on OUR
      ;; class, so the class needs a name a build cannot change and metadata of
      ;; its own - a `reify` had neither.
      (let
        [known
         (registered)

         callback
         (.getName GenerationCallback)]

        (expect (get known callback) (str callback " is not registered for JNI"))
        (expect (:jniAccessible (get known callback)))
        (expect (:allDeclaredMethods (get known callback)))))
  (it "asks for the members the JNI looks up, not merely for the class"
      ;; A type registered without its FIELDS is the failure the user hit: the
      ;; class exists in the image and `GetFieldID` still finds nothing.
      (let
        [thin (remove (fn [[_ e]]
                        (and (:jniAccessible e)
                             (:allDeclaredFields e)
                             (:allDeclaredMethods e)
                             (:allDeclaredConstructors e)))
                (registered))]
        (expect (empty? thin)
                (str "registered too thinly to speak through: " (pr-str (mapv key thin))))))
  (it "names the two the JNI asked for first"
      (let [known (registered)]
        (expect (get known "com.k2fsa.sherpa.onnx.OfflineTtsConfig"))
        (expect (get known "com.k2fsa.sherpa.onnx.WaveData")))))
