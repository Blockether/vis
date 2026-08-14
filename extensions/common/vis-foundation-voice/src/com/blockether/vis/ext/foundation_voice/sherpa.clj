(ns com.blockether.vis.ext.foundation-voice.sherpa
  "sherpa-onnx's native libraries, for THIS platform and no other.

   sherpa publishes one native jar per platform, each 8-13 MB and each carrying
   BOTH `libsherpa-onnx-jni` and the exact `libonnxruntime` it was linked
   against, side by side under `sherpa-onnx/native/<platform>/`. Depending on
   all five in `deps.edn` makes every machine download 51 MB to use one of them,
   so the extension depends on the 187 KB API jar ALONE and the pair arrives
   here, one of three ways:

   - **Already loadable** — `sherpa_onnx.native.path` names a directory holding
     both libraries. This is sherpa's own first loading method, so it is also
     the seam for a self-built native (an espeak-free one, say) and nothing is
     downloaded or checked out from under the user.
   - **Embedded** — the libraries are classpath resources. That is the native
     image, where `build.clj` puts the BUILD HOST's native jar on the image
     classpath and `-H:IncludeResources` bakes that one directory in, and any
     JVM run that puts a `sherpa-onnx-native-lib-*` jar on the classpath.
   - **Downloaded** — the host platform's jar is fetched once from the same
     JitPack coordinate `deps.edn` pins, unpacked into `~/.vis`, and handed to
     sherpa through `sherpa_onnx.native.path`.

   No digest is pinned for that download: JitPack rebuilds a tag when its cache
   evicts, so a pinned digest would eventually break every user rather than
   catch anything. Integrity comes from the transfer being length-checked
   (`files/download!`), from the install being atomic, and from the loaded
   library having to answer `version` — which `sherpa-native-test` asserts."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.foundation-voice.files :as files])
  (:import [java.io File]
           [java.util.zip ZipFile]))

;; Reflective interop is FATAL in the native image (needs metadata per call
;; site) — keep this ns reflection-free at compile time.
(set! *warn-on-reflection* true)

(def version
  "The sherpa-onnx release this extension is built against. `deps.edn` pins tag
   `v<version>` of the API jar and the native pair MUST come from that same tag:
   the JNI and the ONNX Runtime beside it are one unit, which is the whole
   reason there is no ONNX Runtime coordinate to keep in step any more."
  "1.13.5")

(def native-dir-env "VIS_SHERPA_NATIVE_DIR")

(def native-path-property
  "sherpa's own override, documented at the top of its `LibraryUtils`: a
   directory holding sherpa-onnx-jni AND onnxruntime, loaded in that order."
  "sherpa_onnx.native.path")

(def published-platforms
  "The platforms k2-fsa publishes a native jar for. `LibraryUtils` also names
   linux-arm, win-arm64 and the x86 pair, but no jar exists for those — they
   need `sherpa_onnx.native.path` and a self-built library."
  #{"osx-aarch64" "osx-x64" "linux-x64" "linux-aarch64" "win-x64"})

(defn platform-token
  "The `sherpa-onnx/native/<token>` directory name for an os/arch pair — the
   MIRROR of `LibraryUtils.getOsArch()`, including the ORDER of its tests, since
   `x86_64` matches the x64 branch before the x86 one and `arm64` means
   `win-arm64` but `linux-aarch64`. Drifting from it means downloading a jar
   whose resources sherpa then cannot find."
  ([]
   (platform-token (System/getProperty "os.name" "generic")
                   (System/getProperty "os.arch" "generic")))
  ([os-name os-arch]
   (let
     [os
      (str/lower-case (str os-name))

      arch
      (str/lower-case (str os-arch))

      o
      (cond (or (str/includes? os "mac") (str/includes? os "darwin")) "osx"
            (str/includes? os "win") "win"
            (str/includes? os "nux") "linux"
            :else (throw (ex-info "sherpa-onnx has no native library for this operating system"
                                  {:type :voice/unsupported-platform :os os-name})))

      a
      (cond (or (str/starts-with? arch "amd64") (str/starts-with? arch "x86_64")) "x64"
            (str/starts-with? arch "x86") "x86"
            (or (str/starts-with? arch "aarch64") (str/starts-with? arch "arm64"))
            (if (= "win" o) "arm64" "aarch64")
            (str/starts-with? arch "arm") "arm"
            :else (throw (ex-info "sherpa-onnx has no native library for this CPU architecture"
                                  {:type :voice/unsupported-platform :arch os-arch})))]

     (str o "-" a))))

(defn library-names
  "The two files a sherpa native directory holds, in LOAD order: the ONNX
   Runtime first, because sherpa loads it before its own JNI so a system copy
   cannot win. `System/mapLibraryName` is what sherpa itself calls, so these are
   `libonnxruntime.dylib`, `libonnxruntime.so` or `onnxruntime.dll` in step with
   the platform the jar was built for."
  []
  [(System/mapLibraryName "onnxruntime") (System/mapLibraryName "sherpa-onnx-jni")])

(defn default-native-dir
  "~/.vis path for the downloaded pair. A function, never a top-level `def`:
   `native-image` initializes this namespace at BUILD time, so a captured
   `user.home` would point every installed binary at the BUILDER's home."
  ([] (default-native-dir (platform-token)))
  ([token] (str (System/getProperty "user.home") "/.vis/native/sherpa-onnx-" version "/" token)))

(defn native-dir
  []
  (or (some-> (vis/extension-env-value native-dir-env)
              str
              str/trim
              not-empty)
      (some-> (System/getenv native-dir-env)
              str
              str/trim
              not-empty)
      (default-native-dir)))

(defn installed?
  "True when `dir` holds both libraries. Never a partial answer: a directory
   with one of them is as unloadable as an empty one."
  [dir]
  (and (not (str/blank? (str dir))) (every? #(.isFile (io/file (str dir) %)) (library-names))))

(defn embedded?
  "True when the libraries are already on the classpath as resources, which is
   exactly where sherpa's own loader looks second."
  ([] (embedded? (platform-token)))
  ([token] (boolean (io/resource (str "sherpa-onnx/native/" token "/" (second (library-names)))))))

(defn jar-url
  "Where the platform jar comes from, and the ONE place a Vis release does not
   mirror. sherpa's VITS path phonemizes through espeak-ng, which is compiled
   INTO `libsherpa-onnx-jni` — 10 `espeak_*` symbols and its data paths are in
   the shipped library — so the jar is GPL-3 object code. It is fetched by the
   user from the project that published it and is never re-hosted by Vis."
  [token]
  (str "https://jitpack.io/com/github/k2-fsa/sherpa-onnx/sherpa-onnx-native-lib-"
       token
       "/v"
       version
       "/sherpa-onnx-native-lib-"
       token
       "-v"
       version
       ".jar"))

(defn- install!
  "Fetch the platform jar, unpack ONLY its two libraries into a STAGING dir and
   move that into place atomically. `dir` never holds a half-written `.dylib`:
   an interrupted download leaves nothing behind rather than a file that passes
   `.isFile` and then aborts the JVM inside `System/load`. Returns dir."
  [token dir]
  (let
    [^File archive
     (File/createTempFile "vis-voice-sherpa-" ".jar")

     staging
     (io/file (str dir ".staging-" (System/nanoTime)))]

    (try (files/download! (jar-url token) (str archive) nil)
         (.mkdirs staging)
         (with-open [zip (ZipFile. archive)]
           (doseq [lib (library-names)]
             (let
               [entry-name (str "sherpa-onnx/native/" token "/" lib)
                entry (or (.getEntry zip entry-name)
                          (throw (ex-info "sherpa's native jar is missing a library"
                                          {:type :voice/native-incomplete
                                           :entry entry-name
                                           :platform token})))]

               (with-open [in (.getInputStream zip entry)]
                 (io/copy in (io/file staging lib))))))
         (when-not (installed? (str staging))
           (throw (ex-info "sherpa's native download did not produce both libraries"
                           {:type :voice/native-incomplete :platform token :native-dir dir})))
         (let [final (io/file dir)]
           (when (.exists final) (files/delete-dir! final))
           (.mkdirs (.getParentFile final))
           (when-not (.renameTo staging final)
             (throw (ex-info "Could not move sherpa's native libraries into place"
                             {:type :voice/install-failed :native-dir dir}))))
         dir
         (finally (try (.delete archive) (catch Throwable _))
                  (try (when (.exists staging) (files/delete-dir! staging)) (catch Throwable _))))))

(defn- provision!
  [token]
  (let [given (System/getProperty native-path-property)]
    (cond (installed? given) {:source :property :platform token :dir given}
          (embedded? token) {:source :embedded :platform token}
          :else (let [dir (native-dir)]
                  (when-not (contains? published-platforms token)
                    (throw (ex-info "sherpa-onnx publishes no native jar for this platform"
                                    {:type :voice/unsupported-platform
                                     :platform token
                                     :published published-platforms
                                     :property native-path-property})))
                  (when-not (installed? dir)
                    (vis/notify!
                      (str "Downloading sherpa-onnx " version " native libraries (" token ")...")
                      :level :info
                      :ttl-ms 5000)
                    (install! token dir))
                  (System/setProperty native-path-property dir)
                  {:source :downloaded :platform token :dir dir}))))

(defonce ^:private provisioned (atom nil))

(defn ensure-native!
  "Make sherpa's JNI loadable, ONCE, and return how: `{:source :property
   |:embedded|:downloaded :platform <token> :dir <path?>}`. Every entry point
   that touches a `com.k2fsa.sherpa.onnx` class calls this first, because the
   class's static initializer is what runs sherpa's loader — after that first
   touch, a missing library is an `UnsatisfiedLinkError` no property can undo.
   A failure is NOT cached: the next call retries the download."
  []
  (or @provisioned
      (locking provisioned (or @provisioned (reset! provisioned (provision! (platform-token)))))))
