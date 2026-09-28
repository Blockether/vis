(ns com.blockether.vis.internal.speech.sherpa
  "sherpa-onnx JNI for speech, sharing Java decisions' ONNX Runtime on the
   macOS ARM64 and Linux release platforms. The upstream per-platform jar still
   bundles an older runtime; Vis extracts its JNI only and stages it beside
   ONNX Runtime Java 1.30.0. Unsupported platforms retain upstream's pair.

   On a plain JVM the platform jar is downloaded on first speech use; native
   images embed only the host JNI. Both loaders use one versioned directory.
   A user-supplied `sherpa_onnx.native.path` remains authoritative.

   The JitPack jar is length-checked and installed atomically; it is fetched
   from the original publisher, not mirrored by Vis. Its JNI answers the
   pinned `version`, as `sherpa-test` asserts.

   ;; JNI and not `java.lang.foreign`, because the choice is upstream's: the library
   ;; Vis ships, `libsherpa-onnx-jni`, exports 133 `Java_*` entry points and not one
   ;; `SherpaOnnx*` C symbol, so a Panama downcall has nothing to bind to. sherpa's C
   ;; API is a separate artifact — per-platform tarballs under no Maven coordinate —
   ;; whose 156 functions over 86 structs would leave us owning their layouts. Vis
   ;; does use FFM where it owns the boundary (`internal/foundation/pty`); here the
   ;; image registers the API jar's types for JNI instead
   ;; (`reachability-metadata.json`, pinned by `sherpa-test`)."
  (:require [clojure.java.io :as io]
            [com.blockether.vis.internal.channel.notifications :as notifications]
            [com.blockether.vis.internal.config.core :as config]
            [com.blockether.vis.internal.extension.capability :as capability]
            [com.blockether.vis.internal.inference.runtime :as runtime]
            [com.blockether.vis.internal.speech.files :as files]
            [com.blockether.vis.internal.util :as util])
  (:import [java.io ByteArrayInputStream File InputStream]
           [java.nio.charset StandardCharsets]
           [java.security MessageDigest]
           [java.util Arrays HexFormat]
           [java.util.zip ZipFile]))

;; Reflective interop is FATAL in the native image (needs metadata per call
;; site) — keep this ns reflection-free at compile time.
(set! *warn-on-reflection* true)

(def version
  "The Sherpa JNI and Java API release; the shared runtime has its own pin."
  runtime/sherpa-version)

(def native-dir-env runtime/native-dir-env)

(def native-path-property
  "Sherpa's override: a directory containing its JNI and ONNX Runtime."
  runtime/sherpa-native-path-property)

(def published-platforms
  "Platforms with an upstream Sherpa native jar. Other OS/CPU pairs require
   a user-provided native build."
  #{"osx-aarch64" "osx-x64" "linux-x64" "linux-aarch64" "win-x64"})

(defn platform-token
  "Sherpa's `native/<token>` directory name; both Java loaders use this token."
  ([] (runtime/platform-token))
  ([os-name os-arch] (runtime/platform-token os-name os-arch)))

(defn library-names
  "Sherpa's runtime and JNI filenames, in its native loading order."
  []
  (let [[ort _ sherpa] (runtime/library-names)]
    [ort sherpa]))

(defn default-native-dir
  "Versioned default path, computed at runtime rather than captured in the image."
  ([] (default-native-dir (platform-token)))
  ([token]
   (if (contains? runtime/shared-platforms token)
     (runtime/default-native-dir token)
     (str (System/getProperty "user.home") "/.vis/native/sherpa-onnx-" version "/" token))))

(defn native-dir [] (or (config/extension-env-value native-dir-env) (default-native-dir)))

(defn installed?
  "True when both of Sherpa's required native libraries exist."
  [dir]
  (every? #(runtime/installed? dir %) (library-names)))

(defn embedded?
  "True when this platform's Sherpa JNI is a classpath resource."
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

(def ^:private linux-ort-symbol-patches
  ;; The v1.13.8 JNI imports only OrtGetApiBase@VERS_1.28.2 from libonnxruntime.so.
  ;; ORT 1.30 retains the older C API, but its ELF export is named VERS_1.30.0.
  ;; Pin the exact upstream binaries before changing that version reference locally;
  ;; never mirror sherpa's GPL JNI or rewrite an unrecognized binary.
  {"linux-x64" {:sha256 "adcabd1866f667ec78796a504ff96030eff30fbd80792e892752c64a861bf231"
                :version-offset 33151
                :hash-offset 34656}
   "linux-aarch64" {:sha256 "a2b107bb7125bc8518731655781bfcddb54a5a4731eaeeb177274b08fe79aebc"
                    :version-offset 33255
                    :hash-offset 34696}})

(defn- replace-verified-bytes!
  [^bytes data offset ^bytes before ^bytes after]
  (let [offset
        (long offset)

        end
        (+ offset (alength before))]

    (when-not (and (= (alength before) (alength after))
                   (<= 0 offset)
                   (<= end (alength data))
                   (Arrays/equals before (Arrays/copyOfRange data (int offset) (int end))))
      (throw (ex-info "Sherpa's ONNX symbol version did not match its pinned binary"
                      {:type :speech/native-incompatible :offset offset})))
    (System/arraycopy after 0 data (int offset) (alength before)))
  data)

(defn- compatible-linux-jni!
  [token ^bytes data {:keys [sha256 version-offset hash-offset]}]
  (when-not (MessageDigest/isEqual (.parseHex (HexFormat/of) ^String sha256) (util/sha256 data))
    (throw (ex-info "Sherpa's Linux JNI changed; cannot safely share ONNX Runtime 1.30.0"
                    {:type :speech/native-incompatible :platform token})))
  (replace-verified-bytes! data
                           version-offset
                           (.getBytes "VERS_1.28.2" StandardCharsets/US_ASCII)
                           (.getBytes "VERS_1.30.0" StandardCharsets/US_ASCII))
  (replace-verified-bytes! data
                           hash-offset
                           (byte-array (map unchecked-byte [0x82 0xfe 0x7b 0x02]))
                           (byte-array (map unchecked-byte [0x80 0xc6 0x7b 0x02])))
  data)

(defn- shared-jni-stream
  [token ^InputStream stream]
  (if-let [patch (get linux-ort-symbol-patches token)]
    (with-open [in stream]
      (ByteArrayInputStream. (compatible-linux-jni! token (.readAllBytes in) patch)))
    stream))

(defn- install-embedded-jni!
  [token dir]
  (let [name
        (second (library-names))

        resource
        (str "sherpa-onnx/native/" token "/" name)

        url
        (or (io/resource resource)
            (throw (ex-info "Sherpa's embedded JNI resource is missing"
                            {:type :speech/native-incomplete :resource resource})))]

    (runtime/install-stream! dir name (shared-jni-stream token (io/input-stream url)))))

(defn- install!
  "Download the platform jar; on shared platforms extract only Sherpa's JNI."
  [token dir shared?]
  (let [^File archive
        (File/createTempFile "vis-speech-sherpa-" ".jar")

        ^File staging
        (when-not shared? (io/file (str dir ".staging-" (System/nanoTime))))]

    (try (files/download! (jar-url token) (str archive) nil)
         (when staging (.mkdirs staging))
         (with-open [zip (ZipFile. archive)]
           (doseq [lib (if shared? [(second (library-names))] (library-names))]
             (let [entry-name (str "sherpa-onnx/native/" token "/" lib)
                   entry (or (.getEntry zip entry-name)
                             (throw (ex-info "sherpa's native jar is missing a library"
                                             {:type :speech/native-incomplete
                                              :entry entry-name
                                              :platform token})))]

               (if shared?
                 (runtime/install-stream! dir
                                          lib
                                          (shared-jni-stream token (.getInputStream zip entry)))
                 (with-open [in (.getInputStream zip entry)]
                   (io/copy in (io/file staging lib)))))))
         (when-not (installed? (if shared? dir (str staging)))
           (throw (ex-info "sherpa's native download did not produce its libraries"
                           {:type :speech/native-incomplete :platform token :native-dir dir})))
         (when staging
           (let [final (io/file dir)]
             (when (.exists final) (files/delete-dir! final))
             (.mkdirs (.getParentFile final))
             (when-not (.renameTo staging final)
               (throw (ex-info "Could not move sherpa's native libraries into place"
                               {:type :speech/install-failed :native-dir dir})))))
         dir
         (finally (try (.delete archive) (catch Throwable _))
                  (try (when (and staging (.exists staging)) (files/delete-dir! staging))
                       (catch Throwable _))))))

(defn- provision!
  [token]
  (let [given
        (System/getProperty native-path-property)

        shared?
        (contains? runtime/shared-platforms token)]

    (cond (installed? given) {:source :property :platform token :dir given}
          shared? (let [dir
                        (:dir (runtime/ensure-ort!))

                        embedded
                        (embedded? token)]

                    (when-not (installed? dir)
                      (if embedded
                        (install-embedded-jni! token dir)
                        (do (notifications/notify!
                              (str "Downloading sherpa-onnx " version " JNI library (" token ")...")
                              :level :info
                              :ttl-ms 5000)
                            (install! token dir true))))
                    (System/setProperty native-path-property dir)
                    {:source (if embedded :embedded :downloaded) :platform token :dir dir})
          (embedded? token) {:source :embedded :platform token}
          :else (let [dir (native-dir)]
                  (when-not (contains? published-platforms token)
                    (throw (ex-info "sherpa-onnx publishes no native jar for this platform"
                                    {:type :speech/unsupported-platform
                                     :platform token
                                     :published published-platforms
                                     :property native-path-property})))
                  (when-not (installed? dir)
                    (notifications/notify!
                      (str "Downloading sherpa-onnx " version " native libraries (" token ")...")
                      :level :info
                      :ttl-ms 5000)
                    (install! token dir false))
                  (System/setProperty native-path-property dir)
                  {:source :downloaded :platform token :dir dir}))))

(defn ensure-native!
  "Make sherpa's JNI loadable, ONCE, and return how: `{:source :property
   |:embedded|:downloaded :platform <token> :dir <path?>}`. Every entry point
   that touches a `com.k2fsa.sherpa.onnx` class calls this first, because the
   class's static initializer is what runs sherpa's loader — after that first
   touch, a missing library is an `UnsatisfiedLinkError` no property can undo.

   Whether an answer is worth keeping is the HOST's rule, not this pack's: a
   download that failed is retried on the next call, while a library this JVM has
   already refused to link is answered from memory instead of fetched again."
  []
  (let [{:keys [status detail cause]} (capability/ensure! ::native #(provision! (platform-token)))]
    (if (= :ready status) detail (throw cause))))

(defn native-failure
  "Turn a linker failure into an ex-info a HUMAN can act on.

   A class whose static initializer already failed can NEVER load again in the
   same process - the JVM caches that verdict - so an engine that met a missing
   library once keeps answering `NoClassDefFoundError` however much is downloaded
   afterwards. That is exactly the reported \"voice only works after restarting
   Vis\", so in that state the message SAYS to restart instead of repeating a
   linker error nobody can act on."
  [^Throwable t]
  (let [restart? (capability/terminal-error? t)]
    (ex-info (if restart?
               (str "The speech runtime could not be linked into this running process"
                    " - restart Vis and try again ("
                    (or (ex-message t) (str t))
                    ").")
               (str "The speech runtime is unavailable: " (or (ex-message t) (str t))))
             {:type :speech/native-unavailable
              :is-restart-required restart?
              :platform (platform-token)
              :remediation (if restart?
                             "Restart Vis - this JVM can no longer load the library."
                             (str "Check the network, or point "
                                  native-dir-env
                                  " at a directory holding sherpa-onnx-jni and onnxruntime."))}
             t)))

(defn call-native
  "Run `f` with the native runtime provisioned, reporting a linker failure -
   here or inside sherpa's own loader - as [[native-failure]] rather than as a
   stack trace. Every engine entry point goes through this, so no surface has to
   know what a JNI is to tell a human what to do.

   A linker failure met HERE is also handed to the host, because this is where it
   is normally met: sherpa loads its library from the static initializer of the
   first class a call touches, long after provisioning answered. Recording it is
   what stops every later call from fetching 13 MB to meet the same wall."
  [f]
  (try (ensure-native!) (catch Throwable t (throw (native-failure t))))
  (try (f)
       (catch Throwable t
         (capability/fail! ::native t)
         (throw (if (capability/terminal-error? t) (native-failure t) t)))))
