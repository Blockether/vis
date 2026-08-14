(ns com.blockether.vis.ext.foundation-voice.sherpa-test
  (:require [com.blockether.vis.ext.foundation-voice.sherpa :as sherpa]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [com.k2fsa.sherpa.onnx VersionInfo]))

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
