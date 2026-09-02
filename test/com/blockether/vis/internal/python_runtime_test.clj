(ns com.blockether.vis.internal.python-runtime-test
  "Getting the interpreter onto a machine that has none.

   The download itself is not exercised here — a suite that fetched 25 MB to
   prove HTTP works would be testing GitHub. What IS exercised is everything
   around it: the asset a version and platform name, the unpack that has to keep
   symlinks and execute bits, and the promise that a runtime already resolvable
   is never touched."
  (:require [clojure.java.io :as io]
            [com.blockether.vis-python-runtime :as runtime]
            [com.blockether.vis.internal.python-runtime :as python-runtime]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is testing]])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn- temp-dir
  ^java.io.File [prefix]
  (.toFile (Files/createTempDirectory prefix (make-array FileAttribute 0))))

(defn- sample-archive
  "A tar.gz shaped like a platform release: a library, an executable under
   `bin/`, and a symlink — the three things a jar could not have carried."
  ^java.io.File []
  (let [source
        (temp-dir "vis-python-archive-src")

        out
        (io/file (temp-dir "vis-python-archive") "runtime.tar.gz")]

    (spit (io/file source "libvispython.dylib") "cdylib")
    (.mkdirs (io/file source "python" "bin"))
    (spit (io/file source "python" "bin" "python3") "#!/bin/sh\n")
    (.setExecutable (io/file source "python" "bin" "python3") true false)
    (Files/createSymbolicLink (.toPath (io/file source "python" "bin" "python"))
                              (.toPath (io/file "python3"))
                              (make-array FileAttribute 0))
    (let [^java.util.List command ["tar" "czf" (.getAbsolutePath out)
                                   "-C" (.getAbsolutePath source) "."]
          process (.start (ProcessBuilder. command))]
      (.waitFor process))
    out))

(deftest archive-url-names-the-release-asset-test
  (testing "the asset a version and platform resolve to, on the runtime's own release"
    (is (= (str "https://github.com/Blockether/vis-python-runtime/releases/download"
                "/v0.1.0/vis-python-runtime-darwin-arm64-0.1.0.tar.gz")
           (python-runtime/archive-url "0.1.0" "darwin-arm64")))))

(deftest install-archive-keeps-what-an-interpreter-needs-test
  (testing "modes and symlinks survive, because `tar` unpacks and a jar never could"
    (let [home (io/file (temp-dir "vis-python-home") "0.1.0")]
      (#'python-runtime/install-archive! (sample-archive) home)
      (is (.isFile (io/file home "libvispython.dylib")))
      (is (.canExecute (io/file home "python" "bin" "python3"))
          "pip runs the interpreter as a program")
      (is (Files/isSymbolicLink (.toPath (io/file home "python" "bin" "python")))
          "the vendored tree links its own names")))
  (testing "the staging directory is gone, so an installation is whole or absent"
    (let [home (io/file (temp-dir "vis-python-home") "0.1.0")]
      (#'python-runtime/install-archive! (sample-archive) home)
      (is (empty? (filter #(re-find #"\.tmp\." (.getName ^java.io.File %))
                          (.listFiles (.getParentFile home))))))))

(deftest ensure-library-answers-a-real-runtime-test
  (testing
    "the interpreter this machine runs — already resolvable, or fetched once — and the same one on the next call"
    (let [library (python-runtime/ensure-library!)]
      (is (.isFile (io/file library)))
      (is (= library (python-runtime/ensure-library!)))
      (is (= library (:path (runtime/resolve-library)))))))
