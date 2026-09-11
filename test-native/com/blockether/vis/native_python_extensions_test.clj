(ns com.blockether.vis.native-python-extensions-test
  "Python extension registration during isolated native gateway startup."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.native-binary-test :as native]
            [com.blockether.vis.native-speech-startup-test :as startup]
            [com.blockether.vis.internal.util :as util]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [java.io File]
           [java.net ServerSocket]
           [java.util.concurrent TimeUnit]))

(defdescribe
  native-runtime-source-selection-test
  ;; #194: the prebuilt worker must not override the host's pinned guest sources.
  (it
    "loads the package helper from the pinned runtime revision"
    (let [home
          (#'native/temp-dir "vis-native-runtime-source-")

          binary
          (#'native/require-binary)

          expected
          (util/sha256-hex (slurp (io/resource "vis-python/package_paths.py")))]

      (try
        (let
          [[^Process child log]
           (#'startup/start-native!
            home
            binary
            "source"
            ["python" "--no-env" "--no-network" "-c"
             (str
               "import hashlib, package_paths, re\nfrom pathlib import Path\n"
               "assert VIS_VERSION == VIS_PYTHON_SDK_VERSION != 'dev'\n"
               "assert re.fullmatch(r'[0-9a-f]{40}(-dirty)?', VIS_SHA_RELEASE)\n"
               "assert re.fullmatch(r'\\d+\\.\\d+\\.\\d+', VIS_PYTHON_RUNTIME_VERSION)\n"
               "print(hashlib.sha256(Path(package_paths.__file__).read_bytes()).hexdigest())\n")])]
          (try (expect (.waitFor child 60 TimeUnit/SECONDS) "native source probe timed out")
               (let [output (slurp log)]
                 (expect (= 0 (.exitValue child)) output)
                 (expect (str/includes? output expected) output))
               (finally (when (.isAlive child) (#'native/kill-tree! child)))))
        (finally (#'native/delete-tree! home))))))

(defdescribe
  native-editable-sdk-startup-test
  ;; #194: both the injected SDK and its host-bound session must survive refresh.
  (it
    "registers extensions from an editable cwd without installing another SDK"
    (let [home
          (#'native/temp-dir "vis-native-editable-sdk-")

          binary
          (#'native/require-binary)

          entries
          (doto (io/file home ".vis/extensions") .mkdirs)

          packages
          (doto (io/file home ".vis/python/packages") .mkdirs)

          names
          ["editable-sdk-first" "editable-sdk-second"]

          markers
          (mapv #(io/file entries (str % ".loaded")) names)]

      (try (spit (io/file home "vis.yml") "{}\n")
           (spit (io/file packages "cwd.pth") (str (.getCanonicalPath home) "\n"))
           (doseq [name names]
             (spit
               (io/file entries (str name ".py"))
               (str
                 "import hashlib, package_paths\nfrom pathlib import Path\n"
                 "assert hashlib.sha256(Path(package_paths.__file__).read_bytes()).hexdigest() == "
                 (pr-str (util/sha256-hex (slurp (io/resource "vis-python/package_paths.py"))))
                 ", package_paths.__file__\n"
                 "import blockether.vis.extension as vis\n"
                 "assert VIS_VERSION == VIS_PYTHON_SDK_VERSION != 'dev'\n"
                 "assert len(VIS_SHA_RELEASE) >= 40\n"
                 "assert VIS_PYTHON_RUNTIME_VERSION != 'dev'\n"
                 "vis.register(vis.Extension(name=" (pr-str name)
                 ", description='Native editable SDK fixture'))\n"
                 "Path(__file__).with_suffix('.loaded').write_text('registered')\n")))
           (let [port
                 (with-open [socket (ServerSocket. 0)]
                   (.getLocalPort socket))

                 [^Process gateway gateway-log]
                 (#'startup/start-native!
                  home
                  binary
                  "gateway"
                  ["gateway" "start" "--host" "127.0.0.1" "--port" (str port) "--require-token"])]

             (try (let [registered?
                        (loop [remaining 300]
                          (cond (not (.isAlive gateway)) false
                                (every? #(.isFile ^File %) markers) true
                                (zero? remaining) false
                                :else (do (Thread/sleep 50) (recur (dec remaining)))))

                        errors
                        (->> (cons gateway-log
                                   (filter #(and (.isFile ^File %)
                                                 (str/ends-with? (.getName ^File %) ".log"))
                                           (file-seq (io/file home ".vis/logs"))))
                             (mapcat #(str/split-lines (slurp %)))
                             (filter #(re-find #"(?i)error|exception|failed" %))
                             (take-last 20)
                             (str/join "\n"))]

                    (expect registered? (str "native SDK registration did not complete\n" errors))
                    (expect (#'startup/listening? port))
                    (doseq [marker markers]
                      (expect (= "registered" (slurp marker)))))
                  (finally (when (.isAlive gateway) (#'native/kill-tree! gateway)))))
           (finally (#'native/delete-tree! home))))))
