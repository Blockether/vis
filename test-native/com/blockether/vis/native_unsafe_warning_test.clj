(ns com.blockether.vis.native-unsafe-warning-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.native-binary-test :as native]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [java.io File]
           [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defdescribe
  native-launcher-unsafe-warning-test
  (it "completes an agent turn without Unsafe deprecation warnings"
      (let [^File dir
            (#'native/temp-dir "vis-native-unsafe-")

            ^File bin
            (#'native/require-binary)

            launcher
            (io/file dir "vis-agent")

            {:keys [server port asked]}
            (#'native/start-stub-provider! "UNSAFE_CHECK_COMPLETE")]

        (try (io/copy (io/file "bin/vis-agent") launcher)
             (Files/createSymbolicLink (.toPath (io/file dir "vis-agent-native"))
                                       (.toPath (.getAbsoluteFile bin))
                                       (make-array FileAttribute 0))
             (#'native/overlay! dir port)
             (let [{:keys [exit output]} (#'native/run-binary
                                          dir
                                          ["env" "-i" (str "PATH=" (System/getenv "PATH"))
                                           (str "HOME=" (.getAbsolutePath dir)) "bash"
                                           (.getAbsolutePath launcher) "--db" ":memory" "--raw"
                                           "Reply with UNSAFE_CHECK_COMPLETE"]
                                          60)]
               (expect (= 0 exit) output)
               (expect (seq @asked) output)
               (expect (str/includes? output "UNSAFE_CHECK_COMPLETE") output)
               ;; Airlift Zstd compresses persisted session data through deprecated Unsafe.
               (expect (not (str/includes? output "sun.misc.Unsafe")) output))
             (finally (.stop ^com.sun.net.httpserver.HttpServer server 0)
                      (#'native/delete-tree! dir))))))
