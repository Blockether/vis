(ns com.blockether.vis.internal.python.worker-paths-test
  (:require [clojure.java.io :as io]
            [com.blockether.vis.internal.paths :as paths]
            [com.blockether.vis.internal.python.worker :as worker]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is]])
  (:import (java.io File)
           (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)))

(deftest worker-diagnostics-use-canonical-date-directory-test
  (let [home
        (.toFile (Files/createTempDirectory "vis-worker-paths-" (make-array FileAttribute 0)))

        previous-home
        (System/getProperty "user.home")

        date-dir
        (io/file home ".vis" "logs" "2026-09-14")]

    (try (System/setProperty "user.home" (.getPath home))
         (with-redefs [paths/log-date-dir (constantly (.getPath date-dir))]
           (let [^File directory (#'worker/worker-dir "test-worker")]
             (is (= (io/file date-dir "pyext-test-worker") directory))
             (is (.isDirectory directory))))
         (finally (System/setProperty "user.home" previous-home)
                  (doseq [^File file (reverse (file-seq home))]
                    (Files/deleteIfExists (.toPath file)))))))
