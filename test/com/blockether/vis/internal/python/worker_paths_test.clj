(ns com.blockether.vis.internal.python.worker-paths-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.paths :as paths]
            [com.blockether.vis.internal.python.worker :as worker]
            [com.blockether.vis.internal.sandbox.jail :as jail]
            [com.blockether.vis-python-runtime :as runtime]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is testing]])
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

(deftest worker-process-output-keeps-start-date-test
  (doseq [jvm?
          [false true]

          trusted?
          [false true]]

    (testing (str (if jvm? "JVM" "Packaged") " worker, trusted=" trusted?)
      (let [directory
            (.getCanonicalFile (.toFile (Files/createTempDirectory "vis-worker-logs-"
                                                                   (make-array FileAttribute 0))))

            date
            (atom "2026-09-14")

            key
            (if trusted?
              (worker/extension-worker-key (str (java.util.UUID/randomUUID)))
              (str (java.util.UUID/randomUUID)))

            resolve-worker
            runtime/resolve-worker

            spawn
            jail/spawn!

            launched
            (atom nil)]

        (try
          (with-redefs [paths/log-date-dir
                        (fn [& _]
                          (str (io/file directory @date)))

                        runtime/resolve-worker
                        (if jvm? (constantly nil) resolve-worker)

                        jail/spawn!
                        (fn [argv cwd policy options]
                          (reset! launched {:argv argv :policy policy})
                          (spawn argv cwd policy options))]

            (when-not trusted?
              (worker/configure! key
                                 (fn []
                                   {:roots-fn (constantly [(System/getProperty "user.dir")])
                                    :net-enabled? false})))
            (worker/exec! key
                          runtime/default-session
                          "import os; os.write(2, b'worker-log-before-midnight\\n')")
            (let [state
                  (get @@#'worker/workers key)

                  ^File log
                  (:log state)

                  run-directory
                  (.getParentFile log)

                  start-directory
                  (io/file directory "2026-09-14")]

              (is (= "worker.log" (.getName log)))
              (is (= start-directory (.getParentFile run-directory)))
              (is (str/starts-with? (.getName run-directory) "pyext-"))
              (if trusted?
                (is (nil? (:policy @launched)))
                (is (= [(.getAbsolutePath run-directory)]
                       (get-in @launched [:policy :allow-read-write]))))
              (when jvm?
                (is (some #{(str "-XX:ErrorFile=" (io/file run-directory "jvm-crash-%p.log"))}
                          (:argv @launched)))
                (is (some #{(str "-XX:HeapDumpPath=" (io/file run-directory "jvm-heap.hprof"))}
                          (:argv @launched))))
              (reset! date "2026-09-15")
              (worker/exec! key
                            runtime/default-session
                            "os.write(2, b'worker-log-after-midnight\\n')")
              (is (= log (:log (get @@#'worker/workers key))))
              (is (not (.exists (io/file directory "2026-09-15"))))
              (is (loop [attempt 0]
                    (if (and (.isFile log) (str/includes? (slurp log) "worker-log-after-midnight"))
                      true
                      (when (< attempt 200) (Thread/sleep 10) (recur (inc attempt))))))
              (is (str/includes? (slurp log) "worker-log-before-midnight"))))
          (finally (worker/stop-worker! key)
                   (worker/forget-policy! key)
                   (doseq [^File file (reverse (file-seq directory))]
                     (Files/deleteIfExists (.toPath file)))))))))
