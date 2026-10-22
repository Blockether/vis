(ns com.blockether.vis.internal.python.worker-paths-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.paths :as paths]
            [com.blockether.vis.internal.python.worker :as worker]
            [com.blockether.vis.internal.sandbox.jail :as jail]
            [com.blockether.vis-python-runtime :as runtime]
            [lazytest.core :refer [defdescribe expect it]])
  (:import (java.io File)
           (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)))

(defdescribe worker-diagnostics-use-canonical-date-directory-test
             (it "worker diagnostics use canonical date directory"
                 (let [home
                       (.toFile (Files/createTempDirectory "vis-worker-paths-"
                                                           (make-array FileAttribute 0)))

                       previous-home
                       (System/getProperty "user.home")

                       date-dir
                       (io/file home ".vis" "logs" "2026-09-14")]

                   (try (System/setProperty "user.home" (.getPath home))
                        (with-redefs [paths/log-date-dir (constantly (.getPath date-dir))]
                          (let [^File directory (#'worker/worker-dir "test-worker")]
                            (expect (= (io/file date-dir "pyext-test-worker") directory))
                            (expect (.isDirectory directory))))
                        (finally (System/setProperty "user.home" previous-home)
                                 (doseq [^File file (reverse (file-seq home))]
                                   (Files/deleteIfExists (.toPath file))))))))

(defdescribe
  worker-run-directory-cleanup-test
  (doseq [output? [false true]]
    (it (if output? "keeps a run directory with output" "removes an empty run directory")
        (let [directory (.getCanonicalFile (.toFile (Files/createTempDirectory
                                                      "vis-worker-prune-"
                                                      (make-array FileAttribute 0))))
              key (worker/extension-worker-key (str (java.util.UUID/randomUUID)))]

          (try (with-redefs [paths/log-date-dir (fn [& _]
                                                  (str (io/file directory "2026-10-19")))]
                 (worker/exec! key
                               runtime/default-session
                               (if output? "import os; os.write(2, b'kept-output\\n')" "1"))
                 (let [^File log (:log (get @@#'worker/workers key))
                       run-directory (.getParentFile log)]

                   (when output?
                     (loop [attempt 0]
                       (when (and (< attempt 200) (not (str/includes? (slurp log) "kept-output")))
                         (Thread/sleep 10)
                         (recur (inc attempt)))))
                   (worker/stop-worker! key)
                   (expect (= output? (.isDirectory run-directory)))))
               (finally (worker/stop-worker! key)
                        (doseq [^File file (reverse (file-seq directory))]
                          (Files/deleteIfExists (.toPath file)))))))))

(defdescribe worker-run-directory-exit-cleanup-test
             (it "removes an empty run directory when the worker exits on its own"
                 ;; Regression: only `stop-worker!` pruned, so a worker that died or outlived the
                 ;; 200 ms stop wait left an empty pyext-* directory behind.
                 (let [directory
                       (.getCanonicalFile (.toFile (Files/createTempDirectory
                                                     "vis-worker-exit-"
                                                     (make-array FileAttribute 0))))

                       key
                       (worker/extension-worker-key (str (java.util.UUID/randomUUID)))]

                   (try (with-redefs [paths/log-date-dir (fn [& _]
                                                           (str (io/file directory "2026-10-20")))]
                          (worker/exec! key runtime/default-session "1")
                          (let [^File log (:log (get @@#'worker/workers key))
                                ^Process process (:process (get @@#'worker/workers key))
                                run-directory (.getParentFile log)]

                            (expect (.isDirectory run-directory))
                            (.destroyForcibly process)
                            (.waitFor process)
                            ;; The exit hook runs asynchronously; a loaded CI runner needed more than 2 s.
                            (loop [attempt 0]
                              (when (and (< attempt 1000) (.exists run-directory))
                                (Thread/sleep 10)
                                (recur (inc attempt))))
                            (expect (not (.exists run-directory))
                                    (pr-str (for [^File file (or (.listFiles run-directory) [])]
                                              [(.getName file) (.length file)])))))
                        (finally (worker/stop-worker! key)
                                 (doseq [^File file (reverse (file-seq directory))]
                                   (Files/deleteIfExists (.toPath file))))))))

(defdescribe
  worker-process-output-keeps-start-date-test
  (doseq [jvm?
          [false true]

          trusted?
          [false true]]

    (it
      (str (if jvm? "JVM" "Packaged") " worker, trusted=" trusted?)
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

              (expect (= "worker.log" (.getName log)))
              (expect (= start-directory (.getParentFile run-directory)))
              (expect (str/starts-with? (.getName run-directory) "pyext-"))
              (if trusted?
                (expect (nil? (:policy @launched)))
                (expect (= [(.getAbsolutePath run-directory)]
                           (get-in @launched [:policy :allow-read-write]))))
              (when jvm?
                (expect (some #{(str "-XX:ErrorFile=" (io/file run-directory "jvm-crash-%p.log"))}
                              (:argv @launched)))
                (expect (some #{(str "-XX:HeapDumpPath=" (io/file run-directory "jvm-heap.hprof"))}
                              (:argv @launched))))
              (reset! date "2026-09-15")
              (worker/exec! key
                            runtime/default-session
                            "os.write(2, b'worker-log-after-midnight\\n')")
              (expect (= log (:log (get @@#'worker/workers key))))
              (expect (not (.exists (io/file directory "2026-09-15"))))
              (expect (loop [attempt 0]
                        (if (and (.isFile log)
                                 (str/includes? (slurp log) "worker-log-after-midnight"))
                          true
                          (when (< attempt 200) (Thread/sleep 10) (recur (inc attempt))))))
              (expect (str/includes? (slurp log) "worker-log-before-midnight"))))
          (finally (worker/stop-worker! key)
                   (worker/forget-policy! key)
                   (doseq [^File file (reverse (file-seq directory))]
                     (Files/deleteIfExists (.toPath file)))))))))
