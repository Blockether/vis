(ns com.blockether.vis.internal.config.logging-test
  "Real file logging in a separate JVM: initialization changes process-wide streams."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.config.core :as config]
            [lazytest.core :refer [defdescribe expect it]]
            [taoensso.telemere :as tel]
            [taoensso.trove :as trove])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]
           [java.util.concurrent TimeUnit]))

(defn -main
  [mode directory]
  (let [path
        (str directory "/diagnostic.log")

        console
        System/out

        boot?
        (str/starts-with? mode "boot")

        debug?
        (str/ends-with? mode "debug")

        markers
        (cond-> ["telemere-info-marker" "telemere-error-marker" "trove-info-marker"]
          (not boot?)
          (into ["java-out-marker" "java-err-marker" "clojure-out-marker" "clojure-err-marker"])

          debug?
          (conj "telemere-debug-marker" "trove-debug-marker"))]

    (with-redefs [config/config-dir
                  (constantly directory)

                  config/log-path
                  (constantly path)]

      (when (or boot? debug?)
        (require 'com.blockether.vis.internal.main)
        ((ns-resolve 'com.blockether.vis.internal.main 'configure-logging!)
          (if debug? ["--debug"] [])))
      (when-not boot? ((if (str/starts-with? mode "cli") config/init-cli! config/init!)))
      ;; A fresh thread observes root bindings, like native gen-class entrypoints.
      (let [thread (Thread. ^Runnable
                            (fn []
                              (.println System/out "java-out-marker")
                              (.println System/err "java-err-marker")
                              (println "clojure-out-marker")
                              (binding [*out* *err*]
                                (println "clojure-err-marker"))
                              (tel/log! :info "telemere-info-marker")
                              (tel/log! :error "telemere-error-marker")
                              (tel/log! :debug "telemere-debug-marker")
                              (trove/log! {:level :debug :msg "trove-debug-marker"})
                              (trove/log! {:level :info :msg "trove-info-marker"})))]
        (.start thread)
        (.join thread 5000))
      ;; Observe the live file, without shutdown or manually flushing writers.
      (let [missing (loop [remaining 100]
                      (let [text (if (.isFile (io/file path)) (slurp path) "")
                            missing (remove #(str/includes? text %) markers)]

                        (if (and (seq missing) (pos? remaining))
                          (do (Thread/sleep 20) (recur (dec remaining)))
                          (vec missing))))]
        (.println console (pr-str {:missing missing}))
        (config/shutdown!)
        (System/exit (if (empty? missing) 0 1))))))

(defdescribe
  live-diagnostic-logs-test
  (it "writes Java, Clojure, Telemere and Trove diagnostics before shutdown"
      (doseq [mode ["boot" "boot-debug" "cli" "cli-debug" "tui" "tui-debug"]]
        (let [directory (.toFile (Files/createTempDirectory "vis-live-logs-"
                                                            (make-array FileAttribute 0)))
              output (io/file directory "child-output")
              process (-> (ProcessBuilder. ^java.util.List
                                           [(str (System/getProperty "java.home") "/bin/java") "-cp"
                                            (System/getProperty "java.class.path") "clojure.main"
                                            "-m" "com.blockether.vis.internal.config.logging-test"
                                            mode (.getAbsolutePath directory)])
                          (.redirectErrorStream true)
                          (.redirectOutput output)
                          (.start))]

          (try (expect (.waitFor process 40 TimeUnit/SECONDS) (str mode " logging child timed out"))
               (expect (zero? (.exitValue process)) (str mode ": " (slurp output)))
               (finally (.destroyForcibly process)
                        (.waitFor process 5 TimeUnit/SECONDS)
                        (doseq [file (reverse (file-seq directory))]
                          (io/delete-file file true))))))))
