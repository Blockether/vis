(ns com.blockether.vis.internal.gateway.startup-test
  "Fresh-JVM gateway startup regression and benchmark. Each child has its own home,
   configuration, database and loopback listener; it never attaches to a user daemon.
   Set -Dvis.startup.runs=3 for repeated measurements, and -Dvis.startup.jfr=/path/startup
   to record each child from JVM launch (files get a run-number suffix)."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is]])
  (:import (java.io File)
           (java.nio.file Files FileVisitOption Path)
           (java.nio.file.attribute FileAttribute)
           (java.util.concurrent TimeUnit)))

(def ^:private startup-form
  '(let
    [output *out* timings (atom []) result (atom {}) timed
     (fn
      [label f]
      (let
       [start (System/nanoTime)]
       (try
        (f)
        (finally (swap! timings conj [label (/ (double (- (System/nanoTime) start)) 1e6)])))))]
    (timed :load-core #(require 'com.blockether.vis.core))
    (let
     [initializer (ns-resolve 'com.blockether.vis.internal.extension.manifest 'run-initializer!)
      original-initializer @initializer warm-db
      (requiring-resolve 'com.blockether.vis.internal.gateway.state/warm-db!) original-warm-db
      @warm-db build-app (ns-resolve 'com.blockether.vis.internal.gateway.server 'rebuild-app!)
      original-build-app @build-app serve
      (requiring-resolve 'com.blockether.vis.internal.gateway.server/serve-main!)]
     (with-redefs-fn
      {build-app #(timed :http-app original-build-app)
       initializer (fn [sym] (timed [:initializer sym] #(original-initializer sym)))
       serve
       (fn
        [opts]
        ;; Exercise the real CLI initialization and server start, replacing only
        ;; the foreground banner/signal handlers and indefinite wait with a probe.
        (let
         [port (with-open [socket (java.net.ServerSocket. 0)] (.getLocalPort socket))]
         (try
          (timed
           :server-start
           #((requiring-resolve 'com.blockether.vis.internal.gateway.server/start!)
             (assoc opts :port port)))
          (reset!
           result
           {:extensions (mapv
                         :ext/name
                         ((requiring-resolve
                           'com.blockether.vis.internal.extension.core/registered-extensions)))
            :home (System/getProperty "user.home")
            :loaded-namespaces (set (map ns-name (all-ns)))
            :ready-ms (.getUptime (java.lang.management.ManagementFactory/getRuntimeMXBean))
            :sqlite-tmpdir (System/getProperty "org.sqlite.tmpdir")})
          (swap!
           result
           assoc
           :health-status
           (:status
            ((requiring-resolve 'com.blockether.vis.internal.gateway.client/request!)
             :get
             "/healthz")))
          (let
           [format-source
            (requiring-resolve 'com.blockether.vis.internal.language.clojure.format/format-source)
            source "(defn f [x]\n(+ x 1))" home (System/getProperty "user.home")]
           (swap!
            result
            assoc
            :default-format
            (timed :first-default-format #(format-source source nil)))
           (spit (str home "/.zprint.edn") "{:width 80}")
           (swap!
            result
            assoc
            :configured-format
            (timed :first-zprint-format #(format-source source (str home "/example.clj")))))
          (let
           [extensions
            ((requiring-resolve 'com.blockether.vis.internal.extension.core/registered-extensions))
            test-fn
            (->>
             extensions
             (mapcat :ext/language-tools)
             (filter #(= "clojure" (:language %)))
             first
             :test-fn)]
           (swap!
            result
            assoc
            :first-test-error
            (timed
             :first-test-handler
             #(try
               (test-fn
                {:workspace/root (System/getProperty "user.home")}
                {"path" "missing_test.clj"})
               nil
               (catch clojure.lang.ExceptionInfo e (.getMessage e))))))
          (binding
           [*out* output]
           (println "VIS_STARTUP" (pr-str (assoc @result :timings @timings)))
           (flush))
          (finally ((requiring-resolve 'com.blockether.vis.internal.gateway.server/stop!))))))
       warm-db #(timed :database original-warm-db)}
      #((requiring-resolve 'com.blockether.vis.core/-main)
        "gateway"
        "start"
        "--host"
        "127.0.0.1")))))

(defn- cold-start!
  [run-number]
  (let [home
        (.toFile (Files/createTempDirectory "vis-gateway-startup-" (make-array FileAttribute 0)))

        log
        (io/file home "child.log")

        classpath
        (str/join File/pathSeparator
                  (map #(.getCanonicalPath (io/file %))
                       (str/split (System/getProperty "java.class.path")
                                  (re-pattern File/pathSeparator))))

        jfr
        (some-> (System/getProperty "vis.startup.jfr")
                (str "-" run-number ".jfr")
                io/file
                .getAbsolutePath)

        jvm-opts
        (get-in (edn/read-string (slurp "deps.edn")) [:aliases :vis :jvm-opts])

        command
        (into [(str (System/getProperty "java.home") "/bin/java")]
              (concat jvm-opts
                      [(str "-Duser.home=" home)]
                      (when jfr
                        ["-XX:FlightRecorderOptions=stackdepth=256"
                         (str "-XX:StartFlightRecording=settings=profile,dumponexit=true,filename="
                              jfr)])
                      ["-cp" classpath "clojure.main" "-e" (pr-str startup-form)]))

        builder
        (doto (ProcessBuilder. ^java.util.List command)
          (.directory home)
          (.redirectErrorStream true)
          (.redirectOutput log))]

    (when jfr (io/make-parents jfr))
    ;; No inherited gateway override, DB path, extensions, provider credentials or
    ;; profiling flags. Keep only the platform process-launch environment.
    (let [env
          (.environment builder)

          kept
          (select-keys (into {} env) ["PATH" "JAVA_HOME" "SystemRoot" "TMPDIR" "TMP" "TEMP"])]

      (.clear env)
      (.putAll env kept)
      (.put env "HOME" (.getPath home)))
    (try
      ;; Reuse only artifact bytes, never the user's Maven settings or Vis home.
      (let [repo (io/file home ".m2/repository")]
        (io/make-parents repo)
        (Files/createSymbolicLink (.toPath repo)
                                  (.toPath (io/file (System/getProperty "user.home")
                                                    ".m2/repository"))
                                  (make-array FileAttribute 0)))
      (let [process (.start builder)]
        (try (when-not (.waitFor process 120 TimeUnit/SECONDS)
               (throw (ex-info "isolated gateway startup timed out" {})))
             (let [output (slurp log)
                   line (some #(when (str/starts-with? % "VIS_STARTUP ")
                                 (subs % (count "VIS_STARTUP ")))
                              (str/split-lines output))]

               (when-not (and (zero? (.exitValue process)) line)
                 (let [logs (cons output
                                  (for [^File file (file-seq (io/file home ".vis/logs"))
                                        :when (and (.isFile file)
                                                   (str/ends-with? (.getName file) ".log"))]

                                    (slurp file)))
                       tail (str/join "\n" (mapcat #(take-last 40 (str/split-lines %)) logs))]

                   (throw (ex-info (str "isolated gateway startup failed\n" tail)
                                   {:exit (.exitValue process)}))))
               (edn/read-string line))
             (finally (when (.isAlive process)
                        (.destroyForcibly process)
                        (.waitFor process 10 TimeUnit/SECONDS)))))
      (finally
        ;; Files.walk does not follow the artifact-cache symlink.
        (with-open [paths (Files/walk (.toPath home) (make-array FileVisitOption 0))]
          (doseq [^Path path (reverse (iterator-seq (.iterator paths)))]
            (Files/deleteIfExists path)))))))

(deftest cold-gateway-startup-test
  (dotimes [n (Long/parseLong (System/getProperty "vis.startup.runs" "1"))]
    (let [result (cold-start! (inc n))]
      (println "Gateway startup" (pr-str (dissoc result :loaded-namespaces)))
      (is (= 200 (:health-status result)))
      (is (= (str (:home result) "/.vis/native/sqlite") (:sqlite-tmpdir result)))
      (is (every? (set (:extensions result))
                  ["foundation-core" "language-clojure" "language-python"]))
      ;; Registration must keep callable handlers, not eagerly compile both formatters.
      ;; A fresh JVM is essential: other tests may already have used either backend.
      (doseq [ns-sym '[zprint.core zprint.config cljfmt.core cljfmt.config
                       com.blockether.vis.internal.language.clojure.test-runner]]
        (is (not (contains? (:loaded-namespaces result) ns-sym)) (str ns-sym)))
      (is (= "(defn f [x]\n  (+ x 1))\n" (:default-format result)))
      (is (str/includes? (:first-test-error result) "no such path"))
      (is (= "(defn f [x] (+ x 1))\n" (:configured-format result)))
      (is (some #(= :database (first %)) (:timings result))))))
