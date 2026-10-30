(ns com.blockether.vis.internal.python.runtime-test
  "Getting the interpreter onto a machine that has none.

   The download itself is not exercised here — a suite that fetched 25 MB to
   prove HTTP works would be testing GitHub. What IS exercised is everything
   around it: the asset a version and platform name, the unpack that has to keep
   symlinks and execute bits, and the promise that a runtime already resolvable
   is never touched."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis-python-runtime :as runtime]
            [com.blockether.vis.internal.config.core :as config]
            [com.blockether.vis.internal.gateway.runtime :as gateway-runtime]
            [com.blockether.vis.internal.paths :as paths]
            [com.blockether.vis.internal.python.runtime :as python-runtime]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [com.blockether.vispython Interpreter]
           [com.sun.net.httpserver HttpServer HttpHandler HttpExchange]
           [java.net InetSocketAddress]
           [java.nio.file Files]
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
    (let [^java.util.List command
          ["tar" "czf" (.getAbsolutePath out) "-C" (.getAbsolutePath source) "."]

          process
          (.start (ProcessBuilder. command))]

      (.waitFor process))
    out))

(defdescribe archive-url-names-the-release-asset-test
             (it "the asset a version and platform resolve to, on the runtime's own release"
                 (expect (= (str
                              "https://github.com/Blockether/vis-python-runtime/releases/download"
                              "/v0.1.0/vis-python-runtime-darwin-arm64-0.1.0.tar.gz")
                            (python-runtime/archive-url "0.1.0" "darwin-arm64")))))

(defdescribe install-archive-keeps-what-an-interpreter-needs-test
             (it "modes and symlinks survive, because `tar` unpacks and a jar never could"
                 (let [home (io/file (temp-dir "vis-python-home") "0.1.0")]
                   (#'python-runtime/install-archive! (sample-archive) home)
                   (expect (.isFile (io/file home "libvispython.dylib")))
                   (expect (.canExecute (io/file home "python" "bin" "python3"))
                           "pip runs the interpreter as a program")
                   (expect (Files/isSymbolicLink (.toPath (io/file home "python" "bin" "python")))
                           "the vendored tree links its own names")))
             (it "the staging directory is gone, so an installation is whole or absent"
                 (let [home (io/file (temp-dir "vis-python-home") "0.1.0")]
                   (#'python-runtime/install-archive! (sample-archive) home)
                   (expect (empty? (filter #(re-find #"\.tmp\." (.getName ^java.io.File %))
                                           (.listFiles (.getParentFile home))))))))

(defdescribe
  concurrent-cold-library-provisioning-test
  (it
    "concurrent cold library provisioning"
    ;; Startup extension loading and an HTTP MCP probe can provision concurrently.
    (let [home
          (temp-dir "vis-concurrent-runtime")

          prior-home
          (System/getProperty "user.home")

          selected
          (atom nil)

          downloads
          (atom 0)

          installs
          (atom 0)

          entered
          (promise)

          release
          (promise)

          second-thread
          (promise)]

      (try (System/setProperty "user.home" (str home))
           (with-redefs-fn {#'python-runtime/resolved-library #(deref selected)
                            #'python-runtime/download! (fn [_ archive]
                                                         (swap! downloads inc)
                                                         (deliver entered true)
                                                         @release
                                                         (io/make-parents archive)
                                                         (spit archive "fixture"))
                            #'python-runtime/install-archive!
                            (fn [_ destination]
                              (swap! installs inc)
                              (.mkdirs ^java.io.File destination)
                              (spit (io/file destination (runtime/library-name (runtime/platform)))
                                    "fixture"))
                            #'runtime/use-library!
                            (fn [destination]
                              (reset! selected (str (io/file destination
                                                             (runtime/library-name
                                                               (runtime/platform))))))}
             (fn []
               (let [first-call (future (python-runtime/ensure-library!))]
                 (try (expect (true? (deref entered 5000 false)))
                      (let [second-call (future (deliver second-thread (Thread/currentThread))
                                                (python-runtime/ensure-library!))]
                        (try (let [thread (deref second-thread 5000 nil)]
                               (expect (some? thread))
                               ;; Wait for either the protected monitor or an overlapping download.
                               (loop [remaining 5000]
                                 (when (and (pos? remaining)
                                            (= 1 @downloads)
                                            (not= Thread$State/BLOCKED (.getState ^Thread thread)))
                                   (Thread/sleep 1)
                                   (recur (dec remaining))))
                               (expect (= Thread$State/BLOCKED (.getState ^Thread thread)))
                               (expect (= 1 @downloads)))
                             (deliver release true)
                             (expect (= (deref first-call 5000 ::timeout)
                                        (deref second-call 5000 ::timeout)
                                        @selected))
                             (expect (= 1 @installs))
                             (expect (= @selected (python-runtime/ensure-library!)))
                             (expect (= 1 @downloads))
                             (finally (deliver release true) (future-cancel second-call))))
                      (finally (deliver release true) (future-cancel first-call))))))
           (finally (System/setProperty "user.home" prior-home)
                    (#'python-runtime/delete-tree! home))))))

(defdescribe
  failed-library-provisioning-can-retry-test
  (it
    "failed library provisioning can retry"
    (let [home
          (temp-dir "vis-runtime-retry")

          prior-home
          (System/getProperty "user.home")

          selected
          (atom nil)

          attempts
          (atom 0)]

      (try (System/setProperty "user.home" (str home))
           (with-redefs-fn
             {#'python-runtime/resolved-library #(deref selected)
              #'python-runtime/download! (fn [_ _]
                                           (when (= 1 (swap! attempts inc))
                                             (throw (ex-info "fixture download failure" {}))))
              #'python-runtime/install-archive!
              (fn [_ destination]
                (.mkdirs ^java.io.File destination)
                (spit (io/file destination (runtime/library-name (runtime/platform))) "fixture"))
              #'runtime/use-library! (fn [destination]
                                       (reset! selected (str (io/file destination
                                                                      (runtime/library-name
                                                                        (runtime/platform))))))}
             (fn []
               (expect (= "fixture download failure"
                          (try (python-runtime/ensure-library!)
                               (catch clojure.lang.ExceptionInfo error (ex-message error)))))
               ;; Retry from a different thread: an unreleased lock must not pass.
               (let [retry (future (python-runtime/ensure-library!))]
                 (try (expect (= (deref retry 5000 ::timeout) @selected))
                      (expect (= 2 @attempts))
                      (finally (future-cancel retry))))))
           (finally (System/setProperty "user.home" prior-home)
                    (#'python-runtime/delete-tree! home))))))

(defdescribe
  ensure-library-answers-a-real-runtime-test
  (it
    "the interpreter this machine runs — already resolvable, or fetched once — and the same one on the next call"
    (let [library (python-runtime/ensure-library!)]
      (expect (.isFile (io/file library)))
      (expect (= library (python-runtime/ensure-library!)))
      (expect (= library (:path (runtime/resolve-library)))))))

(defdescribe
  pip-index-from-vis-yaml-test
  (it
    "pip index from vis yaml"
    (let [dir
          (temp-dir "vis-pip-config")

          yml
          (io/file dir "vis.yml")

          calls
          (atom [])

          refreshes
          (atom 0)]

      (try (with-redefs [config/load-config-raw
                         #(@#'config/read-yaml-config-map (str yml))

                         runtime/pip-install!
                         (fn [opts specs]
                           (swap! calls conj [opts specs])
                           {:exit 0 :out ""})

                         runtime/exec!
                         (fn [& _]
                           (swap! refreshes inc))]

             (spit yml "python:\n  index_url: https://gateway.example.com/simple\n")
             (expect (= "https://gateway.example.com/simple"
                        (get-in (config/load-config false) [:python :index-url])))
             (expect (= 0 (:exit (python-runtime/pip-install! ["pytest==8.4.0"]))))
             (expect (= [{} ["--index-url" "https://gateway.example.com/simple" "pytest==8.4.0"]]
                        (last @calls)))
             (spit yml "python:\n  index_url: https://gateway.example.com/other/simple\n")
             (python-runtime/pip-install! ["six"])
             (expect (= [{} ["--index-url" "https://gateway.example.com/other/simple" "six"]]
                        (last @calls)))
             (spit yml "python: {}\n")
             (python-runtime/pip-install! ["six"])
             (expect (= [{} ["six"]] (last @calls)))
             (expect (= 3 @refreshes)))
           (finally (io/delete-file yml true) (io/delete-file dir true))))))

(defdescribe pip-index-invalid-config-does-not-install-test
             (it "pip index invalid config does not install"
                 ;; The live config reader is lenient; the install boundary must still refuse
                 ;; invalid settings rather than silently installing from a different index.
                 (doseq [value [nil "" " " 42 ["https://gateway.example.com/simple"] "--no-index"
                                "https://user:password@gateway.example.com/simple"
                                "https://gateway.example.com/simple?token=fixture"
                                "https://gateway.example.com/simple#fragment"]]
                   (let [calls (atom 0)]
                     (with-redefs [config/load-config-raw (constantly {"python" {"index_url"
                                                                                 value}})
                                   runtime/pip-install! (fn [& _]
                                                          (swap! calls inc))]

                       (expect (re-find #"python.index_url"
                                        (try (python-runtime/pip-install! ["six"])
                                             "no error"
                                             (catch clojure.lang.ExceptionInfo e (ex-message e)))))
                       (expect (zero? @calls)))))))

(defdescribe
  pip-requests-index-from-vis-yaml-test
  (it "pip requests index from vis yaml"
      ;; Exercise the real runtime/pip boundary against an offline index. A 404 is
      ;; deliberate: the fixture must never fetch or install a distribution.
      (python-runtime/ensure-library!)
      (let [dir
            (temp-dir "vis-pip-index")

            yml
            (io/file dir "vis.yml")

            requests
            (atom [])

            server
            (HttpServer/create (InetSocketAddress. "127.0.0.1" 0) 0)

            install!
            runtime/pip-install!]

        (.createContext server
                        "/"
                        (reify
                          HttpHandler
                            (handle [_ exchange]
                              (let [^HttpExchange exchange exchange]
                                (swap! requests conj (str (.getRequestURI exchange)))
                                (.sendResponseHeaders exchange 404 -1)
                                (.close exchange)))))
        (.start server)
        (try (spit yml
                   (str "python:\n  index_url: http://127.0.0.1:"
                        (.getPort (.getAddress server))
                        "/simple\n"))
             (with-redefs [config/load-config-raw
                           #(@#'config/read-yaml-config-map (str yml))

                           runtime/pip-install!
                           (fn [opts specs]
                             (install! (assoc opts :target (str (io/file dir "packages"))) specs))]

               (let [result (python-runtime/pip-install! ["--isolated" "--proxy" "" "--retries" "0"
                                                          "--timeout" "3" "--no-cache-dir"
                                                          "vis-index-fixture-missing"])]
                 (expect (= 1 (:exit result)))
                 (expect (= ["/simple/vis-index-fixture-missing/"] @requests))
                 (expect (some #{"--only-binary=:all:"} (:command result)))))
             (finally (.stop server 0)
                      (doseq [f (reverse (file-seq dir))]
                        (io/delete-file f true)))))))

(defdescribe
  manual-project-uses-uv-check-test
  (it "manual project uses uv check"
      ;; #183: readiness belongs to uv, not Vis fingerprints or a shared-package marker.
      (python-runtime/ensure-library!)
      (let [project
            (temp-dir "vis-manual-uv")

            packages
            (doto (io/file project "custom-env/site-packages") .mkdirs)

            calls
            (atom [])]

        (try (with-redefs-fn {#'python-runtime/shared-project? (constantly false)
                              #'python-runtime/runtime-environment? (constantly true)
                              #'python-runtime/bundled-uv! (constantly "/bundled/uv")
                              #'python-runtime/run-uv!
                              (fn [cwd args]
                                (expect (= project cwd))
                                (swap! calls conj args)
                                (when (= "run" (second args))
                                  (str "VIS_PROJECT_SITE=" (pr-str (str packages)) "\n")))}
               (fn []
                 (expect (= (.getCanonicalFile packages) (python-runtime/prepared-project project)))
                 (expect (= (into ["/bundled/uv" "sync" "--check"]
                                  (#'python-runtime/embedded-python))
                            (first @calls)))
                 (expect (= ["/bundled/uv" "run" "--no-sync" "--python"
                             (com.blockether.vispython.Interpreter/pythonExecutable)
                             "--no-python-downloads" "python" "-I" "-B" "-c"]
                            (vec (take 10 (second @calls)))))
                 (expect (= 2 (count @calls)))))
             (finally (doseq [file (reverse (file-seq project))]
                        (io/delete-file file true)))))))

(defdescribe
  manual-project-sync-advice-uses-current-directory-test
  (it "manual project sync advice uses current directory"
      (python-runtime/ensure-library!)
      (let [current
            (.getCanonicalFile (io/file (System/getProperty "user.dir")))

            elsewhere
            (temp-dir "vis manual uv other")]

        (try (with-redefs-fn {#'python-runtime/shared-project? (constantly false)
                              #'python-runtime/runtime-environment? (constantly true)
                              #'python-runtime/bundled-uv! (constantly "/bundled/uv")
                              #'python-runtime/run-uv! (fn [_ _]
                                                         (throw (ex-info "uv sync failed" {})))}
               (fn []
                 (let [same-dir
                       (try (python-runtime/prepared-project current)
                            (catch clojure.lang.ExceptionInfo e e))

                       other-dir
                       (try (python-runtime/prepared-project elsewhere)
                            (catch clojure.lang.ExceptionInfo e e))]

                   (expect (= (str "uv sync failed\nRun vis-agent python uv sync --project ."
                                   " --python "
                                   (paths/shell-path
                                     (com.blockether.vispython.Interpreter/pythonExecutable))
                                   " --no-python-downloads, then /reload.")
                              (.getMessage same-dir)))
                   (expect (= (str "uv sync failed\nRun vis-agent python uv sync --project "
                                   (paths/shell-path (.getCanonicalPath elsewhere))
                                   " --python "
                                   (paths/shell-path
                                     (com.blockether.vispython.Interpreter/pythonExecutable))
                                   " --no-python-downloads, then /reload.")
                              (.getMessage other-dir)))
                   (expect (= ::python-runtime/project-sync-required (:type (ex-data same-dir)))))))
             (finally (doseq [file (reverse (file-seq elsewhere))]
                        (io/delete-file file true)))))))

(defdescribe
  manual-project-refuses-a-foreign-interpreter-test
  (it "rejects a foreign interpreter before the package probe or any installation"
      ;; uv sync --check alone accepts an environment from another Python.
      (python-runtime/ensure-library!)
      (let [project
            (temp-dir "vis-manual-foreign-python")

            packages
            (doto (io/file project ".venv/site-packages") .mkdirs)

            config
            (io/file project ".venv/pyvenv.cfg")

            calls
            (atom [])]

        (try (spit config "home = /foreign/python/bin\n")
             (with-redefs-fn {#'python-runtime/bundled-uv! (constantly "/bundled/uv")
                              #'python-runtime/run-uv!
                              (fn [_ args]
                                (swap! calls conj args)
                                (case (second args)
                                  "workspace"
                                  (str project)

                                  "export"
                                  (do (spit (nth args
                                                 (inc (.indexOf ^java.util.List args
                                                                "--output-file")))
                                            "fixture-dependency==1.0\n")
                                      "")

                                  "run"
                                  (str "VIS_PROJECT_SITE=" (pr-str (str packages)) "\n")

                                  ""))}
               (fn []
                 (let [error (try (python-runtime/prepared-project project)
                                  nil
                                  (catch clojure.lang.ExceptionInfo e e))]
                   (expect (= ::python-runtime/project-sync-required (:type (ex-data error))))
                   (expect (str/includes? (str (ex-message error)) "--no-python-downloads"))
                   (expect (not-any? #(contains? #{"sync" "run"} (second %)) @calls))
                   (expect (= "home = /foreign/python/bin\n" (slurp config))))))
             (finally (#'python-runtime/delete-tree! project))))))

(defdescribe
  automatic-project-preparation-test
  (it "automatic project preparation"
      (python-runtime/ensure-library!)
      (let [project
            (temp-dir "vis-auto-project")

            calls
            (atom [])]

        (try (with-redefs-fn {#'python-runtime/run-uv! (fn [_ args]
                                                         (swap! calls conj args))
                              #'python-runtime/shared-project? (constantly false)
                              #'python-runtime/runtime-environment? (constantly true)
                              #'python-runtime/project-packages (constantly project)}
               (fn []
                 (dotimes [_ 2]
                   (expect (= project (python-runtime/ensure-project! project))))
                 (expect (= 2 (count @calls)) "uv decides whether the environment needs updating")
                 (doseq [args @calls]
                   (expect (.isAbsolute (io/file (first args))))
                   (expect (= ["sync" "--check" "--offline" "--python"
                               (com.blockether.vispython.Interpreter/pythonExecutable)
                               "--no-python-downloads"]
                              (vec (rest args)))))
                 (expect (= "cached"
                            (:stage (first (filter #(= (.getName project) (:name %))
                                                   (python-runtime/preparation-status))))))))
             (finally (doseq [file (reverse (file-seq project))]
                        (io/delete-file file true)))))))

(defdescribe
  automatic-project-preparation-falls-back-after-an-offline-miss
  (it "automatic project preparation falls back after an offline miss"
      (python-runtime/ensure-library!)
      (let [project
            (temp-dir "vis-cold-project")

            calls
            (atom [])]

        (try (with-redefs-fn {#'python-runtime/run-uv! (fn [_ args]
                                                         (swap! calls conj (vec (rest args)))
                                                         (when (= 1 (count @calls))
                                                           (throw (ex-info "Environment needs sync"
                                                                           {}))))
                              #'python-runtime/shared-project? (constantly false)
                              #'python-runtime/remove-environment-shell! (constantly nil)
                              #'python-runtime/runtime-environment? (constantly true)
                              #'python-runtime/project-packages (constantly project)}
               (fn []
                 (expect (= project (python-runtime/ensure-project! project)))
                 (expect (= [["sync" "--check" "--offline" "--python"
                              (com.blockether.vispython.Interpreter/pythonExecutable)
                              "--no-python-downloads"]
                             ["sync" "--python"
                              (com.blockether.vispython.Interpreter/pythonExecutable)
                              "--no-python-downloads"]]
                            @calls))
                 (expect (= "ready"
                            (:stage (first (filter #(= (.getName project) (:name %))
                                                   (python-runtime/preparation-status))))))))
             (finally (doseq [file (reverse (file-seq project))]
                        (io/delete-file file true)))))))

;; Issue #407: a verified preparation skips uv until the project, lock or environment changes.
(defdescribe
  automatic-project-preparation-reuses-a-stamped-environment
  (it
    "skips uv while the stamped inputs are unchanged"
    (python-runtime/ensure-library!)
    (let [project
          (temp-dir "vis-stamped-project")

          site
          (io/file project
                   (#'python-runtime/automatic-environment)
                   "lib"
                   "python3.14"
                   "site-packages")

          calls
          (atom [])]

      (try (spit (io/file project "pyproject.toml") "[project]\nname = \"stamped\"\n")
           (spit (io/file project "uv.lock") "version = 1\n")
           (.mkdirs site)
           (spit (io/file project (#'python-runtime/automatic-environment) "pyvenv.cfg")
                 "home = /embedded/bin\n")
           (with-redefs-fn {#'python-runtime/run-uv! (fn [_ args]
                                                       (swap! calls conj (second args)))
                            #'python-runtime/shared-project? (constantly false)
                            #'python-runtime/runtime-environment? (constantly true)
                            #'python-runtime/project-packages (fn [_]
                                                                (.getCanonicalFile site))}
             (fn []
               (let [packages
                     (.getCanonicalFile site)

                     prepare!
                     #(do (reset! calls [])
                          (expect (= packages (python-runtime/ensure-project! project)))
                          @calls)]

                 (expect (= ["sync"] (prepare!)) "the first preparation asks uv")
                 (expect (= [] (prepare!)) "an unchanged project skips uv")
                 (spit (io/file project "uv.lock") "version = 1\n# changed\n")
                 (expect (= ["sync"] (prepare!)) "a changed lock asks uv again")
                 (expect (= [] (prepare!)))
                 (.setLastModified site (- (.lastModified site) 60000))
                 (expect (= ["sync"] (prepare!)) "a changed environment asks uv again")
                 (.delete
                   (io/file project (#'python-runtime/automatic-environment) ".vis-prepared.edn"))
                 (expect (= []
                            (do (#'python-runtime/stamp-synced-project!
                                 ["sync" (str "--project=" (.getPath project))])
                                (prepare!)))
                         "a successful vis-agent python uv sync stamps the project"))))
           (finally (#'python-runtime/delete-tree! project)))))
  ;; A container image holds a prepared, read-only environment; a failed stamp must not fail it.
  (it "prepares a read-only environment with the uv check alone"
      (python-runtime/ensure-library!)
      (let [project
            (temp-dir "vis-read-only-project")

            environment
            (io/file project (#'python-runtime/automatic-environment))

            site
            (io/file environment "lib" "python3.14" "site-packages")

            calls
            (atom [])]

        (try (spit (io/file project "pyproject.toml") "[project]\nname = \"read-only\"\n")
             (spit (io/file project "uv.lock") "version = 1\n")
             (.mkdirs site)
             (spit (io/file environment "pyvenv.cfg") "home = /embedded/bin\n")
             (.setWritable environment false false)
             (with-redefs-fn {#'python-runtime/run-uv! (fn [_ args]
                                                         (swap! calls conj (second args)))
                              #'python-runtime/shared-project? (constantly false)
                              #'python-runtime/runtime-environment? (constantly true)
                              #'python-runtime/project-packages (fn [_]
                                                                  (.getCanonicalFile site))}
               (fn []
                 (expect (= (.getCanonicalFile site) (python-runtime/ensure-project! project)))
                 (expect (= ["sync"] @calls) "only the offline check runs")
                 (expect (not (.exists (io/file environment ".vis-prepared.edn"))))))
             (finally (.setWritable environment true false)
                      (#'python-runtime/delete-tree! project)))))
  ;; #407: a new installation compiles its bytecode, so a worker imports without a compile.
  (it "compiles a new installation into the bytecode cache"
      (python-runtime/ensure-library!)
      (let [project
            (temp-dir "vis-compiled-project")

            prefix
            (temp-dir "vis-compiled-prefix")

            site
            (io/file project
                     (#'python-runtime/automatic-environment)
                     "lib"
                     "python3.14"
                     "site-packages")]

        (try (spit (io/file project "pyproject.toml") "[project]\nname = \"compiled\"\n")
             (spit (io/file project "extension.py") "VALUE = 1\n")
             (.mkdirs site)
             (spit (io/file site "dependency.py") "VALUE = 2\n")
             (spit (io/file project (#'python-runtime/automatic-environment) "pyvenv.cfg")
                   "home = /embedded/bin\n")
             (with-redefs-fn {#'python-runtime/run-uv! (fn [_ args]
                                                         (when (some #{"--check"} args)
                                                           (throw (ex-info "not ready" {}))))
                              #'python-runtime/shared-project? (constantly false)
                              #'python-runtime/runtime-environment? (constantly true)
                              #'python-runtime/remove-environment-shell! (fn [_])
                              #'python-runtime/pycache-prefix (constantly (str prefix))
                              #'python-runtime/project-packages (fn [_]
                                                                  (.getCanonicalFile site))}
               (fn []
                 (python-runtime/ensure-project! project)
                 (let [compiled (->> (file-seq prefix)
                                     (map #(.getName ^java.io.File %))
                                     (filter #(str/ends-with? % ".pyc"))
                                     set)]
                   (expect (some #(str/starts-with? % "extension.") compiled))
                   (expect (some #(str/starts-with? % "dependency.") compiled)))))
             (finally (#'python-runtime/delete-tree! project)
                      (#'python-runtime/delete-tree! prefix))))))

;; User report 2026-10-30: graphify failed with "No module named graphify". A native run
;; rebuilt the shared `.venv` of an extension under a running JVM gateway.
(defdescribe automatic-environment-per-interpreter-test
             (it "gives the JVM and the native interpreter separate environments"
                 (let [jvm-python
                       "/home/user/.vis/python/runtime/0.5.25/darwin-arm64/python/bin/python3"

                       jvm
                       (#'python-runtime/environment-name jvm-python)

                       native
                       (#'python-runtime/environment-name
                        "/opt/vis/vis-agent-python/python/bin/python3")]

                   (expect (not= jvm native))
                   (expect (= jvm (#'python-runtime/environment-name jvm-python)))
                   ;; Extension scans and snapshots skip these directories by this prefix.
                   (expect (every? #(str/starts-with? % ".venv-") [jvm native]))))
             ;; A declared project keeps its manually synced `.venv` and its editable paths.
             (it "keeps a project .venv of this Python and leaves another Python's .venv alone"
                 (python-runtime/ensure-library!)
                 (let [project
                       (temp-dir "vis-own-venv")

                       config
                       (io/file project ".venv" "pyvenv.cfg")

                       own-home
                       (.getParent (io/file (Interpreter/pythonExecutable)))]

                   (try (io/make-parents config)
                        (spit config (str "home = " own-home "\n"))
                        (expect (= ".venv" (#'python-runtime/own-venv project)))
                        (spit config "home = /other/embedded/python/bin\n")
                        (expect (nil? (#'python-runtime/own-venv project)))
                        (finally (#'python-runtime/delete-tree! project))))))

(defdescribe
  automatic-project-preparation-replaces-an-environment-from-another-python
  (it "automatic project preparation replaces an environment from another python"
      ;; uv's check also passes without .venv or with another interpreter,
      ;; so preparation syncs on the embedded Python without asking it.
      (python-runtime/ensure-library!)
      (let [project
            (temp-dir "vis-foreign-project")

            calls
            (atom [])]

        (try (with-redefs-fn {#'python-runtime/run-uv! (fn [_ args]
                                                         (swap! calls conj (vec (rest args))))
                              #'python-runtime/shared-project? (constantly false)
                              #'python-runtime/remove-environment-shell! (constantly nil)
                              #'python-runtime/runtime-environment? (constantly false)
                              #'python-runtime/project-packages (constantly project)}
               (fn []
                 (expect (= project (python-runtime/ensure-project! project)))
                 (expect (= [["sync" "--python"
                              (com.blockether.vispython.Interpreter/pythonExecutable)
                              "--no-python-downloads"]]
                            @calls))
                 (expect (= "ready"
                            (:stage (first (filter #(= (.getName project) (:name %))
                                                   (python-runtime/preparation-status))))))))
             (finally (doseq [file (reverse (file-seq project))]
                        (io/delete-file file true)))))))

;; Regression (#276, and a gateway startup that printed `[vis extensions] 1.5.1: cached`):
;; an installed extension's project directory IS its version, so the bare directory
;; named no package and two extensions on one version shared a single status entry.
(defdescribe
  an-installed-extension-is-staged-under-its-package-and-version
  (it "an installed extension is staged under its package and version"
      (let [root
            (temp-dir "vis-ext-name")

            project
            (io/file root "vis-lang-clojure" "1.5.1")

            sibling
            (io/file root "vis-lang-python" "1.5.1")]

        (.mkdirs project)
        (.mkdirs sibling)
        (expect (= "vis-lang-clojure 1.5.1" (#'python-runtime/project-display-name project)))
        (expect (= "greeter" (#'python-runtime/project-display-name (io/file root "greeter")))
                "a project that is not a version directory keeps its own name")
        (#'python-runtime/preparation-stage! project "cached")
        (#'python-runtime/preparation-stage! sibling "installing")
        (expect (= "cached"
                   (:stage (first (filter #(= "vis-lang-clojure 1.5.1" (:name %))
                                          (python-runtime/preparation-status))))))
        (expect (= "installing"
                   (:stage (first (filter #(= "vis-lang-python 1.5.1" (:name %))
                                          (python-runtime/preparation-status)))))
                "sync names every live stage line, so one version cannot hide another"))))

;; Regression: a gateway start printed `cached` for every package, once per check.
(defdescribe
  preparation-reports-only-failures-to-the-terminal
  (it "preparation reports only failures to the terminal"
      (let [project
            (io/file (System/getProperty "java.io.tmpdir") "vis-quiet-preparation" "quiet-greeter")

            out
            (java.io.ByteArrayOutputStream.)]

        (with-redefs [config/original-stderr (java.io.PrintStream. out true "UTF-8")]
          (doseq [stage ["cached" "installing" "ready"]]
            (#'python-runtime/preparation-stage! project stage))
          (expect (= "" (.toString out "UTF-8")) "a healthy package is not news")
          (expect (= "ready"
                     (:stage (first (filter #(= "quiet-greeter" (:name %))
                                            (python-runtime/preparation-status)))))
                  "status still follows every stage")
          (#'python-runtime/preparation-stage! project "failed")
          (expect (= "[vis extensions] quiet-greeter: failed" (.trim (.toString out "UTF-8"))))))))

(defdescribe version-globals-test
             ;; #312: betas reported VIS_VERSION for an SDK newer than that PyPI release.
             (it "reports the SDK version that the build stamped, apart from VIS_VERSION"
                 (with-redefs [gateway-runtime/release-version
                               (constantly "1.4.9")

                               gateway-runtime/sdk-version
                               (constantly "1.4.10.dev3")]

                   (expect (= {"VIS_VERSION" "1.4.9" "VIS_PYTHON_SDK_VERSION" "1.4.10.dev3"}
                              (select-keys (python-runtime/version-globals)
                                           ["VIS_VERSION" "VIS_PYTHON_SDK_VERSION"]))))))

;; Issue #378: a project that installs with `vis-agent python --shared uv sync` can prepare
;; its extensions the same way, without its own .venv.
(defn- shared-preparation-calls
  "Run `f` with a recording uv. The dry-run answer says whether shared packages match the lock."
  [project packages matches? f]
  (let [calls (atom [])]
    (with-redefs-fn {#'python-runtime/run-uv! (fn [dir args]
                                                (swap! calls conj (vec (rest args)))
                                                (expect (= project dir))
                                                (case (second args)
                                                  "workspace"
                                                  (str (.getCanonicalPath project) "\n")

                                                  "pip"
                                                  (if (and matches? (some #{"--dry-run"} args))
                                                    "Would make no changes"
                                                    "Would install 1 package")

                                                  ""))
                     #'runtime/packages-dir (constantly (str packages))}
      (fn []
        {:result (try (f) (catch clojure.lang.ExceptionInfo e e)) :calls @calls}))))

(defdescribe
  shared-project-preparation-test
  (it
    "uses shared packages when the lock matches them, and installs into them when it does not"
    (python-runtime/ensure-library!)
    (let [project
          (temp-dir "vis-shared-project")

          packages
          (temp-dir "vis-shared-packages")

          stage
          #(:stage (first (filter (fn [row]
                                    (= (.getName project) (:name row)))
                                  (python-runtime/preparation-status))))]

      (try
        (let [{:keys [result calls]} (shared-preparation-calls
                                       project
                                       packages
                                       true
                                       #(python-runtime/ensure-shared-project! project))]
          (expect (= (.getCanonicalFile packages) result))
          (expect (= [["workspace" "dir" "--offline"] ["export" "run" "pip"] ["pip" "install"]]
                     [(first calls) (mapv first (take 3 (rest calls)))
                      (vec (take 2 (nth calls 3)))]))
          (expect (every? (set (second calls))
                          ["pylock.toml" "--locked" "--offline" "--no-emit-package" "vis-agent"]))
          (expect (every? (set (nth calls 2)) ["--no-project" "--offline" "-I" "-c"])
                  "the range check script runs offline on the embedded interpreter")
          (expect (every? (set (nth calls 3)) ["--dry-run" "--offline" "--overrides" "--excludes"]))
          (expect (not-any? #{"--no-deps"} (nth calls 3))
                  "the check resolves the declared ranges, not the exact lock pins (#391)")
          (expect (= (str packages) (second (drop-while #(not= "--target" %) (nth calls 3)))))
          (expect (= "cached" (stage))))
        (let [{:keys [result calls]}
              (shared-preparation-calls project
                                        packages
                                        false
                                        #(python-runtime/ensure-shared-project! project))

              install
              (last calls)]

          (expect (= (.getCanonicalFile packages) result))
          (expect (= ["export" "pip"] [(first (nth calls 5)) (first install)]))
          (expect (not-any? #{"--locked" "--offline"} (nth calls 5)))
          (expect (some #{"vis-agent"} (nth calls 5))
                  "extension preparation never installs the SDK")
          (expect (some #{"--no-deps"} install) "an installation keeps the exact lock pins")
          (expect (not-any? #{"--dry-run" "--offline"} install))
          (expect (= "ready" (stage))))
        (expect (not (.exists (io/file project ".venv"))))
        (expect (empty? (filter #(str/starts-with? (.getName ^java.io.File %) "pylock.vis-")
                                (.listFiles project)))
                "the temporary lock export is removed")
        (finally (#'python-runtime/delete-tree! project)
                 (#'python-runtime/delete-tree! packages))))))

(defdescribe
  prepared-shared-project-test
  (it "checks shared packages without installing and names the shared sync command"
      (python-runtime/ensure-library!)
      (let [project
            (temp-dir "vis-shared-manual")

            packages
            (temp-dir "vis-shared-manual-packages")]

        (try (let [{:keys [result calls]} (shared-preparation-calls
                                            project
                                            packages
                                            true
                                            #(python-runtime/prepared-shared-project project))]
               (expect (= (.getCanonicalFile packages) result))
               (expect (= ["workspace" "export" "run" "pip"] (mapv first calls))))
             (let [{:keys [result calls]} (shared-preparation-calls
                                            project
                                            packages
                                            false
                                            #(python-runtime/prepared-shared-project project))]
               (expect (= ::python-runtime/project-sync-required (:type (ex-data result))))
               (expect (str/includes? (ex-message result)
                                      (str "Run vis-agent python --shared uv sync --project "
                                           (paths/shell-path (.getCanonicalPath project)))))
               (expect (every? #(some #{"--dry-run"} %) (filter #(= "pip" (first %)) calls))
                       "manual validation never installs"))
             (finally (#'python-runtime/delete-tree! project)
                      (#'python-runtime/delete-tree! packages))))))

;; Issue #422: each /reload ran four uv processes for every shared project, also unchanged ones.
(defdescribe
  shared-project-inputs-test
  (it "skips uv while the project, its lock and shared packages stay the same"
      (python-runtime/ensure-library!)
      (let [project
            (temp-dir "vis-shared-inputs")

            packages
            (temp-dir "vis-shared-inputs-packages")

            ensure
            #(shared-preparation-calls project
                                       packages
                                       true
                                       (fn []
                                         (python-runtime/ensure-shared-project! project)))]

        (try (spit (io/file project "pyproject.toml") "[project]\nname = \"inputs\"\n")
             (spit (io/file project "uv.lock") "version = 1\n")
             (expect (= ["workspace" "export" "run" "pip"] (mapv first (:calls (ensure)))))
             (let [{:keys [result calls]} (ensure)]
               (expect (= (.getCanonicalFile packages) result))
               (expect (empty? calls) "an unchanged project skips uv"))
             (expect (empty? (:calls (shared-preparation-calls
                                       project
                                       packages
                                       false
                                       #(python-runtime/prepared-shared-project project))))
                     "the check without installation reads the same record")
             (spit (io/file project "uv.lock") "version = 1\nrevision = 2\n")
             (expect (= 4 (count (:calls (ensure)))) "a changed lock runs the check again")
             (.mkdirs (io/file packages "other-1.0.dist-info"))
             (expect (= 4 (count (:calls (ensure))))
                     "a change in shared packages runs the check again")
             (expect (empty? (:calls (ensure))))
             (finally (#'python-runtime/delete-tree! project)
                      (#'python-runtime/delete-tree! packages)))))
  (it "records an installation, so that the next preparation does not install again"
      (python-runtime/ensure-library!)
      (let [project
            (temp-dir "vis-shared-installed")

            packages
            (temp-dir "vis-shared-installed-packages")

            ensure
            #(shared-preparation-calls project
                                       packages
                                       false
                                       (fn []
                                         (python-runtime/ensure-shared-project! project)))]

        (try (spit (io/file project "uv.lock") "version = 1\n")
             (expect (= "pip" (first (last (:calls (ensure))))))
             (expect (empty? (:calls (ensure))))
             (finally (#'python-runtime/delete-tree! project)
                      (#'python-runtime/delete-tree! packages)))))
  (it "keeps the uv check for a project without uv.lock"
      (python-runtime/ensure-library!)
      (let [project
            (temp-dir "vis-shared-unlocked")

            packages
            (temp-dir "vis-shared-unlocked-packages")]

        (try (dotimes [_ 2]
               (expect (= 4
                          (count (:calls (shared-preparation-calls
                                           project
                                           packages
                                           true
                                           #(python-runtime/ensure-shared-project! project)))))))
             (finally (#'python-runtime/delete-tree! project)
                      (#'python-runtime/delete-tree! packages))))))

;; Issue #391: every extension lock installs into the same shared packages. A check of the exact
;; lock pins failed each project whose versions a later installation replaced.
(defn- wheel!
  "Write a minimal pure-Python wheel of `package` at `version` into `dir`."
  [^java.io.File dir ^String package ^String version]
  (let [module
        (str/replace package "-" "_")

        dist
        (str module "-" version ".dist-info")

        files
        {(str module "/__init__.py") (str "VERSION = '" version "'\n")
         (str dist "/METADATA")
         (str "Metadata-Version: 2.1\nName: " package "\nVersion: " version "\n")
         (str dist "/WHEEL")
         "Wheel-Version: 1.0\nGenerator: vis-test\nRoot-Is-Purelib: true\nTag: py3-none-any\n"}

        files
        (assoc files
          (str dist "/RECORD")
          (str (str/join (map #(str % ",,\n") (keys files))) dist "/RECORD,,\n"))]

    (with-open [zip (java.util.zip.ZipOutputStream.
                      (io/output-stream (io/file dir
                                                 (str module "-" version "-py3-none-any.whl"))))]
      (doseq [[^String path ^String text] files]
        (.putNextEntry zip (java.util.zip.ZipEntry. path))
        (.write zip (.getBytes text "UTF-8"))
        (.closeEntry zip)))))

(defn- locked-project!
  "Create and lock a uv project `project-name` in `root` that needs `dependencies` from `wheels`."
  ^java.io.File [^java.io.File root project-name dependencies ^java.io.File wheels]
  (let [project (io/file root project-name)]
    (.mkdirs project)
    (spit (io/file project "pyproject.toml")
          (str "[project]\nname = \""
               project-name
               "\"\nversion = \"0.1.0\"\n"
               "requires-python = \">=3.12\"\ndependencies = ["
               (str/join ", " (map #(str "\"" % "\"") dependencies))
               "]\n\n[tool.uv]\npackage = false\nno-index = true\nfind-links = [\""
               (.getCanonicalPath wheels)
               "\"]\n"))
    (#'python-runtime/run-uv!
     project
     (into [(#'python-runtime/bundled-uv!) "lock" "--offline"] (#'python-runtime/embedded-python)))
    project))

(defn- installed-versions
  "The versions of distribution `module` in shared `packages`, from the dist-info names."
  [^java.io.File packages module]
  (sort (keep #(second (re-matches (re-pattern (str module "-(.+)\\.dist-info"))
                                   (.getName ^java.io.File %)))
              (.listFiles packages))))

(defdescribe
  shared-project-ranges-test
  (it
    "accepts versions from another extension lock that satisfy the declared ranges"
    (python-runtime/ensure-library!)
    (let [root
          (temp-dir "vis-shared-ranges")

          wheels
          (doto (io/file root "wheels") .mkdirs)

          packages
          (doto (io/file root "packages") .mkdirs)]

      (try
        (wheel! wheels "dep" "1.0")
        (wheel! wheels "vis-agent" "0.0.1")
        (let [older
              (locked-project! root "older" ["dep>=1.0" "vis-agent>=0.0.1"] wheels)

              _
              (wheel! wheels "dep" "1.1")

              newer
              (locked-project! root "newer" ["dep>=1.0"] wheels)

              strict
              (locked-project! root "strict" ["dep<1.1"] wheels)

              failure
              #(try (python-runtime/prepared-shared-project %)
                    nil
                    (catch clojure.lang.ExceptionInfo e e))]

          (with-redefs [runtime/packages-dir (constantly (str packages))]
            (python-runtime/ensure-shared-project! older)
            (expect (= ["1.0"] (installed-versions packages "dep")))
            (expect (empty? (installed-versions packages "vis_agent"))
                    "extension preparation never installs the bundled SDK")
            (#'python-runtime/shared-project-install! newer)
            (expect (= ["1.1"] (installed-versions packages "dep")))
            (expect (= (.getCanonicalFile packages) (python-runtime/prepared-shared-project older))
                    "dep 1.1 satisfies dep>=1.0, although the older lock pins 1.0")
            (expect (= (.getCanonicalFile packages) (python-runtime/prepared-shared-project newer)))
            (let [e (failure strict)]
              (expect (= ::python-runtime/project-sync-required (:type (ex-data e))))
              (expect (str/includes? (ex-message e) "dep==1.0")
                      "a real conflict names the package and the version it needs"))
            (python-runtime/ensure-shared-project! older)
            (expect (= ["1.1"] (installed-versions packages "dep"))
                    "a satisfied project installs nothing")))
        (finally (#'python-runtime/delete-tree! root))))))

(defdescribe
  shared-environment-setting-test
  (it "reads python.environment and rejects unknown values"
      (doseq [[python expected] [[nil false] [{"environment" "project"} false]
                                 [{"environment" "shared"} true]
                                 [{"environment" "venv"} ::invalid]]]
        (with-redefs [config/load-config-raw (constantly (if python {"python" python} {}))]
          (expect (= expected
                     (try (python-runtime/shared-environment?)
                          (catch clojure.lang.ExceptionInfo e
                            (when (= ::python-runtime/invalid-environment (:type (ex-data e)))
                              ::invalid)))))))))
