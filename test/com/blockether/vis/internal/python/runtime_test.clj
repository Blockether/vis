(ns com.blockether.vis.internal.python.runtime-test
  "Getting the interpreter onto a machine that has none.

   The download itself is not exercised here — a suite that fetched 25 MB to
   prove HTTP works would be testing GitHub. What IS exercised is everything
   around it: the asset a version and platform name, the unpack that has to keep
   symlinks and execute bits, and the promise that a runtime already resolvable
   is never touched."
  (:require [clojure.java.io :as io]
            [com.blockether.vis-python-runtime :as runtime]
            [com.blockether.vis.internal.config.core :as config]
            [com.blockether.vis.internal.paths :as paths]
            [com.blockether.vis.internal.python.runtime :as python-runtime]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [com.sun.net.httpserver HttpServer HttpHandler HttpExchange]
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
      (let [project
            (temp-dir "vis-manual-uv")

            packages
            (doto (io/file project "custom-env/site-packages") .mkdirs)

            calls
            (atom [])]

        (try (with-redefs-fn {#'python-runtime/bundled-uv! (constantly "/bundled/uv")
                              #'python-runtime/run-uv!
                              (fn [cwd args]
                                (expect (= project cwd))
                                (swap! calls conj args)
                                (when (= "run" (second args))
                                  (str "VIS_PROJECT_SITE=" (pr-str (str packages)) "\n")))}
               (fn []
                 (expect (= (.getCanonicalFile packages) (python-runtime/prepared-project project)))
                 (expect (= ["/bundled/uv" "sync" "--check"] (first @calls)))
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
      (let [current
            (.getCanonicalFile (io/file (System/getProperty "user.dir")))

            elsewhere
            (temp-dir "vis manual uv other")]

        (try (with-redefs-fn {#'python-runtime/bundled-uv! (constantly "/bundled/uv")
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
                                   ", then /reload.")
                              (.getMessage same-dir)))
                   (expect (= (str "uv sync failed\nRun vis-agent python uv sync --project "
                                   (paths/shell-path (.getCanonicalPath elsewhere))
                                   ", then /reload.")
                              (.getMessage other-dir)))
                   (expect (= ::python-runtime/project-sync-required (:type (ex-data same-dir)))))))
             (finally (doseq [file (reverse (file-seq elsewhere))]
                        (io/delete-file file true)))))))

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

(defdescribe automatic-project-preparation-falls-back-after-an-offline-miss
             (it "automatic project preparation falls back after an offline miss"
                 (python-runtime/ensure-library!)
                 (let [project
                       (temp-dir "vis-cold-project")

                       calls
                       (atom [])]

                   (try (with-redefs-fn {#'python-runtime/run-uv!
                                         (fn [_ args]
                                           (swap! calls conj (vec (rest args)))
                                           (when (= 1 (count @calls))
                                             (throw (ex-info "Environment needs sync" {}))))
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
                                       (:stage (first (filter
                                                        #(= (.getName project) (:name %))
                                                        (python-runtime/preparation-status))))))))
                        (finally (doseq [file (reverse (file-seq project))]
                                   (io/delete-file file true)))))))

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
