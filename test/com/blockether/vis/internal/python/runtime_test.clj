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
            [com.blockether.vis.internal.python.runtime :as python-runtime]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is testing]])
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

(deftest pip-index-from-vis-yaml-test
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
           (is (= "https://gateway.example.com/simple"
                  (get-in (config/load-config false) [:python :index-url])))
           (is (= 0 (:exit (python-runtime/pip-install! ["pytest==8.4.0"]))))
           (is (= [{} ["--index-url" "https://gateway.example.com/simple" "pytest==8.4.0"]]
                  (last @calls)))
           (spit yml "python:\n  index_url: https://gateway.example.com/other/simple\n")
           (python-runtime/pip-install! ["six"])
           (is (= [{} ["--index-url" "https://gateway.example.com/other/simple" "six"]]
                  (last @calls)))
           (spit yml "python: {}\n")
           (python-runtime/pip-install! ["six"])
           (is (= [{} ["six"]] (last @calls)))
           (is (= 3 @refreshes)))
         (finally (io/delete-file yml true) (io/delete-file dir true)))))

(deftest pip-index-invalid-config-does-not-install-test
  ;; The live config reader is lenient; the install boundary must still refuse
  ;; invalid settings rather than silently installing from a different index.
  (doseq [value [nil "" " " 42 ["https://gateway.example.com/simple"] "--no-index"
                 "https://user:password@gateway.example.com/simple"
                 "https://gateway.example.com/simple?token=fixture"
                 "https://gateway.example.com/simple#fragment"]]
    (let [calls (atom 0)]
      (with-redefs [config/load-config-raw (constantly {"python" {"index_url" value}})
                    runtime/pip-install! (fn [& _]
                                           (swap! calls inc))]

        (is (re-find #"python.index_url"
                     (try (python-runtime/pip-install! ["six"])
                          "no error"
                          (catch clojure.lang.ExceptionInfo e (ex-message e)))))
        (is (zero? @calls))))))

(deftest pip-requests-index-from-vis-yaml-test
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
             (is (= 1 (:exit result)))
             (is (= ["/simple/vis-index-fixture-missing/"] @requests))
             (is (some #{"--only-binary=:all:"} (:command result)))))
         (finally (.stop server 0)
                  (doseq [f (reverse (file-seq dir))]
                    (io/delete-file f true))))))

(deftest manual-uv-publication-test
  (python-runtime/ensure-library!)
  (let [dir
        (temp-dir "vis-manual-uv")

        project
        (doto (io/file dir "project") .mkdirs)

        home
        (io/file dir "prepared")

        packages
        (.getCanonicalFile (io/file dir ".vis/python/packages"))

        failed?
        (atom false)

        calls
        (atom [])

        args
        ["sync" "--project" (str project) "--locked" "--offline"]

        sync-required?
        (fn []
          (= :com.blockether.vis.internal.python.runtime/project-sync-required
             (try (python-runtime/prepared-project project)
                  nil
                  (catch clojure.lang.ExceptionInfo e (:type (ex-data e))))))]

    (spit (io/file project "pyproject.toml") "[project]\nname='fixture'\nversion='1'\n")
    (spit (io/file project "uv.lock") "version = 1\n")
    (.mkdirs packages)
    (spit (io/file packages "unrelated.py") "VALUE = 7\n")
    ;; Regression #178: shared packages unrelated to this project must not stale it.
    (let [metadata (io/file packages "unrelated-1.dist-info/METADATA")]
      (io/make-parents metadata)
      (spit metadata "Name: unrelated\nVersion: 1\n"))
    (try
      (with-redefs-fn {#'python-runtime/project-home (fn [_]
                                                       home)
                       #'runtime/packages-dir (constantly (str packages))
                       #'config/load-config-raw (constantly {})
                       #'python-runtime/uv-sync!
                       (fn [p target options]
                         (swap! calls conj [p target options])
                         (when @failed? (throw (ex-info "simulated sync failure" {})))
                         (spit (io/file target "value.py") "VALUE = 42")
                         (let [metadata (io/file target "fixture-1.dist-info/METADATA")]
                           (io/make-parents metadata)
                           (spit metadata "Name: fixture\nVersion: 1\n")))}
        (fn []
          (is (sync-required?))
          (is (= {:exit 0 :packages (str packages)} (python-runtime/uv-command! args)))
          (is (= packages (python-runtime/prepared-project project))
              "uv and both workers use the one shared packages directory")
          (is (= [[(.getCanonicalFile project) packages ["--offline"]]] @calls))
          (is (not (.exists (io/file project ".venv"))))
          (reset! failed? true)
          (is (= "simulated sync failure"
                 (try (python-runtime/uv-command! args) nil (catch Exception e (.getMessage e)))))
          (is (= packages (python-runtime/prepared-project project)))
          (reset! failed? false)
          (python-runtime/uv-command! args)
          (is (= packages (python-runtime/prepared-project project)))
          (is (.isFile (io/file packages "value.py")))
          (is (.isFile (io/file packages "unrelated.py")))
          (is (not-any? #(.isDirectory ^java.io.File %) (.listFiles home)))
          (spit (io/file packages "unrelated-1.dist-info/METADATA")
                "Name: unrelated\nVersion: 2\n")
          (is (= packages (python-runtime/prepared-project project))
              "An unrelated shared distribution update does not require another sync")
          (let [extra (io/file packages "fixture-2.dist-info/METADATA")]
            (io/make-parents extra)
            (spit extra "Name: fixture\nVersion: 2\n")
            (is (sync-required?) "A second installed version must not preserve stale readiness")
            (io/delete-file extra)
            (io/delete-file (.getParentFile extra)))
          (spit (io/file packages "fixture-1.dist-info/METADATA") "Name: fixture\nVersion: 2\n")
          (is (sync-required?) "A conflicting shared install invalidates the prepared project")
          (python-runtime/uv-command! args)
          (spit (io/file project "uv.lock") "version = 2\n")
          (is (sync-required?))
          (let [before @calls]
            (doseq [invalid [["pip" "install" "x"] ["sync" "--python" "other"] ["sync" "--active"]
                             ["sync" "--project"]]]
              (is (try (python-runtime/uv-command! invalid)
                       false
                       (catch clojure.lang.ExceptionInfo _ true))))
            (is (= before @calls)))))
      (finally (doseq [file (reverse (file-seq dir))]
                 (io/delete-file file true))))))

(deftest automatic-project-preparation-test
  (let [project
        (temp-dir "vis-auto-project")

        ready?
        (atom false)

        calls
        (atom [])]

    (try (with-redefs-fn
           {#'python-runtime/run-uv! (fn [_ args]
                                       (swap! calls conj :resolve)
                                       (is (= ["uv" "lock"] (vec (take 2 args))))
                                       (spit (io/file project "uv.lock") "version = 1"))
            #'python-runtime/prepared-project
            (fn [_]
              (if @ready?
                project
                (throw (ex-info
                         "not ready"
                         {:type
                          :com.blockether.vis.internal.python.runtime/project-sync-required}))))
            #'python-runtime/sync-project! (fn [_ _]
                                             (swap! calls conj :install)
                                             (reset! ready? true))}
           (fn []
             (is (= project (python-runtime/ensure-project! project)))
             (is (= [:resolve :install] @calls))
             (is (= project (python-runtime/ensure-project! project)))
             (is (= [:resolve :install] @calls) "An unchanged project never runs an installer")
             (is (= "version = 1" (slurp (io/file project "uv.lock"))))
             (is (= "ready"
                    (:stage (first (filter #(= (.getName project) (:name %))
                                           (python-runtime/preparation-status))))))))
         (finally (doseq [file (reverse (file-seq project))]
                    (io/delete-file file true))))))
