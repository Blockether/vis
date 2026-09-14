(ns com.blockether.vis.internal.python.cli-environment-test
  "Clean-process coverage: Python imports and editable hooks are process-wide."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.python.runtime :as python-runtime]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [com.blockether.vispython Interpreter]
           [java.io File]
           [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]
           [java.util.concurrent TimeUnit]))

(set! *warn-on-reflection* true)

(defn- delete-tree!
  [^File dir]
  (doseq [^File file (reverse (file-seq dir))]
    (.delete file)))

(defn- run-cli
  [^File dir environment args]
  (let [classpath
        (str/join File/pathSeparator
                  (map (fn [entry]
                         (let [file (io/file entry)]
                           (.getCanonicalPath file)))
                       (str/split (System/getProperty "java.class.path")
                                  (re-pattern (java.util.regex.Pattern/quote File/pathSeparator)))))

        command
        (into [(str (io/file (System/getProperty "java.home") "bin/java"))
               "--enable-native-access=ALL-UNNAMED" "--enable-preview" "-cp" classpath
               "clojure.main" "-m" "com.blockether.vis.core" "python"]
              args)

        output
        (io/file dir (str "cli-" (java.util.UUID/randomUUID) ".log"))

        builder
        (doto (ProcessBuilder. ^java.util.List command)
          (.directory dir)
          (.redirectErrorStream true)
          (.redirectOutput output))]

    (doseq [key ["PYTHONPATH" "UV_PROJECT_ENVIRONMENT" "VIRTUAL_ENV" "VIS_PYTHON_PACKAGES"]]
      (.remove (.environment builder) key))
    (.putAll (.environment builder) environment)
    (let [process (.start builder)]
      (try (.close (.getOutputStream process))
           (when-not (.waitFor process 90 TimeUnit/SECONDS)
             (throw (ex-info "Python CLI timed out" {:output (slurp output)})))
           {:exit (.exitValue process) :output (slurp output)}
           (finally (when (.isAlive process) (.destroyForcibly process)))))))

;; Regression #226: a project must not borrow shared wheels, editable roots,
;; startup hooks or modules imported by those hooks before its environment loads.
(defdescribe
  python-cli-environment-isolation-test
  (it
    "loads a synced project without activating shared packages or editable hooks"
    (let [dir
          (.toFile (Files/createTempDirectory (.toPath (doto (io/file "target") .mkdirs))
                                              "vis-cli-isolation-"
                                              (make-array FileAttribute 0)))

          project
          (doto (io/file dir "project") .mkdirs)

          shared
          (doto (io/file dir "shared") .mkdirs)

          editable
          (doto (io/file dir "editable") .mkdirs)]

      (try
        (spit (io/file project "pyproject.toml")
              (str "[project]\nname = 'cli-isolation-project'\nversion = '0.1.0'\n"
                   "[build-system]\nrequires = ['hatchling']\nbuild-backend = 'hatchling.build'\n"
                   "[tool.hatch.build.targets.wheel]\npackages = ['src/cli_isolation_project']\n"))
        (io/make-parents (io/file project "src/cli_isolation_project/__init__.py"))
        (spit (io/file project "src/cli_isolation_project/__init__.py") "VALUE = 226\n")
        (spit (io/file project "src/cli_isolation_project/__main__.py")
              "from . import VALUE\nprint(VALUE)\n")
        (spit (io/file shared "cli_shared_only.py") "VALUE = 'shared'\n")
        (spit (io/file editable "cli_shared_editable.py") "VALUE = 'editable'\n")
        (spit (io/file shared "shared.pth")
              (str (.getCanonicalPath editable)
                   "\nimport cli_shared_only; print('SHARED_HOOK_RAN')\n"))
        (python-runtime/ensure-library!)
        (#'python-runtime/run-uv!
         project
         [(#'python-runtime/bundled-uv!) "sync" "--python" (Interpreter/pythonExecutable)])
        (let [environment
              {"VIS_PYTHON_PACKAGES" (.getCanonicalPath shared)}

              isolated-code
              (str "import sys, importlib.util\n"
                   "from importlib.metadata import version\n"
                   "from cli_isolation_project import VALUE\n"
                   "assert importlib.util.find_spec('cli_shared_only') is None\n"
                   "assert importlib.util.find_spec('cli_shared_editable') is None\n"
                   "assert 'cli_shared_only' not in sys.modules\n"
                   "print('ISOLATED', VALUE, version('cli-isolation-project'))")]

          (doseq [selection [{} {"UV_PROJECT_ENVIRONMENT" ".venv"}
                             {"UV_PROJECT_ENVIRONMENT" (.getCanonicalPath (io/file project
                                                                                   ".venv"))}]]
            (let [result (run-cli project
                                  (merge environment selection)
                                  ["--no-network" "-c" isolated-code])]
              (expect (= 0 (:exit result)) (:output result))
              (expect (str/includes? (:output result) "ISOLATED 226 0.1.0") (:output result))
              (expect (not (str/includes? (:output result) "SHARED_HOOK_RAN")) (:output result))))
          (let [result (run-cli project environment ["--no-network" "-m" "cli_isolation_project"])]
            (expect (= 0 (:exit result)) (:output result))
            (expect (str/includes? (:output result) "226") (:output result)))
          (let [shadow (doto (io/file project "shadow") .mkdirs)]
            (spit (io/file project "src/cli_priority_fixture.py") "VALUE = 226\n")
            (spit (io/file shadow "cli_priority_fixture.py") "VALUE = 999\n")
            (let [result (run-cli project
                                  (assoc environment "PYTHONPATH" (.getCanonicalPath shadow))
                                  ["--no-network" "-c"
                                   "from cli_priority_fixture import VALUE; print(VALUE)"])]
              (expect (= 0 (:exit result)) (:output result))
              (expect (str/includes? (:output result) "999") (:output result))))
          (doseq [[cwd options] [[project ["--shared"]] [dir []]]]
            (let [result
                  (run-cli
                    cwd
                    environment
                    (into
                      options
                      ["--no-network" "-c"
                       (str "import cli_shared_only, cli_shared_editable, importlib.util\n"
                            "assert importlib.util.find_spec('cli_isolation_project') is None\n"
                            "print('SHARED', cli_shared_only.VALUE, cli_shared_editable.VALUE)")]))]
              (expect (= 0 (:exit result)) (:output result))
              (expect (str/includes? (:output result) "SHARED shared editable") (:output result))))
          (let [missing (doto (io/file dir "missing") .mkdirs)]
            (spit (io/file missing "pyproject.toml")
                  "[project]\nname = 'missing'\nversion = '0.1.0'\n")
            (doseq [selection [{} {"UV_PROJECT_ENVIRONMENT" "nonexistent"}]]
              (let [result (run-cli missing
                                    (merge environment selection)
                                    ["--no-network" "-c" "print('WRONG')"])]
                (expect (not= 0 (:exit result)) (:output result))
                (expect (str/includes? (:output result) "has no site-packages for embedded Python")
                        (:output result))
                (expect (str/includes? (:output result) "vis-agent python uv sync")
                        (:output result))))))
        (finally (delete-tree! dir))))))
