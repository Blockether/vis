(ns com.blockether.vis.internal.python.cli-environment-test
  "Clean-process coverage: Python imports and editable hooks are process-wide.
   Each source-launcher JVM loads the whole engine, so independent invocations run
   together and one probe checks every contract an invocation shares."
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [com.blockether.vis.internal.python.cli-support :refer
             [cli-queue delete-tree! run-cli-together runtime-installs shared-fixture-wheel!]]
            [com.blockether.vis.internal.python.runtime :as python-runtime]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [com.blockether.vispython Interpreter]
           [java.io File]
           [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(set! *warn-on-reflection* true)

(defdescribe
  python-cli-invocation-test
  ;; Regression #237: the source launcher runs in its install directory and
  ;; carries the caller's directory in user.dir, not the process cwd.
  ;; Regression #287: file mode skipped __main__ and -m missed the invocation cwd.
  ;; Regression #226: a project must not borrow shared wheels, editable roots,
  ;; startup hooks or modules imported by those hooks before its environment loads.
  ;; The native suite covers custom and missing environments against the binary.
  ;; `-c` and stdin ran in a sandbox module, printed errors to stdout and exited 1.
  ;; pip installed into the runtime, and a child of `sys.executable` knew only the runtime.
  (it
    "runs code, stdin, modules, files and pytest like python in the invocation project"
    (let [dir
          (.getCanonicalFile (.toFile (Files/createTempDirectory (.toPath (doto (io/file "target")
                                                                            .mkdirs))
                                                                 "vis-cli-invocation-"
                                                                 (make-array FileAttribute 0))))

          install
          (doto (io/file dir "install") .mkdirs)

          project
          (doto (io/file dir "project with spaces") .mkdirs)

          shared
          (doto (io/file dir "shared") .mkdirs)

          editable
          (doto (io/file dir "editable") .mkdirs)

          shadow
          (doto (io/file dir "shadow") .mkdirs)

          probe
          (str "import importlib.util\n"
               "from importlib.metadata import version\n" "from pathlib import Path\n"
               "from cli_invocation_project import VALUE\n"
               "assert str(Path.cwd()) == Path('expected-cwd.txt').read_text()\n"
               "assert importlib.util.find_spec('cli_shared_only') is None\n"
               "assert importlib.util.find_spec('cli_shared_editable') is None\n"
               "print('PROBE', __name__, Path(globals().get('__file__', '-')).name, VALUE,"
               " version('cli-invocation-project'))\n")

          invocation
          (fn [args expected & [environment extra]]
            (merge {:args (into ["--no-network"] args)
                    :expected expected
                    :exit 0
                    :stderr ""
                    :environment (merge {"VIS_PYTHON_PACKAGES" (.getCanonicalPath shared)}
                                        environment)
                    :options {:invocation-dir project}}
                   extra))]

      (try
        (spit (io/file project "pyproject.toml")
              (str "[project]\nname = 'cli-invocation-project'\nversion = '0.1.0'\n"
                   "dependencies = ['pytest']\n"
                   "[build-system]\nrequires = ['hatchling']\nbuild-backend = 'hatchling.build'\n"
                   "[tool.hatch.build.targets.wheel]\npackages = ['src/cli_invocation_project']\n"))
        (spit (io/file project "expected-cwd.txt") (.getCanonicalPath project))
        (spit (io/file project "cwd_probe.py") probe)
        (io/make-parents (io/file project "src/cli_invocation_project/__init__.py"))
        (spit (io/file project "src/cli_invocation_project/__init__.py") "VALUE = 226\n")
        (spit (io/file project "src/cli_priority_fixture.py") "VALUE = 226\n")
        (spit (io/file shadow "cli_priority_fixture.py") "VALUE = 999\n")
        (io/make-parents (io/file project "tests/test_cwd.py"))
        (spit (io/file project "tests/test_cwd.py") "def test_cwd():\n    import cwd_probe\n")
        (spit (io/file shared "cli_shared_only.py") "VALUE = 'shared'\n")
        (spit (io/file editable "cli_shared_editable.py") "VALUE = 'editable'\n")
        (spit (io/file shared "shared.pth")
              (str (.getCanonicalPath editable)
                   "\nimport cli_shared_only; print('SHARED_HOOK_RAN')\n"))
        (python-runtime/ensure-library!)
        (#'python-runtime/run-uv!
         project
         [(#'python-runtime/bundled-uv!) "sync" "--python" (Interpreter/pythonExecutable)])
        (let
          [invocations
           [(invocation
              ["-c"
               ;; python -c dedents its code; the host has to ask for that.
               (str/replace
                 (str
                   "import atexit, sys, threading, time\n"
                   probe
                   "from cli_priority_fixture import VALUE as PRIORITY\n"
                   "print('PRIORITY', PRIORITY, sys._getframe().f_code.co_filename)\n"
                   "atexit.register(print, 'AT_EXIT')\n"
                   "threading.Thread(target=lambda: (time.sleep(0.2), print('JOINED'))).start()\n"
                   "import subprocess\n"
                   "subprocess.check_call([sys.executable, '-c',"
                   " \"import cli_invocation_project as p; print('CHILD', p.VALUE)\"])\n"
                   "print('note', file=sys.stderr)\n" "sys.exit(3)\n")
                 #"(?m)^"
                 "    ")]
              ;; Like python: __main__, stderr apart, the program's exit status, and
              ;; non-daemon threads and atexit handlers finish before the process exits.
              ;; A child process of `sys.executable` imports the project too.
              ["PROBE __main__ - 226 0.1.0" "PRIORITY 999 <string>" "CHILD 226" "JOINED" "AT_EXIT"]
              {"PYTHONPATH" (.getCanonicalPath shadow)}
              {:exit 3 :stderr "note\n"})
            (invocation
              ["-" "extra"]
              ["STDIN __main__ <stdin> ['-', 'extra']"]
              nil
              {:input (str
                        "import sys\n"
                        "print('STDIN', __name__, sys._getframe().f_code.co_filename, sys.argv)\n"
                        "sys.exit('stdin exit')\n")
               :exit 1
               :stderr "stdin exit\n"})
            (invocation ["-m" "cwd_probe"] ["PROBE __main__ cwd_probe.py 226 0.1.0"])
            (invocation ["./cwd_probe.py"] ["PROBE __main__ cwd_probe.py 226 0.1.0"])
            (invocation ["-m" "pytest" "./tests" "-q"]
                        ["1 passed"]
                        {"PYTHONPATH" "." "PYTEST_DISABLE_PLUGIN_AUTOLOAD" "1"})
            (invocation ["--shared" "-c"
                         (str "import cli_shared_only, cli_shared_editable, importlib.util\n"
                              "from pathlib import Path\n"
                              "assert str(Path.cwd()) == Path('expected-cwd.txt').read_text()\n"
                              "assert importlib.util.find_spec('cli_invocation_project') is None\n"
                              "print('SHARED', cli_shared_only.VALUE, cli_shared_editable.VALUE)")]
                        ["SHARED shared editable"])
            (invocation ["-m" "pip" "install" "--no-index" "--no-deps"
                         (str (shared-fixture-wheel! dir "cli_project_pip"))]
                        []
                        {"PIP_DISABLE_PIP_VERSION_CHECK" "1"})]]
          (doseq [[{:keys [args expected exit stderr]} result]
                  (map vector invocations (run-cli-together install invocations))
                  :let [message (str args "\n" (:output result))]]

            (expect (= exit (:exit result)) message)
            (expect (= stderr (:stderr result)) message)
            (doseq [text expected]
              (expect (str/includes? (:stdout result) text) message))
            (when-not (some #{"--shared"} args)
              (expect (not (str/includes? (:output result) "SHARED_HOOK_RAN")) message)))
          ;; pip installs into the environment of the project, not into the runtime.
          (expect (some #(= "cli_project_pip.py" (.getName ^File %))
                        (file-seq (io/file project ".venv"))))
          (expect (empty? (runtime-installs "cli_project_pip"))))
        (finally (run! delete-tree! (runtime-installs "cli_project_pip")) (delete-tree! dir))))))

(defdescribe
  python-cli-shared-pip-test
  (it
    "installs and uninstalls with pip in shared packages, which child processes import too"
    (let [dir
          (.getCanonicalFile (.toFile (Files/createTempDirectory "vis-shared-pip-"
                                                                 (make-array FileAttribute 0))))

          shared
          (io/file dir "shared")

          environment
          {"VIS_PYTHON_PACKAGES" (str shared)
           "PIP_NO_INDEX" "1"
           "PIP_DISABLE_PIP_VERSION_CHECK" "1"}

          wheel
          (str (shared-fixture-wheel! dir "shared_base" "shared-base-tool"))

          ;; Seven invocations, each through the real `-main` dispatch.
          {:keys [invoke! stop!]}
          (cli-queue (repeat 7 {:dir dir :environment environment}))

          pip!
          (fn [& args]
            (invoke! (into ["--shared" "-m" "pip"] args)))]

      (try
        ;; The uninstall also runs after a failure: a wrong install must not stay in the runtime.
        (try (let [result (pip! "install" wheel)]
               (expect (= 0 (:exit result)) (:output result))
               (expect (.isFile (io/file shared "shared_base.py")) (:output result))
               ;; The program owns each argument after its code. Its child process imports
               ;; shared packages, and so does the installed script when it runs by itself.
               (let [result (invoke! ["--shared" "-c"
                                      (str "import subprocess, sys\n"
                                           "print('argv', sys.argv[1:], flush=True)\n"
                                           "sys.exit(subprocess.call([sys.executable, '-c',"
                                           " 'import shared_base; print(shared_base.VALUE)']))")
                                      "--help" "--gateway=" "--measure" "-h"])]
                 (expect (= [0 "argv ['--help', '--gateway=', '--measure', '-h']\n42\n"]
                            [(:exit result) (:stdout result)])
                         (:output result)))
               (let [tool (shell/sh (str (io/file shared "bin" "shared-base-tool")) "direct")]
                 (expect (= [5 "script ['direct']\n"] [(:exit tool) (:out tool)]) (:err tool)))
               ;; Regression #481: the pip of a child process also removes a shared package.
               (let [result (invoke! ["--shared" "-c"
                                      (str "import subprocess, sys\n"
                                           "sys.exit(subprocess.call([sys.executable, '-m', 'pip',"
                                           " 'uninstall', '-y', 'shared-base']))")])]
                 (expect (= 0 (:exit result)) (:output result))
                 (expect (not (.exists (io/file shared "shared_base.py"))) (:output result))))
             (finally (let [result (pip! "uninstall" "-y" "shared-base")]
                        (expect (= 0 (:exit result)) (:output result)))))
        (expect (not (.exists (io/file shared "shared_base.py"))))
        ;; pip would delete the runtime's own pip, which the sandbox installs with.
        (let [result (pip! "install" "--upgrade" "pip")]
          (expect (= 1 (:exit result)) (:output result))
          (expect (not (.exists (io/file shared "pip"))) (:output result)))
        ;; A command that only names `install` and pip changes nothing.
        (let [result (pip! "show" "install" "pip")]
          (expect (= 0 (:exit result)) (:output result)))
        ;; `list` can be the value of --log, so `install` can be the command.
        (let [result (pip! "--log" "list" "install" "--dry-run" "pip")]
          (expect (= 1 (:exit result)) (:output result)))
        (finally (stop!) (delete-tree! dir))))))
