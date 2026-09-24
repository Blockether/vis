(ns com.blockether.vis.internal.python.cli-test
  "End-to-end cover for the `vis-agent python` standalone interpreter helpers
   (`python-cli-context` / `run-python-source!`). Drives the SAME
   `env/*` machinery the native binary runs, so these assertions hold on
   both the JVM and the native image. Boots ONE no-network sandbox for the
   ns (context creation is expensive) and captures the real-terminal
   output by rebinding `config/original-stdout`."
  (:require [com.blockether.vis.internal.config.core :as config]
            [com.blockether.vis.internal.python.env :as env]
            [com.blockether.vis.internal.main]
            [com.blockether.vis.internal.python.project]
            [com.blockether.vis.internal.python.test-runner]
            [com.blockether.vis.test-python-context :as tpc]
            [lazytest.core :refer [defdescribe expect it]]))

(def ^:private python-cli-context #'com.blockether.vis.internal.main/python-cli-context)

(def ^:private run-python-source! #'com.blockether.vis.internal.main/run-python-source!)

(defn- capture-cli-run
  "Run a CLI helper, capturing the real terminal output and exit code."
  [run!]
  (with-open [baos
              (java.io.ByteArrayOutputStream.)

              ps
              (java.io.PrintStream. baos true "UTF-8")]

    (with-redefs [config/original-stdout ps]
      {:exit (run!) :out (.toString baos "UTF-8")})))

(defn- run-src
  "Run one Python block through the CLI helper, capturing the terminal
   output. Returns {:exit code :out captured-stdout}."
  [ctx code]
  (capture-cli-run #(run-python-source! ctx code)))

(def ^:private ensure-pytest!
  "Install the real `pytest` for the sandbox once, so a `-m pytest` case runs on a
   machine that has never had it."
  #'com.blockether.vis.internal.python.test-runner/ensure-pytest!)

(defn- scratch-dir!
  "A throwaway directory INSIDE the process's working directory, which is exactly
   what `python-cli-context` roots the interpreter at.

   The CLI interpreter is confined to the directory the human ran it in, so a
   fixture in the system temp folder is a directory the guest genuinely may not
   read — the confinement is the product, not the obstacle."
  ^java.nio.file.Path [^String prefix]
  (let [target (doto (java.io.File. "target") .mkdirs)]
    (java.nio.file.Files/createTempDirectory (.toPath target)
                                             prefix
                                             (make-array java.nio.file.attribute.FileAttribute 0))))

(defn- delete-tree!
  "Delete `dir` and everything under it, deepest entry first."
  [^java.nio.file.Path dir]
  (with-open [walk (java.nio.file.Files/walk dir (make-array java.nio.file.FileVisitOption 0))]
    (doseq [^java.nio.file.Path p (reverse (vec (.toArray (.sorted walk))))]
      (.delete (.toFile p)))))

(defdescribe
  python-cli-test
  (let [ctx (python-cli-context {:network? false})]
    (it "runs a basic print block: exit 0, output surfaces"
        (let [{:keys [exit out]} (run-src ctx "print('hi', 1 + 1)")]
          (expect (= 0 exit))
          (expect (= "hi 2\n" out))))
    (it "a bare trailing expression prints nothing: print is the one channel"
        (let [{:keys [exit out]} (run-src ctx "40 + 2")]
          (expect (= 0 exit))
          (expect (= "" out))))
    (it "a raised exception renders the error and exits 1"
        (let [{:keys [exit out]} (run-src ctx "raise ValueError('boom')")]
          (expect (= 1 exit))
          (expect (re-find #"boom" out))))
    (it "keeps output printed before an exception exactly once"
        ;; Regression #229: live CLI output must not be replayed or lost on error.
        (let [{:keys [exit out]} (run-src ctx "print('before-error'); raise ValueError('boom')")]
          (expect (= 1 exit))
          (expect (re-find #"^before-error\n" out))
          (expect (= 1 (count (re-seq #"(?m)^before-error$" out))))
          (expect (re-find #"boom" out))))
    (it "state persists across blocks in the same context"
        (run-src ctx "carry = 7")
        (let [{:keys [exit out]} (run-src ctx "print('carry', carry + 1)")]
          (expect (= 0 exit))
          (expect (re-find #"carry 8" out))))
    (it "the standard library is CPython's own, not a reimplementation"
        (let [{:keys [exit out]}
              (run-src ctx
                       (str "import sqlite3\n"
                            "c = sqlite3.connect(':memory:')\n"
                            "c.execute('create table t(n int)')\n"
                            "c.executemany('insert into t values (?)', [(3,), (4,)])\n"
                            "print('sql', c.execute('select sum(n) from t').fetchone()[0])"))]
          (expect (= 0 exit))
          (expect (re-find #"sql 7" out))))
    (it "the stdlib arrives whole — the modules a shim never covered import too"
        (let [{:keys [exit out]}
              (run-src ctx
                       (str "import csv, decimal, lzma, secrets, unicodedata, zlib\n"
                            "print('stdlib', unicodedata.name('A'), decimal.Decimal('0.1') + 0)"))]
          (expect (= 0 exit))
          (expect (re-find #"stdlib LATIN CAPITAL LETTER A 0.1" out))))
    ;; The shims are gone: a distribution the sandbox never installed is simply
    ;; not there, and says so in Python's own words instead of answering with a
    ;; reimplementation. With network it would be fetched; this context has none.
    (it "a distribution nobody installed is absent, not imitated"
        (let [{:keys [exit out]} (run-src ctx
                                          (str "try:\n"
                                               "    import pandas\n" "    print('imitated')\n"
                                               "except ImportError as e:\n"
                                               "    print('absent', type(e).__name__)"))]
          (expect (= 0 exit))
          (expect (re-find #"absent ModuleNotFoundError" out))))
    (it "a no-network context with a jail blocks socket name resolution"
        ;; The guard follows the jail, like the filesystem: `vis-agent python`
        ;; runs unjailed and reaches the machine through `shell` anyway, so a
        ;; refusal in Python there would guard nothing. This case states the
        ;; jailed contract, which is the one that means something.
        (tpc/with-own [jailed {} (constantly [(System/getProperty "user.dir")])
                       {:jail-enabled? true :enabled? false}]
                      (let [{:keys [stdout error]}
                            (env/run-python-block jailed
                                                  (str "import socket\n" "try:\n"
                                                       "    socket.gethostbyname('example.com')\n"
                                                       "    print('resolved')\n"
                                                       "except Exception:\n"
                                                       "    print('blocked')"))]
                        (expect (nil? error))
                        (expect (re-find #"blocked" stdout)))))
    (it "a network-enabled context builds without error"
        (expect (some? (python-cli-context {:network? true}))))))

(defdescribe python-cli-reexecution-test
             ;; #199: exercise the same subprocess cases as the built native CLI.
             (it "re-executes bundled CPython with ordinary child import semantics"
                 (let [ctx
                       (python-cli-context {:network? false})

                       {:keys [exit out]}
                       (run-src ctx (slurp "test/resources/python_reexecution.py"))]

                   (expect (= 0 exit) out)
                   (expect (re-find #"python-reexecution-ok" out) out))))

(def ^:private parse-python-cli-args #'com.blockether.vis.internal.main/parse-python-cli-args)

(def ^:private python-cli-env-overrides->map
  #'com.blockether.vis.internal.main/python-cli-env-overrides->map)

(defdescribe explicit-uv-command-test
             (it "routes uv sync separately from Python scripts"
                 (expect (= :uv
                            (:mode (parse-python-cli-args ["uv" "sync" "--project" "einmal"
                                                           "--locked"]))))))

(defdescribe parse-python-cli-args-test
             (it "-c forwards trailing args as sys.argv after the '-c' marker"
                 (let [p (parse-python-cli-args ["-c" "code" "a" "b"])]
                   (expect (= :code (:mode p)))
                   (expect (= "code" (:code p)))
                   (expect (= ["-c" "a" "b"] (:argv p)))))
             (it "a FILE selector keeps the filename as argv[0]"
                 (let [p (parse-python-cli-args ["script.py" "x" "--flag"])]
                   (expect (= :file (:mode p)))
                   (expect (= "script.py" (:file p)))
                   (expect (= ["script.py" "x" "--flag"] (:argv p)))))
             (it "leading --no-network / --no-env / --env are consumed, not argv"
                 (let [p (parse-python-cli-args ["--no-network" "--no-env" "--env" "FOO=bar" "-c"
                                                 "c" "z"])]
                   (expect (false? (:network? p)))
                   (expect (false? (:inherit-env? p)))
                   (expect (= ["FOO=bar"] (:env-overrides p)))
                   (expect (= ["-c" "z"] (:argv p)))))
             (it "-- ends option parsing so a flag-named script arg survives"
                 (let [p (parse-python-cli-args ["--" "-" "--no-network"])]
                   (expect (= :stdin (:mode p)))
                   (expect (= ["-" "--no-network"] (:argv p)))))
             (it "defaults: network + env inherited, interactive with no selector"
                 (let [p (parse-python-cli-args [])]
                   (expect (= :interactive (:mode p)))
                   (expect (true? (:network? p)))
                   (expect (true? (:inherit-env? p)))))
             (it "-m forwards the module as argv[0] with trailing args after it"
                 (let [p (parse-python-cli-args ["-m" "pytest" "tests/" "-q"])]
                   (expect (= :module (:mode p)))
                   (expect (= "pytest" (:module p)))
                   (expect (= ["pytest" "tests/" "-q"] (:argv p)))))
             (it "-m with no module name still parses (module blank, runner rejects later)"
                 (let [p (parse-python-cli-args ["-m"])]
                   (expect (= :module (:mode p)))
                   (expect (nil? (:module p)))
                   (expect (= ["-m"] (:argv p))))))

(defdescribe python-cli-env-overrides-test
             (it "parses K=V, bare K (empty), and keeps later = in the value"
                 (expect (= {"A" "1" "B" "" "C" "x=y"}
                            (python-cli-env-overrides->map ["A=1" "B" "C=x=y"])))))

(defdescribe
  python-cli-runtime-test
  (it "argv is forwarded into sys.argv"
      (let [ctx
            (python-cli-context {:network? false :argv ["-c" "alpha" "beta"]})

            {:keys [exit out]}
            (run-src ctx "import sys\nprint('argv', sys.argv[0], sys.argv[1], sys.argv[2])")]

        (expect (= 0 exit))
        (expect (re-find #"argv -c alpha beta" out))))
  (it "env is merged into os.environ"
      (let [ctx
            (python-cli-context {:network? false :env {"VIS_TEST_KEY" "vis-test-val"}})

            {:keys [exit out]}
            (run-src ctx "import os\nprint('env', os.environ.get('VIS_TEST_KEY'))")]

        (expect (= 0 exit))
        (expect (re-find #"env vis-test-val" out))))
  (it "stdin stream is wired to the guest sys.stdin (no hang with -c)"
      (let [in
            (java.io.ByteArrayInputStream. (.getBytes "piped-payload\n" "UTF-8"))

            {:keys [python-context]}
            (tpc/new-context {} nil {:enabled? false} in)]

        (try (let [{:keys [stdout error]} (env/run-python-block
                                            python-context
                                            "import sys\nprint('stdin', sys.stdin.read().strip())")]
               (expect (nil? error))
               (expect (re-find #"stdin piped-payload" (str stdout))))
             (finally (env/dispose-python-context! python-context))))))

(defdescribe
  python-module-asyncio-test
  (it "lets a synchronous module own its asyncio loop without breaking later blocks"
      ;; JVM/SDK dogfooding: -m pytest failed tests that called asyncio.run.
      (let [dir
            (scratch-dir! "vis-python-module-asyncio-")

            module-file
            (.toFile (.resolve dir "async_cli_probe.py"))

            _
            (spit module-file
                  (str "import asyncio, sys\n" "async def compute():\n"
                       "    await asyncio.sleep(0)\n" "    return 42\n"
                       "print('module-result', asyncio.run(compute()), sys.argv[1])\n"
                       "raise SystemExit(7)\n"))

            ctx
            (python-cli-context {:network? false
                                 :argv ["async_cli_probe" "argument"]
                                 :env {"PYTHONPATH" (.toString dir)}})]

        (try
          (with-open [baos
                      (java.io.ByteArrayOutputStream.)

                      ps
                      (java.io.PrintStream. baos true "UTF-8")]

            (let [exit
                  (with-redefs [config/original-stdout ps]
                    (#'com.blockether.vis.internal.main/run-python-module! ctx "async_cli_probe"))]
              (expect (= 7 exit) (.toString baos "UTF-8"))
              (expect (re-find #"module-result 42 argument" (.toString baos "UTF-8")))))
          (let [{:keys [exit out]}
                (run-src ctx "import asyncio\nawait asyncio.sleep(0)\nprint('await-still-works')")]
            (expect (= 0 exit) out)
            (expect (re-find #"await-still-works" out)))
          (finally (env/dispose-python-context! ctx) (delete-tree! dir))))))

(defdescribe
  python-module-exit-test
  (it "preserves a bundled pytest collection failure's non-zero exit status"
      (let [dir
            (scratch-dir! "vis-python-module-exit-")

            test-file
            (.toFile (.resolve dir "test_import.py"))

            _
            (spit test-file "from missing_package import value\n")

            ctx
            (python-cli-context {:network? false :argv ["pytest" (.getAbsolutePath test-file)]})]

        (try
          (ensure-pytest! ctx)
          (let [baos
                (java.io.ByteArrayOutputStream.)

                ps
                (java.io.PrintStream. baos true "UTF-8")

                exit
                (with-redefs [config/original-stdout ps]
                  ((var-get #'com.blockether.vis.internal.main/run-python-module!) ctx "pytest"))]

            ;; pytest's own code, not a flattened 1: a collection error is
            ;; ExitCode.INTERRUPTED (2), and the point of the case is that the
            ;; module runner hands the guest's status back untouched.
            (expect (= 2 exit))
            (expect (re-find #"ERROR collecting" (.toString baos "UTF-8"))))
          (finally (env/dispose-python-context! ctx) (delete-tree! dir))))))

(defdescribe
  python-module-cwd-test
  (it "loads a module from the invocation directory without PYTHONPATH"
      ;; Regression #287: the embedded interpreter did not put cwd on sys.path.
      (let [file
            (.toFile (java.nio.file.Files/createTempFile
                       (.toPath (java.io.File. "."))
                       "vis_cli_issue_287_"
                       ".py"
                       (make-array java.nio.file.attribute.FileAttribute 0)))

            module
            (subs (.getName file) 0 (- (count (.getName file)) 3))]

        (try (spit file
                   (str "print('module', __name__)\n"
                        "if __name__ == '__main__':\n    print('ran main')\n"))
             (let [ctx (python-cli-context
                         {:network? false :mode :module :argv [module] :env {"PYTHONPATH" ""}})]
               (try (run-src ctx "import sys\nsys.dont_write_bytecode = True")
                    (let [{:keys [exit out]}
                          (capture-cli-run
                            #((var-get #'com.blockether.vis.internal.main/run-python-module!)
                                ctx
                                module))]
                      (expect (= 0 exit))
                      (expect (= "module __main__\nran main\n" out)))
                    (finally (env/dispose-python-context! ctx))))
             (finally (.delete file))))))

(defdescribe
  python-file-main-test
  (it "executes a file as main with its own directory importable"
      ;; Regression #287: evaluating file source in sandbox globals skipped __main__.
      (let [dir
            (scratch-dir! "vis-python-file-main-")

            file
            (.resolve dir "probe_cli.py")

            sibling
            (.resolve dir "sibling_probe.py")]

        (try (spit (.toFile sibling) "VALUE = 42\n")
             (spit (.toFile file)
                   (str "import asyncio, sys\n" "from sibling_probe import VALUE\n"
                        "print('file', __name__, __file__ == sys.argv[0], sys.argv[1], VALUE)\n"
                        "if __name__ == '__main__':\n"
                        "    print('ran main', asyncio.run(asyncio.sleep(0, result=VALUE)))\n"
                        "raise SystemExit(7)\n"))
             (let [ctx (python-cli-context {:network? false
                                            :mode :file
                                            :argv [(.toString file) "argument"]
                                            :env {"PYTHONPATH" ""}})]
               (try (let [{:keys [exit out]}
                          (capture-cli-run #((var-get
                                               #'com.blockether.vis.internal.main/run-python-file!)
                                               ctx
                                               (.toString file)))]
                      (expect (= 7 exit))
                      (expect (= "file __main__ True argument 42\nran main 42\n" out)))
                    (finally (env/dispose-python-context! ctx))))
             (finally (delete-tree! dir))))))

(defdescribe
  python-module-pythonpath-test
  (it
    "uses PYTHONPATH for pytest collection, like a src-layout project"
    (let [dir
          (scratch-dir! "vis-python-pythonpath-")

          src
          (.resolve dir "src")

          package
          (.resolve src "sample_project")

          _
          (java.nio.file.Files/createDirectories package
                                                 (make-array java.nio.file.attribute.FileAttribute
                                                             0))

          init-file
          (.toFile (.resolve package "__init__.py"))

          test-file
          (.toFile (.resolve dir "test_sample_project.py"))

          _
          (spit init-file "VALUE = 42\n")

          _
          (spit test-file
                "from sample_project import VALUE\n\ndef test_value():\n    assert VALUE == 42\n")

          ctx
          (python-cli-context {:network? false
                               :argv ["pytest" (.getAbsolutePath test-file)]
                               :env {"PYTHONPATH" (.toString src)}})]

      (try (ensure-pytest! ctx)
           (let [baos
                 (java.io.ByteArrayOutputStream.)

                 ps
                 (java.io.PrintStream. baos true "UTF-8")

                 exit
                 (with-redefs [config/original-stdout ps]
                   ((var-get #'com.blockether.vis.internal.main/run-python-module!) ctx "pytest"))]

             (expect (= 0 exit))
             (expect (re-find #"1 passed" (.toString baos "UTF-8"))))
           (finally (env/dispose-python-context! ctx) (delete-tree! dir))))))

(def ^:private python-project-import-roots com.blockether.vis.internal.python.project/import-roots)

(defn- write-project!
  "Materialise a throwaway project: `pyproject.toml` plus the `dirs` that its
   metadata points at. Returns the project root as a `java.io.File`."
  [pyproject dirs]
  (let [root (.toFile (scratch-dir! "vis-python-srclayout-"))]
    (doseq [d dirs]
      (.mkdirs (java.io.File. root ^String d)))
    (spit (java.io.File. root "pyproject.toml") pyproject)
    root))

(defn- write-files!
  "Materialise a throwaway project from a `name -> content` map, plus the `dirs`
   its metadata points at. Returns the project root as a `java.io.File`."
  [files dirs]
  (let [root (.toFile (scratch-dir! "vis-python-srclayout-"))]
    (doseq [d dirs]
      (.mkdirs (java.io.File. root ^String d)))
    (doseq [[name content] files]
      (spit (java.io.File. root ^String name) content))
    root))

(defdescribe
  python-src-layout-inference-test
  ;; The declarations are read by PYTHON's own `tomllib`/`configparser` inside the
  ;; interpreter, so these cases need a live one -- one for the ns.
  (let [ctx
        (python-cli-context {:network? false})

        roots
        (fn [^java.io.File root]
          (python-project-import-roots ctx (.getCanonicalPath root)))]

    (it "infers the setuptools `where` root, so plain `-m pytest` imports the project"
        (let [root (write-project! (str "[project]\nname = \"sample\"\n\n"
                                        "[tool.setuptools.packages.find]\n" "where = [\"src\"]\n\n"
                                        "[tool.pytest.ini_options]\n" "testpaths = [\"tests\"]\n")
                                   ["src"])]
          (expect (= [(.getCanonicalPath (java.io.File. root "src"))] (roots root)))))
    (it "infers a poetry `from` root"
        (let [root (write-project! (str "[tool.poetry]\nname = \"sample\"\n"
                                        "packages = [{include = \"sample\", from = \"lib\"}]\n")
                                   ["lib"])]
          (expect (= [(.getCanonicalPath (java.io.File. root "lib"))] (roots root)))))
    (it "infers the parent of a hatch wheel package path"
        (let [root (write-project! (str "[tool.hatch.build.targets.wheel]\n"
                                        "packages = [\"src/sample\"]\n")
                                   ["src/sample"])]
          (expect (= [(.getCanonicalPath (java.io.File. root "src"))] (roots root)))))
    (it "stays silent without packaging metadata — inference is declarative only"
        (let [root (write-project! "[project]\nname = \"flat\"\n" ["flat"])]
          (expect (empty? (roots root)))))
    (it "ignores a declared root that does not exist on disk"
        (let [root (write-project! (str "[tool.setuptools.packages.find]\n" "where = [\"src\"]\n")
                                   [])]
          (expect (empty? (roots root)))))
    (it "reports nothing for a directory without a pyproject.toml"
        (let [root (.toFile (java.nio.file.Files/createTempDirectory
                              "vis-python-nopyproject-"
                              (make-array java.nio.file.attribute.FileAttribute 0)))]
          (expect (empty? (roots root)))))
    (it "survives a malformed pyproject.toml instead of scraping it"
        (let [root (write-project! "[tool.setuptools\nwhere = oops\n" ["src"])]
          (expect (empty? (roots root)))))
    (it "infers a setuptools `package-dir` inline table"
        (let [root (write-project! (str "[tool.setuptools]\n" "package-dir = {\"\" = \"src\"}\n")
                                   ["src"])]
          (expect (= [(.getCanonicalPath (java.io.File. root "src"))] (roots root)))))
    (it "infers a pdm `package-dir` string"
        (let [root (write-project! (str "[tool.pdm.build]\n" "package-dir = \"src\"\n") ["src"])]
          (expect (= [(.getCanonicalPath (java.io.File. root "src"))] (roots root)))))
    (it "honours pytest's own `pythonpath` option in pyproject.toml"
        (let [root (write-project! (str "[tool.pytest.ini_options]\n" "pythonpath = [\"lib\"]\n")
                                   ["lib"])]
          (expect (= [(.getCanonicalPath (java.io.File. root "lib"))] (roots root)))))
    (it "infers the setup.cfg `package_dir` src layout"
        (let [root (write-files! {"setup.cfg" (str "[metadata]\nname = sample\n\n"
                                                   "[options]\npackage_dir =\n    =src\n")}
                                 ["src"])]
          (expect (= [(.getCanonicalPath (java.io.File. root "src"))] (roots root)))))
    (it "honours a whitespace-separated `pythonpath` in pytest.ini"
        (let [root (write-files! {"pytest.ini" "[pytest]\npythonpath = src other\n"}
                                 ["src" "other"])]
          (expect (= [(.getCanonicalPath (java.io.File. root "src"))
                      (.getCanonicalPath (java.io.File. root "other"))]
                     (roots root)))))
    (it "honours `pythonpath` under tox.ini's [pytest] section"
        (let [root (write-files! {"tox.ini" "[pytest]\npythonpath = lib\n"} ["lib"])]
          (expect (= [(.getCanonicalPath (java.io.File. root "lib"))] (roots root)))))
    (it "prepends the configured `python.source_paths`, ahead of what it infers"
        (let [root (write-project! (str "[tool.setuptools.packages.find]\n" "where = [\"src\"]\n")
                                   ["src" "vendor"])]
          (with-redefs [config/load-config-raw (fn []
                                                 {"python" {"source_paths" ["vendor"]}})]
            (expect (= [(.getCanonicalPath (java.io.File. root "vendor"))
                        (.getCanonicalPath (java.io.File. root "src"))]
                       (roots root))))))
    (it "drops a configured source path that is not a directory"
        (let [root (write-project! "[project]\nname = \"flat\"\n" [])]
          (with-redefs [config/load-config-raw (fn []
                                                 {"python" {"source_paths" ["nope"]}})]
            (expect (empty? (roots root))))))))

;; Regression, issue #110: the `vis-agent python` Context wired only `.out`, so
;; guest `sys.stderr` fell through to `System/err` — which `config/init-cli!` has
;; already pointed at vis.log. Every guest traceback, warning and
;; `sys.stderr.write` was silently discarded, pytest's `-s` stderr included.
;; The interpreter owns descriptor 2 itself now, so what this guards is that
;; nothing on the host ever takes it away again.
(defdescribe python-cli-stderr-test
             (it "leaves guest sys.stderr on the process's own descriptor 2"
                 (let [ctx (python-cli-context {:network? false})]
                   (try (let [{:keys [stdout error]}
                              (env/run-python-block
                                ctx
                                (str "import sys\n" "sys.stderr.write('ERR-DIRECT\\n')\n"
                                     "sys.stderr.flush()\n"
                                     "print('stderr fd', sys.stderr.fileno())"))]
                          (expect (nil? error))
                          (expect (re-find #"stderr fd 2" (str stdout))))
                        (finally (env/dispose-python-context! ctx))))))

(defdescribe
  python-cli-project-selection-test
  ;; Regression #226: select before boot; live interpreters cannot change imports.
  (it "selects only the current project and lets shared tools opt out"
      (let [dir
            (scratch-dir! "vis-cli-selection-")

            cwd
            (.toString dir)

            select
            #'com.blockether.vis.internal.main/python-cli-project-environment]

        (try (expect (nil? (select cwd {} false)))
             (spit (java.io.File. cwd "pyproject.toml") "[project]\nname = 'fixture'\n")
             (expect (= (.getCanonicalPath (java.io.File. cwd ".venv")) (select cwd {} false)))
             (expect (= (.getCanonicalPath (java.io.File. cwd "custom"))
                        (select cwd {"UV_PROJECT_ENVIRONMENT" "custom"} false)))
             (expect (nil? (select cwd {"UV_PROJECT_ENVIRONMENT" "custom"} true)))
             (finally (delete-tree! dir)))))
  (it "consumes --shared only before the program selector"
      (expect (true? (:shared? (parse-python-cli-args ["--shared" "-m" "pip" "list"]))))
      (expect (false? (:shared? (parse-python-cli-args ["-c" "print(1)" "--shared"]))))
      (expect (= ["-c" "--shared"] (:argv (parse-python-cli-args ["-c" "print(1)" "--shared"]))))))

(defdescribe
  python-cli-incompatible-environment-test
  (it "reports an unusable environment rather than silently ignoring it"
      (let [dir
            (scratch-dir! "vis-cli-incompatible-")

            ctx
            (python-cli-context {:network? false})]

        (try (.mkdirs (java.io.File. (.toFile dir) ".venv/lib/python0.0/site-packages"))
             (doseq [environment [{} {"UV_PROJECT_ENVIRONMENT" "missing"}]]
               (let [message (try
                               (#'com.blockether.vis.internal.main/activate-python-cli-environment!
                                ctx
                                (.toString dir)
                                environment)
                               "unexpected success"
                               (catch Exception e (.getMessage e)))]
                 (expect (re-find #"has no site-packages for embedded Python" message))
                 (expect (re-find #"uv run --no-sync python" message))))
             (finally (env/dispose-python-context! ctx) (delete-tree! dir))))))
