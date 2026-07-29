(ns com.blockether.vis.internal.python-cli-test
  "End-to-end cover for the `vis python` standalone interpreter helpers
   (`python-cli-context` / `run-python-source!`). Drives the SAME
   `env/*` machinery the native binary runs, so these assertions hold on
   both the JVM and the native image. Boots ONE no-network sandbox for the
   ns (context creation is expensive) and captures the real-terminal
   output by rebinding `config/original-stdout`."
  (:require [com.blockether.vis.internal.config :as config]
            [com.blockether.vis.internal.env-python :as env]
            [com.blockether.vis.internal.main]
            [lazytest.core :refer [defdescribe expect it]]))

(def ^:private python-cli-context #'com.blockether.vis.internal.main/python-cli-context)

(def ^:private run-python-source! #'com.blockether.vis.internal.main/run-python-source!)

(defn- run-src
  "Run one Python block through the CLI helper, capturing the terminal
   output. Returns {:exit code :out captured-stdout}."
  [ctx code]
  (let
    [baos
     (java.io.ByteArrayOutputStream.)

     ps
     (java.io.PrintStream. baos true "UTF-8")]

    (with-redefs [config/original-stdout ps]
      (let [exit (run-python-source! ctx code)]
        {:exit exit :out (.toString baos "UTF-8")}))))

(defdescribe
  python-cli-test
  (let [ctx (python-cli-context {:network? false})]
    (it "runs a basic print block: exit 0, output surfaces"
        (let [{:keys [exit out]} (run-src ctx "print('hi', 1 + 1)")]
          (expect (= 0 exit))
          (expect (re-find #"hi 2" out))))
    (it "a bare trailing expression echoes its repr (CPython-like)"
        (let [{:keys [exit out]} (run-src ctx "40 + 2")]
          (expect (= 0 exit))
          (expect (re-find #"42" out))))
    (it "a raised exception renders the error and exits 1"
        (let [{:keys [exit out]} (run-src ctx "raise ValueError('boom')")]
          (expect (= 1 exit))
          (expect (re-find #"boom" out))))
    (it "state persists across blocks in the same context"
        (run-src ctx "carry = 7")
        (let [{:keys [exit out]} (run-src ctx "print('carry', carry + 1)")]
          (expect (= 0 exit))
          (expect (re-find #"carry 8" out))))
    (it "numpy shim computes"
        (let
          [{:keys [exit out]} (run-src ctx
                                       "import numpy as np\nprint('np', int(np.arange(5).sum()))")]
          (expect (= 0 exit))
          (expect (re-find #"np 10" out))))
    (it "pandas shim computes"
        (let
          [{:keys [exit out]}
           (run-src ctx
                    (str "import pandas as pd\n"
                         "print('pd', int(pd.DataFrame({'a': [1, 2, 3]})['a'].sum()))"))]
          (expect (= 0 exit))
          (expect (re-find #"pd 6" out))))
    (it "sqlite3 shim roundtrips"
        (let
          [{:keys [exit out]}
           (run-src ctx
                    (str "import sqlite3\n"
                         "c = sqlite3.connect(':memory:')\n" "c.execute('create table t(n int)')\n"
                         "c.executemany('insert into t values (?)', [(3,), (4,)])\n"
                         "print('sql', c.execute('select sum(n) from t').fetchone()[0])"))]
          (expect (= 0 exit))
          (expect (re-find #"sql 7" out))))
    (it "yaml shim parses"
        (let
          [{:keys [exit out]} (run-src ctx
                                       (str "import yaml\n"
                                            "d = yaml.safe_load('a: 1\\nb: [2, 3]')\n"
                                            "print('yaml', d['a'], d['b'][1])"))]
          (expect (= 0 exit))
          (expect (re-find #"yaml 1 3" out))))
    (it "http-client shims import clean"
        (let
          [{:keys [exit out]} (run-src ctx
                                       (str "import requests, httpx, bs4, toml, tabulate\n"
                                            "print('imports ok')"))]
          (expect (= 0 exit))
          (expect (re-find #"imports ok" out))))
    (it "a no-network context blocks socket name resolution"
        (let
          [{:keys [exit out]} (run-src ctx
                                       (str "import socket\n" "try:\n"
                                            "    socket.gethostbyname('example.com')\n"
                                            "    print('resolved')\n"
                                            "except Exception:\n" "    print('blocked')"))]
          (expect (= 0 exit))
          (expect (re-find #"blocked" out))))
    (it "a network-enabled context builds without error"
        (expect (some? (python-cli-context {:network? true}))))))

(def ^:private parse-python-cli-args #'com.blockether.vis.internal.main/parse-python-cli-args)

(def ^:private python-cli-env-overrides->map
  #'com.blockether.vis.internal.main/python-cli-env-overrides->map)

(defdescribe
  parse-python-cli-args-test
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
      (let [p (parse-python-cli-args ["--no-network" "--no-env" "--env" "FOO=bar" "-c" "c" "z"])]
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
      (let
        [ctx
         (python-cli-context {:network? false :argv ["-c" "alpha" "beta"]})

         {:keys [exit out]}
         (run-src ctx "import sys\nprint('argv', sys.argv[0], sys.argv[1], sys.argv[2])")]

        (expect (= 0 exit))
        (expect (re-find #"argv -c alpha beta" out))))
  (it "env is merged into os.environ"
      (let
        [ctx
         (python-cli-context {:network? false :env {"VIS_TEST_KEY" "vis-test-val"}})

         {:keys [exit out]}
         (run-src ctx "import os\nprint('env', os.environ.get('VIS_TEST_KEY'))")]

        (expect (= 0 exit))
        (expect (re-find #"env vis-test-val" out))))
  (it "stdin stream is wired to the guest sys.stdin (no hang with -c)"
      (let
        [in
         (java.io.ByteArrayInputStream. (.getBytes "piped-payload\n" "UTF-8"))

         {:keys [python-context]}
         (env/create-python-context {} nil {:enabled? false} in)]

        (try (let
               [{:keys [stdout error]} (env/run-python-block
                                         python-context
                                         "import sys\nprint('stdin', sys.stdin.read().strip())")]
               (expect (nil? error))
               (expect (re-find #"stdin piped-payload" (str stdout))))
             (finally (.close ^org.graalvm.polyglot.Context python-context))))))

(defdescribe
  python-module-exit-test
  (it "preserves a bundled pytest collection failure's non-zero exit status"
      (let
        [dir
         (java.nio.file.Files/createTempDirectory "vis-python-module-exit-"
                                                  (make-array java.nio.file.attribute.FileAttribute
                                                              0))

         test-file
         (.toFile (.resolve dir "test_import.py"))

         _
         (spit test-file "from missing_package import value\n")

         ctx
         (python-cli-context {:network? false :argv ["pytest" (.getAbsolutePath test-file)]})]

        (try (let
               [baos
                (java.io.ByteArrayOutputStream.)

                ps
                (java.io.PrintStream. baos true "UTF-8")

                exit
                (with-redefs [config/original-stdout ps]
                  ((var-get #'com.blockether.vis.internal.main/run-python-module!) ctx "pytest"))]

               (expect (= 1 exit))
               (expect (re-find #"ERROR collecting" (.toString baos "UTF-8"))))
             (finally (.close ^org.graalvm.polyglot.Context ctx)
                      (.delete test-file)
                      (.delete (.toFile dir)))))))

(defdescribe
  python-module-pythonpath-test
  (it
    "uses PYTHONPATH for pytest collection, like a src-layout project"
    (let
      [dir
       (java.nio.file.Files/createTempDirectory "vis-python-pythonpath-"
                                                (make-array java.nio.file.attribute.FileAttribute
                                                            0))

       src
       (.resolve dir "src")

       package
       (.resolve src "sample_project")

       _
       (java.nio.file.Files/createDirectories package
                                              (make-array java.nio.file.attribute.FileAttribute 0))

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

      (try (let
             [baos
              (java.io.ByteArrayOutputStream.)

              ps
              (java.io.PrintStream. baos true "UTF-8")

              exit
              (with-redefs [config/original-stdout ps]
                ((var-get #'com.blockether.vis.internal.main/run-python-module!) ctx "pytest"))]

             (expect (= 0 exit))
             (expect (re-find #"1 passed" (.toString baos "UTF-8"))))
           (finally (.close ^org.graalvm.polyglot.Context ctx)
                    (.delete test-file)
                    (.delete init-file)
                    (.delete (.toFile package))
                    (.delete (.toFile src))
                    (.delete (.toFile dir)))))))

(def ^:private python-project-import-roots
  #'com.blockether.vis.internal.main/python-project-import-roots)

(defn- write-project!
  "Materialise a throwaway project: `pyproject.toml` plus the `dirs` that its
   metadata points at. Returns the project root as a `java.io.File`."
  [pyproject dirs]
  (let
    [root (.toFile (java.nio.file.Files/createTempDirectory
                     "vis-python-srclayout-"
                     (make-array java.nio.file.attribute.FileAttribute 0)))]
    (doseq [d dirs]
      (.mkdirs (java.io.File. root ^String d)))
    (spit (java.io.File. root "pyproject.toml") pyproject)
    root))

(defn- write-files!
  "Materialise a throwaway project from a `name -> content` map, plus the `dirs`
   its metadata points at. Returns the project root as a `java.io.File`."
  [files dirs]
  (let
    [root (.toFile (java.nio.file.Files/createTempDirectory
                     "vis-python-srclayout-"
                     (make-array java.nio.file.attribute.FileAttribute 0)))]
    (doseq [d dirs]
      (.mkdirs (java.io.File. root ^String d)))
    (doseq [[name content] files]
      (spit (java.io.File. root ^String name) content))
    root))

(defdescribe
  python-src-layout-inference-test
  ;; The declarations are read by PYTHON's own `tomllib`/`configparser` inside a
  ;; GraalPy context, so these cases need a live interpreter -- one for the ns.
  (let [ctx (python-cli-context {:network? false})

        roots
        (fn [^java.io.File root]
          (python-project-import-roots ctx (.getCanonicalPath root)))]

    (it "infers the setuptools `where` root, so plain `-m pytest` imports the project"
        (let
          [root (write-project! (str "[project]\nname = \"sample\"\n\n"
                                     "[tool.setuptools.packages.find]\n" "where = [\"src\"]\n\n"
                                     "[tool.pytest.ini_options]\n" "testpaths = [\"tests\"]\n")
                                ["src"])]
          (expect (= [(.getCanonicalPath (java.io.File. root "src"))] (roots root)))))
    (it "infers a poetry `from` root"
        (let
          [root (write-project! (str "[tool.poetry]\nname = \"sample\"\n"
                                     "packages = [{include = \"sample\", from = \"lib\"}]\n")
                                ["lib"])]
          (expect (= [(.getCanonicalPath (java.io.File. root "lib"))] (roots root)))))
    (it "infers the parent of a hatch wheel package path"
        (let
          [root (write-project! (str "[tool.hatch.build.targets.wheel]\n"
                                     "packages = [\"src/sample\"]\n")
                                ["src/sample"])]
          (expect (= [(.getCanonicalPath (java.io.File. root "src"))] (roots root)))))
    (it "stays silent without packaging metadata — inference is declarative only"
        (let [root (write-project! "[project]\nname = \"flat\"\n" ["flat"])]
          (expect (empty? (roots root)))))
    (it "ignores a declared root that does not exist on disk"
        (let
          [root (write-project! (str "[tool.setuptools.packages.find]\n" "where = [\"src\"]\n") [])]
          (expect (empty? (roots root)))))
    (it "reports nothing for a directory without a pyproject.toml"
        (let
          [root (.toFile (java.nio.file.Files/createTempDirectory
                           "vis-python-nopyproject-"
                           (make-array java.nio.file.attribute.FileAttribute 0)))]
          (expect (empty? (roots root)))))
    (it "survives a malformed pyproject.toml instead of scraping it"
        (let [root (write-project! "[tool.setuptools\nwhere = oops\n" ["src"])]
          (expect (empty? (roots root)))))
    (it "infers a setuptools `package-dir` inline table"
        (let
          [root (write-project! (str "[tool.setuptools]\n" "package-dir = {\"\" = \"src\"}\n")
                                ["src"])]
          (expect (= [(.getCanonicalPath (java.io.File. root "src"))] (roots root)))))
    (it "infers a pdm `package-dir` string"
        (let [root (write-project! (str "[tool.pdm.build]\n" "package-dir = \"src\"\n") ["src"])]
          (expect (= [(.getCanonicalPath (java.io.File. root "src"))] (roots root)))))
    (it "honours pytest's own `pythonpath` option in pyproject.toml"
        (let
          [root (write-project! (str "[tool.pytest.ini_options]\n" "pythonpath = [\"lib\"]\n")
                                ["lib"])]
          (expect (= [(.getCanonicalPath (java.io.File. root "lib"))] (roots root)))))
    (it "infers the setup.cfg `package_dir` src layout"
        (let
          [root (write-files! {"setup.cfg" (str "[metadata]\nname = sample\n\n"
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
        (let
          [root (write-project! (str "[tool.setuptools.packages.find]\n" "where = [\"src\"]\n")
                                ["src" "vendor"])]
          (with-redefs [config/load-config-raw (fn [] {"python" {"source_paths" ["vendor"]}})]
            (expect (= [(.getCanonicalPath (java.io.File. root "vendor"))
                        (.getCanonicalPath (java.io.File. root "src"))]
                       (roots root))))))
    (it "drops a configured source path that is not a directory"
        (let [root (write-project! "[project]\nname = \"flat\"\n" [])]
          (with-redefs [config/load-config-raw (fn [] {"python" {"source_paths" ["nope"]}})]
            (expect (empty? (roots root))))))))
