(ns com.blockether.vis.internal.python.cli-environment-test
  "Clean-process coverage: Python imports and editable hooks are process-wide."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.python.runtime :as python-runtime]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [com.blockether.vispython Interpreter]
           [com.sun.net.httpserver HttpExchange HttpHandler HttpServer]
           [java.io File]
           [java.net InetSocketAddress]
           [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]
           [java.util.concurrent TimeUnit]
           [java.util.zip ZipEntry ZipOutputStream]))

(set! *warn-on-reflection* true)

(defn- delete-tree!
  [^File dir]
  (doseq [^File file (reverse (file-seq dir))]
    (.delete file)))

(defn- run-cli
  ([dir environment args] (run-cli dir environment args {}))
  ([^File dir environment args {:keys [tty? invocation-dir]}]
   (let [classpath
         (str/join File/pathSeparator
                   (map (fn [entry]
                          (let [file (io/file entry)]
                            (.getCanonicalPath file)))
                        (str/split (System/getProperty "java.class.path")
                                   (re-pattern (java.util.regex.Pattern/quote
                                                 File/pathSeparator)))))

         command
         (into (cond-> [(str (io/file (System/getProperty "java.home") "bin/java"))
                        "--enable-native-access=ALL-UNNAMED" "--enable-preview"]
                 invocation-dir
                 (conj (str "-Duser.dir=" (.getCanonicalPath ^File invocation-dir)))

                 true
                 (into ["-cp" classpath "clojure.main" "-m" "com.blockether.vis.core" "python"]))
               args)

         command
         (if tty?
           (into ["python3"
                  (.getCanonicalPath
                    (io/file "test-native/com/blockether/vis/fixtures/python_cli_tty.py"))]
                 command)
           command)

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
            (when-not (.waitFor process (if tty? 240 90) TimeUnit/SECONDS)
              (throw (ex-info "Python CLI timed out" {:output (slurp output)})))
            {:exit (.exitValue process) :output (slurp output)}
            (finally (when (.isAlive process)
                       (doseq [^java.lang.ProcessHandle child (-> process
                                                                  .toHandle
                                                                  .descendants
                                                                  .toList)]
                         (.destroyForcibly child))
                       (.destroyForcibly process)
                       (.waitFor process 10 TimeUnit/SECONDS))))))))

(defdescribe
  python-cli-invocation-directory-test
  ;; Regression #237: the source launcher runs in its install directory and
  ;; carries the caller's directory in user.dir, not the process cwd.
  (it
    "preserves cwd and relative paths in code, module and file modes, including --shared"
    (let [dir
          (.getCanonicalFile (.toFile (Files/createTempDirectory (.toPath (doto (io/file "target")
                                                                            .mkdirs))
                                                                 "vis-cli-cwd-"
                                                                 (make-array FileAttribute 0))))

          install
          (doto (io/file dir "install") .mkdirs)

          project
          (doto (io/file dir "project with spaces") .mkdirs)

          shared
          (doto (io/file dir "shared") .mkdirs)

          environment
          {"VIS_PYTHON_PACKAGES" (.getCanonicalPath shared)
           "PYTHONPATH" "."
           "PYTEST_DISABLE_PLUGIN_AUTOLOAD" "1"}

          source
          (str "from pathlib import Path\n" "print('CLI_CWD', Path.cwd())\n"
               "assert str(Path.cwd()) == Path('expected-cwd.txt').read_text()\n"
               "print('RELATIVE_CWD_OK')\n")]

      (try (spit (io/file project "pyproject.toml")
                 (str "[project]\nname = 'cli-cwd-project'\nversion = '0.1.0'\n"
                      "dependencies = ['pytest']\n[tool.uv]\npackage = false\n"))
           (spit (io/file project "expected-cwd.txt") (.getCanonicalPath project))
           (spit (io/file project "cwd_probe.py") source)
           (io/make-parents (io/file project "tests/test_cwd.py"))
           (spit (io/file project "tests/test_cwd.py") "def test_cwd():\n    import cwd_probe\n")
           (python-runtime/ensure-library!)
           (doseq [flags [[] ["--shared"]]]
             (let [synced (run-cli project
                                   environment
                                   (into flags
                                         (cond-> ["uv" "sync"]
                                           (empty? flags)
                                           (into ["--python" (Interpreter/pythonExecutable)]))))]
               (expect (= 0 (:exit synced)) (:output synced))))
           (doseq [flags
                   [[] ["--shared"]]

                   [args expected]
                   [[["-c" source] "RELATIVE_CWD_OK"] [["-m" "cwd_probe"] "RELATIVE_CWD_OK"]
                    [["./cwd_probe.py"] "RELATIVE_CWD_OK"]
                    [[(.getCanonicalPath (io/file project "cwd_probe.py"))] "RELATIVE_CWD_OK"]
                    [["-m" "pytest" "./tests" "-q"] "1 passed"]]]

             (let [result (run-cli install
                                   environment
                                   (into flags (into ["--no-network"] args))
                                   {:invocation-dir project})]
               (expect (= 0 (:exit result)) (str flags " " args "\n" (:output result)))
               (expect (str/includes? (:output result) expected) (:output result))))
           (finally (delete-tree! dir))))))

(defdescribe
  python-cli-main-entrypoint-test
  ;; Regression #287: file mode skipped __main__ and -m missed the invocation cwd.
  (it "runs a file and a module as main without PYTHONPATH"
      (let [dir
            (.toFile (Files/createTempDirectory (.toPath (doto (io/file "target") .mkdirs))
                                                "vis-cli-main-"
                                                (make-array FileAttribute 0)))

            install
            (doto (io/file dir "install") .mkdirs)

            project
            (doto (io/file dir "project") .mkdirs)]

        (try (spit (io/file project "probe_cli.py")
                   (str "print('name:', __name__)\n"
                        "print('file:', __file__)\n"
                        "if __name__ == '__main__':\n    print('ran main')\n"))
             (doseq [args [["-m" "probe_cli"] ["probe_cli.py"]]]
               (let [result
                     (run-cli install {} (into ["--no-network"] args) {:invocation-dir project})]
                 (expect (= 0 (:exit result)) (:output result))
                 (expect (str/includes? (:output result) "name: __main__") (:output result))
                 (expect (re-find #"file: .*probe_cli\.py" (:output result)) (:output result))
                 (expect (str/includes? (:output result) "ran main") (:output result))))
             (finally (delete-tree! dir))))))

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

(defn- shared-fixture-wheel!
  ^File [^File project module]
  (let [dist
        (str module "-1.0.0.dist-info/")

        wheel
        (io/file project (str module "-1.0.0-py3-none-any.whl"))]

    (with-open [zip (ZipOutputStream. (io/output-stream wheel))]
      (doseq [[path text] {(str module ".py") "VALUE = 42\n"
                           (str dist "METADATA")
                           (str "Metadata-Version: 2.1\nName: " module "\nVersion: 1.0.0\n")
                           (str dist "WHEEL")
                           "Wheel-Version: 1.0\nRoot-Is-Purelib: true\nTag: py3-none-any\n"
                           (str dist "RECORD") ""}]
        (.putNextEntry zip (ZipEntry. ^String path))
        (.write zip (.getBytes ^String text "UTF-8"))
        (.closeEntry zip)))
    wheel))

(defdescribe
  python-cli-shared-sync-test
  (it
    "syncs locked editable projects and selected groups into shared packages without pruning"
    (let [dir
          (.getCanonicalFile (.toFile (Files/createTempDirectory (.toPath (doto (io/file "target")
                                                                            .mkdirs))
                                                                 "vis-shared-sync-"
                                                                 (make-array FileAttribute 0))))

          project
          (doto (io/file dir "project with spaces") .mkdirs)

          shared
          (doto (io/file dir "shared packages") .mkdirs)

          environment
          {"VIS_PYTHON_PACKAGES" (.getPath shared)
           "UV_PROJECT_ENVIRONMENT" "untouched-environment"
           "UV_PYTHON" "not-the-embedded-python"}

          sync!
          (fn [& args]
            (run-cli dir
                     environment
                     (into ["--shared" "uv" "sync" "--project" (.getPath project) "--offline"]
                           args)))

          probe!
          (fn [code]
            (run-cli project environment ["--shared" "--no-network" "-c" code]))]

      (try
        (spit
          (io/file project "pyproject.toml")
          (str "[project]\nname = 'vis-editable-fixture'\nversion = '0.0.1'\n"
               "requires-python = '>=3.12'\ndependencies = ['shared-base==1.0.0']\n"
               "[project.optional-dependencies]\nextra = ['shared-extra==1.0.0']\n"
               "[dependency-groups]\ndev = ['shared-dev==1.0.0']\n"
               "[build-system]\nrequires = []\nbuild-backend = 'backend'\nbackend-path = ['.']\n"
               "[tool.uv.sources]\n" "shared-base = {path = 'shared_base-1.0.0-py3-none-any.whl'}\n"
               "shared-dev = {path = 'shared_dev-1.0.0-py3-none-any.whl'}\n"
               "shared-extra = {path = 'shared_extra-1.0.0-py3-none-any.whl'}\n"))
        (io/copy (io/file "test/com/blockether/vis/internal/python/fixtures/editable_backend.py")
                 (io/file project "backend.py"))
        (io/make-parents (io/file project "src/shared_project.py"))
        (spit (io/file project "src/shared_project.py") "VALUE = 7\n")
        (doseq [module ["shared_base" "shared_dev" "shared_extra"]]
          (shared-fixture-wheel! project module))
        (spit (io/file shared "unrelated.py") "VALUE = 'kept'\n")
        (io/make-parents (io/file project ".venv/sentinel"))
        (spit (io/file project ".venv/sentinel") "untouched")
        (let [result (sync!)]
          (expect (= 0 (:exit result)) (:output result)))
        (expect (.isFile (io/file project "uv.lock")))
        (expect (not (.exists (io/file project "untouched-environment"))))
        (expect (= "untouched" (slurp (io/file project ".venv/sentinel"))))
        (let [result
              (probe!
                (str "import shared_project, shared_base, shared_dev, unrelated, importlib.util\n"
                     "assert importlib.util.find_spec('shared_extra') is None\n"
                     "print('SHARED_SYNC', shared_project.VALUE, shared_base.VALUE, "
                     "shared_dev.VALUE, unrelated.VALUE)"))]
          (expect (= 0 (:exit result)) (:output result))
          (expect (str/includes? (:output result) "SHARED_SYNC 7 42 42 kept") (:output result)))
        (let [lock
              (slurp (io/file project "uv.lock"))

              result
              (sync! "--locked" "--all-groups" "--all-extras")]

          (expect (= 0 (:exit result)) (:output result))
          (expect (= lock (slurp (io/file project "uv.lock")))))
        (spit (io/file project "src/shared_project.py") "VALUE = 8\n")
        (let [result (sync! "--frozen" "--no-dev")]
          (expect (= 0 (:exit result)) (:output result)))
        (let [result (probe! (str "import shared_project, shared_extra, shared_dev, unrelated\n"
                                  "print('RETAINED', shared_project.VALUE, shared_extra.VALUE, "
                                  "shared_dev.VALUE, unrelated.VALUE)"))]
          (expect (= 0 (:exit result)) (:output result))
          (expect (str/includes? (:output result) "RETAINED 8 42 42 kept") (:output result)))
        (spit (io/file project "pyproject.toml")
              (str/replace (slurp (io/file project "pyproject.toml"))
                           "requires-python = '>=3.12'"
                           "requires-python = '>=3.13'"))
        (let [lock
              (slurp (io/file project "uv.lock"))

              result
              (sync! "--locked")]

          (expect (not= 0 (:exit result)) (:output result))
          (expect (= lock (slurp (io/file project "uv.lock")))))
        (expect (not-any? #(re-find #"^(requirements.*|pylock.*)\.(txt|toml)$" (.getName ^File %))
                          (.listFiles project)))
        (finally (delete-tree! dir))))))

(defdescribe
  python-cli-shared-workspace-sync-test
  (it
    "discovers the workspace root from a member and supports an empty selection"
    (let [dir
          (.getCanonicalFile (.toFile (Files/createTempDirectory "vis-shared-workspace-"
                                                                 (make-array FileAttribute 0))))

          project
          (doto (io/file dir "workspace") .mkdirs)

          member
          (doto (io/file project "member") .mkdirs)

          shared
          (io/file dir "shared")

          environment
          {"VIS_PYTHON_PACKAGES" (str shared)}

          sync!
          (fn [& options]
            (run-cli dir
                     environment
                     (into ["--shared" "uv" "sync" "--directory" (str member) "--package=member"
                            "--offline"]
                           options)))]

      (try (shared-fixture-wheel! project "shared_base")
           (spit
             (io/file project "pyproject.toml")
             (str
               "[tool.uv.workspace]\nmembers = ['member']\n"
               "[tool.uv.sources]\nshared-base = {path = 'shared_base-1.0.0-py3-none-any.whl'}\n"))
           (spit (io/file member "pyproject.toml")
                 (str "[project]\nname = 'member'\nversion = '0.1.0'\nrequires-python = '>=3.12'\n"
                      "dependencies = ['shared-base']\n[dependency-groups]\nempty = []\n"))
           (let [result (sync!)]
             (expect (= 0 (:exit result)) (:output result)))
           (expect (.isFile (io/file project "uv.lock")))
           (expect (.isFile (io/file shared "shared_base.py")))
           (expect (not (.exists (io/file member "uv.lock"))))
           (expect (not (.exists (io/file project ".venv"))))
           (let [result (sync! "--locked" "--only-group" "empty")]
             (expect (= 0 (:exit result)) (:output result)))
           (expect (.isFile (io/file shared "shared_base.py")))
           (expect (not-any? #(str/starts-with? (.getName ^File %) "pylock.") (file-seq project)))
           (finally (delete-tree! dir))))))

(defdescribe
  python-cli-shared-index-sync-test
  (it
    "keeps named-index artifacts and authentication through both shared sync stages"
    (let [dir
          (.getCanonicalFile (.toFile (Files/createTempDirectory "vis-shared-index-"
                                                                 (make-array FileAttribute 0))))

          project
          (doto (io/file dir "project") .mkdirs)

          shared
          (io/file dir "shared")

          server
          (HttpServer/create (InetSocketAddress. "127.0.0.1" 0) 0)

          requests
          (atom [])

          authorization
          (str "Basic "
               (.encodeToString (java.util.Base64/getEncoder)
                                (.getBytes "fixture:fixture-password" "UTF-8")))]

      (try
        (let [wheel (Files/readAllBytes (.toPath (shared-fixture-wheel! project "shared_base")))]
          (.createContext
            server
            "/"
            (reify
              HttpHandler
                (handle [_ exchange]
                  (let
                    [^HttpExchange exchange exchange
                     path (.getPath (.getRequestURI exchange))
                     authorized? (= authorization
                                    (.getFirst (.getRequestHeaders exchange) "Authorization"))
                     body
                     (cond
                       (not authorized?) (.getBytes "Authentication required" "UTF-8")
                       (= path "/private/simple/shared-base/")
                       (.getBytes
                         "<a href='/private/files/shared_base-1.0.0-py3-none-any.whl'>fixture</a>"
                         "UTF-8")
                       (= path "/private/files/shared_base-1.0.0-py3-none-any.whl") wheel
                       :else (.getBytes "Not found" "UTF-8"))
                     status (cond (not authorized?) 401
                                  (#{"/private/simple/shared-base/"
                                     "/private/files/shared_base-1.0.0-py3-none-any.whl"}
                                   path)
                                  200
                                  :else 404)]

                    (swap! requests conj {:path path :authorized? authorized?})
                    (try (when-not authorized?
                           (.set (.getResponseHeaders exchange)
                                 "WWW-Authenticate"
                                 "Basic realm=fixture"))
                         (.set
                           (.getResponseHeaders exchange)
                           "Content-Type"
                           (if (str/ends-with? path ".whl") "application/octet-stream" "text/html"))
                         (.sendResponseHeaders exchange status (alength ^bytes body))
                         (with-open [out (.getResponseBody exchange)]
                           (.write out ^bytes body))
                         (finally (.close exchange)))))))
          (.start server)
          (let [base (str "http://127.0.0.1:" (.getPort (.getAddress server)))
                environment {"VIS_PYTHON_PACKAGES" (str shared)
                             "UV_DEFAULT_INDEX" (str base "/public/simple")
                             "UV_INDEX_FIXTURE_USERNAME" "fixture"
                             "UV_INDEX_FIXTURE_PASSWORD" "fixture-password"}]

            (spit (io/file project "pyproject.toml")
                  (str "[project]\nname='shared-index-project'\nversion='0.1.0'\n"
                       "requires-python='>=3.12'\ndependencies=['shared-base==1.0.0']\n"
                       "[[tool.uv.index]]\nname='fixture'\nurl='" base
                       "/private/simple'\nexplicit=true\n"
                       "[tool.uv.sources]\nshared-base={index='fixture'}\n"))
            (let [result (run-cli dir
                                  environment
                                  ["--shared" "uv" "sync" "--project" (str project) "--no-cache"])]
              (expect (= 0 (:exit result)) (:output result)))
            (let [result (run-cli
                           project
                           environment
                           ["--shared" "--no-network" "-c"
                            "import shared_base; print('PRIVATE_INDEX', shared_base.VALUE)"])]
              (expect (= 0 (:exit result)) (:output result))
              (expect (str/includes? (:output result) "PRIVATE_INDEX 42") (:output result)))
            (expect (>= (count (filter #(and (:authorized? %)
                                             (= "/private/files/shared_base-1.0.0-py3-none-any.whl"
                                                (:path %)))
                                       @requests))
                        2)
                    (pr-str @requests))
            (expect (every? #(str/starts-with? (:path %) "/private/") @requests)
                    (pr-str @requests))))
        (finally (.stop server 0) (delete-tree! dir))))))

(defdescribe
  python-cli-interactive-input-test
  ;; Regression #229: enter input only after the prompt reaches a real terminal.
  (it "reads delayed terminal input and EOF in both file and module modes"
      (let [dir (.toFile (Files/createTempDirectory (.toPath (doto (io/file "target") .mkdirs))
                                                    "vis-cli-tty-"
                                                    (make-array FileAttribute 0)))]
        (try (let [result (run-cli dir {} [] {:tty? true})]
               (expect (= 0 (:exit result)) (:output result))
               (expect (str/includes? (:output result) "file: interactive input and EOF passed")
                       (:output result))
               (expect (str/includes? (:output result) "module: interactive input and EOF passed")
                       (:output result)))
             (finally (delete-tree! dir))))))
