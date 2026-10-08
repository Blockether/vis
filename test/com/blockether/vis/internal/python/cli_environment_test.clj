(ns com.blockether.vis.internal.python.cli-environment-test
  "Clean-process coverage: Python imports and editable hooks are process-wide.
   Each source-launcher JVM loads the whole engine, so independent invocations run
   together and one probe checks every contract an invocation shares."
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

(defn- start-cli!
  "Start `vis-agent python` from source like the launcher: the process runs in `dir`
   and an :invocation-dir travels in user.dir. :input is written to its stdin."
  [^File dir environment args {:keys [invocation-dir jvm-opts ^String input]}]
  (let [classpath
        (str/join File/pathSeparator
                  (map (fn [entry]
                         (let [file (io/file entry)]
                           (.getCanonicalPath file)))
                       (str/split (System/getProperty "java.class.path")
                                  (re-pattern (java.util.regex.Pattern/quote File/pathSeparator)))))

        command
        (into (cond-> (into [(str (io/file (System/getProperty "java.home") "bin/java"))
                             "--enable-native-access=ALL-UNNAMED" "--enable-preview"]
                            jvm-opts)
                invocation-dir
                (conj (str "-Duser.dir=" (.getCanonicalPath ^File invocation-dir)))

                true
                (into ["-cp" classpath "clojure.main" "-m" "com.blockether.vis.core" "python"]))
              args)

        log
        (str "cli-" (java.util.UUID/randomUUID))

        output
        (io/file dir (str log ".log"))

        errors
        (io/file dir (str log ".err"))

        builder
        (doto (ProcessBuilder. ^java.util.List command)
          (.directory dir)
          (.redirectError errors)
          (.redirectOutput output))]

    (doseq [key ["PYTHONPATH" "UV_PROJECT_ENVIRONMENT" "VIRTUAL_ENV" "VIS_PYTHON_PACKAGES"]]
      (.remove (.environment builder) key))
    (.putAll (.environment builder) environment)
    (let [process (.start builder)]
      (with-open [stdin (.getOutputStream process)]
        (when input (.write stdin (.getBytes input "UTF-8"))))
      {:process process :output output :errors errors})))

(defn- await-cli!
  "Wait for a started CLI. :output joins stdout and stderr for messages; :stderr omits
   the JVM's JAVA_TOOL_OPTIONS notice (CI sets it), which is not the program's output."
  [{:keys [^Process process output errors]} timeout-seconds]
  (try (when-not (.waitFor process timeout-seconds TimeUnit/SECONDS)
         (throw (ex-info "Python CLI timed out" {:output (str (slurp output) (slurp errors))})))
       (let [stdout
             (slurp output)

             stderr
             (str/replace (slurp errors) #"(?m)^Picked up .*\n?" "")]

         {:exit (.exitValue process) :stdout stdout :stderr stderr :output (str stdout stderr)})
       (finally (when (.isAlive process)
                  (doseq [^java.lang.ProcessHandle child (-> process
                                                             .toHandle
                                                             .descendants
                                                             .toList)]
                    (.destroyForcibly child))
                  (.destroyForcibly process)
                  (.waitFor process 10 TimeUnit/SECONDS)))))

(defn- run-cli [dir environment args] (await-cli! (start-cli! dir environment args {}) 90))

(defn- run-cli-together
  "Run independent invocations at once. C1 and the serial collector roughly halve
   each short-lived JVM's CPU, which is what several of them contend for."
  [dir invocations]
  (let [started (mapv (fn [{:keys [environment args options input]}]
                        (start-cli! dir
                                    environment
                                    args
                                    (assoc options
                                      :input input
                                      :jvm-opts ["-XX:TieredStopAtLevel=1" "-XX:+UseSerialGC"])))
                      invocations)]
    (mapv #(await-cli! % 240) started)))

(defdescribe
  python-cli-invocation-test
  ;; Regression #237: the source launcher runs in its install directory and
  ;; carries the caller's directory in user.dir, not the process cwd.
  ;; Regression #287: file mode skipped __main__ and -m missed the invocation cwd.
  ;; Regression #226: a project must not borrow shared wheels, editable roots,
  ;; startup hooks or modules imported by those hooks before its environment loads.
  ;; The native suite covers custom and missing environments against the binary.
  ;; `-c` and stdin ran in a sandbox module, printed errors to stdout and exited 1.
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
                   "import atexit, sys, threading, time\n" probe
                   "from cli_priority_fixture import VALUE as PRIORITY\n"
                   "print('PRIORITY', PRIORITY, sys._getframe().f_code.co_filename)\n"
                   "atexit.register(print, 'AT_EXIT')\n"
                   "threading.Thread(target=lambda: (time.sleep(0.2), print('JOINED'))).start()\n"
                   "print('note', file=sys.stderr)\n" "sys.exit(3)\n")
                 #"(?m)^"
                 "    ")]
              ;; Like python: __main__, stderr apart, the program's exit status, and
              ;; non-daemon threads and atexit handlers finish before the process exits.
              ["PROBE __main__ - 226 0.1.0" "PRIORITY 999 <string>" "JOINED" "AT_EXIT"]
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
                        ["SHARED shared editable"])]]
          (doseq [[{:keys [args expected exit stderr]} result]
                  (map vector invocations (run-cli-together install invocations))
                  :let [message (str args "\n" (:output result))]]

            (expect (= exit (:exit result)) message)
            (expect (= stderr (:stderr result)) message)
            (doseq [text expected]
              (expect (str/includes? (:stdout result) text) message))
            (when-not (some #{"--shared"} args)
              (expect (not (str/includes? (:output result) "SHARED_HOOK_RAN")) message))))
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
        ;; Issue #340: `--check` passes while the locked packages are installed, also
        ;; with unrelated shared packages. It fails when a locked package is missing.
        (let [result (sync! "--check")]
          (expect (= 0 (:exit result)) (:output result)))
        (delete-tree! (io/file shared "shared_base-1.0.0.dist-info"))
        (let [result (sync! "--dry-run")]
          (expect (= 0 (:exit result)) (:output result))
          (expect (str/includes? (:output result) "shared-base") (:output result))
          (expect (not (.exists (io/file shared "shared_base-1.0.0.dist-info")))))
        (let [result (sync! "--check")]
          (expect (= 1 (:exit result)) (:output result))
          (expect (str/includes? (:output result) "shared-base") (:output result)))
        (let [result (sync!)]
          (expect (= 0 (:exit result)) (:output result)))
        (let [result (sync! "--check")]
          (expect (= 0 (:exit result)) (:output result)))
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
            ;; Extension dependencies may also consult the caller's default index.
            (expect (not-any? #(and (str/starts-with? (:path %) "/public/")
                                    (re-find #"shared[-_]base" (:path %)))
                              @requests)
                    (pr-str @requests))))
        (finally (.stop server 0) (delete-tree! dir))))))
