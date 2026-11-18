(ns com.blockether.vis.internal.python.cli-shared-uv-test
  "Clean-process coverage of `vis-agent python --shared uv` and `--shared run`. These
   tests have their own namespace, so that they can run in their own test part."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.python.cli-support :refer
             [cli-queue delete-tree! run-uv-cli shared-fixture-wheel!]]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [com.sun.net.httpserver HttpExchange HttpHandler HttpServer]
           [java.io File]
           [java.net InetSocketAddress]
           [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(set! *warn-on-reflection* true)

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

          ;; The first sync goes through the real `-main` dispatch; the seven later syncs
          ;; skip its engine load. The last child is the import probe, in the project.
          {:keys [invoke! stop!]}
          (cli-queue (concat
                       [{:dir dir :environment environment}]
                       (repeat 7 {:dir dir :environment environment :options {:runtime-only? true}})
                       [{:dir project :environment environment}]))

          sync!
          (fn [& args]
            (invoke! (into ["--shared" "uv" "sync" "--project" (.getPath project) "--offline"]
                           args)))

          probe!
          (fn [code]
            (invoke! ["--shared" "--no-network" "-c" code]))]

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
        ;; The default sync installs the base and the dev group, not the extra.
        ;; The import probe below proves the editable project and the kept file.
        (expect (.isFile (io/file shared "shared_base.py")))
        (expect (.isFile (io/file shared "shared_dev.py")))
        (expect (not (.exists (io/file shared "shared_extra.py"))))
        (expect (= "VALUE = 'kept'\n" (slurp (io/file shared "unrelated.py"))))
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
        (expect (not-any? #(re-find #"^(requirements.*|pylock.*)\.(txt|toml)$" (.getName ^File %))
                          (.listFiles project)))
        (finally (stop!) (delete-tree! dir))))))

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

          ;; python-cli-shared-sync-test proves the `-main` dispatch; skip its engine load here.
          {:keys [invoke! stop!]}
          (cli-queue (repeat 2 {:dir dir :environment environment :options {:runtime-only? true}}))

          sync!
          (fn [& options]
            (invoke! (into ["--shared" "uv" "sync" "--directory" (str member) "--package=member"
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
           (finally (stop!) (delete-tree! dir))))))

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
            (let [result (run-uv-cli dir
                                     environment
                                     ["--shared" "uv" "sync" "--project" (str project)
                                      "--no-cache"])]
              (expect (= 0 (:exit result)) (:output result)))
            (expect (= "VALUE = 42\n" (slurp (io/file shared "shared_base.py"))))
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

(defdescribe
  python-cli-shared-uv-test
  (it
    "adds, removes and installs packages in shared packages without a project environment"
    (let [dir
          (.getCanonicalFile (.toFile (Files/createTempDirectory "vis-shared-uv-"
                                                                 (make-array FileAttribute 0))))

          project
          (doto (io/file dir "project") .mkdirs)

          shared
          (io/file dir "shared")

          environment
          {"VIS_PYTHON_PACKAGES" (str shared) "UV_PYTHON" "not-the-embedded-python"}

          wheel
          (fn [module]
            (str (shared-fixture-wheel! project module)))

          lock
          (io/file project "uv.lock")

          ;; The first invocation goes through the real `-main` dispatch; the eight later
          ;; invocations skip its engine load.
          {:keys [invoke! stop!]}
          (cli-queue
            (cons {:dir dir :environment environment}
                  (repeat 8 {:dir dir :environment environment :options {:runtime-only? true}})))

          uv!
          (fn [& args]
            (invoke! (into ["--shared" "uv"] args)))]

      (try
        (spit (io/file project "pyproject.toml")
              (str "[project]\nname = 'shared-edit-project'\nversion = '0.1.0'\n"
                   "requires-python = '>=3.12'\ndependencies = []\n"))
        (let [result (uv! "add" "--project" (str project) "--offline" (wheel "shared_base"))]
          (expect (= 0 (:exit result)) (:output result)))
        (expect (str/includes? (slurp lock) "name = \"shared-base\""))
        (expect (.isFile (io/file shared "shared_base.py")))
        ;; Upstream uv installs the group that it changed; a plain shared sync does not select it.
        (let [result (uv! "add"
                          "--project" (str project)
                          "--offline" "--group"
                          "lint" (wheel "shared_extra"))]
          (expect (= 0 (:exit result)) (:output result)))
        (expect (.isFile (io/file shared "shared_extra.py")))
        (let [result (uv! "remove" "--project" (str project) "--offline" "shared-base")]
          (expect (= 0 (:exit result)) (:output result)))
        (expect (not (str/includes? (slurp lock) "name = \"shared-base\"")))
        ;; Other projects can use a removed package, so shared packages keep it.
        (expect (.isFile (io/file shared "shared_base.py")))
        (let [result (uv! "pip" "install" "--offline" (wheel "shared_dev"))]
          (expect (= 0 (:exit result)) (:output result)))
        (expect (.isFile (io/file shared "shared_dev.py")))
        (let [result (uv! "pip" "list")]
          (expect (= 0 (:exit result)) (:output result))
          (expect (str/includes? (:output result) "shared-dev") (:output result)))
        (let [result (uv! "pip" "uninstall" "shared-dev")]
          (expect (= 0 (:exit result)) (:output result)))
        (expect (not (.exists (io/file shared "shared_dev.py"))))
        ;; An exact sync deletes the packages of every other project.
        (spit (io/file dir "requirements.txt") "")
        (let [result (uv! "pip" "sync" (str (io/file dir "requirements.txt")))]
          (expect (= 1 (:exit result)) (:output result)))
        (expect (.isFile (io/file shared "shared_base.py")))
        ;; uv refuses --no-sync together with an install selection and with --frozen.
        (let [result (uv! "add"
                          "--project"
                          (str project)
                          "--offline"
                          "--no-install-package"
                          "shared-skip"
                          (wheel "shared_skip")
                          (wheel "shared_kept"))]
          (expect (= 0 (:exit result)) (:output result)))
        (expect (.isFile (io/file shared "shared_kept.py")))
        (expect (not (.exists (io/file shared "shared_skip.py"))))
        (let [result
              (uv! "add" "--project" (str project) "--offline" "--frozen" (wheel "shared_frozen"))]
          (expect (= 0 (:exit result)) (:output result)))
        (expect (str/includes? (slurp (io/file project "pyproject.toml")) "shared-frozen"))
        (expect (not (.exists (io/file shared "shared_frozen.py"))))
        (expect (not (.exists (io/file project ".venv"))))
        (finally (stop!) (delete-tree! dir))))))

(defdescribe
  python-cli-shared-run-test
  (it
    "runs the command of uv run in the embedded Python with shared packages"
    (let [dir
          (.getCanonicalFile (.toFile (Files/createTempDirectory "vis-shared-run-"
                                                                 (make-array FileAttribute 0))))

          project
          (doto (io/file dir "project") .mkdirs)

          shared
          (io/file dir "shared")

          environment
          {"VIS_PYTHON_PACKAGES" (str shared) "UV_PYTHON" "not-the-embedded-python"}

          ;; Each invocation goes through the real `-main` dispatch: the first in `dir`,
          ;; the five later invocations in the project.
          {:keys [invoke! stop!]}
          (cli-queue (cons {:dir dir :environment environment}
                           (repeat 5 {:dir project :environment environment})))]

      (try (shared-fixture-wheel! project "shared_run" "shared-run-tool")
           (spit (io/file project "pyproject.toml")
                 (str "[project]\nname = 'shared-run-project'\nversion = '0.1.0'\n"
                      "requires-python = '>=3.12'\ndependencies = ['shared-run==1.0.0']\n"
                      "[tool.uv.sources]\n"
                      "shared-run = {path = 'shared_run-1.0.0-py3-none-any.whl'}\n"))
           (spit (io/file project "program.py")
                 "import shared_run, sys\nprint('file', shared_run.VALUE, sys.argv[1:])\n")
           (spit (io/file project "uv") "import sys\nprint('uv file', sys.argv[1:])\n")
           ;; The sync before the run installs the project; the script gets each later token.
           (let [result (invoke! ["--shared" "uv" "run" "--project" (str project) "--offline"
                                  "shared-run-tool" "--help" "-V"])]
             (expect (= 5 (:exit result)) (:output result))
             (expect (= "script ['--help', '-V']\n" (:stdout result)) (:output result)))
           (expect (.isFile (io/file shared "shared_run.py")))
           (let [[module program python child script]
                 (mapv #(invoke! (into ["--shared" "uv" "run" "--offline"] %))
                       [["-m" "shared_run" "--help"] ["program.py" "-v" "--gateway="]
                        ["--no-sync" "python" "-c" "import shared_run, sys; sys.exit(3)"]
                        ["--no-sync" "shared-run-tool-program" "--help"]
                        ["--no-sync" "--script" "uv" "--help"]])]
             (expect (= [0 "module ['--help']\n"] [(:exit module) (:stdout module)])
                     (:output module))
             ;; A sync that changes nothing prints nothing.
             (expect (= "" (:stderr module)) (:stderr module))
             ;; A flag of Vis after the command is an argument of the command.
             (expect (= [0 "file 42 ['-v', '--gateway=']\n" ""]
                        [(:exit program) (:stdout program) (:stderr program)])
                     (:output program))
             (expect (= 3 (:exit python)) (:output python))
             ;; A program that is not Python runs as a child process.
             (expect (= [4 "program --help\n"] [(:exit child) (:stdout child)]) (:output child))
             ;; A script with the name `uv` is that file, not bundled uv.
             (expect (= [0 "uv file ['--help']\n"] [(:exit script) (:stdout script)])
                     (:output script)))
           (expect (not (.exists (io/file project ".venv"))))
           (finally (stop!) (delete-tree! dir))))))
