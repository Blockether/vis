(ns com.blockether.vis.native-uv-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.native-binary-test :as native]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe
  native-canonical-uv-test
  (it
    "preserves upstream help, version, argument errors, output and exit status (#183)"
    (let [dir
          (#'native/temp-dir "vis-native-canonical-uv")

          bin
          (#'native/require-binary)

          home
          (io/file (.getParentFile (#'native/python-library bin)) "python")

          uv
          (io/file home "bin/uv")

          environment
          (merge (#'native/native-environment) {"PATH" "/nonexistent" "HOME" (str dir)})

          direct
          [(str uv)]

          wrapped
          [(.getAbsolutePath bin) (str "-Duser.home=" dir) "python" "uv"]]

      (try
        (expect (.canExecute uv))
        (with-redefs-fn {#'native/native-environment (constantly environment)}
          (fn []
            (doseq
              [args
               [["--version"] ["sync" "--help"] ["pip" "--help"]
                ["sync" "--unsupported-vis-test-option"]
                ["run" "--no-project" "--offline" "--python" (str (io/file home "bin/python3"))
                 "python" "-c"
                 "import sys; print('UV_STDOUT'); print('UV_STDERR', file=sys.stderr); print(sys.argv[1:]); sys.exit(23)"
                 "--gateway" "--debug" "--measure" "--jfr" "--stream-trace" "--version" "--help"]]]
              (let [expected (#'native/run-binary dir (into direct args) 30)
                    actual (#'native/run-binary dir (into wrapped args) 30)]

                (expect (= (:exit expected) (:exit actual)) (:output actual))
                (expect (= (:output expected) (:output actual)))))))
        (finally (#'native/delete-tree! dir))))))

(defdescribe
  native-uv-stdio-test
  (it
    "inherits stdin and keeps stdout and stderr separate without rewriting environment (#183)"
    (let
      [dir
       (#'native/temp-dir "vis-native-uv-stdio")

       bin
       (#'native/require-binary)

       home
       (io/file (.getParentFile (#'native/python-library bin)) "python")

       input
       (io/file dir "input")

       output
       (io/file dir "output")

       error
       (io/file dir "error")

       command
       [(.getAbsolutePath bin) (str "-Duser.home=" dir) "python" "uv" "run" "--no-project"
        "--offline" "--python" (str (io/file home "bin/python3")) "python" "-c"
        "import os, sys; print(os.environ['UV_VIS_FIXTURE'] + ':' + sys.stdin.read(), end=''); print('UV_STDERR', file=sys.stderr); sys.exit(23)"]]

      (try (spit input "UV_STDIN\n")
           (let [builder
                 (doto (ProcessBuilder. ^java.util.List command)
                   (.directory dir)
                   (.redirectInput input)
                   (.redirectOutput output)
                   (.redirectError error))

                 _
                 (.putAll (.environment builder)
                          (merge
                            (#'native/native-environment)
                            {"PATH" "/nonexistent" "HOME" (str dir) "UV_VIS_FIXTURE" "UV_ENV"}))

                 process
                 (.start builder)]

             (try (expect (.waitFor process 30 java.util.concurrent.TimeUnit/SECONDS))
                  (expect (= 23 (.exitValue process)))
                  (expect (= "UV_ENV:UV_STDIN\n" (slurp output)))
                  (expect (= "UV_STDERR\n" (slurp error)))
                  (finally (when (.isAlive process) (.destroyForcibly process)))))
           (finally (#'native/delete-tree! dir))))))

(defn- built-wheel
  []
  (let [out (java.io.ByteArrayOutputStream.)]
    (with-open [zip (java.util.zip.ZipOutputStream. out)]
      (doseq [[name text] {"vis_built_fixture.py" "VALUE = 84\n"
                           "vis_built_fixture-1.0.dist-info/METADATA"
                           "Metadata-Version: 2.1\nName: vis-built-fixture\nVersion: 1.0\n"
                           "vis_built_fixture-1.0.dist-info/WHEEL"
                           "Wheel-Version: 1.0\nRoot-Is-Purelib: true\nTag: py3-none-any\n"
                           "vis_built_fixture-1.0.dist-info/RECORD" ""}]
        (.putNextEntry zip (java.util.zip.ZipEntry. ^String name))
        (.write zip (.getBytes ^String text java.nio.charset.StandardCharsets/UTF_8))
        (.closeEntry zip)))
    (.toByteArray out)))

(defdescribe
  native-private-index-build-test
  (it
    "uses a named authenticated index for locked artifacts and isolated build requirements (#183)"
    (let [dir
          (#'native/temp-dir "vis-native-private-index")

          bin
          (#'native/require-binary)

          home
          (io/file (.getParentFile (#'native/python-library bin)) "python")

          project
          (doto (io/file dir "project") .mkdirs)

          built
          (doto (io/file dir "built") .mkdirs)

          wheel-name
          "vis_cli_fixture-1.0-py3-none-any.whl"

          built-name
          "vis_built_fixture-1.0-py3-none-any.whl"

          wheel
          (#'native/pip-wheel)

          requests
          (atom [])

          server
          (com.sun.net.httpserver.HttpServer/create (java.net.InetSocketAddress. "127.0.0.1" 0) 0)

          auth
          (str "Basic "
               (.encodeToString (java.util.Base64/getEncoder)
                                (.getBytes "fixture-user:fixture-password"
                                           java.nio.charset.StandardCharsets/UTF_8)))]

      (try
        (.createContext
          server
          "/"
          (reify
            com.sun.net.httpserver.HttpHandler
              (handle [_ exchange]
                (let [^com.sun.net.httpserver.HttpExchange exchange
                      exchange

                      path
                      (.getPath (.getRequestURI exchange))

                      authenticated?
                      (= auth (.getFirst (.getRequestHeaders exchange) "Authorization"))

                      artifact?
                      (= path (str "/files/" wheel-name))

                      body
                      (if (and authenticated? artifact?)
                        wheel
                        (.getBytes (if authenticated?
                                     (str "<a href='/files/" wheel-name "'>fixture</a>")
                                     "Authentication required")
                                   java.nio.charset.StandardCharsets/UTF_8))]

                  (swap! requests conj {:path path :authenticated? authenticated?})
                  (when-not authenticated?
                    (.set (.getResponseHeaders exchange) "WWW-Authenticate" "Basic realm=fixture"))
                  (.set (.getResponseHeaders exchange)
                        "Content-Type"
                        (if artifact? "application/octet-stream" "text/html"))
                  (.sendResponseHeaders
                    exchange
                    (if authenticated? 200 401)
                    (if (= "HEAD" (.getRequestMethod exchange)) -1 (alength ^bytes body)))
                  (when-not (= "HEAD" (.getRequestMethod exchange))
                    (with-open [out (.getResponseBody exchange)]
                      (.write out ^bytes body)))
                  (.close exchange)))))
        (.start server)
        (let [index
              (str "http://127.0.0.1:" (.getPort (.getAddress server)) "/simple")

              environment
              (merge (#'native/native-environment)
                     {"HOME" (str dir)
                      "PATH" "/nonexistent"
                      "UV_CACHE_DIR" (str (io/file dir "cache"))
                      "UV_NO_CACHE" "1"
                      "UV_INDEX_FIXTURE_USERNAME" "fixture-user"
                      "UV_INDEX_FIXTURE_PASSWORD" "fixture-password"})]

          (spit
            (io/file project "pyproject.toml")
            (str
              "[project]\nname = 'private-index-project'\nversion = '1.0'\n"
              "requires-python = '>=3.12'\ndependencies = ['vis-cli-fixture==1.0', 'vis-built-fixture==1.0']\n"
              "[tool.uv.sources]\nvis-cli-fixture = {index = 'fixture'}\n"
              "vis-built-fixture = {path = '../built'}\n"
              "[[tool.uv.index]]\nname = 'fixture'\nurl = '"
              index
              "'\ndefault = true\n"))
          (spit (io/file built "pyproject.toml")
                (str "[project]\nname = 'vis-built-fixture'\nversion = '1.0'\n"
                     "[build-system]\nrequires = ['vis-cli-fixture==1.0']\n"
                     "build-backend = 'backend'\nbackend-path = ['.']\n"))
          (with-open [out (io/output-stream (io/file built built-name))]
            (.write out ^bytes (built-wheel)))
          (spit
            (io/file built "backend.py")
            (str
              "def build_wheel(wheel_directory, config_settings=None, metadata_directory=None):\n"
              "    import shutil, vis_cli_fixture\n    from pathlib import Path\n"
              "    assert vis_cli_fixture.VALUE == 42\n"
              "    name = '"
              built-name
              "'\n"
              "    shutil.copyfile(Path(__file__).with_name(name), Path(wheel_directory) / name)\n"
              "    return name\n"))
          (with-redefs-fn {#'native/native-environment (constantly environment)}
            (fn []
              (let [locked (#'native/run-binary
                            dir
                            [(str (io/file home "bin/uv")) "lock" "--project" (str project)
                             "--python" (str (io/file home "bin/python3")) "--no-python-downloads"]
                            60)]
                (expect (= 0 (:exit locked)) (:output locked)))
              (let [lock-before
                    (slurp (io/file project "uv.lock"))

                    _
                    (reset! requests [])

                    synced
                    (#'native/run-binary
                     dir
                     [(.getAbsolutePath bin) (str "-Duser.home=" dir) "python" "uv" "sync"
                      "--project" (str project) "--locked" "--no-cache" "--python"
                      (str (io/file home "bin/python3"))]
                     120)]

                (expect (= 0 (:exit synced)) (:output synced))
                (expect (= lock-before (slurp (io/file project "uv.lock"))))
                (expect (some #(and (:authenticated? %) (= (:path %) (str "/files/" wheel-name)))
                              @requests))
                (let [probe (#'native/run-binary
                             dir
                             [(.getAbsolutePath bin) (str "-Duser.home=" dir) "python" "uv" "run"
                              "--project" (str project) "--no-sync" "python" "-c"
                              "import vis_built_fixture; print(vis_built_fixture.VALUE)"]
                             30)]
                  (expect (= 0 (:exit probe)) (:output probe))
                  (expect (= "84" (str/trim (:output probe)))))
                (expect (not (.exists (io/file dir ".vis/python/packages"))))
                (expect (.isDirectory (io/file project ".venv")))))))
        (finally (.stop server 0) (#'native/delete-tree! dir))))))

(defdescribe
  native-uv-sync-parity-test
  (it
    "uses upstream locks, default groups, custom environments and exact sync cleanup (#183)"
    (let
      [dir
       (#'native/temp-dir "vis-native-uv-sync")

       bin
       (#'native/require-binary)

       home
       (io/file (.getParentFile (#'native/python-library bin)) "python")

       python
       (str (io/file home "bin/python3"))

       projects
       (mapv #(doto (io/file dir %) .mkdirs) ["direct" "wrapped"])

       prefixes
       [[(str (io/file home "bin/uv"))]
        [(.getAbsolutePath bin) (str "-Duser.home=" dir) "python" "uv"]]

       environment
       (merge (#'native/native-environment)
              {"HOME" (str dir) "PATH" "/nonexistent" "UV_PROJECT_ENVIRONMENT" ".fixture-env"})

       manifest
       (fn [version]
         (str
           "[project]\nname='parity-project'\nversion='" version
           "'\n" "requires-python='>=3.12'\n"
           "[dependency-groups]\ndev=['vis-cli-fixture==1.0']\n"
           "[tool.uv.sources]\nvis-cli-fixture={path='../vis_cli_fixture-1.0-py3-none-any.whl'}\n"))

       run-both
       (fn [args exit]
         (mapv (fn [project prefix]
                 (let [result (#'native/run-binary
                               dir
                               (into prefix (concat ["--directory" (str project)] args))
                               60)]
                   (expect (= exit (:exit result)) (:output result))
                   result))
               projects
               prefixes))]

      (try
        (with-open [out (io/output-stream (io/file dir "vis_cli_fixture-1.0-py3-none-any.whl"))]
          (.write out ^bytes (#'native/pip-wheel)))
        (with-open [out (io/output-stream (io/file dir "vis_built_fixture-1.0-py3-none-any.whl"))]
          (.write out ^bytes (built-wheel)))
        (doseq [project projects]
          (spit (io/file project "pyproject.toml") (manifest "1.0")))
        (with-redefs-fn {#'native/native-environment (constantly environment)}
          (fn []
            (run-both ["sync" "--offline" "--python" python] 0)
            (doseq [project projects]
              (expect (.isFile (io/file project "uv.lock")))
              (expect (.isDirectory (io/file project ".fixture-env")))
              (expect (not (.exists (io/file project ".venv")))))
            (expect (apply = (map #(slurp (io/file % "uv.lock")) projects)))
            (run-both ["run" "--no-sync" "python" "-c"
                       "import vis_cli_fixture; assert vis_cli_fixture.VALUE == 42"]
                      0)
            (let [locks (mapv #(slurp (io/file % "uv.lock")) projects)]
              (doseq [project projects]
                (spit (io/file project "pyproject.toml") (manifest "2.0")))
              (run-both ["sync" "--offline" "--python" python "--locked"] 2)
              (expect (= locks (mapv #(slurp (io/file % "uv.lock")) projects)))
              (run-both ["sync" "--offline" "--python" python] 0)
              (expect (every? true? (map not= locks (map #(slurp (io/file % "uv.lock")) projects))))
              (expect (apply = (map #(slurp (io/file % "uv.lock")) projects))))
            (run-both ["pip" "install" "--offline" "--python" ".fixture-env/bin/python"
                       "../vis_built_fixture-1.0-py3-none-any.whl"]
                      0)
            (run-both ["sync" "--offline" "--python" python "--no-dev"] 0)
            (run-both
              ["run" "--no-sync" "python" "-c"
               "import importlib.util; assert importlib.util.find_spec('vis_cli_fixture') is None; assert importlib.util.find_spec('vis_built_fixture') is None"]
              0)
            (expect (not (.exists (io/file dir ".vis/python/packages"))))))
        (finally (#'native/delete-tree! dir))))))
