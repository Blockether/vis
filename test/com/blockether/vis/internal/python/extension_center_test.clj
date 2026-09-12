(ns com.blockether.vis.internal.python.extension-center-test
  "SDK source installer -> uv -> trusted worker -> source reload.
   All storage and dependency indexes are test-owned. No live gateway or PyPI."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.config.core :as config]
            [com.blockether.vis.internal.python.extensions :as pyx]
            [com.blockether.vis.internal.python.extensions-test :as fixtures]
            [com.blockether.vis.internal.python.runtime :as python-runtime]
            [com.blockether.vis.internal.workspace.core :as workspace]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is]])
  (:import [java.io ByteArrayOutputStream]
           [java.net InetSocketAddress]
           [java.nio.charset StandardCharsets]
           [java.util.zip ZipEntry ZipOutputStream]
           [com.sun.net.httpserver HttpServer HttpHandler HttpExchange]))

(defn- zip-bytes
  [entries]
  (let [buffer (ByteArrayOutputStream.)]
    (with-open [zip (ZipOutputStream. buffer)]
      (doseq [[path text] entries]
        (.putNextEntry zip (ZipEntry. ^String path))
        (.write zip (.getBytes ^String text StandardCharsets/UTF_8))
        (.closeEntry zip)))
    (.toByteArray buffer)))

(defn- wheel
  [name version module source]
  (let [dist (str name "-" version ".dist-info/")]
    (zip-bytes {module source
                (str dist "METADATA")
                (str "Metadata-Version: 2.1\nName: " name "\nVersion: " version "\n")
                (str dist "WHEEL") "Wheel-Version: 1.0\nRoot-Is-Purelib: true\nTag: py3-none-any\n"
                (str dist "RECORD") ""})))

(deftest install-subdirectory-prepare-call-and-reload
  (doseq [declarative? [false true]]
    (#'fixtures/with-shared-packages
     (fn [_]
       (#'fixtures/with-fresh-loaded
        {}
        (fn [_ {:keys [ext-dir]}]
          (let
            [index (HttpServer/create (InetSocketAddress. "127.0.0.1" 0) 0)
             wheels {"vis_agent-0.1.0-py3-none-any.whl" (wheel "vis_agent" "0.1.0"
                                                               "sdk_fixture.py" "VALUE = 1\n")
                     "vis_center_dep-1.0.0-py3-none-any.whl"
                     (wheel "vis_center_dep" "1.0.0" "vis_center_dep.py" "VALUE = 42\n")}
             repository (io/file ext-dir "repository")
             source (doto (io/file repository "plugins/greeting") .mkdirs)
             opts {:dirs [(str (io/file ext-dir ".vis/extensions"))]}
             calls (atom [])
             diagnostics (atom [])
             code
             "import vis_center_dep\ndef greet():\n    \"Return the installed dependency value.\"\n    return vis_center_dep.VALUE\n"
             run-uv
             (fn [project args]
               (let [builder (doto (ProcessBuilder. ^java.util.List args)
                               (.directory project)
                               (.redirectErrorStream true))]
                 ;; Upstream uv reads its own settings, not Vis pip configuration.
                 (.put (.environment builder)
                       "UV_DEFAULT_INDEX"
                       (str "http://127.0.0.1:" (.getPort (.getAddress index)) "/simple"))
                 (.put (.environment builder) "UV_CACHE_DIR" (str (io/file ext-dir "uv-cache")))
                 (.put (.environment builder) "UV_PYTHON_DOWNLOADS" "never")
                 (let [process (.start builder)
                       output (slurp (.getInputStream process))
                       exit (.waitFor process)]

                   (swap! diagnostics conj {:exit exit :output output})
                   (when-not (zero? exit) (throw (ex-info "Fixture uv failed" {})))
                   output)))]

            (.createContext
              index
              "/"
              (reify
                HttpHandler
                  (handle [_ exchange]
                    (let [^HttpExchange exchange exchange
                          file (last (str/split (.getPath (.getRequestURI exchange)) #"/"))
                          bytes (or (get wheels file)
                                    (.getBytes
                                      (str/join
                                        ""
                                        (for [name (keys wheels)
                                              :when (str/starts-with?
                                                      name
                                                      (str (str/replace file "-" "_") "-"))]

                                          (str "<a href='/files/" name "'>" name "</a>")))
                                      StandardCharsets/UTF_8))]

                      (.set (.getResponseHeaders exchange)
                            "Content-Type"
                            (if (get wheels file) "application/octet-stream" "text/html"))
                      (.sendResponseHeaders exchange 200 (alength ^bytes bytes))
                      (with-open [out (.getResponseBody exchange)]
                        (.write out ^bytes bytes))
                      (.close exchange)))))
            (.start index)
            (try
              ;; The installer executes Git in its trusted interpreter, not the model sandbox.
              (is (str/starts-with? (#'pyx/package-call "_git('version')") "git version"))
              (spit (io/file source "pyproject.toml")
                    (str "[project]\nname='vis-center-greeter'\nversion='1.0.0'\n"
                         "description='End-to-end greeting tools'\nrequires-python='>=3.11'\n"
                         "dependencies=['vis-agent>=0.1.0','vis-center-dep==1.0.0']\n"
                         "[tool.vis]\ncategory='tools'\nsource_paths=['src']\n"))
              (spit
                (io/file source "extension.py")
                (str
                  "import blockether.vis.extension as vis\nfrom center_logic import greet\n"
                  "vis.register(vis.Extension(name='vis-center-greeter',description='Greeting tools',"
                  "alias='center',symbols=[vis.Symbol(greet)]))\n"))
              (.mkdirs (io/file source "src"))
              (spit (io/file source "src/center_logic.py") code)
              (when-not declarative?
                (is (= "source"
                       (get (pyx/install-package! (str repository)
                                                  {:trust true
                                                   :subdirectory "plugins/greeting"
                                                   :directory (first (:dirs opts))})
                            "mode"))))
              (spit
                (io/file ext-dir "vis.yml")
                "extensions:
  vis-center-greeter:
    source: ./repository
    subdirectory: plugins/greeting
")
              (with-redefs [config/config-dir (constantly (str (io/file ext-dir "global")))
                            workspace/cwd (constantly (str ext-dir))
                            python-runtime/run-uv! (fn [project args]
                                                     (swap! calls conj args)
                                                     (run-uv project args))]

                (let [invoke #(:result ((#'fixtures/symbol-fn
                                         (#'fixtures/registered "vis-center-greeter")
                                         'greet)))]
                  (is (not (.exists (io/file source "uv.lock"))))
                  (when declarative?
                    (is (= ["would-sync"]
                           (mapv #(get % "status") (pyx/sync-packages! {:dry-run true}))))
                    (is (empty? @calls))
                    (let [result (pyx/sync-packages! {:trust true})]
                      (is (= ["installed"] (mapv #(get % "status") result)) (pr-str result))
                      (is (every? #(true? (get % "prepared")) result)))
                    (is (nil? (#'fixtures/registered "vis-center-greeter"))
                        "Sync never imports entrypoints")
                    (is (= ["sync" "sync" "run"] (mapv second @calls)))
                    (reset! calls []))
                  (let [result (pyx/reload-python-extensions! opts)]
                    (is (= 0 (:failed result))
                        (pr-str {:failures (pyx/load-failures) :uv @diagnostics})))
                  (is (= 42 (invoke)))
                  (is (.isFile (io/file source "uv.lock")))
                  (is (.isDirectory (io/file source ".venv")))
                  (is (= (if declarative? ["sync" "run"] ["sync" "sync" "run"])
                         (mapv second @calls))
                      "Cold preparation installs; warm preparation only checks uv's environment")
                  (reset! calls [])
                  (when declarative?
                    (is (= ["cached"] (mapv #(get % "status") (pyx/sync-packages! {:trust true}))))
                    (is (= ["sync" "run"] (mapv second @calls)))
                    (is (= ["--check" "--offline"] (subvec (first @calls) 2 4)))
                    (reset! calls []))
                  (is (= 0 (:failed (pyx/reload-python-extensions! opts))))
                  (is (= ["sync" "run"] (mapv second @calls))
                      "uv owns checking whether an unchanged environment needs updating")
                  (spit (io/file source "src/center_logic.py")
                        (str/replace code
                                     "return vis_center_dep.VALUE"
                                     "return vis_center_dep.VALUE + 1"))
                  (is (= 42 (invoke)))
                  (is (= 0 (:failed (pyx/reload-python-extensions! opts))))
                  (is (= 43 (invoke)))
                  (is (= ["sync" "run" "sync" "run"] (mapv second @calls)))
                  (spit (io/file source "extension.py") "raise ValueError('broken edit')\n")
                  (is (= 1 (:failed (pyx/reload-python-extensions! opts))))
                  (is (= 43 (invoke)))))
              (finally (config/invalidate-config-cache!) (.stop index 0))))))))))

(deftest approved-release-lifecycle-crosses-the-embedded-host
  (#'fixtures/with-shared-packages
   (fn [packages]
     (#'fixtures/with-fresh-loaded
      {}
      (fn [_ {:keys [ext-dir]}]
        (let
          [directory
           (str (io/file ext-dir "extensions"))

           invoke
           #'pyx/package-call

           setup
           (str
             "def _catalog(repository, folder):\n"
             "    return {'releases': [{'repository_url': repository, 'subdirectory': folder,"
             " 'name': 'vis-release-fixture', 'version': version, 'revision': digit * 40,"
             " 'release_tag': 'v' + version} for version, digit in [('1.0.0', 'a'), ('1.1.0', 'b')]]}\n"
             "def _checkout(repository, directory, revision):\n"
             "    selected = directory / 'plugins' / 'greeting'\n"
             "    selected.mkdir(parents=True)\n"
             "    version = '1.0.0' if revision == 'a' * 40 else '1.1.0'\n"
             "    (selected / 'extension.py').write_text("
             (#'pyx/python-string-literal
              "import blockether.vis.extension as vis\nVERSION = SELECTED_VERSION\ndef current_version():\n    \"Return the active release.\"\n    return VERSION\nvis.register(vis.Extension(name=\"vis-release-fixture\", description=\"Release fixture\", alias=\"release\", symbols=[vis.Symbol(current_version, activity=vis.Activity(label=\"Read active release\", show_start=False))]))\n")
             ".replace('SELECTED_VERSION', repr(version)))\n"
             "    (selected / 'pyproject.toml').write_text("
             "'[project]\\nname=\"vis-release-fixture\"\\nversion=\"' + version + '\"\\n'"
             " + 'description=\"Release fixture\"\\nrequires-python=\">=3.11\"\\n'"
             " + 'dependencies=[\"vis-agent>=0.1.0\"]\\n[tool.vis]\\ncategory=\"tools\"\\n')\n"
             "    return revision\n")

           package-call
           @invoke

           options
           {:directory directory :trust true :version "1.0.0" :subdirectory "plugins/greeting"}

           active-version
           #(:result ((#'fixtures/symbol-fn
                       (#'fixtures/registered "vis-release-fixture")
                       'current_version)))

           reload-release
           (fn []
             (let [result (pyx/reload-python-extensions! {:dirs [directory]})]
               (is (= 0 (:failed result))
                   (pr-str {:result result :failures (pyx/load-failures)}))))]

          (with-redefs-fn {invoke (fn [expression]
                                    (package-call (str "exec(" (#'pyx/python-string-literal setup)
                                                       ", install.__globals__) or " expression)))
                           ;; The preceding test covers real uv. Here only dependency preparation
                           ;; and remote transport are replaced; discovery and trusted workers are real.
                           #'python-runtime/ensure-project! (constantly packages)}
            (fn []
              (is (= "1.0.0" (get (pyx/install-package! "example/extensions" options) "version")))
              (reload-release)
              (is (= "1.0.0" (active-version)))
              (let [status (pyx/package-versions "Example/Extensions" {:directory directory})]
                (is (= "1.0.0" (get status "installed")))
                (is (= "1.1.0" (get status "latest")))
                (is (true? (get status "update_available"))))
              (is (= "1.1.0"
                     (get (pyx/update-package! "example/extensions" (dissoc options :version))
                          "version")))
              (is (= "1.0.0" (active-version)) "Installed code changes only after reload")
              (reload-release)
              (is (= "1.1.0" (active-version)))
              (is (= "1.0.0"
                     (get (pyx/rollback-package! "example/extensions" (dissoc options :version))
                          "version")))
              (is (= "1.1.0" (active-version)))
              (reload-release)
              (is (= "1.0.0" (active-version)))
              (is (= 3 (count (.listFiles (io/file directory ".versions" "vis-release-fixture")))))
              (is (.isFile (io/file directory "vis-release-fixture" "extension.py")))))))))))
