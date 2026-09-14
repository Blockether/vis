(ns com.blockether.vis.native-python-environment-test
  "Regression #226: select one dependency environment before native Python starts."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.native-binary-test :as native]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [java.io File]))

(set! *warn-on-reflection* true)

(defn- write-file!
  [^File dir path text]
  (let [file (io/file dir path)]
    (io/make-parents file)
    (spit file text)))

(defn- run-python
  [{:keys [bin home environment]} ^File cwd overrides args]
  (with-redefs-fn {#'native/native-environment (constantly (merge environment overrides))}
    (fn []
      (#'native/run-binary
       cwd
       (into [(.getAbsolutePath ^File bin) (str "-Duser.home=" home) "python"] args)
       45))))

(defn- with-environment
  [f]
  (let [home
        (#'native/temp-dir "vis-native-python-environment")

        bin
        (#'native/require-binary)

        project
        (doto (io/file home "project") .mkdirs)

        plain
        (doto (io/file home "plain") .mkdirs)

        shared
        (doto (io/file home "shared") .mkdirs)

        editable
        (doto (io/file home "shared-editable") .mkdirs)

        python
        (io/file (.getParentFile ^File (#'native/python-library bin)) "python/bin/python3")

        fixture
        {:home home
         :bin bin
         :project project
         :plain plain
         :shared shared
         :environment (merge (#'native/native-environment)
                             {"VIS_PYTHON_PACKAGES" (.getCanonicalPath shared)
                              "UV_PROJECT_ENVIRONMENT" ""
                              "VIRTUAL_ENV" ""
                              "PYTHONPATH" ""
                              "UV_CACHE_DIR" (str (io/file home "uv-cache"))})}]

    (try
      (write-file!
        project
        "pyproject.toml"
        "[project]\nname = 'native-environment-project'\nversion = '0.1.0'\n[tool.uv]\npackage = false\n")
      (write-file! project "editable-code/native_env_project/__init__.py" "VALUE = 226\n")
      (write-file! project
                   "editable-code/native_env_project/__main__.py"
                   "from . import VALUE\nprint('PROJECT_MODULE', VALUE)\n")
      (write-file! shared "native_env_project.py" "VALUE = 999\n")
      (write-file! shared "native_shared_only.py" "VALUE = 999\n")
      (write-file! editable "native_shared_editable.py" "VALUE = 999\n")
      (write-file! shared
                   "native_shared_preload.py"
                   "import os, native_env_project\nos.environ['VIS_SHARED_HOOK_RAN'] = 'yes'\n")
      (write-file! shared
                   "shared.pth"
                   (str (.getCanonicalPath editable) "\nimport native_shared_preload\n"))
      (let [synced
            (run-python fixture project {} ["uv" "sync" "--offline" "--python" (str python)])]
        (expect (= 0 (:exit synced)) (:output synced)))
      (let [site-result
            (run-python
              fixture
              project
              {}
              ["uv" "run" "--no-sync" "python" "-I" "-c"
               "import sysconfig; print('PROJECT_SITE=' + sysconfig.get_path('purelib'))"])

            site
            (some #(when (str/starts-with? % "PROJECT_SITE=")
                     (io/file (subs % (count "PROJECT_SITE="))))
                  (str/split-lines (:output site-result)))]

        (expect (= 0 (:exit site-result)) (:output site-result))
        (expect (some? site) (:output site-result))
        ;; Installed editable layout: outside inferred src roots, so import success
        ;; proves .pth activation rather than packaging-layout inference.
        (write-file! site
                     "project.pth"
                     (str (.getCanonicalPath (io/file project "editable-code")) "\n"))
        (write-file! site
                     "native_environment_project-0.1.0.dist-info/METADATA"
                     "Metadata-Version: 2.1\nName: native-environment-project\nVersion: 0.1.0\n")
        (f (assoc fixture :site site)))
      (finally (#'native/delete-tree! home)))))

(defdescribe
  native-python-project-isolation-test
  (it
    "loads project editable imports and metadata without shared wheels, hooks or preloaded modules"
    (with-environment
      (fn [{:keys [project] :as fixture}]
        (let [result
              (run-python fixture
                          project
                          {}
                          ["--no-network" "-c"
                           (str
                             "import os, sys, importlib.util\n"
                             "from importlib.metadata import version\n"
                             "from native_env_project import VALUE\n"
                             "assert VALUE == 226\n"
                             "assert importlib.util.find_spec('native_shared_only') is None\n"
                             "assert importlib.util.find_spec('native_shared_editable') is None\n"
                             "assert 'native_shared_preload' not in sys.modules\n"
                             "assert 'VIS_SHARED_HOOK_RAN' not in os.environ\n"
                             "print('PROJECT_ISOLATED', version('native-environment-project'))")])

              module
              (run-python fixture project {} ["--no-network" "-m" "native_env_project"])]

          (expect (= 0 (:exit result)) (:output result))
          (expect (str/includes? (:output result) "PROJECT_ISOLATED 0.1.0") (:output result))
          (expect (= 0 (:exit module)) (:output module))
          (expect (str/includes? (:output module) "PROJECT_MODULE 226") (:output module)))))))

(defdescribe
  native-python-shared-selection-test
  (it "selects shared tools explicitly from a project and by default outside projects"
      (with-environment
        (fn [{:keys [project plain] :as fixture}]
          (doseq [[cwd flags] [[plain []] [project ["--shared"]]]]
            (let [result (run-python
                           fixture
                           cwd
                           {}
                           (into flags
                                 ["--no-network" "-c"
                                  (str "import os, native_shared_only, native_shared_editable\n"
                                       "from native_env_project import VALUE\n"
                                       "assert VALUE == 999\n"
                                       "assert os.environ['VIS_SHARED_HOOK_RAN'] == 'yes'\n"
                                       "print('SHARED_SELECTED')")]))]
              (expect (= 0 (:exit result)) (:output result))
              (expect (str/includes? (:output result) "SHARED_SELECTED") (:output result))))))))

(defdescribe
  native-python-custom-environment-test
  (it "resolves explicit relative and absolute environments without falling back to shared"
      (with-environment
        (fn [{:keys [project] :as fixture}]
          (let [custom (io/file project "custom-env")]
            (expect (.renameTo (io/file project ".venv") custom))
            (doseq [selected ["custom-env" (.getCanonicalPath custom)]]
              (let [result (run-python
                             fixture
                             project
                             {"UV_PROJECT_ENVIRONMENT" selected}
                             ["--no-network" "-c"
                              (str "import importlib.util\nfrom native_env_project import VALUE\n"
                                   "assert VALUE == 226\n"
                                   "assert importlib.util.find_spec('native_shared_only') is None\n"
                                   "print('CUSTOM_SELECTED')")])]
                (expect (= 0 (:exit result)) (:output result))
                (expect (str/includes? (:output result) "CUSTOM_SELECTED") (:output result)))))))))

(defdescribe
  native-python-missing-environment-test
  (it
    "reports missing and incompatible project environments instead of using shared packages"
    (with-environment
      (fn [{:keys [project] :as fixture}]
        (#'native/delete-tree! (io/file project ".venv"))
        (doseq [incompatible? [false true]]
          (when incompatible? (.mkdirs (io/file project ".venv/lib/python0.0/site-packages")))
          (let [result
                (run-python fixture project {} ["--no-network" "-c" "print('UNEXPECTED_SUCCESS')"])]
            (expect (not= 0 (:exit result)) (:output result))
            (expect (not (str/includes? (:output result) "UNEXPECTED_SUCCESS")))
            (expect (str/includes? (:output result) "uv") (:output result))))))))

(defdescribe
  native-python-extension-environment-isolation-test
  ;; Regression #226: a shared .pth can cache the wrong dependency before registration.
  (it
    "boots a declared-project extension without shared packages or preload hooks"
    (with-environment
      (fn [{:keys [bin home plain project environment site]}]
        ;; uv checks declared distributions before loading an extension. This
        ;; non-package fixture keeps its editable path, not CLI-only metadata.
        (#'native/delete-tree! (io/file site "native_environment_project-0.1.0.dist-info"))
        (write-file! home
                     ".vis/extensions/environment_probe.py"
                     (str "# /// script\n# dependencies = []\n# [tool.vis]\n"
                          "# project = '../../project'\n# ///\n"
                          "import importlib.util, os, sys\n"
                          "import blockether.vis.extension as vis\n"
                          "from native_env_project import VALUE\n" "assert VALUE == 226\n"
                          "assert importlib.util.find_spec('native_shared_only') is None\n"
                          "assert importlib.util.find_spec('native_shared_editable') is None\n"
                          "assert importlib.util.find_spec('native_shared_preload') is None\n"
                          "assert 'native_shared_preload' not in sys.modules\n"
                          "assert 'VIS_SHARED_HOOK_RAN' not in os.environ\n"
                          "vis.register_extension(vis.Extension(\n"
                          "    name='native-project-isolated',\n"
                          "    description='Verified project environment and bundled SDK'))\n"))
        (let [lock-before
              (slurp (io/file project "uv.lock"))

              result
              (with-redefs-fn {#'native/native-environment (constantly environment)}
                (fn []
                  (#'native/run-binary
                   plain
                   [(.getAbsolutePath ^File bin) (str "-Duser.home=" home) "extension" "list"]
                   90)))]

          (expect (= 0 (:exit result)) (:output result))
          (expect (str/includes? (:output result) "native-project-isolated") (:output result))
          (expect (= lock-before (slurp (io/file project "uv.lock")))))))))
