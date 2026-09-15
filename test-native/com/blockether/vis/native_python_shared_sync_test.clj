(ns com.blockether.vis.native-python-shared-sync-test
  "Shared uv sync through the linked CLI, using only offline fixture packages."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.native-binary-test :as binary]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe
  native-python-shared-sync-test
  (it
    "installs locked wheels and an editable project into the selected shared directory"
    (let [bin
          (#'binary/require-binary)

          dir
          (#'binary/temp-dir "vis-native-shared-sync-")

          project
          (doto (io/file dir "project with spaces") .mkdirs)

          shared
          (doto (io/file dir "shared") .mkdirs)

          environment
          (assoc (#'binary/native-environment)
            "VIS_PYTHON_PACKAGES" (.getAbsolutePath shared)
            "UV_PROJECT_ENVIRONMENT" "untouched-environment")

          run
          (fn [args]
            (#'binary/run-binary project (into [(.getAbsolutePath bin) "python"] args) 120))]

      (try
        (spit
          (io/file project "pyproject.toml")
          (str
            "[project]\nname = 'vis-editable-fixture'\nversion = '0.0.1'\n"
            "requires-python = '>=3.12'\ndependencies = ['vis-cli-fixture==1.0']\n"
            "[build-system]\nrequires = []\nbuild-backend = 'backend'\nbackend-path = ['.']\n"
            "[tool.uv.sources]\nvis-cli-fixture = {path = 'vis_cli_fixture-1.0-py3-none-any.whl'}\n"))
        (io/copy (io/file "test/com/blockether/vis/internal/python/fixtures/editable_backend.py")
                 (io/file project "backend.py"))
        (with-open [out (io/output-stream (io/file project "vis_cli_fixture-1.0-py3-none-any.whl"))]
          (.write out ^bytes (#'binary/pip-wheel)))
        (io/make-parents (io/file project "src/shared_native_project.py"))
        (spit (io/file project "src/shared_native_project.py") "VALUE = 7\n")
        (spit (io/file shared "unrelated.py") "VALUE = 'kept'\n")
        (with-redefs-fn {#'binary/native-environment (constantly environment)}
          (fn []
            (let [help (run ["--shared" "uv" "sync" "--help"])]
              (expect (= 0 (:exit help)) (:output help))
              (expect (str/includes? (:output help) "--all-groups") (:output help)))
            (doseq [options [[] ["--locked" "--all-groups" "--all-extras"]]]
              (let [result (run (into ["--shared" "uv" "sync" "--offline"] options))]
                (expect (= 0 (:exit result)) (:output result))))
            (let [result (run ["--shared" "--no-network" "-c"
                               (str "import shared_native_project, vis_cli_fixture, unrelated\n"
                                    "print('NATIVE_SHARED', shared_native_project.VALUE, "
                                    "vis_cli_fixture.VALUE, unrelated.VALUE)")])]
              (expect (= 0 (:exit result)) (:output result))
              (expect (str/includes? (:output result) "NATIVE_SHARED 7 42 kept")
                      (:output result)))))
        (expect (.isFile (io/file project "uv.lock")))
        (expect (not (.exists (io/file project ".venv"))))
        (expect (not (.exists (io/file project "untouched-environment"))))
        (expect (not-any? #(str/starts-with? (.getName ^java.io.File %) "pylock.")
                          (.listFiles project)))
        (finally (#'binary/delete-tree! dir))))))
