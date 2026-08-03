(ns com.blockether.vis.ext.language-python.test-fn-test
  "run_tests(\"python\") handler: path resolution + the default hermetic GraalPy
   backend that discovers a `tests/` tree and runs it through the built-in
   pytest shim. Requiring `shim-pytest` registers the shim so the runner can
   pull its preamble; the GraalPy engine is exercised end to end."
  (:require [clojure.java.io :as io]
            [com.blockether.vis.ext.language-python.core :as core]
            [com.blockether.vis.ext.language-python.interpreter :as interp]
            [com.blockether.vis.internal.foundation.language-surface :as language-surface]
            [com.blockether.vis.internal.process-jail :as process-jail]
            [com.blockether.vis.internal.python-project :as pyproj]
            ;; side-effecting require: registers the built-in pytest shim so
            ;; `extension/sandbox-shims` can hand the runner its preamble.
            [com.blockether.vis.internal.foundation.shim-pytest]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn- tmp-dir
  ^java.io.File []
  (.toFile (Files/createTempDirectory "vis-py-test-fn-" (into-array FileAttribute []))))

(defn- cleanup
  [^java.io.File root]
  (when (.exists root)
    (doseq [^java.io.File f (reverse (file-seq root))]
      (.delete f))))

(def ^:private resolve-test-paths @#'core/resolve-test-paths)

(def ^:private on-path? @#'interp/on-path?)

(defn- has-python? [] (boolean (or (on-path? "python3") (on-path? "python"))))

(defdescribe resolve-test-paths-test
             "Default target: honor {paths}, else tests/ when it exists, else the root."
             (it "prefers a tests/ dir when present"
                 (let [root (tmp-dir)]
                   (try (.mkdirs (io/file root "tests"))
                        (expect (= [(.getCanonicalPath (io/file root "tests"))]
                                   (resolve-test-paths (.getPath root) {})))
                        (finally (cleanup root)))))
             (it "falls back to the workspace root with no tests/ dir"
                 (let [root (tmp-dir)]
                   (try (expect (= [(.getCanonicalPath root)]
                                   (resolve-test-paths (.getPath root) {})))
                        (finally (cleanup root)))))
             (it "honors explicit {paths} that EXIST, resolved to absolute"
                 (let [root (tmp-dir)]
                   (try (.mkdirs (io/file root "a"))
                        (.mkdirs (io/file root "b"))
                        (expect (= [(.getCanonicalPath (io/file root "a"))
                                    (.getCanonicalPath (io/file root "b"))]
                                   (resolve-test-paths (.getPath root) {"paths" ["a" "b"]})))
                        (finally (cleanup root)))))
             (it "throws on a {paths} entry that does not exist"
                 (let [root (tmp-dir)]
                   (try (expect (= :py/bad-args
                                   (try (resolve-test-paths (.getPath root) {"paths" ["ghost"]})
                                        nil
                                        (catch clojure.lang.ExceptionInfo e (:type (ex-data e))))))
                        (finally (cleanup root))))))

(defdescribe
  graalpy-backend-test
  "The default hermetic backend discovers a tests/ tree and reports per-test
   counts derived from the shim's records."
  (it "runs a discovered tests/ tree and reports passed + failed counts"
      (let [root (tmp-dir)]
        (try (.mkdirs (io/file root "tests"))
             (spit (io/file root "tests" "test_sample.py")
                   (str "def test_ok():\n" "    assert 1 + 1 == 2\n\n"
                        "def test_bad():\n" "    assert 1 == 2\n"))
             (let
               [r (core/py-test-fn {:workspace/root (.getPath root)} {"runner" "graalpy"})
                res (:result r)]

               (expect (:success? r))
               (expect (= "graalpy" (get res "runner")))
               (expect (= 1 (get res "files")))
               (expect (= 1 (get res "passed")))
               (expect (= 1 (get res "failed")))
               (expect (false? (get res "ok"))))
             (finally (cleanup root)))))
  (it "reports zero files (not a crash) when no tests are present"
      (let [root (tmp-dir)]
        (try (spit (io/file root "notes.txt") "no tests here\n")
             (let [res (:result (core/py-test-fn {:workspace/root (.getPath root)} {}))]
               (expect (= 0 (get res "files")))
               (expect (= 0 (get res "passed"))))
             (finally (cleanup root)))))
  (it "exposes the generic project environment and routes it to project pytest"
      (let
        [environment-schema (get-in language-surface/test-symbol
                                    [:ext.symbol/schema :properties "environment"])]
        (expect (= ["project"] (:enum environment-schema)))
        (expect (not (re-find #"(?i)python|graalpy|pytest|venv"
                              (:description environment-schema)))))
      ;; No project pytest is assumed in CI; assert routing from the public option
      ;; to the project-process result shape rather than whether that suite passes.
      (when (has-python?)
        (let
          [root
           (tmp-dir)

           session-id
           (str "python-test-fn-" (random-uuid))]

          (try (process-jail/register-session-jail!
                 session-id
                 (constantly
                   {:roots-fn (constantly [(.getPath root)]) :net-enabled? true :disabled? true}))
               (let
                 [res (:result (core/py-test-fn {:workspace/root (.getPath root)
                                                 :session-id session-id}
                                                {"environment" "project"}))]
                 (expect (= "project" (get res "runner")))
                 (expect (vector? (get res "cmd")))
                 (expect (some #{"-m" "pytest"} (get res "cmd"))))
               (finally (process-jail/unregister-session-jail! session-id) (cleanup root)))))))

(defdescribe
  explicit-target-test
  "A named test FILE actually runs, relative paths follow the run's `cwd`, a
   missing target is a user error, and a run that discovered nothing is never
   green (issue #70: explicit target came back is_pass=true, 0/0)."
  (it "runs an explicitly named *.py file (not only a directory)"
      (let [root (tmp-dir)]
        (try (.mkdirs (io/file root "tests"))
             (spit (io/file root "tests" "test_one.py")
                   (str "def test_ok():\n" "    assert 1 + 1 == 2\n"))
             (spit (io/file root "tests" "test_two.py")
                   (str "def test_other():\n" "    assert 1 == 2\n"))
             (let
               [res (:result (core/py-test-fn {:workspace/root (.getPath root)}
                                              {"paths" ["tests/test_one.py"]}))]
               (expect (= 1 (get res "files")))
               (expect (= 1 (get res "passed")))
               (expect (= 0 (get res "failed")))
               (expect (true? (get res "ok"))))
             (finally (cleanup root)))))
  (it "resolves relative paths against {cwd}, not the workspace root"
      (let [root (tmp-dir)]
        (try (.mkdirs (io/file root "proj" "tests"))
             (spit (io/file root "proj" "tests" "test_deep.py")
                   (str "def test_ok():\n" "    assert True\n"))
             (let
               [res (:result (core/py-test-fn {:workspace/root (.getPath root)}
                                              {"cwd" "proj" "paths" ["tests"]}))]
               (expect (= 1 (get res "files")))
               (expect (= 1 (get res "passed")))
               (expect (true? (get res "ok"))))
             (finally (cleanup root)))))
  (it "rejects a target that does not exist instead of running nothing"
      (let [root (tmp-dir)]
        (try (expect (= :py/bad-args
                        (try (core/py-test-fn {:workspace/root (.getPath root)}
                                              {"paths" ["nope/test_missing.py"]})
                             nil
                             (catch clojure.lang.ExceptionInfo e (:type (ex-data e))))))
             (finally (cleanup root)))))
  (it "is NOT a pass when the target discovered no test file"
      (let [root (tmp-dir)]
        (try (.mkdirs (io/file root "tests"))
             (spit (io/file root "tests" "helpers.py") "X = 1\n")
             (let
               [res (:result (core/py-test-fn {:workspace/root (.getPath root)}
                                              {"paths" ["tests"]}))]
               (expect (= 0 (get res "files")))
               (expect (false? (get res "ok")))
               (expect (string? (get res "error"))))
             (finally (cleanup root))))))

(defdescribe
  project-layout-test
  "Issue #93: the hermetic backend must see what the project DECLARES about
   itself — a `src` import root on `sys.path`, pytest's own `testpaths` as the
   default target, and a real `__file__` for tests that read fixtures sitting
   beside them."
  (it "imports a src-layout package, honors testpaths, and binds __file__"
      (let [root (tmp-dir)]
        (try (.mkdirs (io/file root "src" "einmal"))
             (.mkdirs (io/file root "suite"))
             ;; `suite/`, not `tests/`: only the declared testpaths can find it.
             (spit (io/file root "pyproject.toml")
                   (str "[project]\n"
                        "name = \"einmal\"\n" "version = \"0.1.0\"\n\n"
                        "[tool.setuptools]\n" "package-dir = {\"\" = \"src\"}\n\n"
                        "[tool.pytest.ini_options]\n" "testpaths = [\"suite\"]\n"))
             (spit (io/file root "src" "einmal" "__init__.py") "")
             (spit (io/file root "src" "einmal" "core.py") "def add(a, b):\n    return a + b\n")
             (spit (io/file root "suite" "fixture.txt") "42\n")
             (spit (io/file root "suite" "test_core.py")
                   (str "import pathlib\n"
                        "from einmal.core import add\n\n" "def test_add():\n"
                        "    assert add(1, 2) == 3\n\n" "def test_reads_a_file_beside_it():\n"
                        "    here = pathlib.Path(__file__).parent\n"
                        "    assert (here / 'fixture.txt').read_text().strip() == '42'\n"))
             (let [res (:result (core/py-test-fn {:workspace/root (.getPath root)} {}))]
               (expect (= 1 (get res "files")))
               (expect (= 2 (get res "passed")))
               (expect (= 0 (get res "failed")))
               (expect (= 0 (get res "errored")))
               (expect (true? (get res "ok")))
               (expect (some #{(.getCanonicalPath (io/file root "src"))} (get res "sys_path"))))
             (finally (cleanup root)))))
  (it "an explicit {paths} still wins over declared testpaths"
      (let [root (tmp-dir)]
        (try (.mkdirs (io/file root "a"))
             (expect (= [(.getCanonicalPath (io/file root "a"))]
                        (resolve-test-paths (.getPath root) {"paths" ["a"]} ["/declared"])))
             (expect (= ["/declared"] (resolve-test-paths (.getPath root) {} ["/declared"])))
             (finally (cleanup root)))))
  (it "a project declaring nothing keeps the tests/ then cwd fallback"
      (let [root (tmp-dir)]
        (try (.mkdirs (io/file root "tests"))
             (expect (= [(.getCanonicalPath (io/file root "tests"))]
                        (resolve-test-paths (.getPath root) {} nil)))
             (finally (cleanup root))))))

;; ── backend selection + a layout read that FAILED (Blockether/vis#98) ──────────
(def ^:private select-runner @#'core/select-runner)

(defdescribe
  runner-selection-test
  "`python.runner` in merged config picks the default backend; an explicit
   `environment` / `runner` argument still wins."
  (it "defaults to the hermetic sandbox"
      (with-redefs [interp/configured-runner (constantly nil)]
        (expect (= "graalpy" (select-runner {})))))
  (it "honors python.runner from merged config"
      (with-redefs [interp/configured-runner (constantly "project")]
        (expect (= "project" (select-runner {})))))
  (it "lets an explicit environment beat the configured default"
      (with-redefs [interp/configured-runner (constantly "project")]
        (expect (= "graalpy" (select-runner {"environment" "graalpy"})))))
  (it "lets an explicit runner beat the configured default"
      (with-redefs [interp/configured-runner (constantly "project")]
        (expect (= "graalpy" (select-runner {"runner" "graalpy"})))))
  (it "keeps environment/project and the private compatibility aliases"
      (expect (= "project" (select-runner {"environment" "project"})))
      (expect (= "project" (select-runner {"runner" "project"})))
      (expect (= "project" (select-runner {"interpreter" "python3"})))))

(defdescribe
  layout-warning-test
  "A layout read that FAILED is REPORTED. Degrading silently to `no import roots`
   is what makes a src-layout project report bogus `No module named <pkg>`."
  (it "surfaces the layout warning on the run_tests result"
      (let [root (tmp-dir)]
        (try (.mkdirs (io/file root "tests"))
             (spit (io/file root "tests" "test_sample.py") "def test_ok():\n    assert True\n")
             (with-redefs
               [pyproj/project-layout (constantly {:import-roots []
                                                   :testpaths []
                                                   :warning "project layout not read: boom"})]
               (let
                 [res (:result (core/py-test-fn {:workspace/root (.getPath root)}
                                                {"runner" "graalpy"}))]
                 (expect (= "project layout not read: boom" (get res "warning")))
                 (expect (= 1 (get res "passed")))))
             (finally (cleanup root)))))
  (it "adds no warning key when the layout reads cleanly"
      (let [root (tmp-dir)]
        (try (.mkdirs (io/file root "tests"))
             (spit (io/file root "tests" "test_sample.py") "def test_ok():\n    assert True\n")
             (let
               [res (:result (core/py-test-fn {:workspace/root (.getPath root)}
                                              {"runner" "graalpy"}))]
               (expect (nil? (get res "warning"))))
             (finally (cleanup root))))))
