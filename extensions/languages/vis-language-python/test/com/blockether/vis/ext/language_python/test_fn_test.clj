(ns com.blockether.vis.ext.language-python.test-fn-test
  "run_tests with {\"language\": \"python\"}: path resolution + the default hermetic GraalPy
   backend that discovers a `tests/` tree and runs it through the built-in
   pytest shim. Requiring `shim-pytest` registers the shim so the runner can
   pull its preamble; the GraalPy engine is exercised end to end."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.ext.language-python.core :as core]
            [com.blockether.vis.ext.language-python.interpreter :as interp]
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
  ;; The `environment` option is documented in the tool's description, not in a
  ;; JSON Schema enum: the model reaches `run_tests` from Python, where the option
  ;; is a plain string key.
  (it "routes the generic project environment to project pytest"
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

(def ^:private pytest-counts @#'core/pytest-counts)

;; Regression, issue #132: an all-green project run reported only "12 passed", so
;; every other count stayed UNKNOWN, `total` was never derived and the run_tests
;; headline shrank to a bare " (16484ms)".
(defdescribe pytest-counts-test
             (it "zero-fills the outcomes pytest left out of its summary"
                 (expect (= {"passed" 12 "failed" 0 "errored" 0 "skipped" 0}
                            (pytest-counts "==== 12 passed in 3.21s ====")))
                 (expect (= {"passed" 10 "failed" 2 "errored" 1 "skipped" 3}
                            (pytest-counts "= 2 failed, 10 passed, 3 skipped, 1 error in 4.5s ="))))
             (it "reports nothing when the run printed no summary at all"
                 (expect (nil? (pytest-counts "")))
                 (expect (nil? (pytest-counts "ERROR: file or directory not found: nope.py")))))

;; ── issue #136: counts with no node ids, and an output cut that said nothing ──
(def ^:private clamp-output @#'core/clamp-output)

(def ^:private junit-report @#'core/junit-report)

(def ^:private output-char-cap @#'core/output-char-cap)

;; Regression, issue #136: `run_tests("python", {"environment" "project"})`
;; reported `fail: 1` with `failures: []` / `errors: []` — counts and not one
;; node id, file or message — and its `output` was a tail slice behind a bare
;; `…`, so a run with many failures came back with the summary line and no
;; `=== FAILURES ===` section at all, with nothing to say it had been dropped.
(defdescribe
  output-clamp-test
  "The transcript cap keeps BOTH ends and names what it dropped."
  (it "leaves an output that fits untouched" (expect (= "short" (clamp-output "short" 8000))))
  (it "keeps the head and the tail, and says how much went missing"
      (let
        [s
         (str
           "=== test session starts ===\n" (apply str (repeat 20000 "x"))
           "\n=================================== FAILURES ===================================\n"
           (apply str (repeat 20000 "y"))
           "\n=== short test summary info ===\nFAILED tests/test_x.py::test_bad - assert 1 == 2\n"
           "=== 40 failed in 1.23s ===")

         out
         (clamp-output s 8000)]

        (expect (<= (count out) 8000))
        (expect (str/starts-with? out "=== test session starts ==="))
        (expect (str/ends-with? out "=== 40 failed in 1.23s ==="))
        (expect (str/includes? out "characters omitted"))
        (expect (str/includes? out (str (- (count s) (- 8000 96)))))))
  (it "caps at output-char-cap chars" (expect (= 8000 output-char-cap))))

(defdescribe
  junit-report-test
  "pytest's --junitxml report is what turns `1 failed` into a named test."
  (it
    "reads failures, errors, counts, resolved file and a 1-based line"
    (let [root (tmp-dir)]
      (try
        (.mkdirs (io/file root "tests"))
        (spit (io/file root "tests" "test_x.py") "def test_bad():\n    assert 1 == 2\n")
        (spit
          (io/file root "report.xml")
          (str
            "<?xml version=\"1.0\" encoding=\"utf-8\"?>" "<testsuites name=\"pytest tests\">"
            "<testsuite name=\"pytest\" errors=\"1\" failures=\"1\" skipped=\"1\" tests=\"4\">"
            "<testcase classname=\"tests.test_x\" name=\"test_ok\" file=\"tests/test_x.py\" line=\"0\"/>"
            "<testcase classname=\"tests.test_x\" name=\"test_skip\" file=\"tests/test_x.py\" line=\"3\">"
            "<skipped message=\"no reason\"/></testcase>"
            "<testcase classname=\"tests.test_x\" name=\"test_bad\" file=\"tests/test_x.py\" line=\"5\">"
            "<failure message=\"assert 1 == 2\">def test_bad():\n"
            "&gt;       assert 1 == 2\nE       assert 1 == 2\n\ntests/test_x.py:7: AssertionError"
            "</failure></testcase>"
            "<testcase classname=\"tests.test_x.TestG\" name=\"test_err\" file=\"tests/test_x.py\" line=\"9\">"
            "<error message=\"failed on setup with &quot;ValueError: boom&quot;\">"
            "E       ValueError: boom</error></testcase>" "</testsuite></testsuites>"))
        (let
          [r (junit-report (.getPath root) (io/file root "report.xml"))
           f (first (:failures r))
           e (first (:errors r))]

          (expect (= 1 (count (:failures r))))
          (expect (= 1 (count (:errors r))))
          (expect (= "test_bad" (get f "test")))
          (expect (= "tests.test_x" (get f "ns")))
          (expect (= "assert 1 == 2" (get f "message")))
          (expect (= (.getCanonicalPath (io/file root "tests" "test_x.py")) (get f "file")))
          ;; pytest writes a 0-based line index; the contract's line is 1-based.
          (expect (= 6 (get f "line")))
          (expect (= "test_err" (get e "test")))
          (expect (str/includes? (get e "message") "ValueError: boom"))
          (expect (= {"passed" 1 "failed" 1 "errored" 1 "skipped" 1} (:counts r))))
        (finally (cleanup root)))))
  (it "returns nothing (not a crash) for a report that was never written"
      (let [root (tmp-dir)]
        (try (expect (nil? (junit-report (.getPath root) (io/file root "absent.xml"))))
             (spit (io/file root "junk.xml") "not xml at all <<<")
             (expect (nil? (junit-report (.getPath root) (io/file root "junk.xml"))))
             (finally (cleanup root))))))

(defdescribe
  faulted-run-test
  "Both backends NAME every failing test, not just count it."
  (it "reports the hermetic backend's failures with node id and file"
      (let [root (tmp-dir)]
        (try (.mkdirs (io/file root "tests"))
             (spit (io/file root "tests" "test_sample.py")
                   (str "def test_ok():\n" "    assert True\n\n"
                        "def test_bad():\n" "    assert 1 == 2\n"))
             (let
               [res (:result (core/py-test-fn {:workspace/root (.getPath root)}
                                              {"runner" "graalpy"}))
                f (first (get res "failures"))]

               (expect (= 1 (count (get res "failures"))))
               (expect (= [] (get res "errors")))
               (expect (str/includes? (str (get f "test")) "test_bad"))
               (expect (str/includes? (str (get f "file")) "test_sample.py"))
               (expect (seq (str (get f "message")))))
             (finally (cleanup root)))))
  (it "reports the project backend's failures from pytest's own junit report"
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
               (.mkdirs (io/file root "tests"))
               (spit (io/file root "tests" "test_sample.py")
                     (str "def test_ok():\n" "    assert True\n\n"
                          "def test_bad():\n" "    assert 1 == 2\n"))
               (let
                 [res (:result (core/py-test-fn {:workspace/root (.getPath root)
                                                 :session-id session-id}
                                                {"environment" "project"}))]
                 (expect (= "project" (get res "runner")))
                 ;; pytest itself may be absent on the machine running this suite;
                 ;; assert the faults only for a run that actually reported one.
                 (when (= 1 (get res "failed"))
                   (expect (= 1 (count (get res "failures"))))
                   (expect (= "test_bad" (get-in res ["failures" 0 "test"])))
                   (expect (str/includes? (get-in res ["failures" 0 "file"]) "test_sample.py"))
                   (expect (str/includes? (get-in res ["failures" 0 "message"]) "assert 1 == 2"))))
               (finally (process-jail/unregister-session-jail! session-id) (cleanup root)))))))
