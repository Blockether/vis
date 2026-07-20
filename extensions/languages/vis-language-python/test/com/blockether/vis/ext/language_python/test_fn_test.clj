(ns com.blockether.vis.ext.language-python.test-fn-test
  "run_tests(\"python\") handler: path resolution + the default hermetic GraalPy
   backend that discovers a `tests/` tree and runs it through the built-in
   pytest shim. Requiring `shim-pytest` registers the shim so the runner can
   pull its preamble; the GraalPy engine is exercised end to end."
  (:require [clojure.java.io :as io]
            [com.blockether.vis.ext.language-python.core :as core]
            [com.blockether.vis.ext.language-python.interpreter :as interp]
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
             (it "honors explicit {paths}, resolved to absolute"
                 (let [root (tmp-dir)]
                   (try (expect (= [(.getCanonicalPath (io/file root "a"))
                                    (.getCanonicalPath (io/file root "b"))]
                                   (resolve-test-paths (.getPath root) {"paths" ["a" "b"]})))
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
  (it "routes {interpreter true} to the project backend (not graalpy)"
      ;; No project pytest is installed in CI, so we only assert the ROUTING:
      ;; the project backend shells a process and returns a "project" runner tag
      ;; with the resolved cmd — never the graalpy shape.
      (when (has-python?)
        (let [root (tmp-dir)]
          (try (let
                 [res (:result (core/py-test-fn {:workspace/root (.getPath root)}
                                                {"interpreter" true}))]
                 (expect (= "project" (get res "runner")))
                 (expect (vector? (get res "cmd")))
                 (expect (some #{"-m" "pytest"} (get res "cmd"))))
               (finally (cleanup root)))))))
