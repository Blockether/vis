(ns com.blockether.vis.internal.python-package-test
  "The published Python packages against their checkout sources."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.foundation.shell :as shell]
            [lazytest.core :refer [defdescribe describe expect it]]))

(def ^:private agent-dir "packages/vis-agent")

(def ^:private contract-dir "packages/vis-contract")

(defn- pyproject [dir] (slurp (io/file dir "pyproject.toml")))

(defn- declared-version [dir] (second (re-find #"(?m)^version = \"([^\"]+)\"" (pyproject dir))))

(defn- python-tuple
  [source binding]
  (let [body (second (re-find (re-pattern (str binding #"\s*=\s*\(([^)]*)\)")) source))]
    (mapv second (re-seq #"\"([^\"]+)\"" (or body "")))))

(defdescribe
  python-package-test
  (describe "the module `vis-agent` ships"
            (it "is the very file the engine execs into an extension context"
                (expect (= (slurp (io/file agent-dir "src/vis/__init__.py"))
                           (slurp (io/resource "vis/__init__.py")))))
            (it "is the only copy — the injector carries the host and nothing else"
                (let [injector (slurp (io/resource "vis-python/extension_bootstrap.py"))]
                  (expect (str/includes? injector "_vis_body"))
                  (expect (not (str/includes? injector "def ask(")))))
            (it "imports nothing a sandbox cannot give it"
                (expect (nil? (re-find #"(?m)^\s*(?:import|from)\s+vis_contract"
                                       (slurp (io/file agent-dir "src/vis/__init__.py")))))))
  (describe "the outside host"
            (it "answers the engine's own shell result keys, so no lookup can KeyError"
                (expect (= (set (keys @#'shell/shell-result-base))
                           (set (python-tuple (slurp (io/file agent-dir "src/vis/_outside.py"))
                                              "_SHELL_RESULT_KEYS"))))))
  (describe "the distributions"
            (it "carry the one version the rest of the product is cut at"
                (let [version (str/trim (slurp "VIS_VERSION"))]
                  (expect (= version (declared-version agent-dir)))
                  (expect (= version (declared-version contract-dir)))))
            (it "make `vis-agent` depend on the contract it was cut against"
                (expect (str/includes?
                          (pyproject agent-dir)
                          (str "\"vis-contract==" (declared-version contract-dir) "\""))))))
