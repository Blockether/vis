(ns com.blockether.vis.internal.python-package-test
  "The published Python SDK against its checkout sources and canonical contracts."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.foundation.shell :as shell]
            [lazytest.core :refer [defdescribe describe expect it]]))

(def ^:private agent-dir "packages/vis-agent")

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
                (expect (= (slurp (io/file agent-dir "src/blockether/vis/extension.py"))
                           (slurp (io/resource "blockether/vis/extension.py")))))
            (it "is the only copy — the injector carries the host and nothing else"
                (let [injector (slurp (io/resource "vis-guest/extension_bootstrap.py"))]
                  (expect (str/includes? injector "_vis_body"))
                  (expect (not (str/includes? injector "def ask(")))))
            (it "imports nothing a sandbox cannot give it"
                (expect (nil? (re-find #"(?m)^\s*(?:import|from)\s+blockether\.vis\._contracts"
                                       (slurp (io/file agent-dir
                                                       "src/blockether/vis/extension.py")))))))
  (describe "the outside host"
            (it "answers the engine's own shell result keys, so no lookup can KeyError"
                (expect (= (set (keys @#'shell/shell-result-base))
                           (set (python-tuple (slurp (io/file agent-dir
                                                              "src/blockether/vis/_outside.py"))
                                              "_SHELL_RESULT_KEYS"))))))
  (describe "the distribution"
            (it "carries the product version"
                (expect (= (str/trim (slurp "VIS_VERSION")) (declared-version agent-dir))))
            (it "bundles contracts instead of depending on another Python distribution"
                (expect (not (.exists (io/file "packages/vis-contract/pyproject.toml"))))
                (expect (not (str/includes? (pyproject agent-dir) "vis-contract==")))
                (expect (str/includes? (pyproject agent-dir) "jsonschema>=4.23,<5"))
                (expect (str/includes? (pyproject agent-dir) "hatch_build.py")))))
