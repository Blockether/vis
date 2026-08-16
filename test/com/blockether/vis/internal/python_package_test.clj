(ns com.blockether.vis.internal.python-package-test
  "The published package (`packages/vis-agent`) against the engine it was cut from.

   `vis-agent` is not a copy of anything: `src/vis/__init__.py` IS the module the
   engine execs inside every extension context, and `src/vis/contract.json` is
   `resources/vis-contract/python-host.edn` rendered for a reader with no EDN and
   no JVM. Both are checked in, because a wheel installed from PyPI has no
   repository to consult — so these are the tests that fail when the checkout and
   the package stop being the same thing."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.foundation.shell :as shell]
            [com.blockether.vis.internal.python-contract :as contract]
            [lazytest.core :refer [defdescribe describe expect it]]))

(def ^:private package-dir "packages/vis-agent")

(defn- package-file [& parts] (apply io/file package-dir parts))

(defn- python-tuple
  "The string entries of a Python tuple assigned to `binding` in `source`."
  [source binding]
  (let [body (second (re-find (re-pattern (str binding #"\s*=\s*\(([^)]*)\)")) source))]
    (mapv second (re-seq #"\"([^\"]+)\"" (or body "")))))

(defdescribe
  python-package-test
  (describe "the module the package ships"
            (it "is the very file the engine execs into an extension context"
                ;; Not "a copy kept in step": `deps.edn` puts `packages/vis-agent/src` on
                ;; :paths, so the classpath resource and the packaged file are one file.
                (expect (= (slurp (package-file "src/vis/__init__.py"))
                           (slurp (io/resource "vis/__init__.py")))))
            (it "is the only copy — the injector carries the host dict and nothing else"
                (let [injector (slurp (io/resource "vis-python/extension_bootstrap.py"))]
                  (expect (str/includes? injector "_vis_body"))
                  (expect (not (str/includes? injector "def ask("))))))
  (describe "the contract the package reads"
            (it "is what the renderer renders today"
                ;; Regenerate with `(python-contract/write-package-document!)` — never by
                ;; hand, or the human-input vocabulary has a second definition again.
                (expect (= (contract/package-document-json)
                           (slurp (io/file contract/package-document-path)))))
            (it "answers the engine's own shell result keys, so no lookup can KeyError"
                (expect (= (set (keys @#'shell/shell-result-base))
                           (set (python-tuple (slurp (package-file "src/vis/_outside.py"))
                                              "_SHELL_RESULT_KEYS"))))))
  (describe "the distribution"
            (it "carries the one version the rest of the product is cut at"
                ;; VIS_VERSION is the single source (build.clj); the wheel mirrors it.
                (expect (= (str/trim (slurp "VIS_VERSION"))
                           (second (re-find #"(?m)^version = \"([^\"]+)\""
                                            (slurp (package-file "pyproject.toml")))))))))
