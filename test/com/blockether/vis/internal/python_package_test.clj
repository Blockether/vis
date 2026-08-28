(ns com.blockether.vis.internal.python-package-test
  "The two published Python distributions against the engine they were cut from.

   Neither is a copy kept in step by hand. `vis-agent`'s `src/vis/__init__.py` IS
   the module the engine execs inside every extension context, and `vis-contract`'s
   `src/vis_contract/contract.json` is the contract document rendered for a reader
   with no EDN and no JVM. Both are checked in, because a wheel installed from PyPI
   has no repository to consult — so these are the tests that fail when the checkout
   and the packages stop being the same thing."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.contract.python-host :as contract]
            [com.blockether.vis.internal.foundation.shell :as shell]
            [com.blockether.vis.internal.view.spec :as hi]
            [lazytest.core :refer [defdescribe describe expect it]]))

(def ^:private agent-dir "packages/vis-agent")

(def ^:private contract-dir "packages/vis-contract/python")

(defn- pyproject
  "One distribution's `pyproject.toml`, verbatim."
  [dir]
  (slurp (io/file dir "pyproject.toml")))

(defn- declared-version [dir] (second (re-find #"(?m)^version = \"([^\"]+)\"" (pyproject dir))))

(defn- python-tuple
  "The string entries of a Python tuple assigned to `binding` in `source`."
  [source binding]
  (let [body (second (re-find (re-pattern (str binding #"\s*=\s*\(([^)]*)\)")) source))]
    (mapv second (re-seq #"\"([^\"]+)\"" (or body "")))))

(defdescribe
  python-package-test
  (describe "the module `vis-agent` ships"
            (it "is the very file the engine execs into an extension context"
                ;; Not "a copy kept in step": `deps.edn` puts `packages/vis-agent/src` on
                ;; :paths, so the classpath resource and the packaged file are one file.
                (expect (= (slurp (io/file agent-dir "src/vis/__init__.py"))
                           (slurp (io/resource "vis/__init__.py")))))
            (it "is the only copy — the injector carries the host and nothing else"
                (let [injector (slurp (io/resource "vis-python/extension_bootstrap.py"))]
                  (expect (str/includes? injector "_vis_body"))
                  (expect (not (str/includes? injector "def ask(")))))
            (it "imports nothing a sandbox cannot give it"
                ;; Inside Vis the module is EXECed from a classpath resource: there is no
                ;; site-packages, so the API file may not import its own contract package.
                ;; Only `_outside`, which runs where pip put it, is allowed to.
                (expect (nil? (re-find #"(?m)^\s*(?:import|from)\s+vis_contract"
                                       (slurp (io/file agent-dir "src/vis/__init__.py")))))))
  (describe "the contract `vis-contract` ships"
            (it "is what the renderer renders today"
                ;; Regenerate with `(python-host/write-package-document!
                ;; (hi/contract-vocabulary))` — never by hand, or the human-input
                ;; vocabulary has a second definition again.
                (expect (= (contract/package-document-json (hi/contract-vocabulary))
                           (slurp (io/file contract/package-document-path)))))
            (it "answers the engine's own shell result keys, so no lookup can KeyError"
                (expect (= (set (keys @#'shell/shell-result-base))
                           (set (python-tuple (slurp (io/file agent-dir "src/vis/_outside.py"))
                                              "_SHELL_RESULT_KEYS"))))))
  (describe "the distributions"
            (it "carry the one version the rest of the product is cut at"
                ;; VIS_VERSION is the single source (build.clj); every wheel mirrors it.
                (let [version (str/trim (slurp "VIS_VERSION"))]
                  (expect (= version (declared-version agent-dir)))
                  (expect (= version (declared-version contract-dir)))))
            (it "make `vis-agent` depend on the contract it was cut against"
                ;; An extension author who installs the API gets the declaration with it,
                ;; at the exact version — a wheel pair that disagrees is the drift these
                ;; tests exist to prevent.
                (expect (str/includes?
                          (pyproject agent-dir)
                          (str "\"vis-contract==" (declared-version contract-dir) "\""))))))
