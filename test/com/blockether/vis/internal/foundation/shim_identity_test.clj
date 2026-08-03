(ns com.blockether.vis.internal.foundation.shim-identity-test
  "Module IDENTITY of the built-in Python sandbox shims.

   A shim module is synthesised by eval'ing Python source into the context, so it
   starts with none of the attributes an imported module carries — and the very
   first thing a debugging agent reaches for (`PIL.__file__`, `ruff.__version__`)
   raised `AttributeError`. `env-python/install-sandbox-shims!` stamps every shim
   module through `vis-python/shim_identity.py`, on BOTH the eager and the lazy
   load path; this pins that every registered shim answers, and that `-m <mod>`
   still routes through the module's own `console_main` now that a shim module
   HAS a `__file__`."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.env-python :as ep]
            [com.blockether.vis.internal.extension :as extension]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [org.graalvm.polyglot Context]))

(defn- ev [^Context c code] (ep/->clj (.eval c "python" code)))


(defn- registered-shims
  "Every registered sandbox shim, with the built-in extension namespaces loaded."
  []
  (#'extension/load-builtin-extensions!)
  (extension/sandbox-shims))

;; ONE context for the whole namespace: each shim is materialised on first import,
;; which is exactly the path under test, and paying GraalPy boot per assertion
;; would dominate the runtime.
(defonce ^:private python-context* (delay (ep/create-python-context {})))

(defmacro ^:private with-python-context
  [& body]
  `(let [~(with-meta 'python-context {:tag `Context}) (:python-context @python-context*)]
     ~@body))

(defn- identity-of
  "`[__file__ __vis_shim__ __version__]` of shim module `module-name`, imported
   the way an agent would import it."
  [^Context c module-name]
  (ev c
      (str "import "
           module-name
           "\n"
           "_m = __import__('sys').modules["
           (pr-str module-name)
           "]\n"
           "[getattr(_m, '__file__', None), getattr(_m, '__vis_shim__', None),"
           " getattr(_m, '__version__', None)]")))

(defdescribe
  shim-module-identity-test
  (it "stamps every importable shim module with file, shim id and version"
      (with-python-context
        (doseq
          [{:shim/keys [name source imports]}
           (registered-shims)

           :when (seq imports)
           module
           imports]

          (let [[file marker version] (identity-of python-context module)]
            (expect (= (str "<vis-shim>/" source) file) (str module " has __file__ " (pr-str file)))
            (expect (= name marker) (str module " has __vis_shim__ " (pr-str marker)))
            (expect (and (string? version) (not (str/blank? version)))
                    (str module " has __version__ " (pr-str version)))))))
  (it "never overwrites a version the shim declares itself"
      ;; The stamp is a FALLBACK: a shim that states what it emulates keeps saying so.
      (with-python-context
        (expect (= "10.0-vis-imaging" (ev python-context "__import__('PIL').__version__")))
        (expect (= "1.26-vis-pure" (ev python-context "__import__('numpy').__version__")))))
  (it "gives submodules the same origin as the shim that published them"
      (with-python-context
        (expect (= ["<vis-shim>/vis-shims/pil.py" "<vis-shim>/vis-shims/matplotlib.py"]
                   (ev python-context
                       (str "import PIL.Image, mpl_toolkits.mplot3d\n"
                            "_s = __import__('sys').modules\n"
                            "[_s['PIL.Image'].__file__, _s['mpl_toolkits.mplot3d'].__file__]")))))))

(defdescribe
  shim-module-runner-test
  "`vis-agent python -m <mod>`: the runner recognises a shim by its `__vis_shim__`
   marker, so it still calls the module's `console_main` rather than runpy — which
   has no code object for a synthesised module and would report 'No module named'."
  (it "runs a shim module's console_main and returns its exit code"
      (with-python-context (expect (= 0
                                      (ev python-context
                                          (str (slurp (io/resource "vis-python/module_runner.py"))
                                               "\n"
                                               "__import__('sys').argv = ['ruff', 'version']\n"
                                               "__vis_run_module__('ruff')"))))))
  (it "still recognises a shim module that was never stamped"
      ;; The missing-`__file__` fallback: an extension-contributed module the
      ;; identity stamp never reached must keep working as a `-m` target.
      (with-python-context (expect (= 0
                                      (ev python-context
                                          (str (slurp (io/resource "vis-python/module_runner.py"))
                                               "\n" "import sys, types\n"
                                               "_m = types.ModuleType('vis_unstamped_mod')\n"
                                               "_m.console_main = lambda argv: 0\n"
                                               "sys.modules['vis_unstamped_mod'] = _m\n"
                                               "__vis_run_module__('vis_unstamped_mod')")))))))
