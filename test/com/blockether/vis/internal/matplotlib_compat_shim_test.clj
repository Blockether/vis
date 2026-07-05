(ns com.blockether.vis.internal.matplotlib-compat-shim-test
  "The matplotlib-compat shim installed into every sandbox context via the
   generic sandbox-shim mechanism (`extension/sandbox-shims`): a minimal
   `matplotlib.pyplot` published into `sys.modules` (so `import matplotlib.pyplot`
   works) and backed by a pure-JVM Java2D renderer. `savefig` delegates the
   accumulated figure across the boundary to the host `__vis_mpl_render__`,
   which returns a PNG the shim writes to a path or file-like buffer."
  (:require
   [com.blockether.vis.internal.env-python :as ep]
   [lazytest.core :refer [defdescribe expect it]])
  (:import [org.graalvm.polyglot Context]))

(defn- ev [^Context c code] (ep/->clj (.eval c "python" code)))

(defn- png-len
  "Render `plot-code` (pyplot calls) to a PNG in an in-memory buffer and return
   the byte count (0 on failure). Avoids the filesystem so it runs under the
   default IO-NONE sandbox."
  [^Context c plot-code]
  (ev c (str "import matplotlib.pyplot as plt\nimport io\nplt.clf()\n"
          plot-code "\n"
          "__buf = io.BytesIO()\nplt.savefig(__buf)\nlen(__buf.getvalue())")))

(defn- png-magic?
  "True when the buffer starts with the PNG signature bytes."
  [^Context c plot-code]
  (ev c (str "import matplotlib.pyplot as plt\nimport io\nplt.clf()\n"
          plot-code "\n"
          "__buf = io.BytesIO()\nplt.savefig(__buf)\n"
          "list(__buf.getvalue()[:8]) == [137, 80, 78, 71, 13, 10, 26, 10]")))

(defdescribe matplotlib-module-test
  (it "publishes matplotlib + matplotlib.pyplot under sys.modules"
    (let [{:keys [^Context python-context]} (ep/create-python-context {})]
      (expect (true? (ev python-context
                       "__import__('sys').modules.get('matplotlib.pyplot') is not None")))
      (expect (true? (ev python-context
                       "__import__('sys').modules.get('matplotlib') is not None")))))

  (it "autoloads `matplotlib` onto builtins (no import needed)"
    (let [{:keys [^Context python-context]} (ep/create-python-context {})]
      ;; deliberately NO import
      (expect (true? (ev python-context "matplotlib.pyplot is not None")))))

  (it "exposes a version string"
    (let [{:keys [^Context python-context]} (ep/create-python-context {})]
      (expect (= "3.0-vis-java2d"
                (ev python-context "__import__('matplotlib').__version__"))))))

(defdescribe matplotlib-render-test
  (it "renders a line plot to a real PNG (correct magic bytes)"
    (let [{:keys [^Context python-context]} (ep/create-python-context {})]
      (expect (true? (png-magic? python-context
                       "plt.plot([1,2,3,4],[1,4,9,16], label='sq')\nplt.title('t'); plt.xlabel('x'); plt.ylabel('y'); plt.grid(True); plt.legend()")))))

  (it "renders scatter + bar + hist figures to non-empty PNGs"
    (let [{:keys [^Context python-context]} (ep/create-python-context {})]
      (expect (< 100 (png-len python-context "plt.scatter([1,2,3],[3,1,2])")))
      (expect (< 100 (png-len python-context "plt.bar([1,2,3],[4,5,6])")))
      (expect (< 100 (png-len python-context "plt.hist([1,1,2,3,3,3,4], bins=4)")))))

  (it "renders an empty figure without error (no series)"
    (let [{:keys [^Context python-context]} (ep/create-python-context {})]
      (expect (< 100 (png-len python-context "plt.title('empty')")))))

  (it "savefig returns the file-like object it wrote to"
    (let [{:keys [^Context python-context]} (ep/create-python-context {})]
      (expect (true? (ev python-context
                       (str "import matplotlib.pyplot as plt\nimport io\nplt.clf()\n"
                         "plt.plot([0,1],[0,1])\n"
                         "__b = io.BytesIO()\nplt.savefig(__b) is __b")))))))
