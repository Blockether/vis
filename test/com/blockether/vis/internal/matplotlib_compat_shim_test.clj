(ns com.blockether.vis.internal.matplotlib-compat-shim-test
  "The matplotlib-compat shim installed into every sandbox context via the
   generic sandbox-shim mechanism (`extension/sandbox-shims`): a minimal
   `matplotlib.pyplot` published into `sys.modules` (so `import matplotlib.pyplot`
   works) and backed by the native `imaging` renderer. `savefig` delegates the
   accumulated figure across the boundary to the host `__vis_mpl_render__`,
   which returns a PNG the shim writes to a path or file-like buffer."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.env-python :as ep]
            [com.blockether.vis.test-python-context :as tpc]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]
           [org.graalvm.polyglot Context]))

(defn- ev [^Context c code] (ep/->clj (.eval c "python" code)))

(defn- tmp-dir
  "A directory this sandbox may open: Python's `open` refuses everything outside
   the Context's roots, so a figure SAVED to a real path has to land under one."
  ^String []
  (str (Files/createTempDirectory "vis-mpl-" (make-array FileAttribute 0))))

(defmacro ^:private with-fs-context
  [dir & body]
  `(let [~(with-meta 'python-context {:tag `Context})
         (:python-context (ep/create-python-context {} (constantly [~dir])))]
     (try ~@body (finally (.close ~'python-context)))))

(defn- png-len
  "Render `plot-code` (pyplot calls) to a PNG in an in-memory buffer and return
   the byte count (0 on failure). Avoids the filesystem so it runs under the
   default IO-NONE sandbox."
  [^Context c plot-code]
  (ev c
      (str "import matplotlib.pyplot as plt\nimport io\nplt.clf()\n" plot-code
           "\n" "__buf = io.BytesIO()\nplt.savefig(__buf)\nlen(__buf.getvalue())")))

(defn- png-magic?
  "True when the buffer starts with the PNG signature bytes."
  [^Context c plot-code]
  (ev c
      (str "import matplotlib.pyplot as plt\nimport io\nplt.clf()\n"
           plot-code
           "\n"
           "__buf = io.BytesIO()\nplt.savefig(__buf)\n"
           "list(__buf.getvalue()[:8]) == [137, 80, 78, 71, 13, 10, 26, 10]")))

(defmacro with-python-context
  [& body]
  `(let [~(with-meta 'python-context {:tag `Context}) (tpc/shared)]
     ~@body))

(defdescribe
  matplotlib-module-test
  (it
    "publishes matplotlib + matplotlib.pyplot under sys.modules"
    (with-python-context
      (expect
        (true?
          (ev
            python-context
            "import matplotlib.pyplot\n__import__('sys').modules.get('matplotlib.pyplot') is not None")))
      (expect (true? (ev python-context
                         "__import__('sys').modules.get('matplotlib') is not None")))))
  (it "autoloads `matplotlib` onto builtins (no import needed)"
      (with-python-context
        ;; deliberately NO import
        (expect (true? (ev python-context "matplotlib.pyplot is not None")))))
  (it "autoloads a bare `pyplot` and the `plt` alias onto builtins (no import)"
      (with-python-context
        (expect (true? (ev python-context "pyplot is not None")))
        (expect (true? (ev python-context "plt is not None")))
        ;; all three names resolve to the same module object
        (expect (true? (ev
                         python-context
                         "import matplotlib.pyplot\nplt is pyplot and plt is matplotlib.pyplot")))))
  (it "exposes a version string"
      (with-python-context (expect (= "3.0-vis-imaging"
                                      (ev python-context "__import__('matplotlib').__version__")))))
  (it "exposes matplotlib.style with a no-op use()"
      (with-python-context
        (expect (true? (ev python-context
                           "import matplotlib.style as st\nst.use('ggplot') is None"))))))

(defdescribe
  matplotlib-backend-and-rcparams-test
  (it
    "use / switch_backend record a backend and get_backend reads it back"
    (with-python-context
      (expect (true? (ev python-context "import matplotlib; matplotlib.use('Qt5Agg') is None")))
      (expect (= "svg" (ev python-context "matplotlib.use('svg'); matplotlib.get_backend()")))
      (expect
        (=
          "agg"
          (ev
            python-context
            "import matplotlib.pyplot as plt\nplt.switch_backend('agg')\nmatplotlib.get_backend()")))))
  (it
    "rcParams tolerates unknown keys, update() and item assignment (no KeyError)"
    (with-python-context
      (expect (true? (ev python-context
                         "import matplotlib\nmatplotlib.rcParams['no.such.key'] is None")))
      (expect (= [6.4 4.8] (ev python-context "list(matplotlib.rcParams['figure.figsize'])")))
      (expect
        (=
          3
          (ev
            python-context
            "matplotlib.rcParams.update({'lines.linewidth': 3})\nmatplotlib.rcParams['lines.linewidth']")))
      (expect
        (=
          14
          (ev
            python-context
            "matplotlib.rcParams['axes.titlesize'] = 14\nmatplotlib.rcParams['axes.titlesize']")))))
  (it
    "interactive-mode + draw stubs are callable no-ops"
    (with-python-context
      (expect (false?
                (ev python-context
                    "import matplotlib.pyplot as plt\nplt.ion()\nplt.ioff()\nplt.isinteractive()")))
      (expect
        (true?
          (ev
            python-context
            "import matplotlib.pyplot as plt\nplt.draw() is None and plt.pause(0.001) is None and plt.figtext(0.5, 0.5, 'x') is None"))))))

(defdescribe
  matplotlib-api-surface-test
  (it "publishes the expected pyplot callables"
      (with-python-context
        (expect (true? (ev python-context
                           (str
                             "import matplotlib.pyplot as plt\n"
                             "all(callable(getattr(plt, n, None)) for n in "
                             "['plot','scatter','bar','barh','hist','fill_between','step',"
                             "'pie','axhline','axvline','errorbar','text','annotate',"
                             "'title','suptitle','xlabel','ylabel','grid','legend',"
                             "'xlim','ylim','xscale','yscale','semilogx','semilogy','loglog',"
                             "'xticks','yticks','tight_layout','subplots_adjust',"
                             "'clf','cla','close','show','savefig',"
                             "'subplots','subplot','gca','gcf',"
                             "'use','switch_backend','get_backend','rc','rcdefaults',"
                             "'ion','ioff','isinteractive','draw','pause','set_cmap',"
                             "'margins','minorticks_on','minorticks_off','clim','figtext'])")))))))

(defdescribe
  matplotlib-render-test
  (it
    "renders a line plot to a real PNG (correct magic bytes)"
    (with-python-context
      (expect
        (true?
          (png-magic?
            python-context
            "plt.plot([1,2,3,4],[1,4,9,16], label='sq')\nplt.title('t'); plt.xlabel('x'); plt.ylabel('y'); plt.grid(True); plt.legend()")))))
  (it "renders scatter + bar + hist figures to non-empty PNGs"
      (with-python-context (expect (< 100 (png-len python-context "plt.scatter([1,2,3],[3,1,2])")))
                           (expect (< 100 (png-len python-context "plt.bar([1,2,3],[4,5,6])")))
                           (expect (< 100 (png-len python-context "plt.barh([1,2,3],[4,5,6])")))
                           (expect
                             (< 100 (png-len python-context "plt.hist([1,1,2,3,3,3,4], bins=4)")))))
  (it "bar/barh accept string (categorical) x labels via an integer axis"
      (with-python-context
        ;; categorical x renders instead of raising / collapsing to x=0
        (expect (< 100 (png-len python-context "plt.bar(['a','b','c'],[10,20,30])")))
        (expect (< 100 (png-len python-context "plt.barh(['a','b','c'],[10,20,30])")))
        ;; the ASCII backend maps the strings onto an integer axis and prints
        ;; every category name as an x tick label (proves the categorical path)
        (let [ascii (ev python-context
                        (str "import matplotlib.pyplot as plt, io\nplt.clf()\n"
                             "plt.bar(['repo-a','repo-b','repo-c'],[1,2,3])\n"
                             "b=io.StringIO()\nplt.savefig(b, format='txt', width=60, height=12)\n"
                             "b.getvalue()"))]
          (expect (str/includes? ascii "repo-a"))
          (expect (str/includes? ascii "repo-b"))
          (expect (str/includes? ascii "repo-c")))
        ;; barh with string categories renders through the same path
        (expect (str/includes?
                  (ev python-context
                      (str "import matplotlib.pyplot as plt, io\nplt.clf()\n"
                           "plt.barh(['alpha','beta','gamma'],[3,7,2])\n"
                           "b=io.StringIO()\nplt.savefig(b, format='txt', width=60, height=12)\n"
                           "b.getvalue()"))
                  "alpha"))))
  (it
    "renders fill_between / step / axhline / axvline"
    (with-python-context
      (expect (< 100 (png-len python-context "plt.fill_between([0,1,2,3],[0,1,0,2],[0,0,0,0])")))
      (expect (< 100 (png-len python-context "plt.step([0,1,2,3],[1,3,2,4])")))
      (expect
        (<
          100
          (png-len
            python-context
            "plt.plot([0,1,2],[1,2,3])\nplt.axhline(2, linestyle='--')\nplt.axvline(1, linestyle=':')")))))
  (it "renders a pie chart to a real PNG"
      (with-python-context
        (expect (true? (png-magic?
                         python-context
                         "plt.pie([30,20,50], labels=['a','b','c'])\nplt.title('shares')")))))
  (it "renders dashed line styles and markers"
      (with-python-context
        (expect (< 100 (png-len python-context "plt.plot([0,1,2,3],[0,1,4,9], 'r--o')")))
        (expect (< 100
                   (png-len python-context
                            "plt.plot([0,1,2,3],[3,2,1,0], linestyle=':', marker='s')")))))
  (it "renders log-scaled axes (semilogy / loglog)"
      (with-python-context
        (expect (< 100 (png-len python-context "plt.semilogy([1,2,3,4],[10,100,1000,10000])")))
        (expect (< 100 (png-len python-context "plt.loglog([1,10,100],[1,100,10000])")))))
  (it "renders text annotations"
      (with-python-context
        (expect
          (<
            100
            (png-len
              python-context
              "plt.plot([0,1,2],[0,1,2])\nplt.text(1,1,'peak')\nplt.annotate('note', xy=(0,0))")))))
  (it
    "renders multiple series with a legend"
    (with-python-context
      (expect
        (<
          100
          (png-len
            python-context
            "plt.plot([0,1,2,3],[0,1,2,3], label='up')\nplt.plot([0,1,2,3],[3,2,1,0], label='down')\nplt.legend()")))))
  (it "renders an empty figure without error (no series)"
      (with-python-context (expect (< 100 (png-len python-context "plt.title('empty')")))))
  (it "honours figure(figsize=...) — bigger canvas => more bytes"
      (with-python-context
        (let [small
              (png-len python-context "plt.figure(figsize=(2,2)); plt.plot([0,1,2],[0,1,2])")

              big
              (png-len python-context "plt.figure(figsize=(10,8)); plt.plot([0,1,2],[0,1,2])")]

          (expect (< 100 small))
          (expect (< small big))))))

(defdescribe
  matplotlib-oo-api-test
  (it "supports the fig, ax = plt.subplots() object API"
      (with-python-context
        (expect (true? (png-magic?
                         python-context
                         (str "fig, ax = plt.subplots()\n" "ax.plot([1,2,3],[1,4,9], label='sq')\n"
                              "ax.set_title('oo'); ax.set_xlabel('x'); ax.set_ylabel('y')\n"
                              "ax.grid(True); ax.legend()"))))))
  (it
    "subplots(2,1) returns a list of axes, all drawing into the same figure"
    (with-python-context
      (expect
        (true?
          (ev
            python-context
            "import matplotlib.pyplot as plt\nplt.clf()\nfig, axes = plt.subplots(2,1)\nisinstance(axes, list) and len(axes) == 2")))
      (expect
        (<
          100
          (png-len
            python-context
            "fig, axes = plt.subplots(2,1)\naxes[0].bar([1,2,3],[1,2,3])\naxes[1].scatter([1,2,3],[3,2,1])"))))))

(defdescribe matplotlib-savefig-test
             (it "savefig returns the file-like object it wrote to"
                 (with-python-context
                   (expect (true? (ev python-context
                                      (str "import matplotlib.pyplot as plt\nimport io\nplt.clf()\n"
                                           "plt.plot([0,1],[0,1])\n"
                                           "__b = io.BytesIO()\nplt.savefig(__b) is __b")))))))

(defdescribe
  matplotlib-figure-oo-test
  (it "fig.savefig writes a real PNG via the OO subplots API"
      (with-python-context (expect (true?
                                     (ev python-context
                                         (str "import matplotlib.pyplot as plt, io\nplt.clf()\n"
                                              "fig, ax = plt.subplots()\nax.plot([1,2,3],[1,4,9])\n"
                                              "__b = io.BytesIO()\nfig.savefig(__b)\n"
                                              "list(__b.getvalue()[:4]) == [137, 80, 78, 71]"))))))
  (it "fig.suptitle / tight_layout / set_size_inches / add_subplot all render"
      (with-python-context
        (expect (< 100
                   (png-len python-context
                            (str "fig, ax = plt.subplots()\nfig.suptitle('t'); fig.tight_layout()\n"
                                 "fig.set_size_inches(8, 6)\nax.plot([1,2],[1,2])"))))
        (expect (< 100
                   (png-len
                     python-context
                     "fig = plt.figure()\nax = fig.add_subplot(1,1,1)\nax.plot([1,2],[1,2])")))))
  (it "twinx returns an Axes that draws into the same figure"
      (with-python-context
        (expect (< 100
                   (png-len python-context
                            (str "ax = plt.gca()\nax2 = ax.twinx()\n"
                                 "ax.plot([1,2,3],[1,2,3])\nax2.plot([1,2,3],[30,20,10])")))))))

(defdescribe
  matplotlib-expanded-api-test
  (it "publishes the newly added pyplot callables"
      (with-python-context
        (expect (true? (ev python-context
                           (str "import matplotlib.pyplot as plt\n"
                                "all(callable(getattr(plt, n, None)) for n in "
                                "['axis','boxplot','imshow','colorbar','hlines','vlines'])"))))))
  (it
    "plot() with multiple x,y pairs accumulates one series per pair"
    (with-python-context
      (expect
        (=
          2
          (ev
            python-context
            "import matplotlib.pyplot as plt\nplt.clf()\nlen(plt.plot([1,2,3],[1,2,3],[1,2,3],[3,2,1]))")))))
  (it "plot() returns line handles supporting `line, = ...` and set_*"
      (with-python-context
        (expect (true? (ev python-context
                           (str "import matplotlib.pyplot as plt\nplt.clf()\n"
                                "ln, = plt.plot([1,2,3],[1,2,3])\n"
                                "ln.set_label('a'); ln.set_color('red'); ln.set_linestyle('--')\n"
                                "ln.get_label() == 'a'"))))))
  (it "renders boxplot / imshow to real PNGs"
      (with-python-context
        (expect (true? (png-magic? python-context
                                   "plt.boxplot([[1,2,3,4,5],[2,4,6,8,10],[1,1,3,7,9]])")))
        (expect (true? (png-magic? python-context "plt.imshow([[1,2,3],[4,5,6],[7,8,9]])")))))
  (it "renders hlines / vlines and honours a hex color"
      (with-python-context
        (expect (<
                  100
                  (png-len
                    python-context
                    "plt.plot([0,1,2],[0,1,2])\nplt.hlines([1,2],0,2)\nplt.vlines([0.5,1.5],0,2)")))
        (expect (< 100 (png-len python-context "plt.plot([1,2,3],[1,2,3], color='#00ff88')")))))
  (it
    "colorbar is a no-op that does not break rendering"
    (with-python-context
      (expect
        (true?
          (ev
            python-context
            "import matplotlib.pyplot as plt\nplt.clf()\nplt.scatter([1,2],[1,2], c=[1,2])\nplt.colorbar() is None")))
      (expect (< 100
                 (png-len python-context
                          "plt.scatter([1,2,3],[1,2,3], c=[1,2,3])\nplt.colorbar()")))))
  (it "axis('off') renders without a frame; axis([...]) sets limits"
      (with-python-context
        (let [off
              (png-len python-context "plt.plot([1,2,3],[1,2,3])\nplt.axis('off')")

              on
              (png-len python-context "plt.plot([1,2,3],[1,2,3])")]

          (expect (< 100 off))
          (expect (< off on))
          (expect
            (< 100 (png-len python-context "plt.plot([1,2,3],[1,2,3])\nplt.axis([0,5,0,10])")))))))

(defdescribe
  matplotlib-ascii-test
  (it "the ASCII savefig target returns a framed multi-line ASCII plot with title + legend"
      (with-python-context
        (expect (true? (ev python-context
                           (str
                             "import matplotlib.pyplot as plt, math, io\nplt.clf()\n"
                             "xs=[i*0.3 for i in range(21)]\n"
                             "plt.plot(xs,[math.sin(x) for x in xs],label='sin')\n"
                             "plt.plot(xs,[math.cos(x) for x in xs],label='cos')\n"
                             "plt.title('trig'); plt.legend()\n"
                             "b=io.StringIO()\nplt.savefig(b, format='txt', width=50, height=14)\n"
                             "s=b.getvalue()\n"
                             "isinstance(s,str) and 'trig' in s and 'sin' in s "
                             "and 'cos' in s and s.count(chr(10))>=14"))))))
  (it "savefig(format='txt') writes an ASCII render (not a PNG) to a buffer"
      (with-python-context
        (expect (true? (ev python-context
                           (str "import matplotlib.pyplot as plt, io\nplt.clf()\n"
                                "plt.plot([0,1,2,3],[0,1,4,9])\n"
                                "b=io.StringIO()\nplt.savefig(b, format='txt')\nv=b.getvalue()\n"
                                "len(v)>0 and chr(10) in v and not v.startswith(chr(137))"))))))
  (it "show() emits a vis-image fence to stdout and returns None"
      (with-python-context
        (expect (true? (ev python-context
                           (str "import matplotlib.pyplot as plt, io, sys\n" "plt.clf()\n"
                                "plt.bar([1,2,3],[3,7,2]); plt.title('bars')\n"
                                "_o=sys.stdout; sys.stdout=io.StringIO()\n"
                                "_r=plt.show()\n" "_v=sys.stdout.getvalue(); sys.stdout=_o\n"
                                "_r is None and _v.startswith('````vis-image') "
                                "and 'image/png' in _v and 'bars' in _v"))))))
  (it "show() writes a real PNG on disk for the fence path (works even IO-NONE)"
      (with-python-context
        (let [out
              (ev python-context
                  (str "import matplotlib.pyplot as plt, io, sys\n"
                       "plt.clf()\n" "plt.plot([0,1,2],[0,1,4]); plt.title('line')\n"
                       "_o=sys.stdout; sys.stdout=io.StringIO()\nplt.show()\n"
                       "_v=sys.stdout.getvalue(); sys.stdout=_o\n_v"))

              lines
              (str/split-lines out)

              path
              (nth lines 2)

              f
              (java.io.File. ^String path)]

          (expect (str/starts-with? out "````vis-image"))
          (expect (= "image/png" (nth lines 3)))
          (expect (= "640x480" (nth lines 4)))
          (expect (.exists f))
          (expect (> (.length f) 100)))))
  (it "ASCII savefig on an empty figure returns a string without error"
      (with-python-context (expect (true? (ev python-context
                                              (str
                                                "import matplotlib.pyplot as plt, io\nplt.clf()\n"
                                                "b=io.StringIO()\nplt.savefig(b, format='txt')\n"
                                                "isinstance(b.getvalue(), str)"))))))
  (it "ASCII savefig renders a bar chart into the braille canvas"
      (with-python-context
        (expect (true? (ev python-context
                           (str
                             "import matplotlib.pyplot as plt, io\nplt.clf()\n"
                             "plt.bar([1,2,3,4],[3,7,2,5])\n"
                             "b=io.StringIO()\nplt.savefig(b, format='txt', width=40, height=12)\n"
                             "s=b.getvalue()\n"
                             "isinstance(s,str) and any(0x2800<=ord(c)<=0x28ff for c in s)"))))))
  (it "color=True resolves a per-element hex color list without raising (issue #32)"
      (with-python-context
        (expect
          (true?
            (ev python-context
                (str
                  "import matplotlib.pyplot as plt, io\nplt.clf()\n"
                  "plt.bar([1,2,3],[1,4,9],color=['#4C9F70','#123456','#abcdef'])\n"
                  "b=io.StringIO()\nplt.savefig(b, format='txt', width=40, height=12, color=True)\n"
                  "isinstance(b.getvalue(), str)"))))
        (expect
          (true?
            (ev python-context
                (str
                  "import matplotlib.pyplot as plt, io\nplt.clf()\n"
                  "plt.plot([1,2,3],[1,4,9],color='#4C9F70')\n"
                  "plt.plot([1,2,3],[2,5,10],color='green')\n"
                  "b=io.StringIO()\nplt.savefig(b, format='txt', width=40, height=12, color=True)\n"
                  "isinstance(b.getvalue(), str)"))))))
  (it
    "color=True emits ANSI escapes; default stays plain"
    (with-python-context
      (expect
        (true?
          (ev
            python-context
            (str
              "import matplotlib.pyplot as plt, io\nplt.clf()\n"
              "plt.plot([0,1,2,3],[0,1,4,9],label='q')\nplt.legend()\n"
              "bc=io.StringIO()\nplt.savefig(bc, format='txt', width=40, height=12, color=True)\nc=bc.getvalue()\n"
              "bp=io.StringIO()\nplt.savefig(bp, format='txt', width=40, height=12)\np=bp.getvalue()\n"
              "(chr(27) in c) and (chr(27) not in p) " "and '│' in p and '└' in p")))))))

(defdescribe
  matplotlib-explicit-ticks-test
  (it
    "xticks/yticks set and read back positions and labels"
    (with-python-context
      (expect
        (true?
          (ev
            python-context
            (str
              "import matplotlib.pyplot as plt\nplt.clf()\n" "plt.bar([0,1,2],[3,7,2])\n"
              "plt.xticks([0,1,2], ['a','b','c'])\n" "plt.yticks([0,5,10])\n"
              "t, l = plt.xticks()\nyt, yl = plt.yticks()\n"
              "t == [0.0,1.0,2.0] and l == ['a','b','c'] and yt == [0.0,5.0,10.0] and yl == []"))))))
  (it "Axes.set_xticks/set_xticklabels feed the same state"
      (with-python-context
        (expect (true? (ev python-context
                           (str "import matplotlib.pyplot as plt\nplt.clf()\n"
                                "fig, ax = plt.subplots()\n" "ax.bar(range(3), [1,2,3])\n"
                                "ax.set_xticks(range(3))\n"
                                "ax.set_xticklabels(['05.01','12.01','19.01'])\n"
                                "t, l = plt.xticks()\n"
                                "t == [0.0,1.0,2.0] and l == ['05.01','12.01','19.01']"))))))
  (it "the imaging renderer draws the explicit labels (PNG differs from the default locator)"
      (with-python-context
        (expect (true? (ev python-context
                           (str "import matplotlib.pyplot as plt, io\nplt.clf()\n"
                                "plt.bar([0,1,2],[3,7,2])\n"
                                "a = io.BytesIO()\nplt.savefig(a)\n" "plt.clf()\n"
                                "plt.bar([0,1,2],[3,7,2])\n"
                                "plt.xticks([0,1,2], ['alpha','beta','gamma'])\n"
                                "b = io.BytesIO()\nplt.savefig(b)\n"
                                "len(a.getvalue()) > 0 and a.getvalue() != b.getvalue()")))))))

(defdescribe
  matplotlib-mplot3d-test
  (it "projection='3d' returns a real Axes3D and registers mpl_toolkits"
      (with-python-context
        (expect (true?
                  (ev python-context
                      (str
                        "import matplotlib.pyplot as plt\nfrom mpl_toolkits.mplot3d import Axes3D\n"
                        "import mpl_toolkits.mplot3d.axes3d as a3\nplt.clf()\n"
                        "ax = plt.figure().add_subplot(111, projection='3d')\n"
                        "ax.name == '3d' and hasattr(ax, 'plot_surface') and plt.gca() is ax"
                        " and Axes3D is not None and hasattr(a3, 'Axes3D')"))))))
  (it "plot_surface and plot_wireframe render distinct PNGs from the same grid"
      (with-python-context
        (expect
          (true?
            (ev
              python-context
              (str
                "import matplotlib.pyplot as plt, io\nplt.clf()\n"
                "X = [[float(j) for j in range(6)] for i in range(6)]\n"
                "Y = [[float(i) for j in range(6)] for i in range(6)]\n"
                "Z = [[float(i * j) for j in range(6)] for i in range(6)]\n"
                "plt.figure().add_subplot(projection='3d').plot_surface(X, Y, Z, cmap='viridis')\n"
                "a = io.BytesIO()\nplt.savefig(a)\nplt.clf()\n"
                "plt.figure().add_subplot(projection='3d').plot_wireframe(X, Y, Z)\n"
                "b = io.BytesIO()\nplt.savefig(b)\n"
                "list(a.getvalue()[:8]) == [137, 80, 78, 71, 13, 10, 26, 10]"
                " and len(a.getvalue()) > 0 and a.getvalue() != b.getvalue()"))))))
  (it "view_init rotates the camera: the same surface renders differently"
      (with-python-context
        (expect (true? (ev python-context
                           (str "import matplotlib.pyplot as plt, io\n"
                                "X = [[float(j) for j in range(5)] for i in range(5)]\n"
                                "Y = [[float(i) for j in range(5)] for i in range(5)]\n"
                                "Z = [[float(i + j) for j in range(5)] for i in range(5)]\n"
                                "def shot(elev, azim):\n"
                                "    plt.clf()\n"
                                "    ax = plt.figure().add_subplot(projection='3d')\n"
                                "    ax.plot_surface(X, Y, Z)\n"
                                "    ax.view_init(elev=elev, azim=azim)\n"
                                "    buf = io.BytesIO()\n" "    plt.savefig(buf)\n"
                                "    return buf.getvalue()\n" "shot(30, -60) != shot(72, 15)"))))))
  (it "1-D x/y broadcast over a 2-D Z the way meshgrid output does"
      (with-python-context
        (expect (true? (ev python-context
                           (str
                             "import matplotlib.pyplot as plt, io\nplt.clf()\n"
                             "Z = [[float(i * i + j) for j in range(7)] for i in range(7)]\n"
                             "ax = plt.figure().add_subplot(projection='3d')\n"
                             "ax.plot_surface(list(range(7)), list(range(7)), Z, cmap='plasma')\n"
                             "buf = io.BytesIO()\nplt.savefig(buf)\n"
                             "list(buf.getvalue()[:8]) == [137, 80, 78, 71, 13, 10, 26, 10]"))))))
  (it "3-D scatter / line / bar3d / contour(offset) all reach the renderer"
      (with-python-context
        (expect
          (true? (ev python-context
                     (str "import matplotlib.pyplot as plt, io\nplt.clf()\n"
                          "X = [[float(j) for j in range(6)] for i in range(6)]\n"
                          "Y = [[float(i) for j in range(6)] for i in range(6)]\n"
                          "Z = [[float(i * j) for j in range(6)] for i in range(6)]\n"
                          "ax = plt.figure().add_subplot(projection='3d')\n"
                          "ax.plot([0, 1, 2], [0, 1, 0], [0, 2, 1], 'r-o', label='path')\n"
                          "ax.scatter([0, 1], [1, 0], [2, 0], c=[0.1, 0.9], cmap='plasma', s=40)\n"
                          "ax.bar3d([0, 1], [0, 1], [0, 0], 0.5, 0.5, [1, 2], color='tab:orange')\n"
                          "cs = ax.contour(X, Y, Z, levels=4, offset=0.0)\n"
                          "ax.set_zlabel('z'); ax.set_zlim(0, 30); ax.text(0, 0, 1, 'here')\n"
                          "buf = io.BytesIO()\nplt.savefig(buf)\n"
                          "len(cs) > 0 and ax.get_zlim() == [0.0, 30.0]"
                          " and list(buf.getvalue()[:8]) == [137, 80, 78, 71, 13, 10, 26, 10]"))))))
  (it "a 3-D figure also renders to the ASCII target"
      (with-python-context
        (expect
          (let [s (ev python-context
                      (str "import matplotlib.pyplot as plt, io\nplt.clf()\n"
                           "Z = [[float(i * j) for j in range(6)] for i in range(6)]\n"
                           "ax = plt.figure().add_subplot(projection='3d')\n"
                           "ax.plot_surface(list(range(6)), list(range(6)), Z, cmap='viridis')\n"
                           "ax.set_title('mesh')\n"
                           "buf = io.StringIO()\nplt.savefig(buf, format='txt')\nbuf.getvalue()"))]
            (and (string? s)
                 (str/includes? s "3-D view")
                 (str/includes? s "mesh")
                 (> (count (str/split-lines s)) 10)))))))

(defdescribe
  matplotlib-figure-artist-test
  "Figure/Axes styling boilerplate written against real matplotlib -- the
   background `patch` artist, dpi/size accessors, canvas and gridspec -- runs
   without raising and leaves rendering intact."
  (it "fig.patch and ax.patch accept the usual artist setters"
      (with-python-context
        (expect (true? (ev python-context
                           (str "import matplotlib.pyplot as plt\n" "plt.clf()\n"
                                "fig, ax = plt.subplots()\n" "fig.patch.set_facecolor('#101014')\n"
                                "fig.patch.set_alpha(0.5)\n" "ax.patch.set_facecolor('#222222')\n"
                                "fig.set_edgecolor('white')\n" "fig.get_facecolor() == '#101014' "
                                "and ax.patch.get_facecolor() == '#222222' "
                                "and fig.patch is plt.gcf().patch"))))))
  (it "dpi and size accessors round-trip"
      (with-python-context
        (expect (true? (ev python-context
                           (str "import matplotlib.pyplot as plt\n"
                                "plt.clf()\n" "fig = plt.figure(figsize=(8,4), dpi=110)\n"
                                "ok = fig.dpi == 110.0 and fig.get_size_inches() == (8.0, 4.0)\n"
                                "fig.set_size_inches(6, 3)\n"
                                "ok and fig.get_size_inches() == (6.0, 3.0) "
                                "and fig.canvas.get_width_height() == (660, 330)"))))))
  (it "canvas / gridspec / layout helpers are no-op safe"
      (with-python-context
        (expect (true? (ev python-context
                           (str "import matplotlib.pyplot as plt\n" "plt.clf()\n"
                                "fig, ax = plt.subplots()\n" "fig.canvas.draw()\n"
                                "fig.canvas.flush_events()\n" "gs = fig.add_gridspec(2, 2)\n"
                                "ax2 = fig.add_subplot(gs[0, 1])\n"
                                "fig.supxlabel('t'); fig.supylabel('v')\n"
                                "fig.set_tight_layout(True); fig.autofmt_xdate()\n"
                                "ax2 is not None and len(fig.axes) == 1"))))))
  (it "a styled figure still renders a PNG"
      (with-python-context (expect (< 100
                                      (png-len python-context
                                               (str "fig, ax = plt.subplots()\n"
                                                    "fig.patch.set_facecolor('#101014')\n"
                                                    "ax.patch.set_facecolor('#202024')\n"
                                                    "fig.set_dpi(120)\n"
                                                    "ax.plot([1,2,3],[3,1,2])")))))))
(it
  "FigureCanvasAgg accepts and retains an OO figure"
  (with-python-context
    (expect
      (true?
        (ev
          python-context
          (str
            "from matplotlib.backends.backend_agg import FigureCanvasAgg\n"
            "from matplotlib.figure import Figure\n"
            "fig = Figure()\ncanvas = FigureCanvasAgg(fig)\n"
            "canvas.figure is fig and fig.canvas.figure is fig and canvas.get_width_height() == (640, 480)"))))))

(defdescribe
  matplotlib-submodule-imports-test
  "Real plotting code imports from matplotlib SUBMODULES -- `from matplotlib.patches
   import Rectangle`, `matplotlib.ticker`, `matplotlib.colors`,
   `mpl_toolkits.axes_grid1`. Until they were registered in `sys.modules`, every one
   of those imports died with `matplotlib is not a package`."
  (it
    "the shimmed submodules import and mirror the package attributes"
    (with-python-context
      (expect
        (true?
          (ev
            python-context
            "
def _run():
    import sys
    mods = ['matplotlib.patches', 'matplotlib.colors', 'matplotlib.ticker',
            'matplotlib.lines', 'matplotlib.collections', 'matplotlib.text',
            'matplotlib.legend', 'matplotlib.transforms', 'matplotlib.dates',
            'matplotlib.figure', 'matplotlib.gridspec', 'matplotlib.axes',
            'matplotlib.axis', 'matplotlib.artist', 'matplotlib.font_manager',
            'matplotlib.image', 'matplotlib.animation', 'matplotlib.path',
            'matplotlib.patheffects', 'matplotlib.offsetbox', 'matplotlib.spines',
            'matplotlib.markers', 'matplotlib.colorbar', 'matplotlib.container',
            'matplotlib.backends.backend_agg', 'mpl_toolkits.mplot3d',
            'mpl_toolkits.axes_grid1']
    for m in mods:
        __import__(m)
        if sys.modules.get(m) is None:
            return 'not in sys.modules: ' + m
    from matplotlib.patches import Rectangle, Circle, Ellipse, Polygon, Wedge
    from matplotlib.colors import (Normalize, LogNorm, ListedColormap,
                                   LinearSegmentedColormap, to_hex, to_rgba)
    from matplotlib.ticker import (FuncFormatter, MultipleLocator, MaxNLocator,
                                   PercentFormatter)
    from matplotlib.lines import Line2D
    from matplotlib.collections import LineCollection
    from matplotlib.transforms import Bbox, Affine2D
    from matplotlib.dates import date2num, num2date, DateFormatter
    from matplotlib.artist import setp, getp
    from mpl_toolkits.axes_grid1 import make_axes_locatable
    import matplotlib
    if matplotlib.patches.Rectangle is not Rectangle:
        return 'matplotlib.patches attribute is not the submodule class'
    if matplotlib.colors.Normalize is not Normalize:
        return 'matplotlib.colors attribute is not the submodule class'
    return True
_run()
")))))
  (it
    "patches, ticker, colors, dates and artist helpers behave"
    (with-python-context
      (expect
        (true?
          (ev
            python-context
            "
def _run():
    import matplotlib.pyplot as plt
    from matplotlib.patches import Rectangle
    from matplotlib.ticker import FuncFormatter, MultipleLocator
    from matplotlib.colors import Normalize, to_rgba
    from matplotlib.dates import date2num, num2date
    from matplotlib.artist import setp
    from mpl_toolkits.axes_grid1 import make_axes_locatable
    import datetime
    plt.clf()
    fig, ax = plt.subplots()
    rect = Rectangle((0, 0), 2, 3, facecolor='#101014')
    ax.add_patch(rect)
    ax.xaxis.set_major_formatter(FuncFormatter(lambda v, p: '%.1f' % v))
    ax.xaxis.set_major_locator(MultipleLocator(0.5))
    line, = ax.plot([1, 2, 3], [3, 1, 2])
    setp(line, linewidth=2.0)
    checks = []
    checks.append(rect.get_bbox().bounds == (0.0, 0.0, 2.0, 3.0))
    checks.append(sorted(ax.spines.keys()) == ['bottom', 'left', 'right', 'top'])
    checks.append(MultipleLocator(0.5).tick_values(0, 2) == [0.0, 0.5, 1.0, 1.5, 2.0])
    checks.append(Normalize(0, 10)(5) == 0.5)
    checks.append(to_rgba('#ff0000') == (1.0, 0.0, 0.0, 1.0))
    d = datetime.datetime(2024, 1, 2)
    checks.append(abs(date2num(num2date(date2num(d))) - date2num(d)) < 1e-6)
    checks.append(rect in ax.get_children())
    checks.append(make_axes_locatable(ax) is not None)
    return all(checks)
_run()
")))))
  (it "a figure assembled through the submodule API still renders a PNG"
      (with-python-context
        (expect (< 100
                   (png-len python-context
                            (str "from matplotlib.patches import Rectangle\n"
                                 "from matplotlib.ticker import MultipleLocator\n"
                                 "fig, ax = plt.subplots()\n"
                                 "ax.add_patch(Rectangle((0, 0), 1, 2, facecolor='#101014'))\n"
                                 "ax.xaxis.set_major_locator(MultipleLocator(0.5))\n"
                                 "ax.plot([1, 2, 3], [3, 1, 2])")))))))

;; Regression: `savefig(pathlib.Path('plot.txt'))` wrote PNG BYTES into the .txt.
;; Only a str filename was tested for the .txt/.asc suffix, so one path in two
;; spellings produced two different formats and neither call complained.
(defdescribe savefig-takes-a-path-object-test
  (it "reads the ASCII suffix off an os.PathLike, like the str spelling of it"
      (let [dir (tmp-dir)]
        (with-fs-context dir
          (expect (= [true true]
                     (ev python-context
                         (str "import pathlib\n"
                              "import matplotlib.pyplot as plt\n"
                              "d = pathlib.Path('" dir "')\n"
                              "plt.clf()\n"
                              "plt.plot([1, 2, 3], [3, 1, 2])\n"
                              "plt.savefig(d / 'via_path.txt')\n"
                              "plt.savefig(str(d / 'via_str.txt'))\n"
                              "a = (d / 'via_path.txt').read_bytes()\n"
                              ;; 137 is the first byte of the PNG signature.
                              "[a[0] != 137, a == (d / 'via_str.txt').read_bytes()]"))))))))
