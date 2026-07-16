(ns com.blockether.vis.internal.matplotlib-compat-shim-test
  "The matplotlib-compat shim installed into every sandbox context via the
   generic sandbox-shim mechanism (`extension/sandbox-shims`): a minimal
   `matplotlib.pyplot` published into `sys.modules` (so `import matplotlib.pyplot`
   works) and backed by a pure-JVM Java2D renderer. `savefig` delegates the
   accumulated figure across the boundary to the host `__vis_mpl_render__`,
   which returns a PNG the shim writes to a path or file-like buffer."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.env-python :as ep]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [org.graalvm.polyglot Context]))

(defn- ev [^Context c code] (ep/->clj (.eval c "python" code)))

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
  `(let [~(with-meta 'python-context {:tag `Context}) (:python-context (ep/create-python-context
                                                                         {}))]
     (try ~@body (finally (.close ~'python-context)))))

(defdescribe
  matplotlib-module-test
  (it "publishes matplotlib + matplotlib.pyplot under sys.modules"
      (with-python-context
        (expect (true? (ev python-context
                           "__import__('sys').modules.get('matplotlib.pyplot') is not None")))
        (expect (true? (ev python-context
                           "__import__('sys').modules.get('matplotlib') is not None")))))
  (it "autoloads `matplotlib` onto builtins (no import needed)"
      (with-python-context
        ;; deliberately NO import
        (expect (true? (ev python-context "matplotlib.pyplot is not None")))))
  (it "autoloads a bare `pyplot` and the `plt` alias onto builtins (no import)"
      (with-python-context (expect (true? (ev python-context "pyplot is not None")))
                           (expect (true? (ev python-context "plt is not None")))
                           ;; all three names resolve to the same module object
                           (expect (true? (ev python-context
                                              "plt is pyplot and plt is matplotlib.pyplot")))))
  (it "exposes a version string"
      (with-python-context (expect (= "3.0-vis-java2d"
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
                        (str "import matplotlib.pyplot as plt\nplt.clf()\n"
                             "plt.bar(['repo-a','repo-b','repo-c'],[1,2,3])\n"
                             "plt.to_ascii(60,12)"))]
          (expect (str/includes? ascii "repo-a"))
          (expect (str/includes? ascii "repo-b"))
          (expect (str/includes? ascii "repo-c")))
        ;; barh with string categories renders through the same path
        (expect (str/includes? (ev python-context
                                   (str "import matplotlib.pyplot as plt\nplt.clf()\n"
                                        "plt.barh(['alpha','beta','gamma'],[3,7,2])\n"
                                        "plt.to_ascii(60,12)"))
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
  (it "to_ascii returns a framed multi-line ASCII plot with title + legend"
      (with-python-context
        (expect (true? (ev python-context
                           (str "import matplotlib.pyplot as plt, math\nplt.clf()\n"
                                "xs=[i*0.3 for i in range(21)]\n"
                                "plt.plot(xs,[math.sin(x) for x in xs],label='sin')\n"
                                "plt.plot(xs,[math.cos(x) for x in xs],label='cos')\n"
                                "plt.title('trig'); plt.legend()\n" "s=plt.to_ascii(50,14)\n"
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
  (it "to_ascii on an empty figure returns a string without error"
      (with-python-context
        (expect
          (true?
            (ev python-context
                "import matplotlib.pyplot as plt\nplt.clf()\nisinstance(plt.to_ascii(), str)")))))
  (it "to_ascii renders a bar chart into the braille canvas"
      (with-python-context
        (expect (true? (ev python-context
                           (str "import matplotlib.pyplot as plt\nplt.clf()\n"
                                "plt.bar([1,2,3,4],[3,7,2,5])\n"
                                "s=plt.to_ascii(40,12)\n"
                                "isinstance(s,str) and any(0x2800<=ord(c)<=0x28ff for c in s)"))))))
  (it "to_ascii(color=True) resolves a per-element hex color list without raising (issue #32)"
      (with-python-context
        (expect (true? (ev python-context
                           (str "import matplotlib.pyplot as plt\nplt.clf()\n"
                                "plt.bar([1,2,3],[1,4,9],color=['#4C9F70','#123456','#abcdef'])\n"
                                "isinstance(plt.to_ascii(40,12,color=True), str)"))))
        (expect (true? (ev python-context
                           (str "import matplotlib.pyplot as plt\nplt.clf()\n"
                                "plt.plot([1,2,3],[1,4,9],color='#4C9F70')\n"
                                "plt.plot([1,2,3],[2,5,10],color='green')\n"
                                "isinstance(plt.to_ascii(40,12,color=True), str)"))))))
  (it "to_ascii(color=True) emits ANSI escapes; default stays plain"
      (with-python-context
        (expect (true? (ev python-context
                           (str "import matplotlib.pyplot as plt\nplt.clf()\n"
                                "plt.plot([0,1,2,3],[0,1,4,9],label='q')\nplt.legend()\n"
                                "c=plt.to_ascii(40,12,color=True)\np=plt.to_ascii(40,12)\n"
                                "(chr(27) in c) and (chr(27) not in p) "
                                "and '\u2502' in p and '\u2514' in p")))))))
