(ns com.blockether.vis.internal.matplotlib-compat-shim-test
  "The matplotlib-compat shim installed into every sandbox context via the
   generic sandbox-shim mechanism (`extension/sandbox-shims`): a minimal
   `matplotlib.pyplot` published into `sys.modules` (so `import matplotlib.pyplot`
   works) and backed by a pure-JVM Java2D renderer. `savefig` delegates the
   accumulated figure across the boundary to the host `__vis_mpl_render__`,
   which returns a PNG the shim writes to a path or file-like buffer."
  (:require [com.blockether.vis.internal.env-python :as ep]
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

(defdescribe
  matplotlib-module-test
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
  (it "autoloads a bare `pyplot` and the `plt` alias onto builtins (no import)"
      (let [{:keys [^Context python-context]} (ep/create-python-context {})]
        (expect (true? (ev python-context "pyplot is not None")))
        (expect (true? (ev python-context "plt is not None")))
        ;; all three names resolve to the same module object
        (expect (true? (ev python-context "plt is pyplot and plt is matplotlib.pyplot")))))
  (it "exposes a version string"
      (let [{:keys [^Context python-context]} (ep/create-python-context {})]
        (expect (= "3.0-vis-java2d" (ev python-context "__import__('matplotlib').__version__")))))
  (it "exposes matplotlib.style with a no-op use()"
      (let [{:keys [^Context python-context]} (ep/create-python-context {})]
        (expect (true? (ev python-context
                           "import matplotlib.style as st\nst.use('ggplot') is None"))))))

(defdescribe
  matplotlib-api-surface-test
  (it "publishes the expected pyplot callables"
      (let [{:keys [^Context python-context]} (ep/create-python-context {})]
        (expect (true? (ev python-context
                           (str "import matplotlib.pyplot as plt\n"
                                "all(callable(getattr(plt, n, None)) for n in "
                                "['plot','scatter','bar','barh','hist','fill_between','step',"
                                "'pie','axhline','axvline','errorbar','text','annotate',"
                                "'title','suptitle','xlabel','ylabel','grid','legend',"
                                "'xlim','ylim','xscale','yscale','semilogx','semilogy','loglog',"
                                "'xticks','yticks','tight_layout','subplots_adjust',"
                                "'clf','cla','close','show','savefig',"
                                "'subplots','subplot','gca','gcf'])")))))))

(defdescribe
  matplotlib-render-test
  (it
    "renders a line plot to a real PNG (correct magic bytes)"
    (let [{:keys [^Context python-context]} (ep/create-python-context {})]
      (expect
        (true?
          (png-magic?
            python-context
            "plt.plot([1,2,3,4],[1,4,9,16], label='sq')\nplt.title('t'); plt.xlabel('x'); plt.ylabel('y'); plt.grid(True); plt.legend()")))))
  (it "renders scatter + bar + hist figures to non-empty PNGs"
      (let [{:keys [^Context python-context]} (ep/create-python-context {})]
        (expect (< 100 (png-len python-context "plt.scatter([1,2,3],[3,1,2])")))
        (expect (< 100 (png-len python-context "plt.bar([1,2,3],[4,5,6])")))
        (expect (< 100 (png-len python-context "plt.barh([1,2,3],[4,5,6])")))
        (expect (< 100 (png-len python-context "plt.hist([1,1,2,3,3,3,4], bins=4)")))))
  (it
    "renders fill_between / step / axhline / axvline"
    (let [{:keys [^Context python-context]} (ep/create-python-context {})]
      (expect (< 100 (png-len python-context "plt.fill_between([0,1,2,3],[0,1,0,2],[0,0,0,0])")))
      (expect (< 100 (png-len python-context "plt.step([0,1,2,3],[1,3,2,4])")))
      (expect
        (<
          100
          (png-len
            python-context
            "plt.plot([0,1,2],[1,2,3])\nplt.axhline(2, linestyle='--')\nplt.axvline(1, linestyle=':')")))))
  (it "renders a pie chart to a real PNG"
      (let [{:keys [^Context python-context]} (ep/create-python-context {})]
        (expect (true? (png-magic?
                         python-context
                         "plt.pie([30,20,50], labels=['a','b','c'])\nplt.title('shares')")))))
  (it "renders dashed line styles and markers"
      (let [{:keys [^Context python-context]} (ep/create-python-context {})]
        (expect (< 100 (png-len python-context "plt.plot([0,1,2,3],[0,1,4,9], 'r--o')")))
        (expect (< 100
                   (png-len python-context
                            "plt.plot([0,1,2,3],[3,2,1,0], linestyle=':', marker='s')")))))
  (it "renders log-scaled axes (semilogy / loglog)"
      (let [{:keys [^Context python-context]} (ep/create-python-context {})]
        (expect (< 100 (png-len python-context "plt.semilogy([1,2,3,4],[10,100,1000,10000])")))
        (expect (< 100 (png-len python-context "plt.loglog([1,10,100],[1,100,10000])")))))
  (it "renders text annotations"
      (let [{:keys [^Context python-context]} (ep/create-python-context {})]
        (expect
          (<
            100
            (png-len
              python-context
              "plt.plot([0,1,2],[0,1,2])\nplt.text(1,1,'peak')\nplt.annotate('note', xy=(0,0))")))))
  (it
    "renders multiple series with a legend"
    (let [{:keys [^Context python-context]} (ep/create-python-context {})]
      (expect
        (<
          100
          (png-len
            python-context
            "plt.plot([0,1,2,3],[0,1,2,3], label='up')\nplt.plot([0,1,2,3],[3,2,1,0], label='down')\nplt.legend()")))))
  (it "renders an empty figure without error (no series)"
      (let [{:keys [^Context python-context]} (ep/create-python-context {})]
        (expect (< 100 (png-len python-context "plt.title('empty')")))))
  (it "honours figure(figsize=...) — bigger canvas => more bytes"
      (let [{:keys [^Context python-context]}
            (ep/create-python-context {})

            small
            (png-len python-context "plt.figure(figsize=(2,2)); plt.plot([0,1,2],[0,1,2])")

            big
            (png-len python-context "plt.figure(figsize=(10,8)); plt.plot([0,1,2],[0,1,2])")]

        (expect (< 100 small))
        (expect (< small big)))))

(defdescribe
  matplotlib-oo-api-test
  (it "supports the fig, ax = plt.subplots() object API"
      (let [{:keys [^Context python-context]} (ep/create-python-context {})]
        (expect (true? (png-magic?
                         python-context
                         (str "fig, ax = plt.subplots()\n" "ax.plot([1,2,3],[1,4,9], label='sq')\n"
                              "ax.set_title('oo'); ax.set_xlabel('x'); ax.set_ylabel('y')\n"
                              "ax.grid(True); ax.legend()"))))))
  (it
    "subplots(2,1) returns a list of axes, all drawing into the same figure"
    (let [{:keys [^Context python-context]} (ep/create-python-context {})]
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
                 (let [{:keys [^Context python-context]} (ep/create-python-context {})]
                   (expect (true? (ev python-context
                                      (str "import matplotlib.pyplot as plt\nimport io\nplt.clf()\n"
                                           "plt.plot([0,1],[0,1])\n"
                                           "__b = io.BytesIO()\nplt.savefig(__b) is __b")))))))

(defdescribe
  matplotlib-figure-oo-test
  (it "fig.savefig writes a real PNG via the OO subplots API"
      (let [{:keys [^Context python-context]} (ep/create-python-context {})]
        (expect (true? (ev python-context
                           (str "import matplotlib.pyplot as plt, io\nplt.clf()\n"
                                "fig, ax = plt.subplots()\nax.plot([1,2,3],[1,4,9])\n"
                                "__b = io.BytesIO()\nfig.savefig(__b)\n"
                                "list(__b.getvalue()[:4]) == [137, 80, 78, 71]"))))))
  (it "fig.suptitle / tight_layout / set_size_inches / add_subplot all render"
      (let [{:keys [^Context python-context]} (ep/create-python-context {})]
        (expect (< 100
                   (png-len python-context
                            (str "fig, ax = plt.subplots()\nfig.suptitle('t'); fig.tight_layout()\n"
                                 "fig.set_size_inches(8, 6)\nax.plot([1,2],[1,2])"))))
        (expect (< 100
                   (png-len
                     python-context
                     "fig = plt.figure()\nax = fig.add_subplot(1,1,1)\nax.plot([1,2],[1,2])")))))
  (it "twinx returns an Axes that draws into the same figure"
      (let [{:keys [^Context python-context]} (ep/create-python-context {})]
        (expect (< 100
                   (png-len python-context
                            (str "ax = plt.gca()\nax2 = ax.twinx()\n"
                                 "ax.plot([1,2,3],[1,2,3])\nax2.plot([1,2,3],[30,20,10])")))))))

(defdescribe
  matplotlib-expanded-api-test
  (it "publishes the newly added pyplot callables"
      (let [{:keys [^Context python-context]} (ep/create-python-context {})]
        (expect (true? (ev python-context
                           (str "import matplotlib.pyplot as plt\n"
                                "all(callable(getattr(plt, n, None)) for n in "
                                "['axis','boxplot','imshow','colorbar','hlines','vlines'])"))))))
  (it
    "plot() with multiple x,y pairs accumulates one series per pair"
    (let [{:keys [^Context python-context]} (ep/create-python-context {})]
      (expect
        (=
          2
          (ev
            python-context
            "import matplotlib.pyplot as plt\nplt.clf()\nlen(plt.plot([1,2,3],[1,2,3],[1,2,3],[3,2,1]))")))))
  (it "plot() returns line handles supporting `line, = ...` and set_*"
      (let [{:keys [^Context python-context]} (ep/create-python-context {})]
        (expect (true? (ev python-context
                           (str "import matplotlib.pyplot as plt\nplt.clf()\n"
                                "ln, = plt.plot([1,2,3],[1,2,3])\n"
                                "ln.set_label('a'); ln.set_color('red'); ln.set_linestyle('--')\n"
                                "ln.get_label() == 'a'"))))))
  (it "renders boxplot / imshow to real PNGs"
      (let [{:keys [^Context python-context]} (ep/create-python-context {})]
        (expect (true? (png-magic? python-context
                                   "plt.boxplot([[1,2,3,4,5],[2,4,6,8,10],[1,1,3,7,9]])")))
        (expect (true? (png-magic? python-context "plt.imshow([[1,2,3],[4,5,6],[7,8,9]])")))))
  (it "renders hlines / vlines and honours a hex color"
      (let [{:keys [^Context python-context]} (ep/create-python-context {})]
        (expect (<
                  100
                  (png-len
                    python-context
                    "plt.plot([0,1,2],[0,1,2])\nplt.hlines([1,2],0,2)\nplt.vlines([0.5,1.5],0,2)")))
        (expect (< 100 (png-len python-context "plt.plot([1,2,3],[1,2,3], color='#00ff88')")))))
  (it
    "colorbar is a no-op that does not break rendering"
    (let [{:keys [^Context python-context]} (ep/create-python-context {})]
      (expect
        (true?
          (ev
            python-context
            "import matplotlib.pyplot as plt\nplt.clf()\nplt.scatter([1,2],[1,2], c=[1,2])\nplt.colorbar() is None")))
      (expect (< 100
                 (png-len python-context
                          "plt.scatter([1,2,3],[1,2,3], c=[1,2,3])\nplt.colorbar()")))))
  (it "axis('off') renders without a frame; axis([...]) sets limits"
      (let [{:keys [^Context python-context]}
            (ep/create-python-context {})

            off
            (png-len python-context "plt.plot([1,2,3],[1,2,3])\nplt.axis('off')")

            on
            (png-len python-context "plt.plot([1,2,3],[1,2,3])")]

        (expect (< 100 off))
        (expect (< off on))
        (expect (< 100
                   (png-len python-context "plt.plot([1,2,3],[1,2,3])\nplt.axis([0,5,0,10])"))))))

(defdescribe
  matplotlib-ascii-test
  (it "to_ascii returns a framed multi-line ASCII plot with title + legend"
      (let [{:keys [^Context python-context]} (ep/create-python-context {})]
        (expect (true? (ev python-context
                           (str "import matplotlib.pyplot as plt, math\nplt.clf()\n"
                                "xs=[i*0.3 for i in range(21)]\n"
                                "plt.plot(xs,[math.sin(x) for x in xs],label='sin')\n"
                                "plt.plot(xs,[math.cos(x) for x in xs],label='cos')\n"
                                "plt.title('trig'); plt.legend()\n" "s=plt.to_ascii(50,14)\n"
                                "isinstance(s,str) and 'trig' in s and 'sin' in s "
                                "and 'cos' in s and s.count(chr(10))>=14"))))))
  (it "savefig(format='txt') writes an ASCII render (not a PNG) to a buffer"
      (let [{:keys [^Context python-context]} (ep/create-python-context {})]
        (expect (true? (ev python-context
                           (str "import matplotlib.pyplot as plt, io\nplt.clf()\n"
                                "plt.plot([0,1,2,3],[0,1,4,9])\n"
                                "b=io.StringIO()\nplt.savefig(b, format='txt')\nv=b.getvalue()\n"
                                "len(v)>0 and chr(10) in v and not v.startswith(chr(137))"))))))
  (it "show() renders ASCII to stdout and returns None"
      (let [{:keys [^Context python-context]} (ep/create-python-context {})]
        (expect (true? (ev python-context
                           (str "import matplotlib.pyplot as plt\nplt.clf()\n"
                                "plt.bar([1,2,3],[3,7,2]); plt.title('bars')\n"
                                "plt.show() is None"))))))
  (it "to_ascii on an empty figure returns a string without error"
      (let [{:keys [^Context python-context]} (ep/create-python-context {})]
        (expect
          (true?
            (ev python-context
                "import matplotlib.pyplot as plt\nplt.clf()\nisinstance(plt.to_ascii(), str)")))))
  (it "to_ascii renders a bar chart into the braille canvas"
      (let [{:keys [^Context python-context]} (ep/create-python-context {})]
        (expect (true? (ev python-context
                           (str "import matplotlib.pyplot as plt\nplt.clf()\n"
                                "plt.bar([1,2,3,4],[3,7,2,5])\n"
                                "s=plt.to_ascii(40,12)\n"
                                "isinstance(s,str) and any(0x2800<=ord(c)<=0x28ff for c in s)"))))))
  (it "to_ascii(color=True) emits ANSI escapes; default stays plain"
      (let [{:keys [^Context python-context]} (ep/create-python-context {})]
        (expect (true? (ev python-context
                           (str "import matplotlib.pyplot as plt\nplt.clf()\n"
                                "plt.plot([0,1,2,3],[0,1,4,9],label='q')\nplt.legend()\n"
                                "c=plt.to_ascii(40,12,color=True)\np=plt.to_ascii(40,12)\n"
                                "(chr(27) in c) and (chr(27) not in p) "
                                "and '\u2502' in p and '\u2514' in p")))))))
