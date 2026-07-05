(ns com.blockether.vis.internal.foundation.shim-matplotlib
  "Built-in sandbox SHIM: a minimal `matplotlib.pyplot`-compatible module for the
   model's Python sandbox, backed by a pure-JVM Java2D renderer. The agent
   sandbox ships no CPython matplotlib wheel (it needs numpy's native core +
   freetype, all blocked by the deny-by-default Context); this extension instead
   contributes a `:ext/sandbox-shims` entry whose Python preamble accumulates the
   familiar pyplot artists (`plot`/`scatter`/`bar`/`hist`/`title`/`xlabel`/…) and
   whose `savefig` DELEGATES the whole figure spec across the boundary to the host
   callable `__vis_mpl_render__`, which draws it with `java.awt`/`ImageIO` and
   returns a base64 PNG. The Python side base64-decodes and writes it (to a path,
   confined to the sandbox roots, or to any file-like buffer).

   It is a SUBSET, not real matplotlib: line/scatter/bar/hist with title, axis
   labels, grid and legend — enough for the model to visualize data. Rendering
   runs entirely on the JVM (no pip, no native wheels), and any render failure
   surfaces to Python as a catchable exception (never crashes the sandbox).

   Together with `shim-yaml` this demonstrates the sandbox-shim mechanism: an
   extension turns a host / JVM capability into a real importable Python module
   while `env-python` stays completely generic about which shims exist."
  (:require
   [com.blockether.vis.core :as vis])
  (:import
   [java.awt BasicStroke Color Font Graphics2D RenderingHints]
   [java.awt.image BufferedImage]
   [java.io ByteArrayOutputStream]
   [java.util Base64 Locale]
   [javax.imageio ImageIO]))

;; ----------------------------------------------------------------------------
;; Host renderer — Java2D. Input is the pyplot figure spec (string-keyed map
;; marshalled from the sandbox); output is a base64 PNG string. Kept dependency
;; free (only the JDK's AWT + ImageIO, already reachable in the native image).
;; ----------------------------------------------------------------------------

(def ^:private palette
  "matplotlib tab10-ish default series colors, cycled by series index."
  [[31 119 180] [255 127 14] [44 160 44] [214 39 40] [148 103 189]
   [140 86 75] [227 119 194] [127 127 127] [188 189 34] [23 190 207]])

(def ^:private named-colors
  {"b" [0 0 255] "g" [0 128 0] "r" [255 0 0] "c" [0 191 191]
   "m" [191 0 191] "y" [191 191 0] "k" [0 0 0] "w" [255 255 255]
   "blue" [0 0 255] "green" [0 128 0] "red" [255 0 0] "cyan" [0 191 191]
   "magenta" [191 0 191] "yellow" [191 191 0] "black" [0 0 0] "white" [255 255 255]
   "orange" [255 127 14] "purple" [148 103 189] "gray" [127 127 127]
   "grey" [127 127 127] "brown" [140 86 75] "pink" [227 119 194]})

(defn- ->color ^Color [c idx]
  (let [rgb (or (when (and (string? c) (seq c))
                  (get named-colors (.toLowerCase ^String (str c))))
              (nth palette (mod (int idx) (count palette))))]
    (Color. (int (nth rgb 0)) (int (nth rgb 1)) (int (nth rgb 2)))))

(defn- as-double ^double [x]
  (cond
    (number? x) (double x)
    :else (try (Double/parseDouble (str x)) (catch Exception _ 0.0))))

(defn- fmt-num ^String [^double v]
  (if (== v (Math/rint v))
    (str (long v))
    (String/format Locale/ROOT "%.2f" (object-array [v]))))

(defn- series-xs [s] (mapv as-double (get s "x")))
(defn- series-ys [s] (mapv as-double (get s "y")))

(defn- render-png-base64
  "Render the figure `spec` (string-keyed map) to a PNG and return it base64.
   Throws on any drawing failure (the caller wraps it in an envelope)."
  ^String [spec]
  (let [W        (int (as-double (or (get spec "width") 640)))
        H        (int (as-double (or (get spec "height") 480)))
        title    (get spec "title")
        xlabel   (get spec "xlabel")
        ylabel   (get spec "ylabel")
        grid?    (boolean (get spec "grid"))
        legend?  (boolean (get spec "legend"))
        series   (vec (get spec "series"))
        has-bar? (some #(= "bar" (get % "kind")) series)
        ml 62, mr 26
        mt (if (and (string? title) (seq title)) 46 22)
        mb (if (and (string? xlabel) (seq xlabel)) 58 42)
        px0 ml, py0 mt
        pw (max 1 (- W ml mr))
        ph (max 1 (- H mt mb))
        all-x (mapcat series-xs series)
        all-y (mapcat series-ys series)
        xlim (get spec "xlim")
        ylim (get spec "ylim")
        [xmin xmax] (if (seq xlim)
                      [(as-double (first xlim)) (as-double (second xlim))]
                      (if (seq all-x) [(apply min all-x) (apply max all-x)] [0.0 1.0]))
        raw-ys (cond-> (vec all-y) has-bar? (conj 0.0))
        [ymin ymax] (if (seq ylim)
                      [(as-double (first ylim)) (as-double (second ylim))]
                      (if (seq raw-ys) [(apply min raw-ys) (apply max raw-ys)] [0.0 1.0]))
        ;; guard zero-span + add a little headroom so points aren't on the frame
        [xmin xmax] (if (== xmin xmax) [(- xmin 1.0) (+ xmax 1.0)] [xmin xmax])
        [ymin ymax] (if (== ymin ymax) [(- ymin 1.0) (+ ymax 1.0)] [ymin ymax])
        ypad (* 0.05 (- ymax ymin))
        ymin (- ymin ypad)
        ymax (+ ymax ypad)
        sx (fn ^double [^double x] (+ px0 (* pw (/ (- x xmin) (- xmax xmin)))))
        sy (fn ^double [^double y] (+ py0 (* ph (- 1.0 (/ (- y ymin) (- ymax ymin))))))
        img (BufferedImage. W H BufferedImage/TYPE_INT_RGB)
        ^Graphics2D g (.createGraphics img)]
    (try
      (.setRenderingHint g RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON)
      (.setRenderingHint g RenderingHints/KEY_TEXT_ANTIALIASING RenderingHints/VALUE_TEXT_ANTIALIAS_ON)
      ;; background
      (.setColor g Color/WHITE)
      (.fillRect g 0 0 W H)
      ;; gridlines + tick labels (5 divisions each axis)
      (let [ticks 5
            fm (.getFontMetrics g)]
        (.setFont g (Font. "SansSerif" Font/PLAIN 10))
        (dotimes [i (inc ticks)]
          (let [t (/ (double i) ticks)
                xv (+ xmin (* t (- xmax xmin)))
                yv (+ ymin (* t (- ymax ymin)))
                xp (int (sx xv))
                yp (int (sy yv))]
            (when grid?
              (.setColor g (Color. 230 230 230))
              (.drawLine g xp py0 xp (+ py0 ph))
              (.drawLine g px0 yp (+ px0 pw) yp))
            (.setColor g (Color. 90 90 90))
            (let [xl (fmt-num xv)]
              (.drawString g xl (int (- xp (/ (.stringWidth fm xl) 2))) (int (+ py0 ph 16))))
            (let [yl (fmt-num yv)]
              (.drawString g yl (int (- px0 6 (.stringWidth fm yl))) (int (+ yp 4)))))))
      ;; axes frame
      (.setColor g (Color. 60 60 60))
      (.setStroke g (BasicStroke. 1.0))
      (.drawRect g px0 py0 pw ph)
      ;; series
      (let [nbar (count (filter #(= "bar" (get % "kind")) series))
            bar-slots (max 1 (reduce max 1 (map #(count (series-xs %)) series)))]
        (doseq [[idx s] (map-indexed vector series)]
          (let [kind (get s "kind")
                xs (series-xs s)
                ys (series-ys s)
                col (->color (get s "color") idx)
                pts (map vector xs ys)]
            (.setColor g col)
            (case kind
              "scatter"
              (doseq [[x y] pts]
                (.fillOval g (int (- (sx x) 3)) (int (- (sy y) 3)) 6 6))
              "bar"
              (let [bw (max 2 (int (* (/ pw bar-slots) (/ 0.7 (max 1 nbar)))))
                    y0 (int (sy (max ymin (min ymax 0.0))))]
                (doseq [[x y] pts]
                  (let [yp (int (sy y))
                        top (min y0 yp)
                        hgt (Math/abs (- y0 yp))]
                    (.fillRect g (int (- (sx x) (/ bw 2))) top bw (max 1 hgt)))))
              ;; default: line
              (do
                (.setStroke g (BasicStroke. 2.0 BasicStroke/CAP_ROUND BasicStroke/JOIN_ROUND))
                (doseq [[[x1 y1] [x2 y2]] (partition 2 1 pts)]
                  (.drawLine g (int (sx x1)) (int (sy y1)) (int (sx x2)) (int (sy y2)))))))))
      ;; title / axis labels
      (let [fm (.getFontMetrics g)]
        (.setColor g (Color. 30 30 30))
        (when (and (string? title) (seq title))
          (.setFont g (Font. "SansSerif" Font/BOLD 14))
          (let [fmt2 (.getFontMetrics g)]
            (.drawString g ^String title (int (- (+ px0 (/ pw 2)) (/ (.stringWidth fmt2 title) 2))) 18)))
        (when (and (string? xlabel) (seq xlabel))
          (.setFont g (Font. "SansSerif" Font/PLAIN 12))
          (let [fmt2 (.getFontMetrics g)]
            (.drawString g ^String xlabel (int (- (+ px0 (/ pw 2)) (/ (.stringWidth fmt2 xlabel) 2))) (int (- H 12)))))
        (when (and (string? ylabel) (seq ylabel))
          (.setFont g (Font. "SansSerif" Font/PLAIN 12))
          (let [fmt2 (.getFontMetrics g)
                tx (.getTransform g)]
            (.translate g 16.0 (double (+ py0 (/ ph 2))))
            (.rotate g (- (/ Math/PI 2)))
            (.drawString g ^String ylabel (int (- (/ (.stringWidth fmt2 ylabel) 2))) 0)
            (.setTransform g tx)))
        ;; legend
        (let [labelled (filter #(let [l (get % "label")] (and (string? l) (seq l)))
                         (map-indexed (fn [i s] (assoc s "__idx" i)) series))]
          (when (and (seq labelled) (or legend? (seq labelled)))
            (.setFont g (Font. "SansSerif" Font/PLAIN 11))
            (let [fmt2 (.getFontMetrics g)
                  rows (vec labelled)
                  lw (+ 34 (reduce max 0 (map #(.stringWidth fmt2 (str (get % "label"))) rows)))
                  lh (+ 8 (* 16 (count rows)))
                  lx (- (+ px0 pw) lw 8)
                  ly (+ py0 8)]
              (.setColor g (Color. 255 255 255))
              (.fillRect g lx ly lw lh)
              (.setColor g (Color. 180 180 180))
              (.drawRect g lx ly lw lh)
              (doseq [[ri s] (map-indexed vector rows)]
                (let [yy (+ ly 8 (* ri 16))]
                  (.setColor g (->color (get s "color") (get s "__idx")))
                  (.fillRect g (+ lx 8) (+ yy 3) 16 6)
                  (.setColor g (Color. 40 40 40))
                  (.drawString g (str (get s "label")) (+ lx 30) (+ yy 11))))))))
      (let [baos (ByteArrayOutputStream.)]
        (ImageIO/write img "png" baos)
        (.encodeToString (Base64/getEncoder) (.toByteArray baos)))
      (finally (.dispose g)))))

(defn- mpl-envelope
  "Run thunk `f`, returning the 2-vector the pyplot shim expects: [true payload]
   on success, [false message] on any Throwable. Errors cross the boundary as
   DATA so the shim can raise a catchable Python exception instead of a raw host
   `PolyglotException` (GraalPy does not route host exceptions through Python's
   `except`)."
  [f]
  (try [true (f)]
    (catch Throwable t [false (str (or (.getMessage t) t))])))

(defn- mpl-bridge-bindings
  "Host callables the matplotlib shim delegates to. `__vis_mpl_render__` takes a
   figure spec (string-keyed map with a `series` list) and returns
   `[true base64-png]` / `[false message]`."
  []
  {"__vis_mpl_render__" (fn [spec] (mpl-envelope #(render-png-base64 spec)))})

(def ^:private matplotlib-shim-src
  "Pure-Python preamble publishing a minimal `matplotlib` package with a
   `pyplot` submodule. Artists accumulate a figure spec; `savefig` delegates to
   the host `__vis_mpl_render__` (Java2D) and writes the returned PNG bytes.
   Single-quoted string literals throughout so this Clojure string needs no
   escaping."
  "# vis sandbox matplotlib-compat shim (Java2D-backed pyplot subset).

def __vis_install_matplotlib__():
    import sys
    import types
    import base64

    _COLORS = set('bgrcmykw')
    _MARKERS = set('o.,x+*sdv^<>ph')

    _state = {}

    def _reset():
        _state.clear()
        _state.update({'series': [], 'title': None, 'xlabel': None,
                       'ylabel': None, 'grid': False, 'legend': False,
                       'xlim': None, 'ylim': None, 'width': 640, 'height': 480})

    _reset()

    def _nums(v):
        out = []
        try:
            for e in v:
                try:
                    out.append(float(e))
                except Exception:
                    out.append(0.0)
        except TypeError:
            out.append(float(v))
        return out

    def _add_series(kind, x, y, label, color):
        _state['series'].append({
            'kind': kind, 'x': _nums(x), 'y': _nums(y),
            'label': label, 'color': color,
        })

    def _parse_fmt(fmt):
        color = None
        marker = None
        line = None
        s = fmt or ''
        for ls in ('--', '-.', ':', '-'):
            if ls in s:
                line = ls
                s = s.replace(ls, '', 1)
                break
        for ch in s:
            if ch in _COLORS and color is None:
                color = ch
            elif ch in _MARKERS and marker is None:
                marker = ch
        return color, marker, line

    def figure(num=None, figsize=None, dpi=None, **kwargs):
        d = float(dpi or 100)
        if figsize:
            _state['width'] = int(float(figsize[0]) * d)
            _state['height'] = int(float(figsize[1]) * d)
        return _state

    def plot(*args, **kwargs):
        a = list(args)
        fmt = ''
        if a and isinstance(a[-1], str):
            fmt = a.pop()
        if len(a) == 1:
            y = list(a[0])
            x = list(range(len(y)))
        elif len(a) >= 2:
            x = list(a[0])
            y = list(a[1])
        else:
            return None
        color, marker, line = _parse_fmt(fmt)
        kind = 'scatter' if (marker and not line) else 'line'
        _add_series(kind, x, y, kwargs.get('label'), kwargs.get('color', color))
        return None

    def scatter(x, y, s=None, c=None, label=None, color=None, **kwargs):
        _add_series('scatter', x, y, label, color or c)
        return None

    def bar(x, height, width=0.8, label=None, color=None, **kwargs):
        _add_series('bar', x, height, label, color)
        return None

    def hist(x, bins=10, label=None, color=None, **kwargs):
        xs = _nums(x)
        if not xs:
            return [], []
        bins = int(bins) if bins else 10
        lo = min(xs)
        hi = max(xs)
        if hi == lo:
            hi = lo + 1.0
        w = (hi - lo) / bins
        counts = [0] * bins
        for v in xs:
            idx = int((v - lo) / w)
            if idx >= bins:
                idx = bins - 1
            if idx < 0:
                idx = 0
            counts[idx] += 1
        centers = [lo + (i + 0.5) * w for i in range(bins)]
        _add_series('bar', centers, counts, label, color)
        return counts, centers

    def title(s, **kwargs):
        _state['title'] = str(s)

    def xlabel(s, **kwargs):
        _state['xlabel'] = str(s)

    def ylabel(s, **kwargs):
        _state['ylabel'] = str(s)

    def grid(b=True, **kwargs):
        _state['grid'] = bool(b)

    def legend(*args, **kwargs):
        _state['legend'] = True

    def xlim(*args, **kwargs):
        if len(args) == 2:
            _state['xlim'] = [float(args[0]), float(args[1])]
        elif len(args) == 1 and args[0] is not None:
            _state['xlim'] = [float(args[0][0]), float(args[0][1])]
        return _state['xlim']

    def ylim(*args, **kwargs):
        if len(args) == 2:
            _state['ylim'] = [float(args[0]), float(args[1])]
        elif len(args) == 1 and args[0] is not None:
            _state['ylim'] = [float(args[0][0]), float(args[0][1])]
        return _state['ylim']

    def clf(*args, **kwargs):
        _reset()

    def cla(*args, **kwargs):
        _reset()

    def close(*args, **kwargs):
        _reset()

    def show(*args, **kwargs):
        # No display in the sandbox; savefig is the way to get an image out.
        return None

    def _spec():
        return {
            'width': _state['width'], 'height': _state['height'],
            'title': _state['title'], 'xlabel': _state['xlabel'],
            'ylabel': _state['ylabel'], 'grid': _state['grid'],
            'legend': _state['legend'], 'xlim': _state['xlim'],
            'ylim': _state['ylim'],
            'series': list(_state['series']),
        }

    def savefig(fname, format=None, dpi=None, **kwargs):
        render = globals().get('__vis_mpl_render__')
        if render is None:
            raise RuntimeError('vis: matplotlib Java2D backend is not bound in this sandbox.')
        env = render(_spec())
        if not env[0]:
            raise RuntimeError('matplotlib render failed: ' + str(env[1]))
        data = base64.b64decode(env[1])
        if hasattr(fname, 'write'):
            fname.write(data)
        else:
            with open(fname, 'wb') as _f:
                _f.write(data)
        return fname

    pyplot = types.ModuleType('matplotlib.pyplot')
    pyplot.__doc__ = 'vis Java2D-backed matplotlib.pyplot subset (line/scatter/bar/hist).'
    for _fn in (figure, plot, scatter, bar, hist, title, xlabel, ylabel,
                grid, legend, xlim, ylim, clf, cla, close, show, savefig):
        setattr(pyplot, _fn.__name__, _fn)

    mpl = types.ModuleType('matplotlib')
    mpl.__doc__ = 'vis matplotlib-compat shim (no CPython matplotlib wheel).'
    mpl.__version__ = '3.0-vis-java2d'
    mpl.pyplot = pyplot

    sys.modules['matplotlib'] = mpl
    sys.modules['matplotlib.pyplot'] = pyplot

    # Autoload: staple `matplotlib` onto builtins so `matplotlib.pyplot` works
    # even without an explicit import (the model still usually does
    # `import matplotlib.pyplot as plt`).
    try:
        import builtins as _b
        _b.matplotlib = mpl
    except Exception:
        pass

__vis_install_matplotlib__()
del __vis_install_matplotlib__
")

(def vis-extension
  (vis/extension
    {:ext/name         "foundation-shim-matplotlib"
     :ext/description  "Sandbox shim: a minimal matplotlib.pyplot (plot/scatter/bar/hist + title/labels/grid/legend) rendered to PNG by a pure-JVM Java2D backend. savefig writes the image; no pip, no native wheel."
     :ext/version      "0.1.0"
     :ext/author       "Blockether"
     :ext/owner        "vis"
     :ext/license      "Apache-2.0"
     :ext/kind         "foundation"
     :ext/sandbox-shims
     [{:shim/name        "matplotlib"
       :shim/description "matplotlib.pyplot subset backed by a Java2D PNG renderer."
       :shim/bindings    mpl-bridge-bindings
       :shim/preamble    matplotlib-shim-src}]}))

(vis/register-extension! vis-extension)
