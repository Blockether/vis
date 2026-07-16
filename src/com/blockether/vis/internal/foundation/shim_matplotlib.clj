(ns com.blockether.vis.internal.foundation.shim-matplotlib
  "Built-in sandbox SHIM: a minimal `matplotlib.pyplot`-compatible module for the
   model's Python sandbox, backed by a pure-JVM Java2D renderer. The agent
   sandbox ships no CPython matplotlib wheel (it needs numpy's native core +
   freetype, all blocked by the deny-by-default Context); this extension instead
   contributes a `:ext/sandbox-shims` entry whose Python preamble accumulates the
   familiar pyplot artists (`plot`/`scatter`/`bar`/`hist`/`fill_between`/`step`/
   `pie`/`axhline`/`axvline`/`title`/`xlabel`/… plus the OO `subplots`/`Axes`
   API) and whose `savefig` DELEGATES the whole figure spec across the boundary
   to the host callable `__vis_mpl_render__`, which draws it with `java.awt`/
   `ImageIO` and returns a base64 PNG. The Python side base64-decodes and writes
   it (to a path, confined to the sandbox roots, or to any file-like buffer).

   It is a SUBSET, not real matplotlib: line/scatter/bar/hist/fill/step/pie with
   title, axis labels, grid, legend, dashed line styles, markers, log scales and
   text annotations — enough for the model to visualize data. Rendering runs
   entirely on the JVM (no pip, no native wheels), and any render failure
   surfaces to Python as a catchable exception (never crashes the sandbox).

   Together with `shim-yaml` this demonstrates the sandbox-shim mechanism: an
   extension turns a host / JVM capability into a real importable Python module
   while `env-python` stays completely generic about which shims exist."
  (:require [com.blockether.vis.core :as vis]
            [com.blockether.vis.internal.foundation.mpl-capture :as mpl-capture])
  (:import [java.awt BasicStroke Color Font Graphics2D RenderingHints]
           [java.awt.image BufferedImage]
           [java.io ByteArrayOutputStream]
           [java.util Base64 Locale]
           [javax.imageio ImageIO]))

;; ----------------------------------------------------------------------------
;; macOS/AWT guard — MUST run before the toolkit initializes.
;;
;; If AWT's `createGraphics` / `GraphicsEnvironment.getLocalGraphicsEnvironment`
;; runs WITHOUT headless mode, macOS boots the Cocoa AWT toolkit: a Java Dock
;; icon appears, the app can steal focus, and image ops may pop a Preview
;; window — none of which the user asked for. This shim is the process's only
;; AWT consumer and it loads at boot (built-in extension), so we force headless
;; HERE, at ns-load, which is guaranteed to precede the first render. Setting it
;; after the toolkit inits is a no-op, hence the top-level side effect rather
;; than a per-render bind. `apple.awt.UIElement` is a macOS belt: even if some
;; other path forces non-headless AWT, the process stays a background UI element
;; (no Dock icon, no menu bar).
(System/setProperty "java.awt.headless" "true")
(System/setProperty "apple.awt.UIElement" "true")

;; ----------------------------------------------------------------------------
;; Host renderer — Java2D. Input is the pyplot figure spec (string-keyed map
;; marshalled from the sandbox); output is a base64 PNG string. Kept dependency
;; free (only the JDK's AWT + ImageIO, already reachable in the native image).
;; ----------------------------------------------------------------------------

(def ^:private palette
  "matplotlib tab10-ish default series colors, cycled by series index."
  [[31 119 180] [255 127 14] [44 160 44] [214 39 40] [148 103 189] [140 86 75] [227 119 194]
   [127 127 127] [188 189 34] [23 190 207]])

(def ^:private named-colors
  {"b" [0 0 255]
   "g" [0 128 0]
   "r" [255 0 0]
   "c" [0 191 191]
   "m" [191 0 191]
   "y" [191 191 0]
   "k" [0 0 0]
   "w" [255 255 255]
   "blue" [0 0 255]
   "green" [0 128 0]
   "red" [255 0 0]
   "cyan" [0 191 191]
   "magenta" [191 0 191]
   "yellow" [191 191 0]
   "black" [0 0 0]
   "white" [255 255 255]
   "orange" [255 127 14]
   "purple" [148 103 189]
   "gray" [127 127 127]
   "grey" [127 127 127]
   "brown" [140 86 75]
   "pink" [227 119 194]})

(defn- hex->rgb
  [^String s]
  (let [h (.replace s "#" "")]
    (when (= 6 (count h))
      (try [(Integer/parseInt (subs h 0 2) 16) (Integer/parseInt (subs h 2 4) 16)
            (Integer/parseInt (subs h 4 6) 16)]
           (catch Exception _ nil)))))

(defn- ->color
  ^Color [c idx]
  (let [cs
        (when (and (string? c) (seq c)) (.toLowerCase ^String (str c)))

        rgb
        (or (when cs (get named-colors cs))
            (when (and cs (.startsWith ^String cs "#")) (hex->rgb cs))
            (nth palette (mod (int idx) (count palette))))]

    (Color. (int (nth rgb 0)) (int (nth rgb 1)) (int (nth rgb 2)))))

(defn- as-double
  ^double [x]
  (cond (number? x) (double x)
        :else (try (Double/parseDouble (str x)) (catch Exception _ 0.0))))

(defn- fmt-num
  ^String [^double v]
  (if (== v (Math/rint v)) (str (long v)) (String/format Locale/ROOT "%.2f" (object-array [v]))))

(defn- series-xs [s] (mapv as-double (get s "x")))
(defn- series-ys [s] (mapv as-double (get s "y")))

(defn- dash-stroke
  ^BasicStroke [linestyle width]
  (let [w (float width)]
    (case (str linestyle)
      "--"
      (BasicStroke. w BasicStroke/CAP_BUTT BasicStroke/JOIN_ROUND 10.0 (float-array [9.0 6.0]) 0.0)

      ":"
      (BasicStroke. w BasicStroke/CAP_ROUND BasicStroke/JOIN_ROUND 10.0 (float-array [2.0 5.0]) 0.0)

      "-."
      (BasicStroke. w
                    BasicStroke/CAP_BUTT
                    BasicStroke/JOIN_ROUND
                    10.0
                    (float-array [10.0 5.0 2.0 5.0])
                    0.0)

      (BasicStroke. w BasicStroke/CAP_ROUND BasicStroke/JOIN_ROUND))))

(defn- new-canvas
  [^long W ^long H]
  (let [img
        (BufferedImage. W H BufferedImage/TYPE_INT_RGB)

        g
        (.createGraphics img)]

    (.setRenderingHint g RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON)
    (.setRenderingHint g
                       RenderingHints/KEY_TEXT_ANTIALIASING
                       RenderingHints/VALUE_TEXT_ANTIALIAS_ON)
    (.setColor g Color/WHITE)
    (.fillRect g 0 0 W H)
    [img g]))

(defn- png-base64
  ^String [^BufferedImage img]
  (let [baos (ByteArrayOutputStream.)]
    (ImageIO/write img "png" baos)
    (.encodeToString (Base64/getEncoder) (.toByteArray baos))))

(defn- lerp-col
  [[r1 g1 b1] [r2 g2 b2] ^double t]
  [(+ (double r1) (* (- (double r2) (double r1)) t))
   (+ (double g1) (* (- (double g2) (double g1)) t))
   (+ (double b1) (* (- (double b2) (double b1)) t))])

(defn- viridis
  ^Color [^double t]
  (let [t
        (double (max 0.0 (min 1.0 t)))

        stops
        [[68 1 84] [59 82 139] [33 145 140] [94 201 98] [253 231 37]]

        n
        (dec (count stops))

        f
        (* t n)

        i
        (min (int f) (dec n))

        [a b c]
        (lerp-col (nth stops i) (nth stops (inc i)) (- f i))]

    (Color. (int a) (int b) (int c))))

(defn- draw-title
  [^Graphics2D g title ^long px0 ^long pw]
  (when (and (string? title) (seq title))
    (.setColor g (Color. 30 30 30))
    (.setFont g (Font. "SansSerif" Font/BOLD 14))
    (let [fm (.getFontMetrics g)]
      (.drawString g
                   ^String title
                   (int (- (+ px0 (quot pw 2)) (quot (.stringWidth fm title) 2)))
                   18))))

(defn- render-pie
  "Full-canvas pie chart (ignores axes). `s` carries sizes in `x` and optional
   `labels`."
  ^String [^long W ^long H spec s]
  (let [[img ^Graphics2D g] (new-canvas W H)]
    (try
      (let [vals (mapv #(Math/abs (as-double %)) (get s "x"))
            labels (get s "labels")
            total (double (reduce + 0.0 vals))
            cx (/ W 2.0)
            cy (+ 12.0 (/ H 2.0))
            r (double (- (quot (long (min W H)) 2) 66))
            start (volatile! 90.0)]

        (.setFont g (Font. "SansSerif" Font/PLAIN 11))
        (dotimes [i (count vals)]
          (let [frac (if (< 0.0 total) (/ (double (nth vals i)) total) 0.0)
                ang (* 360.0 frac)
                col (->color nil i)
                mid (Math/toRadians (- (double @start) (/ ang 2.0)))]

            (.setColor g col)
            (.fillArc g
                      (int (- cx r))
                      (int (- cy r))
                      (int (* 2 r))
                      (int (* 2 r))
                      (int (Math/round (double @start)))
                      (int (Math/round (- ang))))
            (let [lx (+ cx (* (+ r 16) (Math/cos mid)))
                  ly (- cy (* (+ r 16) (Math/sin mid)))
                  lbl (if (and labels (< i (count labels)))
                        (str (nth labels i))
                        (str (Math/round (* 100.0 frac)) "%"))
                  fm (.getFontMetrics g)]

              (.setColor g (Color. 40 40 40))
              (.drawString g
                           lbl
                           (int (if (neg? (Math/cos mid)) (- lx (.stringWidth fm lbl)) lx))
                           (int ly)))
            (vswap! start
                    (fn [s]
                      (- (double s) ang)))))
        (draw-title g (get spec "title") 0 W)
        (png-base64 img))
      (finally (.dispose g)))))

(defn- render-xy
  "Line/scatter/bar/hist/fill/step/hline/vline figure with axes, ticks, grid,
   labels, log scales, annotations and legend."
  ^String [^long W ^long H spec series]
  (let [title
        (get spec "title")

        xlabel
        (get spec "xlabel")

        ylabel
        (get spec "ylabel")

        grid?
        (boolean (get spec "grid"))

        legend?
        (boolean (get spec "legend"))

        axis-off?
        (boolean (get spec "axis_off"))

        annotations
        (get spec "annotations")

        xlog?
        (= "log" (str (get spec "xscale")))

        ylog?
        (= "log" (str (get spec "yscale")))

        xfwd
        (fn ^double [^double v]
          (if xlog? (Math/log10 (Math/max 1.0e-12 v)) v))

        yfwd
        (fn ^double [^double v]
          (if ylog? (Math/log10 (Math/max 1.0e-12 v)) v))

        xinv
        (fn ^double [^double v]
          (if xlog? (Math/pow 10.0 v) v))

        yinv
        (fn ^double [^double v]
          (if ylog? (Math/pow 10.0 v) v))

        has-bar?
        (some #(= "bar" (str (get % "kind"))) series)

        cat-labels
        (some (fn [s]
                (when (= "bar" (str (get s "kind"))) (get s "labels")))
              series)

        ml
        62

        mr
        26

        mt
        (if (and (string? title) (seq title)) 46 22)

        mb
        (if (and (string? xlabel) (seq xlabel)) 58 42)

        px0
        ml

        py0
        mt

        pw
        (long (max 1 (- W ml mr)))

        ph
        (long (max 1 (- H mt mb)))

        all-x
        (map xfwd (mapcat series-xs series))

        all-y
        (map yfwd
             (concat (mapcat series-ys series)
                     (mapcat (fn [s]
                               (mapv as-double (get s "y2")))
                             series)))

        xlim
        (get spec "xlim")

        ylim
        (get spec "ylim")

        [xmin xmax]
        (if (seq xlim)
          [(xfwd (as-double (first xlim))) (xfwd (as-double (second xlim)))]
          (if (seq all-x) [(apply min all-x) (apply max all-x)] [0.0 1.0]))

        raw-ys
        (cond-> (vec all-y)
          has-bar?
          (conj 0.0))

        [ymin ymax]
        (if (seq ylim)
          [(yfwd (as-double (first ylim))) (yfwd (as-double (second ylim)))]
          (if (seq raw-ys) [(apply min raw-ys) (apply max raw-ys)] [0.0 1.0]))

        [xmin xmax]
        (if (== (double xmin) (double xmax))
          [(- (double xmin) 1.0) (+ (double xmax) 1.0)]
          [xmin xmax])

        [ymin ymax]
        (if (== (double ymin) (double ymax))
          [(- (double ymin) 1.0) (+ (double ymax) 1.0)]
          [ymin ymax])

        ypad
        (* 0.05 (- (double ymax) (double ymin)))

        ymin
        (- (double ymin) ypad)

        ymax
        (+ (double ymax) ypad)

        sxf
        (fn ^double [^double xf]
          (+ px0 (* pw (/ (- xf (double xmin)) (- (double xmax) (double xmin))))))

        syf
        (fn ^double [^double yf]
          (+ py0 (* ph (- 1.0 (/ (- yf ymin) (- ymax ymin))))))

        sx
        (fn ^double [^double x]
          (sxf (xfwd x)))

        sy
        (fn ^double [^double y]
          (syf (yfwd y)))

        [img ^Graphics2D g]
        (new-canvas W H)]

    (try
      ;; gridlines + tick labels + frame (all skipped when axis('off'))
      (when-not axis-off?
        (let [ticks 5]
          (.setFont g (Font. "SansSerif" Font/PLAIN 10))
          (let [fm (.getFontMetrics g)]
            (dotimes [i (inc ticks)]
              (let [t (/ (double i) ticks)
                    xv (+ (double xmin) (* t (- (double xmax) (double xmin))))
                    yv (+ ymin (* t (- ymax ymin)))
                    xp (int (sxf xv))
                    yp (int (syf yv))]

                (when grid?
                  (.setColor g (Color. 230 230 230))
                  (.drawLine g xp py0 xp (+ py0 ph))
                  (.drawLine g px0 yp (+ px0 pw) yp))
                (.setColor g (Color. 90 90 90))
                (when-not cat-labels
                  (let [^String xl (fmt-num (xinv xv))]
                    (.drawString g
                                 xl
                                 (int (- xp (quot (.stringWidth fm xl) 2)))
                                 (int (+ py0 ph 16)))))
                (let [^String yl (fmt-num (yinv yv))]
                  (.drawString g yl (int (- px0 6 (.stringWidth fm yl))) (int (+ yp 4))))))
            (when cat-labels
              (.setColor g (Color. 90 90 90))
              (dotimes [i (count cat-labels)]
                (let [^String xl (str (nth cat-labels i))
                      xp (int (sx (double i)))]

                  (.drawString g
                               xl
                               (int (- xp (quot (.stringWidth fm xl) 2)))
                               (int (+ py0 ph 16))))))))
        ;; axes frame
        (.setColor g (Color. 60 60 60))
        (.setStroke g (BasicStroke. 1.0))
        (.drawRect g px0 py0 pw ph))
      ;; series
      (let [nbar
            (count (filter #(= "bar" (str (get % "kind"))) series))

            bar-slots
            (long (max 1 (long (reduce max 1 (map #(count (series-xs %)) series)))))]

        (doseq [[idx s] (map-indexed vector series)]
          (let [kind (str (get s "kind"))
                xs (series-xs s)
                ys (series-ys s)
                col (->color (get s "color") idx)
                pts (map vector xs ys)]

            (.setColor g col)
            (case kind
              "scatter"
              (doseq [[x y] pts]
                (.fillOval g (int (- (long (sx x)) 3)) (int (- (long (sy y)) 3)) 6 6))

              "bar"
              (let [bw (long (max 2 (int (* (quot pw bar-slots) (/ 0.7 (long (max 1 nbar)))))))
                    y0 (int (syf (max ymin (min ymax 0.0))))]

                (doseq [[x y] pts]
                  (let [yp (int (sy y))
                        top (min y0 yp)
                        hgt (Math/abs (- y0 yp))]

                    (.fillRect g (int (- (long (sx x)) (quot bw 2))) top bw (max 1 hgt)))))

              "hline"
              (when (seq ys)
                (.setStroke g (dash-stroke (get s "linestyle") 1.5))
                (let [yp (int (sy (first ys)))]
                  (.drawLine g px0 yp (+ px0 pw) yp)))

              "vline"
              (when (seq xs)
                (.setStroke g (dash-stroke (get s "linestyle") 1.5))
                (let [xp (int (sx (first xs)))]
                  (.drawLine g xp py0 xp (+ py0 ph))))

              "fill"
              (let [y2 (mapv as-double (get s "y2"))
                    n (count xs)]

                (when (and (pos? n) (= n (count y2)))
                  (let [xsi (int-array (concat (map #(int (sx %)) xs)
                                               (map #(int (sx %)) (reverse xs))))
                        ysi (int-array (concat (map #(int (sy %)) ys)
                                               (map #(int (sy %)) (reverse y2))))
                        fc (Color. (.getRed col) (.getGreen col) (.getBlue col) 90)]

                    (.setColor g fc)
                    (.fillPolygon g xsi ysi (* 2 n)))))

              "step"
              (do (.setStroke g (dash-stroke (get s "linestyle") 2.0))
                  (doseq [[[x1 y1] [x2 y2]] (partition 2 1 pts)]
                    (.drawLine g (int (sx x1)) (int (sy y1)) (int (sx x2)) (int (sy y1)))
                    (.drawLine g (int (sx x2)) (int (sy y1)) (int (sx x2)) (int (sy y2)))))

              "image"
              (let [rows (get s "rows")
                    nr (int (as-double (get s "nrows")))
                    nc (int (as-double (get s "ncols")))
                    vmin (as-double (get s "vmin"))
                    vmax (as-double (get s "vmax"))
                    span (let [d (- vmax vmin)]
                           (if (zero? d) 1.0 d))]

                (dotimes [ri nr]
                  (let [row (nth rows ri nil)]
                    (dotimes [ci nc]
                      (let [v (as-double (nth row ci 0))
                            col (viridis (/ (- v vmin) span))
                            x0 (int (sx ci))
                            x1 (int (sx (inc ci)))
                            yt (int (sy (- nr ri)))
                            yb (int (sy (- nr (inc ri))))]

                        (.setColor g col)
                        (.fillRect g
                                   (min x0 x1)
                                   (min yt yb)
                                   (max 1 (Math/abs (- x1 x0)))
                                   (max 1 (Math/abs (- yb yt)))))))))

              "box"
              (let [stats (get s "stats")
                    pos (mapv as-double (get s "positions"))
                    bw 24]

                (doseq [[bi st] (map-indexed vector stats)]
                  (let [xc (int (sx (nth pos bi (inc (long bi)))))
                        q1 (int (sy (as-double (get st "q1"))))
                        q2 (int (sy (as-double (get st "q2"))))
                        q3 (int (sy (as-double (get st "q3"))))
                        lo (int (sy (as-double (get st "lo"))))
                        hi (int (sy (as-double (get st "hi"))))
                        hw (int (/ bw 2))
                        col (->color nil bi)]

                    (.setStroke g (BasicStroke. 1.5))
                    (.setColor g (Color. 60 60 60))
                    (.drawLine g xc lo xc q1)
                    (.drawLine g xc q3 xc hi)
                    (.drawLine g (- xc hw) lo (+ xc hw) lo)
                    (.drawLine g (- xc hw) hi (+ xc hw) hi)
                    (.setColor g col)
                    (.fillRect g (- xc hw) (min q1 q3) bw (max 1 (Math/abs (- q3 q1))))
                    (.setColor g (Color. 30 30 30))
                    (.drawRect g (- xc hw) (min q1 q3) bw (max 1 (Math/abs (- q3 q1))))
                    (.drawLine g (- xc hw) q2 (+ xc hw) q2))))

              ;; default: line (+ optional markers)
              (do (.setStroke g (dash-stroke (get s "linestyle") 2.0))
                  (doseq [[[x1 y1] [x2 y2]] (partition 2 1 pts)]
                    (.drawLine g (int (sx x1)) (int (sy y1)) (int (sx x2)) (int (sy y2))))
                  (when (seq (str (get s "marker")))
                    (doseq [[x y] pts]
                      (.fillOval g (int (- (long (sx x)) 3)) (int (- (long (sy y)) 3)) 6 6))))))))
      ;; title / axis labels
      (draw-title g title px0 pw)
      (.setColor g (Color. 30 30 30))
      (when (and (string? xlabel) (seq xlabel))
        (.setFont g (Font. "SansSerif" Font/PLAIN 12))
        (let [fm (.getFontMetrics g)]
          (.drawString g
                       ^String xlabel
                       (int (- (+ px0 (quot pw 2)) (quot (.stringWidth fm xlabel) 2)))
                       (int (- H 12)))))
      (when (and (string? ylabel) (seq ylabel))
        (.setFont g (Font. "SansSerif" Font/PLAIN 12))
        (let [fm
              (.getFontMetrics g)

              tx
              (.getTransform g)]

          (.translate g 16.0 (double (+ py0 (quot ph 2))))
          (.rotate g (- (/ Math/PI 2)))
          (.drawString g ^String ylabel (int (- (quot (.stringWidth fm ylabel) 2))) 0)
          (.setTransform g tx)))
      ;; text annotations (data coords)
      (when (seq annotations)
        (.setFont g (Font. "SansSerif" Font/PLAIN 11))
        (.setColor g (Color. 20 20 20))
        (doseq [a annotations]
          (.drawString g
                       (str (get a "text"))
                       (int (sx (as-double (get a "x"))))
                       (int (sy (as-double (get a "y")))))))
      ;; legend
      (let [labelled (filter #(let [l (get % "label")]

                                (and (string? l) (seq l)))
                             (map-indexed (fn [i s]
                                            (assoc s "__idx" i))
                                          series))]
        (when (and (seq labelled) (or legend? (seq labelled)))
          (.setFont g (Font. "SansSerif" Font/PLAIN 11))
          (let [fm (.getFontMetrics g)
                rows (vec labelled)
                lw (+ 34 (long (reduce max 0 (map #(.stringWidth fm (str (get % "label"))) rows))))
                lh (+ 8 (* 16 (count rows)))
                lx (- (+ px0 pw) lw 8)
                ly (+ py0 8)]

            (.setColor g (Color. 255 255 255))
            (.fillRect g lx ly lw lh)
            (.setColor g (Color. 180 180 180))
            (.drawRect g lx ly lw lh)
            (doseq [[ri s] (map-indexed vector rows)]
              (let [yy (+ ly 8 (* (long ri) 16))]
                (.setColor g (->color (get s "color") (get s "__idx")))
                (.fillRect g (+ lx 8) (+ yy 3) 16 6)
                (.setColor g (Color. 40 40 40))
                (.drawString g (str (get s "label")) (int (+ lx 30)) (int (+ yy 11))))))))
      (png-base64 img)
      (finally (.dispose g)))))

(defn- render-png-base64
  "Render the figure `spec` (string-keyed map) to a PNG and return it base64.
   Dispatches to a pie chart when a pie series is present, else the XY renderer.
   Throws on any drawing failure (the caller wraps it in an envelope)."
  ^String [spec]
  (let [W
        (int (as-double (or (get spec "width") 640)))

        H
        (int (as-double (or (get spec "height") 480)))

        series
        (vec (get spec "series"))

        pie
        (first (filter #(= "pie" (str (get % "kind"))) series))]

    (if pie (render-pie W H spec pie) (render-xy W H spec series))))

(defn- mpl-envelope
  "Run thunk `f`, returning the 2-vector the pyplot shim expects: [true payload]
   on success, [false message] on any Throwable. Errors cross the boundary as
   DATA so the shim can raise a catchable Python exception instead of a raw host
   `PolyglotException` (GraalPy does not route host exceptions through Python's
   `except`)."
  [f]
  (try [true (f)] (catch Throwable t [false (str (or (.getMessage t) t))])))

(defn- mpl-bridge-bindings
  "Host callables the matplotlib shim delegates to. `__vis_mpl_render__` takes a
   figure spec (string-keyed map with a `series` list) and returns
   `[true base64-png]` / `[false message]` (savefig decodes + writes it).
   `__vis_mpl_render_file__` renders the same spec but WRITES the PNG to a host
   temp file with HOST IO — so it works even when the sandbox's own Python
   filesystem is denied (the inline-image path for `plt.show()`) — returning
   `[true [abs-path width height byte-count]]`."
  []
  {"__vis_mpl_render__" (fn [spec]
                          (mpl-envelope #(render-png-base64 spec)))
   "__vis_mpl_render_file__"
   (fn [spec]
     (mpl-envelope
       #(let [b64
              (render-png-base64 spec)

              bytes
              (.decode (Base64/getDecoder) ^String b64)

              dir
              (doto (java.io.File. (System/getProperty "java.io.tmpdir") "vis-mpl") (.mkdirs))

              f
              (java.io.File/createTempFile "fig-" ".png" dir)

              w
              (int (as-double (or (get spec "width") 640)))

              h
              (int (as-double (or (get spec "height") 480)))]

          (with-open [o (java.io.FileOutputStream. f)]
            (.write o ^bytes bytes))
          ;; SINK the bytes at the source — the engine OWNS this figure (a DB
          ;; attachment) with no stdout-fence parsing. The temp file stays only
          ;; for the inline display fence. No-op when no per-block sink is bound.
          (mpl-capture/record-attachment! {:kind "image"
                                           :media-type "image/png"
                                           :base64 b64
                                           :size (alength bytes)
                                           :filename (.getName f)
                                           :dims (str w "x" h)}) [(.getAbsolutePath f) w h
                                                                  (alength bytes)])))})

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
                       'xlim': None, 'ylim': None, 'xscale': 'linear',
                       'yscale': 'linear', 'annotations': [],
                       'width': 640, 'height': 480, 'axis_off': False})

    _reset()

    def _nums(v):
        out = []
        if v is None:
            return out
        try:
            for e in v:
                try:
                    out.append(float(e))
                except Exception:
                    out.append(0.0)
        except TypeError:
            out.append(float(v))
        return out

    def _add_series(kind, x, y, label, color, marker=None,
                    linestyle=None, y2=None, labels=None):
        s = {
            'kind': kind, 'x': _nums(x), 'y': _nums(y),
            'label': label, 'color': color, 'marker': marker,
            'linestyle': linestyle,
            'y2': (_nums(y2) if y2 is not None else None),
            'labels': labels,
        }
        _state['series'].append(s)
        return s

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
        return _Figure()

    def plot(*args, **kwargs):
        a = list(args)
        handles = []
        i = 0
        n = len(a)
        while i < n:
            if i + 1 < n and not isinstance(a[i + 1], str):
                x = list(a[i])
                y = list(a[i + 1])
                i += 2
            else:
                y = list(a[i])
                x = list(range(len(y)))
                i += 1
            fmt = ''
            if i < n and isinstance(a[i], str):
                fmt = a[i]
                i += 1
            color, marker, line = _parse_fmt(fmt)
            kind = 'scatter' if (marker and not line) else 'line'
            s = _add_series(kind, x, y, kwargs.get('label'),
                            kwargs.get('color', color),
                            marker=kwargs.get('marker', marker),
                            linestyle=kwargs.get('linestyle',
                                                  kwargs.get('ls', line)))
            handles.append(_Line(s))
        return handles

    def scatter(x, y, s=None, c=None, label=None, color=None, **kwargs):
        _add_series('scatter', x, y, label, color or c)
        return None

    def _is_number(v):
        if isinstance(v, bool):
            return False
        if isinstance(v, (int, float)):
            return True
        try:
            float(v)
            return True
        except (TypeError, ValueError):
            return False

    def _categorical(vals):
        # matplotlib maps string/categorical x onto an integer axis, deduping
        # to distinct categories in first-seen order; each value takes its
        # category's index. Numeric input keeps the float path (None, None).
        seq = list(vals)
        if not seq or all(_is_number(v) for v in seq):
            return None, None
        order = []
        index = {}
        for v in seq:
            key = str(v)
            if key not in index:
                index[key] = len(order)
                order.append(key)
        return [index[str(v)] for v in seq], order

    def bar(x, height, width=0.8, label=None, color=None, **kwargs):
        pos, labels = _categorical(x)
        if pos is not None:
            _add_series('bar', pos, height, label, color, labels=labels)
        else:
            _add_series('bar', x, height, label, color)
        return None

    def barh(y, width, height=0.8, label=None, color=None, **kwargs):
        pos, labels = _categorical(y)
        if pos is not None:
            _add_series('bar', pos, width, label, color, labels=labels)
        else:
            _add_series('bar', y, width, label, color)
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

    def fill_between(x, y1, y2=0, label=None, color=None, **kwargs):
        xs = list(x)
        n = len(xs)
        y1l = _nums(y1)
        if isinstance(y2, (int, float)):
            y2l = [float(y2)] * n
        else:
            y2l = _nums(y2)
        _add_series('fill', xs, y1l, label, color, y2=y2l)
        return None

    def step(x, y, *args, label=None, color=None, **kwargs):
        _add_series('step', x, y, label, color)
        return None

    def axhline(y=0, color=None, linestyle=None, ls=None, label=None, **kwargs):
        _add_series('hline', [], [y], label, color, linestyle=(linestyle or ls))
        return None

    def axvline(x=0, color=None, linestyle=None, ls=None, label=None, **kwargs):
        _add_series('vline', [x], [], label, color, linestyle=(linestyle or ls))
        return None

    def pie(sizes, labels=None, colors=None, autopct=None, **kwargs):
        _add_series('pie', list(sizes), [], None, None,
                    labels=(list(labels) if labels else None))
        return None

    def errorbar(x, y, yerr=None, xerr=None, fmt='', label=None, color=None, **kwargs):
        _add_series('line', x, y, label, color)
        return None

    def text(x, y, s, **kwargs):
        _state['annotations'].append({'x': float(x), 'y': float(y), 'text': str(s)})
        return None

    def annotate(s, xy=None, xytext=None, **kwargs):
        pt = xytext or xy or (0, 0)
        _state['annotations'].append({'x': float(pt[0]), 'y': float(pt[1]), 'text': str(s)})
        return None

    def title(s, **kwargs):
        _state['title'] = str(s)

    def suptitle(s, **kwargs):
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

    def xscale(v, **kwargs):
        _state['xscale'] = str(v)

    def yscale(v, **kwargs):
        _state['yscale'] = str(v)

    def semilogx(*args, **kwargs):
        r = plot(*args, **kwargs)
        _state['xscale'] = 'log'
        return r

    def semilogy(*args, **kwargs):
        r = plot(*args, **kwargs)
        _state['yscale'] = 'log'
        return r

    def loglog(*args, **kwargs):
        r = plot(*args, **kwargs)
        _state['xscale'] = 'log'
        _state['yscale'] = 'log'
        return r

    def xticks(*args, **kwargs):
        return [], []

    def yticks(*args, **kwargs):
        return [], []

    def tight_layout(*args, **kwargs):
        return None

    def subplots_adjust(*args, **kwargs):
        return None

    def clf(*args, **kwargs):
        _reset()

    def cla(*args, **kwargs):
        _reset()

    def close(*args, **kwargs):
        _reset()

    def _human_size(n):
        # Compact byte-size label for the vis-image fence (B / KB / MB).
        n = float(n)
        for unit in ('B', 'KB', 'MB'):
            if n < 1024.0 or unit == 'MB':
                return (str(int(n)) + ' B') if unit == 'B' else ('%.1f %s' % (n, unit))
            n = n / 1024.0

    _img_seq = [0]

    def _emit_image():
        # Render the current figure to a PNG on the HOST (Java2D) and print a
        # `vis-image` fence: 5 header lines (summary / path / mime / WxH / size) the
        # channel reads to paint the picture inline, plus the ASCII plot appended as
        # the fallback body for non-graphical terminals. The PNG is written HOST-side
        # (`__vis_mpl_render_file__`), so this works even when the sandbox's own
        # Python filesystem is denied. Returns True on success, False (no bridge /
        # render error) so show() can fall back to the plain ASCII print.
        render = globals().get('__vis_mpl_render_file__')
        if render is None:
            return False
        try:
            spec = _spec()
            env = render(spec)
            if not env[0]:
                return False
            path, w, h, nbytes = env[1]
            path = str(path)
            w = int(w)
            h = int(h)
            size = _human_size(int(nbytes))
            _img_seq[0] += 1
            title = spec.get('title') or 'matplotlib figure'
            summary = ('[Image #' + str(_img_seq[0]) + ': ' + str(title) + ' '
                       + str(w) + '×' + str(h) + ', ' + size + ']')
            fence = '`' * 4
            lines = [fence + 'vis-image', summary, path, 'image/png',
                     str(w) + 'x' + str(h), size]
            if spec.get('series'):
                lines.append(_render_ascii(spec, 74, 22, False))
            lines.append(fence)
            print('\\n'.join(lines))
            return True
        except Exception:
            return False

    def show(*args, **kwargs):
        # Prefer the Java2D PNG backend: write the figure to a temp file and emit a
        # `vis-image` fence so a graphical TUI/web paints it inline, the ASCII plot
        # riding along as the fence's text fallback. No bridge (or a render failure)
        # falls back to printing the ASCII plot straight to stdout, so `plt.show()`
        # still shows the plot in a purely textual environment.
        if _state.get('series'):
            if _emit_image():
                return None
            print(_render_ascii(_spec(), kwargs.get('width', 74),
                                kwargs.get('height', 22),
                                kwargs.get('color', True)))
        return None

    class _Line(object):
        # Handle returned by plot(); supports `line, = plt.plot(...)` unpacking
        # and the common set_* mutators (they edit the accumulated series).
        def __init__(self, s):
            self._s = s
        def set_label(self, v):
            self._s['label'] = v
            return None
        def set_color(self, v):
            self._s['color'] = v
            return None
        def set_linestyle(self, v):
            self._s['linestyle'] = v
            return None
        def set_linewidth(self, *a, **k):
            return None
        def get_label(self):
            return self._s.get('label')

    def axis(*args, **kwargs):
        if not args:
            return (0.0, 1.0, 0.0, 1.0)
        a = args[0]
        if a is False or a == 'off':
            _state['axis_off'] = True
        elif a is True or a == 'on':
            _state['axis_off'] = False
        elif isinstance(a, (list, tuple)) and len(a) == 4:
            _state['xlim'] = [float(a[0]), float(a[1])]
            _state['ylim'] = [float(a[2]), float(a[3])]
        return (0.0, 1.0, 0.0, 1.0)

    def _quartiles(vals):
        xs = sorted(_nums(vals))
        if not xs:
            return None
        def q(p):
            if len(xs) == 1:
                return xs[0]
            idx = p * (len(xs) - 1)
            lo = int(idx)
            hi = min(lo + 1, len(xs) - 1)
            return xs[lo] + (xs[hi] - xs[lo]) * (idx - lo)
        return {'lo': xs[0], 'q1': q(0.25), 'q2': q(0.5),
                'q3': q(0.75), 'hi': xs[-1]}

    def boxplot(data, positions=None, labels=None, **kwargs):
        try:
            first = data[0]
            seqs = data if hasattr(first, '__iter__') else [data]
        except Exception:
            seqs = [data]
        stats = []
        for d in seqs:
            st = _quartiles(d)
            if st:
                stats.append(st)
        pos = ([float(p) for p in positions] if positions
               else list(range(1, len(stats) + 1)))
        ally = []
        for st in stats:
            ally.append(st['lo'])
            ally.append(st['hi'])
        s = _add_series('box', pos, ally, None, None)
        s['stats'] = stats
        s['positions'] = pos
        return {'boxes': stats}

    def imshow(data, cmap=None, aspect=None, extent=None,
               vmin=None, vmax=None, **kwargs):
        rows = [_nums(r) for r in data]
        nr = len(rows)
        nc = max((len(r) for r in rows), default=0)
        flat = [v for r in rows for v in r]
        lo = float(vmin) if vmin is not None else (min(flat) if flat else 0.0)
        hi = float(vmax) if vmax is not None else (max(flat) if flat else 1.0)
        s = _add_series('image', [0.0, float(nc)], [0.0, float(nr)], None, None)
        s['rows'] = rows
        s['nrows'] = nr
        s['ncols'] = nc
        s['vmin'] = lo
        s['vmax'] = hi
        return s

    def colorbar(*args, **kwargs):
        return None

    def hlines(y, xmin=None, xmax=None, colors=None, color=None,
               linestyles=None, linestyle=None, label=None, **kwargs):
        for yy in _nums(y):
            _add_series('hline', [], [yy], label, color or colors,
                        linestyle=(linestyle or linestyles))
        return None

    def vlines(x, ymin=None, ymax=None, colors=None, color=None,
               linestyles=None, linestyle=None, label=None, **kwargs):
        for xx in _nums(x):
            _add_series('vline', [xx], [], label, color or colors,
                        linestyle=(linestyle or linestyles))
        return None

    class _Axes(object):
        # Minimal OO Axes: every method delegates to the module-level artist so
        # `fig, ax = plt.subplots(); ax.plot(...)` works like the pyplot API.
        def plot(self, *a, **k):
            return plot(*a, **k)

        def scatter(self, *a, **k):
            return scatter(*a, **k)

        def bar(self, *a, **k):
            return bar(*a, **k)

        def barh(self, *a, **k):
            return barh(*a, **k)

        def hist(self, *a, **k):
            return hist(*a, **k)

        def fill_between(self, *a, **k):
            return fill_between(*a, **k)

        def step(self, *a, **k):
            return step(*a, **k)

        def pie(self, *a, **k):
            return pie(*a, **k)

        def errorbar(self, *a, **k):
            return errorbar(*a, **k)

        def axhline(self, *a, **k):
            return axhline(*a, **k)

        def axvline(self, *a, **k):
            return axvline(*a, **k)

        def text(self, *a, **k):
            return text(*a, **k)

        def annotate(self, *a, **k):
            return annotate(*a, **k)

        def legend(self, *a, **k):
            return legend(*a, **k)

        def grid(self, *a, **k):
            return grid(*a, **k)

        def set_title(self, s, **k):
            title(s)

        def set_xlabel(self, s, **k):
            xlabel(s)

        def set_ylabel(self, s, **k):
            ylabel(s)

        def set_xlim(self, *a, **k):
            return xlim(*a, **k)

        def set_ylim(self, *a, **k):
            return ylim(*a, **k)

        def set_xscale(self, v, **k):
            xscale(v)

        def set_yscale(self, v, **k):
            yscale(v)

        def set_xticks(self, *a, **k):
            return None

        def set_yticks(self, *a, **k):
            return None

        def tick_params(self, *a, **k):
            return None

        def twinx(self, *a, **k):
            return _Axes()

        def twiny(self, *a, **k):
            return _Axes()

        def axis(self, *a, **k):
            return axis(*a, **k)

        def imshow(self, *a, **k):
            return imshow(*a, **k)

        def boxplot(self, *a, **k):
            return boxplot(*a, **k)

        def hlines(self, *a, **k):
            return hlines(*a, **k)

        def vlines(self, *a, **k):
            return vlines(*a, **k)

        def set_xticklabels(self, *a, **k):
            return None

        def set_yticklabels(self, *a, **k):
            return None

    class _Figure(object):
        # Wraps the single global figure state so the OO idiom
        # `fig, ax = plt.subplots(); ...; fig.savefig(...)` works. Every method
        # delegates to the module-level artist / renderer.
        def savefig(self, *a, **k):
            return savefig(*a, **k)
        def to_ascii(self, *a, **k):
            return to_ascii(*a, **k)
        def suptitle(self, s, **k):
            _state['title'] = str(s)
            return None
        def tight_layout(self, *a, **k):
            return None
        def subplots_adjust(self, *a, **k):
            return None
        def set_size_inches(self, w, h=None, **k):
            if h is None and hasattr(w, '__len__'):
                w, h = w[0], w[1]
            _state['width'] = int(float(w) * 100)
            _state['height'] = int(float(h) * 100)
            return None
        def add_subplot(self, *a, **k):
            return _Axes()
        def add_axes(self, *a, **k):
            return _Axes()
        def gca(self, *a, **k):
            return _Axes()
        def colorbar(self, *a, **k):
            return None
        def legend(self, *a, **k):
            return legend(*a, **k)
        def clf(self, *a, **k):
            _reset()
        def align_labels(self, *a, **k):
            return None

    def subplots(nrows=1, ncols=1, figsize=None, dpi=None, **kwargs):
        figure(figsize=figsize, dpi=dpi)
        n = int(nrows) * int(ncols)
        if n <= 1:
            return _Figure(), _Axes()
        return _Figure(), [_Axes() for _ in range(n)]

    def subplot(*args, **kwargs):
        return _Axes()

    def gca(*args, **kwargs):
        return _Axes()

    def gcf(*args, **kwargs):
        return _Figure()

    _TAB10 = [(31, 119, 180), (255, 127, 14), (44, 160, 44), (214, 39, 40),
              (148, 103, 189), (140, 86, 75), (227, 119, 194),
              (127, 127, 127), (188, 189, 34), (23, 190, 207)]
    _CNAMED = {
        'b': (31, 119, 180), 'g': (44, 160, 44), 'r': (214, 39, 40),
        'c': (23, 190, 207), 'm': (191, 0, 191), 'y': (188, 189, 34),
        'k': (70, 70, 70), 'w': (230, 230, 230), 'blue': (31, 119, 180),
        'orange': (255, 127, 14), 'green': (44, 160, 44), 'red': (214, 39, 40),
        'purple': (148, 103, 189), 'brown': (140, 86, 75),
        'pink': (227, 119, 194), 'gray': (127, 127, 127),
        'grey': (127, 127, 127), 'olive': (188, 189, 34),
        'cyan': (23, 190, 207), 'magenta': (191, 0, 191),
        'yellow': (188, 189, 34), 'black': (70, 70, 70),
        'white': (230, 230, 230),
    }

    def _rgb(color, idx):
        # Resolve a matplotlib color spec to an (r,g,b) triple; fall back to the
        # tab10 cycle by series index when unknown/None.
        if color is None:
            return _TAB10[idx % len(_TAB10)]
        if isinstance(color, (tuple, list)):
            # An (r,g,b[,a]) NUMERIC tuple scales 0..1 floats to 0..255; a
            # per-element color LIST (e.g. bar(..., color=['#4C9F70', ...]))
            # resolves its first entry, so hex/named specs never reach float().
            nums = color[:3]
            if len(color) >= 3 and all(
                    isinstance(v, (int, float)) and not isinstance(v, bool)
                    for v in nums):
                return tuple(max(0, min(255, int(round(float(v) * 255))))
                             for v in nums)
            if len(color):
                return _rgb(color[0], idx)
            return _TAB10[idx % len(_TAB10)]
        s = str(color).strip()
        if s.startswith('#'):
            h = s[1:]
            if len(h) == 3:
                h = ''.join(ch * 2 for ch in h)
            if len(h) >= 6:
                try:
                    return (int(h[0:2], 16), int(h[2:4], 16), int(h[4:6], 16))
                except ValueError:
                    pass
        if len(s) >= 2 and s[0] == 'C' and s[1:].isdigit():
            return _TAB10[int(s[1:]) % len(_TAB10)]
        return _CNAMED.get(s.lower(), _TAB10[idx % len(_TAB10)])

    # Braille dot bit for (col in 0..1, row in 0..3) inside one 2x4 cell.
    _BRAILLE = ((0x01, 0x08), (0x02, 0x10), (0x04, 0x20), (0x40, 0x80))

    def _render_ascii(spec, width=74, height=22, color=False):
        # Pure-Python renderer of the current figure spec - no JVM, no image.
        # Rasterises line/scatter/step/fill/bar/hline/vline into a Unicode
        # BRAILLE canvas (2x4 sub-cell dots => smooth high-res curves) inside a
        # box-drawing frame with y/x tick labels, title, axis labels and a
        # per-series legend. `color=True` adds ANSI truecolor per series.
        series = spec.get('series') or []
        title = spec.get('title')
        xlabel = spec.get('xlabel')
        ylabel = spec.get('ylabel')
        all_x = []
        all_y = []
        has_bar = False
        cat_labels = None
        for s in series:
            k = str(s.get('kind'))
            if k in ('pie', 'image', 'box'):
                continue
            if k == 'bar':
                has_bar = True
                if cat_labels is None and s.get('labels'):
                    cat_labels = s.get('labels')
            all_x += [float(v) for v in (s.get('x') or [])]
            all_y += [float(v) for v in (s.get('y') or [])]
            if s.get('y2') is not None:
                all_y += [float(v) for v in s.get('y2')]
        xlim = spec.get('xlim')
        ylim = spec.get('ylim')
        if xlim:
            xmin, xmax = float(xlim[0]), float(xlim[1])
        elif all_x:
            xmin, xmax = min(all_x), max(all_x)
        else:
            xmin, xmax = 0.0, 1.0
        ys = list(all_y)
        if has_bar:
            ys.append(0.0)
        if ylim:
            ymin, ymax = float(ylim[0]), float(ylim[1])
        elif ys:
            ymin, ymax = min(ys), max(ys)
        else:
            ymin, ymax = 0.0, 1.0
        if xmax == xmin:
            xmin, xmax = xmin - 1.0, xmax + 1.0
        if ymax == ymin:
            ymin, ymax = ymin - 1.0, ymax + 1.0
        pad = 0.05 * (ymax - ymin)
        ymin -= pad
        ymax += pad
        Wc = max(20, int(width))
        Hc = max(8, int(height))
        DW = Wc * 2
        DH = Hc * 4
        cell = [[[0, None] for _ in range(Wc)] for _ in range(Hc)]

        def dput(dx, dy, rgb):
            if 0 <= dx < DW and 0 <= dy < DH:
                cc = cell[dy // 4][dx // 2]
                cc[0] |= _BRAILLE[dy % 4][dx % 2]
                cc[1] = rgb

        def dxof(x):
            return int(round((float(x) - xmin) / (xmax - xmin) * (DW - 1)))

        def dyof(y):
            return int(round((ymax - float(y)) / (ymax - ymin) * (DH - 1)))

        legend = []
        ci = 0
        for s in series:
            k = str(s.get('kind'))
            if k in ('pie', 'image', 'box'):
                continue
            rgb = _rgb(s.get('color'), ci)
            xs = [float(v) for v in (s.get('x') or [])]
            ysv = [float(v) for v in (s.get('y') or [])]
            if k == 'bar':
                base = dyof(max(ymin, min(ymax, 0.0)))
                for x, y in zip(xs, ysv):
                    dc = dxof(x)
                    dr = dyof(y)
                    lo, hi = (dr, base) if dr <= base else (base, dr)
                    for rr in range(lo, hi + 1):
                        for off in (-1, 0, 1):
                            dput(dc + off, rr, rgb)
            elif k == 'hline':
                for y in ysv:
                    dr = dyof(y)
                    for dc in range(DW):
                        dput(dc, dr, rgb)
            elif k == 'vline':
                for x in xs:
                    dc = dxof(x)
                    for dr in range(DH):
                        dput(dc, dr, rgb)
            elif k == 'fill':
                y2 = [float(v) for v in (s.get('y2') or [])]
                for i2, x in enumerate(xs):
                    if i2 >= len(ysv):
                        break
                    dc = dxof(x)
                    r1 = dyof(ysv[i2])
                    r2 = dyof(y2[i2] if i2 < len(y2) else 0.0)
                    lo, hi = (r1, r2) if r1 <= r2 else (r2, r1)
                    for rr in range(lo, hi + 1):
                        dput(dc, rr, rgb)
            elif k == 'scatter':
                for x, y in zip(xs, ysv):
                    dput(dxof(x), dyof(y), rgb)
            elif k == 'step':
                pts = list(zip(xs, ysv))
                for (x1, y1), (x2, y2) in zip(pts, pts[1:]):
                    c1, c2 = dxof(x1), dxof(x2)
                    r1, r2 = dyof(y1), dyof(y2)
                    for dc in range(min(c1, c2), max(c1, c2) + 1):
                        dput(dc, r1, rgb)
                    for dr in range(min(r1, r2), max(r1, r2) + 1):
                        dput(c2, dr, rgb)
            else:
                pts = list(zip(xs, ysv))
                for (x1, y1), (x2, y2) in zip(pts, pts[1:]):
                    c1, r1 = dxof(x1), dyof(y1)
                    c2, r2 = dxof(x2), dyof(y2)
                    n = max(abs(c2 - c1), abs(r2 - r1), 1)
                    for t in range(n + 1):
                        f = t / n
                        dput(int(round(c1 + (c2 - c1) * f)),
                             int(round(r1 + (r2 - r1) * f)), rgb)
                for x, y in pts:
                    dput(dxof(x), dyof(y), rgb)
            lbl = s.get('label')
            legend.append((rgb, str(lbl) if lbl else None))
            ci += 1

        def fmt(v):
            if abs(v) < 1e15 and float(v) == int(v):
                return str(int(v))
            return '%.3g' % v

        def paint(txt, rgb):
            if not color or rgb is None:
                return txt
            return '\\x1b[38;2;%d;%d;%dm%s\\x1b[0m' % (
                rgb[0], rgb[1], rgb[2], txt)

        def dim(txt):
            return ('\\x1b[90m' + txt + '\\x1b[0m') if color else txt

        nyt = min(5, Hc)
        yticks = {}
        for i in range(nyt):
            t = (i / (nyt - 1)) if nyt > 1 else 0.0
            yticks[int(round(t * (Hc - 1)))] = fmt(ymax - t * (ymax - ymin))
        lw = max((len(v) for v in yticks.values()), default=1)
        out = []
        if title:
            out.append(' ' * (lw + 1) + str(title).center(Wc))
        if ylabel:
            out.append(' ' * (lw + 1) + str(ylabel)[:Wc])
        for r in range(Hc):
            buf = []
            for cc in cell[r]:
                bits, rgb = cc
                buf.append(paint(chr(0x2800 + bits), rgb) if bits else ' ')
            axis = '┤' if r in yticks else '│'
            out.append(yticks.get(r, '').rjust(lw) + dim(axis) + ''.join(buf))
        axisrow = ['─'] * Wc
        nxt = min(5, Wc)
        xt = {}
        if cat_labels:
            for i, lab in enumerate(cat_labels):
                col = int(round((float(i) - xmin) / (xmax - xmin) * (Wc - 1)))
                if 0 <= col < Wc:
                    xt[col] = str(lab)
        else:
            for i in range(nxt):
                t = (i / (nxt - 1)) if nxt > 1 else 0.0
                xt[int(round(t * (Wc - 1)))] = fmt(xmin + t * (xmax - xmin))
        for col in xt:
            if 0 <= col < Wc:
                axisrow[col] = '┬'
        out.append(' ' * lw + dim('└' + ''.join(axisrow)))
        xrow = [' '] * Wc
        for col, label in xt.items():
            start = min(max(0, col - len(label) // 2), Wc - len(label))
            for j, chc in enumerate(label):
                if 0 <= start + j < Wc:
                    xrow[start + j] = chc
        out.append(' ' * (lw + 1) + ''.join(xrow))
        if xlabel:
            out.append(' ' * (lw + 1) + str(xlabel).center(Wc))
        labs = [(rgb, l) for (rgb, l) in legend if l]
        if labs:
            out.append('')
            out.append(' ' * (lw + 1) +
                       '   '.join(paint('●', rgb) + ' ' + l
                                  for rgb, l in labs))
        return '\\n'.join(out)

    def to_ascii(width=74, height=22, color=False):
        # Return the current figure as an ASCII-art string (no image, no file).
        return _render_ascii(_spec(), width, height, color)

    def _spec():
        return {
            'width': _state['width'], 'height': _state['height'],
            'title': _state['title'], 'xlabel': _state['xlabel'],
            'ylabel': _state['ylabel'], 'grid': _state['grid'],
            'legend': _state['legend'], 'xlim': _state['xlim'],
            'ylim': _state['ylim'], 'xscale': _state['xscale'],
            'yscale': _state['yscale'], 'axis_off': _state.get('axis_off', False),
            'annotations': list(_state['annotations']),
            'series': list(_state['series']),
        }

    def savefig(fname, format=None, dpi=None, **kwargs):
        # Text targets (.txt/.asc filename, or format 'ascii'/'txt') get the
        # pure-Python ASCII render; everything else goes through the Java2D PNG
        # backend and writes the returned bytes.
        is_text = str(format).lower() in ('ascii', 'txt') or (
            isinstance(fname, str) and fname.lower().endswith(('.txt', '.asc')))
        if is_text:
            txt = _render_ascii(_spec())
            if hasattr(fname, 'write'):
                fname.write(txt)
            else:
                with open(fname, 'w') as _f:
                    _f.write(txt)
            return fname
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

    class _RcParams(dict):
        # matplotlib.rcParams: a dict that never KeyErrors on unknown keys and
        # tolerates `.update(...)` / item assignment, so user rc tweaks are
        # harmless no-ops that still read back instead of blowing up the sandbox.
        def __missing__(self, key):
            return None

    _rcparams = _RcParams()
    _rcparams.update({
        'figure.figsize': [6.4, 4.8], 'figure.dpi': 100.0,
        'savefig.dpi': 100.0, 'lines.linewidth': 1.5, 'font.size': 10.0,
        'axes.grid': False, 'interactive': False, 'backend': 'Agg',
    })

    _backend = ['Agg']

    def use(backend=None, *a, **k):
        # matplotlib.use(...) — record the requested backend name; the vis shim
        # always renders through its Java2D backend regardless of the choice.
        if backend is not None:
            _backend[0] = str(backend)
        return None

    def switch_backend(backend=None, *a, **k):
        return use(backend)

    def get_backend():
        return _backend[0]

    def rc(*a, **k):
        return None

    def rcdefaults(*a, **k):
        return None

    def ion(*a, **k):
        return None

    def ioff(*a, **k):
        return None

    def isinteractive(*a, **k):
        return False

    def draw(*a, **k):
        return None

    def draw_if_interactive(*a, **k):
        return None

    def pause(*a, **k):
        return None

    def set_cmap(*a, **k):
        return None

    def margins(*a, **k):
        return None

    def minorticks_on(*a, **k):
        return None

    def minorticks_off(*a, **k):
        return None

    def clim(*a, **k):
        return None

    def figtext(x, y, s, *a, **k):
        _state['annotations'].append({'x': float(x), 'y': float(y), 'text': str(s)})
        return None

    pyplot = types.ModuleType('matplotlib.pyplot')
    pyplot.__doc__ = 'vis Java2D-backed matplotlib.pyplot subset.'
    for _fn in (figure, plot, scatter, bar, barh, hist, fill_between, step,
                axhline, axvline, hlines, vlines, pie, errorbar, text, annotate,
                title, suptitle, xlabel, ylabel, grid, legend, axis,
                xlim, ylim, xscale, yscale, semilogx, semilogy, loglog,
                xticks, yticks, tight_layout, subplots_adjust,
                boxplot, imshow, colorbar,
                clf, cla, close, show, savefig, to_ascii,
                subplots, subplot, gca, gcf,
                use, switch_backend, get_backend, rc, rcdefaults,
                ion, ioff, isinteractive, draw, draw_if_interactive, pause,
                set_cmap, margins, minorticks_on, minorticks_off, clim, figtext):
        setattr(pyplot, _fn.__name__, _fn)
    pyplot.Axes = _Axes
    pyplot.rcParams = _rcparams

    style = types.ModuleType('matplotlib.style')
    style.use = lambda *a, **k: None
    style.available = ['default', 'classic']

    mpl = types.ModuleType('matplotlib')
    mpl.__doc__ = 'vis matplotlib-compat shim (no CPython matplotlib wheel).'
    mpl.__version__ = '3.0-vis-java2d'
    mpl.pyplot = pyplot
    mpl.style = style
    mpl.use = use
    mpl.get_backend = get_backend
    mpl.rc = rc
    mpl.rcdefaults = rcdefaults
    mpl.rcParams = _rcparams

    sys.modules['matplotlib'] = mpl
    sys.modules['matplotlib.pyplot'] = pyplot
    sys.modules['matplotlib.style'] = style

    # Autoload: staple the module names onto builtins so `matplotlib.pyplot`,
    # a bare `pyplot`, and the conventional `plt` alias all work WITHOUT any
    # explicit import.
    try:
        import builtins as _b
        _b.matplotlib = mpl
        _b.pyplot = pyplot
        _b.plt = pyplot
    except Exception:
        pass

__vis_install_matplotlib__()
del __vis_install_matplotlib__
")

(def vis-extension
  (vis/extension
    {:ext/name "foundation-shim-matplotlib"
     :ext/description
     "Sandbox shim: a matplotlib.pyplot subset (plot/scatter/bar/barh/hist/fill_between/step/pie/boxplot/imshow/hlines/vlines/axhline/axvline + the OO Figure/Axes API with subplots, add_subplot, savefig, suptitle, tight_layout, set_size_inches, twinx; multi-pair plot with Line2D-like handles; axis('off'|[x0,x1,y0,y1]); log scales, markers, dashed styles, hex + named colors, viridis heatmaps, title/labels/grid/legend/text) with TWO renderers: a pure-JVM Java2D PNG backend and a pure-Python ASCII backend. plt.show() renders the figure to a PNG and paints it INLINE in a graphics-capable terminal (Kitty/iTerm2, e.g. Ghostty), automatically falling back to an ASCII plot on text-only terminals; plt.to_ascii() always returns the ASCII plot as a string; savefig writes a PNG (or ASCII for a '*.txt'/'*.asc' target). No pip, no native wheel."
     :ext/version "0.4.0"
     :ext/author "Blockether"
     :ext/owner "vis"
     :ext/license "Apache-2.0"
     :ext/kind "foundation"
     :ext/sandbox-shims
     [{:shim/name "matplotlib"
       :shim/description
       "matplotlib.pyplot subset (line/scatter/bar/hist/fill/step/pie/box/image + OO Figure/Axes) with Java2D PNG and ASCII renderers. plt.show() paints the real PNG INLINE on a graphics terminal (Kitty/iTerm2), falling back to ASCII on text-only terminals; plt.to_ascii() returns the ASCII plot; savefig writes PNG (or *.txt/*.asc ASCII). Not supported: animation, 3-D axes, full rcParams theming; only documented plot types render."
       :shim/bindings mpl-bridge-bindings
       :shim/preamble matplotlib-shim-src}]}))

(vis/register-extension! vis-extension)
