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
            [com.blockether.vis.internal.awt-boot :as awt-boot]
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
  (let
    [cs
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

(defn- even-ticks
  "Six evenly spaced tick values across [lo hi] (used for log axes)."
  [^double lo ^double hi]
  (mapv (fn [i] (+ lo (* (/ (double i) 5.0) (- hi lo)))) (range 6)))

(defn- nice-ticks
  "matplotlib-ish tick locator: round values (1/2/2.5/5 x 10^k), about `n` of
   them, covering [lo hi]. Falls back to the endpoints for a degenerate range."
  [^double lo ^double hi ^long n]
  (let [span (- hi lo)]
    (if (or (Double/isNaN span) (Double/isInfinite span) (<= span 0.0))
      [lo]
      (let [raw (/ span (double (max 1 n)))
            mag (Math/pow 10.0 (Math/floor (/ (Math/log raw) (Math/log 10.0))))
            norm (/ raw mag)
            step (* mag (double (cond (<= norm 1.0) 1.0
                                      (<= norm 2.0) 2.0
                                      (<= norm 2.5) 2.5
                                      (<= norm 5.0) 5.0
                                      :else 10.0)))
            start (* step (Math/ceil (/ lo step)))
            eps (* 1.0e-9 span)]
        (vec (take-while (fn [^double v] (<= v (+ hi eps)))
                         (take 64 (iterate (fn [^double v] (+ v step)) start))))))))

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
  (let
    [img
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
  (let
    [t
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
      (let
        [vals (mapv #(Math/abs (as-double %)) (get s "x"))
         labels (get s "labels")
         total (double (reduce + 0.0 vals))
         cx (/ W 2.0)
         cy (+ 12.0 (/ H 2.0))
         r (double (- (quot (long (min W H)) 2) 66))
         start (volatile! 90.0)]

        (.setFont g (Font. "SansSerif" Font/PLAIN 11))
        (dotimes [i (count vals)]
          (let
            [frac (if (< 0.0 total) (/ (double (nth vals i)) total) 0.0)
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
            (let
              [lx (+ cx (* (+ r 16) (Math/cos mid)))
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
  (let
    [title
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
     (if (== (double xmin) (double xmax)) [(- (double xmin) 1.0) (+ (double xmax) 1.0)] [xmin xmax])

     [ymin ymax]
     (if (== (double ymin) (double ymax)) [(- (double ymin) 1.0) (+ (double ymax) 1.0)] [ymin ymax])

     slots
     (long (max 1 (long (reduce max 1 (map #(count (series-xs %)) series)))))

     xpad
     (if (seq xlim)
       0.0
       (+ (* 0.05 (- (double xmax) (double xmin)))
          (if has-bar?
            (* 0.5 (/ (- (double xmax) (double xmin)) (double (max 1 (dec slots)))))
            0.0)))

     xmin
     (- (double xmin) xpad)

     xmax
     (+ (double xmax) xpad)

     ypad
     (if (seq ylim) 0.0 (* 0.05 (- (double ymax) (double ymin))))

     ymin
     (if (and has-bar? (>= (double ymin) 0.0)) (double ymin) (- (double ymin) ypad))

     ymax
     (+ (double ymax) ypad)

     xticks-spec
     (seq (get spec "xticks"))

     xticklabels
     (vec (or (get spec "xticklabels") []))

     yticks-spec
     (seq (get spec "yticks"))

     yticklabels
     (vec (or (get spec "yticklabels") []))

     yticks
     (cond
       yticks-spec (mapv (fn [v] (yfwd (as-double v))) yticks-spec)
       ylog? (even-ticks (double ymin) (double ymax))
       :else (nice-ticks (double ymin) (double ymax) 5))

     xticks
     (cond
       xticks-spec (mapv (fn [v] (xfwd (as-double v))) xticks-spec)
       (or xlog? cat-labels) (even-ticks (double xmin) (double xmax))
       :else (nice-ticks (double xmin) (double xmax) 5))

     ytick-strs
     (vec (map-indexed (fn [i v]
                         (if (< (long i) (long (count yticklabels)))
                           (str (nth yticklabels i))
                           (fmt-num (yinv (double v)))))
                       yticks))

     xtick-strs
     (vec (map-indexed (fn [i v]
                         (if (< (long i) (long (count xticklabels)))
                           (str (nth xticklabels i))
                           (fmt-num (xinv (double v)))))
                       xticks))

     ml
     (long (max 46 (+ 18 (* 6 (long (reduce max 3 (map count ytick-strs)))))))

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
        (.setFont g (Font. "SansSerif" Font/PLAIN 10))
        (let [fm (.getFontMetrics g)]
          (dotimes [i (count yticks)]
            (let [yv (double (nth yticks i))
                  yp (int (syf yv))]
              (when (and (<= (double ymin) yv) (<= yv (double ymax)))
                (when grid?
                  (.setColor g (Color. 230 230 230))
                  (.drawLine g px0 yp (+ px0 pw) yp))
                (.setColor g (Color. 90 90 90))
                (let [^String yl (nth ytick-strs i)]
                  (.drawString g yl (int (- px0 6 (.stringWidth fm yl))) (int (+ yp 4)))))))
          (if (and cat-labels (not xticks-spec))
            (dotimes [i (count cat-labels)]
              (let [^String xl (str (nth cat-labels i))
                    xp (int (sx (double i)))]
                (when grid?
                  (.setColor g (Color. 230 230 230))
                  (.drawLine g xp py0 xp (+ py0 ph)))
                (.setColor g (Color. 90 90 90))
                (.drawString g xl (int (- xp (quot (.stringWidth fm xl) 2))) (int (+ py0 ph 16)))))
            (dotimes [i (count xticks)]
              (let [xv (double (nth xticks i))
                    xp (int (sxf xv))]
                (when (and (<= (double xmin) xv) (<= xv (double xmax)))
                  (when grid?
                    (.setColor g (Color. 230 230 230))
                    (.drawLine g xp py0 xp (+ py0 ph)))
                  (.setColor g (Color. 90 90 90))
                  (let [^String xl (nth xtick-strs i)]
                    (.drawString g xl (int (- xp (quot (.stringWidth fm xl) 2))) (int (+ py0 ph 16)))))))))
        ;; axes frame
        (.setColor g (Color. 60 60 60))
        (.setStroke g (BasicStroke. 1.0))
        (.drawRect g px0 py0 pw ph))
      ;; series
      (let
        [nbar
         (count (filter #(= "bar" (str (get % "kind"))) series))

         bar-slots
         (long (max 1 (long (reduce max 1 (map #(count (series-xs %)) series)))))]

        (doseq [[idx s] (map-indexed vector series)]
          (let
            [kind (str (get s "kind"))
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
              (let
                [bw (long (max 2 (int (* (quot pw bar-slots) (/ 0.7 (long (max 1 nbar)))))))
                 y0 (int (syf (max ymin (min ymax 0.0))))]

                (doseq [[x y] pts]
                  (let
                    [yp (int (sy y))
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
              (let
                [y2 (mapv as-double (get s "y2"))
                 n (count xs)]

                (when (and (pos? n) (= n (count y2)))
                  (let
                    [xsi (int-array (concat (map #(int (sx %)) xs)
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
              (let
                [rows (get s "rows")
                 nr (int (as-double (get s "nrows")))
                 nc (int (as-double (get s "ncols")))
                 vmin (as-double (get s "vmin"))
                 vmax (as-double (get s "vmax"))
                 span (let [d (- vmax vmin)]
                        (if (zero? d) 1.0 d))]

                (dotimes [ri nr]
                  (let [row (nth rows ri nil)]
                    (dotimes [ci nc]
                      (let
                        [v (as-double (nth row ci 0))
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
              (let
                [stats (get s "stats")
                 pos (mapv as-double (get s "positions"))
                 bw 24]

                (doseq [[bi st] (map-indexed vector stats)]
                  (let
                    [xc (int (sx (nth pos bi (inc (long bi)))))
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
        (let
          [fm
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
      (let
        [labelled (filter #(let [l (get % "label")]

                             (and (string? l) (seq l)))
                          (map-indexed (fn [i s]
                                         (assoc s "__idx" i))
                                       series))]
        (when (and (seq labelled) (or legend? (seq labelled)))
          (.setFont g (Font. "SansSerif" Font/PLAIN 11))
          (let
            [fm (.getFontMetrics g)
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
  (let
    [W
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
  ;; `awt-boot/ensure!` forced here: in a native image the headless/font
  ;; bootstrap must run at runtime before the first Graphics2D call, or Java2D
  ;; dies with NoClassDefFoundError java/awt/event/InputEvent. Every render funnels
  ;; through this envelope.
  (awt-boot/ensure!)
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
       #(let
          [b64
           (render-png-base64 spec)

           bytes
           (.decode (Base64/getDecoder) ^String b64)

           f
           (mpl-capture/display-cache-file "fig-" "png" bytes)

           w
           (int (as-double (or (get spec "width") 640)))

           h
           (int (as-double (or (get spec "height") 480)))]

          ;; SINK the bytes at the source — the engine OWNS this figure (a DB
          ;; attachment) with no stdout-fence parsing. The cache file stays only
          ;; for the inline display fence (durable, so history re-renders it).
          (mpl-capture/record-attachment! {:kind "image"
                                           :media-type "image/png"
                                           :base64 b64
                                           :size (alength bytes)
                                           :filename (.getName f)
                                           :dims (str w "x" h)})
          [(.getAbsolutePath f) w h (alength bytes)])))})


(def vis-extension
  (vis/extension
    {:ext/name "foundation-shim-matplotlib"
     :ext/description
     "Sandbox shim: a matplotlib.pyplot subset (plot/scatter/bar/barh/hist/fill_between/step/pie/boxplot/imshow/hlines/vlines/axhline/axvline + the OO Figure/Axes API with subplots, add_subplot, savefig, suptitle, tight_layout, set_size_inches, twinx; multi-pair plot with Line2D-like handles; axis('off'|[x0,x1,y0,y1]); log scales, markers, dashed styles, hex + named colors, viridis heatmaps, title/labels/grid/legend/text) with TWO renderers: a pure-JVM Java2D PNG backend and a pure-Python ASCII backend. plt.show() is the ONE display call: it renders the figure to a PNG and paints it INLINE in a graphics-capable terminal (Kitty/iTerm2, e.g. Ghostty), automatically falling back to an ASCII plot on text-only terminals; savefig writes a PNG (or ASCII for a '*.txt'/'*.asc'/format='txt' target, honoring width/height/color kwargs). No pip, no native wheel."
     :ext/version "0.4.0"
     :ext/author "Blockether"
     :ext/owner "vis"
     :ext/license "Apache-2.0"
     :ext/kind "foundation"
     :ext/sandbox-shims
     [{:shim/name "matplotlib"
       :shim/imports ["matplotlib"]
       :shim/description
       "matplotlib.pyplot subset (line/scatter/bar/hist/fill/step/pie/box/image + OO Figure/Axes) with Java2D PNG and ASCII renderers. plt.show() is the ONE display call — it paints the real PNG INLINE on a graphics terminal (Kitty/iTerm2) and automatically falls back to ASCII on text-only terminals; savefig writes PNG (or *.txt/*.asc/format='txt' ASCII, honoring width/height/color kwargs). Not supported: animation, 3-D axes, full rcParams theming; only documented plot types render."
       :shim/bindings mpl-bridge-bindings
       :shim/source "vis-shims/matplotlib.py"}]}))

(vis/register-extension! vis-extension)
