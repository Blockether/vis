(ns com.blockether.vis.internal.foundation.shim-matplotlib
  "Built-in sandbox SHIM: a minimal `matplotlib.pyplot`-compatible module for the
   model's Python sandbox, backed by a host-side vector renderer. The agent
   sandbox ships no CPython matplotlib wheel (it needs numpy's native core +
   freetype, all blocked by the deny-by-default Context); this extension instead
   contributes a `:ext/sandbox-shims` entry whose Python preamble accumulates the
   familiar pyplot artists (`plot`/`scatter`/`bar`/`hist`/`fill_between`/`step`/
   `pie`/`axhline`/`axvline`/`title`/`xlabel`/… plus the OO `subplots`/`Axes`
   API) and whose `savefig` DELEGATES the whole figure spec across the boundary
   to the host callable `__vis_mpl_render__`, which draws it with
   `com.blockether/imaging` and returns a base64 PNG. The Python side
   base64-decodes and writes it (to a path, confined to the sandbox roots, or to
   any file-like buffer).

   Rendering uses NO `java.desktop`: `imaging` is FFM over a Rust cdylib
   (tiny-skia + embedded Noto faces), so there is no AWT toolkit, no headless
   dance and no system fontconfig anywhere in this path.

   It is a SUBSET, not real matplotlib: line/scatter/bar/hist/fill/step/pie with
   title, axis labels, grid, legend, dashed line styles, markers, log scales and
   text annotations — enough for the model to visualize data. Rendering runs
   entirely in-process (no pip, no native wheels), and any render failure
   surfaces to Python as a catchable exception (never crashes the sandbox).

   Together with `shim-yaml` this demonstrates the sandbox-shim mechanism: an
   extension turns a host / JVM capability into a real importable Python module
   while `env-python` stays completely generic about which shims exist."
  (:require [com.blockether.imaging :as im]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.internal.foundation.mpl-capture :as mpl-capture])
  (:import [java.util Base64 Locale]))

;; Host renderer — `com.blockether/imaging` vector ops. Input is the pyplot
;; figure spec (string-keyed map marshalled from the sandbox); output is a
;; base64 PNG string. Every drawing primitive is an op map handed to
;; `imaging/draw!` in ONE batch per figure, which keeps FFI chatter out of the
;; plotting loops.

(def ^:private sans
  "The embedded font family used for every label (present in the cdylib's own
   font database, so it resolves identically in the native image)."
  "Noto Sans")

(def ^:private grid-color [230 230 230])

(def ^:private tick-color [90 90 90])

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
   "pink" [227 119 194]
   ;; the tab10 cycle, reachable both bare and as "tab:<name>"
   "olive" [188 189 34]
   "crimson" [220 20 60]
   "navy" [0 0 128]
   "teal" [0 128 128]
   "gold" [255 215 0]
   "indigo" [75 0 130]
   "salmon" [250 128 114]
   "skyblue" [135 206 235]
   "lime" [0 255 0]
   "maroon" [128 0 0]
   "silver" [192 192 192]
   "tan" [210 180 140]
   "violet" [238 130 238]
   "turquoise" [64 224 208]
   "coral" [255 127 80]
   "khaki" [240 230 140]
   "lavender" [230 230 250]
   "plum" [221 160 221]
   "orchid" [218 112 214]
   "beige" [245 245 220]
   "chocolate" [210 105 30]
   "darkblue" [0 0 139]
   "darkgreen" [0 100 0]
   "darkred" [139 0 0]
   "lightblue" [173 216 230]
   "lightgreen" [144 238 144]
   "lightgray" [211 211 211]
   "lightgrey" [211 211 211]
   "steelblue" [70 130 180]
   "seagreen" [46 139 87]
   "royalblue" [65 105 225]
   "firebrick" [178 34 34]
   "goldenrod" [218 165 32]})

(defn- hex->rgb
  [^String s]
  (let [h (.replace s "#" "")]
    (when (= 6 (count h))
      (try [(Integer/parseInt (subs h 0 2) 16) (Integer/parseInt (subs h 2 4) 16)
            (Integer/parseInt (subs h 4 6) 16)]
           (catch Exception _ nil)))))

(defn- ->rgb
  "The series colour as an `[r g b]` vector: a named colour, a `#rrggbb` string,
   else the palette entry for series index `idx`."
  [c idx]
  (let [cs
        (when (and (string? c) (seq c)) (.toLowerCase ^String (str c)))

        ;; matplotlib spells the same colour "tab:blue", "xkcd:blue" or "blue";
        ;; "C0".."C9" is the property cycle, and a bare "0.4" is a grey level.
        cs
        (when cs
          (cond (.startsWith ^String cs "tab:") (subs cs 4)
                (.startsWith ^String cs "xkcd:") (subs cs 5)
                :else cs))

        grey
        (when (and cs (re-matches #"0?\.[0-9]+|[01](\.0*)?" cs))
          (let [v (long (Math/round (* 255.0 (Double/parseDouble cs))))]
            [v v v]))

        cyc
        (when (and cs (re-matches #"c[0-9]" cs))
          (nth palette (mod (Integer/parseInt (subs cs 1)) (count palette))))]

    (or (when cs (get named-colors cs))
        grey
        cyc
        (when (and cs (.startsWith ^String cs "#")) (hex->rgb cs))
        (nth palette (mod (int idx) (count palette))))))

(defn- as-double
  ^double [x]
  (cond (number? x) (double x)
        :else (try (Double/parseDouble (str x)) (catch Exception _ 0.0))))

(defn- rnd
  "Round a *boxed* scale-function result to the nearest long.

   The scale fns (`sx`/`sy`/`sxf`/`syf`) are local `fn` values, so the compiler
   only ever sees `Object` at their call sites: a bare `(Math/round (sx x))`
   therefore compiles to REFLECTION, which the native image cannot resolve —
   `java.lang.Math` is not registered, so every plot died with
   \"matplotlib render failed: java.lang.Math\" in the binary while working fine
   on the JVM. Coercing here keeps the call static (and the result a primitive
   long, so the arithmetic on it stays unboxed)."
  ^long [v]
  (Math/round (double v)))

(defn- fmt-num
  ^String [^double v]
  (if (== v (Math/rint v)) (str (long v)) (String/format Locale/ROOT "%.2f" (object-array [v]))))

(defn- even-ticks
  "Six evenly spaced tick values across [lo hi] (used for log axes)."
  [^double lo ^double hi]
  (mapv (fn [i]
          (+ lo (* (/ (double i) 5.0) (- hi lo))))
        (range 6)))

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
            step (* mag
                    (double (cond (<= norm 1.0) 1.0
                                  (<= norm 2.0) 2.0
                                  (<= norm 2.5) 2.5
                                  (<= norm 5.0) 5.0
                                  :else 10.0)))
            start (* step (Math/ceil (/ lo step)))
            eps (* 1.0e-9 span)]

        (vec (take-while (fn [^double v]
                           (<= v (+ hi eps)))
                         (take 64
                               (iterate (fn [^double v]
                                          (+ v step))
                                        start))))))))

(defn- series-xs [s] (mapv as-double (get s "x")))

(defn- series-ys [s] (mapv as-double (get s "y")))

(defn- dash-style
  "Stroke style keys for a matplotlib `linestyle`, merged into a draw op."
  [linestyle width]
  (case (str linestyle)
    "--"
    {:stroke-width width :cap :butt :join :round :dash [9.0 6.0]}

    ":"
    {:stroke-width width :cap :round :join :round :dash [2.0 5.0]}

    "-."
    {:stroke-width width :cap :butt :join :round :dash [10.0 5.0 2.0 5.0]}

    {:stroke-width width :cap :round :join :round}))

(defn- text-width
  "Advance width of `s` at `size` px in the shim's font (the `FontMetrics`
   replacement used for centring and legend boxes)."
  ^double [^String s ^double size weight]
  (double (or (:width (im/text-measure {:text s :size size :family sans :weight weight})) 0.0)))


(defn- png-base64 ^String [img] (.encodeToString (Base64/getEncoder) ^bytes (im/encode img :png)))

(defn- lerp-col
  [[r1 g1 b1] [r2 g2 b2] ^double t]
  [(+ (double r1) (* (- (double r2) (double r1)) t))
   (+ (double g1) (* (- (double g2) (double g1)) t))
   (+ (double b1) (* (- (double b2) (double b1)) t))])

(defn- viridis
  [^double t]
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

    [(int a) (int b) (int c)]))

(defn- title-ops
  "Draw ops for the figure title, centred over the plot area (empty when none)."
  [title ^long px0 ^long pw]
  (if (and (string? title) (seq title))
    [{:op :text
      :text title
      :x (+ px0 (quot pw 2))
      :y 18
      :anchor :middle
      :size 14
      :weight 700
      :family sans
      :fill [30 30 30]}]
    []))

(defn- render-pie
  "Full-canvas pie chart (ignores axes). `s` carries sizes in `x` and optional
   `labels`. Wedge angles are SCREEN degrees — clockwise from 3 o'clock — so the
   first slice starts at 12 o'clock (-90) and each one advances clockwise."
  ^String [^long W ^long H spec s]
  (let [vals
        (mapv #(Math/abs (as-double %)) (get s "x"))

        labels
        (get s "labels")

        total
        (double (reduce + 0.0 vals))

        cx
        (/ W 2.0)

        cy
        (+ 12.0 (/ H 2.0))

        r
        (double (- (quot (long (min W H)) 2) 66))

        img
        (im/blank W H "white")

        fracs
        (mapv (fn [v]
                (if (< 0.0 total) (/ (double v) total) 0.0))
              vals)

        ops
        (reduce (fn [acc i]
                  (let [frac
                        (double (nth fracs i))

                        ang
                        (* 360.0 frac)

                        a0
                        (- (* 360.0 (double (reduce + 0.0 (take i fracs)))) 90.0)

                        mid
                        (Math/toRadians (+ a0 (/ ang 2.0)))

                        lx
                        (+ cx (* (+ r 16) (Math/cos mid)))

                        ly
                        (+ cy (* (+ r 16) (Math/sin mid)))

                        lbl
                        (if (and labels (< (long i) (count labels)))
                          (str (nth labels i))
                          (str (Math/round (* 100.0 frac)) "%"))]

                    (conj
                      acc
                      {:op :wedge :cx cx :cy cy :r r :start a0 :end (+ a0 ang) :fill (->rgb nil i)}
                      {:op :text
                       :text lbl
                       :x lx
                       :y ly
                       :size 11
                       :family sans
                       :fill [40 40 40]
                       :anchor (if (neg? (Math/cos mid)) :end :start)})))
                []
                (range (count vals)))]

    (im/draw! img (into ops (title-ops (get spec "title") 0 W)))
    (png-base64 img)))

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

        ;; matplotlib draws the legend as soon as artists carry labels; `legend()`
        ;; itself only forces it on, which is what `labelled` already covers.
        _legend?
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
        (if (== (double xmin) (double xmax))
          [(- (double xmin) 1.0) (+ (double xmax) 1.0)]
          [xmin xmax])

        [ymin ymax]
        (if (== (double ymin) (double ymax))
          [(- (double ymin) 1.0) (+ (double ymax) 1.0)]
          [ymin ymax])

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
        (cond yticks-spec (mapv (fn [v]
                                  (yfwd (as-double v)))
                                yticks-spec)
              ylog? (even-ticks (double ymin) (double ymax))
              :else (nice-ticks (double ymin) (double ymax) 5))

        xticks
        (cond xticks-spec (mapv (fn [v]
                                  (xfwd (as-double v)))
                                xticks-spec)
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

        img
        (im/blank W H "white")

        ops
        (volatile! [])

        add!
        (fn [& os]
          (vswap! ops into (remove nil? os)))]

    ;; gridlines + tick labels + frame (all skipped when axis('off'))
    (when-not axis-off?
      (dotimes [i (count yticks)]
        (let [yv (double (nth yticks i))
              yp (rnd (syf yv))]

          (when (and (<= (double ymin) yv) (<= yv (double ymax)))
            (when grid?
              (add! {:op :line
                     :x1 px0
                     :y1 yp
                     :x2 (+ px0 pw)
                     :y2 yp
                     :stroke grid-color
                     :stroke-width 1}))
            (add! {:op :text
                   :text (str (nth ytick-strs i))
                   :x (- px0 6)
                   :y (+ yp 4)
                   :anchor :end
                   :size 10
                   :family sans
                   :fill tick-color}))))
      (if (and cat-labels (not xticks-spec))
        (dotimes [i (count cat-labels)]
          (let [xl (str (nth cat-labels i))
                xp (rnd (sx (double i)))]

            (when grid?
              (add! {:op :line
                     :x1 xp
                     :y1 py0
                     :x2 xp
                     :y2 (+ py0 ph)
                     :stroke grid-color
                     :stroke-width 1}))
            (add! {:op :text
                   :text xl
                   :x xp
                   :y (+ py0 ph 16)
                   :anchor :middle
                   :size 10
                   :family sans
                   :fill tick-color})))
        (dotimes [i (count xticks)]
          (let [xv (double (nth xticks i))
                xp (rnd (sxf xv))]

            (when (and (<= (double xmin) xv) (<= xv (double xmax)))
              (when grid?
                (add! {:op :line
                       :x1 xp
                       :y1 py0
                       :x2 xp
                       :y2 (+ py0 ph)
                       :stroke grid-color
                       :stroke-width 1}))
              (add! {:op :text
                     :text (str (nth xtick-strs i))
                     :x xp
                     :y (+ py0 ph 16)
                     :anchor :middle
                     :size 10
                     :family sans
                     :fill tick-color})))))
      ;; axes frame
      (add! {:op :rect :x px0 :y py0 :w pw :h ph :stroke [60 60 60] :stroke-width 1}))
    ;; series
    (let [nbar
          (count (filter #(= "bar" (str (get % "kind"))) series))

          bar-slots
          (long (max 1 (long (reduce max 1 (map #(count (series-xs %)) series)))))]

      (doseq [[idx s] (map-indexed vector series)]
        (let [kind (str (get s "kind"))
              xs (series-xs s)
              ys (series-ys s)
              col (->rgb (get s "color") idx)
              pts (map vector xs ys)]

          (case kind
            "scatter"
            (doseq [[x y] pts]
              (add! {:op :circle :cx (sx x) :cy (sy y) :r 3 :fill col}))

            "bar"
            (let [bw (long (max 2 (int (* (quot pw bar-slots) (/ 0.7 (long (max 1 nbar)))))))
                  y0 (rnd (syf (max ymin (min ymax 0.0))))]

              (doseq [[x y] pts]
                (let [yp (rnd (sy y))
                      top (min y0 yp)
                      hgt (Math/abs (- y0 yp))]

                  (add! {:op :rect
                         :x (- (rnd (sx x)) (quot bw 2))
                         :y top
                         :w bw
                         :h (max 1 hgt)
                         :fill col}))))

            "hline"
            (when (seq ys)
              (let [yp (rnd (sy (first ys)))]
                (add! (merge {:op :line :x1 px0 :y1 yp :x2 (+ px0 pw) :y2 yp :stroke col}
                             (dash-style (get s "linestyle") 1.5)))))

            "vline"
            (when (seq xs)
              (let [xp (rnd (sx (first xs)))]
                (add! (merge {:op :line :x1 xp :y1 py0 :x2 xp :y2 (+ py0 ph) :stroke col}
                             (dash-style (get s "linestyle") 1.5)))))

            "fill"
            (let [y2 (mapv as-double (get s "y2"))
                  n (count xs)]

              (when (and (pos? n) (= n (count y2)))
                (add! {:op :polygon
                       :points (vec (concat (map (fn [x y]
                                                   [(sx x) (sy y)])
                                                 xs
                                                 ys)
                                            (map (fn [x y]
                                                   [(sx x) (sy y)])
                                                 (reverse xs)
                                                 (reverse y2))))
                       :close true
                       :fill (conj (vec col) 90)})))

            "step"
            (when (seq pts)
              (add! (merge {:op :polyline
                            :points (vec (cons [(sx (ffirst pts)) (sy (second (first pts)))]
                                               (mapcat (fn [[[_ y1] [x2 y2]]]
                                                         [[(sx x2) (sy y1)] [(sx x2) (sy y2)]])
                                                       (partition 2 1 pts))))
                            :stroke col}
                           (dash-style (get s "linestyle") 2.0))))

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
                          cell (viridis (/ (- v vmin) span))
                          x0 (rnd (sx ci))
                          x1 (rnd (sx (inc ci)))
                          yt (rnd (sy (- nr ri)))
                          yb (rnd (sy (- nr (inc ri))))]

                      (add! {:op :rect
                             :x (min x0 x1)
                             :y (min yt yb)
                             :w (max 1 (Math/abs (- x1 x0)))
                             :h (max 1 (Math/abs (- yb yt)))
                             :fill cell}))))))

            "box"
            (let [stats (get s "stats")
                  pos (mapv as-double (get s "positions"))
                  bw 24]

              (doseq [[bi st] (map-indexed vector stats)]
                (let [xc (rnd (sx (nth pos bi (inc (long bi)))))
                      q1 (rnd (sy (as-double (get st "q1"))))
                      q2 (rnd (sy (as-double (get st "q2"))))
                      q3 (rnd (sy (as-double (get st "q3"))))
                      lo (rnd (sy (as-double (get st "lo"))))
                      hi (rnd (sy (as-double (get st "hi"))))
                      hw (long (/ bw 2))
                      bcol (->rgb nil bi)
                      whisker {:stroke [60 60 60] :stroke-width 1.5}]

                  (add! (merge {:op :line :x1 xc :y1 lo :x2 xc :y2 q1} whisker)
                        (merge {:op :line :x1 xc :y1 q3 :x2 xc :y2 hi} whisker)
                        (merge {:op :line :x1 (- xc hw) :y1 lo :x2 (+ xc hw) :y2 lo} whisker)
                        (merge {:op :line :x1 (- xc hw) :y1 hi :x2 (+ xc hw) :y2 hi} whisker)
                        {:op :rect
                         :x (- xc hw)
                         :y (min q1 q3)
                         :w bw
                         :h (max 1 (Math/abs (- q3 q1)))
                         :fill bcol}
                        {:op :rect
                         :x (- xc hw)
                         :y (min q1 q3)
                         :w bw
                         :h (max 1 (Math/abs (- q3 q1)))
                         :stroke [30 30 30]
                         :stroke-width 1}
                        (merge {:op :line :x1 (- xc hw) :y1 q2 :x2 (+ xc hw) :y2 q2}
                               {:stroke [30 30 30] :stroke-width 1.5})))))

            ;; default: line (+ optional markers)
            (do (when (seq pts)
                  (add! (merge {:op :polyline
                                :points (mapv (fn [[x y]]
                                                [(sx x) (sy y)])
                                              pts)
                                :stroke col}
                               (dash-style (get s "linestyle") 2.0))))
                (when (seq (str (get s "marker")))
                  (doseq [[x y] pts]
                    (add! {:op :circle :cx (sx x) :cy (sy y) :r 3 :fill col}))))))))
    ;; title / axis labels
    (vswap! ops into (title-ops title px0 pw))
    (when (and (string? xlabel) (seq xlabel))
      (add! {:op :text
             :text xlabel
             :x (+ px0 (quot pw 2))
             :y (- H 12)
             :anchor :middle
             :size 12
             :family sans
             :fill [30 30 30]}))
    (when (and (string? ylabel) (seq ylabel))
      (add! {:op :text
             :text ylabel
             :x 16
             :y (+ py0 (quot ph 2))
             :rotate -90
             :anchor :middle
             :size 12
             :family sans
             :fill [30 30 30]}))
    ;; text annotations (data coords)
    (when (seq annotations)
      (doseq [a annotations]
        (add! {:op :text
               :text (str (get a "text"))
               :x (sx (as-double (get a "x")))
               :y (sy (as-double (get a "y")))
               :size 11
               :family sans
               :fill [20 20 20]})))
    ;; legend
    (let [labelled (filter #(let [l (get % "label")] (and (string? l) (seq l)))
                           (map-indexed (fn [i s]
                                          (assoc s "__idx" i))
                                        series))]
      (when (seq labelled)
        (let [rows (vec labelled)
              lw (+ 34
                    (long (Math/ceil (double (reduce max
                                                     0.0
                                                     (map
                                                       #(text-width (str (get % "label")) 11.0 400)
                                                       rows))))))
              lh (+ 8 (* 16 (count rows)))
              lx (- (+ px0 pw) lw 8)
              ly (+ py0 8)]

          (add! {:op :rect :x lx :y ly :w lw :h lh :fill [255 255 255]}
                {:op :rect :x lx :y ly :w lw :h lh :stroke [180 180 180] :stroke-width 1})
          (doseq [[ri s] (map-indexed vector rows)]
            (let [yy (+ ly 8 (* (long ri) 16))]
              (add! {:op :rect
                     :x (+ lx 8)
                     :y (+ yy 3)
                     :w 16
                     :h 6
                     :fill (->rgb (get s "color") (get s "__idx"))}
                    {:op :text
                     :text (str (get s "label"))
                     :x (+ lx 30)
                     :y (+ yy 11)
                     :size 11
                     :family sans
                     :fill [40 40 40]}))))))
    (im/draw! img @ops)
    (png-base64 img)))









(defn- render-png-base64
  "Render the figure `spec` (string-keyed map) to a PNG and return it base64.
   Axes3D figures go through imaging's native Rust plot engine; pie and XY keep
   the compatibility renderer. Throws on drawing failure (the caller envelopes
   it for Python)."
  ^String [spec]
  (let [W
        (int (as-double (or (get spec "width") 640)))

        H
        (int (as-double (or (get spec "height") 480)))

        series
        (vec (get spec "series"))

        pie
        (first (filter #(= "pie" (str (get % "kind"))) series))]

    (cond (= "3d" (str (get spec "projection"))) (with-open [img (im/plot spec)]
                                                   (png-base64 img))
          pie (render-pie W H spec pie)
          :else (render-xy W H spec series))))

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
   "__vis_mpl_render_file__" (fn [spec]
                               (mpl-envelope
                                 #(let [b64 (render-png-base64 spec) bytes
                                        (.decode (Base64/getDecoder) ^String b64) f
                                        (mpl-capture/display-cache-file "fig-" "png" bytes) w
                                        (int (as-double (or (get spec "width") 640))) h
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
     (str "Sandbox Matplotlib subset with pyplot/OO Figure-Axes APIs and `mpl_toolkits.mplot3d`: "
          "line/scatter/bar/hist/fill/step/pie/box/image plus "
          "surface/wireframe/contour/scatter/plot/text/bar3d, styling, labels, legends, "
          "subplots, views, and colormaps. Native imaging renders PNG; pure Python renders "
          "ASCII/Braille. `show()` displays inline PNG or text fallback; `savefig` writes PNG or "
          "txt/asc. No pip/native wheel.")
     :ext/version "0.5.0"
     :ext/author "Blockether"
     :ext/owner "vis"
     :ext/license "Apache-2.0"
     :ext/kind "foundation"
     :ext/sandbox-shims
     [{:shim/name "matplotlib"
       :shim/imports ["matplotlib" "mpl_toolkits"]
       :shim/docs
       (str "matplotlib.pyplot/OO subset with native PNG and ASCII renderers: "
            "line/scatter/bar/hist/fill/step/pie/box/image. "
            "mpl_toolkits.mplot3d supports surface/wireframe/contour/scatter/plot/text/bar3d, "
            "views, z limits/labels, shading, and colormaps. "
            "`show()` is the display call: inline PNG on graphics terminals, ASCII otherwise. "
            "`savefig` writes PNG or txt/asc ASCII. Not supported: "
            "animation, full rcParams theming, undocumented plot types.")
       :shim/bindings mpl-bridge-bindings
       :shim/source "vis-shims/matplotlib.py"}]}))

(defn register! [] (vis/register-extension! vis-extension))
