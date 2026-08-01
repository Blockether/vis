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

;; ----------------------------------------------------------------------------
;; Host renderer — `com.blockether/imaging` vector ops. Input is the pyplot
;; figure spec (string-keyed map marshalled from the sandbox); output is a
;; base64 PNG string. Every drawing primitive is an op map handed to
;; `imaging/draw!` in ONE batch per figure, which keeps FFI chatter out of the
;; plotting loops.
;; ----------------------------------------------------------------------------

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
  (let
    [cs (when (and (string? c) (seq c)) (.toLowerCase ^String (str c)))
     ;; matplotlib spells the same colour "tab:blue", "xkcd:blue" or "blue";
     ;; "C0".."C9" is the property cycle, and a bare "0.4" is a grey level.
     cs (when cs
          (cond (.startsWith ^String cs "tab:") (subs cs 4)
                (.startsWith ^String cs "xkcd:") (subs cs 5)
                :else cs))
     grey (when (and cs (re-matches #"0?\.[0-9]+|[01](\.0*)?" cs))
            (let [v (long (Math/round (* 255.0 (Double/parseDouble cs))))]
              [v v v]))
     cyc (when (and cs (re-matches #"c[0-9]" cs))
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

(defn- dash-style
  "Stroke style keys for a matplotlib `linestyle`, merged into a draw op."
  [linestyle width]
  (case (str linestyle)
    "--" {:stroke-width width :cap :butt :join :round :dash [9.0 6.0]}
    ":" {:stroke-width width :cap :round :join :round :dash [2.0 5.0]}
    "-." {:stroke-width width :cap :butt :join :round :dash [10.0 5.0 2.0 5.0]}
    {:stroke-width width :cap :round :join :round}))

(defn- text-width
  "Advance width of `s` at `size` px in the shim's font (the `FontMetrics`
   replacement used for centring and legend boxes)."
  ^double [^String s ^double size weight]
  (double (or (:width (im/text-measure {:text s :size size :family sans :weight weight})) 0.0)))


(defn- png-base64
  ^String [img]
  (.encodeToString (Base64/getEncoder) ^bytes (im/encode img :png)))

(defn- lerp-col
  [[r1 g1 b1] [r2 g2 b2] ^double t]
  [(+ (double r1) (* (- (double r2) (double r1)) t))
   (+ (double g1) (* (- (double g2) (double g1)) t))
   (+ (double b1) (* (- (double b2) (double b1)) t))])

(defn- viridis
  [^double t]
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

    [(int a) (int b) (int c)]))

(defn- title-ops
  "Draw ops for the figure title, centred over the plot area (empty when none)."
  [title ^long px0 ^long pw]
  (if (and (string? title) (seq title))
    [{:op :text :text title :x (+ px0 (quot pw 2)) :y 18 :anchor :middle
      :size 14 :weight 700 :family sans :fill [30 30 30]}]
    []))

(defn- render-pie
  "Full-canvas pie chart (ignores axes). `s` carries sizes in `x` and optional
   `labels`. Wedge angles are SCREEN degrees — clockwise from 3 o'clock — so the
   first slice starts at 12 o'clock (-90) and each one advances clockwise."
  ^String [^long W ^long H spec s]
  (let
    [vals (mapv #(Math/abs (as-double %)) (get s "x"))
     labels (get s "labels")
     total (double (reduce + 0.0 vals))
     cx (/ W 2.0)
     cy (+ 12.0 (/ H 2.0))
     r (double (- (quot (long (min W H)) 2) 66))
     img (im/blank W H "white")
     fracs (mapv (fn [v]
                   (if (< 0.0 total) (/ (double v) total) 0.0))
                 vals)
     ops (reduce
           (fn [acc i]
             (let
               [frac (double (nth fracs i))
                ang (* 360.0 frac)
                a0 (- (* 360.0 (double (reduce + 0.0 (take i fracs)))) 90.0)
                mid (Math/toRadians (+ a0 (/ ang 2.0)))
                lx (+ cx (* (+ r 16) (Math/cos mid)))
                ly (+ cy (* (+ r 16) (Math/sin mid)))
                lbl (if (and labels (< i (count labels)))
                      (str (nth labels i))
                      (str (Math/round (* 100.0 frac)) "%"))]

               (conj acc
                     {:op :wedge :cx cx :cy cy :r r :start a0 :end (+ a0 ang)
                      :fill (->rgb nil i)}
                     {:op :text :text lbl :x lx :y ly :size 11 :family sans :fill [40 40 40]
                      :anchor (if (neg? (Math/cos mid)) :end :start)})))
           []
           (range (count vals)))]

    (im/draw! img (into ops (title-ops (get spec "title") 0 W)))
    (png-base64 img)))

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
              (add! {:op :line :x1 px0 :y1 yp :x2 (+ px0 pw) :y2 yp
                     :stroke grid-color :stroke-width 1}))
            (add! {:op :text :text (str (nth ytick-strs i)) :x (- px0 6) :y (+ yp 4)
                   :anchor :end :size 10 :family sans :fill tick-color}))))
      (if (and cat-labels (not xticks-spec))
        (dotimes [i (count cat-labels)]
          (let [xl (str (nth cat-labels i))
                xp (rnd (sx (double i)))]
            (when grid?
              (add! {:op :line :x1 xp :y1 py0 :x2 xp :y2 (+ py0 ph)
                     :stroke grid-color :stroke-width 1}))
            (add! {:op :text :text xl :x xp :y (+ py0 ph 16)
                   :anchor :middle :size 10 :family sans :fill tick-color})))
        (dotimes [i (count xticks)]
          (let [xv (double (nth xticks i))
                xp (rnd (sxf xv))]
            (when (and (<= (double xmin) xv) (<= xv (double xmax)))
              (when grid?
                (add! {:op :line :x1 xp :y1 py0 :x2 xp :y2 (+ py0 ph)
                       :stroke grid-color :stroke-width 1}))
              (add! {:op :text :text (str (nth xtick-strs i)) :x xp :y (+ py0 ph 16)
                     :anchor :middle :size 10 :family sans :fill tick-color})))))
      ;; axes frame
      (add! {:op :rect :x px0 :y py0 :w pw :h ph :stroke [60 60 60] :stroke-width 1}))
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
           col (->rgb (get s "color") idx)
           pts (map vector xs ys)]

          (case kind
            "scatter"
            (doseq [[x y] pts]
              (add! {:op :circle :cx (sx x) :cy (sy y) :r 3 :fill col}))

            "bar"
            (let
              [bw (long (max 2 (int (* (quot pw bar-slots) (/ 0.7 (long (max 1 nbar)))))))
               y0 (rnd (syf (max ymin (min ymax 0.0))))]

              (doseq [[x y] pts]
                (let
                  [yp (rnd (sy y))
                   top (min y0 yp)
                   hgt (Math/abs (- y0 yp))]

                  (add! {:op :rect :x (- (rnd (sx x)) (quot bw 2)) :y top
                         :w bw :h (max 1 hgt) :fill col}))))

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
            (let
              [y2 (mapv as-double (get s "y2"))
               n (count xs)]

              (when (and (pos? n) (= n (count y2)))
                (add! {:op :polygon
                       :points (vec (concat (map (fn [x y] [(sx x) (sy y)]) xs ys)
                                            (map (fn [x y] [(sx x) (sy y)])
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
                       cell (viridis (/ (- v vmin) span))
                       x0 (rnd (sx ci))
                       x1 (rnd (sx (inc ci)))
                       yt (rnd (sy (- nr ri)))
                       yb (rnd (sy (- nr (inc ri))))]

                      (add! {:op :rect :x (min x0 x1) :y (min yt yb)
                             :w (max 1 (Math/abs (- x1 x0))) :h (max 1 (Math/abs (- yb yt)))
                             :fill cell}))))))

            "box"
            (let
              [stats (get s "stats")
               pos (mapv as-double (get s "positions"))
               bw 24]

              (doseq [[bi st] (map-indexed vector stats)]
                (let
                  [xc (rnd (sx (nth pos bi (inc (long bi)))))
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
                        {:op :rect :x (- xc hw) :y (min q1 q3) :w bw
                         :h (max 1 (Math/abs (- q3 q1))) :fill bcol}
                        {:op :rect :x (- xc hw) :y (min q1 q3) :w bw
                         :h (max 1 (Math/abs (- q3 q1))) :stroke [30 30 30] :stroke-width 1}
                        (merge {:op :line :x1 (- xc hw) :y1 q2 :x2 (+ xc hw) :y2 q2}
                               {:stroke [30 30 30] :stroke-width 1.5})))))

            ;; default: line (+ optional markers)
            (do (when (seq pts)
                  (add! (merge {:op :polyline
                                :points (mapv (fn [[x y]] [(sx x) (sy y)]) pts)
                                :stroke col}
                               (dash-style (get s "linestyle") 2.0))))
                (when (seq (str (get s "marker")))
                  (doseq [[x y] pts]
                    (add! {:op :circle :cx (sx x) :cy (sy y) :r 3 :fill col}))))))))
    ;; title / axis labels
    (vswap! ops into (title-ops title px0 pw))
    (when (and (string? xlabel) (seq xlabel))
      (add! {:op :text :text xlabel :x (+ px0 (quot pw 2)) :y (- H 12)
             :anchor :middle :size 12 :family sans :fill [30 30 30]}))
    (when (and (string? ylabel) (seq ylabel))
      (add! {:op :text :text ylabel :x 16 :y (+ py0 (quot ph 2)) :rotate -90
             :anchor :middle :size 12 :family sans :fill [30 30 30]}))
    ;; text annotations (data coords)
    (when (seq annotations)
      (doseq [a annotations]
        (add! {:op :text :text (str (get a "text"))
               :x (sx (as-double (get a "x"))) :y (sy (as-double (get a "y")))
               :size 11 :family sans :fill [20 20 20]})))
    ;; legend
    (let
      [labelled (filter #(let [l (get % "label")]

                           (and (string? l) (seq l)))
                        (map-indexed (fn [i s]
                                       (assoc s "__idx" i))
                                     series))]
      (when (seq labelled)
        (let
          [rows (vec labelled)
           lw (+ 34 (long (Math/ceil (double (reduce max 0.0
                                                     (map #(text-width (str (get % "label")) 11.0 400)
                                                          rows))))))
           lh (+ 8 (* 16 (count rows)))
           lx (- (+ px0 pw) lw 8)
           ly (+ py0 8)]

          (add! {:op :rect :x lx :y ly :w lw :h lh :fill [255 255 255]}
                {:op :rect :x lx :y ly :w lw :h lh :stroke [180 180 180] :stroke-width 1})
          (doseq [[ri s] (map-indexed vector rows)]
            (let [yy (+ ly 8 (* (long ri) 16))]
              (add! {:op :rect :x (+ lx 8) :y (+ yy 3) :w 16 :h 6
                     :fill (->rgb (get s "color") (get s "__idx"))}
                    {:op :text :text (str (get s "label")) :x (+ lx 30) :y (+ yy 11)
                     :size 11 :family sans :fill [40 40 40]}))))))
    (im/draw! img @ops)
    (png-base64 img)))

(def ^:private cmaps
  "Colormap anchor stops by matplotlib name, sampled by `ramp`. Used by the 3-D
   renderer for surface facets and per-point scatter colours; an unknown name
   falls back to viridis."
  {"viridis" [[68 1 84] [59 82 139] [33 145 140] [94 201 98] [253 231 37]]
   "plasma" [[13 8 135] [126 3 168] [204 71 120] [248 149 64] [240 249 33]]
   "inferno" [[0 0 4] [87 16 110] [188 55 84] [249 142 9] [252 255 164]]
   "magma" [[0 0 4] [81 18 124] [183 55 121] [252 137 97] [252 253 191]]
   "cividis" [[0 32 76] [59 74 108] [110 112 106] [166 155 84] [255 233 69]]
   "coolwarm" [[59 76 192] [141 176 254] [221 221 221] [244 154 123] [180 4 38]]
   "bwr" [[0 0 255] [255 255 255] [255 0 0]]
   "seismic" [[0 0 76] [0 0 255] [255 255 255] [255 0 0] [128 0 0]]
   "jet" [[0 0 131] [0 128 255] [124 252 0] [255 165 0] [128 0 0]]
   "turbo" [[48 18 59] [26 175 213] [126 246 92] [250 168 42] [122 4 3]]
   "rainbow" [[128 0 255] [0 128 255] [0 255 128] [255 255 0] [255 0 0]]
   "hot" [[10 0 0] [178 0 0] [255 111 0] [255 234 0] [255 255 255]]
   "afmhot" [[0 0 0] [128 0 0] [255 128 0] [255 255 128] [255 255 255]]
   "bone" [[0 0 0] [84 84 116] [166 198 198] [255 255 255]]
   "copper" [[0 0 0] [255 160 102] [255 199 127]]
   "spring" [[255 0 255] [255 255 0]]
   "summer" [[0 128 102] [255 255 102]]
   "autumn" [[255 0 0] [255 255 0]]
   "winter" [[0 0 255] [0 255 128]]
   "gray" [[0 0 0] [255 255 255]]
   "grey" [[0 0 0] [255 255 255]]
   "greys" [[255 255 255] [0 0 0]]
   "binary" [[255 255 255] [0 0 0]]
   "ocean" [[0 128 0] [0 20 128] [200 255 255]]
   "terrain" [[51 51 153] [0 166 166] [242 242 130] [128 92 46] [255 255 255]]
   "cubehelix" [[0 0 0] [30 85 60] [160 90 140] [200 190 130] [255 255 255]]})

(defn- ramp
  "Sample a `cmaps` stop list at `t` in [0 1] and return an [r g b] vector."
  [stops ^double t]
  (let [t (double (max 0.0 (min 1.0 t)))
        n (long (max 1 (dec (count stops))))
        f (* t (double n))
        i (int (min (long f) (dec n)))
        [r g b] (lerp-col (nth stops i) (nth stops (inc i)) (- f (double i)))]
    [(int r) (int g) (int b)]))

(defn- shade-col
  "Scale an [r g b] by a lighting factor, clamped to the byte range."
  [rgb ^double f]
  (mapv (fn [v] (int (max 0.0 (min 255.0 (* (as-double v) f))))) rgb))

(defn- dot3
  "Dot product of the 3-vector `v` with the primitive triple (x, y, z)."
  ^double [v ^double x ^double y ^double z]
  (+ (* (as-double (nth v 0)) x)
     (* (as-double (nth v 1)) y)
     (* (as-double (nth v 2)) z)))

(defn- axis-range3
  "Autoscaled [lo hi] for one 3-D axis: an explicit `lim` pair wins per end, a
   degenerate or empty range is padded so the projection never divides by zero."
  [vs lim]
  (let [vs (filterv (fn [^double v]
                      (and (not (Double/isNaN v)) (not (Double/isInfinite v))))
                    (map as-double vs))
        lo (double (if (seq vs) (reduce min vs) 0.0))
        hi (double (if (seq vs) (reduce max vs) 1.0))
        lo (double (if (and (sequential? lim) (some? (first lim))) (as-double (first lim)) lo))
        hi (double (if (and (sequential? lim) (some? (second lim))) (as-double (second lim)) hi))]
    (if (< (Math/abs (- hi lo)) 1.0e-12) [(- lo 0.5) (+ hi 0.5)] [lo hi])))

(defn- grid-rows
  "A 2-D series field (`X`/`Y`/`Z`) as a vector of double rows."
  [s k]
  (mapv (fn [r] (mapv as-double r)) (get s k)))

(defn- series-3d-points
  "Every [x y z] a 3-D series occupies — used only for autoscaling the cube."
  [s]
  (let [kind (str (get s "kind"))]
    (case kind
      ("surface3d" "wire3d")
      (let [xg (grid-rows s "X")
            yg (grid-rows s "Y")
            zg (grid-rows s "Z")]
        (for [i (range (count zg))
              j (range (count (nth zg i)))]
          [(get-in xg [i j] 0.0) (get-in yg [i j] 0.0) (get-in zg [i j] 0.0)]))

      "bar3d"
      (let [xs (series-xs s)
            ys (series-ys s)
            zs (mapv as-double (get s "z"))
            dxs (mapv as-double (get s "dx"))
            dys (mapv as-double (get s "dy"))
            dzs (mapv as-double (get s "dz"))]
        (mapcat (fn [i]
                  (let [x (double (nth xs i 0.0))
                        y (double (nth ys i 0.0))
                        z (double (nth zs i 0.0))]
                    [[x y z]
                     [(+ x (double (nth dxs i 0.0)))
                      (+ y (double (nth dys i 0.0)))
                      (+ z (double (nth dzs i 0.0)))]]))
                (range (count xs))))

      "seg3d"
      (mapcat (fn [seg]
                (let [v (mapv as-double seg)]
                  (if (<= 6 (count v))
                    [[(nth v 0) (nth v 1) (nth v 2)] [(nth v 3) (nth v 4) (nth v 5)]]
                    [])))
              (get s "segs"))

      (map vector (series-xs s) (series-ys s) (mapv as-double (get s "z"))))))

(defn- render-3d
  "Axes3D figure: a painter's-algorithm 3-D renderer for `surface3d`, `wire3d`,
   `line3d`, `scatter3d` and `bar3d` series.

   The data is normalised into the unit cube, rotated by the spec's `elev`/`azim`
   (matplotlib's own defaults, 30 and -60), projected orthographically onto the
   camera's right/up basis and fitted to the canvas by the projected extent of
   the cube's eight corners. Every facet, segment and marker carries the depth of
   its centroid along the view axis and the whole batch is drawn far-to-near, so
   geometry occludes correctly without a z-buffer. Surface facets are additionally
   shaded by the angle between their normal and the light, which is what makes a
   flat colormap read as a solid surface. Panes, gridlines and tick labels sit on
   the three BACK walls (chosen from the sign of the view vector), like matplotlib."
  ^String [^long W ^long H spec series]
  (let
    [title
     (get spec "title")

     xlabel
     (get spec "xlabel")

     ylabel
     (get spec "ylabel")

     zlabel
     (get spec "zlabel")

     grid?
     (not (false? (get spec "grid")))

     axis-off?
     (boolean (get spec "axis_off"))

     elev
     (as-double (if (some? (get spec "elev")) (get spec "elev") 30.0))

     azim
     (as-double (if (some? (get spec "azim")) (get spec "azim") -60.0))

     ar
     (Math/toRadians azim)

     er
     (Math/toRadians elev)

     ca
     (Math/cos ar)

     sa
     (Math/sin ar)

     ce
     (Math/cos er)

     se
     (Math/sin er)

     ;; camera basis: `cam-v` points from the cube towards the eye, so a larger
     ;; dot product with it means nearer the viewer.
     cam-r
     [(- sa) ca 0.0]

     cam-u
     [(* (- ca) se) (* (- sa) se) ce]

     cam-v
     [(* ca ce) (* sa ce) se]

     ;; light: the camera direction tilted upward, so tops stay brightest
     light
     (let [l [(+ (double (nth cam-v 0)) (* 0.45 (double (nth cam-u 0))))
              (+ (double (nth cam-v 1)) (* 0.45 (double (nth cam-u 1))))
              (+ (double (nth cam-v 2)) (* 0.45 (double (nth cam-u 2))))]
           m (Math/sqrt (dot3 l (double (nth l 0)) (double (nth l 1)) (double (nth l 2))))]
       (mapv (fn [c] (/ (as-double c) (max 1.0e-9 m))) l))

     pts
     (vec (mapcat series-3d-points series))

     [xmin xmax]
     (axis-range3 (map #(nth % 0) pts) (get spec "xlim"))

     [ymin ymax]
     (axis-range3 (map #(nth % 1) pts) (get spec "ylim"))

     [zmin zmax]
     (axis-range3 (map #(nth % 2) pts) (get spec "zlim"))

     xspan
     (- (double xmax) (double xmin))

     yspan
     (- (double ymax) (double ymin))

     zspan
     (- (double zmax) (double zmin))

     nx
     (fn ^double [^double v] (- (/ (- v (double xmin)) xspan) 0.5))

     ny
     (fn ^double [^double v] (- (/ (- v (double ymin)) yspan) 0.5))

     nz
     (fn ^double [^double v] (- (/ (- v (double zmin)) zspan) 0.5))

     pad
     52

     top
     (if (and (string? title) (seq title)) 40 22)

     bot
     34

     ;; fit the projected cube corners into the canvas
     corners
     (for [x [-0.5 0.5] y [-0.5 0.5] z [-0.5 0.5]] [x y z])

     us
     (mapv (fn [[x y z]] (dot3 cam-r (double x) (double y) (double z))) corners)

     vs
     (mapv (fn [[x y z]] (dot3 cam-u (double x) (double y) (double z))) corners)

     umin
     (double (reduce min us))

     umax
     (double (reduce max us))

     vmin
     (double (reduce min vs))

     vmax
     (double (reduce max vs))

     avail-w
     (double (max 40 (- W (* 2 (long pad)))))

     avail-h
     (double (max 40 (- H (long top) (long bot))))

     sc
     (min (/ avail-w (max 1.0e-9 (- umax umin)))
          (/ avail-h (max 1.0e-9 (- vmax vmin))))

     ox
     (- (+ (double pad) (/ avail-w 2.0)) (* sc (/ (+ umin umax) 2.0)))

     oy
     (+ (double top) (/ avail-h 2.0) (* sc (/ (+ vmin vmax) 2.0)))

     ;; unit-cube point -> [px py depth]
     sp
     (fn [^double x ^double y ^double z]
       [(+ (double ox) (* (double sc) (dot3 cam-r x y z)))
        (- (double oy) (* (double sc) (dot3 cam-u x y z)))
        (dot3 cam-v x y z)])

     sp2
     (fn [^double x ^double y ^double z] (subvec (sp x y z) 0 2))

     ;; data point -> [px py depth]
     dp
     (fn [^double x ^double y ^double z] (sp (nx x) (ny y) (nz z)))

     ;; back walls: the side of each pair the camera is NOT on
     zfloor
     (if (pos? (double (nth cam-v 2))) -0.5 0.5)

     xback
     (if (pos? (double (nth cam-v 0))) -0.5 0.5)

     yback
     (if (pos? (double (nth cam-v 1))) -0.5 0.5)

     xfront
     (- (double xback))

     yfront
     (- (double yback))

     xticks
     (or (seq (mapv as-double (get spec "xticks"))) (nice-ticks xmin xmax 5))

     yticks
     (or (seq (mapv as-double (get spec "yticks"))) (nice-ticks ymin ymax 5))

     zticks
     (or (seq (mapv as-double (get spec "zticks"))) (nice-ticks zmin zmax 5))

     img
     (im/blank W H "white")

     bg
     (volatile! [])

     fg
     (volatile! [])

     ;; depth-sorted geometry: [depth [op ...]]
     prims
     (volatile! [])

     back!
     (fn [& os] (vswap! bg into (remove nil? os)))

     front!
     (fn [& os] (vswap! fg into (remove nil? os)))

     push!
     (fn [^double d os] (vswap! prims conj [d (vec (remove nil? os))]))

     pane-line
     (fn [p1 p2]
       (let [[x1 y1] (apply sp2 p1)
             [x2 y2] (apply sp2 p2)]
         {:op :line :x1 x1 :y1 y1 :x2 x2 :y2 y2 :stroke [255 255 255] :stroke-width 1.2}))

     edge-line
     (fn [p1 p2]
       (let [[x1 y1] (apply sp2 p1)
             [x2 y2] (apply sp2 p2)]
         {:op :line :x1 x1 :y1 y1 :x2 x2 :y2 y2 :stroke [176 176 184] :stroke-width 1.0}))]

    ;; ── panes (three back walls), their gridlines and their frames ──────────
    (when-not axis-off?
      (doseq [quad [[[-0.5 -0.5 zfloor] [0.5 -0.5 zfloor] [0.5 0.5 zfloor] [-0.5 0.5 zfloor]]
                    [[xback -0.5 -0.5] [xback 0.5 -0.5] [xback 0.5 0.5] [xback -0.5 0.5]]
                    [[-0.5 yback -0.5] [0.5 yback -0.5] [0.5 yback 0.5] [-0.5 yback 0.5]]]]
        (back! {:op :polygon
                :points (mapv (fn [p] (apply sp2 p)) quad)
                :close true
                :fill [244 244 249]}))
      (when grid?
        (doseq [t xticks]
          (let [v (nx (as-double t))]
            (back! (pane-line [v -0.5 zfloor] [v 0.5 zfloor])
                   (pane-line [v yback -0.5] [v yback 0.5]))))
        (doseq [t yticks]
          (let [v (ny (as-double t))]
            (back! (pane-line [-0.5 v zfloor] [0.5 v zfloor])
                   (pane-line [xback v -0.5] [xback v 0.5]))))
        (doseq [t zticks]
          (let [v (nz (as-double t))]
            (back! (pane-line [xback -0.5 v] [xback 0.5 v])
                   (pane-line [-0.5 yback v] [0.5 yback v])))))
      ;; the three cube edges that meet at the back corner + the floor outline
      (back! (edge-line [xback yback -0.5] [xback yback 0.5])
             (edge-line [-0.5 -0.5 zfloor] [0.5 -0.5 zfloor])
             (edge-line [0.5 -0.5 zfloor] [0.5 0.5 zfloor])
             (edge-line [0.5 0.5 zfloor] [-0.5 0.5 zfloor])
             (edge-line [-0.5 0.5 zfloor] [-0.5 -0.5 zfloor])))
    ;; ── series geometry ─────────────────────────────────────────────────────
    (doseq [[idx s] (map-indexed vector series)]
      (let
        [kind (str (get s "kind"))
         col (->rgb (get s "color") idx)
         cmap-name (when (get s "cmap") (.toLowerCase (str (get s "cmap"))))
         stops (when cmap-name (get cmaps cmap-name (get cmaps "viridis")))]

        (case kind
          "surface3d"
          (let
            [xg (grid-rows s "X")
             yg (grid-rows s "Y")
             zg (grid-rows s "Z")
             nr (count zg)
             nc (long (reduce max 0 (map count zg)))
             stops (or stops (when-not (get s "color") (get cmaps "viridis")))
             edges? (not (false? (get s "edges")))
             q (fn [i j]
                 [(nx (double (get-in xg [i j] 0.0)))
                  (ny (double (get-in yg [i j] 0.0)))
                  (nz (double (get-in zg [i j] 0.0)))])]

            (dotimes [i (max 0 (dec nr))]
              (dotimes [j (max 0 (dec nc))]
                (let
                  [c00 (q i j)
                   c10 (q (inc i) j)
                   c11 (q (inc i) (inc j))
                   c01 (q i (inc j))
                   quad [c00 c10 c11 c01]
                   scr (mapv (fn [[x y z]] (sp (double x) (double y) (double z))) quad)
                   depth (/ (double (reduce + 0.0 (map #(double (nth % 2)) scr))) 4.0)
                   ;; face normal from the two cell edges (unit-cube space)
                   ax (- (double (nth c10 0)) (double (nth c00 0)))
                   ay (- (double (nth c10 1)) (double (nth c00 1)))
                   az (- (double (nth c10 2)) (double (nth c00 2)))
                   bx (- (double (nth c01 0)) (double (nth c00 0)))
                   by (- (double (nth c01 1)) (double (nth c00 1)))
                   bz (- (double (nth c01 2)) (double (nth c00 2)))
                   nxv (- (* ay bz) (* az by))
                   nyv (- (* az bx) (* ax bz))
                   nzv (- (* ax by) (* ay bx))
                   nlen (Math/sqrt (+ (* nxv nxv) (* nyv nyv) (* nzv nzv)))
                   lam (if (< nlen 1.0e-12)
                         1.0
                         (Math/abs (/ (dot3 light nxv nyv nzv) nlen)))
                   lit (+ 0.62 (* 0.42 lam))
                   zavg (/ (+ (double (get-in zg [i j] 0.0))
                              (double (get-in zg [(inc i) j] 0.0))
                              (double (get-in zg [(inc i) (inc j)] 0.0))
                              (double (get-in zg [i (inc j)] 0.0)))
                           4.0)
                   base (if stops
                          (ramp stops (/ (- zavg (double zmin)) zspan))
                          col)
                   face (shade-col base lit)
                   pts2 (mapv (fn [p] (subvec p 0 2)) scr)]

                  (push! depth
                         [{:op :polygon :points pts2 :close true :fill face}
                          (when edges?
                            {:op :polygon :points pts2 :close true
                             :stroke (shade-col base (* lit 0.82)) :stroke-width 0.8})])))))

          "wire3d"
          (let
            [xg (grid-rows s "X")
             yg (grid-rows s "Y")
             zg (grid-rows s "Z")
             nr (count zg)
             nc (long (reduce max 0 (map count zg)))
             q (fn [i j]
                 (sp (nx (double (get-in xg [i j] 0.0)))
                     (ny (double (get-in yg [i j] 0.0)))
                     (nz (double (get-in zg [i j] 0.0)))))
             strand (fn [ps]
                      (when (< 1 (count ps))
                        (push! (/ (double (reduce + 0.0 (map #(double (nth % 2)) ps)))
                                  (double (count ps)))
                               [{:op :polyline
                                 :points (mapv (fn [p] (subvec p 0 2)) ps)
                                 :stroke col :stroke-width 1.1 :cap :round :join :round}])))]

            (dotimes [i nr] (strand (mapv (fn [j] (q i j)) (range nc))))
            (dotimes [j nc] (strand (mapv (fn [i] (q i j)) (range nr)))))

          "scatter3d"
          (let
            [xs (series-xs s)
             ys (series-ys s)
             zs (mapv as-double (get s "z"))
             cvals (mapv as-double (get s "c"))
             cmin (double (if (seq cvals) (reduce min cvals) 0.0))
             cmax (double (if (seq cvals) (reduce max cvals) 1.0))
             cspan (let [d (- cmax cmin)] (if (< (Math/abs d) 1.0e-12) 1.0 d))
             sizes (mapv as-double (get s "sizes"))
             dflt-size (as-double (or (get s "size") 20.0))]

            (dotimes [i (min (count xs) (count ys) (count zs))]
              (let
                [[px py d] (dp (double (nth xs i)) (double (nth ys i)) (double (nth zs i)))
                 c (if (and stops (seq cvals))
                     (ramp stops (/ (- (double (nth cvals i 0.0)) cmin) cspan))
                     col)
                 sz (double (nth sizes i dflt-size))
                 r (max 1.5 (Math/sqrt (/ (max 1.0 sz) 3.14159)))]

                (push! d
                       [{:op :circle :cx px :cy py :r r :fill c}
                        {:op :circle :cx px :cy py :r r
                         :stroke (shade-col c 0.55) :stroke-width 0.8}]))))

          "bar3d"
          (let
            [xs (series-xs s)
             ys (series-ys s)
             zs (mapv as-double (get s "z"))
             dxs (mapv as-double (get s "dx"))
             dys (mapv as-double (get s "dy"))
             dzs (mapv as-double (get s "dz"))
             colors (get s "colors")]

            (dotimes [i (count xs)]
              (let
                [x0 (double (nth xs i 0.0))
                 y0 (double (nth ys i 0.0))
                 z0 (double (nth zs i 0.0))
                 x1 (+ x0 (double (nth dxs i 1.0)))
                 y1 (+ y0 (double (nth dys i 1.0)))
                 z1 (+ z0 (double (nth dzs i 1.0)))
                 bcol (if (and colors (< i (count colors)))
                        (->rgb (nth colors i) idx)
                        col)
                 v (fn [^long k]
                     (let [x (if (zero? (bit-and k 1)) x0 x1)
                           y (if (zero? (bit-and k 2)) y0 y1)
                           z (if (zero? (bit-and k 4)) z0 z1)]
                       (dp (double x) (double y) (double z))))
                 ;; the six box faces as corner-index quads (bit 1 = x, 2 = y, 4 = z)
                 faces [[0 1 3 2] [4 5 7 6] [0 1 5 4] [2 3 7 6] [0 2 6 4] [1 3 7 5]]
                 shades [0.72 1.0 0.86 0.8 0.9 0.78]]

                (doseq [[fi f] (map-indexed vector faces)]
                  (let [ps (mapv v f)
                        depth (/ (double (reduce + 0.0 (map #(double (nth % 2)) ps))) 4.0)
                        pts2 (mapv (fn [p] (subvec p 0 2)) ps)
                        fcol (shade-col bcol (double (nth shades fi)))]
                    (push! depth
                           [{:op :polygon :points pts2 :close true :fill fcol}
                            {:op :polygon :points pts2 :close true
                             :stroke (shade-col fcol 0.7) :stroke-width 0.8}]))))))

          ;; `contour`/`contour3D`: marching-squares segments, each already carrying
          ;; its own level (or the `offset=` plane) as z
          "seg3d"
          (doseq [seg (get s "segs")]
            (let [v (mapv as-double seg)]
              (when (<= 6 (count v))
                (let [a (dp (double (nth v 0)) (double (nth v 1)) (double (nth v 2)))
                      b (dp (double (nth v 3)) (double (nth v 4)) (double (nth v 5)))]
                  (push! (/ (+ (double (nth a 2)) (double (nth b 2))) 2.0)
                         [{:op :line :x1 (nth a 0) :y1 (nth a 1)
                           :x2 (nth b 0) :y2 (nth b 1)
                           :stroke col :stroke-width 1.2 :cap :round}])))))

          ;; default: a 3-D polyline (`plot3D`), optionally with markers
          (let
            [xs (series-xs s)
             ys (series-ys s)
             zs (mapv as-double (get s "z"))
             ps (mapv (fn [i] (dp (double (nth xs i)) (double (nth ys i)) (double (nth zs i))))
                      (range (min (count xs) (count ys) (count zs))))]

            (when (< 1 (count ps))
              (push! (/ (double (reduce + 0.0 (map #(double (nth % 2)) ps)))
                        (double (count ps)))
                     [(merge {:op :polyline
                              :points (mapv (fn [p] (subvec p 0 2)) ps)
                              :stroke col}
                             (dash-style (get s "linestyle") 2.0))]))
            (when (seq (str (get s "marker")))
              (doseq [p ps]
                (push! (double (nth p 2))
                       [{:op :circle :cx (nth p 0) :cy (nth p 1) :r 3 :fill col}])))))))
    ;; ── tick labels, axis labels, title, annotations, legend ────────────────
    (when-not axis-off?
      (doseq [t xticks]
        (let [[px py] (sp2 (nx (as-double t)) (* 1.16 (double yfront)) (double zfloor))]
          (front! {:op :text :text (fmt-num (as-double t)) :x px :y (+ (double py) 4.0)
                   :anchor :middle :size 9 :family sans :fill tick-color})))
      (doseq [t yticks]
        (let [[px py] (sp2 (* 1.16 (double xfront)) (ny (as-double t)) (double zfloor))]
          (front! {:op :text :text (fmt-num (as-double t)) :x px :y (+ (double py) 4.0)
                   :anchor :middle :size 9 :family sans :fill tick-color})))
      (let
        [;; z ticks belong on whichever back-corner vertical edge projects leftmost
         cand [[(double xback) (* 1.1 (double yfront))] [(* 1.1 (double xfront)) (double yback)]]
         [zx zy] (first (sort-by (fn [[a b]] (nth (sp2 (double a) (double b) 0.0) 0)) cand))]

        (doseq [t zticks]
          (let [[px py] (sp2 (double zx) (double zy) (nz (as-double t)))]
            (front! {:op :text :text (fmt-num (as-double t)) :x (- (double px) 6.0) :y (+ (double py) 4.0)
                     :anchor :end :size 9 :family sans :fill tick-color})))
        (when (and (string? zlabel) (seq zlabel))
          (let [[px py] (sp2 (double zx) (double zy) 0.0)]
            (front! {:op :text :text zlabel :x (- (double px) 34.0) :y py :rotate -90
                     :anchor :middle :size 11 :family sans :fill [30 30 30]}))))
      (when (and (string? xlabel) (seq xlabel))
        (let [[px py] (sp2 0.0 (* 1.42 (double yfront)) (double zfloor))]
          (front! {:op :text :text xlabel :x px :y (+ (double py) 10.0)
                   :anchor :middle :size 11 :family sans :fill [30 30 30]})))
      (when (and (string? ylabel) (seq ylabel))
        (let [[px py] (sp2 (* 1.42 (double xfront)) 0.0 (double zfloor))]
          (front! {:op :text :text ylabel :x px :y (+ (double py) 10.0)
                   :anchor :middle :size 11 :family sans :fill [30 30 30]}))))
    (doseq [a (get spec "annotations")]
      (let [[px py] (dp (as-double (get a "x")) (as-double (get a "y")) (as-double (get a "z")))]
        (front! {:op :text :text (str (get a "text")) :x px :y py
                 :size 11 :family sans :fill [20 20 20]})))
    (vswap! fg into (title-ops title 0 W))
    (let
      [labelled (filter #(let [l (get % "label")] (and (string? l) (seq l)))
                        (map-indexed (fn [i s] (assoc s "__idx" i)) series))]

      (when (seq labelled)
        (let
          [rows (vec labelled)
           lw (+ 34 (long (Math/ceil (double (reduce max 0.0
                                                     (map #(text-width (str (get % "label")) 11.0 400)
                                                          rows))))))
           lh (+ 8 (* 16 (count rows)))
           lx (- W lw 10)
           ly 10]

          (front! {:op :rect :x lx :y ly :w lw :h lh :fill [255 255 255]}
                  {:op :rect :x lx :y ly :w lw :h lh :stroke [180 180 180] :stroke-width 1})
          (doseq [[ri s] (map-indexed vector rows)]
            (let [yy (+ ly 8 (* (long ri) 16))]
              (front! {:op :rect :x (+ lx 8) :y (+ yy 3) :w 16 :h 6
                       :fill (->rgb (get s "color") (get s "__idx"))}
                      {:op :text :text (str (get s "label")) :x (+ lx 30) :y (+ yy 11)
                       :size 11 :family sans :fill [40 40 40]}))))))
    (im/draw! img (vec (concat @bg
                               (mapcat second (sort-by first @prims))
                               @fg)))
    (png-base64 img)))

(defn- render-png-base64
  "Render the figure `spec` (string-keyed map) to a PNG and return it base64.
   Dispatches to the 3-D renderer for an Axes3D figure, to a pie chart when a pie
   series is present, else the XY renderer.
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

    (cond (= "3d" (str (get spec "projection"))) (render-3d W H spec series)
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
     "Sandbox shim: a matplotlib.pyplot subset (plot/scatter/bar/barh/hist/fill_between/step/pie/boxplot/imshow/hlines/vlines/axhline/axvline + the OO Figure/Axes API with subplots, add_subplot, savefig, suptitle, tight_layout, set_size_inches, twinx; multi-pair plot with Line2D-like handles; axis('off'|[x0,x1,y0,y1]); log scales, markers, dashed styles, hex + named colors, viridis heatmaps, title/labels/grid/legend/text) plus real 3-D axes via mpl_toolkits.mplot3d / projection='3d' (plot_surface, plot_wireframe, contour with offset, 3-D scatter/plot/text, bar3d, view_init/set_zlim/set_zlabel), rendered by a painter's-algorithm camera with shading and colormaps. TWO renderers: a native `imaging` PNG backend and a pure-Python ASCII backend that also projects 3-D into braille. plt.show() is the ONE display call: it renders the figure to a PNG and paints it INLINE in a graphics-capable terminal (Kitty/iTerm2, e.g. Ghostty), automatically falling back to an ASCII plot on text-only terminals; savefig writes a PNG (or ASCII for a '*.txt'/'*.asc'/format='txt' target, honoring width/height/color kwargs). No pip, no native wheel."
     :ext/version "0.5.0"
     :ext/author "Blockether"
     :ext/owner "vis"
     :ext/license "Apache-2.0"
     :ext/kind "foundation"
     :ext/sandbox-shims
     [{:shim/name "matplotlib"
       :shim/imports ["matplotlib" "mpl_toolkits"]
       :shim/description
       "matplotlib.pyplot subset (line/scatter/bar/hist/fill/step/pie/box/image + OO Figure/Axes) with native imaging PNG and ASCII renderers. 3-D via mpl_toolkits.mplot3d/projection='3d': plot_surface, plot_wireframe, contour(offset=), scatter/plot/text in 3-D, bar3d, view_init/set_zlim/set_zlabel - depth-sorted, shaded, colormapped. plt.show() is the ONE display call - it paints the real PNG INLINE on a graphics terminal (Kitty/iTerm2) and automatically falls back to ASCII on text-only terminals; savefig writes PNG (or *.txt/*.asc/format='txt' ASCII, honoring width/height/color kwargs). Not supported: animation, full rcParams theming; only documented plot types render."
       :shim/bindings mpl-bridge-bindings
       :shim/source "vis-shims/matplotlib.py"}]}))

(vis/register-extension! vis-extension)
