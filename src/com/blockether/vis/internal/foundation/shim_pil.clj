(ns com.blockether.vis.internal.foundation.shim-pil
  "Built-in sandbox SHIM: a Pillow (PIL)-compatible `PIL` package for the model's
   Python sandbox, backed by the JVM's Java2D / ImageIO image stack. No CPython
   Pillow wheel ships in the sandbox; this extension contributes a
   `:ext/sandbox-shims` entry that `env-python/build-agent-context` installs into
   every sandbox Context (main + every `sub_loop` fork): the host bridge callables
   are wired onto the globals, then the Python preamble publishes a `PIL` package
   (with `Image`, `ImageDraw`, `ImageFilter`, `ImageOps`, `ImageColor`,
   `ImageEnhance`, `ImageChops`, `ImageFont`, `ImageMath` submodules) into
   `sys.modules` (so `from PIL import Image` works) and staples them onto builtins.

   Images live HOST-side as `BufferedImage`s in a per-JVM registry keyed by an
   integer handle; the Python `Image` object is a thin handle wrapper. All pixel
   ops, drawing, filtering, geometry and codec work happen on the JVM; only small
   metadata vectors and base64 blobs cross the strings-only boundary. Mirrors the
   `shim-matplotlib` Java2D approach and reuses `mpl-capture/record-attachment!`
   so `Image.show()` surfaces the image inline as a session attachment."
  (:require [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.internal.foundation.mpl-capture :as mpl-capture])
  (:import [java.awt AlphaComposite BasicStroke Color Font RenderingHints]
           [java.awt.image BufferedImage]
           [java.io ByteArrayInputStream ByteArrayOutputStream]
           [java.util Arrays Base64]
           [javax.imageio ImageIO]))

;; Java2D must run headless in a server JVM (no display, no Dock icon on macOS).
(System/setProperty "java.awt.headless" "true")

(System/setProperty "apple.awt.UIElement" "true")

;; ---------------------------------------------------------------------------
;; Host-side image registry: handle (long) -> {:img BufferedImage :mode String}.
;; The Python Image is just a handle; the pixels stay on the JVM.
;; ---------------------------------------------------------------------------

(defonce ^:private registry (atom {}))

(defonce ^:private counter (atom 0))

(defn- put-img!
  "Register `img` under mode string, returning its new integer handle."
  [^BufferedImage img mode]
  (let [h (swap! counter inc)]
    (swap! registry assoc h {:img img :mode mode})
    h))

(defn- entry [h] (get @registry (long h)))

(defn- free-img! [h] (swap! registry dissoc (long h)) nil)

;; ---------------------------------------------------------------------------
;; Pixel / colour helpers. Pixels are handled as packed 0xAARRGGBB ints via
;; getRGB/setRGB, which works uniformly across TYPE_INT_ARGB / RGB / BYTE_GRAY.
;; ---------------------------------------------------------------------------

(defn- ch ^long [^long p ^long sh] (bit-and (bit-shift-right p sh) 0xff))

(defn- argb
  ^long [a r g b]
  (bit-or (bit-shift-left (bit-and (long a) 0xff) 24)
          (bit-shift-left (bit-and (long r) 0xff) 16)
          (bit-shift-left (bit-and (long g) 0xff) 8)
          (bit-and (long b) 0xff)))

(defn- clamp255 ^long [^double v] (long (min 255 (max 0 (Math/round v)))))

(defn- gray-argb ^long [v] (argb 255 v v v))

(defn- mode->type
  ^long [mode]
  (case (str mode)
    ;; Grayscale-family modes are stored as TYPE_INT_RGB with the gray value
    ;; replicated across R/G/B, NOT TYPE_BYTE_GRAY: the latter uses a LINEAR
    ;; grayscale color space, so setRGB/getRGB would gamma-convert and an 'L'
    ;; pixel would not round-trip its sRGB byte value.
    ("1" "L" "I" "F" "P")
    BufferedImage/TYPE_INT_RGB

    ("RGBA" "LA")
    BufferedImage/TYPE_INT_ARGB

    BufferedImage/TYPE_INT_RGB))

(defn- new-buffered ^BufferedImage [mode w h] (BufferedImage. (int w) (int h) (mode->type mode)))

(defn- ->color
  ^Color [c mode]
  (cond (nil? c) (if (contains? #{"RGBA" "LA"} (str mode)) (Color. 0 0 0 0) Color/BLACK)
        (number? c) (let [v (int c)]
                      (Color. v v v))
        (sequential? c)
        (let
          [v
           (mapv int c)

           [r g b a]
           v]

          (case (count v)
            1
            (Color. (int r) (int r) (int r))

            (if a (Color. (int r) (int g) (int b) (int a)) (Color. (int r) (int g) (int b)))))
        :else Color/BLACK))

(defn- img->mode
  [^BufferedImage img]
  (cond (= (.getType img) BufferedImage/TYPE_BYTE_GRAY) "L"
        (.. img getColorModel hasAlpha) "RGBA"
        :else "RGB"))

(defn- meta-of
  [h]
  (let [{:keys [^BufferedImage img mode]} (entry h)]
    [(long h) (.getWidth img) (.getHeight img) mode]))

(defn- flatten-rgb
  ^BufferedImage [^BufferedImage src]
  (let
    [out
     (BufferedImage. (.getWidth src) (.getHeight src) BufferedImage/TYPE_INT_RGB)

     g
     (.createGraphics out)]

    (.setColor g Color/WHITE)
    (.fillRect g 0 0 (.getWidth src) (.getHeight src))
    (.drawImage g src 0 0 nil)
    (.dispose g)
    out))

;; ---------------------------------------------------------------------------
;; Core ops. Each returns a value the Python shim understands: a meta vector
;; [handle w h mode] for image-producing ops, else a scalar / base64 string.
;; ---------------------------------------------------------------------------

(defn- op-new
  [mode w h fill]
  (let
    [img
     (new-buffered mode w h)

     g
     (.createGraphics img)]

    (when (some? fill)
      (.setComposite g AlphaComposite/Src)
      (.setColor g (->color fill mode))
      (.fillRect g 0 0 (int w) (int h)))
    (.dispose g)
    (put-img! img (str mode))
    (meta-of @counter)))

(defn- op-open
  [b64]
  (let
    [bytes
     (.decode (Base64/getDecoder) ^String b64)

     img
     (ImageIO/read (ByteArrayInputStream. bytes))]

    (when (nil? img) (throw (ex-info "cannot identify image file" {})))
    (let
      [mode
       (img->mode img)

       h
       (put-img! img mode)]

      (meta-of h))))

(defn- op-save
  [h fmt]
  (let
    [{:keys [^BufferedImage img]}
     (entry h)

     fmt
     (str/lower-case (or fmt "png"))

     fmt
     (case fmt
       "jpg"
       "jpeg"

       fmt)

     img
     (if (and (#{"jpeg" "bmp"} fmt) (.. img getColorModel hasAlpha)) (flatten-rgb img) img)

     baos
     (ByteArrayOutputStream.)

     ok
     (ImageIO/write img fmt baos)]

    (when-not ok (throw (ex-info (str "no image writer for format " fmt) {})))
    (.encodeToString (Base64/getEncoder) (.toByteArray baos))))

(defn- op-save-temp
  [h fmt]
  (let
    [{:keys [^BufferedImage img]}
     (entry h)

     fmt
     (str/lower-case (or fmt "png"))

     norm
     (case fmt
       "jpg"
       "jpeg"

       fmt)

     b64
     (op-save h fmt)

     bytes
     (.decode (Base64/getDecoder) ^String b64)

     f
     (mpl-capture/display-cache-file "img-" fmt bytes)]

    (mpl-capture/record-attachment! {:kind "image"
                                     :media-type (str "image/" norm)
                                     :base64 b64
                                     :size (alength bytes)
                                     :filename (.getName f)
                                     :dims (str (.getWidth img) "x" (.getHeight img))})
    [(.getAbsolutePath f) (.getWidth img) (.getHeight img) (alength bytes)]))

(defn- op-copy
  [h]
  (let
    [{:keys [^BufferedImage img mode]}
     (entry h)

     out
     (new-buffered mode (.getWidth img) (.getHeight img))

     g
     (.createGraphics out)]

    (.setComposite g AlphaComposite/Src)
    (.drawImage g img 0 0 nil)
    (.dispose g)
    (meta-of (put-img! out mode))))

(defn- resample->hint
  [r]
  (case (int r)
    0
    RenderingHints/VALUE_INTERPOLATION_NEAREST_NEIGHBOR

    2
    RenderingHints/VALUE_INTERPOLATION_BILINEAR

    RenderingHints/VALUE_INTERPOLATION_BICUBIC))

(defn- op-resize
  [h w h2 resample]
  (let
    [{:keys [^BufferedImage img mode]}
     (entry h)

     out
     (new-buffered mode w h2)

     g
     (.createGraphics out)]

    (.setComposite g AlphaComposite/Src)
    (.setRenderingHint g RenderingHints/KEY_INTERPOLATION (resample->hint resample))
    (.drawImage g img 0 0 (int w) (int h2) nil)
    (.dispose g)
    (meta-of (put-img! out mode))))

(defn- op-crop
  [h l t r b]
  (let
    [{:keys [^BufferedImage img mode]}
     (entry h)

     w
     (max 1 (- (int r) (int l)))

     hh
     (max 1 (- (int b) (int t)))

     out
     (new-buffered mode w hh)

     g
     (.createGraphics out)]

    (.setComposite g AlphaComposite/Src)
    (.drawImage g img (- (int l)) (- (int t)) nil)
    (.dispose g)
    (meta-of (put-img! out mode))))

(defn- op-rotate
  [h angle expand fillc]
  (let
    [{:keys [^BufferedImage img mode]}
     (entry h)

     w
     (.getWidth img)

     hh
     (.getHeight img)

     rad
     (Math/toRadians (double angle))

     cos
     (Math/abs (Math/cos rad))

     sin
     (Math/abs (Math/sin rad))

     nw
     (if expand (long (Math/round (+ (* w cos) (* hh sin)))) w)

     nh
     (if expand (long (Math/round (+ (* w sin) (* hh cos)))) hh)

     out
     (new-buffered mode nw nh)

     g
     (.createGraphics out)]

    (when (some? fillc) (.setColor g (->color fillc mode)) (.fillRect g 0 0 (int nw) (int nh)))
    (.setRenderingHint g
                       RenderingHints/KEY_INTERPOLATION
                       RenderingHints/VALUE_INTERPOLATION_BILINEAR)
    (.translate g (/ (double nw) 2.0) (/ (double nh) 2.0))
    (.rotate g (- rad))
    (.translate g (/ (- w) 2.0) (/ (- hh) 2.0))
    (.drawImage g img 0 0 nil)
    (.dispose g)
    (meta-of (put-img! out mode))))

(defn- op-transpose
  [h method]
  (let
    [{:keys [^BufferedImage img mode]}
     (entry h)

     w
     (.getWidth img)

     hh
     (.getHeight img)

     m
     (int method)

     [nw nh]
     (case m
       (2 4 5 6)
       [hh w]

       [w hh])

     out
     (new-buffered mode nw nh)]

    (dotimes [y hh]
      (dotimes [x w]
        (let
          [p (.getRGB img x y)
           [nx ny] (case m
                     0
                     [(- w 1 x) y]

                     1
                     [x (- hh 1 y)]

                     3
                     [(- w 1 x) (- hh 1 y)]

                     2
                     [y (- w 1 x)]

                     4
                     [(- hh 1 y) x]

                     5
                     [y x]

                     6
                     [(- hh 1 y) (- w 1 x)]

                     [x y])]

          (.setRGB out (int nx) (int ny) p))))
    (meta-of (put-img! out mode))))

(defn- lum ^long [^long p] (clamp255 (+ (* 0.299 (ch p 16)) (* 0.587 (ch p 8)) (* 0.114 (ch p 0)))))

(defn- op-convert
  [h target]
  (let [{:keys [^BufferedImage img mode]} (entry h)]
    (if (= mode (str target))
      (op-copy h)
      (let
        [w (.getWidth img)
         hh (.getHeight img)
         target (str target)]

        (case target
          ;; sRGB-space luminance (Pillow's ITU-R 601-2), computed per pixel —
          ;; NOT Java2D's linear-space drawImage conversion.
          ("L" "I" "F" "P")
          (let [out (new-buffered "L" w hh)]
            (dotimes [y hh]
              (dotimes [x w]
                (.setRGB out x y (unchecked-int (gray-argb (lum (.getRGB img x y)))))))
            (meta-of (put-img! out target)))

          "1"
          (let [out (new-buffered "1" w hh)]
            (dotimes [y hh]
              (dotimes [x w]
                (let [v (if (>= (lum (.getRGB img x y)) 128) 255 0)]
                  (.setRGB out x y (unchecked-int (gray-argb v))))))
            (meta-of (put-img! out "1")))

          "LA"
          (let [out (new-buffered "LA" w hh)]
            (dotimes [y hh]
              (dotimes [x w]
                (let
                  [p (.getRGB img x y)
                   v (lum p)]

                  (.setRGB out x y (unchecked-int (argb (ch p 24) v v v))))))
            (meta-of (put-img! out "LA")))

          ;; RGB / RGBA: a straight channel copy (drawImage preserves sRGB).
          (let
            [out (new-buffered target w hh)
             g (.createGraphics out)]

            (.setComposite g AlphaComposite/Src)
            (.drawImage g img 0 0 nil)
            (.dispose g)
            (meta-of (put-img! out target))))))))

(defn- op-getpixel
  [h x y]
  (let
    [{:keys [^BufferedImage img mode]}
     (entry h)

     p
     (.getRGB img (int x) (int y))]

    (case (str mode)
      ("L" "1" "I" "F" "P")
      (ch p 16)

      ("RGBA" "LA")
      [(ch p 16) (ch p 8) (ch p 0) (ch p 24)]

      [(ch p 16) (ch p 8) (ch p 0)])))

(defn- color->argb
  ^long [c _mode]
  (cond (number? c) (gray-argb (int c))
        (sequential? c) (let
                          [v
                           (mapv int c)

                           [r g b a]
                           v]

                          (case (count v)
                            1
                            (gray-argb (int r))

                            2
                            (argb (int g) (int r) (int r) (int r))

                            3
                            (argb 255 (int r) (int g) (int b))

                            (argb (int a) (int r) (int g) (int b))))
        :else (argb 255 0 0 0)))

(defn- op-putpixel
  [h x y c]
  (let [{:keys [^BufferedImage img]} (entry h)]
    (.setRGB img (int x) (int y) (unchecked-int (color->argb c nil)))
    nil))

(defn- blend-argb
  ^long [pd ps ^double t]
  (let
    [mix (fn [sh]
           (clamp255 (+ (* (- 1.0 t) (ch pd sh)) (* t (ch ps sh)))))]
    (argb (mix 24) (mix 16) (mix 8) (mix 0))))

(defn- op-paste
  [dst src x y mask]
  (let
    [{d :img}
     (entry dst)

     {s :img}
     (entry src)

     ^BufferedImage d
     d

     ^BufferedImage s
     s

     x
     (int x)

     y
     (int y)

     sw
     (.getWidth s)

     sh
     (.getHeight s)

     dw
     (.getWidth d)

     dh
     (.getHeight d)

     mimg
     (when (and mask (>= (long mask) 0)) (:img (entry mask)))]

    (dotimes [j sh]
      (dotimes [i sw]
        (let
          [dx (+ x i)
           dy (+ y j)]

          (when (and (>= dx 0) (< dx dw) (>= dy 0) (< dy dh))
            (if mimg
              (let [mp (ch (.getRGB ^BufferedImage mimg i j) 0)]
                (cond (>= mp 255) (.setRGB d dx dy (.getRGB s i j))
                      (pos? mp) (.setRGB d
                                         dx
                                         dy
                                         (unchecked-int (blend-argb (.getRGB d dx dy)
                                                                    (.getRGB s i j)
                                                                    (/ mp 255.0))))))
              (.setRGB d dx dy (.getRGB s i j)))))))
    nil))

(defn- op-getbbox
  [h]
  (let
    [{:keys [^BufferedImage img mode]}
     (entry h)

     w
     (.getWidth img)

     hh
     (.getHeight img)

     alpha?
     (contains? #{"RGBA" "LA"} (str mode))

     minx
     (long-array 1 w)

     miny
     (long-array 1 hh)

     maxx
     (long-array 1 -1)

     maxy
     (long-array 1 -1)]

    (dotimes [y hh]
      (dotimes [x w]
        (let
          [p (.getRGB img x y)
           nz (if alpha?
                (not (zero? (bit-and p (unchecked-int 0xffffffff))))
                (not (zero? (bit-and p 0xffffff))))]

          (when nz
            (when (< x (aget minx 0)) (aset minx 0 (long x)))
            (when (< y (aget miny 0)) (aset miny 0 (long y)))
            (when (> x (aget maxx 0)) (aset maxx 0 (long x)))
            (when (> y (aget maxy 0)) (aset maxy 0 (long y)))))))
    (if (neg? (aget maxx 0))
      nil
      [(aget minx 0) (aget miny 0) (inc (aget maxx 0)) (inc (aget maxy 0))])))

(defn- op-histogram
  [h]
  (let
    [{:keys [^BufferedImage img mode]}
     (entry h)

     w
     (.getWidth img)

     hh
     (.getHeight img)

     chans
     (case (str mode)
       ("L" "1" "I" "F" "P")
       [16]

       ("RGBA" "LA")
       [16 8 0 24]

       [16 8 0])

     nch
     (count chans)

     bins
     (int-array (* 256 nch))]

    (dotimes [y hh]
      (dotimes [x w]
        (let [p (.getRGB img x y)]
          (dotimes [c nch]
            (let
              [v (ch p (nth chans c))
               idx (+ (* c 256) v)]

              (aset bins idx (inc (aget bins idx))))))))
    (vec bins)))

(defn- op-tobytes
  [h]
  (let
    [{:keys [^BufferedImage img mode]}
     (entry h)

     w
     (.getWidth img)

     hh
     (.getHeight img)

     bpp
     (case (str mode)
       ("L" "1" "I" "F" "P")
       1

       ("RGBA" "LA")
       4

       3)

     buf
     (byte-array (* w hh bpp))]

    (dotimes [y hh]
      (dotimes [x w]
        (let
          [p (.getRGB img x y)
           i (* (+ (* y w) x) bpp)]

          (case bpp
            1
            (aset buf i (unchecked-byte (ch p 16)))

            4
            (do (aset buf i (unchecked-byte (ch p 16)))
                (aset buf (+ i 1) (unchecked-byte (ch p 8)))
                (aset buf (+ i 2) (unchecked-byte (ch p 0)))
                (aset buf (+ i 3) (unchecked-byte (ch p 24))))

            (do (aset buf i (unchecked-byte (ch p 16)))
                (aset buf (+ i 1) (unchecked-byte (ch p 8)))
                (aset buf (+ i 2) (unchecked-byte (ch p 0))))))))
    (.encodeToString (Base64/getEncoder) buf)))

(defn- op-frombytes
  [mode w h b64]
  (let
    [data
     (.decode (Base64/getDecoder) ^String b64)

     mode
     (str mode)

     bpp
     (case mode
       ("L" "1" "I" "F" "P")
       1

       ("RGBA" "LA")
       4

       3)

     out
     (new-buffered mode w h)]

    (dotimes [y h]
      (dotimes [x w]
        (let
          [i (* (+ (* y (long w)) x) (long bpp))
           u (fn [^long k]
               (bit-and (aget data (+ i k)) 0xff))]

          (.setRGB out
                   x
                   y
                   (unchecked-int (case bpp
                                    1
                                    (gray-argb (u 0))

                                    4
                                    (argb (u 3) (u 0) (u 1) (u 2))

                                    (argb 255 (u 0) (u 1) (u 2))))))))
    (meta-of (put-img! out mode))))

(defn- op-point
  [h lut]
  (let
    [{:keys [^BufferedImage img mode]}
     (entry h)

     w
     (.getWidth img)

     hh
     (.getHeight img)

     L
     (int-array (map int lut))

     out
     (new-buffered mode w hh)]

    (dotimes [y hh]
      (dotimes [x w]
        (let [p (.getRGB img x y)]
          (.setRGB out
                   x
                   y
                   (unchecked-int
                     (argb (ch p 24) (aget L (ch p 16)) (aget L (ch p 8)) (aget L (ch p 0))))))))
    (meta-of (put-img! out mode))))

(defn- op-conv
  [h size kernel scale offset]
  (let
    [{:keys [^BufferedImage img mode]}
     (entry h)

     w
     (.getWidth img)

     hh
     (.getHeight img)

     k
     (int size)

     half
     (quot k 2)

     ker
     (double-array (map double kernel))

     sc
     (let [s (double scale)]
       (if (zero? s) 1.0 s))

     off
     (double offset)

     out
     (new-buffered mode w hh)]

    (dotimes [y hh]
      (dotimes [x w]
        (let [acc (double-array 3)]
          (dotimes [ky k]
            (dotimes [kx k]
              (let
                [sx (min (- w 1) (max 0 (+ x (- kx half))))
                 sy (min (- hh 1) (max 0 (+ y (- ky half))))
                 p (.getRGB img sx sy)
                 wgt (aget ker (+ (* ky k) kx))]

                (aset acc 0 (+ (aget acc 0) (* wgt (ch p 16))))
                (aset acc 1 (+ (aget acc 1) (* wgt (ch p 8))))
                (aset acc 2 (+ (aget acc 2) (* wgt (ch p 0)))))))
          (.setRGB out
                   x
                   y
                   (unchecked-int (argb (ch (.getRGB img x y) 24)
                                        (clamp255 (+ off (/ (aget acc 0) sc)))
                                        (clamp255 (+ off (/ (aget acc 1) sc)))
                                        (clamp255 (+ off (/ (aget acc 2) sc)))))))))
    (meta-of (put-img! out mode))))

(defn- op-rank
  [h size rank]
  (let
    [{:keys [^BufferedImage img mode]}
     (entry h)

     w
     (.getWidth img)

     hh
     (.getHeight img)

     k
     (int size)

     half
     (quot k 2)

     n
     (* k k)

     rank
     (int (min (dec n) (max 0 (long rank))))

     out
     (new-buffered mode w hh)]

    (dotimes [y hh]
      (dotimes [x w]
        (let
          [rs (int-array n)
           gs (int-array n)
           bs (int-array n)
           as (int-array n)
           c (int-array 1)]

          (dotimes [ky k]
            (dotimes [kx k]
              (let
                [sx (min (- w 1) (max 0 (+ x (- kx half))))
                 sy (min (- hh 1) (max 0 (+ y (- ky half))))
                 p (.getRGB img sx sy)
                 i (aget c 0)]

                (aset rs i (int (ch p 16)))
                (aset gs i (int (ch p 8)))
                (aset bs i (int (ch p 0)))
                (aset as i (int (ch p 24)))
                (aset c 0 (inc i)))))
          (Arrays/sort rs)
          (Arrays/sort gs)
          (Arrays/sort bs)
          (Arrays/sort as)
          (.setRGB out
                   x
                   y
                   (unchecked-int
                     (argb (aget as rank) (aget rs rank) (aget gs rank) (aget bs rank)))))))
    (meta-of (put-img! out mode))))

(defn- op-blend
  [ha hb t]
  (let
    [{a :img ma :mode}
     (entry ha)

     {b :img}
     (entry hb)

     ^BufferedImage a
     a

     ^BufferedImage b
     b

     t
     (double t)

     w
     (.getWidth a)

     hh
     (.getHeight a)

     out
     (new-buffered ma w hh)]

    (dotimes [y hh]
      (dotimes [x w]
        (.setRGB out x y (unchecked-int (blend-argb (.getRGB a x y) (.getRGB b x y) t)))))
    (meta-of (put-img! out ma))))

(defn- op-composite
  [ha hb hmask]
  (let
    [{a :img ma :mode}
     (entry ha)

     {b :img}
     (entry hb)

     {m :img}
     (entry hmask)

     ^BufferedImage a
     a

     ^BufferedImage b
     b

     ^BufferedImage m
     m

     w
     (.getWidth a)

     hh
     (.getHeight a)

     out
     (new-buffered ma w hh)]

    (dotimes [y hh]
      (dotimes [x w]
        (let [t (/ (ch (.getRGB m x y) 0) 255.0)]
          ;; composite(im1, im2, mask) = im1*mask + im2*(1-mask)
          (.setRGB out x y (unchecked-int (blend-argb (.getRGB b x y) (.getRGB a x y) t))))))
    (meta-of (put-img! out ma))))

(defn- chop-fn
  [op]
  (case (str op)
    "difference"
    (fn [^long a ^long b]
      (Math/abs (- a b)))

    "add"
    (fn [^long a ^long b]
      (min 255 (+ a b)))

    "subtract"
    (fn [^long a ^long b]
      (max 0 (- a b)))

    "multiply"
    (fn [^long a ^long b]
      (quot (* a b) 255))

    "screen"
    (fn [^long a ^long b]
      (- 255 (quot (* (- 255 a) (- 255 b)) 255)))

    "lighter"
    (fn [^long a ^long b]
      (max a b))

    "darker"
    (fn [^long a ^long b]
      (min a b))

    "add_modulo"
    (fn [^long a ^long b]
      (mod (+ a b) 256))

    "subtract_modulo"
    (fn [^long a ^long b]
      (mod (- a b) 256))

    "logical_and"
    (fn [^long a ^long b]
      (if (and (pos? a) (pos? b)) 255 0))

    "logical_or"
    (fn [^long a ^long b]
      (if (or (pos? a) (pos? b)) 255 0))

    "logical_xor"
    (fn [^long a ^long b]
      (if (not= (pos? a) (pos? b)) 255 0))

    "overlay"
    (fn [^long a ^long b]
      (if (< a 128) (quot (* 2 a b) 255) (- 255 (quot (* 2 (- 255 a) (- 255 b)) 255))))

    "hard_light"
    (fn [^long a ^long b]
      (if (< b 128) (quot (* 2 a b) 255) (- 255 (quot (* 2 (- 255 a) (- 255 b)) 255))))

    "soft_light"
    (fn [^long a ^long b]
      (let
        [a'
         (/ (double a) 255.0)

         b'
         (/ (double b) 255.0)

         res
         (if (<= b' 0.5)
           (- a' (* (- 1.0 (* 2.0 b')) a' (- 1.0 a')))
           (let [d (if (<= a' 0.25) (* (+ (* (- (* 16.0 a') 12.0) a') 4.0) a') (Math/sqrt a'))]
             (+ a' (* (- (* 2.0 b') 1.0) (- d a')))))]

        (clamp255 (* res 255.0))))

    (fn [^long a ^long _b]
      a)))

(defn- op-chop
  [op ha hb]
  (let
    [f
     (chop-fn op)

     {a :img ma :mode}
     (entry ha)

     {b :img}
     (entry hb)

     ^BufferedImage a
     a

     ^BufferedImage b
     b

     w
     (.getWidth a)

     hh
     (.getHeight a)

     out
     (new-buffered ma w hh)]

    (dotimes [y hh]
      (dotimes [x w]
        (let
          [pa (.getRGB a x y)
           pb (.getRGB b x y)]

          (.setRGB out
                   x
                   y
                   (unchecked-int (argb (f (ch pa 24) (ch pb 24))
                                        (f (ch pa 16) (ch pb 16))
                                        (f (ch pa 8) (ch pb 8))
                                        (f (ch pa 0) (ch pb 0))))))))
    (meta-of (put-img! out ma))))

(defn- op-split
  [h]
  (let
    [{:keys [^BufferedImage img mode]}
     (entry h)

     w
     (.getWidth img)

     hh
     (.getHeight img)

     shifts
     (case (str mode)
       ("L" "1" "I" "F" "P")
       [16]

       ("RGBA" "LA")
       [16 8 0 24]

       [16 8 0])]

    (mapv (fn [sh]
            (let [out (new-buffered "L" w hh)]
              (dotimes [y hh]
                (dotimes [x w]
                  (.setRGB out x y (unchecked-int (gray-argb (ch (.getRGB img x y) sh))))))
              (meta-of (put-img! out "L"))))
          shifts)))

(defn- op-merge
  [mode handles]
  (let
    [mode
     (str mode)

     imgs
     (mapv #(:img (entry %)) handles)

     ^BufferedImage f
     (first imgs)

     w
     (.getWidth f)

     hh
     (.getHeight f)

     out
     (new-buffered mode w hh)]

    (dotimes [y hh]
      (dotimes [x w]
        (let
          [vals (mapv #(ch (.getRGB ^BufferedImage % x y) 0) imgs)
           [r g b a] vals]

          (.setRGB out
                   x
                   y
                   (unchecked-int (case mode
                                    ("RGBA" "LA")
                                    (argb (or a 255) r g b)

                                    "RGB"
                                    (argb 255 r g b)

                                    (gray-argb r)))))))
    (meta-of (put-img! out mode))))

(defn- op-offset
  "Roll `img` by (dx, dy) with wraparound (ImageChops.offset)."
  [h dx dy]
  (let
    [{:keys [^BufferedImage img mode]}
     (entry h)

     w
     (.getWidth img)

     hh
     (.getHeight img)

     dx
     (long dx)

     dy
     (long dy)

     out
     (new-buffered mode w hh)]

    (dotimes [y hh]
      (dotimes [x w]
        (.setRGB out x y (.getRGB img (int (mod (- x dx) w)) (int (mod (- y dy) hh))))))
    (meta-of (put-img! out mode))))

(defn- op-alpha-composite
  "Porter-Duff `src` OVER `dst` at offset (dx, dy); returns a new RGBA image."
  [hdst hsrc dx dy]
  (let
    [{d :img}
     (entry hdst)

     {s :img}
     (entry hsrc)

     ^BufferedImage d
     d

     ^BufferedImage s
     s

     dx
     (long dx)

     dy
     (long dy)

     w
     (.getWidth d)

     hh
     (.getHeight d)

     sw
     (.getWidth s)

     sh
     (.getHeight s)

     out
     (new-buffered "RGBA" w hh)]

    (dotimes [y hh]
      (dotimes [x w]
        (.setRGB out x y (.getRGB d x y))))
    (dotimes [y sh]
      (dotimes [x sw]
        (let
          [ox (+ x dx)
           oy (+ y dy)]

          (when (and (>= ox 0) (< ox w) (>= oy 0) (< oy hh))
            (let
              [ps (.getRGB s x y)
               pd (.getRGB out ox oy)
               sa (/ (double (ch ps 24)) 255.0)
               da (/ (double (ch pd 24)) 255.0)
               oa (+ sa (* da (- 1.0 sa)))]

              (if (<= oa 0.0)
                (.setRGB out ox oy (unchecked-int (argb 0 0 0 0)))
                (let
                  [f (fn [^long cs ^long cd]
                       (clamp255 (/ (+ (* cs sa) (* cd da (- 1.0 sa))) oa)))]
                  (.setRGB out
                           ox
                           oy
                           (unchecked-int (argb (clamp255 (* oa 255.0))
                                                (f (ch ps 16) (ch pd 16))
                                                (f (ch ps 8) (ch pd 8))
                                                (f (ch ps 0) (ch pd 0))))))))))))
    (meta-of (put-img! out "RGBA"))))

(defn- op-transform
  "Geometric transform via BACKWARD mapping: output pixel (x,y) samples source at
   coeffs applied. method AFFINE -> (a b c d e f); PERSPECTIVE -> (a b c d e f g h).
   Out-of-bounds samples take `fillc`. Nearest-neighbour (PIL's AFFINE default)."
  [h ow oh method coeffs fillc]
  (let
    [{:keys [^BufferedImage img mode]}
     (entry h)

     ow
     (long ow)

     oh
     (long oh)

     sw
     (.getWidth img)

     sh
     (.getHeight img)

     cf
     (mapv double coeffs)

     persp?
     (= (str method) "PERSPECTIVE")

     out
     (new-buffered mode ow oh)

     ^Color fc
     (->color fillc mode)

     fill-argb
     (.getRGB fc)]

    (dotimes [y oh]
      (dotimes [x ow]
        (let
          [xd (double x)
           yd (double y)
           den (if persp? (+ (* (double (nth cf 6)) xd) (* (double (nth cf 7)) yd) 1.0) 1.0)
           sx (/ (+ (* (double (nth cf 0)) xd) (* (double (nth cf 1)) yd) (double (nth cf 2))) den)
           sy (/ (+ (* (double (nth cf 3)) xd) (* (double (nth cf 4)) yd) (double (nth cf 5))) den)]

          (if (and (>= sx 0.0) (< sx sw) (>= sy 0.0) (< sy sh))
            (.setRGB out x y (.getRGB img (int sx) (int sy)))
            (.setRGB out x y (unchecked-int fill-argb))))))
    (meta-of (put-img! out mode))))

;; ---------------------------------------------------------------------------
;; ImageDraw dispatcher. `xy` is a flat [x0 y0 x1 y1 ...] seq; `opts` is a
;; string-keyed map with resolved fill/outline colours (lists/ints), width and
;; arc start/end. Colours are resolved Python-side via ImageColor.
;; ---------------------------------------------------------------------------

(defn- op-draw
  [h op xy opts]
  (let
    [{:keys [^BufferedImage img]}
     (entry h)

     g
     (.createGraphics img)

     pts
     (mapv double xy)

     fill
     (get opts "fill")

     outline
     (get opts "outline")

     width
     (int (or (get opts "width") 1))]

    (.setRenderingHint g RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON)
    (.setStroke g (BasicStroke. (float (max 1 width))))
    (case (str op)
      "point"
      (when (some? fill)
        (.setColor g (->color fill "RGB"))
        (doseq [[x y] (partition 2 pts)]
          (.fillRect g (int x) (int y) 1 1)))

      "line"
      (when (some? fill)
        (.setColor g (->color fill "RGB"))
        (doseq [[[x1 y1] [x2 y2]] (partition 2 1 (partition 2 pts))]
          (.drawLine g (int x1) (int y1) (int x2) (int y2))))

      "rectangle"
      (let
        [[x0 y0 x1 y1]
         pts

         rx
         (int (min (double x0) (double x1)))

         ry
         (int (min (double y0) (double y1)))

         rw
         (int (Math/abs (- (double x1) (double x0))))

         rh
         (int (Math/abs (- (double y1) (double y0))))]

        (when (some? fill) (.setColor g (->color fill "RGB")) (.fillRect g rx ry (inc rw) (inc rh)))
        (when (some? outline) (.setColor g (->color outline "RGB")) (.drawRect g rx ry rw rh)))

      "ellipse"
      (let
        [[x0 y0 x1 y1]
         pts

         w
         (int (- (double x1) (double x0)))

         hh
         (int (- (double y1) (double y0)))]

        (when (some? fill)
          (.setColor g (->color fill "RGB"))
          (.fillOval g (int x0) (int y0) (inc w) (inc hh)))
        (when (some? outline)
          (.setColor g (->color outline "RGB"))
          (.drawOval g (int x0) (int y0) w hh)))

      "polygon"
      (let
        [n
         (int (/ (count pts) 2))

         xs
         (int-array (map int (take-nth 2 pts)))

         ys
         (int-array (map int (take-nth 2 (rest pts))))]

        (when (some? fill) (.setColor g (->color fill "RGB")) (.fillPolygon g xs ys n))
        (when (some? outline) (.setColor g (->color outline "RGB")) (.drawPolygon g xs ys n)))

      ("arc" "chord" "pieslice")
      (let
        [[x0 y0 x1 y1]
         pts

         start
         (double (or (get opts "start") 0))

         end
         (double (or (get opts "end") 0))

         kind
         (case (str op)
           "arc"
           java.awt.geom.Arc2D/OPEN

           "chord"
           java.awt.geom.Arc2D/CHORD

           java.awt.geom.Arc2D/PIE)

         arc
         (java.awt.geom.Arc2D$Double. (double x0)
                                      (double y0)
                                      (- (double x1) (double x0))
                                      (- (double y1) (double y0))
                                      (- start)
                                      (- (- end start))
                                      kind)]

        (when (and (some? fill) (not= (str op) "arc"))
          (.setColor g (->color fill "RGB"))
          (.fill g arc))
        (let [oc (or outline (when (= (str op) "arc") fill))]
          (when (some? oc) (.setColor g (->color oc "RGB")) (.draw g arc))))

      "text"
      (let
        [[x y]
         pts

         s
         (str (get opts "text"))

         size
         (int (or (get opts "font_size") 12))]

        (.setFont g (Font. "SansSerif" Font/PLAIN size))
        (.setColor g (->color (or fill [0 0 0]) "RGB"))
        (let [fm (.getFontMetrics g)]
          (.drawString g s (int x) (int (+ (double y) (.getAscent fm))))))

      nil)
    (.dispose g)
    nil))

(defn- op-textbbox
  [text size]
  (let
    [img
     (BufferedImage. 1 1 BufferedImage/TYPE_INT_RGB)

     g
     (.createGraphics img)]

    (.setFont g (Font. "SansSerif" Font/PLAIN (int size)))
    (let
      [fm
       (.getFontMetrics g)

       w
       (.stringWidth fm (str text))

       hh
       (.getHeight fm)]

      (.dispose g)
      [0 0 w hh])))

;; ---------------------------------------------------------------------------
;; Bridge: name -> Clojure fn. Wrapped by `wrap-ifn` at install time (positional
;; Python args marshalled to Clojure, result back to Python). Every call is
;; enveloped [true payload] / [false message] so a host failure crosses as DATA
;; the Python shim can raise as a catchable OSError.
;; ---------------------------------------------------------------------------

(defn- pil-envelope [f] (try [true (f)] (catch Throwable t [false (str (or (.getMessage t) t))])))

(defn- pil-bridge-bindings
  "Host callables (Java2D / ImageIO) the PIL shim delegates to. All image ops go
   through here; the Python side only holds integer handles + base64 blobs."
  []
  {"__vis_pil_new__" (fn [mode w h fill]
                       (pil-envelope #(op-new mode (long w) (long h) fill)))
   "__vis_pil_open__" (fn [b64]
                        (pil-envelope #(op-open b64)))
   "__vis_pil_save__" (fn [h fmt]
                        (pil-envelope #(op-save h fmt)))
   "__vis_pil_save_temp__" (fn [h fmt]
                             (pil-envelope #(op-save-temp h fmt)))
   "__vis_pil_meta__" (fn [h]
                        (pil-envelope #(meta-of h)))
   "__vis_pil_copy__" (fn [h]
                        (pil-envelope #(op-copy h)))
   "__vis_pil_free__" (fn [h]
                        (pil-envelope #(free-img! h)))
   "__vis_pil_resize__" (fn [h w hh r]
                          (pil-envelope #(op-resize h (long w) (long hh) r)))
   "__vis_pil_crop__" (fn [h l t r b]
                        (pil-envelope #(op-crop h (long l) (long t) (long r) (long b))))
   "__vis_pil_rotate__" (fn [h ang exp fill]
                          (pil-envelope #(op-rotate h ang exp fill)))
   "__vis_pil_transpose__" (fn [h m]
                             (pil-envelope #(op-transpose h m)))
   "__vis_pil_convert__" (fn [h t]
                           (pil-envelope #(op-convert h t)))
   "__vis_pil_getpixel__" (fn [h x y]
                            (pil-envelope #(op-getpixel h x y)))
   "__vis_pil_putpixel__" (fn [h x y c]
                            (pil-envelope #(op-putpixel h x y c)))
   "__vis_pil_paste__" (fn [d s x y m]
                         (pil-envelope #(op-paste d s x y m)))
   "__vis_pil_getbbox__" (fn [h]
                           (pil-envelope #(op-getbbox h)))
   "__vis_pil_histogram__" (fn [h]
                             (pil-envelope #(op-histogram h)))
   "__vis_pil_tobytes__" (fn [h]
                           (pil-envelope #(op-tobytes h)))
   "__vis_pil_frombytes__" (fn [mode w h b64]
                             (pil-envelope #(op-frombytes mode (long w) (long h) b64)))
   "__vis_pil_point__" (fn [h lut]
                         (pil-envelope #(op-point h lut)))
   "__vis_pil_conv__" (fn [h size ker sc off]
                        (pil-envelope #(op-conv h (long size) ker sc off)))
   "__vis_pil_rank__" (fn [h size rank]
                        (pil-envelope #(op-rank h (long size) (long rank))))
   "__vis_pil_blend__" (fn [a b t]
                         (pil-envelope #(op-blend a b t)))
   "__vis_pil_composite__" (fn [a b m]
                             (pil-envelope #(op-composite a b m)))
   "__vis_pil_chop__" (fn [op a b]
                        (pil-envelope #(op-chop op a b)))
   "__vis_pil_split__" (fn [h]
                         (pil-envelope #(op-split h)))
   "__vis_pil_merge__" (fn [mode hs]
                         (pil-envelope #(op-merge mode hs)))
   "__vis_pil_draw__" (fn [h op xy opts]
                        (pil-envelope #(op-draw h op xy opts)))
   "__vis_pil_textbbox__" (fn [text size]
                            (pil-envelope #(op-textbbox text size)))
   "__vis_pil_offset__" (fn [h dx dy]
                          (pil-envelope #(op-offset h dx dy)))
   "__vis_pil_alpha_composite__" (fn [d s dx dy]
                                   (pil-envelope #(op-alpha-composite d s dx dy)))
   "__vis_pil_transform__" (fn [h ow oh method coeffs fill]
                             (pil-envelope
                               #(op-transform h (long ow) (long oh) method coeffs fill)))})


(def vis-extension
  (vis/extension
    {:ext/name "foundation-shim-pil"
     :ext/description
     "Sandbox shim: a broad Pillow (PIL) surface (PIL.Image with new/open/save/copy/resize/thumbnail/reduce/crop/rotate/transpose/transform(AFFINE/EXTENT/PERSPECTIVE/QUAD/MESH)/convert/quantize/paste/alpha_composite/getpixel/putpixel/point/split/merge/getbbox/getcolors/getextrema/histogram/entropy/getprojection/tobytes/frombytes/getdata/putdata/getchannel/putalpha/get-put-palette/seek/tell/n_frames + module new/open/blend/composite/alpha_composite/eval/merge/fromarray/frombytes/linear_gradient/radial_gradient/effect_noise/effect_mandelbrot; ImageDraw point/line/rectangle/rounded_rectangle/ellipse/polygon/regular_polygon/circle/arc/chord/pieslice/text/multiline_text/textbbox/floodfill; ImageFilter blur/sharpen/edge/emboss/GaussianBlur/BoxBlur/Median/Min/Max/Mode/UnsharpMask/Kernel/Color3DLUT; ImageOps grayscale/invert/mirror/flip/posterize/solarize/autocontrast/equalize/expand/crop/fit/pad/contain/cover/scale/colorize/exif_transpose/deform; ImageColor named+hex+rgb(); ImageEnhance Color/Contrast/Brightness/Sharpness; ImageChops difference/add/subtract/multiply/screen/lighter/darker/add-sub_modulo/logical_*/overlay/soft_light/hard_light/offset/blend/composite; ImageStat mean/median/stddev/var/rms/extrema; ImageMath eval; ImageFont truetype/load_default; plus ImageSequence/ImagePalette/ImageTransform/features/ExifTags/TiffTags) backed by a pure-JVM Java2D/ImageIO renderer. Image.show() surfaces the image inline as a session attachment. No pip, no native wheel."
     :ext/version "0.1.0"
     :ext/author "Blockether"
     :ext/owner "vis"
     :ext/license "Apache-2.0"
     :ext/kind "foundation"
     :ext/sandbox-shims
     [{:shim/name "pil"
       :shim/imports ["PIL"]
       :shim/description
       "Pillow-compatible `PIL` (Image/ImageDraw/ImageFilter/ImageOps/ImageColor/ImageEnhance/ImageChops/ImageFont) backed by JVM Java2D/ImageIO. Not supported: some color-mode conversions and `Image.transform` methods raise `ValueError`."
       :shim/bindings pil-bridge-bindings
       :shim/source "vis-shims/pil.py"}]}))

(vis/register-extension! vis-extension)
