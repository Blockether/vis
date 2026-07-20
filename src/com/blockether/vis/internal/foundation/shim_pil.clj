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
           [java.io ByteArrayInputStream ByteArrayOutputStream File FileOutputStream]
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
        (let [v
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
  (let [out
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
  (let [img
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
  (let [bytes
        (.decode (Base64/getDecoder) ^String b64)

        img
        (ImageIO/read (ByteArrayInputStream. bytes))]

    (when (nil? img) (throw (ex-info "cannot identify image file" {})))
    (let [mode
          (img->mode img)

          h
          (put-img! img mode)]

      (meta-of h))))

(defn- op-save
  [h fmt]
  (let [{:keys [^BufferedImage img]}
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
  (let [{:keys [^BufferedImage img]}
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

        dir
        (doto (File. (System/getProperty "java.io.tmpdir") "vis-pil") (.mkdirs))

        f
        (File/createTempFile "img-" (str "." fmt) dir)]

    (with-open [o (FileOutputStream. f)]
      (.write o ^bytes bytes))
    (mpl-capture/record-attachment! {:kind "image"
                                     :media-type (str "image/" norm)
                                     :base64 b64
                                     :size (alength bytes)
                                     :filename (.getName f)
                                     :dims (str (.getWidth img) "x" (.getHeight img))})
    [(.getAbsolutePath f) (.getWidth img) (.getHeight img) (alength bytes)]))

(defn- op-copy
  [h]
  (let [{:keys [^BufferedImage img mode]}
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
  (let [{:keys [^BufferedImage img mode]}
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
  (let [{:keys [^BufferedImage img mode]}
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
  (let [{:keys [^BufferedImage img mode]}
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
  (let [{:keys [^BufferedImage img mode]}
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
        (let [p (.getRGB img x y)
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
      (let [w (.getWidth img)
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
                (let [p (.getRGB img x y)
                      v (lum p)]

                  (.setRGB out x y (unchecked-int (argb (ch p 24) v v v))))))
            (meta-of (put-img! out "LA")))

          ;; RGB / RGBA: a straight channel copy (drawImage preserves sRGB).
          (let [out (new-buffered target w hh)
                g (.createGraphics out)]

            (.setComposite g AlphaComposite/Src)
            (.drawImage g img 0 0 nil)
            (.dispose g)
            (meta-of (put-img! out target))))))))

(defn- op-getpixel
  [h x y]
  (let [{:keys [^BufferedImage img mode]}
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
        (sequential? c) (let [v
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
  (let [mix (fn [sh]
              (clamp255 (+ (* (- 1.0 t) (ch pd sh)) (* t (ch ps sh)))))]
    (argb (mix 24) (mix 16) (mix 8) (mix 0))))

(defn- op-paste
  [dst src x y mask]
  (let [{d :img}
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
        (let [dx (+ x i)
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
  (let [{:keys [^BufferedImage img mode]}
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
        (let [p (.getRGB img x y)
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
  (let [{:keys [^BufferedImage img mode]}
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
            (let [v (ch p (nth chans c))
                  idx (+ (* c 256) v)]

              (aset bins idx (inc (aget bins idx))))))))
    (vec bins)))

(defn- op-tobytes
  [h]
  (let [{:keys [^BufferedImage img mode]}
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
        (let [p (.getRGB img x y)
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
  (let [data
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
        (let [i (* (+ (* y (long w)) x) (long bpp))
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
  (let [{:keys [^BufferedImage img mode]}
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
  (let [{:keys [^BufferedImage img mode]}
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
              (let [sx (min (- w 1) (max 0 (+ x (- kx half))))
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
  (let [{:keys [^BufferedImage img mode]}
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
        (let [rs (int-array n)
              gs (int-array n)
              bs (int-array n)
              as (int-array n)
              c (int-array 1)]

          (dotimes [ky k]
            (dotimes [kx k]
              (let [sx (min (- w 1) (max 0 (+ x (- kx half))))
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
  (let [{a :img ma :mode}
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
  (let [{a :img ma :mode}
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
      (let [a'
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
  (let [f
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
        (let [pa (.getRGB a x y)
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
  (let [{:keys [^BufferedImage img mode]}
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
  (let [mode
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
        (let [vals (mapv #(ch (.getRGB ^BufferedImage % x y) 0) imgs)
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
  (let [{:keys [^BufferedImage img mode]}
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
  (let [{d :img}
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
        (let [ox (+ x dx)
              oy (+ y dy)]

          (when (and (>= ox 0) (< ox w) (>= oy 0) (< oy hh))
            (let [ps (.getRGB s x y)
                  pd (.getRGB out ox oy)
                  sa (/ (double (ch ps 24)) 255.0)
                  da (/ (double (ch pd 24)) 255.0)
                  oa (+ sa (* da (- 1.0 sa)))]

              (if (<= oa 0.0)
                (.setRGB out ox oy (unchecked-int (argb 0 0 0 0)))
                (let [f (fn [^long cs ^long cd]
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
  (let [{:keys [^BufferedImage img mode]}
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
        (let [xd (double x)
              yd (double y)
              den (if persp? (+ (* (double (nth cf 6)) xd) (* (double (nth cf 7)) yd) 1.0) 1.0)
              sx (/ (+ (* (double (nth cf 0)) xd) (* (double (nth cf 1)) yd) (double (nth cf 2)))
                    den)
              sy (/ (+ (* (double (nth cf 3)) xd) (* (double (nth cf 4)) yd) (double (nth cf 5)))
                    den)]

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
  (let [{:keys [^BufferedImage img]}
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
      (let [[x0 y0 x1 y1]
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
      (let [[x0 y0 x1 y1]
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
      (let [n
            (int (/ (count pts) 2))

            xs
            (int-array (map int (take-nth 2 pts)))

            ys
            (int-array (map int (take-nth 2 (rest pts))))]

        (when (some? fill) (.setColor g (->color fill "RGB")) (.fillPolygon g xs ys n))
        (when (some? outline) (.setColor g (->color outline "RGB")) (.drawPolygon g xs ys n)))

      ("arc" "chord" "pieslice")
      (let [[x0 y0 x1 y1]
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
      (let [[x y]
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
  (let [img
        (BufferedImage. 1 1 BufferedImage/TYPE_INT_RGB)

        g
        (.createGraphics img)]

    (.setFont g (Font. "SansSerif" Font/PLAIN (int size)))
    (let [fm
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

(def ^:private pil-shim-src
  "Pure-Python preamble that publishes a Pillow-compatible `PIL` package backed by
   the JVM Java2D / ImageIO stack. All image work is DELEGATED to the host
   callables `__vis_pil_*` (bound from `pil-bridge-bindings`), looked up in
   `globals()` at CALL time so the shim is backend-agnostic. Images live host-side
   as `BufferedImage`s keyed by an integer handle; the Python `Image` is a thin
   wrapper. Published into `sys.modules` (so `from PIL import Image` works) AND
   stapled onto builtins (so `PIL.Image` / `Image.new` work with NO import).
   INLINED here so it ships in-jar with no separate `.py` resource. Installed once
   per sandbox context (main + every sub_loop fork), BEFORE the baseline snapshot
   so its `__vis_*` names are filtered out of the model-visible live-vars view.
   Python uses single-quoted string literals throughout so this Clojure string
   needs no escaping."
  "# vis sandbox PIL/Pillow-compat shim, backed by the JVM Java2D / ImageIO stack.
#
# The agent sandbox ships no CPython Pillow wheel. This shim publishes a
# Pillow-compatible PIL package whose Image/ImageDraw/ImageFilter/ImageOps/
# ImageColor/ImageEnhance/ImageChops/ImageFont operations DELEGATE to host
# callables (__vis_pil_*), looked up in globals() at CALL time so the shim is
# backend-agnostic. Images live host-side as BufferedImages keyed by an integer
# handle; the Python Image is a thin wrapper. Published into sys.modules (so
# `from PIL import Image` works) and stapled onto builtins (so PIL.Image /
# Image.new work with NO import). Single-quoted string literals throughout so
# the enclosing Clojure string needs no escaping.

def __vis_install_pil__():
    import sys, types, base64, math

    def _H(name, *args):
        fn = globals().get(name)
        if fn is None:
            raise OSError('vis: the PIL Java2D backend is not bound in this sandbox')
        env = fn(*args)
        if not env[0]:
            raise OSError(str(env[1]))
        return env[1]

    def _lst(x):
        try:
            return list(x)
        except Exception:
            return x

    # -- resampling / transpose / mode constants -----------------------------
    NEAREST = 0
    LANCZOS = 1
    ANTIALIAS = 1
    BILINEAR = 2
    BICUBIC = 3
    BOX = 4
    HAMMING = 5

    class Resampling:
        NEAREST = 0
        LANCZOS = 1
        BILINEAR = 2
        BICUBIC = 3
        BOX = 4
        HAMMING = 5

    FLIP_LEFT_RIGHT = 0
    FLIP_TOP_BOTTOM = 1
    ROTATE_90 = 2
    ROTATE_180 = 3
    ROTATE_270 = 4
    TRANSPOSE = 5
    TRANSVERSE = 6

    class Transpose:
        FLIP_LEFT_RIGHT = 0
        FLIP_TOP_BOTTOM = 1
        ROTATE_90 = 2
        ROTATE_180 = 3
        ROTATE_270 = 4
        TRANSPOSE = 5
        TRANSVERSE = 6

    # -- geometric-transform method constants -------------------------------
    AFFINE = 0
    EXTENT = 1
    PERSPECTIVE = 2
    QUAD = 3
    MESH = 4

    def _solve_linear(matrix, rhs):
        n = len(rhs)
        m = [matrix[i][:] + [rhs[i]] for i in range(n)]
        for col in range(n):
            piv = max(range(col, n), key=lambda r: abs(m[r][col]))
            m[col], m[piv] = m[piv], m[col]
            d = m[col][col] or 1e-12
            for j in range(col, n + 1):
                m[col][j] /= d
            for r in range(n):
                if r != col and m[r][col]:
                    fr = m[r][col]
                    for j in range(col, n + 1):
                        m[r][j] -= fr * m[col][j]
        return [m[i][n] for i in range(n)]

    def _quad_to_perspective(w, h, quad):
        sx = [float(quad[0]), float(quad[2]), float(quad[4]), float(quad[6])]
        sy = [float(quad[1]), float(quad[3]), float(quad[5]), float(quad[7])]
        tx = [0.0, 0.0, float(w), float(w)]
        ty = [0.0, float(h), float(h), 0.0]
        a = []
        b = []
        for i in range(4):
            a.append([tx[i], ty[i], 1.0, 0.0, 0.0, 0.0, -tx[i] * sx[i], -ty[i] * sx[i]])
            b.append(sx[i])
            a.append([0.0, 0.0, 0.0, tx[i], ty[i], 1.0, -tx[i] * sy[i], -ty[i] * sy[i]])
            b.append(sy[i])
        return _solve_linear(a, b)

    # -- ImageColor ----------------------------------------------------------
    _NAMED = {
        'black': (0, 0, 0), 'white': (255, 255, 255), 'red': (255, 0, 0),
        'lime': (0, 255, 0), 'green': (0, 128, 0), 'blue': (0, 0, 255),
        'yellow': (255, 255, 0), 'cyan': (0, 255, 255), 'aqua': (0, 255, 255),
        'magenta': (255, 0, 255), 'fuchsia': (255, 0, 255),
        'silver': (192, 192, 192), 'gray': (128, 128, 128), 'grey': (128, 128, 128),
        'maroon': (128, 0, 0), 'olive': (128, 128, 0), 'purple': (128, 0, 128),
        'teal': (0, 128, 128), 'navy': (0, 0, 128), 'orange': (255, 165, 0),
        'pink': (255, 192, 203), 'brown': (165, 42, 42), 'gold': (255, 215, 0),
        'violet': (238, 130, 238), 'indigo': (75, 0, 130), 'ivory': (255, 255, 240),
        'khaki': (240, 230, 140), 'crimson': (220, 20, 60), 'coral': (255, 127, 80),
        'salmon': (250, 128, 114), 'turquoise': (64, 224, 208), 'tan': (210, 180, 140),
        'beige': (245, 245, 220), 'lavender': (230, 230, 250), 'plum': (221, 160, 221),
        'orchid': (218, 112, 214), 'skyblue': (135, 206, 235),
        'lightgray': (211, 211, 211), 'lightgrey': (211, 211, 211),
        'darkgray': (169, 169, 169), 'darkgrey': (169, 169, 169),
        'lightblue': (173, 216, 230), 'lightgreen': (144, 238, 144),
        'darkblue': (0, 0, 139), 'darkgreen': (0, 100, 0), 'darkred': (139, 0, 0),
        'transparent': (0, 0, 0, 0),
    }

    def _getrgb(color):
        if isinstance(color, (list, tuple)):
            return tuple(int(c) for c in color)
        if isinstance(color, int):
            return (color, color, color)
        s = str(color).strip().lower()
        if s in _NAMED:
            return _NAMED[s]
        if s.startswith('#'):
            h = s[1:]
            if len(h) == 3:
                return tuple(int(c * 2, 16) for c in h)
            if len(h) == 4:
                return tuple(int(c * 2, 16) for c in h)
            if len(h) == 6:
                return (int(h[0:2], 16), int(h[2:4], 16), int(h[4:6], 16))
            if len(h) == 8:
                return (int(h[0:2], 16), int(h[2:4], 16), int(h[4:6], 16), int(h[6:8], 16))
        if s.startswith('rgb'):
            inside = s[s.index('(') + 1:s.index(')')]
            vals = []
            for p in inside.split(','):
                p = p.strip()
                if p.endswith('%'):
                    vals.append(int(round(float(p[:-1]) * 255 / 100)))
                else:
                    vals.append(int(float(p)))
            return tuple(vals)
        raise ValueError('unknown color specifier: ' + repr(color))

    def _getcolor(color, mode):
        rgb = _getrgb(color)
        if mode in ('L', '1'):
            r, g, b = rgb[0], rgb[1], rgb[2]
            return int(round(0.299 * r + 0.587 * g + 0.114 * b))
        return rgb

    ImageColor = types.ModuleType('PIL.ImageColor')
    ImageColor.getrgb = _getrgb
    ImageColor.getcolor = _getcolor
    ImageColor.colormap = dict(_NAMED)

    # -- the Image class -----------------------------------------------------
    def _wrap(meta):
        m = _lst(meta)
        return Image(int(m[0]), int(m[1]), int(m[2]), str(m[3]))

    class Image:
        def __init__(self, handle, w, h, mode):
            self._handle = int(handle)
            self._w = int(w)
            self._h = int(h)
            self.mode = str(mode)
            self.info = {}
            self.palette = None
            self.format = None

        @property
        def size(self):
            return (self._w, self._h)

        @property
        def width(self):
            return self._w

        @property
        def height(self):
            return self._h

        def __repr__(self):
            return '<PIL.Image.Image mode=%s size=%dx%d at handle %d>' % (
                self.mode, self._w, self._h, self._handle)

        def __enter__(self):
            return self

        def __exit__(self, *a):
            self.close()
            return False

        def _set(self, meta):
            m = _lst(meta)
            self._handle, self._w, self._h, self.mode = int(m[0]), int(m[1]), int(m[2]), str(m[3])
            return self

        def copy(self):
            return _wrap(_H('__vis_pil_copy__', self._handle))

        def close(self):
            try:
                _H('__vis_pil_free__', self._handle)
            except Exception:
                pass

        def load(self):
            img = self

            class _Access:
                def __getitem__(self, xy):
                    return img.getpixel(xy)

                def __setitem__(self, xy, value):
                    img.putpixel(xy, value)
            return _Access()

        def resize(self, size, resample=BICUBIC, box=None):
            w, h = size
            return _wrap(_H('__vis_pil_resize__', self._handle, int(w), int(h), int(resample)))

        def thumbnail(self, size, resample=BICUBIC):
            mw, mh = size
            w, h = self._w, self._h
            r = min(mw / float(w), mh / float(h), 1.0)
            nw, nh = max(1, int(w * r)), max(1, int(h * r))
            self._set(_H('__vis_pil_resize__', self._handle, nw, nh, int(resample)))
            return None

        def crop(self, box):
            l, t, r, b = box
            return _wrap(_H('__vis_pil_crop__', self._handle, int(l), int(t), int(r), int(b)))

        def rotate(self, angle, resample=NEAREST, expand=0, center=None,
                   translate=None, fillcolor=None):
            fc = None
            if fillcolor is not None:
                fc = _getrgb(fillcolor) if isinstance(fillcolor, str) else fillcolor
                if isinstance(fc, (list, tuple)):
                    fc = [int(x) for x in fc]
            return _wrap(_H('__vis_pil_rotate__', self._handle, float(angle),
                            bool(expand), fc))

        def transpose(self, method):
            return _wrap(_H('__vis_pil_transpose__', self._handle, int(method)))

        def convert(self, mode=None, *a, **k):
            if mode is None or mode == self.mode:
                return self.copy()
            if str(mode) not in ('1', 'L', 'LA', 'La', 'I', 'F', 'P', 'RGB', 'RGBA', 'RGBX', 'RGBa', 'CMYK', 'YCbCr', 'HSV', 'I;16', 'I;16B'):
                raise ValueError('conversion from ' + str(self.mode) + ' to ' + str(mode) + ' not supported')
            return _wrap(_H('__vis_pil_convert__', self._handle, str(mode)))

        def getpixel(self, xy):
            v = _H('__vis_pil_getpixel__', self._handle, int(xy[0]), int(xy[1]))
            if isinstance(v, (list, tuple)):
                return tuple(int(c) for c in v)
            return int(v)

        def putpixel(self, xy, value):
            if isinstance(value, (list, tuple)):
                val = [int(c) for c in value]
            else:
                val = int(value)
            _H('__vis_pil_putpixel__', self._handle, int(xy[0]), int(xy[1]), val)

        def paste(self, im, box=None, mask=None):
            if not isinstance(im, Image):
                # color paste: fill the region (box) with a solid colour
                if box is None:
                    box = (0, 0, self._w, self._h)
                if len(box) == 2:
                    box = (box[0], box[1], self._w, self._h)
                col = _getrgb(im) if isinstance(im, str) else im
                tmp = new(self.mode, (box[2] - box[0], box[3] - box[1]), col)
                im = tmp
                box = (box[0], box[1])
            x, y = 0, 0
            if box is not None:
                x, y = int(box[0]), int(box[1])
            mh = mask._handle if isinstance(mask, Image) else -1
            _H('__vis_pil_paste__', self._handle, im._handle, x, y, int(mh))

        def save(self, fp, format=None, **kw):
            fmt = (format or '').upper()
            name = fp if isinstance(fp, str) else getattr(fp, 'name', '')
            if not fmt and isinstance(name, str) and '.' in name:
                fmt = name.rsplit('.', 1)[1].upper()
            if not fmt:
                fmt = 'PNG'
            b64 = _H('__vis_pil_save__', self._handle, fmt)
            data = base64.b64decode(b64)
            if isinstance(fp, str):
                with open(fp, 'wb') as f:
                    f.write(data)
            else:
                fp.write(data)

        def show(self, title=None, **kw):
            try:
                _H('__vis_pil_save_temp__', self._handle, 'PNG')
            except Exception:
                pass

        def tobytes(self, encoder_name='raw', *args):
            return base64.b64decode(_H('__vis_pil_tobytes__', self._handle))

        def getdata(self, band=None):
            raw = self.tobytes()
            if self.mode in ('L', '1', 'I', 'F', 'P'):
                data = list(raw)
            elif self.mode in ('RGBA', 'LA'):
                data = [tuple(raw[i:i + 4]) for i in range(0, len(raw), 4)]
            else:
                data = [tuple(raw[i:i + 3]) for i in range(0, len(raw), 3)]
            if band is not None:
                return [px[band] for px in data]
            return data

        def __array__(self, dtype=None):
            w, h = self._w, self._h
            data = self.getdata()
            if self.mode in ('L', '1', 'I', 'F', 'P'):
                rows = [list(data[y * w:(y + 1) * w]) for y in range(h)]
            else:
                rows = [[list(px) for px in data[y * w:(y + 1) * w]]
                        for y in range(h)]
            return rows

        def putdata(self, data, scale=1.0, offset=0.0):
            ba = bytearray()
            for px in data:
                if isinstance(px, (list, tuple)):
                    for c in px:
                        ba.append(int(c * scale + offset) & 255)
                else:
                    ba.append(int(px * scale + offset) & 255)
            b64 = base64.b64encode(bytes(ba)).decode('ascii')
            self._set(_H('__vis_pil_frombytes__', self.mode, self._w, self._h, b64))

        def point(self, lut, mode=None):
            if callable(lut):
                lut = [lut(i) for i in range(256)]
            lut = [int(v) for v in lut]
            if len(lut) < 256:
                lut = lut + [lut[-1] if lut else 0] * (256 - len(lut))
            return _wrap(_H('__vis_pil_point__', self._handle, lut[:256]))

        def histogram(self, mask=None, extrema=None):
            return [int(x) for x in _lst(_H('__vis_pil_histogram__', self._handle))]

        def getbbox(self, *a, **k):
            v = _H('__vis_pil_getbbox__', self._handle)
            if v is None:
                return None
            v = _lst(v)
            return (int(v[0]), int(v[1]), int(v[2]), int(v[3]))

        def getextrema(self):
            hist = self.histogram()
            out = []
            nb = len(hist) // 256
            for c in range(nb):
                band = hist[c * 256:(c + 1) * 256]
                lo = next((i for i in range(256) if band[i] > 0), 0)
                hi = next((i for i in range(255, -1, -1) if band[i] > 0), 0)
                out.append((lo, hi))
            return out[0] if len(out) == 1 else tuple(out)

        def getbands(self):
            return {'L': ('L',), '1': ('1',), 'I': ('I',), 'F': ('F',), 'P': ('P',),
                    'RGB': ('R', 'G', 'B'), 'RGBA': ('R', 'G', 'B', 'A'),
                    'LA': ('L', 'A')}.get(self.mode, ('L',))

        def split(self):
            res = _lst(_H('__vis_pil_split__', self._handle))
            return tuple(_wrap(m) for m in res)

        def getchannel(self, channel):
            bands = self.getbands()
            if isinstance(channel, str):
                channel = bands.index(channel)
            return self.split()[channel]

        def putalpha(self, alpha):
            rgb = self if self.mode == 'RGB' else self.convert('RGB')
            r, g, b = rgb.split()
            if isinstance(alpha, Image):
                a = alpha if alpha.mode == 'L' else alpha.convert('L')
            else:
                a = new('L', self.size, int(alpha))
            self._set(_H('__vis_pil_merge__', 'RGBA',
                         [r._handle, g._handle, b._handle, a._handle]))

        def filter(self, filt):
            if isinstance(filt, type):
                filt = filt()
            return filt.filter(self)

        def transform(self, size, method, data=None, resample=NEAREST, fill=1, fillcolor=None):
            if hasattr(method, 'method') and hasattr(method, 'data'):
                data = method.data
                method = method.method
            w, h = int(size[0]), int(size[1])
            fc = fillcolor
            if isinstance(fc, str):
                fc = list(_getrgb(fc))
            elif isinstance(fc, (list, tuple)):
                fc = [int(x) for x in fc]
            if method == EXTENT:
                x0, y0, x1, y1 = data
                sx = (x1 - x0) / float(w) if w else 1.0
                sy = (y1 - y0) / float(h) if h else 1.0
                return _wrap(_H('__vis_pil_transform__', self._handle, w, h, 'AFFINE',
                                [sx, 0.0, float(x0), 0.0, sy, float(y0)], fc))
            if method == AFFINE:
                return _wrap(_H('__vis_pil_transform__', self._handle, w, h, 'AFFINE',
                                [float(c) for c in data], fc))
            if method == PERSPECTIVE:
                return _wrap(_H('__vis_pil_transform__', self._handle, w, h, 'PERSPECTIVE',
                                [float(c) for c in data], fc))
            if method == QUAD:
                return _wrap(_H('__vis_pil_transform__', self._handle, w, h, 'PERSPECTIVE',
                                _quad_to_perspective(w, h, data), fc))
            if method == MESH:
                out = new(self.mode, (w, h), fillcolor if fillcolor is not None else 0)
                for box, quad in data:
                    bx0, by0, bx1, by1 = [int(v) for v in box]
                    bw, bh = bx1 - bx0, by1 - by0
                    if bw <= 0 or bh <= 0:
                        continue
                    coeffs = _quad_to_perspective(bw, bh, quad)
                    piece = _wrap(_H('__vis_pil_transform__', self._handle, bw, bh,
                                     'PERSPECTIVE', coeffs, fc))
                    out.paste(piece, (bx0, by0))
                return out
            raise ValueError('unsupported transform method: %r' % (method,))

        def reduce(self, factor, box=None):
            if isinstance(factor, (tuple, list)):
                fx, fy = int(factor[0]), int(factor[1])
            else:
                fx = fy = int(factor)
            nw = max(1, self._w // max(1, fx))
            nh = max(1, self._h // max(1, fy))
            return _wrap(_H('__vis_pil_resize__', self._handle, nw, nh, BOX))

        def alpha_composite(self, im, dest=(0, 0), source=(0, 0)):
            src = im
            if tuple(source) != (0, 0):
                if len(source) == 2:
                    sbox = (source[0], source[1], im._w, im._h)
                else:
                    sbox = source
                src = im.crop(sbox)
            self._set(_H('__vis_pil_alpha_composite__', self._handle, src._handle,
                         int(dest[0]), int(dest[1])))

        def entropy(self, mask=None, extrema=None):
            hist = self.histogram()
            total = float(sum(hist)) or 1.0
            ent = 0.0
            for c in hist:
                if c > 0:
                    p = c / total
                    ent -= p * math.log(p, 2)
            return ent

        def getprojection(self):
            w, h = self._w, self._h
            g = self if self.mode == 'L' else self.convert('L')
            data = g.getdata()
            xp = [0] * w
            yp = [0] * h
            for y in range(h):
                row = data[y * w:(y + 1) * w]
                for x in range(w):
                    if row[x]:
                        xp[x] = 1
                        yp[y] = 1
            return (xp, yp)

        def getcolors(self, maxcolors=256):
            counts = {}
            for px in self.getdata():
                counts[px] = counts.get(px, 0) + 1
                if len(counts) > maxcolors:
                    return None
            return [(v, k) for k, v in counts.items()]

        def getpalette(self, rawmode='RGB'):
            pal = getattr(self, '_palette', None)
            return list(pal) if pal else None

        def putpalette(self, data, rawmode='RGB'):
            self._palette = list(data)
            if self.mode not in ('P', 'L'):
                self.mode = 'P'

        def remap_palette(self, dest_map, source_palette=None):
            return self.copy()

        def quantize(self, colors=256, method=None, kmeans=0, palette=None, dither=1):
            try:
                return self.convert('P')
            except Exception:
                return self.copy()

        def apply_transparency(self):
            return None

        def draft(self, mode, size):
            return None

        def verify(self):
            return None

        def seek(self, frame):
            if frame != 0:
                raise EOFError('attempt to seek beyond the last frame')

        def tell(self):
            return 0

        @property
        def n_frames(self):
            return 1

        @property
        def is_animated(self):
            return False

        def effect_spread(self, distance):
            return self.copy()

        def frombytes(self, data, decoder_name='raw', *args):
            b64 = base64.b64encode(bytes(data)).decode('ascii')
            self._set(_H('__vis_pil_frombytes__', self.mode, self._w, self._h, b64))

        def resize_(self, *a, **k):
            return self.resize(*a, **k)

    # -- module-level Image constructors ------------------------------------
    def new(mode, size, color=0):
        w, h = size
        if isinstance(color, str):
            color = _getrgb(color)
        if color is None:
            fill = None
        elif isinstance(color, (list, tuple)):
            fill = [int(c) for c in color]
        else:
            fill = int(color)
        return _wrap(_H('__vis_pil_new__', str(mode), int(w), int(h), fill))

    def _sniff_format(data):
        if data[:8] == bytes([137, 80, 78, 71, 13, 10, 26, 10]):
            return 'PNG'
        if data[:3] == bytes([255, 216, 255]):
            return 'JPEG'
        if data[:6] in (b'GIF87a', b'GIF89a'):
            return 'GIF'
        if data[:2] == b'BM':
            return 'BMP'
        if data[:4] == b'RIFF' and data[8:12] == b'WEBP':
            return 'WEBP'
        if data[:2] in (b'II', b'MM'):
            return 'TIFF'
        return None

    def _open(fp, mode='r', formats=None):
        if hasattr(fp, 'read'):
            data = fp.read()
        elif isinstance(fp, (bytes, bytearray)):
            data = bytes(fp)
        else:
            with open(fp, 'rb') as f:
                data = f.read()
        b64 = base64.b64encode(bytes(data)).decode('ascii')
        _im = _wrap(_H('__vis_pil_open__', b64))
        _im.format = _sniff_format(bytes(data))
        return _im

    def frombytes(mode, size, data, decoder_name='raw', *args):
        w, h = size
        b64 = base64.b64encode(bytes(data)).decode('ascii')
        return _wrap(_H('__vis_pil_frombytes__', str(mode), int(w), int(h), b64))

    frombuffer = frombytes

    def merge(mode, bands):
        hs = [b._handle for b in bands]
        return _wrap(_H('__vis_pil_merge__', str(mode), hs))

    def blend(im1, im2, alpha):
        return _wrap(_H('__vis_pil_blend__', im1._handle, im2._handle, float(alpha)))

    def composite(image1, image2, mask):
        return _wrap(_H('__vis_pil_composite__', image1._handle, image2._handle, mask._handle))

    def fromarray(obj, mode=None):
        # minimal numpy-array support: obj is a nested list / has tolist()
        arr = obj.tolist() if hasattr(obj, 'tolist') else obj
        rows = list(arr)
        h = len(rows)
        first = list(rows[0])
        w = len(first)
        px0 = first[0]
        if isinstance(px0, (list, tuple)):
            nb = len(px0)
            m = mode or ('RGBA' if nb == 4 else 'RGB')
        else:
            nb = 1
            m = mode or 'L'
        ba = bytearray()
        for row in rows:
            for px in row:
                if isinstance(px, (list, tuple)):
                    for c in px:
                        ba.append(int(c) & 255)
                else:
                    ba.append(int(px) & 255)
        b64 = base64.b64encode(bytes(ba)).decode('ascii')
        return _wrap(_H('__vis_pil_frombytes__', m, w, h, b64))

    # -- Image submodule -----------------------------------------------------
    Image_mod = types.ModuleType('PIL.Image')
    Image_mod.Image = Image
    Image_mod.new = new
    Image_mod.open = _open
    Image_mod.frombytes = frombytes
    Image_mod.frombuffer = frombuffer
    Image_mod.merge = merge
    Image_mod.blend = blend
    Image_mod.composite = composite
    Image_mod.fromarray = fromarray

    def _module_alpha_composite(im1, im2, dest=None, source=None):
        return _wrap(_H('__vis_pil_alpha_composite__', im1._handle, im2._handle, 0, 0))

    def _module_eval(image, *args):
        return image.point(args[0])

    def _linear_gradient(mode='L'):
        ba = bytearray()
        for y in range(256):
            ba.extend([y] * 256)
        b64 = base64.b64encode(bytes(ba)).decode('ascii')
        g = _wrap(_H('__vis_pil_frombytes__', 'L', 256, 256, b64))
        return g if mode == 'L' else g.convert(mode)

    def _radial_gradient(mode='L'):
        ba = bytearray()
        for y in range(256):
            for x in range(256):
                d = math.hypot(x - 127.5, y - 127.5) / 127.5 * 255.0
                ba.append(min(255, int(d)))
        b64 = base64.b64encode(bytes(ba)).decode('ascii')
        g = _wrap(_H('__vis_pil_frombytes__', 'L', 256, 256, b64))
        return g if mode == 'L' else g.convert(mode)

    def _effect_noise(size, sigma):
        import random
        w, h = int(size[0]), int(size[1])
        ba = bytearray()
        for _ in range(w * h):
            ba.append(max(0, min(255, int(random.gauss(128, sigma)))))
        b64 = base64.b64encode(bytes(ba)).decode('ascii')
        return _wrap(_H('__vis_pil_frombytes__', 'L', w, h, b64))

    def _effect_mandelbrot(size, extent, quality):
        w, h = int(size[0]), int(size[1])
        x0, y0, x1, y1 = extent
        ba = bytearray()
        for py in range(h):
            for px in range(w):
                cx = x0 + (x1 - x0) * px / float(w)
                cy = y0 + (y1 - y0) * py / float(h)
                zx = zy = 0.0
                i = 0
                while zx * zx + zy * zy <= 4.0 and i < quality:
                    zx, zy = zx * zx - zy * zy + cx, 2.0 * zx * zy + cy
                    i += 1
                ba.append(int(255 * i / quality))
        b64 = base64.b64encode(bytes(ba)).decode('ascii')
        return _wrap(_H('__vis_pil_frombytes__', 'L', w, h, b64))

    _MODEBANDS = {'1': 1, 'L': 1, 'P': 1, 'I': 1, 'F': 1, 'RGB': 3, 'RGBA': 4,
                  'CMYK': 4, 'YCbCr': 3, 'LAB': 3, 'HSV': 3, 'LA': 2, 'RGBX': 4}
    _MODEBANDNAMES = {'1': ('1',), 'L': ('L',), 'P': ('P',), 'I': ('I',), 'F': ('F',),
                      'RGB': ('R', 'G', 'B'), 'RGBA': ('R', 'G', 'B', 'A'),
                      'LA': ('L', 'A'), 'CMYK': ('C', 'M', 'Y', 'K'),
                      'YCbCr': ('Y', 'Cb', 'Cr'), 'HSV': ('H', 'S', 'V')}

    Image_mod.alpha_composite = _module_alpha_composite
    Image_mod.eval = _module_eval
    Image_mod.linear_gradient = _linear_gradient
    Image_mod.radial_gradient = _radial_gradient
    Image_mod.effect_noise = _effect_noise
    Image_mod.effect_mandelbrot = _effect_mandelbrot
    Image_mod.getmodebands = lambda mode: _MODEBANDS.get(mode, 3)
    Image_mod.getmodebandnames = lambda mode: _MODEBANDNAMES.get(mode, ('R', 'G', 'B'))
    Image_mod.AFFINE = AFFINE
    Image_mod.EXTENT = EXTENT
    Image_mod.PERSPECTIVE = PERSPECTIVE
    Image_mod.QUAD = QUAD
    Image_mod.MESH = MESH
    Image_mod.NEAREST = NEAREST
    Image_mod.LANCZOS = LANCZOS
    Image_mod.ANTIALIAS = ANTIALIAS
    Image_mod.BILINEAR = BILINEAR
    Image_mod.BICUBIC = BICUBIC
    Image_mod.BOX = BOX
    Image_mod.HAMMING = HAMMING
    Image_mod.Resampling = Resampling
    Image_mod.FLIP_LEFT_RIGHT = FLIP_LEFT_RIGHT
    Image_mod.FLIP_TOP_BOTTOM = FLIP_TOP_BOTTOM
    Image_mod.ROTATE_90 = ROTATE_90
    Image_mod.ROTATE_180 = ROTATE_180
    Image_mod.ROTATE_270 = ROTATE_270
    Image_mod.TRANSPOSE = TRANSPOSE
    Image_mod.TRANSVERSE = TRANSVERSE
    Image_mod.Transpose = Transpose
    Image_mod.__version__ = '10.0-vis-java2d'

    # -- ImageDraw -----------------------------------------------------------
    class _Draw:
        def __init__(self, im, mode=None):
            self._im = im
            self.mode = im.mode

        def _col(self, c):
            if c is None:
                return None
            if isinstance(c, str):
                return list(_getrgb(c))
            if isinstance(c, (list, tuple)):
                return [int(x) for x in c]
            return int(c)

        def _flat(self, xy):
            xy = list(xy)
            if len(xy) == 0:
                return []
            if isinstance(xy[0], (int, float)):
                return [float(v) for v in xy]
            out = []
            for p in xy:
                out.append(float(p[0]))
                out.append(float(p[1]))
            return out

        def point(self, xy, fill=None):
            _H('__vis_pil_draw__', self._im._handle, 'point', self._flat(xy),
               {'fill': self._col(fill)})

        def line(self, xy, fill=None, width=1, joint=None):
            _H('__vis_pil_draw__', self._im._handle, 'line', self._flat(xy),
               {'fill': self._col(fill), 'width': int(width)})

        def rectangle(self, xy, fill=None, outline=None, width=1):
            _H('__vis_pil_draw__', self._im._handle, 'rectangle', self._flat(xy),
               {'fill': self._col(fill), 'outline': self._col(outline), 'width': int(width)})

        def rounded_rectangle(self, xy, radius=0, fill=None, outline=None, width=1, corners=None, **k):
            f = self._flat(xy)
            x0, y0, x1, y1 = f[0], f[1], f[2], f[3]
            r = min(float(radius), (x1 - x0) / 2.0, (y1 - y0) / 2.0)
            if r <= 0:
                self.rectangle((x0, y0, x1, y1), fill=fill, outline=outline, width=width)
                return
            d = 2 * r
            if fill is not None:
                self.rectangle((x0 + r, y0, x1 - r, y1), fill=fill)
                self.rectangle((x0, y0 + r, x1, y1 - r), fill=fill)
                self.pieslice((x0, y0, x0 + d, y0 + d), 180, 270, fill=fill)
                self.pieslice((x1 - d, y0, x1, y0 + d), 270, 360, fill=fill)
                self.pieslice((x0, y1 - d, x0 + d, y1), 90, 180, fill=fill)
                self.pieslice((x1 - d, y1 - d, x1, y1), 0, 90, fill=fill)
            if outline is not None and width > 0:
                self.line((x0 + r, y0, x1 - r, y0), fill=outline, width=width)
                self.line((x0 + r, y1, x1 - r, y1), fill=outline, width=width)
                self.line((x0, y0 + r, x0, y1 - r), fill=outline, width=width)
                self.line((x1, y0 + r, x1, y1 - r), fill=outline, width=width)
                self.arc((x0, y0, x0 + d, y0 + d), 180, 270, fill=outline, width=width)
                self.arc((x1 - d, y0, x1, y0 + d), 270, 360, fill=outline, width=width)
                self.arc((x0, y1 - d, x0 + d, y1), 90, 180, fill=outline, width=width)
                self.arc((x1 - d, y1 - d, x1, y1), 0, 90, fill=outline, width=width)

        def ellipse(self, xy, fill=None, outline=None, width=1):
            _H('__vis_pil_draw__', self._im._handle, 'ellipse', self._flat(xy),
               {'fill': self._col(fill), 'outline': self._col(outline), 'width': int(width)})

        def polygon(self, xy, fill=None, outline=None, width=1):
            _H('__vis_pil_draw__', self._im._handle, 'polygon', self._flat(xy),
               {'fill': self._col(fill), 'outline': self._col(outline), 'width': int(width)})

        def arc(self, xy, start, end, fill=None, width=1):
            _H('__vis_pil_draw__', self._im._handle, 'arc', self._flat(xy),
               {'fill': self._col(fill), 'start': float(start), 'end': float(end),
                'width': int(width)})

        def chord(self, xy, start, end, fill=None, outline=None, width=1):
            _H('__vis_pil_draw__', self._im._handle, 'chord', self._flat(xy),
               {'fill': self._col(fill), 'outline': self._col(outline),
                'start': float(start), 'end': float(end), 'width': int(width)})

        def pieslice(self, xy, start, end, fill=None, outline=None, width=1):
            _H('__vis_pil_draw__', self._im._handle, 'pieslice', self._flat(xy),
               {'fill': self._col(fill), 'outline': self._col(outline),
                'start': float(start), 'end': float(end), 'width': int(width)})

        def text(self, xy, text, fill=None, font=None, anchor=None, **kw):
            size = getattr(font, 'size', 12) if font is not None else 12
            _H('__vis_pil_draw__', self._im._handle, 'text',
               [float(xy[0]), float(xy[1])],
               {'text': str(text), 'fill': self._col(fill) if fill is not None else [0, 0, 0],
                'font_size': int(size)})

        def multiline_text(self, xy, text, fill=None, font=None, spacing=4, **kw):
            size = getattr(font, 'size', 12) if font is not None else 12
            x, y = float(xy[0]), float(xy[1])
            for i, line in enumerate(str(text).split(chr(10))):
                self.text((x, y + i * (size + spacing)), line, fill=fill, font=font)

        def textbbox(self, xy, text, font=None, **kw):
            size = getattr(font, 'size', 12) if font is not None else 12
            b = _lst(_H('__vis_pil_textbbox__', str(text), int(size)))
            x, y = xy[0], xy[1]
            return (x + b[0], y + b[1], x + b[2], y + b[3])

        def textlength(self, text, font=None, **kw):
            size = getattr(font, 'size', 12) if font is not None else 12
            b = _lst(_H('__vis_pil_textbbox__', str(text), int(size)))
            return b[2] - b[0]

        def textsize(self, text, font=None, **kw):
            size = getattr(font, 'size', 12) if font is not None else 12
            b = _lst(_H('__vis_pil_textbbox__', str(text), int(size)))
            return (b[2] - b[0], b[3] - b[1])

        def regular_polygon(self, bounding_circle, n_sides, rotation=0, fill=None, outline=None, width=1):
            bc = bounding_circle
            if len(bc) == 2 and isinstance(bc[0], (list, tuple)):
                cx, cy = bc[0]
                r = bc[1]
            else:
                cx, cy, r = bc[0], bc[1], bc[2]
            pts = []
            for i in range(int(n_sides)):
                ang = math.radians(float(rotation) - 90.0) + 2.0 * math.pi * i / float(n_sides)
                pts.append((cx + r * math.cos(ang), cy + r * math.sin(ang)))
            self.polygon(pts, fill=fill, outline=outline, width=width)

        def circle(self, xy, radius, fill=None, outline=None, width=1):
            x, y = xy[0], xy[1]
            self.ellipse((x - radius, y - radius, x + radius, y + radius),
                         fill=fill, outline=outline, width=width)

        def multiline_textbbox(self, xy, text, font=None, anchor=None, spacing=4, **kw):
            size = getattr(font, 'size', 12) if font is not None else 12
            x, y = float(xy[0]), float(xy[1])
            lines = str(text).split(chr(10))
            x0 = y0 = 1e18
            x1 = y1 = -1e18
            for i, line in enumerate(lines):
                bb = self.textbbox((x, y + i * (size + spacing)), line, font=font)
                x0 = min(x0, bb[0])
                y0 = min(y0, bb[1])
                x1 = max(x1, bb[2])
                y1 = max(y1, bb[3])
            return (x0, y0, x1, y1)

        def multiline_textsize(self, text, font=None, spacing=4, **kw):
            bb = self.multiline_textbbox((0, 0), text, font=font, spacing=spacing)
            return (bb[2] - bb[0], bb[3] - bb[1])

        def getfont(self):
            return _Font(12)

        def bitmap(self, xy, bitmap, fill=None):
            self._im.paste(bitmap, (int(xy[0]), int(xy[1])), bitmap)

    ImageDraw = types.ModuleType('PIL.ImageDraw')
    ImageDraw.ImageDraw = _Draw
    ImageDraw.Draw = lambda im, mode=None: _Draw(im, mode)

    def _floodfill(image, xy, value, border=None, thresh=0):
        x, y = int(xy[0]), int(xy[1])
        w, h = image.size
        if x < 0 or y < 0 or x >= w or y >= h:
            return
        px = image.load()
        bg = px[x, y]
        fill_val = _getrgb(value) if isinstance(value, str) else value
        if isinstance(fill_val, (list, tuple)):
            fill_val = tuple(int(c) for c in fill_val)
        bd = None
        if border is not None:
            bd = _getrgb(border) if isinstance(border, str) else border
            if isinstance(bd, (list, tuple)):
                bd = tuple(int(c) for c in bd)

        def _same(a, b):
            if isinstance(a, (list, tuple)):
                return all(abs(a[i] - b[i]) <= thresh for i in range(len(a)))
            return abs(a - b) <= thresh

        stack = [(x, y)]
        seen = set()
        while stack:
            cx, cy = stack.pop()
            if (cx, cy) in seen or cx < 0 or cy < 0 or cx >= w or cy >= h:
                continue
            seen.add((cx, cy))
            cur = px[cx, cy]
            if bd is None:
                if not _same(cur, bg):
                    continue
            else:
                if _same(cur, bd) or cur == fill_val:
                    continue
            px[cx, cy] = fill_val
            stack.extend([(cx + 1, cy), (cx - 1, cy), (cx, cy + 1), (cx, cy - 1)])

    ImageDraw.floodfill = _floodfill
    ImageDraw.getdraw = lambda im=None, hints=None: (_Draw(im) if im is not None else None, None)

    # -- ImageFont -----------------------------------------------------------
    class _Font:
        def __init__(self, size=10, name='SansSerif'):
            self.size = int(size)
            self.path = name

        def getbbox(self, text, *a, **k):
            b = _lst(_H('__vis_pil_textbbox__', str(text), int(self.size)))
            return (b[0], b[1], b[2], b[3])

        def getsize(self, text, *a, **k):
            b = _lst(_H('__vis_pil_textbbox__', str(text), int(self.size)))
            return (b[2] - b[0], b[3] - b[1])

        def getlength(self, text, *a, **k):
            b = _lst(_H('__vis_pil_textbbox__', str(text), int(self.size)))
            return b[2] - b[0]

    ImageFont = types.ModuleType('PIL.ImageFont')
    ImageFont.FreeTypeFont = _Font
    ImageFont.ImageFont = _Font
    ImageFont.truetype = lambda font=None, size=10, *a, **k: _Font(size, str(font) if font else 'SansSerif')
    ImageFont.load_default = lambda size=None: _Font(size or 10)
    ImageFont.load = lambda filename: _Font(10, str(filename))

    # -- ImageFilter ---------------------------------------------------------
    ImageFilter = types.ModuleType('PIL.ImageFilter')

    class _Kernel:
        name = 'Kernel'

        def __init__(self, size, kernel, scale=None, offset=0):
            self.size = size if isinstance(size, (tuple, list)) else (size, size)
            self.kernel = list(kernel)
            self.scale = scale if scale is not None else (sum(self.kernel) or 1)
            self.offset = offset

        def filter(self, image):
            s = self.size[0]
            m = _H('__vis_pil_conv__', image._handle, int(s),
                   [float(x) for x in self.kernel], float(self.scale), float(self.offset))
            return _wrap(m)

    class _BuiltinFilter(_Kernel):
        def __init__(self):
            _Kernel.__init__(self, self._size, self._kernel, self._scale, self._offset)

    def _mk_builtin(nm, size, scale, offset, kernel):
        return type(nm, (_BuiltinFilter,), {'name': nm, '_size': size, '_scale': scale,
                                            '_offset': offset, '_kernel': kernel})

    ImageFilter.Kernel = _Kernel
    ImageFilter.BuiltinFilter = _BuiltinFilter
    ImageFilter.BLUR = _mk_builtin('BLUR', (5, 5), 16, 0,
        [1, 1, 1, 1, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 1, 1, 1, 1])
    ImageFilter.SMOOTH = _mk_builtin('SMOOTH', (3, 3), 13, 0,
        [1, 1, 1, 1, 5, 1, 1, 1, 1])
    ImageFilter.SMOOTH_MORE = _mk_builtin('SMOOTH_MORE', (5, 5), 100, 0,
        [1, 1, 1, 1, 1, 1, 5, 5, 5, 1, 1, 5, 44, 5, 1, 1, 5, 5, 5, 1, 1, 1, 1, 1, 1])
    ImageFilter.SHARPEN = _mk_builtin('SHARPEN', (3, 3), 16, 0,
        [-2, -2, -2, -2, 32, -2, -2, -2, -2])
    ImageFilter.DETAIL = _mk_builtin('DETAIL', (3, 3), 6, 0,
        [0, -1, 0, -1, 10, -1, 0, -1, 0])
    ImageFilter.EDGE_ENHANCE = _mk_builtin('EDGE_ENHANCE', (3, 3), 2, 0,
        [-1, -1, -1, -1, 10, -1, -1, -1, -1])
    ImageFilter.EDGE_ENHANCE_MORE = _mk_builtin('EDGE_ENHANCE_MORE', (3, 3), 1, 0,
        [-1, -1, -1, -1, 9, -1, -1, -1, -1])
    ImageFilter.FIND_EDGES = _mk_builtin('FIND_EDGES', (3, 3), 1, 0,
        [-1, -1, -1, -1, 8, -1, -1, -1, -1])
    ImageFilter.EMBOSS = _mk_builtin('EMBOSS', (3, 3), 1, 128,
        [-1, 0, 0, 0, 1, 0, 0, 0, 0])
    ImageFilter.CONTOUR = _mk_builtin('CONTOUR', (3, 3), 1, 255,
        [-1, -1, -1, -1, 8, -1, -1, -1, -1])

    class GaussianBlur:
        name = 'GaussianBlur'

        def __init__(self, radius=2):
            self.radius = radius

        def filter(self, image):
            r = self.radius
            if isinstance(r, (tuple, list)):
                r = r[0]
            r = float(r)
            rad = max(1, int(math.ceil(r * 2)))
            sigma = r if r > 0 else 1e-6
            ker = []
            s = 0.0
            for j in range(-rad, rad + 1):
                for i in range(-rad, rad + 1):
                    v = math.exp(-(i * i + j * j) / (2 * sigma * sigma))
                    ker.append(v)
                    s += v
            size = 2 * rad + 1
            return _wrap(_H('__vis_pil_conv__', image._handle, size, ker, s, 0.0))

    class BoxBlur:
        name = 'BoxBlur'

        def __init__(self, radius=1):
            self.radius = radius

        def filter(self, image):
            r = self.radius
            if isinstance(r, (tuple, list)):
                r = r[0]
            rad = max(1, int(round(float(r))))
            size = 2 * rad + 1
            n = size * size
            ker = [1.0] * n
            return _wrap(_H('__vis_pil_conv__', image._handle, size, ker, float(n), 0.0))

    class _RankFilter:
        def __init__(self, size=3):
            self.size = size

        def _rank(self):
            return (self.size * self.size) // 2

        def filter(self, image):
            n = self.size
            return _wrap(_H('__vis_pil_rank__', image._handle, int(n), int(self._rank())))

    class MedianFilter(_RankFilter):
        name = 'MedianFilter'

    class MinFilter(_RankFilter):
        name = 'MinFilter'

        def _rank(self):
            return 0

    class MaxFilter(_RankFilter):
        name = 'MaxFilter'

        def _rank(self):
            return self.size * self.size - 1

    class ModeFilter(_RankFilter):
        name = 'ModeFilter'

        def _rank(self):
            return (self.size * self.size) // 2

    class UnsharpMask:
        name = 'UnsharpMask'

        def __init__(self, radius=2, percent=150, threshold=3):
            self.radius = radius
            self.percent = percent

        def filter(self, image):
            blurred = image.filter(GaussianBlur(self.radius))
            f = self.percent / 100.0
            # sharpened = image + f*(image - blurred) = blend(blurred, image, 1+f)
            return Image_mod.blend(blurred, image, 1.0 + f)

    ImageFilter.GaussianBlur = GaussianBlur
    ImageFilter.BoxBlur = BoxBlur
    ImageFilter.MedianFilter = MedianFilter
    ImageFilter.MinFilter = MinFilter
    ImageFilter.MaxFilter = MaxFilter
    ImageFilter.ModeFilter = ModeFilter
    ImageFilter.RankFilter = _RankFilter
    ImageFilter.UnsharpMask = UnsharpMask

    class MultibandFilter(_Kernel):
        pass

    class Color3DLUT:
        name = 'Color3DLUT'

        def __init__(self, size, table, channels=3, target_mode=None, **k):
            self.size = size if isinstance(size, (tuple, list)) else (size, size, size)
            self.table = list(table)
            self.channels = channels
            self.mode = target_mode

        @classmethod
        def generate(cls, size, callback, channels=3, target_mode=None):
            sz = size if isinstance(size, (tuple, list)) else (size, size, size)
            sr, sg, sb = sz
            table = []
            for b in range(sb):
                for g in range(sg):
                    for r in range(sr):
                        table.extend(callback(r / (sr - 1 or 1), g / (sg - 1 or 1), b / (sb - 1 or 1)))
            return cls(sz, table, channels, target_mode)

        def filter(self, image):
            sr, sg, sb = self.size
            data = image.convert('RGB').getdata()
            out = bytearray()
            tbl = self.table
            for px in data:
                ir = int(px[0] / 255.0 * (sr - 1) + 0.5)
                ig = int(px[1] / 255.0 * (sg - 1) + 0.5)
                ib = int(px[2] / 255.0 * (sb - 1) + 0.5)
                idx = (ib * sg * sr + ig * sr + ir) * self.channels
                for c in range(3):
                    out.append(max(0, min(255, int(round(tbl[idx + c] * 255.0)))))
            b64 = base64.b64encode(bytes(out)).decode('ascii')
            return _wrap(_H('__vis_pil_frombytes__', 'RGB', image.size[0], image.size[1], b64))

    ImageFilter.MultibandFilter = MultibandFilter
    ImageFilter.Color3DLUT = Color3DLUT

    # -- ImageChops ----------------------------------------------------------
    ImageChops = types.ModuleType('PIL.ImageChops')

    def _chop(op):
        return lambda a, b: _wrap(_H('__vis_pil_chop__', op, a._handle, b._handle))

    ImageChops.difference = _chop('difference')
    ImageChops.add = lambda a, b, scale=1.0, offset=0: _wrap(_H('__vis_pil_chop__', 'add', a._handle, b._handle))
    ImageChops.subtract = lambda a, b, scale=1.0, offset=0: _wrap(_H('__vis_pil_chop__', 'subtract', a._handle, b._handle))
    ImageChops.multiply = _chop('multiply')
    ImageChops.screen = _chop('screen')
    ImageChops.lighter = _chop('lighter')
    ImageChops.darker = _chop('darker')
    ImageChops.add_modulo = _chop('add_modulo')
    ImageChops.subtract_modulo = _chop('subtract_modulo')
    ImageChops.logical_and = _chop('logical_and')
    ImageChops.logical_or = _chop('logical_or')
    ImageChops.logical_xor = _chop('logical_xor')
    ImageChops.lighter = _chop('lighter')
    ImageChops.invert = lambda image: image.point([255 - i for i in range(256)])
    ImageChops.duplicate = lambda image: image.copy()
    ImageChops.constant = lambda image, value: new('L', image.size, value)
    ImageChops.overlay = _chop('overlay')
    ImageChops.soft_light = _chop('soft_light')
    ImageChops.hard_light = _chop('hard_light')
    ImageChops.blend = lambda im1, im2, alpha: blend(im1, im2, alpha)
    ImageChops.composite = lambda im1, im2, mask: composite(im1, im2, mask)

    def _chops_offset(image, xoffset, yoffset=None):
        if yoffset is None:
            yoffset = xoffset
        return _wrap(_H('__vis_pil_offset__', image._handle, int(xoffset), int(yoffset)))

    ImageChops.offset = _chops_offset

    # -- ImageOps ------------------------------------------------------------
    ImageOps = types.ModuleType('PIL.ImageOps')

    def _ops_grayscale(image):
        return image.convert('L')

    def _ops_invert(image):
        return image.point([255 - i for i in range(256)])

    def _ops_mirror(image):
        return image.transpose(FLIP_LEFT_RIGHT)

    def _ops_flip(image):
        return image.transpose(FLIP_TOP_BOTTOM)

    def _ops_posterize(image, bits):
        mask = (~(2 ** (8 - bits) - 1)) & 255
        return image.point([i & mask for i in range(256)])

    def _ops_solarize(image, threshold=128):
        return image.point([(255 - i if i >= threshold else i) for i in range(256)])

    def _ops_autocontrast(image, cutoff=0, ignore=None, mask=None, preserve_tone=False):
        hist = image.convert('L').histogram()
        lo = next((i for i in range(256) if hist[i] > 0), 0)
        hi = next((i for i in range(255, -1, -1) if hist[i] > 0), 255)
        if hi <= lo:
            return image.copy()
        scale = 255.0 / (hi - lo)
        lut = [max(0, min(255, int(round((i - lo) * scale)))) for i in range(256)]
        return image.point(lut)

    def _ops_expand(image, border=0, fill=0):
        if isinstance(border, int):
            l = t = r = b = border
        elif len(border) == 2:
            l = r = border[0]
            t = b = border[1]
        else:
            l, t, r, b = border
        w, h = image.size
        out = new(image.mode, (w + l + r, h + t + b), fill)
        out.paste(image, (l, t))
        return out

    def _ops_fit(image, size, method=BICUBIC, bleed=0.0, centering=(0.5, 0.5)):
        w, h = size
        iw, ih = image.size
        scale = max(w / float(iw), h / float(ih))
        nw, nh = int(round(iw * scale)), int(round(ih * scale))
        tmp = image.resize((nw, nh), method)
        left = int((nw - w) * centering[0])
        top = int((nh - h) * centering[1])
        return tmp.crop((left, top, left + w, top + h))

    def _ops_pad(image, size, method=BICUBIC, color=None, centering=(0.5, 0.5)):
        w, h = size
        iw, ih = image.size
        scale = min(w / float(iw), h / float(ih))
        nw, nh = int(round(iw * scale)), int(round(ih * scale))
        tmp = image.resize((nw, nh), method)
        out = new(image.mode, (w, h), color if color is not None else 0)
        left = int((w - nw) * centering[0])
        top = int((h - nh) * centering[1])
        out.paste(tmp, (left, top))
        return out

    def _ops_contain(image, size, method=BICUBIC):
        w, h = size
        iw, ih = image.size
        scale = min(w / float(iw), h / float(ih), 1.0)
        return image.resize((max(1, int(iw * scale)), max(1, int(ih * scale))), method)

    def _ops_cover(image, size, method=BICUBIC):
        w, h = size
        iw, ih = image.size
        scale = max(w / float(iw), h / float(ih))
        return image.resize((max(1, int(iw * scale)), max(1, int(ih * scale))), method)

    def _ops_scale(image, factor, resample=BICUBIC):
        w, h = image.size
        return image.resize((max(1, int(round(w * factor))), max(1, int(round(h * factor)))), resample)

    def _ops_colorize(image, black, white, mid=None, blackpoint=0, whitepoint=255, midpoint=127):
        black = _getrgb(black) if isinstance(black, str) else black
        white = _getrgb(white) if isinstance(white, str) else white
        l = image if image.mode == 'L' else image.convert('L')
        rl = [int(black[0] + (white[0] - black[0]) * i / 255.0) for i in range(256)]
        gl = [int(black[1] + (white[1] - black[1]) * i / 255.0) for i in range(256)]
        bl = [int(black[2] + (white[2] - black[2]) * i / 255.0) for i in range(256)]
        return merge('RGB', (l.point(rl), l.point(gl), l.point(bl)))

    def _ops_equalize(image, mask=None):
        return _ops_autocontrast(image)

    ImageOps.grayscale = _ops_grayscale
    ImageOps.invert = _ops_invert
    ImageOps.mirror = _ops_mirror
    ImageOps.flip = _ops_flip
    ImageOps.posterize = _ops_posterize
    ImageOps.solarize = _ops_solarize
    ImageOps.autocontrast = _ops_autocontrast
    ImageOps.equalize = _ops_equalize
    ImageOps.expand = _ops_expand
    ImageOps.fit = _ops_fit
    ImageOps.pad = _ops_pad
    ImageOps.contain = _ops_contain
    ImageOps.cover = _ops_cover
    ImageOps.scale = _ops_scale
    ImageOps.colorize = _ops_colorize

    def _ops_crop(image, border=0):
        if isinstance(border, int):
            l = t = r = b = border
        elif len(border) == 2:
            l = r = border[0]
            t = b = border[1]
        else:
            l, t, r, b = border
        w, h = image.size
        return image.crop((l, t, w - r, h - b))

    def _ops_exif_transpose(image, in_place=False):
        # No EXIF metadata is carried by this shim, so this is an identity op.
        if in_place:
            return None
        return image.copy()

    def _ops_deform(image, deformer, resample=BICUBIC):
        # Apply a deformer via its getmesh(image) -> [(box, quad), ...] using MESH.
        mesh = deformer.getmesh(image)
        return image.transform(image.size, MESH, mesh, resample)

    ImageOps.crop = _ops_crop
    ImageOps.exif_transpose = _ops_exif_transpose
    ImageOps.deform = _ops_deform

    # -- ImageEnhance --------------------------------------------------------
    ImageEnhance = types.ModuleType('PIL.ImageEnhance')

    class _Enhance:
        def enhance(self, factor):
            return Image_mod.blend(self.degenerate, self.image, factor)

    class Color(_Enhance):
        def __init__(self, image):
            self.image = image
            self.degenerate = image.convert('L').convert(image.mode)

    class Contrast(_Enhance):
        def __init__(self, image):
            self.image = image
            gray = image.convert('L')
            hist = gray.histogram()
            n = sum(hist) or 1
            mean = int(round(sum(i * hist[i] for i in range(256)) / n))
            self.degenerate = new(image.mode, image.size, mean)

    class Brightness(_Enhance):
        def __init__(self, image):
            self.image = image
            self.degenerate = new(image.mode, image.size, 0)

    class Sharpness(_Enhance):
        def __init__(self, image):
            self.image = image
            self.degenerate = image.filter(ImageFilter.SMOOTH)

    ImageEnhance.Color = Color
    ImageEnhance.Contrast = Contrast
    ImageEnhance.Brightness = Brightness
    ImageEnhance.Sharpness = Sharpness

    # -- ImageStat -----------------------------------------------------------
    ImageStat = types.ModuleType('PIL.ImageStat')

    class _Stat:
        def __init__(self, image_or_list, mask=None):
            if isinstance(image_or_list, list):
                self.h = image_or_list
            else:
                self.h = image_or_list.histogram(mask)
            self.bands = list(range(max(1, len(self.h) // 256)))

        def _band(self, b):
            return self.h[b * 256:(b + 1) * 256]

        @property
        def count(self):
            return [sum(self._band(b)) for b in self.bands]

        @property
        def sum(self):
            return [float(sum(i * self._band(b)[i] for i in range(256))) for b in self.bands]

        @property
        def sum2(self):
            return [float(sum(i * i * self._band(b)[i] for i in range(256))) for b in self.bands]

        @property
        def mean(self):
            c = self.count
            s = self.sum
            return [s[b] / (c[b] or 1) for b in self.bands]

        @property
        def median(self):
            out = []
            for b in self.bands:
                hb = self._band(b)
                half = sum(hb) // 2
                acc = 0
                med = 0
                for i in range(256):
                    acc += hb[i]
                    if acc > half:
                        med = i
                        break
                out.append(med)
            return out

        @property
        def rms(self):
            c = self.count
            s2 = self.sum2
            return [math.sqrt(s2[b] / (c[b] or 1)) for b in self.bands]

        @property
        def var(self):
            c = self.count
            s2 = self.sum2
            mn = self.mean
            return [(s2[b] / (c[b] or 1)) - mn[b] ** 2 for b in self.bands]

        @property
        def stddev(self):
            return [math.sqrt(max(0.0, v)) for v in self.var]

        @property
        def extrema(self):
            out = []
            for b in self.bands:
                hb = self._band(b)
                lo = next((i for i in range(256) if hb[i] > 0), 0)
                hi = next((i for i in range(255, -1, -1) if hb[i] > 0), 0)
                out.append((lo, hi))
            return out

    ImageStat.Stat = _Stat

    # -- ImageMath -----------------------------------------------------------
    ImageMath = types.ModuleType('PIL.ImageMath')

    class _Operand:
        def __init__(self, im):
            self.im = im

        def _bin(self, other, fn_img, fn_scalar):
            o = other.im if isinstance(other, _Operand) else other
            if hasattr(o, '_handle'):
                return _Operand(fn_img(self.im, o))
            return _Operand(self.im.point(lambda i: fn_scalar(i, o)))

        def __add__(self, o):
            return self._bin(o, lambda a, b: ImageChops.add(a, b), lambda i, v: i + v)

        def __radd__(self, o):
            return self.__add__(o)

        def __sub__(self, o):
            return self._bin(o, lambda a, b: ImageChops.subtract(a, b), lambda i, v: i - v)

        def __mul__(self, o):
            return self._bin(o, lambda a, b: ImageChops.multiply(a, b), lambda i, v: i * v)

        def __rmul__(self, o):
            return self.__mul__(o)

        def __and__(self, o):
            return self._bin(o, lambda a, b: ImageChops.logical_and(a, b), lambda i, v: i & int(v))

        def __or__(self, o):
            return self._bin(o, lambda a, b: ImageChops.logical_or(a, b), lambda i, v: i | int(v))

    def _imagemath_eval(expression, _dict=None, **kw):
        env = {}
        if _dict:
            env.update(_dict)
        env.update(kw)
        operands = {}
        for k, v in env.items():
            operands[k] = _Operand(v) if hasattr(v, '_handle') else v

        def _convert(op, mode):
            im = op.im if isinstance(op, _Operand) else op
            return _Operand(im.convert(mode))

        operands['convert'] = _convert
        operands['float'] = lambda op: op
        operands['int'] = lambda op: op
        operands['abs'] = abs
        operands['min'] = min
        operands['max'] = max
        result = eval(expression, {'__builtins__': {}}, operands)
        return result.im if isinstance(result, _Operand) else result

    ImageMath.eval = _imagemath_eval
    ImageMath.lambda_eval = _imagemath_eval
    ImageMath.unsafe_eval = _imagemath_eval

    # -- ImageSequence -------------------------------------------------------
    ImageSequence = types.ModuleType('PIL.ImageSequence')

    class _SeqIterator:
        def __init__(self, im):
            self.im = im
            self.pos = 0

        def __iter__(self):
            return self

        def __next__(self):
            if self.pos > 0:
                raise StopIteration
            self.pos += 1
            return self.im

        def __getitem__(self, ix):
            if ix == 0:
                return self.im
            raise IndexError('no such frame')

    ImageSequence.Iterator = _SeqIterator
    ImageSequence.all_frames = lambda im, func=None: [func(im) if func else im]

    # -- ImagePalette --------------------------------------------------------
    ImagePalette = types.ModuleType('PIL.ImagePalette')

    class _Palette:
        def __init__(self, mode='RGB', palette=None, size=0):
            self.mode = mode
            self.palette = list(palette) if palette is not None else [i for i in range(256) for _ in range(3)]

        def getdata(self):
            return (self.mode, bytes(bytearray(int(x) & 255 for x in self.palette)))

        def tobytes(self):
            return bytes(bytearray(int(x) & 255 for x in self.palette))

        def getcolor(self, color, image=None):
            return _getrgb(color)[0]

    ImagePalette.ImagePalette = _Palette

    # -- ImageTransform ------------------------------------------------------
    ImageTransform = types.ModuleType('PIL.ImageTransform')

    class _Transform:
        def __init__(self, data):
            self.data = data

        def getdata(self):
            return (self.method, self.data)

    class AffineTransform(_Transform):
        method = AFFINE

    class ExtentTransform(_Transform):
        method = EXTENT

    class PerspectiveTransform(_Transform):
        method = PERSPECTIVE

    class QuadTransform(_Transform):
        method = QUAD

    class MeshTransform(_Transform):
        method = MESH

    ImageTransform.Transform = _Transform
    ImageTransform.AffineTransform = AffineTransform
    ImageTransform.ExtentTransform = ExtentTransform
    ImageTransform.PerspectiveTransform = PerspectiveTransform
    ImageTransform.QuadTransform = QuadTransform
    ImageTransform.MeshTransform = MeshTransform

    # -- PIL.features --------------------------------------------------------
    features = types.ModuleType('PIL.features')
    _FEATURES = {'jpg': True, 'zlib': True, 'libjpeg_turbo': False, 'freetype2': True,
                 'raqm': False, 'webp': False, 'transp_webp': False, 'jpg_2000': False}
    features.check = lambda feature: bool(_FEATURES.get(feature, False))
    features.check_feature = features.check
    features.check_codec = lambda feature: True
    features.check_module = lambda module: True
    features.version = lambda feature: None
    features.version_feature = lambda feature: None
    features.version_codec = lambda feature: None
    features.version_module = lambda module: None
    features.get_supported = lambda: ['jpg', 'zlib', 'freetype2']
    features.get_supported_modules = lambda: ['freetype2']
    features.get_supported_codecs = lambda: ['jpg', 'zlib']
    features.get_supported_features = lambda: []
    features.pilinfo = lambda out=None, supported_formats=True: None

    # -- ExifTags / TiffTags -------------------------------------------------
    ExifTags = types.ModuleType('PIL.ExifTags')
    ExifTags.TAGS = {256: 'ImageWidth', 257: 'ImageLength', 258: 'BitsPerSample',
                     259: 'Compression', 262: 'PhotometricInterpretation',
                     271: 'Make', 272: 'Model', 274: 'Orientation',
                     277: 'SamplesPerPixel', 282: 'XResolution', 283: 'YResolution',
                     296: 'ResolutionUnit', 305: 'Software', 306: 'DateTime',
                     315: 'Artist', 316: 'HostComputer', 33432: 'Copyright',
                     34665: 'ExifOffset', 36867: 'DateTimeOriginal',
                     37377: 'ShutterSpeedValue', 37378: 'ApertureValue',
                     37386: 'FocalLength', 40962: 'PixelXDimension',
                     40963: 'PixelYDimension'}
    ExifTags.GPSTAGS = {0: 'GPSVersionID', 1: 'GPSLatitudeRef', 2: 'GPSLatitude',
                        3: 'GPSLongitudeRef', 4: 'GPSLongitude', 5: 'GPSAltitudeRef',
                        6: 'GPSAltitude', 7: 'GPSTimeStamp'}

    class _TagEnum:
        pass

    ExifTags.Base = _TagEnum
    ExifTags.GPS = _TagEnum
    ExifTags.Interop = _TagEnum
    ExifTags.IFD = _TagEnum
    ExifTags.LightSource = _TagEnum

    TiffTags = types.ModuleType('PIL.TiffTags')
    TiffTags.TAGS = dict(ExifTags.TAGS)
    TiffTags.TAGS_V2 = {}
    TiffTags.lookup = lambda tag, group=None: None

    # -- ImageMorph (module presence) ----------------------------------------
    ImageMorph = types.ModuleType('PIL.ImageMorph')

    # -- assemble the PIL package -------------------------------------------
    PIL = types.ModuleType('PIL')
    PIL.__doc__ = 'vis Pillow-compatible shim backed by the JVM Java2D / ImageIO stack.'
    PIL.__version__ = '10.0-vis-java2d'
    PIL.Image = Image_mod
    PIL.ImageDraw = ImageDraw
    PIL.ImageFilter = ImageFilter
    PIL.ImageOps = ImageOps
    PIL.ImageColor = ImageColor
    PIL.ImageEnhance = ImageEnhance
    PIL.ImageChops = ImageChops
    PIL.ImageFont = ImageFont
    PIL.ImageStat = ImageStat
    PIL.ImageMath = ImageMath
    PIL.ImageSequence = ImageSequence
    PIL.ImagePalette = ImagePalette
    PIL.ImageTransform = ImageTransform
    PIL.features = features
    PIL.ExifTags = ExifTags
    PIL.TiffTags = TiffTags
    PIL.ImageMorph = ImageMorph
    PIL.__all__ = ['Image', 'ImageDraw', 'ImageFilter', 'ImageOps', 'ImageColor',
                   'ImageEnhance', 'ImageChops', 'ImageFont', 'ImageStat', 'ImageMath',
                   'ImageSequence', 'ImagePalette', 'ImageTransform', 'features',
                   'ExifTags', 'TiffTags', 'ImageMorph']

    sys.modules['PIL'] = PIL
    sys.modules['PIL.Image'] = Image_mod
    sys.modules['PIL.ImageDraw'] = ImageDraw
    sys.modules['PIL.ImageFilter'] = ImageFilter
    sys.modules['PIL.ImageOps'] = ImageOps
    sys.modules['PIL.ImageColor'] = ImageColor
    sys.modules['PIL.ImageEnhance'] = ImageEnhance
    sys.modules['PIL.ImageChops'] = ImageChops
    sys.modules['PIL.ImageFont'] = ImageFont
    sys.modules['PIL.ImageStat'] = ImageStat
    sys.modules['PIL.ImageMath'] = ImageMath
    sys.modules['PIL.ImageSequence'] = ImageSequence
    sys.modules['PIL.ImagePalette'] = ImagePalette
    sys.modules['PIL.ImageTransform'] = ImageTransform
    sys.modules['PIL.features'] = features
    sys.modules['PIL.ExifTags'] = ExifTags
    sys.modules['PIL.TiffTags'] = TiffTags
    sys.modules['PIL.ImageMorph'] = ImageMorph

    # Autoload: staple onto builtins so PIL.Image / Image.new work in every
    # run_python block WITHOUT an explicit import (mirrors json/yaml/matplotlib).
    try:
        import builtins as _b
        _b.PIL = PIL
        _b.Image = Image_mod
        _b.ImageDraw = ImageDraw
        _b.ImageFilter = ImageFilter
        _b.ImageOps = ImageOps
        _b.ImageColor = ImageColor
        _b.ImageEnhance = ImageEnhance
        _b.ImageChops = ImageChops
        _b.ImageFont = ImageFont
        _b.ImageStat = ImageStat
        _b.ImageMath = ImageMath
        _b.ImageSequence = ImageSequence
        _b.ImagePalette = ImagePalette
        _b.ImageTransform = ImageTransform
        _b.ImageMorph = ImageMorph
    except Exception:
        pass

__vis_install_pil__()
del __vis_install_pil__
")

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
       :shim/description
       "Pillow-compatible `PIL` (Image/ImageDraw/ImageFilter/ImageOps/ImageColor/ImageEnhance/ImageChops/ImageFont) backed by JVM Java2D/ImageIO. Not supported: some color-mode conversions and `Image.transform` methods raise `ValueError`."
       :shim/bindings pil-bridge-bindings
       :shim/preamble pil-shim-src}]}))

(vis/register-extension! vis-extension)
