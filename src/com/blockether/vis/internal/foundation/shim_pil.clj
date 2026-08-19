(ns com.blockether.vis.internal.foundation.shim-pil
  "Built-in sandbox SHIM: a Pillow (PIL)-compatible `PIL` package for the model's
   Python sandbox, backed by the `com.blockether/imaging` FFM stack (Rust
   `image` + `resvg` + `tiny-skia`). NOTHING here touches `java.desktop`: no
   CPython Pillow wheel ships in the sandbox, and no Java2D/ImageIO either; this
   extension contributes a
   `:ext/sandbox-shims` entry that `env-python/build-agent-context` installs into
   every sandbox Context: the host bridge callables
   are wired onto the globals, then the Python preamble publishes a `PIL` package
   (with `Image`, `ImageDraw`, `ImageFilter`, `ImageOps`, `ImageColor`,
   `ImageEnhance`, `ImageChops`, `ImageFont`, `ImageMath` submodules) into
   `sys.modules` (so `from PIL import Image` works) and staples them onto builtins.

   Images live HOST-side as `Raster`s -- a plain packed-0xAARRGGBB `int[]` plus
   width/height -- in a per-SESSION handle table keyed by an integer; the Python
   `Image` object is a thin handle wrapper. All pixel ops, drawing, filtering,
   geometry and codec work happen on the host; only small metadata vectors and
   base64 blobs cross the strings-only boundary. Codecs, resampling, rotation and
   vector drawing go through `com.blockether.imaging`; per-pixel algorithms stay
   in Clojure over the raster. Reuses `mpl-capture/record-attachment!` so
   `Image.show()` surfaces the image inline as a session attachment."
  (:require [clojure.string :as str]
            [com.blockether.imaging :as im]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.internal.sandbox-resources :as res]
            [com.blockether.vis.internal.foundation.gif :as gif]
            [com.blockether.vis.internal.foundation.mpl-capture :as mpl-capture])
  (:import [java.util Base64]))

;; Images live on the JVM as {:img Raster :mode String}; the guest holds integer
;; handles. The table, the cap and the release are declared in `:shim/resources`.
;; The Python Image is just a handle; the pixels stay on the host.

;; Live draw runs: handle -> the cdylib image a run of consecutive ImageDraw
;; ops shares, plus the ops queued against it. Converting the whole canvas into
;; the cdylib and back on EVERY op is O(pixels) per op -- and so is every
;; `im/draw!` call itself (~1.4 ms on 800x600, whatever the shape is), because
;; the canvas round-trips through the renderer per call. So a run of N ops keeps
;; ONE live image AND crosses as ONE batch; `entry` flushes both before any
;; other op observes the pixels.
(defonce ^:private live-draws (atom {}))

(defonce ^:private pending-draws (atom {}))

(def ^:private max-pending-draws
  "Queued ops that force a flush -- bounds what one unread draw loop can pin."
  4096)

(declare flush-draws! take-draw! run-draws!)

;; The raster. `getRGB`/`setRGB`/`getWidth`/`getHeight` keep exactly the
;; BufferedImage shape the pixel algorithms below were written against:
;; `getRGB` hands back an UNSIGNED packed 0xAARRGGBB value, and `amask` forces
;; alpha to 255 for the opaque modes, mirroring TYPE_INT_RGB.

(definterface IRaster
              (^long getWidth [])
              (^long getHeight [])
              (^long getRGB [^long x ^long y])
              (setRGB [^long x ^long y ^long v]))

(deftype Raster [^ints px ^long w ^long h ^long amask]
  IRaster
    (getWidth [_] w)
    (getHeight [_] h)
    (getRGB [_ x y] (bit-or amask (bit-and 0xffffffff (aget px (int (+ (* y w) x))))))
    (setRGB [_ x y v] (aset px (int (+ (* y w) x)) (unchecked-int v)) nil))

(def ^:dynamic *scope*
  "Scope of the Context whose guest is inside a host image op, bound once by
   [[pil-envelope]]. Dynamic rather than an argument because `put-img!` is
   reached from ~28 op sites, and threading a scope through all of them would
   bury the one thing that matters — that an image belongs to the session that
   made it — in mechanical churn."
  nil)

(defn- put-img!
  "Register `img` under mode string, returning its new integer handle.

   The handle is OWNED by the current scope, so an image the guest never
   `close()`s is still freed when its Context is disposed. A decoded image is an
   on-heap `int[]` of w*h*4 bytes — 48 MB for one 4000x3000 frame — so an
   unclosed image outliving its session is the single most expensive leak this
   shim can produce."
  [^Raster img mode]
  (res/open! *scope* ::images {:img img :mode mode}))

(defn- raw-entry
  "Registry entry WITHOUT flushing an in-flight draw run. Only the flush path
   itself may read an entry this way; anything else would observe stale pixels."
  [h]
  (res/value ::images h))

(defn- entry
  "The registry entry for a LIVE handle, its drawing flushed. A handle with no
   entry was closed -- or freed once the last Python `Image` holding it became
   unreachable -- and the Java NPE that escaped from its missing raster named
   nothing the caller could act on."
  [h]
  (flush-draws! h)
  (or (raw-entry h)
      (throw (IllegalArgumentException.
               (str "PIL image handle " (long h)
                    " is not live: it was closed, or freed after the last reference"
                    " to it was dropped")))))

(defn- free-img!
  "Free ONE image. Also the scope releaser, so it stays idempotent and silent for
   a handle that is already gone."
  [h]
  (swap! pending-draws dissoc (long h))
  (some-> (take-draw! h)
          im/close!)
  (res/close! ::images h)
  nil)

;; Pixel / colour helpers. Pixels are handled as packed 0xAARRGGBB longs.

(defn- ch ^long [^long p ^long sh] (bit-and (bit-shift-right p sh) 0xff))

(defn- argb
  ^long [a r g b]
  (bit-or (bit-shift-left (bit-and (long a) 0xff) 24)
          (bit-shift-left (bit-and (long r) 0xff) 16)
          (bit-shift-left (bit-and (long g) 0xff) 8)
          (bit-and (long b) 0xff)))

(defn- clamp255 ^long [^double v] (long (min 255 (max 0 (Math/round v)))))

(defn- gray-argb ^long [v] (argb 255 v v v))

(defn- alpha-mode?
  "Modes stored WITH an alpha channel. Grayscale-family modes keep the gray value
   replicated across R/G/B at full alpha, so an 'L' pixel round-trips its sRGB
   byte value untouched."
  [mode]
  (contains? #{"RGBA" "LA"} (str mode)))

(defn- mask-band
  "Which byte of a mask raster a paste reads: the ALPHA band of an 'RGBA'/'LA'
   mask, the gray VALUE of an 'L' or bitmap mask -- PIL's own rule. Reading blue
   instead blended `im.paste(im, box, im)` -- the idiom for dropping a
   transparent PNG onto a canvas -- by the source's own BLUENESS, so gold
   (253,198,80) came out cream over white and brown over charcoal."
  ^long [mode]
  (if (alpha-mode? mode) 24 0))

(defn- band-shifts
  "Which byte of a packed 0xAARRGGBB pixel each of a mode's PILLOW BANDS occupies,
   in band order. Storage is always four components; the bands a mode ANSWERS are
   Pillow's own, so 'LA' is (gray, alpha) -- TWO -- and every op that reports bands
   (`getpixel`, `tobytes`, `split`, `histogram`) counts them here instead of
   lumping 'LA' in with 'RGBA'. 'CMYK' answers FOUR, its K ink in the fourth byte."
  [mode]
  (case (str mode)
    ("L" "1" "I" "F" "P")
    [16]

    "LA"
    [16 24]

    ("RGBA" "CMYK")
    [16 8 0 24]

    [16 8 0]))
(defn- mask-at
  "One mask sample, 0-255. A '1' mask is BOOLEAN -- any non-zero copies the
   source pixel whole -- every other mode reads `band` straight."
  [^Raster m band bitmap? x y]
  (let [v (ch (.getRGB m (long x) (long y)) (long band))]
    (if bitmap? (if (zero? v) 0 255) v)))
(defn- four-band?
  "Modes whose FOURTH stored byte carries DATA -- alpha for 'RGBA'/'LA', the K ink
   for 'CMYK'. Their rasters start zeroed and keep no opaque alpha mask, so that
   byte reads back exactly as it was written."
  [mode]
  (or (alpha-mode? mode) (= "CMYK" (str mode))))

(defn- new-raster
  "A blank raster for `mode`: transparent for the alpha modes, every ink zero (white)
   for 'CMYK', opaque black otherwise -- the same starting state
   `new BufferedImage(...)` used to give."
  ^Raster [mode w h]
  (let [w
        (long w)

        h
        (long h)

        a?
        (four-band? mode)]

    (Raster. (int-array (* w h) (if a? (int 0) (unchecked-int 0xff000000)))
             w
             h
             (if a? 0 0xff000000))))

(defn- map-raster
  "A fresh `mode` raster carrying `f` over every packed pixel of `src`."
  ^Raster [^Raster src mode f]
  (let [w
        (.getWidth src)

        hh
        (.getHeight src)

        out
        (new-raster mode w hh)]

    (dotimes [y hh]
      (dotimes [x w]
        (.setRGB out x y (unchecked-int (long (f (.getRGB src x y)))))))
    out))

(defn- cmyk->rgb
  "One CMYK sample -- C, M, Y in the colour bytes and K in the fourth -- as packed
   sRGB, Pillow's own inversion: `r = 255 - min(255, c + k)`."
  ^long [^long p]
  (let [k (ch p 24)]
    (argb 255
          (- 255 (min 255 (+ (ch p 16) k)))
          (- 255 (min 255 (+ (ch p 8) k)))
          (- 255 (min 255 (+ (ch p 0) k))))))

(defn- rgb->cmyk
  "Packed sRGB -> CMYK storage. Pillow separates WITHOUT black generation: K stays
   0 and the inks are plain complements, so `convert('CMYK').convert('RGB')` is a
   round trip."
  ^long [^long p]
  (argb 0 (- 255 (ch p 16)) (- 255 (ch p 8)) (- 255 (ch p 0))))

(defn- rgb-raster
  "The raster `imaging` can take: 'CMYK' is separated INK, not colour -- no codec
   here writes it, and reading its K byte as alpha would encode a transparent
   picture -- so it comes back to colour first. Every other mode is already sRGB."
  ^Raster [^Raster r mode]
  (if (= "CMYK" (str mode)) (map-raster r "RGB" cmyk->rgb) r))

(def ^:private encodable-formats
  "Formats `imaging` can write. Anything else is a PIL-visible error."
  #{"png" "jpeg" "webp" "gif" "bmp" "tiff" "ico" "qoi" "pnm" "tga" "exr" "ff"})

(defn- has-alpha?
  "True when this raster carries a real alpha channel (an `alpha-mode?` raster)."
  [^Raster r]
  (zero? (.amask r)))

(defn- ->argb
  "A RESOLVED colour -> packed 0xAARRGGBB. RESOLVING a PIL colour SPEC is PIL's own
   `getink` rule -- an int is the packed 0xAABBGGRR compatibility form for a
   multi-band mode but one clipped byte for a single-band one, and a colour name is
   read in the target mode -- and it lives in the Python shim (`_ink`), the only
   side that knows a mode's BAND COUNT and PIL's error vocabulary. What crosses is
   [r g b a]; a shorter vector is one of this namespace's own literals."
  ^long [c mode]
  (if (sequential? c)
    (let [[r g b a] (mapv long c)]
      (case (count c)
        1
        (argb 255 r r r)

        2
        (argb g r r r)

        3
        (argb 255 r g b)

        (argb a r g b)))
    (if (alpha-mode? mode) (argb 0 0 0 0) (argb 255 0 0 0))))

(defn- ->hex
  "Packed 0xAARRGGBB -> the `#rrggbbaa` string `imaging`'s draw ops take."
  ^String [^long p]
  (format "#%02x%02x%02x%02x" (ch p 16) (ch p 8) (ch p 0) (ch p 24)))

(defn- raster->rgba
  "A raster's pixels as straight RGBA8 rows -- the shape the cdylib takes for both
   drawing and GIF frames. A flat scan over the backing `int[]`: drawing converts
   the WHOLE canvas, so a per-pixel `getRGB`/`setRGB` here is felt as lag.

   `aset` on a `^bytes` local, NEVER `aset-byte`: the latter is `java.lang.
   reflect.Array/setByte`, and four reflective stores per pixel cost 65 ms on a
   800x600 canvas versus 1.8 ms for the inlined array store."
  ^bytes [^Raster r]
  (let [^ints px
        (.px r)

        amask
        (.amask r)

        n
        (alength px)

        b
        (byte-array (* 4 n))]

    (dotimes [i n]
      (let [p (bit-or amask (bit-and 0xffffffff (aget px i)))
            o (* 4 i)]

        (aset b o (unchecked-byte (ch p 16)))
        (aset b (+ o 1) (unchecked-byte (ch p 8)))
        (aset b (+ o 2) (unchecked-byte (ch p 0)))
        (aset b (+ o 3) (unchecked-byte (ch p 24)))))
    b))

(defn- ->img
  "A live `imaging` image holding this raster's pixels (straight RGBA8)."
  [^Raster r]
  (im/from-pixels (raster->rgba r) (.getWidth r) (.getHeight r)))

(defn- rgba->px!
  "Overwrite a raster's pixels from straight RGBA8 rows, IN PLACE, and return it.

   The packing is spelled out rather than delegated to `argb`, whose arguments
   are boxed: this runs once per pixel on every read of a drawn canvas."
  ^Raster [^Raster r ^bytes b]
  (let [^ints px
        (.px r)

        n
        (alength px)]

    (dotimes [i n]
      (let [o (* 4 i)]
        (aset px
              i
              (unchecked-int (bit-or (bit-shift-left (bit-and (aget b (+ o 3)) 0xff) 24)
                                     (bit-shift-left (bit-and (aget b o) 0xff) 16)
                                     (bit-shift-left (bit-and (aget b (+ o 1)) 0xff) 8)
                                     (bit-and (aget b (+ o 2)) 0xff))))))
    r))

(defn- ->raster
  "An `imaging` image's pixels as a raster in `mode`."
  ^Raster [img mode]
  (let [{:keys [width height]} (im/info img)]
    (rgba->px! (new-raster mode (long width) (long height)) (im/pixels img))))

(defn- take-draw!
  "Detach this handle's in-flight draw image, if any."
  [h]
  (let [k (long h)]
    (when-let [img (get @live-draws k)]
      (swap! live-draws dissoc k)
      img)))

(defn- flush-draws!
  "Send a handle's queued draw ops to the cdylib, write the resulting image back
   into its raster and close it, so every other op reads pixels that already
   include the drawing."
  [h]
  (run-draws! h)
  (when-let [img (take-draw! h)]
    (try (when-let [^Raster r (:img (raw-entry h))]
           (rgba->px! r (im/pixels img)))
         (finally (im/close! img))))
  nil)

(defn- img->mode
  "Pillow's `mode` for a just-opened file.

   Grayscale-ness comes from `imaging/probe`, which reports the SOURCE colour
   type: the decoded handle is always RGBA8 and cannot tell an 'L' PNG from a
   colour one. 16-bit and float sources map onto their 8-bit family (a raster
   stores one byte per channel) and indexed sources report what they expand
   into.

   Transparency, in contrast, comes from the PIXELS and not from the header's
   channel count: this shim always encodes an RGBA raster, so every file it
   writes carries an alpha channel and a saved 'RGB' image has to open as 'RGB'
   again. An unreadable header just means not-grayscale."
  [^bytes b img]
  (let [{:keys [is-grayscale]}
        (try (im/probe b) (catch Throwable _ nil))

        alpha?
        (not (:is-opaque (im/info img)))]

    (if is-grayscale (if alpha? "LA" "L") (if alpha? "RGBA" "RGB"))))

(defn- meta-of
  [h]
  (let [{:keys [^Raster img mode]} (entry h)]
    [(long h) (.getWidth img) (.getHeight img) mode]))

(defn- flatten-rgb
  "Composite onto opaque white -- what the alpha-less codecs (JPEG, BMP) need."
  ^Raster [^Raster src]
  (let [w
        (.getWidth src)

        h
        (.getHeight src)

        out
        (new-raster "RGB" w h)]

    (dotimes [y h]
      (dotimes [x w]
        (let [p (.getRGB src x y)
              a (/ (ch p 24) 255.0)]

          (.setRGB out
                   x
                   y
                   (argb 255
                         (clamp255 (+ (* (ch p 16) a) (* 255.0 (- 1.0 a))))
                         (clamp255 (+ (* (ch p 8) a) (* 255.0 (- 1.0 a))))
                         (clamp255 (+ (* (ch p 0) a) (* 255.0 (- 1.0 a)))))))))
    out))

;; Core ops. Each returns a value the Python shim understands: a meta vector
;; [handle w h mode] for image-producing ops, else a scalar / base64 string.

(defn- op-new
  [mode w h fill]
  (let [img (new-raster mode w h)]
    (when (some? fill)
      (let [p (->argb fill mode)]
        (dotimes [y (long h)]
          (dotimes [x (long w)]
            (.setRGB img x y p)))))
    (meta-of (put-img! img (str mode)))))

(def ^:private max-decode-bytes
  "Ceiling on the RGBA8 buffer ONE decode may allocate: 512 MiB, the `image`
   crate's own `Limits::max_alloc` default. Checked from the HEADER --
   `imaging/probe` reads dimensions WITHOUT decoding -- so a file whose pixels
   cannot fit reports its real size instead of the decoder's misleading
   `cannot identify image file`."
  (* 512 1024 1024))

(defn- guard-decodable!
  "Throw a truthful `image too large` before an oversized decode is attempted."
  [probe]
  (let [w
        (long (:width probe 0))

        h
        (long (:height probe 0))

        need
        (* w h 4)]

    (when (> need (long max-decode-bytes))
      (throw
        (ex-info
          (format
            "image too large to decode: %dx%d needs %.0f MiB of RGBA pixels, over the %d MiB decoder limit"
            w
            h
            (/ (double need) 1048576.0)
            (long (/ (long max-decode-bytes) 1048576)))
          {:width w :height h :bytes need :limit max-decode-bytes})))))

(defn- frame-raster
  "A decoded GIF frame's full-canvas ARGB pixels as a raster + its Pillow mode."
  [^ints argb ^long w ^long h]
  (let [opaque? (loop [i 0]
                  (cond (>= i (alength argb)) true
                        (not= 255 (bit-and 0xff (bit-shift-right (aget argb i) 24))) false
                        :else (recur (inc i))))]
    [(Raster. (aclone argb) w h (if opaque? 0xff000000 0)) (if opaque? "RGB" "RGBA")]))

(defn- open-animation
  "Register a decoded multi-frame GIF: the handle holds frame 0 and the entry
   keeps every frame, so `seek` can swap them in without re-decoding."
  [{:keys [width height loop-count frames]}]
  (let [[img mode]
        (frame-raster (:argb (first frames)) (long width) (long height))

        h
        (put-img! img mode)]

    (res/update! ::images
                 h
                 assoc
                 :frames (mapv #(select-keys % [:argb :delay-ms :disposal]) frames)
                 :frame 0
                 :loop-count loop-count)
    (meta-of h)))

(defn- op-open
  [b64]
  (let [bytes
        (.decode (Base64/getDecoder) ^String b64)

        _
        (guard-decodable! (try (im/probe bytes) (catch Throwable _ nil)))

        anim
        (when (gif/gif? bytes) (try (gif/decode bytes) (catch Throwable _ nil)))]

    (if (seq (:frames anim))
      (open-animation anim)
      (let [img (try (im/decode bytes) (catch Throwable _ nil))]
        (when (nil? img) (throw (ex-info "cannot identify image file" {})))
        (let [mode (img->mode bytes img)
              h (put-img! (->raster img mode) mode)]

          (im/close! img)
          (meta-of h))))))

(defn- op-frames
  "Animation metadata for the Python side: [n-frames loop-count delays-ms].
   `loop-count` is -1 when the file carries no NETSCAPE loop block."
  [h]
  (let [{:keys [frames loop-count]} (entry h)]
    [(max 1 (count frames)) (long (or loop-count -1)) (mapv #(long (or (:delay-ms %) 0)) frames)]))

(defn- op-seek
  "Make frame `n` the current one, in place (Pillow's `Image.seek`)."
  [h n]
  (let [{:keys [frames]}
        (entry h)

        n
        (long n)]

    (when-not (and (seq frames) (<= 0 n) (< n (count frames)))
      (throw (ex-info "attempt to seek beyond the last frame" {:frame n})))
    (let [{:keys [^ints argb]}
          (nth frames n)

          ^Raster cur
          (:img (entry h))

          [img mode]
          (frame-raster argb (.getWidth cur) (.getHeight cur))]

      (res/update! ::images (long h) assoc :img img :mode mode :frame n)
      (meta-of h))))

(defn- normalise-format
  "A Pillow format name -> the `imaging` codec key, or a PIL-visible error."
  [fmt]
  (let [fmt
        (str/lower-case (or fmt "png"))

        fmt
        (case fmt
          "jpg"
          "jpeg"

          ;; Pillow's netpbm family is ONE `imaging` codec: PBM/PGM/PPM are the
          ;; bilevel/gray/colour flavours of PNM, picked by the content.
          ("ppm" "pgm" "pbm" "pnm")
          "pnm"

          fmt)]

    (when-not (contains? encodable-formats fmt)
      (throw (ex-info (str "no image writer for format " fmt) {})))
    fmt))

(defn- optimised
  "Pillow's `optimize=True`, honoured for real: hand the ENCODED bytes to the
   imaging library's format-specific optimisers (oxipng's filter/colour-type
   search for PNG, jpegtran-style lossless metadata stripping for JPEG,
   gifsicle's differencing re-encoder for GIF). Lossless — the pixels come back
   bit-identical — and never bigger, so a format with no optimiser is a no-op."
  ^bytes [^bytes data]
  (try (im/optimize data) (catch Throwable _ data)))

(defn- op-save
  "Encode one image. `quality` is Pillow's 1-100 lossy quality (JPEG, WebP);
   nil keeps the codec default, and the lossless codecs ignore it. `optimize`
   is Pillow's flag of the same name: a second, LOSSLESS pass through the
   format's real optimiser."
  [h fmt quality optimize]
  (let [{:keys [^Raster img mode]}
        (entry h)

        fmt
        (normalise-format fmt)

        ^Raster img
        (rgb-raster img mode)

        img
        (if (and (#{"jpeg" "bmp"} fmt) (has-alpha? img)) (flatten-rgb img) img)

        q
        (when (number? quality) (min 100 (max 1 (long quality))))

        src
        (->img img)

        ^bytes raw
        (try (if q (im/encode src (keyword fmt) q) (im/encode src (keyword fmt)))
             (finally (im/close! src)))]

    (.encodeToString (Base64/getEncoder) (if optimize (optimised raw) raw))))

(defn- op-save-temp
  [h fmt]
  (let [{:keys [^Raster img]}
        (entry h)

        fmt
        (str/lower-case (or fmt "png"))

        norm
        (normalise-format fmt)

        b64
        (op-save h fmt nil false)

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
  (let [{:keys [^Raster img mode palette indices transparent]}
        (entry h)

        w
        (.getWidth img)

        hh
        (.getHeight img)

        out
        (new-raster mode w hh)]

    (dotimes [y hh]
      (dotimes [x w]
        (.setRGB out x y (.getRGB img x y))))
    (let [nh (put-img! out mode)]
      ;; a P image copies its palette with it -- `getpalette` on the copy has to
      ;; answer the same table Pillow's does.
      (when palette
        (res/update! ::images nh assoc :palette palette :indices indices :transparent transparent))
      (meta-of nh))))

(defn- resample->filter
  "PIL resample constant -> an `imaging` resampling filter."
  [r]
  (case (int r)
    0
    :nearest

    2
    :triangle

    :catmull-rom))

(defn- op-resize
  [h w h2 resample]
  (let [{:keys [^Raster img mode]}
        (entry h)

        src
        (->img img)

        out
        (im/resize src (int w) (int h2) (resample->filter resample))]

    (try (meta-of (put-img! (->raster out mode) mode)) (finally (im/close! src) (im/close! out)))))

(defn- op-crop
  [h l t r b]
  (let [{:keys [^Raster img mode]}
        (entry h)

        w
        (max 1 (- (int r) (int l)))

        hh
        (max 1 (- (int b) (int t)))

        iw
        (.getWidth img)

        ih
        (.getHeight img)

        out
        (new-raster mode w hh)]

    (dotimes [y hh]
      (dotimes [x w]
        (let [sx (+ x (int l))
              sy (+ y (int t))]

          (when (and (>= sx 0) (< sx iw) (>= sy 0) (< sy ih))
            (.setRGB out x y (.getRGB img sx sy))))))
    (meta-of (put-img! out mode))))

(defn- op-rotate
  [h angle expand fillc]
  (let [{:keys [^Raster img mode]}
        (entry h)

        src
        (->img img)

        out
        (im/rotate src
                   (double angle)
                   {:expand (boolean expand) :background (->hex (->argb fillc mode))})]

    (try (meta-of (put-img! (->raster out mode) mode)) (finally (im/close! src) (im/close! out)))))

(defn- op-transpose
  "PIL `Image.transpose`: every flip and quarter turn is `imaging`'s own
   `flip`/`rotate` (counter-clockwise, like PIL), and the two diagonal methods
   compose the pair — no pixel loop of ours."
  [h method]
  (let [{:keys [^Raster img mode]}
        (entry h)

        step
        (fn [im op]
          (let [out (if (keyword? op) (im/flip im op) (im/rotate im (double op) {:expand true}))]
            (im/close! im)
            out))

        ops
        (case (int method)
          0
          [:horizontal]

          1
          [:vertical]

          2
          [90]

          3
          [180]

          4
          [270]

          5
          [90 :vertical]

          6
          [270 :vertical]

          [])

        out
        (reduce step (->img img) ops)]

    (try (meta-of (put-img! (->raster out mode) mode)) (finally (im/close! out)))))

(defn- lum ^long [^long p] (clamp255 (+ (* 0.299 (ch p 16)) (* 0.587 (ch p 8)) (* 0.114 (ch p 0)))))

;; Palette ("P") images. A P raster keeps the QUANTISED RGB in its pixels, so
;; every downstream op and codec just works; the registry entry additionally
;; carries `:palette` (packed 0xRRGGBB per index) and `:indices` (one byte per
;; pixel) -- what `getpalette`, `getpixel`, `tobytes` and the GIF writer report.






(defn- quantize-raster
  "Median-cut `src` down to at most `ncolors` colours.

   Returns `{:img :palette :indices :transparent}`: `:img` is a raster holding
   the quantised colours (transparent where the source was), `:palette` an int[]
   of packed 0xRRGGBB, `:indices` one byte per pixel. When the source has
   see-through pixels one palette slot is reserved for them, exactly as a GIF
   transparent index. `dither?` adds Floyd-Steinberg error diffusion --
   Pillow's default for `convert(\"P\")`.

   The median cut, the nearest-colour search and the error diffusion are
   `imaging/quantize` (Rust) -- the SAME palette engine behind `imaging/optimize`'s
   lossy PNG and GIF paths, so a shim palette and a written GIF cannot drift
   apart. Only the packing into a PIL raster is done here."
  [^Raster src ^long ncolors dither?]
  (let [w
        (.getWidth src)

        hh
        (.getHeight src)

        img
        (->img src)]

    (try (let [{:keys [palette indices transparent ^bytes rgba]}
               (im/quantize img {:colors ncolors :dither (boolean dither?)})

               out
               (new-raster "P" w hh)]

           (dotimes [y hh]
             (dotimes [x w]
               (let [o (* 4 (+ (* y w) x))
                     a (bit-and (aget rgba (+ o 3)) 0xff)]

                 (.setRGB out
                          x
                          y
                          (if (< a 128)
                            0
                            (argb 255
                                  (bit-and (aget rgba o) 0xff)
                                  (bit-and (aget rgba (+ o 1)) 0xff)
                                  (bit-and (aget rgba (+ o 2)) 0xff)))))))
           {:img out :palette (int-array palette) :indices indices :transparent transparent})
         (finally (im/close! img)))))

(defn- put-palette-img!
  "Register a quantisation result as one P image, palette and all."
  [{:keys [img palette indices transparent]}]
  (let [h (put-img! img "P")]
    (res/update! ::images h assoc :palette palette :indices indices :transparent transparent)
    h))

(defn- op-quantize
  [h colors dither]
  (let [{:keys [^Raster img]} (entry h)]
    (meta-of (put-palette-img! (quantize-raster img (long (or colors 256)) (boolean dither))))))

(defn- op-getpalette
  "The image's palette as Pillow's flat [r g b r g b ...], or nil when it has none."
  [h]
  (when-let [^ints pal (:palette (entry h))]
    (vec (mapcat (fn [c]
                   [(ch c 16) (ch c 8) (ch c 0)])
                 pal))))

(defn- op-putpalette
  "Install a flat [r g b ...] palette IN PLACE (Pillow mutates the image and
   switches it to mode P): the pixels become the colours their index selects, so
   a later save or convert shows the new table."
  [h data]
  (let [{:keys [^Raster img ^bytes indices]}
        (entry h)

        pal
        (int-array (map (fn [[r g b]]
                          (bit-or (bit-shift-left (bit-and (long r) 255) 16)
                                  (bit-shift-left (bit-and (long g) 255) 8)
                                  (bit-and (long b) 255)))
                        (partition 3 3 [0 0] (map long data))))

        w
        (.getWidth img)

        hh
        (.getHeight img)

        ind
        (or indices (byte-array (* w hh)))]

    (when (zero? (alength pal)) (throw (ex-info "palette must hold at least one RGB triple" {})))
    (dotimes [y hh]
      (dotimes [x w]
        (let [i (+ (* y w) x)
              idx (if indices
                    (bit-and (aget ind i) 255)
                    ;; an L / 1 raster indexes the new table by its gray value
                    (ch (.getRGB img x y) 16))
              idx (min idx (dec (alength pal)))
              c (aget pal (int idx))]

          (aset ind i (unchecked-byte idx))
          (.setRGB img x y (argb 255 (ch c 16) (ch c 8) (ch c 0))))))
    (res/update! ::images (long h) assoc :mode "P" :palette pal :indices ind :transparent nil)
    (meta-of h)))

(defn- op-save-all
  "Write `handles` as ONE multi-frame file. GIF is the only multi-frame writer
   here, so anything else is a PIL-visible error rather than an animation that
   silently collapses to its first frame."
  [handles fmt duration loop-count optimize]
  (let [fmt
        (normalise-format fmt)

        _
        (when-not (= fmt "gif")
          (throw (ex-info (str "save_all/append_images is only supported for GIF here, not "
                               (str/upper-case fmt))
                          {})))

        hs
        (mapv long handles)

        rasters
        (mapv (fn [h]
                (:img (entry h)))
              hs)

        ^Raster r0
        (first rasters)

        w
        (.getWidth r0)

        hh
        (.getHeight r0)

        _
        (when-not (every? (fn [^Raster r]
                            (and (= w (.getWidth r)) (= hh (.getHeight r))))
                          rasters)
          (throw (ex-info "every frame of an animation must have the same size" {})))

        delays
        (if (sequential? duration)
          (mapv #(long (or % 0)) duration)
          (repeat (count rasters) (long (or duration 0))))

        frames
        (mapv (fn [^Raster r d]
                ;; the cdylib owns palette quantization, LZW and disposal now, so each
                ;; frame crosses as a full-canvas straight-RGBA8 buffer + its delay.
                {:delay-ms d :rgba (raster->rgba r)})
              rasters
              delays)

        ^bytes raw
        (gif/encode {:width w
                     :height hh
                     :loop-count (when (some? loop-count) (long loop-count))
                     :frames frames})]

    (.encodeToString (Base64/getEncoder) (if optimize (optimised raw) raw))))

(defn- op-convert
  [h target dither]
  (let [{:keys [^Raster img mode]} (entry h)]
    (if (= mode (str target))
      (op-copy h)
      (let [target (str target)
            ;; a CMYK source is separated ink; every conversion below reads sRGB.
            ^Raster img (rgb-raster img mode)
            w (.getWidth img)
            hh (.getHeight img)]

        (case target
          ;; P: an adaptive median-cut palette (Pillow's default here is the web
          ;; palette; an adaptive one is both closer to `quantize` and better).
          "P"
          (meta-of (put-palette-img! (quantize-raster img 256 true)))

          ;; sRGB-space luminance (Pillow's ITU-R 601-2), computed per pixel —
          ;; NOT Java2D's linear-space drawImage conversion.
          ("L" "I" "F")
          (let [out (new-raster "L" w hh)]
            (dotimes [y hh]
              (dotimes [x w]
                (.setRGB out x y (unchecked-int (gray-argb (lum (.getRGB img x y)))))))
            (meta-of (put-img! out target)))

          ;; '1': Pillow DITHERS by default (Floyd-Steinberg over the luminance
          ;; plane) and only thresholds when the caller passes `dither=NONE`. Always
          ;; thresholding turned a gradient into two flat blocks.
          "1"
          (let [out (new-raster "1" w hh)
                gray (fn ^double [^long x ^long y]
                       (double (lum (.getRGB img (int x) (int y)))))]

            (if (zero? (long dither))
              (dotimes [y hh]
                (dotimes [x w]
                  (let [v (if (>= (double (gray x y)) 128.0) 255 0)]
                    (.setRGB out x y (unchecked-int (gray-argb v))))))
              (let [err (double-array (* (long w) (long hh)))
                    spill (fn [^long x ^long y ^double e ^double f]
                            (when (and (< -1 x w) (< -1 y hh))
                              (let [i (+ (* y (long w)) x)]
                                (aset err i (+ (aget err i) (* e f))))))]

                (dotimes [y hh]
                  (dotimes [x w]
                    (let [v (+ (double (gray x y))
                               (double (aget err (+ (* (long y) (long w)) (long x)))))
                          q (if (>= v 128.0) 255 0)
                          e (- v (double q))]

                      (.setRGB out x y (unchecked-int (gray-argb q)))
                      (spill (inc x) y e 0.4375)
                      (spill (dec x) (inc y) e 0.1875)
                      (spill x (inc y) e 0.3125)
                      (spill (inc x) (inc y) e 0.0625))))))
            (meta-of (put-img! out "1")))

          "LA"
          (let [out (new-raster "LA" w hh)]
            (dotimes [y hh]
              (dotimes [x w]
                (let [p (.getRGB img x y)
                      v (lum p)]

                  (.setRGB out x y (unchecked-int (argb (ch p 24) v v v))))))
            (meta-of (put-img! out "LA")))

          "CMYK"
          (meta-of (put-img! (map-raster img "CMYK" rgb->cmyk) "CMYK"))

          ;; RGB / RGBA: a straight channel copy (drawImage preserves sRGB).
          (let [out (new-raster target w hh)]
            (dotimes [y hh]
              (dotimes [x w]
                (.setRGB out x y (.getRGB img x y))))
            (meta-of (put-img! out target))))))))

(defn- op-getpixel
  [h x y]
  (let [{:keys [^Raster img mode ^bytes indices]}
        (entry h)

        p
        (.getRGB img (int x) (int y))]

    (if (and (= "P" (str mode)) indices)
      ;; a P pixel is its PALETTE INDEX, not the colour that index resolves to.
      (bit-and (long (aget indices (+ (* (long y) (.getWidth img)) (long x)))) 0xFF)
      (let [vs (mapv #(ch p (long %)) (band-shifts mode))]
        (if (= 1 (count vs)) (first vs) vs)))))

(defn- op-putpixel
  [h x y c]
  (let [{:keys [^Raster img mode]} (entry h)]
    (.setRGB img (int x) (int y) (unchecked-int (->argb c mode)))
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

        ^Raster d
        d

        ^Raster s
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

        {mimg :img mmode :mode}
        (when (and mask (>= (long mask) 0)) (entry mask))

        band
        (mask-band mmode)

        bitmap?
        (= "1" (str mmode))]

    (dotimes [j sh]
      (dotimes [i sw]
        (let [dx (+ x i)
              dy (+ y j)]

          (when (and (>= dx 0) (< dx dw) (>= dy 0) (< dy dh))
            (if mimg
              (let [mp (long (mask-at mimg band bitmap? i j))]
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
  (let [{:keys [^Raster img mode]}
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
  "PIL's `histogram`, MASK included: with a mask only the pixels it marks non-zero
   are counted -- what `ImageStat.Stat(im, mask)` measures. The mask used to be
   dropped on the Python side, so every masked statistic described the whole image."
  [h mask]
  (let [{:keys [^Raster img mode]}
        (entry h)

        {^Raster m :img mmode :mode}
        (when mask (entry mask))

        mband
        (when m (mask-band mmode))

        bitmap?
        (= "1" (str mmode))

        w
        (.getWidth img)

        hh
        (.getHeight img)

        chans
        (band-shifts mode)

        nch
        (count chans)

        bins
        (int-array (* 256 nch))]

    (when (and m (or (not= w (.getWidth m)) (not= hh (.getHeight m))))
      (throw (ex-info "mask size does not match image size" {})))
    (dotimes [y hh]
      (dotimes [x w]
        (when (or (nil? m) (pos? (long (mask-at m mband bitmap? x y))))
          (let [p (.getRGB img x y)]
            (dotimes [c nch]
              (let [v (ch p (long (nth chans c)))
                    idx (+ (* c 256) v)]

                (aset bins idx (inc (aget bins idx)))))))))
    (vec bins)))

(defn- op-tobytes
  [h]
  (let [{:keys [^Raster img mode ^bytes indices]}
        (entry h)

        w
        (.getWidth img)

        hh
        (.getHeight img)

        shifts
        (band-shifts mode)

        bpp
        (count shifts)]

    (cond
      ;; a P image's bytes ARE its palette indices.
      (and (= "P" (str mode)) indices (= (alength indices) (* w hh)))
      (.encodeToString (Base64/getEncoder) ^bytes indices)
      ;; PIL packs a BILEVEL image one BIT per pixel, most significant bit first,
      ;; each row padded to a whole byte -- a 2x2 '1' image is TWO bytes, not four.
      (= "1" (str mode)) (let [stride
                               (quot (+ (long w) 7) 8)

                               buf
                               (byte-array (* stride (long hh)))]

                           (dotimes [y hh]
                             (dotimes [x w]
                               (when (pos? (ch (.getRGB img x y) 16))
                                 (let [i (+ (* y stride) (quot x 8))]
                                   (aset buf
                                         i
                                         (unchecked-byte (bit-or (bit-and (aget buf i) 0xff)
                                                                 (bit-shift-right 128
                                                                                  (rem x 8)))))))))
                           (.encodeToString (Base64/getEncoder) buf))
      :else (let [buf (byte-array (* w hh bpp))]
              (dotimes [y hh]
                (dotimes [x w]
                  (let [p (.getRGB img x y)
                        i (* (+ (* y w) x) bpp)]

                    (dotimes [c bpp]
                      (aset buf (+ i c) (unchecked-byte (ch p (long (nth shifts c)))))))))
              (.encodeToString (Base64/getEncoder) buf)))))

(defn- op-frombytes
  [mode w h b64]
  (let [data
        (.decode (Base64/getDecoder) ^String b64)

        mode
        (str mode)

        bpp
        (count (band-shifts mode))

        out
        (new-raster mode w h)]

    (if (= "1" mode)
      ;; the inverse of the bilevel packing above: a set bit reads back as 255, the
      ;; way PIL's raw '1' decoder unpacks it.
      (let [stride (quot (+ (long w) 7) 8)]
        (dotimes [y h]
          (dotimes [x w]
            (let [b (bit-and (aget data (+ (* (long y) stride) (quot (long x) 8))) 0xff)]
              (.setRGB out
                       x
                       y
                       (unchecked-int
                         (gray-argb (if (pos? (bit-and b (bit-shift-right 128 (rem (long x) 8))))
                                      255
                                      0))))))))
      (dotimes [y h]
        (dotimes [x w]
          (let [i (* (+ (* y (long w)) x) (long bpp))
                u (fn [^long k]
                    (bit-and (aget data (+ i k)) 0xff))]

            (.setRGB out
                     x
                     y
                     (unchecked-int (case (long bpp)
                                      1
                                      (gray-argb (u 0))

                                      ;; 'LA' arrives as (gray, alpha): the gray band is
                                      ;; replicated so every colour read sees it.
                                      2
                                      (argb (u 1) (u 0) (u 0) (u 0))

                                      4
                                      (argb (u 3) (u 0) (u 1) (u 2))

                                      (argb 255 (u 0) (u 1) (u 2)))))))))
    (meta-of (put-img! out mode))))

(defn- op-point
  [h lut]
  (let [{:keys [^Raster img mode]}
        (entry h)

        w
        (.getWidth img)

        hh
        (.getHeight img)

        L
        (int-array (map int lut))

        out
        (new-raster mode w hh)]

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
  "PIL `ImageFilter.Kernel`: the convolution is `imaging/convolve` — edge
   clamping, `scale`/`offset` arithmetic and the alpha carry all live there."
  [h size kernel scale offset]
  (let [{:keys [^Raster img mode]}
        (entry h)

        src
        (->img img)

        out
        (im/convolve src
                     (int size)
                     (mapv double kernel)
                     {:scale (double scale) :offset (double offset)})]

    (try (meta-of (put-img! (->raster out mode) mode)) (finally (im/close! src) (im/close! out)))))

(defn- op-rank
  "PIL's rank family (`RankFilter`, `MinFilter`, `MedianFilter`, `MaxFilter`):
   `imaging/rank-filter` sorts every channel over the k x k window and keeps the
   `rank`-th smallest."
  [h size rank]
  (let [{:keys [^Raster img mode]}
        (entry h)

        n
        (* (long size) (long size))

        r
        (min (dec n) (max 0 (long rank)))

        src
        (->img img)

        out
        (im/rank-filter src (int size) r)]

    (try (meta-of (put-img! (->raster out mode) mode)) (finally (im/close! src) (im/close! out)))))

(defn- op-blend
  [ha hb t]
  (let [{a :img ma :mode}
        (entry ha)

        {b :img}
        (entry hb)

        ^Raster a
        a

        ^Raster b
        b

        t
        (double t)

        w
        (.getWidth a)

        hh
        (.getHeight a)

        out
        (new-raster ma w hh)]

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

        {m :img mmode :mode}
        (entry hmask)

        ^Raster a
        a

        ^Raster b
        b

        ^Raster m
        m

        band
        (mask-band mmode)

        bitmap?
        (= "1" (str mmode))

        w
        (.getWidth a)

        hh
        (.getHeight a)

        out
        (new-raster ma w hh)]

    (dotimes [y hh]
      (dotimes [x w]
        (let [t (/ (long (mask-at m band bitmap? x y)) 255.0)]
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

        ^Raster a
        a

        ^Raster b
        b

        w
        (.getWidth a)

        hh
        (.getHeight a)

        out
        (new-raster ma w hh)]

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
  (let [{:keys [^Raster img mode]}
        (entry h)

        w
        (.getWidth img)

        hh
        (.getHeight img)]

    (mapv (fn [sh]
            (let [out (new-raster "L" w hh)]
              (dotimes [y hh]
                (dotimes [x w]
                  (.setRGB out x y (unchecked-int (gray-argb (ch (.getRGB img x y) (long sh)))))))
              (meta-of (put-img! out "L"))))
          (band-shifts mode))))

(defn- op-merge
  [mode handles]
  (let [mode
        (str mode)

        imgs
        (mapv #(:img (entry %)) handles)

        ^Raster f
        (first imgs)

        w
        (.getWidth f)

        hh
        (.getHeight f)

        out
        (new-raster mode w hh)]

    (dotimes [y hh]
      (dotimes [x w]
        (let [vals (mapv #(ch (.getRGB ^Raster % x y) 0) imgs)
              [r g b a] vals]

          (.setRGB out
                   x
                   y
                   (unchecked-int (case mode
                                    ;; 'LA' takes TWO bands -- gray and alpha -- and
                                    ;; keeps the gray replicated across R/G/B.
                                    "LA"
                                    (argb (or g 255) r r r)

                                    "RGBA"
                                    (argb (or a 255) r g b)

                                    "RGB"
                                    (argb 255 r g b)

                                    (gray-argb r)))))))
    (meta-of (put-img! out mode))))

(defn- op-offset
  "Roll `img` by (dx, dy) with wraparound (ImageChops.offset)."
  [h dx dy]
  (let [{:keys [^Raster img mode]}
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
        (new-raster mode w hh)]

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

        ^Raster d
        d

        ^Raster s
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
        (new-raster "RGBA" w hh)]

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
  (let [{:keys [^Raster img mode]}
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
        (new-raster mode ow oh)

        fill-argb
        (->argb fillc mode)]

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

;; ImageDraw dispatcher. `xy` is a flat [x0 y0 x1 y1 ...] seq; `opts` is a
;; string-keyed map with resolved fill/outline colours (lists/ints), width and
;; arc start/end. Colours are resolved Python-side via ImageColor.

(def ^:private sans-family
  "Fallback face. Text used to be PINNED to it -- the font a caller asked for
   never crossed the bridge -- so every render came out in sans at whatever size
   was requested. Now it is only what an unresolvable request falls back to."
  "Noto Sans")

(def ^:private font-weight-words
  "Weight words a font FILE name or family name carries, longest first so
   `extrabold` is never read as `bold`. A PIL font object has no weight of its
   own, so this name is the only place a `-Bold` face announces itself."
  [["extrablack" 950] ["ultrablack" 950] ["extrabold" 800] ["ultrabold" 800] ["extralight" 200]
   ["ultralight" 200] ["semibold" 600] ["demibold" 600] ["semilight" 350] ["heavy" 900]
   ["black" 900] ["bold" 700] ["medium" 500] ["light" 300] ["thin" 100]])

(defn- font-key
  "Families compare on letters and digits only: `JetBrainsMono-Bold.ttf` and
   `JetBrains Mono` name the same face."
  [s]
  (str/replace (str/lower-case (str s)) #"[^a-z0-9]+" ""))

(def ^:private family-index
  "font-key -> family, over every family the shared database knows."
  (delay (into {}
               (map (fn [f]
                      [(font-key f) f]))
               (im/fonts))))

(defonce ^:private font-cache (atom {}))

(defn- font-file-families
  "Register a real font FILE with the shared database (once per path) and answer
   the families it provided; `nil` when the spec is not a readable font file."
  [^String spec]
  (when (re-find #"(?i)\.(ttf|otf|ttc)$" spec)
    (let [f (java.io.File. spec)]
      (when (.isFile f) (try (seq (im/register-font! f)) (catch Throwable _ nil))))))

(defn- font-family-named
  "The database family this name means, tolerating punctuation and a trailing
   weight/style word (`Noto Sans Bold` -> `Noto Sans`)."
  [s]
  (let [idx
        @family-index

        k
        (font-key s)]

    (or (get idx k)
        (get idx
             (reduce (fn [acc w]
                       (str/replace acc w ""))
                     k
                     (into ["italic" "oblique"] (map first) font-weight-words))))))

(defn- resolve-font*
  [^String spec]
  (let [stem
        (str/replace (or (peek (str/split spec #"[/\\]")) spec) #"(?i)\.(ttf|otf|ttc)$" "")

        lower
        (str/lower-case stem)

        weight
        (some (fn [[w n]]
                (when (str/includes? lower w) n))
              font-weight-words)

        italic
        (boolean (re-find #"italic|oblique" lower))

        family
        (or (first (font-file-families spec)) (font-family-named stem) sans-family)]

    (cond-> {:family family}
      weight
      (assoc :weight weight)

      italic
      (assoc :italic true))))

(defn- resolve-font
  "PIL hands the host whatever `ImageFont.truetype` was given: a font FILE path
   or a family name. A file is registered with the shared font database and
   answers its real families; a name is matched against the database; anything
   else falls back to `sans-family`. Weight and italic are read off the name."
  [spec]
  (let [s (str/trim (str (or spec "")))]
    (if (str/blank? s)
      {:family sans-family}
      (or (get @font-cache s)
          (let [resolved (resolve-font* s)]
            (swap! font-cache assoc s resolved)
            resolved)))))

(defn- draw-img
  "The live cdylib image backing this handle's draw run, created on first draw
   and reused by the ops that follow."
  [h ^Raster r]
  (or (get @live-draws (long h))
      (let [img (->img r)]
        (swap! live-draws assoc (long h) img)
        img)))

(def ^:private ink-keys "The op keys carrying a `#rrggbbaa` ink." [:fill :stroke])

(defn- translucent-op?
  "Does this op paint with an ink that is not fully opaque?"
  [op]
  (boolean (some (fn [k]
                   (when-let [^String c (get op k)]
                     (< (Long/parseLong (subs c 7 9) 16) 255)))
                 ink-keys)))

(defn- opaque-op
  "The same op with every ink forced fully opaque -- what the coverage pass draws."
  [op]
  (reduce (fn [o k]
            (if-let [^String c (get o k)]
              (assoc o k (str (subs c 0 7) "ff"))
              o))
          op
          ink-keys))

(defn- ink-alphas
  "This op's inks as {0xRRGGBB -> alpha}, plus `:default` for a pixel whose colour
   is neither ink exactly (an antialiased blend between fill and outline)."
  [op]
  (reduce (fn [m k]
            (if-let [^String c (get op k)]
              (let [a (Long/parseLong (subs c 7 9) 16)]
                (-> m
                    (assoc (Long/parseLong (subs c 1 7) 16) a)
                    (update :default #(or % a))))
              m))
          {}
          ink-keys))

(defn- replace-op!
  "Draw ONE op the way PIL's ImageDraw does -- REPLACING the pixels it covers,
   never compositing onto them. tiny-skia only ever blends source-over, so an ink
   whose alpha is below 255 (`fill=(0, 0, 0, 0)` to erase a hole, a translucent
   overlay written verbatim) would otherwise come back blended and, at alpha 0,
   not drawn at all. The op is drawn ONCE at full opacity onto a transparent
   scratch canvas: that canvas' straight RGBA says which pixels PIL would touch
   and with which ink, so the destination takes the ink colour with its REAL
   alpha. PIL draws aliased, so a pixel the renderer only half covers is left
   alone. Answers the image to keep drawing into; the caller closes the old one."
  [img op]
  (let [{:keys [width height]}
        (im/info img)

        w
        (long width)

        h
        (long height)

        scratch
        (im/blank (int w) (int h))]

    (try (im/draw! scratch [(opaque-op op)])
         (let [^bytes cov
               (im/pixels scratch)

               ^bytes dst
               (im/pixels img)

               alphas
               (ink-alphas op)

               fallback
               (long (:default alphas 255))]

           (dotimes [i (* w h)]
             (let [o (* 4 i)]
               (when (<= 128 (bit-and (aget cov (+ o 3)) 0xff))
                 (let [r (bit-and (aget cov o) 0xff)
                       g (bit-and (aget cov (+ o 1)) 0xff)
                       b (bit-and (aget cov (+ o 2)) 0xff)
                       a (long (get alphas
                                    (bit-or (bit-shift-left r 16) (bit-shift-left g 8) b)
                                    fallback))]

                   (aset dst o (unchecked-byte r))
                   (aset dst (+ o 1) (unchecked-byte g))
                   (aset dst (+ o 2) (unchecked-byte b))
                   (aset dst (+ o 3) (unchecked-byte a))))))
           (im/from-pixels dst (int w) (int h)))
         (finally (im/close! scratch)))))

(defn- run-draws!
  "Hand this handle's queued ops to the cdylib as ONE `im/draw!` batch -- unless
   one of them paints with a translucent ink, which `replace-op!` has to run on
   its own."
  [h]
  (let [k (long h)]
    (when-let [ops (seq (get @pending-draws k))]
      (swap! pending-draws dissoc k)
      (when-let [^Raster r (:img (raw-entry k))]
        (let [img (draw-img k r)]
          (if (some translucent-op? ops)
            (swap! live-draws assoc
              k
              (reduce (fn [im op]
                        (if (translucent-op? op)
                          (let [next-im (replace-op! im op)]
                            (im/close! im)
                            next-im)
                          (do (im/draw! im [op]) im)))
                      img
                      ops))
            (im/draw! img (vec ops)))))))
  nil)

(defn- draw-into!
  "Queue `ops` for this handle rather than drawing them now. Vector drawing lives
   in the cdylib (tiny-skia) and every `im/draw!` call round-trips the whole
   canvas through it, so a run of draws (per-character text, say) crosses as ONE
   batch against ONE live image -- flushed by `flush-draws!` before anything
   reads the pixels, and every `max-pending-draws` ops so the queue stays
   bounded."
  [h ops]
  (let [k (long h)]
    (when (<= (long max-pending-draws)
              (count (get (swap! pending-draws update k (fnil into []) ops) k)))
      (run-draws! k)))
  nil)

(defn- arc-path
  "SVG path data for a PIL arc/chord/pieslice over the bounding box
   [x0 y0 x1 y1]. PIL angles are degrees CLOCKWISE from 3 o'clock, which is what
   screen-space `(cos a, sin a)` already gives with y pointing down."
  [kind x0 y0 x1 y1 start end]
  (let [start
        (double start)

        cx
        (/ (+ (double x0) (double x1)) 2.0)

        cy
        (/ (+ (double y0) (double y1)) 2.0)

        rx
        (/ (- (double x1) (double x0)) 2.0)

        ry
        (/ (- (double y1) (double y0)) 2.0)

        sweep
        (min 360.0 (max 0.0 (- (double end) (double start))))

        pt
        (fn [^double a]
          [(+ cx (* rx (Math/cos (Math/toRadians a)))) (+ cy (* ry (Math/sin (Math/toRadians a))))])

        [sx sy]
        (pt (double start))

        ;; A single SVG arc cannot span a full turn: split it in two.
        [mx my]
        (pt (+ start (/ sweep 2.0)))

        [ex ey]
        (pt (+ start sweep))

        laf
        (if (> sweep 180.0) 1 0)

        body
        (if (>= sweep 359.999)
          (format "M %s %s A %s %s 0 1 1 %s %s A %s %s 0 1 1 %s %s" sx sy rx ry mx my rx ry ex ey)
          (format "M %s %s A %s %s 0 %s 1 %s %s" sx sy rx ry laf ex ey))]

    (case kind
      "pieslice"
      (str (format "M %s %s L %s %s" cx cy sx sy) (subs body (count (format "M %s %s" sx sy))) " Z")

      "chord"
      (str body " Z")

      body)))

(defn- draw-ops
  "The vector-draw ops ONE PIL draw call expands to. `fc`/`oc` are the resolved
   `#rrggbbaa` fill/outline strings (or nil); `opts` carries the rest."
  [op pts fc oc opts]
  (let [width
        (int (or (get opts "width") 1))

        stroke
        (fn [c]
          {:stroke c :stroke-width (max 1 width) :cap :butt :join :miter})]

    (case (str op)
      "point"
      (when fc
        (mapv (fn [[x y]]
                {:op :rect :x x :y y :w 1 :h 1 :fill fc})
              (partition 2 pts)))

      "line"
      (when fc
        ;; +0.5 puts an odd-width stroke on the pixel CENTRE line, so a
        ;; horizontal/vertical 1px line lands crisp instead of half-covering
        ;; two rows the way a boundary-aligned path would.
        (let [o (if (odd? (max 1 width)) 0.5 0.0)]
          [(merge {:op :polyline
                   :points (mapv (fn [[x y]]
                                   [(+ (double x) o) (+ (double y) o)])
                                 (partition 2 pts))}
                  (stroke fc))]))

      "rectangle"
      (let [[x0 y0 x1 y1]
            pts

            rx
            (min (double x0) (double x1))

            ry
            (min (double y0) (double y1))

            rw
            (Math/abs (- (double x1) (double x0)))

            rh
            (Math/abs (- (double y1) (double y0)))]

        (cond-> []
          fc
          (conj {:op :rect :x rx :y ry :w (inc rw) :h (inc rh) :fill fc})

          oc
          (conj (merge {:op :rect :x (+ rx 0.5) :y (+ ry 0.5) :w (max 1.0 rw) :h (max 1.0 rh)}
                       (stroke oc)))))

      "ellipse"
      (let [[x0 y0 x1 y1]
            pts

            w
            (- (double x1) (double x0))

            hh
            (- (double y1) (double y0))]

        (cond-> []
          fc
          (conj {:op :ellipse
                 :cx (+ (double x0) (/ (inc w) 2.0))
                 :cy (+ (double y0) (/ (inc hh) 2.0))
                 :rx (/ (inc w) 2.0)
                 :ry (/ (inc hh) 2.0)
                 :fill fc})

          oc
          (conj (merge {:op :ellipse
                        :cx (+ (double x0) (/ w 2.0))
                        :cy (+ (double y0) (/ hh 2.0))
                        :rx (max 0.5 (/ w 2.0))
                        :ry (max 0.5 (/ hh 2.0))}
                       (stroke oc)))))

      "polygon"
      (let [poly (mapv vec (partition 2 pts))]
        (cond-> []
          fc
          (conj {:op :polygon :points poly :close true :fill fc})

          oc
          (conj (merge {:op :polygon :points poly :close true} (stroke oc)))))

      ("arc" "chord" "pieslice")
      (let [[x0 y0 x1 y1]
            pts

            d
            (arc-path (str op)
                      x0
                      y0
                      x1
                      y1
                      (double (or (get opts "start") 0))
                      (double (or (get opts "end") 0)))

            line-c
            (or oc (when (= (str op) "arc") fc))]

        (cond-> []
          (and fc (not= (str op) "arc"))
          (conj {:op :path :d d :fill fc})

          line-c
          (conj (merge {:op :path :d d} (stroke line-c)))))

      "text"
      (let [[x y]
            pts

            size
            (double (or (get opts "font_size") 12))

            {:keys [family weight italic]}
            (resolve-font (get opts "font"))]

        [(cond-> {:op :text
                  :text (str (get opts "text"))
                  :x x
                  :y (+ (double y) (Math/round (* 0.8 size)))
                  :size size
                  :family family
                  :fill (or fc (->hex (->argb [0 0 0] "RGB")))}
           weight
           (assoc :weight weight)

           italic
           (assoc :italic true))])

      nil)))

(defn- read-draw-op
  "Decode ONE op record of a flat draw batch starting at `i`: name, n-coords,
   coords..., n-opts, then key/value pairs. Returns [next-index ops]."
  [b ^long i]
  (let [nc
        (long (nth b (inc i)))

        c0
        (+ i 2)

        pts
        (mapv #(double (nth b %)) (range c0 (+ c0 nc)))

        ko
        (+ c0 nc)

        nkv
        (long (nth b ko))

        kv
        (inc ko)

        opts
        (persistent! (reduce (fn [m p]
                               (let [o (+ kv (* 2 (long p)))]
                                 (assoc! m (str (nth b o)) (nth b (inc o)))))
                             (transient {})
                             (range nkv)))

        hex
        (fn [k]
          (when-let [v (get opts k)]
            (->hex (long v))))]

    [(+ kv (* 2 nkv)) (draw-ops (str (nth b i)) pts (hex "fill") (hex "outline") opts)]))

(defn- op-draws
  "Queue a whole BATCH of ImageDraw ops that the Python side buffered. Crossing
   the bridge dominates a draw: a nested list+dict per op marshals in ~35 us,
   while one flat scalar record costs ~1 us, so a run of draws crosses ONCE.
   Layout, per handle: handle, n-ops, then each op as `read-draw-op` reads it,
   with colours pre-packed as 0xAARRGGBB longs."
  [batch]
  (let [b
        (vec batch)

        n
        (count b)]

    (loop [i 0]
      (when (< i n)
        (let [h (long (nth b i))
              nops (long (nth b (inc i)))
              [j ops] (loop [j (+ i 2)
                             k 0
                             acc []]

                        (if (< k nops)
                          (let [[j2 more] (read-draw-op b j)]
                            (recur (long j2) (inc k) (into acc more)))
                          [j acc]))]

          (when (seq ops) (draw-into! h ops))
          (recur (long j)))))
    nil))

(defn- op-textbbox
  [text size font]
  (let [{:keys [family weight italic]}
        (resolve-font font)

        m
        (im/text-measure (cond-> {:text (str text) :size (double size) :family family}
                           weight
                           (assoc :weight weight)

                           italic
                           (assoc :italic true)))]

    [0 0 (long (Math/ceil (double (:width m 0)))) (long (Math/round (* 1.2 (double size))))]))

;; Bridge: name -> Clojure fn. Wrapped by `wrap-ifn` at install time (positional
;; Python args marshalled to Clojure, result back to Python). Every call is
;; enveloped [true payload] / [false message] so a host failure crosses as DATA
;; the Python shim can raise as a catchable OSError.

(defn- pil-envelope
  ;; Every host image op funnels through this envelope; failures come back to
  ;; Python as [false message] rather than a JVM stack trace. It is also the ONE
  ;; place that binds [[*scope*]], so every handle an op mints is owned by the
  ;; Context whose guest asked for it.
  [scope f]
  (binding [*scope* scope]
    (try [true (f)] (catch Throwable t [false (str (or (.getMessage t) t))]))))

(defn- pil-bridge-bindings
  "Host callables (imaging-backed) the PIL shim delegates to. All image ops go
   through here; the Python side only holds integer handles + base64 blobs.

   `env` closes over the scope ONCE. Repeating `scope` at every op instead would
   be 38 chances to forget one, and forgetting is invisible: `pil-envelope`
   turns any throw — an arity mismatch included — into a `[false message]` the
   guest sees as an OSError, so a missed site fails at RUNTIME in Python rather
   than at compile time in Clojure."
  [scope]
  (let [env (fn [f]
              (pil-envelope scope f))]
    {"__vis_pil_new__" (fn [mode w h fill]
                         (env #(op-new mode (long w) (long h) fill)))
     "__vis_pil_open__" (fn [b64]
                          (env #(op-open b64)))
     "__vis_pil_save__" (fn [h fmt quality optimize]
                          (env #(op-save h fmt quality (boolean optimize))))
     "__vis_pil_save_all__" (fn [hs fmt duration loop-count optimize]
                              (env #(op-save-all hs fmt duration loop-count (boolean optimize))))
     "__vis_pil_frames__" (fn [h]
                            (env #(op-frames h)))
     "__vis_pil_seek__" (fn [h n]
                          (env #(op-seek h (long n))))
     "__vis_pil_quantize__" (fn [h colors dither]
                              (env #(op-quantize h colors dither)))
     "__vis_pil_getpalette__" (fn [h]
                                (env #(op-getpalette h)))
     "__vis_pil_putpalette__" (fn [h data]
                                (env #(op-putpalette h data)))
     "__vis_pil_save_temp__" (fn [h fmt]
                               (env #(op-save-temp h fmt)))
     "__vis_pil_meta__" (fn [h]
                          (env #(meta-of h)))
     "__vis_pil_copy__" (fn [h]
                          (env #(op-copy h)))
     "__vis_pil_free__" (fn [h]
                          (env #(free-img! h)))
     "__vis_pil_resize__" (fn [h w hh r]
                            (env #(op-resize h (long w) (long hh) r)))
     "__vis_pil_crop__" (fn [h l t r b]
                          (env #(op-crop h (long l) (long t) (long r) (long b))))
     "__vis_pil_rotate__" (fn [h ang exp fill]
                            (env #(op-rotate h ang exp fill)))
     "__vis_pil_transpose__" (fn [h m]
                               (env #(op-transpose h m)))
     "__vis_pil_convert__" (fn [h t dither]
                             (env #(op-convert h t (if (nil? dither) 3 (long dither)))))
     "__vis_pil_getpixel__" (fn [h x y]
                              (env #(op-getpixel h x y)))
     "__vis_pil_putpixel__" (fn [h x y c]
                              (env #(op-putpixel h x y c)))
     "__vis_pil_paste__" (fn [d s x y m]
                           (env #(op-paste d s x y m)))
     "__vis_pil_getbbox__" (fn [h]
                             (env #(op-getbbox h)))
     "__vis_pil_histogram__" (fn [h mask]
                               (env #(op-histogram h mask)))
     "__vis_pil_tobytes__" (fn [h]
                             (env #(op-tobytes h)))
     "__vis_pil_frombytes__" (fn [mode w h b64]
                               (env #(op-frombytes mode (long w) (long h) b64)))
     "__vis_pil_point__" (fn [h lut]
                           (env #(op-point h lut)))
     "__vis_pil_conv__" (fn [h size ker sc off]
                          (env #(op-conv h (long size) ker sc off)))
     "__vis_pil_rank__" (fn [h size rank]
                          (env #(op-rank h (long size) (long rank))))
     "__vis_pil_blend__" (fn [a b t]
                           (env #(op-blend a b t)))
     "__vis_pil_composite__" (fn [a b m]
                               (env #(op-composite a b m)))
     "__vis_pil_chop__" (fn [op a b]
                          (env #(op-chop op a b)))
     "__vis_pil_split__" (fn [h]
                           (env #(op-split h)))
     "__vis_pil_merge__" (fn [mode hs]
                           (env #(op-merge mode hs)))
     "__vis_pil_draws__" (fn [batch]
                           (env #(op-draws batch)))
     "__vis_pil_textbbox__" (fn [text size font]
                              (env #(op-textbbox text size font)))
     "__vis_pil_offset__" (fn [h dx dy]
                            (env #(op-offset h dx dy)))
     "__vis_pil_alpha_composite__" (fn [d s dx dy]
                                     (env #(op-alpha-composite d s dx dy)))
     "__vis_pil_transform__" (fn [h ow oh method coeffs fill]
                               (env #(op-transform h (long ow) (long oh) method coeffs fill)))}))


(def vis-extension
  (vis/extension
    {:ext/name "foundation-shim-pil"
     :ext/description
     (str "Sandbox Pillow (`PIL`) subset backed by com.blockether/imaging's Rust renderer (no "
          "java.desktop/pip/native wheel): Image, ImageDraw, ImageFilter, ImageOps, ImageColor, "
          "ImageEnhance, ImageChops, ImageStat/Math/Font and related APIs; transforms, "
          "compositing, drawing, filters, palettes, EXIF, JPEG/WebP quality, and animated GIF "
          "read/write. `Image.show()` attaches inline; images above 512 MiB RGBA are rejected.")
     :ext/version "0.1.0"
     :ext/author "Blockether"
     :ext/owner "vis"
     :ext/license "Apache-2.0"
     :ext/kind "foundation"
     :ext/sandbox-shims
     [{:shim/name "pil"
       :shim/imports ["PIL"]
       :shim/description
       (str
         "Pillow-compatible `PIL` backed by a Rust renderer, no java.desktop or pip: Image, ImageDraw, "
         "ImageFilter, ImageOps, ImageFont, ImageMath, ImagePalette, ExifTags/TiffTags and the rest of "
         "Pillow 10's module set. Rejects images over 512 MiB RGBA; samples are 8-bit and a saved file "
         "reopens as 'RGB'/'RGBA'. Full surface: `doc(\"pil\")`.")
       :shim/docs
       (str
         "Pillow-compatible `PIL` published into `sys.modules`, backed by com.blockether/imaging's Rust "
         "renderer -- no java.desktop, no pip, no native wheel. Pillow 10's module set is there: Image, "
         "ImageDraw, ImageOps, ImageChops, ImageColor, ImageEnhance, ImageStat, ImageFilter (builtin "
         "kernels carry `filterargs`, plus Kernel and Color3DLUT with `generate`/`transform`), ImageMath "
         "(`lambda_eval`/`unsafe_eval` and the `ops` table), ImageFont, ImagePalette, ImagePath, "
         "ImageMorph, ImageSequence, ImageFile (`Parser`, PyDecoder/PyEncoder), ExifTags and TiffTags as "
         "real enums with a typed `lookup()`, `features`, ImageShow, ImageTk/ImageWin/ImageQt/PSDraw. "
         "Transforms, compositing, drawing, text, filters, palettes, quantization, EXIF, JPEG/WebP "
         "quality and animated GIF read/write all run on the host. `Image.show()` ATTACHES inline "
         "instead of opening a desktop viewer, and no viewer is registered by default, so "
         "`ImageShow.show()` answers False until `ImageShow.register` adds one. Images above 512 MiB "
         "RGBA are rejected. Samples are 8-bit, so 'I'/'F' pixels clamp to 0-255, drawing is antialiased "
         "where Pillow is hard-edged, and a saved file reopens as 'RGB'/'RGBA'. An image compares by "
         "VALUE like Pillow's but stays hashable by identity, because the host resource registry holds "
         "it. Not supported: `ImageCms`, the C `_imaging` internals (`im`, `tile`, `PyAccess`) and the "
         "per-format plugin modules.")
       :shim/bindings pil-bridge-bindings
       :shim/resources {::images {:resource/label "PIL image"
                                  ;; A decoded image is an on-heap int[] of w*h*4 — 48 MB for one
                                  ;; 4000x3000 frame — so this is the most expensive thing a
                                  ;; guest can forget to close.
                                  :resource/release (fn [h _entry]
                                                      (swap! pending-draws dissoc (long h))
                                                      (some-> (take-draw! h)
                                                              im/close!))
                                  :resource/max 256}}
       :shim/source "vis-shims/pil.py"}]}))

(vis/register-extension! vis-extension)
