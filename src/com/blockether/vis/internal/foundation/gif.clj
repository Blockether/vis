(ns com.blockether.vis.internal.foundation.gif
  "Multi-frame GIF (87a/89a) codec, delegating every byte of the GIF format to
  the `com.blockether/imaging` native cdylib (Rust `image` crate): LZW, palette
  quantization, interlace, disposal and NETSCAPE looping -- all of it lives there
  now.

  This used to be a hand-rolled pure-Clojure codec (its own LZW decode/encode,
  sub-block framing, disposal compositor, median-cut-free palette path). That
  could not survive the `image` crate's decode-limit changes and duplicated work
  the cdylib already does; the native path composites every frame onto a
  full-size canvas honouring GIF disposal (the `image` crate's own
  `GifFrameIterator`), so a caller only ever sees plain packed-0xAARRGGBB
  pixels -- exactly the shape the old [[decode]] produced."
  (:require [com.blockether.imaging :as im]))

;; ---------------------------------------------------------------------------
;; Signature + pixel packing
;; ---------------------------------------------------------------------------

(defn gif?
  "True when `data` starts with a GIF87a/GIF89a signature."
  [^bytes data]
  (boolean (and data
                (>= (alength data) 6)
                (let [^bytes d data]
                  (and (= 0x47 (bit-and (aget d 0) 0xff)) ; G
                       (= 0x49 (bit-and (aget d 1) 0xff)) ; I
                       (= 0x46 (bit-and (aget d 2) 0xff)) ; F
                       (= 0x38 (bit-and (aget d 3) 0xff)) ; 8
                       (= 0x61 (bit-and (aget d 5) 0xff))))))) ; a

(defn- rgba->argb
  "Straight RGBA8 rows (the cdylib's frame shape) -> a packed 0xAARRGGBB int[],
  the shape the PIL shim's raster holds."
  ^ints [^bytes b]
  (let
    [n
     (alength b)

     out
     (int-array (quot n 4))]

    (loop
      [i
       0

       j
       0]

      (when (< i n)
        (aset out
              j
              (unchecked-int (bit-or (bit-shift-left (bit-and (aget b (+ i 3)) 0xff) 24)
                                     (bit-shift-left (bit-and (aget b i) 0xff) 16)
                                     (bit-shift-left (bit-and (aget b (+ i 1)) 0xff) 8)
                                     (bit-and (aget b (+ i 2)) 0xff))))
        (recur (+ i 4) (inc j))))
    out))

;; ---------------------------------------------------------------------------
;; Decode / encode -- thin bridges to the native cdylib
;; ---------------------------------------------------------------------------

(defn decode
  "Decode every frame of `data` via the native `imaging` cdylib.

  Returns `{:width :height :loop-count :frames [{:delay-ms :disposal :argb}]}`
  where `:argb` is a full-canvas int[] of packed 0xAARRGGBB pixels (the cdylib
  composites each frame honoring GIF disposal, so every frame is directly
  displayable), or nil when `data` is not a GIF. `:loop-count` is -1 for loop
  forever, otherwise the iteration count."
  [^bytes data]
  (when (gif? data)
    (let [m (im/decode-gif data)]
      (-> m
          (assoc :loop-count (long (:loop-count m)))
          (update :frames
                  (fn [fs]
                    (mapv (fn [f]
                            {:delay-ms (long (:delay-ms f))
                             ;; disposal is internal to the cdylib's compositor;
                             ;; every frame is already full-canvas, so 0 (Keep).
                             :disposal 0
                             :argb (rgba->argb ^bytes (:rgba f))})
                          fs)))))))

(defn encode
  "Encode `frames` into one GIF89a byte array via the native `imaging` cdylib.

  `{:width :height :loop-count :frames}`, every frame `{:delay-ms :rgba}` where
  `:rgba` is straight RGBA8 (width*height*4 bytes). `:loop-count` nil or 0 means
  loop forever (the GIF/PIL convention); an explicit count plays that many times.
  The cdylib owns palette quantization, LZW, disposal and the NETSCAPE loop
  block."
  ^bytes [spec]
  (let
    [lc
     (:loop-count spec)

     ;; nil OR 0 both mean \"loop forever\"; the cdylib takes -1 for forever.
     lc'
     (let
       [n (some-> lc
                  long)]
       (cond (nil? n) -1
             (zero? (long n)) -1
             :else n))]

    (im/encode-gif (assoc spec :loop-count lc'))))
