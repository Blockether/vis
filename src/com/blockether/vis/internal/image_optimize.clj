(ns com.blockether.vis.internal.image-optimize
  "Automatic image shrinking for store-bound attachments.

   Images are HOT (see `attachment-storage`): they never offload to an external
   backend, so every screenshot a user drops and every plot the sandbox renders
   lands inline in the `session_attachment` BLOB and is replayed to the vision
   model for as long as its iteration stays live. A 5 MB retina PNG therefore
   costs disk forever AND upload bandwidth every turn -- while the provider
   downscales it server-side to ~1568px on the long edge anyway, so the extra
   pixels buy the model nothing.

   This namespace re-encodes such payloads before they are stored:

     1. downscale so the long edge is at most `default-max-dimension`
        (the provider's own bound -- no visual information is lost that the
        model would have seen),
     2. re-encode -- JPEG when the image is fully opaque (screenshots, plots),
        PNG when it genuinely carries transparency,
     3. keep the result ONLY when it is meaningfully smaller (`default-min-gain`),
        otherwise the original bytes pass through untouched.

   Work runs on a small daemon pool with LOW priority, off the caller's thread:
   a turn with several attachments shrinks them concurrently and never waits
   longer than `*timeout-ms*`. Every failure mode -- an undecodable payload, a
   missing AWT/ImageIO stack (GraalVM native-image on macOS), a slow encode --
   degrades to the original attachment, never to an exception."
  (:require [clojure.string :as str]
            [taoensso.telemere :as tel])
  (:import (java.awt RenderingHints)
           (java.awt.image BufferedImage)
           (java.io ByteArrayInputStream ByteArrayOutputStream)
           (java.util Base64)
           (java.util.concurrent Executors ExecutorService Future ThreadFactory TimeUnit)
           (javax.imageio IIOImage ImageIO ImageWriteParam ImageWriter)
           (javax.imageio.stream MemoryCacheImageOutputStream)))

;; =============================================================================
;; Policy
;; =============================================================================

(def ^:const default-max-dimension
  "Long-edge pixel bound. Anthropic resizes anything larger server-side and
   OpenAI's high-detail tiling caps out in the same neighbourhood, so pixels
   above this line are paid for and then thrown away."
  1568)

(def ^:const default-floor-bytes
  "Payloads below this never earn a decode/encode round-trip (64 KiB)."
  (* 64 1024))

(def ^:const default-jpeg-quality
  "JPEG quality for opaque images. High enough that UI text in a screenshot
   stays legible to a vision model."
  0.82)

(def ^:const default-min-gain
  "The re-encoded payload is kept only when it is at most this fraction of the
   original. A 5% win is not worth a lossy round-trip."
  0.85)

(def ^:const default-timeout-ms
  "Longest a caller waits for the background pool before shipping originals."
  4000)

(def optimizable-media-types
  "Raster formats worth re-encoding. GIF is excluded: it may be animated and a
   single-frame re-encode would silently drop the animation."
  #{"image/png" "image/jpeg" "image/jpg" "image/bmp" "image/webp"})

(def ^:dynamic *enabled?* "Kill switch -- bind false to store payloads verbatim." true)

(def ^:dynamic *max-dimension* default-max-dimension)
(def ^:dynamic *floor-bytes* default-floor-bytes)
(def ^:dynamic *timeout-ms* default-timeout-ms)

(defn optimizable-media-type?
  [media-type]
  (contains? optimizable-media-types (str/lower-case (str/trim (str media-type)))))

;; =============================================================================
;; Pixels (AWT / ImageIO -- absent in the macOS native image, hence guarded)
;; =============================================================================

(defn- fully-opaque?
  "True when no pixel carries partial or full transparency. Scanned row by row
   so a large image never materializes a full int[] copy. macOS window captures
   have an alpha channel that is opaque everywhere except the rounded corners --
   worth the scan, since an opaque image can take the far cheaper JPEG path."
  [^BufferedImage img]
  (if-not (.hasAlpha (.getColorModel img))
    true
    (let
      [w
       (.getWidth img)

       h
       (.getHeight img)

       row
       (int-array w)]

      (loop [y 0]
        (if (>= y h)
          true
          (do (.getRGB img 0 y w 1 row 0 w)
              (if (loop [x 0]
                    (cond (>= x w) true
                          (not= 255 (bit-and (unsigned-bit-shift-right (aget row x) 24) 0xFF)) false
                          :else (recur (unchecked-inc x))))
                (recur (unchecked-inc y))
                false)))))))

(defn- draw-into
  ^BufferedImage [^BufferedImage src ^long w ^long h ^long img-type]
  (let
    [dst
     (BufferedImage. (int w) (int h) (int img-type))

     g
     (.createGraphics dst)]

    (try (.setRenderingHint g
                            RenderingHints/KEY_INTERPOLATION
                            RenderingHints/VALUE_INTERPOLATION_BILINEAR)
         (.setRenderingHint g RenderingHints/KEY_RENDERING RenderingHints/VALUE_RENDER_QUALITY)
         (.setRenderingHint g RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON)
         (.drawImage g src 0 0 (int w) (int h) nil)
         (finally (.dispose g)))
    dst))

(defn- resample
  "Downscale `src` to fit `w`x`h`, halving progressively: one-shot bilinear over
   a large ratio aliases badly (thin UI text turns to mush), repeated halving
   does not. Also used at 1:1 to flatten an alpha channel for the JPEG writer."
  ^BufferedImage [^BufferedImage src ^long w ^long h opaque?]
  (let [img-type (if opaque? BufferedImage/TYPE_INT_RGB BufferedImage/TYPE_INT_ARGB)]
    (loop [cur src]
      (let
        [cw (.getWidth cur)
         ch (.getHeight cur)]

        (if (and (<= cw (* 2 w)) (<= ch (* 2 h)))
          (if (and (= cw w) (= ch h) (= (.getType cur) (int img-type)))
            cur
            (draw-into cur w h img-type))
          (recur
            (draw-into cur (max w (long (quot cw 2))) (max h (long (quot ch 2))) img-type)))))))

(defn- encode-png
  ^bytes [^BufferedImage img]
  (let [baos (ByteArrayOutputStream.)]
    (when (ImageIO/write img "png" baos) (.toByteArray baos))))

(defn- encode-jpeg
  ^bytes [^BufferedImage img ^double quality]
  (when-let
    [^ImageWriter writer (first (iterator-seq (ImageIO/getImageWritersByFormatName "jpeg")))]
    (let
      [baos (ByteArrayOutputStream.)
       out (MemoryCacheImageOutputStream. baos)
       param (.getDefaultWriteParam writer)]

      (try (when (.canWriteCompressed param)
             (.setCompressionMode param ImageWriteParam/MODE_EXPLICIT)
             (.setCompressionQuality param (float quality)))
           (.setOutput writer out)
           (.write writer nil (IIOImage. img nil nil) param)
           (.flush out)
           (.toByteArray baos)
           (finally (.dispose writer) (.close out))))))

;; =============================================================================
;; Optimize one payload
;; =============================================================================

(defn optimize
  "Shrink one image payload. Returns nil when the bytes are already as good as
   vis can make them (too small, undecodable, unsupported format, no ImageIO,
   or the re-encode did not beat `:min-gain`); otherwise

     {:bytes :media-type :size :original-size :width :height
      :original-width :original-height}

   Pure with respect to the caller: `data` is never mutated. Never throws."
  ([^bytes data media-type] (optimize data media-type nil))
  ([^bytes data media-type {:keys [max-dimension floor-bytes jpeg-quality min-gain]}]
   (let
     [max-dim
      (long (or max-dimension *max-dimension*))

      floor
      (long (or floor-bytes *floor-bytes*))

      quality
      (double (or jpeg-quality default-jpeg-quality))

      gain
      (double (or min-gain default-min-gain))]

     (when (and *enabled?* data (>= (alength data) floor) (optimizable-media-type? media-type))
       (try (when-let [^BufferedImage src (ImageIO/read (ByteArrayInputStream. data))]
              (let
                [w (.getWidth src)
                 h (.getHeight src)
                 longest (max w h)
                 ratio (if (> longest max-dim) (/ (double max-dim) (double longest)) 1.0)
                 tw (max 1 (long (Math/round (* (double w) ratio))))
                 th (max 1 (long (Math/round (* (double h) ratio))))
                 opaque? (fully-opaque? src)
                 img (resample src tw th opaque?)
                 out (if opaque? (encode-jpeg img quality) (encode-png img))]

                (when (and out (< (alength ^bytes out) (long (* gain (alength data)))))
                  {:bytes out
                   :media-type (if opaque? "image/jpeg" "image/png")
                   :size (alength ^bytes out)
                   :original-size (alength data)
                   :width tw
                   :height th
                   :original-width w
                   :original-height h})))
            (catch Throwable _
              ;; Undecodable payload, no AWT (native image), or an OOM-ish encode:
              ;; the original attachment is always a correct answer.
              nil))))))

;; =============================================================================
;; Optimize one attachment envelope
;; =============================================================================

(defn- retarget-filename
  "Keep `:filename` honest when the container changed."
  [filename media-type]
  (when-not (str/blank? (str filename))
    (let
      [ext
       (if (= media-type "image/jpeg") ".jpg" ".png")

       base
       (str/replace (str filename) #"\.[A-Za-z0-9]{1,5}$" "")]

      (str base ext))))

(defn optimize-attachment
  "Store-bound attachment envelope (`:base64` `:media-type` `:size`, optionally
   `:filename`) -> the same envelope with smaller bytes, or the SAME MAP when
   nothing was gained. Adds no keys: the wire shape is untouched."
  [att]
  (or (when (and (map? att) (:base64 att) (optimizable-media-type? (:media-type att)))
        (try (let [data (.decode (Base64/getDecoder) (str (:base64 att)))]
               (when-let
                 [{:keys [^bytes bytes media-type size original-size]} (optimize data
                                                                                 (:media-type att))]
                 (tel/log! {:level :debug
                            :id ::attachment-optimized
                            :data {:from original-size :to size :media-type media-type}})
                 (cond->
                   (assoc att
                     :base64 (.encodeToString (Base64/getEncoder) bytes)
                     :media-type media-type
                     :size size)
                   (retarget-filename (:filename att) media-type)
                   (assoc :filename (retarget-filename (:filename att) media-type)))))
             (catch Throwable _ nil)))
      att))

;; =============================================================================
;; Background pool
;; =============================================================================

(defn- thread-factory
  ^ThreadFactory []
  (let [n (atom 0)]
    (reify
      ThreadFactory
        (newThread [_ r]
          (doto (Thread. ^Runnable r (str "vis-image-optimize-" (swap! n inc)))
            (.setDaemon true)
            (.setPriority Thread/MIN_PRIORITY))))))

(defonce ^:private pool
  (delay (Executors/newFixedThreadPool
           (int (max 1 (min 4 (dec (.availableProcessors (Runtime/getRuntime))))))
           (thread-factory))))

(defn optimize-attachments
  "Shrink every image in a store-bound attachment seq, concurrently, on the
   background pool. Returns a vector in the SAME order. A payload that is still
   in flight after `*timeout-ms*` (total, across the batch) is shipped as-is, so
   this can delay a persist by at most that budget and never fails a turn."
  [atts]
  (let [atts (vec (or atts []))]
    (if (or (not *enabled?*)
            (empty? atts)
            (not-any? #(and (:base64 %) (optimizable-media-type? (:media-type %))) atts))
      atts
      (let
        [^ExecutorService ex @pool
         deadline (+ (System/currentTimeMillis) (long *timeout-ms*))
         ;; `bound-fn*` so a test's dynamic bindings reach the pool thread.
         futures (mapv (fn [att]
                         (when (and (:base64 att) (optimizable-media-type? (:media-type att)))
                           (try (.submit ex ^Callable (bound-fn* #(optimize-attachment att)))
                                (catch Throwable _ nil))))
                       atts)]

        (mapv (fn [att ^Future fut]
                (or (when fut
                      (try (.get fut
                                 (max 0 (- deadline (System/currentTimeMillis)))
                                 TimeUnit/MILLISECONDS)
                           (catch Throwable _ (.cancel fut true) nil)))
                    att))
              atts
              futures)))))
