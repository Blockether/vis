(ns com.blockether.vis.internal.image-convert
  "Container conversion for attachment payloads -- NOT optimization.

   vis stores and replays attachment bytes VERBATIM: whatever the user dropped
   is what is stored and what the provider sees. The one thing that cannot be
   passed through is a container no vision wire accepts (`attachments/
   provider-image-media-types`): a BMP is a perfectly good image and an SVG is
   a perfectly good figure, and both are a hard 400 on every provider. Since an
   attachment REPLAYS on every later turn, one such row kills the whole session.

   So this namespace does exactly one thing: turn bytes a provider REFUSES into
   the same picture in a container it ACCEPTS.

     * raster (BMP, or anything else ImageIO can decode) -> PNG, 1:1, lossless
     * vector (`.svg` / gzipped `.svgz`)                 -> rendered PNG

   No downscaling, no quality knob, no size heuristics -- image optimization was
   removed deliberately and is not hiding here. Every failure mode (undecodable
   payload, missing AWT/ImageIO stack, malformed markup) returns nil, never
   throws; the caller then SKIPS the attachment rather than shipping bytes the
   provider refuses."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.awt-boot :as awt-boot])
  (:import (com.github.weisj.jsvg SVGDocument)
           (com.github.weisj.jsvg.parser LoaderContext SVGLoader)
           (com.github.weisj.jsvg.parser.resources ResourcePolicy)
           (com.github.weisj.jsvg.view FloatSize ViewBox)
           (java.awt Color RenderingHints)
           (java.awt.image BufferedImage)
           (java.io ByteArrayInputStream ByteArrayOutputStream InputStream)
           (java.net URI)
           (java.util.zip GZIPInputStream)
           (java.util.logging Level Logger)
           (javax.imageio ImageIO)))

;; =============================================================================
;; Media types
;; =============================================================================

(def svg-media-types
  "Vector containers vis RASTERIZES on the way in instead of refusing. No
   provider reads SVG -- it is markup, not pixels -- but unlike a HEIC the
   pixels are recoverable: the document is rendered here, so a matplotlib
   `savefig(format=\"svg\")` or a dropped icon still reaches the model."
  #{"image/svg+xml" "image/svg" "application/svg+xml" "text/svg+xml"})

(defn svg-media-type?
  [media-type]
  (contains? svg-media-types (str/lower-case (str/trim (str media-type)))))

(def ^:dynamic *enabled?*
  "Kill switch -- bind false to make conversion unavailable (what a build with
   no AWT/ImageIO stack looks like)."
  true)

;; =============================================================================
;; SVG rendering (AWT / ImageIO -- absent in the macOS native image, hence guarded)
;; =============================================================================

(def wire-safe-media-types
  "Containers that already need NO conversion (mirrors
   `attachments/provider-image-media-types`). Kept here so
   [[to-provider-safe]] can hand these back byte-identical instead of decoding
   and re-encoding a payload that was already fine."
  #{"image/jpeg" "image/png" "image/gif" "image/webp"})

(def ^:const svg-max-raster-dimension
  "Hard ceiling on the rendered long edge. A vector document can declare any
   size at all, and a 40000px canvas is an OOM, not a picture."
  4096)

(defonce ^:private quiet-jsvg!
  ;; jsvg reports a malformed document by LOGGING a stack trace through
  ;; java.util.logging and returning nil. On a TUI that console handler paints
  ;; over the rendered frame, and the nil already tells us everything, so the
  ;; whole logger family is silenced once, lazily.
  (delay (.setLevel (Logger/getLogger "com.github.weisj.jsvg") Level/OFF) true))

(def ^:private awt-ready!
  ;; Shared with the PIL / matplotlib shims: every Java2D user in vis needs the
  ;; same RUNTIME headless + font bootstrap (see `internal.awt-boot`).
  (delay (awt-boot/ensure!)))

(defn- encode-png
  ^bytes [^BufferedImage img]
  (let [baos (ByteArrayOutputStream.)]
    (when (ImageIO/write img "png" baos) (.toByteArray baos))))

(defn- svg-stream
  "SVG bytes as a stream, transparently gunzipping the `.svgz` container."
  ^InputStream [^bytes data]
  (if (and (>= (alength data) 2)
           (= 0x1f (bit-and (long (aget data 0)) 0xff))
           (= 0x8b (bit-and (long (aget data 1)) 0xff)))
    (GZIPInputStream. (ByteArrayInputStream. data))
    (ByteArrayInputStream. data)))

(defn- svg-loader-context
  "Loader context that DENIES every external resource. An attached SVG is
   untrusted input and `<image href>` / `url(...)` would otherwise let it read
   local files or call out over the network while vis renders it."
  ^LoaderContext []
  (.build (.externalResourcePolicy (LoaderContext/builder) ResourcePolicy/DENY_ALL)))

(defn- svg-document
  "Parsed document, or nil when the markup is not a usable SVG."
  ^SVGDocument [^bytes data]
  (let [^URI base nil]
    (with-open [in (svg-stream data)]
      (.load (SVGLoader.) ^InputStream in base (svg-loader-context)))))

(defn- usable-dims
  "`[w h]` as positive finite doubles, or nil -- a document may declare 0, a
   negative, or NaN, and every one of those is a broken canvas, not a size."
  [dims]
  (when-let [[w h] dims]
    (let
      [w (double w)
       h (double h)]

      (when (and (pos? w)
                 (pos? h)
                 (not (Double/isNaN w))
                 (not (Double/isNaN h))
                 (not (Double/isInfinite w))
                 (not (Double/isInfinite h)))
        [w h]))))

(defn rasterize-svg
  "Render an SVG (`.svg` or gzipped `.svgz`) into a PNG every vision wire
   accepts. Returns `{:bytes :media-type :size :original-size :width :height
   :original-width :original-height}`, or nil when the document does not parse,
   AWT/ImageIO is missing (GraalVM native-image on macOS) or conversion is
   disabled. Never throws.

   The ASPECT RATIO is the whole game: a squashed chart is a wrong chart, and
   the model cannot tell the difference. The raster size is the document's own
   resolved size -- percentages resolved against the `viewBox`, not jsvg's raw
   `.size`, which reports `width=\"100%\"` as 100px -- and the only scaling that
   ever happens is the [[svg-max-raster-dimension]] ceiling, one factor applied
   to both edges.

   The canvas is filled WHITE first: SVG figures routinely leave the background
   transparent, and a transparent PNG of black axes is what a viewer -- or a
   model -- sees as an empty image."
  ([^bytes data] (rasterize-svg data nil))
  ([^bytes data {:keys [max-dimension]}]
   (when (and *enabled?* data (pos? (alength data)))
     (try
       @quiet-jsvg!
       @awt-ready!
       (when-let [doc (svg-document data)]
         (let
           [max-dim (long (or max-dimension svg-max-raster-dimension))
            ^ViewBox vb (.viewBox doc)
            vb-dims (when vb [(.width vb) (.height vb)])
            ;; `.size` LIES about a RELATIVE width: `width="100%"` comes back
            ;; as 100px, so a `viewBox="0 0 640 480"` figure would rasterize as
            ;; a 100x100 square -- the aspect ratio destroyed before a single
            ;; pixel is drawn. `sizeForViewport` resolves percentages against
            ;; the viewBox (the only viewport a standalone document has) and
            ;; hands absolute sizes straight back.
            vp-dims (when-let [[vw vh] (usable-dims vb-dims)]
                      (let
                        [^FloatSize s (.sizeForViewport
                                        doc
                                        (ViewBox. (float 0) (float 0) (float vw) (float vh)))]
                        [(.width s) (.height s)]))
            ^FloatSize size (.size doc)
            ;; Resolved viewport size, then the viewBox, then whatever the
            ;; document claims, then the SVG default canvas.
            [iw ih] (or (usable-dims vp-dims)
                        (usable-dims vb-dims)
                        (usable-dims [(.width size) (.height size)])
                        [300.0 150.0])
            iw (double iw)
            ih (double ih)
            longest (max iw ih)
            ;; 1:1 with the document's own declared size. Rasterizing needs SOME
            ;; pixel count, and the honest one is the size the author wrote; the
            ;; ceiling only exists because a 40000px canvas is an OOM -- and it
            ;; scales BOTH edges by the SAME factor, so the ratio survives it.
            target (double (min (double max-dim) longest))
            scale (/ target longest)
            tw (max 1 (long (Math/round (* iw scale))))
            th (max 1 (long (Math/round (* ih scale))))
            img (BufferedImage. (int tw) (int th) BufferedImage/TYPE_INT_RGB)
            g (.createGraphics img)]

           (try
             (.setRenderingHint g RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON)
             (.setRenderingHint g RenderingHints/KEY_RENDERING RenderingHints/VALUE_RENDER_QUALITY)
             (.setRenderingHint g
                                RenderingHints/KEY_STROKE_CONTROL
                                RenderingHints/VALUE_STROKE_PURE)
             (.setColor g Color/WHITE)
             (.fillRect g 0 0 (int tw) (int th))
             (.scale g scale scale)
             ;; The viewport handed to jsvg is the document's own resolved size,
             ;; so `preserveAspectRatio` letterboxes rather than stretches.
             (.render doc nil g (ViewBox. (float iw) (float ih)))
             (finally (.dispose g)))
           (when-let [^bytes out (encode-png img)]
             {:bytes out
              :media-type "image/png"
              :size (alength out)
              :original-size (alength data)
              :width tw
              :height th
              :original-width (long (Math/round iw))
              :original-height (long (Math/round ih))})))
       (catch Throwable _
         ;; Unparseable markup, a document limit, or no AWT: the caller skips
         ;; the attachment rather than shipping bytes the provider refuses.
         nil)))))

;; =============================================================================
;; Raster re-containering
;; =============================================================================

(defn- transcode-png
  "Decode `data` and write it back out as PNG, pixel for pixel -- same
   dimensions, no resample, no lossy step. nil when ImageIO cannot read the
   payload (HEIC, AVIF) or write it."
  [^bytes data]
  (try @awt-ready!
       (when-let [^BufferedImage src (ImageIO/read (ByteArrayInputStream. data))]
         (when-let [^bytes out (encode-png src)]
           {:bytes out
            :media-type "image/png"
            :size (alength out)
            :original-size (alength data)
            :width (.getWidth src)
            :height (.getHeight src)
            :original-width (.getWidth src)
            :original-height (.getHeight src)}))
       (catch Throwable _ nil)))

(defn to-provider-safe
  "Re-container `data` into something every vision wire accepts (PNG). Payloads
   whose container is already wire-safe come back UNTOUCHED, byte for byte --
   this is a validity fix, never a size one. Returns the same map as
   [[rasterize-svg]], or nil when the payload can be turned into pixels by
   neither ImageIO nor the SVG renderer (HEIC, AVIF) or conversion is disabled
   -- the caller must then skip it."
  [^bytes data media-type]
  (when (and *enabled?* data (pos? (alength data)))
    (let [mt (str/lower-case (str/trim (str media-type)))]
      (cond (contains? wire-safe-media-types mt)
            {:bytes data :media-type mt :size (alength data) :original-size (alength data)}
            (svg-media-type? mt) (rasterize-svg data nil)
            :else (transcode-png data)))))
