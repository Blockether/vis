(ns com.blockether.vis.internal.image-convert-test
  "Container conversion, and above all the GEOMETRY of it.

   vis never optimizes an image, but a vector document has no pixels at all, so
   SVG -> PNG is a real rendering decision: how big, and in what proportion. A
   squashed chart is a WRONG chart and neither a human nor a model can tell it
   was squashed, so every case here asserts the rasterized size against the
   size the document actually declares -- including the ones jsvg reports
   badly (`width=\"100%\"`), the ones it cannot report at all (no attributes),
   and the ceiling that scales a huge canvas down.

   The pixel probes exist for the same reason: identical dimensions still allow
   a mirrored, offset or stretched drawing, so the content is checked where it
   must land."
  (:require [com.blockether.vis.internal.image-convert :as image-convert]
            [lazytest.core :refer [defdescribe describe expect it]])
  (:import (java.awt.image BufferedImage)
           (java.io ByteArrayInputStream ByteArrayOutputStream)
           (java.util.zip GZIPOutputStream)
           (javax.imageio ImageIO)))

;; =============================================================================
;; Helpers
;; =============================================================================

(defn- doc
  "An SVG document with `attrs` on the root and `body` inside it."
  ^String [attrs & body]
  (str "<svg xmlns=\"http://www.w3.org/2000/svg\" " attrs ">" (apply str body) "</svg>"))

(defn- svg-bytes ^bytes [^String markup] (.getBytes markup "UTF-8"))

(defn- gzip
  ^bytes [^bytes data]
  (let [baos (ByteArrayOutputStream.)]
    (with-open [out (GZIPOutputStream. baos)]
      (.write out data))
    (.toByteArray baos)))

(defn- raster
  "Rasterize `markup`, returning the whole result map."
  ([markup] (raster markup nil))
  ([markup opts] (image-convert/rasterize-svg (svg-bytes markup) opts)))

(defn- size
  "Reported [width height] of the rendered PNG."
  ([markup] (size markup nil))
  ([markup opts]
   (when-let [r (raster markup opts)]
     [(:width r) (:height r)])))

(defn- decode ^BufferedImage [^bytes data] (ImageIO/read (ByteArrayInputStream. data)))

(defn- decoded-size
  "[width height] read back out of the encoded PNG -- the reported size is a
   claim, this is the fact."
  [^bytes data]
  (let [img (decode data)]
    [(.getWidth img) (.getHeight img)]))

(defn- rendered ^BufferedImage [markup] (decode (:bytes (raster markup))))

(defn- px
  "Pixel at (x, y) as `\"rrggbb\"`."
  [^BufferedImage img x y]
  (format "%06x" (bit-and (.getRGB img (int x) (int y)) 0xffffff)))

(defn- raster-bytes
  "PNG/BMP/... bytes of a `w` x `h` image filled with `rgb`, in `fmt`."
  ^bytes [fmt w h rgb]
  (let
    [img
     (BufferedImage. (int w) (int h) BufferedImage/TYPE_INT_RGB)

     g
     (.createGraphics img)

     baos
     (ByteArrayOutputStream.)]

    (try (.setColor g (java.awt.Color. (int rgb)))
         (.fillRect g 0 0 (int w) (int h))
         (finally (.dispose g)))
    (ImageIO/write img ^String fmt baos)
    (.toByteArray baos)))

(def ^:private white "ffffff")
(def ^:private red "ff0000")
(def ^:private blue "0000ff")

;; =============================================================================
;; Declared size -> raster size
;; =============================================================================

(def ^:private size-cases
  "[label markup expected-size]. Every expectation is the size a browser would
   give the same standalone document."
  [["absolute px width/height" (doc "width=\"400\" height=\"100\"") [400 100]]
   ["square" (doc "width=\"256\" height=\"256\"") [256 256]]
   ["portrait" (doc "width=\"100\" height=\"400\"") [100 400]]
   ["explicit px unit" (doc "width=\"400px\" height=\"100px\"") [400 100]]
   ["fractional size rounds, ratio kept" (doc "width=\"100.5\" height=\"50.25\"") [101 50]]
   ["viewBox only -- the viewBox IS the size" (doc "viewBox=\"0 0 400 100\"") [400 100]]
   ["viewBox with a non-zero origin -- only the extent counts" (doc "viewBox=\"10 20 400 100\"")
    [400 100]]
   ["width/height agreeing with the viewBox wins over the viewBox"
    (doc "width=\"800\" height=\"200\" viewBox=\"0 0 400 100\"") [800 200]]
   ["width/height DISAGREEING with the viewBox is deliberate letterboxing"
    (doc "width=\"400\" height=\"100\" viewBox=\"0 0 100 100\"") [400 100]]
   ["percentage size resolves against the viewBox, NOT to 100px"
    (doc "width=\"100%\" height=\"100%\" viewBox=\"0 0 640 480\"") [640 480]]
   ["half-percentage keeps the 4:3 ratio"
    (doc "width=\"50%\" height=\"50%\" viewBox=\"0 0 640 480\"") [320 240]]
   ["physical units convert to px" (doc "width=\"10cm\" height=\"5cm\" viewBox=\"0 0 200 100\"")
    [378 189]] ["no size at all -- the SVG default canvas" (doc "") [300 150]]
   ["zero size falls back to the viewBox" (doc "width=\"0\" height=\"0\" viewBox=\"0 0 30 60\"")
    [30 60]]
   ["negative size falls back to the viewBox"
    (doc "width=\"-10\" height=\"-5\" viewBox=\"0 0 20 10\"") [20 10]]])

(defdescribe rasterize-size-test
             (describe
               "declared size"
               (it "is reproduced 1:1, ratio and all"
                   (doseq [[label markup expected] size-cases]
                     (expect (= expected (size markup)) label)))
               (it "matches the pixels actually encoded into the PNG"
                   (doseq [[label markup expected] size-cases]
                     (expect (= expected (decoded-size (:bytes (raster markup)))) label)))
               (it "is reported back as :original-width / :original-height when nothing is scaled"
                   (doseq [[label markup [w h]] size-cases]
                     (let [r (raster markup)]
                       (expect (= [w h] [(:original-width r) (:original-height r)]) label))))
               (it "survives the .svgz gzip container unchanged"
                   (let
                     [markup
                      (doc "width=\"400\" height=\"100\" viewBox=\"0 0 400 100\""
                           "<rect width=\"400\" height=\"100\" fill=\"#ff0000\"/>")

                      plain
                      (raster markup)

                      zipped
                      (image-convert/rasterize-svg (gzip (svg-bytes markup)) nil)]

                     (expect (= [400 100] [(:width zipped) (:height zipped)]))
                     (expect (= (seq (:bytes plain)) (seq (:bytes zipped))))))))

;; =============================================================================
;; The ceiling: scaled, never squashed
;; =============================================================================

(defdescribe
  raster-ceiling-test
  (describe "a document larger than the ceiling"
            (it "scales BOTH edges by the same factor"
                (expect (= [4096 1024] (size (doc "width=\"8000\" height=\"2000\"")))))
            (it "keeps the ratio for a portrait document too"
                (expect (= [1024 4096] (size (doc "width=\"2000\" height=\"8000\"")))))
            (it "keeps an extreme ratio renderable -- never a zero-height canvas"
                (let [[w h] (size (doc "width=\"20000\" height=\"10\""))]
                  (expect (= image-convert/svg-max-raster-dimension w))
                  (expect (pos? h))))
            (it "reports the pre-ceiling size as the original"
                (let [r (raster (doc "width=\"8000\" height=\"2000\""))]
                  (expect (= [8000 2000] [(:original-width r) (:original-height r)]))
                  (expect (= [4096 1024] [(:width r) (:height r)]))))
            (it "honours an explicit :max-dimension, still proportionally"
                (expect (= [64 16] (size (doc "width=\"800\" height=\"200\"") {:max-dimension 64})))
                (expect (= [16 64]
                           (size (doc "width=\"200\" height=\"800\"") {:max-dimension 64})))))
  (describe "a document smaller than the ceiling"
            (it "is NOT upscaled -- conversion is not optimization"
                (expect (= [16 8] (size (doc "width=\"16\" height=\"8\""))))
                (expect (= [1 1] (size (doc "width=\"1\" height=\"1\"")))))))

;; =============================================================================
;; Geometry: the drawing lands where the document put it
;; =============================================================================

(defdescribe
  rasterize-content-test
  (describe
    "rendered content"
    (it "maps left/right without mirroring or stretching"
        (let
          [img (rendered (doc "width=\"400\" height=\"100\" viewBox=\"0 0 400 100\""
                              "<rect width=\"200\" height=\"100\" fill=\"#ff0000\"/>"
                              "<rect x=\"200\" width=\"200\" height=\"100\" fill=\"#0000ff\"/>"))]
          (expect (= red (px img 50 50)))
          (expect (= blue (px img 350 50)))))
    (it "maps top/bottom the same way"
        (let
          [img (rendered (doc "width=\"100\" height=\"400\" viewBox=\"0 0 100 400\""
                              "<rect width=\"100\" height=\"200\" fill=\"#ff0000\"/>"
                              "<rect y=\"200\" width=\"100\" height=\"200\" fill=\"#0000ff\"/>"))]
          (expect (= red (px img 50 50)))
          (expect (= blue (px img 50 350)))))
    (it "letterboxes a square viewBox inside a wide viewport instead of stretching it"
        (let
          [img (rendered (doc "width=\"400\" height=\"100\" viewBox=\"0 0 100 100\""
                              "<rect width=\"100\" height=\"100\" fill=\"#008000\"/>"))]
          (expect (= [400 100] [(.getWidth img) (.getHeight img)]))
          (expect (= "008000" (px img 200 50)) "centred content")
          (expect (= white (px img 10 50)) "left bar")
          (expect (= white (px img 390 50)) "right bar")))
    (it "keeps the geometry after the ceiling scales the canvas down"
        (let
          [img (rendered (doc
                           "width=\"8000\" height=\"2000\""
                           "<rect width=\"4000\" height=\"2000\" fill=\"#ff0000\"/>"
                           "<rect x=\"4000\" width=\"4000\" height=\"2000\" fill=\"#0000ff\"/>"))]
          (expect (= [4096 1024] [(.getWidth img) (.getHeight img)]))
          (expect (= red (px img 100 500)))
          (expect (= blue (px img 4000 500)))))
    (it "resolves a percentage document against the viewBox, drawing at full size"
        (let
          [img (rendered (doc "width=\"100%\" height=\"100%\" viewBox=\"0 0 640 480\""
                              "<rect width=\"320\" height=\"480\" fill=\"#ff0000\"/>"
                              "<rect x=\"320\" width=\"320\" height=\"480\" fill=\"#0000ff\"/>"))]
          (expect (= [640 480] [(.getWidth img) (.getHeight img)]))
          (expect (= red (px img 100 240)))
          (expect (= blue (px img 540 240)))))
    (it "flattens transparency onto WHITE, not onto black-on-black"
        (let
          [img (rendered (doc "width=\"20\" height=\"20\""
                              "<circle cx=\"10\" cy=\"10\" r=\"2\" fill=\"#000000\"/>"))]
          (expect (= white (px img 0 0)))
          (expect (= white (px img 19 19)))
          (expect (= "000000" (px img 10 10)))))
    (it "renders a text document without dying on the font stack"
        (let
          [r (raster (doc "width=\"200\" height=\"40\""
                          "<text x=\"4\" y=\"28\" font-size=\"20\">vis</text>"))]
          (expect (some? r))
          (expect (= [200 40] [(:width r) (:height r)]))))))

;; =============================================================================
;; Untrusted input: nothing renders is better than something escapes
;; =============================================================================

(defdescribe
  rasterize-safety-test
  (describe
    "hostile or broken markup"
    (it "returns nil instead of throwing"
        (expect (nil? (raster "not an svg at all")))
        (expect (nil? (raster "<svg")))
        (expect (nil? (raster (doc "width=\"10\" height=\"10\"" "<rect"))))
        (expect (nil? (image-convert/rasterize-svg (byte-array 0) nil)))
        (expect (nil? (image-convert/rasterize-svg nil nil)))
        (expect (nil? (image-convert/rasterize-svg (byte-array [0x1f 0x8b 0x08 0x00]) nil))
                "truncated gzip"))
    (it "does not resolve an external XML entity into the picture (XXE)"
        (let
          [markup
           (str "<?xml version=\"1.0\"?>"
                "<!DOCTYPE svg [<!ENTITY xxe SYSTEM \"file:///etc/passwd\">]>"
                (doc "width=\"200\" height=\"40\""
                     "<text x=\"2\" y=\"30\" font-size=\"12\">&xxe;</text>"))

           img
           (rendered markup)]

          (expect (= [200 40] [(.getWidth img) (.getHeight img)]))
          (expect (every? #(= white (px img % 20)) (range 0 200 4))
                  "an empty canvas -- no file content was drawn")))
    (it "does not fetch an external <image href> (SSRF / local file read)"
        (let
          [img (rendered
                 (str "<svg xmlns=\"http://www.w3.org/2000/svg\""
                      " xmlns:xlink=\"http://www.w3.org/1999/xlink\""
                      " width=\"40\" height=\"20\">"
                      "<image xlink:href=\"file:///etc/passwd\" width=\"40\" height=\"20\"/>"
                      "<image xlink:href=\"http://127.0.0.1:1/x.png\" width=\"40\" height=\"20\"/>"
                      "</svg>"))]
          (expect (= [40 20] [(.getWidth img) (.getHeight img)]))
          (expect (= white (px img 20 10)))))
    (it "does not expand a billion-laughs entity bomb"
        (let
          [entities
           (str "<!ENTITY lol \"lol\">"
                (apply str
                  (for [i (range 1 10)]
                    (str "<!ENTITY lol"
                         i
                         " \""
                         (apply str (repeat 10 (if (= i 1) "&lol;" (str "&lol" (dec i) ";"))))
                         "\">"))))

           markup
           (str "<?xml version=\"1.0\"?><!DOCTYPE svg [" entities
                "]>" (doc "width=\"40\" height=\"20\"" "<text>&lol9;</text>"))

           start
           (System/nanoTime)

           r
           (raster markup)

           elapsed-ms
           (quot (- (System/nanoTime) start) 1000000)]

          (expect (or (nil? r) (= [40 20] [(:width r) (:height r)])))
          (expect (< elapsed-ms 5000) "finished promptly instead of expanding 10^9 nodes")))))

;; =============================================================================
;; to-provider-safe
;; =============================================================================

(defdescribe
  to-provider-safe-test
  (describe "a container the wire already accepts"
            (it "comes back byte-IDENTICAL, never re-encoded"
                (doseq [mt ["image/png" "image/jpeg" "image/gif" "image/webp"]]
                  (let
                    [data (raster-bytes "png" 4 4 0xff0000)
                     r (image-convert/to-provider-safe data mt)]

                    (expect (identical? data (:bytes r)) mt)
                    (expect (= mt (:media-type r)) mt)
                    (expect (= (alength data) (:size r)) mt))))
            (it "normalizes a padded / upper-case media type"
                (let [data (raster-bytes "png" 4 4 0xff0000)]
                  (expect (identical? data
                                      (:bytes (image-convert/to-provider-safe data
                                                                              "  IMAGE/PNG ")))))))
  (describe "a raster container the wire refuses"
            (it "becomes PNG at exactly the same dimensions"
                (let
                  [bmp
                   (raster-bytes "bmp" 37 11 0x00ff00)

                   r
                   (image-convert/to-provider-safe bmp "image/bmp")]

                  (expect (= "image/png" (:media-type r)))
                  (expect (= [37 11] [(:width r) (:height r)]))
                  (expect (= [37 11] (decoded-size (:bytes r))))
                  (expect (= "00ff00" (px (decode (:bytes r)) 18 5)))))
            (it "returns nil for a payload nothing can decode"
                (expect (nil? (image-convert/to-provider-safe (.getBytes "not an image" "UTF-8")
                                                              "image/heic")))))
  (describe "an SVG payload"
            (it "is routed through the rasterizer, ratio intact"
                (doseq [mt image-convert/svg-media-types]
                  (let
                    [r (image-convert/to-provider-safe
                         (svg-bytes (doc "width=\"100%\" height=\"100%\" viewBox=\"0 0 400 100\""
                                         "<rect width=\"400\" height=\"100\" fill=\"#ff0000\"/>"))
                         mt)]
                    (expect (= "image/png" (:media-type r)) mt)
                    (expect (= [400 100] [(:width r) (:height r)]) mt))))
            (it "recognises its media types, and only those"
                (expect (image-convert/svg-media-type? "image/svg+xml"))
                (expect (image-convert/svg-media-type? " Image/SVG+XML "))
                (expect (not (image-convert/svg-media-type? "image/png")))
                (expect (not (image-convert/svg-media-type? nil)))))
  (describe "with conversion disabled (no AWT/ImageIO stack)"
            (it "returns nil so the caller skips the attachment"
                (binding [image-convert/*enabled?* false]
                  (expect (nil? (image-convert/to-provider-safe (raster-bytes "png" 4 4 0xff0000)
                                                                "image/png")))
                  (expect (nil? (raster (doc "width=\"10\" height=\"10\""))))))))
