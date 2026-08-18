(ns com.blockether.vis.internal.image-convert-test
  "Container conversion, and above all the GEOMETRY of it.

   vis never optimizes an image, but a vector document has no pixels at all, so
   SVG -> PNG is a real rendering decision: how big, and in what proportion. A
   squashed chart is a WRONG chart and neither a human nor a model can tell it
   was squashed, so every case here asserts the rasterized size against the
   size the document actually declares -- including the ones a rasterizer sizes
   badly on its own (`width=\"100%\"`), the ones it cannot report at all (no attributes),
   and the ceiling that scales a huge canvas down.

   The pixel probes exist for the same reason: identical dimensions still allow
   a mirrored, offset or stretched drawing, so the content is checked where it
   must land."
  (:require [com.blockether.imaging :as imaging]
            [com.blockether.vis.internal.image-convert :as image-convert]
            [lazytest.core :refer [defdescribe describe expect it]])
  (:import (java.awt.image BufferedImage)
           (java.io ByteArrayInputStream ByteArrayOutputStream)
           (java.util.zip GZIPOutputStream)
           (javax.imageio ImageIO)))

;; Helpers

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

(defn- decode
  "An `Img` handle for encoded bytes. Handles are freed by the library's Cleaner,
   so a test may simply drop one."
  [^bytes data]
  (imaging/decode data))

(defn- decoded-size
  "[width height] read back out of the encoded PNG -- the reported size is a
   claim, this is the fact."
  [^bytes data]
  (let [img (decode data)]
    [(imaging/width img) (imaging/height img)]))

(defn- rendered [markup] (decode (:bytes (raster markup))))

(defn- px
  "Pixel at (x, y) as `\"rrggbb\"` -- the handle hands back packed `0xRRGGBBAA`."
  [img x y]
  (format "%06x" (bit-and (bit-shift-right (long (imaging/get-pixel img x y)) 8) 0xffffff)))

(defn- raster-bytes
  "PNG/BMP/... bytes of a `w` x `h` image filled with `rgb`, in `fmt`."
  ^bytes [fmt w h rgb]
  (with-open [img (imaging/blank w h (bit-or (bit-shift-left (long rgb) 8) 0xff))]
    (imaging/encode img (keyword fmt))))

(def ^:private white "ffffff")
(def ^:private red "ff0000")
(def ^:private blue "0000ff")

;; Declared size -> raster size

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
                   (let [markup
                         (doc "width=\"400\" height=\"100\" viewBox=\"0 0 400 100\""
                              "<rect width=\"400\" height=\"100\" fill=\"#ff0000\"/>")

                         plain
                         (raster markup)

                         zipped
                         (image-convert/rasterize-svg (gzip (svg-bytes markup)) nil)]

                     (expect (= [400 100] [(:width zipped) (:height zipped)]))
                     (expect (= (seq (:bytes plain)) (seq (:bytes zipped))))))))

;; No declared size -> framed by its own shapes

(defn- about
  "Within `tol` px -- the ink is measured on a probe raster, so the frame carries
   a couple of pixels of margin around the shapes and is never off by more."
  ([expected actual] (about expected actual 6))
  ([expected actual tol] (<= (abs (- (long expected) (long actual))) (long tol))))

(defn- ratio [[w h]] (/ (double w) (double h)))

(defdescribe
  rasterize-ink-size-test
  (describe
    "a document that declares no size at all"
    (it "is framed by the shapes it paints, not by a fixed default canvas"
        (let [[w h] (size
                      (doc ""
                           "<rect x=\"50\" y=\"20\" width=\"900\" height=\"300\" fill=\"blue\"/>"))]
          (expect (about 904 w))
          (expect (about 304 h))))
    (it "keeps the aspect ratio of those shapes"
        (expect
          (< 2.9
             (ratio (size
                      (doc "" "<rect x=\"0\" y=\"0\" width=\"600\" height=\"200\" fill=\"red\"/>")))
             3.1)))
    (it "grows a tiny figure to something legible instead of shipping a thumbnail"
        (let [[w h] (size (doc "" "<circle cx=\"10\" cy=\"10\" r=\"8\" fill=\"red\"/>"))]
          (expect (= 512 w))
          (expect (= 512 h))))
    (it "finds shapes drawn at negative coordinates"
        (let [[w h]
              (size (doc
                      ""
                      "<rect x=\"-800\" y=\"-400\" width=\"800\" height=\"400\" fill=\"green\"/>"))]
          (expect (about 804 w))
          (expect (about 404 h))))
    (it "finds shapes drawn far off the origin"
        (let [[w h]
              (size
                (doc ""
                     "<rect x=\"-3000\" y=\"9000\" width=\"600\" height=\"300\" fill=\"black\"/>"))]
          (expect (about 604 w))
          (expect (about 304 h))))
    (it "still obeys the ceiling for a figure larger than it"
        (let [[w h]
              (size (doc ""
                         "<rect x=\"0\" y=\"0\" width=\"20000\" height=\"5000\" fill=\"black\"/>"))]
          (expect (= image-convert/svg-max-raster-dimension w))
          (expect (< 3.9 (ratio [w h]) 4.1))))
    (it "actually PAINTS those shapes -- the frame is not an empty crop"
        (let [img (rendered
                    (doc
                      ""
                      "<rect x=\"120\" y=\"60\" width=\"400\" height=\"200\" fill=\"#ff0000\"/>"))]
          (expect (= red (px img 200 100)))
          (expect (= white (px img 1 1)))))
    (it "falls back to the SVG default canvas when there is nothing to measure"
        (expect (= [300 150] (size (doc "")))))))

;; The ceiling: scaled, never squashed

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

;; Geometry: the drawing lands where the document put it

(defdescribe
  rasterize-content-test
  (describe
    "rendered content"
    (it "maps left/right without mirroring or stretching"
        (let [img (rendered (doc
                              "width=\"400\" height=\"100\" viewBox=\"0 0 400 100\""
                              "<rect width=\"200\" height=\"100\" fill=\"#ff0000\"/>"
                              "<rect x=\"200\" width=\"200\" height=\"100\" fill=\"#0000ff\"/>"))]
          (expect (= red (px img 50 50)))
          (expect (= blue (px img 350 50)))))
    (it "maps top/bottom the same way"
        (let [img (rendered (doc
                              "width=\"100\" height=\"400\" viewBox=\"0 0 100 400\""
                              "<rect width=\"100\" height=\"200\" fill=\"#ff0000\"/>"
                              "<rect y=\"200\" width=\"100\" height=\"200\" fill=\"#0000ff\"/>"))]
          (expect (= red (px img 50 50)))
          (expect (= blue (px img 50 350)))))
    (it "letterboxes a square viewBox inside a wide viewport instead of stretching it"
        (let [img (rendered (doc "width=\"400\" height=\"100\" viewBox=\"0 0 100 100\""
                                 "<rect width=\"100\" height=\"100\" fill=\"#008000\"/>"))]
          (expect (= [400 100] [(imaging/width img) (imaging/height img)]))
          (expect (= "008000" (px img 200 50)) "centred content")
          (expect (= white (px img 10 50)) "left bar")
          (expect (= white (px img 390 50)) "right bar")))
    (it "keeps the geometry after the ceiling scales the canvas down"
        (let [img (rendered
                    (doc "width=\"8000\" height=\"2000\""
                         "<rect width=\"4000\" height=\"2000\" fill=\"#ff0000\"/>"
                         "<rect x=\"4000\" width=\"4000\" height=\"2000\" fill=\"#0000ff\"/>"))]
          (expect (= [4096 1024] [(imaging/width img) (imaging/height img)]))
          (expect (= red (px img 100 500)))
          (expect (= blue (px img 4000 500)))))
    (it "resolves a percentage document against the viewBox, drawing at full size"
        (let [img (rendered (doc
                              "width=\"100%\" height=\"100%\" viewBox=\"0 0 640 480\""
                              "<rect width=\"320\" height=\"480\" fill=\"#ff0000\"/>"
                              "<rect x=\"320\" width=\"320\" height=\"480\" fill=\"#0000ff\"/>"))]
          (expect (= [640 480] [(imaging/width img) (imaging/height img)]))
          (expect (= red (px img 100 240)))
          (expect (= blue (px img 540 240)))))
    (it "flattens transparency onto WHITE, not onto black-on-black"
        (let [img (rendered (doc "width=\"20\" height=\"20\""
                                 "<circle cx=\"10\" cy=\"10\" r=\"2\" fill=\"#000000\"/>"))]
          (expect (= white (px img 0 0)))
          (expect (= white (px img 19 19)))
          (expect (= "000000" (px img 10 10)))))
    (it "renders a text document without dying on the font stack"
        (let [r (raster (doc "width=\"200\" height=\"40\""
                             "<text x=\"4\" y=\"28\" font-size=\"20\">vis</text>"))]
          (expect (some? r))
          (expect (= [200 40] [(:width r) (:height r)]))))))

;; Untrusted input: nothing renders is better than something escapes

(defdescribe
  rasterize-safety-test
  (describe
    "hostile or broken markup"
    (it "yields no pixels -- and a reason -- instead of throwing"
        (expect (nil? (:bytes (raster "not an svg at all"))))
        (expect (some? (:reason (raster "not an svg at all"))))
        (expect (nil? (:bytes (raster "<svg"))))
        (expect (nil? (:bytes (raster (doc "width=\"10\" height=\"10\"" "<rect")))))
        (expect (nil? (image-convert/rasterize-svg (byte-array 0) nil)))
        (expect (nil? (image-convert/rasterize-svg nil nil)))
        (expect (nil? (:bytes (image-convert/rasterize-svg (byte-array [0x1f 0x8b 0x08 0x00]) nil)))
                "truncated gzip"))
    (it "refuses a document with an external XML entity outright (XXE)"
        (let [markup (str "<?xml version=\"1.0\"?>"
                          "<!DOCTYPE svg [<!ENTITY xxe SYSTEM \"file:///etc/passwd\">]>"
                          (doc "width=\"200\" height=\"40\""
                               "<text x=\"2\" y=\"30\" font-size=\"12\">&xxe;</text>"))]
          ;; The renderer never resolves the entity -- it REJECTS the whole
          ;; document, so nothing is rendered and the attachment is skipped.
          ;; Stronger than drawing an empty canvas, and it costs a real figure
          ;; nothing: a plain `<!DOCTYPE svg PUBLIC ...>` (what older exporters
          ;; emit) still renders.
          (expect (nil? (:bytes (raster markup))))
          (expect (some? (:bytes (raster (str
                                           "<?xml version=\"1.0\"?>"
                                           "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\""
                                           " \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">"
                                           (doc "width=\"20\" height=\"20\""
                                                "<rect width=\"20\" height=\"20\"/>")))))
                  "a plain DOCTYPE is still a good document")))
    (it "does not fetch an external <image href> (SSRF / local file read)"
        (let [img (rendered
                    (str
                      "<svg xmlns=\"http://www.w3.org/2000/svg\""
                      " xmlns:xlink=\"http://www.w3.org/1999/xlink\""
                      " width=\"40\" height=\"20\">"
                      "<image xlink:href=\"file:///etc/passwd\" width=\"40\" height=\"20\"/>"
                      "<image xlink:href=\"http://127.0.0.1:1/x.png\" width=\"40\" height=\"20\"/>"
                      "</svg>"))]
          (expect (= [40 20] [(imaging/width img) (imaging/height img)]))
          (expect (= white (px img 20 10)))))
    (it "does not expand a billion-laughs entity bomb"
        (let [entities
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

          (expect (or (nil? (:bytes r)) (= [40 20] [(:width r) (:height r)])))
          (expect (< elapsed-ms 5000) "finished promptly instead of expanding 10^9 nodes")))))

;; to-provider-safe

(defdescribe
  to-provider-safe-test
  (describe
    "a container the wire already accepts"
    (it "comes back byte-IDENTICAL, never re-encoded"
        (doseq [mt ["image/png" "image/jpeg" "image/gif" "image/webp"]]
          (let [data (raster-bytes "png" 4 4 0xff0000)
                r (image-convert/to-provider-safe data mt)]

            (expect (identical? data (:bytes r)) mt)
            (expect (= mt (:media-type r)) mt)
            (expect (= (alength data) (:size r)) mt))))
    (it "normalizes a padded / upper-case media type"
        (let [data (raster-bytes "png" 4 4 0xff0000)]
          (expect (identical? data (:bytes (image-convert/to-provider-safe data "  IMAGE/PNG "))))))
    (it "is still REFUSED when the container is legal and the pixels are not"
        ;; A perfect signature + IHDR and an unreadable stream after it:
        ;; wire-legal to every sniff, and a permanent `Could not process
        ;; image` 400 that replays on every later turn of the session.
        ;; Passing the container is not the same as being an image.
        (let [good
              (raster-bytes "png" 4 4 0xff0000)

              corrupt
              (byte-array (concat (take 33 good) (repeat 24 0)))

              r
              (image-convert/to-provider-safe corrupt "image/png")]

          (expect (nil? (:bytes r)))
          (expect (re-find #"could not be decoded" (str (:reason r)))))))
  (describe "a raster container the wire refuses"
            (it "becomes PNG at exactly the same dimensions"
                (let [bmp
                      (raster-bytes "bmp" 37 11 0x00ff00)

                      r
                      (image-convert/to-provider-safe bmp "image/bmp")]

                  (expect (= "image/png" (:media-type r)))
                  (expect (= [37 11] [(:width r) (:height r)]))
                  (expect (= [37 11] (decoded-size (:bytes r))))
                  (expect (= "00ff00" (px (decode (:bytes r)) 18 5)))))
            (it "says WHY, in the decoder's words, when nothing can decode it"
                (let [r (image-convert/to-provider-safe (.getBytes "not an image" "UTF-8")
                                                        "image/heic")]
                  ;; no pixels -- but the caller can tell the user what happened
                  ;; instead of dropping a perfectly valid attachment in silence.
                  (expect (nil? (:bytes r)))
                  (expect (re-find #"could not be decoded" (str (:reason r)))))))
  (describe "an SVG payload"
            (it "is routed through the rasterizer, ratio intact"
                (doseq [mt image-convert/svg-media-types]
                  (let [r (image-convert/to-provider-safe
                            (svg-bytes (doc
                                         "width=\"100%\" height=\"100%\" viewBox=\"0 0 400 100\""
                                         "<rect width=\"400\" height=\"100\" fill=\"#ff0000\"/>"))
                            mt)]
                    (expect (= "image/png" (:media-type r)) mt)
                    (expect (= [400 100] [(:width r) (:height r)]) mt))))
            (it "recognises its media types, and only those"
                (expect (image-convert/svg-media-type? "image/svg+xml"))
                (expect (image-convert/svg-media-type? " Image/SVG+XML "))
                (expect (not (image-convert/svg-media-type? "image/png")))
                (expect (not (image-convert/svg-media-type? nil)))))
  (describe "with conversion disabled (no imaging cdylib)"
            (it "returns nil so the caller skips the attachment"
                (binding [image-convert/*enabled?* false]
                  (expect (nil? (image-convert/to-provider-safe (raster-bytes "png" 4 4 0xff0000)
                                                                "image/png")))
                  (expect (nil? (raster (doc "width=\"10\" height=\"10\""))))))))

;; fit-dimensions

(defdescribe
  fit-dimensions-test
  "The PIXEL ceiling. A provider refuses an oversized picture whatever it
   WEIGHS: a 4K screenshot is a couple of hundred KB and still a hard 400 once
   one request carries many images -- and attachments replay, so every long
   session becomes exactly that request."
  (it "hands back a picture within the ceiling ITSELF, byte for byte"
      (let [data
            (raster-bytes "png" 40 20 0xff0000)

            r
            (image-convert/fit-dimensions data 1568)]

        (expect (identical? data (:bytes r)))
        (expect (= [40 20] [(:width r) (:height r)]))))
  (it "downscales an oversized picture, ratio intact"
      (let [data
            (raster-bytes "png" 4000 1000 0x00ff00)

            r
            (image-convert/fit-dimensions data 500)]

        (expect (nil? (:reason r)))
        (expect (= [500 125] [(:width r) (:height r)]))
        (expect (= [500 125] (decoded-size (:bytes r))))
        (expect (= "00ff00" (px (decode (:bytes r)) 250 60)))))
  (it "defaults to a ceiling under every provider's many-image limit"
      (expect (<= image-convert/max-wire-dimension 2000))
      (let [r (image-convert/fit-dimensions (raster-bytes "png" 2400 2400 0xff0000))]
        (expect (= [image-convert/max-wire-dimension image-convert/max-wire-dimension]
                   [(:width r) (:height r)]))))
  (it "leaves the payload alone when conversion is unavailable"
      (binding [image-convert/*enabled?* false]
        (let [data (raster-bytes "png" 40 20 0xff0000)]
          (expect (identical? data (:bytes (image-convert/fit-dimensions data 10)))))))
  (it "steps DOWN again when the scaler hands the payload straight back"
      (let [data
            (raster-bytes "png" 4000 1000 0x00ff00)

            real
            imaging/optimize

            targets
            (atom [])]

        ;; A container the scaler refuses to re-encode at the first rung: the
        ;; bytes come back UNCHANGED, still 4000px wide. Trusting that output is
        ;; exactly how an oversized picture reaches the wire believing it fits.
        (with-redefs [imaging/optimize (fn [d opts]
                                         (swap! targets conj (:max-width opts))
                                         (if (= 500 (:max-width opts)) d (real d opts)))]
          (let [r (image-convert/fit-dimensions data 500)]
            (expect (= [500 250] @targets))
            (expect (nil? (:reason r)))
            (expect (<= (long (:width r)) 500))
            (expect (<= (long (:height r)) 500))
            (expect (= [(:width r) (:height r)] (decoded-size (:bytes r))))))))
  (it "never lets a picture past the ceiling, whatever its shape"
      (doseq [[w h] [[4000 1000] [1000 4000] [2400 2400] [1569 10] [10 1569]]]
        (let [r (image-convert/fit-dimensions (raster-bytes "png" w h 0xff0000) 1568)]
          (expect (nil? (:reason r)))
          (expect (= [(:width r) (:height r)] (decoded-size (:bytes r))))
          (expect (<= (long (:width r)) 1568))
          (expect (<= (long (:height r)) 1568)))))
  (it "hands back a payload no decoder can measure instead of dropping it"
      (let [junk (byte-array (map byte (range 1 40)))]
        (expect (identical? junk (:bytes (image-convert/fit-dimensions junk 100)))))))

;; Cross-validation: a decoder that is NOT the one that wrote the bytes

(defn- io-image
  "The payload as read by `javax.imageio` -- the JDK's own decoder, a completely
   separate implementation from the Rust `image` crate that wrote these bytes.
   `nil` when the JDK ships no reader for the container (WebP), `:refused` when
   it has one and the bytes defeat it.

   This oracle is deliberately TEST-only: `java.desktop` is banned in production
   (the native image ships without it), but asking the library that produced a
   payload whether the payload is good is not a check at all -- and a payload
   exactly one decoder accepts is what killed a session with a permanent
   `Could not process image` 400."
  [^bytes data]
  (try (or (ImageIO/read (ByteArrayInputStream. data)) :no-reader) (catch Throwable _ :refused)))

(defn- io-size
  "[width height] according to the INDEPENDENT decoder, or its verdict keyword."
  [^bytes data]
  (let [img (io-image data)]
    (if (instance? BufferedImage img)
      [(.getWidth ^BufferedImage img) (.getHeight ^BufferedImage img)]
      img)))

(defn- io-px
  "Pixel at (x, y) as `\"rrggbb\"`, read out of the JDK's raster."
  [^bytes data x y]
  (format "%06x" (bit-and (.getRGB ^BufferedImage (io-image data) (int x) (int y)) 0xffffff)))

(defn- io-ink-bounds
  "[x0 y0 x1 y1] of every non-white pixel, scanned out of the JDK's own raster --
   an independent re-derivation of the ink bounds [[image-convert]] measures
   with the renderer."
  [^bytes data]
  (let [^BufferedImage img
        (io-image data)

        w
        (.getWidth img)

        h
        (.getHeight img)]

    (loop [x
           0

           y
           0

           x0
           Long/MAX_VALUE

           y0
           Long/MAX_VALUE

           x1
           -1

           y1
           -1]

      (cond (>= y h) [x0 y0 x1 y1]
            (>= x w) (recur 0 (inc y) x0 y0 x1 y1)
            :else (if (= 0xffffff (bit-and (.getRGB img x y) 0xffffff))
                    (recur (inc x) y x0 y0 x1 y1)
                    (recur (inc x)
                           y
                           (min x0 (long x))
                           (min y0 (long y))
                           (max x1 (long x))
                           (max y1 (long y))))))))

(defdescribe
  independent-decoder-test
  (describe
    "every PNG this namespace emits"
    (it "is readable by a decoder that is not the one that wrote it"
        (doseq [[label markup expected] size-cases]
          (expect (= expected (io-size (:bytes (raster markup)))) label)))
    (it "carries the drawing where our own reader says it is"
        (let [png (:bytes (raster (doc "width=\"40\" height=\"20\""
                                       "<rect width=\"40\" height=\"20\" fill=\"#ff0000\"/>")))]
          (expect (= red (io-px png 20 10)))
          (expect (= (px (decode png) 20 10) (io-px png 20 10)))))
    (it "frames a size-less document where an independent raster scan finds the ink"
        ;; The frame comes from OUR alpha scan of a probe render; these bounds
        ;; come from the JDK's raster of the finished PNG. They must agree, or
        ;; the framing is measuring something other than the picture.
        (let [r
              (raster
                (doc "" "<rect x=\"120\" y=\"60\" width=\"400\" height=\"200\" fill=\"#ff0000\"/>"))

              [x0 y0 x1 y1]
              (io-ink-bounds (:bytes r))]

          (expect (about 0 x0))
          (expect (about 0 y0))
          (expect (about (dec (long (:width r))) x1))
          (expect (about (dec (long (:height r))) y1)))))
  (describe "a re-containered raster"
            (it "decodes independently, at the dimensions vis reported"
                (let [r (image-convert/to-provider-safe (raster-bytes "bmp" 37 11 0x00ff00)
                                                        "image/bmp")]
                  (expect (= [37 11] [(:width r) (:height r)]))
                  (expect (= [37 11] (io-size (:bytes r))))
                  (expect (= "00ff00" (io-px (:bytes r) 18 5)))))
            (it "is decodable independently for every container passed through untouched"
                ;; PNG and GIF are handed back byte-identical, so the INPUT bytes are
                ;; what the provider sees; WebP has no JDK reader, which is a gap in the
                ;; oracle, not in the payload.
                (doseq [[fmt mt] [["png" "image/png"] ["gif" "image/gif"]]]
                  (let [data (raster-bytes fmt 9 5 0x0000ff)
                        r (image-convert/to-provider-safe data mt)]

                    (expect (identical? data (:bytes r)) mt)
                    (expect (= [9 5] (io-size (:bytes r))) mt)))
                (expect (= :no-reader (io-size (raster-bytes "webp" 8 8 0x0000ff))))))
  (describe "a payload the gate refuses"
            (it "is refused by BOTH decoders -- the refusal is about the bytes, not the vendor"
                (let [good
                      (raster-bytes "png" 4 4 0xff0000)

                      corrupt
                      (byte-array (concat (take 33 good) (repeat 24 0)))

                      r
                      (image-convert/to-provider-safe corrupt "image/png")]

                  (expect (nil? (:bytes r)))
                  (expect (re-find #"could not be decoded" (str (:reason r))))
                  (expect (= :refused (io-size corrupt)))
                  ;; ... and the sound payload it was cut from passes both.
                  (expect (= [4 4] (io-size good)))))))

;; What vis delegates, and the two repairs it still owes

(defn- probe-size
  "[w h] the RENDERER alone reports for a document, or nil when it refuses it."
  [markup]
  (try (let [{:keys [width height]} (imaging/probe (svg-bytes markup))]
         [width height])
       (catch Throwable _ nil)))

(defdescribe
  renderer-delegation-test
  (describe "a document the renderer can size"
            (it "is sized by the LIBRARY, not by markup parsing of our own"
                ;; px/percent/cm/viewBox/letterboxing: resvg resolves all of it, so
                ;; vis asks and reports the answer verbatim. If this drifts, the sizing
                ;; has quietly moved back into vis.
                (doseq [[label markup expected] size-cases]
                  (expect (= expected (probe-size markup)) label)
                  (expect (= expected (size markup)) label))))
  (describe
    "the documents only a BROWSER sizes"
    ;; A bare resvg REFUSES a zero or negative size outright ("SVG has an
    ;; invalid size") and measures a size-less document from the ORIGIN, so a
    ;; figure at x=50 gains a 50px margin and content at negative coordinates
    ;; collapses to a default canvas. Both repairs were hand-written HERE once;
    ;; they are the imaging library's DEFAULT now, which is why the bare probe
    ;; and vis's own answer agree to the pixel below. If they ever disagree, a
    ;; repair has crept back into vis -- or the library stopped repairing.
    (it "falls back to the viewBox for a zero or negative size"
        (doseq [[markup expected] [[(doc "width=\"0\" height=\"0\" viewBox=\"0 0 30 60\"") [30 60]]
                                   [(doc "width=\"-10\" height=\"-5\" viewBox=\"0 0 20 10\"")
                                    [20 10]]]]
          (expect (= expected (probe-size markup)))
          (expect (= expected (size markup)))))
    (it "frames a size-less document by the ink it actually paints"
        (let [off-origin
              (doc "" "<rect x=\"50\" y=\"20\" width=\"900\" height=\"300\" fill=\"blue\"/>")

              negative
              (doc "" "<rect x=\"-800\" y=\"-400\" width=\"800\" height=\"400\" fill=\"green\"/>")]

          (expect (= (probe-size off-origin) (size off-origin)))
          (expect (= (probe-size negative) (size negative)))
          (let [[w h] (size off-origin)]
            (expect (about 904 w))
            (expect (about 304 h)))
          (let [[w h] (size negative)]
            (expect (about 804 w))
            (expect (about 404 h)))))))
