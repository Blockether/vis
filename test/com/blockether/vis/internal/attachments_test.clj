(ns com.blockether.vis.internal.attachments-test
  "User-message image attachment collection: path extraction from
   drop-shaped text, magic-byte MIME sniffing, and the size/count caps."
  (:require [clojure.java.io :as io]
            [com.blockether.imaging :as imaging]
            [clojure.string :as str]
            [com.blockether.vis.internal.attachments :as attachments]
            [com.blockether.vis.internal.image-convert :as image-convert]
            [lazytest.core :refer [defdescribe describe expect it]])
  (:import [java.awt.image BufferedImage]
           [java.io ByteArrayInputStream File]
           [java.nio.file Files]
           [java.util Base64]
           [javax.imageio ImageIO]))

;; 1x1 red PNG (67 bytes) - a real, complete still PNG.
(def ^:private tiny-png-b64
  "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mP8z8BQDwAEhQGAhKmMIQAAAABJRU5ErkJggg==")

(def ^:private tiny-png-bytes (.decode (Base64/getDecoder) ^String tiny-png-b64))

(def ^:private jpeg-header
  ;; FF D8 FF E0 + padding - enough for the sniffer (header-only, not a
  ;; decodable image, which is fine: the sniffer never decodes).
  (byte-array (concat [0xff 0xd8 0xff 0xe0] (repeat 32 0))))

(defn- temp-dir
  ^File []
  (.toFile (Files/createTempDirectory "vis-attachments-test"
                                      (make-array java.nio.file.attribute.FileAttribute 0))))

(defn- write-file
  ^File [^File dir ^String name ^bytes data]
  (let [f (io/file dir name)]
    (io/copy data f)
    f))

(defn- noisy-png
  "An INCOMPRESSIBLE PNG: pseudo-random RGB, so no lossless pass can shrink it
   and the optimiser has to earn every byte it saves."
  ^bytes [^long w ^long h]
  (let
    [rnd
     (java.util.Random. 7)

     buf
     (byte-array (* w h 4))]

    (dotimes [i (* w h)]
      (aset-byte buf (* i 4) (byte (- (.nextInt rnd 256) 128)))
      (aset-byte buf (+ 1 (* i 4)) (byte (- (.nextInt rnd 256) 128)))
      (aset-byte buf (+ 2 (* i 4)) (byte (- (.nextInt rnd 256) 128)))
      (aset-byte buf (+ 3 (* i 4)) (byte -1)))
    (let [img (imaging/from-pixels buf w h)]
      (try (imaging/encode img :png) (finally (imaging/close! img))))))

(defdescribe
  detect-image-mime-test
  (it "sniffs png" (expect (= "image/png" (attachments/detect-image-mime tiny-png-bytes))))
  (it "sniffs jpeg" (expect (= "image/jpeg" (attachments/detect-image-mime jpeg-header))))
  (it "rejects jpeg-ls (4th byte 0xF7)"
      (expect (nil? (attachments/detect-image-mime (byte-array [0xff 0xd8 0xff 0xf7 0 0 0 0])))))
  (it "sniffs gif"
      (expect (= "image/gif"
                 (attachments/detect-image-mime (.getBytes "GIF89a-and-some-padding" "US-ASCII")))))
  (it "sniffs webp"
      (expect (= "image/webp"
                 (attachments/detect-image-mime (byte-array (concat (.getBytes "RIFF" "US-ASCII")
                                                                    [0 0 0 0]
                                                                    (.getBytes "WEBP" "US-ASCII")
                                                                    (repeat 8 0)))))))
  (it "rejects text bytes"
      (expect (nil? (attachments/detect-image-mime (.getBytes "hello, this is not an image"
                                                              "UTF-8"))))))

(defdescribe
  collect-user-images-test
  (describe
    "path shapes"
    (it "collects a plain absolute path"
        (let
          [dir
           (temp-dir)

           f
           (write-file dir "shot.png" tiny-png-bytes)

           res
           (attachments/collect-user-images (str "what is wrong on " (.getAbsolutePath f) " ?"))]

          (expect (= 1 (count (:attached res))))
          (expect (= "image/png"
                     (-> res
                         :attached
                         first
                         :media-type)))
          (expect (= tiny-png-b64
                     (-> res
                         :attached
                         first
                         :base64)))))
    (it "collects a backslash-escaped path (macOS terminal drop)"
        (let
          [dir
           (temp-dir)

           f
           (write-file dir "My Shot.png" tiny-png-bytes)

           escaped
           (str/replace (.getAbsolutePath f) " " "\\ ")

           res
           (attachments/collect-user-images escaped)]

          (expect (= 1 (count (:attached res))))
          (expect (= (.getAbsolutePath f)
                     (-> res
                         :attached
                         first
                         :path)))))
    (it "collects a single-quoted path with spaces"
        (let
          [dir
           (temp-dir)

           f
           (write-file dir "Screen Shot.png" tiny-png-bytes)

           res
           (attachments/collect-user-images (str "look at '" (.getAbsolutePath f) "' please"))]

          (expect (= 1 (count (:attached res))))))
    (it "resolves a path trailed by sentence punctuation"
        (let
          [dir
           (temp-dir)

           f
           (write-file dir "shot.png" tiny-png-bytes)

           res
           (attachments/collect-user-images (str "see this: " (.getAbsolutePath f) ". thanks"))]

          (expect (= 1 (count (:attached res))))))
    (it "resolves a path wrapped in parentheses"
        (let
          [dir
           (temp-dir)

           f
           (write-file dir "shot.png" tiny-png-bytes)

           res
           (attachments/collect-user-images (str "look (" (.getAbsolutePath f) ")"))]

          (expect (= 1 (count (:attached res))))))
    (it "resolves a relative path against :workspace-root"
        (let
          [dir
           (temp-dir)

           _
           (write-file dir "logo.png" tiny-png-bytes)

           res
           (attachments/collect-user-images "check logo.png"
                                            {:workspace-root (.getAbsolutePath dir)})]

          (expect (= 1 (count (:attached res))))))
    (it "dedupes the same file mentioned twice"
        (let
          [dir
           (temp-dir)

           f
           (write-file dir "a.png" tiny-png-bytes)

           p
           (.getAbsolutePath f)

           res
           (attachments/collect-user-images (str p " and again " p))]

          (expect (= 1 (count (:attached res)))))))
  (describe "filtering"
            (it "ignores nonexistent paths"
                (let [res (attachments/collect-user-images "/nope/definitely/missing.png")]
                  (expect (= {:attached [] :skipped []} res))))
            (it "ignores files whose bytes are not a supported image"
                (let
                  [dir
                   (temp-dir)

                   f
                   (write-file dir "fake.png" (.getBytes "not an image" "UTF-8"))

                   res
                   (attachments/collect-user-images (.getAbsolutePath f))]

                  (expect (= {:attached [] :skipped []} res))))
            (it "ignores non-image extensions without touching the file"
                (let
                  [dir
                   (temp-dir)

                   f
                   (write-file dir "notes.txt" (.getBytes "text" "UTF-8"))

                   res
                   (attachments/collect-user-images (.getAbsolutePath f))]

                  (expect (= {:attached [] :skipped []} res))))
            (it "handles blank/nil text"
                (expect (= {:attached [] :skipped []} (attachments/collect-user-images nil)))
                (expect (= {:attached [] :skipped []} (attachments/collect-user-images "")))))
  (describe "caps"
            (it "skips oversized images with a reason"
                (let
                  [dir
                   (temp-dir)

                   f
                   (write-file dir "big.png" tiny-png-bytes)

                   res
                   (attachments/collect-user-images (.getAbsolutePath f) {:max-bytes 10})]

                  (expect (empty? (:attached res)))
                  (expect (= 1 (count (:skipped res))))
                  (expect (str/includes? (-> res
                                             :skipped
                                             first
                                             :reason)
                                         "exceeds"))))
            (it "caps the attachment count and reports the overflow"
                (let
                  [dir
                   (temp-dir)

                   f1
                   (write-file dir "one.png" tiny-png-bytes)

                   f2
                   (write-file dir "two.png" tiny-png-bytes)

                   res
                   (attachments/collect-user-images
                     (str (.getAbsolutePath f1) " " (.getAbsolutePath f2))
                     {:max-images 1})]

                  (expect (= 1 (count (:attached res))))
                  (expect (= 1 (count (:skipped res))))
                  (expect (str/includes? (-> res
                                             :skipped
                                             first
                                             :reason)
                                         "limit"))))))

(defdescribe
  text-chip-preview-test
  "Queue rows must never paint a raw `/var/folders/…/clipboard-….png`."
  (describe "text->chip-preview"
            (it "collapses an image path to a named chip and keeps the prose"
                (expect (= "clipboard-1.png LOOK AT THIS"
                           (attachments/text->chip-preview
                             "/var/folders/x/T/clipboard-1.png\n LOOK AT THIS"))))
            (it "chips an image-only message instead of returning the path"
                (expect (= "shot.png" (attachments/text->chip-preview "/tmp/shot.png"))))
            (it "handles a quoted path with spaces"
                (expect (= "check my shot.png ok"
                           (attachments/text->chip-preview "check \"/tmp/my shot.png\" ok"))))
            (it "chips several images in one message"
                (expect (= "a.png b.jpg" (attachments/text->chip-preview "/tmp/a.png /tmp/b.jpg"))))
            (it "is pure — a path whose file is gone still chips"
                (expect (= "vanished.png"
                           (attachments/text->chip-preview
                             "/tmp/definitely-not-here/vanished.png"))))
            (it "leaves prose untouched apart from whitespace collapsing"
                (expect (= "hello world" (attachments/text->chip-preview "hello   world"))))
            (it "returns nil for blank/nil input"
                (expect (nil? (attachments/text->chip-preview nil)))
                (expect (nil? (attachments/text->chip-preview "   "))))))

(defdescribe text-inline-chips-test
             "Transcript bubbles chip image paths WITHOUT collapsing their layout."
             (describe "text->inline-chips"
                       (it "chips the path but keeps the newline the prose sat on"
                           (expect (= "clipboard-1.png\n LOOK AT THIS"
                                      (attachments/text->inline-chips
                                        "/var/folders/x/T/clipboard-1.png\n LOOK AT THIS"))))
                       (it "keeps blank lines and indentation intact"
                           (expect (= "one\n\n  two   spaced"
                                      (attachments/text->inline-chips "one\n\n  two   spaced"))))
                       (it "chips every image path in place"
                           (expect (= "a.png\nb.jpg"
                                      (attachments/text->inline-chips "/tmp/a.png\n/tmp/b.jpg"))))
                       (it "passes blank/nil through unchanged"
                           (expect (= "" (attachments/text->inline-chips nil)))
                           (expect (= "   " (attachments/text->inline-chips "   "))))))

(defn- bmp-bytes
  "A real, decodable 8x8 BMP — a valid image every vision provider still refuses."
  ^bytes []
  (imaging/encode (imaging/blank 8 8 "black") :bmp))

(defn- b64 ^String [^bytes data] (.encodeToString (Base64/getEncoder) data))

(defn- corrupt-png
  "The shape that actually bricked a session: a PERFECT 8-byte signature and a
   valid `IHDR`, then bytes no zlib stream can read. Every header sniff on earth
   calls this `image/png`; no decoder — and no provider — can turn it into
   pixels."
  ^bytes []
  (byte-array (concat (take 33 tiny-png-bytes) (repeat 24 0))))

(def ^:private svg-doc
  "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"120\" height=\"60\"><rect width=\"120\" height=\"60\" fill=\"#333\"/></svg>")

(defdescribe
  provider-safe-media-type-test
  "Intake STORES, it does not decide. The wire's four containers are still the
   only ones a provider takes, but which of them a payload must become is a
   send-time question (see `wire-image-test`) — so an upload is kept in its
   ORIGINAL container and nothing lossy happens on the way in."
  (describe "provider-image-media-type?"
            (it "accepts exactly the four formats every vision wire takes"
                (expect (= #{"image/jpeg" "image/png" "image/gif" "image/webp"}
                           attachments/provider-image-media-types))
                (expect (every? attachments/provider-image-media-type?
                                attachments/provider-image-media-types))
                (expect (attachments/provider-image-media-type? "IMAGE/PNG "))
                (expect (not (attachments/provider-image-media-type? "image/svg+xml")))
                (expect (not (attachments/provider-image-media-type? "image/bmp")))
                (expect (not (attachments/provider-image-media-type? nil)))))
  (describe
    "prepare-inline-attachments"
    (it "keeps an SVG upload as SVG instead of rasterizing it into the DB"
        (let
          [out (attachments/prepare-inline-attachments
                 [{:base64 (str "data:image/svg+xml;base64," (b64 (.getBytes svg-doc "UTF-8")))
                   :filename "logo.svg"
                   :media-type "image/svg+xml"}])]
          (expect (empty? (:skipped out)))
          (expect (= ["image/svg+xml"] (mapv :media-type (:attached out))))))
    (it "stores the SVG even when nothing can render it — the wire gate decides later"
        (binding [image-convert/*enabled?* false]
          (let
            [out (attachments/prepare-inline-attachments [{:base64 (b64 (.getBytes svg-doc "UTF-8"))
                                                           :filename "logo.svg"}])]
            (expect (empty? (:skipped out)))
            (expect (= ["image/svg+xml"] (mapv :media-type (:attached out)))))))
    (it "keeps a BMP in its own container, byte-for-byte"
        (let
          [bmp
           (bmp-bytes)

           out
           (attachments/prepare-inline-attachments [{:base64 (b64 bmp) :filename "shot.bmp"}])]

          (expect (empty? (:skipped out)))
          (expect (= ["image/bmp"] (mapv :media-type (:attached out))))
          (expect (= (b64 bmp) (:base64 (first (:attached out)))))))
    (it "still attaches a PNG"
        (let
          [out (attachments/prepare-inline-attachments [{:base64 tiny-png-b64 :filename "a.png"}])]
          (expect (= ["image/png"] (mapv :media-type (:attached out))))))))

(defdescribe
  wire-image-test
  "The ONE gate every image crosses on its way to a provider, at SEND time.

   A stored row is re-judged on every turn, which is the whole point: an
   attachment replays forever, so a payload blessed on the way IN can never be
   reconsidered — that is how one corrupt-but-wire-legal PNG earned a permanent
   `Could not process image` 400 on every later request of a session."
  (describe "a container the wire already accepts"
            (it "goes out with the caller's OWN base64 once the bytes really decode"
                (let [wired (attachments/wire-image {:base64 tiny-png-b64 :media-type "image/png"})]
                  (expect (= "image/png" (:media-type wired)))
                  (expect (= tiny-png-b64 (:base64 wired)))
                  (expect (= (alength tiny-png-bytes) (:size wired)))))
            (it "strips a data-URL prefix the browser added"
                (expect (= tiny-png-b64
                           (:base64 (attachments/wire-image {:base64 (str "data:image/png;base64,"
                                                                          tiny-png-b64)
                                                             :media-type "image/png"})))))
            (it "believes the BYTES, not the label"
                (expect (= "image/png"
                           (:media-type (attachments/wire-image {:base64 tiny-png-b64
                                                                 :media-type "image/jpeg"}))))))
  (describe "a payload no decoder can read"
            (it "is REFUSED in the decoder's own words instead of bricking the session"
                (let
                  [wired (attachments/wire-image {:base64 (b64 (corrupt-png))
                                                  :media-type "image/png"
                                                  :path "/tmp/dot.png"})]
                  (expect (nil? (:base64 wired)))
                  (expect (= "/tmp/dot.png" (:path wired)))
                  (expect (re-find #"could not be decoded" (str (:reason wired))))))
            (it "never trusts an unverifiable payload with a blank media type"
                (expect (nil? (attachments/wire-image {:base64 tiny-png-b64 :media-type ""})))))
  (describe "a container the wire refuses"
            (it "rasterizes SVG to PNG on the way out, leaving the stored vector alone"
                (let
                  [b
                   (b64 (.getBytes svg-doc "UTF-8"))

                   wired
                   (attachments/wire-image {:base64 b :media-type "image/svg+xml"})]

                  (expect (= "image/png" (:media-type wired)))
                  (expect (not= b (:base64 wired)))))
            (it "re-containers a BMP to PNG"
                (expect (= "image/png"
                           (:media-type (attachments/wire-image {:base64 (b64 (bmp-bytes))
                                                                 :media-type "image/bmp"})))))
            (it "says why when the format is beyond the decoder"
                (expect (re-find #"could not be decoded"
                                 (str (:reason (attachments/wire-image
                                                 {:base64 (b64 (.getBytes "nope" "UTF-8"))
                                                  :media-type "image/heic"})))))))
  (describe "payloads the gate must not touch at all"
            (it "returns nil for a non-image artifact, which is not a failure"
                (expect (nil? (attachments/wire-image {:base64 (b64 (.getBytes "a,b\n1,2" "UTF-8"))
                                                       :media-type "text/csv"})))))
  (describe "the wire size limit"
            (it "refuses an image past it, naming both sizes"
                (expect (re-find #"exceeds the"
                                 (str (:reason (attachments/wire-image {:base64 tiny-png-b64
                                                                        :media-type "image/png"}
                                                                       {:max-bytes 8}))))))
            (it "optimizes an oversize image UNDER the cap instead of dropping it"
                (let
                  [png
                   (noisy-png 400 300)

                   cap
                   (long (* 0.4 (alength png)))

                   wired
                   (attachments/wire-image {:base64 (b64 png) :media-type "image/png"}
                                           {:max-bytes cap})

                   ;; a third decoder again: the rescued bytes must still be a
                   ;; picture, not merely small.
                   decoded
                   (ImageIO/read (ByteArrayInputStream. (.decode (Base64/getDecoder)
                                                                 ^String (:base64 wired))))]

                  (expect (> (alength png) cap))
                  (expect (nil? (:reason wired)))
                  (expect (= "image/png" (:media-type wired)))
                  (expect (<= (long (:size wired)) cap))
                  (expect (some? decoded))
                  (expect (pos? (.getWidth decoded)))
                  (expect (pos? (.getHeight decoded)))))
            (it "never re-encodes a payload that already fits"
                (expect (= tiny-png-b64
                           (:base64 (attachments/wire-image {:base64 tiny-png-b64
                                                             :media-type "image/png"}))))))
  (describe "repeat sends"
            (it "re-uses the cached verdict instead of rasterizing the same SVG every turn"
                (let
                  [att
                   {:base64 (b64 (.getBytes svg-doc "UTF-8")) :media-type "image/svg+xml"}

                   a
                   (attachments/wire-image att)

                   b
                   (attachments/wire-image att)]

                  ;; the very same converted String, not merely an equal one: attachments
                  ;; replay on EVERY later turn, so an uncached gate would re-render the
                  ;; whole session's figures on every single request.
                  (expect (identical? (:base64 a) (:base64 b))))))
  (describe "with conversion disabled (no imaging cdylib)"
            (it "still sends a wire-legal container, and only that"
                ;; Fresh pixels on purpose: the verdict cache is content-keyed, so a
                ;; payload another example already converted would answer from there.
                (let [bmp (imaging/encode (imaging/blank 9 9 "white") :bmp)]
                  (binding [image-convert/*enabled?* false]
                    (expect (= "image/png"
                               (:media-type (attachments/wire-image {:base64 tiny-png-b64
                                                                     :media-type "image/png"}))))
                    (expect (re-find #"image/bmp"
                                     (str (:reason (attachments/wire-image {:base64 (b64 bmp)
                                                                            :media-type
                                                                            "image/bmp"}))))))))))

(defdescribe
  wire-images-test
  "The whole message's images, in the shape the prompt manifest speaks."
  (it "attaches what it can and NAMES what it could not send"
      (let
        [out (attachments/wire-images
               [{:base64 tiny-png-b64 :media-type "image/png" :path "ok.png"}
                {:base64 (b64 (corrupt-png)) :media-type "image/png" :path "bad.png"}])]
        (expect (= ["ok.png"] (mapv :path (:attached out))))
        (expect (= ["bad.png"] (mapv :path (:skipped out))))
        (expect (re-find #"could not be decoded" (str (:reason (first (:skipped out))))))))
  (it "tells a NON-VISION target the files are on disk rather than sending blocks it cannot read"
      (let
        [out (attachments/wire-images [{:base64 tiny-png-b64 :media-type "image/png" :path "a.png"}]
                                      {:vision? false})]
        (expect (empty? (:attached out)))
        (expect (= ["a.png"] (mapv :path (:skipped out))))
        (expect (true? (:readable-blind? (first (:skipped out)))))
        (expect (re-find #"no vision" (str (:reason (first (:skipped out))))))))
  (it "keeps a DISPLAY-ONLY image off the wire even on a vision target"
      ;; The opt-out: bytes an image replay would re-upload on EVERY later
      ;; request stay stored and displayed, and the model is told they are
      ;; openable on disk instead.
      (let
        [out (attachments/wire-images [{:base64 tiny-png-b64
                                        :media-type "image/png"
                                        :path "secret.png"
                                        :is-display-only true}
                                       {:base64 tiny-png-b64 :media-type "image/png" :path "ok.png"}])]
        (expect (= ["ok.png"] (mapv :path (:attached out))))
        (expect (= ["secret.png"] (mapv :path (:skipped out))))
        (expect (true? (:readable-blind? (first (:skipped out)))))
        (expect (re-find #"display-only" (str (:reason (first (:skipped out))))))))
  (it "is empty, not broken, with nothing to send"
      (expect (= {:attached [] :skipped []} (attachments/wire-images nil)))))

(defn- decoded
  "What a wired payload looks like to `javax.imageio` — the JDK's decoder, which
   shares no code with the Rust `image`/`resvg` stack that produced the bytes.

   `:refused` when the JDK has a reader and the bytes defeat it. The gate's whole
   job is to guarantee that a SECOND, unrelated decoder can read whatever goes on
   the wire, because the provider's decoder is a third one again — a payload only
   its own encoder accepts is exactly what earned a session a permanent
   `Could not process image` 400."
  [wired]
  (try (ImageIO/read (ByteArrayInputStream. (.decode (Base64/getDecoder) ^String (:base64 wired))))
       (catch Throwable _ :refused)))

(defn- decoded-size
  [wired]
  (let [img (decoded wired)]
    (if (instance? BufferedImage img)
      [(.getWidth ^BufferedImage img) (.getHeight ^BufferedImage img)]
      img)))

(defdescribe
  wire-image-cross-validation-test
  "Everything the gate lets through, re-read by an INDEPENDENT decoder."
  (it "emits a raster a foreign decoder reads, at the size vis reported"
      (doseq
        [[label att expected]
         [["png passed through untouched" {:base64 tiny-png-b64 :media-type "image/png"} [1 1]]
          ["svg rasterized on the way out"
           {:base64 (b64 (.getBytes svg-doc "UTF-8")) :media-type "image/svg+xml"} [120 60]]
          ["bmp re-containered"
           {:base64 (b64 (imaging/encode (imaging/blank 21 13 "white") :bmp))
            :media-type "image/bmp"} [21 13]]]]
        (let [wired (attachments/wire-image att)]
          (expect (= "image/png" (:media-type wired)) label)
          (expect (= expected (decoded-size wired)) label))))
  (it "keeps the picture, not merely a decodable container"
      ;; A re-container that silently dropped the pixels would still satisfy
      ;; every size assertion above.
      (let
        [wired
         (attachments/wire-image {:base64 (b64 (.getBytes svg-doc "UTF-8"))
                                  :media-type "image/svg+xml"})

         ^BufferedImage img
         (decoded wired)]

        (expect (= "333333" (format "%06x" (bit-and (.getRGB img 60 30) 0xffffff))))))
  (it "refuses precisely what the foreign decoder also refuses"
      (let [bad {:base64 (b64 (corrupt-png)) :media-type "image/png"}]
        (expect (nil? (:base64 (attachments/wire-image bad))))
        (expect (= :refused (decoded-size (assoc bad :base64 (:base64 bad))))))))
