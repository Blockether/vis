(ns com.blockether.vis.internal.image-optimize-test
  "Automatic image shrinking on the store-bound / prompt-bound attachment path:
   the downscale + re-encode decision (`optimize`), its refusals (too small,
   wrong format, undecodable), the envelope rewrite (`optimize-attachment`),
   and the concurrent batch (`optimize-attachments`) that keeps order and
   leaves non-images alone."
  (:require [com.blockether.vis.internal.image-optimize :as io]
            [lazytest.core :refer [defdescribe describe expect it]])
  (:import (java.awt Color Font)
           (java.awt.image BufferedImage)
           (java.io ByteArrayInputStream ByteArrayOutputStream)
           (java.util Base64)
           (javax.imageio ImageIO)))

(defn- render
  "A screenshot-ish PNG: flat fills plus text, so it compresses like real UI
   rather than like noise. `alpha?` punches a transparent corner (what a macOS
   window capture looks like)."
  ^bytes [^long w ^long h alpha?]
  (let
    [img
     (BufferedImage. (int w)
                     (int h)
                     (if alpha? BufferedImage/TYPE_INT_ARGB BufferedImage/TYPE_INT_RGB))

     g
     (.createGraphics img)]

    (.setColor g Color/WHITE)
    (.fillRect g 0 0 (int w) (int h))
    (.setFont g (Font. "SansSerif" Font/PLAIN 18))
    (dotimes [i 60]
      (.setColor
        g
        (Color. (int (rem (* i 37) 255)) (int (rem (* i 91) 255)) (int (rem (* i 13) 255))))
      (.fillRect g (* i 17) (* i 11) 200 40)
      (.setColor g Color/BLACK)
      (.drawString g (str "Line " i " of terminal-looking text 0123456789") 40 (+ 30 (* i 25))))
    (when alpha?
      (dotimes [x 12]
        (dotimes [y 12]
          (.setRGB img x y 0))))
    (.dispose g)
    (let [baos (ByteArrayOutputStream.)]
      (ImageIO/write img "png" baos)
      (.toByteArray baos))))

(defn- b64 [^bytes data] (.encodeToString (Base64/getEncoder) data))

(defn- unb64 ^bytes [^String s] (.decode (Base64/getDecoder) s))

(defn- dims
  [^bytes data]
  (let [img (ImageIO/read (ByteArrayInputStream. data))]
    [(.getWidth img) (.getHeight img)]))

(defn- att
  [^bytes data media-type filename]
  {:kind "image" :media-type media-type :filename filename :base64 (b64 data) :size (alength data)})

(def ^:private opaque-big (delay (render 3000 2000 false)))

(def ^:private alpha-big (delay (render 3000 2000 true)))

(def ^:private tiny (delay (render 60 40 false)))

(defdescribe
  optimize-test
  (describe
    "a large opaque screenshot"
    (it "is downscaled to the provider's long-edge bound and re-encoded as JPEG"
        (let
          [{:keys [media-type size original-size width height] :as r} (io/optimize @opaque-big
                                                                                   "image/png")]
          (expect (some? r))
          (expect (= "image/jpeg" media-type))
          (expect (= io/default-max-dimension width))
          (expect (= 1045 height) "aspect ratio preserved")
          (expect (< size original-size))
          (expect (= [width height] (dims (:bytes r))) "the bytes really carry those dims")))
    (it "honors an explicit :max-dimension"
        (let [{:keys [width height]} (io/optimize @opaque-big "image/png" {:max-dimension 640})]
          (expect (= 640 width))
          (expect (= 427 height)))))
  (describe "an image with real transparency"
            (it "stays PNG rather than losing its alpha channel to JPEG"
                (let [{:keys [media-type size original-size]} (io/optimize @alpha-big "image/png")]
                  (expect (= "image/png" media-type))
                  (expect (< size original-size)))))
  (describe
    "refusals — the original bytes are always a correct answer"
    (it "leaves a payload under the floor alone" (expect (nil? (io/optimize @tiny "image/png"))))
    (it "never touches a GIF (it may be animated)"
        (expect (nil? (io/optimize @opaque-big "image/gif"))))
    (it "ignores a non-image media type"
        (expect (nil? (io/optimize @opaque-big "application/pdf"))))
    (it "returns nil instead of throwing on undecodable bytes"
        (expect (nil? (io/optimize (.getBytes (apply str (repeat 100000 "not a png")) "UTF-8")
                                   "image/png"))))
    (it "returns nil when the re-encode does not beat :min-gain"
        (expect (nil? (io/optimize @opaque-big "image/png" {:min-gain 0.01}))))
    (it "is a no-op while disabled"
        (binding [io/*enabled?* false]
          (expect (nil? (io/optimize @opaque-big "image/png")))))))

(defdescribe
  optimize-attachment-test
  (it "rewrites :base64 :size :media-type :filename consistently"
      (let [out (io/optimize-attachment (att @opaque-big "image/png" "shot.png"))]
        (expect (= "image/jpeg" (:media-type out)))
        (expect (= "shot.jpg" (:filename out)) "the container changed, so the name follows")
        (expect (= (:size out) (alength (unb64 (:base64 out)))) ":size matches the payload")
        (expect (< (long (:size out)) (alength ^bytes @opaque-big)))))
  (it "adds no keys to the envelope"
      (let [in (att @opaque-big "image/png" "shot.png")]
        (expect (= (set (keys in)) (set (keys (io/optimize-attachment in)))))))
  (it "returns the SAME map when there is nothing to gain"
      (let [in (att @tiny "image/png" "tiny.png")]
        (expect (identical? in (io/optimize-attachment in)))))
  (it "survives a corrupt :base64"
      (let [in {:kind "image" :media-type "image/png" :base64 "!!!not base64!!!" :size 9}]
        (expect (identical? in (io/optimize-attachment in))))))

(defdescribe optimize-attachments-test
             (it "shrinks a batch concurrently, preserving order and non-images"
                 (let
                   [in
                    [(att @opaque-big "image/png" "a.png")
                     {:kind "file"
                      :media-type "text/csv"
                      :filename "d.csv"
                      :base64 (b64 (.getBytes "a,b" "UTF-8"))
                      :size 3} (att @alpha-big "image/png" "b.png") (att @tiny "image/png" "c.png")]

                    out
                    (io/optimize-attachments in)]

                   (expect (= 4 (count out)))
                   (expect (= ["image/jpeg" "text/csv" "image/png" "image/png"]
                              (mapv :media-type out)))
                   (expect (identical? (nth in 1) (nth out 1)) "a non-image is never decoded")
                   (expect (identical? (nth in 3) (nth out 3)) "a small image is never re-encoded")
                   (expect (< (long (:size (nth out 0))) (long (:size (nth in 0)))))
                   (expect (< (long (:size (nth out 2))) (long (:size (nth in 2)))))))
             (it "ships originals when the batch cannot finish in the time budget"
                 (binding [io/*timeout-ms* 0]
                   (let [in [(att @opaque-big "image/png" "a.png")]]
                     (expect (= in (io/optimize-attachments in))))))
             (it "handles nil and empty input"
                 (expect (= [] (io/optimize-attachments nil)))
                 (expect (= [] (io/optimize-attachments [])))))
