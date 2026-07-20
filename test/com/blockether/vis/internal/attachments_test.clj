(ns com.blockether.vis.internal.attachments-test
  "User-message image attachment collection: path extraction from
   drop-shaped text, magic-byte MIME sniffing, and the size/count caps."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.attachments :as attachments]
            [lazytest.core :refer [defdescribe describe expect it]])
  (:import [java.io File]
           [java.nio.file Files]
           [java.util Base64]))

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
