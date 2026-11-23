(ns com.blockether.vis.internal.attachment.codec
  "How a stored attachment payload is ENCODED in its BLOB, never what it says.

   A row holds its payload `raw` or as one `zstd` frame. The codec is a property of
   the storage only: every read gives the original bytes back, the media type,
   name and `size_bytes` stay those of the original, and nothing outside the store
   sees the codec. Text, HTML, JSON, diffs, SVG, WAV and video often shrink to a
   fraction; a format that is compressed already stays `raw`, because a frame is
   kept only when it saves at least [[min-saving-percent]] percent.

   Zstd is the pure-Java aircompressor through Nippy, which the store already uses
   for each BLOB column, so the native image needs nothing new."
  (:require [taoensso.nippy :as nippy]
            [taoensso.nippy.compression :as compression])
  (:import (java.util Arrays)))

(set! *warn-on-reflection* true)

(def ^:const raw "The codec of a payload stored as it is." "raw")

(def ^:const zstd "The codec of a payload stored as one zstd frame." "zstd")

(def ^:const min-bytes
  "The smallest payload that the compactor reads. A smaller one cannot save enough
   to pay for its pass."
  1024)

(def ^:const min-saving-percent "The smallest saving that a zstd frame must give." 10)

(defn unpack
  "The original bytes of a payload stored as `stored` with `codec`. A nil codec is
   `raw`: a row from before the codec column. An unknown codec throws, because its
   bytes are not the payload."
  ^bytes [^bytes stored codec]
  (when stored
    (condp = (or codec raw)
      raw stored
      zstd (compression/decompress nippy/zstd-compressor stored)
      (throw (ex-info (str "Unknown attachment storage codec: " codec)
                      {:type :attachment/unknown-codec :codec codec})))))

(defn pack
  "`{:bytes :codec}` for the payload `data`: a zstd frame when it saves at least
   [[min-saving-percent]] percent and gives `data` back exactly, else `data` itself
   as `raw`. Never throws."
  [^bytes data]
  (let [n
        (alength data)

        ^bytes frame
        (when (>= n (long min-bytes))
          (try (compression/compress nippy/zstd-compressor data) (catch Throwable _ nil)))]

    (if (and frame
             (<= (* 100 (alength frame)) (* (- 100 (long min-saving-percent)) n))
             ;; The hint keeps the byte[] overload direct: a reflective call fails
             ;; in the native image.
             (let [^bytes restored (try (unpack frame zstd) (catch Throwable _ nil))]
               (Arrays/equals data restored)))
      {:bytes frame :codec zstd}
      {:bytes data :codec raw})))
