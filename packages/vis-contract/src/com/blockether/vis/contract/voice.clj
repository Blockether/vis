(ns com.blockether.vis.contract.voice
  "Live voice messages and bounded PCM frames. JSON Schema owns the wire vocabulary."
  (:require [clojure.string :as str]
            [com.blockether.vis.contract.document :as document]
            [com.blockether.vis.contract.wire :as wire])
  (:import [java.nio ByteBuffer ByteOrder]
           [java.nio.charset StandardCharsets]))

(set! *warn-on-reflection* true)

(def ^:private source (delay (document/schema-document "voice")))

(def limits
  "Pilot limits, derived from the canonical schema. Durations use milliseconds."
  (wire/->engine (get @source "x-vis-voice-limits")))

(def frame-format
  "Binary layout and PCM format, derived from the canonical schema."
  (wire/->engine (get @source "x-vis-voice-frame")))

(def ^:private fields (mapv #(keyword (str/replace % "_" "-")) (:fields frame-format)))

(defn valid?
  "Check a named voice definition after normalizing wire keys."
  [definition value]
  (document/valid? "voice" definition value))

(defn- invalid-frame [] (ex-info "Invalid live voice frame" {:type :voice/invalid-frame}))

(defn encode-frame
  "Encode one header and its PCM16 bytes. Reject oversized frames before allocation."
  ^bytes [header ^bytes pcm]
  (let [size
        (alength pcm)

        header
        (assoc header :sample-count (quot size 2))]

    (when-not (and (even? size) (valid? "frame_header" header)) (throw (invalid-frame)))
    (let [buffer (doto (ByteBuffer/allocate (+ (long (:header-bytes limits)) size))
                   (.order ByteOrder/LITTLE_ENDIAN))]
      (.put buffer (.getBytes ^String (:magic frame-format) StandardCharsets/US_ASCII))
      (.put buffer (byte (:version frame-format)))
      (.put buffer (byte (get (:kinds frame-format) (keyword (:kind header)))))
      (.putShort buffer (short 0))
      (doseq [field fields]
        (.putInt buffer (unchecked-int (get header field))))
      (.put buffer pcm)
      (.array buffer))))

(defn decode-frame
  "Decode a byte array or ByteBuffer without retaining its backing storage."
  [value]
  (let [^ByteBuffer buffer
        (cond (bytes? value) (ByteBuffer/wrap ^bytes value)
              (instance? ByteBuffer value) (.slice ^ByteBuffer value)
              :else (throw (invalid-frame)))

        size
        (.remaining buffer)

        header-size
        (long (:header-bytes limits))]

    (when-not (<= header-size size (+ header-size (* 2 (long (:frame-samples limits)))))
      (throw (invalid-frame)))
    (.order buffer ByteOrder/LITTLE_ENDIAN)
    (let [magic
          (byte-array 4)

          _
          (.get buffer magic)

          version
          (bit-and 255 (.get buffer))

          kind-code
          (bit-and 255 (.get buffer))

          reserved
          (.getShort buffer)

          kind
          (some (fn [[k v]]
                  (when (= v kind-code) k))
                (:kinds frame-format))

          header
          (reduce (fn [m field]
                    (assoc m field (Integer/toUnsignedLong (.getInt buffer))))
                  {:kind kind}
                  fields)

          count-samples
          (:sample-count header)]

      (when-not (and (= (:magic frame-format) (String. magic StandardCharsets/US_ASCII))
                     (= (:version frame-format) version)
                     (zero? reserved)
                     (valid? "frame_header" header)
                     (= (* 2 (long count-samples)) (.remaining buffer)))
        (throw (invalid-frame)))
      (let [pcm (byte-array (.remaining buffer))]
        (.get buffer pcm)
        (assoc header :pcm pcm)))))

(defn pcm->samples
  "Convert validated PCM16 bytes to normalized Float32 samples."
  ^floats [^bytes pcm]
  (let [buffer
        (doto (ByteBuffer/wrap pcm) (.order ByteOrder/LITTLE_ENDIAN))

        samples
        (float-array (quot (alength pcm) 2))]

    (dotimes [i (alength samples)]
      (aset-float samples i (float (/ (.getShort buffer) 32768.0))))
    samples))

(defn samples->pcm
  "Convert finite Float32 samples to clipped PCM16 bytes."
  ^bytes [^floats samples]
  (let [buffer (doto (ByteBuffer/allocate (* 2 (alength samples)))
                 (.order ByteOrder/LITTLE_ENDIAN))]
    (dotimes [i (alength samples)]
      (let [sample (double (aget samples i))]
        (when-not (Double/isFinite sample) (throw (invalid-frame)))
        (.putShort buffer (short (long (Math/round (* 32767.0 (max -1.0 (min 1.0 sample)))))))))
    (.array buffer)))
