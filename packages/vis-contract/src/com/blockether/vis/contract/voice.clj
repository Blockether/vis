(ns com.blockether.vis.contract.voice
  "Live voice messages and bounded PCM frames. JSON Schema owns the wire vocabulary."
  (:require [clojure.string :as str]
            [com.blockether.vis.contract.document :as document]
            [com.blockether.vis.contract.wire :as wire])
  (:import [java.nio ByteBuffer ByteOrder]))

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

(defn- pcm-size?
  "True for an even PCM16 size of one sample to one frame."
  [size]
  (and (even? (long size)) (<= 2 (long size) (* 2 (long (:frame-samples limits))))))

(defn encode-frame
  "Encode one header and its PCM16 bytes. The direction gives the kind and the
   length gives the sample count. Reject oversized frames before allocation."
  ^bytes [header ^bytes pcm]
  (let [size (alength pcm)]
    (when-not (and (pcm-size? size) (valid? "frame_header" header)) (throw (invalid-frame)))
    (let [buffer (doto (ByteBuffer/allocate (+ (long (:header-bytes limits)) size))
                   (.order ByteOrder/LITTLE_ENDIAN))]
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
        (- (.remaining buffer) (long (:header-bytes limits)))]

    (when-not (pcm-size? size) (throw (invalid-frame)))
    (.order buffer ByteOrder/LITTLE_ENDIAN)
    (let [header
          (reduce (fn [m field]
                    (assoc m field (Integer/toUnsignedLong (.getInt buffer))))
                  {}
                  fields)

          pcm
          (byte-array size)]

      (.get buffer pcm)
      (assoc header
        :sample-count (quot size 2)
        :pcm pcm))))

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
