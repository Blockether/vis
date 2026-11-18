(ns com.blockether.vis.contract.voice-test
  (:require [com.blockether.vis.contract.voice :as voice]
            [lazytest.core :refer [defdescribe describe it expect]])
  (:import [java.nio ByteBuffer]))

(def ^:private header {:generation 3 :stream-id 7 :sample-offset 0})

(defn- refused? [f] (try (f) false (catch clojure.lang.ExceptionInfo _ true)))

(defdescribe
  live-voice-contract
  (describe "endpoint timing"
            (it "allows an additional four hundred milliseconds for thinking pauses"
                (expect (= 1100 (:endpoint-silence-ms voice/limits)))
                (expect (= 120 (:min-speech-ms voice/limits)))))
  (describe
    "control validation"
    (it "accepts only the closed control vocabulary"
        (expect (voice/valid? "client_control" {:type "heartbeat"}))
        (expect (not (voice/valid? "client_control" {:type "heartbeat" :text "execute this"})))
        (expect (not (voice/valid? "client_control" {:type "transcript" :text "execute this"}))))
    (it "requires explicit local speech and one supported language"
        (let [start {:schema 1
                     :client-id "f8bd47a5-4e53-4a25-bc56-72f727095ab2"
                     :language "en"
                     :privacy "local_speech"
                     :muted true}]
          (expect (voice/valid? "start" start))
          (expect (not (voice/valid? "start" (assoc start :privacy "device"))))
          (expect (not (voice/valid? "start" (dissoc start :muted)))))))
  (describe "bounded binary frames"
            (it "roundtrips unsigned identifiers and PCM without retaining mutable input"
                (let [pcm
                      (byte-array [0 0 -1 127 0 -128])

                      bytes
                      (voice/encode-frame (assoc header :generation 4294967295) pcm)

                      decoded
                      (voice/decode-frame (ByteBuffer/wrap bytes))]

                  (expect (= 4294967295 (:generation decoded)))
                  (expect (= 3 (:sample-count decoded)))
                  (expect (= (vec pcm) (vec (:pcm decoded))))
                  (aset-byte bytes 12 (byte 1))
                  (expect (= 0 (first (:pcm decoded))))))
            (it "rejects malformed sizes and headers before native decoding"
                (expect (refused? #(voice/decode-frame (byte-array 12))))
                (expect (refused? #(voice/decode-frame (byte-array 15))))
                (expect (refused? #(voice/decode-frame (byte-array 974))))
                (expect (refused? #(voice/encode-frame header (byte-array 961))))
                (expect (refused? #(voice/encode-frame header (byte-array 962))))
                (expect (refused? #(voice/encode-frame (assoc header :sequence 0) (byte-array 2)))))
            (it "bounds one media frame to twenty milliseconds"
                (expect (= 480 (:frame-samples voice/limits)))
                (expect (= 20 (:frame-ms voice/limits)))
                (expect (= 972 (alength (voice/encode-frame header (byte-array 960))))))
            (it "clips finite PCM and rejects non-finite samples"
                (let [pcm
                      (voice/samples->pcm (float-array [-2.0 0.0 2.0]))

                      samples
                      (voice/pcm->samples pcm)]

                  (expect (< (aget samples 0) -0.99))
                  (expect (= 0.0 (aget samples 1)))
                  (expect (> (aget samples 2) 0.99)))
                (expect (refused? #(voice/samples->pcm (float-array [Float/NaN])))))))
