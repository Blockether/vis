(ns com.blockether.vis.ext.foundation-voice.transcode-test
  "The one question every path from audio to text asks: is this a WAV, and if not,
   what turns it into one? A phone hands Vis an .m4a memo far more often than a WAV,
   so a refusal here is a refusal of the most likely recording in the world."
  (:require [com.blockether.vis.ext.foundation-voice.transcode :as transcode]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [java.io File]))

(defn- wav!
  "A one-channel 16-bit PCM WAV of `seconds`, carrying a tone: a converter has to
   move real samples, and ffmpeg refuses a file that is only a header."
  ^File [^double seconds]
  (let [rate
        16000

        frames
        (long (* seconds (double rate)))

        data-bytes
        (* frames 2)

        buffer
        (doto (java.nio.ByteBuffer/allocate (+ 44 data-bytes))
          (.order java.nio.ByteOrder/LITTLE_ENDIAN))

        file
        (doto (File/createTempFile "vis-transcode-test" ".wav") (.deleteOnExit))]

    (.put buffer (.getBytes "RIFF" "US-ASCII"))
    (.putInt buffer (+ 36 data-bytes))
    (.put buffer (.getBytes "WAVEfmt " "US-ASCII"))
    (.putInt buffer 16)
    (.putShort buffer (short 1))
    (.putShort buffer (short 1))
    (.putInt buffer rate)
    (.putInt buffer (* rate 2))
    (.putShort buffer (short 2))
    (.putShort buffer (short 16))
    (.put buffer (.getBytes "data" "US-ASCII"))
    (.putInt buffer data-bytes)
    (dotimes [i frames]
      (.putShort buffer (short (* 8000 (Math/sin (* 2 Math/PI 440 (/ (double i) rate)))))))
    (with-open [out (java.io.FileOutputStream. file)]
      (.write out (.array buffer)))
    file))

(defn- m4a!
  "The same tone as an AAC/MP4 memo - what an iPhone actually records - or nil on a
   machine with no ffmpeg to make one."
  ^File [^File wav]
  (when-let [ffmpeg (transcode/ffmpeg-path)]
    (let [out (doto (File/createTempFile "vis-transcode-test" ".m4a") (.deleteOnExit))
          p (.start (doto (ProcessBuilder. ^java.util.List
                                           [ffmpeg "-v" "error" "-y" "-i" (str wav) (str out)])
                      (.redirectErrorStream true)))]

      (slurp (.getInputStream p))
      (when (zero? (.waitFor p)) out))))

(defdescribe
  transcode-test
  (it "reads the container from the head, never the extension"
      (let [wav
            (wav! 0.25)

            renamed
            (doto (File/createTempFile "vis-transcode-test" ".m4a") (.deleteOnExit))]

        (expect (true? (transcode/wav? wav)))
        (java.nio.file.Files/copy (.toPath wav)
                                  (.toPath renamed)
                                  ^"[Ljava.nio.file.CopyOption;"
                                  (into-array java.nio.file.CopyOption
                                              [java.nio.file.StandardCopyOption/REPLACE_EXISTING]))
        ;; Named .m4a, still a WAV: nothing is converted and nothing is refused.
        (expect (true? (transcode/wav? renamed)))
        (expect (false? (transcode/wav? (doto (File/createTempFile "vis-transcode-test" ".wav")
                                          (.deleteOnExit)
                                          (spit "this is not a recording")))))))
  (it "hands a WAV straight through and never leaves a temp file behind"
      (let [wav
            (wav! 0.25)

            {:keys [file is-temp]}
            (transcode/->wav! wav)]

        (expect (false? is-temp))
        (expect (= (str wav) (str file)))))
  (it "converts an .m4a memo to a readable WAV and deletes the conversion"
      (let [source (m4a! (wav! 0.5))]
        ;; No ffmpeg on this machine is not a failing test - it is the one case
        ;; [[transcode/missing-ffmpeg-message]] exists to explain.
        (if source
          (let [seen (atom nil)
                answer (transcode/with-wav source
                                           (fn [^File wav]
                                             (reset! seen wav)
                                             (expect (true? (transcode/wav? wav)))
                                             :transcribed))]

            (expect (= :transcribed answer))
            (expect (false? (.exists ^File @seen))))
          (expect (re-find #"ffmpeg" (transcode/missing-ffmpeg-message (wav! 0.1)))))))
  (it "names the fix when a container needs converting and nothing can"
      (let [message (transcode/missing-ffmpeg-message (File. "/tmp/memo.m4a"))]
        (expect (re-find #"memo\.m4a" message))
        (expect (re-find #"pcm_s16le" message)))))
