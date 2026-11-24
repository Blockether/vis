(ns com.blockether.vis.internal.speech.transcode
  "Whatever somebody recorded, as the 16-bit PCM WAV every local model reads.

   A recorder never asks what the model wants. An iPhone memo is `.m4a` (AAC in an
   MP4 box), an Android one `.amr`, `.aac` or `.m4a`, a browser records `.ogg` or
   `.webm`, a shared clip arrives as `.mp3`, and sherpa-onnx reads exactly ONE
   container: RIFF/WAVE, 16-bit PCM. So every path that turns audio into text meets
   the SAME question — voice INPUT in the TUI, a recording ATTACHED to a message,
   a clip IMPORTED as a voice — and it is answered once, here.

   Vis ships no decoder of its own: AAC, Opus and AMR are a codec suite rather than
   a namespace, and a wrong decoder is silence that transcribes into confident
   words. `ffmpeg` is the one converter a machine reliably has, so a non-WAV
   recording goes through it
   into a TEMP mono file that is deleted the moment the work is done. A caller that
   names a sample rate (Parakeet reads 16 kHz) also gets every other WAV converted to
   that rate, so a 48 kHz recording is not held in memory at three times its size."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.paths :as paths])
  (:import [java.io File RandomAccessFile]
           [java.nio.charset StandardCharsets]))

(set! *warn-on-reflection* true)

(defn wav?
  "Is this file a RIFF/WAVE container at all? Cheap enough to ask before deciding
   whether a converter is needed — the head, never the whole recording."
  [^File file]
  (and (some? file)
       (.isFile file)
       (> (.length file) 44)
       (with-open [in (io/input-stream file)]
         (let [head (byte-array 12)]
           (and (= 12 (.read in head 0 12))
                (= "RIFF" (String. head 0 4 StandardCharsets/US_ASCII))
                (= "WAVE" (String. head 8 4 StandardCharsets/US_ASCII)))))))

(defn- wav-format
  "`{:format :channels :rate :bits}` from the fmt chunk of a WAV, or nil when it has none."
  [^File file]
  (with-open [raf (RandomAccessFile. file "r")]
    (loop [pos 12]
      (when (<= (+ pos 8) (.length raf))
        (.seek raf pos)
        (let [id (let [b (byte-array 4)]
                   (.readFully raf b)
                   (String. b StandardCharsets/US_ASCII))
              size (bit-and 0xFFFFFFFF (Integer/reverseBytes (.readInt raf)))]

          (if (and (= "fmt " id) (>= size 16))
            (let [fmt (Short/toUnsignedInt (Short/reverseBytes (.readShort raf)))
                  channels (Short/toUnsignedInt (Short/reverseBytes (.readShort raf)))
                  rate (Integer/reverseBytes (.readInt raf))]

              (.skipBytes raf 6)
              {:format fmt
               :channels channels
               :rate rate
               :bits (Short/toUnsignedInt (Short/reverseBytes (.readShort raf)))})
            (recur (+ pos 8 size (rem size 2)))))))))

(defn ffmpeg-path
  "The `ffmpeg` this process can execute, or nil. Resolved per call: a machine that
   installs it while Vis runs converts the next recording without a restart."
  []
  (some #(let [f (io/file % "ffmpeg")] (when (.canExecute f) (str f)))
        (str/split (str (System/getenv "PATH")) (re-pattern File/pathSeparator))))

(defn missing-ffmpeg-message
  "The refusal a human can ACT on: what is wrong, and the one command that fixes it."
  [^File file]
  (str "That recording is not a WAV file, and this machine has no ffmpeg to convert it."
       " Install ffmpeg, or convert it first: ffmpeg -i "
       (if file (.getName file) "<file>")
       " -ac 1 -c:a pcm_s16le clip.wav"))

(defn ->wav!
  "`file` as a readable 16-bit PCM WAV, converted only when it is not one already.
   With `rate`, a WAV whose fmt chunk is not mono 16-bit PCM at `rate` is converted
   too, when this machine has ffmpeg.

   Answers `{:file <File> :is-temp <bool>}`; the caller DELETES a temp file (see
   [[with-wav]], which is that contract written down). Throws `:speech/no-ffmpeg`
   when conversion is needed and impossible, `:speech/unreadable` when ffmpeg
   refused the container."
  [^File file & [rate]]
  (if (and (wav? file)
           (or (nil? rate)
               (nil? (ffmpeg-path))
               (contains? #{nil {:format 1 :channels 1 :rate rate :bits 16}} (wav-format file))))
    {:file file :is-temp false}
    (let [ffmpeg
          (ffmpeg-path)

          converted
          (File/createTempFile "vis-transcode" ".wav" (paths/temp-dir))]

      (when-not ffmpeg
        (.delete converted)
        (throw (ex-info (missing-ffmpeg-message file) {:type :speech/no-ffmpeg :path (str file)})))
      (try (let [^java.util.List command
                 (-> [ffmpeg "-v" "error" "-y" "-i" (str file) "-ac" "1"]
                     (cond->
                       rate
                       (conj "-ar" (str rate)))
                     (conj "-c:a" "pcm_s16le" (str converted)))

                 ^Process process
                 (.start (doto (ProcessBuilder. command) (.redirectErrorStream true)))

                 output
                 (slurp (.getInputStream process))

                 code
                 (.waitFor process)]

             (when-not (zero? code)
               (throw (ex-info (str "ffmpeg could not read that recording: " (str/trim output))
                               {:type :speech/unreadable :exit code :path (str file)})))
             {:file converted :is-temp true})
           (catch Throwable t (.delete converted) (throw t))))))

(defn with-wav
  "Call `f` with `file` as a 16-bit PCM WAV and delete the conversion afterwards.

   The deletion is the whole point of the fn: a converted memo is a temp file per
   transcription, and a caller that forgets fills the temp directory with the
   user's own speech. `rate` is the sample rate the reader needs ([[->wav!]])."
  ([^File file f] (with-wav file nil f))
  ([^File file rate f]
   (let [{:keys [^File file is-temp]} (->wav! file rate)]
     (try (f file) (finally (when is-temp (.delete file)))))))
