(ns com.blockether.vis.ext.foundation-voice.transcode
  "Whatever somebody recorded, as the 16-bit PCM WAV every local model reads.

   A recorder never asks what the model wants. An iPhone memo is `.m4a` (AAC in an
   MP4 box), an Android one `.amr`, `.aac` or `.m4a`, a browser records `.ogg` or
   `.webm`, a shared clip arrives as `.mp3`, and sherpa-onnx reads exactly ONE
   container: RIFF/WAVE, 16-bit PCM. So every path that turns audio into text meets
   the SAME question — voice INPUT in the TUI, a recording ATTACHED to a message,
   a clip IMPORTED as a voice — and it is answered once, here.

   Vis ships no decoder of its own: AAC, Opus and AMR are a codec suite rather than
   a namespace, and a wrong decoder is silence that transcribes into confident
   words. `ffmpeg` is the one converter a machine reliably has (this extension's
   doctor already reports whether it does), so a non-WAV recording goes through it
   into a TEMP mono 16 kHz file that is deleted the moment the work is done."
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.io File]
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

   Answers `{:file <File> :is-temp <bool>}`; the caller DELETES a temp file (see
   [[with-wav]], which is that contract written down). Throws `:voice/no-ffmpeg`
   when conversion is needed and impossible, `:voice/unreadable` when ffmpeg
   refused the container."
  [^File file]
  (if (wav? file)
    {:file file :is-temp false}
    (let [ffmpeg
          (ffmpeg-path)

          converted
          (File/createTempFile "vis-transcode" ".wav")]

      (when-not ffmpeg
        (.delete converted)
        (throw (ex-info (missing-ffmpeg-message file) {:type :voice/no-ffmpeg :path (str file)})))
      (try (let [^java.util.List command
                 [ffmpeg "-v" "error" "-y" "-i" (str file) "-ac" "1" "-c:a" "pcm_s16le"
                  (str converted)]

                 ^Process process
                 (.start (doto (ProcessBuilder. command) (.redirectErrorStream true)))

                 output
                 (slurp (.getInputStream process))

                 code
                 (.waitFor process)]

             (when-not (zero? code)
               (throw (ex-info (str "ffmpeg could not read that recording: " (str/trim output))
                               {:type :voice/unreadable :exit code :path (str file)})))
             {:file converted :is-temp true})
           (catch Throwable t (.delete converted) (throw t))))))

(defn with-wav
  "Call `f` with `file` as a 16-bit PCM WAV and delete the conversion afterwards.

   The deletion is the whole point of the fn: a converted memo is a temp file per
   transcription, and a caller that forgets fills the temp directory with the
   user's own speech."
  [^File file f]
  (let [{:keys [^File file is-temp]} (->wav! file)]
    (try (f file) (finally (when is-temp (.delete file))))))
