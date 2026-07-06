(ns com.blockether.vis.ext.foundation-voice.recorder
  "Small Java Sound WAV recorder for push-to-talk voice input."
  (:require [clojure.java.io :as io])
  (:import [javax.sound.sampled AudioFileFormat$Type AudioFormat AudioInputStream AudioSystem
            DataLine$Info TargetDataLine]
           [java.io File]
           [java.util.concurrent FutureTask]))

(def ^:private sample-rate 16000.0)

(defn audio-format [] (AudioFormat. sample-rate 16 1 true false))

(defn default-output-file [] (doto (File/createTempFile "vis-voice-asr-" ".wav") (.deleteOnExit)))

(defn start!
  "Start recording microphone audio to a WAV file. Returns a recorder map;
   stop with [[stop!]]."
  ([] (start! (default-output-file)))
  ([path]
   (let [file
         (io/file path)

         format
         (audio-format)

         info
         (DataLine$Info. TargetDataLine format)

         line
         ^TargetDataLine (AudioSystem/getLine info)

         task
         (FutureTask. (fn []
                        (with-open [stream (AudioInputStream. line)]
                          (AudioSystem/write stream AudioFileFormat$Type/WAVE file))))]

     (.open line format)
     (.start line)
     (doto (Thread. task "vis-voice-asr-recorder") (.setDaemon true) (.start))
     {:file file :line line :task task :started-at-ms (System/currentTimeMillis)})))

(defn stop!
  "Stop a recorder returned by [[start!]]. Returns the WAV file."
  [{:keys [^TargetDataLine line ^FutureTask task file]}]
  (when line (try (.stop line) (catch Throwable _)) (try (.close line) (catch Throwable _)))
  (when task (try (.get task) (catch Throwable _)))
  file)
