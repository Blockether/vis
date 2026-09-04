(ns com.blockether.vis.tui.voice-recorder
  "Push-to-talk WAV capture.

   Java Sound is the primary backend. Linux falls back to the native PipeWire
   (`pw-record`) and PulseAudio (`parec`) clients because WSL2 exposes its Windows
   microphone through WSLg sockets without presenting an ALSA capture device to
   OpenJDK. Every backend writes mono signed 16-bit PCM at 16 kHz."
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.io File]
           [java.lang Process ProcessBuilder ProcessBuilder$Redirect]
           [java.util.concurrent FutureTask TimeUnit]
           [javax.sound.sampled AudioFileFormat$Type AudioFormat AudioInputStream AudioSystem
            DataLine$Info TargetDataLine]))

(def ^:private sample-rate 16000.0)
(def ^:private external-startup-ms 150)

(defn audio-format [] (AudioFormat. sample-rate 16 1 true false))

(defn default-output-file [] (doto (File/createTempFile "vis-speech-asr-" ".wav") (.deleteOnExit)))

(defn- start-java-sound!
  [^File file]
  (let [format
        (audio-format)

        info
        (DataLine$Info. TargetDataLine format)

        line
        ^TargetDataLine (AudioSystem/getLine info)

        task
        (FutureTask. (fn []
                       (with-open [stream (AudioInputStream. line)]
                         (AudioSystem/write stream AudioFileFormat$Type/WAVE file))))]

    (try (.open line format)
         (.start line)
         (doto (Thread. task "vis-speech-asr-recorder") (.setDaemon true) (.start))
         {:backend :java-sound :file file :line line :task task}
         (catch Throwable t (try (.close line) (catch Throwable _)) (throw t)))))

(defn- linux-host?
  []
  (str/includes? (str/lower-case (or (System/getProperty "os.name") "")) "linux"))

(defn- recorder-commands
  [^File file]
  (let [path (.getAbsolutePath file)]
    [[:pipewire ["pw-record" "--format=s16" "--rate=16000" "--channels=1" path]]
     [:pulse ["parec" "--file-format=wav" "--format=s16le" "--rate=16000" "--channels=1" path]]]))

(defn- error-text
  [^File file]
  (when (and file (.isFile file))
    (let [text (str/trim (slurp file))]
      (when-not (str/blank? text) (subs text 0 (min 500 (count text)))))))

(defn- start-command!
  [^File file [backend argv]]
  (io/delete-file file true)
  (let [stderr-file
        (doto (File/createTempFile "vis-speech-recorder-" ".log") (.deleteOnExit))

        builder
        (ProcessBuilder. ^"[Ljava.lang.String;" (into-array String argv))]

    (.redirectInput builder ProcessBuilder$Redirect/PIPE)
    (.redirectOutput builder ProcessBuilder$Redirect/DISCARD)
    (.redirectError builder (ProcessBuilder$Redirect/to stderr-file))
    (try (let [process ^Process (.start builder)]
           (try (Thread/sleep (long external-startup-ms))
                (if (.isAlive process)
                  {:backend backend
                   :command (first argv)
                   :file file
                   :process process
                   :stderr-file stderr-file}
                  (throw (ex-info
                           (or (error-text stderr-file)
                               (str (first argv) " exited before recording started"))
                           {:backend backend :command (first argv) :exit (.exitValue process)})))
                (catch Throwable t (when (.isAlive process) (.destroyForcibly process)) (throw t))))
         (catch Throwable t (io/delete-file stderr-file true) (throw t)))))

(defn- start-external!
  [^File file]
  (loop [[[backend _argv :as candidate] & more]
         (recorder-commands file)

         failures
         []]

    (if candidate
      (let [result (try {:recorder (start-command! file candidate)}
                        (catch InterruptedException t (.interrupt (Thread/currentThread)) (throw t))
                        (catch Throwable t
                          {:failure {:backend backend :error (or (ex-message t) (str t))}}))]
        (if-let [recorder (:recorder result)]
          recorder
          (recur more (conj failures (:failure result)))))
      (throw (ex-info "PipeWire/Pulse microphone capture is unavailable"
                      {:type ::no-external-recorder
                       :attempts failures
                       :remediation
                       (str "Install PipeWire tools (`pw-record`) or PulseAudio tools (`parec`), "
                            "then verify that the WSLg audio server is reachable.")})))))

(defn start!
  "Start recording microphone audio to a WAV file. Java Sound is preferred;
   Linux automatically falls back to PipeWire, then PulseAudio. Returns a
   recorder map; stop with [[stop!]]."
  ([] (start! (default-output-file)))
  ([path]
   (let [file (io/file path)]
     (try (start-java-sound! file)
          (catch InterruptedException t (.interrupt (Thread/currentThread)) (throw t))
          (catch Throwable java-sound-error
            (if-not (linux-host?)
              (throw java-sound-error)
              (try (start-external! file)
                   (catch InterruptedException t (.interrupt (Thread/currentThread)) (throw t))
                   (catch Throwable external-error
                     (throw (ex-info "No microphone capture backend could start"
                                     {:type ::no-recorder
                                      :backend :auto
                                      :java-sound-error (or (ex-message java-sound-error)
                                                            (str java-sound-error))
                                      :attempts (:attempts (ex-data external-error))
                                      :remediation (:remediation (ex-data external-error))}
                                     external-error))))))))))

(defn- stop-java-sound!
  [{:keys [^TargetDataLine line ^FutureTask task file]}]
  (when line (try (.stop line) (catch Throwable _)) (try (.close line) (catch Throwable _)))
  (when task (try (.get task) (catch Throwable _)))
  file)

(defn- stop-external!
  [{:keys [^Process process ^File stderr-file ^File file backend]}]
  (when (and process (.isAlive process)) (.destroy process))
  (when (and process (not (.waitFor process 5 TimeUnit/SECONDS)))
    (.destroyForcibly process)
    (.waitFor process))
  (let [failure (error-text stderr-file)]
    (io/delete-file stderr-file true)
    (if (and (.isFile file) (pos? (.length file)))
      file
      (throw (ex-info (or failure "The microphone recorder produced no audio file")
                      {:type ::empty-recording :backend backend})))))

(defn stop!
  "Stop a recorder returned by [[start!]]. Returns the WAV file."
  [{:keys [backend] :as recorder}]
  (case backend
    :java-sound
    (stop-java-sound! recorder)

    (:pipewire :pulse)
    (stop-external! recorder)

    (throw (ex-info "Unknown microphone recorder backend" {:backend backend}))))
