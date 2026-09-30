(ns com.blockether.vis.tui.voice-recorder
  "Push-to-talk WAV capture.

   Java Sound is the primary backend. Linux falls back to PipeWire (`pw-record`)
   and PulseAudio (`parec`) for WSLg microphones; macOS falls back to SoX or
   FFmpeg when Java Sound cannot open a capture line. Every backend writes mono
   signed 16-bit PCM at 16 kHz."
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

(defn- macos-host? [] (str/includes? (str/lower-case (or (System/getProperty "os.name") "")) "mac"))

(defn- command-directories
  []
  (concat (remove str/blank?
            (str/split (or (System/getenv "PATH") "")
                       (re-pattern (java.util.regex.Pattern/quote File/pathSeparator))))
          (when (macos-host?) ["/opt/homebrew/bin" "/usr/local/bin"])))

(defn- executable-path
  [command]
  (let [file
        (io/file command)

        candidates
        (if (.isAbsolute file) [file] (map #(io/file % command) (command-directories)))]

    (some (fn [^File candidate]
            (when (and (.isFile candidate) (.canExecute candidate)) (.getAbsolutePath candidate)))
          candidates)))

(defn- recorder-programs
  []
  (if (macos-host?)
    [[:sox ["sox" "-q" "-d" "-r" "16000" "-c" "1" "-b" "16" "-e" "signed-integer"]]
     [:ffmpeg
      ["ffmpeg" "-nostdin" "-f" "avfoundation" "-i" ":default" "-ar" "16000" "-ac" "1" "-c:a"
       "pcm_s16le" "-f" "wav"]]]
    [[:pipewire ["pw-record" "--format=s16" "--rate=16000" "--channels=1"]]
     [:pulse ["parec" "--file-format=wav" "--format=s16le" "--rate=16000" "--channels=1"]]]))

(defn- recorder-commands
  [^File file]
  (mapv (fn [[backend argv]]
          [backend (conj argv (.getAbsolutePath file))])
        (recorder-programs)))

(defn external-backends
  "List external capture backends and their executable paths, or nil when absent.
   Checks PATH and the standard macOS Homebrew locations without opening a microphone."
  []
  (mapv (fn [[backend [command]]]
          {:backend backend :command command :path (executable-path command)})
        (recorder-programs)))

(defn- error-text
  [^File file]
  (when (and file (.isFile file))
    (let [text (str/trim (slurp file))]
      (when-not (str/blank? text) (subs text 0 (min 500 (count text)))))))

(defn- start-command!
  [^File file [backend argv]]
  (let [command
        (first argv)

        executable
        (or (executable-path command)
            (throw (ex-info (str "Capture program not found: " command)
                            {:backend backend :command command :reason :missing-executable})))

        argv
        (assoc argv 0 executable)]

    (io/delete-file file true)
    (let [stderr-file
          (doto (File/createTempFile "vis-speech-recorder-" ".log") (.deleteOnExit))

          builder
          (ProcessBuilder. ^"[Ljava.lang.String;" (into-array String argv))]

      (.redirectInput builder ProcessBuilder$Redirect/PIPE)
      (.redirectOutput builder ProcessBuilder$Redirect/DISCARD)
      (.redirectError builder (ProcessBuilder$Redirect/to stderr-file))
      (try (let [process ^Process (.start builder)]
             (try
               (Thread/sleep (long external-startup-ms))
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
           (catch Throwable t (io/delete-file stderr-file true) (throw t))))))

(defn- external-failure-data
  [failures]
  (let
    [reason
     (cond (every? #(= :missing-executable (:reason %)) failures) :missing-executable
           (some #(= :permission-denied (:reason %)) failures) :permission-denied
           :else :capture-failed)

     remediation
     (if (macos-host?)
       (case reason
         :missing-executable
         (str "SoX/FFmpeg were not found in PATH or the standard Homebrew locations. "
              "Install a recorder with `brew install sox` or `brew install ffmpeg`, then retry.")

         :permission-denied
         (str "A capture backend reported a permission error. Allow microphone access for "
              "your terminal or Vis in System Settings > Privacy & Security > Microphone, "
              "then retry.")

         (str "A capture backend is installed but could not record. Check the microphone in "
              "System Settings > Sound > Input. If access was blocked, allow it in "
              "System Settings > Privacy & Security > Microphone. "
              "Run `vis-agent tui --check-audio` for backend paths."))
       (if (= :missing-executable reason)
         (str "Install PipeWire tools (`pw-record`) or PulseAudio tools (`parec`), " "then retry.")
         "Check microphone access and verify that the PipeWire/Pulse audio server is reachable."))]

    {:type ::no-external-recorder :reason reason :attempts failures :remediation remediation}))

(defn- start-external!
  [^File file]
  (loop [[[backend _argv :as candidate] & more]
         (recorder-commands file)

         failures
         []]

    (if candidate
      (let
        [result
         (try
           {:recorder (start-command! file candidate)}
           (catch InterruptedException t (.interrupt (Thread/currentThread)) (throw t))
           (catch Throwable t
             (let
               [message (or (ex-message t) (str t))
                reason
                (or
                  (:reason (ex-data t))
                  (when
                    (re-find
                      #"(?i)(permission denied|not permitted|not authorized|access denied|microphone denied)"
                      message)
                    :permission-denied)
                  :capture-failed)]

               {:failure (merge (select-keys (ex-data t) [:command :exit])
                                {:backend backend :reason reason :error message})})))]
        (if-let [recorder (:recorder result)]
          recorder
          (recur more (conj failures (:failure result)))))
      (throw (ex-info "No external microphone capture backend could start"
                      (external-failure-data failures))))))

(defn start!
  "Start recording microphone audio to a WAV file. Java Sound is preferred;
   Linux falls back to PipeWire/PulseAudio and macOS to SoX/FFmpeg. Returns a
   recorder map; stop with [[stop!]]."
  ([] (start! (default-output-file)))
  ([path]
   (let [file (io/file path)]
     (try (start-java-sound! file)
          (catch InterruptedException t (.interrupt (Thread/currentThread)) (throw t))
          (catch Throwable java-sound-error
            (if-not (or (linux-host?) (macos-host?))
              (throw java-sound-error)
              (try (start-external! file)
                   (catch InterruptedException t (.interrupt (Thread/currentThread)) (throw t))
                   (catch Throwable external-error
                     (throw (ex-info "No microphone capture backend could start"
                                     {:type ::no-recorder
                                      :backend :auto
                                      :reason (:reason (ex-data external-error))
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

    (:pipewire :pulse :sox :ffmpeg)
    (stop-external! recorder)

    (throw (ex-info "Unknown microphone recorder backend" {:backend backend}))))
