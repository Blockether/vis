(ns com.blockether.vis.tui.voice-recorder-test
  (:require [clojure.java.io :as io]
            [com.blockether.vis.tui.voice-recorder :as recorder]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [java.io ByteArrayInputStream]
           [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]
           [javax.sound.sampled AudioFileFormat$Type AudioInputStream AudioSystem]))

(defn- recorder-var [symbol] (ns-resolve 'com.blockether.vis.tui.voice-recorder symbol))

(defn- with-recorder-directory
  [f]
  (let [dir (.toFile (Files/createTempDirectory "vis-recorder-298-" (make-array FileAttribute 0)))]
    (try (f dir)
         (finally (doseq [file (reverse (file-seq dir))]
                    (io/delete-file file true))))))

;; Regression, issue #172: WSL2 exposes microphone capture through PipeWire/Pulse
;; sockets, but a Java Sound failure ended recording before either backend was tried.
(defdescribe
  pipewire-recorder-fallback-test
  (it "falls back from Java Sound to the Linux audio-server recorder"
      (let [calls
            (atom [])

            output
            (recorder/default-output-file)]

        (with-redefs-fn {(recorder-var 'linux-host?) (constantly true)
                         (recorder-var 'start-java-sound!) (fn [_]
                                                             (swap! calls conj :java-sound)
                                                             (throw (IllegalArgumentException.
                                                                      "no ALSA capture line")))
                         (recorder-var 'start-external!) (fn [file]
                                                           (swap! calls conj :external)
                                                           {:backend :pipewire :file file})}
          #(let [started (recorder/start! output)] (expect (= [:java-sound :external] @calls))
             (expect (= :pipewire (:backend started))) (expect (= output (:file started)))))))
  (it "defines PipeWire first and PulseAudio second at the ASR wire format"
      (let [output
            (recorder/default-output-file)

            commands
            (with-redefs-fn {(recorder-var 'macos-host?) (constantly false)}
              #((recorder-var 'recorder-commands) output))]

        (expect (= [:pipewire :pulse] (mapv first commands)))
        (expect (= ["pw-record" "--format=s16" "--rate=16000" "--channels=1"
                    (.getAbsolutePath output)]
                   (second (first commands))))
        (expect (= ["parec" "--file-format=wav" "--format=s16le" "--rate=16000" "--channels=1"
                    (.getAbsolutePath output)]
                   (second (second commands)))))))

;; Regression, issue #293: macOS native Java Sound may not expose a capture line.
(defdescribe
  macos-recorder-fallback-test
  (it "falls back to an external recorder and routes it through stop!"
      (let [calls
            (atom [])

            output
            (recorder/default-output-file)]

        (with-redefs-fn {(recorder-var 'linux-host?) (constantly false)
                         (recorder-var 'macos-host?) (constantly true)
                         (recorder-var 'start-java-sound!) (fn [_]
                                                             (swap! calls conj :java-sound)
                                                             (throw (IllegalArgumentException.
                                                                      "no capture line")))
                         (recorder-var 'start-external!) (fn [file]
                                                           (swap! calls conj :external)
                                                           {:backend :sox :file file})
                         (recorder-var 'stop-external!) (fn [rec]
                                                          (swap! calls conj :stop-external)
                                                          (:file rec))}
          #(let [started (recorder/start! output)] (expect (= [:java-sound :external] @calls))
             (expect (= :sox (:backend started))) (expect (= output (recorder/stop! started)))
             (expect (= [:java-sound :external :stop-external] @calls))))))
  (it "tries SoX then FFmpeg with mono 16 kHz signed WAV output"
      (let [output
            (recorder/default-output-file)

            path
            (.getAbsolutePath output)

            commands
            (with-redefs-fn {(recorder-var 'macos-host?) (constantly true)}
              #((recorder-var 'recorder-commands) output))]

        (expect (= [:sox :ffmpeg] (mapv first commands)))
        (expect (= ["sox" "-q" "-d" "-r" "16000" "-c" "1" "-b" "16" "-e" "signed-integer" path]
                   (second (first commands))))
        (expect (= ["ffmpeg" "-nostdin" "-f" "avfoundation" "-i" ":default" "-ar" "16000" "-ac" "1"
                    "-c:a" "pcm_s16le" "-f" "wav" path]
                   (second (second commands)))))))

;; Regression, issue #293: macOS external backends must fail over and retain the cause.
(defdescribe
  macos-recorder-errors-test
  (it "tries FFmpeg when SoX cannot start"
      (let [calls
            (atom [])

            output
            (recorder/default-output-file)]

        (with-redefs-fn {(recorder-var 'macos-host?) (constantly true)
                         (recorder-var 'start-command!) (fn [file [backend _]]
                                                          (swap! calls conj backend)
                                                          (if (= backend :sox)
                                                            (throw (ex-info "SoX unavailable" {}))
                                                            {:backend backend :file file}))}
          #(let [started ((recorder-var 'start-external!) output)] (expect (= [:sox :ffmpeg]
                                                                              @calls))
             (expect (= :ffmpeg (:backend started))) (expect (= output (:file started)))))))
  (it "reports both attempts and microphone access steps when all backends fail"
      (let [output
            (recorder/default-output-file)

            error
            (with-redefs-fn {(recorder-var 'linux-host?) (constantly false)
                             (recorder-var 'macos-host?) (constantly true)
                             (recorder-var 'start-java-sound!) (fn [_]
                                                                 (throw (IllegalArgumentException.
                                                                          "no capture line")))
                             (recorder-var 'start-command!) (fn [_ [backend _]]
                                                              (throw (ex-info "microphone denied"
                                                                              {:backend backend})))}
              #(try (recorder/start! output) (catch clojure.lang.ExceptionInfo t t)))]

        (expect (= :com.blockether.vis.tui.voice-recorder/no-recorder (:type (ex-data error))))
        (expect (= "no capture line" (:java-sound-error (ex-data error))))
        (expect (= [:sox :ffmpeg] (mapv :backend (:attempts (ex-data error)))))
        (expect (every? #(= "microphone denied" (:error %)) (:attempts (ex-data error))))
        (expect (= :permission-denied (:reason (ex-data error))))
        (expect (every? #(= :permission-denied (:reason %)) (:attempts (ex-data error))))
        (expect (re-find #"Microphone" (:remediation (ex-data error))))
        (expect (not (re-find #"Install" (:remediation (ex-data error))))))))

;; Regression, issue #298: absent capture tools were reported as microphone-permission failures.
(defdescribe missing-macos-recorder-test
             (it "distinguishes missing programs from microphone or permission failures"
                 (let [output
                       (recorder/default-output-file)

                       error
                       (with-redefs-fn
                         {(recorder-var 'linux-host?) (constantly false)
                          (recorder-var 'macos-host?) (constantly true)
                          (recorder-var 'start-java-sound!) (fn [_]
                                                              (throw (IllegalArgumentException.
                                                                       "no capture line")))
                          (recorder-var 'recorder-commands)
                          (fn [file]
                            [[:sox ["vis-issue-298-sox-unavailable" (.getAbsolutePath file)]]
                             [:ffmpeg
                              ["vis-issue-298-ffmpeg-unavailable" (.getAbsolutePath file)]]])}
                         #(try (recorder/start! output) (catch clojure.lang.ExceptionInfo t t)))

                       data
                       (ex-data error)]

                   (expect (= :missing-executable (:reason data)))
                   (expect (= "no capture line" (:java-sound-error data)))
                   (expect (= [:sox :ffmpeg] (mapv :backend (:attempts data))))
                   (expect (every? #(= :missing-executable (:reason %)) (:attempts data)))
                   (expect (re-find #"brew install sox" (:remediation data)))
                   (expect (not (re-find #"Microphone|permission" (:remediation data)))))))

;; Regression, issue #298: launch environments can omit both Homebrew prefixes from PATH.
(defdescribe
  recorder-executable-discovery-test
  (it "checks both standard macOS Homebrew locations after PATH"
      (let [directories (with-redefs-fn {(recorder-var 'macos-host?) (constantly true)}
                          #((recorder-var 'command-directories)))]
        (expect (= ["/opt/homebrew/bin" "/usr/local/bin"] (vec (take-last 2 directories))))))
  (it "skips directories and non-executable files, and prefers the first executable"
      (with-recorder-directory
        (fn [dir]
          (let [path-bin
                (io/file dir "path-bin")

                brew-bin
                (io/file dir "homebrew-bin")

                path-sox
                (io/file path-bin "sox")

                brew-sox
                (io/file brew-bin "sox")]

            (.mkdirs path-bin)
            (.mkdirs brew-bin)
            (.mkdirs (io/file path-bin "ffmpeg"))
            (spit path-sox "not executable")
            (.setExecutable path-sox false false)
            (spit brew-sox "#!/bin/sh\n")
            (.setExecutable brew-sox true)
            (with-redefs-fn {(recorder-var 'macos-host?) (constantly true)
                             (recorder-var 'command-directories)
                             (constantly [(.getAbsolutePath path-bin) (.getAbsolutePath brew-bin)])}
              #(do (expect (= [{:backend :sox :command "sox" :path (.getAbsolutePath brew-sox)}
                               {:backend :ffmpeg :command "ffmpeg" :path nil}]
                              (recorder/external-backends)))
                   (.setExecutable path-sox true)
                   (expect (= (.getAbsolutePath path-sox)
                              (:path (first (recorder/external-backends)))))))))))
  (it
    "launches a resolved recorder outside PATH and finalizes its WAV through stop!"
    (with-recorder-directory
      (fn [dir]
        (let [program
              (io/file dir "sox")

              fixture
              (io/file dir "fixture.wav")

              output
              (io/file dir "capture with spaces.wav")

              started
              (atom nil)]

          (with-open [stream (AudioInputStream. (ByteArrayInputStream. (byte-array 3200))
                                                (recorder/audio-format)
                                                1600)]
            (AudioSystem/write stream AudioFileFormat$Type/WAVE fixture))
          (spit program
                (str "#!/bin/sh\nfor output; do :; done\n/bin/cp '" (.getAbsolutePath fixture)
                     "' \"$output\"\n" "while IFS= read -r line; do :; done\n"))
          (.setExecutable program true)
          (with-redefs-fn {(recorder-var 'macos-host?) (constantly true)
                           (recorder-var 'command-directories) (constantly [(.getAbsolutePath dir)])
                           (recorder-var 'start-java-sound!) (fn [_]
                                                               (throw (IllegalArgumentException.
                                                                        "no capture line")))}
            #(try (let [rec (recorder/start! output)]
                    (reset! started rec)
                    (expect (= :sox (:backend rec)))
                    (expect (= (.getAbsolutePath program) (:command rec)))
                    (expect (loop [attempts 100]
                              (cond (and (.isFile output) (pos? (.length output))) true
                                    (zero? attempts) false
                                    :else (do (Thread/sleep 10) (recur (dec attempts)))))
                            (slurp (:stderr-file rec)))
                    (expect (= output (recorder/stop! rec)))
                    (reset! started nil)
                    (with-open [stream (AudioSystem/getAudioInputStream output)]
                      (let [format (.getFormat stream)]
                        (expect (= 16000.0 (.getSampleRate format)))
                        (expect (= 16 (.getSampleSizeInBits format)))
                        (expect (= 1 (.getChannels format)))
                        (expect (= 1600 (.getFrameLength stream))))))
                  (finally (when @started
                             (try (recorder/stop! @started) (catch Throwable _)))))))))))

;; Regression, issue #298: an installed recorder's capture failure does not mean it is missing.
(defdescribe
  recorder-capture-failure-test
  (it "does not recommend installing a recorder when an installed backend cannot capture"
      (let [error
            (with-redefs-fn {(recorder-var 'macos-host?) (constantly true)
                             (recorder-var 'start-java-sound!) (fn [_]
                                                                 (throw (IllegalArgumentException.
                                                                          "no capture line")))
                             (recorder-var 'start-command!)
                             (fn [_ [backend _]]
                               (throw (ex-info (if (= :sox backend) "missing SoX" "no input device")
                                               {:reason (if (= :sox backend)
                                                          :missing-executable
                                                          :capture-failed)})))}
              #(try (recorder/start!) (catch clojure.lang.ExceptionInfo t t)))

            data
            (ex-data error)]

        (expect (= :capture-failed (:reason data)))
        (expect (= [:missing-executable :capture-failed] (mapv :reason (:attempts data))))
        (expect (re-find #"Sound > Input" (:remediation data)))
        (expect (not (re-find #"Install" (:remediation data)))))))
