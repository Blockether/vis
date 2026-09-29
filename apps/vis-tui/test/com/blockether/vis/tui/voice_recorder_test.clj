(ns com.blockether.vis.tui.voice-recorder-test
  (:require [com.blockether.vis.tui.voice-recorder :as recorder]
            [lazytest.core :refer [defdescribe expect it]]))

(defn- recorder-var [symbol] (ns-resolve 'com.blockether.vis.tui.voice-recorder symbol))

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
        (expect (re-find #"Microphone" (:remediation (ex-data error)))))))
