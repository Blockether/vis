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
            ((recorder-var 'recorder-commands) output)]

        (expect (= [:pipewire :pulse] (mapv first commands)))
        (expect (= ["pw-record" "--format=s16" "--rate=16000" "--channels=1"
                    (.getAbsolutePath output)]
                   (second (first commands))))
        (expect (= ["parec" "--file-format=wav" "--format=s16le" "--rate=16000" "--channels=1"
                    (.getAbsolutePath output)]
                   (second (second commands)))))))
