(ns com.blockether.vis.ext.foundation-voice.core-test
  (:require [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.foundation-voice.core :as voice]
            [lazytest.core :refer [defdescribe it expect]]))

(defdescribe
  voice-config-test
  (it "uses the sharp Piper voice by default"
      (expect (= "en_US-ryan-high" voice/default-piper-voice)))
  (it "reports expected sherpa Piper model files"
      (expect (= {:model "/tmp/piper/en_US-ryan-high.onnx"
                  :tokens "/tmp/piper/tokens.txt"
                  :data "/tmp/piper/espeak-ng-data"}
                 (voice/model-files "/tmp/piper"))))
  (it "does not claim an absent model is installed"
      (expect (false? (voice/model-installed? "/path/that/does/not/exist"))))
  (it "accepts the Piper archive model filename as installed"
      (let [dir (java.nio.file.Files/createTempDirectory
                  "vis-piper-model-test"
                  (make-array java.nio.file.attribute.FileAttribute 0))]
        (try (spit (str (.resolve dir "en_US-ryan-high.onnx")) "model")
             (spit (str (.resolve dir "tokens.txt")) "tokens")
             (java.nio.file.Files/createDirectories
               (.resolve dir "espeak-ng-data")
               (make-array java.nio.file.attribute.FileAttribute 0))
             (expect (true? (voice/model-installed? (str dir))))
             ;; model-files returns `/`-separated paths on EVERY OS.
             (expect (= (.replace (str (.resolve dir "en_US-ryan-high.onnx")) "\\" "/")
                        (:model (voice/model-files (str dir)))))
             (finally (doseq [^java.io.File path (reverse (file-seq (.toFile dir)))]
                        (.delete path))))))
  (it "synthesizes with the default model directory when :model-dir is omitted"
      (let [out
            (java.io.File/createTempFile "vis-voice-test" ".wav")

            ensured
            (atom nil)

            saved
            (atom nil)]

        (try (with-redefs [voice/ensure-model!
                           (fn [dir]
                             (reset! ensured dir)
                             dir)

                           com.blockether.vis.ext.foundation-voice.core/new-instance
                           (fn [class-name & _args]
                             {:class class-name})

                           com.blockether.vis.ext.foundation-voice.core/call!
                           (fn [_target method & args]
                             (when (= "save" method) (reset! saved (first args)))
                             (when (= "generate" method) :audio))]

               (let [result (voice/synthesize-file! "hello" {:out-file out})]
                 (expect (= (voice/model-dir) @ensured))
                 (expect (= (str out) @saved))
                 (expect (= {:voice voice/default-piper-voice
                             :model-dir (voice/model-dir)
                             :out-file (str (.getAbsoluteFile out))}
                            result))))
             (finally (.delete out)))))
  (it "accepts canonical IR answers from the TUI response pipeline"
      (let [out
            (java.io.File/createTempFile "vis-voice-ir-test" ".wav")

            spoken
            (atom nil)]

        (try (with-redefs [voice/ensure-model!
                           identity

                           com.blockether.vis.ext.foundation-voice.core/new-instance
                           (fn [class-name & _args]
                             {:class class-name})

                           com.blockether.vis.ext.foundation-voice.core/call!
                           (fn [_target method & args]
                             (when (= "generate" method) (reset! spoken (first args)))
                             (when (= "save" method) nil)
                             (when (= "generate" method) :audio))]

               (let [result (voice/synthesize-file! [:ast {} [:p {} "hello"]] {:out-file out})]
                 (expect (str/includes? @spoken "hello"))
                 (expect (= (str (.getAbsoluteFile out)) (:out-file result)))))
             (finally (.delete out)))))
  (it "speaks prose from Markdown lists and tables without Markdown punctuation"
      (let [out
            (java.io.File/createTempFile "vis-voice-markdown-test" ".wav")

            spoken
            (atom nil)]

        (try (with-redefs [voice/ensure-model!
                           identity

                           com.blockether.vis.ext.foundation-voice.core/new-instance
                           (fn [class-name & _args]
                             {:class class-name})

                           com.blockether.vis.ext.foundation-voice.core/call!
                           (fn [_target method & args]
                             (when (= "generate" method) (reset! spoken (first args)))
                             (when (= "generate" method) :audio))]

               (voice/synthesize-file!
                 "- first win\n- second win\n\n| Area | Result |\n|---|---|\n| TUI | fixed |\n"
                 {:out-file out})
               (expect (str/includes? @spoken "first win"))
               (expect (str/includes? @spoken "second win"))
               (expect (str/includes? @spoken "Area: TUI; Result: fixed"))
               (expect (not (str/includes? @spoken "- first")))
               (expect (not (str/includes? @spoken "•")))
               (expect (not (str/includes? @spoken "|"))))
             (finally (.delete out)))))
  (it "redirects child TTS stdout and stderr away from the parent terminal into the Vis log"
      (let [log-file (java.io.File/createTempFile "vis-voice-log-test" ".log")]
        (try (spit log-file "before\n")
             (with-redefs [com.blockether.vis.ext.foundation-voice.core/tts-worker-argv
                           (fn []
                             ;; A portable two-stream stand-in for the sherpa worker:
                             ;; Windows has no `/bin/sh`, so drive cmd.exe there.
                             (if (str/starts-with? (str/lower-case (System/getProperty "os.name"
                                                                                       ""))
                                                   "win")
                               ["cmd" "/c" "echo sherpa-out&echo sherpa-err 1>&2"]
                               ["sh" "-c" "printf sherpa-out; printf sherpa-err >&2"]))]
               (let [captured (with-out-str (let [process (#'voice/start-tts-worker! log-file)]
                                              (expect (zero? (.waitFor ^Process process)))))]
                 (expect (= "" captured))
                 (let [logged (slurp log-file)]
                   (expect (str/starts-with? logged "before\n"))
                   (expect (str/includes? logged "sherpa-out"))
                   (expect (str/includes? logged "sherpa-err")))))
             (finally (.delete log-file)))))
  (it "uses the isolated worker for async TUI voice responses"
      (let [calls (atom [])]
        (with-redefs [com.blockether.vis.ext.foundation-voice.core/notify-progress!
                      (fn [& args]
                        (swap! calls conj (into [:notify] args)))
                      com.blockether.vis.ext.foundation-voice.core/synthesize-file-in-worker!
                      (fn [answer {:keys [out-file]}]
                        (swap! calls conj [:worker answer (str out-file)])
                        {:out-file (str out-file)})
                      voice/play-file! (fn [wav]
                                         (swap! calls conj [:play (str wav)])
                                         {:process (.start (ProcessBuilder.
                                                             ^"[Ljava.lang.String;"
                                                             (into-array String
                                                                         ["sh" "-c" "true"])))})]

          @(voice/speak-answer-async! "hello")
          (expect (some #(= [:worker "hello"] (subvec % 0 2)) @calls))
          (expect (some #(= :play (first %)) @calls)))))
  (it "clears the left header status when voice response completion becomes a left notification"
      (let [events
            (atom [])

            notifications
            (atom [])]

        (with-redefs [vis/publish-channel-event!
                      (fn [channel event]
                        (swap! events conj [channel event]))

                      vis/notify!
                      (fn [text & kvs]
                        (swap! notifications conj [text kvs]))]

          (#'voice/notify-progress! :ready "Voice response complete" 1 1)
          (expect (= [[:tui
                       {:op :status/clear
                        :id :voice/piper
                        :text "Voice response complete 100%"
                        :level :info
                        :model :voice/piper
                        :phase :ready
                        :bytes-read 1
                        :bytes-total 1}]]
                     @events))
          (expect (= [["Voice response complete 100%" [:level :info :ttl-ms 3000]]]
                     @notifications)))))
  (it "contributes spoken-answer prompt only for voice-response turns"
      (expect (nil? (voice/voice-response-prompt {})))
      (let [prompt (voice/voice-response-prompt {:turn/features {:voice-response? true}})]
        (expect (string? prompt))
        (expect (re-find #"canonical final answer as plain text" prompt))
        (expect (re-find #"saved to the session database" prompt))
        (expect (re-find #"manager update" prompt))
        (expect (re-find #"do not include extra trails" prompt))
        (expect (re-find #"do not read code aloud" prompt))))
  (it "mounts voice model commands under vis extensions voice"
      (let [cli (-> voice/voice-extension
                    :ext/cli
                    first)]
        (expect (= "voice" (:cmd/name cli)))
        (expect (= ["models"] (mapv :cmd/name (:cmd/subcommands cli))))))
  (it "contributes voice-specific doctor diagnostics"
      (with-redefs [voice/model-dir
                    (constantly "/tmp/piper")

                    voice/model-files
                    (fn [_]
                      {:model "/tmp/piper/model.onnx"
                       :tokens "/tmp/piper/tokens.txt"
                       :data "/tmp/piper/espeak-ng-data"})

                    voice/model-installed?
                    (constantly true)

                    voice/model-status
                    (constantly {:parakeet {:installed? true}})

                    com.blockether.vis.ext.foundation-voice.core/executable?
                    (constantly true)

                    clojure.core/requiring-resolve
                    (fn [sym]
                      (case sym
                        com.blockether.vis.ext.foundation-voice.asr/transcribe-file!
                        identity

                        com.blockether.vis.ext.foundation-voice.core/synthesize-file!
                        identity))]

        (let [msgs ((:ext/doctor-fn voice/voice-extension) {})]
          (expect (= [::voice/runtime ::voice/ffmpeg ::voice/piper ::voice/parakeet]
                     (mapv :check-id msgs)))
          (expect (every? #(= :info (:level %)) msgs)))))
  (it "defers voice input namespace until the /voice slash run-fn fires (K10)"
      ;; The declarative `/voice` slash spec lazily requiring-resolves
      ;; `toggle-recording!` from the input ns so the host doesn't pay
      ;; the audio stack cost until the user actually toggles voice.
      (let [voice-slash
            (first (filter #(= "voice" (:slash/name %))
                           (:ext/slash-commands voice/voice-extension)))

            calls
            (atom [])]

        (with-redefs [clojure.core/requiring-resolve
                      (fn [sym]
                        (swap! calls conj sym)
                        (expect (= 'com.blockether.vis.ext.foundation-voice.input/toggle-recording!
                                   sym))
                        (fn [ctx]
                          (swap! calls conj [:invoked ctx])
                          :toggled))]
          (let [result ((:slash/run-fn voice-slash) {:source :test})]
            (expect (= :ok (:slash/status result)))
            (expect (= [:invoked {:source :test}] (last @calls)))
            (expect (= 'com.blockether.vis.ext.foundation-voice.input/toggle-recording!
                       (first @calls))))))))
