(ns com.blockether.vis.ext.voice.core-test
  (:require [com.blockether.vis.ext.voice.core :as voice]
            [lazytest.core :refer [defdescribe it expect]]))

(defdescribe voice-config-test
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
    (let [dir (java.nio.file.Files/createTempDirectory "vis-piper-model-test" (make-array java.nio.file.attribute.FileAttribute 0))]
      (try
        (spit (str (.resolve dir "en_US-ryan-high.onnx")) "model")
        (spit (str (.resolve dir "tokens.txt")) "tokens")
        (java.nio.file.Files/createDirectories (.resolve dir "espeak-ng-data")
          (make-array java.nio.file.attribute.FileAttribute 0))
        (expect (true? (voice/model-installed? (str dir))))
        (expect (= (str (.resolve dir "en_US-ryan-high.onnx"))
                  (:model (voice/model-files (str dir)))))
        (finally
          (doseq [path (reverse (file-seq (.toFile dir)))]
            (.delete path))))))

  (it "synthesizes with the default model directory when :model-dir is omitted"
    (let [out     (java.io.File/createTempFile "vis-voice-test" ".wav")
          ensured (atom nil)
          saved   (atom nil)]
      (try
        (with-redefs [voice/ensure-model! (fn [dir]
                                            (reset! ensured dir)
                                            dir)
                      com.blockether.vis.ext.voice.core/new-instance
                      (fn [class-name & _args] {:class class-name})
                      com.blockether.vis.ext.voice.core/call!
                      (fn [_target method & args]
                        (when (= "save" method)
                          (reset! saved (first args)))
                        (when (= "generate" method)
                          :audio))]
          (let [result (voice/synthesize-file! "hello" {:out-file out})]
            (expect (= (voice/model-dir) @ensured))
            (expect (= (str out) @saved))
            (expect (= {:voice voice/default-piper-voice
                        :model-dir (voice/model-dir)
                        :out-file (str (.getAbsoluteFile out))}
                      result))))
        (finally
          (.delete out)))))

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
    (let [cli (-> voice/voice-extension :ext/cli first)]
      (expect (= "voice" (:cmd/name cli)))
      (expect (= ["models"] (mapv :cmd/name (:cmd/subcommands cli))))))

  (it "contributes voice-specific doctor diagnostics"
    (with-redefs [voice/model-dir (constantly "/tmp/piper")
                  voice/model-files (fn [_] {:model "/tmp/piper/model.onnx"
                                             :tokens "/tmp/piper/tokens.txt"
                                             :data "/tmp/piper/espeak-ng-data"})
                  voice/model-installed? (constantly true)
                  voice/model-status (constantly {:parakeet {:installed? true}})
                  com.blockether.vis.ext.voice.core/executable? (constantly true)
                  clojure.core/requiring-resolve (fn [sym]
                                                   (case sym
                                                     com.blockether.vis.ext.voice.asr/transcribe-file! identity
                                                     com.blockether.vis.ext.voice.core/synthesize-file! identity))]
      (let [msgs ((:ext/doctor-fn voice/voice-extension) {})]
        (expect (= [::voice/runtime ::voice/ffmpeg ::voice/piper ::voice/parakeet]
                  (mapv :check-id msgs)))
        (expect (every? #(= :info (:level %)) msgs)))))

  (it "defers TUI voice input namespace until the channel contribution is invoked"
    (let [contribution (first (get-in voice/voice-extension
                                [:ext/channel-contributions :tui.slot/commands]))
          calls (atom [])]
      (with-redefs [clojure.core/requiring-resolve
                    (fn [sym]
                      (swap! calls conj sym)
                      (expect (= 'com.blockether.vis.ext.voice.input/tui-commands sym))
                      (fn [ctx]
                        [{:id :test/voice-command
                          :ctx ctx}]))]
        (expect (= [{:id :test/voice-command
                     :ctx {:source :test}}]
                  ((:fn contribution) {:source :test})))
        (expect (= ['com.blockether.vis.ext.voice.input/tui-commands] @calls))))))
