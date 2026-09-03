(ns com.blockether.vis.tui.voice-output-test
  (:require [com.blockether.vis.tui.client :as vis]
            [com.blockether.vis.tui.voice-output :as voice-output]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe
  gateway-voice-output-test
  (it "asks the gateway session to synthesize and only plays the returned file locally"
      (let [file
            (doto (java.io.File/createTempFile "vis-speech-output-test" ".wav") (.deleteOnExit))

            call
            (atom nil)

            played
            (promise)]

        (with-redefs [vis/gateway-synthesize-speech! (fn [sid text opts]
                                                       (reset! call [sid text opts])
                                                       file)]
          (with-redefs-fn {#'voice-output/play-file! (fn [audio]
                                                       (deliver played audio))}
            (fn []
              (voice-output/speak! "session-1" "hello")
              (expect (= file (deref played 1000 nil)))
              (expect (= ["session-1" "hello" {:engine-id nil :voice-id nil}] @call))))))))
