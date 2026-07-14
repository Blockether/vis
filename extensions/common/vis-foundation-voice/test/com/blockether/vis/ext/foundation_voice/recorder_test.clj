(ns com.blockether.vis.ext.foundation-voice.recorder-test
  (:require [com.blockether.vis.ext.foundation-voice.recorder :as recorder]
            [lazytest.core :refer [defdescribe it expect]]))

(defdescribe recorder-test
             (it "builds the expected mono 16k PCM format"
                 (let [^javax.sound.sampled.AudioFormat fmt (recorder/audio-format)]
                   (expect (= 16000.0 (.getSampleRate fmt)))
                   (expect (= 1 (.getChannels fmt)))
                   (expect (= 16 (.getSampleSizeInBits fmt))))))
