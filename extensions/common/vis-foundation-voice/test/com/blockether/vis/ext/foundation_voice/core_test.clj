(ns com.blockether.vis.ext.foundation-voice.core-test
  (:require [com.blockether.vis.ext.foundation-voice.core :as voice]
            [lazytest.core :refer [defdescribe it expect]]))

(defdescribe
  voice-config-test
  (it "mounts voice model commands under vis extensions voice"
      (let
        [cli (-> voice/voice-extension
                 :ext/cli
                 first)]
        (expect (= "voice" (:cmd/name cli)))
        (expect (= ["models"] (mapv :cmd/name (:cmd/subcommands cli))))))
  (it "contributes voice-specific doctor diagnostics"
      (with-redefs
        [voice/model-status
         (constantly {:parakeet {:installed? true}})

         com.blockether.vis.ext.foundation-voice.core/executable?
         (constantly true)

         clojure.core/requiring-resolve
         (fn [sym]
           (case sym
             com.blockether.vis.ext.foundation-voice.asr/transcribe-file!
             identity))]

        (let [msgs ((:ext/doctor-fn voice/voice-extension) {})]
          (expect (= [::voice/runtime ::voice/ffmpeg ::voice/parakeet] (mapv :check-id msgs)))
          (expect (every? #(= :info (:level %)) msgs)))))
  (it "defers voice input namespace until the /voice slash run-fn fires (K10)"
      ;; The declarative `/voice` slash spec lazily requiring-resolves
      ;; `toggle-recording!` from the input ns so the host doesn't pay
      ;; the audio stack cost until the user actually toggles voice.
      (let
        [voice-slash
         (first (filter #(= "voice" (:slash/name %)) (:ext/slash-commands voice/voice-extension)))

         calls
         (atom [])]

        (with-redefs
          [clojure.core/requiring-resolve
           (fn [sym]
             (swap! calls conj sym)
             (expect (= 'com.blockether.vis.ext.foundation-voice.input/toggle-recording! sym))
             (fn [ctx]
               (swap! calls conj [:invoked ctx])
               :toggled))]
          (let [result ((:slash/run-fn voice-slash) {:source :test})]
            (expect (= :ok (:slash/status result)))
            (expect (= [:invoked {:source :test}] (last @calls)))
            (expect (= 'com.blockether.vis.ext.foundation-voice.input/toggle-recording!
                       (first @calls))))))))
