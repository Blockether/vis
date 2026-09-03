(ns com.blockether.vis.internal.speech.synthesis
  "The gateway's built-in local Piper and pocket-tts speaking engines."
  (:require [com.blockether.vis.internal.speech.sherpa :as sherpa]
            [com.blockether.vis.internal.speech.tts :as tts]
            [com.blockether.vis.internal.speech.voices :as voices]))

(set! *warn-on-reflection* true)

(def piper-engine-id :piper-local)

(def pocket-engine-id :pocket-tts-local)

(def descriptors
  "The gateway's fixed speaking engines, with Piper as the default."
  [{:id piper-engine-id
    :label "Piper (local)"
    :synthesize #(sherpa/call-native (fn []
                                       (tts/synthesize! :piper %)))
    :voices tts/piper-voices
    :model-state #(tts/model-state :piper)
    :start-download #(tts/start-download! :piper)
    :voice-model-state #(tts/model-state :piper %)
    :start-voice-download (fn [{:keys [voice-id is-license-accepted]}]
                            (tts/start-download! :piper voice-id is-license-accepted))
    :voice-sample tts/piper-sample
    :prepare-voice-sample (fn [voice-id]
                            (sherpa/call-native (fn []
                                                  (tts/prepare-piper-sample! voice-id))))}
   {:id pocket-engine-id
    :label "Pocket TTS (local)"
    :synthesize #(sherpa/call-native (fn []
                                       (tts/synthesize! :pocket-tts %)))
    :voices tts/pocket-voices
    :import-voice voices/import!
    :forget-voice voices/forget!
    :model-state #(tts/model-state :pocket-tts)
    :start-download #(tts/start-download! :pocket-tts)
    :voice-sample tts/pocket-sample}])
