(ns com.blockether.vis.ext.foundation-voice.speech
  "The LOCAL speaking engines, registered into the engine-agnostic registry in
   `com.blockether.vis.internal.voice` — the mirror of `engine.clj`, which does
   the same for listening.

   Two engines, not one, because a voice means two different things: a Piper voice
   is a MODEL per speaker, while pocket-tts clones a reference CLIP. Vis fetches
   both. Every surface —
   the gateway, the TUI, the companion — sees the same
   `{:id :label :synthesize :voices :model-state :start-download}` and needs to
   know neither fact. pocket-tts declares two more, because a voice it can speak
   in is a RECORDING: importing one is the same act on every surface."
  (:require [com.blockether.vis.ext.foundation-voice.sherpa :as sherpa]
            [com.blockether.vis.ext.foundation-voice.tts :as tts]
            [com.blockether.vis.ext.foundation-voice.voices :as voices]
            [com.blockether.vis.internal.voice :as voice]))

(set! *warn-on-reflection* true)

(def piper-engine-id :piper-local)

(def pocket-engine-id :pocket-tts-local)

(defn register!
  "Idempotent — [[voice/register-engine!]] replaces by id. Piper registers FIRST
   and so is the default engine: it speaks a baked speaker with no clip to choose,
   which is the least a caller that named no voice has to know."
  []
  (voice/register-engine! :synthesize
                          {:id piper-engine-id
                           :label "Piper (local)"
                           :synthesize #(sherpa/call-native (fn []
                                                              (tts/synthesize! :piper %)))
                           :voices tts/piper-voices
                           :model-state #(tts/model-state :piper)
                           :start-download #(tts/start-download! :piper)})
  (voice/register-engine! :synthesize
                          {:id pocket-engine-id
                           :label "Pocket TTS (local)"
                           :synthesize #(sherpa/call-native (fn []
                                                              (tts/synthesize! :pocket-tts %)))
                           :voices tts/pocket-voices
                           ;; A pocket voice IS a reference clip, so this engine
                           ;; can learn one from any recording somebody has.
                           :import-voice voices/import!
                           :forget-voice voices/forget!
                           :model-state #(tts/model-state :pocket-tts)
                           :start-download #(tts/start-download! :pocket-tts)}))
