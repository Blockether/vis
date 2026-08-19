(ns com.blockether.vis.ext.foundation-voice.engine
  "The LOCAL Parakeet transcription engine, registered into the engine-agnostic
   registry in `com.blockether.vis.internal.voice`.

   This namespace is the whole coupling between Vis and sherpa-onnx: the gateway,
   the TUI and the companion only ever see `{:id :label :transcribe}`. Swapping in
   a whisper.cpp server is therefore an extension that calls
   `voice/register-engine!` with its own `:transcribe`, and zero lines change
   anywhere else."
  (:require [clojure.java.io :as io]
            [com.blockether.vis.ext.foundation-voice.asr :as asr]
            [com.blockether.vis.ext.foundation-voice.sherpa :as sherpa]
            [com.blockether.vis.ext.foundation-voice.transcode :as transcode]
            [com.blockether.vis.internal.voice :as voice])
  (:import [java.io File]))

(set! *warn-on-reflection* true)

(def engine-id :parakeet-local)

(def ^:private ^:const model-poll-ms 400)

(defn- await-model!
  "Block until the model is installed, reporting the DOWNLOAD as `:preparing`
   progress. The transfer is a real part of \"how long until my text arrives\", so
   it is reported rather than hidden behind a silent multi-minute stall.

   Regression: a failed transfer used to be a VERDICT. `model-state` holds the
   failure until something starts a new download, and this loop threw the moment
   it saw one - so a single dropped connection made every later recording fail
   with the same stale message until Vis was restarted. A failure now buys one
   fresh attempt; only the second one is reported."
  [report]
  (loop [retried? false]
    (let [{:keys [state progress error]} (asr/model-state)]
      (case state
        :ready
        nil

        :failed
        (if retried?
          (throw (ex-info (str "Voice model download failed: " (or error "unknown error"))
                          {:type :voice-asr/model-download-failed :error error}))
          (do (report {:phase :preparing :progress 0})
              (asr/start-download!)
              (Thread/sleep (long model-poll-ms))
              (recur true)))

        (do (report {:phase :preparing :progress (or progress 0)})
            (asr/start-download!)
            (Thread/sleep (long model-poll-ms))
            (recur retried?))))))

(defn- clean-text
  "Parakeet's stutter/filler cleanup lives beside the TUI input that grew it;
   resolving it LAZILY keeps the recorder namespace out of a gateway that only
   wants to transcribe. Cleaning belongs to the ENGINE, so every surface — TUI,
   gateway, app — receives the same finished text."
  [text]
  (if-let [f (try (requiring-resolve
                    'com.blockether.vis.ext.foundation-voice.input/clean-transcript)
                  (catch Throwable _ nil))]
    (str (f text))
    (str text)))

(defn transcribe
  "The engine fn: `{:audio-path :on-progress}` -> transcript.

   The container is normalized FIRST ([[transcode/with-wav]]): sherpa-onnx reads
   16-bit PCM WAV and nothing else, while the recordings people actually hand Vis
   are `.m4a` memos and `.mp3` shares. Doing it here rather than at each surface
   is what makes an ATTACHED voice memo and the TUI's own microphone one path."
  [{:keys [audio-path on-progress]}]
  (let [report (fn [m]
                 (when on-progress (try (on-progress m) (catch Throwable _ nil))))]
    (await-model! report)
    (transcode/with-wav
      (io/file (str audio-path))
      (fn [^File wav]
        (sherpa/call-native
          #(clean-text (asr/transcribe-file! (asr/model-dir) (str wav) {:on-progress on-progress})))))))

(defn register!
  "Idempotent — [[voice/register-engine!]] replaces by id. `:model-state` and
   `:start-download` are what let a surface say \"downloading 42%\" without any
   caller knowing that THIS engine happens to need a 465MB model."
  []
  (voice/register-engine! :transcribe
                          {:id engine-id
                           :label "Parakeet (local)"
                           :transcribe transcribe
                           :model-state asr/model-state
                           :start-download asr/start-download!}))
