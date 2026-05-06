(ns com.blockether.vis.ext.voice-parakeet.core
  "TUI voice-input extension backed by local Parakeet-class ASR."
  (:require [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.voice-parakeet.recorder :as recorder]
            [com.blockether.vis.ext.voice-parakeet.rewrite :as rewrite]
            [com.blockether.vis.ext.voice-parakeet.sherpa :as sherpa]))

(defonce state
  (atom {:recorder nil
         :ticker nil}))

(defn- publish!
  [event]
  (vis/publish-channel-event! :tui event))

(defn- elapsed-label
  [started-at-ms]
  (let [s (quot (- (System/currentTimeMillis) (long started-at-ms)) 1000)]
    (format "%02d:%02d" (quot s 60) (mod s 60))))

(defn- start-ticker!
  [started-at-ms]
  (future
    (while (:recorder @state)
      (publish! {:op :status/set
                 :id :voice/parakeet
                 :text (str "🎙 Recording " (elapsed-label started-at-ms))
                 :level :info})
      (Thread/sleep 1000))))

(defn start-recording!
  [_ctx]
  (if (:recorder @state)
    (publish! {:op :notify :text "Voice recording is already running" :level :warn})
    (let [rec (recorder/start!)
          ticker (start-ticker! (:started-at-ms rec))]
      (reset! state {:recorder rec :ticker ticker})
      (publish! {:op :status/set
                 :id :voice/parakeet
                 :text "🎙 Recording 00:00"
                 :level :info}))))

(defn- transcribe-and-insert!
  [audio-file]
  (future
    (try
      (publish! {:op :status/set :id :voice/parakeet :text "🎙 Transcribing…" :level :info})
      (let [raw (sherpa/transcribe-file! audio-file)]
        (publish! {:op :status/set :id :voice/parakeet :text "🎙 Rewriting…" :level :info})
        (let [rewritten (rewrite/rewrite-transcript! raw)
              text      (if (str/blank? rewritten) raw rewritten)]
          (publish! {:op :input/replace :text text :source :voice/parakeet})
          (publish! {:op :status/clear :id :voice/parakeet})
          (publish! {:op :notify :text "✓ Voice inserted into input" :level :success})))
      (catch Throwable t
        (publish! {:op :status/clear :id :voice/parakeet})
        (publish! {:op :notify
                   :text (str "Voice failed: " (or (ex-message t) t))
                   :level :error})))))

(defn stop-and-transcribe!
  [_ctx]
  (if-let [rec (:recorder @state)]
    (let [audio-file (recorder/stop! rec)]
      (reset! state {:recorder nil :ticker nil})
      (transcribe-and-insert! audio-file))
    (publish! {:op :notify :text "Voice recording is not running" :level :warn})))

(defn cancel-recording!
  [_ctx]
  (when-let [rec (:recorder @state)]
    (recorder/stop! rec))
  (reset! state {:recorder nil :ticker nil})
  (publish! {:op :status/clear :id :voice/parakeet})
  (publish! {:op :notify :text "Voice recording cancelled" :level :info}))

(defn tui-commands
  [_ctx]
  [{:id :voice-parakeet/start
    :label "Voice: Start Recording"
    :run-fn start-recording!}
   {:id :voice-parakeet/stop
    :label "Voice: Stop and Transcribe"
    :run-fn stop-and-transcribe!}
   {:id :voice-parakeet/cancel
    :label "Voice: Cancel Recording"
    :run-fn cancel-recording!}])

(def parakeet-extension
  (vis/extension
    {:ext/namespace 'com.blockether.vis.ext.voice-parakeet.core
     :ext/doc       "Local Parakeet voice input: TUI recording commands, ASR, dynamic cheap/fast rewrite, input insertion."
     :ext/version   "0.1.0"
     :ext/author    "Blockether"
     :ext/owner     "vis"
     :ext/license   "Apache-2.0"
     :ext/kind      "voice"
     :ext/env       [{:name sherpa/model-dir-env
                      :label "Parakeet model directory"
                      :description "Directory containing encoder.int8.onnx, decoder.int8.onnx, joiner.int8.onnx, and tokens.txt. Missing files are downloaded automatically on first use."
                      :required? false}]
     :ext/channel-hooks [{:channel-id :tui
                          :hook-id :voice/parakeet
                          :commands-fn tui-commands}]}))

(vis/register-extension! parakeet-extension)
