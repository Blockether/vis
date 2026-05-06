(ns com.blockether.vis.ext.voice-parakeet.core
  "TUI voice-input extension backed by local Parakeet-class ASR."
  (:require [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.voice-parakeet.recorder :as recorder]
            [com.blockether.vis.ext.voice-parakeet.rewrite :as rewrite]
            [com.blockether.vis.ext.voice-parakeet.sherpa :as sherpa]
            [taoensso.telemere :as tel]))

(defonce state
  (atom {:recorder nil
         :ticker nil
         :transcribing? false}))

(defn- publish!
  [event]
  (vis/publish-channel-event! :tui event))

(defn- elapsed-label
  [started-at-ms]
  (let [s (quot (- (System/currentTimeMillis) (long started-at-ms)) 1000)]
    (format "%02d:%02d" (quot s 60) (mod s 60))))

(defn- voice-status!
  [text level]
  (publish! {:op :status/set
             :id :voice/parakeet
             :text text
             :level level}))

(defn- idle-status! []
  (voice-status! "○ Voice ready" :success))

(defn- voice-asr-failed-signal
  [audio-file throwable message]
  {:level :error
   :id ::voice-asr-failed
   :data {:audio-file (str audio-file)
          :error message
          :type (:type (ex-data throwable))}})

(defn- log-voice-asr-failed!
  [audio-file throwable message]
  (tel/log! (voice-asr-failed-signal audio-file throwable message)))

(defn- start-ticker!
  [started-at-ms]
  (future
    (while (:recorder @state)
      (voice-status! (str "● Recording " (elapsed-label started-at-ms)) :warn)
      (Thread/sleep 1000))))

(defn start-recording!
  [_ctx]
  (cond
    (:transcribing? @state)
    (publish! {:op :notify
               :text "Voice is still transcribing the previous recording"
               :level :warn})

    (:recorder @state)
    (publish! {:op :notify :text "Voice recording is already running" :level :warn})

    :else
    (let [rec (recorder/start!)
          ticker (start-ticker! (:started-at-ms rec))]
      (reset! state {:recorder rec :ticker ticker :transcribing? false})
      (voice-status! "● Recording 00:00" :warn))))

(defn- transcribe-and-insert!
  [audio-file]
  (future
    (try
      (voice-status! "● Transcribing…" :info)
      (let [raw (sherpa/transcribe-file! audio-file)]
        (voice-status! "● Rewrite…" :info)
        (let [rewritten (rewrite/rewrite-transcript! raw)
              text      (if (str/blank? rewritten) raw rewritten)]
          (publish! {:op :input/append :text text :source :voice/parakeet})
          (idle-status!)
          (publish! {:op :notify :text "✓ Voice appended to input" :level :success})))
      (catch Throwable t
        (let [message (or (ex-message t) (str t))]
          (log-voice-asr-failed! audio-file t message)
          (voice-status! "○ Voice failed" :error)
          (publish! {:op :notify
                     :text (str "Voice failed: " message)
                     :level :error})))
      (finally
        (swap! state assoc :transcribing? false)))))

(defn stop-and-transcribe!
  [_ctx]
  (cond
    (:transcribing? @state)
    (publish! {:op :notify
               :text "Voice is still transcribing the previous recording"
               :level :warn})

    (:recorder @state)
    (let [rec (:recorder @state)
          audio-file (recorder/stop! rec)]
      (reset! state {:recorder nil :ticker nil :transcribing? true})
      (transcribe-and-insert! audio-file))

    :else
    (publish! {:op :notify :text "Voice recording is not running" :level :warn})))

(defn cancel-recording!
  [_ctx]
  (cond
    (:transcribing? @state)
    (publish! {:op :notify
               :text "Voice transcription cannot be cancelled"
               :level :warn})

    :else
    (do
      (when-let [rec (:recorder @state)]
        (recorder/stop! rec))
      (reset! state {:recorder nil :ticker nil :transcribing? false})
      (idle-status!)
      (publish! {:op :notify :text "Voice recording cancelled" :level :info}))))

(defn toggle-recording!
  [ctx]
  (if (:recorder @state)
    (stop-and-transcribe! ctx)
    (start-recording! ctx)))

(defn tui-commands
  [_ctx]
  [{:id :voice-parakeet/toggle
    :label "Voice: Toggle Recording (Ctrl+B)"
    :palette? false
    :run-fn toggle-recording!}])

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
