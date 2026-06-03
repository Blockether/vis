(ns com.blockether.vis.ext.foundation-voice.input
  "TUI voice input backed by local Parakeet-class ASR."
  (:require [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.foundation-voice.recorder :as recorder]
            [com.blockether.vis.ext.foundation-voice.asr :as asr]
            [taoensso.telemere :as tel]))

(defonce state
  (atom {:recorder nil
         :ticker nil
         :transcribing? false
         :workspace-id nil}))

(defn- publish!
  [event]
  (vis/publish-channel-event! :tui event))

(defn- elapsed-label
  [started-at-ms]
  (let [s (quot (- (System/currentTimeMillis) (long started-at-ms)) 1000)]
    (format "%02d:%02d" (quot s 60) (mod s 60))))

(defn- voice-status!
  ([text level]
   (voice-status! text level nil))
  ([text level ttl-ms]
   (publish! (cond-> {:op :status/set
                      :id :voice/input
                      :text text
                      :level level}
               ttl-ms (assoc :ttl-ms ttl-ms)))))

(defn- idle-status! []
  (publish! {:op :status/clear
             :id :voice/input}))

(defn- ctx-workspace-id
  [ctx]
  (or (:workspace-id ctx)
    (some-> ctx :app-db deref :active-tab-id)))

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
  [recorder started-at-ms]
  (future
    (while (identical? recorder (:recorder @state))
      (voice-status! (str "● Recording " (elapsed-label started-at-ms)) :warn)
      (Thread/sleep 1000))))

(defn start-recording!
  [ctx]
  (cond
    (:transcribing? @state)
    (publish! {:op :notify
               :text "Voice is still transcribing the previous recording"
               :level :warn})

    (:recorder @state)
    (publish! {:op :notify :text "Voice recording is already running" :level :warn})

    :else
    (let [workspace-id (ctx-workspace-id ctx)
          rec          (recorder/start!)]
      (reset! state {:recorder rec
                     :ticker nil
                     :transcribing? false
                     :workspace-id workspace-id})
      (let [ticker (start-ticker! rec (:started-at-ms rec))]
        (swap! state assoc :ticker ticker))
      (voice-status! "● Recording 00:00" :warn))))

(defn- transcribe-and-insert!
  [audio-file workspace-id]
  (future
    (try
      (voice-status! "● Transcribing..." :info)
      (let [text  (asr/transcribe-file! audio-file)
            blank? (or (nil? text) (str/blank? text))]
        (idle-status!)
        (if blank?
          ;; Empty ASR result was the silent failure mode that made
          ;; Ctrl+B feel broken: we still fired `:input/append` with
          ;; an empty string AND a \"✓ Voice appended to input\"
          ;; notification, so the user saw the success toast but
          ;; nothing changed in the editor. Surface the empty case
          ;; explicitly instead of pretending we appended text.
          (publish! {:op :notify
                     :text "Voice produced no audible text"
                     :level :warn})
          (do
            (publish! (cond-> {:op :input/append
                               :text text
                               :source :voice/input}
                        workspace-id (assoc :workspace-id workspace-id)))
            (publish! {:op :notify :text "✓ Voice appended to input" :level :success}))))
      (catch Throwable t
        (let [message (or (ex-message t) (str t))]
          (log-voice-asr-failed! audio-file t message)
          (voice-status! "○ Voice failed" :error 3000)
          (publish! {:op :notify
                     :text (str "Voice failed: " message)
                     :level :error})))
      (finally
        (swap! state assoc :transcribing? false :workspace-id nil)))))

(defn stop-and-transcribe!
  [_ctx]
  (cond
    (:transcribing? @state)
    (publish! {:op :notify
               :text "Voice is still transcribing the previous recording"
               :level :warn})

    (:recorder @state)
    (let [recording-state @state
          rec             (:recorder recording-state)
          workspace-id    (:workspace-id recording-state)
          audio-file      (recorder/stop! rec)]
      (reset! state {:recorder nil
                     :ticker nil
                     :transcribing? true
                     :workspace-id workspace-id})
      (transcribe-and-insert! audio-file workspace-id))

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
      (reset! state {:recorder nil :ticker nil :transcribing? false :workspace-id nil})
      (idle-status!)
      (publish! {:op :notify :text "Voice recording cancelled" :level :info}))))

(defn toggle-recording!
  [ctx]
  (if (:recorder @state)
    (stop-and-transcribe! ctx)
    (start-recording! ctx)))

(defn tui-commands
  [_ctx]
  [{:id :voice/toggle-recording
    :label "Voice: Toggle Recording (Ctrl+B)"
    :palette? false
    :run-fn toggle-recording!}])

