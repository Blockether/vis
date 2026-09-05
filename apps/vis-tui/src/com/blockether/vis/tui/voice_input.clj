(ns com.blockether.vis.tui.voice-input
  "TUI microphone capture; transcription runs in the gateway-owned speech engine."
  (:require [clojure.string :as str]
            [com.blockether.vis.tui.client :as vis]
            [com.blockether.vis.tui.voice-output :as output]
            [com.blockether.vis.tui.voice-recorder :as recorder]
            [taoensso.telemere :as tel]))

(defonce state (atom {:recorder nil :ticker nil :transcribing? false :workspace-id nil}))

(defn- publish! [event] (vis/publish-channel-event! :tui event))

(defn- elapsed-label
  [started-at-ms]
  (let [s (quot (- (vis/now-ms) (long started-at-ms)) 1000)]
    (format "%02d:%02d" (quot s 60) (mod s 60))))

(defn- voice-status!
  ([text level] (voice-status! text level nil))
  ([text level ttl-ms]
   (publish! (cond-> {:op :status/set :id :voice/input :text text :level level}
               ttl-ms
               (assoc :ttl-ms ttl-ms)))))

(defn- idle-status! [] (publish! {:op :status/clear :id :voice/input}))

(defn- ctx-workspace-id
  [ctx]
  (or (:workspace-id ctx)
      (some-> ctx
              :app-db
              deref
              :active-tab-id)))

(defn- voice-recording-failed-signal
  [throwable message]
  (let [data (ex-data throwable)]
    {:level :error
     :id ::voice-recording-failed
     :data (cond-> {:error message :type (:type data)}
             (:backend data)
             (assoc :backend (:backend data))

             (seq (:attempts data))
             (assoc :attempts (:attempts data)))}))

(defn- log-voice-recording-failed!
  [throwable message]
  (tel/log! (voice-recording-failed-signal throwable message) message))

(defn- voice-asr-failed-signal
  [audio-file throwable message]
  {:level :error
   :id ::voice-asr-failed
   :data {:audio-file (str audio-file) :error message :type (:type (ex-data throwable))}})

(defn- log-voice-asr-failed!
  [audio-file throwable message]
  (tel/log! (voice-asr-failed-signal audio-file throwable message) message))

(defn- recording-failure-text
  "What the human is told when voice fails. An engine that named a REMEDIATION -
   install espeak-ng, restart Vis after a linker failure - has already done the
   thinking, so it is carried through instead of being flattened into a stack
   message nobody can act on."
  [t]
  (let [message
        (or (ex-message t) (str t))

        remediation
        (:remediation (ex-data t))]

    (if (and remediation (not (str/includes? message remediation)))
      (str message " " remediation)
      message)))

(defn- start-ticker!
  [recorder started-at-ms]
  (future (while (identical? recorder (:recorder @state))
            (voice-status! (str "● Recording " (elapsed-label started-at-ms)) :warn)
            (Thread/sleep 1000))))

(defn start-recording!
  [ctx]
  (cond
    (:transcribing? @state)
    (publish! {:op :notify :text "Voice is still transcribing the previous recording" :level :warn})
    (:recorder @state) (publish!
                         {:op :notify :text "Voice recording is already running" :level :warn})
    :else
    (let [workspace-id (ctx-workspace-id ctx)]
      ;; Reaching for the microphone SILENCES the answer being spoken: a
      ;; machine that keeps talking while the human talks is not holding a
      ;; conversation. No-op when nothing is playing.
      (output/stop!)
      ;; A microphone that refuses (no input device, permission not
      ;; granted) used to throw out of the keymap: the status line kept
      ;; whatever it said and the human was told nothing at all.
      (if-let [rec (try (recorder/start!)
                        (catch Throwable t
                          (let [message (recording-failure-text t)]
                            (reset! state
                              {:recorder nil :ticker nil :transcribing? false :workspace-id nil})
                            (idle-status!)
                            (log-voice-recording-failed! t message)
                            (publish! {:op :notify
                                       :text (str "Voice cannot record: " message)
                                       :level :error})
                            nil)))]
        (let [started-at-ms (vis/now-ms)]
          (reset! state {:recorder rec :ticker nil :transcribing? false :workspace-id workspace-id})
          (let [ticker (start-ticker! rec started-at-ms)]
            (swap! state assoc :ticker ticker))
          (voice-status! "● Recording 00:00" :warn))
        nil))))

(defn- progress-label
  "What the status line says while the gateway-owned engine works."
  [update-map]
  (let [phase
        (keyword (or (:phase update-map) (get update-map "phase") "transcribing"))

        progress
        (or (:progress update-map) (get update-map "progress"))

        pct
        (some-> progress
                long)]

    (case phase
      (:preparing :downloading :unpacking)
      (if (and pct (pos? (long pct)))
        (str "● Preparing voice engine " pct "%")
        "● Preparing voice engine...")

      :transcribing
      (if pct (str "● Transcribing " pct "%") "● Transcribing...")

      :done
      "● Transcribing 100%"

      "● Transcribing...")))

(defn- transcribe-and-insert!
  [audio-file workspace-id]
  (future
    (try (voice-status! "● Sending to gateway..." :info)
         (let [text
               (vis/gateway-transcribe-audio! workspace-id
                                              audio-file
                                              {:on-progress #(voice-status! (progress-label %)
                                                                            :info)})

               blank?
               (or (nil? text) (str/blank? text))]

           (idle-status!)
           (if blank?
             (publish! {:op :notify :text "Voice produced no audible text" :level :warn})
             (do (publish! (cond-> {:op :input/append :text text :source :voice/input}
                             workspace-id
                             (assoc :workspace-id workspace-id)))
                 (publish! {:op :notify :text "✓ Voice appended to input" :level :success}))))
         (catch Throwable t
           (let [message (recording-failure-text t)]
             (log-voice-asr-failed! audio-file t message)
             (voice-status! "○ Voice failed" :error 3000)
             (publish! {:op :notify :text (str "Voice failed: " message) :level :error})))
         (finally (swap! state assoc :transcribing? false :workspace-id nil)))))

(defn stop-and-transcribe!
  [_ctx]
  (cond
    (:transcribing? @state)
    (publish! {:op :notify :text "Voice is still transcribing the previous recording" :level :warn})
    (:recorder @state)
    (let [recording-state
          @state

          rec
          (:recorder recording-state)

          workspace-id
          (:workspace-id recording-state)

          ;; The state is cleared BEFORE anything can throw. A recorder that fails
          ;; to close used to leave `:recorder` set for the rest of the process,
          ;; so every later Ctrl+B answered "already running" and the only cure
          ;; anyone found was restarting Vis.
          audio-file
          (try (recorder/stop! rec)
               (catch Throwable t
                 (reset! state {:recorder nil :ticker nil :transcribing? false :workspace-id nil})
                 (idle-status!)
                 (publish! {:op :notify
                            :text (str "Voice recording failed: " (recording-failure-text t))
                            :level :error})
                 nil))]

      (when audio-file
        (reset! state {:recorder nil :ticker nil :transcribing? true :workspace-id workspace-id})
        (transcribe-and-insert! audio-file workspace-id)))
    :else (publish! {:op :notify :text "Voice recording is not running" :level :warn})))

(defn cancel-recording!
  [_ctx]
  (cond (:transcribing? @state)
        (publish! {:op :notify :text "Speech transcription cannot be cancelled" :level :warn})
        :else (do (when-let [rec (:recorder @state)]
                    (recorder/stop! rec))
                  (reset! state {:recorder nil :ticker nil :transcribing? false :workspace-id nil})
                  (idle-status!)
                  (publish! {:op :notify :text "Voice recording cancelled" :level :info}))))

(defn toggle-recording!
  [ctx]
  (if (:recorder @state) (stop-and-transcribe! ctx) (start-recording! ctx)))
