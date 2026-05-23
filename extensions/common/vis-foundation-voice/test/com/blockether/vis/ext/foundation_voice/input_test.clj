(ns com.blockether.vis.ext.foundation-voice.input-test
  (:require [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.foundation-voice.core :as core]
            [com.blockether.vis.ext.foundation-voice.input :as voice]
            [com.blockether.vis.ext.foundation-voice.recorder :as recorder]
            [com.blockether.vis.ext.foundation-voice.asr :as asr]
            [lazytest.core :refer [defdescribe it expect]]))

(defdescribe voice-input-test
  (it "registers /voice as a declarative slash command (PLAN.md §3, K10)"
    ;; K10 migration: the TUI-only `:tui.slot/commands` contribution
    ;; has been replaced by a declarative `:ext/slash-commands` entry
    ;; whose availability spans BOTH TUI and Telegram. One
    ;; registration, every channel renders the same surface via the
    ;; engine slash dispatch.
    (let [slashes (:ext/slash-commands core/voice-extension)]
      (expect (some #(= "voice" (:slash/name %)) slashes))
      (let [voice-slash (first (filter #(= "voice" (:slash/name %)) slashes))]
        (expect (= #{:channel} (:slash/requires voice-slash)))
        (expect (true?  ((:slash/availability-fn voice-slash) {:channel/id :tui})))
        (expect (true?  ((:slash/availability-fn voice-slash) {:channel/id :telegram})))
        (expect (false? ((:slash/availability-fn voice-slash) {:channel/id :web})))
        (expect (ifn?   (:slash/run-fn voice-slash)))))
    ;; Voice extension no longer contributes a TUI-only commands slot.
    (expect (not-any? #(= :voice/input (:id %))
              (vis/channel-contributions-for :tui :tui.slot/commands))))

  (it "appends Parakeet transcript without rewriting or replacing existing input"
    (let [events (atom [])
          app-db (atom {:active-workspace-id :first})]
      (reset! voice/state {:recorder nil :ticker nil :transcribing? false :workspace-id nil})
      (with-redefs [recorder/start! (fn [] {:started-at-ms (System/currentTimeMillis)})
                    recorder/stop! (fn [_] :audio-file)
                    asr/transcribe-file! (fn [audio-file]
                                           (expect (= :audio-file audio-file))
                                           "Parakeet translation")
                    vis/publish-channel-event! (fn [channel event]
                                                 (expect (= :tui channel))
                                                 (swap! events conj event))]
        (voice/start-recording! {:app-db app-db})
        (reset! app-db {:active-workspace-id :second})
        (voice/stop-and-transcribe! {:app-db app-db})
        (loop [n 50]
          (when (and (pos? n)
                  (not (and (some #(= :input/append (:op %)) @events)
                         (some #(= {:op :status/clear :id :voice/input} %) @events))))
            (Thread/sleep 20)
            (recur (dec n))))
        (expect (some #(= {:op :input/append
                           :text "Parakeet translation"
                           :source :voice/input
                           :workspace-id :first}
                         %)
                  @events))
        (expect (not-any? #(= :input/replace (:op %)) @events))
        (expect (not-any? #(= "● Rewrite..." (:text %)) @events))
        (expect (some #(= {:op :status/clear :id :voice/input} %) @events))
        (expect (not-any? #(= "○ Voice ready" (:text %)) @events))
        (expect (not-any? #(= "● Rewriting..." (:text %)) @events)))))

  (it "starts ticker after recorder is visible in shared state"
    (let [events (atom [])]
      (reset! voice/state {:recorder nil :ticker nil :transcribing? false :workspace-id nil})
      (with-redefs [recorder/start! (fn [] {:started-at-ms 1000})
                    vis/publish-channel-event! (fn [_ event]
                                                 (swap! events conj event))]
        (with-redefs-fn {#'voice/start-ticker! (fn [rec started-at-ms]
                                                 (expect (= {:started-at-ms 1000} rec))
                                                 (expect (= 1000 started-at-ms))
                                                 (expect (identical? rec (:recorder @voice/state)))
                                                 :ticker)}
          (fn []
            (voice/start-recording! {})
            (expect (= :ticker (:ticker @voice/state)))
            (expect (some #(= "● Recording 00:00" (:text %)) @events)))))))

  (it "blocks new recording while previous transcription has not finished"
    (let [starts (atom 0)
          events (atom [])]
      (reset! voice/state {:recorder nil :ticker nil :transcribing? true})
      (with-redefs [recorder/start! (fn []
                                      (swap! starts inc)
                                      {:started-at-ms (System/currentTimeMillis)})
                    vis/publish-channel-event! (fn [_ event]
                                                 (swap! events conj event))]
        (voice/toggle-recording! {})
        (voice/start-recording! {})
        (voice/cancel-recording! {})
        (voice/start-recording! {})
        (expect (zero? @starts))
        (expect (true? (:transcribing? @voice/state)))
        (expect (= 3
                  (count (filter #(= "Voice is still transcribing the previous recording"
                                    (:text %))
                           @events))))
        (expect (some #(= "Voice transcription cannot be cancelled" (:text %))
                  @events)))))

  (it "publishes clean voice failure and logs ASR exceptions"
    (let [events (atom [])
          logs   (atom [])]
      (reset! voice/state {:recorder nil :ticker nil :transcribing? false})
      (with-redefs [recorder/start! (fn [] {:started-at-ms (System/currentTimeMillis)})
                    recorder/stop! (fn [_] "too-short.wav")
                    asr/transcribe-file! (fn [audio-file]
                                           (expect (= "too-short.wav" audio-file))
                                           (throw (ex-info "Voice recording too short - try again"
                                                    {:type :voice-asr/audio-too-short})))
                    vis/publish-channel-event! (fn [channel event]
                                                 (expect (= :tui channel))
                                                 (swap! events conj event))]
        (with-redefs-fn {#'voice/log-voice-asr-failed! (fn [audio-file throwable message]
                                                         (swap! logs conj ((var-get #'voice/voice-asr-failed-signal)
                                                                           audio-file throwable message)))}
          (fn []
            (voice/start-recording! {})
            (voice/stop-and-transcribe! {})
            (loop [n 50]
              (when (and (pos? n)
                      (not-any? #(= "Voice failed: Voice recording too short - try again" (:text %)) @events))
                (Thread/sleep 20)
                (recur (dec n))))
            (expect (some #(= {:op :notify
                               :text "Voice failed: Voice recording too short - try again"
                               :level :error}
                             %)
                      @events))
            (expect (some #(and (= "○ Voice failed" (:text %))
                             (= 3000 (:ttl-ms %)))
                      @events))
            (expect (= false (:transcribing? @voice/state)))
            (expect (some #(and (= :error (:level %))
                             (= ::voice/voice-asr-failed (:id %))
                             (= "too-short.wav" (get-in % [:data :audio-file]))
                             (= "Voice recording too short - try again" (get-in % [:data :error]))
                             (= :voice-asr/audio-too-short (get-in % [:data :type])))
                      @logs))))))))
