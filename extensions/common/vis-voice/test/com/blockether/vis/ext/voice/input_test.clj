(ns com.blockether.vis.ext.voice.input-test
  (:require [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.voice.core :as core]
            [com.blockether.vis.ext.voice.input :as voice]
            [com.blockether.vis.ext.voice.recorder :as recorder]
            [com.blockether.vis.ext.voice.rewrite :as rewrite]
            [com.blockether.vis.ext.voice.asr :as asr]
            [lazytest.core :refer [defdescribe it expect]]))

(defdescribe voice-input-test
  (it "registers a hidden direct-toggle TUI channel hook"
    (let [hooks (vis/channel-hooks-for :tui)]
      (expect (some #(= :voice/input (:hook-id %)) hooks))
      (expect (some #(= :voice/input (:hook-id %)) (:ext/channel-hooks core/voice-extension)))
      (let [commands (voice/tui-commands {})]
        (expect (= [:voice/toggle-recording]
                  (mapv :id commands)))
        (expect (= [false] (mapv :palette? commands)))
        (expect (every? ifn? (map :run-fn commands))))))

  (it "appends rewritten transcript without replacing existing input"
    (let [events (atom [])
          app-db (atom {:active-workspace-id :first})]
      (reset! voice/state {:recorder nil :ticker nil :transcribing? false :workspace-id nil})
      (with-redefs [recorder/start! (fn [] {:started-at-ms (System/currentTimeMillis)})
                    recorder/stop! (fn [_] :audio-file)
                    asr/transcribe-file! (fn [audio-file]
                                           (expect (= :audio-file audio-file))
                                           "raw transcript")
                    rewrite/rewrite-transcript! (fn [raw]
                                                  (expect (= "raw transcript" raw))
                                                  "Rewrite")
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
                           :text "Rewrite"
                           :source :voice/input
                           :workspace-id :first}
                         %)
                  @events))
        (expect (not-any? #(= :input/replace (:op %)) @events))
        (expect (some #(= "● Rewrite..." (:text %)) @events))
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
                    rewrite/rewrite-transcript! (fn [_]
                                                  (expect false)
                                                  "")
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
