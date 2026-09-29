(ns com.blockether.vis.tui.voice-input-test
  (:require [clojure.string :as str]
            [com.blockether.vis.tui.client :as vis]
            [com.blockether.vis.tui.voice-input :as voice-input]
            [com.blockether.vis.tui.voice-recorder :as recorder]
            [lazytest.core :refer [defdescribe expect it]]
            [taoensso.telemere :as tel]))

(defn- await-event
  [events pred]
  (loop [attempts 100]
    (cond (some pred @events) true
          (zero? attempts) false
          :else (do (Thread/sleep 10) (recur (dec attempts))))))

(defn- reset-voice!
  []
  (reset! voice-input/state
    {:recorder nil :ticker nil :transcribing? false :workspace-id nil :session-id nil}))

(defdescribe
  gateway-voice-input-test
  (it
    "captures locally but transcribes through the active gateway session"
    (let [events
          (atom [])

          calls
          (atom [])

          app-db
          (atom {:active-tab-id :tab-1 :session {:id "session-1"}})]

      (reset-voice!)
      (with-redefs [recorder/start!
                    (fn []
                      {:started-at-ms (System/currentTimeMillis)})

                    recorder/stop!
                    (constantly "/tmp/clip.wav")

                    vis/gateway-transcribe-audio!
                    (fn [sid audio-path {:keys [on-progress]}]
                      (swap! calls conj [sid audio-path])
                      (on-progress {"phase" "preparing" "progress" 40})
                      (on-progress {"phase" "transcribing" "progress" 70})
                      "gateway transcript")

                    vis/publish-channel-event!
                    (fn [channel event]
                      (expect (= :tui channel))
                      (swap! events conj event))]

        (voice-input/start-recording! {:app-db app-db})
        (voice-input/stop-and-transcribe! {:app-db app-db})
        (expect (await-event events #(= :input/append (:op %))))
        (expect (= [["session-1" "/tmp/clip.wav"]] @calls))
        (expect (some #(= {:op :input/append
                           :text "gateway transcript"
                           :source :voice/input
                           :workspace-id :tab-1}
                          %)
                      @events))
        (let [texts (mapv :text @events)]
          (expect (some #{"● Preparing voice engine 40%"} texts))
          (expect (some #{"● Transcribing 70%"} texts))))))
  (it "keeps the session that owned recording even after the active tab changes"
      (let [events
            (atom [])

            seen
            (atom nil)

            app-db
            (atom {:active-tab-id :tab-1 :session {:id "session-1"}})]

        (reset-voice!)
        (with-redefs [recorder/start!
                      (fn []
                        {:started-at-ms 0})

                      recorder/stop!
                      (constantly "/tmp/clip.wav")

                      vis/gateway-transcribe-audio!
                      (fn [sid _ _]
                        (reset! seen sid)
                        "hello")

                      vis/publish-channel-event!
                      (fn [_ event]
                        (swap! events conj event))]

          (voice-input/start-recording! {:app-db app-db})
          (reset! app-db {:active-tab-id :tab-2 :session {:id "session-2"}})
          (voice-input/stop-and-transcribe! {:app-db app-db})
          (expect (await-event events #(= :input/append (:op %))))
          (expect (= "session-1" @seen))
          (expect (= :tab-1 (:workspace-id (first (filter #(= :input/append (:op %)) @events))))))))
  (it "does not claim that a silent transcription was appended"
      (let [events (atom [])]
        (reset-voice!)
        (with-redefs [recorder/start! (fn []
                                        {:started-at-ms 0})
                      recorder/stop! (constantly "/tmp/silent.wav")
                      vis/gateway-transcribe-audio! (fn [& _]
                                                      "  ")
                      vis/publish-channel-event! (fn [_ event]
                                                   (swap! events conj event))]

          (voice-input/start-recording! {:workspace-id :tab-1 :session-id "session-1"})
          (voice-input/stop-and-transcribe! {})
          (expect (await-event events #(= "Voice produced no audible text" (:text %))))
          (expect (not-any? #(= :input/append (:op %)) @events)))))
  (it "does not start recording before the tab has a gateway session"
      (let [events
            (atom [])

            starts
            (atom 0)]

        (reset-voice!)
        (with-redefs [recorder/start!
                      (fn []
                        (swap! starts inc))

                      vis/publish-channel-event!
                      (fn [_ event]
                        (swap! events conj event))]

          (voice-input/start-recording! {:app-db (atom {:active-tab-id :tab-1 :session nil})})
          (expect (zero? @starts))
          (expect (nil? (:recorder @voice-input/state)))
          (expect (some #(and (= :notify (:op %))
                              (str/includes? (str (:text %)) "session is ready"))
                        @events)))))
  ;; Regression, issues #172 and #293: a failed microphone must leave a useful
  ;; notification and diagnostic instead of throwing from its own logger.
  (it "logs recorder initialization failures and preserves their remediation"
      (let [events (atom [])]
        (reset-voice!)
        (with-redefs [recorder/start! (fn []
                                        (throw (ex-info "no input device"
                                                        {:type :voice/no-recorder
                                                         :backend :java-sound
                                                         :remediation "Grant microphone access."})))
                      vis/publish-channel-event! (fn [_ event]
                                                   (swap! events conj event))]

          (let [signal (tel/with-signal true
                                        (voice-input/start-recording! {:session-id "session-1"}))]
            (expect (nil? (:recorder @voice-input/state)))
            (expect (= :com.blockether.vis.tui.voice-input/voice-recording-failed (:id signal)))
            (expect (= :error (:level signal)))
            (expect (= :java-sound (get-in signal [:data :backend])))
            (expect (str/includes? (get-in signal [:data :error]) "Grant microphone access."))
            (expect (some #(and (= :notify (:op %))
                                (str/includes? (str (:text %)) "Grant microphone access."))
                          @events))))))
  ;; Regression, issue #293: retain both failed capture backends in the log.
  (it "keeps Java Sound and external failures in the recorder diagnostic"
      (let [events
            (atom [])

            attempts
            [{:backend :sox :error "no device"} {:backend :ffmpeg :error "permission denied"}]]

        (reset-voice!)
        (with-redefs [recorder/start!
                      (fn []
                        (throw (ex-info "No microphone capture backend could start"
                                        {:type :voice/no-recorder
                                         :backend :auto
                                         :java-sound-error "no capture line"
                                         :attempts attempts
                                         :remediation "Allow microphone access."})))

                      vis/publish-channel-event!
                      (fn [_ event]
                        (swap! events conj event))]

          (let [signal (tel/with-signal true
                                        (voice-input/start-recording! {:session-id "session-1"}))]
            (expect (= "no capture line" (get-in signal [:data :java-sound-error])))
            (expect (= attempts (get-in signal [:data :attempts])))
            (expect (some #(and (= :notify (:op %))
                                (str/includes? (str (:text %)) "Allow microphone access."))
                          @events))))))
  ;; Regression, issue #293: transcription errors used the same invalid log call.
  (it "logs transcription failures with their audio context"
      (let [audio-file
            "/tmp/failed-voice.wav"

            failure
            (ex-info "gateway unavailable" {:type :voice/asr})

            signal
            (tel/with-signal true
                             ((ns-resolve 'com.blockether.vis.tui.voice-input
                                          'log-voice-asr-failed!)
                               audio-file
                               failure
                               "gateway unavailable"))]

        (expect (= :com.blockether.vis.tui.voice-input/voice-asr-failed (:id signal)))
        (expect (= :error (:level signal)))
        (expect (= audio-file (get-in signal [:data :audio-file])))
        (expect (= :voice/asr (get-in signal [:data :type]))))))
