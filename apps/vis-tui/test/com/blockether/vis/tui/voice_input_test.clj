(ns com.blockether.vis.tui.voice-input-test
  (:require [clojure.string :as str]
            [com.blockether.vis.tui.client :as vis]
            [com.blockether.vis.tui.voice-input :as voice-input]
            [com.blockether.vis.tui.voice-recorder :as recorder]
            [lazytest.core :refer [defdescribe expect it]]))

(defn- await-event
  [events pred]
  (loop [attempts 100]
    (cond (some pred @events) true
          (zero? attempts) false
          :else (do (Thread/sleep 10) (recur (dec attempts))))))

(defn- reset-voice!
  []
  (reset! voice-input/state {:recorder nil :ticker nil :transcribing? false :workspace-id nil}))

(defdescribe
  gateway-voice-input-test
  (it
    "captures locally but transcribes through the active gateway session"
    (let [events
          (atom [])

          calls
          (atom [])

          app-db
          (atom {:active-tab-id "session-1"})]

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
                           :workspace-id "session-1"}
                          %)
                      @events))
        (let [texts (mapv :text @events)]
          (expect (some #{"● Preparing voice engine 40%"} texts))
          (expect (some #{"● Transcribing 70%"} texts))))))
  (it
    "keeps the session that owned recording even after the active tab changes"
    (let [events
          (atom [])

          seen
          (atom nil)

          app-db
          (atom {:active-tab-id "first"})]

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
        (reset! app-db {:active-tab-id "second"})
        (voice-input/stop-and-transcribe! {:app-db app-db})
        (expect (await-event events #(= :input/append (:op %))))
        (expect (= "first" @seen))
        (expect (= "first" (:workspace-id (first (filter #(= :input/append (:op %)) @events))))))))
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

          (voice-input/start-recording! {:workspace-id "session-1"})
          (voice-input/stop-and-transcribe! {})
          (expect (await-event events #(= "Voice produced no audible text" (:text %))))
          (expect (not-any? #(= :input/append (:op %)) @events)))))
  (it "clears recorder state and preserves actionable microphone errors"
      (let [events (atom [])]
        (reset-voice!)
        (with-redefs [recorder/start! (fn []
                                        (throw (ex-info "no input device"
                                                        {:remediation "Grant microphone access."})))
                      vis/publish-channel-event! (fn [_ event]
                                                   (swap! events conj event))]

          (voice-input/start-recording! {})
          (expect (nil? (:recorder @voice-input/state)))
          (expect (some #(str/includes? (str (:text %)) "Grant microphone access.") @events))))))
