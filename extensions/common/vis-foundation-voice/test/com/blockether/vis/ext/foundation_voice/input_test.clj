(ns com.blockether.vis.ext.foundation-voice.input-test
  (:require [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.foundation-voice.core :as core]
            [com.blockether.vis.ext.foundation-voice.input :as voice]
            [com.blockether.vis.ext.foundation-voice.recorder :as recorder]
            [com.blockether.vis.ext.foundation-voice.asr :as asr]
            [lazytest.core :refer [defdescribe it expect]]))

(defdescribe
  voice-input-test
  (it "registers /voice as a declarative slash command"
      ;; The TUI-only `:tui.slot/commands` contribution has been
      ;; replaced by a declarative `:ext/slash-commands` entry
      ;; whose availability is TUI-scoped. One
      ;; registration, every channel renders the same surface via the
      ;; engine slash dispatch.
      (let [slashes (:ext/slash-commands core/voice-extension)]
        (expect (some #(= "voice" (:slash/name %)) slashes))
        (let [voice-slash (first (filter #(= "voice" (:slash/name %)) slashes))]
          ;; voice-ext /voice is TUI-only. Per-channel availability
          ;; partitioning is what lets extensions own the same
          ;; slash path without colliding at register time.
          (expect (= #{:channel} (:slash/requires voice-slash)))
          (expect (true? ((:slash/availability-fn voice-slash) {:channel/id :tui})))
          (expect (false? ((:slash/availability-fn voice-slash) {:channel/id :cli})))
          (expect (false? ((:slash/availability-fn voice-slash) {:channel/id :api})))
          (expect (ifn? (:slash/run-fn voice-slash)))))
      ;; Voice extension no longer contributes a TUI-only commands slot.
      (expect (not-any? #(= :voice/input (:id %))
                        (vis/channel-contributions-for :tui :tui.slot/commands))))
  (it
    "appends Parakeet transcript without rewriting or replacing existing input"
    (let [events
          (atom [])

          app-db
          (atom {:active-tab-id :first})]

      (reset! voice/state {:recorder nil :ticker nil :transcribing? false :workspace-id nil})
      (with-redefs [recorder/start!
                    (fn []
                      {:started-at-ms (System/currentTimeMillis)})

                    recorder/stop!
                    (fn [_]
                      :audio-file)

                    asr/transcribe-file!
                    (fn [audio-file]
                      (expect (= :audio-file audio-file))
                      "Parakeet translation")

                    vis/publish-channel-event!
                    (fn [channel event]
                      (expect (= :tui channel))
                      (swap! events conj event))]

        (voice/start-recording! {:app-db app-db})
        (reset! app-db {:active-tab-id :second})
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
  (it "cleans ASR filler sounds and adjacent stutters before appending"
      (expect (= "I want to add this to the transcript"
                 (voice/clean-transcript
                   "uh I I want to you know add add this to to the transcript")))
      (expect (= "we should remove all of those words"
                 (voice/clean-transcript
                   "um we should remove all of those all of those uh uh words")))
      (expect (= "keep meaningful repeated non-adjacent words because context matters"
                 (voice/clean-transcript
                   "keep meaningful repeated non-adjacent words because context matters"))))
  (it "strips stutter punctuation so I, I, I collapses to I (not I,)"
      ;; Parakeet emits punctuation between stuttered tokens; the raw first
      ;; token used to leak its trailing comma/ellipsis. Regression for the
      ;; I I I I I I / I, I, I class of stutters.
      (expect (= "I want to fix it" (voice/clean-transcript "I, I, I, I want to fix it")))
      (expect (= "I" (voice/clean-transcript "I I I I I I")))
      (expect (= "I want" (voice/clean-transcript "I... I... I want")))
      (expect (= "So I think we should" (voice/clean-transcript "So I, I, I think we should")))
      ;; case and internal punctuation (apostrophe) preserved
      (expect (= "don't worry" (voice/clean-transcript "don't don't worry"))))
  (it "appends the cleaned Parakeet transcript"
      (let [events (atom [])]
        (reset! voice/state {:recorder nil :ticker nil :transcribing? false :workspace-id nil})
        (with-redefs [recorder/start! (fn []
                                        {:started-at-ms (System/currentTimeMillis)})
                      recorder/stop! (fn [_]
                                       :audio-file)
                      asr/transcribe-file! (fn [_]
                                             "uh add add this this to to the prompt")
                      vis/publish-channel-event! (fn [_ event]
                                                   (swap! events conj event))]

          (voice/start-recording! {})
          (voice/stop-and-transcribe! {})
          (loop [n 50]
            (when (and (pos? n) (not (some #(= :input/append (:op %)) @events)))
              (Thread/sleep 20)
              (recur (dec n))))
          (expect (some #(= {:op :input/append :text "add this to the prompt" :source :voice/input}
                            %)
                        @events)))))
  (it "starts ticker after recorder is visible in shared state"
      (let [events (atom [])]
        (reset! voice/state {:recorder nil :ticker nil :transcribing? false :workspace-id nil})
        (with-redefs [recorder/start! (fn []
                                        {:started-at-ms 1000})
                      vis/publish-channel-event! (fn [_ event]
                                                   (swap! events conj event))]

          (with-redefs-fn {#'voice/start-ticker! (fn [rec started-at-ms]
                                                   (expect (= {:started-at-ms 1000} rec))
                                                   (expect (= 1000 started-at-ms))
                                                   (expect (identical? rec
                                                                       (:recorder @voice/state)))
                                                   :ticker)}
            (fn []
              (voice/start-recording! {})
              (expect (= :ticker (:ticker @voice/state)))
              (expect (some #(= "● Recording 00:00" (:text %)) @events)))))))
  (it "blocks new recording while previous transcription has not finished"
      (let [starts
            (atom 0)

            events
            (atom [])]

        (reset! voice/state {:recorder nil :ticker nil :transcribing? true})
        (with-redefs [recorder/start!
                      (fn []
                        (swap! starts inc)
                        {:started-at-ms (System/currentTimeMillis)})

                      vis/publish-channel-event!
                      (fn [_ event]
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
          (expect (some #(= "Voice transcription cannot be cancelled" (:text %)) @events)))))
  (it
    "empty ASR transcription surfaces a clear no-audible-text notification (NOT a false `✓ appended` success)"
    ;; Regression: when the ASR returned an empty / blank string
    ;; (silent recording, mic muted, too-quiet input), we still
    ;; fired `:input/append` with `\"\"` AND published a
    ;; \"✓ Voice appended to input\" success notification — so the
    ;; toast lied: the editor never changed. User experience:
    ;; \"Ctrl+B not adding text\" with no actionable feedback. See
    ;; conversation 11d4f817-fbd1-43ab-a6b4-052c8557af0a / \"why
    ;; is ctrl+b not adding to the text\".
    ;;
    ;; Fix: when transcription is blank, skip `:input/append` and
    ;; publish exactly the terse `:warn` notify the user should see.
    (let [events (atom [])]
      (reset! voice/state {:recorder nil :ticker nil :transcribing? false :workspace-id nil})
      (with-redefs [recorder/start! (fn []
                                      {:started-at-ms (System/currentTimeMillis)})
                    recorder/stop! (fn [_]
                                     :silent.wav)
                    asr/transcribe-file! (fn [_]
                                           "")
                    vis/publish-channel-event! (fn [channel event]
                                                 (expect (= :tui channel))
                                                 (swap! events conj event))]

        (voice/start-recording! {})
        (voice/stop-and-transcribe! {})
        (loop [n 50]
          (when (and (pos? n)
                     (not (some #(and (= :notify (:op %))
                                      (= :warn (:level %))
                                      (str/includes? (str (:text %)) "no audible text"))
                                @events)))
            (Thread/sleep 20)
            (recur (dec n))))
        ;; No `:input/append` should have fired — the editor must not
        ;; receive a phantom empty-string append.
        (expect (not-any? #(= :input/append (:op %)) @events))
        ;; The fake “appended” toast must NOT fire on empty text.
        (expect (not-any? #(and (= :notify (:op %))
                                (clojure.string/includes? (str (:text %)) "✓ Voice appended"))
                          @events))
        ;; A clear no-audible-text warning DID fire, with no parenthetical advice.
        (expect (some #(and (= :notify (:op %))
                            (= :warn (:level %))
                            (= "Voice produced no audible text" (:text %)))
                      @events))
        (expect (= false (:transcribing? @voice/state))))))
  (it "blank-only (whitespace) ASR transcription is treated the same as empty (no false append)"
      ;; Some ASR runs return a whitespace string instead of empty.
      ;; Treat both identically so the user sees actionable feedback.
      (let [events (atom [])]
        (reset! voice/state {:recorder nil :ticker nil :transcribing? false :workspace-id nil})
        (with-redefs [recorder/start! (fn []
                                        {:started-at-ms (System/currentTimeMillis)})
                      recorder/stop! (fn [_]
                                       :silent.wav)
                      asr/transcribe-file! (fn [_]
                                             "   \t\n  ")
                      vis/publish-channel-event! (fn [_ event]
                                                   (swap! events conj event))]

          (voice/start-recording! {})
          (voice/stop-and-transcribe! {})
          (loop [n 50]
            (when (and (pos? n)
                       (not (some #(and (= :notify (:op %))
                                        (clojure.string/includes? (str (:text %))
                                                                  "no audible text"))
                                  @events)))
              (Thread/sleep 20)
              (recur (dec n))))
          (expect (not-any? #(= :input/append (:op %)) @events))
          (expect (some #(= "Voice produced no audible text" (:text %)) @events)))))
  (it
    "publishes clean voice failure and logs ASR exceptions"
    (let [events
          (atom [])

          logs
          (atom [])]

      (reset! voice/state {:recorder nil :ticker nil :transcribing? false})
      (with-redefs [recorder/start!
                    (fn []
                      {:started-at-ms (System/currentTimeMillis)})

                    recorder/stop!
                    (fn [_]
                      "too-short.wav")

                    asr/transcribe-file!
                    (fn [audio-file]
                      (expect (= "too-short.wav" audio-file))
                      (throw (ex-info "Voice recording too short - try again"
                                      {:type :voice-asr/audio-too-short})))

                    vis/publish-channel-event!
                    (fn [channel event]
                      (expect (= :tui channel))
                      (swap! events conj event))]

        (with-redefs-fn
          {#'voice/log-voice-asr-failed!
           (fn [audio-file throwable message]
             (swap! logs conj
               ((var-get #'voice/voice-asr-failed-signal) audio-file throwable message)))}
          (fn []
            (voice/start-recording! {})
            (voice/stop-and-transcribe! {})
            (loop [n 50]
              (when (and (pos? n)
                         (not-any? #(= "Voice failed: Voice recording too short - try again"
                                       (:text %))
                                   @events))
                (Thread/sleep 20)
                (recur (dec n))))
            (expect (some #(= {:op :notify
                               :text "Voice failed: Voice recording too short - try again"
                               :level :error}
                              %)
                          @events))
            (expect (some #(and (= "○ Voice failed" (:text %)) (= 3000 (:ttl-ms %))) @events))
            (expect (= false (:transcribing? @voice/state)))
            (expect (some #(and (= :error (:level %))
                                (= ::voice/voice-asr-failed (:id %))
                                (= "too-short.wav" (get-in % [:data :audio-file]))
                                (= "Voice recording too short - try again"
                                   (get-in % [:data :error]))
                                (= :voice-asr/audio-too-short (get-in % [:data :type])))
                          @logs))))))))
