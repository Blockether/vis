(ns com.blockether.vis.internal.gateway.voice-test
  (:require [clojure.string :as str]
            [com.blockether.vis.contract.voice :as contract]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.internal.gateway.event-check :as event-check]
            [com.blockether.vis.internal.gateway.server.voice :as media]
            [com.blockether.vis.internal.gateway.state :as state]
            [com.blockether.vis.internal.gateway.voice :as voice]
            [lazytest.core :refer [defdescribe describe expect it]]
            [ring.websocket :as ws])
  (:import [java.nio ByteBuffer]
           [java.util UUID]
           [java.util.concurrent ArrayBlockingQueue]))

(set! *warn-on-reflection* true)

(def ^:private session-id (UUID/fromString "31d34262-789a-43bb-82e3-6e8531b32000"))

(def ^:private start-request
  {:schema 1
   :client-id "31d34262-789a-43bb-82e3-6e8531b32001"
   :language "en"
   :privacy "local_speech"
   :muted false})

(defn- await!
  [predicate]
  (let [deadline (+ (System/nanoTime) 3000000000)]
    (loop []

      (cond (predicate) true
            (> (System/nanoTime) deadline) false
            :else (do (Thread/sleep 5) (recur))))))

(defn- fixture
  [options f]
  (let [events
        (atom [])

        durable
        (atom [])

        admitted
        (atom [])

        cancelled
        (atom [])

        subscriber
        (atom nil)

        opened
        (atom 0)

        api
        {'capabilities (constantly {:available true})
         'prepare! (get options :prepare (constantly {:ready true}))
         'open-asr! (fn [_]
                      (swap! opened inc)
                      (atom []))
         'feed-asr! (get options
                         :feed
                         (fn [handle samples offset]
                           (swap! handle conj [offset (alength ^floats samples)])
                           {:text "Provisional text" :revision 1}))
         'finish-asr! (get options
                           :finish
                           (fn [_]
                             {:text "Explain the test." :revision 2}))
         'abort-asr! (constantly {:state :acknowledged})
         'asr-status (constantly {:state :open})
         'synthesize-stream! (get options
                                  :synthesize
                                  (fn [{:keys [on-pcm]}]
                                    (on-pcm (float-array 960) 24000)
                                    {:state :finished}))}]

    (binding [voice/*engine-api* api]
      (with-redefs [state/subscribe! (fn [_ _ sink _]
                                       (reset! subscriber sink)
                                       [])
                    state/unsubscribe! (fn [_ _]
                                         (reset! subscriber nil))
                    state/append-event! (fn [_ type payload]
                                          (event-check/check! type (wire/canonical payload))
                                          (swap! durable conj [type payload]))
                    state/submit-turn! (fn [_ request]
                                         (swap! admitted conj request)
                                         (if-let [submit (:submit options)]
                                           (submit request)
                                           {:turn {:turn_id (str (UUID/randomUUID))
                                                   :status (get options :turn-status "running")}}))
                    state/current-turn-id (constantly (:current-turn-id options))
                    state/update-queued-turn! (get options
                                                   :update-queued
                                                   (fn [_ tid _]
                                                     {:turn {:turn_id tid :status "queued"}}))
                    state/cancel-turn! (fn [_ turn-id _]
                                         (swap! cancelled conj turn-id)
                                         {})
                    ws/send (fn [_ value success _]
                              (swap! events conj
                                (if (instance? ByteBuffer value)
                                  (contract/decode-frame value)
                                  (wire/->engine (wire/parse-json value))))
                              (success))
                    ws/close (fn [_ _ _]
                               nil)]

        (try (f {:events events
                 :durable durable
                 :admitted admitted
                 :cancelled cancelled
                 :subscriber subscriber
                 :opened opened
                 :start #(voice/start! session-id start-request "https://gateway.example.com")})
             (finally (voice/shutdown!)))))))

(defn- open!
  [start]
  (let [result
        (start)

        {:keys [voice-session-id ticket]}
        (:data result)

        runtime
        (voice/claim! session-id voice-session-id ticket "https://gateway.example.com")]

    (expect (= 201 (:status result)))
    (expect (some? runtime))
    (voice/open-socket! runtime :fake-socket)
    runtime))

(defn- control! [runtime message] (voice/message! runtime (wire/json-str message)))

(defdescribe
  voice-steering-test
  ;; Regression for the hands-free handoff: Send now lost the running turn's speech ownership.
  (it
    "speaks the steered answer without replaying interrupted progress or cancelling work"
    (fixture
      {:turn-status "queued"}
      (fn [{:keys [start events subscriber cancelled]}]
        (let [runtime (open! start)]
          (expect (await! #(some (fn [event]
                                   (= "session.ready" (:type event)))
                                 @events)))
          (control! runtime {:type "utterance.start" :capture-generation 0 :utterance-id 0})
          (control! runtime {:type "utterance.end" :capture-generation 0 :utterance-id 0})
          (expect (await! #(= 1 (count (:receipts (voice/snapshot runtime))))))
          (let [first-turn (:turn-id (first (:receipts (voice/snapshot runtime))))]
            (@subscriber {"type" "turn.started" "turn_id" first-turn})
            (@subscriber
             {"type" "speech.committed"
              "turn_id" first-turn
              "iteration" 1
              "source" "presentation:progress"
              "text" "I'll check."})
            (expect (await! #(some (fn [event]
                                     (= "speech.committed" (:type event)))
                                   @events)))
            (control! runtime {:type "utterance.start" :capture-generation 0 :utterance-id 1})
            (control! runtime {:type "utterance.end" :capture-generation 0 :utterance-id 1})
            (expect (await! #(= 2 (count (:receipts (voice/snapshot runtime))))))
            (let [queued-turn (:turn-id (first (filter #(= 1 (:utterance-id %))
                                                       (:receipts (voice/snapshot runtime)))))]
              (@subscriber
               {"type" "turn.input"
                "turn_id" first-turn
                "iteration" 2
                "messages" [{"queued_turn_id" queued-turn "request" "Also check this."}]})
              (@subscriber
               {"type" "speech.committed"
                "turn_id" first-turn
                "iteration" 1
                "source" "presentation:late"
                "text" "Old output."})
              (@subscriber
               {"type" "speech.committed"
                "turn_id" first-turn
                "iteration" 2
                "source" "presentation:answer"
                "text" "I agree."})
              (@subscriber
               {"type" "turn.completed"
                "turn_id" first-turn
                "content"
                [{"id" "progress" "type" "conversation" "phase" "progress" "text" "I'll check."}
                 {"id" "answer" "type" "conversation" "phase" "answer" "text" "I agree."}]})
              (expect (await! #(= "terminal" (:agent (:state (voice/snapshot runtime))))))
              (expect (= ["I'll check." "I agree."]
                         (mapv :text (filter #(= "speech.committed" (:type %)) @events))))
              (expect (empty? @cancelled))))))))
  (it "marks only the new follow-up for the next step of an owned running voice task"
      (let [running-id
            "31d34262-789a-43bb-82e3-6e8531b32010"

            queued-id
            "31d34262-789a-43bb-82e3-6e8531b32011"

            submitted
            (atom 0)

            marked
            (atom [])]

        (fixture
          {:submit (fn [_]
                     (if (= 1 (swap! submitted inc))
                       {:turn {:turn_id running-id :status "streaming"}}
                       {:turn {:turn_id queued-id :status "queued"}}))
           :current-turn-id running-id
           :update-queued (fn [sid tid patch]
                            (swap! marked conj [sid tid patch])
                            {:turn {:turn_id tid :status "queued"}})}
          (fn [{:keys [start events]}]
            (let [runtime (open! start)]
              (expect (await! #(some (fn [event]
                                       (= "session.ready" (:type event)))
                                     @events)))
              (doseq [id (range 2)]
                (control! runtime {:type "utterance.start" :capture-generation 0 :utterance-id id})
                (control! runtime {:type "utterance.end" :capture-generation 0 :utterance-id id})
                (expect (await! #(= (inc id) (count (:receipts (voice/snapshot runtime)))))))
              (expect (= [[session-id queued-id {:deliver "next_iteration"}]] @marked))))))))

(defdescribe
  voice-steering-boundary-test
  (it "does not automatically steer an unrelated running task"
      (let [submitted
            (atom 0)

            marked
            (atom [])]

        (fixture
          {:current-turn-id "another-task"
           :submit (fn [_]
                     {:turn {:turn_id (str (UUID/randomUUID))
                             :status (if (= 1 (swap! submitted inc)) "streaming" "queued")}})
           :update-queued (fn [& args]
                            (swap! marked conj args))}
          (fn [{:keys [start events]}]
            (let [runtime (open! start)]
              (expect (await! #(some (fn [event]
                                       (= "session.ready" (:type event)))
                                     @events)))
              (doseq [id (range 2)]
                (control! runtime {:type "utterance.start" :capture-generation 0 :utterance-id id})
                (control! runtime {:type "utterance.end" :capture-generation 0 :utterance-id id})
                (expect (await! #(= (inc id) (count (:receipts (voice/snapshot runtime)))))))
              (expect (empty? @marked))))))))

(defdescribe media-error-diagnostics-test
             (it "keeps causes and stack frames without exception data or credentials"
                 (let [ticket
                       "example-voice-ticket"

                       error
                       (ex-info "Outer failure"
                                {:audio "private-audio-marker"}
                                (ex-info (str "Missing method " ticket)
                                         {:secret "private-data-marker"}))

                       details
                       (#'media/media-error-details error [ticket])]

                   (expect (str/includes? details "Missing method"))
                   (expect (str/includes? details ":trace"))
                   (expect (str/includes? details "[REDACTED]"))
                   (expect (not (str/includes? details ticket)))
                   (expect (not (str/includes? details "private-audio-marker")))
                   (expect (not (str/includes? details "private-data-marker")))))
             (it "closes media even when diagnostic logging fails"
                 (let [closed
                       (atom nil)

                       response
                       (#'media/media-handler {::media/runtime :runtime})

                       on-error
                       (get-in response [:ring.websocket/listener :on-error])]

                   (with-redefs-fn {#'media/log-media-error! (fn [& _]
                                                               (throw (ex-info "Logger failed" {})))
                                    #'voice/close! (fn [runtime reason]
                                                     (reset! closed [runtime reason]))}
                     #(try (on-error nil (ex-info "Media failed" {})) (catch Exception _ nil)))
                   (expect (= [:runtime "connection_failed"] @closed))))
             (it "tells a departed peer from a media defect"
                 (let [gone (java.io.IOException. "Broken pipe")]
                   (expect (#'media/peer-left? gone))
                   (expect (#'media/peer-left? (ex-info "Send failed" {} gone)))
                   (expect (not (#'media/peer-left? (ex-info "Missing method" {})))))))

(defdescribe
  event-overflow-test
  (it
    "closes an overflowed subscription off the session event thread"
    (let [sink
          (atom nil)

          release
          (promise)

          closed
          (promise)

          coordinator
          {:state (atom {:closed false})}

          runtime
          {:events (ArrayBlockingQueue. 1)
           :sid (str session-id)
           :id "subscriber"
           :coordinator coordinator
           :threads (atom [])}]

      (with-redefs-fn {#'state/subscribe! (fn [_ _ f _]
                                            (reset! sink f))
                       #'state/unsubscribe! (fn [& _]
                                              nil)
                       #'voice/task-event! (fn [& _]
                                             @release)
                       #'voice/close! (fn [_ reason]
                                        (swap! (:state coordinator) assoc :closed true)
                                        (deliver closed [reason (Thread/currentThread)]))}
        #(do (#'voice/subscribe! runtime)
             (dotimes [_ 4]
               (@sink {"type" "turn.started"}))
             (deliver release true)
             (let [[reason thread] (deref closed 3000 nil)]
               (expect (= "event_overflow" reason))
               (expect (some? thread))
               (expect (not= (Thread/currentThread) thread)))
             (.join ^Thread (first @(:threads runtime)) 3000))))))

(defdescribe mobile-capability-gate-test
             (it "lists native mobile test clients without claiming device qualification"
                 (fixture {}
                          (fn [_]
                            (let [profile (:profile (voice/capabilities))]
                              (expect (= ["companion_desktop" "companion_ios_test"
                                          "companion_android_test" "tui_headset"]
                                         (:clients profile)))
                              (expect (false? (:mobile-qualified profile))))))))

(defdescribe
  live-admission-status-test
  ;; A real Companion turn returned streaming and closed voice on schema validation.
  (it "normalizes active admission and preserves queued admission"
      (doseq [[status expected] [["streaming" "running"] ["running" "running"] ["queued" "queued"]]]
        (fixture
          {:turn-status status}
          (fn [{:keys [start events subscriber]}]
            (let [runtime (open! start)]
              (expect (await! #(some (fn [event]
                                       (= "session.ready" (:type event)))
                                     @events)))
              (control! runtime {:type "utterance.start" :capture-generation 0 :utterance-id 0})
              (control! runtime {:type "utterance.end" :capture-generation 0 :utterance-id 0})
              (expect (await! #(some (fn [event]
                                       (= "utterance.admitted" (:type event)))
                                     @events)))
              (let [event (first (filter #(= "utterance.admitted" (:type %)) @events))]
                (expect (= expected (:status event)))
                (expect (contract/valid? "server_event" event))
                (expect (= expected (:status (first (:receipts (voice/snapshot runtime))))))
                (@subscriber
                 {"type" "speech.committed"
                  "turn_id" (:turn-id event)
                  "source" "stream:1:0"
                  "text" "I am here."})
                (expect (await! #(some (fn [item]
                                         (= "output.finished" (:type item)))
                                       @events)))
                (expect (= "live" (get-in (voice/snapshot runtime) [:state :capture]))))))))))

(defdescribe
  missing-live-speech-test
  ;; A finalized voice request returned prose without any speech or audio events.
  (it "announces missing speech without reading arbitrary answer content"
      (doseq [content [[{"type" "prose" "markdown" "I am here. What would you like to work on?"}]
                       [{"type" "prose" "markdown" "private-answer-marker"}
                        {"type" "code" "text" "private-code-marker"}
                        {"type" "reasoning" "text" "private-reasoning-marker"}]
                       [{"type" "speech" "text" "   "}] []]]
        (fixture
          {}
          (fn [{:keys [start events subscriber admitted]}]
            (let [runtime (open! start)]
              (expect (await! #(some (fn [event]
                                       (= "session.ready" (:type event)))
                                     @events)))
              (control! runtime {:type "utterance.start" :capture-generation 0 :utterance-id 0})
              (control! runtime {:type "utterance.end" :capture-generation 0 :utterance-id 0})
              (expect (await! #(and (= 1 (count @admitted))
                                    (seq (:receipts (voice/snapshot runtime))))))
              (let [turn-id (:turn-id (first (:receipts (voice/snapshot runtime))))
                    event {"type" "turn.completed" "turn_id" turn-id "content" content}]

                (@subscriber (assoc event "turn_id" (str (UUID/randomUUID))))
                (expect (not-any? #(= "speech.committed" (:type %)) @events))
                (@subscriber event)
                (@subscriber event)
                (expect (await! #(some (fn [item]
                                         (= "output.finished" (:type item)))
                                       @events)))
                (let [speech (filter #(= "speech.committed" (:type %)) @events)]
                  (expect (= 1 (count speech)))
                  (expect (= "The spoken reply is unavailable. Please check the text answer."
                             (:text (first speech)))))
                (expect (= "terminal" (:agent (:state (voice/snapshot runtime))))))))))))

(defdescribe
  presentation-terminal-test
  ;; Regression: streamed progress must not hide a missing final conversational outcome.
  (it
    "deduplicates records and distinguishes final answers, silence and presentation failure"
    (doseq [[final expected]
            [[[{"id" "progress" "type" "conversation" "phase" "progress" "text" "Checking."}
               {"id" "answer" "type" "conversation" "phase" "answer" "text" "The diagram is ready."}
               {"id" "visual" "type" "visual" "markdown" "```mermaid\ngraph LR; A-->B\n```"}]
              ["Checking." "The diagram is ready."]]
             [[{"id" "visual" "type" "visual" "markdown" "Visible result"}
               {"id" "notice"
                "type" "notice"
                "code" "presentation_unavailable"
                "message" "The work finished, but its presentation is unavailable."}]
              ["Checking." "The spoken reply is unavailable. Please check the text answer."]]
             [[{"id" "progress" "type" "conversation" "phase" "progress" "text" "Checking."}]
              ["Checking." "The spoken reply is unavailable. Please check the text answer."]]]]
      (fixture
        {}
        (fn [{:keys [start events subscriber admitted]}]
          (let [runtime (open! start)]
            (expect (await! #(some (fn [e]
                                     (= "session.ready" (:type e)))
                                   @events)))
            (control! runtime {:type "utterance.start" :capture-generation 0 :utterance-id 0})
            (control! runtime {:type "utterance.end" :capture-generation 0 :utterance-id 0})
            (expect (await! #(and (= 1 (count @admitted))
                                  (seq (:receipts (voice/snapshot runtime))))))
            (let [turn-id (:turn-id (first (:receipts (voice/snapshot runtime))))
                  event {"type" "turn.completed" "turn_id" turn-id "content" final}]

              (@subscriber
               {"type" "speech.committed"
                "turn_id" turn-id
                "source" "presentation:progress"
                "text" "Checking."})
              (expect (await! #(some (fn [e]
                                       (= "output.finished" (:type e)))
                                     @events)))
              (@subscriber event)
              (@subscriber event)
              (expect (await! #(some (fn [e]
                                       (= "terminal" (get-in e [:state :agent])))
                                     @events)))
              (expect (= expected
                         (mapv :text (filter #(= "speech.committed" (:type %)) @events))))))))))
  (it "keeps explicit user-requested silence silent while completing the task"
      (fixture {}
               (fn [{:keys [start events subscriber admitted]}]
                 (let [runtime (open! start)]
                   (expect (await! #(some (fn [e]
                                            (= "session.ready" (:type e)))
                                          @events)))
                   (control! runtime
                             {:type "utterance.start" :capture-generation 0 :utterance-id 0})
                   (control! runtime {:type "utterance.end" :capture-generation 0 :utterance-id 0})
                   (expect (await! #(and (= 1 (count @admitted))
                                         (seq (:receipts (voice/snapshot runtime))))))
                   (@subscriber
                    {"type" "turn.completed"
                     "turn_id" (:turn-id (first (:receipts (voice/snapshot runtime))))
                     "content" [{"id" "silence" "type" "silence" "reason" "user_requested"}
                                {"id" "visual" "type" "visual" "markdown" "## Finished"}]})
                   (expect (await! #(some (fn [e]
                                            (= "terminal" (get-in e [:state :agent])))
                                          @events)))
                   (expect (not-any? #(= "speech.committed" (:type %)) @events)))))))

(defdescribe
  live-voice-runtime
  (describe
    "readiness and leases"
    (it "fails closed when the control queue fills before a socket opens"
        (fixture
          {}
          (fn [{:keys [start cancelled]}]
            (let [response
                  (start)

                  runtime
                  (voice/runtime-for session-id (get-in response [:data :voice-session-id]))]

              (dotimes [_ 70]
                (control! runtime {:type "heartbeat"}))
              (expect (= "off" (get-in (voice/snapshot runtime) [:state :capture])))
              (expect (empty? @cancelled))))))
    (it "retains a long session snapshot after close"
        (let [clock (atom 0)]
          (with-redefs [voice/monotonic-ms #(deref clock)]
            (fixture {}
                     (fn [{:keys [start]}]
                       (let [first-start (start)
                             id (get-in first-start [:data :voice-session-id])
                             runtime (voice/runtime-for session-id id)]

                         (reset! clock (inc (:retention-ms contract/limits)))
                         (voice/close! runtime "user_ended")
                         (expect (await! #(every? (fn [^Thread thread]
                                                    (not (.isAlive thread)))
                                                  @(:threads runtime))))
                         (expect (= 201 (:status (start))))
                         (expect (some? (voice/runtime-for session-id id)))))))))
    (it "does not send ready before native preparation completes"
        (let [entered
              (promise)

              release
              (promise)]

          (fixture {:prepare (fn [_]
                               (deliver entered true)
                               @release
                               {:ready true})}
                   (fn [{:keys [start events]}]
                     (open! start)
                     (try (expect (= true (deref entered 1000 :timeout)))
                          (expect (not-any? #(= "session.ready" (:type %)) @events))
                          (deliver release true)
                          (expect (await! #(some (fn [event]
                                                   (= "session.ready" (:type event)))
                                                 @events)))
                          (finally (deliver release true)))))))
    (it "cannot reopen capture after close during model preparation"
        (let [entered
              (promise)

              release
              (promise)]

          (fixture {:prepare (fn [_]
                               (deliver entered true)
                               @release
                               {:ready true})}
                   (fn [{:keys [start events opened]}]
                     (let [runtime (open! start)]
                       (try (expect (= true (deref entered 1000 :timeout)))
                            (voice/close! runtime "user_ended")
                            (deliver release true)
                            (expect (await! #(every? (fn [^Thread thread]
                                                       (not (.isAlive thread)))
                                                     @(:threads runtime))))
                            (expect (not-any? #(= "session.ready" (:type %)) @events))
                            (expect (zero? @opened))
                            (finally (deliver release true))))))))
    (it "keeps heartbeat acknowledgements available while preparing"
        (let [release (promise)]
          (fixture {:prepare (fn [_]
                               @release
                               {:ready true})}
                   (fn [{:keys [start events]}]
                     (let [runtime (open! start)]
                       (try (control! runtime {:type "heartbeat"})
                            (expect (await! #(some (fn [event]
                                                     (= "session.state" (:type event)))
                                                   @events)))
                            (finally (deliver release true))))))))
    (it "rejects input before preparation and never opens an ASR handle"
        (let [release (promise)]
          (fixture {:prepare (fn [_]
                               @release
                               {:ready true})}
                   (fn [{:keys [start opened]}]
                     (let [runtime (open! start)]
                       (try (control!
                              runtime
                              {:type "utterance.start" :capture-generation 0 :utterance-id 0})
                            (expect (= "off" (:capture (:state (voice/snapshot runtime)))))
                            (expect (zero? @opened))
                            (finally (deliver release true))))))))
    (it "requires an exact origin and session, and consumes a ticket once"
        (fixture
          {}
          (fn [{:keys [start]}]
            (let [{:keys [data]}
                  (start)

                  {:keys [voice-session-id ticket]}
                  data]

              (expect (nil? (voice/claim! (UUID/randomUUID)
                                          voice-session-id
                                          ticket
                                          "https://gateway.example.com")))
              (expect
                (nil?
                  (voice/claim! session-id voice-session-id ticket "https://other.example.com")))
              (expect
                (some?
                  (voice/claim! session-id voice-session-id ticket "https://gateway.example.com")))
              (expect
                (nil?
                  (voice/claim! session-id voice-session-id ticket "https://gateway.example.com")))
              (expect (= 409 (:status (start)))))))))
  (describe
    "streaming and task ownership"
    (it "tracks owned human input without speaking private form content"
        (fixture
          {}
          (fn [{:keys [start events subscriber admitted cancelled]}]
            (let [runtime (open! start)]
              (expect (await! #(some (fn [event]
                                       (= "session.ready" (:type event)))
                                     @events)))
              (control! runtime {:type "utterance.start" :capture-generation 0 :utterance-id 0})
              (control! runtime {:type "utterance.end" :capture-generation 0 :utterance-id 0})
              (expect (await! #(and (= 1 (count @admitted))
                                    (seq (:receipts (voice/snapshot runtime))))))
              (let [turn-id (:turn-id (first (:receipts (voice/snapshot runtime))))
                    view-id (str (UUID/randomUUID))]

                (with-redefs [state/current-turn-id (constantly turn-id)]
                  (@subscriber
                   {"type" "view.open"
                    "kind" "input"
                    "view_id" view-id
                    "view" {"origin" {"session_id" (str session-id)}
                            "text" "A protected input request."}})
                  (expect (await! #(= "waiting_for_input"
                                      (get-in (voice/snapshot runtime) [:state :agent]))))
                  (@subscriber {"type" "view.close" "kind" "input" "view_id" view-id})
                  (expect (await! #(= "running"
                                      (get-in (voice/snapshot runtime) [:state :agent])))))
                (expect (= 1 (count @admitted)))
                (expect (empty? @cancelled))
                (expect (not-any? #(= "speech.committed" (:type %)) @events)))))))
    (it "records generated PCM separately from credited output on interruption"
        (fixture
          {:synthesize (fn [{:keys [on-pcm]}]
                         (on-pcm (float-array 48000) 24000)
                         {:state :finished})}
          (fn [{:keys [start events subscriber admitted durable]}]
            (let [runtime (open! start)]
              (expect (await! #(some (fn [event]
                                       (= "session.ready" (:type event)))
                                     @events)))
              (control! runtime {:type "utterance.start" :capture-generation 0 :utterance-id 0})
              (control! runtime {:type "utterance.end" :capture-generation 0 :utterance-id 0})
              (expect (await! #(and (= 1 (count @admitted))
                                    (seq (:receipts (voice/snapshot runtime))))))
              (let [turn-id (:turn-id (first (:receipts (voice/snapshot runtime))))]
                (@subscriber
                 {"type" "speech.committed"
                  "turn_id" turn-id
                  "source" "stream:1:0"
                  "text" "A verified result."})
                (expect (await! #(= 50
                                    (count (filter (fn [event]
                                                     (:pcm event))
                                                   @events)))))
                (control! runtime {:type "output.interrupt" :output-epoch 0 :reason "stop_speech"})
                (let [receipt (second (first (filter #(= "voice.playback" (first %)) @durable)))]
                  (expect (= 48000 (:generated receipt)))
                  (expect (zero? (:played receipt)))))))))
    (it
      "delivers a fatal error before closing without acknowledging native abort early"
      (let [entered
            (promise)

            aborted
            (promise)

            release
            (promise)]

        (fixture
          {:synthesize (fn [{:keys [register-abort!]}]
                         (register-abort! #(deliver aborted true))
                         (deliver entered true)
                         @release
                         {:state :aborted})}
          (fn [{:keys [start events subscriber admitted cancelled]}]
            (let [runtime (open! start)]
              (try
                (expect (await! #(some (fn [event]
                                         (= "session.ready" (:type event)))
                                       @events)))
                (control! runtime {:type "utterance.start" :capture-generation 0 :utterance-id 0})
                (control! runtime {:type "utterance.end" :capture-generation 0 :utterance-id 0})
                (expect (await! #(and (= 1 (count @admitted))
                                      (seq (:receipts (voice/snapshot runtime))))))
                (@subscriber
                 {"type" "speech.committed"
                  "turn_id" (:turn-id (first (:receipts (voice/snapshot runtime))))
                  "source" "stream:1:0"
                  "text" "A verified result."})
                (expect (= true (deref entered 1000 :timeout)))
                (control! runtime {:type "output.interrupt" :output-epoch 0 :reason "stop_speech"})
                (expect (= true (deref aborted 1000 :timeout)))
                (expect (not-any? #(= "acknowledged" (:abort %)) @events))
                (deliver release true)
                (expect (await! #(some (fn [event]
                                         (= "acknowledged" (:abort event)))
                                       @events)))
                (voice/message! runtime "invalid json")
                (expect (await! #(some (fn [event]
                                         (and (= "error" (:type event)) (:fatal event)))
                                       @events)))
                (expect (empty? @cancelled))
                (finally (deliver release true))))))))
    (it
      "keeps the lease alive after a muted inference throws during native abort"
      (let [entered
            (promise)

            release
            (promise)

            calls
            (atom 0)]

        (fixture
          {:feed (fn [_ _ _]
                   (when (= 1 (swap! calls inc))
                     (deliver entered true)
                     @release
                     (throw (ex-info "Aborted" {:type :speech-live/closed}))))}
          (fn [{:keys [start events opened]}]
            (let [runtime
                  (open! start)

                  frame
                  (fn [generation id]
                    (contract/encode-frame {:generation generation :stream-id id :sample-offset 0}
                                           (byte-array 960)))]

              (try
                (expect (await! #(some (fn [event]
                                         (= "session.ready" (:type event)))
                                       @events)))
                (control! runtime {:type "utterance.start" :capture-generation 0 :utterance-id 0})
                (voice/message! runtime (frame 0 0))
                (expect (= true (deref entered 1000 :timeout)))
                (control! runtime {:type "capture.mute" :muted true})
                ;; Unmute aborts the input. Mute keeps it.
                (control! runtime {:type "capture.mute" :muted false})
                (deliver release true)
                (control! runtime {:type "utterance.start" :capture-generation 1 :utterance-id 1})
                (voice/message! runtime (frame 1 1))
                (expect (await! #(= 2 @opened)))
                (expect (await! #(= 2 @calls)))
                (expect (= "live" (:capture (:state (voice/snapshot runtime)))))
                (expect (= 1 (count @(:input-handles runtime))))
                (finally (deliver release true))))))))
    (it
      "streams a committed segment before completion and never repeats it at the terminal"
      (fixture
        {}
        (fn [{:keys [start events subscriber admitted]}]
          (let [runtime (open! start)]
            (expect (await! #(some (fn [event]
                                     (= "session.ready" (:type event)))
                                   @events)))
            (control! runtime {:type "utterance.start" :capture-generation 0 :utterance-id 0})
            (control! runtime {:type "utterance.end" :capture-generation 0 :utterance-id 0})
            (expect (await! #(and (= 1 (count @admitted))
                                  (seq (:receipts (voice/snapshot runtime))))))
            (let [turn-id (:turn-id (first (:receipts (voice/snapshot runtime))))
                  event {"type" "speech.committed"
                         "turn_id" turn-id
                         "source" "presentation:answer"
                         "text" "A verified result."}]

              (@subscriber event)
              (@subscriber event)
              (expect (await! #(some (fn [item]
                                       (= "output.finished" (:type item)))
                                     @events)))
              (expect (= "running" (:agent (:state (voice/snapshot runtime)))))
              (@subscriber
               {"type" "turn.completed"
                "turn_id" turn-id
                "content" [{"id" "answer"
                            "type" "conversation"
                            "phase" "answer"
                            "text" "A verified result."}]})
              (expect (await! #(= "terminal" (:agent (:state (voice/snapshot runtime))))))
              (expect (= 1 (count (filter #(= "speech.committed" (:type %)) @events))))
              (@subscriber {"type" "speech.invalidated" "turn_id" turn-id})
              (expect (await! #(= 1 (:output-epoch (:state (voice/snapshot runtime)))))))))))
    (it
      "streams finalized input through normal admission and streams only its owned answer"
      (fixture
        {}
        (fn [{:keys [start events admitted subscriber cancelled durable]}]
          (let [runtime (open! start)]
            (expect (await! #(some (fn [event]
                                     (= "session.ready" (:type event)))
                                   @events)))
            (control! runtime {:type "utterance.start" :capture-generation 0 :utterance-id 0})
            (voice/message! runtime
                            (contract/encode-frame {:generation 0 :stream-id 0 :sample-offset 0}
                                                   (byte-array 960)))
            (expect (await! #(some (fn [event]
                                     (= "transcript" (:type event)))
                                   @events)))
            (expect (empty? @admitted))
            (control! runtime {:type "utterance.end" :capture-generation 0 :utterance-id 0})
            (expect (await! #(and (= 1 (count @admitted))
                                  (seq (:receipts (voice/snapshot runtime))))))
            (let [turn-id (:turn-id (first (:receipts (voice/snapshot runtime))))]
              (expect (= {"voice_projection" true "voice_segments" true}
                         (:turn-features (first @admitted))))
              (expect (await! #(some? @subscriber)))
              (@subscriber
               {"type" "turn.completed"
                "turn_id" (str (UUID/randomUUID))
                "content"
                [{"id" "answer" "type" "conversation" "phase" "answer" "text" "Unowned answer."}]})
              (@subscriber
               {"type" "turn.completed"
                "turn_id" turn-id
                "content"
                [{"id" "answer" "type" "conversation" "phase" "answer" "text" "The test passed."}]})
              (expect (await! #(some (fn [event]
                                       (= "output.finished" (:type event)))
                                     @events)))
              (expect (= 2 (count (filter :pcm @events))))
              (let [segment (:segment-id (first (filter #(= "speech.committed" (:type %))
                                                        @events)))]
                (control!
                  runtime
                  {:type "playback.receipt" :output-epoch 0 :segment-id segment :played 960})
                (expect (await! #(some (fn [[type _]]
                                         (= "voice.playback" type))
                                       @durable))))
              (control! runtime {:type "output.interrupt" :output-epoch 0 :reason "stop_speech"})
              (expect (await! #(some (fn [event]
                                       (= "acknowledged" (:abort event)))
                                     @events)))
              (expect (empty? @cancelled))
              (expect (= "live" (:capture (:state (voice/snapshot runtime)))))
              (expect (every? #(or (:pcm %) (contract/valid? "server_event" %)) @events)))))))))

(defdescribe
  input-abort-test
  ;; The Companion overflow report requires recovery while native finalization still exits.
  (it
    "ignores aborted finalization before opening the next generation"
    (doseq [fails? [false true]]
      (let [entered (promise)
            release (promise)
            calls (atom 0)]

        (fixture
          {:finish (fn [_]
                     (when (= 1 (swap! calls inc))
                       (deliver entered true)
                       @release
                       (when fails? (throw (ex-info "Aborted" {}))))
                     {:text "New request." :revision 2})}
          (fn [{:keys [start events admitted opened]}]
            (let [runtime (open! start)]
              (try
                (expect (await! #(some (fn [e]
                                         (= "session.ready" (:type e)))
                                       @events)))
                (control! runtime {:type "utterance.start" :capture-generation 0 :utterance-id 0})
                (control! runtime {:type "utterance.end" :capture-generation 0 :utterance-id 0})
                (expect (= true (deref entered 1000 :timeout)))
                (control! runtime {:type "capture.mute" :muted true})
                (control! runtime {:type "capture.mute" :muted false})
                (control! runtime {:type "utterance.start" :capture-generation 1 :utterance-id 1})
                (expect (= 1 @opened))
                (deliver release true)
                (expect (await! #(= 2 @opened)))
                (expect (empty? @admitted))
                (control! runtime {:type "utterance.end" :capture-generation 1 :utterance-id 1})
                (expect (await! #(= 1 (count @admitted))))
                (expect (= [1] (mapv :utterance-id (:receipts (voice/snapshot runtime)))))
                (expect (= "live" (get-in (voice/snapshot runtime) [:state :capture])))
                (finally (deliver release true))))))))))

(defdescribe
  input-buffer-test
  ;; The Companion overflow report exposed lifecycle commands sharing the audio budget.
  (it
    "reserves lifecycle capacity during slow decoding and finalization"
    (doseq [operation [:feed :finish]]
      (let [entered (promise)
            release (promise)
            blocking (fn [& _]
                       (deliver entered true)
                       @release
                       nil)]

        (fixture
          {operation blocking}
          (fn [{:keys [start events durable]}]
            (let [runtime (open! start)
                  effects (:effects (:coordinator runtime))
                  frame {:generation 0 :pcm (byte-array 960) :sample-offset 0}]

              (try
                (expect (await! #(some (fn [e]
                                         (= "session.ready" (:type e)))
                                       @events)))
                (expect ((:open-input effects) 0 0))
                (expect (await! #(contains? @(:input-handles runtime) 0)))
                (if (= operation :feed) ((:feed-input effects) 0 frame) ((:finish-input effects) 0))
                (expect (= true (deref entered 1000 :timeout)))
                (expect (every? true? (repeatedly 100 #((:feed-input effects) 1 frame))))
                (expect ((:finish-input effects) 0))
                (expect ((:open-input effects) 1 0))
                (expect ((:finish-input effects) 1))
                (expect ((:open-input effects) 2 0))
                (expect ((:finish-input effects) 2))
                (expect (false? ((:feed-input effects) 2 frame)))
                (let [[type payload] (last @durable)]
                  (expect (= "voice.input_overflow" type))
                  (expect (= 2000 (:buffered-ms payload)))
                  (expect (= (name operation) (:active-operation payload)))
                  (expect (= "feed" (:operation payload)))
                  (expect (nat-int? (:active-duration-ms payload)))
                  (expect (= #{:queue-depth :buffered-ms :audio-frames :voice-session-id
                               :capture-generation :operation :active-operation :active-duration-ms}
                             (set (keys payload)))))
                (expect ((:open-input effects) 3 0))
                (expect (false? ((:finish-input effects) 3)))
                (expect (= [:finish :open :finish :open :finish :open]
                           (mapv first
                                 (drop 100
                                       (seq (.toArray ^java.util.concurrent.ArrayBlockingQueue
                                                      (:input runtime)))))))
                ((:abort-input effects))
                (expect (every? true? (repeatedly 100 #((:feed-input effects) 3 frame))))
                (expect (false? ((:feed-input effects) 3 frame)))
                (finally (deliver release true))))))))))
