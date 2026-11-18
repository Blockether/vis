(ns com.blockether.vis.internal.gateway.voice-session-test
  (:require [com.blockether.vis.contract.voice :as contract]
            [com.blockether.vis.internal.gateway.voice-session :as voice]
            [lazytest.core :refer [defdescribe describe expect it]]))

(defn- fixture
  ([] (fixture {}))
  ([options]
   (let [clock
         (atom 0)

         events
         (atom [])

         durable
         (atom [])

         admitted
         (atom [])

         cancelled
         (atom [])

         feeds
         (atom [])

         aborts
         (atom [])

         session
         (voice/create (merge {:voice-session-id "31d34262-789a-43bb-82e3-6e8531b32000"
                               :client-id "31d34262-789a-43bb-82e3-6e8531b32001"
                               :now #(deref clock)
                               :emit #(swap! events conj %)
                               :persist #(swap! durable conj [%1 %2])
                               :admit (fn [request]
                                        (swap! admitted conj request)
                                        {:turn-id (format "31d34262-789a-43bb-82e3-%012d"
                                                          (+ 10 (count @admitted)))
                                         :status (if (= 1 (count @admitted)) "running" "queued")})
                               :cancel-task #(swap! cancelled conj %)
                               :feed-input #(do (swap! feeds conj [%1 %2]) true)
                               :abort-input #(swap! aborts conj :input)
                               :abort-output #(swap! aborts conj :output)}
                              options))]

     {:session session
      :clock clock
      :events events
      :durable durable
      :admitted admitted
      :cancelled cancelled
      :feeds feeds
      :aborts aborts})))

(defn- final!
  [session id text]
  (voice/start-utterance! session (:capture-generation (voice/snapshot session)) id)
  (voice/end-utterance! session (:capture-generation (voice/snapshot session)) id)
  (voice/final! session id {:text text :revision 1}))

(defn- owned-turn [session] (:turn-id (last (voice/receipts session))))

(defn- segment!
  [session]
  (voice/commit-speech! session
                        {:turn-id (owned-turn session) :text "A safe answer." :source "answer:0"}))

(defdescribe
  input-overload-recovery-test
  (it
    "rejects pending input, preserves admissions and ignores late finalization"
    (doseq [operation [:open-input :feed-input :finish-input]]
      (let [refuse? (atom false)
            {:keys [session events admitted cancelled aborts durable]}
            (fixture {operation (fn [& _]
                                  (not @refuse?))})]

        (final! session 0 "Keep this admitted task")
        (voice/start-utterance! session 0 1)
        (voice/end-utterance! session 0 1)
        (reset! refuse? true)
        (voice/start-utterance! session 0 2)
        (when (= operation :feed-input)
          (voice/input-frame! session
                              {:generation 0 :stream-id 2 :sample-offset 0 :sample-count 480}))
        (when (= operation :finish-input) (voice/end-utterance! session 0 2))
        (voice/final! session 1 {:text "Must not execute" :revision 1})
        (voice/final! session 2 {:text "Must not execute" :revision 1})
        (expect (= "muted" (:capture (voice/snapshot session))))
        (expect (= "rejected" (:utterance (voice/snapshot session))))
        (expect (= 1 (:capture-generation (voice/snapshot session))))
        (expect (= 1 (count @admitted)))
        (expect (empty? @cancelled))
        (expect (= :input (last @aborts)))
        (expect (= 2 (count (filter #(= "input_overflow" (:error (second %))) @durable))))
        (expect (= #{1 2}
                   (set (map :utterance-id (filter #(= "utterance.rejected" (:type %)) @events)))))
        (expect (= 2 (count (filter #(= "input_overflow" (:error %)) (voice/receipts session)))))
        (expect (some #(and (= "error" (:type %)) (false? (:fatal %))) @events))
        (expect (every? #(contract/valid? "server_event" %) @events))
        (reset! refuse? false)
        (voice/mute! session false)
        (final! session 3 "New request after explicit unmute")
        (expect (= 2 (count @admitted)))
        (expect (= "live" (:capture (voice/snapshot session))))))))

(defdescribe
  live-voice-coordinator
  (describe
    "input and admission"
    (it "does not give a late final the next utterance output epoch"
        (let [{:keys [session]} (fixture)]
          (voice/start-utterance! session 0 1)
          (voice/end-utterance! session 0 1)
          (voice/start-utterance! session 0 2)
          (voice/end-utterance! session 0 2)
          (voice/final! session 1 {:text "The earlier request" :revision 1})
          (expect (nil? (segment! session)))
          (voice/final! session 2 {:text "The current request" :revision 1})
          (expect (some? (segment! session)))))
    (it "admits ten finalized turns once and never executes provisional text"
        (let [{:keys [session admitted events durable]} (fixture)]
          (doseq [id (range 10)]
            (voice/start-utterance! session 0 id)
            (voice/partial! session id {:text "do not" :revision 1})
            (expect (= id (count @admitted)))
            (voice/end-utterance! session 0 id)
            (voice/final! session id {:text "Do not delete anything." :revision 2})
            (voice/final! session id {:text "delete everything" :revision 3}))
          (expect (= 10 (count @admitted)))
          (expect (= 10 (count (set (map :idempotency-key @admitted)))))
          (expect (= "running" (:agent (voice/snapshot session))))
          (expect (= 10 (count (filter #(= "voice.admission" (first %)) @durable))))
          (expect (every? #(contract/valid? "server_event" %) @events))))
    (it "tells the client when ended speech held no words"
        (let [{:keys [session admitted events]} (fixture)]
          (final! session 1 "  ")
          (expect (empty? @admitted))
          (expect (= [{:type "utterance.rejected" :utterance-id 1 :reason "no_speech"}]
                     (filterv #(= "utterance.rejected" (:type %)) @events)))
          (expect (every? #(contract/valid? "server_event" %) @events))))
    (it
      "drops repeated and stale frames and fails on an input gap"
      (let [{:keys [session feeds]}
            (fixture)

            frame
            {:generation 0 :stream-id 1 :sample-offset 0 :sample-count 480 :pcm (byte-array 960)}]

        (voice/start-utterance! session 0 1)
        (expect (voice/input-frame! session frame))
        (expect (false? (voice/input-frame! session frame)))
        (expect (false? (voice/input-frame! session (assoc frame :generation 9))))
        (expect (= 1 (count @feeds)))
        (voice/input-frame! session (assoc frame :sample-offset 960))
        (expect (= "off" (:capture (voice/snapshot session))))))
    (it "admits speech from before a mute and invalidates unfinished input on unmute"
        (let [finished
              (atom [])

              {:keys [session admitted aborts]}
              (fixture {:finish-input #(do (swap! finished conj %) true)})]

          (voice/start-utterance! session 0 1)
          (voice/end-utterance! session 0 1)
          (voice/start-utterance! session 0 2)
          (voice/mute! session true)
          (expect (= [1 2] @finished))
          (expect (not-any? #{:input} @aborts))
          (expect (= "muted" (:capture (voice/snapshot session))))
          (expect (false? (voice/start-utterance! session 0 3)))
          (voice/final! session 1 {:text "Said before the mute" :revision 1})
          (expect (= ["Said before the mute"] (mapv :request @admitted)))
          (voice/control! session {:type "capture.mute" :muted false})
          (voice/final! session 2 {:text "A stale request" :revision 1})
          (expect (= 1 (count @admitted)))
          (expect (= [:input] (filterv #{:input} @aborts)))
          (expect (= 1 (:capture-generation (voice/snapshot session))))))
    (it "preserves mute when the session starts muted"
        (let [{:keys [session admitted]} (fixture {:muted true})]
          (final! session 0 "Cannot hear this")
          (expect (= "muted" (:capture (voice/snapshot session))))
          (expect (empty? @admitted))))
    (it "refuses overflow without passing more PCM to the engine"
        (let [{:keys [session admitted]} (fixture {:feed-input (constantly false)})]
          (voice/start-utterance! session 0 0)
          (voice/input-frame! session
                              {:generation 0 :stream-id 0 :sample-offset 0 :sample-count 480})
          (voice/end-utterance! session 0 0)
          (voice/final! session 0 {:text "Cannot execute this" :revision 1})
          (expect (= "muted" (:capture (voice/snapshot session))))
          (expect (empty? @admitted)))))
  (describe
    "output and task isolation"
    (it "sends a spoken cancel phrase to the agent and cancels no task"
        (let [{:keys [session admitted cancelled]} (fixture)]
          (final! session 0 "Check the first result.")
          (final! session 1 "Cancel that task.")
          (expect (empty? @cancelled))
          (expect (= 2 (count @admitted)))))
    (it "keeps a current task visible when an older task reaches its terminal"
        (let [{:keys [session]} (fixture)]
          (final! session 0 "The first request")
          (let [first-id (owned-turn session)]
            (final! session 1 "The next request")
            (let [next-id (owned-turn session)]
              (voice/task-state! session first-id "terminal")
              (voice/task-state! session next-id "running")
              (voice/task-state! session first-id "terminal")
              (expect (= "running" (:agent (voice/snapshot session))))))))
    (it "observes all owned input views without answering them"
        (let [{:keys [session admitted cancelled]} (fixture)]
          (final! session 0 "Check the result")
          (let [turn-id (owned-turn session)]
            (voice/task-input! session turn-id "input-one" true)
            (voice/task-input! session turn-id "input-two" true)
            (expect (= "waiting_for_input" (:agent (voice/snapshot session))))
            (voice/task-input! session nil "input-one" false)
            (expect (= "waiting_for_input" (:agent (voice/snapshot session))))
            (voice/task-input! session "unowned" "input-other" true)
            (voice/task-input! session nil "input-two" false)
            (expect (= "running" (:agent (voice/snapshot session))))
            (expect (= 1 (count @admitted)))
            (expect (empty? @cancelled)))))
    (it "interrupts output but keeps capture and tasks alive"
        (let [{:keys [session cancelled]} (fixture)]
          (final! session 0 "Check this")
          (segment! session)
          (voice/interrupt! session 0 "barge_in")
          (expect (= "live" (:capture (voice/snapshot session))))
          (expect (= "running" (:agent (voice/snapshot session))))
          (expect (= 1 (:output-epoch (voice/snapshot session))))
          (expect (nil? (segment! session)))
          (expect (empty? @cancelled))))
    (it "suppresses the old turn when newer input starts before its first speech segment"
        (let [{:keys [session cancelled]} (fixture)]
          (final! session 0 "Check the first thing")
          (let [old-turn (owned-turn session)]
            (voice/start-utterance! session 0 1)
            (expect (nil? (voice/commit-speech!
                            session
                            {:turn-id old-turn :text "An old answer." :source "late"})))
            (expect (empty? @cancelled)))))
    (it "speaks only committed owned text and rejects duplicate sources"
        (let [{:keys [session]} (fixture)]
          (final! session 0 "Check this")
          (expect (nil? (voice/commit-speech! session
                                              {:turn-id "other" :text "Unowned" :source "x"})))
          (expect (some? (segment! session)))
          (expect (nil? (segment! session)))))
    (it "records the played position, not the sent position, at an interruption"
        (let [{:keys [session durable]} (fixture)]
          (final! session 0 "Check this")
          (let [{:keys [id epoch]} (segment! session)]
            (voice/reserve-output! session id epoch 480)
            (voice/playback! session {:output-epoch epoch :segment-id id :played 240})
            (voice/interrupt! session epoch "stop_speech")
            (let [receipt (second (last (filter #(= "voice.playback" (first %)) @durable)))]
              (expect (= 240 (:played receipt)))))))
    (it "drains once when playback reaches the end before synthesis finishes"
        (let [{:keys [session durable]} (fixture)]
          (final! session 0 "Check this")
          (let [{:keys [id epoch]} (segment! session)]
            (voice/reserve-output! session id epoch 480)
            (voice/playback! session {:output-epoch epoch :segment-id id :played 480})
            (voice/output-finished! session id epoch)
            (voice/output-finished! session id epoch)
            (expect (empty? (:segments @(:state session))))
            (expect (= "drained" (:output (voice/snapshot session))))
            (expect (= 1 (count (filter #(= "voice.playback" (first %)) @durable)))))))
    (it "does not report all output drained while another segment remains"
        (let [{:keys [session]} (fixture)]
          (final! session 0 "Check this")
          (let [{:keys [id epoch]} (segment! session)]
            (voice/commit-speech!
              session
              {:turn-id (owned-turn session) :text "Next answer." :source "answer:1"})
            (voice/reserve-output! session id epoch 480)
            (voice/output-finished! session id epoch)
            (voice/playback! session {:output-epoch epoch :segment-id id :played 480})
            (expect (= 1 (count (:segments @(:state session)))))
            (expect (not= "drained" (:output (voice/snapshot session)))))))
    (it "reports an interrupt request before a synchronous abort acknowledgement"
        (let [holder
              (atom nil)

              {:keys [session events]}
              (fixture {:abort-output #(voice/abort-acknowledged! @holder
                                                                  (:output-epoch (voice/snapshot
                                                                                   @holder)))})]

          (reset! holder session)
          (voice/interrupt! session 0 "stop_speech")
          (expect (= ["requested" "acknowledged"]
                     (mapv :abort (filter #(= "output.interrupted" (:type %)) @events))))))
    (it "releases a credit wait on interruption without accepting another frame"
        (let [{:keys [session]} (fixture)]
          (final! session 0 "Check this")
          (let [{:keys [id epoch]} (segment! session)]
            (dotimes [_ 50]
              (expect (some? (voice/reserve-output! session id epoch 480))))
            (let [waiting (future (voice/reserve-output! session id epoch 480))]
              (try (expect (= :waiting (deref waiting 30 :waiting)))
                   (voice/interrupt! session epoch "stop_speech")
                   (expect (nil? (deref waiting 1000 :timeout)))
                   (finally (future-cancel waiting)))))))
    (it "rejects impossible receipts and never cancels work on failure"
        (let [{:keys [session cancelled]} (fixture)]
          (final! session 0 "Check this")
          (let [{:keys [id epoch]} (segment! session)]
            (voice/reserve-output! session id epoch 480)
            (voice/playback! session {:output-epoch epoch :segment-id id :played 481})
            (expect (= "off" (:capture (voice/snapshot session))))
            (expect (empty? @cancelled)))))
    (it "accepts explicit cancellation only for owned turns"
        (let [{:keys [session cancelled]} (fixture)]
          (final! session 0 "Check this")
          (voice/control! session
                          {:type "task.cancel" :turn-id "31d34262-789a-43bb-82e3-6e8531b32999"})
          (expect (empty? @cancelled))
          (voice/control! session {:type "task.cancel" :turn-id (owned-turn session)})
          (expect (= [(owned-turn session)] @cancelled))))
    (it "recognizes exact speech controls without treating quotations or negation as controls"
        (let [{:keys [session admitted cancelled]} (fixture)]
          (final! session 0 "Do not stop speaking.")
          (expect (= 1 (count @admitted)))
          (final! session 1 "Stop speaking.")
          (expect (= 1 (count @admitted)))
          (expect (= "live" (:capture (voice/snapshot session))))
          (final! session 2 "End voice mode.")
          (expect (= "off" (:capture (voice/snapshot session))))
          (expect (empty? @cancelled)))))
  (describe
    "steered voice ownership"
    ;; Regression for the hands-free handoff: consumed voice rows must not remain active tasks.
    (it
      "does not revive stale input and transfers only a consumed owned queue row"
      (let [{:keys [session]} (fixture)]
        (final! session 0 "Start the task")
        (let [running (owned-turn session)]
          (final! session 1 "The earlier follow-up")
          (let [earlier (owned-turn session)]
            (voice/start-utterance! session 0 2)
            (voice/end-utterance! session 0 2)
            (voice/task-steered! session running [earlier] 2)
            (expect (nil? (voice/commit-speech!
                            session
                            {:turn-id running :source "earlier" :text "Old reply." :iteration 2})))
            (voice/final! session 2 {:text "The latest follow-up" :revision 1})
            (let [latest (owned-turn session)]
              (voice/task-steered! session running ["unowned" running] 3)
              (expect (= "queued" (get-in @(:state session) [:owned-turns latest :status])))
              (voice/task-steered! session running [latest] 3)
              (voice/task-steered! session running [earlier] 2)
              (expect (nil? (voice/commit-speech!
                              session
                              {:turn-id running :source "late" :text "Old reply." :iteration 2})))
              (expect
                (some? (voice/commit-speech!
                         session
                         {:turn-id running :source "current" :text "Current reply." :iteration 3})))
              (expect (nil? (voice/commit-speech!
                              session
                              {:turn-id latest :source "consumed" :text "Duplicate task."})))
              (expect (= [running]
                         (for [[id turn] (:owned-turns @(:state session))
                               :when (not= "terminal" (:status turn))]

                           id))))))))
    (it "does not regain speech from duplicate completion or a closed voice session"
        (let [{:keys [session]} (fixture)]
          (final! session 0 "Start the task")
          (let [running (owned-turn session)]
            (segment! session)
            (voice/task-state! session running "terminal")
            (expect (nil? (get-in @(:state session) [:owned-turns running :sources])))
            (expect (nil? (voice/commit-speech!
                            session
                            {:turn-id running :source "late" :text "Late reply."})))
            (final! session 1 "A queued follow-up")
            (let [queued (owned-turn session)]
              (voice/close! session "user_ended")
              (voice/task-steered! session "another-turn" [queued] 2)
              (expect (nil? (get-in @(:state session) [:owned-turns "another-turn"]))))))))
  (describe "lifecycle"
            (it "expires a silent owner and suppresses all late finals"
                (let [{:keys [session clock admitted cancelled]} (fixture)]
                  (voice/start-utterance! session 0 1)
                  (voice/end-utterance! session 0 1)
                  (reset! clock (:lease-ms contract/limits))
                  (voice/reap! session)
                  (voice/final! session 1 {:text "Stale" :revision 1})
                  (expect (= "off" (:capture (voice/snapshot session))))
                  (expect (empty? @admitted))
                  (expect (empty? @cancelled))))
            (it "rejects client-authored transcripts"
                (let [{:keys [session admitted]} (fixture)]
                  (voice/control! session {:type "transcript" :text "Run this"})
                  (expect (= "off" (:capture (voice/snapshot session))))
                  (expect (empty? @admitted))))))
