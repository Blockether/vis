(ns com.blockether.vis.tui.voice-live-test
  (:require [com.blockether.vis.contract.voice :as protocol]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.tui.client :as vis]
            [com.blockether.vis.tui.voice-input :as dictation]
            [com.blockether.vis.tui.voice-output :as manual-output]
            [com.blockether.vis.tui.voice-live :as live]
            [com.blockether.vis.tui.voice-live-model :as model]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [java.lang.reflect InvocationHandler Proxy]
           [java.net.http WebSocket]
           [java.util.concurrent ArrayBlockingQueue]
           [javax.sound.sampled SourceDataLine TargetDataLine]))

(defn- fails? [f] (try (f) false (catch Exception _ true)))

(defn- feed
  [s count voiced?]
  (reduce (fn [[s effects] _]
            (let [[next-state more] (model/capture s (byte-array 960) voiced?)]
              [next-state (into effects more)]))
          [s []]
          (range count)))

(defn- fake-interface
  ([interface calls] (fake-interface interface calls {}))
  ([interface calls handlers]
   (Proxy/newProxyInstance (.getClassLoader ^Class interface)
                           (into-array Class [interface])
                           (reify
                             InvocationHandler
                               (invoke [_ _ method args]
                                 (let [name
                                       (.getName ^java.lang.reflect.Method method)

                                       type
                                       (.getReturnType ^java.lang.reflect.Method method)]

                                   (swap! calls conj name)
                                   (if-let [f (get handlers name)]
                                     (apply f args)
                                     (cond (= type Integer/TYPE) (int
                                                                   (if (= name "available") 4800 0))
                                           (= type Long/TYPE) (long 0)
                                           (= type Boolean/TYPE) false
                                           (= type Float/TYPE) (float 0)
                                           :else nil))))))))

(defdescribe
  live-voice-model-test
  (it "uses canonical framing and rejects corruption"
      (let [[s _]
            (feed (model/initial 3 5) 10 false)

            [_ effects]
            (feed s 6 true)

            [_ header pcm]
            (first (filter #(= :frame (first %)) effects))

            encoded
            (protocol/encode-frame header pcm)

            decoded
            (protocol/decode-frame encoded)]

        (expect (= 3 (:generation decoded)))
        (expect (= 480 (:sample-count decoded)))
        (expect (fails? #(protocol/decode-frame (java.util.Arrays/copyOf ^bytes encoded 15))))))
  (it "keeps playback active through short noise and resets speech qualification"
      (let [initial
            (assoc-in (model/initial 0 2) [:segments 1] {:received 0})

            [noise effects]
            (feed initial 5 true)

            [quiet later]
            (feed noise 1 false)

            [retry retry-effects]
            (feed quiet 5 true)

            header
            {:generation 2 :stream-id 1 :sample-offset 0 :sample-count 480}]

        (expect (empty? effects))
        (expect (empty? later))
        (expect (empty? retry-effects))
        (expect (false? (:blocked-output? retry)))
        (expect (= (:segments initial) (:segments retry)))
        (expect (second (model/accept-output retry header)))
        (expect (nil? (:utterance retry)))))
  (it "stops playback only when speech qualifies, before start and preroll audio"
      (let [[warm _]
            (feed (model/initial 3 5) 10 false)

            [pending early]
            (feed warm 5 true)

            [active effects]
            (feed pending 1 true)

            [continued more]
            (feed active 1 true)

            frames
            (filter #(= :frame (first %)) effects)]

        (expect (empty? early))
        (expect (= :stop-output (ffirst effects)))
        (expect (= {:type "output.interrupt" :output_epoch 5 :reason "barge_in"}
                   (get-in effects [1 1])))
        (expect (= "utterance.start" (get-in effects [2 1 :type])))
        (expect (= 16 (count frames)))
        (expect (= (map #(* 480 %) (range 16)) (map #(get-in % [1 :sample-offset]) frames)))
        (expect (:blocked-output? active))
        (expect (empty? (:segments active)))
        (expect (= [:frame] (mapv first more)))
        (expect (= (* 17 480) (get-in continued [:utterance :samples])))))
  (it "keeps preroll bounded and automatically rearms across ten turns"
      (let [[warm _]
            (feed (model/initial 1 1) 200 false)

            [done effects]
            (reduce (fn [[s all] _]
                      (let [[speaking start]
                            (feed s 12 true)

                            [ended end]
                            (feed speaking
                                  (quot (:endpoint-silence-ms protocol/limits)
                                        (:frame-ms protocol/limits))
                                  false)]

                        [ended (into (into all start) end)]))
                    [warm []]
                    (range 10))

            starts
            (filter #(= "utterance.start" (get-in % [1 :type])) effects)

            ends
            (filter #(= "utterance.end" (get-in % [1 :type])) effects)]

        (expect (= 10 (count (:preroll warm))))
        (expect (= 10 (count starts) (count ends)))
        (expect (= (range 1 11) (map #(get-in % [1 :utterance_id]) starts)))
        (expect (nil? (:utterance done)))))
  (it "keeps the utterance open until eleven hundred milliseconds of silence"
      (let [[speaking _]
            (feed (model/initial 1 1) 6 true)

            [waiting effects]
            (feed speaking 54 false)

            [ended last-frame]
            (feed waiting 1 false)]

        (expect (:utterance waiting))
        (expect (not-any? #(= "utterance.end" (get-in % [1 :type])) effects))
        (expect (nil? (:utterance ended)))
        (expect (= "utterance.end" (get-in (last last-frame) [1 :type])))))
  (it "bounds a long utterance"
      (let [[s effects] (feed (model/initial 0 0) 1500 true)]
        (expect (nil? (:utterance s)))
        (expect (= "utterance.end" (get-in (last effects) [1 :type])))))
  (it "rejects stale output and an invalid offset without crediting it as heard"
      (let [s
            (assoc-in (model/initial 0 2) [:segments 1] {:received 0 :scheduled 0})

            header
            {:generation 2 :stream-id 1 :sample-offset 0 :sample-count 480}

            [next-state accepted?]
            (model/accept-output s header)]

        (expect accepted?)
        (expect (false? (second (model/accept-output s (assoc header :generation 1)))))
        (expect (fails? #(model/accept-output next-state header)))
        (expect (= 0 (:played (model/receipt 2 1 (get-in next-state [:segments 1]) 100))))
        (expect (= 120
                   (:played (model/receipt 2 1 {:received 480 :scheduled 480 :base 1000} 1120))))))
  (it "uses RMS on signed PCM without opening devices"
      (expect (false? (model/speech? (byte-array 960))))
      (expect (model/speech? (protocol/samples->pcm (float-array 480 0.1))))))

(defdescribe
  live-voice-lifecycle-test
  (it "rejects speaker profiles before HTTP or devices"
      (with-redefs [vis/request!
                    (fn [& _]
                      (throw (AssertionError. "HTTP must not run")))

                    live/open-input!
                    #(throw (AssertionError. "Microphone must not open"))]

        (expect (fails? #(live/start! (atom {}) "speakers")))))
  (it "never auto-enables an unavailable gateway"
      (with-redefs [vis/request!
                    (fn [& _]
                      {:status 200 :body "{\"enabled\":false,\"available\":false}"})

                    live/open-output!
                    #(throw (AssertionError. "Speaker must not open"))]

        (expect (fails? #(live/start! (atom {:session {:id "s"}}) "headset")))))
  (it
    "owns fake devices, closes input on mute, and stops on a session switch"
    (let [calls
          (atom [])

          requests
          (atom [])

          workers
          (atom [])

          db
          (atom {:active-tab-id "tab" :session {:id "s"}})

          state
          {"voice_session_id" "00000000-0000-0000-0000-000000000001"
           "capture_generation" 0
           "output_epoch" 0
           "capture" "live"
           "utterance" "idle"
           "agent" "idle"
           "output" "idle"}]

      (with-redefs [vis/request!
                    (fn [method path & _]
                      (swap! requests conj [method path])
                      {:status 200
                       :body (wire/json-str (if (= method :get)
                                              {:enabled true :available true}
                                              {:voice_session_id (get state "voice_session_id")
                                               :media_path "/v1/media"
                                               :ticket "opaque"
                                               :state state}))})

                    vis/worker-future
                    (fn [name f]
                      (swap! workers conj [name f]))

                    vis/publish-channel-event!
                    (fn [& _])

                    vis/notify!
                    (fn [& _])

                    dictation/cancel-recording!
                    (fn [& _])

                    manual-output/stop!
                    (fn [])

                    live/open-input!
                    #(fake-interface TargetDataLine calls)

                    live/open-output!
                    #(fake-interface SourceDataLine calls)

                    live/connect!
                    (fn [& _]
                      (fake-interface WebSocket calls))]

        (try (live/start! db "headset")
             (expect (live/active?))
             (let [r @(var-get #'live/active)]
               (#'live/server-event! r {"type" "session.state" "state" state})
               (expect (some? @(:input r)))
               (live/mute! true)
               (expect (nil? @(:input r)))
               (expect (empty? (:preroll @(:model r))))
               (expect (some #{"close"} @calls))
               (expect (not-any? #(= "task.cancel" (:type %)) (seq (:send r))))
               (live/stop-speech!)
               (live/mute! false)
               (expect (nil? @(:input r)))
               (#'live/server-event!
                r
                {"type" "session.state" "state" (assoc state "capture_generation" 1)})
               (expect (some? @(:input r)))
               (#'live/server-event!
                r
                {"type" "output.finished" "output_epoch" 99 "segment_id" 3 "sample_count" 480})
               (expect (empty? (:segments @(:model r))))
               (swap! db assoc :active-tab-id "another")
               (expect (not (live/active?)))
               (expect (some #{"abort"} @calls))
               (expect (zero? (.size ^ArrayBlockingQueue (:send r)))))
             (finally (live/stop!)))))))

(defdescribe
  live-device-workers-test
  (it
    "keeps capture active during playback and reports only the device playback position"
    (let [calls
          (atom [])

          running
          (atom true)

          reading
          (promise)

          played
          (promise)

          input
          (fake-interface TargetDataLine
                          calls
                          {"read" (fn [& _]
                                    (deliver reading true)
                                    (when-not (deref played 1000 false)
                                      (throw (ex-info "No concurrent playback" {})))
                                    (int 960))})

          output
          (fake-interface SourceDataLine
                          calls
                          {"write" (fn [& _]
                                     (when-not (deref reading 1000 false)
                                       (throw (ex-info "No concurrent capture" {})))
                                     (reset! running false)
                                     (deliver played true)
                                     (int 960))
                           "getLongFramePosition" (fn []
                                                    (long 120))})

          r
          {:running running
           :input (atom input)
           :output (atom output)
           :muted (atom false)
           :scheduled (atom 0)
           :model (atom (assoc-in (model/initial 0 0) [:segments 1] {:received 480 :scheduled 0}))
           :send (ArrayBlockingQueue. 100)
           :play (ArrayBlockingQueue. 5)}

          _
          (.offer
            ^ArrayBlockingQueue (:play r)
            {:generation 0 :stream-id 1 :sample-offset 0 :sample-count 480 :pcm (byte-array 960)})

          capture
          (future (#'live/capture-loop! r))

          playback
          (future (#'live/playback-loop! r))]

      (try (expect (nil? (deref capture 2000 :timeout)))
           (expect (nil? (deref playback 2000 :timeout)))
           (let [receipt (.poll ^ArrayBlockingQueue (:send r))]
             (expect (= 120 (:played receipt))))
           (expect (not-any? #{"close" "stop"} @calls))
           (finally (reset! running false) (future-cancel capture) (future-cancel playback)))))
  (it "does not treat a muted read as device loss"
      (let [calls
            (atom [])

            running
            (atom true)

            input
            (atom nil)

            line
            (fake-interface TargetDataLine
                            calls
                            {"read" (fn [& _]
                                      (reset! input nil)
                                      (reset! running false)
                                      (int 0))})

            r
            {:running running :input input :muted (atom true)}]

        (reset! input line)
        (expect (nil? (#'live/capture-loop! r))))))

(defdescribe live-media-address-test
             (it "uses the configured secure gateway without an authentication query"
                 (with-redefs-fn {#'vis/target! (constantly {:base-url
                                                             "https://gateway.example.com/prefix"})}
                   (fn []
                     (expect (= "wss://gateway.example.com/prefix/v1/voice/media"
                                (str (vis/live-voice-uri "/v1/voice/media"))))
                     (expect (fails? #(vis/live-voice-uri "https://other.example.com/v1/media")))
                     (expect (fails? #(vis/live-voice-uri "/v1/media?ticket=secret")))
                     (expect (fails? #(vis/live-voice-uri "/v1/media#secret")))))))

(defdescribe
  live-control-presentation-test
  (it
    "shows independent capture, speech and authenticated input state"
    (let [state {"capture" "live" "output" "playing" "agent" "waiting_for_input"}]
      (expect
        (=
          "Hands-free · headset only · Mic: live · Speech: playing · Task: waiting for input (use task controls) · /hands-free end"
          (model/status-text state false)))
      (expect
        (=
          "Hands-free · headset only · Mic: muted · Speech: playing · Task: waiting for input (use task controls) · /hands-free end"
          (model/status-text state true)))
      (expect
        (=
          "Hands-free · headset only · Mic: live · Speech: playing · Task: cancelling · /hands-free end"
          (model/status-text (assoc state "agent" "cancelling") false)))))
  (it "reports cancellation results without sending controls or changing task state"
      (doseq [[status text] [["requested"
                              "Task cancellation requested. Wait for task confirmation."]]]
        (let [notices (atom [])
              before (assoc (model/initial 0 0) :server-state {"agent" "running"})
              r {:model (atom before) :send (ArrayBlockingQueue. 10)}
              event {"type" "task.cancellation" "status" status "message" "Control result"}]

          (expect (protocol/valid? "server_event" event))
          (with-redefs [vis/notify! (fn [& args]
                                      (swap! notices conj args))]
            (#'live/server-event! r event))
          (expect (= [[text :level :info]] @notices))
          (expect (= before @(:model r)))
          (expect (zero? (.size ^ArrayBlockingQueue (:send r))))))))

(defdescribe live-identity-test
             (it "rejects unowned speech before it reaches playback"
                 (let [r
                       {:voice-id "00000000-0000-0000-0000-000000000001"
                        :model (atom (model/initial 0 0))}

                       event
                       {"type" "speech.committed"
                        "output_epoch" 0
                        "segment_id" 1
                        "turn_id" "00000000-0000-0000-0000-000000000002"
                        "text" "Answer"
                        "source" "final"}]

                   (expect (fails? #(#'live/server-event! r event)))
                   (expect (empty? (:segments @(:model r)))))))

(defdescribe
  live-overload-test
  (it
    "pauses overloaded input without cancelling output or queued tasks"
    (let [notices
          (atom [])

          r
          {:model (atom (assoc (model/initial 1 1) :segments {7 {:received 480}}))
           :muted (atom false)
           :input (atom nil)
           :running (atom true)
           :voice-id "00000000-0000-0000-0000-000000000001"
           :send (ArrayBlockingQueue. 10)}]

      (.add ^ArrayBlockingQueue (:send r) (byte-array 4))
      (.add ^ArrayBlockingQueue (:send r) {:type "utterance.end" :utterance_id 1})
      (.add ^ArrayBlockingQueue (:send r) {:type "output.played"})
      (with-redefs [vis/notify! (fn [& args]
                                  (swap! notices conj args))]
        (#'live/server-event!
         r
         {"type" "error"
          "code" "input_overflow"
          "message" "Input paused. Unmute to retry."
          "fatal" false}))
      (with-redefs [live/open-input!
                    #(throw (AssertionError. "No automatic capture restart"))

                    vis/publish-channel-event!
                    (fn [& _])]

        (doseq [capture ["muted" "live"]]
          (#'live/server-event!
           r
           {"type" "session.state"
            "state" {"voice_session_id" (:voice-id r)
                     "capture_generation" 2
                     "output_epoch" 1
                     "capture" capture
                     "utterance" "rejected"
                     "agent" "running"
                     "output" "playing"}})))
      (expect @(:muted r))
      (expect (= {7 {:received 480}} (:segments @(:model r))))
      (expect (= [{:type "output.played"}] (vec (:send r))))
      (expect (= [["Input paused. Unmute to retry." :level :warn]] @notices)))))

(defdescribe
  live-recovery-test
  (it "resolves only rejected IDs and recovers durable rejection receipts"
      (let [s
            (-> (model/initial 1 1)
                (model/ended 1)
                (model/ended 2))

            event
            {"type" "utterance.rejected" "utterance_id" 1 "reason" "input_overflow"}

            snapshot
            {"state" {"voice_session_id" "voice"}
             "receipts" [{"voice_session_id" "voice"
                          "utterance_id" 2
                          "idempotency_key" "voice/voice/2"
                          "error" "input_overflow"}]}]

        (expect (protocol/valid? "server_event" event))
        (expect (= {:confirmed 0 :unknown 1} (model/recovery (model/rejected s event) "voice" nil)))
        (expect (= {:confirmed 0 :unknown 0}
                   (model/recovery (model/rejected s event) "voice" snapshot)))
        (expect (= {:confirmed 0 :unknown 2} (model/recovery s "other" snapshot)))))
  (it "retains uncertainty after a successful but partial snapshot"
      (let [vid
            "00000000-0000-0000-0000-000000000001"

            s
            (-> (model/initial 0 0)
                (model/ended 1)
                (model/ended 2))

            receipt
            {"voice_session_id" vid
             "utterance_id" 1
             "idempotency_key" (str "voice/" vid "/1")
             "turn_id" "task"}

            snapshot
            {"state" {"voice_session_id" vid} "receipts" [receipt]}]

        (expect (= {:confirmed 1 :unknown 1} (model/recovery s vid snapshot)))
        (expect (= {:confirmed 0 :unknown 2} (model/recovery s vid nil)))
        (expect (= {:confirmed 0 :unknown 2} (model/recovery s "other" snapshot)))))
  (it "does not count never-ended capture as an uncertain submission"
      (let [[s _] (feed (model/initial 0 0) 10 true)]
        (expect (some? (:utterance s)))
        (expect (= {:confirmed 0 :unknown 0} (model/recovery s "voice" nil)))))
  (it "resolves final spoken controls only after their observed control outcomes"
      (doseq [[text outcome] [["Stop speaking!" "output.interrupted"]
                              ["End voice mode." "session.closed"]]]
        (let [s (-> (model/initial 0 0)
                    (model/ended 1)
                    (model/final-transcript {"utterance_id" 1 "text" text "final" true}))]
          (expect (= 1 (:unknown (model/recovery s "voice" nil))))
          (expect (= {:confirmed 0 :unknown 0}
                     (model/recovery (model/control-outcome s outcome) "voice" nil))))))
  (it "requires matching admission identity and refuses changing its owner"
      (let [vid
            "voice"

            s
            (model/ended (model/initial 0 0) 1)

            admission
            {"utterance_id" 1 "turn_id" "task" "idempotency_key" "voice/voice/1"}

            owned
            (model/admitted s vid admission)]

        (expect (= #{"task"} (:owned-turns owned)))
        (expect (fails? #(model/admitted owned vid (assoc admission "turn_id" "other"))))
        (expect (fails? #(model/admitted s vid (assoc admission "utterance_id" 2))))
        (expect (fails? #(model/admitted s "other" admission)))))
  (it "rejects another voice session before opening a device"
      (let [r
            {:voice-id "00000000-0000-0000-0000-000000000001" :model (atom (model/initial 0 0))}

            state
            {"voice_session_id" "00000000-0000-0000-0000-000000000002"
             "capture_generation" 0
             "output_epoch" 0
             "capture" "live"
             "utterance" "idle"
             "agent" "idle"
             "output" "idle"}]

        (with-redefs [live/open-input! #(throw (AssertionError. "No device access"))]
          (expect (fails? #(#'live/server-event! r {"type" "session.state" "state" state}))))))
  (it
    "fetches recovery for display only and warns when the snapshot is unavailable"
    (let [calls
          (atom [])

          notices
          (atom [])

          r
          {:path "/snapshot" :voice-id "voice" :model (atom (model/ended (model/initial 0 0) 1))}]

      (with-redefs [vis/request!
                    (fn [method path _]
                      (swap! calls conj [method path])
                      (throw (ex-info "Offline" {})))

                    vis/notify!
                    (fn [& args]
                      (swap! notices conj args))]

        (#'live/reconcile! r))
      (expect (= [[:get "/snapshot"]] @calls))
      (expect (= [(model/recovery-text {:confirmed 0 :unknown 1}) :level :warn]
                 (first @notices))))))
