(ns com.blockether.vis.internal.gateway.voice
  "Own live voice leases, bounded workers and one-use media tickets. Store no raw audio."
  (:require [com.blockether.vis.contract.voice :as contract]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.internal.gateway.state :as state]
            [com.blockether.vis.internal.gateway.voice-session :as coordinator]
            [com.blockether.vis.internal.util :as util]
            [ring.websocket :as ws]
            [taoensso.telemere :as tel])
  (:import [java.nio ByteBuffer]
           [java.security MessageDigest]
           [java.util Arrays UUID]
           [java.util.concurrent ArrayBlockingQueue CountDownLatch TimeUnit]))

(set! *warn-on-reflection* true)

(defonce ^:private sessions (atom {}))

(defonce ^:private registry-lock (Object.))

(def ^:dynamic *engine-api*
  "Tests can supply engines without native models. Each worker captures this map at creation."
  nil)

(defn- engine-api
  []
  (or *engine-api*
      (into {}
            (map (fn [name]
                   [name
                    (requiring-resolve (symbol "com.blockether.vis.internal.speech.live"
                                               (str name)))]))
            '[capabilities prepare! open-asr! feed-asr! finish-asr! abort-asr! asr-status
              synthesize-stream!])))

(defn capabilities
  "Read readiness without downloading assets or changing the selected batch engines."
  []
  (let [engines ((get (engine-api) 'capabilities))]
    ;; Released clients read `enabled` before `available`. Live voice has no switch.
    {:enabled true
     :available (boolean (:available engines))
     :reason (:reason engines)
     :limits contract/limits
     :engines engines
     :profile {:language "en"
               :privacy "local_speech"
               :concurrency 1
               :clients ["companion_desktop" "companion_ios_test" "companion_android_test"
                         "tui_headset"]
               :mobile-qualified false
               :playback-precision "rendered_minus_latency"}}))

(defn- monotonic-ms [] (quot (System/nanoTime) 1000000))

(defn- digest ^bytes [^String value] (util/sha256 (util/utf8 value)))

(defn- ticket [] (util/base64url (util/random-bytes 32)))

(defn- enqueue! [^ArrayBlockingQueue queue value] (.offer queue value))

(defn- running? [runtime] (not (:closed @(:state (:coordinator runtime)))))

(defn- workers-alive? [runtime] (some #(.isAlive ^Thread %) @(:threads runtime)))

(defn runtime-for
  "Resolve an exact session and voice pair, including retained reconciliation snapshots."
  [sid voice-id]
  (let [runtime (get @sessions voice-id)]
    (when (= (str sid) (:sid runtime)) runtime)))

(defn snapshot
  [runtime]
  {:state (coordinator/snapshot (:coordinator runtime))
   :receipts (coordinator/receipts (:coordinator runtime))})

(defn snapshot-for
  "Recover committed receipts after restart. Never restore media or admit a request."
  [sid voice-id]
  (if-let [runtime (runtime-for sid voice-id)]
    (snapshot runtime)
    (when-let [receipts (seq (state/voice-receipts sid voice-id))]
      {:state {:voice-session-id voice-id
               :capture-generation 0
               :output-epoch 0
               :capture "off"
               :utterance "admitted"
               :agent "idle"
               :output "idle"}
       :receipts (mapv wire/->engine receipts)})))

(defn close! [runtime reason] (coordinator/close! (:coordinator runtime) reason))

(defn- worker!
  [runtime f]
  (let [thread (Thread/startVirtualThread
                 (fn []
                   (try (f)
                        (catch InterruptedException _ nil)
                        (catch Throwable _
                          (coordinator/fail!
                            (:coordinator runtime)
                            "speech_failed"
                            "Live speech failed. The text conversation is still available.")))))]
    (swap! (:threads runtime) conj thread)
    thread))

(defn- enqueue-input!
  [runtime [operation _ generation :as command]]
  ;; Snapshot occupancy under the producer lock. Polling only releases capacity.
  ;; This avoids counter underflow when abort clears the queue during a native call.
  (let [^ArrayBlockingQueue queue
        (:input runtime)

        result
        (locking queue
          (let [commands
                (seq (.toArray queue))

                frames
                (count (filter #(= :feed (first %)) commands))

                lifecycle
                (- (count commands) frames)

                audio-limit
                (quot (:input-buffer-ms contract/limits) (:frame-ms contract/limits))]

            (if (and (if (= :feed operation) (< frames audio-limit) (< lifecycle 6))
                     (.offer queue command))
              true
              {:queue-depth (count commands)
               :buffered-ms (* frames (:frame-ms contract/limits))
               :audio-frames frames})))]

    (if (true? result)
      true
      (let [{active :operation started :started-at}
            @(:input-active runtime)

            payload
            (assoc result
              :voice-session-id (:id runtime)
              :capture-generation generation
              :operation (name operation)
              :active-operation (some-> active
                                        name)
              :active-duration-ms (if started (- (monotonic-ms) (long started)) 0))]

        ;; The journal drops this event at the next turn start. The log keeps the cause.
        (tel/log! {:level :warn :id ::input-overflow :data payload})
        (state/append-event! (:sid runtime) "voice.input_overflow" payload)
        false))))

(defn- clear-input! [^ArrayBlockingQueue queue] (locking queue (.clear queue)))

(defn- input-worker!
  [runtime]
  (let [api
        (:engines runtime)

        coordinator
        (:coordinator runtime)

        ^ArrayBlockingQueue queue
        (:input runtime)

        handles
        (:input-handles runtime)

        current?
        (fn [generation]
          (and (running? runtime)
               (= generation (:capture-generation (coordinator/snapshot coordinator)))))]

    (worker!
      runtime
      (fn []
        (try
          (while (running? runtime)
            (when-let [[operation id generation value] (.poll queue 200 TimeUnit/MILLISECONDS)]
              (when (current? generation)
                (reset! (:input-active runtime) {:operation operation :started-at (monotonic-ms)})
                (try
                  (case operation
                    :open
                    (let [handle ((get api 'open-asr!)
                                   {:max-samples (quot (* (long (:max-utterance-ms contract/limits))
                                                          (long (:sample-rate contract/limits)))
                                                       1000)})]
                      (swap! handles assoc id handle)
                      (when-not (current? generation)
                        ((get api 'abort-asr!) handle)
                        (swap! handles dissoc id)))

                    :feed
                    (when-let [handle (get @handles id)]
                      (when-let [partial ((get api 'feed-asr!)
                                           handle
                                           (contract/pcm->samples (:pcm value))
                                           (:sample-offset value))]
                        (when (current? generation) (coordinator/partial! coordinator id partial))))

                    :finish
                    (when-let [handle (get @handles id)]
                      (try (let [result ((get api 'finish-asr!) handle)]
                             (when (current? generation)
                               (coordinator/final! coordinator id result)))
                           (finally (swap! handles dissoc id)))))
                  (catch Throwable error (when (current? generation) (throw error)))
                  (finally (reset! (:input-active runtime) nil))))))
          (finally (doseq [handle (vals @handles)]
                     ((get api 'abort-asr!) handle))
                   (reset! handles {})
                   (.clear queue)))))))

(defn- emit-pcm!
  [runtime segment ^floats samples sample-rate]
  (when-not (= sample-rate (:sample-rate contract/limits))
    (throw (ex-info "Live speech sample rate mismatch" {:type :voice/sample-rate})))
  (let [coordinator
        (:coordinator runtime)

        ^ArrayBlockingQueue queue
        (:media runtime)]

    (and (coordinator/generated! coordinator (:id segment) (:epoch segment) (alength samples))
         (loop [offset 0]
           (if (>= offset (alength samples))
             true
             (let [end (min (alength samples) (+ offset (long (:frame-samples contract/limits))))
                   header (coordinator/reserve-output! coordinator
                                                       (:id segment)
                                                       (:epoch segment)
                                                       (- end offset))]

               (if header
                 (let [pcm (contract/samples->pcm
                             (Arrays/copyOfRange samples (int offset) (int end)))]
                   (if (.offer queue
                               [(:epoch segment) (contract/encode-frame header pcm)]
                               100
                               TimeUnit/MILLISECONDS)
                     (recur end)
                     (coordinator/fail! coordinator
                                        "output_overflow"
                                        "The playback connection is too slow")))
                 false)))))))

(defn- output-worker!
  [runtime]
  (let [coordinator
        (:coordinator runtime)

        ^ArrayBlockingQueue queue
        (:output runtime)

        abort
        (:output-abort runtime)

        active
        (:output-active runtime)]

    (worker!
      runtime
      (fn []
        (while (running? runtime)
          (when-let [segment (.poll queue 200 TimeUnit/MILLISECONDS)]
            (when #_{:clj-kondo/ignore [:locking-suspicious-lock]}
              (locking (:lock coordinator)
                (when (coordinator/output-current? coordinator (:epoch segment))
                  (reset! active (:epoch segment))
                  true))
              (try ((get (:engines runtime) 'synthesize-stream!)
                     {:text (:text segment)
                      :voice-id (:pocket-voice-id runtime)
                      :on-pcm (fn [samples rate]
                                (emit-pcm! runtime segment samples rate))
                      :cancelled? #(not (coordinator/output-current? coordinator (:epoch segment)))
                      :register-abort!
                      (fn [f]
                        (reset! abort f)
                        (when-not (coordinator/output-current? coordinator (:epoch segment)) (f)))})
                   (coordinator/output-finished! coordinator (:id segment) (:epoch segment))
                   (finally #_{:clj-kondo/ignore [:locking-suspicious-lock]}
                            (locking (:lock coordinator)
                              (reset! abort nil)
                              (reset! active nil)
                              (when-not (coordinator/output-current? coordinator (:epoch segment))
                                (coordinator/abort-acknowledged!
                                  coordinator
                                  (:output-epoch (coordinator/snapshot coordinator))))))))))))))

(defn- send-message!
  [socket value]
  (let [done
        (CountDownLatch. 1)

        failed
        (atom false)

        message
        (if (bytes? value) (ByteBuffer/wrap ^bytes value) (wire/json-str value))]

    (ws/send socket
             message
             #(.countDown done)
             (fn [_]
               (reset! failed true)
               (.countDown done)))
    (and (.await done 2 TimeUnit/SECONDS) (not @failed))))

(defn- writer!
  [runtime socket]
  (let [^ArrayBlockingQueue controls
        (:controls runtime)

        ^ArrayBlockingQueue media
        (:media runtime)]

    (worker!
      runtime
      (fn []
        (try (while (or (running? runtime) (not (.isEmpty controls)))
               (when-let [[epoch value] (or (.poll controls)
                                            (.poll media 50 TimeUnit/MILLISECONDS))]
                 (when (or (nil? epoch) (coordinator/output-current? (:coordinator runtime) epoch))
                   (when-not (send-message! socket value)
                     (close! runtime "connection_stalled")
                     (.clear controls)))))
             (finally (let [reason (or (:close-reason @(:state (:coordinator runtime)))
                                       "connection_failed")]
                        (ws/close socket (if (= reason "user_ended") 1000 1011) reason))))))))

(defn- task-event!
  [runtime event]
  (let [coordinator
        (:coordinator runtime)

        type
        (get event "type")

        turn-id
        (get event "turn_id")]

    (case type
      "turn.started"
      (coordinator/task-state! coordinator turn-id "running")

      "turn.input"
      (coordinator/task-steered! coordinator
                                 turn-id
                                 (map #(get % "queued_turn_id") (get event "messages"))
                                 (get event "iteration"))

      "view.open"
      (when (= "input" (get event "kind"))
        (coordinator/task-input! coordinator turn-id (get event "view_id") true))

      "view.close"
      (when (= "input" (get event "kind"))
        (coordinator/task-input! coordinator nil (get event "view_id") false))

      "speech.committed"
      (coordinator/commit-speech! coordinator
                                  {:turn-id turn-id
                                   :text (get event "text")
                                   :source (get event "source")
                                   :iteration (get event "iteration")})

      "speech.invalidated"
      (coordinator/interrupt-turn! coordinator turn-id "projection_changed")

      "turn.completed"
      (let [blocks
            (get event "content")

            conversations
            (filter #(= "conversation" (get % "type")) blocks)

            answer?
            (some #(= "answer" (get % "phase")) conversations)

            silent?
            (and (empty? conversations)
                 (some #(and (= "silence" (get % "type")) (= "user_requested" (get % "reason")))
                       blocks))

            unavailable?
            (some #(= "presentation_unavailable" (get % "code")) blocks)]

        ;; Stable record sources deduplicate final delivery against streamed commits.
        ;; Progress never satisfies the final conversational outcome.
        (doseq [block conversations]
          (coordinator/commit-speech! coordinator
                                      {:turn-id turn-id
                                       :text (get block "text")
                                       :source (str "presentation:" (get block "id"))}))
        (when (or unavailable? (and (not answer?) (not silent?)))
          (coordinator/commit-speech!
            coordinator
            {:turn-id turn-id
             :text "The spoken reply is unavailable. Please check the text answer."
             :source "final:missing-speech"}))
        (coordinator/task-state! coordinator turn-id "terminal"))

      ("turn.failed" "turn.cancelled")
      (do (coordinator/interrupt-turn! coordinator turn-id type)
          (coordinator/task-state! coordinator turn-id "terminal"))

      nil)))

(defn- subscribe!
  [runtime]
  (let [^ArrayBlockingQueue queue
        (:events runtime)

        sid
        (UUID/fromString (:sid runtime))

        subscriber
        (:id runtime)

        overflow
        (atom false)]

    (state/subscribe! sid
                      subscriber
                      (fn [event]
                        (let [type
                              (get event "type")

                              event
                              (if (contains? #{"view.open" "view.close"} type)
                                (assoc (select-keys event ["type" "kind" "view_id"])
                                  "turn_id" (when (= (:sid runtime)
                                                     (get-in event ["view" "origin" "session_id"]))
                                              (state/current-turn-id sid)))
                                event)]

                          (when (contains? #{"turn.started" "turn.completed" "turn.failed"
                                             "turn.cancelled" "turn.input" "speech.committed"
                                             "speech.invalidated" "view.open" "view.close"}
                                           type)
                            ;; This sink runs under the session event lock, and a close
                            ;; takes the coordinator lock. Only record the overflow here.
                            (when-not (enqueue! queue event) (reset! overflow true)))))
                      Long/MAX_VALUE)
    (worker! runtime
             (fn []
               (try (while (running? runtime)
                      (if @overflow
                        (close! runtime "event_overflow")
                        (when-let [event (.poll queue 200 TimeUnit/MILLISECONDS)]
                          (task-event! runtime event))))
                    (finally (state/unsubscribe! sid subscriber)))))))

(defn- make-runtime
  [sid body origin capabilities]
  (let [id
        (str (UUID/randomUUID))

        raw-ticket
        (ticket)

        api
        (engine-api)

        input
        (ArrayBlockingQueue.
          (int (+ 6 (quot (:input-buffer-ms contract/limits) (:frame-ms contract/limits)))))

        output
        (ArrayBlockingQueue. (int (:max-pending-segments contract/limits)))

        controls
        (ArrayBlockingQueue. 64)

        media
        (ArrayBlockingQueue.
          (int (+ 2 (quot (:output-credit-ms contract/limits) (:frame-ms contract/limits)))))

        handles
        (atom {})

        abort
        (atom nil)

        active
        (atom nil)

        socket
        (atom nil)

        holder
        (atom nil)

        now
        (monotonic-ms)

        enqueue-control
        (fn [event]
          (let [queue
                (if (= "output.finished" (:type event)) media controls)

                epoch
                (when (= "output.finished" (:type event)) (:output-epoch event))]

            (when-not (enqueue! queue [epoch event])
              (.clear queue)
              (coordinator/fail! (:coordinator @holder)
                                 "control_overflow"
                                 "The voice connection is too slow"))))

        coordinator
        (coordinator/create
          {:voice-session-id id
           :client-id (:client-id body)
           :muted (:muted body)
           :now monotonic-ms
           :emit enqueue-control
           :persist #(state/append-event! sid %1 %2)
           :admit (fn [request]
                    (let [result
                          (state/submit-turn! sid
                                              (assoc request
                                                :turn-features {"voice_projection" true
                                                                "voice_segments" true}))

                          turn
                          (wire/->engine (:turn result))]

                      ;; Steer only this new voice row. Never send another queued request.
                      (when (and (= "queued" (:status turn))
                                 (coordinator/owns-running-task? (:coordinator @holder)
                                                                 (state/current-turn-id sid)))
                        (state/update-queued-turn! sid (:turn-id turn) {:deliver "next_iteration"}))
                      (if turn
                        {:turn-id (:turn-id turn)
                         ;; The turn API calls active work streaming. Voice calls it running.
                         :status (case (:status turn)
                                   "streaming"
                                   "running"

                                   (:status turn))}
                        {:error (name (or (:error result) :admission-failed))})))
           :cancel-task (fn [turn-id]
                          (let [result (state/cancel-turn! sid turn-id :voice-explicit-cancel)]
                            (when (:error result)
                              (enqueue-control
                                {:type "error"
                                 :code "cancel_rejected"
                                 :message
                                 "This task cannot be cancelled here. Use the task controls."
                                 :fatal false}))
                            result))
           :open-input (fn [utterance-id generation]
                         (enqueue-input! @holder [:open utterance-id generation nil]))
           :feed-input (fn [utterance-id frame]
                         (enqueue-input! @holder [:feed utterance-id (:generation frame) frame]))
           :finish-input (fn [utterance-id]
                           (enqueue-input! @holder
                                           [:finish utterance-id
                                            (:capture-generation (coordinator/snapshot (:coordinator
                                                                                         @holder)))
                                            nil]))
           :abort-input (fn []
                          (clear-input! input)
                          (let [previous (vals @handles)]
                            (reset! handles {})
                            (doseq [handle previous]
                              ((get api 'abort-asr!) handle))))
           :speak #(enqueue! output %)
           :abort-output (fn []
                           (.clear output)
                           (.clear media)
                           (if (some? @active)
                             (when-let [f @abort]
                               (f))
                             (let [session (:coordinator @holder)]
                               (coordinator/abort-acknowledged! session
                                                                (:output-epoch (coordinator/snapshot
                                                                                 session))))))
           :close (fn [_]
                    (clear-input! input)
                    (.clear output)
                    (.clear media)
                    nil)})

        runtime
        {:id id
         :sid (str sid)
         :origin origin
         :coordinator coordinator
         :pocket-voice-id (:pocket-voice-id body)
         :engines api
         :capabilities capabilities
         :created-at now
         :ticket-hash (digest raw-ticket)
         :ticket-until (+ now (long (:ticket-ms contract/limits)))
         :claimed (atom false)
         :input input
         :input-active (atom nil)
         :input-handles handles
         :output output
         :output-abort abort
         :output-active active
         :controls controls
         :media media
         :events (ArrayBlockingQueue. 32)
         :socket socket
         :threads (atom [])
         :ready (atom false)}]

    (reset! holder runtime)
    [runtime raw-ticket]))

(defn start!
  "Create one media lease after ordinary gateway authentication. Models must already be present."
  [sid body origin]
  (locking registry-lock
    (let [capabilities
          (capabilities)

          now
          (monotonic-ms)]

      (doseq [runtime (vals @sessions)]
        (coordinator/reap! (:coordinator runtime)))
      (swap! sessions (fn [all]
                        (into {}
                              (filter (fn [[_ runtime]]
                                        (or (running? runtime)
                                            (workers-alive? runtime)
                                            (< (- now
                                                  (long (:closed-at @(:state (:coordinator
                                                                               runtime)))))
                                               (long (:retention-ms contract/limits))))))
                              all)))
      (cond (not (contract/valid? "start" body)) {:status 400 :error "invalid_request"}
            (not (:available capabilities)) {:status 425 :error "engines_not_ready"}
            (some #(or (running? %) (workers-alive? %)) (vals @sessions)) {:status 409
                                                                           :error "voice_busy"}
            (>= (count @sessions) (long (:max-retained-sessions contract/limits)))
            {:status 429 :error "voice_session_limit"}
            :else (let [[runtime raw-ticket] (make-runtime sid body origin capabilities)]
                    (swap! sessions assoc (:id runtime) runtime)
                    (worker! runtime
                             (fn []
                               (while (running? runtime)
                                 (Thread/sleep 500)
                                 (coordinator/reap! (:coordinator runtime)))))
                    {:status 201
                     :data {:voice-session-id (:id runtime)
                            :client-id (:client-id body)
                            :media-path
                            (str "/v1/sessions/" sid "/voice/live/" (:id runtime) "/media")
                            :ticket raw-ticket
                            :state (coordinator/snapshot (:coordinator runtime))
                            :limits contract/limits
                            :capabilities capabilities}})))))

(defn claim!
  "Consume an origin-bound ticket once. Never accept a ticket for a different session."
  [sid voice-id raw-ticket origin]
  (locking registry-lock
    (when-let [runtime (runtime-for sid voice-id)]
      (when (and (string? raw-ticket)
                 (<= (count raw-ticket) 80)
                 (running? runtime)
                 (not (coordinator/expired? (:coordinator runtime)))
                 (< (monotonic-ms) (long (:ticket-until runtime)))
                 (= origin (:origin runtime))
                 (MessageDigest/isEqual ^bytes (:ticket-hash runtime) (digest raw-ticket))
                 (compare-and-set! (:claimed runtime) false true))
        runtime))))

(defn open-socket!
  [runtime socket]
  (reset! (:socket runtime) socket)
  (writer! runtime socket)
  (worker! runtime
           (fn []
             (let [prepared
                   ((get (:engines runtime) 'prepare!) {:cancelled? #(not (running? runtime))})

                   coordinator
                   (:coordinator runtime)]

               #_{:clj-kondo/ignore [:locking-suspicious-lock]}
               (locking (:lock coordinator)
                 (when (running? runtime)
                   (if (:ready prepared)
                     (do (input-worker! runtime)
                         (output-worker! runtime)
                         (subscribe! runtime)
                         (reset! (:ready runtime) true)
                         (when-not (enqueue! (:controls runtime)
                                             [nil
                                              {:type "session.ready"
                                               :state (coordinator/snapshot coordinator)
                                               :capabilities (:capabilities runtime)
                                               :limits contract/limits}])
                           (coordinator/fail! coordinator
                                              "control_overflow"
                                              "The voice connection is too slow")))
                     (coordinator/fail! coordinator
                                        "preparation_failed"
                                        "Local speech is not ready"))))))))

(defn message!
  "Validate bounded control or binary data before native work is enqueued."
  [runtime value]
  (try
    (let [coordinator (:coordinator runtime)]
      (if (string? value)
        (if (> (alength (util/utf8 value)) (long (:max-control-bytes contract/limits)))
          (coordinator/fail! coordinator
                             "control_too_large"
                             "The voice control exceeds the size limit")
          (let [control (wire/->engine (wire/parse-json value))]
            (if (and (not @(:ready runtime))
                     (contains? #{"utterance.start" "utterance.end"} (:type control)))
              (coordinator/fail! coordinator "input_not_ready" "Wait for local speech preparation")
              (coordinator/control! coordinator control))))
        (if @(:ready runtime)
          (coordinator/input-frame! coordinator (contract/decode-frame value))
          (coordinator/fail! coordinator "input_not_ready" "Wait for local speech preparation"))))
    (catch Throwable _
      (coordinator/fail! (:coordinator runtime) "invalid_media" "The voice message is invalid"))))

(defn shutdown!
  "Stop temporary or gateway-owned voice machinery without touching agent tasks."
  []
  (doseq [runtime (vals @sessions)]
    (close! runtime "gateway_stopped"))
  (doseq [runtime
          (vals @sessions)

          ^Thread thread
          @(:threads runtime)]

    (.join thread 1000))
  (swap! sessions #(into {}
                         (filter (fn [[_ runtime]]
                                   (workers-alive? runtime)))
                         %))
  nil)
