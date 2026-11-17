(ns com.blockether.vis.tui.voice-live
  "Experimental local speech over authenticated media. Headsets are required; Java Sound has no AEC."
  (:require [clojure.string :as str]
            [com.blockether.vis.contract.voice :as protocol]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.tui.client :as vis]
            [com.blockether.vis.tui.voice-live-model :as model]
            [com.blockether.vis.tui.voice-input :as dictation]
            [com.blockether.vis.tui.voice-output :as manual-output])
  (:import [java.net.http HttpClient WebSocket WebSocket$Listener]
           [java.nio ByteBuffer]
           [java.time Duration]
           [java.util UUID]
           [java.util.concurrent ArrayBlockingQueue TimeUnit]
           [javax.sound.sampled AudioFormat AudioSystem DataLine$Info SourceDataLine
            TargetDataLine]))

(set! *warn-on-reflection* true)

(def ^:private frame-bytes (* 2 (long (:frame-samples protocol/limits))))

(def ^:private media-bytes (+ frame-bytes (long (:header-bytes protocol/limits))))

(def ^:private playback-frames (max 1 (quot 100 (long (:frame-ms protocol/limits)))))

(def ^:private input-frames
  (quot (long (:input-buffer-ms protocol/limits)) (long (:frame-ms protocol/limits))))

(defonce ^:private active (atom nil))

(defn active? [] (boolean @active))

(defn- notice!
  [text]
  (vis/publish-channel-event! :tui {:op :status/set :id :voice/live :text text :level :warn}))

(defn- queue!
  [^ArrayBlockingQueue queue item]
  (when-not (.offer queue item) (throw (ex-info "Live voice queue full; restart voice mode" {}))))

(defn- control!
  [r message]
  (when-not (protocol/valid? "client_control" message)
    (throw (ex-info "Invalid live voice control" {})))
  (when (= "utterance.end" (:type message)) (swap! (:model r) model/ended (:utterance_id message)))
  (queue! (:send r) message))

(defn- close-input!
  [r]
  (when-let [^TargetDataLine line @(:input r)]
    (reset! (:input r) nil)
    (.stop line)
    (.flush line)
    (.close line)))

(defn- silence!
  [r]
  (locking r
    (.clear ^ArrayBlockingQueue (:play r))
    (when-let [^SourceDataLine line @(:output r)]
      (.stop line)
      (.flush line)
      (reset! (:scheduled r) (.getLongFramePosition line)))
    (swap! (:model r) assoc :blocked-output? true :segments {})))

(defn- reconcile!
  [r]
  (let [snapshot
        (try (let [response (vis/request! :get (:path r) {:timeout-ms 3000})]
               (when (= 200 (:status response)) (wire/parse-json (:body response))))
             (catch Throwable _ nil))

        result
        (model/recovery @(:model r) (:voice-id r) snapshot)]

    (vis/notify! (model/recovery-text result) :level (if (pos? (:unknown result)) :warn :info))))

(defn stop!
  "Close local devices immediately. Ending media never cancels a task."
  []
  (when-let [r @active]
    (when (compare-and-set! active r nil)
      (reset! (:running r) false)
      (remove-watch (:db r) (:watch-key r))
      (try (close-input! r) (catch Throwable _ nil))
      (try (silence! r) (catch Throwable _ nil))
      (when-let [^SourceDataLine line @(:output r)]
        (.close line))
      (when-let [^WebSocket socket @(:socket r)]
        (.abort socket))
      (.clear ^ArrayBlockingQueue (:send r))
      (.clear ^ArrayBlockingQueue (:receive r))
      (vis/publish-channel-event! :tui {:op :status/clear :id :voice/live})
      (vis/worker-future "voice-live-end"
                         #(try (reconcile! r)
                               (finally (try (vis/request! :delete (:path r) {:timeout-ms 3000})
                                             (catch Throwable _ nil))))))))

(defn- fail!
  [r]
  (when (identical? r @active)
    (stop!)
    (vis/notify! "Hands-free voice stopped. Check devices and connection, then start again."
                 :level
                 :warn)))

(defn- worker! [r name f] (vis/worker-future name #(try (f) (catch Throwable _ (fail! r)))))

(defn audio-format
  []
  (AudioFormat. (float (:sample-rate protocol/limits))
                16 (int (:channels protocol/frame-format))
                true false))

(defn open-output!
  []
  (let [fmt
        (audio-format)

        ^SourceDataLine line
        (AudioSystem/getLine (DataLine$Info. SourceDataLine fmt))]

    (try (.open line fmt (int (* (long frame-bytes) (long playback-frames))))
         line
         (catch Throwable t (.close line) (throw t)))))

(defn open-input!
  []
  (let [fmt
        (audio-format)

        ^TargetDataLine line
        (AudioSystem/getLine (DataLine$Info. TargetDataLine fmt))]

    (try (.open line fmt (int (* (long frame-bytes) (long playback-frames))))
         (.start line)
         line
         (catch Throwable t (.close line) (throw t)))))

(defn- effects!
  [r effects]
  (doseq [[op header pcm] effects]
    (case op
      :stop-output
      (silence! r)

      :control
      (control! r header)

      :frame
      (queue! (:send r) (protocol/encode-frame header pcm)))))

(defn- capture-loop!
  [r]
  (while @(:running r)
    (if-let [^TargetDataLine line @(:input r)]
      (let [pcm (byte-array frame-bytes)
            n (try (.read line pcm 0 (int frame-bytes))
                   (catch Exception t (if (identical? line @(:input r)) (throw t) 0)))]

        (when (and (identical? line @(:input r)) (not= n frame-bytes))
          (throw (ex-info "Capture route lost" {})))
        (locking r
          (when (and (identical? line @(:input r)) (not @(:muted r)))
            (let [[next-state effects] (model/capture @(:model r) pcm (model/speech? pcm))]
              (reset! (:model r) next-state)
              (effects! r effects)))))
      (Thread/sleep (long (:frame-ms protocol/limits))))))

(defn- state!
  [r state]
  (locking r
    (when-not (= (:voice-id r) (get state "voice_session_id"))
      (throw (ex-info "Voice session identity mismatch" {})))
    (let [generation
          (get state "capture_generation")

          epoch
          (get state "output_epoch")]

      (when (not= epoch (:epoch @(:model r))) (silence! r))
      (when (not= generation (:generation @(:model r)))
        (swap! (:model r) assoc :preroll [] :utterance nil :voiced-ms 0))
      (swap! (:model r) assoc :generation generation :epoch epoch :server-state state)
      (when (not= "live" (get state "capture"))
        (close-input! r)
        (swap! (:model r) assoc :preroll [] :utterance nil :voiced-ms 0))
      (when
        (and @(:running r) (= "live" (get state "capture")) (not @(:muted r)) (nil? @(:input r)))
        (let [^TargetDataLine line (open-input!)]
          (reset! (:input r) line)
          (when-not @(:running r) (close-input! r))))
      (notice! (model/status-text state @(:muted r))))))

(defn- pause-input!
  [r]
  (close-input! r)
  (.removeIf ^ArrayBlockingQueue (:send r)
             (reify
               java.util.function.Predicate
                 (test [_ item]
                   (or (bytes? item)
                       (contains? #{"utterance.start" "utterance.end"} (:type item))))))
  (swap! (:model r) assoc :preroll [] :utterance nil :voiced-ms 0))

(defn- server-event!
  [r event]
  (when-not (protocol/valid? "server_event" event) (throw (ex-info "Invalid live voice event" {})))
  (swap! (:model r) model/control-outcome (get event "type"))
  (case (get event "type")
    "transcript"
    (swap! (:model r) model/final-transcript event)

    "utterance.rejected"
    (swap! (:model r) model/rejected event)

    "utterance.admitted"
    (swap! (:model r) model/admitted (:voice-id r) event)

    ("session.ready" "session.state")
    (state! r (get event "state"))

    "output.interrupted"
    (when (> (long (get event "output_epoch")) (long (:epoch @(:model r))))
      (silence! r)
      (swap! (:model r) assoc :epoch (get event "output_epoch") :blocked-output? false))

    "speech.committed"
    (do (when-not (contains? (:owned-turns @(:model r)) (get event "turn_id"))
          (throw (ex-info "Unowned live speech" {})))
        (when (= (:epoch @(:model r)) (get event "output_epoch"))
          (when (or (get-in @(:model r) [:segments (get event "segment_id")])
                    (>= (count (:segments @(:model r))) (:max-pending-segments protocol/limits)))
            (throw (ex-info "Invalid live speech segment" {})))
          (swap! (:model r) assoc-in
            [:segments (get event "segment_id")]
            {:sequence 0 :received 0 :scheduled 0})))

    "output.finished"
    (when (and (= (:epoch @(:model r)) (get event "output_epoch"))
               (get-in @(:model r) [:segments (get event "segment_id")]))
      (swap! (:model r) assoc-in
        [:segments (get event "segment_id") :finished]
        (get event "sample_count")))

    "task.cancellation"
    (vis/notify! (model/cancellation-text event) :level :info)

    "session.closed"
    (stop!)

    "error"
    (if (get event "fatal")
      (fail! r)
      (do (when (= "input_overflow" (get event "code"))
            (locking r (reset! (:muted r) true) (pause-input! r)))
          (vis/notify! (get event "message") :level :warn)))

    nil))

(defn- receive-loop!
  [r]
  (while @(:running r)
    (when-let [item (.poll ^ArrayBlockingQueue (:receive r) 100 TimeUnit/MILLISECONDS)]
      (if (string? item)
        (locking r (server-event! r (wire/parse-json item)))
        (let [header (protocol/decode-frame item)
              accepted? (locking r
                          (let [[next-state accepted?] (model/accept-output @(:model r) header)]
                            (reset! (:model r) next-state)
                            accepted?))]

          (when (and accepted?
                     (not (.offer ^ArrayBlockingQueue (:play r) header 500 TimeUnit/MILLISECONDS)))
            (throw (ex-info "Playback stalled" {}))))))))

(defn- playback-loop!
  [r]
  (while @(:running r)
    (locking r
      (when-let [^SourceDataLine line @(:output r)]
        (when-let [header (.peek ^ArrayBlockingQueue (:play r))]
          (let [pcm ^bytes (:pcm header)
                id (:stream-id header)]

            (when (>= (.available line) (alength pcm))
              (.poll ^ArrayBlockingQueue (:play r))
              (when (and (= (:generation header) (:epoch @(:model r)))
                         (not (:blocked-output? @(:model r))))
                (swap! (:model r) update-in
                  [:segments id]
                  #(cond-> % (nil? (:base %)) (assoc :base @(:scheduled r))))
                (.start line)
                (let [n (.write line pcm 0 (alength pcm))]
                  (when-not (= n (alength pcm)) (throw (ex-info "Playback route lost" {})))
                  (swap! (:scheduled r) + (quot n 2))
                  (swap! (:model r) update-in [:segments id :scheduled] + (quot n 2)))))))
        (doseq [[id segment] (:segments @(:model r))]
          (let [receipt
                (model/receipt (:epoch @(:model r)) id segment (.getLongFramePosition line))]
            (control! r receipt)
            (when (= (:finished segment) (:played receipt))
              (swap! (:model r) update :segments dissoc id))))))
    (Thread/sleep (long (:frame-ms protocol/limits)))))

(defn- sender-loop!
  [r]
  (while @(:running r)
    (when-let [item (.poll ^ArrayBlockingQueue (:send r) 100 TimeUnit/MILLISECONDS)]
      (let [^WebSocket socket @(:socket r)]
        (.get (if (bytes? item)
                (.sendBinary socket (ByteBuffer/wrap ^bytes item) true)
                (.sendText socket (wire/json-str item) true))
              2
              TimeUnit/SECONDS)))))

(defn connect!
  [r ticket media-path]
  (let [text
        (StringBuilder.)

        binary
        (java.io.ByteArrayOutputStream.)

        listener
        (reify
          WebSocket$Listener
            (onOpen [_ socket] (.request ^WebSocket socket 1))
            (onText [_ socket data last?]
              (try (when (> (+ (.length text) (.length ^CharSequence data))
                            (long (:max-control-bytes protocol/limits)))
                     (throw (ex-info "Voice control too large" {})))
                   (.append text ^CharSequence data)
                   (when last? (queue! (:receive r) (str text)) (.setLength text 0))
                   (.request ^WebSocket socket 1)
                   (catch Throwable _ (fail! r)))
              nil)
            (onBinary [_ socket data last?]
              (try (let [buffer
                         ^ByteBuffer data

                         n
                         (.remaining buffer)]

                     (when (> (+ (.size binary) n) media-bytes)
                       (throw (ex-info "Voice frame too large" {})))
                     (let [bytes (byte-array n)]
                       (.get buffer bytes)
                       (.write binary bytes 0 n)))
                   (when last? (queue! (:receive r) (.toByteArray binary)) (.reset binary))
                   (.request ^WebSocket socket 1)
                   (catch Throwable _ (fail! r)))
              nil)
            (onClose [_ _ _ _] (fail! r) nil)
            (onError [_ _ _] (fail! r)))]

    (.get (.buildAsync (-> (HttpClient/newHttpClient)
                           (.newWebSocketBuilder)
                           (.connectTimeout (Duration/ofSeconds 5))
                           (.subprotocols "vis-voice.v1"
                                          (into-array String [(str "vis-ticket." ticket)])))
                       (vis/live-voice-uri media-path)
                       listener)
          5
          TimeUnit/SECONDS)))

(defn- json-request!
  [method path opts]
  (let [response (vis/request! method path opts)]
    (when-not (<= 200 (:status response) 299)
      (throw (ex-info "Live voice unavailable; check gateway speech readiness" {})))
    (wire/parse-json (:body response))))

(defn start!
  "Start only after an explicit headset assertion and local gateway capability check."
  [db profile]
  (when-not (= profile "headset")
    (throw (ex-info "Use /hands-free start headset. Speakers are unsupported without AEC." {})))
  (when @active (throw (ex-info "Hands-free voice is already active" {})))
  (when (:transcribing? @dictation/state)
    (throw (ex-info "Wait for dictation before starting hands-free voice" {})))
  (let [sid
        (get-in @db [:session :id])

        tab
        (:active-tab-id @db)]

    (when-not sid (throw (ex-info "Open a session before starting hands-free voice" {})))
    (let [caps (json-request! :get "/v1/voice/live/capabilities" {})]
      (when-not (and (true? (get caps "enabled")) (true? (get caps "available")))
        (throw (ex-info "Local live speech is not enabled or ready" {}))))
    (let [base
          (str "/v1/sessions/"
               (java.net.URLEncoder/encode (str sid) java.nio.charset.StandardCharsets/UTF_8)
               "/voice/live")

          answer
          (json-request! :post
                         base
                         {:body {:schema 1
                                 :client_id (str (UUID/randomUUID))
                                 :language "en"
                                 :privacy "local_speech"
                                 :muted false}})

          state
          (get answer "state")

          r
          {:db db
           :voice-id (get answer "voice_session_id")
           :path (str base "/" (get answer "voice_session_id"))
           :running (atom true)
           :muted (atom false)
           :socket (atom nil)
           :input (atom nil)
           :output (atom nil)
           :scheduled (atom 0)
           :model (atom (model/initial (get state "capture_generation") (get state "output_epoch")))
           :watch-key (Object.)
           :send (ArrayBlockingQueue. (int input-frames))
           :receive (ArrayBlockingQueue. (int (+ input-frames
                                                 (:max-pending-segments protocol/limits))))
           :play (ArrayBlockingQueue. (int playback-frames))}]

      (when-not (compare-and-set! active nil r)
        (vis/request! :delete (:path r))
        (throw (ex-info "Hands-free voice already started" {})))
      (try (add-watch db
                      (:watch-key r)
                      (fn [_ _ _ new-db]
                        (when (or (not= tab (:active-tab-id new-db))
                                  (not= sid (get-in new-db [:session :id])))
                          (stop!))))
           (when (or (not= tab (:active-tab-id @db)) (not= sid (get-in @db [:session :id])))
             (throw (ex-info "Session changed during voice start" {})))
           (when (:recorder @dictation/state) (dictation/cancel-recording! {:app-db db}))
           (manual-output/stop!)
           (let [^SourceDataLine line (open-output!)]
             (reset! (:output r) line)
             (when-not @(:running r) (.close line)))
           (reset! (:socket r) (connect! r (get answer "ticket") (get answer "media_path")))
           (when-not @(:running r)
             (when-let [^WebSocket socket @(:socket r)]
               (.abort socket))
             (throw (ex-info "Voice start interrupted" {})))
           (worker! r "voice-live-send" #(sender-loop! r))
           (worker! r "voice-live-receive" #(receive-loop! r))
           (worker! r "voice-live-play" #(playback-loop! r))
           (worker! r "voice-live-capture" #(capture-loop! r))
           (worker! r
                    "voice-live-heartbeat"
                    #(while @(:running r)
                       (control! r {:type "heartbeat"})
                       (Thread/sleep (long (:heartbeat-ms protocol/limits)))))
           (catch Throwable t (when (identical? r @active) (stop!)) (throw t))))))

(defn mute!
  [muted?]
  (when-let [r @active]
    (locking r
      (reset! (:muted r) muted?)
      (pause-input! r)
      (control! r {:type "capture.mute" :muted muted?})
      (notice! (model/status-text (cond-> (:server-state @(:model r))
                                    (not muted?)
                                    (assoc "capture" "waiting for capture"))
                                  muted?)))))

(defn stop-speech!
  []
  (when-let [r @active]
    (locking r
      (silence! r)
      (control!
        r
        {:type "output.interrupt" :output_epoch (:epoch @(:model r)) :reason "stop_speech"}))))

(defn command? [text] (boolean (re-find #"^/hands-free(?:\s|$)" (str/trim text))))

(defn command!
  [db text]
  (let [[_ action profile] (str/split (str/trim text) #"\s+")]
    (if (= action "start")
      (vis/worker-future "voice-live-start"
                         #(try (start! db profile)
                               (catch Throwable t (vis/notify! (ex-message t) :level :warn))))
      (case action
        "mute"
        (mute! true)

        "unmute"
        (mute! false)

        "stop-speech"
        (stop-speech!)

        "end"
        (stop!)

        (vis/notify!
          "Hands-free: start headset | mute | unmute | stop-speech | end. Task cancellation stays separate."
          :level
          :info)))))
