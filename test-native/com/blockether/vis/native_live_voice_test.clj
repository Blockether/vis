(ns com.blockether.vis.native-live-voice-test
  "Exercise media upgrade and local speech preparation in the linked image, without audio devices."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.contract.voice :as contract]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.internal.gateway.client :as gateway-client]
            [com.blockether.vis.internal.speech.assets :as assets]
            [com.blockether.vis.internal.speech.live :as live]
            [com.blockether.vis.internal.speech.sherpa :as sherpa]
            [com.blockether.vis.internal.speech.tts :as tts]
            [com.blockether.vis.native-binary-test :as native]
            [com.blockether.vis.native-speech-startup-test :as startup]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [java.io ByteArrayOutputStream]
           [java.net ServerSocket URI]
           [java.net.http HttpClient WebSocket WebSocket$Listener]
           [java.nio ByteBuffer]
           [java.util UUID]
           [java.util.concurrent CompletableFuture TimeUnit]))

(set! *warn-on-reflection* true)

(defn- request!
  [method path body]
  (let [response (gateway-client/request! method path {:body body :timeout-ms 60000})]
    (when-not (<= 200 (:status response) 299)
      (throw (ex-info "Native voice request failed" {:status (:status response) :path path})))
    (wire/->engine (wire/parse-json (:body response)))))

(defn- await-health!
  [^Process process]
  (loop [remaining 600]
    (cond (not (.isAlive process)) (throw (ex-info "Native gateway exited before readiness" {}))
          (try (= 200 (:status (gateway-client/request! :get "/healthz" {:timeout-ms 1000})))
               (catch Exception _ false))
          true
          (zero? remaining) (throw (ex-info "Native gateway readiness timed out" {}))
          :else (do (Thread/sleep 100) (recur (dec remaining))))))

(defn- await-event!
  [^WebSocket connection ready]
  (loop [remaining 24]
    (let [event (deref ready 5000 :timeout)]
      (cond (not= :timeout event) event
            (zero? remaining) :timeout
            :else (do (.join (.sendText connection (wire/json-str {:type "heartbeat"}) true))
                      (recur (dec remaining)))))))

(defn- input-frames
  []
  (let [chunks
        (atom [])

        generation-config
        @#'tts/pocket-generation-config]

    (when-not (:available (live/capabilities))
      (throw (ex-info "Install live speech assets before native qualification" {})))
    (with-redefs-fn {#'tts/pocket-generation-config (fn [clip text]
                                                      (let [^com.k2fsa.sherpa.onnx.GenerationConfig
                                                            config
                                                            (generation-config clip text)]
                                                        (.put (.getExtra config) "seed" "42")
                                                        config))}
      #(live/synthesize-stream!
         {:text
          "Please explain how the local speech test checks a useful answer. Read these words clearly and keep the answer short."
          :on-pcm (fn [^floats samples rate]
                    (expect (= 24000 rate))
                    (swap! chunks conj (aclone samples))
                    true)}))
    (let [size
          (max 96000 (reduce + (map #(alength ^floats %) @chunks)))

          samples
          (float-array (* 480 (long (Math/ceil (/ (double size) 480.0)))))]

      (loop [chunks
             @chunks

             offset
             0]

        (when-let [^floats chunk (first chunks)]
          (System/arraycopy chunk 0 samples offset (alength chunk))
          (recur (next chunks) (+ offset (alength chunk)))))
      (mapv (fn [offset]
              (contract/encode-frame
                {:generation 0 :stream-id 0 :sample-offset offset}
                (contract/samples->pcm
                  (java.util.Arrays/copyOfRange samples (int offset) (int (+ offset 480))))))
            (range 0 (alength samples) 480)))))

(defn- wait-until!
  [deadline]
  (loop []

    (let [remaining (- (long deadline) (System/nanoTime))]
      (when (pos? remaining)
        (java.util.concurrent.locks.LockSupport/parkNanos remaining)
        (recur)))))

(defn- direct-reference
  [frames]
  (let [trials (mapv (fn [trial]
                       (let [handle (live/open-asr! {})
                             started (System/nanoTime)]

                         (try (doseq [encoded frames]
                                (let [frame (contract/decode-frame encoded)]
                                  (live/feed-asr! handle
                                                  (contract/pcm->samples (:pcm frame))
                                                  (:sample-offset frame))))
                              (let [fed (System/nanoTime)
                                    final (live/finish-asr! handle)
                                    ended (System/nanoTime)
                                    result {:trial trial
                                            :feed-ms (/ (- fed started) 1000000.0)
                                            :finish-ms (/ (- ended fed) 1000000.0)
                                            :text (:text final)}]

                                (println "DIRECT exact PCM" result)
                                result)
                              (finally (live/abort-asr! handle)))))
                     (range 1 4))]
    (expect (apply = (map :text trials)))
    (:text (first trials))))

(defn- process-lines
  [command]
  (let [process (.start (doto (ProcessBuilder. ^java.util.List command)
                          (.redirectErrorStream true)))]
    (try (with-open [reader (io/reader (.getInputStream process))]
           (let [lines (vec (line-seq reader))]
             (when-not (.waitFor process 10 TimeUnit/SECONDS)
               (throw (ex-info "Process metrics timed out" {})))
             (when-not (zero? (.exitValue process))
               (throw (ex-info "Process metrics failed" {:command (first command)})))
             lines))
         (finally (when (.isAlive process) (.destroyForcibly process))))))

(defn- process-metrics
  [^Process process]
  (let [pid (str (.pid process))]
    {:rss-kib (Long/parseLong (str/trim (first (process-lines ["ps" "-o" "rss=" "-p" pid]))))
     :threads (dec (count (process-lines ["ps" "-M" "-p" pid])))
     :fds (count (filter #(re-matches #"f[0-9]+" %)
                         (process-lines ["lsof" "-a" "-p" pid "-Ff"])))}))

(defn- gateway-diagnostics
  [^java.io.File home]
  (-> (->> (str/split-lines (slurp (io/file home "live-gateway.log")))
           (take-last 80)
           (str/join "\n"))
      (str/replace #"(?i)(bearer |vis-ticket\.)[^\s\"]+" "$1[redacted]")
      (str/replace (.getAbsolutePath home) "[test-home]")))

(defn- run-native-media!
  [persistent? soak-seconds & [{:keys [repair?]}]]
  (let [binary
        (#'native/require-binary)

        home
        (#'native/temp-dir "vis-native-live-voice-")

        port
        (with-open [socket (ServerSocket. 0)]
          (.getLocalPort socket))

        lease-ready-at
        (atom nil)

        counts
        (atom {:cycles 0 :admissions 0 :receipts 0 :playback-receipts 0 :interruptions 0})

        metrics
        (atom [])

        monitor-stop
        (atom false)

        monitor
        (atom nil)

        process
        (atom nil)

        socket
        (atom nil)

        frames
        (input-frames)

        reference
        (when (= "1" (System/getenv "VIS_TEST_NATIVE_VOICE_PROFILE")) (direct-reference frames))

        reply
        (str (wire/json-str
               {"type" "conversation" "phase" "answer" "text" "The local speech test passed."})
             "\n"
             (wire/json-str {"type" "visual"
                             "markdown" "```mermaid\ngraph LR; Input-->Answer\n```"})
             "\n")

        provider
        (#'native/start-stub-provider!
         (if repair?
           (fn [{:keys [body]}]
             (if (str/includes? body "Repair only the final presentation.")
               reply
               "The local speech test passed."))
           reply))]

    (try
      (let [digest (java.security.MessageDigest/getInstance "SHA-256")]
        (doseq [frame frames]
          (.update digest ^bytes frame))
        (println "INPUT framed PCM SHA256" (.formatHex (java.util.HexFormat/of) (.digest digest))))
      (#'native/overlay! home (:port provider))
      (spit (io/file home ".vis/config.yml")
            "titling:\n  mode: disabled\ntoggles:\n  hands_free_voice: true\n"
            :append
            true)
      (let [[gateway _] (#'startup/start-native!
                         home
                         binary
                         "live-gateway"
                         ["gateway" "start" "--host" "127.0.0.1" "--port" (str port) "--db"
                          (.getAbsolutePath (io/file home "sessions"))]
                         {assets/models-dir-env (assets/models-root)
                          sherpa/native-dir-env (sherpa/native-dir)})]
        (reset! process gateway))
      (with-redefs-fn {#'gateway-client/ensure-gateway!
                       (constantly {:host "127.0.0.1" :port port :remote? true})
                       #'gateway-client/client-id (atom nil)
                       #'gateway-client/release-hook-installed? (atom true)}
        (fn []
          (await-health! @process)
          (when (pos? soak-seconds)
            (reset! monitor (Thread/startVirtualThread
                              (fn []
                                (while (not @monitor-stop)
                                  (try (let [sample (assoc (process-metrics @process)
                                                      :counts @counts
                                                      :at-ms (System/currentTimeMillis))]
                                         (swap! metrics conj sample)
                                         (println "NATIVE media soak metrics" sample))
                                       (catch Throwable error
                                         (swap! metrics conj {:error (.getMessage error)})))
                                  (dotimes [_ 60]
                                    (when-not @monitor-stop (Thread/sleep 1000))))))))
          (let [deadline (+ (System/nanoTime) (* soak-seconds 1000000000))]
            (loop []

              (let [cycle-start (System/nanoTime)
                    interrupt? (fn []
                                 (and (pos? soak-seconds) (= 4 (mod (:cycles @counts) 5))))
                    capabilities (request! :get "/v1/voice/live/capabilities" nil)
                    _ (expect (:available capabilities)
                              (str "Native live capability: "
                                   (select-keys capabilities
                                                [:enabled :available :reason :engines])))
                    sid (:id (request! :post
                                       "/v1/sessions"
                                       {:channel "api"
                                        :root (.getAbsolutePath ^java.io.File home)}))
                    started (request! :post
                                      (str "/v1/sessions/" sid "/voice/live")
                                      {:schema 1
                                       :client-id (str (UUID/randomUUID))
                                       :language "en"
                                       :privacy "local_speech"
                                       :muted false})
                    ready (promise)
                    state (promise)
                    closed (promise)
                    errors (atom [])
                    text (StringBuilder.)
                    partial (atom (promise))
                    utterance-id (atom 0)
                    ended-at (atom nil)
                    admission-events (atom [])
                    final-events (atom [])
                    speech (atom nil)
                    speech-events (atom [])
                    previous-epoch (atom -1)
                    interrupted-output? (atom false)
                    final-at (atom nil)
                    final-text (atom nil)
                    partial-times (atom [])
                    feed-start (atom nil)
                    feed-end (atom nil)
                    lateness (atom [])
                    admission-at (atom nil)
                    interrupted (atom (promise))
                    interrupt-request-at (atom nil)
                    interrupt-ack-at (atom nil)
                    finished (atom (promise))
                    first-pcm (atom nil)
                    output-count (atom 0)
                    binary-buffer (ByteArrayOutputStream.)]

                (with-open [client (HttpClient/newHttpClient)]
                  (let [^WebSocket connection
                        (.get
                          (.buildAsync
                            (.subprotocols (.newWebSocketBuilder client)
                                           "vis-voice.v1"
                                           (into-array String
                                                       [(str "vis-ticket." (:ticket started))]))
                            (URI. (str "ws://127.0.0.1:" port (:media-path started)))
                            (reify
                              WebSocket$Listener
                                (onOpen [_ connection]
                                  (.request ^WebSocket connection Long/MAX_VALUE))
                                (onText [_ _ data last?]
                                  (.append text ^CharSequence data)
                                  (when last?
                                    (let [event (wire/->engine (wire/parse-json (.toString text)))]
                                      (case (:type event)
                                        "session.ready"
                                        (deliver ready event)

                                        "session.state"
                                        (deliver state event)

                                        "transcript"
                                        (do (when (not= @utterance-id (:utterance-id event))
                                              (swap! errors conj {:wrong-transcript event}))
                                            (if (:final event)
                                              (do (swap! final-events conj event)
                                                  (when-not @ended-at
                                                    (swap! errors conj :final-before-end))
                                                  (reset! final-text (:text event))
                                                  (compare-and-set! final-at nil (System/nanoTime)))
                                              (when (seq (:text event))
                                                (swap! partial-times conj (System/nanoTime))
                                                (deliver @partial event))))

                                        "utterance.admitted"
                                        (do (swap! admission-events conj event)
                                            (when (or (not= @utterance-id (:utterance-id event))
                                                      (nil? @final-at))
                                              (swap! errors conj {:wrong-admission event}))
                                            (compare-and-set! admission-at nil (System/nanoTime)))

                                        "speech.committed"
                                        (do (when (or (not= (:turn-id (last @admission-events))
                                                            (:turn-id event))
                                                      (<= (long (:output-epoch event))
                                                          (long @previous-epoch)))
                                              (swap! errors conj {:wrong-speech event}))
                                            (swap! speech-events conj event)
                                            (reset! speech event))

                                        "output.finished"
                                        (when (and (= (:output-epoch @speech) (:output-epoch event))
                                                   (= (:segment-id @speech) (:segment-id event)))
                                          (deliver @finished event))

                                        "output.interrupted"
                                        (when (and @interrupt-request-at
                                                   (= "acknowledged" (:abort event))
                                                   (= (inc (long (:output-epoch @speech)))
                                                      (:output-epoch event)))
                                          (reset! interrupt-ack-at (System/nanoTime))
                                          (deliver @interrupted event))

                                        "error"
                                        (swap! errors conj event)

                                        nil))
                                    (.setLength text 0))
                                  (CompletableFuture/completedFuture nil))
                                (onBinary [_ connection data last?]
                                  (let [bytes (byte-array (.remaining ^ByteBuffer data))]
                                    (.get ^ByteBuffer data bytes)
                                    (.write binary-buffer bytes 0 (alength bytes)))
                                  (when last?
                                    (let [frame (contract/decode-frame (.toByteArray binary-buffer))
                                          position (+ (long (:sample-offset frame))
                                                      (long (:sample-count frame)))]

                                      (.reset binary-buffer)
                                      (when (and (not @interrupted-output?)
                                                 (some #(and (= (:output-epoch %)
                                                                (:generation frame))
                                                             (= (:segment-id %) (:stream-id frame)))
                                                       @speech-events))
                                        (when (and
                                                (compare-and-set! first-pcm nil (System/nanoTime))
                                                (interrupt?))
                                          (reset! interrupted-output? true)
                                          (reset! interrupt-request-at (System/nanoTime))
                                          (.sendText ^WebSocket connection
                                                     (wire/json-str {:type "output.interrupt"
                                                                     :output-epoch (:generation
                                                                                     frame)
                                                                     :reason "barge_in"})
                                                     true))
                                        (swap! output-count inc)
                                        (when-not @interrupted-output?
                                          (swap! counts update :playback-receipts inc)
                                          ;; Credit confirms test consumption, not physical playback.
                                          (.sendText ^WebSocket connection
                                                     (wire/json-str {:type "playback.receipt"
                                                                     :output-epoch (:generation
                                                                                     frame)
                                                                     :segment-id (:stream-id frame)
                                                                     :played position})
                                                     true)))))
                                  (CompletableFuture/completedFuture nil))
                                (onClose [_ _ code reason]
                                  (deliver closed [code reason])
                                  (CompletableFuture/completedFuture nil))
                                (onError [_ _ error] (swap! errors conj (str error)))))
                          30
                          TimeUnit/SECONDS)]
                    (reset! socket connection)
                    (try
                      (let [result (try (await-event! connection ready)
                                        (catch Exception _ :timeout))]
                        (expect (not= :timeout result)
                                (str
                                  "Native preparation failed: " @errors
                                  " closed=" (deref closed 0 nil)
                                  " gateway-diagnostics=" (slurp (io/file home "live-gateway.log"))
                                  " state=" (select-keys (request! :get
                                                                   (str "/v1/sessions/" sid
                                                                        "/voice/live/"
                                                                        (:voice-session-id started))
                                                                   nil)
                                                         [:state]))))
                      (reset! lease-ready-at (System/nanoTime))
                      (expect (= "vis-voice.v1" (.getSubprotocol connection)))
                      (.join (.sendText connection (wire/json-str {:type "heartbeat"}) true))
                      (expect (not= :timeout (deref state 5000 :timeout)))
                      (loop []

                        (let [before-requests (count @(:asked provider))]
                          (reset! utterance-id (if persistent? (:cycles @counts) 0))
                          (doseq [cell [partial interrupted finished]]
                            (reset! cell (promise)))
                          (doseq [cell [final-at final-text feed-start feed-end admission-at
                                        first-pcm interrupt-request-at interrupt-ack-at ended-at
                                        speech]]
                            (reset! cell nil))
                          (doseq [cell [partial-times lateness speech-events]]
                            (reset! cell []))
                          (reset! interrupted-output? false)
                          (reset! output-count 0)
                          (.join (.sendText connection
                                            (wire/json-str {:type "utterance.start"
                                                            :capture-generation 0
                                                            :utterance-id @utterance-id})
                                            true))
                          (reset! feed-start (System/nanoTime))
                          (doseq [original-frame frames]
                            (let [header (assoc (contract/decode-frame original-frame)
                                           :stream-id @utterance-id)
                                  frame (contract/encode-frame (select-keys header
                                                                            [:generation :stream-id
                                                                             :sample-offset])
                                                               (:pcm header))
                                  deadline (+ (long @feed-start)
                                              (quot (* 1000000000 (long (:sample-offset header)))
                                                    (long (:sample-rate contract/limits))))]

                              (wait-until! deadline)
                              (swap! lateness conj (max 0 (- (System/nanoTime) deadline)))
                              (try (.get (.sendBinary connection (ByteBuffer/wrap frame) true)
                                         5
                                         TimeUnit/SECONDS)
                                   (catch Exception error
                                     (throw (ex-info "Native input transport failed"
                                                     {:errors @errors
                                                      :closed (deref closed 0 nil)
                                                      :gateway-diagnostics
                                                      (-> (slurp (io/file home "live-gateway.log"))
                                                          (str/replace
                                                            #"(?i)(bearer |vis-ticket\.)[^\s\"]+"
                                                            "$1[redacted]")
                                                          (str/replace (.getAbsolutePath
                                                                         ^java.io.File home)
                                                                       "[test-home]"))}
                                                     error))))))
                          (wait-until! (+ (long @feed-start) (* 20000000 (count frames))))
                          (reset! feed-end (System/nanoTime))
                          (expect (not= :timeout (await-event! connection @partial))
                                  "No incremental transcript before finalization")
                          (expect (= before-requests (count @(:asked provider))))
                          (expect (nil? @final-at))
                          (expect (nil? @admission-at))
                          (let [ended (System/nanoTime)]
                            (reset! ended-at ended)
                            (.join (.sendText connection
                                              (wire/json-str {:type "utterance.end"
                                                              :capture-generation 0
                                                              :utterance-id @utterance-id})
                                              true))
                            (let [completion (await-event!
                                               connection
                                               (if (interrupt?) @interrupted @finished))]
                              (expect (not= :timeout completion)
                                      (when (= :timeout completion)
                                        (str {:errors @errors
                                              :closed (deref closed 0 nil)
                                              :final-transcript? (some? @final-at)
                                              :admitted? (some? @admission-at)
                                              :speech @speech
                                              :speech-events @speech-events
                                              :output-frames @output-count
                                              :provider-requests (count @(:asked provider))
                                              :state (:state (request! :get
                                                                       (str "/v1/sessions/" sid
                                                                            "/voice/live/"
                                                                            (:voice-session-id
                                                                              started))
                                                                       nil))
                                              :gateway-diagnostics (gateway-diagnostics home)}))))
                            (expect (pos? @output-count))
                            (expect (some? @first-pcm))
                            (let [turn (request! :get
                                                 (str "/v1/sessions/" sid
                                                      "/turns/" (:turn-id (last @admission-events)))
                                                 nil)
                                  blocks (:content turn)]

                              (expect (= "completed" (:status turn)))
                              (expect (= [{:type "conversation"
                                           :phase "answer"
                                           :text "The local speech test passed."}
                                          {:type "visual"
                                           :markdown "```mermaid\ngraph LR; Input-->Answer\n```"}]
                                         (mapv #(select-keys % [:type :phase :text :markdown])
                                               blocks)))
                              (expect (= ["The local speech test passed."]
                                         (mapv :text @speech-events))
                                      (str {:speech-events @speech-events :content blocks}))
                              (expect (= (str "presentation:" (:id (first blocks)))
                                         (:source @speech))))
                            (expect (some? @final-at))
                            (expect (some? @admission-at))
                            (when reference (expect (= reference @final-text)))
                            (when (interrupt?)
                              (expect (some? @interrupt-request-at))
                              (expect (some? @interrupt-ack-at))
                              (expect (<= (long @interrupt-request-at) (long @interrupt-ack-at))))
                            (println
                              "NATIVE live synthetic"
                              {:cycle (inc (:cycles @counts))
                               :utterance-id @utterance-id
                               :output-epoch (:output-epoch @speech)
                               :interrupt-wire-request-to-ack-ms
                               (when (interrupt?)
                                 (/ (- (long @interrupt-ack-at) (long @interrupt-request-at))
                                    1000000.0))
                               :final-text @final-text
                               :actual-feed-ms (/ (- (long @feed-end) (long @feed-start)) 1000000.0)
                               :send-lateness-max-ms (/ (double (apply max @lateness)) 1000000.0)
                               :send-lateness-mean-ms
                               (/ (double (reduce + @lateness)) (count @lateness) 1000000.0)
                               :partial-count (count @partial-times)
                               :partial-from-feed-ms
                               (mapv #(/ (- (long %) (long @feed-start)) 1000000.0) @partial-times)
                               :profile (if (zero? (:cycles @counts)) :cold :warm)
                               :observation :client-wire-receive
                               :input-pacing-ms (* 20 (count frames))
                               :end-to-final-ms (/ (- (long @final-at) ended) 1000000.0)
                               :final-to-admission-ms (/ (- (long @admission-at) (long @final-at))
                                                         1000000.0)
                               :admission-to-first-pcm-ms
                               (/ (- (long @first-pcm) (long @admission-at)) 1000000.0)
                               :endpoint-delay-ms 0
                               :finalization-to-first-pcm-ms
                               (when @first-pcm (/ (- (long @first-pcm) ended) 1000000.0))}))
                          (let [snapshot (request! :get
                                                   (str "/v1/sessions/" sid
                                                        "/voice/live/" (:voice-session-id started))
                                                   nil)
                                receipts (:receipts snapshot)
                                expected (if persistent? (inc (:cycles @counts)) 1)
                                receipt (first (filter #(= @utterance-id (:utterance-id %))
                                                       receipts))]

                            (expect (= expected
                                       (count receipts)
                                       (count @admission-events)
                                       (count @final-events)))
                            (expect (= expected
                                       (count (set (map :utterance-id receipts)))
                                       (count (set (map :turn-id receipts)))))
                            (expect (= (:voice-session-id started) (:voice-session-id receipt)))
                            (expect (= (:turn-id (last @admission-events))
                                       (:turn-id receipt)
                                       (:turn-id @speech)))
                            (expect (= @final-text (:text receipt)))
                            (expect (= (:idempotency-key (last @admission-events))
                                       (:idempotency-key receipt)))
                            (swap! counts update :admissions inc)
                            (swap! counts update :receipts inc))
                          (let [requests (drop before-requests @(:asked provider))]
                            (expect (= (if repair? 2 1) (count requests)))
                            (when repair?
                              (expect (str/includes? (:body (last requests))
                                                     "Repair only the final presentation."))
                              (expect (empty? (get (wire/parse-json (:body (last requests)))
                                                   "tools")))))
                          (reset! (:asked provider) [])
                          (reset! previous-epoch (:output-epoch @speech))
                          (when (interrupt?) (swap! counts update :interruptions inc))
                          (swap! counts update :cycles inc)
                          (expect (empty? @errors)))
                        (when persistent?
                          ;; Idle for longer than the lease timeout, with regular heartbeats.
                          (dotimes [_ (inc (quot (long (:lease-ms contract/limits)) 4000))]
                            (.get (.sendText connection (wire/json-str {:type "heartbeat"}) true)
                                  5
                                  TimeUnit/SECONDS)
                            (Thread/sleep 4000))
                          (expect (not (realized? closed)))
                          (expect (empty? @(:asked provider)))
                          (expect
                            (= (:cycles @counts) (count @final-events) (count @admission-events)))
                          (expect (empty? @errors))
                          (when (< (- (System/nanoTime) (long @lease-ready-at))
                                   (* soak-seconds 1000000000))
                            (recur))))
                      (.join (.sendText connection (wire/json-str {:type "session.end"}) true))
                      (expect (not= :timeout (deref closed 5000 :timeout)))
                      (let [retained (request! :get
                                               (str "/v1/sessions/" sid
                                                    "/voice/live/" (:voice-session-id started))
                                               nil)]
                        (expect (= (if persistent? (:cycles @counts) 1)
                                   (count (:receipts retained)))))
                      (when persistent?
                        (expect (>= (:cycles @counts) 5))
                        (expect (pos? (:interruptions @counts)))
                        (expect (>= (- (System/nanoTime) (long @lease-ready-at))
                                    (* soak-seconds 1000000000)))
                        (println "NATIVE persistent lease"
                                 {:session-id sid
                                  :voice-session-id (:voice-session-id started)
                                  :ready-to-close-ms (/ (- (System/nanoTime) (long @lease-ready-at))
                                                        1000000.0)
                                  :counts @counts}))
                      (finally (.abort connection)))))
                ;; At most ten retained leases per five-minute retention window.
                (when (and (not persistent?) (pos? soak-seconds) (< (System/nanoTime) deadline))
                  (let [remaining (- 30000 (quot (- (System/nanoTime) cycle-start) 1000000))]
                    (when (pos? remaining) (Thread/sleep remaining)))))
              (when (and (not persistent?) (pos? soak-seconds) (< (System/nanoTime) deadline))
                (recur))))
          (when (pos? soak-seconds)
            (expect (not-any? :error @metrics))
            (expect (= (:cycles @counts) (:admissions @counts) (:receipts @counts)))
            (let [warm
                  (first (drop 5 @metrics))

                  final
                  (last @metrics)

                  growth
                  (when (and warm (not (:error warm)) (not (:error final)))
                    (into {}
                          (map (fn [key]
                                 [key (- (long (get final key)) (long (get warm key)))])
                               [:rss-kib :threads :fds])))]

              ;; Report growth after warmup. Acoustic and heap-retention diagnosis remain separate.
              (println "NATIVE media soak final"
                       {:counts @counts :growth-after-warmup growth :samples @metrics})
              (when growth
                (expect (< (long (:threads growth)) 32)
                        "Native thread count grew beyond the soak allowance")
                (expect (< (long (:fds growth)) 32)
                        "Native descriptor count grew beyond the soak allowance"))))))
      (finally (reset! monitor-stop true)
               (when-let [^Thread thread @monitor]
                 (.join thread 2000))
               (when-let [^WebSocket connection @socket]
                 (.abort connection))
               (when-let [^Process gateway @process]
                 (#'native/kill-tree! gateway))
               (.stop ^com.sun.net.httpserver.HttpServer (:server provider) 0)
               (#'native/delete-tree! home)))))

(defdescribe native-live-voice-media
             (it "decodes incremental speech and streams a useful answer through native callbacks"
                 (run-native-media!
                   false
                   (Long/parseLong (or (System/getenv "VIS_TEST_NATIVE_VOICE_SOAK_SECONDS") "0")))))

(defdescribe native-live-voice-presentation-repair
             ;; Regression: the iPhone received fallback speech when reply repair failed before inference.
             (it "repairs malformed output once and speaks the repaired answer"
                 (run-native-media! false 0 {:repair? true})))

(defdescribe native-live-voice-persistent-lease
             (it "keeps one voice lease across successive utterances and idle heartbeats"
                 (if-let [seconds (System/getenv "VIS_TEST_NATIVE_VOICE_PERSISTENT_SECONDS")]
                   (let [duration (Long/parseLong seconds)]
                     (expect (>= duration 150))
                     (run-native-media! true duration))
                   (println
                     "SKIP: Set VIS_TEST_NATIVE_VOICE_PERSISTENT_SECONDS to at least 150."))))
