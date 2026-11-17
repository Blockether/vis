(ns com.blockether.vis.internal.gateway.server.voice-transport-test
  (:require [clojure.string :as str]
            [com.blockether.vis.contract.gateway :as gateway-contract]
            [com.blockether.vis.contract.voice :as contract]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.internal.config.toggles :as toggles]
            [com.blockether.vis.internal.gateway.event-check :as event-check]
            [com.blockether.vis.internal.gateway.server :as gateway]
            [com.blockether.vis.internal.gateway.state :as state]
            [com.blockether.vis.internal.gateway.voice :as voice]
            [lazytest.core :refer [defdescribe expect it]]
            [ring.adapter.jetty9 :as jetty]
            [ring.websocket :as ws])
  (:import [java.io ByteArrayOutputStream]
           [java.net URI]
           [java.net.http HttpClient HttpRequest HttpRequest$BodyPublishers
            HttpResponse$BodyHandlers WebSocket WebSocket$Listener WebSocketHandshakeException]
           [java.nio ByteBuffer]
           [java.util UUID]
           [java.util.concurrent CompletableFuture CountDownLatch TimeUnit]
           [org.eclipse.jetty.server Server ServerConnector]))

(set! *warn-on-reflection* true)

(defdescribe
  live-voice-transport-feasibility
  (it
    "upgrades on the existing adapter and exchanges bounded binary messages"
    (let [received
          (CountDownLatch. 200)

          bytes-received
          (atom 0)

          errors
          (atom [])

          ^Server server
          (jetty/run-jetty (fn [request]
                             (if (ws/upgrade-request? request)
                               {:status 101
                                :ring.websocket/protocol "vis-voice.v1"
                                :ring.websocket/listener
                                {:on-message (fn [socket data]
                                               (let [^ByteBuffer source
                                                     data

                                                     copy
                                                     (ByteBuffer/allocate (.remaining source))]

                                                 (.put copy source)
                                                 (.flip copy)
                                                 (ws/send socket copy)))}}
                               {:status 400 :body "Upgrade required"}))
                           {:host "127.0.0.1"
                            :port 0
                            :join? false
                            :ws-max-binary-message-size 2048
                            :ws-max-frame-size 2048})

          port
          (.getLocalPort ^ServerConnector (first (.getConnectors server)))

          socket
          (atom nil)]

      (try (let [^WebSocket connection
                 (.join (.buildAsync (.subprotocols (.newWebSocketBuilder
                                                      (HttpClient/newHttpClient))
                                                    "vis-voice.v1"
                                                    (make-array String 0))
                                     (URI. (str "ws://127.0.0.1:" port "/media"))
                                     (reify
                                       WebSocket$Listener
                                         (onOpen [_ connection]
                                           (.request ^WebSocket connection Long/MAX_VALUE))
                                         (onBinary [_ _ data last?]
                                           (swap! bytes-received + (.remaining ^ByteBuffer data))
                                           (when last? (.countDown received))
                                           (CompletableFuture/completedFuture nil))
                                         (onError [_ _ error] (swap! errors conj (str error))))))]
             (reset! socket connection)
             (dotimes [_ 200]
               (.join (.sendBinary connection (ByteBuffer/wrap (byte-array 992)) true)))
             (expect (.await received 10 TimeUnit/SECONDS))
             (expect (= (* 200 992) @bytes-received))
             (expect (empty? @errors)))
           (finally (when-let [^WebSocket connection @socket]
                      (.abort connection))
                    (.stop server))))))

(def ^:private session-id (UUID/fromString "31d34262-789a-43bb-82e3-6e8531b32000"))

(def ^:private origin "https://gateway.example.com")

(defn- await!
  [predicate]
  (let [deadline (+ (System/nanoTime) 5000000000)]
    (loop []

      (cond (predicate) true
            (> (System/nanoTime) deadline) false
            :else (do (Thread/sleep 5) (recur))))))

(defn- request!
  [^HttpClient client base method path body]
  (let [builder
        (doto (HttpRequest/newBuilder (URI. (str base path)))
          (.header "X-Vis-Protocol" (str gateway-contract/protocol-version))
          (.header "Origin" origin)
          (.header "Content-Type" "application/json"))

        request
        (.build (.method builder
                         method
                         (if body
                           (HttpRequest$BodyPublishers/ofString (wire/json-str body))
                           (HttpRequest$BodyPublishers/noBody))))

        response
        (.send client request (HttpResponse$BodyHandlers/ofString))]

    {:status (.statusCode response) :data (wire/->engine (wire/parse-json (.body response)))}))

(defn- connect!
  [^HttpClient client base path ticket request-origin]
  (let [events
        (atom [])

        frames
        (atom [])

        closed
        (promise)

        errors
        (atom [])

        text
        (StringBuilder.)

        binary
        (ByteArrayOutputStream.)

        builder
        (.newWebSocketBuilder client)]

    (.subprotocols builder
                   "vis-voice.v1"
                   (into-array String (when ticket [(str "vis-ticket." ticket)])))
    (when request-origin (.header builder "Origin" request-origin))
    (try
      {:socket (.join
                 (.buildAsync
                   builder
                   (URI. (str base path))
                   (reify
                     WebSocket$Listener
                       (onOpen [_ socket] (.request ^WebSocket socket Long/MAX_VALUE))
                       (onText [_ _ data last?]
                         (.append text ^CharSequence data)
                         (when last?
                           (swap! events conj (wire/->engine (wire/parse-json (.toString text))))
                           (.setLength text 0))
                         (CompletableFuture/completedFuture nil))
                       (onBinary [_ _ data last?]
                         (let [bytes (byte-array (.remaining ^ByteBuffer data))]
                           (.get ^ByteBuffer data bytes)
                           (.write binary bytes 0 (alength bytes)))
                         (when last?
                           (swap! frames conj
                             (contract/decode-frame (ByteBuffer/wrap (.toByteArray binary))))
                           (.reset binary))
                         (CompletableFuture/completedFuture nil))
                       (onClose [_ _ status reason]
                         (deliver closed [status reason])
                         (CompletableFuture/completedFuture nil))
                       (onError [_ _ error] (swap! errors conj (str error))))))
       :events events
       :frames frames
       :closed closed
       :errors errors
       :status 101}
      (catch java.util.concurrent.CompletionException error
        (let [cause (.getCause error)]
          (if (instance? WebSocketHandshakeException cause)
            {:status (.statusCode (.getResponse ^WebSocketHandshakeException cause))}
            (throw error)))))))

(defn- send!
  [connection message]
  (.join (.sendText ^WebSocket (:socket connection) (wire/json-str message) true)))

(defn- with-gateway
  [f]
  (let [admitted
        (atom [])

        durable
        (atom [])

        subscriber
        (atom nil)

        engines
        {'capabilities (constantly {:available true})
         'prepare! (constantly {:ready true})
         'open-asr! (fn [_]
                      (atom []))
         'feed-asr! (fn [_ _ _]
                      {:text "Provisional text" :revision 1})
         'finish-asr! (fn [_]
                        {:text "Explain the test." :revision 2})
         'abort-asr! (constantly {:state :acknowledged})
         'asr-status (constantly {:state :open})
         'synthesize-stream! (fn [{:keys [on-pcm]}]
                               (on-pcm (float-array 960) 24000)
                               {:state :finished})}]

    (with-redefs [voice/engine-api
                  (constantly engines)

                  toggles/enabled?
                  (constantly true)

                  gateway/auth-required?
                  (constantly false)

                  state/soul
                  (fn [sid]
                    (when (= session-id sid) {}))

                  state/session-archived?
                  (constantly false)

                  state/subscribe!
                  (fn [_ _ sink _]
                    (reset! subscriber sink)
                    [])

                  state/unsubscribe!
                  (fn [_ _]
                    (reset! subscriber nil))

                  state/append-event!
                  (fn [_ type payload]
                    (event-check/check! type (wire/canonical payload))
                    (swap! durable conj [type payload]))

                  state/submit-turn!
                  (fn [_ request]
                    (swap! admitted conj request)
                    {:turn {:turn_id (str (UUID/randomUUID)) :status "running"}})]

      (let [^Server server
            (jetty/run-jetty (#'gateway/app "fixture-token" [])
                             {:host "127.0.0.1"
                              :port 0
                              :join? false
                              :ws-max-text-message-size (:max-control-bytes contract/limits)
                              :ws-max-binary-message-size 992
                              :ws-max-frame-size 8192})

            port
            (.getLocalPort ^ServerConnector (first (.getConnectors server)))

            base
            (str "http://127.0.0.1:" port)

            client
            (HttpClient/newHttpClient)

            sockets
            (atom [])

            start
            (request! client
                      base
                      "POST"
                      (str "/v1/sessions/" session-id "/voice/live")
                      {:schema 1
                       :client-id (str (UUID/randomUUID))
                       :language "en"
                       :privacy "local_speech"
                       :muted false})

            data
            (:data start)

            runtime
            (voice/runtime-for session-id (:voice-session-id data))

            connect
            (fn [path ticket request-origin]
              (let [result
                    (connect! client (str "ws://127.0.0.1:" port) path ticket request-origin)]
                (when-let [socket (:socket result)]
                  (swap! sockets conj socket))
                result))]

        (try (expect (= 201 (:status start)) (str start))
             (f {:connect connect
                 :data data
                 :runtime runtime
                 :subscriber subscriber
                 :admitted admitted
                 :durable durable
                 :request #(request! client base %1 %2 %3)})
             (finally (doseq [^WebSocket socket @sockets]
                        (.abort socket))
                      (voice/shutdown!)
                      (expect (every? (fn [^Thread thread]
                                        (not (.isAlive thread)))
                                      @(:threads runtime)))
                      (.stop server)
                      (.close client)))))))

(defdescribe
  live-voice-gateway-transport
  (it "requires a ticket on loopback and rejects session, origin and ticket mismatches"
      (with-gateway (fn [{:keys [connect data]}]
                      (let [{:keys [media-path ticket]} data]
                        (doseq [[path token request-origin]
                                [[media-path nil origin] [media-path "invalid" origin]
                                 [media-path ticket "https://other.example.com"]
                                 [media-path ticket nil]
                                 [(str/replace media-path (str session-id) (str (UUID/randomUUID)))
                                  ticket origin]]]
                          (expect (= 401 (:status (connect path token request-origin)))))
                        (let [connection (connect media-path ticket origin)]
                          (expect (= 101 (:status connection)))
                          (expect (await! #(some (fn [event]
                                                   (= "session.ready" (:type event)))
                                                 @(:events connection)))))
                        (expect (= 401 (:status (connect media-path ticket origin))))))))
  (it "accepts a scoped ticket while the ordinary bearer gate remains closed"
      (with-gateway (fn [{:keys [connect data request]}]
                      (with-redefs [gateway/auth-required? (constantly true)]
                        (expect (= 401 (:status (request "GET" "/v1/voice/live/capabilities" nil))))
                        (expect (= 401 (:status (connect (:media-path data) nil origin))))
                        (let [connection (connect (:media-path data) (:ticket data) origin)]
                          (expect (= 101 (:status connection)))
                          (expect (await! #(some (fn [event]
                                                   (= "session.ready" (:type event)))
                                                 @(:events connection)))))))))
  (it "rejects an expired ticket before upgrading"
      (with-gateway (fn [{:keys [connect data runtime]}]
                      (with-redefs [voice/monotonic-ms (constantly (inc (:ticket-until runtime)))]
                        (expect
                          (= 401 (:status (connect (:media-path data) (:ticket data) origin))))))))
  (it
    "streams PCM through the real handler and records rendered playback"
    (with-gateway
      (fn [{:keys [connect data runtime subscriber admitted durable request]}]
        (let [connection
              (connect (:media-path data) (:ticket data) origin)

              events
              (:events connection)]

          (expect (await! #(some (fn [event]
                                   (= "session.ready" (:type event)))
                                 @events)))
          (send! connection {:type "utterance.start" :capture-generation 0 :utterance-id 0})
          (.join (.sendBinary ^WebSocket (:socket connection)
                              (ByteBuffer/wrap (contract/encode-frame {:kind :input
                                                                       :generation 0
                                                                       :stream-id 0
                                                                       :sequence 0
                                                                       :sample-offset 0
                                                                       :sample-rate 24000}
                                                                      (byte-array 960)))
                              true))
          (expect (await! #(some (fn [event]
                                   (= "transcript" (:type event)))
                                 @events)))
          (expect (empty? @admitted))
          (send! connection {:type "utterance.end" :capture-generation 0 :utterance-id 0})
          (expect (await! #(= 1 (count @admitted))))
          (let [turn-id (:turn-id (first (:receipts (voice/snapshot runtime))))]
            (@subscriber
             {"type" "speech.committed"
              "turn_id" turn-id
              "source" "stream:1:0"
              "text" "The test passed."}))
          (expect (await! #(some (fn [event]
                                   (= "output.finished" (:type event)))
                                 @events)))
          (expect (= 2 (count @(:frames connection))))
          (expect (= [0 480] (mapv :sample-offset @(:frames connection))))
          (let [segment (:segment-id (first (filter #(= "speech.committed" (:type %)) @events)))]
            (send! connection
                   {:type "playback.receipt"
                    :output-epoch 0
                    :segment-id segment
                    :received 960
                    :decoded 960
                    :scheduled 960
                    :played 960}))
          (expect (await! #(some (fn [[type _]]
                                   (= "voice.playback" type))
                                 @durable)))
          (expect (= 200 (:status (request "GET" "/v1/voice/live/capabilities" nil))))
          (let [path (str "/v1/sessions/" session-id "/voice/live/" (:voice-session-id data))]
            (expect (= 200 (:status (request "GET" path nil))))
            (expect (= 200 (:status (request "DELETE" path nil)))))
          (expect (not= :timeout (deref (:closed connection) 5000 :timeout)))
          (expect (empty? @(:errors connection)))))))
  (it "closes malformed JSON without admitting a task"
      (with-gateway (fn [{:keys [connect data admitted]}]
                      (let [connection (connect (:media-path data) (:ticket data) origin)]
                        (expect (await! #(some (fn [event]
                                                 (= "session.ready" (:type event)))
                                               @(:events connection))))
                        (.join (.sendText ^WebSocket (:socket connection) "{" true))
                        (expect (not= :timeout (deref (:closed connection) 5000 :timeout)))
                        (expect (empty? @admitted))))))
  (it "closes malformed binary without admitting a task"
      (with-gateway (fn [{:keys [connect data admitted]}]
                      (let [connection (connect (:media-path data) (:ticket data) origin)]
                        (expect (await! #(some (fn [event]
                                                 (= "session.ready" (:type event)))
                                               @(:events connection))))
                        (.join (.sendBinary ^WebSocket (:socket connection)
                                            (ByteBuffer/wrap (byte-array 32))
                                            true))
                        (expect (not= :timeout (deref (:closed connection) 5000 :timeout)))
                        (expect (empty? @admitted))))))
  (it "closes oversized messages at the adapter boundary"
      (doseq [binary? [false true]]
        (with-gateway (fn [{:keys [connect data admitted]}]
                        (let [connection (connect (:media-path data) (:ticket data) origin)
                              ^WebSocket socket (:socket connection)]

                          (if binary?
                            (.join (.sendBinary socket (ByteBuffer/wrap (byte-array 993)) true))
                            (.join (.sendText socket (apply str (repeat 8193 "x")) true)))
                          (expect (not= :timeout (deref (:closed connection) 5000 :timeout)))
                          (expect (empty? @admitted))))))))
