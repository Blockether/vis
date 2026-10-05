(ns com.blockether.vis.native-queue-send-now-test
  "Send now in the linked gateway image: a marked queued message reaches the running turn at its
   next step, and an unmarked message still waits for the turn end."
  (:require [charred.api :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.gateway.client :as gateway-client]
            [com.blockether.vis.native-binary-test :as binary]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [com.sun.net.httpserver HttpServer]
           [java.io BufferedReader File]
           [java.net ServerSocket]))

(def ^:private slow-step
  "Python that keeps a turn in one step long enough to queue and mark messages."
  "import time\ntime.sleep(3)\nprint('native-send-now-step')")

(defn- request-json!
  [method path opts]
  (let [{:keys [status body]} (gateway-client/request! method path opts)]
    (when-not (<= 200 status 299)
      (throw (ex-info "Owned gateway request failed" {:status status :path path :body body})))
    (json/read-json body)))

(defn- await-health!
  [^Process process]
  (let [deadline (+ (System/nanoTime) 60000000000)]
    (loop []

      (if (try (= 200 (:status (gateway-client/request! :get "/healthz" {:timeout-ms 1000})))
               (catch Exception _ false))
        true
        (if (and (.isAlive process) (< (System/nanoTime) deadline))
          (do (Thread/sleep 50) (recur))
          (throw (ex-info "Owned native gateway did not become healthy" {})))))))

(defn- await-value!
  "Call `f` every 50 ms until it returns a truthy value. Throw `message` after 120 s."
  [message f]
  (let [deadline (+ (System/nanoTime) 120000000000)]
    (loop []

      (or (f)
          (if (< (System/nanoTime) deadline)
            (do (Thread/sleep 50) (recur))
            (throw (ex-info message {})))))))

(defn- turn [turns-path tid] (request-json! :get (str turns-path "/" tid) {}))

(defn- await-terminal!
  [turns-path tid]
  (await-value! (str "Owned native turn did not finish: " tid)
                #(let [record (turn turns-path tid)] (when (contains? #{"completed" "failed"
                                                                        "cancelled" "suspended"}
                                                                      (get record "status"))
                                                       record))))

(defn- replayed-events
  "The stored events of `sid` through the `turn.completed` of `tid`, from a full SSE replay."
  [sid tid]
  (let [{:keys [status body]} (gateway-client/request! :get
                                                       (str "/v1/events?sids=" sid ":0&replay=full")
                                                       {:as :stream :timeout-ms 15000})]
    (expect (= 200 status))
    (with-open [reader (io/reader body)]
      (let [pending (future (loop [events []]
                              (if-let [line (.readLine ^BufferedReader reader)]
                                (if (str/starts-with? line "data: ")
                                  (let [event (json/read-json (subs line 6))
                                        events (conj events event)]

                                    (if (and (= "turn.completed" (get event "type"))
                                             (= tid (get event "turn_id")))
                                      events
                                      (recur events)))
                                  (recur events))
                                events)))
            result (deref pending 15000 ::timeout)]

        (try (when (= ::timeout result) (throw (ex-info "Owned native SSE replay timed out" {})))
             result
             (finally (future-cancel pending)))))))

(defdescribe
  native-queue-send-now-test
  (it
    "delivers marked queued messages into the running turn and starts unmarked ones after it"
    (let [^File dir
          (#'binary/temp-dir "vis-native-send-now-")

          ^File bin
          (#'binary/require-binary)

          {:keys [server asked port]}
          (#'binary/start-stub-provider! "native-send-now-reply")

          gateway-port
          (with-open [socket (ServerSocket. 0)]
            (.getLocalPort socket))

          process
          (atom nil)

          calls
          (atom 0)

          original-stream
          @#'binary/stream-body

          original-whole
          @#'binary/whole-body

          ;; An odd call starts a turn with a slow Python step; the next call ends that turn.
          answer
          (fn [stream? text]
            (let [n (swap! calls inc)]
              (if (odd? n)
                (#'binary/python-tool-body slow-step n stream?)
                ((if stream? original-stream original-whole) text))))]

      (try
        (#'binary/overlay! dir port)
        ;; A local title keeps every model call inside a turn, so the call order is fixed.
        (spit (io/file dir ".vis" "config.yml") "titling:\n  mode: disabled\n" :append true)
        (let [pb
              (doto (ProcessBuilder. ^java.util.List
                                     [(.getAbsolutePath bin)
                                      (str "-Duser.home=" (.getAbsolutePath dir))
                                      (str "-Djava.io.tmpdir=" (.getAbsolutePath dir)) "gateway"
                                      "start" "--host" "127.0.0.1" "--port" (str gateway-port)
                                      "--db" (.getAbsolutePath (io/file dir "sessions"))])
                (.directory dir)
                (.redirectErrorStream true)
                (.redirectOutput (io/file dir "gateway.log")))

              environment
              (.environment pb)]

          (.putAll environment (#'binary/native-environment))
          (doseq [key ["VIS_GATEWAY_URL" "VIS_GATEWAY_TOKEN" "VIS_GATEWAY_MANAGED"]]
            (.remove environment key))
          (reset! process (.start pb)))
        ;; Pin routing to our owned listener: discovery must never reach a user's daemon.
        (with-redefs-fn {#'gateway-client/ensure-gateway!
                         (constantly {:host "127.0.0.1" :port gateway-port :remote? true})
                         #'gateway-client/client-id (atom nil)
                         #'gateway-client/release-hook-installed? (atom true)
                         #'binary/stream-body #(answer true %)
                         #'binary/whole-body #(answer false %)}
          (fn []
            (await-health! @process)
            (let [sid
                  (get (request-json! :post
                                      "/v1/sessions"
                                      {:body {:channel "api" :root (.getAbsolutePath dir)}})
                       "id")

                  turns-path
                  (str "/v1/sessions/" sid "/turns")

                  submit!
                  (fn [text]
                    (get (request-json! :post
                                        turns-path
                                        {:body {:request text
                                                :provider "stub-local"
                                                :model "stub-model"
                                                :idempotency_key text}})
                         "turn_id"))

                  running
                  (submit! "native-first-request")

                  _
                  (await-value! "The first turn never asked the model" #(pos? @calls))

                  marked
                  (submit! "native-marked-message")

                  waiting
                  (submit! "native-waiting-message")]

              (expect (= ["queued" "queued"]
                         (mapv #(get (turn turns-path %) "status") [marked waiting])))
              ;; The row arrow marks one queued message.
              (expect (= "next_iteration"
                         (get (request-json! :patch
                                             (str turns-path "/" marked)
                                             {:body {:deliver "next_iteration"}})
                              "deliver")))
              (expect (= "completed" (get (await-terminal! turns-path running) "status")))
              (let [delivered
                    (turn turns-path marked)

                    next-request
                    (:body (nth @asked 1))]

                (expect (= ["sent" running]
                           [(get delivered "status") (get delivered "into_turn_id")]))
                (expect (str/includes? next-request "native-marked-message"))
                (expect (not (str/includes? next-request "native-waiting-message"))
                        "An unmarked message must wait for the turn end"))
              ;; The unmarked message starts as its own turn after the first turn ends.
              (await-value! "The waiting message never started its own turn" #(>= @calls 3))
              (let [first-header
                    (submit! "native-header-first")

                    second-header
                    (submit! "native-header-second")

                    ;; The header action marks every queued message.
                    result
                    (request-json! :post (str "/v1/sessions/" sid "/queue/send-now") {})]

                (expect (= [first-header second-header] (get result "marked")))
                (expect (= "completed" (get (await-terminal! turns-path waiting) "status")))
                (let [next-request (:body (nth @asked 3))]
                  (expect (< (str/index-of next-request "native-header-first")
                             (str/index-of next-request "native-header-second"))
                          "The running turn must take marked messages in queue order"))
                (expect (= [waiting waiting]
                           (mapv #(get (turn turns-path %) "into_turn_id")
                                 [first-header second-header])))
                (expect (= [[running ["native-marked-message"]]
                            [waiting ["native-header-first" "native-header-second"]]]
                           (->> (replayed-events sid waiting)
                                (filter #(= "turn.input" (get % "type")))
                                (mapv (fn [event]
                                        [(get event "turn_id")
                                         (mapv #(get % "request") (get event "messages"))])))))
                (expect (= 4 (count @asked)) (pr-str (mapv :path @asked)))))))
        (finally (when-let [owned @process]
                   (#'binary/kill-tree! owned))
                 (.stop ^HttpServer server 0)
                 (#'binary/delete-tree! dir))))))
