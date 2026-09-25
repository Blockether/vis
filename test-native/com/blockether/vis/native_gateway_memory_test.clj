(ns com.blockether.vis.native-gateway-memory-test
  "Disk-backed terminal turns, SSE replay and Python display formatting in the linked gateway
   image."
  (:require [charred.api :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.gateway.client :as gateway-client]
            [com.blockether.vis.native-binary-test :as binary]
            [lazytest.core :refer [defdescribe expect it]]
            [taoensso.nippy :as nippy])
  (:import [com.sun.net.httpserver HttpServer]
           [java.io File]
           [java.net ServerSocket]))

(def ^:private python-code
  "The model's Python as it arrives, before the gateway formats it for display."
  "answer=[1,2];print('native-display',answer)")

(defn- request-json!
  [method path opts]
  (let [{:keys [status body]} (gateway-client/request! method path opts)]
    (when-not (<= 200 status 299)
      (throw (ex-info "Owned gateway request failed" {:status status :path path})))
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

(defn- await-terminal!
  [path]
  (let [deadline (+ (System/nanoTime) 120000000000)]
    (loop []

      (let [turn (request-json! :get path {})]
        (if (contains? #{"completed" "failed" "cancelled" "suspended"} (get turn "status"))
          turn
          (if (< (System/nanoTime) deadline)
            (do (Thread/sleep 50) (recur))
            (throw (ex-info "Owned native turn did not finish" {}))))))))

(defn- replay-through-terminal!
  [sid tid]
  (let [{:keys [status body]}
        (gateway-client/request! :get
                                 (str "/v1/events?sids=" sid ":0")
                                 {:as :stream :timeout-ms 15000})

        seen
        (atom [])]

    (expect (= 200 status))
    (with-open [reader (io/reader body)]
      (let [pending (future
                      (loop [events []]
                        (if-let [line (.readLine ^java.io.BufferedReader reader)]
                          (if (str/starts-with? line "data: ")
                            (let [event (json/read-json (subs line 6))
                                  events (conj events event)
                                  _ (swap! seen #(vec (take-last 10
                                                                 (conj %
                                                                       (select-keys
                                                                         event
                                                                         ["type" "seq" "turn_id"
                                                                          "reason" "cursor"])))))]

                              (if (or (and (= "turn.completed" (get event "type"))
                                           (= tid (get event "turn_id")))
                                      (>= (count events) 4096))
                                events
                                (recur events)))
                            (recur events))
                          events)))
            result (deref pending 15000 ::timeout)]

        (try (when (= ::timeout result)
               (throw (ex-info (str "Owned native SSE replay timed out: " (pr-str @seen)) {})))
             result
             (finally (future-cancel pending)))))))

(defdescribe
  native-gateway-memory-test
  (it
    "hydrates archived terminal turns and replays their persisted events with formatted Python"
    (let [^File dir
          (#'binary/temp-dir "vis-native-gateway-memory-")

          ^File bin
          (#'binary/require-binary)

          reply
          (str/join " " (repeat 2048 "native-archive"))

          request
          (apply str (repeat 2048 "native-request "))

          {:keys [server asked port]}
          (#'binary/start-stub-provider! reply)

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

          ;; The first answer runs Python, so the turn carries a block the gateway
          ;; formats for display; the next one finishes the turn.
          answer
          (fn [stream? text]
            (if (= 1 (swap! calls inc))
              (#'binary/python-tool-body python-code 1 stream?)
              ((if stream? original-stream original-whole) text)))]

      (try
        (#'binary/overlay! dir port)
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
        ;; Keep real client registration and HTTP/SSE transport, with no JVM shutdown lease.
        (with-redefs-fn {#'gateway-client/ensure-gateway!
                         (constantly {:host "127.0.0.1" :port gateway-port :remote? true})
                         #'gateway-client/client-id (atom nil)
                         #'gateway-client/release-hook-installed? (atom true)
                         #'binary/stream-body #(answer true %)
                         #'binary/whole-body #(answer false %)}
          (fn []
            (await-health! @process)
            (let [created
                  (request-json! :post
                                 "/v1/sessions"
                                 {:body {:channel "api" :root (.getAbsolutePath dir)}})

                  sid
                  (get created "id")

                  _
                  (expect (string? sid))

                  turns-path
                  (str "/v1/sessions/" sid "/turns")

                  submitted
                  (request-json! :post
                                 turns-path
                                 {:body {:request request
                                         :provider "stub-local"
                                         :model "stub-model"
                                         :idempotency_key "native-memory"}})

                  tid
                  (get submitted "turn_id")

                  turn
                  (await-terminal! (str turns-path "/" tid))]

              (expect (= "completed" (get turn "status")))
              (expect (= request (get turn "request")))
              (expect (= reply (get-in turn ["content" 0 "markdown"])))
              (expect (seq @asked) "The isolated local model must answer the turn")
              (let [history (first (get (request-json! :get turns-path {}) "turns"))]
                (doseq [field ["turn_id" "session_id" "status" "request"]]
                  (expect (= (get turn field) (get history field))))
                (expect (= reply (get-in history ["content" 0 "markdown"]))))
              (let [archives (for [^File file (file-seq dir)
                                   :when (and (.isFile file)
                                              (str/starts-with? (.getName (.getParentFile file))
                                                                "vis-gateway-")
                                              (str/ends-with? (.getName file) ".nippy"))]

                               (nippy/thaw-from-file file))]
                ;; Prove the native writer succeeded rather than only testing DB fallback.
                (expect (seq archives)
                        (str "No native archive files. Gateway diagnostics: "
                             (str/join "\n"
                                       (take 20
                                             (filter #(re-find #"ERROR LOG|WARN LOG|error-class" %)
                                                     (mapcat (fn [^File file]
                                                               (when (and (.isFile file)
                                                                          (re-find
                                                                            #"\.(log|jsonl|ndjson)$"
                                                                            (.getName file)))
                                                                 (str/split-lines (slurp file))))
                                                             (file-seq dir)))))))
                (expect (some #(and (= tid (:turn_id %)) (= request (:request %))) archives))
                (expect (some #(and (= tid (get % "turn_id")) (= "turn.completed" (get % "type")))
                              archives)))
              (let [events
                    (replay-through-terminal! sid tid)

                    sequences
                    (keep #(get % "seq") events)

                    completed
                    (last events)]

                (expect (> (count events) 1))
                (expect (apply < sequences))
                (expect (some #(= "turn.started" (get % "type")) events))
                (expect (= "turn.completed" (get completed "type")))
                (expect (= tid (get completed "turn_id")))
                (expect (= reply (get-in completed ["content" 0 "markdown"])))
                (expect (= "answer = [1, 2]\nprint(\"native-display\", answer)\n"
                           (some #(when (= "block.started" (get % "type")) (get % "display_code"))
                                 events))
                        "The linked gateway must format the model's Python for display")))))
        (finally (when-let [owned @process]
                   (#'binary/kill-tree! owned))
                 (.stop ^HttpServer server 0)
                 (#'binary/delete-tree! dir))))))
