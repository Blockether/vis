;; Native coverage for the evaluation routes in the linked gateway image.
(ns com.blockether.vis.native-evals-test
  "Evaluation routes in the linked gateway image: the bundled SDK modules load and answer."
  (:require [charred.api :as json]
            [clojure.java.io :as io]
            [com.blockether.vis.internal.gateway.client :as gateway-client]
            [com.blockether.vis.native-binary-test :as binary]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [java.io File]
           [java.net ServerSocket]))

(defn- request-json!
  "Return the status and the parsed JSON body of one owned gateway request."
  [method path opts]
  (let [{:keys [status body]} (gateway-client/request! method path opts)]
    {:status status :body (when (seq body) (json/read-json body))}))

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

(defn- cases
  [passed]
  (map-indexed (fn [n p]
                 {"task" (str "t" n) "agent" "vis" "model" "m" "passed" p "runs" 1})
               passed))

(defdescribe
  native-evals-test
  (it
    "runs the bundled SDK evaluation modules"
    (let [^File dir
          (#'binary/temp-dir "vis-native-evals-")

          ^File bin
          (#'binary/require-binary)

          gateway-port
          (with-open [socket (ServerSocket. 0)]
            (.getLocalPort socket))

          process
          (atom nil)]

      (try (let [pb
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
           ;; Pin routing to the owned listener: discovery must never reach a user's daemon.
           (with-redefs-fn {#'gateway-client/ensure-gateway!
                            (constantly {:host "127.0.0.1" :port gateway-port :remote? true})
                            #'gateway-client/client-id (atom nil)
                            #'gateway-client/release-hook-installed? (atom true)}
             (fn []
               (await-health! @process)
               (let [compared
                     (request-json! :post
                                    "/v1/evals/compare"
                                    {:body {:baseline (cases (repeat 20 0))
                                            :candidate (cases (repeat 20 1))}
                                     :timeout-ms 60000})

                     agreed
                     (request-json! :post
                                    "/v1/evals/agreement"
                                    {:body {:items [{:name "a" :automatic "write" :human "write"}
                                                    {:name "b" :automatic "verify" :human "write"}]
                                            :source "judge"
                                            :noun "judge stages"}
                                     :timeout-ms 60000})

                     refused
                     (request-json! :post
                                    "/v1/evals/compare"
                                    {:body {:baseline [{}] :candidate []} :timeout-ms 60000})]

                 (expect (= 200 (:status compared)) (pr-str compared))
                 (expect (= "better" (get-in compared [:body "verdict"])))
                 (expect (<= (get-in compared [:body "difference_ci95" 1]) 100.0)
                         "The interval must stay inside the possible changes")
                 (expect (= 200 (:status agreed)) (pr-str agreed))
                 (expect (= 2 (get-in agreed [:body "labelled"])))
                 (expect (= 400 (:status refused))))))
           (finally (when-let [owned @process]
                      (#'binary/kill-tree! owned))
                    (#'binary/delete-tree! dir))))))
