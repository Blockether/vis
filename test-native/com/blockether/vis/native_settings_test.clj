;; Native coverage for the typed Settings batch in the linked gateway image.
(ns com.blockether.vis.native-settings-test
  "Typed Settings batches in the linked gateway image: revisions, writes, stale retries and
   schema refusals."
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

(defn- setting-row
  "Find setting `id` in a Settings catalog."
  [catalog id]
  (first (for [group
               (get catalog "groups")

               row
               (get group "toggles")

               :when (= id (get row "id"))]

           row)))

(defdescribe
  native-settings-test
  (it
    "applies one typed batch and refuses stale or invalid batches without writing"
    (let [^File dir
          (#'binary/temp-dir "vis-native-settings-")

          ^File bin
          (#'binary/require-binary)

          gateway-port
          (with-open [socket (ServerSocket. 0)]
            (.getLocalPort socket))

          process
          (atom nil)]

      (try
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
        ;; Pin routing to the owned listener: discovery must never reach a user's daemon.
        (with-redefs-fn {#'gateway-client/ensure-gateway!
                         (constantly {:host "127.0.0.1" :port gateway-port :remote? true})
                         #'gateway-client/client-id (atom nil)
                         #'gateway-client/release-hook-installed? (atom true)}
          (fn []
            (await-health! @process)
            (let [sid
                  (get (:body (request-json! :post
                                             "/v1/sessions"
                                             {:body {:channel "api" :root (.getAbsolutePath dir)}}))
                       "id")

                  catalog-path
                  (str "/v1/settings?scope=session&target_id=" sid)

                  initial
                  (:body (request-json! :get catalog-path {}))

                  row
                  (first (for [group
                               (get initial "groups")

                               row
                               (get group "toggles")

                               :when (= "boolean" (get row "type"))]

                           row))

                  id
                  (get row "id")

                  wanted
                  (not (get row "enabled"))

                  batch
                  {:scope "session"
                   :target_id sid
                   :context_session_id sid
                   :revision (get initial "revision")
                   :changes [{:id id :action "value" :value wanted}]}

                  saved
                  (request-json! :patch "/v1/settings" {:body batch})

                  revision
                  (get-in saved [:body "revision"])]

              (expect (string? sid))
              (expect (re-matches #"[0-9a-f]{64}" (str (get initial "revision")))
                      "The catalog must carry a SHA-256 revision")
              (expect (string? id) "The session catalog must offer a boolean setting")
              (expect (= 200 (:status saved)))
              (expect (= wanted (get (setting-row (:body saved) id) "own_value")))
              (expect (= wanted (get (setting-row (:body saved) id) "enabled")))
              (expect (not= (get initial "revision") revision))
              (let [stale
                    (request-json! :patch
                                   "/v1/settings"
                                   {:body (assoc batch :changes [{:id id :action "inherit"}])})

                    invalid
                    (request-json! :patch
                                   "/v1/settings"
                                   {:body (assoc batch
                                            :revision revision
                                            :unknown true)})

                    current
                    (:body (request-json! :get catalog-path {}))]

                (expect (= 409 (:status stale)))
                (expect (= 400 (:status invalid)))
                (expect (= revision (get current "revision")))
                (expect (= wanted (get (setting-row current id) "own_value"))))
              (let [cleared (request-json! :patch
                                           "/v1/settings"
                                           {:body (assoc batch
                                                    :revision revision
                                                    :changes [{:id id :action "inherit"}])})]
                (expect (= 200 (:status cleared)))
                (expect (nil? (get (setting-row (:body cleared) id) "own_value")))))))
        (finally (when-let [owned @process]
                   (#'binary/kill-tree! owned))
                 (#'binary/delete-tree! dir))))))
