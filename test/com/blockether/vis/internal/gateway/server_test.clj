(ns com.blockether.vis.internal.gateway.server-test
  (:require [lazytest.experimental.interfaces.clojure-test :refer [deftest is testing]]
            [babashka.http-client :as http]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.config :as config]
            [com.blockether.vis.internal.foundation.mcp.core :as mcp-core]
            [com.blockether.vis.internal.gateway.client :as client]
            [com.blockether.vis.internal.gateway.discovery :as discovery]
            [com.blockether.vis.internal.gateway.protocol :as protocol]
            [com.blockether.vis.internal.gateway.server :as server]
            [com.blockether.vis.internal.gateway.state :as state]
            [com.blockether.vis.internal.gateway.wire :as wire]
            [com.blockether.vis.internal.providers :as providers]
            [com.blockether.vis.internal.resources :as resources]
            [com.blockether.vis.internal.slash :as slash]
            [com.blockether.vis.internal.workspace :as workspace]
            [com.blockether.vis.internal.theme :as theme]
            [com.blockether.vis.internal.toggles :as toggles]
            [com.blockether.vis.internal.voice :as voice]
            [com.blockether.vis.internal.loop :as lp]
            [reitit.ring :as rr]
            [ring.adapter.jetty :as jetty]
            [ring.core.protocols :as ring-protocols]
            [ring.middleware.params :as ring-params]))

(defn- rv
  "Resolve a (possibly private) var in the server namespace for with-redefs-fn."
  [sym]
  (ns-resolve 'com.blockether.vis.internal.gateway.server sym))

(defn- server-state [] @(rv 'server-state))

(defn- with-server-state!
  [m f]
  (let [state-atom (server-state)]
    (reset! state-atom m)
    (try (f) (finally (reset! state-atom nil)))))

(defn- with-stop-stub!
  [stops extra f]
  (with-redefs-fn (merge {#'state/running-turn-count (constantly 0)
                          #'server/stop! (fn []
                                           (swap! stops inc))}
                         extra)
    f))

(defn- wait-until
  [pred]
  (loop [remaining 20]
    (cond (pred) true
          (zero? remaining) false
          :else (do (Thread/sleep 10) (recur (dec remaining))))))

(deftest orphan-health-probe-does-not-repair-the-registry
  (let
    [repairs
     (atom 0)

     health-handler
     (rv 'health-handler)]

    (with-redefs-fn {(rv 'ensure-self-registered!) #(swap! repairs inc)}
      (fn []
        (health-handler {:headers {"x-vis-suppress-registry-recovery" "true"}})
        (is (zero? @repairs))
        (health-handler {:headers {}})
        (is (= 1 @repairs))))))

(deftest gateway-router-compiles-with-project-action-routes
  (testing "static project actions do not conflict with the dynamic project-id route"
    (is (some? ((rv 'router) "test-token" [])))))
(deftest mcp-management-handlers-are-sanitized-and-route-mutations-test
  (let
    [body
     {"name" "filesystem" "enabled" true "server" {"transport" "stdio" "command" "npx"}}

     saved
     (atom nil)

     enabled
     (atom nil)

     deleted
     (atom nil)]

    (with-redefs-fn {(rv 'body-json) (constantly body)
                     #'mcp-core/gateway-servers (constantly {"servers" [{"name" "filesystem"
                                                                         "transport" "stdio"
                                                                         "enabled" true
                                                                         "is_connected" false
                                                                         "tools" 0}]})
                     #'mcp-core/save-gateway-server!
                     (fn [name spec]
                       (reset! saved [name spec])
                       {"name" name "transport" "stdio" "enabled" true})
                     #'mcp-core/set-gateway-server-enabled! (fn [name value]
                                                              (reset! enabled [name value])
                                                              {"name" name "enabled" value})
                     #'mcp-core/delete-gateway-server! (fn [name]
                                                         (reset! deleted name)
                                                         {"name" name "is_deleted" true})
                     #'mcp-core/test-gateway-server!
                     (fn [name _spec]
                       {"name" name "is_connected" true "tools" [{"name" "list_files"}]})}
      (fn []
        (let [listed (wire/parse-json (:body ((rv 'mcp-servers-handler) {})))]
          (is (= 200 (:status ((rv 'mcp-servers-handler) {}))))
          (is (= "filesystem" (get-in listed ["servers" 0 "name"])))
          (is (nil? (get-in listed ["servers" 0 "env"]))))
        (is (= 200 (:status ((rv 'save-mcp-server-handler) {}))))
        (is (= ["filesystem" {"transport" "stdio" "command" "npx"}] @saved))
        (is (= 200
               (:status ((rv 'set-mcp-server-enabled-handler)
                          {:path-params {:name "filesystem"}}))))
        (is (= ["filesystem" true] @enabled))
        (is (= 200 (:status ((rv 'delete-mcp-server-handler) {:path-params {:name "filesystem"}}))))
        (is (= "filesystem" @deleted))
        (is (= 200 (:status ((rv 'test-mcp-server-handler) {}))))))))

(deftest mcp-management-refuses-hand-written-servers-test
  (testing "a server declared in a user config file answers 409, never a silent success"
    (let
      [refuse (fn [name]
                (throw (ex-info "declared in a hand-written config file"
                                {:type :mcp/not-managed :server name})))]
      (with-redefs-fn {(rv 'body-json) (constantly {"enabled" false})
                       #'mcp-core/set-gateway-server-enabled! (fn [name _enabled]
                                                                (refuse name))
                       #'mcp-core/delete-gateway-server! (fn [name]
                                                           (refuse name))
                       #'mcp-core/save-gateway-server! (fn [name _spec]
                                                         (refuse name))}
        (fn []
          (doseq
            [handler ['set-mcp-server-enabled-handler 'delete-mcp-server-handler
                      'save-mcp-server-handler]]
            (let [response ((rv handler) {:path-params {:name "team"}})]
              (is (= 409 (:status response)))
              (is (= "not-managed"
                     (get-in (wire/parse-json (:body response)) ["error" "type"]))))))))))

(deftest list-turns-status-filter-routes-to-queued-overlay
  (let
    [sid
     (random-uuid)

     calls
     (atom [])

     request
     {:path-params {:sid (str sid)}}]

    (with-redefs-fn {#'state/soul (fn [actual]
                                    (= sid actual))
                     #'state/list-queued-turns (fn [actual]
                                                 (swap! calls conj [:queued actual])
                                                 [])
                     #'state/list-turns (fn [actual]
                                          (swap! calls conj [:all actual])
                                          [])}
      #(do (is (= 200
                  (:status ((rv 'list-turns-handler)
                             (assoc request :query-params {"status" "queued"})))))
           (is (= [[:queued sid]] @calls))
           (reset! calls [])
           (is (= 200 (:status ((rv 'list-turns-handler) request))))
           (is (= [[:all sid]] @calls))))))

(deftest soul-handler-optionally-includes-queued-turns
  (let
    [sid
     (random-uuid)

     calls
     (atom [])

     request
     {:path-params {:sid (str sid)}}]

    (with-redefs-fn {#'state/soul (fn [actual]
                                    (when (= sid actual) {:id sid}))
                     #'state/list-queued-turns (fn [actual]
                                                 (swap! calls conj actual)
                                                 [{:turn_id "queued-1"}])}
      #(do (let [response ((rv 'soul-handler) (assoc request :query-params {"include" "queued"}))]
             (is (= 200 (:status response)))
             (is (re-find #"\"id\"" (:body response)))
             (is (re-find #"\"queued_turns\"" (:body response)))
             (is (re-find #"\"turn_id\":\"queued-1\"" (:body response))))
           (is (= [sid] @calls))
           (reset! calls [])
           (doseq
             [plain-request [request (assoc request :query-params {"include" "anything-else"})]]
             (let [response ((rv 'soul-handler) plain-request)]
               (is (= 200 (:status response)))
               (is (not (re-find #"\"queued_turns\"" (:body response))))))
           (is (empty? @calls))
           (let
             [response ((rv 'soul-handler)
                         {:path-params {:sid (str (random-uuid))}
                          :query-params {"include" "queued"}})]
             (is (= 404 (:status response))))
           (is (empty? @calls))))))

(deftest draft-management-client-builds-canonical-routes
  (let
    [sent
     (atom [])

     sid
     (str (random-uuid))

     wid
     (str (random-uuid))]

    (with-redefs-fn {#'client/send-json! (fn [method path body]
                                           (swap! sent conj [method path body])
                                           {"workspace" {"root" "/repo"}})}
      (fn []
        (is (= {"root" "/repo"} (client/create-draft! sid "feature-c" false)))
        (is (= {"root" "/repo"} (client/abandon-draft! sid wid "done")))
        (is (= [["POST" (str "/v1/sessions/" sid "/workspace/drafts")
                 {:label "feature-c" :clean false}]
                ["DELETE" (str "/v1/sessions/" sid "/workspace/drafts/" wid) {:reason "done"}]]
               @sent))))))

(deftest foreground-daemon-does-not-refcount-stop
  (testing "a manually-run `vis gateway start` is user-owned, not client-refcounted"
    (let [stops (atom 0)]
      (with-stop-stub! stops
                       {}
                       (fn []
                         (with-server-state! {:managed? false
                                              :saw-client? true
                                              :started-at-ms (System/currentTimeMillis)
                                              :clients {}
                                              :sse-clients {}}
                                             (fn []
                                               ((rv 'maybe-stop-when-idle!))
                                               (Thread/sleep 80)
                                               (is (zero? @stops)))))))))

(deftest managed-daemon-stops-when-last-client-is-gone
  (testing "an auto-spawned gateway self-reaps once there are no clients and no turn"
    (let [stops (atom 0)]
      (with-stop-stub! stops
                       {}
                       (fn []
                         (with-server-state! {:managed? true
                                              :saw-client? true
                                              :started-at-ms (System/currentTimeMillis)
                                              :clients {}
                                              :sse-clients {}}
                                             (fn []
                                               ((rv 'maybe-stop-when-idle!))
                                               (is (wait-until #(= 1 @stops))))))))))

(deftest killed-client-lease-does-not-pin-managed-daemon
  (testing "dead recorded client pids are reaped, so SIGKILLed TUIs still let the daemon die"
    (let [stops (atom 0)]
      (with-stop-stub! stops
                       {#'discovery/pid-alive-cached? (constantly false)
                        (rv 'log-client-lease-warning!) (fn [& _])}
                       (fn []
                         (with-server-state! {:managed? true
                                              :saw-client? true
                                              :started-at-ms (System/currentTimeMillis)
                                              :clients {"c1" {:pid 12345 :kind "clojure-client"}}
                                              :sse-clients {}}
                                             (fn []
                                               ((rv 'reap-client-leases!))
                                               ((rv 'maybe-stop-when-idle!))
                                               (is (wait-until #(= 1 @stops))))))))))

;; Regression (reported: "closing the TUI does not close the gateway"): a TUI that
;; quit released its client lease immediately, but the SSE stream it had open was
;; counted as a separate client until the server's own 15s heartbeat write finally
;; failed - so a managed daemon kept running long after its last TUI was gone, and
;; a pump parked in `.poll` was not woken by closing the socket at all.

(deftest killed-client-sse-stream-does-not-pin-managed-daemon
  (testing "an SSE stream whose owner process is gone is closed and stops counting"
    (let
      [stops
       (atom 0)

       closed
       (atom 0)]

      (with-stop-stub! stops
                       {#'discovery/pid-alive-cached? (constantly false)}
                       (fn []
                         (with-server-state! {:managed? true
                                              :saw-client? true
                                              :started-at-ms (System/currentTimeMillis)
                                              :clients {}
                                              :sse-clients {"s1" {:pid 12345
                                                                  :close! #(swap! closed inc)}}}
                                             (fn []
                                               ((rv 'reap-sse-clients!))
                                               (is (= 1 @closed))
                                               (is (empty? (:sse-clients @(server-state))))
                                               ((rv 'maybe-stop-when-idle!))
                                               (is (wait-until #(= 1 @stops))))))))))

(deftest remote-sse-client-without-a-pid-is-never-reaped
  (testing "a phone/browser stream carries no local pid, so liveness is not guessed"
    (let
      [stops
       (atom 0)

       closed
       (atom 0)]

      (with-stop-stub! stops
                       {#'discovery/pid-alive-cached? (fn [_]
                                                        (throw (ex-info "must not probe" {})))}
                       (fn []
                         (with-server-state! {:managed? true
                                              :saw-client? true
                                              :started-at-ms (System/currentTimeMillis)
                                              :clients {}
                                              :sse-clients {"s1" {:pid nil
                                                                  :close! #(swap! closed inc)}}}
                                             (fn []
                                               ((rv 'reap-sse-clients!))
                                               (is (zero? @closed))
                                               (is (= 1 ((rv 'client-count))))
                                               ((rv 'maybe-stop-when-idle!))
                                               (Thread/sleep 80)
                                               (is (zero? @stops)))))))))

(deftest closing-an-sse-stream-unblocks-a-pump-parked-on-the-heartbeat
  (testing "the writer must exit at once, not at the next 15s keepalive"
    (let
      [out
       (java.io.ByteArrayOutputStream.)

       queue
       (java.util.concurrent.ArrayBlockingQueue. 8)

       dead?
       (volatile! false)

       unsubscribed
       (atom 0)

       close!
       ((rv 'sse-closer) out queue dead? #(swap! unsubscribed inc))

       pump
       (future ((rv 'pump-sse!)
                 out
                 queue
                 dead?
                 (fn [_])))]

      (Thread/sleep 50)
      (close!)
      (is (not= ::timeout (deref pump 2000 ::timeout)))
      (is @dead?)
      (is (= 1 @unsubscribed)))))

(deftest sse-owner-pid-is-read-from-the-client-header
  (testing "only a local vis client sends X-Vis-Client-Pid; anything else owns no pid"
    (let [client-pid (rv 'request-client-pid)]
      (is (= 4242 (client-pid {:headers {"x-vis-client-pid" "4242"}})))
      (is (nil? (client-pid {:headers {"x-vis-client-pid" "phone"}})))
      (is (nil? (client-pid {:headers {}}))))))

(deftest gateway-requests-carry-the-client-pid-header
  (testing "the daemon can only reap a dead owner if every request names its process"
    (let
      [sent
       (atom nil)

       gw-send!
       (ns-resolve 'com.blockether.vis.internal.gateway.client 'gw-send!)]

      (with-redefs-fn {#'http/request (fn [request]
                                        (reset! sent request)
                                        {:status 200 :body "{}"})}
        (fn []
          (gw-send! {:host "127.0.0.1" :port 7890 :secret "s"} "GET" "/v1/events" {:as :stream})
          (is (= (str (discovery/current-pid)) (get-in @sent [:headers "X-Vis-Client-Pid"]))))))))

(deftest client-count-is-constant-time-and-does-not-probe-pids
  (testing "status reads the already-reaped lease map without OS liveness work"
    (with-server-state! {:clients {"c1" {:pid 10} "c2" {:pid 11}} :sse-clients #{"s1"}}
                        (fn []
                          (with-redefs
                            [discovery/pid-alive-cached? (fn [_]
                                                           (throw (ex-info "must not probe" {})))]
                            (is (= 3 ((rv 'client-count)))))))))

(deftest compact-client-leases-removes-dead-and-duplicate-pids
  (testing "one sweep probes each pid once, keeps nil-pid leases, and preserves identity when clean"
    (let
      [checks
       (atom [])

       clients
       {"live-old" {:pid 10} "live-duplicate" {:pid 10} "dead" {:pid 20} "browser" {:pid nil}}

       compact
       (rv 'compact-client-leases)]

      (with-redefs
        [discovery/pid-alive-cached? (fn [pid]
                                       (swap! checks conj pid)
                                       (= 10 pid))]
        (let [{after :clients :keys [dead duplicates]} (compact clients)]
          (is (= 1 dead))
          (is (= 1 duplicates))
          (is (= #{"browser"} (set (filter #(nil? (get-in after [% :pid])) (keys after)))))
          (is (= 2 (count after)))
          (is (= #{10 20} (set @checks))))
        (let
          [clean {"live" {:pid 10} "browser" {:pid nil}}
           {after :clients :keys [dead duplicates]} (compact clean)]

          (is (identical? clean after))
          (is (zero? dead))
          (is (zero? duplicates)))))))

(deftest registering-a-pid-upserts-its-single-process-lease
  (testing "re-registration replaces the old opaque id but preserves other processes and browsers"
    (let
      [register
       (rv 'register-client-lease)

       before
       {"old" {:pid 10} "other" {:pid 20} "browser" {:pid nil}}

       {after :clients :keys [replaced]}
       (register before "new" {:pid 10 :kind "clojure-client"})]

      (is (= 1 replaced))
      (is (= #{"new" "other" "browser"} (set (keys after))))
      (is (= 10 (get-in after ["new" :pid]))))))

(deftest managed-daemon-gets-startup-grace-before-first-client
  (testing "the daemon does not exit in the gap between self-registration and first client lease"
    (let [stops (atom 0)]
      (with-stop-stub! stops
                       {}
                       (fn []
                         (with-server-state! {:managed? true
                                              :saw-client? false
                                              :started-at-ms (System/currentTimeMillis)
                                              :clients {}
                                              :sse-clients #{}}
                                             (fn []
                                               ((rv 'maybe-stop-when-idle!))
                                               (Thread/sleep 80)
                                               (is (zero? @stops)))))))))

(defn- body-stream
  [m]
  (java.io.ByteArrayInputStream. (.getBytes ^String (wire/json-str m) "UTF-8")))

(defn- with-only-engine!
  "Run `f` with EXACTLY `engine` registered (nil = a gateway with no voice engine
   at all), then put the registry back."
  [engine f]
  (let [before (voice/engines)]
    (doseq [e before]
      (voice/unregister-engine! (:id e)))
    (try (when engine (voice/register-engine! engine))
         (f)
         (finally (doseq [e (voice/engines)]
                    (voice/unregister-engine! (:id e)))
                  (doseq [e before]
                    (voice/register-engine! e))))))

(defn- wav-body
  "A RIFF/WAVE header long enough to pass the gateway's cheap pre-filter."
  []
  (let [b (byte-array 64)]
    (System/arraycopy (.getBytes "RIFF" "US-ASCII") 0 b 0 4)
    (System/arraycopy (.getBytes "WAVE" "US-ASCII") 0 b 8 4)
    (java.io.ByteArrayInputStream. b)))

(deftest capabilities-advertise-gateway-voice-and-attachment-contract
  (testing "a gateway without any voice engine reports it honestly"
    (with-only-engine!
      nil
      (fn []
        (let
          [response
           ((rv 'capabilities-handler) {})

           body
           (wire/parse-json (:body response))]

          (is (= 200 (:status response)))
          (is (= 1 (get body "version")))
          (is (true? (get-in body ["features" "attachments" "enabled"])))
          (is (= 8 (get-in body ["features" "attachments" "max_files"])))
          ;; INTAKE ceiling (25MB), not the 5MB provider cap: an oversize still is
          ;; squeezed on the way OUT rather than refused at upload.
          (is (= (* 25 1024 1024) (get-in body ["features" "attachments" "max_file_bytes"])))
          ;; Clips are advertised as ATTACHABLE media with their own ceiling, so a
          ;; companion knows to offer the gallery's videos and how big one may be.
          (is (= ["image/jpeg" "image/png" "image/gif" "image/webp" "image/bmp" "video/mp4"
                  "video/quicktime"]
                 (get-in body ["features" "attachments" "media_types"])))
          (is (= ["video/mp4" "video/quicktime"]
                 (get-in body ["features" "attachments" "video_media_types"])))
          (is (= (* 32 1024 1024) (get-in body ["features" "attachments" "max_video_bytes"])))
          (is (false? (get-in body ["features" "voice" "enabled"])))
          (is (= "unavailable" (get-in body ["features" "voice" "model" "status"])))
          (is (empty? (get-in body ["features" "voice" "engines"])))
          (is (nil? (get-in body ["features" "voice" "selected"])))))))
  (testing "voice advertises the engine CATALOGUE, the selection and the phase vocabulary"
    (with-only-engine! {:id :fake-engine
                        :label "Fake"
                        :transcribe (constantly "hi")
                        :model-state (constantly {:state :ready})}
                       (fn []
                         (let
                           [body (-> ((rv 'capabilities-handler) {})
                                     :body
                                     wire/parse-json)]
                           (is (true? (get-in body ["features" "voice" "enabled"])))
                           (is (= "audio/wav" (get-in body ["features" "voice" "transport"])))
                           (is (= "ready" (get-in body ["features" "voice" "model" "status"])))
                           ;; a client that sees this STREAMS the job's progress instead of
                           ;; holding a socket open for a minute or polling for a percentage
                           (is (true? (get-in body ["features" "voice" "is_async"])))
                           (is (= "sse" (get-in body ["features" "voice" "progress"])))
                           (is (= ["uploading" "queued" "preparing" "transcribing" "done" "failed"]
                                  (get-in body ["features" "voice" "phases"])))
                           (is (= "fake-engine" (get-in body ["features" "voice" "selected"])))
                           (is (= [{"id" "fake-engine" "label" "Fake"}]
                                  (get-in body ["features" "voice" "engines"]))))))))

(deftest voice-post-accepts-the-recording-and-reports-progress-through-a-job
  ;; POST /voice used to BLOCK until the transcript existed: the client could not
  ;; tell "still uploading" from "transcribing", and a long recording was an
  ;; unexplained spinner over a socket that could time out.
  (let
    [sid
     (str (random-uuid))

     release
     (promise)]

    (with-redefs-fn {#'state/soul (constantly {:session-id sid})}
      (fn []
        (voice/reset-jobs!)
        (with-only-engine!
          {:id :slow
           :label "Slow"
           :transcribe (fn [{:keys [on-progress]}]
                         (on-progress {:phase :transcribing :progress 40})
                         @release
                         "the transcript")}
          (fn []
            (let
              [accepted
               ((rv 'voice-handler) {:path-params {:sid sid} :body (wav-body)})

               job
               (wire/parse-json (:body accepted))

               poll
               (fn []
                 (wire/parse-json (:body ((rv 'voice-job-handler)
                                           {:request-method :get
                                            :path-params {:sid sid :job-id (get job "id")}}))))]

              (testing "the upload is answered immediately with a job, not a transcript"
                (is (= 202 (:status accepted)))
                (is (string? (get job "id")))
                (is (= "slow" (get job "engine")))
                (is (false? (get job "is_done")))
                (is (nil? (get job "text"))))
              (testing "the job reports the phase and the percentage while it runs"
                (is (wait-until #(= "transcribing" (get (poll) "phase"))))
                (is (= 40 (get (poll) "progress"))))
              (testing "the finished job carries the text"
                (deliver release :go)
                (is (wait-until #(true? (get (poll) "is_done"))))
                (let [done (poll)]
                  (is (= "done" (get done "phase")))
                  (is (= 100 (get done "progress")))
                  (is (= "the transcript" (get done "text")))))
              (testing "a collected job can be forgotten, and an unknown one is a 404"
                (is (= 200
                       (:status ((rv 'voice-job-handler)
                                  {:request-method :delete
                                   :path-params {:sid sid :job-id (get job "id")}}))))
                (is (= 404
                       (:status ((rv 'voice-job-handler)
                                  {:request-method :get
                                   :path-params {:sid sid :job-id "vj_nope"}}))))))))))))

(defn- sse-jobs
  "Every `data:` payload of an SSE body, parsed, in the order it was written."
  [body]
  (into []
        (comp (map str/split-lines)
              (mapcat (fn [lines]
                        (filter #(str/starts-with? % "data: ") lines)))
              (map #(wire/parse-json (subs % 6))))
        (str/split body #"\n\n")))

(deftest voice-job-progress-is-pushed-as-server-sent-events
  ;; Progress used to be POLLED: one request per tick, a percentage that was
  ;; already up to a poll interval stale when it was painted, and a client left
  ;; guessing when to stop asking. The job's own stream answers all three.
  (let
    [sid
     (str (random-uuid))

     release
     (promise)]

    (with-redefs-fn {#'state/soul (constantly {:session-id sid})}
      (fn []
        (voice/reset-jobs!)
        (with-only-engine!
          {:id :slow
           :label "Slow"
           :transcribe (fn [{:keys [on-progress]}]
                         (on-progress {:phase :transcribing :progress 40})
                         @release
                         "the transcript")}
          (fn []
            (let
              [job-id
               (get (wire/parse-json (:body ((rv 'voice-handler)
                                              {:path-params {:sid sid} :body (wav-body)})))
                    "id")

               response
               ((rv 'voice-job-events-handler)
                 {:request-method :get :path-params {:sid sid :job-id job-id}})

               out
               (java.io.ByteArrayOutputStream.)

               written
               (fn []
                 (String. (.toByteArray out) "UTF-8"))

               stream
               (future (ring-protocols/write-body-to-stream (:body response) response out)
                       (written))]

              (testing "it is an event stream, and no intermediary may buffer it"
                (is (= 200 (:status response)))
                (is (= "text/event-stream" (get-in response [:headers "Content-Type"])))
                (is (= "no" (get-in response [:headers "X-Accel-Buffering"]))))
              (testing "the percentage reaches the client while the engine is still working"
                (is (wait-until #(str/includes? (written) "\"transcribing\""))))
              (deliver release :go)
              (let [body (deref stream 5000 :timeout)]
                (testing "the stream ENDS itself on the terminal frame - nothing to poll"
                  (is (string? body)))
                (let
                  [jobs (sse-jobs (str body))
                   final (last jobs)]

                  (testing "EVERY frame names itself, and none carries a session cursor"
                    (let
                      [frames (->> (str/split (str body) #"\n\n")
                                   (remove str/blank?)
                                   ;; `: ping` heartbeats are comments, not frames.
                                   (remove #(str/starts-with? % ":")))]
                      (is (seq frames))
                      (is (every? #(str/starts-with? % (str "event: " wire/voice-job-event))
                                  frames))
                      (is (not-any? #(str/includes? % "id: ") frames))))
                  (is (seq jobs))
                  (is (= #{job-id} (set (map #(get % "id") jobs))))
                  (is (contains? (set (map #(get % "phase") jobs)) "transcribing"))
                  (is (= 40
                         (apply max
                           (map #(get % "progress")
                                (filter #(= "transcribing" (get % "phase")) jobs)))))
                  (testing "the last frame IS the result: no follow-up request"
                    (is (= "done" (get final "phase")))
                    (is (true? (get final "is_done")))
                    (is (= 100 (get final "progress")))
                    (is (= "the transcript" (get final "text"))))))
              (testing "a job nobody submitted is refused before a stream is opened"
                (is (= 404
                       (:status ((rv 'voice-job-events-handler)
                                  {:request-method :get
                                   :path-params {:sid sid :job-id "vj_nope"}}))))))))))))

;; A job frame used to be recognisable only by the SHAPE of its JSON: the event
;; name was hand-written at the one place that emitted it and no client was ever
;; TOLD it, so a consumer of an SSE socket had to guess whether a frame was a
;; session event or a transcription's progress.
(deftest voice-job-frames-are-named-and-that-name-is-published
  (testing "the frame names itself, and carries no session cursor"
    (let [frame (wire/voice-job-sse-frame {"id" "vj_1" "phase" "transcribing"})]
      (is (= "voice.job" wire/voice-job-event))
      (is (str/starts-with? frame (str "event: " wire/voice-job-event "\n")))
      (is (str/includes? frame "data: {"))
      (is (str/ends-with? frame "\n\n"))
      ;; `id:` is the SESSION log's replay cursor. A job has no log to replay, so
      ;; a client must never mistake this stream for a resumable one.
      (is (not (str/includes? frame "id:")))))
  (testing "capabilities tell a client the name instead of leaving it to guess"
    (let
      [voice (-> ((rv 'capabilities-handler) {})
                 :body
                 wire/parse-json
                 (get-in ["features" "voice"]))]
      (is (= "sse" (get voice "progress")))
      (is (= wire/voice-job-event (get voice "progress_event")))
      (is (true? (get voice "is_async")))))
  (testing "the companion filters on that very string, not on a payload's shape"
    (let [ts (slurp "apps/vis-companion/src/lib/gateway.ts")]
      (is (str/includes? ts (str "export const VOICE_JOB_EVENT = \"" wire/voice-job-event "\";")))
      (is (str/includes? ts "if (event !== VOICE_JOB_EVENT) return;"))
      (is (str/includes? ts "if (frameName === VOICE_JOB_EVENT) return;")))))

(deftest voice-refusals-name-the-reason-instead-of-failing-late
  (let [sid (str (random-uuid))]
    (with-redefs-fn {#'state/soul (constantly {:session-id sid})}
      (fn []
        (testing "no engine at all is 501, not a broken 500"
          (with-only-engine! nil
                             (fn []
                               (is (= 501
                                      (:status ((rv 'voice-handler)
                                                 {:path-params {:sid sid} :body (wav-body)})))))))
        (with-only-engine!
          {:id :fake-engine :transcribe (constantly "hi") :model-state (constantly {:state :ready})}
          (fn []
            (testing "naming an engine nobody registered is the CALLER's 400"
              (is (= 400
                     (:status ((rv 'voice-handler)
                                {:path-params {:sid sid}
                                 :query-params {"engine" "whisper-server"}
                                 :body (wav-body)})))))
            (testing "a body that is not RIFF/WAVE never reaches the engine"
              (is (= 400
                     (:status ((rv 'voice-handler)
                                {:path-params {:sid sid}
                                 :body (java.io.ByteArrayInputStream. (byte-array 64))})))))))
        (testing "an engine that is still preparing answers 425 with its own state"
          (with-only-engine! {:id :downloading
                              :transcribe (constantly "hi")
                              :model-state (constantly {:state :downloading :progress 42})}
                             (fn []
                               (let
                                 [response ((rv 'voice-handler)
                                             {:path-params {:sid sid} :body (wav-body)})
                                  body (wire/parse-json (:body response))]

                                 (is (= 425 (:status response)))
                                 (is (= "downloading" (get body "status")))
                                 (is (= 42 (get body "progress")))))))))))

(deftest theme-handler-shares-the-tui-palette-and-persistence
  (let [saved (atom nil)]
    (with-redefs
      [config/load-config-raw (constantly {"providers" [{"id" "demo"}]
                                           "tui_settings" {"theme_name" "solarized-dark"}})
       config/save-config! #(reset! saved %)]

      (let
        [get-response ((rv 'get-theme-handler) {})
         current (wire/parse-json (:body get-response))]

        (is (= 200 (:status get-response)))
        (is (= "solarized-dark" (get current "id")))
        (is (= (get (theme/theme->web-css-vars (theme/theme "solarized-dark")) "--bg")
               (get-in current ["css_vars" "--bg"])))
        (doseq
          [css-var ["--dialog-title-bg" "--dialog-title-fg" "--dialog-border" "--dialog-shadow"
                    "--dialog-hint" "--input-field-bg" "--button-bg" "--button-fg"
                    "--user-bubble-bg" "--user-bubble-fg" "--user-role-fg" "--ai-bubble-bg"
                    "--ai-bubble-fg" "--ai-role-fg" "--iteration-header-fg" "--iteration-header-bg"
                    "--answer-bg" "--answer-fg" "--md-h1-fg" "--md-h2-fg" "--md-h3-fg" "--code-bg"
                    "--code-fg" "--code-ok-bg" "--code-err-bg" "--code-success" "--code-error"
                    "--code-syntax-keyword" "--code-syntax-special" "--code-syntax-string"
                    "--code-syntax-number" "--code-syntax-comment" "--result-bg" "--code-result"
                    "--code-duration" "--footer-fg" "--footer-muted" "--footer-spinner"]]
          (is (string? (get-in current ["css_vars" css-var])) css-var))
        (is (some #(= "vis-dark" (get % "id")) (get current "themes")))
        ;; Each theme in the list carries its own browser-ready palette so the
        ;; companion can pin a local theme (e.g. light) without a POST.
        (doseq [t (get current "themes")]
          (is (string? (get-in t ["css_vars" "--bg"])) (get t "id"))))
      (let
        [set-response ((rv 'set-theme-handler) {:body (body-stream {:id "vis-dark"})})
         updated (wire/parse-json (:body set-response))]

        (is (= 200 (:status set-response)))
        (is (= "vis-dark" (get updated "id")))
        (is (= "vis-dark" (get-in @saved ["tui_settings" "theme_name"])))
        (is (= [{"id" "demo"}] (get @saved "providers")))))))

(deftest theme-handler-rejects-unknown-themes
  (let [saved? (atom false)]
    (with-redefs
      [config/load-config-raw (constantly {})
       config/save-config! (fn [_]
                             (reset! saved? true))]

      (let [response ((rv 'set-theme-handler) {:body (body-stream {:id "not-a-theme"})})]
        (is (= 404 (:status response)))
        (is (false? @saved?))))))

;; Regression: the global slash endpoint resolved project skills against the gateway
;; process cwd, so nested-project sessions neither saw their own skills nor their children.
(deftest slashes-handler-uses-the-session-workspace-and-includes-native-commands
  (let
    [seen
     (atom nil)

     sid
     (java.util.UUID/randomUUID)

     root
     "/tmp/vis-companion-project"]

    (with-redefs
      [state/session-workspace-info
       (fn [actual-sid]
         (is (= sid actual-sid))
         {"root" root})

       slash/slash-palette
       (fn [channel extra]
         (reset! seen [channel extra (.getPath (workspace/cwd))])
         (conj (vec extra) {:name "/rename" :doc "Rename"}))]

      (let
        [response
         ((rv 'slashes-handler) {:path-params {:sid (str sid)}})

         body
         (wire/parse-json (:body response))]

        (is (= 200 (:status response)))
        (is (= :web (first @seen)))
        (is (some #(= "/help" (:name %)) (second @seen)))
        (is (= (.getCanonicalPath (io/file root)) (nth @seen 2)))
        (is (some #(= "/rename" (get % "name")) (get body "commands")))))))

(deftest slashes-handler-refuses-an-unknown-session
  (let [sid (java.util.UUID/randomUUID)]
    (with-redefs [state/session-workspace-info (constantly nil)]
      (is (= 404 (:status ((rv 'slashes-handler) {:path-params {:sid (str sid)}})))))))

(deftest wrap-auth-accepts-gateway-secret-header
  (testing "a token-gated gateway authenticates the internal client's X-Vis-Gateway-Secret"
    (with-server-state!
      {:require-token? true}
      (fn []
        (let
          [wrap-auth
           (rv 'wrap-auth)

           handler
           (fn [_req]
             {:status 200 :body "ok"})

           app
           (wrap-auth handler "sekret" [])

           req
           (fn [headers]
             {:uri "/v1/sessions" :headers headers})]

          (testing "no credential → 401" (is (= 401 (:status (app (req {}))))))
          (testing "Authorization: Bearer with the right token → 200"
            (is (= 200 (:status (app (req {"authorization" "Bearer sekret"}))))))
          (testing
            "X-Vis-Gateway-Secret carrying the same secret → 200 (the internal client's carrier)"
            (is (= 200 (:status (app (req {"x-vis-gateway-secret" "sekret"}))))))
          (testing "X-Vis-Gateway-Secret with a wrong secret → 401"
            (is (= 401 (:status (app (req {"x-vis-gateway-secret" "nope"})))))))))))

(deftest wrap-auth-disabled-on-loopback-default
  (testing "with auth off (loopback default) every request passes without a token"
    (with-server-state! {:require-token? false}
                        (fn []
                          (let
                            [wrap-auth
                             (rv 'wrap-auth)

                             app
                             (wrap-auth (fn [_req]
                                          {:status 200})
                                        "sekret"
                                        [])]

                            (is (= 200 (:status (app {:uri "/v1/sessions" :headers {}})))))))))

(deftest cors-preflight-is-answered-before-auth
  (testing
    "a cross-origin browser reaches a token-gated gateway: preflight OPTIONS is 204 without a token, and CORS headers ride every response so the browser can read even a 401"
    (with-server-state!
      {:require-token? true}
      (fn []
        (let
          [app
           ((rv 'app) "sekret" [])

           origin
           "http://100.109.18.77:5273"

           preflight
           (app {:request-method :options
                 :uri "/v1/sessions"
                 :headers {"origin" origin
                           "access-control-request-headers" "authorization,content-type"}})

           noauth
           (app {:request-method :get
                 :uri "/v1/sessions"
                 :headers {"origin" origin
                           protocol/protocol-header (str protocol/protocol-version)}})

           authed
           (app {:request-method :get
                 :uri "/v1/sessions"
                 :headers {"origin" origin
                           "authorization" "Bearer sekret"
                           protocol/protocol-header (str protocol/protocol-version)}})]

          (testing "preflight OPTIONS short-circuits auth with 204"
            (is (= 204 (:status preflight)))
            (is (= origin (get-in preflight [:headers "Access-Control-Allow-Origin"])))
            (is (= "true" (get-in preflight [:headers "Access-Control-Allow-Credentials"])))
            (is (= "authorization,content-type"
                   (get-in preflight [:headers "Access-Control-Allow-Headers"]))))
          (testing "a 401 still carries CORS so the browser surfaces the error, not an opaque block"
            (is (= 401 (:status noauth)))
            (is (= origin (get-in noauth [:headers "Access-Control-Allow-Origin"]))))
          (testing "an authenticated request gets its data with CORS"
            (is (= 200 (:status authed)))
            (is (= origin (get-in authed [:headers "Access-Control-Allow-Origin"])))))))))

(deftest parse-multi-sids-parses-and-filters
  (testing "sid[:cursor] comma list — cursor defaults to 0, unknown/non-UUID sids dropped"
    (let
      [sid-a
       (java.util.UUID/randomUUID)

       sid-b
       (java.util.UUID/randomUUID)

       a
       (str sid-a)

       b
       (str sid-b)]

      (with-redefs-fn {#'state/soul (fn [sid]
                                      (contains? #{sid-a sid-b} sid))}
        (fn []
          (let [parse (rv 'parse-multi-sids)]
            ;; sids are parsed to java.util.UUID — the registry's key type
            ;; (path-sid parity). A string key registered a ghost registry
            ;; entry: idle tabs would miss live queue and turn events.
            (is (= [[sid-a 10] [sid-b 0]]
                   (parse {:query-params {"sids" (str a ":10, " b " , zzz:3")}})))
            (is (nil? (parse {:query-params {}})))
            (is (nil? (parse {:query-params {"sids" ""}})))
            (testing "a syntactically valid but UNKNOWN UUID is dropped"
              (is (= [] (parse {:query-params {"sids" (str (java.util.UUID/randomUUID))}}))))
            (testing "Last-Event-ID overrides the cursor for the SINGLE-sid case (native reconnect)"
              (is (= [[sid-a 42]]
                     (parse {:query-params {"sids" (str a ":0")} :headers {"last-event-id" "42"}})))
              (is (= [[sid-a 7]]
                     (parse {:query-params {"sids" a} :headers {"last-event-id" "7"}}))))
            (testing
              "Last-Event-ID is IGNORED for multi-sid (one header can't resume N per-session seqs)"
              (is (= [[sid-a 10] [sid-b 0]]
                     (parse {:query-params {"sids" (str a ":10," b)}
                             :headers {"last-event-id" "42"}}))))
            (testing "a non-numeric Last-Event-ID is ignored"
              (is (= [[sid-a 3]]
                     (parse {:query-params {"sids" (str a ":3")}
                             :headers {"last-event-id" ""}}))))))))))

(deftest transcript-window-params-never-degrade-to-the-full-transcript
  (let
    [q
     (fn [qs]
       (ring-params/assoc-query-params {:path-params {:sid (str (java.util.UUID/randomUUID))}
                                        :query-string qs}
                                       "UTF-8"))

     query-long
     (rv 'query-long)

     seen
     (atom nil)

     call
     (fn [qs]
       (reset! seen ::unset)
       (with-redefs-fn {#'state/transcript-page (fn [_sid opts]
                                                  (reset! seen opts)
                                                  {:turns [] :total 0 :offset 0 :has-more false})}
         #(let
            [r
             ((rv 'transcript-handler) (q qs))]

            [(:status r) @seen])))]

    (testing "ring hands back a VECTOR for a repeated param, so parsing must not throw"
      (is (= ["1" "2"] (get-in (q "limit=1&limit=2") [:query-params "limit"])))
      (is (= 2 (query-long (q "limit=1&limit=2") "limit"))))
    (testing "absent or blank stays nil — that is what 'unwindowed' means"
      (is (nil? (query-long (q "") "limit")))
      (is (nil? (query-long (q "limit=  ") "limit"))))
    (testing "0 and negatives reach the caller verbatim; clamping is transcript-page's job"
      (is (= 0 (query-long (q "limit=0") "limit")))
      (is (= -5 (query-long (q "limit=-5") "limit"))))
    (testing "no window params = the whole transcript, so an older client is unaffected"
      (is (= [200 {:limit nil :offset nil}] (call ""))))
    (testing "?limit=0 honestly means zero rows, never the whole transcript"
      (is (= [200 {:limit 0 :offset nil}] (call "limit=0"))))
    (testing "a duplicated param answers on its last value instead of a 500"
      (is (= [200 {:limit 2 :offset nil}] (call "limit=1&limit=2"))))
    (testing "a present-but-unparsable window param is a 400, not a silent full-transcript fallback"
      (is (= 400 (first (call "limit=abc"))))
      (is (= 400 (first (call "limit=10&offset=nope")))))))

(deftest multi-sse-fans-many-sessions-down-one-stream
  (testing
    "every listed session's events ride ONE connection, tagged by :session_id, deduped per session"
    (with-redefs-fn {#'server/stop! (fn []
                                      nil)}
      (fn []
        (with-server-state!
          {}
          (fn []
            (let
              [multi-sse-body
               (rv 'multi-sse-body)

               write-body
               (requiring-resolve 'ring.core.protocols/write-body-to-stream)

               sid-a
               (str (java.util.UUID/randomUUID))

               sid-b
               (str (java.util.UUID/randomUUID))

               baos
               (java.io.ByteArrayOutputStream.)

               body
               (multi-sse-body [[sid-a 0] [sid-b 0]] false nil)

               fut
               (future (try (write-body body {} baos) (catch Throwable _ nil)))]

              (is (wait-until #(re-find #"subscription.ready"
                                        (String. (.toByteArray baos) "UTF-8"))))
              (state/append-event! sid-a "test.alpha" {:n 1})
              (state/append-event! sid-b "test.beta" {:n 2})
              (state/append-event! sid-a "test.alpha2" {:n 3})
              (is (wait-until #(re-find #"test.alpha2" (String. (.toByteArray baos) "UTF-8"))))
              (future-cancel fut)
              (let [s (String. (.toByteArray baos) "UTF-8")]
                (testing "both sessions surfaced on the single stream"
                  (is (re-find (re-pattern sid-a) s))
                  (is (re-find (re-pattern sid-b) s))
                  (is (re-find #"test.alpha2" s)))
                (testing "per-session dedup keeps each session's own monotonic run"
                  ;; Each session rides ONE subscription.ready control frame
                  ;; (carrying its :session_id in the JSON data) plus one frame per
                  ;; distinct event. The sid appears only in each frame's data, so its
                  ;; count is the reliable per-session dedup signal:
                  ;; sid-a: ready + test.alpha + test.alpha2 = 3; sid-b: ready + test.beta = 2.
                  (is (re-find #"subscription.ready" s))
                  (is (= 3 (count (re-seq (re-pattern sid-a) s))))
                  (is (= 2 (count (re-seq (re-pattern sid-b) s)))))))))))))

;; ── `subscription.ready` states the daemon's OWN turn, so a reconnect needs no probe ──
;; A client that went dark cannot tell "nothing happened" from "I missed the
;; terminal event": its cursor is accepted either way and the replay ring is
;; process memory. The ready frame therefore carries the one fact only the daemon
;; knows — the turn it is running for this session RIGHT NOW — before any replay.
;; Agreement with what the client paints is a positive verdict for zero round
;; trips; disagreement is proof of a gap and the client reconciles once.

(deftest subscription-ready-carries-the-daemons-current-turn
  (testing "the ready frame names the running turn, and says so for an idle session"
    (with-redefs-fn {#'server/stop! (fn []
                                      nil)}
      (fn []
        (with-server-state!
          {}
          (fn []
            (let
              [multi-sse-body
               (rv 'multi-sse-body)

               write-body
               (requiring-resolve 'ring.core.protocols/write-body-to-stream)

               sid-live
               (str (java.util.UUID/randomUUID))

               sid-idle
               (str (java.util.UUID/randomUUID))

               baos
               (java.io.ByteArrayOutputStream.)]

              ;; A turn running in a SIBLING process is exactly the case a
              ;; reconnecting client cannot resolve from its own stream; the registry
              ;; mirrors it (`ingest-mirrored-event!`), so the ready frame can state it.
              (state/append-event! sid-live "test.seed" {:n 1})
              (state/ingest-mirrored-event!
                sid-live
                true
                {"type" "turn.started" "turn_id" "t-live" "session_id" sid-live})
              (let
                [body
                 (multi-sse-body [[sid-live 0] [sid-idle 0]] false nil)

                 fut
                 (future (try (write-body body {} baos) (catch Throwable _ nil)))]

                (is (wait-until #(= 2
                                    (count (re-seq #"\"type\":\"subscription\.ready\""
                                                   (String. (.toByteArray baos) "UTF-8"))))))
                (future-cancel fut)
                (let
                  [s
                   (String. (.toByteArray baos) "UTF-8")

                   ;; Keyed by the session each frame is ABOUT. Matching the whole
                   ;; body would pass even with the two verdicts swapped, since one
                   ;; `true` and one `false` are on the wire either way.
                   ready
                   (into {}
                         (keep (fn [line]
                                 (when (re-find #"subscription\.ready" line)
                                   (when-let
                                     [sid (second (re-find #"\"session_id\"\s*:\s*\"([^\"]+)\""
                                                           line))]
                                     [sid line]))))
                         (re-seq #"data:.*" s))]

                  (is (= #{sid-live sid-idle} (set (keys ready))))
                  (testing "the live session's frame names the turn the daemon holds"
                    (is (re-find #"\"current_turn_id\"\s*:\s*\"t-live\"" (get ready sid-live)))
                    (is (re-find #"\"is_live\"\s*:\s*true" (get ready sid-live))))
                  (testing "the idle session's frame is an explicit negative, not a silence"
                    ;; Without this a client cannot distinguish "no turn" from "old
                    ;; daemon that never shipped the field" — and must probe blindly.
                    (is (re-find #"\"is_live\"\s*:\s*false" (get ready sid-idle)))
                    (is (nil? (re-find #"\"current_turn_id\"\s*:\s*\""
                                       (get ready sid-idle))))))))))))))

;; ── Resource rid rides the QUERY STRING, not a path segment (issue #14) ──
;; A resource id can embed an absolute path — an nREPL id is `nrepl:/Users/…/ws`.
;; Percent-encoded into a PATH SEGMENT its `/` becomes `%2F`, which Jetty rejects
;; with "Ambiguous URI path separator" (400) — that 400 threw out of the client
;; and wedged F4 when you clicked logs on the clojure nREPL. The fix moves rid to
;; the `rid` query param on stop/logs. These lock that in on BOTH halves.

(def ^:private nrepl-rid
  "A real-shaped nREPL resource id: the `/`-embedding absolute path that broke."
  "nrepl:/Users/fierycod/vis")

(deftest resource-client-builds-query-param-urls
  (testing "stop/logs put rid in the ?rid= query, never a path segment (no %2F in path)"
    (let [sent (atom [])]
      (with-redefs-fn {#'client/send-json! (fn [method path & _]
                                             (swap! sent conj [method path])
                                             {:result "ok" :lines ["a"]})}
        (fn []
          (let [sid (str (random-uuid))]
            (client/stop-resource! sid nrepl-rid)
            (client/resource-logs sid nrepl-rid)
            (let [[[_ stop] [_ logs]] @sent]
              (testing "each url ends with the rid encoded in a query param"
                (is (= (str "/v1/sessions/"
                            sid
                            "/resources/stop?rid=nrepl%3A%2FUsers%2Ffierycod%2Fvis")
                       stop))
                (is (= (str "/v1/sessions/"
                            sid
                            "/resources/logs?rid=nrepl%3A%2FUsers%2Ffierycod%2Fvis")
                       logs)))
              (testing
                "the raw rid never leaks into the PATH portion (would trip the ambiguous-slash 400)"
                (doseq [[_ path] @sent]
                  (is (not (re-find #"resources/nrepl" path))))))))))))

(deftest resource-handlers-read-rid-from-query-param
  (testing "stop/logs handlers forward the rid QUERY param to the resources ns"
    (let
      [seen
       (atom [])

       sid
       (str (random-uuid))

       req
       {:path-params {:sid sid} :query-params {"rid" nrepl-rid}}]

      (with-redefs-fn {#'resources/stop! (fn [_ rid]
                                           (swap! seen conj [:stop rid])
                                           {:result "stopped"})
                       #'resources/logs (fn [_ rid]
                                          (swap! seen conj [:logs rid])
                                          ["line-1"])}
        (fn []
          (let
            [stop
             ((rv 'resource-stop-handler) req)

             logs
             ((rv 'resource-logs-handler) req)]

            (testing "each handler answers 200 and threads the exact slash-embedding rid through"
              (is (= 200 (:status stop)))
              (is (= 200 (:status logs)))
              (is (= [[:stop nrepl-rid] [:logs nrepl-rid]] @seen)))
            (testing "logs handler surfaces the captured lines"
              (is (= ["line-1"] (get (wire/parse-json (:body logs)) "lines"))))))))))

(deftest resource-handlers-404-on-unknown-session
  (testing "a non-uuid sid is rejected before any resources call — 404, resources ns untouched"
    (let
      [touched
       (atom false)

       req
       {:path-params {:sid "not-a-uuid"} :query-params {"rid" nrepl-rid}}]

      (with-redefs-fn {#'resources/stop! (fn [& _]
                                           (reset! touched true)
                                           {})
                       #'resources/logs (fn [& _]
                                          (reset! touched true)
                                          nil)}
        (fn []
          (is (= 404 (:status ((rv 'resource-stop-handler) req))))
          (is (= 404 (:status ((rv 'resource-logs-handler) req))))
          (is (false? @touched)))))))

(deftest resource-rid-survives-router-as-query-param
  (testing
    "the client's encoded url routes to the static handler and decodes rid back verbatim (no 400)"
    (let
      [seen
       (atom nil)

       echo
       (fn [request]
         (reset! seen {:sid (get-in request [:path-params :sid])
                       :rid (get-in request [:query-params "rid"])})
         {:status 200 :body "ok"})

       app
       (-> (rr/ring-handler (rr/router [["/v1/sessions/:sid/resources/stop" {:post echo}]
                                        ["/v1/sessions/:sid/resources/logs" {:get echo}]]))
           ring-params/wrap-params)

       sid
       (str (random-uuid))

       ;; exactly the shape the client emits: rid percent-encoded into the query
       enc
       (fn [s]
         (java.net.URLEncoder/encode ^String s "UTF-8"))

       resp
       (app {:request-method :get
             :uri (str "/v1/sessions/" sid "/resources/logs")
             :query-string (str "rid=" (enc nrepl-rid))})]

      (testing "static logs route matches (a path-segment %2F would 404/400 instead)"
        (is (= 200 (:status resp))))
      (testing "the handler sees the sid and the FULL slash-embedding rid, decoded"
        (is (= {:sid sid :rid nrepl-rid} @seen))))))

(deftest toggle-id-wire-contract-test
  (testing "settings rows expose the canonical string id unchanged"
    (toggles/register-toggle! {:id "server_test_toggle" :label "Test" :default false})
    (is (= "server_test_toggle"
           (:id ((rv 'toggle-json) (toggles/toggle-spec "server_test_toggle"))))))
  (testing "the settings mutation endpoint rejects keyword-like, namespaced, and kebab ids"
    (doseq [id [":server_test_toggle" "vis/server_test_toggle" "server-test-toggle"]]
      (is (= 400
             (:status ((rv 'set-setting-handler) {:query-params {"id" id "action" "toggle"}}))))))
  (testing "a canonical but unknown string id remains a distinct 404"
    (is (= 404
           (:status ((rv 'set-setting-handler)
                      {:query-params {"id" "unknown_toggle" "action" "toggle"}}))))))

(deftest get-setting-handler-serves-hidden-rows-test
  (testing "reasoning_level is readable by id even though the settings list hides it"
    (let
      [response
       ((rv 'get-setting-handler) {:path-params {:id "reasoning_level"}})

       row
       (wire/parse-json (:body response))]

      (is (= 200 (:status response)))
      (is (= "reasoning_level" (get row "id")))
      (is (= "enum" (get row "type")))
      (is (seq (get row "choices")))
      (is (string? (get row "value")))))
  (testing "a non-canonical id is a 400 and an unknown one a 404"
    (is (= 400 (:status ((rv 'get-setting-handler) {:path-params {:id "reasoning-level"}}))))
    (is (= 404 (:status ((rv 'get-setting-handler) {:path-params {:id "unknown_toggle"}}))))))

(deftest settings-change-refreshes-cached-extension-bindings-test
  (toggles/register-toggle! {:id "server_test_toggle" :label "Test" :default false})
  (toggles/set-enabled! "server_test_toggle" false)
  (let [synced (atom 0)]
    (with-redefs [lp/sync-cached-extension-symbols! #(swap! synced inc)]
      (let
        [response ((rv 'set-setting-handler)
                    {:query-params {"id" "server_test_toggle" "action" "toggle"}})]
        (is (= 200 (:status response))))
      (is (= 1 @synced)))
    (toggles/set-enabled! "server_test_toggle" false)))

(deftest provider-models-handler-serves-live-catalog-daemon-side
  (testing
    "GET /v1/providers/:id/models fetches the LIVE catalog DAEMON-side (gateway owns OAuth token) and emits snake_case hidden_count"
    (with-redefs-fn {#'providers/default-model-names (constantly ["claude-opus-4-8"])
                     #'providers/model-options (fn [_ _ show-all?]
                                                 {:models ["claude-opus-4-8" "claude-sonnet-5"]
                                                  :hidden-count (if show-all? 0 4)})}
      (fn []
        (let
          [resp
           ((rv 'provider-models-handler) {:path-params {:provider-id "anthropic-coding-plan"}})

           body
           (wire/parse-json (:body resp))]

          (is (= 200 (:status resp)))
          (is (= ["claude-opus-4-8" "claude-sonnet-5"] (get body "models")))
          (is (= 4 (get body "hidden_count"))))
        (let
          [resp
           ((rv 'provider-models-handler)
             {:path-params {:provider-id "anthropic-coding-plan"}
              :query-params {"show_all" "true"}})

           body
           (wire/parse-json (:body resp))]

          (is (= 0 (get body "hidden_count"))))))))

(deftest set-session-model-handler-validates-against-the-gateway-fleet
  (testing
    "PATCH /model pins only providers THIS gateway serves; the model name stays free (live catalog)"
    (let
      [sid
       (str (java.util.UUID/randomUUID))

       wrote
       (atom nil)

       body
       (fn [m]
         {:path-params {:sid sid}
          :body (java.io.ByteArrayInputStream. (.getBytes (wire/json-str m) "UTF-8"))})]

      (with-redefs-fn {;; The PICKER fleet is what both clients offer: configured providers
                       ;; PLUS presets that are authenticated but not yet written into
                       ;; vis.yml. Validating the pin against `configured-providers`
                       ;; answered 400 for a provider the picker had just listed.
                       #'providers/picker-fleet (constantly [{:id :zai-coding-plan}
                                                             {:id :anthropic-coding-plan}
                                                             {:id :openai-codex}])
                       #'providers/configured-providers-cached (constantly [{:id :zai-coding-plan}])
                       #'state/set-session-model! (fn [_sid p m]
                                                    (reset! wrote [p m]))
                       #'state/session-model (fn [_sid]
                                               @wrote)}
        (fn []
          (testing "a configured provider is accepted"
            (let
              [resp ((rv 'set-session-model-handler)
                      (body {:provider "zai-coding-plan" :model "glm-5.2"}))]
              (is (= 200 (:status resp)))
              (is (= ["zai-coding-plan" "glm-5.2"] @wrote))))
          (testing "a model outside vis.yml is fine — the live catalog offers more"
            (is (= 200
                   (:status ((rv 'set-session-model-handler)
                              (body {:provider "zai-coding-plan" :model "glm-live-preview"})))))
            (is (= ["zai-coding-plan" "glm-live-preview"] @wrote)))
          (testing "a provider the picker offers but vis.yml does not configure is accepted"
            (reset! wrote nil)
            (is (= 200
                   (:status ((rv 'set-session-model-handler)
                              (body {:provider "openai-codex" :model "gpt-5.4"})))))
            (is (= ["openai-codex" "gpt-5.4"] @wrote)))
          (testing "an unknown provider is a 400 and writes NOTHING"
            (reset! wrote :untouched)
            (let
              [resp ((rv 'set-session-model-handler)
                      (body {:provider "not-on-this-gateway" :model "x"}))]
              (is (= 400 (:status resp)))
              (is (= "unknown-provider" (get-in (wire/parse-json (:body resp)) ["error" "type"])))
              (is (= :untouched @wrote))))
          (testing "blank/omitted provider still clears or pins by model alone"
            (reset! wrote nil)
            (is (= 200
                   (:status ((rv 'set-session-model-handler)
                              (body {:provider "  " :model "glm-5.2"})))))
            (is (= [nil "glm-5.2"] @wrote))))))))

(deftest router-handler-assembles-string-keyed-fleet-with-status
  (testing "GET /v1/router returns every model plus the primary and fallback pairs"
    (with-redefs-fn
      {#'providers/picker-fleet
       (constantly
         [{:id :anthropic-coding-plan
           :base-url "https://api.anthropic.com/v1"
           :models [{:name "claude-opus-4-8"} {:name "claude-sonnet-5"}]}
          {:id :zai-coding-plan :base-url "https://api.z.ai/v1" :models [{:name "glm-5.2"}]}])
       #'providers/default-selection (constantly {:provider-id :anthropic-coding-plan
                                                  :model "claude-sonnet-5"})
       #'providers/fallback-selection (constantly {:provider-id :zai-coding-plan :model "glm-5.2"})
       #'providers/provider-status (constantly {:is-authenticated true :source :auth-file})
       #'providers/provider-limits-safe
       (constantly
         {:provider-id :anthropic-coding-plan :status :ok :static {} :dynamic {:limits []}})}
      (fn []
        (let
          [resp
           ((rv 'router-handler) {})

           provs
           (get (wire/parse-json (:body resp)) "providers")

           p0
           (first provs)

           p1
           (second provs)]

          (is (= 200 (:status resp)))
          (is (= "anthropic-coding-plan" (get p0 "id")))
          (is (= "https://api.anthropic.com/v1" (get p0 "base_url")))
          (is (= ["claude-opus-4-8" "claude-sonnet-5"] (get p0 "models")))
          (is (true? (get p0 "is_default")))
          (is (= "claude-sonnet-5" (get p0 "default_model")))
          ;; the primary row is NEVER also the fallback row
          (is (false? (get p0 "is_fallback")))
          (is (nil? (get p0 "fallback_model")))
          (is (true? (get p1 "is_fallback")))
          (is (= "glm-5.2" (get p1 "fallback_model")))
          (is (false? (get p1 "is_default")))
          (is (nil? (get p1 "default_model")))
          ;; connection verdict is the snake_case STRING key — no keyword restore
          (is (true? (get-in p0 ["status" "is_authenticated"])))
          (is (= "auth-file" (get-in p0 ["status" "source"])))
          (is (every? string? (keys (get p0 "status"))))
          ;; limits ride embedded, string-keyed too
          (is (= "ok" (get-in p0 ["limits" "status"]))))))))

(deftest router-default-handler-tags-primary-and-fallback
  (let
    [saved
     (atom nil)

     cleared
     (atom 0)

     fleet
     [{:id :anthropic-coding-plan :models [{:name "claude-fable-5"}]}
      {:id :zai-coding-plan :models [{:name "glm-5.2"}]}]

     body
     (fn [m]
       {:body (java.io.ByteArrayInputStream. (.getBytes (wire/json-str m) "UTF-8"))})

     patch!
     (fn [m]
       ((rv 'router-default-handler) (body m)))]

    (with-redefs
      [providers/picker-fleet
       (constantly fleet)

       providers/default-selection
       (constantly {:provider-id :anthropic-coding-plan :model "claude-fable-5"})

       providers/fallback-selection
       (constantly {:provider-id :zai-coding-plan :model "glm-5.2"})

       providers/save-default-selection!
       (fn [provider model source]
         (reset! saved [:primary provider model source])
         {:provider-id :anthropic-coding-plan :model "claude-fable-5"})

       providers/save-fallback-selection!
       (fn [provider model source]
         (reset! saved [:fallback provider model source])
         {:provider-id :zai-coding-plan :model "glm-5.2"})

       providers/clear-fallback-selection!
       (fn [_source]
         (swap! cleared inc)
         nil)]

      (testing "a roleless PATCH still tags the PRIMARY, and the answer carries BOTH tags"
        (let
          [resp
           (patch! {"provider" "anthropic-coding-plan" "model" "claude-fable-5"})

           out
           (wire/parse-json (:body resp))]

          (is (= 200 (:status resp)))
          (is (= [:primary "anthropic-coding-plan" "claude-fable-5" :gateway] @saved))
          (is (= "anthropic-coding-plan" (get out "default_provider")))
          (is (= "claude-fable-5" (get out "default_model")))
          (is (= "zai-coding-plan" (get out "fallback_provider")))
          (is (= "glm-5.2" (get out "fallback_model")))))
      (testing "role fallback writes the FALLBACK tag and never touches the primary one"
        (let [resp (patch! {"provider" "zai-coding-plan" "model" "glm-5.2" "role" " FallBack "})]
          (is (= 200 (:status resp)))
          (is (= [:fallback "zai-coding-plan" "glm-5.2" :gateway] @saved))))
      (testing "a blank fallback CLEARS the tag, while a blank primary stays a 400"
        (is (= 200 (:status (patch! {"role" "fallback" "model" "  "}))))
        (is (= 1 @cleared))
        (is (= 400 (:status (patch! {"model" "   "})))))
      (testing "an unknown role is refused before anything is written"
        (reset! saved :untouched)
        (let
          [resp
           (patch! {"provider" "zai-coding-plan" "model" "glm-5.2" "role" "tertiary"})

           err
           (get (wire/parse-json (:body resp)) "error")]

          (is (= 400 (:status resp)))
          (is (= "invalid-request" (get err "type")))
          (is (= :untouched @saved))))
      (testing "the daemon's refusal — a fallback on the primary's provider — becomes a 400"
        (with-redefs
          [providers/save-fallback-selection!
           (fn [_provider _model _source]
             (throw (ex-info "the fallback must name a DIFFERENT provider than the primary"
                             {:type :vis/invalid-fallback-provider})))]
          (let
            [resp (patch!
                    {"provider" "anthropic-coding-plan" "model" "claude-fable-5" "role" "fallback"})
             err (get (wire/parse-json (:body resp)) "error")]

            (is (= 400 (:status resp)))
            (is (re-find #"DIFFERENT provider" (get err "message")))))))))

(deftest gateway-prometheus-runtime-metrics-test
  (let
    [text ((rv 'prometheus-text)
            {:tokens-input 12
             :tokens-output 7
             :turns-executing 2
             :turns-waiting 1
             :env-cache-size 3
             :env-heap-pressure true
             :jvm-heap-used-bytes 1024
             :jvm-gc-count-total 4
             :jvm-thread-count 9})]
    (testing "preserves the existing labelled token counter"
      (is (re-find #"vis_turn_tokens_total\{kind=\"input\"\} 12" text))
      (is (re-find #"vis_turn_tokens_total\{kind=\"output\"\} 7" text)))
    (testing "exports resource-pressure and concurrency gauges"
      (is (re-find #"vis_turns_executing 2" text))
      (is (re-find #"vis_turns_waiting 1" text))
      (is (re-find #"vis_env_heap_pressure 1" text))
      (is (re-find #"vis_jvm_heap_used_bytes 1024" text)))))

(defn- non-loopback-ipv4
  "First live non-loopback IPv4 address, i.e. the kind of concrete host `--pair`
   binds. nil on a machine with no such interface, where the mirror is moot."
  []
  (->> (enumeration-seq (java.net.NetworkInterface/getNetworkInterfaces))
       (filter (fn [^java.net.NetworkInterface i]
                 (.isUp i)))
       (mapcat (fn [^java.net.NetworkInterface i]
                 (enumeration-seq (.getInetAddresses i))))
       (filter (fn [a]
                 (instance? java.net.Inet4Address a)))
       (remove (fn [^java.net.InetAddress a]
                 (.isLoopbackAddress a)))
       (map (fn [^java.net.InetAddress a]
              (.getHostAddress a)))
       first))

(deftest pair-bind-still-answers-on-loopback
  (testing
    "a concrete non-loopback bind (what --pair picks) also serves 127.0.0.1,
            so the local TUI attaches to THIS gateway instead of seeing a free port
            and spawning a second one"
    (if-let [host (non-loopback-ipv4)]
      (let
        [port (with-open [s (java.net.ServerSocket. 0)]
                (.getLocalPort s))
         server (jetty/run-jetty (constantly {:status 200 :headers {} :body "ok"})
                                 {:port port
                                  :host host
                                  :join? false
                                  :configurator ((rv 'loopback-mirror-configurator) port)})]

        (try (is (= #{host "127.0.0.1"}
                    (set (map (fn [^org.eclipse.jetty.server.ServerConnector c]
                                (.getHost c))
                              (.getConnectors ^org.eclipse.jetty.server.Server server)))))
             (is (= "ok" (slurp (str "http://" host ":" port "/"))))
             (is (= "ok" (slurp (str "http://127.0.0.1:" port "/"))))
             (finally (.stop ^org.eclipse.jetty.server.Server server))))
      (is (nil? (non-loopback-ipv4)) "no non-loopback interface here; nothing to mirror"))))

(deftest sse-cursor-clamps-a-client-ahead-of-the-gateway-counter
  ;; A client keeps its replay cursor as a monotonic max across reconnects, but
  ;; the gateway's seq counter is per-process: a restarted daemon (or a session
  ;; entry seeded at zero) numbers BELOW what the app already saw. Serving that
  ;; stale cursor verbatim seeds the connection's `seq > last-seq` dedup guard
  ;; above every frame the next turn will ever emit — a connected, heartbeating
  ;; stream that silently delivers NOTHING until the app is killed.
  (let
    [resolve-cursor
     (rv 'resolve-sse-cursor)

     registry
     @(ns-resolve 'com.blockether.vis.internal.gateway.state 'registry)

     sid
     (java.util.UUID/randomUUID)]

    (swap! registry assoc (str sid) {:next-seq 12})
    (try (testing "a cursor past the high-water resolves to the live tail"
           (is (= 12 (resolve-cursor sid 5000))))
         (testing "an in-range cursor is honoured verbatim" (is (= 5 (resolve-cursor sid 5))))
         (testing "the negative live-only sentinel is unchanged"
           (is (= 12 (resolve-cursor sid -1))))
         (finally (swap! registry dissoc (str sid))))))

(deftest mcp-kill-start-and-oauth-routes-test
  (testing "runtime kill/start and every headless OAuth leg answer through the ring layer"
    (let
      [flow
       {"flow_id" "f-1"
        "server" "remote"
        "kind" "pkce"
        "url" "https://auth.example.test/authorize"
        "status" "pending"}

       calls
       (atom [])]

      (with-redefs-fn
        {(rv 'body-json) (constantly {"flow_id" "f-1" "input" "https://cb.example.test/?code=abc"})
         #'mcp-core/kill-gateway-server! (fn [name]
                                           (swap! calls conj [:kill name])
                                           {"name" name "is_killed" true})
         #'mcp-core/start-gateway-server! (fn [name]
                                            (swap! calls conj [:start name])
                                            {"name" name "is_killed" false})
         #'mcp-core/start-gateway-server-auth! (fn [name]
                                                 (swap! calls conj [:auth-start name])
                                                 flow)
         #'mcp-core/complete-gateway-server-auth! (fn [flow-id input]
                                                    (swap! calls conj
                                                      [:auth-complete flow-id input])
                                                    (assoc flow "status" "ok"))
         #'mcp-core/poll-gateway-server-auth! (fn [flow-id]
                                                (swap! calls conj [:auth-poll flow-id])
                                                flow)
         #'mcp-core/cancel-gateway-server-auth! (fn [flow-id]
                                                  {"flow_id" flow-id "is_cancelled" true})
         #'mcp-core/logout-gateway-server-auth! (fn [name]
                                                  {"server" name "is_authorized" false})}
        (fn []
          (let [params {:path-params {:name "remote"}}]
            (is (= 200 (:status ((rv 'kill-mcp-server-handler) params))))
            (is (= 200 (:status ((rv 'start-mcp-server-handler) params))))
            ;; The flow crosses the wire snake_cased, and the id is the ONLY handle
            ;; a client ever holds: the verifier and the token stay in the daemon.
            (let [started (wire/parse-json (:body ((rv 'mcp-auth-start-handler) params)))]
              (is (= "f-1" (get started "flow_id")))
              (is (= "pkce" (get started "kind")))
              (is (nil? (get started "code_verifier"))))
            (is (= "ok"
                   (get (wire/parse-json (:body ((rv 'mcp-auth-complete-handler) params)))
                        "status")))
            (is (= 200 (:status ((rv 'mcp-auth-poll-handler) params))))
            (is (= 200 (:status ((rv 'mcp-auth-cancel-handler) params))))
            (is (= 200 (:status ((rv 'mcp-auth-logout-handler) params))))
            (is (= [[:kill "remote"] [:start "remote"] [:auth-start "remote"]
                    [:auth-complete "f-1" "https://cb.example.test/?code=abc"] [:auth-poll "f-1"]]
                   @calls))))))))

(deftest mcp-oauth-flow-errors-map-to-status-test
  (testing "a missing flow id is 400 and an expired one 404 — never a 500"
    (with-redefs-fn {(rv 'body-json) (constantly {})}
      (fn []
        (is (= 400 (:status ((rv 'mcp-auth-poll-handler) {:path-params {:name "remote"}}))))))
    (with-redefs-fn {(rv 'body-json) (constantly {"flow_id" "gone"})
                     #'mcp-core/poll-gateway-server-auth!
                     (fn [flow-id]
                       (throw (ex-info "Unknown or expired MCP auth flow"
                                       {:type :mcp/oauth-flow-not-found :flow-id flow-id})))}
      (fn []
        (let [response ((rv 'mcp-auth-poll-handler) {:path-params {:name "remote"}})]
          (is (= 404 (:status response)))
          (is (= "oauth-flow-not-found"
                 (get-in (wire/parse-json (:body response)) ["error" "type"]))))))))

(deftest admin-stop-handler-names-its-requester
  (testing
    "POST /v1/admin/stop takes the whole ring request (so the log can name who killed a busy daemon) and still stops"
    (let [stops (atom 0)]
      (with-stop-stub! stops
                       {}
                       (fn []
                         (with-server-state!
                           {:managed? false :clients {} :sse-clients #{}}
                           (fn []
                             (let
                               [res ((rv 'stop-handler)
                                      {:remote-addr "127.0.0.1"
                                       :headers {"user-agent" "vis-agent/test"}})]
                               (is (= 200 (:status res)))
                               (is (re-find #"stopping" (str (:body res))))
                               (is (loop [n 0]
                                     (cond (pos? (long @stops)) true
                                           (> n 100) false
                                           :else (do (Thread/sleep 20) (recur (inc n)))))
                                   "the handler stops the daemon asynchronously")))))))))

(deftest signal-forensics-is-idempotent-and-restorable
  (testing
    "the daemon installs signal handlers once (so an unexplained death names its signal) and can restore the JVM defaults"
    (let
      [install
       (rv 'install-signal-forensics!)

       restore
       (rv 'restore-signal-forensics!)

       installed
       (install)]

      (try (is (map? installed))
           (is (contains? installed "TERM"))
           (is (contains? installed "INT"))
           (is (nil? (install)) "a second install is a no-op")
           (finally (restore installed)))
      (is (nil? @@(rv 'signal-forensics))
          "restoring clears the installed marker so a later daemon can re-install"))))

(deftest a-stray-signal-never-stops-a-detached-daemon
  (testing "policy: only a terminal this daemon OWNS may Ctrl-C it; SIGTERM always stops"
    (let [disposition (rv 'signal-disposition)]
      (is (= :exit (disposition {:signal "TERM" :managed? true :interactive? false}))
          "SIGTERM is the deliberate stop and must keep draining")
      (is (= :exit (disposition {:signal "TERM" :managed? false :interactive? true})))
      (is (= :exit (disposition {:signal "INT" :managed? false :interactive? true}))
          "Ctrl-C in the tab a FOREGROUND gateway runs in still stops it")
      (is (= :exit (disposition {:signal "HUP" :managed? false :interactive? true})))
      (is (= :ignore (disposition {:signal "INT" :managed? true :interactive? true}))
          "a managed daemon is nobody's foreground job, whatever stdio it inherited")
      (is (= :ignore (disposition {:signal "INT" :managed? false :interactive? false}))
          "no controlling terminal => the INT came from someone else's process group")
      (is (= :ignore (disposition {:signal "HUP" :managed? true :interactive? false})))))
  (testing "a REAL SIGINT delivered to a managed daemon is logged and survived"
    (let
      [install
       (rv 'install-signal-forensics!)

       restore
       (rv 'restore-signal-forensics!)

       disposition
       (rv 'signal-disposition)

       installed
       (install {:managed? true})]

      ;; GUARD: fire an actual signal only once the pure policy proves this JVM's
      ;; handler cannot exit — `:managed? true` is what we installed with, so the
      ;; `:interactive?` the handler captured cannot change the verdict. A regression
      ;; fails on this assertion instead of killing the test runner. `installed` is nil
      ;; when another test already owns the handlers; then there is nothing to prove.
      (is (= :ignore (disposition {:signal "INT" :managed? true :interactive? true})))
      (is (= :ignore (disposition {:signal "INT" :managed? true :interactive? false})))
      (try (when (and (seq installed)
                      (= :ignore (disposition {:signal "INT" :managed? true :interactive? true}))
                      (= :ignore (disposition {:signal "INT" :managed? true :interactive? false})))
             (let
               [pid
                (.pid (java.lang.ProcessHandle/current))

                p
                (.start (ProcessBuilder. ^java.util.List ["/bin/sh" "-c" (str "kill -INT " pid)]))]

               (.waitFor p)
               (Thread/sleep 300)
               (is (.isAlive (java.lang.ProcessHandle/current))
                   "a stray SIGINT must leave the daemon serving every other session")
               ;; Non-vacuous: the SAME `kill` shape against a process with the DEFAULT
               ;; disposition really does kill it, so surviving above is the handler.
               (let
                 [ctl
                  (.start (ProcessBuilder. ^java.util.List
                                           ["/bin/sh" "-c" "kill -INT $$; sleep 2; echo survived"]))

                  out
                  (slurp (.getInputStream ctl))]

                 (.waitFor ctl)
                 (is (not (str/includes? out "survived"))
                     "the control process must die of the very same signal"))))
           (finally (restore installed))))))

(deftest the-shutdown-hook-names-who-called-system-exit
  (let [culprit (rv 'exit-culprit)]
    (testing "the thread parked in System/exit is named; JDK plumbing is dropped"
      (let
        [r (culprit {"main" ["java.base/java.lang.Object.wait0(Native Method)"
                             "java.base/java.lang.Thread.join(Thread.java:1327)"
                             "java.base/java.lang.Shutdown.runHooks(Shutdown.java:130)"]
                     "vis-turn-3"
                     ["java.base/java.lang.Shutdown.exit(Shutdown.java:176)"
                      "java.base/java.lang.Runtime.exit(Runtime.java:112)"
                      "java.base/java.lang.System.exit(System.java:1901)"
                      "java.base/jdk.internal.reflect.DirectMethodHandleAccessor.invoke(x:1)"
                      "com.blockether.vis.ext.language_clojure.reflection$c.invoke(refl.clj:120)"
                      "clojure.lang.AFn.run(AFn.java:22)"]})]
        (is (= "vis-turn-3" (get r "thread"))
            "the thread that called exit, not the one running the hooks")
        (is (= "com.blockether.vis.ext.language_clojure.reflection$c.invoke(refl.clj:120)"
               (first (get r "frames")))
            "the first frame must be the CALLER, module prefixes and all")
        (is (= 2 (count (get r "frames"))))))
    (testing "a thread merely running the hooks is not accused, and nothing exiting is nil"
      (is (nil? (culprit {"main" ["java.base/java.lang.Shutdown.runHooks(Shutdown.java:130)"
                                  "clojure.main$main.invoke(main.clj:1)"]})))
      (is (nil? (culprit {}))))))

;;; ── Fleet membership over the wire ──────────────────────────────────────────
;; Adding a provider used to be TUI-only: the gateway exposed operations on
;; providers that were already configured and nothing that could create one, so
;; the companion could never grow a fleet.

(defn- json-body [m] {:body (java.io.ByteArrayInputStream. (.getBytes (wire/json-str m) "UTF-8"))})

(defn- with-stub-fleet!
  "Router payload stubs so a mutation handler can echo the fleet back."
  [fleet f]
  (with-redefs-fn {#'providers/picker-fleet (constantly fleet)
                   #'providers/default-selection (constantly nil)
                   #'providers/fallback-selection (constantly nil)
                   #'providers/provider-status (constantly {:is-authenticated false})
                   #'providers/provider-limits-safe (constantly nil)}
    f))

(deftest provider-presets-handler-lists-what-can-still-be-added
  (with-redefs-fn {#'providers/available-presets (constantly [{:id :lmstudio
                                                               :label "LM Studio"
                                                               :base-url "http://localhost:1234/v1"
                                                               :api-style :openai
                                                               :default-models ["local-model"]}
                                                              {:id :anthropic-coding-plan
                                                               :label "Anthropic"
                                                               :default-models
                                                               ["claude-sonnet-5"]}])}
    (fn []
      (let
        [presets
         (get (wire/parse-json (:body ((rv 'provider-presets-handler) {}))) "presets")

         [local oauth]
         presets]

        (is (= ["lmstudio" "anthropic-coding-plan"] (mapv #(get % "id") presets)))
        (is (= "LM Studio" (get local "label")))
        (is (= "http://localhost:1234/v1" (get local "base_url")))
        (is (= "openai" (get local "api_style")))
        (is (= ["local-model"] (get local "models")))
        ;; a local runtime needs no credential and OWNS its base url
        (is (= "none" (get local "auth_kind")))
        (is (true? (get local "is_local")))
        (is (= "oauth" (get oauth "auth_kind")))
        (is (false? (get oauth "is_local")))))))

(deftest add-provider-handler-writes-the-preset-into-the-fleet
  (let
    [added
     (atom nil)

     post!
     (fn [m]
       ((rv 'add-provider-handler) (json-body m)))]

    (with-redefs-fn {#'config/provider-template (fn [pid]
                                                  (when (= :lmstudio pid)
                                                    {:id :lmstudio
                                                     :label "LM Studio"
                                                     :base-url "http://localhost:1234/v1"
                                                     :api-style :openai
                                                     :default-models ["local-model"]}))
                     #'providers/configured-providers (constantly [{:id :zai-coding-plan}])
                     #'providers/add-config-provider! (fn [cfg source]
                                                        (reset! added [cfg source]))}
      (fn []
        (with-stub-fleet!
          [{:id :lmstudio :models [{:name "local-model"}]}]
          (fn []
            (testing "a local provider takes the base url the caller owns, trailing slash and all"
              (let [resp (post! {:id "lmstudio" :base_url "http://10.0.0.5:1234/v1/"})]
                (is (= 200 (:status resp)))
                (is (= [{:id :lmstudio
                         :models [{:name "local-model"}]
                         :base-url "http://10.0.0.5:1234/v1"
                         :api-style :openai} :gateway]
                       @added))
                ;; the answer IS the new fleet — the caller repaints from it
                (is (= ["lmstudio"]
                       (mapv #(get % "id") (get (wire/parse-json (:body resp)) "providers"))))))
            (testing "no base url keeps the preset default"
              (reset! added nil)
              (is (= 200 (:status (post! {:id "lmstudio"}))))
              (is (= "http://localhost:1234/v1" (:base-url (first @added)))))
            (testing "an unknown preset is a 404 and writes nothing"
              (reset! added nil)
              (let [resp (post! {:id "not-a-provider"})]
                (is (= 404 (:status resp)))
                (is (= "unknown-provider" (get-in (wire/parse-json (:body resp)) ["error" "type"])))
                (is (nil? @added))))
            (testing "a blank id is a 400" (is (= 400 (:status (post! {:id "   "})))))
            (testing "an already-configured provider is a 409, never a duplicate row"
              (reset! added nil)
              (with-redefs-fn {#'providers/configured-providers (constantly [{:id :lmstudio}])}
                (fn []
                  (is (= 409 (:status (post! {:id "lmstudio"}))))
                  (is (nil? @added)))))))))))

(deftest remove-provider-handler-drops-the-provider-and-echoes-the-fleet
  (let [removed (atom nil)]
    (with-redefs-fn {#'providers/remove-provider! (fn [pid source]
                                                    (reset! removed [pid source])
                                                    (= :lmstudio pid))}
      (fn []
        (with-stub-fleet!
          [{:id :zai-coding-plan :models [{:name "glm-5.2"}]}]
          (fn []
            (let
              [resp ((rv 'remove-provider-handler) {:path-params {:provider-id "lmstudio"}})
               payload (wire/parse-json (:body resp))]

              (is (= 200 (:status resp)))
              (is (= [:lmstudio :gateway] @removed))
              (is (true? (get payload "is_removed")))
              (is (= ["zai-coding-plan"] (mapv #(get % "id") (get payload "providers")))))
            (testing "removing what is not configured is not an error"
              (let [resp ((rv 'remove-provider-handler) {:path-params {:provider-id "ghost"}})]
                (is (= 200 (:status resp)))
                (is (false? (get (wire/parse-json (:body resp)) "is_removed")))))))))))

(deftest delete-project-blast-radius-is-explicit-on-the-wire-test
  ;; The default DELETE only ever scattered members back to project-less, and no
  ;; client could remove a project together with its sessions. Recursion is
  ;; OPT-IN and answers with the ids, because the caller has to prune local rows,
  ;; snapshots and unsent drafts without racing a re-read.
  (let
    [pid
     (java.util.UUID/randomUUID)

     sids
     [(str (java.util.UUID/randomUUID)) (str (java.util.UUID/randomUUID))]

     calls
     (atom [])

     stub
     (fn
       ([p] (swap! calls conj [p nil])
        {:project_id (str p) :deleted_session_ids [] :session_count 0})
       ([p opts] (swap! calls conj [p opts])
        {:project_id (str p) :deleted_session_ids sids :session_count (count sids)}))

     handler
     (rv 'delete-project-handler)]

    (with-redefs-fn {#'state/delete-project! stub}
      (fn []
        (testing "a plain DELETE stays the body-less 204 scatter"
          (let [response (handler {:path-params {:pid (str pid)} :query-params {}})]
            (is (= 204 (:status response)))
            (is (nil? (:body response)))
            (is (= [[pid nil]] @calls))))
        (reset! calls [])
        (testing "is_recursive=true is 200 and reports every deleted session id"
          (let
            [response
             (handler {:path-params {:pid (str pid)} :query-params {"is_recursive" "true"}})

             body
             (wire/parse-json (:body response))]

            (is (= 200 (:status response)))
            (is (= [[pid {:is-recursive true}]] @calls))
            (is (= sids (get body "deleted_session_ids")))
            (is (= 2 (get body "session_count")))
            (is (= (str pid) (get body "project_id")))))
        (reset! calls [])
        (testing "a malformed project id is a 404, never a silent empty recursion"
          (let
            [response (handler {:path-params {:pid "not-a-uuid"}
                                :query-params {"is_recursive" "true"}})]
            (is (= 404 (:status response)))
            (is (empty? @calls))))))))

;; Regression: the live `iteration.completed` descriptors DROP model-only
;; artifacts and then RE-NUMBER what survives, while the byte endpoint indexed
;; the UNFILTERED row list. On any iteration whose first artifact was
;; `audience="model"` every index was off by one — the companion's artifacts
;; sheet fetched the wrong bytes for every tile, and the artifact deliberately
;; hidden from the human was handed to it at index 0.
(deftest attachment-byte-endpoint-indexes-the-list-the-descriptors-number
  (let
    [b64
     #(.encodeToString (java.util.Base64/getEncoder) (.getBytes ^String % "UTF-8"))

     iid
     "00000000-0000-0000-0000-0000000000ab"

     rows
     [{:kind "image"
       :media-type "image/png"
       :filename "for-the-model.png"
       :audience "model"
       :size 10
       :base64 (b64 "MODEL-ONLY")}
      {:kind "image"
       :media-type "image/png"
       :filename "for-the-human.png"
       :audience "both"
       :size 5
       :base64 (b64 "SHOWN")}]

     fetch
     (fn [idx]
       ((rv 'attachment-bytes-handler)
         {:path-params {:sid (str (random-uuid)) :iid iid :idx (str idx)}}))]

    (with-redefs-fn {#'state/iteration-attachments (constantly rows)}
      (fn []
        (let
          [descriptors ((ns-resolve 'com.blockether.vis.internal.gateway.state
                                    'live-attachment-descriptors)
                         iid)]
          (testing "the human is offered exactly the artifacts meant for them"
            (is (= [{:index 0 :filename "for-the-human.png"}]
                   (mapv #(select-keys % [:index :filename]) descriptors))))
          (testing "descriptor index 0 serves THAT artifact's bytes"
            (let [response (fetch 0)]
              (is (= 200 (:status response)))
              (is (= "SHOWN" (slurp (:body response))))))
          (testing "nothing past the last descriptor resolves" (is (= 404 (:status (fetch 1)))))
          (testing "a model-only artifact is never served, at any index"
            (is (= []
                   (vec (for
                          [idx (range 4)
                           :let [response (fetch idx)]
                           :when (and (= 200 (:status response))
                                      (= "MODEL-ONLY" (slurp (:body response))))]

                          idx))))))))))

;; The session-creation UX picks a workspace root by RECOGNITION, so the gateway
;; has to be able to show the machine's own folders. `/v1/fs` is that surface and
;; nothing more: directories only, the two facts a chooser reads (how much is in
;; it, which branch it has out), and `~` meaning the GATEWAY user's home.
(defn- fs-child
  ^java.io.File [^java.io.File dir & segments]
  (reduce (fn [^java.io.File f seg]
            (java.io.File. f (str seg)))
          dir
          segments))

(defn- fs-temp-root
  ^java.io.File []
  (let
    [dir (.toFile (java.nio.file.Files/createTempDirectory
                    "vis-fs-test"
                    (make-array java.nio.file.attribute.FileAttribute 0)))]
    (.deleteOnExit dir)
    dir))

(deftest browse-fs-shows-only-folders-and-names-the-repos
  (let
    [root
     (fs-temp-root)

     _
     (run! #(.mkdir (fs-child root %)) ["beta" "alpha" ".hidden"])

     _
     (spit (fs-child root "readme.txt") "a file is not a folder")

     _
     (.mkdir (fs-child root "alpha" ".git"))

     _
     (spit (fs-child root "alpha" ".git" "HEAD") "ref: refs/heads/main\n")

     listing
     (wire/parse-json (:body ((rv 'browse-fs-handler)
                               {:query-params {"path" (.getAbsolutePath root)}})))

     entries
     (get listing "entries")]

    (testing
      "a listing is the directories inside the path, alphabetical, dotfolders and files dropped"
      (is (= ["alpha" "beta"] (mapv #(get % "name") entries)))
      (is (= (.getAbsolutePath root) (get listing "path")))
      (is (= (.getAbsolutePath (.getParentFile root)) (get listing "parent")))
      (is (= (System/getProperty "user.home") (get listing "home")))
      (is (false? (get listing "is_truncated"))))
    (testing "a worktree carries its branch, a plain folder carries none"
      (let
        [alpha
         (first entries)

         beta
         (second entries)]

        (is (true? (get alpha "is_repo")))
        (is (= "main" (get alpha "branch")))
        (is (= 1 (get alpha "entry_count")))
        (is (false? (get beta "is_repo")))
        (is (nil? (get beta "branch")))))
    (testing "`~` and a blank path both mean the gateway user's home, never the phone's"
      (is (= (System/getProperty "user.home")
             (get (wire/parse-json (:body ((rv 'browse-fs-handler) {:query-params {"path" "~"}})))
                  "path")))
      (is (= (System/getProperty "user.home")
             (get (wire/parse-json (:body ((rv 'browse-fs-handler) {:query-params {}}))) "path"))))
    (testing "a path that is not a directory is refused by name"
      (let
        [response ((rv 'browse-fs-handler)
                    {:query-params {"path" (.getAbsolutePath (fs-child root "readme.txt"))}})]
        (is (= 404 (:status response)))
        (is (= "not-a-directory" (get-in (wire/parse-json (:body response)) ["error" "type"])))))))

(deftest mkdir-creates-one-folder-where-the-picker-is-standing
  (let
    [root
     (fs-temp-root)

     made
     ((rv 'create-directory-handler) (json-body {:path (.getAbsolutePath root) :name "gamma"}))

     body
     (wire/parse-json (:body made))]

    (testing "the new folder answers as a listing row, so the picker can select it immediately"
      (is (= 201 (:status made)))
      (is (= "gamma" (get body "name")))
      (is (= (.getAbsolutePath (fs-child root "gamma")) (get body "path")))
      (is (false? (get body "is_repo")))
      (is (.isDirectory (fs-child root "gamma"))))
    (testing "creating it twice is not an error: the folder asked for exists"
      (is (= 201
             (:status ((rv 'create-directory-handler)
                        (json-body {:path (.getAbsolutePath root) :name "gamma"}))))))
    (testing "a name is ONE segment — a picker that accepts `a/../b` writes outside what it showed"
      (doseq [name ["a/b" ".." "." "  " "x\\y"]]
        (let
          [response ((rv 'create-directory-handler)
                      (json-body {:path (.getAbsolutePath root) :name name}))]
          (is (= 400 (:status response)) (str "refuses " (pr-str name)))
          (is (= "invalid-request" (get-in (wire/parse-json (:body response)) ["error" "type"]))))))
    (testing "a parent that does not exist is a 404, not a silent mkdir -p"
      (let
        [response ((rv 'create-directory-handler)
                    (json-body {:path (.getAbsolutePath (fs-child root "nowhere")) :name "delta"}))]
        (is (= 404 (:status response)))
        (is (= "not-a-directory" (get-in (wire/parse-json (:body response)) ["error" "type"])))))))
