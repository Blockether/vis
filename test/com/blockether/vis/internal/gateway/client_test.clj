(ns com.blockether.vis.internal.gateway.client-test
  "Unit coverage for the self-heal decision logic in [[client/ensure-gateway-serving!]].

   The point under test is that the /ui 404 self-heal is NON-DESTRUCTIVE: it only
   force-restarts a stale daemon that is genuinely idle, treats a transport blip as
   \"leave it alone\", and never confuses either with a real 404."
  (:require [babashka.http-client :as http]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is testing]]
            [com.blockether.vis.internal.gateway.client :as client]
            [com.blockether.vis.internal.gateway.discovery :as discovery]))

(defn- rv
  "Resolve a (possibly private) var in the client namespace for with-redefs-fn."
  [sym]
  (ns-resolve 'com.blockether.vis.internal.gateway.client sym))

(def ^:private fake-entry {:host "127.0.0.1" :port 7890 :pid 4242 :secret "s"})

;; Regression: direct API debugging reimplemented registry discovery and authentication
;; instead of using the gateway client's canonical transport.
(deftest request-uses-the-canonical-authenticated-client
  (let [calls
        (atom [])

        response
        {:status 201 :body "created"}]

    (with-redefs-fn {(rv 'ensure-gateway!) (fn []
                                             (swap! calls conj [:gateway])
                                             fake-entry)
                     (rv 'ensure-client!) (fn [entry]
                                            (swap! calls conj [:client entry])
                                            "client-id")
                     (rv 'gw-send!) (fn [entry method path opts]
                                      (swap! calls conj [:request entry method path opts])
                                      response)}
      (fn []
        (is (= response
               (client/request! :post "/v1/debug" {:body {:hello "world"} :timeout-ms 1200})))
        (is (= [[:gateway] [:client fake-entry]
                [:request fake-entry "POST" "/v1/debug" {:body {:hello "world"} :timeout-ms 1200}]]
               @calls))))))

;; Regression: the TUI spoke raw gateway HTTP for these two reads, so a channel
;; extension had to reach past the facade into this namespace to make either one.
(deftest channel-reads-answer-data-or-nil-when-the-daemon-cannot
  (let [seen
        (atom [])

        respond
        (fn [status body]
          (fn [method path opts]
            (swap! seen conj [method path opts])
            {:status status :body body}))]

    (with-redefs-fn {#'client/request!
                     (respond 200 "{\"artifacts\":[{\"filename\":\"decision.html\"}]}")}
      (fn []
        (is (= [{"filename" "decision.html"}] (client/session-artifacts "session-1")))))
    (with-redefs-fn {#'client/request! (respond 200 "{\"attachments\":{\"max_bytes\":10}}")}
      (fn []
        (is (= {"attachments" {"max_bytes" 10}} (client/capabilities)))))
    (testing "each read is bounded, because a person is waiting at an open dialog"
      (is (= [[:get "/v1/sessions/session-1/artifacts" {:timeout-ms 5000}]
              [:get "/v1/capabilities" {:timeout-ms 5000}]]
             @seen)))
    (with-redefs-fn {#'client/request! (respond 503 "")}
      (fn []
        (is (nil? (client/session-artifacts "session-1")))
        (is (nil? (client/capabilities)))))))
(defn- await-value
  "Wait for a background cache refresh to publish its expected value."
  [read expected]
  (loop [attempts 100]
    (let [value (read)]
      (cond (= expected value) value
            (zero? attempts) nil
            :else (do (Thread/sleep 10) (recur (dec attempts)))))))

;; Reported in Vis session a64d44c2-8228-455f-926e-b3381f19a93b: the TUI had
;; no canonical View action with which a live job row could change shared selection.
(deftest view-action-uses-the-one-kind-independent-route
  (let [request (atom nil)]
    (with-redefs-fn {(rv 'send-json!) (fn [method path body]
                                        (reset! request [method path body])
                                        {"action" "select"
                                         "view_id" "view-1"
                                         "is_accepted" true
                                         "node_id" "jobs"
                                         "item_ids" ["macos"]})}
      (fn []
        (is (= {:action :select
                :view-id "view-1"
                :is-accepted true
                :node-id "jobs"
                :item-ids ["macos"]}
               (client/view-action! "session-1"
                                    "view-1"
                                    {:action :select :node-id "jobs" :item-ids ["macos"]})))
        (is (= ["POST" "/v1/sessions/session-1/views/view-1/actions"
                {:action "select" :node-id "jobs" :item-ids ["macos"]}]
               @request))))))

(deftest ensure-project-for-root-uses-project-action-route
  (let [request (atom nil)]
    (with-redefs-fn {(rv 'send-json!) (fn [method path body]
                                        (reset! request [method path body])
                                        {:id "project-id"})}
      (fn []
        (is (= {:id "project-id"} (client/ensure-project-for-root! "/workspace" "Vis")))
        (is (= ["POST" "/v1/projects/actions/ensure" {:root "/workspace" :name "Vis"}]
               @request))))))

;; Regression, Vis session ae259fdd-2712-4591-8f12-e1cdff30b208: the TUI
;; had no gateway-owned catalog and initialized a second GraalPy runtime locally.
(deftest session-slashes-uses-the-gateway-catalog-with-a-cold-load-timeout
  (let [calls (atom [])]
    (with-redefs-fn {(rv 'ensure-gateway!) (constantly fake-entry)
                     (rv 'ensure-client!) (fn [entry]
                                            (swap! calls conj [:client entry])
                                            "client-id")
                     (rv 'send-json-with-entry!)
                     (fn [entry method path body opts]
                       (swap! calls conj [:request entry method path body opts])
                       {"commands" [{"name" "/python-echo" "doc" "Echo"}]})}
      (fn []
        (is (= [{"name" "/python-echo" "doc" "Echo"}] (client/session-slashes "session-1" :tui)))
        (is (= [[:client fake-entry]
                [:request fake-entry "GET" "/v1/sessions/session-1/slashes?channel=tui" nil
                 {:timeout-ms 120000}]]
               @calls))))))

(deftest ensure-client-registers-once-from-canonical-string-keyed-response
  (let [client-id-atom
        @(rv 'client-id)

        previous
        @client-id-atom

        calls
        (atom 0)

        ensure-client
        (rv 'ensure-client!)]

    (try (reset! client-id-atom nil)
         (with-redefs-fn {(rv 'send-json-with-entry!) (fn [_entry method path body]
                                                        (swap! calls inc)
                                                        (is (= "POST" method))
                                                        (is (= "/v1/clients" path))
                                                        (is (integer? (:pid body)))
                                                        {"client_id" "lease-1"})
                          (rv 'ensure-release-hook!) (fn [])}
           (fn []
             (is (= "lease-1" (ensure-client fake-entry)))
             (is (= "lease-1" (ensure-client fake-entry)))
             (is (= 1 @calls))))
         (finally (reset! client-id-atom previous)))))


;; Regression, Vis session ae259fdd-2712-4591-8f12-e1cdff30b208: concurrent
;; TUI startup callbacks each entered gateway discovery and repeated the full wait.
(deftest concurrent-gateway-ensure-is-single-flight-per-database
  (let [cached-atom
        @(rv 'cached-entry)

        fresh-until-atom
        @(rv 'entry-fresh-until-ns)

        previous-cached
        @cached-atom

        previous-fresh-until
        @fresh-until-atom

        calls
        (atom 0)

        start
        (promise)

        rendezvous
        (java.util.concurrent.CyclicBarrier. 2)

        discover
        (fn [& _]
          (swap! calls inc)
          (try (.await rendezvous 250 java.util.concurrent.TimeUnit/MILLISECONDS)
               (catch java.util.concurrent.TimeoutException _ nil)
               (catch java.util.concurrent.BrokenBarrierException _ nil))
          {:mode :spawned :entry fake-entry})]

    (try (reset! cached-atom nil)
         (reset! fresh-until-atom 0)
         (with-redefs-fn {(rv 'remote-gateway) (constantly nil)
                          (rv 'db-target) (constantly "/tmp/single-flight/vis.db")
                          #'discovery/registry-fresh? (constantly false)
                          #'discovery/pid-alive? (constantly true)
                          (rv 'discover-or-recover!) discover
                          (rv 'bounce-stale-daemon!) (constantly {:bounced? false})
                          (rv 'assert-compatible!) identity}
           (fn []
             (let [workers (mapv (fn [_]
                                   (future @start (client/ensure-gateway!)))
                                 (range 2))]
               (deliver start true)
               (is (= [fake-entry fake-entry] (mapv #(deref % 2000 ::timeout) workers)))
               (is (= 1 @calls) "one process performs discovery while peers reuse its result"))))
         (finally (reset! cached-atom previous-cached)
                  (reset! fresh-until-atom previous-fresh-until)))))
(deftest authenticated-loopback-orphan-is-stopped-and-replaced
  (let [token-file
        (java.io.File/createTempFile "vis-gateway-token-" ".txt")

        calls
        (atom [])]

    (try (spit token-file "stable-secret\n")
         (with-redefs-fn {#'discovery/default-token-file (fn []
                                                           token-file)
                          #'discovery/pid-alive? (constantly true)
                          #'discovery/read-registry (constantly nil)
                          (rv 'port-free?) (constantly true)
                          (rv 'gw-send!) (fn [entry method path opts]
                                           (swap! calls conj [entry method path opts])
                                           (case path
                                             "/healthz"
                                             {:status 200
                                              :body
                                              (str "{\"status\":\"ok\",\"secret_match\":true,"
                                                   "\"pid\":9154,\"db\":\"/tmp/recover/vis.db\"}")}

                                             "/v1/admin/stop"
                                             {:status 200}))}
           (fn []
             (is (true? ((rv 'retire-loopback-orphan!) "/tmp/recover/vis.db" "127.0.0.1" 7890)))
             (is (= [[{:host "127.0.0.1" :port 7890 :secret "stable-secret"} "GET" "/healthz"
                      {:timeout-ms 1500 :headers {"X-Vis-Suppress-Registry-Recovery" "true"}}]
                     [{:host "127.0.0.1" :port 7890 :secret "stable-secret" :pid 9154} "POST"
                      "/v1/admin/stop" {}]]
                    @calls))))
         (finally (.delete token-file)))))

(deftest registered-loopback-gateway-is-never-retired
  (let [calls (atom [])]
    (with-redefs-fn {#'discovery/read-registry (constantly fake-entry)
                     (rv 'gw-send!) (fn [& args]
                                      (swap! calls conj args))}
      (fn []
        (is (nil? ((rv 'retire-loopback-orphan!) "/tmp/registered/vis.db" "127.0.0.1" 7890)))
        (is (empty? @calls))))))

(deftest occupied-orphan-port-never-spawns-a-bind-loser
  (let [spawns
        (atom 0)

        ex
        (with-redefs-fn {(rv 'retire-loopback-orphan!) (constantly nil)
                         (rv 'port-free?) (constantly false)
                         #'discovery/await-registry! (fn [_db _probe opts]
                                                       (is (= 3000 (:timeout-ms opts)))
                                                       (is (= 100 (:poll-ms opts)))
                                                       nil)
                         #'discovery/discover-or-start! (fn [& _]
                                                          (swap! spawns inc)
                                                          {:mode :spawned :entry fake-entry})}
          (fn []
            (try ((rv 'discover-or-recover!) "/tmp/orphan/vis.db" "127.0.0.1" 7890)
                 nil
                 (catch clojure.lang.ExceptionInfo e e))))]

    (is (= :gateway/orphaned-port (:type (ex-data ex))))
    (is (true? (:vis/user-error (ex-data ex))))
    (is (= "127.0.0.1" (:host (ex-data ex))))
    (is (= 7890 (:port (ex-data ex))))
    (is (zero? @spawns) "an occupied port can never enter the daemon spawn path")))

(deftest occupied-port-allows-a-registering-daemon-to-win-the-race
  (let [spawns (atom 0)]
    (with-redefs-fn {(rv 'retire-loopback-orphan!) (constantly nil)
                     (rv 'port-free?) (constantly false)
                     #'discovery/await-registry! (fn [_db _probe _opts]
                                                   fake-entry)
                     #'discovery/discover-or-start! (fn [& _]
                                                      (swap! spawns inc)
                                                      nil)}
      (fn []
        (is (= {:mode :awaited :entry fake-entry}
               ((rv 'discover-or-recover!) "/tmp/race/vis.db" "127.0.0.1" 7890)))
        (is (zero? @spawns))))))

(deftest stale-registry-stop-does-not-report-a-listening-gateway-as-stopped
  (let [server
        (java.net.ServerSocket. 0)

        port
        (.getLocalPort server)]

    (try (let [result (with-redefs-fn {(rv 'db-target) (constantly "/tmp/orphan/vis.db")
                                       #'discovery/read-registry (constantly (assoc fake-entry
                                                                               :port port))
                                       #'discovery/registry-fresh? (constantly false)
                                       #'discovery/pid-alive? (constantly false)}
                        (fn []
                          (client/stop-daemon!)))]
           (is (not= "stopped" (:status result))
               "a listening configured endpoint must not be reported as stopped")
           (is (= :gateway/orphaned-daemon (:type result)))
           (is (= "127.0.0.1" (:host result)))
           (is (= port (:port result))))
         (finally (.close server)))))

(def ^:private idle-status
  {"status" "running" "managed" true "clients" 0 "running_turns" 0 "pid" 4242})

(deftest daemon-idle-is-the-one-definition-of-a-free-bounce
  (testing "a managed daemon nobody holds is free to release"
    (is (true? (:idle? (client/daemon-idle? idle-status))))
    (is (= :idle (:reason (client/daemon-idle? idle-status)))))
  (testing "work in progress is never aborted for a release that was optional"
    (is (= :clients (:reason (client/daemon-idle? (assoc idle-status "clients" 2)))))
    (is (= :running-turns (:reason (client/daemon-idle? (assoc idle-status "running_turns" 1))))))
  (testing "a daemon somebody started by hand belongs to them, idle or not"
    (is (= :user-owned (:reason (client/daemon-idle? (assoc idle-status "managed" false))))))
  (testing "nothing running is nothing to stop"
    (is (= :not-running (:reason (client/daemon-idle? {"status" "stopped"})))))
  (testing "a count in a shape this build does not know refuses instead of throwing"
    ;; The peer whose status decides a bounce is by definition a build this one did
    ;; not ship with: a count it cannot read must never read as zero, and must never
    ;; take the attach path down with it.
    (doseq [odd [{} [] :two "two"]]
      (is (= :not-running (:reason (client/daemon-idle? (assoc idle-status "clients" odd)))))
      (is (= :not-running (:reason (client/daemon-idle? (assoc idle-status "running_turns" odd))))))
    (is (= :not-running (:reason (client/daemon-idle? nil))))
    (is (false? (:bounce? (client/stale-bounce-verdict {:ours "0.1.40"
                                                        :theirs "0.1.39"
                                                        :status (assoc idle-status
                                                                  "clients" "many")})))))
  (testing "a numeric count is read whichever wire shape carried it"
    (is (= :clients (:reason (client/daemon-idle? (assoc idle-status "clients" "2")))))
    (is (= :idle (:reason (client/daemon-idle? (assoc idle-status "clients" 0.0))))))
  (testing "the same rule, calibrated for a caller that is itself attached"
    (is (true? (:idle? (client/daemon-idle? (assoc idle-status "clients" 1)
                                            {:tolerate-clients 1}))))
    (is (true? (:idle? (client/daemon-idle? (assoc idle-status "managed" false)
                                            {:user-owned-ok? true}))))))

(deftest stop-if-idle-leaves-a-daemon-somebody-is-using-alone
  (let [stops (atom 0)]
    (with-redefs-fn {(rv 'remote-gateway) (constantly nil)
                     #'client/status (constantly (assoc idle-status
                                                   "clients" 2
                                                   "running_turns" 1))
                     #'client/stop-daemon! (fn []
                                             (swap! stops inc)
                                             {:status "stopped"})}
      (fn []
        (let [verdict (client/stop-daemon-if-idle!)]
          (is (false? (:stopped? verdict)))
          (is (= :clients (:reason verdict)))
          (is (zero? @stops) "an update must never abort an open session"))))))

(deftest stop-if-idle-releases-an-unused-managed-daemon
  (let [stops (atom 0)]
    (with-redefs-fn {(rv 'remote-gateway) (constantly nil)
                     #'client/status (constantly idle-status)
                     #'client/stop-daemon! (fn []
                                             (swap! stops inc)
                                             {:status "stopped" :stopping false})}
      (fn []
        (let [verdict (client/stop-daemon-if-idle!)]
          (is (true? (:stopped? verdict)))
          (is (= 1 @stops)))))))

;; Regression, session 78b0c0b5-f5ba-453f-97ee-af0a85f72d25: the freshly
;; updated protocol-3 runtime could probe its protocol-2 gateway, but the safety
;; status and stop requests were refused before the idle daemon could be released.
(deftest update-can-release-an-idle-gateway-across-the-old-protocol-boundary
  (let [handshake
        @(rv 'gateway-handshake*)

        previous
        @handshake

        calls
        (atom [])]

    (reset! handshake {:protocol 2 :min-client 2 :min-gateway 2 :version "0.1.41"})
    (try
      (with-redefs-fn
        {(rv 'remote-gateway) (constantly nil)
         (rv 'db-target) (constantly "/tmp/vis-update-control-test.db")
         #'discovery/read-registry (constantly fake-entry)
         #'discovery/registry-fresh? (constantly true)
         #'http/request
         (fn [{:keys [method headers] :as request}]
           (swap! calls conj request)
           (if (= "2" (get headers "X-Vis-Min-Gateway-Protocol"))
             {:status 200
              :body
              (if (= :get method)
                "{\"status\":\"running\",\"managed\":true,\"clients\":0,\"running_turns\":0,\"pid\":4242}"
                "{\"status\":\"stopped\",\"stopping\":false}")}
             {:status 400 :body "{\"message\":\"Update the gateway\"}"}))}
        (fn []
          (let [result (client/stop-daemon-if-idle!)]
            (is (true? (:stopped? result)))
            (is (= [:get :post] (mapv :method @calls)))
            (is (every? #(= "2" (get-in % [:headers "X-Vis-Min-Gateway-Protocol"])) @calls)))))
      (finally (reset! handshake previous)))))

;; The state `vis-agent update` leaves behind when a session was open: the daemon
;; keeps serving the old image, so the next client to find it unused replaces it.
(deftest a-daemon-older-than-this-build-is-replaced-only-when-nobody-is-using-it
  (testing "an idle daemon on the old image is the whole reason this rule exists"
    (let [verdict (client/stale-bounce-verdict
                    {:ours "0.1.40" :theirs "0.1.39" :status idle-status})]
      (is (true? (:bounce? verdict)))
      (is (= "0.1.39" (:from verdict)))
      (is (= "0.1.40" (:to verdict)))))
  (testing "no version is worth aborting somebody's work for"
    (is (= :clients
           (:reason (client/stale-bounce-verdict
                      {:ours "0.1.40" :theirs "0.1.39" :status (assoc idle-status "clients" 1)}))))
    (is (= :running-turns
           (:reason (client/stale-bounce-verdict {:ours "0.1.40"
                                                  :theirs "0.1.39"
                                                  :status (assoc idle-status "running_turns" 1)}))))
    (is (= :user-owned
           (:reason (client/stale-bounce-verdict {:ours "0.1.40"
                                                  :theirs "0.1.39"
                                                  :status (assoc idle-status "managed" false)})))))
  (testing "same build, an older client, or a dev checkout: nothing to pick up"
    (is (= :fresh
           (:reason (client/stale-bounce-verdict
                      {:ours "0.1.40" :theirs "0.1.40" :status idle-status}))))
    (is (= :fresh
           (:reason (client/stale-bounce-verdict
                      {:ours "0.1.39" :theirs "0.1.40" :status idle-status}))))
    (is (= :fresh
           (:reason (client/stale-bounce-verdict
                      {:ours "dev" :theirs "0.1.39" :status idle-status})))))
  (testing "a dev checkout has no release to be ordered by, so its commit decides"
    (is (true? (:bounce? (client/stale-bounce-verdict {:ours "dev"
                                                       :theirs "dev"
                                                       :our-build "aaa111aaa111"
                                                       :their-build "bbb222bbb222"
                                                       :status idle-status}))))
    (is (= :clients
           (:reason (client/stale-bounce-verdict {:ours "dev"
                                                  :theirs "dev"
                                                  :our-build "aaa111aaa111"
                                                  :their-build "bbb222bbb222"
                                                  :status (assoc idle-status "clients" 1)})))
        "a commit is worth no more of somebody's work than a version is")
    (is (= :fresh
           (:reason (client/stale-bounce-verdict {:ours "dev"
                                                  :theirs "dev"
                                                  :our-build "aaa111aaa111"
                                                  :their-build "aaa111aaa111"
                                                  :status idle-status}))))
    (is (= :fresh
           (:reason (client/stale-bounce-verdict
                      {:ours "dev" :theirs "dev" :our-build "aaa111aaa111" :status idle-status})))
        "a daemon too old to advertise a build says nothing about being stale"))
  (testing "a status nobody could read is not evidence of an idle daemon"
    (is (false? (:bounce? (client/stale-bounce-verdict
                            {:ours "0.1.40" :theirs "0.1.39" :status nil}))))))

(deftest a-stale-daemon-is-bounced-once-per-process-never-in-a-loop
  (let [stops
        (atom 0)

        guard
        @(rv 'stale-bounce-attempted?)

        handshake
        @(rv 'gateway-handshake*)

        previous
        @handshake]

    (reset! guard false)
    (reset! handshake {:protocol 2 :min-client 2 :min-gateway 2 :version "0.1.39"})
    (try (with-redefs-fn {(requiring-resolve
                            'com.blockether.vis.internal.gateway.protocol/release-version)
                          (constantly "0.1.40")
                          (rv 'report-version-bounce!) (constantly nil)
                          (rv 'send-json-with-entry!) (fn [& _]
                                                        idle-status)
                          (rv 'db-target) (constantly "/tmp/vis-stale-bounce-test.db")
                          (rv 'await-daemon-down!) (constantly true)
                          #'client/stop-daemon! (fn []
                                                  (swap! stops inc)
                                                  {:status "stopped" :stopping false})}
           (fn []
             (let [bounce! (rv 'bounce-stale-daemon!)]
               (is (true? (:bounced? (bounce! fake-entry))))
               (is (= :checked (:reason (bounce! fake-entry)))
                   "a daemon that comes back old costs one restart, never a restart loop")
               (is (= 1 @stops)))))
         (finally (reset! guard false) (reset! handshake previous)))))

;; The same pickup for a source checkout, where both halves say "dev": the daemon's
;; advertised commit is the whole difference, and it must work with no native image
;; and no release version anywhere in sight.
(deftest a-dev-daemon-on-another-commit-is-replaced-by-its-build-id
  (let [stops
        (atom 0)

        guard
        @(rv 'stale-bounce-attempted?)

        handshake
        @(rv 'gateway-handshake*)

        previous
        @handshake]

    (reset! guard false)
    (reset! handshake
      {:protocol 2 :min-client 2 :min-gateway 2 :version "dev" :build "bbb222bbb222"})
    (try (with-redefs-fn {(requiring-resolve
                            'com.blockether.vis.internal.gateway.protocol/release-version)
                          (constantly "dev")
                          (requiring-resolve 'com.blockether.vis.internal.gateway.protocol/build-id)
                          (constantly "aaa111aaa111")
                          (rv 'report-version-bounce!) (constantly nil)
                          (rv 'send-json-with-entry!) (fn [& _]
                                                        idle-status)
                          (rv 'db-target) (constantly "/tmp/vis-dev-bounce-test.db")
                          (rv 'await-daemon-down!) (constantly true)
                          #'client/stop-daemon! (fn []
                                                  (swap! stops inc)
                                                  {:status "stopped" :stopping false})}
           (fn []
             (let [bounce! (rv 'bounce-stale-daemon!)]
               (is (true? (:bounced? (bounce! fake-entry))))
               (is (= 1 @stops)))))
         (finally (reset! guard false) (reset! handshake previous)))))

;; The daemon `vis-agent update` could not release is usually one whose wire protocol
;; this build no longer speaks; the mismatch screen belongs to a daemon somebody is
;; USING, never to an idle one this process is free to replace.
(deftest an-idle-daemon-too-old-to-speak-to-is-replaced-not-refused
  (let [guard
        @(rv 'stale-bounce-attempted?)

        handshake
        @(rv 'gateway-handshake*)

        cached
        @(rv 'cached-entry)

        previous-handshake
        @handshake

        previous-entry
        @cached

        stops
        (atom 0)

        attaches
        (atom 0)

        new-entry
        (assoc fake-entry :pid 4243)]

    (reset! guard false)
    (reset! cached nil)
    (reset! handshake {:protocol 1 :min-client 1 :min-gateway 1 :version "0.1.39"})
    (try
      (with-redefs-fn
        {(requiring-resolve 'com.blockether.vis.internal.gateway.protocol/release-version)
         (constantly "0.1.40")
         (rv 'report-version-bounce!) (constantly nil)
         (rv 'remote-gateway) (constantly nil)
         (rv 'db-target) (constantly "/tmp/vis-stale-bounce-test.db")
         (rv 'send-json-with-entry!) (fn [& _]
                                       idle-status)
         (rv 'await-daemon-down!) (constantly true)
         #'discovery/registry-fresh? (constantly false)
         (rv 'discover-or-recover!)
         (fn [& _]
           (let [attach (swap! attaches inc)]
             (when (> attach 1)
               ;; What this process starts in its place speaks this build's
               ;; protocol — READ, not spelled out, so raising the floor never
               ;; leaves this fixture pretending to be a daemon it just refused.
               (let [now (deref (requiring-resolve
                                  'com.blockether.vis.internal.gateway.protocol/protocol-version))]
                 (reset! handshake
                   {:protocol now :min-client now :min-gateway now :version "0.1.40"})))
             {:entry (if (> attach 1) new-entry fake-entry)}))
         #'client/stop-daemon! (fn []
                                 (swap! stops inc)
                                 (reset! cached nil)
                                 {:status "stopped" :stopping false})}
        (fn []
          (is (= new-entry (client/ensure-gateway!))
              "an idle daemon older than this build is replaced, not refused as incompatible")
          (is (= 1 @stops))
          (is (= 2 @attaches))))
      (finally (reset! guard false)
               (reset! handshake previous-handshake)
               (reset! cached previous-entry)))))
;; Regression (reported: a gateway that stopped answering had to be killed by hand):
;; `stop-daemon!` reported a live orphan and handed the human an `lsof` line, with
;; the daemon's pid sitting in the registry entry it had just read.
(deftest an-unresponsive-daemon-is-escalated-to-its-registered-pid
  (let [killed
        (atom nil)

        result
        (with-redefs-fn {(rv 'db-target) (constantly "/tmp/wedged/vis.db")
                         (rv 'remote-gateway) (constantly nil)
                         #'discovery/read-registry (constantly fake-entry)
                         #'discovery/registry-fresh? (constantly false)
                         (rv 'port-free?) (constantly false)
                         (rv 'kill-registered-daemon!) (fn [db entry]
                                                         (reset! killed [db entry])
                                                         {:signal :term :stopped? true})}
          (fn []
            (client/stop-daemon!)))]

    (is (= "stopped" (:status result)))
    (is (= :term (:escalated result)))
    (is (= ["/tmp/wedged/vis.db" fake-entry] @killed))))

(deftest a-pid-that-is-not-provably-ours-is-never-signalled
  (testing "a dead pid, or none at all, escalates to nothing"
    (with-redefs-fn {#'discovery/pid-alive? (constantly false)}
      (fn []
        (is (nil? ((rv 'registered-daemon-handle) "/tmp/x/vis.db" 4242)))))
    (is (nil? ((rv 'registered-daemon-handle) "/tmp/x/vis.db" nil)))
    (is (= {:signal nil :stopped? false}
           (with-redefs-fn {#'discovery/pid-alive? (constantly false)}
             (fn []
               ((rv 'kill-registered-daemon!) "/tmp/x/vis.db" fake-entry)))))))
(deftest provider-limits-restores-engine-shape-from-gateway-wire
  (let [request (atom nil)]
    (with-redefs-fn {(rv 'ensure-gateway-serving!) (fn [path]
                                                     (reset! request path)
                                                     fake-entry)
                     (rv 'ensure-client!) (constantly "client-id")
                     (rv 'send-json-with-entry!)
                     (fn [_ method path]
                       (is (= "GET" method))
                       (is (= @request path))
                       {"report" {"provider_id" "openai-codex"
                                  "status" "ok"
                                  "dynamic" {"limits" [{"id" "codex-5h"
                                                        "scope" "account"
                                                        "kind" "percentage"
                                                        "precision" "percent"
                                                        "source" "live"
                                                        "window" {"kind" "rolling"
                                                                  "unit" "hour"
                                                                  "size" 5
                                                                  "resets_at_ms" 1234}}]}}})}
      (fn []
        (let [report (client/provider-limits :openai-codex)]
          (is (= "/v1/providers/openai-codex/limits" @request))
          (is (= :openai-codex (:provider-id report)))
          (is (= :ok (:status report)))
          (is (= :codex-5h (get-in report [:dynamic :limits 0 :id])))
          (is (= :account (get-in report [:dynamic :limits 0 :scope])))
          (is (= :rolling (get-in report [:dynamic :limits 0 :window :kind])))
          (is (= :hour (get-in report [:dynamic :limits 0 :window :unit])))
          (is (= 1234 (get-in report [:dynamic :limits 0 :window :resets-at-ms]))))))))

(deftest provider-status-reads-is-authenticated-from-gateway-wire
  ;; The gateway emits snake_case wire keys (`is_authenticated`). The client
  ;; returns the canonical STRING-keyed status map verbatim — no keyword
  ;; restoration — so consumers read `(get status "is_authenticated")`.
  (let [request (atom nil)]
    (with-redefs-fn {(rv 'ensure-gateway-serving!) (fn [path]
                                                     (reset! request path)
                                                     fake-entry)
                     (rv 'ensure-client!) (constantly "client-id")
                     (rv 'send-json-with-entry!) (fn [_ method path]
                                                   (is (= "GET" method))
                                                   (is (= @request path))
                                                   {"status" {"is_authenticated" true
                                                              "source" "auth-file"
                                                              "oauth_token_preview" "sk-ant-o..."
                                                              "expires_in_ms" 10859960}})}
      (fn []
        (let [status (client/provider-status :anthropic-coding-plan)]
          (is (= "/v1/providers/anthropic-coding-plan/status" @request))
          (is (every? string? (keys status)))
          (is (true? (get status "is_authenticated")))
          (is (= "auth-file" (get status "source")))
          (is (= "sk-ant-o..." (get status "oauth_token_preview")))
          (is (= 10859960 (get status "expires_in_ms"))))))))

(defn- run-serving!
  "Drive ensure-gateway-serving! with a scripted `probe-route` (a seq of results,
   consumed left-to-right) and a scripted `status`. Records how many times the
   destructive stop-daemon! / await-daemon-down! fired."
  [{:keys [probes status]}]
  (let [probe-seq
        (atom probes)

        stops
        (atom 0)

        awaits
        (atom 0)]

    (with-redefs-fn {(rv 'ensure-gateway!) (fn [& _]
                                             fake-entry)
                     (rv 'probe-route) (fn [_ _]
                                         (let [[p] @probe-seq]
                                           (swap! probe-seq rest)
                                           p))
                     (rv 'status) (fn []
                                    status)
                     (rv 'stop-daemon!) (fn []
                                          (swap! stops inc)
                                          {:status "stopping"})
                     (rv 'await-daemon-down!) (fn [_ _ _]
                                                (swap! awaits inc)
                                                true)
                     (rv 'db-target) (fn []
                                       :fake-db)}
      (fn []
        (let [result (try {:entry (client/ensure-gateway-serving! "/ui")}
                          (catch clojure.lang.ExceptionInfo e {:ex (ex-data e)}))]
          (assoc result
            :stops @stops
            :awaits @awaits))))))

(deftest served-route-returns-without-restart
  (testing "a mounted route is used as-is; the daemon is never touched"
    (let [{:keys [entry stops]} (run-serving! {:probes [:served]})]
      (is (= fake-entry entry))
      (is (zero? stops) "no destructive restart when the route is served"))))

(deftest transport-blip-never-force-kills
  (testing
    ":unreachable (connection reset/timeout on the probe) is NOT a 404 —
            we retreat to leaving the daemon alone rather than force-restarting it"
    (let [{:keys [entry stops]} (run-serving! {:probes [:unreachable]})]
      (is (= fake-entry entry))
      (is (zero? stops) "a transport blip must never trigger a restart"))))

(deftest idle-daemon-with-missing-route-is-restarted
  (testing
    "a real 404 on an IDLE daemon (no other clients, no running turn) respawns:
            stop → await-down → re-ensure → re-probe :served"
    (let [{:keys [entry stops awaits ex]} (run-serving! {:probes [:absent :served]
                                                         :status {"clients" 1 "running_turns" 0}})]
      (is (nil? ex))
      (is (= fake-entry entry))
      (is (= 1 stops) "the idle stale daemon is stopped exactly once")
      (is (= 1 awaits) "and we wait for it to go down before respawning"))))

(deftest busy-daemon-is-not-force-killed
  (testing "a real 404 on a daemon OTHER clients depend on is refused, not nuked"
    (let [{:keys [ex stops awaits]} (run-serving! {:probes [:absent]
                                                   :status {"clients" 2 "running_turns" 0}})]
      (is (= :gateway/route-missing-busy (:type ex)))
      (is (= 2 (:clients ex)))
      (is (zero? stops) "a shared daemon is never stopped")
      (is (zero? awaits)))))

(deftest running-turn-blocks-restart
  (testing "a real 404 while a turn is running is refused — a restart would abort it"
    (let [{:keys [ex stops]} (run-serving! {:probes [:absent]
                                            :status {"clients" 1 "running_turns" 1}})]
      (is (= :gateway/route-missing-busy (:type ex)))
      (is (= 1 (:running-turns ex)))
      (is (zero? stops) "an in-flight turn is never force-aborted by the heal"))))

(deftest respawn-that-still-404s-throws-route-missing
  (testing "if the fresh daemon STILL lacks the route, surface a clear error"
    (let [{:keys [ex stops]} (run-serving! {:probes [:absent :absent]
                                            :status {"clients" 1 "running_turns" 0}})]
      (is (= :gateway/route-missing (:type ex)))
      (is (= 1 stops)))))

(deftest port-free?-reflects-a-live-listener
  (testing "port-free? is false while something listens, true once released"
    (let [port-free?
          (rv 'port-free?)

          sock
          (java.net.ServerSocket. 0)

          port
          (.getLocalPort sock)]

      (try (is (false? (port-free? "127.0.0.1" port)) "occupied port is not free")
           (finally (.close sock)))
      (is (true? (port-free? "127.0.0.1" port)) "released port is free"))))

(deftest sse-event-action-test
  (testing "own turn terminal returns the event"
    (is (= [:terminal {"type" "turn.completed" "turn_id" "t1"}]
           (client/sse-event-action {"type" "turn.completed" "turn_id" "t1"} "t1"))))
  (testing "own turn progress forwards"
    (is (= :forward (first (client/sse-event-action {"type" "block.output" "turn_id" "t1"} "t1")))))
  (testing "a CANCELLED own turn is terminal too — a user stop ends the stream"
    ;; Regression: `turn.cancelled` was missing from the terminal set, so an
    ;; Esc (or a stall force-cancel) left this reader parked on the turn
    ;; forever: the SSE connection never closed, the tab kept spinning, and a
    ;; queued turn draining behind it streamed in under a stream that had
    ;; never ended.
    (let [[action event'] (client/sse-event-action
                            {"type" "turn.cancelled" "turn_id" "t1" "status" "cancelled"}
                            "t1")]
      (is (= :terminal action))
      (is (= "cancelled" (get event' "status")))))
  (testing "a FAILED own turn is terminal"
    (is (= :terminal (first (client/sse-event-action {"type" "turn.failed" "turn_id" "t1"} "t1")))))
  (testing "own queued record deleted synthesizes a cancelled terminal (no hang)"
    (let [[action event'] (client/sse-event-action {"type" "turn.queued.deleted" "turn_id" "t1"}
                                                   "t1")]
      (is (= :terminal action))
      (is (= "cancelled" (get event' "status")))
      (is (= "turn.completed" (get event' "type")))))
  (testing "a SIBLING turn's queue lifecycle events forward (cross-TUI queue mirror)"
    (doseq [type ["turn.queued" "turn.queued.updated" "turn.queued.deleted" "turn.queued.drained"]]
      (is (= :forward (first (client/sse-event-action {"type" type "turn_id" "OTHER"} "t1")))
          type)))
  (testing "a sibling turn's non-queue events are dropped"
    (is (= :skip (first (client/sse-event-action {"type" "block.output" "turn_id" "OTHER"} "t1"))))
    (is (= :skip
           (first (client/sse-event-action {"type" "turn.completed" "turn_id" "OTHER"} "t1"))))))

(deftest terminal-event->result-keeps-canonical-nested-maps
  (testing
    "the blocking result IS the canonical snake_case string-keyed wire event
           (plus derived fills) — tokens/cost/utilization are never re-keyed"
    (let [t->r
          (rv 'terminal-event->result)

          ;; What `parse-json` yields after the SSE hop: snake_case STRING keys.
          event
          {"type" "turn.completed"
           "turn_id" "t1"
           "session_id" "s1"
           "cost" {"total_cost" 0.0123 "model" "m" "provider" "p"}
           "tokens" {"input" 10 "cached" 4 "output" 2}
           "utilization" {"saturation" 42 "headroom_tokens" 1000}}

          result
          (with-redefs [client/get-turn (fn [_ _]
                                          {"content" [{"id" "b1" "type" "prose" "markdown" "done"}]
                                           "iteration_count" 1})]
            (t->r event "t1"))]

      (is (= 0.0123 (get-in result ["cost" "total_cost"])) "cost stays canonical")
      (is (= "m" (get-in result ["cost" "model"])))
      (is (= 4 (get-in result ["tokens" "cached"])) "token slots stay canonical")
      (is (= 42 (get-in result ["utilization" "saturation"])) "utilization stays canonical")
      (is (= "t1" (get result "session_turn_id")))
      (is (= "done" (get-in result ["content" 0 "markdown"])))
      (is (not-any? keyword? (keys result)) "no keyword keys survive in the blocking result"))))

(deftest read-events-until!-surfaces-disconnect
  (testing
    "a stream that never reaches a terminal event throws a clear
           gateway-disconnected error (not a silent blank result) after the
           reconnect budget is spent"
    (let [reads (atom 0)]
      (with-redefs-fn {(rv 'read-sse-stream!) (fn [_ _ _ _ _]
                                                (swap! reads inc)
                                                [:closed])
                       (rv 'sse-reconnect-backoff-ms) 0
                       (rv 'sse-reconnect-max-attempts) 2}
        (fn []
          (let [ex (try ((rv 'read-events-until!) "s" 0 "t1" nil)
                        nil
                        (catch clojure.lang.ExceptionInfo e (ex-data e)))]
            (is (true? (:gateway-disconnected ex)))
            (is (= 3 @reads) "initial attempt + 2 reconnects")))))))

(deftest read-events-until!-reconnects-then-completes
  (testing "a dropped stream reconnects and still returns the terminal event"
    (let [scripted (atom [[:closed] [:terminal {:type "turn.completed" :turn_id "t1"}]])]
      (with-redefs-fn {(rv 'read-sse-stream!) (fn [_ _ _ _ _]
                                                (let [[r] @scripted]
                                                  (swap! scripted rest)
                                                  r))
                       (rv 'sse-reconnect-backoff-ms) 0}
        (fn []
          (is (= {:type "turn.completed" :turn_id "t1"}
                 ((rv 'read-events-until!) "s" 0 "t1" nil))))))))

(deftest read-events-until!-reconnects-on-http-status
  (testing
    "a non-200 mid-turn (502/503 while the daemon restarts) is treated as a
           drop and reconnected, same as an EOF — not rethrown as a bare error"
    (let [reads (atom 0)]
      (with-redefs-fn {(rv 'read-sse-stream!)
                       (fn [_ _ _ _ _]
                         (if (< @reads 2)
                           (do (swap! reads inc)
                               (throw (ex-info "gateway SSE HTTP 503" {:http-status 503})))
                           (do (swap! reads inc)
                               [:terminal {:type "turn.completed" :turn_id "t1"}])))
                       (rv 'sse-reconnect-backoff-ms) 0}
        (fn []
          (is (= {:type "turn.completed" :turn_id "t1"} ((rv 'read-events-until!) "s" 0 "t1" nil)))
          (is (= 3 @reads) "two 503 reconnects + the completing read"))))))

(deftest read-events-until!-rethrows-non-http-ex-info
  (testing "an ex-info WITHOUT :http-status is not swallowed as a drop"
    (with-redefs-fn {(rv 'read-sse-stream!) (fn [_ _ _ _ _]
                                              (throw (ex-info "boom" {:kaboom true})))
                     (rv 'sse-reconnect-backoff-ms) 0}
      (fn []
        (let [ex (try ((rv 'read-events-until!) "s" 0 "t1" nil)
                      nil
                      (catch clojure.lang.ExceptionInfo e (ex-data e)))]
          (is (true? (:kaboom ex))))))))

(deftest mux-advance-cursor!-honours-the-subscription-ready-echo
  (let [advance! (rv 'mux-advance-cursor!)]
    (testing "an ordinary frame advances the cursor monotonically"
      (let [cursor (atom 10)]
        (advance! cursor {"type" "turn.delta" "seq" 12})
        (is (= 12 @cursor))
        (advance! cursor {"type" "turn.delta" "seq" 11})
        (is (= 12 @cursor) "a late lower seq never rewinds a live cursor")))
    (testing "subscription.ready OVERRIDES the max, so a renumbered daemon heals"
      ;; A restarted gateway numbers from its journal high-water, far below the
      ;; cursor this client carried across the outage. Keeping the max would ask
      ;; for a cursor above the session's high-water on EVERY reconnect, the
      ;; server would clamp it, and this session would never replay again.
      (let [cursor (atom 4200)]
        (advance! cursor {"type" "subscription.ready" "cursor" 7})
        (is (= 7 @cursor) "the echoed resume point wins outright")
        (advance! cursor {"type" "turn.completed" "seq" 8})
        (is (= 8 @cursor) "and the renumbered stream is delivered and tracked from there")))
    (testing "a ready frame with no usable cursor leaves the cursor alone"
      (let [cursor (atom 5)]
        (advance! cursor {"type" "subscription.ready"})
        (advance! cursor {"type" "subscription.ready" "cursor" nil})
        (is (= 5 @cursor))))))

(deftest mux-subscribe!-shares-one-remote-session-subscription
  (testing "multiple local listeners for one sid do not reconnect/open one SSE per tab"
    (let [mux-var
          (rv 'mux)

          restarts
          (atom 0)

          seen-a
          (atom [])

          seen-b
          (atom [])]

      (reset! @mux-var {:subs {} :epoch 0 :future nil :stream nil})
      (with-redefs-fn {(rv 'restart-mux!) (fn []
                                            (swap! restarts inc)
                                            nil)}
        (fn []
          (let [cleanup-a
                (client/mux-subscribe! "sid-1" #(swap! seen-a conj %) 10)

                cleanup-b
                (client/mux-subscribe! "sid-1" #(swap! seen-b conj %) 10)

                entry
                (get-in @@mux-var [:subs "sid-1"])]

            (is (= 1 @restarts) "second listener for same sid should not reopen /v1/events")
            (is (= 2 (count (:sinks entry))))
            (doseq [[_ sink] (:sinks entry)]
              (sink {:type "turn.started" :session_id "sid-1" :seq 11}))
            (is (= [{:type "gateway.connected"} {:type "turn.started" :session_id "sid-1" :seq 11}]
                   @seen-b)
                "new same-sid listener gets connection state and live events")
            (is (= [{:type "turn.started" :session_id "sid-1" :seq 11}] @seen-a))
            (cleanup-a)
            (is (= 1 @restarts) "dropping one of two listeners leaves the remote mux alone")
            (is (= 1 (count (get-in @@mux-var [:subs "sid-1" :sinks]))))
            (cleanup-b)
            (is (= 2 @restarts) "only the last listener removal changes the remote session set")
            (is (empty? (:subs @@mux-var)))))))))

(deftest fleet-subscribe!-rides-one-stream-instead-of-asking-per-session
  (testing "a session LIST watches the fleet feed and opens no per-session route"
    (let
      [calls
       (atom [])

       seen
       (atom [])

       frames
       (str
         "data: {\"type\":\"session.status\",\"session_id\":\"a\",\"is_live\":true}\n\n"
         "data: {\"type\":\"session.title_updated\",\"session_id\":\"a\",\"title\":\"named\"}\n\n")]

      (with-redefs-fn {(rv 'ensure-gateway!) (fn []
                                               fake-entry)
                       (rv 'ensure-client!) (fn [_]
                                              nil)
                       (rv 'ensure-release-hook!) (fn []
                                                    nil)
                       (rv 'gw-send!)
                       (fn [_ method path _]
                         (swap! calls conj [method path])
                         {:status 200
                          :body (java.io.ByteArrayInputStream.
                                  (.getBytes frames java.nio.charset.StandardCharsets/UTF_8))})}
        (fn []
          (let [stop! (client/fleet-subscribe! (fn [frame]
                                                 (swap! seen conj frame)))]
            (try (loop [waited 0]
                   (when (and (< (count @seen) 2) (< waited 2000))
                     (Thread/sleep 10)
                     (recur (+ waited 10))))
                 (finally (stop!)))
            (is (= ["GET" "/v1/events?scope=fleet"] (first @calls)))
            (is (= ["session.status" "session.title_updated"]
                   (mapv #(get % "type") (take 2 @seen))))
            (let [after-stop (count @calls)]
              (Thread/sleep 400)
              (is (= after-stop (count @calls))
                  "stopping ends the watch instead of reconnecting"))))))))
(deftest mux-finalization-barrier-forbids-new-subscriptions
  (let [mux-var
        (rv 'mux)

        finalizing-var
        (rv 'client-finalizing?)

        previous-mux
        @@mux-var

        previous-finalizing
        @@finalizing-var]

    (try (reset! @mux-var {:subs {} :epoch 0 :future nil :stream nil})
         (reset! @finalizing-var true)
         (with-redefs-fn {(rv 'ensure-release-hook!)
                          (fn []
                            (throw (ex-info "must not install during finalization" {})))
                          (rv 'restart-mux!)
                          (fn []
                            (throw (ex-info "must not restart during finalization" {})))}
           (fn []
             (let [cleanup (client/mux-subscribe! "sid-final"
                                                  (fn [_])
                                                  0)]
               (is (fn? cleanup))
               (is (empty? (:subs @@mux-var)))
               (cleanup))))
         (finally (reset! @mux-var previous-mux) (reset! @finalizing-var previous-finalizing)))))

(deftest restart-mux-never-starts-a-reader-during-finalization
  (let [mux-var
        (rv 'mux)

        finalizing-var
        (rv 'client-finalizing?)

        previous-mux
        @@mux-var

        previous-finalizing
        @@finalizing-var

        starts
        (atom 0)]

    (try (reset! @mux-var {:subs {"sid-final" {:cursor-atom (atom 0)
                                               :sinks {"sub" (fn [_])}}}
                           :epoch 0
                           :future nil
                           :stream nil})
         (reset! @finalizing-var true)
         (with-redefs-fn {(rv 'mux-run!) (fn [_]
                                           (swap! starts inc)
                                           (future nil))}
           (fn []
             ((rv 'restart-mux!))
             (is (zero? @starts))
             (is (nil? (:future @@mux-var)))))
         (finally (reset! @mux-var previous-mux) (reset! @finalizing-var previous-finalizing)))))

(deftest shutdown-subscriptions-closes-all-streams-without-reconnect
  (let [mux-var
        (rv 'mux)

        subscriptions-var
        (rv 'subscriptions)

        finalizing-var
        (rv 'client-finalizing?)

        previous-mux
        @@mux-var

        previous-subscriptions
        @@subscriptions-var

        previous-finalizing
        @@finalizing-var

        closes
        (atom 0)

        closeable
        (reify
          java.io.Closeable
            (close [_] (swap! closes inc)))]

    (try (reset! @finalizing-var false)
         (reset! @subscriptions-var {"legacy" {:future nil :stream (atom closeable)}})
         (reset! @mux-var {:subs {"sid" {:cursor-atom (atom 0)
                                         :sinks {"sub" (fn [_])}}}
                           :epoch 0
                           :future nil
                           :stream closeable})
         ((rv 'shutdown-subscriptions!))
         (is (true? @@finalizing-var))
         (is (empty? @@subscriptions-var))
         (is (empty? (:subs @@mux-var)))
         (is (= 2 @closes))
         (finally (reset! @mux-var previous-mux)
                  (reset! @subscriptions-var previous-subscriptions)
                  (reset! @finalizing-var previous-finalizing)))))

(deftest list-resources-cached-never-blocks-the-caller
  ;; REGRESSION: the footer calls this on the render thread every frame. The
  ;; daemon round-trip MUST run in the background so a busy/slow daemon can't
  ;; stall painting. A cold read returns the last-known value (nil) instantly
  ;; and kicks a single-flight refresh; once it lands, subsequent reads are
  ;; served from cache. If someone reintroduces a synchronous round-trip this
  ;; test blocks for `slow-ms` and the timing assertion fails.
  (let [slow-ms
        300

        cache
        (rv 'resources-cache)

        inflight
        (rv 'resources-refreshing)

        calls
        (atom 0)]

    (with-redefs-fn {(rv 'list-resources) (fn [_sid]
                                            (swap! calls inc)
                                            (Thread/sleep slow-ms)
                                            [{"id" "bg"}])}
      (fn []
        (reset! @cache {})
        (reset! @inflight #{})
        (let [t0
              (System/nanoTime)

              cold
              (client/list-resources-cached "sid-x")

              cold-ms
              (/ (- (System/nanoTime) t0) 1e6)]

          (is (nil? cold) "cold read serves the last-known value (nil) immediately")
          (is (< cold-ms 50.0) "cold read must NOT block on the daemon round-trip")
          ;; several stale reads while the fetch is in flight stay single-flight
          (dotimes [_ 5]
            (client/list-resources-cached "sid-x"))
          (await-value #(client/list-resources-cached "sid-x") [{"id" "bg"}])
          (is (= 1 @calls) "only ONE background fetch runs per sid (single-flight)")
          (let [t1
                (System/nanoTime)

                warm
                (client/list-resources-cached "sid-x")

                warm-ms
                (/ (- (System/nanoTime) t1) 1e6)]

            (is (= [{"id" "bg"}] warm) "a fresh entry is served from cache")
            (is (< warm-ms 50.0) "warm read is a pure cache hit")
            (is (empty? @@inflight) "the in-flight slot is released after the fetch")))))))

(deftest session-model-cached-never-blocks-the-caller
  ;; REGRESSION (issue #29, gateway leg): the footer reads the session's model
  ;; pref every frame. This used to be a LIVE daemon round-trip per frame; it
  ;; must serve from a per-sid cache and refresh in the background — same
  ;; discipline as `list-resources-cached` above.
  (let [slow-ms
        300

        cache
        (rv 'session-model-cache)

        inflight
        (rv 'session-model-refreshing)

        calls
        (atom 0)]

    (with-redefs-fn {(rv 'session-model) (fn [_sid]
                                           (swap! calls inc)
                                           (Thread/sleep slow-ms)
                                           {:provider "anthropic" :model "opus"})}
      (fn []
        (reset! @cache {})
        (reset! @inflight #{})
        (let [t0
              (System/nanoTime)

              cold
              (client/session-model-cached "sid-m")

              cold-ms
              (/ (- (System/nanoTime) t0) 1e6)]

          (is (nil? cold) "cold read serves the last-known value (nil) immediately")
          (is (< cold-ms 50.0) "cold read must NOT block on the daemon round-trip")
          ;; several stale reads while the fetch is in flight stay single-flight
          (dotimes [_ 5]
            (client/session-model-cached "sid-m"))
          (await-value #(client/session-model-cached "sid-m") {:provider "anthropic" :model "opus"})
          (is (= 1 @calls) "only ONE background fetch runs per sid (single-flight)")
          (let [t1
                (System/nanoTime)

                warm
                (client/session-model-cached "sid-m")

                warm-ms
                (/ (- (System/nanoTime) t1) 1e6)]

            (is (= {:provider "anthropic" :model "opus"} warm) "a fresh entry is served from cache")
            (is (< warm-ms 50.0) "warm read is a pure cache hit")
            (is (empty? @@inflight) "the in-flight slot is released after the fetch")))))))

(deftest set-session-model!-writes-through-the-session-model-cache
  ;; A pick made in THIS client must show on the very next footer frame, not
  ;; after the cache TTL expires.
  (let [cache (rv 'session-model-cache)]
    (with-redefs-fn {(rv 'send-json!) (fn [method path body]
                                        (is (= "PATCH" method))
                                        (is (= "/v1/sessions/sid-w/model" path))
                                        {"model" {"provider" (:provider body)
                                                  "model" (:model body)}})}
      (fn []
        (reset! @cache {})
        (is (= {:provider "zai" :model "glm"} (client/set-session-model! "sid-w" "zai" "glm")))
        (is (= {:provider "zai" :model "glm"} (:val (get @@cache "sid-w")))
            "the PATCHed pref lands in the footer cache immediately")))))

(deftest setting-actions-proxy-to-the-daemon
  (let [calls (atom [])]
    (with-redefs-fn {(rv 'send-json!) (fn [method path body]
                                        (swap! calls conj [method path body])
                                        (if (= "cycle" (:action body))
                                          {"id" (:id body) "type" "enum" "value" "deep"}
                                          {"id" (:id body) "type" "boolean" "enabled" true}))}
      (fn []
        (is (= {"id" "shell" "type" "boolean" "enabled" true} (client/toggle-setting! "shell")))
        (is (= {"id" "reasoning_level" "type" "enum" "value" "deep"}
               (client/cycle-setting! "reasoning_level")))
        (is (= [["POST" "/v1/settings" {:id "shell" :action "toggle"}]
                ["POST" "/v1/settings" {:id "reasoning_level" :action "cycle"}]]
               @calls))))))

(deftest provider-models-proxies-to-daemon-catalog-route
  (testing
    "provider-models asks the DAEMON for the catalog instead of building a token-resolving router client-side"
    (let [request (atom nil)]
      (with-redefs-fn {(rv 'ensure-gateway-serving!) (fn [path]
                                                       (reset! request path)
                                                       fake-entry)
                       (rv 'ensure-client!) (constantly "client-id")
                       (rv 'send-json-with-entry!) (fn [_ method path]
                                                     (is (= "GET" method))
                                                     (is (= @request path))
                                                     {"models" ["claude-opus-4-8" "claude-sonnet-5"]
                                                      "hidden_count" 3})}
        (fn []
          (let [r (client/provider-models :anthropic-coding-plan false)]
            (is (= "/v1/providers/anthropic-coding-plan/models" @request))
            (is (= ["claude-opus-4-8" "claude-sonnet-5"] (:models r)))
            (is (= 3 (:hidden-count r))))
          (client/provider-models :anthropic-coding-plan true)
          (is (= "/v1/providers/anthropic-coding-plan/models?show_all=true" @request)))))))

(deftest set-router-default-proxies-and-decodes-the-explicit-pair
  (let [request (atom nil)]
    (with-redefs-fn {(rv 'ensure-gateway-serving!) (constantly fake-entry)
                     (rv 'ensure-client!) (constantly "client-id")
                     (rv 'send-json-with-entry!) (fn [_ method path body]
                                                   (reset! request [method path body])
                                                   {"default_provider" "anthropic-coding-plan"
                                                    "default_model" "claude-fable-5"})}
      (fn []
        (is (= {:provider-id :anthropic-coding-plan :model "claude-fable-5"}
               (client/set-router-default! :anthropic-coding-plan "claude-fable-5")))
        (is (= ["PATCH" "/v1/router"
                {"role" "primary" "provider" "anthropic-coding-plan" "model" "claude-fable-5"}]
               @request)
            "the primary tag is explicit on the wire, so the daemon never guesses the role")))))

(deftest set-router-fallback-tags-and-clears-the-second-root
  (testing "a fallback tag rides the SAME route under role=fallback and decodes the fallback_* pair"
    (let [request (atom nil)]
      (with-redefs-fn {(rv 'ensure-gateway-serving!) (constantly fake-entry)
                       (rv 'ensure-client!) (constantly "client-id")
                       (rv 'send-json-with-entry!) (fn [_ method path body]
                                                     (reset! request [method path body])
                                                     {"default_provider" "anthropic-coding-plan"
                                                      "default_model" "claude-fable-5"
                                                      "fallback_provider" "zai-coding-plan"
                                                      "fallback_model" "glm-5.2"})}
        (fn []
          (is (= {:provider-id :zai-coding-plan :model "glm-5.2"}
                 (client/set-router-fallback! :zai-coding-plan "glm-5.2"))
              "the FALLBACK pair comes back, never the primary one")
          (is (= ["PATCH" "/v1/router"
                  {"role" "fallback" "provider" "zai-coding-plan" "model" "glm-5.2"}]
                 @request))))))
  (testing "the zero-arity clear sends role=fallback with NO pair and decodes nil"
    (let [request (atom nil)]
      (with-redefs-fn {(rv 'ensure-gateway-serving!) (constantly fake-entry)
                       (rv 'ensure-client!) (constantly "client-id")
                       (rv 'send-json-with-entry!) (fn [_ method path body]
                                                     (reset! request [method path body])
                                                     {"default_provider" "anthropic-coding-plan"
                                                      "default_model" "claude-fable-5"})}
        (fn []
          (is (nil? (client/set-router-fallback!)))
          (is (= ["PATCH" "/v1/router" {"role" "fallback"}] @request)))))))

(defn- refusal-ex
  "The ExceptionInfo a refusing daemon produces, with `body` as its raw answer."
  [status body]
  (with-redefs-fn {(rv 'gw-send!) (fn [_ _ _ _]
                                    {:status status :body body})}
    (fn []
      (try ((rv 'send-json-with-entry!) fake-entry "PATCH" "/v1/router" {})
           nil
           (catch clojure.lang.ExceptionInfo e e)))))

(deftest rejected-requests-surface-the-daemons-own-reason
  (testing
    "a 400 whose reason is nested under error.message reaches the caller verbatim, so the TUI dialog explains the refusal instead of printing a bare status"
    (let [e (refusal-ex 400
                        (str "{\"error\":{\"message\":\"Fallback provider must differ "
                             "from the primary provider (anthropic-coding-plan)\"}}"))]
      (is (some? e))
      (is (= "Fallback provider must differ from the primary provider (anthropic-coding-plan)"
             (ex-message e)))
      (is (= 400 (:http-status (ex-data e))))))
  (testing "a flat `message` body still wins"
    (is (= "flat reason" (ex-message (refusal-ex 400 "{\"message\":\"flat reason\"}")))))
  (testing "a reasonless refusal keeps the bare status text"
    (is (= "gateway HTTP 503" (ex-message (refusal-ex 503 ""))))))

;; The provider dialog fanned out 2×N per-provider probes because no client
;; function handed back the whole fleet's status AND limits from the one
;; /v1/router read that already carries both.
(deftest router-diagnostics-loads-the-whole-fleet-in-one-call
  (let [calls
        (atom 0)

        fleet
        [{"id" "openai"
          "status" {"is_authenticated" true "source" "gateway"}
          "limits" {"provider_id" "openai"
                    "status" "ready"
                    "static" {"rpm" 10}
                    "dynamic" {"limits"
                               [{"id" "requests" "scope" "account" "is_unlimited" false}]}}}
         {"id" "anthropic" "status" {"is_authenticated" false} "limits" nil}]

        result
        (with-redefs-fn {#'client/router (fn []
                                           (swap! calls inc)
                                           fleet)}
          #(client/router-diagnostics))]

    (testing "one gateway read serves every provider"
      (is (= 1 @calls))
      (is (= #{:openai :anthropic} (set (keys result)))))
    (testing "status stays verbatim wire strings and limits are engine-shaped"
      (is (= true (get-in result [:openai :status "is_authenticated"])))
      (is (= false (get-in result [:anthropic :status "is_authenticated"])))
      (is (= :ready (get-in result [:openai :limits :status])))
      (is (= {:rpm 10} (get-in result [:openai :limits :static])))
      (is (= :requests (get-in result [:openai :limits :dynamic :limits 0 :id])))
      (is (nil? (get-in result [:anthropic :limits]))))))

;;; ── Remote gateway target (`--gateway` / VIS_GATEWAY_URL) ─────────────────────

(deftest remote-entry-reads-a-host-a-host-port-and-a-url
  (let [remote-entry (rv 'remote-entry)]
    (testing "a bare host is plain HTTP on the standard gateway port"
      (is
        (=
          {:base-url "http://10.0.0.5:7890" :host "10.0.0.5" :port 7890 :secret "tok" :remote? true}
          (remote-entry "10.0.0.5" "tok"))))
    (testing "an explicit port wins, and https defaults to 443"
      (is (= "http://10.0.0.5:7899" (:base-url (remote-entry "10.0.0.5:7899" nil))))
      (is (= "https://gateway.example.com:443"
             (:base-url (remote-entry "https://gateway.example.com/" nil))))
      (is (= "https://gateway.example.com:8443/vis"
             (:base-url (remote-entry "https://gateway.example.com:8443/vis/" nil)))))
    (testing "a blank token is no token: a loopback daemon reached by tunnel needs none"
      (is (nil? (:secret (remote-entry "127.0.0.1:7899" "   ")))))
    (testing "no url is no remote target" (is (nil? (remote-entry "  " "tok"))))
    (testing "a value that names no host is a user error, never a silent local fallback"
      (is (= :gateway/invalid-remote-url
             (:type (ex-data (try (remote-entry ":7890" nil)
                                  (catch clojure.lang.ExceptionInfo e e)))))))))

(deftest remote-target-attaches-without-registry-or-spawn
  (let [target
        ((rv 'remote-entry) "10.0.0.5:7891" "tok")

        fresh-until
        @(rv 'entry-fresh-until-ns)

        cached
        @(rv 'cached-entry)

        previous-fresh
        @fresh-until

        previous-cached
        @cached]

    (try (reset! fresh-until 0)
         (with-redefs-fn {(rv 'remote-gateway) (constantly target)
                          (rv 'probe-entry?) (constantly true)
                          (rv 'assert-compatible!) identity
                          #'discovery/discover-or-start!
                          (fn [& _]
                            (throw (AssertionError. "a remote gateway must never be spawned")))
                          #'discovery/read-registry
                          (fn [& _]
                            (throw (AssertionError. "a remote gateway has no local registry")))}
           (fn []
             (is (= target (client/ensure-gateway!)))))
         (finally (reset! fresh-until previous-fresh) (reset! cached previous-cached)))))

(deftest remote-request-carries-the-bearer-token-and-claims-no-pid
  (let [captured
        (atom nil)

        target
        ((rv 'remote-entry) "10.0.0.5:7891" "tok")

        capture
        (fn [request]
          (reset! captured request)
          {:status 200 :body "{}"})]

    (with-redefs-fn {#'http/request capture}
      (fn []
        ((rv 'gw-send!) target "GET" "/healthz" {})
        (testing "a gateway on another machine is reached at its own base url"
          (is (= "http://10.0.0.5:7891/healthz" (:uri @captured))))
        (testing "one secret, both carriers"
          (is (= "Bearer tok" (get-in @captured [:headers "Authorization"])))
          (is (= "tok" (get-in @captured [:headers "X-Vis-Gateway-Secret"]))))
        (testing "no pid: this process owns none on the gateway's machine"
          (is (nil? (get-in @captured [:headers "X-Vis-Client-Pid"]))))
        ((rv 'gw-send!) fake-entry "GET" "/healthz" {})
        (testing "the locally managed daemon still gets the pid its lease reaper needs"
          (is (= (str (discovery/current-pid))
                 (get-in @captured [:headers "X-Vis-Client-Pid"]))))))))

(deftest tokenless-remote-probe-accepts-an-auth-free-gateway
  (let [handshake
        @(rv 'gateway-handshake*)

        previous
        @handshake

        body
        "{\"status\":\"ok\",\"secret_match\":false}"]

    (try (with-redefs-fn {(rv 'gw-send!) (fn [& _]
                                           {:status 200 :body body})}
           (fn []
             (testing "a token-less target cannot match a secret and does not need to"
               (is (true? ((rv 'probe-entry?) ((rv 'remote-entry) "127.0.0.1:7899" nil)))))
             (testing "the local daemon must still prove it owns our registry secret"
               (is (false? ((rv 'probe-entry?) fake-entry))))))
         (finally (reset! handshake previous)))))

(deftest remote-client-lease-carries-no-pid
  (let [client-id-atom
        @(rv 'client-id)

        previous
        @client-id-atom

        captured
        (atom nil)

        ensure-client
        (rv 'ensure-client!)

        register
        (fn [_entry _method _path body]
          (reset! captured body)
          {"client_id" "cid"})]

    (try (with-redefs-fn {(rv 'send-json-with-entry!) register}
           (fn []
             (reset! client-id-atom nil)
             (ensure-client (assoc fake-entry :remote? true))
             (is (= {:kind "clojure-client"} @captured))
             (reset! client-id-atom nil)
             (ensure-client fake-entry)
             (is (= {:kind "clojure-client" :pid (discovery/current-pid)} @captured))))
         (finally (reset! client-id-atom previous)))))

(deftest a-remote-gateway-is-never-stopped-from-here
  (with-redefs-fn {(rv 'remote-gateway) (constantly ((rv 'remote-entry) "10.0.0.5" "tok"))}
    (fn []
      (is (= :gateway/remote-target
             (:type (ex-data (try (client/stop-daemon!)
                                  (catch clojure.lang.ExceptionInfo e e)))))))))
