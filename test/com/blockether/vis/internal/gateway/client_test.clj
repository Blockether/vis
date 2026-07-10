(ns com.blockether.vis.internal.gateway.client-test
  "Unit coverage for the self-heal decision logic in [[client/ensure-gateway-serving!]].

   The point under test is that the /ui 404 self-heal is NON-DESTRUCTIVE: it only
   force-restarts a stale daemon that is genuinely idle, treats a transport blip as
   \"leave it alone\", and never confuses either with a real 404."
  (:require [clojure.test :refer [deftest is testing]]
            [com.blockether.vis.internal.gateway.client :as client]))

(defn- rv
  "Resolve a (possibly private) var in the client namespace for with-redefs-fn."
  [sym]
  (ns-resolve 'com.blockether.vis.internal.gateway.client sym))

(def ^:private fake-entry {:host "127.0.0.1" :port 7890 :pid 4242 :secret "s"})

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

    (with-redefs-fn {(rv 'ensure-gateway!) (fn []
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
                                                         :status {:clients 1 :running_turns 0}})]
      (is (nil? ex))
      (is (= fake-entry entry))
      (is (= 1 stops) "the idle stale daemon is stopped exactly once")
      (is (= 1 awaits) "and we wait for it to go down before respawning"))))

(deftest busy-daemon-is-not-force-killed
  (testing "a real 404 on a daemon OTHER clients depend on is refused, not nuked"
    (let [{:keys [ex stops awaits]} (run-serving! {:probes [:absent]
                                                   :status {:clients 2 :running_turns 0}})]
      (is (= :gateway/route-missing-busy (:type ex)))
      (is (= 2 (:clients ex)))
      (is (zero? stops) "a shared daemon is never stopped")
      (is (zero? awaits)))))

(deftest running-turn-blocks-restart
  (testing "a real 404 while a turn is running is refused — a restart would abort it"
    (let [{:keys [ex stops]} (run-serving! {:probes [:absent]
                                            :status {:clients 1 :running_turns 1}})]
      (is (= :gateway/route-missing-busy (:type ex)))
      (is (= 1 (:running-turns ex)))
      (is (zero? stops) "an in-flight turn is never force-aborted by the heal"))))

(deftest respawn-that-still-404s-throws-route-missing
  (testing "if the fresh daemon STILL lacks the route, surface a clear error"
    (let [{:keys [ex stops]} (run-serving! {:probes [:absent :absent]
                                            :status {:clients 1 :running_turns 0}})]
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
