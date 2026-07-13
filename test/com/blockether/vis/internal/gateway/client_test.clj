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

(deftest sse-event-action-test
  (testing "own turn terminal returns the event"
    (is (= [:terminal {:type "turn.completed" :turn_id "t1"}]
           (client/sse-event-action {:type "turn.completed" :turn_id "t1"} "t1"))))
  (testing "own turn progress forwards"
    (is (= :forward (first (client/sse-event-action {:type "block.output" :turn_id "t1"} "t1")))))
  (testing "own queued record deleted synthesizes a cancelled terminal (no hang)"
    (let [[action event'] (client/sse-event-action {:type "turn.queued.deleted" :turn_id "t1"}
                                                   "t1")]
      (is (= :terminal action))
      (is (= "cancelled" (:status event')))
      (is (= "turn.completed" (:type event')))))
  (testing "a SIBLING turn's queue lifecycle events forward (cross-TUI queue mirror)"
    (doseq [type ["turn.queued" "turn.queued.updated" "turn.queued.deleted" "turn.queued.drained"]]
      (is (= :forward (first (client/sse-event-action {:type type :turn_id "OTHER"} "t1"))) type)))
  (testing "a sibling turn's non-queue events are dropped"
    (is (= :skip (first (client/sse-event-action {:type "block.output" :turn_id "OTHER"} "t1"))))
    (is (= :skip
           (first (client/sse-event-action {:type "turn.completed" :turn_id "OTHER"} "t1"))))))

(deftest terminal-event->result-unwires-nested-maps
  (testing
    "the wire munges :total-cost -> :total_cost; the client must restore
           the KEBAB shape so meta-cost / the footer's session-cost-keys read it"
    (let [t->r
          (rv 'terminal-event->result)

          ;; What `parse-json` yields after the SSE hop: nested map keys snake_cased.
          event
          {:type "turn.completed"
           :turn_id "t1"
           :cost {:total_cost 0.0123 :model "m" :provider "p"}
           :tokens {:input 10 :cached_input 4 :output 2}}

          result
          (t->r event "t1")]

      (is (= 0.0123 (get-in result [:cost :total-cost])) "cost total is kebab again")
      (is (= "m" (get-in result [:cost :model])))
      (is (= 4 (get-in result [:tokens :cached-input])) "token slots kebab too"))))

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
