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

(deftest ensure-project-for-root-uses-project-action-route
  (let [request (atom nil)]
    (with-redefs-fn {(rv 'send-json!) (fn [method path body]
                                        (reset! request [method path body])
                                        {:id "project-id"})}
      (fn []
        (is (= {:id "project-id"} (client/ensure-project-for-root! "/workspace" "Vis")))
        (is (= ["POST" "/v1/projects/actions/ensure" {:root "/workspace" :name "Vis"}]
               @request))))))

(deftest ensure-client-registers-once-from-canonical-string-keyed-response
  (let
    [client-id-atom
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
  ;; reads that boolean straight into `:is-authenticated` — otherwise an
  ;; authenticated provider paints RED forever.
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
          (is (every? keyword? (keys status)))
          (is (true? (:is-authenticated status)))
          (is (= "auth-file" (:source status)))
          (is (= "sk-ant-o..." (:oauth-token-preview status)))
          (is (= 10859960 (:expires-in-ms status))))))))

(defn- run-serving!
  "Drive ensure-gateway-serving! with a scripted `probe-route` (a seq of results,
   consumed left-to-right) and a scripted `status`. Records how many times the
   destructive stop-daemon! / await-daemon-down! fired."
  [{:keys [probes status]}]
  (let
    [probe-seq
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
        (let
          [result (try {:entry (client/ensure-gateway-serving! "/ui")}
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
    (let
      [{:keys [entry stops awaits ex]} (run-serving! {:probes [:absent :served]
                                                      :status {"clients" 1 "running_turns" 0}})]
      (is (nil? ex))
      (is (= fake-entry entry))
      (is (= 1 stops) "the idle stale daemon is stopped exactly once")
      (is (= 1 awaits) "and we wait for it to go down before respawning"))))

(deftest busy-daemon-is-not-force-killed
  (testing "a real 404 on a daemon OTHER clients depend on is refused, not nuked"
    (let
      [{:keys [ex stops awaits]} (run-serving! {:probes [:absent]
                                                :status {"clients" 2 "running_turns" 0}})]
      (is (= :gateway/route-missing-busy (:type ex)))
      (is (= 2 (:clients ex)))
      (is (zero? stops) "a shared daemon is never stopped")
      (is (zero? awaits)))))

(deftest running-turn-blocks-restart
  (testing "a real 404 while a turn is running is refused — a restart would abort it"
    (let
      [{:keys [ex stops]} (run-serving! {:probes [:absent]
                                         :status {"clients" 1 "running_turns" 1}})]
      (is (= :gateway/route-missing-busy (:type ex)))
      (is (= 1 (:running-turns ex)))
      (is (zero? stops) "an in-flight turn is never force-aborted by the heal"))))

(deftest respawn-that-still-404s-throws-route-missing
  (testing "if the fresh daemon STILL lacks the route, surface a clear error"
    (let
      [{:keys [ex stops]} (run-serving! {:probes [:absent :absent]
                                         :status {"clients" 1 "running_turns" 0}})]
      (is (= :gateway/route-missing (:type ex)))
      (is (= 1 stops)))))

(deftest port-free?-reflects-a-live-listener
  (testing "port-free? is false while something listens, true once released"
    (let
      [port-free?
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
  (testing "own queued record deleted synthesizes a cancelled terminal (no hang)"
    (let
      [[action event'] (client/sse-event-action {"type" "turn.queued.deleted" "turn_id" "t1"} "t1")]
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
    (let
      [t->r
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
       (with-redefs
         [client/get-turn (fn [_ _]
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
          (let
            [ex (try ((rv 'read-events-until!) "s" 0 "t1" nil)
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
        (let
          [ex (try ((rv 'read-events-until!) "s" 0 "t1" nil)
                   nil
                   (catch clojure.lang.ExceptionInfo e (ex-data e)))]
          (is (true? (:kaboom ex))))))))

(deftest mux-subscribe!-shares-one-remote-session-subscription
  (testing "multiple local listeners for one sid do not reconnect/open one SSE per tab"
    (let
      [mux-var
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
          (let
            [cleanup-a
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

(deftest list-resources-cached-never-blocks-the-caller
  ;; REGRESSION: the footer calls this on the render thread every frame. The
  ;; daemon round-trip MUST run in the background so a busy/slow daemon can't
  ;; stall painting. A cold read returns the last-known value (nil) instantly
  ;; and kicks a single-flight refresh; once it lands, subsequent reads are
  ;; served from cache. If someone reintroduces a synchronous round-trip this
  ;; test blocks for `slow-ms` and the timing assertion fails.
  (let
    [slow-ms
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
        (let
          [t0
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
          (Thread/sleep (+ slow-ms 250))
          (is (= 1 @calls) "only ONE background fetch runs per sid (single-flight)")
          (let
            [t1
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
  (let
    [slow-ms
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
        (let
          [t0
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
          (Thread/sleep (+ slow-ms 250))
          (is (= 1 @calls) "only ONE background fetch runs per sid (single-flight)")
          (let
            [t1
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
