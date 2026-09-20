(ns com.blockether.vis.contract.gateway-test
  "Gateway payload validation and parity with the runtime HTTP surface."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.contract.document :as document]
            [com.blockether.vis.contract.gateway :as contract]
            com.blockether.vis.internal.gateway.server
            [lazytest.core :refer [defdescribe expect it]]
            [reitit.core :as reitit]))

(def ^:private http-methods #{:delete :get :patch :post :put})

(defn- runtime-route-table
  []
  (let [router-var
        (ns-resolve 'com.blockether.vis.internal.gateway.server 'router)

        router-fn
        (var-get router-var)]

    (->> (reitit/routes (router-fn "contract-characterization" []))
         (map (fn [[path data]]
                {:path path :methods (into #{} (filter #(contains? data %) http-methods))}))
         (sort-by :path)
         vec)))

(defdescribe
  gateway-contract-test
  (it
    "loads the gateway schema and consumed transport metadata"
    (let [{:keys [ttl-ms touch-ms keepalive-ms keepalive-timeout-ms]} contract/client-lease]
      (expect (< 0 touch-ms keepalive-ms ttl-ms))
      (expect (< 0 keepalive-timeout-ms keepalive-ms)))
    (expect (= 130 (count contract/route-table)))
    (expect (= 161 (count (contract/route-methods))))
    (expect (= {:none 108 :json 49 :binary 4}
               (frequencies (map :request (mapcat (comp vals :operations) contract/route-table)))))
    (expect (= {:json 143 :resource 2 :sse 5 :empty 3 :binary 5 :negotiated 1 :html 1 :markdown 1}
               (frequencies (map :response (mapcat (comp vals :operations) contract/route-table)))))
    (expect (= 35 (count contract/event-types)))
    (expect (= {:transcribe "voice.job" :synthesize "speech.job"} contract/job-events))
    (expect (= #{"model" "provider" "llm_selected" "llm_actual" "is_llm_fallback"
                 "llm_routing_trace" "tokens" "cost" "confidence" "eval" "duration_ms"
                 "utilization"}
               (set contract/turn-meta-keys))))
  (it "reads declarations directly from the schema, without a parallel catalog"
      (let [gateway
            (document/schema-document "gateway")

            devices
            (first (filter #(= "/v1/devices" (get % "path")) (get gateway "x-vis-routes")))

            events
            (mapv #(get % "const") (get-in gateway ["$defs" "session_event_type" "oneOf"]))]

        (expect (nil? (io/resource "vis-contract/gateway.json")))
        (expect (= "#/$defs/session" (get gateway "$ref")))
        (expect (= {"request" "none" "response" "json"} (get-in devices ["operations" "get"])))
        (expect (= {"request" "json" "response" "json"} (get-in devices ["operations" "post"])))
        (expect (= (sort events) events))
        (expect (= "subscription.ready"
                   (get-in gateway ["$defs" "subscription_ready" "properties" "type" "const"])))
        (expect (= #{"build" "min_client" "min_gateway" "protocol" "version"}
                   (set (get-in gateway ["$defs" "handshake" "required"]))))))
  (it "pins every built-in operation path and method from the runtime router"
      (expect (= (mapv (fn [{:keys [path operations]}]
                         {:path path :methods (set (keys operations))})
                       (sort-by :path contract/route-table))
                 (runtime-route-table))))
  (it "declares each request and successful response transport"
      (let [operation contract/operation]
        (expect (= {:request :none :response :resource} (operation :get "/docs")))
        (expect (= {:request :none :response :negotiated} (operation :get "/metrics")))
        (expect (= {:request :binary :response :json} (operation :post "/v1/speech/voices")))
        (expect (= {:request :none :response :empty} (operation :delete "/v1/sessions/:sid")))
        (expect (= {:request :json :response :json}
                   (operation :post "/v1/sessions/:sid/council/wake")))
        (expect (= {:request :none :response :sse} (operation :get "/v1/events")))
        (expect (= {:request :none :response :binary}
                   (operation :get "/v1/sessions/:sid/speech/jobs/:job-id/audio")))))
  (it "owns protocol compatibility without a runtime mirror"
      (expect (= 13 contract/protocol-version))
      (expect (= 13 contract/minimum-client-protocol))
      (expect (= 13 contract/minimum-gateway-protocol))
      (expect (every? (set (keys (get-in (document/schema-document "gateway")
                                         ["$defs" "http_headers" "properties"])))
                      ["x-vis-protocol" "x-vis-min-gateway-protocol" "x-vis-client"
                       "x-vis-client-version"]))
      (expect (= {:protocol 3 :min-client 2 :min-gateway 1 :version "1.2.3" :build "abc123def456"}
                 (contract/wire->handshake {"protocol" 3
                                            "min_client" "2"
                                            "min_gateway" 1.0
                                            "version" "1.2.3"
                                            "build" "abc123def456"})))
      (expect (= "client-too-old"
                 (:reason (contract/verdict {:gateway-protocol 2
                                             :gateway-min-client 2
                                             :client-protocol 1
                                             :client-min-gateway 1})))))
  (it "builds handshake and error response envelopes"
      (expect (=
                {:protocol 13 :min-client 13 :min-gateway 13 :version "1.2.3" :build "abc123def456"}
                (contract/handshake {:version "1.2.3" :build "abc123def456"})))
      (expect (= {"error" {"type" "invalid-request" "message" "replacement" "session_id" "s1"}}
                 (contract/error-body :mcp/invalid-request
                                      "original"
                                      {:message "replacement" :session_id "s1"}))))
  (it "owns session, journal and ready envelopes"
      (expect (= {"schema" 1 "seq" 7 "ts" 9 "session_id" "s1" "type" "turn.started" "text" "hello"}
                 (contract/stamp-session-event {"schema" 99 "session_id" "spoofed" "text" "hello"}
                                               "s1" 7
                                               9 "turn.started")))
      (let [event
            {"type" "turn.started"}

            line
            (contract/stamp-journal-line event "producer-1" 42 true)]

        (expect (= "producer-1" (contract/journal-producer line)))
        (expect (= 42 (contract/journal-pid line)))
        (expect (contract/journal-stored? line))
        (expect (= event (contract/strip-journal-metadata line))))
      (expect (= {"type" "subscription.ready"
                  "session_id" "s1"
                  "cursor" 7
                  "current_turn_id" "t1"
                  "is_live" true
                  "server_time_ms" 9
                  "goal" nil
                  "agent_name" "Ada"
                  "latest_iteration" 4}
                 (contract/subscription-ready-event {:session-id "s1"
                                                     :cursor 7
                                                     :current-turn-id "t1"
                                                     :is-live true
                                                     :server-time-ms 9
                                                     :agent-name "Ada"
                                                     :latest-iteration 4}))))
  (it "exports generated View event constants"
      (expect (= {:open contract/view-open-event
                  :patch contract/view-patch-event
                  :close contract/view-close-event}
                 contract/view-events))
      (let [source (slurp "apps/vis-companion/src/lib/view.ts")]
        (expect (every? (fn [[constant event]]
                          (str/includes? source (str "export const " constant " = '" event "';")))
                        [["VIEW_OPEN_EVENT" contract/view-open-event]
                         ["VIEW_PATCH_EVENT" contract/view-patch-event]
                         ["VIEW_CLOSE_EVENT" contract/view-close-event]])))))

(defdescribe
  gateway-payload-schema-test
  (it "validates the handshake itself, not a dictionary of its keys"
      (let [handshake (contract/handshake {:version "1.2.3" :build "abc123"})]
        (expect (document/valid? "gateway" "handshake" handshake))
        (expect (not (document/valid? "gateway" "handshake" (dissoc handshake :protocol))))
        (expect (not (document/valid? "gateway" "handshake" (assoc handshake :protocol 99))))))
  (it "validates real error responses and keeps supported error extras"
      (expect (document/valid-json? "gateway"
                                    "error_response"
                                    (contract/error-body :invalid "Bad request" {:detail "extra"})))
      (expect (not (document/valid-json? "gateway" "error_response" {"error" {"type" "invalid"}}))))
  (it "validates stamped events and refuses unknown event names"
      (let [event (contract/stamp-session-event {"text" "hello"} "s1" 7 9 "turn.started")]
        (expect (document/valid-json? "gateway" "session_event" event))
        (expect
          (document/valid-json? "gateway" "session_event" (assoc event "type" "session.deleted")))
        (expect (not (document/valid-json? "gateway" "session_event" (dissoc event "seq"))))
        (expect (not (document/valid-json? "gateway"
                                           "session_event"
                                           (assoc event "type" "unknown.event"))))))
  (it "validates ready frames with nullable goal and turn identity"
      (let [event (contract/subscription-ready-event {:session-id "s1"
                                                      :cursor 0
                                                      :current-turn-id nil
                                                      :is-live false
                                                      :server-time-ms 9
                                                      :goal nil})]
        (expect (document/valid-json? "gateway" "subscription_ready" event))
        (expect (not (document/valid-json? "gateway" "subscription_ready" (dissoc event "goal"))))
        (expect
          (not (document/valid-json? "gateway" "subscription_ready" (assoc event "cursor" "0"))))))
  (it "derives terminal and queue behavior from event schema annotations"
      (expect (= #{"turn.completed" "turn.failed" "turn.cancelled"}
                 contract/turn-terminal-event-types))
      (expect (= #{"turn.queued" "turn.queued.deleted" "turn.queued.updated" "turn.queued.drained"
                   "queue.paused" "queue.resumed"}
                 contract/queue-mirror-event-types)))
  (it "keeps the session-group palette closed, ordered and derived from the schema"
      ;; BLO-167: a group carries a palette TOKEN, never a hex colour, so every
      ;; surface can ink it in its own theme.
      (expect (= ["slate" "blue" "green" "amber" "red" "violet" "cyan" "pink"]
                 contract/session-group-colors))
      (expect (= "slate" contract/default-session-group-color))
      (expect (every? contract/session-group-color? contract/session-group-colors))
      (expect (not (contract/session-group-color? "#ff00ff")))
      (expect (not (contract/session-group-color? "SLATE")))
      (expect (not (contract/session-group-color? nil)))))
