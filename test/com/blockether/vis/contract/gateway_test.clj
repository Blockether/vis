(ns com.blockether.vis.contract.gateway-test
  "Characterization gates for the gateway surface before its implementation owners move."
  (:require [clojure.string :as str]
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
  (it "loads a closed, independently owned gateway declaration"
      (expect (= 3 contract/version))
      (expect (= 99 (count contract/route-table)))
      (expect (= 121 (count (contract/route-methods))))
      (expect (= 32 (count contract/event-types)))
      (expect (= {:transcribe "voice.job" :synthesize "speech.job"} contract/job-events))
      (expect (= ["model" "provider" "llm_selected" "llm_actual" "is_llm_fallback"
                  "llm_routing_trace" "tokens" "cost" "confidence" "eval" "duration_ms"
                  "utilization"]
                 contract/turn-meta-keys)))
  (it "renders deterministic language-neutral gateway data"
      (let [gateway
            (contract/package-document)

            devices
            (first (filter #(= "/v1/devices" (get % "path")) (get gateway "routes")))]

        (expect (= 3 (get gateway "version")))
        (expect (= ["get" "post"] (get devices "methods")))
        (expect (= (sort (get-in gateway ["events" "session"]))
                   (get-in gateway ["events" "session"])))
        (expect (= "subscription.ready"
                   (get-in gateway ["envelopes" "subscription_ready" "event"])))
        (expect (= {"build" "build"
                    "min_client" "min_client"
                    "min_gateway" "min_gateway"
                    "protocol" "protocol"
                    "version" "version"}
                   (get-in gateway ["envelopes" "handshake" "keys"])))
        (expect (= {"message" "message" "type" "type"}
                   (get-in gateway ["envelopes" "error_response" "error_keys"])))))
  (it "pins every built-in method and path from the runtime router"
      (expect (= (mapv #(select-keys % [:path :methods]) contract/route-table)
                 (runtime-route-table))))
  (it "owns protocol compatibility without a runtime mirror"
      (expect (= 12 contract/protocol-version))
      (expect (= 12 contract/minimum-client-protocol))
      (expect (= 12 contract/minimum-gateway-protocol))
      (expect (= "x-vis-protocol" (contract/header :protocol)))
      (expect (= "x-vis-min-gateway-protocol" (contract/header :minimum-gateway-protocol)))
      (expect (= "x-vis-client" (contract/header :client)))
      (expect (= "x-vis-client-version" (contract/header :client-version)))
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
                {:protocol 12 :min-client 12 :min-gateway 12 :version "1.2.3" :build "abc123def456"}
                (contract/handshake {:version "1.2.3" :build "abc123def456"})))
      (expect (= {"error" {"type" "invalid-request" "message" "replacement" "session_id" "s1"}}
                 (contract/error-body :mcp/invalid-request
                                      "original"
                                      {:message "replacement" :session_id "s1"}))))
  (it "owns session, journal and ready envelopes"
      (expect
        (= {:schema "schema" :sequence "seq" :session-id "session_id" :timestamp "ts" :type "type"}
           contract/session-event-keys))
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
                  "latest_iteration" 4}
                 (contract/subscription-ready-event {:session-id "s1"
                                                     :cursor 7
                                                     :current-turn-id "t1"
                                                     :is-live true
                                                     :server-time-ms 9
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
