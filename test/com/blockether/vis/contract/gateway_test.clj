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
      (expect (= 1 contract/version))
      (expect (= 99 (count contract/route-table)))
      (expect (= 121 (count (contract/route-methods))))
      (expect (= 32 (count contract/event-types)))
      (expect (= {:transcribe "voice.job" :synthesize "speech.job"} contract/job-events))
      (expect (= ["model" "provider" "llm_selected" "llm_actual" "is_llm_fallback"
                  "llm_routing_trace" "tokens" "cost" "confidence" "eval" "duration_ms"
                  "utilization"]
                 contract/turn-meta-keys)))
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
