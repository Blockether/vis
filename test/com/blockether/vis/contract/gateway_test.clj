(ns com.blockether.vis.contract.gateway-test
  "Characterization gates for the gateway surface before its implementation owners move."
  (:require [com.blockether.vis.contract.gateway :as contract]
            [com.blockether.vis.internal.gateway.protocol :as protocol]
            com.blockether.vis.internal.gateway.server
            [com.blockether.vis.internal.gateway.view :as gateway-view]
            [com.blockether.vis.internal.gateway.wire :as wire]
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

(defdescribe gateway-contract-test
             (it "loads a closed, independently owned gateway declaration"
                 (expect (= 1 contract/version))
                 (expect (= 99 (count contract/route-table)))
                 (expect (= 121 (count (contract/route-methods))))
                 (expect (= 32 (count contract/event-types))))
             (it "pins every built-in method and path from the runtime router"
                 (expect (= (mapv #(select-keys % [:path :methods]) contract/route-table)
                            (runtime-route-table))))
             (it "pins protocol numbers and compatibility header spellings"
                 (expect (= contract/protocol-version protocol/protocol-version))
                 (expect (= contract/minimum-client-protocol protocol/min-client-protocol))
                 (expect (= contract/minimum-gateway-protocol protocol/min-gateway-protocol))
                 (expect (= (contract/header :protocol) protocol/protocol-header))
                 (expect (= (contract/header :minimum-gateway-protocol)
                            protocol/min-gateway-header))
                 (expect (= (contract/header :client) protocol/client-header))
                 (expect (= (contract/header :client-version) protocol/client-version-header)))
             (it "pins terminal, queue-mirror and shared View event semantics"
                 (expect (= contract/turn-terminal-event-types wire/turn-terminal-event-types))
                 (expect (= contract/queue-mirror-event-types wire/queue-mirror-event-types))
                 (expect (= contract/view-events
                            {:open gateway-view/view-open-event
                             :patch gateway-view/view-patch-event
                             :close gateway-view/view-close-event}))))
