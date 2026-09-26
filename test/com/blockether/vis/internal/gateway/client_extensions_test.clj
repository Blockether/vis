(ns com.blockether.vis.internal.gateway.client-extensions-test
  (:require [lazytest.experimental.interfaces.clojure-test :refer [deftest is]]
            [com.blockether.vis.contract.gateway :as gateway-contract]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.internal.gateway.discovery :as discovery]
            [com.blockether.vis.internal.gateway.server :as server]
            [com.blockether.vis.internal.gateway.server.instance :as instance]
            [com.blockether.vis.internal.gateway.state :as state]
            [com.blockether.vis.internal.loop :as lp]
            [com.blockether.vis.internal.util :as util])
  (:import [java.io ByteArrayInputStream]
           [java.util UUID]))

(defn- with-server
  [run]
  (let [state-atom
        instance/server-state

        before
        @state-atom

        now
        (util/now-ms)]

    (reset! state-atom {:require-token? false
                        :managed? false
                        :clients {"owner" {:connected-at now :last-seen-at now}}})
    (try (run state-atom) (finally (reset! state-atom before)))))

(defn- request
  [method sid suffix owner body]
  {:request-method method
   :uri (str "/v1/sessions/" sid "/" suffix)
   :headers (cond-> {"content-type" "application/json"
                     "x-vis-protocol" (str gateway-contract/protocol-version)
                     "x-vis-min-gateway-protocol" (str gateway-contract/minimum-gateway-protocol)}
              owner
              (assoc "x-vis-client-id" owner))
   :body (ByteArrayInputStream. (.getBytes ^String (wire/json-str body) "UTF-8"))})

(deftest callback-routes-require-a-live-owner-and-an-existing-session
  (with-server
    (fn [_]
      (let [sid
            (str (UUID/randomUUID))

            registered
            (atom [])

            handler
            (#'server/app "" [])]

        (with-redefs [state/soul
                      (fn [id]
                        (when (= sid (str id)) {"id" sid}))

                      lp/register-client-extensions!
                      (fn [id owner body live?]
                        (swap! registered conj [(str id) owner body (live?)])
                        {})]

          (doseq [owner [nil "missing"]]
            (is (= 403 (:status (handler (request :put sid "client-extensions" owner {}))))))
          (is (= 404
                 (:status (handler
                            (request :put (UUID/randomUUID) "client-extensions" "owner" {})))))
          (is (= 200
                 (:status (handler
                            (request :put sid "client-extensions" "owner" {"extensions" []})))))
          (is (= [[sid "owner" {"extensions" []} true]] @registered))
          (doseq [[method suffix] [[:get "client-calls"] [:post "client-calls/call/result"]
                                   [:post "client-calls/call/activity"]]]
            (is (= 403 (:status (handler (request method sid suffix "missing" {})))))))))))

(deftest expired-lease-cannot-be-revived-by-the-callback-request
  (with-server
    (fn [state-atom]
      (let [sid
            (str (UUID/randomUUID))

            expired
            (- (util/now-ms) 120001)

            handler
            (#'server/app "" [])]

        (swap! state-atom assoc-in [:clients "owner" :last-seen-at] expired)
        (with-redefs [state/soul
                      (constantly {"id" sid})

                      lp/register-client-extensions!
                      (fn [& _]
                        {})]

          (is (= 403 (:status (handler (request :put sid "client-extensions" "owner" {})))))
          (is (= expired (get-in @state-atom [:clients "owner" :last-seen-at]))))))))

(deftest local-pid-lease-outlives-idle-timeout-but-not-its-process
  (with-server
    (fn [state-atom]
      (let [expired (- (util/now-ms) 120001)]
        (swap! state-atom update-in [:clients "owner"] assoc :pid 42 :last-seen-at expired)
        (with-redefs [discovery/pid-alive-cached? #(= 42 %)]
          ;; A long application callback cannot pump the stdio pipe itself.
          (is (true? (instance/live-client? "owner")))
          (is (= expired (get-in @state-atom [:clients "owner" :last-seen-at])))
          (swap! state-atom assoc-in [:clients "owner" :pid] 43)
          (is (not (instance/live-client? "owner")))
          (swap! state-atom update-in [:clients "owner"] dissoc :pid)
          (is (not (instance/live-client? "owner"))))))))
