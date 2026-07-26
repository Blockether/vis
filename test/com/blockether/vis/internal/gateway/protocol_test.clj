(ns com.blockether.vis.internal.gateway.protocol-test
  (:require [clojure.test :refer [deftest is testing]]
            [com.blockether.vis.internal.gateway.client :as client]
            [com.blockether.vis.internal.gateway.protocol :as protocol]))

(defn- client-var [sym] (ns-resolve 'com.blockether.vis.internal.gateway.client sym))

(deftest handshake-wire-roundtrip-test
  (testing "canonical string-keyed handshakes parse into engine keys"
    (is (= {:protocol 3 :min-client 2 :min-gateway 1 :version "1.2.3"}
           (protocol/wire->handshake
             {"protocol" 3 "min_client" "2" "min_gateway" 1.0 "version" "1.2.3"}))))
  (testing "an old peer with no handshake remains compatible"
    (is (= {:protocol nil :min-client nil :min-gateway nil :version nil}
           (protocol/wire->handshake {})))
    (is (= "unknown" (:reason (protocol/client-verdict "vis-test" nil))))
    (is (true? (:is-compatible (protocol/client-verdict "vis-test" nil))))))

(deftest compatibility-verdict-test
  (testing "a gateway rejects an explicitly too-old client"
    (let
      [verdict
       (protocol/verdict
         {:gateway-protocol 3 :gateway-min-client 2 :client-protocol 1 :client-min-gateway 1})]
      (is (false? (:is-compatible verdict)))
      (is (= "client-too-old" (:reason verdict)))
      (is (= "client" (:upgrade verdict)))))
  (testing "a client rejects an explicitly too-old gateway"
    (let
      [verdict
       (protocol/verdict
         {:gateway-protocol 1 :gateway-min-client 1 :client-protocol 3 :client-min-gateway 2})]
      (is (false? (:is-compatible verdict)))
      (is (= "gateway-too-old" (:reason verdict)))
      (is (= "gateway" (:upgrade verdict)))))
  (testing "compatible peers agree"
    (is (= "ok"
           (:reason (protocol/verdict {:gateway-protocol 3
                                       :gateway-min-client 2
                                       :client-protocol 3
                                       :client-min-gateway 2}))))))

(deftest client-records-the-nested-health-handshake-test
  (let
    [handshake-atom
     @(client-var 'gateway-handshake*)

     previous
     @handshake-atom

     body
     {"status" "ok" "protocol" {"protocol" 3 "min_client" 2 "min_gateway" 1 "version" "3.0.0"}}]

    (try (is (= body ((client-var 'note-handshake!) body)))
         (is (= {:protocol 3 :min-client 2 :min-gateway 1 :version "3.0.0"} @handshake-atom))
         (is (= "client-too-old" (:reason (client/compatibility))))
         (finally (reset! handshake-atom previous)))))
