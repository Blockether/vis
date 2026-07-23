(ns com.blockether.vis.internal.gateway.pairing-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [com.blockether.vis.internal.gateway.pairing :as pairing]))

(deftest pairing-url-is-a-scannable-vis-url
  (testing "payload carries gateway URL and bearer token"
    (with-redefs
      [pairing/candidate-hosts (fn [_]
                                 ["127.0.0.1"])]
      (let [payload (pairing/pairing-url {:host "127.0.0.1" :port 7890 :token "secret token"})]
        (is (str/starts-with? payload "vis://gateway?"))
        (is (str/includes? payload "url=http%3A%2F%2F127.0.0.1%3A7890"))
        (is (str/includes? payload "token=secret+token"))))))

(deftest candidate-hosts-prefers-tailscale
  (testing "Tailscale 100.64/10 addresses are offered before LAN addresses"
    (with-redefs-fn {#'pairing/iface-addresses (fn []
                                                 ["192.168.0.45" "10.1.2.3" "100.109.18.77"
                                                  "8.8.8.8"])}
      (fn []
        (is (= ["100.109.18.77" "192.168.0.45" "10.1.2.3" "8.8.8.8"]
               (pairing/candidate-hosts "0.0.0.0")))
        (is (str/includes? (pairing/pairing-url {:host "0.0.0.0" :port 7890 :token "tok"})
                           "url=http%3A%2F%2F100.109.18.77%3A7890"))))))

(deftest tailscale-hosts-selects-only-tailnet-ips
  (testing "only 100.64/10 addresses are returned, in discovery order"
    (with-redefs-fn {#'pairing/iface-addresses (fn []
                                                 ["192.168.0.45" "100.109.18.77" "10.1.2.3"
                                                  "100.72.5.9"])}
      (fn []
        (is (= ["100.109.18.77" "100.72.5.9"] (pairing/tailscale-hosts)))))
    (with-redefs-fn {#'pairing/iface-addresses (fn []
                                                 ["192.168.0.45" "10.1.2.3"])}
      (fn []
        (is (= [] (pairing/tailscale-hosts)))))))

(deftest terminal-qr-renders-non-empty-blocks
  (testing "CLI pairing can print a QR without shelling out"
    (let [qr (pairing/terminal-qr "vis://gateway?url=http%3A%2F%2F127.0.0.1%3A7890&token=s")]
      (is (not (str/blank? qr)))
      (is (or (str/includes? qr "█") (str/includes? qr "▀") (str/includes? qr "▄"))))))
