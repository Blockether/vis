(ns com.blockether.vis.internal.gateway.pairing-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [com.blockether.vis.internal.gateway.pairing :as pairing]))

(deftest pairing-url-is-a-scannable-vis-url
  (testing "payload carries gateway URL and bearer token"
    (with-redefs [pairing/candidate-hosts (fn [_]
                                            ["127.0.0.1"])]
      (let [payload (pairing/pairing-url {:host "127.0.0.1" :port 7890 :token "secret token"})]
        (is (str/starts-with? payload "vis://gateway?"))
        (is (str/includes? payload "url=http%3A%2F%2F127.0.0.1%3A7890"))
        (is (str/includes? payload "token=secret+token"))))))

(deftest terminal-qr-renders-non-empty-blocks
  (testing "CLI pairing can print a QR without shelling out"
    (let [qr (pairing/terminal-qr "vis://gateway?url=http%3A%2F%2F127.0.0.1%3A7890&token=s")]
      (is (not (str/blank? qr)))
      (is (or (str/includes? qr "█") (str/includes? qr "▀") (str/includes? qr "▄"))))))
