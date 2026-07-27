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

(deftest pairing-url-carries-alternate-hosts
  (testing
    "every other reachable host rides along as `alt=` so a phone that
            cannot route the first one falls back instead of failing"
    (with-redefs
      [pairing/candidate-hosts (fn [_]
                                 ["100.64.0.10" "192.168.0.5" "169.254.1.2"])]
      (let
        [url (pairing/pairing-url {:host "0.0.0.0" :port 7890 :token "tok"})
         alt (some-> (re-find #"[?&]alt=([^&]+)" url)
                     second
                     (java.net.URLDecoder/decode "UTF-8"))]

        (is (str/includes? url "url=http%3A%2F%2F100.64.0.10%3A7890"))
        (is (= ["http://192.168.0.5:7890"]
               (some-> alt
                       (str/split #","))))
        (is (not (str/includes? url "169.254")) "link-local is unroutable for a phone")))
    (with-redefs
      [pairing/candidate-hosts (fn [_]
                                 ["100.64.0.10"])]
      (is (not (str/includes? (pairing/pairing-url {:host "0.0.0.0" :port 7890}) "alt="))
          "a lone host adds no alt param"))))

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

(defn- qr-dark-at
  "Read the dark/light state of module (x, y) back out of a half-block render."
  [lines x y]
  (let
    [c
     (.charAt ^String (nth lines (quot (long y) 2)) x)

     top?
     (even? (long y))]

    (case c
      \█
      false

      \space
      true

      \▀
      (not top?)

      \▄
      top?)))

(deftest terminal-qr-is-scannable
  (testing "the render round-trips through a real QR decoder"
    (let
      [payload
       "vis://gateway?url=http%3A%2F%2F100.64.0.10%3A7890&token=abcdefghijklmnop"

       lines
       (str/split-lines (pairing/terminal-qr payload))

       width
       (count (first lines))

       margin
       4

       modules
       (- width (* 2 margin))

       bits
       (com.google.zxing.common.BitMatrix. modules modules)]

      (testing "a full 4-module quiet zone surrounds the symbol"
        (is (every? #(= (apply str (repeat width "█")) %)
                    (concat (take 2 lines) (take-last 2 lines))))
        (is (every? #(str/starts-with? % "████") lines))
        (is (every? #(str/ends-with? % "████") lines)))
      (doseq
        [y
         (range modules)

         x
         (range modules)]

        (when (qr-dark-at lines (+ x margin) (+ y margin)) (.set bits x y)))
      (is (= payload (.getText (.decode (com.google.zxing.qrcode.decoder.Decoder.) bits nil)))
          "block glyphs must paint the light modules, not the dark ones"))))

(deftest loopback-bind-refuses-to-print-a-dead-qr
  (testing
    "bound to loopback the interface scan still sees Tailscale/LAN addresses,
             but nothing listens there — so pairing prints the restart command
             instead of a QR the phone can never open"
    (with-redefs
      [pairing/iface-addresses (fn []
                                 ["100.109.18.77" "192.168.0.227"])]
      (let
        [lines (atom [])
         out (pairing/print-pairing! {:host "127.0.0.1"
                                      :port 7890
                                      :token "tok"
                                      :require-token? true
                                      :emit #(swap! lines conj (str %))})
         text (str/join "\n" @lines)]

        (is (nil? out) "no payload is produced")
        (is (not (str/includes? text "vis://")) "no unreachable pairing URL")
        (is (not (str/includes? text "█")) "no QR")
        (is (str/includes? text "only this machine can reach it"))
        (is (str/includes? text "vis gateway start --host 100.109.18.77")
            "remediation points at the Tailscale IP when the tailnet is up"))))
  (testing "no Tailscale — fall back to 0.0.0.0 guidance"
    (with-redefs
      [pairing/iface-addresses (fn []
                                 ["192.168.0.227"])]
      (let [lines (atom [])]
        (pairing/print-pairing! {:host "localhost" :port 7890 :emit #(swap! lines conj (str %))})
        (is (str/includes? (str/join "\n" @lines) "--host 0.0.0.0")))))
  (testing "a reachable bind host still prints URL + QR"
    (with-redefs
      [pairing/iface-addresses (fn []
                                 ["100.109.18.77"])]
      (let
        [lines (atom [])
         out (pairing/print-pairing! {:host "100.109.18.77"
                                      :port 7890
                                      :token "tok"
                                      :require-token? true
                                      :emit #(swap! lines conj (str %))})]

        (is (str/starts-with? (str out) "vis://gateway?"))
        (is (str/includes? (str/join "\n" @lines) "█"))))))

(deftest pair-implies-a-phone-reachable-bind
  (testing
    "asking to pair with no --host must never fall back to the loopback
           default: pairing IS the request for phone access, so the bind follows
           it — Tailscale IP when the tailnet is up, else every interface"
    (with-redefs
      [pairing/iface-addresses (fn []
                                 ["100.109.18.77" "192.168.0.227"])]
      (is (= "100.109.18.77" (pairing/pair-bind-host))))
    (with-redefs
      [pairing/iface-addresses (fn []
                                 ["192.168.0.227"])]
      (is (= "0.0.0.0" (pairing/pair-bind-host))))
    (with-redefs
      [pairing/iface-addresses (fn []
                                 [])]
      (is (= "0.0.0.0" (pairing/pair-bind-host)))))
  (testing
    "every auto-picked bind is non-loopback, so print-pairing! emits a
           real QR and server/start! forces the bearer token"
    (with-redefs
      [pairing/iface-addresses (fn []
                                 ["100.109.18.77"])]
      (is (false? (pairing/loopback-bind? (pairing/pair-bind-host)))))
    (with-redefs
      [pairing/iface-addresses (fn []
                                 [])]
      (is (false? (pairing/loopback-bind? (pairing/pair-bind-host)))))))
