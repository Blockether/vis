(ns com.blockether.vis.internal.gateway.pairing-test
  (:require [clojure.string :as str]
            [lazytest.core :refer [around-each set-ns-context!]]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is testing]]
            [com.blockether.vis.internal.gateway.pairing :as pairing]))

;; Routing-table discovery is the one candidate that leaves this process. Stub it
;; off and clear its cache around every test, so each one sees the interfaces it
;; declares and never the network this machine happens to be plugged into.
(set-ns-context! [(around-each [run]
                               (reset! @#'pairing/default-route-cache nil)
                               (with-redefs-fn {#'pairing/discover-default-route (constantly nil)}
                                 run))])

(deftest pairing-url-is-a-scannable-vis-url
  (testing "payload carries gateway URL and bearer token"
    (with-redefs [pairing/candidate-hosts (fn [_]
                                            ["127.0.0.1"])]
      (let [payload (pairing/pairing-url {:host "127.0.0.1" :port 7890 :token "secret token"})]
        (is (str/starts-with? payload "vis://gateway?"))
        (is (str/includes? payload "url=http%3A%2F%2F127.0.0.1%3A7890"))
        (is (str/includes? payload "token=secret+token"))))))

(deftest pairing-url-carries-alternate-hosts
  (testing
    "every other reachable host rides along as `alt=` so a phone that
            cannot route the first one falls back instead of failing"
    (with-redefs [pairing/candidate-hosts (fn [_]
                                            ["100.64.0.10" "192.168.0.5" "169.254.1.2"])]
      (let [url (pairing/pairing-url {:host "0.0.0.0" :port 7890 :token "tok"})
            alt (some-> (re-find #"[?&]alt=([^&]+)" url)
                        second
                        (java.net.URLDecoder/decode "UTF-8"))]

        (is (str/includes? url "url=http%3A%2F%2F100.64.0.10%3A7890"))
        (is (= ["http://192.168.0.5:7890"]
               (some-> alt
                       (str/split #","))))
        (is (not (str/includes? url "169.254")) "link-local is unroutable for a phone")))
    (with-redefs [pairing/candidate-hosts (fn [_]
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

(deftest an-interface-that-refuses-to-describe-itself-is-skipped
  (testing
    "Linux answers an interface whose flags it cannot read with
            `SocketException: Invalid argument`: the scan drops that ONE
            interface, because a refusal here took the whole pairing answer
            and `GET /v1/capabilities` down with it"
    (with-redefs-fn {#'pairing/network-interfaces (fn []
                                                    [::refuses-every-query])}
      (fn []
        (is (= [] (#'pairing/iface-addresses)))
        (is (= [] (pairing/candidate-hosts "0.0.0.0")))))))

(deftest concrete-bind-advertises-only-the-bound-address
  (testing
    "a specific --host answers on that address and nowhere else, so the link
            leads with it and offers no fallback the socket cannot serve: a
            network that allows one address only must not get a tailnet `url=`"
    (with-redefs-fn {#'pairing/iface-addresses (fn []
                                                 ["100.109.18.77" "192.168.0.116"])}
      (fn []
        (is (= ["192.168.0.116"] (pairing/candidate-hosts "192.168.0.116")))
        (let [url (pairing/pairing-url {:host "192.168.0.116" :port 7890 :token "tok"})]
          (is (str/includes? url "url=http%3A%2F%2F192.168.0.116%3A7890"))
          (is (not (str/includes? url "alt=")) "nothing listens on the other interfaces")
          (is (not (str/includes? url "100.109.18.77")) "the tailnet address is not bound"))
        (is (= ["100.109.18.77" "192.168.0.116"] (pairing/candidate-hosts "0.0.0.0"))
            "a wildcard bind still offers every interface")))))

(deftest advertised-url-completes-an-operator-address
  (testing "a bare host, a host:port and a full URL all become one base URL"
    (is (= "http://192.168.0.1:7890" (pairing/advertised-url "192.168.0.1" 7890)))
    (is (= "http://192.168.0.1:8080" (pairing/advertised-url "192.168.0.1:8080" 7890)))
    (is (= "http://192.168.0.1" (pairing/advertised-url "http://192.168.0.1/" 7890))
        "a scheme the operator typed wins: only they know the port behind it")
    (is (= "https://gateway.example.com"
           (pairing/advertised-url "https://gateway.example.com" 7890))))
  (testing "nothing to advertise"
    (is (nil? (pairing/advertised-url nil 7890)))
    (is (nil? (pairing/advertised-url "  " 7890)))))

(deftest advertise-pins-the-link-to-the-route-that-works
  (testing
    "a network can insist on an address this machine cannot see, so --advertise
            takes `url=` and every scanned address falls back to `alt=`"
    (with-redefs-fn {#'pairing/iface-addresses (fn []
                                                 ["100.109.18.77" "192.168.0.116"])}
      (fn []
        (let [url
              (pairing/pairing-url
                {:host "0.0.0.0" :port 7890 :token "tok" :advertise "192.168.0.1"})

              alt
              (some-> (re-find #"[?&]alt=([^&]+)" url)
                      second
                      (java.net.URLDecoder/decode "UTF-8"))]

          (is (str/includes? url "url=http%3A%2F%2F192.168.0.1%3A7890"))
          (is (= ["http://100.109.18.77:7890" "http://192.168.0.116:7890"]
                 (some-> alt
                         (str/split #","))))))))
  (testing
    "a loopback bind plus an advertised route still prints a QR: the operator
            named a path Vis cannot see, and nothing else on this machine listens"
    (with-redefs [pairing/iface-addresses (fn []
                                            ["192.168.0.116"])]
      (let [lines (atom [])
            out (pairing/print-pairing! {:host "127.0.0.1"
                                         :port 7890
                                         :token "tok"
                                         :require-token? true
                                         :advertise "http://192.168.0.1"
                                         :emit #(swap! lines conj (str %))})
            text (str/join "\n" @lines)]

        (is (str/includes? (str out) "url=http%3A%2F%2F192.168.0.1"))
        (is (not (str/includes? (str out) "alt=")) "the loopback bind serves nothing else")
        (is (str/includes? text "advertising: http://192.168.0.1"))
        (is (str/includes? text "█") "the QR is printed")))))

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
  (let [c
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
    (let [payload
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
      (doseq [y
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
    (with-redefs [pairing/iface-addresses (fn []
                                            ["100.109.18.77" "192.168.0.227"])]
      (let [lines (atom [])
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
        (is (str/includes? text "vis-agent gateway start --host 100.109.18.77")
            "remediation points at the Tailscale IP when the tailnet is up"))))
  (testing "no Tailscale — fall back to 0.0.0.0 guidance"
    (with-redefs [pairing/iface-addresses (fn []
                                            ["192.168.0.227"])]
      (let [lines (atom [])]
        (pairing/print-pairing! {:host "localhost" :port 7890 :emit #(swap! lines conj (str %))})
        (is (str/includes? (str/join "\n" @lines) "--host 0.0.0.0")))))
  (testing "a reachable bind host still prints URL + QR"
    (with-redefs [pairing/iface-addresses (fn []
                                            ["100.109.18.77"])]
      (let [lines (atom [])
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
           default, and must never bind narrower than the pairing link
           advertises: `alt=` offers the LAN hosts as fallbacks, so the socket
           has to serve every interface"
    (with-redefs [pairing/iface-addresses (fn []
                                            ["100.109.18.77" "192.168.0.227"])]
      (is (= "0.0.0.0" (pairing/pair-bind-host))))
    (with-redefs [pairing/iface-addresses (fn []
                                            ["192.168.0.227"])]
      (is (= "0.0.0.0" (pairing/pair-bind-host))))
    (with-redefs [pairing/iface-addresses (fn []
                                            [])]
      (is (= "0.0.0.0" (pairing/pair-bind-host)))))
  (testing
    "every auto-picked bind is non-loopback, so print-pairing! emits a
           real QR and server/start! forces the bearer token"
    (with-redefs [pairing/iface-addresses (fn []
                                            ["100.109.18.77"])]
      (is (false? (pairing/loopback-bind? (pairing/pair-bind-host)))))
    (with-redefs [pairing/iface-addresses (fn []
                                            [])]
      (is (false? (pairing/loopback-bind? (pairing/pair-bind-host)))))))

(deftest router-address-is-never-a-dialable-candidate
  (testing
    "the router is the one address this machine does NOT hold and nothing ever
             verifies it answers — offered as a candidate it tied with the real
             LAN address and won, so a client on the same LAN dialed the router
             instead of Vis (#277)"
    (with-redefs-fn {#'pairing/iface-addresses (fn []
                                                 ["100.109.18.77" "192.168.0.116"])
                     #'pairing/discover-default-route (fn []
                                                        "192.168.0.1")}
      (fn []
        (is (= ["100.109.18.77" "192.168.0.116"] (pairing/candidate-hosts "0.0.0.0")))
        (let [url (pairing/pairing-url {:host "0.0.0.0" :port 7890 :token "tok"})]
          (is (str/includes? url "url=http%3A%2F%2F100.109.18.77%3A7890"))
          (is (not (str/includes? url "192.168.0.1%3A7890"))
              "and it does not ride along as an alt= either"))
        (is (= ["192.168.0.116"] (pairing/candidate-hosts "192.168.0.116"))
            "a concrete bind still answers on that address alone")))))

(deftest port-forward-hint-names-the-router-instead-of-offering-it
  (testing
    "the honest way in from outside the LAN is a port forward plus --advertise,
             so the router shows up as operator guidance and never as a URL the
             client dials (#277)"
    (with-redefs-fn {#'pairing/iface-addresses (fn []
                                                 ["192.168.0.116"])
                     #'pairing/discover-default-route (fn []
                                                        "192.168.0.1")}
      (fn []
        (let [lines (atom [])]
          (pairing/print-pairing! {:host "0.0.0.0"
                                   :port 7890
                                   :token "tok"
                                   :require-token? true
                                   :emit #(swap! lines conj (str %))})
          (let [text (str/join "\n" @lines)]
            (is (str/includes? text "reachable hosts: 192.168.0.116"))
            (is (str/includes? text "forward port 7890 on your router (192.168.0.1)"))
            (is (str/includes? text "--advertise"))
            (is (not (str/includes? text "192.168.0.1%3A7890"))
                "the pairing link still carries only addresses that answer"))))))
  (testing "an advertised route is already the way in, so the hint stays quiet"
    (with-redefs-fn {#'pairing/iface-addresses (fn []
                                                 ["192.168.0.116"])
                     #'pairing/discover-default-route (fn []
                                                        "192.168.0.1")}
      (fn []
        (let [lines (atom [])]
          (pairing/print-pairing! {:host "0.0.0.0"
                                   :port 7890
                                   :token "tok"
                                   :require-token? true
                                   :advertise "https://gateway.example.com"
                                   :emit #(swap! lines conj (str %))})
          (is (not (str/includes? (str/join "\n" @lines) "forward port"))))))))

(deftest wildcard-bind-is-the-one-that-also-answers-on-loopback
  (testing "a same-machine client can use 127.0.0.1 only when every interface is served (#277)"
    (is (true? (pairing/wildcard-bind? "0.0.0.0")))
    (is (true? (pairing/wildcard-bind? "::")))
    (is (true? (pairing/wildcard-bind? "[::]")))
    (is (true? (pairing/wildcard-bind? nil)) "a blank bind is the wildcard default")
    (is (false? (pairing/wildcard-bind? "192.168.0.116")))
    (is (false? (pairing/wildcard-bind? "127.0.0.1"))
        "a loopback bind answers nowhere else, so nothing else may be offered")))

(deftest public-default-gateway-is-never-offered
  (testing
    "a link carries the bearer token, so neither the candidates nor the printed
             hint leave the private ranges for the ISP's router"
    (with-redefs-fn {#'pairing/iface-addresses (fn []
                                                 ["192.168.0.116"])
                     #'pairing/discover-default-route (fn []
                                                        "203.0.113.1")}
      (fn []
        (is (= ["192.168.0.116"] (pairing/candidate-hosts "0.0.0.0")))
        (let [lines (atom [])]
          (pairing/print-pairing! {:host "0.0.0.0"
                                   :port 7890
                                   :token "tok"
                                   :require-token? true
                                   :emit #(swap! lines conj (str %))})
          (is (not (str/includes? (str/join "\n" @lines) "203.0.113.1"))))))))

(deftest default-route-reads-the-platform-routing-table
  (testing "Linux keeps `/proc/net/route` gateways as little-endian hex"
    (is (= "192.168.0.1"
           (#'pairing/parse-proc-net-route
            (str "Iface\tDestination\tGateway\tFlags\tRefCnt\tUse\tMetric\tMask\n"
                 "en0\t00000000\t0100A8C0\t0003\t0\t0\t100\t00000000\n"
                 "en0\t0000A8C0\t00000000\t0001\t0\t0\t100\t00FFFFFF\n"))))
    (is (nil? (#'pairing/parse-proc-net-route
               (str "Iface\tDestination\tGateway\n" "en0\t0000A8C0\t00000000\n")))
        "a subnet route is not a default route"))
  (testing "macOS answers `route -n get default` in prose"
    (is (= "192.168.0.1"
           (#'pairing/parse-route-get-default
            (str "   route to: default\n"
                 "destination: default\n" "       mask: default\n"
                 "    gateway: 192.168.0.1\n" "  interface: en0\n"))))
    (is (nil? (#'pairing/parse-route-get-default "route: writing to routing socket: not in table"))
        "no default route means no candidate")))

(deftest default-route-is-cached-between-calls
  (testing "pairing can be printed again on a new network, so discovery cannot run per call"
    (let [calls (atom 0)]
      (with-redefs-fn {#'pairing/discover-default-route (fn []
                                                          (swap! calls inc)
                                                          "192.168.0.1")}
        (fn []
          (is (= "192.168.0.1" (#'pairing/default-route-host)))
          (is (= "192.168.0.1" (#'pairing/default-route-host)))
          (is (= 1 @calls)))))))
