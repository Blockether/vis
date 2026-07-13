(ns com.blockether.vis.internal.gateway.pairing
  "Gateway pairing helpers for mobile companions.

  The QR payload is deliberately tiny and URL-shaped so native apps can scan it
  without an HTTP round trip:

    vis://gateway?url=http%3A%2F%2F100.64.0.10%3A7890&token=...

  Tailscale fits naturally: if a 100.64.0.0/10 interface is present we prefer it
  over LAN addresses, otherwise we fall back to site-local IPv4 addresses."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.gateway.wire :as wire])
  (:import (com.google.zxing BarcodeFormat EncodeHintType)
           (com.google.zxing.qrcode QRCodeWriter)
           (java.net Inet4Address NetworkInterface URLEncoder)
           (java.nio.charset StandardCharsets)
           (java.util EnumMap)))

(defn- url-encode [s] (URLEncoder/encode (str s) StandardCharsets/UTF_8))

(defn- iface-addresses
  []
  (->> (enumeration-seq (NetworkInterface/getNetworkInterfaces))
       (remove #(.isLoopback ^NetworkInterface %))
       (remove #(not (.isUp ^NetworkInterface %)))
       (mapcat #(enumeration-seq (.getInetAddresses ^NetworkInterface %)))
       (filter #(instance? Inet4Address %))
       (remove #(.isLoopbackAddress ^Inet4Address %))
       (map #(.getHostAddress ^Inet4Address %))
       distinct
       vec))

(defn- tailscale-ip?
  [ip]
  (let [[a b] (map #(Integer/parseInt %) (take 2 (str/split (str ip) #"\.")))]
    (and (= 100 a) (<= 64 b 127))))

(defn- site-local-ip?
  [ip]
  (or (str/starts-with? ip "10.")
      (str/starts-with? ip "192.168.")
      (when-let [[_ b] (re-matches #"172\.(\d+)\..*" ip)]
        (<= 16 (Integer/parseInt b) 31))))

(defn candidate-hosts
  "Reachable hostnames/IPs worth showing in a pairing QR, in preference order.
  Tailscale addresses come first because they keep working off-LAN; then LAN;
  then the server's bind host when concrete."
  [bind-host]
  (let [ips
        (iface-addresses)

        concrete
        (when-not (#{"0.0.0.0" "::" "127.0.0.1" "localhost"} (str bind-host)) (str bind-host))]

    (->> (concat (filter tailscale-ip? ips)
                 (filter site-local-ip? ips)
                 (remove #(or (tailscale-ip? %) (site-local-ip? %)) ips)
                 [concrete])
         (remove str/blank?)
         distinct
         vec)))

(defn pairing-url
  [{:keys [host port token]}]
  (let [host
        (or (first (candidate-hosts host)) host)

        url
        (str "http://" host ":" port)]

    (str "vis://gateway?url="
         (url-encode url)
         (when-not (str/blank? (str token)) (str "&token=" (url-encode token))))))

(defn pairing-json
  [{:keys [host port token require-token?] :as opts}]
  (let [host
        (or (first (candidate-hosts host)) host)

        url
        (str "http://" host ":" port)]

    (wire/json-str (cond-> {:type "vis-gateway-pairing"
                            :version 1
                            :url url
                            :hosts (candidate-hosts (:host opts))}
                     require-token?
                     (assoc :token token)))))

(defn terminal-qr
  "Render `text` as a terminal QR code using Unicode half-blocks. Returns a string
  so tests and CLI callers can decide where to print it."
  [text]
  (let [hints
        (doto (EnumMap. EncodeHintType) (.put EncodeHintType/MARGIN 1))

        matrix
        (.encode (QRCodeWriter.) text BarcodeFormat/QR_CODE 0 0 hints)

        w
        (.getWidth matrix)

        h
        (.getHeight matrix)]

    (str/join "\n"
              (for [y (range 0 h 2)]
                (apply str
                  (for [x (range w)]
                    (let [top? (.get matrix x y)
                          bot? (and (< (inc y) h) (.get matrix x (inc y)))]

                      (cond (and top? bot?) "█"
                            top? "▀"
                            bot? "▄"
                            :else " "))))))))

(defn print-pairing!
  [{:keys [require-token?] :as opts}]
  (let [payload
        (pairing-url (cond-> opts
                       (not require-token?)
                       (dissoc :token)))

        hosts
        (candidate-hosts (:host opts))]

    (println)
    (println "VIS companion pairing")
    (println "scan this in iOS Settings → Gateway → Scan QR")
    (when (seq hosts) (println (str "reachable hosts: " (str/join ", " hosts))))
    (println payload)
    (println (terminal-qr payload))
    (when (= "127.0.0.1" (str (:host opts)))
      (println
        "note: 127.0.0.1 is phone-local; for device pairing start with --host 0.0.0.0 --require-token or use a Tailscale host."))
    payload))
