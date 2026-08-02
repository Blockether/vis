(ns com.blockether.vis.internal.gateway.pairing
  "Gateway pairing helpers for remote clients.

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
  (let
    [ips
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

(defn tailscale-hosts
  "Tailscale (100.64/10) IPv4 addresses currently bound to a live interface, in
  discovery order. Empty when Tailscale is not up, so callers can fall back to
  LAN / `0.0.0.0` guidance."
  []
  (vec (filter tailscale-ip? (iface-addresses))))

(defn pair-bind-host
  "Bind address to use when `--pair` is requested WITHOUT an explicit `--host`.

   Pairing on the loopback default is a contradiction: the QR would encode an
   address no socket is listening on and the phone would time out minutes later.
   Asking to pair IS asking for phone access, so it binds every interface.

   Binding only the Tailscale IP was the earlier, narrower choice and it broke
   the pairing link's own promise: `pairing-url` advertises the LAN addresses as
   `alt=` fallbacks, but nothing listened there, so a phone whose Tailscale was
   off or paused failed on EVERY candidate while sitting on the same Wi-Fi as
   the gateway. `0.0.0.0` is non-loopback exactly like the Tailscale IP was, so
   `server/start!` still forces the bearer token: this widens reach, never auth."
  []
  "0.0.0.0")

(defn pairing-url
  "The `vis://gateway` deep link. `url=` is the best guess (Tailscale first), and
  `alt=` carries the remaining routable hosts so a phone that cannot reach the
  first one (no Tailscale, different LAN) falls back instead of failing. IPv4
  link-local (169.254/16) is dropped from the alternates: no phone can route it,
  and every extra host makes the QR denser."
  [{:keys [host port token]}]
  (let
    [hosts
     (let [c (candidate-hosts host)]
       (if (seq c) c [host]))

     ->url
     (fn [h]
       (str "http://" h ":" port))

     alts
     (into [] (comp (remove #(str/starts-with? (str %) "169.254.")) (map ->url)) (rest hosts))]

    (str "vis://gateway?url="
         (url-encode (->url (first hosts)))
         (when (seq alts) (str "&alt=" (url-encode (str/join "," alts))))
         (when-not (str/blank? (str token)) (str "&token=" (url-encode token))))))

(defn pairing-json
  [{:keys [host port token require-token?] :as opts}]
  (let
    [host
     (or (first (candidate-hosts host)) host)

     url
     (str "http://" host ":" port)]

    (wire/json-str
      (cond->
        {:type "vis-gateway-pairing" :version 1 :url url :hosts (candidate-hosts (:host opts))}
        require-token?
        (assoc :token token)))))

(defn terminal-qr
  "Render `text` as a terminal QR code using Unicode half-blocks. Returns a string
  so tests and CLI callers can decide where to print it.

  Two properties matter for a phone camera to actually decode this:

  - a full 4-module quiet zone (the spec minimum; a 1-module margin scans only
    on a perfect white background), padded to an even module height so the
    bottom quiet zone survives the half-block row packing;
  - the block glyph paints the *light* modules, like `qrencode -t UTF8`, so the
    code reads correctly on the dark terminal themes everyone runs. Painting
    dark modules instead produces a photo-negative that most scanners reject."
  [text]
  (let
    [hints
     (doto (EnumMap. EncodeHintType) (.put EncodeHintType/MARGIN 4))

     matrix
     (.encode (QRCodeWriter.) text BarcodeFormat/QR_CODE 0 0 hints)

     w
     (.getWidth matrix)

     h
     (.getHeight matrix)

     ;; Pad to an even number of rows so the last half-block pair is a full
     ;; quiet-zone row rather than a clipped one.
     h*
     (if (even? h) h (inc h))

     light?
     (fn [x y]
       (or (>= (long y) h) (not (.get matrix x y))))]

    (str/join "\n"
              (for [y (range 0 h* 2)]
                (apply str
                  (for [x (range w)]
                    (let
                      [top? (light? x y)
                       bot? (light? x (inc (long y)))]

                      (cond (and top? bot?) "█"
                            top? "▀"
                            bot? "▄"
                            :else " "))))))))

(defn loopback-bind?
  "True when `host` is an address only this machine can reach. A phone can never
   open such a URL, so pairing against it is meaningless no matter how good the
   QR is."
  [host]
  (contains? #{"127.0.0.1" "localhost" "::1" "[::1]" "0:0:0:0:0:0:0:1"} (str host)))

(defn print-pairing!
  "Emit the companion pairing block (title, reachable hosts, `vis://` URL, and a
   terminal QR). Each line goes through `emit` (default `println`); CLI callers
   whose stdout is redirected to the log file pass a real-terminal writer so the
   QR is actually visible. Returns the pairing URL payload.

   Bound to loopback there is nothing to pair with: the interface scan still
   finds Tailscale/LAN addresses, but the listener is not on them, so a QR built
   from those would encode a URL that times out — the failure landing on the
   phone, minutes later, looking like a broken app. Refuse and print the restart
   command instead; returns nil."
  [{:keys [require-token? emit host] :or {emit println} :as opts}]
  (if (loopback-bind? host)
    (let [ts (first (tailscale-hosts))]
      (emit "")
      (emit "VIS companion pairing")
      (emit (str "not pairable: vis is bound to " host " — only this machine can reach it."))
      (emit "No QR printed: it would encode a URL your phone cannot open.")
      (emit "Restart on a host the phone can reach:")
      (emit (if ts
              (str "  vis-agent gateway start --host " ts
                   " --require-token --pair"
                   "   # your Tailscale IP — reachable from the phone on your tailnet")
              "  vis-agent gateway start --host 0.0.0.0 --require-token --pair"))
      (flush)
      nil)
    (let
      [payload
       (pairing-url (cond-> opts
                      (not require-token?)
                      (dissoc :token)))

       hosts
       (candidate-hosts host)]

      (emit "")
      (emit "VIS companion pairing")
      (emit "scan this in iOS Settings → Gateway → Scan QR")
      (when (seq hosts) (emit (str "reachable hosts: " (str/join ", " hosts))))
      (emit payload)
      (emit (terminal-qr payload))
      ;; Callers often park (the gateway daemon) right after this; `*out*` does
      ;; not autoflush, so an unflushed QR is an invisible QR.
      (flush)
      payload)))
