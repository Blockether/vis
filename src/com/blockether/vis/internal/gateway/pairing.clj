(ns com.blockether.vis.internal.gateway.pairing
  "Gateway pairing helpers for remote clients.

  The QR payload is deliberately tiny and URL-shaped so native apps can scan it
  without an HTTP round trip:

    vis://gateway?url=http%3A%2F%2F100.64.0.10%3A7890&token=...

  Tailscale fits naturally: if a 100.64.0.0/10 interface is present we prefer it
  over LAN addresses, otherwise we fall back to site-local IPv4 addresses."
  (:require [clojure.string :as str]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.internal.util :as util])
  (:import (com.google.zxing BarcodeFormat EncodeHintType)
           (com.google.zxing.qrcode QRCodeWriter)
           (java.io File)
           (java.lang ProcessBuilder$Redirect)
           (java.net Inet4Address NetworkInterface URLEncoder)
           (java.nio.charset StandardCharsets)
           (java.util EnumMap)
           (java.util.concurrent TimeUnit)))

(defn- url-encode [s] (URLEncoder/encode (str s) StandardCharsets/UTF_8))

(def ^:private LOOPBACK_HOSTS
  "Addresses that only this machine can dial. A bind on one of them serves
  nothing else, so neither the pairing link nor its `alt=` list may offer the
  scanned interfaces alongside it."
  #{"127.0.0.1" "localhost" "::1" "[::1]" "0:0:0:0:0:0:0:1"})

(defn- network-interfaces
  "Every interface the OS admits to, or none when it refuses to enumerate them."
  []
  (try (enumeration-seq (NetworkInterface/getNetworkInterfaces)) (catch Exception _ nil)))

(defn- iface-ips
  "The non-loopback IPv4 addresses `nif` currently holds, or none when the
  interface will not describe itself. Linux answers an interface whose flags it
  cannot read with `SocketException: Invalid argument`, and that ONE interface
  must not take pairing and `/v1/capabilities` down with it."
  [^NetworkInterface nif]
  (try (when (and (not (.isLoopback nif)) (.isUp nif))
         (->> (enumeration-seq (.getInetAddresses nif))
              (filter #(instance? Inet4Address %))
              (remove #(.isLoopbackAddress ^Inet4Address %))
              (map #(.getHostAddress ^Inet4Address %))))
       (catch Exception _ nil)))

(defn- iface-addresses
  []
  (->> (network-interfaces)
       (mapcat iface-ips)
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

(def ^:private DEFAULT_ROUTE_TTL_MS
  "How long a discovered router address stays usable. Pairing can be printed
  again after a laptop changes networks, so the answer is cached briefly
  instead of being rediscovered on every print or frozen for the process."
  30000)

(def ^:private default-route-cache (atom nil))

(defn- parse-proc-net-route
  "IPv4 default gateway from a Linux `/proc/net/route` table, or nil. The
  gateway column is little-endian hex, so `0100A8C0` reads 192.168.0.1."
  [table]
  (some (fn [line]
          (let [[_ destination gateway] (str/split (str/trim (str line)) #"\s+")]
            (when (and (= "00000000" destination)
                       (re-matches #"[0-9A-Fa-f]{8}" (str gateway))
                       (not= "00000000" gateway))
              (->> (re-seq #".." gateway)
                   (map #(Integer/parseInt ^String % 16))
                   reverse
                   (str/join ".")))))
        (rest (str/split-lines (str table)))))

(defn- parse-route-get-default
  "IPv4 default gateway from BSD/macOS `route -n get default` output, or nil."
  [output]
  (second (re-find #"(?m)^\s*gateway:\s*(\d+(?:\.\d+){3})\s*$" (str output))))

(defn- command-stdout
  "stdout of a short-lived command, or nil when it is missing, fails or hangs."
  [argv]
  (try (let [^Process proc
             (.start (doto (ProcessBuilder. ^java.util.List argv)
                       (.redirectError ProcessBuilder$Redirect/DISCARD)))

             out
             (slurp (.getInputStream proc))]

         (if (.waitFor proc 2 TimeUnit/SECONDS) out (do (.destroyForcibly proc) nil)))
       (catch Exception _ nil)))

(defn- discover-default-route
  "Ask the OS routing table for the router this machine sends off-subnet traffic
  to. Linux answers from `/proc/net/route` with no subprocess; BSD and macOS
  need `route`, which a service-launched daemon may not have on its PATH."
  []
  (let [proc-table (File. "/proc/net/route")]
    (if (.canRead proc-table)
      (some-> (try (slurp proc-table) (catch Exception _ nil))
              parse-proc-net-route)
      (some #(parse-route-get-default (command-stdout %))
            [["/sbin/route" "-n" "get" "default"] ["route" "-n" "get" "default"]]))))

(defn- default-route-host
  "This machine's router, when it is a site-local address - otherwise nil.

  Printed as an operator hint, never offered as a candidate: this machine does
  not hold that address, nothing verifies anything answers there, and a client
  that picks it up dials the router instead of Vis (#277). The honest way in
  from outside the LAN is a port forward plus `--advertise <public host>`. A
  public default gateway is dropped: a link carrying a bearer token is a LAN
  guess, never an invitation to the ISP's router."
  []
  (let [now
        (util/now-ms)

        cached
        @default-route-cache]

    (if (and cached (< (- now (long (:at cached))) (long DEFAULT_ROUTE_TTL_MS)))
      (:host cached)
      (let [found
            (some-> (discover-default-route)
                    str/trim
                    not-empty)

            host
            (when (and found (site-local-ip? found)) found)]

        (reset! default-route-cache {:at now :host host})
        host))))

(defn wildcard-bind?
  "True when `host` names every interface instead of one address. Such a
   listener also answers on loopback, which is how a client on this same
   machine reaches it without crossing the LAN (#277)."
  [host]
  (contains? #{"" "0.0.0.0" "::" "[::]"} (str/trim (str host))))

(defn candidate-hosts
  "Reachable hostnames/IPs worth showing in a pairing QR, in preference order.

  A concrete bind host is the ONLY candidate: the socket answers there and
  nowhere else, so offering the machine's other interfaces hands the phone URLs
  that time out. It also decides WHICH address leads, which is what a network
  that allows exactly one of them needs — the allowed address has to be `url=`,
  not an `alt=` behind a tailnet address that network drops.

  A wildcard bind really does serve every interface, so there Tailscale
  addresses come first because they keep working off-LAN, then LAN, then the
  rest. Only addresses this machine actually holds qualify. Every candidate is
  a guess the client resolves by trying them in order, so an address nothing
  answers on - the router this machine routes through, say - costs a timeout
  and can even win the race against a working one (#277). Reaching the gateway
  from outside the LAN is a port forward plus `--advertise`, not a guess."
  [bind-host]
  (let [host
        (str bind-host)

        concrete
        (when-not (or (str/blank? host) (#{"0.0.0.0" "::" "127.0.0.1" "localhost"} host)) host)]

    (if concrete
      [concrete]
      (let [ips (iface-addresses)]
        (->> (concat (filter tailscale-ip? ips)
                     (filter site-local-ip? ips)
                     (remove #(or (tailscale-ip? %) (site-local-ip? %)) ips))
             (remove str/blank?)
             distinct
             vec)))))

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

(defn advertised-url
  "Normalize an operator-supplied address into the base URL clients should dial,
  or nil when there is nothing to advertise.

  The interface scan only sees the addresses this machine HOLDS, and a network
  can insist on one it does not: a port forward, a proxy, or a policy that drops
  every address but one. `--advertise` is that claim, so it takes the link's
  `url=` while the scanned addresses ride along as `alt=`.

  A bare host or `host:port` is completed with `http://` and the gateway's own
  port; anything carrying a scheme is taken verbatim, because only the operator
  knows the scheme and port on the other side of that route."
  [advertise port]
  (let [value (str/trim (str advertise))]
    (cond (str/blank? value) nil
          (str/includes? value "://") (str/replace value #"/+$" "")
          (re-matches #"[^:/]+:\d+" value) (str "http://" value)
          :else (str "http://" value ":" port))))

(defn pairing-url
  "The `vis://gateway` deep link. `url=` is the best guess (Tailscale first), and
  `alt=` carries the remaining routable hosts so a phone that cannot reach the
  first one (no Tailscale, different LAN) falls back instead of failing. A
  concrete bind has a single candidate, so its link carries no `alt=` at all.
  IPv4 link-local (169.254/16) is dropped from the alternates: no phone can
  route it, and every extra host makes the QR denser.

  `:advertise` pins `url=` to an address the scan cannot know and demotes every
  scanned host to `alt=`."
  [{:keys [host port token advertise]}]
  (let [hosts
        (let [c (candidate-hosts host)]
          (if (seq c) c [host]))

        ->url
        (fn [h]
          (str "http://" h ":" port))

        pinned
        (advertised-url advertise port)

        primary
        (or pinned (->url (first hosts)))

        alts
        (into
          []
          (comp (remove #(str/starts-with? (str %) "169.254.")) (map ->url) (remove #(= primary %)))
          (cond (contains? LOOPBACK_HOSTS (str host)) []
                pinned hosts
                :else (rest hosts)))]

    (str "vis://gateway?url="
         (url-encode primary)
         (when (seq alts) (str "&alt=" (url-encode (str/join "," alts))))
         (when-not (str/blank? (str token)) (str "&token=" (url-encode token))))))

(defn pairing-json
  [{:keys [host port token require-token? advertise]}]
  (let [url (or (advertised-url advertise port)
                (str "http://" (or (first (candidate-hosts host)) host) ":" port))]
    (wire/json-str
      (cond-> {:type "vis-gateway-pairing" :version 1 :url url :hosts (candidate-hosts host)}
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
  (let [hints
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
                    (let [top? (light? x y)
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
  (contains? LOOPBACK_HOSTS (str host)))

(defn print-pairing!
  "Emit the companion pairing block (title, reachable hosts, `vis://` URL, and a
   terminal QR). Each line goes through `emit` (default `println`); CLI callers
   whose stdout is redirected to the log file pass a real-terminal writer so the
   QR is actually visible. Returns the pairing URL payload.

   Bound to loopback there is nothing to pair with: the interface scan still
   finds Tailscale/LAN addresses, but the listener is not on them, so a QR built
   from those would encode a URL that times out — the failure landing on the
   phone, minutes later, looking like a broken app. Refuse and print the restart
   command instead; returns nil. An `:advertise` address overrides that refusal:
   the operator has named a route Vis cannot see, such as a proxy in front of
   the loopback port."
  [{:keys [require-token? emit host port advertise] :or {emit println} :as opts}]
  (if (and (loopback-bind? host) (str/blank? (str advertise)))
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
    (let [payload
          (pairing-url (cond-> opts
                         (not require-token?)
                         (dissoc :token)))

          pinned
          (advertised-url advertise port)

          hosts
          (if (loopback-bind? host) [] (candidate-hosts host))

          router
          (when (and port (seq hosts) (not pinned)) (default-route-host))]

      (emit "")
      (emit "VIS companion pairing")
      (emit
        "in the companion app open Machines → Add a machine, then scan this or paste the link below")
      (when pinned (emit (str "advertising: " pinned)))
      (when (seq hosts) (emit (str "reachable hosts: " (str/join ", " hosts))))
      (when router
        (emit (str "from outside this network: forward port " port " on your router (" router ")"))
        (emit "  then restart with --advertise <public host or domain> so the link points there"))
      (emit payload)
      (emit (terminal-qr payload))
      ;; Callers often park (the gateway daemon) right after this; `*out*` does
      ;; not autoflush, so an unflushed QR is an invisible QR.
      (flush)
      payload)))
