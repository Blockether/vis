(ns com.blockether.vis.internal.gateway.relay
  "Relayed push — how a gateway wakes a phone WITHOUT holding Apple's or
   Google's signing key.

   APNs binds a topic to the Apple team that owns it: a key minted by anyone
   else, aimed at someone else's bundle id, is refused forever (`403
   InvalidProviderToken` / `TopicDisallowed`). So a self-hosted gateway can
   never push to a companion built and signed by somebody else — unless the
   signing key stays on infrastructure the app's publisher runs, and the
   gateway is given a capability instead of a credential.

   That capability is a GRANT. The *device* asks the relay for one and hands
   it to this gateway during \"notify this device\"; the gateway POSTs
   `{grant, title, body}` and the relay signs and forwards. Consequences worth
   the indirection:

   * this gateway holds no `.p8`, no service-account JSON, nothing revocable
     only by breaking push for everyone else;
   * this gateway never learns the raw APNs/FCM device token, so a gateway you
     do not trust cannot fingerprint the device it notifies;
   * a grant expires by itself. Its expiry travels inside it, sealed, so the
     relay keeps no list of anybody and an abandoned gateway simply goes mute.

   The relay itself lives in `apps/vis-companion-relay` (a Cloudflare Worker).
   The DEVICE names it: which relay can sign for a build is a property of the
   BUILD, so the app mints its grant at the relay serving the app it is and
   posts `{grant, relay_url}` to `/v1/devices` — a gateway nobody configured
   still delivers. `VIS_PUSH_RELAY_URL`, or `~/.vis/relay.edn`
   `{:url \"https://push.example.com\"}`, is an operator override for devices
   that named nothing; the direct `gateway.push` / `gateway.fcm` credentials
   still work exactly as before."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.gateway.wire :as wire]
            [taoensso.telemere :as tel])
  (:import [java.io File]
           [java.net URI]
           [java.net.http HttpClient HttpRequest HttpRequest$BodyPublishers HttpResponse
            HttpResponse$BodyHandlers]
           [java.time Duration]))

(defn- vis-home
  ^File []
  (io/file (or (System/getProperty "vis.push.home")
               (System/getenv "VIS_HOME")
               (str (System/getProperty "user.home") File/separator ".vis"))))

(defn- env-val
  [k]
  (let [v (System/getenv k)]
    (when-not (str/blank? v) (str/trim v))))

(defn- side-config
  "Optional `~/.vis/relay.edn` — `{:url \"https://push.example.com\"}`."
  []
  (let [f (io/file (vis-home) "relay.edn")]
    (when (.isFile f) (try (edn/read-string (slurp f)) (catch Throwable _ nil)))))

(defn- loopback?
  "True when a URL cannot leave this machine, which is the one place cleartext
   costs nothing — `wrangler dev`, and the tests' own relay."
  [^String url]
  (try (contains? #{"127.0.0.1" "localhost" "::1" "[::1]" "0:0:0:0:0:0:0:1"}
                  (str/lower-case (str (.getHost (URI/create url)))))
       (catch Throwable _ false)))

(defn usable-url
  "The address a grant may be handed to, trimmed of trailing slashes — or nil.

   A grant is a BEARER capability and the alert carries the title and body of
   what just happened, so the address both are handed to is TLS or nothing:
   silently trusting cleartext would put a permanent right to push to that phone
   on the wire. Loopback is the exception; it never reaches a network."
  [url]
  (let
    [u (some-> url
               str
               str/trim
               (str/replace #"/+$" ""))]
    (when (and (not (str/blank? u))
               (or (str/starts-with? u "https://")
                   (and (str/starts-with? u "http://") (loopback? u))))
      u)))

(defn config
  "The relay this gateway names for EVERY device, or `:is-configured false`.
   Never throws. An operator override; most gateways need none, because a
   registered device carries the address of the relay that sealed its grant."
  []
  (let
    [side
     (or (side-config) {})

     raw
     (some-> (or (env-val "VIS_PUSH_RELAY_URL") (:url side))
             str
             str/trim
             (str/replace #"/+$" ""))

     usable
     (usable-url raw)]

    {:url (when-not (str/blank? raw) raw)
     :source (cond (env-val "VIS_PUSH_RELAY_URL") "env"
                   (:url side) "file"
                   :else nil)
     :is-insecure (boolean (and (not (str/blank? raw)) (nil? usable)))
     :is-configured (some? usable)}))

(defn configured?
  "True when a grant registered here can actually be delivered."
  []
  (:is-configured (config)))

(defn mask
  "A grant is a bearer capability. This is the ONLY form allowed into a log."
  [grant]
  (let [s (str grant)]
    (if (<= (count s) 12) "…" (str (subs s 0 6) "…" (subs s (- (count s) 4))))))

(defonce ^:private http-client
  (delay (-> (HttpClient/newBuilder)
             (.connectTimeout (Duration/ofSeconds 10))
             (.build))))

(defn- payload
  ^String [{:keys [title body data thread-id collapse-id badge]}]
  (wire/json-str (cond-> {:title title :body body :data (or data {})}
                   thread-id
                   (assoc :thread-id thread-id)

                   collapse-id
                   (assoc :collapse-id collapse-id)

                   badge
                   (assoc :badge badge))))

(def ^:private RETRY-DELAY-MS 300)

(defn- transient-verdict?
  "Verdicts that prove the relay never reached a provider: a transport failure,
   or the relay itself reporting an upstream that stumbled. Nothing was
   delivered, so asking again cannot duplicate a notification — and not asking
   loses it for good."
  [{:keys [status]}]
  (contains? #{0 502 503 504} status))

(defn- post-once
  [url grant notification]
  (try (let
         [^HttpResponse resp
          (.send ^HttpClient @http-client
                 (-> (HttpRequest/newBuilder (URI/create (str url "/v1/push")))
                     (.header "authorization" (str "Bearer " grant))
                     (.header "content-type" "application/json")
                     (.timeout (Duration/ofSeconds 15))
                     (.POST (HttpRequest$BodyPublishers/ofString (payload notification)))
                     (.build))
                 (HttpResponse$BodyHandlers/ofString))

          status
          (.statusCode resp)

          parsed
          (wire/parse-json (.body resp))

          reason
          (str (or (get parsed "reason") (get-in parsed ["error" "code"]) ""))]

         {:status status :reason reason})
       (catch Throwable t {:status 0 :reason (or (ex-message t) "transport-error")})))

(defn send!
  "Ask `relay-url` to deliver one alert to the device this grant names. Returns
   `{:status int :reason str}` — status 0 for a transport failure, so this never
   throws. A stumble is tried once more; the relay answers 404/410 once the
   grant is gone, which is the caller's cue to forget the device.

   The address is an argument, not a global: a grant is sealed by ONE relay, so
   it is only ever spendable at the one the device named when it registered."
  [relay-url grant notification]
  (let [url (usable-url relay-url)]
    (cond (str/blank? (str relay-url)) {:status 0 :reason "not-configured"}
          (nil? url) {:status 0 :reason "insecure-relay-url"}
          :else
          (let
            [first-try (post-once url grant notification)
             result (if-not (transient-verdict? first-try)
                      first-try
                      (do (Thread/sleep ^long RETRY-DELAY-MS) (post-once url grant notification)))]

            (when (not= 200 (:status result))
              (tel/log! {:level :warn
                         :id ::relay-push-failed
                         :data
                         {:grant (mask grant) :status (:status result) :reason (:reason result)}}))
            result))))

(defn dead-grant?
  "True when the relay's verdict means this grant will never deliver again —
   revoked by its owner, or dropped because the device unregistered."
  [{:keys [status]}]
  (contains? #{404 410} status))

(defn status
  "Relay half of the push capability. Carries the URL, never the grants."
  []
  (let [cfg (config)]
    {:is-available (:is-configured cfg)
     :url (:url cfg)
     :source (:source cfg)
     :is-insecure (:is-insecure cfg)}))
