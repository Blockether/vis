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
   Configure a gateway with `VIS_PUSH_RELAY_URL`, or `~/.vis/relay.edn`
   `{:url \"https://push.example.com\"}`. Unset = no relay; the direct
   `gateway.push` / `gateway.fcm` credentials still work exactly as before."
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

(defn config
  "Where this gateway relays pushes, or `:is-configured false`. Never throws."
  []
  (let
    [side
     (or (side-config) {})

     url
     (some-> (or (env-val "VIS_PUSH_RELAY_URL") (:url side))
             str
             str/trim
             (str/replace #"/+$" ""))]

    {:url (when-not (str/blank? url) url)
     :source (cond (env-val "VIS_PUSH_RELAY_URL") "env"
                   (:url side) "file"
                   :else nil)
     :is-configured (boolean (and (not (str/blank? url)) (str/starts-with? url "http")))}))

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

(defn send!
  "Ask the relay to deliver one alert to the device this grant names. Returns
   `{:status int :reason str}` — status 0 for a transport failure, so this
   never throws. The relay answers 404/410 once the grant is gone, which is the
   caller's cue to forget the device."
  [grant notification]
  (let [cfg (config)]
    (if-not (:is-configured cfg)
      {:status 0 :reason "not-configured"}
      (try (let
             [^HttpResponse resp
              (.send ^HttpClient @http-client
                     (-> (HttpRequest/newBuilder (URI/create (str (:url cfg) "/v1/push")))
                         (.header "authorization" (str "Bearer " grant))
                         (.header "content-type" "application/json")
                         (.timeout (Duration/ofSeconds 15))
                         (.POST (HttpRequest$BodyPublishers/ofString (payload notification)))
                         (.build))
                     (HttpResponse$BodyHandlers/ofString))
              status (.statusCode resp)
              parsed (wire/parse-json (.body resp))
              reason (str (or (get parsed "reason") (get-in parsed ["error" "code"]) ""))]

             (when (not= 200 status)
               (tel/log! {:level :warn
                          :id ::relay-push-failed
                          :data {:grant (mask grant) :status status :reason reason}}))
             {:status status :reason reason})
           (catch Throwable t {:status 0 :reason (or (ex-message t) "transport-error")})))))

(defn dead-grant?
  "True when the relay's verdict means this grant will never deliver again —
   revoked by its owner, or dropped because the device unregistered."
  [{:keys [status]}]
  (contains? #{404 410} status))

(defn status
  "Relay half of the push capability. Carries the URL, never the grants."
  []
  (let [cfg (config)]
    {:is-available (:is-configured cfg) :url (:url cfg) :source (:source cfg)}))
