(ns com.blockether.vis.internal.gateway.push
  "Native push notifications (Apple Push Notification service).

   ONE job: when a turn finishes on this gateway, wake the phone that asked
   to be woken. Everything here is server-side; the app only ever hands us a
   device token.

   Three moving parts:

   1. **Credentials.** A token-based APNs auth key (`.p8`, ES256) plus its key
      id, the Apple team id and the app's bundle id (the APNs *topic*).
      Resolved from `VIS_APNS_KEY_PATH` / `VIS_APNS_KEY_ID` / `VIS_APNS_TEAM_ID`
      / `VIS_APNS_TOPIC`, else auto-discovered from `~/.vis/apns/AuthKey_<kid>.p8`
      (the key id is read off the filename) with the team/topic still from env
      or `~/.vis/apns/apns.edn`. No credentials = push silently OFF; the gateway
      keeps working exactly as before.

   2. **A device registry** at `~/.vis/devices.edn` — device token -> platform,
      APNs environment, client label/version, timestamps. Registration is
      idempotent on the token. Tokens are SECRETS: nothing here logs more than
      a masked prefix.

   3. **A sender** — a signed ES256 JWT (cached, refreshed well inside Apple's
      one-hour window) over HTTP/2 to `api.push.apple.com`. A device that
      registered with the wrong environment is retried once against the other
      host, and an APNs `BadDeviceToken`/`Unregistered` verdict evicts the
      device so a stale token cannot accumulate.

   The wire surface lives in `gateway.server` (`/v1/devices`); this namespace
   knows nothing about Ring."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [com.blockether.vis.internal.gateway.fcm :as fcm]
            [com.blockether.vis.internal.gateway.web-push :as web-push]
            [com.blockether.vis.internal.gateway.relay :as relay]
            [clojure.java.shell :as sh]
            [clojure.string :as str]
            [com.blockether.vis.internal.gateway.wire :as wire]
            [taoensso.telemere :as tel])
  (:import [java.io File]
           [java.net URI]
           [java.net.http HttpClient HttpClient$Version HttpRequest HttpRequest$BodyPublishers
            HttpResponse HttpResponse$BodyHandlers]
           [java.nio.charset StandardCharsets]
           [java.security KeyFactory Signature]
           [java.security.spec PKCS8EncodedKeySpec]
           [java.time Duration]
           [java.util Base64]))

(def ^:private PROD_HOST "https://api.push.apple.com")

(def ^:private SANDBOX_HOST "https://api.sandbox.push.apple.com")

(def ^:private ^:const JWT_TTL_MS
  "Refresh the APNs JWT well inside Apple's 60-minute maximum; a token older
   than that is rejected with `ExpiredProviderToken`."
  (* 45 60 1000))

(def ^:private ^:const MAX_DEVICES
  "Hard cap on the registry so a misbehaving client cannot grow the file
   without bound. Oldest-seen devices are evicted first."
  200)

;; =============================================================================
;; Paths + credentials
;; =============================================================================

(defn- vis-home
  ;; `vis.push.home` redirects the whole push home (registry + APNs key dir) so a
  ;; test never touches the real `~/.vis`.
  ^File []
  (if-let [override (System/getProperty "vis.push.home")]
    (io/file override)
    (io/file (System/getProperty "user.home") ".vis")))

(defn devices-file
  "Where the device registry is persisted."
  ^File []
  (io/file (vis-home) "devices.edn"))

(defn- apns-dir ^File [] (io/file (vis-home) "apns"))

(defn- env-val
  [k]
  (let [v (System/getenv k)]
    (when-not (str/blank? v) (str/trim v))))

(defn- unhex
  "`security -w` prints hex, not text, whenever the stored password is not plain
   printable ASCII — which a multi-line PEM never is. Decode that back."
  [s]
  (if (and (even? (count s)) (re-matches #"(?i)[0-9a-f]{32,}" s))
    (String. (byte-array (map #(unchecked-byte (Integer/parseInt (apply str %) 16))
                              (partition 2 s)))
             StandardCharsets/UTF_8)
    s))

(defn- keychain
  "Generic password stored under service `vis-apns`, account `account`, in the
   macOS login keychain — or nil anywhere else. Read on demand rather than
   cached, so locking the keychain revokes access immediately and the secret
   never sits in a world-readable file."
  [account]
  (when (and (str/includes? (str/lower-case (str (System/getProperty "os.name"))) "mac")
             ;; A redirected push home means a test fixture: never let the
             ;; developer's real keychain leak into it.
             (nil? (System/getProperty "vis.push.home")))
    (try (let
           [{:keys [exit out]}
            (sh/sh "security" "find-generic-password" "-s" "vis-apns" "-a" account "-w")]
           (when (and (= 0 (long exit)) (not (str/blank? out))) (unhex (str/trim out))))
         (catch Throwable _ nil))))

(defn- discovered-key
  "First `AuthKey_<kid>.p8` under `~/.vis/apns/`, as `{:key-path :key-id}`.
   Apple names the download that way, so the key id needs no extra config."
  []
  (let
    [f (->> (.listFiles (apns-dir))
            seq
            (filter #(and (.isFile ^File %) (str/ends-with? (.getName ^File %) ".p8")))
            (sort-by #(.getName ^File %))
            first)]
    (when f
      (let
        [n (.getName ^File f)
         kid (second (re-matches #"(?i)AuthKey_(.+)\.p8" n))]

        {:key-path (.getAbsolutePath ^File f) :key-id kid}))))

(defn- side-config
  "Optional `~/.vis/apns/apns.edn` — `{:team-id \"…\" :topic \"…\" :key-id \"…\"}`.
   Env always wins over it."
  []
  (let [f (io/file (apns-dir) "apns.edn")]
    (when (.isFile f) (try (edn/read-string (slurp f)) (catch Throwable _ nil)))))

(defn config
  "Resolved APNs credentials, or a map with `:is-configured false` and the
   list of what is still missing. Never throws and never returns key MATERIAL."
  []
  (let
    [side
     (or (side-config) {})

     disc
     (or (discovered-key) {})

     ;; The key itself: keychain material beats any file on disk.
     kc-key
     (some? (keychain "key"))

     key-path
     (or (env-val "VIS_APNS_KEY_PATH") (:key-path side) (:key-path disc))

     key-id
     (or (env-val "VIS_APNS_KEY_ID") (keychain "key_id") (:key-id side) (:key-id disc))

     team-id
     (or (env-val "VIS_APNS_TEAM_ID") (keychain "team_id") (:team-id side))

     topic
     (or (env-val "VIS_APNS_TOPIC") (keychain "topic") (:topic side))

     default-env
     (or (env-val "VIS_APNS_ENV") (keychain "environment") (:environment side) "production")

     missing
     (cond-> []
       (and (not kc-key) (or (str/blank? (str key-path)) (not (.isFile (io/file (str key-path))))))
       (conj "key")

       (str/blank? (str key-id))
       (conj "key_id")

       (str/blank? (str team-id))
       (conj "team_id")

       (str/blank? (str topic))
       (conj "topic"))]

    {:key-path (when-not kc-key key-path)
     :key-source (if kc-key "keychain" "file")
     :key-id key-id
     :team-id team-id
     :topic topic
     :default-environment (if (= "sandbox" default-env) "sandbox" "production")
     :missing missing
     :is-configured (empty? missing)}))

(defn configured? "True when this gateway can deliver an APPLE push." [] (:is-configured (config)))


;; =============================================================================
;; ES256 provider token (JWT)
;; =============================================================================

(defn- b64url ^String [^bytes b] (.encodeToString (.withoutPadding (Base64/getUrlEncoder)) b))

(defn- utf8 ^bytes [^String s] (.getBytes s StandardCharsets/UTF_8))

(defn- private-key
  "Parse a PKCS#8 PEM (`.p8`, what Apple hands out) into an EC private key."
  [^String pem]
  (let
    [body
     (-> pem
         (str/replace #"-----(BEGIN|END)[^-]+-----" "")
         (str/replace #"\s" ""))

     der
     (.decode (Base64/getDecoder) body)]

    (.generatePrivate (KeyFactory/getInstance "EC") (PKCS8EncodedKeySpec. der))))

(defn- unsigned-int
  "Left-pad/trim one DER INTEGER to exactly 32 bytes of the P-256 field."
  ^bytes [^bytes der ^long off ^long len]
  (let
    [out
     (byte-array 32)

     src-off
     (if (> len 32) (+ off (- len 32)) off)

     n
     (min len 32)]

    (System/arraycopy der src-off out (- 32 n) n)
    out))

(defn- der->jose
  "ECDSA DER `SEQUENCE{INTEGER r, INTEGER s}` -> the raw 64-byte `r||s` JOSE
   signature. `Signature` emits DER; JWS ES256 demands the concatenation."
  ^bytes [^bytes der]
  (let
    [r-len
     (long (aget der 3))

     r
     (unsigned-int der 4 r-len)

     s-off
     (+ 4 r-len 2)

     s-len
     (long (aget der (+ 4 r-len 1)))

     s
     (unsigned-int der s-off s-len)

     out
     (byte-array 64)]

    (System/arraycopy r 0 out 0 32)
    (System/arraycopy s 0 out 32 32)
    out))

(defn- sign-jwt
  [{:keys [key-path key-source key-id team-id]}]
  (let
    [header
     (b64url (utf8 (wire/json-str {:alg "ES256" :kid key-id})))

     claims
     (b64url (utf8 (wire/json-str {:iss team-id :iat (quot (System/currentTimeMillis) 1000)})))

     signing-input
     (str header "." claims)

     sig
     (doto (Signature/getInstance "SHA256withECDSA")
       (.initSign (private-key (if (= "keychain" key-source) (keychain "key") (slurp key-path))))
       (.update (utf8 signing-input)))]

    (str signing-input "." (b64url (der->jose (.sign sig))))))

(defonce ^:private jwt-cache
  ;; {:token "…" :at ms :key-id "…"} — one provider token per key, reused
  ;; across every notification. Apple rate-limits token MINTING, not use.
  (atom nil))

(defn- provider-token
  [cfg]
  (let
    [{:keys [token at key-id]}
     @jwt-cache

     fresh?
     (and token
          (= key-id (:key-id cfg))
          (< (- (System/currentTimeMillis) (long at)) (long JWT_TTL_MS)))]

    (if fresh?
      token
      (let [t (sign-jwt cfg)]
        (reset! jwt-cache {:token t :at (System/currentTimeMillis) :key-id (:key-id cfg)})
        t))))

;; =============================================================================
;; Device registry
;; =============================================================================

(defn mask
  "A device token is a secret. This is the ONLY form allowed into a log."
  [token]
  (let [s (str token)]
    (if (<= (count s) 12) "…" (str (subs s 0 6) "…" (subs s (- (count s) 4))))))

(defonce ^:private devices
  ;; token -> device map. nil = not yet loaded from disk.
  (atom nil))

(defn- read-devices
  []
  (let [f (devices-file)]
    (if (.isFile f) (try (or (edn/read-string (slurp f)) {}) (catch Throwable _ {})) {})))

(defn- write-devices!
  [m]
  (try (io/make-parents (devices-file))
       (spit (devices-file) (pr-str m))
       (catch Throwable t
         (tel/log! {:level :warn :id ::devices-write-failed :data {:error (ex-message t)}}))))

(defn- ensure-loaded! [] (or @devices (reset! devices (read-devices))))

(defn reload-devices!
  "Drop the in-memory cache and re-read the registry from disk."
  []
  (reset! devices (read-devices)))

(defn- prune
  "Keep the registry bounded, newest-seen first."
  [m]
  (if (<= (long (count m)) (long MAX_DEVICES))
    m
    (into {} (take MAX_DEVICES (sort-by #(- (long (:last-seen (val %) 0))) m)))))

(defn public-device
  "One device as the wire may see it. The raw device token and the relay grant
   are both SECRETS: neither ever leaves this process."
  [device]
  (-> device
      (dissoc :token :grant :id)
      (assoc :token_preview (mask (or (:token device) (:grant device)))
             :is-relayed (some? (:grant device)))))

(defn list-devices
  "Every registered device, secrets MASKED — safe for an HTTP response."
  []
  (->> (vals (ensure-loaded!))
       (sort-by #(- (long (:last-seen % 0))))
       (mapv public-device)))

(defn device-count ^long [] (count (ensure-loaded!)))

(defn any-configured?
  "True when this gateway can deliver a push to SOME platform."
  []
  (boolean (or (configured?) (fcm/configured?) (web-push/configured?) (relay/configured?))))

(defn- not-blank
  [s]
  (let
    [v (some-> s
               str
               str/trim)]
    (when-not (str/blank? v) v)))

(defn register-device!
  "Idempotently register (or refresh) one device. A device identifies itself
   either by a raw APNs/FCM token this gateway pushes to directly, or by a relay
   GRANT (`gateway.relay`) that lets it be woken WITHOUT this gateway ever
   learning its token. Returns the stored device, or nil when neither is usable.

   A grant is sealed by ONE relay, so the device also names where to spend it;
   an address that is not TLS is dropped here rather than at send time. The
   address sticks across a refresh that omits it — losing it would silence a
   device that is still perfectly reachable."
  [{:keys [token grant platform environment client client-version label bundle-id relay-url]}]
  (let
    [token
     (not-blank token)

     grant
     (not-blank grant)

     id
     (or token grant)]

    (when id
      (let
        [now
         (System/currentTimeMillis)

         existing
         (get (ensure-loaded!) id)

         device
         (merge {:registered-at now}
                existing
                {:id id
                 :token token
                 :grant grant
                 :relay-url (or (relay/usable-url relay-url) (:relay-url existing))
                 :platform (or platform "ios")
                 :environment (if (= "sandbox" environment) "sandbox" "production")
                 :client (or client "vis-companion")
                 :client-version client-version
                 :label label
                 :bundle-id bundle-id
                 :last-seen now})]

        (swap! devices #(prune (assoc (or % {}) id device)))
        (write-devices! @devices)
        (tel/log! {:level :info
                   :id ::device-registered
                   :data {:token (mask id)
                          :platform (:platform device)
                          :env (:environment device)
                          :is-relayed (some? grant)
                          :relay (:relay-url device)}})
        device))))

(defn unregister-device!
  "Drop one device token. Returns true when it was present."
  [token]
  (let [present? (contains? (ensure-loaded!) token)]
    (when present?
      (swap! devices dissoc token)
      (write-devices! @devices)
      (tel/log! {:level :info :id ::device-unregistered :data {:token (mask token)}}))
    present?))

;; =============================================================================
;; Sending
;; =============================================================================

(defonce ^:private http-client
  (delay (-> (HttpClient/newBuilder)
             (.version HttpClient$Version/HTTP_2)
             (.connectTimeout (Duration/ofSeconds 10))
             (.build))))

(defn- host-for [environment] (if (= "sandbox" environment) SANDBOX_HOST PROD_HOST))

(defn- post-apns
  "One HTTP/2 POST to APNs. Returns `{:status int :reason str}`; a transport
   failure is reported as status 0 so a caller never sees an exception."
  [cfg environment token ^String payload {:keys [collapse-id]}]
  (try
    (let
      [req
       (cond-> (HttpRequest/newBuilder (URI/create (str (host-for environment) "/3/device/" token)))
         :always
         (.header "authorization" (str "bearer " (provider-token cfg)))

         :always
         (.header "apns-topic" (:topic cfg))

         :always
         (.header "apns-push-type" "alert")

         :always
         (.header "apns-priority" "10")

         :always
         (.header "content-type" "application/json")

         collapse-id
         (.header "apns-collapse-id" (subs (str collapse-id) 0 (min 64 (count (str collapse-id)))))

         :always
         (.timeout (Duration/ofSeconds 15))

         :always
         (.POST (HttpRequest$BodyPublishers/ofString payload)))

       ^HttpResponse resp
       (.send ^HttpClient @http-client (.build req) (HttpResponse$BodyHandlers/ofString))

       status
       (.statusCode resp)

       reason
       (or (some-> (wire/parse-json (.body resp))
                   (get "reason"))
           "")]

      {:status status :reason reason})
    (catch Throwable t {:status 0 :reason (or (ex-message t) "transport-error")})))

(defn- alert-payload
  ;; APNs spells the `aps` keys in kebab-case (`thread-id`, `interruption-level`)
  ;; and silently ignores anything else, so these MUST be literal strings: the
  ;; wire encoder mechanically snake_cases keywords, which would ship
  ;; `thread_id` and cost us notification grouping without any error.
  ^String [{:keys [title body data thread-id]}]
  (wire/json-str
    (merge {:aps (cond->
                   {:alert {:title title :body body} :sound "default" "interruption-level" "active"}
                   thread-id
                   (assoc "thread-id" thread-id))}
           data)))

(defn send-to-device!
  "Deliver one alert to one registered device. A device that handed us a relay
   grant is delivered through the relay it named — that relay holds the signing
   key so this gateway does not have to — and everything else goes straight to
   APNs/FCM with this gateway's own credentials. Retries once against the other
   APNs environment (a TestFlight build registered as `sandbox`, or the reverse,
   is the single most common misconfiguration) and forgets the device when the
   provider says it is gone. Returns `{:is-delivered bool :status :reason}`."
  [device notification]
  (let
    [platform
     (or (:platform device) "ios")

     token
     (:token device)

     grant
     (:grant device)

     ;; The device's own relay first: it is the one that sealed this grant. The
     ;; configured relay is the operator override, for a device that named none.
     relay-url
     (or (:relay-url device) (:url (relay/config)))

     id
     (or (:id device) token grant)

     result
     (cond
       ;; A grant wins over any local credential: it is the only path that also
       ;; works when this gateway is not the one that signed the app.
       (and grant (relay/usable-url relay-url)) (let [r (relay/send! relay-url grant notification)]
                                                  (when (relay/dead-grant? r)
                                                    (unregister-device! id))
                                                  r)
       (= "web" platform) (let [r (web-push/send! token notification)]
                            (when (contains? #{404 410} (:status r)) (unregister-device! id))
                            r)
       (str/blank? (str token)) {:status 0 :reason "relay-not-configured"}
       (= "android" platform) (let [r (fcm/send! token notification)]
                                (when (fcm/dead-token? r) (unregister-device! id))
                                r)
       ;; Anything else (a browser, some future platform) is stored so the app
       ;; can see itself registered, but never handed to a provider.
       (not (contains? #{"ios" "ipados"} platform)) {:status 0 :reason "unsupported-platform"}
       :else (let [cfg (config)]
               (if-not (:is-configured cfg)
                 {:status 0 :reason "not-configured"}
                 (let
                   [payload (alert-payload notification)
                    env (or (:environment device) (:default-environment cfg))
                    attempt (post-apns cfg env token payload notification)
                    other (when (contains? #{"BadDeviceToken" "BadEnvironmentKeyInToken"}
                                           (:reason attempt))
                            (post-apns cfg
                                       (if (= "sandbox" env) "production" "sandbox")
                                       token
                                       payload
                                       notification))
                    result (or (when (= 200 (:status other)) other) other attempt)]

                   (when (and other (= 200 (:status other)))
                     (swap! devices assoc-in
                       [id :environment]
                       (if (= "sandbox" env) "production" "sandbox"))
                     (write-devices! @devices))
                   (when (contains? #{"BadDeviceToken" "Unregistered" "DeviceTokenNotForTopic"}
                                    (:reason result))
                     (unregister-device! id))
                   result))))]

    (when (not= 200 (:status result))
      (tel/log!
        {:level :warn
         :id ::push-failed
         :data
         {:token (mask id) :platform platform :status (:status result) :reason (:reason result)}}))
    (assoc result :is-delivered (<= 200 (long (:status result)) 299))))

(defn broadcast!
  "Send one notification to every registered device. Returns a per-device
   summary; never throws."
  [notification]
  (let [ds (vals (ensure-loaded!))]
    (mapv (fn [d]
            (merge {:token_preview (mask (:token d))}
                   (select-keys (send-to-device! d notification) [:is-delivered :status :reason])))
          ds)))

;; =============================================================================
;; The turn-finished trigger
;; =============================================================================

(defonce ^:private describe-session
  ;; [sid tid] -> {:title … :answer …} — injected by the server so this ns stays
  ;; free of any dependency on the session registry.
  (atom (fn [_sid _tid]
          nil)))

(defn set-session-describer!
  "Install the fn that turns a session id + turn id into `{:title … :answer …}`
   for the alert. `:answer` is the finished turn's own text."
  [f]
  (reset! describe-session (or f
                               (fn [_ _]
                                 nil))))

(defonce ^:private gateway-id
  ;; This gateway's own instance id — injected by the server, same value
  ;; `/healthz` reports. A phone can be paired with several machines and a
  ;; session id only means anything on the gateway that minted it, so every
  ;; alert has to say which one it came from.
  (atom nil))

(defn set-gateway-id!
  "Install this gateway's stable instance id; every alert carries it as
   `gateway_id` so a tap opens the session on the gateway that sent it."
  [id]
  (reset! gateway-id (not-empty (str id))))

(defn- with-gateway
  "Stamp a notification payload with the sending gateway, when known."
  [data]
  (if-let [gid @gateway-id]
    (assoc data :gateway_id gid)
    data))

(def ^:private BODY_LIMIT
  ;; iOS shows ~2 lines on the lock screen and ~4 expanded; past this the tail is
  ;; never read, and a huge alert payload only risks APNs' 4KB limit.
  180)

(defn- clip
  [^String s ^long limit]
  (if (<= (count s) limit)
    s
    (let
      [cut
       (subs s 0 limit)

       sp
       (.lastIndexOf cut " ")]

      (str (str/trimr (if (> sp (quot limit 2)) (subs cut 0 sp) cut)) "…"))))

(defn- answer-body
  "The answer, flattened to one banner-safe line.

   A notification exists to tell you WHAT vis said. Markdown is written for a
   renderer, not a lock screen, so the syntax is stripped rather than shown:
   fenced code becomes a marker (it is unreadable at this width and would eat
   the whole budget), links keep their label, emphasis/heading/bullet markers
   go. nil when nothing readable survives — the caller then falls back to the
   status line."
  [text]
  (some-> text
          str
          (str/replace #"(?s)```.*?```" " [code] ")
          (str/replace #"`([^`]*)`" "$1")
          (str/replace #"!?\[([^\]]*)\]\([^)]*\)" "$1")
          (str/replace #"(?m)^\s{0,3}#{1,6}\s*" "")
          (str/replace #"(?m)^\s{0,3}>\s?" "")
          (str/replace #"(?m)^\s{0,3}[-*+]\s+" "• ")
          (str/replace #"\*\*([^*]+)\*\*" "$1")
          (str/replace #"(?<!\w)[*_]([^*_\n]+)[*_](?!\w)" "$1")
          (str/replace #"\s+" " ")
          str/trim
          not-empty
          (clip BODY_LIMIT)))

(defn- turn-notification
  [sid event]
  (let
    [status
     (or (get event "status") (when (= "turn.failed" (get event "type")) "failed") "completed")

     described
     (@describe-session sid (get event "turn_id"))

     title
     (or (not-empty (str (:title described))) "Vis")]

    {:title title
     :body (or (answer-body (:answer described))
               (if (= "failed" status) "Turn failed." "Turn finished."))
     :thread-id (str sid)
     :collapse-id (str sid)
     :data
     (with-gateway
       {:session_id (str sid) :turn_id (get event "turn_id") :status status :type "turn.end"})}))

(defn- human-input-notification
  "Alert for a run BLOCKED on a human. The only push that says \"vis is waiting
   for YOU\": a turn parked on an unanswered dialog produces no terminal event,
   so without this the phone stays silent until the request times out."
  [sid event]
  (let
    [request
     (get event "request")

     asked
     (or (not-empty (str (get request "title"))) "Input needed")

     description
     (not-empty (str (get request "description")))

     described
     (@describe-session sid nil)]

    {:title (or (not-empty (str (:title described))) "Vis")
     :body (clip (str "Waiting for your input \u2014 "
                      (if description (str asked ": " description) asked))
                 BODY_LIMIT)
     :thread-id (str sid)
     ;; Its own collapse lane: an input request must not be swallowed by the
     ;; session's last turn banner.
     :collapse-id (str sid ":human-input")
     :data (with-gateway {:session_id (str sid)
                          :request_id (str (get request "id"))
                          :type "human_input.request"})}))

(defn on-event!
  "Event tap: push exactly on a terminal turn event or on a human-input request
   the run is now blocked on, and only when push is both configured and wanted
   by at least one device. Cheap and silent otherwise — this runs on EVERY
   gateway event."
  [sid event]
  (try (when-let [kind (#{"turn.completed" "turn.failed" "human_input.request"} (get event "type"))]
         (when (and (pos? (device-count)) (any-configured?))
           (let
             [n (if (= "human_input.request" kind)
                  (human-input-notification sid event)
                  (turn-notification sid event))]
             (future (broadcast! n)))))
       (catch Throwable t
         (tel/log! {:level :warn :id ::push-tap-failed :data {:error (ex-message t)}})))
  nil)

(defn status
  "Push capability for `/v1/capabilities` and `/v1/admin/status`."
  []
  (let
    [cfg
     (config)

     f
     (fcm/config)

     w
     (web-push/config)

     r
     (relay/status)

     is-available
     (or (:is-configured cfg) (:is-configured f) (:is-configured w) (:is-available r))]

    {:is-available is-available
     :provider (cond (and (:is-configured cfg) (:is-configured f)) "apns+fcm"
                     (:is-configured f) "fcm"
                     (:is-configured w) "web"
                     (and (:is-available r) (not (:is-configured cfg))) "relay"
                     :else "apns")
     :environment (:default-environment cfg)
     :topic (:topic cfg)
     :missing (:missing cfg)
     :apns {:is-available (:is-configured cfg)
            :environment (:default-environment cfg)
            :topic (:topic cfg)
            :key-source (:key-source cfg)
            :missing (:missing cfg)}
     :fcm {:is-available (:is-configured f)
           :project-id (:project-id f)
           :source (:source f)
           :missing (:missing f)}
     :web-push {:is-available (:is-configured w)
                :application-server-key (:application-server-key w)
                :subject (:subject w)
                :source (:source w)
                :missing (:missing w)}
     :relay r
     :devices (device-count)}))
