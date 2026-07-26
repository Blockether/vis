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

     key-path
     (or (env-val "VIS_APNS_KEY_PATH") (:key-path side) (:key-path disc))

     key-id
     (or (env-val "VIS_APNS_KEY_ID") (:key-id side) (:key-id disc))

     team-id
     (or (env-val "VIS_APNS_TEAM_ID") (:team-id side))

     topic
     (or (env-val "VIS_APNS_TOPIC") (:topic side))

     default-env
     (or (env-val "VIS_APNS_ENV") (:environment side) "production")

     missing
     (cond-> []
       (or (str/blank? (str key-path)) (not (.isFile (io/file (str key-path)))))
       (conj "key")

       (str/blank? (str key-id))
       (conj "key_id")

       (str/blank? (str team-id))
       (conj "team_id")

       (str/blank? (str topic))
       (conj "topic"))]

    {:key-path key-path
     :key-id key-id
     :team-id team-id
     :topic topic
     :default-environment (if (= "sandbox" default-env) "sandbox" "production")
     :missing missing
     :is-configured (empty? missing)}))

(defn configured?
  "True when this gateway can actually deliver a push."
  []
  (:is-configured (config)))

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
  [{:keys [key-path key-id team-id]}]
  (let
    [header
     (b64url (utf8 (wire/json-str {:alg "ES256" :kid key-id})))

     claims
     (b64url (utf8 (wire/json-str {:iss team-id :iat (quot (System/currentTimeMillis) 1000)})))

     signing-input
     (str header "." claims)

     sig
     (doto (Signature/getInstance "SHA256withECDSA")
       (.initSign (private-key (slurp key-path)))
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
     (and token (= key-id (:key-id cfg)) (< (- (System/currentTimeMillis) (long at)) JWT_TTL_MS))]

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
  (if (<= (count m) MAX_DEVICES)
    m
    (into {} (take MAX_DEVICES (sort-by #(- (long (:last-seen (val %) 0))) m)))))

(defn list-devices
  "Every registered device, tokens MASKED — safe for an HTTP response."
  []
  (->> (vals (ensure-loaded!))
       (sort-by #(- (long (:last-seen % 0))))
       (mapv (fn [d]
               (-> d
                   (dissoc :token)
                   (assoc :token_preview (mask (:token d))))))))

(defn device-count ^long [] (count (ensure-loaded!)))

(defn register-device!
  "Idempotently register (or refresh) one device token. Returns the stored
   device, or nil when the token is unusable."
  [{:keys [token platform environment client client-version label bundle-id]}]
  (let
    [token (some-> token
                   str
                   str/trim)]
    (when-not (str/blank? token)
      (let
        [now (System/currentTimeMillis)
         existing (get (ensure-loaded!) token)
         device (merge {:registered-at now}
                       existing
                       {:token token
                        :platform (or platform "ios")
                        :environment (if (= "sandbox" environment) "sandbox" "production")
                        :client (or client "vis-companion")
                        :client-version client-version
                        :label label
                        :bundle-id bundle-id
                        :last-seen now})]

        (swap! devices #(prune (assoc (or % {}) token device)))
        (write-devices! @devices)
        (tel/log! {:level :info
                   :id ::device-registered
                   :data
                   {:token (mask token) :platform (:platform device) :env (:environment device)}})
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
  ^String [{:keys [title body data thread-id]}]
  (wire/json-str
    (merge {:aps (cond->
                   {:alert {:title title :body body} :sound "default" :interruption-level "active"}
                   thread-id
                   (assoc :thread-id thread-id))}
           data)))

(defn send-to-device!
  "Deliver one alert to one registered device. Retries once against the other
   APNs environment (a TestFlight build registered as `sandbox`, or the
   reverse, is the single most common misconfiguration) and evicts the device
   when Apple says the token is dead. Returns `{:is-delivered bool :status
   :reason}`."
  [device notification]
  (let [cfg (config)]
    (if-not (:is-configured cfg)
      {:is-delivered false :status 0 :reason "not-configured"}
      (let
        [payload (alert-payload notification)
         token (:token device)
         env (or (:environment device) (:default-environment cfg))
         attempt (post-apns cfg env token payload notification)
         other
         (when (contains? #{"BadDeviceToken" "BadEnvironmentKeyInToken"} (:reason attempt))
           (post-apns cfg (if (= "sandbox" env) "production" "sandbox") token payload notification))
         result (or (when (= 200 (:status other)) other) other attempt)]

        (when (and other (= 200 (:status other)))
          (swap! devices assoc-in
            [token :environment]
            (if (= "sandbox" env) "production" "sandbox"))
          (write-devices! @devices))
        (when (contains? #{"BadDeviceToken" "Unregistered" "DeviceTokenNotForTopic"}
                         (:reason result))
          (unregister-device! token))
        (when (not= 200 (:status result))
          (tel/log! {:level :warn
                     :id ::push-failed
                     :data
                     {:token (mask token) :status (:status result) :reason (:reason result)}}))
        (assoc result :is-delivered (= 200 (:status result)))))))

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
  ;; sid -> {:title …} — injected by the server so this ns stays free of any
  ;; dependency on the session registry.
  (atom (fn [_sid]
          nil)))

(defn set-session-describer!
  "Install the fn that turns a session id into `{:title …}` for the alert."
  [f]
  (reset! describe-session (or f
                               (fn [_]
                                 nil))))

(defn- turn-notification
  [sid event]
  (let
    [status
     (or (get event "status") (when (= "turn.failed" (get event "type")) "failed") "completed")

     title
     (or (not-empty (str (:title (@describe-session sid)))) "Vis")]

    {:title title
     :body (if (= "failed" status) "Turn failed." "Turn finished.")
     :thread-id (str sid)
     :collapse-id (str sid)
     :data {:session_id (str sid) :turn_id (get event "turn_id") :status status :type "turn.end"}}))

(defn on-event!
  "Event tap: push exactly on a terminal turn event, and only when push is
   both configured and wanted by at least one device. Cheap and silent
   otherwise — this runs on EVERY gateway event."
  [sid event]
  (try (when (and (contains? #{"turn.completed" "turn.failed"} (get event "type"))
                  (pos? (device-count))
                  (configured?))
         (let [n (turn-notification sid event)]
           (future (broadcast! n))))
       (catch Throwable t
         (tel/log! {:level :warn :id ::push-tap-failed :data {:error (ex-message t)}})))
  nil)

(defn status
  "Push capability for `/v1/capabilities` and `/v1/admin/status`."
  []
  (let [cfg (config)]
    {:is-available (:is-configured cfg)
     :provider "apns"
     :environment (:default-environment cfg)
     :topic (:topic cfg)
     :missing (:missing cfg)
     :devices (device-count)}))
