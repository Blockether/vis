(ns com.blockether.vis.internal.gateway.fcm
  "Android push (Firebase Cloud Messaging HTTP v1).

   Apple's APNs lives in `gateway.push`; this is its Android twin and the two
   are dispatched on the registered device's `:platform`. Credentials are a
   Google service-account JSON — from the macOS keychain (service `vis-fcm`,
   account `service_account`), from the environment, or from a file under
   `~/.vis/fcm/`. Key material is never returned, logged or sent over the wire."
  (:require [babashka.http-client :as http]
            [clojure.java.io :as io]
            [clojure.java.shell :as sh]
            [clojure.string :as str]
            [com.blockether.vis.internal.gateway.wire :as wire]
            [taoensso.telemere :as tel])
  (:import [java.io File]
           [java.net URLEncoder]
           [java.nio.charset StandardCharsets]
           [java.security KeyFactory Signature]
           [java.security.spec PKCS8EncodedKeySpec]
           [java.util Base64]))

(def ^:private TOKEN_URI "https://oauth2.googleapis.com/token")

(def ^:private SCOPE "https://www.googleapis.com/auth/firebase.messaging")

(def ^:private ^:const JWT_TTL_SECONDS 3600)

;; Credentials

(defn- vis-home
  ^File []
  (io/file (or (System/getProperty "vis.push.home")
               (System/getenv "VIS_HOME")
               (str (System/getProperty "user.home") File/separator ".vis"))))

(defn- fcm-dir ^File [] (io/file (vis-home) "fcm"))

(defn- env-val
  [k]
  (let [v (System/getenv k)]
    (when-not (str/blank? v) (str/trim v))))

(defn- unhex
  "`security -w` prints hex, not text, whenever the stored password is not plain
   printable ASCII — which a service-account JSON with an embedded PEM never is."
  [s]
  (if (and (even? (count s)) (re-matches #"(?i)[0-9a-f]{32,}" s))
    (String. (byte-array (map #(unchecked-byte (Integer/parseInt (apply str %) 16))
                              (partition 2 s)))
             StandardCharsets/UTF_8)
    s))

(defn- keychain
  "Generic password under service `vis-fcm` in the macOS login keychain, read on
   demand so locking the keychain revokes access immediately."
  [account]
  (when (and (str/includes? (str/lower-case (str (System/getProperty "os.name"))) "mac")
             ;; A redirected push home means a test fixture: never let the
             ;; developer's real keychain leak into it.
             (nil? (System/getProperty "vis.push.home")))
    (try (let [{:keys [exit out]}
               (sh/sh "security" "find-generic-password" "-s" "vis-fcm" "-a" account "-w")]
           (when (and (= 0 (long exit)) (not (str/blank? out))) (unhex (str/trim out))))
         (catch Throwable _ nil))))

(defn- discovered-file
  "First `*.json` under `~/.vis/fcm/` — where a downloaded service-account key
   naturally lands."
  []
  (->> (.listFiles (fcm-dir))
       seq
       (filter #(and (.isFile ^File %) (str/ends-with? (.getName ^File %) ".json")))
       (sort-by #(.getName ^File %))
       first))

(defn- service-account
  "The parsed service-account JSON, or nil. NEVER expose the result."
  []
  (let [raw (or (keychain "service_account")
                (env-val "VIS_FCM_SERVICE_ACCOUNT")
                (some-> (env-val "VIS_FCM_SERVICE_ACCOUNT_PATH")
                        io/file
                        (as-> f (when (.isFile ^File f) (slurp f))))
                (some-> (discovered-file)
                        slurp))]
    (when-not (str/blank? raw) (try (wire/parse-json raw) (catch Throwable _ nil)))))

(defn- source
  []
  (cond (keychain "service_account") "keychain"
        (env-val "VIS_FCM_SERVICE_ACCOUNT") "env"
        (env-val "VIS_FCM_SERVICE_ACCOUNT_PATH") "file"
        (discovered-file) "file"
        :else nil))

(defn config
  "Resolved FCM credentials WITHOUT key material: project id, the service
   account's email, where it came from, and what is still missing."
  []
  (let [sa
        (service-account)

        project-id
        (or (env-val "VIS_FCM_PROJECT_ID") (get sa "project_id"))

        missing
        (cond-> []
          (str/blank? (str (get sa "private_key")))
          (conj "service_account")

          (str/blank? (str (get sa "client_email")))
          (conj "client_email")

          (str/blank? (str project-id))
          (conj "project_id"))]

    {:project-id project-id
     :client-email (get sa "client_email")
     :source (source)
     :missing missing
     :is-configured (empty? missing)}))

(defn configured?
  "True when this gateway can actually deliver an Android push."
  []
  (:is-configured (config)))

;; OAuth access token (RS256 JWT -> Google token endpoint)

(defn- b64url ^String [^bytes b] (.encodeToString (.withoutPadding (Base64/getUrlEncoder)) b))

(defn- utf8 ^bytes [^String s] (.getBytes s StandardCharsets/UTF_8))

(defn- private-key
  "Parse the PKCS#8 PEM Google embeds in the service-account JSON."
  [^String pem]
  (let [b64 (-> pem
                (str/replace #"-----[A-Z ]+-----" "")
                (str/replace #"\s" ""))]
    (.generatePrivate (KeyFactory/getInstance "RSA")
                      (PKCS8EncodedKeySpec. (.decode (Base64/getDecoder) b64)))))

(defn- sign-jwt
  [sa]
  (let [now
        (quot (System/currentTimeMillis) 1000)

        header
        (b64url (utf8 (wire/json-str {:alg "RS256" :typ "JWT"})))

        claims
        (b64url (utf8 (wire/json-str {:iss (get sa "client_email")
                                      :scope SCOPE
                                      :aud (or (get sa "token_uri") TOKEN_URI)
                                      :iat now
                                      :exp (+ now (long JWT_TTL_SECONDS))})))

        signing-input
        (str header "." claims)

        sig
        (doto (Signature/getInstance "SHA256withRSA")
          (.initSign (private-key (get sa "private_key")))
          (.update (utf8 signing-input)))]

    (str signing-input "." (b64url (.sign sig)))))

(defonce ^:private http-client (delay (http/client {:connect-timeout 10000 :version :http2})))

(defonce ^:private token-cache
  ;; {:token "…" :expires-at ms :client-email "…"}
  (atom nil))

(defn- form-encode
  [m]
  (str/join "&"
            (map (fn [[k v]]
                   (str k "=" (URLEncoder/encode (str v) StandardCharsets/UTF_8)))
                 m)))

(defn- fetch-access-token
  [sa]
  (let [body
        (form-encode {"grant_type" "urn:ietf:params:oauth:grant-type:jwt-bearer"
                      "assertion" (sign-jwt sa)})

        resp
        (http/request {:uri (or (get sa "token_uri") TOKEN_URI)
                       :method :post
                       :client @http-client
                       :headers {"content-type" "application/x-www-form-urlencoded"}
                       :body body
                       :timeout 15000
                       :throw false
                       :as :string})

        parsed
        (wire/parse-json (:body resp))]

    (when (= 200 (:status resp)) (get parsed "access_token"))))

(defn- access-token
  "Cached OAuth access token; Google's are valid an hour, refreshed at 50 min."
  [sa]
  (let [{:keys [token expires-at client-email]} @token-cache]
    (if (and token
             (= client-email (get sa "client_email"))
             (< (System/currentTimeMillis) (long expires-at)))
      token
      (when-let [t (fetch-access-token sa)]
        (reset! token-cache {:token t
                             :expires-at (+ (System/currentTimeMillis) (* 50 60 1000))
                             :client-email (get sa "client_email")})
        t))))

;; Sending

(defn- message
  "One FCM v1 message. `data` values must be STRINGS — FCM rejects anything else.

   `thread-id` becomes the notification's TAG, and that tag is the whole Android
   badge. A launcher there writes no number of its own: it dots the icon while
   this app holds a notification, so the tray IS the badge and must hold one
   live alert per session. The tag is also the only identity that survives
   delivery — Firebase builds the tray entry itself and copies only its own
   `android.*` keys into it, never this `data` map — so a phone tidying its tray
   can tell which session a delivered alert came from by tag and nothing else."
  [token {:keys [title body data thread-id collapse-id]}]
  {:message {:token token
             :notification {:title title :body body}
             :data (into {}
                         (map (fn [[k v]]
                                [(name k) (str v)]))
                         (or data {}))
             :android (cond-> {:priority "HIGH"
                               :notification (cond-> {:sound "default"}
                                               thread-id
                                               (assoc :tag (str thread-id)))}
                        collapse-id
                        (assoc :collapse_key (str collapse-id)))}})

(defn send!
  "Deliver one alert to one Android device token. Returns `{:status int :reason
   str}` — status 0 for a transport failure, so this never throws. A `reason` of
   `UNREGISTERED` means the caller should drop the token."
  [token notification]
  (try (let [sa
             (service-account)

             cfg
             (config)]

         (if-not (:is-configured cfg)
           {:status 0 :reason "not-configured"}
           (if-let [at (access-token sa)]
             (let [resp (http/request {:uri (str "https://fcm.googleapis.com/v1/projects/"
                                                 (:project-id cfg)
                                                 "/messages:send")
                                       :method :post
                                       :client @http-client
                                       :headers {"authorization" (str "Bearer " at)
                                                 "content-type" "application/json"}
                                       :body (wire/json-str (message token notification))
                                       :timeout 15000
                                       :throw false
                                       :as :string})
                   status (:status resp)
                   parsed (wire/parse-json (:body resp))
                   reason (or (get-in parsed ["error" "details" 0 "errorCode"])
                              (get-in parsed ["error" "status"])
                              "")]

               (when (not= 200 status)
                 (tel/log! {:level :warn :id ::fcm-failed :data {:status status :reason reason}}))
               {:status status :reason reason})
             {:status 0 :reason "oauth-failed"})))
       (catch Throwable t {:status 0 :reason (or (ex-message t) "transport-error")})))

(defn dead-token?
  "True when FCM's verdict means the registration is gone for good."
  [{:keys [status reason]}]
  (or (= 404 status) (contains? #{"UNREGISTERED" "NOT_FOUND"} (str reason))))

(defn status
  "FCM half of the push capability."
  []
  (let [cfg (config)]
    {:is-available (:is-configured cfg)
     :provider "fcm"
     :project_id (:project-id cfg)
     :source (:source cfg)
     :missing (:missing cfg)}))
