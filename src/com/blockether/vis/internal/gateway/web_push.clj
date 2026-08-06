(ns com.blockether.vis.internal.gateway.web-push
  "Gateway-local Web Push (RFC 8291 and RFC 8292).

   Each gateway owns one generated VAPID P-256 key pair in its own `~/.vis`
   home. The browser gets this gateway's public key, registers its subscription
   here, and this gateway encrypts and sends notifications directly to the
   browser's push service. There is no publisher URL or shared web relay.

   Java interop is kept at the cryptographic, file, and HTTP boundaries. The
   rest of the namespace passes ordinary Clojure maps and byte arrays between
   small helpers so the protocol steps remain visible and testable."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.gateway.wire :as wire]
            [taoensso.telemere :as tel])
  (:import [java.io File]
           [java.math BigInteger]
           [java.net URI]
           [java.net.http HttpClient HttpClient$Version HttpRequest HttpRequest$BodyPublishers
            HttpRequest$Builder HttpResponse HttpResponse$BodyHandlers]
           [java.nio ByteBuffer]
           [java.nio.charset StandardCharsets]
           [java.nio.file Files]
           [java.security KeyFactory KeyPair KeyPairGenerator PrivateKey SecureRandom Signature]
           [java.security.interfaces ECPublicKey]
           [java.security.spec ECGenParameterSpec X509EncodedKeySpec PKCS8EncodedKeySpec]
           [java.time Duration]
           [java.util Arrays Base64]
           [javax.crypto Cipher KeyAgreement Mac]
           [javax.crypto.spec GCMParameterSpec SecretKeySpec]))

;; The aes128gcm content-coding reserves a two-byte delimiter and a 16-byte
;; GCM authentication tag. Keep the protocol's limits in one place rather than
;; scattering magic numbers through the encryption and dispatch paths.
(def ^:private MAX_PAYLOAD_BYTES 4079)
(def ^:private RECORD_SIZE 4096)
(def ^:private KEY_FILE "vapid.p8")
(def ^:private PUBLIC_FILE "vapid.pub")
(def ^:private DEFAULT_SUBJECT "mailto:vis@localhost")
(def ^:private KEY_INFO_PREFIX "WebPush: info\0")
(def ^:private AES_INFO "Content-Encoding: aes128gcm\0")
(def ^:private NONCE_INFO "Content-Encoding: nonce\0")

;; SubjectPublicKeyInfo for an uncompressed P-256 public key. Browsers hand us
;; the 65-byte `04 || x || y` form; JCA wants this DER prefix around it.
(def ^:private PUBLIC_KEY_PREFIX
  (byte-array [0x30 0x59 0x30 0x13 0x06 0x07 0x2a 0x86 0x48 0xce 0x3d 0x02 0x01 0x06 0x08 0x2a 0x86
               0x48 0xce 0x3d 0x03 0x01 0x07 0x03 0x42 0x00]))

(defonce ^:private key-lock (Object.))

(defn- env-val
  "Return a trimmed, non-blank environment variable, or nil."
  [name]
  (some-> (System/getenv name)
          str/trim
          not-empty))

(defn- web-home
  "Resolve the Vis home used for gateway-owned push state.

   Tests override `vis.push.home`; normal gateways use `VIS_HOME` or the
   conventional `~/.vis` directory."
  ^File []
  (io/file (or (System/getProperty "vis.push.home")
               (env-val "VIS_HOME")
               (str (System/getProperty "user.home") File/separator ".vis"))))

(defn- web-push-dir
  "Return the directory containing this gateway's Web Push identity."
  ^File []
  (io/file (web-home) "web-push"))

(defn key-file
  "Return the private VAPID key path owned by this gateway.

   The companion never receives this file; only the derived public key is
   exposed through the gateway capability response."
  ^File []
  (io/file (web-push-dir) KEY_FILE))

(defn- public-file
  "Return the persisted SubjectPublicKeyInfo for the VAPID public key."
  ^File []
  (io/file (web-push-dir) PUBLIC_FILE))

(defn- key-files
  "Return the two persisted files as named data for key loading/writing."
  []
  {:private-file (key-file) :public-file (public-file)})

(defn- b64url
  "Encode bytes as unpadded RFC 4648 base64url."
  ^String [^bytes bytes]
  (.encodeToString (.withoutPadding (Base64/getUrlEncoder)) bytes))

(defn- b64url-decode
  "Decode an unpadded browser base64url value."
  ^bytes [^String value]
  (.decode (Base64/getUrlDecoder) value))

(defn- base64-decode
  "Decode regular base64, as used inside a PEM document."
  ^bytes [^String value]
  (.decode (Base64/getDecoder) value))

(defn- utf8
  "Encode a protocol string with the protocol's required UTF-8 charset."
  ^bytes [^String value]
  (.getBytes value StandardCharsets/UTF_8))

(defn- concat-bytes
  "Concatenate byte arrays without converting them through Clojure sequences."
  ^bytes [& arrays]
  (let
    [length
     (reduce (fn [^long total ^bytes array]
               (+ total (alength array)))
             0
             arrays)

     output
     (byte-array length)]

    (loop
      [offset
       0

       remaining
       arrays]

      (if-let [array (first remaining)]
        (let
          [^bytes array array
           length (alength array)]

          (System/arraycopy array 0 output offset length)
          (recur (+ offset length) (next remaining)))
        output))))

(defn- fixed32
  "Represent a positive EC coordinate as exactly 32 unsigned bytes."
  ^bytes [^BigInteger value]
  (let
    [raw
     (.toByteArray value)

     source-offset
     (max 0 (- (alength raw) 32))

     length
     (min 32 (alength raw))

     output
     (byte-array 32)]

    (System/arraycopy raw source-offset output (- 32 length) length)
    output))

(defn- public-key-bytes
  "Encode a JCA P-256 public key as the browser's uncompressed point form."
  ^bytes [^ECPublicKey key]
  (concat-bytes (byte-array [4])
                (fixed32 (.getAffineX (.getW key)))
                (fixed32 (.getAffineY (.getW key)))))

(defn- pem
  "Serialize a PKCS#8 byte array as the PEM form used on disk."
  [^bytes der]
  (str "-----BEGIN PRIVATE KEY-----\n"
       (.encodeToString (Base64/getMimeEncoder 64 (.getBytes "\n")) der)
       "\n-----END PRIVATE KEY-----\n"))

(defn- private-key
  "Parse a PKCS#8 EC private key from PEM text."
  ^PrivateKey [^String pem-text]
  (let
    [der (-> pem-text
             (str/replace #"-----(BEGIN|END)[^-]+-----" "")
             (str/replace #"\s" "")
             base64-decode)]
    (.generatePrivate (KeyFactory/getInstance "EC") (PKCS8EncodedKeySpec. der))))

(defn- read-bytes
  "Read a complete binary key file."
  ^bytes [^File file]
  (Files/readAllBytes (.toPath file)))

(defn- write-bytes!
  "Write bytes to a file, creating its parent directory first."
  [^File file ^bytes bytes]
  (io/make-parents file)
  (with-open [out (io/output-stream file)]
    (.write out bytes))
  file)

(defn- ec-key-pair
  "Generate one fresh P-256 key pair for the gateway's VAPID identity."
  []
  (.generateKeyPair (doto (KeyPairGenerator/getInstance "EC")
                      (.initialize (ECGenParameterSpec. "secp256r1")))))

(defn- ec-public-key
  "Parse a SubjectPublicKeyInfo-encoded EC public key from disk."
  ^ECPublicKey [^File file]
  (.generatePublic (KeyFactory/getInstance "EC") (X509EncodedKeySpec. (read-bytes file))))

(defn- write-key-pair!
  "Persist a generated key pair, restricting the private file to its owner."
  [^KeyPair pair]
  (let
    [{:keys [private-file public-file]}
     (key-files)

     ^PrivateKey private-key-value
     (.getPrivate pair)

     ^ECPublicKey public-key-value
     (.getPublic pair)]

    (io/make-parents private-file)
    (spit private-file (pem (.getEncoded private-key-value)))
    (write-bytes! public-file (.getEncoded public-key-value))
    (.setReadable ^File private-file false false)
    (.setReadable ^File private-file true true)
    {:private private-key-value :public public-key-value :file private-file}))

(defn- load-key-pair
  "Load the persisted VAPID pair, or nil when either half is absent/invalid."
  []
  (let [{:keys [private-file public-file]} (key-files)]
    (when (every? #(.isFile ^File %) [private-file public-file])
      (try {:private (private-key (slurp private-file))
            :public (ec-public-key public-file)
            :file private-file}
           (catch Throwable t
             (tel/log! {:level :warn :id ::invalid-vapid-key :data {:error (ex-message t)}})
             nil)))))

(defn- load-or-generate-key-pair
  "Load the stable gateway identity, generating it once when needed.

   The second load inside the lock closes the startup race between gateway
   threads that first ask for capability data at the same time."
  []
  (or (load-key-pair) (locking key-lock (or (load-key-pair) (write-key-pair! (ec-key-pair))))))

(defn- valid-subject?
  "Whether a VAPID subject is a permitted mailto or HTTPS contact URI."
  [subject]
  (or (str/starts-with? subject "mailto:") (str/starts-with? subject "https://")))

(defn- missing-config
  "Describe the public configuration pieces that are not usable."
  [pair subject-valid?]
  (cond-> []
    (nil? pair)
    (conj "vapid_key")

    (not subject-valid?)
    (conj "subject")))

(defn config
  "Return public Web Push configuration without private key material.

   Calling this ensures that a gateway has a stable VAPID identity. The
   private key remains on disk and is only used by `send!`."
  []
  (let
    [pair
     (load-or-generate-key-pair)

     subject
     (or (env-val "VIS_WEB_PUSH_SUBJECT") DEFAULT_SUBJECT)

     subject-valid?
     (valid-subject? subject)]

    {:is-configured (boolean (and pair subject-valid?))
     :application-server-key (when pair (b64url (public-key-bytes (:public pair))))
     :subject subject
     :source (when pair "generated")
     :missing (missing-config pair subject-valid?)}))

(defn configured?
  "True when this gateway can encrypt and authenticate Web Push requests."
  []
  (:is-configured (config)))

(defn- hmac
  "Compute HMAC-SHA256, the primitive used by VAPID and Web Push HKDF."
  ^bytes [^bytes key ^bytes data]
  (let [mac (doto (Mac/getInstance "HmacSHA256") (.init (SecretKeySpec. key "HmacSHA256")))]
    (.doFinal mac data)))

(defn- hkdf-expand
  "Expand an HKDF pseudorandom key to exactly `length` bytes."
  ^bytes [^bytes prk ^bytes info ^long length]
  (loop
    [previous
     (byte-array 0)

     output
     (byte-array 0)

     counter
     1]

    (if (>= (alength output) length)
      (Arrays/copyOf output length)
      (let [block (hmac prk (concat-bytes previous info (byte-array [(unchecked-byte counter)])))]
        (recur block (concat-bytes output block) (inc counter))))))

(defn- sign
  "Sign UTF-8 text with the requested JCA signature algorithm."
  ^bytes [^PrivateKey private ^String algorithm ^String input]
  (let
    [signature (doto (Signature/getInstance algorithm) (.initSign private) (.update (utf8 input)))]
    (.sign signature)))

(defn- jose-signature
  "Convert JCA's DER ECDSA signature into the JOSE `r || s` form."
  ^bytes [^bytes der]
  (let
    [r-length
     (long (aget der 3))

     r
     (fixed32 (BigInteger. 1 (Arrays/copyOfRange der 4 (+ 4 r-length))))

     s-offset
     (+ 4 r-length 2)

     s-length
     (long (aget der (+ 4 r-length 1)))

     s
     (fixed32 (BigInteger. 1 (Arrays/copyOfRange der s-offset (+ s-offset s-length))))]

    (concat-bytes r s)))

(defn- vapid-token
  "Build the short-lived VAPID JWT for one push-service origin."
  [cfg ^PrivateKey private ^String audience]
  (let
    [header
     (b64url (utf8 (wire/json-str {:alg "ES256" :typ "JWT"})))

     claims
     (b64url (utf8 (wire/json-str {:aud audience
                                   :exp (+ (quot (System/currentTimeMillis) 1000) (* 12 60 60))
                                   :sub (:subject cfg)})))

     input
     (str header "." claims)

     signature
     (sign private "SHA256withECDSA" input)]

    (str input "." (b64url (jose-signature signature)))))

(defn- subscription-key
  "Decode one browser subscription key and enforce its RFC byte length."
  [keys name length]
  (let
    [encoded
     (some-> (get keys name)
             str
             str/trim)

     decoded
     (some-> encoded
             b64url-decode)]

    (when (and decoded (= length (alength ^bytes decoded))) decoded)))

(defn- subscription
  "Parse and validate the JSON subscription stored as a device token."
  [^String token]
  (try (let
         [value
          (wire/parse-json token)

          endpoint
          (some-> (get value "endpoint")
                  str
                  str/trim)

          keys
          (get value "keys")

          client-public
          (subscription-key keys "p256dh" 65)

          auth
          (subscription-key keys "auth" 16)]

         (when (and (str/starts-with? endpoint "https://") client-public auth)
           {:endpoint endpoint :public-key client-public :auth auth}))
       (catch Throwable _ nil)))

(defn- client-public-key
  "Convert a browser's uncompressed P-256 point into a JCA public key."
  ^ECPublicKey [^bytes raw]
  (.generatePublic (KeyFactory/getInstance "EC")
                   (X509EncodedKeySpec. (concat-bytes PUBLIC_KEY_PREFIX raw))))

(defn- key-agreement
  "Generate an ephemeral server key and derive its ECDH secret."
  [^bytes client-public]
  (let
    [client-key
     (client-public-key client-public)

     ^KeyPair ephemeral
     (ec-key-pair)

     agreement
     (doto (KeyAgreement/getInstance "ECDH")
       (.init (.getPrivate ephemeral))
       (.doPhase client-key true))]

    {:shared-secret (.generateSecret agreement)
     :server-public (public-key-bytes (.getPublic ephemeral))}))

(defn- content-keys
  "Derive the aes128gcm content-encryption key and nonce from ECDH output."
  [^bytes client-public ^bytes server-public ^bytes auth ^bytes shared]
  (let
    [key-info
     (concat-bytes (utf8 KEY_INFO_PREFIX) client-public server-public)

     ikm
     (hkdf-expand (hmac auth shared) key-info 32)

     salt
     (byte-array 16)

     _
     (doto (SecureRandom.) (.nextBytes salt))

     prk
     (hmac salt ikm)]

    {:salt salt
     :cek (hkdf-expand prk (utf8 AES_INFO) 16)
     :nonce (hkdf-expand prk (utf8 NONCE_INFO) 12)}))

(defn- encrypted-payload
  "Encrypt a notification and frame it as one RFC 8188/8291 record."
  ^bytes [^bytes client-public ^bytes auth ^bytes plaintext]
  (let
    [agreement
     (key-agreement client-public)

     ^bytes shared-secret
     (:shared-secret agreement)

     ^bytes server-public
     (:server-public agreement)

     {:keys [salt cek nonce]}
     (content-keys client-public server-public auth shared-secret)

     cipher
     (doto (Cipher/getInstance "AES/GCM/NoPadding")
       (.init Cipher/ENCRYPT_MODE (SecretKeySpec. cek "AES") (GCMParameterSpec. 128 nonce)))

     ciphertext
     (.doFinal cipher (concat-bytes plaintext (byte-array [2])))

     record-size
     (doto (ByteBuffer/allocate 4) (.putInt RECORD_SIZE))]

    (concat-bytes salt
                  (.array record-size)
                  (byte-array [(unchecked-byte (alength server-public))])
                  server-public
                  ciphertext)))

(defn- notification-json
  "Encode the browser notification envelope using the gateway wire encoder."
  [{:keys [title body data collapse-id]}]
  (wire/json-str (cond-> {:title (or title "Vis") :body (or body "") :data (or data {})}
                   collapse-id
                   (assoc :tag (str collapse-id)))))

(defn- origin
  "Return the scheme and authority used as the VAPID JWT audience."
  ^String [^String endpoint]
  (let [uri (URI/create endpoint)]
    (str (.getScheme uri) "://" (.getAuthority uri))))

(defn- build-http-client
  "Build the shared HTTP/2 client used for push-service requests."
  []
  (-> (HttpClient/newBuilder)
      (.version HttpClient$Version/HTTP_2)
      (.connectTimeout (Duration/ofSeconds 10))
      (.build)))

(defonce ^:private http-client (delay (build-http-client)))

(defn- header
  "Add one string-valued header while keeping builder interop in one helper."
  ^HttpRequest$Builder [^HttpRequest$Builder request name value]
  (.header request name (str value)))

(defn- push-request
  "Build the authenticated encrypted request sent to a browser push service."
  [cfg pair subscription ^bytes body]
  (let
    [endpoint
     (:endpoint subscription)

     jwt
     (vapid-token cfg (:private pair) (origin endpoint))]

    (-> (HttpRequest/newBuilder (URI/create endpoint))
        (header "authorization" (str "vapid t=" jwt ", k=" (:application-server-key cfg)))
        (header "ttl" "86400")
        (header "content-encoding" "aes128gcm")
        (header "content-type" "application/octet-stream")
        (.timeout (Duration/ofSeconds 15))
        (.POST (HttpRequest$BodyPublishers/ofByteArray body))
        (.build))))

(defn- response-result
  "Convert a JDK response into the gateway's stable push result map."
  [^HttpResponse response]
  {:status (.statusCode response) :reason (if (str/blank? (.body response)) "" (.body response))})

(defn- send-http-request
  "Send one request through the shared JDK HTTP client."
  [^HttpRequest request]
  (let [^HttpClient client @http-client]
    (.send client request (HttpResponse$BodyHandlers/ofString))))

(defn- post!
  "Send one encrypted request and turn transport failures into status zero."
  [cfg pair subscription ^bytes body]
  (try (-> (push-request cfg pair subscription body)
           send-http-request
           response-result)
       (catch Throwable t {:status 0 :reason (or (ex-message t) "transport-error")})))

(defn send!
  "Encrypt and send one browser notification directly from this gateway.

   `token` is the JSON subscription stored by `push/register-device!`.
   Returns `{:status int :reason string}` and never lets a provider or
   transport exception escape into the gateway event loop."
  [token notification]
  (let [cfg (config)]
    (cond (not (:is-configured cfg)) {:status 0 :reason "not-configured"}
          :else (let [target (subscription token)]
                  (cond (nil? target) {:status 400 :reason "invalid-subscription"}
                        :else (let [payload (utf8 (notification-json notification))]
                                (if (> (long (alength ^bytes payload)) (long MAX_PAYLOAD_BYTES))
                                  {:status 413 :reason "payload-too-large"}
                                  (let [pair (load-or-generate-key-pair)]
                                    (post! cfg
                                           pair
                                           target
                                           (encrypted-payload (:public-key target)
                                                              (:auth target)
                                                              payload))))))))))
