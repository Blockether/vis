(ns com.blockether.vis.internal.gateway.web-push
  "Gateway-local Web Push.

   Every gateway owns one generated VAPID P-256 key pair in its own `~/.vis`
   home. The browser gets this gateway's public key, registers its subscription
   here, and this gateway encrypts and sends notifications directly to the
   browser's push service. There is no publisher URL or shared web relay."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.gateway.wire :as wire]
            [taoensso.telemere :as tel])
  (:import [java.io File]
           [java.math BigInteger]
           [java.net URI]
           [java.net.http HttpClient HttpClient$Version HttpRequest HttpRequest$BodyPublishers
            HttpResponse HttpResponse$BodyHandlers]
           [java.nio ByteBuffer]
           [java.nio.charset StandardCharsets]
           [java.security KeyFactory KeyPairGenerator PrivateKey SecureRandom Signature]
           [java.security.interfaces ECPublicKey]
           [java.security.spec ECGenParameterSpec X509EncodedKeySpec PKCS8EncodedKeySpec]
           [java.time Duration]
           [java.util Arrays Base64]
           [javax.crypto Cipher KeyAgreement Mac]
           [javax.crypto.spec GCMParameterSpec SecretKeySpec]))

(def ^:private MAX_PAYLOAD_BYTES 4079)
(def ^:private RECORD_SIZE 4096)
(def ^:private KEY_FILE "vapid.p8")
(def ^:private PUBLIC_FILE "vapid.pub")
(def ^:private DEFAULT_SUBJECT "mailto:vis@localhost")
(def ^:private KEY_INFO_PREFIX "WebPush: info\0")
(def ^:private AES_INFO "Content-Encoding: aes128gcm\0")
(def ^:private NONCE_INFO "Content-Encoding: nonce\0")
(def ^:private PUBLIC_KEY_PREFIX
  (byte-array [0x30 0x59 0x30 0x13 0x06 0x07 0x2a 0x86 0x48 0xce 0x3d 0x02 0x01 0x06 0x08 0x2a 0x86
               0x48 0xce 0x3d 0x03 0x01 0x07 0x03 0x42 0x00]))

(defn- env-val
  [name]
  (let [value (System/getenv name)]
    (when-not (str/blank? value) (str/trim value))))

(defn- web-home
  ^File []
  (io/file (or (System/getProperty "vis.push.home")
               (env-val "VIS_HOME")
               (str (System/getProperty "user.home") File/separator ".vis"))))

(defn key-file ^File [] (io/file (web-home) "web-push" KEY_FILE))
(defn- public-file ^File [] (io/file (web-home) "web-push" PUBLIC_FILE))

(defn- b64url
  ^String [^bytes bytes]
  (.encodeToString (.withoutPadding (Base64/getUrlEncoder)) bytes))

(defn- b64url-decode ^bytes [^String value] (.decode (Base64/getUrlDecoder) value))

(defn- base64-decode ^bytes [^String value] (.decode (Base64/getDecoder) value))

(defn- utf8 ^bytes [^String value] (.getBytes value StandardCharsets/UTF_8))

(defn- bytes-concat
  ^bytes [& arrays]
  (let [out (byte-array (reduce + 0 (map alength arrays)))]
    (loop
      [offset 0
       remaining arrays]

      (if-let [array (first remaining)]
        (do (System/arraycopy ^bytes array 0 out offset (alength array))
            (recur (+ offset (alength array)) (next remaining)))
        out))))

(defn- fixed32
  ^bytes [^BigInteger value]
  (let
    [raw
     (.toByteArray value)

     source-offset
     (max 0 (- (alength raw) 32))

     length
     (min 32 (alength raw))

     out
     (byte-array 32)]

    (System/arraycopy raw source-offset out (- 32 length) length)
    out))

(defn- public-key-bytes
  ^bytes [^ECPublicKey key]
  (bytes-concat (byte-array [4])
                (fixed32 (.getAffineX (.getW key)))
                (fixed32 (.getAffineY (.getW key)))))

(defn- pem
  [^bytes der]
  (str "-----BEGIN PRIVATE KEY-----\n"
       (.encodeToString (Base64/getMimeEncoder 64 (.getBytes "\n")) der)
       "\n-----END PRIVATE KEY-----\n"))

(defn- private-key
  ^PrivateKey [^String pem-text]
  (let
    [der (-> pem-text
             (str/replace #"-----(BEGIN|END)[^-]+-----" "")
             (str/replace #"\s" "")
             (base64-decode))]
    (.generatePrivate (KeyFactory/getInstance "EC") (PKCS8EncodedKeySpec. der))))

(defn- write-bytes!
  [^File file ^bytes bytes]
  (io/make-parents file)
  (with-open [out (io/output-stream file)]
    (.write out bytes))
  file)

(defn- generate-key-pair!
  []
  (let
    [pair
     (.generateKeyPair (doto (KeyPairGenerator/getInstance "EC")
                         (.initialize (ECGenParameterSpec. "secp256r1"))))

     private-file
     (key-file)

     public-file
     (public-file)]

    (io/make-parents private-file)
    (spit private-file (pem (.getEncoded (.getPrivate pair))))
    (write-bytes! public-file (.getEncoded (.getPublic pair)))
    (.setReadable private-file false false)
    (.setReadable private-file true true)
    {:private (.getPrivate pair) :public (.getPublic pair) :file private-file}))

(defn- load-key-pair
  []
  (let
    [private-file
     (key-file)

     public-file
     (public-file)]

    (when (and (.isFile private-file) (.isFile public-file))
      (try {:private (private-key (slurp private-file))
            :public (.generatePublic (KeyFactory/getInstance "EC")
                                     (X509EncodedKeySpec. (java.nio.file.Files/readAllBytes
                                                            (.toPath public-file))))
            :file private-file}
           (catch Throwable t
             (tel/log! {:level :warn :id ::invalid-vapid-key :data {:error (ex-message t)}})
             nil)))))

(defn- load-or-generate-key-pair
  []
  (or (load-key-pair)
      (locking (web-home)
        (or (load-key-pair)
            (when-not (and (.isFile (key-file)) (.isFile (public-file))) (generate-key-pair!))))))

(defn config
  "Return public Web Push configuration without private key material."
  []
  (let
    [pair
     (load-or-generate-key-pair)

     subject
     (or (env-val "VIS_WEB_PUSH_SUBJECT") DEFAULT_SUBJECT)

     valid-subject?
     (or (str/starts-with? subject "mailto:") (str/starts-with? subject "https://"))]

    {:is-configured (boolean (and pair valid-subject?))
     :application-server-key (when pair (b64url (public-key-bytes (:public pair))))
     :subject subject
     :source (when pair "generated")
     :missing (cond-> []
                (nil? pair)
                (conj "vapid_key")

                (not valid-subject?)
                (conj "subject"))}))

(defn configured? [] (:is-configured (config)))

(defn- hmac
  ^bytes [^bytes key ^bytes data]
  (let [mac (Mac/getInstance "HmacSHA256")]
    (.init mac (SecretKeySpec. key "HmacSHA256"))
    (.doFinal mac data)))

(defn- hkdf-expand
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
      (let [block (hmac prk (bytes-concat previous info (byte-array [(unchecked-byte counter)])))]
        (recur block (bytes-concat output block) (inc counter))))))

(defn- jose-signature
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

    (bytes-concat r s)))

(defn- vapid-token
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
     (doto (Signature/getInstance "SHA256withECDSA") (.initSign private) (.update (utf8 input)))]

    (str input "." (b64url (jose-signature (.sign signature))))))

(defn- subscription
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

          public-key
          (some-> (get keys "p256dh")
                  str
                  b64url-decode)

          auth
          (some-> (get keys "auth")
                  str
                  b64url-decode)]

         (when (and (str/starts-with? endpoint "https://")
                    (= 65 (alength public-key))
                    (= 16 (alength auth)))
           {:endpoint endpoint :public-key public-key :auth auth}))
       (catch Throwable _ nil)))

(defn- public-key
  ^ECPublicKey [^bytes raw]
  (.generatePublic (KeyFactory/getInstance "EC")
                   (X509EncodedKeySpec. (bytes-concat PUBLIC_KEY_PREFIX raw))))

(defn- encrypted-payload
  ^bytes [^bytes client-public ^bytes auth ^bytes plaintext]
  (let
    [client-key
     (public-key client-public)

     ephemeral
     (.generateKeyPair (doto (KeyPairGenerator/getInstance "EC")
                         (.initialize (ECGenParameterSpec. "secp256r1"))))

     agreement
     (doto (KeyAgreement/getInstance "ECDH")
       (.init (.getPrivate ephemeral))
       (.doPhase client-key true))

     shared
     (.generateSecret agreement)

     server-public
     (public-key-bytes (.getPublic ephemeral))

     key-info
     (bytes-concat (utf8 KEY_INFO_PREFIX) client-public server-public)

     ikm
     (hkdf-expand (hmac auth shared) key-info 32)

     salt
     (byte-array 16)

     _
     (doto (SecureRandom.) (.nextBytes salt))

     prk
     (hmac salt ikm)

     cek
     (hkdf-expand prk (utf8 AES_INFO) 16)

     nonce
     (hkdf-expand prk (utf8 NONCE_INFO) 12)

     cipher
     (doto (Cipher/getInstance "AES/GCM/NoPadding")
       (.init Cipher/ENCRYPT_MODE (SecretKeySpec. cek "AES") (GCMParameterSpec. 128 nonce)))

     ciphertext
     (.doFinal cipher (bytes-concat plaintext (byte-array [2])))

     record-size
     (doto (ByteBuffer/allocate 4) (.putInt RECORD_SIZE))]

    (bytes-concat salt
                  (.array record-size)
                  (byte-array [(unchecked-byte (alength server-public))])
                  server-public
                  ciphertext)))

(defn- notification-json
  [{:keys [title body data collapse-id]}]
  (wire/json-str (cond-> {:title (or title "Vis") :body (or body "") :data (or data {})}
                   collapse-id
                   (assoc :tag (str collapse-id)))))

(defn- origin
  ^String [^String endpoint]
  (let [uri (URI/create endpoint)]
    (str (.getScheme uri) "://" (.getAuthority uri))))

(defonce ^:private http-client
  (delay (-> (HttpClient/newBuilder)
             (.version HttpClient$Version/HTTP_2)
             (.connectTimeout (Duration/ofSeconds 10))
             (.build))))

(defn- post!
  [cfg pair subscription ^bytes body]
  (try (let
         [audience
          (origin (:endpoint subscription))

          jwt
          (vapid-token cfg (:private pair) audience)

          request
          (-> (HttpRequest/newBuilder (URI/create (:endpoint subscription)))
              (.header "authorization" (str "vapid t=" jwt ", k=" (:application-server-key cfg)))
              (.header "ttl" "86400")
              (.header "content-encoding" "aes128gcm")
              (.header "content-type" "application/octet-stream")
              (.timeout (Duration/ofSeconds 15))
              (.POST (HttpRequest$BodyPublishers/ofByteArray body))
              (.build))

          ^HttpResponse response
          (.send ^HttpClient @http-client request (HttpResponse$BodyHandlers/ofString))]

         {:status (.statusCode response)
          :reason (if (str/blank? (.body response)) "" (.body response))})
       (catch Throwable t {:status 0 :reason (or (ex-message t) "transport-error")})))

(defn send!
  "Encrypt and send one browser notification directly from this gateway."
  [token notification]
  (let
    [cfg
     (config)

     pair
     (load-or-generate-key-pair)

     target
     (subscription token)

     payload
     (utf8 (notification-json notification))]

    (cond (not (:is-configured cfg)) {:status 0 :reason "not-configured"}
          (nil? target) {:status 400 :reason "invalid-subscription"}
          (> (alength payload) MAX_PAYLOAD_BYTES) {:status 413 :reason "payload-too-large"}
          :else
          (post! cfg pair target (encrypted-payload (:public-key target) (:auth target) payload)))))
