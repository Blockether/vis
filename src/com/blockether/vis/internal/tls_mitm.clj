(ns com.blockether.vis.internal.tls-mitm
  "Ephemeral CA + per-host leaf minting for the egress proxy's TLS-terminating
   (MITM) tier — the piece that gives a jailed shell child GET-not-POST fidelity
   over HTTPS, matching what the interpreter method-guard already does pre-TLS.

   Why it exists: a plain CONNECT proxy only sees `CONNECT host:443` — the method
   and path live inside the TLS the proxy never opens, so HTTPS verb/path is
   opaque. To read them the proxy must TERMINATE the child's TLS: present the
   child a leaf cert for the requested host, decrypt, inspect method+path, then
   re-encrypt to the real upstream (whose real cert the proxy still validates).

   Trust model:
     - The CA is EPHEMERAL and per-session — born in this JVM, never written to
       the host trust store. Its cert PEM is written to a temp file whose path is
       injected into the jailed child's trust env (`CURL_CA_BUNDLE`/`SSL_CERT_FILE`
       /`REQUESTS_CA_BUNDLE`/`NODE_EXTRA_CA_CERTS`/`GIT_SSL_CAINFO`). Only children
       inside the jail ever see or trust it.
     - Upstream (proxy -> real server) uses the SYSTEM trust store by default, so
       the real server's real certificate is still validated end to end.

   No JCA provider is registered globally: bcpkix's Jca* builders use the default
   platform signer (SHA256withRSA via SunRsaSign), which keeps this native-image
   friendly and side-effect free."
  (:import
    (java.io File)
    (java.math BigInteger)
    (java.security KeyPair KeyPairGenerator KeyStore PrivateKey SecureRandom)
    (java.security.cert X509Certificate)
    (java.util Base64 Date)
    (java.util.concurrent ConcurrentHashMap)
    (java.util.function Function)
    (javax.net.ssl KeyManagerFactory SSLContext SSLSocketFactory TrustManager X509TrustManager)
    (org.bouncycastle.asn1.x500 X500Name)
    (org.bouncycastle.asn1.x509 BasicConstraints
                                ExtendedKeyUsage
                                Extension
                                GeneralName
                                GeneralNames
                                KeyPurposeId
                                KeyUsage)
    (org.bouncycastle.cert.jcajce JcaX509CertificateConverter JcaX509v3CertificateBuilder)
    (org.bouncycastle.operator.jcajce JcaContentSignerBuilder)))

(def ^:private ^SecureRandom secure-rng (SecureRandom.))

(def ^:private day-ms 86400000)
(def ^:private validity-ms (* 825 day-ms)) ; 825 days: the CA/browser leaf ceiling.

(defn- gen-keypair
  "A fresh 2048-bit RSA key pair."
  ^KeyPair []
  (.generateKeyPair (doto (KeyPairGenerator/getInstance "RSA") (.initialize 2048))))

(defn- rand-serial ^BigInteger [] (BigInteger. 159 secure-rng))

(defn- ->x509 ^X509Certificate [holder] (.getCertificate (JcaX509CertificateConverter.) holder))

(defn- signer [^PrivateKey key] (.build (JcaContentSignerBuilder. "SHA256withRSA") key))

(defn gen-ca
  "Mint a self-signed CA certificate + its key pair (in memory only)."
  []
  (let
    [kp
     (gen-keypair)

     nm
     (X500Name. "CN=vis ephemeral CA,O=vis")

     now
     (System/currentTimeMillis)

     builder
     (doto (JcaX509v3CertificateBuilder. nm
                                         (rand-serial)
                                         (Date. (- now day-ms))
                                         (Date. (+ now validity-ms))
                                         nm
                                         (.getPublic kp))
       (.addExtension Extension/basicConstraints true (BasicConstraints. true))
       (.addExtension Extension/keyUsage
                      true
                      (KeyUsage. (int (bit-or KeyUsage/keyCertSign KeyUsage/cRLSign)))))]

    {:cert (->x509 (.build builder (signer (.getPrivate kp)))) :key-pair kp :name nm}))

(def ^:private ipv4-re #"\d{1,3}(\.\d{1,3}){3}")

(defn- mint-leaf
  "Mint a leaf certificate for `host`, signed by the CA, with `host` as SAN."
  ^X509Certificate [^X500Name ca-name ^PrivateKey ca-key ^KeyPair leaf-kp ^String host]
  (let
    [issuer
     ca-name

     subject
     (X500Name. (str "CN=" host))

     now
     (System/currentTimeMillis)

     ip?
     (boolean (re-matches ipv4-re host))

     san
     (GeneralNames. (into-array GeneralName
                                [(GeneralName. (if ip? GeneralName/iPAddress GeneralName/dNSName)
                                               host)]))

     builder
     (doto (JcaX509v3CertificateBuilder. issuer
                                         (rand-serial)
                                         (Date. (- now day-ms))
                                         (Date. (+ now validity-ms))
                                         subject
                                         (.getPublic leaf-kp))
       (.addExtension Extension/basicConstraints true (BasicConstraints. false))
       (.addExtension Extension/keyUsage
                      true
                      (KeyUsage. (int (bit-or KeyUsage/digitalSignature KeyUsage/keyEncipherment))))
       (.addExtension Extension/extendedKeyUsage
                      false
                      (ExtendedKeyUsage. (into-array KeyPurposeId [KeyPurposeId/id_kp_serverAuth])))
       (.addExtension Extension/subjectAlternativeName false san))]

    (->x509 (.build builder (signer ca-key)))))

(defn- host-context
  "A server-mode SSLContext that presents a freshly minted leaf for `host`."
  ^SSLContext [^X500Name ca-name ^X509Certificate ca-cert ^PrivateKey ca-key ^KeyPair leaf-kp ^String host]
  (let
    [leaf
     (mint-leaf ca-name ca-key leaf-kp host)

     pw
     (char-array 0)

     ks
     (doto (KeyStore/getInstance "PKCS12")
       (.load nil nil)
       (.setKeyEntry "leaf" (.getPrivate leaf-kp) pw (into-array X509Certificate [leaf ca-cert])))

     kmf
     (doto (KeyManagerFactory/getInstance (KeyManagerFactory/getDefaultAlgorithm)) (.init ks pw))]

    (doto (SSLContext/getInstance "TLS") (.init (.getKeyManagers kmf) nil nil))))

(defn- trust-all-factory
  "An SSLSocketFactory that trusts any upstream cert — TEST ONLY (self-signed
   local upstreams). Production upstream validation uses the system trust store."
  ^SSLSocketFactory []
  (let
    [tm (reify
          X509TrustManager
            (checkClientTrusted [_ _ _])
            (checkServerTrusted [_ _ _])
            (getAcceptedIssuers [_] (make-array X509Certificate 0)))]
    (.getSocketFactory (doto (SSLContext/getInstance "TLS")
                         (.init nil (into-array TrustManager [tm]) secure-rng)))))

(defn- write-ca-pem
  "Write `cert` as a PEM file under the system temp dir (readable inside the jail,
   whose RW set includes the temp dirs) and return its absolute path."
  ^String [^X509Certificate cert]
  (let
    [b64
     (.encodeToString (Base64/getMimeEncoder 64 (.getBytes "\n")) (.getEncoded cert))

     pem
     (str "-----BEGIN CERTIFICATE-----\n" b64 "\n-----END CERTIFICATE-----\n")

     f
     (File/createTempFile "vis-ca-" ".pem" (File. (System/getProperty "java.io.tmpdir")))]

    (.deleteOnExit f)
    (spit f pem)
    (.getAbsolutePath f)))

(defn create!
  "Build a per-session MITM capability. Returns a map:
     :ca-cert          the ephemeral CA certificate
     :ca-file          path to the CA PEM (inject into the child's trust env)
     :ctx-for          (fn [host] -> server SSLContext), cached per host
     :upstream-factory SSLSocketFactory for the proxy->real-server leg
     :close!           delete the CA PEM file

   Options: `:upstream-trust-all?` (TEST) swaps upstream validation for trust-all."
  ([] (create! {}))
  ([{:keys [upstream-trust-all?]}]
   (let
     [{:keys [cert key-pair name]}
      (gen-ca)

      ca-key
      (.getPrivate ^KeyPair key-pair)

      leaf-kp
      (gen-keypair)

      cache
      (ConcurrentHashMap.)

      ctx-for
      (fn [^String host]
        (.computeIfAbsent cache
                          host
                          (reify
                            Function
                              (apply [_ h] (host-context name cert ca-key leaf-kp h)))))

      ca-file
      (write-ca-pem cert)]

     {:ca-cert cert
      :ca-file ca-file
      :ctx-for ctx-for
      :upstream-factory (if upstream-trust-all? (trust-all-factory) (SSLSocketFactory/getDefault))
      :close! (fn []
                (try (.delete (File. ca-file)) (catch Throwable _ nil)))})))
