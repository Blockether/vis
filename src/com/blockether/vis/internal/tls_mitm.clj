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
  (:import (java.io File FileOutputStream)
           (java.math BigInteger)
           (java.security KeyPair KeyPairGenerator KeyStore PrivateKey SecureRandom)
           (java.security.cert X509Certificate)
           (java.util Base64 Date UUID)
           (java.util.concurrent ConcurrentHashMap)
           (java.util.function Function)
           (javax.net.ssl KeyManagerFactory
                          SSLContext
                          SSLSocketFactory
                          TrustManager
                          TrustManagerFactory
                          X509TrustManager)
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

(def ^:private secure-rng (delay (SecureRandom.))) ; delayed: keeps the RNG out of the native-image heap

(def ^:private day-ms 86400000)
(def ^:private validity-ms (* 825 (long day-ms))) ; 825 days: the CA/browser leaf ceiling.

(defn- gen-keypair
  "A fresh 2048-bit RSA key pair."
  ^KeyPair []
  (.generateKeyPair (doto (KeyPairGenerator/getInstance "RSA") (.initialize 2048))))

(defn- rand-serial ^BigInteger [] (BigInteger. 159 ^SecureRandom @secure-rng))

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
                                         (Date. (- now (long day-ms)))
                                         (Date. (+ now (long validity-ms)))
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
     (GeneralNames. ^"[Lorg.bouncycastle.asn1.x509.GeneralName;"
                    (into-array GeneralName
                                [(GeneralName. (if ip? GeneralName/iPAddress GeneralName/dNSName)
                                               host)]))

     builder
     (doto (JcaX509v3CertificateBuilder. issuer
                                         (rand-serial)
                                         (Date. (- now (long day-ms)))
                                         (Date. (+ now (long validity-ms)))
                                         subject
                                         (.getPublic leaf-kp))
       (.addExtension Extension/basicConstraints true (BasicConstraints. false))
       (.addExtension Extension/keyUsage
                      true
                      (KeyUsage. (int (bit-or KeyUsage/digitalSignature KeyUsage/keyEncipherment))))
       (.addExtension Extension/extendedKeyUsage
                      false
                      (ExtendedKeyUsage. ^"[Lorg.bouncycastle.asn1.x509.KeyPurposeId;"
                                         (into-array KeyPurposeId [KeyPurposeId/id_kp_serverAuth])))
       (.addExtension Extension/subjectAlternativeName false san))]

    (->x509 (.build builder (signer ca-key)))))

(defn- host-context
  "A server-mode SSLContext that presents a freshly minted leaf for `host`."
  ^SSLContext
  [^X500Name ca-name ^X509Certificate ca-cert ^PrivateKey ca-key ^KeyPair leaf-kp ^String host]
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
                         (.init nil (into-array TrustManager [tm]) ^SecureRandom @secure-rng)))))

(defn- default-root-certificates
  "Return the JVM's current default trust anchors. The child trust bundle includes
   them so explicitly tunnelled domains still validate their real certificates."
  []
  (let
    [tmf
     (TrustManagerFactory/getInstance (TrustManagerFactory/getDefaultAlgorithm))

     ^KeyStore no-explicit-store
     nil]

    (.init tmf no-explicit-store)
    (->> (.getTrustManagers tmf)
         (filter #(instance? X509TrustManager %))
         (mapcat #(seq (.getAcceptedIssuers ^X509TrustManager %)))
         (reduce (fn [m ^X509Certificate cert]
                   (assoc m (str (.getSubjectX500Principal cert)) cert))
                 {})
         vals
         vec)))

(defn- write-ca-pem
  "Write a PEM trust bundle with the ephemeral vis CA first and the JVM's default
   roots after it. CA-env-aware clients can therefore validate both MITM leaves and
   real certificates for explicitly tunnelled domains."
  ^String [^X509Certificate cert roots]
  (let
    [encoder
     (Base64/getMimeEncoder 64 (.getBytes "\n"))

     pem
     (apply str
       (for [^X509Certificate c (cons cert roots)]
         (str "-----BEGIN CERTIFICATE-----\n"
              (.encodeToString encoder (.getEncoded c))
              "\n-----END CERTIFICATE-----\n")))

     f
     (File/createTempFile "vis-ca-" ".pem" (File. (System/getProperty "java.io.tmpdir")))]

    (.deleteOnExit f)
    (spit f pem)
    (.getAbsolutePath f)))

(defn- write-java-truststore
  "Write an ephemeral PKCS12 truststore containing the vis CA plus the JVM's
   default roots. Managed JVM children receive it through JAVA_TOOL_OPTIONS;
   no host trust store is modified."
  [^X509Certificate cert roots]
  (let
    [file
     (File/createTempFile "vis-ca-" ".p12")

     password
     (str (UUID/randomUUID))

     chars
     (.toCharArray password)

     store
     (KeyStore/getInstance "PKCS12")]

    (.load store nil chars)
    (.setCertificateEntry store "vis-egress-ca" cert)
    (doseq [[idx ^X509Certificate root] (map-indexed vector roots)]
      (.setCertificateEntry store (str "system-root-" idx) root))
    (with-open [out (FileOutputStream. file)]
      (.store store out chars))
    (.deleteOnExit file)
    {:java-trust-store (.getAbsolutePath file) :java-trust-store-password password}))

(defn create!
  "Build the gateway MITM capability. Returns a map:
     :ca-cert                    ephemeral CA certificate
     :ca-file                    CA PEM for curl/Python/Bun trust env
     :java-trust-store           PKCS12 path for managed JVM children
     :java-trust-store-password  random password for that ephemeral store
     :ctx-for                    (fn [host] -> server SSLContext), cached per host
     :upstream-factory           proxy->real-server SSLSocketFactory
     :close!                     delete both ephemeral trust files

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

      roots
      (default-root-certificates)

      ca-file
      (write-ca-pem cert roots)

      java-trust
      (write-java-truststore cert roots)]

     (merge {:ca-cert cert
             :ca-file ca-file
             :ctx-for ctx-for
             :upstream-factory
             (if upstream-trust-all? (trust-all-factory) (SSLSocketFactory/getDefault))
             :close! (fn []
                       (doseq [path [ca-file (:java-trust-store java-trust)]]
                         (try (.delete (File. ^String path)) (catch Throwable _ nil))))}
            java-trust))))
