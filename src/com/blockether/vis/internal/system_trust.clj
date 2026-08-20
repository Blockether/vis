(ns com.blockether.vis.internal.system-trust
  "Runtime TLS trust merged from a launcher-resolved PEM certificate bundle.

   The native image and JVM both retain their configured default roots. A PEM
   bundle discovered by `bin/vis-agent` is an additional trust source, which is
   required on WSL because Windows and the Linux distribution have independent
   certificate stores."
  (:require [clojure.string :as str])
  (:import [java.io BufferedInputStream]
           [java.nio.file Files OpenOption Paths]
           [java.security KeyStore]
           [java.security.cert CertificateException CertificateFactory X509Certificate]
           [javax.net.ssl SSLContext TrustManager TrustManagerFactory X509TrustManager]))

(defn- x509-trust-manager
  ^X509TrustManager [^KeyStore key-store]
  (let [factory (TrustManagerFactory/getInstance (TrustManagerFactory/getDefaultAlgorithm))]
    (.init factory key-store)
    (or (first (filter #(instance? X509TrustManager %) (.getTrustManagers factory)))
        (throw (ex-info "No X.509 trust manager is available" {})))))

(defn- default-trust-manager ^X509TrustManager [] (x509-trust-manager nil))

(defn- pem-trust-manager
  ^X509TrustManager [^String path]
  (let [resolved (Paths/get path (make-array String 0))]
    (when-not (Files/isReadable resolved)
      (throw (ex-info "System CA bundle is not readable" {:path path})))
    (with-open [input (BufferedInputStream. (Files/newInputStream resolved
                                                                  (make-array OpenOption 0)))]
      (let [certificates (.generateCertificates (CertificateFactory/getInstance "X.509") input)
            key-store (doto (KeyStore/getInstance (KeyStore/getDefaultType)) (.load nil nil))]

        (when (.isEmpty certificates)
          (throw (ex-info "System CA bundle contains no certificates" {:path path})))
        (doseq [[index ^X509Certificate certificate] (map-indexed vector certificates)]
          (.setCertificateEntry key-store (str "system-ca-" index) certificate))
        (x509-trust-manager key-store)))))

(defn trust-manager-for-pem
  "Return an X.509 trust manager combining JVM defaults with every CA in `path`."
  ^X509TrustManager [^String path]
  (let [primary
        (default-trust-manager)

        additional
        (pem-trust-manager path)]

    (reify
      X509TrustManager
        (getAcceptedIssuers [_]
          (into-array X509Certificate
                      (concat (.getAcceptedIssuers primary) (.getAcceptedIssuers additional))))
        (checkClientTrusted [_ chain auth-type]
          (try (.checkClientTrusted primary chain auth-type)
               (catch CertificateException _ (.checkClientTrusted additional chain auth-type))))
        (checkServerTrusted [_ chain auth-type]
          (try (.checkServerTrusted primary chain auth-type)
               (catch CertificateException _ (.checkServerTrusted additional chain auth-type)))))))

(defn ssl-context-for-pem
  "Return a TLS context combining JVM defaults with every CA in `path`."
  ^SSLContext [^String path]
  (let [context
        (SSLContext/getInstance "TLS")

        manager
        (trust-manager-for-pem path)]

    (.init context nil (into-array TrustManager [manager]) nil)
    context))

(defn install!
  "Install launcher-resolved PEM trust as the process-wide TLS default, if present."
  []
  (let [path (System/getenv "VIS_SYSTEM_CA_CERT")]
    (when-not (str/blank? path) (SSLContext/setDefault (ssl-context-for-pem path)) path)))
