(ns com.blockether.vis.internal.tls
  "Outbound HTTPS trust bootstrap for corporate TLS-intercepting proxies.

   Zscaler (and similar proxies) re-sign every upstream certificate with
   a private root CA that the JVM truststore does not ship. Every
   provider/channel HTTP client in Vis - anthropic, openai, copilot,
   zai, telegram, exa, foundation-search, and the TUI provider - is
   built on `java.net.http.HttpClient` (directly or via babashka
   http-client). All of those fall back to `SSLContext/getDefault` when
   no explicit context is supplied, so a single `SSLContext/setDefault`
   at startup teaches the whole process to trust the extra CA(s) with no
   per-client wiring.

   We never DISABLE verification. We ADD the corporate root(s) to the
   platform defaults through a composite `X509TrustManager`: the
   platform trust manager is consulted first and the extra-CA trust
   manager only as a fallback, so genuinely-unknown certificates still
   fail the handshake.

   CA sources, in order, de-duplicated by absolute path:
     1. JVM property `vis.ca-bundle`
     2. env vars: ZSCALER_CA_BUNDLE, NODE_EXTRA_CA_CERTS,
        REQUESTS_CA_BUNDLE, SSL_CERT_FILE, CURL_CA_BUNDLE, AWS_CA_BUNDLE
     3. drop-in dir `~/.vis/certs/` (*.pem, *.crt, *.cer)

   When no extra CA is found the function is a no-op and the JVM keeps
   its stock truststore."
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [taoensso.telemere :as tel])
  (:import
   (java.io File)
   (java.security KeyStore)
   (java.security.cert CertificateFactory X509Certificate)
   (javax.net.ssl SSLContext TrustManager TrustManagerFactory X509TrustManager)))

(def ^:private ca-env-vars
  ["ZSCALER_CA_BUNDLE" "NODE_EXTRA_CA_CERTS" "REQUESTS_CA_BUNDLE"
   "SSL_CERT_FILE" "CURL_CA_BUNDLE" "AWS_CA_BUNDLE"])

(def ^:private cert-extensions #{"pem" "crt" "cer"})

(defonce ^:private installed (atom false))

(defn certs-dir
  "Drop-in directory for corporate CA certificates: `~/.vis/certs`."
  ^File []
  (io/file (System/getProperty "user.home") ".vis" "certs"))

(defn- readable-file
  "Coerce `x` to a `File` and return it only when it is a readable file."
  ^File [x]
  (when (some? x)
    (let [s (str/trim (str x))]
      (when-not (str/blank? s)
        (let [f (io/file s)]
          (when (and (.isFile f) (.canRead f)) f))))))

(defn- cert-ext? [^File f]
  (let [n (str/lower-case (.getName f))
        i (str/last-index-of n ".")]
    (and i (contains? cert-extensions (subs n (inc i))))))

(defn- drop-in-files
  "Readable *.pem/*.crt/*.cer files under `~/.vis/certs`, sorted by name."
  []
  (let [dir (certs-dir)]
    (when (.isDirectory dir)
      (->> (.listFiles dir)
        (filter #(and (.isFile ^File %) (.canRead ^File %) (cert-ext? %)))
        (sort-by #(.getName ^File %))))))

(defn ca-sources
  "Ordered, de-duplicated seq of readable CA bundle `File`s from JVM
   property, env vars, and the `~/.vis/certs` drop-in directory."
  []
  (let [candidates (concat
                     [(readable-file (System/getProperty "vis.ca-bundle"))]
                     (map #(readable-file (System/getenv %)) ca-env-vars)
                     (drop-in-files))]
    (->> candidates
      (remove nil?)
      (reduce (fn [[seen acc] ^File f]
                (let [p (.getAbsolutePath f)]
                  (if (contains? seen p)
                    [seen acc]
                    [(conj seen p) (conj acc f)])))
        [#{} []])
      second)))

(defn- x509-of
  "First `X509TrustManager` produced by `tmf`, or nil."
  ^X509TrustManager [^TrustManagerFactory tmf]
  (some (fn [^TrustManager tm]
          (when (instance? X509TrustManager tm) tm))
    (.getTrustManagers tmf)))

(defn- platform-trust-manager
  "Platform-default `X509TrustManager` (the stock JVM truststore).
   Initializing a `TrustManagerFactory` with a nil `KeyStore` loads the
   JVM's default trust material."
  ^X509TrustManager []
  (let [tmf       (TrustManagerFactory/getInstance (TrustManagerFactory/getDefaultAlgorithm))
        ^KeyStore default-ks nil]
    (.init tmf default-ks)
    (x509-of tmf)))

(defn- extra-trust-manager
  "`X509TrustManager` backed by a keystore loaded from `files`, plus the
   total count of certificates loaded. Returns `[tm n]` or nil when no
   certificate could be read."
  [files]
  (let [ks (doto (KeyStore/getInstance (KeyStore/getDefaultType))
             (.load nil nil))
        cf (CertificateFactory/getInstance "X.509")
        n  (volatile! 0)]
    (doseq [^File f files]
      (try
        (with-open [in (io/input-stream f)]
          (doseq [cert (.generateCertificates cf in)]
            (.setCertificateEntry ks (str "vis-ca-" @n) cert)
            (vswap! n inc)))
        (catch Throwable t
          (tel/log! {:level :warn :id ::ca-load-failed}
            (str "Skipped unreadable CA bundle " (.getAbsolutePath f) ": " (ex-message t))))))
    (when (pos? @n)
      (let [tmf (TrustManagerFactory/getInstance (TrustManagerFactory/getDefaultAlgorithm))]
        (.init tmf ks)
        [(x509-of tmf) @n]))))

(defn- composite-trust-manager
  "Trust manager that accepts a chain trusted by EITHER `platform` or
   `extra`. Platform is tried first; `extra` is the corporate-CA
   fallback. Client-auth and accepted-issuers delegate to both."
  ^X509TrustManager [^X509TrustManager platform ^X509TrustManager extra]
  (reify X509TrustManager
    (checkClientTrusted [_ chain auth-type]
      (.checkClientTrusted platform chain auth-type))
    (checkServerTrusted [_ chain auth-type]
      (try
        (.checkServerTrusted platform chain auth-type)
        (catch java.security.cert.CertificateException _
          (.checkServerTrusted extra chain auth-type))))
    (getAcceptedIssuers [_]
      (into-array X509Certificate
        (concat (seq (.getAcceptedIssuers platform))
          (seq (.getAcceptedIssuers extra)))))))

(defn install!
  "Install a process-wide default `SSLContext` that trusts the platform
   CAs plus any corporate root CA(s) discovered via `ca-sources`.

   No-op (returns nil) when no extra CA is found or trust is already
   installed. Idempotent. Returns a summary map
   `{:certs n :sources [paths...]}` on success.

   Safe to call early in `-main`: failures are logged and swallowed so a
   broken CA file can never prevent the process from starting (it just
   falls back to the stock truststore)."
  []
  (when (compare-and-set! installed false true)
    (try
      (let [files (ca-sources)]
        (if (empty? files)
          (do (tel/log! {:level :debug :id ::no-extra-ca}
                "No corporate CA bundle found; using platform truststore.")
            nil)
          (if-let [[extra n] (extra-trust-manager files)]
            (let [platform (platform-trust-manager)
                  ctx      (SSLContext/getInstance "TLS")
                  paths    (mapv #(.getAbsolutePath ^File %) files)]
              (.init ctx nil
                (into-array TrustManager [(composite-trust-manager platform extra)])
                nil)
              (SSLContext/setDefault ctx)
              (tel/log! {:level :info :id ::installed
                         :data {:certs n :sources paths}}
                (str "Installed corporate TLS trust: " n " cert(s) from "
                  (count paths) " source(s)."))
              {:certs n :sources paths})
            (do (tel/log! {:level :warn :id ::no-certs-loaded}
                  "Corporate CA source(s) present but no certificate could be loaded.")
              nil))))
      (catch Throwable t
        ;; Never let trust bootstrap crash startup - fall back to stock truststore.
        (reset! installed false)
        (tel/log! {:level :warn :id ::install-failed}
          (str "Corporate TLS trust bootstrap failed: " (ex-message t)))
        nil))))
