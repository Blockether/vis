(ns com.blockether.vis.graalvm-truststore-test
  "A downloaded JDK trusts the public roots and NOTHING else.

   Behind a TLS-intercepting corporate proxy that turns every later network
   step — dependency resolution, `native-image`, the JDK download itself —
   into `SunCertPathBuilderException: unable to find valid certification path`,
   while the system JDK keeps working because its cacerts was patched by the
   corporate installer. `bin/require-graalvm` therefore accepts VIS_CA_CERT (a
   PEM) or VIS_TRUSTSTORE (a ready keystore) and is the SINGLE owner of that
   policy: build.clj only forwards what `--truststore` prints.

   These tests drive the real script with a self-signed CA, because the two
   properties that matter cannot be read off the source: the generated store
   must ALSO keep the public roots (or every ordinary download breaks), and the
   JDK's own cacerts must come out untouched (a patched cacerts is silently
   lost on the next reinstall)."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(def ^:private jdk-home (System/getProperty "java.home"))

(defn- keytool-path [] (.getAbsolutePath (io/file jdk-home "bin" "keytool")))

(defn- run
  "Run `command` with `env` added, capturing both streams."
  [command env]
  (let [pb (ProcessBuilder. ^java.util.List (vec command))]
    (.putAll (.environment pb) ^java.util.Map env)
    (.directory pb (io/file (System/getProperty "user.dir")))
    (let
      [proc (.start pb)
       out (slurp (.getInputStream proc))
       err (slurp (.getErrorStream proc))]

      {:exit (.waitFor proc) :out out :err err})))

(defn- keytool! [args] (run (into [(keytool-path)] args) {}))

(defn- entry-count
  "How many entries a keystore holds, as keytool reports them."
  [store password]
  (let [{:keys [out]} (keytool! ["-list" "-keystore" (str store) "-storepass" password])]
    (some-> (re-find #"contains (\d+) entr" out)
            second
            parse-long)))

(defn- sha256
  [file]
  (let [digest (java.security.MessageDigest/getInstance "SHA-256")]
    (str/join (map #(format "%02x" %)
                   (.digest digest (Files/readAllBytes (.toPath (io/file file))))))))

(def ^:private fixture
  "One temp dir, one self-signed CA, one cache — the store is built once and the
   rest of the suite reads it (keytool costs a JVM start apiece)."
  (delay
    (let
      [root
       (.toFile (Files/createTempDirectory "vis-truststore-test-" (make-array FileAttribute 0)))

       pem
       (io/file root "corporate-ca.pem")

       own
       (io/file root "own-store.p12")

       cache
       (io/file root "cache")]

      (keytool! ["-genkeypair" "-alias" "fake" "-dname" "CN=Vis Fake Corp CA" "-keyalg" "RSA"
                 "-keysize" "2048" "-validity" "1" "-keystore" (.getPath own) "-storepass"
                 "changeit" "-storetype" "PKCS12"])
      (spit pem
            (:out (keytool! ["-exportcert" "-rfc" "-alias" "fake" "-keystore" (.getPath own)
                             "-storepass" "changeit"])))
      {:root root
       :pem pem
       :own own
       :env {"XDG_CACHE_HOME" (.getPath cache) "VIS_CA_CERT" (.getPath pem)}})))

(defn- script [args env] (run (into ["bash" "bin/require-graalvm"] args) env))

(defdescribe graalvm-truststore-test
             (it "imports a PEM into a cached store that ALSO keeps the public roots"
                 (let
                   [{:keys [pem env]}
                    @fixture

                    cacerts
                    (io/file jdk-home "lib" "security" "cacerts")

                    before
                    (sha256 cacerts)

                    {:keys [exit out]}
                    (script ["--truststore"] env)

                    store
                    (str/trim out)]

                   (expect (zero? exit))
                   (expect (str/ends-with? store ".p12"))
                   (expect (.isFile (io/file store)))
                   ;; the JDK is NEVER modified — that is the whole point of the copy
                   (expect (= before (sha256 cacerts)))
                   (let
                     [listing (:out (keytool! ["-list" "-keystore" store "-storepass" "changeit"
                                               "-alias" "vis-custom-ca-1"]))]
                     (expect (str/includes? listing "trustedCertEntry")))
                   (expect (= (inc (entry-count cacerts "changeit")) (entry-count store "changeit"))
                           (str "the generated store must be cacerts PLUS the corporate CA, got "
                                (entry-count store "changeit")))
                   (expect (str/includes? (slurp pem) "BEGIN CERTIFICATE"))))
             (it "reuses the cached store instead of re-importing on every call"
                 (let
                   [{:keys [env]}
                    @fixture

                    first-run
                    (script ["--truststore"] env)

                    second-run
                    (script ["--truststore"] env)]

                   (expect (= (str/trim (:out first-run)) (str/trim (:out second-run))))
                   (expect (not (str/includes? (:err second-run) "imported")))))
             (it "exports the truststore through JAVA_TOOL_OPTIONS, which every forked JVM reads"
                 (let
                   [{:keys [pem env]}
                    @fixture

                    {:keys [exit out]}
                    (script ["--export"] env)]

                   (expect (zero? exit))
                   (expect (re-find #"export JAVA_TOOL_OPTIONS=.*-Djavax\.net\.ssl\.trustStore="
                                    out))
                   (expect (str/includes? out "-Djavax.net.ssl.trustStoreType=PKCS12"))
                   ;; curl and every OpenSSL client on that shell need the PEM, not the keystore
                   (expect (str/includes? out (str "export CURL_CA_BUNDLE=" (.getPath pem))))
                   (expect (str/includes? out (str "export SSL_CERT_FILE=" (.getPath pem))))
                   ;; an existing JAVA_TOOL_OPTIONS is preserved, ours only appends
                   (expect (str/includes? out "${JAVA_TOOL_OPTIONS:+$JAVA_TOOL_OPTIONS }"))))
             (it "uses a supplied keystore verbatim — no conversion, no cache"
                 (let
                   [{:keys [own env]}
                    @fixture

                    {:keys [exit out]}
                    (script ["--truststore"] (assoc env "VIS_TRUSTSTORE" (.getPath own)))]

                   (expect (zero? exit))
                   (expect (= (.getPath own) (str/trim out)))))
             (it "stays silent when nothing custom is configured"
                 (let
                   [{:keys [env]}
                    @fixture

                    {:keys [exit out]}
                    (script ["--truststore"] (dissoc env "VIS_CA_CERT"))]

                   (expect (zero? exit))
                   (expect (str/blank? out))))
             (it "refuses an unreadable VIS_CA_CERT by name instead of failing later on TLS"
                 (let
                   [{:keys [env]}
                    @fixture

                    {:keys [exit err]}
                    (script ["--truststore"] (assoc env "VIS_CA_CERT" "/nope/corporate-ca.pem"))]

                   (expect (= 1 exit))
                   (expect (str/includes? err "/nope/corporate-ca.pem"))))
             (it "documents the corporate-CA switches in its own --help"
                 (let [{:keys [out]} (script ["--help"] {})]
                   (expect (str/includes? out "VIS_CA_CERT"))
                   (expect (str/includes? out "VIS_TRUSTSTORE"))
                   (expect (str/includes? out "--truststore")))))
