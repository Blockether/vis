(ns com.blockether.vis.internal.python.tls-test
  "Vis #185 and #383: nested config reaches sandbox, extension and CLI TLS handshakes."
  (:require [charred.api :as json]
            [clojure.java.io :as io]
            [com.blockether.vis.contract.config :as contract]
            [com.blockether.vis.internal.config.core :as config]
            [com.blockether.vis.internal.python.env :as env]
            [com.blockether.vis.internal.main]
            [com.blockether.vis.internal.python.extensions :as pyx]
            [com.blockether.vis.internal.python.runtime :as python-runtime]
            [com.blockether.vis.internal.python.worker :as worker]
            [com.blockether.vis.internal.sandbox.tls-mitm :as tls]
            [com.blockether.vis.internal.util :as util]
            [lazytest.core :refer [defdescribe expect it]])
  (:import (java.security KeyPair)
           (java.security.cert X509Certificate)
           (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)
           (java.util Base64)
           (org.bouncycastle.asn1.x509 BasicConstraints Extension)
           (org.bouncycastle.cert.jcajce JcaX509ExtensionUtils JcaX509v3CertificateBuilder)))

(defn- pem
  [kind bytes]
  (str "-----BEGIN "
       kind
       "-----\n"
       (.encodeToString (Base64/getMimeEncoder) ^bytes bytes)
       "\n-----END "
       kind
       "-----\n"))

(defn- certificates!
  [directory]
  (let [ca
        (tls/gen-ca)

        ^KeyPair ca-key
        (:key-pair ca)

        ^KeyPair leaf-key
        (#'tls/gen-keypair)

        extensions
        (JcaX509ExtensionUtils.)]

    (spit (io/file directory "key.pem") (pem "PRIVATE KEY" (.getEncoded (.getPrivate leaf-key))))
    (doseq [[name critical?] [["valid" true] ["legacy" false]]]
      (let [builder (doto (JcaX509v3CertificateBuilder. ^X509Certificate (:cert ca))
                      (.replaceExtension Extension/basicConstraints
                                         (boolean critical?)
                                         (BasicConstraints. true))
                      (.addExtension Extension/subjectKeyIdentifier
                                     false
                                     (.createSubjectKeyIdentifier extensions (.getPublic ca-key))))
            ^X509Certificate cert (#'tls/->x509
                                   (.build builder (#'tls/signer (.getPrivate ca-key))))]

        (spit (io/file directory (str name "-ca.pem")) (pem "CERTIFICATE" (.getEncoded cert)))
        (doseq [kind (if critical? ["valid" "expired" "signature"] ["legacy"])]
          (let [now (util/now-ms)
                leaf
                (with-redefs [util/now-ms
                              (fn ^long []
                                (long (if (= "expired" kind) (- now (* 1000 86400000)) now)))]
                  (#'tls/mint-leaf (:name ca) (.getPrivate ca-key) leaf-key "gateway.example.com"))
                leaf-builder (doto (JcaX509v3CertificateBuilder. ^X509Certificate leaf)
                               (.addExtension Extension/authorityKeyIdentifier
                                              false
                                              (.createAuthorityKeyIdentifier extensions cert))
                               (.addExtension Extension/subjectKeyIdentifier
                                              false
                                              (.createSubjectKeyIdentifier extensions
                                                                           (.getPublic leaf-key))))
                signing-key (if (= "signature" kind) (#'tls/gen-keypair) ca-key)
                ^X509Certificate signed
                (#'tls/->x509
                 (.build leaf-builder (#'tls/signer (.getPrivate ^KeyPair signing-key))))]

            (spit (io/file directory (str kind ".pem"))
                  (pem "CERTIFICATE" (.getEncoded signed)))))))))

(defdescribe
  tls-config-contract-test
  (it "tls config contract"
      (doseq [value [true false]]
        (expect (contract/definition-valid? "python" {"tls_strict" value})))
      (doseq [value [nil "false" 0 [] {}]]
        (expect (not (contract/definition-valid? "python" {"tls_strict" value})))
        (with-redefs [config/load-config-raw (constantly {"python" {"tls_strict" value}})]
          (expect (re-find #"python.tls_strict"
                           (try (python-runtime/tls-strict?)
                                "no error"
                                (catch clojure.lang.ExceptionInfo error (ex-message error)))))))
      (doseq [[global project expected] [[{} {} true] [{"tls_strict" false} {} false]
                                         [{"tls_strict" false} {"tls_strict" true} true]
                                         [{"tls_strict" true} {"tls_strict" false} false]]]
        (with-redefs [config/load-global-yaml-config-raw (constantly {"python" global})
                      config/load-global-config-raw (constantly nil)
                      config/load-project-root-config-raw (constantly {"python" project})
                      config/load-project-config-raw (constantly nil)]

          (config/invalidate-config-cache!)
          (try (expect (= expected (python-runtime/tls-strict?)))
               (when (or (seq global) (seq project))
                 (expect (= expected (get-in (config/load-config false) [:python :tls-strict]))))
               (finally (config/invalidate-config-cache!)))))))

(defdescribe
  tls-policy-crosses-both-worker-boundaries-test
  (it
    "tls policy crosses both worker boundaries"
    (let [directory
          (.toFile (Files/createTempDirectory (.toPath (doto (io/file "target") .mkdirs))
                                              "tls-policy-"
                                              (make-array FileAttribute 0)))

          project
          (.getCanonicalPath (io/file "."))

          source
          (slurp (io/resource "com/blockether/vis/internal/python/fixtures/tls_probe.py"))

          argument
          (pr-str (.getCanonicalPath directory))]

      (try (certificates! directory)
           (doseq [strict [nil true false]]
             (with-redefs [config/load-config-raw
                           (constantly (if (nil? strict) {} {"python" {"tls_strict" strict}}))]
               (let [made (env/create-python-context {}
                                                     (constantly [project])
                                                     {:worker? true
                                                      :worker-policy-fn
                                                      (constantly {:roots-fn (constantly [project])
                                                                   :net-enabled? false})
                                                      :jail-enabled? true
                                                      :enabled? false}
                                                     nil)
                     session (:python-context made)
                     extension-worker (worker/extension-worker-key session)
                     extension-context (atom nil)]

                 (try (let [sandbox (env/run-python-block
                                      session
                                      (str source "\nprint(json.dumps(tls_probe(" argument ")))"))
                            _ (expect (nil? (:error sandbox)))
                            sandbox-value (json/read-json (:stdout sandbox))
                            ctx (pyx/build-context extension-worker "tls-policy")
                            _ (reset! extension-context ctx)]

                        (pyx/bind-host! ctx "tls-policy")
                        (worker/exec! extension-worker ctx pyx/bootstrap-python)
                        (worker/exec! extension-worker ctx source)
                        ;; Invoke a sealed Python extension callable through its real Clojure adapter.
                        (let [callable
                              (#'pyx/unseal ctx (#'pyx/run-in ctx "__vis_seal__(tls_probe)"))
                              extension-value (callable [(.getCanonicalPath directory)])]

                          (expect (= sandbox-value extension-value))
                          (expect (= (not (false? strict)) (get sandbox-value "strict")))
                          (expect (true? (get sandbox-value "required")))
                          (expect (true? (get sandbox-value "hostname")))
                          (expect (= "ok" (get sandbox-value "valid")))
                          (expect (= (if (false? strict) "ok" 89) (get sandbox-value "legacy")))
                          (doseq [failure ["unknown" "wrong_host" "expired" "signature"]]
                            (expect (integer? (get sandbox-value failure)) failure))))
                      (finally (when-let [ctx @extension-context]
                                 (#'pyx/discard-context! ctx))
                               (worker/stop-worker! extension-worker)
                               (env/dispose-python-context! session))))))
           (finally (doseq [file (reverse (file-seq directory))]
                      (io/delete-file file true)))))))

(defdescribe
  tls-policy-reaches-vis-agent-python-test
  (it "vis-agent python applies the configured policy in its own process"
      ;; Vis #383: the CLI runs the interpreter in-process, so `true` after `false` must restore STRICT.
      (let [directory
            (.toFile (Files/createTempDirectory (.toPath (doto (io/file "target") .mkdirs))
                                                "tls-cli-policy-"
                                                (make-array FileAttribute 0)))

            source
            (slurp (io/resource "com/blockether/vis/internal/python/fixtures/tls_probe.py"))

            probe
            (str source
                 "\nimport json, os\nresult = tls_probe(" (pr-str (.getCanonicalPath directory))
                 ")\nresult['environment'] = os.environ.get('VIS_PYTHON_TLS_STRICT')"
                 "\nprint(json.dumps(result))")]

        (try (certificates! directory)
             (doseq [strict [false true nil]]
               (with-redefs [config/load-config-raw
                             (constantly (if (nil? strict) {} {"python" {"tls_strict" strict}}))]
                 (let [ctx ((var-get #'com.blockether.vis.internal.main/python-cli-context)
                             {:network? false :env {"VIS_PYTHON_TLS_STRICT" "false"}})]
                   (try (let [{:keys [stdout error]} (with-redefs
                                                       [config/original-stdout
                                                        (java.io.PrintStream.
                                                          (java.io.OutputStream/nullOutputStream))]
                                                       (env/run-python-block ctx probe))
                              value (json/read-json stdout)]

                          (expect (nil? error) (str error))
                          (expect (= (not (false? strict)) (get value "strict")))
                          (expect (= (str (not (false? strict))) (get value "environment")))
                          (expect (= "ok" (get value "valid")))
                          (expect (= (if (false? strict) "ok" 89) (get value "legacy")))
                          (doseq [failure ["unknown" "wrong_host" "expired" "signature"]]
                            (expect (integer? (get value failure)) failure)))
                        (finally (env/dispose-python-context! ctx))))))
             (finally (doseq [file (reverse (file-seq directory))]
                        (io/delete-file file true)))))))
