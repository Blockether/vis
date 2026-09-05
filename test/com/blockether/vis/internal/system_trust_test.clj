(ns com.blockether.vis.internal.system-trust-test
  (:require [clojure.java.io :as io]
            [com.blockether.vis.internal.system-trust :as system-trust]
            [com.blockether.vis.internal.sandbox.tls-mitm :as tls-mitm]
            [com.blockether.vis-python-runtime :as runtime]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]
           [java.security.cert X509Certificate]
           [java.util Base64]
           [javax.net.ssl SSLContext]))

(defdescribe
  pem-context-test
  (it
    "merges a PEM certificate with the JVM default trust"
    (let [tmp
          (.toFile (Files/createTempDirectory "vis-system-trust" (make-array FileAttribute 0)))

          pem
          (io/file tmp "windows-roots.pem")

          ^X509Certificate cert
          (:cert (tls-mitm/gen-ca))

          encoded
          (.encodeToString (Base64/getMimeEncoder 64 (byte-array [(byte 10)])) (.getEncoded cert))]

      (try (spit pem (str "-----BEGIN CERTIFICATE-----\n" encoded "\n-----END CERTIFICATE-----\n"))
           (let [manager (system-trust/trust-manager-for-pem (.getAbsolutePath pem))]
             (expect (some #(java.util.Arrays/equals (.getEncoded ^X509Certificate %)
                                                     (.getEncoded cert))
                           (.getAcceptedIssuers manager))))
           (finally (io/delete-file pem true) (io/delete-file tmp true))))))

;; Regression: host PEM roots did not reach pip's independently exported trust store.
(defdescribe
  pip-shares-host-trust-test
  (it
    "exports the additional host CA for pip"
    (let [tmp
          (.toFile (Files/createTempDirectory "vis-pip-trust" (make-array FileAttribute 0)))

          pem
          (io/file tmp "root.pem")

          bundle
          (io/file tmp "pip.pem")

          original
          (SSLContext/getDefault)

          ^X509Certificate cert
          (:cert (tls-mitm/gen-ca))

          encoded
          (.encodeToString (Base64/getMimeEncoder 64 (byte-array [(byte 10)])) (.getEncoded cert))]

      (try (spit pem (str "-----BEGIN CERTIFICATE-----\n" encoded "\n-----END CERTIFICATE-----\n"))
           (system-trust/install! (.getAbsolutePath pem))
           (runtime/certificates-pem! (.getAbsolutePath bundle))
           (expect (.contains (slurp bundle) encoded))
           (finally (SSLContext/setDefault original)
                    (doseq [f [bundle pem tmp]]
                      (io/delete-file f true)))))))
