(ns com.blockether.vis.internal.system-trust-test
  (:require [clojure.java.io :as io]
            [com.blockether.vis.internal.system-trust :as system-trust]
            [com.blockether.vis.internal.tls-mitm :as tls-mitm]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]
           [java.security.cert X509Certificate]
           [java.util Base64]))

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
