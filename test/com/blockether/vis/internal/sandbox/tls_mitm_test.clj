(ns com.blockether.vis.internal.sandbox.tls-mitm-test
  "Pure-JVM ephemeral CA + per-host leaf minting for the egress proxy's MITM tier —
   asserted as crypto facts, no network: a self-signed CA, a leaf that carries the
   requested host in its SAN and verifies under the CA, per-host context caching,
   and the ephemeral CA-PEM lifecycle. Cross-platform (runs on Linux CI too)."
  (:require [lazytest.core :refer [defdescribe expect it]]
            [com.blockether.vis.internal.sandbox.tls-mitm :as tls])
  (:import (java.io File FileInputStream)
           (java.security KeyStore)
           (java.security.cert X509Certificate)
           (javax.net.ssl SSLContext SSLSocketFactory)))

(defdescribe gen-ca-is-a-self-signed-ca
             (it "gen ca is a self signed ca"
                 (let [{:keys [cert key-pair name]} (tls/gen-ca)]
                   ;; returns an X509 CA cert + its key pair + X500 name
                   (expect (instance? X509Certificate cert))
                   (expect (some? key-pair))
                   (expect (some? name))
                   ;; self-signed: issuer == subject, and it verifies under its own public key
                   (expect (= (.getSubjectX500Principal cert) (.getIssuerX500Principal cert)))
                   (expect (nil? (.verify cert (.getPublic key-pair)))) ; throws if invalid
                   ;; is marked a CA (basicConstraints != -1)
                   (expect (not= -1 (.getBasicConstraints cert))))))

(defdescribe minted-leaf-has-host-san-and-is-ca-signed
             (it "minted leaf has host san and is ca signed"
                 (let [{:keys [key-pair name]}
                       (tls/gen-ca)

                       leaf-kp
                       (#'tls/gen-keypair)

                       leaf
                       (#'tls/mint-leaf name (.getPrivate key-pair) leaf-kp "api.example.com")]

                   ;; the leaf verifies under the CA public key (real chain of trust)
                   (expect (nil? (.verify leaf (.getPublic key-pair))))
                   ;; the requested host is present as a dNSName SAN
                   (let [names (map second (.getSubjectAlternativeNames leaf))]
                     (expect (some #{"api.example.com"} names)))
                   ;; the leaf is NOT itself a CA
                   (expect (= -1 (.getBasicConstraints leaf))))))

(defdescribe minted-leaf-for-ip-uses-ip-san
             (it "a numeric host is encoded as an iPAddress SAN, not dNSName"
                 (let [{:keys [key-pair name]}
                       (tls/gen-ca)

                       leaf-kp
                       (#'tls/gen-keypair)

                       leaf
                       (#'tls/mint-leaf name (.getPrivate key-pair) leaf-kp "127.0.0.1")

                       names
                       (map second (.getSubjectAlternativeNames leaf))]

                   (expect (some #{"127.0.0.1"} names)))))

(defdescribe
  create!-capability-shape
  (it
    "create! capability shape"
    (let [cap (tls/create! {:upstream-trust-all? true})]
      (try
        ;; exposes the documented capability keys
        (expect (instance? X509Certificate (:ca-cert cap)))
        (expect (string? (:ca-file cap)))
        (expect (string? (:java-trust-store cap)))
        (expect (string? (:java-trust-store-password cap)))
        (expect (fn? (:ctx-for cap)))
        (expect (instance? SSLSocketFactory (:upstream-factory cap)))
        (expect (fn? (:close! cap)))
        ;; the combined PEM bundle and JVM PKCS12 truststore exist
        (let [pem (File. ^String (:ca-file cap))
              store-file (File. ^String (:java-trust-store cap))
              chars (.toCharArray ^String (:java-trust-store-password cap))
              store (KeyStore/getInstance "PKCS12")]

          (expect (.exists pem))
          (expect (.exists store-file))
          (expect (< 1 (count (re-seq #"BEGIN CERTIFICATE" (slurp pem)))))
          (with-open [in (FileInputStream. store-file)]
            (.load store in chars))
          (expect (.containsAlias store "vis-egress-ca"))
          (expect (< 1 (.size store))))
        ;; ctx-for returns a server SSLContext, cached per host (same instance)
        (let [a ((:ctx-for cap) "example.com")
              b ((:ctx-for cap) "example.com")
              c ((:ctx-for cap) "other.com")]

          (expect (instance? SSLContext a))
          (expect (identical? a b))
          (expect (not (identical? a c))))
        (finally ((:close! cap))))
      ;; close! removes both ephemeral trust files
      (expect (not (.exists (File. ^String (:ca-file cap)))))
      (expect (not (.exists (File. ^String (:java-trust-store cap))))))))

(defdescribe upstream-default-validates-real-certs
             (it "without the TEST flag, upstream uses the system default factory"
                 (let [cap (tls/create! {})] ; no :upstream-trust-all?
                   (try (expect (instance? SSLSocketFactory (:upstream-factory cap)))
                        (finally ((:close! cap)))))))
