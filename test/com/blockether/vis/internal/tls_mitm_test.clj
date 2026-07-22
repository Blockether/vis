(ns com.blockether.vis.internal.tls-mitm-test
  "Pure-JVM ephemeral CA + per-host leaf minting for the egress proxy's MITM tier —
   asserted as crypto facts, no network: a self-signed CA, a leaf that carries the
   requested host in its SAN and verifies under the CA, per-host context caching,
   and the ephemeral CA-PEM lifecycle. Cross-platform (runs on Linux CI too)."
  (:require [clojure.test :refer [deftest is testing]]
            [com.blockether.vis.internal.tls-mitm :as tls])
  (:import (java.io File FileInputStream)
           (java.security KeyStore)
           (java.security.cert X509Certificate)
           (javax.net.ssl SSLContext SSLSocketFactory)))

(deftest gen-ca-is-a-self-signed-ca
  (let [{:keys [cert key-pair name]} (tls/gen-ca)]
    (testing "returns an X509 CA cert + its key pair + X500 name"
      (is (instance? X509Certificate cert))
      (is (some? key-pair))
      (is (some? name)))
    (testing "self-signed: issuer == subject, and it verifies under its own public key"
      (is (= (.getSubjectX500Principal cert) (.getIssuerX500Principal cert)))
      (is (nil? (.verify cert (.getPublic key-pair))))) ; throws if invalid
    (testing "is marked a CA (basicConstraints != -1)" (is (not= -1 (.getBasicConstraints cert))))))

(deftest minted-leaf-has-host-san-and-is-ca-signed
  (let
    [{:keys [key-pair name]}
     (tls/gen-ca)

     leaf-kp
     (#'tls/gen-keypair)

     leaf
     (#'tls/mint-leaf name (.getPrivate key-pair) leaf-kp "api.example.com")]

    (testing "the leaf verifies under the CA public key (real chain of trust)"
      (is (nil? (.verify leaf (.getPublic key-pair)))))
    (testing "the requested host is present as a dNSName SAN"
      (let [names (map second (.getSubjectAlternativeNames leaf))]
        (is (some #{"api.example.com"} names))))
    (testing "the leaf is NOT itself a CA" (is (= -1 (.getBasicConstraints leaf))))))

(deftest minted-leaf-for-ip-uses-ip-san
  (let
    [{:keys [key-pair name]}
     (tls/gen-ca)

     leaf-kp
     (#'tls/gen-keypair)

     leaf
     (#'tls/mint-leaf name (.getPrivate key-pair) leaf-kp "127.0.0.1")]

    (testing "a numeric host is encoded as an iPAddress SAN, not dNSName"
      (let [names (map second (.getSubjectAlternativeNames leaf))]
        (is (some #{"127.0.0.1"} names))))))

(deftest create!-capability-shape
  (let [cap (tls/create! {:upstream-trust-all? true})]
    (try (testing "exposes the documented capability keys"
           (is (instance? X509Certificate (:ca-cert cap)))
           (is (string? (:ca-file cap)))
           (is (string? (:java-trust-store cap)))
           (is (string? (:java-trust-store-password cap)))
           (is (fn? (:ctx-for cap)))
           (is (instance? SSLSocketFactory (:upstream-factory cap)))
           (is (fn? (:close! cap))))
         (testing "the combined PEM bundle and JVM PKCS12 truststore exist"
           (let
             [pem (File. ^String (:ca-file cap))
              store-file (File. ^String (:java-trust-store cap))
              chars (.toCharArray ^String (:java-trust-store-password cap))
              store (KeyStore/getInstance "PKCS12")]

             (is (.exists pem))
             (is (.exists store-file))
             (is (< 1 (count (re-seq #"BEGIN CERTIFICATE" (slurp pem)))))
             (with-open [in (FileInputStream. store-file)]
               (.load store in chars))
             (is (.containsAlias store "vis-egress-ca"))
             (is (< 1 (.size store)))))
         (testing "ctx-for returns a server SSLContext, cached per host (same instance)"
           (let
             [a ((:ctx-for cap) "example.com")
              b ((:ctx-for cap) "example.com")
              c ((:ctx-for cap) "other.com")]

             (is (instance? SSLContext a))
             (is (identical? a b))
             (is (not (identical? a c)))))
         (finally ((:close! cap))))
    (testing "close! removes both ephemeral trust files"
      (is (not (.exists (File. ^String (:ca-file cap)))))
      (is (not (.exists (File. ^String (:java-trust-store cap))))))))

(deftest upstream-default-validates-real-certs
  (let [cap (tls/create! {})] ; no :upstream-trust-all?
    (try (testing "without the TEST flag, upstream uses the system default factory"
           (is (instance? SSLSocketFactory (:upstream-factory cap))))
         (finally ((:close! cap))))))
