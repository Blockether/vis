(ns com.blockether.vis.internal.gateway.fcm-test
  "Android push (FCM HTTP v1): credential resolution, the RS256 service-account
   assertion, the message shape, and the platform dispatch in `gateway.push`.

   Every test redirects the push home (`vis.push.home`) at a temp dir, which also
   disables keychain reads — the developer's real credentials can never leak in."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is testing]]
            [com.blockether.vis.internal.gateway.fcm :as fcm]
            [com.blockether.vis.internal.gateway.push :as push])
  (:import [java.security KeyPairGenerator Signature]
           [java.util Base64]))

(defn- temp-home
  ^java.io.File []
  (let [d (io/file (System/getProperty "java.io.tmpdir") (str "vis-fcm-test-" (System/nanoTime)))]
    (.mkdirs (io/file d "fcm"))
    d))

(defmacro with-push-home
  [binding & body]
  `(let
     [~(first binding)
      (temp-home)

      prev#
      (System/getProperty "vis.push.home")]

     (try (System/setProperty "vis.push.home" (.getAbsolutePath ~(first binding)))
          (push/reload-devices!)
          ~@body
          (finally (if prev#
                     (System/setProperty "vis.push.home" prev#)
                     (System/clearProperty "vis.push.home"))
                   (push/reload-devices!)))))

(defn- write-service-account!
  "Write a real RSA service-account JSON where the gateway looks, and return the
   public key its assertion can be verified against."
  [home]
  (let
    [kp
     (.generateKeyPair (doto (KeyPairGenerator/getInstance "RSA") (.initialize 2048)))

     pem
     (str "-----BEGIN PRIVATE KEY-----\n"
          (.encodeToString (Base64/getMimeEncoder) (.getEncoded (.getPrivate kp)))
          "\n-----END PRIVATE KEY-----\n")

     json
     (str "{\"type\":\"service_account\",\"project_id\":\"vis-test-proj\","
          "\"client_email\":\"pusher@vis-test-proj.iam.gserviceaccount.com\","
          "\"private_key\":"
          (pr-str pem)
          "}")]

    (spit (io/file home "fcm" "service-account.json") json)
    (.getPublic kp)))

(deftest config-reports-what-is-missing
  #_{:clj-kondo/ignore [:unresolved-symbol]}
  (with-push-home [home]
                  (testing "an empty home cannot push to Android, and says exactly why"
                    (let [cfg (fcm/config)]
                      (is (false? (:is-configured cfg)))
                      (is (false? (fcm/configured?)))
                      (is (= ["service_account" "client_email" "project_id"] (:missing cfg)))
                      (is (nil? (:project-id cfg)))))
                  (testing "a service-account JSON under ~/.vis/fcm/ configures it"
                    (write-service-account! home)
                    (let [cfg (fcm/config)]
                      (is (true? (:is-configured cfg)))
                      (is (= "vis-test-proj" (:project-id cfg)))
                      (is (= "pusher@vis-test-proj.iam.gserviceaccount.com" (:client-email cfg)))
                      (is (= "file" (:source cfg)))
                      (is (empty? (:missing cfg)))))
                  (testing "config never returns key material"
                    (is (not (str/includes? (pr-str (fcm/config)) "PRIVATE KEY"))))))

(deftest assertion-is-a-verifiable-rs256-jwt
  (with-push-home [home]
                  (let
                    [pub
                     (write-service-account! home)

                     sa
                     (#'fcm/service-account)

                     jwt
                     (#'fcm/sign-jwt sa)

                     [h c s]
                     (str/split jwt #"\.")

                     decode
                     #(String. (.decode (Base64/getUrlDecoder) ^String %) "UTF-8")]

                    (is (str/includes? (decode h) "\"RS256\""))
                    (is (str/includes? (decode c) "firebase.messaging"))
                    (is (str/includes? (decode c) "pusher@vis-test-proj.iam.gserviceaccount.com"))
                    (is (str/includes? (decode c) "oauth2.googleapis.com"))
                    (is (true? (.verify (doto (Signature/getInstance "SHA256withRSA")
                                          (.initVerify pub)
                                          (.update (.getBytes (str h "." c) "UTF-8")))
                                        (.decode (Base64/getUrlDecoder) ^String s)))))))

(deftest message-shape-matches-fcm-v1
  (let
    [m (:message (#'fcm/message
                  "TOK"
                  {:title "Turn finished"
                   :body "vis"
                   :data {:session_id "s1" :turn_id 7}
                   :collapse-id "s1"}))]
    (is (= "TOK" (:token m)))
    (is (= {:title "Turn finished" :body "vis"} (:notification m)))
    (is (= "HIGH" (get-in m [:android :priority])))
    (is (= "s1" (get-in m [:android :collapse_key])))
    (testing "FCM rejects non-string data values, so every value is stringified"
      (is (= {"session_id" "s1" "turn_id" "7"} (:data m))))))

(deftest dead-token-detection
  (is (true? (fcm/dead-token? {:status 404 :reason "NOT_FOUND"})))
  (is (true? (fcm/dead-token? {:status 400 :reason "UNREGISTERED"})))
  (is (false? (fcm/dead-token? {:status 200 :reason ""})))
  (is (false? (fcm/dead-token? {:status 0 :reason "transport-error"}))))

(deftest send-dispatches-on-platform
  #_{:clj-kondo/ignore [:unresolved-symbol]}
  (with-push-home [_home]
                  (testing "an Android device goes to FCM, not to Apple"
                    (is (= {:status 0 :reason "not-configured" :is-delivered false}
                           (push/send-to-device! {:token "and-token" :platform "android"}
                                                 {:title "t" :body "b"}))))
                  (testing "anything that is neither Apple nor Android is never sent"
                    (is (= "unsupported-platform"
                           (:reason (push/send-to-device! {:token "web-token" :platform "web"}
                                                          {:title "t" :body "b"})))))
                  (testing "an iOS device still takes the APNs path"
                    (is (= "not-configured"
                           (:reason (push/send-to-device! {:token "ios-token" :platform "ios"}
                                                          {:title "t" :body "b"})))))))

(deftest status-exposes-both-providers
  (with-push-home [home]
                  (testing "with no credentials at all it is still push-capable — the relay"
                    (is (true? (:is-available (push/status))))
                    (is (= "relay" (:provider (push/status))))
                    (is (true? (push/any-configured?))))
                  (write-service-account! home)
                  (let [st (push/status)]
                    (testing "Android-only credentials are a valid, push-capable setup"
                      (is (true? (:is-available st)))
                      (is (true? (push/any-configured?)))
                      (is (= "fcm" (:provider st)))
                      (is (true? (get-in st [:fcm :is-available])))
                      (is (false? (get-in st [:apns :is-available]))))
                    (testing "status never leaks credentials"
                      (is (not (str/includes? (pr-str st) "PRIVATE KEY")))))))
