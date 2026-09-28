(ns com.blockether.vis.internal.gateway.fcm-test
  "Android push (FCM HTTP v1): credential resolution, the RS256 service-account
   assertion, the message shape, and the platform dispatch in `gateway.push`.

   Every test redirects the push home (`vis.push.home`) at a temp dir, which also
   disables keychain reads — the developer's real credentials can never leak in."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [lazytest.core :refer [defdescribe expect it]]
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
  `(let [~(first binding)
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
  (let [kp
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

(defdescribe config-reports-what-is-missing
             (it "config reports what is missing"
                 #_{:clj-kondo/ignore [:unresolved-symbol]}
                 (with-push-home
                   [home]
                   ;; an empty home cannot push to Android, and says exactly why
                   (let [cfg (fcm/config)]
                     (expect (false? (:is-configured cfg)))
                     (expect (false? (fcm/configured?)))
                     (expect (= ["service_account" "client_email" "project_id"] (:missing cfg)))
                     (expect (nil? (:project-id cfg))))
                   ;; a service-account JSON under ~/.vis/fcm/ configures it
                   (write-service-account! home)
                   (let [cfg (fcm/config)]
                     (expect (true? (:is-configured cfg)))
                     (expect (= "vis-test-proj" (:project-id cfg)))
                     (expect (= "pusher@vis-test-proj.iam.gserviceaccount.com" (:client-email cfg)))
                     (expect (= "file" (:source cfg)))
                     (expect (empty? (:missing cfg))))
                   ;; config never returns key material
                   (expect (not (str/includes? (pr-str (fcm/config)) "PRIVATE KEY"))))))

(defdescribe assertion-is-a-verifiable-rs256-jwt
             (it "assertion is a verifiable rs256 jwt"
                 (with-push-home
                   [home]
                   (let [pub
                         (write-service-account! home)

                         sa
                         (#'fcm/service-account)

                         jwt
                         (#'fcm/sign-jwt sa)

                         [h c s]
                         (str/split jwt #"\.")

                         decode
                         #(String. (.decode (Base64/getUrlDecoder) ^String %) "UTF-8")]

                     (expect (str/includes? (decode h) "\"RS256\""))
                     (expect (str/includes? (decode c) "firebase.messaging"))
                     (expect (str/includes? (decode c)
                                            "pusher@vis-test-proj.iam.gserviceaccount.com"))
                     (expect (str/includes? (decode c) "oauth2.googleapis.com"))
                     (expect (true? (.verify (doto (Signature/getInstance "SHA256withRSA")
                                               (.initVerify pub)
                                               (.update (.getBytes (str h "." c) "UTF-8")))
                                             (.decode (Base64/getUrlDecoder) ^String s))))))))

(defdescribe message-shape-matches-fcm-v1
             (it "message shape matches fcm v1"
                 (let [m (:message (#'fcm/message
                                    "TOK"
                                    {:title "Turn finished"
                                     :body "vis"
                                     :data {:session_id "s1" :turn_id 7}
                                     :thread-id "s1"
                                     :collapse-id "s1"}))]
                   (expect (= "TOK" (:token m)))
                   (expect (= {:title "Turn finished" :body "vis"} (:notification m)))
                   (expect (= "HIGH" (get-in m [:android :priority])))
                   (expect (= "s1" (get-in m [:android :collapse_key])))
                   ;; the tag is the Android badge: one live alert per session, and the
                   ;; only identity a delivered notification keeps, since Firebase builds
                   ;; the tray entry itself and never copies `data` into it
                   (expect (= {:sound "default" :tag "s1"} (get-in m [:android :notification])))
                   ;; FCM rejects non-string data values, so every value is stringified
                   (expect (= {"session_id" "s1" "turn_id" "7"} (:data m))))))

(defdescribe dead-token-detection
             (it "dead token detection"
                 (expect (true? (fcm/dead-token? {:status 404 :reason "NOT_FOUND"})))
                 (expect (true? (fcm/dead-token? {:status 400 :reason "UNREGISTERED"})))
                 (expect (false? (fcm/dead-token? {:status 200 :reason ""})))
                 (expect (false? (fcm/dead-token? {:status 0 :reason "transport-error"})))))

(defdescribe send-dispatches-on-platform
             (it "send dispatches on platform"
                 #_{:clj-kondo/ignore [:unresolved-symbol]}
                 (with-push-home
                   [_home]
                   ;; an Android device goes to FCM, not to Apple
                   (expect (= {:status 0 :reason "not-configured" :is-delivered false}
                              (push/send-to-device! {:token "and-token" :platform "android"}
                                                    {:title "t" :body "b"})))
                   ;; a browser takes the Web Push path, which refuses a non-subscription
                   (expect (= "invalid-subscription"
                              (:reason (push/send-to-device! {:token "web-token" :platform "web"}
                                                             {:title "t" :body "b"}))))
                   ;; anything that is neither Apple, Android nor a browser is never sent
                   (expect (= "unsupported-platform"
                              (:reason (push/send-to-device! {:token "watch-token"
                                                              :platform "watchos"}
                                                             {:title "t" :body "b"}))))
                   ;; an iOS device still takes the APNs path
                   (expect (= "not-configured"
                              (:reason (push/send-to-device! {:token "ios-token" :platform "ios"}
                                                             {:title "t" :body "b"})))))))

(defdescribe status-exposes-both-providers
             (it "status exposes both providers"
                 (with-push-home [home]
                                 ;; with no credentials at all it is still push-capable — the relay
                                 (expect (true? (:is-available (push/status))))
                                 (expect (= "relay" (:provider (push/status))))
                                 (expect (true? (push/any-configured?)))
                                 ;; Web Push generates its VAPID identity on demand, so its half is
                                 ;; available on every gateway — and must never take the name of the
                                 ;; provider a device is actually delivered through.
                                 ;; the self-minted Web Push identity never shadows the relay
                                 (expect (true? (get-in (push/status) [:web-push :is-available])))
                                 (expect (= "relay" (:provider (push/status))))
                                 (write-service-account! home)
                                 (let [st (push/status)]
                                   ;; Android-only credentials are a valid, push-capable setup
                                   (expect (true? (:is-available st)))
                                   (expect (true? (push/any-configured?)))
                                   (expect (= "fcm" (:provider st)))
                                   (expect (true? (get-in st [:fcm :is-available])))
                                   (expect (false? (get-in st [:apns :is-available])))
                                   ;; status never leaks credentials
                                   (expect (not (str/includes? (pr-str st) "PRIVATE KEY")))))))
