(ns com.blockether.vis.internal.gateway.push-test
  "Native push (APNs): credential resolution, the device registry, the ES256
   provider token, and the turn-finished trigger.

   Every test redirects the push home (`vis.push.home`) at a temp dir, so the
   real `~/.vis/devices.edn` is never read or written."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [com.blockether.vis.internal.gateway.push :as push]
            [com.blockether.vis.internal.gateway.state :as state])
  (:import [java.security KeyPairGenerator Signature]
           [java.security.spec ECGenParameterSpec]
           [java.util Arrays Base64]))

(defn- temp-home
  ^java.io.File []
  (let [d (io/file (System/getProperty "java.io.tmpdir") (str "vis-push-test-" (System/nanoTime)))]
    (.mkdirs (io/file d "apns"))
    d))

(defmacro with-push-home
  "Run `body` against a throwaway push home with an empty device registry."
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

(defn- write-key!
  "Generate a real P-256 key, write it as Apple's `AuthKey_<kid>.p8` PKCS#8 PEM,
   and return the public key the JWT can be verified against."
  [home kid]
  (let
    [kp
     (.generateKeyPair (doto (KeyPairGenerator/getInstance "EC")
                         (.initialize (ECGenParameterSpec. "secp256r1"))))

     pem
     (str "-----BEGIN PRIVATE KEY-----\n"
          (.encodeToString (Base64/getMimeEncoder 64 (.getBytes "\n"))
                           (.getEncoded (.getPrivate kp)))
          "\n-----END PRIVATE KEY-----\n")]

    (spit (io/file home "apns" (str "AuthKey_" kid ".p8")) pem)
    (.getPublic kp)))

(defn- jose->der
  "Raw 64-byte `r||s` back into the DER the JCA verifier expects."
  [^bytes raw]
  (let
    [pad
     (fn [^bytes b]
       (if (neg? (aget b 0)) (byte-array (cons (byte 0) (seq b))) b))

     r
     (pad (Arrays/copyOfRange raw 0 32))

     s
     (pad (Arrays/copyOfRange raw 32 64))]

    (byte-array
      (concat [0x30 (+ 4 (count r) (count s)) 0x02 (count r)] (seq r) [0x02 (count s)] (seq s)))))

(deftest config-discovery-test
  (testing "an empty push home reports exactly what is missing and is not configured"
    #_{:clj-kondo/ignore [:unresolved-symbol]}
    (with-push-home [home]
                    (let [cfg (push/config)]
                      (is (false? (:is-configured cfg)))
                      (is (= ["key" "key_id" "team_id" "topic"] (:missing cfg)))
                      (is (false? (push/configured?)))
                      (is (str/starts-with? (str (push/devices-file)) (.getAbsolutePath home))))))
  (testing "an AuthKey_<kid>.p8 plus apns.edn is a complete configuration"
    (with-push-home [home]
                    (write-key! home "ABC123DEFG")
                    (spit (io/file home "apns" "apns.edn")
                          (pr-str {:team-id "TEAMID1234"
                                   :topic "com.example.testapp"
                                   :environment "sandbox"}))
                    (let [cfg (push/config)]
                      (is (true? (:is-configured cfg)))
                      (is (= [] (:missing cfg)))
                      (is (= "ABC123DEFG" (:key-id cfg)))
                      (is (= "com.example.testapp" (:topic cfg)))
                      (is (= "sandbox" (:default-environment cfg)))
                      (is (true? (push/configured?)))))))

(deftest provider-token-is-a-verifiable-es256-jwt-test
  (with-push-home
    [home]
    (let
      [pub
       (write-key! home "KID1234567")

       _
       (spit (io/file home "apns" "apns.edn")
             (pr-str {:team-id "TEAM123456" :topic "com.example.app"}))

       sign
       (ns-resolve 'com.blockether.vis.internal.gateway.push 'sign-jwt)

       jwt
       (sign (push/config))

       [h p s]
       (str/split jwt #"\.")

       decode
       #(String. (.decode (Base64/getUrlDecoder) ^String %) "UTF-8")

       raw
       (.decode (Base64/getUrlDecoder) ^String s)]

      (testing "header and claims are exactly what Apple requires"
        (is (= "{\"alg\":\"ES256\",\"kid\":\"KID1234567\"}" (decode h)))
        (is (str/includes? (decode p) "\"iss\":\"TEAM123456\""))
        (is (str/includes? (decode p) "\"iat\":")))
      (testing "the signature is raw JOSE r||s and verifies against the key"
        (is (= 64 (count raw)))
        (is (true? (.verify (doto (Signature/getInstance "SHA256withECDSA")
                              (.initVerify pub)
                              (.update (.getBytes (str h "." p) "UTF-8")))
                            (jose->der raw))))))))

(deftest device-registry-test
  #_{:clj-kondo/ignore [:unresolved-symbol]}
  (with-push-home [_home]
                  (let [tok (apply str (repeat 64 "a"))]
                    (testing "registration is idempotent and persists across a cache drop"
                      (is (some? (push/register-device! {:token tok
                                                         :platform "ios"
                                                         :environment "sandbox"
                                                         :client "vis-companion"
                                                         :client-version "1.0.1"})))
                      (is (= 1 (push/device-count)))
                      (push/register-device! {:token tok :platform "ios" :environment "sandbox"})
                      (is (= 1 (push/device-count)))
                      (push/reload-devices!)
                      (is (= 1 (push/device-count))))
                    (testing "listed devices never carry the raw token"
                      (let [d (first (push/list-devices))]
                        (is (nil? (:token d)))
                        (is (= "aaaaaa…aaaa" (:token_preview d)))
                        (is (= "sandbox" (:environment d)))))
                    (testing "a blank token is refused"
                      (is (nil? (push/register-device! {:token "  "})))
                      (is (= 1 (push/device-count))))
                    (testing "unregister is idempotent"
                      (is (true? (push/unregister-device! tok)))
                      (is (false? (push/unregister-device! tok)))
                      (is (= 0 (push/device-count))))
                    (testing "status reports availability, device count and what is missing"
                      (let [st (push/status)]
                        (is (= "apns" (:provider st)))
                        (is (false? (:is-available st)))
                        (is (= 0 (:devices st))))))))

(deftest turn-finished-trigger-test
  (with-push-home
    [_home]
    (let
      [sid
       (random-uuid)

       sent
       (atom [])]

      (push/register-device! {:token (apply str (repeat 64 "b")) :environment "sandbox"})
      (push/set-session-describer! (fn [_]
                                     {:title "Fix the gateway"}))
      (with-redefs
        [push/configured?
         (fn []
           true)

         push/broadcast!
         (fn [n]
           (swap! sent conj n)
           [])]

        (testing "a non-terminal event pushes nothing"
          (push/on-event! sid {"type" "content.block.delta" "turn_id" "t1"})
          (is (= [] @sent)))
        (testing "a completed turn pushes one alert carrying the session title and ids"
          (push/on-event! sid {"type" "turn.completed" "turn_id" "t1" "status" "completed"})
          (Thread/sleep 200)
          (let [n (first @sent)]
            (is (= 1 (count @sent)))
            (is (= "Fix the gateway" (:title n)))
            (is (= "Turn finished." (:body n)))
            (is (= (str sid) (:collapse-id n)))
            (is (= {:session_id (str sid) :turn_id "t1" :status "completed" :type "turn.end"}
                   (:data n)))))
        (testing "a failed turn says so"
          (reset! sent [])
          (push/on-event! sid {"type" "turn.failed" "turn_id" "t2" "status" "failed"})
          (Thread/sleep 200)
          (is (= "Turn failed." (:body (first @sent))))))
      (testing "with no device registered nothing is sent at all"
        (push/unregister-device! (apply str (repeat 64 "b")))
        (reset! sent [])
        (with-redefs
          [push/configured?
           (fn []
             true)

           push/broadcast!
           (fn [n]
             (swap! sent conj n)
             [])]

          (push/on-event! sid {"type" "turn.completed" "turn_id" "t3" "status" "completed"})
          (Thread/sleep 200)
          (is (= [] @sent))))
      (push/set-session-describer! nil))))

(deftest event-tap-runs-on-append-test
  (testing "state/append-event! runs registered taps and survives a throwing one"
    (let
      [seen
       (atom [])

       sid
       (random-uuid)]

      (try (state/add-event-tap! ::boom
                                 (fn [_ _]
                                   (throw (ex-info "nope" {}))))
           (state/add-event-tap! ::spy
                                 (fn [s e]
                                   (swap! seen conj [s (get e "type")])))
           (state/append-event! sid "turn.completed" {:turn_id "t9" :status "completed"})
           (is (= [[sid "turn.completed"]] @seen))
           (finally (state/remove-event-tap! ::boom) (state/remove-event-tap! ::spy))))))
