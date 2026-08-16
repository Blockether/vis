(ns com.blockether.vis.internal.gateway.push-test
  "Native push (APNs): credential resolution, the device registry, the ES256
   provider token, and the turn-finished trigger.

   Every test redirects the push home (`vis.push.home`) at a temp dir, so the
   real `~/.vis/devices.edn` is never read or written."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is testing]]
            [com.blockether.vis.internal.gateway.push :as push]
            [com.blockether.vis.internal.gateway.web-push :as web-push]
            [com.blockether.vis.internal.gateway.state :as state]
            [com.blockether.vis.internal.gateway.wire :as wire])
  (:import [java.security KeyPairGenerator Signature]
           [java.security.spec ECGenParameterSpec]
           [java.util Arrays Base64]))

(defn- temp-home
  ^java.io.File []
  (let [d (io/file (System/getProperty "java.io.tmpdir") (str "vis-push-test-" (System/nanoTime)))]
    (.mkdirs (io/file d "apns"))
    d))

(defn- await-count
  "Wait only until async push dispatch reaches `n`, rather than sleeping a fixed interval."
  [sent n]
  (loop [attempts 100]
    (cond (>= (count @sent) (long n)) true
          (zero? attempts) false
          :else (do (Thread/sleep 10) (recur (dec attempts))))))

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
  "Raw 64-byte `r||s` back into the DER the JCA verifier expects.

   Each half is an UNSIGNED big-endian integer, and DER carries an INTEGER
   MINIMALLY: a leading zero byte only when the high bit is set, never a run of
   them. `BigInteger/toByteArray` IS that encoding. Padding the first byte by
   hand instead left the leading zeros a 32-byte JOSE half carries whenever its
   component is short, the JCA's strict DER parser threw `Invalid encoding for
   signature`, and the test went red on roughly one signature in a hundred."
  [^bytes raw]
  (let
    [der-int
     (fn [^bytes b]
       (.toByteArray (BigInteger. 1 b)))

     r
     (der-int (Arrays/copyOfRange raw 0 32))

     s
     (der-int (Arrays/copyOfRange raw 32 64))]

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
    (with-push-home
      [home]
      (write-key! home "ABC123DEFG")
      (spit (io/file home "apns" "apns.edn")
            (pr-str {:team-id "TEAMID1234" :topic "com.example.testapp" :environment "sandbox"}))
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
  (with-push-home
    [_home]
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
      (testing "status: available with this gateway's generated Web Push identity"
        (let [st (push/status)]
          ;; The identity is MINTED on demand, so it is available on every
          ;; gateway and can never be the provider a device is delivered
          ;; through while the relay is up — that is what `:provider` names.
          (is (true? (get-in st [:web-push :is-available])))
          (is (= "relay" (:provider st)))
          (is (true? (:is-available st)))
          (is (string? (get-in st [:web-push :application-server-key])))
          (is (= 0 (:devices st))))))))

(deftest web-device-dispatches-through-its-gateway-test
  (with-push-home [_home]
                  (let
                    [token
                     "{\"endpoint\":\"https://push.example.test/sub\",\"keys\":{}}"

                     device
                     (push/register-device! {:token token :platform "web"})]

                    (with-redefs
                      [web-push/send! (fn [sent-token _notification]
                                        (is (= token sent-token))
                                        {:status 201 :reason "accepted"})]
                      (let [result (push/send-to-device! device {:title "t" :body "b"})]
                        (is (= 201 (:status result)))
                        (is (true? (:is-delivered result))))))))

(deftest turn-finished-trigger-test
  (with-push-home
    [_home]
    (let
      [sid
       (random-uuid)

       sent
       (atom [])]

      (push/register-device! {:token (apply str (repeat 64 "b")) :environment "sandbox"})
      (push/set-session-describer!
        (fn [_ tid]
          {:title "Fix the gateway"
           :answer
           (get {"t1" "**Fixed** the gateway: the QR encoded a dead host.\n\n```clj\n(inc 1)\n```"
                 "t2" "Compile failed: unable to resolve symbol `foo`."}
                tid)}))
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
        (testing "a completed turn pushes one alert carrying the ANSWER, title and ids"
          (push/on-event! sid {"type" "turn.completed" "turn_id" "t1" "status" "completed"})
          (is (await-count sent 1) "completed-turn push arrives")
          (let [n (first @sent)]
            (is (= 1 (count @sent)))
            (is (= "Fix the gateway" (:title n)))
            ;; the point of the alert: what vis SAID, not that it said something.
            (is (= "Fixed the gateway: the QR encoded a dead host. [code]" (:body n)))
            (is (= (str sid) (:collapse-id n)))
            (is (= {:session_id (str sid) :turn_id "t1" :status "completed" :type "turn.end"}
                   (:data n)))))
        (testing "a failed turn carries the failure text it produced"
          (reset! sent [])
          (push/on-event! sid {"type" "turn.failed" "turn_id" "t2" "status" "failed"})
          (is (await-count sent 1) "failed-turn push arrives")
          (is (= "Compile failed: unable to resolve symbol foo." (:body (first @sent)))))
        (testing "with no answer text the status line is the fallback, never a blank body"
          (reset! sent [])
          (push/on-event! sid {"type" "turn.completed" "turn_id" "unknown" "status" "completed"})
          (is (await-count sent 1) "completed fallback push arrives")
          (push/on-event! sid {"type" "turn.failed" "turn_id" "unknown" "status" "failed"})
          (is (await-count sent 2) "failed fallback push arrives")
          (is (= ["Turn finished." "Turn failed."] (mapv :body @sent)))))
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
          (Thread/sleep 50)
          (is (= [] @sent))))
      (push/set-session-describer! nil))))

(deftest alerts-name-the-sending-gateway-test
  (with-push-home
    [_home]
    (let
      [sid
       (random-uuid)

       sent
       (atom [])]

      (push/register-device! {:token (apply str (repeat 64 "c")) :environment "sandbox"})
      (with-redefs
        [push/configured?
         (fn []
           true)

         push/broadcast!
         (fn [n]
           (swap! sent conj n)
           [])]

        (testing "with no gateway id installed the payload simply omits the key"
          (push/set-gateway-id! nil)
          (push/on-event! sid {"type" "turn.completed" "turn_id" "t1" "status" "completed"})
          (is (await-count sent 1) "push arrives")
          (is (not (contains? (:data (first @sent)) :gateway_id))))
        (testing "an empty id is no id at all"
          (reset! sent [])
          (push/set-gateway-id! "")
          (push/on-event! sid {"type" "turn.completed" "turn_id" "t1" "status" "completed"})
          (is (await-count sent 1) "push arrives")
          (is (not (contains? (:data (first @sent)) :gateway_id))))
        ;; The whole point: a phone paired with several machines cannot tell which
        ;; gateway a session id belongs to, and opening it on the wrong one is a 404.
        (testing "both alert kinds name the gateway that sent them"
          (reset! sent [])
          (push/set-gateway-id! "0123456789abcdef")
          (push/on-event! sid {"type" "turn.completed" "turn_id" "t1" "status" "completed"})
          (push/on-event! sid
                          {"type" "human_input.request" "request" {"id" "r1" "title" "Pick one"}})
          (is (await-count sent 2) "both pushes arrive")
          (is (= #{"turn.end" "human_input.request"} (set (map (comp :type :data) @sent))))
          (is (= ["0123456789abcdef" "0123456789abcdef"] (mapv (comp :gateway_id :data) @sent)))))
      (push/set-gateway-id! nil))))

(deftest answer-body-is-lock-screen-shaped-test
  (testing "markdown is written for a renderer, not a banner: it is stripped, not shown"
    (let [body #(@#'push/answer-body %)]
      (is (= "Two bugs, both real. \u2022 the QR was dead \u2022 the deeplink was unregistered"
             (body
               "## Two bugs, both real.\n\n- the QR was dead\n- the *deeplink* was unregistered")))
      (is (= "See the pairing docs for why." (body "See the [pairing docs](http://x/y) for why.")))
      (is (= "Fixed: [code]" (body "Fixed:\n```clj\n(defn f [] 1)\n```")))
      (is (nil? (body nil)))
      (is (nil? (body "   \n\n  ")))))
  (testing "a long answer is clipped on a word boundary with an ellipsis"
    (let
      [long-answer
       (str/join " " (repeat 80 "word"))

       out
       (@#'push/answer-body long-answer)]

      (is (<= (count out) 181))
      (is (str/ends-with? out "\u2026"))
      (is (not (str/includes? out "wor\u2026"))))))

(deftest alert-payload-speaks-apns-kebab-case-test
  (testing "aps keys are APNs' literal kebab-case, not the wire encoder's snake_case"
    (let
      [payload
       (@#'push/alert-payload
        {:title "Fix the gateway"
         :body "Turn finished."
         :thread-id "sess-1"
         :data {:session_id "sess-1" :type "turn.end"}})

       parsed
       (wire/parse-json payload)

       aps
       (get parsed "aps")]

      ;; APNs ignores unknown `aps` keys silently, so a snake_case slip costs
      ;; grouping and interruption level with no error anywhere to notice.
      (is (= "sess-1" (get aps "thread-id")))
      (is (= "active" (get aps "interruption-level")))
      (is (nil? (get aps "thread_id")))
      (is (nil? (get aps "interruption_level")))
      (is (= {"title" "Fix the gateway" "body" "Turn finished."} (get aps "alert")))
      (is (= "default" (get aps "sound")))
      ;; Without `mutable-content` iOS never runs the VisNotify service
      ;; extension, so the icon badge stays at whatever the last push left it —
      ;; this one key is the whole feature.
      (is (= 1 (get aps "mutable-content")))
      (is (nil? (get aps "mutable_content")))
      ;; the custom payload beside `aps` stays snake_case: that half IS our wire
      (is (= "sess-1" (get parsed "session_id")))
      (is (= "turn.end" (get parsed "type"))))))

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

(deftest keychain-credentials
  (testing "`security -w` hex output is decoded back to the PEM"
    (let
      [pem
       "-----BEGIN PRIVATE KEY-----\nMIGHAg\n-----END PRIVATE KEY-----\n"

       hex
       (str/join (map #(format "%02x" (int %)) pem))]

      (is (= pem (#'push/unhex hex)))
      (is (= "ABCD123456" (#'push/unhex "ABCD123456")) "plain values pass through")))
  (testing "a redirected push home never reads the developer's real keychain"
    (with-push-home [home]
                    (is (some? home))
                    (is (nil? (#'push/keychain "key")))
                    (is (contains? (set (:missing (push/config))) "key")))))
