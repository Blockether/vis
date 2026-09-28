(ns com.blockether.vis.internal.gateway.push-test
  "Native push (APNs): credential resolution, the device registry, the ES256
   provider token, and the turn-finished trigger.

   Every test redirects the push home (`vis.push.home`) at a temp dir, so the
   real `~/.vis/devices.edn` is never read or written."
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as sh]
            [clojure.string :as str]
            [lazytest.core :refer [defdescribe expect it]]
            [com.blockether.vis.internal.gateway.push :as push]
            [com.blockether.vis.internal.gateway.keychain :as keychain]
            [com.blockether.vis.internal.gateway.web-push :as web-push]
            [com.blockether.vis.internal.gateway.state :as state]
            [com.blockether.vis.contract.wire :as wire])
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

(defn- write-key!
  "Generate a real P-256 key, write it as Apple's `AuthKey_<kid>.p8` PKCS#8 PEM,
   and return the public key the JWT can be verified against."
  [home kid]
  (let [kp
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
  (let [der-int
        (fn [^bytes b]
          (.toByteArray (BigInteger. 1 b)))

        r
        (der-int (Arrays/copyOfRange raw 0 32))

        s
        (der-int (Arrays/copyOfRange raw 32 64))]

    (byte-array
      (concat [0x30 (+ 4 (count r) (count s)) 0x02 (count r)] (seq r) [0x02 (count s)] (seq s)))))

(defdescribe config-discovery-test
             (it "an empty push home reports exactly what is missing and is not configured"
                 #_{:clj-kondo/ignore [:unresolved-symbol]}
                 (with-push-home [home]
                                 (let [cfg (push/config)]
                                   (expect (false? (:is-configured cfg)))
                                   (expect (= ["key" "key_id" "team_id" "topic"] (:missing cfg)))
                                   (expect (false? (push/configured?)))
                                   (expect (str/starts-with? (str (push/devices-file))
                                                             (.getAbsolutePath home))))))
             (it "an AuthKey_<kid>.p8 plus apns.edn is a complete configuration"
                 (with-push-home [home]
                                 (write-key! home "ABC123DEFG")
                                 (spit (io/file home "apns" "apns.edn")
                                       (pr-str {:team-id "TEAMID1234"
                                                :topic "com.example.testapp"
                                                :environment "sandbox"}))
                                 (let [cfg (push/config)]
                                   (expect (true? (:is-configured cfg)))
                                   (expect (= [] (:missing cfg)))
                                   (expect (= "ABC123DEFG" (:key-id cfg)))
                                   (expect (= "com.example.testapp" (:topic cfg)))
                                   (expect (= "sandbox" (:default-environment cfg)))
                                   (expect (true? (push/configured?)))))))

(defdescribe
  provider-token-is-a-verifiable-es256-jwt-test
  (it
    "provider token is a verifiable es256 jwt"
    (with-push-home
      [home]
      (let [pub
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

        ;; header and claims are exactly what Apple requires
        (expect (= "{\"alg\":\"ES256\",\"kid\":\"KID1234567\"}" (decode h)))
        (expect (str/includes? (decode p) "\"iss\":\"TEAM123456\""))
        (expect (str/includes? (decode p) "\"iat\":"))
        ;; the signature is raw JOSE r||s and verifies against the key
        (expect (= 64 (count raw)))
        (expect (true? (.verify (doto (Signature/getInstance "SHA256withECDSA")
                                  (.initVerify pub)
                                  (.update (.getBytes (str h "." p) "UTF-8")))
                                (jose->der raw))))))))

(defdescribe
  device-registry-test
  (it "device registry"
      #_{:clj-kondo/ignore [:unresolved-symbol]}
      (with-push-home
        [_home]
        (let [tok (apply str (repeat 64 "a"))]
          ;; registration is idempotent and persists across a cache drop
          (expect (some? (push/register-device! {:token tok
                                                 :platform "ios"
                                                 :environment "sandbox"
                                                 :client "vis-companion"
                                                 :client-version "1.0.1"})))
          (expect (= 1 (push/device-count)))
          (push/register-device! {:token tok :platform "ios" :environment "sandbox"})
          (expect (= 1 (push/device-count)))
          (push/reload-devices!)
          (expect (= 1 (push/device-count)))
          ;; listed devices never carry the raw token
          (let [d (first (push/list-devices))]
            (expect (nil? (:token d)))
            (expect (= "aaaaaa…aaaa" (:token_preview d)))
            (expect (= "sandbox" (:environment d))))
          ;; a blank token is refused
          (expect (nil? (push/register-device! {:token "  "})))
          (expect (= 1 (push/device-count)))
          ;; unregister is idempotent
          (expect (true? (push/unregister-device! tok)))
          (expect (false? (push/unregister-device! tok)))
          (expect (= 0 (push/device-count)))
          ;; status: available with this gateway's generated Web Push identity
          (let [st (push/status)]
            ;; The identity is MINTED on demand, so it is available on every
            ;; gateway and can never be the provider a device is delivered
            ;; through while the relay is up — that is what `:provider` names.
            (expect (true? (get-in st [:web-push :is-available])))
            (expect (= "relay" (:provider st)))
            (expect (true? (:is-available st)))
            (expect (string? (get-in st [:web-push :application-server-key])))
            (expect (= 0 (:devices st))))))))

(defdescribe web-device-dispatches-through-its-gateway-test
             (it "web device dispatches through its gateway"
                 (with-push-home
                   [_home]
                   (let [token
                         "{\"endpoint\":\"https://push.example.test/sub\",\"keys\":{}}"

                         device
                         (push/register-device! {:token token :platform "web"})]

                     (with-redefs [web-push/send! (fn [sent-token _notification]
                                                    (expect (= token sent-token))
                                                    {:status 201 :reason "accepted"})]
                       (let [result (push/send-to-device! device {:title "t" :body "b"})]
                         (expect (= 201 (:status result)))
                         (expect (true? (:is-delivered result)))))))))

(defdescribe
  turn-finished-trigger-test
  (it
    "turn finished trigger"
    (with-push-home
      [_home]
      (let [sid
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
        (with-redefs [push/configured?
                      (fn []
                        true)

                      push/broadcast!
                      (fn [n]
                        (swap! sent conj n)
                        [])]

          ;; a non-terminal event pushes nothing
          (push/on-event! sid {"type" "content.block.delta" "turn_id" "t1"})
          (expect (= [] @sent))
          ;; Council and managed children never create completion alerts
          (push/on-event! sid {"type" "turn.completed" "turn_id" "c" "request_kind" "council"})
          (push/on-event! sid {"type" "turn.failed" "turn_id" "a" "subagent" true})
          (Thread/sleep 50)
          (expect (= [] @sent))
          ;; a completed turn pushes one alert carrying the ANSWER, title and ids
          (push/on-event! sid {"type" "turn.completed" "turn_id" "t1" "status" "completed"})
          (expect (await-count sent 1) "completed-turn push arrives")
          (let [n (first @sent)]
            (expect (= 1 (count @sent)))
            (expect (= "Fix the gateway" (:title n)))
            ;; the point of the alert: what vis SAID, not that it said something.
            (expect (= "Fixed the gateway: the QR encoded a dead host. [code]" (:body n)))
            (expect (= (str sid) (:collapse-id n)))
            (expect (= {:session_id (str sid) :turn_id "t1" :status "completed" :type "turn.end"}
                       (:data n))))
          ;; a failed turn carries the failure text it produced
          (reset! sent [])
          (push/on-event! sid {"type" "turn.failed" "turn_id" "t2" "status" "failed"})
          (expect (await-count sent 1) "failed-turn push arrives")
          (expect (= "Compile failed: unable to resolve symbol foo." (:body (first @sent))))
          ;; with no answer text the status line is the fallback, never a blank body
          (reset! sent [])
          (push/on-event! sid {"type" "turn.completed" "turn_id" "unknown" "status" "completed"})
          (expect (await-count sent 1) "completed fallback push arrives")
          (push/on-event! sid {"type" "turn.failed" "turn_id" "unknown" "status" "failed"})
          (expect (await-count sent 2) "failed fallback push arrives")
          (expect (= ["Turn finished." "Turn failed."] (mapv :body @sent))))
        ;; with no device registered nothing is sent at all
        (push/unregister-device! (apply str (repeat 64 "b")))
        (reset! sent [])
        (with-redefs [push/configured?
                      (fn []
                        true)

                      push/broadcast!
                      (fn [n]
                        (swap! sent conj n)
                        [])]

          (push/on-event! sid {"type" "turn.completed" "turn_id" "t3" "status" "completed"})
          (Thread/sleep 50)
          (expect (= [] @sent)))
        (push/set-session-describer! nil)))))

(defdescribe
  alerts-name-the-sending-gateway-test
  (it
    "alerts name the sending gateway"
    (with-push-home
      [_home]
      (let [sid
            (random-uuid)

            sent
            (atom [])]

        (push/register-device! {:token (apply str (repeat 64 "c")) :environment "sandbox"})
        (with-redefs [push/configured?
                      (fn []
                        true)

                      push/broadcast!
                      (fn [n]
                        (swap! sent conj n)
                        [])]

          ;; with no gateway id installed the payload simply omits the key
          (push/set-gateway-id! nil)
          (push/on-event! sid {"type" "turn.completed" "turn_id" "t1" "status" "completed"})
          (expect (await-count sent 1) "push arrives")
          (expect (not (contains? (:data (first @sent)) :gateway_id)))
          ;; an empty id is no id at all
          (reset! sent [])
          (push/set-gateway-id! "")
          (push/on-event! sid {"type" "turn.completed" "turn_id" "t1" "status" "completed"})
          (expect (await-count sent 1) "push arrives")
          (expect (not (contains? (:data (first @sent)) :gateway_id)))
          ;; The whole point: a phone paired with several machines cannot tell which
          ;; gateway a session id belongs to, and opening it on the wrong one is a 404.
          ;; both alert kinds name the gateway that sent them
          (reset! sent [])
          (push/set-gateway-id! "0123456789abcdef")
          (push/on-event! sid {"type" "turn.completed" "turn_id" "t1" "status" "completed"})
          (push/on-event! sid
                          {"type" "view.open" "kind" "input" "view" {"id" "r1" "title" "Pick one"}})
          (expect (await-count sent 2) "both pushes arrive")
          (expect (= #{"turn.end" "view.open"} (set (map (comp :type :data) @sent))))
          (expect (= ["0123456789abcdef" "0123456789abcdef"]
                     (mapv (comp :gateway_id :data) @sent))))
        (push/set-gateway-id! nil)))))

(defdescribe
  answer-body-is-lock-screen-shaped-test
  (it "markdown is written for a renderer, not a banner: it is stripped, not shown"
      (let [body #(@#'push/answer-body %)]
        (expect
          (= "Two bugs, both real. \u2022 the QR was dead \u2022 the deeplink was unregistered"
             (body
               "## Two bugs, both real.\n\n- the QR was dead\n- the *deeplink* was unregistered")))
        (expect (= "See the pairing docs for why."
                   (body "See the [pairing docs](http://x/y) for why.")))
        (expect (= "Fixed: [code]" (body "Fixed:\n```clj\n(defn f [] 1)\n```")))
        (expect (nil? (body nil)))
        (expect (nil? (body "   \n\n  ")))))
  (it "a long answer is clipped on a word boundary with an ellipsis"
      (let [long-answer
            (str/join " " (repeat 80 "word"))

            out
            (@#'push/answer-body long-answer)]

        (expect (<= (count out) 181))
        (expect (str/ends-with? out "\u2026"))
        (expect (not (str/includes? out "wor\u2026"))))))

(defdescribe
  alerts-are-worded-in-one-place-test
  ;; THE shape of a notification, wherever it is raised: the gateway pushes it to a phone and
  ;; the desktop app reads the same two lines back over `GET /v1/sessions/:sid/alert`.
  (it "an answer is the session's own name and what vis said"
      (expect (= {:title "deploy" :body "Done • Shipped v2 to prod"}
                 (push/answer-alert {:title "deploy"
                                     :answer "## Done\n\n- Shipped **v2** to `prod`"}))))
  (it "a turn that left nothing readable still says what happened"
      (expect (= {:title "Vis" :body "Turn finished."} (push/answer-alert {:answer "  \n "})))
      (expect (= {:title "notes" :body "Turn failed."}
                 (push/answer-alert {:title "notes" :answer nil :is-failed true}))))
  (it "a parked run is the question its wire View asked (#291)"
      (expect (= {:title "Action needed — Which branch?" :body "main is two commits ahead."}
                 (push/question-alert {"title" "Which branch?"
                                       "description" "main is two commits ahead."}))))
  (it "a request that names nothing still asks for the human"
      (expect (= {:title "Action needed" :body "Vis is waiting on your answer."}
                 (push/question-alert nil)))))

(defdescribe
  alert-payload-speaks-apns-kebab-case-test
  (it "aps keys are APNs' literal kebab-case, not the wire encoder's snake_case"
      (let [payload
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
        (expect (= "sess-1" (get aps "thread-id")))
        (expect (= "active" (get aps "interruption-level")))
        (expect (nil? (get aps "thread_id")))
        (expect (nil? (get aps "interruption_level")))
        (expect (= {"title" "Fix the gateway" "body" "Turn finished."} (get aps "alert")))
        (expect (= "default" (get aps "sound")))
        ;; Without `mutable-content` iOS never runs the VisNotify service
        ;; extension, so the icon badge stays at whatever the last push left it —
        ;; this one key is the whole feature.
        (expect (= 1 (get aps "mutable-content")))
        (expect (nil? (get aps "mutable_content")))
        ;; the custom payload beside `aps` stays snake_case: that half IS our wire
        (expect (= "sess-1" (get parsed "session_id")))
        (expect (= "turn.end" (get parsed "type"))))))

(defdescribe event-tap-runs-on-append-test
             (it "state/append-event! runs registered taps and survives a throwing one"
                 (let [seen
                       (atom [])

                       sid
                       (random-uuid)]

                   (try
                     (state/add-event-tap! ::boom
                                           (fn [_ _]
                                             (throw (ex-info "nope" {}))))
                     (state/add-event-tap! ::spy
                                           (fn [s e]
                                             (swap! seen conj [s (get e "type")])))
                     (state/append-event! sid "turn.completed" {:turn_id "t9" :status "completed"})
                     (expect (= [[sid "turn.completed"]] @seen))
                     (finally (state/remove-event-tap! ::boom) (state/remove-event-tap! ::spy))))))

(defdescribe keychain-cache
             ;; Regression, gateway CPU audit: `push/config` asks the keychain for five
             ;; secrets and runs inside `capabilities`, a request every connected client
             ;; makes, so the daemon forked `security` (~10 ms of CPU each) on a steady
             ;; cadence to re-read values that change only when a human edits them.
             (it "one `security` fork per key per TTL, present or absent, until reset"
                 (let [mac?
                       (str/includes? (str/lower-case (str (System/getProperty "os.name"))) "mac")

                       forks
                       (atom [])

                       answers
                       (atom {["vis-apns" "topic"] "com.example.app"})]

                   (keychain/reset-cache!)
                   (try (with-redefs [sh/sh (fn [& args]
                                              (swap! forks conj (vec args))
                                              (if-let [v (get @answers [(nth args 3) (nth args 5)])]
                                                {:exit 0 :out (str v "\n") :err ""}
                                                {:exit 44 :out "" :err "not found"}))]
                          (let [a (keychain/secret "vis-apns" "topic")
                                b (keychain/secret "vis-apns" "topic")
                                c (keychain/secret "vis-apns" "team_id")
                                d (keychain/secret "vis-apns" "team_id")]

                            (if mac?
                              (do (expect (= "com.example.app" a b))
                                  (expect (nil? c) "an absent key is nil ...")
                                  (expect (nil? d) "... and stays nil from the cache")
                                  (expect (= 2 (count @forks)) "two keys, two forks, four reads"))
                              (do (expect (nil? a)) (expect (nil? b)) (expect (empty? @forks)))))
                          (keychain/reset-cache!)
                          (keychain/secret "vis-apns" "topic")
                          (when mac? (expect (= 3 (count @forks)) "a reset asks `security` again")))
                        (finally (keychain/reset-cache!))))))

(defdescribe keychain-credentials
             (it "`security -w` hex output is decoded back to the PEM"
                 (let [pem
                       "-----BEGIN PRIVATE KEY-----\nMIGHAg\n-----END PRIVATE KEY-----\n"

                       hex
                       (str/join (map #(format "%02x" (int %)) pem))]

                   (expect (= pem (#'keychain/unhex hex)))
                   (expect (= "ABCD123456" (#'keychain/unhex "ABCD123456"))
                           "plain values pass through")))
             (it "a redirected push home never reads the developer's real keychain"
                 (with-push-home [home]
                                 (expect (some? home))
                                 (expect (nil? (keychain/secret "vis-apns" "key")))
                                 (expect (contains? (set (:missing (push/config))) "key")))))
