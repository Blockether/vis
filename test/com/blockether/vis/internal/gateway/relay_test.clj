(ns com.blockether.vis.internal.gateway.relay-test
  "Relayed push: a gateway that holds NO Apple/Google key waking a device
   through a grant.

   Every test runs against a real HTTP server standing in for
   `apps/vis-companion-relay`, and against a throwaway push home, so neither
   the real `~/.vis/devices.edn` nor a provider is ever touched."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [lazytest.core :refer [defdescribe expect it]]
            [com.blockether.vis.internal.gateway.push :as push]
            [com.blockether.vis.internal.gateway.relay :as relay]
            [com.blockether.vis.contract.wire :as wire])
  (:import [com.sun.net.httpserver HttpExchange HttpHandler HttpServer]
           [java.net InetSocketAddress]
           [java.nio.charset StandardCharsets]))

(defn- temp-home
  ^java.io.File []
  (let [d (io/file (System/getProperty "java.io.tmpdir") (str "vis-relay-test-" (System/nanoTime)))]
    (.mkdirs d)
    d))

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

(defn- start-relay!
  "A REAL HTTP server standing in for the relay. `respond` sees the recorded
   request map and returns `{:status int :body str}`. Returns
   `{:url :requests :stop!}`."
  [respond]
  (let [requests
        (atom [])

        server
        (HttpServer/create (InetSocketAddress. "127.0.0.1" 0) 0)]

    (.createContext server
                    "/v1/push"
                    (reify
                      HttpHandler
                        ;; NOTE: hinting the PARAMETER makes the compiler refuse the
                        ;; void `handle`; hint a local instead.
                        (handle [_ e]
                          (let [^HttpExchange exchange
                                e

                                body
                                (slurp (.getRequestBody exchange))

                                request
                                {:method (.getRequestMethod exchange)
                                 :path (.getPath (.getRequestURI exchange))
                                 :authorization (.getFirst (.getRequestHeaders exchange)
                                                           "Authorization")
                                 :body (wire/parse-json body)}

                                _
                                (swap! requests conj request)

                                {:keys [status body]}
                                (respond request)

                                bytes
                                (.getBytes (str body) StandardCharsets/UTF_8)]

                            (.sendResponseHeaders exchange (int status) (alength bytes))
                            (with-open [out (.getResponseBody exchange)]
                              (.write out bytes)))
                          nil)))
    (.start server)
    {:url (str "http://127.0.0.1:" (.getPort (.getAddress server)))
     :requests requests
     :stop! #(.stop server 0)}))

(defn- configure-relay! [home url] (spit (io/file home "relay.edn") (pr-str {:url url})))

(def ^:private ALERT
  {:title "Fix the gateway" :body "Done." :data {:session_id "s-1"} :collapse-id "s-1"})

(defdescribe relay-config-test
             (it "no relay.edn and no env is the PUBLISHER's relay, never an error"
                 #_{:clj-kondo/ignore [:unresolved-symbol]}
                 (with-push-home [_home]
                                 (let [cfg (relay/config)]
                                   (expect (true? (:is-configured cfg)))
                                   (expect (= relay/DEFAULT-URL (:url cfg)))
                                   (expect (= "default" (:source cfg)))
                                   (expect (true? (relay/configured?))))))
             (it "~/.vis/relay.edn configures it, and a trailing slash is not a second path"
                 #_{:clj-kondo/ignore [:unresolved-symbol]}
                 (with-push-home [home]
                                 (configure-relay! home "https://push.example.com/")
                                 (let [cfg (relay/config)]
                                   (expect (true? (:is-configured cfg)))
                                   (expect (= "https://push.example.com" (:url cfg)))
                                   (expect (= "file" (:source cfg))))))
             (it "a grant is a secret: only a masked form is loggable"
                 (expect (= "abcdef…wxyz" (relay/mask "abcdefghijklmnopqrstuvwxyz")))
                 (expect (= "…" (relay/mask "short")))))

(defdescribe send-posts-the-grant-as-a-bearer-capability-test
             (it "send posts the grant as a bearer capability"
                 (with-push-home
                   [home]
                   (let [relay-server (start-relay! (fn [_]
                                                      {:status 200
                                                       :body "{\"is_delivered\":true}"}))]
                     (try (configure-relay! home (:url relay-server))
                          (let [result (relay/send! (:url relay-server) "GRANT-abcdef123456" ALERT)]
                            ;; the relay's verdict is the gateway's verdict
                            (expect (= 200 (:status result)))
                            (expect (= "" (:reason result)))
                            ;; one POST to /v1/push, authorised by the grant alone
                            (let [request (first @(:requests relay-server))]
                              (expect (= 1 (count @(:requests relay-server))))
                              (expect (= "POST" (:method request)))
                              (expect (= "/v1/push" (:path request)))
                              (expect (= "Bearer GRANT-abcdef123456" (:authorization request))))
                            ;; the body is the wire's snake_case, carrying no device token
                            (let [body (:body (first @(:requests relay-server)))]
                              (expect (= "Fix the gateway" (get body "title")))
                              (expect (= "Done." (get body "body")))
                              (expect (= "s-1" (get body "collapse_id")))
                              (expect (= {"session_id" "s-1"} (get body "data")))
                              ;; The relay turns this into APNs' `mutable-content`,
                              ;; which is what runs VisNotify and moves the badge.
                              (expect (true? (get body "is_mutable")))
                              (expect (not (str/includes? (wire/json-str body) "token")))))
                          (finally ((:stop! relay-server))))))))

(defdescribe
  relay-is-preferred-over-local-credentials-test
  (it "relay is preferred over local credentials"
      (with-push-home
        [home]
        (let [relay-server (start-relay! (fn [_]
                                           {:status 200 :body "{\"is_delivered\":true}"}))]
          (try (configure-relay! home (:url relay-server))
               ;; a gateway with NO Apple key still delivers, which is the whole point
               (expect (false? (push/configured?)))
               (expect (true? (push/any-configured?)))
               (let [device (push/register-device! {:grant "GRANT-abcdef123456" :platform "ios"})]
                 (expect (some? device))
                 (expect (= 1 (push/device-count)))
                 (let [result (push/send-to-device! device ALERT)]
                   (expect (true? (:is-delivered result)))
                   (expect (= 200 (:status result))))
                 (expect (= 1 (count @(:requests relay-server)))))
               ;; status advertises the relay, and the URL is not a secret
               (let [st (push/status)]
                 (expect (true? (:is-available st)))
                 (expect (= "relay" (:provider st)))
                 (expect (= (:url relay-server) (get-in st [:relay :url])))
                 (expect (true? (get-in st [:relay :is-available]))))
               (finally ((:stop! relay-server))))))))

(defdescribe
  revoked-grant-forgets-the-device-test
  (it "revoked grant forgets the device"
      (with-push-home
        [home]
        (let [relay-server
              (start-relay!
                (fn [_]
                  {:status 410
                   :body
                   "{\"is_delivered\":false,\"reason\":\"Unregistered\",\"is_revoked\":true}"}))]
          (try (configure-relay! home (:url relay-server))
               (let [device (push/register-device! {:grant "GRANT-abcdef123456" :platform "ios"})]
                 ;; 410 means the grant is gone for good
                 (expect (true? (relay/dead-grant? {:status 410})))
                 (expect (true? (relay/dead-grant? {:status 404})))
                 (expect (false? (relay/dead-grant? {:status 502})))
                 ;; and the device is dropped instead of retried forever
                 (let [result (push/send-to-device! device ALERT)]
                   (expect (false? (:is-delivered result)))
                   (expect (= 410 (:status result)))
                   (expect (= "Unregistered" (:reason result))))
                 (expect (= 0 (push/device-count))))
               (finally ((:stop! relay-server))))))))

(defdescribe grant-never-reaches-the-wire-test
             (it "grant never reaches the wire"
                 (with-push-home [home]
                                 (configure-relay! home "https://push.example.com")
                                 (push/register-device!
                                   {:grant "GRANT-abcdef123456" :platform "ios" :label "iPhone"})
                                 (let [d (first (push/list-devices))]
                                   ;; a listed device carries neither the grant nor a raw token
                                   (expect (nil? (:grant d)))
                                   (expect (nil? (:token d)))
                                   (expect (= "GRANT-…3456" (:token_preview d)))
                                   (expect (true? (:is-relayed d)))
                                   (expect (= "iPhone" (:label d)))
                                   ;; and nothing in the wire encoding leaks it either
                                   (expect (not (str/includes? (wire/json-str d)
                                                               "abcdef123456")))))))

;; A device that named no relay comes from a build that predates the address
;; travelling with the grant. It is not silent: the gateway spends that grant at
;; the relay IT names, which is the publisher's until an operator says otherwise.

(defdescribe
  a-grant-device-without-a-relay-uses-the-gateways-own-test
  (it "a grant device without a relay uses the gateways own"
      (with-push-home
        [home]
        (let [relay-server (start-relay! (fn [_]
                                           {:status 200 :body "{\"is_delivered\":true}"}))]
          (try (configure-relay! home (:url relay-server))
               (let [device (push/register-device! {:grant "GRANT-abcdef123456" :platform "ios"})]
                 (expect (some? device))
                 (expect (nil? (:relay-url device)))
                 ;; the grant is spent at the gateway's own relay
                 (expect (true? (:is-delivered (push/send-to-device! device ALERT))))
                 (expect (= "Bearer GRANT-abcdef123456"
                            (:authorization (first @(:requests relay-server)))))
                 (expect (= 1 (push/device-count))))
               (finally ((:stop! relay-server)))))
        ;; a registration with neither token nor grant is refused
        (expect (nil? (push/register-device! {:token "  "})))
        (expect (nil? (push/register-device! {}))))))

;; A relay is one more machine on the network, so the failures worth testing are
;; the network's: a stumble that must be tried again, a verdict that must not be,
;; and an address that must never see a bearer grant at all.

(defdescribe
  a-relay-that-stumbles-is-asked-twice-test
  (it "a 503 costs one retry, and the second answer is the verdict"
      (with-push-home
        [home]
        (let [attempts
              (atom 0)

              relay-server
              (start-relay! (fn [_]
                              (if (= 1 (swap! attempts inc))
                                {:status 503 :body "{\"reason\":\"upstream\"}"}
                                {:status 200 :body "{\"is_delivered\":true}"})))]

          (try (configure-relay! home (:url relay-server))
               (expect (= 200
                          (:status (relay/send! (:url relay-server) "GRANT-abcdef123456" ALERT))))
               (expect (= 2 (count @(:requests relay-server))))
               (finally ((:stop! relay-server)))))))
  (it "a verdict the relay already reached is never asked again"
      (with-push-home
        [home]
        (let [relay-server (start-relay! (fn [_]
                                           {:status 404
                                            :body "{\"error\":{\"code\":\"unknown_grant\"}}"}))]
          (try (configure-relay! home (:url relay-server))
               (expect (= 404
                          (:status (relay/send! (:url relay-server) "GRANT-abcdef123456" ALERT))))
               (expect (= 1 (count @(:requests relay-server))))
               (finally ((:stop! relay-server))))))))

(defdescribe a-cleartext-relay-is-refused-before-the-grant-leaves-test
             (it "a grant is a bearer capability: the address it is handed to is TLS or nothing"
                 (with-push-home
                   [home]
                   (configure-relay! home "http://push.example.com")
                   (let [cfg (relay/config)]
                     (expect (false? (:is-configured cfg)))
                     (expect (true? (:is-insecure cfg)))
                     (expect (= "http://push.example.com" (:url cfg))))
                   (expect (false? (relay/configured?)))
                   (expect (= {:status 0 :reason "insecure-relay-url"}
                              (relay/send! "http://push.example.com" "GRANT-abcdef123456" ALERT)))
                   (expect (= {:status 0 :reason "not-configured"}
                              (relay/send! nil "GRANT-abcdef123456" ALERT)))
                   (let [st (relay/status)]
                     (expect (false? (:is-available st)))
                     (expect (true? (:is-insecure st))))))
             (it "https is fine, and so is loopback — it cannot leave the machine"
                 (with-push-home [home]
                                 (configure-relay! home "https://push.example.com")
                                 (expect (true? (relay/configured?)))
                                 (configure-relay! home "http://127.0.0.1:8787")
                                 (expect (true? (relay/configured?)))
                                 (configure-relay! home "http://localhost:8787")
                                 (expect (true? (relay/configured?))))))

;; Zero configuration is the whole product claim: install the app, run
;; `vis-agent gateway`, tap "notify this device". Which relay can sign for a build is a
;; property of the BUILD, so the phone — not the laptop — is the one that knows
;; the address, and it learns it from the relay that sealed its grant.

(defdescribe a-device-names-the-relay-that-sealed-its-grant-test
             (it "a device names the relay that sealed its grant"
                 (with-push-home
                   [_home]
                   (let [relay-server (start-relay! (fn [_]
                                                      {:status 200
                                                       :body "{\"is_delivered\":true}"}))]
                     (try
                       ;; this gateway is configured with nothing, so it names the publisher's relay
                       (expect (true? (relay/configured?)))
                       (expect (= relay/DEFAULT-URL (:url (relay/config))))
                       (expect (= "default" (:source (relay/config))))
                       (let [device (push/register-device! {:grant "GRANT-abcdef123456"
                                                            :platform "ios"
                                                            :relay-url (:url relay-server)})]
                         ;; push is nevertheless available — a device brought an address
                         (expect (true? (push/any-configured?)))
                         (expect (true? (:is-available (push/status))))
                         ;; and the alert goes to the relay the DEVICE named
                         (let [result (push/send-to-device! device ALERT)]
                           (expect (true? (:is-delivered result)))
                           (expect (= 200 (:status result))))
                         (let [request (first @(:requests relay-server))]
                           (expect (= 1 (count @(:requests relay-server))))
                           (expect (= "/v1/push" (:path request)))
                           (expect (= "Bearer GRANT-abcdef123456" (:authorization request))))
                         ;; the address is not a secret: it is where this device's alerts go
                         (expect (= (:url relay-server) (:relay-url (first (push/list-devices))))))
                       (finally ((:stop! relay-server))))))))

(defdescribe a-device-may-not-name-a-cleartext-relay-test
             (it
               "an address a device supplies is still TLS or nothing"
               (with-push-home
                 [home]
                 (let [relay-server (start-relay! (fn [_]
                                                    {:status 200 :body "{\"is_delivered\":true}"}))]
                   (try (configure-relay! home (:url relay-server))
                        (let [device (push/register-device! {:grant "GRANT-abcdef123456"
                                                             :platform "ios"
                                                             :relay-url "http://push.example.com"})]
                          (expect (some? device))
                          (expect (nil? (:relay-url device)))
                          ;; and the alert goes to the relay this gateway names instead
                          (expect (true? (:is-delivered (push/send-to-device! device ALERT))))
                          (expect (= 1 (count @(:requests relay-server)))))
                        (finally ((:stop! relay-server))))))))

;; One constant in two languages. The gateway must name the relay the APP mints
;; at: a grant is gibberish to every relay but the one that sealed it, so drift
;; between the two is silent and total — every push 404s. Read the file rather
;; than remember it.

(defdescribe the-app-and-the-gateway-name-the-same-relay-test
             (it "the app and the gateway name the same relay"
                 (let [src
                       (slurp (io/file "apps/vis-companion/src/lib/relay.ts"))

                       named
                       (second (re-find #"PUBLISHER_RELAY_URL\s*=\s*['\"]([^'\"]+)['\"]" src))]

                   (expect (= "https://vis.relay.blockether.com" relay/DEFAULT-URL))
                   (expect (some? named))
                   (expect (= relay/DEFAULT-URL named)))))
