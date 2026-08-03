(ns com.blockether.vis.internal.gateway.relay-test
  "Relayed push: a gateway that holds NO Apple/Google key waking a device
   through a grant.

   Every test runs against a real HTTP server standing in for
   `apps/vis-companion-relay`, and against a throwaway push home, so neither
   the real `~/.vis/devices.edn` nor a provider is ever touched."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is testing]]
            [com.blockether.vis.internal.gateway.push :as push]
            [com.blockether.vis.internal.gateway.relay :as relay]
            [com.blockether.vis.internal.gateway.wire :as wire])
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

(defn- start-relay!
  "A REAL HTTP server standing in for the relay. `respond` sees the recorded
   request map and returns `{:status int :body str}`. Returns
   `{:url :requests :stop!}`."
  [respond]
  (let
    [requests
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
                          (let
                            [^HttpExchange exchange
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

(deftest relay-config-test
  (testing "no relay.edn and no env is simply OFF — never an error"
    #_{:clj-kondo/ignore [:unresolved-symbol]}
    (with-push-home [_home]
                    (let [cfg (relay/config)]
                      (is (false? (:is-configured cfg)))
                      (is (nil? (:url cfg)))
                      (is (false? (relay/configured?))))))
  (testing "~/.vis/relay.edn configures it, and a trailing slash is not a second path"
    #_{:clj-kondo/ignore [:unresolved-symbol]}
    (with-push-home [home]
                    (configure-relay! home "https://push.example.com/")
                    (let [cfg (relay/config)]
                      (is (true? (:is-configured cfg)))
                      (is (= "https://push.example.com" (:url cfg)))
                      (is (= "file" (:source cfg))))))
  (testing "a grant is a secret: only a masked form is loggable"
    (is (= "abcdef…wxyz" (relay/mask "abcdefghijklmnopqrstuvwxyz")))
    (is (= "…" (relay/mask "short")))))

(deftest send-posts-the-grant-as-a-bearer-capability-test
  (with-push-home [home]
                  (let
                    [relay-server (start-relay! (fn [_]
                                                  {:status 200 :body "{\"is_delivered\":true}"}))]
                    (try (configure-relay! home (:url relay-server))
                         (let [result (relay/send! "GRANT-abcdef123456" ALERT)]
                           (testing "the relay's verdict is the gateway's verdict"
                             (is (= 200 (:status result)))
                             (is (= "" (:reason result))))
                           (testing "one POST to /v1/push, authorised by the grant alone"
                             (let [request (first @(:requests relay-server))]
                               (is (= 1 (count @(:requests relay-server))))
                               (is (= "POST" (:method request)))
                               (is (= "/v1/push" (:path request)))
                               (is (= "Bearer GRANT-abcdef123456" (:authorization request)))))
                           (testing "the body is the wire's snake_case, carrying no device token"
                             (let [body (:body (first @(:requests relay-server)))]
                               (is (= "Fix the gateway" (get body "title")))
                               (is (= "Done." (get body "body")))
                               (is (= "s-1" (get body "collapse_id")))
                               (is (= {"session_id" "s-1"} (get body "data")))
                               (is (not (str/includes? (wire/json-str body) "token"))))))
                         (finally ((:stop! relay-server)))))))

(deftest relay-is-preferred-over-local-credentials-test
  (with-push-home
    [home]
    (let
      [relay-server (start-relay! (fn [_]
                                    {:status 200 :body "{\"is_delivered\":true}"}))]
      (try (configure-relay! home (:url relay-server))
           (testing "a gateway with NO Apple key still delivers, which is the whole point"
             (is (false? (push/configured?)))
             (is (true? (push/any-configured?)))
             (let [device (push/register-device! {:grant "GRANT-abcdef123456" :platform "ios"})]
               (is (some? device))
               (is (= 1 (push/device-count)))
               (let [result (push/send-to-device! device ALERT)]
                 (is (true? (:is-delivered result)))
                 (is (= 200 (:status result))))
               (is (= 1 (count @(:requests relay-server))))))
           (testing "status advertises the relay, and the URL is not a secret"
             (let [st (push/status)]
               (is (true? (:is-available st)))
               (is (= "relay" (:provider st)))
               (is (= (:url relay-server) (get-in st [:relay :url])))
               (is (true? (get-in st [:relay :is-available])))))
           (finally ((:stop! relay-server)))))))

(deftest revoked-grant-forgets-the-device-test
  (with-push-home
    [home]
    (let
      [relay-server
       (start-relay!
         (fn [_]
           {:status 410
            :body "{\"is_delivered\":false,\"reason\":\"Unregistered\",\"is_revoked\":true}"}))]
      (try (configure-relay! home (:url relay-server))
           (let [device (push/register-device! {:grant "GRANT-abcdef123456" :platform "ios"})]
             (testing "410 means the grant is gone for good"
               (is (true? (relay/dead-grant? {:status 410})))
               (is (true? (relay/dead-grant? {:status 404})))
               (is (false? (relay/dead-grant? {:status 502}))))
             (testing "and the device is dropped instead of retried forever"
               (let [result (push/send-to-device! device ALERT)]
                 (is (false? (:is-delivered result)))
                 (is (= 410 (:status result)))
                 (is (= "Unregistered" (:reason result))))
               (is (= 0 (push/device-count)))))
           (finally ((:stop! relay-server)))))))

(deftest grant-never-reaches-the-wire-test
  (with-push-home [home]
                  (configure-relay! home "https://push.example.com")
                  (push/register-device!
                    {:grant "GRANT-abcdef123456" :platform "ios" :label "iPhone"})
                  (let [d (first (push/list-devices))]
                    (testing "a listed device carries neither the grant nor a raw token"
                      (is (nil? (:grant d)))
                      (is (nil? (:token d)))
                      (is (= "GRANT-…3456" (:token_preview d)))
                      (is (true? (:is-relayed d)))
                      (is (= "iPhone" (:label d))))
                    (testing "and nothing in the wire encoding leaks it either"
                      (is (not (str/includes? (wire/json-str d) "abcdef123456")))))))

(deftest a-grant-device-without-a-relay-is-honest-about-it-test
  (with-push-home [_home]
                  (testing "registration still works — the relay may be configured later"
                    (let
                      [device (push/register-device! {:grant "GRANT-abcdef123456" :platform "ios"})]
                      (is (some? device))
                      (testing "but a send says exactly why it cannot happen, and keeps the device"
                        (let [result (push/send-to-device! device ALERT)]
                          (is (false? (:is-delivered result)))
                          (is (= "relay-not-configured" (:reason result))))
                        (is (= 1 (push/device-count))))))
                  (testing "a registration with neither token nor grant is refused"
                    (is (nil? (push/register-device! {:token "  "})))
                    (is (nil? (push/register-device! {}))))))
