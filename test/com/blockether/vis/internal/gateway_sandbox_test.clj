(ns com.blockether.vis.internal.gateway-sandbox-test
  "The SHARED gateway egress proxy/CA (internal.gateway-sandbox): the session REGISTRY
   + token-keyed policy resolution are asserted as pure data (FAIL-CLOSED on an unknown
   or absent token), then one hermetic in-process wire round-trip proves a registered
   token is attributed to its session's policy while an unknown/missing token is denied
   \u2014 one shared listener, many sessions, no external network."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [com.blockether.vis.internal.egress-proxy :as ep]
            [com.blockether.vis.internal.gateway-sandbox :as gs])
  (:import (java.io BufferedReader InputStreamReader)
           (java.net InetSocketAddress ServerSocket Socket)
           (java.util Base64)))

;; ---------------------------------------------------------------------------
;; Registry + resolver — pure, cross-platform (runs on Linux CI too)
;; ---------------------------------------------------------------------------

(deftest register-resolve-fail-closed
  (try (testing "unknown token ⇒ deny-all sentinel, and `decide` denies every request"
         (let [p (gs/resolve-policy "nope")]
           (is (:deny-all? p))
           (is (not (:allow? (ep/decide p "GET" "example.com" "/"))))
           (is (not (:allow? (ep/decide p nil "example.com" nil))))))
       (testing "nil token (missing Proxy-Authorization) ⇒ deny-all"
         (is (:deny-all? (gs/resolve-policy nil))))
       (testing "a registered token resolves to THAT session's policy"
         (let
           [tok
            "sess-A"

            pol
            (ep/compile-policy {:allowed-domains ["example.com"]})]

           (is (not (gs/registered? tok)))
           (gs/register-session! tok
                                 (fn []
                                   pol))
           (is (gs/registered? tok))
           (is (= (assoc pol :reserved-loopback-ports #{}) (gs/resolve-policy tok)))
           (is (:allow? (ep/decide (gs/resolve-policy tok) "GET" "example.com" "/")))
           (is (not (:allow? (ep/decide (gs/resolve-policy tok) "GET" "other.com" "/"))))))
       (testing "sessions are isolated — one token never resolves another's policy"
         (gs/register-session! "sess-B"
                               (fn []
                                 (ep/compile-policy {:allowed-domains ["beta.test"]})))
         (is (:allow? (ep/decide (gs/resolve-policy "sess-B") "GET" "beta.test" "/")))
         ;; sess-A's policy (example.com) must NOT admit sess-B's host.
         (is (not (:allow? (ep/decide (gs/resolve-policy "sess-A") "GET" "beta.test" "/")))))
       (testing "unregister drops a session back to fail-closed"
         (gs/unregister-session! "sess-A")
         (is (not (gs/registered? "sess-A")))
         (is (:deny-all? (gs/resolve-policy "sess-A"))))
       (finally (gs/shutdown!))))

;; ---------------------------------------------------------------------------
;; Wire round-trip — token attribution through the ONE shared proxy
;; ---------------------------------------------------------------------------

(defn- start-origin!
  "A one-request-per-connection HTTP origin on 127.0.0.1 that always answers 200 `ok`."
  []
  (let
    [server
     (doto (ServerSocket.) (.bind (InetSocketAddress. "127.0.0.1" 0)))

     running
     (atom true)

     loop-fn
     (fn []
       (while @running
         (when-let [c (try (.accept server) (catch Throwable _ nil))]
           (future (try
                     (let [in (BufferedReader. (InputStreamReader. (.getInputStream c)))]
                       (loop []

                         (let [l (.readLine in)]
                           (when (and l (not= l "")) (recur))))
                       (doto (.getOutputStream c)
                         (.write
                           (.getBytes
                             "HTTP/1.1 200 OK\r\nContent-Length: 2\r\nConnection: close\r\n\r\nok"))
                         (.flush)))
                     (catch Throwable _ nil)
                     (finally (try (.close c) (catch Throwable _ nil))))))))]

    (doto (Thread. ^Runnable loop-fn "gs-origin") (.setDaemon true) (.start))
    {:port (.getLocalPort server)
     :stop! (fn []
              (reset! running false)
              (try (.close server) (catch Throwable _ nil)))}))

(defn- basic-token
  [tok]
  (str "Basic " (.encodeToString (Base64/getEncoder) (.getBytes (str tok ":")))))

(defn- get-status
  "Raw absolute-form GET through the proxy with an optional Proxy-Authorization token.
   Returns the status line the client observes."
  [proxy-port origin-port token]
  (with-open [s (Socket.)]
    (.connect s (InetSocketAddress. "127.0.0.1" (int proxy-port)) 5000)
    (let
      [req (str "GET http://localhost:"
                origin-port
                "/ HTTP/1.1\r\n"
                "Host: localhost\r\n"
                (when token (str "Proxy-Authorization: " (basic-token token) "\r\n"))
                "Connection: close\r\n\r\n")]
      (.write (.getOutputStream s) (.getBytes req))
      (.flush (.getOutputStream s))
      (str (.readLine (BufferedReader. (InputStreamReader. (.getInputStream s))))))))

(deftest wire-token-attribution
  (let
    [origin
     (start-origin!)

     tok
     (str (java.util.UUID/randomUUID))]

    (gs/register-session! tok
                          (fn []
                            (ep/compile-policy {:allowed-domains ["localhost"]})))
    (let [port (gs/ensure-proxy!)]
      (try (testing "registered token ⇒ its policy applies; allowed host forwarded (200)"
             (is (str/includes? (get-status port (:port origin) tok) "200")))
           (testing "unknown token ⇒ fail-closed deny (403), never reaches origin"
             (is (str/includes? (get-status port (:port origin) "bogus-token") "403")))
           (testing "missing token ⇒ fail-closed deny (403)"
             (is (str/includes? (get-status port (:port origin) nil) "403")))
           (finally ((:stop! origin)) (gs/shutdown!))))))
