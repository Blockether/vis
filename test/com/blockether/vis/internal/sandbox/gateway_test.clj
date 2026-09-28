(ns com.blockether.vis.internal.sandbox.gateway-test
  "The SHARED gateway egress proxy/CA (internal.gateway-sandbox): the session REGISTRY
   + token-keyed policy resolution are asserted as pure data (FAIL-CLOSED on an unknown
   or absent token), then one hermetic in-process wire round-trip proves a registered
   token is attributed to its session's policy while an unknown/missing token is denied
   \u2014 one shared listener, many sessions, no external network."
  (:require [clojure.string :as str]
            [lazytest.core :refer [defdescribe expect it]]
            [com.blockether.vis-python-runtime :as runtime]
            [com.blockether.vis.internal.sandbox.egress-proxy :as ep]
            [com.blockether.vis.internal.sandbox.gateway :as gs])
  (:import (java.io BufferedReader InputStreamReader)
           (java.net InetSocketAddress ServerSocket Socket)
           (java.util Base64)))

(defn- run-wire-test
  [f]
  (if (runtime/jailed?)
    (expect true "conditional skip: inherited Seatbelt forbids test listeners")
    (f)))

;; Registry + resolver — pure, cross-platform (runs on Linux CI too)

(defdescribe
  register-resolve-fail-closed
  (it
    "register resolve fail closed"
    (try
      ;; unknown token ⇒ auth-required deny-all sentinel
      (let [p (gs/resolve-policy "nope")]
        (expect (:deny-all? p))
        (expect (:proxy-auth-required? p))
        (expect (not (:allow? (ep/decide p "GET" "example.com" "/"))))
        (expect (not (:allow? (ep/decide p nil "example.com" nil)))))
      ;; nil token (missing Proxy-Authorization) ⇒ auth-required deny-all
      (let [p (gs/resolve-policy nil)]
        (expect (:deny-all? p))
        (expect (:proxy-auth-required? p)))
      ;; a registered token resolves to THAT session's policy
      (let [tok
            "sess-A"

            pol
            (ep/compile-policy {:allowed-domains ["example.com"]})]

        (expect (not (gs/registered? tok)))
        (gs/register-session! tok
                              (fn []
                                pol))
        (expect (gs/registered? tok))
        (expect (= (assoc pol :reserved-loopback-ports (#'gs/reserved-loopback-ports))
                   (gs/resolve-policy tok)))
        (expect (:allow? (ep/decide (gs/resolve-policy tok) "GET" "example.com" "/")))
        (expect (not (:allow? (ep/decide (gs/resolve-policy tok) "GET" "other.com" "/")))))
      ;; sessions are isolated — one token never resolves another's policy
      (gs/register-session! "sess-B"
                            (fn []
                              (ep/compile-policy {:allowed-domains ["beta.test"]})))
      (expect (:allow? (ep/decide (gs/resolve-policy "sess-B") "GET" "beta.test" "/")))
      ;; sess-A's policy (example.com) must NOT admit sess-B's host.
      (expect (not (:allow? (ep/decide (gs/resolve-policy "sess-A") "GET" "beta.test" "/"))))
      ;; unregister drops a session back to fail-closed
      (gs/unregister-session! "sess-A")
      (expect (not (gs/registered? "sess-A")))
      (expect (:deny-all? (gs/resolve-policy "sess-A")))
      (finally (gs/shutdown!)))))

(defdescribe denied-egress-diagnostics-omit-private-request-data
             (it "denied egress diagnostics omit private request data"
                 (let [denied {:phase :connect
                               :host "repo.clojars.org"
                               :allow? false
                               :reason "private detail"
                               :path "/?token=secret"
                               :headers {"proxy-authorization" "secret"}}]
                   (expect (= {:source :vis-proxy :phase :connect :host "repo.clojars.org"}
                              (#'gs/denial-details denied)))
                   (expect (nil? (#'gs/denial-details (assoc denied :allow? true)))))))

(defdescribe both-proxies-install-denial-logging
             (it "both proxies install denial logging"
                 (let [started
                       (atom [])

                       token
                       "log-test"]

                   (with-redefs [ep/start! (fn [options]
                                             (swap! started conj options)
                                             {:port (+ 10000 (count @started))
                                              :stop! (fn []
                                                       nil)})]
                     (try (gs/register-session! token (constantly nil))
                          (gs/ensure-proxy!)
                          (gs/ensure-session-proxy! token)
                          (expect (= 2 (count @started)))
                          (expect (every? (comp fn? :on-log) @started))
                          (finally (gs/shutdown!)))))))

;; Wire round-trip — token attribution through the ONE shared proxy

(defn- start-origin!
  "A one-request-per-connection HTTP origin on 127.0.0.1 that always answers 200 `ok`."
  []
  (let [server
        (doto (ServerSocket.) (.bind (InetSocketAddress. "127.0.0.1" 0)))

        running
        (atom true)

        loop-fn
        (fn []
          (while @running
            (when-let [c (try (.accept server) (catch Throwable _ nil))]
              (future
                (try (let [in (BufferedReader. (InputStreamReader. (.getInputStream c)))]
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
    (let [req (str "GET http://localhost:"
                   origin-port
                   "/ HTTP/1.1\r\n"
                   "Host: localhost\r\n"
                   (when token (str "Proxy-Authorization: " (basic-token token) "\r\n"))
                   "Connection: close\r\n\r\n")]
      (.write (.getOutputStream s) (.getBytes req))
      (.flush (.getOutputStream s))
      (str (.readLine (BufferedReader. (InputStreamReader. (.getInputStream s))))))))

(defdescribe
  wire-token-attribution
  (it "wire token attribution"
      (run-wire-test
        (fn []
          (let [origin
                (start-origin!)

                tok
                (str (java.util.UUID/randomUUID))]

            (gs/register-session! tok
                                  (fn []
                                    (ep/compile-policy {:allowed-domains ["localhost"]})))
            (let [port (gs/ensure-proxy!)]
              (try
                ;; registered token ⇒ its policy applies; allowed host forwarded (200)
                (expect (str/includes? (get-status port (:port origin) tok) "200"))
                ;; unknown token ⇒ fail-closed auth challenge (407), never reaches origin
                (expect (str/includes? (get-status port (:port origin) "bogus-token") "407"))
                ;; missing token ⇒ fail-closed auth challenge (407)
                (expect (str/includes? (get-status port (:port origin) nil) "407"))
                (finally ((:stop! origin)) (gs/shutdown!)))))))))
