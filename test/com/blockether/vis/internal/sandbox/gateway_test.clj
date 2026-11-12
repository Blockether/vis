(ns com.blockether.vis.internal.sandbox.gateway-test
  "The SHARED gateway egress proxy/CA (internal.gateway-sandbox): the session REGISTRY
   + token-keyed policy resolution are asserted as pure data (FAIL-CLOSED on an unknown
   or absent token), then one hermetic in-process wire round-trip proves a registered
   token is attributed to its session's policy while an unknown/missing token is denied
   \u2014 one shared listener, many sessions, no external network."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [lazytest.core :refer [defdescribe expect it]]
            [com.blockether.vis-python-runtime :as runtime]
            [com.blockether.vis.internal.sandbox.egress-proxy :as ep]
            [com.blockether.vis.internal.sandbox.gateway :as gs]
            [com.blockether.vis.internal.sandbox.jail :as process-jail])
  (:import (java.io BufferedReader InputStreamReader)
           (java.net InetSocketAddress ServerSocket Socket)
           (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)
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
                          (gs/ensure-session-proxy! token token)
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

;; Direct loopback (#469) — the deny list must hold every port a later session takes

(defdescribe loopback-grant-denies-later-front-doors
             (it "a front door that a later session takes is already in an earlier deny list"
                 (try (gs/register-session! "early" (constantly nil))
                      (let [deny (set (:loopback-deny-ports (gs/loopback-grant nil)))]
                        (expect (contains? deny (gs/proxy-port)))
                        (gs/register-session! "late" (constantly nil))
                        (expect (contains? deny (gs/ensure-session-proxy! "late" "late"))))
                      (finally (gs/shutdown!))))
             (it "no grant when the allowlist does not already pass loopback"
                 (try (expect (nil? (gs/loopback-grant (ep/compile-policy {:allowed-domains
                                                                           ["example.com"]}))))
                      (expect (some? (gs/loopback-grant (ep/compile-policy {:allowed-domains
                                                                            ["example.com"
                                                                             "localhost"
                                                                             "127.0.0.1"]}))))
                      (finally (gs/shutdown!))))
             (it "a session past the reserved front doors gets none, not a port outside the list"
                 (try (let [deny
                            (set (:loopback-deny-ports (gs/loopback-grant nil)))

                            ports
                            (->> (range)
                                 (map (fn [i]
                                        (let [token (str "s" i)]
                                          (gs/register-session! token (constantly nil))
                                          (gs/ensure-session-proxy! token token))))
                                 (take 1000)
                                 (take-while some?)
                                 vec)]

                        (expect (seq ports))
                        (expect (every? deny ports))
                        (gs/register-session! "past" (constantly nil))
                        (expect (nil? (gs/ensure-session-proxy! "past" "past"))))
                      (finally (gs/shutdown!)))))

(defdescribe
  released-front-door-fails-closed
  (it "a front door stops serving when its session is gone"
      (run-wire-test (fn []
                       (let [origin
                             (start-origin!)

                             tok
                             (str (java.util.UUID/randomUUID))]

                         (try (gs/register-session!
                                tok
                                (constantly (ep/compile-policy {:allowed-domains ["localhost"]})))
                              (let [port (gs/ensure-session-proxy! tok tok)]
                                (expect (str/includes? (get-status port (:port origin) nil) "200"))
                                (gs/unregister-session! tok)
                                (expect (str/includes? (get-status port (:port origin) nil) "407")))
                              (finally ((:stop! origin)) (gs/shutdown!)))))))
  (it "a front door never serves a second session, also after its session ends"
      (try (let [take!
                 (fn [token]
                   (gs/register-session! token (constantly nil))
                   (let [p (gs/ensure-session-proxy! token token)]
                     (gs/unregister-session! token)
                     p))

                 ports
                 (vec (take-while some? (map #(take! (str "s" %)) (range 1000))))]

             (expect (seq ports))
             (expect (= (count ports) (count (set ports)))))
           (finally (gs/shutdown!))))
  (it "a session that its environment builds again keeps its one front door"
      (try (let [build!
                 (fn [token]
                   (gs/register-session! token (constantly nil))
                   (gs/ensure-session-proxy! token "sleeper"))

                 port
                 (build! "wake0")

                 wakes
                 (mapv #(do (gs/unregister-session! (str "wake" (dec %))) (build! (str "wake" %)))
                       (range 1 1000))]

             (expect (every? #{port} wakes))
             (gs/register-session! "other" (constantly nil))
             (expect (not= port (gs/ensure-session-proxy! "other" "other"))))
           (finally (gs/shutdown!))))
  (it "a door serves the newer of two live environments of one session"
      (run-wire-test
        (fn []
          (let [origin
                (start-origin!)

                status
                #(get-status % (:port origin) nil)]

            (try (gs/register-session! "old-env"
                                       (constantly (ep/compile-policy {:allowed-domains
                                                                       ["example.com"]})))
                 (gs/register-session! "new-env"
                                       (constantly (ep/compile-policy {:allowed-domains
                                                                       ["localhost"]})))
                 (let [port (gs/ensure-session-proxy! "old-env" "same")]
                   (expect (str/includes? (status port) "403"))
                   (expect (= port (gs/ensure-session-proxy! "new-env" "same")))
                   (expect (str/includes? (status port) "200"))
                   (gs/ensure-session-proxy! "old-env" "same")
                   (expect (str/includes? (status port) "200"))
                   (gs/unregister-session! "old-env")
                   (expect (str/includes? (status port) "200"))
                   (gs/unregister-session! "new-env")
                   (expect (str/includes? (status port) "407")))
                 (finally ((:stop! origin)) (gs/shutdown!))))))))

;; Another gateway (#473) — a child never reaches a port that another live Vis process holds

(defn- jailed-curl-status
  "The HTTP status that a confined curl with jail `owner` gets from `origin-port` through
   the proxy on `proxy-port`, or 000 when the proxy answers nothing."
  [owner proxy-port origin-port]
  (let [root
        (.toFile (Files/createTempDirectory "vis-door" (make-array FileAttribute 0)))

        ^Process p
        (runtime/spawn-process!
          ["/usr/bin/curl" "-s" "-o" "/dev/null" "-w" "%{http_code}" "-m" "10" "-x"
           (str "http://127.0.0.1:" proxy-port) (str "http://localhost:" origin-port "/")]
          {:environment {"PATH" "/usr/bin:/bin" "HOME" (str root) "TMPDIR" (str root)}
           :directory (str root)
           :policy {:read-write [(str root)] :network {:proxy proxy-port} :owner owner}})

        out
        (future (slurp (.getInputStream p)))]

    (.waitFor p)
    @out))

(defdescribe door-serves-only-its-own-jailed-children
             ;; #473: a jailed child of another gateway used a session door of this one.
             (it "a door refuses a jailed child of another Vis process and serves its own"
                 (when (= "Mac OS X" (System/getProperty "os.name"))
                   (run-wire-test
                     (fn []
                       (let [origin
                             (start-origin!)

                             tok
                             (str (java.util.UUID/randomUUID))]

                         (try (gs/register-session!
                                tok
                                (constantly (ep/compile-policy {:allowed-domains ["localhost"]})))
                              (let [door (gs/ensure-session-proxy! tok tok)]
                                (expect (= "200"
                                           (jailed-curl-status (process-jail/claim-owner!)
                                                               door
                                                               (:port origin))))
                                (expect (= "000" (jailed-curl-status "1-1" door (:port origin)))))
                              (finally ((:stop! origin)) (gs/shutdown!)))))))))

(defdescribe
  other-process-ports-stay-closed
  (it
    "a port that another live Vis process publishes is blocked, also after the policy exists"
    (let [dir
          (.toFile (Files/createTempDirectory "vis-live-ports" (make-array FileAttribute 0)))

          other
          (.start (ProcessBuilder. ["sleep" "30"]))

          owner
          (#'process-jail/process-owner (.toHandle other))

          tok
          (str (java.util.UUID/randomUUID))]

      (with-redefs-fn {#'gs/live-ports-dir (constantly dir)}
        (fn []
          (try
            (gs/register-session! tok (constantly (ep/compile-policy {:allowed-domains ["*"]})))
            (let [port
                  (gs/ensure-proxy!)

                  blocked?
                  #(:blocked (ep/safe-upstream-address "127.0.0.1" 4711 (gs/resolve-policy tok)))]

              (expect (= [port] (edn/read-string (slurp (io/file dir (process-jail/claim-owner!)))))
                      "this process publishes its own ports for the others")
              (expect (not (blocked?)))
              (spit (io/file dir owner) (pr-str [4711]))
              (expect (blocked?) "the other process started after the policy was read")
              (.destroyForcibly other)
              (.waitFor other)
              (expect (not (blocked?)) "the ports of an ended process are free again")
              (expect (not (.exists (io/file dir owner)))))
            (finally (.destroyForcibly other) (gs/shutdown!)))))))
  (it "a stopped process removes its published ports"
      (let [dir (.toFile (Files/createTempDirectory "vis-live-ports" (make-array FileAttribute 0)))]
        (with-redefs-fn {#'gs/live-ports-dir (constantly dir)}
          (fn []
            (try (gs/ensure-proxy!)
                 (expect (.exists (io/file dir (process-jail/claim-owner!))))
                 (gs/shutdown!)
                 (expect (not (.exists (io/file dir (process-jail/claim-owner!)))))
                 (finally (gs/shutdown!))))))))
