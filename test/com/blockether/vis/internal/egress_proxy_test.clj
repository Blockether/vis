(ns com.blockether.vis.internal.egress-proxy-test
  "The shell-child egress proxy: the POLICY BRAIN (compile-policy + decide) is asserted
   as pure data — host allow/deny, per-host verb/path rules, presets, CONNECT host-only —
   so it runs on every OS (incl. Linux CI). Then one hermetic IN-PROCESS round-trip drives
   real HTTP through the proxy against a local origin, proving a GET is forwarded and a POST
   is denied at the wire with no external network."
  (:require [clojure.test :refer [deftest is testing]]
            [com.blockether.vis.internal.egress-proxy :as ep])
  (:import (java.io BufferedReader InputStreamReader)
           (java.net InetSocketAddress Proxy Proxy$Type ServerSocket URL HttpURLConnection)))

;; ---------------------------------------------------------------------------
;; Policy brain — pure, cross-platform
;; ---------------------------------------------------------------------------

(deftest compile-policy-shape
  (testing "no restriction ⇒ nil (caller skips the proxy entirely)"
    (is (nil? (ep/compile-policy {:allowed-domains ["*"] :denied-domains [] :rules []})))
    (is (nil? (ep/compile-policy {:allowed-domains [] :denied-domains []}))))
  (testing "any domain restriction or rule ⇒ a policy value"
    (is (some? (ep/compile-policy {:denied-domains ["evil.com"]})))
    (is (some? (ep/compile-policy {:allowed-domains ["example.com"]})))
    (is (some? (ep/compile-policy {:rules [{:host "api.example.com" :access "read-only"}]})))))

(deftest decide-host-allow-deny
  (let
    [pol (ep/compile-policy {:allowed-domains ["example.com"]
                             :denied-domains ["evil.example.com"]})]
    (testing "allow-list confines hosts (subdomains of an allowed apex pass)"
      (is (:allow? (ep/decide pol "GET" "example.com" "/")))
      (is (:allow? (ep/decide pol "GET" "api.example.com" "/")))
      (is (not (:allow? (ep/decide pol "GET" "other.com" "/")))))
    (testing "deny wins over allow"
      (is (not (:allow? (ep/decide pol "GET" "evil.example.com" "/")))))
    (testing "CONNECT (method nil) is host-only — allowed host tunnels, denied blocked"
      (is (:allow? (ep/decide pol nil "example.com" nil)))
      (is (not (:allow? (ep/decide pol nil "evil.example.com" nil)))))))

(deftest decide-verb-path-rules
  (let
    [pol (ep/compile-policy {:allowed-domains ["api.example.com"]
                             :rules [{:host "api.example.com"
                                      :access "read-only"
                                      :allow [{:method "POST" :path "/repos/**"}]}]})]
    (testing "read-only preset ⇒ GET/HEAD/OPTIONS pass, other verbs denied"
      (is (:allow? (ep/decide pol "GET" "api.example.com" "/x")))
      (is (:allow? (ep/decide pol "HEAD" "api.example.com" "/x")))
      (is (not (:allow? (ep/decide pol "POST" "api.example.com" "/x"))))
      (is (not (:allow? (ep/decide pol "DELETE" "api.example.com" "/x")))))
    (testing ":allow carves a per-path verb exception"
      (is (:allow? (ep/decide pol "POST" "api.example.com" "/repos/me/x")))
      (is (not (:allow? (ep/decide pol "POST" "api.example.com" "/issues")))))
    (testing "a host with NO rule is verb-unrestricted (still host-gated elsewhere)"
      (is (:allow? (ep/decide pol "POST" "api.example.com" "/repos/a")))))
  (testing "legacy :method-policy merges with :rules (GET-only)"
    (let
      [pol (ep/compile-policy {:method-policy {"h.example.com" ["GET"]}
                               :allowed-domains ["h.example.com"]})]
      (is (:allow? (ep/decide pol "GET" "h.example.com" "/")))
      (is (not (:allow? (ep/decide pol "POST" "h.example.com" "/")))))))

;; ---------------------------------------------------------------------------
;; In-process wire round-trip — hermetic (local origin, no external network)
;; ---------------------------------------------------------------------------

(defn- start-origin!
  "A one-request-per-connection HTTP origin on 127.0.0.1 that always answers 200 `ok`.
   Returns {:port :stop!}."
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
                       ;; drain request line + headers
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

    (doto (Thread. ^Runnable loop-fn "origin") (.setDaemon true) (.start))
    {:port (.getLocalPort server)
     :stop! (fn []
              (reset! running false)
              (try (.close server) (catch Throwable _ nil)))}))

(defn- http-through-proxy
  "Do `method http://localhost:<origin-port>/<path>` via the proxy at `proxy-port`.
   Returns the HTTP status code the client observes."
  [proxy-port method origin-port path]
  (let
    [proxy
     (Proxy. Proxy$Type/HTTP (InetSocketAddress. "127.0.0.1" (int proxy-port)))

     url
     (URL. (str "http://localhost:" origin-port path))

     ^HttpURLConnection conn
     (.openConnection url proxy)]

    (doto conn
      (.setRequestMethod method)
      (.setConnectTimeout 5000)
      (.setReadTimeout 5000)
      (.setInstanceFollowRedirects false))
    (when (= method "POST")
      (.setDoOutput conn true)
      (doto (.getOutputStream conn) (.write (.getBytes "x")) (.flush)))
    (try (.getResponseCode conn) (catch Throwable _ (.getResponseCode conn)))))

(deftest proxy-wire-roundtrip
  (let
    [origin
     (start-origin!)

     ;; localhost is allowed; read-only ⇒ GET forwarded, POST denied at the proxy.
     policy
     (ep/compile-policy {:allowed-domains ["localhost"]
                         :rules [{:host "localhost" :access "read-only"}]})

     proxy
     (ep/start! {:policy-fn (fn []
                              policy)})]

    (try (testing "GET to an allowed, read-only host is forwarded (origin 200)"
           (is (= 200 (http-through-proxy (:port proxy) "GET" (:port origin) "/"))))
         (testing "POST to a read-only host is denied at the proxy (403), never reaching origin"
           (is (= 403 (http-through-proxy (:port proxy) "POST" (:port origin) "/"))))
         (finally ((:stop! proxy)) ((:stop! origin))))))
