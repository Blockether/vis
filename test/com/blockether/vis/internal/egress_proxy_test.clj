(ns com.blockether.vis.internal.egress-proxy-test
  "The shell-child egress proxy: the POLICY BRAIN (compile-policy + decide) is asserted
   as pure data — host allow/deny, per-host verb/path rules, presets, CONNECT host-only —
   so it runs on every OS (incl. Linux CI). Then one hermetic IN-PROCESS round-trip drives
   real HTTP through the proxy against a local origin, proving a GET is forwarded and a POST
   is denied at the wire with no external network."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [com.blockether.vis.internal.egress-proxy :as ep]
            [com.blockether.vis.internal.tls-mitm :as tls])
  (:import (java.io BufferedReader InputStreamReader)
           (java.net InetSocketAddress Proxy Proxy$Type ServerSocket Socket URL HttpURLConnection)
           (java.security KeyStore)
           (javax.net.ssl SSLContext SSLServerSocket SSLSocket TrustManagerFactory)))

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
  (testing ":rules :methods (GET-only)"
    (let
      [pol (ep/compile-policy {:rules [{:host "h.example.com" :methods ["GET"]}]
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

;; ---------------------------------------------------------------------------
;; MITM (TLS-terminating) round-trip — HTTPS verb enforcement for shell children
;; ---------------------------------------------------------------------------

(defn- start-tls-origin!
  "A one-request-per-connection HTTPS origin on 127.0.0.1 using `server-ctx`. Records
   each request method into `:seen` and always answers 200. Returns {:port :stop! :seen}."
  [^SSLContext server-ctx]
  (let
    [server
     (doto ^SSLServerSocket (.createServerSocket (.getServerSocketFactory server-ctx))
       (.bind (InetSocketAddress. "127.0.0.1" 0)))

     running
     (atom true)

     seen
     (atom [])

     loop-fn
     (fn []
       (while @running
         (when-let [c (try (.accept server) (catch Throwable _ nil))]
           (future (try
                     (let
                       [in (BufferedReader. (InputStreamReader. (.getInputStream c)))
                        line (.readLine in)]

                       (swap! seen conj (first (str/split (str line) #"\s+")))
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

    (doto (Thread. ^Runnable loop-fn "tls-origin") (.setDaemon true) (.start))
    {:port (.getLocalPort server)
     :seen seen
     :stop! (fn []
              (reset! running false)
              (try (.close server) (catch Throwable _ nil)))}))

(defn- client-ctx-trusting
  "A client SSLContext that trusts only `ca-cert` (the ephemeral MITM CA)."
  ^SSLContext [ca-cert]
  (let
    [ks
     (doto (KeyStore/getInstance "PKCS12") (.load nil nil) (.setCertificateEntry "ca" ca-cert))

     tmf
     (doto (TrustManagerFactory/getInstance (TrustManagerFactory/getDefaultAlgorithm)) (.init ks))]

    (doto (SSLContext/getInstance "TLS") (.init nil (.getTrustManagers tmf) nil))))

(defn- https-through-proxy
  "CONNECT localhost:<origin-port> through the proxy, then a real TLS `method /` request
   over the tunnel (client trusts the CA + verifies the leaf hostname). Returns the status
   line the client observes, or an :error string."
  [proxy-port ^SSLContext client-ctx method origin-port]
  (let [raw (doto (Socket.) (.connect (InetSocketAddress. "127.0.0.1" (int proxy-port)) 5000))]
    (.write (.getOutputStream raw)
            (.getBytes
              (str "CONNECT localhost:" origin-port " HTTP/1.1\r\nHost: localhost\r\n\r\n")))
    (.flush (.getOutputStream raw))
    (let [cbr (BufferedReader. (InputStreamReader. (.getInputStream raw)))]
      (loop []

        (let [l (.readLine cbr)]
          (when (and l (not= l "")) (recur)))))
    (let
      [ssl ^SSLSocket
           (.createSocket (.getSocketFactory client-ctx) raw "localhost" (int origin-port) true)]
      (.setSSLParameters ssl
                         (doto (.getSSLParameters ssl)
                           (.setEndpointIdentificationAlgorithm "HTTPS")))
      (try (.startHandshake ssl)
           (.write (.getOutputStream ssl)
                   (.getBytes (str method
                                   " / HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n")))
           (.flush (.getOutputStream ssl))
           (let
             [br (BufferedReader. (InputStreamReader. (.getInputStream ssl)))
              status (.readLine br)]

             (.close ssl)
             status)
           (catch Throwable t (str "error: " (.getMessage t)))))))

(deftest proxy-mitm-https-roundtrip
  (let
    [cap
     (tls/create! {:upstream-trust-all? true})

     origin
     (start-tls-origin! ((:ctx-for cap) "localhost"))

     client-ctx
     (client-ctx-trusting (:ca-cert cap))

     ;; localhost read-only ⇒ GET forwarded, POST denied — over HTTPS via MITM.
     policy
     (assoc (ep/compile-policy {:allowed-domains ["localhost"]
                                :rules [{:host "localhost" :access "read-only"}]})
       :mitm? true)

     proxy
     (ep/start! {:mitm (fn []
                         cap)
                 :policy-fn (fn []
                              policy)})]

    (try (testing "the client accepts the proxy's ephemeral leaf (hostname-verified)"
           (is (= "HTTP/1.1 200 OK"
                  (https-through-proxy (:port proxy) client-ctx "GET" (:port origin)))))
         (testing "POST over HTTPS is read from the decrypted stream and denied at the proxy"
           (is (= "HTTP/1.1 403 Forbidden"
                  (https-through-proxy (:port proxy) client-ctx "POST" (:port origin)))))
         (testing "the denied POST never reached the origin; only the GET did"
           (is (= ["GET"] @(:seen origin))))
         (finally ((:stop! proxy)) ((:stop! origin)) ((:close! cap))))))
