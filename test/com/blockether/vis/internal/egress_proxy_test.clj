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

(deftest exclude-domains-tunnel
  ;; `:exclude-domains` is the honest escape hatch for clients MITM cannot serve —
  ;; cert-pinned tools and mTLS upstreams (gh/Go-on-macOS, statically-trusted
  ;; binaries). Such hosts are still HOST-allowlisted, but the proxy must NOT
  ;; terminate their TLS — it tunnels opaquely, so verb/path is unenforced there.
  (let
    [pol (ep/compile-policy {:allowed-domains ["*"]
                             :rules [{:host "*" :access "read-only"}]
                             :exclude-domains ["GitHub.com" "*.pinned.example"]})]
    (testing "compile-policy carries normalized (lower-cased) :exclude-domains"
      (is (some? pol))
      (is (= ["github.com" "*.pinned.example"] (:exclude-domains pol))))
    (testing "excluded hosts + their subdomains/globs are MITM-excluded, others are not"
      (is (ep/mitm-excluded? pol "github.com"))
      (is (ep/mitm-excluded? pol "api.github.com"))
      (is (ep/mitm-excluded? pol "x.pinned.example"))
      (is (not (ep/mitm-excluded? pol "example.com")))))
  (testing "no :exclude-domains ⇒ nothing excluded"
    (let [pol (ep/compile-policy {:rules [{:host "h.example.com" :access "read-only"}]})]
      (is (not (ep/mitm-excluded? pol "h.example.com")))))
  (testing "nil policy ⇒ not excluded (proxy never engaged)"
    (is (not (ep/mitm-excluded? nil "anything"))))
  (testing "exclusion only skips TLS termination — host allow/deny still applies"
    (let [pol (ep/compile-policy {:allowed-domains ["good.com"] :exclude-domains ["good.com"]})]
      (is (:allow? (ep/decide pol nil "good.com" nil)))
      (is (not (:allow? (ep/decide pol nil "evil.com" nil)))))))

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
                       ;; drain remaining headers
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
     :seen seen
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
     (ep/start! {:policy-fn (fn [_token]
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
                 :policy-fn (fn [_token]
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

;; ---------------------------------------------------------------------------
;; Tier-2 network filters — the extension escape valve above :rules
;; ---------------------------------------------------------------------------

(deftest filter-registries
  (testing "request filter denies via the decrypted request; decide+filter honors it"
    (let [owner ::req-filt]
      (try
        (ep/register-network-filter! owner
                                     (fn [req]
                                       (when (= "POST" (:method req))
                                         {:allow? false :reason "no post"})))
        (let [pol (ep/compile-policy {:allowed-domains ["*"] :rules [{:host "*" :access "full"}]})]
          ;; tier-1 `full` allows POST; the tier-2 filter is what denies it.
          (is (:allow? (ep/decide+filter pol {:method "GET" :host "x.com" :path "/" :headers {}})))
          (is (not (:allow?
                     (ep/decide+filter pol {:method "POST" :host "x.com" :path "/" :headers {}})))))
        (finally (ep/unregister-network-filters-for-owner! owner)))))
  (testing "response filters: a throwing filter FAILS CLOSED; none registered ⇒ allow"
    (let [owner ::resp-filt]
      (try (ep/register-network-filter! owner
                                        (fn [_]
                                          (throw (RuntimeException. "boom"))))
           (is (not (:allow? (ep/apply-network-filters {:status 200 :headers {}}))))
           (finally (ep/unregister-network-filters-for-owner! owner)))
      (is (:allow? (ep/apply-network-filters {:status 200 :headers {}}))))))

(deftest response-filter-wire
  ;; A registered response filter sees the upstream status + headers and can
  ;; replace the response with a 403 — the child never receives the body.
  (let
    [origin
     (start-origin!)

     policy
     (ep/compile-policy {:allowed-domains ["localhost"]})

     owner
     ::resp-wire

     seen
     (atom nil)]

    (ep/register-network-filter!
      owner
      (fn [resp]
        (reset! seen resp)
        (if (= 200 (:status resp)) {:allow? false :reason "200 blocked"} {:allow? true})))
    (let
      [proxy (ep/start! {:policy-fn (fn [_token]
                                      policy)})]
      (try (testing "the upstream 200 is blocked at the proxy — child observes 403"
             (is (= 403 (http-through-proxy (:port proxy) "GET" (:port origin) "/"))))
           (testing "the filter saw the real upstream status + response headers"
             (is (= 200 (:status @seen)))
             (is (= "2" (get (:headers @seen) "content-length")))
             (is (= :http-response (:phase @seen))))
           (finally (ep/unregister-network-filters-for-owner! owner)
                    ((:stop! proxy))
                    ((:stop! origin)))))))

;; ---------------------------------------------------------------------------
;; SSRF deny-floor — pure, cross-platform. The proxy is an UNJAILED deputy, so
;; `allowed-domains ["*"]` must never mean "fetch the host's own trust plane."
;; ---------------------------------------------------------------------------

(deftest ssrf-deny-floor
  (testing "always-blocked (non-overridable even with allow-private?)"
    (doseq [h ["169.254.169.254" "0.0.0.0"]]
      (is (:blocked (ep/safe-upstream-address h nil {:allow-private? false}))
          (str h " must be blocked"))
      (is (:blocked (ep/safe-upstream-address h nil {:allow-private? true}))
          (str h " must stay blocked with allow-private"))))
  (testing
    "loopback (the user's OWN machine) is ALLOWED by default; only reserved gateway ports are not"
    (doseq [h ["127.0.0.1" "localhost"]]
      (is (:addr (ep/safe-upstream-address h 3000 {:allow-private? false}))
          (str h " local dev server reachable by default")))
    (is (:blocked (ep/safe-upstream-address "127.0.0.1" 7890 {:reserved-loopback-ports #{7890}}))
        "a reserved gateway port stays blocked"))
  (testing "private ranges (RFC1918/CGNAT/ULA): blocked by default, opt-in via allow-private?"
    (doseq [h ["10.0.0.1" "192.168.1.1" "172.16.0.1" "100.64.0.1"]]
      (is (:blocked (ep/safe-upstream-address h nil {:allow-private? false}))
          (str h " blocked by default"))
      (is (:addr (ep/safe-upstream-address h nil {:allow-private? true}))
          (str h " allowed under allow-private"))))
  (testing "a resolvable public host yields a validated IP literal to dial"
    (let [r (ep/safe-upstream-address "93.184.216.34" nil {:allow-private? false})]
      (is (instance? java.net.InetAddress (:addr r)))))
  (testing "unresolvable / nil host ⇒ blocked (fail closed)"
    (is (:blocked (ep/safe-upstream-address "no-such-host.invalid" nil {:allow-private? false})))
    (is (:blocked (ep/safe-upstream-address nil nil {:allow-private? false}))))
  (testing
    "compile-policy carries :allow-private? and forces a policy value even when domains are open"
    (is (true? (:allow-private? (ep/compile-policy {:allowed-domains ["*"] :allow-private true}))))
    (is (nil? (ep/compile-policy {:allowed-domains ["*"]})))))

(deftest loopback-policy
  (testing "loopback dev servers are ALLOWED by default — the agent runs on your own machine"
    (is (:addr (ep/safe-upstream-address "127.0.0.1" 3000 {})))
    (is (:addr (ep/safe-upstream-address "localhost" 5432 {}))))
  (testing "reserved gateway/proxy ports can NEVER be reached (non-overridable control-plane guard)"
    (is (:blocked (ep/safe-upstream-address "127.0.0.1" 7890 {:reserved-loopback-ports #{7890}}))))
  (testing "metadata / link-local stay blocked (separate always-on trust-plane floor)"
    (is (:blocked (ep/safe-upstream-address "169.254.169.254" 80 {}))))
  (testing "compile-policy no longer emits an :allow-loopback key"
    (is (nil? (ep/compile-policy {:allowed-domains ["*"]})))
    (is (nil? (:allow-loopback (ep/compile-policy {:allowed-domains ["example.com"]}))))))

(deftest ssrf-wire-blocks-reserved-loopback-port
  ;; End-to-end: loopback dev servers are reachable by default (see proxy-wire-roundtrip),
  ;; but the gateway's OWN reserved control-plane/proxy port can NEVER be reached through
  ;; the proxy — even under the friendly allow-all posture.
  (let
    [origin
     (start-origin!)

     ;; Allow-all domains + loopback, but mark THIS origin's port reserved (as if it were
     ;; the gateway control plane) and prove the confused-deputy proxy refuses it.
     proxy
     (ep/start! {:policy-fn (fn [_token]
                              {:reserved-loopback-ports #{(:port origin)}})})]

    (try (testing "a reserved loopback port is refused (403) even though loopback is allowed"
           (is (= 403 (http-through-proxy (:port proxy) "GET" (:port origin) "/"))))
         (testing "the blocked request never reached the reserved-port origin"
           (is (empty? @(:seen origin))))
         (finally ((:stop! proxy)) ((:stop! origin))))))
