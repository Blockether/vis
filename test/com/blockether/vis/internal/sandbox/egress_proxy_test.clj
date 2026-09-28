(ns com.blockether.vis.internal.sandbox.egress-proxy-test
  "The shell-child egress proxy: the POLICY BRAIN (compile-policy + decide) is asserted
   as pure data — host allow/deny, per-host verb/path rules, presets, CONNECT host-only —
   so it runs on every OS (incl. Linux CI). Then one hermetic IN-PROCESS round-trip drives
   real HTTP through the proxy against a local origin, proving a GET is forwarded and a POST
   is denied at the wire with no external network."
  (:require [clojure.string :as str]
            [lazytest.core :refer [defdescribe expect it]]
            [com.blockether.vis-python-runtime :as runtime]
            [com.blockether.vis.internal.sandbox.egress-proxy :as ep]
            [com.blockether.vis.internal.sandbox.tls-mitm :as tls])
  (:import (java.io BufferedReader InputStreamReader)
           (java.net InetSocketAddress Proxy Proxy$Type ServerSocket Socket URL HttpURLConnection)
           (java.security KeyStore)
           (javax.net.ssl SSLContext SSLServerSocket SSLSocket TrustManagerFactory)))

(defn- run-wire-test
  "Run socket integration only in an unconfined host JVM."
  [f]
  (if (runtime/jailed?)
    (expect true "conditional skip: inherited Seatbelt forbids test listeners")
    (f)))

;; Policy brain — pure, cross-platform

(defdescribe compile-policy-shape
             (it "no restriction ⇒ nil (caller skips the proxy entirely)"
                 (expect (nil? (ep/compile-policy
                                 {:allowed-domains ["*"] :denied-domains [] :rules []})))
                 (expect (nil? (ep/compile-policy {:allowed-domains [] :denied-domains []}))))
             (it "any domain restriction or rule ⇒ a policy value"
                 (expect (some? (ep/compile-policy {:denied-domains ["evil.com"]})))
                 (expect (some? (ep/compile-policy {:allowed-domains ["example.com"]})))
                 (expect (some? (ep/compile-policy {:rules [{:host "api.example.com"
                                                             :access "read-only"}]})))))

(defdescribe decide-host-allow-deny
             (it "decide host allow deny"
                 (let [pol (ep/compile-policy {:allowed-domains ["example.com"]
                                               :denied-domains ["evil.example.com"]})]
                   ;; allow-list confines hosts (subdomains of an allowed apex pass)
                   (expect (:allow? (ep/decide pol "GET" "example.com" "/")))
                   (expect (:allow? (ep/decide pol "GET" "api.example.com" "/")))
                   (expect (not (:allow? (ep/decide pol "GET" "other.com" "/"))))
                   ;; deny wins over allow
                   (expect (not (:allow? (ep/decide pol "GET" "evil.example.com" "/"))))
                   ;; CONNECT (method nil) is host-only — allowed host tunnels, denied blocked
                   (expect (:allow? (ep/decide pol nil "example.com" nil)))
                   (expect (not (:allow? (ep/decide pol nil "evil.example.com" nil)))))))

(defdescribe decide-verb-path-rules
             (it "decide verb path rules"
                 (let [pol (ep/compile-policy {:allowed-domains ["api.example.com"]
                                               :rules [{:host "api.example.com"
                                                        :access "read-only"
                                                        :allow [{:method "POST"
                                                                 :path "/repos/**"}]}]})]
                   ;; read-only preset ⇒ GET/HEAD/OPTIONS pass, other verbs denied
                   (expect (:allow? (ep/decide pol "GET" "api.example.com" "/x")))
                   (expect (:allow? (ep/decide pol "HEAD" "api.example.com" "/x")))
                   (expect (not (:allow? (ep/decide pol "POST" "api.example.com" "/x"))))
                   (expect (not (:allow? (ep/decide pol "DELETE" "api.example.com" "/x"))))
                   ;; :allow carves a per-path verb exception
                   (expect (:allow? (ep/decide pol "POST" "api.example.com" "/repos/me/x")))
                   (expect (not (:allow? (ep/decide pol "POST" "api.example.com" "/issues"))))
                   ;; a host with NO rule is verb-unrestricted (still host-gated elsewhere)
                   (expect (:allow? (ep/decide pol "POST" "api.example.com" "/repos/a"))))
                 ;; :rules :methods (GET-only)
                 (let [pol (ep/compile-policy {:rules [{:host "h.example.com" :methods ["GET"]}]
                                               :allowed-domains ["h.example.com"]})]
                   (expect (:allow? (ep/decide pol "GET" "h.example.com" "/")))
                   (expect (not (:allow? (ep/decide pol "POST" "h.example.com" "/")))))))

(defdescribe decide-port-rules
             (it "a rule's :ports restricts which ports reach the host (CONNECT/SOCKS path)"
                 (let [pol (ep/compile-policy {:allowed-domains ["*"]
                                               :rules [{:host "github.com" :ports [22 443]}]})]
                   (expect (:allow? (ep/decide pol nil "github.com" nil 443)))
                   (expect (:allow? (ep/decide pol nil "github.com" nil 22)))
                   (expect (not (:allow? (ep/decide pol nil "github.com" nil 6379))))
                   (expect (not (:allow? (ep/decide pol nil "github.com" nil 80))))))
             (it "a ports-only rule leaves verbs unrestricted on an allowed port"
                 (let [pol (ep/compile-policy {:allowed-domains ["*"]
                                               :rules [{:host "db.internal" :ports [5432]}]})]
                   (expect (:allow? (ep/decide pol "POST" "db.internal" "/" 5432)))
                   (expect (not (:allow? (ep/decide pol "POST" "db.internal" "/" 3306))))))
             (it "no :ports ⇒ any port (backward compatible)"
                 (let [pol (ep/compile-policy {:allowed-domains ["gh.example"]})]
                   (expect (:allow? (ep/decide pol nil "gh.example" nil 22)))
                   (expect (:allow? (ep/decide pol nil "gh.example" nil 443)))))
             (it "4-arity decide ignores ports (legacy callers unaffected)"
                 (let [pol (ep/compile-policy {:allowed-domains ["*"]
                                               :rules [{:host "github.com" :ports [443]}]})]
                   (expect (:allow? (ep/decide pol nil "github.com" nil)))))
             (it ":ports combine with verb rules on the same host"
                 (let [pol (ep/compile-policy
                             {:allowed-domains ["*"]
                              :rules [{:host "api.example.com" :access "read-only" :ports [443]}]})]
                   (expect (:allow? (ep/decide pol "GET" "api.example.com" "/x" 443)))
                   (expect (not (:allow? (ep/decide pol "POST" "api.example.com" "/x" 443)))) ; verb denied
                   (expect (not (:allow? (ep/decide pol "GET" "api.example.com" "/x" 8443)))))) ; port denied
             (it ":ports accepts numeric strings"
                 (let [pol (ep/compile-policy {:allowed-domains ["*"]
                                               :rules [{:host "h.example" :ports ["443"]}]})]
                   (expect (:allow? (ep/decide pol nil "h.example" nil 443)))
                   (expect (not (:allow? (ep/decide pol nil "h.example" nil 80)))))))

(defdescribe
  exclude-domains-tunnel
  (it "exclude domains tunnel"
      ;; `:exclude-domains` is the honest escape hatch for clients MITM cannot serve —
      ;; cert-pinned tools and mTLS upstreams (gh/Go-on-macOS, statically-trusted
      ;; binaries). Such hosts are still HOST-allowlisted, but the proxy must NOT
      ;; terminate their TLS — it tunnels opaquely, so verb/path is unenforced there.
      (let [pol (ep/compile-policy {:allowed-domains ["*"]
                                    :rules [{:host "*" :access "read-only"}]
                                    :exclude-domains ["GitHub.com" "*.pinned.example"]})]
        ;; compile-policy carries normalized (lower-cased) :exclude-domains
        (expect (some? pol))
        (expect (= ["github.com" "*.pinned.example"] (:exclude-domains pol)))
        ;; excluded hosts + their subdomains/globs are MITM-excluded, others are not
        (expect (ep/mitm-excluded? pol "github.com"))
        (expect (ep/mitm-excluded? pol "api.github.com"))
        (expect (ep/mitm-excluded? pol "x.pinned.example"))
        (expect (not (ep/mitm-excluded? pol "example.com"))))
      ;; no :exclude-domains ⇒ nothing excluded
      (let [pol (ep/compile-policy {:rules [{:host "h.example.com" :access "read-only"}]})]
        (expect (not (ep/mitm-excluded? pol "h.example.com"))))
      ;; nil policy ⇒ not excluded (proxy never engaged)
      (expect (not (ep/mitm-excluded? nil "anything")))
      ;; exclusion only skips TLS termination — host allow/deny still applies
      (let [pol (ep/compile-policy {:allowed-domains ["good.com"] :exclude-domains ["good.com"]})]
        (expect (:allow? (ep/decide pol nil "good.com" nil)))
        (expect (not (:allow? (ep/decide pol nil "evil.com" nil)))))))

;; In-process wire round-trip — hermetic (local origin, no external network)

(defn- start-origin!
  "A one-request-per-connection HTTP origin on 127.0.0.1 that always answers 200 `ok`.
   Returns {:port :stop!}."
  []
  (let [server
        (doto (ServerSocket.) (.bind (InetSocketAddress. "127.0.0.1" 0)))

        running
        (atom true)

        seen
        (atom [])

        loop-fn
        (fn []
          (while @running
            (when-let [c (try (.accept server) (catch Throwable _ nil))]
              (future
                (try (let [in (BufferedReader. (InputStreamReader. (.getInputStream c)))
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
  (let [proxy
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

(defdescribe proxy-wire-roundtrip
             (it
               "proxy wire roundtrip"
               (run-wire-test
                 (fn []
                   (let [origin
                         (start-origin!)

                         ;; localhost is allowed; read-only ⇒ GET forwarded, POST denied at the proxy.
                         policy
                         (ep/compile-policy {:allowed-domains ["localhost"]
                                             :rules [{:host "localhost" :access "read-only"}]})

                         proxy
                         (ep/start! {:policy-fn (fn [_token]
                                                  policy)})]

                     (try
                       ;; GET to an allowed, read-only host is forwarded (origin 200)
                       (expect (= 200 (http-through-proxy (:port proxy) "GET" (:port origin) "/")))
                       ;; POST to a read-only host is denied at the proxy (403), never reaching origin
                       (expect (= 403 (http-through-proxy (:port proxy) "POST" (:port origin) "/")))
                       (finally ((:stop! proxy)) ((:stop! origin)))))))))

;; MITM (TLS-terminating) round-trip — HTTPS verb enforcement for shell children

(defn- start-tls-origin!
  "A one-request-per-connection HTTPS origin on 127.0.0.1 using `server-ctx`. Records
   each request method into `:seen` and always answers 200. Returns {:port :stop! :seen}."
  [^SSLContext server-ctx]
  (let [server
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
              (future
                (try (let [in (BufferedReader. (InputStreamReader. (.getInputStream c)))
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
  (let [ks
        (doto (KeyStore/getInstance "PKCS12") (.load nil nil) (.setCertificateEntry "ca" ca-cert))

        tmf
        (doto (TrustManagerFactory/getInstance (TrustManagerFactory/getDefaultAlgorithm))
          (.init ks))]

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
    (let [ssl ^SSLSocket
              (.createSocket (.getSocketFactory client-ctx) raw "localhost" (int origin-port) true)]
      (.setSSLParameters ssl
                         (doto (.getSSLParameters ssl)
                           (.setEndpointIdentificationAlgorithm "HTTPS")))
      (try (.startHandshake ssl)
           (.write (.getOutputStream ssl)
                   (.getBytes (str method
                                   " / HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n")))
           (.flush (.getOutputStream ssl))
           (let [br (BufferedReader. (InputStreamReader. (.getInputStream ssl)))
                 status (.readLine br)]

             (.close ssl)
             status)
           (catch Throwable t (str "error: " (.getMessage t)))))))

(defdescribe
  proxy-mitm-https-roundtrip
  (it
    "proxy mitm https roundtrip"
    (run-wire-test
      (fn []
        (let [cap
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

          (try
            ;; the client accepts the proxy's ephemeral leaf (hostname-verified)
            (expect (= "HTTP/1.1 200 OK"
                       (https-through-proxy (:port proxy) client-ctx "GET" (:port origin))))
            ;; POST over HTTPS is read from the decrypted stream and denied at the proxy
            (expect (= "HTTP/1.1 403 Forbidden"
                       (https-through-proxy (:port proxy) client-ctx "POST" (:port origin))))
            ;; the denied POST never reached the origin; only the GET did
            (expect (= ["GET"] @(:seen origin)))
            (finally ((:stop! proxy)) ((:stop! origin)) ((:close! cap)))))))))

(defn- connect-status
  "Send a bare CONNECT localhost:<origin-port> through the proxy and return the
   first status line the proxy replies with (200 Connection Established when the
   tunnel is permitted, 403 Forbidden when the port/host is refused)."
  [proxy-port origin-port]
  (with-open [s (doto (Socket.) (.connect (InetSocketAddress. "127.0.0.1" (int proxy-port)) 5000))]
    (.setSoTimeout s 4000)
    (.write (.getOutputStream s)
            (.getBytes
              (str "CONNECT localhost:" origin-port " HTTP/1.1\r\nHost: localhost\r\n\r\n")))
    (.flush (.getOutputStream s))
    (.readLine (BufferedReader. (InputStreamReader. (.getInputStream s))))))

(defdescribe
  connect-port-allowlist-wire
  (it "connect port allowlist wire"
      ;; The :ports allowlist gates HTTPS CONNECT exactly as it gates SOCKS — the
      ;; proxy reads only host:port from the CONNECT line, so the port is the finest
      ;; filter available for an opaque (non-MITM) TLS tunnel.
      (run-wire-test
        (fn []
          (let [origin (start-origin!)]
            (try
              ;; CONNECT to a port in the host's :ports is tunnelled (200)
              (let [proxy (ep/start! {:policy-fn (fn [_]
                                                   (ep/compile-policy
                                                     {:allowed-domains ["*"]
                                                      :rules [{:host "localhost"
                                                               :ports [(:port origin)]}]}))})]
                (try (expect (= "HTTP/1.1 200 Connection Established"
                                (connect-status (:port proxy) (:port origin))))
                     (finally ((:stop! proxy)))))
              ;; CONNECT to a port NOT in :ports is refused (403) before any tunnel
              (let [proxy (ep/start! {:policy-fn (fn [_]
                                                   (ep/compile-policy {:allowed-domains ["*"]
                                                                       :rules [{:host "localhost"
                                                                                :ports [1]}]}))})]
                (try (expect (= "HTTP/1.1 403 Forbidden"
                                (connect-status (:port proxy) (:port origin))))
                     (finally ((:stop! proxy)))))
              (finally ((:stop! origin)))))))))

(defn- https-post-body
  "POST `body` over a MITM tunnel to origin (client trusts the CA); return the
   status line the client observes, or an :error string."
  [proxy-port ^SSLContext client-ctx origin-port ^String body]
  (let [raw (doto (Socket.) (.connect (InetSocketAddress. "127.0.0.1" (int proxy-port)) 5000))]
    (.write (.getOutputStream raw)
            (.getBytes
              (str "CONNECT localhost:" origin-port " HTTP/1.1\r\nHost: localhost\r\n\r\n")))
    (.flush (.getOutputStream raw))
    (let [cbr (BufferedReader. (InputStreamReader. (.getInputStream raw)))]
      (loop []

        (let [l (.readLine cbr)]
          (when (and l (not= l "")) (recur)))))
    (let [ssl ^SSLSocket
              (.createSocket (.getSocketFactory client-ctx) raw "localhost" (int origin-port) true)]
      (.setSSLParameters ssl
                         (doto (.getSSLParameters ssl)
                           (.setEndpointIdentificationAlgorithm "HTTPS")))
      (try (.startHandshake ssl)
           (let [payload (.getBytes body)]
             (.write (.getOutputStream ssl)
                     (.getBytes (str "POST / HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n"
                                     "Content-Length: "
                                     (count payload)
                                     "\r\n\r\n")))
             (.write (.getOutputStream ssl) payload)
             (.flush (.getOutputStream ssl)))
           (let [br (BufferedReader. (InputStreamReader. (.getInputStream ssl)))
                 status (.readLine br)]

             (.close ssl)
             status)
           (catch Throwable t (str "error: " (.getMessage t)))))))

(defdescribe
  mitm-body-filter-wire
  (it "mitm body filter wire"
      ;; A tier-2 filter sees the DECRYPTED request `:body` on the MITM path: the
      ;; proxy buffers a small Content-Length body BEFORE deciding, so a content rule
      ;; (deny bodies containing "SECRET") is enforceable over HTTPS — not just verb/path.
      (run-wire-test
        (fn []
          (let [cap
                (tls/create! {:upstream-trust-all? true})

                origin
                (start-tls-origin! ((:ctx-for cap) "localhost"))

                client-ctx
                (client-ctx-trusting (:ca-cert cap))

                policy
                (assoc (ep/compile-policy {:allowed-domains ["localhost"]
                                           :rules [{:host "localhost" :access "full"}]})
                  :mitm? true)

                proxy
                (ep/start! {:mitm (fn []
                                    cap)
                            :policy-fn (fn [_]
                                         policy)})

                owner
                ::body-filt]

            (try (ep/register-network-filter!
                   owner
                   (fn [req]
                     (when (and (:body req) (str/includes? (str (:body req)) "SECRET"))
                       {:allow? false :reason "secret in body"})))
                 ;; a POST whose decrypted body contains SECRET is denied at the proxy
                 (expect
                   (=
                     "HTTP/1.1 403 Forbidden"
                     (https-post-body (:port proxy) client-ctx (:port origin) "hello SECRET data")))
                 ;; the denied POST never reached the origin
                 (expect (= [] @(:seen origin)))
                 ;; a POST with a clean body is forwarded and reaches the origin
                 (expect
                   (= "HTTP/1.1 200 OK"
                      (https-post-body (:port proxy) client-ctx (:port origin) "hello clean data")))
                 (expect (= ["POST"] @(:seen origin)))
                 (finally (ep/unregister-network-filters-for-owner! owner)
                          ((:stop! proxy))
                          ((:stop! origin))
                          ((:close! cap)))))))))

;; Tier-2 network filters — the extension escape valve above :rules

(defdescribe
  non-extension-network-filters-test
  (it "free-standing filters stay whatever their owner; descriptor filters are left to sessions"
      (let [free
            (fn [_]
              nil)

            before
            (count (ep/non-extension-network-filters))]

        (try (ep/register-network-filter! :ext/free-standing-filter-test free)
             (ep/register-network-filter! :ext/descriptor-filter-test
                                          (fn [_]
                                            nil)
                                          {:descriptor? true})
             (expect (some #(identical? free %) (ep/non-extension-network-filters)))
             (expect (= (inc before) (count (ep/non-extension-network-filters))))
             (finally (ep/unregister-network-filters-for-owner! :ext/free-standing-filter-test)
                      (ep/unregister-network-filters-for-owner! :ext/descriptor-filter-test))))))

(defdescribe
  filter-registries
  (it "request filter denies via the decrypted request; decide+filter honors it"
      (let [owner ::req-filt]
        (try (ep/register-network-filter! owner
                                          (fn [req]
                                            (when (= "POST" (:method req))
                                              {:allow? false :reason "no post"})))
             (let [pol (ep/compile-policy {:allowed-domains ["*"]
                                           :rules [{:host "*" :access "full"}]})]
               ;; tier-1 `full` allows POST; the tier-2 filter is what denies it.
               (expect (:allow? (ep/decide+filter
                                  pol
                                  {:method "GET" :host "x.com" :path "/" :headers {}})))
               (expect (not (:allow? (ep/decide+filter
                                       pol
                                       {:method "POST" :host "x.com" :path "/" :headers {}})))))
             (finally (ep/unregister-network-filters-for-owner! owner)))))
  (it "response filters: a throwing filter FAILS CLOSED; none registered ⇒ allow"
      (let [owner ::resp-filt]
        (try (ep/register-network-filter! owner
                                          (fn [_]
                                            (throw (RuntimeException. "boom"))))
             (expect (not (:allow? (ep/apply-network-filters {:status 200 :headers {}}))))
             (finally (ep/unregister-network-filters-for-owner! owner)))
        (expect (:allow? (ep/apply-network-filters {:status 200 :headers {}}))))))

(defdescribe
  response-filter-wire
  (it
    "response filter wire"
    (run-wire-test
      (fn []
        ;; A registered response filter sees the upstream status + headers and can
        ;; replace the response with a 403 — the child never receives the body.
        (let [origin
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
          (let [proxy (ep/start! {:policy-fn (fn [_token]
                                               policy)})]
            (try
              ;; the upstream 200 is blocked at the proxy — child observes 403
              (expect (= 403 (http-through-proxy (:port proxy) "GET" (:port origin) "/")))
              ;; the filter saw the real upstream status + response headers
              (expect (= 200 (:status @seen)))
              (expect (= "2" (get (:headers @seen) "content-length")))
              (expect (= :http-response (:phase @seen)))
              (finally (ep/unregister-network-filters-for-owner! owner)
                       ((:stop! proxy))
                       ((:stop! origin))))))))))

(defdescribe
  project-response-filter-wire
  (it
    "project response filter wire"
    (run-wire-test
      (fn []
        (let [origin
              (start-origin!)

              seen
              (atom [])

              base
              (ep/compile-policy {:allowed-domains ["localhost"]})

              policy-a
              (assoc base
                :network-filters-fn (fn []
                                      [(fn [response]
                                         (swap! seen conj (:phase response))
                                         (when (= 200 (:status response))
                                           {:allow? false
                                            :reason "Project A blocks this response"}))]))

              policy-b
              (assoc base :network-filters-fn (constantly []))

              proxy-a
              (ep/start! {:policy-fn (constantly policy-a)})

              proxy-b
              (ep/start! {:policy-fn (constantly policy-b)})]

          (try (expect (= 403 (http-through-proxy (:port proxy-a) "GET" (:port origin) "/")))
               (expect (= 200 (http-through-proxy (:port proxy-b) "GET" (:port origin) "/")))
               (expect (= 403 (http-through-proxy (:port proxy-a) "GET" (:port origin) "/")))
               (expect (= 2 (count (filter #{:http-response} @seen))))
               (finally ((:stop! proxy-a)) ((:stop! proxy-b)) ((:stop! origin)))))))))

;; SSRF deny-floor — pure, cross-platform. The proxy is an UNJAILED deputy, so
;; `allowed-domains ["*"]` must never mean "fetch the host's own trust plane."

(defdescribe
  ssrf-deny-floor
  (it "always-blocked (non-overridable even with allow-private?)"
      (doseq [h ["169.254.169.254" "0.0.0.0"]]
        (expect (:blocked (ep/safe-upstream-address h nil {:allow-private? false}))
                (str h " must be blocked"))
        (expect (:blocked (ep/safe-upstream-address h nil {:allow-private? true}))
                (str h " must stay blocked with allow-private"))))
  (it "loopback (the user's OWN machine) is ALLOWED by default; only reserved gateway ports are not"
      (doseq [h ["127.0.0.1" "localhost"]]
        (expect (:addr (ep/safe-upstream-address h 3000 {:allow-private? false}))
                (str h " local dev server reachable by default")))
      (expect (:blocked
                (ep/safe-upstream-address "127.0.0.1" 7890 {:reserved-loopback-ports #{7890}}))
              "a reserved gateway port stays blocked"))
  (it "private ranges (RFC1918/CGNAT/ULA): blocked by default, opt-in via allow-private?"
      (doseq [h ["10.0.0.1" "192.168.1.1" "172.16.0.1" "100.64.0.1"]]
        (expect (:blocked (ep/safe-upstream-address h nil {:allow-private? false}))
                (str h " blocked by default"))
        (expect (:addr (ep/safe-upstream-address h nil {:allow-private? true}))
                (str h " allowed under allow-private"))))
  (it "a resolvable public host yields a validated IP literal to dial"
      (let [r (ep/safe-upstream-address "93.184.216.34" nil {:allow-private? false})]
        (expect (instance? java.net.InetAddress (:addr r)))))
  (it "unresolvable / nil host ⇒ blocked (fail closed)"
      (expect (:blocked
                (ep/safe-upstream-address "no-such-host.invalid" nil {:allow-private? false})))
      (expect (:blocked (ep/safe-upstream-address nil nil {:allow-private? false}))))
  (it "compile-policy carries :allow-private? and forces a policy value even when domains are open"
      (expect (true? (:allow-private? (ep/compile-policy {:allowed-domains ["*"]
                                                          :allow-private true}))))
      (expect (nil? (ep/compile-policy {:allowed-domains ["*"]})))))

(defdescribe
  loopback-policy
  (it "loopback dev servers are ALLOWED by default — the agent runs on your own machine"
      (expect (:addr (ep/safe-upstream-address "127.0.0.1" 3000 {})))
      (expect (:addr (ep/safe-upstream-address "localhost" 5432 {}))))
  (it "reserved gateway/proxy ports can NEVER be reached (non-overridable control-plane guard)"
      (expect (:blocked
                (ep/safe-upstream-address "127.0.0.1" 7890 {:reserved-loopback-ports #{7890}}))))
  (it "metadata / link-local stay blocked (separate always-on trust-plane floor)"
      (expect (:blocked (ep/safe-upstream-address "169.254.169.254" 80 {}))))
  (it "compile-policy no longer emits an :allow-loopback key"
      (expect (nil? (ep/compile-policy {:allowed-domains ["*"]})))
      (expect (nil? (:allow-loopback (ep/compile-policy {:allowed-domains ["example.com"]}))))))

(defdescribe denied-domain-blocks-ip
             (it "a denied domain is blocked whether the child dials the NAME or its resolved IP"
                 ;; Use the local hosts entry, not public DNS: an outage must neither fail this
                 ;; test nor silently skip its raw-IP bypass assertion.
                 (let [pol
                       (ep/compile-policy {:allowed-domains ["*"] :denied-domains ["localhost"]})

                       ip
                       (.getHostAddress (java.net.InetAddress/getByName "localhost"))]

                   (expect (false? (:allow? (ep/decide pol nil "localhost" nil 443))))
                   (expect (= "blocked denied-domain address for host: localhost"
                              (:blocked (ep/safe-upstream-address "localhost" 443 pol))))
                   ;; Name matching alone allows the IP, but the dial chokepoint must deny it.
                   (expect (:allow? (ep/decide pol nil ip nil 443)))
                   (expect (= (str "blocked denied-domain address for host: " ip)
                              (:blocked (ep/safe-upstream-address ip 443 pol))))
                   ;; A public IP needs no DNS and remains allowed.
                   (expect (:addr (ep/safe-upstream-address "93.184.216.34" 443 pol))))))

(defdescribe
  host-policy-specificity
  ;; The egress proxy owns the allow/deny verdict. A SPECIFIC (non-`*`) match wins
  ;; over a `*` in the OTHER list, so `denied ["*"]` + `allowed ["example.com"]`
  ;; means "deny everything EXCEPT example.com" — NOT "deny wins unconditionally."
  (it "denied `*` + a specific allow ⇒ deny all EXCEPT the allowlist (subdomains too)"
      (let [pol (ep/compile-policy {:denied-domains ["*"] :allowed-domains ["example.com"]})]
        (expect (:allow? (ep/decide pol nil "example.com" nil)))
        (expect (:allow? (ep/decide pol nil "www.example.com" nil)))
        (expect (not (:allow? (ep/decide pol nil "evil.com" nil))))))
  (it "allow `*` + a specific deny ⇒ allow all EXCEPT the denylist"
      (let [pol (ep/compile-policy {:allowed-domains ["*"] :denied-domains ["example.com"]})]
        (expect (not (:allow? (ep/decide pol nil "example.com" nil))))
        (expect (not (:allow? (ep/decide pol nil "www.example.com" nil))))
        (expect (:allow? (ep/decide pol nil "other.com" nil)))))
  (it "a host on both specific lists is denied (fail safe)"
      (let [pol (ep/compile-policy {:allowed-domains ["example.com"]
                                    :denied-domains ["example.com"]})]
        (expect (not (:allow? (ep/decide pol nil "example.com" nil))))))
  (it "both `*` ⇒ deny wins"
      (let [pol (ep/compile-policy {:allowed-domains ["*"] :denied-domains ["*"]})]
        (expect (not (:allow? (ep/decide pol nil "anything.com" nil)))))))

(defdescribe ssrf-wire-blocks-reserved-loopback-port
             (it
               "ssrf wire blocks reserved loopback port"
               (run-wire-test
                 (fn []
                   ;; End-to-end: loopback dev servers are reachable by default (see proxy-wire-roundtrip),
                   ;; but the gateway's OWN reserved control-plane/proxy port can NEVER be reached through
                   ;; the proxy — even under the friendly allow-all posture.
                   (let [origin
                         (start-origin!)

                         ;; Allow-all domains + loopback, but mark THIS origin's port reserved (as if it were
                         ;; the gateway control plane) and prove the confused-deputy proxy refuses it.
                         proxy
                         (ep/start! {:policy-fn (fn [_token]
                                                  {:reserved-loopback-ports #{(:port origin)}})})]

                     (try
                       ;; a reserved loopback port is refused (403) even though loopback is allowed
                       (expect (= 403 (http-through-proxy (:port proxy) "GET" (:port origin) "/")))
                       ;; the blocked request never reached the reserved-port origin
                       (expect (empty? @(:seen origin)))
                       (finally ((:stop! proxy)) ((:stop! origin)))))))))

(defdescribe policy-hot-path-performance-budget
             (it "policy hot path performance budget"
                 ;; A generous regression ceiling, not a benchmark claim: policy evaluation must
                 ;; remain cheap enough to run synchronously on every decrypted request.
                 (let [policy
                       (ep/compile-policy {:allowed-domains ["*"]
                                           :denied-domains ["evil.example"]
                                           :rules [{:host "api.example.com"
                                                    :access "read-only"
                                                    :allow [{:method "POST" :path "/v1/issues/**"}]}
                                                   {:host "*" :access "read-only"}]})

                       started
                       (System/nanoTime)]

                   (dotimes [i 50000]
                     (let [method (if (zero? (bit-and i 1)) "GET" "POST")
                           path (if (zero? (mod i 3)) "/v1/issues/42" "/v1/other")]

                       (ep/decide policy method "api.example.com" path)))
                   (let [elapsed-ms (/ (- (System/nanoTime) started) 1000000.0)]
                     (expect (< elapsed-ms 5000.0)
                             (str "50k policy decisions exceeded the 5s regression budget: "
                                  elapsed-ms
                                  "ms"))))))

;; SOCKS5 lane — generic-TCP door (ssh / git+ssh / db / raw TCP), multiplexed on
;; the SAME loopback port as the HTTP proxy (first byte 0x05). Host allow/deny +
;; SSRF floor + token attribution, no verb/path.

(defn- socks5-http
  "Through the SOCKS5 lane at `proxy-port`, negotiate (no-auth, or user/pass when
   `:token` is given → RFC 1929) then CONNECT to `host`:`origin-port` and do a bare
   HTTP/1.0 GET. Returns {:rep <socks-reply-code> :status <http-status-or-nil>}."
  [proxy-port host origin-port & {:keys [token]}]
  (with-open [s (Socket. "127.0.0.1" (int proxy-port))]
    (.setSoTimeout s 4000)
    (let [in (.getInputStream s)
          out (.getOutputStream s)
          rd2 (fn []
                (let [b (byte-array 2)]
                  (.read in b)
                  b))]

      (if token
        (let [ub (.getBytes ^String token "UTF-8")]
          (.write out (byte-array (map unchecked-byte [0x05 0x01 0x02])))
          (.flush out)
          (rd2)
          (.write out (byte-array (concat [0x01 (count ub)] (map unchecked-byte ub) [0x00])))
          (.flush out)
          (rd2))
        (do (.write out (byte-array (map unchecked-byte [0x05 0x01 0x00]))) (.flush out) (rd2)))
      (let [hb (.getBytes ^String host "UTF-8")]
        (.write out
                (byte-array (concat [0x05 0x01 0x00 0x03 (count hb)]
                                    (map unchecked-byte hb)
                                    [(bit-and 0xff (bit-shift-right (int origin-port) 8))
                                     (bit-and 0xff (int origin-port))])))
        (.flush out))
      (let [^bytes rep (byte-array 10)]
        (.read in rep)
        (let [code (bit-and 0xff (long (aget rep 1)))]
          (if (zero? code)
            (do (.write out (.getBytes "GET / HTTP/1.0\r\n\r\n" "UTF-8"))
                (.flush out)
                (let [b (byte-array 64)
                      n (.read in b)]

                  {:rep 0 :status (when (re-find #"\b200\b" (String. b 0 (max 0 n) "UTF-8")) 200)}))
            {:rep code}))))))

(defdescribe
  socks5-lane-wire
  (it
    "socks5 lane wire"
    (run-wire-test
      (fn []
        (let [origin (start-origin!)]
          (try
            ;; no-auth CONNECT to an allowed loopback origin relays raw TCP (HTTP over SOCKS)
            (let [proxy (ep/start! {:policy-fn (fn [_]
                                                 (ep/compile-policy {:allowed-domains ["*"]}))})]
              (try (let [r (socks5-http (:port proxy) "localhost" (:port origin))]
                     (expect (= 0 (:rep r)) "SOCKS5 CONNECT succeeded")
                     (expect (= 200 (:status r)) "bytes relayed end to end"))
                   (finally ((:stop! proxy)))))
            ;; token attributes the session (RFC 1929 username) — unknown token fails closed
            (let [proxy (ep/start! {:policy-fn (fn [t]
                                                 (if (= t "sess")
                                                   (ep/compile-policy {:allowed-domains ["*"]})
                                                   {:deny-all? true}))})]
              (try
                (expect
                  (= 0 (:rep (socks5-http (:port proxy) "localhost" (:port origin) :token "sess"))))
                (expect
                  (= 2 (:rep (socks5-http (:port proxy) "localhost" (:port origin) :token "nope"))))
                (finally ((:stop! proxy)))))
            ;; host allow/deny is enforced (denied host ⇒ REP 0x02 not-allowed)
            (let [proxy (ep/start! {:policy-fn (fn [_]
                                                 (ep/compile-policy {:allowed-domains ["*"]
                                                                     :denied-domains
                                                                     ["localhost"]}))})]
              (try (expect (= 2 (:rep (socks5-http (:port proxy) "localhost" (:port origin)))))
                   (finally ((:stop! proxy)))))
            ;; the SSRF floor applies: a reserved gateway/proxy port is refused
            (let [proxy (ep/start! {:policy-fn (fn [_]
                                                 {:reserved-loopback-ports #{(:port origin)}})})]
              (try (expect (= 2 (:rep (socks5-http (:port proxy) "localhost" (:port origin)))))
                   (finally ((:stop! proxy)))))
            ;; a registered network_filter intercepts SOCKS (same guard as HTTP): denies on :phase :socks, allows when it doesn't match
            (let [owner (gensym "socks-filter")
                  proxy (ep/start! {:policy-fn (fn [_]
                                                 (ep/compile-policy {:allowed-domains ["*"]}))})]

              (try (ep/register-network-filter! owner
                                                (fn [ctx]
                                                  (when (= :socks (:phase ctx))
                                                    {:allow? false
                                                     :reason "socks denied by filter"})))
                   (expect (= 2 (:rep (socks5-http (:port proxy) "localhost" (:port origin))))
                           "filter denies the SOCKS connection ⇒ REP 0x02")
                   (ep/unregister-network-filters-for-owner! owner)
                   (expect (= 0 (:rep (socks5-http (:port proxy) "localhost" (:port origin))))
                           "no matching filter ⇒ SOCKS connection allowed")
                   (finally (ep/unregister-network-filters-for-owner! owner) ((:stop! proxy)))))
            ;; a rule's :ports gates which port the SOCKS CONNECT may reach
            (let [proxy (ep/start! {:policy-fn (fn [_]
                                                 (ep/compile-policy
                                                   {:allowed-domains ["*"]
                                                    :rules [{:host "localhost"
                                                             :ports [(:port origin)]}]}))})]
              (try (expect (= 0 (:rep (socks5-http (:port proxy) "localhost" (:port origin))))
                           "the origin's port is in :ports ⇒ CONNECT allowed")
                   (finally ((:stop! proxy)))))
            (let [proxy (ep/start! {:policy-fn (fn [_]
                                                 (ep/compile-policy {:allowed-domains ["*"]
                                                                     :rules [{:host "localhost"
                                                                              :ports [1]}]}))})]
              (try (expect (= 2 (:rep (socks5-http (:port proxy) "localhost" (:port origin))))
                           "a port not in :ports ⇒ REP 0x02 not-allowed")
                   (finally ((:stop! proxy)))))
            (finally ((:stop! origin)))))))))

(defdescribe
  probe-engine
  (it
    "probe runs tier-1 + each filter individually, no collapse, surfaces per-filter error"
    (let [pol
          (ep/compile-policy {:allowed-domains ["*"]})

          o1
          (gensym "ok")

          o2
          (gensym "blk")

          o3
          (gensym "bug")]

      (try (ep/register-network-filter! o1
                                        (fn [_]
                                          nil))
           (ep/register-network-filter! o2
                                        (fn [c]
                                          (when (= "POST" (:method c))
                                            {:allow? false :reason "no POST"})))
           (ep/register-network-filter! o3
                                        (fn [_]
                                          (throw (ex-info "boom" {}))))
           (let [{:keys [tier1 filters final]}
                 (ep/probe
                   pol
                   {:phase :http :method "POST" :host "api.example.com" :path "/x" :port 443})

                 by
                 (into {} (map (juxt :owner identity)) filters)]

             (expect (:allow? tier1) "host allowed at tier-1")
             (expect (= 3 (count filters)) "every registered filter reported, not collapsed")
             (expect (:allow? (by o1)) "clean filter allows")
             (expect (false? (:allow? (by o2))) "blocker denies")
             (expect (= "no POST" (:reason (by o2))))
             (expect (nil? (:error (by o2))) "an intentional block carries no :error")
             (expect (false? (:allow? (by o3))) "throwing filter denies (fail-closed)")
             (expect (some? (:error (by o3))) "a crash surfaces a structured :error")
             (expect (false? (:allow? final)) "final = first deny wins"))
           (finally (doseq [o [o1 o2 o3]]
                      (ep/unregister-network-filters-for-owner! o))))))
  (it "tier-1 deny short-circuits: filters never run"
      (let [pol
            (ep/compile-policy {:allowed-domains ["github.com"]})

            o
            (gensym "should-not-run")]

        (try (ep/register-network-filter! o
                                          (fn [_]
                                            (throw (ex-info "must-not-run" {}))))
             (let [{:keys [tier1 filters final]}
                   (ep/probe pol {:phase :http :method "GET" :host "evil.com" :path "/" :port 443})]
               (expect (false? (:allow? tier1)) "host denied at tier-1")
               (expect (empty? filters) "no filter runs once tier-1 denies")
               (expect (false? (:allow? final))))
             (finally (ep/unregister-network-filters-for-owner! o))))))

;; CONNECT tunnel liveness — a read timeout is a POLL, not an EOF

(defn- start-tunnel-origin!
  "A raw (non-HTTP) origin for CONNECT-tunnel tests: accepts, NEVER reads, and
   emits `ticks` lines `gap-ms` apart (0 ticks ⇒ dead silent and never closed).
   `:sent` counts the lines it got out; `:broken-at` records the ms offset at
   which its write first failed — i.e. when the proxy tore the tunnel down.
   Returns {:port :sent :broken-at :stop!}."
  [ticks gap-ms]
  (let [server
        (doto (ServerSocket.) (.bind (InetSocketAddress. "127.0.0.1" 0)))

        running
        (atom true)

        sent
        (atom 0)

        broken-at
        (atom nil)

        t0
        (System/currentTimeMillis)

        loop-fn
        (fn []
          (while @running
            (when-let [^Socket c (try (.accept server) (catch Throwable _ nil))]
              ;; A REAL streaming origin notices a half-close: it reads the request
              ;; side, and when that side EOFs it abandons the response. That is what
              ;; made the pre-fix half-close destructive rather than cosmetic.
              ;;
              ;; With NO ticks the origin models the opposite peer: one that never
              ;; reads, never writes and never sends a FIN (half-open TCP after a
              ;; NAT/VPN drop) — the shape that used to park a copy thread forever.
              (when (pos? ticks)
                (future (try (let [in (.getInputStream c)]
                               (loop []

                                 (let [n (.read in)]
                                   (if (neg? n)
                                     (do (compare-and-set! broken-at
                                                           nil
                                                           (- (System/currentTimeMillis) t0))
                                         (try (.close c) (catch Throwable _ nil)))
                                     (when @running (recur))))))
                             (catch Throwable _ nil))))
              (future (try (let [out (.getOutputStream c)]
                             (dotimes [i ticks]
                               (.write out (.getBytes (str "tick " i "\n")))
                               (.flush out)
                               (swap! sent inc)
                               (Thread/sleep (long gap-ms)))
                             ;; stay connected and silent until the test stops us
                             (while @running (Thread/sleep 25)))
                           (catch Throwable _
                             (compare-and-set! broken-at nil (- (System/currentTimeMillis) t0)))
                           (finally (try (.close c) (catch Throwable _ nil))))))))]

    (doto (Thread. ^Runnable loop-fn "tunnel-origin") (.setDaemon true) (.start))
    {:port (.getLocalPort server)
     :sent sent
     :broken-at broken-at
     :stop! (fn []
              (reset! running false)
              (try (.close server) (catch Throwable _ nil)))}))

(defn- open-tunnel!
  "CONNECT through the proxy to `origin-port` and hand the raw tunnel socket to
   `f` once the 200 is in. The client then says NOTHING — exactly like a browser
   or an LLM SDK waiting on a streaming response."
  [proxy-port origin-port f]
  (with-open [s (doto (Socket.) (.connect (InetSocketAddress. "127.0.0.1" (int proxy-port)) 5000))]
    (.setSoTimeout s 15000)
    (.write (.getOutputStream s)
            (.getBytes
              (str "CONNECT localhost:" origin-port " HTTP/1.1\r\nHost: localhost\r\n\r\n")))
    (.flush (.getOutputStream s))
    (let [in (BufferedReader. (InputStreamReader. (.getInputStream s)))
          status (.readLine in)]

      (loop []

        (let [l (.readLine in)]
          (when (and l (not= l "")) (recur))))
      (f status in))))

(defdescribe
  tunnel-read-timeout-is-not-eof
  (it "tunnel read timeout is not eof"
      ;; REGRESSION — the client socket used to keep the HANDSHAKE read timeout for
      ;; the whole life of the tunnel, and `splice` treated that timeout as EOF. A
      ;; tunnelled client is legitimately silent client→upstream for the entire
      ;; streaming response, so every long/quiet CONNECT (LLM completion, SSE,
      ;; websocket, ssh) got half-closed mid-stream: the proxy called
      ;; `shutdownOutput` on the upstream and the origin saw the response die.
      (run-wire-test
        (fn []
          (with-redefs-fn {#'ep/TUNNEL_READ_TIMEOUT_MS 200 #'ep/TUNNEL_MAX_IDLE_MS 60000}
            (fn []
              (let [origin (start-tunnel-origin! 6 250)]
                (try (let [proxy (ep/start! {:policy-fn (fn [_]
                                                          (ep/compile-policy
                                                            {:allowed-domains ["*"]
                                                             :rules [{:host "localhost"
                                                                      :ports [(:port
                                                                                origin)]}]}))})]
                       (try (open-tunnel!
                              (:port proxy)
                              (:port origin)
                              (fn [status in]
                                ;; the tunnel is established
                                (expect (= "HTTP/1.1 200 Connection Established" status))
                                ;; a silent client survives many read timeouts while the origin streams
                                ;; 6 ticks over ~1.5 s with a 200 ms poll timeout: the old
                                ;; code cut this after the FIRST timeout.
                                (let [got (doall (repeatedly 6 #(.readLine in)))]
                                  (expect (= ["tick 0" "tick 1" "tick 2" "tick 3" "tick 4" "tick 5"]
                                             got))
                                  (expect (nil? @(:broken-at origin))
                                          "the origin's writes were never broken by the proxy"))))
                            (finally ((:stop! proxy)))))
                     (finally ((:stop! origin)))))))))))

(defdescribe tunnel-idle-reclaim
             (it "tunnel idle reclaim"
                 ;; The flip side: a bounded read is the ONLY way a wedged tunnel is ever
                 ;; reclaimed. A blocking socket read ignores interrupt/shutdownNow, so a peer
                 ;; that dies WITHOUT a FIN (NAT/VPN drop, half-open TCP) used to park both
                 ;; copy directions forever and pin two pool threads for the life of the JVM.
                 (run-wire-test
                   (fn []
                     (with-redefs-fn {#'ep/TUNNEL_READ_TIMEOUT_MS 150 #'ep/TUNNEL_MAX_IDLE_MS 600}
                       (fn []
                         (let [origin (start-tunnel-origin! 0 0)]
                           (try
                             (let [proxy (ep/start! {:policy-fn
                                                     (fn [_]
                                                       (ep/compile-policy
                                                         {:allowed-domains ["*"]
                                                          :rules [{:host "localhost"
                                                                   :ports [(:port origin)]}]}))})]
                               (try (open-tunnel!
                                      (:port proxy)
                                      (:port origin)
                                      (fn [status in]
                                        (expect (= "HTTP/1.1 200 Connection Established" status))
                                        ;; a fully idle tunnel is torn down instead of parking forever
                                        (let [t0 (System/currentTimeMillis)
                                              eof (.readLine in)
                                              took (- (System/currentTimeMillis) t0)]

                                          (expect (nil? eof) "the relay closed the idle tunnel")
                                          (expect (< took 10000) (str "reclaimed in " took "ms")))))
                                    (finally ((:stop! proxy)))))
                             (finally ((:stop! origin)))))))))))

(defn- start-patient-origin!
  "An origin that IGNORES its request side entirely (never reads, never reacts to
   a half-close), waits `think-ms` — the model's time-to-first-token — and only
   then streams. Returns {:port :wrote :stop!}."
  [think-ms]
  (let [server
        (doto (ServerSocket.) (.bind (InetSocketAddress. "127.0.0.1" 0)))

        running
        (atom true)

        wrote
        (atom nil)

        loop-fn
        (fn []
          (while @running
            (when-let [^Socket c (try (.accept server) (catch Throwable _ nil))]
              (future (try (Thread/sleep (long think-ms))
                           (let [out (.getOutputStream c)]
                             (.write out (.getBytes "late-token\n"))
                             (.flush out)
                             (reset! wrote true)
                             (while @running (Thread/sleep 25)))
                           (catch Throwable _ (compare-and-set! wrote nil false))
                           (finally (try (.close c) (catch Throwable _ nil))))))))]

    (doto (Thread. ^Runnable loop-fn "patient-origin") (.setDaemon true) (.start))
    {:port (.getLocalPort server)
     :wrote wrote
     :stop! (fn []
              (reset! running false)
              (try (.close server) (catch Throwable _ nil)))}))

(defdescribe
  tunnel-half-close-does-not-end-the-tunnel
  (it "tunnel half close does not end the tunnel"
      ;; REGRESSION — a client that half-closes after sending its request
      ;; (`shutdownOutput`; ordinary for HTTP/1 and ssh) EOFs the client→upstream
      ;; direction at once. The relay used to read that as "the tunnel is over": the
      ;; FIRST direction to finish flipped a shared flag, so the response direction
      ;; gave up at its very next poll and the client got EOF instead of the answer.
      ;; Only a real abort (an exception on either leg) may stop the sibling.
      (run-wire-test
        (fn []
          (with-redefs-fn {#'ep/TUNNEL_READ_TIMEOUT_MS 150 #'ep/TUNNEL_MAX_IDLE_MS 60000}
            (fn []
              (let [origin (start-patient-origin! 1500)]
                (try (let [proxy (ep/start! {:policy-fn (fn [_]
                                                          (ep/compile-policy
                                                            {:allowed-domains ["*"]
                                                             :rules [{:host "localhost"
                                                                      :ports [(:port
                                                                                origin)]}]}))})]
                       (try (with-open [s (doto (Socket.)
                                            (.connect (InetSocketAddress. "127.0.0.1"
                                                                          (int (:port proxy)))
                                                      5000))]
                              (.setSoTimeout s 15000)
                              (.write (.getOutputStream s)
                                      (.getBytes (str "CONNECT localhost:"
                                                      (:port origin)
                                                      " HTTP/1.1\r\nHost: localhost\r\n\r\n")))
                              (.flush (.getOutputStream s))
                              (let [in (BufferedReader. (InputStreamReader. (.getInputStream s)))
                                    status (.readLine in)]

                                (expect (= "HTTP/1.1 200 Connection Established" status))
                                (loop []

                                  (let [l (.readLine in)]
                                    (when (and l (not= l "")) (recur))))
                                ;; the request is complete — say so at the TCP level
                                (.shutdownOutput s)
                                ;; the response still arrives after the client half-closes
                                (let [got (.readLine in)]
                                  (expect (= "late-token" got))
                                  (expect (true? @(:wrote origin))
                                          "the origin's write was not torn down by the proxy"))))
                            (finally ((:stop! proxy)))))
                     (finally ((:stop! origin)))))))))))
