(ns com.blockether.vis.internal.network-guard-test
  "Security regression guard for the Python sandbox's network capability.

   Two layers, both asserted here:

   HOST FLOOR (cooperative socket guard — catches RAW sockets / DNS):
     - OFF (default)        ⇒ no sockets at all (DNS resolution refused).
     - ON, `*` allowlist    ⇒ unrestricted EXCEPT the always-on denylist.
     - ON, with allowlist   ⇒ hosts outside the allowlist raise PermissionError
                              before any connection (`getaddrinfo`/`gethostbyname`).
     - default denylist     ⇒ cloud-metadata SSRF endpoints blocked even under `*`.
     - explicit denylist    ⇒ wins over the allowlist (even `*`).

   EGRESS ROUTING (verb/path enforcement moved to the gateway proxy): when the
   session supplies a loopback `:proxy-port` + shared MITM `:ca-file`, the
   interpreter's HTTP stack is pointed at the proxy and trusts the CA, so `:rules`
   AND `network_filter`s enforce host + verb + path at the ONE gateway policy engine
   (the in-interpreter urllib method-guard is retired). Loopback stays reachable so
   urllib can reach the proxy even under a restrictive allowlist."
  (:require [com.blockether.vis.internal.env-python :as env]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [org.graalvm.polyglot Context]))

(defn- pctx ^Context [ctx] (get ctx :python-context))

(defn- outcome
  "Eval a DNS resolution for `host` in `ctx` and classify: `:ok`, `:blocked`
   (guard refused), or `:no-socket` (capability denied / unresolvable)."
  [ctx host]
  (try (.eval (pctx ctx) "python" (str "import socket; socket.gethostbyname(" (pr-str host) ")"))
       :ok
       (catch Throwable e (if (re-find #"is blocked" (str (.getMessage e))) :blocked :no-socket))))

(defn- raw-connect-outcome
  "Attempt a RAW socket connect to `host` (no DNS) and classify: `:blocked` when
   the guard refuses, else `:reached-socket-layer` (connection refused/timeout —
   i.e. the guard did NOT stop it). Proves enforcement at `connect`, not just DNS."
  [ctx host]
  (let
    [code (str "def _p():\n" "    import socket\n"
               "    s = socket.socket(); s.settimeout(0.2)\n" "    try:\n"
               "        s.connect((" (pr-str host)
               ", 9)); return 'connected'\n" "    except PermissionError: return 'blocked'\n"
               "    except Exception: return 'reached'\n" "_p()")]
    (case (.asString (.eval (pctx ctx) "python" code))
      "blocked"
      :blocked

      :reached-socket-layer)))

(defn- env-value
  "Read `os.environ.get(k, '')` in `ctx`."
  [ctx k]
  (.asString (.eval (pctx ctx) "python" (str "import os; os.environ.get(" (pr-str k) ", '')"))))

(defdescribe
  network-guard-test
  (it "OFF ⇒ no sockets at all (DNS denied)"
      (let [off (env/create-python-context {} nil nil)]
        (try (expect (= :no-socket (outcome off "localhost"))) (finally (.close (pctx off) true)))))
  (it "`*` allowlist ⇒ unrestricted EXCEPT the always-on metadata denylist"
      (let [star (env/create-python-context {} nil {:enabled? true :allowed-domains ["*"]})]
        (try (expect (= :ok (outcome star "localhost")))
             ;; cloud-metadata SSRF endpoint is denied by default even under `*`
             (expect (= :blocked (outcome star "169.254.169.254")))
             (finally (.close (pctx star) true)))))
  (it "allowlist ⇒ confines to listed hosts (subdomain ok, others blocked)"
      (let
        [conf (env/create-python-context {} nil {:enabled? true :allowed-domains ["example.com"]})]
        (try (expect (= :ok (outcome conf "www.example.com")))
             (expect (= :blocked (outcome conf "evil.com")))
             (finally (.close (pctx conf) true)))))
  (it "denied `*` + allow some ⇒ deny everything EXCEPT the allowlist"
      (let
        [d (env/create-python-context
             {}
             nil
             {:enabled? true :denied-domains ["*"] :allowed-domains ["example.com"]})]
        (try (expect (= :ok (outcome d "www.example.com"))) ; specific allow beats deny `*`
             (expect (= :blocked (outcome d "evil.com"))) ; deny `*` blocks the rest
             (finally (.close (pctx d) true)))))
  (it "allow `*` + deny some ⇒ allow everything EXCEPT the denylist"
      (let
        [a (env/create-python-context
             {}
             nil
             {:enabled? true :allowed-domains ["*"] :denied-domains ["example.com"]})]
        (try (expect (= :blocked (outcome a "example.com"))) ; specific deny beats allow `*`
             (expect (= :ok (outcome a "localhost")))
             (finally (.close (pctx a) true)))))
  (it "enforces at connect() too — a raw-IP socket can't skip DNS to a denied IP"
      ;; The default denylist's headline target (the metadata IP 169.254.169.254) is
      ;; an IP literal; a raw `socket.connect((ip, port))` never hits DNS, so guarding
      ;; only getaddrinfo would leave it reachable. connect-level enforcement closes it.
      (let
        [c (env/create-python-context
             {}
             nil
             {:enabled? true :allowed-domains ["*"] :denied-domains ["127.0.0.1"]})]
        (try (expect (= :blocked (raw-connect-outcome c "127.0.0.1")))
             (expect (= :blocked (raw-connect-outcome c "169.254.169.254"))) ; default SSRF denylist
             (finally (.close (pctx c) true)))))
  (it "egress routing ⇒ proxy + CA env wired, loopback reachable even under a strict allowlist"
      ;; With :proxy-port + :ca-file the interpreter's HTTP stack is pointed at the
      ;; gateway proxy (verb/path enforced there, not by an in-interpreter method
      ;; guard). Loopback must stay reachable so urllib can reach the proxy.
      (let
        [p (env/create-python-context {}
                                      nil
                                      {:enabled? true
                                       :allowed-domains ["example.com"]
                                       :proxy-port 65500
                                       :ca-file "/tmp/vis-fake-ca.pem"})]
        (try (expect (= "http://127.0.0.1:65500" (env-value p "http_proxy")))
             (expect (= "http://127.0.0.1:65500" (env-value p "https_proxy")))
             (expect (= "/tmp/vis-fake-ca.pem" (env-value p "REQUESTS_CA_BUNDLE")))
             (expect (= "/tmp/vis-fake-ca.pem" (env-value p "SSL_CERT_FILE")))
             ;; loopback reachable (guard permits it so urllib can reach the proxy)
             (expect (= :ok (outcome p "127.0.0.1")))
             ;; the raw host floor still blocks a non-allowlisted host for raw sockets
             (expect (= :blocked (outcome p "evil.com")))
             (finally (.close (pctx p) true))))))
