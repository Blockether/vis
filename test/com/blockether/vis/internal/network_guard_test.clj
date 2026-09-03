(ns com.blockether.vis.internal.network-guard-test
  "Security regression guard for the Python sandbox's network capability.

   Two layers, both asserted here, and they are not the same KIND of thing:

   CAPABILITY (C). Whether this session has egress at all is a flag the runtime's
   audit hook reads, so `socket.__new__`, `connect`, `bind` and every name lookup
   are refused before any Python of ours runs. Nothing seeded in the guest can
   rebind its way back to a socket:
     - OFF (default)        ⇒ no sockets at all (DNS resolution refused).

   LEGIBILITY (Python). Inside a session that HAS egress, the cooperative domain
   guard turns a policy refusal into a `PermissionError` naming the host instead
   of a timeout. It installs only when the session is jailed (`:jail-enabled?`)
   and there is something to enforce:
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
  (:require [com.blockether.vis-python-runtime :as runtime]
            [com.blockether.vis.internal.env-python :as env]
            [com.blockether.vis.test-python-context :as tpc]
            [lazytest.core :refer [defdescribe expect it]]))

(def ^:private probes
  "The two guest probes every test reads, defined once per session.

   Both classify instead of throwing, because the interesting distinction is WHICH
   layer refused: a `PermissionError` naming the host came from the domain guard,
   any other one came from the capability flag in C."
  (str "def __vis_probe_dns__(host):\n" "    import socket\n"
       "    try:\n" "        socket.gethostbyname(host)\n"
       "        return 'ok'\n" "    except PermissionError as e:\n"
       "        return 'blocked' if 'is blocked' in str(e) else 'no-socket'\n"
       "    except Exception:\n"
       "        return 'unresolved'\n" "\n"
       "def __vis_probe_connect__(host):\n" "    import socket\n"
       "    try:\n" "        s = socket.socket()\n"
       "    except PermissionError:\n" "        return 'no-socket'\n"
       "    s.settimeout(0.2)\n" "    try:\n"
       "        s.connect((host, 9))\n" "        return 'reached'\n"
       "    except PermissionError:\n" "        return 'blocked'\n"
       "    except Exception:\n" "        return 'reached'\n"
       "    finally:\n" "        s.close()\n"))

(defn- sandbox
  "A jailed sandbox with `network-opts`, its probes already defined."
  [network-opts]
  (let [network-opts
        (merge {:enabled? false :jail-enabled? true} network-opts)

        env
        (tpc/new-context {} nil network-opts)]

    (tpc/ev (:python-context env) probes)
    env))

(defn- dispose!
  "Drop the sandbox and give the process its egress back — the capability flag is
   process state, so a test that left it off would blind every test after it."
  [env]
  (env/dispose-python-context! (:python-context env))
  (try (runtime/network! true) (catch Throwable _ nil)))

(defn- outcome
  "Resolve `host` in `env` and classify: `:ok`, `:blocked` (domain guard refused),
   `:no-socket` (capability denied) or `:unresolved`."
  [env host]
  (keyword (tpc/ev (:python-context env) (str "__vis_probe_dns__(" (pr-str host) ")"))))

(defn- raw-connect-outcome
  "Connect a RAW socket to `host` (no DNS) and classify: `:blocked` when the guard
   refuses, else `:reached` — the guard did NOT stop it. Proves enforcement at
   `connect`, not just at DNS."
  [env host]
  (keyword (tpc/ev (:python-context env) (str "__vis_probe_connect__(" (pr-str host) ")"))))

(defn- env-value
  "Read `os.environ.get(k, '')` in `env`."
  [env k]
  (tpc/ev (:python-context env) (str "__import__('os').environ.get(" (pr-str k) ", '')")))

(defdescribe
  network-guard-test
  (it "OFF ⇒ no sockets at all (DNS denied)"
      (let [off (sandbox nil)]
        (try (expect (= :no-socket (outcome off "localhost")))
             (expect (= :no-socket (raw-connect-outcome off "127.0.0.1")))
             (finally (dispose! off)))))
  ;; The guard's wrapper and its policy holder live in the INTERPRETER, not in a
  ;; session: a confined session left its allowlist behind and the next session,
  ;; granted no network at all, answered `blocked` from Python instead of the
  ;; capability layer's refusal. Green in isolation, red only after a neighbour ran.
  (it "a previous session's allowlist does not answer for a session with no network"
      (let [confined (sandbox
                       {:enabled? true :jail-enabled? true :allowed-domains ["example.com"]})]
        (expect (= :blocked (outcome confined "evil.com")))
        (dispose! confined)
        (let [off (sandbox nil)]
          (try (expect (= :no-socket (outcome off "localhost"))) (finally (dispose! off))))))
  (it "`*` allowlist ⇒ unrestricted EXCEPT the always-on metadata denylist"
      (let [star (sandbox {:enabled? true :jail-enabled? true :allowed-domains ["*"]})]
        (try (expect (= :ok (outcome star "localhost")))
             ;; cloud-metadata SSRF endpoint is denied by default even under `*`
             (expect (= :blocked (outcome star "169.254.169.254")))
             (finally (dispose! star)))))
  (it "allowlist ⇒ confines to listed hosts (subdomain ok, others blocked)"
      (let [conf (sandbox {:enabled? true :jail-enabled? true :allowed-domains ["example.com"]})]
        (try (expect (= :ok (outcome conf "www.example.com")))
             (expect (= :blocked (outcome conf "evil.com")))
             (finally (dispose! conf)))))
  (it "denied `*` + allow some ⇒ deny everything EXCEPT the allowlist"
      (let [d (sandbox {:enabled? true
                        :jail-enabled? true
                        :denied-domains ["*"]
                        :allowed-domains ["example.com"]})]
        (try (expect (= :ok (outcome d "www.example.com"))) ; specific allow beats deny `*`
             (expect (= :blocked (outcome d "evil.com"))) ; deny `*` blocks the rest
             (finally (dispose! d)))))
  (it "allow `*` + deny some ⇒ allow everything EXCEPT the denylist"
      (let [a (sandbox {:enabled? true
                        :jail-enabled? true
                        :allowed-domains ["*"]
                        :denied-domains ["example.com"]})]
        (try (expect (= :blocked (outcome a "example.com"))) ; specific deny beats allow `*`
             (expect (= :ok (outcome a "localhost")))
             (finally (dispose! a)))))
  (it "enforces at connect() too — a raw-IP socket can't skip DNS to a denied IP"
      ;; The default denylist's headline target (the metadata IP 169.254.169.254) is
      ;; an IP literal; a raw `socket.connect((ip, port))` never hits DNS, so guarding
      ;; only getaddrinfo would leave it reachable. connect-level enforcement closes it.
      (let [c (sandbox {:enabled? true
                        :jail-enabled? true
                        :allowed-domains ["*"]
                        :denied-domains ["127.0.0.1"]})]
        (try (expect (= :blocked (raw-connect-outcome c "127.0.0.1")))
             (expect (= :blocked (raw-connect-outcome c "169.254.169.254"))) ; default SSRF denylist
             (finally (dispose! c)))))
  (it "egress routing ⇒ proxy + CA env wired, loopback reachable even under a strict allowlist"
      ;; With :proxy-port + :ca-file the interpreter's HTTP stack is pointed at the
      ;; gateway proxy (verb/path enforced there, not by an in-interpreter method
      ;; guard). Loopback must stay reachable so urllib can reach the proxy.
      (let [p (sandbox {:enabled? true
                        :jail-enabled? true
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
             (finally (dispose! p))))))
