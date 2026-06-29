(ns com.blockether.vis.internal.network-guard-test
  "Security regression guard for the Python sandbox's network capability.

   Policy, all asserted here:
     - OFF (default)        ⇒ no sockets at all (DNS resolution refused).
     - ON, `*` allowlist    ⇒ unrestricted EXCEPT the always-on denylist.
     - ON, with allowlist   ⇒ hosts outside the allowlist raise PermissionError
                              before any connection (`getaddrinfo`/`gethostbyname`).
     - default denylist     ⇒ cloud-metadata SSRF endpoints blocked even under `*`.
     - explicit denylist    ⇒ wins over the allowlist (even `*`)."
  (:require
   [com.blockether.vis.internal.env-python :as env]
   [lazytest.core :refer [defdescribe expect it]]))

(defn- outcome
  "Eval a DNS resolution for `host` in `ctx` and classify: `:ok`, `:blocked`
   (guard refused), or `:no-socket` (capability denied / unresolvable)."
  [ctx host]
  (try
    (.eval (:python-context ctx) "python"
      (str "import socket; socket.gethostbyname(" (pr-str host) ")"))
    :ok
    (catch Throwable e
      (if (re-find #"is blocked" (str (.getMessage e))) :blocked :no-socket))))

(defdescribe network-guard-test
  (it "OFF ⇒ no sockets at all (DNS denied)"
    (let [off (env/create-python-context {} nil nil)]
      (try (expect (= :no-socket (outcome off "localhost")))
        (finally (.close (:python-context off) true)))))

  (it "`*` allowlist ⇒ unrestricted EXCEPT the always-on metadata denylist"
    (let [star (env/create-python-context {} nil {:enabled? true :allowed-domains ["*"]})]
      (try
        (expect (= :ok (outcome star "localhost")))
        ;; cloud-metadata SSRF endpoint is denied by default even under `*`
        (expect (= :blocked (outcome star "169.254.169.254")))
        (finally (.close (:python-context star) true)))))

  (it "allowlist ⇒ confines to listed hosts (subdomain ok, others blocked)"
    (let [conf (env/create-python-context {} nil {:enabled? true :allowed-domains ["example.com"]})]
      (try
        (expect (= :ok (outcome conf "www.example.com")))
        (expect (= :blocked (outcome conf "evil.com")))
        (finally (.close (:python-context conf) true)))))

  (it "denied `*` + allow some ⇒ deny everything EXCEPT the allowlist"
    (let [d (env/create-python-context {} nil {:enabled? true :denied-domains ["*"]
                                               :allowed-domains ["example.com"]})]
      (try
        (expect (= :ok (outcome d "www.example.com")))   ; specific allow beats deny `*`
        (expect (= :blocked (outcome d "evil.com")))      ; deny `*` blocks the rest
        (finally (.close (:python-context d) true)))))

  (it "allow `*` + deny some ⇒ allow everything EXCEPT the denylist"
    (let [a (env/create-python-context {} nil {:enabled? true :allowed-domains ["*"]
                                               :denied-domains ["example.com"]})]
      (try
        (expect (= :blocked (outcome a "example.com")))   ; specific deny beats allow `*`
        (expect (= :ok (outcome a "localhost")))
        (finally (.close (:python-context a) true))))))
