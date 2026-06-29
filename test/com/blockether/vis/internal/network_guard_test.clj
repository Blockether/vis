(ns com.blockether.vis.internal.network-guard-test
  "Security regression guard for the Python sandbox's network capability.

   Three layers, all asserted here:
     - OFF (default)        ⇒ no sockets at all (DNS resolution refused).
     - ON, no allowlist     ⇒ unrestricted (resolution works).
     - ON, with allowlist   ⇒ hosts outside the allowlist raise PermissionError
                              before any connection (`getaddrinfo`/`gethostbyname`)."
  (:require
   [com.blockether.vis.internal.env-python :as env]
   [lazytest.core :refer [defdescribe expect it]]))

(defn- resolve-outcome
  "Eval a DNS resolution for `host` in `ctx` and classify the result."
  [ctx host]
  (try
    (.eval (:python-context ctx) "python"
      (str "import socket; socket.gethostbyname(" (pr-str host) ")"))
    :ok
    (catch Throwable e
      (let [m (str (.getMessage e))]
        (cond
          (re-find #"allowed domains" m) :blocked-domain
          (re-find #"not allowed|SecurityException|Operation|denied|getaddrinfo|gaierror" m) :no-socket
          :else (keyword (str "other:" (subs m 0 (min 60 (count m))))))))))

(defdescribe network-guard-test
  (it "OFF ⇒ no sockets; ON ⇒ resolves; ON+allowlist ⇒ confines to listed hosts"
    (let [off  (env/create-python-context {} nil nil)
          on   (env/create-python-context {} nil {:enabled? true})
          conf (env/create-python-context {} nil {:enabled? true :allowed-domains ["example.com"]})]
      (try
        ;; OFF (default): the socket capability itself is gated — DNS denied.
        (expect (= :no-socket (resolve-outcome off "localhost")))
        ;; ON, no allowlist: unrestricted.
        (expect (= :ok (resolve-outcome on "localhost")))
        ;; ON + allowlist: subdomain of an allowed apex passes…
        (expect (= :ok (resolve-outcome conf "www.example.com")))
        ;; …a host outside the allowlist is refused by the guard.
        (expect (= :blocked-domain (resolve-outcome conf "evil.com")))
        (finally
          (.close (:python-context off) true)
          (.close (:python-context on) true)
          (.close (:python-context conf) true))))))
