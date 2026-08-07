(ns com.blockether.vis.test-network-guard
  "Suite-wide wall between the test run and the public internet.

   Every outbound HTTP call in this repository — Vis' own clients and svar's
   router alike — goes through `babashka.http-client/request`, so ONE wrapper
   there covers the whole tree. Loopback stays open, because the gateway,
   egress-proxy and MCP tests serve real sockets on 127.0.0.1; anything else is
   refused instantly with the URL that asked for it.

   This is a speed contract before it is a hygiene one: a test that dials
   `api.openai.com` pays DNS, TLS and a provider timeout for a result it cannot
   assert on, and CI without network pays the same wait to fail. Refusing at the
   call site turns that into microseconds and names the offender.

   A test that genuinely needs the wire binds `*allow-network*`."
  (:require [babashka.http-client :as http]
            [lazytest.hooks :as hooks]))

(def ^:dynamic *allow-network*
  "Bind true around a call that deliberately reaches the real internet."
  false)

(def loopback-hosts
  "Hosts the guard always lets through — the sockets the suite serves itself."
  #{"localhost" "127.0.0.1" "0.0.0.0" "::1" "[::1]" "[0:0:0:0:0:0:0:1]"})

(defn request-host
  "Host of a `babashka.http-client` request map, or nil when it has none.
   `:uri` is a URL string or an already-parsed map."
  [opts]
  (let [uri (or (:uri opts) (:url opts))]
    (cond (map? uri) (:host uri)
          (string? uri) (try (.getHost (java.net.URI. uri)) (catch Exception _ nil))
          :else nil)))

(defn refused?
  "True when a request to `host` must not leave the machine."
  [host]
  (boolean (and host
                (not *allow-network*)
                (not (contains? loopback-hosts host))
                (not (re-find #"\.localhost$" host)))))

(defn guard
  "Wrap a `request`-like fn so non-loopback traffic throws instead of dialing."
  [request-fn]
  (fn [opts & more]
    (let [host (request-host opts)]
      (when (refused? host)
        (throw (ex-info
                 (str "test network guard: refused outbound request to " host
                      " — mock it, or bind "
                      "com.blockether.vis.test-network-guard/*allow-network*")
                 {:type :vis.test/network-refused :host host :uri (or (:uri opts) (:url opts))})))
      (apply request-fn opts more))))

(defonce ^:private installed (atom false))

(defn install!
  "Idempotently wrap `babashka.http-client/request` with the guard."
  []
  (when (compare-and-set! installed false true) (alter-var-root #'http/request guard))
  true)

(hooks/defhook no-network
               "Refuse non-loopback HTTP for the whole test run."
               (pre-test-run [_config m] (install!) m))
