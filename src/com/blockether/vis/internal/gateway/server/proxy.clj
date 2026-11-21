;; Copyright (c) Blockether. All rights reserved.
(ns com.blockether.vis.internal.gateway.server.proxy
  "The forward proxy of the gateway (`gateway.proxy` in the global configuration).

   A paired client, for example the Companion app on a phone outside a company VPC,
   opens the hosts of `allowed_domains` through the network of the gateway machine.
   The listener is the egress proxy of the sandbox (`sandbox.egress-proxy`) on its own
   port of the gateway host, so HTTPS `CONNECT`, plain HTTP and SOCKS5 all work.

   Rules:
   - Only the global configuration turns the proxy on. A project `vis.yml` cannot,
     because a cloned repository must never open the network of the machine.
   - Each connection must send the gateway token as the user name of
     `Proxy-Authorization: Basic`, also on a loopback bind. A jailed child has no token,
     and the proxy port is a reserved loopback port, so the proxy is no escape from a jail.
   - Private addresses are allowed (a VPC is private). Link-local and cloud metadata
     addresses stay blocked, and so do the ports of the gateway and of the proxy.
   - Each connection reads the configuration again, so a domain change needs no
     restart. `sync!` starts, moves or stops the listener."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.config.core :as config]
            [com.blockether.vis.internal.gateway.server.http :as http]
            [com.blockether.vis.internal.gateway.server.instance :as instance]
            [com.blockether.vis.internal.sandbox.egress-proxy :as egress]
            [com.blockether.vis.internal.sandbox.gateway :as gateway-sandbox]
            [taoensso.telemere :as tel])
  (:import (java.net InetAddress InetSocketAddress ServerSocket)
           (java.nio.charset StandardCharsets)
           (java.security MessageDigest)))

(def default-ports "The target ports when `gateway.proxy.ports` is absent." [80 443])

(defn- domains
  "The trimmed, lower-case domain patterns of `xs`. A bare `*` is dropped: it makes an
   open proxy, and the contract refuses it."
  [xs]
  (into []
        (comp (filter string?)
              (map (comp str/lower-case str/trim))
              (remove #(or (str/blank? %) (= "*" %)))
              (distinct))
        (when (sequential? xs) xs)))

(defn settings
  "The proxy settings of the string-keyed configuration `raw`, or nil when the proxy is
   off: `{:allowed-domains [...] :denied-domains [...] :ports [...] :port n-or-nil}`."
  [raw]
  (let [proxy
        (get-in raw ["gateway" "proxy"])

        allowed
        (domains (get proxy "allowed_domains"))

        ports
        (into [] (comp (filter integer?) (filter #(<= 1 % 65535)) (distinct)) (get proxy "ports"))

        port
        (get proxy "port")]

    (when (and (map? proxy) (seq allowed))
      {:allowed-domains allowed
       :denied-domains (domains (get proxy "denied_domains"))
       :ports (if (seq ports) ports default-ports)
       :port (when (and (integer? port) (<= 1 port 65535)) port)})))

(defn- global-settings
  "The settings of the global configuration files only, or nil when the proxy is off or
   the files cannot be read. See the namespace rules."
  []
  (try (settings (config/deep-merge-config (config/load-global-yaml-config-raw)
                                           (config/load-global-config-raw)))
       (catch Throwable e
         (tel/log! {:level :warn :id ::config-unreadable :error e}
                   "The gateway proxy could not read the configuration, so it is off")
         nil)))

(defn- token=
  "Constant-time comparison of a presented token with the gateway token."
  [presented expected]
  (boolean (and (string? presented)
                (string? expected)
                (MessageDigest/isEqual (.getBytes ^String presented StandardCharsets/UTF_8)
                                       (.getBytes ^String expected StandardCharsets/UTF_8)))))

(defn policy
  "The egress policy of one connection that presented `token`. `settings` nil gives a
   refusal, a wrong token a 407 answer, else the policy of the allowed domains. The
   answer is never nil: the egress proxy reads a nil policy as `open every host`."
  [settings token expected-token reserved-ports]
  (cond (nil? settings) {:deny-all? true :reason "vis: the gateway proxy is off"}
        (not (token= token expected-token))
        {:proxy-auth-required? true :reason "vis: send the gateway token as the proxy user name"}
        :else (if-let [compiled (egress/compile-policy {:allowed-domains (:allowed-domains settings)
                                                        :denied-domains (:denied-domains settings)
                                                        :rules [{:host "*"
                                                                 :ports (:ports settings)}]
                                                        :allow-private true})]
                (assoc compiled
                  :reserved-loopback-ports (vec (remove nil? reserved-ports))
                  :deny-names-only? true)
                {:deny-all? true :reason "vis: the gateway proxy has no allowed domains"})))

(defonce ^:private listener
  ;; {:host :wanted-port :port :stop!} of the running listener, or nil. `sync!` and
  ;; `stop!` are its only writers.
  (atom nil))

(defn- log-decision!
  [{:keys [phase host allow? reason]}]
  (if allow?
    (tel/log! {:level :info :id ::opened :data {:phase phase :host host}}
              "The gateway proxy opened a connection")
    (tel/log! {:level :warn :id ::refused :data {:phase phase :host host :reason reason}}
              "The gateway proxy refused a connection")))

(defn- reserve-ports!
  "Keep the gateway port and the proxy port out of reach of a jailed child."
  [gateway-port proxy-port]
  (try (gateway-sandbox/set-reserved-ports! (filterv some? [gateway-port proxy-port]))
       (catch Throwable e
         (tel/log! {:level :warn :id ::reserve-failed :error e}
                   "Vis could not reserve the gateway ports"))))

(defn- start-listener!
  [host wanted-port gateway-port]
  (let [server
        (doto (ServerSocket.)
          (.setReuseAddress true)
          (.bind (InetSocketAddress. (InetAddress/getByName host) (int wanted-port))))

        port
        (.getLocalPort server)

        {:keys [stop!]}
        (egress/start!
          {:server server
           :policy-fn
           (fn [token]
             ;; The egress proxy reads a failure here as a nil policy, which opens every
             ;; host. So a failure refuses the connection.
             (try
               (policy (global-settings) token (:token @instance/server-state) [gateway-port port])
               (catch Throwable e
                 (tel/log! {:level :warn :id ::policy-failed :error e}
                           "The gateway proxy refused a connection after an error")
                 {:deny-all? true :reason "vis: the gateway proxy failed"})))
           :on-log log-decision!})]

    (tel/log! {:level :info :id ::listening :data {:host host :port port}}
              "The gateway proxy listens")
    {:host host :wanted-port wanted-port :port port :stop! stop!}))

(defn sync!
  "Make the listener match `gateway.proxy`: start it, move it to a new port, or stop it.
   A repeated call changes nothing. Returns the port, or nil when the proxy is off."
  []
  (locking listener
    (let [{:keys [host] gateway-port :port}
          @instance/server-state

          s
          (when host (global-settings))

          want
          (when s {:host host :wanted-port (or (:port s) 0)})

          current
          @listener]

      (when-not (= want
                   (some-> current
                           (select-keys [:host :wanted-port])))
        (when current ((:stop! current)) (reset! listener nil))
        (when want
          (try (reset! listener (start-listener! host (:wanted-port want) gateway-port))
               (catch Throwable e
                 (tel/log! {:level :warn :id ::start-failed :error e :data want}
                           "The gateway proxy could not listen")))))
      (reserve-ports! gateway-port (:port @listener))
      (:port @listener))))

(defn stop!
  "Stop the listener and release the ports that `sync!` reserved. Idempotent."
  []
  (locking listener
    (when-let [{:keys [stop!]} @listener]
      (stop!)
      (reset! listener nil))
    (reserve-ports! nil nil)))

(defn- proxy-handler
  "GET /v1/proxy — the proxy settings that a client uses to route a link."
  [_request]
  (let [port
        (sync!)

        s
        (when port (global-settings))]

    (http/json-response {"is_enabled" (boolean s)
                         "port" (when s port)
                         "allowed_domains" (or (:allowed-domains s) [])
                         "denied_domains" (or (:denied-domains s) [])
                         "ports" (if s (:ports s) [])})))

(def handlers {[:get "/v1/proxy"] proxy-handler})
