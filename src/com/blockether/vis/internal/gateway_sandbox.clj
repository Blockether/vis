(ns com.blockether.vis.internal.gateway-sandbox
  "Gateway-lifecycle SANDBOX CAPABILITY: ONE shared loopback egress proxy and ONE
   ephemeral MITM CA for the WHOLE daemon, keyed PER SESSION.

   Why shared, not per-session (turn 32): the gateway is multi-tenant — many
   clients/sessions hit one daemon. A per-session proxy+CA thrashes listeners and
   mints a fresh CA per session; worse, every child would carry a different trust
   root. Instead there is ONE listener and ONE CA (the \"same certs\" property —
   every child trusts the same root), plus a REGISTRY mapping a per-session TOKEN →
   that session's live policy fn.

   How a connection is attributed to a session: the jailed child's proxy env carries
   its unguessable token in the proxy URL userinfo (`http://<token>@127.0.0.1:<port>`);
   curl/git/requests/… send it back as `Proxy-Authorization: Basic base64(<token>:)`;
   the proxy hands the token to `resolve-policy`, which looks up the registry.

   FAIL-CLOSED: a request whose token is missing or not registered to a LIVE session
   is DENIED (a `:deny-all?` sentinel policy) — the shared door never serves a policy
   it cannot attribute. The token is a random UUID, so one session cannot reach
   another's (broader) policy by guessing.

   Lazy: the proxy listener and the CA keygen happen only on first `ensure-proxy!` /
   `ensure-ca!` — a gateway that never jails a shell child opens neither."
  (:require [com.blockether.vis.internal.egress-proxy :as egress]
            [com.blockether.vis.internal.tls-mitm :as tls-mitm]))

;; token -> 0-arg policy fn (returns the session's compiled policy value, or nil)
(defonce ^:private registry (atom {}))
;; {:port <int> :stop! (fn [])} | nil — the ONE shared proxy
(defonce ^:private proxy-state (atom nil))
;; tls-mitm capability {:ca-file :ctx-for :close! …} | nil — the ONE shared CA
(defonce ^:private ca-state (atom nil))

(def ^:private proxy-lock (Object.))
(def ^:private ca-lock (Object.))

;; Extra loopback ports a jailed child can NEVER reach even though loopback egress is
;; ALLOWED by default — the gateway's own control-plane API port(s). The shared proxy's
;; own port is reserved automatically at resolve time. Set by the gateway server at startup.
(defonce ^:private extra-reserved-ports (atom #{}))

(defn set-reserved-ports!
  "Register loopback ports a jailed child must never reach (the gateway's own control
   plane) even though loopback is allowed by default. The shared proxy's own port is
   reserved automatically. Idempotent."
  [ports]
  (reset! extra-reserved-ports (set (filter integer? ports)))
  nil)

(defn- reserved-loopback-ports
  "All loopback ports off-limits to a jailed child: the shared proxy's own port plus any
   gateway-registered control-plane ports."
  []
  (into @extra-reserved-ports
        (when-let [p (:port @proxy-state)]
          [p])))

(defn register-session!
  "Register `policy-fn` (0-arg → the session's compiled policy value or nil) under
   `token`. The shared proxy resolves this session's policy from here per request."
  [token policy-fn]
  (when (and token policy-fn) (swap! registry assoc token policy-fn))
  token)

(defn unregister-session!
  "Drop a session's policy from the registry (on env dispose). Idempotent."
  [token]
  (when token (swap! registry dissoc token))
  nil)

(defn registered?
  "True when `token` maps to a live session policy fn."
  [token]
  (contains? @registry token))

(defn resolve-policy
  "Token-keyed resolver handed to the shared proxy's `:policy-fn`. Returns the
   session's compiled policy, or a `:deny-all?` sentinel when the token is absent or
   unregistered (FAIL-CLOSED — an unattributable request reaches nothing)."
  [token]
  (if-let [pf (and token (get @registry token))]
    (try (let [p (pf)]
           (assoc (if (map? p) p {}) :reserved-loopback-ports (reserved-loopback-ports)))
         (catch Throwable _ {:deny-all? true :reason "vis: session policy error"}))
    {:deny-all? true
     :reason (if token
               "vis: no live session for this proxy token"
               "vis: missing Proxy-Authorization (unattributable egress)")}))

(defn ensure-ca!
  "Lazily create the ONE shared ephemeral MITM CA. Returns its `:ca-file` PEM path
   (injected into a jailed child's trust env). Idempotent + thread-safe."
  []
  (or (:ca-file @ca-state)
      (locking ca-lock
        (or (:ca-file @ca-state)
            (let [cap (tls-mitm/create!)]
              (reset! ca-state cap)
              (:ca-file cap))))))

(defn ensure-proxy!
  "Lazily start the ONE shared loopback egress proxy bound to the token-keyed
   resolver + the shared CA. Returns its port. Idempotent + thread-safe."
  []
  (or (:port @proxy-state)
      (locking proxy-lock
        (or (:port @proxy-state)
            (let
              [srv (egress/start! {:policy-fn resolve-policy
                                   :mitm (fn []
                                           @ca-state)})]
              (reset! proxy-state srv)
              (:port srv))))))

(defn proxy-port
  "The running shared proxy's port, or nil if it was never started."
  []
  (:port @proxy-state))

(defn shutdown!
  "Stop the shared proxy, delete the shared CA PEM, clear the registry. For daemon
   shutdown. Idempotent."
  []
  (locking proxy-lock
    (when-let [stop! (:stop! @proxy-state)]
      (try (stop!) (catch Throwable _ nil)))
    (reset! proxy-state nil))
  (locking ca-lock
    (when-let [close! (:close! @ca-state)]
      (try (close!) (catch Throwable _ nil)))
    (reset! ca-state nil))
  (reset! registry {})
  nil)
