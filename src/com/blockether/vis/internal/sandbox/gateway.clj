(ns com.blockether.vis.internal.sandbox.gateway
  "Gateway-lifecycle SANDBOX CAPABILITY: ONE shared loopback egress proxy and ONE
   ephemeral MITM CA for the WHOLE daemon, keyed PER SESSION.

   Why shared, not per-session (turn 32): the gateway is multi-tenant — many
   clients/sessions hit one daemon. A per-session proxy+CA thrashes listeners and
   mints a fresh CA per session; worse, every child would carry a different trust
   root. Instead there is ONE listener and ONE CA (the \"same certs\" property —
   every child trusts the same root), plus a REGISTRY mapping a per-session TOKEN →
   that session's live policy fn.

   How a connection is attributed to a session: the jailed child's proxy env carries
   its unguessable token in the proxy URL userinfo
   (`http://<token>:vis@127.0.0.1:<port>`); curl/git/requests/… send it back as
   `Proxy-Authorization: Basic base64(<token>:vis)` and the password is ignored;
   the proxy hands the token to `resolve-policy`, which looks up the registry.

   FAIL-CLOSED: a request whose token is missing or not registered to a LIVE session
   is DENIED (a `:deny-all?` sentinel policy) — the shared door never serves a policy
   it cannot attribute. The token is a random UUID, so one session cannot reach
   another's (broader) policy by guessing.

   Lazy: the proxy listener and the CA keygen happen only on first `ensure-proxy!` /
   `ensure-ca!` — a gateway that never jails a shell child opens neither."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [com.blockether.vis-python-runtime :as python-runtime]
            [com.blockether.vis.internal.sandbox.egress-proxy :as egress]
            [com.blockether.vis.internal.python.runtime :as vis-python-runtime]
            [com.blockether.vis.internal.sandbox.jail :as process-jail]
            [com.blockether.vis.internal.sandbox.tls-mitm :as tls-mitm]
            [taoensso.telemere :as tel])
  (:import (java.io File)
           (java.net Socket)
           (java.nio.file CopyOption Files StandardCopyOption)))

;; token -> {:policy-fn <0-arg fn: the session's compiled policy value, or nil>
;;           :order <registration number, larger is newer>}
(defonce ^:private registry (atom {}))

(defonce ^:private registrations (atom 0))

;; {:port <int> :stop! (fn [])} | nil — the ONE shared proxy
(defonce ^:private proxy-state (atom nil))

;; token -> front-door slot; compatibility front doors for managed
;; language processes whose HTTP stacks cannot attach Proxy-Authorization.
;; They share the policy engine + CA; only attribution moves from header to port.
(defonce ^:private session-proxy-states (atom {}))

;; Vector of front-door slots {:port :server :owner (atom id-or-nil) :tokens (atom [token ...])
;; :proxy (delay)}, reserved together on first use, or nil. `:tokens` are the live tokens of
;; the one owner session, oldest registration first; a slot serves only the newest.
(defonce ^:private front-doors (atom nil))

(def ^:private front-door-count
  "Front doors reserved at once. A child with direct loopback denies every gateway port
   that exists at its spawn, so a session that starts later may only take a reserved
   port. A door serves one session only, because a process of that session can outlive
   it outside every tracked process tree. When all are owned, a new session's workers
   use the shared authenticated proxy until the gateway restarts."
  256)

(def ^:private session-proxy-lock (Object.))

;; True after the log that every front door has an owner, once per gateway run.
(defonce ^:private front-doors-full-logged? (atom false))

;; tls-mitm capability {:ca-file :ctx-for :close! …} | nil — the ONE shared CA
(defonce ^:private ca-state (atom nil))

(def ^:private proxy-lock (Object.))

(def ^:private ca-lock (Object.))

;; Extra loopback ports a jailed child can NEVER reach even though loopback egress is
;; ALLOWED by default — the gateway's own control-plane API port(s). The shared proxy's
;; own port is reserved automatically at resolve time. Set by the gateway server at startup.
(defonce ^:private extra-reserved-ports (atom #{}))

;; The reserved ports of each Vis process of this user (#473): one EDN file for each
;; process, named by its jail owner. The folder does not follow VIS_HOME, so gateways with
;; different homes see each other, and no jail write root holds it. The proxy reads it for
;; each request, so a child cannot reach a gateway that starts after the child.

(def ^:private live-ports-lock (Object.))

(defn- live-ports-dir
  ^File []
  (io/file (System/getProperty "user.home") ".vis" "gateway" "live-ports"))

(defn- own-reserved-ports
  []
  (into @extra-reserved-ports
        (concat (when-let [p (:port @proxy-state)]
                  [p])
                (map :port @front-doors))))

(defn- publish-ports!
  "Write the reserved ports of this process for the other Vis processes. The file is
   replaced whole, so a reader never sees a part of it."
  []
  (locking live-ports-lock
    (try (let [dir
               (live-ports-dir)

               ports
               (vec (sort (own-reserved-ports)))

               file
               (io/file dir (process-jail/claim-owner!))]

           (if (empty? ports)
             (.delete file)
             (let [_
                   (.mkdirs dir)

                   tmp
                   (File/createTempFile ".ports" ".tmp" dir)]

               (spit tmp (pr-str ports))
               (Files/move (.toPath tmp)
                           (.toPath file)
                           (into-array CopyOption
                                       [StandardCopyOption/ATOMIC_MOVE
                                        StandardCopyOption/REPLACE_EXISTING])))))
         (catch Exception e
           (tel/log! {:level :warn :id ::live-ports-publish-failed :error e}
                     "Vis could not publish its gateway ports to other Vis processes")))))

(defn- unpublish-ports!
  []
  (locking live-ports-lock
    (try (.delete (io/file (live-ports-dir) (process-jail/claim-owner!)))
         (catch Exception e
           (tel/log! {:level :warn :id ::live-ports-unpublish-failed :error e}
                     "Vis could not remove its published gateway ports")))))

(defn- other-process-ports
  "The reserved ports of each other live Vis process. The file of an ended process goes."
  []
  (let [own (process-jail/claim-owner!)]
    (into #{}
          (comp (filter #(re-matches #"\d+-\d+" (.getName ^File %)))
                (remove #(= own (.getName ^File %)))
                (filter (fn [^File file]
                          (or (process-jail/live-owner? (.getName file))
                              (do (.delete file) false))))
                (mapcat (fn [^File file]
                          (try (edn/read-string (slurp file))
                               (catch Exception e
                                 (tel/log! {:level :warn
                                            :id ::live-ports-unreadable
                                            :error e
                                            :data {:file (.getName file)}}
                                           "Vis could not read the ports of another Vis process")
                                 nil))))
                (filter integer?))
          (.listFiles (live-ports-dir)))))

(defn set-reserved-ports!
  "Register loopback ports a jailed child must never reach (the gateway's own control
   plane) even though loopback is allowed by default. The shared proxy's own port is
   reserved automatically. Idempotent."
  [ports]
  (reset! extra-reserved-ports (set (filter integer? ports)))
  (publish-ports!)
  nil)

(defn- reserved-loopback-ports
  "All loopback ports off-limits to a jailed child: the shared authenticated
   proxy, every front-door slot, and gateway control-plane ports, of this process
   and of each other live Vis process. A proxy can therefore never pivot into another
   proxy listener, and a child never reaches another gateway (#473)."
  []
  (into (own-reserved-ports) (other-process-ports)))

(defn register-session!
  "Register `policy-fn` (0-arg → the session's compiled policy value or nil) under
   `token`. The shared proxy resolves this session's policy from here per request."
  [token policy-fn]
  (when (and token policy-fn)
    (swap! registry assoc token {:policy-fn policy-fn :order (swap! registrations inc)}))
  token)

(defn unregister-session!
  "Drop a session's policy. Its optional session front door then serves the newest
   other live token of the same session, or refuses every request until that session
   registers again; no other session gets it. Idempotent; the shared proxy + CA remain
   gateway-owned."
  [token]
  (when token
    (locking session-proxy-lock
      (some-> (get @session-proxy-states token)
              :tokens
              (swap! (fn [tokens]
                       (filterv #(not= token %) tokens))))
      (swap! session-proxy-states dissoc token)
      (swap! registry dissoc token)))
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
  (if-let [pf (and token (:policy-fn (get @registry token)))]
    (try (let [p (pf)]
           (assoc (if (map? p) p {}) :reserved-loopback-ports (reserved-loopback-ports)))
         (catch Throwable _ {:deny-all? true :reason "vis-agent: session policy error"}))
    {:deny-all? true
     :proxy-auth-required? true
     :reason (if token
               "vis-agent: no live session for this proxy token"
               "vis-agent: missing Proxy-Authorization (unattributable egress)")}))

(defn- ensure-ca-capability!
  []
  (or @ca-state
      (locking ca-lock
        (or @ca-state
            (let [cap (tls-mitm/create!)]
              (reset! ca-state cap)
              cap)))))

(defn ensure-ca!
  "Lazily create the ONE shared ephemeral MITM CA. Returns its PEM path."
  []
  (:ca-file (ensure-ca-capability!)))

(defn ensure-java-trust!
  "Return the shared CA's ephemeral PKCS12 truststore credentials for managed JVM
   children. The store is deleted with the gateway capability."
  []
  (select-keys (ensure-ca-capability!) [:java-trust-store :java-trust-store-password]))

(defn- denial-details
  "Only safe context for a proxy refusal; paths, headers and filter reasons may hold secrets."
  [{:keys [allow? phase host]}]
  (when (false? allow?) {:source :vis-proxy :phase phase :host host}))

(defn- log-egress-decision!
  [event]
  (when-let [details (denial-details event)]
    (tel/log! {:level :warn :id ::egress-denied :data details}
              "Vis egress proxy denied a request")))

(defn ensure-proxy!
  "Lazily start the ONE shared loopback egress proxy bound to the token-keyed
   resolver + the shared CA. Returns its port. Idempotent + thread-safe."
  []
  (or (:port @proxy-state)
      (locking proxy-lock
        (or (:port @proxy-state)
            (let [srv (egress/start! {:policy-fn resolve-policy
                                      :on-log log-egress-decision!
                                      :mitm (fn []
                                              @ca-state)})]
              (reset! proxy-state srv)
              (publish-ports!)
              (:port srv))))))

;; A door attributes a request by its port, and loopback has no authentication by default.
;; So both must know who connects: a jailed child of another Vis process must reach neither
;; (#473). Its own profile cannot deny them, because they can start after it.

(defn- peer-confined?
  "True when a process that a Vis jail confined, with an owner other than `owner` (nil: any
   owner), holds the client end of the loopback connection from `peer-port` to `server-port`.
   `fallback` when the runtime cannot tell."
  [server-port peer-port owner fallback]
  (try (python-runtime/loopback-peer-confined? server-port peer-port owner)
       (catch Throwable e
         (tel/log! {:level :warn
                    :id ::peer-check-failed
                    :error e
                    :data {:server-port server-port :fallback fallback}}
                   "Vis could not tell whether a jailed process opened a loopback connection")
         fallback)))

(defn jailed-peer?
  "True when a jailed child of any Vis process opened the loopback connection from
   `peer-port` to the gateway API on `server-port`. A child of this process cannot connect
   at all. False when the runtime cannot tell, so local clients keep their gateway."
  [server-port peer-port]
  (peer-confined? server-port peer-port nil false))

(defn- door-admits?
  "A front door serves only the processes of this Vis process. It refuses when the runtime
   cannot tell."
  [^Socket client]
  (not (peer-confined? (.getLocalPort client) (.getPort client) (process-jail/claim-owner!) true)))

(defn- front-door
  "One reserved front-door slot: a bound loopback port that serves the newest live token
   of its owner, and fails closed while it has none."
  []
  (let [server
        (doto (java.net.ServerSocket.)
          (.setReuseAddress true)
          (.bind (java.net.InetSocketAddress. (java.net.InetAddress/getByName "127.0.0.1") 0)))

        tokens
        (atom [])]

    {:port (.getLocalPort server)
     :server server
     :owner (atom nil)
     :tokens tokens
     :proxy (delay (egress/start! {:server server
                                   :admit? door-admits?
                                   :policy-fn (fn [_]
                                                (resolve-policy (peek @tokens)))
                                   :on-log log-egress-decision!
                                   :mitm (fn []
                                           @ca-state)}))}))

(defn- front-door-for
  "The slot of `owner`, else a slot that no session has owned, else nil."
  [slots owner]
  (or (some #(when (= owner @(:owner %)) %) slots) (some #(when (nil? @(:owner %)) %) slots)))

(defn- ensure-front-doors!
  "Reserve every front-door port at once, before any child can read the deny list. The
   runtime library comes first: without it a door cannot tell who connects and refuses."
  []
  (or @front-doors
      (locking session-proxy-lock
        (or @front-doors
            (let [_
                  (try (vis-python-runtime/ensure-library!)
                       (catch Throwable e
                         (tel/log! {:level :warn :id ::runtime-library-missing :error e}
                                   "Vis has no runtime library, so its front doors refuse")))

                  slots
                  (reset! front-doors (vec (repeatedly front-door-count front-door)))]

              (publish-ports!)
              slots)))))

(defn ensure-session-proxy!
  "Give `token` of session `owner` a session-attributed compatibility front door for
   managed language processes whose HTTP stacks do not reliably send Proxy-Authorization
   from URL userinfo (notably urllib and JVM dependency resolvers). The listener is bound
   to loopback, resolves exactly `token`'s live policy, and shares the gateway CA and
   proxy implementation. Unknown/disposed sessions fail closed. Returns its port, or nil
   when every reserved front door has another owner.

   A door has one owner for the gateway's life: a new environment of the same session
   gets it again, and a process that outlives its session never finds another session
   on its port. While two environments of the session live, the door serves the newer."
  [token owner]
  (when (nil? owner)
    (throw (ex-info "Managed language proxy denied: no owner session"
                    {:type ::session-proxy-missing :token token})))
  (or (get-in @session-proxy-states [token :port])
      (locking session-proxy-lock
        (when-not (registered? token)
          (throw (ex-info "Managed language proxy denied: session is not registered"
                          {:type ::session-proxy-missing :token token})))
        (or (get-in @session-proxy-states [token :port])
            (if-let [slot (front-door-for (ensure-front-doors!) owner)]
              (do (reset! (:owner slot) owner)
                  (swap! (:tokens slot) (fn [tokens]
                                          (vec (sort-by #(get-in @registry [% :order])
                                                        (conj tokens token)))))
                  (force (:proxy slot))
                  (swap! session-proxy-states assoc token slot)
                  (:port slot))
              (when (compare-and-set! front-doors-full-logged? false true)
                (tel/log!
                  {:level :warn :id ::front-doors-taken :data {:count front-door-count}}
                  "Every session front door has an owner; workers use the shared proxy")))))))

(defn loopback-grant
  "Jail policy keys that let a confined child connect to loopback directly, or nil
   (#469). Granted only when `network-policy` already passes all loopback traffic
   through the proxy. The deny list holds every gateway port a child must never reach:
   the control plane, the shared proxy and each front door, also one that a later
   session takes, because all front doors are reserved before the list is read."
  [network-policy]
  (when (egress/loopback-direct? network-policy)
    (ensure-proxy!)
    (ensure-front-doors!)
    {:loopback-direct? true :loopback-deny-ports (vec (sort (reserved-loopback-ports)))}))

(defn proxy-port
  "The running shared proxy's port, or nil if it was never started."
  []
  (:port @proxy-state))

(defn shutdown!
  "Stop every gateway proxy listener, delete the shared ephemeral trust files, and
   clear all session policy state. Idempotent."
  []
  (locking session-proxy-lock
    (doseq [{:keys [proxy ^java.net.ServerSocket server]} @front-doors]
      (try (when (realized? proxy) ((:stop! @proxy))) (catch Throwable _ nil))
      (try (.close server) (catch Throwable _ nil)))
    (reset! front-doors nil)
    (reset! front-doors-full-logged? false)
    (reset! session-proxy-states {}))
  (locking proxy-lock
    (when-let [stop! (:stop! @proxy-state)]
      (try (stop!) (catch Throwable _ nil)))
    (reset! proxy-state nil))
  (locking ca-lock
    (when-let [close! (:close! @ca-state)]
      (try (close!) (catch Throwable _ nil)))
    (reset! ca-state nil))
  (reset! registry {})
  (unpublish-ports!)
  nil)
