(ns com.blockether.vis.internal.gateway.server.instance
  "The running gateway instance: the state the server lifecycle writes and route
   handlers read, the gateway's stable identity, and client-lease liveness."
  (:require [com.blockether.vis.contract.gateway :as gateway-contract]
            [com.blockether.vis.internal.gateway.discovery :as discovery]
            [com.blockether.vis.internal.util :as util]))

(def CLIENT_LEASE_TTL_MS
  "How long a lease that carries NO pid may go without a single request before this
   daemon stops counting it as a client.

   Only a remote client registers one - a phone, or a `--gateway` CLI on another
   machine - and its process lives where this daemon cannot look, so its own traffic
   is the entire proof it is still there (`server/touch-client-lease!`). Long enough that
   a human reading a screen between two requests keeps their lease, and an ATTACHED
   remote client is counted through its open SSE stream anyway (that one dies with
   its socket). Short enough that a client which vanished mid-flight stops pinning
   the daemon past an update."
  (:ttl-ms gateway-contract/client-lease))

(defonce server-state (atom nil))

(defn gateway-instance-id
  "Stable, opaque identity for THIS gateway's data store, derived from the db
   target. Deterministic across restarts and independent of the bind host
   (loopback vs LAN vs Tailscale / cloudflared), so a shared session link
   resolves to the SAME gateway no matter which URL a client reached it on.
   Distinct data stores (distinct machines/homes) get distinct ids. Opaque and
   non-secret: it only names *which* gateway, never grants access."
  [db host port]
  (let [seed
        (or (some-> db
                    discovery/db-target
                    str)
            (str host ":" port))

        raw
        (util/sha256-hex seed)]

    (subs raw 0 16)))

(defn live-client?
  [client-id]
  (when-let [lease (get-in @server-state [:clients client-id])]
    (if-let [pid (:pid lease)]
      (discovery/pid-alive-cached? pid)
      (<= (- (util/now-ms) (long (or (:last-seen-at lease) (:connected-at lease) 0)))
          (long CLIENT_LEASE_TTL_MS)))))
