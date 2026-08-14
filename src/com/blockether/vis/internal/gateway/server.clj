(ns com.blockether.vis.internal.gateway.server
  "Gateway HTTP/SSE server.

   Clojure-native stack: reitit-ring routes -> Ring middleware -> the
   Ring Jetty adapter on JDK virtual threads (`:virtual-threads? true`).
   SSE is a Ring `StreamableResponseBody` whose virtual thread is the
   connection's SINGLE socket writer: replay rides first, then it drains a
   bounded per-connection event queue that `state/fan-out!` enqueues onto,
   emitting a heartbeat comment on idle to keep the pipe warm and detect
   dead clients.

   This is internal plumbing, not a channel: it registers no channel
   descriptor and owns no renderer - it ships canonical IR and the
   client renders (§4.1). Any host process (the `vis-agent gateway start` daemon, a
   TUI run, an embedded caller) can start it alongside whatever else it
   is doing via `start!`."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.attachments :as attachments]
            [com.blockether.vis.internal.config :as config]
            [com.blockether.vis.internal.loop :as lp]
            [com.blockether.vis.internal.docs :as docs]
            [com.blockether.vis.internal.extension :as extension]
            [com.blockether.vis.internal.file-picker :as file-picker]
            [com.blockether.vis.internal.gateway.discovery :as discovery]
            [com.blockether.vis.internal.gateway.human-input :as gw-human-input]
            [com.blockether.vis.internal.gateway.pairing :as pairing]
            [com.blockether.vis.internal.gateway.protocol :as protocol]
            [com.blockether.vis.internal.gateway.push :as push]
            [com.blockether.vis.internal.gateway.state :as state]
            [com.blockether.vis.internal.gateway.wire :as wire]
            [com.blockether.vis.internal.registry :as registry]
            [com.blockether.vis.internal.provider-auth :as provider-auth]
            [com.blockether.vis.internal.provider-limits :as provider-limits]
            [com.blockether.vis.internal.providers :as providers]
            [com.blockether.vis.internal.gateway-sandbox :as gateway-sandbox]
            [com.blockether.vis.internal.resources :as resources]
            [com.blockether.vis.internal.slash :as slash]
            [com.blockether.vis.internal.toggles :as toggles]
            [com.blockether.vis.internal.voice :as voice]
            [reitit.ring :as rr]
            [ring.adapter.jetty :as jetty]
            [ring.core.protocols :as ring-protocols]
            [ring.middleware.cookies :as ring-cookies]
            [ring.middleware.params :as ring-params]
            [ring.middleware.multipart-params :as ring-multipart]
            [ring.middleware.multipart-params.byte-array :as multipart-ba]
            [taoensso.telemere :as tel])
  (:import [java.io OutputStream]
           [java.net BindException]
           [java.nio.charset StandardCharsets]
           [java.nio.file Files LinkOption OpenOption Path]
           [java.nio.file.attribute FileAttribute PosixFilePermissions]
           [java.security MessageDigest]
           [java.util.concurrent ArrayBlockingQueue TimeUnit]
           [org.eclipse.jetty.server ConnectionFactory HttpConfiguration HttpConnectionFactory
            Server ServerConnector]))


(def ^:private DEFAULT_PORT 7890)

(def ^:private DEFAULT_HOST "127.0.0.1")

(def ^:private HEARTBEAT_MS 15000)

(def ^:private SSE_QUEUE_CAP
  "Per-SSE-connection bounded event queue. `state/fan-out!` (the TURN's
   thread) only ever ENQUEUES here — it never touches the socket — so a
   stalled client (TCP backpressure: backgrounded tab, dead Wi-Fi, buffering
   proxy) fills its own queue and is DROPPED on overflow instead of parking
   the appender, the heartbeat, sibling watchers, or the turn itself."
  1024)

(def ^:private IDLE_REAP_MS 1000)

(def ^:private STARTUP_IDLE_GRACE_MS 30000)

(defonce ^:private server-state (atom nil))

;; Delivered by `stop!`; `serve-main!` parks on it so a stopped daemon process
;; EXITS instead of idling forever. In-process callers (tests, REPL) deliver
;; harmlessly — nothing is parked on the latch there.
(defonce ^:private serve-exit (promise))

(defonce ^:private idle-reaper (atom nil))

(defn- log-client-lease-warning!
  "Emit rare lease-compaction warnings to BOTH telemetry and the managed
   gateway's stderr log. The explicit stderr line remains visible when the
   gateway's asynchronous telemetry handler is saturated."
  [event data]
  (let [message (str "gateway " (name event) " " (pr-str data))]
    (tel/log! :warn [message])
    (.println System/err (str (java.time.Instant/now) " WARN " message))))

(defn- compact-client-leases
  "Drop dead-pid leases and collapse duplicate live leases to one per process.
   Returns the original map by identity when no cleanup is needed, keeping the
   once-per-second steady-state sweep allocation-light. Nil-pid leases remain
   independent because they cannot be associated with an OS process."
  [clients]
  (let
    [seen-pids
     (java.util.HashSet.)

     removed-ids
     (transient [])

     counts
     (long-array 2)]

    ; [dead duplicates]
    (reduce-kv (fn [_ client-id {:keys [pid]}]
                 (when (some? pid)
                   (cond (not (.add seen-pids pid))
                         (do (aset-long counts 1 (unchecked-inc (aget counts 1)))
                             (conj! removed-ids client-id))
                         (not (discovery/pid-alive-cached? pid))
                         (do (aset-long counts 0 (unchecked-inc (aget counts 0)))
                             (conj! removed-ids client-id))))
                 nil)
               nil
               clients)
    (let [removed-ids (persistent! removed-ids)]
      {:clients (if (seq removed-ids) (reduce dissoc clients removed-ids) clients)
       :dead (aget counts 0)
       :duplicates (aget counts 1)})))

(defn- reap-client-leases!
  "Compact the process-lease map without clobbering a concurrent register or
   release. A skipped CAS is harmless; the one-second reaper retries."
  []
  (when-let [state @server-state]
    (let
      [before (:clients state)
       {:keys [clients dead duplicates]} (compact-client-leases before)
       removed (+ (long dead) (long duplicates))]

      (when (pos? removed)
        (let [applied? (volatile! false)]
          (swap! server-state (fn [current]
                                (if (identical? before (:clients current))
                                  (do (vreset! applied? true)
                                      (-> current
                                          (assoc :clients clients)
                                          (update :client-leases-reaped-total (fnil + 0) removed)
                                          (update :client-dead-reaped-total (fnil + 0) dead)
                                          (update :client-duplicates-reaped-total
                                                  (fnil + 0)
                                                  duplicates)))
                                  (do (vreset! applied? false) current))))
          (when @applied?
            (log-client-lease-warning! :client-leases-compacted
                                       {:before (count before)
                                        :after (count clients)
                                        :dead dead
                                        :duplicates duplicates})))))))

(defn- reap-sse-clients!
  "Close every SSE stream whose owning process is gone.

   An exiting TUI releases its client lease at once (and a killed one has its
   lease compacted within a second), but the event stream it had open kept
   counting as a client of its own until a keepalive write finally failed - so a
   managed daemon outlived its last TUI by up to `HEARTBEAT_MS`. Streams without
   a pid (remote companion/browser clients) own no local process and are never
   touched."
  []
  (when-let [state @server-state]
    (doseq [[sub-id {:keys [pid close!]}] (:sse-clients state)]
      (when (and pid (not (discovery/pid-alive-cached? pid)))
        (swap! server-state update :sse-clients dissoc sub-id)
        (when close! (try (close!) (catch Throwable _ nil)))))))

(defn- gateway-client-metrics
  []
  (let
    [{:keys [clients sse-clients client-registrations-total client-releases-total
             client-replacements-total client-leases-reaped-total client-dead-reaped-total
             client-duplicates-reaped-total]}
     @server-state]
    {:gateway-client-leases (count clients)
     :gateway-sse-clients (count sse-clients)
     :gateway-client-registrations-total (long (or client-registrations-total 0))
     :gateway-client-releases-total (long (or client-releases-total 0))
     :gateway-client-replacements-total (long (or client-replacements-total 0))
     :gateway-client-leases-reaped-total (long (or client-leases-reaped-total 0))
     :gateway-client-dead-reaped-total (long (or client-dead-reaped-total 0))
     :gateway-client-duplicates-reaped-total (long (or client-duplicates-reaped-total 0))}))

(defn- client-count
  "O(1) hot-path count. Dead and duplicate process leases are removed by the
   daemon-local one-second reaper instead of being re-scanned on every status
   response."
  []
  (let [{:keys [clients sse-clients]} @server-state]
    (+ (count clients) (count sse-clients))))

(defn- running-turn-count [] (state/running-turn-count))

(defn- gateway-instance-id
  "Stable, opaque identity for THIS gateway's data store, derived from the db
   target. Deterministic across restarts and independent of the bind host
   (loopback vs LAN vs Tailscale / cloudflared), so a shared session link
   resolves to the SAME gateway no matter which URL a client reached it on.
   Distinct data stores (distinct machines/homes) get distinct ids. Opaque and
   non-secret: it only names *which* gateway, never grants access."
  [db host port]
  (let
    [seed
     (or (some-> db
                 discovery/db-target
                 str)
         (str host ":" port))

     raw
     (.digest (MessageDigest/getInstance "SHA-256")
              (.getBytes ^String seed StandardCharsets/UTF_8))]

    (subs (.formatHex (java.util.HexFormat/of) ^bytes raw) 0 16)))

(defn- status-map
  []
  (let
    [{:keys [port host db require-token? managed?]}
     @server-state

     {:keys [gateway-client-leases gateway-sse-clients gateway-client-registrations-total
             gateway-client-releases-total gateway-client-replacements-total
             gateway-client-leases-reaped-total]}
     (gateway-client-metrics)]

    {:status (if @server-state "running" "stopped")
     :id (gateway-instance-id db host port)
     :protocol (protocol/handshake)
     :pid (discovery/current-pid)
     :host host
     :port port
     :db (when db (str (discovery/db-target db)))
     :require_token (boolean require-token?)
     :managed (boolean managed?)
     :clients (+ (long gateway-client-leases) (long gateway-sse-clients))
     :client_leases gateway-client-leases
     :sse_clients gateway-sse-clients
     :client_registrations_total gateway-client-registrations-total
     :client_releases_total gateway-client-releases-total
     :client_replacements_total gateway-client-replacements-total
     :client_leases_reaped_total gateway-client-leases-reaped-total
     :running_turns (running-turn-count)}))

(declare stop!)

(defn- ensure-self-registered!
  "Repair this live daemon's registry when it is missing or still points at a
   dead predecessor. Never overwrite another live PID: that keeps close/reopen
   handoff ownership monotonic even while shutdown and startup overlap."
  []
  (when-let [{:keys [^Server server db port host token]} @server-state]
    (when (and server db (.isStarted server))
      (try (let
             [entry (discovery/read-registry db)
              owner-pid (:pid entry)
              self-pid (discovery/current-pid)
              ours? (= owner-pid self-pid)
              complete?
              (and ours? (= port (:port entry)) (= host (:host entry)) (= token (:secret entry)))]

             (when (and (not complete?)
                        (or (nil? owner-pid) ours? (not (discovery/pid-alive? owner-pid))))
               (discovery/register-self! db {:port port :host host :secret token})))
           (catch Throwable t
             (tel/log! :warn ["gateway: registry self-repair failed" (ex-message t)]))))))

(defn- idle-shutdown-eligible?
  "True when this daemon is allowed to stop itself. Foreground `vis-agent gateway start`
   is user-owned and lives until Ctrl-C/admin stop; auto-spawned gateway daemons are
   managed by client refcounts. A fresh auto-spawn gets a startup grace period so it
   does not exit before the spawning TUI has had a chance to register its lease."
  []
  (let [{:keys [managed? saw-client? started-at-ms]} @server-state]
    (and managed?
         (or saw-client?
             (>= (- (System/currentTimeMillis) (long (or started-at-ms 0)))
                 (long STARTUP_IDLE_GRACE_MS))))))

(defn- maybe-stop-when-idle!
  "Refcount shutdown (Q1): no timer/idle timeout for foreground daemons. A managed
   daemon exits only when no live client lease/SSE stream remains AND no turn is
   running. Dead-pid leases do not count, so a killed TUI cannot pin the daemon
   forever."
  []
  (when (and @server-state
             (idle-shutdown-eligible?)
             (zero? (long (client-count)))
             (zero? (long (running-turn-count))))
    (future (try (Thread/sleep 25) ; let the HTTP response that released the last client flush
                 (when (and @server-state
                            (idle-shutdown-eligible?)
                            (zero? (long (client-count)))
                            (zero? (long (running-turn-count))))
                   (stop!))
                 (catch Throwable t
                   (tel/log! :warn ["gateway: refcount shutdown failed" (ex-message t)]))))))

(defn- ensure-idle-reaper!
  "Managed daemons reap dead and duplicate process leases once per second, then
   evaluate refcount shutdown. Status/health requests therefore read an O(1)
   count and never perform OS liveness probes or rebuild a set."
  []
  (when (compare-and-set! idle-reaper nil ::starting)
    (reset! idle-reaper (future (try (loop []

                                       (Thread/sleep (long IDLE_REAP_MS))
                                       (when @server-state
                                         (ensure-self-registered!)
                                         (reap-client-leases!)
                                         (reap-sse-clients!)
                                         (maybe-stop-when-idle!)
                                         (recur)))
                                     (catch Throwable t
                                       (tel/log! :warn
                                                 ["gateway: idle reaper failed" (ex-message t)]))
                                     (finally (reset! idle-reaper nil)))))))

;; =============================================================================
;; Bearer token (§3)
;; =============================================================================

(defn- default-token-path ^Path [] (.toPath (discovery/default-token-file)))

(defn- ensure-token!
  "Read the bearer token at `path`, minting one on first run. The token file
   is CREATED owner-only (600) ATOMICALLY via create-with-attribute rather
   than write-then-chmod, so the secret is never briefly world-readable at the
   process umask."
  ^String [^Path path]
  (if (Files/exists path (make-array LinkOption 0))
    (str/trim (String. (Files/readAllBytes path) StandardCharsets/UTF_8))
    (let
      [token
       (str (java.util.UUID/randomUUID))

       owner-only
       (PosixFilePermissions/asFileAttribute (PosixFilePermissions/fromString "rw-------"))]

      (some-> (.getParent path)
              (Files/createDirectories (make-array FileAttribute 0)))
      (try (Files/createFile path (into-array FileAttribute [owner-only]))
           (catch UnsupportedOperationException _
             ;; Non-POSIX filesystem: create without the perm attribute.
             (Files/createFile path (make-array FileAttribute 0))))
      (Files/write path
                   (.getBytes token StandardCharsets/UTF_8)
                   ^"[Ljava.nio.file.OpenOption;" (make-array OpenOption 0))
      token)))

;; =============================================================================
;; Ring helpers
;; =============================================================================

(defn- json-response
  ([body] (json-response 200 body))
  ([status body]
   {:status status :headers {"Content-Type" "application/json"} :body (wire/json-str body)}))

(defn- error-response
  [status type message & {:as extra}]
  (json-response status {:error (merge {:type (name type) :message message} extra)}))

(defn- session-404
  [sid-str]
  (error-response 404 :session-not-found "unknown session" :session_id (str sid-str)))

(defn- body-json
  [request]
  (some-> (:body request)
          slurp
          wire/parse-json))

(defn- path-sid
  [request]
  (some-> (get-in request [:path-params :sid])
          parse-uuid))

(defn- query-long
  "Long value of query param `k`, or nil when it is absent, blank or unparsable.

  Ring hands back a VECTOR when a param repeats (`?limit=1&limit=2`), so read the
  LAST value — a duplicated param is a client bug, not a ClassCastException.
  Range policy belongs to the caller: 0 and negatives come back as themselves."
  [request k]
  (let [v (get-in request [:query-params k])]
    (some-> (if (sequential? v) (last v) v)
            str
            str/trim
            not-empty
            parse-long)))

(defn- path-tid [request] (get-in request [:path-params :tid]))

(defn- sid-route
  "Build a `/sessions/:sid...` route.

   Reitit (unlike Compojure) has NO inline-colon regex param syntax —
   `:sid([regex])` is swallowed whole into the PARAM NAME, so `path-sid`
   reads `[:path-params :sid]` as nil and EVERY per-session route 404s
   (soul/events/turns/transcript…), while `/v1/sessions` (no sid) still
   works. Plain `:sid` binds correctly; a non-UUID segment still yields
   `session-404` via `parse-uuid` → nil in `path-sid`."
  [tail]
  (str "/sessions/:sid" (or tail "")))

;; =============================================================================
;; SSE (§6.3)
;; =============================================================================

(defn- sse-cursor
  [request]
  (or (some-> (get-in request [:headers "last-event-id"])
              parse-long)
      (some-> (get-in request [:query-params "cursor"])
              parse-long)
      0))

(defn- request-client-pid
  "OS pid of the LOCAL vis process that opened this connection, from the
   `X-Vis-Client-Pid` header every gateway client sends. Remote clients (phone,
   browser) send none: such a stream owns no local pid and is never pid-reaped."
  [request]
  (some-> (get-in request [:headers "x-vis-client-pid"])
          parse-long))

(def ^:private sse-wake
  "Sentinel queued to unpark a pump parked in `.poll`; never written to a socket."
  ::sse-wake)

(defn- sse-closer
  "Zero-arg terminator for ONE SSE connection: mark it dead, unsubscribe, close
   the socket, and unpark the writer. The wake sentinel is the point - a pump
   parked in `.poll` does not notice a closed socket until its next
   `HEARTBEAT_MS` keepalive write throws, which is exactly how long a daemon
   kept counting a client that had already vanished."
  [^OutputStream out ^ArrayBlockingQueue queue dead? unsubscribe!]
  (fn []
    (vreset! dead? true)
    (try (unsubscribe!) (catch Throwable _ nil))
    (try (.close out) (catch Throwable _ nil))
    (.offer queue sse-wake)
    nil))

(defn- sse-sink
  "NON-BLOCKING fan-out sink for one SSE connection: offer the event onto the
   bounded `queue`, never touch the socket. On overflow (the client is not
   draining) the subscriber is dead - `close!` unsubscribes it, closes the
   socket and unparks the writer. The appending (turn) thread NEVER waits here."
  [^ArrayBlockingQueue queue close!]
  (fn [event]
    (when-not (.offer queue event) (close!))))

(defn- pump-sse!
  "Drain `queue` onto the connection — the SINGLE writer loop, run on the SSE
   body's own virtual thread. Each dequeued event goes through `write!`; an
   idle `HEARTBEAT_MS` gap emits a keepalive comment instead (dead-client
   detection). Exits when `dead?` is set (queue overflow dropped this
   subscriber) or a socket write throws (client gone)."
  [^OutputStream out ^ArrayBlockingQueue queue dead? write!]
  (loop []

    (when-not @dead?
      (let [event (.poll queue (long HEARTBEAT_MS) TimeUnit/MILLISECONDS)]
        (cond (nil? event) (do (.write out (.getBytes ": ping\n\n" StandardCharsets/UTF_8))
                               (.flush out))
              (identical? sse-wake event) nil
              :else (write! event)))
      (recur))))

(defn- sse-proxy-pad!
  "8KB SSE comment pad, written to a PROXIED connection only. Edge proxies
   (Cloudflare tunnels, nginx) buffer a streaming body until a byte threshold,
   so without it the first real frames sit in the edge buffer and live streaming
   reads as dead. Direct clients shouldn't pay the bytes."
  [^OutputStream out]
  (.write out (.getBytes (str ": " (apply str (repeat 8192 " ")) "\n\n") StandardCharsets/UTF_8))
  (.flush out))

(defn- resolve-sse-cursor
  "Effective replay cursor for one subscribed session. A NEGATIVE requested
   cursor is the live-only sentinel: it lets a client restore a bounded watch
   list without replaying every ring or issuing N `/seq` requests first. A
   live-only join to a session whose turn is ALREADY running rewinds to that
   turn's `turn.started` so the in-flight bubble replays in full — the same live
   'Vis is running: …' the originating channel shows — not a bare post-connect
   tail.

   A cursor ABOVE the session's high-water is treated EXACTLY like the sentinel.
   A client's cursor is a monotonic max it keeps across reconnects, while the
   gateway's counter is per-process: a restarted daemon (or any entry seeded at
   zero) numbers BELOW what the app already saw, and then every frame of the new
   turn fails both the `seq > cursor` replay filter and the per-connection
   dedup guard — a connected, heartbeating stream that silently delivers
   NOTHING for that session until the app is killed. Clamping here heals the
   resume in one place: the client learns the real cursor from the
   `subscription.ready` echo, so it recovers on the very next reconnect.

   Shared by BOTH event endpoints so `/v1/events?sids=…` and
   `/v1/sessions/:sid/events` resolve a cursor identically."
  ^long [sid requested]
  (let
    [requested
     (long requested)

     current
     (long (state/current-seq sid))]

    (if (or (neg? requested) (> requested current))
      (long (or (state/running-turn-start-cursor sid) current))
      requested)))

(defn- sse-ready!
  "Write the `subscription.ready` control frame for one subscribed session,
   echoing the cursor the server actually resumed from so a client that asked
   for the live-only sentinel learns its concrete resume point and can reconnect
   losslessly after its first connection.

   It also carries the daemon's OWN turn state — `current_turn_id` and `is_live`,
   read from the registry AFTER `state/subscribe!` so it describes the same
   instant the replay was captured at. That inverts the control: a reconnecting
   client no longer has to poll to find out whether the bubble it is painting is
   still real. Agreement costs zero round trips; disagreement is a definitive
   verdict that the socket missed a terminal frame, so the client reconciles at
   once instead of waiting out a grace + probe interval.

   `is_live` is what keeps 'the session is idle' distinguishable from 'this
   daemon is too old to say': a client that sees no `is_live` must treat the
   frame as no verdict at all and fall back to asking.

   EVERY SSE endpoint emits it for EVERY session it serves — single-session and
   multiplexed alike — so no client has to special-case which endpoint it is
   attached to. Like every other frame it rides `wire/sse-frame`, i.e. it is an
   ordinary `id:`/`event:`/`data:` frame, not a bespoke encoding."
  [^OutputStream out sid ^long cursor]
  (let [tid (state/current-turn-id sid)]
    (.write out
            (.getBytes (wire/sse-frame (wire/canonical {:type "subscription.ready"
                                                        :session_id (str sid)
                                                        :cursor cursor
                                                        :current_turn_id (some-> tid
                                                                                 str)
                                                        :is_live (some? tid)
                                                        :server_time_ms
                                                        (System/currentTimeMillis)}))
                       StandardCharsets/UTF_8)))
  (.flush out))

(defn- sse-body
  "Ring streamable body for one SSE subscription. Replay-then-live without
   gaps: `state/subscribe!` registers a non-blocking enqueue sink
   ([[sse-sink]]) ATOMICALLY with the replay capture; this body thread is the
   connection's ONLY socket writer — it writes the replay, then drains the
   bounded queue ([[pump-sse!]]). No output-stream lock: single-writer by
   construction. The per-connection `last-seq` guard drops duplicates (a live
   event can land in both replay and the queue). A stalled client fills its
   own queue and is dropped — it can never park the turn's appender or
   sibling watchers."
  [sid cursor proxied? owner-pid]
  (reify
    ring-protocols/StreamableResponseBody
      (write-body-to-stream [_ _ output-stream]
        (let
          [^OutputStream out
           output-stream

           sub-id
           (str (java.util.UUID/randomUUID))

           last-seq
           (atom (resolve-sse-cursor sid cursor))

           queue
           (ArrayBlockingQueue. (int SSE_QUEUE_CAP))

           dead?
           (volatile! false)

           close!
           (sse-closer out queue dead? #(state/unsubscribe! sid sub-id))

           sink
           (sse-sink queue close!)

           write!
           (fn [event]
             (when (> (long (get event "seq")) (long @last-seq))
               (.write out (.getBytes (wire/sse-frame event) StandardCharsets/UTF_8))
               (.flush out)
               (reset! last-seq (long (get event "seq")))))]

          (swap! server-state (fn [st]
                                (-> st
                                    (assoc :saw-client? true)
                                    (assoc-in [:sse-clients sub-id]
                                              {:pid owner-pid :close! close!}))))
          (try (when proxied? (sse-proxy-pad! out))
               (let [replay (state/subscribe! sid sub-id sink @last-seq)]
                 (sse-ready! out sid @last-seq)
                 (doseq [event replay]
                   (write! event)))
               (pump-sse! out queue dead? write!)
               (catch Throwable _ nil)
               (finally (state/unsubscribe! sid sub-id)
                        (swap! server-state update :sse-clients dissoc sub-id)
                        (maybe-stop-when-idle!)
                        (try (.close out) (catch Throwable _ nil))))))))

(defn- events-handler
  [request]
  (let [sid (path-sid request)]
    (if (and sid (state/soul sid))
      {:status 200
       ;; no-transform + X-Accel-Buffering: intermediaries (Cloudflare
       ;; tunnels, nginx) BUFFER a streaming body unless told not to —
       ;; buffered SSE delivers nothing until disconnect, which reads as
       ;; "streaming dead until refresh" in any proxied client.
       :headers {"Content-Type" "text/event-stream"
                 "Cache-Control" "no-cache, no-transform"
                 "X-Accel-Buffering" "no"}
       :body (sse-body sid
                       (sse-cursor request)
                       ;; forwarding header = an edge proxy sits in the path —
                       ;; only then is the anti-buffering pad worth its bytes
                       (boolean (some #(get-in request [:headers %])
                                      ["cf-ray" "cf-connecting-ip" "x-forwarded-for" "via"]))
                       (request-client-pid request))}
      (session-404 (get-in request [:path-params :sid])))))

(defn- parse-multi-sids
  "Parse the `sids` query param of the multiplexed events endpoint: a comma
   list of `sid` or `sid:cursor` tokens (cursor defaults to 0). Returns
   `[[sid cursor] …]` keeping only sids that resolve to a live soul, so a
   stale/unknown sid can't wedge the whole fan-out.

   Each sid is parsed to a `java.util.UUID` — the SAME key type `path-sid`
   hands every other route — because the gateway registry is UUID-keyed. A
   string sid here registered the SSE sink under a GHOST string entry, so idle
   tabs never received queue or turn events until their next submit parsed the
   sid correctly.

   When the request carries a `Last-Event-ID` header AND resolves to exactly
   ONE sid, that header overrides the sole sid's cursor. This lets a NATIVE
   EventSource (browser / react-native-sse) whose reconnect carries only a
   single `Last-Event-ID` resume losslessly against the multiplexed endpoint —
   so `/v1/events?sids=<sid>` is a strict superset of `/v1/sessions/:sid/events`.
   Multi-sid callers (the hand-rolled TUI mux) manage per-session cursors in the
   `sids=` param and never send `Last-Event-ID`, so they are unaffected: a single
   header cannot disambiguate N independent per-session seq counters."
  [request]
  (let
    [parsed
     (let [raw (get-in request [:query-params "sids"])]
       (when (seq raw)
         (->> (str/split raw #",")
              (keep (fn [tok]
                      (let
                        [[sid c] (str/split (str/trim tok) #":" 2)
                         sid (some-> (str/trim (str sid))
                                     parse-uuid)]

                        (when (and sid (state/soul sid))
                          [sid
                           (or (some-> c
                                       str/trim
                                       parse-long)
                               0)]))))
              (distinct)
              (vec))))

     last-event-id
     (some-> (get-in request [:headers "last-event-id"])
             str/trim
             parse-long)]

    (if (and last-event-id (= 1 (count parsed))) [[(ffirst parsed) last-event-id]] parsed)))

(defn- multi-sse-body
  "SSE body fanning MANY sessions down ONE connection — the multiplexed twin
   of [[sse-body]]. Every event already carries `:session_id`, so the client
   demuxes by session. Same single-writer discipline: every session registers
   the SAME non-blocking enqueue sink onto one bounded queue, and this body
   thread is the only socket writer. A per-session `last-seq` guard dedups
   each session's monotonic stream independently. Replays each session
   (events past its cursor) then drains live; an idle gap emits the shared
   heartbeat, and a dead client's IO error → unsubscribe of every session."
  [sid+cursors proxied? owner-pid]
  (reify
    ring-protocols/StreamableResponseBody
      (write-body-to-stream [_ _ output-stream]
        (let
          [^OutputStream out
           output-stream

           sub-id
           (str (java.util.UUID/randomUUID))

           last-seqs
           (atom {})

           queue
           (ArrayBlockingQueue. (int SSE_QUEUE_CAP))

           dead?
           (volatile! false)

           unsubscribe-all!
           (fn []
             (doseq [[sid _] sid+cursors]
               (state/unsubscribe! sid sub-id)))

           close!
           (sse-closer out queue dead? unsubscribe-all!)

           sink
           (sse-sink queue close!)

           write!
           (fn [event]
             (let [esid (str (get event "session_id"))]
               (when (> (long (get event "seq")) (long (get @last-seqs esid Long/MIN_VALUE)))
                 (.write out (.getBytes (wire/sse-frame event) StandardCharsets/UTF_8))
                 (.flush out)
                 (swap! last-seqs assoc esid (long (get event "seq"))))))]

          (swap! server-state (fn [st]
                                (-> st
                                    (assoc :saw-client? true)
                                    (assoc-in [:sse-clients sub-id]
                                              {:pid owner-pid :close! close!}))))
          (try (when proxied? (sse-proxy-pad! out))
               (doseq [[sid requested-cursor] sid+cursors]
                 (let [cursor (resolve-sse-cursor sid requested-cursor)]
                   ;; Seed the guard before atomic registration.
                   (swap! last-seqs assoc (str sid) cursor)
                   (let [replay (state/subscribe! sid sub-id sink cursor)]
                     (sse-ready! out sid cursor)
                     (doseq [event replay]
                       (write! event)))))
               (pump-sse! out queue dead? write!)
               (catch Throwable _ nil)
               (finally (unsubscribe-all!)
                        (swap! server-state update :sse-clients dissoc sub-id)
                        (maybe-stop-when-idle!)
                        (try (.close out) (catch Throwable _ nil))))))))

(defn- multi-events-handler
  "GET /v1/events?sids=a:10,b,c:3 — ONE SSE stream carrying every listed
   session's events, so a client watching N sessions holds ONE connection +
   ONE server heartbeat thread instead of N. Demuxed client-side by each
   event's `:session_id`."
  [request]
  (let [sid+cursors (parse-multi-sids request)]
    (if (seq sid+cursors)
      {:status 200
       :headers {"Content-Type" "text/event-stream"
                 "Cache-Control" "no-cache, no-transform"
                 "X-Accel-Buffering" "no"}
       :body (multi-sse-body sid+cursors
                             (boolean (some #(get-in request [:headers %])
                                            ["cf-ray" "cf-connecting-ip" "x-forwarded-for" "via"]))
                             (request-client-pid request))}
      (error-response 400 :bad-request "no valid sids"))))

;; =============================================================================
;; /metrics (§6.5)
;; =============================================================================

(defn- prometheus-text
  [snapshot]
  (let
    [series
     [[:turns-total "vis_turns_total" "counter"] [:turns-failed "vis_turns_failed_total" "counter"]
      [:cost-total "vis_turn_cost_usd_total" "counter"]
      [:duration-ms-total "vis_turn_duration_ms_total" "counter"]
      [:sessions-tracked "vis_sessions_tracked" "gauge"]
      [:turns-running "vis_turns_running" "gauge"] [:turns-executing "vis_turns_executing" "gauge"]
      [:turns-waiting "vis_turns_waiting" "gauge"] [:turns-queued "vis_turns_queued" "gauge"]
      [:turn-concurrency-limit "vis_turn_concurrency_limit" "gauge"]
      [:replay-events-retained "vis_replay_events_retained" "gauge"]
      [:env-cache-size "vis_env_cache_size" "gauge"]
      [:env-heap-pressure "vis_env_heap_pressure" "gauge"]
      [:jvm-heap-used-bytes "vis_jvm_heap_used_bytes" "gauge"]
      [:process-rss-bytes "vis_process_rss_bytes" "gauge"]
      [:jvm-heap-committed-bytes "vis_jvm_heap_committed_bytes" "gauge"]
      [:jvm-heap-max-bytes "vis_jvm_heap_max_bytes" "gauge"]
      [:jvm-gc-count-total "vis_jvm_gc_count_total" "counter"]
      [:jvm-gc-time-ms-total "vis_jvm_gc_time_ms_total" "counter"]
      [:jvm-thread-count "vis_jvm_thread_count" "gauge"]
      [:gateway-client-leases "vis_gateway_client_leases" "gauge"]
      [:gateway-sse-clients "vis_gateway_sse_clients" "gauge"]
      [:gateway-client-registrations-total "vis_gateway_client_registrations_total" "counter"]
      [:gateway-client-releases-total "vis_gateway_client_releases_total" "counter"]
      [:gateway-client-replacements-total "vis_gateway_client_replacements_total" "counter"]
      [:gateway-client-leases-reaped-total "vis_gateway_client_leases_reaped_total" "counter"]]]
    (str "# TYPE vis_turn_tokens_total counter\n"
         "vis_turn_tokens_total{kind=\"input\"} "
         (get snapshot :tokens-input 0)
         "\n"
         "vis_turn_tokens_total{kind=\"output\"} " (get snapshot :tokens-output 0)
         "\n" (apply str
                (map
                  (fn [[k metric-name metric-type]]
                    (let
                      [value (get snapshot k 0)
                       value (if (boolean? value) (if value 1 0) value)]

                      (str "# TYPE " metric-name " " metric-type "\n" metric-name " " value "\n")))
                  series)))))

(defn- metrics-handler
  [request]
  (let [snapshot (merge (state/metrics-snapshot) (gateway-client-metrics))]
    (if (str/includes? (str (get-in request [:headers "accept"])) "application/json")
      (json-response snapshot)
      {:status 200
       :headers {"Content-Type" "text/plain; version=0.0.4"}
       :body (prometheus-text snapshot)})))

;; =============================================================================
;; Route handlers (§5-§6)
;; =============================================================================

(defn- health-handler
  [request]
  ;; `/healthz` is also the recovery rendezvous for a client that still knows
  ;; the stable token but found the registry missing. The orphan-retirement probe
  ;; suppresses that repair long enough to make its authenticated stop decision.
  (when-not (= "true" (get-in request [:headers "x-vis-suppress-registry-recovery"]))
    (ensure-self-registered!))
  (let
    [{:keys [token]}
     @server-state

     supplied
     (get-in request [:headers "x-vis-gateway-secret"])]

    (json-response (assoc (status-map)
                     :status "ok"
                     :secret_match (= token supplied)))))

(defn- register-client-lease
  "Insert one opaque client lease while enforcing the process invariant: at
   most one lease per non-nil pid. Returns replacement count for observability."
  [clients client-id {:keys [pid] :as lease}]
  (if (nil? pid)
    {:clients (assoc clients client-id lease) :replaced 0}
    (let
      [stale-ids (persistent! (reduce-kv (fn [ids existing-id existing]
                                           (if (= pid (:pid existing)) (conj! ids existing-id) ids))
                                         (transient [])
                                         clients))]
      {:clients (assoc (reduce dissoc clients stale-ids) client-id lease)
       :replaced (count stale-ids)})))

(defn- client-register-handler
  [request]
  (let
    [{:strs [pid kind]}
     (body-json request)

     client-id
     (str (java.util.UUID/randomUUID))

     lease
     {:pid pid :kind kind :connected-at (System/currentTimeMillis)}

     replacement-stats
     (long-array 2)]

    ; [this registration, cumulative]
    (swap! server-state (fn [st]
                          (let
                            [{:keys [clients replaced]}
                             (register-client-lease (:clients st) client-id lease)

                             total
                             (+ (long (or (:client-replacements-total st) 0)) (long replaced))]

                            (aset-long replacement-stats 0 (long replaced))
                            (aset-long replacement-stats 1 (long total))
                            (-> st
                                (assoc :saw-client? true
                                       :clients clients)
                                (update :client-registrations-total (fnil inc 0))
                                (assoc :client-replacements-total total)))))
    (let
      [replaced
       (aget replacement-stats 0)

       replacement-total
       (aget replacement-stats 1)]

      (when (and (pos? replaced)
                 (or (= 1 replacement-total) (zero? (long (mod replacement-total 100)))))
        (log-client-lease-warning! :client-lease-replaced
                                   {:replaced replaced
                                    :replacements-total replacement-total
                                    :leases (count (:clients @server-state))})))
    (json-response {:client_id client-id :status (status-map)})))

(defn- client-release-handler
  [request]
  (let [client-id (get-in request [:path-params :cid])]
    (swap! server-state (fn [st]
                          (if (contains? (:clients st) client-id)
                            (-> st
                                (update :clients dissoc client-id)
                                (update :client-releases-total (fnil inc 0)))
                            st)))
    (maybe-stop-when-idle!)
    (json-response {:released true :status (status-map)})))

(defn- status-handler [_] (json-response (status-map)))

(defn- stop-handler
  "POST /v1/admin/stop. Logs WHO asked and what it costs BEFORE stopping: this
   path and the JVM shutdown hook were previously indistinguishable in the log,
   which made every unexplained daemon death (`gateway: draining before stop N
   turn(s) running`) unattributable."
  [request]
  (tel/log! :warn
            ["gateway: /v1/admin/stop requested by" (or (:remote-addr request) "?")
             (str "ua=" (or (get-in request [:headers "user-agent"]) "?")) "-" (running-turn-count)
             "turn(s) running"])
  (future (try (Thread/sleep 25)
               (stop!)
               (catch Throwable t
                 (tel/log! :warn ["gateway: explicit stop failed" (ex-message t)]))))
  (json-response {:stopping true :status (status-map)}))

(defn- models-handler
  [_]
  (json-response
    {:providers (mapv (fn [{:provider/keys [id doc]}]
                        {:id (name id) :doc doc})
                      (registry/registered-providers))
     ;; Configured fleet with per-provider model names — the same source every
     ;; channel's model picker renders (`configured-providers`), so no channel
     ;; needs its own catalog route.
     :catalog (into []
                    (keep (fn [{:keys [id models]}]
                            (let [names (into [] (keep :name) models)]
                              (when (seq names)
                                {:id (name id) :label (config/display-label id) :models names}))))
                    (providers/configured-providers))}))

(def ^:private web-native-slashes
  [{:name "/help" :doc "Show the available slash commands."}
   {:name "/new-session" :doc "Create and open a new session. Optional text starts its first turn."}
   {:name "/sessions" :doc "Return to the session list."}
   {:name "/clear" :doc "Start a fresh session without deleting this transcript."}])

(defn- slashes-handler
  "GET /v1/sessions/:sid/slashes — resolve dynamic slash/template discovery in
   the active session workspace, then add commands implemented by Companion."
  [request]
  (if-let [sid (path-sid request)]
    (if-let [info (state/session-workspace-info sid)]
      (let [root (or (get info "root") (:root info))]
        (extension/with-context {:env {:session-id sid :workspace/root root}}
                                (json-response {:commands
                                                (slash/slash-palette :web web-native-slashes)})))
      (session-404 (get-in request [:path-params :sid])))
    (session-404 (get-in request [:path-params :sid]))))

(defn- configured-provider
  [provider-id]
  (or (some (fn [provider]
              (when (= provider-id (:id provider)) provider))
            (providers/configured-providers))
      {:id provider-id}))

(defn- provider-status-handler
  [request]
  (let
    [provider-id (some-> (get-in request [:path-params :provider-id])
                         keyword)]
    (json-response {:status (providers/provider-status (configured-provider provider-id))})))

(defn- provider-limits-handler
  [request]
  (let
    [provider-id (some-> (get-in request [:path-params :provider-id])
                         keyword)]
    (json-response {:report (provider-limits/provider-limits provider-id)})))

(defn- provider-models-handler
  "GET /v1/providers/:provider-id/models[?show_all=true] — the LIVE model
   catalog for ONE provider, fetched DAEMON-side so the gateway stays the SOLE
   owner of OAuth token resolution. The `svar/models!` probe (and any OAuth
   token refresh it triggers) runs HERE, in the daemon that owns the credential
   file — never in a thin client. Returns `{models [id …] hidden_count n}` with
   snake_case STRING wire keys."
  [request]
  (let
    [provider-id
     (some-> (get-in request [:path-params :provider-id])
             keyword)

     show-all?
     (contains? #{"1" "true" "yes"}
                (some-> (get-in request [:query-params "show_all"])
                        str/lower-case))

     provider
     (configured-provider provider-id)

     {:keys [models hidden-count]}
     (providers/model-options provider (providers/default-model-names provider) show-all?)]

    (json-response {:models (vec models) :hidden-count (long (or hidden-count 0))})))

(defn- auth-error-response
  "Map a `provider-auth` failure map onto an HTTP status. Unknown provider/flow
   is 404, an unsupported or malformed request is 400, and a genuine upstream
   OAuth failure is 502 — the caller can tell 'you asked wrong' from 'GitHub
   said no'."
  [{:keys [error message]}]
  (error-response (case error
                    (:unknown-provider :unknown-flow)
                    404

                    (:auth-unsupported :auth-self-minted :missing-input :invalid-input)
                    400

                    502)
                  (or error :auth-failed)
                  (or message "authorization failed")))

(defn- provider-auth-start-handler
  "POST /v1/providers/:provider-id/auth/start — mint a headless OAuth flow.

   Answers `{flow_id, kind, url, user_code?, verification_uri?, interval_ms?,
   instructions?}`. `kind` is `pkce` (finish with `auth/complete`) or `device`
   (finish by polling `auth/poll`). The PKCE verifier and device code stay in
   the daemon and never appear in this response."
  [request]
  (let
    [provider-id
     (some-> (get-in request [:path-params :provider-id])
             keyword)

     result
     (provider-auth/start-auth! provider-id)]

    (if (:ok? result) (json-response (:flow result)) (auth-error-response result))))

(defn- provider-auth-complete-handler
  "POST /v1/providers/:provider-id/auth/complete {flow_id, redirect_url|api_key} —
   finish the flow the client cannot finish alone: a PKCE flow with the URL the
   user pasted back from the browser, or an `api-key` flow with the key the user
   typed. Credentials are exchanged and persisted DAEMON-side; the response
   carries only `{status}`."
  [request]
  (let
    [body
     (try (body-json request) (catch Throwable _ nil))

     flow-id
     (or (get body "flow_id") (get-in request [:query-params "flow_id"]))

     input
     (or (get body "redirect_url")
         (get body "api_key")
         (get body "code")
         (get-in request [:query-params "redirect_url"]))

     result
     (provider-auth/complete-auth! flow-id input)]

    (if (:ok? result) (json-response {:status (:status result)}) (auth-error-response result))))

(defn- provider-auth-poll-handler
  "POST /v1/providers/:provider-id/auth/poll {flow_id} — read a device flow's
   verdict WITHOUT blocking: `pending`, `ok`, or `error`. The blocking wait
   runs on a daemon thread from the moment `auth/start` returned, so a phone
   can poll this on any cadence it likes."
  [request]
  (let
    [body
     (try (body-json request) (catch Throwable _ nil))

     flow-id
     (or (get body "flow_id") (get-in request [:query-params "flow_id"]))

     result
     (provider-auth/poll-auth! flow-id)]

    (if (:ok? result)
      (json-response (select-keys result [:status :message]))
      (auth-error-response result))))

(defn- provider-auth-cancel-handler
  "POST /v1/providers/:provider-id/auth/cancel {flow_id} — forget an abandoned
   flow. Idempotent, so a client that lost track of its flow can always call it."
  [request]
  (let
    [body
     (try (body-json request) (catch Throwable _ nil))

     flow-id
     (or (get body "flow_id") (get-in request [:query-params "flow_id"]))]

    (json-response (select-keys (provider-auth/cancel-auth! flow-id) [:status]))))

(defn- provider-logout-handler
  "POST /v1/providers/:provider-id/logout — clear the provider's persisted
   credentials through its registered logout and invalidate the cached fleet,
   so the very next `/v1/router` read shows `is_authenticated` false."
  [request]
  (let
    [provider-id
     (some-> (get-in request [:path-params :provider-id])
             keyword)

     result
     (provider-auth/logout! provider-id)]

    (if (:ok? result) (json-response {:status (:status result)}) (auth-error-response result))))

(defn- router-provider-entry
  "One row of the unified router payload, carrying both explicit tags: the
   PRIMARY pair every turn starts on and the FALLBACK pair on another provider."
  [provider primary fallback]
  (let
    [id
     (:id provider)

     is-default
     (= id (:provider-id primary))

     is-fallback
     (= id (:provider-id fallback))]

    {:id (name id)
     :label (config/display-label id)
     :base-url (or (config/provider-base-url provider) (:base-url provider))
     :models (into [] (keep :name) (:models provider))
     :is-default is-default
     :default-model (when is-default (:model primary))
     :is-fallback is-fallback
     :fallback-model (when is-fallback (:model fallback))
     :status (providers/provider-status provider)
     :limits (providers/provider-limits-safe provider)}))

(defn- router-selection-json
  "Both router tags as one payload, `null` where a role is untagged. Answered by
   every PATCH so a client repaints without re-reading the catalog."
  []
  (let
    [fleet
     (providers/picker-fleet)

     primary
     (providers/default-selection fleet)

     fallback
     (providers/fallback-selection fleet primary)]

    {:default-provider (some-> (:provider-id primary)
                               name)
     :default-model (:model primary)
     :fallback-provider (some-> (:provider-id fallback)
                                name)
     :fallback-model (:model fallback)}))

(defn- router-fleet-json
  "The whole provider catalog with both explicit tags.

   Every fleet MUTATION answers with this exact payload, so a client that just
   added or removed a provider repaints from the response it already holds —
   no second read, and no window where the phone shows a fleet the daemon no
   longer has."
  []
  (let
    [fleet
     (providers/picker-fleet)

     primary
     (providers/default-selection fleet)

     fallback
     (providers/fallback-selection fleet primary)]

    {:providers (mapv #(router-provider-entry % primary fallback) fleet)}))

(defn- router-handler
  "GET /v1/router — the whole provider catalog and both explicit tags."
  [_]
  (json-response (router-fleet-json)))

(defn- router-default-handler
  "PATCH /v1/router — tag one provider/model pair.

   `role` selects the tag: `primary` (the default, and what every client written
   before roles sends) or `fallback`, which the daemon REFUSES on the primary's
   own provider. `{\"role\": \"fallback\"}` with no provider and no model clears
   the fallback. The answer always carries both tags."
  [request]
  (let
    [{:strs [provider model role]}
     (body-json request)

     role
     (or (some-> role
                 str
                 str/trim
                 str/lower-case
                 not-empty)
         "primary")

     is-blank
     (and (str/blank? (str provider)) (str/blank? (str model)))]

    (cond (not (contains? #{"primary" "fallback"} role))
          (error-response 400 :invalid-request "role must be \"primary\" or \"fallback\"")
          (and (= role "fallback") is-blank) (do (providers/clear-fallback-selection! :gateway)
                                                 (json-response (router-selection-json)))
          is-blank
          (error-response 400 :invalid-request "provider and model must be non-blank strings")
          :else (try (if (= role "fallback")
                       (providers/save-fallback-selection! provider model :gateway)
                       (providers/save-default-selection! provider model :gateway))
                     (json-response (router-selection-json))
                     (catch clojure.lang.ExceptionInfo e
                       (error-response 400 :invalid-request (ex-message e)))))))

(defn- provider-preset-json
  "One 'Add Provider' row: what the preset IS, and what adding it will ask for.

   `auth_kind` tells the client which second step follows the add — `oauth`
   (start a flow), `api-key` (collect a key), `none` (a local runtime needs
   neither) — and `is_local` marks the presets whose `base_url` the user OWNS,
   since LM Studio and Ollama listen wherever that machine put them."
  [preset]
  (let [pid (:id preset)]
    (cond->
      {:id (name pid)
       :label (or (:label preset) (config/display-label pid))
       :auth-kind (name (providers/auth-kind pid))
       :is-local (contains? providers/local-no-auth-provider-ids pid)
       :models (mapv :name (providers/default-model-configs preset))}
      (:base-url preset)
      (assoc :base-url (:base-url preset))

      (:api-style preset)
      (assoc :api-style (name (:api-style preset))))))

(defn- provider-presets-handler
  "GET /v1/provider-presets — every provider this machine knows how to add and
   does NOT carry yet. This is the 'Add Provider' picker, headless: without it a
   client can only ever operate the providers that are already configured."
  [_]
  (json-response {:presets (mapv provider-preset-json (providers/available-presets))}))

(defn- add-provider-handler
  "POST /v1/providers {id, base_url?} — put a preset into THIS machine's fleet.

   The daemon owns config: a client names a preset and, for a LOCAL provider,
   where it listens; the models come from the preset. No credential is accepted
   here — a fresh provider starts signed out and finishes through
   `/v1/providers/:id/auth/*`, which is the ONE path that writes a key, on the
   machine that owns it."
  [request]
  (let
    [body
     (try (body-json request) (catch Throwable _ nil))

     raw-id
     (some-> (get body "id")
             str
             str/trim
             not-empty)

     provider-id
     (some-> raw-id
             keyword)

     preset
     (some-> provider-id
             config/provider-template)

     base-url
     (some-> (get body "base_url")
             str
             str/trim
             (str/replace #"/+$" "")
             not-empty)

     configured
     (into #{} (map :id) (providers/configured-providers))]

    (cond
      (nil? provider-id) (error-response 400 :invalid-request "id must be a non-blank provider id")
      (nil? preset) (error-response 404 :unknown-provider (str "no such provider preset: " raw-id))
      (contains? configured provider-id)
      (error-response 409 :provider-exists (str raw-id " is already configured"))
      :else (let
              [preset
               (cond-> preset
                 base-url
                 (assoc :base-url base-url))

               models
               (providers/default-model-configs preset)]

              (providers/add-config-provider! (providers/provider-config-with-models preset models)
                                              :gateway)
              (json-response (router-fleet-json))))))

(defn- remove-provider-handler
  "DELETE /v1/providers/:provider-id — drop it from the fleet and run its
   registered logout, so removing a provider never leaves a credential behind.
   Idempotent: removing what is not there answers `is_removed` false, never an
   error."
  [request]
  (let
    [provider-id
     (some-> (get-in request [:path-params :provider-id])
             keyword)

     is-removed
     (boolean (some-> provider-id
                      (providers/remove-provider! :gateway)))]

    (json-response (assoc (router-fleet-json) :is-removed is-removed))))

(defn- toggle-json
  "One settings row as JSON — the wire twin of the server-side
   `toggle-row` hiccup: boolean rows carry `enabled`, enum rows carry
   `value` + `choices`."
  [{:keys [id label description type]}]
  (let
    [choices
     (try (toggles/choices-of id) (catch Throwable _ nil))

     value
     (try (toggles/value-of id) (catch Throwable _ nil))

     pretty
     (fn [v]
       (if (keyword? v) (name v) (str v)))

     base
     {:id id :label (str (or label id)) :type (name (or type (if (seq choices) :enum :boolean)))}]

    (cond-> base
      description
      (assoc :description (str description))

      (seq choices)
      (assoc :value
        (pretty value) :choices
        (mapv pretty choices))

      (empty? choices)
      (assoc :enabled (boolean (try (toggles/enabled? id) (catch Throwable _ false)))))))

(defn- list-settings-handler
  "GET /v1/settings[?channel=web|all] — the feature-toggle registry every
   channel renders (web dialog, TUI pane, mobile app) as grouped JSON.
   `channel` scopes rows exactly like `toggles-for-channel`; `all` (or
   `*`, or omitting the param) ships every visible toggle regardless of
   channel — the cross-channel view a remote companion wants."
  [request]
  (let
    [raw
     (get-in request [:query-params "channel"])

     channel
     (when (and raw (not (contains? #{"all" "*"} (str/lower-case raw)))) (keyword raw))

     specs
     (if channel (toggles/toggles-for-channel channel) (toggles/visible-toggles))

     grouped
     (sort-by (comp str key) (group-by #(or (:group %) :other) specs))]

    (json-response {:groups (into []
                                  (map (fn [[group group-specs]]
                                         {:id (name group)
                                          :title (str/capitalize
                                                   (str/replace (name group) #"[-_]+" " "))
                                          :toggles (mapv toggle-json group-specs)}))
                                  grouped)})))

(defn- get-setting-handler
  "GET /v1/settings/:id — ONE registered toggle row, INCLUDING the ids
   `list-settings-handler` hides. `reasoning_level` is registered
   `:settings? false` because every channel drives it from its own dedicated
   control (TUI Ctrl+R, the companion's model dialog) rather than the Settings
   list, so a remote channel still needs a way to READ its current value.
   Same row shape as the list endpoint."
  [request]
  (let
    [id-str
     (get-in request [:path-params :id])

     id
     (when (string? id-str) (str/trim id-str))

     spec
     (when (seq id) (toggles/toggle-spec id))]

    (cond (not (toggles/toggle-id? id))
          (error-response 400 :bad-setting-id "settings id must be a snake_case string")
          (nil? spec) (error-response 404 :unknown-setting "no such setting" :id (str id-str))
          :else (json-response (toggle-json spec)))))

(defn- set-setting-handler
  "POST /v1/settings {id, action} — flip (`toggle`, the default), `cycle`,
   or set an exact enum choice (`value` action with `{value}`) on one
   registered toggle; answers with the refreshed row. JSON body or query
   params both work."
  [request]
  (let
    [body
     (try (body-json request) (catch Throwable _ nil))

     id-str
     (or (get body "id") (get-in request [:query-params "id"]))

     action
     (str (or (get body "action") (get-in request [:query-params "action"]) "toggle"))

     raw-value
     (or (get body "value") (get-in request [:query-params "value"]))

     id
     (when (string? id-str) (str/trim id-str))

     spec
     (when (seq id) (toggles/toggle-spec id))]

    (cond (not (toggles/toggle-id? id))
          (error-response 400 :bad-setting-id "settings id must be a snake_case string")
          (nil? spec) (error-response 404 :unknown-setting "no such setting" :id (str id-str))
          :else (do (cond (= action "value")
                          ;; Set an EXACT choice. The wire carries an enum choice as its
                          ;; string name (e.g. "balanced"); map it back to the registered
                          ;; choice (keyword or string) before `set-value!` validates it.
                          (let
                            [choices
                             (toggles/choices-of id)

                             chosen
                             (if (seq choices)
                               (some #(when (= (name %) (str raw-value)) %) choices)
                               raw-value)]

                            (when (some? chosen) (toggles/set-value! id chosen)))
                          (= action "cycle") (toggles/cycle-value! id)
                          :else (toggles/set-enabled! id (not (toggles/enabled? id))))
                    (json-response (toggle-json (toggles/toggle-spec id)))))))

(defn- mcp-error-response
  [e]
  (let
    [{:keys [type]}
     (ex-data e)

     status
     (case type
       :mcp/not-found
       404

       :mcp/invalid-name
       400

       :mcp/invalid-server
       400

       :mcp/not-managed
       409

       ;; An auth flow the gateway no longer has: abandoned, cancelled, spent, or
       ;; swept after its TTL. The client must start a new one.
       :mcp/oauth-flow-not-found
       404

       409
       400)]

    (error-response status (or type :mcp/invalid-request) (ex-message e))))

(defn- mcp-servers-handler
  [_]
  (json-response ((requiring-resolve
                    'com.blockether.vis.internal.foundation.mcp.core/gateway-servers))))

(defn- save-mcp-server-handler
  [request]
  (try (let
         [body
          (body-json request)

          name
          (or (get-in request [:path-params :name]) (get body "name"))

          server
          (or (get body "server") body)]

         (json-response ((requiring-resolve
                           'com.blockether.vis.internal.foundation.mcp.core/save-gateway-server!)
                          name
                          server)))
       (catch clojure.lang.ExceptionInfo e (mcp-error-response e))
       (catch Throwable e (error-response 400 :mcp/invalid-request (ex-message e)))))

(defn- set-mcp-server-enabled-handler
  [request]
  (try (let [enabled (get (body-json request) "enabled")]
         (if (boolean? enabled)
           (json-response
             ((requiring-resolve
                'com.blockether.vis.internal.foundation.mcp.core/set-gateway-server-enabled!)
               (get-in request [:path-params :name])
               enabled))
           (error-response 400 :mcp/invalid-request "enabled must be a boolean")))
       (catch clojure.lang.ExceptionInfo e (mcp-error-response e))))

(defn- delete-mcp-server-handler
  [request]
  (try (json-response ((requiring-resolve
                         'com.blockether.vis.internal.foundation.mcp.core/delete-gateway-server!)
                        (get-in request [:path-params :name])))
       (catch clojure.lang.ExceptionInfo e (mcp-error-response e))))

(defn- test-mcp-server-handler
  [request]
  (try (let [body (body-json request)]
         (json-response ((requiring-resolve
                           'com.blockether.vis.internal.foundation.mcp.core/test-gateway-server!)
                          (get body "name")
                          (or (get body "server") body))))
       (catch clojure.lang.ExceptionInfo e (mcp-error-response e))
       (catch Throwable e (error-response 400 :mcp/test-failed (ex-message e)))))

(defn- kill-mcp-server-handler
  "Stop a server NOW and hold it down. Not a config edit — works for hand-written
   servers too, because killing a runaway process is not rewriting the user's file."
  [request]
  (try (json-response ((requiring-resolve
                         'com.blockether.vis.internal.foundation.mcp.core/kill-gateway-server!)
                        (get-in request [:path-params :name])))
       (catch clojure.lang.ExceptionInfo e (mcp-error-response e))))

(defn- start-mcp-server-handler
  [request]
  (try (json-response ((requiring-resolve
                         'com.blockether.vis.internal.foundation.mcp.core/start-gateway-server!)
                        (get-in request [:path-params :name])))
       (catch clojure.lang.ExceptionInfo e (mcp-error-response e))))

(defn- mcp-auth-start-handler
  "Begin a headless OAuth flow for an HTTP MCP server. The response carries the
   URL the CLIENT shows its user — the gateway never assumes a browser of its own."
  [request]
  (try (json-response
         ((requiring-resolve
            'com.blockether.vis.internal.foundation.mcp.core/start-gateway-server-auth!)
           (get-in request [:path-params :name])))
       (catch clojure.lang.ExceptionInfo e (mcp-error-response e))
       (catch Throwable e (error-response 400 :mcp/oauth-failed (ex-message e)))))

(defn- mcp-auth-flow-id
  [body]
  (let [flow-id (get body "flow_id")]
    (when (and (string? flow-id) (seq (str/trim flow-id))) (str/trim flow-id))))

(defn- mcp-auth-complete-handler
  [request]
  (try (let
         [body
          (body-json request)

          flow-id
          (mcp-auth-flow-id body)

          input
          (or (get body "input") (get body "redirect_url") (get body "code"))]

         (if (and flow-id (string? input) (seq (str/trim ^String input)))
           (json-response
             ((requiring-resolve
                'com.blockether.vis.internal.foundation.mcp.core/complete-gateway-server-auth!)
               flow-id
               input))
           (error-response 400
                           :mcp/invalid-request
                           "flow_id and input (redirect URL or code) are required")))
       (catch clojure.lang.ExceptionInfo e (mcp-error-response e))
       (catch Throwable e (error-response 400 :mcp/oauth-failed (ex-message e)))))

(defn- mcp-auth-poll-handler
  [request]
  (try (if-let [flow-id (mcp-auth-flow-id (body-json request))]
         (json-response
           ((requiring-resolve
              'com.blockether.vis.internal.foundation.mcp.core/poll-gateway-server-auth!)
             flow-id))
         (error-response 400 :mcp/invalid-request "flow_id is required"))
       (catch clojure.lang.ExceptionInfo e (mcp-error-response e))))

(defn- mcp-auth-cancel-handler
  [request]
  (try (if-let [flow-id (mcp-auth-flow-id (body-json request))]
         (json-response
           ((requiring-resolve
              'com.blockether.vis.internal.foundation.mcp.core/cancel-gateway-server-auth!)
             flow-id))
         (error-response 400 :mcp/invalid-request "flow_id is required"))
       (catch clojure.lang.ExceptionInfo e (mcp-error-response e))))

(defn- mcp-auth-logout-handler
  [request]
  (try (json-response
         ((requiring-resolve
            'com.blockether.vis.internal.foundation.mcp.core/logout-gateway-server-auth!)
           (get-in request [:path-params :name])))
       (catch clojure.lang.ExceptionInfo e (mcp-error-response e))))

(defn- create-session-handler
  [request]
  (let [body (body-json request)]
    (json-response 201
                   (state/create-session! {:channel (some-> (get body "channel")
                                                            keyword)
                                           :title (get body "title")
                                           :external-id (get body "external_id")
                                           :workspace-id (get body "workspace_id")
                                           :root (get body "root")}))))

(def ^:private sessions-order-header
  "Response header carrying `state/list-sessions-page`'s ordering digest.

   Deliberately a HEADER and not a body field: the ordering moves whenever ANY
   session anywhere in the fleet is touched, while a given window's rows usually
   do not. In the payload it would land inside `sessions-etag` and turn every
   unrelated change into a full re-download of every window; as a header it also
   rides along on the 304 that keeps those windows free."
  "X-Vis-Sessions-Order")

(defn- sessions-etag
  "Conditional-GET validator for a session-list ANSWER: SHA-256 over the rows
   MINUS `server_time_ms`, together with the window frame (total/offset/limit)
   that identifies WHICH answer they are.

   `server_time_ms` is a per-request clock sample (`state/soul` takes it fresh on
   every call), so hashing it would make every poll a guaranteed miss and no
   client could ever revalidate. WEAK (`W/`) for exactly that reason: the bytes
   may differ between two answers, the CONTENT a client renders cannot."
  ^String [payload]
  (let
    [stable
     (wire/json-str [(:total payload) (:offset payload) (:limit payload) (:root payload)
                     (mapv #(dissoc % "server_time_ms") (:sessions payload))])

     raw
     (.digest (MessageDigest/getInstance "SHA-256") (.getBytes stable StandardCharsets/UTF_8))]

    (str "W/\"" (subs (.formatHex (java.util.HexFormat/of) ^bytes raw) 0 32) "\"")))

(defn- list-sessions-handler
  "GET /v1/sessions[?limit=&offset=&root=] — sessions in navigator order, WINDOWED on
   request, with a validator so a poller can revalidate instead of re-downloading.

   Without `limit` this is still the whole fleet, so every existing client keeps
   working. With it, the reply carries `total` / `offset` / `limit` / `has_more`
   and the gateway only decorates the rows it returns: the ordering is derived
   from cheap facts (see `state/list-sessions-page`), so a 100-row first page of
   a 448-session store costs roughly a fifth of the ~257ms full build and a fifth
   of its ~300KB — the app paints its first screen without waiting for the tail.

   The companion refreshes this on a timer and the payload is BIG while the
   content is identical until a turn moves. With `If-None-Match` the steady state
   collapses to a 304 with no body: nothing transferred, nothing parsed, nothing
   reconciled, nothing repainted. A window param that is PRESENT but unparsable
   is a 400, never a silent fallback to the full fleet."
  [request]
  (let
    [given?
     (fn [k]
       (some? (get-in request [:query-params k])))

     limit
     (query-long request "limit")

     offset
     (query-long request "offset")

     ;; One PROJECT's window. The companion pages a project header in place, and a
     ;; page it slices locally is not a page: it needs the whole fleet downloaded
     ;; first. With `root` the gateway cuts the ordering to that project, so
     ;; `total`/`has_more` describe the project the pager is printing.
     root
     (some-> (get-in request [:query-params "root"])
             str
             not-empty)]

    (if (or (and (given? "limit") (nil? limit)) (and (given? "offset") (nil? offset)))
      (error-response 400 :invalid-window "limit and offset must be integers")
      (let
        [page
         (state/list-sessions-page :all {:limit limit :offset offset :root root})

         payload
         {:sessions (:sessions page)
          :root root
          :total (:total page)
          :offset (:offset page)
          :limit (:limit page)
          :has-more (:has-more page)}

         etag
         (sessions-etag payload)

         ;; Same ordering digest on 200 and 304 alike: a client draining several
         ;; windows compares them to see whether the fleet was re-ranked under it
         ;; mid-walk (see `state/list-sessions-page`), and a revalidated window
         ;; that answers 304 must not leave that comparison blind.
         base
         {"ETag" etag "Cache-Control" "no-cache" sessions-order-header (:order-digest page)}]

        (if (= etag (get-in request [:headers "if-none-match"]))
          {:status 304 :headers base :body nil}
          (update (json-response payload) :headers merge base))))))

(defn- search-sessions-handler
  "GET /v1/sessions/actions/search?q=&channel= — the ranked answer to `q`.

   The SERVER decides relevance: `matches` carries every session whose title or
   transcript matches, each with the `rank` band it earned (title 0, request 1,
   reply 2, thinking 3) and `is_in_title`, already ordered band-then-newest.
   Clients PAINT that order; they never re-derive it, so a third client cannot
   invent a fourth ordering. `session_ids` mirrors the same order."
  [request]
  (let
    [q
     (str (get-in request [:query-params "q"]))

     channel
     (or (some-> (get-in request [:query-params "channel"])
                 keyword)
         :all)

     ;; ONE search per request: `session_ids` is derived from the matches
     ;; instead of re-running the identical (previously full-table) scan.
     matches
     (state/search-session-matches channel q)]

    (json-response {:session_ids (mapv :session_id matches) :matches matches})))

(defn- soul-handler
  [request]
  (let [sid (path-sid request)]
    (if-let
      [soul (some-> sid
                    state/soul)]
      (json-response (cond-> soul
                       (= "queued" (get-in request [:query-params "include"]))
                       (assoc :queued_turns (state/list-queued-turns sid))))
      (session-404 (get-in request [:path-params :sid])))))

(defn- patch-session-handler
  "PATCH /v1/sessions/:sid — rename (`{title}`) OR change project membership
   (`{project_id}`, null to remove from project). Membership takes precedence."
  [request]
  (let
    [sid
     (path-sid request)

     body
     (body-json request)]

    (cond (not sid) (session-404 (get-in request [:path-params :sid]))
          (contains? body "project_id") (if-let
                                          [soul (state/assign-project! sid
                                                                       (some-> (get body
                                                                                    "project_id")
                                                                               parse-uuid))]
                                          (json-response soul)
                                          (session-404 (get-in request [:path-params :sid])))
          (str/blank? (str (get body "title")))
          (error-response 400 :invalid-request "title must be a non-blank string")
          :else (if-let [soul (state/set-title! sid (get body "title"))]
                  (json-response soul)
                  (session-404 (get-in request [:path-params :sid]))))))

(defn- delete-session-handler
  [request]
  (some-> (path-sid request)
          state/close-session!)
  {:status 204 :headers {} :body nil})

(defn- release-session-handler
  "POST /v1/sessions/:sid/release — a client closed its VIEW of the session
   (TUI tab/exit). Releases the live runtime and stops the session's background
   resources; the persisted transcript stays resumable. Idempotent, 204 always
   (mirrors DELETE — releasing an unknown sid is a no-op, not an error)."
  [request]
  (some-> (path-sid request)
          state/release-session!)
  {:status 204 :headers {} :body nil})

;; --- Projects (cross-channel) + movable project sessions + ownership (V6/V7) ---

(defn- path-pid
  [request]
  (some-> (get-in request [:path-params :pid])
          parse-uuid))

(defn- project-404
  [pid-str]
  (error-response 404 :project-not-found "unknown project" :project_id (str pid-str)))

;; =============================================================================
;; Filesystem browse — picking a workspace root
;; =============================================================================
;;
;; A machine OWNS its projects, so the only place that knows which folders exist
;; is the machine itself. The companion's "Switch project" sheet walks THIS tree;
;; it commits a folder and lets `POST /v1/sessions {root}` decide what that folder
;; is. Directories only — a picker that offers files offers a root that cannot be
;; one — and the two facts a chooser actually reads: how much is in it, and the
;; branch if it is a worktree.

(def ^:private fs-entry-limit
  "A home folder with 4000 entries is a scroll, not a picker. The client is told
   the list was cut so it can say `typing the path` instead of lying by omission."
  400)

(defn- fs-home [] (System/getProperty "user.home"))

(defn- expand-user
  "`~` and `~/x` name the GATEWAY user's home, never the phone's."
  [path]
  (let [path (str/trim (str path))]
    (cond (str/blank? path) (fs-home)
          (= "~" path) (fs-home)
          (str/starts-with? path "~/") (.getAbsolutePath (io/file (fs-home) (subs path 2)))
          :else path)))

(defn- git-branch
  "The branch `dir` has checked out, or nil when it is not a worktree. Read from
   `.git/HEAD` rather than shelled out: this runs once per row of a listing."
  [^java.io.File dir]
  (let [head (io/file dir ".git" "HEAD")]
    (when (.isFile head)
      (let [line (str/trim (slurp head))]
        (or (second (re-find #"^ref:\s+refs/heads/(.+)$" line))
            (when-not (str/blank? line) (subs line 0 (min 7 (count line)))))))))

(defn- fs-entry
  [^java.io.File dir]
  (let
    [kids
     (.listFiles dir)

     branch
     (git-branch dir)]

    (cond->
      {:name (.getName dir)
       :path (.getAbsolutePath dir)
       :entry-count (if kids (alength kids) 0)
       :is-repo (some? branch)}
      branch
      (assoc :branch branch))))

(defn- browse-fs-handler
  "GET /v1/fs[?path=…] — the directories inside `path` (default: this user's home),
   so a client can pick a workspace root by recognition instead of by typing one.
   Dotfolders are skipped: they are not projects, and they are most of `~`."
  [request]
  (let [^java.io.File dir (io/file (expand-user (get-in request [:query-params "path"])))]
    (cond (not (.isDirectory dir))
          (error-response 404 :not-a-directory "no such directory" :path (.getAbsolutePath dir))
          (not (.canRead dir)) (error-response 403
                                               :directory-unreadable
                                               "that directory is not readable"
                                               :path (.getAbsolutePath dir))
          :else (let
                  [kids (or (.listFiles dir) (make-array java.io.File 0))
                   dirs (->> kids
                             (filter (fn [^java.io.File f]
                                       (and (.isDirectory f)
                                            (not (str/starts-with? (.getName f) ".")))))
                             (sort-by (fn [^java.io.File f]
                                        (str/lower-case (.getName f)))))]

                  (json-response {:path (.getAbsolutePath dir)
                                  :parent (some-> (.getParentFile dir)
                                                  .getAbsolutePath)
                                  :home (fs-home)
                                  :is-truncated (boolean (> (count dirs) (long fs-entry-limit)))
                                  :entries (mapv fs-entry (take fs-entry-limit dirs))})))))

(defn- create-directory-handler
  "POST /v1/fs/actions/mkdir {path, name} — one folder inside `path`, so a project
   can start somewhere that does not exist yet. One SEGMENT only: a picker that
   silently accepts `a/b/../..` is a picker that writes outside what it showed."
  [request]
  (let
    [{:strs [path name]}
     (body-json request)

     ^java.io.File parent
     (io/file (expand-user path))

     folder
     (str/trim (str name))]

    (cond (str/blank? folder)
          (error-response 400 :invalid-request "name must be a non-blank string")
          (or (re-find #"[/\\]" folder) (contains? #{"." ".."} folder))
          (error-response 400 :invalid-request "name must be a single folder name")
          (not (.isDirectory parent))
          (error-response 404 :not-a-directory "no such directory" :path (.getAbsolutePath parent))
          :else (let [^java.io.File made (io/file parent folder)]
                  (if (or (.isDirectory made) (.mkdir made))
                    (json-response 201 (fs-entry made))
                    (error-response 400
                                    :mkdir-failed "could not create that folder"
                                    :path (.getAbsolutePath made)))))))

(defn- list-projects-handler
  "GET /v1/projects[?owner=…&archived=true] — the owner's projects (projects
   are CROSS-CHANNEL), each with a live session_count."
  [request]
  (let
    [owner
     (not-empty (get-in request [:query-params "owner"]))

     archived?
     (= "true" (get-in request [:query-params "archived"]))]

    (json-response {:projects (state/list-projects (cond-> {:include-archived? archived?}
                                                     owner
                                                     (assoc :owner-id owner)))})))

(defn- create-project-handler
  "POST /v1/projects {name, color?, owner_id?, root?} — create a (cross-channel) project."
  [request]
  (let [{:strs [name color owner_id root]} (body-json request)]
    (if (str/blank? (str name))
      (error-response 400 :invalid-request "name must be a non-blank string")
      (json-response 201
                     (state/create-project! (cond-> {:name name}
                                              color
                                              (assoc :color color)

                                              (not (str/blank? (str root)))
                                              (assoc :workspace-root root)

                                              owner_id
                                              (assoc :owner-id owner_id)))))))

(defn- ensure-project-for-root-handler
  "POST /v1/projects/actions/ensure {root, name?, owner_id?} — get-or-create the project
   bound to a canonical workspace root. A project IS a TUI tab set; this is the
   launch-dir -> project resolution. Idempotent (safe under concurrent TUIs)."
  [request]
  (let [{:strs [root name owner_id]} (body-json request)]
    (if (str/blank? (str root))
      (error-response 400 :invalid-request "root must be a non-blank string")
      (json-response
        (state/ensure-project-for-root! (or (not-empty owner_id) "local") root name)))))

(defn- get-project-handler
  [request]
  (let [pid-str (get-in request [:path-params :pid])]
    (if-let
      [p (some-> (path-pid request)
                 state/get-project)]
      (json-response p)
      (project-404 pid-str))))

(defn- patch-project-handler
  "PATCH /v1/projects/:pid {name?, color?, position?, archived?} — patch a project."
  [request]
  (let
    [pid-str
     (get-in request [:path-params :pid])

     pid
     (path-pid request)

     body
     (body-json request)

     opts
     (cond-> {}
       (contains? body "name")
       (assoc :name (get body "name"))

       (contains? body "color")
       (assoc :color (get body "color"))

       (contains? body "position")
       (assoc :position (get body "position"))

       (contains? body "archived")
       (assoc :archived? (boolean (get body "archived"))))]

    (cond (not pid) (project-404 pid-str)
          (and (contains? opts :name) (str/blank? (str (:name opts))))
          (error-response 400 :invalid-request "name must be a non-blank string")
          (empty? opts) (error-response 400 :invalid-request "no project fields to update")
          :else (if-let [p (state/update-project! pid opts)]
                  (json-response p)
                  (project-404 pid-str)))))

(defn- delete-project-handler
  "DELETE /v1/projects/:pid[?is_recursive=true] — by default member sessions
   scatter back to project-less (204, body-less).

   `is_recursive=true` DELETES every member session (and its draft clones) before
   dropping the project row, and answers 200 with `{project_id,
   deleted_session_ids, session_count}`: a client needs those ids to prune local
   state (rows, snapshots, unsent drafts) without racing a re-read."
  [request]
  (let
    [pid-str
     (get-in request [:path-params :pid])

     pid
     (path-pid request)]

    (if (= "true" (get-in request [:query-params "is_recursive"]))
      (if pid
        (json-response (state/delete-project! pid {:is-recursive true}))
        (project-404 pid-str))
      (do (some-> pid
                  state/delete-project!)
          {:status 204 :headers {} :body nil}))))

(defn- reorder-project-sessions-handler
  "PATCH /v1/projects/:pid/sessions {order:[sid…]} — persist the manual order of
   the sessions (TUI tabs) inside a project so they stay MOVABLE cross-channel.
   LOOSE sessions named in `order` are ADOPTED into the project atomically; guests
   owned by another project are never stolen."
  [request]
  (let
    [pid-str
     (get-in request [:path-params :pid])

     pid
     (path-pid request)

     order
     (->> (get (body-json request) "order")
          (keep #(some-> %
                         str
                         parse-uuid))
          vec)]

    (cond (not pid) (project-404 pid-str)
          (empty? order)
          (error-response 400 :invalid-request "order must be a non-empty array of session ids")
          :else (let [count (state/reorder-project-sessions! pid order)]
                  (json-response {:project_id (str pid) :count count})))))

(defn- configured-reasoning-level
  "The shared `reasoning_level` toggle as a plain wire string (`quick` /
   `balanced` / `deep`), or nil when it is unreadable.

   The gateway is the ONLY reasoning source for channels that do not send
   `reasoning_default` themselves (the companion app, plain HTTP clients): the
   TUI reads the same toggle and passes it per turn, so honouring it here makes
   one flip mean the same thing everywhere instead of silently falling back to
   the engine's `balanced`."
  []
  (let [v (try (toggles/value-of "reasoning_level") (catch Throwable _ nil))]
    (cond (keyword? v) (name v)
          (string? v) (not-empty v)
          :else nil)))

(defn- submit-turn-handler
  [request]
  (let
    [sid
     (path-sid request)

     body
     (body-json request)]

    (if (nil? sid)
      (session-404 (get-in request [:path-params :sid]))
      (let
        [result (state/submit-turn! sid
                                    {:request (get body "request")
                                     :idempotency-key (get body "idempotency_key")
                                     :model (get body "model")
                                     :reasoning-default (or (get body "reasoning_default")
                                                            (configured-reasoning-level))
                                     :extra-body (get body "extra_body")
                                     :turn-features (get body "turn_features")
                                     :workspace (get body "workspace")
                                     :attachments (get body "attachments")
                                     ;; The submitter's own pre-expansion prose. Dropping it here
                                     ;; is what made a queued image render as a raw /var/folders path.
                                     :display-request (get body "display_request")})]
        (cond (:turn result) (json-response (if (:idempotent? result) 200 202) (:turn result))
              (= :turn-in-progress (:error result))
              (error-response 409
                              :turn-in-progress "session already has a running turn"
                              :session_id (str sid)
                              :turn_id (:turn-id result))
              (= :session-not-found (:error result)) (session-404 (str sid))
              :else
              (error-response 400 :invalid-request (or (:message result) "invalid request")))))))

(defn- list-turns-handler
  "GET the session's turns. `?status=queued` narrows the response to the live
   queued backlog — the tray's poll — which is served straight from the
   registry overlay with no turn-history hydration. Without the filter this
   ships the FULL history (every completed turn's content), so a poller that
   only wants the backlog must pass it."
  [request]
  (let
    [sid
     (path-sid request)

     queued-only?
     (= "queued" (get-in request [:query-params "status"]))]

    (if (and sid (state/soul sid))
      (json-response {:turns
                      (if queued-only? (state/list-queued-turns sid) (state/list-turns sid))})
      (session-404 (get-in request [:path-params :sid])))))

(defn- get-turn-handler
  [request]
  (let
    [sid
     (path-sid request)

     tid
     (path-tid request)]

    (if-let [turn (and sid (state/get-turn sid tid))]
      (json-response turn)
      (error-response 404 :turn-not-found "unknown turn" :turn_id tid))))

(defn- update-queued-turn-handler
  [request]
  (let
    [sid
     (path-sid request)

     tid
     (path-tid request)

     result
     (if sid
       (state/update-queued-turn! sid tid (get (body-json request) "request"))
       {:error :turn-not-found})]

    (cond (:turn result) (json-response (:turn result))
          (= :turn-not-found (:error result))
          (error-response 404 :turn-not-found "unknown turn" :turn_id tid)
          :else (error-response 409
                                (or (:error result) :not-queued)
                                (or (:message result) "turn is not queued")
                                :turn_id tid
                                :turn_status (:status result)))))

(defn- delete-queued-turn-handler
  [request]
  (let
    [sid
     (path-sid request)

     tid
     (path-tid request)

     result
     (if sid (state/delete-queued-turn! sid tid) {:error :turn-not-found})]

    (cond (= "deleted" (:status result)) (json-response 200 result)
          (= :turn-not-found (:error result))
          (error-response 404 :turn-not-found "unknown turn" :turn_id tid)
          :else (error-response 409
                                (or (:error result) :not-queued)
                                "turn is not queued"
                                :turn_id tid
                                :turn_status (:status result)))))

(defn- cancel-turn-handler
  [request]
  (let
    [sid
     (path-sid request)

     tid
     (path-tid request)

     result
     (if sid (state/cancel-turn! sid tid) {:error :turn-not-found})]

    (cond (:status result) (json-response 202 result)
          (= :turn-not-found (:error result))
          (error-response 404 :turn-not-found "unknown turn" :turn_id tid)
          :else (error-response 409
                                :not-running "turn is not running"
                                :turn_id tid
                                :turn_status (:status result)))))

(defn- cancel-current-turn-handler
  "POST /sessions/:sid/cancel-current {idempotency_key} — tid-less cancel: fire
   the cancel token of the turn holding the session's `:current-turn`, iff the
   caller submitted it under `idempotency_key`. For clients that lost the turn id
   (Esc before `turn.started` bound it, or a cancel self-heal that dropped it) —
   the id-addressed `/turns/:tid/cancel` is useless to them and the still-running
   ghost queues every next submit. A session is shared, so the correlation id is
   what keeps this route from killing another channel's work. 202 + `{:status
   \"cancelling\" :turn_id tid}`, 409 `:not-owner` for someone else's turn, 409
   `:no-running-turn` when idle."
  [request]
  (let
    [sid
     (path-sid request)

     owner-key
     (get (try (body-json request) (catch Throwable _ nil)) "idempotency_key")]

    (if (and sid (state/soul sid))
      (let [result (state/cancel-current-turn! sid owner-key)]
        (cond (:status result) (json-response 202 result)
              (= :no-running-turn (:error result))
              (error-response 409 :no-running-turn "session has no running turn")
              (= :not-owner (:error result))
              (error-response 409
                              :not-owner "the running turn was submitted by another client"
                              :turn_id (:turn_id result))
              (= :turn-not-found (:error result))
              (error-response 404 :turn-not-found "unknown turn")
              :else (error-response 409
                                    :not-running "turn is not running"
                                    :turn_status (:status result))))
      (session-404 (get-in request [:path-params :sid])))))

(defn- drain-idle-handler
  "POST /sessions/:sid/drain-queue — start the session's oldest queued turn iff
   it is idle. Returns `{:turn <started>|nil}`; nil turn means nothing was
   queued or a turn is already running (both benign)."
  [request]
  (let [sid (path-sid request)]
    (if (and sid (state/soul sid))
      (json-response {:turn (state/drain-idle! sid)})
      (session-404 (get-in request [:path-params :sid])))))

(defn- resume-queue-handler
  "POST /sessions/:sid/resume-queue — clear a queue PAUSED by a provider failure
   and start its head. An explicit resume also resets the failure breaker.
   Returns `{:turn <started>|nil}`; nil turn means the queue was not paused."
  [request]
  (let [sid (path-sid request)]
    (if (and sid (state/soul sid))
      (json-response {:turn (state/resume-queue! sid {:auto? false})})
      (session-404 (get-in request [:path-params :sid])))))

(defn- context-handler
  [request]
  (if-let
    [snapshot (some-> (path-sid request)
                      state/context-snapshot)]
    (json-response snapshot)
    (session-404 (get-in request [:path-params :sid]))))

(defn- transcript-handler
  "Transcript rows for a session, optionally WINDOWED: `?limit=` (window size,
  defaulting to the NEWEST rows) and `?offset=` (0-based start in the
  oldest-first list). Without them the whole transcript is returned, so an older
  client is unaffected. The window is sliced BEFORE hydration, so a page costs
  page-sized work instead of session-sized work, and it is ALSO capped in bytes
  (turn count does not bound bytes: one real session's newest 24 turns encode to
  9.5 MB). The reply's `offset` can therefore be HIGHER than the one asked for —
  page from the RETURNED offset, never from your own arithmetic.

  A window param that is PRESENT but unparsable is a 400, never a silent
  fallback: falling back would answer garbage with the whole (40 MB on a big
  session) transcript — the exact cost this endpoint exists to avoid. In-range
  policy is `state/transcript-page`'s: it clamps, so `?limit=0` honestly means
  zero rows."
  [request]
  (if-let [sid (path-sid request)]
    (let
      [given? (fn [k]
                (some? (get-in request [:query-params k])))
       limit (query-long request "limit")
       offset (query-long request "offset")]

      (if (or (and (given? "limit") (nil? limit)) (and (given? "offset") (nil? offset)))
        (error-response 400 :invalid-window "limit and offset must be integers")
        (let [page (state/transcript-page sid {:limit limit :offset offset})]
          (json-response {:turns (:turns page)
                          :total (:total page)
                          :offset (:offset page)
                          :has-more (:has-more page)}))))
    (session-404 (get-in request [:path-params :sid]))))

(defn- session-artifacts-handler
  "GET /v1/sessions/:sid/artifacts — `{\"artifacts\": [descriptor, …]}` for the
   WHOLE session, oldest turn first, metadata only. A gallery asks this once
   instead of deriving itself from the transcript page it happens to hold, which
   listed only what the reader had already scrolled back to."
  [request]
  (if-let [sid (path-sid request)]
    (json-response {:artifacts (state/session-artifacts sid)})
    (session-404 (get-in request [:path-params :sid]))))

(defn- transcript-md-handler
  "Render a session's user/assistant dialog as Markdown — the canonical
   `transcript->md :dialog` every surface (CLI, web, file export) renders
   through — served as text so a channel can DISPLAY it without re-implementing
   transcript rendering client-side. `transcript` is resolved dynamically to
   avoid a load-time require cycle (core -> gateway.server -> transcript ->
   core)."
  [request]
  (if-let [sid (path-sid request)]
    {:status 200
     :headers {"Content-Type" "text/markdown; charset=utf-8"}
     :body (str ((requiring-resolve
                   'com.blockether.vis.internal.foundation.transcript/transcript-md)
                  (lp/db-info)
                  sid
                  {:mode :dialog}))}
    (session-404 (get-in request [:path-params :sid]))))

(defn- transcript-html-handler
  "Render a session's transcript as a STANDALONE HTML document — the canonical
   `transcript->html` every surface (CLI, web, file export) renders through, the
   HTML sibling of `transcript-md-handler`. `:dialog` when `?mode=dialog`, else
   the full forensic report. `transcript` is resolved dynamically to avoid a
   load-time require cycle (core -> gateway.server -> transcript -> core)."
  [request]
  (if-let [sid (path-sid request)]
    (let [mode (if (= "dialog" (get-in request [:query-params "mode"])) :dialog :full)]
      {:status 200
       :headers {"Content-Type" "text/html; charset=utf-8"}
       :body (str ((requiring-resolve
                     'com.blockether.vis.internal.foundation.transcript/transcript-html)
                    (lp/db-info)
                    sid
                    {:mode mode}))})
    (session-404 (get-in request [:path-params :sid]))))

(defn- turn-trace-handler
  [request]
  (if (path-sid request)
    (json-response {:iterations (state/turn-trace (get-in request [:path-params :tid]))})
    (session-404 (get-in request [:path-params :sid]))))

(defn- req-rid
  "Resource id from the request. It rides as the `rid` QUERY PARAM (not a path
   segment) because resource ids can embed absolute paths (e.g. an nREPL id
   `nrepl:/Users/.../ws`); an encoded `/` in a path segment trips Jetty's
   \"Ambiguous URI path separator\" 400."
  [request]
  (get-in request [:query-params "rid"]))

(defn- resources-handler
  "GET /v1/sessions/:sid/resources — the session's live vis-managed resources
   (background `shell` children, managed REPLs, MCP connections, …) FROM THE DAEMON's
   registry. An in-process client reads its own registry directly because
   it runs INSIDE the daemon, but the TUI and remote clients run in a DIFFERENT
   process from the one the agent's tools execute in; without this endpoint they
   read an empty local registry and never learn a background started."
  [request]
  (if-let [sid (path-sid request)]
    (json-response {:resources (resources/list-resources sid)})
    (session-404 (get-in request [:path-params :sid]))))

(defn- resource-stop-handler
  "POST /v1/sessions/:sid/resources/stop?rid=… — run the resource's stop-fn in
   the daemon (the single canonical stop path) and unregister it."
  [request]
  (if-let [sid (path-sid request)]
    (json-response (resources/stop! sid (req-rid request)))
    (session-404 (get-in request [:path-params :sid]))))

(defn- resource-logs-handler
  "GET /v1/sessions/:sid/resources/logs?rid=… — captured output lines for a
   background via its logs-fn (nil when the resource has none)."
  [request]
  (if-let [sid (path-sid request)]
    (json-response {:lines (resources/logs sid (req-rid request))})
    (session-404 (get-in request [:path-params :sid]))))

(defn- path-iid [request] (get-in request [:path-params :iid]))

(defn- path-idx
  [request]
  (some-> (get-in request [:path-params :idx])
          parse-long))

(defn- attachment-bytes-handler
  "GET /v1/sessions/:sid/iterations/:iid/attachments/:idx — the raw bytes of ONE
   outbound artifact (a matplotlib figure / produced image) a tool call emitted
   in iteration `:iid`, addressed by its 0-based `:idx` in the iteration's ordered
   attachment list — the SAME list (and order) the live `iteration.completed`
   descriptors index. Served with the artifact's own Content-Type so a native
   client (iOS/RN) `<img>`-loads it directly; the lazy fetch that keeps the live
   SSE frame lean. HISTORY and LIVE resolve through the SAME durable DB rows, so
   it works the instant the iteration is stored and forever after (404 until the
   row lands — the client retries, as the trace re-fetch already does). An
   attachment is append-only + content-addressed by (iteration, index), so it is
   safely `immutable`-cacheable."
  [request]
  (if (path-sid request)
    (let
      [idx
       (path-idx request)

       atts
       (state/user-iteration-attachments (path-iid request))

       att
       (when (and idx (nat-int? idx)) (nth atts idx nil))

       ^bytes bs
       (some-> att
               state/attachment-bytes)]

      (if bs
        {:status 200
         :headers {"Content-Type" (or (not-empty (str (:media-type att)))
                                      "application/octet-stream")
                   "Content-Length" (str (alength bs))
                   "Cache-Control" "private, max-age=31536000, immutable"}
         :body (java.io.ByteArrayInputStream. bs)}
        (error-response 404
                        :attachment-not-found "unknown attachment"
                        :iteration_id (str (path-iid request))
                        :index idx)))
    (session-404 (get-in request [:path-params :sid]))))

(defn- append-attachment-handler
  "POST /v1/sessions/:sid/iterations/:iid/attachments — a HUMAN's revision of an
   artifact the model produced, stored into the iteration that produced it:
   `{\"filename\": \"notes.md\", \"media_type\": \"text/markdown\",
   \"base64\": \"…\"}`.

   The filename is the identity, so re-sending the name is the NEXT VERSION of
   that artifact and not a second file beside it — the annotated note the
   companion saves is `v2` of the note it was reading. Answers with the same
   descriptor shape the transcript and the byte endpoint already speak, so the
   client re-reads the revision through the paths it already has."
  [request]
  (if (path-sid request)
    (let
      [body
       (body-json request)

       filename
       (some-> (get body "filename")
               str
               str/trim)

       base64
       (get body "base64")]

      (if (or (str/blank? filename) (str/blank? (str base64)))
        (error-response 400 :invalid-attachment "filename and base64 are required")
        (if-let
          [descriptor (state/append-iteration-attachment!
                        (path-iid request)
                        {:filename filename
                         :media-type (or (not-empty (str (get body "media_type")))
                                         "application/octet-stream")
                         :base64 (str base64)
                         :kind "doc"
                         :audience "user"})]
          (json-response 201 descriptor)
          (error-response 404
                          :attachment-not-stored "unknown iteration"
                          :iteration_id (str (path-iid request))))))
    (session-404 (get-in request [:path-params :sid]))))

(defn- turn-attachments-handler
  "GET /v1/sessions/:sid/turns/:tid/attachments — the inline images a USER sent
   with one turn: `{\"attachments\": [{filename, media_type, base64}, …]}`.

   The live rail deliberately ships byte-free chips, and a turn's persisted row
   only exists once it lands, so before this endpoint the only copy of a
   still-running turn's pictures was the sending client's own memory — an app
   restart, or a second device, painted the message with its images missing.
   The gateway has held them the whole time (registry entry while in flight,
   attachment store afterwards); this hands them back on demand, which is why it
   is a SEPARATE endpoint and not a fatter turn row."
  [request]
  (if-let [sid (path-sid request)]
    (json-response {:attachments (vec (state/turn-attachments sid (path-tid request)))})
    (session-404 (get-in request [:path-params :sid]))))

(defn- session-model-handler
  [request]
  (if-let [sid (path-sid request)]
    (json-response {:model (state/session-model-cached sid)})
    (session-404 (get-in request [:path-params :sid]))))

(defn- set-session-model-handler
  "PATCH /v1/sessions/:sid/model {provider, model} — pin the session to a
   provider+model from THIS gateway's fleet (blank/omitted clears the pin).

   The provider is validated against the PICKER fleet because the gateway OWNS
   the fleet: a client that pins an id this gateway does not serve would silently
   degrade to the default route on every turn while every picker/footer kept
   rendering the phantom pick. Unknown id -> 400 `unknown-provider`.

   `picker-fleet` — not `configured-providers` — is the exact set the TUI picker
   and the companion's `/v1/router` dialog OFFER: it also carries providers that
   are AUTHENTICATED but not yet persisted into `:providers`, and the engine
   routes those too (the router build appends them). Validating against config
   alone answered 400 for a provider the user had just picked from the list.

   The MODEL name is deliberately NOT restricted to the configured names — the
   live catalog (`/v1/providers/:id/models`, the TUI's \"Show all models\")
   legitimately offers models that are not pinned in vis.yml."
  [request]
  (if-let [sid (path-sid request)]
    (let
      [{:strs [provider model]} (body-json request)
       pid (some-> provider
                   str
                   str/trim
                   not-empty)
       known (into #{} (map (comp name :id)) (providers/picker-fleet))]

      (if (and pid (not (contains? known pid)))
        (error-response 400
                        :unknown-provider (str "provider " pid " is not configured on this gateway")
                        :provider_id pid)
        (do (state/set-session-model! sid pid model)
            (json-response {:model (state/session-model sid)}))))
    (session-404 (get-in request [:path-params :sid]))))

(defn- usage-handler
  "GET the whole-session usage rollup (turns, iterations, tool calls, folds,
   token split, cache hit rate, cost). ON-DEMAND only: it decodes every
   iteration's tool-call BLOB to count tools, so it is never folded into
   `list-sessions`. `{\"usage\" nil}` for a session with no turns yet."
  [request]
  (if-let [sid (path-sid request)]
    (json-response {:usage (state/session-usage-info sid)})
    (session-404 (get-in request [:path-params :sid]))))

(defn- workspace-handler
  [request]
  (if-let [sid (path-sid request)]
    (json-response {:workspace (state/session-workspace-info sid)})
    (session-404 (get-in request [:path-params :sid]))))

(defn- change-root-handler
  [request]
  (if-let [sid (path-sid request)]
    (let [{:strs [path]} (body-json request)]
      (json-response {:workspace (state/change-root! sid path)}))
    (session-404 (get-in request [:path-params :sid]))))

(defn- drafts-handler
  [request]
  (if-let [sid (path-sid request)]
    (json-response {:drafts (state/list-drafts sid)})
    (session-404 (get-in request [:path-params :sid]))))

(defn- stash-draft-handler
  [request]
  (if-let [sid (path-sid request)]
    (json-response {:workspace (state/stash-draft! sid)})
    (session-404 (get-in request [:path-params :sid]))))

(defn- resume-draft-handler
  [request]
  (if-let [sid (path-sid request)]
    (let [{:strs [workspace_id]} (body-json request)]
      (try (json-response {:workspace (state/resume-draft! sid workspace_id)})
           (catch clojure.lang.ExceptionInfo e
             (error-response 409 (:type (ex-data e) :draft-resume-failed) (ex-message e)))))
    (session-404 (get-in request [:path-params :sid]))))

(defn- create-draft-handler
  [request]
  (if-let [sid (path-sid request)]
    (let [{:strs [label clean]} (body-json request)]
      (try (json-response {:workspace (state/create-draft! sid label clean)})
           (catch clojure.lang.ExceptionInfo e
             (error-response 409 (:type (ex-data e) :draft-create-failed) (ex-message e)))))
    (session-404 (get-in request [:path-params :sid]))))

(defn- abandon-draft-handler
  [request]
  (if-let [sid (path-sid request)]
    (let
      [workspace-id (get-in request [:path-params :workspace-id])
       {reason "reason"} (body-json request)]

      (try (json-response {:workspace (state/abandon-draft! sid workspace-id reason)})
           (catch clojure.lang.ExceptionInfo e
             (error-response 409 (:type (ex-data e) :draft-abandon-failed) (ex-message e)))))
    (session-404 (get-in request [:path-params :sid]))))

(defn- seq-handler
  [request]
  (if-let [sid (path-sid request)]
    (json-response {:seq (state/current-seq sid)})
    (session-404 (get-in request [:path-params :sid]))))

(defn- events-since-handler
  [request]
  (if-let [sid (path-sid request)]
    (json-response {:events (state/events-since sid (sse-cursor request))})
    (session-404 (get-in request [:path-params :sid]))))

;; =============================================================================
;; Voice — canonical transcription through the LOCAL Parakeet model
;; (vis-foundation-voice / sherpa-onnx, soft-resolved so a build without the
;; extension answers 501 instead of failing to load). Lives on the GATEWAY so
;; every client — web, iOS, TUI — hits the SAME canonical /v1 route.
;; =============================================================================

(defn- requested-engine-id
  "`?engine=whisper-server` names the engine for THIS request; absent means the
   gateway's default. Naming an engine is how a client picks between several
   registered transcribers without any of them being special."
  [request]
  (some-> (get-in request [:query-params "engine"])
          str/trim
          not-empty
          keyword))

(defn- with-voice-engine
  "Resolve the session and the engine, or answer the ONE refusal that fits:
   404 unknown session, 400 an engine id nobody registered, 501 no transcription
   engine at all (a build without any voice extension)."
  [request f]
  (let
    [sid
     (path-sid request)

     id
     (requested-engine-id request)]

    (cond (not (and sid (state/soul sid)))
          (json-response 404 {:status "unavailable" :error "unknown session"})
          :else (if-let [engine (try (voice/resolve-engine id) (catch Throwable _ nil))]
                  (f sid engine)
                  (if id
                    (json-response 400
                                   {:status "unavailable"
                                    :error (str "unknown voice engine: " (name id))
                                    :engines (mapv voice/public-engine (voice/engines))})
                    (json-response 501
                                   {:status "unavailable"
                                    :error "no voice transcription engine is registered"}))))))

(defn- voice-state->json
  [st]
  (cond-> {:status (name (:state st))}
    (:progress st)
    (assoc :progress (:progress st))

    ;; "downloading" covers both the transfer and the multi-minute unpack; the
    ;; phase lets a UI name what is actually happening instead of stalling on 99%.
    (:phase st)
    (assoc :phase (name (:phase st)))

    (:error st)
    (assoc :error (:error st))))

;; --- Native push devices (APNs) ---

(defn- device-wire
  "One registered device in wire shape — the raw token and the relay grant NEVER
   leave the gateway."
  [device]
  (push/public-device device))

(defn- list-devices-handler
  "GET /v1/devices — registered push devices (tokens masked) plus this gateway's
   APNs readiness, so a client can tell \"push impossible here\" apart from
   \"push possible, this device just isn't registered\"."
  [_]
  (json-response {:devices (push/list-devices) :push (push/status)}))

(defn- register-device-handler
  "POST /v1/devices — idempotently register one device for push.
   `{token?, grant?, platform?, environment?, client?, client_version?, label?,
   bundle_id?}`. Exactly one identifier is required: a raw APNs/FCM `token`
   this gateway pushes to with its OWN credentials, or a relay `grant` the
   device obtained from the push relay — the relay holds the signing key, so a
   gateway that is not the app's publisher can wake it without ever learning
   the device token. Re-registering refreshes instead of duplicating."
  [request]
  (let
    [body
     (body-json request)

     token
     (some-> (get body "token")
             str
             str/trim)

     grant
     (some-> (get body "grant")
             str
             str/trim)]

    (if (and (str/blank? token) (str/blank? grant))
      (error-response 400 :bad-request "token or grant is required")
      (if-let
        [device (push/register-device! {:token token
                                        :grant grant
                                        :platform (get body "platform")
                                        :environment (get body "environment")
                                        :client (get body "client")
                                        :client-version (get body "client_version")
                                        :label (get body "label")
                                        :bundle-id (get body "bundle_id")
                                        :relay-url (get body "relay_url")})]
        (json-response {:device (device-wire device) :push (push/status)})
        (error-response 400 :bad-request "unusable device token")))))

(defn- delete-device-handler
  "DELETE /v1/devices/:token — stop pushing to this device (logout, permission
   revoked, app uninstalled)."
  [request]
  (let [token (str (get-in request [:path-params :token]))]
    (json-response {:is_removed (push/unregister-device! token)})))

(defn- test-device-handler
  "POST /v1/devices/actions/test — send one test alert to every registered device
   and report APNs' per-device verdict. The ONLY way to prove the whole chain
   (key, topic, environment, token) without waiting for a real turn."
  [_]
  (if-not (push/any-configured?)
    (error-response 503
                    :push-unavailable "push is not configured on this gateway"
                    :push (push/status))
    (json-response {:results (push/broadcast! {:title "Vis"
                                               :body "Push notifications are working."
                                               :data {:type "test"}})
                    :push (push/status)})))

;; --- Human-input requests (a run BLOCKED on the operator) ---

(defn- human-input-404
  [request-id]
  (error-response 404
                  :human-input-not-found "no such pending human-input request"
                  :request_id (str request-id)))

(defn- list-human-input-handler
  "GET /v1/sessions/:sid/human-input — the typed input requests this session is
   BLOCKED on right now.

   The live `human_input.request` event is the fast path; this is how a client
   that connected LATER (cold start, background, reinstall) still finds the
   open form instead of watching a turn that never moves."
  [request]
  (if-let [sid (path-sid request)]
    (json-response {:requests (gw-human-input/pending sid)})
    (session-404 (get-in request [:path-params :sid]))))

(defn- submit-human-input-handler
  "POST /v1/sessions/:sid/human-input/:request-id/actions/submit — answer one
   pending request with `{values: {field_id: value}}`.

   Validation stays in the engine, so the app and the TUI accept exactly the
   same answers: a rejected one comes back `{is_accepted false, errors {…}}`
   and the request STAYS pending so the operator can fix it."
  [request]
  (let
    [sid
     (path-sid request)

     request-id
     (str (get-in request [:path-params :request-id]))

     values
     (get (body-json request) "values")]

    (cond (nil? sid) (session-404 (get-in request [:path-params :sid]))
          (nil? (gw-human-input/request-of sid request-id)) (human-input-404 request-id)
          (not (map? values)) (error-response 400 :bad-request "values must be an object")
          :else (let [outcome (gw-human-input/submit! request-id values)]
                  (json-response
                    (cond-> {:is_accepted (boolean (:is-accepted outcome)) :request_id request-id}
                      (seq (:errors outcome))
                      (assoc :errors (:errors outcome))))))))

(defn- cancel-human-input-handler
  "POST /v1/sessions/:sid/human-input/:request-id/actions/cancel — dismiss one
   pending request. The blocked extension resumes with `is_submitted false`.
   A request declared `is_cancellable false` refuses, exactly as in the TUI."
  [request]
  (let
    [sid
     (path-sid request)

     request-id
     (str (get-in request [:path-params :request-id]))

     view
     (when sid (gw-human-input/request-of sid request-id))]

    (cond (nil? sid) (session-404 (get-in request [:path-params :sid]))
          (nil? view) (human-input-404 request-id)
          (false? (:is-cancellable view)) (error-response 409
                                                          :human-input-not-cancellable
                                                          "this request cannot be cancelled"
                                                          :request_id request-id)
          :else (json-response {:is_cancelled (boolean (gw-human-input/cancel! request-id))
                                :request_id request-id}))))

(defn- reachable-addresses
  "Every base URL this gateway answers on, most durable first (Tailscale before
   LAN — see [[pairing/candidate-hosts]]).

   A pairing QR carries the same list, but only ONCE: a phone paired at home
   picks the LAN address, keeps it, and is stranded the moment it leaves the
   house. Advertising the addresses on a live, token-gated endpoint lets an
   already-paired client discover the tailnet address and move itself, with no
   second QR. The port/scheme come from the request when the bind is unknown, so
   a tunnel sees itself correctly."
  [request]
  (let
    [{:keys [host port]}
     @server-state

     port
     (or port (:server-port request) 7890)

     scheme
     (name (or (:scheme request) :http))]

    (->> (pairing/candidate-hosts (or host "0.0.0.0"))
         (remove str/blank?)
         (map #(str scheme "://" % ":" port))
         distinct
         vec)))

(defn- capabilities-handler
  "GET /v1/capabilities — stable feature negotiation for remote/native clients.
   Availability describes what THIS gateway can accept; device-side permissions
   remain the client's responsibility. Voice reports the SELECTED engine, every
   engine that is registered, the phase vocabulary a client may be shown, and the
   selected engine's readiness — without starting any download. `protocol` carries
   the VERSION contract ([[protocol/handshake]]) and `compatibility` this
   gateway's verdict on the CALLER, so one request answers both \"what can you
   do\" and \"can we talk\"."
  [request]
  (let
    [engine
     (try (voice/default-engine) (catch Throwable _ nil))

     voice-caps
     (merge {:enabled (boolean engine)
             :transport "audio/wav"
             :transcription "gateway-local"
             ;; the POST returns a JOB, not a transcript: a client that sees this
             ;; STREAMS the job's own progress (`/voice/jobs/:id/events`) instead of
             ;; holding a socket open for a minute or polling for a percentage. That
             ;; stream is NOT the session event log, so `:progress-event` NAMES every
             ;; frame on it — a client filters on a name it was told rather than
             ;; guessing a job from the payload's shape.
             :is-async true
             :progress "sse"
             :progress-event wire/voice-job-event
             :phases (mapv name voice/phases)
             :model
             (if engine (voice-state->json (voice/readiness engine)) {:status "unavailable"})}
            (voice/engines-info))]

    (json-response {:version 1
                    :protocol (protocol/handshake)
                    :addresses (reachable-addresses request)
                    :compatibility (protocol/gateway-verdict request)
                    :features {:chat {:enabled true}
                               :attachments {:enabled true
                                             :transport "inline-base64"
                                             :media-types ["image/jpeg" "image/png" "image/gif"
                                                           "image/webp" "image/bmp" "video/mp4"
                                                           "video/quicktime"]
                                             :video-media-types ["video/mp4" "video/quicktime"]
                                             :max-files attachments/max-image-count
                                             :max-file-bytes attachments/max-upload-image-bytes
                                             :max-video-bytes attachments/max-video-bytes}
                               :voice voice-caps
                               :push (push/status)}})))

(defn- wav-file?
  "RIFF/WAVE magic + minimum header length — the CHEAP pre-filter that turns an
   obviously-not-audio body into a clear 400 without waking the ASR. sherpa-onnx's
   native WaveReader ABORTS THE WHOLE JVM on malformed input, so the header is
   verified in JVM code before the native reader ever runs."
  [^java.io.File f]
  (and (>= (.length f) 44)
       (with-open [in (io/input-stream f)]
         (let [head (byte-array 12)]
           (and (= 12 (.read in head))
                (= "RIFF" (String. head 0 4 "US-ASCII"))
                (= "WAVE" (String. head 8 4 "US-ASCII")))))))

(defn- voice-model-handler
  "GET  /v1/sessions/:sid/voice/model — the selected engine's readiness (clients
        poll this before recording).
   POST /v1/sessions/:sid/voice/model — ask the engine to prepare itself (a local
        model starts downloading; idempotent, returns immediately).
   JSON: {:status \"ready|downloading|failed|absent|unavailable\" :progress 0..100?
   :error \"…\" :engine \"…\"}. An engine that needs no preparation is simply ready."
  [request]
  (with-voice-engine request
                     (fn [_sid engine]
                       (json-response 200
                                      (assoc (voice-state->json (if (= :post
                                                                       (:request-method request))
                                                                  (voice/prepare! engine)
                                                                  (voice/readiness engine)))
                                        :engine (name (:id engine)))))))

(defn- voice-handler
  "POST /v1/sessions/:sid/voice — body is a recorded WAV blob. ACCEPTS the audio
   and answers **202 with a job**; transcription runs on its own thread and the
   client STREAMS `/voice/jobs/:job-id/events` (SSE) to watch it.

   That split is the whole point: the upload is the CLIENT's progress (bytes in
   flight), and 202 is the gateway saying \"I have your recording\" — from then on
   the phase and percentage come from the job, so a minute-long transcription is
   never an unexplained spinner and never a dead socket.

   The engine must be ready — the client drives preparation via /voice/model; a
   not-ready engine answers 425 (Too Early) with its state, NEVER blocking the
   request thread on a ~465MB download."
  [request]
  (with-voice-engine
    request
    (fn [_sid engine]
      (if-not (voice/ready? engine)
        (json-response 425 (voice-state->json (voice/readiness engine)))
        (let [tmp (java.io.File/createTempFile "vis-voice" ".wav")]
          (try (with-open
                 [in ^java.io.InputStream (:body request)
                  out (io/output-stream tmp)]

                 (io/copy in out))
               (if-not (wav-file? tmp)
                 (do (.delete tmp)
                     (json-response 400 {:error "body must be a RIFF/WAVE audio file"}))
                 ;; the temp file outlives this response on purpose: it is the
                 ;; job's input and is deleted by `:on-done`, whichever way the
                 ;; job ends.
                 (json-response 202
                                (voice/submit! {:audio-path (str tmp)
                                                :engine-id (:id engine)
                                                :on-done (fn [_job]
                                                           (.delete tmp))})))
               (catch Throwable t
                 (.delete tmp)
                 (tel/log! {:level :error :id ::voice-transcribe-failed :data {:error (str t)}})
                 (json-response 400 {:error (voice/error-message t)}))))))))

(defn- voice-job-handler
  "GET    /v1/sessions/:sid/voice/jobs/:job-id — where this transcription is:
          `{:id :engine :phase :progress :is_done :text? :error?}`. ONE read of
          the job resource; a client that wants to WATCH it streams the twin
          below instead of asking again and again.
   DELETE /v1/sessions/:sid/voice/jobs/:job-id — forget it once the transcript has
          been collected (finished jobs also expire on their own)."
  [request]
  (let
    [sid
     (path-sid request)

     job-id
     (get-in request [:path-params :job-id])]

    (cond (not (and sid (state/soul sid))) (json-response 404 {:error "unknown session"})
          (= :delete (:request-method request)) (do (voice/forget! job-id)
                                                    (json-response 200 {:is-forgotten true}))
          :else (if-let [job (voice/job job-id)]
                  (json-response 200 job)
                  (json-response 404 {:error "unknown transcription job"})))))

(def ^:private VOICE_JOB_QUEUE_CAP
  "Per-connection queue of job states. A transcription reports a handful of
   percentages per second at most, so anything the writer cannot keep up with is
   a dead socket, not backpressure worth buffering."
  64)

(defn- voice-job-events-body
  "Ring streamable body for ONE transcription job: its CURRENT state first, then
   a `voice.job` frame per change until the job is done or failed.

   The current state rides first on purpose — that is what makes a reconnect free
   of a poll: the stream is subscribed BEFORE the snapshot is read, so a job that
   finished in that instant is still reported, and a client that lost the socket
   re-opens and is told the terminal phase immediately.

   [[voice/watch!]] only enqueues; this thread is the connection's single socket
   writer, so the engine's thread is never blocked by a stalled reader."
  [job-id]
  (reify
    ring-protocols/StreamableResponseBody
      (write-body-to-stream [_ _ output-stream]
        (let
          [^OutputStream out
           output-stream

           queue
           (ArrayBlockingQueue. (int VOICE_JOB_QUEUE_CAP))

           unwatch
           (voice/watch! job-id
                         (fn [job]
                           (.offer queue job)))

           write!
           (fn [job]
             (.write out (.getBytes (wire/voice-job-sse-frame job) StandardCharsets/UTF_8))
             (.flush out))]

          (try (let [current (voice/job job-id)]
                 (when current (write! current))
                 (loop [done? (or (nil? current) (:is-done current))]
                   (when-not done?
                     (if-let [job (.poll queue (long HEARTBEAT_MS) TimeUnit/MILLISECONDS)]
                       (do (write! job) (recur (:is-done job)))
                       ;; heartbeat comment: keeps proxies from reaping a quiet
                       ;; upload-to-first-chunk gap, and detects a dead client.
                       (do (.write out (.getBytes ": ping\n\n" StandardCharsets/UTF_8))
                           (.flush out)
                           (recur false))))))
               (catch Throwable _ nil)
               (finally (unwatch) (try (.close out) (catch Throwable _ nil))))))))

(defn- voice-job-events-handler
  "GET /v1/sessions/:sid/voice/jobs/:job-id/events — the job's phase and
   percentage PUSHED as they happen, as `text/event-stream`.

   Progress that is polled is progress that arrives late and costs a request per
   tick; this is the same job resource, streamed. The stream ENDS itself on the
   terminal frame, so the client neither polls nor guesses when to stop reading."
  [request]
  (let
    [sid
     (path-sid request)

     job-id
     (get-in request [:path-params :job-id])]

    (cond (not (and sid (state/soul sid))) (json-response 404 {:error "unknown session"})
          (nil? (voice/job job-id)) (json-response 404 {:error "unknown transcription job"})
          :else {:status 200
                 :headers {"Content-Type" "text/event-stream"
                           "Cache-Control" "no-cache, no-transform"
                           "X-Accel-Buffering" "no"}
                 :body (voice-job-events-body job-id)})))

(defn- suggest-handler
  "GET /v1/sessions/:sid/suggest?kind=file&q= — the SHARED fuzzy suggestion
   service behind every composer sigil (the `@` file picker today). It is a
   pure query: given `q`, return the ranked index. The *trigger* smarts —
   when `@` means pick-a-file, `@@` escaping to a literal `@` — live in each
   client (web/TUI), NEVER here, so writing a literal `@` can never be
   endangered by the backend. Row shape is `{:name :size :age :status}`, the
   same rows the web + TUI pickers render."
  [request]
  (if-not (some-> (path-sid request)
                  state/soul)
    (session-404 (get-in request [:path-params :sid]))
    (let
      [kind
       (or (not-empty (get-in request [:query-params "kind"])) "file")

       q
       (str (get-in request [:query-params "q"]))]

      (case kind
        "file"
        (json-response (file-picker/suggest-file-rows q {:limit 20}))

        (error-response 400 :invalid-request (str "unknown suggest kind: " kind))))))

;; =============================================================================
;; Router + middleware
;; =============================================================================

;; =============================================================================
;; Route contributions — the whiteboard pattern (pull, not push)
;; =============================================================================
;;
;; The gateway core serves ONLY the JSON API. Anything else (the /ui web
;; companion, a future surface) is a contribution the gateway PULLS at
;; handler-build time — extensions never reach into the gateway, so there
;; is NO ordering requirement between starting the server and loading the
;; extension (OSGi calls this the whiteboard pattern; ServiceLoader and
;; Spring auto-configuration are the same pull move).
;;
;; Primary source — declarative, vis's own slot idiom: an extension puts
;;   {:ext/channel-contributions
;;    {:gateway.slot/http-routes [{:id :web/ui :fn (fn [] contribution)}]}}
;; on its extension map; the gateway enumerates the slot via
;; `extension/channel-contributions-for` whenever it (re)builds the
;; handler. A fingerprint check on each request notices contributions
;; that arrived AFTER the server started (extension loaded late, jar
;; dropped + `vis-agent extension reload`) and rebuilds — both orders just work.
;;
;; Secondary source — imperative escape hatch for embedded/REPL callers:
;; `register-routes!` below.
;;
;; Contribution shape (all keys but :routes optional):
;;   {:prefix            "/ui"        ; uri namespace this contribution owns
;;    :routes            (fn [token] reitit-route-data)
;;    :open-uris         #{"/ui" ...} ; reachable without auth
;;    :request-authed-fn (fn [request token] bool)   ; extra auth carrier
;;    :on-unauthorized   (fn [request] ring-response) ; custom 401 for :prefix
;;    :on-not-found      (fn [request] ring-response) ; custom 404 for :prefix
;;    :form-params?      true}        ; urlencoded form parsing under :prefix
(defonce ^:private route-contributions (atom {}))

(defonce ^:private imperative-version (atom 0))

(declare ^:private rebuild-app!)

(defn register-routes!
  "Imperative escape hatch: register (or replace, by `id`) a route
   contribution from an embedded/REPL caller. Extensions should prefer
   the declarative `:gateway.slot/http-routes` channel-contribution slot
   — the gateway pulls it with no registration call at all."
  [id contribution]
  (swap! route-contributions assoc id contribution)
  (swap! imperative-version inc)
  (rebuild-app!)
  id)

(defn deregister-routes!
  [id]
  (swap! route-contributions dissoc id)
  (swap! imperative-version inc)
  (rebuild-app!)
  nil)

(defn- declared-contributions
  "Whiteboard pull: resolve every registered extension's
   `:gateway.slot/http-routes` entries by calling each entry's 0-arg
   `:fn`. A throwing contribution is dropped, never fatal."
  []
  (keep (fn [{:keys [id] f :fn}]
          (try (f)
               (catch Throwable t
                 (tel/log! :error ["gateway: http-routes contribution threw" id (ex-message t)])
                 nil)))
        (extension/channel-contributions-for :gateway :gateway.slot/http-routes)))

(defn- contributions [] (concat (declared-contributions) (vals @route-contributions)))

(defn- routes-fingerprint
  "Cheap identity of the current contribution set: declared slot entry
   ids + each contribution's `:rev` (contributions stamp it with their
   namespace load time, so a REPL/watcher `:reload` that adds ROUTES
   remounts the table — handler vars are live, the route table is not)
   + the imperative registry version. Compared per request to mount
   late arrivals without restarting the server."
  []
  [(mapv (fn [{:keys [id] f :fn}]
           [id (try (:rev (f)) (catch Throwable _ nil))])
         (extension/channel-contributions-for :gateway :gateway.slot/http-routes))
   @imperative-version])

(defn auth-required?
  "True when this gateway instance demands the bearer token. OFF by
   default on a loopback bind (a localhost single-user daemon — the
   token dance is pure friction there); ALWAYS on for a non-loopback
   bind; `--require-token` forces it on loopback too."
  []
  (boolean (:require-token? @server-state)))

(defn- constant-time=?
  "Timing-safe comparison for secret strings. Plain `=` early-outs on the
   first differing byte, leaking token length/prefix through response timing
   once auth is enabled (non-loopback); `MessageDigest/isEqual` compares in
   constant time. nil-safe — a missing header never matches."
  [^String a ^String b]
  (boolean (and a
                b
                (MessageDigest/isEqual (.getBytes a StandardCharsets/UTF_8)
                                       (.getBytes b StandardCharsets/UTF_8)))))

(defn- wrap-auth
  "Token gate (§3). Skipped entirely when [[auth-required?]] is false
   (loopback default). When on: the API sends `Authorization: Bearer`;
   contributions may add carriers of the SAME secret (e.g. the web
   channel's HttpOnly cookie) via :request-authed-fn, declare
   :open-uris, and shape their own unauthorized response for uris under
   their :prefix. `contribs` is the realized contribution vector baked
   in at handler-build time."
  [handler ^String token contribs]
  (let [expected (str "Bearer " token)]
    (fn [request]
      (if-not (auth-required?)
        (handler request)
        (let
          [uri (str (:uri request))
           open? (or (= "/healthz" uri)
                     ;; The embedded docs site is public content (the vis.dev
                     ;; pages) — viewable on the tunnel without the token.
                     (= "/docs" uri)
                     (str/starts-with? uri "/docs/")
                     (some #(contains? (or (:open-uris %) #{}) uri) contribs))
           authed? (or (constant-time=? expected
                                        (some-> (get-in request [:headers "authorization"])
                                                str/trim))
                       ;; The internal same-machine client (TUI/CLI) carries the
                       ;; SAME secret in X-Vis-Gateway-Secret (read from the on-disk
                       ;; registry) — the header it already sends on the /healthz
                       ;; probe. Accept it so a token-gated gateway (any non-loopback
                       ;; bind like --host 0.0.0.0) doesn't 401 its own local clients.
                       (constant-time=? (str token)
                                        (some-> (get-in request [:headers "x-vis-gateway-secret"])
                                                str/trim))
                       (some (fn [{:keys [request-authed-fn]}]
                               (when request-authed-fn (request-authed-fn request token)))
                             contribs))]

          (if (or open? authed?)
            (handler request)
            (or (some (fn [{:keys [prefix on-unauthorized]}]
                        (when (and prefix on-unauthorized (str/starts-with? uri prefix))
                          (on-unauthorized request)))
                      contribs)
                (error-response 401 :unauthorized "missing or invalid bearer token"))))))))

(def ^:private protocol-open-uris
  "Paths that answer EVEN an unsupported client. They are HOW a peer learns the
   gateway's protocol and reads the mismatch verdict, so refusing them would
   leave an old client with nothing but an opaque failure."
  #{"/healthz" "/readyz" "/v1/capabilities"})

(defn- wrap-protocol
  "Wire-protocol gate (§3). A client whose advertised protocol is unsupported —
   including a client that advertises nothing — is refused once, up front, with
   426 and the same verdict + copy every surface renders, instead of being fed a
   shape it cannot read and failing later as a mystery 404 or missing field."
  [handler]
  (fn [request]
    (let [uri (str (:uri request))]
      (if (or (contains? protocol-open-uris uri) (str/starts-with? uri "/docs"))
        (handler request)
        (let [v (protocol/gateway-verdict request)]
          (if (:is-compatible v)
            (handler request)
            (let [{:keys [title summary remedy]} (protocol/explain v)]
              (json-response 426
                             {:error {:type "incompatible_protocol"
                                      :message (str title " — " summary)
                                      :title title
                                      :remedy remedy}
                              :protocol (protocol/handshake)
                              :compatibility v}))))))))

(defn- wrap-errors
  [handler]
  (fn [request]
    (try (handler request)
         (catch Throwable t
           (tel/log! :error ["gateway: unhandled request error" (:uri request) (ex-message t)])
           (error-response 500 :engine-error (or (ex-message t) "internal error"))))))

(def ^:private cors-allow-methods "GET, POST, PATCH, DELETE, OPTIONS")

(defn- cors-headers
  "CORS headers for a cross-origin browser request. The bearer token is the
   real authorization gate (§3); CORS only tells the browser the response is
   readable. We echo the request Origin — so `Access-Control-Allow-Credentials`
   is legal for cookie-bearing browser clients — and fall back to `*` for callers
   that send no Origin (curl, native clients)."
  [request]
  (let
    [origin
     (get-in request [:headers "origin"])

     req-headers
     (get-in request [:headers "access-control-request-headers"])]

    (cond->
      {"Access-Control-Allow-Methods" cors-allow-methods
       "Access-Control-Allow-Headers" (or req-headers
                                          "Authorization, Content-Type, X-Vis-Gateway-Secret")
       ;; Without this a browser fetch can read the BODY but not the `ETag`
       ;; header, so the app could never send `If-None-Match` and every session
       ;; list poll would re-download the whole fleet. The session-list ordering
       ;; digest is read the same way, and on 304s where there is no body at all.
       "Access-Control-Expose-Headers" (str "ETag, " sessions-order-header)
       "Access-Control-Max-Age" "600"
       "Vary" "Origin"}
      origin
      (assoc "Access-Control-Allow-Origin"
        origin "Access-Control-Allow-Credentials"
        "true")

      (not origin)
      (assoc "Access-Control-Allow-Origin" "*"))))

(defn- wrap-cors
  "Outermost middleware. A cross-origin browser client (the Companion web/mobile
   app hitting a Tailscale/cloudflared gateway URL) sends a CORS preflight
   `OPTIONS` carrying NO Authorization header; answer it here with 204 + CORS
   headers BEFORE [[wrap-auth]] can 401 it, and stamp the same headers on every
   real response so the browser may read it. CORS is not auth — the bearer token
   remains the sole authorization check."
  [handler]
  (fn [request]
    (if (= :options (:request-method request))
      {:status 204 :headers (cors-headers request) :body ""}
      (let [response (handler request)]
        (some-> response
                (update :headers merge (cors-headers request)))))))

(defn- router
  [^String token contribs]
  (rr/router
    (into
      [["/healthz" {:get health-handler}] ["/readyz" {:get health-handler}]
       ["/metrics" {:get metrics-handler}]
       ;; Embedded docs site (resources/vis-docs/*.md). `docs/handle` owns
       ;; /docs, /docs/<slug>, /docs/assets/**, and re-reads the markdown per
       ;; request (live-reload) so editing a doc during development shows on a
       ;; browser refresh — no gateway restart. Wrapped to 404 a /docs path the
       ;; handler doesn't own (it returns nil there). #'var → live on :reload.
       ["/docs"
        {:get (fn [req]
                (or (docs/handle req) (error-response 404 :not-found "no such doc")))}]
       ["/docs/*path"
        {:get (fn [req]
                (or (docs/handle req) (error-response 404 :not-found "no such doc")))}]
       ["/v1" ["/models" {:get models-handler}] ["/events" {:get multi-events-handler}]
        ["/capabilities" {:get capabilities-handler}]
        ["/devices" {:get list-devices-handler :post register-device-handler}]
        ["/devices/actions/test" {:post test-device-handler}]
        ["/devices/:token" {:delete delete-device-handler}]
        ["/settings" {:get list-settings-handler :post set-setting-handler}]
        ["/mcp/servers" {:get mcp-servers-handler :post save-mcp-server-handler}]
        ["/mcp/servers/actions/test" {:post test-mcp-server-handler}]
        ["/mcp/servers/:name" {:put save-mcp-server-handler :delete delete-mcp-server-handler}]
        ["/mcp/servers/:name/actions/enable" {:post set-mcp-server-enabled-handler}]
        ["/mcp/servers/:name/actions/kill" {:post kill-mcp-server-handler}]
        ["/mcp/servers/:name/actions/start" {:post start-mcp-server-handler}]
        ["/mcp/servers/:name/auth/start" {:post mcp-auth-start-handler}]
        ["/mcp/servers/:name/auth/complete" {:post mcp-auth-complete-handler}]
        ["/mcp/servers/:name/auth/poll" {:post mcp-auth-poll-handler}]
        ["/mcp/servers/:name/auth/cancel" {:post mcp-auth-cancel-handler}]
        ["/mcp/servers/:name/auth/logout" {:post mcp-auth-logout-handler}]
        ["/settings/:id" {:get get-setting-handler}] ["/providers" {:post add-provider-handler}]
        ["/provider-presets" {:get provider-presets-handler}]
        ["/providers/:provider-id" {:delete remove-provider-handler}]
        ["/providers/:provider-id/status" {:get provider-status-handler}]
        ["/providers/:provider-id/limits" {:get provider-limits-handler}]
        ["/providers/:provider-id/models" {:get provider-models-handler}]
        ["/providers/:provider-id/auth/start" {:post provider-auth-start-handler}]
        ["/providers/:provider-id/auth/complete" {:post provider-auth-complete-handler}]
        ["/providers/:provider-id/auth/poll" {:post provider-auth-poll-handler}]
        ["/providers/:provider-id/auth/cancel" {:post provider-auth-cancel-handler}]
        ["/providers/:provider-id/logout" {:post provider-logout-handler}]
        ["/router" {:get router-handler :patch router-default-handler}]
        ["/clients" {:post client-register-handler}]
        ["/clients/:cid" {:delete client-release-handler}] ["/admin/status" {:get status-handler}]
        ["/admin/stop" {:post stop-handler}]
        ["/sessions" {:get list-sessions-handler :post create-session-handler}]
        ["/sessions/actions/search" {:get search-sessions-handler}] ["/fs" {:get browse-fs-handler}]
        ["/fs/actions/mkdir" {:post create-directory-handler}]
        ["/projects" {:get list-projects-handler :post create-project-handler}]
        ["/projects/actions/ensure" {:post ensure-project-for-root-handler}]
        ["/projects/:pid"
         {:get get-project-handler :patch patch-project-handler :delete delete-project-handler}]
        ["/projects/:pid/sessions" {:patch reorder-project-sessions-handler}]
        [(sid-route "")
         {:get soul-handler :patch patch-session-handler :delete delete-session-handler}]
        [(sid-route "/slashes") {:get slashes-handler}]
        [(sid-route "/release") {:post release-session-handler}]
        [(sid-route "/human-input") {:get list-human-input-handler}]
        [(sid-route "/human-input/:request-id/actions/submit") {:post submit-human-input-handler}]
        [(sid-route "/human-input/:request-id/actions/cancel") {:post cancel-human-input-handler}]
        [(sid-route "/events") {:get events-handler}] [(sid-route "/voice") {:post voice-handler}]
        [(sid-route "/voice/model") {:get voice-model-handler :post voice-model-handler}]
        [(sid-route "/voice/jobs/:job-id") {:get voice-job-handler :delete voice-job-handler}]
        [(sid-route "/voice/jobs/:job-id/events") {:get voice-job-events-handler}]
        [(sid-route "/events-since") {:get events-since-handler}]
        [(sid-route "/seq") {:get seq-handler}] [(sid-route "/context") {:get context-handler}]
        [(sid-route "/transcript") {:get transcript-handler}]
        [(sid-route "/artifacts") {:get session-artifacts-handler}]
        [(sid-route "/transcript.md") {:get transcript-md-handler}]
        [(sid-route "/transcript.html") {:get transcript-html-handler}]
        [(sid-route "/resources") {:get resources-handler}]
        [(sid-route "/resources/stop") {:post resource-stop-handler}]
        [(sid-route "/resources/logs") {:get resource-logs-handler}]
        [(sid-route "/iterations/:iid/attachments") {:post append-attachment-handler}]
        [(sid-route "/iterations/:iid/attachments/:idx") {:get attachment-bytes-handler}]
        [(sid-route "/model") {:get session-model-handler :patch set-session-model-handler}]
        [(sid-route "/usage") {:get usage-handler}]
        [(sid-route "/workspace") {:get workspace-handler}]
        [(sid-route "/workspace/root") {:patch change-root-handler}]
        [(sid-route "/workspace/drafts") {:get drafts-handler :post create-draft-handler}]
        [(sid-route "/workspace/drafts/:workspace-id") {:delete abandon-draft-handler}]
        [(sid-route "/workspace/stash") {:post stash-draft-handler}]
        [(sid-route "/workspace/resume") {:post resume-draft-handler}]
        [(sid-route "/suggest") {:get suggest-handler}]
        [(sid-route "/turns") {:get list-turns-handler :post submit-turn-handler}]
        [(sid-route "/turns/:tid")
         {:get get-turn-handler
          :patch update-queued-turn-handler
          :delete delete-queued-turn-handler}]
        [(sid-route "/turns/:tid/trace") {:get turn-trace-handler}]
        [(sid-route "/turns/:tid/attachments") {:get turn-attachments-handler}]
        [(sid-route "/turns/:tid/cancel") {:post cancel-turn-handler}]
        [(sid-route "/cancel-current") {:post cancel-current-turn-handler}]
        [(sid-route "/drain-queue") {:post drain-idle-handler}]
        [(sid-route "/resume-queue") {:post resume-queue-handler}]]]
      (keep (fn [{:keys [routes]}]
              (when routes
                (try (routes token)
                     (catch Throwable t
                       (tel/log! :error ["gateway: route contribution failed" (ex-message t)])
                       nil))))
            contribs))))

(defn- wrap-scoped-params
  "Param parsing with a hard boundary: uris under a contribution prefix
   that declared `:form-params?` get full `wrap-params` (query +
   urlencoded form bodies - what HTMX forms send); everything else gets
   query params ONLY, so the form parser can never consume a JSON API
   body (curl -d and many clients default to the urlencoded
   content-type while posting JSON)."
  [handler contribs]
  (let [form-handler (ring-params/wrap-params handler)]
    (fn [request]
      (let
        [uri (str (:uri request))
         form? (some (fn [{:keys [prefix form-params?]}]
                       (and form-params? prefix (str/starts-with? uri prefix)))
                     contribs)]

        (if form?
          (form-handler request)
          (handler (ring-params/assoc-query-params request "UTF-8")))))))

(defn- wrap-scoped-multipart
  "Multipart parsing, prefix-scoped exactly like `wrap-scoped-params`: only
   uris under a contribution that declared `:multipart?` get their
   `multipart/form-data` body parsed, with each part stored as an in-memory
   byte array (`:multipart-params` → `{\"field\" {:filename :content-type
   :bytes} | \"text\"}`) — right for the small, capped image uploads the web
   composer posts, and no temp-file cleanup. Non-multipart requests pass
   straight through, so JSON/urlencoded routes are never touched."
  [handler contribs]
  (let
    [mp-handler (ring-multipart/wrap-multipart-params handler
                                                      {:store (multipart-ba/byte-array-store)})]
    (fn [request]
      (let
        [uri (str (:uri request))
         multipart? (some (fn [{:keys [prefix multipart?]}]
                            (and multipart? prefix (str/starts-with? uri prefix)))
                          contribs)]

        (if multipart? (mp-handler request) (handler request))))))

(defn- app
  [^String token contribs]
  (->
    (rr/ring-handler
      (router token contribs)
      (rr/routes
        ;; /ui/ and /ui (and any /path/) are the same place: strip the
        ;; trailing slash with a redirect before falling to 404.
        (rr/redirect-trailing-slash-handler {:method :strip})
        (rr/create-default-handler
          {:not-found (fn [request]
                        ;; A contribution that owns a `:prefix` may render its
                        ;; OWN 404 (e.g. a styled HTML page) instead
                        ;; of the raw JSON below — same per-prefix dispatch as
                        ;; `:on-unauthorized`. Non-prefixed paths (the API) keep
                        ;; the JSON error.
                        (let [uri (str (:uri request))]
                          (or (some (fn [{:keys [prefix on-not-found]}]
                                      (when (and prefix on-not-found (str/starts-with? uri prefix))
                                        (on-not-found request)))
                                    contribs)
                              (error-response 404 :not-found "no such route"))))
           :method-not-allowed (fn [_]
                                 (error-response 405 :method-not-allowed "method not allowed"))})))
    (wrap-auth token contribs)
    ;; Runs BEFORE the token gate: an out-of-date client deserves the version
    ;; verdict, not a 401 that hides it.
    (wrap-protocol)
    (wrap-scoped-params contribs)
    (wrap-scoped-multipart contribs)
    (ring-cookies/wrap-cookies)
    (wrap-errors)
    (wrap-cors)))

(defonce ^:private live-app
  ;; `{:handler ring-handler :fp routes-fingerprint}` — the handler Jetty
  ;; actually calls, rebuilt whenever the contribution fingerprint moves
  ;; (extension loaded after start, jar dropped + ext reload, imperative
  ;; register) so routes mount into a RUNNING server without a restart.
  (atom nil))

(defn- rebuild-app!
  []
  (when-let [{:keys [token]} @server-state]
    (reset! live-app {:handler (app token (vec (contributions))) :fp (routes-fingerprint)}))
  nil)

(defn- serving-handler
  "The fn handed to Jetty: serve the cached handler, but first compare
   the contribution fingerprint and rebuild on drift. This is what makes
   ordering irrelevant — the server notices contributions that arrive
   after it started, on their first request."
  [request]
  (let [{:keys [handler fp]} @live-app]
    (if (and handler (= fp (routes-fingerprint)))
      (handler request)
      (do (rebuild-app!) ((:handler @live-app) request)))))

;; =============================================================================
;; Lifecycle
;; =============================================================================

(defonce ^:private toggle-persist-listener-installed? (atom false))

(defn- install-toggle-persistence!
  "Hydrate feature toggles from the `toggles:` slot of the merged YAML config
   and install a listener that writes every change back. Mirrors the
   TUI's wiring in `channel-tui/screen.clj` so a toggle flipped from any
   gateway client survives a gateway restart -
   without this, only TUI-hosted processes ever persisted toggles.
   Idempotent: hydration re-runs harmlessly; the save listener installs
   once per process."
  []
  (try (let [raw (or (config/load-config-raw) {})]
         (toggles/hydrate-from-config! raw)
         ;; Self-heal stale toggle cruft: an old build persisted keyword-id
         ;; toggles whose namespace was dropped on serialise (`:shell/enabled`
         ;; -> a meaningless `enabled: true`). Those ids no longer register, so
         ;; hydrate ignores them but they linger in state.yml until a flip. If
         ;; any orphan is present, rewrite the canonical snapshot NOW so the
         ;; file converges instead of carrying the garbage forever.
         (when (toggles/has-orphan-keys? (get raw "toggles"))
           (config/save-config! (assoc raw "toggles" (toggles/snapshot)))))
       (when (compare-and-set! toggle-persist-listener-installed? false true)
         (toggles/add-listener!
           (fn [_event]
             (try (let [raw (or (config/load-config-raw) {})]
                    (config/save-config! (assoc raw "toggles" (toggles/snapshot))))
                  (catch Throwable t
                    (tel/log!
                      {:level :warn :id ::toggle-persist-failed :data {:error (ex-message t)}}
                      "Toggle persistence failed; in-memory value still applies."))))))
       (catch Throwable t
         (tel/log! {:level :warn :id ::toggles-hydrate-failed :data {:error (ex-message t)}}
                   "Toggle hydration from config failed; defaults stand."))))

(defn- bind-failure?
  "True when `t`'s cause chain carries a port-already-bound `BindException` —
   the signature of a successor daemon racing a predecessor that has not yet
   released the port during a close-then-reopen handoff."
  [^Throwable t]
  (loop [c t]
    (cond (nil? c) false
          (instance? BindException c) true
          :else (recur (.getCause c)))))

(defn- start-jetty!
  "Run Jetty, tolerating a TRANSIENT bind failure until `deadline-ms`. A daemon
   spawned right after its predecessor stopped can find the port still held while
   the old Jetty finishes draining the exiting client's parked SSE connection;
   dying here would leave the client's `await-registry!` to time out with
   \"gateway daemon did not become ready\". Instead we back off and retry until
   the port frees or the deadline passes, then let the original failure surface."
  [handler opts deadline-ms]
  (loop []

    (let
      [outcome (try {:server (jetty/run-jetty handler opts)}
                    (catch Throwable t
                      (if (and (bind-failure? t) (< (System/currentTimeMillis) (long deadline-ms)))
                        ::retry
                        (throw t))))]
      (if (= outcome ::retry) (do (Thread/sleep 150) (recur)) (:server outcome)))))

(defn- loopback-mirror-configurator
  "Ring/Jetty `:configurator` adding a SECOND connector on 127.0.0.1:`port`.

   A specific-IP bind — what `--pair` picks so the phone can reach us — does not
   answer on loopback, yet every local caller (the TUI, the `vis` CLI, and
   discovery's `port-free?` probe) dials 127.0.0.1. Without this mirror the local
   half of the machine sees a free port and starts a SECOND gateway on it: split
   brain rather than a visible error. The mirror shares the same `Server`, so it
   is one handler, one thread pool, one session state, and it widens reach by
   exactly loopback — auth is untouched. `0.0.0.0` already covers loopback and
   must not get a mirror (the bind would collide with itself)."
  [^long port]
  (fn [^Server server]
    (let
      [^ServerConnector primary
       (first (.getConnectors server))

       ^HttpConnectionFactory http
       (.getConnectionFactory primary HttpConnectionFactory)

       factories
       ^"[Lorg.eclipse.jetty.server.ConnectionFactory;"
       (into-array ConnectionFactory
                   [(HttpConnectionFactory. (HttpConfiguration. (.getHttpConfiguration http)))])

       mirror
       (ServerConnector. server factories)]

      (.setHost mirror DEFAULT_HOST)
      (.setPort mirror (int port))
      (.addConnector server mirror))))

(defn start!
  "Start the gateway on the Ring Jetty adapter with virtual threads.
   Returns `{:port :host :token-file}`. Throws when already running.
   Safe to call from any host process - the daemon (`vis-agent gateway start`), a TUI
   run, or an embedded caller."
  ([] (start! {}))
  ([{:keys [port host token-file require-token? db managed?]}]
   (when @server-state (throw (ex-info "gateway already running" {:type :gateway/already-running})))
   (let
     [port
      (int (or port DEFAULT_PORT))

      host
      (or host DEFAULT_HOST)

      loopback?
      (= host DEFAULT_HOST)

      ;; Keep 127.0.0.1 served even when the primary bind is a concrete remote
      ;; IP, so a `--pair` daemon is still the one gateway the local TUI finds.
      mirror-loopback?
      (not (or loopback? (= host "0.0.0.0")))

      ;; Loopback default: NO token (single local user; the dance is
      ;; friction). Non-loopback: token MANDATORY, not overridable —
      ;; an open bind without auth is never a sane default.
      require-token?
      (if loopback? (boolean require-token?) true)

      path
      (if token-file (Path/of token-file (make-array String 0)) (default-token-path))

      token
      (ensure-token! path)

      db
      (or db (config/resolve-db-spec))

      _
      (when-let [db-path (and (map? db) (:path db))]
        (System/setProperty "vis.db.path" (str db-path)))

      ;; ONE gateway per DB (see [[discovery/foreign-owner]]). A second daemon does
      ;; not fail to bind - BSD lets `0.0.0.0:P` listen beside an existing
      ;; `127.0.0.1:P` - it just takes the registry over while the first keeps
      ;; running: two halves narrating one session, each with its own
      ;; `:current-turn` and cancellation tokens, so a stop only reaches the half
      ;; that answered the client and the other keeps iterating.
      _
      (when-let [{:keys [pid host port]} (discovery/foreign-owner db)]
        (throw (ex-info (str "a gateway is already running for this DB at http://"
                             host
                             ":"
                             port
                             " (pid "
                             pid
                             "). Stop it first: `vis-agent gateway stop`. "
                             "To pair a phone with the daemon that is already running: "
                             "`vis-agent gateway pair`.")
                        {:type :gateway/db-already-served
                         :pid pid
                         :host host
                         :port port
                         :vis/user-error true})))

      ;; :token must be visible to rebuild-app! before Jetty serves the
      ;; first request; a failed boot must roll the state back so a
      ;; retry isn't refused as "already running".
      _
      (reset! server-state {:token token
                            :require-token? require-token?
                            :managed? (boolean managed?)
                            :started-at-ms (System/currentTimeMillis)})

      _
      (rebuild-app!)

      ;; Load the persistence backend NOW, single-threaded, so the
      ;; first DB touch never happens on N concurrent request threads.
      _
      (do (state/warm-db!)
          (try (state/start-prewarming! [:api :tui])
               (catch Throwable t
                 (tel/log! :warn ["gateway: startup session prewarm failed" (ex-message t)]))))

      ;; A dead process can leave durable turn rows marked :running. Clear
      ;; those stale flags to :interrupted, but NEVER reconstruct or resubmit
      ;; their requests: queued work is intentionally process-memory only.
      _
      (try (state/reconcile-orphaned-turns!)
           (catch Throwable t
             (tel/log! :warn
                       ["gateway: orphan-running-turn reconciliation failed" (ex-message t)])))

      ;; Hydrate persisted toggles + install the state.yml save
      ;; listener so web/gateway-driven flips survive restarts.
      _
      (install-toggle-persistence!)

      ;; Native push: one tap on the event appender turns every terminal turn
      ;; into an APNs alert. Silent no-op until a device registers AND an APNs
      ;; key is configured, so this costs one set lookup per event otherwise.
      _
      (do (push/set-session-describer! (fn [sid tid]
                                         (try {:title (get (state/soul sid) "title")
                                               ;; the ANSWER itself, so the banner says what
                                               ;; vis said rather than that it said something.
                                               :answer (state/turn-answer-text sid tid)}
                                              (catch Throwable _ nil))))
          ;; Which gateway the alert came from: a phone paired with several
          ;; machines must open the tapped session on THIS one, and a session id
          ;; only means anything on the gateway that minted it.
          (push/set-gateway-id! (gateway-instance-id db host port))
          (state/add-event-tap! ::push push/on-event!)
          ;; Human-input bridge: a `request-human-input!` raised inside a
          ;; session becomes a session event, so the companion app sees the
          ;; blocked run live (and the push tap above alerts the phone).
          (gw-human-input/install!))

      server
      (try
        (start-jetty!
          serving-handler
          (cond->
            {:port port :host host :join? false :virtual-threads? true :send-server-version? false}
            mirror-loopback?
            (assoc :configurator (loopback-mirror-configurator port)))
          (+ (System/currentTimeMillis) 6000))
        (catch Throwable t (reset! server-state nil) (reset! live-app nil) (throw t)))]

     (when-not (= host DEFAULT_HOST)
       (tel/log! :warn ["gateway: binding to non-loopback host" host]))
     (reset! server-state {:server server
                           :port port
                           :host host
                           :token token
                           :token-path (str path)
                           :db db
                           :clients {}
                           :sse-clients {}
                           :client-registrations-total 0
                           :client-releases-total 0
                           :client-replacements-total 0
                           :client-leases-reaped-total 0
                           :client-dead-reaped-total 0
                           :client-duplicates-reaped-total 0
                           :require-token? require-token?
                           :managed? (boolean managed?)
                           :started-at-ms (System/currentTimeMillis)
                           :saw-client? false})
     ;; The gateway's own control-plane port is reserved so a jailed child can NEVER reach
     ;; it through the proxy, even though loopback egress is allowed by default (SSRF floor).
     (try (gateway-sandbox/set-reserved-ports! [port]) (catch Throwable _ nil))
     (try (discovery/register-self! db {:port port :host host :secret token})
          (catch Throwable t
            (tel/log! :warn ["gateway: registry self-registration failed" (ex-message t)])))
     (when managed? (ensure-idle-reaper!))
     (tel/log! :info
               ["gateway: listening" (str host ":" port)
                (if require-token? "auth: bearer token" "auth: disabled (loopback)")
                (if managed? "lifecycle: managed" "lifecycle: foreground")])
     {:port port
      :host host
      :token-file (str path)
      :require-token? require-token?
      :managed? (boolean managed?)})))

(def ^:private GRACEFUL_DRAIN_MS
  "Max time `stop!` waits for in-flight turns to finish before forcing Jetty
   down, so a SIGTERM / `vis-agent gateway restart` landing mid-turn lets active work
   complete instead of being cut off. Only ever waits when turns are actually
   running (the refcount-idle stop path already has zero)."
  8000)

(defn- await-turns-drained!
  "Block up to `GRACEFUL_DRAIN_MS` for running turns to reach zero, polling
   every 100ms. Returns the residual running-turn count (0 = fully drained)."
  []
  (let [deadline (+ (System/currentTimeMillis) (long GRACEFUL_DRAIN_MS))]
    (loop []

      (let [n (long (running-turn-count))]
        (if (or (zero? n) (>= (System/currentTimeMillis) deadline))
          n
          (do (Thread/sleep 100) (recur)))))))

(defn stop!
  "Stop the gateway server if running. Idempotent."
  []
  (when-let [{:keys [^Server server db]} @server-state]
    ;; Release the listening socket FIRST so a successor daemon racing this
    ;; close-then-reopen handoff can bind the port immediately. The slow reap
    ;; below (killing every session's background `shell` children + REPLs) can eat
    ;; seconds; when `.stop` ran AFTER it, the old process kept the port in
    ;; LISTEN through the whole reap, the successor's bind-retry AND the
    ;; client's `await-registry!` both timed out, and the first reopen died
    ;; with "gateway daemon did not become ready". SO_REUSEADDR can't rescue
    ;; this — it never lets a bind win over an ACTIVE listener, only a closed
    ;; one, so the fix is to close the socket before the reap, not to retry.
    ;; Graceful drain: give in-flight turns a bounded window to finish before we
    ;; tear the socket + runtime down, so a SIGTERM / restart mid-turn doesn't
    ;; guillotine active work. No-op when nothing is running (refcount-idle stop).
    (let [pending (long (running-turn-count))]
      (when (pos? pending)
        (tel/log! :info ["gateway: draining before stop" pending "turn(s) running"])
        ;; Cancel in-flight turns FIRST. The drain below only waits for them to
        ;; reach a terminal state; it does NOT keep the provider transport
        ;; alive — that executor is torn down concurrently on shutdown. A turn
        ;; left looping would dispatch its next LLM iteration into the dying
        ;; pool and die with a RejectedExecutionException surfaced to the user
        ;; as a bogus "Provider unavailable"; cancelling makes it exit cleanly.
        (try (state/cancel-all-running!) (catch Throwable _ nil))
        (let [residual (long (await-turns-drained!))]
          (when (pos? residual)
            (tel/log! :warn
                      ["gateway: drain timed out; forcing stop" residual
                       "turn(s) still running"])))))
    (try (.stop server) (catch Throwable _ nil))
    ;; Kill every session's background resources (background `shell` children, REPLs)
    ;; BEFORE the JVM goes away — their :stop-fn thunks live only in this
    ;; process; once it exits the children reparent to init and leak.
    (try (state/discard-prewarmed!) (catch Throwable _ nil))
    (try (resources/shutdown!) (catch Throwable _ nil))
    (try (discovery/deregister-self! db) (catch Throwable _ nil))
    (reset! server-state nil)
    (reset! live-app nil)
    ;; Unpark `serve-main!` so the daemon process ends after a refcount/admin
    ;; stop. Without this the JVM stayed parked on a dead promise: every TUI
    ;; close-then-reopen leaked one idle daemon process (port + registry were
    ;; released, but nothing terminated the process).
    (deliver serve-exit true))
  nil)

(defn running? [] (some? @server-state))

(defonce ^:private signal-forensics (atom nil))

(defn- interactive-terminal?
  "True when this JVM really is attached to a terminal a human can Ctrl-C. JDK 22+
   ALWAYS hands back a `Console` (JLine-backed), so `isTerminal` is the only honest
   test; false for a managed/background daemon whose stdio is a log file or a pipe."
  []
  (try (if-let [c (System/console)]
         (.isTerminal ^java.io.Console c)
         false)
       (catch Throwable _ false)))

(defn- signal-disposition
  "PURE policy: what a delivered signal DOES to this daemon — `:exit` or `:ignore`.

   SIGTERM is the deliberate stop (`kill`, a supervisor, system shutdown) and always
   exits, draining in-flight turns. SIGINT/SIGHUP only MEAN 'the operator stopped me'
   when this process owns the terminal they came from. A managed/background gateway
   that receives one is collateral — a child tool signalling its own process group, a
   `kill 0`, a terminal closing on some other member of the group — and a daemon that
   is serving other sessions' live turns must not die of someone else's Ctrl-C. It
   logs loudly and keeps serving; `/v1/admin/stop` and SIGTERM still stop it."
  [{:keys [signal managed? interactive?]}]
  (if (and (contains? #{"INT" "HUP"} signal) (or managed? (not interactive?))) :ignore :exit))

(def ^:private exit-frame-re
  ;; A rendered frame carries its MODULE ("java.base/java.lang.Shutdown.exit(…)"), so
  ;; this must not anchor at the start of the line.
  #"java\.lang\.(?:Shutdown\.exit|Runtime\.(?:exit|halt)|System\.exit)\(")

(defn- exit-culprit
  "Given `traces` (thread NAME -> stack frames, top frame first), name the thread that
   is INSIDE `System/exit` and the first frames of whatever called it.

   A `System/exit` anywhere in the daemon kills every live turn in every session, and
   it needs no signal to happen: an extension, a library that thinks it owns the
   process, or a script the linter compiled can all reach it. The shutdown hook is the
   LAST place that can still see the caller — by then the thread is parked in
   `Shutdown.runHooks` with its own frames still on the stack. Nil when no thread is
   exiting (a signal handled by the JVM default, or a normal end of `-main`)."
  [traces]
  (some (fn [[nm frames]]
          (let
            [fv
             (mapv str frames)

             i
             (first (keep-indexed (fn [idx f]
                                    (when (re-find exit-frame-re f) idx))
                                  fv))]

            (when i
              {"thread" (str nm)
               "frames"
               (into []
                     (comp (remove #(re-find exit-frame-re %))
                           ;; reflection/method-handle plumbing names nobody
                           (remove #(re-find #"(?:^|/)(?:jdk\.internal|java\.lang\.reflect)\." %))
                           (take 6))
                     (subvec fv i))})))
        traces))

(defn- thread-stacks
  "Every live thread as NAME -> frame strings, top frame first."
  []
  (persistent! (reduce (fn [acc [^Thread t frames]]
                         (assoc! acc (.getName t) (mapv str frames)))
                       (transient {})
                       (Thread/getAllStackTraces))))

(defn- install-signal-forensics!
  "Name — and, when it is collateral, SURVIVE — the signal that reaches a daemon.

   A FOREGROUND `vis-agent gateway start` shares its process group and controlling
   terminal with anything that was spawned before children were detached, so a
   group-directed SIGINT/SIGHUP — Ctrl-C in that tab, the terminal closing, or a child
   tool signalling its own process group — reached the JVM and ran the shutdown hook.
   In the log that death looked exactly like an explicit `/v1/admin/stop`.

   [[signal-disposition]] decides: a stray INT/HUP on a detached daemon is LOGGED and
   IGNORED (the daemon keeps serving every other session), everything else logs first
   and then `System/exit`s 128+signum so the shutdown hook still drains in-flight turns
   exactly as before.

   Idempotent: returns a map of signal name -> previously installed handler on the
   first call (so a caller/test can restore them), nil afterwards."
  ([] (install-signal-forensics! nil))
  ([{:keys [managed?]}]
   (when (nil? @signal-forensics)
     (let
       [interactive?
        (interactive-terminal?)

        installed
        (reduce
          (fn [acc ^String nm]
            (try (let
                   [prev (sun.misc.Signal/handle
                           (sun.misc.Signal. nm)
                           (reify
                             sun.misc.SignalHandler
                               (handle [_ sig]
                                 (let
                                   [^sun.misc.Signal s sig
                                    action (signal-disposition {:signal (.getName s)
                                                                :managed? (boolean managed?)
                                                                :interactive? interactive?})]

                                   (try
                                     (if (= :ignore action)
                                       (tel/log! :warn
                                                 ["gateway: ignoring SIG" (.getName s)
                                                  "- a detached daemon is not stopped by a stray"
                                                  "group signal;" (running-turn-count)
                                                  "turn(s) running; use /v1/admin/stop or SIGTERM"])
                                       (tel/log! :warn
                                                 ["gateway: received SIG" (.getName s) "- stopping;"
                                                  (running-turn-count) "turn(s) running"]))
                                     (catch Throwable _ nil))
                                   (when (= :exit action) (System/exit (+ 128 (.getNumber s))))))))]
                   (assoc acc nm prev))
                 (catch Throwable _ acc)))
          {}
          ["INT" "TERM" "HUP"])]

       (reset! signal-forensics installed)
       installed))))

#_{:clj-kondo/ignore [:unused-private-var]}
(defn- restore-signal-forensics!
  "Undo [[install-signal-forensics!]] by re-installing the captured handlers."
  [installed]
  (doseq [[^String nm prev] installed]
    (when prev (try (sun.misc.Signal/handle (sun.misc.Signal. nm) prev) (catch Throwable _ nil))))
  (reset! signal-forensics nil)
  nil)

(defn serve-main!
  "Blocking entry for the `vis-agent gateway start` command: start, print the
   connection line, park forever (Ctrl-C / SIGTERM stops the JVM)."
  [{:keys [port host token-file require-token? db managed? pair?]}]
  ;; Profile the daemon into its own JFR file when VIS_JFR is inherited from the
  ;; client that spawned us (idempotent with the -main call for direct callers).
  (try ((requiring-resolve 'com.blockether.vis.internal.jfr/maybe-start!) "gateway")
       (catch Throwable _ nil))
  (let
    [;; `--pair` is a request for PHONE access, so it selects the bind. With no
     ;; explicit `--host` the loopback default printed a QR for an address
     ;; nothing listened on — the failure landed on the phone, looking like a
     ;; broken app. Bind every interface, which is what the QR's `alt=` hosts
     ;; promise; it is non-loopback, so `start!` forces the bearer token.
     auto-host
     (when (and pair? (str/blank? host)) (pairing/pair-bind-host))

     {:keys [port host token-file require-token?]}
     (start! {:port (some-> port
                            parse-long)
              :host (or auto-host host)
              :token-file token-file
              :require-token? require-token?
              :db db
              :managed? managed?})

     ;; `config/init-cli!` has already redirected System/out AND `*out*` into
     ;; ~/.vis/logs/vis.log, so a plain `println` here is invisible — the
     ;; daemon looked completely silent (no listen line, no pairing QR).
     ;; Write the human banner to the process' ORIGINAL stdout instead.
     emit!
     (fn [line]
       (.println config/original-stdout ^String (str line))
       (.flush config/original-stdout))]

    (emit! (str "vis-agent gateway listening on http://" host ":" port))
    (if require-token?
      (emit! (str "bearer token: " token-file))
      (emit! "auth: disabled (loopback default; pass --require-token to enable)"))
    (when auto-host
      (emit! (str "--pair with no --host: bound "
                  auto-host
                  (if (= auto-host "0.0.0.0")
                    " (all interfaces) so your phone can reach it"
                    " (your Tailscale IP) so your phone can reach it")))
      (when-not (= auto-host "0.0.0.0")
        (emit! (str "127.0.0.1:" port " is served too, so the local TUI still attaches"))))
    (when pair?
      (pairing/print-pairing! {:host host
                               :port port
                               :token (some-> token-file
                                              slurp
                                              str/trim)
                               :require-token? require-token?
                               :emit emit!}))
    ;; Forensics BEFORE the hook: a signal-driven death and an explicit
    ;; /v1/admin/stop both surface as "gateway: draining before stop", so name
    ;; the trigger in the log or an unexplained daemon exit stays unexplainable.
    ;; `managed?` also decides POLICY: a detached daemon survives a stray INT/HUP.
    (install-signal-forensics! {:managed? managed?})
    (.addShutdownHook
      (Runtime/getRuntime)
      (Thread. ^Runnable
               (fn []
                 ;; Name the caller while the stack still exists: an
                 ;; in-process `System/exit` (extension, library,
                 ;; compiled script) leaves no other trace at all.
                 (let [culprit (try (exit-culprit (thread-stacks)) (catch Throwable _ nil))]
                   (tel/log! :info
                             (into ["gateway: JVM shutdown hook fired"
                                    "(signal or System/exit) - stopping;" (running-turn-count)
                                    "turn(s) running"]
                                   (when culprit
                                     ["- exit called on thread" (get culprit "thread")
                                      (str/join " <- " (get culprit "frames"))]))))
                 (stop!))
               "vis-gateway-shutdown"))
    @serve-exit
    (System/exit 0)))
