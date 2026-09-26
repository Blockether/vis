(ns com.blockether.vis.internal.gateway.server
  "Gateway HTTP/SSE server.

   Clojure-native stack: reitit-ring routes -> Ring middleware -> a Jetty 12
   CORE handler (`ring.adapter.jetty9` — no servlet layer) on JDK virtual
   threads (`:virtual-threads? true`).
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
  (:require
    [clojure.string :as str]
    [com.blockether.vis.contract.gateway :as gateway-contract]
    [com.blockether.vis.contract.openapi :as openapi-contract]
    [com.blockether.vis.internal.config.core :as config]
    [com.blockether.vis.internal.gateway.wiring :as wiring]
    [com.blockether.vis.internal.improve.review :as improve-review]
    [com.blockether.vis.internal.docs.core :as docs]
    [com.blockether.vis.internal.extension.core :as extension]
    [com.blockether.vis.internal.extension.client :as client-extensions]
    [com.blockether.vis.internal.gateway.discovery :as discovery]
    [com.blockether.vis.internal.gateway.view :as gw-view]
    [com.blockether.vis.internal.gateway.pairing :as pairing]
    [com.blockether.vis.internal.gateway.runtime :as protocol]
    [com.blockether.vis.internal.gateway.push :as push]
    [com.blockether.vis.internal.gateway.state :as state]
    [com.blockether.vis.contract.wire :as wire]
    [com.blockether.vis.internal.gateway.server.transport.sse :as sse]
    [com.blockether.vis.internal.gateway.server.council :as council-api]
    [com.blockether.vis.internal.gateway.server.decisions :as decisions-api]
    [com.blockether.vis.internal.gateway.server.devices :as devices-api]
    [com.blockether.vis.internal.gateway.server.fs :as fs-api]
    [com.blockether.vis.internal.gateway.server.http :as http]
    [com.blockether.vis.internal.gateway.server.instance :as instance]
    [com.blockether.vis.internal.gateway.server.mcp :as mcp-api]
    [com.blockether.vis.internal.gateway.server.projects :as projects-api]
    [com.blockether.vis.internal.gateway.server.providers :as providers-api]
    [com.blockether.vis.internal.gateway.server.sessions :as sessions-api]
    [com.blockether.vis.internal.gateway.server.settings :as settings-api]
    [com.blockether.vis.internal.gateway.server.speech :as speech-api]
    [com.blockether.vis.internal.gateway.server.transcripts :as transcripts-api]
    [com.blockether.vis.internal.gateway.server.turns :as turns-api]
    [com.blockether.vis.internal.gateway.server.views :as views-api]
    [com.blockether.vis.internal.sandbox.gateway :as gateway-sandbox]
    [com.blockether.vis.internal.gateway.resources :as resources]
    [com.blockether.vis.internal.python.extensions :as python-extensions]
    [com.blockether.vis.internal.python.runtime :as python-runtime]
    [com.blockether.vis.internal.config.toggles :as toggles]
    [com.blockether.vis.internal.util :as util]
    [com.blockether.vis.internal.speech.core :as speech]
    [reitit.exception :as reitit-exception]
    [reitit.ring :as rr]
    [com.blockether.vis.internal.decisions.cache :as decision-cache]
    [com.blockether.vis.internal.decisions.core :as decisions]
    [com.blockether.vis.internal.decisions.jobs :as decision-jobs]
    [ring.adapter.jetty9 :as jetty]
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
            Server ServerConnector]
           [org.eclipse.jetty.server.handler.gzip GzipHandler]))

(def ^:private DEFAULT_PORT 7890)

(def ^:private DEFAULT_HOST "127.0.0.1")

(def ^:private SSE_QUEUE_CAP
  "Per-SSE-connection bounded event queue. `state/fan-out!` (the TURN's
   thread) only ever ENQUEUES here — it never touches the socket — so a
   stalled client (TCP backpressure: backgrounded tab, dead Wi-Fi, buffering
   proxy) fills its own queue and is DROPPED on overflow instead of parking
   the appender, the heartbeat, sibling watchers, or the turn itself."
  1024)

(def ^:private IDLE_REAP_MS 1000)

(def ^:private STARTUP_IDLE_GRACE_MS 30000)

(def ^:private STUCK_TURN_IDLE_MS
  "How long a turn may show no progress at all - no new event on any ring -
   before a managed daemon with NO client left treats it as a ghost rather than
   as work. Long enough that a slow provider call, a long tool run or a paused
   sandbox is never mistaken for one; short enough that a wedged daemon releases
   the port while a human is still looking at it."
  60000)

(def ^:private CLIENT_LEASE_TOUCH_MS
  "Granularity of the lease refresh. A lease already seen this recently is not
   written again, so a busy client costs no `swap!` per request."
  (:touch-ms gateway-contract/client-lease))

;; Delivered by `stop!`; `serve-main!` parks on it so a stopped daemon process
;; EXITS instead of idling forever. In-process callers (tests, REPL) deliver
;; harmlessly — nothing is parked on the latch there.
(defonce ^:private serve-exit (promise))

(defonce ^:private idle-reaper (atom nil))

;; Where the running turns' event rings stood when they last MOVED, and when that
;; was: `{:marker {:turns n :seq n} :since ms}`. Sampled once per reap sweep, so
;; the stall clock measures the TURN and not the moment the last watcher left.
(defonce ^:private turn-progress-watch (atom nil))

(defn- log-client-lease-warning!
  "Emit rare lease-compaction warnings to BOTH telemetry and the managed
   gateway's stderr log. The explicit stderr line remains visible when the
   gateway's asynchronous telemetry handler is saturated."
  [event data]
  (let [message (str "gateway " (name event) " " (pr-str data))]
    (tel/log! :warn [message])
    (.println System/err (str (java.time.Instant/now) " WARN " message))))

(defn- compact-client-leases
  "Drop every lease whose owner is gone and collapse duplicate live leases to one
   per process. Returns the original map by identity when no cleanup is needed,
   keeping the once-per-second steady-state sweep allocation-light.

   An owner proves itself in exactly one of two ways, because only one of them
   exists on this machine:

     a pid    the process is alive HERE - a killed TUI is gone within the second
     no pid   a REMOTE client, judged by its own traffic: every request it makes
              refreshes the lease ([[touch-client-lease!]]), so silence past
              `CLIENT_LEASE_TTL_MS` is the only evidence a phone or a `--gateway`
              CLI that vanished mid-flight will ever leave. Before this, one such
              lease pinned the daemon for the life of the machine."
  [clients now]
  (let [seen-pids
        (java.util.HashSet.)

        removed-ids
        (transient [])

        counts
        (long-array 3)]

    ; [dead duplicates expired]
    (reduce-kv (fn [_ client-id {:keys [pid last-seen-at connected-at]}]
                 (if (some? pid)
                   (cond (not (.add seen-pids pid))
                         (do (aset-long counts 1 (unchecked-inc (aget counts 1)))
                             (conj! removed-ids client-id))
                         (not (discovery/pid-alive-cached? pid))
                         (do (aset-long counts 0 (unchecked-inc (aget counts 0)))
                             (conj! removed-ids client-id)))
                   (when (> (- (long now) (long (or last-seen-at connected-at 0)))
                            (long instance/CLIENT_LEASE_TTL_MS))
                     (aset-long counts 2 (unchecked-inc (aget counts 2)))
                     (conj! removed-ids client-id)))
                 nil)
               nil
               clients)
    (let [removed-ids (persistent! removed-ids)]
      {:clients (if (seq removed-ids) (reduce dissoc clients removed-ids) clients)
       :dead (aget counts 0)
       :duplicates (aget counts 1)
       :expired (aget counts 2)})))

(defn- reap-client-leases!
  "Compact the process-lease map without clobbering a concurrent register or
   release. A skipped CAS is harmless; the one-second reaper retries."
  []
  (when-let [state @instance/server-state]
    (let [before (:clients state)
          {:keys [clients dead duplicates expired]} (compact-client-leases before (util/now-ms))
          ;; An expired lease IS a dead owner - the pid it would have been judged by
          ;; lives on another machine.
          gone (+ (long dead) (long expired))
          removed (+ gone (long duplicates))]

      (when (pos? removed)
        (let [applied? (volatile! false)]
          (swap! instance/server-state
            (fn [current]
              (if (identical? before (:clients current))
                (do (vreset! applied? true)
                    (-> current
                        (assoc :clients clients)
                        (update :client-leases-reaped-total (fnil + 0) removed)
                        (update :client-dead-reaped-total (fnil + 0) gone)
                        (update :client-duplicates-reaped-total (fnil + 0) duplicates)))
                (do (vreset! applied? false) current))))
          (when @applied?
            (doseq [client-id (remove #(contains? clients %) (keys before))]
              (client-extensions/detach-owner! client-id))
            (log-client-lease-warning! :client-leases-compacted
                                       {:before (count before)
                                        :after (count clients)
                                        :dead dead
                                        :expired expired
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
  (when-let [state @instance/server-state]
    (doseq [[sub-id {:keys [pid close!]}] (:sse-clients state)]
      (when (and pid (not (discovery/pid-alive-cached? pid)))
        (swap! instance/server-state update :sse-clients dissoc sub-id)
        (when close! (try (close!) (catch Throwable _ nil)))))))

(defn- gateway-client-metrics
  []
  (let [{:keys [clients sse-clients client-registrations-total client-releases-total
                client-replacements-total client-leases-reaped-total client-dead-reaped-total
                client-duplicates-reaped-total]}
        @instance/server-state]
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
  (let [{:keys [clients sse-clients]} @instance/server-state]
    (+ (count clients) (count sse-clients))))

(defn- running-turn-count [] (state/running-turn-count))

(defonce ^:private extension-startup (atom {:stage "idle"}))

(defn- extension-diagnostic-text
  [text]
  (-> (str text)
      (str/replace #"\u001b\[[0-?]*[ -/]*[@-~]" "")
      (str/replace #"[\p{Cntrl}&&[^\n\t]]" "")
      (util/redact-secret-text (keep (fn [[key value]]
                                       (when (util/secret-key? key) value))
                                     (System/getenv)))))

(defn- extension-startup-status
  "Current extension counts and safe diagnostics for client registration and admin status."
  []
  (let [status (assoc @extension-startup :packages (vec (python-runtime/preparation-status)))]
    (if (= "ready" (:stage status))
      (let [failures (python-extensions/load-failures)]
        (assoc status
          :loaded (count (python-extensions/loaded-python-extensions))
          :failed (count failures)
          :failures (mapv (fn [{:keys [file extension error stale?]}]
                            {:file (some-> file
                                           extension-diagnostic-text)
                             :extension (some-> extension
                                                extension-diagnostic-text)
                             :error (extension-diagnostic-text error)
                             :stale (boolean stale?)})
                          failures)))
      status)))

(defn- prepare-startup-extensions!
  "Load after HTTP starts so clients can report preparation without a health timeout."
  []
  (try (python-extensions/ensure-python-extensions-loaded!)
       (reset! extension-startup {:stage "ready"})
       (catch Throwable t
         (reset! extension-startup {:stage "failed"
                                    :error (extension-diagnostic-text (or (ex-message t)
                                                                          (str t)))}))))

(defn- status-map
  []
  (let [{:keys [port host db require-token? managed?]}
        @instance/server-state

        {:keys [gateway-client-leases gateway-sse-clients gateway-client-registrations-total
                gateway-client-releases-total gateway-client-replacements-total
                gateway-client-leases-reaped-total]}
        (gateway-client-metrics)]

    {:status (if @instance/server-state "running" "stopped")
     :id (instance/gateway-instance-id db host port)
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
  (when-let [{:keys [^Server server db port host token]} @instance/server-state]
    (when (and server db (.isStarted server))
      (try
        (let [entry (discovery/read-registry db)
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
  (let [{:keys [managed? saw-client? started-at-ms]} @instance/server-state]
    (and managed?
         (or saw-client?
             (>= (- (util/now-ms) (long (or started-at-ms 0))) (long STARTUP_IDLE_GRACE_MS))))))

(defn- note-turn-progress!
  "Sample how far the turns this daemon still counts as running have advanced,
   remembering when that sample last CHANGED. Called on every reap sweep whatever
   the client count, so [[turns-stalled?]] never mistakes \"the last client just
   left\" for \"this turn stopped moving a minute ago\"."
  []
  (let [marker
        (state/running-turn-progress)

        now
        (util/now-ms)]

    (swap! turn-progress-watch (fn [prev]
                                 (if (and prev (= marker (:marker prev)))
                                   prev
                                   {:marker marker :since now})))))

(defn- turns-stalled?
  "True when turns are still counted as running but nothing about them has moved
   for `STUCK_TURN_IDLE_MS`. `running-turn-count` alone cannot end a daemon's
   life: a turn whose launch died before it could clear `:current-turn`, or whose
   worker is parked in uninterruptible code and ignored its cancel, keeps that
   entry forever - and the daemon then outlives every client of a turn that will
   never produce another event, holding the port until someone kills the pid."
  []
  (let [{:keys [marker since]} @turn-progress-watch]
    (boolean (and marker
                  (pos? (long (:turns marker)))
                  (>= (- (util/now-ms) (long (or since 0))) (long STUCK_TURN_IDLE_MS))))))

(defn- idle-shutdown-reason
  "Why this managed daemon may stop itself right now, or nil to keep serving.
   `:idle` - nothing holds it. `:stalled-turns` - no client left AND every turn it
   still counts as running has stopped producing events, so nothing alive is
   watching and nothing will finish. Zero clients plus a MOVING turn keeps it
   serving: that is \"I closed the TUI, finish in the background\"."
  []
  (when (and @instance/server-state (idle-shutdown-eligible?) (zero? (long (client-count))))
    (cond (zero? (long (running-turn-count))) :idle
          (turns-stalled?) :stalled-turns)))

(defn- maybe-stop-when-idle!
  "Refcount shutdown (Q1): no timer/idle timeout for foreground daemons. A managed
   daemon exits when no live client lease/SSE stream remains and no turn is still
   moving. Dead-pid leases do not count, so a killed TUI cannot pin the daemon
   forever - and neither can the ghost turn it left behind, which is cancelled on
   the way out so nothing is left holding a cancellation token nobody will fire."
  []
  (when (idle-shutdown-reason)
    (future (try (Thread/sleep 25) ; let the HTTP response that released the last client flush
                 (when-let [reason (idle-shutdown-reason)]
                   (when (= :stalled-turns reason)
                     (tel/log! :warn
                               ["gateway: no clients and" (running-turn-count)
                                "stalled turn(s) - stopping"]))
                   (stop!))
                 (catch Throwable t
                   (tel/log! :warn ["gateway: refcount shutdown failed" (ex-message t)]))))))

(defn register-contributed-sse!
  "Count a contributed SSE connection in the gateway's existing client lifecycle."
  [stream-id close!]
  (let [registered? (volatile! false)]
    (swap! instance/server-state (fn [state]
                                   (when state
                                     (vreset! registered? true)
                                     (-> state
                                         (assoc :saw-client? true)
                                         (assoc-in [:sse-clients stream-id]
                                                   {:pid nil :close! close!})))))
    @registered?))

(defn unregister-contributed-sse!
  "Remove a contributed SSE connection and re-run managed-idle shutdown policy."
  [stream-id]
  (swap! instance/server-state #(when % (update % :sse-clients dissoc stream-id)))
  (maybe-stop-when-idle!)
  nil)

(defn- reap-sweep!
  "One reap sweep, with every step isolated. A step that throws loses ITS step for
   this second and nothing else - above all it must not cost the refcount-shutdown
   check, which is the only thing that ever ends a managed daemon's life."
  []
  (let [step! (fn [label f]
                (try (f)
                     (catch Throwable t
                       (tel/log! :warn ["gateway: idle reap step failed" label (ex-message t)]))))]
    (step! "self-register" ensure-self-registered!)
    (step! "client-leases" reap-client-leases!)
    (step! "sse-clients" reap-sse-clients!)
    (step! "turn-progress" note-turn-progress!)
    (step! "uploads" #(turns-api/reap-uploads! (util/now-ms)))
    (step! "refcount-shutdown" maybe-stop-when-idle!)))

(defn- ensure-idle-reaper!
  "Managed daemons reap dead and duplicate process leases once per second, then
   evaluate refcount shutdown. Status/health requests therefore read an O(1)
   count and never perform OS liveness probes or rebuild a set.

   The loop is armed once per boot and is the ONLY place shutdown is evaluated, so
   it must be unkillable: [[reap-sweep!]] isolates each step, and the client
   register/release and status handlers re-arm it if it ever did die."
  []
  (when (compare-and-set! idle-reaper nil ::starting)
    (reset! turn-progress-watch nil)
    (reset! idle-reaper (future (try (loop []

                                       (Thread/sleep (long IDLE_REAP_MS))
                                       (when @instance/server-state (reap-sweep!) (recur)))
                                     (catch Throwable t
                                       (tel/log! :warn
                                                 ["gateway: idle reaper failed" (ex-message t)]))
                                     (finally (reset! idle-reaper nil)))))))

;; Bearer token (§3)

(defn- default-token-path ^Path [] (.toPath (discovery/default-token-file)))

(defn- ensure-token!
  "Read the bearer token at `path`, minting one on first run. The token file
   is CREATED owner-only (600) ATOMICALLY via create-with-attribute rather
   than write-then-chmod, so the secret is never briefly world-readable at the
   process umask."
  ^String [^Path path]
  (if (Files/exists path (make-array LinkOption 0))
    (str/trim (String. (Files/readAllBytes path) StandardCharsets/UTF_8))
    (let [token
          (str (java.util.UUID/randomUUID))

          owner-only
          (PosixFilePermissions/asFileAttribute (PosixFilePermissions/fromString "rw-------"))]

      (some-> (.getParent path)
              (Files/createDirectories (make-array FileAttribute 0)))
      (try (Files/createFile path (into-array FileAttribute [owner-only]))
           (catch UnsupportedOperationException _
             ;; Non-POSIX filesystem: create without the perm attribute.
             (Files/createFile path (make-array FileAttribute 0))))
      (Files/write path (util/utf8 token) ^"[Ljava.nio.file.OpenOption;" (make-array OpenOption 0))
      token)))

;; SSE (§6.3)

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

(defn- without-settled-picture
  "Drop the repeated live close picture; every supported client rebuilds it."
  [event]
  (if (and (= "view.close" (get event "type"))
           (= "live" (get event "kind"))
           (map? (get event "result")))
    (update event "result" dissoc "view")
    event))

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
      (let [event (.poll queue (long sse/HEARTBEAT_MS) TimeUnit/MILLISECONDS)]
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

   A cursor BELOW the ring's floor (`state/replay-floor`) is the sentinel too.
   The ring is bounded, so ONE long turn evicts thousands of its own frames: a
   client that dropped out mid-turn resumes at a cursor whose neighbourhood is
   gone, and `seq > cursor` answers the surviving TAIL — deltas for blocks whose
   `content.block.started` was evicted, activity for forms it never saw opened —
   megabytes of it, and a partial picture neither side can detect. Rewinding
   replays the running turn WHOLE, exactly what a fresh join is served, and
   falls back to the live tail when nothing is running so the durable transcript
   fills the history in.

   Shared by every session stream the daemon serves, so `/v1/events?sids=…` and the
   fleet feed resolve a cursor identically."
  ^long [sid requested]
  (let [requested
        (long requested)

        current
        (long (state/current-seq sid))

        floor
        (long (state/replay-floor sid))]

    (if (or (neg? requested) (> requested current) (< requested floor))
      (long (or (state/running-turn-start-cursor sid) current))
      requested)))

(defn- latest-replay-iteration
  "Highest 1-based iteration for the current turn in an atomically captured SSE
   replay. The ready frame sends this HEAD before the replay body, so a joining
   UI can state where the live turn is now while it backfills older frames."
  ^long [replay turn-id]
  (if-let [wanted (some-> turn-id
                          str)]
    (reduce (fn [^long latest event]
              (let [event-turn (some-> (get event "turn_id")
                                       str)
                    position (get event "iteration")]

                (if (and (= wanted event-turn) (number? position))
                  (max latest (long position))
                  latest)))
            0
            replay)
    0))

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

   `latest_iteration` is the replay's high-water position. It rides this first,
   flushed frame so a client paints iteration 420 immediately, then fills its
   details from the chronological replay without visibly counting 1…420.

   `is_live` is the required canonical verdict; protocol negotiation rejects a client
   or daemon that cannot exchange it.

   EVERY SSE endpoint emits it for EVERY session it serves — single-session and
   multiplexed alike — so no client has to special-case which endpoint it is
   attached to. Like every other frame it rides `sse/sse-frame`, i.e. it is an
   ordinary `id:`/`event:`/`data:` frame, not a bespoke encoding."
  [^OutputStream out sid cursor replay session]
  (let [tid
        (state/current-turn-id sid)

        latest-iteration
        (latest-replay-iteration replay tid)

        payload
        (gateway-contract/subscription-ready-event {:session-id sid
                                                    :cursor cursor
                                                    :current-turn-id tid
                                                    :is-live (some? tid)
                                                    :server-time-ms (util/now-ms)
                                                    :goal (get session "goal")
                                                    :agent-name (get session "agent_name")
                                                    :latest-iteration (when (pos? latest-iteration)
                                                                        latest-iteration)})]

    (.write out (.getBytes (sse/sse-frame (wire/canonical payload)) StandardCharsets/UTF_8))
    (.flush out)))

(defn- parse-multi-sids
  "Parse the `sids` query param of the multiplexed events endpoint: a comma
   list of `sid` or `sid:cursor` tokens (cursor defaults to 0). Returns
   `[[sid cursor] …]` for syntactically valid UUIDs. Keep missing sessions so
   reconnecting clients receive a deletion verdict instead of silently hanging.

   Each sid is parsed to a `java.util.UUID` — the SAME key type `path-sid`
   hands every other route — because the gateway registry is UUID-keyed. A
   string sid here registered the SSE sink under a GHOST string entry, so idle
   tabs never received queue or turn events until their next submit parsed the
   sid correctly.

   When the request carries a `Last-Event-ID` header AND resolves to exactly
   ONE sid, that header overrides the sole sid's cursor. This lets a NATIVE
   EventSource (browser / react-native-sse) whose reconnect carries only a
   single `Last-Event-ID` resume losslessly — `sids=<sid>` IS the single-session
   subscription, and there is no per-session route beside it.
   Multi-sid callers (the hand-rolled TUI mux) manage per-session cursors in the
   `sids=` param and never send `Last-Event-ID`, so they are unaffected: a single
   header cannot disambiguate N independent per-session seq counters."
  [request]
  (let [parsed
        (let [raw (get-in request [:query-params "sids"])]
          (when (seq raw)
            (->> (str/split raw #",")
                 (keep (fn [tok]
                         (let [[sid c] (str/split (str/trim tok) #":" 2)
                               sid (some-> (str/trim (str sid))
                                           parse-uuid)]

                           (when sid
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
   of [[sse-body]]. Every session registers the SAME non-blocking enqueue sink
   onto one bounded queue; this body thread is the only socket writer. A
   per-session `last-seq` guard dedups each session independently. Optionally
   carries the fleet status feed on that same connection, marked with
   `scope=fleet` so its independent sequence cannot advance a session cursor.
   Replays each session past its cursor, then drains live and heartbeats; a
   dead client unsubscribes every feed."
  [sid+cursors proxied? owner-pid & [include-fleet?]]
  (reify
    ring-protocols/StreamableResponseBody
      (write-body-to-stream [_ _ output-stream]
        (let [^OutputStream out
              output-stream

              outbound
              without-settled-picture

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
                  (state/unsubscribe! sid sub-id))
                (when include-fleet? (state/unsubscribe-fleet! sub-id)))

              close!
              (sse-closer out queue dead? unsubscribe-all!)

              sink
              (sse-sink queue close!)

              write!
              (fn [event]
                (if (= "fleet" (get event "scope"))
                  (do (.write out (.getBytes (sse/sse-frame event) StandardCharsets/UTF_8))
                      (.flush out))
                  (let [esid (str (get event "session_id"))]
                    (when (> (long (get event "seq")) (long (get @last-seqs esid Long/MIN_VALUE)))
                      (.write out
                              (.getBytes (sse/sse-frame (outbound event)) StandardCharsets/UTF_8))
                      (.flush out)
                      (swap! last-seqs assoc esid (long (get event "seq")))))))]

          (swap! instance/server-state (fn [st]
                                         (-> st
                                             (assoc :saw-client? true)
                                             (assoc-in [:sse-clients sub-id]
                                                       {:pid owner-pid :close! close!}))))
          (try (when proxied? (sse-proxy-pad! out))
               (doseq [[sid requested-cursor] sid+cursors]
                 (let [cursor (when (state/soul sid) (resolve-sse-cursor sid requested-cursor))
                       replay (when (some? cursor)
                                ;; Seed the guard before atomic registration.
                                (swap! last-seqs assoc (str sid) cursor)
                                (state/subscribe! sid sub-id sink cursor))
                       ;; Deletion may win between the first read and registration.
                       session (when (some? cursor) (state/soul sid))]

                   (if session
                     (do (sse-ready! out sid cursor replay session)
                         (doseq [event replay]
                           (write! event)))
                     ;; There is no ring left to replay. Advance past either cursor
                     ;; so a disconnected client cannot deduplicate the verdict away.
                     (write! (gateway-contract/stamp-session-event
                               {}
                               (str sid)
                               (inc (max 0 (long requested-cursor) (long (or cursor 0))))
                               (util/now-ms)
                               "session.deleted")))))
               (when include-fleet?
                 ;; Register before ready: a list read on ready overlaps the live
                 ;; feed, so a transition cannot fall between the two.
                 (state/subscribe-fleet! sub-id #(sink (assoc % "scope" "fleet")))
                 (write! {"schema" 1
                          "type" "subscription.ready"
                          "scope" "fleet"
                          "seq" 0
                          "ts" (util/now-ms)}))
               (pump-sse! out queue dead? write!)
               (catch Throwable _ nil)
               (finally (unsubscribe-all!)
                        (swap! instance/server-state update :sse-clients dissoc sub-id)
                        (maybe-stop-when-idle!)
                        (try (.close out) (catch Throwable _ nil))))))))

(defn- fleet-sse-body
  "SSE body carrying the FLEET's status deltas down ONE connection: which
   sessions started running, went idle, or parked on a human — never what
   happens INSIDE a turn.

   A session list used to learn all of that by re-reading its window on a timer,
   paying a whole window per tick to discover that usually nothing moved. Here a
   real change costs about a hundred bytes and arrives at once. The windowed read
   stays as the COLD read — first paint, reconnect, foreground — and is also the
   only resync: this stream has no replay and no cursor, so a gap heals with one
   ordinary read instead of a rewind."
  [proxied? owner-pid]
  (reify
    ring-protocols/StreamableResponseBody
      (write-body-to-stream [_ _ output-stream]
        (let [^OutputStream out
              output-stream

              sub-id
              (str (java.util.UUID/randomUUID))

              queue
              (ArrayBlockingQueue. (int SSE_QUEUE_CAP))

              dead?
              (volatile! false)

              close!
              (sse-closer out
                          queue
                          dead?
                          (fn []
                            (state/unsubscribe-fleet! sub-id)))

              sink
              (sse-sink queue close!)

              write!
              (fn [event]
                (.write out (.getBytes (sse/sse-frame event) StandardCharsets/UTF_8))
                (.flush out))]

          (swap! instance/server-state (fn [st]
                                         (-> st
                                             (assoc :saw-client? true)
                                             (assoc-in [:sse-clients sub-id]
                                                       {:pid owner-pid :close! close!}))))
          (try
            (when proxied? (sse-proxy-pad! out))
            ;; Attach before ready: the client's resync must overlap an active
            ;; subscription, otherwise a transition between its read and this
            ;; registration disappears. The queue keeps ready first on the wire.
            (state/subscribe-fleet! sub-id sink)
            (write!
              {"schema" 1 "type" "subscription.ready" "scope" "fleet" "seq" 0 "ts" (util/now-ms)})
            (pump-sse! out queue dead? write!)
            (catch Throwable _ nil)
            (finally (state/unsubscribe-fleet! sub-id)
                     (swap! instance/server-state update :sse-clients dissoc sub-id)
                     (maybe-stop-when-idle!)
                     (try (.close out) (catch Throwable _ nil))))))))

(defn- multi-events-handler
  "GET /v1/events?sids=a:10,b,c:3 carries every listed session on one SSE
   connection. Add scope=both to carry whole-machine status changes on that
   connection too. scope=fleet remains the fleet-only feed."
  [request]
  (let [proxied?
        (boolean (some #(get-in request [:headers %])
                       ["cf-ray" "cf-connecting-ip" "x-forwarded-for" "via"]))

        scope
        (some-> (get-in request [:query-params "scope"])
                str/trim)

        fleet?
        (= "fleet" scope)

        combined?
        (= "both" scope)

        sid+cursors
        (parse-multi-sids request)]

    (cond fleet? {:status 200
                  :headers sse/sse-headers
                  :body (fleet-sse-body proxied? (request-client-pid request))}
          (seq sid+cursors)
          {:status 200
           :headers sse/sse-headers
           :body (multi-sse-body sid+cursors proxied? (request-client-pid request) combined?)}
          :else (http/error-response 400 :bad-request "no valid sids"))))

;; /metrics (§6.5)

(defn- prometheus-text
  [snapshot]
  (let [series
        [[:turns-total "vis_turns_total" "counter"]
         [:turns-failed "vis_turns_failed_total" "counter"]
         [:cost-total "vis_turn_cost_usd_total" "counter"]
         [:duration-ms-total "vis_turn_duration_ms_total" "counter"]
         [:sessions-tracked "vis_sessions_tracked" "gauge"]
         [:turns-running "vis_turns_running" "gauge"]
         [:turns-executing "vis_turns_executing" "gauge"]
         [:turns-waiting "vis_turns_waiting" "gauge"] [:turns-queued "vis_turns_queued" "gauge"]
         [:turn-concurrency-limit "vis_turn_concurrency_limit" "gauge"]
         [:replay-events-retained "vis_replay_events_retained" "gauge"]
         [:env-cache-size "vis_env_cache_size" "gauge"]
         [:env-memory-pressure "vis_env_memory_pressure" "gauge"]
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
                    (let [value (get snapshot k 0)
                          value (if (boolean? value) (if value 1 0) value)]

                      (str "# TYPE " metric-name " " metric-type "\n" metric-name " " value "\n")))
                  series)))))

(defn- metrics-handler
  [request]
  (let [snapshot (merge (state/metrics-snapshot) (gateway-client-metrics))]
    (if (str/includes? (str (get-in request [:headers "accept"])) "application/json")
      (http/json-response snapshot)
      {:status 200
       :headers {"Content-Type" "text/plain; version=0.0.4"}
       :body (prometheus-text snapshot)})))

;; Route handlers (§5-§6)

(def ^:private openapi-answer
  ;; The document is a pure function of the built-in contract, so it is rendered
  ;; once per image: identical bytes for every caller, and a validator that can be
  ;; compared without hashing the body again.
  (delay (let [body (wire/json-str (openapi-contract/document))]
           {:body body :etag (str "\"" (subs (util/sha256-hex body) 0 32) "\"")})))

(defn- openapi-handler
  "GET /openapi.json — the built-in HTTP surface as an OpenAPI 3.1 document.

   Public, like `/docs`: generating a client against a gateway is what a caller
   does BEFORE it holds a token, and the document states only what the contract
   already publishes. Routes contributed by an extension are absent by design."
  [request]
  (let [{:keys [body etag]} @openapi-answer]
    (if (= etag (get-in request [:headers "if-none-match"]))
      {:status 304 :headers {"ETag" etag "Cache-Control" "no-cache"}}
      {:status 200
       :headers {"Content-Type" "application/json" "ETag" etag "Cache-Control" "no-cache"}
       :body body})))

(defn- health-handler
  [request]
  ;; `/healthz` is also the recovery rendezvous for a client that still knows
  ;; the stable token but found the registry missing. The orphan-retirement probe
  ;; suppresses that repair long enough to make its authenticated stop decision.
  (when-not (= "true" (get-in request [:headers "x-vis-suppress-registry-recovery"]))
    (ensure-self-registered!))
  (let [{:keys [token]}
        @instance/server-state

        supplied
        (get-in request [:headers "x-vis-gateway-secret"])]

    (http/json-response (assoc (status-map)
                          :status "ok"
                          :secret_match (= token supplied)))))

(defn- touch-client-lease!
  "Record that the client holding `client-id` was seen NOW.

   Refreshes an EXISTING lease only: a reaped or invented id never creates one, so
   the lease map stays exactly as big as `POST /v1/clients` made it. Writes at most
   once per `CLIENT_LEASE_TOUCH_MS`, so a busy client costs one map lookup per
   request and no `swap!` at all."
  [client-id now]
  (when (seq (str client-id))
    (let [lease (get-in @instance/server-state [:clients client-id])]
      (when (and lease
                 (or (some? (:pid lease))
                     (<= (- (long now) (long (or (:last-seen-at lease) (:connected-at lease) 0)))
                         (long instance/CLIENT_LEASE_TTL_MS)))
                 (> (- (long now) (long (or (:last-seen-at lease) (:connected-at lease) 0)))
                    (long CLIENT_LEASE_TOUCH_MS)))
        (swap! instance/server-state (fn [current]
                                       (if (contains? (:clients current) client-id)
                                         (assoc-in current [:clients client-id :last-seen-at] now)
                                         current)))))))

(defn- register-client-lease
  "Insert one opaque client lease while enforcing the process invariant: at
   most one lease per non-nil pid. Returns replacement count for observability."
  [clients client-id {:keys [pid] :as lease}]
  (if (nil? pid)
    {:clients (assoc clients client-id lease) :replaced 0}
    (let [stale-ids (persistent! (reduce-kv
                                   (fn [ids existing-id existing]
                                     (if (= pid (:pid existing)) (conj! ids existing-id) ids))
                                   (transient [])
                                   clients))]
      {:clients (assoc (reduce dissoc clients stale-ids) client-id lease)
       :replaced (count stale-ids)})))

(defn- client-register-handler
  [request]
  (let [{:strs [pid kind]}
        (http/body-json request)

        client-id
        (str (java.util.UUID/randomUUID))

        lease
        (let [now (util/now-ms)]
          {:pid pid :kind kind :connected-at now :last-seen-at now})

        replacement-stats
        (long-array 2)]

    ; [this registration, cumulative]
    (swap! instance/server-state (fn [st]
                                   (let [{:keys [clients replaced]}
                                         (register-client-lease (:clients st) client-id lease)

                                         total
                                         (+ (long (or (:client-replacements-total st) 0))
                                            (long replaced))]

                                     (aset-long replacement-stats 0 (long replaced))
                                     (aset-long replacement-stats 1 (long total))
                                     (-> st
                                         (assoc :saw-client? true
                                                :clients clients)
                                         (update :client-registrations-total (fnil inc 0))
                                         (assoc :client-replacements-total total)))))
    (let [replaced
          (aget replacement-stats 0)

          replacement-total
          (aget replacement-stats 1)]

      (when (and (pos? replaced)
                 (or (= 1 replacement-total) (zero? (long (mod replacement-total 100)))))
        (log-client-lease-warning! :client-lease-replaced
                                   {:replaced replaced
                                    :replacements-total replacement-total
                                    :leases (count (:clients @instance/server-state))})))
    (http/json-response
      {:client_id client-id :status (status-map) :extensions (extension-startup-status)})))

(defn- client-release-handler
  [request]
  (let [client-id (get-in request [:path-params :cid])]
    (swap! instance/server-state (fn [st]
                                   (if (contains? (:clients st) client-id)
                                     (-> st
                                         (update :clients dissoc client-id)
                                         (update :client-releases-total (fnil inc 0)))
                                     st)))
    (client-extensions/detach-owner! client-id)
    ;; Re-arm the lifecycle before judging it: the reaper is what evaluates refcount
    ;; shutdown, and a daemon whose reaper died must not become immortal just
    ;; because the sweep that would have noticed is gone.
    (ensure-idle-reaper!)
    (maybe-stop-when-idle!)
    (http/json-response {:released true :status (status-map)})))

(defn- status-handler
  [_]
  ;; Same belt as the release path: whoever is asking whether this daemon is idle
  ;; (`vis-agent gateway stop --if-idle`, the TUI, a health probe) also revives a
  ;; reaper that died, so the answer describes a daemon that can still act on it.
  (ensure-idle-reaper!)
  (http/json-response (assoc (status-map) :extensions (extension-startup-status))))

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
  (http/json-response {:stopping true :status (status-map)}))

(defn- seq-handler
  [request]
  (if-let [sid (http/path-sid request)]
    (http/json-response {:seq (state/current-seq sid)})
    (http/session-404 (get-in request [:path-params :sid]))))

(defn- events-since-handler
  [request]
  (if-let [sid (http/path-sid request)]
    {:status 200
     :headers {"Content-Type" "application/json"}
     :body (wire/canonical-json-str {"events" (state/events-since sid (sse-cursor request))})}
    (http/session-404 (get-in request [:path-params :sid]))))

;; Router and middleware

;; Pull declarative HTTP route contributions from `:gateway.slot/http-routes` whenever
;; the handler fingerprint changes; extensions never mutate the server. Embedded callers
;; may use `register-routes!`. Contributions define routes plus optional prefix, open URI
;; sets, custom auth/error handlers, form parsing and shutdown callback.
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

(defn- stop-route-contributions!
  [contribs]
  (doseq [{:keys [stop-fn]} contribs]
    (when stop-fn
      (try (stop-fn)
           (catch Throwable t
             (tel/log! :warn ["gateway: route contribution stop failed" (ex-message t)])))))
  nil)

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
  (boolean (:require-token? @instance/server-state)))

(defn- constant-time=?
  "Timing-safe comparison for secret strings. Plain `=` early-outs on the
   first differing byte, leaking token length/prefix through response timing
   once auth is enabled (non-loopback); `MessageDigest/isEqual` compares in
   constant time. nil-safe — a missing header never matches."
  [^String a ^String b]
  (boolean (and a b (MessageDigest/isEqual (util/utf8 a) (util/utf8 b)))))

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
        (let [uri (str (:uri request))
              open? (or (= "/healthz" uri)
                        ;; The embedded docs site is public content (the vis.dev
                        ;; pages) — viewable on the tunnel without the token.
                        (= "/docs" uri)
                        (str/starts-with? uri "/docs/")
                        ;; The API description is public for the same reason: a
                        ;; client is GENERATED against a gateway before it holds
                        ;; a token.
                        (= "/openapi.json" uri)
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
                                           (some-> (get-in request
                                                           [:headers "x-vis-gateway-secret"])
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
                (http/error-response 401 :unauthorized "missing or invalid bearer token"))))))))

(def ^:private protocol-open-uris
  "Paths that answer EVEN an unsupported client. They are HOW a peer learns the
   gateway's protocol and reads the mismatch verdict, so refusing them would
   leave an old client with nothing but an opaque failure."
  #{"/healthz" "/readyz" "/v1/capabilities" "/openapi.json"})

(defn- wrap-protocol
  "Wire-protocol gate (§3). API clients must advertise a compatible version.
   A contributed browser route may name exact `:protocol-open-uris` because native
   navigation and EventSource cannot attach API protocol headers."
  [handler contribs]
  (fn [request]
    (let [uri
          (str (:uri request))

          browser-uri?
          (some #(contains? (or (:protocol-open-uris %) #{}) uri) contribs)]

      (if (or (contains? protocol-open-uris uri) (str/starts-with? uri "/docs") browser-uri?)
        (handler request)
        (let [v (protocol/gateway-verdict request)]
          (if (:is-compatible v)
            (handler request)
            (let [{:keys [title summary remedy]} (protocol/explain v)]
              (http/json-response 426
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
           ;; "No AI provider is usable" keeps its TYPE on the wire. Flattened into
           ;; :engine-error no caller could tell the most likely first-run state
           ;; from a real crash, so `vis-agent tui` printed a stack trace instead
           ;; of opening the provider manager. Answer with the ORIGINAL message —
           ;; whatever wrapped it on the way here says something generic.
           (if-let [no-provider (config/no-provider-ex t)]
             (http/error-response 503
                                  config/no-provider-error-type
                                  (or (ex-message no-provider) "No AI provider is configured yet."))
             (http/error-response 500 :engine-error (or (ex-message t) "internal error")))))))

(def ^:private cors-allow-methods "GET, POST, PUT, PATCH, DELETE, OPTIONS")

(defn- cors-headers
  "CORS headers for a cross-origin browser request. The bearer token is the
   real authorization gate (§3); CORS only tells the browser the response is
   readable. We echo the request Origin — so `Access-Control-Allow-Credentials`
   is legal for cookie-bearing browser clients — and fall back to `*` for callers
   that send no Origin (curl, native clients)."
  [request]
  (let [origin
        (get-in request [:headers "origin"])

        req-headers
        (get-in request [:headers "access-control-request-headers"])]

    (cond-> {"Access-Control-Allow-Methods" cors-allow-methods
             "Access-Control-Allow-Headers" (or req-headers
                                                "Authorization, Content-Type, X-Vis-Gateway-Secret")
             ;; Without this a browser fetch can read the BODY but not the `ETag`
             ;; header, so the app could never send `If-None-Match` and every session
             ;; list poll would re-download the whole fleet.
             "Access-Control-Expose-Headers" "ETag"
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

(defn- docs-handler
  "Serve the embedded docs site from `resources/vis-docs/*.md`. [[docs/handle]] owns
   `/docs`, `/docs/<slug>` and `/docs/assets/**` and re-reads the markdown on every
   request, so an edited doc shows on a browser refresh without a gateway restart. A
   docs path it does not own is a 404."
  [request]
  (or (docs/handle request) (http/error-response 404 :not-found "no such doc")))

(def ^:private server-handlers
  "Handlers for the routes this namespace owns: health, metrics, docs, OpenAPI,
   administration, client leases and event streams, keyed by the gateway contract's
   `[method path]`."
  {[:get "/healthz"] health-handler
   [:get "/readyz"] health-handler
   [:get "/openapi.json"] openapi-handler
   [:get "/metrics"] metrics-handler
   [:get "/v1/events"] multi-events-handler
   [:post "/v1/clients"] client-register-handler
   [:delete "/v1/clients/:cid"] client-release-handler
   [:get "/v1/admin/status"] status-handler
   [:post "/v1/admin/stop"] stop-handler
   [:get "/v1/sessions/:sid/events-since"] events-since-handler
   [:get "/v1/sessions/:sid/seq"] seq-handler
   [:get "/docs"] docs-handler
   [:get "/docs/*path"] docs-handler})

(def ^:private route-handlers
  "Every handler map the built-in router binds. Each operation in
   [[gateway-contract/route-table]] belongs to exactly one of them."
  [server-handlers council-api/handlers decisions-api/handlers devices-api/handlers fs-api/handlers
   mcp-api/handlers projects-api/handlers providers-api/handlers sessions-api/handlers
   settings-api/handlers speech-api/handlers transcripts-api/handlers turns-api/handlers
   views-api/handlers])

(defn- route-precedence
  "Sort key for a route path. At the first segment where two paths differ, a fixed
   segment sorts before a path parameter and a parameter before a catch-all. Reitit
   matches conflicting routes in order, so `/v1/projects/overview` is tried before
   `/v1/projects/:pid` and only a real project id reaches the parameter route."
  [path]
  (str/join "/"
            (map (fn [segment]
                   (case (first segment)
                     \:
                     "1"

                     \*
                     "2"

                     (str "0" segment)))
                 (str/split path #"/"))))

(defn- path-shape
  "`path` without its parameter names. Routes of the same shape match the same
   requests, so no order can tell them apart."
  [path]
  (str/replace path #"/([:*])[^/]+" "/$1"))

(defn- route-conflicts!
  "Reitit `:conflicts` handler. Built-in routes are ordered by [[route-precedence]], so
   a conflict between two built-in routes of different shape is resolved by that
   order. Any other conflict, including one involving a contributed route, fails the
   router build as it does by default."
  [conflicts]
  (let [built-in
        (into #{} (map :path) gateway-contract/route-table)

        ordered?
        (fn [path other]
          (and (built-in path) (built-in other) (not= (path-shape path) (path-shape other))))

        unresolved
        (into {}
              (keep (fn [[[path :as route] others]]
                      (when-let [left (not-empty (into #{}
                                                       (remove (fn [[other]]
                                                                 (ordered? path other)))
                                                       others))]
                        [route left])))
              conflicts)]

    (when (seq unresolved) (reitit-exception/fail! :path-conflicts unresolved))))

(defn- built-in-routes
  "Reitit route data for every operation in [[gateway-contract/route-table]], bound to
   its handler from `handler-maps` and ordered by [[route-precedence]]. Throws when the
   maps repeat an operation, leave one unbound or serve one the contract does not
   declare, so the router cannot drift from the contract."
  [handler-maps]
  (let [handlers
        (apply merge handler-maps)

        declared
        (gateway-contract/route-methods)

        repeated
        (sort (keep (fn [[operation n]]
                      (when (< 1 n) operation))
                    (frequencies (mapcat keys handler-maps))))

        unbound
        (sort (remove handlers declared))

        undeclared
        (sort (remove declared (keys handlers)))]

    (when (or (seq repeated) (seq unbound) (seq undeclared))
      (throw (ex-info
               "gateway: route handlers do not match the gateway contract"
               {:repeated (vec repeated) :unbound (vec unbound) :undeclared (vec undeclared)})))
    (->> gateway-contract/route-table
         (sort-by (comp route-precedence :path))
         (mapv (fn [{:keys [path operations]}]
                 [path
                  (into {}
                        (map (fn [method]
                               [method (get handlers [method path])]))
                        (keys operations))])))))

(defn- router
  "Reitit router for every built-in contract route plus each contribution's routes."
  [^String token contribs]
  (rr/router (into (built-in-routes route-handlers)
                   (keep (fn [{:keys [routes]}]
                           (when routes
                             (try (routes token)
                                  (catch Throwable t
                                    (tel/log! :error
                                              ["gateway: route contribution failed" (ex-message t)])
                                    nil))))
                         contribs))
             {:conflicts route-conflicts!}))

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
      (let [uri (str (:uri request))
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
  (let [mp-handler (ring-multipart/wrap-multipart-params handler
                                                         {:store (multipart-ba/byte-array-store)})]
    (fn [request]
      (let [uri (str (:uri request))
            multipart? (some (fn [{:keys [prefix multipart?]}]
                               (and multipart? prefix (str/starts-with? uri prefix)))
                             contribs)]

        (if multipart? (mp-handler request) (handler request))))))

(defn- wrap-client-lease
  "Every request a client makes is proof that client is still there: refresh its
   lease. `X-Vis-Client-Id` is what a client stamps on every request once
   `POST /v1/clients` gave it one.

   This is the ONLY liveness a remote lease has - it carries no pid this daemon
   could look up - and it is what lets `CLIENT_LEASE_TTL_MS` retire a phone or a
   `--gateway` CLI that vanished mid-flight instead of counting it as a client
   forever. Innermost wrapper on purpose: an unauthenticated or protocol-refused
   request never reaches it, so no stranger can keep a lease warm."
  [handler]
  (fn [request]
    (touch-client-lease! (get-in request [:headers "x-vis-client-id"]) (util/now-ms))
    (handler request)))

(defn- app
  [^String token contribs]
  (-> (rr/ring-handler
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
                            (or (some
                                  (fn [{:keys [prefix on-not-found]}]
                                    (when (and prefix on-not-found (str/starts-with? uri prefix))
                                      (on-not-found request)))
                                  contribs)
                                (http/error-response 404 :not-found "no such route"))))
             :method-not-allowed
             (fn [_]
               (http/error-response 405 :method-not-allowed "method not allowed"))})))
      (wrap-client-lease)
      (wrap-auth token contribs)
      ;; Runs BEFORE the token gate: an out-of-date client deserves the version
      ;; verdict, not a 401 that hides it.
      (wrap-protocol contribs)
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
  (when-let [{:keys [token]} @instance/server-state]
    (let [contribs (vec (contributions))]
      (reset! live-app {:handler (app token contribs)
                        :fp (routes-fingerprint)
                        :contribs contribs})))
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

;; Lifecycle

(defonce ^:private toggle-persist-listener-installed? (atom false))

(defn- install-toggle-persistence!
  "Hydrate feature toggles from the `toggles:` slot of the merged YAML config
   and install a listener that writes every change back — through
   `config/save-toggles!`, which touches ONLY the `toggles:` block of the machine
   store: handing the MERGED config to `save-config!` folded the hand-written and
   project tiers into `~/.vis/state.yml`, where the machine tier then outranked
   them. Mirrors the TUI's wiring in `channel-tui/screen.clj` so a toggle flipped
   from any gateway client survives a gateway restart - without this, only
   TUI-hosted processes ever persisted toggles. Idempotent: hydration re-runs
   harmlessly; the save listener installs once per process."
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
           (config/save-toggles! (toggles/snapshot))))
       (when (compare-and-set! toggle-persist-listener-installed? false true)
         (toggles/add-listener!
           (fn [_event]
             (try (config/save-toggles! (toggles/snapshot))
                  (catch Throwable t
                    (tel/log!
                      {:level :warn :id ::toggle-persist-failed :data {:error (ex-message t)}}
                      "Toggle persistence failed; in-memory value still applies."))))))
       (catch Throwable t
         (tel/log! {:level :warn :id ::toggles-hydrate-failed :data {:error (ex-message t)}}
                   "Toggle hydration from config failed; defaults stand."))))

(defn local-handler
  "Build the SDK handler for an owned stdio engine, without opening HTTP listeners.
   The caller owns process lifetime and must select an isolated database."
  []
  (install-toggle-persistence!)
  (app nil []))

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

    (let [outcome (try {:server (jetty/run-jetty handler opts)}
                       (catch Throwable t
                         (if (and (bind-failure? t) (< (util/now-ms) (long deadline-ms)))
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
    (let [^ServerConnector primary
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

(def ^:private GZIP_MIN_BYTES
  "Response floor below which gzip is not worth its own framing (1 KiB). Jetty's
   own default is 32 bytes, which spends a deflate on envelopes smaller than the
   header it adds."
  1024)

(defn- gzip-handler
  "A `GzipHandler` wrapping the gateway's handler, so JSON bodies cross the wire
   compressed.

   The transcript surface is what makes this worth having: a session's
   `/v1/sessions/:sid/transcript` is a single unpaginated envelope holding every
   form's `stdout` verbatim, and command output is the most redundant payload the
   gateway serves — repeated file paths, repeated classpaths, repeated framing.
   Measured on a real 27.5 MB transcript, deflate returns it in 6.8 MB (4.0x), and
   the `stdout` inside it alone compresses 7.2x. A phone on Tailscale pays that
   difference directly.

   `text/event-stream` MUST NOT be compressed. Jetty already ships it in
   `excludedMimeTypes`, but the exclusion is re-stated here because it is a
   correctness invariant of the live surface rather than a tuning preference: the
   deflater buffers, and a buffered SSE body is one that stops arriving as it
   happens — `client.clj` disables compression on its own side for exactly this
   reason. Re-stating it costs nothing and keeps a Jetty default change from
   silently freezing every live view.

   Note this also enables REQUEST inflation for `Content-Encoding: gzip` uploads,
   which the gateway simply did not accept before."
  []
  (doto (GzipHandler.)
    (.setMinGzipSize (int GZIP_MIN_BYTES))
    (.addExcludedMimeTypes (into-array String ["text/event-stream"]))))

(defn- gateway-configurator
  "The adapter's `:configurator` — the seam for `Server` tuning no Ring option
   expresses. A nil `mirror-port` skips the loopback mirror above.

   Clearing `stopAtShutdown` is not optional: `ring.adapter.jetty9` turns it ON when
   it builds the `Server`, which registers Jetty's OWN JVM shutdown hook to `.stop`
   it. This namespace's hook is meant to be the only shutdown path — it cancels and
   then DRAINS in-flight turns before releasing the socket — and a second hook racing
   it would guillotine exactly the mid-turn work that drain exists to save."
  [mirror-port]
  (let [mirror (some-> mirror-port
                       loopback-mirror-configurator)]
    (fn [^Server server]
      (.setStopAtShutdown server false)
      (.insertHandler server (gzip-handler))
      (when mirror (mirror server)))))

(defn- preload-voice-model!
  "Load the installed transcription model on a BACKGROUND thread, so the first
   recording of the day decodes instead of waiting for ~640 MB of model (#275).
   Answers the decision it made: `:off` when the switch is off, `:started` otherwise.

   Never on the boot thread, and only once the port is already serving: a gateway
   must be ANSWERABLE before it is fast. [[speech/preload-transcription!]] reads the
   asset manifest first, so a machine that never installed a model neither loads the
   speech backend nor downloads anything for it."
  []
  (if-not (toggles/enabled? "speech_preload_model")
    :off
    (do (future (try (speech/preload-transcription!)
                     (catch Throwable t
                       (tel/log! :warn ["gateway: voice model preload failed" (ex-message t)]))))
        :started)))

(defn- preload-decision-models!
  "Warm installed versions after Jetty listens. Other versions remain cold; no downloads."
  []
  (let [enabled (config/extension-env-value "VIS_DECISION_WARMUP")]
    (when-not (contains? #{"false" "off" "0"}
                         (some-> enabled
                                 str/lower-case))
      (let [installed (into #{}
                            (keep #(when (get % "installed") (get % "model_ref")))
                            (decisions/models-status))
            extra (config/extension-env-value "VIS_DECISION_WARM_MODELS")
            requested (distinct (cons "laya-typed-decisions"
                                      (remove str/blank?
                                        (map str/trim (str/split (or extra "") #",")))))
            ready (filterv installed requested)]

        (when (seq ready)
          (future (doseq [id ready]
                    (try (decisions/warm! id)
                         (catch Throwable t
                           (tel/log! :warn
                                     ["gateway: decision model preload failed" id
                                      (ex-message t)]))))))))))

(defn start!
  "Start the gateway on the Jetty 12 core adapter with virtual threads.
   Returns `{:port :host :token-file}`. Throws when already running.
   Safe to call from any host process - the daemon (`vis-agent gateway start`), a TUI
   run, or an embedded caller."
  ([] (start! {}))
  ([{:keys [port host token-file require-token? db managed? advertise]}]
   (when @instance/server-state
     (throw (ex-info "gateway already running" {:type :gateway/already-running})))
   (wiring/install!)
   (let [port
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
         (reset! instance/server-state {:token token
                                        :require-token? require-token?
                                        :managed? (boolean managed?)
                                        :started-at-ms (util/now-ms)})

         _
         (rebuild-app!)

         ;; Load the persistence backend NOW, single-threaded, so the
         ;; first DB touch never happens on N concurrent request threads.
         _
         (state/warm-db!)

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
             (push/set-gateway-id! (instance/gateway-instance-id db host port))
             (state/add-event-tap! ::push push/on-event!)
             ;; Human-input bridge: a `request-human-input!` raised inside a
             ;; session becomes a session event, so the companion app sees the
             ;; blocked run live (and the push tap above alerts the phone).
             (gw-view/install!))

         server
         (try
           (start-jetty! serving-handler
                         {:port port
                          :host host
                          :join? false
                          :virtual-threads? true
                          :send-server-version? false
                          :configurator (gateway-configurator (when mirror-loopback? port))}
                         (+ (util/now-ms) 6000))
           (catch Throwable t (reset! instance/server-state nil) (reset! live-app nil) (throw t)))]

     (when-not (= host DEFAULT_HOST)
       (tel/log! :warn ["gateway: binding to non-loopback host" host]))
     (reset! instance/server-state
       {:server server
        :port port
        :host host
        :advertise advertise
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
        :started-at-ms (util/now-ms)
        :saw-client? false})
     (decision-cache/enable!)
     ;; The gateway's own control-plane port is reserved so a jailed child can NEVER reach
     ;; it through the proxy, even though loopback egress is allowed by default (SSRF floor).
     (try (gateway-sandbox/set-reserved-ports! [port]) (catch Throwable _ nil))
     (try (discovery/register-self! db {:port port :host host :secret token})
          (catch Throwable t
            (tel/log! :warn ["gateway: registry self-registration failed" (ex-message t)])))
     (swap! instance/server-state assoc :stop-improve! (improve-review/start! db))
     (when managed? (ensure-idle-reaper!))
     (tel/log! :info
               ["gateway: listening" (str host ":" port)
                (if require-token? "auth: bearer token" "auth: disabled (loopback)")
                (if managed? "lifecycle: managed" "lifecycle: foreground")])
     (preload-voice-model!)
     (swap! instance/server-state assoc :decision-warmup (preload-decision-models!))
     {:port port
      :host host
      :token-file (str path)
      :require-token? require-token?
      :managed? (boolean managed?)})))

(def ^:private GRACEFUL_DRAIN_MS
  "Max time `stop!` waits for in-flight turns to finish before forcing Jetty
   down, so a SIGTERM / `vis-agent gateway stop` landing mid-turn lets active work
   complete instead of being cut off. Only ever waits when turns are actually
   running (the refcount-idle stop path already has zero)."
  8000)

(defn- await-turns-drained!
  "Block up to `GRACEFUL_DRAIN_MS` for running turns to reach zero, polling
   every 100ms. Returns the residual running-turn count (0 = fully drained)."
  []
  (let [deadline (+ (util/now-ms) (long GRACEFUL_DRAIN_MS))]
    (loop []

      (let [n (long (running-turn-count))]
        (if (or (zero? n) (>= (util/now-ms) deadline)) n (do (Thread/sleep 100) (recur)))))))

(defn stop!
  "Stop the gateway server if running. Idempotent."
  []
  (when-let [{:keys [^Server server db stop-improve!]} @instance/server-state]
    (when stop-improve! (stop-improve!))
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
    (when-let [warmup (:decision-warmup @instance/server-state)]
      (future-cancel warmup))
    (try (decision-jobs/stop!)
         (catch Throwable t
           (tel/log! :warn ["gateway: decision training shutdown failed" (ex-message t)])))
    (try (decision-cache/shutdown!)
         (catch Throwable t
           (tel/log! :warn ["gateway: decision session shutdown failed" (ex-message t)])))
    (try (.stop server) (catch Throwable _ nil))
    (stop-route-contributions! (:contribs @live-app))
    ;; The live-view bridge holds patches for up to one flush window. A gateway
    ;; going away must publish what the engine already accepted, not swallow it.
    (try (gw-view/uninstall!) (catch Throwable _ nil))
    ;; Kill every session's background resources (background `shell` children, REPLs)
    ;; BEFORE the JVM goes away — their :stop-fn thunks live only in this
    ;; process; once it exits the children reparent to init and leak.
    (try (resources/shutdown!) (catch Throwable _ nil))
    (try (discovery/deregister-self! db) (catch Throwable _ nil))
    (reset! instance/server-state nil)
    (reset! live-app nil)
    ;; Unpark `serve-main!` so the daemon process ends after a refcount/admin
    ;; stop. Without this the JVM stayed parked on a dead promise: every TUI
    ;; close-then-reopen leaked one idle daemon process (port + registry were
    ;; released, but nothing terminated the process).
    (deliver serve-exit true))
  nil)

(defn running? [] (some? @instance/server-state))

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
          (let [fv
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
     (let [interactive?
           (interactive-terminal?)

           installed
           (reduce
             (fn [acc ^String nm]
               (try
                 (let [prev
                       (sun.misc.Signal/handle
                         (sun.misc.Signal. nm)
                         (reify
                           sun.misc.SignalHandler
                             (handle [_ sig]
                               (let [^sun.misc.Signal s sig
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
  [{:keys [port host token-file require-token? db managed? pair? advertise]}]
  ;; Profile the daemon into its own JFR file when VIS_JFR is inherited from the
  ;; client that spawned us (idempotent with the -main call for direct callers).
  (try ((requiring-resolve 'com.blockether.vis.internal.jfr/maybe-start!) "gateway")
       (catch Throwable _ nil))
  (let [;; `--pair` is a request for PHONE access, so it selects the bind. With no
        ;; explicit `--host` the loopback default printed a QR for an address
        ;; nothing listened on — the failure landed on the phone, looking like a
        ;; broken app. Bind every interface, which is what the QR's `alt=` hosts
        ;; promise; it is non-loopback, so `start!` forces the bearer token.
        auto-host
        (when (and pair? (str/blank? host)) (pairing/pair-bind-host))

        _
        (reset! extension-startup {:stage "initializing"})

        {:keys [port host token-file require-token?]}
        (start! {:port (some-> port
                               parse-long)
                 :host (or auto-host host)
                 :token-file token-file
                 :require-token? require-token?
                 :db db
                 :managed? managed?
                 :advertise advertise})

        ;; `config/init-cli!` has already redirected System/out AND `*out*` into
        ;; the gateway's role-labelled file under ~/.vis/logs/, so a plain
        ;; `println` here is invisible — the daemon looked completely silent.
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
                               :advertise advertise
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
    (prepare-startup-extensions!)
    @serve-exit
    (System/exit 0)))
