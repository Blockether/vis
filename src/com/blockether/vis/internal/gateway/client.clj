(ns com.blockether.vis.internal.gateway.client
  "HTTP/SSE client for the long-lived gateway daemon.

   Interactive channels call this facade instead of `gateway.state` directly. It
   discover-or-starts the one daemon for the current DB, then speaks the same
   HTTP/SSE API every other client uses. This is the thin-client half of the
   gateway-daemon plan: token refresh, turn execution, and live streaming happen
   in ONE process.

   WHICH daemon is a policy of this namespace: normally the one this machine
   manages for the current DB, or — through `connect-remote!` (the `--gateway`
   flag / `VIS_GATEWAY_URL`) — a gateway on another machine, attached to over HTTP
   and never spawned, restarted or stopped from here."
  (:require [babashka.http-client :as http]
            [clojure.string :as str]
            [com.blockether.vis.internal.cancellation :as cancellation]
            [com.blockether.vis.internal.config :as config]
            [com.blockether.vis.internal.gateway.discovery :as discovery]
            [com.blockether.vis.internal.gateway.protocol :as protocol]
            [com.blockether.vis.internal.gateway.wire :as wire])
  (:import (java.io BufferedReader InputStream InputStreamReader)
           (java.net URI URLEncoder)
           (java.nio.charset StandardCharsets)
           (java.util.concurrent.locks ReentrantLock)))

(def ^:private DEFAULT_PORT 7890)

(def ^:private DEFAULT_HOST "127.0.0.1")

(def ^:private health-probe-timeout-ms 1500)

;; A cold GraalPy initialization exceeded 40 seconds on the reported WSL host. The
;; catalog request runs off the UI thread, but its transport must survive that first load.
(def ^:private slash-catalog-timeout-ms 120000)

(def ^:private occupied-port-registry-wait-ms 3000)

(defonce ^:private http-client
  (delay
    ;; HTTP/1.1 + NO accept-encoding on purpose: the loopback daemon streams
    ;; plain `text/event-stream`; gzip or HTTP/2 framing would buffer the SSE
    ;; body and defeat BOTH the line reader and the idle watchdog in
    ;; `open-sse-events!`. Same babashka.http-client stack svar and every
    ;; provider already use — one HTTP client library across the codebase.
    (http/client {:follow-redirects :normal
                  :connect-timeout 2000
                  :version :http1.1
                  :request {:headers {"accept" "*/*"}}})))

(defonce ^:private cached-entry (atom nil))

(defonce ^:private ensure-locks
  ;; One slow-path lock per canonical DB. A process normally touches one DB;
  ;; retaining the lock ensures no waiter can race a newly-created replacement.
  (atom {}))

(defn- ensure-lock-for
  ^ReentrantLock [db]
  (let [key (discovery/registry-key db)]
    (get (swap! ensure-locks update key #(or % (ReentrantLock.))) key)))

(defn- call-with-ensure-lock
  [db f]
  (let [^ReentrantLock lock (ensure-lock-for db)]
    (.lock lock)
    (try (f) (finally (.unlock lock)))))
;; Freshness debounce: `ensure-gateway!` verifies the daemon with a full HTTP
;; GET /healthz probe on EVERY call. The TUI footer/poll loop calls it dozens of
;; times a second, so that doubled every gateway request (probe + real call) and
;; dominated client-side allocation (reflective interop around the old raw
;; transport response and futures) on the render JVM. The JSON layer is charred
;; (reflection-free); transport interop was the churn. We keep the belt-and-suspenders probe but only re-run it once per
;; `entry-probe-ttl-ms`; within the window a cheap pid-liveness check suffices.
(def ^:private entry-probe-ttl-ms 4000)

(defonce ^:private entry-fresh-until-ns (atom 0))

(defonce ^:private client-id (atom nil))

(defonce ^:private release-hook-installed? (atom false))

(defonce ^:private subscriptions (atom {}))

(defonce ^:private client-finalizing? (atom false))

;; Multiplexed subscription: ONE SSE connection for MANY sessions.
;;
;; A channel watching N sessions previously opened N SSE sockets (+N client
;; futures +N server heartbeat threads). `mux-subscribe!` instead folds every
;; watched session down a SINGLE process-wide connection to `/v1/events?sids=…`,
;; demuxed by each event's `:session_id`. Opening/closing a tab just edits the
;; session set and reconnects (resuming each session from its advanced cursor).

(defonce ^:private mux
  ;; {:subs {sid {:sinks {sub-id fn} :cursor-atom atom<long>}}
  ;;  :epoch long :future f :stream in}
  (atom {:subs {} :epoch 0 :future nil :stream nil}))

(def ^:private client-label
  "How this process names itself in the version handshake — what the mismatch
   panel prints as the client half."
  "vis")

(defonce ^:private gateway-handshake*
  ;; The daemon's advertised {:protocol :min-client :min-gateway :version},
  ;; captured from the /healthz probe every attach already pays for. nil until
  ;; the first probe; all-nil fields when the daemon is too old to advertise.
  (atom nil))

(defonce ^:private stale-bounce-attempted?
  ;; One version-driven restart per process, ever: see [[bounce-stale-daemon!]].
  (atom false))

(defn- db-target [] (config/resolve-db-spec))

(defn- enc [x] (URLEncoder/encode (str x) StandardCharsets/UTF_8))

(def ^:private REMOTE_URL_ENV "VIS_GATEWAY_URL")

(def ^:private REMOTE_TOKEN_ENV "VIS_GATEWAY_TOKEN")

(defonce ^:private remote-gateway*
  ;; The gateway this process drives INSTEAD of the daemon it manages for the
  ;; local DB, or nil for that daemon. `::unset` means the environment has not
  ;; been read yet: `remote-gateway` runs on every `ensure-gateway!` (dozens of
  ;; times a second under the TUI), so the env lookup resolves exactly once.
  (atom ::unset))

(defn- remote-scheme-port
  "Port for a `--gateway` value that named none: the HTTPS default for a TLS
   endpoint (a tunnel or reverse proxy), else the standard gateway port."
  [scheme]
  (if (= "https" scheme) 443 DEFAULT_PORT))

(defn- remote-entry
  "The daemon entry for gateway `url`, or nil when `url` is blank.

   `url` is a host (`10.0.0.5`), a host:port (`10.0.0.5:7890`) or a full
   `http(s)://host[:port][/prefix]`; a bare value is plain HTTP on the standard
   port. `token` is that gateway's bearer token, and nil is legitimate: a daemon
   bound to loopback runs auth-free, so an SSH tunnel needs the host alone."
  [url token]
  (when-let [raw (not-empty (str/trim (str url)))]
    (let [trimmed (str/replace raw #"/+$" "")
          absolute
          (if (re-find #"^[A-Za-z][A-Za-z0-9+.-]*://" trimmed) trimmed (str "http://" trimmed))
          ^URI uri (try (URI. absolute) (catch Exception _ nil))
          scheme (when uri
                   (some-> (.getScheme uri)
                           str/lower-case))
          host (when uri (not-empty (.getHost uri)))
          declared-port (long (if uri (.getPort uri) -1))
          prefix (str/replace (str (when uri (.getRawPath uri))) #"/+$" "")]

      (when-not (and host (contains? #{"http" "https"} scheme))
        (throw (ex-info (str "not a gateway address: "
                             (pr-str (str url))
                             ". Use HOST, HOST:PORT or http(s)://HOST[:PORT].")
                        {:type :gateway/invalid-remote-url :url (str url) :vis/user-error true})))
      (let [port (if (pos? declared-port) declared-port (remote-scheme-port scheme))]
        {:base-url (str scheme "://" host ":" port prefix)
         :host host
         :port port
         :secret (not-empty (str/trim (str token)))
         :remote? true}))))

(defn connect-remote!
  "Aim EVERY gateway call in this process at the gateway at `url` (the `--gateway`
   flag, `VIS_GATEWAY_URL`) instead of the daemon this machine manages: attach over
   HTTP, and never spawn, restart or stop it — a gateway on another machine is not
   ours to run. Returns the target entry, or nil when `url` is blank (stay local)."
  [{:keys [url token]}]
  (reset! remote-gateway* (remote-entry url token)))

(defn remote-gateway
  "The remote gateway target for this process, or nil when the locally managed
   daemon owns the work. `connect-remote!` wins; otherwise `VIS_GATEWAY_URL` and
   `VIS_GATEWAY_TOKEN` are read exactly once."
  []
  (let [current @remote-gateway*]
    (if (identical? ::unset current)
      (reset! remote-gateway* (remote-entry (System/getenv REMOTE_URL_ENV)
                                            (System/getenv REMOTE_TOKEN_ENV)))
      current)))

(defn- base-url
  [{:keys [host port] :as entry}]
  (or (:base-url entry) (str "http://" (or host DEFAULT_HOST) ":" (or port DEFAULT_PORT))))

(defn- gw-send!
  "Perform a babashka.http-client request against gateway `entry`. `method` is a
   verb string (\"GET\"/\"POST\"/\"PATCH\"/\"DELETE\"/…); `opts` may carry `:body`
   (serialized to JSON) and `:as` ∈ #{:string :bytes :stream} (default :string).
   `:throw false`, so 4xx/5xx come back as a response map (callers branch on
   `:status`) directly. A :stream request
   asks for `text/event-stream` with compression disabled so the SSE body stays
   byte-live for the line reader + idle watchdog."
  [{:keys [secret remote?] :as _entry} method path
   {:keys [body as timeout-ms headers] :or {as :string timeout-ms 30000}}]
  (http/request
    (cond-> {:client @http-client
             :method (keyword (str/lower-case method))
             :uri (str (base-url _entry) path)
             :timeout timeout-ms
             :throw false
             :as as
             :headers (cond-> (merge (protocol/client-headers client-label)
                                     headers
                                     {"Accept"
                                      (if (= as :stream) "text/event-stream" "application/json")})
                        ;; A remote client owns no process on the gateway's machine and the
                        ;; daemon reaps every lease whose pid is dead THERE, so a remote call
                        ;; must not claim one (server-side `request-client-pid`).
                        (not remote?)
                        (assoc "X-Vis-Client-Pid" (str (discovery/current-pid)))

                        ;; The lease this process holds. A REMOTE lease carries no pid
                        ;; the daemon could look up, so this header is the only
                        ;; liveness it has: without it, a client that vanished
                        ;; mid-flight counted as a client until the daemon died.
                        (some? @client-id)
                        (assoc "X-Vis-Client-Id" (str @client-id))

                        ;; One secret, both carriers: `Authorization` is what a token-gated
                        ;; (non-loopback) gateway checks, `X-Vis-Gateway-Secret` is what
                        ;; `/healthz` echoes back as `secret_match`. Blank means the target
                        ;; is an auth-free loopback daemon — send neither.
                        (seq (str secret))
                        (assoc "Authorization"
                          (str "Bearer " secret) "X-Vis-Gateway-Secret"
                          (str secret))

                        (= as :stream)
                        (assoc "Accept-Encoding" "identity"))}
      (some? body)
      (-> (assoc :body (wire/json-str body))
          (assoc-in [:headers "Content-Type"] "application/json")))))

(defn- parse-json-body [^String body] (or (wire/parse-json body) {}))

(defn- note-handshake!
  "Record the daemon's advertised version contract from a `/healthz` body — the
   probe EVERY attach already pays for, so compatibility costs no extra round
   trip. A daemon without a `protocol` block records all-nils and is rejected.
   Returns the body unchanged."
  [body]
  (reset! gateway-handshake* (protocol/wire->handshake (get body "protocol")))
  body)

(defn compatibility
  "This client's verdict on the daemon it last probed — the SAME pure comparison
   the gateway runs on us ([[protocol/verdict]]), so the two halves never
   disagree about which one is out of date."
  []
  (protocol/client-verdict client-label @gateway-handshake*))

(defn- lifecycle-protocol-headers
  "Let local status/stop cross ONLY a gateway-too-old boundary.

   `/healthz` already identified the peer and remains protocol-open. Its advertised
   protocol is safe to echo as this request's minimum for two lifecycle routes: the
   status shape is read fail-closed by [[daemon-idle?]], and stop either follows that
   idle proof or an explicit human command. Session data still goes through the real
   minimum and is refused. This narrow bridge is what lets a freshly updated runtime
   release the old gateway instead of failing the update on its safety query."
  []
  (let [{:keys [reason gateway-protocol]} (compatibility)]
    (when (and (= "gateway-too-old" reason) gateway-protocol)
      {"X-Vis-Min-Gateway-Protocol" (str gateway-protocol)})))

(defn- assert-compatible!
  "Refuse to drive a daemon whose wire protocol this build cannot speak, with the
   rendered mismatch screen attached. Returns `entry` when compatible."
  [entry]
  (let [v (compatibility)]
    (when-not (:is-compatible v) (throw (protocol/incompatible-ex v)))
    entry))

(defn- send-json-with-entry!
  ([entry method path] (send-json-with-entry! entry method path nil nil))
  ([entry method path body] (send-json-with-entry! entry method path body nil))
  ([entry method path body opts]
   (let [response
         (gw-send! entry method path (assoc (or opts {}) :body body))

         status
         (long (:status response))

         parsed
         (parse-json-body (:body response))

         ;; `error-response` nests the reason under `error.message`; older routes
         ;; answer a flat `message`. Read BOTH, or a rejected request surfaces as a
         ;; bare "gateway HTTP 400" and the caller's dialog explains nothing.
         reason
         (or (get parsed "message") (get-in parsed ["error" "message"]))]

     (when (>= status 400)
       (throw
         (if (= status 401)
           (ex-info
             (str
               "could not authenticate to the gateway (HTTP 401: "
               (or reason "unauthorized")
               "). It demands a bearer token and this client presented "
               "none, or the wrong one. "
               (if (:remote? entry)
                 "Pass the token that gateway printed at startup: --gateway-token TOKEN (or VIS_GATEWAY_TOKEN)."
                 "Run the client on the SAME machine as the gateway (it reads the token from ~/.vis), or reach it with --gateway HOST --gateway-token TOKEN."))
             (assoc parsed
               :http-status status
               :vis/user-error true))
           (ex-info (or reason (str "gateway HTTP " status)) (assoc parsed :http-status status)))))
     parsed)))

(defn- probe-entry?
  [{:keys [secret remote?] :as entry}]
  (try (let [response
             (gw-send! entry "GET" "/healthz" {:timeout-ms health-probe-timeout-ms})

             body
             (note-handshake! (parse-json-body (:body response)))]

         (boolean (and (= 200 (:status response))
                       (= "ok" (get body "status"))
                       ;; `secret_match` is the daemon confirming OUR registry secret. A remote
                       ;; target we hold no token for cannot match it and need not: an auth-free
                       ;; gateway serves every route anyway, and a token-gated one answers 401.
                       (or (true? (get body "secret_match"))
                           (and remote? (str/blank? (str secret)))))))
       (catch Throwable _ false)))

(defn- port-free?
  "True when nothing is accepting TCP connections on host:port — i.e. a previous
   daemon has fully released it, so a respawn on the same port won't bind-race."
  [^String host port]
  (try (with-open [sock (java.net.Socket.)]
         (.connect sock (java.net.InetSocketAddress. host (int port)) 200)
         false)
       (catch Throwable _ true)))

(defn- retire-loopback-orphan!
  "Stop a registry-less standard loopback gateway before replacing it.

   The stable token and `/healthz` prove that this is Vis and that it owns `db`,
   but a missing registry means the daemon's launch environment is unknown. Never
   reattach to it: shut it down, wait for its port to close, then let discovery
   launch this process's gateway."
  [db host port]
  (when (and (= DEFAULT_HOST host) (nil? (discovery/read-registry db)))
    (try
      (let [token-file
            (discovery/default-token-file)

            secret
            (when (.exists token-file)
              (some-> (slurp token-file)
                      str/trim
                      not-empty))

            candidate
            (when secret {:host host :port port :secret secret})

            response
            (when candidate
              ;; Health normally repairs a missing registry. Keep this diagnostic probe
              ;; side-effect-free so we can retire only a genuine orphan.
              (gw-send! candidate
                        "GET"
                        "/healthz"
                        {:timeout-ms health-probe-timeout-ms
                         :headers {"X-Vis-Suppress-Registry-Recovery" "true"}}))

            body
            (when (= 200 (:status response)) (note-handshake! (parse-json-body (:body response))))

            pid
            (get body "pid")

            daemon-db
            (get body "db")]

        (when (and (nil? (discovery/read-registry db))
                   (= "ok" (get body "status"))
                   (true? (get body "secret_match"))
                   pid
                   (discovery/pid-alive? pid)
                   daemon-db
                   (= (discovery/registry-key db) (discovery/registry-key daemon-db)))
          (let [stop-response
                (gw-send! (assoc candidate :pid pid) "POST" "/v1/admin/stop" {})

                deadline
                (+ (System/currentTimeMillis) 3000)]

            (when (<= 200 (long (:status stop-response)) 299)
              (loop []

                (cond (port-free? host port) true
                      (>= (System/currentTimeMillis) deadline) false
                      :else (do (Thread/sleep 50) (recur))))))))
      (catch Throwable t (cancellation/preserve-interrupt! t) nil))))

(def ^:private spinner-frames ["⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏"])

(defn- interactive-tty? [] (some? (System/console)))

(defn- progress-reporter
  "Build an `:on-event` callback for [[discovery/discover-or-start!]] that surfaces
   cold-start progress on stderr so a user is never left wondering whether vis
   hung. It is SILENT on the fast attach path (no event fires there), and clearly
   DISTINGUISHES 'this process is starting the gateway' from 'another vis is
   already starting it — we're waiting' so a herd of clients reads as one boot,
   not N frozen screens. On a TTY it renders a single spinner line that rewrites
   in place with the elapsed seconds; off a TTY it logs one line per milestone.
   Never throws."
  []
  (let [tty
        (interactive-tty?)

        err
        ^java.io.PrintStream System/err

        state
        (atom {:label nil :frame 0 :active false})

        clear
        (fn []
          (when tty (.print err "\r\u001b[K")))

        start
        (fn [label plain]
          (swap! state assoc :label label :active true)
          (if tty
            (.print err (str "\r\u001b[K⟳ " label "…"))
            (.println err (str "vis-agent: " plain)))
          (.flush err))

        finish
        (fn [line]
          (when (:active @state)
            (clear)
            (.println err line)
            (.flush err)
            (swap! state assoc :active false)))]

    (fn [{:keys [phase mode elapsed-ms]}]
      (try (case phase
             :spawning
             (start "starting vis" "starting vis…")

             :awaiting
             (start "another vis is starting up — waiting" "another vis is starting up — waiting…")

             :recovering
             (start "vis missed a health check — waiting" "vis missed a health check — waiting…")

             :tick
             (when (and tty (:active @state))
               (let [{:keys [label frame]}
                     @state

                     f
                     (nth spinner-frames (mod frame (count spinner-frames)))

                     secs
                     (format "%.1f" (/ (double (or elapsed-ms 0)) 1000.0))]

                 (swap! state update :frame inc)
                 (.print err (str "\r\u001b[K" f " " label "… " secs "s"))
                 (.flush err)))

             :ready
             (finish (str "✓ vis ready" (when (= mode :awaited) " (started by another vis)")))

             :timeout
             (finish "✗ vis did not become ready in time")

             nil)
           (catch Throwable _ nil)))))

(defn- discover-or-recover!
  "Attach or start the managed gateway without ever spawning onto an occupied port.

   A listener can appear a few milliseconds before its registry move becomes
   visible, so an occupied port gets one short registry wait. A registry-less
   authenticated loopback gateway is an orphan: retire it and start a fresh daemon
   from this process rather than inheriting its stale launch environment. Any other
   listener is left alone and reported to the user."
  [db target-host target-port]
  (if (retire-loopback-orphan! db target-host target-port)
    (discovery/discover-or-start! {:db db :port target-port :host target-host}
                                  :probe probe-entry?
                                  :on-event (progress-reporter)
                                  :timeout-ms (if (discovery/native-image?) 15000 60000))
    (if (port-free? target-host target-port)
      (discovery/discover-or-start! {:db db :port target-port :host target-host}
                                    :probe probe-entry?
                                    :on-event (progress-reporter)
                                    :timeout-ms (if (discovery/native-image?) 15000 60000))
      (if-let [entry (discovery/await-registry! db
                                                probe-entry?
                                                {:timeout-ms occupied-port-registry-wait-ms
                                                 :poll-ms 100})]
        {:mode :awaited :entry entry}
        (throw (ex-info (str "gateway port "
                             target-host
                             ":"
                             target-port
                             " is occupied, but no healthy gateway registry appeared. "
                             "A stale Vis gateway or another process owns the port; "
                             "stop that process and retry.")
                        {:type :gateway/orphaned-port
                         :host target-host
                         :port target-port
                         :db (str (discovery/db-target db))
                         :vis/user-error true}))))))

(defn- ensure-remote-gateway!
  "Attach to the REMOTE gateway `entry`: probe `/healthz` (debounced exactly like
   the local path) and check the wire contract. Nothing else happens — a gateway on
   another machine has no registry here, no pid to watch, and no lifecycle this
   process may drive."
  [entry]
  (let [now (System/nanoTime)]
    (when-not (< now (long @entry-fresh-until-ns))
      (when-not (probe-entry? entry)
        (throw (ex-info
                 (str "no gateway answered at " (base-url entry)
                      ". Check that it is running and reachable"
                      (if (:secret entry)
                        ", and that the token matches the one it was started with."
                        "; a token-gated gateway also needs --gateway-token."))
                 {:type :gateway/remote-unreachable :url (base-url entry) :vis/user-error true})))
      (reset! entry-fresh-until-ns (+ now (* (long entry-probe-ttl-ms) 1000000))))
    (reset! cached-entry entry)
    (assert-compatible! entry)))

(defn- shutdown-subscriptions!
  "Close every client-owned SSE stream exactly once without reconnecting.

   Called by the single process shutdown hook after `client-finalizing?` flips.
   Clearing both registries before closing their streams makes every reader's
   EOF/reconnect guard observe terminal state, even though JVM hooks and socket
   callbacks race concurrently."
  []
  (reset! client-finalizing? true)
  (let [[legacy _]
        (swap-vals! subscriptions (constantly {}))

        [mux-before _]
        (swap-vals! mux
                    (fn [m]
                      (-> m
                          (update :epoch inc)
                          (assoc :subs {}
                                 :future nil
                                 :stream nil))))]

    (doseq [[_ {:keys [future stream]}] legacy]
      (try (some-> ^java.io.Closeable @stream
                   .close)
           (catch Throwable _ nil))
      (when future (future-cancel future)))
    (when-let [stream (:stream mux-before)]
      (try (.close ^java.io.Closeable stream) (catch Throwable _ nil)))
    (when-let [future (:future mux-before)]
      (future-cancel future)))
  nil)

(defn- release-client!
  []
  (when-let [cid @client-id]
    (try (when-let [entry @cached-entry]
           (send-json-with-entry! entry "DELETE" (str "/v1/clients/" (enc cid))))
         (catch Throwable _ nil)
         (finally (reset! client-id nil)))))

(defn- ensure-release-hook!
  []
  (when (compare-and-set! release-hook-installed? false true)
    (.addShutdownHook (Runtime/getRuntime)
                      (Thread. ^Runnable
                               (fn []
                                 ;; Shutdown hooks run concurrently. Flip the barrier FIRST so
                                 ;; no per-stream reconnect path can create a fresh future while
                                 ;; another hook is tearing the JVM down.
                                 (reset! client-finalizing? true)
                                 (shutdown-subscriptions!)
                                 (release-client!))
                               "vis-gateway-client-shutdown"))))

(defn- ensure-client!
  "Register this JVM as a daemon client exactly once. This is the refcount lease
   that keeps a detached gateway alive while a TUI/client process is alive; the
   shutdown hook releases it gracefully, and the daemon ignores the lease if this
   pid is killed. Gateway JSON is canonical STRING-keyed data, so the returned
   `client_id` must be read as a string key; accepting nil here causes one new
   registration before every request."
  [entry]
  (when-not @client-id
    (locking client-id
      (when-not @client-id
        (let [response
              (send-json-with-entry! entry
                                     "POST"
                                     "/v1/clients"
                                     (cond-> {:kind "clojure-client"}
                                       ;; A remote lease carries no pid: the daemon's
                                       ;; reaper judges pids on ITS machine.
                                       (not (:remote? entry))
                                       (assoc :pid (discovery/current-pid))))

              registered-id
              (get response "client_id")]

          (when-not (seq registered-id)
            (throw (ex-info "gateway client registration returned no client_id"
                            {:type :gateway/invalid-client-registration})))
          (reset! client-id registered-id)
          (ensure-release-hook!)))))
  @client-id)

(defn- loopback-host?
  "True for a bind a phone (or any other machine) can never reach."
  [host]
  (contains? #{"127.0.0.1" "::1" "localhost"} (str host)))

(defn status
  "Admin status of the gateway this process drives — the REMOTE target when one is
   configured, else the daemon registered for the current DB. Always the daemon's
   own wire map (STRING keys), including when nothing is running."
  []
  (if-let [remote (remote-gateway)]
    (send-json-with-entry! (ensure-remote-gateway! remote) "GET" "/v1/admin/status")
    (let [db (db-target)
          entry (discovery/read-registry db)]

      (if (discovery/registry-fresh? entry probe-entry?)
        (send-json-with-entry! entry
                               "GET"
                               "/v1/admin/status"
                               nil
                               {:headers (lifecycle-protocol-headers)})
        {"status" "stopped"
         "db" (when-not (discovery/memory-db? db) (str (discovery/db-target db)))}))))

(defn pairing-info
  "Connection details for the daemon registered for the current DB, so a caller
   can build a companion pairing QR on demand (not only at `--pair` boot time).
   Returns {:running? :host :port :token :loopback?}; `:running?` is false when no
   fresh daemon is registered, and `:loopback?` flags a 127.0.0.1/::1/localhost
   bind that a phone can never reach."
  []
  (if-let [{:keys [host port secret]} (remote-gateway)]
    ;; A remote target is reachable BY DEFINITION — it just answered a probe — so
    ;; its own connection details are what a pairing QR must carry.
    {:running? true :host host :port port :token secret :loopback? (loopback-host? host)}
    (let [db (db-target)
          {:keys [host port secret] :as entry} (discovery/read-registry db)]

      (if (discovery/registry-fresh? entry probe-entry?)
        {:running? true :host host :port port :token secret :loopback? (loopback-host? host)}
        {:running? false}))))

(defn- await-port-free!
  "Block (bounded) until nothing is listening on host:port. True when the port was
   released, false on timeout."
  [host port timeout-ms]
  (let [deadline (+ (System/currentTimeMillis) (long timeout-ms))]
    (loop []

      (cond (port-free? (str host) port) true
            (>= (System/currentTimeMillis) deadline) false
            :else (do (Thread/sleep 50) (recur))))))

(defn- registered-daemon-handle
  "The OS handle for the pid a registry entry names, or nil when signalling it
   would be a guess. Two facts must agree: the pid is alive, and that process
   started BEFORE the registry entry naming it was written. A vis daemon writes
   its entry at boot, so a pid the OS recycled after that daemon died is YOUNGER
   than the file and is never signalled - the one way a stranger's process could
   otherwise inherit both the pid and the port. A platform that hides start times
   yields nothing to compare, and the liveness + port evidence stands alone."
  [db pid]
  (when (and pid (discovery/pid-alive? pid))
    (when-let [handle (.orElse (java.lang.ProcessHandle/of (long pid)) nil)]
      (let [registry-ms (.lastModified ^java.io.File (discovery/registry-file db))
            started (.orElse (.startInstant (.info ^java.lang.ProcessHandle handle)) nil)]

        (when (or (nil? started)
                  (not (pos? registry-ms))
                  (<= (.toEpochMilli ^java.time.Instant started) (+ (long registry-ms) 1000)))
          handle)))))

(defn- kill-registered-daemon!
  "Last resort for a daemon that stopped answering: signal the pid its own registry
   entry claims. A wedged daemon - event loop stuck, shutdown hook deadlocked, JVM
   thrashing - still holds the port, so nothing else can start there and
   `POST /v1/admin/stop` never returns; that is the state a human otherwise leaves
   with `lsof -nP -iTCP:<port>` and a manual kill.

   Ownership evidence is the registry entry vis itself wrote for THIS db plus the
   port still being held; [[registered-daemon-handle]] refuses everything else, so
   a stranger is never signalled. SIGTERM first, because the daemon's own shutdown
   hook is what releases sessions, sandboxes and child processes; SIGKILL only if
   the port is still held after it. Returns {:signal :term|:kill|nil :stopped? bool}."
  [db {:keys [host port pid]}]
  (if-let [handle (registered-daemon-handle db pid)]
    (do (.destroy ^java.lang.ProcessHandle handle)
        (if (await-port-free! host port 3000)
          {:signal :term :stopped? true}
          (do (.destroyForcibly ^java.lang.ProcessHandle handle)
              {:signal :kill :stopped? (await-port-free! host port 3000)})))
    {:signal nil :stopped? false}))
(defn stop-daemon!
  "Stop the daemon registered for this DB, escalating when it stops answering.
   `POST /v1/admin/stop` first; when that is met with silence from a daemon that
   still holds its port, signal the pid the registry names ([[kill-registered-daemon!]])
   rather than reporting a live orphan and handing the human an `lsof`. A port held
   by a process this registry cannot claim is still reported, never signalled."
  []
  (when (remote-gateway)
    (throw (ex-info (str "connected to a remote gateway (--gateway / VIS_GATEWAY_URL): "
                         "vis stops only the daemon it manages for this DB. Stop that "
                         "gateway on the machine that runs it.")
                    {:type :gateway/remote-target :vis/user-error true})))
  (let [db
        (db-target)

        entry
        (discovery/read-registry db)

        forget!
        (fn []
          (reset! cached-entry nil)
          (reset! client-id nil))

        escalate!
        (fn []
          (let [{:keys [signal stopped?]} (kill-registered-daemon! db entry)]
            (forget!)
            (if stopped?
              {:status "stopped" :stopping false :escalated signal :pid (:pid entry)}
              {:status "orphaned"
               :type :gateway/orphaned-daemon
               :host (:host entry)
               :port (:port entry)
               :pid (:pid entry)
               :recovery (str "It answered neither /v1/admin/stop nor a signal. Inspect it with "
                              "`lsof -nP -iTCP:" (:port entry)
                              " -sTCP:LISTEN`, stop that process, then retry "
                              "`vis-agent gateway stop`.")})))]

    (if (discovery/registry-fresh? entry probe-entry?)
      (let [res (try (send-json-with-entry! entry
                                            "POST"
                                            "/v1/admin/stop"
                                            nil
                                            {:headers (lifecycle-protocol-headers)})
                     (catch Throwable _ ::unreachable))]
        (if (= ::unreachable res) (escalate!) (do (forget!) res)))
      (if (and (:host entry) (:port entry) (not (port-free? (str (:host entry)) (:port entry))))
        (escalate!)
        {:status "stopped" :stopping false}))))

(defn- await-daemon-down!
  "Block (bounded) until the daemon for `db` is provably gone: its registry entry
   cleared by the shutdown hook AND host:port released. [[stop-daemon!]] only
   *asks* the daemon to stop (the stop handler sleeps ~25ms, then shutdown work
   runs async), so without this the immediate re-ensure would rediscover the
   still-fresh registry and attach to the DYING daemon — then bind-race its corpse
   on respawn. Returns true when down, false on timeout (the caller still
   proceeds: discover-or-start! deletes a stale registry and tolerates a race)."
  [db host port]
  (let [deadline (+ (System/currentTimeMillis) 3000)]
    (loop []

      (let [entry (discovery/read-registry db)]
        (cond (and (not (discovery/registry-fresh? entry probe-entry?)) (port-free? host port)) true
              (> (System/currentTimeMillis) deadline) false
              :else (do (Thread/sleep 50) (recur)))))))

(defn- wire-count
  "A count read off a peer's status map, or nil when this build cannot read it. An
   absent key is zero (a stopped daemon reports no counts and is refused on its
   `status` field instead), but a value of a shape this build does not know is
   NEVER rounded down to zero: zero is the only reading that would let a caller
   stop a daemon somebody is using, and the peer whose status matters most here is
   by definition a build this one did not ship with."
  [x]
  (cond (nil? x) 0
        (number? x) (max 0 (long x))
        (string? x) (try (max 0 (Long/parseLong (str/trim x))) (catch Exception _ nil))
        :else nil))
(defn daemon-idle?
  "THE one definition of \"this daemon may be bounced\", read off an admin status
   map (canonical STRING keys, e.g. from [[status]]).

   A managed daemon with no client and no running turn is free to release: it was
   auto-spawned for whoever needed it, it self-reaps anyway, and the next client
   spawns a fresh one from whatever is on disk. A busy one holds work - someone's
   TUI, someone's turn - that a stop would abort. A user-owned one
   (`vis-agent gateway start`, nohup, systemd) is never ours to stop, whatever it
   is doing.

   `opts` calibrates that one rule for a caller that is itself attached:
   `:tolerate-clients` is how many of the leases belong to the caller, and
   `:user-owned-ok?` admits the self-heal path that must replace a daemon whose
   classpath lacks a route no matter who started it.

   Returns {:idle? :reason :clients :running-turns :managed? :pid}, where `:reason`
   is one of :idle :not-running :user-owned :clients :running-turns. A status this
   build cannot read - no map at all, or a count in a shape it does not know
   ([[wire-count]]) - is :not-running: never evidence that stopping is free."
  ([status] (daemon-idle? status nil))
  ([status {:keys [tolerate-clients user-owned-ok?]}]
   (let [clients
         (wire-count (get status "clients"))

         turns
         (wire-count (get status "running_turns"))

         managed?
         (boolean (get status "managed"))

         ;; A count this build cannot read comes FIRST, then USE: a daemon somebody
         ;; is working on is off limits whoever started it and whatever its status
         ;; field says, so no caller can talk itself past a live client or a turn.
         reason
         (cond (or (nil? clients) (nil? turns)) :not-running
               (> (long clients) (long (or tolerate-clients 0))) :clients
               (pos? (long turns)) :running-turns
               (not= "running" (get status "status")) :not-running
               (and (not managed?) (not user-owned-ok?)) :user-owned
               :else :idle)]

     {:idle? (= :idle reason)
      :reason reason
      :clients clients
      :running-turns turns
      :managed? managed?
      :pid (get status "pid")})))

(defn stop-daemon-if-idle!
  "Release the daemon registered for this DB when releasing it is free, and leave
   it strictly alone otherwise. This is what runs after `vis-agent update`: every
   live daemon is then older than the runtime on disk, and stopping an unused
   managed one costs nothing because the next client spawns the new build. A
   --gateway target belongs to another machine and is never touched.

   Returns the [[daemon-idle?]] verdict plus `:stopped?` and, when it acted, the
   `:stop` result."
  []
  (if (remote-gateway)
    {:idle? false :reason :remote :stopped? false}
    (let [verdict (daemon-idle? (status))]
      (if (:idle? verdict)
        (let [res (stop-daemon!)]
          (assoc verdict
            :stopped? (not= "orphaned" (:status res))
            :stop res))
        (assoc verdict :stopped? false)))))
(defn stale-bounce-verdict
  "THE rule for replacing a daemon that is merely OLD - a pure decision over what
   the two halves advertise about themselves and an admin status map (canonical
   STRING keys).

   `vis-agent update` releases an idle daemon itself, but one a TUI held open
   survives the install and would keep serving the old image to every session after
   it. The next client to attach is the one that can fix that: it learns the other
   half's version AND build out of the `/healthz` handshake every attach already
   pays for. [[protocol/superseded?]] owns that comparison - the release version
   where the two carry an order, the build commit where they do not - which is what
   makes this work for a `dev` checkout and for two builds of one VIS_VERSION, in a
   native image exactly as in a source JVM.

   Use decides the rest, exactly as everywhere else: [[daemon-idle?]] over the status
   map, tolerating NO client, because this runs before this process takes its lease.
   Nobody's open session or running turn is ever aborted to pick up a build, and a
   status that could not be read is not evidence of an idle daemon.

   Returns {:bounce? :reason :from :to}: `:reason` is `:fresh` when there is nothing
   to pick up, otherwise the [[daemon-idle?]] reason."
  [{:keys [ours theirs our-build their-build status]}]
  (if-not (protocol/superseded?
            {:our-version ours :their-version theirs :our-build our-build :their-build their-build})
    {:bounce? false :reason :fresh :from theirs :to ours}
    (let [{:keys [reason]} (daemon-idle? status)]
      {:bounce? (= :idle reason) :reason reason :from theirs :to ours})))

(defn- report-version-bounce!
  "One stderr line before a restart nobody asked for, so the extra seconds read as a
   build pickup and not as a hang. The commit is shown only where the versions alone
   cannot tell the two apart (`dev` against `dev`), which is exactly the dev case.
   Never throws."
  [{:keys [from to from-build to-build]}]
  (let [ambiguous?
        (= from to)

        label
        (fn [version build]
          (str (or version "an older build") (when (and ambiguous? build) (str " (" build ")"))))]

    (try (.println ^java.io.PrintStream System/err
                   (str "vis-agent: gateway is running "
                        (label from from-build)
                        " - restarting it on "
                        (label to to-build)
                        "…"))
         (catch Throwable _ nil))))

(defn- bounce-stale-daemon!
  "Act on [[stale-bounce-verdict]] for the daemon `entry` this process just attached
   to: stop it, so the caller starts THIS build in its place.

   At most ONCE per process. A daemon that comes back old anyway - a client whose
   own classpath predates the update spawning it again, or two checkouts sharing one
   DB - then costs exactly one restart instead of a loop, which is what bounds the
   build-identity half of [[protocol/superseded?]] (an identity is symmetric where a
   version order is not). The comparison happens first and is free (the handshake is
   already in hand, the build id is computed once per process), so an up-to-date
   daemon never pays for the status round trip. Returns the verdict with `:bounced?`."
  [entry]
  (let [ours
        (protocol/release-version)

        our-build
        (protocol/build-id)

        {theirs :version their-build :build}
        @gateway-handshake*

        identity*
        {:our-version ours :their-version theirs :our-build our-build :their-build their-build}]

    (cond (not (protocol/superseded? identity*)) {:bounced? false :reason :fresh}
          (not (compare-and-set! stale-bounce-attempted? false true)) {:bounced? false
                                                                       :reason :checked}
          :else (let [status
                      (try (send-json-with-entry! entry
                                                  "GET"
                                                  "/v1/admin/status"
                                                  nil
                                                  {:headers (lifecycle-protocol-headers)})
                           (catch Throwable _ nil))

                      verdict
                      (stale-bounce-verdict {:ours ours
                                             :theirs theirs
                                             :our-build our-build
                                             :their-build their-build
                                             :status status})]

                  (if (:bounce? verdict)
                    (do (report-version-bounce!
                          {:from theirs :to ours :from-build their-build :to-build our-build})
                        (stop-daemon!)
                        (await-daemon-down! (db-target) (:host entry) (:port entry))
                        (assoc verdict :bounced? true))
                    (assoc verdict :bounced? false))))))

(defn- cached-entry-if-fresh
  "Return the compatible cached daemon entry while its freshness proof holds."
  []
  (let [cached
        @cached-entry

        now
        (System/nanoTime)

        fresh-until
        (long @entry-fresh-until-ns)

        fresh?
        (if (and (map? cached) (< now fresh-until) (discovery/pid-alive? (:pid cached)))
          true
          ;; Window elapsed (or no cached entry): pay for the real HTTP probe
          ;; once, then re-open the debounce window.
          (when (discovery/registry-fresh? cached probe-entry?)
            (reset! entry-fresh-until-ns (+ now (* (long entry-probe-ttl-ms) 1000000)))
            true))]

    (when fresh? (assert-compatible! cached))))

(defn ensure-gateway!
  "Return a fresh daemon registry entry for the current DB, auto-starting the
   detached gateway if needed. `:memory` is a programmer error for this client;
   headless one-shots stay in-process and should not call here.

   Optional `:port`/`:host` overrides the bind used WHEN THIS CALL SPAWNS a fresh
   daemon (e.g. `vis-agent channels web --port`); a fresh daemon already registered for
   the DB is a singleton and is attached to as-is, so the override is moot there.

   Freshness is DEBOUNCED: the full HTTP /healthz probe (via `probe-entry?`)
   runs at most once per `entry-probe-ttl-ms`. Within that window a cached entry
   whose pid is still alive is trusted directly, so the TUI's chatty poll loop
   stops paying for a doubled HTTP round-trip (and its JSON/reflection churn) on
   every gateway call.

   The slow discover/start path is single-flight per canonical DB inside this
   process. Callers re-check the cache after acquiring that lock, so concurrent
   startup callbacks share one spawn/wait instead of each waiting for readiness.

   A daemon running a DIFFERENT build than this one is also replaced here when
   replacing it is free ([[bounce-stale-daemon!]]) - that is how the first vis
   started after `vis-agent update`, or after a rebuild of a dev checkout, comes up
   on the new code with nobody stopping anything by hand. That decision comes BEFORE
   the compatibility assert: a daemon too old to speak this build's wire protocol is
   the one most worth replacing, so the mismatch screen is left for the daemon
   somebody is still using."
  ([] (ensure-gateway! nil))
  ([{:keys [port host] :as opts}]
   (if-let [remote (remote-gateway)]
     ;; A remote gateway is ATTACHED to, never managed: no registry, no spawn.
     (ensure-remote-gateway! remote)
     (let [db (db-target)]
       (when (discovery/memory-db? db)
         (throw (ex-info "gateway daemon is disabled for :memory DB" {:type :gateway/no-daemon})))
       (or (cached-entry-if-fresh)
           (call-with-ensure-lock
             db
             (fn []
               ;; Another caller may have completed discovery while this one waited.
               (or (cached-entry-if-fresh)
                   (let [target-port (or port DEFAULT_PORT)
                         target-host (or host DEFAULT_HOST)
                         ;; First recover the exact failure mode where a live standard daemon
                         ;; owns 7890 but its registry was removed. Never enter discovery's
                         ;; spawn path while the requested port already has a listener.
                         {:keys [entry] :as result}
                         (discover-or-recover! db target-host target-port)]

                     (if entry
                       (do (reset! cached-entry entry)
                           (reset! entry-fresh-until-ns (+ (System/nanoTime)
                                                           (* (long entry-probe-ttl-ms) 1000000)))
                           ;; Staleness BEFORE compatibility: a daemon too old to speak this
                           ;; build's wire protocol is exactly the one worth replacing, and
                           ;; the mismatch screen is for the daemon somebody is still using.
                           (if (:bounced? (bounce-stale-daemon! entry))
                             ;; The old image released the port; start this build in its place.
                             (ensure-gateway! opts)
                             (assert-compatible! entry)))
                       (throw (ex-info "gateway daemon did not become ready"
                                       (assoc result :type :gateway/start-timeout)))))))))))))

(defn- send-json!
  ([method path] (send-json! method path nil))
  ([method path body]
   (let [entry (ensure-gateway!)]
     (ensure-client! entry)
     (send-json-with-entry! entry method path body))))

(defn request!
  "Canonical authenticated HTTP request for gateway development and diagnostics.

   Resolves or starts the registered daemon for the current DB, acquires this
   process's client lease, adds protocol/authentication headers without exposing
   the registry secret, JSON-encodes `:body`, and delegates to the same
   babashka.http-client transport as every production client call.

   `method` may be a keyword or string. `opts` accepts `:body`, `:as`,
   `:timeout-ms`, and additional `:headers`; gateway-owned authentication and
   protocol headers cannot be overridden. The raw non-throwing HTTP response map
   is returned so callers can inspect `:status`, `:headers`, and `:body`."
  ([method path] (request! method path {}))
  ([method path opts]
   (when-not (and (string? path) (str/starts-with? path "/"))
     (throw (ex-info "gateway request path must start with /"
                     {:type :gateway/invalid-request-path :path path})))
   (let [entry (ensure-gateway!)]
     (ensure-client! entry)
     (gw-send! entry
               (str/upper-case (if (keyword? method) (name method) (str method)))
               path
               opts))))

(def ^:private channel-read-timeout-ms
  "Ceiling for a read a CHANNEL makes while a person waits at an open dialog.
   Short on purpose: an unreachable daemon must paint \"unavailable\" within
   seconds instead of parking a terminal thread on the transport's own default."
  5000)

(defn capabilities
  "The daemon's capability document, string-keyed, or nil when it cannot answer.
   The attachment contract a channel admits file drops against comes from here."
  []
  (try (let [response (request! :get "/v1/capabilities" {:timeout-ms channel-read-timeout-ms})]
         (when (= 200 (:status response)) (wire/parse-json (:body response))))
       (catch Throwable _ nil)))

(defn session-artifacts
  "Every durable artifact `sid` has produced, string-keyed and in gateway order,
   or nil when the daemon cannot answer. nil is UNAVAILABLE — a channel must
   paint it differently from an index that is genuinely empty."
  [sid]
  (try (let [response (request! :get
                                (str "/v1/sessions/" (enc sid) "/artifacts")
                                {:timeout-ms channel-read-timeout-ms})]
         (when (= 200 (:status response))
           (vec (get (wire/parse-json (:body response)) "artifacts" []))))
       (catch Throwable _ nil)))
(defn toggle-setting!
  "Atomically flip one boolean setting in the gateway and return its refreshed
   string-keyed settings row. The gateway owns both persistence and live runtime
   fan-out; clients must not mutate a process-local toggle registry instead."
  [id]
  (send-json! "POST" "/v1/settings" {:id id :action "toggle"}))

(defn cycle-setting!
  "Atomically advance one enum setting in the gateway and return its refreshed
   string-keyed settings row."
  [id]
  (send-json! "POST" "/v1/settings" {:id id :action "cycle"}))

(defn create-session! [opts] (send-json! "POST" "/v1/sessions" opts))

(defn session-slashes
  "GET the gateway-owned slash catalog for `sid` and `channel`. The first call may
   initialize Python extensions in the gateway, so it uses the cold-load timeout."
  ([sid] (session-slashes sid :web))
  ([sid channel]
   (let [entry
         (ensure-gateway!)

         path
         (str "/v1/sessions/" (enc sid) "/slashes?channel=" (enc (name channel)))]

     (ensure-client! entry)
     (vec (get (send-json-with-entry! entry "GET" path nil {:timeout-ms slash-catalog-timeout-ms})
               "commands")))))

(defn soul
  [sid]
  (try (send-json! "GET" (str "/v1/sessions/" (enc sid)))
       (catch clojure.lang.ExceptionInfo e
         (when-not (= 404 (:http-status (ex-data e))) (throw e)))))

(defn- session-window-path
  "The path of ONE window of the session list.

   `opts` names the cut: `:limit` rows, `:after` the cursor of the last row already held,
   `:root` one project's column, `:project-id` one project's tab set, `:id-prefix` the
   session a short id names, `:ids` exactly the rows those ids name. The gateway owns the
   ordering, so a caller that wants ten rows asks for ten instead of downloading the fleet
   to slice it locally - and a read that names no cut and no limit is answered with the
   head window, never the fleet."
  [{:keys [limit after root project-id id-prefix ids]}]
  (let [qs (->> [(when limit (str "limit=" (enc limit)))
                 (when (seq (str after)) (str "after=" (enc after)))
                 (when (seq (str root)) (str "root=" (enc root)))
                 (when (seq (str project-id)) (str "project_id=" (enc project-id)))
                 (when (seq (str id-prefix)) (str "id_prefix=" (enc id-prefix)))
                 (when (seq ids) (str "ids=" (enc (str/join "," (map str ids)))))]
                (remove nil?)
                (str/join "&"))]
    (cond-> "/v1/sessions"
      (seq qs)
      (str "?" qs))))

(defn list-sessions
  "The ROWS of one window of the session list, in the gateway's own order. `opts` names
   the cut - see `session-window-path`."
  [opts]
  (get (send-json! "GET" (session-window-path opts)) "sessions"))

(defn list-sessions-page
  "One window of the session list WITH the walk that continues it:
   `{:sessions rows :next-cursor str-or-nil :has-more bool :total n}`.

   `opts` names the cut (`session-window-path`). A surface that pages - the session picker
   - holds this window and asks for the next one with `:after` `:next-cursor`, so a list of
   a thousand sessions is read a screen at a time instead of downloaded whole."
  [opts]
  (let [body (send-json! "GET" (session-window-path opts))]
    {:sessions (vec (get body "sessions"))
     :next-cursor (get body "next_cursor")
     :has-more (boolean (get body "has_more"))
     :total (get body "total")}))

(defn search-session-ids
  "GET /v1/sessions/actions/search?q= — soul-id STRINGS whose transcript (user request +
   assistant text) matches `query`. Blank query → []. The heavy assistant text
   never crosses the wire; callers union these ids into a local title filter."
  [query]
  (let [q (some-> query
                  str
                  str/trim)]
    (if (or (nil? q) (= "" q))
      []
      (get (send-json! "GET" (str "/v1/sessions/actions/search?q=" (enc q))) "session_ids"))))

(defn search-session-matches
  "GET /v1/sessions/actions/search?q= — like `search-session-ids` but each hit is
   TAGGED with WHERE it matched, RANKED by the server, and carries up to a handful
   of snippets:
   `[{:id str :rank 0-3 :in-title? bool :in-request? bool :in-reply? bool
      :in-thinking? bool :request-snippet str :reply-snippet str
      :hits [{:side :request|:reply|:thinking :snippet str :at ms}]}]`.
   `:in-title?` = the session's own name matched; `:in-request?` = the user's own
   request; `:in-reply?` = the assistant's answer; `:in-thinking?` = only its
   reasoning aside. The vector arrives in the gateway's own order — running
   sessions first, then FRESHEST first, the same order its session list is in —
   and is painted in it; `:rank` (0 best) says WHERE the query hit and a surface
   never re-orders. Blank query → []. Heavy assistant text never crosses the
   wire."
  [query]
  (let [q (some-> query
                  str
                  str/trim)]
    (if (or (nil? q) (= "" q))
      []
      (->> (get (send-json! "GET" (str "/v1/sessions/actions/search?q=" (enc q))) "matches")
           (mapv (fn [m]
                   {:id (get m "session_id")
                    :rank (long (or (get m "rank") 0))
                    :in-title? (boolean (get m "is_in_title"))
                    :in-request? (boolean (get m "is_in_request"))
                    :in-reply? (boolean (get m "is_in_reply"))
                    :in-thinking? (boolean (get m "is_in_thinking"))
                    :request-snippet (get m "request_snippet")
                    :reply-snippet (get m "reply_snippet")
                    :hits (mapv (fn [h]
                                  {:side (keyword (or (get h "side") "reply"))
                                   :snippet (get h "snippet")
                                   :at (get h "at")})
                                (or (get m "hits") []))}))))))

(defn close-session! [sid] (send-json! "DELETE" (str "/v1/sessions/" (enc sid))))

;; --- Projects (cross-channel) + movable project sessions + ownership (V6/V7) ---

(defn list-projects
  "GET /v1/projects — projects are CROSS-CHANNEL. `opts`: :owner (string),
   :archived? (bool). Returns the :projects vector."
  ([] (list-projects nil))
  ([{:keys [owner archived?]}]
   (let [qs
         (->> [(when owner (str "owner=" (enc owner))) (when archived? "archived=true")]
              (remove nil?)
              (str/join "&"))

         path
         (cond-> "/v1/projects"
           (seq qs)
           (str "?" qs))]

     (get (send-json! "GET" path) "projects"))))

(defn projects-overview
  "GET /v1/projects/overview — every project with its counts plus the gateway's
   totals, in one answer (`state/projects-overview`). The whole map."
  []
  (send-json! "GET" "/v1/projects/overview"))

(defn create-project! [opts] (send-json! "POST" "/v1/projects" opts))

(defn ensure-project-for-root!
  "POST /v1/projects/actions/ensure — get-or-create the project bound to canonical
   workspace `root` (a project IS a TUI tab set). `name` seeds a fresh project.
   Returns the project."
  ([root] (ensure-project-for-root! root nil))
  ([root name]
   (send-json! "POST"
               "/v1/projects/actions/ensure"
               (cond-> {:root (str root)}
                 (not-empty (str name))
                 (assoc :name (str name))))))

(defn get-project
  [pid]
  (try (send-json! "GET" (str "/v1/projects/" (enc pid)))
       (catch clojure.lang.ExceptionInfo e
         (when-not (= 404 (:http-status (ex-data e))) (throw e)))))

(defn update-project! [pid opts] (send-json! "PATCH" (str "/v1/projects/" (enc pid)) opts))

(defn delete-project!
  "DELETE /v1/projects/:pid. Default: member sessions scatter back to
   project-less. With `{:is-recursive? true}` every member session is deleted
   too, and the response names the deleted ids."
  ([pid] (delete-project! pid nil))
  ([pid {:keys [is-recursive?]}]
   (send-json! "DELETE"
               (cond-> (str "/v1/projects/" (enc pid))
                 is-recursive?
                 (str "?is_recursive=true")))))

(defn assign-project!
  "Assign a session to a project (nil clears / removes from project). Returns the soul."
  [sid pid]
  (send-json! "PATCH" (str "/v1/sessions/" (enc sid)) {:project_id (when pid (str pid))}))

(defn reorder-project-sessions!
  "Persist a project's manual session order in one gateway call. Loose named
   sessions are adopted atomically; guests owned by another project are not moved."
  [pid session-ids]
  (send-json! "PATCH" (str "/v1/projects/" (enc pid) "/sessions") {:order (mapv str session-ids)}))

(defn release-session-runtime!
  "Release a session's live RUNTIME on the daemon WITHOUT touching the process
   client lease: stop its background resources (background `shell` children, managed REPLs)
   and drop its loop/env, keeping the transcript resumable. Used when ONE view of
   a session closes (e.g. a single TUI tab) while the owning process stays
   connected — so the whole-process refcount lease is left intact and the daemon
   is never nudged toward self-reap while other tabs remain open. Best-effort and
   never daemon-spawning — nothing to release against when no fresh daemon is
   registered."
  [sid]
  (when sid
    (try (let [entry (or @cached-entry (discovery/read-registry (db-target)))]
           (when (discovery/registry-fresh? entry probe-entry?)
             (send-json-with-entry! entry "POST" (str "/v1/sessions/" (enc sid) "/release"))))
         (catch Throwable _ nil))))

(defn release-session!
  "Release a session VIEW when the owning channel exits: tell the daemon to
   stop the session's background resources (background `shell` children, REPLs) and drop
   its live runtime, then release the process-level client lease. This is NOT
   a per-session delete (the transcript stays resumable) and never sends daemon
   shutdown; the daemon stops itself only when refcount AND running-turn-count
   hit zero. Best-effort and never daemon-spawning — if no fresh daemon is
   registered there is nothing to release against."
  [sid]
  (release-session-runtime! sid)
  (release-client!))

(defn get-turn
  [sid tid]
  (try (send-json! "GET" (str "/v1/sessions/" (enc sid) "/turns/" (enc tid)))
       (catch clojure.lang.ExceptionInfo e
         (when-not (= 404 (:http-status (ex-data e))) (throw e)))))

(defn list-turns [sid] (get (send-json! "GET" (str "/v1/sessions/" (enc sid) "/turns")) "turns"))

(defn transcript
  "Every turn of `sid`, hydrated. UNBOUNDED — the whole session is listed AND
   hydrated, which on a long session is seconds of work and megabytes of JSON.
   Prefer `transcript-page` for anything interactive."
  [sid]
  (get (send-json! "GET" (str "/v1/sessions/" (enc sid) "/transcript")) "turns"))

(defn transcript-page
  "A WINDOW of `sid`'s transcript — the paging counterpart of `transcript`.

   `opts`: `:limit` window size (nil = the whole transcript), `:offset` 0-based
   start in the OLDEST-FIRST list (nil = the NEWEST `:limit` turns). The gateway
   also caps a window in BYTES, so the reply's `offset` can come back HIGHER
   than the one asked for — page from the RETURNED `offset`, never from your own
   arithmetic.

   Returns the canonical wire map `{\"turns\" [...] \"total\" n \"offset\" n
   \"has_more\" bool}` (oldest-first turns)."
  [sid {:keys [limit offset]}]
  (let [qs (cond-> []
             (some? limit)
             (conj (str "limit=" (enc limit)))

             (some? offset)
             (conj (str "offset=" (enc offset))))]
    (send-json! "GET"
                (str "/v1/sessions/" (enc sid)
                     "/transcript" (when (seq qs) (str "?" (str/join "&" qs)))))))

(defn transcript-md
  "The gateway-rendered user/assistant dialog Markdown for `sid` — the canonical
   `transcript->md :dialog`. Returns the string, or nil on a non-2xx."
  [sid]
  (let [entry
        (ensure-gateway!)

        _
        (ensure-client! entry)

        response
        (gw-send! entry "GET" (str "/v1/sessions/" (enc sid) "/transcript.md") {:as :string})]

    (when (< (long (:status response)) 400) (:body response))))

(defn transcript-html
  "The gateway-rendered STANDALONE HTML transcript for `sid` — the canonical
   `transcript->html`, the HTML sibling of `transcript-md`. Returns the string,
   or nil on a non-2xx."
  [sid]
  (let [entry
        (ensure-gateway!)

        _
        (ensure-client! entry)

        response
        (gw-send! entry "GET" (str "/v1/sessions/" (enc sid) "/transcript.html") {:as :string})]

    (when (< (long (:status response)) 400) (:body response))))

(defn turn-trace
  "Canonical wire iterations of ONE persisted turn (nil when the id is
   unknown to the daemon)."
  [sid tid]
  (get (send-json! "GET" (str "/v1/sessions/" (enc sid) "/turns/" (enc tid) "/trace"))
       "iterations"))

(defn context-snapshot [sid] (send-json! "GET" (str "/v1/sessions/" (enc sid) "/context")))

(defn- pref<-wire
  "Project the wire model pref `{\"provider\" \"model\"}` into the engine-shaped
   `{:provider :model}` map every channel's model UI consumes — the ONE exit
   where this wire value becomes engine data."
  [m]
  (when m {:provider (get m "provider") :model (get m "model")}))

(defn session-model
  [sid]
  (pref<-wire (get (send-json! "GET" (str "/v1/sessions/" (enc sid) "/model")) "model")))

(defonce ^:private session-model-cache (atom {}))

(defonce ^:private session-model-refreshing (atom #{}))

(def ^:private session-model-cache-ttl-ms 750)

(defn- refresh-session-model!
  "Single-flight background refresh of the session-model cache for `sid` —
   same discipline as `refresh-resources!`: the daemon round-trip is BLOCKING
   and must never run on the render thread. Errors leave the last-known value
   untouched."
  [sid k]
  (let [[old _] (swap-vals! session-model-refreshing conj k)]
    (when-not (contains? old k)
      (future (try (let [v (session-model sid)]
                     (swap! session-model-cache assoc k {:at (System/currentTimeMillis) :val v}))
                   (catch Throwable _ nil)
                   (finally (swap! session-model-refreshing disj k)))))))

(defn session-model-cached
  "Footer-frequency read of the session's model pref served from a per-sid
   cache that NEVER blocks the caller (issue #29, gateway leg: this used to
   be a live `session-model` HTTP round-trip per footer frame). A stale (or
   cold) entry kicks a background single-flight refresh and this returns the
   last-known value immediately (nil before the first success).
   `set-session-model!` writes through, so a pick made in THIS client shows
   on the very next frame."
  [sid]
  (let [k
        (str sid)

        now
        (System/currentTimeMillis)

        {:keys [at val]}
        (get @session-model-cache k)]

    (when-not (and at (< (- now (long at)) (long session-model-cache-ttl-ms)))
      (refresh-session-model! sid k))
    val))

;; Managed resources (backgrounds) — the daemon owns the registry (the agent's
;; tools register here while a turn runs IN THE DAEMON), so a client in another
;; process reads/controls them over HTTP. An in-process client uses the
;; local registry directly and never touches these.

(defn list-resources
  "Vector of the session's live resource DATA maps from the daemon's registry
   (string-keyed, same shape `resources/list-resources` returns in-process)."
  [sid]
  (vec (get (send-json! "GET" (str "/v1/sessions/" (enc sid) "/resources")) "resources")))

(defonce ^:private resources-cache (atom {}))

(defonce ^:private resources-refreshing (atom #{}))

(def ^:private resources-cache-ttl-ms 750)

(defn- refresh-resources!
  "Single-flight background refresh of the resource cache for `sid`. The daemon
   round-trip (`list-resources`) is BLOCKING and must never run on the render
   thread — a busy daemon then stalls every TUI frame. Only one fetch per sid is
   ever in flight, so render-cadence misses can't pile futures up. Errors leave
   the last-known value untouched."
  [sid k]
  (let [[old _] (swap-vals! resources-refreshing conj k)]
    (when-not (contains? old k)
      (future (try (let [v (list-resources sid)]
                     (swap! resources-cache assoc k {:at (System/currentTimeMillis) :val v}))
                   (catch Throwable _ nil)
                   (finally (swap! resources-refreshing disj k)))))))

(defn list-resources-cached
  "Footer-frequency read: the session's resource list served from a per-sid cache
   that NEVER blocks the caller. A stale (or cold) entry kicks a background
   single-flight refresh and this returns the last-known value immediately (nil
   before the first success). Keeping the daemon HTTP round-trip OFF the render
   thread is what stops a busy daemon from stalling every TUI frame."
  [sid]
  (let [k
        (str sid)

        now
        (System/currentTimeMillis)

        {:keys [at val]}
        (get @resources-cache k)]

    (when-not (and at (< (- now (long at)) (long resources-cache-ttl-ms)))
      (refresh-resources! sid k))
    val))

(defn stop-resource!
  "Run the resource's stop-fn in the daemon and unregister it. Returns the
   daemon's stop result map (`{:result \"stopped\"|\"unknown\"|… :id …}`)."
  [sid rid]
  (send-json! "POST" (str "/v1/sessions/" (enc sid) "/resources/stop?rid=" (enc rid))))

(defn resource-logs
  "Captured output lines for a background via its daemon-side logs-fn, or nil."
  [sid rid]
  (get (send-json! "GET" (str "/v1/sessions/" (enc sid) "/resources/logs?rid=" (enc rid))) "lines"))

(defn iteration-attachment-bytes
  "Raw bytes (a byte-array) of ONE outbound artifact — iteration `iid`, its 0-based
   `idx` in the iteration's ordered attachment list — fetched from the daemon's
   attachment byte endpoint, or nil (404 / no bytes). The lazy-fetch companion to
   a live `iteration.completed` attachment descriptor: a client sees `{:index
   :media_type …}` on the frame, then pulls the bytes here. HISTORY resolves the
   same way (the trace iteration's `:id` + attachment index)."
  [sid iid idx]
  (let [entry
        (ensure-gateway!)

        _
        (ensure-client! entry)

        path
        (str "/v1/sessions/" (enc sid) "/iterations/" (enc iid) "/attachments/" idx)

        response
        (gw-send! entry "GET" path {:as :bytes})]

    (when (< (long (:status response)) 400) (:body response))))

(defn set-session-model!
  "PATCH the session's model pref in the daemon. Writes the returned pref
   straight through into the `session-model-cached` snapshot so the footer
   chip flips on the very next frame instead of waiting out the cache TTL."
  [sid provider model]
  (let [pref (pref<-wire (get (send-json! "PATCH"
                                          (str "/v1/sessions/" (enc sid) "/model")
                                          {:provider provider :model model})
                              "model"))]
    (swap! session-model-cache assoc (str sid) {:at (System/currentTimeMillis) :val pref})
    pref))

;; ── Headless provider OAuth ────────────────────────────────────────────────
;; These mirror `POST /v1/providers/:id/auth/{start,complete,poll,cancel}` and
;; `/logout`. Every one of them is a THIN pass-through: the daemon owns the
;; PKCE verifier, the device code, the token exchange, and the credential file.
;; A client only ever sees what it must SHOW the user (URL, user code) and the
;; verdict. This is what lets a remote client — the companion app, or a TUI
;; attached to a gateway on another machine — sign a provider in
;; WITHOUT the provider extension on its own classpath.

(defn provider-auth-start!
  "Begin OAuth for `provider-id`. Returns the string-keyed wire flow
   (`flow_id`, `kind`, `url`, `user_code`, `verification_uri`, `interval_ms`,
   `instructions`) or nil when the daemon refused."
  [provider-id]
  (send-json! "POST" (str "/v1/providers/" (enc (name provider-id)) "/auth/start")))

(defn provider-auth-complete!
  "Finish a `pkce` flow with the redirect URL the user pasted back."
  [provider-id flow-id redirect-url]
  (send-json! "POST"
              (str "/v1/providers/" (enc (name provider-id)) "/auth/complete")
              {:flow_id flow-id :redirect_url redirect-url}))

(defn provider-auth-submit-key!
  "Finish an `api-key` flow: hand the key the user typed to the DAEMON, which
   persists it in ITS OWN config. The calling process never writes the
   credential — same boundary as OAuth."
  [provider-id flow-id api-key]
  (send-json! "POST"
              (str "/v1/providers/" (enc (name provider-id)) "/auth/complete")
              {:flow_id flow-id :api_key api-key}))

(defn provider-auth-poll!
  "Read a `device` flow's verdict: `pending`, `ok`, or `error`. Never blocks."
  [provider-id flow-id]
  (send-json! "POST"
              (str "/v1/providers/" (enc (name provider-id)) "/auth/poll")
              {:flow_id flow-id}))

(defn provider-auth-cancel!
  "Forget an abandoned flow. Idempotent."
  [provider-id flow-id]
  (send-json! "POST"
              (str "/v1/providers/" (enc (name provider-id)) "/auth/cancel")
              {:flow_id flow-id}))

(defn provider-logout!
  "Clear `provider-id`'s persisted credentials IN THE DAEMON."
  [provider-id]
  (send-json! "POST" (str "/v1/providers/" (enc (name provider-id)) "/logout")))

(defn provider-remove!
  "DELETE /v1/providers/:id — drop `provider-id` from the fleet IN THE DAEMON,
   credential included. Removal is the daemon's to do because it owns BOTH the
   config file and the token file: a row dropped while its credential stays on
   disk comes straight back as an authenticated preset. Idempotent — `is_removed`
   is false when the id was not in the persisted fleet."
  [provider-id]
  (send-json! "DELETE" (str "/v1/providers/" (enc (name provider-id)))))

;; ── MCP servers ────────────────────────────────────────────────────────────────
;; MCP is configured and RUN on the gateway: it owns the connection pool, the
;; child processes, and the OAuth tokens. A channel only inspects that inventory,
;; toggles/kills a server, and drives the headless auth legs — exactly the same
;; boundary as provider auth above.

(defn mcp-servers
  "Sanitized MCP inventory (string-keyed rows: `name`, `transport`, `enabled`,
   `is_connected`, `is_managed`, `is_killed`, `tools`, `is_authorized`, …)."
  []
  (vec (get (send-json! "GET" "/v1/mcp/servers") "servers")))

(defn mcp-save-server!
  "Create or replace a gateway-managed server. `spec` is the string-keyed wire
   spec (`transport`, `command`/`args`/`cwd`/`env`, or `url`/`headers`, plus the
   optional `enabled` and `timeout_ms`). The DAEMON validates it, persists it in
   its own machine state, and reconnects — nothing is written on this side, so a
   TUI attached to a REMOTE gateway adds servers exactly like the app does.

   Secrets survive an omitting save: see `mcp.core/with-preserved-secrets`.
   Returns the saved sanitized row."
  [server spec]
  (send-json! "POST" "/v1/mcp/servers" {:name server :server spec}))

(defn mcp-test-server!
  "Connect a CANDIDATE spec without saving it and return `{name, is_connected,
   tools}`. The gateway opens and closes the connection, so a bad command or an
   unreachable endpoint is reported before it is ever persisted."
  [server spec]
  (send-json! "POST" "/v1/mcp/servers/actions/test" {:name server :server spec}))

(defn mcp-kill-server!
  "Stop a server NOW and hold it down until it is started again. Runtime only —
   nothing in the user's config changes."
  [server]
  (send-json! "POST" (str "/v1/mcp/servers/" (enc server) "/actions/kill")))

(defn mcp-start-server!
  "Release a kill and connect the server again."
  [server]
  (send-json! "POST" (str "/v1/mcp/servers/" (enc server) "/actions/start")))

(defn mcp-set-server-enabled!
  "Persist a server's on/off switch in the gateway's own state."
  [server enabled]
  (send-json! "POST"
              (str "/v1/mcp/servers/" (enc server) "/actions/enable")
              {:enabled (boolean enabled)}))

(defn mcp-delete-server! [server] (send-json! "DELETE" (str "/v1/mcp/servers/" (enc server))))

(defn mcp-auth-start!
  "Begin headless OAuth for an HTTP MCP server. Returns the wire flow
   (`flow_id`, `kind`, `url`, `redirect_uri`, `expires_at_ms`, `status`)."
  [server]
  (send-json! "POST" (str "/v1/mcp/servers/" (enc server) "/auth/start")))

(defn mcp-auth-complete!
  "Finish a flow with the redirect URL the user pasted back (or a bare code)."
  [server flow-id input]
  (send-json! "POST"
              (str "/v1/mcp/servers/" (enc server) "/auth/complete")
              {:flow_id flow-id :input input}))

(defn mcp-auth-poll!
  "Read a flow's verdict without blocking: `pending`, `ok`, or `error`."
  [server flow-id]
  (send-json! "POST" (str "/v1/mcp/servers/" (enc server) "/auth/poll") {:flow_id flow-id}))

(defn mcp-auth-cancel!
  [server flow-id]
  (send-json! "POST" (str "/v1/mcp/servers/" (enc server) "/auth/cancel") {:flow_id flow-id}))

(defn mcp-auth-logout!
  "Forget the gateway's persisted OAuth tokens for a server."
  [server]
  (send-json! "POST" (str "/v1/mcp/servers/" (enc server) "/auth/logout")))

(defn- decode-workspace
  "The gateway serves the workspace in THE canonical string-keyed wire shape
   (`wire/canonical`) on BOTH transports, so the remote client passes it through
   VERBATIM — one representation, no re-hydration."
  [w]
  w)

(defn session-workspace-info
  [sid]
  (decode-workspace (get (send-json! "GET" (str "/v1/sessions/" (enc sid) "/workspace"))
                         "workspace")))

(defn change-root!
  "Repoint `sid`'s PRIMARY filesystem root to `path` IN THE DAEMON, returning the
   refreshed `session-workspace-info` (whose `:id` is the newly pinned workspace)."
  [sid path]
  (decode-workspace
    (get (send-json! "PATCH" (str "/v1/sessions/" (enc sid) "/workspace/root") {:path path})
         "workspace")))

(defn list-drafts
  "Active/stashed DRAFTS for `sid`'s repo IN THE DAEMON, newest first, in the
   canonical wire shape `[{\"workspace_id\" \"label\" \"root\" \"repo_root\"
   \"fork_ms\" \"is_current\"}]`. The gateway is the source of truth for parked
   drafts, so every channel reads the SAME list here (web picker, TUI drafts view)."
  [sid]
  (get (send-json! "GET" (str "/v1/sessions/" (enc sid) "/workspace/drafts")) "drafts"))

(defn stash-draft!
  "Park `sid`'s current draft IN THE DAEMON (non-destructive), returning the
   refreshed `session-workspace-info` — now back on trunk."
  [sid]
  (decode-workspace (get (send-json! "POST" (str "/v1/sessions/" (enc sid) "/workspace/stash") {})
                         "workspace")))

(defn resume-draft!
  "Switch `sid` INTO the stashed draft `workspace-id` IN THE DAEMON (stashing any
   current draft first), returning the refreshed `session-workspace-info`."
  [sid workspace-id]
  (decode-workspace (get (send-json! "POST"
                                     (str "/v1/sessions/" (enc sid) "/workspace/resume")
                                     {:workspace_id workspace-id})
                         "workspace")))

(defn create-draft!
  "Create and enter a named draft for `sid` IN THE DAEMON. Any current draft is
   stashed first. Pass `clean?` to seed from the committed HEAD without the
   user's uncommitted files."
  [sid label clean?]
  (decode-workspace (get (send-json! "POST"
                                     (str "/v1/sessions/" (enc sid) "/workspace/drafts")
                                     {:label label :clean (boolean clean?)})
                         "workspace")))

(defn abandon-draft!
  "Permanently abandon `workspace-id` IN THE DAEMON. The target may be current or
   parked, but not pinned to another session."
  [sid workspace-id reason]
  (decode-workspace (get (send-json! "DELETE"
                                     (str "/v1/sessions/" (enc sid)
                                          "/workspace/drafts/" (enc workspace-id))
                                     {:reason reason})
                         "workspace")))

(defn submit-turn!
  [sid opts]
  (let [res (send-json! "POST" (str "/v1/sessions/" (enc sid) "/turns") opts)]
    (if (get res "turn_id") {:turn res} res)))

(defn update-queued-turn!
  [sid tid request]
  (send-json! "PATCH" (str "/v1/sessions/" (enc sid) "/turns/" (enc tid)) {:request request}))

(defn delete-queued-turn!
  [sid tid]
  (send-json! "DELETE" (str "/v1/sessions/" (enc sid) "/turns/" (enc tid))))

(defn cancel-turn!
  [sid tid]
  (send-json! "POST" (str "/v1/sessions/" (enc sid) "/turns/" (enc tid) "/cancel")))

(defn cancel-current-turn!
  "Tid-less cancel: kill the turn currently holding `sid`'s `:current-turn` slot
   in the daemon, iff THIS caller submitted it under `owner-key` (the
   `idempotency_key` it sent). For callers that lost (or never learned) the
   gateway turn id. A session is shared, so an unaddressed cancel would kill
   whatever another channel happens to be running. Returns the parsed body
   (`{\"status\" \"cancelling\", \"turn_id\" tid}`); throws on HTTP error (409 when
   the session is idle or the running turn is someone else's)."
  [sid owner-key]
  (send-json! "POST"
              (str "/v1/sessions/" (enc sid) "/cancel-current")
              {:idempotency-key owner-key}))

(defn drain-idle! [sid] (send-json! "POST" (str "/v1/sessions/" (enc sid) "/drain-queue")))

;; --- Human-input requests (a run in the DAEMON blocked on the operator) ---
;;
;; `internal/human-input` parks the extension thread that raised the request and
;; publishes it on the in-process channel bus. That bus never leaves the JVM, so
;; a client process — the TUI attached to a serve daemon — can only reach the
;; request over these routes.

(defn human-input-requests
  "Pending human-input request views for `sid` IN THE DAEMON, oldest first, in
   canonical wire shape. The live `human_input.request` event is the fast path;
   this is how a client that attached LATER still finds the open form instead of
   watching a turn that never moves."
  [sid]
  (vec (get (send-json! "GET" (str "/v1/sessions/" (enc sid) "/human-input")) "requests")))

(defn live-views
  "The live views session `sid` is SHOWING in the daemon right now, oldest first,
   in canonical wire shape. The `human_input.live.*` events are the fast path; this
   is how a client that attached MID-RUN paints the whole picture at once instead
   of waiting for the next patch to tell it a view exists."
  [sid]
  (vec (get (send-json! "GET" (str "/v1/sessions/" (enc sid) "/human-input/live")) "views")))

(defn submit-human-input!
  "Answer the DAEMON-side request `request-id` of `sid` with a raw
   `field id -> value` map. Same verdict shape as the in-process
   `human-input/submit!`, because the daemon runs the engine's own validation:
   `{:is-accepted true}`, or `{:is-accepted false :errors {field-id message}}`
   with the request still pending so the operator can fix it."
  [sid request-id values]
  (let [res (send-json!
              "POST"
              (str "/v1/sessions/" (enc sid) "/human-input/" (enc request-id) "/actions/submit")
              {:values (or values {})})]
    (cond-> {:is-accepted (boolean (get res "is_accepted"))}
      (seq (get res "errors"))
      (assoc :errors (get res "errors")))))

(defn cancel-human-input!
  "Dismiss the DAEMON-side request `request-id` of `sid`, releasing the parked
   run with `is_submitted false`. Returns whether it was still pending and
   dismissable."
  [sid request-id]
  (boolean (get
             (send-json!
               "POST"
               (str "/v1/sessions/" (enc sid) "/human-input/" (enc request-id) "/actions/cancel"))
             "is_cancelled")))

(defn focus-live-view!
  "Focus `item-ids` in focusable table `node-id` of the DAEMON-side live view.
   Returns the canonical shared selection the action recorded."
  [sid view-id node-id item-ids]
  (let [res (send-json!
              "POST"
              (str "/v1/sessions/" (enc sid) "/human-input/live/" (enc view-id) "/actions/focus")
              {:node_id node-id :focused_ids (vec item-ids)})]
    {:view-id (get res "view_id")
     :node-id (get res "node_id")
     :focused-ids (vec (get res "focused_ids"))}))

(defn interrupt-live-view!
  "Stop the DAEMON-side live view `view-id` of `sid`, with `note` — the comment the
   person typed with the stop, when they typed one. Returns whether a view was
   still open to stop.

   A view is ALWAYS stoppable, so a TUI watching a REMOTE session ends one exactly
   as it ends a local one: the run resumes with an interrupted verdict that says a
   HUMAN stopped it."
  ([sid view-id] (interrupt-live-view! sid view-id nil))
  ([sid view-id note]
   (boolean
     (get (send-json!
            "POST"
            (str "/v1/sessions/" (enc sid) "/human-input/live/" (enc view-id) "/actions/interrupt")
            (cond-> {}
              (not (str/blank? (str note)))
              (assoc :note (str note))))
          "is_interrupted"))))

(defn reconcile-running-turns!
  "Clients do not sweep. Only the daemon may reconcile its own startup orphans."
  []
  nil)

(defn- probe-route
  "Probe whether the daemon actually SERVES `path`, distinguishing three cases so
   the caller only ever force-restarts on a genuine missing-route 404:
     :served      — the daemon answered (any status other than 404).
     :absent      — a real 404: the daemon's classpath lacks the extension that
                    owns `path` (e.g. a gateway auto-started without the web
                    channel never mounts /ui).
     :unreachable — the probe request itself failed (connection reset, timeout).
                    NOT a 404, so NEVER treated as license to force-kill; the
                    caller retreats to leaving the daemon alone."
  [entry path]
  (try (let [response (gw-send! entry "GET" path {})]
         (if (= 404 (:status response)) :absent :served))
       (catch Throwable _ :unreachable)))

(defn ensure-gateway-serving!
  "Like [[ensure-gateway!]], but tries to GUARANTEE the returned daemon actually
   serves `path`. When [[ensure-gateway!]] attaches to an already-running daemon
   that 404s on `path` (started from a classpath missing the extension that owns
   it), respawn a fresh daemon from THIS process — whose classpath, by
   construction, carries the route. This is what lets `vis-agent channels web`
   self-heal instead of parking on a `/ui` that 404s.

   Optional `opts` (`{:port :host}`) overrides the bind used when THIS call has
   to spawn a fresh daemon (the `vis-agent channels web --port/--host` flags); it is
   moot when a fresh daemon is already registered for the DB.

   The respawn is NON-DESTRUCTIVE. A blind POST /v1/admin/stop is refcount-blind:
   it would abort every in-flight turn and kill every session's background
   resources. So we force-restart the stale daemon ONLY when it is idle — no OTHER
   clients and no running turn. Otherwise we leave it untouched and surface a clear
   error. A transport blip on the probe (not a real 404) never triggers a restart.
   Returns the entry."
  ([path] (ensure-gateway-serving! path nil))
  ([path opts]
   (let [entry (ensure-gateway! opts)]
     (if (:remote? entry)
       ;; A gateway on another machine is not ours to restart: name the missing
       ;; route instead of running the local self-heal against it.
       (if (= :absent (probe-route entry path))
         (throw (ex-info (str "the remote gateway at " (base-url entry) " does not serve " path)
                         {:type :gateway/route-missing :path path :vis/user-error true}))
         entry)
       (case (probe-route entry path)
         ;; Mounted — or a transient transport blip we must not misread as "missing".
         (:served :unreachable)
         entry

         :absent
         (let [{:keys [reason clients running-turns]}
               (daemon-idle? (status) {:tolerate-clients 1 :user-owned-ok? true})]
           ;; Refuse on USE, never on ownership: this heal replaces a daemon whose
           ;; classpath lacks the route whoever started it, but it must never abort
           ;; another client's session or an in-flight turn to do so.
           (when (contains? #{:clients :running-turns} reason)
             (throw (ex-info (str "gateway daemon does not serve " path
                                  " but is in use (" clients
                                  " client(s), " running-turns
                                  " running turn(s)); refusing" " to force-restart a shared daemon")
                             {:type :gateway/route-missing-busy
                              :path path
                              :clients clients
                              :running-turns running-turns})))
           (stop-daemon!)
           (await-daemon-down! (db-target) (:host entry) (:port entry))
           (let [entry (ensure-gateway! opts)]
             (when-not (= :served (probe-route entry path))
               (throw (ex-info
                        (str "gateway daemon is not serving " path " even after a fresh restart")
                        {:type :gateway/route-missing :path path})))
             entry)))))))

(defn provider-status
  [provider-id]
  (let [path
        (str "/v1/providers/" (enc (name provider-id)) "/status")

        entry
        (ensure-gateway-serving! path)]

    (ensure-client! entry)
    ;; Canonical wire shape: the status map keeps its snake_case STRING keys
    ;; (`is_authenticated`, `source`, …) exactly as it crossed the wire — NO
    ;; keyword restoration. Consumers read `(get status "is_authenticated")`.
    (get (send-json-with-entry! entry "GET" path) "status")))

(defn- wire-enum [x] (if (string? x) (keyword x) x))

(defn- provider-limit-window<-wire
  [window]
  (when (map? window)
    (cond-> {:kind (wire-enum (get window "kind"))}
      (some? (get window "unit"))
      (assoc :unit (wire-enum (get window "unit")))

      (some? (get window "size"))
      (assoc :size (get window "size"))

      (some? (get window "resets_at_ms"))
      (assoc :resets-at-ms (get window "resets_at_ms")))))

(defn- provider-limit-row<-wire
  [row]
  (when (map? row)
    (cond-> {:id (wire-enum (get row "id"))
             :label (get row "label")
             :scope (wire-enum (get row "scope"))
             :kind (wire-enum (get row "kind"))
             :precision (wire-enum (get row "precision"))
             :source (wire-enum (get row "source"))
             :is-unlimited (get row "is_unlimited")}
      (some? (get row "subject"))
      (assoc :subject (get row "subject"))

      (some? (get row "window"))
      (assoc :window (provider-limit-window<-wire (get row "window")))

      (some? (get row "used"))
      (assoc :used (get row "used"))

      (some? (get row "limit"))
      (assoc :limit (get row "limit"))

      (some? (get row "remaining"))
      (assoc :remaining (get row "remaining"))

      (some? (get row "note"))
      (assoc :note (get row "note")))))

(defn- provider-limit-error<-wire
  [error]
  (when (map? error)
    (cond-> {:type (wire-enum (get error "type")) :message (get error "message")}
      (some? (get error "data"))
      (assoc :data (get error "data")))))

(defn- provider-limits<-wire
  "Restore the gateway provider-limits report to the engine/TUI shape using the
  explicit provider-limits schema only. Do not generic-walk gateway data here:
  this boundary knows the few string/snake_case fields it accepts and rewrites
  only those fields."
  [report]
  (when (map? report)
    (let [static
          (or (get report "static") {})

          dynamic
          (or (get report "dynamic") {})

          limits
          (get dynamic "limits")

          error
          (get report "error")]

      (cond-> {:provider-id (wire-enum (get report "provider_id"))
               :status (wire-enum (get report "status"))
               :fetched-at-ms (get report "fetched_at_ms")
               :static (cond-> {}
                         (some? (get static "rpm"))
                         (assoc :rpm (get static "rpm"))

                         (some? (get static "tpm"))
                         (assoc :tpm (get static "tpm")))
               :dynamic (cond-> {:limits (mapv provider-limit-row<-wire (or limits []))}
                          (some? (get dynamic "note"))
                          (assoc :note (get dynamic "note")))}
        (some? error)
        (assoc :error (provider-limit-error<-wire error))))))

(defn provider-limits
  [provider-id]
  (let [path
        (str "/v1/providers/" (enc (name provider-id)) "/limits")

        entry
        (ensure-gateway-serving! path)]

    (ensure-client! entry)
    (provider-limits<-wire (get (send-json-with-entry! entry "GET" path) "report"))))

(defn provider-models
  "GET /v1/providers/:id/models — the LIVE model catalog resolved DAEMON-side,
   where the gateway owns OAuth token resolution. A thin client NEVER builds a
   token-resolving svar router to list models; it asks the daemon, which runs
   the `svar/models!` probe (and any token refresh) against its own credential.
   Returns the engine-shaped `{:models [id …] :hidden-count n}`."
  [provider-id show-all?]
  (let [path
        (str "/v1/providers/" (enc (name provider-id)) "/models" (when show-all? "?show_all=true"))

        entry
        (ensure-gateway-serving! path)

        resp
        (do (ensure-client! entry) (send-json-with-entry! entry "GET" path))]

    {:models (vec (get resp "models")) :hidden-count (long (or (get resp "hidden_count") 0))}))

(defn router
  "GET /v1/router — the unified router dialog payload assembled by the gateway:
   `{\"providers\" [{\"id\" … \"label\" … \"base_url\" … \"models\" [...]
   \"status\" {\"is_authenticated\" …} \"limits\" {…}} …]}`. Returned VERBATIM with
   snake_case STRING keys — NO keyword restoration. Consumers read the string
   keys directly (`(get status \"is_authenticated\")`)."
  []
  (let [path
        "/v1/router"

        entry
        (ensure-gateway-serving! path)]

    (ensure-client! entry)
    (get (send-json-with-entry! entry "GET" path) "providers")))

(defn router-diagnostics
  "The WHOLE provider dialog in ONE gateway call.

   `GET /v1/router` already carries every provider's `status` and `limits`, so a
   client that wants both for N providers reads it once instead of firing 2×N
   per-provider probes. Keyed by provider-id keyword:
   `{:openai {:status {\"is_authenticated\" …} :limits {…}}}` — `:status` stays
   VERBATIM snake_case strings (same shape `provider-status` returns) and
   `:limits` is restored to the engine shape `provider-limits` returns, so both
   values drop straight into the callers those two functions already have."
  []
  (into {}
        (keep (fn [entry]
                (when-let [id (get entry "id")]
                  [(keyword id)
                   {:status (or (get entry "status") {})
                    :limits (provider-limits<-wire (get entry "limits"))}])))
        (router)))

(defn- patch-router!
  "PATCH /v1/router with `body`, returning the raw snake_case answer (both tags)."
  [body]
  (let [path
        "/v1/router"

        entry
        (ensure-gateway-serving! path)]

    (ensure-client! entry)
    (send-json-with-entry! entry "PATCH" path body)))

(defn- router-selection
  "One tag from a `/v1/router` answer as `{:provider-id … :model …}`, nil when the
   role carries no provider."
  [response provider-key model-key]
  (when-let [provider (get response provider-key)]
    {:provider-id (keyword provider) :model (get response model-key)}))

(defn set-router-default!
  "PATCH /v1/router — tag the PRIMARY provider/model pair (the router root every
   turn starts on). Returns `{:provider-id … :model …}`."
  [provider-id model]
  (-> (patch-router! {"role" "primary" "provider" (name provider-id) "model" (str model)})
      (router-selection "default_provider" "default_model")))

(defn set-router-fallback!
  "PATCH /v1/router — tag the FALLBACK provider/model pair: the router's second
   root, on a provider the primary does NOT use (the daemon refuses the primary's
   own with a 400). Zero args, or a nil provider, CLEARS the tag. Returns the
   resulting `{:provider-id … :model …}`, or nil once cleared."
  ([] (set-router-fallback! nil nil))
  ([provider-id model]
   (-> (patch-router! (cond-> {"role" "fallback"}
                        provider-id
                        (assoc "provider"
                          (name provider-id) "model"
                          (str model))))
       (router-selection "fallback_provider" "fallback_model"))))

(defn current-seq [sid] (get (send-json! "GET" (str "/v1/sessions/" (enc sid) "/seq")) "seq"))

(defn events-since
  [sid cursor]
  (get (send-json! "GET"
                   (str "/v1/sessions/" (enc sid) "/events-since?cursor=" (long (or cursor 0))))
       "events"))

(defn- terminal-event->result
  "Resolve a terminal event to the canonical settled content. The event has no
   duplicate answer body; fetch the turn message that owns the content array."
  [event fallback-turn-id]
  (let [failed?
        (or (= "turn.failed" (get event "type")) (= "failed" (get event "status")))

        cancelled?
        (= "cancelled" (get event "status"))

        needs-input?
        (= "suspended" (get event "status"))

        turn-id
        (or (get event "turn_id") fallback-turn-id)

        message
        (get-turn (get event "session_id") turn-id)

        blocks
        (or (get message "content") [])]

    ;; Terminal events are LEAN ({:turn_id :status}); the fetched turn row
    ;; (`message`) owns the settled meta (tokens/cost/model/…) — mirror of
    ;; the in-process gateway.state resolution, same shared key list.
    (cond-> (-> (merge (select-keys message wire/turn-meta-keys)
                       (into {} (filter (comp some? val)) (select-keys event wire/turn-meta-keys)))
                (assoc "content" blocks
                       "iteration_count" (or (get message "iteration_count") 1)
                       "session_turn_id" (or (get message "engine_turn_id") turn-id)))
      needs-input?
      (assoc "status" "needs_input")

      cancelled?
      (assoc "status" "cancelled")

      failed?
      (assoc "error"
        (or (some #(when (= "error" (get % "type")) (get % "message")) blocks)
            (get event "error")
            "turn failed")))))

(defn- sse-response!
  "Open the gateway SSE stream for `sid` resuming at `cursor`. Returns the
   babashka.http-client response map whose `:body` is a live `InputStream`.

   ONE session is just the smallest set the multiplexed route serves. The daemon
   has a SINGLE session-event endpoint (`/v1/events?sids=…`) and it emits the
   same `subscription.ready` + replay-then-live frames for every session it
   carries, so a blocking turn and the live mirror ask for `sids=<sid>:<cursor>`
   instead of a per-session route of their own."
  [sid cursor]
  (let [entry (ensure-gateway!)]
    (gw-send! entry
              "GET"
              (str "/v1/events?sids=" (enc sid) ":" (long (or cursor 0)))
              {:as :stream})))

(def ^:private sse-idle-timeout-ms
  "Close the client SSE stream when NOTHING — not even the daemon's ~15s
   heartbeat frame (gateway.server/HEARTBEAT_MS) — has arrived for this long.
   A wedged / half-dead daemon (GC pause, deadlock, dead heartbeat over a
   half-open TCP) otherwise leaves `.readLine` parked FOREVER (OS TCP keepalive
   is ~2h), silently freezing the turn. Closing the body `InputStream` kicks
   the parked read with an IOException, which `read-events-until!` / `subscribe!`
   already treat as a drop and RECONNECT from the last cursor: a recovered
   daemon resumes losslessly, a truly dead one fails fast and surfaces a real
   disconnect once the reconnect budget is spent. 4× the heartbeat so a couple
   of missed heartbeats don't trip it."
  60000)

(defn- start-sse-idle-watchdog!
  "Daemon thread that closes `in` once `last-line-ns*` is staler than
   `sse-idle-timeout-ms`, unblocking a parked `.readLine`. `alive?*` is flipped
   false by the reader on normal exit so the watchdog stops touching the stream.
   Returns the Thread (interrupt it to stop early)."
  [^InputStream in last-line-ns* alive?*]
  (let [check-ms
        (-> (long sse-idle-timeout-ms)
            (quot 4)
            (max 250)
            (min 5000))

        runnable
        (fn []
          (loop []

            (when @alive?*
              (let [idle-ms (long (/ (- (System/nanoTime) (long @last-line-ns*)) 1000000))]
                (if (>= idle-ms (long sse-idle-timeout-ms))
                  (when @alive?* (try (.close in) (catch Throwable _ nil)))
                  (do (try (Thread/sleep check-ms) (catch InterruptedException _ nil))
                      (recur)))))))]

    (doto (Thread. ^Runnable runnable "vis-gateway-sse-idle-watchdog") (.setDaemon true) (.start))))

(def ^:private sse-reconnect-max-attempts
  "How many times a blocking turn stream reconnects after the daemon drops the
   connection mid-turn before giving up and surfacing a disconnect error."
  5)

(def ^:private sse-reconnect-backoff-ms 250)

(defn- sse-data-line
  "The payload of ONE `data:` line, or nil when the line is not a data field.

   Per the SSE spec the single space after the colon is OPTIONAL, so both
   `data: {…}` and `data:{…}` are legal frames and BOTH parsers here (blocking
   turn stream and multiplexed mirror) must accept them identically — a client
   that understood only one spelling would silently drop events depending on
   which producer wrote the frame. Comments (`: ping`, the proxy pad) and any
   other field (`id:`, `event:`) yield nil and are skipped."
  [^String line]
  (when (str/starts-with? line "data:")
    (let [rest' (subs line 5)]
      (if (str/starts-with? rest' " ") (subs rest' 1) rest'))))

(defn- read-sse-frames!
  "Drive the raw `data:`-line/blank-line frame parser over `in` — the ONE place
   this client parses an SSE stream. Every parsed event is handed to `handle`; a
   truthy answer stops the read and IS the return value (a terminal signal).
   `stop?` is consulted before every line and its truthy answer stops the read
   the same way, so a reader whose subscription set changed under it bails
   without waiting for a frame. An idle watchdog (see [[sse-idle-timeout-ms]])
   closes the stream when NOTHING — not even a heartbeat — arrives for too long,
   so a wedged daemon surfaces as a normal drop instead of an infinite park.
   Returns `[:closed]` on EOF (or an idle/close-driven read failure)."
  [^InputStream in handle stop?]
  (with-open [rdr (BufferedReader. (InputStreamReader. in StandardCharsets/UTF_8))]
    (let [last-line-ns* (atom (System/nanoTime))
          alive?* (atom true)
          watchdog (start-sse-idle-watchdog! in last-line-ns* alive?*)]

      (try (loop [data-lines []]
             (or (when stop? (stop?))
                 (if-let [line (.readLine rdr)]
                   (do (reset! last-line-ns* (System/nanoTime))
                       (if (str/blank? line)
                         (let [data (str/join "\n" data-lines)
                               event (when (seq data) (wire/parse-json data))]

                           (if-not event (recur []) (or (handle event) (recur []))))
                         (if-let [d (sse-data-line line)]
                           (recur (conj data-lines d))
                           (recur data-lines))))
                   [:closed])))
           (finally (reset! alive?* false)
                    (some-> ^Thread watchdog
                            .interrupt))))))

(defn- open-sse-events!
  "Open ONE SSE connection for `sid` from `cursor` and read it with
   [[read-sse-frames!]]. For each parsed event: advance `cursor*` (highest `:seq`
   seen) then call `(handle event)`. When `handle` returns a truthy value, stop
   and return it (a terminal signal); otherwise keep reading. Resets `stream*`
   (when non-nil) to the live InputStream so `unsubscribe!` can close it. Returns
   `[:closed]` on EOF; throws `ex-info` with `:http-status` on a non-200
   response. Shared by `read-sse-stream!` (blocking turns) and `subscribe!` (the
   live mirror)."
  [sid cursor cursor* stream* handle & [on-open]]
  (let [response (sse-response! sid cursor)]
    (when-not (= 200 (:status response))
      (throw (ex-info (str "gateway SSE HTTP " (:status response))
                      {:http-status (:status response)})))
    (with-open [^InputStream in (:body response)]
      (when stream* (reset! stream* in))
      (when on-open (on-open))
      (read-sse-frames! in
                        (fn [event]
                          (when-let [s (get event "seq")]
                            (swap! cursor* max (long s)))
                          (handle event))
                        nil))))

(defn subscribe!
  "Remote equivalent of gateway.state/subscribe!: start a background SSE reader
   that replays `cursor` then calls `sink` for every live event. Returns an empty
  replay vector because the gateway's SSE endpoint itself handles replay before
   live delivery."
  [sid sub-id sink cursor]
  (if @client-finalizing?
    []
    (do
      (ensure-release-hook!)
      (let [entry
            (ensure-gateway!)

            _
            (ensure-client! entry)

            stream*
            (atom nil)

            cursor*
            (atom (long (or cursor 0)))

            fut
            (future
              ;; Reconnect (resuming from the last-seen cursor) whenever the daemon
              ;; drops the stream, so a gateway restart / transient blip no longer
              ;; kills the live mirror silently. Stops only when unsubscribe!
              ;; removes the sub from the registry (or closes the stream).
              (loop [attempt 0]
                ;; A live mirror never terminates on its own: the handler always
                ;; returns nil, so open-sse-events! only comes back on EOF ([:closed])
                ;; or throws (non-200 / IO) — either way `dropped?` is true and we
                ;; reconnect. `on-open` fires once the stream is live, and a drop
                ;; before we (maybe) reconnect fires the inverse — both delivered
                ;; through `sink` as synthetic `gateway.connected`/`.disconnected`
                ;; events so the channel can paint a live connection indicator.
                (let [dropped? (try (open-sse-events! sid
                                                      @cursor*
                                                      cursor*
                                                      stream*
                                                      (fn [event]
                                                        (sink event)
                                                        nil)
                                                      (fn []
                                                        (try (sink {:type "gateway.connected"})
                                                             (catch Throwable _ nil))))
                                    true
                                    (catch Throwable _ true))]
                  (when (and dropped? (not @client-finalizing?) (contains? @subscriptions sub-id))
                    (try (sink {:type "gateway.disconnected"}) (catch Throwable _ nil))
                    (let [delay-ms
                          (long (min 5000 (* (long sse-reconnect-backoff-ms) (inc (long attempt)))))
                          interrupted?
                          (try (Thread/sleep delay-ms) false (catch InterruptedException _ true))]

                      (when (and (not interrupted?) (not @client-finalizing?))
                        (recur (inc attempt)))))))
              (swap! subscriptions dissoc sub-id))]

        (swap! subscriptions assoc sub-id {:future fut :stream stream*})
        []))))

(defn unsubscribe!
  [_sid sub-id]
  (when-let [{:keys [future stream]} (get @subscriptions sub-id)]
    (try (some-> ^java.io.Closeable @stream
                 .close)
         (catch Throwable _ nil))
    (future-cancel future)
    (swap! subscriptions dissoc sub-id))
  nil)

(defn- mux-sids-param
  "Comma list of `sid:cursor` for the current session set (UUIDs are URL-safe,
   so no encoding needed). Cursors are read live, so a reconnect resumes each
   session from the highest seq already delivered — no replay churn, no gaps."
  [subs]
  (->> subs
       (map (fn [[sid {:keys [cursor-atom]}]]
              (str sid ":" (long @cursor-atom))))
       (str/join ",")))

(defn- mux-advance-cursor!
  "Move one session's replay cursor after a frame was delivered.

   An ordinary event advances it to the highest `\"seq\"` seen, so a reconnect
   resumes exactly where this client stopped. `subscription.ready` is different:
   it echoes the cursor the daemon ACTUALLY resumed from (server.clj's
   `resolve-sse-cursor` / `sse-ready!`), and it OVERRIDES the running max rather
   than joining it.

   Without that override a RESTARTED daemon is unrecoverable for this client.
   The gateway's counter is per-process, so a restart renumbers BELOW what we
   already saw; every reconnect then asks for a cursor above that session's
   high-water, the server clamps it to the running turn (or to live-only), and
   the session replays nothing but a tail — for good, since a monotonic max
   never comes back down. The symptom is a connected, heartbeating stream that
   silently never delivers the `turn.completed` the TUI is waiting for. The echo
   is the documented heal; the companion client already honours it (gateway.ts)."
  [cursor-atom event]
  (if (= "subscription.ready" (get event "type"))
    (let [cursor (get event "cursor")]
      (when (number? cursor) (reset! cursor-atom (long cursor))))
    (when-let [s (get event "seq")]
      (swap! cursor-atom max (long s))))
  nil)

(defn- mux-broadcast!
  "Deliver a synthetic connection event to EVERY live sink (shared stream =
   shared connection state), so each tab still paints a live/lost indicator."
  [type]
  (doseq [[_ {:keys [sinks]}]
          (:subs @mux)

          [_ sink]
          sinks]

    (try (sink {:type type}) (catch Throwable _ nil))))

(defn- open-mux-events!
  "Open ONE multiplexed SSE connection for the current session set and read it
   with [[read-sse-frames!]]. Each parsed event is demuxed by `:session_id`:
   advance that session's cursor, then call its sink. Bails with
   `[:epoch-changed]` the moment the session set is edited (so the caller
   reconnects with the new set) and `[:closed]` on EOF/drop. Throws with
   `:http-status` on a non-200."
  [my-epoch]
  (let [entry
        (ensure-gateway!)

        _
        (ensure-client! entry)

        response
        (gw-send! entry "GET" (str "/v1/events?sids=" (mux-sids-param (:subs @mux))) {:as :stream})]

    (when-not (= 200 (:status response))
      (throw (ex-info (str "gateway mux SSE HTTP " (:status response))
                      {:http-status (:status response)})))
    (with-open [^InputStream in (:body response)]
      (swap! mux assoc :stream in)
      (mux-broadcast! "gateway.connected")
      (read-sse-frames! in
                        (fn [event]
                          (let [esid (str (get event "session_id"))
                                {:keys [sinks cursor-atom]} (get (:subs @mux) esid)]

                            (when (seq sinks)
                              (mux-advance-cursor! cursor-atom event)
                              (doseq [[_ sink] sinks]
                                (try (sink event) (catch Throwable _ nil)))))
                          nil)
                        (fn []
                          (when (not= my-epoch (:epoch @mux)) [:epoch-changed]))))))

(defn- mux-run!
  "Background reconnect loop owning epoch `my-epoch`. Reconnects (resuming from
   each session's advanced cursor) whenever the daemon drops the stream, and
   stops for good once a newer epoch takes over, the session set empties, or the
   set was edited (a fresh run already owns the new set)."
  [my-epoch]
  (future
    (when-not @client-finalizing?
      (loop [attempt 0]
        (let [dropped? (try (not= [:epoch-changed] (open-mux-events! my-epoch))
                            (catch Throwable _ true))]
          (when
            (and dropped? (not @client-finalizing?) (= my-epoch (:epoch @mux)) (seq (:subs @mux)))
            (mux-broadcast! "gateway.disconnected")
            (let [delay-ms (long (min 5000
                                      (* (long sse-reconnect-backoff-ms) (inc (long attempt)))))
                  interrupted?
                  (try (Thread/sleep delay-ms) false (catch InterruptedException _ true))]

              (when (and (not interrupted?) (not @client-finalizing?)) (recur (inc attempt))))))))))

(defn- restart-mux!
  "Bump the epoch, close the live stream (unblocking the parked reader), cancel
   the old run, and — if any session remains — start a fresh run for the new
   set. Called after every subscribe/unsubscribe."
  []
  (let [{:keys [epoch stream future]} (swap! mux update :epoch inc)]
    (when stream (try (.close ^java.io.Closeable stream) (catch Throwable _ nil)))
    (when future (future-cancel future))
    (if (and (not @client-finalizing?) (seq (:subs @mux)))
      (swap! mux assoc :future (mux-run! epoch) :stream nil)
      (swap! mux assoc :future nil :stream nil))))

(defn mux-unsubscribe!
  "Drop one local listener from the multiplexed stream and reconnect only when
   the last listener for that sid is gone (or tear the connection down when it
   was the last watched session)."
  ([sid] (mux-unsubscribe! sid nil))
  ([sid sub-id]
   (let [sid
         (str sid)

         changed-session-set?
         (volatile! false)]

     (swap! mux (fn [m]
                  (let [path
                        [:subs sid]

                        entry
                        (get-in m path)

                        entry'
                        (if sub-id (update entry :sinks dissoc sub-id) nil)]

                    (if (seq (:sinks entry'))
                      (assoc-in m path entry')
                      (do (when entry (vreset! changed-session-set? true))
                          (update m :subs dissoc sid))))))
     (when @changed-session-set? (restart-mux!))
     nil)))

(defn mux-subscribe!
  "Add `sid`'s `sink` to the ONE process-wide multiplexed event stream, starting
   at `cursor` (its `current-seq` for a live-only stream). The connection is
   (re)opened only when the session set changes; multiple local listeners for
   the SAME session share one cursor and one remote subscription. Returns a
   zero-arg cleanup fn. Every sink sees gateway.connected / gateway.disconnected
   on connection changes, exactly like the per-session [[subscribe!]]."
  [sid sink cursor]
  (if @client-finalizing?
    (fn [])
    (do (ensure-release-hook!)
        (let [sid
              (str sid)

              sub-id
              (str (java.util.UUID/randomUUID))

              changed-session-set?
              (volatile! false)]

          (swap! mux (fn [m]
                       (let [existing (get-in m [:subs sid])]
                         (when-not existing (vreset! changed-session-set? true))
                         (assoc-in m
                           [:subs sid]
                           (-> (or existing {:cursor-atom (atom (long (or cursor 0))) :sinks {}})
                               (update :sinks assoc sub-id sink))))))
          (if @changed-session-set?
            (restart-mux!)
            (try (sink {:type "gateway.connected"}) (catch Throwable _ nil)))
          (fn []
            (mux-unsubscribe! sid sub-id))))))

(defn fleet-subscribe!
  "Watch the FLEET stream — `GET /v1/events?scope=fleet` — and hand every frame
   to `sink`. One frame per session whose list-visible state changed
   (`session.status`: `is_live` / `is_awaiting_input` / `current_turn_id`) or
   that was renamed (`session.title_updated`). Returns a zero-arg stop fn.

   This is what a session LIST subscribes to instead of asking about sessions one
   by one: the fleet answers WHICH sessions changed, so a picker holding a
   windowed read never polls a row again. There is no replay and no cursor — the
   feed is a delta layered on a cold `/v1/sessions` window, so a reconnect costs
   nothing to arrange and a missed frame heals on the next read. `sink` runs on
   the reader thread and must not block it; drops reconnect with the multiplexed
   mirror's backoff until the returned fn is called."
  [sink]
  (if @client-finalizing?
    (fn [])
    (let [_
          (ensure-release-hook!)

          running?
          (atom true)

          stream*
          (atom nil)

          fut
          (future
            (loop [attempt 0]
              (when (and @running? (not @client-finalizing?))
                (let [dropped?
                      (try (let [entry (ensure-gateway!)
                                 _ (ensure-client! entry)
                                 response
                                 (gw-send! entry "GET" "/v1/events?scope=fleet" {:as :stream})]

                             (when-not (= 200 (:status response))
                               (throw (ex-info (str "gateway fleet SSE HTTP " (:status response))
                                               {:http-status (:status response)})))
                             (with-open [^InputStream in (:body response)]
                               (reset! stream* in)
                               (read-sse-frames! in
                                                 (fn [event]
                                                   (try (sink event) (catch Throwable _ nil))
                                                   nil)
                                                 (fn []
                                                   (when-not @running? [:stopped]))))
                             true)
                           (catch Throwable _ true))]
                  (when (and dropped? @running? (not @client-finalizing?))
                    (let [delay-ms
                          (long (min 5000 (* (long sse-reconnect-backoff-ms) (inc (long attempt)))))
                          interrupted?
                          (try (Thread/sleep delay-ms) false (catch InterruptedException _ true))]

                      (when-not interrupted? (recur (inc attempt)))))))))]

      (fn []
        (reset! running? false)
        (when-let [in @stream*]
          (try (.close ^java.io.Closeable in) (catch Throwable _ nil)))
        (future-cancel fut)
        nil))))
(defn sse-event-action
  "Pure classifier for one parsed SSE event while blocking on `wanted-turn-id`.
   Returns `[action event']`:
     :terminal — the wanted turn reached a terminal event (return `event'`)
     :forward  — hand to on-event (own-turn progress OR a sibling turn's
                 queue-mirror event — see `wire/queue-mirror-event-types`),
                 then keep reading
     :skip     — another turn's non-queue event, drop it.
   A `turn.queued.deleted` for the WANTED turn is terminal too: the queued
   record was pulled back into an editor before it ever ran, so a cancelled
   terminal is synthesized instead of blocking on a turn that never starts."
  [event wanted-turn-id]
  (let [type
        (get event "type")

        own?
        (= (str (get event "turn_id")) (str wanted-turn-id))]

    (cond (and own? (contains? wire/turn-terminal-event-types type)) [:terminal event]
          (and own? (= "turn.queued.deleted" type)) [:terminal
                                                     (assoc event
                                                       "type" "turn.completed"
                                                       "status" "cancelled")]
          own? [:forward event]
          (contains? wire/queue-mirror-event-types type) [:forward event]
          :else [:skip event])))

(defn- read-sse-stream!
  "Read ONE SSE connection for `sid` from `cursor` until the wanted turn reaches
   a terminal event OR the stream closes. Forwards non-terminal events to
   `on-event` and advances `cursor*` (an atom holding the highest `:seq` seen)
   so a reconnect resumes losslessly. Returns `[:terminal event]` on a terminal
   event, or `[:closed]` when the daemon dropped the stream before the turn
   finished (EOF)."
  [sid cursor wanted-turn-id on-event cursor*]
  (open-sse-events! sid
                    cursor
                    cursor*
                    nil
                    (fn [event]
                      (let [[action event'] (sse-event-action event wanted-turn-id)]
                        (case action
                          :terminal
                          (do (when on-event (on-event event)) [:terminal event'])

                          :forward
                          (do (when on-event (on-event event)) nil)

                          nil)))))

(defn- read-events-until!
  "Block on the session SSE stream until the wanted turn reaches a terminal
   event. RECONNECTS (resuming from the last-seen cursor) when the gateway
   daemon drops the stream mid-turn — a transient blip or a daemon restart no
   longer strands the turn as a silent blank bubble. When the reconnect budget
   is spent, THROWS so the caller renders a real disconnect error instead of an
   empty answer."
  [sid cursor wanted-turn-id on-event]
  (let [cursor* (atom (long (or cursor 0)))]
    (loop [attempt 0]
      (let [outcome (try (read-sse-stream! sid @cursor* wanted-turn-id on-event cursor*)
                         (catch java.io.IOException _ [:closed])
                         ;; A non-200 mid-turn (502/503 while the daemon
                         ;; restarts) throws from open-sse-events!; treat it as a
                         ;; drop and reconnect, same as an EOF — otherwise a
                         ;; transient 5xx would strand the turn.
                         (catch clojure.lang.ExceptionInfo e
                           (if (:http-status (ex-data e)) [:closed] (throw e))))]
        (if (= :terminal (first outcome))
          (second outcome)
          ;; Stream closed before a terminal event → the daemon dropped us
          ;; mid-turn. Back off and reconnect from the last cursor; give up
          ;; (with a real error) once the budget is spent.
          (if (< (long attempt) (long sse-reconnect-max-attempts))
            (do (Thread/sleep (* (long sse-reconnect-backoff-ms) (inc (long attempt))))
                (recur (inc attempt)))
            (throw (ex-info "Lost connection to the gateway daemon before the turn finished."
                            {:gateway-disconnected true :turn-id (str wanted-turn-id)}))))))))

(defn submit-turn-sync!
  [sid {:keys [on-event] :as opts}]
  (let [submitted
        (submit-turn! sid (dissoc opts :on-event))

        turn
        (:turn submitted)

        turn-id
        (get turn "turn_id")]

    (when-let [e (or (:error submitted) (get submitted "error"))]
      (throw (ex-info (or (:message submitted) (get submitted "message") (str e)) submitted)))
    (terminal-event->result (read-events-until! sid 0 turn-id on-event) turn-id)))

(defn attach-turn-sync!
  [sid tid {:keys [on-event]}]
  (terminal-event->result (read-events-until! sid 0 tid on-event) tid))
