(ns com.blockether.vis.internal.gateway.server
  "Gateway HTTP/SSE server.

   Clojure-native stack: reitit-ring routes -> Ring middleware -> the
   Ring Jetty adapter on JDK virtual threads (`:virtual-threads? true`).
   SSE is a Ring `StreamableResponseBody` that parks its virtual thread
   on the connection: replay rides first, live events fan in under the
   same output-stream monitor, a heartbeat comment keeps the pipe warm
   and detects dead clients.

   This is internal plumbing, not a channel: it registers no channel
   descriptor and owns no renderer - it ships canonical IR and the
   client renders (§4.1). Any host process (the `vis gateway start` daemon, a
   TUI run, an embedded caller) can start it alongside whatever else it
   is doing via `start!`."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.config :as config]
            [com.blockether.vis.internal.loop :as lp]
            [com.blockether.vis.internal.docs :as docs]
            [com.blockether.vis.internal.extension :as extension]
            [com.blockether.vis.internal.file-picker :as file-picker]
            [com.blockether.vis.internal.gateway.discovery :as discovery]
            [com.blockether.vis.internal.gateway.pairing :as pairing]
            [com.blockether.vis.internal.gateway.state :as state]
            [com.blockether.vis.internal.gateway.wire :as wire]
            [com.blockether.vis.internal.registry :as registry]
            [com.blockether.vis.internal.provider-limits :as provider-limits]
            [com.blockether.vis.internal.providers :as providers]
            [com.blockether.vis.internal.resources :as resources]
            [com.blockether.vis.internal.toggles :as toggles]
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
           [org.eclipse.jetty.server Server]))

(def ^:private DEFAULT_PORT 7890)
(def ^:private DEFAULT_HOST "127.0.0.1")
(def ^:private HEARTBEAT_MS 15000)
(def ^:private IDLE_REAP_MS 1000)
(def ^:private STARTUP_IDLE_GRACE_MS 30000)

(defonce ^:private server-state (atom nil))

;; Delivered by `stop!`; `serve-main!` parks on it so a stopped daemon process
;; EXITS instead of idling forever. In-process callers (tests, REPL) deliver
;; harmlessly — nothing is parked on the latch there.
(defonce ^:private serve-exit (promise))
(defonce ^:private idle-reaper (atom nil))

(defn- live-client-ids
  "Client leases that still count for daemon refcount. A lease with a recorded pid
   is ignored once that pid is dead, so a killed TUI does not pin the daemon
   forever. Browser/SSE-only clients have no pid and are counted by the stream
   connection itself."
  []
  (let [clients (:clients @server-state)]
    (->> clients
         (filter (fn [[_ {:keys [pid]}]]
                   (or (nil? pid) (discovery/pid-alive? pid))))
         (map key)
         set)))

(defn- client-count [] (+ (count (live-client-ids)) (count (:sse-clients @server-state))))

(defn- running-turn-count [] (state/running-turn-count))

(defn- status-map
  []
  (let [{:keys [port host db require-token? managed?]} @server-state]
    {:status (if @server-state "running" "stopped")
     :pid (discovery/current-pid)
     :host host
     :port port
     :db (when db (str (discovery/db-target db)))
     :require_token (boolean require-token?)
     :managed (boolean managed?)
     :clients (client-count)
     :running_turns (running-turn-count)}))

(declare stop!)

(defn- idle-shutdown-eligible?
  "True when this daemon is allowed to stop itself. Foreground `vis gateway start`
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
  "Managed daemons also need to reap clients that were SIGKILLed and therefore never
   sent DELETE /v1/clients/:id. Polling is deliberately daemon-local; clients do
   not sweep each other."
  []
  (when (compare-and-set! idle-reaper nil ::starting)
    (reset! idle-reaper (future (try (loop []

                                       (Thread/sleep (long IDLE_REAP_MS))
                                       (when @server-state (maybe-stop-when-idle!) (recur)))
                                     (catch Throwable t
                                       (tel/log! :warn
                                                 ["gateway: idle reaper failed" (ex-message t)]))
                                     (finally (reset! idle-reaper nil)))))))

;; =============================================================================
;; Bearer token (§3)
;; =============================================================================

(defn- default-token-path
  ^Path []
  (Path/of (System/getProperty "user.home") (into-array String [".vis" "gateway.token"])))

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

(defn- path-tid [request] (get-in request [:path-params :tid]))

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

(defn- sse-body
  "Ring streamable body for one SSE subscription. Replay-then-live
   without gaps: `state/subscribe!` registers the sink and captures the
   replay atomically, and the replay writes under the same
   output-stream monitor the live sink locks, so a concurrent fan-out
   blocks until replay lands. The per-connection `last-seq` guard drops
   duplicate deliveries. The heartbeat loop parks this virtual thread
   on the connection and turns a dead client into an IO error ->
   unsubscribe."
  [sid cursor proxied?]
  (reify
    ring-protocols/StreamableResponseBody
      (write-body-to-stream [_ _ output-stream]
        (let [^OutputStream out
              output-stream

              sub-id
              (str (java.util.UUID/randomUUID))

              last-seq
              (atom (long cursor))

              write!
              (fn [event]
                (locking out
                  (when (> (long (get event "seq")) (long @last-seq))
                    (.write out (.getBytes (wire/sse-frame event) StandardCharsets/UTF_8))
                    (.flush out)
                    (reset! last-seq (long (get event "seq"))))))]

          (swap! server-state (fn [st]
                                (-> st
                                    (assoc :saw-client? true)
                                    (update :sse-clients (fnil conj #{}) sub-id))))
          (try (locking out
                 ;; 8KB SSE comment pad (clients ignore comments): proxy edges
                 ;; (Cloudflare tunnel) buffer a streaming body until a byte
                 ;; threshold — without the pad the first real frames sit in
                 ;; the edge buffer and live streaming looks dead. ONLY for
                 ;; proxied requests; direct clients shouldn't pay the bytes.
                 (when proxied?
                   (.write out
                           (.getBytes (str ": " (apply str (repeat 8192 " ")) "\n\n")
                                      StandardCharsets/UTF_8))
                   (.flush out))
                 (doseq [event (state/subscribe! sid sub-id write! @last-seq)]
                   (write! event)))
               (loop []

                 (Thread/sleep (long HEARTBEAT_MS))
                 (locking out
                   (.write out (.getBytes ": ping\n\n" StandardCharsets/UTF_8))
                   (.flush out))
                 (recur))
               (catch Throwable _ nil)
               (finally (state/unsubscribe! sid sub-id)
                        (swap! server-state update :sse-clients disj sub-id)
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
                                      ["cf-ray" "cf-connecting-ip" "x-forwarded-for" "via"])))}
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
  (let [parsed
        (let [raw (get-in request [:query-params "sids"])]
          (when (seq raw)
            (->> (str/split raw #",")
                 (keep (fn [tok]
                         (let [[sid c] (str/split (str/trim tok) #":" 2)
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
   demuxes by session. One shared output stream (locked); a per-session
   `last-seq` guard dedups each session's monotonic stream independently.
   Replays each session (events past its cursor) then goes live; the shared
   heartbeat parks the virtual thread and turns a dead client into an IO
   error → unsubscribe of every session."
  [sid+cursors proxied?]
  (reify
    ring-protocols/StreamableResponseBody
      (write-body-to-stream [_ _ output-stream]
        (let [^OutputStream out
              output-stream

              sub-id
              (str (java.util.UUID/randomUUID))

              last-seqs
              (atom {})

              write!
              (fn [event]
                (let [esid (str (get event "session_id"))]
                  (locking out
                    (when (> (long (get event "seq")) (long (get @last-seqs esid Long/MIN_VALUE)))
                      (.write out (.getBytes (wire/sse-frame event) StandardCharsets/UTF_8))
                      (.flush out)
                      (swap! last-seqs assoc esid (long (get event "seq")))))))]

          (swap! server-state (fn [st]
                                (-> st
                                    (assoc :saw-client? true)
                                    (update :sse-clients (fnil conj #{}) sub-id))))
          (try (locking out
                 (when proxied?
                   (.write out
                           (.getBytes (str ": " (apply str (repeat 8192 " ")) "\n\n")
                                      StandardCharsets/UTF_8))
                   (.flush out)))
               (doseq [[sid cursor] sid+cursors]
                 ;; seed the guard at the requested cursor BEFORE registering the
                 ;; live sink, so a live event racing replay still dedups cleanly.
                 ;; `sid` is a UUID (parse-multi-sids); events demux by the
                 ;; STRING :session_id, so seed the dedup guard under the
                 ;; string key `write!` reads back.
                 (swap! last-seqs assoc (str sid) (long cursor))
                 (doseq [event (state/subscribe! sid sub-id write! cursor)]
                   (write! event)))
               (loop []

                 (Thread/sleep (long HEARTBEAT_MS))
                 (locking out
                   (.write out (.getBytes ": ping\n\n" StandardCharsets/UTF_8))
                   (.flush out))
                 (recur))
               (catch Throwable _ nil)
               (finally (doseq [[sid _] sid+cursors]
                          (state/unsubscribe! sid sub-id))
                        (swap! server-state update :sse-clients disj sub-id)
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
                                            ["cf-ray" "cf-connecting-ip" "x-forwarded-for"
                                             "via"])))}
      (error-response 400 :bad-request "no valid sids"))))

;; =============================================================================
;; /metrics (§6.5)
;; =============================================================================

(defn- prometheus-text
  [{:keys [turns-total turns-failed tokens-input tokens-output cost-total duration-ms-total
           sessions-tracked turns-running]}]
  (str "# TYPE vis_turns_total counter\nvis_turns_total "
       turns-total
       "\n"
       "# TYPE vis_turns_failed_total counter\nvis_turns_failed_total "
       turns-failed
       "\n"
       "# TYPE vis_turn_tokens_total counter\n"
       "vis_turn_tokens_total{kind=\"input\"} "
       tokens-input
       "\n"
       "vis_turn_tokens_total{kind=\"output\"} "
       tokens-output
       "\n"
       "# TYPE vis_turn_cost_usd_total counter\nvis_turn_cost_usd_total "
       cost-total
       "\n"
       "# TYPE vis_turn_duration_ms_total counter\nvis_turn_duration_ms_total "
       duration-ms-total
       "\n"
       "# TYPE vis_sessions_tracked gauge\nvis_sessions_tracked "
       sessions-tracked
       "\n"
       "# TYPE vis_turns_running gauge\nvis_turns_running "
       turns-running
       "\n"))

(defn- metrics-handler
  [request]
  (let [snapshot (state/metrics-snapshot)]
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
  (let [{:keys [token]}
        @server-state

        supplied
        (get-in request [:headers "x-vis-gateway-secret"])]

    (json-response (assoc (status-map)
                     :status "ok"
                     :secret_match (= token supplied)))))

(defn- client-register-handler
  [request]
  (let [{:strs [pid kind]}
        (body-json request)

        client-id
        (str (java.util.UUID/randomUUID))]

    (swap! server-state (fn [st]
                          (-> st
                              (assoc :saw-client? true)
                              (assoc-in
                                [:clients client-id]
                                {:pid pid :kind kind :connected-at (System/currentTimeMillis)}))))
    (json-response {:client_id client-id :status (status-map)})))

(defn- client-release-handler
  [request]
  (let [client-id (get-in request [:path-params :cid])]
    (swap! server-state update :clients dissoc client-id)
    (maybe-stop-when-idle!)
    (json-response {:released true :status (status-map)})))

(defn- status-handler [_] (json-response (status-map)))

(defn- stop-handler
  [_]
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

(defn- configured-provider
  [provider-id]
  (or (some (fn [provider]
              (when (= provider-id (:id provider)) provider))
            (providers/configured-providers))
      {:id provider-id}))

(defn- provider-status-handler
  [request]
  (let [provider-id (some-> (get-in request [:path-params :provider-id])
                            keyword)]
    (json-response {:status (providers/provider-status (configured-provider provider-id))})))

(defn- provider-limits-handler
  [request]
  (let [provider-id (some-> (get-in request [:path-params :provider-id])
                            keyword)]
    (json-response {:report (provider-limits/provider-limits provider-id)})))
(defn- toggle-wire-id [id] (str (namespace id) "/" (name id)))

(defn- toggle-json
  "One settings row as JSON — the wire twin of the web channel's
   `toggle-row` hiccup: boolean rows carry `enabled`, enum rows carry
   `value` + `choices`."
  [{:keys [id label description type]}]
  (let [choices
        (try (toggles/choices-of id) (catch Throwable _ nil))

        value
        (try (toggles/value-of id) (catch Throwable _ nil))

        pretty
        (fn [v]
          (if (keyword? v) (name v) (str v)))

        base
        {:id (toggle-wire-id id)
         :label (str (or label id))
         :type (name (or type (if (seq choices) :enum :boolean)))}]

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
  (let [raw
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

(defn- set-setting-handler
  "POST /v1/settings {id, action} — flip (`toggle`, the default), `cycle`,
   or set an exact enum choice (`value` action with `{value}`) on one
   registered toggle; answers with the refreshed row. JSON body or query
   params both work."
  [request]
  (let [body
        (try (body-json request) (catch Throwable _ nil))

        id-str
        (or (get body "id") (get-in request [:query-params "id"]))

        action
        (str (or (get body "action") (get-in request [:query-params "action"]) "toggle"))

        raw-value
        (or (get body "value") (get-in request [:query-params "value"]))

        [ns* n]
        (when id-str (str/split (str id-str) #"/" 2))

        id
        (when (and ns* (seq (str n))) (keyword ns* n))

        spec
        (when id (toggles/toggle-spec id))]

    (cond (nil? id) (error-response 400 :bad-setting-id "settings id must be <ns>/<name>")
          (nil? spec) (error-response 404 :unknown-setting "no such setting" :id (str id-str))
          :else (do (cond (= action "value")
                          ;; Set an EXACT choice. The wire carries an enum choice as its
                          ;; string name (e.g. "balanced"); map it back to the registered
                          ;; choice (keyword or string) before `set-value!` validates it.
                          (let [choices
                                (toggles/choices-of id)

                                chosen
                                (if (seq choices)
                                  (some #(when (= (name %) (str raw-value)) %) choices)
                                  raw-value)]

                            (when (some? chosen) (toggles/set-value! id chosen)))
                          (= action "cycle") (toggles/cycle-value! id)
                          :else (toggles/set-enabled! id (not (toggles/enabled? id))))
                    (json-response (toggle-json (toggles/toggle-spec id)))))))

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

(defn- list-sessions-handler [_] (json-response {:sessions (state/list-sessions)}))

(defn- soul-handler
  [request]
  (if-let [soul (some-> (path-sid request)
                        state/soul)]
    (json-response soul)
    (session-404 (get-in request [:path-params :sid]))))

(defn- patch-session-handler
  "PATCH /v1/sessions/:sid — rename (`{title}`) OR change project membership
   (`{project_id}`, null to remove from project). Membership takes precedence."
  [request]
  (let [sid
        (path-sid request)

        body
        (body-json request)]

    (cond (not sid) (session-404 (get-in request [:path-params :sid]))
          (contains? body "project_id") (if-let [soul (state/assign-project!
                                                        sid
                                                        (some-> (get body "project_id")
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

(defn- list-projects-handler
  "GET /v1/projects[?owner=…&archived=true] — the owner's projects (projects
   are CROSS-CHANNEL), each with a live session_count."
  [request]
  (let [owner
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
    (if-let [p (some-> (path-pid request)
                       state/get-project)]
      (json-response p)
      (project-404 pid-str))))

(defn- patch-project-handler
  "PATCH /v1/projects/:pid {name?, color?, position?, archived?} — patch a project."
  [request]
  (let [pid-str
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
  "DELETE /v1/projects/:pid — member sessions scatter back to project-less."
  [request]
  (some-> (path-pid request)
          state/delete-project!)
  {:status 204 :headers {} :body nil})

(defn- reorder-project-sessions-handler
  "PATCH /v1/projects/:pid/sessions {order:[sid…]} — persist the manual order of
   the sessions (TUI tabs) inside a project so they stay MOVABLE cross-channel."
  [request]
  (let [pid-str
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
          :else (do (state/reorder-project-sessions! pid order)
                    (json-response {:project_id (str pid) :count (count order)})))))


(defn- submit-turn-handler
  [request]
  (let [sid
        (path-sid request)

        body
        (body-json request)]

    (if (nil? sid)
      (session-404 (get-in request [:path-params :sid]))
      (let [result (state/submit-turn! sid
                                       {:request (get body "request")
                                        :idempotency-key (get body "idempotency_key")
                                        :model (get body "model")
                                        :reasoning-default (some-> (get body "reasoning_default")
                                                                   keyword)
                                        :extra-body (get body "extra_body")
                                        :turn-features (get body "turn_features")
                                        :workspace (get body "workspace")
                                        :attachments (get body "attachments")})]
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
  [request]
  (let [sid (path-sid request)]
    (if (and sid (state/soul sid))
      (json-response {:turns (state/list-turns sid)})
      (session-404 (get-in request [:path-params :sid])))))

(defn- get-turn-handler
  [request]
  (let [sid
        (path-sid request)

        tid
        (path-tid request)]

    (if-let [turn (and sid (state/get-turn sid tid))]
      (json-response turn)
      (error-response 404 :turn-not-found "unknown turn" :turn_id tid))))

(defn- update-queued-turn-handler
  [request]
  (let [sid
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
  (let [sid
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
  (let [sid
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

(defn- drain-idle-handler
  "POST /sessions/:sid/drain-queue — start the session's oldest queued turn iff
   it is idle. Returns `{:turn <started>|nil}`; nil turn means nothing was
   queued or a turn is already running (both benign)."
  [request]
  (let [sid (path-sid request)]
    (if (and sid (state/soul sid))
      (json-response {:turn (state/drain-idle! sid)})
      (session-404 (get-in request [:path-params :sid])))))

(defn- context-handler
  [request]
  (if-let [snapshot (some-> (path-sid request)
                            state/context-snapshot)]
    (json-response snapshot)
    (session-404 (get-in request [:path-params :sid]))))

(defn- transcript-handler
  [request]
  (if-let [sid (path-sid request)]
    (json-response {:turns (state/transcript sid)})
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
   (shell_bg children, managed REPLs, MCP connections, …) FROM THE DAEMON's
   registry. The web channel reads its own in-process registry directly because
   it runs INSIDE the daemon, but the TUI and mobile clients run in a DIFFERENT
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

(defn- resource-restart-handler
  "POST /v1/sessions/:sid/resources/restart?rid=… — run the resource's restart-fn
   in the daemon (it owns re-registration of any changed DATA)."
  [request]
  (if-let [sid (path-sid request)]
    (json-response (resources/restart! sid (req-rid request)))
    (session-404 (get-in request [:path-params :sid]))))

(defn- resource-logs-handler
  "GET /v1/sessions/:sid/resources/logs?rid=… — captured output lines for a
   background via its logs-fn (nil when the resource has none)."
  [request]
  (if-let [sid (path-sid request)]
    (json-response {:lines (resources/logs sid (req-rid request))})
    (session-404 (get-in request [:path-params :sid]))))

(defn- startable->wire
  "Serializable descriptor of ONE startable for a remote Resources UI: identity +
   declared inputs, PLUS the options proposed by running its `:options-fn` with
   the daemon-side session `env` (a client has neither the env nor the workspace
   to compute them). The non-serializable fns (:start-fn/:options-fn/:visible-fn)
   are dropped — the client posts {kind, dir, selected} back and the daemon runs
   :start-fn locally."
  [env {:keys [kind label options-label fields dir? options-fn]}]
  (cond-> {:kind (name kind)
           :label label
           :dir? (boolean dir?)
           :root (str (some-> env
                              :workspace/root))}
    options-label
    (assoc :options-label options-label)

    (seq fields)
    (assoc :fields fields)

    options-fn
    (assoc :options?
      true :options
      (vec (try (options-fn env) (catch Throwable _ nil))))))

(defn- startables-handler
  "GET /v1/sessions/:sid/resources/startables — the declarative startables a
   remote Resources UI (TUI/mobile) can offer, each with its options PROPOSED
   here (daemon-side env). The client renders the pick/options/dir dialogs from
   this list, then posts the collected choice to `resource-start-handler`."
  [request]
  (if-let [sid (path-sid request)]
    (let [env (try (lp/env-for sid) (catch Throwable _ nil))
          sts (vec (try (extension/registered-startable-resources) (catch Throwable _ nil)))]

      (json-response {:startables (mapv #(startable->wire env %) sts)}))
    (session-404 (get-in request [:path-params :sid]))))

(defn- resource-start-handler
  "POST /v1/sessions/:sid/resources/start {kind, dir?, selected?} — resolve the
   startable of `kind` in the daemon's registry and run its `:start-fn` HERE, so
   the spawned background registers in the DAEMON's registry and becomes visible
   to every channel's Resources list (a TUI-started one used to register only in
   the TUI's own process and never showed). No fn crosses the wire; the client
   sends the plain choices it collected. Mirrors the web's in-process start."
  [request]
  (if-let [sid (path-sid request)]
    (let [{:strs [kind dir selected]} (body-json request)
          kw (some-> kind
                     not-empty
                     keyword)
          sr (some #(when (= kw (:kind %)) %)
                   (try (extension/registered-startable-resources) (catch Throwable _ [])))]

      (cond (nil? kw) (error-response 400 :invalid-request "kind is required")
            (nil? sr) (error-response 404 :unknown-startable (str "unknown startable: " kind))
            :else
            ;; Run the start-fn OFF the request thread. A REPL/nREPL boot is
            ;; SYNCHRONOUS and can take tens of seconds (cold JVM + deps download);
            ;; blocking the POST that long freezes the caller's UI and blows past the
            ;; client's 30s HTTP timeout. Return "starting" immediately — the resource
            ;; registers itself in the daemon registry as it comes up (with its own
            ;; :status/:health-fn), so the footer/F4 pick it up on the next poll and a
            ;; boot failure surfaces there as :failed instead of a hung dialog.
            (let [env (cond-> (lp/env-for sid)
                        (not (str/blank? (str dir)))
                        (assoc :startable/dir (str dir)))]
              (future (try ((:start-fn sr) env (not-empty selected))
                           (catch Throwable t
                             (tel/log! :warn
                                       ["gateway: startable failed to start" (name kw)
                                        (ex-message t)]))))
              (json-response {:result "starting" :kind (name kw) :label (:label sr)}))))
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
    (let [idx
          (path-idx request)

          atts
          (state/iteration-attachments (path-iid request))

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



(defn- session-model-handler
  [request]
  (if-let [sid (path-sid request)]
    (json-response {:model (state/session-model-cached sid)})
    (session-404 (get-in request [:path-params :sid]))))

(defn- set-session-model-handler
  [request]
  (if-let [sid (path-sid request)]
    (let [{:strs [provider model]} (body-json request)]
      (state/set-session-model! sid provider model)
      (json-response {:model (state/session-model sid)}))
    (session-404 (get-in request [:path-params :sid]))))

(defn- workspace-handler
  [request]
  (if-let [sid (path-sid request)]
    (json-response {:workspace (state/session-workspace-info sid)})
    (session-404 (get-in request [:path-params :sid]))))

(defn- add-filesystem-root-handler
  [request]
  (if-let [sid (path-sid request)]
    (let [{:strs [path]} (body-json request)]
      (json-response {:workspace (state/add-filesystem-root! sid path)}))
    (session-404 (get-in request [:path-params :sid]))))

(defn- remove-filesystem-root-handler
  [request]
  (if-let [sid (path-sid request)]
    (let [path (or (get (body-json request) "path") (get-in request [:query-params "path"]))]
      (json-response {:workspace (state/remove-filesystem-root! sid path)}))
    (session-404 (get-in request [:path-params :sid]))))

(defn- change-root-handler
  [request]
  (if-let [sid (path-sid request)]
    (let [{:strs [path]} (body-json request)]
      (json-response {:workspace (state/change-root! sid path)}))
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

(defn- voice-asr-resolve
  "Soft-resolve a `foundation-voice.asr` fn (nil when the voice extension is not
   on the classpath)."
  [fn-name]
  (try (requiring-resolve (symbol "com.blockether.vis.ext.foundation-voice.asr" fn-name))
       (catch Throwable _ nil)))

(defn- voice-state->json
  [st]
  (cond-> {:status (name (:state st))}
    (:progress st)
    (assoc :progress (:progress st))

    (:error st)
    (assoc :error (:error st))))

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
  "GET  /v1/sessions/:sid/voice/model — current voice-model state (clients poll
        this before recording).
   POST /v1/sessions/:sid/voice/model — start the background download if the model
        is absent (idempotent; returns immediately).
   JSON: {:status \"ready|downloading|failed|absent|unavailable\" :progress 0..100? :error \"…\"?}."
  [request]
  (let [sid
        (path-sid request)

        model-state
        (voice-asr-resolve "model-state")

        start-dl
        (voice-asr-resolve "start-download!")]

    (cond (not (and sid (state/soul sid)))
          (json-response 404 {:status "unavailable" :error "unknown session"})
          (or (nil? model-state) (nil? start-dl))
          (json-response 501
                         {:status "unavailable" :error "voice extension is not on the classpath"})
          :else (json-response 200
                               (voice-state->json (if (= :post (:request-method request))
                                                    (start-dl)
                                                    (model-state)))))))

(defn- voice-handler
  "POST /v1/sessions/:sid/voice — body is a recorded WAV blob. Transcribes through
   the LOCAL Parakeet model; soft-resolved so a build without vis-foundation-voice
   answers 501. The model must already be installed — the client drives the download
   via /voice/model; a not-ready model answers 425 (Too Early) with the model state,
   NEVER blocking the request thread on the ~465MB download."
  [request]
  (let [sid
        (path-sid request)

        transcribe
        (voice-asr-resolve "transcribe-file!")

        clean
        (try (requiring-resolve (symbol "com.blockether.vis.ext.foundation-voice.input"
                                        "clean-transcript"))
             (catch Throwable _ nil))

        model-state
        (voice-asr-resolve "model-state")]

    (cond (not (and sid (state/soul sid))) (json-response 404 {:error "unknown session"})
          (or (nil? transcribe) (nil? model-state))
          (json-response 501 {:error "voice extension is not on the classpath"})
          (not= :ready (:state (model-state))) (json-response 425 (voice-state->json (model-state)))
          :else (let [tmp (java.io.File/createTempFile "vis-voice" ".wav")]
                  (try (with-open [in ^java.io.InputStream (:body request)
                                   out (io/output-stream tmp)]

                         (io/copy in out))
                       (if-not (wav-file? tmp)
                         (json-response 400 {:error "body must be a RIFF/WAVE audio file"})
                         (json-response 200
                                        {:text (let [raw (str/trim (str (transcribe (str tmp))))]
                                                 (if clean (clean raw) raw))}))
                       (catch Throwable t
                         (tel/log!
                           {:level :error :id ::voice-transcribe-failed :data {:error (str t)}})
                         (json-response 400
                                        {:error (or (ex-message t)
                                                    (->> (iterate (fn [^Throwable x]
                                                                    (some-> x
                                                                            .getCause))
                                                                  t)
                                                         (take-while some?)
                                                         (map str)
                                                         (str/join " <- ")))}))
                       (finally (.delete tmp)))))))

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
    (let [kind
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
;; dropped + `vis ext reload`) and rebuilds — both orders just work.
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
        (let [uri (str (:uri request))
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
                (error-response 401 :unauthorized "missing or invalid bearer token"))))))))

(defn- wrap-errors
  [handler]
  (fn [request]
    (try (handler request)
         (catch Throwable t
           (tel/log! :error ["gateway: unhandled request error" (:uri request) (ex-message t)])
           (error-response 500 :engine-error (or (ex-message t) "internal error"))))))

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
        ["/settings" {:get list-settings-handler :post set-setting-handler}]
        ["/providers/:provider-id/status" {:get provider-status-handler}]
        ["/providers/:provider-id/limits" {:get provider-limits-handler}]
        ["/clients" {:post client-register-handler}]
        ["/clients/:cid" {:delete client-release-handler}] ["/admin/status" {:get status-handler}]
        ["/admin/stop" {:post stop-handler}]
        ["/sessions" {:get list-sessions-handler :post create-session-handler}]
        ["/projects" {:get list-projects-handler :post create-project-handler}]
        ["/projects/actions/ensure" {:post ensure-project-for-root-handler}]
        ["/projects/:pid"
         {:get get-project-handler :patch patch-project-handler :delete delete-project-handler}]
        ["/projects/:pid/sessions" {:patch reorder-project-sessions-handler}]
        ["/sessions/:sid"
         {:get soul-handler :patch patch-session-handler :delete delete-session-handler}]
        ["/sessions/:sid/release" {:post release-session-handler}]
        ["/sessions/:sid/events" {:get events-handler}]
        ["/sessions/:sid/voice" {:post voice-handler}]
        ["/sessions/:sid/voice/model" {:get voice-model-handler :post voice-model-handler}]
        ["/sessions/:sid/events-since" {:get events-since-handler}]
        ["/sessions/:sid/seq" {:get seq-handler}] ["/sessions/:sid/context" {:get context-handler}]
        ["/sessions/:sid/transcript" {:get transcript-handler}]
        ["/sessions/:sid/resources" {:get resources-handler}]
        ["/sessions/:sid/resources/startables" {:get startables-handler}]
        ["/sessions/:sid/resources/start" {:post resource-start-handler}]
        ["/sessions/:sid/resources/stop" {:post resource-stop-handler}]
        ["/sessions/:sid/resources/restart" {:post resource-restart-handler}]
        ["/sessions/:sid/resources/logs" {:get resource-logs-handler}]
        ["/sessions/:sid/iterations/:iid/attachments/:idx" {:get attachment-bytes-handler}]
        ["/sessions/:sid/model" {:get session-model-handler :patch set-session-model-handler}]
        ["/sessions/:sid/workspace" {:get workspace-handler}]
        ["/sessions/:sid/workspace/roots"
         {:post add-filesystem-root-handler :delete remove-filesystem-root-handler}]
        ["/sessions/:sid/workspace/root" {:patch change-root-handler}]
        ["/sessions/:sid/suggest" {:get suggest-handler}]
        ["/sessions/:sid/turns" {:get list-turns-handler :post submit-turn-handler}]
        ["/sessions/:sid/turns/:tid"
         {:get get-turn-handler
          :patch update-queued-turn-handler
          :delete delete-queued-turn-handler}]
        ["/sessions/:sid/turns/:tid/trace" {:get turn-trace-handler}]
        ["/sessions/:sid/turns/:tid/cancel" {:post cancel-turn-handler}]
        ["/sessions/:sid/drain-queue" {:post drain-idle-handler}]]]
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
                        ;; OWN 404 (e.g. the web UI's styled HTML page) instead
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
    (wrap-scoped-params contribs)
    (wrap-scoped-multipart contribs)
    (ring-cookies/wrap-cookies)
    (wrap-errors)))

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
  "Hydrate feature toggles from the `:toggles` slot of ~/.vis/config.edn
   and install a listener that writes every change back. Mirrors the
   TUI's wiring in `channel-tui/screen.clj` so a toggle flipped from the
   web channel (or any gateway client) survives a gateway restart -
   without this, only TUI-hosted processes ever persisted toggles.
   Idempotent: hydration re-runs harmlessly; the save listener installs
   once per process."
  []
  (try (toggles/hydrate-from-config! (or (config/load-config-raw) {}))
       (when (compare-and-set! toggle-persist-listener-installed? false true)
         (toggles/add-listener!
           (fn [_event]
             (try (let [raw (or (config/load-config-raw) {})]
                    (config/save-config! (assoc raw :toggles (toggles/snapshot))))
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

    (let [outcome (try {:server (jetty/run-jetty handler opts)}
                       (catch Throwable t
                         (if (and (bind-failure? t)
                                  (< (System/currentTimeMillis) (long deadline-ms)))
                           ::retry
                           (throw t))))]
      (if (= outcome ::retry) (do (Thread/sleep 150) (recur)) (:server outcome)))))

(defn start!
  "Start the gateway on the Ring Jetty adapter with virtual threads.
   Returns `{:port :host :token-file}`. Throws when already running.
   Safe to call from any host process - the daemon (`vis gateway start`), a TUI
   run, or an embedded caller."
  ([] (start! {}))
  ([{:keys [port host token-file require-token? db managed?]}]
   (when @server-state (throw (ex-info "gateway already running" {:type :gateway/already-running})))
   (let [port
         (int (or port DEFAULT_PORT))

         host
         (or host DEFAULT_HOST)

         loopback?
         (= host DEFAULT_HOST)

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

         ;; Hydrate persisted toggles + install the config.edn save
         ;; listener so web/gateway-driven flips survive restarts.
         _
         (install-toggle-persistence!)

         server
         (try
           (start-jetty!
             serving-handler
             {:port port :host host :join? false :virtual-threads? true :send-server-version? false}
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
                           :sse-clients #{}
                           :require-token? require-token?
                           :managed? (boolean managed?)
                           :started-at-ms (System/currentTimeMillis)
                           :saw-client? false})
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
   down, so a SIGTERM / `vis gateway restart` landing mid-turn lets active work
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
    ;; below (killing every session's shell_bg children + REPLs) can eat
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
        ;; reach a terminal state; it does NOT keep the JVM's shared HttpClient
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
    ;; Kill every session's background resources (shell_bg children, REPLs)
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

(defn serve-main!
  "Blocking entry for the `vis gateway start` command: start, print the
   connection line, park forever (Ctrl-C / SIGTERM stops the JVM)."
  [{:keys [port host token-file require-token? db managed? pair?]}]
  ;; Profile the daemon into its own JFR file when VIS_JFR is inherited from the
  ;; client that spawned us (idempotent with the -main call for direct callers).
  (try ((requiring-resolve 'com.blockether.vis.internal.jfr/maybe-start!) "gateway")
       (catch Throwable _ nil))
  (let [{:keys [port host token-file require-token?]} (start! {:port (some-> port
                                                                             parse-long)
                                                               :host host
                                                               :token-file token-file
                                                               :require-token? require-token?
                                                               :db db
                                                               :managed? managed?})]
    (println (str "vis gateway listening on http://" host ":" port))
    (if require-token?
      (println (str "bearer token: " token-file))
      (println "auth: disabled (loopback default; pass --require-token to enable)"))
    (when pair?
      (pairing/print-pairing! {:host host
                               :port port
                               :token (some-> token-file
                                              slurp
                                              str/trim)
                               :require-token? require-token?}))
    (.addShutdownHook (Runtime/getRuntime) (Thread. ^Runnable stop!))
    @serve-exit
    (System/exit 0)))
