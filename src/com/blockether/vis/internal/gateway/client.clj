(ns com.blockether.vis.internal.gateway.client
  "HTTP/SSE client for the long-lived gateway daemon.

   Interactive channels call this facade instead of `gateway.state` directly. It
   discover-or-starts the one daemon for the current DB, then speaks the same
   HTTP/SSE API every other client uses. This is the thin-client half of the
   gateway-daemon plan: token refresh, turn execution, and live streaming happen
   in ONE process."
  (:require [babashka.http-client :as http]
            [clojure.string :as str]
            [com.blockether.vis.internal.config :as config]
            [com.blockether.vis.internal.gateway.discovery :as discovery]
            [com.blockether.vis.internal.gateway.wire :as wire])
  (:import (java.io BufferedReader InputStream InputStreamReader)
           (java.net URLEncoder)
           (java.nio.charset StandardCharsets)))

(def ^:private DEFAULT_PORT 7890)

(def ^:private DEFAULT_HOST "127.0.0.1")

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
;; Freshness debounce: `ensure-gateway!` verifies the daemon with a full HTTP
;; GET /healthz probe on EVERY call. The TUI footer/poll loop calls it dozens of
;; times a second, so that doubled every gateway request (probe + real call) and
;; dominated client-side allocation (Clojure reflective interop on the OLD raw
;; java.net.http response + HTTP futures) on the render JVM. The JSON layer is
;; charred (reflection-free); interop was the churn. We keep the belt-and-suspenders probe but only re-run it once per
;; `entry-probe-ttl-ms`; within the window a cheap pid-liveness check suffices.
(def ^:private entry-probe-ttl-ms 4000)

(defonce ^:private entry-fresh-until-ns (atom 0))

(defonce ^:private client-id (atom nil))

(defonce ^:private release-hook-installed? (atom false))

(defonce ^:private subscriptions (atom {}))

(defn- db-target [] (config/resolve-db-spec))

(defn- enc [x] (URLEncoder/encode (str x) StandardCharsets/UTF_8))

(defn- base-url
  [{:keys [host port]}]
  (str "http://" (or host DEFAULT_HOST) ":" (or port DEFAULT_PORT)))

(defn- gw-send!
  "Perform a babashka.http-client request against gateway `entry`. `method` is a
   verb string (\"GET\"/\"POST\"/\"PATCH\"/\"DELETE\"/…); `opts` may carry `:body`
   (serialized to JSON) and `:as` ∈ #{:string :bytes :stream} (default :string).
   `:throw false`, so 4xx/5xx come back as a response map (callers branch on
   `:status`) exactly like the old `HttpResponse.statusCode`. A :stream request
   asks for `text/event-stream` with compression disabled so the SSE body stays
   byte-live for the line reader + idle watchdog."
  [{:keys [secret] :as _entry} method path {:keys [body as] :or {as :string}}]
  (http/request (cond->
                  {:client @http-client
                   :method (keyword (str/lower-case method))
                   :uri (str (base-url _entry) path)
                   :timeout 30000
                   :throw false
                   :as as
                   :headers (cond->
                              {"Accept" (if (= as :stream) "text/event-stream" "application/json")
                               "X-Vis-Gateway-Secret" (str secret)}
                              (= as :stream)
                              (assoc "Accept-Encoding" "identity"))}
                  (some? body)
                  (-> (assoc :body (wire/json-str body))
                      (assoc-in [:headers "Content-Type"] "application/json")))))

(defn- parse-json-body [^String body] (or (wire/parse-json body) {}))

(declare ensure-gateway! ensure-client! ensure-gateway-serving!)

(defn- send-json-with-entry!
  ([entry method path] (send-json-with-entry! entry method path nil))
  ([entry method path body]
   (let
     [response
      (gw-send! entry method path {:body body})

      status
      (long (:status response))

      parsed
      (parse-json-body (:body response))]

     (when (>= status 400)
       (throw (if (= status 401)
                (ex-info (str "could not authenticate to the gateway (HTTP 401: "
                              (or (get parsed "message") "unauthorized")
                              "). It is bound to a "
                              "non-loopback host, so a bearer token is required and this "
                              "client did not present a valid one. Run the TUI on the SAME "
                              "machine as the gateway (it reads the token from ~/.vis), or "
                              "restart the gateway on loopback (vis gateway start).")
                         (assoc parsed
                           :http-status status
                           :vis/user-error true))
                (ex-info (or (get parsed "message") (str "gateway HTTP " status))
                         (assoc parsed :http-status status)))))
     parsed)))

(defn- send-json!
  ([method path] (send-json! method path nil))
  ([method path body]
   (let [entry (ensure-gateway!)]
     (ensure-client! entry)
     (send-json-with-entry! entry method path body))))

(defn- probe-entry?
  [entry]
  (try (let
         [response
          (gw-send! entry "GET" "/healthz" {})

          body
          (parse-json-body (:body response))]

         (and (= 200 (:status response))
              (= "ok" (get body "status"))
              (true? (get body "secret_match"))))
       (catch Throwable _ false)))

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
  (let
    [tty
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
       (if tty (.print err (str "\r\u001b[K⟳ " label "…")) (.println err (str "vis: " plain)))
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
             (start "starting gateway daemon" "starting gateway daemon…")

             :awaiting
             (start "another vis is starting the gateway — waiting"
                    "another vis is starting the gateway — waiting…")

             :tick
             (when (and tty (:active @state))
               (let
                 [{:keys [label frame]}
                  @state

                  f
                  (nth spinner-frames (mod frame (count spinner-frames)))

                  secs
                  (format "%.1f" (/ (double (or elapsed-ms 0)) 1000.0))]

                 (swap! state update :frame inc)
                 (.print err (str "\r\u001b[K" f " " label "… " secs "s"))
                 (.flush err)))

             :ready
             (finish (str "✓ gateway ready" (when (= mode :awaited) " (started by another vis)")))

             :timeout
             (finish "✗ gateway did not become ready in time")

             nil)
           (catch Throwable _ nil)))))

(defn ensure-gateway!
  "Return a fresh daemon registry entry for the current DB, auto-starting the
   detached gateway if needed. `:memory` is a programmer error for this client;
   headless one-shots stay in-process and should not call here.

   Optional `:port`/`:host` overrides the bind used WHEN THIS CALL SPAWNS a fresh
   daemon (e.g. `vis channels web --port`); a fresh daemon already registered for
   the DB is a singleton and is attached to as-is, so the override is moot there.

   Freshness is DEBOUNCED: the full HTTP /healthz probe (via `probe-entry?`)
   runs at most once per `entry-probe-ttl-ms`. Within that window a cached entry
   whose pid is still alive is trusted directly, so the TUI's chatty poll loop
   stops paying for a doubled HTTP round-trip (and its JSON/reflection churn) on
   every gateway call."
  ([] (ensure-gateway! nil))
  ([{:keys [port host]}]
   (let [db (db-target)]
     (when (discovery/memory-db? db)
       (throw (ex-info "gateway daemon is disabled for :memory DB" {:type :gateway/no-daemon})))
     (let
       [cached @cached-entry
        now (System/nanoTime)
        fresh-until (long @entry-fresh-until-ns)
        fresh? (if (and (map? cached) (< now fresh-until) (discovery/pid-alive? (:pid cached)))
                 true
                 ;; Window elapsed (or no cached entry): pay for the real
                 ;; HTTP probe once, then re-open the debounce window.
                 (when (discovery/registry-fresh? cached probe-entry?)
                   (reset! entry-fresh-until-ns (+ now (* (long entry-probe-ttl-ms) 1000000)))
                   true))]

       (if fresh?
         cached
         ;; Native-image startup can exceed 8s while SQLite/Flyway initializes;
         ;; JVM source boot (dev) needs ~30s to load Clojure + extensions before
         ;; it self-registers, so give it a much longer runway.
         (let
           [{:keys [entry] :as result} (discovery/discover-or-start!
                                         {:db db
                                          :port (or port DEFAULT_PORT)
                                          :host (or host DEFAULT_HOST)}
                                         :probe probe-entry?
                                         :on-event (progress-reporter)
                                         :timeout-ms (if (discovery/native-image?) 15000 60000))]
           (if entry
             (do (reset! cached-entry entry)
                 (reset! entry-fresh-until-ns (+ (System/nanoTime)
                                                 (* (long entry-probe-ttl-ms) 1000000)))
                 entry)
             (throw (ex-info "gateway daemon did not become ready"
                             (assoc result :type :gateway/start-timeout))))))))))

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
    (.addShutdownHook (Runtime/getRuntime) (Thread. ^Runnable release-client!))))

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
        (let
          [response
           (send-json-with-entry! entry
                                  "POST"
                                  "/v1/clients"
                                  {:pid (discovery/current-pid) :kind "clojure-client"})

           registered-id
           (get response "client_id")]

          (when-not (seq registered-id)
            (throw (ex-info "gateway client registration returned no client_id"
                            {:type :gateway/invalid-client-registration})))
          (reset! client-id registered-id)
          (ensure-release-hook!)))))
  @client-id)

(defn create-session! [opts] (send-json! "POST" "/v1/sessions" opts))

(defn soul
  [sid]
  (try (send-json! "GET" (str "/v1/sessions/" (enc sid)))
       (catch clojure.lang.ExceptionInfo e
         (when-not (= 404 (:http-status (ex-data e))) (throw e)))))

(defn list-sessions
  ([] (get (send-json! "GET" "/v1/sessions") "sessions"))
  ([_channel] (list-sessions)))

(defn close-session! [sid] (send-json! "DELETE" (str "/v1/sessions/" (enc sid))))

;; --- Projects (cross-channel) + movable project sessions + ownership (V6/V7) ---

(defn list-projects
  "GET /v1/projects — projects are CROSS-CHANNEL. `opts`: :owner (string),
   :archived? (bool). Returns the :projects vector."
  ([] (list-projects nil))
  ([{:keys [owner archived?]}]
   (let
     [qs
      (->> [(when owner (str "owner=" (enc owner))) (when archived? "archived=true")]
           (remove nil?)
           (str/join "&"))

      path
      (cond-> "/v1/projects"
        (seq qs)
        (str "?" qs))]

     (get (send-json! "GET" path) "projects"))))

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

(defn delete-project! [pid] (send-json! "DELETE" (str "/v1/projects/" (enc pid))))

(defn assign-project!
  "Assign a session to a project (nil clears / removes from project). Returns the soul."
  [sid pid]
  (send-json! "PATCH" (str "/v1/sessions/" (enc sid)) {:project_id (when pid (str pid))}))

(defn reorder-project-sessions!
  "Persist the manual order of a project's sessions (TUI tabs). `session-ids` is
   the desired ordering."
  [pid session-ids]
  (send-json! "PATCH" (str "/v1/projects/" (enc pid) "/sessions") {:order (mapv str session-ids)}))

(defn release-session-runtime!
  "Release a session's live RUNTIME on the daemon WITHOUT touching the process
   client lease: stop its background resources (shell_bg children, managed REPLs)
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
   stop the session's background resources (shell_bg children, REPLs) and drop
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
  [sid]
  (get (send-json! "GET" (str "/v1/sessions/" (enc sid) "/transcript")) "turns"))

(defn transcript-md
  "The gateway-rendered user/assistant dialog Markdown for `sid` — the canonical
   `transcript->md :dialog`. Returns the string, or nil on a non-2xx."
  [sid]
  (let
    [entry
     (ensure-gateway!)

     _
     (ensure-client! entry)

     response
     (gw-send! entry "GET" (str "/v1/sessions/" (enc sid) "/transcript.md") {:as :string})]

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
  (let
    [k
     (str sid)

     now
     (System/currentTimeMillis)

     {:keys [at val]}
     (get @session-model-cache k)]

    (when-not (and at (< (- now (long at)) (long session-model-cache-ttl-ms)))
      (refresh-session-model! sid k))
    val))

;; ---------------------------------------------------------------------------
;; Managed resources (backgrounds) — the daemon owns the registry (the agent's
;; tools register here while a turn runs IN THE DAEMON), so a client in another
;; process reads/controls them over HTTP. An in-process client uses the
;; local registry directly and never touches these.
;; ---------------------------------------------------------------------------

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
  (let
    [k
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

(defn restart-resource!
  "Run the resource's restart-fn in the daemon. Returns the restart result map."
  [sid rid]
  (send-json! "POST" (str "/v1/sessions/" (enc sid) "/resources/restart?rid=" (enc rid))))

(defn resource-logs
  "Captured output lines for a background via its daemon-side logs-fn, or nil."
  [sid rid]
  (get (send-json! "GET" (str "/v1/sessions/" (enc sid) "/resources/logs?rid=" (enc rid))) "lines"))

(defn list-startables
  "Declarative startables the session can offer, each with the options the daemon
   PROPOSED from its own env — canonical string-keyed wire descriptors, the SAME
   shape the in-process web modal reads. Drives a remote 'add
   background' flow: the client renders the dialogs from this, then posts the
   chosen one to `start-resource!`."
  [sid]
  (get (send-json! "GET" (str "/v1/sessions/" (enc sid) "/resources/startables")) "startables"))

(defn start-resource!
  "Start the declared startable in the DAEMON (arg map {:kind :dir :selected}), so
   the spawned background registers there and appears in every channel's Resources
   list. Returns the daemon's result map ({:result \"started\"|\"error\" …})."
  [sid {:keys [kind dir selected]}]
  (send-json! "POST"
              (str "/v1/sessions/" (enc sid) "/resources/start")
              {:kind kind :dir dir :selected selected}))

(defn iteration-attachment-bytes
  "Raw bytes (a byte-array) of ONE outbound artifact — iteration `iid`, its 0-based
   `idx` in the iteration's ordered attachment list — fetched from the daemon's
   attachment byte endpoint, or nil (404 / no bytes). The lazy-fetch companion to
   a live `iteration.completed` attachment descriptor: a client sees `{:index
   :media_type …}` on the frame, then pulls the bytes here. HISTORY resolves the
   same way (the trace iteration's `:id` + attachment index)."
  [sid iid idx]
  (let
    [entry
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
  (let
    [pref (pref<-wire (get (send-json! "PATCH"
                                       (str "/v1/sessions/" (enc sid) "/model")
                                       {:provider provider :model model})
                           "model"))]
    (swap! session-model-cache assoc (str sid) {:at (System/currentTimeMillis) :val pref})
    pref))

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

(defn add-filesystem-root!
  "Add `path` as an extra filesystem root for `sid` IN THE DAEMON, returning the
   refreshed `session-workspace-info`. The daemon owns the session's DB, so the
   new root is what every channel reads back (fixing the local-only mutation that
   never reached the running session)."
  [sid path]
  (decode-workspace
    (get (send-json! "POST" (str "/v1/sessions/" (enc sid) "/workspace/roots") {:path path})
         "workspace")))

(defn remove-filesystem-root!
  "Remove `path` from `sid`'s extra filesystem roots IN THE DAEMON, returning the
   refreshed `session-workspace-info`."
  [sid path]
  (decode-workspace
    (get (send-json! "DELETE" (str "/v1/sessions/" (enc sid) "/workspace/roots") {:path path})
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
  "Tid-less cancel: kill whatever turn currently holds `sid`'s `:current-turn`
   slot in the daemon. For callers that lost (or never learned) the gateway
   turn id. Returns the parsed body (`{\"status\" \"cancelling\", \"turn_id\" tid}`);
   throws on HTTP error (409 when the session is idle)."
  [sid]
  (send-json! "POST" (str "/v1/sessions/" (enc sid) "/cancel-current")))

(defn drain-idle! [sid] (send-json! "POST" (str "/v1/sessions/" (enc sid) "/drain-queue")))

(defn reconcile-running-turns!
  "Clients do not sweep. Only the daemon may reconcile its own startup orphans."
  []
  nil)

(defn status
  []
  (let
    [db
     (db-target)

     entry
     (discovery/read-registry db)]

    (if (discovery/registry-fresh? entry probe-entry?)
      (send-json-with-entry! entry "GET" "/v1/admin/status")
      {:status "stopped" :db (when-not (discovery/memory-db? db) (str (discovery/db-target db)))})))

(defn pairing-info
  "Connection details for the daemon registered for the current DB, so a caller
   can build a companion pairing QR on demand (not only at `--pair` boot time).
   Returns {:running? :host :port :token :loopback?}; `:running?` is false when no
   fresh daemon is registered, and `:loopback?` flags a 127.0.0.1/::1/localhost
   bind that a phone can never reach."
  []
  (let
    [db
     (db-target)

     {:keys [host port secret] :as entry}
     (discovery/read-registry db)]

    (if (discovery/registry-fresh? entry probe-entry?)
      {:running? true
       :host host
       :port port
       :token secret
       :loopback? (contains? #{"127.0.0.1" "::1" "localhost"} (str host))}
      {:running? false})))

(defn stop-daemon!
  []
  (let
    [db
     (db-target)

     entry
     (discovery/read-registry db)]

    (if (discovery/registry-fresh? entry probe-entry?)
      (let [res (send-json-with-entry! entry "POST" "/v1/admin/stop")]
        (reset! cached-entry nil)
        (reset! client-id nil)
        res)
      {:status "stopped" :stopping false})))

(defn- port-free?
  "True when nothing is accepting TCP connections on host:port — i.e. a previous
   daemon has fully released it, so a respawn on the same port won't bind-race."
  [^String host port]
  (try (with-open [sock (java.net.Socket.)]
         (.connect sock (java.net.InetSocketAddress. host (int port)) 200)
         false)
       (catch Throwable _ true)))

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
   construction, carries the route. This is what lets `vis channels web`
   self-heal instead of parking on a `/ui` that 404s.

   Optional `opts` (`{:port :host}`) overrides the bind used when THIS call has
   to spawn a fresh daemon (the `vis channels web --port/--host` flags); it is
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
     (case (probe-route entry path)
       ;; Mounted — or a transient transport blip we must not misread as "missing".
       (:served :unreachable)
       entry

       :absent
       (let
         [st (status)
          clients (long (or (get st "clients") 0))
          running (long (or (get st "running_turns") 0))]

         (when (or (> clients 1) (pos? running))
           (throw (ex-info (str "gateway daemon does not serve " path
                                " but is in use (" clients
                                " client(s), " running
                                " running turn(s)); refusing" " to force-restart a shared daemon")
                           {:type :gateway/route-missing-busy
                            :path path
                            :clients clients
                            :running-turns running})))
         (stop-daemon!)
         (await-daemon-down! (db-target) (:host entry) (:port entry))
         (let [entry (ensure-gateway! opts)]
           (when-not (= :served (probe-route entry path))
             (throw (ex-info
                      (str "gateway daemon is not serving " path " even after a fresh restart")
                      {:type :gateway/route-missing :path path})))
           entry))))))

(defn provider-status
  [provider-id]
  (let
    [path
     (str "/v1/providers/" (enc (name provider-id)) "/status")

     entry
     (ensure-gateway-serving! path)]

    (ensure-client! entry)
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
    (cond->
      {:id (wire-enum (get row "id"))
       :label (get row "label")
       :scope (wire-enum (get row "scope"))
       :kind (wire-enum (get row "kind"))
       :precision (wire-enum (get row "precision"))
       :source (wire-enum (get row "source"))
       :unlimited? (get row "is_unlimited")}
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
    (let
      [static
       (or (get report "static") {})

       dynamic
       (or (get report "dynamic") {})

       limits
       (get dynamic "limits")

       error
       (get report "error")]

      (cond->
        {:provider-id (wire-enum (get report "provider_id"))
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
  (let
    [path
     (str "/v1/providers/" (enc (name provider-id)) "/limits")

     entry
     (ensure-gateway-serving! path)]

    (ensure-client! entry)
    (provider-limits<-wire (get (send-json-with-entry! entry "GET" path) "report"))))

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
  (let
    [failed?
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
    (cond->
      (-> (merge (select-keys message wire/turn-meta-keys)
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
   babashka.http-client response map whose `:body` is a live `InputStream`."
  [sid cursor]
  (let [entry (ensure-gateway!)]
    (gw-send! entry
              "GET"
              (str "/v1/sessions/" (enc sid) "/events?cursor=" (long (or cursor 0)))
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
  (let
    [check-ms
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
               (do (try (Thread/sleep check-ms) (catch InterruptedException _ nil)) (recur)))))))]

    (doto (Thread. ^Runnable runnable "vis-gateway-sse-idle-watchdog") (.setDaemon true) (.start))))


(def ^:private sse-reconnect-max-attempts
  "How many times a blocking turn stream reconnects after the daemon drops the
   connection mid-turn before giving up and surfacing a disconnect error."
  5)

(def ^:private sse-reconnect-backoff-ms 250)

(defn- open-sse-events!
  "Open ONE SSE connection for `sid` from `cursor` and drive the raw
   `data:`-line/blank-line frame parser. For each parsed event: advance `cursor*`
   (highest `:seq` seen) then call `(handle event)`. When `handle` returns a
   truthy value, stop and return it (a terminal signal); otherwise keep reading.
   Resets `stream*` (when non-nil) to the live InputStream so `unsubscribe!` can
   close it. An idle watchdog (see [[sse-idle-timeout-ms]]) closes the stream if
   NO frame — not even a heartbeat — arrives for too long, so a wedged daemon
   surfaces as a normal drop instead of an infinite park. Returns `[:closed]` on
   EOF (or an idle/close-driven read failure); throws `ex-info` with
   `:http-status` on a non-200 response. Shared by `read-sse-stream!` (blocking
   turns) and `subscribe!` (the live mirror) so the frame parsing lives in ONE
   place."
  [sid cursor cursor* stream* handle & [on-open]]
  (let [response (sse-response! sid cursor)]
    (when-not (= 200 (:status response))
      (throw (ex-info (str "gateway SSE HTTP " (:status response))
                      {:http-status (:status response)})))
    (with-open
      [^InputStream in (:body response)
       rdr (BufferedReader. (InputStreamReader. in StandardCharsets/UTF_8))]

      (when stream* (reset! stream* in))
      (when on-open (on-open))
      (let
        [last-line-ns* (atom (System/nanoTime))
         alive?* (atom true)
         watchdog (start-sse-idle-watchdog! in last-line-ns* alive?*)]

        (try (loop [data-lines []]
               (if-let [line (.readLine rdr)]
                 (do (reset! last-line-ns* (System/nanoTime))
                     (if (str/blank? line)
                       (let
                         [data (str/join "\n" data-lines)
                          event (when (seq data) (wire/parse-json data))]

                         (if-not event
                           (recur [])
                           (do (when-let [s (get event "seq")]
                                 (swap! cursor* max (long s)))
                               (or (handle event) (recur [])))))
                       (if (str/starts-with? line "data: ")
                         (recur (conj data-lines (subs line 6)))
                         (recur data-lines))))
                 [:closed]))
             (finally (reset! alive?* false)
                      (some-> ^Thread watchdog
                              .interrupt)))))))

(defn subscribe!
  "Remote equivalent of gateway.state/subscribe!: start a background SSE reader
   that replays `cursor` then calls `sink` for every live event. Returns an empty
   replay vector because the gateway's SSE endpoint itself handles replay before
   live delivery."
  [sid sub-id sink cursor]
  (let
    [entry
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
         (let
           [dropped? (try (open-sse-events! sid
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
           (when (and dropped? (contains? @subscriptions sub-id))
             (try (sink {:type "gateway.disconnected"}) (catch Throwable _ nil))
             (let
               [delay-ms (long (min 5000 (* (long sse-reconnect-backoff-ms) (inc (long attempt)))))
                interrupted?
                (try (Thread/sleep delay-ms) false (catch InterruptedException _ true))]

               (when-not interrupted? (recur (inc attempt)))))))
       (swap! subscriptions dissoc sub-id))]

    (swap! subscriptions assoc sub-id {:future fut :stream stream*})
    []))

(defn unsubscribe!
  [_sid sub-id]
  (when-let [{:keys [future stream]} (get @subscriptions sub-id)]
    (try (some-> ^java.io.Closeable @stream
                 .close)
         (catch Throwable _ nil))
    (future-cancel future)
    (swap! subscriptions dissoc sub-id))
  nil)

;; ---------------------------------------------------------------------------
;; Multiplexed subscription: ONE SSE connection for MANY sessions.
;;
;; A channel watching N sessions previously opened N SSE sockets (+N client
;; futures +N server heartbeat threads). `mux-subscribe!` instead folds every
;; watched session down a SINGLE process-wide connection to `/v1/events?sids=…`,
;; demuxed by each event's `:session_id`. Opening/closing a tab just edits the
;; session set and reconnects (resuming each session from its advanced cursor).
;; ---------------------------------------------------------------------------

(defonce ^:private mux
  ;; {:subs {sid {:sinks {sub-id fn} :cursor-atom atom<long>}}
  ;;  :epoch long :future f :stream in}
  (atom {:subs {} :epoch 0 :future nil :stream nil}))

(declare mux-unsubscribe!)

(defn- mux-sids-param
  "Comma list of `sid:cursor` for the current session set (UUIDs are URL-safe,
   so no encoding needed). Cursors are read live, so a reconnect resumes each
   session from the highest seq already delivered — no replay churn, no gaps."
  [subs]
  (->> subs
       (map (fn [[sid {:keys [cursor-atom]}]]
              (str sid ":" (long @cursor-atom))))
       (str/join ",")))

(defn- mux-broadcast!
  "Deliver a synthetic connection event to EVERY live sink (shared stream =
   shared connection state), so each tab still paints a live/lost indicator."
  [type]
  (doseq
    [[_ {:keys [sinks]}]
     (:subs @mux)

     [_ sink]
     sinks]

    (try (sink {:type type}) (catch Throwable _ nil))))

(defn- open-mux-events!
  "Open ONE multiplexed SSE connection for the current session set and drive
   the raw `data:`/blank-line frame parser. Each parsed event is demuxed by
   `:session_id`: advance that session's cursor, then call its sink. Bails with
   `[:epoch-changed]` the moment the session set is edited (so the caller
   reconnects with the new set) and `[:closed]` on EOF/drop. Throws with
   `:http-status` on a non-200."
  [my-epoch]
  (let
    [entry
     (ensure-gateway!)

     _
     (ensure-client! entry)

     response
     (gw-send! entry "GET" (str "/v1/events?sids=" (mux-sids-param (:subs @mux))) {:as :stream})]

    (when-not (= 200 (:status response))
      (throw (ex-info (str "gateway mux SSE HTTP " (:status response))
                      {:http-status (:status response)})))
    (with-open
      [^InputStream in
       (:body response)

       rdr
       (BufferedReader. (InputStreamReader. in StandardCharsets/UTF_8))]

      (swap! mux assoc :stream in)
      (mux-broadcast! "gateway.connected")
      (let
        [last-line-ns*
         (atom (System/nanoTime))

         alive?*
         (atom true)

         watchdog
         (start-sse-idle-watchdog! in last-line-ns* alive?*)]

        (try (loop [data-lines []]
               (if (not= my-epoch (:epoch @mux))
                 [:epoch-changed]
                 (if-let [line (.readLine rdr)]
                   (do (reset! last-line-ns* (System/nanoTime))
                       (if (str/blank? line)
                         (let
                           [data (str/join "\n" data-lines)
                            event (when (seq data) (wire/parse-json data))]

                           (when event
                             (let
                               [esid (str (get event "session_id"))
                                {:keys [sinks cursor-atom]} (get (:subs @mux) esid)]

                               (when (seq sinks)
                                 (when-let [s (get event "seq")]
                                   (swap! cursor-atom max (long s)))
                                 (doseq [[_ sink] sinks]
                                   (try (sink event) (catch Throwable _ nil))))))
                           (recur []))
                         (if (str/starts-with? line "data: ")
                           (recur (conj data-lines (subs line 6)))
                           (recur data-lines))))
                   [:closed])))
             (finally (reset! alive?* false)
                      (some-> ^Thread watchdog
                              .interrupt)))))))

(defn- mux-run!
  "Background reconnect loop owning epoch `my-epoch`. Reconnects (resuming from
   each session's advanced cursor) whenever the daemon drops the stream, and
   stops for good once a newer epoch takes over, the session set empties, or the
   set was edited (a fresh run already owns the new set)."
  [my-epoch]
  (future
    (loop [attempt 0]
      (let
        [dropped? (try (not= [:epoch-changed] (open-mux-events! my-epoch))
                       (catch Throwable _ true))]
        (when (and dropped? (= my-epoch (:epoch @mux)) (seq (:subs @mux)))
          (mux-broadcast! "gateway.disconnected")
          (let
            [delay-ms (long (min 5000 (* (long sse-reconnect-backoff-ms) (inc (long attempt)))))
             interrupted? (try (Thread/sleep delay-ms) false (catch InterruptedException _ true))]

            (when-not interrupted? (recur (inc attempt)))))))))

(defn- restart-mux!
  "Bump the epoch, close the live stream (unblocking the parked reader), cancel
   the old run, and — if any session remains — start a fresh run for the new
   set. Called after every subscribe/unsubscribe."
  []
  (let [{:keys [epoch stream future]} (swap! mux update :epoch inc)]
    (when stream (try (.close ^java.io.Closeable stream) (catch Throwable _ nil)))
    (when future (future-cancel future))
    (if (seq (:subs @mux))
      (swap! mux assoc :future (mux-run! epoch) :stream nil)
      (swap! mux assoc :future nil :stream nil))))

(defn mux-subscribe!
  "Add `sid`'s `sink` to the ONE process-wide multiplexed event stream, starting
   at `cursor` (its `current-seq` for a live-only stream). The connection is
   (re)opened only when the session set changes; multiple local listeners for
   the SAME session share one cursor and one remote subscription. Returns a
   zero-arg cleanup fn. Every sink sees gateway.connected / gateway.disconnected
   on connection changes, exactly like the per-session [[subscribe!]]."
  [sid sink cursor]
  (let
    [sid
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
      (mux-unsubscribe! sid sub-id))))

(defn mux-unsubscribe!
  "Drop one local listener from the multiplexed stream and reconnect only when
   the last listener for that sid is gone (or tear the connection down when it
   was the last watched session)."
  ([sid] (mux-unsubscribe! sid nil))
  ([sid sub-id]
   (let
     [sid
      (str sid)

      changed-session-set?
      (volatile! false)]

     (swap! mux (fn [m]
                  (let
                    [path
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
  (let
    [type
     (get event "type")

     own?
     (= (str (get event "turn_id")) (str wanted-turn-id))]

    (cond (and own? (contains? #{"turn.completed" "turn.failed"} type)) [:terminal event]
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
      (let
        [outcome (try (read-sse-stream! sid @cursor* wanted-turn-id on-event cursor*)
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
  (let
    [submitted
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
