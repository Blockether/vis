(ns com.blockether.vis.internal.gateway.client
  "HTTP/SSE client for the long-lived gateway daemon.

   Interactive channels call this facade instead of `gateway.state` directly. It
   discover-or-starts the one daemon for the current DB, then speaks the same
   HTTP/SSE API every other client uses. This is the thin-client half of the
   gateway-daemon plan: token refresh, turn execution, and live streaming happen
   in ONE process."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.config :as config]
            [com.blockether.vis.internal.gateway.discovery :as discovery]
            [com.blockether.vis.internal.gateway.wire :as wire])
  (:import
    (java.io BufferedReader InputStream InputStreamReader)
    (java.net URI URLEncoder)
    (java.net.http HttpClient HttpRequest HttpRequest$BodyPublishers HttpResponse$BodyHandlers)
    (java.nio.charset StandardCharsets)
    (java.time Duration)))

(def ^:private DEFAULT_PORT 7890)
(def ^:private DEFAULT_HOST "127.0.0.1")

(defonce ^:private http-client
  (delay (-> (HttpClient/newBuilder)
             (.connectTimeout (Duration/ofSeconds 2))
             (.build))))

(defonce ^:private cached-entry (atom nil))
(defonce ^:private client-id (atom nil))
(defonce ^:private release-hook-installed? (atom false))
(defonce ^:private subscriptions (atom {}))

(defn- db-target [] (config/resolve-db-spec))

(defn- enc [x] (URLEncoder/encode (str x) StandardCharsets/UTF_8))

(defn- base-url
  [{:keys [host port]}]
  (str "http://" (or host DEFAULT_HOST) ":" (or port DEFAULT_PORT)))

(defn- request-builder
  [{:keys [secret] :as entry} path]
  (doto (HttpRequest/newBuilder (URI/create (str (base-url entry) path)))
    (.timeout (Duration/ofSeconds 30))
    (.header "Accept" "application/json")
    (.header "X-Vis-Gateway-Secret" (str secret))))

(defn- parse-json-body [^String body] (or (wire/parse-json body) {}))

(declare ensure-gateway! ensure-client! ensure-gateway-serving!)

(defn- send-json-with-entry!
  ([entry method path] (send-json-with-entry! entry method path nil))
  ([entry method path body]
   (let [builder
         (request-builder entry path)

         _
         (if (some? body)
           (doto builder
             (.header "Content-Type" "application/json")
             (.method method (HttpRequest$BodyPublishers/ofString (wire/json-str body))))
           (.method builder method (HttpRequest$BodyPublishers/noBody)))

         response
         (.send @http-client (.build builder) (HttpResponse$BodyHandlers/ofString))

         status
         (.statusCode response)

         parsed
         (parse-json-body (.body response))]

     (when (>= status 400)
       (throw (ex-info (or (:message parsed) (str "gateway HTTP " status))
                       (assoc parsed :http-status status))))
     parsed)))

(defn- send-json!
  ([method path] (send-json! method path nil))
  ([method path body]
   (let [entry (ensure-gateway!)]
     (ensure-client! entry)
     (send-json-with-entry! entry method path body))))

(defn- probe-entry?
  [entry]
  (try (let [builder
             (request-builder entry "/healthz")

             response
             (.send @http-client (.build (.GET builder)) (HttpResponse$BodyHandlers/ofString))

             body
             (parse-json-body (.body response))]

         (and (= 200 (.statusCode response)) (= "ok" (:status body)) (true? (:secret_match body))))
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
             (finish (str "✓ gateway ready" (when (= mode :awaited) " (started by another vis)")))

             :timeout
             (finish "✗ gateway did not become ready in time")

             nil)
           (catch Throwable _ nil)))))

(defn ensure-gateway!
  "Return a fresh daemon registry entry for the current DB, auto-starting the
   detached gateway if needed. `:memory` is a programmer error for this client;
   headless one-shots stay in-process and should not call here."
  []
  (let [db (db-target)]
    (when (discovery/memory-db? db)
      (throw (ex-info "gateway daemon is disabled for :memory DB" {:type :gateway/no-daemon})))
    (if (discovery/registry-fresh? @cached-entry probe-entry?)
      @cached-entry
      ;; Native-image startup can exceed 8s while SQLite/Flyway initializes;
      ;; JVM source boot (dev) needs ~30s to load Clojure + extensions before
      ;; it self-registers, so give it a much longer runway.
      (let [{:keys [entry] :as result} (discovery/discover-or-start!
                                         {:db db :port DEFAULT_PORT :host DEFAULT_HOST}
                                         :probe probe-entry?
                                         :on-event (progress-reporter)
                                         :timeout-ms (if (discovery/native-image?) 15000 60000))]
        (if entry
          (do (reset! cached-entry entry) entry)
          (throw (ex-info "gateway daemon did not become ready"
                          (assoc result :type :gateway/start-timeout))))))))

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
   pid is killed."
  [entry]
  (when-not @client-id
    (locking client-id
      (when-not @client-id
        (let [{:keys [client_id]} (send-json-with-entry! entry
                                                         "POST"
                                                         "/v1/clients"
                                                         {:pid (discovery/current-pid)
                                                          :kind "clojure-client"})]
          (reset! client-id client_id)
          (ensure-release-hook!)))))
  @client-id)

(defn create-session! [opts] (send-json! "POST" "/v1/sessions" opts))

(defn soul
  [sid]
  (try (send-json! "GET" (str "/v1/sessions/" (enc sid)))
       (catch clojure.lang.ExceptionInfo e
         (when-not (= 404 (:http-status (ex-data e))) (throw e)))))

(defn list-sessions ([] (:sessions (send-json! "GET" "/v1/sessions"))) ([_channel] (list-sessions)))

(defn close-session! [sid] (send-json! "DELETE" (str "/v1/sessions/" (enc sid))))

;; --- Projects (cross-channel) + movable project sessions + ownership (V6/V7) ---

(defn list-projects
  "GET /v1/projects. `opts`: :channel (keyword/string, :all ⇒ every project),
   :owner (string), :archived? (bool). Returns the :projects vector."
  ([] (list-projects nil))
  ([{:keys [channel owner archived?]}]
   (let [qs
         (->> [(when (and channel (not= :all channel) (not= "all" channel))
                 (str "channel=" (enc (name channel)))) (when owner (str "owner=" (enc owner)))
               (when archived? "archived=true")]
              (remove nil?)
              (str/join "&"))

         path
         (cond-> "/v1/projects"
           (seq qs)
           (str "?" qs))]

     (:projects (send-json! "GET" path)))))

(defn create-project! [opts] (send-json! "POST" "/v1/projects" opts))

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

(defn release-session!
  "Release a session VIEW when the owning channel exits: tell the daemon to
   stop the session's background resources (shell_bg children, REPLs) and drop
   its live runtime, then release the process-level client lease. This is NOT
   a per-session delete (the transcript stays resumable) and never sends daemon
   shutdown; the daemon stops itself only when refcount AND running-turn-count
   hit zero. Best-effort and never daemon-spawning — if no fresh daemon is
   registered there is nothing to release against."
  [sid]
  (when sid
    (try (let [entry (or @cached-entry (discovery/read-registry (db-target)))]
           (when (discovery/registry-fresh? entry probe-entry?)
             (send-json-with-entry! entry "POST" (str "/v1/sessions/" (enc sid) "/release"))))
         (catch Throwable _ nil)))
  (release-client!))

(defn get-turn
  [sid tid]
  (try (send-json! "GET" (str "/v1/sessions/" (enc sid) "/turns/" (enc tid)))
       (catch clojure.lang.ExceptionInfo e
         (when-not (= 404 (:http-status (ex-data e))) (throw e)))))

(defn list-turns [sid] (:turns (send-json! "GET" (str "/v1/sessions/" (enc sid) "/turns"))))

(defn transcript [sid] (:turns (send-json! "GET" (str "/v1/sessions/" (enc sid) "/transcript"))))

(defn turn-trace
  "Canonical wire iterations of ONE persisted turn (nil when the id is
   unknown to the daemon)."
  [sid tid]
  (:iterations (send-json! "GET" (str "/v1/sessions/" (enc sid) "/turns/" (enc tid) "/trace"))))

(defn context-snapshot [sid] (send-json! "GET" (str "/v1/sessions/" (enc sid) "/context")))

(defn session-model [sid] (:model (send-json! "GET" (str "/v1/sessions/" (enc sid) "/model"))))

(defn session-model-cached [sid] (session-model sid))

(defn set-session-model!
  [sid provider model]
  (:model (send-json! "PATCH"
                      (str "/v1/sessions/" (enc sid) "/model")
                      {:provider provider :model model})))

(defn- decode-workspace
  "Re-hydrate a gateway workspace response into the engine's HYPHENATED
   workspace-record shape the channels read (`:repo-root`, `:fork-ms`,
   `:filesystem-roots` with `:trunk`/`:clone`/`:fork-ms`/`:backend`).

   The wire munges keyword keys `-`->`_` on the way out (`wire/->wire`) while
   `parse-json` keywordizes VERBATIM, so every hyphenated key comes back snake
   (`:filesystem-roots` -> `:filesystem_roots`) and a channel reading the kebab
   key sees nil — the TUI picker/footer never showed the added root. Invert the
   munge GENERICALLY with `wire/kebab-keys` (recursively, ALL keys) so a
   newly-added hyphenated workspace key can never silently reintroduce this bug,
   then keyword-coerce the `:backend` VALUE (a keyword the wire stringified).
   nil-safe; idempotent on already-kebab in-process input."
  [w]
  (when w
    (let [w (wire/kebab-keys w)]
      (cond-> w
        (contains? w :filesystem-roots)
        (update :filesystem-roots
                (fn [rs]
                  (when (seq rs) (mapv #(cond-> % (:backend %) (update :backend keyword)) rs))))))))

(defn session-workspace-info
  [sid]
  (decode-workspace (:workspace (send-json! "GET" (str "/v1/sessions/" (enc sid) "/workspace")))))

(defn add-filesystem-root!
  "Add `path` as an extra filesystem root for `sid` IN THE DAEMON, returning the
   refreshed `session-workspace-info`. The daemon owns the session's DB, so the
   new root is what every channel reads back (fixing the local-only mutation that
   never reached the running session)."
  [sid path]
  (decode-workspace (:workspace (send-json! "POST"
                                            (str "/v1/sessions/" (enc sid) "/workspace/roots")
                                            {:path path}))))

(defn remove-filesystem-root!
  "Remove `path` from `sid`'s extra filesystem roots IN THE DAEMON, returning the
   refreshed `session-workspace-info`."
  [sid path]
  (decode-workspace (:workspace (send-json! "DELETE"
                                            (str "/v1/sessions/" (enc sid) "/workspace/roots")
                                            {:path path}))))

(defn change-root!
  "Repoint `sid`'s PRIMARY filesystem root to `path` IN THE DAEMON, returning the
   refreshed `session-workspace-info` (whose `:id` is the newly pinned workspace)."
  [sid path]
  (decode-workspace (:workspace (send-json! "PATCH"
                                            (str "/v1/sessions/" (enc sid) "/workspace/root")
                                            {:path path}))))

(defn submit-turn!
  [sid opts]
  (let [res (send-json! "POST" (str "/v1/sessions/" (enc sid) "/turns") opts)]
    (if (:turn_id res) {:turn res} res)))

(defn update-queued-turn!
  [sid tid request]
  (send-json! "PATCH" (str "/v1/sessions/" (enc sid) "/turns/" (enc tid)) {:request request}))

(defn delete-queued-turn!
  [sid tid]
  (send-json! "DELETE" (str "/v1/sessions/" (enc sid) "/turns/" (enc tid))))

(defn cancel-turn!
  [sid tid]
  (send-json! "POST" (str "/v1/sessions/" (enc sid) "/turns/" (enc tid) "/cancel")))

(defn reconcile-running-turns!
  "Clients do not sweep. Only the daemon may reconcile its own startup orphans."
  []
  nil)

(defn status
  []
  (let [db
        (db-target)

        entry
        (discovery/read-registry db)]

    (if (discovery/registry-fresh? entry probe-entry?)
      (send-json-with-entry! entry "GET" "/v1/admin/status")
      {:status "stopped" :db (when-not (discovery/memory-db? db) (str (discovery/db-target db)))})))

(defn stop-daemon!
  []
  (let [db
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
  "Classify whether the daemon at `entry` serves `path`:
     :served      — any status other than 404 (route mounted; an auth 401/303
                    still means it exists).
     :absent      — a real 404: the daemon's classpath lacks the extension that
                    owns `path` (e.g. a gateway auto-started without the web
                    channel never mounts /ui).
     :unreachable — the probe request itself failed (connection reset, timeout).
                    NOT a 404, so NEVER treated as license to force-kill; the
                    caller retreats to leaving the daemon alone."
  [entry path]
  (try (let [builder
             (request-builder entry path)

             response
             (.send @http-client (.build (.GET builder)) (HttpResponse$BodyHandlers/ofString))]

         (if (= 404 (.statusCode response)) :absent :served))
       (catch Throwable _ :unreachable)))

(defn ensure-gateway-serving!
  "Like [[ensure-gateway!]], but tries to GUARANTEE the returned daemon actually
   serves `path`. When [[ensure-gateway!]] attaches to an already-running daemon
   that 404s on `path` (started from a classpath missing the extension that owns
   it), respawn a fresh daemon from THIS process — whose classpath, by
   construction, carries the route. This is what lets `vis channels web`
   self-heal instead of parking on a `/ui` that 404s.

   The respawn is NON-DESTRUCTIVE. A blind POST /v1/admin/stop is refcount-blind:
   it would abort every in-flight turn and kill every session's background
   resources. So we force-restart the stale daemon ONLY when it is idle — no OTHER
   clients and no running turn. Otherwise we leave it untouched and surface a clear
   error. A transport blip on the probe (not a real 404) never triggers a restart.
   Returns the entry."
  [path]
  (let [entry (ensure-gateway!)]
    (case (probe-route entry path)
      ;; Mounted — or a transient transport blip we must not misread as \"missing\".
      (:served :unreachable)
      entry

      :absent
      (let [st (status)
            clients (or (:clients st) 0)
            running (or (:running_turns st) 0)]

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
        (let [entry (ensure-gateway!)]
          (when-not (= :served (probe-route entry path))
            (throw (ex-info
                     (str "gateway daemon is not serving " path " even after a fresh restart")
                     {:type :gateway/route-missing :path path})))
          entry)))))

(defn provider-status
  [provider-id]
  (let [path
        (str "/v1/providers/" (enc (name provider-id)) "/status")

        entry
        (ensure-gateway-serving! path)]

    (ensure-client! entry)
    (:status (send-json-with-entry! entry "GET" path))))

(defn provider-limits
  [provider-id]
  (let [path
        (str "/v1/providers/" (enc (name provider-id)) "/limits")

        entry
        (ensure-gateway-serving! path)]

    (ensure-client! entry)
    (:report (send-json-with-entry! entry "GET" path))))

(defn current-seq [sid] (:seq (send-json! "GET" (str "/v1/sessions/" (enc sid) "/seq"))))

(defn events-since
  [sid cursor]
  (:events (send-json! "GET"
                       (str "/v1/sessions/" (enc sid)
                            "/events-since?cursor=" (long (or cursor 0))))))

(defn- ir-from-wire
  "Undo gateway.wire's keyword->string JSON coercion for canonical IR vectors."
  [x]
  (cond (vector? x) (mapv (fn [i v]
                            (if (and (zero? i) (string? v)) (keyword v) (ir-from-wire v)))
                          (range)
                          x)
        (map? x) (into {}
                       (map (fn [[k v]]
                              [(if (string? k) (keyword (str/replace k "_" "-")) k)
                               (ir-from-wire v)]))
                       x)
        (sequential? x) (mapv ir-from-wire x)
        :else x))

(defn- terminal-event->result
  [event fallback-turn-id]
  (let [failed?
        (or (= "turn.failed" (:type event)) (= "failed" (:status event)))

        cancelled?
        (= "cancelled" (:status event))

        needs-input?
        (or (true? (:needs_input event)) (= "suspended" (:status event)))

        answer
        (or (:answer event) (:answer_md event))]

    (cond-> {:answer answer
             :answer-ir (some-> (:answer_ir event)
                                ir-from-wire)
             :iteration-count (or (:iteration_count event) 1)
             :duration-ms (:duration_ms event)
             :session-turn-id (or (:engine_turn_id event) fallback-turn-id)
             :utilization (:utilization event)}
      (:model event)
      (assoc :model (:model event))

      (:provider event)
      (assoc :provider (:provider event))

      (:llm_selected event)
      (assoc :llm-selected (:llm_selected event))

      (:llm_actual event)
      (assoc :llm-actual (:llm_actual event))

      (some? (:llm_fallback event))
      (assoc :llm-fallback? (:llm_fallback event))

      (seq (:llm_routing_trace event))
      (assoc :llm-routing-trace (:llm_routing_trace event))

      (:tokens event)
      (assoc :tokens (wire/kebab-keys (:tokens event)))

      (:cost event)
      (assoc :cost (wire/kebab-keys (:cost event)))

      (:confidence event)
      (assoc :confidence (:confidence event))

      needs-input?
      (assoc :status :needs-input)

      cancelled?
      (assoc :status :cancelled)

      failed?
      (assoc :error (or (:error event) (:answer_md event) "turn failed")))))

(defn- sse-request
  [sid cursor]
  (let [entry (ensure-gateway!)]
    (-> (request-builder entry
                         (str "/v1/sessions/" (enc sid) "/events?cursor=" (long (or cursor 0))))
        (.header "Accept" "text/event-stream")
        (.GET)
        (.build))))


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
   close it. Returns `[:closed]` on EOF; throws `ex-info` with `:http-status` on
   a non-200 response. Shared by `read-sse-stream!` (blocking turns) and
   `subscribe!` (the live mirror) so the frame parsing lives in ONE place."
  [sid cursor cursor* stream* handle]
  (let [response
        (.send @http-client (sse-request sid cursor) (HttpResponse$BodyHandlers/ofInputStream))]
    (when-not (= 200 (.statusCode response))
      (throw (ex-info (str "gateway SSE HTTP " (.statusCode response))
                      {:http-status (.statusCode response)})))
    (with-open [^InputStream in (.body response)
                rdr (BufferedReader. (InputStreamReader. in StandardCharsets/UTF_8))]

      (when stream* (reset! stream* in))
      (loop [data-lines []]
        (if-let [line (.readLine rdr)]
          (if (str/blank? line)
            (let [data (str/join "\n" data-lines)
                  event (when (seq data) (wire/parse-json data))]

              (if-not event
                (recur [])
                (do (when-let [s (:seq event)]
                      (swap! cursor* max (long s)))
                    (or (handle event) (recur [])))))
            (if (str/starts-with? line "data: ")
              (recur (conj data-lines (subs line 6)))
              (recur data-lines)))
          [:closed])))))
(defn subscribe!
  "Remote equivalent of gateway.state/subscribe!: start a background SSE reader
   that replays `cursor` then calls `sink` for every live event. Returns an empty
   replay vector because the gateway's SSE endpoint itself handles replay before
   live delivery."
  [sid sub-id sink cursor]
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
            ;; reconnect.
            (let [dropped? (try (open-sse-events! sid
                                                  @cursor*
                                                  cursor*
                                                  stream*
                                                  (fn [event]
                                                    (sink event)
                                                    nil))
                                true
                                (catch Throwable _ true))]
              (when (and dropped? (contains? @subscriptions sub-id))
                (let [interrupted? (try (Thread/sleep
                                          (min 5000 (* sse-reconnect-backoff-ms (inc attempt))))
                                        false
                                        (catch InterruptedException _ true))]
                  (when-not interrupted? (recur (inc attempt)))))))
          (swap! subscriptions dissoc sub-id))]

    (swap! subscriptions assoc sub-id {:future fut :stream stream*})
    []))

(defn unsubscribe!
  [_sid sub-id]
  (when-let [{:keys [future stream]} (get @subscriptions sub-id)]
    (try (some-> @stream
                 .close)
         (catch Throwable _ nil))
    (future-cancel future)
    (swap! subscriptions dissoc sub-id))
  nil)

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
        (:type event)

        own?
        (= (str (:turn_id event)) (str wanted-turn-id))]

    (cond (and own? (contains? #{"turn.completed" "turn.failed"} type)) [:terminal event]
          (and own? (= "turn.queued.deleted" type)) [:terminal
                                                     (assoc event
                                                       :type "turn.completed"
                                                       :status "cancelled")]
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
          (if (< attempt sse-reconnect-max-attempts)
            (do (Thread/sleep (* sse-reconnect-backoff-ms (inc attempt))) (recur (inc attempt)))
            (throw (ex-info "Lost connection to the gateway daemon before the turn finished."
                            {:gateway-disconnected true :turn-id (str wanted-turn-id)}))))))))

(defn submit-turn-sync!
  [sid {:keys [on-event] :as opts}]
  (let [submitted
        (submit-turn! sid (dissoc opts :on-event))

        turn
        (:turn submitted)

        turn-id
        (:turn_id turn)]

    (when-let [e (:error submitted)]
      (throw (ex-info (or (:message submitted) (str e)) submitted)))
    (terminal-event->result (read-events-until! sid 0 turn-id on-event) turn-id)))

(defn attach-turn-sync!
  [sid tid {:keys [on-event]}]
  (terminal-event->result (read-events-until! sid 0 tid on-event) tid))