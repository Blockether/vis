(ns com.blockether.vis.internal.gateway.server
  "Gateway HTTP/SSE server (docs/GATEWAY.md §5-§6).

   Clojure-native stack: reitit-ring routes -> Ring middleware -> the
   Ring Jetty adapter on JDK virtual threads (`:virtual-threads? true`).
   SSE is a Ring `StreamableResponseBody` that parks its virtual thread
   on the connection: replay rides first, live events fan in under the
   same output-stream monitor, a heartbeat comment keeps the pipe warm
   and detects dead clients.

   This is internal plumbing, not a channel: it registers no channel
   descriptor and owns no renderer - it ships canonical IR and the
   client renders (§4.1). Any host process (the `vis serve` daemon, a
   TUI run, an embedded caller) can start it alongside whatever else it
   is doing via `start!`."
  (:require
   [clojure.string :as str]
   [com.blockether.vis.internal.gateway.state :as state]
   [com.blockether.vis.internal.gateway.wire :as wire]
   [com.blockether.vis.internal.registry :as registry]
   [reitit.ring :as rr]
   [ring.adapter.jetty :as jetty]
   [ring.core.protocols :as ring-protocols]
   [ring.middleware.cookies :as ring-cookies]
   [ring.middleware.params :as ring-params]
   [taoensso.telemere :as tel])
  (:import
   [java.io OutputStream]
   [java.nio.charset StandardCharsets]
   [java.nio.file Files LinkOption OpenOption Path]
   [java.nio.file.attribute FileAttribute PosixFilePermissions]
   [org.eclipse.jetty.server Server]))

(def ^:private DEFAULT_PORT 7890)
(def ^:private DEFAULT_HOST "127.0.0.1")
(def ^:private HEARTBEAT_MS 15000)

(defonce ^:private server-state (atom nil))

;; =============================================================================
;; Bearer token (§3)
;; =============================================================================

(defn- default-token-path ^Path []
  (Path/of (System/getProperty "user.home") (into-array String [".vis" "gateway.token"])))

(defn- ensure-token!
  "Read the bearer token at `path`, minting one (mode 600) on first run."
  ^String [^Path path]
  (if (Files/exists path (make-array LinkOption 0))
    (str/trim (String. (Files/readAllBytes path) StandardCharsets/UTF_8))
    (let [token (str (java.util.UUID/randomUUID))]
      (some-> (.getParent path) (Files/createDirectories (make-array FileAttribute 0)))
      (Files/write path (.getBytes token StandardCharsets/UTF_8) (make-array OpenOption 0))
      (try
        (Files/setPosixFilePermissions path (PosixFilePermissions/fromString "rw-------"))
        (catch Throwable _ nil))
      token)))

;; =============================================================================
;; Ring helpers
;; =============================================================================

(defn- json-response
  ([body] (json-response 200 body))
  ([status body]
   {:status status
    :headers {"Content-Type" "application/json"}
    :body (wire/json-str body)}))

(defn- error-response [status type message & {:as extra}]
  (json-response status {:error (merge {:type (name type) :message message} extra)}))

(defn- session-404 [sid-str]
  (error-response 404 :session-not-found "unknown session" :session_id (str sid-str)))

(defn- body-json [request]
  (some-> (:body request) slurp wire/parse-json))

(defn- path-sid [request]
  (some-> (get-in request [:path-params :sid]) parse-uuid))

(defn- path-tid [request]
  (get-in request [:path-params :tid]))

;; =============================================================================
;; SSE (§6.3)
;; =============================================================================

(defn- sse-cursor [request]
  (or (some-> (get-in request [:headers "last-event-id"]) parse-long)
    (some-> (get-in request [:query-params "cursor"]) parse-long)
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
  [sid cursor]
  (reify ring-protocols/StreamableResponseBody
    (write-body-to-stream [_ _ output-stream]
      (let [^OutputStream out output-stream
            sub-id   (str (java.util.UUID/randomUUID))
            last-seq (atom (long cursor))
            write!   (fn [event]
                       (locking out
                         (when (> (long (:seq event)) (long @last-seq))
                           (.write out (.getBytes (wire/sse-frame event) StandardCharsets/UTF_8))
                           (.flush out)
                           (reset! last-seq (long (:seq event))))))]
        (try
          (locking out
            (doseq [event (state/subscribe! sid sub-id write! @last-seq)]
              (write! event)))
          (loop []
            (Thread/sleep (long HEARTBEAT_MS))
            (locking out
              (.write out (.getBytes ": ping\n\n" StandardCharsets/UTF_8))
              (.flush out))
            (recur))
          (catch Throwable _ nil)
          (finally
            (state/unsubscribe! sid sub-id)
            (try (.close out) (catch Throwable _ nil))))))))

(defn- events-handler [request]
  (let [sid (path-sid request)]
    (if (and sid (state/soul sid))
      {:status 200
       :headers {"Content-Type" "text/event-stream"
                 "Cache-Control" "no-cache"}
       :body (sse-body sid (sse-cursor request))}
      (session-404 (get-in request [:path-params :sid])))))

;; =============================================================================
;; /metrics (§6.5)
;; =============================================================================

(defn- prometheus-text [{:keys [turns-total turns-failed tokens-input tokens-output
                                cost-total duration-ms-total sessions-tracked
                                turns-running]}]
  (str
    "# TYPE vis_turns_total counter\nvis_turns_total " turns-total "\n"
    "# TYPE vis_turns_failed_total counter\nvis_turns_failed_total " turns-failed "\n"
    "# TYPE vis_turn_tokens_total counter\n"
    "vis_turn_tokens_total{kind=\"input\"} " tokens-input "\n"
    "vis_turn_tokens_total{kind=\"output\"} " tokens-output "\n"
    "# TYPE vis_turn_cost_usd_total counter\nvis_turn_cost_usd_total " cost-total "\n"
    "# TYPE vis_turn_duration_ms_total counter\nvis_turn_duration_ms_total " duration-ms-total "\n"
    "# TYPE vis_sessions_tracked gauge\nvis_sessions_tracked " sessions-tracked "\n"
    "# TYPE vis_turns_running gauge\nvis_turns_running " turns-running "\n"))

(defn- metrics-handler [request]
  (let [snapshot (state/metrics-snapshot)]
    (if (str/includes? (str (get-in request [:headers "accept"])) "application/json")
      (json-response snapshot)
      {:status 200
       :headers {"Content-Type" "text/plain; version=0.0.4"}
       :body (prometheus-text snapshot)})))

;; =============================================================================
;; Route handlers (§5-§6)
;; =============================================================================

(defn- health-handler [_] (json-response {:status "ok"}))

(defn- models-handler [_]
  (json-response
    {:providers (mapv (fn [{:provider/keys [id doc]}]
                        {:id (name id) :doc doc})
                  (registry/registered-providers))}))

(defn- create-session-handler [request]
  (let [body (body-json request)]
    (json-response 201
      (state/create-session! {:title (:title body)
                              :external-id (:external_id body)
                              :workspace-id (:workspace_id body)}))))

(defn- list-sessions-handler [_]
  (json-response {:sessions (state/list-sessions)}))

(defn- soul-handler [request]
  (if-let [soul (some-> (path-sid request) state/soul)]
    (json-response soul)
    (session-404 (get-in request [:path-params :sid]))))

(defn- patch-session-handler [request]
  (let [sid (path-sid request)
        title (:title (body-json request))]
    (cond
      (str/blank? (str title))
      (error-response 400 :invalid-request "title must be a non-blank string")

      :else
      (if-let [soul (and sid (state/set-title! sid title))]
        (json-response soul)
        (session-404 (get-in request [:path-params :sid]))))))

(defn- delete-session-handler [request]
  (some-> (path-sid request) state/close-session!)
  {:status 204 :headers {} :body nil})

(defn- submit-turn-handler [request]
  (let [sid (path-sid request)
        body (body-json request)]
    (if (nil? sid)
      (session-404 (get-in request [:path-params :sid]))
      (let [result (state/submit-turn! sid
                     {:request (:request body)
                      :idempotency-key (:idempotency_key body)
                      :model (:model body)
                      :reasoning-default (some-> (:reasoning_default body) keyword)})]
        (cond
          (:turn result)
          (json-response (if (:idempotent? result) 200 202) (:turn result))

          (= :turn-in-progress (:error result))
          (error-response 409 :turn-in-progress "session already has a running turn"
            :session_id (str sid) :turn_id (:turn-id result))

          (= :session-not-found (:error result))
          (session-404 (str sid))

          :else
          (error-response 400 :invalid-request (or (:message result) "invalid request")))))))

(defn- list-turns-handler [request]
  (let [sid (path-sid request)]
    (if (and sid (state/soul sid))
      (json-response {:turns (state/list-turns sid)})
      (session-404 (get-in request [:path-params :sid])))))

(defn- get-turn-handler [request]
  (let [sid (path-sid request)
        tid (path-tid request)]
    (if-let [turn (and sid (state/get-turn sid tid))]
      (json-response turn)
      (error-response 404 :turn-not-found "unknown turn" :turn_id tid))))

(defn- cancel-turn-handler [request]
  (let [sid (path-sid request)
        tid (path-tid request)
        result (if sid
                 (state/cancel-turn! sid tid)
                 {:error :turn-not-found})]
    (cond
      (:status result) (json-response 202 result)

      (= :turn-not-found (:error result))
      (error-response 404 :turn-not-found "unknown turn" :turn_id tid)

      :else
      (error-response 409 :not-running "turn is not running"
        :turn_id tid :turn_status (:status result)))))

(defn- approve-turn-handler [request]
  (let [sid (path-sid request)
        tid (path-tid request)
        body (body-json request)
        result (if sid
                 (state/approve-turn! sid tid {:decision (:decision body)
                                               :note (:note body)})
                 {:error :turn-not-found})]
    (cond
      (:turn result) (json-response 202 (:turn result))

      (= :turn-not-found (:error result))
      (error-response 404 :turn-not-found "unknown turn" :turn_id tid)

      (= :not-suspended (:error result))
      (error-response 409 :not-suspended "turn is not awaiting a decision"
        :turn_id tid :turn_status (:status result))

      :else
      (error-response 400 :invalid-request (or (:message result) "invalid request")))))

(defn- mind-handler [request]
  (if-let [snapshot (some-> (path-sid request) state/mind-snapshot)]
    (json-response snapshot)
    (session-404 (get-in request [:path-params :sid]))))

;; =============================================================================
;; Router + middleware
;; =============================================================================

;; =============================================================================
;; Route contributions — classpath auto-mount (the extension idiom)
;; =============================================================================
;;
;; The gateway core serves ONLY the JSON API. Anything else (the /ui web
;; companion, a future surface) is contributed by an extension at namespace
;; load — and extension namespaces load via the META-INF/vis-extension
;; manifest classpath scan, so dropping a jar on the classpath mounts its
;; routes with zero wiring (the same auto-discovery move as Java's
;; ServiceLoader or Spring auto-configuration, in vis's own idiom).
;;
;; Contribution shape (all keys but :routes optional):
;;   {:prefix            "/ui"        ; uri namespace this contribution owns
;;    :routes            (fn [token] reitit-route-data)
;;    :open-uris         #{"/ui" ...} ; reachable without auth
;;    :request-authed-fn (fn [request token] bool)   ; extra auth carrier
;;    :on-unauthorized   (fn [request] ring-response) ; custom 401 for :prefix
;;    :form-params?      true}        ; urlencoded form parsing under :prefix
(defonce ^:private route-contributions (atom {}))

(declare ^:private rebuild-app!)

(defn register-routes!
  "Register (or replace, by `id`) a route contribution. Safe to call at
   namespace load before the server exists; if the server is already
   running the live handler is rebuilt so the routes mount immediately."
  [id contribution]
  (swap! route-contributions assoc id contribution)
  (rebuild-app!)
  id)

(defn deregister-routes! [id]
  (swap! route-contributions dissoc id)
  (rebuild-app!)
  nil)

(defn- contributions [] (vals @route-contributions))

(defn- wrap-auth
  "Token gate (§3). The API sends `Authorization: Bearer`; contributions
   may add carriers of the SAME secret (e.g. the web channel's HttpOnly
   cookie) via :request-authed-fn, declare :open-uris, and shape their
   own unauthorized response for uris under their :prefix."
  [handler ^String token]
  (let [expected (str "Bearer " token)]
    (fn [request]
      (let [uri (str (:uri request))
            open? (or (= "/healthz" uri)
                    (some #(contains? (or (:open-uris %) #{}) uri) (contributions)))
            authed? (or (= expected (some-> (get-in request [:headers "authorization"]) str/trim))
                      (some (fn [{:keys [request-authed-fn]}]
                              (when request-authed-fn (request-authed-fn request token)))
                        (contributions)))]
        (if (or open? authed?)
          (handler request)
          (or (some (fn [{:keys [prefix on-unauthorized]}]
                      (when (and prefix on-unauthorized
                              (str/starts-with? uri prefix))
                        (on-unauthorized request)))
                (contributions))
            (error-response 401 :unauthorized "missing or invalid bearer token")))))))

(defn- wrap-errors [handler]
  (fn [request]
    (try
      (handler request)
      (catch Throwable t
        (tel/log! :error ["gateway: unhandled request error" (:uri request) (ex-message t)])
        (error-response 500 :engine-error (or (ex-message t) "internal error"))))))

(defn- router [^String token]
  (rr/router
    (into
      [["/healthz" {:get health-handler}]
       ["/readyz" {:get health-handler}]
       ["/metrics" {:get metrics-handler}]
       ["/v1"
        ["/models" {:get models-handler}]
        ["/sessions" {:get list-sessions-handler
                      :post create-session-handler}]
        ["/sessions/:sid" {:get soul-handler
                           :patch patch-session-handler
                           :delete delete-session-handler}]
        ["/sessions/:sid/events" {:get events-handler}]
        ["/sessions/:sid/mind" {:get mind-handler}]
        ["/sessions/:sid/turns" {:get list-turns-handler
                                 :post submit-turn-handler}]
        ["/sessions/:sid/turns/:tid" {:get get-turn-handler}]
        ["/sessions/:sid/turns/:tid/cancel" {:post cancel-turn-handler}]
        ["/sessions/:sid/turns/:tid/approve" {:post approve-turn-handler}]]]
      (keep (fn [{:keys [routes]}]
              (when routes
                (try (routes token)
                  (catch Throwable t
                    (tel/log! :error ["gateway: route contribution failed" (ex-message t)])
                    nil))))
        (contributions)))))

(defn- wrap-scoped-params
  "Param parsing with a hard boundary: uris under a contribution prefix
   that declared `:form-params?` get full `wrap-params` (query +
   urlencoded form bodies - what HTMX forms send); everything else gets
   query params ONLY, so the form parser can never consume a JSON API
   body (curl -d and many clients default to the urlencoded
   content-type while posting JSON)."
  [handler]
  (let [form-handler (ring-params/wrap-params handler)]
    (fn [request]
      (let [uri (str (:uri request))
            form? (some (fn [{:keys [prefix form-params?]}]
                          (and form-params? prefix (str/starts-with? uri prefix)))
                    (contributions))]
        (if form?
          (form-handler request)
          (handler (ring-params/assoc-query-params request "UTF-8")))))))

(defn- app [^String token]
  (-> (rr/ring-handler
        (router token)
        (rr/create-default-handler
          {:not-found (fn [_] (error-response 404 :not-found "no such route"))
           :method-not-allowed (fn [_] (error-response 405 :method-not-allowed "method not allowed"))}))
    (wrap-auth token)
    (wrap-scoped-params)
    (ring-cookies/wrap-cookies)
    (wrap-errors)))

(defonce ^:private live-app
  ;; The handler Jetty actually calls, swapped whole on every
  ;; register-routes!/deregister-routes! so contributions (and REPL
  ;; :reload) mount into a RUNNING server without a restart.
  (atom nil))

(defn- rebuild-app! []
  (when-let [{:keys [token]} @server-state]
    (reset! live-app (app token)))
  nil)

;; =============================================================================
;; Lifecycle
;; =============================================================================

(defn start!
  "Start the gateway on the Ring Jetty adapter with virtual threads.
   Returns `{:port :host :token-file}`. Throws when already running.
   Safe to call from any host process - the daemon (`vis serve`), a TUI
   run, or an embedded caller."
  ([] (start! {}))
  ([{:keys [port host token-file]}]
   (when @server-state
     (throw (ex-info "gateway already running" {:type :gateway/already-running})))
   (let [port  (int (or port DEFAULT_PORT))
         host  (or host DEFAULT_HOST)
         path  (if token-file
                 (Path/of token-file (make-array String 0))
                 (default-token-path))
         token (ensure-token! path)
         _ (reset! live-app (app token))
         server (jetty/run-jetty (fn [request] (@live-app request))
                  {:port port
                   :host host
                   :join? false
                   :virtual-threads? true
                   :send-server-version? false})]
     (when-not (= host DEFAULT_HOST)
       (tel/log! :warn ["gateway: binding to non-loopback host" host]))
     (reset! server-state {:server server :port port :host host
                           :token token :token-path (str path)})
     (tel/log! :info ["gateway: listening" (str host ":" port)])
     {:port port :host host :token-file (str path)})))

(defn stop!
  "Stop the gateway server if running. Idempotent."
  []
  (when-let [{:keys [^Server server]} @server-state]
    (.stop server)
    (reset! server-state nil)
    (reset! live-app nil))
  nil)

(defn running? []
  (some? @server-state))

(defn serve-main!
  "Blocking entry for the `vis serve` command: start, print the
   connection line, park forever (Ctrl-C / SIGTERM stops the JVM)."
  [{:keys [port host token-file]}]
  (let [{:keys [port host token-file]} (start! {:port (some-> port parse-long)
                                                :host host
                                                :token-file token-file})]
    (println (str "vis gateway listening on http://" host ":" port))
    (println (str "bearer token: " token-file))
    (.addShutdownHook (Runtime/getRuntime) (Thread. ^Runnable stop!))
    @(promise)))
