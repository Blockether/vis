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
   client renders (§4.1). Any host process (the `vis serve` daemon, a
   TUI run, an embedded caller) can start it alongside whatever else it
   is doing via `start!`."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.config :as config]
            [com.blockether.vis.internal.docs :as docs]
            [com.blockether.vis.internal.extension :as extension]
            [com.blockether.vis.internal.file-picker :as file-picker]
            [com.blockether.vis.internal.gateway.state :as state]
            [com.blockether.vis.internal.gateway.wire :as wire]
            [com.blockether.vis.internal.registry :as registry]
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

(defn- default-token-path
  ^Path []
  (Path/of (System/getProperty "user.home") (into-array String [".vis" "gateway.token"])))

(defn- ensure-token!
  "Read the bearer token at `path`, minting one (mode 600) on first run."
  ^String [^Path path]
  (if (Files/exists path (make-array LinkOption 0))
    (str/trim (String. (Files/readAllBytes path) StandardCharsets/UTF_8))
    (let [token (str (java.util.UUID/randomUUID))]
      (some-> (.getParent path)
              (Files/createDirectories (make-array FileAttribute 0)))
      (Files/write path (.getBytes token StandardCharsets/UTF_8) (make-array OpenOption 0))
      (try (Files/setPosixFilePermissions path (PosixFilePermissions/fromString "rw-------"))
           (catch Throwable _ nil))
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
                  (when (> (long (:seq event)) (long @last-seq))
                    (.write out (.getBytes (wire/sse-frame event) StandardCharsets/UTF_8))
                    (.flush out)
                    (reset! last-seq (long (:seq event))))))]

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

(defn- health-handler [_] (json-response {:status "ok"}))

(defn- models-handler
  [_]
  (json-response {:providers (mapv (fn [{:provider/keys [id doc]}]
                                     {:id (name id) :doc doc})
                                   (registry/registered-providers))}))

(defn- create-session-handler
  [request]
  (let [body (body-json request)]
    (json-response 201
                   (state/create-session! {:title (:title body)
                                           :external-id (:external_id body)
                                           :workspace-id (:workspace_id body)}))))

(defn- list-sessions-handler [_] (json-response {:sessions (state/list-sessions)}))

(defn- soul-handler
  [request]
  (if-let [soul (some-> (path-sid request)
                        state/soul)]
    (json-response soul)
    (session-404 (get-in request [:path-params :sid]))))

(defn- patch-session-handler
  [request]
  (let [sid
        (path-sid request)

        title
        (:title (body-json request))]

    (cond (str/blank? (str title))
          (error-response 400 :invalid-request "title must be a non-blank string")
          :else (if-let [soul (and sid (state/set-title! sid title))]
                  (json-response soul)
                  (session-404 (get-in request [:path-params :sid]))))))

(defn- delete-session-handler
  [request]
  (some-> (path-sid request)
          state/close-session!)
  {:status 204 :headers {} :body nil})

(defn- submit-turn-handler
  [request]
  (let [sid
        (path-sid request)

        body
        (body-json request)]

    (if (nil? sid)
      (session-404 (get-in request [:path-params :sid]))
      (let [result (state/submit-turn! sid
                                       {:request (:request body)
                                        :idempotency-key (:idempotency_key body)
                                        :model (:model body)
                                        :reasoning-default (some-> (:reasoning_default body)
                                                                   keyword)
                                        :attachments (:attachments body)})]
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

(defn- context-handler
  [request]
  (if-let [snapshot (some-> (path-sid request)
                            state/context-snapshot)]
    (json-response snapshot)
    (session-404 (get-in request [:path-params :sid]))))

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
              authed? (or (= expected
                             (some-> (get-in request [:headers "authorization"])
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
    (into [["/healthz" {:get health-handler}] ["/readyz" {:get health-handler}]
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
           ["/v1" ["/models" {:get models-handler}]
            ["/sessions" {:get list-sessions-handler :post create-session-handler}]
            ["/sessions/:sid"
             {:get soul-handler :patch patch-session-handler :delete delete-session-handler}]
            ["/sessions/:sid/events" {:get events-handler}]
            ["/sessions/:sid/context" {:get context-handler}]
            ["/sessions/:sid/suggest" {:get suggest-handler}]
            ["/sessions/:sid/turns" {:get list-turns-handler :post submit-turn-handler}]
            ["/sessions/:sid/turns/:tid" {:get get-turn-handler}]
            ["/sessions/:sid/turns/:tid/cancel" {:post cancel-turn-handler}]]]
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

(defn start!
  "Start the gateway on the Ring Jetty adapter with virtual threads.
   Returns `{:port :host :token-file}`. Throws when already running.
   Safe to call from any host process - the daemon (`vis serve`), a TUI
   run, or an embedded caller."
  ([] (start! {}))
  ([{:keys [port host token-file require-token?]}]
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

         ;; :token must be visible to rebuild-app! before Jetty serves the
         ;; first request; a failed boot must roll the state back so a
         ;; retry isn't refused as "already running".
         _
         (reset! server-state {:token token :require-token? require-token?})

         _
         (rebuild-app!)

         ;; Load the persistence backend NOW, single-threaded, so the
         ;; first DB touch never happens on N concurrent request threads.
         _
         (state/warm-db!)

         ;; Re-stamp turns left :running by a PREVIOUS process (a daemon
         ;; restart / crash mid-turn) as :interrupted. Without this the web
         ;; renders a permanent Stop button + thinking dots for a turn that
         ;; is no longer executing. The TUI already sweeps at its startup;
         ;; the gateway must too (it didn't, hence the stuck "running" turns).
         _
         (try ((requiring-resolve 'com.blockether.vis.core/db-sweep-orphaned-running-turns!))
              (catch Throwable t
                (tel/log! :warn ["gateway: orphan-running-turn sweep failed" (ex-message t)])))

         ;; Hydrate persisted toggles + install the config.edn save
         ;; listener so web/gateway-driven flips survive restarts.
         _
         (install-toggle-persistence!)

         server
         (try (jetty/run-jetty serving-handler
                               {:port port
                                :host host
                                :join? false
                                :virtual-threads? true
                                :send-server-version? false})
              (catch Throwable t (reset! server-state nil) (reset! live-app nil) (throw t)))]

     (when-not (= host DEFAULT_HOST)
       (tel/log! :warn ["gateway: binding to non-loopback host" host]))
     (reset! server-state {:server server
                           :port port
                           :host host
                           :token token
                           :token-path (str path)
                           :require-token? require-token?})
     (tel/log! :info
               ["gateway: listening" (str host ":" port)
                (if require-token? "auth: bearer token" "auth: disabled (loopback)")])
     {:port port :host host :token-file (str path) :require-token? require-token?})))

(defn stop!
  "Stop the gateway server if running. Idempotent."
  []
  (when-let [{:keys [^Server server]} @server-state]
    (.stop server)
    (reset! server-state nil)
    (reset! live-app nil))
  nil)

(defn running? [] (some? @server-state))

(defn serve-main!
  "Blocking entry for the `vis serve` command: start, print the
   connection line, park forever (Ctrl-C / SIGTERM stops the JVM)."
  [{:keys [port host token-file require-token?]}]
  (let [{:keys [port host token-file require-token?]} (start! {:port (some-> port
                                                                             parse-long)
                                                               :host host
                                                               :token-file token-file
                                                               :require-token? require-token?})]
    (println (str "vis gateway listening on http://" host ":" port))
    (if require-token?
      (println (str "bearer token: " token-file))
      (println "auth: disabled (loopback default; pass --require-token to enable)"))
    (.addShutdownHook (Runtime/getRuntime) (Thread. ^Runnable stop!))
    @(promise)))
