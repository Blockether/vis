(ns com.blockether.vis.tui.client
  "HTTP/JSON/SSE client used by the standalone terminal application."
  (:require [babashka.http-client :as http]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.contract.gateway :as gateway-contract]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.tui.cancellation :as cancellation]
            [com.blockether.vis.tui.config :as config]
            [com.blockether.vis.tui.error :as error]
            [com.blockether.vis.tui.form :as form]
            [com.blockether.vis.tui.format :as fmt]
            [com.blockether.vis.tui.notifications :as notifications]
            [com.blockether.vis.tui.paths :as paths]
            [com.blockether.vis.tui.presentation :as presentation]
            [com.blockether.vis.tui.progress :as progress]
            [com.blockether.vis.tui.toggles :as toggles]
            [com.blockether.vis.tui.tty :as tty]
            [com.blockether.vis.tui.util :as util])
  (:import (java.io BufferedReader File InputStream InputStreamReader)
           (java.net URI URLEncoder)
           (java.nio.charset StandardCharsets)))

(def ^:private default-host "127.0.0.1")

(def ^:private default-port 7890)

(def ^:private url-env "VIS_GATEWAY_URL")

(def ^:private token-env "VIS_GATEWAY_TOKEN")

(def ^:private client-label "vis-tui")

(def ^:private health-probe-timeout-ms 3000)

(def ^:private channel-read-timeout-ms 5000)

(def ^:private slash-catalog-timeout-ms 120000)

(defonce ^:private http-client
  (delay (http/client {:follow-redirects :normal
                       :connect-timeout 2000
                       :version :http1.1
                       :request {:headers {"accept" "*/*"}}})))

(defonce ^:private target* (atom nil))

(defonce ^:private target-checked? (atom false))

(defonce ^:private client-id (atom nil))

(defonce ^:private slash-cache* (atom []))

(defonce ^:private channel-listeners* (atom {}))

(defonce ^:private title-listeners* (atom {}))

(defonce ^:private release-hook-installed? (atom false))

(defonce ^:private subscriptions (atom {}))

(defonce ^:private client-finalizing? (atom false))

(defonce ^:private mux (atom {:subs {} :epoch 0 :future nil :stream nil}))

(defn now-ms ^long [] (System/currentTimeMillis))

(defn- release-version
  []
  (or (some-> (io/resource "vis-tui/VERSION")
              slurp
              str/trim
              not-empty)
      "dev"))

(defn- protocol-headers
  []
  {(gateway-contract/header :protocol) (str gateway-contract/protocol-version)
   (gateway-contract/header :minimum-gateway-protocol) (str
                                                         gateway-contract/minimum-gateway-protocol)
   (gateway-contract/header :client) client-label
   (gateway-contract/header :client-version) (release-version)})

(defn- remote-scheme-port [scheme] (if (= "https" scheme) 443 default-port))

(defn- target-entry
  [url token]
  (let [raw
        (not-empty (str/trim (str (or url (str "http://" default-host ":" default-port)))))

        trimmed
        (str/replace raw #"/+$" "")

        absolute
        (if (re-find #"^[A-Za-z][A-Za-z0-9+.-]*://" trimmed) trimmed (str "http://" trimmed))

        ^URI uri
        (try (URI. absolute) (catch Exception _ nil))

        scheme
        (some-> uri
                .getScheme
                str/lower-case)

        host
        (some-> uri
                .getHost
                not-empty)

        declared-port
        (long (if uri (.getPort uri) -1))

        prefix
        (str/replace (str (when uri (.getRawPath uri))) #"/+$" "")]

    (when-not (and host (contains? #{"http" "https"} scheme))
      (throw (ex-info (str "not a gateway address: " (pr-str (str url)))
                      {:type :gateway/invalid-url :url (str url) :vis/user-error true})))
    (let [port (if (pos? declared-port) declared-port (remote-scheme-port scheme))]
      {:base-url (str scheme "://" host ":" port prefix)
       :host host
       :port port
       :secret (not-empty (str/trim (str token)))})))

(defn configure!
  "Configure the gateway target. Call before any request."
  [{:keys [url token]}]
  (reset! target* (target-entry (or url (System/getenv url-env))
                                (or token (System/getenv token-env))))
  (reset! target-checked? false)
  (reset! client-id nil)
  @target*)

(defn- base-url [entry] (:base-url entry))

(defn- enc [x] (URLEncoder/encode (str x) StandardCharsets/UTF_8))

(defn- gw-send!
  [{:keys [secret] :as entry} method path
   {:keys [body as timeout-ms headers raw-body?] :or {as :string timeout-ms 30000}}]
  (http/request
    (cond-> {:client @http-client
             :method (keyword (str/lower-case method))
             :uri (str (base-url entry) path)
             :timeout timeout-ms
             :throw false
             :as as
             :headers (cond-> (merge (protocol-headers)
                                     headers
                                     {"Accept"
                                      (if (= as :stream) "text/event-stream" "application/json")})
                        (seq (str secret))
                        (assoc "Authorization"
                          (str "Bearer " secret) "X-Vis-Gateway-Secret"
                          (str secret))

                        (= as :stream)
                        (assoc "Accept-Encoding" "identity"))}
      (some? body)
      (assoc :body (if raw-body? body (wire/json-str body)))

      (and (some? body) (not raw-body?))
      (assoc-in [:headers "Content-Type"] "application/json"))))

(defn- parse-json-body
  [body]
  (or (wire/parse-json (cond (string? body) body
                             (bytes? body) (String. ^bytes body StandardCharsets/UTF_8)
                             :else (str body)))
      {}))

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

         reason
         (or (get parsed "message") (get-in parsed ["error" "message"]))]

     (when (>= status 400)
       (throw (ex-info (or reason (str "gateway HTTP " status))
                       (assoc parsed
                         :http-status status
                         :vis/user-error true))))
     parsed)))

(defn- shutdown-subscriptions!
  []
  (doseq [[_ {:keys [future stream]}] @subscriptions]
    (try (some-> ^java.io.Closeable @stream
                 .close)
         (catch Throwable _ nil))
    (when future (future-cancel future)))
  (reset! subscriptions {})
  (let [{:keys [future stream]} @mux]
    (when stream (try (.close ^java.io.Closeable stream) (catch Throwable _ nil)))
    (when future (future-cancel future)))
  nil)

(defn- release-client!
  []
  (when-let [cid @client-id]
    (try (send-json-with-entry! @target* "DELETE" (str "/v1/clients/" (enc cid)))
         (catch Throwable _ nil)
         (finally (reset! client-id nil)))))

(defn- ensure-release-hook!
  []
  (when (compare-and-set! release-hook-installed? false true)
    (.addShutdownHook (Runtime/getRuntime)
                      (Thread. ^Runnable
                               (fn []
                                 (reset! client-finalizing? true)
                                 (shutdown-subscriptions!)
                                 (release-client!))
                               "vis-tui-gateway-shutdown"))))

(defn- check-target!
  [entry]
  (when-not @target-checked?
    (locking target-checked?
      (when-not @target-checked?
        (let [response
              (gw-send! entry "GET" "/healthz" {:timeout-ms health-probe-timeout-ms})

              body
              (parse-json-body (:body response))

              handshake
              (gateway-contract/wire->handshake (get body "protocol"))

              verdict
              (gateway-contract/verdict {:gateway-protocol (:protocol handshake)
                                         :gateway-min-client (:min-client handshake)
                                         :gateway-version (:version handshake)
                                         :client-protocol gateway-contract/protocol-version
                                         :client-min-gateway
                                         gateway-contract/minimum-gateway-protocol
                                         :client-version (release-version)
                                         :client-name client-label})]

          (when-not (and (= 200 (:status response)) (= "ok" (get body "status")))
            (throw (ex-info "gateway is unavailable"
                            {:type :gateway/unavailable :http-status (:status response)})))
          (when-not (:is-compatible verdict)
            (throw (ex-info "gateway protocol is incompatible"
                            {:type :gateway/incompatible :verdict verdict})))
          (reset! target-checked? true))))))

(defn- target!
  []
  (let [entry (or @target* (configure! {}))]
    (check-target! entry)
    entry))

(defn- ensure-client!
  [entry]
  (when-not @client-id
    (locking client-id
      (when-not @client-id
        (let [response
              (send-json-with-entry! entry "POST" "/v1/clients" {:kind "tui-client"})

              registered-id
              (get response "client_id")]

          (when-not (seq registered-id)
            (throw (ex-info "gateway client registration returned no client_id"
                            {:type :gateway/invalid-client-registration})))
          (reset! client-id registered-id)
          (ensure-release-hook!)))))
  @client-id)

(defn- send-json!
  ([method path] (send-json! method path nil))
  ([method path body]
   (let [entry (target!)]
     (ensure-client! entry)
     (send-json-with-entry! entry method path body))))

(defn request!
  ([method path] (request! method path {}))
  ([method path opts]
   (when-not (and (string? path) (str/starts-with? path "/"))
     (throw (ex-info "gateway request path must start with /"
                     {:type :gateway/invalid-request-path :path path})))
   (let [entry (target!)]
     (ensure-client! entry)
     (gw-send! entry
               (str/upper-case (if (keyword? method) (name method) (str method)))
               path
               opts))))

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
         (target!)

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
  "Best-effort release of one session runtime through the configured gateway."
  [sid]
  (when sid
    (try (send-json! "POST" (str "/v1/sessions/" (enc sid) "/release")) (catch Throwable _ nil))))

(defn release-session!
  "Release one session runtime and this app's gateway lease."
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
        (target!)

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
        (target!)

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
                     (swap! session-model-cache assoc k {:at (now-ms) :val v}))
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
        (now-ms)

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
                     (swap! resources-cache assoc k {:at (now-ms) :val v}))
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
        (now-ms)

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
        (target!)

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
    (swap! session-model-cache assoc (str sid) {:at (now-ms) :val pref})
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

(defn fork-points
  "Lean, oldest-first turn rows at which `sid` can be forked."
  [sid]
  (mapv (fn [index row]
          {:id (get row "turn_id")
           :position (inc (long index))
           :user-request (get row "request")
           :created-at (get row "created_at")})
        (range)
        (get (send-json! "GET" (str "/v1/sessions/" (enc sid) "/forks")) "turns")))

(defn fork-session!
  "Fork `sid` through `turn-id`; nil means the whole session."
  ([sid] (fork-session! sid nil))
  ([sid turn-id]
   (get (send-json! "POST"
                    (str "/v1/sessions/" (enc sid) "/forks")
                    (cond-> {}
                      turn-id
                      (assoc :through_turn_id (str turn-id))))
        "session")))

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

;; --- Input Views (a run in the DAEMON blocked on the operator) ---
;;
;; `internal.view.core` parks the extension thread that raised the request and
;; publishes it on the in-process channel bus. That bus never leaves the JVM, so
;; a client process — the TUI attached to a serve daemon — can only reach the
;; input View over these routes.

(defn input-views
  "Pending input Views for `sid` IN THE DAEMON, oldest first, in
   canonical wire shape. The live `view.open` event is the fast path;
   this is how a client that attached LATER still finds the open form instead of
   watching a turn that never moves."
  [sid]
  (vec (get (send-json! "GET" (str "/v1/sessions/" (enc sid) "/views/input")) "requests")))

(defn live-views
  "The live views session `sid` is SHOWING in the daemon right now, oldest first,
   in canonical wire shape. The `view.*` events with `kind=live` are the fast path; this
   is how a client that attached MID-RUN paints the whole picture at once instead
   of waiting for the next patch to tell it a view exists."
  [sid]
  (vec (get (send-json! "GET" (str "/v1/sessions/" (enc sid) "/views/live")) "views")))

(defn view-action!
  "Apply one operator action to the DAEMON-side View `view-id` of `sid`.

   `action` is the closed View map: `{:action :submit :values …}`,
   `{:action :cancel}`, `{:action :select :node-id … :item-ids …}`, or
   `{:action :interrupt :note …}`. Kind is resolved by the daemon from the View,
   never encoded into this route. Returns the engine's canonical action outcome."
  [sid view-id action]
  (when-not (map? action)
    (throw (ex-info "View action must be a map" {:type :vis/view-invalid-action})))
  (let [action-name
        (some-> (:action action)
                name)

        _
        (when-not action-name
          (throw (ex-info "View action needs :action" {:type :vis/view-invalid-action})))

        res
        (send-json! "POST"
                    (str "/v1/sessions/" (enc sid) "/views/" (enc view-id) "/actions")
                    (assoc action :action action-name))]

    (cond-> {:action (keyword (get res "action" action-name))
             :view-id (get res "view_id" (str view-id))
             :is-accepted (boolean (get res "is_accepted"))}
      (contains? res "errors")
      (assoc :errors (get res "errors"))

      (contains? res "reason")
      (assoc :reason (get res "reason"))

      (contains? res "node_id")
      (assoc :node-id (get res "node_id"))

      (contains? res "item_ids")
      (assoc :item-ids (vec (get res "item_ids"))))))

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
  "Return the configured gateway when it serves `path`; never starts or replaces it."
  ([path] (ensure-gateway-serving! path nil))
  ([path _opts]
   (let [entry (target!)]
     (when-not (= :served (probe-route entry path))
       (throw (ex-info (str "gateway is not serving " path)
                       {:type :gateway/route-missing :path path :vis/user-error true})))
     entry)))

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
    (cond-> (-> (merge (select-keys message gateway-contract/turn-meta-keys)
                       (into {}
                             (filter (comp some? val))
                             (select-keys event gateway-contract/turn-meta-keys)))
                (assoc "content" blocks
                       "iteration_count" (or (get message "iteration_count") 1)
                       "session_turn_id" turn-id))
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
  (let [entry (target!)]
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

(def ^:private ^:const speech-model-poll-ms 400)

(defn- response-text
  [body]
  (cond (string? body) body
        (bytes? body) (String. ^bytes body StandardCharsets/UTF_8)
        :else (str body)))

(defn- parsed-response!
  [response]
  (let [body (wire/parse-json (response-text (:body response)))]
    (when-not (< (long (:status response)) 400)
      (throw (ex-info (or (get body "error") (str "gateway HTTP " (:status response)))
                      {:http-status (:status response) :body body})))
    body))

(defn- speech-query
  [engine-id voice-id]
  (let [parts (cond-> []
                engine-id
                (conj (str "engine=" (enc engine-id)))

                voice-id
                (conj (str "voice_id=" (enc voice-id))))]
    (when (seq parts) (str "?" (str/join "&" parts)))))

(defn prepare-speech-model!
  "Prepare one gateway-owned speech engine and wait until it is ready. `direction`
   is `:transcribe` or `:synthesize`; `on-progress` receives the gateway's
   string-keyed model state. No model or native runtime is initialized here."
  [direction {:keys [engine-id voice-id on-progress]}]
  (let [kind
        (if (= :transcribe direction) "voice" "speech")

        path
        (str "/v1/" kind "/model" (speech-query engine-id voice-id))]

    (loop [method :post]
      (let [model (parsed-response! (request! method path {:timeout-ms 120000}))
            status (get model "status")]

        (when on-progress (on-progress model))
        (case status
          "ready"
          model

          "failed"
          (throw (ex-info (or (get model "error") "speech model preparation failed")
                          {:type :gateway/speech-model-failed :model model}))

          (do (Thread/sleep (long speech-model-poll-ms)) (recur :get)))))))

(defn- speech-job-path
  [sid direction job-id suffix]
  (str "/v1/sessions/"
       (enc sid)
       "/"
       (if (= :transcribe direction) "voice" "speech")
       "/jobs/"
       (enc job-id)
       suffix))

(defn- await-speech-job!
  [sid direction job-id on-progress]
  (let [response (request! :get
                           (speech-job-path sid direction job-id "/events")
                           {:as :stream :timeout-ms 600000})]
    (when-not (= 200 (:status response)) (parsed-response! response))
    (with-open [^InputStream in (:body response)]
      (let [job (read-sse-frames! in
                                  (fn [event]
                                    (when on-progress (on-progress event))
                                    (when (true? (get event "is_done")) event))
                                  nil)]
        (when-not (map? job)
          (throw (ex-info "speech job stream closed before completion"
                          {:type :gateway/speech-stream-closed :job-id job-id})))
        (when-let [error (get job "error")]
          (throw (ex-info error {:type :gateway/speech-job-failed :job job})))
        job))))

(defn transcribe-audio!
  "Upload a WAV to the gateway-owned transcription engine, stream its progress,
   and return the transcript. `audio-path` is read by this client only; Sherpa and
   its model live solely in the gateway process."
  [sid audio-path {:keys [engine-id on-progress]}]
  (prepare-speech-model! :transcribe {:engine-id engine-id :on-progress on-progress})
  (let [query
        (speech-query engine-id nil)

        path
        (str "/v1/sessions/" (enc sid) "/voice" query)]

    (with-open [in (io/input-stream (io/file (str audio-path)))]
      (let [job (parsed-response! (request! :post
                                            path
                                            {:body in
                                             :raw-body? true
                                             :headers {"Content-Type" "audio/wav"}
                                             :timeout-ms 120000}))
            job-id (get job "id")]

        (try (get (await-speech-job! sid :transcribe job-id on-progress) "text")
             (finally (request! :delete (speech-job-path sid :transcribe job-id ""))))))))

(defn- write-temp-audio!
  ^File [body]
  (let [file (File/createTempFile "vis-gateway-speech" ".wav")]
    (with-open [out (io/output-stream file)]
      (.write out ^bytes body))
    file))

(defn synthesize-speech!
  "Ask the gateway-owned speech engine to synthesize `text` and return a temporary
   WAV file owned by the caller. Progress is streamed for asynchronous jobs."
  [sid text {:keys [engine-id voice-id on-progress]}]
  (prepare-speech-model! :synthesize
                         {:engine-id engine-id :voice-id voice-id :on-progress on-progress})
  (let [query
        (speech-query engine-id nil)

        path
        (str "/v1/sessions/" (enc sid) "/speech" query)

        response
        (request! :post
                  path
                  {:body (cond-> {:text text}
                           voice-id
                           (assoc :voice voice-id))
                   :as :bytes
                   :timeout-ms 600000})]

    (cond (= 200 (:status response)) (write-temp-audio! (:body response))
          (= 202 (:status response))
          (let [job-id (get (parsed-response! response) "id")]
            (try (await-speech-job! sid :synthesize job-id on-progress)
                 (let [audio (request! :get
                                       (speech-job-path sid :synthesize job-id "/audio")
                                       {:as :bytes :timeout-ms 120000})]
                   (when-not (= 200 (:status audio)) (parsed-response! audio))
                   (write-temp-audio! (:body audio)))
                 (finally (request! :delete (speech-job-path sid :synthesize job-id "")))))
          :else (do (parsed-response! response) nil))))

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
            (target!)

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
        (target!)

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
                      (try (let [entry (target!)
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
                  queue-mirror event — see `gateway-contract/queue-mirror-event-types`),
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

    (cond (and own? (contains? gateway-contract/turn-terminal-event-types type)) [:terminal event]
          (and own? (= "turn.queued.deleted" type)) [:terminal
                                                     (assoc event
                                                       "type" "turn.completed"
                                                       "status" "cancelled")]
          own? [:forward event]
          (contains? gateway-contract/queue-mirror-event-types type) [:forward event]
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

(defn suggest-files
  "Rank file rows in the gateway workspace of `sid`."
  [sid query]
  (let [path
        (str "/v1/sessions/" (enc sid) "/suggest?kind=file&q=" (enc (or query "")))

        body
        (send-json! "GET" path)]

    (vec (or (get body "items") (get body "rows") body []))))

;; Names consumed by the terminal application. The transport API itself keeps route-oriented names.
(def gateway-assign-project! assign-project!)

(def gateway-attach-turn-sync! attach-turn-sync!)

(def gateway-cancel-current-turn! cancel-current-turn!)

(def gateway-cancel-turn! cancel-turn!)

(def gateway-capabilities capabilities)

(def gateway-close-session! close-session!)

(def gateway-create-project! create-project!)

(def gateway-create-session! create-session!)

(def gateway-current-seq current-seq)

(def gateway-cycle-setting! cycle-setting!)

(def gateway-delete-queued-turn! delete-queued-turn!)

(def gateway-drain-idle! drain-idle!)

(def gateway-ensure-project-for-root! ensure-project-for-root!)

(def gateway-fleet-subscribe! fleet-subscribe!)

(def gateway-input-views input-views)

(def gateway-iteration-attachment-bytes iteration-attachment-bytes)

(def gateway-list-projects list-projects)

(def gateway-list-sessions list-sessions)

(def gateway-list-sessions-page list-sessions-page)

(def gateway-list-turns list-turns)

(def gateway-live-views live-views)

(def gateway-mcp-auth-cancel! mcp-auth-cancel!)

(def gateway-mcp-auth-complete! mcp-auth-complete!)

(def gateway-mcp-auth-logout! mcp-auth-logout!)

(def gateway-mcp-auth-poll! mcp-auth-poll!)

(def gateway-mcp-auth-start! mcp-auth-start!)

(def gateway-mcp-delete-server! mcp-delete-server!)

(def gateway-mcp-kill-server! mcp-kill-server!)

(def gateway-mcp-save-server! mcp-save-server!)

(def gateway-mcp-servers mcp-servers)

(def gateway-mcp-set-server-enabled! mcp-set-server-enabled!)

(def gateway-mcp-start-server! mcp-start-server!)

(def gateway-mcp-test-server! mcp-test-server!)

(def gateway-mux-subscribe! mux-subscribe!)

(def gateway-provider-auth-cancel! provider-auth-cancel!)

(def gateway-provider-auth-complete! provider-auth-complete!)

(def gateway-provider-auth-poll! provider-auth-poll!)

(def gateway-provider-auth-start! provider-auth-start!)

(def gateway-provider-auth-submit-key! provider-auth-submit-key!)

(def gateway-provider-limits provider-limits)

(def gateway-provider-model-options provider-models)

(def gateway-provider-remove! provider-remove!)

(def gateway-provider-status provider-status)

(def gateway-reconcile-running-turns! reconcile-running-turns!)

(def gateway-release-session! release-session!)

(def gateway-release-session-runtime! release-session-runtime!)

(def gateway-reorder-project-sessions! reorder-project-sessions!)

(def gateway-router-fleet router)

(def gateway-search-session-matches search-session-matches)

(def gateway-session-artifacts session-artifacts)

(def gateway-session-model session-model)

(def gateway-session-model-cached session-model-cached)

(def gateway-session-slashes session-slashes)

(def gateway-session-workspace session-workspace-info)

(def gateway-set-router-default! set-router-default!)

(def gateway-set-router-fallback! set-router-fallback!)

(def gateway-set-session-model! set-session-model!)

(def gateway-soul soul)

(def gateway-submit-turn! submit-turn!)

(def gateway-submit-turn-sync! submit-turn-sync!)

(def gateway-synthesize-speech! synthesize-speech!)

(def gateway-toggle-setting! toggle-setting!)

(def gateway-transcribe-audio! transcribe-audio!)

(def gateway-transcript transcript)

(def gateway-transcript-md transcript-md)

(def gateway-transcript-page transcript-page)

(def gateway-turn-trace turn-trace)

(def gateway-view-action! view-action!)

;; App-facing projections over the gateway's string-keyed provider payload.
(defonce ^:private router-cache* (atom nil))

(def ^:private router-cache-ttl-ms 30000)

(defn- invalidate-router-cache! [] (reset! router-cache* nil))

(defn- router-cached
  []
  (let [now
        (now-ms)

        cached
        @router-cache*]

    (if (and cached (< (- now (long (:at cached))) (long router-cache-ttl-ms)))
      (:rows cached)
      (let [rows (vec (router))]
        (reset! router-cache* {:at now :rows rows})
        rows))))

(defn- model-entry
  [model]
  (if (map? model)
    (cond-> {:name (or (get model "name") (:name model))}
      (contains? model "is_reasoning_effort_configurable")
      (assoc :reasoning-effort? (boolean (get model "is_reasoning_effort_configurable")))

      (contains? model "verbosity_style")
      (assoc :verbosity-style
        (some-> (get model "verbosity_style")
                keyword)))
    {:name (str model)}))

(defn- provider-entry
  [row]
  (let [models (mapv model-entry (or (get row "model_details") (get row "models") []))]
    (cond-> {:id (keyword (get row "id"))
             :label (get row "label")
             :models models
             :status (or (get row "status") {})
             :is-default (boolean (get row "is_default"))
             :default-model (get row "default_model")
             :is-fallback (boolean (get row "is_fallback"))
             :fallback-model (get row "fallback_model")}
      (get row "base_url")
      (assoc :base-url (get row "base_url"))

      (contains? row "is_managed")
      (assoc :is-managed (boolean (get row "is_managed")))

      (get row "auth_kind")
      (assoc :auth-kind (keyword (get row "auth_kind"))))))

(defn configured-providers [] (mapv provider-entry (router)))

(defn configured-providers-cached [] (mapv provider-entry (router-cached)))

(defn authenticated-preset-providers [] [])

(defn picker-fleet [] (configured-providers-cached))

(defn get-router [] {:providers (configured-providers-cached)})

(defn model-name
  [model]
  (some-> (if (map? model) (:name model) model)
          str))

(defn- provider-model
  [provider requested-model]
  (let [models
        (:models provider)

        wanted
        (some-> requested-model
                str)

        selected
        (or (some #(when (= wanted (model-name %)) %) models)
            (some #(when (= (:default-model provider) (model-name %)) %) models)
            (first models)
            (when wanted {:name wanted}))]

    (when selected (assoc selected :provider (:id provider)))))

(defn resolve-effective-model
  ([router]
   (let [providers
         (:providers router)

         provider
         (or (some #(when (:is-default %) %) providers) (first providers))]

     (when provider (provider-model provider (:default-model provider)))))
  ([router _routing] (resolve-effective-model router)))

(defn resolve-model-info
  [router provider-id model]
  (let [providers
        (:providers router)

        provider
        (or (some #(when (= (some-> provider-id
                                    keyword)
                            (:id %))
                     %)
                  providers)
            (some #(when (:is-default %) %) providers)
            (first providers))]

    (when provider (provider-model provider model))))

(defn reasoning-effort-configurable? [model] (not (false? (:reasoning-effort? model))))

(defn verbosity-configurable? [model] (some? (:verbosity-style model)))

(defn model-routing-status [& _] nil)

(defn router-initialized? [] (some? @router-cache*))

(defn rebuild-router! [& _] (invalidate-router-cache!) (get-router))

(defn refresh-cached-routers! [& _] (invalidate-router-cache!) nil)

(defn reload-config! [] (invalidate-router-cache!) nil)

(defn load-config-raw [] (config/load-raw))

(defn update-machine-config! [f] (config/update! f))

(defn save-toggles! [snapshot] (config/save-toggles! snapshot))

(defn save-config! [value] (config/update! (constantly value)))

(defn load-config
  []
  (let [providers
        (configured-providers-cached)

        primary
        (or (some #(when (:is-default %) %) providers) (first providers))

        fallback
        (some #(when (:is-fallback %) %) providers)]

    {:providers providers
     :default-provider (some-> primary
                               :id
                               name)
     :default-model (or (:default-model primary)
                        (some-> primary
                                :models
                                first
                                model-name))
     :fallback-provider (some-> fallback
                                :id
                                name)
     :fallback-model (:fallback-model fallback)}))

(defn resolve-default-selection
  [_config fleet]
  (when-let [provider (or (some #(when (:is-default %) %) fleet) (first fleet))]
    {:provider-id (:id provider)
     :model (or (:default-model provider)
                (some-> provider
                        :models
                        first
                        model-name))}))

(defn provider-presets
  []
  (mapv (fn [row]
          (cond-> {:id (keyword (get row "id"))
                   :label (get row "label")
                   :models (mapv model-entry (get row "models"))
                   :auth-kind (some-> (get row "auth_kind")
                                      keyword)
                   :is-local (boolean (get row "is_local"))}
            (get row "base_url")
            (assoc :base-url (get row "base_url"))

            (get row "api_style")
            (assoc :api-style (keyword (get row "api_style")))))
        (get (send-json! "GET" "/v1/provider-presets") "presets")))

(defn gateway-add-provider!
  [provider-id base-url]
  (let [answer (send-json! "POST"
                           "/v1/providers"
                           (cond-> {:id (name provider-id)}
                             (not (str/blank? (str base-url)))
                             (assoc :base_url base-url)))]
    (invalidate-router-cache!)
    answer))

(def provider-local-no-auth-ids #{:ollama :lm-studio})

(defn provider-default-model-configs [provider] (vec (:models provider)))

(defn provider-default-model-names [provider] (mapv model-name (:models provider)))

(defn provider-config-with-models [provider models] (assoc provider :models (vec models)))

(defn provider-managed?
  [provider-id]
  (boolean (:is-managed (some #(when (= (keyword provider-id) (:id %)) %)
                              (configured-providers-cached)))))

(defn provider-command-minted? [provider] (some? (:api-key-command provider)))

(defn provider-by-id
  [provider-id]
  (some #(when (= (keyword provider-id) (:id %)) %) (configured-providers-cached)))

(defn display-label
  [provider-id]
  (-> (name provider-id)
      (str/replace #"[-_]" " ")
      str/capitalize))

(defn provider-status-md
  [provider status limits]
  (str "# "
       (display-label (:id provider))
       "

"
       (if (get status "is_authenticated") "Authenticated" "Not authenticated")
       (when-let [message (or (get status "error") (:message (:error limits)))]
         (str "

" message))))

(defn structurally-silent? [block] (boolean (:vis/structurally-silent? block)))

;; Pure client-side projections and process-local UI state.
(def abbreviate-home paths/abbreviate-home)

(defn workspace-normalize-root [path] (.getCanonicalPath (io/file (str path))))

(def format-date fmt/format-date)

(def format-duration fmt/format-duration)

(def format-meta-line fmt/format-meta-line)

(def meta-fallback-note fmt/meta-fallback-note)

(def meta-summary-line fmt/meta-summary-line)

(def display-model-name fmt/display-model-name)

(def markdown->ast presentation/markdown->ast)

(def reasoning->ast presentation/reasoning->ast)

(def reasoning-preview-line-limit presentation/reasoning-preview-line-limit)

(def reasoning-collapse-min-hidden presentation/reasoning-collapse-min-hidden)

(def parse-block-display presentation/parse-block-display)

(def extract-text presentation/extract-text)

(def result-card form/result-card)

(def form->display form/->display)

(def form-with-display form/with-display)

(def form<-wire form/<-wire)

(def non-blank util/non-blank)

(def utf8 util/utf8)

(def wire-key wire/wire-key)

(def wire->engine wire/->engine)

(def error-message error/error-message)

(def format-error error/format-error)

(def make-progress-tracker progress/make-progress-tracker)

(def worker-future cancellation/worker-future)

(def cancellation-token cancellation/cancellation-token)

(def cancellation-set-future! cancellation/cancellation-set-future!)

(def cancellation? cancellation/cancellation?)

(def cancel! cancellation/cancel!)

(def notifications notifications/notifications)

(def notify! notifications/notify!)

(def watch-notifications! notifications/watch!)

(def unwatch-notifications! notifications/unwatch!)

(def register-toggle! toggles/register-toggle!)

(def registered-toggles toggles/registered-toggles)

(def toggles-for-channel toggles/toggles-for-channel)

(def toggle-spec toggles/toggle-spec)

(def toggle-value toggles/value-of)

(def toggle-enabled? toggles/enabled?)

(def toggle-set-value! toggles/set-value!)

(def toggle-cycle-value! toggles/cycle-value!)

(def toggle-reset-to-default! toggles/reset-to-default!)

(def toggles-snapshot toggles/snapshot)

(def toggles-hydrate-from-config! toggles/hydrate-from-config!)

(def toggle-add-listener! toggles/add-listener!)

(defn registered-slashes [] @slash-cache*)

(defn slash-parse
  [text]
  (when-let [command (some-> (str text)
                             str/trim
                             (str/split #"\s+")
                             first)]
    (when (str/starts-with? command "/")
      {:path (->> (str/split (subs command 1) #"/")
                  (remove str/blank?)
                  vec)})))

(defn publish-channel-event!
  [channel event]
  (doseq [[_ listener] (get @channel-listeners* channel)]
    (try (listener event) (catch Throwable _ nil)))
  event)

(defn add-channel-event-listener!
  [channel id listener]
  (swap! channel-listeners* assoc-in [channel id] listener)
  id)

(defn remove-channel-event-listener!
  [channel id]
  (swap! channel-listeners* update channel dissoc id)
  nil)

(defn add-title-listener!
  [sid listener]
  (swap! title-listeners* assoc-in [(str sid) :settled] listener)
  listener)

(defn remove-title-listener!
  [sid _listener]
  (swap! title-listeners* update (str sid) dissoc :settled)
  nil)

(defn add-title-pending-listener!
  [sid listener]
  (swap! title-listeners* assoc-in [(str sid) :pending] listener)
  listener)

(defn remove-title-pending-listener!
  [sid _listener]
  (swap! title-listeners* update (str sid) dissoc :pending)
  nil)

;; The real terminal and the real stdout belong to `tui.tty`; every consumer
;; reaches them through this namespace, so keep these as plain aliases.
(def tty-in tty/tty-in)

(def tty-out tty/tty-out)

(def original-stdout tty/original-stdout)

(defn init! [& _] true)

(defn shutdown!
  []
  (reset! client-finalizing? true)
  (shutdown-subscriptions!)
  (release-client!)
  true)
