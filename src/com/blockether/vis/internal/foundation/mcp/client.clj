(ns com.blockether.vis.internal.foundation.mcp.client
  "Minimal Model Context Protocol (MCP) client. Speaks JSON-RPC 2.0 over two
   transports:

     :stdio  — spawn the server process and frame newline-delimited JSON-RPC on
               its stdin/stdout (the dominant local-server pattern). A daemon
               thread drains stderr into the vis log so a chatty server never
               deadlocks on a full pipe.

     :http   — Streamable HTTP: POST each JSON-RPC message to one endpoint; the
               reply is either `application/json` (one response) or
               `text/event-stream` (SSE), read as a stream. A stream that ends
               before its reply continues on a GET with `Last-Event-ID`. The
               `Mcp-Session-Id` handed back by `initialize` rides on every later
               request, a `DELETE` frees it on shutdown, and an optional GET
               listen loop hears server-pushed notifications.

   On both transports a `ping` of the server gets its answer, any other server
   request gets `Method not found`, and `notifications/tools/list_changed`
   invalidates the tools cache.

   OAuth 2.1 (spec `2025-06-18`) is supported for HTTP transports via
   `oauth.clj`: pass `:bearer-fn` (a 0/1-arg fn yielding the current Bearer
   token, called with the just-rejected token on 401). A 401 triggers a
   single-flight refresh and one automatic retry.

   A `conn` is a plain map of closures + state; the extension treats it
   opaquely. Lifecycle: `connect` (which performs the `initialize` handshake) →
   `list-tools` / `call-tool` → `close`."
  (:require [babashka.http-client :as http]
            [charred.api :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.session.cancellation :as cancellation]
            [com.blockether.vis.internal.foundation.mcp.http :as mcp-http]
            [com.blockether.vis.internal.sandbox.jail :as process-jail]
            [com.blockether.vis.internal.util :as util]
            [taoensso.telemere :as tel])
  (:import (java.io BufferedReader)
           (java.util.concurrent ConcurrentHashMap)))

(def ^:private protocol-version "2025-11-25")

(def default-timeout-ms 30000)

;; JSON helpers (charred). JSON-RPC keys stay STRINGS end to end.

(defn- json-safe
  "Total JSON projection of `x`. Keywords/symbols become strings, map keys become
   strings, NaN/±Infinity become nil, ratios become doubles, and anything charred
   has no encoder for becomes its `str`.

   The `arguments` of a tool call arrive from a model or from another extension,
   so exactly one NaN or one non-string key used to throw a raw
   `CharredException` from INSIDE the JSON-RPC write — after the request id was
   allocated, with the wire framing half-formed and the caller's turn poisoned.
   Encoding is total here so a bad argument comes back as an MCP error, not as a
   broken transport. `depth` caps pathological nesting instead of blowing the
   stack."
  ([x] (json-safe x 0))
  ([x depth]
   (cond (nil? x) nil
         (string? x) x
         (boolean? x) x
         (> (long depth) 64) (str x)
         (keyword? x) (subs (str x) 1)
         (symbol? x) (str x)
         (ratio? x) (double x)
         (and (float? x) (not (Double/isFinite (double x)))) nil
         (number? x) x
         (map? x) (persistent! (reduce-kv (fn [m k v]
                                            (let [k' (json-safe k (inc (long depth)))]
                                              (assoc! m
                                                      (if (string? k') k' (str k'))
                                                      (json-safe v (inc (long depth))))))
                                          (transient {})
                                          x))
         (or (sequential? x) (set? x) (instance? java.util.Collection x))
         (mapv #(json-safe % (inc (long depth))) x)
         (instance? java.util.Map x) (json-safe (into {} x) depth)
         :else (str x))))

(defn- ->json ^String [m] (util/json-str (json-safe m)))

(defn- json-> [^String s] (json/read-json s))

(defn- clj-name [k] (if (keyword? k) (name k) (str k)))

(defn- rpc-error->ex
  [name method err]
  (ex-info (str "MCP " name " " method " failed: " (get err "message" "unknown error"))
           {:type :mcp/rpc-error
            :server name
            :method method
            :code (get err "code")
            :data (get err "data")}))

;; Messages that the server sends on its own, on either transport.

(defn- server-request-reply
  "The reply to a request that the SERVER sent as `msg`, or nil when `msg` is not a
   request. Vis declares no client capability, so only `ping` has a result."
  [msg]
  (when (and (map? msg) (string? (get msg "method")) (some? (get msg "id")))
    (if (= "ping" (get msg "method"))
      {"jsonrpc" "2.0" "id" (get msg "id") "result" {}}
      {"jsonrpc" "2.0"
       "id" (get msg "id")
       "error" {"code" -32601 "message" (str "Method not found: " (get msg "method"))}})))

(defn- handle-incoming!
  "Handle a message of the server that is not a reply: answer a request through
   `send!`, and give a notification to the fn in the atom `incoming`. Never throws."
  [msg send! incoming]
  (try (if-let [reply (server-request-reply msg)]
         (send! reply)
         (when (and (map? msg) (string? (get msg "method")))
           (when-let [on-notification @incoming]
             (on-notification msg))))
       (catch Throwable _ nil)))

;; stdio transport — async, full-duplex; correlate replies by id.

(defn- start-stderr-drain!
  "Spawn a daemon thread that drains `err-stream` line-by-line into the vis
   log so a chatty server never blocks on a full ~64KB pipe. Silent on empty
   lines; tagged with the server name so multiple servers stay distinguishable."
  [server-name ^java.io.InputStream err-stream]
  (doto (Thread. ^Runnable
                 (fn []
                   (try (with-open [r (io/reader err-stream)]
                          (doseq [^String line (line-seq r)]
                            (when-not (str/blank? line)
                              (tel/log!
                                {:level :debug :id ::stdio-stderr :data {:server server-name}}
                                (str "mcp[" server-name "] " line)))))
                        (catch Throwable _ nil)))
                 (str "mcp-stdio-err-" server-name))
    (.setDaemon true)
    (.start)))

(defn- stop-stdio-process!
  [^Process proc]
  ;; Snapshot the tree BEFORE the parent dies. MCP launchers (`npx`, `uvx`,
  ;; `bunx`) leave their server child behind if only the launcher is stopped.
  (let [kids
        (try (with-open [descendants (.descendants (.toHandle proc))]
               (vec (iterator-seq (.iterator descendants))))
             (catch Throwable _ []))

        destroy-kids!
        (fn [f]
          (run! (fn [^java.lang.ProcessHandle h]
                  (try (when (.isAlive h) (f h)) (catch Throwable _ nil)))
                kids))]

    ;; Closing stdin is the polite stop; the signals do not need consent.
    (try (.close (.getOutputStream proc)) (catch Throwable _ nil))
    (destroy-kids! (fn [^java.lang.ProcessHandle h]
                     (.destroy h)))
    (try (.destroy proc) (catch Throwable _ nil))
    (try (when-not (.waitFor proc 2 java.util.concurrent.TimeUnit/SECONDS) (.destroyForcibly proc))
         (catch Throwable _ nil))
    (destroy-kids! (fn [^java.lang.ProcessHandle h]
                     (.destroyForcibly h)))))

(defn- stdio-transport
  "Wire newline-delimited JSON-RPC around a process owned by start-stdio!."
  [name ^Process proc]
  (let [out
        (BufferedReader. (io/reader (.getInputStream proc)))

        in
        (.getOutputStream proc)

        pending
        (ConcurrentHashMap.)

        next-id
        (atom 0)

        closed?
        (atom false)

        ;; `connect` puts the notification handler here.
        incoming
        (atom nil)

        write!
        (fn [m]
          (locking in (.write in (.getBytes (str (->json m) "\n") "UTF-8")) (.flush in)))

        read-loop
        (fn []
          (try (loop []

                 (when-let [line (.readLine out)]
                   (when-not (str/blank? line)
                     (try (let [msg (json-> line)]
                            ;; A message with a method is no reply, whatever its id.
                            (if (and (map? msg) (contains? msg "method"))
                              (handle-incoming! msg write! incoming)
                              (when-let [id (get msg "id")]
                                (when-let [p (.remove pending id)]
                                  (deliver p msg)))))
                          (catch Throwable t
                            (tel/log! {:level :debug
                                       :id ::stdio-parse
                                       :data {:server name :error (ex-message t)}}
                                      "MCP stdio: unparseable line dropped"))))
                   (recur)))
               (catch Throwable _ nil)
               (finally (reset! closed? true)
                        ;; EOF does not release runtime-owned pipe descriptors.
                        (try (.close out) (catch Throwable _ nil))
                        (doseq [k (enumeration-seq (.keys pending))]
                          (when-let [p (.remove pending k)]
                            (deliver p {"error" {"message" "server stream closed"}}))))))]

    (start-stderr-drain! name (.getErrorStream proc))
    (doto (Thread. ^Runnable read-loop (str "mcp-stdio-" name)) (.setDaemon true) (.start))
    {:pid (try (.pid proc) (catch Throwable _ nil))
     :incoming incoming
     :request-fn (fn [method params timeout-ms]
                   (when @closed?
                     (throw (ex-info "MCP server not running" {:type :mcp/closed :server name})))
                   (let [id
                         (str (swap! next-id inc))

                         p
                         (promise)]

                     (.put pending id p)
                     (write! (cond-> {"jsonrpc" "2.0" "id" id "method" method}
                               (some? params)
                               (assoc "params" params)))
                     (let [msg (deref p timeout-ms ::timeout)]
                       (.remove pending id)
                       (cond (= msg ::timeout)
                             (throw (ex-info (str "MCP " name " " method " timed out")
                                             {:type :mcp/timeout :server name :method method}))
                             (get msg "error") (throw (rpc-error->ex name method (get msg "error")))
                             :else (get msg "result")))))
     :notify-fn (fn [method params]
                  (when-not @closed?
                    (write! (cond-> {"jsonrpc" "2.0" "method" method}
                              (some? params)
                              (assoc "params" params)))))
     :close-fn (fn []
                 (reset! closed? true)
                 (stop-stdio-process! proc))
     :alive-fn (fn []
                 (and (not @closed?) (.isAlive proc)))}))

(defn- start-stdio!
  "Spawn `command`+`args` (with extra `env`), wire newline-delimited JSON-RPC.
   Returns `{:request-fn :notify-fn :close-fn :alive-fn :pid}`."
  [name {:keys [command args env cwd]}]
  (let [directory
        (when (and cwd (string? cwd) (.isDirectory (io/file cwd))) cwd)

        extra-env
        (into {}
              (map (fn [[k v]]
                     [(str (clj-name k)) (str v)]))
              env)

        ^Process proc
        (process-jail/spawn! (vec (cons command (map str (or args []))))
                             directory
                             nil
                             {:extra-environment extra-env})]

    (try (stdio-transport name proc)
         (catch Throwable t
           ;; Setup can fail before read-loop owns stdout or connect has a conn
           ;; to close. Reclaim the process and every available pipe ourselves.
           (stop-stdio-process! proc)
           (try (.close (.getInputStream proc)) (catch Throwable _ nil))
           (try (.close (.getErrorStream proc)) (catch Throwable _ nil))
           (throw t)))))

;; Streamable-HTTP transport — POST → JSON | SSE; DELETE on close;
;; optional GET listen loop for server-pushed notifications.

;; HTTP client is shared with `oauth.clj` via `mcp-http/client` — one JDK
;; `HttpClient` for every MCP subsystem (selector pool + virtual-thread
;; executor). See `mcp/http.clj` for the perf/threading rationale.

(defn- response?
  [msg]
  (and (map? msg)
       (not (contains? msg "method"))
       (or (contains? msg "result") (contains? msg "error"))))

(defn- read-sse-event
  "Read the next event of an SSE stream as `{:id :retry :data}`, or nil at the end
   of the stream. `:data` joins the data lines. An unfinished event is dropped."
  [^BufferedReader rdr]
  (loop [event nil]
    (let [line (.readLine rdr)]
      (cond (nil? line) nil
            (= "" line) (or event (recur nil))
            (str/starts-with? line ":") (recur event)
            :else
            (let [i (str/index-of line ":")
                  field (if i (subs line 0 (long i)) line)
                  value (if i (subs line (inc (long i))) "")
                  value (if (str/starts-with? value " ") (subs value 1) value)
                  event (or event {})]

              (recur (case field
                       "data"
                       (update event :data #(if % (str % "\n" value) value))

                       "id"
                       (assoc event :id value)

                       "retry"
                       (if (re-matches #"\d+" value) (assoc event :retry (parse-long value)) event)

                       event)))))))

(defn- apply-headers
  [headers]
  (reduce-kv (fn [result k v]
               (assoc result (clj-name k) (str v)))
             {}
             (or headers {})))

(defn- response-header
  [response header-name]
  (let [wanted (str/lower-case header-name)]
    (some (fn [[k v]]
            (when (= wanted (str/lower-case (clj-name k))) (if (sequential? v) (first v) v)))
          (:headers response))))

(defn- event-stream?
  [response]
  (str/starts-with? (str/lower-case (str (response-header response "content-type")))
                    "text/event-stream"))

(defn- close-body!
  [response]
  (when-let [body (:body response)]
    (try (.close ^java.io.Closeable body) (catch Throwable _ nil))))

(defn- body-text
  ^String [response]
  (if-let [body (:body response)]
    (with-open [in ^java.io.InputStream body]
      (slurp in :encoding "UTF-8"))
    ""))

(defn- sleep!
  "Sleep `ms`; false when an interrupt ended the sleep."
  [ms]
  (try (Thread/sleep (long ms)) true (catch InterruptedException _ false)))

(defn- http-listen-loop!
  "The SSE listen channel: GET the MCP endpoint and handle each message while the
   stream is open. After the stream ends, connect again after the delay that the
   server gave, with the id of the last event. The loop ends when the transport
   closes, on an interrupt, or when the server has no listen channel (HTTP 404 or
   405). `open!` and `read-stream` are those of the transport."
  [server-name open! read-stream closed?]
  (letfn
    [(listen-once [cursor]
       (let [resp
             (open! {:method :get
                     :headers (cond-> {"Accept" "text/event-stream"}
                                (:id cursor)
                                (assoc "Last-Event-ID" (:id cursor)))
                     :timeout 3600000})

             status
             (long (:status resp))]

         (cond (or (= 404 status) (= 405 status)) (do (close-body! resp) ::stop)
               (or (>= status 400) (not (event-stream? resp)))
               (do (close-body! resp)
                   (tel/log! {:level :debug
                              :id ::http-listen-status
                              :data {:server server-name :status status}}
                             "MCP HTTP listen without a stream")
                   ::retry)
               :else (read-stream resp cursor (constantly false)))))]
    (doto (Thread.
            ^Runnable
            (fn []
              (loop [cursor
                     {}

                     backoff
                     1000]

                (when-not @closed?
                  (let [outcome (try (listen-once cursor)
                                     (catch Throwable t
                                       ;; An interrupt is the ONE failure that ENDS the loop.
                                       (if (cancellation/preserve-interrupt! t) ::stop ::retry)))]
                    (cond (= ::stop outcome) nil
                          (= ::retry outcome) (when (sleep! backoff)
                                                (recur cursor (min 30000 (* 2 (long backoff)))))
                          :else (when (sleep! (or (:retry outcome) 1000)) (recur outcome 1000)))))))
            (str "mcp-http-listen-" server-name))
      (.setDaemon true)
      (.start))))

(defn- start-http!
  "Streamable-HTTP transport against `url` with optional static `headers` and
   an optional `:bearer-fn` OAuth token provider. `:www-auth-atom`, when given, is
   the caller's atom to keep pointed at the latest `WWW-Authenticate` challenge."
  [name {:keys [url headers bearer-fn www-auth-atom listen?]}]
  (let [session
        (atom nil)

        protocol-version*
        (atom protocol-version)

        ;; The CALLER's atom when it supplied one. A server answering 401 with a
        ;; Bearer challenge is the only proof it wants OAuth at all: `oauth.clj`
        ;; discovers from that live header, and the extension tells "sign in" from
        ;; "it is down" by whether this ever got set.
        www-auth
        (or www-auth-atom (atom nil))

        closed?
        (atom false)

        ;; `connect` puts the notification handler here.
        incoming
        (atom nil)

        listen-thread
        (atom nil)

        send1
        (fn [request bearer]
          (let [resp (http/request (assoc request
                                     :uri url
                                     :client @mcp-http/client
                                     :headers (cond-> (merge (apply-headers headers)
                                                             {"MCP-Protocol-Version"
                                                              @protocol-version*}
                                                             (:headers request))
                                                @session
                                                (assoc "Mcp-Session-Id" @session)

                                                bearer
                                                (assoc "Authorization" (str "Bearer " bearer)))
                                     :throw false
                                     ;; A stream: the reply can come before the server ends the body.
                                     :as :stream))]
            (when-let [sid (response-header resp "mcp-session-id")]
              (reset! session sid))
            (when-let [wa (response-header resp "www-authenticate")]
              (reset! www-auth wa))
            resp))

        ;; Send `request` (`:method`, `:headers`, `:body`, `:timeout`) and return the
        ;; response with an open body. The caller closes the body.
        open!
        (fn [request]
          (let [tok
                (when bearer-fn (try (bearer-fn) (catch Throwable _ nil)))

                first-resp
                (send1 request tok)]

            (if (and (= 401 (:status first-resp)) bearer-fn)
              ;; `bearer-fn` refuses only with the typed `:mcp/oauth-required`. It must
              ;; escape: as a plain HTTP 401 it skipped the sign-in backoff, and every
              ;; turn connected again (#458).
              (let [fresh (try (bearer-fn tok)
                               (catch Throwable t (close-body! first-resp) (throw t)))]
                (if (and fresh (not= fresh tok))
                  (do (close-body! first-resp) (send1 request fresh))
                  first-resp))
              first-resp)))

        post!
        (fn [body timeout-ms]
          (open! {:method :post
                  :headers {"Content-Type" "application/json"
                            "Accept" "application/json, text/event-stream"}
                  :body body
                  :timeout timeout-ms}))

        dispatch!
        (fn [msg]
          (handle-incoming! msg #(close-body! (post! (->json %) 10000)) incoming))

        ;; Read the events of `resp` until a message for which `reply?` is true, or
        ;; until the stream ends. Returns `cursor` with the last event `:id` and
        ;; `:retry` delay, plus `:reply` when one came.
        read-stream
        (fn [resp cursor reply?]
          (with-open [rdr (BufferedReader. (io/reader (:body resp) :encoding "UTF-8"))]
            (loop [cursor cursor]
              (if-let [event (read-sse-event rdr)]
                (let [cursor (merge cursor (select-keys event [:id :retry]))
                      msg (when-not (str/blank? (:data event))
                            (try (json-> (:data event)) (catch Throwable _ nil)))]

                  (if (reply? msg)
                    (assoc cursor :reply msg)
                    (do (when msg (dispatch! msg)) (recur cursor))))
                cursor))))

        ;; The JSON-RPC reply to the request that `resp` answers, or nil. A stream
        ;; that ends before the reply continues on a GET with the id of its last
        ;; event, after the delay that the server gave.
        reply-of
        (fn [resp method timeout-ms]
          (let [status (long (:status resp))]
            (cond (>= status 400) (throw (ex-info (str "MCP " name " HTTP " status " on " method)
                                                  {:type :mcp/http-error
                                                   :server name
                                                   :status status
                                                   :body (body-text resp)}))
                  (event-stream? resp)
                  (loop [resp resp
                         cursor {}]

                    (let [cursor (read-stream resp cursor response?)]
                      (cond (:reply cursor) (:reply cursor)
                            (:id cursor)
                            (do (Thread/sleep (long (or (:retry cursor) 1000)))
                                (let [resumed (open! {:method :get
                                                      :headers {"Accept" "text/event-stream"
                                                                "Last-Event-ID" (:id cursor)}
                                                      :timeout timeout-ms})]
                                  (if (and (= 200 (:status resumed)) (event-stream? resumed))
                                    (recur resumed cursor)
                                    (close-body! resumed)))))))
                  :else (let [value (try (json-> (body-text resp)) (catch Throwable _ nil))]
                          (if (sequential? value) (first (filter response? value)) value)))))]

    (cond-> {:request-fn
             (fn [method params timeout-ms]
               (try (let [body
                          (->json
                            (cond-> {"jsonrpc" "2.0" "id" (str (System/nanoTime)) "method" method}
                              (some? params)
                              (assoc "params" params)))

                          ;; A stream read has no timeout of its own, so the exchange
                          ;; runs apart and this thread keeps the deadline.
                          exchange
                          (future (reply-of (post! body timeout-ms) method timeout-ms))

                          msg
                          (try (deref exchange timeout-ms ::timeout)
                               (catch java.util.concurrent.ExecutionException e
                                 (throw (or (ex-cause e) e)))
                               (catch InterruptedException e (future-cancel exchange) (throw e)))]

                      (cond (= ::timeout msg)
                            (do (future-cancel exchange)
                                (throw (ex-info (str "MCP " name " " method " timed out")
                                                {:type :mcp/timeout :server name :method method})))
                            (not (map? msg)) (throw (ex-info (str "MCP " name
                                                                  " empty reply on " method)
                                                             {:type :mcp/protocol :server name}))
                            (get msg "error") (throw (rpc-error->ex name method (get msg "error")))
                            :else (get msg "result")))
                    (catch Throwable t
                      ;; A refusal of the peer and a slow reply are no lost transport.
                      (when-not (or (contains? #{:mcp/rpc-error :mcp/timeout} (:type (ex-data t)))
                                    (instance? InterruptedException t))
                        (reset! closed? true))
                      (throw t))))
             :notify-fn (fn [method params]
                          (try (close-body! (post! (->json (cond-> {"jsonrpc" "2.0" "method" method}
                                                             (some? params)
                                                             (assoc "params" params)))
                                                   10000))
                               (catch Throwable _ nil)))
             :close-fn (fn []
                         (reset! closed? true)
                         (some-> ^Thread @listen-thread
                                 .interrupt)
                         (when-let [sid @session]
                           (try (http/request
                                  {:uri url
                                   :method :delete
                                   :client @mcp-http/client
                                   :headers
                                   (cond-> (merge (apply-headers headers)
                                                  {"Mcp-Session-Id" sid
                                                   "MCP-Protocol-Version" @protocol-version*})
                                     bearer-fn
                                     (assoc "Authorization"
                                       (str "Bearer " (try (bearer-fn) (catch Throwable _ nil)))))
                                   :timeout 5000
                                   :throw false
                                   :as :string})
                                (catch Throwable _ nil)))
                         (reset! session nil))
             :alive-fn (fn []
                         (not @closed?))
             :incoming incoming
             :www-auth-atom www-auth
             :session-atom session
             :protocol-version-atom protocol-version*}
      listen?
      (assoc :listen-start-fn
        (fn []
          (reset! listen-thread (http-listen-loop! name open! read-stream closed?)))))))

;; Public client surface

(defn- transport-of
  [{:keys [transport]}]
  (case (some-> transport
                name)
    ("streamable_http" "streamable-http")
    :streamable-http

    "stdio"
    :stdio

    (throw (ex-info "Unsupported MCP transport" {:transport transport}))))

(defn connect
  "Connect to MCP server `name` per its `spec` and run the `initialize`
   handshake. `spec` supports:
     stdio           → `{:transport :stdio :command :args :env :cwd :timeout-ms}`
     streamable-http → `{:transport :streamable-http :url :headers :bearer-fn
                         :timeout-ms :listen? :on-notification}`
   Returns an opaque `conn` map (or throws). A failed handshake always closes the
   transport it already opened, so failed stdio starts cannot orphan processes."
  [name spec]
  (let [transport
        (transport-of spec)

        t
        (case transport
          :stdio
          (start-stdio! name spec)

          :streamable-http
          (start-http! name spec)

          (throw (ex-info (str "MCP: unknown transport " (pr-str transport))
                          {:type :mcp/config :server name})))

        timeout-ms
        (or (:timeout-ms spec) default-timeout-ms)

        conn
        (merge t
               {:name name
                :transport transport
                :spec spec
                :connected-at (util/now-ms)
                :tools (atom nil)
                :timeout-ms timeout-ms})]

    ;; A `notifications/tools/list_changed` of the server, on either transport,
    ;; invalidates the tools cache so the next `list-tools` fetches again. A
    ;; caller-supplied `:on-notification` fires after that.
    (reset! (:incoming t) (let [user-cb (:on-notification spec)]
                            (fn [msg]
                              (when (= "notifications/tools/list_changed" (get msg "method"))
                                (reset! (:tools conn) nil))
                              (when user-cb (try (user-cb msg) (catch Throwable _ nil))))))
    (try (let [init
               ((:request-fn conn)
                 "initialize"
                 {"protocolVersion" protocol-version
                  "capabilities" {}
                  "clientInfo" {"name" "vis" "version" "0.1.0"}}
                 timeout-ms)

               negotiated-protocol-version
               (or (get init "protocolVersion") protocol-version)]

           ;; Every message after `initialize` names the negotiated version: a server
           ;; refuses a version that it does not know.
           (when-let [version-atom (:protocol-version-atom t)]
             (reset! version-atom negotiated-protocol-version))
           ;; Per spec, acknowledge before issuing further requests.
           ((:notify-fn conn) "notifications/initialized" nil)
           (when-let [start (:listen-start-fn t)]
             (start))
           (assoc conn
             :server-info (get init "serverInfo")
             :server-capabilities (get init "capabilities")
             :protocol-version negotiated-protocol-version))
         (catch Throwable t (try ((:close-fn conn)) (catch Throwable _ nil)) (throw t)))))

(defn list-tools
  "`tools/list` → vector of tool maps `{\"name\" \"description\" \"inputSchema\"}`,
   from every page of the server. Cached on the conn after the first call;
   invalidated by `notifications/tools/list_changed`."
  [conn]
  (or @(:tools conn)
      (let [timeout-ms
            (or (:timeout-ms conn) default-timeout-ms)

            tools
            (loop [tools
                   []

                   cursor
                   nil]

              (let [result
                    ((:request-fn conn) "tools/list" (if cursor {"cursor" cursor} {}) timeout-ms)

                    tools
                    (into tools (get result "tools"))

                    next-cursor
                    (get result "nextCursor")]

                (if (and (string? next-cursor) (seq next-cursor) (not= cursor next-cursor))
                  (recur tools next-cursor)
                  tools)))]

        (reset! (:tools conn) tools)
        tools)))

(defn call-tool
  "`tools/call` `tool-name` with `arguments` (a map). Returns the result map
   (`{\"content\" [...] \"isError\" bool}`)."
  ([conn tool-name arguments]
   (call-tool conn tool-name arguments (or (:timeout-ms conn) default-timeout-ms)))
  ([conn tool-name arguments timeout-ms]
   ((:request-fn conn) "tools/call" {"name" tool-name "arguments" (or arguments {})} timeout-ms)))

(defn alive? [conn] (boolean (try ((:alive-fn conn)) (catch Throwable _ false))))

(defn close [conn] (try ((:close-fn conn)) (catch Throwable _ nil)) (reset! (:tools conn) nil) nil)
