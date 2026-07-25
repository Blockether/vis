(ns com.blockether.vis.internal.foundation.mcp.client
  "Minimal Model Context Protocol (MCP) client. Speaks JSON-RPC 2.0 over two
   transports:

     :stdio  — spawn the server process and frame newline-delimited JSON-RPC on
               its stdin/stdout (the dominant local-server pattern). A daemon
               thread drains stderr into the vis log so a chatty server never
               deadlocks on a full pipe.

     :http   — Streamable HTTP: POST each JSON-RPC message to one endpoint; the
               reply is either `application/json` (one response) or
               `text/event-stream` (SSE) — both handled. The `Mcp-Session-Id`
               handed back by `initialize` rides on every later request, a
               `DELETE` frees it on shutdown, and an optional GET listen loop
               reacts to server-pushed `notifications/tools/list_changed`.

   OAuth 2.1 (spec `2025-06-18`) is supported for HTTP transports via
   `oauth.clj`: pass `:bearer-fn` (a 0/1-arg fn yielding the current Bearer
   token, called with the just-rejected token on 401). A 401 triggers a
   single-flight refresh and one automatic retry.

   A `conn` is a plain map of closures + state; the extension treats it
   opaquely. Lifecycle: `connect` (which performs the `initialize` handshake) →
   `list-tools` / `call-tool` → `close`."
  (:require [charred.api :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [taoensso.telemere :as tel])
  (:import
    (java.io BufferedReader)
    (java.net URI)
    (java.net.http HttpClient HttpRequest HttpRequest$BodyPublishers HttpResponse$BodyHandlers)
    (java.time Duration)
    (java.util.concurrent ConcurrentHashMap)))

(def ^:private protocol-version "2025-06-18")
(def default-timeout-ms 30000)
(def ^:private default-connect-timeout-ms 15000)

(defn- now-ms [] (System/currentTimeMillis))

;; ---------------------------------------------------------------------------
;; JSON helpers (charred). JSON-RPC keys stay STRINGS end to end.
;; ---------------------------------------------------------------------------

(defn- ->json ^String [m] (json/write-json-str m))
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

;; ===========================================================================
;; stdio transport — async, full-duplex; correlate replies by id.
;; ===========================================================================

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

(defn- start-stdio!
  "Spawn `command`+`args` (with extra `env`), wire newline-delimited JSON-RPC.
   Returns `{:request-fn :notify-fn :close-fn :alive-fn :pid}`."
  [name {:keys [command args env cwd]}]
  (let
    [pb
     (ProcessBuilder. ^java.util.List (vec (cons command (map str (or args [])))))

     _
     (when (seq env)
       (let [m (.environment pb)]
         (doseq [[k v] env]
           (.put m (str (clj-name k)) (str v)))))

     _
     (when (and cwd (string? cwd) (.isDirectory (io/file cwd))) (.directory pb (io/file cwd)))

     _
     (.redirectErrorStream pb false)

     proc
     (.start pb)

     out
     (BufferedReader. (io/reader (.getInputStream proc)))

     in
     (.getOutputStream proc)

     pending
     (ConcurrentHashMap.)

     next-id
     (atom 0)

     closed?
     (atom false)

     write!
     (fn [m]
       (locking in (.write in (.getBytes (str (->json m) "\n") "UTF-8")) (.flush in)))

     read-loop
     (fn []
       (try (loop []

              (when-let [line (.readLine out)]
                (when-not (str/blank? line)
                  (try (let [msg (json-> line)]
                         (when-let [id (get msg "id")]
                           (when-let [p (.remove pending id)]
                             (deliver p msg))))
                       (catch Throwable t
                         (tel/log! {:level :debug
                                    :id ::stdio-parse
                                    :data {:server name :error (ex-message t)}}
                                   "MCP stdio: unparseable line dropped"))))
                (recur)))
            (catch Throwable _ nil)
            (finally (reset! closed? true)
                     (doseq [k (enumeration-seq (.keys pending))]
                       (when-let [p (.remove pending k)]
                         (deliver p {"error" {"message" "server stream closed"}}))))))]

    (start-stderr-drain! name (.getErrorStream proc))
    (doto (Thread. ^Runnable read-loop (str "mcp-stdio-" name)) (.setDaemon true) (.start))
    {:pid (try (.pid proc) (catch Throwable _ nil))
     :request-fn (fn [method params timeout-ms]
                   (when @closed?
                     (throw (ex-info "MCP server not running" {:type :mcp/closed :server name})))
                   (let
                     [id
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
                 (try (.close in) (catch Throwable _ nil))
                 (try (.destroy proc) (catch Throwable _ nil))
                 (try (when (.isAlive proc)
                        (when-not (.waitFor proc 2 java.util.concurrent.TimeUnit/SECONDS)
                          (.destroyForcibly proc)))
                      (catch Throwable _ nil)))
     :alive-fn (fn []
                 (and (not @closed?) (.isAlive proc)))}))

;; ===========================================================================
;; Streamable-HTTP transport — POST → JSON | SSE; DELETE on close;
;; optional GET listen loop for server-pushed notifications.
;; ===========================================================================

;; Lazy: a built HttpClient owns selector threads, so creating one at namespace
;; load makes it land in a GraalVM native-image build heap (illegal). A delay
;; defers construction to first use — at runtime, native or JVM alike.
(defonce ^:private http-client
  (delay (-> (HttpClient/newBuilder)
             (.connectTimeout (Duration/ofMillis (long default-connect-timeout-ms)))
             (.build))))

(defn- sse-data-objects
  "Extract every `data:` payload from an SSE body and parse each as JSON,
   dropping unparseable frames. Returns a vec of JSON-RPC maps."
  [^String body]
  (->> (str/split-lines body)
       (keep (fn [^String l]
               (when (str/starts-with? l "data:") (str/trim (subs l 5)))))
       (keep (fn [d]
               (try (json-> d) (catch Throwable _ nil))))
       vec))

(defn- apply-headers
  ^java.net.http.HttpRequest$Builder [^java.net.http.HttpRequest$Builder b headers]
  (reduce-kv (fn [^java.net.http.HttpRequest$Builder bb k v]
               (.header bb (clj-name k) (str v)))
             b
             (or headers {})))

(defn- http-listen-loop!
  "Best-effort SSE listen channel: GET the MCP endpoint with `Accept:
   text/event-stream`, parse pushed JSON-RPC notifications and dispatch them via
   `on-notify` `(fn [msg])`. Reconnects with backoff until `closed?`. Silent when
   the server doesn't support the GET listen channel (HTTP 405/404)."
  [server-name url headers session-atom bearer-fn on-notify closed?]
  (letfn
    [(build-request []
       (let
         [b
          (-> (HttpRequest/newBuilder (URI/create url))
              (.header "Accept" "text/event-stream")
              (.timeout (Duration/ofHours 1))
              (.GET))

          b
          (apply-headers b headers)

          b
          (if-let [s @session-atom]
            (.header b "Mcp-Session-Id" s)
            b)

          b
          (if-let [t (when bearer-fn (try (bearer-fn) (catch Throwable _ nil)))]
            (.header b "Authorization" (str "Bearer " t))
            b)]

         (.build b)))]
    (doto (Thread. ^Runnable
                   (fn []
                     (let [backoff (atom 1000)]
                       (while (not @closed?)
                         (try (let
                                [resp (.send ^HttpClient @http-client
                                             (build-request)
                                             (HttpResponse$BodyHandlers/ofString))
                                 status (.statusCode resp)]

                                (cond (or (= 404 status) (= 405 status))
                                      ;; Server doesn't support the listen channel — quit quietly.
                                      (reset! closed? :no-listen)
                                      (>= status 400) (do (tel/log! {:level :debug
                                                                     :id ::http-listen-status
                                                                     :data {:server server-name
                                                                            :status status}}
                                                                    "MCP HTTP listen non-2xx")
                                                          (Thread/sleep (long @backoff))
                                                          (swap! backoff #(min 30000 (* 2 %))))
                                      :else (do (reset! backoff 1000)
                                                (doseq [msg (sse-data-objects (.body resp))]
                                                  (when (and (map? msg) (get msg "method"))
                                                    (try (on-notify msg)
                                                         (catch Throwable _ nil)))))))
                              (catch Throwable _
                                (when-not @closed?
                                  (Thread/sleep (long @backoff))
                                  (swap! backoff #(min 30000 (* 2 %)))))))))
                   (str "mcp-http-listen-" server-name))
      (.setDaemon true)
      (.start))))

(defn- start-http!
  "Streamable-HTTP transport against `url` with optional static `headers` and
   an optional `:bearer-fn` OAuth token provider."
  [name {:keys [url headers bearer-fn listen?]}]
  (let
    [session
     (atom nil)

     www-auth
     (atom nil)

     closed?
     (atom false)

     post!
     (fn [body timeout-ms]
       (let
         [tok
          (when bearer-fn (try (bearer-fn) (catch Throwable _ nil)))

          build-req
          (fn [^String bearer]
            (let
              [b
               (-> (HttpRequest/newBuilder (URI/create url))
                   (.timeout (Duration/ofMillis (long timeout-ms)))
                   (.header "Content-Type" "application/json")
                   (.header "Accept" "application/json, text/event-stream")
                   (.header "MCP-Protocol-Version" protocol-version)
                   (.POST (HttpRequest$BodyPublishers/ofString body)))

               b
               (apply-headers b headers)

               b
               (if-let [s @session]
                 (.header b "Mcp-Session-Id" s)
                 b)

               b
               (if bearer (.header b "Authorization" (str "Bearer " bearer)) b)]

              (.build b)))

          send1
          (fn [^String bearer]
            (let
              [resp
               (.send ^HttpClient @http-client
                      (build-req bearer)
                      (HttpResponse$BodyHandlers/ofString))

               status
               (.statusCode resp)]

              (when-let
                [sid (-> (.headers resp)
                         (.firstValue "mcp-session-id")
                         (.orElse nil))]
                (reset! session sid))
              (when-let
                [wa (-> (.headers resp)
                        (.firstValue "www-authenticate")
                        (.orElse nil))]
                (reset! www-auth wa))
              {:status status :body (.body resp) :bearer bearer}))

          first-resp
          (send1 tok)]

         (if (and (= 401 (:status first-resp)) bearer-fn)
           (let [fresh (try (bearer-fn tok) (catch Throwable _ nil))]
             (if (and fresh (not= fresh tok)) (send1 fresh) first-resp))
           first-resp)))

     parse-reply
     (fn [name method status body req-id]
       (when (>= (long status) 400)
         (throw (ex-info (str "MCP " name " HTTP " status " on " method)
                         {:type :mcp/http-error :server name :status status :body body})))
       (let
         [objs
          (if (str/includes? (str body) "data:")
            (sse-data-objects body)
            (try [(json-> body)] (catch Throwable _ [])))

          msg
          (or (some #(when (= (get % "id") req-id) %) objs)
              (first (filter #(contains? % "result") objs))
              (first objs))]

         (cond (nil? msg) (throw (ex-info (str "MCP " name " empty reply on " method)
                                          {:type :mcp/protocol :server name}))
               (get msg "error") (throw (rpc-error->ex name method (get msg "error")))
               :else (get msg "result"))))]

    (cond->
      {:request-fn (fn [method params timeout-ms]
                     (let
                       [req-id
                        (str (System/nanoTime))

                        body
                        (->json (cond-> {"jsonrpc" "2.0" "id" req-id "method" method}
                                  (some? params)
                                  (assoc "params" params)))

                        {:keys [status body] :as r}
                        (post! body timeout-ms)]

                       (parse-reply name method status body req-id)))
       :notify-fn (fn [method params]
                    (try (post! (->json (cond-> {"jsonrpc" "2.0" "method" method}
                                          (some? params)
                                          (assoc "params" params)))
                                10000)
                         (catch Throwable _ nil)))
       :close-fn
       (fn []
         (reset! closed? true)
         (when-let [sid @session]
           (try
             (let
               [b (-> (HttpRequest/newBuilder (URI/create url))
                      (.timeout (Duration/ofSeconds 5))
                      (.header "Mcp-Session-Id" sid)
                      (.header "MCP-Protocol-Version" protocol-version)
                      (.DELETE))
                b (apply-headers b headers)
                b (if-let [t (when bearer-fn (try (bearer-fn) (catch Throwable _ nil)))]
                    (.header b "Authorization" (str "Bearer " t))
                    b)]

               (.send ^HttpClient @http-client (.build b) (HttpResponse$BodyHandlers/discarding)))
             (catch Throwable _ nil)))
         (reset! session nil))
       :alive-fn (fn []
                   (not @closed?))
       :www-auth-atom www-auth
       :session-atom session}
      listen?
      (assoc :listen-start-fn
        (fn [on-notify]
          (http-listen-loop! name url headers session bearer-fn
                             (or on-notify (fn [_] nil))
                             closed?))))))

;; ===========================================================================
;; Public client surface
;; ===========================================================================

(defn- transport-of [{:keys [transport url]}] (or transport (if url :http :stdio)))

(defn connect
  "Connect to MCP server `name` per its `spec` and run the `initialize`
   handshake. `spec` supports:
     stdio → `{:transport :stdio :command :args :env :cwd
                :timeout-ms}`
     http  → `{:transport :http  :url :headers :bearer-fn
                :timeout-ms :listen? :on-notification}`
   Returns an opaque `conn` map (or throws)."
  [name spec]
  (let
    [transport
     (transport-of spec)

     t
     (case transport
       :stdio
       (start-stdio! name spec)

       :http
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
             :connected-at (now-ms)
             :tools (atom nil)
             :timeout-ms timeout-ms})

     init
     ((:request-fn conn)
       "initialize"
       {"protocolVersion" protocol-version
        "capabilities" {}
        "clientInfo" {"name" "vis" "version" "0.1.0"}}
       timeout-ms)]

    ;; Per spec, acknowledge before issuing further requests.
    ((:notify-fn conn) "notifications/initialized" nil)
    (let [conn (assoc conn
                 :server-info (get init "serverInfo")
                 :server-capabilities (get init "capabilities")
                 :protocol-version (get init "protocolVersion"))]
      ;; Wire the optional HTTP listen channel: server-pushed
      ;; `notifications/tools/list_changed` invalidates the tools cache so a
      ;; repeat `list-tools` re-fetches; any caller-supplied `:on-notification`
      ;; still fires (called AFTER the invalidator).
      (when-let [start (:listen-start-fn t)]
        (let [user-cb (:on-notification spec)]
          (start (fn [msg]
                   (when (= "notifications/tools/list_changed" (get msg "method"))
                     (reset! (:tools conn) nil))
                   (when user-cb (try (user-cb msg) (catch Throwable _ nil)))))))
      conn)))

(defn list-tools
  "`tools/list` → vector of tool maps `{\"name\" \"description\" \"inputSchema\"}`.
   Cached on the conn after the first call; invalidated by
   `notifications/tools/list_changed` from the listen channel."
  [conn]
  (or @(:tools conn)
      (let
        [result
         ((:request-fn conn) "tools/list" {} (or (:timeout-ms conn) default-timeout-ms))

         tools
         (vec (get result "tools"))]

        (reset! (:tools conn) tools)
        tools)))

(defn call-tool
  "`tools/call` `tool-name` with `arguments` (a map). Returns the result map
   (`{\"content\" [...] \"isError\" bool}`)."
  ([conn tool-name arguments]
   (call-tool conn tool-name arguments (or (:timeout-ms conn) default-timeout-ms)))
  ([conn tool-name arguments timeout-ms]
   ((:request-fn conn) "tools/call" {"name" tool-name "arguments" (or arguments {})} timeout-ms)))

(defn invalidate-tools!
  "Clear the cached tools list so the next `list-tools` re-fetches — called by
   the HTTP listen loop on `notifications/tools/list_changed`."
  [conn]
  (reset! (:tools conn) nil))

(defn alive? [conn] (boolean (try ((:alive-fn conn)) (catch Throwable _ false))))

(defn close [conn] (try ((:close-fn conn)) (catch Throwable _ nil)) (reset! (:tools conn) nil) nil)
