(ns com.blockether.vis.ext.foundation-mcp.client
  "Minimal Model Context Protocol (MCP) client. Speaks JSON-RPC 2.0 over two
   transports:

     :stdio  — spawn the server process and frame newline-delimited JSON-RPC on
               its stdin/stdout (the dominant local-server pattern).
     :http   — Streamable HTTP: POST each JSON-RPC message to one endpoint; the
               reply is either `application/json` (one response) or
               `text/event-stream` (SSE) — both handled. The `Mcp-Session-Id`
               handed back by `initialize` rides on every later request.

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
            (finally
              ;; stream ended — fail any in-flight requests so callers
              ;; don't hang forever.
              (reset! closed? true)
              (doseq [k (enumeration-seq (.keys pending))]
                (when-let [p (.remove pending k)]
                  (deliver p {"error" {"message" "server stream closed"}}))))))]

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
;; Streamable-HTTP transport — sync request/response (POST → JSON or SSE).
;; ===========================================================================

;; Lazy: a built HttpClient owns selector threads, so creating one at namespace
;; load makes it land in a GraalVM native-image build heap (illegal). A delay
;; defers construction to first use — at runtime, native or JVM alike.
(defonce ^:private http-client
  (delay (-> (HttpClient/newBuilder)
             (.connectTimeout (Duration/ofSeconds 15))
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

(defn- start-http!
  "Streamable-HTTP transport against `url` with optional static `headers`.
   `request-fn` POSTs and returns the matching JSON-RPC result; `notify-fn`
   POSTs and ignores the reply. The `Mcp-Session-Id` from `initialize` is
   captured and replayed."
  [name {:keys [url headers]}]
  (let
    [session
     (atom nil)

     post!
     (fn [body timeout-ms]
       (let
         [b
          (-> (HttpRequest/newBuilder (URI/create url))
              (.timeout (Duration/ofMillis (long timeout-ms)))
              (.header "Content-Type" "application/json")
              (.header "Accept" "application/json, text/event-stream")
              (.POST (HttpRequest$BodyPublishers/ofString body)))

          ^java.net.http.HttpRequest$Builder b
          (reduce-kv (fn [^java.net.http.HttpRequest$Builder bb k v]
                       (.header bb (clj-name k) (str v)))
                     b
                     (or headers {}))

          ^java.net.http.HttpRequest$Builder b
          (if-let [s @session]
            (.header b "Mcp-Session-Id" s)
            b)

          resp
          (.send ^HttpClient @http-client (.build b) (HttpResponse$BodyHandlers/ofString))]

         (when-let
           [sid (-> (.headers resp)
                    (.firstValue "mcp-session-id")
                    (.orElse nil))]
           (reset! session sid))
         {:status (.statusCode resp) :body (.body resp)}))]

    {:request-fn (fn [method params timeout-ms]
                   (let
                     [req-id
                      (str (System/nanoTime))

                      body
                      (->json (cond-> {"jsonrpc" "2.0" "id" req-id "method" method}
                                (some? params)
                                (assoc "params" params)))

                      {:keys [status body]}
                      (post! body timeout-ms)]

                     (when (>= (long status) 400)
                       (throw (ex-info
                                (str "MCP " name " HTTP " status " on " method)
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
                             :else (get msg "result")))))
     :notify-fn (fn [method params]
                  (try (post! (->json (cond-> {"jsonrpc" "2.0" "method" method}
                                        (some? params)
                                        (assoc "params" params)))
                              10000)
                       (catch Throwable _ nil)))
     :close-fn (fn []
                 (reset! session nil))
     :alive-fn (fn []
                 true)}))

;; ===========================================================================
;; Public client surface
;; ===========================================================================

(def default-timeout-ms 30000)

(defn- transport-of [{:keys [transport url]}] (or transport (if url :http :stdio)))

(defn connect
  "Connect to MCP server `name` per its `spec` and run the `initialize`
   handshake. `spec` is `{:transport :stdio|:http ...}`:
     stdio → `{:command \"npx\" :args [...] :env {...} :cwd \"...\"}`
     http  → `{:url \"https://...\" :headers {...}}`
   Returns a `conn` map (opaque) or throws."
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

     conn
     (merge t {:name name :transport transport :spec spec :connected-at (now-ms) :tools (atom nil)})

     init
     ((:request-fn conn)
       "initialize"
       {"protocolVersion" protocol-version
        "capabilities" {}
        "clientInfo" {"name" "vis" "version" "0.1.0"}}
       default-timeout-ms)]

    ;; Per spec, acknowledge before issuing further requests.
    ((:notify-fn conn) "notifications/initialized" nil)
    (assoc conn
      :server-info (get init "serverInfo")
      :server-capabilities (get init "capabilities")
      :protocol-version (get init "protocolVersion"))))

(defn list-tools
  "`tools/list` → vector of tool maps `{\"name\" \"description\" \"inputSchema\"}`.
   Cached on the conn after the first call."
  [conn]
  (or @(:tools conn)
      (let
        [result
         ((:request-fn conn) "tools/list" {} default-timeout-ms)

         tools
         (vec (get result "tools"))]

        (reset! (:tools conn) tools)
        tools)))

(defn call-tool
  "`tools/call` `tool-name` with `arguments` (a map). Returns the result map
   (`{\"content\" [...] \"isError\" bool}`)."
  ([conn tool-name arguments] (call-tool conn tool-name arguments default-timeout-ms))
  ([conn tool-name arguments timeout-ms]
   ((:request-fn conn) "tools/call" {"name" tool-name "arguments" (or arguments {})} timeout-ms)))

(defn alive? [conn] (boolean (try ((:alive-fn conn)) (catch Throwable _ false))))

(defn close [conn] (try ((:close-fn conn)) (catch Throwable _ nil)) (reset! (:tools conn) nil) nil)
