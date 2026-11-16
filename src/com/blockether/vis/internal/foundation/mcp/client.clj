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

     :sse    — the HTTP+SSE transport of protocol 2024-11-05, for a server that
               has no Streamable HTTP: one GET stream carries every message of
               the server, and its first event names the endpoint for the POST
               of each client message.

   On every transport a `ping` of the server gets its answer, any other server
   request gets `Method not found`, and `notifications/tools/list_changed`
   invalidates the tools cache. With `:elicitation?` the client declares form
   elicitation: an `elicitation/create` of the server goes to the call that
   waits, and the `:on-elicitation` fn of that call answers it.

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
  (:import (charred JSONReader$ObjReader)
           (java.io BufferedReader)
           (java.net URI)
           (java.util ArrayList)
           (java.util.concurrent ConcurrentHashMap LinkedBlockingQueue TimeUnit)))

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

(def ^:private read-ordered
  "JSON reader that keeps the key order of each object. The properties of a
   schema are a form, and a person reads it in the order that the server wrote."
  (delay (json/parse-json-fn
           {:obj-iface (reify
                         JSONReader$ObjReader
                           (newObj [_] (ArrayList.))
                           (onKV [_ obj k v] (doto ^ArrayList obj (.add k) (.add v)))
                           (finalizeObj [_ obj]
                             (let [pairs (.toArray ^ArrayList obj)]
                               ;; A large object is a table, not a form: keep its lookups fast.
                               (if (> (alength pairs) 128)
                                 (clojure.lang.PersistentHashMap/create pairs)
                                 (clojure.lang.PersistentArrayMap/createAsIfByAssoc pairs)))))})))

(defn- json-> [^String s] (@read-ordered s))

(defn- clj-name [k] (if (keyword? k) (name k) (str k)))

(defn- rpc-error->ex
  [name method err]
  (ex-info (str "MCP " name " " method " failed: " (get err "message" "unknown error"))
           {:type :mcp/rpc-error
            :server name
            :method method
            :code (get err "code")
            :data (get err "data")}))

;; Messages that the server sends on its own, on every transport.

(defn- server-request-reply
  "The reply to a request that the SERVER sent as `msg`, or nil when `msg` is not a
   request. Only `ping` has a result here; an elicitation goes to its caller."
  [msg]
  (when (and (map? msg) (string? (get msg "method")) (some? (get msg "id")))
    (if (= "ping" (get msg "method"))
      {"jsonrpc" "2.0" "id" (get msg "id") "result" {}}
      {"jsonrpc" "2.0"
       "id" (get msg "id")
       "error" {"code" -32601 "message" (str "Method not found: " (get msg "method"))}})))

(defn- elicitation-request?
  [msg]
  (and (map? msg) (= "elicitation/create" (get msg "method")) (some? (get msg "id"))))

(defn- cancelled
  "The reply to the elicitation `msg` when nobody can answer it."
  [msg]
  {"jsonrpc" "2.0" "id" (get msg "id") "result" {"action" "cancel"}})

(defn- with-defaults
  "`content` plus the default of each property of `schema` that `content` leaves
   out (SEP-1034)."
  [schema content]
  (let [properties (get schema "properties")]
    (reduce-kv
      (fn [content field property]
        (if (and (map? property) (contains? property "default") (not (contains? content field)))
          (assoc content field (get property "default"))
          content))
      (if (map? content) content {})
      (if (map? properties) properties {}))))

(defn- elicitation-reply
  "The reply to the elicitation `msg`. `ask` gets the params of the request and
   returns `{\"action\" \"accept\" \"content\" {...}}` or the action `decline`; any
   other answer is a cancel. Vis declares the form mode only."
  [msg ask]
  (let [params
        (get msg "params")

        mode
        (get params "mode")]

    (if (contains? #{nil "form"} mode)
      (let [answer (ask params)]
        {"jsonrpc" "2.0"
         "id" (get msg "id")
         "result" (case (get answer "action")
                    "accept"
                    {"action" "accept"
                     "content" (with-defaults (get params "requestedSchema")
                                              (get answer "content"))}

                    "decline"
                    {"action" "decline"}

                    {"action" "cancel"})})
      {"jsonrpc" "2.0"
       "id" (get msg "id")
       "error" {"code" -32602 "message" (str "Unsupported elicitation mode: " mode)}})))

(defn- await-reply!
  "Take the reply of one call from `inbox`, or `::timeout` when nothing came for
   `timeout-ms`. An elicitation of the server runs HERE, on the thread of the
   caller, through `ask`: the form belongs to the turn of that thread, and the
   time that the person takes is no part of the deadline."
  [^LinkedBlockingQueue inbox timeout-ms ask]
  (loop []

    (let [[kind value send!] (.poll inbox (long timeout-ms) TimeUnit/MILLISECONDS)]
      (case kind
        :reply
        value

        :elicit
        (do (send! (try (elicitation-reply value ask)
                        (catch Throwable t
                          ;; A cancelled turn ends the call; any other failure is no answer.
                          (if (cancellation/preserve-interrupt! t) (throw t) (cancelled value)))))
            (recur))

        ::timeout))))

(defn- without-caller [callers inbox] (vec (remove #(identical? inbox %) callers)))

(defn- handle-incoming!
  "Handle a message of the server that is not a reply. An elicitation goes to the
   newest call that waits in the atom `callers`, and is a cancel when none waits;
   `callers` is nil for a client that declared no elicitation. Any other request
   gets its answer through `send!`, and a notification goes to the fn in the atom
   `incoming`. Never throws."
  [msg send! incoming callers]
  (try (cond (and callers (elicitation-request? msg)) (if-let [^LinkedBlockingQueue inbox
                                                               (peek @callers)]
                                                        (.put inbox [:elicit msg send!])
                                                        (send! (cancelled msg)))
             (server-request-reply msg) (send! (server-request-reply msg))
             (and (map? msg) (string? (get msg "method"))) (when-let [on-notification @incoming]
                                                             (on-notification msg)))
       (catch Throwable _ nil)))

(defn- duplex
  "The request side of a transport where every message of the server comes on ONE
   channel: stdio and legacy SSE. `write!` sends one message and `closed?` is the
   atom of the transport. The reader of the transport gives each message to
   `:receive!` and calls `:fail-pending!` when the channel ends."
  [name write! closed? elicitation?]
  (let [pending
        (ConcurrentHashMap.)

        next-id
        (atom 0)

        ;; `connect` puts the notification handler here.
        incoming
        (atom nil)

        callers
        (when elicitation? (atom []))]

    {:incoming incoming
     :receive! (fn [msg]
                 ;; A message with a method is no reply, whatever its id.
                 (if (and (map? msg) (contains? msg "method"))
                   (handle-incoming! msg write! incoming callers)
                   (when-let [id (get msg "id")]
                     (when-let [^LinkedBlockingQueue inbox (.remove pending id)]
                       (.put inbox [:reply msg])))))
     :fail-pending! (fn []
                      (doseq [k (enumeration-seq (.keys pending))]
                        (when-let [^LinkedBlockingQueue inbox (.remove pending k)]
                          (.put inbox [:reply {"error" {"message" "server stream closed"}}]))))
     :request-fn
     (fn request ([method params timeout-ms] (request method params timeout-ms nil))
       ([method params timeout-ms ask] (when @closed? (throw (ex-info "MCP server not running"
                                                               {:type :mcp/closed :server name})))
        (let [id
              (str (swap! next-id inc))

              inbox
              (LinkedBlockingQueue.)

              ask
              (when callers ask)]

          (.put pending id inbox)
          (when ask (swap! callers conj inbox))
          (try (write! (cond-> {"jsonrpc" "2.0" "id" id "method" method}
                         (some? params)
                         (assoc "params" params)))
               (let [msg (await-reply! inbox timeout-ms ask)]
                 (cond (= msg ::timeout) (throw (ex-info
                                                  (str "MCP " name " " method " timed out")
                                                  {:type :mcp/timeout :server name :method method}))
                       (get msg "error") (throw (rpc-error->ex name method (get msg "error")))
                       :else (get msg "result")))
               (finally (.remove pending id) (when ask (swap! callers without-caller inbox)))))))
     :notify-fn (fn [method params]
                  (when-not @closed?
                    (write! (cond-> {"jsonrpc" "2.0" "method" method}
                              (some? params)
                              (assoc "params" params)))))}))

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
  [name ^Process proc elicitation?]
  (let [out
        (BufferedReader. (io/reader (.getInputStream proc)))

        in
        (.getOutputStream proc)

        closed?
        (atom false)

        write!
        (fn [m]
          (locking in (.write in (.getBytes (str (->json m) "\n") "UTF-8")) (.flush in)))

        {:keys [receive! fail-pending!] :as core}
        (duplex name write! closed? elicitation?)

        read-loop
        (fn []
          (try (loop []

                 (when-let [line (.readLine out)]
                   (when-not (str/blank? line)
                     (try (receive! (json-> line))
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
                        (fail-pending!))))]

    (start-stderr-drain! name (.getErrorStream proc))
    (doto (Thread. ^Runnable read-loop (str "mcp-stdio-" name)) (.setDaemon true) (.start))
    (assoc (select-keys core [:incoming :request-fn :notify-fn])
      :pid (try (.pid proc) (catch Throwable _ nil))
      :close-fn (fn []
                  (reset! closed? true)
                  (stop-stdio-process! proc))
      :alive-fn (fn []
                  (and (not @closed?) (.isAlive proc))))))

(defn- start-stdio!
  "Spawn `command`+`args` (with extra `env`), wire newline-delimited JSON-RPC.
   Returns `{:request-fn :notify-fn :close-fn :alive-fn :pid}`."
  [name {:keys [command args env cwd elicitation?]}]
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

    (try (stdio-transport name proc elicitation?)
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
  "Read the next event of an SSE stream as `{:id :event :retry :data}`, or nil at the end
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

                       "event"
                       (assoc event :event value)

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
   closes, on an interrupt, or when the server refuses the channel (HTTP 4xx; a
   401, 408 or 429 can heal). `open!` and `read-stream` are those of the
   transport. The promise `ready` gets a value when the first GET has its answer."
  [server-name open! read-stream closed? ready]
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

         (deliver ready true)
         (cond (and (<= 400 status 499) (not (contains? #{401 408 429} status)))
               (do (close-body! resp) ::stop)
               (or (>= status 400) (not (event-stream? resp)))
               (do (close-body! resp)
                   (tel/log! {:level :debug
                              :id ::http-listen-status
                              :data {:server server-name :status status}}
                             "MCP HTTP listen without a stream")
                   ::retry)
               :else (read-stream resp cursor (constantly false) nil))))]
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
                                       (deliver ready true)
                                       ;; An interrupt is the ONE failure that ENDS the loop.
                                       (if (cancellation/preserve-interrupt! t) ::stop ::retry)))]
                    (cond (= ::stop outcome) nil
                          (= ::retry outcome) (when (sleep! backoff)
                                                (recur cursor (min 30000 (* 2 (long backoff)))))
                          :else (when (sleep! (or (:retry outcome) 1000)) (recur outcome 1000)))))))
            (str "mcp-http-listen-" server-name))
      (.setDaemon true)
      (.start))))

(defn- http-opener
  "The request fn of an HTTP transport for `url`. `(open! request)` sends `request`
   (`:method`, `:headers`, `:body`, `:timeout`, and `:uri` for another URL than
   `url`) with the static `headers`, the protocol version, the session id and the
   Bearer token, and returns the response with an open body. The caller closes
   the body. One 401 refreshes the token and sends again. The atoms `session`,
   `protocol-version*` and `www-auth` are those of the transport."
  [url headers bearer-fn session protocol-version* www-auth]
  (let [send1 (fn [request bearer]
                (let [resp (http/request
                             (assoc request
                               :uri (or (:uri request) url)
                               :client @mcp-http/client
                               :headers (cond-> (merge (apply-headers headers)
                                                       {"MCP-Protocol-Version" @protocol-version*}
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
                  resp))]
    (fn [request]
      (let [tok (when bearer-fn (try (bearer-fn) (catch Throwable _ nil)))
            first-resp (send1 request tok)]

        (if (and (= 401 (:status first-resp)) bearer-fn)
          ;; `bearer-fn` refuses only with the typed `:mcp/oauth-required`. It must
          ;; escape: as a plain HTTP 401 it skipped the sign-in backoff, and every
          ;; turn connected again (#458).
          (let [fresh (try (bearer-fn tok) (catch Throwable t (close-body! first-resp) (throw t)))]
            (if (and fresh (not= fresh tok))
              (do (close-body! first-resp) (send1 request fresh))
              first-resp))
          first-resp)))))

(defn- http-error
  "The typed failure for the response `resp` to `what` of server `name`."
  [name resp what]
  (ex-info (str "MCP " name " HTTP " (:status resp) " on " what)
           {:type :mcp/http-error :server name :status (:status resp) :body (body-text resp)}))

(defn- start-http!
  "Streamable-HTTP transport against `url` with optional static `headers` and
   an optional `:bearer-fn` OAuth token provider. `:www-auth-atom`, when given, is
   the caller's atom to keep pointed at the latest `WWW-Authenticate` challenge."
  [name {:keys [url headers bearer-fn www-auth-atom listen? elicitation?]}]
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

        ;; The calls that can answer an elicitation, newest last.
        callers
        (when elicitation? (atom []))

        listen-thread
        (atom nil)

        ;; Has a value when the listen channel got its first answer.
        listening
        (promise)

        open!
        (http-opener url headers bearer-fn session protocol-version* www-auth)

        post!
        (fn [body timeout-ms]
          (open! {:method :post
                  :headers {"Content-Type" "application/json"
                            "Accept" "application/json, text/event-stream"}
                  :body body
                  :timeout timeout-ms}))

        reply!
        (fn [msg]
          (close-body! (post! (->json msg) 10000)))

        ;; Read the events of `resp` until a message for which `reply?` is true, or
        ;; until the stream ends. Returns `cursor` with the last event `:id` and
        ;; `:retry` delay, plus `:reply` when one came. An elicitation on the stream
        ;; of a call goes to the `inbox` of that call.
        read-stream
        (fn [resp cursor reply? ^LinkedBlockingQueue inbox]
          (with-open [rdr (BufferedReader. (io/reader (:body resp) :encoding "UTF-8"))]
            (loop [cursor cursor]
              (if-let [event (read-sse-event rdr)]
                (let [cursor (merge cursor (select-keys event [:id :retry]))
                      msg (when-not (str/blank? (:data event))
                            (try (json-> (:data event)) (catch Throwable _ nil)))]

                  (cond (reply? msg) (assoc cursor :reply msg)
                        (and inbox (elicitation-request? msg)) (do (.put inbox [:elicit msg reply!])
                                                                   (recur cursor))
                        :else (do (when msg (handle-incoming! msg reply! incoming callers))
                                  (recur cursor))))
                cursor))))

        ;; The JSON-RPC reply to the request that `resp` answers, or nil. A stream
        ;; that ends before the reply continues on a GET with the id of its last
        ;; event, after the delay that the server gave.
        reply-of
        (fn [resp method timeout-ms inbox]
          (cond (>= (long (:status resp)) 400) (throw (http-error name resp method))
                (event-stream? resp)
                (loop [resp
                       resp

                       cursor
                       {}]

                  (let [cursor (read-stream resp cursor response? inbox)]
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
                        (if (sequential? value) (first (filter response? value)) value))))]

    (cond-> {:request-fn
             (fn request ([method params timeout-ms] (request method params timeout-ms nil))
               ([method params timeout-ms ask] (let [ask
                                                     (when callers ask)

                                                     inbox
                                                     (LinkedBlockingQueue.)]

                                                 (when ask
                                                   (swap! callers conj inbox)
                                                   ;; A server can send its elicitation on the listen channel only.
                                                   (when @listen-thread (deref listening 2000 nil)))
                                                 (try
                                                   (let [body
                                                         (->json (cond-> {"jsonrpc" "2.0"
                                                                          "id" (str
                                                                                 (System/nanoTime))
                                                                          "method" method}
                                                                   (some? params)
                                                                   (assoc "params" params)))

                                                         ;; A stream read has no timeout of its own, so the exchange
                                                         ;; runs apart and this thread keeps the deadline.
                                                         exchange
                                                         (future (.put inbox
                                                                       [:reply
                                                                        (try {:msg
                                                                              (reply-of
                                                                                (post! body
                                                                                       timeout-ms)
                                                                                method
                                                                                timeout-ms
                                                                                (when ask inbox))}
                                                                             (catch Throwable t
                                                                               {:thrown t}))]))

                                                         outcome
                                                         (try (await-reply! inbox timeout-ms ask)
                                                              (catch Throwable t
                                                                (future-cancel exchange)
                                                                (throw t)))

                                                         msg
                                                         (:msg outcome)]

                                                     (cond (= ::timeout outcome)
                                                           (do (future-cancel exchange)
                                                               (throw (ex-info (str "MCP "
                                                                                    name
                                                                                    " "
                                                                                    method
                                                                                    " timed out")
                                                                               {:type :mcp/timeout
                                                                                :server name
                                                                                :method method})))
                                                           (:thrown outcome) (throw (:thrown
                                                                                      outcome))
                                                           (not (map? msg))
                                                           (throw (ex-info (str "MCP " name
                                                                                " empty reply on "
                                                                                method)
                                                                           {:type :mcp/protocol
                                                                            :server name}))
                                                           (get msg "error")
                                                           (throw (rpc-error->ex name
                                                                                 method
                                                                                 (get msg "error")))
                                                           :else (get msg "result")))
                                                   (catch Throwable t
                                                     ;; A refusal of the peer and a slow reply are no lost transport.
                                                     (when-not (or (contains? #{:mcp/rpc-error
                                                                                :mcp/timeout}
                                                                              (:type (ex-data t)))
                                                                   (instance? InterruptedException
                                                                              t))
                                                       (reset! closed? true))
                                                     (throw t))
                                                   (finally (when ask
                                                              (swap! callers without-caller
                                                                inbox)))))))
             :notify-fn (fn [method params]
                          (try (reply! (cond-> {"jsonrpc" "2.0" "method" method}
                                         (some? params)
                                         (assoc "params" params)))
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
          (reset! listen-thread (http-listen-loop! name open! read-stream closed? listening)))))))

(defn- start-sse!
  "The HTTP+SSE transport of protocol 2024-11-05 against `url`. A GET opens the one
   stream of the server. Its first event, `endpoint`, names the URL for the POST
   of each client message, on the origin of `url`; every reply comes on the
   stream. The transport is dead when the stream ends."
  [name {:keys [url headers bearer-fn www-auth-atom elicitation? timeout-ms]}]
  (let [protocol-version*
        (atom protocol-version)

        www-auth
        (or www-auth-atom (atom nil))

        closed?
        (atom false)

        open!
        (http-opener url headers bearer-fn (atom nil) protocol-version* www-auth)

        timeout-ms
        (long (or timeout-ms default-timeout-ms))

        stream
        (open! {:method :get :headers {"Accept" "text/event-stream"} :timeout timeout-ms})

        _
        (when-not (and (< (long (:status stream)) 400) (event-stream? stream))
          (throw (http-error name stream "the SSE stream")))

        endpoint
        (promise)

        write!
        (fn [msg]
          (let [resp (open! {:uri @endpoint
                             :method :post
                             :headers {"Content-Type" "application/json"}
                             :body (->json msg)
                             :timeout timeout-ms})]
            (if (>= (long (:status resp)) 400)
              (throw (http-error name resp "a message"))
              (close-body! resp))))

        {:keys [receive! fail-pending!] :as core}
        (duplex name write! closed? elicitation?)

        origin
        (fn [^URI uri]
          [(.getScheme uri) (.getHost uri) (.getPort uri)])

        read-loop
        (fn []
          (try (with-open [rdr (BufferedReader. (io/reader (:body stream) :encoding "UTF-8"))]
                 (loop []

                   (when-let [event (read-sse-event rdr)]
                     (if (= "endpoint" (:event event))
                       (let [target (.resolve (URI. url) (str/trim (str (:data event))))]
                         ;; Another origin would get the token and every message.
                         (when (= (origin (URI. url)) (origin target))
                           (deliver endpoint (str target))))
                       (when-let [msg (try (json-> (:data event)) (catch Throwable _ nil))]
                         (receive! msg)))
                     (recur))))
               (catch Throwable _ nil)
               (finally (reset! closed? true) (deliver endpoint nil) (fail-pending!))))]

    (doto (Thread. ^Runnable read-loop (str "mcp-sse-" name)) (.setDaemon true) (.start))
    (when-not (deref endpoint timeout-ms nil)
      (reset! closed? true)
      (close-body! stream)
      (throw (ex-info (str "MCP " name " named no message endpoint on its SSE stream")
                      {:type :mcp/protocol :server name})))
    (assoc (select-keys core [:incoming :request-fn :notify-fn])
      :close-fn (fn []
                  (reset! closed? true)
                  (close-body! stream))
      :alive-fn (fn []
                  (not @closed?))
      :www-auth-atom www-auth
      :protocol-version-atom protocol-version*)))

;; Public client surface

(defn- transport-of
  [{:keys [transport]}]
  (case (some-> transport
                name)
    ("streamable_http" "streamable-http")
    :streamable-http

    "stdio"
    :stdio

    "sse"
    :sse

    (throw (ex-info "Unsupported MCP transport" {:transport transport}))))

(defn- connect-as
  "Connect to MCP server `name` on `transport` and run the `initialize` handshake.
   A failed handshake always closes the transport it already opened, so failed
   stdio starts cannot orphan processes."
  [name spec transport]
  (let [t
        (case transport
          :stdio
          (start-stdio! name spec)

          :streamable-http
          (start-http! name spec)

          :sse
          (start-sse! name spec)

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
                  "capabilities" (cond-> {}
                                   (:elicitation? spec)
                                   (assoc "elicitation" {"form" {}}))
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

(defn- older-server?
  "Whether `failure` of a Streamable HTTP `initialize` is the answer of a server
   that can have only the older HTTP+SSE transport: it refused the POST itself."
  [failure]
  (let [{:keys [type status]} (ex-data failure)]
    (and (= :mcp/http-error type) (contains? #{400 404 405} status))))

(defn connect
  "Connect to MCP server `name` per its `spec` and run the `initialize`
   handshake. `spec` supports:
     stdio           → `{:transport :stdio :command :args :env :cwd :timeout-ms}`
     streamable-http → `{:transport :streamable-http :url :headers :bearer-fn
                         :timeout-ms :listen? :on-notification}`
     sse             → `{:transport :sse :url :headers :bearer-fn :timeout-ms}`
   With `:elicitation? true` the client declares form elicitation; see `call-tool`.
   When a Streamable HTTP server refuses the POST of `initialize`, the client tries
   the HTTP+SSE transport on the same URL, as the specification asks. When that
   fails too, the first failure is the one that it reports.
   Returns an opaque `conn` map (or throws)."
  [name spec]
  (let [transport (transport-of spec)]
    (try (connect-as name spec transport)
         (catch clojure.lang.ExceptionInfo failure
           (when-not (and (= :streamable-http transport) (older-server? failure)) (throw failure))
           (try (connect-as name spec :sse)
                (catch Throwable t
                  (when (cancellation/preserve-interrupt! t) (throw t))
                  (throw failure)))))))

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
   (`{\"content\" [...] \"isError\" bool}`).

   `:on-elicitation` answers a form elicitation that the server sends while the
   call waits. It gets the params of `elicitation/create`, runs on the calling
   thread, and returns `{\"action\" \"accept\" \"content\" {...}}` or the action
   `decline`; anything else is a cancel. The defaults of the schema fill the
   fields that an accepted content leaves out. A server that names no call sends
   its elicitation to the newest call that waits on the connection."
  ([conn tool-name arguments]
   (call-tool conn tool-name arguments (or (:timeout-ms conn) default-timeout-ms)))
  ([conn tool-name arguments timeout-ms] (call-tool conn tool-name arguments timeout-ms nil))
  ([conn tool-name arguments timeout-ms {:keys [on-elicitation]}]
   ((:request-fn conn)
     "tools/call"
     {"name" tool-name "arguments" (or arguments {})}
     timeout-ms
     on-elicitation)))

(defn alive? [conn] (boolean (try ((:alive-fn conn)) (catch Throwable _ false))))

(defn close [conn] (try ((:close-fn conn)) (catch Throwable _ nil)) (reset! (:tools conn) nil) nil)
