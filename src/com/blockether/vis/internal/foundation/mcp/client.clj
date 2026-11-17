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
   waits, and the `:on-elicitation` fn of that call answers it. With `:sampling?`
   the same holds for `sampling/createMessage` and `:on-sampling`. With `:roots?`
   the client declares roots and answers `roots/list` with the `:roots` of the
   newest request.

   OAuth 2.1 (spec `2025-06-18`) is supported for HTTP transports via
   `oauth.clj`: pass `:bearer-fn` (a 0/1-arg fn yielding the current Bearer
   token, called with the just-rejected token on 401). A 401 triggers a
   single-flight refresh and one automatic retry.

   The client speaks two eras of the protocol. A server of a stateless revision
   (`2026-07-28`) answers `server/discover`: it gets no handshake and no session,
   every request carries the protocol version, the client capabilities and the
   client info in `_meta`, and a result of the type `input_required` gets its
   input and a new request. Over HTTP, the headers `Mcp-Method`, `Mcp-Name` and
   `Mcp-Param-*` name the request. Any other server gets the `initialize`
   handshake of the older revisions.

   A `conn` is a plain map of closures + state; the extension treats it
   opaquely. Lifecycle: `connect` (which finds the era of the server) →
   `list-tools` / `call-tool` / `request` → `close`."
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

(def ^:private protocol-version "The newest revision with the `initialize` handshake." "2025-11-25")

(def ^:private modern-versions
  "The stateless revisions that the client speaks, newest first."
  ["2026-07-28"])

(def ^:private client-info {"name" "vis" "version" "0.1.0"})

(def default-timeout-ms 30000)

(def ^:private stdio-hedge-ms
  "How long a stdio server can be silent on `server/discover` before it gets
   `initialize` too. A server of an older revision can ignore an unknown method,
   and a server that starts slowly answers nothing for some seconds."
  1000)

(def ^:private stdio-order-grace-ms
  "How long a `server/discover` result can come after the `initialize` result of
   the same server and still decide the era."
  100)

(def ^:private http-probe-timeout-ms 10000)

(def ^:private roots-grace-ms
  "How long a request waits for a server to read roots that changed."
  500)

(def ^:private max-input-rounds "How many times one request can come back as `input_required`." 16)

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
  "The typed failure for the JSON-RPC error `err`. `status` is the HTTP status that
   a stateless server sent with it."
  ([name method err] (rpc-error->ex name method err nil))
  ([name method err status]
   (ex-info (str "MCP " name " " method " failed: " (get err "message" "unknown error"))
            (cond-> {:type :mcp/rpc-error
                     :server name
                     :method method
                     :code (get err "code")
                     :data (get err "data")}
              status
              (assoc :status status)))))

;; Stateless revisions: request metadata, request headers and input rounds.

(defn- client-capabilities
  [spec]
  (cond-> {}
    (:elicitation? spec)
    (assoc "elicitation" {"form" {}})

    (:roots? spec)
    (assoc "roots" {"listChanged" true})

    (:sampling? spec)
    (assoc "sampling" {})))

(defn- request-meta
  "The `_meta` fields that each request in the stateless protocol `version` has."
  [version spec]
  {"io.modelcontextprotocol/protocolVersion" version
   "io.modelcontextprotocol/clientCapabilities" (client-capabilities spec)
   "io.modelcontextprotocol/clientInfo" client-info})

(defn- header-value
  "`text` as the value of an `Mcp-Name` or `Mcp-Param-*` header. Text that a header
   cannot carry as it is goes in Base64."
  [^String text]
  (if (or (re-find #"[^\x20-\x7E]" text)
          (str/starts-with? text " ")
          (str/ends-with? text " ")
          (re-matches #"(?i)=\?base64\?.*\?=" text))
    (str "=?base64?" (.encodeToString (java.util.Base64/getEncoder) (.getBytes text "UTF-8")) "?=")
    text))

(defn- request-headers
  "The headers that name the request `method` with `params` to a stateless HTTP
   server."
  [method params]
  (let [target (case method
                 ("tools/call" "prompts/get")
                 (get params "name")

                 "resources/read"
                 (get params "uri")

                 nil)]
    (cond-> {"Mcp-Method" method}
      (string? target)
      (assoc "Mcp-Name" (header-value target)))))

(defn- header-annotations
  "Each `x-mcp-header` in the input schema `schema` as `{:path :name :type
   :property?}`. `:path` has the property names from the root, and `:property?` is
   true when only `properties` keys lead to the annotation."
  [schema]
  (letfn
    [(walk [schema path property?]
       (when (map? schema)
         (concat (when (contains? schema "x-mcp-header")
                   [{:path path
                     :name (get schema "x-mcp-header")
                     :type (get schema "type")
                     :property? (and property? (boolean (seq path)))}])
                 (mapcat (fn [[k v]]
                           (cond (= "properties" k)
                                 (when (map? v)
                                   (mapcat (fn [[property child]]
                                             (walk child (conj path property) property?))
                                           v))
                                 ;; A value of the instance is no schema.
                                 (contains? #{"default" "examples" "const" "enum"} k) nil
                                 (map? v) (walk v path false)
                                 (sequential? v) (mapcat #(walk % path false) v)))
                         schema))))]
    (walk schema [] true)))

(defn- valid-header-annotations?
  "Whether each `x-mcp-header` of `tool` obeys the rules: a header token as its
   name, on a string, integer or boolean property, one time in the schema."
  [tool]
  (let [annotations (header-annotations (get tool "inputSchema"))]
    (and (every? (fn [{:keys [name type property?]}]
                   (and property?
                        (string? name)
                        (re-matches #"[!#$%&'*+\-.^_`|~0-9A-Za-z]+" name)
                        (contains? #{"string" "integer" "boolean"} type)))
                 annotations)
         (or (empty? annotations)
             (apply distinct? (map #(str/lower-case (:name %)) annotations))))))

(defn- param-headers
  "The `Mcp-Param-*` headers of a call of `tool` with `arguments`: one for each
   annotated argument that has a value."
  [tool arguments]
  (reduce (fn [headers {:keys [path name]}]
            (let [value
                  (get-in arguments path)

                  text
                  (cond (string? value) value
                        (boolean? value) (str value)
                        (integer? value) (str value))]

              (if text (assoc headers (str "Mcp-Param-" name) (header-value text)) headers)))
          {}
          (header-annotations (get tool "inputSchema"))))

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

(def ^:private input-capability
  "The requests of a server for input, each with the key of the client spec that
   declares it."
  {"elicitation/create" :elicitation? "sampling/createMessage" :sampling? "roots/list" :roots?})

(defn- declared-input
  "The method of `msg` when `msg` is a request of the server for an input that the
   client of `spec` declared, else nil."
  [spec msg]
  (when (and (map? msg) (some? (get msg "id")))
    (let [method (get msg "method")]
      (when (get spec (input-capability method)) method))))

(defn- person-input?
  "Whether `msg` asks for an input that only a call with a person can give."
  [spec msg]
  (contains? #{"elicitation/create" "sampling/createMessage"} (declared-input spec msg)))

(defn- unanswered
  "The reply to the input request `msg` when no call can answer it: a cancelled
   elicitation, or a refused sampling request."
  [msg]
  (if (= "sampling/createMessage" (get msg "method"))
    {"jsonrpc" "2.0"
     "id" (get msg "id")
     "error" {"code" -1 "message" "User rejected sampling request"}}
    {"jsonrpc" "2.0" "id" (get msg "id") "result" {"action" "cancel"}}))

(defn- with-defaults
  "`content` plus the default of each property of `schema` that `content` leaves
   out (SEP-1034). A property in `shown` gets no default: the asker showed that
   field with its default, and the person left it empty."
  [schema content shown]
  (let [properties
        (get schema "properties")

        shown
        (set shown)]

    (reduce-kv (fn [content field property]
                 (if (and (map? property)
                          (contains? property "default")
                          (not (contains? content field))
                          (not (contains? shown field)))
                   (assoc content field (get property "default"))
                   content))
               (if (map? content) content {})
               (if (map? properties) properties {}))))

(defn- elicitation-reply
  "The reply to the elicitation `msg`. `ask` gets the params of the request and
   returns `{\"action\" \"accept\" \"content\" {...}}` or the action `decline`; any
   other answer is a cancel. An asker that shows a form adds `\"fields\"`, the
   names of the fields that the person saw. Vis declares the form mode only."
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
                                              (get answer "content")
                                              (get answer "fields"))}

                    "decline"
                    {"action" "decline"}

                    {"action" "cancel"})})
      {"jsonrpc" "2.0"
       "id" (get msg "id")
       "error" {"code" -32602 "message" (str "Unsupported elicitation mode: " mode)}})))

(defn- input-reply
  "The reply to the person input `msg` from the `answers` of one call: `:ask` for
   an elicitation, as in `elicitation-reply`, and `:sample` for a sampling request.
   `:sample` gets the params of `sampling/createMessage` and returns its result, or
   nil when the person refuses the request."
  [msg {:keys [ask sample]}]
  (if (= "sampling/createMessage" (get msg "method"))
    (let [result (when sample (sample (get msg "params")))]
      (if (map? result) {"jsonrpc" "2.0" "id" (get msg "id") "result" result} (unanswered msg)))
    (if ask (elicitation-reply msg ask) (unanswered msg))))

(defn- answers-of
  "The answers that the `opts` of one call have for the inputs of the server, or
   nil when it has none."
  [opts]
  (not-empty (into {} (filter (comp some? val)) (select-keys opts [:ask :sample :roots]))))

(defn- input-response
  "The result of the client of `spec` for the input request `request` of server
   `name`, from the `answers` of the call. An elicitation with no answer is a
   cancel. A refused sampling request has no result in this protocol, so it ends
   the call. A server must not ask for an input that the client did not declare."
  [name spec request {:keys [ask sample roots]}]
  (let [method
        (get request "method")

        params
        (get request "params")]

    (case (when (get spec (input-capability method)) method)
      "elicitation/create"
      (or (when ask
            (try (get (elicitation-reply {"params" params} ask) "result")
                 (catch Throwable t
                   ;; A cancelled turn ends the call; any other failure is no answer.
                   (when (cancellation/preserve-interrupt! t) (throw t)))))
          {"action" "cancel"})

      "roots/list"
      {"roots" (vec roots)}

      "sampling/createMessage"
      (let [result (when sample (sample params))]
        (if (map? result)
          result
          (throw (ex-info (str "MCP " name " asked for a model answer, and the person refused it")
                          {:type :mcp/input-refused :server name}))))

      (throw (ex-info (str "MCP " name " asked for " method ", which Vis did not declare")
                      {:type :mcp/protocol :server name})))))

(defn- input-params
  "`params` of a request with the answer to its `input_required` result `result`:
   one response for each input request, and the state of the server only when the
   server gave one."
  [name spec params result answers]
  (let [requests (get result "inputRequests")]
    (cond-> params
      (and (map? requests) (seq requests))
      (assoc "inputResponses"
        (reduce-kv (fn [responses k request]
                     (assoc responses k (input-response name spec request answers)))
                   {}
                   requests))

      (contains? result "requestState")
      (assoc "requestState" (get result "requestState")))))

(defn- modern-request-fn
  "The request fn of a connection in a stateless revision, over the request fn
   `request` of its transport. Each request gets `meta` in its `_meta`. A result of
   the type `input_required` gets its input from the answers of the call: `:ask`,
   `:sample` and `:roots`. Then the request goes again, as a new request, with
   the answers and the state."
  [name spec request meta]
  (fn modern ([method params timeout-ms] (modern method params timeout-ms nil))
    ([method params timeout-ms opts] (let [params (update (or params {}) "_meta" merge meta)]
                                       (loop [sent params
                                              round 0]

                                         (let [result (request method sent timeout-ms opts)
                                               result-type (or (when (map? result)
                                                                 (get result "resultType"))
                                                               "complete")]

                                           (cond
                                             (= "complete" result-type) result
                                             (not (and (= "input_required" result-type)
                                                       (contains? #{"tools/call" "prompts/get"
                                                                    "resources/read"}
                                                                  method)))
                                             (throw
                                               (ex-info
                                                 (str "MCP " name
                                                      " " method
                                                      " has the unknown result type " result-type)
                                                 {:type :mcp/protocol :server name :method method}))
                                             (>= (long round) (long max-input-rounds))
                                             (throw (ex-info (str "MCP "
                                                                  name
                                                                  " "
                                                                  method
                                                                  " asked for input too many times")
                                                             {:type :mcp/protocol
                                                              :server name
                                                              :method method}))
                                             :else (recur
                                                     (input-params name spec params result opts)
                                                     (inc (long round))))))))))

(defn- await-reply!
  "Take the reply of one call from `inbox`, or `::timeout` when nothing came for
   `timeout-ms`. A person input of the server runs HERE, on the thread of the
   caller, through `answers`: the form belongs to the turn of that thread, and the
   time that the person takes is no part of the deadline."
  [^LinkedBlockingQueue inbox timeout-ms answers]
  (loop []

    (let [[kind value send!] (.poll inbox (long timeout-ms) TimeUnit/MILLISECONDS)]
      (case kind
        :reply
        value

        :input
        (do (send! (try (input-reply value answers)
                        (catch Throwable t
                          ;; A cancelled turn ends the call; any other failure is no answer.
                          (if (cancellation/preserve-interrupt! t) (throw t) (unanswered value)))))
            (recur))

        ::timeout))))

(defn- without-caller [callers inbox] (vec (remove #(identical? inbox (first %)) callers)))

(defn- input-state
  "The state of one transport for the input requests of its server. `:callers` is
   an atom with the calls that can answer a person input, newest last, each as
   `[inbox asker]`. `:roots` is
   an atom with the roots of the connection. Each is nil when the client of `spec`
   did not declare that input."
  [spec]
  {:spec spec
   :callers (when (or (:elicitation? spec) (:sampling? spec)) (atom []))
   :roots (when (:roots? spec) (atom {:roots (vec (:roots spec))}))})

(defn- handle-incoming!
  "Handle a message of the server that is not a reply. A person input goes to the
   newest call that waits in `inputs`. It has no answer when none waits, and when
   the calls that wait have more than one asker: the server names no call, and a
   guess can ask the person of another session. A
   `roots/list` gets the roots of the connection. Any other request gets its
   answer through `send!`, and a notification goes to the fn in the atom
   `incoming`. Never throws."
  [msg send! incoming {:keys [spec callers roots]}]
  (try (case (declared-input spec msg)
         ("elicitation/create" "sampling/createMessage")
         (let [waiting
               @callers

               [^LinkedBlockingQueue inbox]
               (peek waiting)]

           (if (and inbox (apply = (map second waiting)))
             (.put inbox [:input msg send!])
             (send! (unanswered msg))))

         "roots/list"
         (let [{:keys [read] :as state} (swap! roots assoc :asked? true)]
           (send! {"jsonrpc" "2.0" "id" (get msg "id") "result" {"roots" (:roots state)}})
           (some-> read
                   (deliver true)))

         (cond (server-request-reply msg) (send! (server-request-reply msg))
               (and (map? msg) (string? (get msg "method"))) (when-let [on-notification @incoming]
                                                               (on-notification msg))))
       (catch Throwable _ nil)))

(defn- duplex
  "The request side of a transport where every message of the server comes on ONE
   channel: stdio and legacy SSE. `write!` sends one message and `closed?` is the
   atom of the transport. The reader of the transport gives each message to
   `:receive!` and calls `:fail-pending!` when the channel ends. `:subscribe-fn`
   sends `subscriptions/listen`: its notifications come on the channel. `spec` is
   the client spec: it says which inputs the client declared."
  [name write! closed? spec]
  (let [pending
        (ConcurrentHashMap.)

        next-id
        (atom 0)

        ;; `connect` puts the notification handler here.
        incoming
        (atom nil)

        {:keys [callers] :as inputs}
        (input-state spec)]

    {:incoming incoming
     :roots-atom (:roots inputs)
     :receive! (fn [msg]
                 ;; A message with a method is no reply, whatever its id.
                 (if (and (map? msg) (contains? msg "method"))
                   (handle-incoming! msg write! incoming inputs)
                   (when-let [id (get msg "id")]
                     (when-let [^LinkedBlockingQueue inbox (.remove pending id)]
                       (.put inbox [:reply msg])))))
     :fail-pending! (fn []
                      (doseq [k (enumeration-seq (.keys pending))]
                        (when-let [^LinkedBlockingQueue inbox (.remove pending k)]
                          (.put inbox [:reply {"error" {"message" "server stream closed"}}]))))
     :request-fn
     (fn request ([method params timeout-ms] (request method params timeout-ms nil))
       ([method params timeout-ms opts] (when @closed? (throw (ex-info "MCP server not running"
                                                                {:type :mcp/closed :server name})))
        (let [id
              (str (swap! next-id inc))

              inbox
              (LinkedBlockingQueue.)

              answers
              (when callers (answers-of opts))]

          (.put pending id inbox)
          (when answers (swap! callers conj [inbox (:asker opts)]))
          (try (write! (cond-> {"jsonrpc" "2.0" "id" id "method" method}
                         (some? params)
                         (assoc "params" params)))
               (let [msg (await-reply! inbox timeout-ms answers)]
                 (cond (= msg ::timeout) (throw (ex-info
                                                  (str "MCP " name " " method " timed out")
                                                  {:type :mcp/timeout :server name :method method}))
                       (get msg "error") (throw (rpc-error->ex name method (get msg "error")))
                       :else (get msg "result")))
               (finally (.remove pending id)
                        (when answers (swap! callers without-caller inbox)))))))
     :notify-fn (fn [method params]
                  (when-not @closed?
                    (write! (cond-> {"jsonrpc" "2.0" "method" method}
                              (some? params)
                              (assoc "params" params)))))
     ;; No call waits for the end of the subscription.
     :subscribe-fn (fn [params]
                     (when-not @closed?
                       (write! {"jsonrpc" "2.0"
                                "id" (str "listen-" (swap! next-id inc))
                                "method" "subscriptions/listen"
                                "params" params})))}))

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
  [name ^Process proc spec]
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
        (duplex name write! closed? spec)

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
    (assoc (select-keys core [:incoming :roots-atom :request-fn :notify-fn :subscribe-fn])
      :pid (try (.pid proc) (catch Throwable _ nil))
      :close-fn (fn []
                  (reset! closed? true)
                  (stop-stdio-process! proc))
      :alive-fn (fn []
                  (and (not @closed?) (.isAlive proc))))))

(defn- start-stdio!
  "Spawn `command`+`args` (with extra `env`), wire newline-delimited JSON-RPC.
   Returns `{:request-fn :notify-fn :close-fn :alive-fn :pid}`."
  [name {:keys [command args env cwd] :as spec}]
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

    (try (stdio-transport name proc spec)
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
   transport. `request-of` gives the request for a cursor: the GET of a session,
   or the POST of `subscriptions/listen`. A message for which `reply?` is true
   ends one stream. The promise `ready` gets a value when the first request has
   its answer."
  [server-name open! read-stream closed? ready request-of reply?]
  (letfn
    [(listen-once [cursor]
       (let [resp
             (open! (request-of cursor))

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
               :else (let [cursor (read-stream resp cursor reply? nil)]
                       ;; The reply to a subscription is its end: ask again later.
                       (if (:reply cursor) ::retry cursor)))))]
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
   token, and returns the response with an open body. The caller closes the body.
   One 401 refreshes the token and sends again. The atoms `session`,
   `protocol-version*` and `www-auth` are those of the transport.

   `proof-fn` gives the DPoP proof for a DPoP-bound token (RFC 9449): the request
   then has the `DPoP` scheme and the proof, with the last nonce of the server. One
   `use_dpop_nonce` refusal is sent again with the new nonce."
  [url headers bearer-fn proof-fn session protocol-version* www-auth]
  (let [nonce
        (atom nil)

        send1
        (fn [request bearer]
          (let [target
                (or (:uri request) url)

                proof
                (when (and bearer proof-fn)
                  (proof-fn (str/upper-case (name (:method request))) target bearer @nonce))

                resp
                (http/request (assoc request
                                :uri target
                                :client @mcp-http/client
                                :headers (cond-> (merge (apply-headers headers)
                                                        {"MCP-Protocol-Version" @protocol-version*}
                                                        (:headers request))
                                           @session
                                           (assoc "Mcp-Session-Id" @session)

                                           bearer
                                           (assoc "Authorization"
                                             (str (if proof "DPoP " "Bearer ") bearer))

                                           proof
                                           (assoc "DPoP" proof))
                                :throw false
                                ;; A stream: the reply can come before the server ends the body.
                                :as :stream))]

            (when-let [sid (response-header resp "mcp-session-id")]
              (reset! session sid))
            (when-let [wa (response-header resp "www-authenticate")]
              (reset! www-auth wa))
            (when-let [value (response-header resp "dpop-nonce")]
              (reset! nonce value))
            (assoc resp ::proof? (some? proof))))

        send
        (fn [request bearer]
          (let [resp (send1 request bearer)]
            (if (and (::proof? resp)
                     (= 401 (:status resp))
                     (some->> (response-header resp "www-authenticate")
                              (re-find #"use_dpop_nonce")))
              (do (close-body! resp) (send1 request bearer))
              resp)))]

    (fn [request]
      (let [tok
            (when bearer-fn (try (bearer-fn) (catch Throwable _ nil)))

            first-resp
            (send request tok)]

        (if (and (= 401 (:status first-resp)) bearer-fn)
          ;; `bearer-fn` refuses only with a typed failure: `:mcp/oauth-required`, or
          ;; `:mcp/oauth-token` for a grant that needs no person. It must escape: as
          ;; a plain HTTP 401 it skipped the sign-in backoff, and every turn
          ;; connected again (#458).
          (let [fresh (try (bearer-fn tok) (catch Throwable t (close-body! first-resp) (throw t)))]
            (if (and fresh (not= fresh tok))
              (do (close-body! first-resp) (send request fresh))
              first-resp))
          first-resp)))))

(defn- http-error
  "The typed failure for the response `resp` to `what` of server `name`."
  [name resp what]
  (ex-info (str "MCP " name " HTTP " (:status resp) " on " what)
           {:type :mcp/http-error :server name :status (:status resp) :body (body-text resp)}))

(defn- refusal
  "The failure for the error response `resp` to `method` of server `name`. With
   `modern?`, a JSON-RPC error in the body is the refusal of the peer: a stateless
   server sends it with an HTTP error status, and the transport is not lost. A 401
   or 403 stays an HTTP failure, because the sign-in reads it."
  [name resp method modern?]
  (let [failure
        (http-error name resp method)

        {:keys [status body]}
        (ex-data failure)

        error
        (when (and modern? (not (contains? #{401 403} status)))
          (let [value (try (json-> body) (catch Throwable _ nil))]
            (when (and (map? value) (map? (get value "error"))) (get value "error"))))]

    (if error (rpc-error->ex name method error status) failure)))

(defn- start-http!
  "Streamable-HTTP transport against `url` with optional static `headers` and
   an optional `:bearer-fn` OAuth token provider. `:www-auth-atom`, when given, is
   the caller's atom to keep pointed at the latest `WWW-Authenticate` challenge."
  [name {:keys [url headers bearer-fn proof-fn www-auth-atom listen?] :as spec}]
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

        ;; True while the client speaks a stateless revision. `connect` writes it.
        modern
        (atom false)

        ;; `connect` puts the notification handler here.
        incoming
        (atom nil)

        {:keys [callers] :as inputs}
        (input-state spec)

        listen-thread
        (atom nil)

        ;; Has a value when the listen channel got its first answer.
        listening
        (promise)

        open!
        (http-opener url headers bearer-fn proof-fn session protocol-version* www-auth)

        post!
        (fn [body timeout-ms headers]
          (open! {:method :post
                  :headers (merge {"Content-Type" "application/json"
                                   "Accept" "application/json, text/event-stream"}
                                  headers)
                  :body body
                  :timeout timeout-ms}))

        reply!
        (fn [msg]
          (close-body! (post! (->json msg) 10000 nil)))

        ;; Read the events of `resp` until a message for which `reply?` is true, or
        ;; until the stream ends. Returns `cursor` with the last event `:id` and
        ;; `:retry` delay, plus `:reply` when one came. A person input on the stream
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
                        (and inbox (person-input? spec msg)) (do (.put inbox [:input msg reply!])
                                                                 (recur cursor))
                        :else (do (when msg (handle-incoming! msg reply! incoming inputs))
                                  (recur cursor))))
                cursor))))

        ;; The JSON-RPC reply to the request that `resp` answers, or nil. A stream
        ;; that ends before the reply continues on a GET with the id of its last
        ;; event, after the delay that the server gave. A stateless server has no
        ;; stream to continue.
        reply-of
        (fn [resp method timeout-ms inbox]
          (cond (>= (long (:status resp)) 400) (throw (refusal name resp method @modern))
                (event-stream? resp)
                (loop [resp
                       resp

                       cursor
                       {}]

                  (let [cursor (read-stream resp cursor response? inbox)]
                    (cond (:reply cursor) (:reply cursor)
                          (and (:id cursor) (not @modern))
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
               ([method params timeout-ms {:keys [headers] :as opts}]
                (let [answers
                      (when callers (answers-of opts))

                      inbox
                      (LinkedBlockingQueue.)]

                  (when answers
                    (swap! callers conj [inbox (:asker opts)])
                    ;; A server can send its person input on the listen channel only.
                    (when @listen-thread (deref listening 2000 nil)))
                  (try
                    (let [body
                          (->json
                            (cond-> {"jsonrpc" "2.0" "id" (str (System/nanoTime)) "method" method}
                              (some? params)
                              (assoc "params" params)))

                          ;; A stream read has no timeout of its own, so the exchange
                          ;; runs apart and this thread keeps the deadline.
                          exchange
                          (future (.put inbox
                                        [:reply
                                         (try {:msg (reply-of (post! body
                                                                     timeout-ms
                                                                     (when @modern
                                                                       (merge (request-headers
                                                                                method
                                                                                params)
                                                                              headers)))
                                                              method
                                                              timeout-ms
                                                              (when answers inbox))}
                                              (catch Throwable t {:thrown t}))]))

                          outcome
                          (try (await-reply! inbox timeout-ms answers)
                               (catch Throwable t (future-cancel exchange) (throw t)))

                          msg
                          (:msg outcome)]

                      (cond (= ::timeout outcome)
                            (do (future-cancel exchange)
                                (throw (ex-info (str "MCP " name " " method " timed out")
                                                {:type :mcp/timeout :server name :method method})))
                            (:thrown outcome) (throw (:thrown outcome))
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
                      (throw t))
                    (finally (when answers (swap! callers without-caller inbox)))))))
             :notify-fn (fn [method params]
                          (try (reply! (cond-> {"jsonrpc" "2.0" "method" method}
                                         (some? params)
                                         (assoc "params" params)))
                               (catch Throwable _ nil)))
             :close-fn (fn []
                         (reset! closed? true)
                         (some-> ^Thread @listen-thread
                                 .interrupt)
                         (when @session
                           (try (close-body! (open! {:method :delete :timeout 5000}))
                                (catch Throwable _ nil)))
                         (reset! session nil))
             :alive-fn (fn []
                         (not @closed?))
             :incoming incoming
             :roots-atom (:roots inputs)
             :www-auth-atom www-auth
             :session-atom session
             :protocol-version-atom protocol-version*
             :modern-atom modern}
      listen?
      (assoc :listen-start-fn
        (fn []
          (reset! listen-thread (http-listen-loop! name
                                                   open!
                                                   read-stream
                                                   closed?
                                                   listening
                                                   (fn [cursor]
                                                     {:method :get
                                                      :headers
                                                      (cond-> {"Accept" "text/event-stream"}
                                                        (:id cursor)
                                                        (assoc "Last-Event-ID" (:id cursor)))
                                                      :timeout 3600000})
                                                   (constantly false))))
        :subscribe-fn
        (fn [params]
          (reset! listen-thread (http-listen-loop!
                                  name
                                  open!
                                  read-stream
                                  closed?
                                  listening
                                  (fn [_cursor]
                                    {:method :post
                                     :headers {"Content-Type" "application/json"
                                               "Accept" "application/json, text/event-stream"
                                               "Mcp-Method" "subscriptions/listen"}
                                     :body (->json {"jsonrpc" "2.0"
                                                    "id" (str (System/nanoTime))
                                                    "method" "subscriptions/listen"
                                                    "params" params})
                                     :timeout 3600000})
                                  response?)))))))

(defn- start-sse!
  "The HTTP+SSE transport of protocol 2024-11-05 against `url`. A GET opens the one
   stream of the server. Its first event, `endpoint`, names the URL for the POST
   of each client message, on the origin of `url`; every reply comes on the
   stream. The transport is dead when the stream ends."
  [name {:keys [url headers bearer-fn proof-fn www-auth-atom timeout-ms] :as spec}]
  (let [protocol-version*
        (atom protocol-version)

        www-auth
        (or www-auth-atom (atom nil))

        closed?
        (atom false)

        open!
        (http-opener url headers bearer-fn proof-fn (atom nil) protocol-version* www-auth)

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
        (duplex name write! closed? spec)

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
    (assoc (select-keys core [:incoming :roots-atom :request-fn :notify-fn])
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

(defn- open-transport!
  [name spec transport]
  (case transport
    :stdio
    (start-stdio! name spec)

    :streamable-http
    (start-http! name spec)

    :sse
    (start-sse! name spec)

    (throw (ex-info (str "MCP: unknown transport " (pr-str transport))
                    {:type :mcp/config :server name}))))

(defn- initialize-request
  "Send `initialize` on `conn` and return the result of the server."
  [conn]
  ((:request-fn conn)
    "initialize"
    {"protocolVersion" protocol-version
     "capabilities" (client-capabilities (:spec conn))
     "clientInfo" client-info}
    (:timeout-ms conn)))

(defn- initialized!
  "Complete the handshake of an older revision on `conn`, after the server gave
   the result `init` of `initialize`."
  [conn init]
  (let [negotiated-protocol-version (or (get init "protocolVersion") protocol-version)]
    ;; Every message after `initialize` names the negotiated version: a server
    ;; refuses a version that it does not know.
    (when-let [version-atom (:protocol-version-atom conn)]
      (reset! version-atom negotiated-protocol-version))
    ;; Per spec, acknowledge before issuing further requests.
    ((:notify-fn conn) "notifications/initialized" nil)
    (when-let [start (:listen-start-fn conn)]
      (start))
    (assoc conn
      :server-info (get init "serverInfo")
      :server-capabilities (get init "capabilities")
      :protocol-version negotiated-protocol-version)))

(defn- initialize!
  "Run the `initialize` handshake of an older revision on `conn`."
  [conn]
  (initialized! conn (initialize-request conn)))

(def ^:private probe-final-statuses
  "The client error statuses of `server/discover` that say nothing about the era:
   a sign-in, a proxy sign-in and a rate limit."
  #{401 403 407 429})

(defn- discover!
  "Ask the server of `conn` for `server/discover`, the request that only a server
   of a stateless revision knows. Returns `{:version :result}` for such a server.
   Returns `{:legacy? true}` for a server that wants `initialize`, also when it
   gave no answer for `timeout-ms` or refused the request with a client error
   status. A stateless server that names
   its versions gets one more request, in a version that Vis speaks too. Throws
   when a stateless server refuses the client, and when the answer says nothing
   about the era: a sign-in, a server failure or a lost connection."
  [conn timeout-ms]
  (let [ask (fn [version]
              (when-let [version-atom (:protocol-version-atom conn)]
                (reset! version-atom version))
              (try {:result ((:request-fn conn)
                              "server/discover"
                              {"_meta" (request-meta version (:spec conn))}
                              timeout-ms)}
                   (catch clojure.lang.ExceptionInfo failure {:failure failure})))]
    (loop [version (first modern-versions)
           retried? false]

      (let [{:keys [result failure]} (ask version)
            {:keys [type code data status]} (ex-data failure)]

        (cond (and (map? result) (sequential? (get result "supportedVersions"))) {:version version
                                                                                  :result result}
              ;; Any other result is the answer of a server that knows no such request.
              (nil? failure) {:legacy? true}
              (and (= :mcp/rpc-error type) (= -32022 code))
              (let [supported (set (filter string? (get data "supported")))
                    mutual (some supported modern-versions)]

                (cond (and mutual (not retried?)) (recur mutual true)
                      (and (not mutual) (seq supported)) {:legacy? true}
                      :else (throw failure)))
              ;; A header mismatch or a missing capability: a stateless server refused.
              (and (= :mcp/rpc-error type) (contains? #{-32020 -32021} code)) (throw failure)
              ;; A server refuses a request that it does not know with one of many
              ;; statuses. `initialize` then gets the real answer.
              (or (contains? #{:mcp/rpc-error :mcp/protocol :mcp/timeout} type)
                  (and (= :mcp/http-error type)
                       (<= 400 (long (or status 0)) 499)
                       (not (contains? probe-final-statuses status))))
              {:legacy? true}
              :else (throw failure))))))

(defn- modern-conn
  "`conn` for a server that answered `server/discover` with `result` in the
   stateless protocol `version`. No handshake follows: each request carries the
   request metadata. When the server can say that its tools changed, the client
   subscribes to that."
  [conn version result]
  (let [meta
        (request-meta version (:spec conn))

        conn
        (assoc conn
          :request-fn (modern-request-fn (:name conn) (:spec conn) (:request-fn conn) meta)
          :modern? true
          :server-info (or (get-in result ["_meta" "io.modelcontextprotocol/serverInfo"])
                           (get result "serverInfo"))
          :server-capabilities (get result "capabilities")
          :protocol-version version)]

    (when (true? (get-in result ["capabilities" "tools" "listChanged"]))
      (when-let [subscribe (:subscribe-fn conn)]
        (subscribe {"_meta" meta "notifications" {"toolsListChanged" true}})))
    conn))

(defn- stdio-era!
  "Find the era of the stdio server of `conn`. `server/discover` goes first. A
   server that is silent for `stdio-hedge-ms` gets `initialize` too, and both
   requests wait: a server of an older revision can ignore the first, and a
   server that starts slowly reads both when it is ready. A `server/discover`
   result means a stateless server, whatever `initialize` gets. Returns the
   answer of `discover!`, with the result of `initialize` under `:init` when
   the server gave one."
  [conn timeout-ms]
  (let [answers
        (LinkedBlockingQueue.)

        ask
        (fn [k f]
          (future (.put answers [k (try (f) (catch Throwable t {:failure t}))])))

        answer!
        (fn [wait-ms]
          (.poll answers (long wait-ms) TimeUnit/MILLISECONDS))

        settle
        (fn [{:keys [failure] :as answer}]
          (if failure (throw failure) answer))

        _
        (ask :discover #(discover! conn timeout-ms))

        [_ early]
        (answer! stdio-hedge-ms)]

    (if early
      (settle early)
      (do (ask :initialize #(hash-map :legacy? true :init (initialize-request conn)))
          (loop [seen {}]
            ;; Each request ends at its own timeout, so an answer always comes.
            (let [[k answer] (answer! (+ (long timeout-ms) (long stdio-hedge-ms) 1000))
                  {:keys [discover initialize] :as seen} (cond-> seen
                                                           k
                                                           (assoc k answer))]

              (cond (nil? k)
                    (throw (ex-info (str "MCP " (:name conn) " initialize timed out")
                                    {:type :mcp/timeout :server (:name conn) :method "initialize"}))
                    (:version discover) discover
                    (:failure discover) (throw (:failure discover))
                    (and discover initialize) (settle initialize)
                    ;; A server that answers out of order can still be stateless.
                    (contains? initialize :init) (let [[_ late] (answer! stdio-order-grace-ms)]
                                                   (if (:version late) late initialize))
                    :else (recur seen))))))))

(def ^:private listed
  "The list requests that `list-all` keeps, each with the key of its rows and the
   notification of the server that drops the kept list."
  {"prompts/list" ["prompts" "notifications/prompts/list_changed"]
   "resources/list" ["resources" "notifications/resources/list_changed"]})

(defn- connect-as
  "Connect to MCP server `name` on `transport` and find its era. With `probe?`,
   the first request is `server/discover`: a server of a stateless revision
   answers it and gets no handshake. Any other server gets `initialize`. A stdio
   server can exit on a request that comes before `initialize`: it starts again
   and gets only the handshake. A failed connect always closes the transport it
   already opened, so failed stdio starts cannot orphan processes."
  ([name spec transport] (connect-as name spec transport (not= :sse transport)))
  ([name spec transport probe?]
   (let [t
         (open-transport! name spec transport)

         timeout-ms
         (or (:timeout-ms spec) default-timeout-ms)

         conn
         (merge t
                {:name name
                 :transport transport
                 :spec spec
                 :connected-at (util/now-ms)
                 :tools (atom nil)
                 :lists (atom {})
                 :timeout-ms timeout-ms})

         http?
         (= :streamable-http transport)]

     ;; A `notifications/tools/list_changed` of the server, on either transport,
     ;; invalidates the tools cache so the next `list-tools` fetches again. The
     ;; notification of a list of `listed` drops that list. A caller-supplied
     ;; `:on-notification` fires after that.
     (reset! (:incoming t) (let [user-cb (:on-notification spec)]
                             (fn [msg]
                               (let [method (get msg "method")]
                                 (when (= "notifications/tools/list_changed" method)
                                   (reset! (:tools conn) nil))
                                 (doseq [[request [_ notification]] listed
                                         :when (= notification method)]

                                   (swap! (:lists conn) dissoc request)))
                               (when user-cb (try (user-cb msg) (catch Throwable _ nil))))))
     (try (let [ended?
                (fn []
                  (and probe? (not http?) (not ((:alive-fn conn)))))

                answer
                (cond (not probe?) {:legacy? true}
                      http? (do (when-let [modern (:modern-atom t)]
                                  (reset! modern true))
                                (discover! conn
                                           (min (long timeout-ms) (long http-probe-timeout-ms))))
                      :else (try (stdio-era! conn timeout-ms)
                                 (catch clojure.lang.ExceptionInfo failure
                                   (if (ended?) {:legacy? true} (throw failure)))))]

            (cond (:version answer) (modern-conn conn (:version answer) (:result answer))
                  ;; The refused request can be the end of a transport: open a new one.
                  (and probe? (or http? (ended?))) (do ((:close-fn conn))
                                                       (connect-as name spec transport false))
                  (contains? answer :init) (initialized! conn (:init answer))
                  :else (initialize! conn)))
          (catch Throwable failure
            (try ((:close-fn conn)) (catch Throwable _ nil))
            (throw failure))))))

(defn- older-server?
  "Whether `failure` of a Streamable HTTP `initialize` is the answer of a server
   that can have only the older HTTP+SSE transport: it refused the POST itself."
  [failure]
  (let [{:keys [type status]} (ex-data failure)]
    (and (= :mcp/http-error type) (contains? #{400 404 405} status))))

(defn connect
  "Connect to MCP server `name` per its `spec` and find its era. `spec` supports:
     stdio           → `{:transport :stdio :command :args :env :cwd :timeout-ms}`
     streamable-http → `{:transport :streamable-http :url :headers :bearer-fn
                         :timeout-ms :listen? :on-notification}`
     sse             → `{:transport :sse :url :headers :bearer-fn :timeout-ms}`
   With `:elicitation? true` the client declares form elicitation, with
   `:sampling? true` sampling and with `:roots? true` roots; see `request`.
   On stdio and Streamable HTTP the first request is `server/discover`. A server
   of a stateless revision answers it, and the conn has `:modern? true`. Any other
   server gets the `initialize` handshake.
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

(defn- mirrors-headers?
  "Whether a tool call on `conn` mirrors arguments into `Mcp-Param-*` headers."
  [conn]
  (and (:modern? conn) (= :streamable-http (:transport conn))))

(defn list-tools
  "`tools/list` → vector of tool maps `{\"name\" \"description\" \"inputSchema\"}`,
   from every page of the server. Cached on the conn after the first call;
   invalidated by `notifications/tools/list_changed`. A tool of a stateless HTTP
   server with an `x-mcp-header` that breaks the rules is left out."
  [conn]
  (or
    @(:tools conn)
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
                tools)))

          tools
          (if (mirrors-headers? conn)
            (filterv (fn [tool]
                       (or (valid-header-annotations? tool)
                           (do (tel/log! {:level :warn
                                          :id ::invalid-tool-header
                                          :data {:server (:name conn) :tool (get tool "name")}}
                                         "MCP tool left out: its x-mcp-header is not valid")
                               false)))
              tools)
            tools)]

      (reset! (:tools conn) tools)
      tools)))

(defn- use-roots!
  "Make `roots` the roots of `conn` for the request that follows. A server of an
   older revision reads the roots with `roots/list` when it wants. So the client
   says that the list changed, and a server that read the list before gets
   `roots-grace-ms` to read it again. A connection has ONE list: with calls from
   two workspaces at the same time, the server sees the list of the newest call."
  [conn roots]
  (when-let [state (:roots-atom conn)]
    (when (and (some? roots) (not= (vec roots) (:roots @state)))
      (let [read (promise)
            [before _] (swap-vals! state assoc :roots (vec roots) :read read)]

        (when-not (:modern? conn)
          ((:notify-fn conn) "notifications/roots/list_changed" nil)
          (when (:asked? before) (deref read roots-grace-ms nil)))))))

(defn request
  "Send the MCP request `method` with `params` on `conn`, and return its result
   map. `opts` has the answers of this request to the inputs that the client
   declared and that the server asks for, while the request waits or in an
   `input_required` result of a stateless server:

   `:on-elicitation` answers a form elicitation. It gets the params of
   `elicitation/create`, runs on the calling thread, and returns
   `{\"action\" \"accept\" \"content\" {...}}` or the action `decline`;
   anything else is a cancel. The defaults of the schema fill the fields that an
   accepted content leaves out.

   `:on-sampling` answers `sampling/createMessage`. It gets the params, runs on
   the calling thread, and returns the result map, or nil to refuse the request.

   `:roots` is the vector of roots (`{\"uri\" \"name\"}`) for `roots/list`.

   `:asker` names who gives those answers, such as a session id. A server that
   names no call sends its request to the newest call that waits on the
   connection. When the calls that wait have more than one asker, nobody is
   asked: the request gets no answer."
  ([conn method params] (request conn method params (or (:timeout-ms conn) default-timeout-ms) nil))
  ([conn method params timeout-ms {:keys [on-elicitation on-sampling roots headers asker]}]
   (use-roots! conn roots)
   ((:request-fn conn)
     method
     params
     timeout-ms
     (cond-> {:ask on-elicitation :sample on-sampling :roots roots :asker asker}
       headers
       (assoc :headers headers)))))

(defn call-tool
  "`tools/call` `tool-name` with `arguments` (a map). Returns the result map
   (`{\"content\" [...] \"isError\" bool}`). `opts` are those of `request`."
  ([conn tool-name arguments]
   (call-tool conn tool-name arguments (or (:timeout-ms conn) default-timeout-ms)))
  ([conn tool-name arguments timeout-ms] (call-tool conn tool-name arguments timeout-ms nil))
  ([conn tool-name arguments timeout-ms opts]
   (let [arguments
         (json-safe (or arguments {}))

         tool
         (when (mirrors-headers? conn)
           (some #(when (= tool-name (get % "name")) %) (list-tools conn)))]

     (request conn
              "tools/call"
              {"name" tool-name "arguments" arguments}
              timeout-ms
              (cond-> (select-keys opts [:on-elicitation :on-sampling :roots :asker])
                tool
                (assoc :headers (param-headers tool arguments)))))))

(defn stale-era?
  "Whether `failure` of a request on `conn` can mean that the server changed its
   era after the connect, or lost its session: a Streamable HTTP server refused
   the request itself, as a server of the other era does. Such a request did not
   run. The caller connects again, which finds the era again."
  [conn failure]
  (let [{:keys [type status code]} (ex-data failure)]
    (and (= :streamable-http (:transport conn))
         (contains? #{400 404 405} status)
         (or (= :mcp/http-error type)
             (and (:modern? conn)
                  (= :mcp/rpc-error type)
                  (contains? #{-32000 -32022 -32600 -32601} code))))))

(def list-row-limit
  "The most rows that `list-all` reads from one server. A server can list
   thousands of resources."
  1000)

(defn list-all
  "Every row of the list request `method`, `prompts/list` or `resources/list`,
   from the pages of the server: at most `list-row-limit` rows. Cached on the conn
   after the first call; the `list_changed` notification of that list drops it.
   `opts` are those of `request`."
  ([conn method] (list-all conn method nil))
  ([conn method opts]
   (let [cache
         (:lists conn)

         rows-key
         (first (listed method))]

     (or (some-> cache
                 deref
                 (get method))
         (let [timeout-ms
               (or (:timeout-ms conn) default-timeout-ms)

               rows
               (loop [rows
                      []

                      cursor
                      nil]

                 (let [result
                       (request conn method (if cursor {"cursor" cursor} {}) timeout-ms opts)

                       rows
                       (into rows (get result rows-key))

                       next-cursor
                       (get result "nextCursor")]

                   (if (and (string? next-cursor)
                            (seq next-cursor)
                            (not= cursor next-cursor)
                            (< (count rows) (long list-row-limit)))
                     (recur rows next-cursor)
                     (vec (take list-row-limit rows)))))]

           (when cache (swap! cache assoc method rows))
           rows)))))

(defn alive? [conn] (boolean (try ((:alive-fn conn)) (catch Throwable _ false))))

(defn close
  [conn]
  (try ((:close-fn conn)) (catch Throwable _ nil))
  (reset! (:tools conn) nil)
  (some-> (:lists conn)
          (reset! {}))
  nil)
