(ns com.blockether.vis.internal.foundation.acp
  "ACP — the Agent Client Protocol (Zed's editor↔agent protocol, also spoken by
   Neovim/JetBrains plug-ins) served BY vis.

   MCP points OUT of the agent (agent → tools). ACP points IN (editor → agent),
   so here vis is the SERVER: an editor spawns `vis acp`, speaks newline-framed
   JSON-RPC 2.0 over stdio, and drives real vis sessions.

   The protocol layer is deliberately TRANSPORT-FREE and BACKEND-FREE:

     * [[handle-line!]] takes one framed line and returns the response map. Every
       adversarial case (bad JSON, batch array, non-object message, wrong
       `jsonrpc`, missing/duplicate id, unknown method, wrong param types, calls
       before `initialize`) is answered with a well-formed JSON-RPC error instead
       of a thrown stack trace or a desynchronized stream.

     * [[connection]] takes a `:backend` map of functions
       (`:new-session`/`:load-session`/`:prompt`/`:cancel`). [[gateway-backend]]
       is the live one — every ACP session IS an ordinary gateway session, so the
       SAME turn can be watched from the TUI, the web surface, and the phone
       Companion while the editor drives it. Nobody else's ACP agent does that,
       because everyone else's is a private per-editor subprocess.

   Two transports ship:

     * [[serve!]] — stdio, the transport the ACP spec requires. Full duplex, so
       `session/request_permission` and `fs/*` work.
     * [[routes-contribution]] — `POST /v1/acp` on the regular gateway daemon,
       one JSON-RPC message per request, every message the agent produced coming
       back in `messages`. HALF-DUPLEX by construction (the agent cannot block on
       an answer that would need a second request), so client-bound calls fail
       fast instead of deadlocking, and the permission hook stays out of the way.

   Encoding is total: [[json-safe]] renders keywords, non-finite doubles and
   unknown objects rather than letting `write-json-str` throw halfway through a
   frame and poison the stream — the same discipline `gateway/wire.clj` applies."
  (:require [charred.api :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.internal.foundation.mcp.core :as mcp]
            [taoensso.telemere :as tel])
  (:import [java.io BufferedReader InputStream InputStreamReader OutputStream OutputStreamWriter
            Writer]
           [java.nio.charset StandardCharsets]
           [java.util Map]
           [java.util.concurrent ExecutorService Executors TimeUnit]))

;; =============================================================================
;; Protocol constants
;; =============================================================================

(def protocol-version "Newest ACP protocol version this agent implements." 1)

(def supported-protocol-versions
  "Versions [[handle-line!]] will echo back as negotiated.

   ONLY v1, deliberately. v0 was listed here too, but nothing in this namespace
   ever branched on it: we answered `protocolVersion 0` and then spoke v1 shapes
   at the client anyway. Claiming a version we do not implement is worse than
   refusing it — a v0-only client now reads `1` out of the initialize response
   and disconnects on its own terms, per the spec's instruction to the client."
  #{1})

(def agent-info
  "ACP `Implementation` for the initialize response — what a client shows the
   user and puts in bug reports. Version is the `vis/VERSION` build resource —
   the repo-root VIS_VERSION, verbatim — and \"dev\" when running from source."
  {"name" "vis"
   "version" (or (some-> (io/resource "vis/VERSION")
                         slurp
                         str/trim
                         not-empty)
                 "dev")})

(def error-codes
  "JSON-RPC 2.0 codes plus ACP's own reserves.

   ACP hands out meanings inside JSON-RPC's implementation-defined range: -32000
   is `Authentication required`, -32002 is `Resource not found`, -32800 is
   `Request cancelled`. `:not-initialized` is ours alone, so it sits on -32001,
   which the spec leaves free — it used to squat on -32002 and told any client
   that special-cases the standard codes that a FILE was missing."
  {:parse-error -32700
   :invalid-request -32600
   :method-not-found -32601
   :invalid-params -32602
   :internal-error -32603
   :auth-required -32000
   :not-initialized -32001})

(def ^:dynamic *client-call-timeout-ms*
  "How long the agent waits for an editor to answer `fs/*` or
   `session/request_permission` before giving up. A hung editor must never hang
   the agent forever."
  120000)

(def ^:dynamic *permission-ops*
  "Tool ops routed through `session/request_permission` while an ACP editor owns
   the session."
  #{:write :patch :struct_patch :fs :shell})

(def ^:dynamic *mirror-ops*
  "Tool ops whose result is mirrored into the editor's buffer with
   `fs/write_text_file`, so the editor shows the change without a disk reload."
  #{:write :patch :struct_patch :format_code})

(def ^:dynamic *max-mirror-bytes*
  "Files larger than this are not pushed into an editor buffer."
  (* 2 1024 1024))

;; =============================================================================
;; Encoding — total, single-line, never throws mid-frame
;; =============================================================================

(def ^:private max-json-depth
  "Nesting past this is truncated instead of overflowing the stack. ACP is one
   message per LINE: a value that cannot be encoded must degrade, never throw."
  64)

(def ^:private max-json-items
  "Elements of ONE sequence that get encoded. Depth is not the only way to hang a
   writer: a LAZY sequence can be unbounded, and realizing it would spin until the
   daemon dies. Past this the tail becomes a marker."
  10000)

(defn json-safe
  "Total JSON projection of `x`: keywords/symbols become strings, map keys become
   strings, NaN/±Infinity become nil, anything unknown becomes its `str`, and
   anything nested past [[max-json-depth]] or longer than [[max-json-items]]
   becomes a marker string. ACP is one
   message per LINE, so a value that cannot be encoded must not be able to throw
   halfway through a write and desynchronize the framing."
  ([x] (json-safe x 0))
  ([x depth]
   (let [d (long depth)]
     (cond (nil? x) nil
           (string? x) x
           (boolean? x) x
           (keyword? x) (subs (str x) 1)
           (symbol? x) (str x)
           (and (double? x) (or (Double/isNaN ^double x) (Double/isInfinite ^double x))) nil
           (number? x) x
           ;; Depth guard sits AFTER the scalars so a leaf at the limit still
           ;; encodes exactly, and only containers are truncated.
           (>= d (long max-json-depth)) (str "<nesting deeper than " max-json-depth " truncated>")
           (map? x) (persistent! (reduce-kv (fn [m k v]
                                              (let [k' (json-safe k (inc d))]
                                                (assoc! m
                                                        (if (string? k') k' (str k'))
                                                        (json-safe v (inc d)))))
                                            (transient {})
                                            x))
           (instance? Map x) (json-safe (into {} ^Map x) d)
           (or (sequential? x) (set? x))
           ;; `take` one past the cap so an INFINITE lazy seq is never realized.
           (let [head (into [] (take (inc (long max-json-items))) x)]
             (if (> (count head) (long max-json-items))
               (conj (mapv #(json-safe % (inc d)) (subvec head 0 (long max-json-items)))
                     (str "<more than " max-json-items " items truncated>"))
               (mapv #(json-safe % (inc d)) head)))
           (instance? (Class/forName "[B") x) (str "<" (alength ^bytes x) " bytes>")
           :else (str x)))))

(defn encode
  "One newline-free JSON line for `msg`."
  ^String [msg]
  (json/write-json-str (json-safe msg)))

(defn- err-obj
  [kind message & [data]]
  (cond-> {"code" (get error-codes kind (:internal-error error-codes)) "message" (str message)}
    (some? data)
    (assoc "data" (json-safe data))))

(defn- fail!
  "Abort the current method with a JSON-RPC error of `kind`."
  [kind message & [data]]
  (throw (ex-info (str message)
                  (cond-> {:acp/kind kind}
                    (some? data)
                    (assoc :acp/data data)))))

(defn- response-message [id result] {"jsonrpc" "2.0" "id" id "result" (or result {})})

(defn- error-message [id error] {"jsonrpc" "2.0" "id" id "error" error})

(defn decode
  "Parse ONE framed line. Returns `{:msg m}`, or `{:error e}` with the JSON-RPC
   error object to answer with (id `null`)."
  [line]
  (let [line (str/trim (str line))]
    (if (str/blank? line)
      {:error (err-obj :invalid-request "empty message")}
      (let
        [parsed (try {:v (json/read-json line)}
                     (catch Throwable t {:bad (or (ex-message t) "invalid JSON")}))]
        (cond (contains? parsed :bad) {:error (err-obj :parse-error (:bad parsed))}
              (sequential? (:v parsed))
              {:error (err-obj :invalid-request "batched JSON-RPC is not supported by ACP")}
              (not (map? (:v parsed))) {:error (err-obj :invalid-request
                                                        "a JSON-RPC message must be an object")}
              :else {:msg (:v parsed)})))))

;; =============================================================================
;; Backends — the protocol never touches the gateway directly
;; =============================================================================

(defn- resolve+
  "`requiring-resolve` that reports a usable ACP error instead of a class-load
   stack trace when the daemon client is unavailable."
  [sym]
  (or (try (requiring-resolve sym) (catch Throwable _ nil))
      (fail! :internal-error (str "this build cannot resolve " sym))))

(defn- session-id-of
  "The session id inside a gateway create/soul reply, whatever it calls it."
  [m]
  (some #(let
           [v
            (get m %)]

           (when (and (string? v) (seq v)) v))
        ["id" "session_id" "sid" "soul_id"]))

(defn gateway-backend
  "The live backend: every ACP session is an ORDINARY vis gateway session, so the
   editor, the TUI, the web surface, and the phone all watch the same turns."
  []
  {:new-session (fn [{:keys [cwd]}]
                  (let
                    [created ((resolve+ 'com.blockether.vis.internal.gateway.client/create-session!)
                               {:cwd cwd :channel "acp"})]
                    (or (session-id-of created)
                        (fail! :internal-error "the gateway did not return a session id" created))))
   ;; nil when the daemon has no such session (`soul` answers nil on 404), so
   ;; `session/load` can refuse a stale id instead of resuming a phantom.
   :load-session (fn [{:keys [session-id]}]
                   (when ((resolve+ 'com.blockether.vis.internal.gateway.client/soul) session-id)
                     (let
                       [page ((resolve+ 'com.blockether.vis.internal.gateway.client/transcript-page)
                               session-id
                               {:limit 50})]
                       (vec (mapcat (fn [t]
                                      (keep (fn [[role k]]
                                              (let [v (get t k)]
                                                (when (and (string? v) (seq (str/trim v)))
                                                  {:role role :text v})))
                                            [["user" "request"] ["assistant" "answer"]]))
                                    (get page "turns"))))))
   :prompt (fn [{:keys [session-id text on-event]}]
             ((resolve+ 'com.blockether.vis.internal.gateway.client/submit-turn-sync!)
               session-id
               {:request text :on-event on-event}))
   :cancel (fn [{:keys [session-id]}]
             ((resolve+ 'com.blockether.vis.internal.gateway.client/cancel-current-turn!)
               session-id))})

(defn echo-backend
  "A dependency-free backend used by tests and by `--self-test`: it never starts a
   daemon and answers every prompt from `f` (default: echo the prompt text).

   `:load-session` answers `nil` for a session it never created, which is the
   backend contract for \"no such session\"."
  [& [f]]
  (let
    [n
     (atom 0)

     known
     (atom #{})]

    {:new-session (fn [_]
                    (let [sid (str "acp-echo-" (swap! n inc))]
                      (swap! known conj sid)
                      sid))
     :load-session (fn [{:keys [session-id]}]
                     (when (contains? @known session-id) []))
     :prompt (fn [{:keys [text on-event]}]
               (when on-event
                 (on-event {"type" "content.block.delta" "field" "markdown" "text" (str text)}))
               (if f (f text) {:status "completed" :answer (str text)}))
     :cancel (fn [_]
               true)}))

;; =============================================================================
;; Connection
;; =============================================================================

(defn connection
  "A live ACP connection. `:out-fn` is called with ONE encoded line per outgoing
   message and must be safe to call from several threads. `:half-duplex?` marks a
   transport that cannot carry agent→client REQUESTS (the HTTP route)."
  [{:keys [out-fn backend half-duplex?]}]
  (atom {:out-fn (or out-fn
                     (fn [_]))
         :backend (or backend (gateway-backend))
         :half-duplex? (boolean half-duplex?)
         :initialized? false
         :protocol-version nil
         :client-caps {}
         ;; sid → {:cwd :state :turn :cancel-mark}; a cancel is remembered
         ;; against the TURN it named, never against the session at large.
         :sessions {}
         :decisions {}
         :next-id 0
         :pending {}
         :closed? false}))

(defn- emit!
  [conn msg]
  (let [{:keys [out-fn closed?]} @conn]
    (when-not closed?
      (try (out-fn (encode msg))
           (catch Throwable t
             (swap! conn assoc :closed? true)
             (tel/log! :warn ["acp: outgoing write failed" (ex-message t)])))))
  msg)

(defn notify!
  "Send a client-bound NOTIFICATION (no id, no reply)."
  [conn method params]
  (emit! conn {"jsonrpc" "2.0" "method" (str method) "params" (or params {})})
  nil)

(defn call!
  "Send a client-bound REQUEST and block for the answer. Returns `{:result r}` or
   `{:error e}` — never throws, so a hostile/absent editor cannot break a tool."
  [conn method params & [{:keys [timeout-ms]}]]
  (if (:half-duplex? @conn)
    {:error (err-obj :method-not-found
                     (str "the HTTP ACP transport cannot issue client requests (" method ")"))}
    (let
      [id
       (str "vis-" (:next-id (swap! conn update :next-id inc)))

       p
       (promise)]

      (swap! conn assoc-in [:pending id] p)
      (emit! conn {"jsonrpc" "2.0" "id" id "method" (str method) "params" (or params {})})
      (let [v (deref p (or timeout-ms *client-call-timeout-ms*) ::timeout)]
        (swap! conn update :pending dissoc id)
        (if (= v ::timeout)
          {:error (err-obj :internal-error (str "the ACP client did not answer " method))}
          v)))))

(defn- deliver-response!
  [conn msg]
  (let [id (get msg "id")]
    (when-let [p (get-in @conn [:pending id])]
      (swap! conn update :pending dissoc id)
      (deliver
        p
        (if (contains? msg "error") {:error (get msg "error")} {:result (get msg "result")}))))
  nil)

(defn client-supports?
  "True when the client advertised the nested capability, e.g.
   `(client-supports? conn \"fs\" \"writeTextFile\")`."
  [conn & ks]
  (boolean (get-in (:client-caps @conn) (mapv str ks))))

;; =============================================================================
;; Content blocks
;; =============================================================================

(defn content-block->text
  "Flatten ONE ACP content block to the text vis puts in a turn request. Unknown
   or malformed blocks flatten to nil rather than exploding."
  [b]
  (when (map? b)
    (case (str (get b "type"))
      "text"
      (let [t (get b "text")]
        (when (string? t) t))

      "resource_link"
      (let [u (get b "uri")]
        (when (string? u) (str "@" u)))

      "resource"
      (let [r (get b "resource")]
        (when (map? r)
          (let
            [t (get r "text")
             u (get r "uri")]

            (cond (string? t) (if (string? u) (str "<" u ">\n" t) t)
                  (string? u) (str "@" u)))))

      "image"
      "[image]"

      "audio"
      "[audio]"

      nil)))

(defn prompt->text
  "The prompt array of an ACP `session/prompt` as one request string."
  [blocks]
  (when-not (sequential? blocks)
    (fail! :invalid-params "prompt must be an array of content blocks"))
  (->> blocks
       (map content-block->text)
       (remove str/blank?)
       (str/join "\n")))

;; =============================================================================
;; Gateway event → `session/update`
;; =============================================================================

(defn tool-kind
  "Map a vis tool name onto an ACP tool-call kind."
  [tool]
  (case (if (keyword? tool) (name tool) (str tool))
    ("cat" "ls" "struct_index" "struct_nodes")
    "read"

    ("write" "patch" "struct_patch" "format_code")
    "edit"

    ("grep" "apropos" "doc")
    "search"

    ("shell" "run_tests" "repl_eval" "repl" "python_execution")
    "execute"

    "fs"
    "move"

    "search"
    "fetch"

    "other"))

(defn event->update
  "Translate ONE gateway SSE event into the `update` payload of an ACP
   `session/update`, or nil when the event has no ACP meaning."
  [event]
  (when (map? event)
    (let [block (get event "block")]
      (case (str (get event "type"))
        "content.block.delta"
        (let [text (get event "text")]
          (when (and (string? text) (seq text))
            (case (str (get event "field"))
              "text"
              {"sessionUpdate" "agent_thought_chunk" "content" {"type" "text" "text" text}}

              "markdown"
              {"sessionUpdate" "agent_message_chunk" "content" {"type" "text" "text" text}}

              nil)))

        ("block.started" "block.preview")
        (when (and (map? block) (= "tool" (str (get block "type"))))
          {"sessionUpdate" "tool_call"
           "toolCallId" (str (get block "id"))
           "title" (str (get block "tool"))
           "kind" (tool-kind (get block "tool"))
           "status" (if (= "block.preview" (str (get event "type"))) "pending" "in_progress")})

        "block.output"
        (when (map? block)
          {"sessionUpdate" "tool_call_update"
           "toolCallId" (str (get block "id"))
           "status"
           (if (contains? #{"error" "failed"} (str (get block "status"))) "failed" "completed")})

        nil))))

;; =============================================================================
;; Method handlers
;; =============================================================================

(defn- require-initialized!
  [conn]
  (when-not (:initialized? @conn)
    (fail! :not-initialized "initialize must be called before this method")))

(defn- session-id!
  [params]
  (let [sid (get params "sessionId")]
    (when-not (and (string? sid) (seq sid))
      (fail! :invalid-params "sessionId (string) is required"))
    sid))

(defn- known-session!
  [conn sid]
  (when-not (contains? (:sessions @conn) sid)
    (fail! :invalid-params (str "unknown sessionId: " sid)))
  sid)

(def ^:private connections
  "vis session id → the ACP connection that owns it, so tool op-hooks can find
   the editor to ask."
  (atom {}))

(defn register-connection! [sid conn] (swap! connections assoc sid conn) conn)
(defn unregister-connection! [sid] (swap! connections dissoc sid) nil)
(defn connection-for [sid] (get @connections sid))

(defn- handle-initialize
  [conn params]
  (let
    [v
     (get params "protocolVersion")

     caps
     (get params "clientCapabilities")]

    (when-not (or (nil? v) (integer? v))
      (fail! :invalid-params "protocolVersion must be an integer"))
    (when-not (or (nil? caps) (map? caps))
      (fail! :invalid-params "clientCapabilities must be an object"))
    (let [negotiated (if (contains? supported-protocol-versions v) (long v) protocol-version)]
      (swap! conn assoc :initialized? true :protocol-version negotiated :client-caps (or caps {}))
      {"protocolVersion" negotiated
       "agentInfo" agent-info
       "agentCapabilities"
       ;; `image`/`audio` stay FALSE while [[content-block->text]] flattens a
       ;; non-text block to "[image]" and drops the bytes: advertising them only
       ;; makes clients ship payloads the model never sees.
       {"loadSession" true
        "promptCapabilities" {"image" false "audio" false "embeddedContext" true}
        ;; stdio and streamable HTTP are what `mcp/client` actually speaks; SSE
        ;; is not implemented, and advertising it would only earn us servers we
        ;; cannot reach.
        "mcpCapabilities" {"http" true "sse" false}}
       "authMethods" []})))

(defn- handle-authenticate
  [_conn params]
  (let [m (get params "methodId")]
    ;; `authMethods` is empty: a local editor spawning the agent already has the
    ;; user's trust. Naming a method we never advertised is a client bug.
    (when (and (some? m) (not= "" m)) (fail! :invalid-params (str "unknown auth method: " m)))
    {}))

(defn- mcp-kv-pairs
  "ACP ships env vars and HTTP headers as `[{\"name\" … \"value\" …}]`; the MCP
   client wants a map. A malformed entry is REFUSED rather than skipped —
   silently dropping one is how a server ends up unauthenticated for a whole
   session."
  [what v]
  (when-not (or (nil? v) (sequential? v))
    (fail! :invalid-params (str what " must be an array of {name, value} objects")))
  (reduce (fn [m e]
            (let
              [n
               (get e "name")

               val
               (get e "value")]

              (when-not (and (map? e) (string? n) (seq n) (string? val))
                (fail! :invalid-params
                       (str what " entries must be objects with a string name and a string value")))
              (assoc m n val)))
          {}
          v))

(defn- mcp-strings
  [what v]
  (when-not (or (nil? v) (sequential? v))
    (fail! :invalid-params (str what " must be an array of strings")))
  (mapv (fn [s]
          (when-not (string? s) (fail! :invalid-params (str what " must be an array of strings")))
          s)
        v))

(defn- mcp-server->spec
  "One ACP `McpServer` → `[name raw-spec]`, the raw spec shaped exactly like an
   `:mcp :servers` config entry, so a CLIENT's server is coerced, interpolated
   and connected by the same code path as a configured one.

   `sse` is refused instead of quietly downgraded to HTTP: we advertise
   `mcpCapabilities.sse false`, and a client that ignores that must hear about it
   rather than watch its server never answer."
  [srv]
  (when-not (map? srv) (fail! :invalid-params "each mcpServers entry must be an object"))
  (let
    [nm
     (get srv "name")

     t
     (get srv "type")]

    (when-not (and (string? nm) (seq nm))
      (fail! :invalid-params "mcpServers[].name must be a non-empty string"))
    (cond (= "sse" t) (fail! :invalid-params
                             (str "MCP server " nm
                                  ": the sse transport is not supported "
                                  "(agentCapabilities.mcpCapabilities.sse is false)"))
          (= "http" t)
          (let [url (get srv "url")]
            (when-not (and (string? url) (seq url))
              (fail! :invalid-params (str "MCP server " nm ": url must be a non-empty string")))
            [nm
             {"transport" "streamable_http"
              "url" url
              "headers" (mcp-kv-pairs (str "MCP server " nm ": headers") (get srv "headers"))}])
          (or (nil? t) (= "stdio" t))
          (let [cmd (get srv "command")]
            (when-not (and (string? cmd) (seq cmd))
              (fail! :invalid-params (str "MCP server " nm ": command must be a non-empty string")))
            [nm
             {"transport" "stdio"
              "command" cmd
              "args" (mcp-strings (str "MCP server " nm ": args") (get srv "args"))
              "env" (mcp-kv-pairs (str "MCP server " nm ": env") (get srv "env"))}])
          :else (fail! :invalid-params
                       (str "MCP server " nm ": unknown transport type " (pr-str t))))))

(defn- attach-mcp-servers!
  "Attach one request's `mcpServers` to `sid`, session-scoped, connected EAGERLY.

   Ignoring `mcpServers` — which this agent used to do — is the worst failure
   mode on offer: the editor believes the session can reach its servers, the
   model never sees a single one of their tools, and nothing anywhere says so. A
   server that cannot be reached therefore FAILS the `session/new` /
   `session/load` that asked for it, and leaves nothing half-attached."
  [sid params]
  (let [raw (get params "mcpServers")]
    (when-not (or (nil? raw) (sequential? raw))
      (fail! :invalid-params "mcpServers must be an array"))
    (let
      [specs (reduce (fn [m srv]
                       (let [[nm spec] (mcp-server->spec srv)]
                         (when (contains? m nm)
                           (fail! :invalid-params (str "duplicate MCP server name: " nm)))
                         (assoc m nm spec)))
                     {}
                     raw)]
      ;; A reused session id must not inherit the servers of its previous life.
      (if (empty? specs)
        (mcp/clear-session-servers! sid)
        (let [failed (get (mcp/set-session-servers! sid specs) "failed")]
          (when (seq failed)
            (mcp/clear-session-servers! sid)
            (fail! :internal-error
                   (str "could not connect to MCP server(s): "
                        (str/join ", "
                                  (map (fn [row]
                                         (str (get row "server") ": " (get row "error")))
                                       failed)))
                   (json-safe {"failedMcpServers" (mapv (fn [row]
                                                          {"name" (get row "server")
                                                           "error" (str (get row "error"))})
                                                        failed)}))))))
    nil))

(defn- handle-session-new
  [conn params]
  (require-initialized! conn)
  (let [cwd (get params "cwd")]
    (when-not (and (string? cwd) (seq cwd))
      (fail! :invalid-params "cwd (absolute path) is required"))
    (when-not (.isAbsolute (io/file ^String cwd))
      (fail! :invalid-params "cwd must be an absolute path"))
    (let [sid (str ((:new-session (:backend @conn)) {:cwd cwd}))]
      (when (str/blank? sid) (fail! :internal-error "the agent could not create a session"))
      ;; MCP BEFORE registration: a session whose declared servers are missing is
      ;; not the session the client asked for, so it is never handed back as if
      ;; it were.
      (attach-mcp-servers! sid params)
      (swap! conn assoc-in [:sessions sid] {:cwd cwd :state :idle})
      (register-connection! sid conn)
      {"sessionId" sid})))

(defn- handle-session-load
  [conn params]
  (require-initialized! conn)
  (let
    [sid
     (session-id! params)

     cwd
     (get params "cwd")]

    ;; `session/new` insists on an absolute cwd; a resumed session must not be
    ;; the lax door into the same state.
    (when (and (some? cwd) (not (and (string? cwd) (seq cwd) (.isAbsolute (io/file ^String cwd)))))
      (fail! :invalid-params "cwd must be an absolute path"))
    (let [turns ((:load-session (:backend @conn)) {:session-id sid :cwd cwd})]
      ;; nil = the backend has never heard of this session. Registering it anyway
      ;; hands the editor a PHANTOM: `session/load` says ok, the transcript is
      ;; empty, every later prompt fails deep inside the turn, and the id lingers
      ;; in the global connection registry.
      (when (nil? turns) (fail! :invalid-params (str "unknown sessionId: " sid)))
      (attach-mcp-servers! sid params)
      (swap! conn assoc-in [:sessions sid] {:cwd cwd :state :idle})
      (register-connection! sid conn)
      (doseq
        [t turns
         :let [text (str (:text t))]
         :when (not (str/blank? text))]

        (notify! conn
                 "session/update"
                 {"sessionId" sid
                  "update" {"sessionUpdate" (if (= "user" (str (:role t)))
                                              "user_message_chunk"
                                              "agent_message_chunk")
                            "content" {"type" "text" "text" text}}}))
      {})))

(def ^:private max-tokens-error-re
  "vis' loop reports an output-budget death as a FAILED turn whose message names
   `max_tokens` / `max_output_tokens`. ACP has a stop reason for exactly that,
   and an editor renders it as \"the model ran out of room\" — not as \"the agent
   crashed\", which is what a JSON-RPC error would say."
  #"(?i)max[_ -]?(output[_ -]?)?tokens|output budget was exhausted")

(defn- stop-reason
  [result cancelled?]
  (let
    [status
     (str (or (:status result) (get result "status")))

     error
     (str (or (:error result) (get result "error") ""))

     failed?
     (contains? #{"failed" "error"} status)]

    (cond cancelled? "cancelled"
          (= "cancelled" status) "cancelled"
          (and failed? (re-find max-tokens-error-re error)) "max_tokens"
          failed? (fail! :internal-error
                         (or (:error result) (get result "error") "the turn failed")
                         (json-safe result))
          :else "end_turn")))

(def ^:private ^:dynamic *turn-ticket*
  "The turn number `serve!` issued for the prompt line running on THIS thread.
   Bound per worker so a handler can tell its own turn from the next one."
  nil)

(defn- mark-pending!
  "Issue this prompt's TURN NUMBER and, when the session is idle, park it at
   `:pending`.

   `serve!` calls this the moment it READS a prompt line, before handing the turn
   to a worker: a `session/cancel` that follows on the wire would otherwise be
   dropped for a session that is not `:running` yet, and the user's escape key
   would do nothing.

   The number is what makes a cancel precise. Turn numbers only ever grow, so
   `session/cancel` can mark everything issued so far as cancelled and still be
   unable to touch a prompt that arrives after it — even while an earlier,
   malformed prompt for the same session is still being rejected."
  [conn sid]
  (when (string? sid)
    (let
      [[_ after] (swap-vals! conn
                             (fn [s]
                               (if (contains? (:sessions s) sid)
                                 (-> s
                                     (update-in [:sessions sid :turn] (fnil inc 0))
                                     (update-in [:sessions sid :state]
                                                #(if (= :idle %) :pending %)))
                                 s)))]
      (get-in after [:sessions sid :turn]))))

(defn- release-pending!
  "Undo [[mark-pending!]] for a prompt that never reached its claim. Only touches
   `:pending`, so it can never free a turn some other thread is running. A cancel
   already recorded against that dead turn stays recorded against IT: the number
   never matches a later turn, so nothing has to be cleaned up here."
  [conn sid]
  (swap! conn (fn [s]
                (if (= :pending (get-in s [:sessions sid :state]))
                  (assoc-in s [:sessions sid :state] :idle)
                  s)))
  nil)

(defn- handle-session-prompt
  [conn params]
  (require-initialized! conn)
  (let
    [sid
     (known-session! conn (session-id! params))

     ;; `serve!` already issued this line's turn number, in wire order, and parked
     ;; the session at `:pending`. The HTTP transport calls this handler directly,
     ;; so there the turn takes its own number here.
     turn
     (or *turn-ticket* (mark-pending! conn sid))

     ;; Cancelled means "a `session/cancel` arrived while THIS turn was in flight".
     ;; A per-session flag could not tell one turn from the next: a cancel recorded
     ;; against a prompt that was still being rejected used to abort the innocent
     ;; prompt that followed it.
     cancelled?
     (fn []
       (<= (long turn) (long (or (get-in @conn [:sessions sid :cancel-mark]) 0))))

     text
     ;; EVERY exit from validation has to undo the `:pending` park, or the session
     ;; stays parked and keeps accepting cancels for a turn that will never run.
     (try (let [t (prompt->text (get params "prompt"))]
            (when (str/blank? t)
              (fail! :invalid-params "prompt must carry at least one non-empty content block"))
            t)
          (catch Throwable t (release-pending! conn sid) (throw t)))]

    ;; Claim the session ATOMICALLY. `serve!` hands prompts to their own virtual
    ;; thread, so a read-then-write guard lets two prompts own one session at
    ;; once: their `session/update` streams interleave and whichever finishes
    ;; first marks the session idle while the other is still streaming.
    (let
      [[before] (swap-vals! conn
                            (fn [s]
                              (if (= :running (get-in s [:sessions sid :state]))
                                s
                                (assoc-in s [:sessions sid :state] :running))))]
      (when (= :running (get-in before [:sessions sid :state]))
        (fail! :invalid-request (str "a prompt is already running for session " sid))))
    (try (let
           [result ((:prompt (:backend @conn))
                     {:session-id sid
                      :text text
                      :on-event (fn [event]
                                  (when-let [u (event->update event)]
                                    (notify! conn "session/update" {"sessionId" sid "update" u})))
                      :cancelled? cancelled?})]
           {"stopReason" (stop-reason result (cancelled?))})
         (catch Throwable t
           ;; ACP requires `cancelled` for a turn the client cancelled EVEN IF the
           ;; cancellation tears the underlying operation down with an exception —
           ;; and it does: cancelling drops the gateway's SSE stream, so the read
           ;; loop throws instead of returning a terminal event. Answering
           ;; `-32603` would show the user a hard failure for pressing escape.
           (if (cancelled?) {"stopReason" "cancelled"} (throw t)))
         (finally (swap! conn assoc-in [:sessions sid :state] :idle)))))

(defn- handle-session-cancel
  [conn params]
  (let
    [sid
     (session-id! params)

     ;; `session/cancel` is a NOTIFICATION: any client can send any id at any time
     ;; and never sees an answer. Recording ids we never handed out would let a
     ;; buggy or hostile editor grow this map without bound, and recording a cancel
     ;; for a session with no turn in flight would abort the NEXT prompt instead —
     ;; a turn the user never asked to stop. So a cancel only lands on a session
     ;; with a turn in flight, and what it writes is a WATERMARK: every turn issued
     ;; so far is cancelled — the spec cancels the session's ongoing operations —
     ;; and every turn issued after it is untouched.
     [before]
     (swap-vals! conn
                 (fn [s]
                   (let [{:keys [state turn]} (get-in s [:sessions sid])]
                     (if (contains? #{:pending :running} state)
                       (assoc-in s [:sessions sid :cancel-mark] turn)
                       s))))]

    (when (contains? #{:pending :running} (get-in before [:sessions sid :state]))
      (try ((:cancel (:backend @conn)) {:session-id sid})
           (catch Throwable t (tel/log! :warn ["acp: cancel failed" (ex-message t)]))))
    {}))

(def handlers
  "ACP method → handler `(fn [conn params])`."
  {"initialize" handle-initialize
   "authenticate" handle-authenticate
   "session/new" handle-session-new
   "session/load" handle-session-load
   "session/prompt" handle-session-prompt
   "session/cancel" handle-session-cancel})

;; =============================================================================
;; Dispatch — every hostile shape answered, never thrown
;; =============================================================================

(defn- valid-id?
  "True for a JSON-RPC id this agent can echo back verbatim.

   ACP's `RequestId` is `null | i64 | string`. A fractional or oversized number
   is therefore NOT a usable id: echoing `1.5` — or 2^63 — into the response
   makes the whole frame undeserializable for a typed client, which drops the
   connection instead of surfacing our error. Reject it and answer with id null."
  [x]
  (or (nil? x)
      (string? x)
      (and (integer? x) (<= (bigint Long/MIN_VALUE) (bigint x) (bigint Long/MAX_VALUE)))))

(defn- error-of
  "The JSON-RPC error object for a Throwable raised inside a handler."
  [^Throwable t]
  (let [d (ex-data t)]
    (if (:acp/kind d)
      (err-obj (:acp/kind d) (ex-message t) (:acp/data d))
      (err-obj :internal-error (or (ex-message t) (str (class t)))))))

(defn- dispatch!
  [conn method params]
  (let [h (get handlers method)]
    (when-not h (fail! :method-not-found (str "unknown method: " method)))
    (when-not (or (nil? params) (map? params)) (fail! :invalid-params "params must be an object"))
    (h conn (or params {}))))

(defn handle-line!
  "Process ONE framed message. Emits the response through the connection's
   `out-fn` and returns it, or nil for a notification / a client answer.

   This never throws: malformed input becomes a JSON-RPC error message."
  [conn line]
  (let [{:keys [msg error]} (decode line)]
    (cond error (emit! conn (error-message nil error))
          :else
          (let
            [id (get msg "id")
             has-id? (contains? msg "id")
             method (get msg "method")
             jsonrpc (get msg "jsonrpc")]

            (cond (and (some? jsonrpc) (not= "2.0" jsonrpc))
                  (emit! conn
                         (error-message (when (valid-id? id) id)
                                        (err-obj :invalid-request "jsonrpc must be \"2.0\"")))
                  (and (nil? method) (or (contains? msg "result") (contains? msg "error")))
                  (deliver-response! conn msg)
                  (not (string? method)) (emit! conn
                                                (error-message (when (valid-id? id) id)
                                                               (err-obj :invalid-request
                                                                        "method must be a string")))
                  (not has-id?)
                  (do (try (dispatch! conn method (get msg "params"))
                           (catch Throwable t
                             (tel/log! :debug ["acp: notification failed" method (ex-message t)])))
                      nil)
                  (not (valid-id? id))
                  (emit! conn
                         (error-message
                           nil
                           (err-obj :invalid-request
                                    "id must be null, a string, or an integer that fits i64")))
                  :else
                  (try (emit! conn (response-message id (dispatch! conn method (get msg "params"))))
                       (catch Throwable t (emit! conn (error-message id (error-of t))))))))))

;; =============================================================================
;; Client-bound calls: permission + the editor's filesystem
;; =============================================================================

(def default-permission-options
  [{"optionId" "allow-once" "name" "Allow once" "kind" "allow_once"}
   {"optionId" "allow-always" "name" "Allow always" "kind" "allow_always"}
   {"optionId" "reject-once" "name" "Reject" "kind" "reject_once"}
   {"optionId" "reject-always" "name" "Reject always" "kind" "reject_always"}])

(defn request-permission!
  "Ask the editor to approve `tool-call`. Returns `:allow-once`, `:allow-always`,
   `:reject-once`, `:reject-always`, or `:cancelled`.

   FAILS CLOSED: a timeout, a transport error, or an option id we never offered
   is a rejection, never an approval."
  [conn session-id tool-call & [options]]
  (let
    [{:keys [result error]} (call! conn
                                   "session/request_permission"
                                   {"sessionId" session-id
                                    "toolCall" tool-call
                                    "options" (or options default-permission-options)})]
    (if error
      :cancelled
      (let [outcome (get result "outcome")]
        (if (= "selected" (str (get outcome "outcome")))
          (case (str (get outcome "optionId"))
            "allow-once"
            :allow-once

            "allow-always"
            :allow-always

            "reject-once"
            :reject-once

            "reject-always"
            :reject-always

            :reject-once)
          :cancelled)))))

(defn read-text-file!
  "The editor's CURRENT buffer for `path` — unsaved edits included — when the
   client advertises `fs.readTextFile`; the bytes on disk otherwise."
  [conn session-id path & [{:keys [line limit]}]]
  (if (and conn (client-supports? conn "fs" "readTextFile"))
    (let
      [{:keys [result error]} (call! conn
                                     "fs/read_text_file"
                                     (cond-> {"sessionId" session-id "path" (str path)}
                                       (some? line)
                                       (assoc "line" line)

                                       (some? limit)
                                       (assoc "limit" limit)))]
      (if error
        (throw (ex-info (str "fs/read_text_file failed: " (get error "message"))
                        {:acp/error error :path (str path)}))
        (str (get result "content"))))
    (slurp (io/file (str path)))))

(defn write-text-file!
  "Push `content` into the editor's buffer for `path`. Returns true when the
   editor took it, false when the client has no `fs.writeTextFile`."
  [conn session-id path content]
  (if (and conn (client-supports? conn "fs" "writeTextFile"))
    (let
      [{:keys [error]} (call! conn
                              "fs/write_text_file"
                              {"sessionId" session-id "path" (str path) "content" (str content)})]
      (if error
        (throw (ex-info (str "fs/write_text_file failed: " (get error "message"))
                        {:acp/error error :path (str path)}))
        true))
    false))

;; =============================================================================
;; Op-hook: the editor approves, then sees, every mutation
;; =============================================================================

(defn arg-paths
  "Every `:path`/`\"path\"` value anywhere inside a tool's arguments.

   The walk is ITERATIVE on purpose. Arguments are untrusted JSON that an editor,
   a model, or an extension can nest arbitrarily deep, and this runs inside the
   permission/mirror hook: a recursive walk answers a deep value with a
   `StackOverflowError`, which is an `Error` no handler catches — it kills the
   turn instead of degrading the way [[json-safe]] does."
  [x]
  (let
    [path-key?
     #{:path "path" :dest "dest" :src "src"}

     hit?
     (fn [k v]
       (and (path-key? k) (string? v) (seq v)))]

    (loop
      [stack
       [x]

       acc
       []]

      (if-let [v (peek stack)]
        (let [stack (pop stack)]
          (cond (map? v) (let
                           [hits (reduce-kv (fn [a k vv]
                                              (if (hit? k vv) (conj a vv) a))
                                            []
                                            v)
                            kids (reduce-kv (fn [a k vv]
                                              (if (hit? k vv) a (conj a vv)))
                                            []
                                            v)]

                           ;; children go on REVERSED so the stack pops them in source order:
                           ;; a deep `first` stays the same path it has always been.
                           (recur (into stack (reverse kids)) (into acc hits)))
                (sequential? v) (recur (into stack (reverse v)) acc)
                :else (recur stack acc)))
        (if (seq stack)
          ;; a nil child: `peek` cannot tell it from an empty stack.
          (recur (pop stack) acc)
          (vec (distinct acc)))))))

(defn- mirror-to-editor!
  "After a successful edit, push each touched file's new content into the
   editor's buffer. Never allowed to break the tool."
  [conn sid args]
  (try (when (client-supports? conn "fs" "writeTextFile")
         (doseq
           [p
            (arg-paths args)

            :let [f
                  (io/file ^String p)]
            :when (and (.isFile f) (< (.length f) (long *max-mirror-bytes*)))]

           (write-text-file! conn sid p (slurp f))))
       (catch Throwable t (tel/log! :debug ["acp: buffer mirror failed" (ex-message t)])))
  nil)

(defn around-hook
  "`:around` op-hook. Outside an ACP session this is a pass-through. Inside one,
   a mutating op is approved by the editor first (`allow always` remembered per
   session+op) and its result is mirrored into the editor's buffers."
  [env op-kw args next-fn]
  (let
    [sid
     (:session-id env)

     conn
     (connection-for sid)]

    (if-not (and conn (not (:half-duplex? @conn)))
      (next-fn args)
      (do (when (contains? *permission-ops* op-kw)
            (let [remembered (get-in @conn [:decisions sid op-kw])]
              (when (= :reject-always remembered)
                (throw (ex-info (str "the editor has rejected `" (name op-kw) "` for this session")
                                {:acp/denied true :op op-kw})))
              (when-not (= :allow-always remembered)
                (let
                  [decision (request-permission! conn
                                                 sid
                                                 {"toolCallId" (str "op-" (System/nanoTime))
                                                  "title" (str (name op-kw)
                                                               (when-let
                                                                 [p (first (arg-paths args))]
                                                                 (str " " p)))
                                                  "kind" (tool-kind (name op-kw))
                                                  "status" "pending"
                                                  "rawInput" (json-safe args)})]
                  (when (contains? #{:allow-always :reject-always} decision)
                    (swap! conn assoc-in [:decisions sid op-kw] decision))
                  (when-not (contains? #{:allow-once :allow-always} decision)
                    (throw (ex-info (str "the editor denied `" (name op-kw) "`")
                                    {:acp/denied true :op op-kw :acp/decision decision})))))))
          (let [result (next-fn args)]
            (when (contains? *mirror-ops* op-kw) (mirror-to-editor! conn sid args))
            result)))))

(def op-hooks
  (mapv (fn [op]
          {:op op :phase :around :fn around-hook})
        (distinct (concat *permission-ops* *mirror-ops*))))

;; =============================================================================
;; stdio transport — the one the ACP spec requires
;; =============================================================================

(defn serve!
  "Run the ACP agent loop over `:in`/`:out` (default stdin/stdout) until EOF.
   Lines are handled IN ORDER, except `session/prompt`, which runs on its own
   virtual thread so `session/cancel` lands while the turn is still streaming.
   Returns the closed connection."
  [& [{:keys [in out backend]}]]
  (let
    [^InputStream in
     (or in System/in)

     ^Writer w
     (if (instance? Writer out)
       out
       (let
         [^OutputStream os
          (or out System/out)

          ^java.nio.charset.Charset cs
          StandardCharsets/UTF_8]

         (OutputStreamWriter. os cs)))

     lock
     (Object.)

     conn
     (connection {:backend backend
                  :out-fn (fn [^String line]
                            (locking lock (.write w line) (.write w "\n") (.flush w)))})

     ^ExecutorService pool
     (Executors/newVirtualThreadPerTaskExecutor)

     ^BufferedReader rdr
     (BufferedReader. (InputStreamReader. in StandardCharsets/UTF_8))]

    (try
      (loop []

        (when-let [line (.readLine rdr)]
          (when-not (str/blank? line)
            ;; A stream has an ORDER and a client may pipeline: handing every
            ;; line to its own thread lets `session/new` overtake the
            ;; `initialize` it depends on, or one `session/prompt` overtake
            ;; the `session/load` that has to precede it. Only a prompt runs
            ;; long enough to deserve a thread — and that is exactly what
            ;; keeps `session/cancel` answerable mid-turn.
            (let [m (:msg (decode line))]
              (if (= "session/prompt" (get m "method"))
                ;; The ticket travels WITH the line: the worker that runs this
                ;; prompt must know which turn it is, or a cancel meant for the
                ;; prompt before it lands on this one.
                (let [ticket (mark-pending! conn (get-in m ["params" "sessionId"]))]
                  (.submit pool
                           ^Runnable
                           (fn []
                             (binding [*turn-ticket* ticket]
                               (handle-line! conn line)))))
                (handle-line! conn line))))
          (recur)))
      (finally (.shutdown pool)
               (try (.awaitTermination pool 5 TimeUnit/SECONDS) (catch InterruptedException _ nil))
               (swap! conn assoc :closed? true)
               (doseq [sid (keys (:sessions @conn))]
                 (unregister-connection! sid)
                 ;; Session-scoped MCP servers belong to the CLIENT, not to
                 ;; the machine: a stdio server would otherwise outlive the
                 ;; editor that asked for it.
                 (mcp/clear-session-servers! sid))))
    conn))

;; =============================================================================
;; HTTP transport — ACP on the REGULAR gateway daemon
;; =============================================================================

(def ^:private max-http-connections
  "Distinct `?client=` ids kept alive. Beyond this the least recently used is
   evicted, so an unbounded stream of client ids cannot grow the table forever."
  64)

(def ^:private max-http-backlog
  "Out-of-band lines parked for a client that has no request in flight. A client
   that stops polling must cost a bounded amount of heap, so the oldest are
   dropped rather than kept forever."
  256)

(def ^:private ^:dynamic *outbox*
  "Per-REQUEST sink for the half-duplex HTTP transport, bound by [[acp-handler]].
   Two concurrent requests sharing ONE client id must not be able to steal or
   erase each other's replies, so the reply buffer belongs to the request and
   not to the connection."
  nil)

(def ^:private http-connections
  "client id → `{:conn <connection> :used <nanos>}`, LRU-capped."
  (atom {}))

(defn- http-connection
  "The half-duplex connection for one HTTP client id, created on first use and
   kept only while it stays among the [[max-http-connections]] most recent."
  [client-id]
  (let
    [fresh
     (delay
       (let
         [box
          (atom [])

          c
          (connection {:half-duplex? true
                       :out-fn (fn [line]
                                 (if-let [ob *outbox*]
                                   (swap! ob conj line)
                                   ;; Nothing in flight: park it
                                   ;; for the next poll, bounded.
                                   (swap! box (fn [v]
                                                (let
                                                  [v' (conj v line)
                                                   n (count v')]

                                                  (if (> n (long max-http-backlog))
                                                    (subvec v' (- n (long max-http-backlog)))
                                                    v'))))))})]

         (swap! c assoc :outbox box)
         c))

     [old m]
     (swap-vals! http-connections
                 (fn [m]
                   (let
                     [c
                      (or (:conn (get m client-id)) @fresh)

                      m'
                      (assoc m client-id {:conn c :used (System/nanoTime)})]

                     (if (<= (count m') (long max-http-connections))
                       m'
                       (dissoc m'
                         (->> m'
                              (sort-by #(:used (val %)))
                              ffirst))))))]

    ;; Evicting the connection is not enough: its sessions stay in the GLOBAL
    ;; registry, so the map grows for the life of the daemon and op-hooks keep
    ;; resolving vis sessions to a connection nobody can answer on.
    (doseq
      [[cid entry]
       old

       :when (not (contains? m cid))
       :let [c
             (:conn entry)]
       :when c]

      (swap! c assoc :closed? true)
      (doseq [sid (keys (:sessions @c))]
        (unregister-connection! sid)))
    (:conn (get m client-id))))

(defn- json-response
  [status body]
  {:status status
   :headers {"Content-Type" "application/json; charset=utf-8"}
   :body (json/write-json-str (json-safe body))})

(defn- acp-handler
  [request]
  (let
    [client-id
     (or (get-in request [:query-params "client"])
         (get-in request [:headers "x-acp-client"])
         "default")

     body
     (try (slurp (:body request)) (catch Throwable _ ""))

     conn
     (http-connection (str client-id))

     ;; This request's OWN buffer: concurrent requests on one client id each get
     ;; exactly the messages their own line produced.
     box
     (atom [])]

    (binding [*outbox* box]
      (handle-line! conn body))
    ;; Backlog FIRST: notifications the engine emitted between polls are older
    ;; than this request's own replies, and draining them here is what keeps the
    ;; parked buffer from growing forever.
    (let [parked (first (reset-vals! (:outbox @conn) []))]
      (json-response 200
                     {"client" (str client-id)
                      "messages" (mapv #(json/read-json %) (into parked @box))}))))

(defn routes-contribution
  []
  {:prefix "/v1/acp"
   :rev (str (System/identityHashCode #'routes-contribution))
   :routes (fn [_token]
             ["/v1/acp" {:post acp-handler}])})

;; =============================================================================
;; Slash command
;; =============================================================================

(defn- handle-acp
  "`/acp` reports whether an editor is driving this session over ACP."
  [ctx]
  (let
    [sid
     (or (:session/id ctx) (:session-id ctx))

     conn
     (connection-for sid)]

    {:slash/status :ok
     :slash/title (if conn "ACP: an editor is attached" "ACP: no editor attached")
     :slash/body (if-not conn
                   (str "Start vis as an ACP agent from your editor:\n" "  command: vis acp\n"
                        "  protocol: JSON-RPC 2.0 over stdio, ACP v" protocol-version
                        "\n" "Or POST one JSON-RPC message per request to /v1/acp on the gateway.")
                   (str/join
                     "\n"
                     [(str "protocol version   " (:protocol-version @conn))
                      (str "transport          "
                           (if (:half-duplex? @conn) "http (half duplex)" "stdio"))
                      (str "editor filesystem  read=" (client-supports? conn "fs" "readTextFile")
                           " write=" (client-supports? conn "fs" "writeTextFile"))
                      (str "sessions           " (str/join ", " (keys (:sessions @conn))))
                      (str "remembered grants  " (pr-str (get-in @conn [:decisions sid])))]))}))

(def slash-specs
  [{:slash/name "acp"
    :slash/doc "Show the Agent Client Protocol (editor) attachment for this session."
    :slash/usage "/acp"
    :slash/requires #{:session}
    :slash/run-fn handle-acp}])

;; =============================================================================
;; Registration
;; =============================================================================

(def vis-extension
  (vis/extension {:ext/name "acp"
                  :ext/description
                  (str "Agent Client Protocol server: editors (Zed, Neovim, JetBrains) drive vis "
                       "over JSON-RPC 2.0 — on stdio via `vis acp`, or on the regular gateway "
                       "daemon at POST /v1/acp, so the same session stays visible in the TUI, "
                       "the web surface, and the phone.")
                  :ext/kind "foundation"
                  :ext/op-hooks op-hooks
                  :ext/slash-commands slash-specs
                  :ext/channel-contributions {:gateway.slot/http-routes
                                              [{:id :acp/http :fn routes-contribution}]}}))

(vis/register-extension! vis-extension)
