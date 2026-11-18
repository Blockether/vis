(ns com.blockether.vis.internal.foundation.mcp.serve
  "`vis-agent mcp serve`: Vis as an MCP server on stdio, like `claude mcp serve`.

   Each line on the input is one JSON-RPC 2.0 message, and each answer is one line
   on the output. The caller gives the tools, which are the tools that a Vis agent
   has. A tool call runs in one Vis session for the
   workspace of `:cwd`. The first call creates the session, and later calls use it
   again, so the variables of a block stay for the next block. The gateway runs each
   block (`POST /v1/sessions/:sid/blocks`), and no model is called.

   The output carries only protocol messages. Logs go to stderr."
  (:require [charred.api :as json]
            [clojure.string :as str]
            [com.blockether.vis.internal.config.runtime-settings :as rt]
            [com.blockether.vis.internal.gateway.client :as gateway-client]
            [com.blockether.vis.internal.gateway.runtime :as gateway-runtime])
  (:import [java.io BufferedReader InputStream InputStreamReader OutputStream OutputStreamWriter
            Writer]
           [java.nio.charset StandardCharsets]))

(def ^:private protocol-versions
  "The MCP revisions that this server speaks, newest first. `initialize` answers
   the revision of the client when it is here, else the newest."
  ["2025-11-25" "2025-06-18" "2025-03-26" "2024-11-05"])

(def ^:private block-timeout-ms
  "How long one block request may take: the longest block time and one minute for
   the gateway."
  (+ (long rt/MAX_EVAL_TIMEOUT_MS) 60000))

(defn- mcp-tools
  "Tools `tools` (maps with `:name`, `:description` and `:schema`) as MCP tool
   records."
  [tools]
  (mapv (fn [{:keys [name description schema]}]
          {"name" name "description" description "inputSchema" schema})
        tools))

(defn- tool-result
  "An MCP tool result with one text item."
  [text error?]
  {"content" [{"type" "text" "text" (str text)}] "isError" (boolean error?)})

(defn- answer-body
  "The JSON body of gateway answer `response`, or nil."
  [response]
  (try (json/read-json (str (:body response))) (catch Exception _ nil)))

(defn- gateway-failure
  "The text of a failed gateway answer: its error message, else its status."
  [response]
  (or (get-in (answer-body response) ["error" "message"])
      (str "the gateway answered HTTP " (:status response))))

(defn- create-session!
  "Create the Vis session for workspace `cwd` and answer its id."
  [cwd]
  (let [response
        (gateway-client/request! :post "/v1/sessions" {:body {"root" cwd "title" "MCP client"}})]
    (if (<= 200 (long (:status response)) 299)
      (get (answer-body response) "id")
      (throw (ex-info (str "Vis could not create a session: " (gateway-failure response))
                      {:type :mcp-serve/session-failed})))))

(defn- run-block!
  "Run `code` in session `sid` and answer the gateway response."
  [sid code]
  (gateway-client/request! :post
                           (str "/v1/sessions/" sid "/blocks")
                           {:body {"code" code} :timeout-ms block-timeout-ms}))

(defn- python-execution!
  "Run one `python_execution` call in the session of `state`. Create the session on
   the first call, and again when the gateway does not know it any more."
  [state code]
  (let [sid-of
        #(or (:sid @state) (:sid (swap! state assoc :sid (create-session! (:cwd @state)))))

        response
        (let [first-try (run-block! (sid-of) code)]
          (if (= 404 (:status first-try))
            (do (swap! state dissoc :sid) (run-block! (sid-of) code))
            first-try))]

    (if (= 200 (:status response))
      (let [body (answer-body response)]
        (tool-result (get body "result") (get body "is_error")))
      (tool-result (gateway-failure response) true))))

(defn- call-tool
  "The result of `tools/call` with `params`, or an error map for a bad call."
  [state params]
  (let [tool
        (get params "name")

        code
        (get-in params ["arguments" "code"])]

    (cond (not= "python_execution" tool) {:error [-32602 (str "Unknown tool: " tool)]}
          (not (string? code)) {:error [-32602 "The argument code must be a string"]}
          :else {:result (try (python-execution! state code)
                              (catch Exception e
                                (tool-result (or (ex-message e) (str e)) true)))})))

(defn- initialize
  "The result of `initialize`."
  [params]
  (let [asked (get params "protocolVersion")]
    {"protocolVersion" (if (some #{asked} protocol-versions) asked (first protocol-versions))
     "capabilities" {"tools" {}}
     "serverInfo" {"name" "vis" "version" (gateway-runtime/release-version)}}))

(defn- handle
  "The answer map to one request `message`, or nil for a notification."
  [state message]
  (let [id
        (get message "id")

        method
        (get message "method")

        params
        (get message "params")

        {:keys [result error]}
        (case method
          "initialize"
          {:result (initialize params)}

          "ping"
          {:result {}}

          "tools/list"
          {:result {"tools" (mcp-tools (:tools @state))}}

          "tools/call"
          (call-tool state params)

          {:error [-32601 (str "Method not found: " method)]})]

    (when (contains? message "id")
      (if error
        {"jsonrpc" "2.0" "id" id "error" {"code" (first error) "message" (second error)}}
        {"jsonrpc" "2.0" "id" id "result" result}))))

(defn- answer
  "The answer to one input `line`, or nil when no answer is due."
  [state line]
  (let [message (try (json/read-json line) (catch Exception _ ::invalid))]
    (cond (= ::invalid message)
          {"jsonrpc" "2.0" "id" nil "error" {"code" -32700 "message" "Parse error"}}
          (not
            (and (map? message) (= "2.0" (get message "jsonrpc")) (string? (get message "method"))))
          (when (or (not (map? message)) (not (contains? message "result")))
            {"jsonrpc" "2.0"
             "id" (when (map? message) (get message "id"))
             "error" {"code" -32600 "message" "Invalid Request"}})
          :else (handle state message))))

(defn- log!
  "Write `text` to `err` when `debug?` is true."
  [^Writer err debug? text]
  (when debug? (.write err (str "[vis mcp serve] " text "\n")) (.flush err)))

(defn serve!
  "Serve MCP on `in` and `out` until the input ends, then answer exit code 0.
   `cwd` is the workspace of the session, and `tools` are the tools that
   `tools/list` answers. With `debug?`, each message goes to `err`, by default
   stderr. Output of other code goes to `*err*`, so it cannot break the protocol."
  [{:keys [^InputStream in ^OutputStream out ^OutputStream err cwd tools debug?]}]
  (let [reader
        (BufferedReader. (InputStreamReader. in StandardCharsets/UTF_8))

        writer
        ^Writer (OutputStreamWriter. out StandardCharsets/UTF_8)

        err
        ^Writer (if err (OutputStreamWriter. err StandardCharsets/UTF_8) *err*)

        state
        (atom {:cwd (str cwd) :tools tools})]

    (binding [*out* *err*]
      (loop []

        (if-let [line (.readLine reader)]
          (do (when-not (str/blank? line)
                (log! err debug? (str "<- " line))
                (when-let [reply (answer state line)]
                  (let [text (json/write-json-str reply)]
                    (log! err debug? (str "-> " text))
                    (.write writer (str text "\n"))
                    (.flush writer))))
              (recur))
          0)))))
