(ns com.blockether.vis.internal.foundation.mcp.serve-test
  (:require [charred.api :as json]
            [clojure.string :as str]
            [com.blockether.vis.internal.foundation.mcp.serve :as serve]
            [com.blockether.vis.internal.gateway.client :as gateway-client]
            [com.blockether.vis.internal.loop.transcript :as transcript]
            [lazytest.core :refer [defdescribe expect it]]))

(defn- exchange
  "Send `messages` as JSON lines to `serve!` and answer the exit code, the parsed
   output lines and the gateway requests."
  [messages respond]
  (let [requests
        (atom [])

        in
        (java.io.ByteArrayInputStream.
          (.getBytes (str (str/join "\n" (map #(if (string? %) % (json/write-json-str %)) messages))
                          "\n")
                     "UTF-8"))

        out
        (java.io.ByteArrayOutputStream.)

        code
        (with-redefs [gateway-client/request! (fn [method path opts]
                                                (swap! requests conj [method path (:body opts)])
                                                (respond method path opts))]
          (serve/serve! {:in in
                         :out out
                         :cwd "/work/project"
                         :tools (transcript/model-facing-tools)
                         :debug? false}))]

    [code (mapv json/read-json (str/split-lines (.toString out "UTF-8"))) @requests]))

(defn- gateway
  "A fake gateway: session `s1`, and block results from `results` in order."
  [results]
  (let [left (atom results)]
    (fn [_ path _]
      (if (= "/v1/sessions" path)
        {:status 201 :body (json/write-json-str {"id" "s1"})}
        (let [[status body] (first @left)]
          (swap! left rest)
          {:status status :body (json/write-json-str body)})))))

(defdescribe
  serve-test
  (it
    "answers initialize, tools/list and tools/call, and reuses one session"
    (let [[code replies requests]
          (exchange [{"jsonrpc" "2.0"
                      "id" 1
                      "method" "initialize"
                      "params" {"protocolVersion" "2025-06-18"
                                "capabilities" {}
                                "clientInfo" {"name" "test" "version" "1"}}}
                     {"jsonrpc" "2.0" "method" "notifications/initialized"}
                     {"jsonrpc" "2.0" "id" 2 "method" "tools/list"}
                     {"jsonrpc" "2.0"
                      "id" 3
                      "method" "tools/call"
                      "params" {"name" "python_execution" "arguments" {"code" "x = 2"}}}
                     {"jsonrpc" "2.0"
                      "id" 4
                      "method" "tools/call"
                      "params" {"name" "python_execution" "arguments" {"code" "print(x + 2)"}}}
                     {"jsonrpc" "2.0" "id" 5 "method" "ping"}]
                    (gateway [[200 {"result" "" "is_error" false}]
                              [200 {"result" "4" "is_error" false}]]))

          by-id
          (into {} (map (juxt #(get % "id") identity)) replies)]

      (expect (= 0 code))
      ;; The notification has no answer.
      (expect (= [1 2 3 4 5] (mapv #(get % "id") replies)))
      (expect (= {"protocolVersion" "2025-06-18" "capabilities" {"tools" {}}}
                 (select-keys (get-in by-id [1 "result"]) ["protocolVersion" "capabilities"])))
      (expect (= "vis" (get-in by-id [1 "result" "serverInfo" "name"])))
      (let [[tool :as tools] (get-in by-id [2 "result" "tools"])]
        (expect (= 1 (count tools)))
        (expect (= "python_execution" (get tool "name")))
        (expect (= {"type" "object"
                    "properties" {"code" {"type" "string" "description" "Python source."}}
                    "required" ["code"]
                    "additionalProperties" false}
                   (get tool "inputSchema"))))
      (expect (= {"content" [{"type" "text" "text" "4"}] "isError" false}
                 (get-in by-id [4 "result"])))
      (expect (= {} (get-in by-id [5 "result"])))
      (expect (= [[:post "/v1/sessions" {"root" "/work/project" "title" "MCP client"}]
                  [:post "/v1/sessions/s1/run" {"code" "x = 2"}]
                  [:post "/v1/sessions/s1/run" {"code" "print(x + 2)"}]]
                 requests))))
  (it
    "answers a failed block as a tool error and a bad message as a protocol error"
    (let [[_ replies]
          (exchange
            [{"jsonrpc" "2.0"
              "id" 1
              "method" "tools/call"
              "params" {"name" "python_execution" "arguments" {"code" "1/0"}}}
             {"jsonrpc" "2.0" "id" 2 "method" "tools/call" "params" {"name" "bash" "arguments" {}}}
             {"jsonrpc" "2.0" "id" 3 "method" "resources/list"} "{not json"]
            (gateway [[200 {"result" "✗ runtime error: division by zero" "is_error" true}]]))]
      (expect (= {"content" [{"type" "text" "text" "✗ runtime error: division by zero"}]
                  "isError" true}
                 (get (nth replies 0) "result")))
      (expect (= -32602 (get-in (nth replies 1) ["error" "code"])))
      (expect (= -32601 (get-in (nth replies 2) ["error" "code"])))
      (expect (= -32700 (get-in (nth replies 3) ["error" "code"])))))
  (it "creates a new session when the gateway does not know the old one"
      (let [[_ replies requests] (exchange [{"jsonrpc" "2.0"
                                             "id" 1
                                             "method" "tools/call"
                                             "params" {"name" "python_execution"
                                                       "arguments" {"code" "1"}}}]
                                           (gateway [[404 {"error" {"message" "unknown session"}}]
                                                     [200 {"result" "" "is_error" false}]]))]
        (expect (= false (get-in (first replies) ["result" "isError"])))
        (expect (= ["/v1/sessions" "/v1/sessions/s1/run" "/v1/sessions" "/v1/sessions/s1/run"]
                   (mapv second requests))))))
