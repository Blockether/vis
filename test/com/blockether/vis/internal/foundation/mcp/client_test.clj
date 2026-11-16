(ns com.blockether.vis.internal.foundation.mcp.client-test
  "Exercises the MCP client's stdio transport + JSON-RPC handshake against a
   tiny fake server (test/resources/mcp/fake_mcp_server.py). Python 3 and POSIX
   process utilities are required; missing prerequisites must fail verification."
  (:require [babashka.http-client :as http]
            [charred.api :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.foundation.mcp.client :as mcp]
            [com.blockether.vis.internal.sandbox.jail :as process-jail]
            [lazytest.core :refer [defdescribe expect it]])
  (:import (com.sun.net.httpserver HttpExchange HttpHandler HttpServer)
           (java.io OutputStream)
           (java.net InetSocketAddress)
           (java.util.concurrent Executors)))

(def ^:private server-path "test/resources/mcp/fake_mcp_server.py")

(defn- require-executable
  "Resolve a required test executable, failing explicitly when it is absent."
  [exe]
  (or (some (fn [d]
              (let [f (io/file d exe)]
                (when (.canExecute f) (.getPath f))))
            (str/split (or (System/getenv "PATH") "") #":"))
      (throw (ex-info (str "MCP tests require " exe " on PATH") {:executable exe}))))

(defn- all-dead-within?
  "Wait at most `timeout-ms` for every ProcessHandle in `handles` to stop."
  [handles timeout-ms]
  (let [deadline (+ (System/currentTimeMillis) (long timeout-ms))]
    (loop []

      (cond (not-any? (fn [^java.lang.ProcessHandle h]
                        (.isAlive h))
                      handles)
            true
            (< (System/currentTimeMillis) deadline) (do (Thread/sleep 50) (recur))
            :else false))))

(defdescribe mcp-stdio-client-test
             (it "initialize + tools/list + tools/call roundtrip over stdio"
                 (let [py
                       (require-executable "python3")

                       f
                       (io/file server-path)

                       conn
                       (mcp/connect "fake" {:transport :stdio :command py :args [(.getPath f)]})]

                   (try
                     ;; initialize handshake surfaced the server identity
                     (expect (= "fake" (get (:server-info conn) "name")))
                     (expect (true? (mcp/alive? conn)))
                     ;; tools/list (cached after first call)
                     (let [tools (mcp/list-tools conn)]
                       (expect (= 1 (count tools)))
                       (expect (= "echo" (get (first tools) "name")))
                       (expect (identical? tools (mcp/list-tools conn))))
                     ;; tools/call returns the content block
                     (let [r (mcp/call-tool conn "echo" {"msg" "hi"})]
                       (expect (false? (boolean (get r "isError"))))
                       (expect (= "echo: hi" (get-in r ["content" 0 "text"]))))
                     ;; unknown tool surfaces isError from the server
                     (let [r (mcp/call-tool conn "nope" {})]
                       (expect (true? (boolean (get r "isError")))))
                     (finally (mcp/close conn) (expect (false? (mcp/alive? conn))))))))

(defn- with-stdio-conn
  "Run `(f conn)` with a connection to the fake stdio server. `options` are more
   keys of the client spec."
  ([f] (with-stdio-conn {} f))
  ([options f]
   (let [conn (mcp/connect "fake"
                           (merge {:transport :stdio
                                   :command (require-executable "python3")
                                   :args [(.getPath (io/file server-path))]}
                                  options))]
     (try (f conn) (finally (mcp/close conn))))))

(defn- call-json
  "The text of the result of tool `tool` on `conn`, read as JSON."
  [conn tool opts]
  (json/read-json (get-in (mcp/call-tool conn tool {} 3000 opts) ["content" 0 "text"])))

(def ^:private model-answer
  {"role" "assistant"
   "content" {"type" "text" "text" "Paris"}
   "model" "small"
   "stopReason" "endTurn"})

(defdescribe mcp-stdio-input-test
             (it "gives a server the roots of the newest call"
                 (with-stdio-conn
                   {:roots? true}
                   (fn [conn]
                     (doseq [uri ["file:///work/a" "file:///work/b"]]
                       (let [roots [{"uri" uri "name" "work"}]]
                         (expect (= {"roots" roots} (call-json conn "roots" {:roots roots}))))))))
             (it "gives a sampling request to the call that waits, and refuses it with no answer"
                 (with-stdio-conn
                   {:sampling? true}
                   (fn [conn]
                     (let [seen (atom nil)
                           answer (fn [params]
                                    (reset! seen params)
                                    model-answer)]
                       (expect (= model-answer (call-json conn "sample" {:on-sampling answer})))
                       (expect (= "Capital of France?"
                                  (get-in @seen ["messages" 0 "content" "text"])))
                       (expect (= -1
                                  (get (call-json conn "sample" {:on-sampling (constantly nil)})
                                       "code"))))))))

(defdescribe mcp-stdio-server-message-test
             (it "answers a ping that the server sends before its reply"
                 (with-stdio-conn (fn [conn]
                                    (expect (= "pong"
                                               (get-in (mcp/call-tool conn "ping" {} 3000)
                                                       ["content" 0 "text"]))))))
             (it "lists the tools again after the server says that they changed"
                 (with-stdio-conn
                   (fn [conn]
                     (expect (= ["echo"] (mapv #(get % "name") (mcp/list-tools conn))))
                     (mcp/call-tool conn "grow" {})
                     (expect (= ["echo" "grown"] (mapv #(get % "name") (mcp/list-tools conn))))))))

(defdescribe mcp-stdio-kill-tree-test
             (it "closing a server kills its whole process tree, not just the launcher"
                 (let [py
                       (require-executable "python3")

                       sh
                       (require-executable "sh")

                       f
                       (io/file server-path)

                       conn
                       ;; What every real MCP server looks like: a launcher (`npx`, `uvx`,
                       ;; `docker run`) with the actual work under it. `sleep` stands in for
                       ;; the worker that outlives a bare `Process.destroy` of the parent.
                       (mcp/connect "fake"
                                    {:transport :stdio
                                     :command sh
                                     :args ["-c" (str "sleep 300 & exec " py " " (.getPath f))]})

                       kids
                       (let [^java.lang.ProcessHandle handle
                             (.orElse (java.lang.ProcessHandle/of (long (:pid conn))) nil)]
                         (with-open [descendants (.descendants handle)]
                           (vec (iterator-seq (.iterator descendants)))))]

                   (expect (= "fake" (get (:server-info conn) "name")))
                   (expect (= 1 (count kids)))
                   (mcp/close conn)
                   ;; Give SIGTERM→SIGKILL its grace window before judging.
                   (expect (all-dead-within? kids 8000)))))

(defdescribe
  mcp-failed-stdio-handshake-cleanup-test
  (it
    "kills the stdio process when initialize fails before connect can return"
    (let [sh
          (require-executable "sh")

          sleep
          (require-executable "sleep")

          pid-file
          (java.io.File/createTempFile "vis-mcp-failed-" ".pid")

          pid*
          (atom nil)]

      ;; The child, not this test, creates the marker. That proves it really
      ;; started before the intentionally unanswered initialize timed out.
      (.delete pid-file)
      (try (let [failure (try (mcp/connect "never-initializes"
                                           {:transport :stdio
                                            :command sh
                                            :args ["-c" "echo $$ > \"$1\"; exec \"$2\" 300"
                                                   "vis-mcp-test" (.getPath pid-file) sleep]
                                            :timeout-ms 100})
                              ::no-throw
                              (catch clojure.lang.ExceptionInfo e e))]
             (expect (instance? clojure.lang.ExceptionInfo failure))
             (expect (= :mcp/timeout (:type (ex-data failure))))
             (expect (.exists pid-file))
             (when (.exists pid-file)
               (let [pid (parse-long (str/trim (slurp pid-file)))
                     handle (some-> (java.lang.ProcessHandle/of (long pid))
                                    (.orElse nil))]

                 (reset! pid* pid)
                 ;; `connect` has already thrown. There is no conn for a caller
                 ;; to close, so only its internal failure cleanup can pass this.
                 (expect (or (nil? handle) (all-dead-within? [handle] 8000))))))
           (finally (when-let [pid @pid*]
                      (when-let [^java.lang.ProcessHandle handle (some-> (java.lang.ProcessHandle/of
                                                                           (long pid))
                                                                         (.orElse nil))]
                        (when (.isAlive handle) (.destroyForcibly handle))))
                    (.delete pid-file))))))

(defdescribe
  mcp-stdio-stream-cleanup-test
  (it "closes stdout after both a normal connection and a failed handshake"
      ;; JVM dogfooding: twenty connect/close cycles leaked twenty descriptors.
      ;; EOF is not close: runtime-owned pipe descriptors have no GC fallback.
      (let [py (require-executable "python3")]
        (doseq [args [[server-path] ["-c" "import time; time.sleep(10)"]]]
          (let [spawn! process-jail/spawn!
                started (atom nil)]

            (try (with-redefs [process-jail/spawn! (fn [& spawn-args]
                                                     (let [proc (apply spawn! spawn-args)]
                                                       (reset! started proc)
                                                       proc))]
                   (try (mcp/close (mcp/connect
                                     "stream-cleanup"
                                     {:transport :stdio :command py :args args :timeout-ms 500}))
                        (catch clojure.lang.ExceptionInfo e
                          (expect (= :mcp/timeout (:type (ex-data e)))))))
                 (let [^Process proc @started
                       stream (.getInputStream proc)]

                   (expect (.waitFor proc 2 java.util.concurrent.TimeUnit/SECONDS))
                   ;; The reader owns closing stdout. Allow its EOF/finally to finish.
                   (expect (loop [remaining 100]
                             (let [closed?
                                   (try (.read stream) false (catch java.io.IOException _ true))]
                               (cond closed? true
                                     (pos? remaining) (do (Thread/sleep 10) (recur (dec remaining)))
                                     :else false)))))
                 (finally (when-let [^Process proc @started]
                            (.destroyForcibly proc)
                            (.close (.getInputStream proc))
                            (.close (.getErrorStream proc))
                            (.close (.getOutputStream proc))))))))))

(defdescribe mcp-stdio-setup-failure-cleanup-test
             (it "reclaims the process and available streams when transport setup throws"
                 ;; A connection does not exist yet; connect's handshake cleanup cannot help.
                 (doseq [stage [:stdout :stdin :stderr]]
                   (let [alive (atom true)
                         closed (atom #{})
                         input (fn [kind]
                                 (proxy [java.io.ByteArrayInputStream] [(byte-array 0)]
                                   (close [] (swap! closed conj kind))))
                         out (input :stdout)
                         err (input :stderr)
                         in (proxy [java.io.ByteArrayOutputStream] []
                              (close [] (swap! closed conj :stdin)))
                         failure (java.io.IOException. "fixture setup failure")
                         proc (proxy [Process] []
                                (getInputStream [] (if (= stage :stdout) (throw failure) out))
                                (getOutputStream [] (if (= stage :stdin) (throw failure) in))
                                (getErrorStream [] (if (= stage :stderr) (throw failure) err))
                                (destroy [] (reset! alive false))
                                (isAlive [] @alive)
                                (waitFor ([] 0) ([timeout unit] true))
                                (exitValue [] 0))]

                     (with-redefs [process-jail/spawn! (fn [& _]
                                                         proc)]
                       (expect (identical? failure
                                           (try (mcp/connect "setup-failure"
                                                             {:transport :stdio :command "fixture"})
                                                nil
                                                (catch java.io.IOException e e)))))
                     (expect (false? @alive))
                     (expect (= (disj #{:stdout :stdin :stderr} stage) @closed))))))

(defdescribe mcp-transport-normalization-test
             (it "accepts canonical external transport values and their internal keyword form"
                 (let [transport-of (ns-resolve 'com.blockether.vis.internal.foundation.mcp.client
                                                'transport-of)]
                   (expect (= :stdio (transport-of {:transport "stdio"})))
                   (expect (= :streamable-http (transport-of {:transport "streamable_http"})))
                   (expect (= :streamable-http (transport-of {:transport :streamable-http})))
                   (doseq [spec [{:transport "http"} {:url "https://mcp.example.test/mcp"}]]
                     (expect
                       (= "Unsupported MCP transport"
                          (try (transport-of spec) nil (catch Exception e (ex-message e)))))))))

(defdescribe mcp-json-encoding-total-test
             (it "encodes ANY tool argument instead of throwing inside the JSON-RPC write"
                 (let [->json
                       (ns-resolve 'com.blockether.vis.internal.foundation.mcp.client '->json)

                       out
                       (->json {"n" ##NaN
                                "i" ##Inf
                                "-i" ##-Inf
                                :kw "v"
                                7 "int-key"
                                "nested" {:deep [1/3 #{:x} (java.util.Date. 0)]}})]

                   ;; `arguments` come from a model or another extension. One NaN, one keyword
                   ;; key, one stray object used to throw a raw CharredException from INSIDE
                   ;; the write - after the request id was allocated, leaving broken framing
                   ;; instead of a tool error.
                   (expect (string? out))
                   (expect (str/includes? out "\"n\":null"))
                   (expect (str/includes? out "\"i\":null"))
                   (expect (str/includes? out "\"-i\":null"))
                   (expect (str/includes? out "\"kw\":\"v\""))
                   ;; JSON has exactly one kind of key.
                   (expect (str/includes? out "\"7\":\"int-key\""))
                   ;; Pathological nesting is data, not a StackOverflowError.
                   (expect (string? (->json (reduce (fn [m _]
                                                      {"k" m})
                                                    {"leaf" 1}
                                                    (range 400))))))))

(defdescribe
  mcp-http-failed-request-status-test
  (it
    "invalidates failed HTTP connections but preserves RPC refusals and cancellation"
    ;; Regression: a cached catalog kept an unauthorized HTTP connection marked alive.
    (doseq
      [[response connected?]
       [[{:status 401 :body "Unauthorized"} false] [{:status 503 :body "Unavailable"} false]
        [(java.io.IOException. "Connection lost") false] [{:status 200 :body "not JSON"} false]
        [{:status 200
          :body
          "{\"jsonrpc\":\"2.0\",\"error\":{\"code\":-32602,\"message\":\"Invalid arguments\"}}"}
         true] [(InterruptedException. "Cancelled") true]
        [{:status 200 :body "{\"jsonrpc\":\"2.0\",\"result\":{\"isError\":true,\"content\":[]}}"}
         true]]]
      (let
        [reply
         (atom
           {:status 200
            :body
            "{\"jsonrpc\":\"2.0\",\"result\":{\"protocolVersion\":\"2025-11-25\",\"capabilities\":{},\"serverInfo\":{\"name\":\"fake\",\"version\":\"1\"}}}"})]
        (with-redefs [http/request (fn [_]
                                     (let [r @reply]
                                       (if (instance? Throwable r)
                                         (throw r)
                                         ;; The transport reads the body as a stream.
                                         (update r
                                                 :body
                                                 #(java.io.ByteArrayInputStream.
                                                    (.getBytes ^String % "UTF-8"))))))]
          (let [conn (mcp/connect "fake"
                                  {:transport :streamable-http
                                   :url "https://gateway.example.com/mcp"})]
            (try (expect (true? (mcp/alive? conn)))
                 (reset! reply response)
                 (let [result (try (mcp/call-tool conn "read" {}) (catch Throwable t t))]
                   (expect (or (instance? Throwable result) (true? (get result "isError"))))
                   (expect (= connected? (mcp/alive? conn))))
                 (finally (mcp/close conn)))))))))

;; Streamable HTTP transport, against a local server.

(defn- send!
  [^HttpExchange exchange status content-type ^String body]
  (let [bytes (.getBytes body "UTF-8")]
    (.add (.getResponseHeaders exchange) "Content-Type" content-type)
    (.sendResponseHeaders exchange (int status) (if (zero? (alength bytes)) -1 (alength bytes)))
    (when (pos? (alength bytes)) (.write (.getResponseBody exchange) bytes))
    (.close exchange)))

(defn- open-stream!
  "Start an SSE response and return its stream. The caller closes it."
  ^OutputStream [^HttpExchange exchange]
  (.add (.getResponseHeaders exchange) "Content-Type" "text/event-stream")
  (.sendResponseHeaders exchange 200 0)
  (.getResponseBody exchange))

(defn- event! [^OutputStream out ^String text] (.write out (.getBytes text "UTF-8")) (.flush out))

(defn- rpc-result
  [message value]
  (json/write-json-str {"jsonrpc" "2.0" "id" (get message "id") "result" value}))

(defn- text-result [message text] (rpc-result message {"content" [{"type" "text" "text" text}]}))

(defn- with-mcp-http
  "Run `(f url requests)` against a local Streamable HTTP server. `routes` maps a
   JSON-RPC method, `:reply` (a reply of the client) or `:get` to
   `(fn [request exchange])`. `initialize`, a notification and `tools/list` have
   default answers. `requests` collects `{:method :headers :message}`."
  [routes f]
  (let [requests
        (atom [])

        pool
        (Executors/newCachedThreadPool)

        server
        (HttpServer/create (InetSocketAddress. "127.0.0.1" 0) 0)

        serve
        (fn [^HttpExchange exchange]
          (let [message
                (when (= "POST" (.getRequestMethod exchange))
                  (json/read-json (slurp (.getRequestBody exchange))))

                request
                {:method (.getRequestMethod exchange)
                 :headers (into {}
                                (map (fn [[k v]]
                                       [(str/lower-case k) (first v)]))
                                (.getRequestHeaders exchange))
                 :message message}

                route
                (get routes (if message (get message "method" :reply) :get))]

            (swap! requests conj request)
            (cond route (route request exchange)
                  (nil? message) (send! exchange 405 "text/plain" "")
                  (= "initialize" (get message "method"))
                  (send! exchange
                         200
                         "application/json"
                         (rpc-result message
                                     {"protocolVersion" "2025-06-18"
                                      "capabilities" {"tools" {}}
                                      "serverInfo" {"name" "local" "version" "1"}}))
                  (nil? (get message "id")) (send! exchange 202 "text/plain" "")
                  (= "tools/list" (get message "method"))
                  (send! exchange
                         200
                         "application/json"
                         (rpc-result message {"tools" [{"name" "echo"}]}))
                  :else (send! exchange 404 "text/plain" ""))))]

    (.createContext server
                    "/mcp"
                    (reify
                      HttpHandler
                        (handle [_ exchange]
                          (try (serve exchange) (catch Throwable _ (.close exchange))))))
    (.setExecutor server pool)
    (.start server)
    (try (f (str "http://127.0.0.1:" (.getPort (.getAddress server)) "/mcp") requests)
         (finally (.stop server 0) (.shutdownNow pool)))))

(defn- with-http-conn
  "Run `(f conn requests)` with a connection to a server of `routes`."
  [routes options f]
  (with-mcp-http routes
                 (fn [url requests]
                   (let [conn (mcp/connect "http-test"
                                           (merge
                                             {:transport :streamable-http :url url :timeout-ms 3000}
                                             options))]
                     (try (f conn requests) (finally (mcp/close conn)))))))

(defn- call-text [conn] (get-in (mcp/call-tool conn "echo" {}) ["content" 0 "text"]))

(defdescribe
  mcp-http-reply-test
  (it "reads a JSON reply whose text has the SSE field name in it"
      (with-http-conn {"tools/call" (fn [request exchange]
                                      (send! exchange
                                             200
                                             "application/json"
                                             (text-result (:message request) "metadata: none")))}
                      {}
                      (fn [conn _]
                        (expect (= "metadata: none" (call-text conn))))))
  (it "sends the negotiated protocol version from the first message after initialize"
      (with-http-conn {}
                      {}
                      (fn [_ requests]
                        (let [initialized (first (filter #(= "notifications/initialized"
                                                             (get-in % [:message "method"]))
                                                         @requests))]
                          (expect (= "2025-06-18"
                                     (get-in initialized [:headers "mcp-protocol-version"])))))))
  (it "lists the tools of every page"
      (with-http-conn
        {"tools/list" (fn [request exchange]
                        (send! exchange
                               200
                               "application/json"
                               (rpc-result (:message request)
                                           (if (= "page-2"
                                                  (get-in request [:message "params" "cursor"]))
                                             {"tools" [{"name" "second"}]}
                                             {"tools" [{"name" "first"}] "nextCursor" "page-2"}))))}
        {}
        (fn [conn _]
          (expect (= ["first" "second"] (mapv #(get % "name") (mcp/list-tools conn))))))))

(defdescribe
  mcp-http-stream-test
  (it
    "continues on a new stream after the server closes the first one"
    (let [call
          (atom nil)

          closed-at
          (atom nil)

          resumed
          (atom nil)

          release
          (promise)]

      (with-http-conn
        {"tools/call" (fn [request exchange]
                        (reset! call (:message request))
                        (with-open [out (open-stream! exchange)]
                          (event! out "id: event-1\nretry: 200\ndata: \n\n"))
                        (reset! closed-at (System/nanoTime)))
         :get (fn [request exchange]
                (reset! resumed {:at (System/nanoTime)
                                 :last-event-id (get-in request [:headers "last-event-id"])})
                (with-open [out (open-stream! exchange)]
                  (event! out
                          (str "event: message\nid: event-2\ndata: "
                               (text-result @call "resumed")
                               "\n\n"))
                  ;; The stream stays open after the reply.
                  (deref release 5000 nil)))}
        {}
        (fn [conn _]
          (try (expect (= "resumed" (call-text conn)))
               (expect (= "event-1" (:last-event-id @resumed)))
               ;; The server asked for 200 ms before the next request.
               (expect (<= 150 (quot (- (long (:at @resumed)) (long @closed-at)) 1000000)))
               (finally (deliver release true)))))))
  (it "answers a ping that the server sends before its reply"
      (let [pong (promise)]
        (with-http-conn
          {"tools/call" (fn [request exchange]
                          (with-open [out (open-stream! exchange)]
                            (event! out
                                    (str "data: "
                                         (json/write-json-str
                                           {"jsonrpc" "2.0" "id" "server-1" "method" "ping"})
                                         "\n\n"))
                            (event! out
                                    (str "data: "
                                         (text-result (:message request)
                                                      (if (deref pong 2000 nil) "pong" "silence"))
                                         "\n\n"))))
           :reply (fn [request exchange]
                    (deliver pong (:message request))
                    (send! exchange 202 "text/plain" ""))}
          {}
          (fn [conn _]
            (expect (= "pong" (call-text conn)))
            (expect (= {"jsonrpc" "2.0" "id" "server-1" "result" {}} (deref pong 0 nil)))))))
  (it "stops at the timeout when the server holds the stream open without a reply"
      (let [release (promise)]
        (with-http-conn {"tools/call" (fn [_ exchange]
                                        (with-open [out (open-stream! exchange)]
                                          (event! out ": waiting\n\n")
                                          (deref release 8000 nil)))}
                        {:timeout-ms 500}
                        (fn [conn _]
                          (let [outcome (deref (future (try (call-text conn)
                                                            (catch clojure.lang.ExceptionInfo e
                                                              (:type (ex-data e)))))
                                               4000
                                               :hung)]
                            (deliver release true)
                            (expect (= :mcp/timeout outcome)))))))
  (it "stays connected to a server that has no listen channel"
      (with-http-conn {}
                      {:listen? true}
                      (fn [conn _]
                        ;; The default GET answer is HTTP 405.
                        (Thread/sleep 300)
                        (expect (true? (mcp/alive? conn))))))
  (it "hears a notification while the listen stream stays open"
      (let [heard
            (promise)

            release
            (promise)]

        (with-http-conn
          {:get (fn [_ exchange]
                  (with-open [out (open-stream! exchange)]
                    (event! out
                            (str "data: "
                                 (json/write-json-str {"jsonrpc" "2.0"
                                                       "method" "notifications/tools/list_changed"})
                                 "\n\n"))
                    (deref release 5000 nil)))}
          {:listen? true :on-notification #(deliver heard (get % "method"))}
          (fn [_ _]
            (try (expect (= "notifications/tools/list_changed" (deref heard 2000 :silent)))
                 (finally (deliver release true))))))))

;; Elicitation: the server asks the person in the middle of a call.

(defn- elicitation-event
  [id]
  (str "data: "
       (json/write-json-str
         {"jsonrpc" "2.0"
          "id" id
          "method" "elicitation/create"
          "params" {"message" "Which name?"
                    "requestedSchema" {"type" "object"
                                       "properties" {"name" {"type" "string" "default" "Ann"}}}}})
       "\n\n"))

(defdescribe
  mcp-elicitation-test
  (it "gives the elicitation of a stdio server to the call that waits"
      (let [conn (mcp/connect "fake"
                              {:transport :stdio
                               :command (require-executable "python3")
                               :args [server-path]
                               :elicitation? true})
            asked (atom nil)
            answer (fn [options]
                     (json/read-json (get-in (mcp/call-tool conn "ask" {} 5000 options)
                                             ["content" 0 "text"])))]
        (try (expect (= {"action" "accept" "content" {"name" "Ann" "zone" "UTC" "age" 30}}
                        (answer {:on-elicitation (fn [params]
                                                   (reset! asked params)
                                                   {"action" "accept" "content" {"name" "Ann"}})})))
             ;; A person reads the fields in the order that the server wrote.
             (expect (= ["zone" "age" "name"]
                        (keys (get-in @asked ["requestedSchema" "properties"]))))
             (expect (= {"action" "decline"}
                        (answer {:on-elicitation (constantly {"action" "decline"})})))
             ;; Nobody can answer: the server gets a cancel, and the call does not hang.
             (expect (= {"action" "cancel"} (answer nil)))
             (finally (mcp/close conn)))))
  (it "does not count the time of the person against the deadline of the call"
      (let [reply (promise)]
        (with-http-conn
          {"tools/call" (fn [request exchange]
                          (with-open [out (open-stream! exchange)]
                            (event! out (elicitation-event "server-1"))
                            (event! out
                                    (str "data: "
                                         (text-result (:message request)
                                                      (json/write-json-str
                                                        (get (deref reply 4000 nil) "result")))
                                         "\n\n"))))
           :reply (fn [request exchange]
                    (deliver reply (:message request))
                    (send! exchange 202 "text/plain" ""))}
          {:elicitation? true :timeout-ms 500}
          (fn [conn _]
            (let [result (mcp/call-tool conn
                                        "echo"
                                        {}
                                        500
                                        {:on-elicitation (fn [_]
                                                           (Thread/sleep 1200)
                                                           {"action" "accept" "content" {}})})]
              (expect (= {"action" "accept" "content" {"name" "Ann"}}
                         (json/read-json (get-in result ["content" 0 "text"])))))))))
  (it "hears an elicitation that the server sends on its listen channel"
      ;; The TypeScript SDK sends a server request there when it names no call.
      (let [called (promise)
            reply (promise)
            release (promise)]
        (with-http-conn
          {:get (fn [_ exchange]
                  (with-open [out (open-stream! exchange)]
                    (deref called 4000 nil)
                    (event! out (elicitation-event "server-1"))
                    (deref release 5000 nil)))
           "tools/call" (fn [request exchange]
                          (deliver called true)
                          (send! exchange
                                 200
                                 "application/json"
                                 (text-result (:message request)
                                              (json/write-json-str
                                                (get (deref reply 4000 nil) "result")))))
           :reply (fn [request exchange]
                    (deliver reply (:message request))
                    (send! exchange 202 "text/plain" ""))}
          {:elicitation? true :listen? true}
          (fn [conn _]
            (try (let [result (mcp/call-tool conn
                                             "echo"
                                             {}
                                             3000
                                             {:on-elicitation (constantly {"action" "decline"})})]
                   (expect (= {"action" "decline"}
                              (json/read-json (get-in result ["content" 0 "text"])))))
                 (finally (deliver release true))))))))

;; HTTP+SSE transport of protocol 2024-11-05.

(defn- with-legacy-sse
  "Run `(f url end-stream!)` against a local server that has only the HTTP+SSE
   transport: a GET on `/sse` names `/messages` for the POST of each message, and
   every reply goes out on the stream."
  [f]
  (let [pool
        (Executors/newCachedThreadPool)

        server
        (HttpServer/create (InetSocketAddress. "127.0.0.1" 0) 0)

        stream
        (promise)

        release
        (promise)

        handler
        (fn [serve]
          (reify
            HttpHandler
              (handle [_ exchange]
                (try (serve exchange) (catch Throwable _ (.close exchange))))))]

    (.createContext server
                    "/sse"
                    (handler (fn [^HttpExchange exchange]
                               (if (= "GET" (.getRequestMethod exchange))
                                 (with-open [out (open-stream! exchange)]
                                   (event! out "event: endpoint\ndata: /messages?session=1\n\n")
                                   (deliver stream out)
                                   (deref release 10000 nil))
                                 (send! exchange 405 "text/plain" "Method Not Allowed")))))
    (.createContext
      server
      "/messages"
      (handler
        (fn [^HttpExchange exchange]
          (let [message
                (json/read-json (slurp (.getRequestBody exchange)))

                reply
                (case (get message "method")
                  "initialize"
                  (rpc-result message
                              {"protocolVersion" "2024-11-05"
                               "capabilities" {"tools" {}}
                               "serverInfo" {"name" "legacy" "version" "1"}})

                  "tools/list"
                  (rpc-result message {"tools" [{"name" "echo"}]})

                  "tools/call"
                  (text-result message (.getQuery (.getRequestURI exchange)))

                  nil)]

            (send! exchange 202 "text/plain" "")
            (when reply (event! @stream (str "event: message\ndata: " reply "\n\n")))))))
    (.setExecutor server pool)
    (.start server)
    (try (f (str "http://127.0.0.1:" (.getPort (.getAddress server)) "/sse") #(deliver release true))
         (finally (deliver release true) (.stop server 0) (.shutdownNow pool)))))

(defdescribe
  mcp-legacy-sse-test
  (it "calls a tool of a server that has only the HTTP+SSE transport"
      (with-legacy-sse
        (fn [url end-stream!]
          (let [conn (mcp/connect "legacy" {:transport :sse :url url :timeout-ms 3000})]
            (try (expect (= "legacy" (get (:server-info conn) "name")))
                 (expect (= ["echo"] (mapv #(get % "name") (mcp/list-tools conn))))
                 ;; Each message goes to the endpoint that the stream named.
                 (expect (= "session=1" (call-text conn)))
                 ;; The stream is the connection: without it the server is gone.
                 (end-stream!)
                 (expect (loop [remaining 100]
                           (cond (not (mcp/alive? conn)) true
                                 (pos? remaining) (do (Thread/sleep 20) (recur (dec remaining)))
                                 :else false)))
                 (finally (mcp/close conn)))))))
  (it "falls back to HTTP+SSE when a Streamable HTTP server refuses the POST"
      (with-legacy-sse
        (fn [url _end-stream!]
          (let [conn (mcp/connect "legacy" {:transport :streamable-http :url url :timeout-ms 3000})]
            (try (expect (= "legacy" (get (:server-info conn) "name")))
                 (expect (= "session=1" (call-text conn)))
                 (finally (mcp/close conn)))))))
  (it "reports the refusal of the POST when the server has no HTTP+SSE transport"
      (with-legacy-sse
        (fn [url _end-stream!]
          (let [failure (try (mcp/connect "none"
                                          {:transport :streamable-http
                                           :url (str/replace url "/sse" "/nothing")
                                           :timeout-ms 1000})
                             nil
                             (catch Exception e e))]
            (expect (= :mcp/http-error (:type (ex-data failure)))))))))

;; Stateless revisions (2026-07-28): no handshake, request metadata, input rounds.

(defn- rpc-refusal
  [message code data]
  (json/write-json-str {"jsonrpc" "2.0"
                        "id" (get message "id")
                        "error" (cond-> {"code" code "message" "Refused"}
                                  data
                                  (assoc "data" data))}))

(defn- stateless-routes
  "The routes of a server of revision 2026-07-28 that has `tools` and declares
   `capabilities`: it refuses `initialize`, and `call` answers each `tools/call`."
  [tools capabilities call]
  {"initialize" (fn [request exchange]
                  (send! exchange 400 "application/json" (rpc-refusal (:message request) -32601 nil)))
   "server/discover"
   (fn [request exchange]
     (send! exchange
            200
            "application/json"
            (rpc-result (:message request)
                        {"resultType" "complete"
                         "supportedVersions" ["2026-07-28"]
                         "capabilities" capabilities
                         "_meta" {"io.modelcontextprotocol/serverInfo" {"name" "stateless"
                                                                        "version" "1"}}})))
   "tools/list" (fn [request exchange]
                  (send! exchange
                         200
                         "application/json"
                         (rpc-result (:message request) {"resultType" "complete" "tools" tools})))
   "tools/call" call})

(defn- requests-of
  [requests method]
  (filterv #(= method (get-in % [:message "method"])) @requests))

(defn- text-answer
  [text]
  (fn [request exchange]
    (send! exchange 200 "application/json" (text-result (:message request) text))))

(defdescribe
  mcp-stateless-http-test
  (it "calls a tool of a server that has no handshake"
      (with-http-conn
        (stateless-routes [{"name" "echo"}] {"tools" {}} (text-answer "stateless"))
        {}
        (fn [conn requests]
          (expect (= "stateless" (get (:server-info conn) "name")))
          (expect (= "stateless" (call-text conn)))
          (let [call (first (requests-of requests "tools/call"))
                meta (get-in call [:message "params" "_meta"])]
            (expect (empty? (requests-of requests "initialize")))
            (expect (= "2026-07-28" (get meta "io.modelcontextprotocol/protocolVersion")))
            (expect (map? (get meta "io.modelcontextprotocol/clientCapabilities")))
            (expect (= ["2026-07-28" "tools/call" "echo"]
                       (mapv #(get-in call [:headers %])
                             ["mcp-protocol-version" "mcp-method" "mcp-name"])))))))
  (it "asks again in a version that the server names"
      (let [asked (atom 0)
            routes (stateless-routes [] {} nil)]
        (with-http-conn
          (assoc routes
            "server/discover" (fn [request exchange]
                                (if (= 1 (swap! asked inc))
                                  (send! exchange
                                         400
                                         "application/json"
                                         (rpc-refusal (:message request)
                                                      -32022
                                                      {"supported" ["2026-07-28"]
                                                       "requested" "2026-07-28"}))
                                  ((get routes "server/discover") request exchange))))
          {}
          (fn [conn _]
            (expect (= "2026-07-28" (:protocol-version conn)))
            (expect (= 2 @asked))))))
  (it "answers the input that a result asks for, in a new request with the state of the server"
      (with-http-conn
        (stateless-routes
          [{"name" "echo"}]
          {"tools" {}}
          (fn [request exchange]
            (send! exchange
                   200
                   "application/json"
                   (if (get-in request [:message "params" "inputResponses"])
                     (text-result (:message request) "done")
                     (rpc-result (:message request)
                                 {"resultType" "input_required"
                                  "inputRequests"
                                  {"who" {"method" "elicitation/create"
                                          "params" {"message" "Who?"
                                                    "requestedSchema"
                                                    {"type" "object"
                                                     "properties" {"name" {"type" "string"
                                                                           "default" "Ada"}}}}}}
                                  "requestState" "state-1"})))))
        {:elicitation? true}
        (fn [conn requests]
          (let [answer {:on-elicitation (constantly {"action" "accept" "content" {}})}]
            (expect (= "done"
                       (get-in (mcp/call-tool conn "echo" {} 3000 answer) ["content" 0 "text"])))
            (let [[asked answered] (mapv :message (requests-of requests "tools/call"))]
              (expect (not= (get asked "id") (get answered "id")))
              (expect (= {"who" {"action" "accept" "content" {"name" "Ada"}}}
                         (get-in answered ["params" "inputResponses"])))
              (expect (= "state-1" (get-in answered ["params" "requestState"]))))
            ;; The state of one call is no part of the next call.
            (mcp/call-tool conn "echo" {} 3000 answer)
            (expect (not (contains? (get-in (nth (requests-of requests "tools/call") 2)
                                            [:message "params"])
                                    "requestState")))))))
  (it "answers roots and sampling that a result asks for, and ends the call on a refusal"
      (with-http-conn
        (stateless-routes
          [{"name" "echo"}]
          {"tools" {}}
          (fn [request exchange]
            (send! exchange
                   200
                   "application/json"
                   (if (get-in request [:message "params" "inputResponses"])
                     (text-result (:message request) "done")
                     (rpc-result (:message request)
                                 {"resultType" "input_required"
                                  "inputRequests"
                                  {"where" {"method" "roots/list"}
                                   "what" {"method" "sampling/createMessage"
                                           "params" {"messages" [] "maxTokens" 9}}}})))))
        {:roots? true :sampling? true}
        (fn [conn requests]
          (let [roots [{"uri" "file:///work" "name" "work"}]
                call (fn [sample]
                       (mcp/call-tool conn "echo" {} 3000 {:roots roots :on-sampling sample}))]
            (expect (= "done" (get-in (call (constantly model-answer)) ["content" 0 "text"])))
            (let [[asked answered] (mapv :message (requests-of requests "tools/call"))]
              (expect (= {"roots" {"listChanged" true} "sampling" {}}
                         (get-in asked
                                 ["params" "_meta" "io.modelcontextprotocol/clientCapabilities"])))
              (expect (= {"where" {"roots" roots} "what" model-answer}
                         (get-in answered ["params" "inputResponses"]))))
            (expect (= :mcp/input-refused
                       (try (call (constantly nil))
                            (catch clojure.lang.ExceptionInfo e (:type (ex-data e))))))))))
  (it "reads a resource of a server and names it in the headers"
      (with-http-conn
        (assoc (stateless-routes [] {"resources" {}} nil)
          "resources/read" (fn [request exchange]
                             (send! exchange
                                    200
                                    "application/json"
                                    (rpc-result (:message request)
                                                {"resultType" "complete"
                                                 "contents" [{"uri" "file:///a" "text" "A"}]}))))
        {}
        (fn [conn requests]
          (expect (= "A"
                     (get-in (mcp/request conn "resources/read" {"uri" "file:///a"})
                             ["contents" 0 "text"])))
          (let [read (first (requests-of requests "resources/read"))]
            (expect (= ["resources/read" "file:///a"]
                       (mapv #(get-in read [:headers %]) ["mcp-method" "mcp-name"])))))))
  (it "mirrors an annotated argument into its header, and leaves out a tool with a bad header"
      (with-http-conn
        (stateless-routes
          [{"name" "query"
            "inputSchema" {"type" "object"
                           "properties" {"region" {"type" "string" "x-mcp-header" "Region"}
                                         "note" {"type" "string" "x-mcp-header" "Note"}
                                         "limit" {"type" "integer" "x-mcp-header" "Limit"}}}}
           {"name" "bad"
            "inputSchema" {"type" "object"
                           "properties" {"region" {"type" "string" "x-mcp-header" "My Region"}}}}]
          {"tools" {}}
          (text-answer "ok"))
        {}
        (fn [conn requests]
          (expect (= ["query"] (mapv #(get % "name") (mcp/list-tools conn))))
          (mcp/call-tool conn "query" {"region" "us-west1" "note" "zażółć" "limit" nil})
          (let [headers (:headers (first (requests-of requests "tools/call")))]
            (expect (= "us-west1" (get headers "mcp-param-region")))
            (expect (= (str "=?base64?"
                            (.encodeToString (java.util.Base64/getEncoder)
                                             (.getBytes "zażółć" "UTF-8"))
                            "?=")
                       (get headers "mcp-param-note")))
            (expect (not (contains? headers "mcp-param-limit")))))))
  (it "keeps the connection after a refusal that comes with an HTTP error status"
      (with-http-conn
        (stateless-routes [{"name" "echo"}]
                          {"tools" {}}
                          (fn [request exchange]
                            (send! exchange
                                   400
                                   "application/json"
                                   (rpc-refusal (:message request) -32602 nil))))
        {}
        (fn [conn _]
          (let [failure (try (mcp/call-tool conn "echo" {}) nil (catch Exception e e))]
            (expect (= :mcp/rpc-error (:type (ex-data failure))))
            (expect (true? (mcp/alive? conn)))))))
  (it "hears that the tools changed on the stream of its subscription"
      (let [heard (promise)
            release (promise)]
        (with-http-conn
          (assoc (stateless-routes [] {"tools" {"listChanged" true}} nil)
            "subscriptions/listen"
            (fn [request exchange]
              (with-open [out (open-stream! exchange)]
                (event! out
                        (str "data: "
                             (json/write-json-str
                               {"jsonrpc" "2.0"
                                "method" "notifications/tools/list_changed"
                                "params" {"_meta" {"io.modelcontextprotocol/subscriptionId"
                                                   (get-in request [:message "id"])}}})
                             "\n\n"))
                (deref release 5000 nil))))
          {:listen? true :on-notification #(deliver heard (get % "method"))}
          (fn [_ requests]
            (try (expect (= "notifications/tools/list_changed" (deref heard 2000 :silent)))
                 (expect (= {"toolsListChanged" true}
                            (get-in (first (requests-of requests "subscriptions/listen"))
                                    [:message "params" "notifications"])))
                 (finally (deliver release true))))))))

(defdescribe mcp-stateless-stdio-test
  (it "calls a tool of a stdio server that has no handshake, and hears that its tools changed"
      (let [conn (mcp/connect "fake"
                              {:transport :stdio
                               :command (require-executable "python3")
                               :args [(.getPath (io/file server-path)) "stateless"]})
            names #(mapv (fn [tool] (get tool "name")) (mcp/list-tools conn))]
        (try (expect (= "fake" (get (:server-info conn) "name")))
             (expect (= ["echo"] (names)))
             (expect (= "echo: hi" (get-in (mcp/call-tool conn "echo" {"msg" "hi"}) ["content" 0 "text"])))
             (mcp/call-tool conn "grow" {})
             (expect (= ["echo" "grown"] (names)))
             (finally (mcp/close conn))))))

(defn- era-routes
  "The routes of a server that is of an older revision, with a session, until the
   atom `modern?` is true. After that it is a stateless server."
  [modern?]
  (let [stateless
        (stateless-routes [{"name" "echo"}] {"tools" {}} (text-answer "stateless"))

        refuse
        (fn [request exchange]
          (send! exchange 400 "application/json" (rpc-refusal (:message request) -32000 nil)))]

    {"server/discover" (fn [request exchange]
                         (if @modern?
                           ((get stateless "server/discover") request exchange)
                           (send! exchange 404 "text/plain" "")))
     "initialize" (fn [request ^HttpExchange exchange]
                    (if @modern?
                      (refuse request exchange)
                      (do (.add (.getResponseHeaders exchange) "Mcp-Session-Id" "session-1")
                          (send! exchange
                                 200
                                 "application/json"
                                 (rpc-result (:message request)
                                             {"protocolVersion" "2025-06-18"
                                              "capabilities" {"tools" {}}
                                              "serverInfo" {"name" "local" "version" "1"}})))))
     "tools/call" (fn [request exchange]
                    (let [stateless? (some? (get-in request [:message "params" "_meta"]))
                          session? (some? (get-in request [:headers "mcp-session-id"]))]
                      (cond (and @modern? stateless?) ((text-answer "stateless") request exchange)
                            (and (not @modern?) session?) ((text-answer "legacy") request exchange)
                            :else (refuse request exchange))))}))

(defn- era-change
  "Connect to a server of `era-routes` while it is in the era `modern-first?`, then
   change its era. Returns whether the failure of the next call is a stale era,
   and what a new connection answers."
  [modern-first?]
  (let [modern? (atom modern-first?)]
    (with-mcp-http
      (era-routes modern?)
      (fn [url _]
        (let [spec {:transport :streamable-http :url url :timeout-ms 3000}
              conn (mcp/connect "era" spec)]
          (try (swap! modern? not)
               (let [stale? (try (call-text conn)
                                 (catch clojure.lang.ExceptionInfo failure
                                   (mcp/stale-era? conn failure)))
                     fresh (mcp/connect "era" spec)]
                 (try [(:modern? conn) stale? (boolean (:modern? fresh)) (call-text fresh)]
                      (finally (mcp/close fresh))))
               (finally (mcp/close conn))))))))

(defdescribe mcp-era-change-test
             (it "sees that a server of an older revision became stateless"
                 (expect (= [nil true true "stateless"] (era-change false))))
             (it "sees that a stateless server became a server of an older revision"
                 (expect (= [true true false "legacy"] (era-change true)))))

(defn- stdio-era
  "Connect to the fake stdio server with `modes` and give `[modern? echo ms]`."
  [modes]
  (let [started (System/currentTimeMillis)
        conn (mcp/connect "fake"
                          {:transport :stdio
                           :command (require-executable "python3")
                           :args (into [(.getPath (io/file server-path))] modes)})]
    (try [(boolean (:modern? conn))
          (get-in (mcp/call-tool conn "echo" {"msg" "hi"}) ["content" 0 "text"])
          (- (System/currentTimeMillis) started)]
         (finally (mcp/close conn)))))

(defdescribe mcp-stdio-era-test
  (it "connects without a long wait to a server that is silent on an unknown request"
      ;; #458: such a server got `initialize` only after five seconds of silence.
      (let [[modern? echo ms] (stdio-era ["mute"])]
        (expect (false? modern?))
        (expect (= "echo: hi" echo))
        (expect (< (long ms) 4000) (str ms " ms"))))
  (it "finds the era of a server that starts after the client asked for both eras"
      ;; #458: a slow server of both eras got the old handshake.
      (with-redefs-fn {#'mcp/stdio-hedge-ms 100}
        (fn []
          (doseq [[modes modern?] [[["dual" "slow"] true]
                                   [["stateless" "slow"] true]
                                   [["slow"] false]
                                   [["mute" "slow"] false]]]
            (expect (= [modern? "echo: hi"] (subvec (stdio-era modes) 0 2)) (str modes)))))))
