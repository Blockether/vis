(ns com.blockether.vis.internal.foundation.mcp.client-test
  "Exercises the MCP client's stdio transport + JSON-RPC handshake against a
   tiny fake server (test/resources/fake_mcp_server.py). Skips gracefully when
   python3 or the script isn't present so CI without python stays green."
  (:require [babashka.http-client :as http]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.foundation.mcp.client :as mcp]
            [com.blockether.vis.internal.sandbox.jail :as process-jail]
            [lazytest.core :refer [defdescribe expect it]]))

(def ^:private server-path "test/resources/mcp/fake_mcp_server.py")

(defn- on-path
  "Absolute path of `exe` if it's an executable on PATH, else nil."
  [exe]
  (some (fn [d]
          (let [f (io/file d exe)]
            (when (.canExecute f) (.getPath f))))
        (str/split (or (System/getenv "PATH") "") #":")))

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
                       (on-path "python3")

                       f
                       (io/file server-path)]

                   (if-not (and py (.exists f))
                     (expect true) ; prereqs absent — skip, don't fail CI
                     (let [conn (mcp/connect "fake"
                                             {:transport :stdio :command py :args [(.getPath f)]})]
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
                         (finally (mcp/close conn) (expect (false? (mcp/alive? conn))))))))))

(defdescribe
  mcp-stdio-kill-tree-test
  (it "closing a server kills its whole process tree, not just the launcher"
      (let [py
            (on-path "python3")

            sh
            (on-path "sh")

            f
            (io/file server-path)]

        (if-not (and py sh (.exists f))
          (expect true) ; prereqs absent — skip, don't fail CI
          (let [conn
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
            (expect (all-dead-within? kids 8000)))))))

(defdescribe
  mcp-failed-stdio-handshake-cleanup-test
  (it
    "kills the stdio process when initialize fails before connect can return"
    (let [sh
          (on-path "sh")

          sleep
          (on-path "sleep")

          pid-file
          (java.io.File/createTempFile "vis-mcp-failed-" ".pid")

          pid*
          (atom nil)]

      ;; The child, not this test, creates the marker. That proves it really
      ;; started before the intentionally unanswered initialize timed out.
      (.delete pid-file)
      (if-not (and sh sleep)
        (expect true) ; prereqs absent — skip, don't fail CI
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
                        (when-let [^java.lang.ProcessHandle handle
                                   (some-> (java.lang.ProcessHandle/of (long pid))
                                           (.orElse nil))]
                          (when (.isAlive handle) (.destroyForcibly handle))))
                      (.delete pid-file)))))))

(defdescribe
  mcp-stdio-stream-cleanup-test
  (it "closes stdout after both a normal connection and a failed handshake"
      ;; JVM dogfooding: twenty connect/close cycles leaked twenty descriptors.
      ;; EOF is not close: runtime-owned pipe descriptors have no GC fallback.
      (when-let [py (on-path "python3")]
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
                                       (if (instance? Throwable r) (throw r) r)))]
          (let [conn (mcp/connect "fake"
                                  {:transport :streamable-http
                                   :url "https://gateway.example.com/mcp"})]
            (try (expect (true? (mcp/alive? conn)))
                 (reset! reply response)
                 (let [result (try (mcp/call-tool conn "read" {}) (catch Throwable t t))]
                   (expect (or (instance? Throwable result) (true? (get result "isError"))))
                   (expect (= connected? (mcp/alive? conn))))
                 (finally (mcp/close conn)))))))))
