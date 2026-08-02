(ns com.blockether.vis.internal.foundation.mcp.client-test
  "Exercises the MCP client's stdio transport + JSON-RPC handshake against a
   tiny fake server (test/resources/fake_mcp_server.py). Skips gracefully when
   python3 or the script isn't present so CI without python stays green."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.foundation.mcp.client :as mcp]
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
  (let [deadline (+ (System/currentTimeMillis) timeout-ms)]
    (loop []

      (cond (not-any? (fn [^java.lang.ProcessHandle h]
                        (.isAlive h))
                      handles)
            true
            (< (System/currentTimeMillis) deadline) (do (Thread/sleep 50) (recur))
            :else false))))

(defdescribe
  mcp-stdio-client-test
  (it "initialize + tools/list + tools/call roundtrip over stdio"
      (let
        [py
         (on-path "python3")

         f
         (io/file server-path)]

        (if-not (and py (.exists f))
          (expect true) ; prereqs absent — skip, don't fail CI
          (let [conn (mcp/connect "fake" {:transport :stdio :command py :args [(.getPath f)]})]
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
  (it
    "closing a server kills its whole process tree, not just the launcher"
    (let
      [py
       (on-path "python3")

       sh
       (on-path "sh")

       f
       (io/file server-path)]

      (if-not (and py sh (.exists f))
        (expect true) ; prereqs absent — skip, don't fail CI
        (let
          [conn
           ;; What every real MCP server looks like: a launcher (`npx`, `uvx`,
           ;; `docker run`) with the actual work under it. `sleep` stands in for
           ;; the worker that outlives a bare `Process.destroy` of the parent.
           (mcp/connect "fake"
                        {:transport :stdio
                         :command sh
                         :args ["-c" (str "sleep 300 & exec " py " " (.getPath f))]})

           kids
           (-> (java.lang.ProcessHandle/of (long (:pid conn)))
               (.orElse nil)
               .descendants
               .iterator
               iterator-seq
               vec)]

          (expect (= "fake" (get (:server-info conn) "name")))
          (expect (= 1 (count kids)))
          (mcp/close conn)
          ;; Give SIGTERM→SIGKILL its grace window before judging.
          (expect (all-dead-within? kids 8000)))))))

(defdescribe
  mcp-failed-stdio-handshake-cleanup-test
  (it
    "kills the stdio process when initialize fails before connect can return"
    (let
      [sh
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
        (try (let
               [failure (try (mcp/connect "never-initializes"
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
                 (let
                   [pid (parse-long (str/trim (slurp pid-file)))
                    handle (some-> (java.lang.ProcessHandle/of (long pid))
                                   (.orElse nil))]

                   (reset! pid* pid)
                   ;; `connect` has already thrown. There is no conn for a caller
                   ;; to close, so only its internal failure cleanup can pass this.
                   (expect (or (nil? handle) (all-dead-within? [handle] 8000))))))
             (finally (when-let [pid @pid*]
                        (when-let
                          [^java.lang.ProcessHandle handle (some-> (java.lang.ProcessHandle/of
                                                                     (long pid))
                                                                   (.orElse nil))]
                          (when (.isAlive handle) (.destroyForcibly handle))))
                      (.delete pid-file)))))))

(defdescribe
  mcp-transport-normalization-test
  (it "accepts the string transport values that YAML configuration supplies"
      (let
        [transport-of (ns-resolve 'com.blockether.vis.internal.foundation.mcp.client 'transport-of)]
        (expect (= :stdio (transport-of {:transport "stdio"})))
        (expect (= :streamable-http (transport-of {:transport "streamable_http"})))
        ;; Pre-canonical state keeps loading, but all new saves use the
        ;; standard Streamable HTTP spelling.
        (expect (= :streamable-http (transport-of {:transport "http"})))
        (expect (= :streamable-http (transport-of {:url "https://mcp.example.test/mcp"}))))))

(defdescribe mcp-json-encoding-total-test
             (it "encodes ANY tool argument instead of throwing inside the JSON-RPC write"
                 (let
                   [->json
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
