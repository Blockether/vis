(ns com.blockether.vis.internal.foundation.acp-test
  "ADVERSARIAL coverage for the ACP agent server.

   ACP is one JSON message per LINE over a pipe an editor owns, so the three
   things that must never happen are: a throw that escapes into the read loop, a
   write that emits an embedded newline and desynchronizes framing, and a
   permission path that fails OPEN. Every test here attacks one of those."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.foundation.acp :as acp]
            [com.blockether.vis.internal.foundation.mcp.core :as mcp]
            [lazytest.core :refer [defdescribe expect it]])
  (:import (java.io ByteArrayInputStream ByteArrayOutputStream)))

;; =============================================================================
;; Harness
;; =============================================================================

(defn- decode-line [line] (:msg (acp/decode line)))

(defn- conn+out
  "A connection plus the atom of raw lines it wrote."
  [& [opts]]
  (let
    [out
     (atom [])

     c
     (acp/connection (merge {:out-fn #(swap! out conj %) :backend (acp/echo-backend)} opts))]

    [c out]))

(defn- send!
  "Frame `m` and feed it in. Returns the response message map, or nil."
  [c m]
  (acp/handle-line! c (acp/encode m)))

(defn- code [resp] (get-in resp ["error" "code"]))

(defn- result [resp] (get resp "result"))

(defn- init!
  [c & [caps]]
  (send! c
         {"jsonrpc" "2.0"
          "id" "init"
          "method" "initialize"
          "params" {"protocolVersion" acp/protocol-version "clientCapabilities" (or caps {})}}))

(defn- new-session!
  [c]
  (get (result (send! c
                      {"jsonrpc" "2.0" "id" "new" "method" "session/new" "params" {"cwd" "/tmp"}}))
       "sessionId"))

(defn- scripted
  "A connection whose fake EDITOR answers agent→client requests with
   `(reply request)`. Returning nil means the editor stays silent, so the agent
   must time out rather than hang. Also returns the vector of outgoing lines."
  [reply & [opts]]
  (let
    [out
     (atom [])

     cell
     (atom nil)

     c
     (acp/connection (merge {:backend (acp/echo-backend)
                             :out-fn (fn [line]
                                       (swap! out conj line)
                                       (let [m (decode-line line)]
                                         (when (and (contains? m "id") (get m "method"))
                                           (when-let [r (reply m)]
                                             (acp/handle-line! @cell (acp/encode r))))))}
                            opts))]

    (reset! cell c)
    [c out]))

(defn- requests-of
  "Outgoing agent→client REQUESTS (id + method) for `method`."
  [out method]
  (->> @out
       (map decode-line)
       (filter #(and (contains? % "id") (= method (get % "method"))))
       vec))

(defn- selected
  [req option-id]
  {"jsonrpc" "2.0"
   "id" (get req "id")
   "result" {"outcome" {"outcome" "selected" "optionId" option-id}}})

(defn- caught [f] (try (f) nil (catch Throwable t t)))

(defn- temp-file!
  [content]
  (let [f (java.io.File/createTempFile "vis-acp-test" ".txt")]
    (.deleteOnExit f)
    (spit f content)
    f))

;; =============================================================================
;; Framing — the line discipline the whole protocol rests on
;; =============================================================================

(defdescribe
  framing-test
  (it "never emits an embedded newline, whatever the payload carries"
      (let
        [nasty
         (str "line1\nline2\r\nline3\u2028line4\u0000tail "
              "\"quoted\" \\ backslash 🙈 "
              (str/join (repeat 50 "\n")))

         line
         (acp/encode {"jsonrpc" "2.0" "id" 1 "result" {"text" nasty}})]

        (expect (not (str/includes? line "\n")))
        (expect (not (str/includes? line "\r")))
        ;; and it survives the round trip byte-for-byte
        (expect (= nasty (get-in (decode-line line) ["result" "text"])))))
  (it "encodes values JSON has no opinion about instead of throwing"
      (let
        [line
         (acp/encode {"jsonrpc" "2.0"
                      :keyword-key "kw"
                      1 "numeric key"
                      nil "nil key"
                      "nan" Double/NaN
                      "inf" Double/POSITIVE_INFINITY
                      "-inf" Double/NEGATIVE_INFINITY
                      "bytes" (byte-array 3)
                      "ratio" 22/7
                      "atom" (atom :x)
                      "set" #{:a :b}
                      "sym" 'foo/bar})

         m
         (decode-line line)]

        (expect (not (str/includes? line "\n")))
        (expect (= "kw" (get m "keyword-key")))
        (expect (= "numeric key" (get m "1")))
        (expect (nil? (get m "nan")))
        (expect (nil? (get m "inf")))
        (expect (nil? (get m "-inf")))
        (expect (string? (get m "atom")))
        (expect (= "foo/bar" (get m "sym")))
        (expect (= 2 (count (get m "set"))))))
  (it "survives a deeply nested client payload"
      (let
        [deep (reduce (fn [acc _]
                        {"k" acc})
                      {"leaf" 1}
                      (range 400))]
        (expect (string? (acp/encode deep)))))
  (it "classifies every shape of bad line"
      (expect (= (:parse-error acp/error-codes) (get-in (acp/decode "{not json") [:error "code"])))
      (expect (= (:invalid-request acp/error-codes) (get-in (acp/decode "") [:error "code"])))
      (expect (= (:invalid-request acp/error-codes) (get-in (acp/decode "   ") [:error "code"])))
      (expect (= (:invalid-request acp/error-codes)
                 (get-in (acp/decode "\"a string\"") [:error "code"])))
      (expect (= (:invalid-request acp/error-codes) (get-in (acp/decode "42") [:error "code"])))
      (expect (= (:invalid-request acp/error-codes) (get-in (acp/decode "null") [:error "code"])))
      ;; JSON-RPC batching is legal JSON-RPC but explicitly NOT part of ACP
      (expect (= (:invalid-request acp/error-codes)
                 (get-in (acp/decode "[{\"jsonrpc\":\"2.0\",\"method\":\"initialize\"}]")
                         [:error "code"])))
      (expect (map? (:msg (acp/decode "{}")))))
  (it "answers hostile lines with an error instead of throwing"
      (let [[c out] (conn+out)]
        (doseq
          [line ["" "  " "{" "[]" "null" "\"x\""
                 "{\"jsonrpc\":\"1.0\",\"id\":1,\"method\":\"initialize\"}"
                 "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":42}"
                 "{\"jsonrpc\":\"2.0\",\"id\":{\"bad\":true},\"method\":\"initialize\"}"]]
          (expect (nil? (caught #(acp/handle-line! c line)))))
        (expect (= 9 (count @out)))
        (expect (every? #(contains? % "error") (map decode-line @out)))))
  (it "tolerates a nil line and a giant line"
      (let [[c _] (conn+out)]
        (expect (nil? (caught #(acp/handle-line! c nil))))
        (expect (nil? (caught #(acp/handle-line!
                                 c
                                 (str "{\"a\":\"" (str/join (repeat 200000 "x")) "\"}"))))))))

;; =============================================================================
;; Dispatch — ids, notifications, ordering
;; =============================================================================

(defdescribe
  dispatch-test
  (it "answers a request, stays silent for a notification"
      (let [[c out] (conn+out)]
        (expect (some? (init! c)))
        (expect (nil? (acp/handle-line! c
                                        (acp/encode {"jsonrpc" "2.0"
                                                     "method" "session/cancel"
                                                     "params" {"sessionId" "nope"}}))))
        (expect (= 1 (count @out)))))
  (it "never answers a notification even when its handler blows up"
      (let [[c out] (conn+out)]
        (expect (nil? (acp/handle-line! c
                                        (acp/encode {"jsonrpc" "2.0"
                                                     "method" "session/prompt"
                                                     "params" {"sessionId" 42}}))))
        (expect (nil? (acp/handle-line! c
                                        (acp/encode {"jsonrpc" "2.0" "method" "no/such/method"}))))
        (expect (empty? @out))))
  (it "rejects an unknown method with -32601"
      (let [[c _] (conn+out)]
        (expect (= (:method-not-found acp/error-codes)
                   (code (send! c {"jsonrpc" "2.0" "id" 7 "method" "session/telepathy"}))))))
  (it "keeps the caller's id, including id null and id 0"
      (let [[c _] (conn+out)]
        (init! c)
        (expect (= 0
                   (get (send! c {"jsonrpc" "2.0" "id" 0 "method" "initialize" "params" {}}) "id")))
        (expect (nil? (get (send! c {"jsonrpc" "2.0" "id" nil "method" "initialize" "params" {}})
                           "id")))))
  (it "refuses an id that is neither string, number, nor null"
      (let
        [[c _]
         (conn+out)

         resp
         (send! c {"jsonrpc" "2.0" "id" ["array"] "method" "initialize"})]

        (expect (nil? (get resp "id")))
        (expect (= (:invalid-request acp/error-codes) (code resp)))))
  (it "does not confuse a client RESPONSE with a request"
      (let [[c out] (conn+out)]
        (expect (nil? (acp/handle-line! c
                                        (acp/encode {"jsonrpc" "2.0" "id" "vis-999" "result" {}}))))
        (expect (nil? (acp/handle-line! c
                                        (acp/encode {"jsonrpc" "2.0"
                                                     "id" "vis-999"
                                                     "error" {"code" -1 "message" "x"}}))))
        (expect (empty? @out))))
  (it "an out-fn that explodes closes the connection instead of killing the caller"
      (let
        [c (acp/connection {:out-fn (fn [_]
                                      (throw (ex-info "pipe closed" {})))
                            :backend (acp/echo-backend)})]
        (expect (nil? (caught #(init! c))))
        (expect (true? (:closed? @c)))
        ;; and nothing after it throws either
        (expect (nil? (caught #(send! c
                                      {"jsonrpc" "2.0"
                                       "id" 2
                                       "method" "session/new"
                                       "params" {"cwd" "/tmp"}})))))))

;; =============================================================================
;; Handshake and session lifecycle
;; =============================================================================

(defdescribe
  handshake-test
  (it "refuses session work before initialize"
      (let [[c _] (conn+out)]
        (expect (= (:not-initialized acp/error-codes)
                   (code
                     (send!
                       c
                       {"jsonrpc" "2.0" "id" 1 "method" "session/new" "params" {"cwd" "/tmp"}}))))))
  (it "negotiates an unknown protocol version down instead of failing"
      (let
        [[c _]
         (conn+out)

         r
         (result
           (send!
             c
             {"jsonrpc" "2.0" "id" 1 "method" "initialize" "params" {"protocolVersion" 9999}}))]

        (expect (= acp/protocol-version (get r "protocolVersion")))
        (expect (true? (get-in r ["agentCapabilities" "loadSession"])))))
  (it "validates initialize params"
      (let [[c _] (conn+out)]
        (expect (= (:invalid-params acp/error-codes)
                   (code (send! c
                                {"jsonrpc" "2.0"
                                 "id" 1
                                 "method" "initialize"
                                 "params" {"protocolVersion" "one"}}))))
        (expect (= (:invalid-params acp/error-codes)
                   (code (send! c
                                {"jsonrpc" "2.0"
                                 "id" 2
                                 "method" "initialize"
                                 "params" {"clientCapabilities" ["not" "an" "object"]}}))))))
  (it "advertises no auth methods and rejects an invented one"
      (let [[c _] (conn+out)]
        (expect (= [] (get (result (init! c)) "authMethods")))
        (expect (= {}
                   (result (send! c {"jsonrpc" "2.0" "id" 2 "method" "authenticate" "params" {}}))))
        (expect (= (:invalid-params acp/error-codes)
                   (code (send! c
                                {"jsonrpc" "2.0"
                                 "id" 3
                                 "method" "authenticate"
                                 "params" {"methodId" "sudo"}}))))))
  (it "insists on an absolute cwd"
      (let [[c _] (conn+out)]
        (init! c)
        (expect (= (:invalid-params acp/error-codes)
                   (code (send! c {"jsonrpc" "2.0" "id" 1 "method" "session/new" "params" {}}))))
        (expect (= (:invalid-params acp/error-codes)
                   (code (send! c
                                {"jsonrpc" "2.0"
                                 "id" 2
                                 "method" "session/new"
                                 "params" {"cwd" "relative/path"}}))))
        (expect
          (= (:invalid-params acp/error-codes)
             (code (send! c {"jsonrpc" "2.0" "id" 3 "method" "session/new" "params" {"cwd" ""}}))))
        (expect (string? (new-session! c)))))
  (it "refuses a prompt for a session it never handed out"
      (let [[c _] (conn+out)]
        (init! c)
        (new-session! c)
        (expect (some? (code (send! c
                                    {"jsonrpc" "2.0"
                                     "id" 9
                                     "method" "session/prompt"
                                     "params" {"sessionId" "someone-elses-session"
                                               "prompt" [{"type" "text" "text" "hi"}]}}))))))
  (it "refuses an empty or malformed prompt"
      (let
        [[c _]
         (conn+out)

         _
         (init! c)

         sid
         (new-session! c)

         p
         (fn [blocks]
           (code (send! c
                        {"jsonrpc" "2.0"
                         "id" 1
                         "method" "session/prompt"
                         "params" {"sessionId" sid "prompt" blocks}})))]

        (expect (= (:invalid-params acp/error-codes) (p [])))
        (expect (= (:invalid-params acp/error-codes) (p "not an array")))
        (expect (= (:invalid-params acp/error-codes) (p [{"type" "text" "text" "   "}])))
        (expect (= (:invalid-params acp/error-codes) (p [{"type" "wat"} {"nope" true}])))))
  (it "streams the answer and ends the turn"
      (let
        [[c out]
         (conn+out)

         _
         (init! c)

         sid
         (new-session! c)

         resp
         (send! c
                {"jsonrpc" "2.0"
                 "id" 5
                 "method" "session/prompt"
                 "params" {"sessionId" sid
                           "prompt" [{"type" "text" "text" "hello"}
                                     {"type" "resource_link" "uri" "file:///tmp/a.clj"}]}})

         updates
         (->> @out
              (map decode-line)
              (filter #(= "session/update" (get % "method")))
              vec)]

        (expect (= "end_turn" (get (result resp) "stopReason")))
        (expect (= 1 (count updates)))
        (expect (= "agent_message_chunk"
                   (get-in (first updates) ["params" "update" "sessionUpdate"])))
        (expect (str/includes? (get-in (first updates) ["params" "update" "content" "text"])
                               "@file:///tmp/a.clj"))))
  (it "reports a failed turn as an error, not a silent end_turn"
      (let
        [[c _]
         (conn+out {:backend (acp/echo-backend (fn [_]
                                                 {:status "failed" :error "boom"}))})

         _
         (init! c)

         sid
         (new-session! c)

         resp
         (send! c
                {"jsonrpc" "2.0"
                 "id" 5
                 "method" "session/prompt"
                 "params" {"sessionId" sid "prompt" [{"type" "text" "text" "x"}]}})]

        (expect (= (:internal-error acp/error-codes) (code resp)))
        (expect (str/includes? (get-in resp ["error" "message"]) "boom"))))
  (it "reports a cancelled turn as cancelled and frees the session again"
      (let
        [[c _]
         (conn+out {:backend (acp/echo-backend (fn [_]
                                                 {:status "cancelled"}))})

         _
         (init! c)

         sid
         (new-session! c)

         resp
         (send! c
                {"jsonrpc" "2.0"
                 "id" 5
                 "method" "session/prompt"
                 "params" {"sessionId" sid "prompt" [{"type" "text" "text" "x"}]}})]

        (expect (= "cancelled" (get (result resp) "stopReason")))
        ;; the session must be idle again, not wedged in :running
        (expect (= "cancelled"
                   (get (result (send! c
                                       {"jsonrpc" "2.0"
                                        "id" 6
                                        "method" "session/prompt"
                                        "params" {"sessionId" sid
                                                  "prompt" [{"type" "text" "text" "x"}]}}))
                        "stopReason")))))
  (it "does not wedge the session when the backend throws"
      (let
        [[c _]
         (conn+out {:backend (acp/echo-backend (fn [_]
                                                 (throw (ex-info "backend died" {}))))})

         _
         (init! c)

         sid
         (new-session! c)]

        (expect (some? (code (send! c
                                    {"jsonrpc" "2.0"
                                     "id" 5
                                     "method" "session/prompt"
                                     "params" {"sessionId" sid
                                               "prompt" [{"type" "text" "text" "x"}]}}))))
        (expect (= :idle (get-in @c [:sessions sid :state])))))
  (it
    "refuses a second concurrent prompt for one session"
    (let
      [gate
       (promise)

       [c _]
       (conn+out {:backend (acp/echo-backend (fn [_]
                                               @gate
                                               {:status "completed"}))})

       _
       (init! c)

       sid
       (new-session! c)

       f
       (future (send! c
                      {"jsonrpc" "2.0"
                       "id" 1
                       "method" "session/prompt"
                       "params" {"sessionId" sid "prompt" [{"type" "text" "text" "a"}]}}))]

      ;; wait until the first prompt is actually running
      (while (not= :running (get-in @c [:sessions sid :state])) (Thread/sleep 1))
      (expect (= (:invalid-request acp/error-codes)
                 (code (send! c
                              {"jsonrpc" "2.0"
                               "id" 2
                               "method" "session/prompt"
                               "params" {"sessionId" sid "prompt" [{"type" "text" "text" "b"}]}}))))
      (deliver gate true)
      (expect (= "end_turn" (get (result @f) "stopReason")))))
  (it
    "replays a loaded session as update notifications"
    (let
      [backend
       (assoc (acp/echo-backend)
         :load-session (fn [_]
                         [{:role "user" :text "first"} {:role "assistant" :text "second"}
                          {:role "assistant" :text "   "}]))

       [c out]
       (conn+out {:backend backend})

       _
       (init! c)

       resp
       (send! c
              {"jsonrpc" "2.0"
               "id" 3
               "method" "session/load"
               "params" {"sessionId" "sess-1" "cwd" "/tmp"}})

       updates
       (->> @out
            (map decode-line)
            (filter #(= "session/update" (get % "method")))
            vec)]

      (expect (= {} (result resp)))
      (expect (= 2 (count updates)))
      (expect (= "user_message_chunk" (get-in (first updates) ["params" "update" "sessionUpdate"])))
      (expect (= "agent_message_chunk"
                 (get-in (second updates) ["params" "update" "sessionUpdate"])))
      ;; and a loaded session can immediately be prompted
      (expect (= "end_turn"
                 (get (result (send! c
                                     {"jsonrpc" "2.0"
                                      "id" 4
                                      "method" "session/prompt"
                                      "params" {"sessionId" "sess-1"
                                                "prompt" [{"type" "text" "text" "go"}]}}))
                      "stopReason"))))))

;; =============================================================================
;; Content blocks and event translation
;; =============================================================================

(defdescribe
  content-block-test
  (it "flattens every block type, and refuses to explode on junk"
      (expect (= "hi" (acp/content-block->text {"type" "text" "text" "hi"})))
      (expect (= "@file:///a" (acp/content-block->text {"type" "resource_link" "uri" "file:///a"})))
      (expect (= "<file:///a>\nbody"
                 (acp/content-block->text {"type" "resource"
                                           "resource" {"uri" "file:///a" "text" "body"}})))
      (expect (= "body" (acp/content-block->text {"type" "resource" "resource" {"text" "body"}})))
      (expect (= "[image]" (acp/content-block->text {"type" "image" "data" "…"})))
      (expect (= "[audio]" (acp/content-block->text {"type" "audio"})))
      (expect (nil? (acp/content-block->text nil)))
      (expect (nil? (acp/content-block->text "just a string")))
      (expect (nil? (acp/content-block->text [])))
      (expect (nil? (acp/content-block->text {"type" "text" "text" 42})))
      (expect (nil? (acp/content-block->text {"type" "text"})))
      (expect (nil? (acp/content-block->text {"type" "resource_link" "uri" nil})))
      (expect (nil? (acp/content-block->text {"type" "resource" "resource" "nope"})))
      (expect (nil? (acp/content-block->text {"type" "quantum"}))))
  (it "joins a prompt and drops the blanks"
      (expect (= "a\nb"
                 (acp/prompt->text [{"type" "text" "text" "a"} {"type" "text" "text" ""} nil
                                    {"type" "unknown"} {"type" "text" "text" "b"}])))
      (expect (= "" (acp/prompt->text [])))
      (expect (some? (caught #(acp/prompt->text {"not" "a list"}))))))

(defdescribe
  event-translation-test
  (it "maps only the events that mean something to an editor"
      (expect (= {"sessionUpdate" "agent_message_chunk" "content" {"type" "text" "text" "hi"}}
                 (acp/event->update {"type" "content.block.delta" "field" "markdown" "text" "hi"})))
      (expect (= "agent_thought_chunk"
                 (get (acp/event->update
                        {"type" "content.block.delta" "field" "text" "text" "thinking"})
                      "sessionUpdate")))
      (expect (nil? (acp/event->update
                      {"type" "content.block.delta" "field" "markdown" "text" ""})))
      (expect (nil? (acp/event->update {"type" "content.block.delta" "field" "markdown"})))
      (expect (nil? (acp/event->update {"type" "content.block.delta" "field" "usage" "text" "x"})))
      (expect (nil? (acp/event->update {"type" "session.heartbeat"})))
      (expect (nil? (acp/event->update nil)))
      (expect (nil? (acp/event->update "not a map")))
      (expect (nil? (acp/event->update {}))))
  (it "maps tool blocks onto tool_call updates with a kind"
      (let
        [u (acp/event->update {"type" "block.started"
                               "block" {"type" "tool" "id" "b1" "tool" "write"}})]
        (expect (= "tool_call" (get u "sessionUpdate")))
        (expect (= "b1" (get u "toolCallId")))
        (expect (= "edit" (get u "kind")))
        (expect (= "in_progress" (get u "status"))))
      (expect (= "pending"
                 (get (acp/event->update {"type" "block.preview"
                                          "block" {"type" "tool" "id" "b" "tool" "cat"}})
                      "status")))
      (expect (nil? (acp/event->update {"type" "block.started" "block" {"type" "text"}})))
      (expect (= "failed"
                 (get (acp/event->update {"type" "block.output"
                                          "block" {"id" "b" "status" "error"}})
                      "status")))
      (expect (= "completed"
                 (get (acp/event->update {"type" "block.output" "block" {"id" "b"}}) "status"))))
  (it "classifies tool kinds, unknown tools included"
      (expect (= "read" (acp/tool-kind "cat")))
      (expect (= "edit" (acp/tool-kind :struct_patch)))
      (expect (= "search" (acp/tool-kind "grep")))
      (expect (= "execute" (acp/tool-kind "shell")))
      (expect (= "move" (acp/tool-kind "fs")))
      (expect (= "other" (acp/tool-kind nil)))
      (expect (= "other" (acp/tool-kind "some_extension_tool")))))

;; =============================================================================
;; Client-bound calls — the part a hostile or dead editor can hang
;; =============================================================================

(defdescribe client-call-test
             (it "times out instead of hanging forever when the editor never answers"
                 (let
                   [[c _]
                    (scripted (fn [_]
                                nil))

                    t0
                    (System/currentTimeMillis)

                    r
                    (binding [acp/*client-call-timeout-ms* 60]
                      (acp/call! c "fs/read_text_file" {"path" "/tmp/x"}))]

                   (expect (some? (:error r)))
                   (expect (< (- (System/currentTimeMillis) t0) 5000))
                   ;; the pending slot must not leak
                   (expect (empty? (:pending @c)))))
             (it "refuses to issue client requests over a half-duplex transport"
                 (let
                   [[c out]
                    (scripted (fn [_]
                                nil)
                              {:half-duplex? true})

                    r
                    (acp/call! c "fs/read_text_file" {"path" "/tmp/x"})]

                   (expect (= (:method-not-found acp/error-codes) (get-in r [:error "code"])))
                   (expect (empty? @out))))
             (it "correlates concurrent client calls by id"
                 (let
                   [[c _]
                    (scripted (fn [req]
                                {"jsonrpc" "2.0"
                                 "id" (get req "id")
                                 "result" {"echo" (get-in req ["params" "n"])}}))

                    answers
                    (->> (range 25)
                         (mapv (fn [n]
                                 (future (acp/call! c "fs/read_text_file" {"n" n}))))
                         (mapv deref))]

                   (expect (= (set (range 25)) (set (map #(get-in % [:result "echo"]) answers))))
                   (expect (empty? (:pending @c)))))
             (it "reads the client capability tree without NPEing on missing branches"
                 (let [[c _] (conn+out)]
                   (init! c {"fs" {"readTextFile" true "writeTextFile" false}})
                   (expect (true? (acp/client-supports? c "fs" "readTextFile")))
                   (expect (false? (acp/client-supports? c "fs" "writeTextFile")))
                   (expect (false? (acp/client-supports? c "fs" "nope")))
                   (expect (false? (acp/client-supports? c "terminal")))
                   (expect (false? (acp/client-supports? c "a" "b" "c" "d"))))))

(defdescribe fs-delegation-test
             (it "reads the editor's unsaved buffer when the client offers one"
                 (let
                   [f
                    (temp-file! "ON DISK")

                    [c _]
                    (scripted
                      (fn [req]
                        {"jsonrpc" "2.0" "id" (get req "id") "result" {"content" "IN BUFFER"}}))]

                   (init! c {"fs" {"readTextFile" true}})
                   (expect (= "IN BUFFER" (acp/read-text-file! c "s" (.getPath f))))))
             (it "falls back to disk when the client offers nothing"
                 (let
                   [f
                    (temp-file! "ON DISK")

                    [c _]
                    (scripted (fn [_]
                                nil))]

                   (init! c {})
                   (expect (= "ON DISK" (acp/read-text-file! c "s" (.getPath f))))))
             (it "surfaces an editor fs error instead of pretending the read worked"
                 (let
                   [[c _] (scripted (fn [req]
                                      {"jsonrpc" "2.0"
                                       "id" (get req "id")
                                       "error" {"code" -32603 "message" "no such buffer"}}))]
                   (init! c {"fs" {"readTextFile" true}})
                   (let [t (caught #(acp/read-text-file! c "s" "/tmp/whatever"))]
                     (expect (instance? clojure.lang.ExceptionInfo t))
                     (expect (str/includes? (ex-message t) "no such buffer")))))
             (it "reports honestly that a write went nowhere when the client cannot take it"
                 (let
                   [[c _] (scripted (fn [_]
                                      nil))]
                   (init! c {"fs" {"readTextFile" true}})
                   (expect (false? (acp/write-text-file! c "s" "/tmp/x" "content"))))))

;; =============================================================================
;; Permission — must fail CLOSED
;; =============================================================================

(defdescribe
  permission-test
  (it "maps each selected option onto its decision"
      (doseq
        [[opt expected] {"allow-once" :allow-once
                         "allow-always" :allow-always
                         "reject-once" :reject-once
                         "reject-always" :reject-always}]
        (let
          [[c _] (scripted (fn [req]
                             (selected req opt)))]
          (expect (= expected (acp/request-permission! c "s" {"title" "write"}))))))
  (it "fails CLOSED on every hostile or absent answer"
      (let
        [hostile
         [;; editor never answers
          (fn [_]
            nil)
          ;; editor errors
          (fn [req]
            {"jsonrpc" "2.0" "id" (get req "id") "error" {"code" -1 "message" "nope"}})
          ;; editor cancels
          (fn [req]
            {"jsonrpc" "2.0" "id" (get req "id") "result" {"outcome" {"outcome" "cancelled"}}})
          ;; editor invents an option we never offered
          (fn [req]
            (selected req "allow-everything-forever"))
          ;; editor answers with junk
          (fn [req]
            {"jsonrpc" "2.0" "id" (get req "id") "result" {}})
          (fn [req]
            {"jsonrpc" "2.0" "id" (get req "id") "result" {"outcome" "selected"}})]]
        (doseq [reply hostile]
          (let
            [[c _] (scripted reply)
             d (binding [acp/*client-call-timeout-ms* 60]
                 (acp/request-permission! c "s" {"title" "write"}))]

            (expect (contains? #{:cancelled :reject-once :reject-always} d))))))
  (it "offers the four standard options"
      (let
        [[c out] (scripted (fn [req]
                             (selected req "allow-once")))]
        (acp/request-permission! c "s" {"title" "write"})
        (let [req (first (requests-of out "session/request_permission"))]
          (expect (= #{"allow-once" "allow-always" "reject-once" "reject-always"}
                     (set (map #(get % "optionId") (get-in req ["params" "options"])))))))))

;; =============================================================================
;; Op-hook — the editor gates and then sees every mutation
;; =============================================================================

(defn- with-registered
  [c f]
  (let [sid (str "acp-test-" (System/nanoTime))]
    (acp/register-connection! sid c)
    (try (f sid) (finally (acp/unregister-connection! sid)))))

(defdescribe
  op-hook-test
  (it "is a pass-through outside an ACP session"
      (let [called (atom 0)]
        (expect (= :ok
                   (acp/around-hook {:session-id "not-acp"}
                                    :write
                                    {"path" "/tmp/x"}
                                    (fn [_]
                                      (swap! called inc)
                                      :ok))))
        (expect (= 1 @called))))
  (it "is a pass-through over the half-duplex HTTP transport, never a hang"
      (let
        [[c out] (scripted (fn [_]
                             nil)
                           {:half-duplex? true})]
        (with-registered c
                         (fn [sid]
                           (expect (= :ok
                                      (binding [acp/*permission-ops* #{:write}]
                                        (acp/around-hook {:session-id sid}
                                                         :write
                                                         {"path" "/tmp/x"}
                                                         (fn [_]
                                                           :ok)))))
                           (expect (empty? (requests-of out "session/request_permission")))))))
  (it "denies the op — and never runs it — when the editor rejects"
      (let
        [[c _]
         (scripted (fn [req]
                     (selected req "reject-once")))

         ran
         (atom 0)]

        (init! c {})
        (with-registered c
                         (fn [sid]
                           (let
                             [t (binding [acp/*permission-ops* #{:write}]
                                  (caught #(acp/around-hook {:session-id sid}
                                                            :write
                                                            {"path" "/tmp/x"}
                                                            (fn [_]
                                                              (swap! ran inc)
                                                              :ok))))]
                             (expect (instance? clojure.lang.ExceptionInfo t))
                             (expect (true? (:acp/denied (ex-data t))))
                             (expect (zero? @ran)))))))
  (it "denies when the editor is silent (fail closed, not fail open)"
      (let
        [[c _]
         (scripted (fn [_]
                     nil))

         ran
         (atom 0)]

        (init! c {})
        (with-registered c
                         (fn [sid]
                           (let
                             [t (binding
                                  [acp/*permission-ops* #{:write}
                                   acp/*client-call-timeout-ms* 60]

                                  (caught #(acp/around-hook {:session-id sid}
                                                            :write
                                                            {"path" "/tmp/x"}
                                                            (fn [_]
                                                              (swap! ran inc)
                                                              :ok))))]
                             (expect (instance? clojure.lang.ExceptionInfo t))
                             (expect (zero? @ran)))))))
  (it "remembers allow-always and stops asking"
      (let
        [[c out]
         (scripted (fn [req]
                     (selected req "allow-always")))

         ran
         (atom 0)]

        (init! c {})
        (with-registered c
                         (fn [sid]
                           (binding
                             [acp/*permission-ops*
                              #{:write}

                              acp/*mirror-ops*
                              #{}]

                             (dotimes [_ 4]
                               (acp/around-hook {:session-id sid}
                                                :write
                                                {"path" "/tmp/x"}
                                                (fn [_]
                                                  (swap! ran inc)))))
                           (expect (= 4 @ran))
                           (expect (= 1 (count (requests-of out "session/request_permission"))))))))
  (it "remembers reject-always and stops both asking and running"
      (let
        [[c out]
         (scripted (fn [req]
                     (selected req "reject-always")))

         ran
         (atom 0)]

        (init! c {})
        (with-registered c
                         (fn [sid]
                           (binding [acp/*permission-ops* #{:write}]
                             (dotimes [_ 4]
                               (caught #(acp/around-hook {:session-id sid}
                                                         :write
                                                         {"path" "/tmp/x"}
                                                         (fn [_]
                                                           (swap! ran inc))))))
                           (expect (zero? @ran))
                           (expect (= 1 (count (requests-of out "session/request_permission"))))))))
  (it "keeps decisions per op, not per session"
      (let
        [[c out] (scripted (fn [req]
                             (selected req "allow-always")))]
        (init! c {})
        (with-registered c
                         (fn [sid]
                           (binding
                             [acp/*permission-ops* #{:write :patch}
                              acp/*mirror-ops* #{}]

                             (acp/around-hook {:session-id sid}
                                              :write
                                              {"path" "/tmp/x"}
                                              (fn [_]
                                                :ok))
                             (acp/around-hook {:session-id sid}
                                              :write
                                              {"path" "/tmp/x"}
                                              (fn [_]
                                                :ok))
                             (acp/around-hook {:session-id sid}
                                              :patch
                                              {"path" "/tmp/x"}
                                              (fn [_]
                                                :ok)))
                           (expect (= 2 (count (requests-of out "session/request_permission"))))))))
  (it "mirrors the touched files into the editor's buffers after a successful edit"
      (let
        [f
         (temp-file! "AFTER EDIT")

         [c out]
         (scripted (fn [req]
                     (selected req "allow-once")))]

        (init! c {"fs" {"writeTextFile" true}})
        (with-registered c
                         (fn [sid]
                           (binding
                             [acp/*permission-ops*
                              #{}

                              acp/*mirror-ops*
                              #{:write}]

                             (acp/around-hook {:session-id sid}
                                              :write
                                              {"path" (.getPath f)}
                                              (fn [_]
                                                {:changed true})))
                           (let [w (first (requests-of out "fs/write_text_file"))]
                             (expect (= (.getPath f) (get-in w ["params" "path"])))
                             (expect (= "AFTER EDIT" (get-in w ["params" "content"]))))))))
  (it "never mirrors when the client has no writeTextFile"
      (let
        [f
         (temp-file! "x")

         [c out]
         (scripted (fn [req]
                     (selected req "allow-once")))]

        (init! c {})
        (with-registered c
                         (fn [sid]
                           (binding
                             [acp/*permission-ops*
                              #{}

                              acp/*mirror-ops*
                              #{:write}]

                             (acp/around-hook {:session-id sid}
                                              :write
                                              {"path" (.getPath f)}
                                              (fn [_]
                                                :ok)))
                           (expect (empty? (requests-of out "fs/write_text_file")))))))
  (it "never lets a broken mirror fail the tool that already succeeded"
      (let
        [f
         (temp-file! "x")

         [c _]
         (scripted
           (fn [req]
             (if (= "fs/write_text_file" (get req "method"))
               {"jsonrpc" "2.0" "id" (get req "id") "error" {"code" -1 "message" "buffer gone"}}
               (selected req "allow-once"))))]

        (init! c {"fs" {"writeTextFile" true}})
        (with-registered c
                         (fn [sid]
                           (binding
                             [acp/*permission-ops*
                              #{:write}

                              acp/*mirror-ops*
                              #{:write}]

                             (expect (= :ok
                                        (acp/around-hook {:session-id sid}
                                                         :write
                                                         {"path" (.getPath f)}
                                                         (fn [_]
                                                           :ok)))))))))
  (it "does not mirror a file the tool did not touch, or an oversized one"
      (let
        [big
         (temp-file! (str/join (repeat 4096 "x")))

         [c out]
         (scripted (fn [req]
                     (selected req "allow-once")))]

        (init! c {"fs" {"writeTextFile" true}})
        (with-registered c
                         (fn [sid]
                           (binding
                             [acp/*permission-ops*
                              #{}

                              acp/*mirror-ops*
                              #{:write}

                              acp/*max-mirror-bytes*
                              16]

                             (acp/around-hook {:session-id sid}
                                              :write
                                              {"path" (.getPath big)}
                                              (fn [_]
                                                :ok))
                             (acp/around-hook {:session-id sid}
                                              :write
                                              {"path" "/tmp/does-not-exist-9182"}
                                              (fn [_]
                                                :ok)))
                           (expect (empty? (requests-of out "fs/write_text_file")))))))
  (it "propagates the tool's own failure untouched"
      (let
        [[c _] (scripted (fn [req]
                           (selected req "allow-once")))]
        (init! c {})
        (with-registered c
                         (fn [sid]
                           (let
                             [t (binding [acp/*permission-ops* #{:write}]
                                  (caught #(acp/around-hook {:session-id sid}
                                                            :write
                                                            {"path" "/tmp/x"}
                                                            (fn [_]
                                                              (throw (ex-info "tool exploded"
                                                                              {:mine true}))))))]
                             (expect (= "tool exploded" (ex-message t)))
                             (expect (true? (:mine (ex-data t))))
                             (expect (nil? (:acp/denied (ex-data t))))))))))

(defdescribe arg-paths-test
             (it "finds every path at any depth and ignores everything else"
                 (expect (= ["/a"] (acp/arg-paths {"path" "/a"})))
                 (expect (= ["/a"] (acp/arg-paths {:path "/a"})))
                 (expect (= ["/a" "/b"] (acp/arg-paths {"edits" [{"path" "/a"} {"path" "/b"}]})))
                 (expect (= ["/a" "/b"] (acp/arg-paths {"src" "/a" "dest" "/b"})))
                 (expect (= ["/a"] (acp/arg-paths {"edits" [{"path" "/a"} {"path" "/a"}]})))
                 (expect (= [] (acp/arg-paths {"path" ""})))
                 (expect (= [] (acp/arg-paths {"path" 42})))
                 (expect (= [] (acp/arg-paths {"paths" ["/a"]})))
                 (expect (= [] (acp/arg-paths nil)))
                 (expect (= [] (acp/arg-paths "string")))
                 (expect (= ["/deep"] (acp/arg-paths {"a" {"b" {"c" [[[{"path" "/deep"}]]]}}})))))

;; =============================================================================
;; The stdio loop an editor actually spawns
;; =============================================================================

(defdescribe
  serve-loop-test
  (it
    "answers every line, skips the junk, and exits cleanly on EOF"
    (let
      [in
       (str (acp/encode {"jsonrpc" "2.0" "id" 1 "method" "initialize" "params" {}})
            "\n" "\n"
            "not json at all\n"
            (acp/encode {"jsonrpc" "2.0" "id" 2 "method" "session/new" "params" {"cwd" "/tmp"}})
            "\n" (acp/encode {"jsonrpc" "2.0" "method" "session/cancel" "params" {"sessionId" "x"}})
            "\n"
            ;; a final line with no trailing newline must still be processed
            (acp/encode {"jsonrpc" "2.0" "id" 3 "method" "session/telepathy"}))

       out
       (ByteArrayOutputStream.)

       done
       (future (acp/serve! {:in (ByteArrayInputStream. (.getBytes in "UTF-8"))
                            :out out
                            :backend (acp/echo-backend)}))

       _
       (deref done 10000 ::timeout)

       lines
       (->> (str/split-lines (.toString out "UTF-8"))
            (remove str/blank?)
            (mapv decode-line))]

      (expect (not= ::timeout (deref done 1 ::timeout)))
      ;; initialize, ONE framing error for the garbage line, session/new and
      ;; method-not-found. Blank lines are transport noise and are never
      ;; answered; a notification never gets a reply.
      (expect (= 4 (count lines)))
      ;; every message is handled on its own virtual thread (so `session/cancel`
      ;; can land mid-prompt), so answers may come back out of order: the SET of
      ;; ids is the contract, the sequence is not
      (expect (= #{1 nil 2 3} (set (map #(get % "id") lines))))
      (let [by-id (into {} (map (juxt #(get % "id") identity)) lines)]
        (expect (string? (get-in by-id [2 "result" "sessionId"])))
        (expect (= (:method-not-found acp/error-codes) (get-in by-id [3 "error" "code"])))
        (expect (= (:parse-error acp/error-codes) (get-in by-id [nil "error" "code"]))))
      ;; every answer is exactly one line
      (expect (every? #(= 1 (count (str/split-lines (acp/encode %)))) lines)))))

;; =============================================================================
;; Extension wiring
;; =============================================================================

(defdescribe extension-shape-test
             (it "registers as a vis extension with hooks, routes, and a slash command"
                 (expect (map? acp/vis-extension))
                 (expect (seq acp/op-hooks))
                 (expect (every? #(= :around (:phase %)) acp/op-hooks))
                 (expect (every? #(fn? (:fn %)) acp/op-hooks))
                 (expect (= (set (concat acp/*permission-ops* acp/*mirror-ops*))
                            (set (map :op acp/op-hooks))))
                 (expect (seq (acp/routes-contribution)))
                 (expect (seq acp/slash-specs)))
             (it "gates the mutating ops, not the read-only ones"
                 (expect (contains? acp/*permission-ops* :write))
                 (expect (contains? acp/*permission-ops* :patch))
                 (expect (contains? acp/*permission-ops* :struct_patch))
                 (expect (not (contains? acp/*permission-ops* :cat)))
                 (expect (not (contains? acp/*permission-ops* :grep))))
             (it "keeps the JSON-RPC error codes on their standard values"
                 (expect (= -32700 (:parse-error acp/error-codes)))
                 (expect (= -32600 (:invalid-request acp/error-codes)))
                 (expect (= -32601 (:method-not-found acp/error-codes)))
                 (expect (= -32602 (:invalid-params acp/error-codes)))
                 (expect (= -32603 (:internal-error acp/error-codes)))
                 (expect (contains? acp/supported-protocol-versions acp/protocol-version))))

;; =============================================================================
;; Round 2 regressions: HTTP transport isolation and TOTAL encoding
;; =============================================================================

(defn- http-post!
  "One POST through the real ACP route handler, the way ring delivers it."
  [client-id msg]
  (decode-line (:body (#'acp/acp-handler
                       {:query-params {"client" (str client-id)}
                        :body (ByteArrayInputStream. (.getBytes (acp/encode msg) "UTF-8"))}))))

(defn- nest-maps
  "`n` levels of `{\"a\" …}` — deeper than any recursive encoder's stack."
  [^long n leaf]
  (reduce (fn [acc _]
            {"a" acc})
          leaf
          (range n)))

(defn- map-depth
  [x]
  (loop
    [v
     x

     d
     0]

    (if (map? v) (recur (get v "a") (inc d)) d)))

(defn- map-leaf
  [x]
  (loop [v x]
    (if (map? v) (recur (get v "a")) v)))

(defdescribe
  http-transport-test
  (it "gives each concurrent request on ONE client id only its OWN reply"
      (let
        [n
         64

         answers
         (->> (range n)
              (mapv (fn [i]
                      (future (http-post! "shared-client"
                                          {"jsonrpc" "2.0" "id" i "method" "no/such/method"}))))
              (mapv deref))]

        (expect (= n (count answers)))
        (expect (every? #(= 1 (count (get % "messages"))) answers))
        (expect (= (vec (range n)) (mapv #(get (first (get % "messages")) "id") answers)))))
  (it "keeps distinct client ids on distinct connections"
      (expect (= "cid-a"
                 (get (http-post! "cid-a" {"jsonrpc" "2.0" "id" 1 "method" "no/such/method"})
                      "client")))
      (expect (= "cid-b"
                 (get (http-post! "cid-b" {"jsonrpc" "2.0" "id" 1 "method" "no/such/method"})
                      "client")))
      (expect (not (identical? (#'acp/http-connection "cid-a") (#'acp/http-connection "cid-b"))))
      (expect (identical? (#'acp/http-connection "cid-a") (#'acp/http-connection "cid-a"))))
  (it "caps the connection table instead of growing it forever"
      (dotimes [i 300]
        (http-post! (str "throwaway-" i) {"jsonrpc" "2.0" "id" i "method" "no/such/method"}))
      (expect (<= (count @@#'acp/http-connections) 64))
      (expect (contains? @@#'acp/http-connections "throwaway-299"))))

(defdescribe
  total-encoding-test
  (it "truncates nesting past the limit instead of overflowing the stack"
      (let [safe (acp/json-safe (nest-maps 5000 "leaf"))]
        (expect (map? safe))
        (expect (<= (map-depth safe) 65))
        (expect (str/includes? (str (map-leaf safe)) "truncated"))))
  (it "never throws or emits a newline while encoding a deep message"
      (let [line (acp/encode {"jsonrpc" "2.0" "id" 1 "result" (nest-maps 5000 "leaf")})]
        (expect (string? line))
        (expect (not (str/includes? line "\n")))))
  (it "truncates a deep VECTOR the same way"
      (let
        [deep
         (reduce (fn [acc _]
                   [acc])
                 ["leaf"]
                 (range 5000))

         line
         (acp/encode {"jsonrpc" "2.0" "id" 1 "result" deep})]

        (expect (string? line))
        (expect (not (str/includes? line "\n")))))
  (it "leaves shallow values byte-identical — only containers are truncated"
      (expect (= {"a" 1 "b" ["x" true nil]} (acp/json-safe {"a" 1 "b" ["x" true nil]})))
      (expect (= "x" (acp/json-safe "x")))
      (expect (= 7 (acp/json-safe 7)))
      (expect (= [] (acp/json-safe [])))
      (expect (= {} (acp/json-safe {}))))
  (it "answers a deep request over the line protocol without desynchronizing framing"
      (let [[c out] (conn+out)]
        (send! c
               {"jsonrpc" "2.0" "id" 9 "method" "no/such/method" "params" (nest-maps 5000 "leaf")})
        (expect (every? #(not (str/includes? % "\n")) @out)))))

;; =============================================================================
;; Round 3 regressions: parked HTTP notifications and UNBOUNDED sequences
;; =============================================================================

(defdescribe
  http-backlog-test
  (it "delivers notifications emitted between polls, then drains and caps them"
      (let
        [conn
         (#'acp/http-connection "backlog-client")

         box
         (:outbox @conn)

         ;; Engine threads have no request in flight, so these lines have nowhere
         ;; to go but the parked buffer.
         t
         (Thread. ^Runnable
                  (fn []
                    (dotimes [i 300]
                      (acp/notify! conn "session/update" {"n" i}))))]

        (.start t)
        (.join t)
        (expect (<= (count @box) 256))
        (let
          [msgs (get (http-post! "backlog-client"
                                 {"jsonrpc" "2.0" "id" 1 "method" "no/such/method"})
                     "messages")]
          (expect (< 1 (count msgs)))
          (expect (= "session/update" (get (first msgs) "method")))
          (expect (= 1 (get (last msgs) "id")))
          (expect (zero? (count @box)))
          (expect (= 1
                     (count (get (http-post! "backlog-client"
                                             {"jsonrpc" "2.0" "id" 2 "method" "no/such/method"})
                                 "messages"))))))))

(defdescribe unbounded-encoding-test
             (it "truncates an UNBOUNDED lazy sequence instead of realizing it forever"
                 (let [v (deref (future (acp/json-safe (repeat "x"))) 10000 :hung)]
                   (expect (vector? v))
                   (expect (= 10001 (count v)))
                   (expect (str/includes? (str (last v)) "truncated"))))
             (it "encodes a message carrying an infinite sequence without hanging the writer"
                 (let
                   [line (deref (future (acp/encode
                                          {"jsonrpc" "2.0" "id" 1 "result" (iterate inc 0)}))
                                10000
                                :hung)]
                   (expect (string? line))
                   (expect (not (str/includes? line "\n")))))
             (it "leaves a sequence at or under the cap byte-identical"
                 (expect (= [1 2 3] (acp/json-safe [1 2 3])))
                 (expect (= (vec (range 10000)) (acp/json-safe (vec (range 10000)))))))

;; =============================================================================
;; Concurrency — `serve!` runs EVERY line on its own virtual thread
;; =============================================================================

(defn- prompt-msg
  [id sid]
  {"jsonrpc" "2.0"
   "id" id
   "method" "session/prompt"
   "params" {"sessionId" sid "prompt" [{"type" "text" "text" "hi"}]}})

(defdescribe
  concurrent-prompt-test
  (it
    "lets exactly ONE prompt own a session however many arrive at once"
    (let
      [gate
       (promise)

       started
       (atom 0)

       backend
       (assoc (acp/echo-backend)
         :prompt (fn [_]
                   (swap! started inc)
                   (deref gate 5000 ::timeout)
                   {:status "completed"}))

       [c _]
       (conn+out {:backend backend})

       _
       (init! c)

       sid
       (new-session! c)

       answers
       (mapv (fn [i]
               (future (send! c (prompt-msg i sid))))
             (range 8))

       _
       (loop [n 0]
         (when (and (< n 500) (zero? @started)) (Thread/sleep 2) (recur (inc n))))

       ;; give every loser time to get past the guard too
       _
       (Thread/sleep 100)

       in-flight
       @started

       _
       (deliver gate true)

       resps
       (mapv #(deref % 10000 ::timeout) answers)]

      ;; a read-then-write guard lets all 8 through: two turns then interleave
      ;; their `session/update` streams and the first to finish marks the session
      ;; idle under the others
      (expect (= 1 in-flight))
      (expect (= 1 (count (filter #(contains? % "result") resps))))
      (expect (= 7 (count (filter #(= (:invalid-request acp/error-codes) (code %)) resps))))
      ;; and the session is idle again, never wedged in :running
      (expect (= :idle (get-in @c [:sessions sid :state])))
      ;; nobody cancelled anything, so no watermark may appear out of thin air
      (expect (nil? (get-in @c [:sessions sid :cancel-mark])))
      ;; and all 8 lines took a turn number of their own, winner and losers alike
      (expect (= 8 (get-in @c [:sessions sid :turn])))))
  (it
    "still runs two DIFFERENT sessions of one connection at the same time"
    (let
      [gate
       (promise)

       started
       (atom 0)

       backend
       (assoc (acp/echo-backend)
         :prompt (fn [_]
                   (swap! started inc)
                   (deref gate 5000 ::timeout)
                   {:status "completed"}))

       [c _]
       (conn+out {:backend backend})

       _
       (init! c)

       a
       (new-session! c)

       b
       (new-session! c)

       answers
       [(future (send! c (prompt-msg 1 a))) (future (send! c (prompt-msg 2 b)))]

       _
       (loop [n 0]
         (when (and (< n 500) (< @started 2)) (Thread/sleep 2) (recur (inc n))))

       in-flight
       @started

       _
       (deliver gate true)

       resps
       (mapv #(deref % 10000 ::timeout) answers)]

      ;; the claim is per SESSION, not per connection
      (expect (= 2 in-flight))
      (expect (every? #(= "end_turn" (get-in % ["result" "stopReason"])) resps)))))

;; =============================================================================
;; Cancel bookkeeping — a notification anyone can send, with any id
;; =============================================================================

(defdescribe
  cancel-bookkeeping-test
  (it
    "never remembers a session id it did not hand out"
    (let
      [seen
       (atom [])

       backend
       (assoc (acp/echo-backend)
         :cancel (fn [{:keys [session-id]}]
                   (swap! seen conj session-id)
                   true))

       [c out]
       (conn+out {:backend backend})

       _
       (init! c)

       sid
       (new-session! c)

       quiet
       (count @out)]

      (dotimes [i 2000]
        (acp/handle-line! c
                          (acp/encode {"jsonrpc" "2.0"
                                       "method" "session/cancel"
                                       "params" {"sessionId" (str "ghost-" i)}})))
      ;; `session/cancel` is a NOTIFICATION: unknown ids must not accumulate, or a
      ;; chatty editor grows the connection without bound and never sees an error
      ;; a ghost id must not even create the session state a mark could hang on
      (expect (= #{sid} (set (keys (:sessions @c)))))
      (expect (nil? (get-in @c [:sessions sid :cancel-mark])))
      (expect (empty? @seen))
      ;; a notification is never answered, whatever it carried
      (expect (= quiet (count @out)))
      ;; and a cancel for a KNOWN session with no turn in flight is ignored too:
      ;; remembering it would abort the next prompt instead — a turn the user did
      ;; ask for
      (acp/handle-line! c
                        (acp/encode
                          {"jsonrpc" "2.0" "method" "session/cancel" "params" {"sessionId" sid}}))
      (expect (nil? (get-in @c [:sessions sid :cancel-mark])))
      (expect (empty? @seen))
      (expect (= "end_turn" (get (result (send! c (prompt-msg 9 sid))) "stopReason")))))
  (it
    "cancels the turn it names and nothing that comes after it"
    (let
      [cell
       (atom nil)

       turns
       (atom 0)

       backend
       (assoc (acp/echo-backend)
         :prompt (fn [{:keys [session-id]}]
                   ;; the editor cancels WHILE the first turn is running
                   (when (= 1 (swap! turns inc))
                     (acp/handle-line! @cell
                                       (acp/encode {"jsonrpc" "2.0"
                                                    "method" "session/cancel"
                                                    "params" {"sessionId" session-id}})))
                   {:status "completed"}))

       [c _]
       (conn+out {:backend backend})

       _
       (reset! cell c)

       _
       (init! c)

       sid
       (new-session! c)]

      (expect (= "cancelled" (get (result (send! c (prompt-msg 1 sid))) "stopReason")))
      ;; what a cancel writes is a WATERMARK on the turn that was in flight, not
      ;; a flag on the session: it names that turn exactly
      (expect (= (get-in @c [:sessions sid :turn]) (get-in @c [:sessions sid :cancel-mark])))
      (expect (= :idle (get-in @c [:sessions sid :state])))
      ;; so the very next turn — a higher number — is untouched by it
      (expect (= "end_turn" (get (result (send! c (prompt-msg 2 sid))) "stopReason")))
      (expect (< (long (get-in @c [:sessions sid :cancel-mark]))
                 (long (get-in @c [:sessions sid :turn])))))))

;; =============================================================================
;; HTTP transport — what the LRU cap actually has to bound
;; =============================================================================

(defdescribe
  http-eviction-test
  (it
    "unregisters the sessions of every HTTP connection it evicts"
    (let
      [reg
       @#'acp/connections

       http
       @#'acp/http-connections

       cap
       (long @#'acp/max-http-connections)

       reg0
       @reg

       http0
       @http]

      (try (reset! reg {})
           (reset! http {})
           (let
             [pairs
              (doall (for [i (range (* 3 cap))]
                       (let
                         [c (#'acp/http-connection (str "evict-" i))
                          sid (str "evict-sess-" i)]

                         (swap! c assoc-in [:sessions sid] {:cwd "/tmp" :state :idle})
                         (acp/register-connection! sid c)
                         [sid c])))

              [oldest-sid oldest-conn]
              (first pairs)

              [newest-sid _]
              (last pairs)]

             (expect (= cap (count @http)))
             ;; capping the connection table is worthless while the GLOBAL
             ;; registry keeps every session of every evicted connection: the map
             ;; grows for the life of the daemon and op-hooks keep resolving vis
             ;; sessions onto a connection nobody can answer on
             (expect (= cap (count @reg)))
             (expect (nil? (acp/connection-for oldest-sid)))
             (expect (some? (acp/connection-for newest-sid)))
             ;; an evicted connection is closed, so a late write cannot resurrect
             ;; its parked backlog
             (expect (true? (:closed? @oldest-conn))))
           (finally (reset! reg reg0) (reset! http http0))))))

;; =============================================================================
;; `serve!` — order in, order out, without losing cancel-mid-prompt
;; =============================================================================

(defn- serve-lines
  "Feed `lines` to a real `serve!` loop as ONE burst and decode what came back."
  [lines & [backend]]
  (let
    [out
     (ByteArrayOutputStream.)

     done
     (future (acp/serve! {:in (ByteArrayInputStream. (.getBytes ^String (str/join "\n" lines)
                                                                "UTF-8"))
                          :out out
                          :backend (or backend (acp/echo-backend))}))

     finished
     (deref done 15000 ::timeout)]

    {:timed-out? (= ::timeout finished)
     :messages (->> (str/split-lines (.toString out "UTF-8"))
                    (remove str/blank?)
                    (mapv decode-line))}))

(defdescribe
  serve-order-test
  (it "never lets a pipelined line overtake the handshake it depends on"
      (let
        [pairs
         20

         lines
         (mapcat (fn [i]
                   [(acp/encode {"jsonrpc" "2.0" "id" (* 2 i) "method" "initialize" "params" {}})
                    (acp/encode {"jsonrpc" "2.0"
                                 "id" (inc (* 2 i))
                                 "method" "session/new"
                                 "params" {"cwd" "/tmp"}})])
                 (range pairs))

         {:keys [timed-out? messages]}
         (serve-lines lines)

         by-id
         (into {} (map (juxt #(get % "id") identity)) messages)]

        (expect (not timed-out?))
        (expect (= (* 2 pairs) (count messages)))
        ;; a thread per line lets `session/new` land before the `initialize` that
        ;; must precede it, and the client gets `:not-initialized` for a message it
        ;; only order the protocol allows
        (expect (every? #(string? (get-in by-id [(inc (* 2 %)) "result" "sessionId"]))
                        (range pairs)))))
  (it "still answers session/cancel while a prompt is streaming"
      (let
        [backend
         (assoc (acp/echo-backend)
           :prompt (fn [{:keys [cancelled?]}]
                     ;; the turn only ends when the cancel LANDS: a serve loop that
                     ;; handled every line in order would spin here until the guard
                     ;; expires and answer "end_turn"
                     (loop [n 0]
                       (cond (cancelled?) {:status "cancelled"}
                             (> n 600) {:status "completed"}
                             :else (do (Thread/sleep 5) (recur (inc n)))))))

         lines
         [(acp/encode {"jsonrpc" "2.0" "id" 1 "method" "initialize" "params" {}})
          (acp/encode {"jsonrpc" "2.0" "id" 2 "method" "session/new" "params" {"cwd" "/tmp"}})
          (acp/encode (prompt-msg 3 "acp-echo-1"))
          (acp/encode
            {"jsonrpc" "2.0" "method" "session/cancel" "params" {"sessionId" "acp-echo-1"}})]

         {:keys [timed-out? messages]}
         (serve-lines lines backend)

         by-id
         (into {} (map (juxt #(get % "id") identity)) messages)]

        (expect (not timed-out?))
        (expect (= "acp-echo-1" (get-in by-id [2 "result" "sessionId"])))
        (expect (= "cancelled" (get-in by-id [3 "result" "stopReason"]))))))

;; =============================================================================
;; Conformance with the published ACP v1 schema
;; =============================================================================

(defdescribe
  spec-conformance-test
  (it "refuses request ids ACP cannot represent, and answers with id null"
      ;; `RequestId` is null | i64 | string. A response carrying 1.5 or 2^63 is
      ;; undeserializable for a typed client, which drops the whole connection
      ;; instead of surfacing our error — so it never reaches the wire.
      (let [[c _] (conn+out)]
        (doseq [raw ["1.5" "9223372036854775808" "-9223372036854775809" "1e30" "true" "[]"]]
          (let
            [resp (acp/handle-line! c
                                    (str "{\"jsonrpc\":\"2.0\",\"id\":"
                                         raw
                                         ",\"method\":\"authenticate\",\"params\":{}}"))]
            (expect (= (:invalid-request acp/error-codes) (code resp)) raw)
            (expect (contains? resp "id") raw)
            (expect (nil? (get resp "id")) raw)))))
  (it "echoes every id ACP does allow, i64 edges included"
      (let [[c _] (conn+out)]
        (doseq
          [[raw expected] [["9223372036854775807" 9223372036854775807]
                           ["-9223372036854775808" -9223372036854775808] ["0" 0] ["\"abc\"" "abc"]]]
          (let
            [resp (acp/handle-line! c
                                    (str "{\"jsonrpc\":\"2.0\",\"id\":"
                                         raw
                                         ",\"method\":\"authenticate\",\"params\":{}}"))]
            (expect (= {} (result resp)) raw)
            (expect (= expected (get resp "id")) raw)))))
  (it "negotiates only the version it actually implements"
      ;; v0 sat in this set while every shape on the wire was v1: we answered
      ;; `protocolVersion 0` and then spoke v1 anyway. Answer what we speak and
      ;; let the client disconnect on its own terms, as the spec tells it to.
      (expect (= #{acp/protocol-version} acp/supported-protocol-versions))
      (let
        [[c _]
         (conn+out)

         r
         (result (send! c
                        {"jsonrpc" "2.0"
                         "id" 1
                         "method" "initialize"
                         "params" {"protocolVersion" 0 "clientCapabilities" {}}}))]

        (expect (= acp/protocol-version (get r "protocolVersion")))))
  (it "introduces itself with an ACP Implementation"
      (let
        [[c _]
         (conn+out)

         info
         (get (result (init! c)) "agentInfo")]

        (expect (string? (get info "name")))
        (expect (seq (get info "name")))
        (expect (string? (get info "version")))
        (expect (seq (get info "version")))))
  (it "advertises no prompt capability it silently drops"
      (let
        [[c _]
         (conn+out)

         caps
         (get-in (result (init! c)) ["agentCapabilities" "promptCapabilities"])]

        ;; `content-block->text` flattens these to a placeholder and throws the
        ;; bytes away, so promising them only makes clients ship payloads the
        ;; model never sees.
        (expect (false? (get caps "image")))
        (expect (= "[image]" (acp/content-block->text {"type" "image" "data" "…"})))
        (expect (false? (get caps "audio")))
        (expect (= "[audio]" (acp/content-block->text {"type" "audio"})))
        ;; embedded context IS honored, and stays advertised
        (expect (true? (get caps "embeddedContext")))
        (expect (= "body"
                   (acp/content-block->text {"type" "resource" "resource" {"text" "body"}})))))
  (it "never reuses an error code ACP already assigned a meaning"
      ;; ACP spends JSON-RPC's implementation-defined range itself: -32000 auth
      ;; required, -32002 resource not found, -32800 request cancelled. Ours must
      ;; not collide, or a client that special-cases them mis-reports us.
      (expect (= -32001 (:not-initialized acp/error-codes)))
      (expect (not-any? #{-32002 -32800} (vals acp/error-codes)))
      (expect (apply distinct? (vals acp/error-codes)))))

;; =============================================================================
;; Resume (`session/load`) and the untrusted-shape walk under it
;; =============================================================================

(defdescribe
  session-load-guards-test
  (it "refuses a session the backend never had instead of resuming a PHANTOM"
      (let
        [[c out]
         (conn+out)

         _
         (init! c)

         resp
         (send! c
                {"jsonrpc" "2.0"
                 "id" 9
                 "method" "session/load"
                 "params" {"sessionId" "ghost-42" "cwd" "/tmp"}})]

        (expect (= -32602 (code resp)))
        (expect (str/includes? (get-in resp ["error" "message"]) "ghost-42"))
        ;; nothing about the phantom may survive: not the session table, not the
        ;; process-wide registry the tool op-hooks route through.
        (expect (nil? (acp/connection-for "ghost-42")))
        ;; no transcript was replayed for a session that does not exist
        (expect (empty? (filter #(str/includes? % "session/update") @out)))
        ;; and a prompt for it is still an unknown session, not a half-live turn
        (expect (= -32602
                   (code (send! c
                                {"jsonrpc" "2.0"
                                 "id" 10
                                 "method" "session/prompt"
                                 "params" {"sessionId" "ghost-42"
                                           "prompt" [{"type" "text" "text" "hi"}]}}))))))
  (it
    "insists on the same ABSOLUTE cwd `session/new` does — resume is not the lax door"
    (let
      [[c _]
       (conn+out)

       _
       (init! c)

       sid
       (get (result (send! c
                           {"jsonrpc" "2.0" "id" 2 "method" "session/new" "params" {"cwd" "/tmp"}}))
            "sessionId")

       load
       (fn [cwd]
         (send!
           c
           {"jsonrpc" "2.0" "id" 3 "method" "session/load" "params" {"sessionId" sid "cwd" cwd}}))]

      (expect (= -32602 (code (load "rel/path"))))
      (expect (= -32602 (code (load ""))))
      (expect (= -32602 (code (load 42))))
      (expect (= {} (result (load "/tmp"))))))
  (it
    "replays a REAL session's turns and leaves it promptable"
    (let
      [backend
       (assoc (acp/echo-backend)
         :load-session (fn [_]
                         [{:role "user" :text "first"} {:role "assistant" :text "second"}]))

       [c out]
       (conn+out {:backend backend})

       _
       (init! c)

       resp
       (send! c
              {"jsonrpc" "2.0"
               "id" 4
               "method" "session/load"
               "params" {"sessionId" "sess-real" "cwd" "/tmp"}})

       updates
       (keep #(let
                [m
                 (decode-line %)]

                (when (= "session/update" (get m "method")) (get-in m ["params" "update"])))
             @out)]

      (expect (= {} (result resp)))
      (expect (= 2 (count updates)))
      (expect (some? (acp/connection-for "sess-real")))
      (expect (= "end_turn"
                 (get (result (send! c
                                     {"jsonrpc" "2.0"
                                      "id" 5
                                      "method" "session/prompt"
                                      "params" {"sessionId" "sess-real"
                                                "prompt" [{"type" "text" "text" "hi"}]}}))
                      "stopReason"))))))

(defdescribe arg-paths-depth-test
             (it "walks arbitrarily deep arguments without a StackOverflowError"
                 ;; `arg-paths` runs inside the permission/mirror hook on UNTRUSTED argument
                 ;; JSON. A recursive walk dies with an `Error` no handler catches, so a deep
                 ;; value would kill the turn instead of degrading like `json-safe` does.
                 (let
                   [deep-map
                    (reduce (fn [acc _]
                              {"a" acc})
                            {"path" "/deep"}
                            (range 100000))

                    deep-vec
                    (reduce (fn [acc _]
                              [acc])
                            [{"path" "/vec"}]
                            (range 100000))

                    walk
                    (fn [x]
                      (try (acp/arg-paths x) (catch StackOverflowError _ :stack-overflow)))]

                   (expect (= ["/deep"] (walk deep-map)))
                   (expect (= ["/vec"] (walk deep-vec)))
                   ;; the same value must still ENCODE, which is where the depth cap lives
                   (expect (string? (acp/encode {"rawInput" (acp/json-safe deep-map)})))))
             (it "is total over nil holes and keeps the shallow path FIRST for the permission title"
                 (expect (= ["/top" "/x"]
                            (acp/arg-paths {"a" nil "b" [nil {"path" "/x"} nil] "path" "/top"})))
                 (expect (= [] (acp/arg-paths [nil nil])))
                 (expect (= ["/a" "/b"] (acp/arg-paths {"src" "/a" "dest" "/b"})))))

;; =============================================================================
;; A cancelled turn that aborts by THROWING — spec says `cancelled`, not an error
;; =============================================================================

(defdescribe
  cancel-through-exception-test
  (it
    "answers `cancelled` when the cancellation itself tears the turn down"
    (let
      [cell
       (atom nil)

       backend
       (assoc (acp/echo-backend)
         :prompt (fn [{:keys [session-id]}]
                   (acp/handle-line! @cell
                                     (acp/encode {"jsonrpc" "2.0"
                                                  "method" "session/cancel"
                                                  "params" {"sessionId" session-id}}))
                   ;; this is the REAL path, not a hypothetical: cancelling drops
                   ;; the gateway's SSE stream, so the read loop throws instead of
                   ;; returning a terminal event
                   (throw (ex-info "Lost connection to the gateway daemon" {}))))

       [c _]
       (conn+out {:backend backend})

       _
       (reset! cell c)

       _
       (init! c)

       sid
       (new-session! c)

       resp
       (send! c (prompt-msg 1 sid))]

      ;; ACP: the response to a cancelled `session/prompt` is `cancelled`. An
      ;; error here shows the user a hard failure for pressing escape.
      (expect (= "cancelled" (get (result resp) "stopReason")))
      (expect (nil? (get resp "error")))
      (expect (= :idle (get-in @c [:sessions sid :state])))))
  (it "still reports a turn that failed on its own as an error"
      (let
        [backend
         (assoc (acp/echo-backend)
           :prompt (fn [_]
                     (throw (ex-info "boom" {}))))

         [c _]
         (conn+out {:backend backend})

         _
         (init! c)

         sid
         (new-session! c)

         resp
         (send! c (prompt-msg 1 sid))]

        ;; no cancel was ever sent, so nothing may be dressed up as one
        (expect (= (:internal-error acp/error-codes) (code resp)))
        (expect (nil? (result resp)))
        (expect (= :idle (get-in @c [:sessions sid :state]))))))

;; =============================================================================
;; A REJECTED prompt must not leave a turn parked for a later cancel to hit
;; =============================================================================

(defdescribe
  rejected-prompt-turn-test
  (it "gives the turn back after every bad prompt shape"
      (let
        [[c _]
         (conn+out)

         _
         (init! c)

         sid
         (new-session! c)]

        (doseq
          [bad [nil [] "hi" 7 {"a" 1} [{}] [nil] [[]] ["x"] [{"type" "text"}]
                [{"type" "text" "text" "   "}] [{"type" "bogus"}]]]
          (let
            [resp (send! c
                         {"jsonrpc" "2.0"
                          "id" 1
                          "method" "session/prompt"
                          "params" {"sessionId" sid "prompt" bad}})]
            ;; the shape is refused ...
            (expect (= (:invalid-params acp/error-codes) (code resp)))
            ;; ... and the turn it opened is released, or the session stays parked
            ;; at `:pending` and keeps accepting cancels for a turn that will never
            ;; run
            (expect (= :idle (get-in @c [:sessions sid :state])))
            ;; so a cancel arriving right now has no turn in flight to land on
            (send! c {"jsonrpc" "2.0" "method" "session/cancel" "params" {"sessionId" sid}})
            (expect (nil? (get-in @c [:sessions sid :cancel-mark])))))
        ;; and the good prompt that follows all of them runs to the end
        (expect (= "end_turn" (get (result (send! c (prompt-msg 1 sid))) "stopReason")))))
  (it "never lets a rejected prompt's stray cancel abort the next real turn"
      (let
        [[c _]
         (conn+out)

         _
         (init! c)

         sid
         (new-session! c)

         reasons
         (doall (for [_ (range 200)]
                  (do
                    ;; rejected prompt, then the escape key that follows it a beat late
                    (send! c
                           {"jsonrpc" "2.0"
                            "id" 1
                            "method" "session/prompt"
                            "params" {"sessionId" sid "prompt" []}})
                    (send! c {"jsonrpc" "2.0" "method" "session/cancel" "params" {"sessionId" sid}})
                    (get (result (send! c (prompt-msg 2 sid))) "stopReason"))))]

        (expect (= #{"end_turn"} (set reasons)))
        (expect (= :idle (get-in @c [:sessions sid :state])))))
  (it
    "keeps the state machine sound while bad prompts and cancels race a real turn"
    (let
      [[c _]
       (conn+out)

       _
       (init! c)

       sid
       (new-session! c)

       noise
       (future (dotimes [_ 400]
                 (send! c
                        {"jsonrpc" "2.0"
                         "id" 1
                         "method" "session/prompt"
                         "params" {"sessionId" sid "prompt" "not-an-array"}})
                 (send! c {"jsonrpc" "2.0" "method" "session/cancel" "params" {"sessionId" sid}})))

       resps
       (mapv (fn [i]
               (send! c (prompt-msg i sid)))
             (range 100))

       _
       (deref noise 15000 ::timeout)]

      ;; a cancel that really is in flight may stop a turn — that is the contract.
      ;; What must never happen is a hard error, a missing answer, or a session
      ;; wedged in `:running` because a rejected prompt lost its claim.
      (expect (every? #(contains? #{"end_turn" "cancelled"} (get (result %) "stopReason")) resps))
      (expect (= 100 (count resps)))
      (expect (= :idle (get-in @c [:sessions sid :state]))))))

;; =============================================================================
;; MCP servers — the client's servers, attached to the client's session
;; =============================================================================

(def ^:private fake-mcp-server "test/resources/mcp/fake_mcp_server.py")

(defn- python3-path
  "Absolute python3 on PATH, or nil — the fake MCP server is a python script, and
   a machine without python must skip rather than fail."
  []
  (some (fn [d]
          (let [f (io/file d "python3")]
            (when (.canExecute f) (.getPath f))))
        (str/split (or (System/getenv "PATH") "") #":")))

(defn- new-session-with!
  [c servers]
  (send! c
         {"jsonrpc" "2.0"
          "id" "new"
          "method" "session/new"
          "params" (cond-> {"cwd" "/tmp"}
                     servers
                     (assoc "mcpServers" servers))}))

(defdescribe
  acp-mcp-servers-test
  "`mcpServers` used to be read and DROPPED: the editor declared servers, the
   session got none, the model never saw one of their tools, and nothing on the
   wire admitted it. These drive the real MCP pool."
  (it
    "session/new attaches the client's stdio server session-scoped, and its tools reach the model"
    (let
      [py
       (python3-path)

       f
       (io/file fake-mcp-server)]

      (if-not (and py (.exists f))
        (expect true) ; no python3 — skip, don't fail CI
        (let [[c _] (conn+out)]
          (init! c)
          (let
            [sid (get (result (new-session-with! c
                                                 [{"name" "fake"
                                                   "command" py
                                                   "args" [(.getAbsolutePath f)]
                                                   "env" [{"name" "ACP_TEST" "value" "1"}]}]))
                      "sessionId")]
            (try (expect (= [{:name "fake" :transport "stdio" :is-connected true}]
                            (mcp/session-servers sid)))
                 ;; The tools reach the MODEL through exactly the env the engine
                 ;; passes a tool call — anything less is a session that only LOOKS
                 ;; wired up.
                 (expect (= ["echo"]
                            (mapv #(get % "name")
                                  (get-in (mcp/mcp-tools {:session-id sid} "fake")
                                          [:result "tools"]))))
                 (expect (= "echo: hi"
                            (get-in (mcp/mcp-call {:session-id sid} "fake" "echo" {"msg" "hi"})
                                    [:result "content" 0 "text"])))
                 (expect (= "session"
                            (get-in (mcp/mcp-servers {:session-id sid})
                                    [:result "servers" 0 "scope"])))
                 ;; …and NOWHERE else: one editor's server is not the machine's.
                 (expect (empty? (get-in (mcp/mcp-servers {:session-id "someone-else"})
                                         [:result "servers"])))
                 (finally (mcp/clear-session-servers! sid)))
            (expect (empty? (mcp/session-servers sid))))))))
  (it "a server that cannot be reached FAILS session/new instead of yielding a crippled session"
      (let [[c _] (conn+out)]
        (init! c)
        (let
          [resp (new-session-with!
                  c
                  [{"name" "nope" "command" "/definitely/not/here" "args" [] "env" []}])]
          (expect (= -32603 (code resp)))
          (expect (= "nope" (get-in resp ["error" "data" "failedMcpServers" 0 "name"])))
          (expect (nil? (result resp))))))
  (it "sse is refused rather than silently downgraded, because we advertise sse false"
      (let [[c _] (conn+out)]
        (init! c)
        (let
          [resp (new-session-with!
                  c
                  [{"type" "sse" "name" "s" "url" "https://example.test/sse" "headers" []}])]
          (expect (= -32602 (code resp)))
          (expect (str/includes? (get-in resp ["error" "message"]) "sse")))))
  (it "malformed server entries are invalid-params, never silently dropped"
      (let [[c _] (conn+out)]
        (init! c)
        (doseq
          [servers [{"not" "an array"} ["not an object"] [{"command" "echo"}] [{"name" "s"}]
                    [{"name" "s" "command" "echo" "args" [1 2]}]
                    [{"name" "s" "command" "echo" "env" {"A" "B"}}]
                    [{"name" "s" "command" "echo" "env" [{"name" "A" "value" 1}]}]
                    [{"type" "http" "name" "h"}]
                    [{"type" "websocket" "name" "s" "url" "https://example.test"}]]]
          (expect (= -32602 (code (new-session-with! c servers)))))))
  (it "duplicate server names are refused instead of one silently winning"
      (let [[c _] (conn+out)]
        (init! c)
        (let
          [resp
           (new-session-with! c [{"name" "dup" "command" "echo"} {"name" "dup" "command" "echo"}])]
          (expect (= -32602 (code resp)))
          (expect (str/includes? (get-in resp ["error" "message"]) "duplicate")))))
  (it
    "session/load REPLACES the session's servers, and an absent mcpServers clears them"
    (let
      [py
       (python3-path)

       f
       (io/file fake-mcp-server)]

      (if-not (and py (.exists f))
        (expect true)
        (let
          [[c _]
           (conn+out)

           spec
           (fn [nm]
             [{"name" nm "command" py "args" [(.getAbsolutePath f)]}])]

          (init! c)
          (let [sid (get (result (new-session-with! c (spec "first"))) "sessionId")]
            (try (expect (= ["first"] (mapv :name (mcp/session-servers sid))))
                 (send! c
                        {"jsonrpc" "2.0"
                         "id" "load"
                         "method" "session/load"
                         "params" {"sessionId" sid "cwd" "/tmp" "mcpServers" (spec "second")}})
                 (expect (= ["second"] (mapv :name (mcp/session-servers sid))))
                 ;; A resumed session must not inherit a previous life's servers.
                 (send! c
                        {"jsonrpc" "2.0"
                         "id" "load2"
                         "method" "session/load"
                         "params" {"sessionId" sid "cwd" "/tmp"}})
                 (expect (empty? (mcp/session-servers sid)))
                 (finally (mcp/clear-session-servers! sid))))))))
  (it "initialize advertises exactly the MCP transports the client library implements"
      (let [[c _] (conn+out)]
        (expect (= {"http" true "sse" false}
                   (get-in (result (init! c)) ["agentCapabilities" "mcpCapabilities"]))))))

(defdescribe
  acp-max-tokens-stop-reason-test
  "A turn killed by the output budget is a STOP REASON in ACP, not a JSON-RPC
   error: an editor shows \"the model ran out of room\", not \"the agent crashed\"."
  (it
    "an output-budget death reports stopReason max_tokens"
    (doseq
      [msg
       ["Provider truncated the response at max_tokens (8192) after 3 retries"
        "Provider stopped the response as incomplete because output budget was exhausted (max_output_tokens)."]]
      (let
        [[c _] (conn+out {:backend (acp/echo-backend (fn [_]
                                                       {:status "failed" :error msg}))})]
        (init! c)
        (let
          [sid (new-session! c)
           resp (send! c
                       {"jsonrpc" "2.0"
                        "id" "p"
                        "method" "session/prompt"
                        "params" {"sessionId" sid "prompt" [{"type" "text" "text" "hi"}]}})]

          (expect (= "max_tokens" (get (result resp) "stopReason")))))))
  (it "any other failure is still a real error, not a fake stop reason"
      (let
        [[c _] (conn+out {:backend (acp/echo-backend (fn [_]
                                                       {:status "failed"
                                                        :error "the database is on fire"}))})]
        (init! c)
        (let
          [sid (new-session! c)
           resp (send! c
                       {"jsonrpc" "2.0"
                        "id" "p"
                        "method" "session/prompt"
                        "params" {"sessionId" sid "prompt" [{"type" "text" "text" "hi"}]}})]

          (expect (= -32603 (code resp)))
          (expect (nil? (result resp)))))))
