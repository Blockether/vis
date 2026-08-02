(ns com.blockether.vis.internal.foundation.acp-test
  "ADVERSARIAL coverage for the ACP agent server.

   ACP is one JSON message per LINE over a pipe an editor owns, so the three
   things that must never happen are: a throw that escapes into the read loop, a
   write that emits an embedded newline and desynchronizes framing, and a
   permission path that fails OPEN. Every test here attacks one of those."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.foundation.acp :as acp]
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
