(ns com.blockether.vis.native-binary-test
  "What only the LINKED binary can answer.

   `container-image-test` reads the Dockerfile and the unit suite runs on the
   JVM, so neither can see the failures native-image actually produces: a
   missing reachability entry that compiles cleanly and dies on the first frame,
   an agent path that never boots, a provider extension left out of the image.
   Those exist only once `clojure -T:build native` has linked `target/vis`, so
   they are proven HERE — by RUNNING that file — and nowhere else. Proving them
   inside a `docker build` instead made the answer cost a container build and
   hid it from everyone who does not run one.

     clojure -T:build native      # ~20 min, ~12 GiB live set
     clojure -M:test-native       # VIS_NATIVE_BIN=… to point at another binary

   A SEPARATE alias on purpose. The default suite must not demand a twenty
   minute build, and these tests must not pass by skipping when the binary is
   absent: a missing binary FAILS here and says how to build one.

   THE AGENT TURN IS HERMETIC. A loopback stub speaks the OpenAI dialect and the
   binary is pointed at it through a throwaway project overlay
   (`<cwd>/.vis/config.yml`), so no test spends money and the developer's own
   credentials cannot decide whether it passes. The overlay is the ONLY
   isolation there is: `~/.vis` hangs off `user.home`, which both the JVM and
   the native image read from the operating system's passwd entry — setting
   HOME moves nothing (measured on macOS: HOME=/tmp/… still resolves
   user.home to the real account)."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.foundation.pty :as pty]
            [lazytest.core :refer [defdescribe expect it]])
  (:import (com.sun.net.httpserver HttpExchange HttpHandler HttpServer)
           (java.io File InputStream)
           (java.lang ProcessBuilder$Redirect)
           (java.net InetSocketAddress)
           (java.nio.charset StandardCharsets)
           (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)
           (java.util.concurrent TimeUnit)))

;; ── the binary under test ────────────────────────────────────────────────────

(defn- native-binary
  "The linked runtime this namespace exercises: `VIS_NATIVE_BIN`, else the path
   `clojure -T:build native` writes."
  ^File []
  (io/file (or (not-empty (System/getenv "VIS_NATIVE_BIN")) "target/vis")))

(def ^:private build-it
  "Said on every failure that is really a missing build, because the reader of a
   red run in six months will not know this alias needs one."
  (str "Build the runtime first: `clojure -T:build native` writes target/vis"
       " (~20 min, ~12 GiB live set), or point VIS_NATIVE_BIN at an existing binary."))

(defn- require-binary
  "The binary, or a failure that names the build command instead of an ENOENT."
  ^File []
  (let [bin (native-binary)]
    (expect (.canExecute bin)
            (str "No executable native runtime at " (.getAbsolutePath bin) ". " build-it))
    bin))

;; ── running it ───────────────────────────────────────────────────────────────

(defn- temp-dir
  ^File [prefix]
  (.toFile (Files/createTempDirectory prefix (make-array FileAttribute 0))))

(defn- delete-tree!
  [^File dir]
  (doseq [^File f (reverse (file-seq dir))]
    (io/delete-file f true)))

(defn- kill-tree!
  "Kills the process AND what it spawned. `script` lends the TUI a pty by forking
   it, so destroying only the parent would leave a live native runtime behind."
  [^Process process]
  (doseq
    [^java.lang.ProcessHandle child (-> process
                                        .toHandle
                                        .descendants
                                        .toList)]
    (.destroyForcibly child))
  (.destroyForcibly process)
  (.waitFor process 10 TimeUnit/SECONDS))

(defn- run-binary
  "Runs `argv` with `dir` as its working directory (which is what selects the
   project overlay) and merged output captured to a file — a pipe nobody drains
   fills at 64 KiB and would hang the TUI mid-frame.

   Returns `{:finished? :exit :output}`; `:finished?` false means the deadline
   killed a process that was still running, which for the TUI is the assertion."
  [^File dir argv timeout-secs]
  (let
    [log
     (io/file dir "run.log")

     process
     (-> (ProcessBuilder. ^java.util.List (vec argv))
         (.directory dir)
         (.redirectErrorStream true)
         (.redirectOutput (ProcessBuilder$Redirect/to log))
         (.start))

     finished?
     (.waitFor process timeout-secs TimeUnit/SECONDS)]

    (when-not finished? (kill-tree! process))
    {:finished? finished?
     :exit (when finished? (.exitValue process))
     :output (if (.exists log) (slurp log) "")}))

(defn- pty-text
  "Reads a pseudo-terminal until `enough?` likes what it has seen, or the deadline
   passes; everything read either way.

   `.available` rather than a blocking read: a TUI that never writes another byte
   must not hold the suite until the pipe closes."
  [^InputStream in deadline-ms enough?]
  (let
    [buffer
     (byte-array 8192)

     stop-at
     (+ (System/currentTimeMillis) deadline-ms)]

    (loop [seen ""]
      (cond (enough? seen) seen
            (> (System/currentTimeMillis) stop-at) seen
            (pos? (.available in))
            (let [n (.read in buffer)]
              (recur (if (pos? n) (str seen (String. buffer 0 n StandardCharsets/UTF_8)) seen)))
            :else (do (Thread/sleep 50) (recur seen))))))

(defn- with-a-controlling-terminal
  "`argv` wrapped in `script(1)`, which is the only portable way to hand a child a
   CONTROLLING terminal.

   A pty alone is not enough: `posix_spawn` with SETSID (what
   `internal.foundation.pty` does for background shells) leaves the child in a new
   session with no controlling tty, and the binary opens /dev/tty on its way up —
   MEASURED: `vis-agent: fatal error - /dev/tty (Device not configured)`, twice,
   and then no frame ever. `script` does the setsid + TIOCSCTTY dance itself, and
   it needs a terminal of its own to do it, which is what the pty below is for.
   BSD and util-linux disagree about how the command is passed."
  [argv]
  (if (str/starts-with? (System/getProperty "os.name") "Mac")
    (into ["/usr/bin/script" "-q" "/dev/null"] argv)
    ["/usr/bin/script" "-qec" (str/join " " argv) "/dev/null"]))

(defn- run-on-a-pty
  "Spawns `argv` on a REAL pseudo-terminal — `isatty` true, `$TERM` honoured, a
   size the child can ask for — and reads what it paints.

   The pty is Vis' own `internal.foundation.pty`, parent-side JVM code: nothing
   about the binary under test is mocked by using it."
  [^File dir argv deadline-ms enough?]
  (let
    [handle
     (pty/spawn! {:command (with-a-controlling-terminal argv)
                  :dir (.getAbsolutePath dir)
                  :env (assoc (into {} (System/getenv)) "TERM" "xterm-256color")
                  :cols 120
                  :rows 40})

     painted
     (pty-text (:in handle) deadline-ms enough?)

     alive?
     ((:alive? handle))]

    ((:destroy handle) true)
    {:painted painted :alive? alive?}))

;; ── a provider that answers on loopback ──────────────────────────────────────

(defn- json-chunk
  [body]
  (str "data: {\"id\":\"stub\",\"object\":\"chat.completion.chunk\",\"created\":0,"
       "\"model\":\"stub-model\",\"choices\":["
       body
       "]}\n\n"))

(defn- stream-body
  "The OpenAI streaming shape, which is what the binary asks for (measured:
   `accept: text/event-stream`, `\"stream\": true`). `pr-str` of plain text is
   also its JSON spelling."
  [reply]
  (str (json-chunk
         "{\"index\":0,\"delta\":{\"role\":\"assistant\",\"content\":\"\"},\"finish_reason\":null}")
       (json-chunk
         (str "{\"index\":0,\"delta\":{\"content\":" (pr-str reply) "},\"finish_reason\":null}"))
       (json-chunk "{\"index\":0,\"delta\":{},\"finish_reason\":\"stop\"}")
       "data: [DONE]\n\n"))

(defn- whole-body
  [reply]
  (str "{\"id\":\"stub\",\"object\":\"chat.completion\",\"created\":0,\"model\":\"stub-model\","
       "\"choices\":[{\"index\":0,\"message\":{\"role\":\"assistant\",\"content\":" (pr-str reply)
       "},\"finish_reason\":\"stop\"}],"
       "\"usage\":{\"prompt_tokens\":1,\"completion_tokens\":2,\"total_tokens\":3}}"))

(defn- start-stub-provider!
  "An OpenAI-dialect model server on 127.0.0.1 that always answers `reply` and
   RECORDS what it was asked. The recording is the point: an answer alone would
   also appear if the binary quietly fell back to a credential this machine
   happens to hold."
  [reply]
  (let
    [asked
     (atom [])

     server
     (HttpServer/create (InetSocketAddress. "127.0.0.1" 0) 0)]

    (.createContext server
                    "/"
                    (reify
                      HttpHandler
                        (handle [_ exchange]
                          (let
                            [^HttpExchange exchange
                             exchange

                             request
                             (slurp (.getRequestBody exchange))

                             stream?
                             (str/includes? (str/replace request " " "") "\"stream\":true")

                             payload
                             (.getBytes ^String (if stream? (stream-body reply) (whole-body reply))
                                        StandardCharsets/UTF_8)]

                            (swap! asked conj
                              {:path (.getPath (.getRequestURI exchange)) :body request})
                            (.add (.getResponseHeaders exchange)
                                  "Content-Type"
                                  (if stream? "text/event-stream" "application/json"))
                            (.sendResponseHeaders exchange 200 (alength payload))
                            (with-open [out (.getResponseBody exchange)]
                              (.write out payload))))))
    (.setExecutor server nil)
    (.start server)
    {:server server :asked asked :port (.getPort (.getAddress server))}))

(defn- overlay!
  "Writes `<dir>/.vis/config.yml`. The hidden project overlay is the highest
   config tier, so it names the default provider whatever `~/.vis` says."
  [^File dir port]
  (let [vis-dir (io/file dir ".vis")]
    (.mkdirs vis-dir)
    (spit (io/file vis-dir "config.yml")
          (str "default_provider: stub-local\n" "default_model: stub-model\n"
               "providers:\n" "  - id: stub-local\n"
               "    base_url: http://127.0.0.1:" port
               "/v1\n" "    api_key: stub-key\n"
               "    compatibility: openai\n" "    models:\n"
               "      - name: stub-model\n" "        context: 32000\n"
               "        output_limit: 4096\n" "        is_tool_call: true\n"))))

;; ── the proofs ───────────────────────────────────────────────────────────────

(defdescribe native-binary-is-the-artifact-the-build-produces-test
             (it "is on disk and executable"
                 (let [bin (require-binary)]
                   (expect (pos? (.length bin))
                           (str (.getAbsolutePath bin) " is empty. " build-it))))
             (it "reports the version stamped into it"
                 ;; The cheapest possible run of the linked image: it initializes every
                 ;; build-time-initialized namespace on the way to printing one line.
                 (let
                   [dir
                    (temp-dir "vis-native-version")

                    {:keys [exit output]}
                    (run-binary dir [(.getAbsolutePath (require-binary)) "--version"] 60)]

                   (try (expect (= 0 exit) output)
                        (expect (re-find #"(?m)^vis-agent\s+\S+" output) output)
                        (finally (delete-tree! dir))))))

(defdescribe
  native-binary-paints-the-tui-test
  ;; Lanterna reaches the terminal through JNI and reflection: a reachability
  ;; entry missing from the image compiles fine and dies on the FIRST FRAME, so
  ;; the only proof is a running TUI. A painted alternate screen plus a process
  ;; still alive behind it is that proof — anything else is a TUI that left.
  (it
    "paints a first frame and is still running behind it"
    (let
      [dir
       (temp-dir "vis-native-tui")

       ;; Entering the alternate screen is the first thing a live frame does.
       alt-screen
       "\u001b[?1049h"

       {:keys [painted alive?]}
       (run-on-a-pty dir
                     [(.getAbsolutePath (require-binary)) "channels" "tui"]
                     20000
                     #(str/includes? % alt-screen))]

      (try
        (expect (str/includes? painted alt-screen)
                (str "no alternate-screen frame reached the pty:\n" painted))
        (expect alive? (str "the native TUI left on its own:\n" painted))
        (expect
          (nil?
            (re-find
              #"ClassNotFoundException|NoClassDefFoundError|UnsatisfiedLinkError|NoSuchMethodError"
              painted))
          painted)
        (finally (delete-tree! dir))))))

(defdescribe native-binary-runs-a-whole-agent-turn-test
             ;; The one-shot entrypoint boots the session store, the tool registry, config
             ;; merging, provider selection and the HTTP transport. No unit test crosses
             ;; all of that inside the LINKED image, and every one of those layers has a
             ;; native-image failure mode of its own.
             (it "answers the prompt from the provider its config names"
                 (let
                   [dir
                    (temp-dir "vis-native-agent")

                    {:keys [server asked port]}
                    (start-stub-provider! "hello world")]

                   (try (overlay! dir port)
                        (let
                          [{:keys [exit output]}
                           (run-binary dir
                                       [(.getAbsolutePath (require-binary)) "--db" ":memory" "--raw"
                                        "Reply with exactly: hello world"]
                                       180)

                           requests
                           @asked]

                          (expect (= 0 exit) output)
                          (expect (str/includes? output "hello world") output)
                          ;; Without this the test would also pass on a machine whose own
                          ;; ~/.vis holds a real credential, and would prove nothing.
                          (expect (seq requests)
                                  (str "the binary never called the configured provider:\n" output))
                          (expect (str/includes? (:path (first requests)) "/chat/completions")
                                  (str "unexpected provider route: " (:path (first requests))))
                          (expect (str/includes? (:body (first requests)) "\"stub-model\"")
                                  "the request did not carry the model the overlay names")
                          (expect (str/includes? (:body (first requests))
                                                 "Reply with exactly: hello world")
                                  "the request did not carry the prompt"))
                        (finally (.stop server 0) (delete-tree! dir))))))

(defdescribe native-binary-carries-the-provider-extensions-test
             ;; Provider extensions are compiled INTO the image. A deployment configures
             ;; `zai-coding-plan` from outside it (a key in the environment, a `providers:`
             ;; entry in config), which can only work if the extension is in the binary —
             ;; and its absence is a silent "unknown provider" at the deployment, hours
             ;; later. Here it costs one process.
             (it "lists zai-coding-plan among the providers it knows"
                 (let
                   [dir
                    (temp-dir "vis-native-providers")

                    {:keys [exit output]}
                    (run-binary dir [(.getAbsolutePath (require-binary)) "providers" "list"] 90)

                    ;; The table wraps at the terminal width, so an id can arrive split
                    ;; across two rows. Strip the layout and ask about the text.
                    flat
                    (str/replace output #"\s+" "")]

                   (try (expect (= 0 exit) output)
                        (expect (str/includes? flat "zai-coding-plan") output)
                        (expect (str/includes? flat "api.z.ai") output)
                        (finally (delete-tree! dir))))))
