(ns com.blockether.vis.native-binary-test
  "What only the LINKED binary can answer.

   `container-image-test` reads the Dockerfile and the unit suite runs on the
   JVM, so neither can see the failures native-image actually produces: a
   missing reachability entry that compiles cleanly and dies on the first frame,
   an agent path that never boots, an HTTP transport that cannot reach a model.
   Those exist only once `clojure -T:build native` has linked `target/vis`, so
   they are proven HERE — by RUNNING that file — and nowhere else. Proving them
   inside a `docker build` instead made the answer cost a container build and
   hid it from everyone who does not run one.

     clojure -T:build native      # ~20 min, ~12 GiB live set
     clojure -M:test-native       # VIS_NATIVE_BIN=… to point at another binary

   A SEPARATE alias on purpose. The default suite must not demand a twenty
   minute build, and these tests must not pass by skipping when the binary is
   absent: a missing binary FAILS here and says how to build one.

   THE AGENT TURN IS HERMETIC, AND IT NAMES NO VENDOR. The model it talks to is
   a provider this test INVENTS: an OpenAI-dialect stub on loopback, declared in
   a throwaway project overlay (`<cwd>/.vis/config.yml`) with NO credential of
   any kind. Nothing here may depend on a provider extension that happens to be
   linked in, on a key in the environment, or on a vendor's endpoint — a suite
   that names one is a suite that fails when that provider is dropped, and a
   suite that accepts a key is one the developer's own credentials can pass.
   The stub records what it was asked, so the assertions are about the request
   the binary actually made.

   The overlay is the ONLY isolation there is: `~/.vis` hangs off `user.home`,
   which both the JVM and the native image read from the operating system's
   passwd entry — setting HOME moves nothing (measured on macOS: HOME=/tmp/…
   still resolves user.home to the real account)."
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
   RECORDS what it was asked — route, body and request headers. The recording is
   the point: an answer alone would also appear if the binary quietly fell back
   to a credential this machine happens to hold, and the headers are how the
   suite proves the keyless provider stayed keyless on the wire."
  [reply]
  (let
    [asked
     (atom [])

     server
     (HttpServer/create (InetSocketAddress. "127.0.0.1" 0) 0)]

    (.createContext
      server
      "/"
      (reify
        HttpHandler
          (handle [_ exchange]
            (let
              [^HttpExchange exchange
               exchange

               request
               (slurp (.getRequestBody exchange))

               headers
               (into {}
                     (map (fn [[k v]]
                            [(str/lower-case (str k)) (vec v)]))
                     (.getRequestHeaders exchange))

               stream?
               (str/includes? (str/replace request " " "") "\"stream\":true")

               payload
               (.getBytes ^String (if stream? (stream-body reply) (whole-body reply))
                          StandardCharsets/UTF_8)]

              (swap! asked conj
                {:path (.getPath (.getRequestURI exchange)) :body request :headers headers})
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
  "Writes `<dir>/.vis/config.yml`: a provider that exists nowhere but here.

   The hidden project overlay is the highest config tier, so this entry outranks
   whatever `~/.vis` says and the binary has no reason to look for a vendor. It
   carries NO `api_key` and no `api_key_command` on purpose — a stub needs no
   credential, and a test that supplied one could no longer tell a configured
   provider apart from a machine that happens to be signed in somewhere."
  [^File dir port]
  (let [vis-dir (io/file dir ".vis")]
    (.mkdirs vis-dir)
    (spit (io/file vis-dir "config.yml")
          (str "default_provider: stub-local\n"
               "default_model: stub-model\n"
               "providers:\n"
               "  - id: stub-local\n"
               "    base_url: http://127.0.0.1:"
               port
               "/v1\n"
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
             ;;
             ;; The provider is INVENTED HERE. No shipped provider extension is named, so
             ;; this stays green when the set of bundled vendors changes, and it proves the
             ;; thing a deployment actually relies on: an OpenAI-compatible endpoint put in
             ;; config reaches a real model call out of the native image.
             (it
               "answers the prompt from the keyless custom provider its config names"
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
                         @asked

                         {:keys [path body headers]}
                         (first requests)

                         ;; Whatever the transport spells the auth header as, what matters is
                         ;; whether anything SECRET rode in it.
                         credentials
                         (->> ["authorization" "x-api-key"]
                              (mapcat #(get headers %))
                              (map #(str/trim (str/replace (str %) #"(?i)^bearer" "")))
                              (remove str/blank?))]

                        (expect (= 0 exit) output)
                        (expect (str/includes? output "hello world") output)
                        ;; Without this the test would also pass on a machine whose own
                        ;; ~/.vis holds a real credential, and would prove nothing.
                        (expect (seq requests)
                                (str "the binary never called the configured provider:\n" output))
                        (expect (str/includes? path "/chat/completions")
                                (str "unexpected provider route: " path))
                        (expect (str/includes? body "\"stub-model\"")
                                "the request did not carry the model the overlay names")
                        (expect (str/includes? body "Reply with exactly: hello world")
                                "the request did not carry the prompt")
                        ;; The overlay names no key, so nothing may authenticate on its behalf.
                        ;; MEASURED: the OpenAI-compatible transport still sends the header, and
                        ;; it arrives as a bare `Bearer` with nothing after it. A value here
                        ;; would be a credential from this machine attached to a provider that
                        ;; never asked for one — which is exactly the way this test could pass
                        ;; while proving nothing.
                        (expect (empty? credentials)
                                "the keyless provider authenticated with a credential of its own"))
                      (finally (.stop server 0) (delete-tree! dir))))))

