(ns com.blockether.vis.native-binary-test
  "What only the LINKED binary can answer.

   `container-image-test` reads the Dockerfile and the unit suite runs on the
   JVM, so neither can see the failures native-image actually produces: a
   missing reachability entry that compiles cleanly and dies at runtime, an agent
   path that never boots, or an HTTP transport that cannot reach a model.
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
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [charred.api :as json]
            [com.blockether.vis.contract.gateway :as gateway-contract]
            [com.blockether.vis.contract.wire :as contract-wire]
            [com.blockether.vis-python-runtime :as runtime]
            [com.blockether.vis.internal.config.core :as config]
            [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.internal.loop.environment :as loop-env]
            [com.blockether.vis.internal.persistance.core :as ps]
            [com.blockether.vis.internal.python.env :as ep]
            [com.blockether.vis.internal.python.extensions :as pyx]
            [com.blockether.vis.internal.python.runtime :as python-runtime]
            [com.blockether.vis.internal.python.worker :as worker]
            [lazytest.core :refer [defdescribe expect it]])
  (:import (com.sun.net.httpserver HttpExchange HttpHandler HttpServer)
           (java.io File)
           (java.lang ProcessBuilder$Redirect ProcessHandle)
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

(defn- python-library
  "The staged CPython cdylib beside `bin`, resolved exactly as the public wrapper does."
  ^File [^File bin]
  (let [home (.getParentFile (.getAbsoluteFile bin))]
    (->> ["vis-agent-python" "python"]
         (map #(io/file home %))
         (filter #(.isDirectory ^File %))
         (mapcat #(.listFiles ^File %))
         (filter (fn [^File f]
                   (and (.isFile f) (re-matches #"libvispython\.(dylib|so)" (.getName f)))))
         first)))

(defn- native-environment
  "Environment the public wrapper supplies to the private binary in a release."
  []
  (if-let [library (python-library (native-binary))]
    {"VIS_PYTHON_NATIVE_PATH" (.getAbsolutePath library)}
    {}))

;; ── running it ───────────────────────────────────────────────────────────────

(defn- temp-dir
  ^File [prefix]
  (.toFile (Files/createTempDirectory prefix (make-array FileAttribute 0))))

(defn- delete-tree!
  [^File dir]
  (with-open [walk (Files/walk (.toPath dir) (make-array java.nio.file.FileVisitOption 0))]
    (doseq [^java.nio.file.Path path (reverse (vec (.toArray walk)))]
      (Files/deleteIfExists path))))

(defn- kill-tree!
  "Kills the process AND what it spawned. `script` lends the TUI a pty by forking
   it, so destroying only the parent would leave a live native runtime behind."
  [^Process process]
  (doseq [^java.lang.ProcessHandle child (-> process
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
  (let [log
        (io/file dir "run.log")

        builder
        (doto (ProcessBuilder. ^java.util.List (vec argv))
          (.directory dir)
          (.redirectErrorStream true)
          (.redirectOutput (ProcessBuilder$Redirect/to log)))

        _
        (.putAll (.environment builder) (native-environment))

        process
        (.start builder)

        finished?
        (.waitFor process timeout-secs TimeUnit/SECONDS)]

    (when-not finished? (kill-tree! process))
    {:finished? finished?
     :exit (when finished? (.exitValue process))
     :output (if (.exists log) (slurp log) "")}))

(defdescribe
  native-terminal-supervisor-test
  (it
    "runs a terminal child and releases its lease without stopping the gateway"
    (let [dir
          (temp-dir "vis-native-tui-lease-")

          server
          (HttpServer/create (InetSocketAddress. "127.0.0.1" 0) 0)

          requests
          (atom [])

          handshake
          (contract-wire/->wire (gateway-contract/handshake {:version "dev" :build "fixture"}))]

      (try (.createContext server
                           "/"
                           (reify
                             HttpHandler
                               (handle [_ exchange]
                                 (let [^HttpExchange exchange
                                       exchange

                                       path
                                       (.getPath (.getRequestURI exchange))

                                       method
                                       (.getRequestMethod exchange)

                                       _
                                       (slurp (.getRequestBody exchange))

                                       body
                                       (case path
                                         "/healthz"
                                         {:status "ok" :secret_match true :protocol handshake}

                                         "/v1/clients"
                                         {:client_id "terminal-lease"}

                                         {})

                                       bytes
                                       (.getBytes ^String (json/write-json-str body)
                                                  StandardCharsets/UTF_8)]

                                   (swap! requests conj [method path])
                                   (.sendResponseHeaders exchange 200 (alength bytes))
                                   (with-open [out (.getResponseBody exchange)]
                                     (.write out bytes))))))
           (.start server)
           (let [url
                 (str "http://127.0.0.1:" (.getPort (.getAddress server)))

                 result
                 (run-binary dir
                             [(.getAbsolutePath (require-binary))
                              (str "-Duser.home=" (.getAbsolutePath dir)) "--gateway" url "gateway"
                              "tui" "--" "/bin/sh" "-c" "test -n \"$VIS_GATEWAY_URL\" && exit 7"]
                             60)]

             (expect (:finished? result) (:output result))
             (expect (= 7 (:exit result)) (:output result))
             (expect (some #{["POST" "/v1/clients"]} @requests))
             (expect (some #{["DELETE" "/v1/clients/terminal-lease"]} @requests))
             (expect (not-any? #(= "/v1/admin/stop" (second %)) @requests)))
           (finally (.stop server 0) (delete-tree! dir))))))

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
  (let [asked
        (atom [])

        server
        (HttpServer/create (InetSocketAddress. "127.0.0.1" 0) 0)]

    (.createContext
      server
      "/"
      (reify
        HttpHandler
          (handle [_ exchange]
            (let [^HttpExchange exchange
                  exchange

                  request
                  (slurp (.getRequestBody exchange))

                  headers
                  (into {}
                        (map (fn [[k v]]
                               [(str/lower-case (str k)) (vec v)]))
                        (.getRequestHeaders exchange))

                  ;; Startup metadata refresh must not consume scripted inference replies.
                  catalog?
                  (and (= "GET" (.getRequestMethod exchange))
                       (str/ends-with? (.getPath (.getRequestURI exchange)) "/models"))

                  stream?
                  (str/includes? (str/replace request " " "") "\"stream\":true")

                  payload
                  (.getBytes ^String
                             (cond catalog? "{\"data\":[]}"
                                   stream? (stream-body reply)
                                   :else (whole-body reply))
                             StandardCharsets/UTF_8)]

              (when-not catalog?
                (swap! asked conj
                  {:path (.getPath (.getRequestURI exchange)) :body request :headers headers}))
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
                 (let [dir
                       (temp-dir "vis-native-version")

                       {:keys [exit output]}
                       (run-binary dir [(.getAbsolutePath (require-binary)) "--version"] 60)]

                   (try (expect (= 0 exit) output)
                        (expect (re-find #"(?m)^vis-agent\s+\S+" output) output)
                        (finally (delete-tree! dir))))))

(defdescribe
  native-binary-runs-a-whole-agent-turn-test
  ;; The one-shot entrypoint boots the session store, the tool registry, config
  ;; merging, provider selection and the HTTP transport. No unit test crosses
  ;; all of that inside the LINKED image, and every one of those layers has a
  ;; native-image failure mode of its own.
  ;; Regression: YAMLStar 0.1.21's lazy reference plugin must be linked explicitly;
  ;; otherwise the valid overlay is ignored and startup reports no provider.
  ;;
  ;; The provider is INVENTED HERE. No shipped provider extension is named, so
  ;; this stays green when the set of bundled vendors changes, and it proves the
  ;; thing a deployment actually relies on: an OpenAI-compatible endpoint put in
  ;; config reaches a real model call out of the native image.
  (it
    "answers through a disk-backed session store with debug logging enabled"
    (let [dir
          (temp-dir "vis-native-agent")

          {:keys [server asked port]}
          (start-stub-provider! "hello world")]

      (try
        (overlay! dir port)
        (let [{:keys [exit output]}
              (run-binary dir
                          [(.getAbsolutePath (require-binary))
                           (str "-Duser.home=" (.getAbsolutePath dir)) "--db"
                           (.getAbsolutePath (io/file dir "sessions")) "--debug" "--raw"
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
                   (remove str/blank?))

              migration-logs
              (->> (file-seq (io/file dir ".vis/logs"))
                   (filter #(and (.isFile ^File %) (str/ends-with? (.getName ^File %) ".log")))
                   (mapcat #(str/split-lines (slurp %)))
                   (filter #(str/includes? % "org.flywaydb."))
                   vec)]

          (expect (= 0 exit) output)
          (expect (.isFile (io/file dir "sessions" "vis.db")))
          (expect (str/includes? output "hello world") output)
          ;; Indexed SQL must migrate a fresh native store without Flyway
          ;; trying to enumerate resource: URLs for Java migrations.
          (expect (some #(str/includes? % "Successfully applied 1 migration") migration-logs)
                  (pr-str migration-logs))
          (expect (not-any? #(str/includes? % "Unable to scan location") migration-logs)
                  (pr-str migration-logs))
          ;; Without this the test would also pass on a machine whose own
          ;; ~/.vis holds a real credential, and would prove nothing.
          (expect (seq requests) (str "the binary never called the configured provider:\n" output))
          (expect (str/includes? path "/chat/completions") (str "unexpected provider route: " path))
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

(defn- python-call-body
  [stream? call-id code]
  (let [call {:id call-id
              :type "function"
              :function {:name "python_execution" :arguments (json/write-json-str {:code code})}}]
    (if stream?
      (str (json-chunk (json/write-json-str {:index 0
                                             :delta {:role "assistant"
                                                     :tool_calls [(assoc call :index 0)]}
                                             :finish_reason nil}))
           (json-chunk "{\"index\":0,\"delta\":{},\"finish_reason\":\"tool_calls\"}")
           "data: [DONE]\n\n")
      (json/write-json-str {:id "stub"
                            :object "chat.completion"
                            :model "stub-model"
                            :choices [{:index 0
                                       :message {:role "assistant" :content nil :tool_calls [call]}
                                       :finish_reason "tool_calls"}]
                            :usage {:prompt_tokens 1 :completion_tokens 2 :total_tokens 3}}))))

(defdescribe
  native-sandbox-extension-sdk-test
  ;; Issue #253: exercise the SDK bundled in the image through a real model tool call.
  (it
    "imports declarations in python_execution without trusted host or process access"
    (let [dir
          (temp-dir "vis-native-sdk-")

          calls
          (atom 0)

          original-stream
          stream-body

          original-whole
          whole-body

          code
          (str
            "import blockether.vis.extension as sdk\n"
            "import vis_decisions\n" "import inspect, subprocess\n"
            "assert 'label' in inspect.signature(sdk.ActivityProgress).parameters\n"
            "assert hasattr(vis_decisions, 'infer') and hasattr(vis_decisions, 'models')\n"
            "assert 'torch' not in sys.modules and 'blockether.vis.decisions' not in sys.modules\n"
            "assert sdk.ActivityProgress('Inspect SDK', value=1, total=2).value == 1\n"
            "prototype = sdk.Extension(name='native-prototype', description='Local declaration')\n"
            "for operation in [lambda: sdk.register_extension(prototype), "
            "lambda: sdk.state.get('key'), lambda: sdk.shell({'command': 'exit 0'})]:\n"
            "    try:\n"
            "        operation()\n" "    except RuntimeError as error:\n"
            "        assert 'unavailable in python_execution' in str(error)\n" "    else:\n"
            "        raise AssertionError('Extension host operation allowed')\n"
            "assert sdk._registration['spec'] is None\n"
            "assert 'blockether.vis._outside' not in sys.modules\n" "try:\n"
            "    sdk.fs.read('unused')\n" "except PermissionError as error:\n"
            "    assert 'not trusted' in str(error)\n" "else:\n"
            "    raise AssertionError('Trusted filesystem access granted')\n" "try:\n"
            "    subprocess.run(['/usr/bin/true'], check=True)\n" "except RuntimeError:\n"
            "    pass\n" "else:\n"
            "    raise AssertionError('Direct process creation allowed')\n"
            "print('Native SDK sandbox verified')")

          respond
          (fn [stream? reply]
            (if (= 1 (swap! calls inc))
              (python-call-body stream? "native-sdk" code)
              ((if stream? original-stream original-whole) reply)))]

      (try
        (with-redefs [stream-body
                      #(respond true %)

                      whole-body
                      #(respond false %)]

          (let [{:keys [server port]} (start-stub-provider! "SDK check complete.")]
            (try (overlay! dir port)
                 (spit (io/file dir ".vis/config.yml") "\njail:\n  enabled: true\n" :append true)
                 (let [database (io/file dir "sessions")
                       {:keys [finished? exit output]}
                       (run-binary dir
                                   [(.getAbsolutePath (require-binary))
                                    (str "-Duser.home=" (.getAbsolutePath dir)) "--db"
                                    (.getAbsolutePath database) "--raw"
                                    "Inspect the bundled extension SDK"]
                                   180)]

                   (expect finished? output)
                   (expect (= 0 exit) output)
                   (expect (= 2 @calls) output)
                   (let [store (ps/db-create-connection! (.getAbsolutePath database))]
                     (try (let [sid (:id (first (ps/db-list-sessions store :all)))
                                forms (mapcat :forms
                                              (mapcat #(ps/db-list-session-turn-iterations store
                                                                                           (:id %))
                                                      (ps/db-list-session-turns store sid)))]

                            (expect (= 1 (count forms)) (pr-str forms))
                            (expect (every? #(nil? (:error %)) forms) (pr-str forms))
                            (expect (= "Native SDK sandbox verified\n" (:stdout (first forms)))
                                    (pr-str forms)))
                          (finally (ps/db-dispose-connection! store)))))
                 (finally (.stop ^HttpServer server 0)))))
        (finally (delete-tree! dir))))))

;; Issue #256: the built image must carry both the serializer and the runtime decoder.
(defdescribe
  native-sequence-result-test
  (it
    "restores opted-in collections through an installed extension and python_execution"
    (let [dir
          (temp-dir "vis-native-sequence-")

          calls
          (atom 0)

          original-stream
          stream-body

          original-whole
          whole-body

          code
          (slurp (io/file "test/resources/sequence_check.py"))

          respond
          (fn [stream? reply]
            (if (= 1 (swap! calls inc))
              (python-call-body stream? "native-sequence" code)
              ((if stream? original-stream original-whole) reply)))]

      (try
        (with-redefs [stream-body
                      #(respond true %)

                      whole-body
                      #(respond false %)]

          (let [{:keys [server port]} (start-stub-provider! "Sequence check complete.")]
            (try (overlay! dir port)
                 (spit (io/file dir ".vis/config.yml") "\njail:\n  enabled: true\n" :append true)
                 (let [entry (io/file dir ".vis/extensions/sequence_fixture.py")]
                   (io/make-parents entry)
                   (spit entry (slurp (io/file "test/resources/sequence_extension.py"))))
                 (let [database (io/file dir "sessions")
                       {:keys [finished? exit output]}
                       (run-binary dir
                                   [(.getAbsolutePath (require-binary))
                                    (str "-Duser.home=" (.getAbsolutePath dir)) "--db"
                                    (.getAbsolutePath database) "--raw" "Check typed sequences"]
                                   180)]

                   (expect finished? output)
                   (expect (= 0 exit) output)
                   (expect (= 2 @calls) output)
                   (let [store (ps/db-create-connection! (.getAbsolutePath database))]
                     (try (let [sid (:id (first (ps/db-list-sessions store :all)))
                                forms (mapcat :forms
                                              (mapcat #(ps/db-list-session-turn-iterations store
                                                                                           (:id %))
                                                      (ps/db-list-session-turns store sid)))]

                            (expect (= 1 (count forms)) (pr-str forms))
                            (expect (every? #(nil? (:error %)) forms) (pr-str forms))
                            (expect (= "Field-backed sequences verified\n" (:stdout (first forms)))
                                    (pr-str forms)))
                          (finally (ps/db-dispose-connection! store)))))
                 (finally (.stop ^HttpServer server 0)))))
        (finally (delete-tree! dir))))))

(defn- goal-update-body
  [stream? status]
  (python-call-body stream?
                    "native-goal-update"
                    (str "g = session['goal']\n"
                         "assert g['status'] == 'active', g\n"
                         "print(update_goal(g['id'], g['version'], '"
                         status
                         "', 'Native fixture verified the terminal condition.'))")))

(defn- native-goal-case!
  [{:keys [progress-count resolution budget status iterations]}]
  (let [dir
        (temp-dir "vis-native-goal-")

        progress-count
        (long progress-count)

        original-stream
        stream-body

        original-whole
        whole-body

        calls
        (atom 0)

        respond
        (fn [stream? reply]
          (let [n (long (swap! calls inc))]
            (if (and resolution (= n (inc progress-count)))
              (goal-update-body stream? resolution)
              ((if stream? original-stream original-whole)
                (cond (<= n progress-count) (str "Native goal progress " n ".")
                      resolution reply
                      :else "")))))]

    (try
      (with-redefs [stream-body
                    (fn [reply]
                      (respond true reply))

                    whole-body
                    (fn [reply]
                      (respond false reply))]

        (let [{:keys [server asked port]} (start-stub-provider! "Native goal resolved.")]
          (try
            (overlay! dir port)
            (let [database (io/file dir "sessions")
                  {:keys [finished? exit output]}
                  (run-binary dir
                              [(.getAbsolutePath (require-binary))
                               (str "-Duser.home=" (.getAbsolutePath dir)) "--db"
                               (.getAbsolutePath database) "--raw"
                               (str "/goal --budget " budget " Verify native goal continuation")]
                              180)
                  requests @asked]

              (expect finished? "Native goal continuation must finish without a process timeout")
              (when-not (= "paused" status) (expect (= 0 exit) output))
              (expect (= iterations (count requests)) output)
              (expect (str/includes? (:body (first requests)) "3 consecutive goal continuations")
                      "The linked image must include the current model-assessed blocker audit")
              (when resolution
                ;; #216: terminal tool evidence ends the goal without another provider request.
                (expect (str/includes? output
                                       (str "Goal "
                                            resolution
                                            ": Native fixture verified the terminal condition."))
                        output)
                (expect (not (str/includes? output "Native goal resolved.")))
                (doseq [n (range 1 (inc progress-count))]
                  (let [messages (get (json/read-json (:body (nth requests n))) "messages")
                        previous (filter #(= "assistant" (get % "role")) messages)]

                    (expect (str/includes? (pr-str previous) (str "Native goal progress " n "."))
                            "Each delivered progress reply must survive native provider replay")
                    (expect (str/includes? (str messages) "<goal_continuation>"))
                    (expect (not (str/includes? (str messages) "(final-answer-validation)"))))))
              ;; Reopen the native process's isolated disk store, not a JVM substitute turn.
              (let [store (ps/db-create-connection! (.getAbsolutePath database))]
                (try
                  (let [sessions (ps/db-list-sessions store :all)
                        sid (:id (first sessions))
                        goal (ps/db-get-session-goal store sid)
                        turns (ps/db-list-session-turns store sid)
                        saved (vec (mapcat #(ps/db-list-session-turn-iterations store (:id %))
                                           turns))
                        prose (filterv :assistant-prose saved)
                        forms (mapcat :forms saved)]

                    (expect (= 1 (count sessions)))
                    (expect (= 1 (count turns)) "Goal replies continue the same native user turn")
                    (expect (= status (get goal "status")))
                    (expect (= iterations (get goal "iterations_used")))
                    (expect (= budget (get goal "iteration_budget")))
                    ;; #216: empty replies retain diagnostics without fabricating provider work.
                    (expect (= (if (= "paused" status)
                                 (vec (repeat iterations
                                              (str "Provider returned no executable tool call or "
                                                   "answer text; nothing was executed.")))
                                 (mapv #(str "Native goal progress " % ".")
                                       (range 1 (inc progress-count))))
                               (mapv :assistant-prose prose)))
                    (expect (every? #(empty? (:forms %)) prose)
                            "Prose rows must not persist fake validation-error forms")
                    (expect (every? #(nil? (:error %)) forms) (pr-str forms))
                    (when resolution
                      (expect (= 1 (count forms)))
                      (expect (str/includes? (:stdout (first forms)) status)
                              "The terminal update must execute through the native Python host")))
                  (finally (ps/db-dispose-connection! store)))))
            (finally (.stop ^HttpServer server 0)))))
      (finally (delete-tree! dir)))))

(defdescribe
  native-goal-continuation-test
  (it "retains repeated progress beyond the empty-reply limit before explicit completion"
      (native-goal-case!
        {:progress-count 4 :resolution "complete" :budget 10 :status "complete" :iterations 5}))
  (it "stops after an explicit blocker without losing the preceding progress replies"
      (native-goal-case!
        {:progress-count 3 :resolution "blocked" :budget 10 :status "blocked" :iterations 4}))
  (it "retains the last progress reply but makes no request beyond the iteration budget"
      (native-goal-case! {:progress-count 1 :budget 1 :status "budget_limited" :iterations 1}))
  (it "still pauses an unresolved goal after genuinely empty replies"
      (native-goal-case! {:progress-count 0 :budget 8 :status "paused" :iterations 3})))

(defdescribe
  native-attachment-review-capability-test
  (it
    "preserves review capabilities and draft checkpoints through native agent turns"
    (let [dir
          (temp-dir "vis-native-attachment-review")

          project
          (io/file dir "workspace")

          calls
          (atom 0)

          stream-response
          stream-body

          whole-response
          whole-body

          code
          (str
            "report = attach(\n"
            "    b'# Implementation report\\n', filename='IMPLEMENTATION-native.md',\n"
            "    kind='doc', media_type='text/markdown')\n"
            "assert report['commentable'] is False\n" "spec = attach(\n"
            "    b'# Specification\\n', filename='PLAN-native.md', kind='doc',\n"
            "    media_type='text/markdown', commentable=True)\n"
            "assert spec['commentable'] is True\n" "payload = {\n"
            "    'schema_version': 1,\n"
            "    'patch': '--- a/code.txt\\n+++ b/code.txt\\n@@ -1 +1 @@\\n-old\\n+new\\n',\n"
            "    'source': {'type': 'workspace', 'label': 'Native fixture'}, 'comments': []}\n"
            "change = attach(\n"
            "    json.dumps(payload).encode('utf-8'), filename='DIFF-native.json', kind='diff',\n"
            "    media_type='application/vnd.vis.diff+json', commentable=True)\n"
            "assert change['commentable'] is True\n"
            "assert get_attachment(spec)['commentable'] is True\n"
            "assert get_attachment(change)['kind'] == 'diff'\n"
            "assert json.loads(read_attachment(change))['patch'] == payload['patch']\n"
            "assert draft_status()['in_draft'] is True\n"
            "(project_root_path / 'code.txt').write_text('new\\n', encoding='utf-8')\n"
            "draft_change = draft_diff(filename='DIFF-draft-native.json')\n"
            "assert draft_change['commentable'] is True\n"
            "assert draft_change['empty'] is False\n"
            "draft_body = json.loads(read_attachment(draft_change))\n"
            "assert draft_body['source']['type'] == 'draft'\n"
            "assert draft_body['source']['backend'] == 'worktree'\n"
            "assert '-old\\n' in draft_body['patch'] and '+new\\n' in draft_body['patch']\n"
            "empty_change = draft_diff(\n"
            "    filename='DIFF-draft-empty-native.json', since=draft_change['checkpoint'])\n"
            "assert empty_change['empty'] is True\n"
            "empty_body = json.loads(read_attachment(empty_change))\n"
            "assert empty_body['patch'] == ''\n"
            "assert empty_body['source']['base_revision'] == draft_change['checkpoint']\n"
            "print('Native attachment review verified')\n")

          respond
          (fn [stream? reply]
            (case (swap! calls inc)
              1
              (python-call-body stream?
                                "native-draft-create"
                                "print(draft_create('native-review'))")

              2
              (python-call-body stream? "native-attachment-review" code)

              ((if stream? stream-response whole-response) reply)))]

      (try
        (with-redefs [stream-body
                      #(respond true %)

                      whole-body
                      #(respond false %)]

          (let [{:keys [server port]} (start-stub-provider! "Native attachment review complete.")]
            (try
              (.mkdirs project)
              (spit (io/file project "code.txt") "old\n")
              (spit (io/file project ".gitignore") ".vis/\nrun.log\n")
              (doseq [argv [["git" "init" "--quiet" "-b" "main"]
                            ["git" "add" "--" "code.txt" ".gitignore"]
                            ["git" "-c" "user.name=Native fixture" "-c"
                             "user.email=native@example.com" "-c" "commit.gpgsign=false" "commit"
                             "--quiet" "-m" "test: seed native draft"]]]
                (let [{:keys [finished? exit output]} (run-binary project argv 30)]
                  (expect finished? output)
                  (expect (= 0 exit) output)))
              (overlay! project port)
              ;; #242: draft review keeps the source checkout confined.
              (spit (io/file project ".vis/config.yml")
                    (str "\ntoggles:\n  draft_backend: worktree\n" "\njail:\n  enabled: true\n")
                    :append
                    true)
              (let [database (io/file dir "sessions")
                    {:keys [finished? exit output]}
                    (run-binary
                      project
                      [(.getAbsolutePath (require-binary))
                       (str "-Duser.home=" (.getAbsolutePath dir)) "--db"
                       (.getAbsolutePath database) "--raw"
                       "Create a disposable draft; produce a specification, report and diffs"]
                      180)]

                (expect finished? output)
                (expect (= 0 exit) output)
                (expect (= "old\n" (slurp (io/file project "code.txt"))))
                (let [store (ps/db-create-connection! (.getAbsolutePath database))]
                  (try
                    (let [sid (:id (first (ps/db-list-sessions store :all)))
                          iterations (mapcat #(ps/db-list-session-turn-iterations store (:id %))
                                             (ps/db-list-session-turns store sid))
                          rows (mapcat #(ps/db-list-iteration-attachments store (:id %)) iterations)
                          artifacts (into {} (map (juxt :filename identity)) rows)]

                      (expect (= #{"IMPLEMENTATION-native.md" "PLAN-native.md" "DIFF-native.json"
                                   "DIFF-draft-native.json" "DIFF-draft-empty-native.json"}
                                 (set (keys artifacts))))
                      (expect (false? (:commentable (get artifacts "IMPLEMENTATION-native.md"))))
                      (expect (true? (:commentable (get artifacts "PLAN-native.md"))))
                      (expect (true? (:commentable (get artifacts "DIFF-native.json"))))
                      (expect (= "diff" (:kind (get artifacts "DIFF-native.json"))))
                      (expect (true? (:commentable (get artifacts "DIFF-draft-native.json"))))
                      (expect (true? (:commentable (get artifacts "DIFF-draft-empty-native.json"))))
                      (expect (every? #(nil? (:error %)) (mapcat :forms iterations)))
                      (expect (some #(str/includes? (str (:stdout %))
                                                    "Native attachment review verified")
                                    (mapcat :forms iterations))))
                    (finally (ps/db-dispose-connection! store)))))
              (finally (.stop ^HttpServer server 0)))))
        (finally (delete-tree! dir))))))

(defdescribe native-linked-report-delivery-test
             ;; #193: exercise CommonMark source spans and secure directory handles in the image.
             (it "snapshots a local report in a complete native agent turn"
                 (let [dir
                       (temp-dir "vis-native-report")

                       {:keys [server port]}
                       (start-stub-provider! "[Report](report.md)")]

                   (try (overlay! dir port)
                        (spit (io/file dir "report.md") "# Native report\n")
                        (let [{:keys [exit output]} (run-binary dir
                                                                [(.getAbsolutePath (require-binary))
                                                                 "--db" ":memory" "--raw"
                                                                 "Link the report"]
                                                                180)]
                          (expect (= 0 exit) output)
                          (expect (str/includes? output "attachment://") output)
                          (expect (not (str/includes? output "](report.md)")) output))
                        (finally (.stop server 0) (delete-tree! dir))))))

;; ── the embedded CPython, inside the linked image ────────────────────────────

(defn- run-python
  "`vis-agent python -c CODE` against the private binary under the release wrapper's environment."
  [^File dir ^File bin code]
  (let [library (python-library bin)]
    (expect library
            (str "No CPython sidecar beside " (.getAbsolutePath bin)
                 " — `clojure -T:build native` stages vis-agent-python/ there. " build-it))
    (run-binary dir [(.getAbsolutePath bin) "python" "-c" code] 300)))

(defn- python-tool-body
  [code call-number stream?]
  (let [call {:id (str "native-interrupt-" call-number)
              :type "function"
              :function {:name "python_execution" :arguments (json/write-json-str {:code code})}}]
    (if stream?
      (str (json-chunk (json/write-json-str {:index 0
                                             :delta {:role "assistant"
                                                     :tool_calls [(assoc call :index 0)]}
                                             :finish_reason nil}))
           (json-chunk "{\"index\":0,\"delta\":{},\"finish_reason\":\"tool_calls\"}")
           "data: [DONE]\n\n")
      (json/write-json-str {:id "stub"
                            :object "chat.completion"
                            :model "stub-model"
                            :choices [{:index 0
                                       :message {:role "assistant" :content nil :tool_calls [call]}
                                       :finish_reason "tool_calls"}]
                            :usage {:prompt_tokens 1 :completion_tokens 2 :total_tokens 3}}))))

(defdescribe
  native-python-worker-log-test
  (it
    "reads dated worker output while the native interpreter is still running"
    (let [dir
          (temp-dir "vis-native-worker-log")

          original-stream
          @#'stream-body

          original-whole
          @#'whole-body

          calls
          (atom 0)

          code
          (str "import os, time\nfrom pathlib import Path\n"
               "os.write(2, b'native-worker-log-fixture\\n')\n"
               "logs = Path("
               (pr-str (.getAbsolutePath (io/file dir ".vis" "logs")))
               ")\n"
               "for attempt in range(200):\n"
               "    matches = [p for p in logs.glob('????-??-??/pyext-*/worker.log')\n"
               "               if 'native-worker-log-fixture' in p.read_text()]\n"
               "    if matches:\n        break\n    time.sleep(0.01)\n"
               "assert len(matches) == 1, 'Worker output must be readable before exit'\n"
               "print('Native worker log ' + 'verified')")

          reply
          (fn [stream? text]
            (if (= 1 (swap! calls inc))
              (python-tool-body code 1 stream?)
              ((if stream? original-stream original-whole) text)))]

      (try (with-redefs-fn {#'stream-body #(reply true %) #'whole-body #(reply false %)}
             (fn []
               (let [{:keys [server asked port]} (start-stub-provider! "Worker check complete")]
                 (try (overlay! dir port)
                      (let [{:keys [finished? exit output]}
                            (run-binary dir
                                        [(.getAbsolutePath (require-binary))
                                         (str "-Duser.home=" (.getAbsolutePath dir)) "--db"
                                         (.getAbsolutePath (io/file dir "sessions")) "--raw"
                                         "Run the supplied Python fixture and finish."]
                                        180)
                            tools (->> @asked
                                       (mapcat #(get (json/read-json (:body %)) "messages"))
                                       (filter #(= "tool" (get % "role")))
                                       (map #(str (get % "content"))))]

                        (expect finished? output)
                        (expect (= 0 exit) output)
                        (expect (some #(str/includes? % "Native worker log verified") tools)
                                (pr-str tools)))
                      (finally (.stop server 0))))))
           (finally (delete-tree! dir))))))

(defdescribe
  native-python-interrupt-control-test
  (it
    "unwinds a timed-out Python block and reuses its interpreter through the linked control plane"
    (let [dir
          (temp-dir "vis-native-interrupt")

          original-stream
          @#'stream-body

          original-whole
          @#'whole-body

          calls
          (atom 0)

          fixtures
          ["cancelled_value = 41\nwhile True:\n    pass"
           "print('NATIVE_INTERRUPT_RECOVERED', cancelled_value + 1)"]

          reply
          (fn [stream? text]
            (let [n (swap! calls inc)]
              (if-let [code (get fixtures (dec n))]
                (python-tool-body code n stream?)
                ((if stream? original-stream original-whole) text))))]

      (try (with-redefs-fn {#'stream-body #(reply true %) #'whole-body #(reply false %)}
             (fn []
               (let [{:keys [server asked port]} (start-stub-provider! "NATIVE_INTERRUPT_COMPLETE")]
                 (try (overlay! dir port)
                      ;; The CLI uses the production five-minute eval budget. This slow
                      ;; native-only proof must execute interrupt!, not just link it.
                      (let [{:keys [finished? exit output]}
                            (run-binary dir
                                        [(.getAbsolutePath (require-binary))
                                         (str "-Duser.home=" (.getAbsolutePath dir)) "--db"
                                         (.getAbsolutePath (io/file dir "sessions")) "--raw"
                                         "Run the supplied Python fixtures and finish."]
                                        420)
                            tools (->> @asked
                                       (mapcat #(get (json/read-json (:body %)) "messages"))
                                       (filter #(= "tool" (get % "role")))
                                       (map #(str (get % "content"))))]

                        (expect finished? "A Python timeout must not wedge the linked agent")
                        (expect (= 0 exit) output)
                        (expect (some #(and (str/includes? % "Time limit reached")
                                            (str/includes? % "Python state is kept."))
                                      tools)
                                (pr-str tools))
                        (expect (some #(str/includes? % "NATIVE_INTERRUPT_RECOVERED 42") tools)
                                (pr-str tools))
                        (expect (str/includes? output "NATIVE_INTERRUPT_COMPLETE") output))
                      (finally (.stop server 0))))))
           (finally (delete-tree! dir))))))

(defdescribe
  native-python-hang-evidence-test
  (it
    "saves Python and JVM evidence before restarting a GIL-stuck native worker and finishes the turn"
    (let [dir
          (temp-dir "vis-native-hang")

          pid-file
          (io/file dir "fixture-worker.pid")

          original-stream
          @#'stream-body

          original-whole
          @#'whole-body

          calls
          (atom 0)

          fixture
          (str "import os\nfrom pathlib import Path\n" "def native_hang_fixture():\n"
               "    private_value = 'native-hang-private-payload'\n"
               "    Path('fixture-worker.pid').write_text(str(os.getpid()))\n"
               "    sum(range(1000000000000000000))\n" "native_hang_fixture()")

          reply
          (fn [stream? text]
            (if (= 1 (swap! calls inc))
              (python-tool-body fixture 1 stream?)
              ((if stream? original-stream original-whole) text)))]

      (try
        (with-redefs-fn {#'stream-body #(reply true %) #'whole-body #(reply false %)}
          (fn []
            (let [{:keys [server asked port]} (start-stub-provider! "NATIVE_HANG_RESTART_COMPLETE")]
              (try
                (overlay! dir port)
                ;; Exercise the production five-minute block timeout and failed interrupt.
                ;; A JVM-only test cannot prove the linked host and worker share this ABI.
                (let [{:keys [finished? exit output]}
                      (run-binary dir
                                  [(.getAbsolutePath (require-binary))
                                   (str "-Duser.home=" (.getAbsolutePath dir)) "--db"
                                   (.getAbsolutePath (io/file dir "sessions")) "--raw"
                                   "Run the supplied Python fixture and finish after its timeout."]
                                  420)
                      ;; A worker's run directory IS its diagnostic log directory,
                      ;; `~/.vis/logs/<UTC date>/pyext-*`, and the hang report is written
                      ;; beside that worker's own log.
                      reports (->> (file-seq (io/file dir ".vis/logs"))
                                   (filter #(= "hang.edn" (.getName ^File %))))
                      tools (->> @asked
                                 (mapcat #(get (json/read-json (:body %)) "messages"))
                                 (filter #(= "tool" (get % "role")))
                                 (map #(str (get % "content"))))]

                  (expect finished? "A GIL-held worker must not wedge the linked agent")
                  (expect (= 0 exit) output)
                  (expect (.isFile pid-file) "The fixture must enter its native GIL-holding call")
                  (expect (some #(str/includes? % "Vis restarted Python") tools) (pr-str tools))
                  (expect (= 2 @calls) "The turn must go on once, without replaying the block")
                  (expect (str/includes? output "NATIVE_HANG_RESTART_COMPLETE") output)
                  (expect (= 1 (count reports)) "Retirement must preserve one diagnostic report")
                  (when-let [^File report-file (first reports)]
                    (let [report (edn/read-string (slurp report-file))
                          stacks (:python-stacks report)
                          stack-file (when (string? (:path stacks)) (io/file (:path stacks)))
                          worker-pid (when (.isFile pid-file) (parse-long (slurp pid-file)))]

                      (expect (= worker-pid (:pid report)))
                      (expect (seq (:jvm-threads report)))
                      (expect (seq (get-in report [:rpc :outbound :active])))
                      (expect (not (str/includes? (slurp report-file)
                                                  "native-hang-private-payload")))
                      (expect (= :written (:status stacks)))
                      (expect (some? stack-file))
                      (when stack-file
                        (expect (= (.getCanonicalFile (.getParentFile report-file))
                                   (.getCanonicalFile (.getParentFile ^File stack-file))))
                        (expect (.isFile ^File stack-file))
                        (when (.isFile ^File stack-file)
                          (expect (str/includes? (slurp stack-file) "native_hang_fixture"))
                          (expect (not (str/includes? (slurp stack-file)
                                                      "native-hang-private-payload")))))
                      (doseq [^File artifact (remove nil? [report-file stack-file])]
                        (when (.isFile artifact)
                          (expect (= (java.nio.file.attribute.PosixFilePermissions/fromString
                                       "rw-------")
                                     (Files/getPosixFilePermissions
                                       (.toPath artifact)
                                       (make-array java.nio.file.LinkOption 0))))))
                      (when worker-pid
                        (expect (not (some-> (ProcessHandle/of worker-pid)
                                             (.orElse nil)
                                             .isAlive))
                                "The diagnosed worker must be retired before the turn finishes")))))
                (finally (.stop server 0))))))
        (finally (delete-tree! dir))))))

(defdescribe
  native-python-repair-test
  (it
    "repairs an unbalanced Python block before it runs and explains a block it cannot repair"
    (let [dir
          (temp-dir "vis-native-repair")

          original-stream
          @#'stream-body

          original-whole
          @#'whole-body

          calls
          (atom 0)

          fixtures
          ["xs = [1, 2\nprint('NATIVE_REPAIR', len(xs))" "x = (1 + 2\ny = 3 3"]

          reply
          (fn [stream? text]
            (let [n (swap! calls inc)]
              (if-let [code (get fixtures (dec n))]
                (python-tool-body code n stream?)
                ((if stream? original-stream original-whole) text))))]

      (try
        (with-redefs-fn {#'stream-body #(reply true %) #'whole-body #(reply false %)}
          (fn []
            (let [{:keys [server asked port]} (start-stub-provider! "NATIVE_REPAIR_COMPLETE")]
              (try
                (overlay! dir port)
                ;; The repair engine is Java code from the parinferish jar; only the
                ;; linked image proves that it runs there.
                (let [{:keys [finished? exit output]}
                      (run-binary dir
                                  [(.getAbsolutePath (require-binary))
                                   (str "-Duser.home=" (.getAbsolutePath dir)) "--db"
                                   (.getAbsolutePath (io/file dir "sessions")) "--raw"
                                   "Run the supplied Python fixtures and finish."]
                                  180)
                      tools (->> @asked
                                 (mapcat #(get (json/read-json (:body %)) "messages"))
                                 (filter #(= "tool" (get % "role")))
                                 (map #(str (get % "content"))))]

                  (expect finished? output)
                  (expect (= 0 exit) output)
                  (expect (some #(and (str/includes? % "Vis repaired this block before running it.")
                                      (str/includes? % "NATIVE_REPAIR 2"))
                                tools)
                          (pr-str tools))
                  (expect
                    (some
                      #(and
                         (str/includes?
                           %
                           "Vis could not repair the unbalanced quotes or brackets in this block:")
                         (str/includes? % "'(' is never closed"))
                      tools)
                    (pr-str tools))
                  (expect (str/includes? output "NATIVE_REPAIR_COMPLETE") output))
                (finally (.stop server 0))))))
        (finally (delete-tree! dir))))))

;; Regression, this branch: the interpreter reaches CPython through the JDK Foreign
;; Function & Memory API, and an image links no downcall stub it was not told about.
;; With the registrations missing the JVM suite stayed green while the binary died on
;; the first Python call with MissingForeignRegistrationError; with the runtime's
;; SOURCES manifest missing from the image it died with ModuleNotFoundError:
;; vis_runtime. Both are invisible everywhere except here.
(defdescribe
  native-binary-runs-python-in-the-embedded-interpreter-test
  (it
    "executes a block and prints what the block printed"
    (let
      [dir
       (temp-dir "vis-native-python")

       {:keys [exit output]}
       (run-python
         dir
         (require-binary)
         "import json, sys; print(json.dumps({'v': sys.version_info[:2], 'n': sum(range(11))}))")]

      (try (expect (= 0 exit) output)
           (expect (str/includes? output "\"n\": 55")
                   (str "the embedded interpreter did not run the block:\n" output))
           (expect (re-find #"\"v\": \[3, \d+\]" output)
                   (str "no CPython 3 in the binary:\n" output))
           (finally (delete-tree! dir)))))
  (it
    "loads secrets from the bundled standard library without installed packages"
    ;; SciPy imports secrets; verify that dependency in a clean native CLI process.
    (let [dir (temp-dir "vis-native-secrets")]
      (try
        (let
          [{:keys [exit output]}
           (run-binary
             dir
             [(.getAbsolutePath (require-binary)) (str "-Duser.home=" (.getAbsolutePath dir))
              "python" "--no-env" "--no-network" "-c"
              (str
                "import secrets, sysconfig\n" "from pathlib import Path\n"
                "assert Path(secrets.__file__).resolve().parent == Path(sysconfig.get_path('stdlib')).resolve()\n"
                "assert len(secrets.token_bytes(32)) == 32\n"
                "assert len(secrets.token_hex(32)) == 64\n"
                "assert len(secrets.token_urlsafe(32)) == 43\n"
                "assert 0 <= secrets.randbelow(128) < 128\n"
                "assert secrets.compare_digest(b'native-check', b'native-check')\n"
                "assert not secrets.compare_digest(b'native-check', b'other-check')\n"
                "print('NATIVE_SECRETS_READY')\n")]
             60)]
          (expect (= 0 exit) output)
          (expect (str/includes? output "NATIVE_SECRETS_READY") output))
        (finally (delete-tree! dir)))))
  (it "reads a file through the guarded filesystem door"
      ;; Reading is the audit hook's happy path: the block's own directory is a
      ;; session root, and an interpreter whose confinement policy failed to install
      ;; refuses this — the failure that only appears once the image is linked.
      (let [dir
            (temp-dir "vis-native-python-read")

            note
            (doto (io/file dir "note.txt") (spit "seventy seven"))

            {:keys [exit output]}
            (run-python dir
                        (require-binary)
                        (str "print(open(" (pr-str (.getAbsolutePath note)) ").read())"))]

        (try (expect (= 0 exit) output)
             (expect (str/includes? output "seventy seven") output)
             (finally (delete-tree! dir))))))

(defdescribe
  native-python-module-asyncio-test
  (it "lets a module own asyncio and preserves its output and exit code"
      ;; JVM/SDK dogfooding: module execution must not nest asyncio event loops.
      (let [dir (temp-dir "vis-native-module-asyncio")]
        (try (spit (io/file dir "async_cli_probe.py")
                   (str "import asyncio, sys\n"
                        "async def compute():\n    await asyncio.sleep(0)\n    return 42\n"
                        "print('module-result', asyncio.run(compute()), sys.argv[1])\n"
                        "raise SystemExit(7)\n"))
             (let [{:keys [exit output]} (run-binary dir
                                                     [(.getAbsolutePath (require-binary))
                                                      (str "-Duser.home=" (.getAbsolutePath dir))
                                                      "python" "--no-env" "--no-network" "--env"
                                                      (str "PYTHONPATH=" (.getAbsolutePath dir))
                                                      "-m" "async_cli_probe" "argument"]
                                                     60)]
               (expect (= 7 exit) output)
               (expect (str/includes? output "module-result 42 argument") output))
             (finally (delete-tree! dir))))))

(defn- pip-wheel
  "An offline pure-Python wheel, with no build backend or external dependency."
  []
  (let [out (java.io.ByteArrayOutputStream.)]
    (with-open [zip (java.util.zip.ZipOutputStream. out)]
      (doseq [[name text]
              {"vis_cli_fixture/__init__.py" "VALUE = 42\n"
               "vis_cli_fixture-1.0.dist-info/METADATA"
               "Metadata-Version: 2.1\nName: vis-cli-fixture\nVersion: 1.0\n"
               "vis_cli_fixture-1.0.dist-info/WHEEL"
               "Wheel-Version: 1.0\nGenerator: vis-test\nRoot-Is-Purelib: true\nTag: py3-none-any\n"
               "vis_cli_fixture-1.0.dist-info/RECORD" ""}]
        (.putNextEntry zip (java.util.zip.ZipEntry. ^String name))
        (.write zip (.getBytes ^String text StandardCharsets/UTF_8))
        (.closeEntry zip)))
    (.toByteArray out)))

;; Regression: the public Python CLI was not tested with pip; an incompatible
;; worker exited before connecting while Vis waited for the entire startup timeout.
(defdescribe
  native-wrapper-installs-python-wheels-test
  (it
    "installs from --index-url, -i and a wheel URL through the shipped wrapper"
    (let [dir
          (temp-dir "vis-native-pip")

          wrapper
          (io/file dir "vis-agent")

          bin
          (require-binary)

          wheel
          (pip-wheel)

          requests
          (atom [])

          server
          (HttpServer/create (InetSocketAddress. "127.0.0.1" 0) 0)

          filename
          "vis_cli_fixture-1.0-py3-none-any.whl"

          run
          (fn [args]
            (with-redefs [native-environment (constantly {})]
              (run-binary dir (into ["bash" (.getAbsolutePath wrapper)] args) 90)))]

      (try (io/copy (io/file "bin/vis-agent") wrapper)
           (Files/createSymbolicLink (.toPath (io/file dir "vis-agent-native"))
                                     (.toPath (.getAbsoluteFile bin))
                                     (make-array FileAttribute 0))
           (Files/createSymbolicLink (.toPath (io/file dir "vis-agent-python"))
                                     (.toPath (.getParentFile (.getAbsoluteFile (python-library
                                                                                  bin))))
                                     (make-array FileAttribute 0))
           (.createContext
             server
             "/"
             (reify
               HttpHandler
                 (handle [_ exchange]
                   (let [path
                         (.getPath (.getRequestURI ^HttpExchange exchange))

                         _
                         (swap! requests conj path)

                         archive?
                         (= path (str "/" filename))

                         body
                         (if archive?
                           wheel
                           (.getBytes (str "<a href='/" filename "'>wheel</a>")
                                      StandardCharsets/UTF_8))]

                     (.set (.getResponseHeaders ^HttpExchange exchange)
                           "Content-Type"
                           (if archive? "application/octet-stream" "text/html"))
                     (.sendResponseHeaders ^HttpExchange exchange 200 (alength ^bytes body))
                     (with-open [stream (.getResponseBody ^HttpExchange exchange)]
                       (.write stream ^bytes body))))))
           (.start server)
           (let [base (str "http://127.0.0.1:" (.getPort (.getAddress server)))]
             (doseq [[label args] [["long" ["--index-url" (str base "/simple") "vis-cli-fixture"]]
                                   ["short" ["-i" (str base "/simple") "vis-cli-fixture"]]
                                   ["wheel" [(str base "/" filename)]]
                                   ["proxy"
                                    ["--proxy" base "--index-url" "http://127.0.0.2:9/simple"
                                     "vis-cli-fixture"]]]]
               (let [dest (io/file dir label)
                     result (run (into ["python" "-m" "pip" "--isolated" "install"
                                        "--disable-pip-version-check" "--no-cache-dir" "--no-deps"
                                        "--only-binary=:all:" "--target" (.getAbsolutePath dest)]
                                       args))]

                 (expect (= 0 (:exit result)) (str label ": " (:output result)))
                 (expect (.isFile (io/file dest "vis_cli_fixture" "__init__.py"))
                         (str label ": wheel was not installed"))
                 (let [imported
                       (run ["python" "-c"
                             (str "import sys; sys.path.insert(0, "
                                  (pr-str (.getAbsolutePath dest))
                                  "); import vis_cli_fixture; print(vis_cli_fixture.VALUE)")])]
                   (expect (= 0 (:exit imported)) (:output imported))
                   (expect (str/includes? (:output imported) "42") (:output imported))))))
           (expect (some #{"/simple/vis-cli-fixture/"} @requests))
           (expect (some #{(str "/" filename)} @requests))
           (finally (.stop server 0) (delete-tree! dir))))))

(defdescribe
  native-registers-locked-uv-extension-test
  (it
    "registers a split implementation using a locked uv source in the native image"
    (let [dir
          (temp-dir "vis-native-uv-extension")

          project
          (doto (io/file dir "implementation") .mkdirs)

          entries
          (doto (io/file dir ".vis/extensions") .mkdirs)

          source
          (doto (io/file project "src") .mkdirs)

          wheel-name
          "vis_cli_fixture-1.0-py3-none-any.whl"

          unrelated
          (io/file dir ".vis/python/packages/unrelated-1.dist-info/METADATA")

          bin
          (require-binary)

          python
          (com.blockether.vispython.Locations/pythonExecutable
            (str (io/file (.getParentFile (python-library bin)) "python")))]

      (try
        (with-open [out (io/output-stream (io/file dir wheel-name))]
          (.write out ^bytes (pip-wheel)))
        (spit (io/file dir "vis.yml") "python:\n  index_url: http://127.0.0.1:9/simple\n")
        (spit (io/file project "pyproject.toml")
              (str "[project]\nname = 'native-extension-project'\nversion = '1.0'\n"
                   "requires-python = '>=3.12'\ndependencies = ['vis-cli-fixture==1.0']\n"
                   "[tool.uv.sources]\nvis-cli-fixture = {path = '../"
                   wheel-name
                   "'}\n"))
        (spit (io/file source "fixture_value.py")
              "from vis_cli_fixture import VALUE\ndef answer():\n    return VALUE\n")
        (spit (io/file entries "fixture.py")
              (str "# /// script\n# dependencies = []\n# [tool.vis]\n"
                   "# project = '../../implementation'\n"
                   "# source_paths = ['../../implementation/src']\n# ///\n"
                   "import blockether.vis.extension as vis\nfrom fixture_value import answer\n"
                   "vis.register_extension(vis.Extension(name='native-uv-value-' + str(answer()), "
                   "description='Native locked project fixture'))\n"))
        (let [locked (run-binary dir
                                 ["uv" "lock" "--project" (str project) "--offline" "--python"
                                  python "--no-python-downloads"]
                                 60)]
          (expect (= 0 (:exit locked)) (:output locked)))
        ;; #178: an unrelated shared distribution update must not invalidate this project.
        (io/make-parents unrelated)
        (spit unrelated "Name: unrelated\nVersion: 1\n")
        (let [synced (run-binary dir
                                 [(.getAbsolutePath bin) (str "-Duser.home=" (.getAbsolutePath dir))
                                  "python" "uv" "sync" "--project" (str project) "--locked"
                                  "--offline" "--python" python]
                                 120)]
          (expect (= 0 (:exit synced)) (:output synced)))
        (spit unrelated "Summary: updated metadata\n" :append true)
        (let [lock-before
              (slurp (io/file project "uv.lock"))

              result
              (run-binary dir
                          [(.getAbsolutePath bin) (str "-Duser.home=" (.getAbsolutePath dir))
                           "extension" "list"]
                          120)]

          (expect (= 0 (:exit result)) (:output result))
          (expect (str/includes? (:output result) "native-uv-value-42") (:output result))
          (expect (= lock-before (slurp (io/file project "uv.lock"))))
          (expect (.isDirectory (io/file project ".venv"))))
        (finally (delete-tree! dir))))))

(defdescribe
  native-installs-extension-center-source-test
  (it
    "loads the packaged SDK parser and installs only after explicit trust"
    (let [bin
          (require-binary)

          dir
          (temp-dir "vis-native-extension-center")

          project
          (doto (io/file dir "source/plugins/greeting") .mkdirs)

          destination
          (io/file dir ".vis/extensions/native-center-example/current")

          args
          [(.getAbsolutePath bin) (str "-Duser.home=" (.getAbsolutePath dir)) "extension" "install"
           (str (io/file dir "source")) "--subdirectory" "plugins/greeting" "--project"]]

      (try (spit (io/file project "pyproject.toml")
                 (str "[project]\nname='native-center-example'\nversion='1.0.0'\n"
                      "description='Native catalog installation fixture'\n"
                      "requires-python='>=3.11'\ndependencies=['vis-agent>=0.1.0']\n"
                      "[tool.vis]\ncategory='tools'\n"))
           (spit (io/file project "extension.py")
                 "raise RuntimeError('install must not execute code')\n")
           (let [refused (run-binary dir args 60)]
             (expect (not= 0 (:exit refused)))
             (expect (not (.exists (.getParentFile destination)))))
           (let [installed (run-binary dir (conj args "--trust") 60)]
             (expect (= 0 (:exit installed)) (:output installed))
             (expect (Files/isSymbolicLink (.toPath destination)))
             (expect (= "1.0.0" (str (Files/readSymbolicLink (.toPath destination)))))
             (expect (Files/isSymbolicLink (.toPath (io/file (.getParentFile destination)
                                                             "1.0.0"))))
             (expect (= (.getCanonicalFile project) (.getCanonicalFile destination)))
             (expect (not (.exists (io/file project "uv.lock")))))
           (finally (delete-tree! dir))))))

(defdescribe
  native-installs-github-project-test
  (it
    "fetches a pinned Git revision and installs only its selected project through the binary"
    (let
      [bin
       (require-binary)

       dir
       (temp-dir "vis-native-github")

       repository
       (doto (io/file dir "repository") .mkdirs)

       project
       (doto (io/file repository "plugins/greeting") .mkdirs)

       shim-dir
       (doto (io/file dir "fixture-bin") .mkdirs)

       git
       (some (fn [path]
               (let [candidate (io/file path "git")]
                 (when (.canExecute candidate) (.getAbsolutePath candidate))))
             (str/split (System/getenv "PATH") (re-pattern File/pathSeparator)))

       extension-source
       (str
         "from pathlib import Path\nimport blockether.vis.extension as vis\n"
         "Path(" (pr-str (str (io/file dir "imported")))
         ").write_text('loaded')\n"
         "vis.register_extension(vis.Extension(name='native-github-example', description='Git fixture'))\n")

       url
       "https://github.com/example-owner/extensions"

       environment
       (native-environment)

       destination
       (io/file dir ".vis/extensions/native-github-example/current")]

      (try
        (spit (io/file project "pyproject.toml")
              (str "[project]\nname='native-github-example'\nversion='1.0.0'\n"
                   "description='Native Git source fixture'\nrequires-python='>=3.11'\n"
                   "dependencies=['vis-agent>=0.1.0']\n[tool.vis]\ncategory='tools'\n"))
        (spit (io/file project "extension.py") extension-source)
        (spit (io/file repository "unrelated.txt") "Outside selected project")
        (doseq [args [["init" "--quiet"] ["add" "."]
                      ["-c" "user.name=Test Author" "-c" "user.email=test@example.com" "commit"
                       "--quiet" "-m" "fixture"]]]
          (let [result (run-binary dir (into [git "-C" (str repository)] args) 60)]
            (expect (= 0 (:exit result)) (:output result))))
        (let [revision
              (str/trim (:output
                          (run-binary dir [git "-C" (str repository) "rev-parse" "HEAD"] 60)))

              shim
              (io/file shim-dir "git")]

          ;; Replace transport only; the SDK still executes real Git in its trusted native worker.
          (spit
            shim
            (str
              "#!/usr/bin/env python3\nimport os, sys\n"
              "args = ["
              (pr-str (str repository))
              " if a == "
              (pr-str url)
              " else 'protocol.file.allow=always' if a == 'protocol.file.allow=never' else a for a in sys.argv[1:]]\n"
              "os.execv("
              (pr-str git)
              ", ["
              (pr-str git)
              ", *args])\n"))
          (.setExecutable shim true)
          (with-redefs [native-environment (constantly (assoc environment
                                                         "PATH" (str shim-dir
                                                                     File/pathSeparator
                                                                     (System/getenv "PATH"))))]
            (let [installed (run-binary dir
                                        [(.getAbsolutePath bin)
                                         (str "-Duser.home=" (.getAbsolutePath dir)) "extension"
                                         "install" url "--subdirectory" "plugins/greeting"
                                         "--revision" revision "--project" "--trust"]
                                        120)]
              (expect (= 0 (:exit installed)) (:output installed))
              (expect (str/includes? (:output installed) "(github)"))
              (expect (not (.exists (io/file dir "imported"))))
              ;; Git installs use a copied managed snapshot, not a link to the source checkout.
              (expect (Files/isSymbolicLink (.toPath destination)))
              (expect (= "1.0.0" (str (Files/readSymbolicLink (.toPath destination)))))
              (let [^File snapshot (.getCanonicalFile destination)
                    receipt
                    (json/read-json (slurp (io/file snapshot "receipt.json")) :key-fn keyword)]

                (expect (= (.getCanonicalFile (io/file dir ".vis/extensions/native-github-example"))
                           (.getParentFile snapshot)))
                (expect (= "1.0.0" (.getName snapshot)))
                (expect (not (Files/isSymbolicLink (.toPath snapshot))))
                (expect (= {:name "native-github-example"
                            :version "1.0.0"
                            :repository_url url
                            :subdirectory "plugins/greeting"
                            :revision revision
                            :release_tag nil
                            :previous nil}
                           receipt)))
              (spit (io/file project "extension.py") "VALUE = 'changed after installation'\n")
              (expect (= extension-source (slurp (io/file destination "extension.py"))))
              (expect (not (.exists (io/file destination ".git"))))
              (expect (not (.exists (io/file destination "unrelated.txt"))))
              ;; The GitHub identity must survive install and registration without renaming the package.
              (let [listed (run-binary dir
                                       [(.getAbsolutePath bin)
                                        (str "-Duser.home=" (.getAbsolutePath dir)) "extension"
                                        "list"]
                                       180)]
                (expect (= 0 (:exit listed)) (:output listed))
                (expect (.exists (io/file dir "imported")))
                (expect (str/includes? (:output listed) "example-owner/extensions")
                        (:output listed))
                (expect (re-find #"example-owner/extensions[^\n]+│ example-owner\s+│"
                                 (:output listed))
                        (:output listed))
                (expect (str/includes? (:output listed) "native-github-example")
                        (:output listed))))))
        (finally (delete-tree! dir))))))

(defn- package-check-result
  [stdout]
  (let [line (last (filter #(str/starts-with? % "PACKAGE_CHECK ") (str/split-lines stdout)))]
    (when line (json/read-json (subs line (count "PACKAGE_CHECK ")) :key-fn keyword))))

(defdescribe
  native-package-worker-compatibility-test
  ;; Manual sync must prepare packages that perform real work through extension tools.
  ;; Trusted native calls run outside the model sandbox, without changing its policy.
  (it
    "imports project native wheels through uv and isolated JVM/native extension workers"
    (let [dir
          (temp-dir "vis-native-packages")

          project
          (doto (io/file dir "implementation") .mkdirs)

          source
          (doto (io/file project "src") .mkdirs)

          entries
          (doto (io/file dir ".vis/extensions") .mkdirs)

          bin
          (require-binary)

          library
          (python-library bin)

          cli
          [(.getAbsolutePath bin) (str "-Duser.home=" (.getAbsolutePath dir))]

          old-home
          (System/getProperty "user.home")

          store
          (ps/db-create-connection! :memory)

          index
          {"python" {"index_url" "https://pypi.org/simple"}}]

      (try
        (expect library)
        (runtime/use-library! (.getAbsolutePath library))
        (spit (io/file dir "vis.yml") "python:\n  index_url: https://pypi.org/simple\n")
        (spit (io/file project "pyproject.toml")
              (str "[project]\nname = 'vis-native-package-check'\nversion = '1.0'\n"
                   "requires-python = '>=3.14,<3.15'\n"
                   "dependencies = ['numpy==2.5.3', 'scipy==1.18.1', "
                   "'pydantic==2.13.5', 'cryptography==49.0.0']\n"))
        (io/copy (io/file "test-native/com/blockether/vis/fixtures/package_checks.py")
                 (io/file source "package_checks.py"))
        (spit
          (io/file entries "packages.py")
          (str
            "# /// script\n# dependencies = []\n# [tool.vis]\n"
            "# project = '../../implementation'\n"
            "# source_paths = ['../../implementation/src']\n# ///\n"
            "import blockether.vis.extension as vis\n" "from package_checks import packages_check\n"
            "from dataclasses import dataclass\n"
            "@dataclass(frozen=True, slots=True)\nclass PackageStatus:\n    state: str\n"
            "def packages_status():\n    \"Return a typed extension result.\"\n    return PackageStatus('ready')\n"
            "vis.register_extension(vis.Extension(name='package-check', alias='packages', "
            "description='Native package compatibility', "
            "symbols=[vis.Symbol(packages_check), vis.Symbol(packages_status)]))\n"))
        (let [locked (run-binary dir
                                 (into cli
                                       ["python" "uv" "lock" "--project" (str project) "--python"
                                        (com.blockether.vispython.Locations/pythonExecutable
                                          (str (io/file (.getParentFile library) "python")))
                                        "--no-python-downloads" "--default-index"
                                        "https://pypi.org/simple"])
                                 120)]
          (expect (= 0 (:exit locked)) (:output locked)))
        (let [lock-before
              (slurp (io/file project "uv.lock"))

              synced
              (run-binary dir
                          (into cli
                                ["python" "uv" "sync" "--project" (str project) "--locked"
                                 "--python"
                                 (com.blockether.vispython.Locations/pythonExecutable
                                   (str (io/file (.getParentFile library) "python")))
                                 "--default-index" "https://pypi.org/simple"])
                          240)]

          (expect (= 0 (:exit synced)) (:output synced))
          (System/setProperty "user.home" (.getAbsolutePath dir))
          (binding [extension/*current-environment* {:db-info store}]
            (with-redefs [config/load-config-raw (constantly index)
                          python-runtime/ensure-project! (fn [& _]
                                                           (throw (ex-info "Unexpected sync" {})))
                          python-runtime/pip-install! (fn [& _]
                                                        (throw (ex-info "Unexpected pip" {})))]

              (let [packages (python-runtime/prepared-project project)
                    project-package? (fn [{:keys [status path]}]
                                       (and (= "ok" status)
                                            path
                                            (.startsWith (.toPath (io/file path))
                                                         (.toPath ^File packages))))
                    baseline
                    (run-binary
                      dir
                      (into cli
                            ["python" "uv" "run" "--project" (str project) "--no-sync" "python" "-c"
                             (str "import sys, json\nsys.path[:0] = ["
                                  (pr-str (str source))
                                  "]\n"
                                  "from package_checks import packages_check\n"
                                  "print('PACKAGE_CHECK ' + json.dumps(packages_check()))")])
                      120)
                    report (package-check-result (:output baseline))]

                (expect (not= (.getCanonicalFile (io/file (runtime/packages-dir))) packages))
                (expect (= 0 (:exit baseline)) (:output baseline))
                (expect (= #{:numpy :scipy :pydantic :cryptography} (set (keys (:packages report))))
                        (:output baseline))
                (expect (every? project-package? (vals (:packages report))) (pr-str report))
                (expect (= {:loaded 1 :failed 0 :changed? true}
                           (pyx/reload-python-extensions! {:dirs [(str entries)]})))
                (let [jvm-worker @#'worker/child-argv
                      ext (some #(when (= "package-check" (:ext/name %)) %)
                                (extension/registered-extensions))]

                  (expect ext)
                  ;; Exercise both processes with the JVM and packaged native entrypoints.
                  (doseq [native? [false true]]
                    (with-redefs-fn
                      {#'worker/child-argv
                       (fn [lib socket guest-dir run-directory runtime-roots]
                         (if native?
                           (let [executable (runtime/resolve-worker {:path lib})]
                             (expect executable "The runtime archive must carry its worker")
                             (into [executable (str "-Duser.home=" dir) socket "--resolved-sources"]
                                   (conj runtime-roots guest-dir)))
                           ;; Exercise the JVM even when the archive has a worker.
                           (with-redefs [runtime/resolve-worker (constantly nil)]
                             (jvm-worker lib socket guest-dir run-directory runtime-roots))))}
                      (fn []
                        (let [made (ep/create-python-context
                                     {}
                                     (constantly [(.getCanonicalPath dir)])
                                     {:worker? true :jail-enabled? true :enabled? false}
                                     nil)
                              ctx (:python-context made)
                              env {:python-context ctx
                                   :session-id "native-package-check"
                                   :extensions (atom [ext])
                                   :active-extensions (atom [])
                                   :db-info store}]

                          (try
                            (loop-env/sync-active-extension-symbols! env [ext])
                            (let
                              [answer
                               (ep/run-python-block
                                 ctx
                                 (str
                                   "import json, dataclasses, importlib.util\n"
                                   "assert all(importlib.util.find_spec(name) is None "
                                   "for name in ('numpy', 'scipy', 'pydantic', 'cryptography'))\n"
                                   "status = await packages_status()\n"
                                   "assert type(status).__name__ == 'PackageStatus' and status.state == 'ready'\n"
                                   "assert dataclasses.is_dataclass(status)\n"
                                   "report = await packages_check()\n"
                                   "print('PACKAGE_CHECK ' + json.dumps(report))"))
                               report (package-check-result (:stdout answer))
                               statuses (into {}
                                              (map (fn [[name result]]
                                                     [name (:status result)]))
                                              (:packages report))
                               ^Process process (:process (get @@#'worker/workers ctx))]

                              (expect (nil? (:error answer)) (pr-str answer))
                              (expect (not= (.pid process) (:pid report)))
                              (let [trusted-context (:context (get @@#'pyx/session-contexts
                                                                   [ctx "package-check"]))
                                    trusted-worker (#'pyx/worker-for trusted-context)]

                                (expect (= (.pid ^Process
                                                 (:process (get @@#'worker/workers trusted-worker)))
                                           (:pid report))))
                              (expect (= {:numpy "ok" :scipy "ok" :pydantic "ok" :cryptography "ok"}
                                         statuses)
                                      (pr-str report))
                              (expect (every? project-package? (vals (:packages report))))
                              (println "PACKAGE_COMPATIBILITY"
                                       (if native? :native :jvm)
                                       (pr-str statuses)))
                            (finally (ep/dispose-python-context! ctx)))))))))))
          (expect (= lock-before (slurp (io/file project "uv.lock"))))
          (expect (.isDirectory (io/file project ".venv")))
          (expect (.isDirectory (io/file dir ".vis/python/packages")))
          (expect (not-any? #(= ".vis-packages" (.getName ^File %)) (file-seq dir))))
        (catch Exception error
          (doseq [log
                  (distinct (cons (:log (ex-data error)) (map :log (vals @@#'worker/workers))))

                  :when (and log (.isFile (io/file log)))]

            (let [text (slurp log)]
              (println "PACKAGE_WORKER_STARTUP" (subs text 0 (min 8000 (count text))))))
          (throw error))
        (finally (pyx/reload-python-extensions! {:dirs []})
                 (ps/db-dispose-connection! store)
                 (System/setProperty "user.home" old-home)
                 (runtime/use-library! nil)
                 (delete-tree! dir))))))

(defdescribe
  native-editable-source-test
  ;; #175: the shipped CLI must install source links, not a copied wheel.
  (it
    "imports edited source after one locked sync in the native image"
    (let [dir
          (temp-dir "vis-native-editable")

          project
          (doto (io/file dir "project") .mkdirs)

          source
          (doto (io/file project "src") .mkdirs)

          module
          (io/file source "vis_editable_fixture.py")

          entries
          (doto (io/file dir ".vis/extensions") .mkdirs)

          bin
          (require-binary)

          python
          (com.blockether.vispython.Locations/pythonExecutable
            (str (io/file (.getParentFile (python-library bin)) "python")))

          run
          (fn [args]
            (run-binary dir
                        (into [(.getAbsolutePath bin) (str "-Duser.home=" (.getAbsolutePath dir))]
                              args)
                        120))]

      (try
        (spit
          (io/file project "pyproject.toml")
          "[project]\nname = 'vis-editable-fixture'\nversion = '0.0.1'\nrequires-python = '>=3.12'\n[build-system]\nrequires = []\nbuild-backend = 'backend'\nbackend-path = ['.']\n")
        (io/copy (io/file "test/com/blockether/vis/internal/python/fixtures/editable_backend.py")
                 (io/file project "backend.py"))
        (spit module "VALUE = 41\n")
        (spit (io/file entries "editable.py")
              (str "# /// script\n# [tool.vis]\n# project = '../../project'\n# ///\n"
                   "import blockether.vis.extension as vis\n"
                   "from vis_editable_fixture import VALUE\n"
                   "vis.register_extension(vis.Extension(name='native-editable-' + str(VALUE), "
                   "description='Native editable source fixture'))\n"))
        (let [locked (run-binary dir
                                 ["uv" "lock" "--project" (str project) "--offline" "--python"
                                  python "--no-python-downloads"]
                                 60)]
          (expect (= 0 (:exit locked)) (:output locked)))
        (let [synced (run ["python" "uv" "sync" "--project" (str project) "--locked" "--offline"
                           "--python" python])]
          (expect (= 0 (:exit synced)) (:output synced)))
        (let [pth (some #(when (= "fixture.pth" (.getName ^File %)) %)
                        (file-seq (io/file project ".venv")))]
          (expect (some? pth))
          (expect (= (.getCanonicalPath source) (str/trim (slurp pth))))
          (expect (not (.exists (io/file (.getParentFile ^File pth) "vis_editable_fixture.py")))))
        (let [lock-before
              (slurp (io/file project "uv.lock"))

              mtime
              (.lastModified module)]

          ;; Upstream CPython caches bytecode by second-resolution mtime.
          (doseq [[value modified] [[41 mtime] [42 (+ mtime 2000)]]]
            (spit module (str "VALUE = " value "\n"))
            (.setLastModified module modified)
            (let
              [imported
               (run
                 ["python" "uv" "run" "--project" (str project) "--no-sync" "python" "-c"
                  "import vis_editable_fixture as fixture; print(fixture.VALUE, fixture.__file__)"])
               registered (run ["extension" "list"])]

              (expect (= 0 (:exit imported)) (:output imported))
              (expect (str/includes? (:output imported) (str value " " (.getCanonicalPath module)))
                      (:output imported))
              (expect (= 0 (:exit registered)) (:output registered))
              (expect (str/includes? (:output registered) (str "native-editable-" value))
                      (:output registered))))
          (expect (= lock-before (slurp (io/file project "uv.lock"))))
          (expect (.isDirectory (io/file project ".venv"))))
        (finally (delete-tree! dir))))))

(defdescribe
  native-documented-editable-package-test
  ;; #175: execute the guide's actual files and commands with a real PEP 660 backend.
  (it
    "runs the documented setuptools package, source edits and dependency updates"
    (let [dir
          (temp-dir "vis-documented-package")

          bin
          (require-binary)

          guide
          (slurp (io/resource "vis-docs/extension-development.md"))

          files
          (into {}
                (map (fn [[_ path source]]
                       [path (str source "\n")]))
                (re-seq #"(?s)```(?:toml|python)\n# ([^\n]+)\n(.*?)\n```" guide))

          commands
          (->> (re-seq #"(?ms)^([ \t]*)```bash\n(.*?)^\1```[ \t]*$" guide)
               (mapcat (fn [[_ _indent source]]
                         (str/split-lines source)))
               (map str/trim)
               ;; The shared-package workflow is separate from this project-environment example.
               (filter #(str/starts-with? % "vis-agent python uv "))
               vec)

          registration-command
          (second (re-find #"`(vis-agent extension list)`" guide))

          run
          (fn [command]
            (run-binary
              dir
              ["bash" "-c"
               (str/replace
                 command
                 #"(?m)^vis-agent "
                 (str (pr-str (.getAbsolutePath bin)) " " (pr-str (str "-Duser.home=" dir)) " "))]
              120))

          checked
          (fn [command]
            (let [result (run command)]
              (expect (= 0 (:exit result)) (:output result))
              (:output result)))

          project
          (io/file dir "einmal")

          module
          (io/file project "src/einmal/__init__.py")]

      (try
        (expect (= #{"einmal/pyproject.toml" "einmal/src/einmal/__init__.py"
                     "einmal/tests/test_status.py" ".vis/extensions/einmal_tools.py"}
                   (set (keys files))))
        (expect (= 4 (count commands)))
        (expect (= "vis-agent extension list" registration-command))
        (doseq [[path source] files]
          (let [file (io/file dir path)]
            (io/make-parents file)
            (spit file source)))
        (checked (commands 0))
        (let [lock-before (slurp (io/file project "uv.lock"))]
          (checked (commands 1))
          (let [imported (checked (commands 2))]
            (expect (str/includes? imported (str "ready " (.getCanonicalPath module))) imported))
          (let [registered (checked registration-command)]
            (expect (str/includes? registered "Package example.") registered))
          (expect (str/includes? (checked (commands 3)) "1 passed"))
          (let [mtime (.lastModified module)]
            (spit module (str/replace (slurp module) "ready" "fresh"))
            ;; The guide runs upstream CPython, whose bytecode cache uses whole seconds.
            (.setLastModified module (+ mtime 2000)))
          ;; Neither the install command nor lock generation is repeated for source edits.
          (let [imported (checked (commands 2))]
            (expect (str/includes? imported (str "fresh " (.getCanonicalPath module))) imported))
          (expect (str/includes? (checked registration-command) "Package example."))
          (let [test-file (io/file project "tests/test_status.py")]
            (spit test-file (str/replace (slurp test-file) "ready" "fresh")))
          (expect (str/includes? (checked (commands 3)) "1 passed"))
          (expect (= lock-before (slurp (io/file project "uv.lock")))))
        (let [metadata
              (io/file project "pyproject.toml")

              dependency
              (io/file dir "shared-tools")

              dep-module
              (io/file dependency "src/shared_tools/__init__.py")

              local-source
              (second (first (re-seq #"(?s)```toml\n(\[tool.uv.sources\]\n.*?)\n```" guide)))]

          (expect (some? local-source))
          (io/make-parents dep-module)
          (spit dep-module "VALUE = 41\n")
          (spit (io/file dependency "pyproject.toml")
                (str/replace (files "einmal/pyproject.toml")
                             "name = \"einmal\""
                             "name = \"shared-tools\""))
          (spit metadata
                (str (str/replace (slurp metadata)
                                  "dependencies = []"
                                  "dependencies = [\"idna==3.10\", \"shared-tools\"]")
                     "\n"
                     local-source
                     "\n"))
          (expect (not (str/includes? (checked registration-command) "Package example.")))
          (checked (commands 0))
          (checked (commands 1))
          (expect
            (str/includes?
              (checked
                "vis-agent python uv run --project ./einmal --no-sync python -c \"import idna; print(idna.__version__)\"")
              "3.10"))
          (expect (str/includes? (checked registration-command) "Package example."))
          (let
            [probe
             "vis-agent python uv run --project ./einmal --no-sync python -c \"import shared_tools; print(shared_tools.VALUE, shared_tools.__file__)\""

             mtime
             (.lastModified dep-module)]

            (expect (str/includes? (checked probe) (str "41 " (.getCanonicalPath dep-module))))
            (spit dep-module "VALUE = 42\n")
            (.setLastModified dep-module (+ mtime 2000))
            (expect (str/includes? (checked probe) (str "42 " (.getCanonicalPath dep-module))))))
        (expect (.isDirectory (io/file project ".venv")))
        (finally (delete-tree! dir))))))

(defdescribe
  native-python-interactive-input-test
  ;; Regression #229: the linked CLI must show prompts before accepting input.
  (it "reads delayed terminal input and EOF in both file and module modes"
      (let [dir (temp-dir "vis-native-python-tty-")]
        (try (let [result (run-binary
                            dir
                            ["python3"
                             (.getCanonicalPath
                               (io/file
                                 "test-native/com/blockether/vis/fixtures/python_cli_tty.py"))
                             (.getCanonicalPath (require-binary)) "python"]
                            240)]
               (expect (:finished? result) (:output result))
               (expect (= 0 (:exit result)) (:output result))
               (expect (str/includes? (:output result) "file: interactive input and EOF passed")
                       (:output result))
               (expect (str/includes? (:output result) "module: interactive input and EOF passed")
                       (:output result)))
             (finally (delete-tree! dir))))))

(defn- native-context-case!
  "Exercise token resources, independent input limits and a measured fold in the linked image."
  [model tokenizer & [limits]]
  (let
    [dir
     (temp-dir "vis-native-context-")

     calls
     (atom 0)

     original-stream
     stream-body

     original-whole
     whole-body

     steps
     [{:input 24000
       :code (str "assert session['utilization']['model_input_limit'] == 36000\n"
                  "print('お誕生日おめでとう Zażółć gęślą jaźń 👋 <|endoftext|> ' * 40)")}
      {:input 28000
       :code
       (str
         "fold_session('-t' + str(session['turn']) + '/i1', "
         "'Native context evidence: Unicode and special-token literals counted; input cap 36000.')")}
      {:input 5000
       :code (str "u = session['utilization']\n"
                  "assert u['model_input_limit'] == 36000, u\n"
                  "assert u['latest_measured_input_tokens'] == 5000, u\n"
                  "assert 'fold_count' not in u and 'fold_measurement' not in u, u\n"
                  "print('Native tokenizer and folding verified')")} {:input 5500}]

     respond
     (fn [stream? reply]
       (let [index
             (dec (long (swap! calls inc)))

             {:keys [input code]}
             (get steps index {:input 5500})

             body
             (if code
               (python-call-body stream? (str "native-context-" index) code)
               ((if stream? original-stream original-whole) reply))

             usage
             {"prompt_tokens" input "completion_tokens" 2 "total_tokens" (+ (long input) 2)}]

         (if stream?
           (str/replace body
                        "data: [DONE]\n\n"
                        (str "data: "
                             (json/write-json-str {"choices" [] "usage" usage})
                             "\n\ndata: [DONE]\n\n"))
           (json/write-json-str (assoc (json/read-json body) "usage" usage)))))]

    (try
      (with-redefs [stream-body
                    #(respond true %)

                    whole-body
                    #(respond false %)]

        (let [{:keys [server port asked]} (start-stub-provider! "Native context complete.")]
          (try
            (.mkdirs (io/file dir ".vis"))
            (spit (io/file dir ".vis/config.yml")
                  (json/write-json-str {:default_provider "stub-local"
                                        :default_model model
                                        :providers
                                        [{:id "stub-local"
                                          :base_url (str "http://127.0.0.1:" port "/v1")
                                          :compatibility "openai"
                                          :models [(cond-> (merge {:name model :is_tool_call true}
                                                                  (or limits
                                                                      {:context 50000
                                                                       :input_limit 36000
                                                                       :output_limit 4000}))
                                                     tokenizer
                                                     (assoc :tokenizer tokenizer))]}]}))
            (let [database (io/file dir "sessions")
                  {:keys [finished? exit output]}
                  (run-binary dir
                              [(.getAbsolutePath (require-binary))
                               (str "-Duser.home=" (.getAbsolutePath dir)) "--db"
                               (.getAbsolutePath database) "--raw"
                               "Verify Unicode tokenization and fold the completed evidence."]
                              240)]

              (expect finished? output)
              (expect (= 0 exit) output)
              (expect (= 4 @calls) output)
              (expect (str/includes? output "Native context complete.") output)
              (let [store (ps/db-create-connection! (.getAbsolutePath database))]
                (try
                  (let [sid (:id (first (ps/db-list-sessions store :all)))
                        forms (mapcat :forms
                                      (mapcat #(ps/db-list-session-turn-iterations store (:id %))
                                              (ps/db-list-session-turns store sid)))
                        usage (ps/db-session-usage-stats store sid)
                        health (:health usage)
                        requests (filter #(str/ends-with? (:path %) "/chat/completions") @asked)]

                    (expect (= 3 (count forms))
                            (pr-str {:forms (mapv #(select-keys % [:src :error]) forms)
                                     :paths (mapv :path @asked)}))
                    (expect (every? #(nil? (:error %)) forms) (pr-str forms))
                    (expect (= "Native tokenizer and folding verified\n" (:stdout (last forms)))
                            (pr-str (last forms)))
                    (expect (= 1 (:fold-count usage))
                            (pr-str (select-keys usage [:fold-count :health])))
                    (expect (= 28000 (get-in health [:fold-measurement "before_input_tokens"])))
                    (expect (= 5000 (get-in health [:fold-measurement "after_input_tokens"])))
                    (expect (= 23000 (get-in health [:fold-measurement "net_reduction_tokens"])))
                    (expect (= 36000 (:model-input-limit health)) (pr-str health))
                    (expect (= 5500 (:last-request-tokens health)) (pr-str health))
                    (expect (= 4 (count requests)))
                    (let [before (str (json/read-json (:body (second requests))))
                          after (str (json/read-json (:body (nth requests 2))))]

                      (expect (str/includes? before "お誕生日おめでとう"))
                      (expect (not (str/includes? after "お誕生日おめでとう")))
                      (expect (str/includes? after "Native context evidence"))))
                  (finally (ps/db-dispose-connection! store)))))
            (finally (.stop ^HttpServer server 0)))))
      (finally (delete-tree! dir)))))

(defdescribe native-tokenizers-and-context-folding-test
             (it "loads cl100k resources selected by model" (native-context-case! "gpt-4" nil))
             (it "loads o200k resources selected by model" (native-context-case! "gpt-4o" nil))
             (it "loads p50k resources selected by model"
                 (native-context-case! "text-davinci-003" nil))
             (it "loads r50k resources selected by model" (native-context-case! "davinci" nil))
             (it "honors declared cl100k for an unknown model"
                 (native-context-case! "native-future-model" "cl100k_base"))
             (it "honors declared o200k for an unknown model"
                 (native-context-case! "native-future-model" "o200k_base"))
             (it "keeps unsupported tokenizer declarations on the honest local fallback"
                 (native-context-case! "native-future-model" "provider-private-tokenizer"))
             (it "does not subtract output twice from an independent input-only cap"
                 (native-context-case! "native-future-model"
                                       "cl100k_base"
                                       {:input_limit 36000 :output_limit 4000}))
             (it "reserves output from the total window even when the input cap is larger"
                 (native-context-case! "native-future-model"
                                       "o200k_base"
                                       {:context 40000 :input_limit 48000 :output_limit 4000})))
