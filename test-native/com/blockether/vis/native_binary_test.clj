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
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]
            [charred.api :as json]
            [com.blockether.vis-python-runtime :as runtime]
            [com.blockether.vis.internal.config.core :as config]
            [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.internal.loop :as lp]
            [com.blockether.vis.internal.persistance.core :as ps]
            [com.blockether.vis.internal.python.env :as ep]
            [com.blockether.vis.internal.python.extensions :as pyx]
            [com.blockether.vis.internal.python.runtime :as python-runtime]
            [com.blockether.vis.internal.python.worker :as worker]
            [lazytest.core :refer [defdescribe expect it]])
  (:import (com.sun.net.httpserver HttpExchange HttpHandler HttpServer)
           (java.io File)
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
                 (let [dir
                       (temp-dir "vis-native-version")

                       {:keys [exit output]}
                       (run-binary dir [(.getAbsolutePath (require-binary)) "--version"] 60)]

                   (try (expect (= 0 exit) output)
                        (expect (re-find #"(?m)^vis-agent\s+\S+" output) output)
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
               (let [dir
                     (temp-dir "vis-native-agent")

                     {:keys [server asked port]}
                     (start-stub-provider! "hello world")]

                 (try (overlay! dir port)
                      (let [{:keys [exit output]}
                            (run-binary dir
                                        [(.getAbsolutePath (require-binary)) "--db" ":memory"
                                         "--raw" "Reply with exactly: hello world"]
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

;; ── voice: the two directions, in the linked image ───────────────────────────

(defn- plain-words
  "Lowercase words only. A transcript differs from what was spoken by punctuation
   and casing long before it differs by a word."
  [s]
  (->> (str/split (str/lower-case (str s)) #"[^a-z0-9]+")
       (remove str/blank?)
       (str/join " ")))

(defn- heard-most-words?
  "ASR round-trip proof tolerant of one acoustic substitution."
  [output sentence]
  (let [words
        #(set (str/split (plain-words %) #" "))

        expected
        (words sentence)

        heard
        (words output)]

    (<= (count (set/difference expected heard)) 1)))

(defn- wav-facts
  "What a WAV header claims, read by hand: `nil` when the file is not one."
  [^File f]
  (when (and (.isFile f) (> (.length f) 44))
    (let [header (byte-array 44)]
      (with-open [in (io/input-stream f)]
        (.read in header))
      (let [tag (fn [from]
                  (String. header from 4 StandardCharsets/US_ASCII))
            u32 (fn [from]
                  (reduce (fn [acc i]
                            (+ acc
                               (bit-shift-left (bit-and (long (aget header (+ from i))) 0xff)
                                               (* 8 i))))
                          0
                          (range 4)))]

        (when (and (= "RIFF" (tag 0)) (= "WAVE" (tag 8)))
          {:sample-rate (u32 24) :bytes (.length f)})))))

(def ^:private spoken-sentence
  "Plain words on purpose: this sentence is spoken by one engine and read back by
   another, and a rare word would test the vocabulary rather than the image."
  "Local speech now runs entirely on this machine, with no account and no network.")

(defdescribe
  native-binary-speaks-and-listens-test
  ;; sherpa-onnx reaches ONNX Runtime through JNI, and the model files are found
  ;; through resources the image has to carry: every one of those is a
  ;; native-image failure that compiles cleanly and only appears when audio is
  ;; actually generated. The JVM suite cannot see any of it.
  ;;
  ;; Regression, user report: with the models already installed, voice failed
  ;; anyway and the only cure anyone found was restarting Vis. A round trip
  ;; through the SHIPPED binary is the check that would have caught it.
  ;;
  ;; A machine without the models downloads them here (~63 MB for the voice,
  ;; ~487 MB for ASR) - deliberately, because fetching, verifying and unpacking
  ;; an archive is native code too, and no other test runs it inside the image.
  (it
    "speaks a sentence and reads its own recording back"
    (let [dir
          (temp-dir "vis-native-voice")

          wav
          (io/file dir "spoken.wav")

          bin
          (.getAbsolutePath (require-binary))

          said
          (run-binary dir [bin "speech" "say" spoken-sentence "--out" (.getAbsolutePath wav)] 900)]

      (try
        (expect (= 0 (:exit said)) (:output said))
        (expect
          (nil?
            (re-find
              #"ClassNotFoundException|NoClassDefFoundError|UnsatisfiedLinkError|NoSuchMethodError"
              (:output said)))
          (:output said))
        (let [facts (wav-facts wav)]
          (expect facts (str "no WAV at " (.getAbsolutePath wav) ":\n" (:output said)))
          (expect (<= 8000 (long (:sample-rate facts)) 48000)
                  (str "implausible sample rate: " facts))
          (expect (> (long (:bytes facts)) 20000)
                  (str "the binary wrote a WAV with nothing in it: " facts)))
        (let [heard (run-binary dir [bin "speech" "transcribe" (.getAbsolutePath wav)] 900)]
          (expect (= 0 (:exit heard)) (:output heard))
          (expect (heard-most-words? (:output heard) spoken-sentence)
                  (str "the binary did not hear what it had just said:\n" (:output heard))))
        (finally (delete-tree! dir)))))
  (it "speaks with the pocket-tts export Vis publishes itself"
      ;; The Piper path above is sherpa's VITS engine; pocket-tts is OUR ONNX
      ;; export driven through a different config class, with a reference clip
      ;; read from the installed bundle. Only one of the two proves the other.
      (let [dir
            (temp-dir "vis-native-pocket")

            wav
            (io/file dir "pocket.wav")

            said
            (run-binary dir
                        [(.getAbsolutePath (require-binary)) "speech" "say"
                         "The bundle we ship is the ONNX export itself." "--pocket-tts" "--out"
                         (.getAbsolutePath wav)]
                        900)]

        (try (expect (= 0 (:exit said)) (:output said))
             (let [facts (wav-facts wav)]
               (expect facts (str "no WAV at " (.getAbsolutePath wav) ":\n" (:output said)))
               (expect (= 24000 (long (:sample-rate facts)))
                       (str "pocket-tts speaks at 24 kHz; got " facts)))
             (finally (delete-tree! dir)))))
  (it "lists the voices this machine can speak in without loading a model"
      (let [dir
            (temp-dir "vis-native-voices")

            {:keys [exit output]}
            (run-binary dir [(.getAbsolutePath (require-binary)) "speech" "voices"] 120)]

        (try (expect (= 0 exit) output)
             (expect (str/includes? output "piper") output)
             (expect (str/includes? output "pocket-tts") output)
             (finally (delete-tree! dir))))))

;; ── the embedded CPython, inside the linked image ────────────────────────────

(defn- run-python
  "`vis-agent python -c CODE` against the private binary under the release wrapper's environment."
  [^File dir ^File bin code]
  (let [library (python-library bin)]
    (expect library
            (str "No CPython sidecar beside " (.getAbsolutePath bin)
                 " — `clojure -T:build native` stages vis-agent-python/ there. " build-it))
    (run-binary dir [(.getAbsolutePath bin) "python" "-c" code] 300)))

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

          bin
          (require-binary)]

      (try (with-open [out (io/output-stream (io/file dir wheel-name))]
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
                      "vis.register(vis.Extension(name='native-uv-value-' + str(answer()), "
                      "description='Native locked project fixture'))\n"))
           (let [locked (run-binary dir
                                    ["uv" "lock" "--project" (str project) "--offline" "--python"
                                     (com.blockether.vispython.Locations/pythonExecutable
                                       (str (io/file (.getParentFile (python-library bin))
                                                     "python"))) "--no-python-downloads"]
                                    60)]
             (expect (= 0 (:exit locked)) (:output locked)))
           (let [synced (run-binary dir
                                    [(.getAbsolutePath bin)
                                     (str "-Duser.home=" (.getAbsolutePath dir)) "python" "uv"
                                     "sync" "--project" (str project) "--locked" "--offline"]
                                    120)]
             (expect (= 0 (:exit synced)) (:output synced)))
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
             (expect (not (.exists (io/file project ".venv")))))
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
          (io/file dir ".vis/extensions/native-center-example")

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
             (expect (not (.exists destination))))
           (let [installed (run-binary dir (conj args "--trust") 60)]
             (expect (= 0 (:exit installed)) (:output installed))
             (expect (Files/isSymbolicLink (.toPath destination)))
             (expect (= (.getCanonicalFile project) (.getCanonicalFile destination)))
             (expect (not (.exists (io/file project "uv.lock")))))
           (finally (delete-tree! dir))))))

(defdescribe
  native-installs-github-project-test
  (it
    "fetches a pinned Git revision and installs only its selected project through the binary"
    (let [bin
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

          url
          "https://github.com/example/extensions"

          environment
          (native-environment)

          destination
          (io/file dir ".vis/extensions/native-github-example")]

      (try
        (spit (io/file project "pyproject.toml")
              (str "[project]\nname='native-github-example'\nversion='1.0.0'\n"
                   "description='Native Git source fixture'\nrequires-python='>=3.11'\n"
                   "dependencies=['vis-agent>=0.1.0']\n[tool.vis]\ncategory='tools'\n"))
        (spit (io/file project "extension.py")
              "raise RuntimeError('install must not execute source')\n")
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
              (expect (not (Files/isSymbolicLink (.toPath destination))))
              (expect (= (slurp (io/file project "extension.py"))
                         (slurp (io/file destination "extension.py"))))
              (expect (not (.exists (io/file destination ".git"))))
              (expect (not (.exists (io/file destination "unrelated.txt")))))))
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
    "imports the same native wheels from one directory in the CLI and both worker processes"
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
            "vis.register(vis.Extension(name='package-check', alias='packages', "
            "description='Native package compatibility', "
            "symbols=[vis.Symbol(packages_check), vis.Symbol(packages_status)]))\n"))
        (let [locked (run-binary dir
                                 ["uv" "lock" "--project" (str project) "--python"
                                  (com.blockether.vispython.Locations/pythonExecutable
                                    (str (io/file (.getParentFile library) "python")))
                                  "--no-python-downloads" "--default-index"
                                  "https://pypi.org/simple"]
                                 120)]
          (expect (= 0 (:exit locked)) (:output locked)))
        (let [lock-before
              (slurp (io/file project "uv.lock"))

              synced
              (run-binary dir
                          (into cli ["python" "uv" "sync" "--project" (str project) "--locked"])
                          240)]

          (expect (= 0 (:exit synced)) (:output synced))
          (System/setProperty "user.home" (.getAbsolutePath dir))
          (binding [extension/*current-environment* {:db-info store}]
            (with-redefs [config/load-config-raw (constantly index)
                          python-runtime/uv-sync! (fn [& _]
                                                    (throw (ex-info "Unexpected sync" {})))
                          python-runtime/pip-install! (fn [& _]
                                                        (throw (ex-info "Unexpected pip" {})))]

              (let [packages (python-runtime/prepared-project project)
                    shared-package? (fn [{:keys [status path]}]
                                      (and (= "ok" status)
                                           path
                                           (.startsWith (.toPath (io/file path))
                                                        (.toPath ^File packages))))
                    baseline (run-binary
                               dir
                               (into cli
                                     ["python" "-c"
                                      (str
                                        "import sys, json\nsys.path[:0] = ["
                                        (pr-str (str source))
                                        "]\n"
                                        "from package_checks import packages_check\n"
                                        "print('PACKAGE_CHECK ' + json.dumps(packages_check()))")])
                               120)
                    report (package-check-result (:output baseline))]

                (expect (= (.getCanonicalFile (io/file (runtime/packages-dir))) packages))
                (expect (= 0 (:exit baseline)) (:output baseline))
                (expect (= #{:numpy :scipy :pydantic :cryptography} (set (keys (:packages report))))
                        (:output baseline))
                (expect (every? shared-package? (vals (:packages report))) (pr-str report))
                (expect (= {:loaded 1 :failed 0 :changed? true}
                           (pyx/reload-python-extensions! {:dirs [(str entries)]})))
                (let [jvm-worker @#'worker/child-argv
                      ext (some #(when (= "package-check" (:ext/name %)) %)
                                (extension/registered-extensions))]

                  (expect ext)
                  ;; Exercise both processes with the JVM and packaged native entrypoints.
                  (doseq [native? [false true]]
                    (with-redefs-fn {#'worker/child-argv
                                     (fn [lib socket guest-dir]
                                       (if native?
                                         (let [executable (runtime/resolve-worker {:path lib})]
                                           (expect executable
                                                   "The runtime archive must carry its worker")
                                           [executable (str "-Duser.home=" dir) socket guest-dir])
                                         (jvm-worker lib socket guest-dir)))}
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
                            (lp/sync-active-extension-symbols! env [ext])
                            (let
                              [answer
                               (ep/run-python-block
                                 ctx
                                 (str
                                   "import json, dataclasses, importlib.util\n"
                                   "from pathlib import Path\n" "import numpy as np\n"
                                   "assert np.dot([2, 3], [4, 5]) == 23\n"
                                   "package_paths = {name: str(Path(importlib.util.find_spec(name).origin).resolve()) "
                                   "for name in ('numpy', 'scipy', 'pydantic', 'cryptography')}\n"
                                   "status = await packages_status()\n"
                                   "assert type(status).__name__ == 'PackageStatus' and status.state == 'ready'\n"
                                   "assert dataclasses.is_dataclass(status)\n"
                                   "report = await packages_check()\n"
                                   "assert all(report['packages'][name]['path'] == path "
                                   "for name, path in package_paths.items())\n"
                                   "print('PACKAGE_CHECK ' + json.dumps(report))"))
                               report (package-check-result (:stdout answer))
                               statuses (into {}
                                              (map (fn [[name result]]
                                                     [name (:status result)]))
                                              (:packages report))
                               ^Process process (:process (get @@#'worker/workers ctx))]

                              (expect (nil? (:error answer)) (pr-str answer))
                              (expect (not= (.pid process) (:pid report)))
                              (expect (= (.pid ^Process
                                               (:process (get @@#'worker/workers
                                                              (worker/extension-worker-key ctx))))
                                         (:pid report)))
                              (expect (= {:numpy "ok" :scipy "ok" :pydantic "ok" :cryptography "ok"}
                                         statuses)
                                      (pr-str report))
                              (expect (every? shared-package? (vals (:packages report))))
                              (println "PACKAGE_COMPATIBILITY"
                                       (if native? :native :jvm)
                                       (pr-str statuses)))
                            (finally (ep/dispose-python-context! ctx)))))))))))
          (expect (= lock-before (slurp (io/file project "uv.lock"))))
          (expect (not (.exists (io/file project ".venv"))))
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

          packages
          (io/file dir ".vis/python/packages")

          bin
          (require-binary)

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
              (str "import blockether.vis.extension as vis\n"
                   "from vis_editable_fixture import VALUE\n"
                   "vis.register(vis.Extension(name='native-editable-' + str(VALUE), "
                   "description='Native editable source fixture'))\n"))
        (let [locked (run-binary dir
                                 ["uv" "lock" "--project" (str project) "--offline" "--python"
                                  (com.blockether.vispython.Locations/pythonExecutable
                                    (str (io/file (.getParentFile (python-library bin)) "python")))
                                  "--no-python-downloads"]
                                 60)]
          (expect (= 0 (:exit locked)) (:output locked)))
        (let [synced (run ["python" "uv" "sync" "--project" (str project) "--locked" "--offline"])]
          (expect (= 0 (:exit synced)) (:output synced)))
        (expect (.isFile (io/file packages "fixture.pth")))
        (expect (not (.exists (io/file packages "vis_editable_fixture.py"))))
        (let [lock-before
              (slurp (io/file project "uv.lock"))

              mtime
              (.lastModified module)]

          (doseq [value [41 42]]
            (spit module (str "VALUE = " value "\n"))
            (.setLastModified module mtime)
            (let
              [imported
               (run
                 ["python" "-c"
                  "import vis_editable_fixture as fixture; print(fixture.VALUE, fixture.__file__)"])
               registered (run ["extension" "list"])]

              (expect (= 0 (:exit imported)) (:output imported))
              (expect (str/includes? (:output imported) (str value " " (.getCanonicalPath module)))
                      (:output imported))
              (expect (= 0 (:exit registered)) (:output registered))
              (expect (str/includes? (:output registered) (str "native-editable-" value))
                      (:output registered))))
          (expect (= lock-before (slurp (io/file project "uv.lock"))))
          (expect (not (.exists (io/file project ".venv")))))
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
          (-> (slurp (io/resource "vis-docs/extending.md"))
              (str/split #"### uv projects\n" 2)
              second
              (str/split #"\n## " 2)
              first)

          files
          (into {}
                (map (fn [[_ path source]]
                       [path (str source "\n")]))
                (re-seq #"(?s)```(?:toml|python)\n# ([^\n]+)\n(.*?)\n```" guide))

          commands
          (vec (mapcat (comp str/split-lines second)
                       (take 2 (re-seq #"(?s)```bash\n(.*?)\n```" guide))))

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
        (expect (= 6 (count commands)))
        (doseq [[path source] files]
          (let [file (io/file dir path)]
            (io/make-parents file)
            (spit file source)))
        (checked (commands 0))
        (let [lock-before (slurp (io/file project "uv.lock"))]
          (checked (commands 1))
          (let [imported (checked (commands 2))]
            (expect (str/includes? imported (str "ready " (.getCanonicalPath module))) imported))
          (let [registered (checked (commands 3))]
            (expect (str/includes? registered "Package example.") registered))
          (checked (commands 4))
          (expect (str/includes? (checked (commands 5)) "1 passed"))
          (let [mtime (.lastModified module)]
            (spit module (str/replace (slurp module) "ready" "fresh"))
            (.setLastModified module mtime))
          ;; Neither the install command nor lock generation is repeated for source edits.
          (let [imported (checked (commands 2))]
            (expect (str/includes? imported (str "fresh " (.getCanonicalPath module))) imported))
          (expect (str/includes? (checked (commands 3)) "Package example."))
          (let [test-file (io/file project "tests/test_status.py")]
            (spit test-file (str/replace (slurp test-file) "ready" "fresh")))
          (expect (str/includes? (checked (commands 5)) "1 passed"))
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
          (expect (not (str/includes? (checked (commands 3)) "Package example.")))
          (checked (commands 0))
          (checked (commands 1))
          (expect (str/includes? (checked
                                   "vis-agent python -c \"import idna; print(idna.__version__)\"")
                                 "3.10"))
          (expect (str/includes? (checked (commands 3)) "Package example."))
          (let
            [probe
             "vis-agent python -c \"import shared_tools; print(shared_tools.VALUE, shared_tools.__file__)\""

             mtime
             (.lastModified dep-module)]

            (expect (str/includes? (checked probe) (str "41 " (.getCanonicalPath dep-module))))
            (spit dep-module "VALUE = 42\n")
            (.setLastModified dep-module mtime)
            (expect (str/includes? (checked probe) (str "42 " (.getCanonicalPath dep-module))))))
        (expect (not (.exists (io/file project ".venv"))))
        (finally (delete-tree! dir))))))
