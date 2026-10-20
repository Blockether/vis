(ns com.blockether.vis.native-session-api-test
  "Phase 8 of #405 in the linked gateway image: extension calls into `vis.council`, the host
   refusals and a snapshot round trip across a gateway restart."
  (:require [charred.api :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.gateway.client :as gateway-client]
            [com.blockether.vis.native-binary-test :as binary]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [com.sun.net.httpserver HttpExchange HttpHandler HttpServer]
           [java.io File]
           [java.net InetSocketAddress ServerSocket URLEncoder]
           [java.nio.charset StandardCharsets]
           [java.nio.file Files]))

(def ^:private parent-code
  "E3 spawns one child twice with the same input; E7 spawns from a thread of the extension."
  (str "first = await e2e.spawn('child-' + 'e3', repeat=2)\n"
       "second = await e2e.spawn('child-' + 'e12')\n" "records = await e2e.wait()\n"
       "refused = await e2e.thread_spawn('child-' + 'e7')\n"
       "print('E3', len(set(first['ids'])), len(first['children']))\n"
       "print('E12', len(second['children']), sorted(r['status'] for r in records))\n"
       "print('E7', refused['refused'], refused['code'], len(refused['children']))\n"))

(def ^:private child-code
  "A child answers each Council request of its parent before its turn can end."
  (str "for item in session['council']['pending_replies']:\n"
       "    await council.publish('sub-answer', kind='informational', "
       "reply_to=item['entry_id'])\n" "print('child-replied')\n"))

(def ^:private probe-code
  "E8: a model block has no host door, so both session calls fail before any write."
  (str "import blockether.vis.extension as vis\n"
       "def probe(call):\n" "    try:\n"
       "        call()\n" "    except Exception as error:\n"
       "        return type(error).__name__\n" "    return 'allowed'\n"
       "print('E8', probe(lambda: vis.sessions.create()), "
       "probe(lambda: vis._host_layer().create_session()))\n"))

(defn- tool-result?
  [{:strs [role content]}]
  (or (= "tool" role)
      (and (= "user" role) (string? content) (boolean (re-find #"^# t\d+/i\d+\n" content)))))

(defn- reply
  "The scripted model. A tool result ends the turn; otherwise the last request picks the step."
  [body]
  (let [messages
        (get (json/read-json body) "messages")

        last-message
        (peek (vec messages))

        request
        (str (get last-message "content"))

        stream?
        (str/includes? (str/replace body " " "") "\"stream\":true")

        text
        (fn [answer]
          ((if stream? @#'binary/stream-body @#'binary/whole-body) answer))]

    (cond (tool-result? last-message) (text "block-done")
          (str/includes? request "e2e-follow-up") (text "follow-up-done")
          (re-find #"child-e\d+" request) (#'binary/python-tool-body child-code 3 stream?)
          (str/includes? request "e2e-parent") (#'binary/python-tool-body parent-code 1 stream?)
          (str/includes? request "e2e-probe") (#'binary/python-tool-body probe-code 2 stream?)
          :else (text "idle"))))

(defn- start-provider!
  "An OpenAI-dialect stub that answers from the request body and records each inference body."
  []
  (let [asked
        (atom [])

        server
        (HttpServer/create (InetSocketAddress. "127.0.0.1" 0) 0)]

    (.createContext server
                    "/"
                    (reify
                      HttpHandler
                        (handle [_ exchange]
                          (let [^HttpExchange exchange
                                exchange

                                body
                                (slurp (.getRequestBody exchange))

                                catalog?
                                (and (= "GET" (.getRequestMethod exchange))
                                     (str/ends-with? (.getPath (.getRequestURI exchange))
                                                     "/models"))

                                payload
                                (.getBytes ^String (if catalog? "{\"data\":[]}" (reply body))
                                           StandardCharsets/UTF_8)]

                            (when-not catalog? (swap! asked conj body))
                            (.add (.getResponseHeaders exchange)
                                  "Content-Type"
                                  (if (str/includes? (str/replace body " " "") "\"stream\":true")
                                    "text/event-stream"
                                    "application/json"))
                            (.sendResponseHeaders exchange 200 (alength payload))
                            (with-open [out (.getResponseBody exchange)]
                              (.write out payload))))))
    (.setExecutor server nil)
    (.start server)
    {:server server :asked asked :port (.getPort (.getAddress server))}))

(defn- request-json!
  [method path opts]
  (let [{:keys [status body]} (gateway-client/request! method path opts)]
    (when-not (<= 200 status 299)
      (throw (ex-info "Owned gateway request failed" {:status status :path path :body body})))
    (json/read-json body)))

(defn- await-value!
  "Call `f` every 100 ms until it returns a truthy value. Throw `message` after 240 s."
  [message f]
  (let [deadline (+ (System/nanoTime) 240000000000)]
    (loop []

      (or (f)
          (if (< (System/nanoTime) deadline)
            (do (Thread/sleep 100) (recur))
            (throw (ex-info message {})))))))

(defn- await-health!
  [^Process process]
  (await-value!
    "Owned native gateway did not become healthy"
    #(or (try (= 200 (:status (gateway-client/request! :get "/healthz" {:timeout-ms 1000})))
              (catch Exception _ false))
         (when-not (.isAlive process) (throw (ex-info "Owned native gateway exited" {}))))))

(defn- start-gateway!
  [^File bin ^File dir port]
  (let [^java.util.List argv
        [(.getAbsolutePath bin) (str "-Duser.home=" (.getAbsolutePath dir))
         (str "-Djava.io.tmpdir=" (.getAbsolutePath dir)) "gateway" "start" "--host" "127.0.0.1"
         "--port" (str port) "--db" (.getAbsolutePath (io/file dir "sessions"))]

        pb
        (doto (ProcessBuilder. argv)
          (.directory dir)
          (.redirectErrorStream true)
          (.redirectOutput (java.lang.ProcessBuilder$Redirect/appendTo (io/file dir
                                                                                "gateway.log"))))

        environment
        (.environment pb)]

    (.putAll environment (#'binary/native-environment))
    (doseq [key ["VIS_GATEWAY_URL" "VIS_GATEWAY_TOKEN" "VIS_GATEWAY_MANAGED"]]
      (.remove environment key))
    (.start pb)))

(defn- stop-gateway!
  [^Process process]
  (.destroy process)
  (when-not (.waitFor process 30 java.util.concurrent.TimeUnit/SECONDS)
    (#'binary/kill-tree! process)))

(defn- run-turn!
  "Submit `text` to `sid`, wait for a terminal status and return the turn record."
  [sid text]
  (let [turns
        (str "/v1/sessions/" sid "/turns")

        tid
        (get (request-json!
               :post
               turns
               {:body
                {:request text :provider "stub-local" :model "stub-model" :idempotency_key text}})
             "turn_id")]

    (await-value!
      (str "Owned native turn did not finish: " tid)
      #(let [record (request-json! :get (str turns "/" tid) {})] (when (contains?
                                                                         #{"completed" "failed"
                                                                           "cancelled" "suspended"}
                                                                         (get record "status"))
                                                                   record)))))

(defn- tool-output
  "The tool results that the stub received, joined, so a marker line can be matched."
  [asked]
  (str/join "\n"
            (for [body
                  @asked

                  message
                  (get (json/read-json body) "messages")

                  :when (tool-result? message)]

              (str (get message "content")))))

(defn- session-ids
  []
  (set (map #(get % "id") (get (request-json! :get "/v1/sessions" {}) "sessions"))))

(defn- children
  [sid]
  (set (keep #(get % "session_id") (request-json! :get (str "/v1/sessions/" sid "/agents") {}))))

(defn- install-fixtures!
  [^File dir]
  (let [extensions (doto (io/file dir ".vis" "extensions") .mkdirs)]
    (doseq [name ["e2e_sessions.py" "e2e_other.py"]]
      (io/copy (io/file "test-native/com/blockether/vis/fixtures" name)
               (io/file extensions name)))))

(defdescribe
  native-session-api-test
  (it
    "keeps spawn idempotent, refuses foreign callers and restores a snapshot after a restart"
    (let [^File dir
          (#'binary/temp-dir "vis-native-session-api-")

          ^File bin
          (#'binary/require-binary)

          {:keys [server asked port]}
          (start-provider!)

          gateway-port
          (with-open [socket (ServerSocket. 0)]
            (.getLocalPort socket))

          process
          (atom nil)

          client-id
          (atom nil)]

      (try
        (#'binary/overlay! dir port)
        (spit (io/file dir ".vis" "config.yml")
              "titling:\n  mode: disabled\ntoggles:\n  subagents: true\n"
              :append
              true)
        (install-fixtures! dir)
        (reset! process (start-gateway! bin dir gateway-port))
        (with-redefs-fn {#'gateway-client/ensure-gateway!
                         (constantly {:host "127.0.0.1" :port gateway-port :remote? true})
                         #'gateway-client/client-id client-id
                         #'gateway-client/release-hook-installed? (atom true)}
          (fn []
            (await-health! @process)
            (let [sid
                  (get (request-json! :post
                                      "/v1/sessions"
                                      {:body {:channel "api" :root (.getAbsolutePath dir)}})
                       "id")

                  parent
                  (run-turn! sid "e2e-parent turn; remember the word ZEBRA-4127")

                  output
                  (tool-output asked)

                  log
                  (str output "\n--- gateway.log ---\n" (slurp (io/file dir "gateway.log")))]

              (expect (= "completed" (get parent "status")) log)
              ;; E3: two spawns with one input in one tool call give one child.
              (expect (str/includes? output "E3 1 1") log)
              ;; E12 needs two finished children; E7 adds none.
              (expect (str/includes? output "E12 2 ['completed', 'completed']") output)
              (expect (re-find #"E7 True \S+ 2" output) output)
              (expect (= 2 (count (children sid))))
              ;; E8: a model block reaches neither the facade nor the host layer.
              (let [before
                    (session-ids)

                    probe
                    (run-turn! sid "e2e-probe the host door")

                    probed
                    (tool-output asked)

                    line
                    (re-find #"E8 \S+ \S+" probed)]

                (expect (= "completed" (get probe "status")) probed)
                (expect (some? line) probed)
                (expect (not (str/includes? line "allowed")) line)
                (expect (= before (session-ids))))
              ;; E12: export, restart the gateway on the same database, restore and continue.
              (let [{:keys [status body]}
                    (gateway-client/request! :get
                                             (str "/v1/sessions/" sid "/snapshot")
                                             {:as :bytes :timeout-ms 60000})

                    snapshot
                    (io/file dir "parent.vissnap")

                    original
                    (conj (children sid) sid)]

                (expect (= 200 status) (str (String. ^bytes body "UTF-8") "\n" (slurp (io/file dir "gateway.log"))))
                (Files/write (.toPath snapshot) ^bytes body (make-array java.nio.file.OpenOption 0))
                (stop-gateway! @process)
                (reset! client-id nil)
                (reset! process (start-gateway! bin dir gateway-port))
                (await-health! @process)
                (let [query
                      (str "?root=" (URLEncoder/encode (.getAbsolutePath dir) "UTF-8"))

                      restored
                      (get (request-json! :post
                                          (str "/v1/sessions/actions/import" query)
                                          {:body (Files/readAllBytes (.toPath snapshot))
                                           :raw-body? true
                                           :headers {"content-type" "application/octet-stream"}})
                           "id")

                      restored-children
                      (children restored)

                      asked-before
                      (count @asked)

                      follow-up
                      (run-turn! restored "e2e-follow-up: which word did I ask you to remember?")

                      follow-up-body
                      (first (filter #(str/includes? % "e2e-follow-up")
                                     (drop asked-before @asked)))]

                  (expect (= 2 (count restored-children)))
                  (expect (empty? (filter original (conj restored-children restored))))
                  (expect (= "completed" (get follow-up "status")))
                  (expect (some-> follow-up-body
                                  (str/includes? "ZEBRA-4127"))
                          "The restored session must send the earlier context to the model"))))))
        (finally (when-let [p @process]
                   (stop-gateway! p))
                 (.stop ^HttpServer server 0)
                 (#'binary/delete-tree! dir))))))
