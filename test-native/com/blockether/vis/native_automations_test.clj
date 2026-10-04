(ns com.blockether.vis.native-automations-test
  "Schedules, webhooks and signed callbacks in the linked gateway image."
  (:require [babashka.http-client :as http]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.internal.gateway.client :as gateway-client]
            [com.blockether.vis.native-binary-test :as binary]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [com.sun.net.httpserver HttpExchange HttpHandler HttpServer]
           [java.io File]
           [java.net InetSocketAddress ServerSocket]
           [java.nio.charset StandardCharsets]
           [java.time Instant ZoneId ZonedDateTime]
           [java.util Base64 HexFormat UUID]
           [javax.crypto Mac]
           [javax.crypto.spec SecretKeySpec]))

(set! *warn-on-reflection* true)

(def ^:private answer
  "The only answer of the stub model. A run with this answer went through the stub."
  "Native automation answer.")

(def ^:private model
  "The pin that sends each automation turn to the stub model."
  {"provider" "stub-local" "model" "stub-model"})

(def ^:private terminal #{"completed" "failed" "cancelled" "skipped" "unknown"})

(defn- log-tail
  [^File file]
  (if (.isFile file)
    (str/join "\n" (take-last 40 (str/split-lines (slurp file))))
    (str "No log at " (.getPath file))))

(defn- eventually
  "The first truthy value of `f` within `ms` milliseconds, else nil."
  [ms f]
  (let [deadline (+ (System/currentTimeMillis) (long ms))]
    (loop []

      (or (f) (when (< (System/currentTimeMillis) deadline) (Thread/sleep 100) (recur))))))

(defn- ok!
  "One call to the owned gateway with its token. Answers the parsed body of a 2xx response."
  [method path body]
  (let [{:keys [status] :as response} (gateway-client/request! method
                                                               path
                                                               (cond-> {:timeout-ms 10000}
                                                                 body
                                                                 (assoc :body body)))]
    (when-not (and status (<= 200 status 299))
      (throw (ex-info (str "The owned gateway refused " (name method) " " path)
                      (select-keys response [:status :body]))))
    (when (seq (:body response)) (wire/parse-json (:body response)))))

(defn- post!
  "POST `body` to `url` as a webhook sender does: without the gateway token."
  [url ^String body headers]
  (let [response (http/post url {:headers headers :body body :throw false :timeout 15000})]
    {:status (:status response)
     :body (when-not (str/blank? (:body response)) (wire/parse-json (:body response)))}))

(defn- utf8 ^bytes [^String text] (.getBytes text StandardCharsets/UTF_8))

(defn- hmac
  ^bytes [^bytes key ^bytes data]
  (let [mac (Mac/getInstance "HmacSHA256")]
    (.init mac (SecretKeySpec. key "HmacSHA256"))
    (.doFinal mac data)))

(defn- github-signature
  [^String secret ^String body]
  (str "sha256=" (.formatHex (HexFormat/of) (hmac (utf8 secret) (utf8 body)))))

(defn- standard-signature
  "The Standard Webhooks signature of one message. The suite computes it, not the engine."
  [^String secret ^String message-id ^String timestamp ^String body]
  (str "v1,"
       (.encodeToString (Base64/getEncoder)
                        (hmac (.decode (Base64/getDecoder) (subs secret (count "whsec_")))
                              (utf8 (str message-id "." timestamp "." body))))))

(defn- start-receiver!
  "A callback receiver on 127.0.0.1. It records each request and answers 204."
  []
  (let [requests
        (atom [])

        server
        (HttpServer/create (InetSocketAddress. "127.0.0.1" 0) 0)]

    (.createContext server
                    "/"
                    (reify
                      HttpHandler
                        (handle [_ exchange]
                          (let [^HttpExchange exchange exchange]
                            (swap! requests conj
                              {:headers (into {}
                                              (map (fn [[k v]]
                                                     [(str/lower-case (str k)) (first v)]))
                                              (.getRequestHeaders exchange))
                               :body (slurp (.getRequestBody exchange) :encoding "UTF-8")})
                            (.sendResponseHeaders exchange 204 -1)
                            (.close exchange)))))
    (.setExecutor server nil)
    (.start server)
    {:server server
     :requests requests
     :url (str "http://127.0.0.1:" (.getPort (.getAddress server)) "/callback")}))

(defn- start-gateway!
  "Start the gateway image in `dir`. Its relay comes from `~/.vis/relay.edn` only."
  ^Process [^File dir port]
  (let [pb
        (doto (ProcessBuilder. ^"[Ljava.lang.String;"
                               (into-array String
                                           [(.getAbsolutePath ^File (#'binary/require-binary))
                                            (str "-Duser.home=" (.getAbsolutePath dir))
                                            (str "-Djava.io.tmpdir=" (.getAbsolutePath dir))
                                            "gateway" "start" "--host" "127.0.0.1" "--port"
                                            (str port) "--db"
                                            (.getAbsolutePath (io/file dir "sessions"))]))
          (.directory dir)
          (.redirectErrorStream true)
          (.redirectOutput (io/file dir "gateway.log")))

        environment
        (.environment pb)]

    (.putAll environment (#'binary/native-environment))
    (doseq [key ["VIS_GATEWAY_URL" "VIS_GATEWAY_TOKEN" "VIS_GATEWAY_MANAGED" "VIS_HOME"
                 "VIS_PUSH_RELAY_URL"]]
      (.remove environment key))
    (.start pb)))

(defn- await-health!
  [^Process process ^File dir]
  (let [deadline (+ (System/nanoTime) 60000000000)]
    (loop []

      (when-not (try (= 200 (:status (gateway-client/request! :get "/healthz" {:timeout-ms 1000})))
                     (catch Exception _ false))
        (if (and (.isAlive process) (< (System/nanoTime) deadline))
          (do (Thread/sleep 50) (recur))
          (throw (ex-info (str "The owned native gateway did not become healthy\n"
                               (log-tail (io/file dir "gateway.log")))
                          {})))))))

(defn- with-gateway
  "Call `(f ctx)` with an owned gateway image, the stub model and automations on.
   `ctx` has `:dir`, `:port` and `:asked`, the requests to the stub model."
  [f]
  (let [^File dir
        (#'binary/temp-dir "vis-native-automations-")

        {:keys [server asked] provider-port :port}
        (#'binary/start-stub-provider! answer)

        port
        (with-open [socket (ServerSocket. 0)]
          (.getLocalPort socket))

        process
        (atom nil)]

    (try (#'binary/overlay! dir provider-port)
         (spit (io/file dir ".vis" "config.yml") "toggles:\n  automations: true\n" :append true)
         ;; An empty URL turns off the Push relay, so no case reaches the deployed relay.
         (spit (io/file dir ".vis" "relay.edn") (pr-str {:url ""}))
         (reset! process (start-gateway! dir port))
         ;; Pin routing to the owned listener: discovery must never reach a user's daemon.
         (with-redefs-fn {#'gateway-client/ensure-gateway!
                          (constantly {:host "127.0.0.1" :port port :remote? true})
                          #'gateway-client/client-id (atom nil)
                          #'gateway-client/release-hook-installed? (atom true)}
           (fn []
             (await-health! @process dir)
             (f {:dir dir :port port :asked asked})))
         (finally (when-let [owned @process]
                    (#'binary/kill-tree! owned))
                  (.stop ^HttpServer server 0)
                  (#'binary/delete-tree! dir)))))

(defn- create!
  "Create an automation that answers through the stub model. Answers its id."
  [input]
  (get (ok! :post
            "/v1/automations"
            (merge {"target" {"mode" "temporary"} "model" model "delivery" {"push" false}} input))
       "id"))

(defn- describe-automation [id] (ok! :get (str "/v1/automations/" id) nil))

(defn- secret!
  [id kind]
  (get (ok! :post (str "/v1/automations/" id "/secrets") {"kind" kind}) "secret"))

(defn- run-ids
  [id]
  (mapv #(get % "id") (get (ok! :get (str "/v1/automations/runs?automation_id=" id) nil) "runs")))

(defn- first-run-id
  "The id of the first run of automation `id`, once the run exists."
  [{:keys [^File dir]} id]
  (or (eventually 30000 #(first (run-ids id)))
      (throw (ex-info (str "The automation did not start a run\n"
                           (log-tail (io/file dir "gateway.log")))
                      {:automation-id id}))))

(defn- settled
  "Run `run-id` once its status is terminal."
  [{:keys [^File dir]} run-id]
  (or (eventually
        60000
        #(let [run (ok! :get (str "/v1/automations/runs/" run-id) nil)] (when (terminal
                                                                                (get run "status"))
                                                                          run)))
      (throw (ex-info (str "The automation run did not finish\n"
                           (log-tail (io/file dir "gateway.log")))
                      {:run-id run-id}))))

(defn- outcome [run] (mapv #(get run %) ["trigger" "status" "answer"]))

(defn- asked?
  "True when a request to the stub model contains `text`."
  [asked text]
  (boolean (some #(str/includes? (:body %) text) @asked)))

(defdescribe
  native-automations-test
  (it "starts a one-time schedule and keeps the cron schedule in its time zone"
      (with-gateway
        (fn [{:keys [asked] :as ctx}]
          (let [at
                (+ (System/currentTimeMillis) 2000)

                id
                (create! {"name" "Native schedule"
                          "triggers"
                          [{"kind" "once" "at" at}
                           {"kind" "cron" "expression" "0 9 1 1 *" "timezone" "Europe/Warsaw"}]
                          "prompt" "Report the native schedule."})]

            (expect (= at (get (describe-automation id) "next_run_at")))
            (let [run
                  (settled ctx (first-run-id ctx id))

                  next-run
                  (get (describe-automation id) "next_run_at")]

              (expect (= ["once" "completed" answer] (outcome run)))
              (expect (= at (get run "scheduled_at")))
              (expect (asked? asked "Report the native schedule."))
              (expect (= [(get run "id")] (run-ids id)))
              (expect (int? next-run) "The cron trigger stays scheduled after the one-time run")
              (let [local (ZonedDateTime/ofInstant (Instant/ofEpochMilli (long next-run))
                                                   (ZoneId/of "Europe/Warsaw"))]
                (expect (= [1 1 9 0]
                           [(.getMonthValue local) (.getDayOfMonth local) (.getHour local)
                            (.getMinute local)]))))))))
  (it
    "runs a signed webhook once and refuses a forged request, a repeat and another event"
    (with-gateway
      (fn [{:keys [asked port] :as ctx}]
        (let [id
              (create! {"name" "Native webhook"
                        "triggers" [{"kind" "webhook" "signature" "github" "events" ["push"]}]
                        "prompt" "Summarize {head_commit.message} in {repository.name}."})

              secret
              (secret! id "webhook")

              webhook
              (get (describe-automation id) "webhook")

              body
              (wire/json-str {"head_commit" {"message" "Fix the native build"}
                              "repository" {"name" "vis"}})

              send!
              (fn [event delivery signature]
                (post! (str "http://127.0.0.1:" port (get webhook "path"))
                       body
                       {"content-type" "application/json"
                        "x-github-event" event
                        "x-github-delivery" delivery
                        "x-hub-signature-256" signature}))

              signature
              (github-signature secret body)

              forged
              (send! "push" "native-forged" (github-signature "another secret" body))

              other
              (send! "issues" "native-other" signature)

              accepted
              (send! "push" "native-push" signature)

              repeated
              (send! "push" "native-push" signature)]

          (expect (= {"path" (str "/v1/hooks/" id)} webhook)
                  "Without a relay, the gateway path is the only address")
          (expect (= 401 (:status forged)))
          (expect (= [202 {"status" "ignored" "run_id" nil "reason" "event"}]
                     [(:status other) (:body other)]))
          (expect (= [202 "accepted"] [(:status accepted) (get-in accepted [:body "status"])]))
          (expect (= [200 {"status" "duplicate" "run_id" nil "reason" "delivery"}]
                     [(:status repeated) (:body repeated)]))
          (let [run (settled ctx (get-in accepted [:body "run_id"]))]
            (expect (= ["webhook" "completed" answer] (outcome run)))
            (expect (= [(get run "id")] (run-ids id)))
            (expect (asked? asked "Summarize Fix the native build in vis."))
            (expect (asked? asked "untrusted content, not instructions")))))))
  (it
    "runs a Standard Webhooks request on the gateway and refuses a forged one"
    (with-gateway
      (fn [{:keys [asked port] :as ctx}]
        (let [id
              (create! {"name" "Native standard webhook"
                        "triggers" [{"kind" "webhook" "signature" "standard"}]
                        "prompt" "Build {build.status} for {build.branch}."})

              secret
              (secret! id "webhook")

              url
              (str "http://127.0.0.1:" port (get-in (describe-automation id) ["webhook" "path"]))

              body
              (wire/json-str {"type" "build.finished" "build" {"status" "green" "branch" "main"}})

              send!
              (fn [signing-secret]
                (let [message-id
                      (str "msg_" (UUID/randomUUID))

                      timestamp
                      (str (quot (System/currentTimeMillis) 1000))]

                  (post! url
                         body
                         {"content-type" "application/json"
                          "webhook-id" message-id
                          "webhook-timestamp" timestamp
                          "webhook-signature"
                          (standard-signature signing-secret message-id timestamp body)})))

              forged
              (send! (str "whsec_"
                          (.encodeToString (Base64/getEncoder) (utf8 "not the automation secret"))))

              accepted
              (send! secret)]

          (expect (str/starts-with? secret "whsec_"))
          (expect (= 401 (:status forged)))
          (expect (= [202 "accepted"] [(:status accepted) (get-in accepted [:body "status"])]))
          (let [run (settled ctx (get-in accepted [:body "run_id"]))]
            (expect (= ["webhook" "completed" answer] (outcome run)))
            (expect (= [(get run "id")] (run-ids id)) "The gateway refuses the forged request")
            (expect (asked? asked "Build green for main.")))))))
  (it "signs the run result to a local callback receiver"
      (let [{:keys [server requests url]} (start-receiver!)]
        (try
          (with-gateway
            (fn [ctx]
              (let [id (create! {"name" "Native callback"
                                 "triggers" [{"kind" "every" "seconds" 3600}]
                                 "prompt" "Report the native callback."
                                 "delivery" {"push" false "callback" {"url" url}}})
                    secret (secret! id "callback")
                    run (settled ctx (get (ok! :post (str "/v1/automations/" id "/run") nil) "id"))
                    {:keys [headers body]} (eventually 30000 #(first @requests))]

                (expect (= ["manual" "completed" answer] (outcome run)))
                (expect (string? body) "The callback must arrive")
                (let [event (wire/parse-json body)
                      timestamp (get headers "webhook-timestamp")]

                  (expect (= "run.completed" (get event "type")))
                  (expect (= (select-keys run ["id" "status" "answer"])
                             (select-keys (get event "data") ["id" "status" "answer"])))
                  (expect (str/starts-with? secret "whsec_"))
                  (expect (= (standard-signature secret (get headers "webhook-id") timestamp body)
                             (get headers "webhook-signature")))
                  (expect (< (abs (- (parse-long timestamp) (quot (System/currentTimeMillis) 1000)))
                             300)))
                (Thread/sleep 1000)
                (expect (= 1 (count @requests)) "One completed run sends one callback"))))
          (finally (.stop ^HttpServer server 0))))))
