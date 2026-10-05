(ns com.blockether.vis.internal.gateway.client-test
  "Unit coverage for the self-heal decision logic in [[client/ensure-gateway-serving!]].

   The point under test is that the /ui 404 self-heal is NON-DESTRUCTIVE: it only
   force-restarts a stale daemon that is genuinely idle, treats a transport blip as
   \"leave it alone\", and never confuses either with a real 404."
  (:require [babashka.http-client :as http]
            [clojure.string :as str]
            [lazytest.core :refer [defdescribe expect it]]
            [com.blockether.vis.contract.gateway :as gateway-contract]
            [com.blockether.vis.internal.gateway.client :as client]
            [com.blockether.vis.internal.gateway.discovery :as discovery])
  (:import [com.sun.net.httpserver HttpExchange HttpHandler HttpServer]
           [java.io ByteArrayInputStream]
           [java.net InetSocketAddress]
           [java.nio.charset StandardCharsets]
           [java.nio.file Files]))

(defn- rv
  "Resolve a (possibly private) var in the client namespace for with-redefs-fn."
  [sym]
  (ns-resolve 'com.blockether.vis.internal.gateway.client sym))

(def ^:private fake-entry {:host "127.0.0.1" :port 7890 :pid 4242 :secret "s"})

(defdescribe
  terminal-child-holds-and-releases-the-canonical-lease
  (it "terminal child holds and releases the canonical lease"
      ;; Regression: the standalone native TUI connected to an absent default gateway.
      (doseq [exit [0 7]]
        (let [calls (atom [])]
          (with-redefs-fn {(rv 'ensure-gateway!) #(do (swap! calls conj :discover) fake-entry)
                           (rv 'ensure-client!) (fn [entry]
                                                  (expect (= fake-entry entry))
                                                  (swap! calls conj :acquire))
                           (rv 'release-client!) #(swap! calls conj :release)}
            (fn []
              (expect (= exit
                         (client/run-tui!
                           ["/bin/sh" "-c"
                            (str
                              "test \"$VIS_GATEWAY_URL\" = http://127.0.0.1:7890 || exit 91; "
                              "test \"$VIS_GATEWAY_TOKEN\" = s || exit 92; "
                              "test \"$VIS_TUI_LOCAL_GATEWAY\" = \"$VIS_GATEWAY_URL\" || exit 93; "
                              "exit " exit)])))
              (expect (= [:discover :acquire :release] @calls))))))))

(defdescribe terminal-launch-failure-releases-lease
             (it "terminal launch failure releases lease"
                 (let [released (atom 0)]
                   (with-redefs-fn {(rv 'ensure-gateway!) (constantly fake-entry)
                                    (rv 'ensure-client!) (constantly "lease")
                                    (rv 'release-client!) #(swap! released inc)}
                     (fn []
                       (expect (try (client/run-tui! ["/nonexistent/vis-tui"])
                                    false
                                    (catch java.io.IOException _ true)))
                       (expect (= 1 @released)))))))

(defdescribe
  web-app-holds-the-lease-until-its-gateway-stops-answering
  (it "web app holds the lease until its gateway stops answering"
      ;; `vis-agent web` is the only client an auto-started gateway sees while the
      ;; browser tab is open, so it keeps its lease until the gateway itself is gone.
      (let [calls
            (atom [])

            answers
            (atom [200 503 200 :down 500 404])]

        (with-redefs-fn {(rv 'ensure-gateway-serving!) (fn [path & _]
                                                         (swap! calls conj [:serve path])
                                                         fake-entry)
                         (rv 'ensure-client!) (fn [entry]
                                                (expect (= fake-entry entry))
                                                (swap! calls conj :acquire))
                         (rv 'gw-send!) (fn [entry method path _]
                                          (expect (= [fake-entry "GET" "/healthz"]
                                                     [entry method path]))
                                          (let [[answer] @answers]
                                            (swap! answers rest)
                                            (swap! calls conj :check)
                                            (if (= :down answer)
                                              (throw (java.net.ConnectException. "refused"))
                                              {:status answer})))
                         (rv 'release-client!) #(swap! calls conj :release)}
          (fn []
            (expect (= 1 (client/run-web! #(swap! calls conj [:ready (:url %)]) {:interval-ms 0})))
            (expect (= [[:serve "/"] :acquire [:ready "http://127.0.0.1:7890/"] :check :check :check
                        :check :check :check :release]
                       @calls))
            (expect (empty? @answers)))))))

(defdescribe
  web-app-uses-or-starts-the-gateway-at-its-address
  ;; `vis-agent web --host/--port` names the gateway to use: this DB's daemon when it
  ;; answers there, else that daemon started there, else the gateway answering there.
  (let [running
        (fn [m]
          (merge {"status" "running" "managed" true "clients" 0 "running_turns" 0} m))

        decide
        (fn [{:keys [registered fresh? local? port-free? status]} opts]
          (let [calls (atom [])]
            (with-redefs-fn {(rv 'db-target) (constantly {:backend :sqlite
                                                          :path "/tmp/vis-web-test.mdb"})
                             #'discovery/read-registry (constantly registered)
                             #'discovery/registry-fresh? (fn [entry _]
                                                           (boolean (and fresh? entry)))
                             #'client/local-host? (constantly (not= false local?))
                             (rv 'port-free?) (fn [host port]
                                                (swap! calls conj [:port-free? host port])
                                                (not= false port-free?))
                             (rv 'status) (constantly (running status))
                             (rv 'stop-daemon!) #(swap! calls conj :stop)
                             (rv 'await-daemon-down!) (fn [_ host port]
                                                        (swap! calls conj [:down host port]))
                             #'client/connect-remote! (fn [{:keys [url]}]
                                                        (swap! calls conj [:attach url]))
                             (rv 'ensure-gateway-serving!) (fn [& args]
                                                             (swap! calls conj (into [:serve] args))
                                                             fake-entry)}
              (fn []
                (try ((rv 'web-gateway!) opts)
                     @calls
                     (catch clojure.lang.ExceptionInfo e
                       (conj @calls (select-keys (ex-data e) [:type :url :reason]))))))))

        elsewhere
        {:registered {:host "127.0.0.1" :port 7890 :pid 4242 :secret "s"} :fresh? true}]

    (it "uses the database's gateway wherever it runs when no address is given"
        (expect (= [[:serve "/"]] (decide elsewhere nil))))
    (it "reuses the database's gateway when it already answers at the address"
        (expect (= [[:serve "/"]]
                   (decide {:registered {:host "0.0.0.0" :port 8080 :pid 4242 :secret "s"}
                            :fresh? true}
                           {:host "127.0.0.1" :port 8080}))))
    (it "starts the gateway at the address when none runs, dialing loopback for every interface"
        (expect (= [[:port-free? "127.0.0.1" 8080] [:serve "/" {:host "127.0.0.1" :port 8080}]]
                   (decide {} {:port 8080})))
        (expect (= [[:port-free? "127.0.0.1" 8080] [:serve "/" {:host "0.0.0.0" :port 8080}]]
                   (decide {} {:host "0.0.0.0" :port 8080}))))
    (it "moves an idle managed gateway to the address"
        (expect (= [[:port-free? "127.0.0.1" 8080] :stop [:down "127.0.0.1" 7890]
                    [:serve "/" {:host "127.0.0.1" :port 8080}]]
                   (decide elsewhere {:port 8080}))))
    (it "leaves a busy or user-owned gateway where it runs"
        (expect (= [[:port-free? "127.0.0.1" 8080]
                    {:type :gateway/elsewhere :url "http://127.0.0.1:7890/" :reason :clients}]
                   (decide (assoc elsewhere :status {"clients" 1}) {:port 8080})))
        (expect (= [[:port-free? "127.0.0.1" 8080]
                    {:type :gateway/elsewhere :url "http://127.0.0.1:7890/" :reason :user-owned}]
                   (decide (assoc elsewhere :status {"managed" false}) {:port 8080}))))
    (it "attaches to the gateway on another machine or behind an occupied local port"
        (expect (= [[:attach "10.0.0.5:8080"] [:serve "/"]]
                   (decide {:local? false} {:host "10.0.0.5" :port 8080})))
        (expect (= [[:port-free? "127.0.0.1" 8080] [:attach "127.0.0.1:8080"] [:serve "/"]]
                   (decide {:port-free? false} {:port 8080}))))))

(defdescribe web-app-addresses
             (it "opens a bind on every interface on loopback and keeps IPv6 and remote URLs valid"
                 (expect (= "http://127.0.0.1:7890/" ((rv 'web-url) {:host "0.0.0.0" :port 7890})))
                 (expect (= "http://[::1]:7890/" ((rv 'web-url) {:host "::1" :port 7890})))
                 (expect (= "https://gateway.example.com:443/"
                            ((rv 'web-url)
                              {:base-url "https://gateway.example.com:443" :remote? true}))))
             (it "matches a daemon by its port and its bound address, every interface or loopback"
                 (let [answers-at? (rv 'answers-at?)]
                   (expect (answers-at? {:host "0.0.0.0" :port 8080} "127.0.0.1" 8080))
                   (expect (answers-at? {:host "127.0.0.1" :port 8080} "127.0.0.1" 8080))
                   (expect (answers-at? {:host "127.0.0.1" :port 8080} "localhost" 8080))
                   (expect (answers-at? {:host "::1" :port 8080} "127.0.0.1" 8080))
                   (expect (not (answers-at? {:host "127.0.0.1" :port 7890} "127.0.0.1" 8080)))
                   (expect (not (answers-at? {:host "127.0.0.1" :port 8080} "0.0.0.0" 8080)))))
             (it "starts gateways only on addresses of this machine"
                 (expect (client/local-host? "127.0.0.1"))
                 (expect (client/local-host? "0.0.0.0"))
                 (expect (client/local-host? "::1"))
                 ;; 192.0.2.0/24 is reserved for documentation (RFC 5737): never a local interface.
                 (expect (not (client/local-host? "192.0.2.1")))))

;; Regression: direct API debugging reimplemented registry discovery and authentication
;; instead of using the gateway client's canonical transport.
(defdescribe
  request-uses-the-canonical-authenticated-client
  (it "request uses the canonical authenticated client"
      (let [calls
            (atom [])

            response
            {:status 201 :body "created"}]

        (with-redefs-fn {(rv 'ensure-gateway!) (fn []
                                                 (swap! calls conj [:gateway])
                                                 fake-entry)
                         (rv 'ensure-client!) (fn [entry]
                                                (swap! calls conj [:client entry])
                                                "client-id")
                         (rv 'gw-send!) (fn [entry method path opts]
                                          (swap! calls conj [:request entry method path opts])
                                          response)}
          (fn []
            (expect
              (= response
                 (client/request! :post "/v1/debug" {:body {:hello "world"} :timeout-ms 1200})))
            (expect (= [[:gateway] [:client fake-entry]
                        [:request fake-entry "POST" "/v1/debug"
                         {:body {:hello "world"} :timeout-ms 1200}]]
                       @calls)))))))

(defdescribe binary-request-body-is-not-json-encoded
             (it "binary request body is not json encoded"
                 (let [payload
                       (byte-array [1 2 3])

                       sent
                       (atom nil)]

                   (with-redefs [http/request (fn [opts]
                                                (reset! sent opts)
                                                {:status 202 :body "{}"})]
                     ((rv 'gw-send!)
                       fake-entry
                       "POST"
                       "/v1/upload"
                       {:body payload :raw-body? true :headers {"Content-Type" "audio/wav"}})
                     (expect (identical? payload (:body @sent)))
                     (expect (= "audio/wav" (get-in @sent [:headers "Content-Type"])))))))

(defdescribe
  oauth-requires-paired-tls-away-from-loopback
  (it
    "oauth requires paired tls away from loopback"
    (let [sent (atom [])]
      (with-redefs-fn {(rv 'ensure-client!) (constantly "test-client")
                       #'http/request (fn [opts]
                                        (swap! sent conj opts)
                                        {:status 200 :body "{}"})}
        (fn []
          (doseq [url ["http://10.0.0.5:7890" "https://gateway.example.com"]
                  path ["/v1/providers/test/auth/complete" "/v1/mcp/servers/test/auth/start"]]

            (with-redefs-fn {(rv 'ensure-gateway!)
                             (constantly ((rv 'remote-entry)
                                           url
                                           (when (str/starts-with? url "http:") "test-pair")))}
              (fn []
                (expect (= :gateway/insecure-oauth
                           (:type (ex-data
                                    (try (client/request! :post path {:body {:flow_id "test-flow"}})
                                         (catch clojure.lang.ExceptionInfo e e)))))))))
          (expect (empty? @sent))
          (doseq [url ["http://127.0.0.1:7890" "https://gateway.example.com"]]
            (with-redefs-fn {(rv 'ensure-gateway!) (constantly
                                                     ((rv 'remote-entry) url "test-pair"))}
              (fn []
                (expect
                  (= 200 (:status (client/request! :post "/v1/providers/test/auth/poll"))))))))))))

(defdescribe
  oauth-does-not-forward-callbacks-through-http-redirects
  (it
    "oauth does not forward callbacks through http redirects"
    (let [requests
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

                                  path
                                  (.getPath (.getRequestURI exchange))]

                              (try (swap! requests conj path)
                                   (with-open [input (.getRequestBody exchange)]
                                     (slurp input))
                                   (if (= "/forwarded" path)
                                     (.sendResponseHeaders exchange 200 -1)
                                     (do
                                       (.set (.getResponseHeaders exchange) "Location" "/forwarded")
                                       (.sendResponseHeaders exchange 307 -1)))
                                   (finally (.close exchange)))))))
      (.start server)
      (try (with-redefs-fn {(rv 'ensure-client!) (constantly "test-client")
                            (rv 'ensure-gateway!)
                            (constantly {:host "127.0.0.1" :port (.getPort (.getAddress server))})}
             (fn []
               (expect (= 307
                          (:status (client/request! :post
                                                    "/v1/mcp/servers/test/auth/complete"
                                                    {:body {:flow_id "test-flow"
                                                            :input "test-callback"}}))))
               (expect (= ["/v1/mcp/servers/test/auth/complete"] @requests))))
           (finally (.stop server 0))))))

(defn- sse-body
  "Make a byte-live gateway SSE stream for one terminal voice job."
  [event-json]
  (ByteArrayInputStream. (.getBytes (str "data: " event-json "\n\n") StandardCharsets/UTF_8)))

;; Regression, user report: standalone local speech required a conversation and an AI provider.
(defdescribe
  transcription-stays-in-the-gateway-and-forgets-its-job
  (it
    "transcription stays in the gateway and forgets its job"
    (doseq [sid [nil "session-1"]]
      (let [prefix (if sid (str "/v1/sessions/" sid "/voice") "/v1/voice")
            audio (java.io.File/createTempFile "vis-client-voice" ".wav")
            calls (atom [])
            progress (atom [])]

        (spit audio "RIFF")
        (try (with-redefs
               [client/request!
                (fn [method path & [opts]]
                  (swap! calls conj [method path (dissoc opts :body)])
                  (cond
                    (= path "/v1/voice/model?engine=parakeet") {:status 200
                                                                :body "{\"status\":\"ready\"}"}
                    (= path (str prefix "?engine=parakeet"))
                    (do (expect (= "RIFF" (slurp (:body opts))))
                        (expect (:raw-body? opts))
                        {:status 202 :body "{\"id\":\"job-1\"}"})
                    (= path (str prefix "/jobs/job-1/events"))
                    {:status 200
                     :body
                     (sse-body
                       "{\"phase\":\"done\",\"progress\":100,\"is_done\":true,\"text\":\"hello\"}")}
                    (= path (str prefix "/jobs/job-1")) {:status 204 :body ""}
                    :else (throw (ex-info "unexpected request" {:method method :path path}))))]
               (expect (= "hello"
                          (client/transcribe-audio! sid
                                                    (.getPath audio)
                                                    {:engine-id "parakeet"
                                                     :on-progress #(swap! progress conj %)})))
               (expect (= [:post :post :get :delete] (mapv first @calls)))
               (expect (true? (get (last @progress) "is_done"))))
             (finally (.delete audio)))))))

;; Regression, user report: standalone local speech required a conversation and an AI provider.
(defdescribe
  asynchronous-synthesis-fetches-audio-and-forgets-its-job
  (it
    "asynchronous synthesis fetches audio and forgets its job"
    (doseq [sid [nil "session-2"]]
      (let [prefix (if sid (str "/v1/sessions/" sid "/speech") "/v1/speech")
            calls (atom [])
            wav (byte-array [82 73 70 70])]

        (with-redefs [client/request!
                      (fn [method path & [opts]]
                        (swap! calls conj [method path opts])
                        (cond (= path "/v1/speech/model?engine=pocket&voice_id=amy")
                              {:status 200 :body "{\"status\":\"ready\"}"}
                              (= path (str prefix "?engine=pocket"))
                              {:status 202
                               :body (.getBytes "{\"id\":\"job-2\"}" StandardCharsets/UTF_8)}
                              (= path (str prefix "/jobs/job-2/events"))
                              {:status 200 :body (sse-body "{\"phase\":\"done\",\"is_done\":true}")}
                              (= path (str prefix "/jobs/job-2/audio")) {:status 200 :body wav}
                              (= path (str prefix "/jobs/job-2")) {:status 204 :body ""}
                              :else (throw (ex-info "unexpected request"
                                                    {:method method :path path}))))]
          (let [audio (client/synthesize-speech! sid "hello" {:engine-id "pocket" :voice-id "amy"})]
            (try (expect (= (seq wav) (seq (Files/readAllBytes (.toPath audio)))))
                 (expect (= [:post :post :get :get :delete] (mapv first @calls)))
                 (expect (= {:text "hello" :voice "amy"} (get-in @calls [1 2 :body])))
                 (finally (.delete audio)))))))))

;; Regression: speech and voice refusals answered a bare `{"error": "..."}` body that only
;; the speech readers understood; they now carry the gateway's canonical error envelope.
(defdescribe
  speech-refusals-surface-the-canonical-error-message
  (it "speech refusals surface the canonical error message"
      (with-redefs [client/request!
                    (fn [method path & _]
                      (expect (= [:post "/v1/voice/model?engine=parakeet"] [method path]))
                      {:status 501
                       :body (str "{\"error\":{\"type\":\"engine-unavailable\","
                                  "\"message\":\"no transcription engine is registered\","
                                  "\"reasons\":[\"parakeet: model missing\"]}}")})]
        (let [failure (try (client/prepare-speech-model! :transcribe {:engine-id "parakeet"})
                           nil
                           (catch clojure.lang.ExceptionInfo e e))]
          (expect (= "no transcription engine is registered" (ex-message failure)))
          (expect (= 501 (:http-status (ex-data failure))))
          (expect (= ["parakeet: model missing"]
                     (get-in (ex-data failure) [:body "error" "reasons"])))))
      ;; a refusal without the canonical message still names its HTTP status
      (with-redefs [client/request! (fn [& _]
                                      {:status 502 :body "{\"error\":\"legacy\"}"})]
        (expect (= "gateway HTTP 502"
                   (try (client/prepare-speech-model! :synthesize {:engine-id "pocket"})
                        nil
                        (catch clojure.lang.ExceptionInfo e (ex-message e))))))))

;; Regression: the TUI spoke raw gateway HTTP for these two reads, so a channel
;; extension had to reach past the facade into this namespace to make either one.
(defdescribe
  channel-reads-answer-data-or-nil-when-the-daemon-cannot
  (it "channel reads answer data or nil when the daemon cannot"
      (let [seen
            (atom [])

            respond
            (fn [status body]
              (fn [method path opts]
                (swap! seen conj [method path opts])
                {:status status :body body}))]

        (with-redefs-fn {#'client/request!
                         (respond 200 "{\"artifacts\":[{\"filename\":\"decision.html\"}]}")}
          (fn []
            (expect (= [{"filename" "decision.html"}] (client/session-artifacts "session-1")))))
        (with-redefs-fn {#'client/request! (respond 200 "{\"attachments\":{\"max_bytes\":10}}")}
          (fn []
            (expect (= {"attachments" {"max_bytes" 10}} (client/capabilities)))))
        ;; each read is bounded, because a person is waiting at an open dialog
        (expect (= [[:get "/v1/sessions/session-1/artifacts" {:timeout-ms 5000}]
                    [:get "/v1/capabilities" {:timeout-ms 5000}]]
                   @seen))
        (with-redefs-fn {#'client/request! (respond 503 "")}
          (fn []
            (expect (nil? (client/session-artifacts "session-1")))
            (expect (nil? (client/capabilities))))))))

(defn- await-value
  "Wait for a background cache refresh to publish its expected value."
  [read expected]
  (loop [attempts 100]
    (let [value (read)]
      (cond (= expected value) value
            (zero? attempts) nil
            :else (do (Thread/sleep 10) (recur (dec attempts)))))))

;; Reported in Vis session a64d44c2-8228-455f-926e-b3381f19a93b: the TUI had
;; no canonical View action with which a live job row could change shared selection.
(defdescribe view-action-uses-the-one-kind-independent-route
             (it "view action uses the one kind independent route"
                 (let [request (atom nil)]
                   (with-redefs-fn {(rv 'send-json!) (fn [method path body]
                                                       (reset! request [method path body])
                                                       {"action" "select"
                                                        "view_id" "view-1"
                                                        "is_accepted" true
                                                        "node_id" "jobs"
                                                        "item_ids" ["macos"]})}
                     (fn []
                       (expect (= {:action :select
                                   :view-id "view-1"
                                   :is-accepted true
                                   :node-id "jobs"
                                   :item-ids ["macos"]}
                                  (client/view-action!
                                    "session-1"
                                    "view-1"
                                    {:action :select :node-id "jobs" :item-ids ["macos"]})))
                       (expect (= ["POST" "/v1/sessions/session-1/views/view-1/actions"
                                   {:action "select" :node-id "jobs" :item-ids ["macos"]}]
                                  @request)))))))

(defdescribe
  ensure-project-for-root-uses-project-action-route
  (it "ensure project for root uses project action route"
      (let [request (atom nil)]
        (with-redefs-fn {(rv 'send-json!) (fn [method path body]
                                            (reset! request [method path body])
                                            {:id "project-id"})}
          (fn []
            (expect (= {:id "project-id"} (client/ensure-project-for-root! "/workspace" "Vis")))
            (expect (= ["POST" "/v1/projects/actions/ensure" {:root "/workspace" :name "Vis"}]
                       @request)))))))

;; Regression, Vis session ae259fdd-2712-4591-8f12-e1cdff30b208: the TUI
;; had no gateway-owned catalog and initialized a second CPython runtime locally.
(defdescribe session-slashes-uses-the-gateway-catalog-with-a-cold-load-timeout
             (it "session slashes uses the gateway catalog with a cold load timeout"
                 (let [calls (atom [])]
                   (with-redefs-fn {(rv 'ensure-gateway!) (constantly fake-entry)
                                    (rv 'ensure-client!) (fn [entry]
                                                           (swap! calls conj [:client entry])
                                                           "client-id")
                                    (rv 'send-json-with-entry!)
                                    (fn [entry method path body opts]
                                      (swap! calls conj [:request entry method path body opts])
                                      {"commands" [{"name" "/python-echo" "doc" "Echo"}]})}
                     (fn []
                       (expect (= [{"name" "/python-echo" "doc" "Echo"}]
                                  (client/session-slashes "session-1" :tui)))
                       (expect (= [[:client fake-entry]
                                   [:request fake-entry "GET"
                                    "/v1/sessions/session-1/slashes?channel=tui" nil
                                    {:timeout-ms 120000}]]
                                  @calls)))))))

(defdescribe ensure-client-registers-once-from-canonical-string-keyed-response
             (it "ensure client registers once from canonical string keyed response"
                 (let [client-id-atom
                       @(rv 'client-id)

                       previous
                       @client-id-atom

                       calls
                       (atom 0)

                       ensure-client
                       (rv 'ensure-client!)]

                   (try (reset! client-id-atom nil)
                        (with-redefs-fn {(rv 'send-json-with-entry!)
                                         (fn [_entry method path body]
                                           (swap! calls inc)
                                           (expect (= "POST" method))
                                           (expect (= "/v1/clients" path))
                                           (expect (integer? (:pid body)))
                                           {"client_id" "lease-1"})
                                         (rv 'ensure-release-hook!) (fn [])}
                          (fn []
                            (expect (= "lease-1" (ensure-client fake-entry)))
                            (expect (= "lease-1" (ensure-client fake-entry)))
                            (expect (= 1 @calls))))
                        (finally (reset! client-id-atom previous))))))

;; Regression, Vis session ae259fdd-2712-4591-8f12-e1cdff30b208: concurrent
;; TUI startup callbacks each entered gateway discovery and repeated the full wait.
(defdescribe
  concurrent-gateway-ensure-is-single-flight-per-database
  (it
    "concurrent gateway ensure is single flight per database"
    (let [cached-atom
          @(rv 'cached-entry)

          fresh-until-atom
          @(rv 'entry-fresh-until-ns)

          previous-cached
          @cached-atom

          previous-fresh-until
          @fresh-until-atom

          calls
          (atom 0)

          start
          (promise)

          rendezvous
          (java.util.concurrent.CyclicBarrier. 2)

          discover
          (fn [& _]
            (swap! calls inc)
            (try (.await rendezvous 250 java.util.concurrent.TimeUnit/MILLISECONDS)
                 (catch java.util.concurrent.TimeoutException _ nil)
                 (catch java.util.concurrent.BrokenBarrierException _ nil))
            {:mode :spawned :entry fake-entry})]

      (try (reset! cached-atom nil)
           (reset! fresh-until-atom 0)
           (with-redefs-fn {(rv 'remote-gateway) (constantly nil)
                            (rv 'db-target) (constantly "/tmp/single-flight/vis.db")
                            #'discovery/registry-fresh? (constantly false)
                            #'discovery/pid-alive? (constantly true)
                            (rv 'discover-or-recover!) discover
                            (rv 'bounce-stale-daemon!) (constantly {:bounced? false})
                            (rv 'assert-compatible!) identity}
             (fn []
               (let [workers (mapv (fn [_]
                                     (future @start (client/ensure-gateway!)))
                                   (range 2))]
                 (deliver start true)
                 (expect (= [fake-entry fake-entry] (mapv #(deref % 2000 ::timeout) workers)))
                 (expect (= 1 @calls)
                         "one process performs discovery while peers reuse its result"))))
           (finally (reset! cached-atom previous-cached)
                    (reset! fresh-until-atom previous-fresh-until))))))

;; Regression #290: a managed gateway that exits while starting is a user-facing
;; failure naming its boot log, not an opaque "did not become ready".
(defdescribe
  ensure-gateway!-names-the-boot-log-when-the-daemon-exits-at-start
  (it
    "ensure gateway! names the boot log when the daemon exits at start"
    (let [cached-atom
          @(rv 'cached-entry)

          fresh-until-atom
          @(rv 'entry-fresh-until-ns)

          previous-cached
          @cached-atom

          previous-fresh-until
          @fresh-until-atom

          boot-log
          (java.io.File/createTempFile "gateway-boot-" ".log")]

      (spit boot-log "starting\n\njava.lang.IllegalStateException: config is invalid\n")
      (try (reset! cached-atom nil)
           (reset! fresh-until-atom 0)
           (with-redefs-fn {(rv 'remote-gateway) (constantly nil)
                            (rv 'db-target) (constantly "/tmp/boot-exit/vis.db")
                            (rv 'discover-or-recover!) (constantly {:mode :exited
                                                                    :pid 2147483646
                                                                    :boot-log (.getPath boot-log)})}
             (fn []
               (let [failure
                     (try (client/ensure-gateway!) nil (catch clojure.lang.ExceptionInfo e e))

                     data
                     (ex-data failure)]

                 (expect (true? (:vis/user-error data)))
                 (expect (= :gateway/start-failed (:type data)))
                 (expect (str/includes? (ex-message failure) "stopped while it was starting"))
                 (expect (str/includes? (ex-message failure) "config is invalid"))
                 (expect (str/includes? (ex-message failure) (.getPath boot-log))))))
           (finally (reset! cached-atom previous-cached)
                    (reset! fresh-until-atom previous-fresh-until)
                    (.delete boot-log))))))

;; Regression #290: a start still running after the slow-start threshold names the
;; daemon's boot log once, so a cold `--jvm` start is not a silent wait.
(defdescribe
  progress-reporter-names-the-boot-log-of-a-slow-start
  (it "progress reporter names the boot log of a slow start"
      (let [buf
            (java.io.ByteArrayOutputStream.)

            previous-err
            System/err]

        (System/setErr (java.io.PrintStream. buf true "UTF-8"))
        (try (with-redefs-fn {(rv 'interactive-tty?) (constantly false)}
               (fn []
                 (let [report ((rv 'progress-reporter))]
                   (report {:phase :spawning :pid 4242 :boot-log "/tmp/gateway-boot-290.log"})
                   (report {:phase :tick :elapsed-ms 1000})
                   (expect (not (str/includes? (.toString buf "UTF-8") "boot log"))
                           "not before the threshold")
                   (report {:phase :tick :elapsed-ms 16000})
                   (report {:phase :tick :elapsed-ms 17000})
                   (report {:phase :exited :pid 4242 :boot-log "/tmp/gateway-boot-290.log"}))))
             (finally (System/setErr previous-err)))
        (let [out (.toString buf "UTF-8")]
          (expect (= 1 (count (re-seq #"boot log: /tmp/gateway-boot-290\.log" out))) "named once")
          (expect (str/includes? out "✗ vis stopped while starting"))))))

(defdescribe
  authenticated-loopback-orphan-is-stopped-and-replaced
  (it
    "authenticated loopback orphan is stopped and replaced"
    (let [token-file
          (java.io.File/createTempFile "vis-gateway-token-" ".txt")

          calls
          (atom [])]

      (try (spit token-file "stable-secret\n")
           (with-redefs-fn {#'discovery/default-token-file (fn []
                                                             token-file)
                            #'discovery/pid-alive? (constantly true)
                            #'discovery/read-registry (constantly nil)
                            (rv 'port-free?) (constantly true)
                            (rv 'gw-send!)
                            (fn [entry method path opts]
                              (swap! calls conj [entry method path opts])
                              (case path
                                "/healthz"
                                {:status 200
                                 :body (str "{\"status\":\"ok\",\"secret_match\":true,"
                                            "\"pid\":9154,\"db\":\"/tmp/recover/vis.db\"}")}

                                "/v1/admin/stop"
                                {:status 200}))}
             (fn []
               (expect (true?
                         ((rv 'retire-loopback-orphan!) "/tmp/recover/vis.db" "127.0.0.1" 7890)))
               (expect (= [[{:host "127.0.0.1" :port 7890 :secret "stable-secret"} "GET" "/healthz"
                            {:timeout-ms 1500 :headers {"X-Vis-Suppress-Registry-Recovery" "true"}}]
                           [{:host "127.0.0.1" :port 7890 :secret "stable-secret" :pid 9154} "POST"
                            "/v1/admin/stop" {}]]
                          @calls))))
           (finally (.delete token-file))))))

(defdescribe
  registered-loopback-gateway-is-never-retired
  (it "registered loopback gateway is never retired"
      (let [calls (atom [])]
        (with-redefs-fn {#'discovery/read-registry (constantly fake-entry)
                         (rv 'gw-send!) (fn [& args]
                                          (swap! calls conj args))}
          (fn []
            (expect (nil?
                      ((rv 'retire-loopback-orphan!) "/tmp/registered/vis.db" "127.0.0.1" 7890)))
            (expect (empty? @calls)))))))

(defdescribe
  occupied-orphan-port-never-spawns-a-bind-loser
  (it "occupied orphan port never spawns a bind loser"
      (let [spawns
            (atom 0)

            ex
            (with-redefs-fn {(rv 'retire-loopback-orphan!) (constantly nil)
                             (rv 'port-free?) (constantly false)
                             #'discovery/await-registry! (fn [_db _probe opts]
                                                           (expect (= 3000 (:timeout-ms opts)))
                                                           (expect (= 100 (:poll-ms opts)))
                                                           nil)
                             #'discovery/discover-or-start! (fn [& _]
                                                              (swap! spawns inc)
                                                              {:mode :spawned :entry fake-entry})}
              (fn []
                (try ((rv 'discover-or-recover!) "/tmp/orphan/vis.db" "127.0.0.1" 7890)
                     nil
                     (catch clojure.lang.ExceptionInfo e e))))]

        (expect (= :gateway/orphaned-port (:type (ex-data ex))))
        (expect (true? (:vis/user-error (ex-data ex))))
        (expect (= "127.0.0.1" (:host (ex-data ex))))
        (expect (= 7890 (:port (ex-data ex))))
        (expect (zero? @spawns) "an occupied port can never enter the daemon spawn path"))))

(defdescribe occupied-port-allows-a-registering-daemon-to-win-the-race
             (it "occupied port allows a registering daemon to win the race"
                 (let [spawns (atom 0)]
                   (with-redefs-fn {(rv 'retire-loopback-orphan!) (constantly nil)
                                    (rv 'port-free?) (constantly false)
                                    #'discovery/await-registry! (fn [_db _probe _opts]
                                                                  fake-entry)
                                    #'discovery/discover-or-start! (fn [& _]
                                                                     (swap! spawns inc)
                                                                     nil)}
                     (fn []
                       (expect (= {:mode :awaited :entry fake-entry}
                                  ((rv 'discover-or-recover!) "/tmp/race/vis.db" "127.0.0.1" 7890)))
                       (expect (zero? @spawns)))))))

(defdescribe stale-registry-stop-does-not-report-a-listening-gateway-as-stopped
             (it "stale registry stop does not report a listening gateway as stopped"
                 (let [server
                       (java.net.ServerSocket. 0)

                       port
                       (.getLocalPort server)]

                   (try (let [result (with-redefs-fn {(rv 'db-target) (constantly
                                                                        "/tmp/orphan/vis.db")
                                                      #'discovery/read-registry
                                                      (constantly (assoc fake-entry :port port))
                                                      #'discovery/registry-fresh? (constantly false)
                                                      #'discovery/pid-alive? (constantly false)}
                                       (fn []
                                         (client/stop-daemon!)))]
                          (expect (not= "stopped" (:status result))
                                  "a listening configured endpoint must not be reported as stopped")
                          (expect (= :gateway/orphaned-daemon (:type result)))
                          (expect (= "127.0.0.1" (:host result)))
                          (expect (= port (:port result))))
                        (finally (.close server))))))

(def ^:private idle-status
  {"status" "running" "managed" true "clients" 0 "running_turns" 0 "pid" 4242})

(defdescribe
  daemon-idle-is-the-one-definition-of-a-free-bounce
  (it "a managed daemon nobody holds is free to release"
      (expect (true? (:idle? (client/daemon-idle? idle-status))))
      (expect (= :idle (:reason (client/daemon-idle? idle-status)))))
  (it "work in progress is never aborted for a release that was optional"
      (expect (= :clients (:reason (client/daemon-idle? (assoc idle-status "clients" 2)))))
      (expect (= :running-turns
                 (:reason (client/daemon-idle? (assoc idle-status "running_turns" 1))))))
  (it "a daemon somebody started by hand belongs to them, idle or not"
      (expect (= :user-owned (:reason (client/daemon-idle? (assoc idle-status "managed" false))))))
  (it "nothing running is nothing to stop"
      (expect (= :not-running (:reason (client/daemon-idle? {"status" "stopped"})))))
  (it "a count in a shape this build does not know refuses instead of throwing"
      ;; The peer whose status decides a bounce is by definition a build this one did
      ;; not ship with: a count it cannot read must never read as zero, and must never
      ;; take the attach path down with it.
      (doseq [odd [{} [] :two "two"]]
        (expect (= :not-running (:reason (client/daemon-idle? (assoc idle-status "clients" odd)))))
        (expect (= :not-running
                   (:reason (client/daemon-idle? (assoc idle-status "running_turns" odd))))))
      (expect (= :not-running (:reason (client/daemon-idle? nil))))
      (expect (false? (:bounce? (client/stale-bounce-verdict {:ours "0.1.40"
                                                              :theirs "0.1.39"
                                                              :status (assoc idle-status
                                                                        "clients" "many")})))))
  (it "a numeric count is read whichever wire shape carried it"
      (expect (= :clients (:reason (client/daemon-idle? (assoc idle-status "clients" "2")))))
      (expect (= :idle (:reason (client/daemon-idle? (assoc idle-status "clients" 0.0))))))
  (it "the same rule, calibrated for a caller that is itself attached"
      (expect (true? (:idle? (client/daemon-idle? (assoc idle-status "clients" 1)
                                                  {:tolerate-clients 1}))))
      (expect (true? (:idle? (client/daemon-idle? (assoc idle-status "managed" false)
                                                  {:user-owned-ok? true}))))))

(defdescribe stop-if-idle-leaves-a-daemon-somebody-is-using-alone
             (it "stop if idle leaves a daemon somebody is using alone"
                 (let [stops (atom 0)]
                   (with-redefs-fn {(rv 'remote-gateway) (constantly nil)
                                    #'client/status (constantly (assoc idle-status
                                                                  "clients" 2
                                                                  "running_turns" 1))
                                    #'client/stop-daemon! (fn []
                                                            (swap! stops inc)
                                                            {:status "stopped"})}
                     (fn []
                       (let [verdict (client/stop-daemon-if-idle!)]
                         (expect (false? (:stopped? verdict)))
                         (expect (= :clients (:reason verdict)))
                         (expect (zero? @stops) "an update must never abort an open session")))))))

(defdescribe stop-if-idle-releases-an-unused-managed-daemon
             (it "stop if idle releases an unused managed daemon"
                 (let [stops (atom 0)]
                   (with-redefs-fn {(rv 'remote-gateway) (constantly nil)
                                    #'client/status (constantly idle-status)
                                    #'client/stop-daemon! (fn []
                                                            (swap! stops inc)
                                                            {:status "stopped" :stopping false})}
                     (fn []
                       (let [verdict (client/stop-daemon-if-idle!)]
                         (expect (true? (:stopped? verdict)))
                         (expect (= 1 @stops))))))))

;; Regression, session 78b0c0b5-f5ba-453f-97ee-af0a85f72d25: the freshly
;; updated protocol-3 runtime could probe its protocol-2 gateway, but the safety
;; status and stop requests were refused before the idle daemon could be released.
(defdescribe
  update-can-release-an-idle-gateway-across-the-old-protocol-boundary
  (it
    "update can release an idle gateway across the old protocol boundary"
    (let [handshake
          @(rv 'gateway-handshake*)

          previous
          @handshake

          calls
          (atom [])]

      (reset! handshake {:protocol 2 :min-client 2 :min-gateway 2 :version "0.1.41"})
      (try
        (with-redefs-fn
          {(rv 'remote-gateway) (constantly nil)
           (rv 'db-target) (constantly "/tmp/vis-update-control-test.db")
           #'discovery/read-registry (constantly fake-entry)
           #'discovery/registry-fresh? (constantly true)
           #'http/request
           (fn [{:keys [method headers] :as request}]
             (swap! calls conj request)
             (if (= "2" (get headers "x-vis-min-gateway-protocol"))
               {:status 200
                :body
                (if (= :get method)
                  "{\"status\":\"running\",\"managed\":true,\"clients\":0,\"running_turns\":0,\"pid\":4242}"
                  "{\"status\":\"stopped\",\"stopping\":false}")}
               {:status 400 :body "{\"message\":\"Update the gateway\"}"}))}
          (fn []
            (let [result (client/stop-daemon-if-idle!)]
              (expect (true? (:stopped? result)))
              (expect (= [:get :post] (mapv :method @calls)))
              (expect (every? #(= "2" (get-in % [:headers "x-vis-min-gateway-protocol"]))
                              @calls)))))
        (finally (reset! handshake previous))))))

;; The state `vis-agent update` leaves behind when a session was open: the daemon
;; keeps serving the old image, so the next client to find it unused replaces it.
(defdescribe
  a-daemon-older-than-this-build-is-replaced-only-when-nobody-is-using-it
  (it "an idle daemon on the old image is the whole reason this rule exists"
      (let [verdict (client/stale-bounce-verdict
                      {:ours "0.1.40" :theirs "0.1.39" :status idle-status})]
        (expect (true? (:bounce? verdict)))
        (expect (= "0.1.39" (:from verdict)))
        (expect (= "0.1.40" (:to verdict)))))
  (it "no version is worth aborting somebody's work for"
      (expect (= :clients
                 (:reason (client/stale-bounce-verdict {:ours "0.1.40"
                                                        :theirs "0.1.39"
                                                        :status (assoc idle-status "clients" 1)}))))
      (expect (= :running-turns
                 (:reason (client/stale-bounce-verdict {:ours "0.1.40"
                                                        :theirs "0.1.39"
                                                        :status (assoc idle-status
                                                                  "running_turns" 1)}))))
      (expect (= :user-owned
                 (:reason (client/stale-bounce-verdict {:ours "0.1.40"
                                                        :theirs "0.1.39"
                                                        :status (assoc idle-status
                                                                  "managed" false)})))))
  (it "same build, an older client, or a dev checkout: nothing to pick up"
      (expect (= :fresh
                 (:reason (client/stale-bounce-verdict
                            {:ours "0.1.40" :theirs "0.1.40" :status idle-status}))))
      (expect (= :fresh
                 (:reason (client/stale-bounce-verdict
                            {:ours "0.1.39" :theirs "0.1.40" :status idle-status}))))
      (expect (= :fresh
                 (:reason (client/stale-bounce-verdict
                            {:ours "dev" :theirs "0.1.39" :status idle-status})))))
  (it "a dev checkout has no release to be ordered by, so its commit decides"
      (expect (true? (:bounce? (client/stale-bounce-verdict {:ours "dev"
                                                             :theirs "dev"
                                                             :our-build "aaa111aaa111"
                                                             :their-build "bbb222bbb222"
                                                             :status idle-status}))))
      (expect (= :clients
                 (:reason (client/stale-bounce-verdict {:ours "dev"
                                                        :theirs "dev"
                                                        :our-build "aaa111aaa111"
                                                        :their-build "bbb222bbb222"
                                                        :status (assoc idle-status "clients" 1)})))
              "a commit is worth no more of somebody's work than a version is")
      (expect (= :fresh
                 (:reason (client/stale-bounce-verdict {:ours "dev"
                                                        :theirs "dev"
                                                        :our-build "aaa111aaa111"
                                                        :their-build "aaa111aaa111"
                                                        :status idle-status}))))
      (expect (= :fresh
                 (:reason
                   (client/stale-bounce-verdict
                     {:ours "dev" :theirs "dev" :our-build "aaa111aaa111" :status idle-status})))
              "a daemon too old to advertise a build says nothing about being stale"))
  (it "a status nobody could read is not evidence of an idle daemon"
      (expect (false? (:bounce? (client/stale-bounce-verdict
                                  {:ours "0.1.40" :theirs "0.1.39" :status nil}))))))

(defdescribe
  a-stale-daemon-is-bounced-once-per-process-never-in-a-loop
  (it
    "a stale daemon is bounced once per process never in a loop"
    (let [stops
          (atom 0)

          guard
          @(rv 'stale-bounce-attempted?)

          handshake
          @(rv 'gateway-handshake*)

          previous
          @handshake]

      (reset! guard false)
      (reset! handshake {:protocol 2 :min-client 2 :min-gateway 2 :version "0.1.39"})
      (try (with-redefs-fn {(requiring-resolve
                              'com.blockether.vis.internal.gateway.runtime/release-version)
                            (constantly "0.1.40")
                            (rv 'report-version-bounce!) (constantly nil)
                            (rv 'send-json-with-entry!) (fn [& _]
                                                          idle-status)
                            (rv 'db-target) (constantly "/tmp/vis-stale-bounce-test.db")
                            (rv 'await-daemon-down!) (constantly true)
                            #'client/stop-daemon! (fn []
                                                    (swap! stops inc)
                                                    {:status "stopped" :stopping false})}
             (fn []
               (let [bounce! (rv 'bounce-stale-daemon!)]
                 (expect (true? (:bounced? (bounce! fake-entry))))
                 (expect (= :checked (:reason (bounce! fake-entry)))
                         "a daemon that comes back old costs one restart, never a restart loop")
                 (expect (= 1 @stops)))))
           (finally (reset! guard false) (reset! handshake previous))))))

;; The same pickup for a source checkout, where both halves say "dev": the daemon's
;; advertised commit is the whole difference, and it must work with no native image
;; and no release version anywhere in sight.
(defdescribe
  a-dev-daemon-on-another-commit-is-replaced-by-its-build-id
  (it
    "a dev daemon on another commit is replaced by its build id"
    (let [stops
          (atom 0)

          guard
          @(rv 'stale-bounce-attempted?)

          handshake
          @(rv 'gateway-handshake*)

          previous
          @handshake]

      (reset! guard false)
      (reset! handshake
        {:protocol 2 :min-client 2 :min-gateway 2 :version "dev" :build "bbb222bbb222"})
      (try (with-redefs-fn {(requiring-resolve
                              'com.blockether.vis.internal.gateway.runtime/release-version)
                            (constantly "dev")
                            (requiring-resolve
                              'com.blockether.vis.internal.gateway.runtime/build-id)
                            (constantly "aaa111aaa111")
                            (rv 'report-version-bounce!) (constantly nil)
                            (rv 'send-json-with-entry!) (fn [& _]
                                                          idle-status)
                            (rv 'db-target) (constantly "/tmp/vis-dev-bounce-test.db")
                            (rv 'await-daemon-down!) (constantly true)
                            #'client/stop-daemon! (fn []
                                                    (swap! stops inc)
                                                    {:status "stopped" :stopping false})}
             (fn []
               (let [bounce! (rv 'bounce-stale-daemon!)]
                 (expect (true? (:bounced? (bounce! fake-entry))))
                 (expect (= 1 @stops)))))
           (finally (reset! guard false) (reset! handshake previous))))))

;; The daemon `vis-agent update` could not release is usually one whose wire protocol
;; this build no longer speaks; the mismatch screen belongs to a daemon somebody is
;; USING, never to an idle one this process is free to replace.
(defdescribe
  an-idle-daemon-too-old-to-speak-to-is-replaced-not-refused
  (it
    "an idle daemon too old to speak to is replaced not refused"
    (let [guard
          @(rv 'stale-bounce-attempted?)

          handshake
          @(rv 'gateway-handshake*)

          cached
          @(rv 'cached-entry)

          previous-handshake
          @handshake

          previous-entry
          @cached

          stops
          (atom 0)

          attaches
          (atom 0)

          new-entry
          (assoc fake-entry :pid 4243)]

      (reset! guard false)
      (reset! cached nil)
      (reset! handshake {:protocol 1 :min-client 1 :min-gateway 1 :version "0.1.39"})
      (try
        (with-redefs-fn
          {(requiring-resolve 'com.blockether.vis.internal.gateway.runtime/release-version)
           (constantly "0.1.40")
           (rv 'report-version-bounce!) (constantly nil)
           (rv 'remote-gateway) (constantly nil)
           (rv 'db-target) (constantly "/tmp/vis-stale-bounce-test.db")
           (rv 'send-json-with-entry!) (fn [& _]
                                         idle-status)
           (rv 'await-daemon-down!) (constantly true)
           #'discovery/registry-fresh? (constantly false)
           (rv 'discover-or-recover!)
           (fn [& _]
             (let [attach (swap! attaches inc)]
               (when (> attach 1)
                 ;; What this process starts in its place speaks this build's
                 ;; protocol — READ, not spelled out, so raising the floor never
                 ;; leaves this fixture pretending to be a daemon it just refused.
                 (let [now gateway-contract/protocol-version]
                   (reset! handshake
                     {:protocol now :min-client now :min-gateway now :version "0.1.40"})))
               {:entry (if (> attach 1) new-entry fake-entry)}))
           #'client/stop-daemon! (fn []
                                   (swap! stops inc)
                                   (reset! cached nil)
                                   {:status "stopped" :stopping false})}
          (fn []
            (expect (= new-entry (client/ensure-gateway!))
                    "an idle daemon older than this build is replaced, not refused as incompatible")
            (expect (= 1 @stops))
            (expect (= 2 @attaches))))
        (finally (reset! guard false)
                 (reset! handshake previous-handshake)
                 (reset! cached previous-entry))))))

;; Regression (reported: a gateway that stopped answering had to be killed by hand):
;; `stop-daemon!` reported a live orphan and handed the human an `lsof` line, with
;; the daemon's pid sitting in the registry entry it had just read.
(defdescribe an-unresponsive-daemon-is-escalated-to-its-registered-pid
             (it "an unresponsive daemon is escalated to its registered pid"
                 (let [killed
                       (atom nil)

                       result
                       (with-redefs-fn {(rv 'db-target) (constantly "/tmp/wedged/vis.db")
                                        (rv 'remote-gateway) (constantly nil)
                                        #'discovery/read-registry (constantly fake-entry)
                                        #'discovery/registry-fresh? (constantly false)
                                        (rv 'port-free?) (constantly false)
                                        (rv 'kill-registered-daemon!) (fn [db entry]
                                                                        (reset! killed [db entry])
                                                                        {:signal :term
                                                                         :stopped? true})}
                         (fn []
                           (client/stop-daemon!)))]

                   (expect (= "stopped" (:status result)))
                   (expect (= :term (:escalated result)))
                   (expect (= ["/tmp/wedged/vis.db" fake-entry] @killed)))))

(defdescribe a-pid-that-is-not-provably-ours-is-never-signalled
             (it "a dead pid, or none at all, escalates to nothing"
                 (with-redefs-fn {#'discovery/pid-alive? (constantly false)}
                   (fn []
                     (expect (nil? ((rv 'registered-daemon-handle) "/tmp/x/vis.db" 4242)))))
                 (expect (nil? ((rv 'registered-daemon-handle) "/tmp/x/vis.db" nil)))
                 (expect (= {:signal nil :stopped? false}
                            (with-redefs-fn {#'discovery/pid-alive? (constantly false)}
                              (fn []
                                ((rv 'kill-registered-daemon!) "/tmp/x/vis.db" fake-entry)))))))

(defdescribe
  provider-limits-restores-engine-shape-from-gateway-wire
  (it "provider limits restores engine shape from gateway wire"
      (let [request (atom nil)]
        (with-redefs-fn {(rv 'ensure-gateway-serving!) (fn [path]
                                                         (reset! request path)
                                                         fake-entry)
                         (rv 'ensure-client!) (constantly "client-id")
                         (rv 'send-json-with-entry!)
                         (fn [_ method path]
                           (expect (= "GET" method))
                           (expect (= @request path))
                           {"report" {"provider_id" "openai-codex"
                                      "status" "ok"
                                      "dynamic" {"limits" [{"id" "codex-5h"
                                                            "scope" "account"
                                                            "kind" "percentage"
                                                            "precision" "percent"
                                                            "source" "live"
                                                            "window" {"kind" "rolling"
                                                                      "unit" "hour"
                                                                      "size" 5
                                                                      "resets_at_ms" 1234}}]}}})}
          (fn []
            (let [report (client/provider-limits :openai-codex)]
              (expect (= "/v1/providers/openai-codex/limits" @request))
              (expect (= :openai-codex (:provider-id report)))
              (expect (= :ok (:status report)))
              (expect (= :codex-5h (get-in report [:dynamic :limits 0 :id])))
              (expect (= :account (get-in report [:dynamic :limits 0 :scope])))
              (expect (= :rolling (get-in report [:dynamic :limits 0 :window :kind])))
              (expect (= :hour (get-in report [:dynamic :limits 0 :window :unit])))
              (expect (= 1234 (get-in report [:dynamic :limits 0 :window :resets-at-ms])))))))))

(defdescribe provider-status-reads-is-authenticated-from-gateway-wire
             (it "provider status reads is authenticated from gateway wire"
                 ;; The gateway emits snake_case wire keys (`is_authenticated`). The client
                 ;; returns the canonical STRING-keyed status map verbatim — no keyword
                 ;; restoration — so consumers read `(get status "is_authenticated")`.
                 (let [request (atom nil)]
                   (with-redefs-fn {(rv 'ensure-gateway-serving!) (fn [path]
                                                                    (reset! request path)
                                                                    fake-entry)
                                    (rv 'ensure-client!) (constantly "client-id")
                                    (rv 'send-json-with-entry!)
                                    (fn [_ method path]
                                      (expect (= "GET" method))
                                      (expect (= @request path))
                                      {"status" {"is_authenticated" true
                                                 "source" "auth-file"
                                                 "oauth_token_preview" "sk-ant-o..."
                                                 "expires_in_ms" 10859960}})}
                     (fn []
                       (let [status (client/provider-status :anthropic-coding-plan)]
                         (expect (= "/v1/providers/anthropic-coding-plan/status" @request))
                         (expect (every? string? (keys status)))
                         (expect (true? (get status "is_authenticated")))
                         (expect (= "auth-file" (get status "source")))
                         (expect (= "sk-ant-o..." (get status "oauth_token_preview")))
                         (expect (= 10859960 (get status "expires_in_ms")))))))))

(defn- run-serving!
  "Drive ensure-gateway-serving! with a scripted `probe-route` (a seq of results,
   consumed left-to-right) and a scripted `status`. Records how many times the
   destructive stop-daemon! / await-daemon-down! fired."
  [{:keys [probes status]}]
  (let [probe-seq
        (atom probes)

        stops
        (atom 0)

        awaits
        (atom 0)]

    (with-redefs-fn {(rv 'ensure-gateway!) (fn [& _]
                                             fake-entry)
                     (rv 'probe-route) (fn [_ _]
                                         (let [[p] @probe-seq]
                                           (swap! probe-seq rest)
                                           p))
                     (rv 'status) (fn []
                                    status)
                     (rv 'stop-daemon!) (fn []
                                          (swap! stops inc)
                                          {:status "stopping"})
                     (rv 'await-daemon-down!) (fn [_ _ _]
                                                (swap! awaits inc)
                                                true)
                     (rv 'db-target) (fn []
                                       :fake-db)}
      (fn []
        (let [result (try {:entry (client/ensure-gateway-serving! "/ui")}
                          (catch clojure.lang.ExceptionInfo e {:ex (ex-data e)}))]
          (assoc result
            :stops @stops
            :awaits @awaits))))))

(defdescribe served-route-returns-without-restart
             (it "a mounted route is used as-is; the daemon is never touched"
                 (let [{:keys [entry stops]} (run-serving! {:probes [:served]})]
                   (expect (= fake-entry entry))
                   (expect (zero? stops) "no destructive restart when the route is served"))))

(defdescribe
  transport-blip-never-force-kills
  (it
    ":unreachable (connection reset/timeout on the probe) is NOT a 404 —
            we retreat to leaving the daemon alone rather than force-restarting it"
    (let [{:keys [entry stops]} (run-serving! {:probes [:unreachable]})]
      (expect (= fake-entry entry))
      (expect (zero? stops) "a transport blip must never trigger a restart"))))

(defdescribe
  idle-daemon-with-missing-route-is-restarted
  (it
    "a real 404 on an IDLE daemon (no other clients, no running turn) respawns:
            stop → await-down → re-ensure → re-probe :served"
    (let [{:keys [entry stops awaits ex]} (run-serving! {:probes [:absent :served]
                                                         :status {"clients" 1 "running_turns" 0}})]
      (expect (nil? ex))
      (expect (= fake-entry entry))
      (expect (= 1 stops) "the idle stale daemon is stopped exactly once")
      (expect (= 1 awaits) "and we wait for it to go down before respawning"))))

(defdescribe busy-daemon-is-not-force-killed
             (it "a real 404 on a daemon OTHER clients depend on is refused, not nuked"
                 (let [{:keys [ex stops awaits]}
                       (run-serving! {:probes [:absent] :status {"clients" 2 "running_turns" 0}})]
                   (expect (= :gateway/route-missing-busy (:type ex)))
                   (expect (= 2 (:clients ex)))
                   (expect (zero? stops) "a shared daemon is never stopped")
                   (expect (zero? awaits)))))

(defdescribe running-turn-blocks-restart
             (it "a real 404 while a turn is running is refused — a restart would abort it"
                 (let [{:keys [ex stops]} (run-serving! {:probes [:absent]
                                                         :status {"clients" 1 "running_turns" 1}})]
                   (expect (= :gateway/route-missing-busy (:type ex)))
                   (expect (= 1 (:running-turns ex)))
                   (expect (zero? stops) "an in-flight turn is never force-aborted by the heal"))))

(defdescribe respawn-that-still-404s-throws-route-missing
             (it "if the fresh daemon STILL lacks the route, surface a clear error"
                 (let [{:keys [ex stops]} (run-serving! {:probes [:absent :absent]
                                                         :status {"clients" 1 "running_turns" 0}})]
                   (expect (= :gateway/route-missing (:type ex)))
                   (expect (= 1 stops)))))

(defdescribe port-free?-reflects-a-live-listener
             (it "port-free? is false while something listens, true once released"
                 (let [port-free?
                       (rv 'port-free?)

                       sock
                       (java.net.ServerSocket. 0)

                       port
                       (.getLocalPort sock)]

                   (try (expect (false? (port-free? "127.0.0.1" port)) "occupied port is not free")
                        (finally (.close sock)))
                   ;; macOS can still complete a handshake against a listener closed moments
                   ;; ago, so the port drains asynchronously — exactly why the callers of
                   ;; `port-free?` wait for it instead of reading it once.
                   (expect (true? (loop [deadline (+ (System/currentTimeMillis) 5000)]
                                    (or (port-free? "127.0.0.1" port)
                                        (when (< (System/currentTimeMillis) deadline)
                                          (Thread/sleep 25)
                                          (recur deadline)))))
                           "released port is free"))))

(defdescribe
  sse-event-action-test
  (it "own turn terminal returns the event"
      (expect (= [:terminal {"type" "turn.completed" "turn_id" "t1"}]
                 (client/sse-event-action {"type" "turn.completed" "turn_id" "t1"} "t1"))))
  (it "own turn progress forwards"
      (expect (= :forward
                 (first (client/sse-event-action {"type" "block.output" "turn_id" "t1"} "t1")))))
  (it "a CANCELLED own turn is terminal too — a user stop ends the stream"
      ;; Regression: `turn.cancelled` was missing from the terminal set, so an
      ;; Esc (or a stall force-cancel) left this reader parked on the turn
      ;; forever: the SSE connection never closed, the tab kept spinning, and a
      ;; queued turn draining behind it streamed in under a stream that had
      ;; never ended.
      (let [[action event'] (client/sse-event-action
                              {"type" "turn.cancelled" "turn_id" "t1" "status" "cancelled"}
                              "t1")]
        (expect (= :terminal action))
        (expect (= "cancelled" (get event' "status")))))
  (it "a FAILED own turn is terminal"
      (expect (= :terminal
                 (first (client/sse-event-action {"type" "turn.failed" "turn_id" "t1"} "t1")))))
  (it "own queued record deleted synthesizes a cancelled terminal (no hang)"
      (let [[action event'] (client/sse-event-action {"type" "turn.queued.deleted" "turn_id" "t1"}
                                                     "t1")]
        (expect (= :terminal action))
        (expect (= "cancelled" (get event' "status")))
        (expect (= "turn.completed" (get event' "type")))))
  (it "own queued record sent into the running turn synthesizes a sent terminal (no hang)"
      ;; `→ Send now`: the running turn takes the queued message at its next
      ;; step, so the queued turn never starts. The waiter ends with a receipt
      ;; that names where the message went instead of blocking forever.
      (let [[action event']
            (client/sse-event-action
              {"type" "turn.queued.sent" "turn_id" "t1" "into_turn_id" "r0" "iteration" 2}
              "t1")]
        (expect (= :terminal action))
        (expect (= "turn.completed" (get event' "type")))
        (expect (= "sent" (get event' "status")))
        (expect (= "r0" (get event' "into_turn_id")))
        (expect (= 2 (get event' "iteration")))))
  (it "a SIBLING turn's queue lifecycle events forward (cross-TUI queue mirror)"
      (doseq [type ["turn.queued" "turn.queued.updated" "turn.queued.deleted" "turn.queued.drained"
                    "turn.queued.sent"]]
        (expect (= :forward (first (client/sse-event-action {"type" type "turn_id" "OTHER"} "t1")))
                type)))
  (it "a sibling turn's non-queue events are dropped"
      (expect (= :skip
                 (first (client/sse-event-action {"type" "block.output" "turn_id" "OTHER"} "t1"))))
      (expect (= :skip
                 (first (client/sse-event-action {"type" "turn.completed" "turn_id" "OTHER"}
                                                 "t1"))))))

(defdescribe
  read-sse-stream!-recovers-a-sent-receipt
  (it "a queued turn the running turn already took ends on subscription.ready from the stored row"
      ;; `turn.queued.sent` is live-only: a reader that subscribes after it fired
      ;; never sees it in the replay. The stored row says `sent`, and the daemon
      ;; registers the subscription before `subscription.ready`, so one lookup on
      ;; that frame recovers the receipt without a race.
      (let [looked-up
            (atom [])

            forwarded
            (atom [])]

        (with-redefs-fn {(rv 'open-sse-events!)
                         (fn [_sid _cursor _cursor* handle]
                           (or (handle {"type" "subscription.ready" "current_turn_id" "r0"})
                               (handle {"type" "block.output" "turn_id" "r0"})
                               [:closed]))
                         #'client/get-turn
                         (fn [sid tid]
                           (swap! looked-up conj [sid tid])
                           {"turn_id" tid "status" "sent" "into_turn_id" "r0" "iteration" 2})}
          (fn []
            (expect (= [:terminal
                        {"type" "turn.completed"
                         "turn_id" "q1"
                         "status" "sent"
                         "into_turn_id" "r0"
                         "iteration" 2}]
                       ((rv 'read-sse-stream!) "s" 0 "q1" #(swap! forwarded conj %) (atom 0))))
            (expect (= [["s" "q1"]] @looked-up))
            (expect (= [] @forwarded) "nothing to repaint: the queued turn never ran")))))
  (it "the daemon's current turn is never looked up and a waiting row keeps reading"
      (let [looked-up
            (atom 0)

            ready-then-eof
            (fn [current-turn-id]
              (fn [_sid _cursor _cursor* handle]
                (or (handle {"type" "subscription.ready" "current_turn_id" current-turn-id})
                    [:closed])))]

        (with-redefs-fn {(rv 'open-sse-events!) (ready-then-eof "q1")
                         #'client/get-turn (fn [_ _]
                                             (swap! looked-up inc)
                                             nil)}
          (fn []
            (expect (= [:closed] ((rv 'read-sse-stream!) "s" 0 "q1" nil (atom 0))))
            (expect (= 0 @looked-up) "the running turn cannot be a sent queued row")))
        (with-redefs-fn {(rv 'open-sse-events!) (ready-then-eof "r0")
                         #'client/get-turn (fn [_ _]
                                             (swap! looked-up inc)
                                             {"turn_id" "q1" "status" "queued"})}
          (fn []
            (expect (= [:closed] ((rv 'read-sse-stream!) "s" 0 "q1" nil (atom 0))))
            (expect (= 1 @looked-up)))))))

(defdescribe
  terminal-event->result-keeps-canonical-nested-maps
  (it
    "the blocking result IS the canonical snake_case string-keyed wire event
           (plus derived fills) — tokens/cost/utilization are never re-keyed"
    (let [t->r
          (rv 'terminal-event->result)

          ;; What `parse-json` yields after the SSE hop: snake_case STRING keys.
          event
          {"type" "turn.completed"
           "turn_id" "t1"
           "session_id" "s1"
           "cost" {"total_cost" 0.0123 "model" "m" "provider" "p"}
           "tokens" {"input" 10 "cached" 4 "output" 2}
           "utilization" {"saturation" 42 "headroom_tokens" 1000}}

          result
          (with-redefs [client/get-turn (fn [_ _]
                                          {"content" [{"id" "b1" "type" "prose" "markdown" "done"}]
                                           "iteration_count" 1})]
            (t->r event "t1"))]

      (expect (= 0.0123 (get-in result ["cost" "total_cost"])) "cost stays canonical")
      (expect (= "m" (get-in result ["cost" "model"])))
      (expect (= 4 (get-in result ["tokens" "cached"])) "token slots stay canonical")
      (expect (= 42 (get-in result ["utilization" "saturation"])) "utilization stays canonical")
      (expect (= "t1" (get result "session_turn_id")))
      (expect (= "done" (get-in result ["content" 0 "markdown"])))
      (expect (not-any? keyword? (keys result)) "no keyword keys survive in the blocking result"))))

(defdescribe
  read-events-until!-surfaces-disconnect
  (it
    "a stream that never reaches a terminal event throws a clear
           gateway-disconnected error (not a silent blank result) after the
           reconnect budget is spent"
    (let [reads (atom 0)]
      (with-redefs-fn {(rv 'read-sse-stream!) (fn [_ _ _ _ _]
                                                (swap! reads inc)
                                                [:closed])
                       (rv 'sse-reconnect-backoff-ms) 0
                       (rv 'sse-reconnect-max-attempts) 2}
        (fn []
          (let [ex (try ((rv 'read-events-until!) "s" 0 "t1" nil)
                        nil
                        (catch clojure.lang.ExceptionInfo e (ex-data e)))]
            (expect (true? (:gateway-disconnected ex)))
            (expect (= 3 @reads) "initial attempt + 2 reconnects")))))))

(defdescribe read-events-until!-reconnects-then-completes
             (it "a dropped stream reconnects and still returns the terminal event"
                 (let [scripted (atom [[:closed]
                                       [:terminal {:type "turn.completed" :turn_id "t1"}]])]
                   (with-redefs-fn {(rv 'read-sse-stream!) (fn [_ _ _ _ _]
                                                             (let [[r] @scripted]
                                                               (swap! scripted rest)
                                                               r))
                                    (rv 'sse-reconnect-backoff-ms) 0}
                     (fn []
                       (expect (= {:type "turn.completed" :turn_id "t1"}
                                  ((rv 'read-events-until!) "s" 0 "t1" nil))))))))

(defdescribe
  read-events-until!-reconnects-on-http-status
  (it
    "a non-200 mid-turn (502/503 while the daemon restarts) is treated as a
           drop and reconnected, same as an EOF — not rethrown as a bare error"
    (let [reads (atom 0)]
      (with-redefs-fn {(rv 'read-sse-stream!)
                       (fn [_ _ _ _ _]
                         (if (< @reads 2)
                           (do (swap! reads inc)
                               (throw (ex-info "gateway SSE HTTP 503" {:http-status 503})))
                           (do (swap! reads inc)
                               [:terminal {:type "turn.completed" :turn_id "t1"}])))
                       (rv 'sse-reconnect-backoff-ms) 0}
        (fn []
          (expect (= {:type "turn.completed" :turn_id "t1"}
                     ((rv 'read-events-until!) "s" 0 "t1" nil)))
          (expect (= 3 @reads) "two 503 reconnects + the completing read"))))))

(defdescribe read-events-until!-rethrows-non-http-ex-info
             (it "an ex-info WITHOUT :http-status is not swallowed as a drop"
                 (with-redefs-fn {(rv 'read-sse-stream!) (fn [_ _ _ _ _]
                                                           (throw (ex-info "boom" {:kaboom true})))
                                  (rv 'sse-reconnect-backoff-ms) 0}
                   (fn []
                     (let [ex (try ((rv 'read-events-until!) "s" 0 "t1" nil)
                                   nil
                                   (catch clojure.lang.ExceptionInfo e (ex-data e)))]
                       (expect (true? (:kaboom ex))))))))

(defdescribe mux-advance-cursor!-honours-the-subscription-ready-echo
             (it "mux advance cursor! honours the subscription ready echo"
                 (let [advance! (rv 'mux-advance-cursor!)]
                   ;; an ordinary frame advances the cursor monotonically
                   (let [cursor (atom 10)]
                     (advance! cursor {"type" "turn.delta" "seq" 12})
                     (expect (= 12 @cursor))
                     (advance! cursor {"type" "turn.delta" "seq" 11})
                     (expect (= 12 @cursor) "a late lower seq never rewinds a live cursor"))
                   ;; subscription.ready OVERRIDES the max, so a renumbered daemon heals
                   ;; A restarted gateway numbers from its journal high-water, far below the
                   ;; cursor this client carried across the outage. Keeping the max would ask
                   ;; for a cursor above the session's high-water on EVERY reconnect, the
                   ;; server would clamp it, and this session would never replay again.
                   (let [cursor (atom 4200)]
                     (advance! cursor {"type" "subscription.ready" "cursor" 7})
                     (expect (= 7 @cursor) "the echoed resume point wins outright")
                     (advance! cursor {"type" "turn.completed" "seq" 8})
                     (expect (= 8 @cursor)
                             "and the renumbered stream is delivered and tracked from there"))
                   ;; a ready frame with no usable cursor leaves the cursor alone
                   (let [cursor (atom 5)]
                     (advance! cursor {"type" "subscription.ready"})
                     (advance! cursor {"type" "subscription.ready" "cursor" nil})
                     (expect (= 5 @cursor))))))

(defdescribe
  mux-subscribe!-shares-one-remote-session-subscription
  (it
    "multiple local listeners for one sid do not reconnect/open one SSE per tab"
    (let [mux-var
          (rv 'mux)

          restarts
          (atom 0)

          seen-a
          (atom [])

          seen-b
          (atom [])]

      (reset! @mux-var {:subs {} :epoch 0 :future nil :stream nil})
      (with-redefs-fn {(rv 'restart-mux!) (fn []
                                            (swap! restarts inc)
                                            nil)}
        (fn []
          (let [cleanup-a
                (client/mux-subscribe! "sid-1" #(swap! seen-a conj %) 10)

                cleanup-b
                (client/mux-subscribe! "sid-1" #(swap! seen-b conj %) 10)

                entry
                (get-in @@mux-var [:subs "sid-1"])]

            (expect (= 1 @restarts) "second listener for same sid should not reopen /v1/events")
            (expect (= 2 (count (:sinks entry))))
            (doseq [[_ sink] (:sinks entry)]
              (sink {:type "turn.started" :session_id "sid-1" :seq 11}))
            (expect (= [{:type "gateway.connected"}
                        {:type "turn.started" :session_id "sid-1" :seq 11}]
                       @seen-b)
                    "new same-sid listener gets connection state and live events")
            (expect (= [{:type "turn.started" :session_id "sid-1" :seq 11}] @seen-a))
            (cleanup-a)
            (expect (= 1 @restarts) "dropping one of two listeners leaves the remote mux alone")
            (expect (= 1 (count (get-in @@mux-var [:subs "sid-1" :sinks]))))
            (cleanup-b)
            (expect (= 2 @restarts) "only the last listener removal changes the remote session set")
            (expect (empty? (:subs @@mux-var)))))))))

(defdescribe
  fleet-subscribe!-rides-one-stream-instead-of-asking-per-session
  (it
    "a session LIST watches the fleet feed and opens no per-session route"
    (let
      [calls
       (atom [])

       seen
       (atom [])

       frames
       (str
         "data: {\"type\":\"session.status\",\"session_id\":\"a\",\"is_live\":true}\n\n"
         "data: {\"type\":\"session.title_updated\",\"session_id\":\"a\",\"title\":\"named\"}\n\n")]

      (with-redefs-fn {(rv 'ensure-gateway!) (fn []
                                               fake-entry)
                       (rv 'ensure-client!) (fn [_]
                                              nil)
                       (rv 'ensure-release-hook!) (fn []
                                                    nil)
                       (rv 'gw-send!)
                       (fn [_ method path _]
                         (swap! calls conj [method path])
                         {:status 200
                          :body (java.io.ByteArrayInputStream.
                                  (.getBytes frames java.nio.charset.StandardCharsets/UTF_8))})}
        (fn []
          (let [stop! (client/fleet-subscribe! (fn [frame]
                                                 (swap! seen conj frame)))]
            (try (loop [waited 0]
                   (when (and (< (count @seen) 2) (< waited 2000))
                     (Thread/sleep 10)
                     (recur (+ waited 10))))
                 (finally (stop!)))
            (expect (= ["GET" "/v1/events?scope=fleet"] (first @calls)))
            (expect (= ["session.status" "session.title_updated"]
                       (mapv #(get % "type") (take 2 @seen))))
            (let [after-stop (count @calls)]
              (Thread/sleep 400)
              (expect (= after-stop (count @calls))
                      "stopping ends the watch instead of reconnecting"))))))))

(defdescribe
  mux-finalization-barrier-forbids-new-subscriptions
  (it "mux finalization barrier forbids new subscriptions"
      (let [mux-var
            (rv 'mux)

            finalizing-var
            (rv 'client-finalizing?)

            previous-mux
            @@mux-var

            previous-finalizing
            @@finalizing-var]

        (try (reset! @mux-var {:subs {} :epoch 0 :future nil :stream nil})
             (reset! @finalizing-var true)
             (with-redefs-fn {(rv 'ensure-release-hook!)
                              (fn []
                                (throw (ex-info "must not install during finalization" {})))
                              (rv 'restart-mux!)
                              (fn []
                                (throw (ex-info "must not restart during finalization" {})))}
               (fn []
                 (let [cleanup (client/mux-subscribe! "sid-final"
                                                      (fn [_])
                                                      0)]
                   (expect (fn? cleanup))
                   (expect (empty? (:subs @@mux-var)))
                   (cleanup))))
             (finally (reset! @mux-var previous-mux)
                      (reset! @finalizing-var previous-finalizing))))))

(defdescribe restart-mux-never-starts-a-reader-during-finalization
             (it "restart mux never starts a reader during finalization"
                 (let [mux-var
                       (rv 'mux)

                       finalizing-var
                       (rv 'client-finalizing?)

                       previous-mux
                       @@mux-var

                       previous-finalizing
                       @@finalizing-var

                       starts
                       (atom 0)]

                   (try (reset! @mux-var {:subs {"sid-final" {:cursor-atom (atom 0)
                                                              :sinks {"sub" (fn [_])}}}
                                          :epoch 0
                                          :future nil
                                          :stream nil})
                        (reset! @finalizing-var true)
                        (with-redefs-fn {(rv 'mux-run!) (fn [_]
                                                          (swap! starts inc)
                                                          (future nil))}
                          (fn []
                            ((rv 'restart-mux!))
                            (expect (zero? @starts))
                            (expect (nil? (:future @@mux-var)))))
                        (finally (reset! @mux-var previous-mux)
                                 (reset! @finalizing-var previous-finalizing))))))

(defdescribe
  shutdown-subscriptions-closes-the-mux-without-reconnect
  (it "shutdown subscriptions closes the mux without reconnect"
      (let [closes
            (atom 0)

            pending
            (java.util.concurrent.FutureTask. ^java.util.concurrent.Callable
                                              (fn []
                                                nil))

            stream
            (reify
              java.io.Closeable
                (close [_] (swap! closes inc)))

            state
            (atom {:subs {"sid" {:cursor-atom (atom 0)
                                 :sinks {"sub" (fn [_])}}}
                   :epoch 0
                   :future pending
                   :stream stream})

            finalizing
            (atom false)]

        (with-redefs-fn {(rv 'mux) state (rv 'client-finalizing?) finalizing}
          (fn []
            ((rv 'shutdown-subscriptions!))
            (expect (true? @finalizing))
            (expect (= {:subs {} :epoch 1 :future nil :stream nil} @state))
            (expect (.isCancelled pending))
            ((rv 'shutdown-subscriptions!))
            (expect (= 1 @closes)))))))

(defdescribe
  list-resources-cached-never-blocks-the-caller
  (it "list resources cached never blocks the caller"
      ;; REGRESSION: the footer calls this on the render thread every frame. The
      ;; daemon round-trip MUST run in the background so a busy/slow daemon can't
      ;; stall painting. A cold read returns the last-known value (nil) instantly
      ;; and kicks a single-flight refresh; once it lands, subsequent reads are
      ;; served from cache. If someone reintroduces a synchronous round-trip this
      ;; test blocks for `slow-ms` and the timing assertion fails.
      (let [slow-ms
            300

            cache
            (rv 'resources-cache)

            inflight
            (rv 'resources-refreshing)

            calls
            (atom 0)]

        (with-redefs-fn {(rv 'list-resources) (fn [_sid]
                                                (swap! calls inc)
                                                (Thread/sleep slow-ms)
                                                [{"id" "bg"}])}
          (fn []
            (reset! @cache {})
            (reset! @inflight #{})
            (let [t0
                  (System/nanoTime)

                  cold
                  (client/list-resources-cached "sid-x")

                  cold-ms
                  (/ (- (System/nanoTime) t0) 1e6)]

              (expect (nil? cold) "cold read serves the last-known value (nil) immediately")
              (expect (< cold-ms 50.0) "cold read must NOT block on the daemon round-trip")
              ;; several stale reads while the fetch is in flight stay single-flight
              (dotimes [_ 5]
                (client/list-resources-cached "sid-x"))
              (await-value #(client/list-resources-cached "sid-x") [{"id" "bg"}])
              (expect (= 1 @calls) "only ONE background fetch runs per sid (single-flight)")
              (let [t1
                    (System/nanoTime)

                    warm
                    (client/list-resources-cached "sid-x")

                    warm-ms
                    (/ (- (System/nanoTime) t1) 1e6)]

                (expect (= [{"id" "bg"}] warm) "a fresh entry is served from cache")
                (expect (< warm-ms 50.0) "warm read is a pure cache hit")
                (expect (empty? @@inflight) "the in-flight slot is released after the fetch"))))))))

(defdescribe
  session-model-cached-never-blocks-the-caller
  (it "session model cached never blocks the caller"
      ;; REGRESSION (issue #29, gateway leg): the footer reads the session's model
      ;; pref every frame. This used to be a LIVE daemon round-trip per frame; it
      ;; must serve from a per-sid cache and refresh in the background — same
      ;; discipline as `list-resources-cached` above.
      (let [slow-ms
            300

            cache
            (rv 'session-model-cache)

            inflight
            (rv 'session-model-refreshing)

            calls
            (atom 0)]

        (with-redefs-fn {(rv 'session-model) (fn [_sid]
                                               (swap! calls inc)
                                               (Thread/sleep slow-ms)
                                               {:provider "anthropic" :model "opus"})}
          (fn []
            (reset! @cache {})
            (reset! @inflight #{})
            (let [t0
                  (System/nanoTime)

                  cold
                  (client/session-model-cached "sid-m")

                  cold-ms
                  (/ (- (System/nanoTime) t0) 1e6)]

              (expect (nil? cold) "cold read serves the last-known value (nil) immediately")
              (expect (< cold-ms 50.0) "cold read must NOT block on the daemon round-trip")
              ;; several stale reads while the fetch is in flight stay single-flight
              (dotimes [_ 5]
                (client/session-model-cached "sid-m"))
              (await-value #(client/session-model-cached "sid-m")
                           {:provider "anthropic" :model "opus"})
              (expect (= 1 @calls) "only ONE background fetch runs per sid (single-flight)")
              (let [t1
                    (System/nanoTime)

                    warm
                    (client/session-model-cached "sid-m")

                    warm-ms
                    (/ (- (System/nanoTime) t1) 1e6)]

                (expect (= {:provider "anthropic" :model "opus"} warm)
                        "a fresh entry is served from cache")
                (expect (< warm-ms 50.0) "warm read is a pure cache hit")
                (expect (empty? @@inflight) "the in-flight slot is released after the fetch"))))))))

(defdescribe set-session-model!-writes-through-the-session-model-cache
             (it "set session model! writes through the session model cache"
                 ;; A pick made in THIS client must show on the very next footer frame, not
                 ;; after the cache TTL expires.
                 (let [cache (rv 'session-model-cache)]
                   (with-redefs-fn {(rv 'send-json!) (fn [method path body]
                                                       (expect (= "PATCH" method))
                                                       (expect (= "/v1/sessions/sid-w/model" path))
                                                       {"model" {"provider" (:provider body)
                                                                 "model" (:model body)}})}
                     (fn []
                       (reset! @cache {})
                       (expect (= {:provider "zai" :model "glm"}
                                  (client/set-session-model! "sid-w" "zai" "glm")))
                       (expect (= {:provider "zai" :model "glm"} (:val (get @@cache "sid-w")))
                               "the PATCHed pref lands in the footer cache immediately"))))))

(defdescribe setting-actions-proxy-to-the-daemon
             (it "setting actions proxy to the daemon"
                 (let [calls (atom [])]
                   (with-redefs-fn {(rv 'send-json!)
                                    (fn [method path body]
                                      (swap! calls conj [method path body])
                                      (if (= "cycle" (:action body))
                                        {"id" (:id body) "type" "enum" "value" "deep"}
                                        {"id" (:id body) "type" "boolean" "enabled" true}))}
                     (fn []
                       (expect (= {"id" "shell" "type" "boolean" "enabled" true}
                                  (client/toggle-setting! "shell")))
                       (expect (= {"id" "reasoning_level" "type" "enum" "value" "deep"}
                                  (client/cycle-setting! "reasoning_level")))
                       (expect (= [["POST" "/v1/settings" {:id "shell" :action "toggle"}]
                                   ["POST" "/v1/settings" {:id "reasoning_level" :action "cycle"}]]
                                  @calls)))))))

(defdescribe
  provider-models-proxies-to-daemon-catalog-route
  (it
    "provider-models asks the DAEMON for the catalog instead of building a token-resolving router client-side"
    (let [request (atom nil)]
      (with-redefs-fn {(rv 'ensure-gateway-serving!) (fn [path]
                                                       (reset! request path)
                                                       fake-entry)
                       (rv 'ensure-client!) (constantly "client-id")
                       (rv 'send-json-with-entry!) (fn [_ method path]
                                                     (expect (= "GET" method))
                                                     (expect (= @request path))
                                                     {"models" ["claude-opus-4-8" "claude-sonnet-5"]
                                                      "hidden_count" 3})}
        (fn []
          (let [r (client/provider-models :anthropic-coding-plan false)]
            (expect (= "/v1/providers/anthropic-coding-plan/models" @request))
            (expect (= ["claude-opus-4-8" "claude-sonnet-5"] (:models r)))
            (expect (= 3 (:hidden-count r))))
          (client/provider-models :anthropic-coding-plan true)
          (expect (= "/v1/providers/anthropic-coding-plan/models?show_all=true" @request)))))))

(defdescribe
  set-router-default-proxies-and-decodes-the-explicit-pair
  (it "set router default proxies and decodes the explicit pair"
      (let [request (atom nil)]
        (with-redefs-fn {(rv 'ensure-gateway-serving!) (constantly fake-entry)
                         (rv 'ensure-client!) (constantly "client-id")
                         (rv 'send-json-with-entry!) (fn [_ method path body]
                                                       (reset! request [method path body])
                                                       {"default_provider" "anthropic-coding-plan"
                                                        "default_model" "claude-fable-5"})}
          (fn []
            (expect (= {:provider-id :anthropic-coding-plan :model "claude-fable-5"}
                       (client/set-router-default! :anthropic-coding-plan "claude-fable-5")))
            (expect
              (= ["PATCH" "/v1/router"
                  {"role" "primary" "provider" "anthropic-coding-plan" "model" "claude-fable-5"}]
                 @request)
              "the primary tag is explicit on the wire, so the daemon never guesses the role"))))))

(defdescribe
  set-router-fallback-tags-and-clears-the-second-root
  (it "a fallback tag rides the SAME route under role=fallback and decodes the fallback_* pair"
      (let [request (atom nil)]
        (with-redefs-fn {(rv 'ensure-gateway-serving!) (constantly fake-entry)
                         (rv 'ensure-client!) (constantly "client-id")
                         (rv 'send-json-with-entry!) (fn [_ method path body]
                                                       (reset! request [method path body])
                                                       {"default_provider" "anthropic-coding-plan"
                                                        "default_model" "claude-fable-5"
                                                        "fallback_provider" "zai-coding-plan"
                                                        "fallback_model" "glm-5.2"})}
          (fn []
            (expect (= {:provider-id :zai-coding-plan :model "glm-5.2"}
                       (client/set-router-fallback! :zai-coding-plan "glm-5.2"))
                    "the FALLBACK pair comes back, never the primary one")
            (expect (= ["PATCH" "/v1/router"
                        {"role" "fallback" "provider" "zai-coding-plan" "model" "glm-5.2"}]
                       @request))))))
  (it "the zero-arity clear sends role=fallback with NO pair and decodes nil"
      (let [request (atom nil)]
        (with-redefs-fn {(rv 'ensure-gateway-serving!) (constantly fake-entry)
                         (rv 'ensure-client!) (constantly "client-id")
                         (rv 'send-json-with-entry!) (fn [_ method path body]
                                                       (reset! request [method path body])
                                                       {"default_provider" "anthropic-coding-plan"
                                                        "default_model" "claude-fable-5"})}
          (fn []
            (expect (nil? (client/set-router-fallback!)))
            (expect (= ["PATCH" "/v1/router" {"role" "fallback"}] @request)))))))

(defn- refusal-ex
  "The ExceptionInfo a refusing daemon produces, with `body` as its raw answer."
  [status body]
  (with-redefs-fn {(rv 'gw-send!) (fn [_ _ _ _]
                                    {:status status :body body})}
    (fn []
      (try ((rv 'send-json-with-entry!) fake-entry "PATCH" "/v1/router" {})
           nil
           (catch clojure.lang.ExceptionInfo e e)))))

(defdescribe
  rejected-requests-surface-the-daemons-own-reason
  (it
    "a 400 whose reason is nested under error.message reaches the caller verbatim, so the TUI dialog explains the refusal instead of printing a bare status"
    (let [e (refusal-ex 400
                        (str "{\"error\":{\"message\":\"Fallback provider must differ "
                             "from the primary provider (anthropic-coding-plan)\"}}"))]
      (expect (some? e))
      (expect (= "Fallback provider must differ from the primary provider (anthropic-coding-plan)"
                 (ex-message e)))
      (expect (= 400 (:http-status (ex-data e))))))
  (it "a flat `message` body still wins"
      (expect (= "flat reason" (ex-message (refusal-ex 400 "{\"message\":\"flat reason\"}")))))
  (it "a reasonless refusal keeps the bare status text"
      (expect (= "gateway HTTP 503" (ex-message (refusal-ex 503 ""))))))

;; The provider dialog fanned out 2×N per-provider probes because no client
;; function handed back the whole fleet's status AND limits from the one
;; /v1/router read that already carries both.
(defdescribe
  router-diagnostics-loads-the-whole-fleet-in-one-call
  (it "router diagnostics loads the whole fleet in one call"
      (let [calls
            (atom 0)

            fleet
            [{"id" "openai"
              "status" {"is_authenticated" true "source" "gateway"}
              "limits" {"provider_id" "openai"
                        "status" "ready"
                        "static" {"rpm" 10}
                        "dynamic" {"limits"
                                   [{"id" "requests" "scope" "account" "is_unlimited" false}]}}}
             {"id" "anthropic" "status" {"is_authenticated" false} "limits" nil}]

            result
            (with-redefs-fn {#'client/router (fn []
                                               (swap! calls inc)
                                               fleet)}
              #(client/router-diagnostics))]

        ;; one gateway read serves every provider
        (expect (= 1 @calls))
        (expect (= #{:openai :anthropic} (set (keys result))))
        ;; status stays verbatim wire strings and limits are engine-shaped
        (expect (= true (get-in result [:openai :status "is_authenticated"])))
        (expect (= false (get-in result [:anthropic :status "is_authenticated"])))
        (expect (= :ready (get-in result [:openai :limits :status])))
        (expect (= {:rpm 10} (get-in result [:openai :limits :static])))
        (expect (= :requests (get-in result [:openai :limits :dynamic :limits 0 :id])))
        (expect (nil? (get-in result [:anthropic :limits]))))))

;;; ── Remote gateway target (`--gateway` / VIS_GATEWAY_URL) ─────────────────────

(defdescribe
  remote-entry-reads-a-host-a-host-port-and-a-url
  (it
    "remote entry reads a host a host port and a url"
    (let [remote-entry (rv 'remote-entry)]
      ;; a bare host is plain HTTP on the standard gateway port
      (expect
        (=
          {:base-url "http://10.0.0.5:7890" :host "10.0.0.5" :port 7890 :secret "tok" :remote? true}
          (remote-entry "10.0.0.5" "tok")))
      ;; an explicit port wins, and https defaults to 443
      (expect (= "http://10.0.0.5:7899" (:base-url (remote-entry "10.0.0.5:7899" nil))))
      (expect (= "https://gateway.example.com:443"
                 (:base-url (remote-entry "https://gateway.example.com/" nil))))
      (expect (= "https://gateway.example.com:8443/vis"
                 (:base-url (remote-entry "https://gateway.example.com:8443/vis/" nil))))
      ;; a blank token is no token: a loopback daemon reached by tunnel needs none
      (expect (nil? (:secret (remote-entry "127.0.0.1:7899" "   "))))
      ;; no url is no remote target
      (expect (nil? (remote-entry "  " "tok")))
      ;; a value that names no host is a user error, never a silent local fallback
      (expect (= :gateway/invalid-remote-url
                 (:type (ex-data (try (remote-entry ":7890" nil)
                                      (catch clojure.lang.ExceptionInfo e e)))))))))

(defdescribe
  remote-target-attaches-without-registry-or-spawn
  (it "remote target attaches without registry or spawn"
      (let [target
            ((rv 'remote-entry) "10.0.0.5:7891" "tok")

            fresh-until
            @(rv 'entry-fresh-until-ns)

            cached
            @(rv 'cached-entry)

            previous-fresh
            @fresh-until

            previous-cached
            @cached]

        (try (reset! fresh-until 0)
             (with-redefs-fn {(rv 'remote-gateway) (constantly target)
                              (rv 'probe-entry?) (constantly true)
                              (rv 'assert-compatible!) identity
                              #'discovery/discover-or-start!
                              (fn [& _]
                                (throw (AssertionError. "a remote gateway must never be spawned")))
                              #'discovery/read-registry
                              (fn [& _]
                                (throw (AssertionError. "a remote gateway has no local registry")))}
               (fn []
                 (expect (= target (client/ensure-gateway!)))))
             (finally (reset! fresh-until previous-fresh) (reset! cached previous-cached))))))

(defdescribe remote-request-carries-the-bearer-token-and-claims-no-pid
             (it "remote request carries the bearer token and claims no pid"
                 (let [captured
                       (atom nil)

                       target
                       ((rv 'remote-entry) "10.0.0.5:7891" "tok")

                       capture
                       (fn [request]
                         (reset! captured request)
                         {:status 200 :body "{}"})]

                   (with-redefs-fn {#'http/request capture}
                     (fn []
                       ((rv 'gw-send!) target "GET" "/healthz" {})
                       ;; a gateway on another machine is reached at its own base url
                       (expect (= "http://10.0.0.5:7891/healthz" (:uri @captured)))
                       ;; one secret, both carriers
                       (expect (= "Bearer tok" (get-in @captured [:headers "Authorization"])))
                       (expect (= "tok" (get-in @captured [:headers "X-Vis-Gateway-Secret"])))
                       ;; no pid: this process owns none on the gateway's machine
                       (expect (nil? (get-in @captured [:headers "X-Vis-Client-Pid"])))
                       ((rv 'gw-send!) fake-entry "GET" "/healthz" {})
                       ;; the locally managed daemon still gets the pid its lease reaper needs
                       (expect (= (str (discovery/current-pid))
                                  (get-in @captured [:headers "X-Vis-Client-Pid"]))))))))

(defdescribe tokenless-remote-probe-accepts-an-auth-free-gateway
             (it "tokenless remote probe accepts an auth free gateway"
                 (let [handshake
                       @(rv 'gateway-handshake*)

                       previous
                       @handshake

                       body
                       "{\"status\":\"ok\",\"secret_match\":false}"]

                   (try (with-redefs-fn {(rv 'gw-send!) (fn [& _]
                                                          {:status 200 :body body})}
                          (fn []
                            ;; a token-less target cannot match a secret and does not need to
                            (expect (true? ((rv 'probe-entry?)
                                             ((rv 'remote-entry) "127.0.0.1:7899" nil))))
                            ;; the local daemon must still prove it owns our registry secret
                            (expect (false? ((rv 'probe-entry?) fake-entry)))))
                        (finally (reset! handshake previous))))))

(defdescribe remote-client-lease-carries-no-pid
             (it "remote client lease carries no pid"
                 (let [client-id-atom
                       @(rv 'client-id)

                       previous
                       @client-id-atom

                       captured
                       (atom nil)

                       ensure-client
                       (rv 'ensure-client!)

                       register
                       (fn [_entry _method _path body]
                         (reset! captured body)
                         {"client_id" "cid"})]

                   (try (with-redefs-fn {(rv 'send-json-with-entry!) register}
                          (fn []
                            (reset! client-id-atom nil)
                            (ensure-client (assoc fake-entry :remote? true))
                            (expect (= {:kind "clojure-client"} @captured))
                            (reset! client-id-atom nil)
                            (ensure-client fake-entry)
                            (expect (= {:kind "clojure-client" :pid (discovery/current-pid)}
                                       @captured))))
                        (finally (reset! client-id-atom previous))))))

(defdescribe a-remote-gateway-is-never-stopped-from-here
             (it "a remote gateway is never stopped from here"
                 (with-redefs-fn {(rv 'remote-gateway) (constantly
                                                         ((rv 'remote-entry) "10.0.0.5" "tok"))}
                   (fn []
                     (expect (= :gateway/remote-target
                                (:type (ex-data (try (client/stop-daemon!)
                                                     (catch clojure.lang.ExceptionInfo e e))))))))))

;; Regression (reported: `vis-agent gateway stop` answered with the raw wire map):
;; the stop route replies in JSON, so a body handed back unconverted matched none of
;; the keyword branches its callers read.
(defdescribe
  an-acknowledged-stop-is-answered-in-this-clients-vocabulary
  (it "an acknowledged stop is answered in this clients vocabulary"
      (let [result (with-redefs-fn {(rv 'db-target) (constantly "/tmp/ack/vis.db")
                                    (rv 'remote-gateway) (constantly nil)
                                    #'discovery/read-registry (constantly fake-entry)
                                    #'discovery/registry-fresh? (constantly true)
                                    (rv 'send-json-with-entry!)
                                    (fn [& _]
                                      {"stopping" true
                                       "status" {"pid" 32379 "clients" 4 "running_turns" 0}})}
                     (fn []
                       (client/stop-daemon!)))]
        (expect (true? (:stopping result)))
        (expect (= "stopping" (:status result)))
        (expect (= 32379 (:pid result)))
        (expect (= 4 (:clients result)))
        (expect (= 0 (:running-turns result)))
        (expect (not-any? string? (keys result)) "no wire key survives into the client's answer"))))

;; Regression: a daemon on a NEWER release than this build refused nothing, bounced
;; nothing and said nothing, so an update that was already installed and serving
;; sessions was invisible to the human running the older half.
(defdescribe
  newer-daemon-is-reported-once-test
  (it "newer daemon is reported once"
      (let [line
            (rv 'newer-daemon-line)

            report!
            (rv 'report-newer-daemon!)

            reported?
            @(rv 'newer-daemon-reported?)

            ahead
            {:behind "client" :gateway-version "0.2.22" :client-version "0.2.21"}]

        ;; the line names both releases and what installs the newer one
        (let [text (line ahead)]
          (expect (str/includes? text "0.2.22"))
          (expect (str/includes? text "0.2.21"))
          (expect (str/includes? text "vis-agent update")))
        ;; nothing is said where this build is not the half that is behind
        (expect (nil? (line {:behind nil :gateway-version "0.2.22" :client-version "0.2.22"})))
        (expect (nil? (line
                        {:behind "gateway" :gateway-version "0.2.21" :client-version "0.2.22"})))
        ;; a client says it once, however many times it attaches
        (reset! reported? false)
        (try (expect (some? (report! ahead)))
             (expect (nil? (report! ahead)) "the second attach repeats nothing")
             (finally (reset! reported? false))))))

(defdescribe submit-turn-sync-reads-a-rejected-body-under-json-names
             (it "submit turn sync reads a rejected body under json names"
                 ;; Issue #291: a gateway body arrives JSON-keyed, so a rejected submission is
                 ;; read under "error" and "message" only.
                 (with-redefs-fn {#'client/submit-turn! (fn [_ _]
                                                          {"error" "busy"
                                                           "message" "Session is busy."})}
                   (fn []
                     (let [failure (try (client/submit-turn-sync! "sid" {})
                                        nil
                                        (catch clojure.lang.ExceptionInfo e e))]
                       (expect (= "Session is busy." (ex-message failure)))
                       (expect (= "busy" (get (ex-data failure) "error"))))))))
