(ns com.blockether.vis.internal.gateway.server-test
  (:require
    [lazytest.core :refer [defdescribe expect it throws?]]
    [babashka.http-client :as http]
    [clojure.java.io :as io]
    [clojure.string :as str]
    [com.blockether.vis.contract.gateway :as gateway-contract]
    [com.blockether.vis.internal.attachment.audio-transcribe :as audio-transcribe]
    [com.blockether.vis.internal.attachment.core :as attachments]
    [com.blockether.vis.internal.config.core :as config]
    [com.blockether.vis.internal.decisions.assets :as decision-assets]
    [com.blockether.vis.internal.decisions.cache :as decision-cache]
    [com.blockether.vis.internal.decisions.core :as decision-core]
    [com.blockether.vis.internal.decisions.registry :as decision-registry]
    [com.blockether.vis.internal.decisions.jobs :as decision-jobs]
    [com.blockether.vis.internal.extension.core :as extension]
    [com.blockether.vis.internal.external-opener :as external-opener]
    [com.blockether.vis.internal.foundation.mcp.core :as mcp-core]
    [com.blockether.vis.internal.gateway.client :as client]
    [com.blockether.vis.internal.gateway.discovery :as discovery]
    [com.blockether.vis.internal.gateway.server :as server]
    [com.blockether.vis.internal.gateway.server.council :as council-api]
    [com.blockether.vis.internal.gateway.server.fs :as fs-api]
    [com.blockether.vis.internal.gateway.server.http :as gw-http]
    [com.blockether.vis.internal.gateway.server.instance :as instance]
    [com.blockether.vis.internal.gateway.server.mcp :as mcp-api]
    [com.blockether.vis.internal.gateway.server.projects :as projects-api]
    [com.blockether.vis.internal.gateway.server.providers :as providers-api]
    [com.blockether.vis.internal.gateway.server.sessions :as sessions-api]
    [com.blockether.vis.internal.gateway.server.settings :as settings-api]
    [com.blockether.vis.internal.gateway.server.speech :as speech-api]
    [com.blockether.vis.internal.gateway.server.transcripts :as transcripts-api]
    [com.blockether.vis.internal.gateway.server.turns :as turns-api]
    [com.blockether.vis.internal.gateway.server.views :as views-api]
    [com.blockether.vis.internal.gateway.server.web :as web]
    [com.blockether.vis.internal.gateway.state :as state]
    [com.blockether.vis.internal.gateway.view :as gw-view]
    [com.blockether.vis.internal.gateway.wiring :as wiring]
    [com.blockether.vis.contract.wire :as wire]
    [com.blockether.vis.internal.gateway.server.transport.sse :as sse]
    [com.blockether.vis.internal.persistance.core]
    [com.blockether.vis.internal.provider.catalog :as catalog]
    [com.blockether.vis.internal.provider.limits :as provider-limits]
    [com.blockether.vis.internal.provider.service :as providers]
    [com.blockether.vis.internal.gateway.resources :as resources]
    [com.blockether.vis.internal.python.extensions :as python-extensions]
    [com.blockether.vis.internal.channel.slash :as slash]
    [com.blockether.vis.internal.workspace.core :as workspace]
    [com.blockether.vis.internal.channel.file-picker :as file-picker]
    [com.blockether.vis.internal.config.toggles :as toggles]
    [com.blockether.vis.internal.speech.files :as speech-files]
    [com.blockether.vis.internal.speech.core :as speech]
    [com.blockether.vis.internal.loop :as lp]
    [com.blockether.vis.internal.loop.environment :as loop-env]
    [com.blockether.vis.internal.loop.router :as loop-router]
    [com.blockether.vis.internal.util :as util]
    [reitit.ring :as rr]
    [ring.adapter.jetty9 :as jetty]
    [ring.core.protocols :as ring-protocols]
    [ring.middleware.params :as ring-params]))

(wiring/install!)

(defdescribe
  file-suggestions-use-the-session-workspace-and-requested-limit
  (it "file suggestions use the session workspace and requested limit"
      ;; /pick-file needs more candidates than the inline @ overlay, in the same workspace.
      (let [sid
            (random-uuid)

            calls
            (atom [])

            handler
            #'sessions-api/suggest-handler]

        (with-redefs [state/soul
                      (fn [id]
                        (when (= sid id) {"id" (str sid)}))

                      state/session-workspace-info
                      (fn [id]
                        (when (= sid id) {"root" "/tmp/session-files"}))

                      file-picker/fuzzy-file-rows
                      (fn [q opts]
                        (swap! calls conj [(str (workspace/cwd)) q (:limit opts)])
                        [{:path "report.pdf"
                          :size-label "9B"
                          :age-label "now"
                          :status-label "clean"}])]

          (doseq [[limit expected] [[nil 20] ["1000" 1000] ["5000" 1000] ["0" 1] ["bad" 20]]]
            (let [response (handler {:path-params {:sid (str sid)}
                                     :query-params (cond-> {"kind" "file" "q" "report"}
                                                     limit
                                                     (assoc "limit" limit))})]
              (expect (= 200 (:status response)))
              (expect (= ["/tmp/session-files" "report" expected] (last @calls)))
              (expect (str/includes? (:body response) "\"name\":\"report.pdf\""))))
          (reset! calls [])
          (expect (= 404 (:status (handler {:path-params {:sid (str (random-uuid))}})))))
        (with-redefs [state/soul
                      (constantly {"id" (str sid)})

                      state/session-workspace-info
                      (constantly nil)

                      file-picker/fuzzy-file-rows
                      (fn [& _]
                        (swap! calls conj :unexpected-scan)
                        [])]

          (expect (= 409 (:status (handler {:path-params {:sid (str sid)}})))))
        (expect (empty? @calls)))))

(defn- rv
  "Resolve a (possibly private) var in the server namespace for with-redefs-fn."
  [sym]
  (ns-resolve 'com.blockether.vis.internal.gateway.server sym))

(defdescribe events-since-encodes-canonical-events-without-renormalizing
             (it "events since encodes canonical events without renormalizing"
                 (let [sid
                       (random-uuid)

                       event
                       (wire/canonical {:seq 1
                                        :type "content.delta"
                                        :nested {:tool-name "shell" :ratio (/ 0.0 0.0)}})

                       handler
                       (rv 'events-since-handler)]

                   (with-redefs [state/events-since
                                 (fn [id cursor replay]
                                   (expect (= sid id))
                                   (expect (= 0 cursor))
                                   (expect (= :settled replay))
                                   [event])

                                 wire/->wire
                                 (fn [_]
                                   (throw (ex-info "reconverted" {})))]

                     (let [response (handler {:path-params {:sid (str sid)}})]
                       (expect (= 200 (:status response)))
                       (expect (= "application/json" (get-in response [:headers "Content-Type"])))
                       (expect (= {"events" [event]} (wire/parse-json (:body response)))))))))

(defdescribe sse-replay-test
             (it "replays settled turns by lifecycle unless the client asks for the full stream"
                 (let [replay (rv 'sse-replay)]
                   (expect (= :settled (replay {:query-params {}})))
                   (expect (= :settled (replay {:query-params {"replay" "settled"}})))
                   (expect (= :full (replay {:query-params {"replay" "full"}})))
                   (expect (= :full (replay {:query-params {"replay" " full "}}))))))

(defn- server-state [] instance/server-state)

(defn- with-server-state!
  [m f]
  (let [state-atom (server-state)]
    (reset! state-atom m)
    (try (f) (finally (reset! state-atom nil)))))

(defn- with-stop-stub!
  [stops extra f]
  (with-redefs-fn (merge {#'state/running-turn-count (constantly 0)
                          #'server/stop! (fn []
                                           (swap! stops inc))}
                         extra)
    f))

(defn- wait-until
  [pred]
  (loop [remaining 20]
    (cond (pred) true
          (zero? remaining) false
          :else (do (Thread/sleep 10) (recur (dec remaining))))))

(defdescribe orphan-health-probe-does-not-repair-the-registry
             (it "orphan health probe does not repair the registry"
                 (let [repairs
                       (atom 0)

                       health-handler
                       (rv 'health-handler)]

                   (with-redefs-fn {(rv 'ensure-self-registered!) #(swap! repairs inc)}
                     (fn []
                       (health-handler {:headers {"x-vis-suppress-registry-recovery" "true"}})
                       (expect (zero? @repairs))
                       (health-handler {:headers {}})
                       (expect (= 1 @repairs)))))))

(defdescribe openapi-json-is-public-and-revalidates
             (it "openapi json is public and revalidates"
                 (let [handler
                       (rv 'openapi-handler)

                       {:keys [status headers body]}
                       (handler {:headers {}})

                       etag
                       (get headers "ETag")]

                   (expect (= 200 status))
                   (expect (= "application/json" (get headers "Content-Type")))
                   ;; the document describes the built-in routes, itself included
                   (let [document (wire/parse-json body)]
                     (expect (= "3.1.1" (get document "openapi")))
                     (expect (= (count gateway-contract/route-table)
                                (count (get document "paths"))))
                     (expect (contains? (get document "paths") "/openapi.json")))
                   ;; the bytes are fixed, so a holder of the validator is answered 304
                   (expect (some? etag))
                   (expect (= 304 (:status (handler {:headers {"if-none-match" etag}}))))
                   (expect (= etag (get-in (handler {:headers {}}) [:headers "ETag"])))
                   ;; a token-gated gateway still answers it, like the docs site
                   (with-server-state!
                     {:require-token? true :token "secret"}
                     (fn []
                       (let [gated ((rv 'wrap-auth) (constantly {:status 200}) "secret" [])]
                         (expect (= 200 (:status (gated {:uri "/openapi.json" :headers {}}))))
                         (expect (= 401 (:status (gated {:uri "/v1/models" :headers {}})))))))
                   ;; and answers a client whose protocol this gateway cannot serve
                   (expect (contains? @(rv 'protocol-open-uris) "/openapi.json")))))

(defdescribe json-errors-use-the-contract-envelope
             (it
               "json errors use the contract envelope"
               (let [seen
                     (atom nil)

                     response
                     (with-redefs [gateway-contract/error-body (fn [type message extra]
                                                                 (reset! seen [type message extra])
                                                                 {"contract_error" true})]
                       (gw-http/error-response 409 :example/problem "cannot continue" :detail 7))]

                 (expect (= 409 (:status response)))
                 (expect (= "application/json" (get-in response [:headers "Content-Type"])))
                 (expect (= {"contract_error" true} (wire/parse-json (:body response))))
                 (expect (= [:example/problem "cannot continue" {:detail 7}] @seen)))))

(defdescribe
  gateway-router-reaches-every-contract-route
  (it "gateway router reaches every contract route"
      (let [match-by-path
            (requiring-resolve 'reitit.core/match-by-path)

            router
            ((rv 'router) "test-token" [])]

        ;; each contract route answers its own path with a handler for every method
        (doseq [{:keys [path operations]}
                gateway-contract/route-table

                :let [match
                      (match-by-path router
                                     (-> path
                                         (str/replace #"/:[^/]+" "/value-1")
                                         (str/replace #"/\*[^/]+" "/nested/value")))]]

          (expect (= path (:template match)) path)
          (doseq [method (keys operations)]
            (expect (some? (get-in match [:data method :handler])) (str (name method) " " path))))
        ;; a fixed segment wins over the path parameter it conflicts with
        (doseq [path ["/v1/improve/review" "/v1/improve/settings" "/v1/projects/overview"]]
          (expect (= path (:template (match-by-path router path))))))))

(defdescribe
  built-in-routes-refuse-to-drift-from-the-contract
  (it "built in routes refuse to drift from the contract"
      (let [built-in-routes
            (rv 'built-in-routes)

            handlers
            @(rv 'route-handlers)

            drift
            (fn [handler-maps]
              (try (built-in-routes handler-maps)
                   nil
                   (catch clojure.lang.ExceptionInfo e (ex-data e))))]

        ;; the shipped handler maps bind every contract operation exactly once
        (expect (= (count gateway-contract/route-table) (count (built-in-routes handlers))))
        ;; an operation the contract does not declare
        (expect (= [[:get "/v1/not-in-contract"]]
                   (:undeclared (drift (conj handlers {[:get "/v1/not-in-contract"] identity})))))
        ;; a contract operation left without a handler
        (expect (= [[:get "/healthz"]]
                   (:unbound (drift [(dissoc (apply merge handlers) [:get "/healthz"])]))))
        ;; an operation bound in two handler maps
        (expect (= [[:get "/healthz"]]
                   (:repeated (drift (conj handlers {[:get "/healthz"] identity}))))))))

(defdescribe contributed-routes-cannot-shadow-contract-routes
             (it "contributed routes cannot shadow contract routes"
                 (let [router
                       (rv 'router)

                       contribution
                       (fn [path]
                         {:routes (fn [_token]
                                    [[path {:get (constantly {:status 204})}]])})]

                   ;; a contributed route beside the contract routes compiles and matches
                   (expect (= "/ext/example/ping"
                              (:template ((requiring-resolve 'reitit.core/match-by-path)
                                           (router "test-token"
                                                   [(contribution "/ext/example/ping")])
                                           "/ext/example/ping"))))
                   ;; a contributed route with the same path as a contract route fails the build
                   (expect (throws? clojure.lang.ExceptionInfo
                                    #(router "test-token" [(contribution "/healthz")])))
                   ;; a contributed fixed path under a contract path parameter fails the build
                   (expect (throws? clojure.lang.ExceptionInfo
                                    #(router "test-token" [(contribution "/v1/projects/mine")]))))))

(defdescribe contributed-browser-routes-bypass-api-protocol-headers-only-where-declared
             (it "contributed browser routes bypass api protocol headers only where declared"
                 (let [calls
                       (atom 0)

                       handler
                       ((rv 'wrap-protocol)
                         (fn [_]
                           (swap! calls inc)
                           {:status 204})
                         [{:protocol-open-uris #{"/tui" "/tui/events"}}])]

                   (expect (= 204 (:status (handler {:uri "/tui" :headers {}}))))
                   (expect (= 204 (:status (handler {:uri "/tui/events" :headers {}}))))
                   (expect (= 426 (:status (handler {:uri "/v1/sessions" :headers {}}))))
                   (expect (= 2 @calls)))))

(defdescribe webhook-route-is-open-without-the-gateway-token
             (it "lets a webhook through without the gateway token and the protocol headers"
                 (with-server-state!
                   {:require-token? true :token "secret"}
                   (fn []
                     (let [gated ((rv 'wrap-auth) (constantly {:status 200}) "secret" [])]
                       (expect (= 200 (:status (gated {:uri "/v1/hooks/a1" :headers {}}))))
                       (expect (= 401 (:status (gated {:uri "/v1/automations" :headers {}})))))))
                 (let [handler ((rv 'wrap-protocol)
                                 (fn [_]
                                   {:status 204})
                                 [])]
                   (expect (= 204 (:status (handler {:uri "/v1/hooks/a1" :headers {}}))))
                   (expect (= 426 (:status (handler {:uri "/v1/automations" :headers {}})))))))

(defdescribe gateway-stops-runtimes-owned-by-route-contributions
             (it "gateway stops runtimes owned by route contributions"
                 (let [stopped (atom [])]
                   ((rv 'stop-route-contributions!)
                     [{:stop-fn #(swap! stopped conj :first)} {}
                      {:stop-fn #(swap! stopped conj :second)}])
                   (expect (= [:first :second] @stopped)))))

(defdescribe
  contributed-sse-streams-share-the-gateway-client-lifecycle
  (it "contributed sse streams share the gateway client lifecycle"
      (with-server-state!
        {:managed? false :clients {} :sse-clients {}}
        (fn []
          (let [close! (fn [])]
            (expect (true? (server/register-contributed-sse! :browser close!)))
            (expect (identical? close! (get-in @(server-state) [:sse-clients :browser :close!])))
            (server/unregister-contributed-sse! :browser)
            (expect (empty? (:sse-clients @(server-state)))))))))

(defdescribe
  mcp-management-handlers-are-sanitized-and-route-mutations-test
  (it
    "mcp management handlers are sanitized and route mutations"
    (let [body
          {"name" "filesystem" "enabled" true "server" {"transport" "stdio" "command" "npx"}}

          saved
          (atom nil)

          enabled
          (atom nil)

          deleted
          (atom nil)]

      (with-redefs-fn {#'gw-http/body-json (constantly body)
                       #'mcp-core/gateway-servers (constantly {"servers" [{"name" "filesystem"
                                                                           "transport" "stdio"
                                                                           "enabled" true
                                                                           "is_connected" false
                                                                           "tools" 0}]})
                       #'mcp-core/save-gateway-server!
                       (fn [name spec]
                         (reset! saved [name spec])
                         {"name" name "transport" "stdio" "enabled" true})
                       #'mcp-core/set-gateway-server-enabled! (fn [name value]
                                                                (reset! enabled [name value])
                                                                {"name" name "enabled" value})
                       #'mcp-core/delete-gateway-server! (fn [name]
                                                           (reset! deleted name)
                                                           {"name" name "is_deleted" true})
                       #'mcp-core/test-gateway-server!
                       (fn [name _spec]
                         {"name" name "is_connected" true "tools" [{"name" "list_files"}]})}
        (fn []
          (let [listed (wire/parse-json (:body (#'mcp-api/mcp-servers-handler {})))]
            (expect (= 200 (:status (#'mcp-api/mcp-servers-handler {}))))
            (expect (= "filesystem" (get-in listed ["servers" 0 "name"])))
            (expect (nil? (get-in listed ["servers" 0 "env"]))))
          (expect (= 200 (:status (#'mcp-api/save-mcp-server-handler {}))))
          (expect (= ["filesystem" {"transport" "stdio" "command" "npx"}] @saved))
          (expect (= 200
                     (:status (#'mcp-api/set-mcp-server-enabled-handler
                               {:path-params {:name "filesystem"}}))))
          (expect (= ["filesystem" true] @enabled))
          (expect (= 200
                     (:status (#'mcp-api/delete-mcp-server-handler
                               {:path-params {:name "filesystem"}}))))
          (expect (= "filesystem" @deleted))
          (expect (= 200 (:status (#'mcp-api/test-mcp-server-handler {})))))))))

(defdescribe
  mcp-management-refuses-hand-written-servers-test
  (it "a server declared in a user config file answers 409, never a silent success"
      (let [refuse (fn [name]
                     (throw (ex-info "declared in a hand-written config file"
                                     {:type :mcp/not-managed :server name})))]
        (with-redefs-fn {#'gw-http/body-json (constantly {"enabled" false})
                         #'mcp-core/set-gateway-server-enabled! (fn [name _enabled]
                                                                  (refuse name))
                         #'mcp-core/delete-gateway-server! (fn [name]
                                                             (refuse name))
                         #'mcp-core/save-gateway-server! (fn [name _spec]
                                                           (refuse name))}
          (fn []
            (doseq [handler [#'mcp-api/set-mcp-server-enabled-handler
                             #'mcp-api/delete-mcp-server-handler #'mcp-api/save-mcp-server-handler]]
              (let [response (handler {:path-params {:name "team"}})]
                (expect (= 409 (:status response)))
                (expect (= "not-managed"
                           (get-in (wire/parse-json (:body response)) ["error" "type"]))))))))))

(defdescribe submit-turn-handler-forwards-provider-and-model
             (it "submit turn handler forwards provider and model"
                 (let [sid
                       (random-uuid)

                       submitted
                       (atom nil)

                       body
                       {"request" "next"
                        "provider" "anthropic"
                        "model" "claude-opus-5"
                        "reasoning_default" "deep"}]

                   (with-redefs-fn {#'gw-http/body-json (constantly body)
                                    #'state/submit-turn! (fn [actual opts]
                                                           (reset! submitted [actual opts])
                                                           {:turn {:turn_id "turn-1"}})}
                     #(let [response
                            (#'turns-api/submit-turn-handler {:path-params {:sid (str sid)}})]
                        (expect (= 202 (:status response))) (expect (= sid (first @submitted)))
                        (expect (= "anthropic" (get-in @submitted [1 :provider])))
                        (expect (= "claude-opus-5" (get-in @submitted [1 :model]))))))))

(defdescribe
  binary-upload-is-resolved-at-turn-submit
  (it
    "binary upload is resolved at turn submit"
    (let [sid
          (random-uuid)

          body-bytes
          (.getBytes "binary-image" "UTF-8")

          submitted
          (atom nil)]

      (with-redefs-fn {#'state/soul (constantly {:id sid})
                       #'state/submit-turn! (fn [_ opts]
                                              (reset! submitted opts)
                                              {:turn {:turn_id "turn-1"}})}
        #(let [uploaded
               (#'turns-api/upload-attachment-handler
                {:path-params {:sid (str sid)}
                 :query-params {"filename" "shot.png" "media_type" "image/png"}
                 :headers {"content-length" (str (alength body-bytes))}
                 :body (java.io.ByteArrayInputStream. body-bytes)}) upload-id
               (get (wire/parse-json (:body uploaded)) "upload_id") response
               (#'turns-api/submit-turn-handler
                {:path-params {:sid (str sid)}
                 :body (java.io.ByteArrayInputStream.
                         (.getBytes (wire/json-str {:request "inspect"
                                                    :attachments [{:upload_id upload-id
                                                                   :filename "shot.png"
                                                                   :reference "[IMAGE #3]"
                                                                   :media_type "image/png"
                                                                   :size (alength body-bytes)}]})
                                    "UTF-8"))}) attachment (first (:attachments @submitted))]
           (expect (= 201 (:status uploaded))) (expect (= 202 (:status response)))
           (expect (= "shot.png" (:filename attachment))) (expect (= "[IMAGE #3]"
                                                                     (:reference attachment)))
           (expect (= "image/png" (:media-type attachment))) (expect
                                                               (= "binary-image"
                                                                  (String.
                                                                    (.decode
                                                                      (java.util.Base64/getDecoder)
                                                                      ^String (:base64 attachment))
                                                                    "UTF-8"))))))))

(defdescribe
  inline-attachments-and-workspace-enter-in-engine-shape
  (it "inline attachments and workspace enter in engine shape"
      ;; #291: the body's JSON keys convert once at intake, so gateway and engine
      ;; readers see one spelling.
      (let [sid
            (random-uuid)

            submitted
            (atom nil)]

        (with-redefs-fn {#'state/soul (constantly {:id sid})
                         #'state/submit-turn! (fn [_ opts]
                                                (reset! submitted opts)
                                                {:turn {:turn_id "turn-1"}})}
          #(let [response
                 (#'turns-api/submit-turn-handler
                  {:path-params {:sid (str sid)}
                   :body (java.io.ByteArrayInputStream.
                           (.getBytes (wire/json-str {:request "inspect"
                                                      :workspace {:root "/tmp/project"}
                                                      :attachments [{:filename "shot.png"
                                                                     :media_type "image/png"
                                                                     :base64 "QUJD"
                                                                     :reference "[IMAGE #1]"}]})
                                      "UTF-8"))})] (expect (= 202 (:status response)))
             (expect (= [{:filename "shot.png"
                          :media-type "image/png"
                          :base64 "QUJD"
                          :reference "[IMAGE #1]"}]
                        (:attachments @submitted))) (expect (= {:root "/tmp/project"}
                                                               (:workspace @submitted))))))))

(defdescribe
  markdown-upload-survives-turn-intake
  (it
    "markdown upload survives turn intake"
    (let [sid
          (random-uuid)

          payload
          (.getBytes "# Shared notes\n\n- café\n" "UTF-8")

          prepared
          (atom nil)]

      (with-redefs-fn {#'state/soul (constantly {:id sid})
                       #'state/submit-turn! (fn [_ opts]
                                              (reset! prepared
                                                (attachments/prepare-inline-attachments
                                                  (:attachments opts)))
                                              {:turn {:turn_id "turn-markdown"}})
                       #'state/turn-attachments (fn [actual-sid tid]
                                                  (expect (= sid actual-sid))
                                                  (expect (= "turn-markdown" tid))
                                                  (wire/canonical (:attached @prepared)))}
        (fn []
          (let [uploaded
                (#'turns-api/upload-attachment-handler
                 {:path-params {:sid (str sid)}
                  :query-params {"filename" "notes.md" "media_type" "text/plain"}
                  :body (java.io.ByteArrayInputStream. payload)})

                upload-id
                (get (wire/parse-json (:body uploaded)) "upload_id")

                response
                (#'turns-api/submit-turn-handler
                 {:path-params {:sid (str sid)}
                  :body (java.io.ByteArrayInputStream.
                          (.getBytes (wire/json-str {:request "Read these notes"
                                                     :attachments [{:upload_id upload-id}]})
                                     "UTF-8"))})

                attachment
                (first (:attached @prepared))]

            (expect (= 201 (:status uploaded)))
            (expect (= 202 (:status response)))
            (expect (empty? (:skipped @prepared)))
            (expect (= "notes.md" (:filename attachment)))
            (expect (= "text/markdown" (:media-type attachment)))
            (expect (= (.encodeToString (java.util.Base64/getEncoder) payload)
                       (:base64 attachment)))
            (expect (attachments/hidden-from-model? attachment))
            (let [downloaded
                  (#'turns-api/turn-attachments-handler
                   {:path-params {:sid (str sid) :tid "turn-markdown"}})

                  returned
                  (get-in (wire/parse-json (:body downloaded)) ["attachments" 0])]

              (expect (= 200 (:status downloaded)))
              (expect (= "notes.md" (get returned "filename")))
              (expect (= "text/markdown" (get returned "media_type")))
              (expect (= (:base64 attachment) (get returned "base64"))))))))))

(defdescribe
  audio-upload-starts-transcription-before-turn-submit
  (it
    "audio upload starts transcription before turn submit"
    (let [sid
          (random-uuid)

          body-bytes
          (.getBytes "audio-bytes" "UTF-8")

          calls
          (atom 0)

          submitted
          (atom nil)]

      (with-redefs-fn {#'state/soul (constantly {:id sid})
                       #'audio-transcribe/request-attachments!
                       (fn [rows]
                         (let [n (swap! calls inc)]
                           (mapv #(cond-> (assoc % :transcription-status "pending") (= n 2)
                                    (assoc :transcription "ready words"))
                                 rows)))
                       #'state/submit-turn! (fn [_ opts]
                                              (reset! submitted opts)
                                              {:turn {:turn_id "turn-audio"}})}
        #(let [uploaded
               (#'turns-api/upload-attachment-handler
                {:path-params {:sid (str sid)}
                 :query-params {"filename" "memo.m4a" "media_type" "audio/mp4"}
                 :headers {"content-length" (str (alength body-bytes))}
                 :body (java.io.ByteArrayInputStream. body-bytes)}) upload-id
               (get (wire/parse-json (:body uploaded)) "upload_id") response
               (#'turns-api/submit-turn-handler
                {:path-params {:sid (str sid)}
                 :body (java.io.ByteArrayInputStream.
                         (.getBytes (wire/json-str {:request "listen"
                                                    :attachments [{:upload_id upload-id}]})
                                    "UTF-8"))}) attachment (first (:attachments @submitted))]
           (expect (= 201 (:status uploaded))) (expect (= 202 (:status response))) (expect
                                                                                     (= 2 @calls))
           (expect (= "ready words" (:transcription attachment))))))))

(defdescribe list-turns-status-filter-routes-to-queued-overlay
             (it "list turns status filter routes to queued overlay"
                 (let [sid
                       (random-uuid)

                       calls
                       (atom [])

                       request
                       {:path-params {:sid (str sid)}}]

                   (with-redefs-fn {#'state/soul (fn [actual]
                                                   (= sid actual))
                                    #'state/list-queued-turns (fn [actual]
                                                                (swap! calls conj [:queued actual])
                                                                [])
                                    #'state/list-turns (fn [actual]
                                                         (swap! calls conj [:all actual])
                                                         [])}
                     #(do (expect (= 200
                                     (:status (#'turns-api/list-turns-handler
                                               (assoc request :query-params {"status" "queued"})))))
                          (expect (= [[:queued sid]] @calls))
                          (reset! calls [])
                          (expect (= 200 (:status (#'turns-api/list-turns-handler request))))
                          (expect (= [[:all sid]] @calls)))))))

;; Regression, Vis session 57dfea5e-0c2d-4190-a82c-0e1992e352c3: a client that
;; missed queue.paused could recover the queued rows but not the paused marker, so it
;; had no way to continue work after the provider recovered.
(defdescribe queued-turn-poll-includes-paused-state
             (it "queued turn poll includes paused state"
                 (let [sid
                       (random-uuid)

                       request
                       {:path-params {:sid (str sid)} :query-params {"status" "queued"}}]

                   (with-redefs-fn {#'state/soul (constantly {:id sid})
                                    #'state/list-queued-turns (constantly [{:turn_id "waiting"}])
                                    #'state/queue-paused-info (constantly {:reason "turn_failed"
                                                                           :held 1})}
                     #(let [body (:body (#'turns-api/list-turns-handler request))]
                        (expect (str/includes? body "\"queue_paused\""))
                        (expect (str/includes? body "\"turn_failed\"")))))))

(defdescribe
  soul-handler-optionally-includes-queued-turns
  (it
    "soul handler optionally includes queued turns"
    (let [sid
          (random-uuid)

          calls
          (atom [])

          request
          {:path-params {:sid (str sid)}}]

      (with-redefs-fn {#'state/soul (fn [actual]
                                      (when (= sid actual) {:id sid}))
                       #'state/list-queued-turns (fn [actual]
                                                   (swap! calls conj actual)
                                                   [{:turn_id "queued-1"}])
                       #'state/queue-paused-info (constantly {:reason "turn_failed" :held 1})}
        #(do (let [response (#'sessions-api/soul-handler
                             (assoc request :query-params {"include" "queued"}))]
               (expect (= 200 (:status response)))
               (expect (re-find #"\"id\"" (:body response)))
               (expect (re-find #"\"queued_turns\"" (:body response)))
               (expect (re-find #"\"queue_paused\"" (:body response)))
               (expect (re-find #"\"turn_id\":\"queued-1\"" (:body response))))
             (expect (= [sid] @calls))
             (reset! calls [])
             (doseq [plain-request [request
                                    (assoc request :query-params {"include" "anything-else"})]]
               (let [response (#'sessions-api/soul-handler plain-request)]
                 (expect (= 200 (:status response)))
                 (expect (not (re-find #"\"queued_turns\"" (:body response))))))
             (expect (empty? @calls))
             (let [response (#'sessions-api/soul-handler
                             {:path-params {:sid (str (random-uuid))}
                              :query-params {"include" "queued"}})]
               (expect (= 404 (:status response))))
             (expect (empty? @calls)))))))

(defdescribe foreground-daemon-does-not-refcount-stop
             (it "a manually-run `vis-agent gateway start` is user-owned, not client-refcounted"
                 (let [stops (atom 0)]
                   (with-stop-stub! stops
                                    {}
                                    (fn []
                                      (with-server-state! {:managed? false
                                                           :saw-client? true
                                                           :started-at-ms (System/currentTimeMillis)
                                                           :clients {}
                                                           :sse-clients {}}
                                                          (fn []
                                                            ((rv 'maybe-stop-when-idle!))
                                                            (Thread/sleep 80)
                                                            (expect (zero? @stops)))))))))

(defdescribe managed-daemon-stops-when-last-client-is-gone
             (it "an auto-spawned gateway self-reaps once there are no clients and no turn"
                 (let [stops (atom 0)]
                   (with-stop-stub! stops
                                    {}
                                    (fn []
                                      (with-server-state! {:managed? true
                                                           :saw-client? true
                                                           :started-at-ms (System/currentTimeMillis)
                                                           :clients {}
                                                           :sse-clients {}}
                                                          (fn []
                                                            ((rv 'maybe-stop-when-idle!))
                                                            (expect (wait-until #(= 1
                                                                                    @stops))))))))))

(defdescribe closing-one-terminal-preserves-the-other-client
             (it "closing one terminal preserves the other client"
                 (let [stops (atom 0)]
                   (with-stop-stub! stops
                                    {}
                                    (fn []
                                      (with-server-state!
                                        {:managed? true
                                         :saw-client? true
                                         :started-at-ms (System/currentTimeMillis)
                                         :clients {"other-terminal"
                                                   {:pid (.pid (java.lang.ProcessHandle/current))}}
                                         :sse-clients {}}
                                        (fn []
                                          ((rv 'maybe-stop-when-idle!))
                                          (Thread/sleep 80)
                                          (expect (zero? @stops))
                                          (swap! instance/server-state assoc :clients {})
                                          ((rv 'maybe-stop-when-idle!))
                                          (expect (wait-until #(= 1 @stops))))))))))

(defdescribe killed-client-lease-does-not-pin-managed-daemon
             (it "dead recorded client pids are reaped, so SIGKILLed TUIs still let the daemon die"
                 (let [stops (atom 0)]
                   (with-stop-stub! stops
                                    {#'discovery/pid-alive-cached? (constantly false)
                                     (rv 'log-client-lease-warning!) (fn [& _])}
                                    (fn []
                                      (with-server-state!
                                        {:managed? true
                                         :saw-client? true
                                         :started-at-ms (System/currentTimeMillis)
                                         :clients {"c1" {:pid 12345 :kind "clojure-client"}}
                                         :sse-clients {}}
                                        (fn []
                                          ((rv 'reap-client-leases!))
                                          ((rv 'maybe-stop-when-idle!))
                                          (expect (wait-until #(= 1 @stops))))))))))

;; Regression (reported: "closing the TUI does not close the gateway"): a TUI that
;; quit released its client lease immediately, but the SSE stream it had open was
;; counted as a separate client until the server's own 15s heartbeat write finally
;; failed - so a managed daemon kept running long after its last TUI was gone, and
;; a pump parked in `.poll` was not woken by closing the socket at all.

(defdescribe killed-client-sse-stream-does-not-pin-managed-daemon
             (it "an SSE stream whose owner process is gone is closed and stops counting"
                 (let [stops
                       (atom 0)

                       closed
                       (atom 0)]

                   (with-stop-stub! stops
                                    {#'discovery/pid-alive-cached? (constantly false)}
                                    (fn []
                                      (with-server-state!
                                        {:managed? true
                                         :saw-client? true
                                         :started-at-ms (System/currentTimeMillis)
                                         :clients {}
                                         :sse-clients {"s1" {:pid 12345
                                                             :close! #(swap! closed inc)}}}
                                        (fn []
                                          ((rv 'reap-sse-clients!))
                                          (expect (= 1 @closed))
                                          (expect (empty? (:sse-clients @(server-state))))
                                          ((rv 'maybe-stop-when-idle!))
                                          (expect (wait-until #(= 1 @stops))))))))))

(defdescribe remote-sse-client-without-a-pid-is-never-reaped
             (it "a phone/browser stream carries no local pid, so liveness is not guessed"
                 (let [stops
                       (atom 0)

                       closed
                       (atom 0)]

                   (with-stop-stub! stops
                                    {#'discovery/pid-alive-cached?
                                     (fn [_]
                                       (throw (ex-info "must not probe" {})))}
                                    (fn []
                                      (with-server-state!
                                        {:managed? true
                                         :saw-client? true
                                         :started-at-ms (System/currentTimeMillis)
                                         :clients {}
                                         :sse-clients {"s1" {:pid nil :close! #(swap! closed inc)}}}
                                        (fn []
                                          ((rv 'reap-sse-clients!))
                                          (expect (zero? @closed))
                                          (expect (= 1 ((rv 'client-count))))
                                          ((rv 'maybe-stop-when-idle!))
                                          (Thread/sleep 80)
                                          (expect (zero? @stops)))))))))

(defn- wait-until-slow
  [pred]
  ;; Twelve seconds, not three: this waits on a real SSE frame crossing a real socket, and the
  ;; whole suite runs on the same machine. A slow bound costs nothing when the frame arrives.
  (loop [remaining 240]
    (cond (pred) true
          (zero? remaining) false
          :else (do (Thread/sleep 50) (recur (dec remaining))))))

;; Regression (reported: quitting the TUI leaves a gateway nobody can use behind):
;; refcount shutdown demanded `running-turn-count` be zero, and a turn whose worker
;; died - or was killed mid-launch, or parked in uninterruptible code - keeps
;; `:current-turn` set forever. The daemon then outlived every client of a turn that
;; would never produce another event, holding its port until a human killed the pid.
(defdescribe a-stalled-turn-with-no-clients-releases-the-daemon
             (it "a turn that stopped producing events cannot pin a daemon nobody watches"
                 (let [stops (atom 0)]
                   (with-stop-stub! stops
                                    {#'state/running-turn-count (constantly 1)
                                     #'state/running-turn-progress (constantly {:turns 1 :seq 7})}
                                    (fn []
                                      (with-server-state!
                                        {:managed? true
                                         :saw-client? true
                                         :started-at-ms (System/currentTimeMillis)
                                         :clients {}
                                         :sse-clients {}}
                                        (fn []
                                          ((rv 'note-turn-progress!))
                                          ;; Age the sample past the stall window instead of
                                          ;; sleeping through it.
                                          (swap! @(rv 'turn-progress-watch) update :since - 120000)
                                          ((rv 'maybe-stop-when-idle!))
                                          (expect (wait-until #(= 1 @stops))))))))))

(defdescribe a-turn-still-producing-events-keeps-the-daemon-alive
             (it "zero clients plus live work is \"I closed the TUI, finish in the background\""
                 (let [stops
                       (atom 0)

                       progress
                       (atom {:turns 1 :seq 1})]

                   (with-stop-stub! stops
                                    {#'state/running-turn-count (constantly 1)
                                     #'state/running-turn-progress (fn []
                                                                     @progress)}
                                    (fn []
                                      (with-server-state!
                                        {:managed? true
                                         :saw-client? true
                                         :started-at-ms (System/currentTimeMillis)
                                         :clients {}
                                         :sse-clients {}}
                                        (fn []
                                          ((rv 'note-turn-progress!))
                                          (swap! @(rv 'turn-progress-watch) update :since - 120000)
                                          ;; One new event is all it takes: the stall clock
                                          ;; restarts, because the turn is demonstrably alive.
                                          (reset! progress {:turns 1 :seq 2})
                                          ((rv 'note-turn-progress!))
                                          ((rv 'maybe-stop-when-idle!))
                                          (Thread/sleep 80)
                                          (expect (zero? @stops)))))))))

;; Regression (reported: a gateway with no clients that never goes away): the reap
;; loop caught throws OUTSIDE the loop and cleared its own handle in `finally`, and
;; nothing re-armed it. One throw from any sweep therefore ended lease reaping, SSE
;; reaping and refcount shutdown for the life of the process - the daemon could no
;; longer stop itself at all.
(defdescribe a-throwing-reap-sweep-does-not-make-the-daemon-immortal
             (it "one bad sweep loses that sweep, never the lifecycle"
                 (let [stops
                       (atom 0)

                       sweeps
                       (atom 0)]

                   (reset! @(rv 'idle-reaper) nil)
                   (with-stop-stub!
                     stops
                     {(rv 'ensure-self-registered!) (constantly nil)
                      (rv 'reap-sse-clients!) (constantly nil)
                      (rv 'reap-client-leases!) (fn []
                                                  (swap! sweeps inc)
                                                  (throw (ex-info "reap exploded" {})))}
                     (fn []
                       (with-server-state! {:managed? true
                                            :saw-client? true
                                            :started-at-ms (System/currentTimeMillis)
                                            :clients {}
                                            :sse-clients {}}
                                           (fn []
                                             ((rv 'ensure-idle-reaper!))
                                             (expect (wait-until-slow #(pos? @stops))
                                                     "shutdown is still evaluated after a throw")
                                             (expect (pos? @sweeps)))))))))

(defdescribe closing-an-sse-stream-unblocks-a-pump-parked-on-the-heartbeat
             (it "the writer must exit at once, not at the next 15s keepalive"
                 (let [out
                       (java.io.ByteArrayOutputStream.)

                       queue
                       (java.util.concurrent.ArrayBlockingQueue. 8)

                       dead?
                       (volatile! false)

                       unsubscribed
                       (atom 0)

                       close!
                       ((rv 'sse-closer) out queue dead? #(swap! unsubscribed inc))

                       pump
                       (future ((rv 'pump-sse!)
                                 out
                                 queue
                                 dead?
                                 (fn [_])))]

                   (Thread/sleep 50)
                   (close!)
                   (expect (not= ::timeout (deref pump 2000 ::timeout)))
                   (expect @dead?)
                   (expect (= 1 @unsubscribed)))))

(defdescribe sse-owner-pid-is-read-from-the-client-header
             (it "only a local vis client sends X-Vis-Client-Pid; anything else owns no pid"
                 (let [client-pid (rv 'request-client-pid)]
                   (expect (= 4242 (client-pid {:headers {"x-vis-client-pid" "4242"}})))
                   (expect (nil? (client-pid {:headers {"x-vis-client-pid" "phone"}})))
                   (expect (nil? (client-pid {:headers {}}))))))

(defdescribe
  gateway-requests-carry-the-client-pid-header
  (it "the daemon can only reap a dead owner if every request names its process"
      (let [sent
            (atom nil)

            gw-send!
            (ns-resolve 'com.blockether.vis.internal.gateway.client 'gw-send!)]

        (with-redefs-fn {#'http/request (fn [request]
                                          (reset! sent request)
                                          {:status 200 :body "{}"})}
          (fn []
            (gw-send! {:host "127.0.0.1" :port 7890 :secret "s"} "GET" "/v1/events" {:as :stream})
            (expect (= (str (discovery/current-pid)) (get-in @sent [:headers "X-Vis-Client-Pid"])))
            (expect (nil? (get-in @sent [:headers "X-Vis-Client-Id"]))
                    "a process holding no lease has none to name")
            (with-redefs-fn {(ns-resolve 'com.blockether.vis.internal.gateway.client 'client-id)
                             (atom "lease-1")}
              (fn []
                (gw-send! {:host "127.0.0.1" :port 7890 :secret "s"} "GET" "/healthz" {})
                (expect
                  (= "lease-1" (get-in @sent [:headers "X-Vis-Client-Id"]))
                  "the lease id is what lets the daemon see a REMOTE client is still here"))))))))

(defdescribe client-count-is-constant-time-and-does-not-probe-pids
             (it "status reads the already-reaped lease map without OS liveness work"
                 (with-server-state! {:clients {"c1" {:pid 10} "c2" {:pid 11}} :sse-clients #{"s1"}}
                                     (fn []
                                       (with-redefs [discovery/pid-alive-cached?
                                                     (fn [_]
                                                       (throw (ex-info "must not probe" {})))]
                                         (expect (= 3 ((rv 'client-count)))))))))

(defdescribe
  compact-client-leases-removes-dead-and-duplicate-pids
  (it
    "one sweep probes each pid once, keeps a lease that is still talking, and preserves identity when clean"
    (let [checks
          (atom [])

          now
          (System/currentTimeMillis)

          clients
          {"live-old" {:pid 10}
           "live-duplicate" {:pid 10}
           "dead" {:pid 20}
           "browser" {:pid nil :last-seen-at now}}

          compact
          (rv 'compact-client-leases)]

      (with-redefs [discovery/pid-alive-cached? (fn [pid]
                                                  (swap! checks conj pid)
                                                  (= 10 pid))]
        (let [{after :clients :keys [dead duplicates expired]} (compact clients now)]
          (expect (= 1 dead))
          (expect (= 1 duplicates))
          (expect (zero? expired))
          (expect (= #{"browser"} (set (filter #(nil? (get-in after [% :pid])) (keys after)))))
          (expect (= 2 (count after)))
          (expect (= #{10 20} (set @checks))))
        (let [clean {"live" {:pid 10} "browser" {:pid nil :last-seen-at now}}
              {after :clients :keys [dead duplicates expired]} (compact clean now)]

          (expect (identical? clean after))
          (expect (zero? dead))
          (expect (zero? duplicates))
          (expect (zero? expired)))))))

;; Regression: a lease with no pid (a phone, a `--gateway` CLI on another machine)
;; could never be retired, so ONE client that vanished mid-flight pinned the daemon
;; for the life of the machine — `daemon-idle?` answered `:clients` forever and no
;; update could ever replace that build.
(defdescribe a-lease-with-no-pid-is-retired-once-it-stops-talking
             (it "a lease with no pid is retired once it stops talking"
                 (let [now
                       (System/currentTimeMillis)

                       ttl
                       (long instance/CLIENT_LEASE_TTL_MS)

                       compact
                       (rv 'compact-client-leases)]

                   ;; silence past the TTL is the only evidence a remote client leaves behind
                   (let [clients
                         {"phone-gone" {:pid nil :last-seen-at (- now ttl 1)}
                          "phone-here" {:pid nil :last-seen-at (- now 1000)}
                          "local-quiet" {:pid 10 :connected-at (- now ttl ttl)}}

                         {after :clients :keys [dead duplicates expired]}
                         (with-redefs-fn {#'discovery/pid-alive-cached? (constantly true)}
                           (fn []
                             (compact clients now)))]

                     (expect (= 1 expired))
                     (expect (zero? dead))
                     (expect (zero? duplicates))
                     (expect (= #{"phone-here" "local-quiet"} (set (keys after)))
                             "a local process proves itself by being alive, not by talking"))
                   ;; a lease that never recorded a sighting is judged from when it registered
                   (let [{after :clients :keys [expired]}
                         (compact {"old" {:pid nil :connected-at (- now ttl 1)}} now)]
                     (expect (= 1 expired))
                     (expect (empty? after))))))

(defdescribe
  a-request-refreshes-the-lease-of-the-client-that-made-it
  (it "a request refreshes the lease of the client that made it"
      (let [touch
            (rv 'touch-client-lease!)

            granularity
            (long @(rv 'CLIENT_LEASE_TOUCH_MS))

            now
            (System/currentTimeMillis)]

        ;; the header a client stamps on every request is what keeps its lease alive
        (with-server-state!
          {:clients {"c1" {:pid nil :last-seen-at (- now granularity 1)}}}
          (fn []
            (let [handler ((rv 'wrap-client-lease) (constantly {:status 200}))]
              (handler {:uri "/v1/sessions" :headers {"x-vis-client-id" "c1"}})
              (expect (<= now (long (get-in @(server-state) [:clients "c1" :last-seen-at])))))))
        ;; an id this daemon never issued creates nothing
        (with-server-state! {:clients {}}
                            (fn []
                              (touch "forged" now)
                              (expect (empty? (:clients @(server-state))))))
        ;; a lease seen a moment ago is not written again
        (let [fresh (- now 1)]
          (with-server-state! {:clients {"c1" {:pid nil :last-seen-at fresh}}}
                              (fn []
                                (touch "c1" now)
                                (expect (= fresh
                                           (get-in @(server-state) [:clients "c1" :last-seen-at]))
                                        "a busy client must not cost a swap! per request")))))))

(defdescribe
  registering-a-pid-upserts-its-single-process-lease
  (it "re-registration replaces the old opaque id but preserves other processes and browsers"
      (let [register
            (rv 'register-client-lease)

            before
            {"old" {:pid 10} "other" {:pid 20} "browser" {:pid nil}}

            {after :clients :keys [replaced]}
            (register before "new" {:pid 10 :kind "clojure-client"})]

        (expect (= 1 replaced))
        (expect (= #{"new" "other" "browser"} (set (keys after))))
        (expect (= 10 (get-in after ["new" :pid]))))))

(defdescribe
  managed-daemon-gets-startup-grace-before-first-client
  (it "the daemon does not exit in the gap between self-registration and first client lease"
      (let [stops (atom 0)]
        (with-stop-stub! stops
                         {}
                         (fn []
                           (with-server-state! {:managed? true
                                                :saw-client? false
                                                :started-at-ms (System/currentTimeMillis)
                                                :clients {}
                                                :sse-clients #{}}
                                               (fn []
                                                 ((rv 'maybe-stop-when-idle!))
                                                 (Thread/sleep 80)
                                                 (expect (zero? @stops)))))))))

(defn- with-only-direction-engine!
  "Run `f` with exactly `engine` in one direction, without a mutable production registry."
  [direction engine f]
  (let [built-ins speech/engines]
    (with-redefs-fn {#'speech/engines (fn [d]
                                        (if (= d direction)
                                          (cond-> []
                                            engine
                                            (conj engine))
                                          (built-ins d)))
                     #'speech/env-engine-id (constantly nil)}
      f)))

(defn- with-only-engine! [engine f] (with-only-direction-engine! :transcribe engine f))

(defn- with-only-speech-engine! [engine f] (with-only-direction-engine! :synthesize engine f))

(defn- json-body [m] {:body (java.io.ByteArrayInputStream. (.getBytes (wire/json-str m) "UTF-8"))})

(defn- wav-body
  "A RIFF/WAVE header long enough to pass the gateway's cheap pre-filter."
  []
  (let [b (byte-array 64)]
    (System/arraycopy (.getBytes "RIFF" "US-ASCII") 0 b 0 4)
    (System/arraycopy (.getBytes "WAVE" "US-ASCII") 0 b 8 4)
    (java.io.ByteArrayInputStream. b)))

(defdescribe
  reachable-addresses-lead-with-the-advertised-route
  (it
    "--advertise names the route a client must dial, so the live address list
             a paired app re-reads leads with it and the scanned interfaces follow"
    (with-redefs-fn {(ns-resolve 'com.blockether.vis.internal.gateway.pairing 'iface-addresses)
                     (fn []
                       ["100.109.18.77"])
                     (ns-resolve 'com.blockether.vis.internal.gateway.pairing
                                 'discover-default-route)
                     (fn []
                       "192.168.0.1")}
      (fn []
        (reset! @(ns-resolve 'com.blockether.vis.internal.gateway.pairing 'default-route-cache) nil)
        (with-server-state! {:host "0.0.0.0" :port 7890 :advertise "192.168.0.1"}
                            (fn []
                              (expect (= ["http://192.168.0.1:7890" "http://100.109.18.77:7890"
                                          "http://127.0.0.1:7890"]
                                         (#'views-api/reachable-addresses {:scheme :http})))))
        (with-server-state!
          {:host "0.0.0.0" :port 7890}
          (fn []
            (expect
              (= ["http://100.109.18.77:7890" "http://127.0.0.1:7890"]
                 (#'views-api/reachable-addresses {:scheme :http}))
              "the router this machine routes through is not an address it answers on (#277)")))))))

(defdescribe
  reachable-addresses-end-with-loopback-for-a-client-on-this-machine
  (it
    "a wildcard bind answers on loopback too, so the list ends with it: the
             desktop app running on this same machine can stay off the host's LAN
             interface instead of being refused by its firewall (#277)"
    (with-redefs-fn {(ns-resolve 'com.blockether.vis.internal.gateway.pairing 'iface-addresses)
                     (fn []
                       ["100.109.18.77" "192.168.0.150"])
                     (ns-resolve 'com.blockether.vis.internal.gateway.pairing
                                 'discover-default-route)
                     (fn []
                       nil)}
      (fn []
        (reset! @(ns-resolve 'com.blockether.vis.internal.gateway.pairing 'default-route-cache) nil)
        (with-server-state!
          {:host "0.0.0.0" :port 7890}
          (fn []
            (expect (= ["http://100.109.18.77:7890" "http://192.168.0.150:7890"
                        "http://127.0.0.1:7890"]
                       (#'views-api/reachable-addresses {:scheme :http}))
                    "loopback comes last: it is the fallback, not the route to advertise")))
        (with-server-state!
          {:host "192.168.0.150" :port 7890}
          (fn []
            (expect (= ["http://192.168.0.150:7890"]
                       (#'views-api/reachable-addresses {:scheme :http}))
                    "a concrete bind serves that address alone, loopback included")))))))

(defdescribe
  capabilities-advertise-gateway-voice-and-attachment-contract
  (it
    "a gateway without any voice engine reports it honestly"
    (with-only-engine!
      nil
      (fn []
        (let [response
              (#'views-api/capabilities-handler {})

              body
              (wire/parse-json (:body response))]

          (expect (= 200 (:status response)))
          (expect (= 1 (get body "version")))
          (expect (true? (get-in body ["features" "attachments" "enabled"])))
          (expect (= 8 (get-in body ["features" "attachments" "max_files"])))
          ;; INTAKE ceiling (25MB), not the 5MB provider cap: an oversize still is
          ;; squeezed on the way OUT rather than refused at upload.
          (expect (= (* 25 1024 1024) (get-in body ["features" "attachments" "max_file_bytes"])))
          ;; Every sniffable file is advertised to the picker. Recordings, tables,
          ;; JSONL and gzip diagnostics ride beside documents: their bytes stay in
          ;; session storage, and the model is told it can inspect them on demand.
          (expect (= ["image/jpeg" "image/png" "image/gif" "image/webp" "image/bmp"
                      "application/csv" "application/gzip" "application/pdf" "application/x-csv"
                      "application/x-gzip" "application/x-ndjson" "application/xhtml+xml"
                      "text/comma-separated-values" "text/csv" "text/html" "text/markdown"
                      "text/tab-separated-values" "text/tsv" "text/x-csv" "text/x-markdown"
                      "text/x-tsv" "video/mp4" "video/quicktime" "audio/aac" "audio/aiff"
                      "audio/amr" "audio/flac" "audio/mp4" "audio/mpeg" "audio/ogg" "audio/wav"
                      "audio/x-caf"]
                     (get-in body ["features" "attachments" "media_types"])))
          (expect (= ["video/mp4" "video/quicktime"]
                     (get-in body ["features" "attachments" "video_media_types"])))
          (expect (= ["audio/aac" "audio/aiff" "audio/amr" "audio/flac" "audio/mp4" "audio/mpeg"
                      "audio/ogg" "audio/wav" "audio/x-caf"]
                     (get-in body ["features" "attachments" "audio_media_types"])))
          (expect (= (* 32 1024 1024) (get-in body ["features" "attachments" "max_audio_bytes"])))
          (expect (= (* 32 1024 1024) (get-in body ["features" "attachments" "max_video_bytes"])))
          (expect (false? (get-in body ["features" "voice" "enabled"])))
          (expect (= "unavailable" (get-in body ["features" "voice" "model" "status"])))
          (expect (empty? (get-in body ["features" "voice" "engines"])))
          (expect (nil? (get-in body ["features" "voice" "selected"])))))))
  (it "voice advertises the engine CATALOGUE, the selection and the phase vocabulary"
      (with-only-engine!
        {:id :fake-engine
         :label "Fake"
         :transcribe (constantly "hi")
         :model-state (constantly {:state :ready})}
        (fn []
          (let [body (-> (#'views-api/capabilities-handler {})
                         :body
                         wire/parse-json)]
            (expect (true? (get-in body ["features" "voice" "enabled"])))
            (expect (= "audio/wav" (get-in body ["features" "voice" "transport"])))
            (expect (= "ready" (get-in body ["features" "voice" "model" "status"])))
            ;; a client that sees this STREAMS the job's progress instead of
            ;; holding a socket open for a minute or polling for a percentage
            (expect (true? (get-in body ["features" "voice" "is_async"])))
            (expect (= "sse" (get-in body ["features" "voice" "progress"])))
            (expect (= ["uploading" "queued" "preparing" "transcribing" "done" "failed"]
                       (get-in body ["features" "voice" "phases"])))
            (expect (= "fake-engine" (get-in body ["features" "voice" "selected"])))
            (expect (= [{"id" "fake-engine" "label" "Fake"}]
                       (get-in body ["features" "voice" "engines"]))))))))

(defdescribe
  voice-post-accepts-the-recording-and-reports-progress-through-a-job
  (it "voice post accepts the recording and reports progress through a job"
      ;; POST /voice used to BLOCK until the transcript existed: the client could not
      ;; tell "still uploading" from "transcribing", and a long recording was an
      ;; unexplained spinner over a socket that could time out.
      (let [sid
            (str (random-uuid))

            release
            (promise)]

        (with-redefs-fn {#'state/soul (constantly {:session-id sid})}
          (fn []
            (speech/reset-jobs!)
            (with-only-engine!
              {:id :slow
               :label "Slow"
               :transcribe (fn [{:keys [on-progress]}]
                             (on-progress {:phase :transcribing :progress 40})
                             @release
                             "the transcript")}
              (fn []
                (let [accepted
                      (#'speech-api/voice-handler {:path-params {:sid sid} :body (wav-body)})

                      job
                      (wire/parse-json (:body accepted))

                      poll
                      (fn []
                        (wire/parse-json (:body (#'speech-api/voice-job-handler
                                                 {:request-method :get
                                                  :path-params {:sid sid
                                                                :job-id (get job "id")}}))))]

                  ;; the upload is answered immediately with a job, not a transcript
                  (expect (= 202 (:status accepted)))
                  (expect (string? (get job "id")))
                  (expect (= "slow" (get job "engine")))
                  (expect (false? (get job "is_done")))
                  (expect (nil? (get job "text")))
                  ;; the job reports the phase and the percentage while it runs
                  (expect (wait-until #(= "transcribing" (get (poll) "phase"))))
                  (expect (= 40 (get (poll) "progress")))
                  ;; the finished job carries the text
                  (deliver release :go)
                  (expect (wait-until #(true? (get (poll) "is_done"))))
                  (let [done (poll)]
                    (expect (= "done" (get done "phase")))
                    (expect (= 100 (get done "progress")))
                    (expect (= "the transcript" (get done "text"))))
                  ;; a collected job can be forgotten, and an unknown one is a 404
                  (expect (= 200
                             (:status (#'speech-api/voice-job-handler
                                       {:request-method :delete
                                        :path-params {:sid sid :job-id (get job "id")}}))))
                  (expect (= 404
                             (:status (#'speech-api/voice-job-handler
                                       {:request-method :get
                                        :path-params {:sid sid :job-id "vj_nope"}}))))))))))))

(defn- sse-jobs
  "Every `data:` payload of an SSE body, parsed, in the order it was written."
  [body]
  (into []
        (comp (map str/split-lines)
              (mapcat (fn [lines]
                        (filter #(str/starts-with? % "data: ") lines)))
              (map #(wire/parse-json (subs % 6))))
        (str/split body #"\n\n")))

(defdescribe
  voice-job-progress-is-pushed-as-server-sent-events
  (it "voice job progress is pushed as server sent events"
      ;; Progress used to be POLLED: one request per tick, a percentage that was
      ;; already up to a poll interval stale when it was painted, and a client left
      ;; guessing when to stop asking. The job's own stream answers all three.
      (let [sid
            (str (random-uuid))

            release
            (promise)]

        (with-redefs-fn {#'state/soul (constantly {:session-id sid})}
          (fn []
            (speech/reset-jobs!)
            (with-only-engine!
              {:id :slow
               :label "Slow"
               :transcribe (fn [{:keys [on-progress]}]
                             (on-progress {:phase :transcribing :progress 40})
                             @release
                             "the transcript")}
              (fn []
                (let [job-id
                      (get (wire/parse-json (:body (#'speech-api/voice-handler
                                                    {:path-params {:sid sid} :body (wav-body)})))
                           "id")

                      response
                      (#'speech-api/voice-job-events-handler
                       {:request-method :get :path-params {:sid sid :job-id job-id}})

                      out
                      (java.io.ByteArrayOutputStream.)

                      written
                      (fn []
                        (String. (.toByteArray out) "UTF-8"))

                      stream
                      (future (ring-protocols/write-body-to-stream (:body response) response out)
                              (written))]

                  ;; it is an event stream, and no intermediary may buffer it
                  (expect (= 200 (:status response)))
                  (expect (= "text/event-stream" (get-in response [:headers "Content-Type"])))
                  (expect (= "no" (get-in response [:headers "X-Accel-Buffering"])))
                  ;; the percentage reaches the client while the engine is still working
                  (expect (wait-until #(str/includes? (written) "\"transcribing\"")))
                  (deliver release :go)
                  (let [body (deref stream 5000 :timeout)]
                    ;; the stream ENDS itself on the terminal frame - nothing to poll
                    (expect (string? body))
                    (let [jobs (sse-jobs (str body))
                          final (last jobs)]

                      ;; EVERY frame names itself, and none carries a session cursor
                      (let [frames (->> (str/split (str body) #"\n\n")
                                        (remove str/blank?)
                                        ;; `: ping` heartbeats are comments, not frames.
                                        (remove #(str/starts-with? % ":")))]
                        (expect (seq frames))
                        (expect (every? #(str/starts-with? %
                                                           (str "event: "
                                                                gateway-contract/voice-job-event))
                                        frames))
                        (expect (not-any? #(str/includes? % "id: ") frames)))
                      (expect (seq jobs))
                      (expect (= #{job-id} (set (map #(get % "id") jobs))))
                      (expect (contains? (set (map #(get % "phase") jobs)) "transcribing"))
                      (expect (= 40
                                 (apply max
                                   (map #(get % "progress")
                                        (filter #(= "transcribing" (get % "phase")) jobs)))))
                      ;; the last frame IS the result: no follow-up request
                      (expect (= "done" (get final "phase")))
                      (expect (true? (get final "is_done")))
                      (expect (= 100 (get final "progress")))
                      (expect (= "the transcript" (get final "text")))))
                  ;; a job nobody submitted is refused before a stream is opened
                  (expect (= 404
                             (:status (#'speech-api/voice-job-events-handler
                                       {:request-method :get
                                        :path-params {:sid sid :job-id "vj_nope"}}))))))))))))

;; A job frame used to be recognisable only by the SHAPE of its JSON: the event
;; name was hand-written at the one place that emitted it and no client was ever
;; TOLD it, so a consumer of an SSE socket had to guess whether a frame was a
;; session event or a transcription's progress.
(defdescribe
  voice-job-frames-are-named-and-that-name-is-published
  (it "the frame names itself, and carries no session cursor"
      (let [frame (sse/job-sse-frame gateway-contract/voice-job-event
                                     {"id" "vj_1" "phase" "transcribing"})]
        (expect (= "voice.job" gateway-contract/voice-job-event))
        (expect (str/starts-with? frame (str "event: " gateway-contract/voice-job-event "\n")))
        (expect (str/includes? frame "data: {"))
        (expect (str/ends-with? frame "\n\n"))
        ;; `id:` is the SESSION log's replay cursor. A job has no log to replay, so
        ;; a client must never mistake this stream for a resumable one.
        (expect (not (str/includes? frame "id:")))))
  (it "capabilities tell a client the name instead of leaving it to guess"
      (let [voice (-> (#'views-api/capabilities-handler {})
                      :body
                      wire/parse-json
                      (get-in ["features" "voice"]))]
        (expect (= "sse" (get voice "progress")))
        (expect (= gateway-contract/voice-job-event (get voice "progress_event")))
        (expect (true? (get voice "is_async")))))
  (it "the companion filters on that very string, not on a payload's shape"
      (let [ts (slurp "apps/vis-companion/src/lib/gateway.ts")]
        (expect (str/includes?
                  ts
                  (str "export const VOICE_JOB_EVENT = '" gateway-contract/voice-job-event "';")))
        (expect (str/includes? ts "if (event !== VOICE_JOB_EVENT) return;"))
        (expect (str/includes? ts "if (frameName === VOICE_JOB_EVENT) return;")))))

(defdescribe
  voice-refusals-name-the-reason-instead-of-failing-late
  (it
    "voice refusals name the reason instead of failing late"
    (let [sid (str (random-uuid))]
      (with-redefs-fn {#'state/soul (constantly {:session-id sid})}
        (fn []
          ;; no engine at all is 501, not a broken 500
          (with-only-engine! nil
                             (fn []
                               (expect (= 501
                                          (:status (#'speech-api/voice-handler
                                                    {:path-params {:sid sid} :body (wav-body)}))))))
          (with-only-engine! {:id :fake-engine
                              :transcribe (constantly "hi")
                              :model-state (constantly {:state :ready})}
                             (fn []
                               ;; naming an engine nobody registered is the CALLER's 400
                               (expect (= 400
                                          (:status (#'speech-api/voice-handler
                                                    {:path-params {:sid sid}
                                                     :query-params {"engine" "whisper-server"}
                                                     :body (wav-body)}))))
                               ;; a body that is not RIFF/WAVE never reaches the engine
                               (expect (= 400
                                          (:status (#'speech-api/voice-handler
                                                    {:path-params {:sid sid}
                                                     :body (java.io.ByteArrayInputStream.
                                                             (byte-array 64))}))))))
          ;; an engine that is still preparing answers 425 with its own state
          (with-only-engine! {:id :downloading
                              :transcribe (constantly "hi")
                              :model-state (constantly {:state :downloading :progress 42})}
                             (fn []
                               (let [response (#'speech-api/voice-handler
                                               {:path-params {:sid sid} :body (wav-body)})
                                     body (wire/parse-json (:body response))]

                                 (expect (= 425 (:status response)))
                                 (expect (= "engine-not-ready" (get-in body ["error" "type"])))
                                 (expect (= "downloading" (get-in body ["error" "model" "status"])))
                                 (expect (= 42 (get-in body ["error" "model" "progress"])))))))))))

(defn- spoken-wav
  "The bytes a fake synthesis engine writes: a RIFF/WAVE header and then the line itself,
   so a test can tell two clips apart and compare what the gateway SERVED with what the
   engine WROTE."
  ^bytes [text]
  (let [tail
        (.getBytes (str text) "UTF-8")

        out
        (byte-array (+ 64 (alength tail)))]

    (System/arraycopy (.getBytes "RIFF" "US-ASCII") 0 out 0 4)
    (System/arraycopy (.getBytes "WAVE" "US-ASCII") 0 out 8 4)
    (System/arraycopy tail 0 out 64 (alength tail))
    out))

(defn- body-bytes
  "Everything a Ring body carries, whether the handler answered the bytes or the file."
  ^bytes [body]
  (with-open [in
              (io/input-stream body)

              out
              (java.io.ByteArrayOutputStream.)]

    (io/copy in out)
    (.toByteArray out)))

(defn- speaking-engine
  "A synthesis engine that WRITES a file, the way a real one does — the gateway owns it
   from there: deleted after an inline answer, served from the audio route after a job.
   With a `release` promise it blocks mid-synthesis so a stream can be watched."
  ([] (speaking-engine nil))
  ([release]
   {:id :speaker
    :label "Speaker"
    :voices (constantly [{:id :alba :label "Alba" :language :en-GB}])
    :model-state (constantly {:state :ready})
    :synthesize (fn [{:keys [text on-progress]}]
                  (on-progress {:phase :synthesizing :progress 40})
                  (when release @release)
                  (let [f (java.io.File/createTempFile "vis-speech-test" ".wav")]
                    (io/copy (spoken-wav text) f)
                    {:audio-path (str f) :sample-rate 24000}))}))

(defdescribe
  capabilities-advertise-the-speech-catalogue-and-both-thresholds
  (it "capabilities advertise the speech catalogue and both thresholds"
      ;; A picker cannot be populated by trial: the engine, the voices it can speak in, the
      ;; phase vocabulary and the two lengths that decide inline-or-job all arrive in the one
      ;; request a client already makes.
      (with-only-speech-engine!
        (speaking-engine)
        (fn []
          (let [speech (-> (#'views-api/capabilities-handler {})
                           :body
                           wire/parse-json
                           (get-in ["features" "speech"]))]
            (expect (true? (get speech "is_enabled")))
            (expect (= "audio/wav" (get speech "transport")))
            (expect (true? (get speech "is_async")))
            (expect (= "sse" (get speech "progress")))
            (expect (= "speech.job" gateway-contract/speech-job-event))
            (expect (= gateway-contract/speech-job-event (get speech "progress_event")))
            ;; the OTHER direction's working phase is never promised to a client that would
            ;; sit waiting for it
            (expect (= ["uploading" "queued" "preparing" "synthesizing" "done" "failed"]
                       (get speech "phases")))
            (expect (= "speaker" (get speech "selected")))
            (expect (= [{"id" "alba" "label" "Alba" "language" "en-GB"}]
                       (get-in speech ["engines" 0 "voices"])))
            (expect (= 280 (get speech "inline_max_chars")))
            (expect (< (get speech "inline_max_chars") (get speech "max_chars"))))))
      ;; a gateway that cannot speak says so instead of half-advertising it
      (with-only-speech-engine! nil
                                (fn []
                                  (let [speech (-> (#'views-api/capabilities-handler {})
                                                   :body
                                                   wire/parse-json
                                                   (get-in ["features" "speech"]))]
                                    (expect (false? (get speech "is_enabled")))
                                    (expect (= "unavailable" (get-in speech ["model" "status"])))
                                    (expect (empty? (get speech "engines")))
                                    (expect (nil? (get speech "selected"))))))))

 ;; Regression, user report: local speech required an AI provider just to create a conversation.
(defdescribe
  local-speech-routes-need-no-conversation
  (it
    "local speech routes need no conversation"
    (let [app
          (rr/ring-handler ((rv 'router) "token" []))

          paths
          (atom [])

          transport
          (fn [method path & [opts]]
            (swap! paths conj path)
            (let [[uri query]
                  (str/split path #"\?" 2)

                  response
                  (app (merge {:request-method method :uri uri :query-string query}
                              (when-let [body (:body opts)]
                                (if (:raw-body? opts) {:body body} (json-body body)))))

                  out
                  (java.io.ByteArrayOutputStream.)]

              (ring-protocols/write-body-to-stream (:body response) response out)
              (assoc response
                :body (case (:as opts)
                        :stream
                        (java.io.ByteArrayInputStream. (.toByteArray out))

                        :bytes
                        (.toByteArray out)

                        (.toString out "UTF-8")))))]

      (with-redefs [state/soul
                    (fn [_]
                      (throw (ex-info "Session must not be loaded" {})))

                    loop-router/get-router
                    (fn []
                      (throw (ex-info "No AI provider" {})))

                    client/request!
                    transport]

        (with-only-engine!
          {:id :listener
           :label "Listener"
           :transcribe (constantly "hello")
           :model-state (constantly {:state :ready})}
          (fn []
            (with-only-speech-engine!
              (speaking-engine)
              (fn []
                (doseq [text ["hello" (apply str (repeat 60 "hello "))]]
                  (let [audio (client/synthesize-speech! nil text {})]
                    (try (expect (= (seq (spoken-wav (str/trim text))) (seq (body-bytes audio))))
                         (expect (= "hello" (client/transcribe-audio! nil audio {})))
                         (finally (.delete audio)))))
                (expect (every? #(not (str/includes? % "/sessions/")) @paths))
                (expect (some #(str/ends-with? % "/audio") @paths)))))))
      (with-redefs [state/soul (constantly nil)]
        (doseq [sid ["not-a-uuid" (str (random-uuid))]]
          (expect (= 404
                     (:status (app (merge {:request-method :post
                                           :uri (str "/v1/sessions/" sid "/speech")}
                                          (json-body {:text "hello"})))))))))))

(defdescribe
  speech-post-speaks-a-short-line-on-the-same-connection
  (it "speech post speaks a short line on the same connection"
      ;; A spoken acknowledgement that costs a job, a stream and a second fetch is a spinner
      ;; where a sentence should have been.
      (let [sid (str (random-uuid))]
        (with-redefs-fn {#'state/soul (constantly {:session-id sid})}
          (fn []
            (with-only-speech-engine!
              (speaking-engine)
              (fn []
                (let [line "Ready when you are."
                      response (#'speech-api/speech-handler
                                (merge {:request-method :post :path-params {:sid sid}}
                                       ;; padded on purpose: a client that appends a newline must not
                                       ;; make the engine speak the whitespace
                                       (json-body {:text (str "  " line "\n") :voice "alba"})))
                      spoken (body-bytes (:body response))]

                  (expect (= 200 (:status response)))
                  (expect (= "audio/wav" (get-in response [:headers "Content-Type"])))
                  (expect (= (str (alength spoken)) (get-in response [:headers "Content-Length"])))
                  (expect (= "RIFF" (String. (java.util.Arrays/copyOfRange spoken 0 4) "US-ASCII")))
                  (expect (= "WAVE"
                             (String. (java.util.Arrays/copyOfRange spoken 8 12) "US-ASCII")))
                  (expect (java.util.Arrays/equals spoken (spoken-wav line)))))))))))

(defdescribe
  speech-job-streams-its-progress-and-then-serves-the-audio-it-wrote
  (it
    "speech job streams its progress and then serves the audio it wrote"
    ;; The long path proves the whole loop a client actually walks: accept, watch, fetch,
    ;; forget — with the file the gateway owns disappearing at the end of it.
    (let [sid
          (str (random-uuid))

          release
          (promise)

          line
          (str/join " " (repeat 40 "a paragraph worth watching"))]

      (with-redefs-fn {#'state/soul (constantly {:session-id sid})}
        (fn []
          (speech/reset-jobs!)
          (with-only-speech-engine!
            (speaking-engine release)
            (fn []
              (let [accepted
                    (#'speech-api/speech-handler
                     (merge {:request-method :post :path-params {:sid sid}}
                            (json-body {:text line})))

                    job
                    (wire/parse-json (:body accepted))

                    job-id
                    (get job "id")

                    response
                    (#'speech-api/speech-job-events-handler
                     {:request-method :get :path-params {:sid sid :job-id job-id}})

                    out
                    (java.io.ByteArrayOutputStream.)

                    written
                    (fn []
                      (String. (.toByteArray out) "UTF-8"))

                    stream
                    (future (ring-protocols/write-body-to-stream (:body response) response out)
                            (written))]

                ;; the line is accepted as a job instead of held on the connection
                (expect (= 202 (:status accepted)))
                (expect (= "synthesize" (get job "direction")))
                (expect (= "speaker" (get job "engine")))
                (expect (false? (get job "is_done")))
                ;; the phase a listener waits on is pushed while the engine works
                (expect (= "text/event-stream" (get-in response [:headers "Content-Type"])))
                (expect (wait-until #(str/includes? (written) "\"synthesizing\"")))
                (deliver release :go)
                (let [body
                      (deref stream 5000 :timeout)

                      frames
                      (->> (str/split (str body) #"\n\n")
                           (remove str/blank?)
                           (remove #(str/starts-with? % ":")))

                      final
                      (last (sse-jobs (str body)))]

                  ;; every frame names the SYNTHESIS stream, never the transcription one
                  (expect (seq frames))
                  (expect (every?
                            #(str/starts-with? % (str "event: " gateway-contract/speech-job-event))
                            frames))
                  ;; the last frame IS the result: no follow-up request to learn it
                  (expect (= "done" (get final "phase")))
                  (expect (= 100 (get final "progress")))
                  (expect (true? (get final "is_done")))
                  ;; it describes the audio without ever naming the file
                  (expect (= "audio/wav" (get-in final ["audio" "media_type"])))
                  (expect (= 24000 (get-in final ["audio" "sample_rate"])))
                  (expect (pos? (get-in final ["audio" "bytes"])))
                  (expect (not (str/includes? (str body) "audio_path")))
                  ;; the audio route serves exactly the bytes the engine wrote
                  (let [audio (#'speech-api/speech-job-audio-handler
                               {:request-method :get :path-params {:sid sid :job-id job-id}})]
                    (expect (= 200 (:status audio)))
                    (expect (= "audio/wav" (get-in audio [:headers "Content-Type"])))
                    (expect (java.util.Arrays/equals (body-bytes (:body audio)) (spoken-wav line))))
                  ;; forgetting the job takes its file with it - one WAV per reply is a leak
                  (let [path (speech/job-audio-path job-id)]
                    (expect (.isFile (io/file path)))
                    (expect (= 200
                               (:status (#'speech-api/speech-job-handler
                                         {:request-method :delete
                                          :path-params {:sid sid :job-id job-id}}))))
                    (expect (not (.isFile (io/file path))))
                    (expect (= 404
                               (:status (#'speech-api/speech-job-audio-handler
                                         {:request-method :get
                                          :path-params {:sid sid :job-id job-id}}))))))))))))))

(defdescribe
  a-job-belongs-to-one-direction-and-the-other-route-does-not-know-it
  (it "a job belongs to one direction and the other route does not know it"
      ;; Both directions share one store. A client asking the speech routes about a
      ;; transcription must be told it does not exist, not handed the other half of the store
      ;; - and a DELETE must never reach across and forget someone else's work.
      (let [sid (str (random-uuid))]
        (with-redefs-fn {#'state/soul (constantly {:session-id sid})}
          (fn []
            (speech/reset-jobs!)
            (with-only-engine!
              {:id :fake-engine
               :label "Fake"
               :transcribe (constantly "hi")
               :model-state (constantly {:state :ready})}
              (fn []
                (let [job-id (get (wire/parse-json (:body (#'speech-api/voice-handler
                                                           {:path-params {:sid sid}
                                                            :body (wav-body)})))
                                  "id")
                      speech (fn [handler method]
                               (:status (handler {:request-method method
                                                  :path-params {:sid sid :job-id job-id}})))]

                  (expect (= 404 (speech #'speech-api/speech-job-handler :get)))
                  (expect (= 404 (speech #'speech-api/speech-job-events-handler :get)))
                  (expect (= 404 (speech #'speech-api/speech-job-audio-handler :get)))
                  (expect (= 404 (speech #'speech-api/speech-job-handler :delete)))
                  ;; and the job is still there, on its own route
                  (expect (some? (speech/job job-id)))
                  (expect (= 200
                             (:status (#'speech-api/voice-job-handler
                                       {:request-method :get
                                        :path-params {:sid sid :job-id job-id}}))))))))))))

(defdescribe
  speech-refusals-name-the-reason-instead-of-failing-late
  (it
    "speech refusals name the reason instead of failing late"
    (let [sid (str (random-uuid))]
      (with-redefs-fn {#'state/soul (constantly {:session-id sid})}
        (fn []
          ;; no synthesis engine at all is 501, and it names the direction
          (with-only-speech-engine!
            nil
            (fn []
              (let [response (#'speech-api/speech-handler
                              (merge {:request-method :post :path-params {:sid sid}}
                                     (json-body {:text "hello"})))]
                (expect (= 501 (:status response)))
                (expect (str/includes? (get-in (wire/parse-json (:body response))
                                               ["error" "message"])
                                       "speech synthesis")))))
          (with-only-speech-engine!
            (speaking-engine)
            (fn []
              (let [say
                    (fn say ([text] (say text nil)) ([text query] (#'speech-api/speech-handler
                                                                   (merge {:request-method :post
                                                                           :path-params {:sid sid}}
                                                                     (when query {:query-params
                                                                                  query})
                                                                     (json-body {:text text})))))]
                ;; naming an engine nobody registered is the CALLER's 400
                (expect (= 400 (:status (say "hello" {"engine" "elevenlabs"}))))
                ;; nothing to say is refused before the engine is ever woken
                (expect (= 400 (:status (say "   "))))
                ;; a runaway line is refused rather than synthesized for minutes
                (expect (= 413 (:status (say (apply str (repeat 21000 "x"))))))
                ;; capabilities publish ONE boolean: this machine HAS an engine that speaks
                (expect (true? (-> (#'views-api/capabilities-handler {})
                                   :body
                                   wire/parse-json
                                   (get-in ["features" "speech" "is_enabled"])))))))
          ;; an engine that is still preparing answers 425 with its own state
          (with-only-speech-engine!
            {:id :downloading
             :synthesize (constantly "/tmp/vis-never-written.wav")
             :model-state (constantly {:state :downloading :progress 42})}
            (fn []
              (let [response (#'speech-api/speech-handler
                              (merge {:request-method :post :path-params {:sid sid}}
                                     (json-body {:text "hello"})))
                    body (wire/parse-json (:body response))]

                (expect (= 425 (:status response)))
                (expect (= "engine-not-ready" (get-in body ["error" "type"])))
                (expect (= "downloading" (get-in body ["error" "model" "status"])))
                (expect (= 42 (get-in body ["error" "model" "progress"])))))))))))

;; Regression: the global slash endpoint resolved project skills against the gateway
;; process cwd, so nested-project sessions neither saw their own skills nor their children.
(defdescribe
  slashes-handler-uses-the-session-workspace-and-includes-native-commands
  (it
    "slashes handler uses the session workspace and includes native commands"
    (let [seen
          (atom nil)

          loads
          (atom 0)

          sid
          (java.util.UUID/randomUUID)

          root
          "/tmp/vis-companion-project"]

      (with-redefs [state/session-workspace-info
                    (fn [actual-sid]
                      (expect (= sid actual-sid))
                      {"root" root})

                    python-extensions/ensure-python-extensions-loaded!
                    (fn []
                      (swap! loads inc)
                      {:loaded 0 :failed 0 :changed? false})

                    slash/slash-palette
                    (fn [channel extra]
                      (reset! seen [channel extra (.getPath (workspace/cwd))])
                      (conj (vec extra) {:name "/rename" :doc "Rename"}))]

        (let [response
              (#'sessions-api/slashes-handler {:path-params {:sid (str sid)}})

              body
              (wire/parse-json (:body response))]

          (expect (= 200 (:status response)))
          (expect (= 1 @loads))
          (expect (= :web (first @seen)))
          (expect (= ["/help" "/new-session" "/sessions"] (mapv :name (second @seen))))
          (expect (= (.getCanonicalPath (io/file root)) (nth @seen 2)))
          (expect (some #(= "/rename" (get % "name")) (get body "commands"))))))))

;; Regression, Vis session ae259fdd-2712-4591-8f12-e1cdff30b208: gateway startup
;; loaded Python before listening, while the TUI separately loaded the same files.
(defdescribe slashes-handler-loads-python-on-demand-for-the-tui
             (it "slashes handler loads python on demand for the tui"
                 (let [sid
                       (java.util.UUID/randomUUID)

                       calls
                       (atom [])]

                   (with-redefs [state/session-workspace-info
                                 (constantly {"root" "/tmp/vis-tui-project"})

                                 python-extensions/ensure-python-extensions-loaded!
                                 (fn []
                                   (swap! calls conj :python)
                                   {:loaded 1 :failed 0 :changed? true})

                                 slash/slash-palette
                                 (fn [channel extra]
                                   (swap! calls conj [:palette channel extra])
                                   [{:name "/python-echo" :doc "Echo"}])]

                     (let [response
                           (#'sessions-api/slashes-handler
                            {:path-params {:sid (str sid)} :query-params {"channel" "tui"}})

                           body
                           (wire/parse-json (:body response))]

                       (expect (= 200 (:status response)))
                       (expect (= [:python [:palette :tui nil]] @calls))
                       (expect (= "/python-echo" (get-in body ["commands" 0 "name"]))))))))

(defdescribe slashes-handler-refuses-an-unknown-session
             (it "slashes handler refuses an unknown session"
                 (let [sid (java.util.UUID/randomUUID)]
                   (with-redefs [state/session-workspace-info (constantly nil)]
                     (expect (= 404
                                (:status (#'sessions-api/slashes-handler
                                          {:path-params {:sid (str sid)}}))))))))

(defdescribe wrap-auth-accepts-gateway-secret-header
             (it "a token-gated gateway authenticates the internal client's X-Vis-Gateway-Secret"
                 (with-server-state!
                   {:require-token? true}
                   (fn []
                     (let [wrap-auth
                           (rv 'wrap-auth)

                           handler
                           (fn [_req]
                             {:status 200 :body "ok"})

                           app
                           (wrap-auth handler "sekret" [])

                           req
                           (fn [headers]
                             {:uri "/v1/sessions" :headers headers})]

                       ;; no credential → 401
                       (expect (= 401 (:status (app (req {})))))
                       ;; Authorization: Bearer with the right token → 200
                       (expect (= 200 (:status (app (req {"authorization" "Bearer sekret"})))))
                       ;; X-Vis-Gateway-Secret carrying the same secret → 200 (the internal client's carrier)
                       (expect (= 200 (:status (app (req {"x-vis-gateway-secret" "sekret"})))))
                       ;; X-Vis-Gateway-Secret with a wrong secret → 401
                       (expect (= 401 (:status (app (req {"x-vis-gateway-secret" "nope"}))))))))))

(defdescribe wrap-auth-disabled-on-loopback-default
             (it "with auth off (loopback default) every request passes without a token"
                 (with-server-state!
                   {:require-token? false}
                   (fn []
                     (let [wrap-auth
                           (rv 'wrap-auth)

                           app
                           (wrap-auth (fn [_req]
                                        {:status 200})
                                      "sekret"
                                      [])]

                       (expect (= 200 (:status (app {:uri "/v1/sessions" :headers {}})))))))))

(defdescribe
  cors-preflight-is-answered-before-auth
  (it
    "a cross-origin browser reaches a token-gated gateway: preflight OPTIONS is 204 without a token, and CORS headers ride every response so the browser can read even a 401"
    (with-server-state!
      {:require-token? true}
      (fn []
        (let [app
              ((rv 'app) "sekret" [])

              origin
              "http://100.109.18.77:5273"

              preflight
              (app {:request-method :options
                    :uri "/v1/sessions"
                    :headers {"origin" origin
                              "access-control-request-headers" "authorization,content-type"}})

              noauth
              (app {:request-method :get
                    :uri "/v1/sessions"
                    :headers {"origin" origin
                              "x-vis-protocol" (str gateway-contract/protocol-version)}})

              authed
              (app {:request-method :get
                    :uri "/v1/sessions"
                    :headers {"origin" origin
                              "authorization" "Bearer sekret"
                              "x-vis-protocol" (str gateway-contract/protocol-version)}})]

          ;; preflight OPTIONS short-circuits auth with 204
          (expect (= 204 (:status preflight)))
          (expect (= origin (get-in preflight [:headers "Access-Control-Allow-Origin"])))
          (expect (= "true" (get-in preflight [:headers "Access-Control-Allow-Credentials"])))
          (expect (= "authorization,content-type"
                     (get-in preflight [:headers "Access-Control-Allow-Headers"])))
          ;; #BLO-167: filing a session into a group is `PUT /v1/sessions/:sid/group`.
          ;; With PUT missing from the advertised methods the browser refused the
          ;; preflight and the companion only ever showed "cannot reach the gateway".
          (let [allowed (set (map str/trim
                                  (str/split (get-in preflight
                                                     [:headers "Access-Control-Allow-Methods"])
                                             #",")))]
            (expect (every? allowed ["GET" "POST" "PUT" "PATCH" "DELETE" "OPTIONS"])))
          ;; a 401 still carries CORS so the browser surfaces the error, not an opaque block
          (expect (= 401 (:status noauth)))
          (expect (= origin (get-in noauth [:headers "Access-Control-Allow-Origin"])))
          ;; an authenticated request gets its data with CORS
          (expect (= 200 (:status authed)))
          (expect (= origin (get-in authed [:headers "Access-Control-Allow-Origin"]))))))))

(defdescribe
  parse-multi-sids-parses-and-filters
  (it
    "sid[:cursor] comma list — cursor defaults to 0, only malformed UUIDs are dropped"
    (let [sid-a
          (java.util.UUID/randomUUID)

          sid-b
          (java.util.UUID/randomUUID)

          a
          (str sid-a)

          b
          (str sid-b)]

      (with-redefs-fn {#'state/soul (fn [sid]
                                      (contains? #{sid-a sid-b} sid))}
        (fn []
          (let [parse (rv 'parse-multi-sids)]
            ;; sids are parsed to java.util.UUID — the registry's key type
            ;; (path-sid parity). A string key registered a ghost registry
            ;; entry: idle tabs would miss live queue and turn events.
            (expect (= [[sid-a 10] [sid-b 0]]
                       (parse {:query-params {"sids" (str a ":10, " b " , zzz:3")}})))
            (expect (nil? (parse {:query-params {}})))
            (expect (nil? (parse {:query-params {"sids" ""}})))
            ;; unknown UUIDs survive parsing so reconnect can report their deletion
            (let [missing (java.util.UUID/randomUUID)]
              (expect (= [[missing 91]] (parse {:query-params {"sids" (str missing ":91")}}))))
            ;; Last-Event-ID overrides the cursor for the SINGLE-sid case (native reconnect)
            (expect (= [[sid-a 42]]
                       (parse {:query-params {"sids" (str a ":0")}
                               :headers {"last-event-id" "42"}})))
            (expect (= [[sid-a 7]]
                       (parse {:query-params {"sids" a} :headers {"last-event-id" "7"}})))
            ;; Last-Event-ID is IGNORED for multi-sid (one header can't resume N per-session seqs)
            (expect (= [[sid-a 10] [sid-b 0]]
                       (parse {:query-params {"sids" (str a ":10," b)}
                               :headers {"last-event-id" "42"}})))
            ;; a non-numeric Last-Event-ID is ignored
            (expect (= [[sid-a 3]]
                       (parse {:query-params {"sids" (str a ":3")}
                               :headers {"last-event-id" ""}})))))))))

(defdescribe
  transcript-window-params-never-degrade-to-the-full-transcript
  (it
    "transcript window params never degrade to the full transcript"
    (let [q
          (fn [qs]
            (ring-params/assoc-query-params {:path-params {:sid (str (java.util.UUID/randomUUID))}
                                             :query-string qs}
                                            "UTF-8"))

          query-long
          gw-http/query-long

          seen
          (atom nil)

          call
          (fn [qs]
            (reset! seen ::unset)
            (with-redefs-fn {#'state/transcript-page
                             (fn [_sid opts]
                               (reset! seen opts)
                               {:turns [] :total 0 :offset 0 :has-more false})}
              #(let [r (#'transcripts-api/transcript-handler (q qs))] [(:status r) @seen])))]

      ;; ring hands back a VECTOR for a repeated param, so parsing must not throw
      (expect (= ["1" "2"] (get-in (q "limit=1&limit=2") [:query-params "limit"])))
      (expect (= 2 (query-long (q "limit=1&limit=2") "limit")))
      ;; absent or blank stays nil — that is what 'unwindowed' means
      (expect (nil? (query-long (q "") "limit")))
      (expect (nil? (query-long (q "limit=  ") "limit")))
      ;; 0 and negatives reach the caller verbatim; clamping is transcript-page's job
      (expect (= 0 (query-long (q "limit=0") "limit")))
      (expect (= -5 (query-long (q "limit=-5") "limit")))
      ;; no window params = the whole transcript, so an older client is unaffected
      (expect (= [200 {:limit nil :offset nil}] (call "")))
      ;; ?limit=0 honestly means zero rows, never the whole transcript
      (expect (= [200 {:limit 0 :offset nil}] (call "limit=0")))
      ;; a duplicated param answers on its last value instead of a 500
      (expect (= [200 {:limit 2 :offset nil}] (call "limit=1&limit=2")))
      ;; a present-but-unparsable window param is a 400, not a silent full-transcript fallback
      (expect (= 400 (first (call "limit=abc"))))
      (expect (= 400 (first (call "limit=10&offset=nope"))))
      (expect (= [200 {:limit 24 :offset nil :iteration-limit 8}]
                 (call "limit=24&iteration_limit=8")))
      (expect (= [200 {:limit nil :offset nil :iteration-limit 4}]
                 (call "iteration_limit=8&iteration_limit=4")))
      (doseq [value ["abc" "0" "-1" "" "9223372036854775808"]]
        (expect (= 400 (first (call (str "iteration_limit=" value)))))))))

(defdescribe
  multi-sse-reports-sessions-deleted-while-disconnected
  (it
    "multi sse reports sessions deleted while disconnected"
    (let [missing
          (java.util.UUID/randomUUID)

          live
          (java.util.UUID/randomUUID)

          subscribed
          (atom [])

          out
          (java.io.ByteArrayOutputStream.)]

      (with-server-state! {}
                          (fn []
                            (with-redefs-fn {#'state/soul #(when (= live %) {"id" (str live)})
                                             #'state/current-turn-id (constantly nil)
                                             #'state/subscribe! (fn [sid _ _ _ _]
                                                                  (swap! subscribed conj sid)
                                                                  [])
                                             #'state/unsubscribe! (fn [& _])
                                             #'server/pump-sse! (fn [& _])
                                             #'server/stop! (fn [])}
                              #(ring-protocols/write-body-to-stream
                                 ((rv 'multi-sse-body) [[missing 900] [live 0]] false nil)
                                 {}
                                 out))))
      (let [frames
            (sse-jobs (.toString out "UTF-8"))

            deleted
            (first (filter #(= "session.deleted" (get % "type")) frames))]

        (expect (= [live] @subscribed))
        (expect (= (str missing) (get deleted "session_id")))
        (expect (> (long (get deleted "seq" 0)) 900))
        (expect (= ["session.deleted" "subscription.ready"] (mapv #(get % "type") frames)))))))

(defdescribe multi-sse-reports-deletion-during-subscription
             (it "multi sse reports deletion during subscription"
                 (let [sid
                       (java.util.UUID/randomUUID)

                       present?
                       (atom true)

                       out
                       (java.io.ByteArrayOutputStream.)]

                   (with-server-state!
                     {}
                     (fn []
                       (with-redefs-fn {#'state/soul #(when @present? {"id" (str %)})
                                        #'state/subscribe! (fn [& _]
                                                             (reset! present? false)
                                                             [])
                                        #'state/unsubscribe! (fn [& _])
                                        #'server/pump-sse! (fn [& _])
                                        #'server/stop! (fn [])}
                         #(ring-protocols/write-body-to-stream
                            ((rv 'multi-sse-body) [[sid 0]] false nil)
                            {}
                            out))))
                   (expect (str/includes? (.toString out "UTF-8") "session.deleted"))
                   (expect (not (str/includes? (.toString out "UTF-8") "subscription.ready"))))))

(defdescribe
  multi-sse-fans-many-sessions-down-one-stream
  (it
    "every listed session's events ride ONE connection, tagged by :session_id, deduped per session"
    (with-redefs-fn {#'server/stop! (fn []
                                      nil)
                     #'state/soul (constantly {"id" "exists"})}
      (fn []
        (with-server-state!
          {}
          (fn []
            (let [multi-sse-body
                  (rv 'multi-sse-body)

                  write-body
                  (requiring-resolve 'ring.core.protocols/write-body-to-stream)

                  sid-a
                  (str (java.util.UUID/randomUUID))

                  sid-b
                  (str (java.util.UUID/randomUUID))

                  baos
                  (java.io.ByteArrayOutputStream.)

                  body
                  (multi-sse-body [[sid-a 0] [sid-b 0]] false nil)

                  fut
                  (future (try (write-body body {} baos) (catch Throwable _ nil)))]

              (expect (wait-until #(re-find #"subscription.ready"
                                            (String. (.toByteArray baos) "UTF-8"))))
              (state/append-event! sid-a "test.alpha" {:n 1})
              (state/append-event! sid-b "test.beta" {:n 2})
              (state/append-event! sid-a "test.alpha2" {:n 3})
              (expect (wait-until #(re-find #"test.alpha2" (String. (.toByteArray baos) "UTF-8"))))
              (future-cancel fut)
              (let [s (String. (.toByteArray baos) "UTF-8")]
                ;; both sessions surfaced on the single stream
                (expect (re-find (re-pattern sid-a) s))
                (expect (re-find (re-pattern sid-b) s))
                (expect (re-find #"test.alpha2" s))
                ;; per-session dedup keeps each session's own monotonic run
                ;; Each session rides ONE subscription.ready control frame
                ;; (carrying its :session_id in the JSON data) plus one frame per
                ;; distinct event. The sid appears only in each frame's data, so its
                ;; count is the reliable per-session dedup signal:
                ;; sid-a: ready + test.alpha + test.alpha2 = 3; sid-b: ready + test.beta = 2.
                (expect (re-find #"subscription.ready" s))
                (expect (= 3 (count (re-seq (re-pattern sid-a) s))))
                (expect (= 2 (count (re-seq (re-pattern sid-b) s))))))))))))

;; Regression, reported in this Vis session: the only way a session list could
;; learn that a run had started or ended was to re-read its whole window on a
;; timer, so a phone paid a full window per tick to discover that nothing had
;; moved — and painted a stale row until the next tick.
(defdescribe
  fleet-scope-streams-status-deltas-instead-of-a-window
  (it
    "?scope=fleet opens ONE stream whose frames are single session status changes"
    (with-server-state!
      {}
      (fn []
        (let [sid
              (str (java.util.UUID/randomUUID))

              snapshot
              (atom {})

              baselines
              ;; Observe the empty baseline before exercising a later transition;
              ;; first-tick liveness has its own regression test.
              (atom 0)

              write-body
              (requiring-resolve 'ring.core.protocols/write-body-to-stream)

              handler
              (rv 'multi-events-handler)]

          (with-redefs-fn {#'server/stop! (fn []
                                            nil)
                           #'state/FLEET_POLL_MS 10
                           #'state/fleet-snapshot (fn []
                                                    (let [taken @snapshot]
                                                      (swap! baselines inc)
                                                      taken))}
            (fn []
              (let [response
                    (handler {:query-params {"scope" "fleet"} :headers {}})

                    baos
                    (java.io.ByteArrayOutputStream.)

                    text
                    (fn []
                      (String. (.toByteArray baos) "UTF-8"))

                    frames
                    ;; Parse only COMPLETE frames: the watcher writes while this
                    ;; thread reads, and half a frame is not JSON.
                    (fn []
                      (let [written
                            (text)

                            end
                            (str/last-index-of written "\n\n")]

                        (if end (sse-jobs (subs written 0 (+ (long end) 2))) [])))

                    mine
                    ;; This machine may be running real sessions, and the watcher
                    ;; reports the whole fleet — the test owns exactly one sid.
                    (fn []
                      (filterv #(= sid (get % "session_id")) (frames)))

                    fut
                    (future (try (write-body (:body response) {} baos) (catch Throwable _ nil)))]

                (expect (= 200 (:status response)))
                (expect (= "text/event-stream" (get-in response [:headers "Content-Type"])))
                (expect (wait-until-slow #(seq (frames))))
                ;; the ready frame names the scope, so a client knows to cold-read its window
                (let [ready (first (frames))]
                  (expect (= "subscription.ready" (get ready "type")))
                  (expect (= "fleet" (get ready "scope"))))
                (expect (wait-until-slow #(pos? @baselines)))
                (reset! snapshot {sid {"is_live" true
                                       "is_awaiting_input" false
                                       "current_turn_id" "t-1"}})
                (expect (wait-until-slow #(seq (mine))))
                (reset! snapshot {})
                (expect (wait-until-slow #(some (fn [frame]
                                                  (false? (get frame "is_live")))
                                                (mine))))
                (future-cancel fut)
                (let [status-frames (mine)]
                  ;; a frame is one session's state, not a row and not a window
                  (expect (= ["session.status"] (distinct (map #(get % "type") status-frames))))
                  (expect (= [true false] (mapv #(get % "is_live") status-frames)))
                  (expect (= "t-1" (get (first status-frames) "current_turn_id")))
                  (expect (nil? (get (last status-frames) "current_turn_id")))
                  (expect (every? #(number? (get % "seq")) status-frames))))))))))
  (it "without sids and without a scope the route still refuses"
      (with-server-state!
        {}
        (fn []
          (expect (= 400 (:status ((rv 'multi-events-handler) {:query-params {} :headers {}}))))))))

(defdescribe
  combined-event-stream-keeps-both-feeds-on-one-connection
  (it "combined event stream keeps both feeds on one connection"
      ;; Regression: three visible HTTP/1.1 tabs filled six event slots and stalled GETs.
      (let [sid
            (random-uuid)

            subscribed
            (atom [])

            fleet-attached?
            (atom false)

            out
            (java.io.ByteArrayOutputStream.)]

        (with-server-state!
          {}
          (fn []
            (with-redefs-fn {#'server/stop! (constantly nil)
                             #'state/soul (fn [id]
                                            (when (= sid id) {"id" (str sid)}))
                             #'state/current-turn-id (constantly nil)
                             #'state/current-seq (constantly 4)
                             #'state/replay-floor (constantly 0)
                             #'state/subscribe!
                             (fn [id _ _ cursor _]
                               (swap! subscribed conj [id cursor])
                               [{"schema" 1 "type" "turn.started" "session_id" (str id) "seq" 5}])
                             #'state/unsubscribe! (fn [& _])
                             #'state/subscribe-fleet! (fn [_ sink]
                                                        (reset! fleet-attached? true)
                                                        (sink {"schema" 1
                                                               "type" "session.status"
                                                               "session_id" (str sid)
                                                               "seq" 900
                                                               "is_live" true}))
                             #'state/unsubscribe-fleet! (fn [_]
                                                          (reset! fleet-attached? false))
                             (rv 'pump-sse!)
                             (fn [_ queue _ write!]
                               (loop []

                                 (when-let [event (.poll ^java.util.concurrent.ArrayBlockingQueue
                                                         queue)]
                                   (write! event)
                                   (recur))))}
              (fn []
                (let [response ((rv 'multi-events-handler)
                                 {:query-params {"sids" (str sid ":4") "scope" "both"}
                                  :headers {}})]
                  (expect (= 200 (:status response)))
                  (ring-protocols/write-body-to-stream (:body response) {} out))))))
        (let [frames (sse-jobs (.toString out "UTF-8"))]
          (expect (= [[sid 4]] @subscribed))
          (expect (false? @fleet-attached?))
          (expect (= ["subscription.ready" "turn.started" "subscription.ready" "session.status"]
                     (mapv #(get % "type") frames)))
          (expect (= [nil nil "fleet" "fleet"] (mapv #(get % "scope") frames)))
          (expect (= 4 (get (first frames) "cursor")))
          (expect (= [5 0 900] (mapv #(get % "seq") (rest frames))))))))

(defdescribe
  fleet-ready-follows-subscription
  (it "fleet ready follows subscription"
      ;; Reconnecting clients resync on ready; changes from that instant must be queued.
      (with-server-state!
        {}
        (fn []
          (let [attached?
                (atom false)

                writes
                (atom [])

                out
                (proxy [java.io.ByteArrayOutputStream] [] (flush [] (swap! writes conj @attached?)))

                status
                {"schema" 1
                 "type" "session.status"
                 "session_id" "goal-session"
                 "seq" 1
                 "is_live" true
                 "current_turn_id" "goal-turn"}]

            (with-redefs-fn {#'server/stop! (constantly nil)
                             #'state/subscribe-fleet! (fn [_ sink]
                                                        (reset! attached? true)
                                                        (sink status))
                             #'state/unsubscribe-fleet! (fn [_]
                                                          (reset! attached? false))
                             (rv 'pump-sse!)
                             (fn [_ queue _ write!]
                               (when-let [event (.poll ^java.util.concurrent.ArrayBlockingQueue
                                                       queue)]
                                 (write! event)))}
              (fn []
                (ring-protocols/write-body-to-stream ((rv 'fleet-sse-body) false nil false)
                                                     {}
                                                     out)
                (expect (= [true true] @writes))
                (expect (= ["subscription.ready" "session.status"]
                           (mapv #(get % "type") (sse-jobs (.toString out "UTF-8")))))
                (expect (false? @attached?)))))))))

;; ── `subscription.ready` states the daemon's OWN turn, so a reconnect needs no probe ──
;; A client that went dark cannot tell "nothing happened" from "I missed the
;; terminal event": its cursor is accepted either way and the replay ring is
;; process memory. The ready frame therefore carries the one fact only the daemon
;; knows — the turn it is running for this session RIGHT NOW — before any replay.
;; Agreement with what the client paints is a positive verdict for zero round
;; trips; disagreement is proof of a gap and the client reconciles once.

(defdescribe
  subscription-ready-carries-the-daemons-current-turn
  (it
    "the ready frame names the running turn, and says so for an idle session"
    (with-redefs-fn {#'server/stop! (fn []
                                      nil)
                     #'state/soul (constantly {"id" "exists"})}
      (fn []
        (with-server-state!
          {}
          (fn []
            (let [multi-sse-body
                  (rv 'multi-sse-body)

                  write-body
                  (requiring-resolve 'ring.core.protocols/write-body-to-stream)

                  sid-live
                  (str (java.util.UUID/randomUUID))

                  sid-idle
                  (str (java.util.UUID/randomUUID))

                  baos
                  (java.io.ByteArrayOutputStream.)]

              ;; A turn running in a SIBLING process is exactly the case a
              ;; reconnecting client cannot resolve from its own stream; the registry
              ;; mirrors it (`ingest-mirrored-event!`), so the ready frame can state it.
              (state/append-event! sid-live "test.seed" {:n 1})
              (state/ingest-mirrored-event!
                sid-live
                true
                {"type" "turn.started" "turn_id" "t-live" "session_id" sid-live})
              (let [body
                    (multi-sse-body [[sid-live 0] [sid-idle 0]] false nil)

                    fut
                    (future (try (write-body body {} baos) (catch Throwable _ nil)))]

                (expect (wait-until #(= 2
                                        (count (re-seq #"\"type\":\"subscription\.ready\""
                                                       (String. (.toByteArray baos) "UTF-8"))))))
                (future-cancel fut)
                (let [s
                      (String. (.toByteArray baos) "UTF-8")

                      ;; Keyed by the session each frame is ABOUT. Matching the whole
                      ;; body would pass even with the two verdicts swapped, since one
                      ;; `true` and one `false` are on the wire either way.
                      ready
                      (into {}
                            (keep (fn [line]
                                    (when (re-find #"subscription\.ready" line)
                                      (when-let [sid (second (re-find
                                                               #"\"session_id\"\s*:\s*\"([^\"]+)\""
                                                               line))]
                                        [sid line]))))
                            (re-seq #"data:.*" s))]

                  (expect (= #{sid-live sid-idle} (set (keys ready))))
                  ;; the live session's frame names the turn the daemon holds
                  (expect (re-find #"\"current_turn_id\"\s*:\s*\"t-live\"" (get ready sid-live)))
                  (expect (re-find #"\"is_live\"\s*:\s*true" (get ready sid-live)))
                  ;; the idle session's frame is an explicit negative, not a silence
                  ;; Without this a client cannot distinguish "no turn" from "old
                  ;; daemon that never shipped the field" — and must probe blindly.
                  (expect (re-find #"\"is_live\"\s*:\s*false" (get ready sid-idle)))
                  (expect (nil? (re-find #"\"current_turn_id\"\s*:\s*\""
                                         (get ready sid-idle)))))))))))))

;; Regression, issue reported in session 78b0c0b5-f5ba-453f-97ee-af0a85f72d25:
;; a phone joining iteration 420 learned that position only after replaying the
;; running turn from iteration 1, so its live ticker visibly counted through history.
(defdescribe
  subscription-ready-leads-running-replay-with-its-latest-iteration
  (it
    "the current position arrives before the journal frames it summarizes"
    (with-redefs-fn {#'server/stop! (fn []
                                      nil)
                     #'state/soul (constantly {"id" "exists"})}
      (fn []
        (with-server-state!
          {}
          (fn []
            (let [multi-sse-body
                  (rv 'multi-sse-body)

                  write-body
                  (requiring-resolve 'ring.core.protocols/write-body-to-stream)

                  sid
                  (str (java.util.UUID/randomUUID))

                  baos
                  (java.io.ByteArrayOutputStream.)]

              (state/append-event! sid "test.seed" {:n 1})
              (state/append-event! sid "iteration.completed" {:turn-id "t-old" :iteration 900})
              (state/ingest-mirrored-event! sid
                                            true
                                            {"type" "turn.started"
                                             "turn_id" "t-live"
                                             "request" "keep working"
                                             "session_id" sid})
              (state/ingest-mirrored-event!
                sid
                true
                {"type" "iteration.completed" "turn_id" "t-live" "iteration" 420 "session_id" sid})
              (let [body
                    (multi-sse-body [[sid 0]] false nil)

                    fut
                    (future (try (write-body body {} baos) (catch Throwable _ nil)))]

                (expect (wait-until #(re-find #"iteration.completed"
                                              (String. (.toByteArray baos) "UTF-8"))))
                (future-cancel fut)
                (let [wire
                      (String. (.toByteArray baos) "UTF-8")

                      ready-at
                      (.indexOf wire "subscription.ready")

                      replay-at
                      (.indexOf wire "turn.started")]

                  (expect (<= 0 ready-at))
                  (expect (< ready-at replay-at))
                  (expect (re-find #"\"latest_iteration\"\s*:\s*420"
                                   (subs wire ready-at replay-at))))))))))))

;; ── Resource rid rides the QUERY STRING, not a path segment (issue #14) ──
;; A resource id can embed an absolute path — an nREPL id is `nrepl:/Users/…/ws`.
;; Percent-encoded into a PATH SEGMENT its `/` becomes `%2F`, which Jetty rejects
;; with "Ambiguous URI path separator" (400) — that 400 threw out of the client
;; and wedged F4 when you clicked logs on the clojure nREPL. The fix moves rid to
;; the `rid` query param on stop/logs. These lock that in on BOTH halves.

(def ^:private nrepl-rid
  "A real-shaped nREPL resource id: the `/`-embedding absolute path that broke."
  "nrepl:/Users/fierycod/vis")

(defdescribe resource-client-builds-query-param-urls
             (it "stop/logs put rid in the ?rid= query, never a path segment (no %2F in path)"
                 (let [sent (atom [])]
                   (with-redefs-fn {#'client/send-json! (fn [method path & _]
                                                          (swap! sent conj [method path])
                                                          {:result "ok" :lines ["a"]})}
                     (fn []
                       (let [sid (str (random-uuid))]
                         (client/stop-resource! sid nrepl-rid)
                         (client/resource-logs sid nrepl-rid)
                         (let [[[_ stop] [_ logs]] @sent]
                           ;; each url ends with the rid encoded in a query param
                           (expect (= (str "/v1/sessions/"
                                           sid
                                           "/resources/stop?rid=nrepl%3A%2FUsers%2Ffierycod%2Fvis")
                                      stop))
                           (expect (= (str "/v1/sessions/"
                                           sid
                                           "/resources/logs?rid=nrepl%3A%2FUsers%2Ffierycod%2Fvis")
                                      logs))
                           ;; the raw rid never leaks into the PATH portion (would trip the ambiguous-slash 400)
                           (doseq [[_ path] @sent]
                             (expect (not (re-find #"resources/nrepl" path)))))))))))

(defdescribe resource-handlers-read-rid-from-query-param
             (it "stop/logs handlers forward the rid QUERY param to the resources ns"
                 (let [seen
                       (atom [])

                       sid
                       (str (random-uuid))

                       req
                       {:path-params {:sid sid} :query-params {"rid" nrepl-rid}}]

                   (with-redefs-fn {#'resources/stop! (fn [_ rid]
                                                        (swap! seen conj [:stop rid])
                                                        {:result "stopped"})
                                    #'resources/logs (fn [_ rid]
                                                       (swap! seen conj [:logs rid])
                                                       ["line-1"])}
                     (fn []
                       (let [stop
                             (#'sessions-api/resource-stop-handler req)

                             logs
                             (#'sessions-api/resource-logs-handler req)]

                         ;; each handler answers 200 and threads the exact slash-embedding rid through
                         (expect (= 200 (:status stop)))
                         (expect (= 200 (:status logs)))
                         (expect (= [[:stop nrepl-rid] [:logs nrepl-rid]] @seen))
                         ;; logs handler surfaces the captured lines
                         (expect (= ["line-1"] (get (wire/parse-json (:body logs)) "lines")))))))))

(defdescribe resource-handlers-404-on-unknown-session
             (it
               "a non-uuid sid is rejected before any resources call — 404, resources ns untouched"
               (let [touched
                     (atom false)

                     req
                     {:path-params {:sid "not-a-uuid"} :query-params {"rid" nrepl-rid}}]

                 (with-redefs-fn {#'resources/stop! (fn [& _]
                                                      (reset! touched true)
                                                      {})
                                  #'resources/logs (fn [& _]
                                                     (reset! touched true)
                                                     nil)}
                   (fn []
                     (expect (= 404 (:status (#'sessions-api/resource-stop-handler req))))
                     (expect (= 404 (:status (#'sessions-api/resource-logs-handler req))))
                     (expect (false? @touched)))))))

(defdescribe
  resource-rid-survives-router-as-query-param
  (it
    "the client's encoded url routes to the static handler and decodes rid back verbatim (no 400)"
    (let [seen
          (atom nil)

          echo
          (fn [request]
            (reset! seen {:sid (get-in request [:path-params :sid])
                          :rid (get-in request [:query-params "rid"])})
            {:status 200 :body "ok"})

          app
          (-> (rr/ring-handler (rr/router [["/v1/sessions/:sid/resources/stop" {:post echo}]
                                           ["/v1/sessions/:sid/resources/logs" {:get echo}]]))
              ring-params/wrap-params)

          sid
          (str (random-uuid))

          ;; exactly the shape the client emits: rid percent-encoded into the query
          enc
          (fn [s]
            (java.net.URLEncoder/encode ^String s "UTF-8"))

          resp
          (app {:request-method :get
                :uri (str "/v1/sessions/" sid "/resources/logs")
                :query-string (str "rid=" (enc nrepl-rid))})]

      ;; static logs route matches (a path-segment %2F would 404/400 instead)
      (expect (= 200 (:status resp)))
      ;; the handler sees the sid and the FULL slash-embedding rid, decoded
      (expect (= {:sid sid :rid nrepl-rid} @seen)))))

(defdescribe
  toggle-id-wire-contract-test
  (it "settings rows expose the canonical string id unchanged"
      (toggles/register-toggle! {:id "server_test_toggle" :label "Test" :default false})
      (expect (= "server_test_toggle"
                 (:id (#'settings-api/toggle-json (toggles/toggle-spec "server_test_toggle"))))))
  (it "the settings mutation endpoint rejects keyword-like, namespaced, and kebab ids"
      (doseq [id [":server_test_toggle" "vis/server_test_toggle" "server-test-toggle"]]
        (expect (= 400
                   (:status (#'settings-api/set-setting-handler
                             {:query-params {"id" id "action" "toggle"}}))))))
  (it "a canonical but unknown string id remains a distinct 404"
      (expect (= 404
                 (:status (#'settings-api/set-setting-handler
                           {:query-params {"id" "unknown_toggle" "action" "toggle"}}))))))

(defdescribe get-setting-handler-serves-hidden-rows-test
             (it "reasoning_level is readable by id even though the settings list hides it"
                 (let [response
                       (#'settings-api/get-setting-handler {:path-params {:id "reasoning_level"}})

                       row
                       (wire/parse-json (:body response))]

                   (expect (= 200 (:status response)))
                   (expect (= "reasoning_level" (get row "id")))
                   (expect (= "enum" (get row "type")))
                   (expect (seq (get row "choices")))
                   (expect (string? (get row "value")))))
             (it "a non-canonical id is a 400 and an unknown one a 404"
                 (expect (= 400
                            (:status (#'settings-api/get-setting-handler
                                      {:path-params {:id "reasoning-level"}}))))
                 (expect (= 404
                            (:status (#'settings-api/get-setting-handler
                                      {:path-params {:id "unknown_toggle"}}))))))

(defdescribe settings-change-refreshes-cached-extension-bindings-test
             (it "settings change refreshes cached extension bindings"
                 (toggles/register-toggle! {:id "server_test_toggle" :label "Test" :default false})
                 (toggles/set-enabled! "server_test_toggle" false)
                 (let [synced (atom 0)]
                   (with-redefs [loop-env/sync-cached-extension-symbols! #(swap! synced inc)
                                 config/load-global-yaml-config-raw (constantly {})
                                 config/load-global-config-raw (constantly {})
                                 config/update-machine-config! (fn [f]
                                                                 (f {}))]

                     (let [response (#'settings-api/set-setting-handler
                                     {:query-params {"id" "server_test_toggle" "action" "toggle"}})]
                       (expect (= 200 (:status response))))
                     (expect (= 1 @synced)))
                   (toggles/set-enabled! "server_test_toggle" false))))

(defdescribe
  provider-models-handler-serves-live-catalog-daemon-side
  (it
    "GET /v1/providers/:id/models fetches the LIVE catalog DAEMON-side (gateway owns OAuth token) and emits snake_case hidden_count"
    (with-redefs-fn {#'providers/default-model-names (constantly ["claude-opus-4-8"])
                     #'providers/model-options (fn [_ _ show-all?]
                                                 {:models ["claude-opus-4-8" "claude-sonnet-5"]
                                                  :hidden-count (if show-all? 0 4)})}
      (fn []
        (let [resp
              (#'providers-api/provider-models-handler
               {:path-params {:provider-id "anthropic-coding-plan"}})

              body
              (wire/parse-json (:body resp))]

          (expect (= 200 (:status resp)))
          (expect (= ["claude-opus-4-8" "claude-sonnet-5"] (get body "models")))
          (expect (= 4 (get body "hidden_count"))))
        (let [resp
              (#'providers-api/provider-models-handler
               {:path-params {:provider-id "anthropic-coding-plan"}
                :query-params {"show_all" "true"}})

              body
              (wire/parse-json (:body resp))]

          (expect (= 0 (get body "hidden_count"))))))))

(defdescribe
  set-session-model-handler-validates-against-the-gateway-fleet
  (it
    "PATCH /model pins only providers THIS gateway serves; the model name stays free (live catalog)"
    (let [sid
          (str (java.util.UUID/randomUUID))

          wrote
          (atom nil)

          body
          (fn [m]
            {:path-params {:sid sid}
             :body (java.io.ByteArrayInputStream. (.getBytes (wire/json-str m) "UTF-8"))})]

      (with-redefs-fn {;; The PICKER fleet is what both clients offer: configured providers
                       ;; PLUS presets that are authenticated but not yet written into
                       ;; vis.yml. Validating the pin against `configured-providers`
                       ;; answered 400 for a provider the picker had just listed.
                       #'providers/picker-fleet (constantly [{:id :zai-coding-plan}
                                                             {:id :anthropic-coding-plan}
                                                             {:id :openai-codex}])
                       #'providers/configured-providers-cached (constantly [{:id :zai-coding-plan}])
                       #'state/set-session-model! (fn [_sid p m]
                                                    (reset! wrote [p m]))
                       #'state/session-model (fn [_sid]
                                               @wrote)}
        (fn []
          ;; a configured provider is accepted
          (let [resp (#'sessions-api/set-session-model-handler
                      (body {:provider "zai-coding-plan" :model "glm-5.2"}))]
            (expect (= 200 (:status resp)))
            (expect (= ["zai-coding-plan" "glm-5.2"] @wrote)))
          ;; a model outside vis.yml is fine — the live catalog offers more
          (expect (= 200
                     (:status (#'sessions-api/set-session-model-handler
                               (body {:provider "zai-coding-plan" :model "glm-live-preview"})))))
          (expect (= ["zai-coding-plan" "glm-live-preview"] @wrote))
          ;; a provider the picker offers but vis.yml does not configure is accepted
          (reset! wrote nil)
          (expect (= 200
                     (:status (#'sessions-api/set-session-model-handler
                               (body {:provider "openai-codex" :model "gpt-5.4"})))))
          (expect (= ["openai-codex" "gpt-5.4"] @wrote))
          ;; an unknown provider is a 400 and writes NOTHING
          (reset! wrote :untouched)
          (let [resp (#'sessions-api/set-session-model-handler
                      (body {:provider "not-on-this-gateway" :model "x"}))]
            (expect (= 400 (:status resp)))
            (expect (= "unknown-provider" (get-in (wire/parse-json (:body resp)) ["error" "type"])))
            (expect (= :untouched @wrote)))
          ;; blank/omitted provider still clears or pins by model alone
          (reset! wrote nil)
          (expect (= 200
                     (:status (#'sessions-api/set-session-model-handler
                               (body {:provider "  " :model "glm-5.2"})))))
          (expect (= [nil "glm-5.2"] @wrote)))))))

(defdescribe
  router-handler-assembles-string-keyed-fleet-with-status
  (it
    "GET /v1/router returns every model plus the primary and fallback pairs"
    (with-redefs-fn
      {#'providers/picker-fleet
       (constantly
         [{:id :anthropic-coding-plan
           :base-url "https://api.anthropic.com/v1"
           :models [{:name "claude-opus-4-8"} {:name "claude-sonnet-5"}]}
          {:id :zai-coding-plan :base-url "https://api.z.ai/v1" :models [{:name "glm-5.2"}]}])
       #'providers/default-selection (constantly {:provider-id :anthropic-coding-plan
                                                  :model "claude-sonnet-5"})
       #'providers/fallback-selection (constantly {:provider-id :zai-coding-plan :model "glm-5.2"})
       #'providers/provider-status
       (constantly {:is-authenticated true :auth-state :verified :source :auth-file})
       #'providers/provider-limits-safe
       (constantly
         {:provider-id :anthropic-coding-plan :status :ok :static {} :dynamic {:limits []}})}
      (fn []
        (let [resp
              (#'providers-api/router-handler {})

              provs
              (get (wire/parse-json (:body resp)) "providers")

              p0
              (first provs)

              p1
              (second provs)]

          (expect (= 200 (:status resp)))
          (expect (= "anthropic-coding-plan" (get p0 "id")))
          (expect (= "https://api.anthropic.com/v1" (get p0 "base_url")))
          (expect (= ["claude-opus-4-8" "claude-sonnet-5"] (get p0 "models")))
          (expect (true? (get p0 "is_default")))
          (expect (= "claude-sonnet-5" (get p0 "default_model")))
          ;; the primary row is NEVER also the fallback row
          (expect (false? (get p0 "is_fallback")))
          (expect (nil? (get p0 "fallback_model")))
          (expect (true? (get p1 "is_fallback")))
          (expect (= "glm-5.2" (get p1 "fallback_model")))
          (expect (false? (get p1 "is_default")))
          (expect (nil? (get p1 "default_model")))
          ;; connection verdict is the snake_case STRING key — no keyword restore
          (expect (true? (get-in p0 ["status" "is_authenticated"])))
          (expect (= "verified" (get-in p0 ["status" "auth_state"])))
          (expect (= "auth-file" (get-in p0 ["status" "source"])))
          (expect (every? string? (keys (get p0 "status"))))
          ;; limits ride embedded, string-keyed too
          (expect (= "ok" (get-in p0 ["limits" "status"]))))))))

(defdescribe
  router-handler-preserves-model-wire-capabilities
  (it "router handler preserves model wire capabilities"
      ;; Regression, Vis session 95c4a9b0-ba88-4e8d-86ef-252cb522bcf4: the gateway
      ;; sent names only, so the TUI hid verbosity even for Codex gpt-6-astra.
      (with-redefs [providers/picker-fleet
                    (constantly [{:id :openai-codex :models [{:name "gpt-6-astra"}]}
                                 {:id :github-copilot
                                  :models [{:name "gpt-6-astra"} {:name "claude-opus-5"}]}
                                 {:id :anthropic :models [{:name "claude-opus-5"}]}])

                    providers/default-selection
                    (constantly nil)

                    providers/fallback-selection
                    (constantly nil)

                    providers/provider-status
                    (constantly {})

                    providers/provider-limits-safe
                    (constantly {})]

        (let [response
              (#'providers-api/router-handler {})

              rows
              (get (wire/parse-json (:body response)) "providers")

              by-id
              (into {} (map (juxt #(get % "id") identity)) rows)

              codex
              (get by-id "openai-codex")

              copilot
              (get by-id "github-copilot")

              anthropic
              (get by-id "anthropic")]

          (expect (= 200 (:status response)))
          (expect (= ["gpt-6-astra"] (get codex "models")))
          (expect (= [{"name" "gpt-6-astra"
                       "is_reasoning_effort_configurable" true
                       "verbosity_style" "openai-text"}]
                     (get codex "model_details")))
          (expect (= ["openai-text" nil]
                     (mapv #(get % "verbosity_style") (get copilot "model_details"))))
          (expect (= [{"name" "claude-opus-5"
                       "is_reasoning_effort_configurable" true
                       "verbosity_style" nil}]
                     (get anthropic "model_details")))))))

(defdescribe
  router-model-details-honor-wire-overrides
  (it "router model details honor wire overrides"
      (with-redefs [providers/provider-status
                    (constantly {})

                    providers/provider-limits-safe
                    (constantly {})]

        (doseq [[provider expected] [[{:id :custom
                                       :base-url "https://gateway.example.com/v1"
                                       :api-style :openai_responses
                                       :models [{:name "gpt-6-astra"}
                                                {:name "gpt-5.6-sol" :api-style :openai}]}
                                      ["openai-text" nil]]
                                     [{:id :custom
                                       :base-url "https://gateway.example.com/v1"
                                       :responses-path "/responses"
                                       :models [{:name "gpt-6-astra"}]} ["openai-text"]]
                                     [{:id :openai :models [{:name "gpt-4o"}]} [nil]]
                                     [{:id :openai-codex :models []} []]]]
          (let [row (wire/->wire (#'providers-api/router-provider-entry provider nil nil true))
                details (get row "model_details")]

            (expect (= expected (mapv #(get % "verbosity_style") details)))
            (expect (= (get row "models") (mapv #(get % "name") details)))
            (when (= :openai (:id provider))
              (expect (false? (get-in details [0 "is_reasoning_effort_configurable"])))))))))

;; Regression: the fleet was probed one provider at a time, so /v1/router cost
;; the SUM of every live auth/limits probe — about a minute on eight providers,
;; past the companion's 30s request bound, and its Providers screen never left
;; "Checking provider sign-in…".
(defdescribe
  router-handler-probes-the-fleet-in-parallel
  (it
    "GET /v1/router costs the SLOWEST provider probe, not their sum"
    (let [fleet
          (mapv (fn [i]
                  {:id (keyword (str "p" i))
                   :base-url "https://gateway.example.com/v1"
                   :models [{:name "m"}]})
                (range 6))

          probe-ms
          300]

      (with-redefs-fn {#'providers/picker-fleet (constantly fleet)
                       #'providers/default-selection (constantly nil)
                       #'providers/fallback-selection (constantly nil)
                       #'providers/provider-status (fn [_]
                                                     (Thread/sleep (long probe-ms))
                                                     {:is-authenticated true :source :auth-file})
                       #'providers/provider-limits-safe
                       (constantly {:status :ok :static {} :dynamic {:limits []}})}
        (fn []
          (let [t0
                (System/currentTimeMillis)

                resp
                (#'providers-api/router-handler {})

                elapsed
                (- (System/currentTimeMillis) t0)

                provs
                (get (wire/parse-json (:body resp)) "providers")]

            (expect (= 200 (:status resp)))
            ;; every row is present, in fleet order
            (expect (= ["p0" "p1" "p2" "p3" "p4" "p5"] (mapv #(get % "id") provs)))
            (expect (< elapsed (* 3 (long probe-ms)))
                    (str "fleet probed serially: " elapsed "ms"))))))))

(defdescribe
  router-default-handler-tags-primary-and-fallback
  (it
    "router default handler tags primary and fallback"
    (let [saved
          (atom nil)

          cleared
          (atom 0)

          fleet
          [{:id :anthropic-coding-plan :models [{:name "claude-fable-5"}]}
           {:id :zai-coding-plan :models [{:name "glm-5.2"}]}]

          body
          (fn [m]
            {:body (java.io.ByteArrayInputStream. (.getBytes (wire/json-str m) "UTF-8"))})

          patch!
          (fn [m]
            (#'providers-api/router-default-handler (body m)))]

      (with-redefs [providers/picker-fleet
                    (constantly fleet)

                    providers/default-selection
                    (constantly {:provider-id :anthropic-coding-plan :model "claude-fable-5"})

                    providers/fallback-selection
                    (constantly {:provider-id :zai-coding-plan :model "glm-5.2"})

                    providers/save-default-selection!
                    (fn [provider model source]
                      (reset! saved [:primary provider model source])
                      {:provider-id :anthropic-coding-plan :model "claude-fable-5"})

                    providers/save-fallback-selection!
                    (fn [provider model source]
                      (reset! saved [:fallback provider model source])
                      {:provider-id :zai-coding-plan :model "glm-5.2"})

                    providers/clear-fallback-selection!
                    (fn [_source]
                      (swap! cleared inc)
                      nil)]

        ;; a roleless PATCH still tags the PRIMARY, and the answer carries BOTH tags
        (let [resp
              (patch! {"provider" "anthropic-coding-plan" "model" "claude-fable-5"})

              out
              (wire/parse-json (:body resp))]

          (expect (= 200 (:status resp)))
          (expect (= [:primary "anthropic-coding-plan" "claude-fable-5" :gateway] @saved))
          (expect (= "anthropic-coding-plan" (get out "default_provider")))
          (expect (= "claude-fable-5" (get out "default_model")))
          (expect (= "zai-coding-plan" (get out "fallback_provider")))
          (expect (= "glm-5.2" (get out "fallback_model"))))
        ;; role fallback writes the FALLBACK tag and never touches the primary one
        (let [resp (patch! {"provider" "zai-coding-plan" "model" "glm-5.2" "role" " FallBack "})]
          (expect (= 200 (:status resp)))
          (expect (= [:fallback "zai-coding-plan" "glm-5.2" :gateway] @saved)))
        ;; a blank fallback CLEARS the tag, while a blank primary stays a 400
        (expect (= 200 (:status (patch! {"role" "fallback" "model" "  "}))))
        (expect (= 1 @cleared))
        (expect (= 400 (:status (patch! {"model" "   "}))))
        ;; an unknown role is refused before anything is written
        (reset! saved :untouched)
        (let [resp
              (patch! {"provider" "zai-coding-plan" "model" "glm-5.2" "role" "tertiary"})

              err
              (get (wire/parse-json (:body resp)) "error")]

          (expect (= 400 (:status resp)))
          (expect (= "invalid-request" (get err "type")))
          (expect (= :untouched @saved)))
        ;; the daemon's refusal — a fallback on the primary's provider — becomes a 400
        (with-redefs [providers/save-fallback-selection!
                      (fn [_provider _model _source]
                        (throw (ex-info
                                 "the fallback must name a DIFFERENT provider than the primary"
                                 {:type :vis/invalid-fallback-provider})))]
          (let [resp (patch! {"provider" "anthropic-coding-plan"
                              "model" "claude-fable-5"
                              "role" "fallback"})
                err (get (wire/parse-json (:body resp)) "error")]

            (expect (= 400 (:status resp)))
            (expect (re-find #"DIFFERENT provider" (get err "message")))))))))

(defdescribe gateway-prometheus-runtime-metrics-test
             (it "gateway prometheus runtime metrics"
                 (let [text ((rv 'prometheus-text)
                              {:tokens-input 12
                               :tokens-output 7
                               :turns-executing 2
                               :turns-waiting 1
                               :env-cache-size 3
                               :env-memory-pressure true
                               :jvm-heap-used-bytes 1024
                               :jvm-gc-count-total 4
                               :jvm-thread-count 9})]
                   ;; preserves the existing labelled token counter
                   (expect (re-find #"vis_turn_tokens_total\{kind=\"input\"\} 12" text))
                   (expect (re-find #"vis_turn_tokens_total\{kind=\"output\"\} 7" text))
                   ;; exports resource-pressure and concurrency gauges
                   (expect (re-find #"vis_turns_executing 2" text))
                   (expect (re-find #"vis_turns_waiting 1" text))
                   (expect (re-find #"vis_env_memory_pressure 1" text))
                   (expect (re-find #"vis_jvm_heap_used_bytes 1024" text)))))

(defn- non-loopback-ipv4
  "First live non-loopback IPv4 address, i.e. the kind of concrete host `--pair`
   binds. nil on a machine with no such interface, where the mirror is moot."
  []
  (->> (enumeration-seq (java.net.NetworkInterface/getNetworkInterfaces))
       (filter (fn [^java.net.NetworkInterface i]
                 (.isUp i)))
       (mapcat (fn [^java.net.NetworkInterface i]
                 (enumeration-seq (.getInetAddresses i))))
       (filter (fn [a]
                 (instance? java.net.Inet4Address a)))
       (remove (fn [^java.net.InetAddress a]
                 (.isLoopbackAddress a)))
       (map (fn [^java.net.InetAddress a]
              (.getHostAddress a)))
       first))

(defn- bound-port
  [^org.eclipse.jetty.server.Server server]
  (.getLocalPort ^org.eclipse.jetty.server.ServerConnector (first (.getConnectors server))))

(defdescribe
  web-app-is-served-before-the-token-gate
  (it "web app is served before the token gate"
      ;; `vis-agent web` opens a browser on the gateway root. A navigation carries no
      ;; token and no protocol header, so the app's files answer before both gates,
      ;; through the real Jetty adapter, while the API behind them stays token-gated.
      (let [dir (io/file (System/getProperty "java.io.tmpdir") (str "vis-web-" (random-uuid)))]
        (try (io/make-parents (io/file dir "assets" "index-abc123.js"))
             (spit (io/file dir "index.html") "<!doctype html><title>Vis</title>")
             (spit (io/file dir "assets" "index-abc123.js") "console.log('vis');")
             (with-server-state!
               {:require-token? true}
               (fn []
                 (with-redefs [web/configured-dir (constantly (str dir))]
                   (let [server (jetty/run-jetty ((rv 'app) "sekret" [])
                                                 {:port 0 :host "127.0.0.1" :join? false})
                         base (str "http://127.0.0.1:" (bound-port server))]

                     (try (let [page (http/get (str base "/") {:throw false})
                                head (http/head (str base "/") {:throw false})
                                asset (http/get (str base "/assets/index-abc123.js") {:throw false})
                                api (http/get (str base "/v1/sessions")
                                              {:headers {"X-Vis-Protocol"
                                                         (str gateway-contract/protocol-version)}
                                               :throw false})]

                            (expect (= 200 (:status page)))
                            (expect (= "<!doctype html><title>Vis</title>" (:body page)))
                            (expect (= "text/html; charset=utf-8"
                                       (get-in page [:headers "content-type"])))
                            (expect (= "no-cache" (get-in page [:headers "cache-control"])))
                            (expect (= "DENY" (get-in page [:headers "x-frame-options"])))
                            (expect (= 200 (:status head)))
                            (expect (str/blank? (:body head)))
                            (expect (= 200 (:status asset)))
                            (expect (= "text/javascript; charset=utf-8"
                                       (get-in asset [:headers "content-type"])))
                            (expect (= "public, max-age=31536000, immutable"
                                       (get-in asset [:headers "cache-control"])))
                            (expect (= 401 (:status api))))
                          (finally (.stop server)))))))
             (finally (run! io/delete-file (reverse (file-seq dir))))))))

(defdescribe
  attachment-bytes-never-run-as-the-gateway-origin
  (it "attachment bytes never run as the gateway origin"
      ;; The web app shares the gateway origin and keeps saved gateway tokens there,
      ;; so an agent-made HTML file opened from its URL must render sandboxed.
      (with-redefs [state/user-iteration-attachments
                    (constantly [{:media-type "text/html"}])

                    state/attachment-bytes
                    (constantly (.getBytes "<script>alert(1)</script>" "UTF-8"))]

        (let [response (#'turns-api/attachment-bytes-handler
                        {:path-params {:sid (str (random-uuid)) :iid "iteration" :idx "0"}})]
          (expect (= 200 (:status response)))
          (expect (= "text/html" (get-in response [:headers "Content-Type"])))
          (expect (= "sandbox" (get-in response [:headers "Content-Security-Policy"])))
          (expect (= "nosniff" (get-in response [:headers "X-Content-Type-Options"])))))))

(defdescribe
  pair-bind-still-answers-on-loopback
  (it
    "a concrete non-loopback bind (what --pair picks) also serves 127.0.0.1,
            so the local TUI attaches to THIS gateway instead of seeing a free port
            and spawning a second one"
    (if-let [host (non-loopback-ipv4)]
      ;; CI 34408559005: bind the real connector on port zero, never probe then release a port.
      (let [server (jetty/run-jetty (constantly {:status 200 :headers {} :body "ok"})
                                    {:port 0
                                     :host host
                                     :join? false
                                     :configurator
                                     (fn [^org.eclipse.jetty.server.Server server]
                                       ;; Keep the primary bound while the configurator adds the same-port mirror.
                                       (.open ^org.eclipse.jetty.server.ServerConnector
                                              (first (.getConnectors server)))
                                       (((rv 'gateway-configurator) (bound-port server)) server))})
            port (bound-port server)]

        (try (expect (= #{host "127.0.0.1"}
                        (set (map (fn [^org.eclipse.jetty.server.ServerConnector c]
                                    (.getHost c))
                                  (.getConnectors ^org.eclipse.jetty.server.Server server)))))
             (expect (= "ok" (slurp (str "http://" host ":" port "/"))))
             (expect (= "ok" (slurp (str "http://127.0.0.1:" port "/"))))
             (finally (.stop ^org.eclipse.jetty.server.Server server))))
      (expect (nil? (non-loopback-ipv4)) "no non-loopback interface here; nothing to mirror"))))

(defdescribe
  the-adapters-own-jvm-shutdown-hook-stays-off
  (it
    "`ring.adapter.jetty9` builds its Server with `stopAtShutdown` ON, which registers
            Jetty's own JVM hook to `.stop` it. `stop!` is meant to be the only shutdown
            path: it cancels and then DRAINS in-flight turns before the socket goes, so a
            second hook racing it would guillotine exactly the mid-turn work that drain
            exists to save"
    (let [server
          (jetty/run-jetty (constantly {:status 200 :headers {} :body "ok"})
                           {:port 0
                            :host "127.0.0.1"
                            :join? false
                            ;; nil mirror-port: loopback needs no mirror, and this is
                            ;; the shape `start!` passes on a default bind.
                            :configurator ((rv 'gateway-configurator) nil)})

          port
          (bound-port server)]

      (try (expect (false? (.getStopAtShutdown ^org.eclipse.jetty.server.Server server)))
           (expect (= "ok" (slurp (str "http://127.0.0.1:" port "/"))))
           (finally (.stop ^org.eclipse.jetty.server.Server server))))))

(defdescribe
  a-flushed-frame-reaches-the-socket-before-the-body-returns
  (it
    "every SSE stream here is a `StreamableResponseBody` that writes a frame, flushes,
            and only returns when the session ends. An adapter that buffered the body
            instead of pushing each flush would hold a whole turn's events back and
            deliver them in one burst at the end — a live stream that is not live"
    (let [release
          (promise)

          server
          (jetty/run-jetty (constantly {:status 200
                                        :headers {"Content-Type" "text/event-stream"}
                                        :body
                                        (reify
                                          ring-protocols/StreamableResponseBody
                                            (write-body-to-stream [_ _ output-stream]
                                              (let [^java.io.OutputStream out output-stream]
                                                (.write out (.getBytes "data: first\n\n" "UTF-8"))
                                                (.flush out)
                                                ;; Park exactly as a live stream parks between events. The
                                                ;; timeout only keeps a failing run from hanging the suite.
                                                (deref release 5000 :timed-out)
                                                (.write out (.getBytes "data: last\n\n" "UTF-8"))
                                                (.flush out)
                                                (.close out))))})
                           {:port 0 :host "127.0.0.1" :join? false})

          port
          (bound-port server)]

      (try (with-open [socket (java.net.Socket. "127.0.0.1" (int port))]
             (.setSoTimeout socket 5000)
             (doto (.getOutputStream socket)
               (.write (.getBytes "GET / HTTP/1.1\r\nHost: 127.0.0.1\r\nConnection: close\r\n\r\n"
                                  "UTF-8"))
               (.flush))
             (let [reader (java.io.BufferedReader.
                            (java.io.InputStreamReader. (.getInputStream socket) "UTF-8"))
                   ;; Skip status line, headers and any chunk-size lines.
                   read-frame
                   (fn []
                     (loop [n 0]
                       (when (< n 64)
                         (when-let [line (.readLine reader)]
                           (if (str/starts-with? line "data: ") line (recur (inc n)))))))]

               (expect (= "data: first" (read-frame))
                       "the first frame must arrive while the body thread is still parked")
               (deliver release :go)
               (expect (= "data: last" (read-frame)))))
           (finally (.stop ^org.eclipse.jetty.server.Server server))))))

(defdescribe sse-cursor-clamps-a-client-ahead-of-the-gateway-counter
             (it "sse cursor clamps a client ahead of the gateway counter"
                 ;; A client keeps its replay cursor as a monotonic max across reconnects, but
                 ;; the gateway's seq counter is per-process: a restarted daemon (or a session
                 ;; entry seeded at zero) numbers BELOW what the app already saw. Serving that
                 ;; stale cursor verbatim seeds the connection's `seq > last-seq` dedup guard
                 ;; above every frame the next turn will ever emit — a connected, heartbeating
                 ;; stream that silently delivers NOTHING until the app is killed.
                 (let [resolve-cursor
                       (rv 'resolve-sse-cursor)

                       registry
                       @(ns-resolve 'com.blockether.vis.internal.gateway.state 'registry)

                       sid
                       (java.util.UUID/randomUUID)]

                   (swap! registry assoc (str sid) {:next-seq 12})
                   (try
                     ;; a cursor past the high-water resolves to the live tail
                     (expect (= 12 (resolve-cursor sid 5000)))
                     ;; an in-range cursor is honoured verbatim
                     (expect (= 5 (resolve-cursor sid 5)))
                     ;; the negative live-only sentinel is unchanged
                     (expect (= 12 (resolve-cursor sid -1)))
                     (finally (swap! registry dissoc (str sid)))))))

(defdescribe
  sse-cursor-rewinds-a-client-below-the-replay-floor
  (it "sse cursor rewinds a client below the replay floor"
      ;; The replay ring is bounded, so ONE long turn evicts thousands of its own
      ;; frames. A client that dropped out mid-turn was served the surviving TAIL
      ;; verbatim: megabytes of deltas for blocks whose `content.block.started` had
      ;; been evicted, a picture neither side could tell from a complete one, and a
      ;; reconnect that answered the same partial ring again.
      (let [resolve-cursor
            (rv 'resolve-sse-cursor)

            registry
            @(ns-resolve 'com.blockether.vis.internal.gateway.state 'registry)

            sid
            (java.util.UUID/randomUUID)

            idle-sid
            (java.util.UUID/randomUUID)]

        (swap! registry assoc
          (str sid)
          {:next-seq 900
           :evicted-through 400
           :current-turn "t-run"
           :turns {"t-run" {:event_start_seq 120}}}
          (str idle-sid)
          {:next-seq 900 :evicted-through 400})
        (try
          ;; a cursor the ring already evicted replays the running turn whole
          (expect (= 119 (resolve-cursor sid 300)))
          ;; a cursor still inside the ring is honoured verbatim
          (expect (= 500 (resolve-cursor sid 500)))
          ;; with no turn running it resolves to the live tail, leaving history to the transcript
          (expect (= 900 (resolve-cursor idle-sid 300)))
          (finally (swap! registry dissoc (str sid) (str idle-sid)))))))

(defdescribe
  mcp-kill-start-and-oauth-routes-test
  (it
    "runtime kill/start and every headless OAuth leg answer through the ring layer"
    (let [flow
          {"flow_id" "f-1"
           "server" "remote"
           "kind" "pkce"
           "url" "https://auth.example.test/authorize"
           "status" "pending"}

          calls
          (atom [])]

      (with-redefs-fn
        {#'gw-http/body-json (constantly {"flow_id" "f-1"
                                          "input" "https://cb.example.test/?code=abc"
                                          "callback_mode" "app"})
         #'mcp-core/kill-gateway-server! (fn [name]
                                           (swap! calls conj [:kill name])
                                           {"name" name "is_killed" true})
         #'mcp-core/start-gateway-server! (fn [name]
                                            (swap! calls conj [:start name])
                                            {"name" name "is_killed" false})
         #'mcp-core/start-gateway-server-auth! (fn [name opts]
                                                 (swap! calls conj [:auth-start name opts])
                                                 flow)
         #'mcp-core/complete-gateway-server-auth! (fn [flow-id input]
                                                    (swap! calls conj
                                                      [:auth-complete flow-id input])
                                                    (assoc flow "status" "ok"))
         #'mcp-core/poll-gateway-server-auth! (fn [flow-id]
                                                (swap! calls conj [:auth-poll flow-id])
                                                flow)
         #'mcp-core/cancel-gateway-server-auth! (fn [flow-id]
                                                  {"flow_id" flow-id "is_cancelled" true})
         #'mcp-core/logout-gateway-server-auth! (fn [name]
                                                  {"server" name "is_authorized" false})}
        (fn []
          (let [params {:path-params {:name "remote"}}]
            (expect (= 200 (:status (#'mcp-api/kill-mcp-server-handler params))))
            (expect (= 200 (:status (#'mcp-api/start-mcp-server-handler params))))
            ;; The flow crosses the wire snake_cased, and the id is the ONLY handle
            ;; a client ever holds: the verifier and the token stay in the daemon.
            (let [started (wire/parse-json (:body (#'mcp-api/mcp-auth-start-handler params)))]
              (expect (= "f-1" (get started "flow_id")))
              (expect (= "pkce" (get started "kind")))
              (expect (nil? (get started "code_verifier"))))
            (expect (= "ok"
                       (get (wire/parse-json (:body (#'mcp-api/mcp-auth-complete-handler params)))
                            "status")))
            (expect (= 200 (:status (#'mcp-api/mcp-auth-poll-handler params))))
            (expect (= 200 (:status (#'mcp-api/mcp-auth-cancel-handler params))))
            (expect (= 200 (:status (#'mcp-api/mcp-auth-logout-handler params))))
            (expect
              (= [[:kill "remote"] [:start "remote"] [:auth-start "remote" {:callback-mode "app"}]
                  [:auth-complete "f-1" "https://cb.example.test/?code=abc"] [:auth-poll "f-1"]]
                 @calls))))))))

(defdescribe
  mcp-oauth-flow-errors-map-to-status-test
  (it "a missing flow id is 400 and an expired one 404 — never a 500"
      (with-redefs-fn {#'gw-http/body-json (constantly {})}
        (fn []
          (expect (= 400
                     (:status (#'mcp-api/mcp-auth-poll-handler {:path-params {:name "remote"}}))))))
      (with-redefs-fn {#'gw-http/body-json (constantly {"flow_id" "gone"})
                       #'mcp-core/poll-gateway-server-auth!
                       (fn [flow-id]
                         (throw (ex-info "Unknown or expired MCP auth flow"
                                         {:type :mcp/oauth-flow-not-found :flow-id flow-id})))}
        (fn []
          (let [response (#'mcp-api/mcp-auth-poll-handler {:path-params {:name "remote"}})]
            (expect (= 404 (:status response)))
            (expect (= "oauth-flow-not-found"
                       (get-in (wire/parse-json (:body response)) ["error" "type"]))))))))

(defdescribe
  mcp-validation-errors-preserve-the-original-failure-test
  (it "mcp validation errors preserve the original failure"
      (doseq [type [:mcp/http-error :mcp/oauth-required :mcp/protocol :mcp/unknown]]
        (with-redefs-fn {#'gw-http/body-json (constantly {"name" "remote" "server" {}})
                         #'mcp-core/test-gateway-server!
                         (fn [& _]
                           (throw (ex-info "MCP remote HTTP 401 on initialize"
                                           {:type type :body "private upstream response"})))}
          (fn []
            (let [response (#'mcp-api/test-mcp-server-handler {})
                  body (wire/parse-json (:body response))]

              (expect (= 400 (:status response)))
              (expect (= (name type) (get-in body ["error" "type"])))
              (expect (= "MCP remote HTTP 401 on initialize" (get-in body ["error" "message"])))
              (expect (not (str/includes? (:body response) "private upstream response")))))))))

(defdescribe
  admin-stop-handler-names-its-requester
  (it
    "POST /v1/admin/stop takes the whole ring request (so the log can name who killed a busy daemon) and still stops"
    (let [stops (atom 0)]
      (with-stop-stub! stops
                       {}
                       (fn []
                         (with-server-state!
                           {:managed? false :clients {} :sse-clients #{}}
                           (fn []
                             (let [res ((rv 'stop-handler)
                                         {:remote-addr "127.0.0.1"
                                          :headers {"user-agent" "vis-agent/test"}})]
                               (expect (= 200 (:status res)))
                               (expect (re-find #"stopping" (str (:body res))))
                               (expect (loop [n 0]
                                         (cond (pos? (long @stops)) true
                                               (> n 100) false
                                               :else (do (Thread/sleep 20) (recur (inc n)))))
                                       "the handler stops the daemon asynchronously")))))))))

(defdescribe
  signal-forensics-is-idempotent-and-restorable
  (it
    "the daemon installs signal handlers once (so an unexplained death names its signal) and can restore the JVM defaults"
    (let [install
          (rv 'install-signal-forensics!)

          restore
          (rv 'restore-signal-forensics!)

          installed
          (install)]

      (try (expect (map? installed))
           (expect (contains? installed "TERM"))
           (expect (contains? installed "INT"))
           (expect (nil? (install)) "a second install is a no-op")
           (finally (restore installed)))
      (expect (nil? @@(rv 'signal-forensics))
              "restoring clears the installed marker so a later daemon can re-install"))))

(defdescribe
  a-stray-signal-never-stops-a-detached-daemon
  (it "policy: only a terminal this daemon OWNS may Ctrl-C it; SIGTERM always stops"
      (let [disposition (rv 'signal-disposition)]
        (expect (= :exit (disposition {:signal "TERM" :managed? true :interactive? false}))
                "SIGTERM is the deliberate stop and must keep draining")
        (expect (= :exit (disposition {:signal "TERM" :managed? false :interactive? true})))
        (expect (= :exit (disposition {:signal "INT" :managed? false :interactive? true}))
                "Ctrl-C in the tab a FOREGROUND gateway runs in still stops it")
        (expect (= :exit (disposition {:signal "HUP" :managed? false :interactive? true})))
        (expect (= :ignore (disposition {:signal "INT" :managed? true :interactive? true}))
                "a managed daemon is nobody's foreground job, whatever stdio it inherited")
        (expect (= :ignore (disposition {:signal "INT" :managed? false :interactive? false}))
                "no controlling terminal => the INT came from someone else's process group")
        (expect (= :ignore (disposition {:signal "HUP" :managed? true :interactive? false})))))
  (it
    "a REAL SIGINT delivered to a managed daemon is logged and survived"
    (let [install
          (rv 'install-signal-forensics!)

          restore
          (rv 'restore-signal-forensics!)

          disposition
          (rv 'signal-disposition)

          installed
          (install {:managed? true})]

      ;; GUARD: fire an actual signal only once the pure policy proves this JVM's
      ;; handler cannot exit — `:managed? true` is what we installed with, so the
      ;; `:interactive?` the handler captured cannot change the verdict. A regression
      ;; fails on this assertion instead of killing the test runner. `installed` is nil
      ;; when another test already owns the handlers; then there is nothing to prove.
      (expect (= :ignore (disposition {:signal "INT" :managed? true :interactive? true})))
      (expect (= :ignore (disposition {:signal "INT" :managed? true :interactive? false})))
      (try
        (when (and (seq installed)
                   (= :ignore (disposition {:signal "INT" :managed? true :interactive? true}))
                   (= :ignore (disposition {:signal "INT" :managed? true :interactive? false})))
          (let [pid
                (.pid (java.lang.ProcessHandle/current))

                p
                (.start (ProcessBuilder. ^java.util.List ["/bin/sh" "-c" (str "kill -INT " pid)]))]

            (.waitFor p)
            (Thread/sleep 300)
            (expect (.isAlive (java.lang.ProcessHandle/current))
                    "a stray SIGINT must leave the daemon serving every other session")
            ;; Non-vacuous: the SAME `kill` shape against a process with the DEFAULT
            ;; disposition really does kill it, so surviving above is the handler.
            (let [ctl
                  (.start (ProcessBuilder. ^java.util.List
                                           ["/bin/sh" "-c" "kill -INT $$; sleep 2; echo survived"]))

                  out
                  (slurp (.getInputStream ctl))]

              (.waitFor ctl)
              (expect (not (str/includes? out "survived"))
                      "the control process must die of the very same signal"))))
        (finally (restore installed))))))

(defdescribe
  the-shutdown-hook-names-who-called-system-exit
  (it "the shutdown hook names who called system exit"
      (let [culprit (rv 'exit-culprit)]
        ;; the thread parked in System/exit is named; JDK plumbing is dropped
        (let [r (culprit
                  {"main" ["java.base/java.lang.Object.wait0(Native Method)"
                           "java.base/java.lang.Thread.join(Thread.java:1327)"
                           "java.base/java.lang.Shutdown.runHooks(Shutdown.java:130)"]
                   "vis-turn-3"
                   ["java.base/java.lang.Shutdown.exit(Shutdown.java:176)"
                    "java.base/java.lang.Runtime.exit(Runtime.java:112)"
                    "java.base/java.lang.System.exit(System.java:1901)"
                    "java.base/jdk.internal.reflect.DirectMethodHandleAccessor.invoke(x:1)"
                    "com.blockether.vis.internal.language.clojure.reflection$c.invoke(refl.clj:120)"
                    "clojure.lang.AFn.run(AFn.java:22)"]})]
          (expect (= "vis-turn-3" (get r "thread"))
                  "the thread that called exit, not the one running the hooks")
          (expect (=
                    "com.blockether.vis.internal.language.clojure.reflection$c.invoke(refl.clj:120)"
                    (first (get r "frames")))
                  "the first frame must be the CALLER, module prefixes and all")
          (expect (= 2 (count (get r "frames")))))
        ;; a thread merely running the hooks is not accused, and nothing exiting is nil
        (expect (nil? (culprit {"main" ["java.base/java.lang.Shutdown.runHooks(Shutdown.java:130)"
                                        "clojure.main$main.invoke(main.clj:1)"]})))
        (expect (nil? (culprit {}))))))

;;; ── Fleet membership over the wire ──────────────────────────────────────────
;; Adding a provider used to be TUI-only: the gateway exposed operations on
;; providers that were already configured and nothing that could create one, so
;; the companion could never grow a fleet.

(defn- with-stub-fleet!
  "Router payload stubs so a mutation handler can echo the fleet back: the PROBING
   reads `GET /v1/router` pays, and the cached ones a mutation answers with."
  [fleet f]
  (with-redefs-fn {#'providers/picker-fleet (constantly fleet)
                   #'providers/default-selection (constantly nil)
                   #'providers/fallback-selection (constantly nil)
                   #'providers/provider-status (constantly {:is-authenticated false})
                   #'providers/provider-limits-safe (constantly nil)
                   #'providers/provider-status-cached (constantly {:is-authenticated false})
                   #'provider-limits/limits-without-fetching (constantly nil)
                   #'providers/refresh-models-async! (constantly nil)}
    f))

(defdescribe
  provider-presets-handler-lists-what-can-still-be-added
  (it "provider presets handler lists what can still be added"
      (with-redefs-fn {#'providers/available-presets (constantly
                                                       [{:id :lmstudio
                                                         :label "LM Studio"
                                                         :base-url "http://localhost:1234/v1"
                                                         :api-style :openai
                                                         :default-models ["local-model"]}
                                                        {:id :anthropic-coding-plan
                                                         :label "Anthropic"
                                                         :default-models ["claude-sonnet-5"]}])}
        (fn []
          (let [presets
                (get (wire/parse-json (:body (#'providers-api/provider-presets-handler {})))
                     "presets")

                [local oauth]
                presets]

            (expect (= ["lmstudio" "anthropic-coding-plan"] (mapv #(get % "id") presets)))
            (expect (= "LM Studio" (get local "label")))
            (expect (= "http://localhost:1234/v1" (get local "base_url")))
            (expect (= "openai" (get local "api_style")))
            (expect (= ["local-model"] (get local "models")))
            ;; a local runtime needs no credential and OWNS its base url
            (expect (= "none" (get local "auth_kind")))
            (expect (true? (get local "is_local")))
            (expect (= "oauth" (get oauth "auth_kind")))
            (expect (false? (get oauth "is_local"))))))))

(defdescribe
  add-provider-handler-writes-the-preset-into-the-fleet
  (it
    "add provider handler writes the preset into the fleet"
    (let [added
          (atom nil)

          post!
          (fn [m]
            (#'providers-api/add-provider-handler (json-body m)))]

      (with-redefs-fn {#'catalog/template (fn [pid]
                                            (when (= :lmstudio pid)
                                              {:id :lmstudio
                                               :label "LM Studio"
                                               :base-url "http://localhost:1234/v1"
                                               :api-style :openai
                                               :default-models ["local-model"]}))
                       #'providers/configured-providers (constantly [{:id :zai-coding-plan}])
                       #'providers/add-config-provider! (fn [cfg source]
                                                          (reset! added [cfg source]))}
        (fn []
          (with-stub-fleet!
            [{:id :lmstudio :models [{:name "local-model"}]}]
            (fn []
              ;; a local provider takes the base url the caller owns, trailing slash and all
              (let [resp (post! {:id "lmstudio" :base_url "http://10.0.0.5:1234/v1/"})]
                (expect (= 200 (:status resp)))
                (expect (= [{:id :lmstudio
                             :models [{:name "local-model"}]
                             :base-url "http://10.0.0.5:1234/v1"
                             :api-style :openai} :gateway]
                           @added))
                ;; the answer IS the new fleet — the caller repaints from it
                (expect (= ["lmstudio"]
                           (mapv #(get % "id") (get (wire/parse-json (:body resp)) "providers")))))
              ;; no base url keeps the preset default
              (reset! added nil)
              (expect (= 200 (:status (post! {:id "lmstudio"}))))
              (expect (= "http://localhost:1234/v1" (:base-url (first @added))))
              ;; an unknown preset is a 404 and writes nothing
              (reset! added nil)
              (let [resp (post! {:id "not-a-provider"})]
                (expect (= 404 (:status resp)))
                (expect (= "unknown-provider"
                           (get-in (wire/parse-json (:body resp)) ["error" "type"])))
                (expect (nil? @added)))
              ;; a blank id is a 400
              (expect (= 400 (:status (post! {:id "   "}))))
              ;; an already-configured provider is a 409, never a duplicate row
              (reset! added nil)
              (with-redefs-fn {#'providers/configured-providers (constantly [{:id :lmstudio}])}
                (fn []
                  (expect (= 409 (:status (post! {:id "lmstudio"}))))
                  (expect (nil? @added)))))))))))

(defdescribe router-handler-reports-extension-ownership
             (it "router handler reports extension ownership"
                 (with-redefs [providers/managed? #(= :extension-owned %)]
                   (with-stub-fleet!
                     [{:id :extension-owned :models []} {:id :own-key :models []}]
                     (fn []
                       (let [response (#'providers-api/router-handler {})
                             rows (get (wire/parse-json (:body response)) "providers")]

                         (expect (= 200 (:status response)))
                         (expect (= [true false] (mapv #(get % "is_managed") rows)))))))))

(defdescribe
  remove-provider-handler-refuses-extension-owned-providers
  (it "remove provider handler refuses extension owned providers"
      (with-redefs [providers/remove-provider!
                    (fn [& _]
                      (throw (ex-info "Provider is managed by its extension and cannot be removed."
                                      {:type :provider/managed :provider-id :extension-owned})))]
        (let [response (#'providers-api/remove-provider-handler
                        {:path-params {:provider-id "extension-owned"}})
              payload (wire/parse-json (:body response))]

          (expect (= 409 (:status response)))
          (expect (= "provider-managed" (get-in payload ["error" "type"])))
          (expect (= "Provider is managed by its extension and cannot be removed."
                     (get-in payload ["error" "message"])))))))

(defdescribe
  remove-provider-handler-drops-the-provider-and-echoes-the-fleet
  (it "remove provider handler drops the provider and echoes the fleet"
      (let [removed (atom nil)]
        (with-redefs-fn {#'providers/remove-provider! (fn [pid source]
                                                        (reset! removed [pid source])
                                                        (= :lmstudio pid))}
          (fn []
            (with-stub-fleet!
              [{:id :zai-coding-plan :models [{:name "glm-5.2"}]}]
              (fn []
                (let [resp (#'providers-api/remove-provider-handler
                            {:path-params {:provider-id "lmstudio"}})
                      payload (wire/parse-json (:body resp))]

                  (expect (= 200 (:status resp)))
                  (expect (= [:lmstudio :gateway] @removed))
                  (expect (true? (get payload "is_removed")))
                  (expect (= ["zai-coding-plan"] (mapv #(get % "id") (get payload "providers")))))
                ;; removing what is not configured is not an error
                ;; `is_removed` reports the outcome, not the mechanism: a name the
                ;; fleet never carried is already gone.
                (let [resp (#'providers-api/remove-provider-handler
                            {:path-params {:provider-id "ghost"}})]
                  (expect (= 200 (:status resp)))
                  (expect (true? (get (wire/parse-json (:body resp)) "is_removed")))))))))))

(defdescribe
  fleet-mutations-answer-without-probing-any-provider
  (it "fleet mutations answer without probing any provider"
      ;; Tapping Add used to answer only after re-probing EVERY configured provider's
      ;; auth and quota endpoints — seconds of spinner before the API-key box, which
      ;; needs neither. A mutation repaints from what the daemon already knows.
      (let [probes
            (atom 0)

            cached
            (atom 0)

            refreshed
            (atom [])

            fleet
            [{:id :zai-coding-plan :models [{:name "glm-5.2"}]}]]

        (with-redefs-fn
          {#'providers/picker-fleet (constantly fleet)
           #'providers/default-selection (constantly nil)
           #'providers/fallback-selection (constantly nil)
           #'providers/provider-status (fn [_]
                                         (swap! probes inc)
                                         {:is-authenticated true :auth-state :verified})
           #'providers/provider-limits-safe (fn [_]
                                              (swap! probes inc)
                                              nil)
           #'providers/provider-status-cached (fn [_]
                                                (swap! cached inc)
                                                {:is-authenticated true :auth-state :verified})
           #'provider-limits/limits-without-fetching (fn [_]
                                                       (swap! cached inc)
                                                       nil)
           #'catalog/template (fn [pid]
                                (when (= :lmstudio pid)
                                  {:id :lmstudio
                                   :label "LM Studio"
                                   :base-url "http://localhost:1234/v1"
                                   :api-style :openai
                                   :default-models ["local-model"]}))
           #'providers/configured-providers (constantly [])
           #'providers/add-config-provider! (fn [& _]
                                              nil)
           #'providers/remove-provider! (fn [& _]
                                          true)
           #'providers/refresh-models-async! (fn [provider-id source]
                                               (swap! refreshed conj [provider-id source])
                                               nil)}
          (fn []
            ;; POST /v1/providers
            (let [resp (#'providers-api/add-provider-handler (json-body {:id "lmstudio"}))]
              (expect (= 200 (:status resp)))
              (expect (= ["zai-coding-plan"]
                         (mapv #(get % "id") (get (wire/parse-json (:body resp)) "providers")))
                      "the answer is still the whole fleet the caller repaints from"))
            (expect (= [[:lmstudio :gateway]] @refreshed)
                    "the live catalog is pulled off-thread, never inside this answer")
            ;; DELETE /v1/providers/:provider-id
            (expect (= 200
                       (:status (#'providers-api/remove-provider-handler
                                 {:path-params {:provider-id "lmstudio"}}))))
            (expect (zero? @probes) "no fleet mutation may wait on a provider's network")
            (expect (= 4 @cached)
                    "every row still carries its status and limits, from what is already known")
            ;; GET /v1/router is the read that still asks live
            (expect (= 200 (:status (#'providers-api/router-handler {}))))
            (expect (= 2 @probes)))))))

(defdescribe
  a-provider-recheck-also-refreshes-its-catalog
  (it "a provider recheck also refreshes its catalog"
      ;; A fleet added by an older build keeps advertising that build's models:
      ;; config is what every `/v1/router` row reads and nothing ever rewrote it.
      ;; A recheck is the user looking straight at the provider, so the live catalog
      ;; is pulled then — off-thread, so this answer is still one cached verdict.
      (let [asked (atom [])]
        (with-redefs-fn {#'providers/refresh-models-async! (fn [provider-id source]
                                                             (swap! asked conj [provider-id source])
                                                             nil)
                         #'providers/configured-providers (constantly [{:id :opencode-go}])
                         #'providers/provider-status (constantly {:is-authenticated true})}
          (fn []
            (let [resp (#'providers-api/provider-status-handler
                        {:path-params {:provider-id "opencode-go"}})]
              (expect (= 200 (:status resp)))
              (expect (true? (get-in (wire/parse-json (:body resp)) ["status" "is_authenticated"])))
              (expect (= [[:opencode-go :gateway]] @asked))))))))

(defdescribe
  delete-project-blast-radius-is-explicit-on-the-wire-test
  (it "delete project blast radius is explicit on the wire"
      ;; The default DELETE only ever scattered members back to project-less, and no
      ;; client could remove a project together with its sessions. Recursion is
      ;; OPT-IN and answers with the ids, because the caller has to prune local rows,
      ;; snapshots and unsent drafts without racing a re-read.
      (let [pid
            (java.util.UUID/randomUUID)

            sids
            [(str (java.util.UUID/randomUUID)) (str (java.util.UUID/randomUUID))]

            calls
            (atom [])

            stub
            (fn ([p] (swap! calls conj [p nil]) {:project_id (str p)
                                                 :deleted_session_ids []
                                                 :session_count 0}) ([p opts] (swap! calls conj
                                                                                [p opts])
                                                                     {:project_id (str p)
                                                                      :deleted_session_ids sids
                                                                      :session_count (count sids)}))

            handler
            #'projects-api/delete-project-handler]

        (with-redefs-fn {#'state/delete-project! stub}
          (fn []
            ;; a plain DELETE stays the body-less 204 scatter
            (let [response (handler {:path-params {:pid (str pid)} :query-params {}})]
              (expect (= 204 (:status response)))
              (expect (nil? (:body response)))
              (expect (= [[pid nil]] @calls)))
            (reset! calls [])
            ;; is_recursive=true is 200 and reports every deleted session id
            (let [response
                  (handler {:path-params {:pid (str pid)} :query-params {"is_recursive" "true"}})

                  body
                  (wire/parse-json (:body response))]

              (expect (= 200 (:status response)))
              (expect (= [[pid {:is-recursive true}]] @calls))
              (expect (= sids (get body "deleted_session_ids")))
              (expect (= 2 (get body "session_count")))
              (expect (= (str pid) (get body "project_id"))))
            (reset! calls [])
            ;; a malformed project id is a 404, never a silent empty recursion
            (let [response (handler {:path-params {:pid "not-a-uuid"}
                                     :query-params {"is_recursive" "true"}})]
              (expect (= 404 (:status response)))
              (expect (empty? @calls))))))))

(defdescribe
  project-patch-moves-a-project-to-another-folder
  (it
    "project patch moves a project to another folder"
    (let [pid
          (java.util.UUID/randomUUID)

          calls
          (atom [])

          handler
          #'projects-api/patch-project-handler

          patch!
          (fn [body]
            (handler {:path-params {:pid (str pid)}
                      :body (java.io.ByteArrayInputStream. (.getBytes (wire/json-str body)
                                                                      "UTF-8"))}))]

      (with-redefs-fn {#'state/update-project!
                       (fn [p opts]
                         (swap! calls conj [p opts])
                         (case (:workspace-root opts)
                           "/missing"
                           (throw (ex-info "Not a directory: /missing"
                                           {:type :project/not-a-directory}))

                           "/taken"
                           (throw (ex-info "Another project already uses /taken"
                                           {:type :project/root-taken}))

                           {"id" (str pid) "name" "vis" "workspace_root" (:workspace-root opts)}))}
        (fn []
          (let [response (patch! {"workspace_root" "/new/vis"})]
            (expect (= 200 (:status response)))
            (expect (= "/new/vis" (get (wire/parse-json (:body response)) "workspace_root")))
            (expect (= [[pid {:workspace-root "/new/vis"}]] @calls)))
          (expect (= 400 (:status (patch! {"workspace_root" " "}))))
          (expect (= 400 (:status (patch! {"workspace_root" "/missing"}))))
          (expect (= 409 (:status (patch! {"workspace_root" "/taken"})))))))))

(defdescribe
  session-group-routes-divide-a-project-without-touching-its-sessions
  (it
    "session group routes divide a project without touching its sessions"
    ;; BLO-167: a group is the human's own division INSIDE one project. The routes
    ;; accept a workspace `root` as well as a project id, because the companion
    ;; groups its list by root and never holds a project id of its own.
    (let [pid
          (java.util.UUID/randomUUID)

          gid
          (java.util.UUID/randomUUID)

          sid
          (java.util.UUID/randomUUID)

          group
          {"id" (str gid)
           "project_id" (str pid)
           "name" "Release apps"
           "color" "amber"
           "session_count" 3}

          json-body
          (fn [m]
            {:body (java.io.ByteArrayInputStream. (.getBytes (wire/json-str m) "UTF-8"))})

          created
          (atom [])

          assigned
          (atom [])

          destroyed
          (atom [])

          minted
          (atom [])

          group-views
          (atom [])]

      (with-redefs-fn {#'state/get-project-by-root (fn [_owner root]
                                                     (when (= "/repo" root) {"id" (str pid)}))
                       #'state/ensure-project-for-root! (fn [_owner _root _name]
                                                          {"id" (str pid)})
                       #'state/list-session-groups (fn [p opts]
                                                     (swap! group-views conj (:archived opts))
                                                     (if (= pid p) [group] []))
                       #'state/get-session-group (fn [g]
                                                   (when (= gid g) group))
                       #'state/create-session-group! (fn [p opts]
                                                       (swap! created conj [p opts])
                                                       (if (= "Gateway" (:name opts))
                                                         (throw (ex-info "duplicate name" {}))
                                                         group))
                       #'state/delete-session-group! (fn [g]
                                                       {:group_id (str g)
                                                        :scattered_session_ids [(str sid)]
                                                        :deleted_session_ids []
                                                        :session_count 1})
                       #'state/delete-session-group-with-sessions! (fn [g]
                                                                     (swap! destroyed conj g)
                                                                     {:group_id (str g)
                                                                      :scattered_session_ids []
                                                                      :deleted_session_ids [(str
                                                                                              sid)]
                                                                      :session_count 1})
                       #'state/soul (fn [s]
                                      {"id" (str s)})
                       #'state/assign-session-group! (fn [s g]
                                                       (swap! assigned conj [s g])
                                                       {"id" (str s) "group_id" (when g (str g))})
                       #'state/create-session! (fn [opts]
                                                 (swap! minted conj opts)
                                                 {"id" (str sid)
                                                  "group_id" (some-> (:group-id opts)
                                                                     str)})}
        (fn []
          (let [list-groups
                #'projects-api/list-session-groups-handler

                create-group
                #'projects-api/create-session-group-handler

                delete-group
                #'projects-api/delete-session-group-handler

                assign-group
                #'projects-api/set-session-group-handler

                create-session
                #'sessions-api/create-session-handler]

            ;; groups belong to ONE project: without a project or a root there is nothing to list
            (expect (= 400 (:status (list-groups {:query-params {}}))))
            ;; a root with no project yet has no groups - a READ never creates a project
            (let [body (wire/parse-json (:body (list-groups {:query-params {"root"
                                                                            "/elsewhere"}})))]
              (expect (nil? (get body "project_id")))
              (expect (= [] (get body "groups"))))
            ;; a known root answers with that project's groups
            (let [body (wire/parse-json (:body (list-groups {:query-params {"root" "/repo"}})))]
              (expect (= (str pid) (get body "project_id")))
              (expect (= ["Release apps"] (mapv #(get % "name") (get body "groups")))))
            ;; A human keeps adding bands, so this list is a WINDOW with a total of its
            ;; own - what a pager over the groups prints.
            ;; a limit cuts one page of bands and still counts the whole wall
            (let [body (wire/parse-json (:body (list-groups {:query-params {"root" "/repo"
                                                                            "limit" "1"}})))]
              (expect (= ["Release apps"] (mapv #(get % "name") (get body "groups"))))
              (expect (= 1 (get body "total")))
              ;; The page cut the bands; the count of what is FILED still follows the
              ;; whole wall, so a header over it does not move as the reader walks.
              (expect (= 3 (get body "session_total")))
              (expect (= 0 (get body "offset")))
              (expect (false? (get body "has_more"))))
            ;; an offset past the wall is an empty page, never the whole wall again
            (let [body (wire/parse-json (:body (list-groups {:query-params {"root" "/repo"
                                                                            "limit" "1"
                                                                            "offset" "1"}})))]
              (expect (= [] (get body "groups")))
              (expect (= 1 (get body "total")))
              (expect (= 1 (get body "offset"))))
            ;; a read that names no window still answers every band - what the TUI asks for
            (let [body (wire/parse-json (:body (list-groups {:query-params {"root" "/repo"}})))]
              (expect (= ["Release apps"] (mapv #(get % "name") (get body "groups"))))
              (expect (nil? (get body "limit")))
              (expect (= 1 (get body "total"))))
            ;; THE WALL IS POLLED. No turn moves a band and the gateway announces no frame
            ;; for one, so every client painting a project re-reads this wall on a timer to
            ;; catch what another client did to it. Those reads are answered with a
            ;; validator, so the ones that find nothing changed cost no body.
            (let [answer
                  (list-groups {:query-params {"root" "/repo" "limit" "1"}})

                  etag
                  (get-in answer [:headers "ETag"])]
              (expect (= 200 (:status answer)))
              ;; WEAK, like the session list's: this hashes the content a client paints
              (expect (str/starts-with? (str etag) "W/\""))
              (expect (= "no-cache" (get-in answer [:headers "Cache-Control"])))
              ;; the same wall, revalidated: no body at all, and the validator again
              (let [again (list-groups {:query-params {"root" "/repo" "limit" "1"}
                                        :headers {"if-none-match" etag}})]
                (expect (= 304 (:status again)))
                (expect (nil? (:body again)))
                (expect (= etag (get-in again [:headers "ETag"]))))
              ;; ANOTHER WINDOW IS ANOTHER ANSWER, never this one's 304: the page a
              ;; reader walked to is served even while the first page is unchanged
              (let [deeper (list-groups {:query-params {"root" "/repo" "limit" "1" "offset" "1"}
                                         :headers {"if-none-match" etag}})]
                (expect (= 200 (:status deeper)))
                (expect (= [] (get (wire/parse-json (:body deeper)) "groups")))
                (expect (not= etag (get-in deeper [:headers "ETag"]))))
              ;; a stale validator is a MISS, not an error: the wall is answered whole
              (let [moved (list-groups {:query-params {"root" "/repo" "limit" "1"}
                                        :headers {"if-none-match" "W/\"a-wall-ago\""}})]
                (expect (= 200 (:status moved)))
                (expect (= ["Release apps"]
                           (mapv #(get % "name") (get (wire/parse-json (:body moved)) "groups"))))))
            ;; A group is put away like a session: this list answers the ACTIVE shelves
            ;; unless the reader names another view, and a reveal asks for the archive alone.
            ;; the archive view a reader names is the view the store is asked for
            (reset! group-views [])
            (list-groups {:query-params {"root" "/repo"}})
            (list-groups {:query-params {"root" "/repo" "archived" "only"}})
            (list-groups {:query-params {"root" "/repo" "archived" "include"}})
            (expect (= [:exclude :only :include] @group-views))
            ;; a view this gateway does not have is a 400, never a silent full list
            (reset! group-views [])
            (let [response (list-groups {:query-params {"root" "/repo" "archived" "sometimes"}})]
              (expect (= 400 (:status response)))
              (expect (= "invalid-archived"
                         (get-in (wire/parse-json (:body response)) ["error" "type"])))
              (expect (= [] @group-views)))
            ;; creating by root get-or-creates the project first
            (let [response (create-group (json-body
                                           {:name "Release apps" :color "amber" :root "/repo"}))]
              (expect (= 201 (:status response)))
              (expect (= [[pid {:name "Release apps" :color "amber"}]] @created)))
            ;; a second group with the same name is a 409 conflict, not a 500
            (expect (= 409 (:status (create-group (json-body {:name "Gateway" :root "/repo"})))))
            ;; deleting a group DETACHES its sessions by default - they are never deleted
            (let [body (wire/parse-json (:body (delete-group {:path-params {:gid (str gid)}})))]
              (expect (= [(str sid)] (get body "scattered_session_ids")))
              (expect (= [] (get body "deleted_session_ids")))
              (expect (= 1 (get body "session_count")))
              (expect (= [] @destroyed)))
            ;; `sessions=delete` is the other answer the dialog offers: the members go too
            (let [body (wire/parse-json (:body (delete-group {:path-params {:gid (str gid)}
                                                              :query-params {"sessions"
                                                                             "delete"}})))]
              (expect (= [(str sid)] (get body "deleted_session_ids")))
              (expect (= [] (get body "scattered_session_ids")))
              (expect (= [gid] @destroyed)))
            ;; any other answer is a 400, so a typo can never delete a session
            (expect (= 400
                       (:status (delete-group {:path-params {:gid (str gid)}
                                               :query-params {"sessions" "purge"}}))))
            ;; an unknown group is a 404
            (expect (= 404
                       (:status (delete-group {:path-params {:gid
                                                             (str (java.util.UUID/randomUUID))}}))))
            ;; a null group_id leaves the session ungrouped inside its project
            (let [response (assign-group (assoc (json-body {:group_id nil})
                                           :path-params {:sid (str sid)}))]
              (expect (= 200 (:status response)))
              (expect (= [[sid nil]] @assigned)))
            ;; BLO-167: a session STARTED on a group must be filed as the gateway mints
            ;; it. Creating it loose and moving it afterwards showed the new row outside
            ;; the band the human started it in.
            ;; a session started inside a group is minted already filed under it
            (let [response (create-session (json-body
                                             {:channel "tui" :root "/repo" :group_id (str gid)}))]
              (expect (= 201 (:status response)))
              (expect (= (str gid) (get (wire/parse-json (:body response)) "group_id")))
              (expect (= [gid] (mapv :group-id @minted))))
            ;; an unknown group refuses the create instead of minting a loose session
            (expect (= 404
                       (:status (create-session (json-body {:root "/repo"
                                                            :group_id
                                                            (str (java.util.UUID/randomUUID))})))))
            (expect (= 1 (count @minted)))
            ;; a group_id that is no id at all is a 400
            (expect (= 400
                       (:status (create-session (json-body {:root "/repo" :group_id "the one"})))))
            (expect (= 1 (count @minted)))))))))

;; Regression: the live `iteration.completed` descriptors DROP model-only
;; artifacts and then RE-NUMBER what survives, while the byte endpoint indexed
;; the UNFILTERED row list. On any iteration whose first artifact was
;; `audience="model"` every index was off by one — the companion's artifacts
;; sheet fetched the wrong bytes for every tile, and the artifact deliberately
;; hidden from the human was handed to it at index 0.
(defdescribe
  attachment-byte-endpoint-indexes-the-list-the-descriptors-number
  (it
    "attachment byte endpoint indexes the list the descriptors number"
    (let [b64
          #(.encodeToString (java.util.Base64/getEncoder) (.getBytes ^String % "UTF-8"))

          iid
          "00000000-0000-0000-0000-0000000000ab"

          rows
          [{:kind "image"
            :media-type "image/png"
            :filename "for-the-model.png"
            :audience "model"
            :size 10
            :base64 (b64 "MODEL-ONLY")}
           {:kind "image"
            :media-type "image/png"
            :filename "for-the-human.png"
            :audience "both"
            :size 5
            :base64 (b64 "SHOWN")}]

          fetch
          (fn [idx]
            (#'turns-api/attachment-bytes-handler
             {:path-params {:sid (str (random-uuid)) :iid iid :idx (str idx)}}))]

      (with-redefs-fn {#'state/iteration-attachments (constantly rows)}
        (fn []
          (let [descriptors ((ns-resolve 'com.blockether.vis.internal.gateway.state
                                         'live-attachment-descriptors)
                              iid)]
            ;; the human is offered exactly the artifacts meant for them
            (expect (= [{:index 0 :filename "for-the-human.png"}]
                       (mapv #(select-keys % [:index :filename]) descriptors)))
            ;; descriptor index 0 serves THAT artifact's bytes
            (let [response (fetch 0)]
              (expect (= 200 (:status response)))
              (expect (= "SHOWN" (slurp (:body response)))))
            ;; nothing past the last descriptor resolves
            (expect (= 404 (:status (fetch 1))))
            ;; a model-only artifact is never served, at any index
            (expect (= []
                       (vec (for [idx (range 4)
                                  :let [response (fetch idx)]
                                  :when (and (= 200 (:status response))
                                             (= "MODEL-ONLY" (slurp (:body response))))]

                              idx))))))))))

;; Regression: the companion's Save on an annotated artifact answered "Could not
;; save this revision" — the URL it posts to was served by no route at all, so a
;; human's comments and drawings could never become the next version.
(defdescribe the-companions-save-url-routes-to-the-attachment-append-handler
             (it "the companions save url routes to the attachment append handler"
                 (let [match-by-path
                       (requiring-resolve 'reitit.core/match-by-path)

                       router
                       ((rv 'router) "token" [])

                       sid
                       (str (random-uuid))

                       iid
                       (str (random-uuid))

                       path
                       (str "/v1/sessions/" sid "/iterations/" iid "/attachments")]

                   ;; POST of a revision reaches the append handler
                   (expect (= @#'turns-api/append-attachment-handler
                              (get-in (match-by-path router path) [:data :post :handler])))
                   ;; and the bytes of one version are still read back beside it
                   (expect (= @#'turns-api/attachment-bytes-handler
                              (get-in (match-by-path router (str path "/0"))
                                      [:data :get :handler]))))))

;; Regression, same report: saving an annotated note must file it under its OWN
;; filename, which is what makes the revision the NEXT VERSION of that artifact
;; rather than a second file beside it.
(defdescribe
  saving-a-revision-stores-it-under-the-artifacts-own-filename
  (it "saving a revision stores it under the artifacts own filename"
      (let [seen
            (atom nil)

            iid
            (str (random-uuid))

            body
            {"filename" "PLAN.md"
             "media_type" "text/markdown"
             "commentable" true
             "kind" "diff"
             "base64" (.encodeToString (java.util.Base64/getEncoder)
                                       (.getBytes "# Release plan\n" "UTF-8"))}]

        (with-redefs-fn {#'gw-http/body-json (constantly body)
                         #'gw-http/path-sid (constantly "session-owner")
                         #'state/revise-iteration-attachment!
                         (fn [sid iteration-id attachment]
                           (expect (= "session-owner" sid))
                           (reset! seen [iteration-id attachment])
                           {:index 1 :filename "PLAN.md" :version 2})}
          (fn []
            (let [response (#'turns-api/append-attachment-handler {:path-params {:iid iid}})]
              (expect (= 201 (:status response)))
              (expect (= 2 (get (wire/parse-json (:body response)) "version")))
              (let [[iteration-id attachment] @seen]
                (expect (= iid iteration-id))
                (expect (= "PLAN.md" (:filename attachment)))
                (expect (= "text/markdown" (:media-type attachment)))
                (expect (not (contains? attachment :commentable))))))))))

(defdescribe human-attachment-revision-refusals-have-client-statuses
             (it "human attachment revision refusals have client statuses"
                 (doseq [[reason status] [[:attachment/not-found 404] [:attachment/read-only 403]
                                          [:attachment/invalid-revision 400]]]
                   (with-redefs-fn {#'gw-http/body-json (constantly {"filename" "note.md"
                                                                     "media_type" "text/markdown"
                                                                     "base64" "bm90ZQ=="})
                                    #'gw-http/path-sid (constantly "session-owner")
                                    #'state/revise-iteration-attachment!
                                    (fn [& _]
                                      (throw (ex-info "Revision refused" {:type reason})))}
                     (fn []
                       (expect (= status
                                  (:status (#'turns-api/append-attachment-handler
                                            {:path-params {:iid "iteration"}})))))))))

;; The session-creation UX picks a workspace root by RECOGNITION, so the gateway
;; has to be able to show the machine's own folders. `/v1/fs` is that surface and
;; nothing more: directories only, the two facts a chooser reads (how much is in
;; it, which branch it has out), and `~` meaning the GATEWAY user's home.
(defn- fs-child
  ^java.io.File [^java.io.File dir & segments]
  (reduce (fn [^java.io.File f seg]
            (java.io.File. f (str seg)))
          dir
          segments))

(defn- fs-temp-root
  ^java.io.File []
  (let [dir (.toFile (java.nio.file.Files/createTempDirectory
                       "vis-fs-test"
                       (make-array java.nio.file.attribute.FileAttribute 0)))]
    (.deleteOnExit dir)
    dir))

(defdescribe
  browse-fs-shows-only-folders-and-names-the-repos
  (it
    "browse fs shows only folders and names the repos"
    (let [root
          (fs-temp-root)

          _
          (run! #(.mkdir (fs-child root %)) ["beta" "alpha" ".hidden"])

          _
          (spit (fs-child root "readme.txt") "a file is not a folder")

          _
          (.mkdir (fs-child root "alpha" ".git"))

          _
          (spit (fs-child root "alpha" ".git" "HEAD") "ref: refs/heads/main\n")

          listing
          (wire/parse-json (:body (#'fs-api/browse-fs-handler
                                   {:query-params {"path" (.getAbsolutePath root)}})))

          entries
          (get listing "entries")]

      ;; a listing is the directories inside the path, alphabetical, dotfolders and files dropped
      (expect (= ["alpha" "beta"] (mapv #(get % "name") entries)))
      (expect (= (.getAbsolutePath root) (get listing "path")))
      (expect (= (.getAbsolutePath (.getParentFile root)) (get listing "parent")))
      (expect (= (System/getProperty "user.home") (get listing "home")))
      (expect (false? (get listing "is_truncated")))
      ;; a worktree carries its branch, a plain folder carries none
      (let [alpha
            (first entries)

            beta
            (second entries)]

        (expect (true? (get alpha "is_repo")))
        (expect (= "main" (get alpha "branch")))
        (expect (= 1 (get alpha "entry_count")))
        (expect (false? (get beta "is_repo")))
        (expect (nil? (get beta "branch"))))
      ;; `~` and a blank path both mean the gateway user's home, never the phone's
      (expect (= (System/getProperty "user.home")
                 (get (wire/parse-json (:body (#'fs-api/browse-fs-handler
                                               {:query-params {"path" "~"}})))
                      "path")))
      (expect (= (System/getProperty "user.home")
                 (get (wire/parse-json (:body (#'fs-api/browse-fs-handler {:query-params {}})))
                      "path")))
      ;; a path that is not a directory is refused by name
      (let [response (#'fs-api/browse-fs-handler
                      {:query-params {"path" (.getAbsolutePath (fs-child root "readme.txt"))}})]
        (expect (= 404 (:status response)))
        (expect (= "not-a-directory"
                   (get-in (wire/parse-json (:body response)) ["error" "type"])))))))

(defdescribe
  mkdir-creates-one-folder-where-the-picker-is-standing
  (it
    "mkdir creates one folder where the picker is standing"
    (let [root
          (fs-temp-root)

          made
          (#'fs-api/create-directory-handler
           (json-body {:path (.getAbsolutePath root) :name "gamma"}))

          body
          (wire/parse-json (:body made))]

      ;; the new folder answers as a listing row, so the picker can select it immediately
      (expect (= 201 (:status made)))
      (expect (= "gamma" (get body "name")))
      (expect (= (.getAbsolutePath (fs-child root "gamma")) (get body "path")))
      (expect (false? (get body "is_repo")))
      (expect (.isDirectory (fs-child root "gamma")))
      ;; creating it twice is not an error: the folder asked for exists
      (expect (= 201
                 (:status (#'fs-api/create-directory-handler
                           (json-body {:path (.getAbsolutePath root) :name "gamma"})))))
      ;; a name is ONE segment — a picker that accepts `a/../b` writes outside what it showed
      (doseq [name ["a/b" ".." "." "  " "x\\y"]]
        (let [response (#'fs-api/create-directory-handler
                        (json-body {:path (.getAbsolutePath root) :name name}))]
          (expect (= 400 (:status response)) (str "refuses " (pr-str name)))
          (expect (= "invalid-request"
                     (get-in (wire/parse-json (:body response)) ["error" "type"])))))
      ;; a parent that does not exist is a 404, not a silent mkdir -p
      (let [response (#'fs-api/create-directory-handler
                      (json-body {:path (.getAbsolutePath (fs-child root "nowhere"))
                                  :name "delta"}))]
        (expect (= 404 (:status response)))
        (expect (= "not-a-directory"
                   (get-in (wire/parse-json (:body response)) ["error" "type"])))))))

;; Regression, user report ("the project should have paging, and it should be
;; supported by the backend"): a client could only page the WHOLE fleet, so a
;; project's pages were sliced out of a list it had already downloaded in full.
(defdescribe
  sessions-window-narrows-to-one-project
  (it
    "GET /v1/sessions?root= hands the project down to the windowing engine"
    (let [seen (atom nil)]
      (with-redefs [state/list-sessions-page
                    (fn [channel opts]
                      (reset! seen [channel opts])
                      {:sessions [] :total 0 :limit 20 :next-cursor nil :has-more false})]
        (let [response (#'sessions-api/list-sessions-handler
                        {:query-params {"limit" "20" "root" "/Users/dev/vis"}})
              body (wire/parse-json (:body response))]

          (expect (= 200 (:status response)))
          (expect (= :all (first @seen)))
          ;; Only the keys this window is ABOUT: a cut the gateway learns later must not
          ;; rewrite an assertion about `root`.
          (expect (= {:limit 20 :after nil :root "/Users/dev/vis" :dirty #{}}
                     (select-keys (second @seen) [:limit :after :root :dirty])))
          (expect (= "/Users/dev/vis" (get body "root")))))
      ;; and a listing with no `root` is still the whole fleet
      (with-redefs [state/list-sessions-page
                    (fn [channel opts]
                      (reset! seen [channel opts])
                      {:sessions [] :total 0 :limit nil :next-cursor nil :has-more false})]
        (#'sessions-api/list-sessions-handler {:query-params {}})
        (expect (nil? (:root (second @seen)))))
      ;; Regression, user report (paraphrased: "the No project header says one session and
      ;; the pager under it says 128 pages"): `?root=` is a cut of its own - the sessions no
      ;; project holds - and dropping a blank root to nil answered that shelf with the fleet.
      ;; and a root that is PRESENT and blank is the shelf of sessions no project holds
      (with-redefs [state/list-sessions-page
                    (fn [channel opts]
                      (reset! seen [channel opts])
                      {:sessions [] :total 0 :limit 13 :next-cursor nil :has-more false})]
        (let [body (wire/parse-json (:body (#'sessions-api/list-sessions-handler
                                            {:query-params
                                             {"limit" "13" "root" "" "grouped" "aside"}})))]
          (expect (= "" (:root (second @seen))))
          (expect (= 13 (:limit (second @seen))))
          (expect (= "" (get body "root")))))
      ;; and the ids this DEVICE holds unsent words in ride down with it
      (with-redefs [state/list-sessions-page
                    (fn [channel opts]
                      (reset! seen [channel opts])
                      {:sessions [] :total 0 :limit nil :next-cursor nil :has-more false})]
        (#'sessions-api/list-sessions-handler {:query-params {"dirty" "s-1, s-2 ,,"}})
        ;; Blanks and stray commas are not ids, and an absent overlay is a set,
        ;; never nil: the gateway must not have to ask whether a device answered.
        (expect (= #{"s-1" "s-2"} (:dirty (second @seen))))))))

;; Regression, user report (paraphrased: "session totals grow as the list loads and the
;; whole list flashes again after leaving a session"): rows and project totals were two
;; competing requests, so React painted whichever answer or page happened to land next.
(defdescribe
  sessions-head-carries-project-overview-in-the-same-answer
  (it
    "sessions head carries project overview in the same answer"
    (let [page
          {:sessions [{"id" "a" "server_time_ms" 1}]
           :awaiting []
           :total 1
           :limit 20
           :next-cursor nil
           :has-more false}

          overview
          {:projects [{:root "/workspace" :session_count 400}]
           :project-count 1
           :session-count 400
           :live-count 3
           :awaiting-count 0}

          answer
          (fn [value]
            (with-redefs [state/list-sessions-page
                          (fn [_ _]
                            page)

                          state/projects-overview
                          (constantly value)]

              (#'sessions-api/list-sessions-handler {:query-params {"limit" "20"}})))

          first-answer
          (answer overview)

          changed-answer
          (answer (assoc overview :live-count 4))]

      ;; the head returns the stable totals beside its rows
      (expect (= 400 (get-in (wire/parse-json (:body first-answer)) ["overview" "session_count"])))
      ;; the totals are covered by the head validator
      (expect (not= (get-in first-answer [:headers "ETag"])
                    (get-in changed-answer [:headers "ETag"])))
      ;; a tail window neither repeats nor recomputes the fleet-wide totals
      (with-redefs [state/list-sessions-page
                    (fn [_ _]
                      page)

                    state/projects-overview
                    (fn [& _]
                      (throw (ex-info "tail recomputed overview" {})))]

        (let [response (#'sessions-api/list-sessions-handler
                        {:query-params {"limit" "20" "after" "2:-4000:a"}})]
          (expect (= 200 (:status response)))
          (expect (nil? (get (wire/parse-json (:body response)) "overview"))))))))

;; Regression, user report (paraphrased: "the list keeps jumping while I read it"): the
;; navigator key used to LEAD with a band, so a run parked on a human was lifted into
;; the first window - moving every row under the reader and pushing another session out
;; of the page. The parked rows now ride BESIDE the window, which means the validator
;; has to cover them: hashing only `sessions` would hide a new demand behind a 304.
(defdescribe
  sessions-window-carries-the-parked-rows-beside-it
  (it "sessions window carries the parked rows beside it"
      (let [answer
            (fn [awaiting]
              (with-redefs [state/list-sessions-page (fn [_ _]
                                                       {:sessions [{"id" "a" "server_time_ms" 1}]
                                                        :awaiting awaiting
                                                        :total 1
                                                        :limit 20
                                                        :next-cursor nil
                                                        :has-more false})]
                (#'sessions-api/list-sessions-handler {:query-params {"limit" "20"}})))

            quiet
            (answer [])

            parked
            (answer [{"id" "p" "is_awaiting_input" true "server_time_ms" 2}])]

        ;; the parked sessions travel outside the window
        (expect (= ["p"] (mapv #(get % "id") (get (wire/parse-json (:body parked)) "awaiting"))))
        (expect (= ["a"] (mapv #(get % "id") (get (wire/parse-json (:body parked)) "sessions"))))
        ;; and a demand nobody has answered cannot hide behind a 304
        (expect (not= (get-in quiet [:headers "ETag"]) (get-in parked [:headers "ETag"]))))))

;; Regression, user report (paraphrased: "the list keeps jumping while I read it"): windows
;; were addressed by an OFFSET into an ordering recomputed per request, so a turn landing
;; mid-walk made one session arrive twice and another vanish. A window is now addressed by
;; the CURSOR of the last row a client holds.
(defdescribe
  sessions-window-is-addressed-by-a-cursor
  (it
    "sessions window is addressed by a cursor"
    (let [seen (atom nil)]
      (with-redefs [state/list-sessions-page (fn [channel opts]
                                               (reset! seen [channel opts])
                                               {:sessions [{"id" "b" "server_time_ms" 1}]
                                                :awaiting []
                                                :total 9
                                                :limit 20
                                                :next-cursor "2:-3000:b"
                                                :has-more true})]
        ;; `after` is handed down and the next cursor comes back
        (let [response (#'sessions-api/list-sessions-handler
                        {:query-params {"limit" "20" "after" "2:-4000:a"}})
              body (wire/parse-json (:body response))]

          (expect (= 200 (:status response)))
          (expect (= [:all
                      {:limit 20
                       :after "2:-4000:a"
                       :root nil
                       :project-id nil
                       :id-prefix nil
                       :group-id nil
                       :group-ids nil
                       :ids #{}
                       :dirty #{}
                       :grouped nil
                       :archived :exclude}]
                     @seen))
          (expect (= "2:-3000:b" (get body "next_cursor")))
          (expect (true? (get body "has_more")))
          ;; The offset is gone from the wire, not renamed.
          (expect (nil? (get body "offset")))
          ;; The ordering digest went with it: a cursor cannot be torn, so there is
          ;; nothing for a client to detect.
          (expect (nil? (get-in response [:headers "X-Vis-Sessions-Order"]))))
        ;; a cursor that is present but not a cursor is a 400, never the head of the list
        (let [response (#'sessions-api/list-sessions-handler
                        {:query-params {"limit" "20" "after" "nope"}})]
          (expect (= 400 (:status response)))
          (expect (= "invalid-window"
                     (get-in (wire/parse-json (:body response)) ["error" "type"]))))
        ;; the window a client asked for is part of its validator
        (let [etag (fn [after]
                     (get-in (#'sessions-api/list-sessions-handler
                              {:query-params (cond-> {"limit" "20"}
                                               after
                                               (assoc "after" after))})
                             [:headers "ETag"]))]
          (expect (not= (etag nil) (etag "2:-4000:a"))))))))

;; User report (paraphrased: "one search API for every surface"): the search route answered
;; bare ids, so each client fetched the rows of its hits with a second read and merged them.
(defdescribe
  sessions-search-answers-list-rows
  (it
    "sessions search answers list rows in a window"
    (let [seen (atom [])]
      (with-redefs [state/search-sessions (fn [channel opts]
                                            (swap! seen conj [channel opts])
                                            {:query (:query opts)
                                             :sessions [{"id" "b"
                                                         "match" {"rank" 0 "is_in_title" true}}]
                                             :total 3
                                             :limit 50
                                             :next-cursor "0:-3000:b"
                                             :has-more true})]
        (let [response (#'sessions-api/search-sessions-handler
                        {:query-params {"q" "pager" "after" "0:-4000:a" "dirty" "d1"}})
              body (wire/parse-json (:body response))]

          (expect (= 200 (:status response)))
          (expect
            (= [:all
                {:query "pager" :limit 50 :after "0:-4000:a" :archived :exclude :dirty #{"d1"}}]
               (first @seen)))
          (expect (= ["b"] (mapv #(get % "id") (get body "sessions"))))
          (expect (= 0 (get-in body ["sessions" 0 "match" "rank"])))
          (expect (= "0:-3000:b" (get body "next_cursor")))
          (expect (true? (get body "has_more")))
          ;; The id-only fields are gone from the wire, not kept beside the rows.
          (expect (nil? (get body "session_ids")))
          (expect (nil? (get body "matches"))))
        ;; No query is the RECENTS, windowed like any other answer.
        (#'sessions-api/search-sessions-handler {:query-params {}})
        (expect (= "" (:query (second (second @seen)))))
        (expect (= 50 (:limit (second (second @seen)))))
        ;; A cursor that is present but not a cursor is a 400, never the head of the list.
        (let [response (#'sessions-api/search-sessions-handler {:query-params {"after" "nope"}})]
          (expect (= 400 (:status response)))
          (expect (= "invalid-window"
                     (get-in (wire/parse-json (:body response)) ["error" "type"]))))))))

(defdescribe
  sessions-search-forwards-scope
  (it "forwards project and OR-group filters, including an explicit empty scope"
      (let [seen (atom [])]
        (with-redefs [state/search-sessions (fn [_ opts]
                                              (swap! seen conj opts)
                                              {:sessions []})]
          (#'sessions-api/search-sessions-handler
           {:query-params {"q" "needle" "project_id" "p1" "group_ids" "g1,g2" "root" "/work"}})
          (expect (= {:project-id "p1" :group-ids #{"g1" "g2"} :root "/work"}
                     (select-keys (first @seen) [:project-id :group-ids :root])))
          (#'sessions-api/search-sessions-handler {:query-params {"root" "" "group_ids" ""}})
          (expect (= {:root "" :group-ids #{}} (select-keys (second @seen) [:root :group-ids])))
          (#'sessions-api/search-sessions-handler {:query-params {}})
          (expect (= {} (select-keys (last @seen) [:root :project-id :group-ids])))))))

;; Regression, user report (paraphrased: "groups should be outside the paging"): a client
;; could only paint the group bands of the page it was holding, so a session filed deeper
;; in the fleet looked like it was in no group at all (BLO-167).
(defdescribe
  sessions-window-keeps-group-shelves-aside
  (it
    "sessions window keeps group shelves aside"
    (let [seen
          (atom nil)

          asked
          (atom nil)

          pid
          (java.util.UUID/randomUUID)]

      (with-redefs [state/list-sessions-page (fn [channel opts]
                                               (reset! seen [channel opts])
                                               {:sessions [{"id" "loose"}]
                                                :awaiting []
                                                :grouped [{"id" "filed"}]
                                                :total 1
                                                :limit 20
                                                :next-cursor nil
                                                :has-more false})]
        ;; `grouped=aside` reaches the store and its shelves come back on the wire
        (let [body (wire/parse-json (:body (#'sessions-api/list-sessions-handler
                                            {:query-params {"limit" "20" "grouped" "aside"}})))]
          (expect (= "aside" (:grouped (second @seen))))
          (expect (= ["filed"] (mapv #(get % "id") (get body "grouped"))))
          (expect (= ["loose"] (mapv #(get % "id") (get body "sessions")))))
        ;; a caller that does not ask for them gets the list it always got
        (let [body (wire/parse-json (:body (#'sessions-api/list-sessions-handler
                                            {:query-params {"limit" "20"}})))]
          (expect (nil? (:grouped (second @seen))))
          (expect (nil? (get body "grouped"))))
        ;; A project with a wall of bands would paint every shelf on the first read, so
        ;; a client names the PAGE of bands it is painting and the shelves beside its
        ;; window follow it.
        ;; a band window reaches the store as the groups whose shelves are painted
        (with-redefs [state/get-project-by-root (fn [_owner root]
                                                  (when (= "/repo" root) {"id" (str pid)}))
                      state/list-session-groups-page (fn [project opts]
                                                       (reset! asked [project opts])
                                                       {:groups [{"id" "g3"} {"id" "g4"}]
                                                        :total 9
                                                        :limit (:limit opts)
                                                        :offset (:offset opts)
                                                        :has-more true})]

          (#'sessions-api/list-sessions-handler
           {:query-params
            {"limit" "20" "grouped" "aside" "root" "/repo" "group_limit" "2" "group_offset" "4"}})
          (expect (= [pid {:archived :exclude :limit 2 :offset 4}] @asked))
          (expect (= ["g3" "g4"] (:group-ids (second @seen)))))
        ;; and a read that names no band window still gets every shelf
        (#'sessions-api/list-sessions-handler
         {:query-params {"limit" "20" "grouped" "aside" "root" "/repo"}})
        (expect (nil? (:group-ids (second @seen))))))))

;; Regression, this Vis session (paraphrased: "the TUI should use the limit on the session
;; list too"): the channel downloaded every session to answer two narrow questions - one
;; project's tab set, and which session a short id names.
(defdescribe
  sessions-window-cuts-to-a-project-or-to-a-short-id
  (it "sessions window cuts to a project or to a short id"
      (let [seen (atom nil)]
        (with-redefs [state/list-sessions-page (fn [channel opts]
                                                 (reset! seen [channel opts])
                                                 {:sessions []
                                                  :awaiting []
                                                  :total 0
                                                  :limit 2
                                                  :next-cursor nil
                                                  :has-more false})]
          ;; a project's tab set is a CUT of this ordering, not a client-side filter
          (#'sessions-api/list-sessions-handler {:query-params {"project_id" "p1"}})
          (expect (= "p1" (:project-id (second @seen))))
          ;; and so is the session a short id names
          (#'sessions-api/list-sessions-handler {:query-params {"id_prefix" "aa11" "limit" "2"}})
          (expect (= ["aa11" 2] [(:id-prefix (second @seen)) (:limit (second @seen))]))))))

;; Regression, this Vis session (paraphrased: "why does opening the picker download every
;; session"): a list read that named no window built and shipped the whole store - ~825KB
;; in ~450ms - and a picker holding one window had no way to paint a search hit outside it.
(defdescribe sessions-read-without-a-window-answers-the-head-not-the-fleet
             (it "sessions read without a window answers the head not the fleet"
                 (let [seen (atom nil)]
                   (with-redefs [state/list-sessions-page (fn [channel opts]
                                                            (reset! seen [channel opts])
                                                            {:sessions []
                                                             :awaiting []
                                                             :total 0
                                                             :limit 20
                                                             :next-cursor nil
                                                             :has-more false})]
                     ;; no window and no cut is the HEAD window, never every session in the store
                     (#'sessions-api/list-sessions-handler {:query-params {}})
                     (expect (= 20 (:limit (second @seen))))
                     ;; a read that named its own limit keeps it
                     (#'sessions-api/list-sessions-handler {:query-params {"limit" "5"}})
                     (expect (= 5 (:limit (second @seen))))
                     ;; the rows a set of ids names are a CUT, so the question bounds the answer
                     (#'sessions-api/list-sessions-handler {:query-params {"ids" "aa11,bb33"}})
                     (expect (= #{"aa11" "bb33"} (:ids (second @seen))))
                     (expect (nil? (:limit (second @seen))))
                     ;; and so is a project, which is bounded by the project
                     (#'sessions-api/list-sessions-handler {:query-params {"project_id" "p1"}})
                     (expect (nil? (:limit (second @seen))))))))

;; Regression, issue #146: `wrap-errors` answered EVERY throwable with a generic
;; `engine-error` 500, so a request that failed only because no AI provider was
;; configured reached the TUI as an ordinary crash — `vis-agent tui` printed a
;; 50-frame stack trace instead of opening the provider dialog.
(defdescribe no-provider-failure-keeps-its-type-on-the-wire
             (it "no provider failure keeps its type on the wire"
                 (let [answer (fn [t]
                                (let [handler ((rv 'wrap-errors)
                                                (fn [_]
                                                  (throw t)))
                                      {:keys [status body]} (handler {:uri "/v1/sessions"
                                                                      :request-method :post})
                                      error (get (wire/parse-json body) "error")]

                                  [status (get error "type") (get error "message")]))]
                   ;; nothing configured — the typed verdict AND the original reason survive
                   (expect (= [503 "no-provider" "No AI provider is configured yet."]
                              (answer (ex-info "session create failed"
                                               {}
                                               (ex-info "No AI provider is configured yet."
                                                        {:type :vis/no-provider})))))
                   ;; configured but nothing resolved is the same verdict to a client
                   (expect (= [503 "no-provider" "make-router requires at least one provider"]
                              (answer (ex-info "make-router requires at least one provider"
                                               {:type :svar/no-providers}))))
                   ;; any other failure is still an engine error
                   (expect (= [500 "engine-error" "boom"] (answer (ex-info "boom" {})))))))

(defn- cloning-speaker
  "A speaking engine that LEARNS a voice from the recording it is handed, as the local
   cloning model does: it reads the bytes off the path it was given, so a test that
   gets a voice back has proved the UPLOAD arrived and not merely its name."
  [store]
  (assoc (speaking-engine)
    :id :cloner
    :label "Cloner"
    :voices (fn []
              (vec (vals @store)))
    :import-voice (fn [{:keys [path voice-name language text]}]
                    (let [voice {:id (.replace (.toLowerCase (str voice-name)) " " "-")
                                 :label voice-name
                                 :language language
                                 :clip-text text
                                 :heard (slurp path)
                                 :is-imported true}]
                      (swap! store assoc (:id voice) voice)
                      voice))
    :forget-voice (fn [id]
                    (let [had? (contains? @store id)]
                      (swap! store dissoc id)
                      had?))))

(defdescribe
  a-voice-is-uploaded-listed-and-forgotten-over-one-route
  (it "a voice is uploaded listed and forgotten over one route"
      ;; A cloning engine speaks in whatever voice it is given, so "create a voice" is an
      ;; upload and nothing else. The route has to carry the RECORDING through to the
      ;; engine, report what it became in the same vocabulary a picker already reads, and
      ;; let it be taken back.
      ;;
      ;; No session id anywhere in here: a clip lives on the MACHINE, so these routes are
      ;; reachable from a settings screen that is not looking at any session.
      (let [store (atom {})]
        (with-only-speech-engine!
          (cloning-speaker store)
          (fn []
            (let [upload (fn [query clip]
                           (#'speech-api/speech-voices-handler
                            {:request-method :post
                             :query-params query
                             :body (java.io.ByteArrayInputStream. (.getBytes (str clip) "UTF-8"))}))
                  listed (fn []
                           (#'speech-api/speech-voices-handler {:request-method :get}))
                  forget (fn [id]
                           (#'speech-api/speech-voice-handler
                            {:request-method :delete :path-params {:voice-id id}}))]

              ;; the upload IS the voice: bytes in, catalogue entry out
              (let [response (upload {"name" "My Own" "lang" "en-GB" "text" "what the clip says"}
                                     "RIFFclip")
                    body (wire/parse-json (:body response))]

                (expect (= 201 (:status response)))
                (expect (= {"id" "my-own" "label" "My Own" "language" "en-GB" "is_imported" true}
                           (get body "voice")))
                ;; the engine was handed the RECORDING, not merely a name for it
                (expect (= "RIFFclip" (:heard (get @store "my-own"))))
                (expect (= "what the clip says" (:clip-text (get @store "my-own")))))
              ;; one request answers both what can be spoken and whether more may be added
              (let [body (wire/parse-json (:body (listed)))]
                (expect (true? (get-in body ["engine" "is_voice_import"])))
                (expect (= ["my-own"] (mapv #(get % "id") (get body "voices"))))
                (expect (true? (get-in body ["voices" 0 "is_imported"]))))
              ;; a voice can be taken back, and asking twice says it is gone
              (let [gone (forget "my-own")]
                (expect (= 200 (:status gone)))
                (expect (true? (get (wire/parse-json (:body gone)) "is_forgotten"))))
              (expect (empty? (get (wire/parse-json (:body (listed))) "voices")))
              (expect (= 404 (:status (forget "my-own")))))))
        ;; an engine that cannot clone refuses the upload by name, and it is not a 500
        (with-only-speech-engine!
          (speaking-engine)
          (fn []
            (let [response (#'speech-api/speech-voices-handler
                            {:request-method :post
                             :query-params {"name" "My Own"}
                             :body (java.io.ByteArrayInputStream. (.getBytes "RIFFclip" "UTF-8"))})]
              (expect (= 409 (:status response)))
              (expect (= "voice-import-unsupported"
                         (get-in (wire/parse-json (:body response)) ["error" "type"])))
              (expect (str/includes? (get-in (wire/parse-json (:body response)) ["error" "message"])
                                     "cannot learn a voice"))))))))

(defdescribe speech-tts-refusals-are-client-errors
             (it "speech tts refusals are client errors"
                 (let [response
                       (#'speech-api/voice-import-failure
                        (ex-info "That file is not a recording" {:type :speech-tts/clip-not-wav}))

                       body
                       (wire/parse-json (:body response))]

                   (expect (= 400 (:status response)))
                   (expect (= "invalid-voice-clip" (get-in body ["error" "type"])))
                   (expect (= "clip-not-wav" (get-in body ["error" "reason"]))))))

;; Every speech and voice refusal answers the gateway's one error envelope, so a client
;; branches on `error.type` and shows `error.message` instead of parsing a second shape.
(defdescribe
  speech-refusals-answer-the-canonical-error-envelope
  (it
    "speech refusals answer the canonical error envelope"
    (let [sid
          (str (random-uuid))

          refusal
          (fn [response]
            (let [error (get (wire/parse-json (:body response)) "error")]
              [(:status response) (get error "type") (string? (get error "message"))]))]

      (with-redefs-fn {#'state/soul (constantly {:session-id sid})}
        (fn []
          (speech/reset-jobs!)
          (with-only-engine! nil
                             (fn []
                               (expect (= [501 "engine-unavailable" true]
                                          (refusal (#'speech-api/voice-handler
                                                    {:path-params {:sid sid} :body (wav-body)}))))))
          (with-only-engine!
            {:id :fake-engine
             :transcribe (constantly "hi")
             :model-state (constantly {:state :ready})}
            (fn []
              (expect (= [400 "unknown-engine" true]
                         (refusal (#'speech-api/voice-handler
                                   {:path-params {:sid sid}
                                    :query-params {"engine" "whisper-server"}
                                    :body (wav-body)}))))
              (expect (= [400 "invalid-audio" true]
                         (refusal (#'speech-api/voice-handler
                                   {:path-params {:sid sid}
                                    :body (java.io.ByteArrayInputStream. (byte-array 64))}))))
              (doseq [handler [#'speech-api/voice-job-handler #'speech-api/voice-job-events-handler
                               #'speech-api/speech-job-audio-handler]]
                (expect (= [404 "job-not-found" true]
                           (refusal (handler {:request-method :get
                                              :path-params {:sid sid :job-id "vj_nope"}})))
                        (str handler)))))
          (with-only-speech-engine!
            (assoc (speaking-engine) :forget-voice (constantly false))
            (fn []
              (let [say (fn [text]
                          (#'speech-api/speech-handler
                           (merge {:request-method :post :path-params {:sid sid}}
                                  (json-body {:text text}))))]
                (expect (= [400 "invalid-request" true] (refusal (say "   "))))
                (expect (= [413 "text-too-long" true]
                           (refusal (say (apply str (repeat 21000 "x")))))))
              (expect (= [404 "voice-not-found" true]
                         (refusal (#'speech-api/speech-voice-handler
                                   {:request-method :delete :path-params {:voice-id "nobody"}}))))))
          (expect (= [500 "synthesis-failed" true]
                     (refusal (#'speech-api/speech-failure-response
                               (ex-info "engine crashed" {})))))
          (expect (= [500 "voice-import-failed" true]
                     (refusal (#'speech-api/voice-import-failure (ex-info "disk full" {})))))))
      (with-redefs [state/soul (constantly nil)]
        (expect (= [404 "session-not-found" true]
                   (refusal (#'speech-api/speech-job-handler
                             {:request-method :get
                              :path-params {:sid sid :job-id "sj_nope"}}))))))))

(defdescribe decisions-route-requires-an-explicit-model
             (it "decisions route requires an explicit model"
                 (let [app
                       (rr/ring-handler ((rv 'router) "token" []))

                       response
                       (app (merge {:request-method :post :uri "/v1/systemone"}
                                   (json-body {:state "hello" :questions {}})))]

                   (expect (= 400 (:status response)))
                   (expect (= "model-required"
                              (get-in (wire/parse-json (:body response)) ["error" "reason"]))))))

(defdescribe
  decision-route-reports-shutdown-as-temporarily-unavailable
  (it "decision route reports shutdown as temporarily unavailable"
      (with-redefs [decision-core/infer! (fn [_]
                                           (throw (ex-info "Decision model cache is stopping"
                                                           {:type :decisions/unavailable})))]
        (let [response ((rr/ring-handler ((rv 'router) "token" []))
                         (merge {:request-method :post :uri "/v1/systemone"}
                                (json-body
                                  {:model "laya-typed-decisions" :state "hello" :questions {}})))]
          (expect (= 503 (:status response)))
          (expect (= "unavailable"
                     (get-in (wire/parse-json (:body response)) ["error" "reason"])))))))

(defdescribe
  decision-http-end-to-end-uses-the-installed-fp32-model
  (it
    "decision http end to end uses the installed fp32 model"
    ;; Set -Dvis.test.laya.fp32.dir to a verified assets-pack inference install.
    (when-let [dir (System/getProperty "vis.test.laya.fp32.dir")]
      (with-server-state!
        {:require-token? true}
        (fn []
          (with-redefs [decision-assets/install-dir (fn [& _]
                                                      dir)]
            (let
              [server (jetty/run-jetty ((rv 'app) "decision-test-token" [])
                                       {:port 0 :host "127.0.0.1" :join? false})
               url (str "http://127.0.0.1:" (bound-port server) "/v1/systemone")
               body
               (str
                 "{\"model\":\"laya-typed-decisions\","
                 "\"state\":\"The customer requests a refund after receiving a broken item.\","
                 "\"questions\":{"
                 "\"intent\":{\"type\":\"choice\",\"instructions\":\"What is the customer asking for?\","
                 "\"criteria\":{\"refund\":\"A refund\",\"repair\":\"A repair\"}},"
                 "\"priority\":{\"type\":\"score\",\"instructions\":\"Rate urgency\","
                 "\"criteria\":[\"not urgent\",\"soon\",\"immediate\"]},"
                 "\"policy\":{\"type\":\"noul\",\"instructions\":\"Can the purchase be refunded?\","
                 "\"criteria\":{\"false\":\"not refundable\",\"true\":\"refundable\"}}}}")]

              (try (let [response (http/post url
                                             {:headers {"Authorization" "Bearer decision-test-token"
                                                        "X-Vis-Protocol"
                                                        (str gateway-contract/protocol-version)
                                                        "Content-Type" "application/json"}
                                              :body body
                                              :throw false
                                              :timeout 120000})
                         result (wire/parse-json (:body response))]

                     (expect (= 200 (:status response)))
                     (expect (= "refund" (get-in result ["answers" "intent" "choice"])))
                     (expect (= 1.5357 (get-in result ["answers" "priority" "score"])))
                     (expect (= 0.5466 (get-in result ["answers" "policy" "noul"])))
                     (expect (= 108 (get-in result ["usage" "input_tokens"])))
                     (let [catalog (http/get (str/replace url #"/systemone$" "/decisions/models")
                                             {:headers {"Authorization" "Bearer decision-test-token"
                                                        "X-Vis-Protocol"
                                                        (str gateway-contract/protocol-version)}})
                           listed (wire/parse-json (:body catalog))]

                       (expect (= 200 (:status catalog)))
                       (expect (= [true "ready"]
                                  ((juxt #(get % "installed") #(get % "residency"))
                                    (first (get listed "models")))))))
                   (finally (.stop ^org.eclipse.jetty.server.Server server))))))))))

(defdescribe
  gliner-http-end-to-end-runs-typed-and-action-heads
  (it
    "gliner http end to end runs typed and action heads"
    ;; Supply -Dvis.test.gliner.{base,decide}.fp32.dir=<complete local inference dir>.
    (doseq [name
            ["base" "decide"]

            :let [dir
                  (System/getProperty (str "vis.test.gliner." name ".fp32.dir"))]
            :when dir]

      (let [model-id
            (str "gliner2.5-" name)

            provenance
            (wire/parse-json (slurp (io/file dir "PROVENANCE.json")))

            model
            {:id model-id
             :revision (get provenance "revision")
             :artifacts {:inference {:sha256 (apply str (repeat 64 "a"))
                                     :requires (decision-assets/inference-required model-id)}}}

            request
            {"model" model-id
             "state" "A damaged item needs a refund."
             "questions" (array-map
                           "intent" {"type" "choice"
                                     "instructions" "Select intent"
                                     "criteria" (array-map "refund" "A refund" "repair" "A repair")}
                           "priority" {"type" "score"
                                       "instructions" "Rate urgency"
                                       "criteria" ["not urgent" "soon" "immediate"]}
                           "policy" {"type" "noul" "instructions" "Is refund available?"})}]

        (with-server-state!
          {:require-token? true}
          (fn []
            (with-redefs [decision-assets/manifest
                          (constantly [model])

                          decision-assets/install-dir
                          (fn [& _]
                            dir)

                          decision-assets/installed?
                          (fn [& _]
                            true)]

              (let [server
                    (jetty/run-jetty ((rv 'app) "decision-test-token" [])
                                     {:port 0 :host "127.0.0.1" :join? false})

                    url
                    (str "http://127.0.0.1:" (bound-port server) "/v1/systemone")]

                (try (let [response
                           (http/post url
                                      {:headers {"Authorization" "Bearer decision-test-token"
                                                 "X-Vis-Protocol"
                                                 (str gateway-contract/protocol-version)
                                                 "Content-Type" "application/json"}
                                       :body (wire/json-str request)
                                       :throw false
                                       :timeout 120000})

                           result
                           (wire/parse-json (:body response))]

                       (expect (= 200 (:status response)))
                       (expect (= model-id (get result "model")))
                       (expect (= model-id (get-in result ["routing" "model_ref"])))
                       (expect (contains? #{"refund" "repair"}
                                          (get-in result ["answers" "intent" "choice"])))
                       (expect (number? (get-in result ["answers" "priority" "score"])))
                       (expect (number? (get-in result ["answers" "policy" "noul"])))
                       (expect (number? (get-in result
                                                ["answers" "intent" "action" "act_probability"])))
                       (expect (pos? (get-in result ["usage" "input_tokens"]))))
                     (finally (.stop ^org.eclipse.jetty.server.Server server)
                              (decision-cache/release-idle!)))))))))))

(defdescribe
  decision-sdk-uploads-and-infers-against-a-token-gated-jetty
  (it
    "decision sdk uploads and infers against a token gated jetty"
    ;; Full gate: the unified decision environment and FP32 install run in a real client process.
    (when-let [python (System/getProperty "vis.test.decisions.sdk.python")]
      (let
        [dir (System/getProperty "vis.test.laya.fp32.dir")
         train-dir (System/getProperty "vis.test.laya.training.dir")
         root (io/file (System/getProperty "java.io.tmpdir")
                       (str "decision-sdk-http-" (random-uuid)))
         script
         (str/join
           "\n"
           ["import json, sys, tempfile" "from pathlib import Path"
            "from blockether.vis_decisions import Decisions"
            "from blockether.vis.engine import GatewayClient"
            "url, token, directory, checkpoint = sys.argv[1:]"
            "with tempfile.TemporaryDirectory(prefix=\"vis-sdk-decision-\") as temporary:"
            "    source = directory" "    if checkpoint != \"-\":"
            "        from blockether.vis_decisions import Trainer, TrainingBundle"
            "        root = Path(temporary)"
            "        row = {\"state\": \"Please refund my damaged purchase.\","
            "               \"question\": {\"type\": \"choice\", \"instructions\": \"Choose intent\","
            "                            \"criteria\": [\"refund\", \"repair\"]}, \"target\": 0, \"action\": 0}"
            "        (root / \"train.jsonl\").write_text(json.dumps(row) + \"\\n\")"
            "        evaluation = [{**row, \"state\": \"A different damaged item needs a refund.\"},"
            "            {\"state\": \"Repeated billing errors block the account.\","
            "             \"question\": {\"type\": \"score\", \"instructions\": \"Rate urgency\","
            "                          \"criteria\": [\"low\", \"medium\", \"high\"]}, \"target\": 2, \"action\": 1},"
            "            {\"state\": \"My delivery is still missing.\","
            "             \"question\": {\"type\": \"noul\", \"instructions\": \"Is it missing?\"},"
            "             \"target\": 1, \"action\": 0}]"
            "        (root / \"eval.jsonl\").write_text(\"\".join(json.dumps(r) + \"\\n\" for r in evaluation))"
            "        (root / \"config.json\").write_text(json.dumps({\"epochs\": 1, \"learning_rate\": 1e-5,"
            "                                                       \"train_encoder\": False, \"max_steps\": 1}))"
            "        (root / \"policy.json\").write_text(json.dumps({\"min_decision_accuracy\": 0.0,"
            "                                                       \"min_action_accuracy\": 0.0}))"
            "        with Trainer(TrainingBundle.open(checkpoint)) as trainer:"
            "            source = trainer.train(train_data=root / \"train.jsonl\", "
            "                eval_data=root / \"eval.jsonl\", training_config=root / \"config.json\","
            "                validation_policy=root / \"policy.json\", output_dir=root / \"result\")"
            "        assert source.checkpoint_dir.is_dir()"
            "    with GatewayClient(url, token=token, timeout=900) as gateway:"
            "        decisions = Decisions(gateway)"
            "        assert decisions.list_models()[0][\"installed\"]"
            "        published = decisions.upload_model(source, timeout=900)"
            "        ref = published[\"model_ref\"]"
            "        assert decisions.get_model(ref)[\"installed\"]"
            "        decisions.activate_model(\"sdk-e2e\", ref)"
            "        assert decisions.get_alias(\"sdk-e2e\")[\"model_ref\"] == ref"
            "        questions = {\"intent\": {\"type\": \"choice\", \"instructions\": \"Choose intent\","
            "                                \"criteria\": [\"refund\", \"repair\"]},"
            "                     \"priority\": {\"type\": \"score\", \"instructions\": \"Rate urgency\","
            "                                  \"criteria\": [\"low\", \"medium\", \"high\"]},"
            "                     \"policy\": {\"type\": \"noul\", \"instructions\": \"Is this refundable?\"}}"
            "        answer = decisions.infer(model=\"sdk-e2e\", state=\"broken item refund\","
            "                                 questions=questions)"
            "        baseline = decisions.infer(model=\"laya-typed-decisions\","
            "                                   state=\"broken item refund\", questions=questions)"
            "        print(\"VIS_DECISION_RESULT=\" + json.dumps({\"ref\": ref,"
            "            \"routing\": answer[\"routing\"][\"model_ref\"],"
            "            \"baseline\": baseline[\"routing\"][\"model\"],"
            "            \"trained\": checkpoint != \"-\","
            "            \"choice\": answer[\"answers\"][\"intent\"][\"choice\"],"
            "            \"score\": answer[\"answers\"][\"priority\"][\"score\"],"
            "            \"noul\": answer[\"answers\"][\"policy\"][\"noul\"],"
            "            \"action\": answer[\"answers\"][\"intent\"][\"action\"]}))"])]

        (expect (some? dir))
        (.mkdirs root)
        (try
          (with-server-state!
            {:require-token? true :managed? false :clients {}}
            (fn []
              (with-redefs [decision-assets/models-root (constantly (.getPath root))
                            decision-assets/install-dir (fn [& _]
                                                          dir)]

                (let [server (jetty/run-jetty ((rv 'app) "decision-sdk-test-token" [])
                                              {:port 0 :host "127.0.0.1" :join? false})]
                  (try
                    (let [url (str "http://127.0.0.1:" (bound-port server))
                          builder (ProcessBuilder. ^"[Ljava.lang.String;"
                                                   (into-array String
                                                               [python "-I" "-c" script url
                                                                "decision-sdk-test-token" dir
                                                                (or train-dir "-")]))
                          _ (.redirectErrorStream builder true)
                          _ (.put (.environment builder) "HF_HUB_OFFLINE" "1")
                          _ (.put (.environment builder) "TRANSFORMERS_OFFLINE" "1")
                          process (.start builder)
                          output (future (slurp (.getInputStream process)))
                          finished? (.waitFor process 1200 java.util.concurrent.TimeUnit/SECONDS)]

                      (when-not finished? (.destroyForcibly process))
                      (let [text (deref output 10000 "")]
                        (expect finished?)
                        (expect (and finished? (zero? (.exitValue process))) text)
                        (when (and finished? (zero? (.exitValue process)))
                          (let [line (some #(when (str/starts-with? % "VIS_DECISION_RESULT=")
                                              (subs % (count "VIS_DECISION_RESULT=")))
                                           (str/split-lines text))]
                            (expect (some? line) text)
                            (when line
                              (let [result (wire/parse-json line)]
                                (expect (= (get result "ref") (get result "routing")))
                                (expect (= "laya-typed-decisions" (get result "baseline")))
                                (expect (= (boolean train-dir) (get result "trained")))
                                (expect (#{"refund" "repair"} (get result "choice")))
                                (expect (number? (get result "score")))
                                (expect (number? (get result "noul")))
                                (expect (number? (get-in result ["action" "act_probability"])))
                                (expect (= (get result "ref")
                                           (get (decision-registry/get-alias "sdk-e2e")
                                                "model_ref")))))))))
                    (finally (.stop ^org.eclipse.jetty.server.Server server)))))))
          (finally (decision-cache/release-idle!) (speech-files/delete-dir! root)))))))

(defdescribe
  decision-import-route-streams-with-checksum-and-never-activates-an-alias
  (it
    "decision import route streams with checksum and never activates an alias"
    (let [app
          (rr/ring-handler ((rv 'router) "token" []))

          bytes
          (.getBytes "bounded archive" "UTF-8")

          sha
          (util/sha256-hex bytes)

          calls
          (atom [])]

      (with-redefs [decision-registry/register!
                    (fn [archive expected validate!]
                      (swap! calls conj [(slurp archive) expected (ifn? validate!)])
                      {"model_ref" (str "sha256-" expected) "installed" true})]
        (let [response (app {:request-method :post
                             :uri "/v1/decisions/models"
                             :headers {"x-content-sha256" sha
                                       "content-length" (str (alength bytes))}
                             :body (java.io.ByteArrayInputStream. bytes)})]
          (expect (= 201 (:status response)))
          (expect (= [["bounded archive" sha true]] @calls))
          (expect (= (str "sha256-" sha) (get (wire/parse-json (:body response)) "model_ref"))))
        (expect (= 400
                   (:status (app {:request-method :post
                                  :uri "/v1/decisions/models"
                                  :headers {"x-content-sha256" (apply str (repeat 64 "0"))}
                                  :body (java.io.ByteArrayInputStream. bytes)}))))
        (expect (= 413
                   (:status (app {:request-method :post
                                  :uri "/v1/decisions/models"
                                  :headers {"x-content-sha256" sha
                                            "content-length"
                                            (str (inc decision-assets/max-inference-upload-bytes))}
                                  :body (java.io.ByteArrayInputStream. bytes)}))))
        (expect (= 1 (count @calls)))))))

(defdescribe
  decision-import-enforces-streamed-byte-limit
  (it "enforces declared and actual sizes, cleans temporary files and releases the slot"
      ;; #294: lowering the shared cap exercises streamed overflow without large fixtures.
      (let [app
            (rr/ring-handler ((rv 'router) "token" []))

            root
            (fs-temp-root)

            calls
            (atom 0)]

        (try (with-redefs [decision-assets/models-root
                           (constantly (.getPath root))

                           decision-assets/max-inference-upload-bytes
                           8

                           decision-registry/register!
                           (fn [_ sha _]
                             (swap! calls inc)
                             {"model_ref" (str "sha256-" sha) "installed" true})]

               (doseq [[body declared status] [["12345678" "8" 201] ["123456789" "9" 413]
                                               ["123456789" nil 413] ["123456789" "8" 413]
                                               ["1234567" "8" 400] ["" "0" 400]
                                               ["12345678" nil 201]]]
                 (let [bytes (.getBytes ^String body "UTF-8")
                       response (app {:request-method :post
                                      :uri "/v1/decisions/models"
                                      :headers (cond-> {"x-content-sha256" (util/sha256-hex bytes)}
                                                 declared
                                                 (assoc "content-length" declared))
                                      :body (java.io.ByteArrayInputStream. bytes)})]

                   (expect (= status (:status response)))
                   (expect (empty? (.listFiles (io/file root "registered"))))))
               (expect (= 2 @calls)))
             (finally (.delete (io/file root "registered")) (.delete root))))))

(defdescribe
  decision-alias-route-refuses-an-implicit-replacement
  (it "decision alias route refuses an implicit replacement"
      (let [app
            (rr/ring-handler ((rv 'router) "token" []))

            calls
            (atom [])]

        (with-redefs [com.blockether.vis.internal.decisions.registry/activate!
                      (fn [name ref expected]
                        (swap! calls conj [name ref expected])
                        (when expected
                          (throw (ex-info "Alias changed" {:type :decisions/alias-conflict})))
                        {"alias" name "model_ref" ref})]
          (let [result (app (merge {:request-method :put :uri "/v1/decisions/aliases/sales"}
                                   (json-body {:model_ref "sha256-test"})))
                conflict (app (merge {:request-method :put :uri "/v1/decisions/aliases/sales"}
                                     (json-body {:model_ref "sha256-test"
                                                 :expected_current "sha256-old"})))]

            (expect (= 200 (:status result)))
            (expect (= 409 (:status conflict)))
            (expect (= [["sales" "sha256-test" nil] ["sales" "sha256-test" "sha256-old"]]
                       @calls)))))))

(defdescribe
  decision-training-routes-are-authenticated-bounded-and-cancellable
  (it
    "decision training routes are authenticated bounded and cancellable"
    (with-server-state!
      {:require-token? true}
      (fn []
        (let [id
              (str (random-uuid))

              calls
              (atom [])

              app
              ((rv 'app) "decision-training-token" [])

              request
              (fn [method path body authorized?]
                (let [req (merge {:request-method method :uri path} (when body (json-body body)))]
                  (app (update req
                               :headers
                               merge
                               {"x-vis-protocol" (str gateway-contract/protocol-version)}
                               (when authorized?
                                 {"authorization" "Bearer decision-training-token"})))))]

          (with-redefs [decision-jobs/create!
                        (fn [body]
                          (swap! calls conj [:create body])
                          {"job_id" id "status" "running"})

                        decision-jobs/get!
                        (fn [job]
                          (swap! calls conj [:get job])
                          (when (= job id) {"job_id" id "status" "running"}))

                        decision-jobs/delete!
                        (fn [job]
                          (swap! calls conj [:delete job])
                          {"job_id" id "status" "cancelling"})]

            (expect (= 401 (:status (request :post "/v1/decisions/training/jobs" {} false))))
            (expect
              (= 202
                 (:status
                   (request :post "/v1/decisions/training/jobs" {:train_data "train.jsonl"} true))))
            (expect (= 200
                       (:status (request :get (str "/v1/decisions/training/jobs/" id) nil true))))
            (expect
              (= 202 (:status (request :delete (str "/v1/decisions/training/jobs/" id) nil true))))
            (expect (= 404
                       (:status (request :get "/v1/decisions/training/jobs/not-found" nil true))))
            (expect (= 413
                       (:status (app {:request-method :post
                                      :uri "/v1/decisions/training/jobs"
                                      :headers {"authorization" "Bearer decision-training-token"
                                                "x-vis-protocol"
                                                (str gateway-contract/protocol-version)}
                                      :body (java.io.ByteArrayInputStream. (byte-array 8193))}))))
            (expect (= [:create {"train_data" "train.jsonl"}] (first @calls)))
            (expect (= [:delete id] (last (filter #(= :delete (first %)) @calls))))))))))

(defdescribe
  decision-warmup-respects-installation-switch-and-independent-errors
  (it
    "decision warmup respects installation switch and independent errors"
    (let [calls
          (atom [])

          models
          [{"model_ref" "laya-typed-decisions" "installed" true}
           {"model_ref" "other" "installed" true} {"model_ref" "missing" "installed" false}]

          env
          (fn [key]
            (case key
              "VIS_DECISION_WARMUP"
              "true"

              "VIS_DECISION_WARM_MODELS"
              "other,missing"))]

      (with-redefs [config/extension-env-value
                    env

                    decision-core/models-status
                    (constantly models)

                    decision-core/warm!
                    (fn [id]
                      (swap! calls conj id)
                      (when (= id "laya-typed-decisions") (throw (ex-info "warmup failed" {}))))]

        (expect (nil? (deref ((rv 'preload-decision-models!)) 3000 ::timeout)))
        (expect (= ["laya-typed-decisions" "other"] @calls)))
      (with-redefs [config/extension-env-value
                    (constantly "false")

                    decision-core/models-status
                    (fn []
                      (throw (ex-info "disabled" {})))]

        (expect (nil? ((rv 'preload-decision-models!))))))))

(defdescribe
  stopping-gateway-retires-decision-sessions-and-warmup
  (it "stopping gateway retires decision sessions and warmup"
      (decision-cache/enable!)
      (decision-cache/release-idle!)
      (let [closed
            (atom [])

            entered
            (promise)

            finish
            (promise)

            warm-entered
            (promise)

            warmup
            (future (deliver warm-entered true) @(promise))

            _
            (decision-cache/with-resident! :stop-idle
                                           0
                                           (fn []
                                             {:close #(swap! closed conj :idle)})
                                           identity)

            active
            (future (decision-cache/with-resident! :stop-active
                                                   0
                                                   (fn []
                                                     {:close #(swap! closed conj :active)})
                                                   (fn [_]
                                                     (deliver entered true)
                                                     @finish)))]

        (try (expect (= true (deref entered 3000 nil)))
             (expect (= true (deref warm-entered 3000 nil)))
             (with-redefs-fn {(rv 'running-turn-count) (constantly 0)
                              (rv 'stop-route-contributions!) (fn [_])
                              #'gw-view/uninstall! (fn [])
                              #'resources/shutdown! (fn [])
                              #'discovery/deregister-self! (fn [_])}
               #(with-server-state!
                  {:server (org.eclipse.jetty.server.Server.) :db nil :decision-warmup warmup}
                  server/stop!))
             (expect (future-cancelled? warmup))
             (expect (= [:idle] @closed))
             (deliver finish true)
             (expect (= true (deref active 3000 nil)))
             (expect (= [:idle :active] @closed))
             (expect (= :cold (decision-cache/status :stop-active)))
             (finally (deliver finish true)
                      (future-cancel warmup)
                      (decision-cache/enable!)
                      (decision-cache/release-idle!))))))

(defdescribe
  voices-hang-off-the-machine-not-off-a-session
  (it
    "voices hang off the machine not off a session"
    ;; An imported clip is stored on the machine and every session on it speaks with the
    ;; same catalogue, so the one screen that manages voices - settings, which is looking
    ;; at a machine and not at a session - can reach them without inventing a session id.
    (let [match-by-path
          (requiring-resolve 'reitit.core/match-by-path)

          router
          ((rv 'router) "token" [])]

      ;; the machine's own catalogue is listed, added to and pruned
      (expect (= @#'speech-api/speech-voices-handler
                 (get-in (match-by-path router "/v1/speech/voices") [:data :get :handler])))
      (expect (= @#'speech-api/speech-voices-handler
                 (get-in (match-by-path router "/v1/speech/voices") [:data :post :handler])))
      (expect (= @#'speech-api/speech-voice-handler
                 (get-in (match-by-path router "/v1/speech/voices/mine") [:data :delete :handler])))
      (expect (= @#'speech-api/speech-voice-sample-handler
                 (get-in (match-by-path router "/v1/speech/voices/mine/sample")
                         [:data :get :handler])))
      ;; hearing a voice is not a session's business either: a preview must never have
      ;; to invent a session id to reach POST /v1/sessions/:sid/speech
      (expect (nil? (match-by-path
                      router
                      (str "/v1/sessions/" (random-uuid) "/speech/voices/mine/sample"))))
      ;; and which model each direction uses is a fact about the machine too
      ;; A gateway with no session open still has to answer "is the model here yet" - the
      ;; settings screen asks before any conversation exists.
      (expect (= @#'speech-api/voice-model-handler
                 (get-in (match-by-path router "/v1/voice/model") [:data :get :handler])))
      (expect (= @#'speech-api/speech-model-handler
                 (get-in (match-by-path router "/v1/speech/model") [:data :get :handler])))
      ;; and nothing serves them under a session
      (expect (nil? (match-by-path router (str "/v1/sessions/" (random-uuid) "/speech/voices"))))
      (expect (nil? (match-by-path router (str "/v1/sessions/" (random-uuid) "/voice/model"))))
      (expect (nil? (match-by-path router (str "/v1/sessions/" (random-uuid) "/speech/model")))))))

(defdescribe speech-model-route-forwards-explicit-voice-consent
             (it "speech model route forwards explicit voice consent"
                 (let [seen
                       (atom nil)

                       engine
                       (assoc (speaking-engine)
                         :voice-model-state (fn [_]
                                              {:state :absent})
                         :start-voice-download (fn [opts]
                                                 (reset! seen opts)
                                                 {:state :downloading :progress 0}))]

                   (with-only-speech-engine!
                     engine
                     (fn []
                       (let [response
                             (#'speech-api/speech-model-handler
                              {:request-method :post
                               :query-params {"voice_id" "ryan" "is_license_accepted" "true"}})

                             body
                             (wire/parse-json (:body response))]

                         (expect (= 200 (:status response)))
                         (expect (= "downloading" (get body "status")))
                         (expect (= {:voice-id "ryan" :is-license-accepted true} @seen))))))))

(defdescribe
  voice-routes-report-an-unavailable-engine-consistently
  (it
    "voice routes report an unavailable engine consistently"
    (let [sid
          (str (random-uuid))

          refusals
          [[#'speech-api/speech-voices-handler {:request-method :get}]
           [#'speech-api/speech-voices-handler
            {:request-method :post
             :query-params {"name" "Mine"}
             :body (java.io.ByteArrayInputStream. (byte-array 0))}]
           [#'speech-api/speech-voice-handler
            {:request-method :delete :path-params {:voice-id "mine"}}]
           [#'speech-api/speech-model-handler {:request-method :get}]
           [#'speech-api/voice-model-handler {:request-method :get}]
           [#'speech-api/speech-handler
            (merge {:request-method :post :path-params {:sid sid}} (json-body {:text "hello"}))]]]

      (with-redefs-fn {#'state/soul (constantly {:session-id sid})}
        (fn []
          (with-only-direction-engine!
            :synthesize
            nil
            (fn []
              (with-only-direction-engine!
                :transcribe
                nil
                (fn []
                  (let [features (-> (#'views-api/capabilities-handler {})
                                     :body
                                     wire/parse-json
                                     (get "features"))]
                    (expect (true? (get-in features ["chat" "enabled"])))
                    (expect (false? (get-in features ["voice" "enabled"])))
                    (expect (false? (get-in features ["speech" "is_enabled"]))))
                  (doseq [[handler request] refusals]
                    (let [response (handler request)]
                      (expect (= 501 (:status response)) (str handler))
                      (expect (str/includes? (get-in (wire/parse-json (:body response))
                                                     ["error" "message"])
                                             "engine is available")
                              (str handler)))))))))))))

;; Regression, user report: the star was kept in each DEVICE's own storage, so one
  ;; screen showed a session starred while another showed it plain, and no answer from
  ;; the gateway could settle which was true.
(defdescribe
  the-star-is-set-on-the-gateway-never-on-the-device
  (it
    "the star is set on the gateway never on the device"
    (let [sid
          (str (random-uuid))

          asked
          (atom [])

          favorite-setter
          (fn [rank]
            (fn [_sid favorite?]
              (swap! asked conj favorite?)
              {"id" sid "favorite_rank" (when favorite? rank)}))

          patch-session
          (fn [body]
            (#'sessions-api/patch-session-handler
             (merge {:request-method :patch :path-params {:sid sid}} (json-body body))))]

      ;; starring answers the soul carrying the rank the gateway allocated
      (with-redefs-fn {#'state/set-favorite! (favorite-setter 7)}
        (fn []
          (let [response (patch-session {:is_favorite true})]
            (expect (= 200 (:status response)))
            (expect (= 7 (get (wire/parse-json (:body response)) "favorite_rank")))
            (expect (= [true] @asked)))))
      ;; unstarring is the same route, and the rank comes back empty
      (reset! asked [])
      (with-redefs-fn {#'state/set-favorite! (favorite-setter 7)}
        (fn []
          (let [response (patch-session {:is_favorite false})]
            (expect (= 200 (:status response)))
            (expect (nil? (get (wire/parse-json (:body response)) "favorite_rank")))
            (expect (= [false] @asked)))))
      ;; the star is read before a title, so a body carrying both never renames
      (reset! asked [])
      (with-redefs-fn {#'state/set-favorite! (favorite-setter 1)
                       #'state/set-title! (fn [& _]
                                            (throw (ex-info "renamed instead of starred" {})))}
        (fn []
          (expect (= 200 (:status (patch-session {:is_favorite true :title "new name"}))))
          (expect (= [true] @asked))))
      ;; a session this gateway does not know is a 404, never a silent star
      (with-redefs-fn {#'state/set-favorite! (constantly nil)}
        (fn []
          (expect (= 404 (:status (patch-session {:is_favorite true})))))))))

;; The archive is set on the GATEWAY for the same reason the star above is: a session put
  ;; away on the phone has to be put away in the TUI and in the web list too, and no device
  ;; can hold a private copy of that decision.
(defdescribe
  the-archive-is-set-on-the-gateway-never-on-the-device
  (it
    "the archive is set on the gateway never on the device"
    (let [sid
          (str (random-uuid))

          asked
          (atom [])

          archive-setter
          (fn [stamp]
            (fn [_sid archived?]
              (swap! asked conj archived?)
              {"id" sid "archived_at" (when archived? stamp)}))

          patch-session
          (fn [body]
            (#'sessions-api/patch-session-handler
             (merge {:request-method :patch :path-params {:sid sid}} (json-body body))))]

      ;; archiving answers the soul carrying the stamp the gateway wrote
      (with-redefs-fn {#'state/set-archived! (archive-setter 1717)}
        (fn []
          (let [response (patch-session {:archived true})]
            (expect (= 200 (:status response)))
            (expect (= 1717 (get (wire/parse-json (:body response)) "archived_at")))
            (expect (= [true] @asked)))))
      ;; unarchiving is the same route, and the stamp comes back empty
      (reset! asked [])
      (with-redefs-fn {#'state/set-archived! (archive-setter 1717)}
        (fn []
          (let [response (patch-session {:archived false})]
            (expect (= 200 (:status response)))
            (expect (nil? (get (wire/parse-json (:body response)) "archived_at")))
            (expect (= [false] @asked)))))
      ;; the archive is read before a title, so a body carrying both never renames
      (reset! asked [])
      (with-redefs-fn {#'state/set-archived! (archive-setter 1717)
                       #'state/set-title! (fn [& _]
                                            (throw (ex-info "renamed instead of archived" {})))}
        (fn []
          (expect (= 200 (:status (patch-session {:archived true :title "new name"}))))
          (expect (= [true] @asked))))
      ;; a session this gateway does not know is a 404, never a silent archive
      (with-redefs-fn {#'state/set-archived! (constantly nil)}
        (fn []
          (expect (= 404 (:status (patch-session {:archived true})))))))))

;; The ONE archive vocabulary every list route of this gateway reads: the view arrives by
  ;; NAME, because a boolean could not say whether the reader wants the active rows, both,
  ;; or the archive alone - which is what a reveal asks for.
(defdescribe
  sessions-window-reads-the-archive-view-by-name
  (it "sessions window reads the archive view by name"
      (let [seen
            (atom nil)

            page
            (fn [params]
              (with-redefs [state/list-sessions-page
                            (fn [channel opts]
                              (reset! seen [channel opts])
                              {:sessions [] :total 0 :limit 20 :next-cursor nil :has-more false})]
                (#'sessions-api/list-sessions-handler {:query-params params})))]

        ;; no parameter is the ACTIVE list, so a client that never heard of the archive is unchanged
        (page {})
        (expect (= :exclude (:archived (second @seen))))
        ;; a caller names the view it wants, in any case, and a blank names nothing
        (doseq [[param view]
                {"exclude" :exclude "include" :include "only" :only "ONLY" :only "" :exclude}]
          (page {"archived" param})
          (expect (= view (:archived (second @seen))) param))
        ;; a view this gateway does not have is refused before the window costs anything
        (with-redefs [state/list-sessions-page (fn [& _]
                                                 (throw (ex-info "windowed an unknown archive view"
                                                                 {})))]
          (let [response (#'sessions-api/list-sessions-handler {:query-params {"archived" "yes"}})]
            (expect (= 400 (:status response)))
            (expect (= "invalid-archived"
                       (get-in (wire/parse-json (:body response)) ["error" "type"]))))))))

;; A GROUP is a shelf: putting it away has to take its sessions out of sight WITH it and
  ;; stamp none of them, so unarchiving brings back exactly the rows it hid.
(defdescribe
  the-archive-on-a-group-hides-its-sessions-without-stamping-them
  (it
    "the archive on a group hides its sessions without stamping them"
    (let [gid
          (random-uuid)

          asked
          (atom [])

          group
          (fn [stamp]
            {"id" (str gid) "name" "Release apps" "archived_at" stamp})

          store
          {#'state/get-session-group (fn [g]
                                       (when (= gid g) (group nil)))
           #'state/update-session-group! (fn [_g opts]
                                           (swap! asked conj opts)
                                           (group (when (:archived? opts) 1717)))}

          patch-group
          (fn [body]
            (#'projects-api/patch-session-group-handler
             (merge {:request-method :patch :path-params {:gid (str gid)}} (json-body body))))]

      ;; archiving a group answers the group carrying the stamp the gateway wrote
      (with-redefs-fn store
        (fn []
          (let [response (patch-group {:archived true})]
            (expect (= 200 (:status response)))
            (expect (= 1717 (get (wire/parse-json (:body response)) "archived_at")))
            ;; No member is ever named: the SHELF is what the human put away.
            (expect (= [{:archived? true}] @asked)))))
      ;; unarchiving is the same route, and the stamp comes back empty
      (reset! asked [])
      (with-redefs-fn store
        (fn []
          (let [response (patch-group {:archived false})]
            (expect (= 200 (:status response)))
            (expect (nil? (get (wire/parse-json (:body response)) "archived_at")))
            (expect (= [{:archived? false}] @asked)))))
      ;; a group this gateway does not know is a 404, never a silent archive
      (with-redefs-fn {#'state/get-session-group (constantly nil)
                       #'state/update-session-group! (fn [& _]
                                                       (throw (ex-info "archived an unknown group"
                                                                       {})))}
        (fn []
          (expect (= 404 (:status (patch-group {:archived true}))))))
      ;; The other half of the reveal: an ACTIVE group opens its own archive by naming
      ;; itself, so the shelf paints the sessions filed under it without the fleet.
      ;; a group reveal cuts the session window to that group
      (let [seen (atom nil)]
        (with-redefs [state/list-sessions-page
                      (fn [channel opts]
                        (reset! seen [channel opts])
                        {:sessions [] :total 0 :limit 20 :next-cursor nil :has-more false})]
          (#'sessions-api/list-sessions-handler
           {:query-params {"group_id" (str gid) "archived" "only"}})
          (expect (= (str gid) (:group-id (second @seen))))
          (expect (= :only (:archived (second @seen)))))))))

;; READ-ONLY is the other half of the archive: the row leaves the lists AND stops taking
  ;; work. The app and the TUI disable their composer, so what reaches these routes is a
  ;; stale screen or a caller coming straight through the SDK - and the refusal names the
  ;; archive, because unarchiving is the whole fix.
(defdescribe
  an-archived-session-takes-no-new-work
  (it
    "an archived session takes no new work"
    (let [sid
          (str (random-uuid))

          archived?
          (atom true)

          refuses
          {#'state/session-archived? (fn [_sid]
                                       @archived?)
           #'state/submit-turn! (fn [& _]
                                  (throw (ex-info "submitted a turn into the archive" {})))
           #'state/set-session-model! (fn [& _]
                                        (throw (ex-info "pinned a model on the archive" {})))}

          submit
          (fn []
            (#'turns-api/submit-turn-handler
             (merge {:request-method :post :path-params {:sid sid}}
                    (json-body {:request "keep working"}))))

          pin
          (fn []
            (#'sessions-api/set-session-model-handler
             (merge {:request-method :patch :path-params {:sid sid}}
                    (json-body {:model "glm-5.2"}))))]

      ;; a turn aimed at an archived session is refused, and nothing is queued
      (with-redefs-fn refuses
        (fn []
          (let [response
                (submit)

                error
                (get (wire/parse-json (:body response)) "error")]

            (expect (= 409 (:status response)))
            (expect (= "session-archived" (get error "type")))
            (expect (= sid (get error "session_id")))
            ;; A human reads this one: it has to say what to do, not just what failed.
            (expect (re-find #"unarchive" (get error "message"))))))
      ;; pinning a model on an archived session is refused the same way
      (with-redefs-fn refuses
        (fn []
          (let [response (pin)]
            (expect (= 409 (:status response)))
            (expect (= "session-archived"
                       (get-in (wire/parse-json (:body response)) ["error" "type"]))))))
      ;; the SAME calls go through once the archive is lifted
      (reset! archived? false)
      (with-redefs-fn {#'state/session-archived? (fn [_sid]
                                                   @archived?)
                       #'state/submit-turn! (fn [_sid _opts]
                                              {:turn {:turn_id "turn-1"}})
                       #'state/set-session-model! (fn [& _]
                                                    nil)
                       #'state/session-model (fn [_sid]
                                               {:model "glm-5.2"})}
        (fn []
          (expect (= 202 (:status (submit))))
          (expect (= 200 (:status (pin))))))
      ;; the conversation is KEPT, so reading an archived session is never refused
      (reset! archived? true)
      (with-redefs-fn (assoc refuses
                        #'state/transcript-page (fn [_sid _opts]
                                                  {:turns [] :total 0 :offset 0 :has-more false}))
        (fn []
          (expect (= 200
                     (:status (#'transcripts-api/transcript-handler
                               {:path-params {:sid sid} :query-params {}})))))))))

;; A session still WORKING cannot be put away: the turn would keep running with nothing
  ;; in any list naming it, and cancelling it behind a swipe verb would be worse. The group
  ;; is a shelf, so ONE working member holds the whole shelf up - and the answer names that
  ;; session, because the human has to know where the work is.
(defdescribe
  work-in-flight-refuses-the-archive-and-names-the-session
  (it
    "work in flight refuses the archive and names the session"
    (let [sid
          (str (random-uuid))

          gid
          (random-uuid)

          asked
          (atom [])

          patch-session
          (fn [body]
            (#'sessions-api/patch-session-handler
             (merge {:request-method :patch :path-params {:sid sid}} (json-body body))))

          patch-group
          (fn [body]
            (#'projects-api/patch-session-group-handler
             (merge {:request-method :patch :path-params {:gid (str gid)}} (json-body body))))]

      ;; a session with work in flight refuses the archive and is never stamped
      (with-redefs-fn {#'state/session-working? (constantly true)
                       #'state/set-archived! (fn [& _]
                                               (throw (ex-info "archived a working session" {})))}
        (fn []
          (let [response
                (patch-session {:archived true})

                error
                (get (wire/parse-json (:body response)) "error")]

            (expect (= 409 (:status response)))
            (expect (= "session-busy" (get error "type")))
            (expect (= sid (get error "session_id"))))))
      ;; UNarchiving is never refused - it only brings the row back into the list
      (with-redefs-fn {#'state/session-working? (constantly true)
                       #'state/set-archived! (fn [_sid archived?]
                                               (swap! asked conj archived?)
                                               {"id" sid "archived_at" nil})}
        (fn []
          (expect (= 200 (:status (patch-session {:archived false}))))
          (expect (= [false] @asked))))
      ;; one working member refuses the whole group, and the answer names it
      (with-redefs-fn {#'state/get-session-group (fn [_g]
                                                   {"id" (str gid) "name" "Release apps"})
                       #'state/busy-session-in-group (fn [g]
                                                       (when (= (str gid) (str g)) sid))
                       #'state/update-session-group! (fn [& _]
                                                       (throw (ex-info "archived a working group"
                                                                       {})))}
        (fn []
          (let [response
                (patch-group {:archived true})

                error
                (get (wire/parse-json (:body response)) "error")]

            (expect (= 409 (:status response)))
            (expect (= "session-busy" (get error "type")))
            (expect (= sid (get error "session_id"))))))
      ;; unarchiving a group never asks whether a member is still working
      (with-redefs-fn {#'state/get-session-group (fn [_g]
                                                   {"id" (str gid)})
                       #'state/busy-session-in-group
                       (fn [& _]
                         (throw (ex-info "asked about work while unarchiving" {})))
                       #'state/update-session-group! (fn [_g _opts]
                                                       {"id" (str gid) "archived_at" nil})}
        (fn []
          (expect (= 200 (:status (patch-group {:archived false})))))))))

;; Regression: the settings mutation route answered 200 to every value it could
  ;; not store — a JSON `false` was read as "no value given" and ignored, the string
  ;; "false" was cast by truthiness into ON, an unknown enum choice changed nothing
  ;; silently, and `cycle` on a boolean surfaced as a 500 engine-error.
(defdescribe
  set-setting-value-action-refuses-what-it-cannot-store-test
  (it
    "set setting value action refuses what it cannot store"
    (toggles/register-toggle! {:id "server_test_value_bool" :label "Test bool" :default true})
    (toggles/register-toggle! {:id "server_test_value_enum"
                               :label "Test enum"
                               :type :enum
                               :choices ["quick" "deep"]
                               :default "quick"})
    (let [call
          (fn [request]
            (let [machine (atom {"toggles" {"server_test_value_bool" (toggles/value-of
                                                                       "server_test_value_bool")
                                            "server_test_value_enum" (toggles/value-of
                                                                       "server_test_value_enum")}})]
              (with-redefs [config/load-global-yaml-config-raw (constantly {})
                            config/load-global-config-raw #(deref machine)
                            config/update-machine-config! #(swap! machine %)]

                (#'settings-api/set-setting-handler request))))

          body
          (fn [json]
            {:body (java.io.StringReader. ^String json)})

          row
          (fn [response]
            (wire/parse-json (:body response)))]

      ;; a JSON false is a value, not a missing one
      (let [response (call (body (str "{\"id\":\"server_test_value_bool\","
                                      "\"action\":\"value\",\"value\":false}")))]
        (expect (= 200 (:status response)))
        (expect (false? (get (row response) "enabled")))
        (expect (false? (toggles/enabled? "server_test_value_bool"))))
      ;; the string false means OFF, never its truthy cast
      (toggles/set-value! "server_test_value_bool" true)
      (let [response (call {:query-params
                            {"id" "server_test_value_bool" "action" "value" "value" "false"}})]
        (expect (= 200 (:status response)))
        (expect (false? (toggles/enabled? "server_test_value_bool"))))
      ;; a token the type cannot name is a 400 that changes nothing
      (toggles/set-value! "server_test_value_bool" true)
      (let [response (call {:query-params
                            {"id" "server_test_value_bool" "action" "value" "value" "maybe"}})]
        (expect (= 400 (:status response)))
        (expect (true? (toggles/enabled? "server_test_value_bool"))))
      ;; the value action without a value is a 400, not a silent success
      (let [response (call {:query-params {"id" "server_test_value_bool" "action" "value"}})]
        (expect (= 400 (:status response)))
        (expect (true? (toggles/enabled? "server_test_value_bool"))))
      ;; an enum takes a registered choice and refuses the rest
      (expect (= 200
                 (:status (call {:query-params {"id" "server_test_value_enum"
                                                "action" "value"
                                                "value" "DEEP"}}))))
      (expect (= "deep" (toggles/value-of "server_test_value_enum")))
      (expect (= 400
                 (:status (call {:query-params {"id" "server_test_value_enum"
                                                "action" "value"
                                                "value" "banana"}}))))
      (expect (= "deep" (toggles/value-of "server_test_value_enum")))
      ;; cycle on a boolean is the client's 400, not the engine's 500
      (let [response (call {:query-params {"id" "server_test_value_bool" "action" "cycle"}})]
        (expect (= 400 (:status response))))
      ;; the default flip still flips
      (toggles/set-value! "server_test_value_bool" true)
      (expect (= 200
                 (:status (call {:query-params {"id" "server_test_value_bool"
                                                "action" "toggle"}}))))
      (expect (false? (toggles/enabled? "server_test_value_bool"))))))

;; Compression is a transport win everywhere EXCEPT the live stream, where the
  ;; deflater's buffering is indistinguishable from a stalled turn. Jetty happens to
  ;; ship `text/event-stream` in its default exclusions today, so this test is not
  ;; guarding our own `addExcludedMimeTypes` call so much as the invariant itself —
  ;; it fails whether the regression comes from our configurator or from a Jetty
  ;; default changing under us.
(defdescribe
  gzip-handler-never-compresses-the-live-stream-test
  (it "gzip handler never compresses the live stream"
      (let [^org.eclipse.jetty.server.handler.gzip.GzipHandler g ((rv 'gzip-handler))]
        ;; SSE never deflates
        (expect (false? (.isMimeTypeDeflatable g "text/event-stream"))
                "a buffered SSE body stops arriving as it happens - the live view freezes")
        ;; the surfaces gzip exists for still deflate
        (expect (true? (.isMimeTypeDeflatable g "application/json"))
                "the transcript envelope is why compression was turned on at all")
        ;; tiny envelopes are left alone
        (expect (= 1024 (.getMinGzipSize g))
                "Jetty's own 32-byte floor spends a deflate on bodies smaller than its header"))))

;; Every admitted client speaks the canonical protocol and rebuilds the picture.
(defdescribe
  live-close-uses-one-canonical-shape-test
  (it "live close uses one canonical shape"
      (let [slim
            (rv 'without-settled-picture)

            frame
            {"type" "view.close"
             "kind" "live"
             "seq" 7
             "result" {"view" {"id" "v1" "nodes" []} "reason" "completed" "is_completed" true}}

            out
            (slim frame)]

        (expect (nil? (get-in out ["result" "view"])))
        (expect (= "completed" (get-in out ["result" "reason"])))
        (expect (true? (get-in out ["result" "is_completed"])))
        (expect (= 7 (get out "seq")))
        (doseq [other [{"type" "view.open" "view" {"id" "v1"}}
                       {"type" "view.patch" "patch" {"ops" []}} {"type" "view.close"}
                       {"type" "content.block.delta" "result" {"view" "not a live close"}}]]
          (expect (= other (slim other)))))))

(defdescribe
  live-close-canonical-shape-reaches-every-socket-test
  (it
    "live close canonical shape reaches every socket"
    (with-redefs-fn {#'server/stop! (fn []
                                      nil)
                     #'state/soul (constantly {"id" "exists"})}
      (fn []
        (with-server-state!
          {}
          (fn []
            (let [multi-sse-body
                  (rv 'multi-sse-body)

                  write-body
                  (requiring-resolve 'ring.core.protocols/write-body-to-stream)

                  sid
                  (str (java.util.UUID/randomUUID))

                  outputs
                  [(java.io.ByteArrayOutputStream.) (java.io.ByteArrayOutputStream.)]

                  text
                  (fn [^java.io.ByteArrayOutputStream out]
                    (.toString out "UTF-8"))

                  pumps
                  (mapv (fn [out]
                          (future (try (write-body (multi-sse-body [[sid 0]] false nil) {} out)
                                       (catch Throwable _ nil))))
                        outputs)]

              (try (expect (wait-until #(every? (fn [out]
                                                  (re-find #"subscription.ready" (text out)))
                                                outputs)))
                   (state/append-event! sid
                                        "view.close"
                                        {:kind :live
                                         :view-id "v1"
                                         :result {:view {:id "v1" :title "Activity" :nodes []}
                                                  :reason "completed"
                                                  :is-completed true}})
                   (expect (wait-until #(every? (fn [out]
                                                  (re-find #"view.close" (text out)))
                                                outputs)))
                   (doseq [out (map text outputs)]
                     (expect (nil? (re-find #"\"view\"" out)))
                     (expect (nil? (re-find #"Activity" out)))
                     (expect (re-find #"completed" out))
                     (expect (re-find #"v1" out)))
                   (finally (doseq [pump pumps]
                              (future-cancel pump)))))))))))

(defdescribe council-routes-and-auth-test
             (it "council routes and auth"
                 ;; C18/C31: canonical routes stay authenticated, even while the feature is off.
                 (let [paths (set (map :path gateway-contract/route-table))]
                   (expect (contains? paths "/v1/sessions/:sid/council")))
                 (with-server-state!
                   {:require-token? true :token "fixture-token"}
                   (fn []
                     (let [gated ((rv 'wrap-auth) (constantly {:status 200}) "fixture-token" [])]
                       (expect (= 401
                                  (:status (gated {:uri "/v1/sessions/a/council/entries"
                                                   :headers {}})))))))))

(defdescribe
  council-gateway-roundtrip-test
  (it
    "council gateway roundtrip"
    ;; C18/C23/C26/C30/C31: no mocked Council operations, transport authorship or turn scheduling.
    (let [db
          (com.blockether.vis.internal.persistance.core/db-create-connection! :memory)

          store-session!
          (requiring-resolve
            'com.blockether.vis.internal.persistance.sqlite.test-helpers/store-session!)

          gid
          (str (:id (com.blockether.vis.internal.persistance.core/db-create-project!
                      db
                      {:name "Council wire"})))

          sid
          (str (store-session! db {:channel :api}))

          update!
          (ns-resolve 'com.blockether.vis.internal.gateway.state 'update-session!)

          drop!
          (ns-resolve 'com.blockether.vis.internal.gateway.state 'drop-session!)

          enabled
          (atom true)

          handler
          (rr/ring-handler ((rv 'router) "fixture-token" []))

          call
          (fn [method suffix body]
            (handler {:request-method method
                      :uri (str "/v1/sessions/" sid "/council" suffix)
                      :headers {}
                      :query-params {}
                      :body (when body
                              (java.io.ByteArrayInputStream. (.getBytes ^String (wire/json-str body)
                                                                        "UTF-8")))}))]

      (try
        (com.blockether.vis.internal.persistance.core/db-set-session-project! db sid gid)
        (update! sid
                 (constantly {:current-turn "wire"
                              :turns {"wire" {:status "running" :cancel-token {}}}}))
        (with-redefs [lp/db-info
                      (constantly db)

                      toggles/enabled?
                      (fn [id]
                        (and @enabled (= id "council")))]

          (let [binding
                (wire/parse-json (:body (call :get "" nil)))

                request
                ;; Complaint kinds use the current wire contract.
                {:kind "complain"
                 :content "Wire roundtrip"
                 :activation_id (get binding "activation_id")
                 :idempotency_key "wire-retry"}

                response
                (call :post "/entries" request)

                entry
                (wire/parse-json (:body response))

                id
                (get entry "entry_id")]

            (expect (= 200 (:status response)) (:body response))
            (expect (= sid (get entry "author_session_id")))
            (expect (= "sdk" (get entry "source")))
            (expect (= "complain" (get entry "kind")))
            (expect (not (contains? entry "id")))
            (doseq [kind [nil "question" "potential_issue" 7]]
              (expect (= 400 (:status (call :post "/entries" (assoc request :kind kind))))))
            (expect (= 400 (:status (call :post "/entries" (dissoc request :kind)))))
            (expect (= id
                       (get-in (wire/parse-json (:body (call :get "/threads" nil)))
                               ["entries" 0 "thread_id"])))
            (expect (= entry (wire/parse-json (:body (call :get (str "/entries/" id) nil)))))
            (expect
              (= 400 (:status (call :post "/entries" (assoc request :author_session_id "spoof")))))
            (update! sid #(assoc-in % [:turns "wire" :status] "completed"))
            (expect (= entry (wire/parse-json (:body (call :post "/entries" request)))))
            (expect (= 409
                       (:status (call :post "/entries" (assoc request :idempotency_key "new")))))
            ;; #202: a self-wake route uses the bound sid, not an activation or body target.
            (let [wake-request
                  {:kind "informational" :content "Build finished" :idempotency_key "self-wake"}

                  wake-response
                  (call :post "/wake" wake-request)

                  wake-entry
                  (wire/parse-json (:body wake-response))]

              (expect (= 200 (:status wake-response)))
              (expect (= sid (get wake-entry "author_session_id")))
              (expect (= [sid] (get wake-entry "ping")))
              (expect (= wake-entry (wire/parse-json (:body (call :post "/wake" wake-request)))))
              (doseq [extra [{:session_id "other"} {:activation_id "old"} {:ping ["other"]}]]
                (expect (= 400 (:status (call :post "/wake" (merge wake-request extra)))))))
            (reset! enabled false)
            (expect (= 409 (:status (call :get "/threads" nil))))))
        (finally (drop! sid)
                 (com.blockether.vis.internal.persistance.core/db-dispose-connection! db))))))

(defdescribe council-malformed-body-test
             (it "council malformed body"
                 (let [calls
                       (atom 0)

                       handlers
                       (mapv #'council-api/council-handler [:publish :wake])]

                   (with-redefs [state/council-operation! (fn [& _]
                                                            (swap! calls inc)
                                                            {})]
                     (doseq [handler handlers
                             raw ["{" "[]" "null" "42" "\"text\""]]

                       (let [response (try (handler {:path-params {:sid "fixture"}
                                                     :body (java.io.ByteArrayInputStream.
                                                             (.getBytes ^String raw "UTF-8"))})
                                           (catch Exception _ {:status 500}))]
                         (expect (= 400 (:status response)))))
                     (expect (zero? @calls))))))

(defdescribe
  council-unexpected-failure-is-not-client-error-test
  (it "council unexpected failure is not client error"
      (let [handler (#'council-api/council-handler :members)]
        (doseq [data [{} {:error :unexpected-storage-failure}]]
          (let [error (ex-info "fixture persistence unavailable" data)]
            (with-redefs [state/council-operation! (fn [& _]
                                                     (throw error))]
              (expect (identical? error
                                  (try (handler {:path-params {:sid "fixture"}})
                                       (catch clojure.lang.ExceptionInfo e e)))))))
        (doseq [[kind status] [[:invalid-request 400] [:invalid-thread 400] [:invalid-reply 400]
                               [:group-not-found 404] [:disabled 409] [:already-replied 409]]]
          (with-redefs [state/council-operation! (fn [& _]
                                                   (throw (ex-info "fixture" {:error kind})))]
            (expect (= status (:status (handler {:path-params {:sid "fixture"}})))))))))

(defdescribe
  session-alert-is-the-gateways-own-wording-test
  (it "session alert is the gateways own wording"
      ;; The desktop app cannot be pushed to (its WKWebView has no `PushManager`), so it raises its
      ;; own banners and asks THIS route what they say. One wording therefore reaches a phone and a
      ;; desktop window alike, instead of each surface flattening an answer its own way.
      (let [sid
            (random-uuid)

            handler
            #'sessions-api/session-alert-handler

            alert
            (fn [query]
              (wire/parse-json (:body (handler {:path-params {:sid (str sid)}
                                                :query-params query}))))]

        (with-redefs [state/soul
                      (fn [id]
                        (when (= (str sid) (str id)) {"title" "deploy"}))

                      state/newest-answer-text
                      (constantly "## Done\n\n- Shipped **v2** to `prod`")

                      gw-view/input-views
                      (constantly [{:id "v1"
                                    :title "Which branch should I deploy?"
                                    :description "main is two commits ahead of the tag."}])]

          ;; an answer is the session's own name and what vis said, flattened for a banner
          (expect (= {"title" "deploy" "body" "Done • Shipped v2 to prod"} (alert {})))
          ;; a parked run is the question it asked
          (expect (= {"title" "Action needed — Which branch should I deploy?"
                      "body" "main is two commits ahead of the tag."}
                     (alert {"reason" "question"})))
          ;; a session this gateway does not have has no banner
          (expect (= 404 (:status (handler {:path-params {:sid (str (random-uuid))}}))))))))

(defn- file-handler-test-dir
  "Repository fixture directory outside the mocked session workspace."
  ^java.io.File []
  (let [dir (io/file (System/getProperty "user.dir") "target")]
    (.mkdirs dir)
    dir))

(defn- ungranted-test-file
  []
  (.toFile (java.nio.file.Files/createTempFile (.toPath (file-handler-test-dir))
                                               "vis-outside"
                                               ".md"
                                               (make-array java.nio.file.attribute.FileAttribute
                                                           0))))

(defdescribe
  a-pressed-path-opens-only-when-the-session-can-read-it
  (it
    "a pressed path opens only when the session can read it"
    ;; BLO-172 and #284: the press hands a session-readable file to the editor
    ;; on the machine that ran the step; ungranted paths never reach the editor.
    (let [sid
          (random-uuid)

          root
          (.toFile (java.nio.file.Files/createTempDirectory
                     "vis-open-file"
                     (make-array java.nio.file.attribute.FileAttribute 0)))

          inside
          (io/file root "notes.md")

          outside
          (ungranted-test-file)

          opened
          (atom [])

          handler
          #'fs-api/open-file-handler

          answer
          (fn [body outcome &
               {:keys [session workspace] :or {session sid workspace (.getPath root)}}]
            (with-redefs-fn {#'gw-http/body-json (constantly body)
                             #'state/soul (fn [id]
                                            (when (= session id) {"id" (str id)}))
                             #'state/session-workspace-info (fn [id]
                                                              (when (= session id)
                                                                {"root" workspace}))
                             #'lp/env-for (fn [_]
                                            {:workspace {:root workspace :repo-root workspace}
                                             :security-policy {:jail-enabled true :process-jail {}}
                                             :security/filesystem-roots []})
                             #'external-opener/open-file-in-editor! (fn [path]
                                                                      (swap! opened conj path)
                                                                      outcome)}
              #(handler {:path-params {:sid (str sid)}})))]

      (spit inside "notes")
      (try
        ;; a path the session named opens on the machine that ran it
        (let [response (answer {"path" "notes.md"} {:status :ok})]
          (expect (= 200 (:status response)))
          (expect (str/includes? (:body response) "\"is_open\":true"))
          (expect (= [(.getPath (.getCanonicalFile inside))] @opened)))
        ;; an absolute path inside the workspace is the same file
        (expect (= 200 (:status (answer {"path" (.getPath inside)} {:status :ok}))))
        (expect (= 2 (count @opened)))
        ;; an ungranted path is never opened, including a relative escape
        (reset! opened [])
        (doseq [asked [(.getPath outside)
                       (str (.relativize (.toPath (.getCanonicalFile root))
                                         (.toPath (.getCanonicalFile outside))))]]
          (expect (= 403 (:status (answer {"path" asked} {:status :ok})))))
        (expect (= 404 (:status (answer {"path" "absent.md"} {:status :ok}))))
        (expect (= [] @opened))
        ;; an unusable request is refused before any editor is asked
        (doseq [body [{"path" "  "} {"path" 42} {}]]
          (expect (= 400 (:status (answer body {:status :ok})))))
        (expect (= 409 (:status (answer {"path" "notes.md"} {:status :ok} :workspace "  "))))
        (expect (= 404 (:status (answer {"path" "notes.md"} {:status :ok} :session (random-uuid)))))
        (expect (= [] @opened))
        ;; an editor that refused says so, instead of reporting an open file
        (let [response (answer {"path" "notes.md"}
                               {:status :failed :error "no editor is configured"})]
          (expect (= 400 (:status response)))
          (expect (str/includes? (:body response) "no editor is configured")))
        (finally (.delete inside) (.delete root) (.delete outside))))))

(defdescribe
  a-previewed-file-is-one-bounded-window-of-text
  (it "a previewed file is one bounded window of text"
      ;; The follow-up to BLO-172: a reader holding a phone gets nothing out of an editor
      ;; opening on the machine across the room, so the gateway answers with the lines
      ;; themselves, around the line the press named. Bounded on purpose — one window,
      ;; every line clipped, and never a binary file.
      (let [sid
            (random-uuid)

            root
            (.toFile (java.nio.file.Files/createTempDirectory
                       "vis-read-file"
                       (make-array java.nio.file.attribute.FileAttribute 0)))

            source
            (io/file root "long.txt")

            wide
            (io/file root "wide.txt")

            picture
            (io/file root "logo.png")

            outside
            (ungranted-test-file)

            handler
            #'fs-api/read-file-handler

            answer
            (fn [query & {:keys [session workspace] :or {session sid workspace (.getPath root)}}]
              (with-redefs-fn {#'state/soul (fn [id]
                                              (when (= session id) {"id" (str id)}))
                               #'state/session-workspace-info (fn [id]
                                                                (when (= session id)
                                                                  {"root" workspace}))
                               #'lp/env-for (fn [_]
                                              {:workspace {:root workspace :repo-root workspace}
                                               :security-policy {:jail-enabled true
                                                                 :process-jail {}}
                                               :security/filesystem-roots []})}
                #(handler {:path-params {:sid (str sid)} :query-params query})))

            window
            (fn [query]
              (wire/parse-json (:body (answer query))))]

        (spit source (str/join "\n" (map #(str "line " %) (range 1 1001))))
        (spit wide (str/join (repeat 5000 "x")))
        (with-open [out (java.io.FileOutputStream. picture)]
          (.write out (byte-array [(byte 80) (byte 78) (byte 71) (byte 0) (byte 13)])))
        (try
          ;; a preview stands at the line the press named, with what is around it
          (let [answered (window {"path" "long.txt" "line" "600"})]
            (expect (= 600 (get answered "line")))
            (expect (= 400 (get answered "first_line")))
            (expect (= 400 (count (get answered "lines"))))
            (expect (= "line 400" (first (get answered "lines"))))
            (expect (= "line 799" (last (get answered "lines"))))
            (expect (false? (get answered "is_truncated")))
            (expect (= (.length source) (get answered "size_bytes"))))
          ;; with no line named the window starts at the top of the file
          (let [answered (window {"path" "long.txt"})]
            (expect (= 1 (get answered "first_line")))
            (expect (= "line 1" (first (get answered "lines"))))
            (expect (= 400 (count (get answered "lines")))))
          ;; a file shorter than the window is all of it
          (let [answered (window {"path" "long.txt" "line" "998"})]
            (expect (= 798 (get answered "first_line")))
            (expect (= 203 (count (get answered "lines")))))
          ;; one enormous line is clipped, because no screen shows it whole
          (expect (= [2000] (mapv count (get (window {"path" "wide.txt"}) "lines"))))
          ;; a binary file is refused instead of answered as text
          (expect (= 415 (:status (answer {"path" "logo.png"}))))
          ;; an ungranted path is not read, including a relative escape
          (doseq [asked [(.getPath outside)
                         (str (.relativize (.toPath (.getCanonicalFile root))
                                           (.toPath (.getCanonicalFile outside))))]]
            (expect (= 403 (:status (answer {"path" asked})))))
          (expect (= 404 (:status (answer {"path" "absent.md"}))))
          (expect (= 400 (:status (answer {"path" "  "}))))
          (expect (= 409 (:status (answer {"path" "long.txt"} :workspace "  "))))
          (expect (= 404 (:status (answer {"path" "long.txt"} :session (random-uuid)))))
          ;; a line that is not a line is refused before the file is opened
          (doseq [asked ["0" "-3" "here"]]
            (expect (= 400 (:status (answer {"path" "long.txt" "line" asked})))))
          (finally (.delete source)
                   (.delete wide)
                   (.delete picture)
                   (.delete root)
                   (.delete outside))))))

(defdescribe
  session-readable-external-file-preview-test
  (it
    "session readable external file preview"
    ;; Regression #284: previews and editor opens follow the session's live read scope,
    ;; not just its primary workspace. Deny rules and private draft copies still win.
    (let [sid
          (random-uuid)

          base
          (.toFile (java.nio.file.Files/createTempDirectory
                     (.toPath (file-handler-test-dir))
                     "vis-session-files-"
                     (make-array java.nio.file.attribute.FileAttribute 0)))

          root
          (io/file base "workspace")

          extra
          (io/file base "extra")

          denied
          (io/file base "denied")

          trunk
          (io/file base "trunk")

          clone
          (io/file base "clone")

          _
          (doseq [dir [root extra denied trunk clone]]
            (.mkdir dir))

          readable
          (io/file extra "visible.txt")

          blocked
          (io/file denied "blocked.txt")

          local
          (io/file root "local.txt")

          trunk-file
          (io/file trunk "draft.txt")

          clone-file
          (io/file clone "draft.txt")

          outside-link
          (io/file root "outside-link.txt")

          mapped-link
          (io/file clone "mapped-link.txt")

          env*
          (atom {:workspace {:root (.getPath root) :repo-root (.getPath root) :filesystem-roots []}
                 :security-policy {:jail-enabled true :process-jail {}}
                 :security/filesystem-roots [(.getPath extra)]})

          opened
          (atom [])

          preview-handler
          #'fs-api/read-file-handler

          open-handler
          #'fs-api/open-file-handler

          request-base
          {:path-params {:sid (str sid)}}

          preview
          (fn [path]
            (preview-handler (assoc request-base :query-params {"path" path})))

          open
          (fn [path]
            (with-redefs-fn {#'gw-http/body-json (constantly {"path" path})}
              #(open-handler request-base)))

          lines
          (fn [response]
            (get (wire/parse-json (:body response)) "lines"))]

      (spit readable "outside the primary workspace")
      (spit blocked "never reveal this")
      (spit local "in the workspace")
      (spit trunk-file "live original")
      (spit clone-file "private copy")
      (java.nio.file.Files/createSymbolicLink (.toPath outside-link)
                                              (.toPath blocked)
                                              (make-array java.nio.file.attribute.FileAttribute 0))
      (java.nio.file.Files/createSymbolicLink (.toPath mapped-link)
                                              (.toPath blocked)
                                              (make-array java.nio.file.attribute.FileAttribute 0))
      (try
        (with-redefs [state/soul
                      (fn [id]
                        (when (= sid id) {"id" (str sid)}))

                      state/session-workspace-info
                      (fn [_]
                        {"root" (get-in @env* [:workspace :root])})

                      lp/env-for
                      (fn [_]
                        @env*)

                      external-opener/open-file-in-editor!
                      (fn [path]
                        (swap! opened conj path)
                        {:status :ok})]

          ;; a configured read root works for preview and editor open
          (expect (= ["outside the primary workspace"] (lines (preview (.getPath readable)))))
          (expect (= 200 (:status (preview "../extra/visible.txt"))))
          (expect (= 200 (:status (open (.getPath readable)))))
          (expect (= [(.getCanonicalPath readable)] @opened))
          (expect (= ["in the workspace"] (lines (preview "local.txt"))))
          ;; an ungranted path and a symlink leaving the roots stay closed
          (expect (= 403 (:status (preview (.getPath blocked)))))
          (expect (= 403 (:status (preview (.getPath outside-link)))))
          (expect (= 403 (:status (open (.getPath outside-link)))))
          (expect (= 1 (count @opened)))
          ;; removing a live root revokes both file actions
          (swap! env* assoc :security/filesystem-roots [])
          (expect (= 403 (:status (preview (.getPath readable)))))
          (expect (= 403 (:status (open (.getPath readable)))))
          (expect (= 1 (count @opened)))
          (swap! env* assoc :security/filesystem-roots [(.getPath extra)])
          ;; configured deny-read rules win over allowed roots
          (swap! env* assoc-in
            [:security-policy :process-jail :deny-read-rules]
            [(.getCanonicalPath readable)])
          (expect (= 403 (:status (preview (.getPath readable)))))
          (expect (= 403 (:status (open (.getPath readable)))))
          (expect (= 1 (count @opened)))
          (swap! env* assoc-in [:security-policy :process-jail] {})
          ;; extension file-read gates protect previews and opens alike
          (let [calls (atom [])]
            (with-redefs [extension/gate-hooked? (constantly true)
                          extension/run-gate-hooks (fn [_ _ context]
                                                     (swap! calls conj context)
                                                     {:reason "file is protected"})]

              (expect (= 403 (:status (preview (.getPath readable)))))
              (expect (= 403 (:status (open (.getPath readable)))))
              (expect (= [{:operation "file-read" :path (.getCanonicalPath readable)}
                          {:operation "file-read" :path (.getCanonicalPath readable)}]
                         @calls))
              (expect (= 1 (count @opened)))))
          ;; a draft reads and opens its private copy even with the host root granted
          (reset! env* {:workspace
                        {:root (.getPath clone) :repo-root (.getPath trunk) :filesystem-roots []}
                        :security-policy {:jail-enabled false
                                          :process-jail {}
                                          :draft-policies {(.getPath denied) :not-allowed
                                                           (.getPath extra) :copy-only}}
                        :security/filesystem-roots [(.getPath denied) (.getPath extra)]})
          (expect (= ["private copy"] (lines (preview (.getPath trunk-file)))))
          (expect (= 200 (:status (open (.getPath trunk-file)))))
          (expect (= (.getCanonicalPath clone-file) (last @opened)))
          (expect (= 403 (:status (preview (.getPath blocked)))))
          (expect (= 403 (:status (preview (.getPath readable)))))
          (expect (= 403 (:status (open (.getPath blocked)))))
          (expect (= 2 (count @opened)))
          ;; a mapped file may not follow a clone symlink into a withheld root
          (expect (= 403 (:status (preview (.getPath (io/file trunk "mapped-link.txt"))))))
          (expect (= 403 (:status (open (.getPath (io/file trunk "mapped-link.txt"))))))
          (expect (= 2 (count @opened))))
        (finally (doseq [file [mapped-link outside-link readable blocked local trunk-file
                               clone-file]]
                   (.delete file))
                 (doseq [dir [clone trunk denied extra root base]]
                   (.delete dir)))))))

(defdescribe voice-model-preload-follows-its-toggle
             ;; #275 follow-up: the gateway warms the transcription model once it is already
             ;; serving, so the first recording decodes instead of waiting for ~640 MB of model.
             (it "the switch off loads nothing at all"
                 (let [preloads (atom 0)]
                   (with-redefs [toggles/enabled? (fn [id]
                                                    (not= "speech_preload_model" id))
                                 speech/preload-transcription! (fn []
                                                                 (swap! preloads inc)
                                                                 true)]

                     (expect (= :off (#'server/preload-voice-model!)))
                     (expect (zero? @preloads)))))
             (it "the switch on warms the model OFF the thread that boots the gateway"
                 (let [preloaded (promise)]
                   (with-redefs [toggles/enabled? (fn [id]
                                                    (= "speech_preload_model" id))
                                 speech/preload-transcription! (fn []
                                                                 (deliver preloaded
                                                                          (Thread/currentThread))
                                                                 true)]

                     (expect (= :started (#'server/preload-voice-model!)))
                     (let [thread (deref preloaded 5000 nil)]
                       (expect (some? thread))
                       (expect (not= (Thread/currentThread) thread)))))))

;; Regression, this Vis session (paraphrased: "NEW should be one truth, not a
  ;; count every surface keeps for itself"): opening a conversation reports how far
  ;; this reader has read, and the receipt carries the position the gateway holds.
(defdescribe
  mark-session-read-route-reports-the-position-the-gateway-holds
  (it "mark session read route reports the position the gateway holds"
      (let [sid
            (java.util.UUID/randomUUID)

            marked
            (atom [])]

        (with-redefs-fn {#'state/soul (fn [s]
                                        {"id" (str s)})
                         #'state/mark-session-read!
                         (fn [s opts]
                           (swap! marked conj [(str s) opts])
                           {:session_id (str s) :reader "local" :seen_answers 4 :is_unread false})}
          (fn []
            (let [handler
                  #'sessions-api/mark-session-read-handler

                  params
                  {:path-params {:sid (str sid)}}]

              ;; an absent count means ALL of it - what OPENING a session reports
              (let [response (handler (merge params (json-body {})))]
                (expect (= 200 (:status response)))
                (expect (= [[(str sid) {:seen-answers nil}]] @marked))
                (let [body (wire/parse-json (:body response))]
                  (expect (= 4 (get body "seen_answers")))
                  (expect (false? (get body "is_unread")))))
              ;; a caller may name how far it has read
              (reset! marked [])
              (expect (= 200 (:status (handler (merge params (json-body {:seen_answers 2}))))))
              (expect (= [[(str sid) {:seen-answers 2}]] @marked))
              ;; a count that is not a whole number of answers is a 400, not a 500
              (let [response (handler (merge params (json-body {:seen_answers "soon"})))]
                (expect (= 400 (:status response)))
                (expect (= "invalid-request"
                           (get-in (wire/parse-json (:body response)) ["error" "type"]))))))))
      ;; a session the gateway does not know is a 404
      (with-redefs-fn {#'state/soul (constantly nil)}
        (fn []
          (expect (= 404
                     (:status (#'sessions-api/mark-session-read-handler
                               (merge {:path-params {:sid (str (java.util.UUID/randomUUID))}}
                                      (json-body {}))))))))))

;; The event stream is the gateway's largest payload and its most redundant one -
;; the same block ids, types and key names on every frame. A real capture of one
;; session compressed 16.1x (3,791,664 -> 236,162 bytes), which a phone on a
;; metered or distant link pays directly. `GzipHandler` cannot serve it: its
;; deflater buffers, so the stream compresses ITSELF, per connection, sync-flushed
;; per frame, and only when the client advertises `gzip`.
(defn- gzip-wire-sizes
  "Bytes that reach the SOCKET after each of `n` identical frames, for a stream
   built by `sse-stream` with `gzip?`."
  [gzip? n frame]
  (let [socket
        (java.io.ByteArrayOutputStream.)

        out
        ((rv 'sse-stream) socket gzip?)]

    (into []
          (map (fn [_]
                 (.write out (.getBytes ^String frame "UTF-8"))
                 (.flush out)
                 (.size socket)))
          (range n))))

(defn- gunzip
  [^bytes bytes]
  (slurp (java.util.zip.GZIPInputStream. (java.io.ByteArrayInputStream. bytes))))

(defdescribe
  sse-stream-compresses-only-when-the-client-asks-test
  (it "reads the client's Accept-Encoding, and nothing else"
      (let [gzip? (rv 'sse-gzip?)]
        (expect (gzip? {:headers {"accept-encoding" "gzip, deflate, br"}}))
        (expect (gzip? {:headers {"accept-encoding" "GZIP"}}))
        (expect (not (gzip? {:headers {"accept-encoding" "br"}})))
        (expect (not (gzip? {:headers {}})))
        (expect (not (gzip? {})))
        ;; What the Clojure reader actually sends for a stream (`client.clj`).
        (expect (not (gzip? {:headers {"accept-encoding" "identity"}})))
        ;; `q=0` is a REFUSAL, so a substring match would compress a stream the
        ;; client has already said it cannot inflate.
        (expect (not (gzip? {:headers {"accept-encoding" "gzip;q=0"}})))
        (expect (not (gzip? {:headers {"accept-encoding" "identity, gzip;q=0.000"}})))
        (expect (gzip? {:headers {"accept-encoding" "gzip;q=1.0, identity;q=0.5"}}))
        ;; A coding that merely CONTAINS the token is not the token.
        (expect (not (gzip? {:headers {"accept-encoding" "x-gzip-maybe"}})))))
  (it "declares the encoding it used, and varies on the request header"
      (let [headers (rv 'sse-encoding-headers)]
        (expect (= "text/event-stream" (get (headers false) "Content-Type")))
        (expect (not (contains? (headers false) "Content-Encoding")))
        (expect (= "gzip" (get (headers true) "Content-Encoding")))
        (expect (= "Accept-Encoding" (get (headers true) "Vary")))
        ;; The anti-buffering headers survive compression.
        (expect (= "no" (get (headers true) "X-Accel-Buffering")))))
  ;; This is the whole reason `GzipHandler` is not allowed near the live surface:
  ;; without a SYNC flush the deflater holds every frame until the stream closes.
  (it "puts every flushed frame on the wire instead of buffering the stream"
      (let [frame
            (str "id: 1\nevent: content.delta\ndata: " (apply str (repeat 400 "a")) "\n\n")

            sizes
            (gzip-wire-sizes true 10 frame)]

        ;; The first frame is already out, and every frame after it moves the wire.
        (expect (pos? (long (first sizes))))
        (expect (= sizes (sort sizes)))
        (expect (apply distinct? sizes))
        ;; …and it is genuinely smaller than what a plain stream would have sent.
        (expect (< (long (last sizes)) (* 10 (count frame))))))
  (it "hands the client back exactly the frames it was written"
      (let [socket
            (java.io.ByteArrayOutputStream.)

            out
            ((rv 'sse-stream) socket true)

            frames
            (str (sse/sse-frame {"seq" 1 "type" "turn.started"})
                 ": ping\n\n"
                 (sse/sse-frame {"seq" 2 "type" "content.delta"}))]

        (.write out (.getBytes frames "UTF-8"))
        (.flush out)
        (.close out)
        (expect (= frames (gunzip (.toByteArray socket))))
        ;; A heartbeat comment between two frames survives the round trip too.
        (expect (= ["turn.started" "content.delta"]
                   (mapv #(get % "type") (sse-jobs (gunzip (.toByteArray socket))))))))
  (it "leaves an uncompressed stream as the socket stream itself"
      (let [socket (java.io.ByteArrayOutputStream.)]
        (expect (identical? socket ((rv 'sse-stream) socket false)))))
  ;; The pad exists to push an edge proxy past its buffering threshold, and it is
  ;; the bytes ON THE WIRE that it counts: 8KB of spaces deflates to a few dozen,
  ;; so a compressed pad has to be incompressible to keep doing its job.
  (it "keeps the proxy pad above the buffering threshold under compression"
      (let [pad
            (rv 'sse-proxy-pad!)

            threshold
            (long @(rv 'SSE_PROXY_PAD_BYTES))

            wire-bytes
            (fn [gzip?]
              (let [socket (java.io.ByteArrayOutputStream.)]
                (pad ((rv 'sse-stream) socket gzip?) gzip?)
                (.size socket)))]

        (expect (> (long (wire-bytes false)) threshold))
        (expect (> (long (wire-bytes true)) threshold)))))

(defdescribe
  sse-body-serves-a-compressed-stream-test
  (it "gunzips to the same frames the plain body writes"
      (with-server-state!
        {}
        (fn []
          (let [status
                {"schema" 1
                 "type" "session.status"
                 "session_id" "fleet-session"
                 "seq" 1
                 "is_live" true}

                body-bytes
                (fn [gzip?]
                  (let [socket (java.io.ByteArrayOutputStream.)]
                    (ring-protocols/write-body-to-stream ((rv 'fleet-sse-body) false nil gzip?)
                                                         {}
                                                         socket)
                    (.toByteArray socket)))]

            (with-redefs-fn {#'server/stop! (constantly nil)
                             #'state/subscribe-fleet! (fn [_ sink]
                                                        (sink status))
                             #'state/unsubscribe-fleet! (constantly nil)
                             (rv 'pump-sse!)
                             (fn [_ queue _ write!]
                               (when-let [event (.poll ^java.util.concurrent.ArrayBlockingQueue
                                                       queue)]
                                 (write! event)))}
              (fn []
                (let [plain
                      (String. ^bytes (body-bytes false) "UTF-8")

                      compressed
                      (body-bytes true)

                      decoded
                      (gunzip compressed)]

                  ;; A real gzip member, not a plain body with a lying header.
                  (expect (= [0x1f 0x8b]
                             (mapv #(bit-and (long %) 0xff) (take 2 (seq compressed)))))
                  (expect (< (alength ^bytes compressed) (count plain)))
                  ;; The frames the client ends up holding are the frames the plain
                  ;; body writes. Only `subscription.ready`'s own clock differs
                  ;; between the two reads, so the comparison is on the frames.
                  (expect (= ["subscription.ready" "session.status"]
                             (mapv #(get % "type") (sse-jobs plain))))
                  (expect (= (mapv #(dissoc % "ts") (sse-jobs plain))
                             (mapv #(dissoc % "ts") (sse-jobs decoded))))
                  (expect (= (str/replace plain #"\"ts\":\d+" "")
                             (str/replace decoded #"\"ts\":\d+" ""))))))))))
  ;; Negotiation is the route's job, so the handler must agree with the body it built.
  (it "answers the gzip header only for a client that advertised gzip"
      (with-server-state!
        {}
        (fn []
          (with-redefs-fn {#'server/stop! (constantly nil)
                           #'state/soul (constantly {"id" "exists"})}
            (fn []
              (let [handler
                    (rv 'multi-events-handler)

                    sid
                    (str (java.util.UUID/randomUUID))

                    response
                    (fn [headers]
                      (handler {:query-params {"sids" sid} :headers headers}))]

                (expect (= 200 (:status (response {}))))
                (expect (nil? (get-in (response {}) [:headers "Content-Encoding"])))
                (expect (= "gzip"
                           (get-in (response {"accept-encoding" "gzip"})
                                   [:headers "Content-Encoding"])))
                ;; What a browser and a phone actually send.
                (expect (= "gzip"
                           (get-in (response {"accept-encoding" "gzip, deflate, br"})
                                   [:headers "Content-Encoding"])))
                (expect (nil? (get-in (response {"accept-encoding" "identity"})
                                      [:headers "Content-Encoding"]))))))))))
