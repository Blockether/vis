(ns com.blockether.vis.internal.gateway.provider-auth-test
  "Fresh OAuth across the real client, HTTP router, broker and provider descriptor.
   Only provider network responses and credential persistence are replaced."
  (:require [babashka.http-client :as http]
            [charred.api :as json]
            [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.internal.extension.registry :as registry]
            [com.blockether.vis.internal.foundation.mcp.core :as mcp]
            [com.blockether.vis.internal.foundation.mcp.oauth :as mcp-oauth]
            [com.blockether.vis.internal.gateway.client :as client]
            [com.blockether.vis.internal.gateway.server :as server]
            [com.blockether.vis.internal.provider.service :as providers]
            [com.blockether.vis.internal.provider.vendor.openai-codex :as codex]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is]]
            [reitit.ring :as ring]
            [ring.adapter.jetty9 :as jetty])
  (:import [org.eclipse.jetty.server Server ServerConnector]))

(defn- with-test-gateway
  [f]
  (let [gateway
        (jetty/run-jetty (ring/ring-handler (#'server/router nil []))
                         {:host "127.0.0.1" :port 0 :join? false})

        port
        (.getLocalPort ^ServerConnector (first (.getConnectors ^Server gateway)))]

    (try (with-redefs-fn {#'client/ensure-gateway! (constantly {:host "127.0.0.1" :port port})
                          #'client/ensure-client! (constantly "test-client")}
           f)
         (finally (.stop ^Server gateway)))))

(defn- gateway-json-request
  [path body]
  (let [response (client/request! :post path {:body body :timeout-ms 2000})]
    {:status (:status response) :json (wire/parse-json (:body response))}))

(defn- auth-request
  [provider action body]
  (gateway-json-request (str "/v1/providers/" provider "/auth/" action) body))

(defn- await-verdict
  [provider fid]
  (loop [remaining 100]
    (let [response (auth-request provider "poll" {:flow_id fid})]
      (if (and (= "pending" (get-in response [:json "status"])) (pos? remaining))
        (do (Thread/sleep 10) (recur (dec remaining)))
        response))))

(deftest codex-device-auth-crosses-the-gateway-wire
  (let [extension
        (atom nil)

        release
        (promise)

        requests
        (atom [])

        saved
        (atom [])

        access
        (str "test."
             (#'codex/base64url
              (.getBytes (json/write-json-str {(keyword "https://api.openai.com/auth")
                                               {:chatgpt_account_id "test-account"}})
                         java.nio.charset.StandardCharsets/UTF_8))
             ".test")]

    (with-redefs [vis/register-extension! (fn [ext]
                                            (reset! extension ext))]
      (codex/register!))
    (with-redefs-fn {#'registry/provider-by-id (constantly (first (:ext/providers @extension)))
                     #'providers/configured-providers-cached (constantly [])
                     #'providers/rebuild-shared-router! (constantly nil)
                     #'http/post
                     (fn [url opts]
                       (swap! requests conj url)
                       (cond (str/ends-with? url "/deviceauth/usercode")
                             {:status 200
                              :body (json/write-json-str {:device_auth_id "test-device-private"
                                                          :user_code "ABCD-EFGH"
                                                          :interval "1"})}
                             (str/ends-with? url "/deviceauth/token")
                             (do (deref release 3000 nil)
                                 {:status 200
                                  :body (json/write-json-str {:authorization_code "test-code"
                                                              :code_verifier "test-verifier"})})
                             (= "https://auth.openai.com/oauth/token" url)
                             (do (is (str/includes? (:body opts) "deviceauth%2Fcallback"))
                                 {:status 200
                                  :body (json/write-json-str {:access_token access
                                                              :refresh_token "test-refresh"
                                                              :expires_in 3600})})
                             :else (throw (ex-info "Unexpected test provider request" {}))))
                     #'codex/save-auth-file! (fn [credentials]
                                               (swap! saved conj credentials))}
      (fn []
        (with-test-gateway
          (fn []
            (let [{:keys [status json]}
                  (auth-request "openai-codex" "start" {})

                  fid
                  (get json "flow_id")]

              (try (is (= 200 status))
                   (is (= "device" (get json "kind")))
                   (is (= "ABCD-EFGH" (get json "user_code")))
                   (is (= 1000 (get json "interval_ms")))
                   (is (string? fid))
                   (is (not-any? #(str/includes? (pr-str json) %)
                                 ["test-device-private" "test-verifier" "test-refresh"]))
                   (is (= {:status 200 :json {"status" "pending"}}
                          (auth-request "openai-codex" "poll" {:flow_id fid})))
                   (deliver release true)
                   (is (= {:status 200 :json {"status" "ok"}} (await-verdict "openai-codex" fid)))
                   ;; A lost success response is safe to retry; it cannot exchange twice.
                   (is (= {:status 200 :json {"status" "ok"}}
                          (auth-request "openai-codex" "poll" {:flow_id fid})))
                   (is (= ["test-account"] (mapv :account-id @saved)))
                   (is (= 3 (count @requests)))
                   (finally (deliver release false)
                            (auth-request "openai-codex" "cancel" {:flow_id fid}))))))))))

(deftest browser-return-crosses-the-gateway-wire
  (let [port
        (with-open [socket (java.net.ServerSocket. 0)]
          (.getLocalPort socket))

        redirect
        (str "http://127.0.0.1:" port "/callback")

        exchanges
        (atom [])

        descriptor
        {:provider/auth-start-fn (fn []
                                   {:kind :pkce
                                    :url "https://gateway.example.com/authorize"
                                    :redirect-uri redirect
                                    :flow {:state "test-state" :verifier "test-verifier"}})
         :provider/auth-complete-fn (fn [flow input]
                                      (swap! exchanges conj [flow input]))}]

    (with-redefs [registry/provider-by-id
                  (constantly descriptor)

                  providers/configured-providers-cached
                  (constantly [])

                  providers/rebuild-shared-router!
                  (constantly nil)]

      (with-test-gateway
        (fn []
          (let [{:keys [status json]}
                (auth-request "test-browser" "start" {})

                fid
                (get json "flow_id")

                input
                (str redirect "?code=test-code&state=test-state")]

            (try (is (= 200 status))
                 (is (= "loopback" (get json "callback_mode")))
                 (is (= redirect (get json "redirect_uri")))
                 (is (not (str/includes? (pr-str json) "test-verifier")))
                 ;; This is the synthetic browser callback, not a gateway request.
                 (is (= 200 (:status (http/get input {:throw false :timeout 2000}))))
                 (is (= {:status 200 :json {"status" "ok"}} (await-verdict "test-browser" fid)))
                 (is (= 1 (count @exchanges)))
                 (finally (auth-request "test-browser" "cancel" {:flow_id fid})))))))))

(deftest mcp-app-return-registers-and-exchanges-through-the-gateway-wire
  (let [registrations
        (atom [])

        exchanges
        (atom [])

        saved
        (atom [])

        redirect
        "com.blockether.viscompanion://oauth/callback"

        request
        (fn [action body]
          (gateway-json-request (str "/v1/mcp/servers/work/auth/" action) body))]

    (with-redefs-fn {#'mcp/oauth-server-spec (constantly {:url "https://gateway.example.com/mcp"})
                     #'mcp/conn-of (constantly true)
                     #'mcp-oauth/auth-context
                     (fn [& _]
                       {:server "work"
                        :state "test-state"
                        :verifier "test-verifier"
                        :challenge "test-challenge"
                        :as-url "https://gateway.example.com"
                        :asmeta {"authorization_endpoint" "https://gateway.example.com/authorize"
                                 "registration_endpoint" "https://gateway.example.com/register"
                                 "token_endpoint" "https://gateway.example.com/token"}})
                     #'mcp-oauth/http-post-json (fn [url body]
                                                  (is (= "https://gateway.example.com/register"
                                                         url))
                                                  (swap! registrations conj body)
                                                  {:status 201 :body {"client_id" "test-client"}})
                     #'mcp-oauth/http-post-form (fn [url body]
                                                  (is (= "https://gateway.example.com/token" url))
                                                  (swap! exchanges conj body)
                                                  {:status 200
                                                   :body {"access_token" "test-access"
                                                          "refresh_token" "test-refresh"
                                                          "expires_in" 3600}})
                     #'mcp-oauth/write-tokens! (fn [server tokens]
                                                 (swap! saved conj [server tokens]))}
      (fn []
        (with-test-gateway
          (fn []
            (let [{:keys [status json]}
                  (request "start" {:callback_mode "app"})

                  id
                  (get json "flow_id")

                  input
                  (str redirect "?state=test-state&code=test-code")]

              (try (is (= 200 status))
                   (is (= "app" (get json "callback_mode")))
                   (is (= redirect (get json "redirect_uri")))
                   (is (= [redirect] (get (first @registrations) "redirect_uris")))
                   (is (str/includes?
                         (get json "url")
                         "redirect_uri=com.blockether.viscompanion%3A%2F%2Foauth%2Fcallback"))
                   (is (not (str/includes? (pr-str json) "test-verifier")))
                   (is (= 400 (:status (request "complete" {:flow_id id :input "test-code"}))))
                   (is (empty? @exchanges))
                   (is (= "ok"
                          (get-in (request "complete" {:flow_id id :input input})
                                  [:json "status"])))
                   (is (= "ok" (get-in (request "poll" {:flow_id id}) [:json "status"])))
                   (is (= 1 (count @exchanges)))
                   (is (= redirect (get (first @exchanges) "redirect_uri")))
                   (is (= "test-verifier" (get (first @exchanges) "code_verifier")))
                   (is (= "test-code" (get (first @exchanges) "code")))
                   (is (= "work" (ffirst @saved)))
                   (finally (request "cancel" {:flow_id id}))))))))))
