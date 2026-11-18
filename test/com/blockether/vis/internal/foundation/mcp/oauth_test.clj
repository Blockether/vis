(ns com.blockether.vis.internal.foundation.mcp.oauth-test
  (:require [babashka.http-client :as http]
            [charred.api :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.foundation.mcp.oauth :as oauth]
            [com.blockether.vis.internal.paths :as paths]
            [lazytest.core :refer [defdescribe expect it]]
            [taoensso.telemere :as tel]))

(defdescribe
  rejected-refresh-grant-test
  (it "forgets a permanently rejected grant, but preserves credentials on transient failures"
      ;; Regression: invalid_grant left Linear marked authorized with unusable tokens.
      (doseq [[status error authorized?] [[400 "invalid_grant" false]
                                          [503 "temporarily_unavailable" true]
                                          [400 "invalid_client" true]]]
        (let [file (java.io.File/createTempFile "vis-mcp-oauth-test-" ".edn")
              creds
              {:token "test-access" :refresh-token "test-refresh" :expires-at-ms Long/MAX_VALUE}]

          (try (spit file (pr-str creds))
               (with-redefs-fn {#'oauth/token-file (constantly file)
                                #'oauth/http-post-form (fn [& _]
                                                         {:status status :body {"error" error}})}
                 (fn []
                   (let [bearer (oauth/make-bearer-fn "remote"
                                                      "https://gateway.example.com/mcp"
                                                      (atom nil))]
                     (expect (= "test-access" (bearer)))
                     (expect (= :mcp/oauth-required
                                (try (bearer "test-access")
                                     nil
                                     (catch clojure.lang.ExceptionInfo e (:type (ex-data e))))))
                     (expect (= authorized? (get (oauth/token-status "remote") "is_authorized")))
                     (expect (= authorized?
                                (get (oauth/token-status "remote") "has_refresh_token"))))))
               (finally (.delete file) (.delete (java.io.File. (str file ".lock")))))))))

(defn- with-browser-flow
  ([f] (with-browser-flow {} f))
  ([context f]
   (let [exchanges (atom [])]
     (with-redefs-fn {#'oauth/auth-context
                      (fn [& _]
                        (merge {:state "test-state" :code-verifier "test-verifier"} context))
                      #'oauth/authorize-url
                      (fn [_ _ redirect]
                        {:url (str "https://gateway.example.com/authorize?redirect_uri=" redirect)
                         :client-id "test-client"})
                      #'oauth/exchange-code! (fn [_ _ _ code redirect]
                                               (swap! exchanges conj [code redirect]))}
       (fn []
         (let [flow (oauth/start-authorization! "test" "https://gateway.example.com/mcp" {})]
           (try (f flow exchanges)
                (finally (oauth/cancel-authorization! (get flow "flow_id"))))))))))

(defn- await-verdict
  [id]
  (loop [remaining 100]
    (let [view (oauth/poll-authorization! id)]
      (if (and (pos? remaining) (= "pending" (get view "status")))
        (do (Thread/sleep 20) (recur (dec remaining)))
        view))))

(defdescribe browser-return-test
             (it "receives a fresh callback and exchanges once, including a later manual replay"
                 (with-browser-flow (fn [flow exchanges]
                                      (let [id
                                            (get flow "flow_id")

                                            redirect
                                            (get flow "redirect_uri")

                                            input
                                            (str redirect "?code=test-code&state=test-state")]

                                        (expect (= 200 (:status (http/get input {:timeout 2000}))))
                                        (expect (= "ok" (get (await-verdict id) "status")))
                                        (expect (= [["test-code" redirect]] @exchanges))
                                        (expect (try (oauth/complete-authorization! id input)
                                                     false
                                                     (catch clojure.lang.ExceptionInfo _ true)))
                                        (expect (= 1 (count @exchanges)))))))
             (it "refuses a mismatched manual callback without consuming the pending flow"
                 (with-browser-flow
                   (fn [flow exchanges]
                     (let [id (get flow "flow_id")]
                       (expect (try (oauth/complete-authorization!
                                      id
                                      (str (get flow "redirect_uri")
                                           "?code=test-code&state=other-state"))
                                    false
                                    (catch clojure.lang.ExceptionInfo _ true)))
                       (expect (empty? @exchanges))
                       (expect (= "pending" (get (oauth/poll-authorization! id) "status")))))))
             (it "reports a denied browser authorization instead of leaving the client waiting"
                 (with-browser-flow
                   (fn [flow exchanges]
                     (expect (= 200
                                (:status (http/get (str (get flow "redirect_uri")
                                                        "?error=access_denied&state=test-state")
                                                   {:timeout 2000}))))
                     (expect (= "error" (get (await-verdict (get flow "flow_id")) "status")))
                     (expect (empty? @exchanges)))))
             (it "cancels the flow and releases its listener immediately"
                 (with-browser-flow (fn [flow exchanges]
                                      (oauth/cancel-authorization! (get flow "flow_id"))
                                      (expect (try (http/get (str (get flow "redirect_uri")
                                                                  "?code=x&state=test-state")
                                                             {:timeout 500})
                                                   false
                                                   (catch Exception _ true)))
                                      (expect (empty? @exchanges))))))

(defdescribe
  app-return-test
  (it
    "registers an app callback, keeps PKCE on the gateway and accepts a direct return once"
    (let [redirects
          (atom [])

          exchanges
          (atom [])]

      (with-redefs-fn {#'oauth/auth-context (fn [& _]
                                              {:state "test-state" :verifier "private-verifier"})
                       #'oauth/authorize-url
                       (fn [_ _ redirect]
                         (swap! redirects conj redirect)
                         {:url "https://gateway.example.com/authorize?state=test-state"
                          :client-id "test-client"})
                       #'oauth/exchange-code! (fn [ctx _ _ code redirect]
                                                (swap! exchanges conj
                                                  [(:verifier ctx) code redirect]))}
        (fn []
          (let [flow
                (oauth/start-authorization! "test"
                                            "https://gateway.example.com/mcp"
                                            {:callback-mode "app"})

                id
                (get flow "flow_id")

                redirect
                "com.blockether.viscompanion://oauth/callback"

                input
                (str redirect "?state=test-state&code=test-code")]

            (try (expect (= [redirect] @redirects))
                 (expect (= "app" (get flow "callback_mode")))
                 (expect (not (.contains (pr-str flow) "private-verifier")))
                 (doseq
                   [bad
                    ["test-code" (str input "&error=") (str input "&state=test-state")
                     (str redirect "?state=wrong&code=test-code")
                     "https://gateway.example.com/oauth/callback?state=test-state&code=test-code"]]
                   (expect (try (oauth/complete-authorization! id bad)
                                false
                                (catch clojure.lang.ExceptionInfo _ true))))
                 (expect (empty? @exchanges))
                 (expect (= "ok" (get (oauth/complete-authorization! id input) "status")))
                 (expect (= [["private-verifier" "test-code" redirect]] @exchanges))
                 (expect (try (oauth/complete-authorization! id input)
                              false
                              (catch clojure.lang.ExceptionInfo _ true)))
                 (finally (oauth/cancel-authorization! id))))))))
  (it "rejects arbitrary callback modes before discovery"
      (let [discovered (atom false)]
        (with-redefs-fn {#'oauth/auth-context (fn [& _]
                                                (reset! discovered true)
                                                {})}
          (fn []
            (doseq [mode ["relay" "https://gateway.example.com/callback" "unknown"]]
              (expect (try (oauth/start-authorization! "test"
                                                       "https://gateway.example.com/mcp"
                                                       {:callback-mode mode})
                           false
                           (catch clojure.lang.ExceptionInfo _ true))))
            (expect (false? @discovered)))))))

(defdescribe shared-auth-lifecycle-test
             (it
               "supersedes MCP starts just like model-provider starts and releases the old listener"
               (with-browser-flow (fn [old _]
                                    (with-browser-flow
                                      (fn [current _]
                                        (expect (not= (get old "flow_id") (get current "flow_id")))
                                        (expect (try (oauth/poll-authorization! (get old "flow_id"))
                                                     false
                                                     (catch clojure.lang.ExceptionInfo _ true)))
                                        (expect (try (http/get (str (get old "redirect_uri")
                                                                    "?code=test&state=test-state")
                                                               {:timeout 500})
                                                     false
                                                     (catch Exception _ true)))))))))

(defdescribe
  missing-refresh-token-test
  (it "writes no refresh record for a server that nobody signed in to"
      ;; Audit finding: every request to a public MCP server logged a warning.
      (let [file (java.io.File. (System/getProperty "java.io.tmpdir")
                                (str "vis-mcp-oauth-none-" (System/nanoTime) ".edn"))]
        (with-redefs-fn {#'oauth/token-file (constantly file)}
          (fn []
            (let [bearer
                  (oauth/make-bearer-fn "public" "https://gateway.example.com/mcp" (atom nil))
                  errors (atom [])
                  signals (:signals (tel/with-signals (dotimes [_ 2]
                                                        (try (bearer)
                                                             (catch clojure.lang.ExceptionInfo e
                                                               (swap! errors conj
                                                                 (:type (ex-data e))))))))]

              (expect (= [:mcp/oauth-required :mcp/oauth-required] @errors))
              (expect (not (.exists file)))
              (expect (empty? (filter #(= :com.blockether.vis.internal.provider.oauth/token-refresh
                                          (:id %))
                                      signals))))))))
  (it "uses a token whose server gave no expiry, until a server refuses it"
      ;; Release review 2026-11-17: a token response without `expires_in` gave a token
      ;; that Vis never sent, so each call asked for a new sign-in.
      (let [file (java.io.File/createTempFile "vis-mcp-oauth-test-" ".edn")]
        (try (spit file (pr-str {:token "test-access" :refresh-token nil :expires-at-ms nil}))
             (with-redefs-fn {#'oauth/token-file (constantly file)}
               (fn []
                 (let [bearer
                       (oauth/make-bearer-fn "remote" "https://gateway.example.com/mcp" (atom nil))]
                   (expect (= "test-access" (bearer)))
                   (expect (= :mcp/oauth-required
                              (try (bearer "test-access")
                                   (catch clojure.lang.ExceptionInfo e (:type (ex-data e)))))))))
             (finally (.delete file) (.delete (java.io.File. (str file ".lock")))))))
  (it "gives the token only to the origin of the sign-in"
      ;; Review 2026-11-17: the token file has only the name of the server. After the
      ;; URL of that name changed, the token of the old host went to the new host.
      (let [file
            (java.io.File/createTempFile "vis-mcp-oauth-test-" ".edn")

            posts
            (atom 0)

            cases
            [[{:server-origin "https://gateway.example.com"} "https://gateway.example.com/v2/mcp"
              true]
             [{:server-origin "https://gateway.example.com"} "https://other.example.net/mcp" false]
             [{:resource "https://gateway.example.com/"} "https://GATEWAY.example.com/mcp" true]
             [{:resource "https://gateway.example.com/"} "http://gateway.example.com/mcp" false]
             ;; Tokens of an older Vis, from a server without resource metadata.
             [{} "https://other.example.net/mcp" true]]

            save!
            #(spit file
                   (pr-str (merge {:token "test-access"
                                   :refresh-token "test-refresh"
                                   :expires-at-ms Long/MAX_VALUE}
                                  %)))

            answer
            (fn [f]
              (try (f) (catch clojure.lang.ExceptionInfo e (:type (ex-data e)))))]

        (try (with-redefs-fn {#'oauth/token-file (constantly file)
                              #'oauth/http-post-form (fn [& _]
                                                       (swap! posts inc)
                                                       {:status 200
                                                        :body {"access_token" "new-access"}})}
               (fn []
                 (doseq [[saved url sent?] cases]
                   (save! saved)
                   (reset! posts 0)
                   (let [bearer (oauth/make-bearer-fn "remote" url (atom nil))]
                     (expect (= (if sent? "test-access" :mcp/oauth-required) (answer #(bearer))))
                     (expect (= (if sent? ["new-access" 1] [:mcp/oauth-required 0])
                                [(answer #(bearer "test-access")) @posts]))))
                 (doseq [[saved url sent?] cases]
                   (save! saved)
                   (expect (= sent? (get (oauth/token-status "remote" url) "is_authorized"))))
                 ;; A refresh keeps the tokens of an older Vis for the URL that used them.
                 (save! {})
                 ((oauth/make-bearer-fn "remote" "https://gateway.example.com/mcp" (atom nil))
                   "test-access")
                 (expect (= :mcp/oauth-required
                            (answer (oauth/make-bearer-fn "remote"
                                                          "https://other.example.net/mcp"
                                                          (atom nil)))))))
             (finally (.delete file) (.delete (java.io.File. (str file ".lock")))))))
  (it "asks for sign-in and logs no-refresh-token when nothing can be refreshed"
      ;; Regression: a missing refresh token was logged as a failed exchange.
      (let [file (java.io.File/createTempFile "vis-mcp-oauth-test-" ".edn")]
        (try (spit file (pr-str {:token "test-access" :expires-at-ms 0}))
             (with-redefs-fn {#'oauth/token-file (constantly file)}
               (fn []
                 (let [bearer
                       (oauth/make-bearer-fn "remote" "https://gateway.example.com/mcp" (atom nil))
                       error (atom nil)
                       signal (tel/with-signal (try (bearer)
                                                    (catch clojure.lang.ExceptionInfo e
                                                      (reset! error e))))]

                   (expect (= :mcp/oauth-required (:type (ex-data @error))))
                   (expect (= {:store "mcp/remote" :outcome :no-refresh-token}
                              (select-keys (:data signal) [:store :outcome]))))))
             (finally (.delete file) (.delete (java.io.File. (str file ".lock")))))))
  (it
    "logs persist-failed and superseded for those MCP refresh results"
    ;; Regression: both logged as failed or rejected, which hid a lost rotation.
    (doseq [[outcome post!] [[:persist-failed
                              (fn [^java.io.File file]
                                (.setWritable (.getParentFile file) false)
                                {:status 200 :body {"access_token" "a" "refresh_token" "r2"}})]
                             [:superseded
                              (fn [^java.io.File file]
                                (spit file (pr-str {:token "other" :refresh-token "other-refresh"}))
                                {:status 400 :body {"error" "invalid_grant"}})]]]
      (let [dir (.toFile (java.nio.file.Files/createTempDirectory
                           "vis-mcp-oauth-test-"
                           (make-array java.nio.file.attribute.FileAttribute 0)))
            file (java.io.File. dir "remote.edn")]

        (try (spit file
                   (pr-str {:token "test-access" :refresh-token "test-refresh" :expires-at-ms 0}))
             (with-redefs-fn {#'oauth/token-file (constantly file)
                              #'oauth/http-post-form (fn [& _]
                                                       (post! file))}
               (fn []
                 (let [bearer
                       (oauth/make-bearer-fn "remote" "https://gateway.example.com/mcp" (atom nil))
                       signal (tel/with-signal (bearer))]

                   (expect (= outcome (get-in signal [:data :outcome])) (str outcome)))))
             (finally (.setWritable dir true)
                      (doseq [^java.io.File f (reverse (file-seq dir))]
                        (.delete f))))))))

(defn- authorization-server
  "HTTP stub for an MCP server whose authorization server answers registration with
   `register-status`, as Figma does with 403. Every request lands in `seen`."
  [register-status seen]
  (fn [{:keys [uri] :as request}]
    (swap! seen conj request)
    (cond
      (str/ends-with? uri "/.well-known/oauth-protected-resource")
      {:status 200
       :body
       "{\"resource\":\"https://mcp.example.test/mcp\",\"authorization_servers\":[\"https://auth.example.test\"]}"}
      (str/ends-with? uri "/.well-known/oauth-authorization-server")
      {:status 200
       :body
       (str
         "{\"authorization_endpoint\":\"https://auth.example.test/authorize\","
         "\"token_endpoint\":\"https://auth.example.test/token\","
         "\"registration_endpoint\":\"https://auth.example.test/register\","
         "\"token_endpoint_auth_methods_supported\":[\"client_secret_basic\",\"client_secret_post\"]}")}
      (str/ends-with? uri "/register") {:status register-status :body "Forbidden"}
      (str/ends-with? uri "/token") {:status 200
                                     :body "{\"access_token\":\"test-access\",\"expires_in\":3600}"}
      :else {:status 404 :body ""})))

(defdescribe
  oauth-client-name-test
  (it
    "sends each server's OAuth name without changing the default for other servers"
    (doseq [[hint expected] [[{:client-name "vis MCP client"} "vis MCP client"] [{} "Claude Code"]]]
      (let [seen (atom [])
            request (authorization-server 403 seen)]

        (with-redefs [http/request (fn [{:keys [uri] :as req}]
                                     (let [response (request req)]
                                       (if (str/ends-with? uri "/register")
                                         {:status 201 :body "{\"client_id\":\"registered-client\"}"}
                                         response)))]
          (let [flow (oauth/start-authorization! "client-name-test"
                                                 "https://mcp.example.test/mcp"
                                                 {:auth-hint hint})]
            (try (let [registration (first (filter #(str/ends-with? (:uri %) "/register") @seen))]
                   (expect (= expected (get (json/read-json (:body registration)) "client_name")))
                   ;; A web application may not register a loopback redirect (OpenID
                   ;; Connect registration), and `web` is the default.
                   (expect (= "native"
                              (get (json/read-json (:body registration)) "application_type")))
                   (expect (str/includes? (get flow "url") "client_id=registered-client")))
                 (finally (oauth/cancel-authorization! (get flow "flow_id"))))))))))

(defdescribe
  registered-client-secret-test
  (it
    "signs in and refreshes with the secret that registration issued"
    ;; Regression #458: Figma issues a secret at registration, and its token endpoint
    ;; refuses a request without it. Vis dropped the secret, so sign-in always failed.
    (let [seen
          (atom [])

          discovery
          (authorization-server 403 seen)

          file
          (java.io.File/createTempFile "vis-mcp-oauth-test-" ".edn")

          basic
          (str "Basic "
               (.encodeToString (java.util.Base64/getEncoder)
                                (.getBytes "registered-client:issued-secret" "UTF-8")))]

      (try
        (with-redefs-fn
          {#'oauth/token-file (constantly file)
           #'http/request
           (fn [{:keys [uri headers body] :as request}]
             (cond (str/ends-with? uri "/register")
                   {:status 201
                    :body (str "{\"client_id\":\"registered-client\","
                               "\"client_secret\":\"issued-secret\","
                               "\"token_endpoint_auth_method\":\"none\"}")}
                   (not (str/ends-with? uri "/token")) (discovery request)
                   (not= basic (get headers "Authorization"))
                   {:status 401 :body "{\"error\":\"invalid_client\"}"}
                   (str/includes? body "grant_type=refresh_token")
                   {:status 200 :body "{\"access_token\":\"refreshed-access\",\"expires_in\":3600}"}
                   :else {:status 200
                          :body (str "{\"access_token\":\"test-access\","
                                     "\"refresh_token\":\"test-refresh\",\"expires_in\":3600}")}))}
          (fn []
            (let [flow
                  (oauth/start-authorization! "registered-secret-test"
                                              "https://mcp.example.test/mcp"
                                              {})

                  state
                  (second (re-find #"[?&]state=([^&]+)" (get flow "url")))

                  bearer
                  (oauth/make-bearer-fn "registered-secret-test"
                                        "https://mcp.example.test/mcp"
                                        (atom nil))]

              (oauth/complete-authorization!
                (get flow "flow_id")
                (str (get flow "redirect_uri") "?code=test-code&state=" state))
              (expect (= "test-access" (bearer)))
              (expect (= "refreshed-access" (bearer "test-access"))))))
        (finally (.delete file) (.delete (java.io.File. (str file ".lock"))))))))

(defdescribe
  sign-in-refusal-test
  (it "names a refused client registration with its step, status and URL"
      ;; Regression #458: Figma answered 403, and Vis said it had no registration at all.
      (with-redefs [http/request (authorization-server 403 (atom []))]
        (let [data (try (oauth/start-authorization! "figma" "https://mcp.example.test/mcp" {})
                        nil
                        (catch clojure.lang.ExceptionInfo e
                          (assoc (ex-data e) :message (ex-message e))))]
          (expect (= :mcp/oauth-registration (:type data)))
          (expect (= "registration" (:step data)))
          (expect (= 403 (:upstream-status data)))
          (expect (= "https://auth.example.test/register" (:upstream-url data)))
          (expect (str/includes? (:message data)
                                 "HTTP 403 from https://auth.example.test/register")))))
  (it "signs in with a pre-registered client and secret, without registration"
      (let [seen
            (atom [])

            file
            (java.io.File/createTempFile "vis-mcp-oauth-test-" ".edn")]

        (try (with-redefs-fn {#'http/request (authorization-server 403 seen)
                              #'oauth/token-file (constantly file)}
               (fn []
                 (let [flow
                       (oauth/start-authorization! "figma"
                                                   "https://mcp.example.test/mcp"
                                                   {:auth-hint {:client-name "Claude Code"
                                                                :client-id "vis-test"
                                                                :client-secret "s3cret"}})

                       state
                       (second (re-find #"[?&]state=([^&]+)" (get flow "url")))]

                   (oauth/complete-authorization!
                     (get flow "flow_id")
                     (str (get flow "redirect_uri") "?code=test-code&state=" state))
                   (let [token (first (filter #(str/ends-with? (:uri %) "/token") @seen))]
                     (expect (not-any? #(str/ends-with? (:uri %) "/register") @seen))
                     (expect (str/includes? (get flow "url") "client_id=vis-test"))
                     (expect (= (str "Basic "
                                     (.encodeToString (java.util.Base64/getEncoder)
                                                      (.getBytes "vis-test:s3cret" "UTF-8")))
                                (get-in token [:headers "Authorization"])))
                     (expect (not (str/includes? (:body token) "s3cret")))
                     (expect (not (str/includes? (slurp file) "s3cret")))))))
             (finally (.delete file))))))

(defn- discovery-server
  "HTTP stub that answers only the metadata URLs in `documents` (URL to body map), a
   registration and a token request. Every requested URL lands in `seen`."
  [documents seen]
  (fn [{:keys [uri]}]
    (swap! seen conj uri)
    (cond (contains? documents uri) {:status 200 :body (json/write-json-str (get documents uri))}
          (str/ends-with? uri "/register") {:status 201
                                            :body "{\"client_id\":\"registered-client\"}"}
          :else {:status 404 :body "Not Found"})))

(defn- authorization-query
  "The query parameters of the authorization URL that sign-in gives for `documents`."
  [documents seen options]
  (with-redefs [http/request (discovery-server documents seen)]
    (let [flow
          (oauth/start-authorization! "discovery-test" "https://mcp.example.test/v1/mcp" options)]
      (oauth/cancel-authorization! (get flow "flow_id"))
      (assoc (#'oauth/query-parse (second (str/split (get flow "url") #"\?" 2)))
        ::endpoint (first (str/split (get flow "url") #"\?"))))))

(def ^:private endpoints
  {"authorization_endpoint" "https://auth.example.test/authorize"
   "token_endpoint" "https://auth.example.test/token"
   "registration_endpoint" "https://auth.example.test/register"})

(defdescribe
  metadata-discovery-test
  (it "reads the metadata URL that the server names in its challenge, and no other"
      (let [seen
            (atom [])

            query
            (authorization-query
              {"https://mcp.example.test/custom/metadata.json"
               {"resource" "https://mcp.example.test/v1/mcp"
                "authorization_servers" ["https://auth.example.test"]}
               "https://auth.example.test/.well-known/oauth-authorization-server" endpoints}
              seen
              {:www-auth (str "Bearer resource_metadata="
                              "\"https://mcp.example.test/custom/metadata.json\"")})]

        (expect (= "https://auth.example.test/authorize" (::endpoint query)))
        (expect (not-any? #(str/includes? % "oauth-protected-resource") @seen))))
  (it
    "finds the metadata of a server with a path before the metadata of its host"
    (let [seen (atom [])]
      (authorization-query {"https://mcp.example.test/.well-known/oauth-protected-resource/v1/mcp"
                            {"authorization_servers" ["https://auth.example.test"]}
                            "https://auth.example.test/.well-known/openid-configuration" endpoints}
                           seen
                           {})
      (expect (not-any? #{"https://mcp.example.test/.well-known/oauth-protected-resource"} @seen))))
  (it "finds an authorization server that has a path in its issuer"
      (doseq [location ["https://auth.example.test/.well-known/oauth-authorization-server/tenant1"
                        "https://auth.example.test/.well-known/openid-configuration/tenant1"
                        "https://auth.example.test/tenant1/.well-known/openid-configuration"]]
        (let [seen (atom [])
              query (authorization-query
                      {"https://mcp.example.test/.well-known/oauth-protected-resource"
                       {"authorization_servers" ["https://auth.example.test/tenant1"]}
                       location endpoints}
                      seen
                      {})]

          (expect (= "https://auth.example.test/authorize" (::endpoint query)))
          ;; The host can serve another tenant there.
          (expect (not-any? #{"https://auth.example.test/.well-known/oauth-authorization-server"}
                            @seen)))))
  (it "signs in to a server of protocol 2025-03-26, which has no resource metadata"
      (let [query (authorization-query
                    {"https://mcp.example.test/.well-known/oauth-authorization-server"
                     (assoc endpoints
                       "authorization_endpoint" "https://mcp.example.test/oauth/start")}
                    (atom [])
                    {})]
        (expect (= "https://mcp.example.test/oauth/start" (::endpoint query))))
      (let [seen
            (atom [])

            query
            (authorization-query {} seen {})]

        (expect (= "https://mcp.example.test/authorize" (::endpoint query)))
        (expect (some #{"https://mcp.example.test/register"} @seen)))))

(defdescribe
  authorization-scope-test
  (it "asks for the scope of the challenge, then the scopes of the resource, then none"
      (let [documents
            (fn [resource]
              {"https://mcp.example.test/.well-known/oauth-protected-resource"
               (merge {"authorization_servers" ["https://auth.example.test"]} resource)
               "https://auth.example.test/.well-known/oauth-authorization-server" endpoints})

            scope
            (fn [resource options]
              (get (authorization-query (documents resource) (atom []) options) "scope" ::none))]

        (expect (= "files:read"
                   (scope {"scopes_supported" ["a" "b"]}
                          {:www-auth "Bearer error=\"insufficient_scope\", scope=\"files:read\""})))
        (expect (= "a b" (scope {"scopes_supported" ["a" "b"]} {})))
        (expect (= ::none (scope {} {})))
        (expect (= "configured"
                   (scope {"scopes_supported" ["a" "b"]}
                          {:www-auth "Bearer scope=\"files:read\""
                           :auth-hint {:scope "configured"}})))))
  (it "keeps the scope of the last sign-in when the server asks for one more scope"
      ;; SEP-2350: the server names only the scope of the refused operation. Vis asked
      ;; for that scope alone, lost the first one, and signed in again without end.
      (let [file
            (java.io.File/createTempFile "vis-mcp-oauth-test-" ".edn")

            discovery
            (discovery-server {"https://mcp.example.test/.well-known/oauth-protected-resource"
                               {"authorization_servers" ["https://auth.example.test"]}
                               "https://auth.example.test/.well-known/oauth-authorization-server"
                               endpoints}
                              (atom []))

            start
            (fn [challenge]
              (oauth/start-authorization! "step-up-test"
                                          "https://mcp.example.test/v1/mcp"
                                          {:www-auth challenge}))]

        (try (with-redefs-fn {#'oauth/token-file (constantly file)
                              #'http/request
                              (fn [{:keys [uri] :as request}]
                                (if (str/ends-with? uri "/token")
                                  {:status 200
                                   :body "{\"access_token\":\"test-access\",\"expires_in\":3600}"}
                                  (discovery request)))}
               (fn []
                 (let [flow
                       (start "Bearer scope=\"files:read\"")

                       state
                       (second (re-find #"[?&]state=([^&]+)" (get flow "url")))]

                   (oauth/complete-authorization!
                     (get flow "flow_id")
                     (str (get flow "redirect_uri") "?code=test-code&state=" state))
                   (let [step-up (start
                                   "Bearer error=\"insufficient_scope\", scope=\"files:write\"")]
                     (oauth/cancel-authorization! (get step-up "flow_id"))
                     (expect (= "files:read files:write"
                                (get (#'oauth/query-parse
                                      (second (str/split (get step-up "url") #"\?" 2)))
                                     "scope")))))))
             (finally (.delete file) (.delete (java.io.File. (str file ".lock")))))))
  (it "asks for offline_access with the scope when the authorization server lists it"
      ;; Without it, such a server issues no refresh token: a sign-in each hour.
      (let [scope (fn [resource options]
                    (get (authorization-query
                           {"https://mcp.example.test/.well-known/oauth-protected-resource"
                            (merge {"authorization_servers" ["https://auth.example.test"]} resource)
                            "https://auth.example.test/.well-known/oauth-authorization-server"
                            (assoc endpoints "scopes_supported" ["a" "offline_access"])}
                           (atom [])
                           options)
                         "scope"
                         ::none))]
        (expect (= "a b offline_access" (scope {"scopes_supported" ["a" "b"]} {})))
        (expect (= "files:read offline_access"
                   (scope {} {:www-auth "Bearer scope=\"files:read\""})))
        (expect (= "a offline_access" (scope {"scopes_supported" ["a" "offline_access"]} {})))
        (expect (= ::none (scope {} {})))
        (expect (= "configured" (scope {} {:auth-hint {:scope "configured"}}))))))

(defn- sign-in-refusal
  "The data of the refusal that sign-in gives for `documents`, or nil when it starts."
  [documents]
  (try (authorization-query documents (atom []) {})
       nil
       (catch clojure.lang.ExceptionInfo e (assoc (ex-data e) :message (ex-message e)))))

(defdescribe
  metadata-validation-test
  (it "refuses resource metadata that is for another resource"
      (let [refusal (fn [resource]
                      (sign-in-refusal
                        {"https://mcp.example.test/.well-known/oauth-protected-resource/v1/mcp"
                         {"resource" resource "authorization_servers" ["https://auth.example.test"]}
                         "https://auth.example.test/.well-known/oauth-authorization-server"
                         endpoints}))]
        (doseq [resource ["https://mcp.example.test/v1/mcp" "https://mcp.example.test/v1/mcp/"
                          "https://mcp.example.test/v1" "https://mcp.example.test"
                          "https://MCP.example.test/"]]
          (expect (nil? (refusal resource))))
        (doseq [resource ["https://other.example.test/v1/mcp" "http://mcp.example.test/v1/mcp"
                          "https://mcp.example.test/v2" "https://mcp.example.test/v1/mcp/admin"
                          "https://mcp.example.test/v"]]
          (let [data (refusal resource)]
            (expect (= :mcp/oauth-discovery (:type data)))
            (expect (str/includes? (str (:message data)) resource))))))
  (it "refuses authorization server metadata that names another issuer"
      (let [refusal (fn [issuer]
                      (sign-in-refusal
                        {"https://mcp.example.test/.well-known/oauth-protected-resource"
                         {"authorization_servers" ["https://auth.example.test"]}
                         "https://auth.example.test/.well-known/oauth-authorization-server"
                         (assoc endpoints "issuer" issuer)}))]
        (expect (nil? (refusal "https://auth.example.test")))
        ;; Box publishes its issuer with another trailing slash than its resource metadata.
        (expect (nil? (refusal "https://auth.example.test/")))
        (let [data (refusal "https://attacker.example.test")]
          (expect (= :mcp/oauth-discovery (:type data)))
          (expect (str/includes? (str (:message data)) "https://attacker.example.test"))))))

(defdescribe
  authorization-response-issuer-test
  (let [completes?
        (fn [context query]
          (with-browser-flow
            context
            (fn [flow exchanges]
              (try (oauth/complete-authorization!
                     (get flow "flow_id")
                     (str (get flow "redirect_uri") "?code=test-code&state=test-state" query))
                   (catch clojure.lang.ExceptionInfo _ nil))
              (= 1 (count @exchanges)))))

        issuer
        "https://auth.example.test"

        sends-issuer
        {:as-url issuer
         :asmeta {"issuer" issuer "authorization_response_iss_parameter_supported" true}}]

    (it "exchanges the code only when the response comes from the authorization server"
        (expect (completes? sends-issuer "&iss=https%3A%2F%2Fauth.example.test"))
        (expect (not (completes? sends-issuer "&iss=https%3A%2F%2Fattacker.example.test")))
        (expect (not (completes? sends-issuer "&iss=https%3A%2F%2Fauth.example.test%2F")))
        (expect (not (completes? sends-issuer "")))
        (expect (completes? {:as-url issuer :asmeta {"issuer" issuer}} ""))
        (expect (not (completes? {:as-url issuer :asmeta {"issuer" issuer}}
                                 "&iss=https%3A%2F%2Fattacker.example.test")))
        (expect (not (completes? {:as-url issuer :asmeta {}}
                                 "&iss=https%3A%2F%2Fattacker.example.test"))))
    (it "tells the person why it refused the response"
        ;; #458: the verdict said only "Authorization failed"; the cause was in the log.
        (let [verdict (fn [query]
                        (with-browser-flow
                          sends-issuer
                          (fn [flow _]
                            (http/get (str (get flow "redirect_uri") "?state=test-state" query)
                                      {:timeout 2000})
                            (str (get (await-verdict (get flow "flow_id")) "error")))))]
          (expect (str/includes? (verdict "&code=c&iss=https%3A%2F%2Fattacker.example.test")
                                 "another issuer (https://attacker.example.test)"))
          (expect (str/includes? (verdict (str "&error=access_denied&error_description=No+consent"
                                               "&iss=https%3A%2F%2Fauth.example.test"))
                                 "access_denied (No consent)"))))))

(def ^:private client-document "https://vis.blockether.com/oauth/mcp-client.json")

(defn- client-of
  "The `client_id` of sign-in, and whether sign-in registered a client. The
   authorization server has `server` in its metadata and the web has `published`
   (URL to document)."
  [server published options]
  (let [seen
        (atom [])

        query
        (authorization-query (merge
                               {"https://mcp.example.test/.well-known/oauth-protected-resource"
                                {"authorization_servers" ["https://auth.example.test"]}
                                "https://auth.example.test/.well-known/oauth-authorization-server"
                                (merge endpoints server)}
                               published)
                             seen
                             options)]

    [(get query "client_id") (boolean (some #(str/ends-with? % "/register") @seen))]))

(defdescribe
  client-id-metadata-document-test
  (let [reads-documents
        {"client_id_metadata_document_supported" true}

        document
        {"client_id" client-document "redirect_uris" ["http://127.0.0.1/mcp-callback"]}]

    (it "names its published document as the client, and registers nothing"
        (expect (= [client-document false]
                   (client-of reads-documents {client-document document} {}))))
    (it "registers a client when the authorization server does not read documents"
        (expect (= ["registered-client" true] (client-of {} {client-document document} {}))))
    (it "registers a client when its document is not the one that the server would read"
        (doseq [published [{}
                           {client-document (assoc document "client_id" "https://other.example/c")}
                           {client-document
                            (assoc document "redirect_uris" ["http://127.0.0.1/other-callback"])}]]
          (expect (= ["registered-client" true] (client-of reads-documents published {})))))
    (it "names the configured document without reading it, and none when it is off"
        (expect (= ["https://id.example/vis.json" false]
                   (client-of reads-documents
                              {}
                              {:auth-hint {:client-metadata-url "https://id.example/vis.json"}})))
        (expect (= ["registered-client" true]
                   (client-of reads-documents
                              {client-document document}
                              {:auth-hint {:client-metadata-url false}})))
        (expect (= ["configured-client" false]
                   (client-of reads-documents
                              {client-document document}
                              {:auth-hint {:client-id "configured-client"}}))))
    (it "publishes a document that names itself and has each redirect of Vis"
        (let [published {client-document (json/read-json
                                           (slurp (io/resource
                                                    "vis-contract/oauth/mcp-client.json")))}]
          (doseq [mode [nil "app"]]
            (expect (= [client-document false]
                       (client-of reads-documents published {:callback-mode mode}))))))))

(defn- jwt-claims
  "The payload of the compact JWT `jwt`."
  [^String jwt]
  (json/read-json (String. (.decode (java.util.Base64/getUrlDecoder)
                                    ^String (second (str/split jwt #"\.")))
                           "UTF-8")))

(defn- with-token-file
  "Call `f` with the HTTP stub `handler` and a temporary token file."
  [handler f]
  (let [file (java.io.File/createTempFile "vis-mcp-oauth-test-" ".edn")]
    (try (with-redefs-fn {#'oauth/token-file (constantly file) #'http/request handler} f)
         (finally (.delete file) (.delete (java.io.File. (str file ".lock")))))))

(defdescribe
  machine-grant-test
  (it "gets a token with client credentials after a refusal, with no browser"
      ;; #458: a server for machines has no person to sign in. Vis asked for a sign-in.
      (let [seen
            (atom [])

            auth
            {:grant "client_credentials" :client-id "vis-test" :client-secret "s3cret"}]

        (with-token-file
          (authorization-server 403 seen)
          (fn []
            (let [bearer
                  (oauth/make-bearer-fn "machine"
                                        "https://mcp.example.test/mcp" (atom nil)
                                        "s3cret" auth)

                  token-requests
                  #(filter (fn [r]
                             (str/ends-with? (:uri r) "/token"))
                           @seen)]

              (expect (= "test-access" (bearer nil)))
              (expect (str/includes? (:body (first (token-requests)))
                                     "grant_type=client_credentials"))
              (expect (= "test-access" (bearer)))
              (expect (= 1 (count (token-requests))))
              (expect (= :mcp/oauth-error
                         (try (oauth/start-authorization! "machine"
                                                          "https://mcp.example.test/mcp"
                                                          {:auth-hint auth})
                              nil
                              (catch clojure.lang.ExceptionInfo e (:type (ex-data e)))))))))))
  (it "names the refusal of the authorization server, and asks for no sign-in"
      (let [discovery (authorization-server 403 (atom []))]
        (with-token-file
          (fn [{:keys [uri] :as request}]
            (if (str/ends-with? uri "/token")
              {:status 401 :body "{\"error\":\"invalid_client\"}"}
              (discovery request)))
          (fn []
            (let [bearer (oauth/make-bearer-fn "machine"
                                               "https://mcp.example.test/mcp" (atom nil)
                                               "wrong" {:grant "client_credentials"
                                                        :client-id "vis-test"
                                                        :client-secret "wrong"})
                  data (try (bearer nil) nil (catch clojure.lang.ExceptionInfo e (ex-data e)))]

              (expect (= :mcp/oauth-token (:type data)))
              (expect (= "invalid_client" (:oauth-error data)))))))))

(defdescribe
  dpop-test
  (it
    "binds the token to a key, and answers the nonce that the token endpoint asks for"
    ;; #458: a server that wants DPoP refused each token of Vis, which sent Bearer only.
    (let [proofs
          (atom [])

          documents
          {"https://mcp.example.test/.well-known/oauth-protected-resource"
           {"resource" "https://mcp.example.test/mcp"
            "authorization_servers" ["https://auth.example.test"]}
           "https://auth.example.test/.well-known/oauth-authorization-server"
           (assoc endpoints
             "issuer" "https://auth.example.test"
             "dpop_signing_alg_values_supported" ["ES256"])}

          discovery
          (discovery-server documents (atom []))]

      (with-token-file
        (fn [{:keys [uri headers] :as request}]
          (if (str/ends-with? uri "/token")
            (let [claims (jwt-claims (get headers "DPoP"))]
              (swap! proofs conj claims)
              (if (= "nonce-1" (get claims "nonce"))
                {:status 200
                 :body
                 "{\"access_token\":\"bound-access\",\"token_type\":\"DPoP\",\"expires_in\":3600}"}
                {:status 400
                 :headers {"dpop-nonce" "nonce-1"}
                 :body "{\"error\":\"use_dpop_nonce\"}"}))
            (discovery request)))
        (fn []
          (let [bearer
                (oauth/make-bearer-fn "bound"
                                      "https://mcp.example.test/mcp" (atom "DPoP algs=\"ES256\"")
                                      "s3cret" {:grant "client_credentials"
                                                :client-id "vis-test"
                                                :client-secret "s3cret"})

                proof
                (oauth/make-proof-fn "bound")]

            (expect (= "bound-access" (bearer nil)))
            (expect (= [nil "nonce-1"] (mapv #(get % "nonce") @proofs)))
            (expect (= #{"https://auth.example.test/token"} (set (map #(get % "htu") @proofs))))
            (let [claims (jwt-claims (proof "POST" "https://mcp.example.test/mcp?a=1"
                                            "bound-access" "nonce-2"))]
              (expect (= ["POST" "https://mcp.example.test/mcp" "nonce-2"]
                         (mapv claims ["htm" "htu" "nonce"])))
              (expect (string? (get claims "ath"))))
            (expect (nil? (proof "POST" "https://mcp.example.test/mcp" "other-token" nil)))))))))

(defn- free-port
  []
  (with-open [s (java.net.ServerSocket. 0)]
    (.getLocalPort s)))

(defn- sign-in!
  "Start a browser flow for `server` with `auth-hint` and pass it to `f`, then
   complete it with the redirect of the flow when `f` returns. Record each exchange
   as `[client-id client-secret redirect]`."
  ([exchanges server auth-hint] (sign-in! exchanges server auth-hint (fn [_])))
  ([exchanges server auth-hint f]
   (with-redefs-fn {#'oauth/auth-context (fn [& _] {:state "test-state" :verifier "test-verifier"})
                    #'oauth/authorize-url (fn [_ _ _]
                                            {:url "https://gateway.example.com/authorize"
                                             :client-id (or (:client-id auth-hint) "registered")})
                    #'oauth/exchange-code! (fn [_ client-id client-secret _ redirect]
                                             (swap! exchanges conj [client-id client-secret redirect]))}
     (fn []
       (let [flow (oauth/start-authorization! server
                                              "https://gateway.example.com/mcp"
                                              {:auth-hint auth-hint})]
         (try (f flow)
              (oauth/complete-authorization! (get flow "flow_id")
                                             (str (get flow "redirect_uri")
                                                  "?code=test-code&state=test-state"))
              flow
              (finally (oauth/cancel-authorization! (get flow "flow_id")))))))))

(defdescribe callback-port-test
  (it "listens on the configured port with the redirect URI of Claude Code"
      (let [port (free-port)
            exchanges (atom [])
            redirect (str "http://localhost:" port "/callback")]
        (sign-in! exchanges "fixed" {:client-id "pre" :callback-port port}
                  (fn [flow]
                    (expect (= redirect (get flow "redirect_uri")))
                    (expect (= "loopback" (get flow "callback_mode")))))
        (expect (= [["pre" nil redirect]] @exchanges))))
  (it "names the port when another program uses it"
      (with-open [busy (java.net.ServerSocket. 0 50 (java.net.InetAddress/getByName "127.0.0.1"))]
        (let [port (.getLocalPort busy)
              message (try (sign-in! (atom []) "fixed" {:client-id "pre" :callback-port port})
                           nil
                           (catch clojure.lang.ExceptionInfo e (ex-message e)))]
          (expect (str/includes? (str message) (str "callback port " port " is in use")))))))

(defdescribe stored-client-secret-test
  (it "sends the kept secret of the configured client at exchange and refresh, also after sign-out"
      (let [home (.toFile (java.nio.file.Files/createTempDirectory
                            "vis-mcp-secret-"
                            (make-array java.nio.file.attribute.FileAttribute 0)))
            exchanges (atom [])
            forms (atom [])]
        (try
          (with-redefs-fn {#'paths/vis-home (constantly (str home))}
            (fn []
              (oauth/save-client-secret! "figma" "kept-secret")
              (sign-in! exchanges "figma" {:client-id "pre"})
              ;; A configured secret wins, and a registered client gets no secret.
              (sign-in! exchanges "figma" {:client-id "pre" :client-secret "configured"})
              (sign-in! exchanges "figma" {})
              (expect (= [["pre" "kept-secret"] ["pre" "configured"] ["registered" nil]]
                         (mapv #(subvec % 0 2) @exchanges)))
              ;; Sign-out deletes the tokens, not the secret.
              (spit (io/file home "mcp-tokens" "figma.edn")
                    (pr-str {:token "old" :refresh-token "refresh" :expires-at-ms 0
                             :client-id "pre" :token-auth-method "client_secret_post"
                             :token-endpoint "https://gateway.example.com/token"
                             :server-origin "https://gateway.example.com"}))
              (with-redefs-fn {#'oauth/http-post-form
                               (fn [_ form _]
                                 (swap! forms conj form)
                                 {:status 200 :body {"access_token" "new" "expires_in" 3600}})}
                #(expect (= "new" ((oauth/make-bearer-fn "figma" "https://gateway.example.com/mcp"
                                                         (atom nil) nil {:client-id "pre"})
                                   "old"))))
              (expect (= "kept-secret" (get (first @forms) "client_secret")))
              (oauth/forget! "figma")
              (expect (not (.exists (io/file home "mcp-tokens" "figma.edn"))))
              (sign-in! exchanges "figma" {:client-id "pre"})
              (expect (= "kept-secret" (second (last @exchanges))))
              (expect (true? (oauth/delete-client-secret! "figma")))
              (sign-in! exchanges "figma" {:client-id "pre"})
              (expect (nil? (second (last @exchanges))))))
          (finally
            (doseq [f (reverse (file-seq home))] (.delete ^java.io.File f)))))))
