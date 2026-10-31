(ns com.blockether.vis.internal.foundation.mcp.oauth-test
  (:require [babashka.http-client :as http]
            [com.blockether.vis.internal.foundation.mcp.oauth :as oauth]
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
               (finally (.delete file)))))))

(defn- with-browser-flow
  [f]
  (let [exchanges (atom [])]
    (with-redefs-fn {#'oauth/auth-context (fn [& _]
                                            {:state "test-state" :code-verifier "test-verifier"})
                     #'oauth/authorize-url
                     (fn [_ _ redirect]
                       {:url (str "https://gateway.example.com/authorize?redirect_uri=" redirect)
                        :client-id "test-client"})
                     #'oauth/exchange-code! (fn [_ _ code redirect]
                                              (swap! exchanges conj [code redirect]))}
      (fn []
        (let [flow (oauth/start-authorization! "test" "https://gateway.example.com/mcp" {})]
          (try (f flow exchanges) (finally (oauth/cancel-authorization! (get flow "flow_id")))))))))

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
                       #'oauth/exchange-code! (fn [ctx _ code redirect]
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
                  signals (:signals (tel/with-signals
                                      (dotimes [_ 2]
                                        (try (bearer) (catch clojure.lang.ExceptionInfo _ nil)))))]

              (expect (not (.exists file)))
              (expect (empty? (filter #(= :com.blockether.vis.internal.provider.oauth/token-refresh
                                          (:id %))
                                      signals))))))))
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
             (finally (.delete file)))))
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
                        (.delete f)))))))
  (it "writes no record for a server that nobody signed in to"
      ;; Audit finding: a public MCP server logged a warning on every request.
      (let [file (java.io.File. (System/getProperty "java.io.tmpdir")
                                (str "vis-mcp-oauth-none-" (System/nanoTime) ".edn"))]
        (with-redefs-fn {#'oauth/token-file (constantly file)}
          (fn []
            (let [bearer
                  (oauth/make-bearer-fn "public" "https://gateway.example.com/mcp" (atom nil))
                  signals (:signals (tel/with-signals (dotimes [_ 2]
                                                        (try (bearer) (catch Exception _ nil)))))]

              (expect (empty? (filter #(= :com.blockether.vis.internal.provider.oauth/token-refresh
                                          (:id %))
                                      signals)))))))))
