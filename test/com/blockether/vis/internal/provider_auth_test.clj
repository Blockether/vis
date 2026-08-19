(ns com.blockether.vis.internal.provider-auth-test
  "The daemon-side OAuth broker. These tests pin the two properties the wire
   depends on: a flow's SECRET (PKCE verifier, device code) can never reach a
   client, and a flow id cannot be replayed once it settles."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.provider-auth :as pauth]
            [com.blockether.vis.internal.providers :as providers]
            [com.blockether.vis.internal.registry :as registry]
            [lazytest.core :refer [defdescribe expect it]]))

(defn- eventually
  [pred]
  (loop [attempts 100]
    (cond (pred) true
          (pos? attempts) (do (Thread/sleep 20) (recur (dec attempts)))
          :else false)))

(defdescribe provider-auth-support-test
             (it "reports no headless support for an unregistered provider"
                 (with-redefs [registry/provider-by-id (constantly nil)]
                   (expect (= false (pauth/supported? :nope)))
                   (let [result (pauth/start-auth! :nope)]
                     (expect (= false (:ok? result)))
                     (expect (= :unknown-provider (:error result))))))
             (it "reports no headless support for an OAuth provider with no headless leg"
                 (with-redefs [registry/provider-by-id
                               (constantly {:provider/auth-fn (constantly :ok)})

                               providers/auth-kind
                               (constantly :oauth)]

                   (expect (= false (pauth/supported? :legacy)))
                   (expect (= :auth-unsupported (:error (pauth/start-auth! :legacy)))))))

(defdescribe
  provider-auth-pkce-test
  (it "hands out a URL but NEVER the flow secret, and consumes the flow on success"
      (let [completed
            (atom nil)

            descriptor
            {:provider/auth-start-fn (fn []
                                       {:kind :pkce
                                        :url "https://auth.example/authorize?code_challenge=x"
                                        :flow {:verifier "TOP-SECRET-VERIFIER" :state "st"}})
             :provider/auth-complete-fn (fn [flow input]
                                          (reset! completed [flow input])
                                          {:status :ok})}]

        (with-redefs [registry/provider-by-id (constantly descriptor)]
          (let [{:keys [ok? flow]} (pauth/start-auth! :fake-pkce)]
            (expect (= true ok?))
            (expect (= "pkce" (:kind flow)))
            (expect (string? (:flow-id flow)))
            (expect (str/starts-with? (:url flow) "https://auth.example/"))
            ;; The allowlisted public view is the whole guarantee: no verifier,
            ;; no state, no nested :flow can ride out to a client.
            (expect (= false (str/includes? (pr-str flow) "TOP-SECRET-VERIFIER")))
            (expect (nil? (:flow flow)))
            (let [done (pauth/complete-auth! (:flow-id flow) "https://cb?code=abc&state=st")]
              (expect (= true (:ok? done)))
              (expect (= "ok" (:status done)))
              (expect (= "TOP-SECRET-VERIFIER" (get-in @completed [0 :verifier])))
              (expect (= "https://cb?code=abc&state=st" (nth @completed 1))))
            ;; Replaying the same flow id must fail: it was consumed.
            (expect (= :unknown-flow (:error (pauth/complete-auth! (:flow-id flow) "again"))))))))
  (it "rejects a completion with no pasted input"
      (with-redefs [registry/provider-by-id
                    (constantly {:provider/auth-start-fn (fn []
                                                           {:kind :pkce :url "https://a" :flow {}})
                                 :provider/auth-complete-fn (fn [& _]
                                                              {:status :ok})})]
        (let [{:keys [flow]} (pauth/start-auth! :fake-pkce)]
          (expect (= :missing-input (:error (pauth/complete-auth! (:flow-id flow) "  "))))
          ;; Still resumable — a blank paste must not burn the flow.
          (expect (= true (:ok? (pauth/complete-auth! (:flow-id flow) "https://cb?code=a")))))))
  (it "surfaces an upstream exchange failure as :auth-failed"
      (with-redefs [registry/provider-by-id
                    (constantly {:provider/auth-start-fn (fn []
                                                           {:kind :pkce :url "https://a" :flow {}})
                                 :provider/auth-complete-fn (fn [& _]
                                                              (throw (ex-info "upstream said no"
                                                                              {})))})]
        (let [{:keys [flow]} (pauth/start-auth! :fake-pkce)
              result (pauth/complete-auth! (:flow-id flow) "https://cb?code=a")]

          (expect (= false (:ok? result)))
          (expect (= :auth-failed (:error result)))
          (expect (= "upstream said no" (:message result)))))))

(defdescribe
  provider-auth-device-test
  (it "polls a device flow without blocking, then reports ok once it lands"
      (let [gate
            (promise)

            descriptor
            {:provider/auth-start-fn (fn []
                                       {:kind :device
                                        :user-code "ABCD-EFGH"
                                        :verification-uri "https://github.com/login/device"
                                        :interval-ms 5000
                                        :flow {:device-code "SECRET-DEVICE-CODE"}})
             :provider/auth-await-fn (fn [_flow]
                                       @gate
                                       :ok)}]

        (with-redefs [registry/provider-by-id (constantly descriptor)]
          (let [{:keys [ok? flow]} (pauth/start-auth! :fake-device)]
            (expect (= true ok?))
            (expect (= "device" (:kind flow)))
            (expect (= "ABCD-EFGH" (:user-code flow)))
            (expect (= false (str/includes? (pr-str flow) "SECRET-DEVICE-CODE")))
            ;; The await leg blocks on a daemon thread; the poll must not.
            (expect (= "pending" (:status (pauth/poll-auth! (:flow-id flow)))))
            (deliver gate :go)
            (expect (eventually #(= "ok" (:status (pauth/poll-auth! (:flow-id flow))))))
            ;; Settled flows are consumed, so a stale poller gets a clean 404.
            (expect (= :unknown-flow (:error (pauth/poll-auth! (:flow-id flow)))))))))
  (it "reports a failed device authorization as an error verdict"
      (let [descriptor {:provider/auth-start-fn (fn []
                                                  {:kind :device :user-code "X" :flow {}})
                        :provider/auth-await-fn (fn [_flow]
                                                  (throw (ex-info "access_denied" {})))}]
        (with-redefs [registry/provider-by-id (constantly descriptor)]
          (let [{:keys [flow]} (pauth/start-auth! :fake-device)]
            (expect (eventually #(= "error" (:status (pauth/poll-auth! (:flow-id flow)))))))))))

(defdescribe provider-auth-lifecycle-test
             (it "cancels a flow idempotently"
                 (with-redefs [registry/provider-by-id
                               (constantly {:provider/auth-start-fn
                                            (fn []
                                              {:kind :pkce :url "https://a" :flow {}})})]
                   (let [{:keys [flow]} (pauth/start-auth! :fake-pkce)]
                     (expect (= true (:ok? (pauth/cancel-auth! (:flow-id flow)))))
                     (expect (= true (:ok? (pauth/cancel-auth! (:flow-id flow)))))
                     (expect (= :unknown-flow (:error (pauth/poll-auth! (:flow-id flow))))))))
             (it "logs a provider out through its registered logout"
                 (let [logged-out? (atom false)]
                   (with-redefs [registry/provider-by-id
                                 (constantly {:provider/logout-fn (fn []
                                                                    (reset! logged-out? true))})]
                     (expect (= "logged-out" (:status (pauth/logout! :fake-pkce))))
                     (expect (= true @logged-out?)))))
             ;; A key-only provider has no OAuth session to revoke, but "log out" still has to
             ;; MEAN something: forget the key, keep the provider configured. Answering
             ;; :auth-unsupported made the gateway reply 400 and channels treat an ordinary
             ;; logout as a fatal error.
             (it "clears the stored api key and keeps the config entry when there is no logout"
                 (let [cleared (atom nil)]
                   (with-redefs [registry/provider-by-id (constantly {})
                                 providers/clear-provider-api-key!
                                 (fn [provider-id source]
                                   (reset! cleared {:provider-id provider-id :source source})
                                   true)]

                     (let [result (pauth/logout! :fake-key-provider)]
                       (expect (= true (:ok? result)))
                       (expect (= "logged-out" (:status result)))
                       (expect (= {:provider-id :fake-key-provider :source :provider-auth-logout}
                                  @cleared))))))
             (it "reports not-authenticated when there was no stored key to clear"
                 (with-redefs [registry/provider-by-id
                               (constantly {})

                               providers/clear-provider-api-key!
                               (constantly false)]

                   (let [result (pauth/logout! :fake-key-provider)]
                     (expect (= true (:ok? result)))
                     (expect (= "not-authenticated" (:status result)))))))

(defdescribe
  provider-auth-api-key-test
  ;; Plain API-key providers (zai, openai, …) take the SAME road as OAuth: the
  ;; client only COLLECTS the key, the daemon is what persists it. No channel
  ;; ever writes a provider credential itself.
  (it
    "mints an api-key flow with the provider's own guidance and persists daemon-side"
    (let [saved
          (atom nil)

          descriptor
          {:provider/auth-prompt-fn (constantly ["Create a key at example.com" "" "  Endpoint: x"])
           :provider/auth-fn (fn [& _]
                               (throw (ex-info "in-process auth must never run" {})))}]

      (with-redefs [registry/provider-by-id
                    (constantly descriptor)

                    providers/auth-kind
                    (constantly :api-key)

                    providers/save-provider-api-key!
                    (fn [pid k]
                      (reset! saved [pid k]))

                    providers/invalidate-configured-providers!
                    (constantly nil)]

        (expect (= true (pauth/supported? :fake-key)))
        (let [{:keys [ok? flow]} (pauth/start-auth! :fake-key)]
          (expect (= true ok?))
          (expect (= "api-key" (:kind flow)))
          ;; No url, no user code, and nothing for the client to exchange.
          (expect (nil? (:url flow)))
          (expect (= ["Create a key at example.com" "  Endpoint: x"] (:instructions flow)))
          (expect (= {:ok? false :error :missing-input :message "api_key is required"}
                     (pauth/complete-auth! (:flow-id flow) "   ")))
          (let [done (pauth/complete-auth! (:flow-id flow) "  sk-secret  ")]
            (expect (= true (:ok? done)))
            (expect (= "ok" (:status done)))
            (expect (= [:fake-key "sk-secret"] @saved)))
          ;; Consumed: a captured flow id cannot be replayed.
          (expect (= :unknown-flow (:error (pauth/complete-auth! (:flow-id flow) "sk-secret"))))))))
  (it "refuses an api-key flow for a provider that is not registered at all"
      (with-redefs [registry/provider-by-id (constantly nil)]
        (expect (= false (pauth/supported? :ghost)))
        (expect (= :unknown-provider (:error (pauth/start-auth! :ghost)))))))

(defdescribe
  provider-auth-abuse-test
  (it "stops a cancelled device flow's background poll instead of letting it hammer the provider"
      (let [polls
            (atom 0)

            descriptor
            {:provider/auth-start-fn (fn []
                                       {:kind :device :user-code "AB-CD" :flow {:device-code "dc"}})
             :provider/auth-await-fn (fn [_flow]
                                       (dotimes [_ 200]
                                         (swap! polls inc)
                                         (Thread/sleep 20))
                                       {:status :ok})}]

        (with-redefs [registry/provider-by-id (constantly descriptor)]
          (let [{:keys [flow]} (pauth/start-auth! :fake-device)
                await-future (get-in @(var-get #'pauth/flows) [(:flow-id flow) :await-future])]

            (expect (eventually #(pos? @polls)))
            (pauth/cancel-auth! (:flow-id flow))
            (expect (eventually #(future-done? await-future)))
            ;; A completed cancellation means the interrupted poll can no longer
            ;; increment `polls`; observing it now needs no timing padding.
            (let [frozen @polls]
              (expect (= frozen @polls)))))))
  (it "supersedes a provider's previous flow so retried starts cannot stack polls"
      (let [polls
            (atom 0)

            descriptor
            {:provider/auth-start-fn (fn []
                                       {:kind :device :user-code "AB-CD" :flow {:device-code "dc"}})
             :provider/auth-await-fn (fn [_flow]
                                       (dotimes [_ 200]
                                         (swap! polls inc)
                                         (Thread/sleep 20))
                                       {:status :ok})}]

        (with-redefs [registry/provider-by-id (constantly descriptor)]
          (let [ids (doall (for [_ (range 8)]
                             (:flow-id (:flow (pauth/start-auth! :fake-device)))))]
            (expect (= 8 (count (distinct ids))))
            ;; Only the newest id survives; every older one is already forgotten.
            (expect (every? (fn [id]
                              (= :unknown-flow (:error (pauth/poll-auth! id))))
                            (butlast ids)))
            (expect (= true (:ok? (pauth/poll-auth! (last ids)))))
            (let [await-future (get-in @(var-get #'pauth/flows) [(last ids) :await-future])]
              (pauth/cancel-auth! (last ids))
              (expect (eventually #(future-done? await-future)))
              (let [frozen @polls]
                (expect (= frozen @polls))))))))
  (it "trims the pasted redirect URL before handing it to the provider"
      (let [seen
            (atom nil)

            descriptor
            {:provider/auth-start-fn (fn []
                                       {:kind :pkce :url "https://auth" :flow {:verifier "v"}})
             :provider/auth-complete-fn (fn [_flow input]
                                          (reset! seen input)
                                          {:status :ok})}]

        (with-redefs [registry/provider-by-id (constantly descriptor)]
          (let [{:keys [flow]} (pauth/start-auth! :fake-pkce)]
            (expect (= true
                       (:ok? (pauth/complete-auth! (:flow-id flow)
                                                   "  https://cb?code=a&state=s\n\n"))))
            (expect (= "https://cb?code=a&state=s" @seen))))))
  (it
    "refuses anything that is not a single-line credential string, and keeps the flow retryable"
    (let [saved
          (atom nil)

          descriptor
          {:provider/auth-prompt-fn (fn []
                                      ["paste your key"])}]

      (with-redefs [registry/provider-by-id
                    (constantly descriptor)

                    providers/auth-kind
                    (constantly :api-key)

                    providers/save-provider-api-key!
                    (fn [pid k]
                      (reset! saved [pid k])
                      true)]

        (let [{:keys [flow]}
              (pauth/start-auth! :fake-key)

              fid
              (:flow-id flow)

              err
              (fn [in]
                (:error (pauth/complete-auth! fid in)))]

          ;; JSON gives us whatever the client posted — a map or vector would
          ;; otherwise be `str`'d straight into the config as the "key".
          (expect (= :invalid-input (err {"a" 1})))
          (expect (= :invalid-input (err ["a" "b"])))
          (expect (= :invalid-input (err "sk-a\nsk-b")))
          (expect (= :invalid-input (err "sk-a sk-b")))
          (expect (= :invalid-input (err (apply str (repeat 20000 "x")))))
          (expect (= :missing-input (err nil)))
          (expect (= :missing-input (err "   \n ")))
          (expect (nil? @saved))
          ;; None of that consumed the flow, so the user just types it again.
          (expect (= true (:ok? (pauth/complete-auth! fid "  sk-live-abc  "))))
          (expect (= [:fake-key "sk-live-abc"] @saved))
          (expect (= :unknown-flow (err "sk-live-abc")))))))
  (it "spends a flow exactly once even when two clients complete it at the same instant"
      (let [exchanges
            (atom 0)

            entered
            (java.util.concurrent.CountDownLatch. 1)

            release
            (java.util.concurrent.CountDownLatch. 1)

            descriptor
            {:provider/auth-start-fn (fn []
                                       {:kind :pkce :url "https://auth" :flow {:verifier "v"}})
             :provider/auth-complete-fn (fn [_flow _input]
                                          (swap! exchanges inc)
                                          (.countDown entered)
                                          (.await release)
                                          {:status :ok})}]

        (with-redefs [registry/provider-by-id (constantly descriptor)]
          (let [fid (:flow-id (:flow (pauth/start-auth! :fake-pkce)))
                first (future (pauth/complete-auth! fid "code"))]

            (expect (.await entered 1 java.util.concurrent.TimeUnit/SECONDS))
            (let [second (future (pauth/complete-auth! fid "code"))]
              (.countDown release)
              (let [results [@first @second]]
                (expect (= 1 @exchanges))
                (expect (= 1 (count (filter :ok? results))))
                (expect (= [:unknown-flow] (keep :error results)))))))))
  (it "hands a flow back after a failed exchange so a mistyped paste is retryable"
      (let [descriptor
            {:provider/auth-start-fn (fn []
                                       {:kind :pkce :url "https://auth" :flow {:verifier "v"}})
             :provider/auth-complete-fn
             (fn [_flow input]
               (if (= "good" input) {:status :ok} (throw (ex-info "invalid_grant" {}))))}]
        (with-redefs [registry/provider-by-id (constantly descriptor)]
          (let [fid (:flow-id (:flow (pauth/start-auth! :fake-pkce)))]
            (expect (= :auth-failed (:error (pauth/complete-auth! fid "bad"))))
            (expect (= "invalid_grant" (:message (pauth/complete-auth! fid "bad"))))
            (expect (= true (:ok? (pauth/complete-auth! fid "good"))))
            (expect (= :unknown-flow (:error (pauth/complete-auth! fid "good"))))))))
  (it "forgets a provider's in-flight flow when it is logged out"
      (let [descriptor {:provider/auth-start-fn
                        (fn []
                          {:kind :pkce :url "https://auth" :flow {:verifier "v"}})
                        :provider/auth-complete-fn (fn [_flow _input]
                                                     {:status :ok})
                        :provider/logout-fn (constantly true)}]
        (with-redefs [registry/provider-by-id (constantly descriptor)]
          (let [fid (:flow-id (:flow (pauth/start-auth! :fake-pkce)))]
            (expect (= true (:ok? (pauth/logout! :fake-pkce))))
            ;; A completion that lands after the logout must NOT re-authenticate.
            (expect (= :unknown-flow (:error (pauth/complete-auth! fid "https://cb?code=a")))))))))

(defdescribe provider-auth-self-minted-test
             ;; `api_key_command` mints the credential per request. Offering an auth flow
             ;; for such a provider invites a client to collect a key that would then
             ;; outrank the helper, so the daemon refuses it before any flow is minted.
             (it "refuses to start a flow for a provider whose credential is machine-minted"
                 (with-redefs [providers/configured-providers-cached
                               (constantly [{:id :corp :api-key-command "mint-token"}])

                               registry/provider-by-id
                               (constantly {:provider/auth-prompt-fn (constantly ["paste a key"])})

                               providers/auth-kind
                               (constantly :api-key)]

                   (expect (= false (pauth/supported? :corp)))
                   (let [result (pauth/start-auth! :corp)]
                     (expect (= false (:ok? result)))
                     (expect (= :auth-self-minted (:error result)))
                     (expect (str/includes? (:message result) "api_key_command")))))
             (it "still supports a plain API-key provider that is not command-minted"
                 (with-redefs [providers/configured-providers-cached
                               (constantly [{:id :corp :api-key "sk-1"}])

                               registry/provider-by-id
                               (constantly {:provider/auth-prompt-fn (constantly ["paste a key"])})

                               providers/auth-kind
                               (constantly :api-key)]

                   (expect (= true (pauth/supported? :corp))))))

(defdescribe provider-auth-managed-test
             ;; A provider its extension declares MANAGED has its credential issued by the
             ;; runtime. The daemon still minted an `api-key` flow for it, so any client —
             ;; the TUI band, the companion — collected a key that could never be used.
             (it "refuses to start a flow for a provider declared managed"
                 (with-redefs [providers/configured-providers-cached
                               (constantly [{:id :corp}])

                               registry/provider-by-id
                               (constantly {:provider/id :corp
                                            :provider/label "Corp"
                                            :provider/is-managed true})]

                   (expect (= false (pauth/supported? :corp)))
                   (let [result (pauth/start-auth! :corp)]
                     (expect (= false (:ok? result)))
                     (expect (= :auth-managed (:error result)))
                     (expect (str/includes? (:message result) "managed"))))))
