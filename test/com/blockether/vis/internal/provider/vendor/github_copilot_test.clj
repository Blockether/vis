(ns com.blockether.vis.internal.provider.vendor.github-copilot-test
  (:require [com.blockether.svar.core :as svar]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.internal.provider.vendor.github-copilot :as sut]
            [lazytest.core :refer [defdescribe describe expect it]]))

;; Regression, issue #169: Copilot shipped without a network envelope, so the gateway's
;; fixed 120-second backstop cancelled long-prefill sol calls before svar's watchdog did.
(defdescribe copilot-network-policy-test
             (it "ships the measured Copilot network envelope"
                 (let [expected
                       {:timeout-ms 900000
                        :ttft-timeout-ms 240000
                        :first-byte-timeout-ms 240000
                        :idle-timeout-ms 120000
                        :semantic-timeout-ms 300000}

                       providers
                       [(@#'sut/provider-entry)]]

                   (doseq [provider providers]
                     (expect (= expected (get-in provider [:provider/preset :network])))))))

(defdescribe copilot-luna-input-budget-test
             (it "uses Luna's 200K input cap through the pinned Svar dependency"
                 ;; Copilot advertises 328K total, including up to 128K output.
                 ;; The old Svar overlay incorrectly allowed 922K input.
                 (doseq [provider [(@#'sut/provider-entry)]]
                   (let [router (svar/make-router [(assoc (:provider/preset provider)
                                                     :id (:provider/id provider)
                                                     :api-key "test"
                                                     :models [{:name "gpt-5.6-luna"}])])
                         model (-> router
                                   :providers
                                   first
                                   :models
                                   first)]

                     (expect (= 200000 (:context model)))
                     (expect (= 200000 (:input-limit model)))
                     (expect (= 128000 (:output-limit model)))))))

(defdescribe copilot-astra-catalog-test
             (it "uses Astra's catalog input/output budgets and metered pricing through pinned Svar"
                 ;; Regression: the old overlay hid catalog limits behind a 272K input cap
                 ;; and free pricing, and advertised an unsupported ultra effort.
                 (let [provider
                       (@#'sut/provider-entry)

                       router
                       (svar/make-router [(assoc (:provider/preset provider)
                                            :id (:provider/id provider)
                                            :api-key "test"
                                            :models [{:name "gpt-6-astra"}])])

                       model
                       (-> router
                           :providers
                           first
                           :models
                           first)]

                   (expect (= 922000 (:context model) (:input-limit model)))
                   (expect (= 128000 (:output-limit model)))
                   (expect (= [{:type "effort" :values ["low" "medium" "high" "xhigh" "max"]}]
                              (:reasoning-options model)))
                   (expect (= {:input 10.0 :output 50.0 :cached-input 1.0}
                              (select-keys (:pricing model) [:input :output :cached-input]))))))

(defdescribe copilot-sol-luna-catalog-test
             (it "exposes Sol and Luna with published Svar metadata through the Vis preset"
                 (let [provider
                       (@#'sut/provider-entry)

                       defaults
                       (get-in provider [:provider/preset :default-models])

                       router
                       (svar/make-router [(assoc (:provider/preset provider)
                                            :id (:provider/id provider)
                                            :api-key "test"
                                            :models [{:name "gpt-6-sol"} {:name "gpt-6-luna"}])])]

                   (doseq [model (:models (first (:providers router)))]
                     (expect (some #{(:name model)} defaults))
                     (expect (= 922000 (:context model) (:input-limit model)))
                     (expect (= 128000 (:output-limit model)))
                     (expect (= :openai-compatible-responses (:api-style model)))
                     (expect (= :openai-effort (:reasoning-style model)))
                     (expect (= [{:type "effort"
                                  :values ["none" "low" "medium" "high" "xhigh" "max"]}]
                                (:reasoning-options model)))
                     (expect (= #{:chat :vision} (:capabilities model)))
                     (expect (= (if (= "gpt-6-sol" (:name model))
                                  {:input 2.0 :cached-input 0.2 :output 10.0}
                                  {:input 0.1 :cached-input 0.01 :output 0.5})
                                (select-keys (:pricing model) [:input :cached-input :output])))))))

(defdescribe copilot-gpt6-policy-test
             (it "requests Copilot policy access for Sol and Luna"
                 (let [requested (atom #{})]
                   (with-redefs-fn {#'sut/enable-copilot-model! (fn [_ _ model]
                                                                  (swap! requested conj model)
                                                                  true)}
                     (fn []
                       (#'sut/enable-known-copilot-models! "token" "https://api.githubcopilot.com")
                       (expect (contains? @requested "gpt-6-sol"))
                       (expect (contains? @requested "gpt-6-luna")))))))

(defdescribe
  provider-registration-test
  (it "registers ONE GitHub Copilot provider, not one per seat tier"
      (sut/register!)
      (let [copilot
            (vis/provider-by-id :github-copilot)

            ext-nses
            (set (map :ext/name (vis/registered-extensions)))

            models
            (set (get-in copilot [:provider/preset :default-models]))]

        (expect (= :github-copilot (:provider/id copilot)))
        (expect (= "GitHub Copilot" (:provider/label copilot)))
        ;; The seat tier was a question asked before any credential existed, and the
        ;; only thing the answer changed was a base-url the token exchange overrode.
        (expect (nil? (vis/provider-by-id :github-copilot-individual)))
        (expect (nil? (vis/provider-by-id :github-copilot-business)))
        (expect (nil? (vis/provider-by-id :github-copilot-enterprise)))
        (expect (contains? ext-nses "provider-github-copilot"))
        ;; One entry for the account - the old `...-responses` / `...-chat` per-wire
        ;; sub-providers are gone; one base-url `/v1` carries both wires.
        (expect (nil? (vis/provider-by-id :github-copilot-responses)))
        (expect (nil? (vis/provider-by-id :github-copilot-chat)))
        ;; Bootstrap host only: the token's own `endpoints.api` decides at call time.
        (expect (= "https://api.individual.githubcopilot.com/v1"
                   (get-in copilot [:provider/preset :base-url])))
        (expect (= "/responses" (get-in copilot [:provider/preset :responses-path])))
        ;; The curated defaults intentionally contain only the current cacheable fleets.
        (expect (= #{"claude-opus-5" "claude-fable-5" "claude-sonnet-5" "gpt-6-astra" "gpt-6-sol"
                     "gpt-6-luna" "gpt-5.6-luna" "gpt-5.6-sol" "gpt-5.6-terra"}
                   models))
        (expect (not-any? #(re-find #"(?i)gemini|grok" %) models))
        (expect (ifn? (:provider/status-fn copilot)))
        (expect (ifn? (:provider/logout-fn copilot)))
        (expect (ifn? (:provider/detect-fn copilot)))
        (expect (ifn? (:provider/auth-fn copilot)))
        (expect (ifn? (:provider/get-token-fn copilot)))
        (expect (ifn? (:provider/limits-fn copilot)))))
  (it "requests Copilot policy access for Claude Fable 5.1"
      (let [requested (atom #{})]
        (with-redefs-fn {#'sut/enable-copilot-model! (fn [_ _ model]
                                                       (swap! requested conj model)
                                                       true)}
          (fn []
            (#'sut/enable-known-copilot-models! "token" "https://api.githubcopilot.com")
            (expect (contains? @requested "claude-fable-5.1"))))))
  (describe "credential-detect"
            (it "detects the one Copilot credential whatever tier minted it"
                (sut/register!)
                (let [detect? (fn []
                                (boolean ((:provider/detect-fn (vis/provider-by-id
                                                                 :github-copilot)))))]
                  ;; Issue #48 lit up all three tier rows from ONE token file, and the
                  ;; fix was per-tier gating. With one provider there is one row, so a
                  ;; credential minted for ANY seat authenticates it.
                  (doseq [tier [:individual :business :enterprise]]
                    (with-redefs-fn {#'sut/detect-oauth-token (constantly {:oauth-token "tok"})
                                     #'sut/credential-account-type (constantly tier)}
                      (fn []
                        (expect (detect?)))))
                  (with-redefs-fn {#'sut/detect-oauth-token (constantly nil)}
                    (fn []
                      (expect (not (detect?))))))))
  (it "returns Vis-owned static LLM headers with cached Copilot token"
      (reset! @#'sut/token-cache {:token "tid=x;proxy-ep=proxy.individual.githubcopilot.com;exp=1"
                                  :expires-at-ms (+ (System/currentTimeMillis) 600000)
                                  :account-type :individual
                                  :api-url "https://api.individual.githubcopilot.com/v1"})
      (let [token (sut/get-copilot-token!)]
        (expect (= "https://api.individual.githubcopilot.com/v1" (:api-url token)))
        (expect (= "GitHubCopilotChat/0.26.7" (get-in token [:llm-headers "User-Agent"])))
        (expect (= "vscode-chat" (get-in token [:llm-headers "Copilot-Integration-Id"]))))))

(it "honors refresh_in: a token past its proactive-refresh deadline is NOT re-served (issue #16)"
    ;; refresh_in expired but hard expires_at far in the future. The old code
    ;; trusted expires_at and re-served the proxy-rejected token forever (401
    ;; "IDE token expired" storm). cached-token-usable? must now say unusable so
    ;; get-copilot-token! re-mints instead of looping.
    (let [usable?
          @#'sut/cached-token-usable?

          now
          (System/currentTimeMillis)]

      (expect (false? (usable?
                        {:token "t" :expires-at-ms (+ now 1500000) :refresh-at-ms (- now 1000)}
                        now)))
      ;; before refresh_in elapses the token is still served (no needless churn)
      (expect (true? (usable?
                       {:token "t" :expires-at-ms (+ now 1800000) :refresh-at-ms (+ now 1200000)}
                       now)))
      ;; legacy cache with no :refresh-at-ms falls back to expires - margin
      (expect (true? (usable? {:token "t" :expires-at-ms (+ now 1800000)} now)))))

(defdescribe copilot-base-url-test
             (it "derives API base URL from Copilot token proxy endpoint"
                 (expect (= "https://proxy.individual.githubcopilot.com"
                            (#'sut/copilot-base-url-from-token
                             "tid=x;proxy-ep=proxy.individual.githubcopilot.com;exp=1"))))
             (it "ignores token proxy endpoints for chat and uses the account API host"
                 (with-redefs-fn {#'sut/credential-account-type (constantly :business)}
                   (fn []
                     (expect (= "https://api.business.githubcopilot.com"
                                (#'sut/copilot-api-base-url
                                 "tid=x;exp=1"
                                 {:proxy-ep "proxy.business.githubcopilot.com"}
                                 nil))))))
             (it "falls back to the recorded seat host when the token carries no endpoint"
                 (with-redefs-fn {#'sut/credential-account-type (constantly :business)}
                   (fn []
                     (expect (= "https://api.business.githubcopilot.com"
                                (#'sut/copilot-api-base-url "tid=x;exp=1" {} nil))))))
             (it "falls back to individual Copilot API by default"
                 (with-redefs-fn {#'sut/credential-account-type (constantly :individual)}
                   (fn []
                     (expect (= "https://api.individual.githubcopilot.com"
                                (#'sut/copilot-api-base-url "tid=x;exp=1" {} nil))))))
             (it "uses business API fallback for Enterprise Cloud when no token endpoint is present"
                 (with-redefs-fn {#'sut/credential-account-type (constantly :enterprise)}
                   (fn []
                     (expect (= "https://api.business.githubcopilot.com"
                                (#'sut/copilot-api-base-url "tid=x;exp=1" {} nil))))))
             (it "uses GHE enterprise fallback when an enterprise domain is configured"
                 (expect (= "https://copilot-api.ghe.example.com"
                            (#'sut/copilot-api-base-url "tid=x;exp=1" {} "ghe.example.com"))))
             (it "ensure-api-version appends /v1 to a bare host and is idempotent"
                 (expect (= "https://api.business.githubcopilot.com/v1"
                            (#'sut/ensure-api-version "https://api.business.githubcopilot.com")))
                 (expect (= "https://api.business.githubcopilot.com/v1"
                            (#'sut/ensure-api-version "https://api.business.githubcopilot.com/v1")))
                 (expect (= "https://api.business.githubcopilot.com/v1"
                            (#'sut/ensure-api-version "https://api.business.githubcopilot.com/")))
                 (expect (nil? (#'sut/ensure-api-version nil)))))

(defdescribe copilot-limits-test
             (it "normalizes Copilot quota snapshots"
                 (with-redefs [sut/detect-oauth-token
                               (fn []
                                 {:oauth-token "ghu_test"})

                               sut/fetch-user-usage!
                               (fn [_]
                                 {:copilot_plan "business"
                                  :quota_reset_date "2026-05-30T00:00:00Z"
                                  :quota_snapshots
                                  {:premium_interactions
                                   {:remaining 240 :entitlement 300 :percent_remaining 80}}})]

                   (let [report
                         (#'sut/dynamic-limits!)

                         row
                         (first (get-in report [:dynamic :limits]))]

                     (expect (= :ok (:status report)))
                     (expect (= :premium_interactions (:id row)))
                     (expect (= 240.0 (:remaining row)))
                     (expect (= 300.0 (:limit row)))
                     (expect (= 60.0 (:used row)))))))

(defdescribe
  copilot-unlimited-and-overspent-quota-test
  (it "flags unlimited buckets instead of reporting their fake 100% remaining"
      ;; Verbatim shape returned by api.github.com/copilot_internal/user for a
      ;; token-based-billing account: `chat` is UNLIMITED with entitlement 0 and
      ;; percent_remaining 100.0, while the metered `premium_interactions` bucket
      ;; is overspent (remaining -32 of 1500, has_quota false). Rendering the
      ;; former verbatim told the user "100" on a bucket with no tank at all.
      (with-redefs [sut/detect-oauth-token
                    (fn []
                      {:oauth-token "ghu_test"})

                    sut/fetch-user-usage!
                    (fn [_]
                      {:copilot_plan "individual"
                       :quota_reset_date "2026-08-01"
                       :quota_snapshots {:chat {:unlimited true
                                                :entitlement 0
                                                :remaining 0
                                                :has_quota true
                                                :percent_remaining 100.0}
                                         :premium_interactions {:unlimited false
                                                                :entitlement 1500
                                                                :remaining -32
                                                                :has_quota false
                                                                :overage_permitted false
                                                                :percent_remaining 0.0}}})]

        (let [rows
              (get-in (#'sut/dynamic-limits!) [:dynamic :limits])

              chat
              (first (filter #(= :chat (:id %)) rows))

              premium
              (first (filter #(= :premium_interactions (:id %)) rows))]

          (expect (true? (:is-unlimited chat)))
          (expect (nil? (:remaining chat)))
          (expect (nil? (:limit chat)))
          (expect (nil? (:used chat)))
          (expect (= "unlimited (token-based billing)" (:note chat)))
          (expect (false? (:is-unlimited premium)))
          ;; clamped: never render "-32 left"
          (expect (= 0.0 (:remaining premium)))
          (expect (= 1500.0 (:limit premium)))
          (expect (= 1532.0 (:used premium)))
          (expect
            (= "0.0% remaining - quota exhausted (32 over); requests are rejected until it resets"
               (:note premium)))))))

(defdescribe
  copilot-refresh-margin-test
  (it "subtracts REFRESH_MARGIN_MS from the refresh_in soft deadline on mint (issue #21)"
      ;; GitHub's refresh_in (soft, proxy-reject) is shorter than expires_at (hard).
      ;; The mint must refresh a FULL margin BEFORE the soft deadline, never right
      ;; up to it — otherwise clock skew / round-trip lands us past the soft reject
      ;; ("IDE token expired") and the 401 recovery loop storms.
      (let [now
            (System/currentTimeMillis)

            refresh-in-s
            1500

            margin
            (* 5 60 1000)

            soft
            (+ now (* refresh-in-s 1000))]

        (with-redefs [sut/get-json
                      (fn [_ _]
                        {:token "tid=x;proxy-ep=proxy.individual.githubcopilot.com;exp=1"
                         :expires_at (long (/ (+ now 1800000) 1000))
                         :refresh_in refresh-in-s})

                      sut/copilot-llm-base-url
                      (fn [& _]
                        "https://api.individual.githubcopilot.com/v1")]

          (let [{:keys [refresh-at-ms expires-at-ms]} (#'sut/exchange-for-copilot-token!
                                                       "oauth-tok")]
            ;; refresh-at-ms == (min hard soft) - margin, and soft < hard here
            (expect (< (Math/abs (- (long refresh-at-ms) (- soft margin))) 2000))
            ;; always strictly before the hard expiry too
            (expect (< (long refresh-at-ms) (long expires-at-ms))))))))

(defdescribe
  copilot-seat-tier-test
  ;; Regression (user report, screenshot of Settings -> Providers): the picker
  ;; offered THREE GitHub Copilot entries, so the user had to know which seat had
  ;; been bought before signing in - and signing in to one tier lit up another,
  ;; because all three read the same OAuth token file. There is one provider now;
  ;; the seat tier is reported BY the account once a credential exists.
  (describe "status"
            (it "authenticates the single provider and reports the observed tier"
                (sut/register!)
                (with-redefs-fn {#'sut/detect-oauth-token (constantly {:oauth-token "ghu_test"
                                                                       :source :auth-file})
                                 #'sut/env-account-type (constantly nil)
                                 #'sut/auth-account-type (constantly :enterprise)}
                  (fn []
                    (let [status ((:provider/status-fn (vis/provider-by-id :github-copilot)))]
                      (expect (true? (:is-authenticated status)))
                      (expect (= :enterprise (:account-type status)))
                      (expect (= :auth-file (:source status))))))))
  (describe "limits"
            (it "reports the account quota without being told which tier it is"
                (sut/register!)
                (with-redefs-fn {#'sut/detect-oauth-token (constantly {:oauth-token "ghu_test"})
                                 #'sut/env-account-type (constantly nil)
                                 #'sut/auth-account-type (constantly :enterprise)
                                 #'sut/fetch-user-usage! (fn [_]
                                                           {:copilot_plan "enterprise"
                                                            :quota_snapshots {:premium_interactions
                                                                              {:remaining 240
                                                                               :entitlement 300}}})}
                  (fn []
                    (let [limits ((:provider/limits-fn (vis/provider-by-id :github-copilot)))]
                      (expect (= :ok (:status limits)))
                      (expect (= :github-copilot (:provider-id limits)))
                      (expect (= "Copilot plan: enterprise" (get-in limits [:dynamic :note]))))))))
  (describe
    "sign-in"
    (it "records the tier the ACCOUNT reports instead of asking for one"
        (sut/register!)
        (let [flows
              (atom [])

              remembered
              (atom nil)

              signed-in
              (atom false)]

          (with-redefs-fn {#'sut/detect-oauth-token
                           (fn []
                             (when @signed-in {:oauth-token "ghu_new" :source :auth-file}))
                           #'sut/env-account-type (constantly nil)
                           #'sut/auth-account-type (constantly nil)
                           #'sut/start-device-flow! (fn [opts]
                                                      (swap! flows conj opts)
                                                      {:user-code "ABCD-1234"
                                                       :verification-uri
                                                       "https://example.com/device"
                                                       :device-code "dev-code"
                                                       :interval 5
                                                       :expires-in 900})
                           #'sut/poll-for-token! (fn [& _]
                                                   (reset! signed-in true)
                                                   {:oauth-token "ghu_new"})
                           #'sut/fetch-user-usage! (fn [_]
                                                     {:copilot_plan "business"})
                           #'sut/remember-account-type! (fn [tier]
                                                          (reset! remembered tier))
                           #'sut/get-copilot-token! (fn [& _]
                                                      {:token "tid=x;exp=1"
                                                       :api-url
                                                       "https://api.business.githubcopilot.com/v1"})
                           #'sut/enable-known-copilot-models! (fn [_ _]
                                                                {:attempted 6 :enabled 6})}
            (fn []
              (let [auth! (:provider/auth-fn (vis/provider-by-id :github-copilot))]
                (expect (= :ok (auth! (constantly nil))))
                ;; one device flow, and nothing about a tier was passed into it
                (expect (= [nil] @flows))
                (expect (= :business @remembered)))))))
    (it "short-circuits when the machine already holds the credential"
        (sut/register!)
        (let [flows (atom [])]
          (with-redefs-fn {#'sut/detect-oauth-token (constantly {:oauth-token "ghu_test"
                                                                 :source :auth-file})
                           #'sut/env-account-type (constantly nil)
                           #'sut/auth-account-type (constantly :individual)
                           #'sut/start-device-flow! (fn [opts]
                                                      (swap! flows conj opts)
                                                      {})}
            (fn []
              (let [auth! (:provider/auth-fn (vis/provider-by-id :github-copilot))]
                (expect (= :already-authenticated (auth! (constantly nil))))
                (expect (= [] @flows)))))))))
