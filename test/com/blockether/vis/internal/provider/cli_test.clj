(ns com.blockether.vis.internal.provider.cli-test
  (:require [clojure.string :as str]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.internal.commandline :as commandline]
            [com.blockether.vis.internal.config.core :as config]
            [com.blockether.vis.internal.extension.registry :as registry]
            [com.blockether.vis.internal.provider.cli :as provider-cli]
            [lazytest.core :refer [defdescribe expect it]]))

;; Regression: the CLI answered `Authenticated:  yes` from `is_authenticated`
;; alone, so a key that was merely SAVED read as proven -- while the dialog
;; beside it already said "saved, not verified".
(defdescribe
  cli-provider-status-vocabulary-test
  (it "speaks the daemon's verdict and hides the rows that line covers"
      (let [lines (atom [])]
        (with-redefs [commandline/stdout! (fn [s]
                                            (swap! lines conj s))
                      provider-cli/configured-provider-status (constantly {"is_authenticated" true
                                                                           "auth_state" "unverified"
                                                                           "is_loading" false
                                                                           "plan_name" "pro"})
                      provider-cli/configured-provider-base-url (constantly nil)
                      provider-cli/provider-limit-lines (constantly [])]

          (#'provider-cli/print-provider-status! {:provider/id :acme :provider/label "Acme"}))
        (let [text (str/join "\n" @lines)]
          (expect (str/includes? text "Authenticated:  saved, not verified"))
          (expect (not (str/includes? text "Authenticated:  yes")))
          (expect (str/includes? text "pro"))
          (expect (not (str/includes? text "Auth state")))
          (expect (not (str/includes? text "Is loading")))))))

(defn- run-status
  "Run `providers status` against one fake provider. Returns its output lines and
   exit status."
  [args status]
  (let [lines
        (atom [])

        code
        (atom nil)

        acme
        {:provider/id :acme :provider/label "Acme"}]

    (with-redefs [config/init-cli!
                  (constantly nil)

                  registry/provider-by-id
                  (fn [id]
                    (when (= :acme id) acme))

                  registry/registered-providers
                  (constantly [acme])

                  commandline/stdout!
                  (fn [s]
                    (swap! lines conj s))

                  provider-cli/configured-provider-status
                  (constantly status)

                  provider-cli/configured-provider-base-url
                  (constantly nil)

                  provider-cli/provider-limit-lines
                  (constantly [])

                  provider-cli/finish!
                  (fn [c]
                    (reset! code c))]

      (commandline/dispatch! (some #(when (= "status" (:cmd/name %)) %) provider-cli/subcommands)
                             (into ["status"] args)
                             {:print-fn nil}))
    {:lines @lines :code @code}))

;; Regression for #342: scripts had to grep the human table to learn whether a
;; provider was signed in, because `--json` was ignored and the exit status was
;; always 0.
(defdescribe
  cli-provider-status-for-scripts-test
  (it "prints the auth state as JSON without tokens or token previews"
      (let [{:keys [lines code]} (run-status ["acme" "--json"]
                                             {"is_authenticated" true
                                              "auth_state" "verified"
                                              "source" "auth-file"
                                              "account_type" "business"
                                              "oauth_token_preview" "gho_abcd..."})]
        (expect (= [{"provider" "acme"
                     "label" "Acme"
                     "authenticated" true
                     "state" "verified"
                     "source" "auth-file"
                     "account_type" "business"}]
                   (mapv wire/parse-json lines)))
        (expect (not (str/includes? (first lines) "gho_")))
        (expect (= 0 code))))
  (it "exits 3 when the provider is not signed in or rejected the credential"
      (expect (= 3 (:code (run-status ["acme" "--quiet"] {"is_authenticated" false}))))
      (expect (= 3
                 (:code (run-status ["acme" "--quiet"]
                                    {"is_authenticated" true "auth_state" "rejected"})))))
  (it "exits 4 when a saved credential is not verified"
      (let [{:keys [lines code]} (run-status ["acme" "--quiet"]
                                             {"is_authenticated" true "auth_state" "unverified"})]
        (expect (= [] lines))
        (expect (= 4 code))))
  (it "keeps the human table and gives it the same exit status"
      (let [{:keys [lines code]} (run-status ["acme"] {"is_authenticated" false})]
        (expect (some #(str/includes? % "Authenticated:  not verified") lines))
        (expect (= 3 code))))
  (it "exits 2 for an unknown provider"
      (let [{:keys [lines code]} (run-status ["nope" "--json"] {})]
        (expect (= [{"provider" "nope" "error" "unknown provider"}] (mapv wire/parse-json lines)))
        (expect (= 2 code))))
  (it "prints an array and exits 0 without a provider name"
      (let [{:keys [lines code]} (run-status ["--json"] {"is_authenticated" false})]
        (expect (= ["acme"] (mapv #(get % "provider") (wire/parse-json (first lines)))))
        (expect (= 0 code)))))
