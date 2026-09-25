(ns com.blockether.vis.internal.provider.cli-test
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.commandline :as commandline]
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
