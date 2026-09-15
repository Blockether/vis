(ns com.blockether.vis.internal.gateway.extension-startup-test
  (:require [clojure.string :as str]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.internal.gateway.client :as client]
            [com.blockether.vis.internal.gateway.server :as server]
            [com.blockether.vis.internal.python.extensions :as pyx]
            [com.blockether.vis.internal.python.runtime :as runtime]
            [lazytest.core :refer [around-each set-ns-context!]]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is]]))

(set-ns-context! [(around-each [run]
                               (with-redefs-fn {#'server/extension-startup (atom {:stage "idle"})
                                                #'pyx/load-failures (constantly [])
                                                #'pyx/loaded-python-extensions (constantly {})
                                                #'runtime/preparation-status (constantly [])}
                                 run))])

(deftest startup-prepares-before-announcing-completion
  (let [stages (atom [])]
    (with-redefs [pyx/ensure-python-extensions-loaded! (fn []
                                                         (swap! stages conj :prepared)
                                                         {:loaded 2 :failed 1})]
      (#'server/prepare-startup-extensions!)
      (is (= [:prepared] @stages))
      (is (= {:stage "ready"} @@#'server/extension-startup)))))

(deftest startup-failure-is-safe-and-does-not-stop-the-server
  (with-redefs [pyx/ensure-python-extensions-loaded!
                (fn []
                  (throw (ex-info "Extension discovery failed: api_key=fixture-secret" {})))]
    (#'server/prepare-startup-extensions!)
    (is (= {:stage "failed" :error "Extension discovery failed: api_key=[REDACTED]"}
           @@#'server/extension-startup))))

(deftest startup-failure-without-a-message-still-identifies-the-error
  (with-redefs [pyx/ensure-python-extensions-loaded! (fn []
                                                       (throw (IllegalStateException.)))]
    (#'server/prepare-startup-extensions!)
    (is (= {:stage "failed" :error "java.lang.IllegalStateException"}
           @@#'server/extension-startup))))

(deftest client-reports-package-preparation-and-completion
  (let [polls
        (atom 0)

        output
        (java.io.StringWriter.)]

    (with-redefs-fn {#'client/send-json-with-entry! (fn [& _]
                                                      (swap! polls inc)
                                                      {"extensions"
                                                       {"stage" "ready" "loaded" 1 "failed" 0}})}
      (fn []
        (binding [*err* output]
          (#'client/await-extension-startup!
           {}
           {"extensions" {"stage" "initializing"
                          "packages" [{"name" "greeter" "stage" "installing"}]}}))
        (is (= 1 @polls))
        (is (str/includes? (str output) "greeter: installing"))
        (is (str/includes? (str output) "1 loaded"))))))

(deftest admin-status-has-credential-free-preparation
  (with-redefs [runtime/preparation-status (constantly [{:name "greeter" :stage "ready"}])]
    (is (= [{:name "greeter" :stage "ready"}] (:packages (#'server/extension-startup-status))))))

(deftest client-reports-each-extension-failure-from-cold-and-ready-gateways
  ;; Regression: the terminal printed only the loaded/failed counts, losing the cause.
  (let [failures [{:file "/extensions/missing.py"
                   :error "ModuleNotFoundError: No module named 'missing_dependency'"}
                  {:file "/extensions/greeter.py"
                   :extension "greeter"
                   :stale? true
                   :error (str "Traceback (most recent call last):\n"
                               "  File \"/extensions/greeter.py\", line 3\n"
                               "RuntimeError: api_key=fixture-secret")
                   :requested-fingerprint "internal-fingerprint"
                   :context "internal-context"}]]
    (with-redefs [pyx/ensure-python-extensions-loaded! (constantly {:loaded 1 :failed 2})
                  pyx/loaded-python-extensions (constantly {"/extensions/greeter.py" {}})
                  pyx/load-failures (constantly failures)]

      (#'server/prepare-startup-extensions!)
      (let [status (#'server/extension-startup-status)
            response (wire/->wire {:extensions status})
            encoded (wire/json-str response)]

        (is (= {:stage "ready" :loaded 1 :failed 2} (select-keys status [:stage :loaded :failed])))
        (is (= ["/extensions/missing.py" "/extensions/greeter.py"] (mapv :file (:failures status))))
        (is (= [false true] (mapv :stale (:failures status))))
        (doseq [hidden ["fixture-secret" "internal-fingerprint" "internal-context"]]
          (is (not (str/includes? encoded hidden))))
        (doseq [cold? [true false]]
          (let [polls (atom 0)
                output (java.io.StringWriter.)
                stdout (java.io.StringWriter.)]

            (with-redefs-fn {#'client/send-json-with-entry! (fn [& _]
                                                              (swap! polls inc)
                                                              response)}
              #(binding [*out* stdout *err* output] (#'client/await-extension-startup!
                                                     {}
                                                     (if cold?
                                                       {"extensions" {"stage" "initializing"}}
                                                       response))))
            (is (= (if cold? 1 0) @polls))
            (is (= "" (str stdout)))
            (is (= cold? (str/includes? (str output) "preparing gateway extensions")))
            (doseq
              [detail
               ["1 loaded, 2 failed" "/extensions/missing.py: not loaded"
                "ModuleNotFoundError: No module named 'missing_dependency'"
                "/extensions/greeter.py (greeter): reload failed; using last-known-good version"
                "  Traceback (most recent call last):\n"
                "    File \"/extensions/greeter.py\", line 3\n"
                "  RuntimeError: api_key=[REDACTED]"]]
              (is (str/includes? (str output) detail)))
            (is (not (str/includes? (str output) "fixture-secret")))))))))

(deftest client-reports-fatal-preparation-errors-without-polling-a-finished-gateway
  (with-redefs [pyx/ensure-python-extensions-loaded!
                (fn []
                  (throw (ex-info "Cannot scan extensions: Permission denied" {})))]
    (#'server/prepare-startup-extensions!)
    (doseq [cold? [true false]]
      (let [output (java.io.StringWriter.)
            polls (atom 0)
            response (wire/->wire {:extensions (#'server/extension-startup-status)})]

        (with-redefs-fn {#'client/send-json-with-entry! (fn [& _]
                                                          (swap! polls inc)
                                                          response)}
          #(binding [*err* output] (#'client/await-extension-startup!
                                    {}
                                    (if cold? {"extensions" {"stage" "initializing"}} response))))
        (is (= (if cold? 1 0) @polls))
        (is (str/includes? (str output) "preparation failed"))
        (is (str/includes? (str output) "Cannot scan extensions: Permission denied"))))))

(deftest client-keeps-healthy-ready-gateways-quiet
  (let [output
        (java.io.StringWriter.)

        polls
        (atom 0)]

    (with-redefs-fn {#'client/send-json-with-entry! (fn [& _]
                                                      (swap! polls inc))}
      #(binding [*err* output] (#'client/await-extension-startup!
                                {}
                                {"extensions"
                                 {"stage" "ready" "loaded" 2 "failed" 0 "failures" []}})))
    (is (= 0 @polls))
    (is (= "" (str output)))))

(deftest extension-status-follows-the-latest-scan
  (let [failures
        (atom [{:file "/extensions/greeter.py" :error "NameError: missing_name"}])

        loaded
        (atom {})]

    (with-redefs [pyx/ensure-python-extensions-loaded!
                  (constantly {:loaded 0 :failed 1})

                  pyx/load-failures
                  #(deref failures)

                  pyx/loaded-python-extensions
                  #(deref loaded)]

      (#'server/prepare-startup-extensions!)
      (is (= 1 (:failed (#'server/extension-startup-status))))
      (reset! failures [])
      (reset! loaded {"/extensions/greeter.py" {}})
      (is (= {:loaded 1 :failed 0 :failures []}
             (select-keys (#'server/extension-startup-status) [:loaded :failed :failures]))))))

(deftest extension-diagnostics-redact-credentials-and-terminal-control-codes
  (let [error (str "\u001b[31mCannot load extension\u001b[0m\r\n"
                   "https://user:fixture-password@gateway.example.com/simple\n"
                   "Authorization: Bearer fixture-token\n"
                   "api_key=fixture-key\n"
                   "-----BEGIN PRIVATE KEY-----\nfixture-private-key\n-----END PRIVATE KEY-----")]
    (with-redefs [pyx/ensure-python-extensions-loaded! (constantly {:loaded 0 :failed 1})
                  pyx/load-failures (constantly [{:file "/extensions/greeter.py" :error error}])]

      (#'server/prepare-startup-extensions!)
      (let [encoded (wire/json-str (#'server/extension-startup-status))]
        (is (str/includes? encoded "Cannot load extension"))
        (is (str/includes? encoded "[REDACTED]"))
        (doseq [hidden ["fixture-password" "fixture-token" "fixture-key" "fixture-private-key"
                        "\\u001b" "\\r"]]
          (is (not (str/includes? encoded hidden))))))))
