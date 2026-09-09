(ns com.blockether.vis.internal.gateway.extension-startup-test
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.gateway.client :as client]
            [com.blockether.vis.internal.gateway.server :as server]
            [com.blockether.vis.internal.python.extensions :as pyx]
            [com.blockether.vis.internal.python.runtime :as runtime]
            [lazytest.core :refer [around-each set-ns-context!]]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is]]))

(set-ns-context!
  [(around-each [run] (with-redefs-fn {#'server/extension-startup (atom {:stage "idle"})} run))])

(deftest startup-prepares-before-announcing-completion
  (let [stages (atom [])]
    (with-redefs [pyx/ensure-python-extensions-loaded! (fn []
                                                         (swap! stages conj :prepared)
                                                         {:loaded 2 :failed 1})]
      (#'server/prepare-startup-extensions!)
      (is (= [:prepared] @stages))
      (is (= {:stage "ready" :loaded 2 :failed 1} @@#'server/extension-startup)))))

(deftest startup-failure-is-safe-and-does-not-stop-the-server
  (with-redefs [pyx/ensure-python-extensions-loaded! (fn []
                                                       (throw (ex-info "private installer detail"
                                                                       {})))]
    (#'server/prepare-startup-extensions!)
    (is (= {:stage "failed"} @@#'server/extension-startup))))

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
