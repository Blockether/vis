(ns com.blockether.vis.tui.client-test
  (:require [babashka.http-client :as http]
            [clojure.java.io :as io]
            [com.blockether.vis.tui.client :as client]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is testing]]))

(deftest default-gateway-infers-local-bearer-token
  ;; Regression: bare `vis-agent tui` must authenticate to a local --pair gateway.
  (let [token-file
        (java.io.File/createTempFile "vis-tui-token-" ".txt")

        target-entry
        #'client/target-entry

        requests
        (atom [])]

    (try (spit token-file "local-test-token\n")
         (with-redefs [io/file
                       (fn [& _]
                         token-file)

                       http/request
                       (fn [request]
                         (swap! requests conj request)
                         {:status 200 :body "{}"})]

           (let [entry (target-entry nil nil)]
             (is (= "http://127.0.0.1:7890" (:base-url entry)))
             (is (= "local-test-token" (:secret entry)))
             (#'client/gw-send! entry "POST" "/v1/clients" {:body {:kind "tui"}})
             (is (= "Bearer local-test-token"
                    (get-in (first @requests) [:headers "Authorization"]))))
           (testing "explicit credentials win"
             (is (= "provided" (:secret (target-entry nil " provided "))))))
         (finally (.delete token-file)))))

(deftest explicit-gateways-never-read-local-credentials
  (with-redefs [io/file (fn [& _]
                          (throw (ex-info "must not read local token" {})))]
    (doseq [url ["gateway.example.com" "127.0.0.1:7899" "http://127.0.0.1:7890"]]
      (is (nil? (:secret (#'client/target-entry url nil)))))
    (is (= "provided" (:secret (#'client/target-entry nil "provided"))))))

(deftest absent-or-empty-local-token-stays-tokenless
  (let [token-file (java.io.File/createTempFile "vis-tui-token-" ".txt")]
    (try (with-redefs [io/file (fn [& _]
                                 token-file)]
           (spit token-file " \n")
           (is (nil? (:secret (#'client/target-entry nil nil))))
           (.delete token-file)
           (is (nil? (:secret (#'client/target-entry nil nil)))))
         (finally (.delete token-file)))))
