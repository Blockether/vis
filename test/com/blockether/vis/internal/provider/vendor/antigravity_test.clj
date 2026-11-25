(ns com.blockether.vis.internal.provider.vendor.antigravity-test
  (:require [babashka.http-client :as http]
            [charred.api :as json]
            [clojure.java.io :as io]
            [com.blockether.svar.core :as svar]
            [com.blockether.vis.contract.provider :as contract-provider]
            [com.blockether.vis.internal.extension.registry :as registry]
            [com.blockether.vis.internal.provider.limits :as limits]
            [com.blockether.vis.internal.provider.vendor.antigravity :as antigravity]
            [lazytest.core :refer [defdescribe expect it throws?]])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]
           [java.time Instant]))

(defdescribe refresh-test
             (it "keeps the stored refresh token, because a Google refresh response has none"
                 (with-redefs-fn {#'antigravity/load-auth-file (constantly {:refresh-token "ref-1"
                                                                            :email "a@example.com"})
                                  #'antigravity/client-secret (constantly "secret")
                                  #'http/post (fn [_ _]
                                                {:status 200
                                                 :body (json/write-json-str {:access_token "tok-2"
                                                                             :expires_in 3599})})
                                  #'http/get (fn [_ _]
                                               {:status 500 :body ""})}
                   (fn []
                     (let [creds (#'antigravity/refresh-access-token! "ref-1")]
                       (expect (= ["tok-2" "ref-1" "a@example.com"]
                                  ((juxt :access-token :refresh-token :email) creds))))))))

(defn- model-with-quota
  [id remaining reset]
  {:id id "quotaInfo" {"remainingFraction" remaining "resetTime" reset}})

(defn- antigravity-limits
  [get-token! get-models!]
  (limits/flush-limits-cache! :antigravity)
  (try (with-redefs [antigravity/get-antigravity-token! get-token!
                     svar/models! get-models!
                     registry/provider-by-id
                     (constantly {:provider/limits-fn #'antigravity/fetch-limits-report})]
         (limits/provider-limits :antigravity))
       (finally (limits/flush-limits-cache! :antigravity))))

(defdescribe quota-test
  (it "reports each quota pool once and shows when an exhausted pool resets"
      (let [reset "2026-12-02T09:12:53Z"
            models [(model-with-quota "gemini-3-flash" 0.965 reset)
                    (model-with-quota "gemini-4-hydrogen-high" 0.965 reset)
                    (model-with-quota "claude-opus-4-6-thinking" 0.0 reset)
                    (model-with-quota "gpt-oss-120b-medium" 0.965 reset)
                    (model-with-quota "gemini-3-flash-disabled" 0.0 reset)
                    (assoc (model-with-quota "gemini-unavailable" 0.0 reset) "disabled" true)
                    (model-with-quota "tab_completion" 0.0 reset)]
            report (antigravity-limits (constantly {:token "test-token" :api-url "https://test"})
                                       (constantly models))
            rows (get-in report [:dynamic :limits])
            exhausted (first rows)]
        (expect (contract-provider/report-valid? report))
        (expect (= :ok (:status report)))
        (expect (= [["antigravity-claude" 0.0] ["antigravity-gemini" 96.5]
                    ["antigravity-gpt-oss" 96.5]]
                   (mapv (juxt :id :remaining) rows)))
        (expect (= 100.0 (:limit exhausted)))
        (expect (= "Claude quota is exhausted." (:note exhausted)))
        (expect (= (.toEpochMilli (Instant/parse reset))
                   (get-in exhausted [:window :resets-at-ms])))))
  (it "reports missing credentials without calling the model endpoint"
      (let [report (antigravity-limits (fn [] (throw (ex-info "Sign in first"
                                                              {:type :vis/antigravity-not-authenticated})))
                                       (fn [& _] (throw (AssertionError. "Unexpected model fetch"))))]
        (expect (= :unauthenticated (:status report)))))
  (it "reports a failed fetch with its HTTP status and without the response body"
      (let [report (antigravity-limits (constantly {:token "test-token" :api-url "https://test"})
                                       (fn [& _] (throw (ex-info "Raw upstream body"
                                                                  {:status 503 :body "raw"}))))]
        (expect (= :error (:status report)))
        (expect (= {:status 503} (get-in report [:error :data])))
        (expect (= "Antigravity quota check failed: HTTP 503." (get-in report [:error :message]))))))

(defdescribe logout-test
  (it "fails when credentials cannot be deleted and succeeds if they are absent"
      (let [dir (.toFile (Files/createTempDirectory "antigravity-logout"
                                                    (make-array FileAttribute 0)))
            file (io/file dir "keep")]
        (try (spit file "fixture")
             (with-redefs-fn {#'antigravity/auth-file (constantly (.getPath dir))}
               #(expect (throws? clojure.lang.ExceptionInfo antigravity/logout!)))
             (with-redefs-fn {#'antigravity/auth-file (constantly (.getPath (io/file dir "missing")))}
               #(expect (= :logged-out (antigravity/logout!))))
             (finally (Files/deleteIfExists (.toPath file))
                      (Files/deleteIfExists (.toPath dir)))))))
