(ns com.blockether.vis.internal.foundation.mcp.oauth-test
  (:require [com.blockether.vis.internal.foundation.mcp.oauth :as oauth]
            [lazytest.core :refer [defdescribe expect it]]))

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
