(ns com.blockether.vis.internal.provider.vendor.antigravity-test
  (:require [babashka.http-client :as http]
            [charred.api :as json]
            [com.blockether.vis.internal.provider.vendor.antigravity :as antigravity]
            [lazytest.core :refer [defdescribe expect it]]))

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
