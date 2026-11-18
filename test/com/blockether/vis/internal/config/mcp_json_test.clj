(ns com.blockether.vis.internal.config.mcp-json-test
  (:require [com.blockether.vis.internal.config.mcp-json :as mcp-json]
            [com.blockether.vis.internal.config.validation :as config-validation]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe oauth-mapping-test
  (it "maps oauth.clientId and oauth.callbackPort of Claude Code to auth, and back"
      (let [entry {"type" "http"
                   "url" "https://mcp.example.com/mcp"
                   "oauth" {"clientId" "vis-client" "callbackPort" 8080}}
            spec (mcp-json/->spec entry)]
        (expect (= {"client_id" "vis-client" "callback_port" 8080} (get spec "auth")))
        (expect (config-validation/valid? {"mcp" {"servers" {"remote" spec}}}))
        (expect (= entry (mcp-json/->entry spec)))))
  (it "keeps a callback port without a client id"
      (let [spec (mcp-json/->spec {"url" "https://mcp.example.com/mcp" "oauth" {"callbackPort" 9000}})]
        (expect (= {"callback_port" 9000} (get spec "auth")))
        (expect (= {"callbackPort" 9000} (get (mcp-json/->entry spec) "oauth"))))))
