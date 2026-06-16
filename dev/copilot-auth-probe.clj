;; Authenticate GitHub Copilot via device flow, then probe whether /responses
;; and /chat/completions are also served under /v1 (like /v1/messages).
(require '[com.blockether.vis.ext.provider-github-copilot :as cp]
         '[babashka.http-client :as http])

(let [{:keys [user-code verification-uri device-code interval expires-in]} (cp/start-device-flow!)]
  (println "\n================ COPILOT LOGIN ================")
  (println "  1. Open:" verification-uri)
  (println "  2. Enter code:" user-code)
  (println "  (waiting up to" (quot expires-in 60) "min for you to authorize…)")
  (println "==============================================\n")
  (flush)
  (cp/poll-for-token! device-code interval expires-in nil)   ; blocks until authorized; writes ~/.vis auth file
  (println "\n✓ Authorized. Probing endpoints…\n")
  (let [{:keys [token api-url]} (cp/get-copilot-token!)
        hdr   {"Authorization"          (str "Bearer " token)
               "Content-Type"           "application/json"
               "Editor-Version"         "vscode/1.99.0"
               "Editor-Plugin-Version"  "copilot-chat/0.26.7"
               "Copilot-Integration-Id" "vscode-chat"
               "User-Agent"             "GitHubCopilotChat/0.26.7"}
        probe (fn [path body]
                (let [r (http/post (str api-url path)
                          {:headers hdr :body body :throw false :timeout 20000})]
                  (println (format "  %-22s -> %-4s %s" path (:status r)
                             (subs (str (:body r)) 0 (min 90 (count (str (:body r)))))))))]
    (println "api-url:" api-url)
    (println "--- 404 = path missing; else = path exists ---")
    (probe "/v1/messages"         "{\"model\":\"claude-sonnet-4.6\",\"max_tokens\":1,\"messages\":[{\"role\":\"user\",\"content\":\"hi\"}]}")
    (probe "/responses"           "{\"model\":\"gpt-5.4\",\"input\":\"hi\"}")
    (probe "/v1/responses"        "{\"model\":\"gpt-5.4\",\"input\":\"hi\"}")
    (probe "/chat/completions"    "{\"model\":\"gpt-5.4\",\"messages\":[{\"role\":\"user\",\"content\":\"hi\"}],\"max_tokens\":1}")
    (probe "/v1/chat/completions" "{\"model\":\"gpt-5.4\",\"messages\":[{\"role\":\"user\",\"content\":\"hi\"}],\"max_tokens\":1}")
    (println "\n================ DONE ================")))
