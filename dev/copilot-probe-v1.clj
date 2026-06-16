;; Live probe: does GitHub Copilot serve /responses + /chat/completions under
;; /v1 (like /v1/messages), or only at the root? Decides whether the 3 per-wire
;; Copilot sub-providers can collapse into ONE provider (base-url = …/v1).
;;
;; Run AFTER authenticating Copilot (`vis providers auth github-copilot-individual`),
;; or with COPILOT_GITHUB_TOKEN / GH_TOKEN set to a token that has Copilot:
;;
;;   clojure -Sdeps '{:deps {com.blockether/vis-provider-github-copilot
;;                            {:local/root "extensions/providers/vis-provider-github-copilot"}}}' \
;;           -M dev/copilot-probe-v1.clj
;;
;; Read the status codes: 404 = path does NOT exist; 200/400/401/422 = path EXISTS.
;; If /v1/responses AND /v1/chat/completions are non-404 → collapse to ONE provider.
(require '[com.blockether.vis.ext.provider-github-copilot :as cp]
         '[babashka.http-client :as http])

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
  (println "--- 404 = path missing; anything else = path exists ---")
  (probe "/v1/messages"        "{\"model\":\"claude-sonnet-4.6\",\"max_tokens\":1,\"messages\":[{\"role\":\"user\",\"content\":\"hi\"}]}")
  (probe "/responses"          "{\"model\":\"gpt-5.4\",\"input\":\"hi\"}")
  (probe "/v1/responses"       "{\"model\":\"gpt-5.4\",\"input\":\"hi\"}")
  (probe "/chat/completions"   "{\"model\":\"gpt-5.4\",\"messages\":[{\"role\":\"user\",\"content\":\"hi\"}],\"max_tokens\":1}")
  (probe "/v1/chat/completions" "{\"model\":\"gpt-5.4\",\"messages\":[{\"role\":\"user\",\"content\":\"hi\"}],\"max_tokens\":1}"))
