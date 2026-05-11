(ns com.blockether.vis.ext.exa.core-test
  "Tests for the Exa MCP extension: extension surface, env/config
   resolution, JSON-RPC request shape, bb-http seam, and tool-result
   envelope behavior. No live Exa calls run in the default suite."
  (:require
   [charred.api :as json]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [com.blockether.vis.core :as vis]
   [com.blockether.vis.ext.exa.core :as exa]
   [com.blockether.vis.internal.extension :as extension]
   [lazytest.core :refer [defdescribe expect it]]))

(defn- json-rpc-response [id result]
  {:status 200
   :headers {"content-type" "application/json"}
   :body (json/write-json-str {:jsonrpc "2.0"
                               :id id
                               :result result})})

(defn- exa-manifest-file []
  (let [repo-root-file (io/file "extensions/common/vis-exa/resources/META-INF/vis-extension/vis.edn")]
    (if (.exists repo-root-file)
      repo-root-file
      (io/file "resources/META-INF/vis-extension/vis.edn"))))

(defn- fake-mcp-send [seen]
  (fn [{:keys [body] :as req}]
    (swap! seen conj req)
    (let [payload (json/read-json body :key-fn keyword)
          method  (:method payload)
          id      (:id payload)]
      (case method
        "initialize"
        (json-rpc-response id {:serverInfo {:name "fake-exa"}})

        "notifications/initialized"
        {:status 204 :headers {} :body ""}

        "tools/call"
        (json-rpc-response id {:content [{:type "text"
                                          :text "Result 1\nResult 2"}]
                               :isError false})

        {:status 500 :headers {"content-type" "text/plain"} :body method}))))

(defdescribe exa-extension-loads-test
  (it "ships a parseable vis.edn manifest"
    (let [manifest (edn/read-string {:readers {} :default (fn [_ form] form)}
                     (slurp (exa-manifest-file)))
          readme   (get-in manifest ['exa :docs "README.md" :content])]
      (expect (= '[com.blockether.vis.ext.exa.core] (get-in manifest ['exa :nses])))
      (expect (str/includes? readme "\"url\""))
      (expect (str/includes? readme ":op/result"))
      (expect (not (str/includes? readme "[:result :content]")))))

  (it "exposes Exa symbols, renderers, and a compact prompt"
    (expect (= '[web-search code-context]
              (mapv :ext.symbol/sym exa/exa-symbols)))
    (expect (every? :ext.symbol/journal-render-fn exa/exa-symbols))
    (expect (every? :ext.symbol/channel-render-fn exa/exa-symbols))
    (expect (str/includes? exa/exa-prompt "EXA_API_KEY"))
    (expect (str/includes? exa/exa-prompt ":max-bytes"))
    (expect (not (str/includes? exa/exa-prompt (str ":" (apply str [\p \i]) "-max"))))
    (expect (str/includes? exa/exa-prompt "get-in r [:op/result :content]")))

  (it "exports a valid Vis extension"
    (expect (= 'com.blockether.vis.ext.exa.core (:ext/namespace exa/vis-extension)))
    (expect (= {:ns 'vis.ext.exa :alias 'exa} (:ext/ns-alias exa/vis-extension)))
    (expect (= "EXA_API_KEY" (-> exa/vis-extension :ext/env first :name)))
    (expect (= 2 (count (:ext/symbols exa/vis-extension))))))

(defdescribe exa-config-test
  (it "resolves env through Vis config-backed extension overrides"
    (with-redefs-fn {#'vis/extension-env-value (fn [k]
                                                 (get {"EXA_API_KEY" "configured-key"} k))
                     #'exa/config-from-file (constantly nil)}
      #(expect (= "configured-key" (:api-key (exa/effective-config))))))

  (it "merges config file, env overrides, and env-token values"
    (with-redefs-fn {#'exa/config-from-file
                     (fn [] {:url "env:EXA_URL"
                             :apiKey "$EXA_KEY"
                             :tools "$EXA_TOOLS"
                             :timeoutMs "${EXA_TIMEOUT}"
                             :protocolVersion "2025-06-18"
                             :maxBytes "9000"
                             :maxLines "90"})
                     #'exa/env
                     (fn [k]
                       (get {"EXA_URL" "https://example.test/mcp"
                             "EXA_KEY" "secret"
                             "EXA_TOOLS" "web_search_exa,get_code_context_exa"
                             "EXA_TIMEOUT" "1234"}
                         k))}
      #(let [cfg (exa/effective-config)]
         (expect (= "https://example.test/mcp" (:url cfg)))
         (expect (= "secret" (:api-key cfg)))
         (expect (= ["web_search_exa" "get_code_context_exa"] (:tools cfg)))
         (expect (= 1234 (:timeout-ms cfg)))
         (expect (= 9000 (:max-bytes cfg)))
         (expect (= 90 (:max-lines cfg))))))

  (it "redacts exaApiKey in displayed endpoints"
    (expect (= "https://mcp.exa.ai/mcp?tools=web_search_exa&exaApiKey=REDACTED"
              (exa/redact-endpoint
                "https://mcp.exa.ai/mcp?tools=web_search_exa&exaApiKey=secret")))))

(defdescribe exa-json-rpc-tool-test
  (it "calls initialize, initialized notification, and tools/call through babashka-http request maps"
    (let [seen (atom [])]
      (with-redefs-fn {#'exa/config-from-file (constantly nil)
                       #'exa/env (constantly nil)
                       #'exa/*http-send-fn* (fake-mcp-send seen)}
        #(let [out (exa/web-search "latest Clojure" {:num-results 2
                                                     :max-bytes 1000
                                                     :max-lines 20})
               requests @seen
               payloads (mapv (fn [request]
                                (json/read-json (:body request) :key-fn keyword))
                          requests)]
           (expect (extension/tool-result? out))
           (expect (true? (:op/success? out)))
           (expect (= "Result 1\nResult 2" (get-in out [:op/result :content])))
           (expect (= ["initialize" "notifications/initialized" "tools/call"]
                     (mapv :method payloads)))
           (expect (= "web_search_exa" (get-in (nth payloads 2) [:params :name])))
           (expect (= {:query "latest Clojure" :numResults 2}
                     (get-in (nth payloads 2) [:params :arguments])))
           (expect (every? (fn [request] (= false (:throw request))) requests))
           (expect (every? (fn [request]
                             (= "application/json" (get-in request [:headers "content-type"])))
                     requests))))))

  (it "returns a valid failure envelope when MCP returns an HTTP error"
    (with-redefs-fn {#'exa/config-from-file (constantly nil)
                     #'exa/env (constantly nil)
                     #'exa/*http-send-fn* (fn [_] {:status 500
                                                   :headers {"content-type" "text/plain"}
                                                   :body "boom"})}
      #(let [out (exa/code-context "HoneySQL examples")]
         (expect (extension/tool-result? out))
         (expect (false? (:op/success? out)))
         (expect (str/includes? (get-in out [:op/error :message]) "MCP HTTP 500"))))))

(defdescribe exa-truncation-test
  (it "truncates large output and records truncation metadata"
    (let [trunc (exa/truncate-text "a\nb\nc" {:max-lines 2 :max-bytes 100})]
      (expect (= "a\nb" (:content trunc)))
      (expect (true? (:truncated? trunc)))
      (expect (= :lines (:truncated-by trunc)))))

  (it "uses Vis-native output-bound option names"
    (let [seen (atom [])]
      (with-redefs-fn {#'exa/config-from-file (constantly nil)
                       #'exa/env (constantly nil)
                       #'exa/*http-send-fn* (fake-mcp-send seen)}
        #(let [out (exa/web-search "latest Clojure" {:max-lines 1})]
           (expect (= "Result 1" (get-in out [:op/result :content])))
           (expect (= :lines (get-in out [:op/result :truncation :truncated-by]))))))))
