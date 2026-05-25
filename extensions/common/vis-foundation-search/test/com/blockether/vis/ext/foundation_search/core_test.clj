(ns com.blockether.vis.ext.foundation-search.core-test
  "`search/*` extension tests. HTTP layer is mocked so the suite never
   touches the network. Tests confirm:
     - papers parses arxiv Atom into citation-shaped maps
     - web / code go through the Exa MCP path and return tool-result envelopes
     - all three symbols carry `:engine-scope #{:consult}`"
  (:require
   [babashka.http-client :as http]
   [clojure.string :as str]
   [com.blockether.vis.ext.foundation-search.core :as search]
   [lazytest.core :refer [defdescribe describe expect it]]))

;; ---------------------------------------------------------------------------
;; arxiv Atom sample
;; ---------------------------------------------------------------------------

(def ^:private SAMPLE_ATOM
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<feed xmlns=\"http://www.w3.org/2005/Atom\">
  <entry>
    <id>http://arxiv.org/abs/2303.11366</id>
    <title>Reflexion: Language Agents with Verbal Reinforcement Learning</title>
    <summary>We propose Reflexion, a novel framework to reinforce language agents not by updating weights but by linguistic feedback.</summary>
    <published>2023-03-20T00:00:00Z</published>
    <author><name>Noah Shinn</name></author>
  </entry>
  <entry>
    <id>http://arxiv.org/abs/2402.03620</id>
    <title>Self-Discover: LLMs Self-Compose Reasoning Structures</title>
    <summary>Self-Discover is a framework that lets LLMs compose reasoning structures.</summary>
    <published>2024-02-06T00:00:00Z</published>
    <author><name>Pei Zhou</name></author>
  </entry>
</feed>")

(defdescribe papers-test
  (describe "happy path: arxiv Atom → vec of citation maps"
    (with-redefs [http/get (fn [_url _opts] {:status 200 :body SAMPLE_ATOM})]
      (let [results (search/papers "reflexion")]
        (it "two entries"
          (expect (= 2 (count results))))

        (it "first entry shape :paper + :title + :url + :excerpt + :source"
          (let [e (first results)]
            (expect (= :paper (:type e)))
            (expect (re-find #"Reflexion" (:title e)))
            (expect (= "http://arxiv.org/abs/2303.11366" (:url e)))
            (expect (re-find #"linguistic feedback" (:excerpt e)))
            (expect (= :arxiv (:source e)))))

        (it "Self-Discover preserved"
          (expect (re-find #"Self-Discover" (:title (second results))))))))

  (describe "http throws → single error entry"
    (with-redefs [http/get (fn [_url _opts] (throw (ex-info "503" {})))]
      (let [results (search/papers "anything")]
        (it "one error entry"
          (expect (= 1 (count results)))
          (expect (true? (:error (first results))))
          (expect (= :paper (:type (first results)))))))))

;; ---------------------------------------------------------------------------
;; web / code — Exa MCP layer
;; ---------------------------------------------------------------------------
;;
;; The Exa MCP plumbing is exercised via `*http-send-fn*` which
;; short-circuits actual HTTP calls. We stitch a minimal "initialize"
;; + "tools/call" reply flow so the full path runs without network.

(defn- mcp-stub
  "Build an http-send-fn that returns canned JSON-RPC responses for
   initialize + tools/call. Each call increments `n` so we can pick
   the right reply per phase. `tools-call-text` is the text payload
   the MCP `tools/call` reply will carry."
  [tools-call-text]
  (let [n (atom 0)]
    (fn [{:keys [body]}]
      (swap! n inc)
      (let [parsed (try (clojure.edn/read-string body) (catch Throwable _ {}))
            id     (when (map? parsed) (:id parsed))
            method (when (map? parsed) (:method parsed))]
        ;; Notifications get 202 (no body, no parse). Otherwise we
        ;; respond with a JSON-RPC envelope matching the request id.
        (if (= method "notifications/initialized")
          {:status 202 :headers {"content-type" "application/json"} :body ""}
          (let [result (case method
                         "initialize" {:protocolVersion "2025-06-18"
                                       :capabilities {}
                                       :serverInfo {:name "stub" :version "1"}}
                         "tools/call" {:content [{:type "text"
                                                  :text tools-call-text}]}
                         {})]
            {:status 200
             :headers {"content-type" "application/json"}
             :body (str "{\"jsonrpc\":\"2.0\",\"id\":" (pr-str id)
                     ",\"result\":" (pr-str result) "}")}))))))

(defdescribe engine-scope-test
  (describe "every search/* symbol is :consult-only"
    (it "search/web carries #{:consult}"
      (expect (= #{:consult}
                (:ext.symbol/engine-scope search/web-symbol))))
    (it "search/code carries #{:consult}"
      (expect (= #{:consult}
                (:ext.symbol/engine-scope search/code-symbol))))
    (it "search/papers carries #{:consult}"
      (expect (= #{:consult}
                (:ext.symbol/engine-scope search/papers-symbol))))))

(defdescribe extension-shape-test
  (describe "alias is 'search; ext name is foundation-search"
    (it "alias"
      (expect (= 'search
                (get-in search/vis-extension [:ext/sci :ext.sci/alias]))))
    (it "name"
      (expect (= "foundation-search" (:ext/name search/vis-extension))))
    (it ":consult-only across all symbols"
      (let [scopes (set (map :ext.symbol/engine-scope search/search-symbols))]
        (expect (= #{#{:consult}} scopes))))))

(defdescribe prompt-shape-test
  (describe "search-prompt advertises consult-routing"
    (it "mentions consult-request!"
      (expect (str/includes? search/search-prompt "consult-request!")))
    (it "mentions the three bindings"
      (expect (str/includes? search/search-prompt "search/web"))
      (expect (str/includes? search/search-prompt "search/code"))
      (expect (str/includes? search/search-prompt "search/papers")))))
