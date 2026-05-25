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

;; ---------------------------------------------------------------------------
;; web / code — shape parity tests
;; ---------------------------------------------------------------------------

(def ^:private SAMPLE_EXA_TEXT
  ;; Two entries matching Exa MCP's actual reply format.
  "Title: metosin/malli
URL: https://github.com/metosin/malli
Published: 2019-05-17T19:21:51.000Z
Author: N/A
Highlights:
# Repository: metosin/malli
High-performance data-driven data specification library for Clojure/Script.
- Stars: 1710

Title: clojure.spec.alpha
URL: https://clojuredocs.org/clojure.spec.alpha
Published: 2018-01-01T00:00:00.000Z
Author: Rich Hickey
Highlights:
## Spec
The spec library specifies the structure of data.")

(defn- mock-mcp
  "Replace the private `call-mcp-tool!` so tests bypass the JSON-RPC
   wire format entirely — cleaner than stitching fake JSON."
  [text]
  (fn [_tool _args]
    {:endpoint "https://stub/mcp"
     :result   {:content [{:type "text" :text text}]}}))

(defdescribe web-shape-test
  (describe "search/web returns a vec of citation maps with the canonical shape"
    (with-redefs [com.blockether.vis.ext.foundation-search.core/call-mcp-tool!
                  (mock-mcp SAMPLE_EXA_TEXT)]
      (let [results (search/web "clojure malli" {:num-results 2})]

        (it "vec of two entries"
          (expect (= 2 (count results))))

        (it "every entry has :type :web :title :url :excerpt :source"
          (doseq [e results]
            (expect (= :web (:type e)))
            (expect (string? (:title e)))
            (expect (string? (:url e)))
            (expect (string? (:excerpt e)))
            (expect (= :exa (:source e)))))

        (it "first entry preserves title + url + markdown excerpt"
          (let [e (first results)]
            (expect (= "metosin/malli" (:title e)))
            (expect (= "https://github.com/metosin/malli" (:url e)))
            (expect (clojure.string/includes? (:excerpt e) "# Repository"))))

        (it "non-N/A authors carried through; N/A is stripped"
          (expect (nil? (:authors (first results))))
          (expect (= "Rich Hickey" (:authors (second results)))))

        (it ":published preserved"
          (expect (= "2019-05-17T19:21:51.000Z" (:published (first results)))))

        (it "NO tool-envelope keys (no :result :success? :metadata :symbol :tag)"
          (let [e (first results)]
            (doseq [k [:result :success? :metadata :symbol :tag :error]]
              (expect (not (contains? e k))))))))))

(defdescribe code-shape-test
  (describe "search/code mirrors search/web with :type :code"
    (with-redefs [com.blockether.vis.ext.foundation-search.core/call-mcp-tool!
                  (mock-mcp SAMPLE_EXA_TEXT)]
      (let [results (search/code "clojure spec" {:tokens-num 200})]
        (it ":type :code on every entry"
          (doseq [e results]
            (expect (= :code (:type e)))))
        (it "shape parity with web / papers"
          (doseq [e results]
            (expect (string? (:title e)))
            (expect (string? (:url e)))
            (expect (string? (:excerpt e)))))))))

(defdescribe shape-parity-test
  (describe "all three search/* fns return vec of citation maps with the same canonical shape"
    (with-redefs [com.blockether.vis.ext.foundation-search.core/call-mcp-tool!
                  (mock-mcp SAMPLE_EXA_TEXT)
                  http/get (fn [_url _opts] {:status 200 :body SAMPLE_ATOM})]
      (let [w (first (search/web "x" {}))
            c (first (search/code "x" {}))
            p (first (search/papers "x" {}))
            base-keys #{:type :title :url :excerpt :source}]
        (it ":type is set on every result"
          (expect (#{:web} (:type w)))
          (expect (#{:code} (:type c)))
          (expect (#{:paper} (:type p))))
        (it "every entry has the canonical key set"
          (doseq [e [w c p]]
            (expect (every? #(contains? e %) base-keys))))
        (it ":source is `:exa` for web/code, `:arxiv` for papers"
          (expect (= :exa (:source w)))
          (expect (= :exa (:source c)))
          (expect (= :arxiv (:source p))))))))

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
