(ns com.blockether.vis.ext.foundation-search.core-test
  "`search/*` extension tests. HTTP layer is mocked so the suite never
   touches the network. Tests confirm:
     - every search/* fn returns the canonical tool envelope
       (`extension/success` / `extension/failure`)
     - `:result` carries the structured shape
       `{:vis.op :query :citations [...] :citation-count :truncated?
         :source :endpoint?}`
     - parse-arxiv-atom maps arxiv entries into the canonical citation
       shape
     - all three symbols ship a
       `:render-fn` (channel-renderable structured output)
     - the channel renderer produces canonical answer-IR (`[:ir ...]`)
       with citation cards, markdown-parsed excerpts, and a failure
       badge on errors"
  (:require
   [babashka.http-client :as http]
   [clojure.string :as str]
   [com.blockether.vis.ext.foundation-search.core :as search]
   [com.blockether.vis.internal.extension :as extension]
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

(defn- envelope-result
  "Read the structured `:result` payload off a search envelope."
  [env]
  (:result env))

(defn- citations
  "Pull the citation vec out of an envelope."
  [env]
  (:citations (envelope-result env)))

(defdescribe papers-test
  (describe "happy path: arxiv Atom → envelope with citations vec"
    (with-redefs [http/get (fn [_url _opts] {:status 200 :body SAMPLE_ATOM})]
      (let [env (search/papers "reflexion")
            r   (envelope-result env)
            cs  (:citations r)]

        (it "envelope is a successful tool result"
          (expect (extension/envelope-success? env))
          (expect (= :search/papers (:vis.op r)))
          (expect (= :search/papers (:symbol env))))

        (it "two citations"
          (expect (= 2 (count cs)))
          (expect (= 2 (:citation-count r))))

        (it ":query echoed back on the envelope"
          (expect (= "reflexion" (:query r))))

        (it "first citation shape :paper + :title + :url + :excerpt + :source"
          (let [e (first cs)]
            (expect (= :paper (:type e)))
            (expect (re-find #"Reflexion" (:title e)))
            (expect (= "http://arxiv.org/abs/2303.11366" (:url e)))
            (expect (re-find #"linguistic feedback" (:excerpt e)))
            (expect (= :arxiv (:source e)))))

        (it "Self-Discover preserved"
          (expect (re-find #"Self-Discover" (:title (second cs)))))

        (it "source kw + endpoint url present on the envelope payload"
          (expect (= :arxiv (:source r)))
          (expect (string? (:endpoint r)))
          (expect (str/includes? (:endpoint r) "arxiv.org/api/query"))))))

  (describe "http throws → failure envelope with single error citation"
    (with-redefs [http/get (fn [_url _opts] (throw (ex-info "503" {})))]
      (let [env (search/papers "anything")
            r   (envelope-result env)]
        (it "envelope is a failure"
          (expect (extension/envelope-failure? env)))
        (it "structured :error map carried on the envelope"
          (expect (= "503" (get-in env [:error :message])))
          (expect (= :arxiv (get-in env [:error :source])))
          (expect (= "anything" (get-in env [:error :query]))))
        (it "result :error? flag set + one error-flagged citation for in-band readers"
          (expect (true? (:error? r)))
          (expect (= 1 (count (:citations r))))
          (expect (true? (:error (first (:citations r))))))))))

;; ---------------------------------------------------------------------------
;; web / code — Exa MCP layer
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
   wire format entirely."
  [text]
  (fn [_tool _args]
    {:endpoint "https://stub/mcp?exaApiKey=SECRET"
     :result   {:content [{:type "text" :text text}]}}))

(defdescribe web-shape-test
  (describe "search/web returns a tool envelope wrapping a structured citation map"
    (with-redefs [com.blockether.vis.ext.foundation-search.core/call-mcp-tool!
                  (mock-mcp SAMPLE_EXA_TEXT)]
      (let [env (search/web "clojure malli" {:num-results 2})
            r   (envelope-result env)
            cs  (:citations r)]

        (it "envelope is a successful tool result keyed :search/web"
          (expect (extension/envelope-success? env))
          (expect (= :search/web (:vis.op r)))
          (expect (= :search/web (:symbol env))))

        (it ":query carried on the envelope payload"
          (expect (= "clojure malli" (:query r))))

        (it "two citations + :citation-count parity"
          (expect (= 2 (count cs)))
          (expect (= 2 (:citation-count r))))

        (it "every citation has :type :web + :title + :url + :excerpt + :source"
          (doseq [e cs]
            (expect (= :web (:type e)))
            (expect (string? (:title e)))
            (expect (string? (:url e)))
            (expect (string? (:excerpt e)))
            (expect (= :exa (:source e)))))

        (it "first entry preserves title + url + markdown excerpt"
          (let [e (first cs)]
            (expect (= "metosin/malli" (:title e)))
            (expect (= "https://github.com/metosin/malli" (:url e)))
            (expect (str/includes? (:excerpt e) "# Repository"))))

        (it "endpoint is redacted before it lands on the envelope"
          (expect (str/includes? (:endpoint r) "REDACTED"))
          (expect (not (str/includes? (:endpoint r) "SECRET"))))

        (it "non-N/A authors carried through; N/A is stripped"
          (expect (nil? (:authors (first cs))))
          (expect (= "Rich Hickey" (:authors (second cs)))))

        (it ":published preserved on the citation"
          (expect (= "2019-05-17T19:21:51.000Z" (:published (first cs)))))))))

(defdescribe code-shape-test
  (describe "search/code mirrors search/web with :type :code"
    (with-redefs [com.blockether.vis.ext.foundation-search.core/call-mcp-tool!
                  (mock-mcp SAMPLE_EXA_TEXT)]
      (let [env (search/code "clojure spec" {:tokens-num 200})
            r   (envelope-result env)
            cs  (:citations r)]
        (it "envelope :vis.op = :search/code"
          (expect (= :search/code (:vis.op r))))
        (it ":type :code on every entry"
          (doseq [e cs]
            (expect (= :code (:type e)))))
        (it "shape parity with web / papers"
          (doseq [e cs]
            (expect (string? (:title e)))
            (expect (string? (:url e)))
            (expect (string? (:excerpt e)))))))))

(defdescribe shape-parity-test
  (describe "all three search/* fns return the same envelope+citation shape"
    (with-redefs [com.blockether.vis.ext.foundation-search.core/call-mcp-tool!
                  (mock-mcp SAMPLE_EXA_TEXT)
                  http/get (fn [_url _opts] {:status 200 :body SAMPLE_ATOM})]
      (let [w (envelope-result (search/web "x" {}))
            c (envelope-result (search/code "x" {}))
            p (envelope-result (search/papers "x" {}))
            base-result-keys #{:vis.op :query :citations :citation-count :truncated? :source}
            base-citation-keys #{:type :title :url :excerpt :source}]
        (it "every envelope payload has the canonical envelope keys"
          (doseq [r [w c p]]
            (expect (every? #(contains? r %) base-result-keys))))
        (it ":vis.op is set per fn"
          (expect (= :search/web    (:vis.op w)))
          (expect (= :search/code   (:vis.op c)))
          (expect (= :search/papers (:vis.op p))))
        (it "every citation has the canonical citation key set"
          (doseq [e [(first (:citations w))
                     (first (:citations c))
                     (first (:citations p))]]
            (expect (every? #(contains? e %) base-citation-keys))))
        (it ":source is `:exa` for web/code, `:arxiv` for papers"
          (expect (= :exa   (:source w)))
          (expect (= :exa   (:source c)))
          (expect (= :arxiv (:source p))))))))

(defdescribe channel-render-test
  (describe "channel-render-search returns the {:summary :display} contract"
    (with-redefs [com.blockether.vis.ext.foundation-search.core/call-mcp-tool!
                  (mock-mcp SAMPLE_EXA_TEXT)]
      (let [env    (search/web "clojure malli" {:num-results 2})
            r      (envelope-result env)
            result (search/channel-render-search r)
            disp   (:display result)
            disp-text (pr-str disp)]

        (it "conforms to ::render-fn-result"
          (expect (extension/render-fn-result? result))
          (expect (some? (:summary result)))
          (expect (some? (:display result))))

        (it "summary is a zone map with label + query + count"
          (let [summary (:summary result)]
            (expect (extension/render-zones? summary))
            (let [s (pr-str summary)]
              (expect (str/includes? s "SEARCH WEB"))
              (expect (str/includes? s "clojure malli"))
              (expect (str/includes? s "2 citations")))))

        (it "display root is an :ir node"
          (expect (= :ir (first disp))))

        (it "display head paragraph carries the badge label + citation count"
          (expect (str/includes? disp-text "SEARCH WEB"))
          (expect (str/includes? disp-text "2 citations")))

        (it "each citation contributes a clickable link block in :display"
          (expect (str/includes? disp-text "https://github.com/metosin/malli"))
          (expect (str/includes? disp-text "https://clojuredocs.org/clojure.spec.alpha"))
          (expect (str/includes? disp-text "metosin/malli")))

        (it "markdown excerpts are parsed into commonmark blocks (not raw text)"
          ;; The `# Repository: …` excerpt header must become an :h node
          ;; (`[:h {:level 1} …]` in the canonical IR) — that's the whole
          ;; point of the structured output. A regression to raw
          ;; `# Repository` text would render as a plain :p span without
          ;; any :h / :ul tags in the tree.
          (expect (str/includes? disp-text ":h "))
          (expect (str/includes? disp-text ":level 1"))
          (expect (str/includes? disp-text ":level 2"))
          ;; Bullet list under the first citation lands as :ul + :li.
          (expect (str/includes? disp-text ":ul"))
          (expect (str/includes? disp-text ":li")))))

    (describe "failure render carries a visible failure badge"
      (let [result (search/channel-render-search
                     (envelope-result
                       (with-redefs [http/get (fn [_url _opts] (throw (ex-info "503" {})))]
                         (search/papers "anything"))))]
        (it "conforms to ::render-fn-result"
          (expect (extension/render-fn-result? result))
          (expect (some? (:summary result)))
          (expect (some? (:display result))))
        (it "summary marks the failure"
          (let [s (pr-str (:summary result))]
            (expect (str/includes? s "SEARCH PAPERS"))
            (expect (str/includes? s "failed"))))
        (it "display header text"
          (let [text (pr-str (:display result))]
            (expect (str/includes? text "SEARCH PAPERS"))
            (expect (str/includes? text "failed"))))))))

(defdescribe engine-scope-test
  (describe "no search/* symbol declares an engine-scope (single agent surface)"
    (doseq [[label sym-entry] [[:web    search/web-symbol]
                               [:code   search/code-symbol]
                               [:papers search/papers-symbol]]]
      (it (str (name label) " omits :ext.symbol/engine-scope")
        (expect (nil? (:ext.symbol/engine-scope sym-entry))))
      (it (str (name label) " has a :render-fn (structured-output parity with v/*)")
        (expect (fn? (:ext.symbol/render-fn sym-entry))))
      (it (str (name label) " no longer ships as a :raw? helper")
        (expect (not (true? (:ext.symbol/raw? sym-entry))))))))

(defdescribe extension-shape-test
  (describe "alias is 'search; ext name is foundation-search"
    (it "alias"
      (expect (= 'search
                (get-in search/vis-extension [:ext/sci :ext.sci/alias]))))
    (it "name"
      (expect (= "foundation-search" (:ext/name search/vis-extension))))
    (it "no symbol carries an engine-scope"
      (let [scopes (set (map :ext.symbol/engine-scope search/search-symbols))]
        (expect (= #{nil} scopes))))))

(defdescribe prompt-shape-test
  (describe "search-prompt advertises direct in-sandbox calls"
    (it "does not route through consult"
      (expect (not (str/includes? search/search-prompt "consult"))))
    (it "mentions the three bindings"
      (expect (str/includes? search/search-prompt "search/web"))
      (expect (str/includes? search/search-prompt "search/code"))
      (expect (str/includes? search/search-prompt "search/papers")))))
