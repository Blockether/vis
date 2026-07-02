(ns com.blockether.vis.ext.foundation-search.core-test
  "`search/*` extension tests. HTTP layer is mocked so the suite never
   touches the network. Tests confirm:
     - every search/* fn returns the canonical tool envelope
       (`extension/success` / `extension/failure`)
     - `:result` carries the structured shape
       `{:op :query :citations [...] :citation-count :truncated?
         :source :endpoint?}`
     - parse-arxiv-atom maps arxiv entries into the canonical citation
       shape"
  (:require
   [babashka.http-client :as http]
   [clojure.string :as str]
   [com.blockether.vis.core :as vis]
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
      (let [env (search/search-papers "reflexion")
            r   (envelope-result env)
            cs  (:citations r)]

        (it "envelope is a successful tool result"
          (expect (extension/envelope-success? env))
          (expect (= :search-papers (:op r)))
          (expect (= :search-papers (:symbol env))))

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
      (let [env (search/search-papers "anything")
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
      (let [env (search/search-web "clojure malli" {:num_results 2})
            r   (envelope-result env)
            cs  (:citations r)]

        (it "envelope is a successful tool result keyed :search-web"
          (expect (extension/envelope-success? env))
          (expect (= :search-web (:op r)))
          (expect (= :search-web (:symbol env))))

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
      (let [env (search/search-code "clojure spec" {:tokens_num 200})
            r   (envelope-result env)
            cs  (:citations r)]
        (it "envelope :op = :search-code"
          (expect (= :search-code (:op r))))
        (it ":type :code on every entry"
          (doseq [e cs]
            (expect (= :code (:type e)))))
        (it "shape parity with web / papers"
          (doseq [e cs]
            (expect (string? (:title e)))
            (expect (string? (:url e)))
            (expect (string? (:excerpt e)))))))))

;; Exa stitches non-contiguous page fragments with truncation markers on
;; their OWN line. Between block-level neighbours (a `# File:` heading and
;; a ``` fence, two list blocks, …) CommonMark turns a lone marker into
;; its own paragraph, so it paints on an empty line — and reads the same
;; way in the model-facing excerpt. Exa also (a) wraps body text in a
;; spurious, often unterminated ``` fence with `[...]` separators inside,
;; and (b) starts each code fence with a bare `...` lead marker. The
;; Code/Highlights body must fold/strip all of these inline while leaving
;; a genuine `...` code placeholder (e.g. a Python `Ellipsis` stub) alone.
(def ^:private SAMPLE_EXA_TRUNCATED
  "Title: clojure/core.async ex-go.clj
URL: https://github.com/clojure/core.async/blob/x/examples/ex-go.clj
Code/Highlights:
# File: clojure/core.async/examples/ex-go.clj
[...]
```clj
(require '[clojure.core.async :as async])
```
[...]
trailing prose fragment")

;; Body wrapped in a spurious unterminated ``` fence with `[...]`
;; separators inside, AND a Python fence whose lead `...` is an Exa marker
;; but whose `def f():` / `...` stub body is genuine code.
(def ^:private SAMPLE_EXA_FENCED
  "Title: asyncio.gather guide
URL: https://example.com/asyncio
Code/Highlights:
```
The spec library specifies the structure of data.
[...]
- get-spec
[...]
- fspec
```
```python
...
def handler():
    ...
```")

(defn- bare-marker-line?
  [line]
  (boolean (re-matches #"\s*(?:\[\.\.\.\]|\.\.\.|…)\s*" line)))

(defdescribe excerpt-truncation-marker-test
  (describe "Exa `[...]` truncation markers never sit on their own line"
    (with-redefs [com.blockether.vis.ext.foundation-search.core/call-mcp-tool!
                  (mock-mcp SAMPLE_EXA_TRUNCATED)]
      (let [e (first (:citations (envelope-result (search/search-code "core.async" {}))))
            excerpt (:excerpt e)
            lines (str/split-lines excerpt)]
        (it "no line in the excerpt is a bare truncation marker"
          (expect (string? excerpt))
          (expect (not-any? bare-marker-line? lines)))
        (it "the heading absorbs the marker that preceded the code fence"
          (expect (str/includes? excerpt
                    "# File: clojure/core.async/examples/ex-go.clj [...]")))
        (it "a code fence's own body is left intact"
          (expect (str/includes? excerpt "(require '[clojure.core.async :as async])"))))))

  (describe "spurious fences + bare lead markers + genuine code placeholders"
    (with-redefs [com.blockether.vis.ext.foundation-search.core/call-mcp-tool!
                  (mock-mcp SAMPLE_EXA_FENCED)]
      (let [e (first (:citations (envelope-result (search/search-code "asyncio" {}))))
            excerpt (:excerpt e)
            lines (str/split-lines excerpt)]
        (it "no `[...]` bracket marker survives on its own line anywhere"
          (expect (not-any? #(re-matches #"\s*\[\.\.\.\]\s*" %) lines)))
        (it "Exa's bare lead marker after the ```python fence is dropped"
          ;; the `...` that abutted the opening fence is gone; the only
          ;; bare line left is the genuine Ellipsis stub body
          (expect (not (str/includes? excerpt "python\n...")))
          (expect (= 1 (count (filter bare-marker-line? lines)))))
        (it "`[...]` separators inside the wrapper fold onto prior content"
          (expect (str/includes? excerpt "the structure of data. [...]"))
          (expect (str/includes? excerpt "- get-spec [...]")))
        (it "a genuine `...` Ellipsis stub body is preserved as real code"
          ;; the `def handler():` stub keeps its `...` body line
          (expect (str/includes? excerpt "def handler():"))
          (expect (re-find #"def handler\(\):\n\s*\.\.\." excerpt)))))))

;; Exa sometimes wraps a result's body PROSE in a bare, unterminated ```
;; fence, so the whole entry paints as one mono code block. A real
;; language-tagged manifest in the same excerpt must survive untouched,
;; while the dangling bare doc-prose fence is unwrapped to native markdown.
(def ^:private SAMPLE_EXA_DOC_WRAP
  "Title: K8s Deployment Guide
URL: https://example.com/k8s
Highlights:
```yaml
apiVersion: apps/v1
kind: Deployment
```
```
## Common Mistakes in Kubernetes YAML
- Indentation: YAML files are whitespace sensitive and need consistent spaces.
- Missing fields: ensure all required fields appear in your Deployment manifest.
A Deployment manages the lifecycle of Pods, including scaling and rolling updates.
## Frequently Asked Questions
- A Pod is a single deployable unit while a Deployment orchestrates many Pods here.")

;; A dangling BARE fence whose body is genuine code (no headings, no
;; bullets, no prose sentences) must NOT be unwrapped — that is exactly
;; the regression (code mangled into markdown) we are guarding against.
(def ^:private SAMPLE_EXA_CODE_WRAP
  "Title: JS sample
URL: https://example.com/js
Highlights:
```
function add(a, b) {
  return a + b;
}
const total = add(1, 2);")

(defn- ir-nodes
  "Flatten an excerpt's parsed IR to its block tags + the text under each
   :code / :h node, for structural assertions."
  [excerpt]
  (let [ir (vis/markdown->ir excerpt)]
    {:code (->> (tree-seq vector? seq ir)
             (filter #(and (vector? %) (= :code (first %))))
             (map (fn [c] (apply str (filter string? (rest c))))))
     :headings (->> (tree-seq vector? seq ir)
                 (filter #(and (vector? %) (= :h (first %))))
                 (map (fn [h] (apply str (filter string? (tree-seq vector? seq h))))))}))

(defdescribe spurious-doc-fence-test
  (describe "a dangling bare ``` wrapping documentation prose is unwrapped"
    (with-redefs [com.blockether.vis.ext.foundation-search.core/call-mcp-tool!
                  (mock-mcp SAMPLE_EXA_DOC_WRAP)]
      (let [e (first (:citations (envelope-result (search/search-web "k8s" {}))))
            {:keys [code headings]} (ir-nodes (:excerpt e))]
        (it "the real ```yaml manifest survives as a code block"
          (expect (some #(str/includes? % "apiVersion: apps/v1") code)))
        (it "the wrapped headings render as real headings, not code"
          (expect (some #(str/includes? % "Common Mistakes") headings))
          (expect (some #(str/includes? % "Frequently Asked Questions") headings)))
        (it "no code block contains the unwrapped prose"
          (expect (not-any? #(str/includes? % "Common Mistakes") code))))))

  (describe "a dangling bare ``` wrapping real code is left ALONE"
    (with-redefs [com.blockether.vis.ext.foundation-search.core/call-mcp-tool!
                  (mock-mcp SAMPLE_EXA_CODE_WRAP)]
      (let [e (first (:citations (envelope-result (search/search-web "js" {}))))
            {:keys [code]} (ir-nodes (:excerpt e))]
        (it "the code stays inside a code block (no false unwrap)"
          (expect (some #(str/includes? % "function add(a, b)") code)))))))

(defdescribe shape-parity-test
  (describe "all three search/* fns return the same envelope+citation shape"
    (with-redefs [com.blockether.vis.ext.foundation-search.core/call-mcp-tool!
                  (mock-mcp SAMPLE_EXA_TEXT)
                  http/get (fn [_url _opts] {:status 200 :body SAMPLE_ATOM})]
      (let [w (envelope-result (search/search-web "x" {}))
            c (envelope-result (search/search-code "x" {}))
            p (envelope-result (search/search-papers "x" {}))
            base-result-keys #{:op :query :citations :citation-count :truncated? :source}
            base-citation-keys #{:type :title :url :excerpt :source}]
        (it "every envelope payload has the canonical envelope keys"
          (doseq [r [w c p]]
            (expect (every? #(contains? r %) base-result-keys))))
        (it ":op is set per fn"
          (expect (= :search-web    (:op w)))
          (expect (= :search-code   (:op c)))
          (expect (= :search-papers (:op p))))
        (it "every citation has the canonical citation key set"
          (doseq [e [(first (:citations w))
                     (first (:citations c))
                     (first (:citations p))]]
            (expect (every? #(contains? e %) base-citation-keys))))
        (it ":source is `:exa` for web/code, `:arxiv` for papers"
          (expect (= :exa   (:source w)))
          (expect (= :exa   (:source c)))
          (expect (= :arxiv (:source p))))))))

(defdescribe engine-scope-test
  (describe "no search/* symbol declares an engine-scope (single agent surface)"
    (doseq [[label sym-entry] [[:web    search/web-symbol]
                               [:code   search/code-symbol]
                               [:papers search/papers-symbol]]]
      (it (str (name label) " omits :ext.symbol/engine-scope")
        (expect (nil? (:ext.symbol/engine-scope sym-entry))))
      (it (str (name label) " no longer ships as a :raw? helper")
        (expect (not (true? (:ext.symbol/raw? sym-entry))))))))

(defdescribe extension-shape-test
  (describe "engine binds builtin (bare); ext name is foundation-search"
    (it "builtin?"
      (expect (= true
                (get-in search/vis-extension [:ext/engine :ext.engine/builtin?]))))
    (it "name"
      (expect (= "foundation-search" (:ext/name search/vis-extension))))
    (it "no symbol carries an engine-scope"
      (let [scopes (set (map :ext.symbol/engine-scope search/search-symbols))]
        (expect (= #{nil} scopes))))))
