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
  (:require [babashka.http-client :as http]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.foundation-search.core :as search]
            [com.blockether.vis.internal.env-python :as boundary]
            [com.blockether.vis.internal.extension :as extension]
            [lazytest.core :refer [defdescribe describe expect it]])
  (:import (java.io ByteArrayOutputStream)
           (java.nio.charset StandardCharsets)
           (java.nio.file Files)
           (java.util.zip GZIPOutputStream)
           (org.apache.commons.compress.archivers.tar TarArchiveEntry TarArchiveOutputStream)))

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
  "Read the structured `:result` payload off a search envelope, VIEWED through
   the STRINGS-ONLY boundary. `boundary-view` passes string-keyed maps through
   verbatim and THROWS on any stray keyword key/value, so every assertion below
   also guards that the payload crosses to Python string-clean."
  [env]
  (boundary/boundary-view (:result env)))

(defdescribe
  papers-test
  (describe
    "happy path: arxiv Atom → envelope with citations vec"
    (with-redefs
      [http/get (fn [_url _opts]
                  {:status 200 :body SAMPLE_ATOM})]
      (let
        [env (search/search-papers "reflexion")
         r (envelope-result env)
         cs (get r "citations")]

        (it "envelope is a successful tool result"
            (expect (extension/envelope-success? env))
            (expect (= "search_papers" (get r "op")))
            (expect (= :search-papers (:symbol env))))
        (it "two citations" (expect (= 2 (count cs))) (expect (= 2 (get r "citation_count"))))
        (it ":query echoed back on the envelope" (expect (= "reflexion" (get r "query"))))
        (it "first citation shape paper + title + url + excerpt + source"
            (let [e (first cs)]
              (expect (= "paper" (get e "type")))
              (expect (re-find #"Reflexion" (get e "title")))
              (expect (= "http://arxiv.org/abs/2303.11366" (get e "url")))
              (expect (re-find #"linguistic feedback" (get e "excerpt")))
              (expect (= "arxiv" (get e "source")))))
        (it "Self-Discover preserved" (expect (re-find #"Self-Discover" (get (second cs) "title"))))
        (it "source + endpoint url present on the envelope payload"
            (expect (= "arxiv" (get r "source")))
            (expect (string? (get r "endpoint")))
            (expect (str/includes? (get r "endpoint") "arxiv.org/api/query"))))))
  (describe "http throws → failure envelope with single error citation"
            (with-redefs
              [http/get (fn [_url _opts]
                          (throw (ex-info "503" {})))]
              (let
                [env (search/search-papers "anything")
                 r (envelope-result env)]

                (it "envelope is a failure" (expect (extension/envelope-failure? env)))
                (it "structured :error map carried on the envelope"
                    (expect (= "503" (get-in env [:error :message])))
                    (expect (= :arxiv (get-in env [:error :source])))
                    (expect (= "anything" (get-in env [:error :query]))))
                (it "result error flag set + one error-flagged citation for in-band readers"
                    (expect (true? (get r "error")))
                    (expect (= 1 (count (get r "citations"))))
                    (expect (true? (get (first (get r "citations")) "error"))))))))

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
    {:endpoint "https://stub/mcp?exaApiKey=SECRET" :result {:content [{:type "text" :text text}]}}))

(defdescribe
  web-shape-test
  (describe
    "search/web returns a tool envelope wrapping a structured citation map"
    (with-redefs
      [com.blockether.vis.ext.foundation-search.core/call-mcp-tool! (mock-mcp SAMPLE_EXA_TEXT)]
      (let
        [env (search/search-web "clojure malli" {"num_results" 2})
         r (envelope-result env)
         cs (get r "citations")]

        (it "envelope is a successful tool result keyed :search-web"
            (expect (extension/envelope-success? env))
            (expect (= "search_web" (get r "op")))
            (expect (= :search-web (:symbol env))))
        (it ":query carried on the envelope payload" (expect (= "clojure malli" (get r "query"))))
        (it "two citations + citation_count parity"
            (expect (= 2 (count cs)))
            (expect (= 2 (get r "citation_count"))))
        (it "every citation has type web + title + url + excerpt + source"
            (doseq [e cs]
              (expect (= "web" (get e "type")))
              (expect (string? (get e "title")))
              (expect (string? (get e "url")))
              (expect (string? (get e "excerpt")))
              (expect (= "exa" (get e "source")))))
        (it "first entry preserves title + url + markdown excerpt"
            (let [e (first cs)]
              (expect (= "metosin/malli" (get e "title")))
              (expect (= "https://github.com/metosin/malli" (get e "url")))
              (expect (str/includes? (get e "excerpt") "# Repository"))))
        (it "endpoint is redacted before it lands on the envelope"
            (expect (str/includes? (get r "endpoint") "REDACTED"))
            (expect (not (str/includes? (get r "endpoint") "SECRET"))))
        (it "non-N/A authors carried through; N/A is stripped"
            (expect (nil? (get (first cs) "authors")))
            (expect (= "Rich Hickey" (get (second cs) "authors"))))
        (it ":published preserved on the citation"
            (expect (= "2019-05-17T19:21:51.000Z" (get (first cs) "published"))))))))

(defdescribe code-shape-test
             (describe "search/code mirrors search/web with :type :code"
                       (with-redefs
                         [com.blockether.vis.ext.foundation-search.core/call-mcp-tool!
                          (mock-mcp SAMPLE_EXA_TEXT)]
                         (let
                           [env (search/search-code "clojure spec" {"tokens_num" 200})
                            r (envelope-result env)
                            cs (get r "citations")]

                           (it "envelope op = search_code" (expect (= "search_code" (get r "op"))))
                           (it "type code on every entry"
                               (doseq [e cs]
                                 (expect (= "code" (get e "type")))))
                           (it "shape parity with web / papers"
                               (doseq [e cs]
                                 (expect (string? (get e "title")))
                                 (expect (string? (get e "url")))
                                 (expect (string? (get e "excerpt")))))))))

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

(defn- bare-marker-line? [line] (boolean (re-matches #"\s*(?:\[\.\.\.\]|\.\.\.|…)\s*" line)))

(defdescribe
  excerpt-truncation-marker-test
  (describe
    "Exa `[...]` truncation markers never sit on their own line"
    (with-redefs
      [com.blockether.vis.ext.foundation-search.core/call-mcp-tool! (mock-mcp SAMPLE_EXA_TRUNCATED)]
      (let
        [e (first (get (envelope-result (search/search-code "core.async" {})) "citations"))
         excerpt (get e "excerpt")
         lines (str/split-lines excerpt)]

        (it "no line in the excerpt is a bare truncation marker"
            (expect (string? excerpt))
            (expect (not-any? bare-marker-line? lines)))
        (it "the heading absorbs the marker that preceded the code fence"
            (expect (str/includes? excerpt "# File: clojure/core.async/examples/ex-go.clj [...]")))
        (it "a code fence's own body is left intact"
            (expect (str/includes? excerpt "(require '[clojure.core.async :as async])"))))))
  (describe "spurious fences + bare lead markers + genuine code placeholders"
            (with-redefs
              [com.blockether.vis.ext.foundation-search.core/call-mcp-tool! (mock-mcp
                                                                              SAMPLE_EXA_FENCED)]
              (let
                [e (first (get (envelope-result (search/search-code "asyncio" {})) "citations"))
                 excerpt (get e "excerpt")
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
  (let [ir (vis/markdown->ast excerpt)]
    {:code (->> (tree-seq vector? seq ir)
                (filter #(and (vector? %) (= :code (first %))))
                (map (fn [c]
                       (apply str (filter string? (rest c))))))
     :headings (->> (tree-seq vector? seq ir)
                    (filter #(and (vector? %) (= :h (first %))))
                    (map (fn [h]
                           (apply str (filter string? (tree-seq vector? seq h))))))}))

(defdescribe
  spurious-doc-fence-test
  (describe "a dangling bare ``` wrapping documentation prose is unwrapped"
            (with-redefs
              [com.blockether.vis.ext.foundation-search.core/call-mcp-tool! (mock-mcp
                                                                              SAMPLE_EXA_DOC_WRAP)]
              (let
                [e (first (get (envelope-result (search/search-web "k8s" {})) "citations"))
                 {:keys [code headings]} (ir-nodes (get e "excerpt"))]

                (it "the real ```yaml manifest survives as a code block"
                    (expect (some #(str/includes? % "apiVersion: apps/v1") code)))
                (it "the wrapped headings render as real headings, not code"
                    (expect (some #(str/includes? % "Common Mistakes") headings))
                    (expect (some #(str/includes? % "Frequently Asked Questions") headings)))
                (it "no code block contains the unwrapped prose"
                    (expect (not-any? #(str/includes? % "Common Mistakes") code))))))
  (describe "a dangling bare ``` wrapping real code is left ALONE"
            (with-redefs
              [com.blockether.vis.ext.foundation-search.core/call-mcp-tool! (mock-mcp
                                                                              SAMPLE_EXA_CODE_WRAP)]
              (let
                [e (first (get (envelope-result (search/search-web "js" {})) "citations"))
                 {:keys [code]} (ir-nodes (get e "excerpt"))]

                (it "the code stays inside a code block (no false unwrap)"
                    (expect (some #(str/includes? % "function add(a, b)") code)))))))

(defdescribe
  shape-parity-test
  (describe
    "all three search/* fns return the same envelope+citation shape"
    (with-redefs
      [com.blockether.vis.ext.foundation-search.core/call-mcp-tool!
       (mock-mcp SAMPLE_EXA_TEXT)

       http/get
       (fn [_url _opts]
         {:status 200 :body SAMPLE_ATOM})]

      (let
        [w
         (envelope-result (search/search-web "x" {}))

         c
         (envelope-result (search/search-code "x" {}))

         p
         (envelope-result (search/search-papers "x" {}))

         base-result-keys
         #{"op" "query" "citations" "citation_count" "truncated" "source"}

         base-citation-keys
         #{"type" "title" "url" "excerpt" "source"}]

        (it "every envelope payload has the canonical envelope keys"
            (doseq [r [w c p]]
              (expect (every? #(contains? r %) base-result-keys))))
        (it "op is set per fn"
            (expect (= "search_web" (get w "op")))
            (expect (= "search_code" (get c "op")))
            (expect (= "search_papers" (get p "op"))))
        (it "every citation has the canonical citation key set"
            (doseq
              [e [(first (get w "citations")) (first (get c "citations"))
                  (first (get p "citations"))]]
              (expect (every? #(contains? e %) base-citation-keys))))
        (it "source is `exa` for web/code, `arxiv` for papers"
            (expect (= "exa" (get w "source")))
            (expect (= "exa" (get c "source")))
            (expect (= "arxiv" (get p "source"))))))))

(defdescribe
  github-fallback-test
  (describe
    "code search uses GitHub only after a retryable Exa failure"
    (with-redefs
      [com.blockether.vis.ext.foundation-search.core/call-mcp-tool!
       (fn [_ _]
         (throw (ex-info "MCP HTTP 429" {:type :search/mcp-http-error :status 429})))

       com.blockether.vis.ext.foundation-search.core/*github-token-fn*
       (constantly "gh-cli-token")

       com.blockether.vis.ext.foundation-search.core/*github-get-fn*
       (fn [_url opts]
         (expect (= "Bearer gh-cli-token" (get-in opts [:headers "authorization"])))
         {:status 200
          :body
          "{\"items\":[{\"name\":\"core.clj\",\"path\":\"src/core.clj\",\"html_url\":\"https://github.com/acme/demo/blob/main/src/core.clj\",\"repository\":{\"full_name\":\"acme/demo\"},\"text_matches\":[{\"fragment\":\"(defn hello [] :world)\"}]}]}"})]

      (let
        [env
         (search/search-code "hello" {"num_results" 3})

         r
         (envelope-result env)

         citation
         (first (get r "citations"))]

        (it "returns a successful GitHub-sourced envelope"
            (expect (extension/envelope-success? env)))
        (it "reports the provider and its Exa fallback reason"
            (expect (= "github" (get r "source")))
            (expect (= "exa" (get r "fallback_from"))))
        (it "normalizes GitHub code matches into canonical citations"
            (expect (= "code" (get citation "type")))
            (expect (= "github" (get citation "source")))
            (expect (= "acme/demo/src/core.clj" (get citation "title")))
            (expect (str/includes? (get citation "excerpt") "defn hello"))))))
  (describe "auth and malformed Exa failures do not silently fail over"
            (with-redefs
              [com.blockether.vis.ext.foundation-search.core/call-mcp-tool!
               (fn [_ _]
                 (throw (ex-info "MCP HTTP 401" {:type :search/mcp-http-error :status 401})))

               com.blockether.vis.ext.foundation-search.core/*github-get-fn*
               (fn [& _]
                 (throw (ex-info "GitHub must not be called" {})))]

              (let [env (search/search-code "hello" {})]
                (it "keeps the original Exa error" (expect (extension/envelope-failure? env)))
                (it "does not claim a GitHub fallback"
                    (expect (= "exa" (get (envelope-result env) "source"))))))))

(defdescribe
  github-provider-test
  (describe
    "GitHub can be selected as the primary code provider"
    (with-redefs
      [com.blockether.vis.ext.foundation-search.core/call-mcp-tool!
       (fn [& _]
         (throw (ex-info "Exa must not be called" {})))

       com.blockether.vis.ext.foundation-search.core/*github-token-fn*
       (constantly "gh-cli-token")

       com.blockether.vis.ext.foundation-search.core/*github-get-fn*
       (fn [_ opts]
         (expect (= "Bearer gh-cli-token" (get-in opts [:headers "authorization"])))
         {:status 200
          :body
          "{\"items\":[{\"name\":\"readme.md\",\"path\":\"README.md\",\"html_url\":\"https://github.com/acme/demo/blob/main/README.md\",\"repository\":{\"full_name\":\"acme/demo\"}}]}"})]

      (let
        [env
         (search/search-code "hello" {"provider" "github"})

         r
         (envelope-result env)]

        (it "does not call Exa and does not claim a fallback"
            (expect (extension/envelope-success? env))
            (expect (= "github" (get r "source")))
            (expect (nil? (get r "fallback_from")))))))
  (describe "an unauthenticated GitHub CLI gives an actionable failure"
            (with-redefs
              [com.blockether.vis.ext.foundation-search.core/*github-token-fn*
               (constantly nil)

               com.blockether.vis.ext.foundation-search.core/*github-get-fn*
               (fn [& _]
                 (throw (ex-info "GitHub HTTP must not be called" {})))]

              (let
                [env
                 (search/search-code "hello" {"provider" "github"})

                 c
                 (first (get (envelope-result env) "citations"))]

                (it "asks the user to authenticate with gh instead of searching anonymously"
                    (expect (extension/envelope-failure? env))
                    (expect (= "search_github_auth_required" (get c "error_type")))
                    (expect (str/includes? (get c "excerpt") "gh auth login")))))))

(defdescribe engine-scope-test
             (describe "no search/* symbol declares an engine-scope (single agent surface)"
                       (doseq
                         [[label sym-entry] [[:web search/web-symbol] [:code search/code-symbol]
                                             [:download-code search/download-code-symbol]
                                             [:papers search/papers-symbol]]]
                         (it (str (name label) " omits :ext.symbol/engine-scope")
                             (expect (nil? (:ext.symbol/engine-scope sym-entry))))
                         (it (str (name label) " no longer ships as a :raw? helper")
                             (expect (not (true? (:ext.symbol/raw? sym-entry))))))))

(defdescribe native-contract-test
             (it "keeps research routing compact and every input schema closed"
                 (doseq [s search/search-symbols]
                   (expect (< (count (:ext.symbol/description s)) 500))
                   (expect (false? (get-in s [:ext.symbol/schema :additionalProperties]))))))

(defdescribe
  extension-shape-test
  (describe "engine binds builtin (bare); ext name is foundation-search"
            (it "builtin?"
                (expect (= true (get-in search/vis-extension [:ext/engine :ext.engine/builtin?]))))
            (it "name" (expect (= "foundation-search" (:ext/name search/vis-extension))))
            (it "no symbol carries an engine-scope"
                (let [scopes (set (map :ext.symbol/engine-scope search/search-symbols))]
                  (expect (= #{nil} scopes))))))

(describe "the web_search toggle"
          (it "is persisted, enabled by default, and gates the extension"
              (let
                [spec
                 (vis/toggle-spec "web_search")

                 active?
                 (:ext/activation-fn search/vis-extension)]

                (expect (= "Web search" (:label spec)))
                (expect (true? (:default spec)))
                (expect (true? (:persist? spec)))
                (expect (true? (active? nil)))
                (with-redefs [vis/toggle-enabled? (constantly false)]
                  (expect (false? (active? nil)))))))

(defn- routed
  "Run `search` with the three per-kind fns stubbed so the assertion sees ONLY
   the dispatch decision. Redefs live INSIDE the call (lazytest defers `it`
   bodies, so a `with-redefs` wrapping `describe` would already be unwound)."
  [& args]
  (with-redefs
    [search/search-web
     (fn [q opts]
       {:hit :web :q q :opts opts})

     search/search-code
     (fn [q opts]
       {:hit :code :q q :opts opts})

     search/search-papers
     (fn [q opts]
       {:hit :papers :q q :opts opts})]

    (apply search/search args)))

(defdescribe
  unified-search-test
  (describe
    "one `search` tool, kind picks the corpus"
    (it "default kind is web" (expect (= :web (:hit (routed "x")))))
    (it "kind=web routes to search-web" (expect (= :web (:hit (routed "x" {"kind" "web"})))))
    (it "kind=code routes to search-code" (expect (= :code (:hit (routed "x" {"kind" "code"})))))
    (it "kind=papers routes to search-papers"
        (expect (= :papers (:hit (routed "x" {"kind" "papers"})))))
    (it "`kind` is stripped before per-kind opts are forwarded"
        (expect (= {"num_results" 3} (:opts (routed "x" {"kind" "web" "num_results" 3}))))))
  (describe
    "unknown kind fails loudly instead of silently searching the web"
    (it "returns an error citation naming the valid kinds"
        (let [c (first (get (envelope-result (search/search "x" {"kind" "moon"})) "citations"))]
          (expect (true? (get c "error")))
          (expect (str/includes? (get c "excerpt") "web | code | papers")))))
  (describe "wire surface"
            (it "`search` is the only NATIVE search tool"
                (expect (true? (:ext.symbol/native-tool? search/search-symbol)))
                (doseq
                  [sym-entry [search/web-symbol search/code-symbol search/download-code-symbol
                              search/papers-symbol]]
                  (expect (false? (:ext.symbol/native-tool? sym-entry)))))
            (it "the schema exposes the three search kinds"
                (expect (= ["web" "code" "papers"]
                           (get-in search/search-symbol
                                   [:ext.symbol/schema :properties "kind" :enum]))))
            (it "download_code requires a repository and bounds its input"
                (expect (= ["repository"]
                           (get-in search/download-code-symbol [:ext.symbol/schema :required])))
                (expect (= 20
                           (get-in search/download-code-symbol
                                   [:ext.symbol/schema :properties "max_files" :maximum]))))))

(defn- tar-gzip-fixture
  [entries]
  (let [out (ByteArrayOutputStream.)]
    (with-open
      [gzip (GZIPOutputStream. out)
       tar (TarArchiveOutputStream. gzip)]

      (doseq [[path content] entries]
        (let
          [data (.getBytes ^String content StandardCharsets/UTF_8)
           entry (TarArchiveEntry. path)]

          (.setSize entry (alength data))
          (.putArchiveEntry tar entry)
          (.write tar data 0 (alength data))
          (.closeArchiveEntry tar))))
    (.toByteArray out)))

(defdescribe download-archive-test
             (it "downloads and extracts the complete codeload archive below the supplied workspace"
                 (let
                   [workspace (.toFile (Files/createTempDirectory
                                         "vis-download-archive-test"
                                         (make-array java.nio.file.attribute.FileAttribute 0)))]
                   (try (let
                          [archive (tar-gzip-fixture {"demo-main/README.md" "hello archive\n"
                                                      "demo-main/src/app.clj" "(println :ok)\n"})]
                          (with-redefs
                            [search/*github-get-fn* (fn [_ _]
                                                      {:status 200 :body archive})]
                            (let
                              [env (search/download-archive workspace "acme/demo" {"ref" "main"})
                               result (envelope-result env)
                               path (get result "path")]

                              (expect (extension/envelope-success? env))
                              (expect (= "download_archive" (get result "op")))
                              (expect (= 2 (get result "files")))
                              (expect (= "hello archive\n" (slurp (io/file path "README.md"))))
                              (expect (= "(println :ok)\n" (slurp (io/file path "src/app.clj")))))))
                        (finally (doseq [f (reverse (file-seq workspace))]
                                   (io/delete-file f true))))))
             (it "exposes a closed sandbox schema with a workspace-relative destination"
                 (expect (= ["repository"]
                            (get-in search/download-archive-symbol [:ext.symbol/schema :required])))
                 (expect (contains? (get-in search/download-archive-symbol
                                            [:ext.symbol/schema :properties])
                                    "directory"))))
