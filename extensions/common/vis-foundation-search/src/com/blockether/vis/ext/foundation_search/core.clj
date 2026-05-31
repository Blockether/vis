(ns com.blockether.vis.ext.foundation-search.core
  "`search/` research extension.

   Three model-facing bindings under alias `search`:
     (search/web    query opts?)  — web search via Exa MCP
     (search/code   query opts?)  — code/doc context via Exa MCP
     (search/papers query opts?)  — arxiv papers (Atom feed)

   All three bind directly in the primary agent sandbox.

   Output shape — parity with the rest of the `:tag :observation`
   tool surface (v/cat, v/ls, v/rg). Every search fn returns the
   canonical tool envelope; SCI sees the unwrapped `:result` map:

     {:vis.op       :search/web|:search/code|:search/papers
      :query        \"…\"
      :citations    [{:type :title :url :excerpt :source …} …]
      :citation-count N
      :truncated?   B
      :source       :exa|:arxiv
      :endpoint     \"…REDACTED…\"   ;; web/code only
      :error?       true               ;; only on failure
      :error        {:message … …}}    ;; only on failure

   `:excerpt` is Markdown; the channel renderer parses it through
   `vis/markdown->ir` so the TUI / Telegram / web channels render
   commonmark blocks (headings, lists, code fences) instead of dumping
   the raw blob. Errors are still surfaced inline as a single citation
   with `:error true` AND on the envelope's `:error` slot so failures
   stay visible no matter which lens the consumer reads.

   Exa MCP: the public endpoint supports basic unauthenticated use;
   set `EXA_API_KEY` (or `EXA_MCP_API_KEY`) for higher limits. The
   key is never logged or surfaced; the endpoint shown in results is
   redacted.

   arxiv: free public Atom feed via `https://export.arxiv.org/api/query`."
  (:require
   [babashka.http-client :as http]
   [charred.api :as json]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.xml :as xml]
   [com.blockether.vis.core :as vis]
   [com.blockether.vis.internal.extension :as extension])
  (:import
   (java.io ByteArrayInputStream)
   (java.net URI URLDecoder URLEncoder)
   (java.nio.charset StandardCharsets)
   (java.util UUID)))

(def ^:private default-endpoint "https://mcp.exa.ai/mcp")
(def ^:private default-timeout-ms 30000)
(def ^:private default-protocol-version "2025-06-18")
(def ^:private default-max-bytes 51200)
(def ^:private default-max-lines 2000)
(def ^:private default-tools ["web_search_exa" "get_code_context_exa"])

(def ^:private client-info
  {:name "vis-search-extension"
   :version "1.0.0"})

(def ^:private default-config
  {:url default-endpoint
   :api-key nil
   :tools default-tools
   :timeout-ms default-timeout-ms
   :protocol-version default-protocol-version
   :max-bytes default-max-bytes
   :max-lines default-max-lines})

(def ^:dynamic *http-send-fn*
  "Test seam. Function of `[request-map]` returning the
   babashka.http-client response shape `{:status :headers :body}`."
  nil)

(defn- env
  [k]
  (not-empty (str/trim (or (vis/extension-env-value k) ""))))

(defn- parse-long*
  [x]
  (cond
    (integer? x) (long x)
    (number? x)  (long x)
    (string? x)  (try
                   (Long/parseLong (str/trim x))
                   (catch Throwable _ nil))
    :else nil))

(defn- positive-long
  [x fallback]
  (let [n (parse-long* x)]
    (if (and n (pos? n)) n fallback)))

(defn- normalize-string
  [x]
  (when (string? x)
    (not-empty (str/trim x))))

(defn- env-token-name
  "Return var name for config strings shaped `$NAME`, `${NAME}`, or
   `env:NAME`. This lets extension JSON stay shareable without
   hard-coding secrets."
  [s]
  (when-let [s (normalize-string s)]
    (cond
      (str/starts-with? s "env:") (not-empty (subs s 4))
      (and (str/starts-with? s "${") (str/ends-with? s "}"))
      (not-empty (subs s 2 (dec (count s))))
      (and (str/starts-with? s "$") (> (count s) 1))
      (not-empty (subs s 1))
      :else nil)))

(defn- expand-env-value
  "Resolve `$VAR` / `${VAR}` / `env:VAR` config values. Missing env
   returns nil so defaults or other config layers can win."
  [v]
  (if-let [k (and (string? v) (env-token-name v))]
    (env k)
    v))

(defn- split-tools
  [s]
  (when-let [s (normalize-string s)]
    (let [tools (->> (str/split s #",")
                  (map str/trim)
                  (remove str/blank?)
                  vec)]
      (when (seq tools) tools))))

(defn- normalize-tools
  [x]
  (cond
    (string? x) (split-tools x)
    (sequential? x) (let [tools (->> x
                                  (keep normalize-string)
                                  vec)]
                      (when (seq tools) tools))
    :else nil))

(defn- home-path
  [path]
  (let [home (System/getProperty "user.home")]
    (cond
      (str/starts-with? path "~/") (str home "/" (subs path 2))
      (str/starts-with? path "~")  (str home (subs path 1))
      :else path)))

(defn- config-candidates
  []
  (if-let [path (env "EXA_MCP_CONFIG")]
    [(home-path path)]
    []))

(defn- read-json-file
  [path]
  (let [f (io/file path)]
    (when (.exists f)
      (try
        (json/read-json (slurp f) :key-fn keyword)
        (catch Throwable _ nil)))))

(defn- config-from-file
  []
  (some read-json-file (config-candidates)))

(defn- config-file->internal
  [m]
  (when (map? m)
    (cond-> {}
      (normalize-string (expand-env-value (:url m)))
      (assoc :url (normalize-string (expand-env-value (:url m))))
      (normalize-string (expand-env-value (:apiKey m)))
      (assoc :api-key (normalize-string (expand-env-value (:apiKey m))))
      (normalize-string (expand-env-value (:api-key m)))
      (assoc :api-key (normalize-string (expand-env-value (:api-key m))))
      (normalize-tools (expand-env-value (:tools m)))
      (assoc :tools (normalize-tools (expand-env-value (:tools m))))
      (parse-long* (expand-env-value (:timeoutMs m)))
      (assoc :timeout-ms (parse-long* (expand-env-value (:timeoutMs m))))
      (parse-long* (expand-env-value (:timeout-ms m)))
      (assoc :timeout-ms (parse-long* (expand-env-value (:timeout-ms m))))
      (normalize-string (expand-env-value (:protocolVersion m)))
      (assoc :protocol-version (normalize-string (expand-env-value (:protocolVersion m))))
      (normalize-string (expand-env-value (:protocol-version m)))
      (assoc :protocol-version (normalize-string (expand-env-value (:protocol-version m))))
      (parse-long* (expand-env-value (:maxBytes m)))
      (assoc :max-bytes (parse-long* (expand-env-value (:maxBytes m))))
      (parse-long* (expand-env-value (:max-bytes m)))
      (assoc :max-bytes (parse-long* (expand-env-value (:max-bytes m))))
      (parse-long* (expand-env-value (:maxLines m)))
      (assoc :max-lines (parse-long* (expand-env-value (:maxLines m))))
      (parse-long* (expand-env-value (:max-lines m)))
      (assoc :max-lines (parse-long* (expand-env-value (:max-lines m)))))))

(defn effective-config
  "Return effective Exa MCP config. Public for diagnostics/tests."
  []
  (let [file-cfg (config-file->internal (config-from-file))]
    (-> default-config
      (merge file-cfg)
      (cond->
        (env "EXA_MCP_URL") (assoc :url (env "EXA_MCP_URL"))
        (env "EXA_MCP_TOOLS") (assoc :tools (or (split-tools (env "EXA_MCP_TOOLS")) default-tools))
        (or (env "EXA_API_KEY") (env "EXA_MCP_API_KEY"))
        (assoc :api-key (or (env "EXA_API_KEY") (env "EXA_MCP_API_KEY")))
        (env "EXA_MCP_TIMEOUT_MS")
        (assoc :timeout-ms (positive-long (env "EXA_MCP_TIMEOUT_MS") default-timeout-ms))
        (env "EXA_MCP_PROTOCOL_VERSION")
        (assoc :protocol-version (env "EXA_MCP_PROTOCOL_VERSION"))
        (env "EXA_MCP_MAX_BYTES")
        (assoc :max-bytes (positive-long (env "EXA_MCP_MAX_BYTES") default-max-bytes))
        (env "EXA_MCP_MAX_LINES")
        (assoc :max-lines (positive-long (env "EXA_MCP_MAX_LINES") default-max-lines))))))

(defn- encode-url
  [s]
  (URLEncoder/encode (str s) StandardCharsets/UTF_8))

(defn- decode-url
  [s]
  (URLDecoder/decode (str s) StandardCharsets/UTF_8))

(defn- parse-query
  [q]
  (if (str/blank? q)
    []
    (mapv (fn [part]
            (let [[k v] (str/split part #"=" 2)]
              [(decode-url k) (decode-url (or v ""))]))
      (str/split q #"&"))))

(defn- render-query
  [pairs]
  (when (seq pairs)
    (str/join "&" (map (fn [[k v]] (str (encode-url k) "=" (encode-url v))) pairs))))

(defn- upsert-query-param
  [pairs k v]
  (if (some #(= k (first %)) pairs)
    pairs
    (conj (vec pairs) [k v])))

(defn- uri-base
  [^URI uri query]
  (str (.getScheme uri) "://" (.getAuthority uri) (.getPath uri)
    (when (seq query) (str "?" query))
    (when-let [fragment (.getRawFragment uri)]
      (str "#" fragment))))

(defn- endpoint
  [{:keys [url tools api-key]}]
  (let [uri   (URI/create url)
        pairs (parse-query (.getRawQuery uri))
        pairs (cond-> pairs
                (and (seq tools) (not-any? #(= "tools" (first %)) pairs))
                (upsert-query-param "tools" (str/join "," tools))
                (and (not (str/blank? api-key))
                  (not-any? #(= "exaApiKey" (first %)) pairs))
                (upsert-query-param "exaApiKey" api-key))
        query (render-query pairs)]
    (uri-base uri query)))

(defn redact-endpoint
  "Redact `exaApiKey` query param from an endpoint string."
  [endpoint]
  (try
    (let [uri   (URI/create endpoint)
          pairs (mapv (fn [[k v]] [k (if (= k "exaApiKey") "REDACTED" v)])
                  (parse-query (.getRawQuery uri)))
          query (render-query pairs)]
      (uri-base uri query))
    (catch Throwable _ endpoint)))

(defn- lower-header-map
  [headers]
  (into {}
    (map (fn [[k v]] [(str/lower-case (name k)) (str v)]))
    (or headers {})))

(defn- send-http!
  [{:keys [url] :as req}]
  (let [resp (if *http-send-fn*
               (*http-send-fn* req)
               (http/post url (dissoc req :url)))]
    (-> resp
      (update :headers lower-header-map)
      (update :body #(str (or % ""))))))

(defn- json-request
  [endpoint timeout-ms payload]
  {:url endpoint
   :headers {"content-type" "application/json"
             "accept" "application/json, text/event-stream"}
   :body (json/write-json-str payload)
   :timeout (long timeout-ms)
   :throw false})

(defn- json-rpc-id [] (str "vis-exa-" (UUID/randomUUID)))

(defn- matching-json-rpc
  [payload id]
  (cond
    (and (map? payload) (= "2.0" (:jsonrpc payload))) payload
    (sequential? payload) (some #(when (and (map? %) (= id (:id %))) %) payload)
    :else nil))

(defn- parse-json-body
  [body]
  (json/read-json (or body "") :key-fn keyword))

(defn- parse-sse-body
  [body id]
  (->> (str/split-lines (or body ""))
    (keep (fn [line]
            (when (str/starts-with? (str/trim line) "data:")
              (let [data (str/trim (subs (str/trim line) 5))]
                (when (and (seq data) (not= data "[DONE]"))
                  (try
                    (json/read-json data :key-fn keyword)
                    (catch Throwable _ nil)))))))
    (some #(matching-json-rpc % id))))

(defn- response-content-type
  [{:keys [headers]}]
  (or (get headers "content-type")
    (get headers "Content-Type")
    ""))

(defn- parse-response
  [{:keys [status body] :as resp} id notification?]
  (cond
    (#{202 204} status) nil

    (or (nil? status) (< status 200) (>= status 300))
    (throw (ex-info (str "MCP HTTP " status ": " (subs (or body "") 0 (min 240 (count (or body "")))))
             {:type :search/mcp-http-error
              :status status}))

    notification? nil

    (str/includes? (response-content-type resp) "text/event-stream")
    (or (parse-sse-body body id)
      (throw (ex-info "MCP SSE response ended without matching result"
               {:type :search/mcp-sse-no-result
                :id id})))

    :else
    (or (matching-json-rpc (parse-json-body body) id)
      (throw (ex-info "Invalid MCP JSON-RPC response"
               {:type :search/mcp-invalid-response
                :id id})))))

(defn- send-json-rpc!
  [{:keys [endpoint timeout-ms method params notification?]}]
  (let [id      (when-not notification? (json-rpc-id))
        payload (cond-> {:jsonrpc "2.0"
                         :method method}
                  id (assoc :id id)
                  params (assoc :params params))
        resp    (send-http! (json-request endpoint timeout-ms payload))
        parsed  (parse-response resp id notification?)]
    (when-let [err (:error parsed)]
      (throw (ex-info (str "MCP error " (:code err) ": " (:message err))
               {:type :search/mcp-error
                :error err})))
    (:result parsed)))

(defn- initialize!
  [{:keys [protocol-version] :as cfg} endpoint]
  (send-json-rpc! {:endpoint endpoint
                   :timeout-ms (:timeout-ms cfg)
                   :method "initialize"
                   :params {:protocolVersion protocol-version
                            :capabilities {}
                            :clientInfo client-info}})
  (send-json-rpc! {:endpoint endpoint
                   :timeout-ms (:timeout-ms cfg)
                   :method "notifications/initialized"
                   :params {}
                   :notification? true}))

(defn- call-mcp-tool!
  [tool-name args]
  (let [cfg (effective-config)
        ep  (endpoint cfg)]
    (initialize! cfg ep)
    {:endpoint ep
     :result (send-json-rpc! {:endpoint ep
                              :timeout-ms (:timeout-ms cfg)
                              :method "tools/call"
                              :params {:name tool-name
                                       :arguments args}})}))

(defn- utf8-bytes
  [s]
  (alength (.getBytes (str s) StandardCharsets/UTF_8)))

(defn- take-under-byte-cap
  [lines max-bytes]
  (loop [out [] remaining (seq lines) used 0]
    (if-not remaining
      out
      (let [line (first remaining)
            extra (+ (utf8-bytes line) (if (seq out) 1 0))]
        (if (> (+ used extra) max-bytes)
          out
          (recur (conj out line) (next remaining) (+ used extra)))))))

(defn truncate-text
  "Bound `text` by line and UTF-8 byte limits. Returns truncation map."
  [text {:keys [max-bytes max-lines]}]
  (let [text        (str text)
        max-lines   (long (or max-lines default-max-lines))
        max-bytes   (long (or max-bytes default-max-bytes))
        all-lines   (str/split-lines text)
        line-cut    (vec (take max-lines all-lines))
        byte-cut    (take-under-byte-cap line-cut max-bytes)
        content     (str/join "\n" byte-cut)
        total-lines (count all-lines)
        total-bytes (utf8-bytes text)
        cut-lines   (count line-cut)
        out-lines   (count byte-cut)
        out-bytes   (utf8-bytes content)
        by-lines?   (> total-lines cut-lines)
        by-bytes?   (> cut-lines out-lines)]
    {:content content
     :truncated? (or by-lines? by-bytes?)
     :truncated-by (cond by-bytes? :bytes by-lines? :lines :else nil)
     :total-lines total-lines
     :total-bytes total-bytes
     :output-lines out-lines
     :output-bytes out-bytes
     :max-lines max-lines
     :max-bytes max-bytes}))

(defn- mcp-result->text
  [result]
  (let [blocks (:content result)]
    (if (seq blocks)
      (str/join "\n"
        (map (fn [block]
               (if (and (= "text" (:type block)) (string? (:text block)))
                 (:text block)
                 (json/write-json-str block)))
          blocks))
      (json/write-json-str result))))

(defn- effective-limits
  [opts]
  (let [cfg (effective-config)]
    {:max-bytes (min (positive-long (:max-bytes opts) (:max-bytes cfg))
                  (:max-bytes cfg))
     :max-lines (min (positive-long (:max-lines opts) (:max-lines cfg))
                  (:max-lines cfg))}))

(defn- parse-exa-text
  "Split Exa MCP's blob into a vec of per-result citation maps.
   Exa returns plain markdown with this per-entry header pattern:

     Title: <title>
     URL: <url>
     Published: <iso8601 or empty>
     Author: <author or N/A>
     Highlights:
     <markdown body>

     Title: <next entry>
     ...

   Each entry becomes:
     {:type    citation-type   ; :web | :code
      :title   string
      :url     string
      :excerpt markdown string ; the Highlights body, vis renders commonmark
      :published iso8601 string or nil
      :authors string or nil
      :source  :exa}"
  [^String text citation-type]
  (let [text (or text "")
        ;; Split on the leading `Title: ` boundary (start of file OR a fresh
        ;; entry after a blank line). Keep the prefix attached to each chunk.
        chunks (->> (str/split (str "\n" text) #"\nTitle: ")
                 rest)] ;; drop the empty pre-first-Title slice
    (vec
      (for [chunk chunks
            :let [lines (str/split-lines chunk)
                  title (str/trim (or (first lines) ""))
                  rest-lines (rest lines)
                  ;; Pull URL / Published / Author headers off the top
                  hdr-line (fn [pfx] (some #(when (str/starts-with? % pfx)
                                              (str/trim (subs % (count pfx))))
                                       rest-lines))
                  url (hdr-line "URL: ")
                  published (hdr-line "Published: ")
                  authors-raw (hdr-line "Author: ")
                  authors (when (and authors-raw
                                  (not (str/blank? authors-raw))
                                  (not= "N/A" authors-raw))
                            authors-raw)
                  ;; Excerpt = everything from the line AFTER "Highlights:"
                  ;; (or after the header bundle when no Highlights header)
                  excerpt-lines
                  (let [after-highlights
                        (drop 1
                          (drop-while
                            #(not (or (= % "Highlights:")
                                    (str/starts-with? % "Highlights:")))
                            rest-lines))]
                    (if (seq after-highlights)
                      after-highlights
                      ;; No Highlights line — drop the bare header lines
                      ;; (URL / Published / Author / Code-Highlights label)
                      ;; and keep the rest as the excerpt.
                      (drop-while #(re-matches #"^(URL|Published|Author|Code/Highlights):.*" %)
                        rest-lines)))
                  excerpt (str/trim (str/join "\n" excerpt-lines))]]
        (cond-> {:type    citation-type
                 :title   title
                 :url     (or url "")
                 :excerpt excerpt
                 :source  :exa}
          published (assoc :published published)
          authors   (assoc :authors authors))))))

(defn- kw-get
  [m & ks]
  (some #(when (contains? m %) (get m %)) ks))

(defn- web-args
  [query opts]
  (cond-> {:query query}
    (kw-get opts :num-results :numResults) (assoc :numResults (kw-get opts :num-results :numResults))
    (kw-get opts :type) (assoc :type (name (kw-get opts :type)))
    (kw-get opts :livecrawl :live-crawl) (assoc :livecrawl (name (kw-get opts :livecrawl :live-crawl)))
    (kw-get opts :context-max-characters :contextMaxCharacters)
    (assoc :contextMaxCharacters (kw-get opts :context-max-characters :contextMaxCharacters))))

(defn- code-args
  [query opts]
  (cond-> {:query query}
    (kw-get opts :tokens-num :tokensNum) (assoc :tokensNum (kw-get opts :tokens-num :tokensNum))))

;; ----------------------------------------------------------------------------
;; Envelope helpers
;;
;; Every search/* fn returns a tool envelope so the channel layer
;; (TUI / Telegram / web) gets a structured payload AND a custom
;; render-fn that paints citation cards instead of dumping the raw
;; markdown blob. SCI itself sees the unwrapped `:result` map.
;; ----------------------------------------------------------------------------

(defn- search-result-payload
  "Canonical SCI-facing :result map for a successful search call."
  [{:keys [op query citations source endpoint truncated?]}]
  (cond-> {:vis.op         op
           :query          (str query)
           :citations      (vec citations)
           :citation-count (count citations)
           :truncated?     (boolean truncated?)
           :source         source}
    endpoint (assoc :endpoint endpoint)))

(defn- search-success
  "Wrap a successful search call in the canonical tool envelope so it
   travels through `invoke-symbol-wrapper` the same way v/* tools do."
  [{:keys [op tool query citations source endpoint truncated?]}]
  (let [payload (search-result-payload
                  {:op op :query query :citations citations
                   :source source :endpoint endpoint :truncated? truncated?})]
    (extension/success
      {:result   payload
       :op       op
       :metadata (cond-> {:tool           (str tool)
                          :source         source
                          :citation-count (:citation-count payload)
                          :truncated?     (:truncated? payload)
                          :query          (str query)}
                   endpoint (assoc :endpoint endpoint))})))

(defn- search-failure
  "Failure envelope. Carries a single error-flagged citation on
   `:result` so model code that already destructures
   `(:citations r)` still sees the failure, AND a structured
   `:error` map so the channel renderer paints the fail card
   from the envelope side."
  [{:keys [op tool query source endpoint citation-type ^Throwable throwable]}]
  (let [msg          (or (some-> throwable ex-message) "search failed")
        error-entry  (cond-> {:type    citation-type
                              :title   (str "search failed: " query)
                              :url     ""
                              :excerpt msg
                              :source  source
                              :error   true}
                       (some-> throwable ex-data :type)
                       (assoc :error-type (-> throwable ex-data :type)))
        payload      (-> (search-result-payload
                           {:op op :query query :citations [error-entry]
                            :source source :endpoint endpoint :truncated? false})
                       (assoc :error? true))]
    (extension/failure
      {:result   payload
       :op       op
       :metadata (cond-> {:tool           (str tool)
                          :source         source
                          :citation-count 1
                          :error?         true
                          :query          (str query)}
                   endpoint (assoc :endpoint endpoint))
       :error    {:message msg
                  :reason  (or (some-> throwable ex-data :type) :search/call-failed)
                  :query   (str query)
                  :source  source}})))

(defn- call-exa!
  "Common path for `web` + `code`: call MCP, parse text → envelope.
   Returns an `extension/success` (or `extension/failure`) envelope
   so the wrapper produces channel-renderable structured output.

   `op` is the public op kw (`:search/web` / `:search/code`).
   `tool-name` is the Exa MCP tool string (`\"web_search_exa\"` etc.)."
  [op tool-name args citation-type query]
  (try
    (let [{:keys [endpoint result]} (call-mcp-tool! tool-name args)
          raw          (mcp-result->text result)
          {:keys [content truncated?]} (truncate-text raw (effective-limits {}))
          citations    (parse-exa-text content citation-type)
          redacted-ep  (some-> endpoint redact-endpoint)]
      (search-success
        {:op op :tool tool-name :query query :citations citations
         :source :exa :endpoint redacted-ep :truncated? truncated?}))
    (catch Throwable t
      (search-failure
        {:op op :tool tool-name :query query :source :exa
         :citation-type citation-type :throwable t}))))

(defn web
  "Live web search via Exa MCP. Returns the canonical tool envelope;
   SCI sees the unwrapped `:result` map:
     {:vis.op :search/web :query :citations [{:type :web :title :url
      :excerpt :published? :authors? :source :exa} …]
      :citation-count :truncated? :endpoint :source}

   `:excerpt` is markdown; the channel renderer parses it through
   `vis/markdown->ir` so commonmark blocks (headings, lists, code
   fences) render natively in the TUI / Telegram / web channels.
   Set `EXA_API_KEY` for higher rate limits.

   Opts (kwargs OR map; both accepted by SCI 1.11+ kwargs coercion):
     :num-results            — number of results to fetch
     :type                   — :auto | :fast | :deep
     :livecrawl              — :fallback | :preferred
     :context-max-characters — cap per-result content size"
  ([query] (web query {}))
  ([query opts]
   (call-exa! :search/web "web_search_exa" (web-args (str query) (or opts {}))
     :web (str query))))

(defn code
  "Live code/docs search via Exa MCP (`get_code_context_exa`). De facto
   indexes the public-web code surface — github repos, clojuredocs,
   readthedocs, official API references, etc. Most public code is
   github-hosted, so in practice this is \"github + docs\" results.
   For github-only queries, narrow the query string (e.g.
   `site:github.com X` or `<repo> X`).

   Returns the canonical tool envelope; SCI sees:
     {:vis.op :search/code :query :citations [{:type :code :title :url
      :excerpt :source :exa} …] :citation-count :truncated? :endpoint
      :source}
   `:excerpt` is markdown (commonmark-rendered by the channel layer).

   Opts: :tokens-num."
  ([query] (code query {}))
  ([query opts]
   (call-exa! :search/code "get_code_context_exa" (code-args (str query) (or opts {}))
     :code (str query))))

;; =============================================================================
;; arxiv papers (Atom feed)
;; =============================================================================

(def ^:private ARXIV_API_BASE "https://export.arxiv.org/api/query")
(def ^:private ARXIV_DEFAULT_MAX_RESULTS 10)
(def ^:private ARXIV_DEFAULT_TIMEOUT_MS 20000)

(defn- parse-arxiv-atom
  "Parse arxiv's Atom XML response into a vec of citation maps.
   `xml-bytes` is a UTF-8 byte array. clojure.xml is enough for
   arxiv's stable schema; no clojure.data.xml dep needed."
  [^bytes xml-bytes]
  (try
    (let [stream  (ByteArrayInputStream. xml-bytes)
          parsed  (xml/parse stream)
          entries (filter #(= :entry (:tag %)) (:content parsed))
          extract (fn [entry tag]
                    (some->> (:content entry)
                      (filter #(= tag (:tag %)))
                      first :content first
                      (#(when (string? %) (str/trim %)))))
          extract-author (fn [entry]
                           (some->> (:content entry)
                             (filter #(= :author (:tag %)))
                             first :content
                             (filter #(= :name (:tag %)))
                             first :content first
                             (#(when (string? %) (str/trim %)))))]
      (mapv (fn [e]
              {:type    :paper
               :title   (or (extract e :title) "")
               :url     (or (extract e :id) "")
               :excerpt (or (extract e :summary) "")
               :authors (or (extract-author e) "")
               :published (or (extract e :published) "")
               :source  :arxiv})
        entries))
    (catch Throwable t
      [{:type :paper
        :title "arxiv parse failed"
        :url ""
        :excerpt (or (ex-message t) "")
        :source :arxiv
        :error true}])))

(defn papers
  "arxiv paper search. Returns the canonical tool envelope; SCI sees:
     {:vis.op :search/papers :query :citations [{:type :paper :title
      :url :excerpt :authors :published :source :arxiv} …]
      :citation-count :truncated? :source}

   `:excerpt` is the arxiv abstract (plain text — still rendered
   through commonmark by the channel layer, which is a no-op for
   non-markdown content).

   Opts:
     :max-results  N    default 10
     :sort         :relevance|:lastUpdatedDate|:submittedDate (default :relevance)
     :timeout-ms   ms   default 20 000"
  ([query] (papers query {}))
  ([query opts]
   (let [{:keys [max-results sort timeout-ms]
          :or {max-results ARXIV_DEFAULT_MAX_RESULTS
               sort        :relevance
               timeout-ms  ARXIV_DEFAULT_TIMEOUT_MS}} opts
         url (str ARXIV_API_BASE
               "?search_query=" (URLEncoder/encode (str "all:" query) "UTF-8")
               "&start=0"
               "&max_results=" max-results
               "&sortBy=" (case sort
                            :lastUpdatedDate "lastUpdatedDate"
                            :submittedDate   "submittedDate"
                            :relevance       "relevance"
                            "relevance")
               "&sortOrder=descending")]
     (try
       (let [resp (http/get url {:timeout timeout-ms
                                 :headers {"User-Agent" "vis-foundation-search/0.1"}})
             body (:body resp)
             body-bytes (cond
                          (string? body) (.getBytes ^String body StandardCharsets/UTF_8)
                          (bytes? body) body
                          :else (.getBytes (str body) StandardCharsets/UTF_8))
             citations (parse-arxiv-atom body-bytes)]
         (search-success
           {:op :search/papers :tool "arxiv" :query query
            :citations citations :source :arxiv
            :endpoint url :truncated? false}))
       (catch Throwable t
         (search-failure
           {:op :search/papers :tool "arxiv" :query query
            :source :arxiv :endpoint url
            :citation-type :paper :throwable t}))))))

;; =============================================================================
;; Channel IR renderer
;;
;; Same structural-output story as the v/* tools: a small IR tree the
;; TUI / Telegram / web channels render directly. Each citation is a
;; \"card\" block:
;;
;;   **N.** [title](url)
;;   _meta-line_                ;; published / authors when present
;;   <parsed-markdown excerpt>  ;; commonmark blocks spliced in
;;
;; Errors show one card with a loud failure badge.
;; =============================================================================

(defn- ir-inline [x] (if (vector? x) x [:span {} (str x)]))
(defn- ir-p [& parts]
  (into [:p {}] (map ir-inline (filter some? parts))))
(defn- ir-strong [s] [:strong {} (str s)])
(defn- ir-em [s] [:em {} (str s)])
(defn- ir-link [url label]
  [:a {:href (str url)} (str (or label url))])
(defn- ir-hr [] [:hr {}])

(defn- non-blank-str
  [x]
  (let [s (cond
            (nil? x)     nil
            (keyword? x) (name x)
            :else        (str x))]
    (when (and s (not (str/blank? s))) s)))

(defn- citation-meta-line
  "Build the italic meta-line text. Empty string when no metadata."
  [{:keys [published authors source url]}]
  (->> [(when-let [s (non-blank-str source)]    s)
        (when-let [s (non-blank-str published)] (str "published " s))
        (when-let [s (non-blank-str authors)]   (str "by " s))
        (when-let [s (non-blank-str url)]       s)]
    (remove nil?)
    (str/join " · ")))

(defn- markdown-body-blocks
  "Parse `excerpt` markdown into IR blocks. Strips the outer `[:ir attrs ...]`
   wrapper so the blocks splice cleanly into our card. Blank input → nil."
  [excerpt]
  (when-let [ex (and (string? excerpt) (not-empty (str/trim excerpt)))]
    (try
      (let [parsed (vis/markdown->ir ex)]
        ;; markdown->ir always returns `[:ir {} & blocks]`; drop the
        ;; tag + attrs and keep the body so we can interleave with
        ;; our headline/meta paragraphs.
        (when (vector? parsed)
          (let [body (drop 2 parsed)]
            (seq (vec body)))))
      (catch Throwable _
        ;; Fall back to a fenced code block on parse failure so the
        ;; excerpt is still readable and the failure surfaces visibly.
        [[:code {} ex]]))))

(defn- citation-card
  "Render one citation as a sequence of IR blocks."
  [idx {:keys [title url error] :as citation}]
  (let [headline (cond
                   error      (ir-p (ir-strong (str (inc idx) ". "))
                                (ir-strong "⚠ ") (or title "failure"))
                   (str/blank? url)
                   (ir-p (ir-strong (str (inc idx) ". ")) (or title "(no title)"))
                   :else
                   (ir-p (ir-strong (str (inc idx) ". "))
                     (ir-link url (or title url))))
        meta-txt (citation-meta-line citation)
        meta-blk (when (and (not error) (seq meta-txt))
                   (ir-p (ir-em meta-txt)))
        body     (markdown-body-blocks (:excerpt citation))]
    (cond-> [headline]
      meta-blk         (conj meta-blk)
      (seq body)       (into body))))

(defn- search-badge-label
  [op]
  (case op
    :search/web    "SEARCH WEB"
    :search/code   "SEARCH CODE"
    :search/papers "SEARCH PAPERS"
    "SEARCH"))

(defn- search-summary
  "Zone summary for a search result: label on the left, the query in the
   center, the citation count (plus truncated/failed/source markers) anchored
   right. First `[:strong …]` of `:left` is the badge label by convention."
  [{op :vis.op :keys [query citations citation-count truncated? source error?]}]
  (let [n      (or citation-count (count citations))
        metric (str n " citation" (when (not= 1 n) "s")
                 (when truncated? " (truncated)")
                 (when error?     " ⚠ failed")
                 (when source     (str " · " (name source))))]
    (cond-> {:left  (ir-strong (search-badge-label op))
             :right metric}
      (not (str/blank? (str query)))
      (assoc :center [:c {} (str query)]))))

(defn- search-display
  "Full expanded IR body: a header paragraph echoing the query/count, then
   one card per citation (markdown-parsed excerpts), separated by rules."
  [{op :vis.op :keys [query citations citation-count truncated? source endpoint error?]}]
  (let [head   (ir-p (ir-strong (search-badge-label op))
                 "  " [:c {} (or query "")]
                 "  " (str (or citation-count (count citations)) " citation"
                        (when (not= 1 (or citation-count (count citations))) "s"))
                 (when truncated? "  (truncated)")
                 (when error?     "  ⚠ failed")
                 (when source     (str "  source=" (name source)))
                 (when endpoint   (str "  ep=" endpoint)))
        cards  (->> citations
                 (map-indexed citation-card)
                 (interpose [(ir-hr)])
                 (mapcat identity)
                 vec)
        blocks (cond-> [head]
                 (seq cards) (into cards))]
    (into [:ir {}] (filter some? blocks))))

(defn channel-render-search
  "Render a search result to the `{:summary :display}` contract. Reads the
   structured `:result` map (`{:vis.op :query :citations …}`) directly — no
   markdown round-trip on the result blob itself.

   `:summary` is a zone badge (label · query · citation count); `:display`
   is the full IR body with one markdown-parsed card per citation."
  [result]
  {:summary (search-summary result)
   :display (search-display result)})

;; =============================================================================
;; Symbol entries
;; =============================================================================

(def web-symbol
  (vis/symbol #'web
    {:tag          :observation
     :render-fn    channel-render-search}))

(def code-symbol
  (vis/symbol #'code
    {:tag          :observation
     :render-fn    channel-render-search}))

(def papers-symbol
  (vis/symbol #'papers
    {:tag          :observation
     :render-fn    channel-render-search}))

(def search-symbols
  [web-symbol
   code-symbol
   papers-symbol])

(def search-prompt
  "Live research tools bind directly in your sandbox: `search/web` (Exa MCP web search), `search/code` (Exa MCP code/doc context), and `search/papers` (arxiv). Call them inline like any other observation tool; each returns `{:citations [...] :citation-count N ...}`. Promote durable findings into a fact with `(fact-set! :K {...})`.")

(def search-env
  [{:name "EXA_API_KEY"
    :label "Exa API key"
    :description "Optional Exa key for higher MCP limits. Also read as EXA_MCP_API_KEY."
    :secret? true}
   {:name "EXA_MCP_API_KEY"
    :label "Exa MCP API key"
    :description "Alias for EXA_API_KEY. Use one, not both."
    :secret? true}
   {:name "EXA_MCP_URL"
    :label "Exa MCP URL"
    :description "Override Exa MCP endpoint URL."}
   {:name "EXA_MCP_TOOLS"
    :label "Exa MCP tools"
    :description "Comma-separated MCP tool allow-list."}
   {:name "EXA_MCP_TIMEOUT_MS"
    :label "Exa MCP timeout"
    :description "HTTP timeout in milliseconds."}
   {:name "EXA_MCP_PROTOCOL_VERSION"
    :label "Exa MCP protocol version"
    :description "MCP protocol version sent during initialize."}
   {:name "EXA_MCP_MAX_BYTES"
    :label "Exa max bytes"
    :description "Client-side maximum response bytes before truncation."}
   {:name "EXA_MCP_MAX_LINES"
    :label "Exa max lines"
    :description "Client-side maximum response lines before truncation."}
   {:name "EXA_MCP_CONFIG"
    :label "Exa MCP config file"
    :description "Optional JSON config file path. Lower priority than explicit vars."}])

;; `:tag :observation` carried INLINE on each `vis/symbol` opts map
;; above; register-extension! auto-populates the op registry.

(def vis-extension
  (vis/extension
    {:ext/name "foundation-search"
     :ext/description "Live research bindings under `search/`: web + code (Exa MCP) + papers (arxiv). Consult-scope only."
     :ext/version "0.1.0"
     :ext/author "Blockether"
     :ext/owner "vis"
     :ext/license "Apache-2.0"
     :ext/sci {:ext.sci/alias 'search
               :ext.sci/symbols search-symbols}
     :ext/kind "search"
     :ext/env search-env
     :ext/prompt search-prompt}))

(vis/register-extension! vis-extension)
