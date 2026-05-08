(ns com.blockether.vis.ext.exa.core
  "Exa MCP extension for Vis.

   Two model-facing tools under `exa/` backed by Exa's HTTP MCP endpoint.

   Important distinction: normal Exa REST APIs require `x-api-key`.
   The public MCP endpoint supports basic unauthenticated usage and an
   optional `exaApiKey` query parameter for higher limits. We never log
   or surface that key; redacted endpoints are used in info and
   errors."
  (:require
   [babashka.http-client :as http]
   [charred.api :as json]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [com.blockether.vis.core :as vis]
   [com.blockether.vis.internal.extension :as extension]
   [com.blockether.vis.internal.markdown :as md])
  (:import
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
  {:name "vis-exa-mcp-extension"
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
             {:type :exa/mcp-http-error
              :status status}))

    notification? nil

    (str/includes? (response-content-type resp) "text/event-stream")
    (or (parse-sse-body body id)
      (throw (ex-info "MCP SSE response ended without matching result"
               {:type :exa/mcp-sse-no-result
                :id id})))

    :else
    (or (matching-json-rpc (parse-json-body body) id)
      (throw (ex-info "Invalid MCP JSON-RPC response"
               {:type :exa/mcp-invalid-response
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
               {:type :exa/mcp-error
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

(defn- temp-output-file!
  [tool-name content]
  (let [safe (str/replace tool-name #"[^A-Za-z0-9_-]" "_")
        f    (io/file (System/getProperty "java.io.tmpdir")
               (str "vis-exa-mcp-" safe "-" (System/currentTimeMillis) ".txt"))]
    (spit f content)
    (.getAbsolutePath f)))

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

(defn- render-exa-result
  [{:keys [tool-result]}]
  (if-not (:success? tool-result)
    (let [op    (get-in tool-result [:info :op])
          query (get-in tool-result [:result :query])]
      (md/p "Exa" (md/code op) "failed for" (md/code query) ":"
        (or (get-in tool-result [:error :message]) (pr-str (:error tool-result)))))
    (let [{:keys [query content truncated? temp-file]} (:result tool-result)
          op (get-in tool-result [:info :op])]
      (md/join
        (md/p "Exa" (md/code op) ":" (md/code query))
        content
        (when truncated?
          (md/p "Output truncated; full output saved to" (md/code temp-file) "."))))))

(defn- tool-success
  [{:keys [tool-name op query args mcp endpoint limits]}]
  (let [raw       (mcp-result->text mcp)
        trunc     (truncate-text raw limits)
        temp-file (when (:truncated? trunc) (temp-output-file! tool-name raw))
        result    {:tool tool-name
                   :query query
                   :arguments args
                   :endpoint (redact-endpoint endpoint)
                   :content (:content trunc)
                   :mcp-error? (= true (:isError mcp))
                   :truncated? (:truncated? trunc)
                   :truncation (dissoc trunc :content)}
        result    (cond-> result temp-file (assoc :temp-file temp-file))]
    (extension/success
      {:result result
       :info {:op op
              :target {:kind :exa-mcp
                       :tool tool-name
                       :query query
                       :endpoint (redact-endpoint endpoint)}}})))

(defn- tool-failure
  [op tool-name query endpoint throwable]
  (extension/failure
    {:result {:tool tool-name
              :query query
              :endpoint (when endpoint (redact-endpoint endpoint))}
     :info {:op op
            :target {:kind :exa-mcp
                     :tool tool-name
                     :query query
                     :endpoint (when endpoint (redact-endpoint endpoint))}}
     :throwable throwable}))

(defn- kw-get
  [m & ks]
  (some #(when (contains? m %) (get m %)) ks))

(defn- web-search-args
  [query opts]
  (cond-> {:query query}
    (kw-get opts :num-results :numResults) (assoc :numResults (kw-get opts :num-results :numResults))
    (kw-get opts :type) (assoc :type (name (kw-get opts :type)))
    (kw-get opts :livecrawl :live-crawl) (assoc :livecrawl (name (kw-get opts :livecrawl :live-crawl)))
    (kw-get opts :context-max-characters :contextMaxCharacters)
    (assoc :contextMaxCharacters (kw-get opts :context-max-characters :contextMaxCharacters))))

(defn- code-context-args
  [query opts]
  (cond-> {:query query}
    (kw-get opts :tokens-num :tokensNum) (assoc :tokensNum (kw-get opts :tokens-num :tokensNum))))

(def ^:private tool-result-spec ::extension/tool-result)

(defn web-search
  "Search the web through Exa MCP. Basic use needs no key; set EXA_API_KEY for higher limits. Tool result; payload under :result."
  ([query] (web-search query {}))
  ([query opts]
   (let [query (str query)
         opts  (or opts {})
         args  (web-search-args query opts)
         cfg   (effective-config)
         ep    (endpoint cfg)]
     (try
       (let [{:keys [endpoint result]} (call-mcp-tool! "web_search_exa" args)]
         (tool-success {:tool-name "web_search_exa"
                        :op :exa/web-search
                        :label "Exa web search"
                        :query query
                        :args args
                        :mcp result
                        :endpoint endpoint
                        :limits (effective-limits opts)}))
       (catch Throwable t
         (tool-failure :exa/web-search "web_search_exa" query ep t))))))

(defn code-context
  "Search code/docs through Exa MCP for API usage/examples. Tool result; payload under :result."
  ([query] (code-context query {}))
  ([query opts]
   (let [query (str query)
         opts  (or opts {})
         args  (code-context-args query opts)
         cfg   (effective-config)
         ep    (endpoint cfg)]
     (try
       (let [{:keys [endpoint result]} (call-mcp-tool! "get_code_context_exa" args)]
         (tool-success {:tool-name "get_code_context_exa"
                        :op :exa/code-context
                        :label "Exa code context"
                        :query query
                        :args args
                        :mcp result
                        :endpoint endpoint
                        :limits (effective-limits opts)}))
       (catch Throwable t
         (tool-failure :exa/code-context "get_code_context_exa" query ep t))))))

(def web-search-symbol
  (vis/symbol #'web-search
    {:examples ["(def r (exa/web-search \"latest Clojure release\" {:num-results 5}))"
                "(get-in r [:result :content])"]
     :result-spec tool-result-spec
     :render-fn render-exa-result}))

(def code-context-symbol
  (vis/symbol #'code-context
    {:examples ["(def c (exa/code-context \"rewrite-clj z/find-value examples\" {:tokens-num 12000}))"
                "(get-in c [:result :content])"]
     :result-spec tool-result-spec
     :render-fn render-exa-result}))

(def exa-symbols
  [web-search-symbol
   code-context-symbol])

(def exa-prompt
  "`exa/` web search: (exa/web-search query opts?) for current web facts; (exa/code-context query opts?) for code/docs/API examples. Basic Exa MCP use needs no key; set EXA_API_KEY or EXA_MCP_API_KEY for higher limits. Opts: web-search supports :num-results, :type (:auto/:fast/:deep), :livecrawl (:fallback/:preferred), :context-max-characters, :max-bytes, :max-lines. code-context supports :tokens-num, :max-bytes, :max-lines. Tool results are envelopes; read payload with (get-in r [:result :content]), not (:content r).")

(def exa-env
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

(def vis-extension
  (vis/extension
    {:ext/namespace 'com.blockether.vis.ext.exa.core
     :ext/doc "Exa MCP search tools: web search and code/documentation context under `exa/`."
     :ext/version "0.1.0"
     :ext/author "Blockether"
     :ext/owner "vis"
     :ext/license "Apache-2.0"
     :ext/ns-alias {:ns 'vis.ext.exa :alias 'exa}
     :ext/kind "search"
     :ext/env exa-env
     :ext/prompt exa-prompt
     :ext/symbols exa-symbols}))

(vis/register-extension! vis-extension)
