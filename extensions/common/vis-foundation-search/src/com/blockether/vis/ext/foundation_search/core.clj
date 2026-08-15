(ns com.blockether.vis.ext.foundation-search.core
  "Research extension — NATIVE tools.

   Three model-facing bindings, bound BARE (native, no alias) in the
   sandbox alongside cat/rg/patch — same surface as the foundation
   kernel, no `;; -- EXTENSION search --` prompt block:

     search_web(query, opts?)    — web search via Exa MCP
     search_code(query, opts?)   — code/doc context via Exa MCP, GitHub fallback
     search_papers(query, opts?) — arxiv papers (Atom feed)

   Output shape — consistent with the rest of the `:tag :observation`
   tool surface (cat, ls, rg). Every search fn returns the
   canonical tool envelope; the agent sees the unwrapped `:result` map:

     {:op       :search-web|:search-code|:search-papers
      :query        \"…\"
      :citations    [{:type :title :url :excerpt :source …} …]
      :citation-count N
      :truncated?   B
      :source       :exa|:github|:arxiv
      :fallback-from :exa                    ;; only GitHub fallback results
      :endpoint     \"…REDACTED…\"   ;; web/code only
      :error?       true               ;; only on failure
      :error        {:message … …}}    ;; only on failure

   `:excerpt` is Markdown; the channel renderer parses it through
   `vis/markdown->ast` so channel renderers can present
   commonmark blocks (headings, lists, code fences) instead of dumping
   the raw blob. Errors are still surfaced inline as a single citation
   with `:error true` AND on the envelope's `:error` slot so failures
   stay visible no matter which lens the consumer reads.

   Exa MCP: the public endpoint supports basic unauthenticated use;
   set `EXA_API_KEY` (or `EXA_MCP_API_KEY`) for higher limits. The
   key is never logged or surfaced; the endpoint shown in results is
   redacted.

   arxiv: free public Atom feed via `https://export.arxiv.org/api/query`."
  (:require [babashka.http-client :as http]
            [charred.api :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.xml :as xml]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.internal.cancellation :as cancellation]
            [com.blockether.vis.internal.extension :as extension]
            [com.blockether.vis.internal.workspace :as workspace])
  (:import (java.io ByteArrayInputStream ByteArrayOutputStream)
           (java.net URI URLDecoder URLEncoder)
           (java.nio.charset StandardCharsets)
           (java.nio.file Files LinkOption Path)
           (java.nio.file.attribute FileAttribute)
           (java.security MessageDigest)
           (java.util UUID)
           (java.util.concurrent TimeUnit)
           (java.util.zip GZIPInputStream)
           (org.apache.commons.compress.archivers.tar TarArchiveInputStream)))

(def ^:private default-endpoint "https://mcp.exa.ai/mcp")

(def ^:private default-timeout-ms 30000)

(def ^:private default-protocol-version "2025-06-18")

(def ^:private default-max-bytes 51200)

(def ^:private default-max-lines 2000)

(def ^:private default-tools ["web_search_exa" "get_code_context_exa"])

(def ^:private client-info {:name "vis-search-extension" :version "1.0.0"})

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

(def ^:dynamic *github-get-fn* "Test seam for GitHub REST and codeload GET requests." nil)

(def ^:dynamic *github-token-fn* "Test seam for retrieving the token held by the GitHub CLI." nil)

(defn- env [k] (not-empty (str/trim (or (vis/extension-env-value k) ""))))

(defn- parse-long*
  [x]
  (cond (integer? x) (long x)
        (number? x) (long x)
        (string? x) (try (Long/parseLong (str/trim x)) (catch Throwable _ nil))
        :else nil))

(defn- positive-long
  [x fallback]
  (let [n (parse-long* x)]
    (if (and n (pos? (long n))) n fallback)))

(defn- normalize-string [x] (when (string? x) (not-empty (str/trim x))))

(defn- env-token-name
  "Return var name for config strings shaped `$NAME`, `${NAME}`, or
   `env:NAME`. This lets extension JSON stay shareable without
   hard-coding secrets."
  [s]
  (when-let [s (normalize-string s)]
    (cond (str/starts-with? s "env:") (not-empty (subs s 4))
          (and (str/starts-with? s "${") (str/ends-with? s "}")) (not-empty
                                                                   (subs s 2 (dec (count s))))
          (and (str/starts-with? s "$") (> (count s) 1)) (not-empty (subs s 1))
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
    (let
      [tools (->> (str/split s #",")
                  (map str/trim)
                  (remove str/blank?)
                  vec)]
      (when (seq tools) tools))))

(defn- normalize-tools
  [x]
  (cond (string? x) (split-tools x)
        (sequential? x) (let
                          [tools (->> x
                                      (keep normalize-string)
                                      vec)]
                          (when (seq tools) tools))
        :else nil))

(defn- home-path
  [path]
  (let [home (System/getProperty "user.home")]
    (cond (str/starts-with? path "~/") (str home "/" (subs path 2))
          (str/starts-with? path "~") (str home (subs path 1))
          :else path)))

(defn- config-candidates
  []
  (if-let [path (env "EXA_MCP_CONFIG")]
    [(home-path path)]
    []))

(defn- read-json-file
  [path]
  (let [f (io/file path)]
    (when (.exists f) (try (json/read-json (slurp f) :key-fn keyword) (catch Throwable _ nil)))))

(defn- config-from-file [] (some read-json-file (config-candidates)))

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
          (env "EXA_MCP_URL")
          (assoc :url (env "EXA_MCP_URL"))

          (env "EXA_MCP_TOOLS")
          (assoc :tools (or (split-tools (env "EXA_MCP_TOOLS")) default-tools))

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

(defn- encode-url [s] (URLEncoder/encode (str s) StandardCharsets/UTF_8))

(defn- decode-url [s] (URLDecoder/decode (str s) StandardCharsets/UTF_8))

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
    (str/join "&"
              (map (fn [[k v]]
                     (str (encode-url k) "=" (encode-url v)))
                   pairs))))

(defn- upsert-query-param
  [pairs k v]
  (if (some #(= k (first %)) pairs) pairs (conj (vec pairs) [k v])))

(defn- uri-base
  [^URI uri query]
  (str (.getScheme uri)
       "://"
       (.getAuthority uri)
       (.getPath uri)
       (when (seq query) (str "?" query))
       (when-let [fragment (.getRawFragment uri)]
         (str "#" fragment))))

(defn- endpoint
  [{:keys [url tools api-key]}]
  (let
    [uri
     (URI/create url)

     pairs
     (parse-query (.getRawQuery uri))

     pairs
     (cond-> pairs
       (and (seq tools) (not-any? #(= "tools" (first %)) pairs))
       (upsert-query-param "tools" (str/join "," tools))

       (and (not (str/blank? api-key)) (not-any? #(= "exaApiKey" (first %)) pairs))
       (upsert-query-param "exaApiKey" api-key))

     query
     (render-query pairs)]

    (uri-base uri query)))

(defn redact-endpoint
  "Redact `exaApiKey` query param from an endpoint string."
  [endpoint]
  (try (let
         [uri
          (URI/create endpoint)

          pairs
          (mapv (fn [[k v]]
                  [k (if (= k "exaApiKey") "REDACTED" v)])
                (parse-query (.getRawQuery uri)))

          query
          (render-query pairs)]

         (uri-base uri query))
       (catch Throwable _ endpoint)))

(defn- lower-header-map
  [headers]
  (into {}
        (map (fn [[k v]]
               [(str/lower-case (name k)) (str v)]))
        (or headers {})))

(defn- send-http!
  [{:keys [url] :as req}]
  (let [resp (if *http-send-fn* (*http-send-fn* req) (http/post url (dissoc req :url)))]
    (-> resp
        (update :headers lower-header-map)
        (update :body #(str (or % ""))))))

(defn- json-request
  [endpoint timeout-ms payload]
  {:url endpoint
   :headers {"content-type" "application/json" "accept" "application/json, text/event-stream"}
   :body (json/write-json-str payload)
   :timeout (long timeout-ms)
   :throw false})

(defn- json-rpc-id [] (str "vis-exa-" (UUID/randomUUID)))

(defn- matching-json-rpc
  [payload id]
  (cond (and (map? payload) (= "2.0" (:jsonrpc payload))) payload
        (sequential? payload) (some #(when (and (map? %) (= id (:id %))) %) payload)
        :else nil))

(defn- parse-json-body [body] (json/read-json (or body "") :key-fn keyword))

(defn- parse-sse-body
  [body id]
  (->> (str/split-lines (or body ""))
       (keep (fn [line]
               (when (str/starts-with? (str/trim line) "data:")
                 (let [data (str/trim (subs (str/trim line) 5))]
                   (when (and (seq data) (not= data "[DONE]"))
                     (try (json/read-json data :key-fn keyword) (catch Throwable _ nil)))))))
       (some #(matching-json-rpc % id))))

(defn- response-content-type
  [{:keys [headers]}]
  (or (get headers "content-type") (get headers "Content-Type") ""))

(defn- parse-response
  [{:keys [status body] :as resp} id notification?]
  (cond (#{202 204} status) nil
        (or (nil? status) (< (long status) 200) (>= (long status) 300))
        (throw (ex-info (str "MCP HTTP " status
                             ": " (subs (or body "") 0 (min 240 (count (or body "")))))
                        {:type :search/mcp-http-error :status status}))
        notification? nil
        (str/includes? (response-content-type resp) "text/event-stream")
        (or (parse-sse-body body id)
            (throw (ex-info "MCP SSE response ended without matching result"
                            {:type :search/mcp-sse-no-result :id id})))
        :else (or (matching-json-rpc (parse-json-body body) id)
                  (throw (ex-info "Invalid MCP JSON-RPC response"
                                  {:type :search/mcp-invalid-response :id id})))))

(defn- send-json-rpc!
  [{:keys [endpoint timeout-ms method params notification?]}]
  (let
    [id
     (when-not notification? (json-rpc-id))

     payload
     (cond-> {:jsonrpc "2.0" :method method}
       id
       (assoc :id id)

       params
       (assoc :params params))

     resp
     (send-http! (json-request endpoint timeout-ms payload))

     parsed
     (parse-response resp id notification?)]

    (when-let [err (:error parsed)]
      (throw (ex-info (str "MCP error " (:code err) ": " (:message err))
                      {:type :search/mcp-error :error err})))
    (:result parsed)))

(defn- initialize!
  [{:keys [protocol-version] :as cfg} endpoint]
  (send-json-rpc! {:endpoint endpoint
                   :timeout-ms (:timeout-ms cfg)
                   :method "initialize"
                   :params
                   {:protocolVersion protocol-version :capabilities {} :clientInfo client-info}})
  (send-json-rpc! {:endpoint endpoint
                   :timeout-ms (:timeout-ms cfg)
                   :method "notifications/initialized"
                   :params {}
                   :notification? true}))

(defn- call-mcp-tool!
  [tool-name args]
  (let
    [cfg
     (effective-config)

     ep
     (endpoint cfg)]

    (initialize! cfg ep)
    {:endpoint ep
     :result (send-json-rpc! {:endpoint ep
                              :timeout-ms (:timeout-ms cfg)
                              :method "tools/call"
                              :params {:name tool-name :arguments args}})}))

(defn- utf8-bytes [s] (alength (.getBytes (str s) StandardCharsets/UTF_8)))

(defn- take-under-byte-cap
  [lines max-bytes]
  (loop
    [out
     []

     remaining
     (seq lines)

     used
     0]

    (if-not remaining
      out
      (let
        [line
         (first remaining)

         extra
         (+ (long (utf8-bytes line)) (if (seq out) 1 0))]

        (if (> (+ used extra) (long max-bytes))
          out
          (recur (conj out line) (next remaining) (+ used extra)))))))

(defn truncate-text
  "Bound `text` by line and UTF-8 byte limits. Returns truncation map."
  [text {:keys [max-bytes max-lines]}]
  (let
    [text
     (str text)

     max-lines
     (long (or max-lines default-max-lines))

     max-bytes
     (long (or max-bytes default-max-bytes))

     all-lines
     (str/split-lines text)

     line-cut
     (vec (take max-lines all-lines))

     byte-cut
     (take-under-byte-cap line-cut max-bytes)

     content
     (str/join "\n" byte-cut)

     total-lines
     (count all-lines)

     total-bytes
     (utf8-bytes text)

     cut-lines
     (count line-cut)

     out-lines
     (count byte-cut)

     out-bytes
     (utf8-bytes content)

     by-lines?
     (> total-lines cut-lines)

     by-bytes?
     (> cut-lines out-lines)]

    {:content content
     :truncated? (or by-lines? by-bytes?)
     :truncated-by (cond by-bytes? :bytes
                         by-lines? :lines
                         :else nil)
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
    {:max-bytes (min (long (positive-long (:max-bytes opts) (:max-bytes cfg)))
                     (long (:max-bytes cfg)))
     :max-lines (min (long (positive-long (:max-lines opts) (:max-lines cfg)))
                     (long (:max-lines cfg)))}))

(def ^:private exa-bracket-marker-re
  "A line that is ONLY Exa's bracketed `[...]` separator. Unambiguously a
   fragment boundary — never valid source — so it folds inline even
   inside a (often unterminated) ``` fence Exa wrapped the body in."
  #"^\s*\[\.\.\.\]\s*$")

(def ^:private exa-bare-marker-re
  "A line that is ONLY a bare `...` / `…`. Treated as a separator only in
   prose (outside fences), where a lone ellipsis line can't be real code."
  #"^\s*(?:\.\.\.|…)\s*$")

(def ^:private code-fence-re #"^\s*```")

(defn- kw->snake
  "Keyword -> snake_case string, mirroring the Clojure->Python boundary
   (`env-python/kw->snake`): kebab -> snake, trailing `?`/`!` stripped,
   namespace folded with `_`. Used to stringify the enum values (`:op`,
   `:source`, citation `:type`) the model-facing `:result` payload carries,
   so those maps cross the STRINGS-ONLY boundary already string-clean."
  ^String [k]
  (-> (if (namespace k) (str (namespace k) "_" (name k)) (name k))
      (str/replace "-" "_")
      (str/replace #"[?!]$" "")))

(defn- normalize-exa-excerpt
  "Exa stitches non-contiguous page fragments together with a bare
   `[...]` truncation marker on its OWN line. Between block-level
   neighbours (a heading and a code fence, two list blocks, …)
   CommonMark turns that lone marker into its own paragraph, so it
   paints on an empty line by itself — and reads the same way in the
   model-facing `:excerpt` text. (In free prose CommonMark already
   soft-joins it inline, which is why the behaviour looks inconsistent.)

   Fold every standalone marker into the END of the nearest preceding
   content line so the ellipsis always stays inline. Constraints:
     - A bracketed `[...]` folds anywhere (incl. inside Exa's spurious
       ``` body wrapper).
     - A bare `...` / `…` folds inline in PROSE; inside a fence it is
       dropped ONLY when it abuts the opening ``` (Exa's lead-truncation
       marker) — a genuine mid-code `...` (e.g. a Python `Ellipsis`
       stub body) is left untouched.
     - The backward fold scan stops at a fence delimiter, so a marker is
       never fused onto a ``` line; if it abuts one it is dropped.
     - A leading marker with no content to attach to is dropped.
     - Consecutive markers collapse (a line already ending in the marker
       is not doubled)."
  [excerpt]
  (let
    [lines
     (str/split-lines (or excerpt ""))

     marker
     " [...]"]

    (loop
      [ls
       lines

       in-fence?
       false

       out
       []]

      (if (empty? ls)
        (str/join "\n" out)
        (let
          [ln
           (first ls)

           fence?
           (re-find code-fence-re ln)

           bracket?
           (and (not fence?) (re-matches exa-bracket-marker-re ln))

           bare?
           (and (not fence?) (re-matches exa-bare-marker-re ln))

           ;; Nearest non-blank content line above, or nil when a fence
           ;; delimiter / the start is hit first (marker abuts a fence).
           prev
           (when (or bracket? bare?)
             (loop [i (dec (count out))]
               (when (>= i 0)
                 (let [s (nth out i)]
                   (cond (str/blank? s) (recur (dec i))
                         (re-find code-fence-re s) nil
                         :else i)))))]

          (cond
            ;; `[...]` anywhere, or a bare ellipsis in prose → fold inline
            ;; (or drop when it abuts a fence / the start).
            (or bracket? (and bare? (not in-fence?)))
            (recur (rest ls)
                   in-fence?
                   (cond-> out
                     (and prev (not (str/ends-with? (nth out prev) marker)))
                     (update prev str marker)))
            ;; Bare ellipsis right after an opening fence → Exa lead
            ;; marker → drop (prev is nil ⇒ the only thing above is the
            ;; opening ```). A mid-code `...` has real content above and
            ;; falls through to :else, preserved verbatim.
            (and bare? in-fence? (nil? prev)) (recur (rest ls) in-fence? out)
            :else (recur (rest ls) (if fence? (not in-fence?) in-fence?) (conj out ln))))))))

(def ^:private fence-open-re
  "A ``` fence delimiter, capturing the language tag (group 1, blank for a
   bare fence)."
  #"^\s*```(\S*)\s*$")

(defn- doc-prose?
  "True when a fenced body is documentation PROSE (headings, bullet lists,
   sentences) rather than source code. Conservative on purpose — it gates
   `unwrap-doc-fences`, and a false positive would render real code as
   markdown (mangling `+`/`-`/`#` prefixes). Requires a genuine prose
   signal (a sentence or ≥2 bullets), ≥60% doc-shaped lines, and NO `+`
   diff lines. The trailing ` [...]` that `normalize-exa-excerpt` folds on
   is stripped first so it doesn't hide a sentence's terminal period."
  [body-lines]
  (let
    [strip
     (fn [s]
       (str/replace s #"\s*\[\.\.\.\]\s*$" ""))

     lines
     (map strip (remove str/blank? body-lines))

     n
     (count lines)

     bul
     (count (filter #(re-find #"^\s*[-*]\s+\S" %) lines))

     hd
     (count (filter #(re-find #"^#{1,6}\s+\S" %) lines))

     plus
     (count (filter #(re-find #"^\s*\+\s" %) lines))

     sent
     (count (filter #(and (> (count %) 55) (re-find #"\.\s*$" %)) lines))]

    (and (>= n 3) (zero? plus) (or (>= sent 1) (>= bul 2)) (>= (+ bul hd sent) (* 0.6 n)))))

(defn- unwrap-doc-fences
  "Exa sometimes wraps a result's body PROSE in a bare ``` fence and never
   closes it, so CommonMark paints the whole entry as one mono code block
   (and the model reads it as code). Strip the opening delimiter of a
   DANGLING (unterminated) BARE fence whose body is `doc-prose?`, letting
   the headings / lists / paragraphs render natively.

   Deliberately narrow — only the trailing unclosed bare fence is a
   candidate, so language-tagged fences (```clj …), balanced fences, and
   real-code bodies (a YAML manifest, SQL, shell) are all left intact.
   Runs AFTER `normalize-exa-excerpt` so folded `[...]` markers don't
   dilute the prose ratio."
  [excerpt]
  (let [lines (vec (str/split-lines (or excerpt "")))]
    (loop
      [i 0
       open nil]

      (if (>= i (count lines))
        (if (and open
                 (str/blank? (:lang open))
                 (doc-prose? (subvec lines (inc (long (:idx open))))))
          (str/join "\n"
                    (into (subvec lines 0 (:idx open)) (subvec lines (inc (long (:idx open))))))
          excerpt)
        (let [m (re-matches fence-open-re (nth lines i))]
          (cond (and m (nil? open)) (recur (inc i) {:idx i :lang (second m)})
                (and m open) (recur (inc i) nil)
                :else (recur (inc i) open)))))))

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
  (let
    [text
     (or text "")

     ;; Split on the leading `Title: ` boundary (start of file OR a fresh
     ;; entry after a blank line). Keep the prefix attached to each chunk.
     chunks
     (->> (str/split (str "\n" text) #"\nTitle: ")
          rest)]

    ;; drop the empty pre-first-Title slice
    (vec
      (for
        [chunk
         chunks

         :let [lines
               (str/split-lines chunk)

               title
               (str/trim (or (first lines) ""))

               rest-lines
               (rest lines)

               ;; Pull URL / Published / Author headers off the top
               hdr-line
               (fn [pfx]
                 (some #(when (str/starts-with? % pfx) (str/trim (subs % (count pfx)))) rest-lines))

               url
               (hdr-line "URL: ")

               published
               (hdr-line "Published: ")

               authors-raw
               (hdr-line "Author: ")

               authors
               (when (and authors-raw (not (str/blank? authors-raw)) (not= "N/A" authors-raw))
                 authors-raw)

               ;; Excerpt = everything from the line AFTER "Highlights:"
               ;; (or after the header bundle when no Highlights header)
               excerpt-lines
               (let
                 [after-highlights (drop 1
                                         (drop-while #(not (or (= % "Highlights:")
                                                               (str/starts-with? % "Highlights:")))
                                                     rest-lines))]
                 (if (seq after-highlights)
                   after-highlights
                   ;; No Highlights line — drop the bare header lines
                   ;; (URL / Published / Author / Code-Highlights label)
                   ;; and keep the rest as the excerpt.
                   (drop-while #(re-matches #"^(URL|Published|Author|Code/Highlights):.*" %)
                               rest-lines)))

               excerpt
               (-> (str/trim (str/join "\n" excerpt-lines))
                   normalize-exa-excerpt
                   unwrap-doc-fences)]]

        (cond->
          {"type" (kw->snake citation-type)
           "title" title
           "url" (or url "")
           "excerpt" excerpt
           "source" "exa"}
          published
          (assoc "published" published)

          authors
          (assoc "authors" authors))))))

;; READ side: `opts` arrives from the model as a STRING-keyed dict (native tool
;; input). Read the ONE canonical snake_case key per option — no camelCase
;; fallback, no dual-read, no alias tolerance.
;;
;; WRITE side: the map returned here is Exa's `web_search_exa` / `get_code_context_exa`
;; MCP tool-argument object. `:numResults` / `:contextMaxCharacters` / `:tokensNum`
;; are Exa's OWN camelCase HTTP API field names (charred serializes this to the
;; JSON-RPC `arguments` payload) — a third-party wire contract, NOT vis aliases.
;; They never cross the GraalPy boundary; renaming them to snake_case would make
;; Exa silently ignore the option.
(defn- web-args
  [query opts]
  (cond-> {:query query}
    (get opts "num_results")
    (assoc :numResults (get opts "num_results"))

    (get opts "type")
    (assoc :type (str (get opts "type")))

    (get opts "livecrawl")
    (assoc :livecrawl (str (get opts "livecrawl")))

    (get opts "context_max_characters")
    (assoc :contextMaxCharacters (get opts "context_max_characters"))))

(defn- code-args
  [query opts]
  (cond-> {:query query}
    (get opts "tokens_num")
    (assoc :tokensNum (get opts "tokens_num"))))

;; ----------------------------------------------------------------------------
;; Envelope helpers
;;
;; Every search/* fn returns a tool envelope so the channel layer
;; gets a structured payload AND a custom
;; render-fn that paints citation cards instead of dumping the raw
;; markdown blob. Python itself sees the unwrapped `:result` map.
;; ----------------------------------------------------------------------------

(defn- search-result-payload
  "Canonical Python-facing :result map for a successful search call.
   STRINGS-ONLY: string keys, enum values (`op`, `source`) snake-cased so the
   map crosses the boundary already string-clean."
  [{:keys [op query citations source endpoint truncated? fallback-from]}]
  (cond->
    {"op" (kw->snake op)
     "query" (str query)
     "citations" (vec citations)
     "citation_count" (count citations)
     "truncated" (boolean truncated?)
     "source" (kw->snake source)}
    endpoint
    (assoc "endpoint" endpoint)

    fallback-from
    (assoc "fallback_from" (kw->snake fallback-from))))

(defn- search-success
  "Wrap a successful search call in the canonical tool envelope so it
   travels through `invoke-symbol-wrapper` the same way v/* tools do."
  [{:keys [op tool query citations source endpoint truncated? fallback-from]}]
  (let
    [payload (search-result-payload {:op op
                                     :query query
                                     :citations citations
                                     :source source
                                     :endpoint endpoint
                                     :truncated? truncated?
                                     :fallback-from fallback-from})]
    (extension/success {:result payload
                        :op op
                        :metadata (cond->
                                    {:tool (str tool)
                                     :source source
                                     :citation-count (get payload "citation_count")
                                     :truncated? (get payload "truncated")
                                     :query (str query)}
                                    endpoint
                                    (assoc :endpoint endpoint)

                                    fallback-from
                                    (assoc :fallback-from fallback-from))})))

(defn- search-failure
  "Failure envelope. Carries a single error-flagged citation on
   `:result` so model code that already destructures
   `(:citations r)` still sees the failure, AND a structured
   `:error` map so the channel renderer paints the fail card
   from the envelope side."
  [{:keys [op tool query source endpoint citation-type ^Throwable throwable]}]
  (let
    [msg
     (or (some-> throwable
                 ex-message)
         "search failed")

     error-entry
     (cond->
       {"type" (kw->snake citation-type)
        "title" (str "search failed: " query)
        "url" ""
        "excerpt" msg
        "source" (kw->snake source)
        "error" true}
       (some-> throwable
               ex-data
               :type)
       (assoc "error_type"
         (kw->snake (-> throwable
                        ex-data
                        :type))))

     payload
     (-> (search-result-payload {:op op
                                 :query query
                                 :citations [error-entry]
                                 :source source
                                 :endpoint endpoint
                                 :truncated? false})
         (assoc "error" true))]

    (extension/failure
      {:result payload
       :op op
       :metadata
       (cond-> {:tool (str tool) :source source :citation-count 1 :error? true :query (str query)}
         endpoint
         (assoc :endpoint endpoint))
       :error {:message msg
               :reason (or (some-> throwable
                                   ex-data
                                   :type)
                           :search/call-failed)
               :query (str query)
               :source source}})))

(defn- retryable-exa-error?
  "True only for Exa quota and server failures. Authentication, malformed
   requests, and MCP protocol errors remain visible instead of being masked."
  [t]
  (let [{:keys [type status]} (ex-data t)]
    (and (= :search/mcp-http-error type) (or (= 429 status) (<= 500 (long (or status 0)) 599)))))

(def ^:private github-code-endpoint "https://api.github.com/search/code")

(defn- github-cli-token
  "Read the token from a logged-in `gh` CLI without exposing it in config or logs.
   Returns nil when the CLI is missing, unauthenticated, or too slow."
  []
  (try (let
         [process
          (.start (doto (ProcessBuilder. ^"[Ljava.lang.String;"
                                         (into-array String ["gh" "auth" "token"]))
                    (.redirectError java.lang.ProcessBuilder$Redirect/DISCARD)))

          output
          (future (slurp (io/reader (.getInputStream process))))

          finished?
          (.waitFor process 2 TimeUnit/SECONDS)]

         (if (and finished? (zero? (.exitValue process)))
           (not-empty (str/trim (deref output 1000 "")))
           (do (.destroyForcibly process) nil)))
       (catch Throwable t (cancellation/preserve-interrupt! t) nil)))

(defn- github-request
  [query opts]
  (let [token ((or *github-token-fn* github-cli-token))]
    (when-not token
      (throw
        (ex-info
          "GitHub Code Search requires an authenticated GitHub CLI. Run `gh auth login` and grant it access to the repositories you need."
          {:type :search/github-auth-required})))
    (let
      [per-page (min 100 (long (positive-long (get opts "num_results") 10)))
       url (str github-code-endpoint "?q=" (encode-url query) "&per_page=" per-page)
       headers {"accept" "application/vnd.github.text-match+json"
                "authorization" (str "Bearer " token)
                "user-agent" "vis-foundation-search/0.1"}
       response (if *github-get-fn*
                  (*github-get-fn* url {:headers headers :throw false :timeout default-timeout-ms})
                  (http/get url {:headers headers :throw false :timeout default-timeout-ms}))
       status (:status response)
       body (str (or (:body response) ""))]

      (when (or (nil? status) (< (long status) 200) (>= (long status) 300))
        (throw (ex-info (str "GitHub HTTP " status ": " (subs body 0 (min 240 (count body))))
                        {:type :search/github-http-error :status status})))
      (json/read-json body :key-fn keyword))))

(defn- github-citations
  [response citation-type]
  (mapv (fn [{:keys [name path html_url repository text_matches]}]
          (let
            [repo-name
             (or (get repository :full_name) "GitHub")

             fragments
             (keep :fragment text_matches)

             excerpt
             (if (seq fragments)
               (str/join "\n\n" fragments)
               (str "GitHub code result: " repo-name "/" (or path name "")))]

            {"type" (kw->snake citation-type)
             "title" (str repo-name "/" (or path name ""))
             "url" (or html_url "")
             "excerpt" excerpt
             "source" "github"}))
        (or (:items response) [])))

(defn- search-github-code!
  ([op tool query opts citation-type] (search-github-code! op tool query opts citation-type nil))
  ([op tool query opts citation-type fallback-from]
   (let
     [response
      (github-request query opts)

      citations
      (github-citations response citation-type)]

     (search-success {:op op
                      :tool tool
                      :query query
                      :citations citations
                      :source :github
                      :endpoint github-code-endpoint
                      :truncated? false
                      :fallback-from fallback-from}))))

(defn- call-exa!
  "Common path for `web` + `code`: call MCP, parse text → envelope.
   An optional fallback receives only retryable Exa HTTP errors. Its failure is
   reported directly so callers see remediation such as GitHub authentication."
  ([op tool-name args citation-type query] (call-exa! op tool-name args citation-type query nil))
  ([op tool-name args citation-type query fallback]
   (try
     (let
       [{:keys [endpoint result]}
        (call-mcp-tool! tool-name args)

        raw
        (mcp-result->text result)

        {:keys [content truncated?]}
        (truncate-text raw (effective-limits {}))

        citations
        (parse-exa-text content citation-type)

        redacted-ep
        (some-> endpoint
                redact-endpoint)]

       (search-success {:op op
                        :tool tool-name
                        :query query
                        :citations citations
                        :source :exa
                        :endpoint redacted-ep
                        :truncated? truncated?}))
     (catch Throwable exa-error
       (if (and fallback (retryable-exa-error? exa-error))
         (try (fallback exa-error)
              (catch Throwable fallback-error
                (search-failure {:op op
                                 :tool tool-name
                                 :query query
                                 :source :github
                                 :endpoint github-code-endpoint
                                 :citation-type citation-type
                                 :throwable fallback-error})))
         (search-failure {:op op
                          :tool tool-name
                          :query query
                          :source :exa
                          :citation-type citation-type
                          :throwable exa-error}))))))

(defn search-web
  "await search_web(\"rust async runtime comparison\")
   await search_web(\"…\", {\"num_results\": 5, \"type\": \"auto\", \"livecrawl\": \"preferred\", \"context_max_characters\": N})

   Live web search via Exa."
  ([query] (search-web query {}))
  ([query opts]
   (call-exa! :search-web "web_search_exa" (web-args (str query) (or opts {})) :web (str query))))

(defn search-code
  "await search_code(\"clojure core.async go-loop example\")
   await search_code(\"…\", {\"provider\": \"github\", \"tokens_num\": N})

   Code/docs search. `provider` selects `auto` (default: Exa then GitHub on Exa
   quota/server failures), `exa`, or `github`. GitHub Code Search requires a
   logged-in GitHub CLI; on a missing login the failure tells the user to run
   `gh auth login` rather than attempting an unsupported anonymous search."
  ([query] (search-code query {}))
  ([query opts]
   (let
     [query
      (str query)

      opts
      (or opts {})

      provider
      (str (or (get opts "provider") "auto"))

      provider-opts
      (dissoc opts "provider")]

     (case provider
       "auto"
       (call-exa!
         :search-code
         "get_code_context_exa"
         (code-args query provider-opts)
         :code
         query
         (fn [_]
           (search-github-code! :search-code "github-code-search" query provider-opts :code :exa)))

       "exa"
       (call-exa! :search-code "get_code_context_exa" (code-args query provider-opts) :code query)

       "github"
       (try (search-github-code! :search-code "github-code-search" query provider-opts :code)
            (catch Throwable t
              (search-failure {:op :search-code
                               :tool "github-code-search"
                               :query query
                               :source :github
                               :endpoint github-code-endpoint
                               :citation-type :code
                               :throwable t})))

       (search-failure {:op :search-code
                        :tool "search-code"
                        :query query
                        :source :exa
                        :citation-type :code
                        :throwable (ex-info (str "unknown code search provider "
                                                 (pr-str provider)
                                                 " — use auto | exa | github")
                                            {:provider provider})})))))

;; =============================================================================
;; GitHub source archive download (public codeload)
;; =============================================================================

(def ^:private github-codeload-base "https://codeload.github.com")
(def ^:private github-archive-max-bytes (* 10 1024 1024))
(def ^:private github-full-archive-max-bytes (* 100 1024 1024))
(def ^:private github-full-archive-max-extracted-bytes (* 1024 1024 1024))
(def ^:private github-full-archive-max-files 10000)
(def ^:private github-download-default-files 6)
(def ^:private github-download-max-files 20)
(def ^:private github-download-default-bytes 51200)
(def ^:private github-download-max-bytes 131072)

(defn- github-repository!
  [repository]
  (let [repository (str repository)]
    (when-not (re-matches #"[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+" repository)
      (throw (ex-info "repository must be an owner/repo pair"
                      {:type :search/invalid-github-repository})))
    repository))

(defn- github-ref!
  [ref]
  (let [ref (str (or ref "HEAD"))]
    (when-not (and (<= 1 (count ref) 255) (not (re-find #"[\u0000-\u001f\u007f]" ref)))
      (throw (ex-info "ref must be a non-empty Git ref" {:type :search/invalid-github-ref})))
    ref))

(defn- github-archive-url
  [repository ref]
  (str github-codeload-base "/" repository "/tar.gz/" (encode-url ref)))

(defn- github-download-response
  ([url] (github-download-response url github-archive-max-bytes))
  ([url max-bytes]
   (let
     [response
      (if *github-get-fn*
        (*github-get-fn* url
                         {:as :bytes
                          :throw false
                          :timeout default-timeout-ms
                          :headers {"user-agent" "vis-foundation-search/0.1"}})
        (http/get url
                  {:as :bytes
                   :throw false
                   :timeout default-timeout-ms
                   :headers {"user-agent" "vis-foundation-search/0.1"}}))

      status
      (:status response)

      body
      (:body response)]

     (when (or (nil? status) (< (long status) 200) (>= (long status) 300))
       (throw (ex-info (str "GitHub archive HTTP " status)
                       {:type :search/github-archive-http-error :status status})))
     (when-not (bytes? body)
       (throw (ex-info "GitHub archive response was not binary"
                       {:type :search/github-archive-invalid})))
     (when (> (long (alength ^bytes body)) (long max-bytes))
       (throw (ex-info
                (str "GitHub archive exceeds the " max-bytes " byte compressed download limit")
                {:type :search/github-archive-too-large :max-bytes max-bytes})))
     body)))

(defn- archive-relative-path
  [entry-name]
  ;; codeload wraps every archive in one top-level `owner-repo-ref/` directory.
  (some-> (second (str/split (str entry-name) #"/" 2))
          not-empty))

(defn- requested-path?
  [path prefix]
  (or (nil? prefix) (= path prefix) (str/starts-with? path (str prefix "/"))))

(defn- archive-files
  [archive-bytes prefix max-files max-bytes]
  (let
    [max-files
     (long max-files)

     max-bytes
     (long max-bytes)]

    (with-open
      [gzip
       (GZIPInputStream. (ByteArrayInputStream. ^bytes archive-bytes))

       tar
       (TarArchiveInputStream. gzip)]

      (loop
        [files
         []

         total-bytes
         0

         truncated?
         false]

        (if-let [entry (.getNextTarEntry tar)]
          (let [path (archive-relative-path (.getName entry))]
            (if (or (.isDirectory entry) (nil? path) (not (requested-path? path prefix)))
              (recur files total-bytes truncated?)
              (let
                [remaining (- max-bytes total-bytes)
                 declared-size (.getSize entry)]

                (if (or (zero? remaining) (>= (count files) max-files))
                  (recur files total-bytes true)
                  (let
                    [limit (long (Math/min remaining declared-size))
                     out (ByteArrayOutputStream. (int limit))
                     buffer (byte-array 8192)]

                    (loop [left limit]
                      (when (pos? (long left))
                        (let
                          [read (.read tar
                                       buffer
                                       0
                                       (int (Math/min (long left) (long (alength buffer)))))]
                          (when (pos? read)
                            (.write out buffer 0 read)
                            (recur (long (- (long left) (long read))))))))
                    (let [content (.toString out (.name StandardCharsets/UTF_8))]
                      (recur (conj
                               files
                               {"path" path "content" content "truncated" (> declared-size limit)})
                             (long (+ (long total-bytes) limit))
                             (or truncated? (> declared-size limit)))))))))
          {"files" files "truncated" truncated?})))))

(defn download-code
  "await download_code(\"owner/repo\", {\"ref\": \"main\", \"path\": \"src\"})

   Fetch a public GitHub codeload tarball and return bounded UTF-8 source excerpts.
   This is for a repository already identified by search, not code discovery. It never
   writes to disk, accepts only owner/repo, and caps compressed downloads at 10 MiB.
   `path` restricts the archive prefix; `max_files` (default 6, max 20) and
   `max_bytes` (default 51200, max 131072) bound returned content."
  ([repository] (download-code repository {}))
  ([repository opts]
   (let
     [repository
      (github-repository! repository)

      opts
      (or opts {})

      ref
      (github-ref! (get opts "ref"))

      prefix
      (some-> (get opts "path")
              str
              str/trim
              not-empty)

      max-files
      (long (min (long github-download-max-files)
                 (long (positive-long (get opts "max_files") github-download-default-files))))

      max-bytes
      (long (min (long github-download-max-bytes)
                 (long (positive-long (get opts "max_bytes") github-download-default-bytes))))

      url
      (github-archive-url repository ref)]

     (try (let
            [{:strs [files truncated]}
             (archive-files (github-download-response url) prefix max-files max-bytes)

             citations
             (mapv (fn [{:strs [path content truncated]}]
                     {"type" "code"
                      "title" (str repository "/" path)
                      "url"
                      (str "https://github.com/" repository "/blob/" (encode-url ref) "/" path)
                      "excerpt" content
                      "source" "github"
                      "truncated" (boolean truncated)})
                   files)]

            (search-success {:op :download-code
                             :tool "github-codeload"
                             :query repository
                             :citations citations
                             :source :github
                             :endpoint url
                             :truncated? truncated}))
          (catch Throwable t
            (search-failure {:op :download-code
                             :tool "github-codeload"
                             :query repository
                             :source :github
                             :endpoint url
                             :citation-type :code
                             :throwable t}))))))

(defn- archive-safe-relative-path!
  [entry-name]
  (let
    [path
     (archive-relative-path entry-name)

     segments
     (some-> path
             (str/split #"/"))]

    (when-not (and (seq segments)
                   (every? #(and (not (str/blank? %)) (not= "." %) (not= ".." %)) segments)
                   (not (str/includes? (str path) "\\")))
      (throw (ex-info "GitHub archive contains an unsafe entry path"
                      {:type :search/github-archive-unsafe-path :entry entry-name})))
    path))

(defn- sha256-hex
  [^bytes bytes]
  (let [digest (.digest (MessageDigest/getInstance "SHA-256") bytes)]
    (apply str (map #(format "%02x" (bit-and (int %) 0xff)) digest))))

(defn- extract-github-archive!
  [^bytes archive-bytes ^Path destination]
  (with-open
    [gzip
     (GZIPInputStream. (ByteArrayInputStream. archive-bytes))

     tar
     (TarArchiveInputStream. gzip)]

    (loop
      [files
       0

       total-bytes
       0]

      (if-let [entry (.getNextTarEntry tar)]
        (let [entry-name (.getName entry)]
          (if (nil? (archive-relative-path entry-name))
            (recur files total-bytes)
            (let
              [^String relative (archive-safe-relative-path! entry-name)
               ^Path output (-> ^Path destination
                                (.resolve ^String relative)
                                (.normalize))
               size (.getSize entry)]

              (when-not (.startsWith output destination)
                (throw (ex-info "GitHub archive entry escapes its destination"
                                {:type :search/github-archive-unsafe-path :entry entry-name})))
              (cond (.isDirectory entry) (do (Files/createDirectories output
                                                                      (make-array FileAttribute 0))
                                             (recur files total-bytes))
                    (or (.isSymbolicLink entry) (.isLink entry) (not (.isFile entry)) (neg? size))
                    (throw (ex-info "GitHub archive contains an unsupported entry"
                                    {:type :search/github-archive-unsafe-entry :entry entry-name}))
                    (or (not (neg? (Long/compare (long files)
                                                 (long github-full-archive-max-files))))
                        (pos? (Long/compare (Math/addExact (long total-bytes) (long size))
                                            (long github-full-archive-max-extracted-bytes))))
                    (throw (ex-info "GitHub archive exceeds the extracted content limit"
                                    {:type :search/github-archive-extracted-too-large}))
                    :else
                    (do (Files/createDirectories (.getParent output) (make-array FileAttribute 0))
                        (with-open [out (io/output-stream (.toFile output))]
                          (let [buffer (byte-array 8192)]
                            (loop [remaining size]
                              (when (pos? remaining)
                                (let [n (.read tar buffer 0 (int (min remaining (alength buffer))))]
                                  (when (neg? n)
                                    (throw (ex-info
                                             "GitHub archive ended before an entry was complete"
                                             {:type :search/github-archive-truncated
                                              :entry entry-name})))
                                  (.write out buffer 0 n)
                                  (recur (- remaining n)))))))
                        (recur (inc files) (+ total-bytes size)))))))
        {:files files :bytes total-bytes}))))

(defn- archive-output-path!
  ^Path [workspace-root repository ref directory]
  (let
    [^Path root
     (.toPath (io/file workspace-root))

     ^Path root
     (.toAbsolutePath root)

     _
     (Files/createDirectories root (make-array FileAttribute 0))

     default-name
     (str/replace (str repository "-" ref) #"[^A-Za-z0-9_.-]+" "-")

     ^String relative
     (or (some-> directory
                 str
                 str/trim
                 not-empty)
         (str "downloads/" default-name))

     ^Path candidate
     (-> ^Path root
         (.resolve ^String relative)
         (.normalize))]

    (when (or (.isAbsolute (io/file relative)) (not (.startsWith candidate root)))
      (throw (ex-info "directory must stay within the workspace root"
                      {:type :search/invalid-archive-directory})))
    candidate))

(defn download-archive
  "await download_archive(\"owner/repo\", {\"ref\": \"main\"})

   Download a complete public GitHub codeload tar.gz and extract it into a new directory
   below the workspace. Returns the absolute directory path; it never puts archive bytes in
   the model context. Default destination is `downloads/owner-repo-ref`; `directory` may
   choose another relative destination. The compressed archive is capped at 100 MiB, with a
   1 GiB / 10,000-file extracted safety cap."
  ([repository] (download-archive repository {}))
  ([repository opts] (download-archive (workspace/cwd) repository opts))
  ([workspace-root repository opts]
   (let
     [opts
      (or opts {})

      repository
      (github-repository! repository)

      ref
      (github-ref! (get opts "ref"))

      url
      (github-archive-url repository ref)

      destination
      (archive-output-path! workspace-root repository ref (get opts "directory"))]

     (try (when (Files/exists destination (make-array LinkOption 0))
            (throw (ex-info "archive destination already exists; choose another directory"
                            {:type :search/github-archive-destination-exists
                             :path (str destination)})))
          (let
            [archive-bytes
             (github-download-response url github-full-archive-max-bytes)

             _
             (Files/createDirectories destination (make-array FileAttribute 0))

             {:keys [files bytes]}
             (extract-github-archive! archive-bytes destination)]

            (extension/success {:op :download-archive
                                :result {"op" "download_archive"
                                         "repository" repository
                                         "ref" ref
                                         "path" (str (.toAbsolutePath destination))
                                         "files" files
                                         "bytes" bytes
                                         "archive_bytes" (alength ^bytes archive-bytes)
                                         "sha256" (sha256-hex archive-bytes)
                                         "source" "github"}
                                :metadata
                                {:tool "github-codeload" :endpoint url :path (str destination)}}))
          (catch Throwable t
            (extension/failure {:op :download-archive
                                :result {"op" "download_archive"
                                         "repository" repository
                                         "error" (or (ex-message t) "archive download failed")}
                                :error {:message (or (ex-message t) "archive download failed")}
                                :metadata {:tool "github-codeload" :endpoint url}}))))))


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
    (let
      [stream
       (ByteArrayInputStream. xml-bytes)

       parsed
       (xml/parse stream)

       entries
       (filter #(= :entry (:tag %)) (:content parsed))

       extract
       (fn [entry tag]
         (some->> (:content entry)
                  (filter #(= tag (:tag %)))
                  first
                  :content
                  first
                  (#(when (string? %) (str/trim %)))))

       extract-author
       (fn [entry]
         (some->> (:content entry)
                  (filter #(= :author (:tag %)))
                  first
                  :content
                  (filter #(= :name (:tag %)))
                  first
                  :content
                  first
                  (#(when (string? %) (str/trim %)))))]

      (mapv (fn [e]
              {"type" "paper"
               "title" (or (extract e :title) "")
               "url" (or (extract e :id) "")
               "excerpt" (or (extract e :summary) "")
               "authors" (or (extract-author e) "")
               "published" (or (extract e :published) "")
               "source" "arxiv"})
            entries))
    (catch Throwable t
      [{"type" "paper"
        "title" "arxiv parse failed"
        "url" ""
        "excerpt" (or (ex-message t) "")
        "source" "arxiv"
        "error" true}])))

(defn search-papers
  "await search_papers(\"diffusion models for protein folding\")
   await search_papers(\"…\", {\"num_results\": 10, \"sort\": \"relevance\", \"timeout_ms\": 20000})

   arxiv paper search.
   Returns {\"query\", \"citations\": [{\"type\": \"paper\", \"title\", \"url\", \"excerpt\", \"authors\", \"published\", \"source\"}, ...], \"citation_count\", \"truncated\", \"source\"}.
   opts: \"sort\" is relevance|lastUpdatedDate|submittedDate (default relevance).
   Gotcha: \"excerpt\" is the abstract (plain text); on failure \"citations\"[0] has \"error\": True."
  ([query] (search-papers query {}))
  ([query opts]
   (let
     [max-results
      ;; `num_results` everywhere; `max_results` is the retired papers-only
      ;; spelling, still accepted.
      (or (get opts "num_results") (get opts "max_results") ARXIV_DEFAULT_MAX_RESULTS)

      sort-key
      (or (get opts "sort") "relevance")

      timeout-ms
      (or (get opts "timeout_ms") ARXIV_DEFAULT_TIMEOUT_MS)

      url
      (str ARXIV_API_BASE
           "?search_query="
           (URLEncoder/encode (str "all:" query) "UTF-8")
           "&start=0"
           "&max_results="
           max-results
           "&sortBy="
           (case sort-key
             "lastUpdatedDate"
             "lastUpdatedDate"

             "submittedDate"
             "submittedDate"

             "relevance"
             "relevance"

             "relevance")
           "&sortOrder=descending")]

     (try (let
            [resp
             (http/get url
                       {:timeout timeout-ms :headers {"User-Agent" "vis-foundation-search/0.1"}})

             body
             (:body resp)

             body-bytes
             (cond (string? body) (.getBytes ^String body StandardCharsets/UTF_8)
                   (bytes? body) body
                   :else (.getBytes (str body) StandardCharsets/UTF_8))

             citations
             (parse-arxiv-atom body-bytes)]

            (search-success {:op :search-papers
                             :tool "arxiv"
                             :query query
                             :citations citations
                             :source :arxiv
                             :endpoint url
                             :truncated? false}))
          (catch Throwable t
            (search-failure {:op :search-papers
                             :tool "arxiv"
                             :query query
                             :source :arxiv
                             :endpoint url
                             :citation-type :paper
                             :throwable t}))))))

;; =============================================================================
;; Unified entry point — ONE search, three kinds
;; =============================================================================

(defn search
  "await search(\\\"query\\\")  # web by default
   await search(\\\"query\\\", {\\\"kind\\\": \\\"code\\\", \\\"tokens_num\\\": N})
   await search(\\\"query\\\", {\\\"kind\\\": \\\"papers\\\", \\\"num_results\\\": 10})

   One entry point: `kind` is `web` (Exa live web), `code` (Exa/GitHub repos and docs), or `papers` (arXiv). Options: web `num_results`/`type`/`livecrawl`/`context_max_characters`; code `tokens_num`/`provider`; papers `num_results`/`sort`/`timeout_ms`.
   Returns the query, ranked `citations` (`type`/`title`/`url`/`excerpt`/...), count, truncation, source, and endpoint. Read `excerpt` directly; failures put `error` on the first citation."
  ([query] (search query {}))
  ([query opts]
   (let
     [opts
      (or opts {})

      kind
      (str (or (get opts "kind") "web"))

      opts
      (dissoc opts "kind")]

     (case kind
       "web"
       (search-web query opts)

       "code"
       (search-code query opts)

       "papers"
       (search-papers query opts)

       (search-failure {:op :search-web
                        :tool "search"
                        :query query
                        :source :exa
                        :citation-type :web
                        :throwable (ex-info (str "unknown search kind "
                                                 (pr-str kind)
                                                 " — use web | code | papers")
                                            {:kind kind})})))))

;; =============================================================================
;; Symbol entries
;; =============================================================================

(def web-symbol
  (vis/symbol
    #'search-web
    {:tag :observation
     :name "search_web"
     :params [{:name "num_results" :note "citations to ask the provider for"}
              {:name "type" :note "auto | neural | keyword"}
              {:name "livecrawl" :note "preferred forces a fresh fetch"}
              {:name "context_max_characters" :note "cap on excerpt characters per citation"}]
     :description
     "Search the live web for current facts, external documentation, or research the local project cannot answer. Returns ranked citations with excerpts."
     :result
     (str
       "String-keyed `{op, query, citations, citation_count, truncated, source, endpoint}`; each "
       "citation carries `type`/`title`/`url`/`excerpt` plus its source metadata. A failure "
       "answers the same envelope with `error` on the first citation.")}))

(def code-symbol
  (vis/symbol
    #'search-code
    {:tag :observation
     :name "search_code"
     :params [{:name "provider" :note "auto (default) | exa | github"}
              {:name "tokens_num" :note "context tokens per code result"}]
     :description
     "Search live repositories and technical documentation when the local project and embedded docs are insufficient. Set provider to github to use GitHub Code Search directly, which needs a logged-in `gh` CLI."
     :result
     (str
       "String-keyed `{op, query, citations, citation_count, truncated, source, endpoint}`; each "
       "citation carries `type`/`title`/`url`/`excerpt` plus its source metadata. A failure "
       "answers the same envelope with `error` on the first citation.")}))

(def download-code-symbol
  (vis/symbol
    #'download-code
    {:tag :observation
     :name "download_code"
     :params [{:name "ref" :note "branch, tag or sha"}
              {:name "path" :note "archive prefix to restrict to"}
              {:name "max_files" :note "default 6, max 20"}
              {:name "max_bytes" :note "default 51200, max 131072"}]
     :description
     "Fetch bounded UTF-8 source excerpts from a known public GitHub `owner/repo` archive — use after search, not for discovery. Never writes to disk and caps the compressed download at 10 MiB."
     :result
     (str
       "String-keyed `{op, query, citations, citation_count, truncated, source, endpoint}` where each "
       "citation is ONE file: its path, URL and a bounded UTF-8 excerpt.")}))

(def download-archive-symbol
  (vis/symbol
    #'download-archive
    {:tag :observation
     :name "download_archive"
     ;; The 3-arity `[workspace-root repository opts]` is the HOST's; the model
     ;; calls `download_archive("owner/repo", {…})` and the workspace resolves
     ;; itself. Without this shape the page taught `workspace_root` first.
     :call {:pos ["repository"] :opt-pos ["opts"] :rest :always}
     :params [{:name "ref" :note "branch, tag or sha"}
              {:name "directory"
               :note "workspace-relative destination; default downloads/owner-repo-ref"}]
     :description
     (str
       "Download and extract a complete public GitHub repository archive into the workspace: "
       "`repository` (`owner/name`) is required, optional `ref` and a workspace-relative `directory`. "
       "Returns the saved absolute directory path. The archive is capped at 100 MiB compressed, "
       "1 GiB / 10,000 files extracted, and an existing destination is refused rather than merged.")
     :result
     (str
       "String-keyed `{op, repository, ref, path, files, bytes, archive_bytes, sha256, source}` — "
       "`path` is the absolute extracted directory. A failure answers `{op, repository, error}`.")}))

(def papers-symbol
  (vis/symbol
    #'search-papers
    {:tag :observation
     :name "search_papers"
     :params [{:name "num_results" :note "default 10"}
              {:name "sort" :note "relevance (default) | lastUpdatedDate | submittedDate"}
              {:name "timeout_ms" :note "default 20000"}]
     :description
     "Search arXiv for relevant papers. Returns citations with abstracts so claims can be checked against primary research."
     :result
     (str
       "String-keyed `{op, query, citations, citation_count, truncated, source}`; a citation is a paper "
       "(`title`/`url`/`authors`/`published`) whose `excerpt` is the plain-text abstract.")}))

(def search-symbol
  (vis/symbol
    #'search
    {:tag :observation
     :name "search"
     :params [{:name "kind" :note "web (default) | code | papers"}
              {:name "num_results" :note "web, papers"} {:name "type" :note "web"}
              {:name "livecrawl" :note "web"} {:name "context_max_characters" :note "web"}
              {:name "tokens_num" :note "code"} {:name "provider" :note "code: auto | exa | github"}
              {:name "sort" :note "papers"} {:name "timeout_ms" :note "papers"}]
     :description
     (str
       "Search live web, public code/docs, or arXiv papers — `kind` is one of `web`, `code`, `papers`; "
       "code can use GitHub. Returns ranked citations with excerpts.")
     :result
     (str
       "String-keyed `{op,query,citations,citation_count,truncated,source,endpoint?}`; citations are "
       "normalized source objects with title, URL, and available excerpt/metadata.")}))

(def search-symbols
  [search-symbol web-symbol code-symbol download-code-symbol download-archive-symbol papers-symbol])
;; `:tag :observation` carried INLINE on each `vis/symbol` opts map
;; above; register-extension! auto-populates the op registry.

(vis/register-toggle! {:id "web_search"
                       :label "Web search"
                       :description "Expose live research over Exa, GitHub Code Search, and arXiv."
                       :default true
                       :owner :vis
                       :persist? true
                       :group :extensions})

(def vis-extension
  (vis/extension
    {:ext/name "foundation-search"
     :ext/description
     "Exa, GitHub Code Search/codeload, and arXiv research: native `search` (web/code/papers); sandbox `search_web`, `search_code`, `search_papers`, and `download_code`. Requires `web_search`."
     :ext/version "0.1.0"
     :ext/author "Blockether"
     :ext/owner "vis"
     :ext/license "Apache-2.0"
     :ext/activation-fn (fn [_env]
                          (vis/toggle-enabled? "web_search"))
     :ext/engine {:ext.engine/builtin? true :ext.engine/symbols search-symbols}
     :ext/kind "search"}))

(vis/register-extension! vis-extension)
