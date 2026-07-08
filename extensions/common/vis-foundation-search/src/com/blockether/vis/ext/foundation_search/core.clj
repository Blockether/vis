(ns com.blockether.vis.ext.foundation-search.core
  "Research extension — NATIVE tools.

   Three model-facing bindings, bound BARE (native, no alias) in the
   sandbox alongside cat/rg/patch — same surface as the foundation
   kernel, no `;; -- EXTENSION search --` prompt block:

     search_web(query, opts?)    — web search via Exa MCP
     search_code(query, opts?)   — code/doc context via Exa MCP
     search_papers(query, opts?) — arxiv papers (Atom feed)

   Output shape — consistent with the rest of the `:tag :observation`
   tool surface (cat, ls, rg). Every search fn returns the
   canonical tool envelope; the agent sees the unwrapped `:result` map:

     {:op       :search-web|:search-code|:search-papers
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
  (:require [babashka.http-client :as http]
            [charred.api :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.xml :as xml]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.internal.extension :as extension])
  (:import (java.io ByteArrayInputStream)
           (java.net URI URLDecoder URLEncoder)
           (java.nio.charset StandardCharsets)
           (java.util UUID)))

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
    (if (and n (pos? n)) n fallback)))

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
    (let [tools (->> (str/split s #",")
                     (map str/trim)
                     (remove str/blank?)
                     vec)]
      (when (seq tools) tools))))

(defn- normalize-tools
  [x]
  (cond (string? x) (split-tools x)
        (sequential? x) (let [tools (->> x
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
  (let [uri
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
  (try (let [uri
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
        (or (nil? status) (< status 200) (>= status 300))
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
  (let [id
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
  (let [cfg
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
  (loop [out
         []

         remaining
         (seq lines)

         used
         0]

    (if-not remaining
      out
      (let [line
            (first remaining)

            extra
            (+ (utf8-bytes line) (if (seq out) 1 0))]

        (if (> (+ used extra) max-bytes)
          out
          (recur (conj out line) (next remaining) (+ used extra)))))))

(defn truncate-text
  "Bound `text` by line and UTF-8 byte limits. Returns truncation map."
  [text {:keys [max-bytes max-lines]}]
  (let [text
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
    {:max-bytes (min (positive-long (:max-bytes opts) (:max-bytes cfg)) (:max-bytes cfg))
     :max-lines (min (positive-long (:max-lines opts) (:max-lines cfg)) (:max-lines cfg))}))

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
  (let [lines
        (str/split-lines (or excerpt ""))

        marker
        " [...]"]

    (loop [ls
           lines

           in-fence?
           false

           out
           []]

      (if (empty? ls)
        (str/join "\n" out)
        (let [ln
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
  (let [strip
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
    (loop [i 0
           open nil]

      (if (>= i (count lines))
        (if (and open (str/blank? (:lang open)) (doc-prose? (subvec lines (inc (:idx open)))))
          (str/join "\n" (into (subvec lines 0 (:idx open)) (subvec lines (inc (:idx open)))))
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
  (let [text
        (or text "")

        ;; Split on the leading `Title: ` boundary (start of file OR a fresh
        ;; entry after a blank line). Keep the prefix attached to each chunk.
        chunks
        (->> (str/split (str "\n" text) #"\nTitle: ")
             rest)]

    ;; drop the empty pre-first-Title slice
    (vec
      (for [chunk
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
                    (some #(when (str/starts-with? % pfx) (str/trim (subs % (count pfx))))
                          rest-lines))

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
                  (let [after-highlights (drop 1
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

                  excerpt
                  (-> (str/trim (str/join "\n" excerpt-lines))
                      normalize-exa-excerpt
                      unwrap-doc-fences)]]

        (cond-> {"type" (kw->snake citation-type)
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
;; (TUI / Telegram / web) gets a structured payload AND a custom
;; render-fn that paints citation cards instead of dumping the raw
;; markdown blob. Python itself sees the unwrapped `:result` map.
;; ----------------------------------------------------------------------------

(defn- search-result-payload
  "Canonical Python-facing :result map for a successful search call.
   STRINGS-ONLY: string keys, enum values (`op`, `source`) snake-cased so the
   map crosses the boundary already string-clean."
  [{:keys [op query citations source endpoint truncated?]}]
  (cond-> {"op" (kw->snake op)
           "query" (str query)
           "citations" (vec citations)
           "citation_count" (count citations)
           "truncated" (boolean truncated?)
           "source" (kw->snake source)}
    endpoint
    (assoc "endpoint" endpoint)))

(defn- search-success
  "Wrap a successful search call in the canonical tool envelope so it
   travels through `invoke-symbol-wrapper` the same way v/* tools do."
  [{:keys [op tool query citations source endpoint truncated?]}]
  (let [payload (search-result-payload {:op op
                                        :query query
                                        :citations citations
                                        :source source
                                        :endpoint endpoint
                                        :truncated? truncated?})]
    (extension/success {:result payload
                        :op op
                        :metadata (cond-> {:tool (str tool)
                                           :source source
                                           :citation-count (get payload "citation_count")
                                           :truncated? (get payload "truncated")
                                           :query (str query)}
                                    endpoint
                                    (assoc :endpoint endpoint))})))

(defn- search-failure
  "Failure envelope. Carries a single error-flagged citation on
   `:result` so model code that already destructures
   `(:citations r)` still sees the failure, AND a structured
   `:error` map so the channel renderer paints the fail card
   from the envelope side."
  [{:keys [op tool query source endpoint citation-type ^Throwable throwable]}]
  (let [msg
        (or (some-> throwable
                    ex-message)
            "search failed")

        error-entry
        (cond-> {"type" (kw->snake citation-type)
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

(defn- call-exa!
  "Common path for `web` + `code`: call MCP, parse text → envelope.
   Returns an `extension/success` (or `extension/failure`) envelope
   so the wrapper produces channel-renderable structured output.

   `op` is the public op kw (`:search-web` / `:search-code`).
   `tool-name` is the Exa MCP tool string (`\"web_search_exa\"` etc.)."
  [op tool-name args citation-type query]
  (try (let [{:keys [endpoint result]}
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
       (catch Throwable t
         (search-failure {:op op
                          :tool tool-name
                          :query query
                          :source :exa
                          :citation-type citation-type
                          :throwable t}))))

(defn search-web
  "await search_web(\"rust async runtime comparison\")
   await search_web(\"…\", {\"num_results\": 5, \"type\": \"auto\", \"livecrawl\": \"preferred\", \"context_max_characters\": N})

   Live web search via Exa.
   Returns {\"query\", \"citations\": [{\"type\": \"web\", \"title\", \"url\", \"excerpt\", \"published\", \"authors\", \"source\"}, ...], \"citation_count\", \"truncated\", \"source\", \"endpoint\"}.
   Gotcha: \"excerpt\" is markdown text — read it directly; on failure \"citations\"[0] carries \"error\": True with the message in \"excerpt\"."
  ([query] (search-web query {}))
  ([query opts]
   (call-exa! :search-web "web_search_exa" (web-args (str query) (or opts {})) :web (str query))))

(defn search-code
  "await search_code(\"clojure core.async go-loop example\")
   await search_code(\"…\", {\"tokens_num\": N})

   Live code/docs search via Exa (github repos, clojuredocs, readthedocs, API refs). Narrow with \"site:github.com X\" or \"<repo> X\".
   Returns {\"query\", \"citations\": [{\"type\": \"code\", \"title\", \"url\", \"excerpt\", \"source\"}, ...], \"citation_count\", \"truncated\", \"source\", \"endpoint\"}.
   Gotcha: \"excerpt\" is markdown text; on failure \"citations\"[0] has \"error\": True."
  ([query] (search-code query {}))
  ([query opts]
   (call-exa! :search-code
              "get_code_context_exa" (code-args (str query) (or opts {}))
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
    (let [stream
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
   await search_papers(\"…\", {\"max_results\": 10, \"sort\": \"relevance\", \"timeout_ms\": 20000})

   arxiv paper search.
   Returns {\"query\", \"citations\": [{\"type\": \"paper\", \"title\", \"url\", \"excerpt\", \"authors\", \"published\", \"source\"}, ...], \"citation_count\", \"truncated\", \"source\"}.
   opts: \"sort\" is relevance|lastUpdatedDate|submittedDate (default relevance).
   Gotcha: \"excerpt\" is the abstract (plain text); on failure \"citations\"[0] has \"error\": True."
  ([query] (search-papers query {}))
  ([query opts]
   (let [max-results
         (or (get opts "max_results") ARXIV_DEFAULT_MAX_RESULTS)

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

     (try (let [resp
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
;; Symbol entries
;; =============================================================================

;; =============================================================================
;; Op-card renderer — a boxed citation table + full excerpt cards
;; =============================================================================
;;
;; Declared as `:render` on each search symbol. The channel layer (TUI / web /
;; telegram) calls it with the tool's `:result` map and paints the returned
;; `{:summary :body}` op-card. `:body` is Markdown: a GFM table the channels
;; draw as a boxed grid, then one card per citation with the FULL excerpt.
;; Table CELLS render as plain text (the TUI table painter does not re-parse
;; inline markdown inside a cell), so links live in the cards, not the table.

(defn- search-cell
  "One-line, pipe-escaped, length-capped text for a GFM table cell."
  [s max-len]
  (let [s (-> (str s)
              (str/replace #"\s+" " ")
              str/trim
              (str/replace "|" "\\|"))]
    (if (> (count s) max-len) (str (subs s 0 (max 0 (dec max-len))) "…") s)))

(defn- search-host
  "Bare host of a URL (leading `www.` stripped) for the compact Source column.
   Falls back to the raw string when the URL is unparseable or empty."
  [url]
  (let [u (str url)]
    (try (let [h (.getHost (URI/create u))]
           (if (str/blank? h) u (str/replace h #"^www\." "")))
         (catch Exception _ u))))

(defn- prose-excerpt
  "Flatten an Exa excerpt to wrap-friendly prose. Web excerpts carry ``` fenced
   code blocks whose long verbatim lines (curl commands, mermaid, tables) would
   overflow the TUI bubble's right edge because a bare fence has no `:lang` and
   so renders unwrapped. Dropping the fence markers lets every line reflow as a
   normal paragraph inside the bubble width."
  [s]
  (->> (str/split-lines (str s))
       (remove #(re-matches #"\s*```.*" %))
       (str/join "\n")
       str/trim
       not-empty))

(defn- render-search-result
  "Op-card renderer for search_web / search_code / search_papers. Paints a GFM
   table (index · title · source · date) — drawn as a boxed table by the
   channels — followed by one card per citation carrying the FULL markdown
   excerpt under a linked heading. A failure / empty result degrades to a
   summary-only card (its single excerpt is the error message)."
  [r]
  (let [citations
        (get r "citations")

        query
        (str (get r "query"))

        n
        (count citations)

        error?
        (get r "error")

        summary
        (str (if error? "search failed" (str n " result" (when (not= 1 n) "s")))
             (when (seq query) (str " · «" query "»")))]

    (if (or error? (empty? citations))
      {:summary summary
       :body (some-> (first citations)
                     (get "excerpt")
                     prose-excerpt)}
      (let [header
            ["| # | Result | Source | Published |" "|--:|--------|--------|-----------|"]

            rows
            (map-indexed (fn [i c]
                           (str "| "
                                (inc i)
                                " | "
                                (search-cell (get c "title") 70)
                                " | "
                                (search-cell (search-host (get c "url")) 28)
                                " | "
                                (search-cell (or (not-empty (str (get c "published"))) "—") 12)
                                " |"))
                         citations)

            table
            (str/join "\n" (concat header rows))

            cards
            (map-indexed (fn [i c]
                           (let [title
                                 (str/trim (str (get c "title")))

                                 url
                                 (str (get c "url"))

                                 meta
                                 (->> [(get c "source") (get c "authors") (get c "published")]
                                      (keep #(let [s
                                                   (str %)]

                                               (when (seq s) s)))
                                      (str/join " · "))

                                 head
                                 (if (seq url)
                                   (str "**" (inc i) ". [" title "](" url ")**")
                                   (str "**" (inc i) ". " title "**"))]

                             (str head
                                  (when (seq meta) (str "  \n_" meta "_"))
                                  "\n\n"
                                  (prose-excerpt (get c "excerpt")))))
                         citations)]

        {:summary summary :body (str table "\n\n" (str/join "\n\n---\n\n" cards))}))))


(def web-symbol
  (vis/symbol
    #'search-web
    {:tag :observation
     :native-tool? true
     :name "search_web"
     :color-role :tool-color/search
     :render render-search-result
     :description (str "Live WEB search via Exa. `query` is a natural-language query; returns "
                       "ranked citations, each with a markdown `excerpt`. Use for current events, "
                       "external docs, and general research the local repo can't answer.")
     :schema {:type "object"
              :properties
              {"query" {:type "string" :description "Natural-language web search query."}
               "num_results" {:type "integer" :description "Max results to return."}
               "type" {:type "string"
                       :description "Exa search type, e.g. \"auto\", \"neural\", \"keyword\"."}
               "livecrawl" {:type "string"
                            :description
                            "Live-crawl mode, e.g. \"preferred\", \"always\", \"never\"."}
               "context_max_characters" {:type "integer"
                                         :description "Cap on context characters per result."}}
              :required ["query"]}
     :handler (fn [_env input]
                (search-web (str (get input "query")) (dissoc input "query")))}))

(def code-symbol
  (vis/symbol
    #'search-code
    {:tag :observation
     :native-tool? true
     :name "search_code"
     :color-role :tool-color/search
     :render render-search-result
     :description (str "Live CODE/docs search via Exa (github repos, clojuredocs, readthedocs, API "
                       "refs). `query` is natural language; narrow with \"site:github.com X\" or "
                       "\"<repo> X\". Returns ranked citations with markdown excerpts.")
     :schema {:type "object"
              :properties
              {"query" {:type "string" :description "Natural-language code/docs search query."}
               "tokens_num" {:type "integer"
                             :description "Approximate token budget for the returned context."}}
              :required ["query"]}
     :handler (fn [_env input]
                (search-code (str (get input "query")) (dissoc input "query")))}))

(def papers-symbol
  (vis/symbol #'search-papers
              {:tag :observation
               :native-tool? true
               :name "search_papers"
               :color-role :tool-color/search
               :render render-search-result
               :description
               (str "arXiv paper search. `query` is natural language; returns citations whose "
                    "`excerpt` is the abstract. Sort by relevance (default), lastUpdatedDate, or "
                    "submittedDate.")
               :schema
               {:type "object"
                :properties
                {"query" {:type "string" :description "Natural-language paper search query."}
                 "max_results" {:type "integer" :description "Max papers to return (default 10)."}
                 "sort" {:type "string"
                         :description
                         "relevance | lastUpdatedDate | submittedDate (default relevance)."}
                 "timeout_ms" {:type "integer" :description "HTTP timeout in milliseconds."}}
                :required ["query"]}
               :handler (fn [_env input]
                          (search-papers (str (get input "query")) (dissoc input "query")))}))

(def search-symbols [web-symbol code-symbol papers-symbol])

(def search-env
  [{:name "EXA_API_KEY"
    :label "Exa API key"
    :description "Optional Exa key for higher MCP limits. Also read as EXA_MCP_API_KEY."
    :secret? true}
   {:name "EXA_MCP_API_KEY"
    :label "Exa MCP API key"
    :description "Alias for EXA_API_KEY. Use one, not both."
    :secret? true}
   {:name "EXA_MCP_URL" :label "Exa MCP URL" :description "Override Exa MCP endpoint URL."}
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
     :ext/description
     "Live research bindings (NATIVE/bare): search_web + search_code (Exa MCP) + search_papers (arxiv)."
     :ext/version "0.1.0"
     :ext/author "Blockether"
     :ext/owner "vis"
     :ext/license "Apache-2.0"
     :ext/engine {:ext.engine/builtin? true :ext.engine/symbols search-symbols}
     :ext/kind "search"
     :ext/env search-env}))

(vis/register-extension! vis-extension)
