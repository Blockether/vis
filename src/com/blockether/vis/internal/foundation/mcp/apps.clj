(ns com.blockether.vis.internal.foundation.mcp.apps
  "The host rules of MCP Apps (extension `io.modelcontextprotocol/ui`, revision
   2026-01-26). A tool can name a `ui://` resource in `_meta.ui.resourceUri`. After
   the agent calls that tool, Vis reads the resource and keeps one app envelope as a
   user attachment of the block. The Companion paints the envelope in a sandboxed
   frame, and the TUI opens the web app at it.

   This namespace holds only data rules: the capability that the client declares,
   the visibility of a tool, the validation of a view resource, the Content Security
   Policy and the frame permissions, and the envelope. It does no I/O."
  (:require [clojure.string :as str])
  (:import (java.nio.charset StandardCharsets)
           (java.util Base64)))

(def extension-id "The extension identifier in client capabilities." "io.modelcontextprotocol/ui")

(def mime-type "The media type of an MCP App view resource." "text/html;profile=mcp-app")

(def protocol-version "The MCP Apps revision that this host speaks." "2026-01-26")

(def envelope-media-type
  "The media type of the attachment that holds one app instance."
  "application/vnd.vis.mcp-app+json")

(def capability
  "The value of `capabilities.extensions[\"io.modelcontextprotocol/ui\"]`."
  {"mimeTypes" [mime-type]})

(def ^:private default-visibility ["model" "app"])

(defn- tool-meta-ui [tool] (let [ui (get-in tool ["_meta" "ui"])] (when (map? ui) ui)))

(defn visibility
  "Who may call `tool`: a vector of \"model\" and \"app\". A tool without
   `_meta.ui.visibility` is visible to both."
  [tool]
  (let [declared (get (tool-meta-ui tool) "visibility")]
    (if (and (sequential? declared) (seq declared))
      (vec (filter #{"model" "app"} declared))
      default-visibility)))

(defn model-visible?
  "True when the agent may see and call `tool`."
  [tool]
  (boolean (some #{"model"} (visibility tool))))

(defn app-callable?
  "True when a view may call `tool` through the host."
  [tool]
  (boolean (some #{"app"} (visibility tool))))

(defn resource-uri
  "The `ui://` view resource of `tool`, or nil. The flat key `ui/resourceUri` is the
   deprecated spelling and still counts."
  [tool]
  (let [uri (or (get (tool-meta-ui tool) "resourceUri") (get-in tool ["_meta" "ui/resourceUri"]))]
    (when (and (string? uri) (str/starts-with? uri "ui://")) uri)))

(defn- normal-media-type
  [s]
  (str/lower-case (str/replace (str s) #"\s+" "")))

(defn- invalid!
  [message data]
  (throw (ex-info message (assoc data :type :mcp-app/invalid-resource))))

(def ^:private source-pattern
  "A CSP source that a server may declare: a scheme with a host, or a host, with an
   optional `*.` label, port and path. Quotes, `;`, `,` and spaces never pass, so a
   declared value cannot add a directive or a keyword such as 'unsafe-eval'."
  #"^(?:(?:https?|wss?)://)?(?:\*\.)?[A-Za-z0-9-]+(?:\.[A-Za-z0-9-]+)*(?::(?:\d{1,5}|\*))?(?:/[A-Za-z0-9._~%!$&()*+=:@/-]*)?$")

(defn- sources
  "The valid CSP sources of declared list `xs`, in order and without repeats."
  [xs]
  (if (sequential? xs)
    (into [] (comp (filter string?) (map str/trim) (filter #(re-matches source-pattern %)) (distinct)) xs)
    []))

(defn declared-csp
  "The domains of `_meta.ui.csp` that the host accepts, as the same four keys. An
   invalid entry is dropped, never repaired."
  [csp]
  (let [csp (if (map? csp) csp {})]
    (into {}
          (keep (fn [k] (when-let [xs (not-empty (sources (get csp k)))] [k xs])))
          ["connectDomains" "resourceDomains" "frameDomains" "baseUriDomains"])))

(defn content-security-policy
  "The Content Security Policy for a view with the declared domains `csp` (the
   result of [[declared-csp]]). Nothing beyond the declared domains is allowed:
   no network without `connectDomains`, no nested frame without `frameDomains`, a
   same-origin base without `baseUriDomains`, and never an object."
  [csp]
  (let [joined (fn [k] (str/join " " (get csp k)))
        with (fn [base k] (str/trim (str base " " (joined k))))
        or-else (fn [k fallback] (if (seq (get csp k)) (joined k) fallback))]
    (str/join "; "
              ["default-src 'none'"
               (with "script-src 'self' 'unsafe-inline'" "resourceDomains")
               (with "style-src 'self' 'unsafe-inline'" "resourceDomains")
               (with "img-src 'self' data:" "resourceDomains")
               (with "font-src 'self'" "resourceDomains")
               (with "media-src 'self' data:" "resourceDomains")
               (str "connect-src " (or-else "connectDomains" "'none'"))
               (str "frame-src " (or-else "frameDomains" "'none'"))
               "object-src 'none'"
               (str "base-uri " (or-else "baseUriDomains" "'self'"))])))

(def ^:private permission-features
  {"camera" "camera" "microphone" "microphone" "geolocation" "geolocation" "clipboardWrite"
   "clipboard-write"})

(defn granted-permissions
  "The permissions of `_meta.ui.permissions` that the host knows, as `{name {}}`."
  [permissions]
  (if (map? permissions)
    (into (sorted-map)
          (keep (fn [[k v]] (when (and (permission-features k) (some? v) (not (false? v))) [k {}])))
          permissions)
    {}))

(defn allow-attribute
  "The iframe `allow` value for `granted` ([[granted-permissions]]), or \"\"."
  [granted]
  (str/join "; " (keep permission-features (sort (keys granted)))))

(defn view-resource
  "The view of `uri` from the `resources/read` result `result`: `{\"html\"
   \"csp\" \"permissions\" \"prefers_border\" \"domain\"}`. Throws an ex-info of type
   `:mcp-app/invalid-resource` when the result has no content for a `ui://` URI,
   the media type is not [[mime-type]], or the content has no text."
  [uri result]
  (when-not (and (string? uri) (str/starts-with? uri "ui://"))
    (invalid! (str "An MCP App resource URI must use the ui:// scheme: " (pr-str uri)) {:uri uri}))
  (let [contents (filter map? (get result "contents"))
        content (or (first (filter #(= uri (get % "uri")) contents)) (first contents))
        _ (when-not content (invalid! (str "The MCP App resource " uri " has no content.") {:uri uri}))
        media (get content "mimeType")
        _ (when-not (= (normal-media-type mime-type) (normal-media-type media))
            (invalid! (str "The MCP App resource " uri " has the media type " (pr-str media) ", not "
                           mime-type ".")
                      {:uri uri :media-type media}))
        html (cond (string? (get content "text")) (get content "text")
                   (string? (get content "blob"))
                   (try (String. (.decode (Base64/getDecoder) ^String (get content "blob"))
                                 StandardCharsets/UTF_8)
                        (catch IllegalArgumentException _
                          (invalid! (str "The MCP App resource " uri " has a blob that is not base64.")
                                    {:uri uri}))))
        _ (when (str/blank? html)
            (invalid! (str "The MCP App resource " uri " has no HTML.") {:uri uri}))
        ui (let [ui (get-in content ["_meta" "ui"])] (if (map? ui) ui {}))
        domain (get ui "domain")]
    (cond-> {"html" html
             "csp" (declared-csp (get ui "csp"))
             "permissions" (granted-permissions (get ui "permissions"))
             "prefers_border" (true? (get ui "prefersBorder"))}
      (and (string? domain) (not (str/blank? domain))) (assoc "domain" domain))))

(defn envelope
  "The app instance that one tool call opened: what a surface needs to host it.
   `tool` is the raw MCP tool, `arguments` the call arguments, `result` the raw
   CallToolResult and `view` the result of [[view-resource]]. The MCP values keep
   their own camelCase keys, because the host hands them to the view as they are."
  [{:keys [server tool arguments result view call-id]}]
  (let [csp (get view "csp")
        granted (get view "permissions")]
    (cond-> {"protocol_version" protocol-version
             "call_id" call-id
             "server" server
             "tool" tool
             "resource_uri" (resource-uri tool)
             "arguments" (if (map? arguments) arguments {})
             "result" result
             "html" (get view "html")
             "csp" csp
             "content_security_policy" (content-security-policy csp)
             "permissions" granted
             "allow" (allow-attribute granted)
             "prefers_border" (boolean (get view "prefers_border"))}
      (get view "domain") (assoc "domain" (get view "domain")))))

(def ^:private app-methods
  "The MCP requests that a view may send to its own server through the host."
  #{"tools/call" "tools/list" "resources/read" "resources/list" "resources/templates/list"})

(defn app-method?
  "True for an MCP request that a view may send to its server through the host."
  [method]
  (contains? app-methods method))

(defn refuse-app-call
  "Why a view may not call `tool-name` of a server whose tools are `tools`, or nil.
   A view reaches only the tools of its own server that list \"app\"."
  [tools tool-name]
  (let [tool (first (filter #(= tool-name (get % "name")) tools))]
    (cond (not (string? tool-name)) "tools/call needs a tool name."
          (nil? tool) (str "The server has no tool " (pr-str tool-name) ".")
          (not (app-callable? tool)) (str "The tool " (pr-str tool-name) " is not visible to apps."))))
