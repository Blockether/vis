(ns com.blockether.vis.contract.openapi
  "Renders the gateway payload schemas and HTTP metadata as OpenAPI 3.1.
   Extension routes remain in their extension-owned contracts."
  (:require [clojure.string :as str]
            [clojure.walk :as walk]
            [com.blockether.vis.contract.document :as document]
            [com.blockether.vis.contract.gateway :as gateway]))

(def ^:private openapi-version "OpenAPI specification version this document is written to." "3.1.1")

(def ^:private any-schema
  "The empty schema: this body is JSON, and the contract does not yet say more."
  {})

(def ^:private binary-schema {"type" "string" "format" "binary"})

(def ^:private text-schema {"type" "string"})

(def ^:private request-content
  "Request transport to OpenAPI media types. `:none` carries no body at all."
  {:json {"application/json" {"schema" any-schema}}
   :binary {"application/octet-stream" {"schema" binary-schema}}})

(def ^:private response-content
  "Successful response transport to OpenAPI media types. `:empty` answers 204 and
   so appears here as no content at all."
  {:json {"application/json" {"schema" any-schema}}
   :sse {"text/event-stream" {"schema" text-schema}}
   :html {"text/html" {"schema" text-schema}}
   :markdown {"text/markdown" {"schema" text-schema}}
   :binary {"application/octet-stream" {"schema" binary-schema}}
   :resource {"*/*" {}}
   :negotiated {"application/json" {"schema" any-schema} "text/plain" {"schema" text-schema}}})

(defn- parameter-name
  "Name of the path parameter `segment` carries, or nil when it is a literal.
   `:sid` is one segment; `*path` is the rest of the path."
  [segment]
  (when (and (seq segment) (contains? #{\: \*} (first segment))) (subs segment 1)))

(defn- templated
  "Contract path with its `:name`/`*name` segments in OpenAPI's `{name}` spelling."
  [path]
  (->> (str/split path #"/")
       (map (fn [segment]
              (if-let [n (parameter-name segment)]
                (str "{" n "}")
                segment)))
       (str/join "/")))

(defn- path-parameters
  [path]
  (into []
        (keep (fn [segment]
                (when-let [n (parameter-name segment)]
                  (array-map "name" n
                             "in" "path"
                             "required" true
                             "description" (if (= \* (first segment))
                                             "The rest of the path, slashes included."
                                             "One path segment.")
                             "schema" text-schema))))
        (str/split path #"/")))

(defn- operation-id
  "Stable identifier a generated client names this call by."
  [method path]
  (->> (str/split (templated path) #"[^A-Za-z0-9]+")
       (remove str/blank?)
       (cons (name method))
       (str/join "_")))

(defn- schema-ref
  [ref]
  {"$ref" (if (str/includes? ref ".json")
            ref
            (str "#/$defs/" ref))})

(defn- content-with-schema
  [content schema]
  (cond-> content
    (and schema (contains? content "application/json"))
    (assoc-in ["application/json" "schema"] (schema-ref schema))))

(defn- header-fields
  [names]
  (into (sorted-map) (map (fn [header] [header {"schema" text-schema}])) names))

(defn- responses
  [{:keys [response response-schema] :strs [statuses]}]
  (let [statuses (or statuses
                     {(case response :empty "204" :websocket "101" "200") {}})]
    (into (sorted-map
            "4XX" {"$ref" "#/components/responses/error"}
            "5XX" {"$ref" "#/components/responses/error"}
            "default" {"$ref" "#/components/responses/error"})
          (map (fn [[status {:strs [transport media schema headers description]}]]
                 (let [transport (if transport (keyword transport) response)
                       schema (or schema (some-> response-schema name))
                       content (if media
                                 (into (sorted-map)
                                       (map (fn [media-type]
                                              [media-type {"schema" (if (str/starts-with? media-type "text/")
                                                                        text-schema
                                                                        binary-schema)}]))
                                       media)
                                 (response-content transport))]
                   [status (cond-> {"description" (or description "The result of the call.")}
                             content
                             (assoc "content" (content-with-schema content schema))

                             (seq headers)
                             (assoc "headers" (header-fields headers)))])))
          statuses)))

(def ^:private gateway-security [{"bearer" []} {"gateway_secret" []}])

(defn- operation
  [method {:keys [path audience]}
   {:keys [request response request-schema] :strs [parameters body_required request_media protocol_exempt security]
    :as declaration}]
  (let [content (if request_media
                  (into (sorted-map)
                        (map (fn [media-type] [media-type {"schema" binary-schema}]))
                        request_media)
                  (request-content request))
        body (when content
               (array-map "required" (not= false body_required)
                          "content" (content-with-schema content (some-> request-schema name))))]
    (cond-> (array-map "operationId" (operation-id method path)
                       "tags" [(name audience)]
                       "responses" (cond-> (responses declaration)
                                     (not protocol_exempt)
                                     (assoc "426" {"$ref" "#/components/responses/incompatible_protocol"})))
      body
      (assoc "requestBody" body)

      (seq parameters)
      (assoc "parameters" parameters)

      ;; The document's default security covers the gated audiences.
      (or (= :public audience) (= "public" security))
      (assoc "security" [])

      (= "content_ticket" security)
      (assoc "security" [{"content_ticket" []}])

      (= "gateway_or_content_ticket" security)
      (assoc "security" (conj gateway-security {"content_ticket" []}))

      (= :websocket response)
      (assoc "security" [{"voice_ticket" []}]))))

(defn- path-item
  [{:keys [path operations] :as route}]
  (let [item (into (sorted-map)
                   (map (fn [[method declaration]]
                          [(name method) (operation method route declaration)]))
                   operations)]
    (if-let [parameters (seq (path-parameters path))]
      (assoc item "parameters" (vec parameters))
      item)))

(defn- rewrite-refs
  "Make canonical schema references local to an OpenAPI document."
  [schema source-name dependencies]
  (walk/postwalk
    (fn [value]
      (if-let [ref (and (map? value) (get value "$ref"))]
        (if (str/starts-with? ref "#/components/")
          value
          (let [[file fragment] (str/split ref #"#" 2)
                target (if (str/blank? file)
                         source-name
                         (or (document/ref-document (str file "#"))
                             (throw (ex-info "Unknown contract reference" {:ref ref}))))
                pointer (or fragment "")]
            (when-not (= "gateway" target)
              (vswap! dependencies conj target))
            (assoc value "$ref"
                         (if (= "gateway" target)
                           (str "#/components/schemas/" (subs pointer (count "/$defs/")))
                           (str "#/components/schemas/schema." target pointer)))))
        value))
    schema))

(defn- bundle-schemas
  [paths]
  (let [dependencies (volatile! #{})
        paths (rewrite-refs paths "gateway" dependencies)
        gateway (rewrite-refs (get (document/schema-document "gateway") "$defs")
                              "gateway" dependencies)]
    (loop [schemas gateway
           seen #{}]
      (if-let [target (first (sort (remove seen @dependencies)))]
        (let [schema (dissoc (document/schema-document target) "$id" "$schema")]
          (recur (assoc schemas (str "schema." target) (rewrite-refs schema target dependencies))
                 (conj seen target)))
        [paths schemas]))))

(def ^:private components
  (array-map
    "securitySchemes"
    (array-map
      "bearer"
      (array-map
        "type" "http"
        "scheme" "bearer"
        "description"
        (str
          "Loopback routes do not require a gateway secret. "
          "Other binds require it outside the public audience. Live voice media requires its own ticket."))
      "gateway_secret" (array-map
                         "type" "apiKey"
                         "in" "header"
                         "name" "x-vis-gateway-secret"
                         "description"
                         "The same secret, as the header a same-machine client already sends.")
      "content_ticket"
      (array-map "type" "apiKey" "in" "query" "name" "ticket"
                 "description" "Use the signed ticket and expiry from the link response.")
      "voice_ticket"
      (array-map
        "type" "apiKey"
        "in" "header"
        "name" "Sec-WebSocket-Protocol"
        "description"
        (str
          "Send vis-voice.v1 and vis-ticket.<ticket> as subprotocols. "
          "Use the one-use ticket from the start response. Keep the Origin from the start request.")))
    "responses"
    (array-map "error"
               (array-map "description" "The call was refused; the body names the reason."
                          "content" {"application/json"
                                     {"schema" {"$ref" "#/components/schemas/error_response"}}})
               "incompatible_protocol"
               (array-map "description" "The gateway does not support the caller's protocol."
                          "content" {"application/json"
                                     {"schema" {"$ref" "#/components/schemas/incompatible_protocol"}}}))))

(defn- build-document
  []
  (let [[paths schemas] (bundle-schemas (into (sorted-map)
                                             (map (fn [route]
                                                    [(templated (:path route)) (path-item route)]))
                                             gateway/route-table))]
    (array-map
    "openapi" openapi-version
    "info"
    (array-map
      "title" "Vis Gateway"
      "version" (str gateway/protocol-version)
      "summary" "The HTTP surface every Vis client speaks."
      "description"
      (str "Built-in routes only, rendered from the Vis gateway JSON Schema and HTTP metadata.\n\n"
           "`info.version` is the WIRE PROTOCOL version, which is what a client negotiates: "
           "a call carries it in `x-vis-protocol"
           "`, and a gateway that cannot "
           "serve the caller's protocol answers 426 with its own numbers instead of the "
           "route's result. The routes that report the gateway's own identity answer any "
           "protocol, so a client can always read that verdict.\n\n"
           "Request and response bodies are JSON unless the media type says otherwise. Schema "
           "references resolve within this document.")
      "license" (array-map "name" "MIT"))
    "servers" [(array-map "url" "/" "description" "The gateway that served this document.")]
    "security" gateway-security
    "tags" [(array-map "name" "public" "description" "Answered without a token.")
            (array-map "name" "sdk" "description" "The client API.")
            (array-map "name" "administration" "description" "Local lifecycle and metrics.")]
    "x-vis-protocol" (array-map "version" gateway/protocol-version
                                "minimum_client" gateway/minimum-client-protocol
                                "minimum_gateway" gateway/minimum-gateway-protocol)
      "components" (assoc components "schemas" schemas)
      "paths" paths)))

(def ^:private built-document (delay (build-document)))

(defn document
  "The cached OpenAPI 3.1 document for the built-in gateway routes."
  []
  @built-document)
