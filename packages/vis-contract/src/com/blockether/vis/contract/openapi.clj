(ns com.blockether.vis.contract.openapi
  "Renders the validated built-in gateway contract as OpenAPI 3.1.
   Extension routes remain in their extension-owned contracts."
  (:require [clojure.string :as str]
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

(defn- responses
  [response]
  (array-map (if (= :empty response) "204" "200")
             (if (= :empty response)
               (array-map "description" "Accepted; no body.")
               (array-map "description" "Success." "content" (response-content response)))
             "default"
             (array-map "$ref" "#/components/responses/error")))

(defn- operation
  [method {:keys [path audience]} {:keys [request response]}]
  (let [body (when-let [content (request-content request)]
               (array-map "required" true "content" content))]
    (cond-> (array-map "operationId" (operation-id method path) "tags" [(name audience)])
      body
      (assoc "requestBody" body)

      true
      (assoc "responses" (responses response))

      ;; The document's default security covers the gated audiences; a public
      ;; route is answered without a token even when the gateway requires one.
      (= :public audience)
      (assoc "security" []))))

(defn- path-item
  [{:keys [path operations] :as route}]
  (let [item (into (sorted-map)
                   (map (fn [[method declaration]]
                          [(name method) (operation method route declaration)]))
                   operations)]
    (if-let [parameters (seq (path-parameters path))]
      (assoc item "parameters" (vec parameters))
      item)))

(def ^:private error-schema
  "The one error body every refused call answers with, as the contract spells it."
  (let [body-key
        (:error gateway/error-response-body-keys)

        {message-key :message type-key :type}
        gateway/error-response-error-keys]

    (array-map "type" "object"
               "required" [body-key]
               "properties"
               (array-map body-key
                          (array-map "type" "object"
                                     "required" [type-key message-key]
                                     "properties"
                                     (array-map type-key text-schema message-key text-schema))))))

(def ^:private components
  (array-map "securitySchemes"
             (array-map
               "bearer"
               (array-map
                 "type" "http"
                 "scheme" "bearer"
                 "description"
                 (str "The gateway secret. A gateway bound to loopback serves without it; "
                      "any other bind requires it on every route outside the public audience."))
               "gateway_secret"
               (array-map "type" "apiKey"
                          "in" "header"
                          "name" (gateway/header :gateway-secret)
                          "description"
                          "The same secret, as the header a same-machine client already sends."))
             "schemas" (array-map "error" error-schema)
             "responses"
             (array-map "error"
                        (array-map "description" "The call was refused; the body names the reason."
                                   "content" {"application/json"
                                              {"schema" {"$ref" "#/components/schemas/error"}}}))))

(defn document
  "The OpenAPI 3.1 document for the built-in gateway routes, string-keyed and
   ready to encode. Deterministic: the same contract renders the same bytes."
  []
  (array-map
    "openapi" openapi-version
    "info" (array-map
             "title" "Vis Gateway"
             "version" (str gateway/protocol-version)
             "summary" "The HTTP surface every Vis client speaks."
             "description"
             (str
               "Built-in routes only, rendered from the Vis gateway contract "
               "(document version " gateway/version
               ").\n\n"
               "`info.version` is the WIRE PROTOCOL version, which is what a client negotiates: "
               "a call carries it in `" (gateway/header :protocol)
               "`, and a gateway that cannot "
               "serve the caller's protocol answers 426 with its own numbers instead of the "
               "route's result. The routes that report the gateway's own identity answer any "
               "protocol, so a client can always read that verdict.\n\n"
               "Request and response bodies are JSON unless the media type says otherwise; a body "
               "shown as an empty schema is one the contract does not constrain yet.")
             "license" (array-map "name" "MIT"))
    "servers" [(array-map "url" "/" "description" "The gateway that served this document.")]
    "security" [(array-map "bearer" []) (array-map "gateway_secret" [])]
    "tags" [(array-map "name" "public" "description" "Answered without a token.")
            (array-map "name" "sdk" "description" "The client API.")
            (array-map "name" "administration" "description" "Local lifecycle and metrics.")]
    "x-vis-protocol" (array-map "version" gateway/protocol-version
                                "minimum_client" gateway/minimum-client-protocol
                                "minimum_gateway" gateway/minimum-gateway-protocol)
    "components" components
    "paths" (into (sorted-map)
                  (map (fn [route]
                         [(templated (:path route)) (path-item route)]))
                  gateway/route-table)))
