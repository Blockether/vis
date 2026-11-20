(ns com.blockether.vis.contract.openapi-test
  "The OpenAPI document is the gateway contract in the spelling a client generator
   reads, so what it may never do is disagree with the contract or carry a value a
   JSON reader cannot hold."
  (:require [charred.api :as charred]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.skjema.core :as skjema]
            [com.blockether.vis.contract.gateway :as contract]
            [com.blockether.vis.contract.openapi :as openapi]
            [com.blockether.vis.contract.wire :as wire]
            [lazytest.core :refer [defdescribe expect it]]))

(def ^:private document (delay (openapi/document)))

(def ^:private encoded (delay (charred/read-json (wire/json-str @document))))

(defn- operations
  "Every `[path method operation]` the document declares."
  [encoded-document]
  (for [[path item]
        (get encoded-document "paths")

        [method operation]
        item

        :when (not= "parameters" method)]

    [path method operation]))

(defn- offending-parts
  "Map keys that are not strings and leaf values a JSON reader cannot hold."
  [value]
  (cond (map? value) (concat (remove string? (keys value)) (mapcat offending-parts (vals value)))
        (sequential? value) (mapcat offending-parts value)
        (or (string? value) (number? value) (boolean? value) (nil? value)) nil
        :else [value]))

(defdescribe
  openapi-document-test
  (it "describes every built-in route exactly once and nothing else"
      (expect (= (count contract/route-table) (count (get @encoded "paths"))))
      (expect (= (count (contract/route-methods)) (count (operations @encoded))))
      (expect (= (set
                   (map (fn [{:keys [path]}]
                          (str/replace (str/replace path #":([^/]+)" "{$1}") #"\*([^/]+)" "{$1}"))
                        contract/route-table))
                 (set (keys (get @encoded "paths")))))
      ;; The document describes the route that serves it, because that route is
      ;; declared in the same contract as every other.
      (expect (contains? (get @encoded "paths") "/openapi.json")))
  (it "names every operation once"
      (let [ids (map (fn [[_ _ operation]]
                       (get operation "operationId"))
                     (operations @encoded))]
        (expect (= (count ids) (count (set ids))))
        (expect (every? #(re-matches #"[a-z]+(?:_[a-z0-9]+)*" %) ids))
        (expect (= "get_v1_sessions_sid_turns_tid_trace"
                   (get-in @encoded
                           ["paths" "/v1/sessions/{sid}/turns/{tid}/trace" "get" "operationId"])))))
  (it "declares a path parameter for every templated segment"
      (doseq [[path item] (get @encoded "paths")]
        (expect (= (mapv second (re-seq #"\{([^}]+)\}" path))
                   (mapv #(get % "name") (get item "parameters" []))))))
  (it "turns each declared transport into its media type"
      (expect (= {"application/json" {"schema" {}}}
                 (get-in @encoded ["paths" "/v1/sessions" "post" "requestBody" "content"])))
      (expect (= {"application/octet-stream" {"schema" {"type" "string" "format" "binary"}}}
                 (get-in @encoded
                         ["paths" "/v1/sessions/{sid}/attachments" "post" "requestBody"
                          "content"])))
      (expect (nil? (get-in @encoded ["paths" "/v1/models" "get" "requestBody"])))
      (expect (= ["text/event-stream"]
                 (keys (get-in @encoded ["paths" "/v1/events" "get" "responses" "200" "content"]))))
      (expect (= ["text/markdown"]
                 (keys (get-in @encoded
                               ["paths" "/v1/sessions/{sid}/transcript.md" "get" "responses" "200"
                                "content"]))))
      (expect (= #{"application/json" "text/plain"}
                 (set (keys (get-in @encoded
                                    ["paths" "/metrics" "get" "responses" "200" "content"])))))
      ;; An empty response answers 204 and carries no content at all.
      (expect (= #{"204" "4XX" "5XX" "default"}
                 (set (keys (get-in @encoded
                                    ["paths" "/v1/sessions/{sid}/release" "post" "responses"])))))
      (expect (every? (fn [[_ _ operation]]
                        (every? #(= {"$ref" "#/components/responses/error"}
                                    (get-in operation ["responses" %]))
                                ["4XX" "5XX" "default"]))
                      (operations @encoded))))
  (it "gives each JSON answer a schema that lists its fields"
      ;; An empty schema tells a client generator nothing, so it makes an untyped call.
      (doseq [[path method operation]
              (operations @encoded)

              :let [content
                    (get-in operation ["responses" "200" "content"])

                    schema
                    (get-in content ["application/json" "schema"])]
              ;; A negotiated answer such as /metrics is not a JSON route.
              :when (and schema (= 1 (count content)))]

        (expect (not= {} schema) (str method " " path))
        (expect (some? (get-in @encoded
                               ["components" "schemas"
                                (str/replace (get schema "$ref" "") "#/components/schemas/" "")]))
                (str method " " path))))
  (it "is a valid OpenAPI 3.1 document"
      ;; The official OpenAPI 3.1 schema (2022-10-07), kept next to this test.
      (let [meta-schema (skjema/compile-schema
                          (charred/read-json
                            (slurp (io/resource
                                     "com/blockether/vis/contract/oas-3.1-schema.json"))))]
        (expect (nil? (skjema/explain meta-schema @encoded))
                (pr-str (skjema/explain meta-schema @encoded)))))
  (it "requires a one-use ticket for media instead of inherited bearer authentication"
      (let [operation (get-in @encoded ["paths" "/v1/sessions/{sid}/voice/live/{vid}/media" "get"])]
        (expect (= [{"voice_ticket" []}] (get operation "security")))
        (expect (= #{"101" "4XX" "5XX" "default"} (set (keys (get operation "responses")))))
        (expect (= "Sec-WebSocket-Protocol"
                   (get-in @encoded ["components" "securitySchemes" "voice_ticket" "name"])))))
  (it "gates every audience but the public one"
      (expect (= [{"bearer" []} {"gateway_secret" []}] (get @encoded "security")))
      (expect (= [] (get-in @encoded ["paths" "/healthz" "get" "security"])))
      (expect (= [] (get-in @encoded ["paths" "/openapi.json" "get" "security"])))
      (expect (nil? (get-in @encoded ["paths" "/v1/models" "get" "security"])))
      (expect (= "x-vis-gateway-secret"
                 (get-in @encoded ["components" "securitySchemes" "gateway_secret" "name"])))
      (expect (= (set (map (comp name :audience) contract/route-table))
                 (set (mapcat (fn [[_ _ operation]]
                                (get operation "tags"))
                              (operations @encoded))))))
  (it "answers the error envelope the contract declares"
      (let [schema (get-in @encoded ["components" "schemas" "error_response"])]
        (expect (= ["error"] (get schema "required")))
        (expect (= ["type" "message"] (get-in schema ["properties" "error" "required"])))))
  (it "renders the same bytes every time, and only what JSON can hold"
      (expect (= (wire/json-str (openapi/document)) (wire/json-str (openapi/document))))
      (expect (= [] (vec (offending-parts @document))))
      (expect (= "3.1.1" (get @encoded "openapi")))
      (expect (= (str contract/protocol-version) (get-in @encoded ["info" "version"])))
      (expect (= contract/protocol-version (get-in @encoded ["x-vis-protocol" "version"])))))

(defdescribe
  session-goal-schema-test
  (it "exposes the canonical goal on detail, creation, mutation and list responses"
      (doseq [[path method shape]
              [["/v1/sessions/{sid}" "get" "session"] ["/v1/sessions/{sid}" "patch" "session"]
               ["/v1/sessions" "post" "session_created"] ["/v1/sessions" "get" "session_list"]]]
        (expect (= {"$ref" (str "#/components/schemas/" shape)}
                   (get-in @encoded
                           ["paths" path method "responses" "200" "content" "application/json"
                            "schema"]))))
      (doseq [shape ["session" "session_created"]]
        (expect (every? (set (get-in @encoded ["components" "schemas" shape "required"]))
                        ["id" "goal"])
                shape)
        (expect (= {"$ref" "#/components/schemas/session_goal"}
                   (get-in @encoded ["components" "schemas" shape "properties" "goal" "anyOf" 1]))
                shape))
      (expect (= (set (keys contract/session-goal-labels))
                 (set (map #(get % "const")
                           (get-in @encoded
                                   ["components" "schemas" "session_goal" "properties" "status"
                                    "oneOf"])))))))
