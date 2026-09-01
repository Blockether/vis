(ns com.blockether.vis.contract.document
  "One contract document, read as JSON and validated against its JSON Schema.

   `resources/vis-contract/<name>.json` IS the declaration: string keys,
   snake_case, the same shape the rendered `contract.json` carries, so a reader in
   any language sees one spelling. Its schema is
   `resources/vis-contract/schema/<name>.json`, which keeps the small reusable
   definitions in `$defs` and `$ref`s the cross-document ones from
   `schema/common.json` — the shape of a document is DECLARED there, never
   re-implemented as predicates beside the accessors.

   This is the only place either file is parsed. An invalid document throws while
   loading, with the schema's own errors, instead of surfacing later as a missing
   key in whatever happened to read it first."
  (:require [clojure.java.io :as io]
            [com.blockether.skjema.core :as skjema]))

(set! *warn-on-reflection* true)

(defn- read-resource
  "The JSON resource at `resource-path`, with its keys left as written."
  [resource-path]
  (let [resource (io/resource resource-path)]
    (when-not resource
      (throw (ex-info (str "the contract resource is missing from the classpath: " resource-path)
                      {:type :vis/contract-missing :resource resource-path})))
    (skjema/read-schema resource)))

(def ^:private common-schema (delay (read-resource "vis-contract/schema/common.json")))

(defn- compiled-schema
  "The compiled schema for one document, with `schema/common.json` in the registry
   so its `$ref`s resolve without a network fetch."
  [document-name]
  (let [schema (read-resource (str "vis-contract/schema/" document-name ".json"))]
    (skjema/compile-schema schema
                           {:base (get schema "$id")
                            :registry {(get @common-schema "$id") @common-schema}
                            ;; `format` annotates unless asked to assert, and the
                            ;; contract means its `regex`/`uri` formats as constraints.
                            :format-assertion true})))

(defn load!
  "The contract document named `document-name`, parsed and validated against its
   schema. Throws `:vis/contract-missing` when either file is off the classpath
   and `:vis/contract-invalid`, carrying the schema errors, when the document does
   not satisfy the schema."
  [document-name]
  (let [resource-path
        (str "vis-contract/" document-name ".json")

        parsed
        (read-resource resource-path)

        result
        (skjema/validate (compiled-schema document-name) parsed)]

    (when-not (:valid result)
      (throw (ex-info
               (str resource-path " does not satisfy vis-contract/schema/" document-name ".json")
               {:type :vis/contract-invalid :resource resource-path :errors (:errors result)})))
    parsed))
