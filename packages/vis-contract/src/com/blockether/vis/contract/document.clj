(ns com.blockether.vis.contract.document
  "Loads JSON contract documents and validates them with their same-named JSON Schemas.
   `schema/common.json` supplies shared definitions. Raw validators reject values outside
   the JSON data model; engine adapters may normalize keyword maps before validation."
  (:require [clojure.java.io :as io]
            [com.blockether.skjema.core :as skjema]
            [com.blockether.vis.contract.wire :as wire]))

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
(def ^:private schemas (atom {}))
(def ^:private validators (atom {}))

(defn- schema
  [document-name]
  (or (get @schemas document-name)
      (let [value (read-resource (str "vis-contract/schema/" document-name ".json"))]
        (swap! schemas assoc document-name value)
        value)))

(defn- compiled-schema
  [document-name definition]
  (let [cache-key [document-name definition]]
    (or (get @validators cache-key)
        (let [source (schema document-name)
              schema-id (get source "$id")
              target (if definition
                       (-> (select-keys source ["$schema" "$id" "$defs"])
                           (assoc "$ref" (str "#/$defs/" definition)))
                       source)
              compiled (skjema/compile-schema target
                                              {:base schema-id
                                               :registry {(get @common-schema "$id") @common-schema}
                                               :format-assertion true})]

          (swap! validators assoc cache-key compiled)
          compiled))))

(defn explain
  "JSON Schema errors for `value`, or nil when it satisfies the document root or
   named definition. Clojure keyword maps are normalized to their JSON spelling
   before definition validation."
  ([document-name value] (skjema/explain (compiled-schema document-name nil) value))
  ([document-name definition value]
   (skjema/explain (compiled-schema document-name definition) (wire/->wire value))))

(defn valid?
  "True when `value` satisfies the document root or named JSON Schema definition."
  ([document-name value] (nil? (explain document-name value)))
  ([document-name definition value] (nil? (explain document-name definition value))))

(defn- json-value?
  [value]
  (cond (or (nil? value) (string? value) (boolean? value)) true
        (number? value) (and (not (Double/isNaN (double value)))
                             (not (Double/isInfinite (double value))))
        (vector? value) (every? json-value? value)
        (map? value) (and (every? string? (keys value)) (every? json-value? (vals value)))
        :else false))

(defn explain-json
  "JSON Schema errors without converting Clojure values into JSON spellings."
  [document-name definition value]
  (if (json-value? value)
    (skjema/explain (compiled-schema document-name definition) value)
    {:errors [{:message "value is not JSON data" :value value}]}))

(defn valid-json?
  "True when raw JSON-shaped data satisfies a named definition."
  [document-name definition value]
  (nil? (explain-json document-name definition value)))

(defn load!
  "The contract document named `document-name`, parsed and validated against its
   schema. Throws `:vis/contract-missing` when either file is off the classpath
   and `:vis/contract-invalid`, carrying the schema errors, when the document does
   not satisfy the schema."
  [document-name]
  (let [resource-path
        (str "vis-contract/" document-name ".json")

        parsed
        (read-resource resource-path)]

    (when-let [{:keys [errors]} (skjema/explain (compiled-schema document-name nil) parsed)]
      (throw (ex-info
               (str resource-path " does not satisfy vis-contract/schema/" document-name ".json")
               {:type :vis/contract-invalid :resource resource-path :errors errors})))
    parsed))
