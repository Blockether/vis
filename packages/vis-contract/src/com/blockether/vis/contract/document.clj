(ns com.blockether.vis.contract.document
  "Loads canonical JSON Schemas and validates payloads against their roots or definitions.
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

(def ^:private documents (atom {}))

(def ^:private validators (atom {}))

(defn schema-document
  "The parsed canonical JSON Schema named `document-name`, cached after its first read."
  [document-name]
  (or (get @schemas document-name)
      (let [value (read-resource (str "vis-contract/schema/" document-name ".json"))]
        (swap! schemas assoc document-name value)
        value)))

(def ^:private schema-base
  "The URI prefix of the contract schemas: the `$id` of `common.json` without its file name."
  (delay (let [id (str (get @common-schema "$id"))]
           (subs id 0 (- (count id) (count "common.json"))))))

(defn ref-document
  "The schema document that `$ref` value `ref` names, or nil for a local or foreign ref.
   A ref names a document by its file name or by its full `$id`."
  [ref]
  (let [ref
        (str ref)

        ref
        (if (.startsWith ref ^String @schema-base) (subs ref (count @schema-base)) ref)]

    (second (re-matches #"([a-z_-]+)\.json#.*" ref))))

(defn- referenced-documents
  "The names of the other schema documents that `schema` names in a `$ref`."
  [schema]
  (into #{}
        (keep (fn [node]
                (when (and (map-entry? node) (= "$ref" (key node))) (ref-document (val node)))))
        (tree-seq coll? seq schema)))

(defn- dependency-documents
  "The schema documents that `document-name` reaches through `$ref`, directly or
   through another document. The registry follows the schemas, never a list."
  [document-name]
  (loop [pending
         (referenced-documents (schema-document document-name))

         seen
         #{document-name}]

    (if-let [next-name (first (remove seen pending))]
      (recur (into (disj pending next-name) (referenced-documents (schema-document next-name)))
             (conj seen next-name))
      (map schema-document (disj seen document-name)))))

(defn- compiled-document
  "The compiled root of `document-name`. Each document compiles once; its definitions reuse it."
  [document-name]
  (or (get @documents document-name)
      (let [source
            (schema-document document-name)

            dependencies
            (dependency-documents document-name)

            compiled
            (skjema/compile-schema source
                                   {:base (get source "$id")
                                    :registry (into {(get @common-schema "$id") @common-schema}
                                                    (map (juxt #(get % "$id") identity))
                                                    dependencies)
                                    :format-assertion true})]

        (swap! documents assoc document-name compiled)
        compiled)))

(defn- compiled-schema
  [document-name definition]
  (let [cache-key [document-name definition]]
    (or (get @validators cache-key)
        (let [document (compiled-document document-name)
              compiled
              (if definition (skjema/compile-ref document (str "#/$defs/" definition)) document)]

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
