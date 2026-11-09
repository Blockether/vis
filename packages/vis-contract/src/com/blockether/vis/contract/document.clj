(ns com.blockether.vis.contract.document
  "Loads canonical JSON Schemas and validates payloads against their roots or definitions.
   `schema/common.json` supplies shared definitions. Raw validators reject values outside
   the JSON data model; engine adapters may normalize keyword maps before validation."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
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

(def ^:private error-messages
  "The one message table of schema and live view faults, `x-vis-error-messages` of the
   common schema. Every SDK renders it too."
  (delay (get (schema-document "common") "x-vis-error-messages")))

(defn message-table "The message table of `common.json`." [] @error-messages)

(defn clip-name
  "`text` cut to `most_name_chars` code points, so a cut never splits a character."
  [text]
  (let [text
        (str text)

        most
        (long (get @error-messages "most_name_chars"))]

    (if (> (.codePointCount text 0 (count text)) most)
      (subs text 0 (.offsetByCodePoints text 0 (int most)))
      text)))

(defn number-text
  "A schema number as text: an integral number without a fraction."
  [n]
  (if (and (number? n)
           (not (integer? n))
           (== (double n) (Math/rint (double n)))
           (< (Math/abs (double n)) 1e15))
    (str (long n))
    (str n)))

(defn- value-text
  [v]
  (cond (nil? v) "null"
        (string? v) v
        (number? v) (number-text v)
        :else (str v)))

(defn json-type
  "The JSON type name of a decoded value."
  [v]
  (cond (nil? v) "null"
        (boolean? v) "boolean"
        (integer? v) "integer"
        (number? v) "number"
        (string? v) "string"
        (sequential? v) "array"
        :else "object"))

(defn render
  "Template `template` with each `{name}` replaced by `(get values name)`."
  [template values]
  (str/replace template
               #"\{([a-z_]+)\}"
               (fn [[_ k]]
                 (str (get values k)))))

(defn rule-line
  "One fault line `{path} - {message} ({rule})` of the message table."
  [path message rule]
  (render (get @error-messages "line")
          {"path" (if (str/blank? (str path)) "/" path) "message" message "rule" rule}))

(defn named-line
  "The line of hand rule `rule-name` of the message table at `path`."
  ([path rule-name] (named-line path rule-name {}))
  ([path rule-name values]
   (let [[template rule] (get-in @error-messages ["rules" rule-name])]
     (rule-line path
                (render template
                        (update-vals values #(if (string? %) (clip-name %) (value-text %))))
                rule))))

(defn rule-text
  "The message of hand rule `rule-name` of the message table, without a path: the
   reason of a patch refusal that names its node itself."
  [rule-name values]
  (render (first (get-in @error-messages ["rules" rule-name])) (update-vals values value-text)))

(defn- pointer-get
  [value pointer]
  (reduce (fn [v part]
            (let [part (-> part
                           (str/replace "~1" "/")
                           (str/replace "~0" "~"))]
              (cond (map? v) (get v part)
                    (sequential? v) (nth v (parse-long part) nil)
                    :else nil)))
          value
          (rest (str/split (str pointer) #"/" -1))))

(defn- error-lines
  [value prefix {:keys [instanceLocation keyword params]}]
  (let [table
        (get @error-messages "keywords")

        instance
        (pointer-get value instanceLocation)

        line
        #(rule-line (str prefix instanceLocation) % keyword)]

    (case keyword
      "required"
      [(line (render (get table keyword) {"property" (clip-name (:missingProperty params))}))]

      "additionalProperties"
      [(line (render (get table keyword) {"property" (clip-name (:additionalProperty params))}))]

      "pattern"
      [(line (if (= (:pattern params) (get @error-messages "blank_pattern"))
               (get @error-messages "blank")
               (get table keyword)))]

      (let [limit
            (some-> (:limit params)
                    value-text)

            allowed
            (cond (contains? params :allowedValues)
                  (str/join ", " (map value-text (:allowedValues params)))
                  (contains? params :allowedValue) (value-text (:allowedValue params)))

            expected
            (let [t (:type params)]
              (if (sequential? t) (str/join " or " t) t))

            n
            (cond (string? instance) (.codePointCount ^String instance 0 (count instance))
                  (sequential? instance) (count instance))

            template
            (or (get table keyword) (get @error-messages "default"))]

        [(line (render template
                       {"limit" limit
                        "allowed" allowed
                        "expected" expected
                        "actual" (json-type instance)
                        "count" n
                        "rule" keyword
                        "format" (:format params)}))]))))

(defn explain-lines
  "The sorted, distinct fault lines of `explanation` for `value` (JSON data). A failed
   `oneOf` gives the faults of the one branch whose discriminator `const` holds. Without
   one such branch, it is one line and the faults of its branches are left out. A line never holds a
   value that a person wrote, only names and limits of the schema. `prefix` goes in front of
   each path, for a value that is a part of a larger one."
  ([explanation value] (explain-lines explanation value ""))
  ([explanation value prefix]
   (let [errors
         (vec (:errors explanation))

         dropped
         (reduce
           (fn [dropped {:keys [keyword keywordLocation] :as one}]
             (if (or (not= "oneOf" keyword) (contains? dropped one))
               dropped
               (let [prefix
                     (str keywordLocation "/")

                     by-branch
                     (group-by #(first (str/split (subs (:keywordLocation %) (count prefix)) #"/"))
                               (filter #(and (not (contains? dropped %))
                                             (str/starts-with? (str (:keywordLocation %)) prefix))
                                       errors))

                     ;; A branch whose discriminator const holds is the form the value chose.
                     chosen
                     (remove (fn [[branch faults]]
                               (some
                                 #(and (= "const" (:keyword %))
                                       (re-matches
                                         (re-pattern
                                           (str "\\Q" prefix branch "\\E/properties/[^/]+/const"))
                                         (:keywordLocation %)))
                                 faults))
                       by-branch)]

                 (if (= 1 (count chosen))
                   (into (conj dropped one) (mapcat val (dissoc by-branch (key (first chosen)))))
                   (into dropped (mapcat val by-branch))))))
           #{}
           (sort-by #(count (str (:keywordLocation %))) errors))]

     (->> errors
          (remove dropped)
          (mapcat #(error-lines value prefix %))
          distinct
          sort
          vec))))

(defn joined-lines
  "At most `most_shown` of `lines`, joined with `; `, then `and N more`."
  [lines]
  (let [most
        (long (get @error-messages "most_shown"))

        left
        (- (count lines) most)]

    (str (str/join "; " (take most lines))
         (when (pos? left) (str "; " (render (get @error-messages "more") {"count" left}))))))

(defn explain-text
  "The fault lines of `explanation` for `value`, joined, or nil when it has none."
  ([explanation] (explain-text explanation nil))
  ([explanation value]
   (when (seq (:errors explanation))
     (joined-lines (explain-lines explanation (wire/->wire value))))))

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
