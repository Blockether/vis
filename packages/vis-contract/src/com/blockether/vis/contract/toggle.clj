(ns com.blockether.vis.contract.toggle
  "Feature-toggle vocabulary and contribution validation from JSON Schema."
  (:require [clojure.string :as str]
            [com.blockether.vis.contract.document :as document]))

(def ^:private schema (delay (document/schema-document "toggle")))

(def scopes
  "Ordered settings scopes, from least to most specific."
  (get-in @schema ["$defs" "scope" "enum"]))

(def default-scopes
  "Allowed scopes when a declaration omits them."
  (get-in @schema ["$defs" "contribution" "properties" "scopes" "default"]))

(def id-pattern
  "Portable canonical toggle-id regular expression."
  (get-in @schema ["$defs" "contribution" "properties" "id" "pattern"]))

(def types
  "Closed feature-toggle kinds."
  (set (map keyword (get-in @schema ["$defs" "contribution" "properties" "type" "enum"]))))

(def default-type
  "Kind used when a contribution omits `:type`."
  (keyword (get-in @schema ["$defs" "contribution" "properties" "type" "default"])))

(def max-description-length
  "Maximum length of one settings-row description."
  (get-in @schema ["$defs" "contribution" "properties" "description" "maxLength"]))

(def ^:private single-line-pattern
  (get-in @schema ["$defs" "contribution" "properties" "description" "pattern"]))

(def boolean-true-tokens
  "Lower-case wire tokens that mean true."
  (set (get-in @schema ["$defs" "boolean_true" "enum"])))

(def boolean-false-tokens
  "Lower-case wire tokens that mean false."
  (set (get-in @schema ["$defs" "boolean_false" "enum"])))

(def ^:private id-regex (re-pattern id-pattern))

(defn toggle-id?
  "True only for canonical lower-case snake_case toggle ids."
  [value]
  (and (string? value) (boolean (re-matches id-regex value))))

(defn settings-description?
  "True for one non-blank settings-row line within the contract bound."
  [value]
  (and (string? value)
       (not (str/blank? value))
       (nil? (re-find #"[\r\n]" value))
       (<= (count value) (long max-description-length))))

(defn- semantic-contribution?
  [{:keys [type choices default visible-fn] :as value}]
  (and (or (not (contains? value :visible-fn)) (ifn? visible-fn))
       (case (or type default-type)
         :boolean
         (boolean? default)

         :enum
         (and (sequential? choices) (some? default) (contains? (set choices) default))

         false)))

(defn contribution-valid?
  "True when `value` satisfies the toggle contribution schema and callback semantics."
  [value]
  (and (document/valid? "toggle" "contribution" value) (semantic-contribution? value)))

(defn explain-contribution
  "JSON Schema errors for an invalid toggle contribution, or nil."
  [value]
  (document/explain "toggle" "contribution" value))

(defn- field-name
  [instance-location]
  (let [path (str/replace (str instance-location) #"^/" "")]
    (if (str/blank? path) "declaration" (str/replace path "/" "."))))

(defn- schema-problem
  "One readable line for a JSON Schema error: field, rule, limit and actual size."
  [{:keys [instanceLocation keyword params error]}]
  (let [field
        (field-name instanceLocation)

        {:keys [limit actual allowedValues missingProperty pattern]}
        params]

    (case keyword
      "maxLength"
      (str field
           " is "
           actual
           " characters; maximum is "
           limit
           " (maxLength). Shorten the "
           field
           ".")

      "minLength"
      (str field " is " actual " characters; minimum is " limit " (minLength).")

      "required"
      (str missingProperty " is missing (required).")

      "enum"
      (str field " must be one of: " (str/join ", " allowedValues) " (enum).")

      "pattern"
      (if (= pattern single-line-pattern)
        (str field " must be one line without line breaks (pattern).")
        (str field " does not match the pattern " pattern " (pattern)."))

      (str field ": " error " (" keyword ")."))))

(defn- semantic-problem
  [{:keys [type visible-fn] :as value}]
  (cond (and (contains? value :visible-fn) (not (ifn? visible-fn))) "visible-fn must be a function."
        (= :enum (or type default-type)) "default must be one of the choices."
        :else "default must be true or false for a boolean setting."))

(defn contribution-problems
  "Readable problems of a toggle contribution, one line for each failed rule.
   Each line names the field, the rule and its limit, never the value. Empty when valid."
  [value]
  (if-let [errors (seq (:errors (explain-contribution value)))]
    (mapv schema-problem errors)
    (if (semantic-contribution? value) [] [(semantic-problem value)])))

(defn contribution-message
  "One error message for an invalid contribution: `label`, the id when known, then each problem."
  [label value]
  (let [id (or (:id value) (get value "id"))]
    (str label (when (string? id) (str " " id)) ": " (str/join " " (contribution-problems value)))))
