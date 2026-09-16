(ns com.blockether.vis.contract.toggle
  "Feature-toggle vocabulary and contribution validation from JSON Schema."
  (:require [clojure.string :as str]
            [com.blockether.vis.contract.document :as document]))

(def ^:private schema (delay (document/schema-document "toggle")))

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
