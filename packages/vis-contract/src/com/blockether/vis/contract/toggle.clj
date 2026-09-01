(ns com.blockether.vis.contract.toggle
  "The executable feature-toggle contribution contract.

   `vis-contract/toggle.json` owns the portable id grammar, toggle kinds, description
   bound and boolean token vocabulary, and `vis-contract/schema/toggle.json` declares
   its shape. This namespace reads that document, declares the contribution shape and
   renders the same vocabulary for every SDK.
   Runtime registries, values, listeners, persistence and hydration remain in Core."
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [com.blockether.vis.contract.document :as document]))

(set! *warn-on-reflection* true)

(defn- non-blank-string? [x] (and (string? x) (not (str/blank? x))))

(def ^:private document (delay (document/load! "toggle")))

(def version "Feature-toggle contract document version." (get @document "version"))
(def id-pattern "Portable canonical toggle-id regular expression." (get @document "id_pattern"))
(def types "Closed feature-toggle kinds." (set (map keyword (get @document "types"))))
(def default-type
  "Kind used when a contribution omits `:type`."
  (keyword (get @document "default_type")))
(def max-description-length
  "Maximum length of one settings-row description."
  (get @document "max_description_length"))
(def boolean-true-tokens
  "Lower-case wire tokens that mean true."
  (set (get-in @document ["boolean_wire" "true"])))
(def boolean-false-tokens
  "Lower-case wire tokens that mean false."
  (set (get-in @document ["boolean_wire" "false"])))
(def config-truthy-tokens
  "Lower-case YAML string tokens that hydrate to true; every other string is false."
  (set (get @document "config_truthy")))

(def ^:private id-regex (re-pattern id-pattern))

(defn toggle-id?
  "True only for canonical lower-case snake_case toggle ids."
  [value]
  (and (string? value) (boolean (re-matches id-regex value))))

(defn settings-description?
  "True for one non-blank settings-row line within the portable length bound."
  [value]
  (and (non-blank-string? value)
       (nil? (re-find #"[\r\n]" value))
       (<= (count value) (long max-description-length))))

(s/def ::id toggle-id?)
(s/def ::label non-blank-string?)
(s/def ::description settings-description?)
(s/def ::default any?)
(s/def ::owner
  (s/or :internal #{:vis}
        :extension string?))
(s/def ::since string?)
(s/def ::persist? boolean?)
(s/def ::settings? boolean?)
(s/def ::group keyword?)
(s/def ::type types)
(s/def ::choices (s/and (s/coll-of any?) seq))
(s/def ::channels (s/and (s/coll-of keyword?) seq))
(s/def ::visible-fn ifn?)

(s/def ::spec
  (s/and (s/keys :req-un [::id ::label ::default]
                 :opt-un [::description ::owner ::since ::persist? ::settings? ::group ::type
                          ::choices ::visible-fn ::channels])
         (fn [{:keys [type choices default]}]
           (case (or type default-type)
             :boolean
             (boolean? default)

             :enum
             (and (sequential? choices) (some? default) (contains? (set choices) default))

             false))))

(defn spec-valid?
  "True when `value` is a valid toggle contribution."
  [value]
  (s/valid? ::spec value))

(defn explain-spec
  "Spec explain data for an invalid toggle contribution, or nil when valid."
  [value]
  (s/explain-data ::spec value))

(defn package-document
  "Deterministic JSON-ready toggle section for every generated language contract."
  []
  (array-map "version" version
             "id_pattern" id-pattern
             "types" (get @document "types")
             "default_type" (get @document "default_type")
             "max_description_length" max-description-length
             "boolean_wire" (array-map "true" (get-in @document ["boolean_wire" "true"])
                                       "false" (get-in @document ["boolean_wire" "false"]))
             "config_truthy" (get @document "config_truthy")))
