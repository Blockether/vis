(ns com.blockether.vis.contract.toggle
  "The executable feature-toggle contribution contract.

   `vis-contract/toggle.edn` owns the portable id grammar, toggle kinds, description
   bound and boolean token vocabulary. This namespace validates that document,
   declares the contribution shape and renders the same vocabulary for every SDK.
   Runtime registries, values, listeners, persistence and hydration remain in Core."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]))

(set! *warn-on-reflection* true)

(defn- non-blank-string? [x] (and (string? x) (not (str/blank? x))))
(defn- closed-map? [m expected-keys] (and (map? m) (= expected-keys (set (keys m)))))

(defn- sorted-token-vector?
  [value]
  (and (vector? value)
       (seq value)
       (= value (vec (sort value)))
       (= (count value) (count (set value)))
       (every? #(and (non-blank-string? %) (= % (str/lower-case %))) value)))

(defn- regex-string?
  [value]
  (and (non-blank-string? value)
       (try (boolean (re-pattern value)) (catch java.util.regex.PatternSyntaxException _ false))))

(defn- valid-document?
  [{:contract/keys [version]
    :toggle/keys [id-pattern types default-type max-description-length boolean-wire config-truthy]
    :as value}]
  (let [true-tokens
        (:true boolean-wire)

        false-tokens
        (:false boolean-wire)]

    (and (closed-map? value
                      #{:contract/version :toggle/id-pattern :toggle/types :toggle/default-type
                        :toggle/max-description-length :toggle/boolean-wire :toggle/config-truthy})
         (pos-int? version)
         (regex-string? id-pattern)
         (sorted-token-vector? types)
         (contains? (set types) default-type)
         (pos-int? max-description-length)
         (closed-map? boolean-wire #{:true :false})
         (sorted-token-vector? true-tokens)
         (sorted-token-vector? false-tokens)
         (empty? (set/intersection (set true-tokens) (set false-tokens)))
         (sorted-token-vector? config-truthy)
         (set/subset? (set config-truthy) (set true-tokens)))))

(s/def :contract/toggle valid-document?)

(def ^:private resource-path "vis-contract/toggle.edn")

(def ^:private document
  (delay
    (let [resource
          (io/resource resource-path)

          _
          (when-not resource
            (throw (ex-info (str "the toggle contract is missing from the classpath: "
                                 resource-path)
                            {:type :vis/contract-missing :resource resource-path})))

          parsed
          (edn/read-string (slurp resource))]

      (when-not (s/valid? :contract/toggle parsed)
        (throw (ex-info (str resource-path " is not a valid toggle contract")
                        {:type :vis/contract-invalid
                         :resource resource-path
                         :explain (s/explain-str :contract/toggle parsed)})))
      parsed)))

(def version "Feature-toggle contract document version." (:contract/version @document))
(def id-pattern "Portable canonical toggle-id regular expression." (:toggle/id-pattern @document))
(def types "Closed feature-toggle kinds." (set (map keyword (:toggle/types @document))))
(def default-type
  "Kind used when a contribution omits `:type`."
  (keyword (:toggle/default-type @document)))
(def max-description-length
  "Maximum length of one settings-row description."
  (:toggle/max-description-length @document))
(def boolean-true-tokens
  "Lower-case wire tokens that mean true."
  (set (get-in @document [:toggle/boolean-wire :true])))
(def boolean-false-tokens
  "Lower-case wire tokens that mean false."
  (set (get-in @document [:toggle/boolean-wire :false])))
(def config-truthy-tokens
  "Lower-case YAML string tokens that hydrate to true; every other string is false."
  (set (:toggle/config-truthy @document)))

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
             "types" (:toggle/types @document)
             "default_type" (:toggle/default-type @document)
             "max_description_length" max-description-length
             "boolean_wire" (array-map "true" (get-in @document [:toggle/boolean-wire :true])
                                       "false" (get-in @document [:toggle/boolean-wire :false]))
             "config_truthy" (:toggle/config-truthy @document)))
