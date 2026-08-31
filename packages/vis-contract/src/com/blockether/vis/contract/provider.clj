(ns com.blockether.vis.contract.provider
  "The executable provider limits contract.

   `vis-contract/provider.edn` owns the portable limits vocabulary: report statuses,
   what one row is measured against, what it counts, how its window is anchored, and
   how exactly and from where its numbers are known. This namespace validates that
   document, declares the envelope every provider limits report is judged by, and
   renders the same vocabulary for every SDK.

   A provider's `:provider/limits-fn` is UNTRUSTED input — an extension's Clojure or
   Python, answering whatever it answers. The host runs the result through
   [[report-valid?]] and swaps in an error envelope when a row is malformed, so this
   shape is exactly what a channel may assume it is painting. Fetching, caching, static
   catalog augmentation and error classification stay in Core."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
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

(def ^:private limit-vocabularies
  #{:statuses :scopes :kinds :window-kinds :window-units :precisions :sources})

(defn- valid-document?
  [{:contract/keys [version] :provider/keys [limits] :as value}]
  (and (closed-map? value #{:contract/version :provider/limits})
       (pos-int? version)
       (closed-map? limits limit-vocabularies)
       (every? sorted-token-vector? (vals limits))))

(s/def :contract/provider valid-document?)

(def ^:private resource-path "vis-contract/provider.edn")

(def ^:private document
  (delay
    (let [resource
          (io/resource resource-path)

          _
          (when-not resource
            (throw (ex-info (str "the provider contract is missing from the classpath: "
                                 resource-path)
                            {:type :vis/contract-missing :resource resource-path})))

          parsed
          (edn/read-string (slurp resource))]

      (when-not (s/valid? :contract/provider parsed)
        (throw (ex-info (str resource-path " is not a valid provider contract")
                        {:type :vis/contract-invalid
                         :resource resource-path
                         :explain (s/explain-str :contract/provider parsed)})))
      parsed)))

(defn- vocabulary [key] (set (map keyword (get-in @document [:provider/limits key]))))

(def version "Provider contract document version." (:contract/version @document))
(def statuses "Closed limits report statuses." (vocabulary :statuses))
(def scopes "What one limit row is measured against." (vocabulary :scopes))
(def kinds "What one limit row counts." (vocabulary :kinds))
(def window-kinds "How a row's window is anchored." (vocabulary :window-kinds))
(def window-units "Calendar units a row's window may be sized in." (vocabulary :window-units))
(def precisions "How exactly a row's numbers are known." (vocabulary :precisions))
(def sources "Where a row's numbers came from." (vocabulary :sources))

(s/def ::provider-id keyword?)
(s/def ::status statuses)
(s/def ::fetched-at-ms integer?)

(s/def ::rpm nat-int?)
(s/def ::tpm nat-int?)
(s/def ::static (s/keys :opt-un [::rpm ::tpm]))

(s/def ::id keyword?)
(s/def ::label non-blank-string?)
(s/def ::scope scopes)
(s/def ::subject map?)
(s/def ::kind kinds)
(s/def ::precision precisions)
(s/def ::source sources)
(s/def ::used number?)
(s/def ::limit number?)
(s/def ::remaining number?)
(s/def ::is-unlimited boolean?)
(s/def ::note string?)

(s/def ::window
  (s/and map?
         #(contains? window-kinds (:kind %))
         #(or (nil? (:unit %)) (contains? window-units (:unit %)))
         #(or (nil? (:size %)) (pos-int? (:size %)))
         #(or (nil? (:resets-at-ms %)) (integer? (:resets-at-ms %)))))

(s/def ::limit-row
  (s/keys :req-un [::id ::label ::scope ::kind ::precision ::source ::is-unlimited]
          :opt-un [::subject ::window ::used ::limit ::remaining ::note]))

(s/def ::limits (s/coll-of ::limit-row :kind vector?))

(s/def ::dynamic (s/keys :req-un [::limits] :opt-un [::note]))

(s/def ::type keyword?)
(s/def ::message non-blank-string?)
(s/def ::data map?)
(s/def ::error (s/keys :req-un [::type ::message] :opt-un [::data]))

(s/def ::report
  (s/keys :req-un [::provider-id ::status ::fetched-at-ms ::static ::dynamic] :opt-un [::error]))

(defn report-valid?
  "True when `value` is a whole normalized limits report a channel can paint."
  [value]
  (s/valid? ::report value))

(defn explain-report
  "Spec explain data for an invalid limits report, or nil when valid."
  [value]
  (s/explain-data ::report value))

(defn limit-row-valid?
  "True when `value` is one renderable limit row."
  [value]
  (s/valid? ::limit-row value))

(defn package-document
  "Deterministic JSON-ready provider section for every generated language contract."
  []
  (let [limits (:provider/limits @document)]
    (array-map "version" version
               "limits" (array-map "statuses" (:statuses limits)
                                   "scopes" (:scopes limits)
                                   "kinds" (:kinds limits)
                                   "window_kinds" (:window-kinds limits)
                                   "window_units" (:window-units limits)
                                   "precisions" (:precisions limits)
                                   "sources" (:sources limits)))))
