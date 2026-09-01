(ns com.blockether.vis.contract.provider
  "The executable provider limits contract.

   `vis-contract/provider.json` owns the portable limits vocabulary: report statuses,
   what one row is measured against, what it counts, how its window is anchored, and
   how exactly and from where its numbers are known. `vis-contract/schema/provider.json`
   declares its shape. This namespace reads that
   document, declares the envelope every provider limits report is judged by, and
   renders the same vocabulary for every SDK.

   A provider's `:provider/limits-fn` is UNTRUSTED input — an extension's Clojure or
   Python, answering whatever it answers. The host runs the result through
   [[report-valid?]] and swaps in an error envelope when a row is malformed, so this
   shape is exactly what a channel may assume it is painting. Fetching, caching, static
   catalog augmentation and error classification stay in Core."
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [com.blockether.vis.contract.document :as document]))

(set! *warn-on-reflection* true)

(defn- non-blank-string? [x] (and (string? x) (not (str/blank? x))))
(def ^:private document (delay (document/load! "provider")))

(defn- vocabulary [key] (set (map keyword (get-in @document ["limits" key]))))

(def version "Provider contract document version." (get @document "version"))
(def statuses "Closed limits report statuses." (vocabulary "statuses"))
(def scopes "What one limit row is measured against." (vocabulary "scopes"))
(def kinds "What one limit row counts." (vocabulary "kinds"))
(def window-kinds "How a row's window is anchored." (vocabulary "window_kinds"))
(def window-units "Calendar units a row's window may be sized in." (vocabulary "window_units"))
(def precisions "How exactly a row's numbers are known." (vocabulary "precisions"))
(def sources "Where a row's numbers came from." (vocabulary "sources"))

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
  ;; The document is the rendered shape already — string keys, snake_case. The
  ;; keys are still spelled out rather than passed through so the ORDER is this
  ;; function's, not the parser's: a generated contract that reorders its keys is
  ;; a diff in every SDK for no change at all.
  (let [limits (get @document "limits")]
    (array-map "version" version
               "limits" (array-map "statuses" (get limits "statuses")
                                   "scopes" (get limits "scopes")
                                   "kinds" (get limits "kinds")
                                   "window_kinds" (get limits "window_kinds")
                                   "window_units" (get limits "window_units")
                                   "precisions" (get limits "precisions")
                                   "sources" (get limits "sources")))))
