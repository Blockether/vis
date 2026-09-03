(ns com.blockether.vis.contract.provider
  "Provider limits vocabulary and report validation from JSON Schema."
  (:require [com.blockether.vis.contract.document :as document]))

(def ^:private contract (delay (document/load! "provider")))
(defn- vocabulary [key] (set (map keyword (get-in @contract ["limits" key]))))

(def version "Provider contract document version." (get @contract "version"))
(def statuses "Closed limits report statuses." (vocabulary "statuses"))
(def scopes "What one limit row is measured against." (vocabulary "scopes"))
(def kinds "What one limit row counts." (vocabulary "kinds"))
(def window-kinds "How a row's window is anchored." (vocabulary "window_kinds"))
(def window-units "Calendar units a row's window may use." (vocabulary "window_units"))
(def precisions "How exactly a row's numbers are known." (vocabulary "precisions"))
(def sources "Where a row's numbers came from." (vocabulary "sources"))

(defn report-valid?
  "True when `value` satisfies the provider report schema."
  [value]
  (document/valid? "provider" "report" value))
(defn explain-report
  "JSON Schema errors for an invalid provider report, or nil."
  [value]
  (document/explain "provider" "report" value))
(defn limit-row-valid?
  "True when `value` satisfies the provider limit-row schema."
  [value]
  (document/valid? "provider" "limit_row" value))
(defn package-document "The validated language-neutral provider document." [] @contract)
