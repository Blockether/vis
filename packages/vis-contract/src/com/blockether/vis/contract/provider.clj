(ns com.blockether.vis.contract.provider
  "Provider limits, model catalog and report validation from JSON Schema."
  (:require [com.blockether.vis.contract.document :as document]))

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

(defn model-valid?
  "True when `value` satisfies the catalog model schema."
  [value]
  (document/valid? "provider" "model" value))

(defn explain-model
  "JSON Schema errors for an invalid catalog model, or nil."
  [value]
  (document/explain "provider" "model" value))
