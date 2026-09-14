(ns com.blockether.vis.contract.improve
  "Editable Improve records and immutable source links. See vis-contract/improve.json for
   hierarchy, analysis and automatic-review semantics; same-named JSON Schema owns shapes."
  (:require [com.blockether.vis.contract.document :as document]))

(defn valid?
  "Validate a record, create/update input or list options using the canonical schema."
  [definition value]
  (document/valid? "improve" (name definition) value))

(defn validate!
  "Return valid input or a sanitized client error without echoing private analysis."
  [definition value]
  (when-not (valid? definition value)
    (throw (ex-info (str "Invalid Improve " (name definition))
                    {:type :improve/invalid :status 400})))
  value)
