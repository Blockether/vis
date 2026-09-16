(ns com.blockether.vis.contract.config
  "Vis configuration vocabulary and JSON Schema validation."
  (:require [com.blockether.vis.contract.document :as document]))

(set! *warn-on-reflection* true)

(def ^:private schema (delay (document/schema-document "config")))

(def ^:private api-styles (get-in @schema ["$defs" "apiStyle" "oneOf"]))

(def api-style-values "Documented provider API styles." (mapv #(get % "default") api-styles))

(def api-style-aliases
  "Accepted provider API-style spelling to normalized runtime spelling."
  (into {}
        (mapcat (fn [style]
                  (map #(vector % (get style "x-vis-runtime")) (get style "enum"))))
        api-styles))

(defn definition-property-names
  "Property names declared by one object definition in the configuration schema."
  [definition]
  (some-> (get-in @schema ["$defs" definition "properties"])
          keys
          set))

(def workspace-access-values
  "Accepted workspace access spellings."
  (set (get-in @schema ["$defs" "workspaceEntry" "properties" "access" "enum"])))

(def workspace-draft-values
  "Workspace draft policies."
  (set (get-in @schema ["$defs" "workspaceEntry" "properties" "draft" "enum"])))

(def workspace-os-values
  "Workspace host selectors."
  (set (get-in @schema ["$defs" "workspaceOs" "enum"])))

(def jail-environment-values
  "Sandbox environment modes."
  (set (get-in @schema ["$defs" "jail" "properties" "environment" "enum"])))

(def titling-modes
  "Session title modes."
  (set (get-in @schema ["$defs" "titling" "properties" "mode" "enum"])))

(defn config-valid?
  "True when the raw string-keyed configuration satisfies the contract schema."
  [config]
  (document/valid-json? "config" "config" config))

(defn config-explain-data
  "JSON Schema errors for configuration, or nil."
  [config]
  (document/explain-json "config" "config" config))

(defn definition-valid?
  "True when raw JSON-shaped data satisfies one configuration definition."
  [definition value]
  (document/valid-json? "config" definition value))
