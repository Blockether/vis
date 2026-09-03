(ns com.blockether.vis.contract.config
  "Vis configuration vocabulary and JSON Schema validation."
  (:require [com.blockether.vis.contract.document :as document]))

(set! *warn-on-reflection* true)

(def ^:private contract (delay (document/load! "config")))

(def api-style-values "Documented provider API styles." (vec (get @contract "api_style_values")))
(def api-style-aliases
  "Accepted provider API-style spelling to normalized runtime spelling."
  (get @contract "api_style_aliases"))

(defn definition-property-names
  "Property names declared by one object definition in config.json."
  [definition]
  (some-> (document/schema-document "config")
          (get-in ["$defs" definition "properties"])
          keys
          set))
(def workspace-access-values
  "Accepted workspace access spellings."
  (set (get @contract "workspace_access_values")))
(def workspace-draft-values
  "Workspace draft policies."
  (set (get @contract "workspace_draft_values")))
(def workspace-os-values "Workspace host selectors." (set (get @contract "workspace_os_values")))
(def jail-environment-values
  "Sandbox environment modes."
  (set (get @contract "jail_environment_values")))
(def titling-modes "Session title modes." (set (get @contract "titling_modes")))

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

(defn package-document "The validated language-neutral configuration document." [] @contract)
