(ns com.blockether.vis.contract.content
  "Canonical content vocabulary and JSON Schema validation."
  (:require [com.blockether.vis.contract.document :as document]))

(set! *warn-on-reflection* true)

(def ^:private contract (delay (document/load! "content")))

(def version "Canonical-content contract document version." (get @contract "version"))
(def roles "Canonical message roles." (set (get @contract "roles")))
(def message-statuses
  "Canonical message lifecycle statuses."
  (set (get @contract "message_statuses")))
(def tool-statuses "Canonical tool-block lifecycle statuses." (set (get @contract "tool_statuses")))
(def reasoning-visibilities
  "Canonical reasoning visibility values."
  (set (get @contract "reasoning_visibilities")))
(def block-types "Canonical content-block type names." (set (get @contract "block_types")))
(def event-types "Canonical append-only content event names." (set (get @contract "event_types")))
(def delta-fields "Fields a content delta may append to." (set (get @contract "delta_fields")))

(def vocabulary
  "Portable canonical-content vocabulary from the JSON contract."
  {:roles (get @contract "roles")
   :message-statuses (get @contract "message_statuses")
   :tool-statuses (get @contract "tool_statuses")
   :reasoning-visibilities (get @contract "reasoning_visibilities")
   :block-types (get @contract "block_types")
   :event-types (get @contract "event_types")
   :delta-fields (get @contract "delta_fields")})

(defn package-document "The validated language-neutral content document." [] @contract)

(defn block-valid?
  "True when `block` satisfies the canonical block JSON Schema."
  [block]
  (document/valid-json? "content" "block" block))

(defn message-valid?
  "True when `message` satisfies the canonical message JSON Schema and its timestamps are ordered."
  [message]
  (and (document/valid-json? "content" "message" message)
       (or (nil? (get message "completed_at"))
           (<= (long (get message "created_at")) (long (get message "completed_at"))))))

(defn event-valid?
  "True when `event` satisfies the canonical content-event JSON Schema."
  [event]
  (document/valid-json? "content" "event" event))

(defn block-explain-data
  "JSON Schema errors for an invalid canonical block, or nil."
  [block]
  (document/explain-json "content" "block" block))

(defn message-explain-data
  "JSON Schema errors for an invalid canonical message, or nil."
  [message]
  (document/explain-json "content" "message" message))
