(ns com.blockether.vis.contract.content
  "Canonical content validation from JSON Schema."
  (:require [com.blockether.vis.contract.document :as document]))

(set! *warn-on-reflection* true)

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
