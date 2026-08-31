(ns com.blockether.vis.contract.content
  "The executable canonical-content contract.

   `vis-contract/content.edn` owns the portable role, status, block, event and
   delta-field vocabularies. This namespace validates that document, declares the
   canonical message/content/event shapes and renders the same vocabulary for every
   SDK. It is a dependency leaf and requires no Vis implementation namespace."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]))

(set! *warn-on-reflection* true)

(defn- non-blank-string? [x] (and (string? x) (not (str/blank? x))))
(defn- closed-map? [m expected-keys] (and (map? m) (= expected-keys (set (keys m)))))

(defn- sorted-string-vector?
  [value]
  (and (vector? value)
       (seq value)
       (= value (vec (sort value)))
       (= (count value) (count (set value)))
       (every? non-blank-string? value)))

(defn- valid-document?
  [{:contract/keys [version]
    :content/keys [roles message-statuses tool-statuses reasoning-visibilities block-types
                   event-types delta-fields]
    :as document}]
  (and (closed-map? document
                    #{:contract/version :content/roles :content/message-statuses
                      :content/tool-statuses :content/reasoning-visibilities :content/block-types
                      :content/event-types :content/delta-fields})
       (pos-int? version)
       (every? sorted-string-vector?
               [roles message-statuses tool-statuses reasoning-visibilities block-types event-types
                delta-fields])))

(s/def :contract/content valid-document?)

(def ^:private resource-path "vis-contract/content.edn")

(def ^:private document
  (delay
    (let [resource
          (io/resource resource-path)

          _
          (when-not resource
            (throw (ex-info (str "the content contract is missing from the classpath: "
                                 resource-path)
                            {:type :vis/contract-missing :resource resource-path})))

          parsed
          (edn/read-string (slurp resource))]

      (when-not (s/valid? :contract/content parsed)
        (throw (ex-info (str resource-path " is not a valid content contract")
                        {:type :vis/contract-invalid
                         :resource resource-path
                         :explain (s/explain-str :contract/content parsed)})))
      parsed)))

(def version "Canonical-content contract document version." (:contract/version @document))
(def roles "Canonical message roles." (set (:content/roles @document)))
(def message-statuses
  "Canonical message lifecycle statuses."
  (set (:content/message-statuses @document)))
(def tool-statuses
  "Canonical tool-block lifecycle statuses."
  (set (:content/tool-statuses @document)))
(def reasoning-visibilities
  "Canonical reasoning visibility values."
  (set (:content/reasoning-visibilities @document)))
(def block-types "Canonical content-block type names." (set (:content/block-types @document)))
(def event-types
  "Canonical append-only content event names."
  (set (:content/event-types @document)))
(def delta-fields "Fields a content delta may append to." (set (:content/delta-fields @document)))

(def vocabulary
  "Portable canonical-content vocabulary derived from the owning EDN document."
  {:roles (:content/roles @document)
   :message-statuses (:content/message-statuses @document)
   :tool-statuses (:content/tool-statuses @document)
   :reasoning-visibilities (:content/reasoning-visibilities @document)
   :block-types (:content/block-types @document)
   :event-types (:content/event-types @document)
   :delta-fields (:content/delta-fields @document)})

(defn package-document
  "Deterministic JSON-ready content section for every generated language contract."
  []
  (array-map "version" version
             "roles" (:roles vocabulary)
             "message_statuses" (:message-statuses vocabulary)
             "tool_statuses" (:tool-statuses vocabulary)
             "reasoning_visibilities" (:reasoning-visibilities vocabulary)
             "block_types" (:block-types vocabulary)
             "event_types" (:event-types vocabulary)
             "delta_fields" (:delta-fields vocabulary)))

(defn- json-ready?
  [x]
  (cond (map? x) (and (every? string? (keys x)) (every? json-ready? (vals x)))
        (vector? x) (every? json-ready? x)
        :else (or (nil? x) (string? x) (number? x) (boolean? x))))

(defn- string-keyed-map? [x] (and (map? x) (json-ready? x)))
(defn- timestamp? [x] (and (integer? x) (not (neg? (long x)))))

(defmulti block-valid? #(get % "type"))

(defmethod block-valid? "prose"
  [block]
  (and (string-keyed-map? block)
       (non-blank-string? (get block "id"))
       (string? (get block "markdown"))))

(defmethod block-valid? "speech"
  [block]
  (and (string-keyed-map? block)
       (non-blank-string? (get block "id"))
       (non-blank-string? (get block "text"))))

(defmethod block-valid? "code"
  [block]
  (and (string-keyed-map? block)
       (non-blank-string? (get block "id"))
       (string? (get block "text"))
       (or (nil? (get block "language")) (non-blank-string? (get block "language")))))

(defmethod block-valid? "tool"
  [block]
  (and (string-keyed-map? block)
       (non-blank-string? (get block "id"))
       (non-blank-string? (get block "tool"))
       (contains? tool-statuses (get block "status"))))

(defmethod block-valid? "reasoning"
  [block]
  (and (string-keyed-map? block)
       (non-blank-string? (get block "id"))
       (string? (get block "text"))
       (contains? reasoning-visibilities (get block "visibility" "private"))))

(defmethod block-valid? "error"
  [block]
  (and (string-keyed-map? block)
       (non-blank-string? (get block "id"))
       (non-blank-string? (get block "code"))
       (non-blank-string? (get block "message"))
       (or (nil? (get block "retryable")) (boolean? (get block "retryable")))))

(defmethod block-valid? "attachment"
  [block]
  (and (string-keyed-map? block)
       (non-blank-string? (get block "id"))
       (non-blank-string? (get block "attachment_id"))
       (non-blank-string? (get block "name"))
       (non-blank-string? (get block "media_type"))))

(defmethod block-valid? "notice"
  [block]
  (and (string-keyed-map? block)
       (non-blank-string? (get block "id"))
       (non-blank-string? (get block "code"))
       (non-blank-string? (get block "message"))))

(defmethod block-valid? :default [_] false)

(defn message-valid?
  [message]
  (and (string-keyed-map? message)
       (non-blank-string? (get message "id"))
       (contains? roles (get message "role"))
       (contains? message-statuses (get message "status"))
       (vector? (get message "content"))
       (every? block-valid? (get message "content"))
       (timestamp? (get message "created_at"))
       (or (nil? (get message "completed_at")) (timestamp? (get message "completed_at")))
       (or (nil? (get message "completed_at"))
           (<= (long (get message "created_at")) (long (get message "completed_at"))))))

(defn event-valid?
  [event]
  (and (string-keyed-map? event)
       (contains? event-types (get event "type"))
       (case (get event "type")
         "content.block.started"
         (and (non-blank-string? (get event "turn_id")) (block-valid? (get event "block")))

         "content.block.delta"
         (and (non-blank-string? (get event "turn_id"))
              (non-blank-string? (get event "block_id"))
              (contains? delta-fields (get event "field"))
              (string? (get event "text")))

         "content.block.completed"
         (and (non-blank-string? (get event "turn_id")) (non-blank-string? (get event "block_id")))

         "turn.completed"
         (and (non-blank-string? (get event "turn_id")) (= "completed" (get event "status")))

         "turn.failed"
         (and (non-blank-string? (get event "turn_id")) (= "failed" (get event "status")))

         "turn.cancelled"
         (and (non-blank-string? (get event "turn_id")) (= "cancelled" (get event "status")))

         false)))

(s/def ::block block-valid?)
(s/def ::content (s/coll-of ::block :kind vector?))
(s/def ::message message-valid?)
(s/def ::event event-valid?)

(defn block-explain-data
  "Spec explanation for an invalid canonical block, or nil."
  [block]
  (s/explain-data ::block block))

(defn message-explain-data
  "Spec explanation for an invalid canonical message, or nil."
  [message]
  (s/explain-data ::message message))
