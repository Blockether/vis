(ns com.blockether.vis.internal.content
  "Canonical role-labelled message and typed content-block contract.

   This is the only persisted and transported answer shape. Maps use JSON-ready
   snake_case string keys so in-process and remote consumers observe identical
   values. Markdown exists only as the payload of a prose block."
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [com.blockether.vis.internal.gateway.wire :as wire]))

(def roles #{"user" "assistant" "system" "developer" "tool"})

(def message-statuses #{"streaming" "completed" "failed" "cancelled" "suspended"})

(def tool-statuses #{"pending" "running" "completed" "failed" "cancelled"})

(def reasoning-visibilities #{"private" "visible"})

(def block-types #{"prose" "code" "tool" "reasoning" "error" "attachment" "notice"})

(defn- non-blank-string? [x] (and (string? x) (not (str/blank? x))))

(defn- json-ready?
  [x]
  (cond (map? x) (and (every? string? (keys x)) (every? json-ready? (vals x)))
        (vector? x) (every? json-ready? x)
        :else (or (nil? x) (string? x) (number? x) (boolean? x))))

(defn- string-keyed-map? [x] (and (map? x) (json-ready? x)))

(defn- timestamp? [x] (and (integer? x) (not (neg? x))))

(defmulti block-valid? #(get % "type"))

(defmethod block-valid? "prose"
  [block]
  (and (string-keyed-map? block)
       (non-blank-string? (get block "id"))
       (string? (get block "markdown"))))

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
           (<= (get message "created_at") (get message "completed_at")))))

(defn event-valid?
  [event]
  (and (string-keyed-map? event)
       (case (get event "type")
         "content.block.started"
         (and (non-blank-string? (get event "turn_id")) (block-valid? (get event "block")))

         "content.block.delta"
         (and (non-blank-string? (get event "turn_id"))
              (non-blank-string? (get event "block_id"))
              (contains? #{"markdown" "text"} (get event "field"))
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

(defn assert-block!
  [block]
  (if (s/valid? ::block block)
    block
    (throw (ex-info "Invalid canonical content block"
                    {:block block :explain (s/explain-data ::block block)}))))

(defn assert-message!
  [message]
  (if (s/valid? ::message message)
    message
    (throw (ex-info "Invalid canonical message"
                    {:message message :explain (s/explain-data ::message message)}))))

(defn block-id [] (str "block_" (java.util.UUID/randomUUID)))

(defn prose
  ([markdown] (prose (block-id) markdown))
  ([id markdown] (assert-block! {"id" (str id) "type" "prose" "markdown" (str markdown)})))

(defn code
  ([text] (code (block-id) text nil))
  ([id text language]
   (assert-block! (cond-> {"id" (str id) "type" "code" "text" (str text)}
                    (some? language)
                    (assoc "language" (str language))))))

(defn tool
  [{:keys [id tool status input output error started-at completed-at] :or {id (block-id)}}]
  (assert-block! (cond-> {"id" (str id) "type" "tool" "tool" (str tool) "status" (name status)}
                   (some? input)
                   (assoc "input" (wire/canonical input))

                   (some? output)
                   (assoc "output" (wire/canonical output))

                   (some? error)
                   (assoc "error" (wire/canonical error))

                   started-at
                   (assoc "started_at" started-at)

                   completed-at
                   (assoc "completed_at" completed-at))))

(defn attachment
  [{:keys [id attachment-id name media-type] :or {id (block-id)}}]
  (assert-block! {"id" (str id)
                  "type" "attachment"
                  "attachment_id" (str attachment-id)
                  "name" (str name)
                  "media_type" (str media-type)}))

(defn reasoning
  ([text] (reasoning (block-id) text "private"))
  ([id text visibility]
   (assert-block!
     {"id" (str id) "type" "reasoning" "text" (str text) "visibility" (or visibility "private")})))

(defn error
  ([code message] (error (block-id) code message false))
  ([code message retryable?] (error (block-id) code message retryable?))
  ([id code message retryable?]
   (assert-block! {"id" (str id)
                   "type" "error"
                   "code" (str code)
                   "message" (str message)
                   "retryable" (boolean retryable?)})))

(defn notice
  ([code message] (notice (block-id) code message))
  ([id code message]
   (assert-block! {"id" (str id) "type" "notice" "code" (str code) "message" (str message)})))

(defn message
  [{:keys [id role status content created-at completed-at model provider author]
    :or {status "streaming" content [] created-at (System/currentTimeMillis)}}]
  (assert-message! (cond->
                     {"id" (str id)
                      "role" (name role)
                      "status" (name status)
                      "content" (vec content)
                      "created_at" created-at}
                     completed-at
                     (assoc "completed_at" completed-at)

                     model
                     (assoc "model" (str model))

                     provider
                     (assoc "provider" (name provider))

                     author
                     (assoc "author" (str author)))))

(defn answer-content
  "Convert the engine's final answer value into canonical blocks.
   Accepted answer values are Markdown strings, `{:answer string}`, and
   needs-input maps. Typed content vectors pass through after validation."
  [answer]
  (let [answer (if (and (map? answer) (contains? answer :result)) (:result answer) answer)]
    (cond (nil? answer) []
          (and (vector? answer) (every? block-valid? answer)) answer
          (string? answer) [(prose answer)]
          (and (map? answer) (string? (:answer answer))) [(prose (:answer answer))]
          (and (map? answer) (string? (:answer/text answer))) [(prose (:answer/text answer))]
          :else (throw (ex-info "Final answer must be canonical content or Markdown prose"
                                {:answer-type (type answer)})))))

(defn text-projection
  "Disposable plain-text projection for search, clipboard, and error fallback."
  [blocks]
  (->> blocks
       (keep (fn [block]
               (case (get block "type")
                 "prose"
                 (get block "markdown")

                 ("code" "reasoning")
                 (get block "text")

                 ("error" "notice")
                 (get block "message")

                 "tool"
                 (some-> (get block "output")
                         str)

                 nil)))
       (str/join "\n\n")))
