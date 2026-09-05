(ns com.blockether.vis.internal.content
  "Canonical-content builders, normalization and disposable text projection.

   `com.blockether.vis.contract.content` owns the persisted and transported shapes.
   This namespace constructs those JSON-ready, snake_case values and converts final
   engine answers into them; Markdown exists only as the payload of a prose block."
  (:require [clojure.string :as str]
            [com.blockether.vis.contract.content :as content-contract]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.internal.util :as util]))

(defn assert-block!
  [block]
  (if (content-contract/block-valid? block)
    block
    (throw (ex-info "Invalid canonical content block"
                    {:block block :explain (content-contract/block-explain-data block)}))))

(defn assert-message!
  [message]
  (if (content-contract/message-valid? message)
    message
    (throw (ex-info "Invalid canonical message"
                    {:message message :explain (content-contract/message-explain-data message)}))))

(defn block-id [] (str "block_" (java.util.UUID/randomUUID)))

(defn prose
  ([markdown] (prose (block-id) markdown))
  ([id markdown] (assert-block! {"id" (str id) "type" "prose" "markdown" (str markdown)})))

(defn speech
  ([text] (speech (block-id) text))
  ([id text] (assert-block! {"id" (str id) "type" "speech" "text" (str text)})))

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
    :or {status "streaming" content [] created-at (util/now-ms)}}]
  (assert-message! (cond-> {"id" (str id)
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

(def ^:private speech-fence-pattern
  #"(?s)(?:^|\n)[ \t]*```vis-speech[ \t]*\n(.*?)(?:\n[ \t]*```)(?=\n|$)")

(defn- markdown-content
  "Turn the reserved `vis-speech` fence into a semantic speech block. The fence is
   removed from ordinary prose so visual clients never render a duplicate code
   block. Text outside the fence remains the canonical full answer."
  [markdown]
  (let [markdown
        (str markdown)

        [_ speech-text]
        (re-find speech-fence-pattern markdown)

        prose-text
        (str/trim (str/replace-first markdown speech-fence-pattern "\n"))

        spoken
        (some-> speech-text
                str/trim
                not-empty)]

    (cond-> []
      (not (str/blank? prose-text))
      (conj (prose prose-text))

      spoken
      (conj (speech spoken)))))

(defn answer-content
  "Convert the engine's final answer value into canonical blocks.
   Accepted answer values are Markdown strings, `{:answer string}`, wrapped
   canonical content vectors, and needs-input maps. Typed content vectors pass
   through after validation. A reserved `vis-speech` fence becomes a speech block."
  [answer]
  (let [answer (if (and (map? answer) (contains? answer :result)) (:result answer) answer)]
    (cond (nil? answer) []
          (and (vector? answer) (every? content-contract/block-valid? answer)) answer
          (and (map? answer)
               (vector? (:answer answer))
               (every? content-contract/block-valid? (:answer answer)))
          (:answer answer)
          (string? answer) (markdown-content answer)
          (and (map? answer) (string? (:answer answer))) (markdown-content (:answer answer))
          (and (map? answer) (string? (:answer/text answer))) (markdown-content (:answer/text
                                                                                  answer))
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

                 ("speech" "code" "reasoning")
                 (get block "text")

                 ("error" "notice")
                 (get block "message")

                 "tool"
                 (some-> (get block "output")
                         str)

                 nil)))
       (str/join "\n\n")))
