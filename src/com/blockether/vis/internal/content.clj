(ns com.blockether.vis.internal.content
  "Canonical-content builders, normalization and disposable text projection.

   `com.blockether.vis.contract.content` owns the persisted and transported shapes.
   This namespace constructs those JSON-ready, snake_case values and converts final
   engine answers into them. Conversation is shared text; visual blocks contain screen-only Markdown."
  (:require [charred.api :as json]
            [clojure.string :as str]
            [com.blockether.vis.contract.content :as content-contract]
            [com.blockether.vis.contract.voice :as voice-contract]
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

(def ^:const fork-notice-code
  "Notice code of a turn that still ran in the source session when a fork copied it."
  "turn_forked")

(defn fork-notice
  "The notice that settles a running turn that a fork copied. Clients show it instead of
   a cancel notice: the person forked the session, nobody cancelled the turn."
  [id]
  (notice id fork-notice-code "Forked from this point."))

(defn fork-notice?
  "True when `block` is the notice of a turn that a fork settled."
  [block]
  (and (= "notice" (get block "type")) (= fork-notice-code (get block "code"))))

(def ^:const leader-cancel-notice-code
  "Notice code of a subagent turn that its leader stopped."
  "turn_cancelled_by_leader")

(defn leader-cancel-notice
  "The notice that settles a subagent turn that its leader stopped, or nil when the cancel
   `source` is not the leader. Clients show it instead of a user cancel notice: no person
   stopped that session. `:agent-cancel` is a `council.cancel` and `:parent-cancel` is the
   stop of the leader turn."
  [id source]
  (case source
    :agent-cancel
    (notice id leader-cancel-notice-code "Cancelled by the leader.")

    :parent-cancel
    (notice id leader-cancel-notice-code "Cancelled when the leader turn stopped.")

    nil))

(defn leader-cancel-notice?
  "True when `block` is the notice of a turn that the leader stopped."
  [block]
  (and (= "notice" (get block "type")) (= leader-cancel-notice-code (get block "code"))))

(defn with-cancel-notice
  "The `blocks` of the cancelled turn `turn-id`, with the notice of a leader stop when the
   cancel `source` is the leader. The notice id is fixed, so each terminal path writes the
   same block."
  [blocks turn-id source]
  (if-let [block (leader-cancel-notice (str turn-id ":cancelled") source)]
    (conj (vec blocks) block)
    (vec blocks)))

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

(defn closed-speech-projections
  "Read closed top-level speech fences. Live commits require a newline after the closing fence.
   Return text and source spans. Ignore speech fences inside code or quoted examples."
  [markdown final?]
  (let [text (str markdown)]
    (loop [lines (seq (str/split text #"\n" -1))
           offset 0
           fence nil
           projections []]

      (if-let [line (first lines)]
        (let [complete? (< (+ offset (count line)) (count text))
              end (+ offset (count line) (if complete? 1 0))
              line (str/replace line #"\r$" "")
              [_ closing] (re-matches #" {0,3}(`{3,}|~{3,})[ \t]*" line)
              [_ opening info] (re-matches #" {0,3}(`{3,}|~{3,})([^`~]*)" line)]

          (if fence
            (if (and closing
                     (= (first closing) (first (:marker fence)))
                     (>= (count closing) (count (:marker fence)))
                     (or complete? final?))
              (recur (next lines)
                     end
                     nil
                     (if (:speech? fence)
                       (conj projections
                             {:text (str/trim (str/join "\n" (:lines fence)))
                              :start (:start fence)
                              :end end})
                       projections))
              (recur (next lines) end (update fence :lines conj line) projections))
            (recur
              (next lines)
              end
              (when (and opening complete?)
                {:marker opening :speech? (= "vis-speech" (str/trim info)) :start offset :lines []})
              projections)))
        projections))))

(defn- presentation-error
  [reason index]
  (ex-info "Invalid hands-free presentation"
           {:type :content/invalid-presentation :reason reason :index index}))

(defn- presentation-record
  [line index]
  (try (with-open [^java.lang.AutoCloseable reader
                   (json/read-json-supplier line {:eof-error? false :eof-value ::eof})]
         (let [record (.get ^java.util.function.Supplier reader)
               tail (.get ^java.util.function.Supplier reader)]

           (when-not (and (map? record)
                          (= ::eof tail)
                          (not (contains? record "id"))
                          (contains? #{"conversation" "visual" "silence"} (get record "type")))
             (throw (presentation-error :invalid-record index)))
           (let [identity-text (str index ":" (wire/json-str (into (sorted-map) record)))
                 id (str "presentation_" index
                         "_" (java.util.UUID/nameUUIDFromBytes
                               (.getBytes ^String identity-text
                                          java.nio.charset.StandardCharsets/UTF_8)))
                 block (assoc record "id" id)]

             (when-not (content-contract/block-valid? block)
               (throw (presentation-error :invalid-record index)))
             block)))
       (catch Exception _ (throw (presentation-error :invalid-record index)))))

(defn presentation-records
  "Read complete JSON lines into canonical blocks with source spans. Ignore only an incomplete last line."
  [raw final?]
  (let [text (str raw)]
    (when (> (count text) (:max-projection-chars voice-contract/limits))
      (throw (presentation-error :too-large 0)))
    (loop [lines (seq (str/split text #"\n" -1))
           offset 0
           records []]

      (if-let [line (first lines)]
        (let [complete? (< (+ offset (count line)) (count text))
              end (+ offset (count line) (if complete? 1 0))]

          (if (and (or complete? final?) (not (str/blank? line)))
            (do (when (>= (count records) (:max-speech-segments voice-contract/limits))
                  (throw (presentation-error :too-many-records (count records))))
                (recur (next lines)
                       end
                       (conj records
                             {:block (presentation-record line (count records))
                              :start offset
                              :end end
                              :index (count records)})))
            (recur (next lines) end records)))
        records))))

(defn presentation-content
  "Validate a final presentation. Require a conversational answer or explicit silence, never progress alone."
  [raw]
  (let [blocks
        (mapv :block (presentation-records raw true))

        conversation?
        (some #(= "conversation" (get % "type")) blocks)

        answer?
        (some #(and (= "conversation" (get % "type")) (= "answer" (get % "phase"))) blocks)

        silent?
        (some #(= "silence" (get % "type")) blocks)]

    (when (and silent? conversation?) (throw (presentation-error :conflicting-silence 0)))
    (when-not (or answer? silent?) (throw (presentation-error :missing-outcome 0)))
    blocks))

(defn- markdown-content
  "Keep the full answer as prose. Convert each reserved speech fence into a separate speech block."
  [markdown]
  (let [markdown
        (str markdown)

        projections
        (closed-speech-projections markdown true)

        [parts offset]
        (reduce (fn [[parts offset] {:keys [start end]}]
                  [(conj parts (subs markdown offset start)) end])
                [[] 0]
                projections)

        prose-text
        (str/trim (str/join "\n" (conj parts (subs markdown offset))))]

    (into (cond-> []
            (not (str/blank? prose-text))
            (conj (prose prose-text)))
          (keep (fn [{:keys [text]}]
                  (when-not (str/blank? text) (speech text))))
          projections)))

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
                 ("prose" "visual")
                 (get block "markdown")

                 ("conversation" "speech" "code" "reasoning")
                 (get block "text")

                 ("error" "notice")
                 (get block "message")

                 "tool"
                 (some-> (get block "output")
                         str)

                 nil)))
       (str/join "\n\n")))
