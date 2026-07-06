(ns com.blockether.vis.ext.channel-telegram.api
  "Thin wrapper around the Telegram Bot API using babashka/http-client.

   Telegram Bot API reference: https://core.telegram.org/bots/api

   Telegram-facing primitives:
   - `get-updates`      long-poll for incoming messages
   - `send-message!`    send text back (auto-splits at Telegram's limit)
   - `send-chat-action!`  show 'typing...' while the LLM works
   - `set-my-commands!` install the slash-command menu shown by Telegram"
  (:require [babashka.http-client :as http]
            [charred.api :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(defn- base-url [token]
  (str "https://api.telegram.org/bot" token))

(defn- file-base-url [token]
  (str "https://api.telegram.org/file/bot" token))

(defn- parse-body [resp]
  (json/read-json (:body resp) :key-fn keyword))

(defn get-updates
  "Long-poll Telegram for new updates.

   `offset`  - lowest update_id we haven't processed (0 on first call).
   `timeout` - seconds Telegram may hold the connection (up to 50). The HTTP
               client timeout is bumped by 10s to cover network slack.

   Returns a vector of update maps (Telegram's `Update` objects)."
  [token offset timeout]
  (let [resp (http/get (str (base-url token) "/getUpdates")
               {:query-params {"offset"  (str offset)
                               "timeout" (str timeout)}
                :timeout      (* 1000 (+ timeout 10))})
        body (parse-body resp)]
    (when-not (:ok body)
      (throw (ex-info (str "Telegram getUpdates failed: " (:description body))
               {:body body})))
    (:result body)))

(defn- post-json! [token path payload]
  (let [resp (http/post (str (base-url token) path)
               {:headers {"content-type" "application/json"}
                :body    (json/write-json-str payload)
                :timeout 15000
                :throw   false})]
    (parse-body resp)))

(defn- chunk-text
  "Split text into <=4000-char pieces along paragraph/line boundaries so
   HTML tags stay intact across chunks. Telegram's hard limit is 4096."
  [text]
  (if (<= (count text) 4000)
    [text]
    (loop [remaining text
           chunks    []]
      (if (<= (count remaining) 4000)
        (conj chunks remaining)
        (let [cut-at (or (str/last-index-of remaining "\n\n" (long 4000))
                       (str/last-index-of remaining "\n" (long 4000))
                       (str/last-index-of remaining " " (long 4000))
                       4000)]
          (recur (subs remaining cut-at)
            (conj chunks (subs remaining 0 cut-at))))))))

(defn post-message!
  "Post a single message and return the parsed Telegram response
   (`:ok :result :description ...`).

   Does NOT chunk over 4096 chars — callers that need chunking go
   through `send-message!`.

   `text` defaults to Telegram-HTML; pass `:plain? true` for raw."
  ([token chat-id text]
   (post-message! token chat-id text nil))
  ([token chat-id text {:keys [reply-markup plain?]}]
   (let [payload (cond-> {"chat_id" chat-id
                          "text"    text}
                   (not plain?) (assoc "parse_mode" "HTML")
                   reply-markup (assoc "reply_markup" reply-markup))]
     (post-json! token "/sendMessage" payload))))

(defn send-message!
  "Send a text reply.

   The `text` arg is now expected to be either:
   - pre-rendered Telegram-HTML (default) — send with parse_mode=HTML;
   - plain text when `:plain?` opt is true — send without parse_mode.

   Caller is responsible for rendering. Channel registers a
   `:channel/messages-renderer-fn` that bot.clj calls before this fn.
   Auto-splits at 4096 chars. Falls back to plain text if Telegram
   rejects the HTML payload (defect-#10 last-resort safety net).

   `opts` supports:
   - `:reply-markup` - Telegram reply_markup map, e.g. inline keyboard;
   - `:plain?`       - skip parse_mode; ship `text` raw."
  ([token chat-id text]
   (send-message! token chat-id text nil))
  ([token chat-id text {:keys [reply-markup plain?]}]
   (doseq [chunk (chunk-text (or text ""))]
     (let [payload (cond-> {"chat_id" chat-id
                            "text"    chunk}
                     (not plain?) (assoc "parse_mode" "HTML")
                     reply-markup (assoc "reply_markup" reply-markup))
           resp    (post-json! token "/sendMessage" payload)]
       (when (and (not plain?) (not (:ok resp)))
         ;; HTML rejected — fall back to plain text so the message at
         ;; least lands. Strips formatting visually but never drops the
         ;; payload silently.
         (post-json! token "/sendMessage"
           (cond-> {"chat_id" chat-id
                    "text"    chunk}
             reply-markup (assoc "reply_markup" reply-markup))))))))

(defn set-my-commands!
  "Install Telegram's slash-command menu for the bot.

   `commands` is a seq of Telegram BotCommand maps with string keys
   `command` and `description`.
   Throws when Telegram rejects the menu so caller can log diagnostics."
  [token commands]
  (let [resp (post-json! token "/setMyCommands"
               {"commands" (mapv #(select-keys % ["command" "description"])
                             commands)})]
    (when-not (:ok resp)
      (throw (ex-info (str "Telegram setMyCommands failed: " (:description resp))
               {:body resp})))
    resp))

(defn edit-message!
  "Edit a previously-sent text message. Returns the parsed Telegram
   response map; caller checks `:ok`.

   `text` is expected to be pre-rendered Telegram-HTML (default) or
   plain text when `:plain?` is true. `:reply-markup` keeps / changes
   the inline keyboard; pass `{:inline_keyboard []}` to drop it.

   On HTTP 400 with description `message is not modified`, returns
   the response without retry (caller swallows). On HTTP 429, the
   response carries `:parameters {:retry_after N}` for caller backoff."
  ([token chat-id message-id text]
   (edit-message! token chat-id message-id text nil))
  ([token chat-id message-id text {:keys [reply-markup plain?]}]
   (let [payload (cond-> {"chat_id"    chat-id
                          "message_id" message-id
                          "text"       text}
                   (not plain?) (assoc "parse_mode" "HTML")
                   reply-markup (assoc "reply_markup" reply-markup))]
     (post-json! token "/editMessageText" payload))))

(defn delete-message!
  "Delete a message. Best-effort — swallows errors."
  [token chat-id message-id]
  (try
    (post-json! token "/deleteMessage"
      {"chat_id" chat-id "message_id" message-id})
    (catch Exception _ nil)))

(defn answer-callback-query!
  "Acknowledge a Telegram inline-keyboard callback. Best-effort."
  ([token callback-query-id]
   (answer-callback-query! token callback-query-id nil))
  ([token callback-query-id text]
   (try
     (post-json! token "/answerCallbackQuery"
       (cond-> {"callback_query_id" callback-query-id}
         (seq text) (assoc "text" text)))
     (catch Exception _ nil))))

(defn send-chat-action!
  "Show a transient indicator in the chat (e.g. 'typing...'). Best-effort: errors
   are swallowed because it's UX decoration, not correctness."
  [token chat-id action]
  (try
    (post-json! token "/sendChatAction"
      {"chat_id" chat-id "action" action})
    (catch Exception _ nil)))

(defn get-file
  "Return Telegram File metadata for `file-id`, including `:file_path`."
  [token file-id]
  (let [resp (post-json! token "/getFile" {"file_id" file-id})]
    (when-not (:ok resp)
      (throw (ex-info (str "Telegram getFile failed: " (:description resp))
               {:body resp})))
    (:result resp)))

(defn download-file!
  "Download Telegram file `file-path` into `dest-file`. Returns dest-file."
  [token file-path dest-file]
  (let [url (str (file-base-url token) "/" file-path)]
    (try
      (let [resp (http/get url {:as :stream :throw false :timeout 60000})]
        (if (<= 200 (long (:status resp 0)) 299)
          (with-open [in  (:body resp)
                      out (io/output-stream dest-file)]
            (io/copy in out)
            dest-file)
          (throw (ex-info (str "Telegram file download failed for " file-path)
                   {:file-path file-path
                    :status (:status resp)}))))
      (catch Throwable t
        (if (and (= file-path (:file-path (ex-data t)))
              (contains? (ex-data t) :status))
          (throw t)
          (throw (ex-info (str "Telegram file download failed for " file-path)
                   {:file-path file-path}
                   t)))))))

(defn- post-multipart! [token path fields]
  (let [resp (http/post (str (base-url token) path)
               {:multipart fields
                :timeout 60000
                :throw false})]
    (parse-body resp)))

(defn send-voice!
  "Send an OGG/Opus voice-note file to Telegram."
  [token chat-id voice-file]
  (let [resp (post-multipart! token "/sendVoice"
               [{:name "chat_id" :content (str chat-id)}
                {:name "voice" :content (io/file voice-file)}])]
    (when-not (:ok resp)
      (throw (ex-info (str "Telegram sendVoice failed: " (:description resp))
               {:body resp})))
    resp))

(defn send-audio!
  "Send an audio file to Telegram as a generic audio attachment."
  [token chat-id audio-file]
  (let [resp (post-multipart! token "/sendAudio"
               [{:name "chat_id" :content (str chat-id)}
                {:name "audio" :content (io/file audio-file)}])]
    (when-not (:ok resp)
      (throw (ex-info (str "Telegram sendAudio failed: " (:description resp))
               {:body resp})))
    resp))

(defn send-document!
  "Upload `doc-file` to Telegram as a generic document attachment.

   `filename` names the file the recipient downloads (Telegram uses it
   verbatim). `caption` is optional chat text shown under the document."
  ([token chat-id doc-file filename]
   (send-document! token chat-id doc-file filename nil))
  ([token chat-id doc-file filename caption]
   (let [resp (post-multipart! token "/sendDocument"
                (cond-> [{:name "chat_id" :content (str chat-id)}
                         {:name "document" :content (io/file doc-file)
                          :file-name (str filename)}]
                  (not (str/blank? (str caption)))
                  (conj {:name "caption" :content (str caption)})))]
     (when-not (:ok resp)
       (throw (ex-info (str "Telegram sendDocument failed: " (:description resp))
                {:body resp})))
     resp)))
