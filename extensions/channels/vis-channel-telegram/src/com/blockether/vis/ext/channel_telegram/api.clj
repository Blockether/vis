(ns com.blockether.vis.ext.channel-telegram.api
  "Thin wrapper around the Telegram Bot API using babashka/http-client.

   Telegram Bot API reference: https://core.telegram.org/bots/api

   Telegram-facing primitives:
   - `get-updates`      long-poll for incoming messages
   - `send-message!`    send text back (auto-splits at Telegram's limit)
   - `send-chat-action!`  show 'typing…' while the LLM works
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

   `offset`  — lowest update_id we haven't processed (0 on first call).
   `timeout` — seconds Telegram may hold the connection (up to 50). The HTTP
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

(def ^:private mdv2-specials
  #{\\ \_ \* \[ \] \( \) \~ \` \> \# \+ \- \= \| \{ \} \. \!})

(defn escape-markdown-v2
  "Escape Telegram MarkdownV2 special characters in plain text segments."
  [^String text]
  (let [len (.length text)]
    (loop [i  (int 0)
           sb (StringBuilder. (+ len 16))]
      (if (>= i len)
        (.toString sb)
        (let [ch (.charAt text i)]
          (when (contains? mdv2-specials ch)
            (.append sb \\))
          (.append sb ch)
          (recur (inc i) sb))))))

(defn- escape-html
  [text]
  (-> (str text)
    (str/replace "&" "&amp;")
    (str/replace "<" "&lt;")
    (str/replace ">" "&gt;")
    (str/replace "\"" "&quot;")))

(defn- markdown-table-separator? [line]
  (let [cells (->> (str/split (str/trim (str/replace line #"^\||\|$" "")) #"\|")
                (map str/trim)
                (remove str/blank?))]
    (and (seq cells)
      (every? #(re-matches #":?-{3,}:?" %) cells))))

(defn- markdown-table-line? [line]
  (str/includes? line "|"))

(defn- table-cells [line]
  (->> (str/split (str/trim (str/replace line #"^\||\|$" "")) #"\|")
    (mapv str/trim)))

(defn- pad-right [s width]
  (let [s (str s)]
    (str s (apply str (repeat (max 0 (- width (count s))) " ")))))

(defn- markdown-table->text [lines]
  (let [rows   (mapv table-cells (remove markdown-table-separator? lines))
        cols   (apply max 0 (map count rows))
        rows   (mapv #(into [] (take cols (concat % (repeat "")))) rows)
        widths (mapv (fn [idx]
                       (apply max 1 (map #(count (nth % idx "")) rows)))
                 (range cols))
        fmt-row (fn [row]
                  (str/join "  "
                    (map-indexed (fn [idx cell]
                                   (pad-right cell (nth widths idx)))
                      row)))
        sep     (str/join "  " (map #(apply str (repeat % "─")) widths))]
    (str/join "\n"
      (concat
        (when-let [header (first rows)] [(fmt-row header) sep])
        (map fmt-row (rest rows))))))

(defn- html-inline [text]
  (let [parts (str/split (str text) #"`" -1)]
    (apply str
      (map-indexed
        (fn [idx part]
          (if (odd? idx)
            (str "<code>" (escape-html part) "</code>")
            (-> (escape-html part)
              (str/replace #"\[([^\]\n]+)\]\((https?://[^\s)]+)\)" "<a href=\"$2\">$1</a>")
              (str/replace #"\*\*([^*\n]+)\*\*" "<b>$1</b>")
              (str/replace #"(?<!\*)\*([^*\n]+)\*(?!\*)" "<i>$1</i>")
              (str/replace #"_([^_\n]+)_" "<i>$1</i>"))))
        parts))))

(defn- convert-md-to-html
  "Best-effort Telegram HTML rendering for common LLM Markdown.
   Telegram has no headings or tables, so headings become bold lines and
   Markdown tables become aligned <pre> blocks."
  [text]
  (let [lines (str/split-lines (or text ""))]
    (loop [remaining lines
           out       []]
      (if-not (seq remaining)
        (str/join "\n" out)
        (let [line      (first remaining)
              next-line (second remaining)]
          (cond
            (str/starts-with? line "```")
            (let [[code-lines rest-lines] (split-with #(not (str/starts-with? % "```")) (rest remaining))
                  rest-lines              (if (seq rest-lines) (rest rest-lines) rest-lines)]
              (recur rest-lines
                (conj out (str "<pre>" (escape-html (str/join "\n" code-lines)) "</pre>"))))

            (and (markdown-table-line? line) (markdown-table-separator? (or next-line "")))
            (let [[table-lines rest-lines] (split-with markdown-table-line? remaining)]
              (recur rest-lines
                (conj out (str "<pre>" (escape-html (markdown-table->text table-lines)) "</pre>"))))

            (re-matches #"\s*#{1,6}\s+.+" line)
            (recur (rest remaining)
              (conj out (str "<b>" (escape-html (str/replace line #"^\s*#{1,6}\s+" "")) "</b>")))

            (re-matches #"\s*(-{3,}|_{3,}|\*{3,})\s*" line)
            (recur (rest remaining) (conj out "────────"))

            (str/starts-with? (str/triml line) ">")
            (recur (rest remaining)
              (conj out (str "<blockquote>" (html-inline (str/trim (str/replace line #"^\s*>\s?" ""))) "</blockquote>")))

            :else
            (recur (rest remaining)
              (conj out (html-inline line)))))))))

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

(defn send-message!
  "Send a text reply using Telegram HTML parse mode.
   Falls back to plain text if Telegram rejects the payload.
   Auto-splits at 4096 chars.

   `opts` supports:
   - `:reply-markup` — Telegram reply_markup map, e.g. inline keyboard;
   - `:html?` — send `text` as Telegram HTML without Markdown conversion."
  ([token chat-id text]
   (send-message! token chat-id text nil))
  ([token chat-id text {:keys [reply-markup html?]}]
   (doseq [raw-chunk (chunk-text (or text ""))]
     (let [html-chunk (if html? raw-chunk (convert-md-to-html raw-chunk))
           payload (cond-> {"chat_id"    chat-id
                            "text"       html-chunk
                            "parse_mode" "HTML"}
                     reply-markup (assoc "reply_markup" reply-markup))
           resp    (post-json! token "/sendMessage" payload)]
       (when-not (:ok resp)
         (post-json! token "/sendMessage"
           (cond-> {"chat_id" chat-id
                    "text" raw-chunk}
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
  "Show a transient indicator in the chat (e.g. 'typing…'). Best-effort: errors
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
